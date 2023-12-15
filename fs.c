#include "all.h"

#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include "stb_ds.h"

// #define MODULE_INVALID ((rmodule_t)-1)
// #define MODULE_MAIN ((rmodule_t)0)

u32 fs_node_arena_len;
u32 fs_node_roots_len;
fs_node_t fs_node_arena[128];
fs_rnode_t fs_node_roots[8]; // roots are searched from left to right

// okay to append to, iteration still goes from left to right
u32 fs_files_queue_len;
fs_file_t fs_files_queue[512];

const char *last_path(const char* path) {
	const char* sp = strrchr(path, '/');

	if (sp == NULL) {
		return path;
	}

	return strdup(sp + 1);
}

const char *base_path(const char* path) {
	const char* sp = strrchr(path, '/');

	if (sp == NULL) {
		return ".";
	}

	size_t len = sp - path;
	char *base = (char *)malloc(len + 1);

	memcpy(base, path, len);
	base[len] = '\0';

	return base;
}

const char *make_relative(const char *cwd, const char *path) {
	if (strlen(cwd) > strlen(path)) {
		return path;
	}
	while (*cwd != '\0' && *cwd == *path) {
		cwd++;
		path++;
	}
	while (*path != '\0' && *path == '/') {
		path++;
	}
	return path;
}

const char *relative_path_of_exe(void) {
	#ifndef __linux__ 
		#error "not portable to places other than linux"
	#endif

	char *scratch = (char *)alloc_scratch(0);
	ssize_t len = readlink("/proc/self/exe", scratch, PATH_MAX);
	if (len < 0) {
		err_without_pos("error: could not read `/proc/self/exe`");
	}
	scratch[len] = '\0';
	const char *exe_path = base_path(scratch); // will dup
	if (!getcwd(scratch, PATH_MAX)) {
		err_without_pos("error: could not get current working directory");
	}
	return strdup(make_relative(scratch, exe_path));
}

static fs_rnode_t new_node(void) {
	assert(fs_node_arena_len < ARRAYLEN(fs_node_arena));
	return fs_node_arena_len++;
}

// retargetable embedded compiler
#define FILE_EXTENSION "rec"

bool is_our_ext(const char *fp) {
    size_t len = strlen(fp);
    if (len < strlen(FILE_EXTENSION)) {
        return false;
    }
    return strcmp(fp + len - strlen(FILE_EXTENSION), FILE_EXTENSION) == 0;
}

// there are upsides and downsides to using a mmap approach.
// it doesn't matter to me one bit, i used mmap in stas.
// these files are small and PROT_READ, i assume the kernel would do some magic anyway.
// mmapping is infinitely more elegant than reading the file into memory through f* calls
static void _fs_slurp_file_with_size(const char *p, fs_rnode_t module, size_t size) {
	void *ptr = NULL;

	if (size != 0) {
		int fd = open(p, O_RDONLY);
		ptr = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
		close(0);

		// should i add MAP_POPULATE?
		// it would help, i am reading the whole thing left to right

		// i assume open won't fail, if it does, mmap will
		if (ptr == MAP_FAILED) {
			err_without_pos("error: failed to mmap file '%s' - %s\n", p, strerror(errno));
		}
	}

	assert(fs_files_queue_len < ARRAYLEN(fs_files_queue));
	fs_file_t *file = &fs_files_queue[fs_files_queue_len++];

	file->fp = p;
	file->data = (u8 *)ptr;
	file->len = size;
	file->module = module;
}

void fs_slurp_file(const char *p, fs_rnode_t mod) {
	struct stat statbuf;
	if (stat(p, &statbuf)) {
		err_without_pos("error: failed to stat '%s' - %s\n", p, strerror(errno));
	}
	return _fs_slurp_file_with_size(p, mod, statbuf.st_size);
}

void fs_slurp_dir(fs_rnode_t ref) {
	fs_node_t *node = &fs_node_arena[ref];
	assert(!node->is_src_scanned);
	node->is_src_scanned = true; // files have been read from this directory

	DIR *dir = opendir(node->path);
	
	if (!dir) {
		err_without_pos("error: failed to open directory '%s' - %s\n", node->path, strerror(errno));
	}

	struct dirent *entry;
	struct stat statbuf;

	while ((entry = readdir(dir)) != NULL) {
		// .
		// ..
		// .git
		if (entry->d_name[0] == '.') {
			continue;
		}

		// not all will be kept around, but leaks are okay
		char *concat_name;
		asprintf(&concat_name, "%s/%s", node->path, entry->d_name);

		if (stat(concat_name, &statbuf)) {
			err_without_pos("error: failed to stat '%s' - %s\n", concat_name, strerror(errno));
		}

		if (S_ISREG(statbuf.st_mode) && is_our_ext(entry->d_name)) {
			_fs_slurp_file_with_size(concat_name, ref, statbuf.st_size);
		}
	}
}

static fs_rnode_t _fs_make_directory(const char *path, fs_rnode_t parent, bool read_name, bool slurp) {
	fs_rnode_t ref = new_node();
	fs_node_t *node = &fs_node_arena[ref];

	node->path = path;
	node->parent = parent;
	node->is_src_scanned = slurp; // files have been read from this directory

	if (read_name) {
		// src/mod1 -> mod1
		node->name = sv_move(last_path(path));
	} else {
		node->name = -1;
	}

	DIR *dir = opendir(node->path);

	if (!dir) {
		err_without_pos("error: failed to open directory '%s' - %s\n", node->path, strerror(errno));
	}

	u32 our_files = 0;
	struct dirent *entry;
	struct stat statbuf;

	fs_rnode_t *children = NULL;

	while ((entry = readdir(dir)) != NULL) {
		// .
		// ..
		// .git
		if (entry->d_name[0] == '.') {
			continue;
		}

		// not all will be kept around, but leaks are okay
		char *concat_name;
		asprintf(&concat_name, "%s/%s", node->path, entry->d_name);

		// i don't like malloc, calling means that it's giving away memory that you should hand back.
		// i prefer a `forever_alloc` model, since we're never going to free it anyway.
		// `asprintf` is a nice and clean interface, but it uses malloc. sigh.
		//
		// TODO: eventually reconcile `malloc` and bump ptr allocators.

		if (stat(concat_name, &statbuf)) {
			err_without_pos("error: failed to stat '%s' - %s\n", concat_name, strerror(errno));
		}

		if (S_ISDIR(statbuf.st_mode)) {
			fs_rnode_t child = _fs_make_directory(concat_name, ref, true, false);
			arrpush(children, child);
		} else if (S_ISREG(statbuf.st_mode) && is_our_ext(entry->d_name)) {
			our_files++;
			if (slurp) {
				_fs_slurp_file_with_size(concat_name, ref, statbuf.st_size);
			}
		}
	}

	node->our_files = our_files;
	node->children = children;
	node->children_len = arrlen(children);

	if (closedir(dir)) {
		err_without_pos("error: failed to close directory '%s' - %s\n", node->path, strerror(errno));
	}

	return ref;
}

// eagerly find all directories recursively but lazily find files.
// this tree structure can be matched on easily to locate modules.
//
// import mod1.mod2.mod3
// - `mod1/ and `mod1/mod2` should not be read! 
//
// this will also catch errors quicker, such as importing a module
// with no files in it.
// call this with a circular symlink? that's on you.
//
fs_rnode_t fs_register_root(const char *p, bool is_main, bool slurp) {
	// roots can't have names, unless they're the main module
	fs_rnode_t root = _fs_make_directory(p, (fs_rnode_t)-1, false, slurp);

	if (is_main) {
		fs_node_arena[root].name = sv_move("main");
		fs_node_arena[root].is_main = true;
	}

	assert(fs_node_roots_len < ARRAYLEN(fs_node_roots));
	fs_node_roots[fs_node_roots_len++] = root;

	return root;
}

// returns -1 for none
// also COULD return src, check for this
static fs_rnode_t _fs_locate_node(fs_rnode_t src, istr_t *path, u32 path_len) {
	fs_node_t *srcp = &fs_node_arena[src];

	for (u32 i = 0; i < path_len; i++) {
		istr_t name = path[i];
		fs_rnode_t child;
		fs_node_t *childp;

		bool found = false;
		for (u32 j = 0; j < srcp->children_len; j++) {
			child = srcp->children[j];
			childp = &fs_node_arena[child];
			if (childp->name == name) {
				found = true;	
				break;
			}
		}

		if (!found) {
			return (fs_rnode_t)-1;
		}

		srcp = childp;
		src = child;
	}

	return src;
}

static u32 _fs_is_root(fs_rnode_t module) {
	for (u32 i = 0; i < fs_node_roots_len; i++) {
		if (fs_node_roots[i] == module) {
			return true;
		}
	}
	return false;
}

static u32 _fs_module_symbol_str_conv(fs_rnode_t module, u8 *p) {
	u32 nwritten = 0;
	fs_node_t *node = &fs_node_arena[module];
	if (node->parent != (fs_rnode_t)-1 && !_fs_is_root(node->parent)) {
		nwritten = _fs_module_symbol_str_conv(node->parent, p);
		p += nwritten;
		*p++ = '.', nwritten++;
	}
	const char *sv = sv_from(node->name);
	nwritten += ptrcpy(p, (u8 *)sv, strlen(sv));
	return nwritten;
}

// symbol can be -1
const char *fs_module_symbol_sv(fs_rnode_t module, istr_t symbol) {
	// module: mod1.mod2.mod3 <-- follow chain from mod3
	// symbol: add()
	// return: mod1.mod2.mod3.add

	u8 *p = alloc_scratch(0);

	u32 nwritten = _fs_module_symbol_str_conv(module, p);
	if (symbol != (istr_t)-1) {
		p[nwritten++] = '.';
		const char *sv = sv_from(symbol);
		nwritten += ptrcpy(p + nwritten, (u8 *)sv, strlen(sv));
	}
	p[nwritten++] = '\0';
	(void)alloc_scratch(nwritten);

	return (const char *)p;
}

istr_t fs_module_symbol_str(fs_rnode_t module, istr_t symbol) {
	const char *p = fs_module_symbol_sv(module, symbol);
	istr_t intern = sv_intern((u8*)p, strlen(p));
	alloc_reset((u8*)p);
	return intern;
}

static const char *_path_to_str(istr_t *path, u32 path_len) {
	u8 *p = alloc_scratch(0);
	u8 *po = p;
	for (u32 i = 0; i < path_len; i++) {
		const char *sv = sv_from(path[i]);
		p += ptrcpy(p, (u8 *)sv, strlen(sv));
		if (i + 1 < path_len) {
			*p++ = '.';
		}
	}
	*p = '\0';
	alloc_scratch(p - po + 1);

	return (const char *)po;
}

static fs_rnode_t _fs_locate_node_from_all(fs_rnode_t src, istr_t *path, u32 path_len, loc_t loc) {
	// case 1:
	//   find the module relative to `src`
	// case 2:
	//   find the module relative to roots
	// case 3:
	//   error!

	// case 1:
	fs_rnode_t found = _fs_locate_node(src, path, path_len);

	if (found != (fs_rnode_t)-1 && found != src) {
		return found;
	}

	// case 2:
	for (u32 i = 0; i < fs_node_roots_len; i++) {
		fs_rnode_t root = fs_node_roots[i];
		found = _fs_locate_node(root, path, path_len);

		if (found != (fs_rnode_t)-1 && found != src) {
			return found;
		}
	}

	err_with_pos(loc, "error: could not find module `%s`", _path_to_str(path, path_len));
}

// import mod1.mod2.mod3
//
fs_rnode_t fs_register_import(fs_rnode_t src, fs_rnode_t *path, u32 path_len, loc_t loc) {
	// case 1:
	//   find the module relative to `src`
	// case 2:
	//   find the module relative to roots
	// case 3:
	//   error!

	fs_rnode_t found = _fs_locate_node_from_all(src, path, path_len, loc);

	if (fs_node_arena[found].our_files == 0) {
		err_with_pos(loc, "error: module `%s` has no files", _path_to_str(path, path_len));
	}
	if (fs_node_arena[found].is_src_scanned) {
		return found;
	}
	fs_slurp_dir(found);
	return found;
}

fs_file_t *fs_filep(fs_rfile_t ref) {
	return &fs_files_queue[ref];
}

fs_node_t *fs_nodep(fs_rnode_t ref) {
	return &fs_node_arena[ref];
}

void _fs_dump_tree(fs_rnode_t node, bool is_root, u32 indent) {
	fs_node_t *nodep = &fs_node_arena[node];

	for (u32 i = 0; i < indent; i++) {
		eprintf("\t");
	}

	const char *name;

	if (!is_root || nodep->is_main) {
		name = sv_from(nodep->name);
	} else {
		name = nodep->path;
	}

	eprintf("%s: (%u src files)%s\n", name, nodep->our_files, nodep->is_src_scanned ? " [imported]" : "");

	for (u32 i = 0; i < nodep->children_len; i++) {
		fs_rnode_t child = nodep->children[i];
		_fs_dump_tree(child, false, indent + 1);
	}
}

void fs_dump_tree(void) {
	for (u32 i = 0; i < fs_node_roots_len; i++) {
		fs_rnode_t root = fs_node_roots[i];
		_fs_dump_tree(root, true, 0);
	}
}

fs_rfile_t fs_register_repl(void) {
	static fs_rfile_t repl = -1;

	if (repl == (fs_rfile_t)-1) {
		fs_rnode_t ref = new_node();
		fs_node_t *node = &fs_node_arena[ref];
		*node = (fs_node_t){
			.name = sv_move("main"),
			.is_main = true,
			.is_src_scanned = true,
			.our_files = 1,
			.parent = -1,
		};

		// TODO: why no new_file() for this, we have new_node() already?
		repl = fs_files_queue_len++;
		fs_file_t *file = &fs_files_queue[repl];
		file->module = ref;
	}

	return repl;
}
