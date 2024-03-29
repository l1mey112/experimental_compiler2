#define __USE_GNU
#define _GNU_SOURCE

#include "all.h"

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

// TODO: find name and extension
#define FILE_EXTENSION "rec"

static const char *last_path(const char* path);
static const char *base_path(const char* path);
static const char *make_relative(const char *cwd, const char *path);
static bool is_our_ext(const char *fp);
static const char *relative_directory_of_exe(void);

// TODO: i don't want to make this public just expose a single function to handle file or directory inputs

// retargetable embedded compiler

u32 fs_files_queue_len;
fs_file_t fs_files_queue[512];
u32 fs_mod_arena_len;
fs_mod_t fs_mod_arena[128];

u32 fs_roots_len;
fs_rmod_t fs_roots[8];

static fs_rmod_t _fs_new_stub(const char *dp, fs_rmod_t parent) {
	fs_rmod_t id = fs_mod_arena_len++;
	fs_mod_t *mod = MOD_PTR(id);
	mod->on_disk.is_stub = true;
	mod->on_disk.path = dp;
	mod->on_disk.parent = parent;
	mod->on_disk.name = sv_move(last_path(dp));
	return id;
}

static void _fs_slurp_file_with_size(const char *fp, fs_rmod_t mod, size_t size) {
	void *ptr = NULL;

	if (size != 0) {
		int fd = open(fp, O_RDONLY);
		ptr = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
		close(0);
		if (ptr == MAP_FAILED) {
			err_without_pos("failed to mmap file '%s' (errno: %s)", fp, strerror(errno));
		}
	}

	fs_file_t *file = FILE_PTR(fs_files_queue_len++);

	file->fp = fp;
	file->data = (u8 *)ptr;
	file->len = size;
	file->mod = mod;
}

// assumes that the ptr from `ref` is zero initialised or already initialised
// god this needs a redo to make it truly fs lazy
static fs_rmod_t _fs_make_directory_real(fs_rmod_t ref, const char *dp, fs_rmod_t parent, bool is_main, bool slurp) {
	fs_mod_t *mod = MOD_PTR(ref);

	mod->on_disk.is_stub = false;

	if (mod->on_disk.is_files_read) {
		slurp = false;
	}
	
	// 1. stub
	// 2. dir
	// 3. dir + files

	bool read_dirs = mod->on_disk.children == NULL;

	mod->on_disk.path = dp;
	mod->on_disk.parent = parent;

	if (is_main) {
		mod->on_disk.name = sv_move("main");
	} else {
		mod->on_disk.name = sv_move(last_path(dp));
	}

	DIR *dir = opendir(dp);
	if (!dir) {
		err_without_pos("failed to open directory '%s' (errno: %s)", mod->on_disk.path, strerror(errno));
	}

	u32 files_count = 0;
	struct dirent *entry;
	struct stat statbuf;

	fs_rmod_t *children = NULL;

	while ((entry = readdir(dir)) != NULL) {
		// .
		// ..
		// .git
		if (entry->d_name[0] == '.') {
			continue;
		}

		char *concat_name;
		asprintf(&concat_name, "%s/%s", mod->on_disk.path, entry->d_name);

		if (stat(concat_name, &statbuf)) {
			err_without_pos("failed to stat '%s' (errno: %s)", concat_name, strerror(errno));
		}

		if (read_dirs && S_ISDIR(statbuf.st_mode)) {
			// lazily load folders and source files
			fs_rmod_t child = _fs_new_stub(concat_name, ref);
			arrpush(children, child);
		} else if (slurp && S_ISREG(statbuf.st_mode) && is_our_ext(entry->d_name)) {
			files_count++;
			if (slurp) {
				_fs_slurp_file_with_size(concat_name, ref, statbuf.st_size);
			}
		}
	}

	mod->on_disk.is_files_read = slurp;
	mod->on_disk.files_count = files_count;
	mod->on_disk.children = children;
	mod->on_disk.children_len = arrlen(children);

	if (closedir(dir)) {
		// why the fuck Bad file descriptor
		// err_without_pos("failed to close directory '%s' (errno: %s)", mod->on_disk.path, strerror(errno));
	}

	return ref;
}

// ./compiler file.ext -> is_main: true, slurp: false
// ./compiler .        -> is_main: true, slurp: true
static fs_rmod_t _fs_make_directory(const char *dp, fs_rmod_t parent, bool is_main, bool slurp) {
	fs_rmod_t ref = fs_mod_arena_len++;
	return _fs_make_directory_real(ref, dp, parent, is_main, slurp);
}

static void _fs_make_stub(fs_rmod_t stub, bool slurp) {
	fs_mod_t *mod = MOD_PTR(stub);
	if (!mod->on_disk.is_stub) {
		return;
	}
	_fs_make_directory_real(stub, mod->on_disk.path, mod->on_disk.parent, false, slurp);
}

static void _fs_slurp_file(const char *p, fs_rmod_t mod) {
	struct stat statbuf;
	if (stat(p, &statbuf)) {
		err_without_pos("failed to stat '%s' (errno: %s)", p, strerror(errno));
	}
	_fs_slurp_file_with_size(p, mod, statbuf.st_size);
}

static fs_rmod_t _fs_set_entry_root(const char *dp) {
	fs_rmod_t mod = _fs_make_directory(dp, -1, true, true);
	fs_roots[fs_roots_len++] = mod; // assign root
	return mod;
}

static fs_rmod_t _fs_set_entry_file(const char *fp) {
	const char *bp = base_path(fp);
	fs_rmod_t mod = _fs_make_directory(bp, -1, true, false);
	fs_roots[fs_roots_len++] = mod; // assign root
	_fs_slurp_file(fp, mod);
	fs_mod_arena[mod].on_disk.files_count++;
	return mod;
}

/* void _fs_import_lib(void) {
	const char *exe_path = relative_path_of_exe();
	char *lib_path;
	if (*exe_path == '\0') {
		lib_path = "lib";
	} else {
		asprintf(&lib_path, "%s/lib", exe_path);
	}
	printf("test: %s\n", lib_path);
} */

void fs_set_entry_argp(const char *argp) {
	if (is_our_ext(argp)) {
		_fs_set_entry_file(argp);
	} else {
		_fs_set_entry_root(argp);
	}

	// _fs_import_lib();
}

// roots are searched left to right, for their enclosing directories
fs_rmod_t fs_register_root(const char *dp) {
	fs_rmod_t mod = _fs_make_directory(dp, RMOD_NONE, false, false);
	fs_roots[fs_roots_len++] = mod; // assign root
	return mod;
}

fs_rfile_t fs_set_entry_repl(void) {
	const char *dp = get_current_dir_name();
	fs_rmod_t mod = _fs_make_directory(dp, RMOD_NONE, true, false);
	fs_mod_t *modp = MOD_PTR(mod);

	modp->on_disk.files_count++;

	fs_rfile_t file = fs_files_queue_len++;
	fs_file_t *filep = FILE_PTR(file);
	filep->mod = mod;
	filep->fp = "<repl>";

	return file;
}

static bool _fs_is_root(fs_rmod_t mod) {
	for (u32 i = 0; i < fs_mod_arena_len; i++) {
		if (fs_roots[i] == mod) {
			return true;
		}
	}
	return false;
}

static u32 _fs_module_symbol_str_conv(fs_rmod_t mod, u8 *p) {
	u32 nwritten = 0;
	fs_mod_t *node = MOD_PTR(mod);
	if (node->on_disk.parent != RMOD_NONE && !_fs_is_root(node->on_disk.parent)) {
		nwritten = _fs_module_symbol_str_conv(node->on_disk.parent, p);
		p += nwritten;
		*p++ = '.', nwritten++;
	}
	const char *sv = sv_from(node->on_disk.name);
	nwritten += ptrcpy(p, (u8 *)sv, strlen(sv));
	return nwritten;
}

// symbol can be -1
const char *fs_module_symbol_str(fs_rmod_t mod, istr_t symbol) {
	// module: mod1.mod2.mod3 <-- follow chain from mod3
	// symbol: add()
	// return: mod1.mod2.mod3.add

	u8 *p = alloc_scratch(0);

	u32 nwritten = _fs_module_symbol_str_conv(mod, p);
	if (symbol != ISTR_NONE) {
		p[nwritten++] = '.';
		const char *sv = sv_from(symbol);
		nwritten += ptrcpy(p + nwritten, (u8 *)sv, strlen(sv));
	}
	p[nwritten++] = '\0';
	(void)alloc_scratch(nwritten);

	return (const char *)p;
}

istr_t fs_module_symbol_sv(fs_rmod_t mod, istr_t symbol) {
	const char *p = fs_module_symbol_str(mod, symbol);
	istr_t intern = sv_intern((u8*)p, strlen(p));
	alloc_reset((u8*)p);
	return intern;
}

istr_t fs_module_symbol_selector(istr_t qualified_name, istr_t selector) {
	u8 *p = alloc_scratch(0);
	const char *sv = sv_from(qualified_name);
	u32 nwritten = ptrcpy(p, (u8 *)sv, strlen(sv));
	p[nwritten++] = ':';
	sv = sv_from(selector);
	nwritten += ptrcpy(p + nwritten, (u8 *)sv, strlen(sv));
	p[nwritten++] = '\0';
	(void)alloc_scratch(nwritten);
	return sv_intern(p, nwritten - 1);
}

static const char *_fs_path_to_str(istr_t *path, u32 path_len) {
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

// the final path is the target module
static fs_rmod_t _fs_locate_node(fs_rmod_t mod, istr_t *path, u32 path_len) {
	for (u32 i = 0; i < path_len; i++) {
		istr_t name = path[i];
		fs_mod_t *modp = MOD_PTR(mod);

		bool found = false;
		for (u32 j = 0; j < modp->on_disk.children_len; j++) {
			fs_rmod_t child = modp->on_disk.children[j];
			fs_mod_t *childp = MOD_PTR(child);
			if (childp->on_disk.name == name) {
				mod = child;
				found = true;
				break;
			}
		}

		if (!found) {
			return RMOD_NONE;
		}

		bool is_last = i + 1 == path_len;
		_fs_make_stub(mod, is_last);
	}

	return mod;
}

fs_rmod_t fs_register_import(fs_rmod_t src, istr_t *path, u32 path_len, loc_t onerror) {
	// case 1:
	//   find the module relative to `src`
	// case 2:
	//   find the module relative to roots
	// case 3:
	//   error!

	assert(path_len > 0);

	// case 1:
	fs_rmod_t found = _fs_locate_node(src, path, path_len);

	assert(found != src); // i don't know how this could happen
	if (found != RMOD_NONE) {
		goto found;
	}

	// case 2:
	for (u32 i = 0; i < fs_roots_len; i++) {
		found = _fs_locate_node(fs_roots[i], path, path_len);
		if (found != RMOD_NONE) {
			goto found;
		}
	}

	// case 3:
	err_with_pos(onerror, "could not find module `%s`", _fs_path_to_str(path, path_len));
found:
	if (fs_mod_arena[found].on_disk.files_count == 0) {
		err_with_pos(onerror, "module `%s` has no files", _fs_path_to_str(path, path_len));
	}
	return found;
}

u32 _fs_dt_tabs;

#define TPRINTF(...) do { for (u32 i = 0; i < _fs_dt_tabs; i++) { printf("  "); } printf(__VA_ARGS__); } while (0)

static void _fs_dump_tree(fs_rmod_t i) {
	TPRINTF("");
	fs_mod_t *mod = MOD_PTR(i);
	if (mod->on_disk.is_stub) {
		printf("mod %d: (stub) %s\n", i, mod->on_disk.path);
		return;
	} else {
		printf("mod %d: %s\n", i, sv_from(mod->on_disk.name));
	}
	_fs_dt_tabs++;
	if (mod->on_disk.is_files_read) {
		TPRINTF("files: %d\n", mod->on_disk.files_count);
	}
	for (u32 j = 0; j < mod->on_disk.children_len; j++) {
		_fs_dump_tree(mod->on_disk.children[j]);
	}
	_fs_dt_tabs--;
}

void fs_dump_tree(void) {
	_fs_dt_tabs = 0;
	for (u32 i = 0; i < fs_roots_len; i++) {
		_fs_dump_tree(fs_roots[i]);
	}
}

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

bool is_our_ext(const char *fp) {
    size_t len = strlen(fp);
    if (len < strlen(FILE_EXTENSION)) {
        return false;
    }
    return strcmp(fp + len - strlen(FILE_EXTENSION), FILE_EXTENSION) == 0;
}

const char *relative_directory_of_exe(void) {
	#ifndef __linux__ 
		#error "not portable to places other than linux"
	#endif

	char *scratch = (char *)alloc_scratch(0);
	ssize_t len = readlink("/proc/self/exe", scratch, PATH_MAX);
	if (len < 0) {
		err_without_pos("could not read `/proc/self/exe`");
	}
	scratch[len] = '\0';
	const char *exe_path = base_path(scratch); // will dup
	if (!getcwd(scratch, PATH_MAX)) {
		err_without_pos("could not get current working directory");
	}
	return strdup(make_relative(scratch, exe_path));
}
