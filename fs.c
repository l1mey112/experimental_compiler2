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

// TODO: delete MOD_PTR
// TODO: delete repl mode

u32 fs_files_queue_len;
fs_file_t fs_files_queue[512];
u32 fs_mod_arena_len;
fs_mod_t fs_mod_arena[128];

// order of left to right
u32 fs_roots_len;
fs_rmod_t fs_roots[8];

// register roots
// register ways to walk possible roots

static void _fs_read_file_with_size(const char *fp, fs_rmod_t mod, size_t size) {
	void *ptr = NULL;

	if (size != 0) {
		int fd = open(fp, O_RDONLY);
		ptr = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
		close(fd);
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

static void _fs_read_file(const char *fp, fs_rmod_t mod) {
	struct stat statbuf;
	if (stat(fp, &statbuf)) {
		err_without_pos("failed to stat '%s' (errno: %s)", fp, strerror(errno));
	}
	_fs_read_file_with_size(fp, mod, statbuf.st_size);
}

static fs_rmod_t _fs_nm(fs_mod_t mod) {
	u32 l = fs_mod_arena_len;
	fs_mod_arena[fs_mod_arena_len++] = mod;
	return l;
}

static fs_rmod_t _fs_nstub(const char *dp, fs_rmod_t parent) {
	fs_mod_t *parentp = &fs_mod_arena[parent];

	istr_t key;
	const char *short_name = last_path(dp);

	if (parentp->key != ISTR_NONE) {
		u8 *sp = alloc_scratch(0);
		const char *parent_key = sv_from(parentp->key);

		u32 len = sprintf((char*)sp, "%s.%s", parent_key, short_name);
		key = sv_intern(sp, len);
		alloc_reset(sp);
	} else {
		key = sv_move(short_name);
	}

	return _fs_nm((fs_mod_t){
		.kind = MOD_STUB,
		.key = key,
		.short_name = sv_move(short_name), // ownership
		.path = dp,
		.parent = parent,
		.children = NULL,
	});
}

static void _fs_nroot(fs_rmod_t rmod) {
	fs_roots[fs_roots_len++] = rmod;
}

istr_t fs_module_symbol(fs_rmod_t rmod, istr_t symbol) {
	fs_mod_t *mod = &fs_mod_arena[rmod];

	u8 *sp = alloc_scratch(0);

	const char *key = sv_from(mod->key);
	const char *short_name = sv_from(symbol);

	u32 len = sprintf((char*)sp, "%s.%s", key, short_name);
	istr_t sym = sv_intern(sp, len);
	alloc_reset(sp);

	return sym;
}

istr_t fs_module_symbol_selector(istr_t qualified_name, istr_t selector) {
	u8 *sp = alloc_scratch(0);

	const char *key = sv_from(qualified_name);
	const char *short_name = sv_from(selector);

	u32 len = sprintf((char*)sp, "%s:%s", key, short_name);
	istr_t sym = sv_intern(sp, len);
	alloc_reset(sp);

	return sym;
}

// slurp all files into a module then register it's root status

// doesn't care about module kind, only performs one iteration depth of walking
// only call once. you don't want duplicates
static void _fs_populate(fs_rmod_t rmod, bool read_files) {
	fs_mod_t *mod = &fs_mod_arena[rmod];

	DIR *dir = opendir(mod->path);
	if (!dir) {
		err_without_pos("failed to open directory '%s' (errno: %s)", mod->path, strerror(errno));
	}

	bool read_dirs;

	switch (mod->kind) {
		case MOD_LAZY_CHILD: {
			read_dirs = false;
			break;
		}
		case MOD_STUB:
		case MOD_MAIN:
		case MOD_INCLUDE: {
			read_dirs = true;
			break;
		}
		//
		case MOD_CHILD:
		case MOD_RT: {
			printf("kind: %d\n", mod->kind);
			assert_not_reached();
		}
	}

	struct dirent *entry;
	struct stat statbuf;

	while ((entry = readdir(dir)) != NULL) {
		// . | .. | .git
		if (entry->d_name[0] == '.') {
			continue;
		}

		char *fsp;
		asprintf(&fsp, "%s/%s", mod->path, entry->d_name);

		if (stat(fsp, &statbuf)) {
			err_without_pos("failed to stat '%s' (errno: %s)", fsp, strerror(errno));
		}

		if (read_dirs && S_ISDIR(statbuf.st_mode)) {
			// lazily load folders and source files
			fs_rmod_t child = _fs_nstub(fsp, rmod);
			arrpush(mod->children, child);
		} else if (read_files && S_ISREG(statbuf.st_mode) && is_our_ext(entry->d_name)) {
			_fs_read_file_with_size(fsp, rmod, statbuf.st_size);
			mod->files_count++;
		}
	}

	if (closedir(dir)) {
		err_without_pos("failed to close directory '%s' (errno: %s)", mod->path, strerror(errno));
	}

	if (mod->kind == MOD_STUB) {
		if (read_files) {
			mod->kind = MOD_CHILD;
		} else if (read_dirs) {
			mod->kind = MOD_LAZY_CHILD;
		}
	} else if (mod->kind == MOD_LAZY_CHILD && read_files) {
		mod->kind = MOD_CHILD;
	}
}

void fs_register_include(const char *dp) {
	fs_rmod_t rmod = _fs_nm((fs_mod_t){
		.kind = MOD_INCLUDE,
		.short_name = ISTR_NONE,
		.key = ISTR_NONE,
		.path = dp,
		.parent = RMOD_NONE,
	});

	_fs_populate(rmod, false);
	_fs_nroot(rmod);
}

// TODO: "make absolute path"
// TODO: "is path contained within"
// TODO: pretty_path() reads in absolute path and cwd 
// TODO: with the above path comparisons become easy

void fs_entrypoint(const char *argv) {
	const char *bp;
	bool is_single_file = is_our_ext(argv);
	
	if (is_single_file) {
		// slurp file into the main module
		bp = base_path(argv);
	} else {
		// slurp all files into main module
		bp = argv;
	}

	fs_rmod_t main = _fs_nm((fs_mod_t){
		.kind = MOD_MAIN,
		.short_name = sv_move("main"),
		.key = sv_move("main"),
		.path = bp,
		.parent = RMOD_NONE,
	});

	if (is_single_file) {
		// read in singular fiile at `argv`
		_fs_read_file(argv, main);
	}

	_fs_populate(main, !is_single_file);

	// create lib

	char *lib_path;

	const char *exe_path = relative_directory_of_exe();

	if (*exe_path == '\0') {
		lib_path = "lib";
	} else {
		asprintf(&lib_path, "%s/lib/", exe_path);
	}

	// roots are searched in order
	_fs_nroot(main);
	fs_register_include(lib_path);
}

// the final path is the target module
static fs_rmod_t _fs_locate_node(fs_rmod_t rmod, istr_t *path, u32 path_len) {
	for (u32 i = 0; i < path_len; i++) {
		istr_t name = path[i];
		fs_mod_t *o_mod = &fs_mod_arena[rmod];

		bool found = false;
		for (u32 j = 0, c = arrlenu(o_mod->children); j < c; j++) {
			fs_rmod_t child = o_mod->children[j];
			fs_mod_t *childp = MOD_PTR(child);
			if (childp->short_name == name) {
				rmod = child;
				found = true;
				break;
			}
		}

		if (!found) {
			return RMOD_NONE;
		}

		bool is_last = i + 1 == path_len;
		fs_mod_t *mod = &fs_mod_arena[rmod];

		if (mod->kind != MOD_CHILD) {
			_fs_populate(rmod, is_last);
		}
	}

	return rmod;
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
	if (fs_mod_arena[found].files_count == 0) {
		err_with_pos(onerror, "module `%s` has no files", _fs_path_to_str(path, path_len));
	}
	return found;
}

bool is_our_ext(const char *fp) {
	size_t len = strlen(fp);
	if (len < strlen(FILE_EXTENSION)) {
		return false;
	}
	return strcmp(fp + len - strlen(FILE_EXTENSION), FILE_EXTENSION) == 0;
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


u32 _fs_dt_tabs;

#define TPRINTF(...) do { for (u32 i = 0; i < _fs_dt_tabs; i++) { printf("  "); } printf(__VA_ARGS__); } while (0)

static void _fs_dump_tree(fs_rmod_t rmod) {
	TPRINTF("");
	fs_mod_t *mod = MOD_PTR(rmod);

	const char *kind_str;

	switch (mod->kind) {
		case MOD_STUB: kind_str = "stub"; break;
		case MOD_CHILD: kind_str = "child"; break;
		case MOD_LAZY_CHILD: kind_str = "lazy child"; break;
		case MOD_INCLUDE: kind_str = "include"; break;
		case MOD_RT: kind_str = "rt"; break;
		case MOD_MAIN: kind_str = "main"; break;
		default: {
			assert_not_reached();
		}
	}

	if (mod->key == ISTR_NONE) {
		printf("<root>");
	} else {
		printf("%s", sv_from(mod->key));
	}

	printf(" [%s] %s\n", kind_str, mod->path);

	_fs_dt_tabs++;
	for (u32 i = 0, c = arrlenu(mod->children); i < c; i++) {
		_fs_dump_tree(mod->children[i]);
	}
	_fs_dt_tabs--;
}

void fs_dump_tree(void) {
	_fs_dt_tabs = 0;
	for (u32 i = 0; i < fs_roots_len; i++) {
		_fs_dump_tree(fs_roots[i]);
	}
}