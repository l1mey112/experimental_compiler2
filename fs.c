#include <string.h>
#define __USE_GNU
#define _GNU_SOURCE

#include "all.h"

#include <dirent.h>
#include <errno.h> // IWYU pragma: keep             (im definitely using `errno`, so why clangd, why?)
#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

// TODO: find name and extension
#define FILE_EXTENSION "rec"

static const char *global_cwd;
static u32 global_cwd_len;

static const char *last_path(const char* path);
static bool path_collides(const char *path, const char *npath);
static void base_path_in_place(char* path);
static bool is_our_ext(const char *fp);
static const char *absolute_directory_of_exe(void);

// TODO: delete MOD_PTR
// TODO: delete repl mode

u32 fs_files_queue_len;
fs_file_t fs_files_queue[512];
u32 fs_mod_arena_len;
fs_mod_t fs_mod_arena[128];
u32 fs_roots_len;
fs_rmod_t fs_roots[8];
u32 fs_platforms_len;
fs_platform_t fs_platforms[32];

fs_rmod_t main_module;
fs_rmod_t rt_module;

char *build_token;

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

	static u64 bs_search_next_offset = 0;

	fs_file_t *file = &fs_files_queue[fs_files_queue_len++];

	file->fp = fp;
	file->data = (u8*)ptr;
	file->len = size;
	file->mod = mod;
	file->bs_offset_pos = bs_search_next_offset;

	bs_search_next_offset += size;
}

static void _fs_read_file(const char *fp, fs_rmod_t mod) {
	struct stat statbuf;
	if (stat(fp, &statbuf)) {
		err_without_pos("failed to stat '%s' (errno: %s)", fp, strerror(errno));
	}
	_fs_read_file_with_size(fp, mod, statbuf.st_size);
}

// set root to self as well if none
static fs_rmod_t _fs_nm(fs_mod_t mod) {
	u32 l = fs_mod_arena_len;
	if (mod.root == RMOD_NONE) {
		mod.root = l;
	}
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
		.root = parentp->root,
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

static void _fs_register_runtime(const char *prefix, const char *dp) {
	fs_platforms[fs_platforms_len++] = (fs_platform_t){
		.name = prefix,
		.rt_path = dp,
	};
}

// doesn't care about module kind, only performs one iteration depth of walking
// only call once. you don't want duplicates
static void _fs_populate(fs_rmod_t rmod, bool read_files) {
	fs_mod_t *mod = &fs_mod_arena[rmod];

	DIR *dir = opendir(mod->path);
	if (!dir) {
		err_without_pos("failed to open directory '%s' (errno: %s)", mod->path, strerror(errno));
	}

	assert(mod->kind != MOD_CHILD);
	bool read_dirs = mod->kind != MOD_LAZY_CHILD;

	struct dirent *entry;
	struct stat statbuf;

	nextstep: while ((entry = readdir(dir)) != NULL) {
		char *file_head = entry->d_name;
		
		// . | .. | .git
		if (file_head[0] == '.') {
			continue;
		}

		char *fsp;
		asprintf(&fsp, "%s/%s", mod->path, file_head);

		// dangling pointer if we keep this around
		char *plus_p = strrchr(file_head, '+');

		if (stat(fsp, &statbuf)) {
			err_without_pos("failed to stat '%s' (errno: %s)", fsp, strerror(errno));
		}

		if (S_ISDIR(statbuf.st_mode)) {
			// check for collisions with other roots
			for (u32 i = 0; i < fs_roots_len; i++) {
				fs_rmod_t rroot = fs_roots[i];
				if (rroot == mod->root) {
					continue;
				}
				fs_mod_t *root = &fs_mod_arena[rroot];
				if (path_collides(fsp, root->path)) {
					goto nextstep;
				}
			}

			if (plus_p) {
				if (mod->parent == RMOD_NONE && strcmp(plus_p + 1, "rt") == 0) {
					const char *prefix = strndup(file_head, plus_p - file_head);
					if (strlen(prefix) == 0) {
						err_without_pos("empty runtime platform prefix in directory '%s'", fs_make_relative(fsp));
					}
					_fs_register_runtime(prefix, fsp);
					continue;
				} else {
					plus_p++;
					err_without_pos("unknown postfix '+%s' in directory '%s'", plus_p, fs_make_relative(fsp));
				}
			}

			// lazily load folders and source files
			if (read_dirs) {
				fs_rmod_t child = _fs_nstub(fsp, rmod);
				arrpush(mod->children, child);
			}
		} else if (read_files && S_ISREG(statbuf.st_mode) && is_our_ext(file_head)) {
			// trim off the .rec and plus
			if (plus_p) {
				plus_p++;
				plus_p[strlen(plus_p) - 1 - strlen(FILE_EXTENSION)] = '\0';
			}

			// unknown postfix
			if (plus_p) {
				err_without_pos("unknown postfix '+%s' in file '%s'", plus_p, fs_make_relative(fsp));
			}

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
		.root = RMOD_NONE,
	});

	_fs_populate(rmod, false);
	_fs_nroot(rmod);
}

// only call this once
void fs_entrypoint(const char *argv) {
	// memoise cwd
	global_cwd = get_current_dir_name();
	global_cwd_len = strlen(global_cwd);

	// create lib, roots are searched in order
	char *lib_path;

	const char *exe_path = absolute_directory_of_exe();
	asprintf(&lib_path, "%s/lib", exe_path);
	fs_register_include(lib_path);

	const char *bp;
	bool is_single_file = is_our_ext(argv);

	argv = realpath(argv, NULL);
	if (argv == NULL) {
		err_without_pos("failed to resolve path '%s' (errno: %s)", argv, strerror(errno));
	}

	if (is_single_file) {
		// slurp file into the main module
		bp = strdup(argv);
		base_path_in_place((char *)bp);
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
		.root = RMOD_NONE,
	});

	// single files create a single module, then read
	// everything else in as supporting modules
	if (is_single_file) {
		// read in singular file at `argv`
		_fs_read_file(argv, main);
		fs_mod_arena[main].files_count = 1;
	}
	_fs_populate(main, !is_single_file);

	// register main module
	_fs_nroot(main);
	main_module = main; // setup global
}

const char *_fs_arch_string(arch_t arch) {
	switch (arch.kind) {
		#define X(arch, name) case arch: return name;
			X_ARCHS
		#undef X
	}
}

void _fs_materialise_platform(u32 i) {
	fs_platform_t *p = &fs_platforms[i];

	eprintf("rt_path: %s\n", fs_make_relative(p->rt_path));
	
	fs_rmod_t rmod = _fs_nm((fs_mod_t){
		.kind = MOD_RT,
		.short_name = sv_move("rt"),
		.key = sv_move("rt"),
		.path = p->rt_path,
		.parent = RMOD_NONE,
		.root = RMOD_NONE,
	});
	_fs_populate(rmod, true);
	_fs_nroot(rmod);
	rt_module = rmod; // setup global

	asprintf(&build_token, "target: %s-%s", _fs_arch_string(abi), p->name);
	return;
}

arch_t abi;

// only call this once
void fs_target(const char *arch, const char *platform, bool debug_symbols) {
	eprintf("selected target: %s-%s\n", arch, platform);
	eprintf("debug symbols: %s\n", debug_symbols ? "true" : "false");

	if (strcmp(arch, "stdc_64") == 0) {
		abi = (arch_t){
			.kind = ARCH_STDC_64,
			.ptr_size = 8,
			.debug_symbols = debug_symbols,
		};
	} else {
		err_without_pos("unknown architecture '%s'", arch);
	}

	// select platform
	for (u32 i = 0; i < fs_platforms_len; i++) {
		if (strcmp(fs_platforms[i].name, platform) == 0) {
			_fs_materialise_platform(i);
			return;
		}
	}

	err_without_pos("unknown platform '%s'", platform);
}

// the final path is the target module
// called on roots, will never resolve to a root
static fs_rmod_t _fs_locate_node(fs_rmod_t rmod, istr_t *path, u32 path_len) {
	for (u32 i = 0; i < path_len; i++) {
		istr_t name = path[i];
		fs_mod_t *o_mod = &fs_mod_arena[rmod];

		bool found = false;
		for (u32 j = 0, c = arrlenu(o_mod->children); j < c; j++) {
			fs_rmod_t child = o_mod->children[j];
			fs_mod_t *childp = &fs_mod_arena[child];
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
		p += ptrcpy(p, (u8*)sv, strlen(sv));
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

	// ascertain the kind of the module's root
	u8 kind;
	if (fs_mod_arena[src].root == RMOD_NONE) {
		kind = fs_mod_arena[src].kind;
	} else {
		kind = fs_mod_arena[fs_mod_arena[src].root].kind;
	}

	// case 2:
	// iterate over all roots in opposite order to what they were registered
	for (u32 i = fs_roots_len; i--; ) {
		fs_rmod_t rroot = fs_roots[i];
		fs_mod_t *root = &fs_mod_arena[rroot];
		
		// import directions
		//  - `include` cannot import from `main`
		//  - `include` cannot import from `runtime`
		//  - `main` cannot import from `runtime`
		//  - `runtime` can import from `main` namespace (local)

		if (kind == MOD_INCLUDE) {
			if (root->kind == MOD_MAIN || root->kind == MOD_RT) {
				continue;
			}
		} else if (kind == MOD_MAIN) {
			if (root->kind == MOD_RT) {
				continue;
			}
		}

		found = _fs_locate_node(rroot, path, path_len);
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

// check if `npath` resides within `path` or is equal to `path`
bool path_collides(const char *path, const char *npath) {
	size_t len = strlen(path);
	if (strncmp(path, npath, len) == 0) {
		if (npath[len] == '\0' || npath[len] == '/') {
			return true;
		}
	}
	return false;
}

// call with file path
// assumes no trailing slash
// /file
void base_path_in_place(char* path) {
	char* sp = strrchr(path, '/');

	assert(sp != NULL);

	if (sp == path) {
		path[1] = '\0';
	} else {
		*sp = '\0';
	}
}

// walk forward
const char *fs_make_relative(const char *path) {
	if (global_cwd_len > strlen(path)) {
		return path;
	}
	char *cwd = (char *)global_cwd;
	while (*cwd != '\0' && *cwd == *path) {
		cwd++;
		path++;
	}
	while (*path != '\0' && *path == '/') {
		path++;
	}
	if (*path == '\0') {
		return ".";
	}
	return path;
}

// no trailing slashes: /home/user
const char *absolute_directory_of_exe(void) {
	#ifndef __linux__ 
		#error "not portable to places other than linux"
	#endif

	char *exe = realpath("/proc/self/exe", NULL);
	base_path_in_place(exe);
	return exe;
}

u32 _fs_dt_tabs;

#define TPRINTF(...) do { for (u32 i = 0; i < _fs_dt_tabs; i++) { eprintf("  "); } eprintf(__VA_ARGS__); } while (0)

static void _fs_dump_tree(fs_rmod_t rmod) {
	TPRINTF("");
	fs_mod_t *mod = &fs_mod_arena[rmod];

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
		eprintf("<root>");
	} else {
		eprintf("%s", sv_from(mod->key));
	}

	eprintf(" [%s] %s", kind_str, fs_make_relative(mod->path));
	if (mod->files_count > 0) {
		eprintf(" (%u files)", mod->files_count);
	}
	eprintf("\n");

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

	for (u32 i = 0; i < fs_platforms_len; i++) {
		TPRINTF("runtime %u: %s %s\n", i, fs_platforms[i].name, fs_make_relative(fs_platforms[i].rt_path));
	}
}

lineinfo_t fs_reconstruct_lineinfo(loc_t loc) {
	// 1. list all files in an array in order of processing, store positions in the file
	//    offset by the last position. all files are a "next index" of eachother
	// 2. a location maps to a position which may be within any file
	// 3. perform a binary search to find the file encompassing the location in the shortest time
	// 4. then iterate char by char matching each `\n` to reconstruct the newlines in a file
	//    (possibly memoise this, there will be multiple diagnostics in one go and all errors most likely happen in a single file)

	// check if invalid
	assert(loc.len != 0);

	u32 l_idx = 0;
	u32 r_idx = fs_files_queue_len - 1;

	if (fs_files_queue_len == 0) {
		assert_not_reached();
	}

	while (l_idx != r_idx) {
		// m := ceil((L + R) / 2)
		u32 m = 1 + ((l_idx + r_idx - 1) / 2);

		fs_file_t *mp = &fs_files_queue[m];

		if (loc.pos < mp->bs_offset_pos) {
			r_idx = m - 1;
		} else {
			l_idx = m;
		}
	}

	fs_file_t *mp = &fs_files_queue[l_idx];

	if (!(loc.pos >= mp->bs_offset_pos && loc.pos + loc.len <= mp->bs_offset_pos + mp->len)) {
		// printf("loc.pos: %lu, mp->bs_offset_pos: %lu, loc.len: %u, mp->len: %lu\n", loc.pos, mp->bs_offset_pos, loc.len, mp->len);
		// printf("l_idx: %u, fp: %s\n", l_idx, mp->fp);
		assert_not_reached();
	}

	u32 pos = loc.pos - mp->bs_offset_pos;

	// TODO: memoise this... even at least for one file at a time

	u8 *p = mp->data;
	u32 col = 0;
	u32 line_nr = 0;
	for (u64 i = 0; i < pos; ++i) {
		if (p[i] == '\n') {
			line_nr++;
			col = 0;
		} else {
			col++;
		}
	}

	lineinfo_t lineinfo = {
		.line_nr = line_nr,
		.col = col,
		.pos = pos,
		.len = loc.len,
		.file = l_idx,
	};

	return lineinfo;
}
