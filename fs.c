#include "all.h"

#define __USE_GNU

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "stb_ds.h"

// TODO: find name and extension
#define FILE_EXTENSION "rec"

static const char *last_path(const char* path);
static const char *base_path(const char* path);
static const char *make_relative(const char *cwd, const char *path);
static bool is_our_ext(const char *fp);

// TODO: i don't want to make this public just expose a single function to handle file or directory inputs

// retargetable embedded compiler

u32 fs_files_queue_len;
file_t fs_files_queue[512];
u32 fs_mod_arena_len;
mod_t fs_mod_arena[128];

u32 fs_roots_len;
rmod_t fs_roots[8];

static rmod_t _fs_make_stub(const char *dp, rmod_t parent) {
	rmod_t id = fs_mod_arena_len++;
	mod_t *mod = MOD_PTR(id);
	mod->on_disk.is_stub = true;
	mod->on_disk.path = dp;
	return id;
}

static void _fs_slurp_file_with_size(const char *fp, rmod_t mod, size_t size) {
	void *ptr = NULL;

	if (size != 0) {
		int fd = open(fp, O_RDONLY);
		ptr = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
		close(0);
		if (ptr == MAP_FAILED) {
			err_without_pos("failed to mmap file '%s' (errno: %s)", fp, strerror(errno));
		}
	}

	file_t *file = FILE_PTR(fs_files_queue_len++);

	file->fp = fp;
	file->data = (u8 *)ptr;
	file->len = size;
	file->mod = mod;
}

// ./compiler file.ext -> is_main: true, slurp: false
// ./compiler .        -> is_main: true, slurp: true
static rmod_t _fs_make_directory(const char *dp, rmod_t parent, bool is_main, bool slurp) {
	rmod_t ref = fs_mod_arena_len++;
	mod_t *mod = MOD_PTR(ref);

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

	rmod_t *children = NULL;

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

		if (S_ISDIR(statbuf.st_mode)) {
			// lazily load folders and source files
			rmod_t child = _fs_make_stub(concat_name, ref);
			arrpush(children, child);
		} else if (S_ISREG(statbuf.st_mode) && is_our_ext(entry->d_name)) {
			files_count++;
			if (slurp) {
				_fs_slurp_file_with_size(concat_name, ref, statbuf.st_size);
			}
		}
	}

	mod->on_disk.files_count = files_count;
	mod->on_disk.children = children;
	mod->on_disk.children_len = arrlen(children);

	if (closedir(dir)) {
		err_without_pos("failed to close directory '%s' (errno: %s)", mod->on_disk.path, strerror(errno));
	}

	return ref;
}

static void _fs_slurp_file(const char *p, rmod_t mod) {
	struct stat statbuf;
	if (stat(p, &statbuf)) {
		err_without_pos("failed to stat '%s' (errno: %s)", p, strerror(errno));
	}
	_fs_slurp_file_with_size(p, mod, statbuf.st_size);
	fs_mod_arena[mod].on_disk.files_count++;
}

static rmod_t _fs_set_entry_root(const char *dp) {
	rmod_t mod = _fs_make_directory(dp, -1, true, true);
	fs_roots[fs_roots_len++] = mod; // assign root
	return mod;
}

static rmod_t _fs_set_entry_file(const char *fp) {
	const char *bp = base_path(fp);
	rmod_t mod = _fs_make_directory(bp, -1, true, false);
	fs_roots[fs_roots_len++] = mod; // assign root
	_fs_slurp_file(fp, mod);
	return mod;
}

void fs_set_entry_argp(const char *argp) {
	if (is_our_ext(argp)) {
		_fs_set_entry_file(argp);
	} else {
		_fs_set_entry_root(argp);
	}
}

// roots are searched left to right, for their enclosing directories
rmod_t fs_register_root(const char *dp) {
	rmod_t mod = _fs_make_directory(dp, (rmod_t)-1, false, false);
	fs_roots[fs_roots_len++] = mod; // assign root
	return mod;
}

rfile_t fs_set_entry_repl(void) {
	const char *dp = get_current_dir_name();
	rmod_t mod = _fs_make_directory(dp, (rmod_t)-1, true, false);
	mod_t *modp = MOD_PTR(mod);

	modp->on_disk.files_count++;

	rfile_t file = fs_files_queue_len++;
	file_t *filep = FILE_PTR(file);
	filep->mod = mod;
	filep->fp = "<repl>";

	return file;
}

static bool _fs_is_root(rmod_t mod) {
	for (u32 i = 0; i < fs_mod_arena_len; i++) {
		if (fs_roots[i] == mod) {
			return true;
		}
	}
	return false;
}

static u32 _fs_module_symbol_str_conv(rmod_t mod, u8 *p) {
	u32 nwritten = 0;
	mod_t *node = MOD_PTR(mod);
	if (node->on_disk.parent != (rmod_t)-1 && !_fs_is_root(node->on_disk.parent)) {
		nwritten = _fs_module_symbol_str_conv(node->on_disk.parent, p);
		p += nwritten;
		*p++ = '.', nwritten++;
	}
	const char *sv = sv_from(node->on_disk.name);
	nwritten += ptrcpy(p, (u8 *)sv, strlen(sv));
	return nwritten;
}

// symbol can be -1
const char *fs_module_symbol_str(rmod_t mod, istr_t symbol) {
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

istr_t fs_module_symbol_sv(rmod_t mod, istr_t symbol) {
	const char *p = fs_module_symbol_str(mod, symbol);
	istr_t intern = sv_intern((u8*)p, strlen(p));
	alloc_reset((u8*)p);
	return intern;
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
