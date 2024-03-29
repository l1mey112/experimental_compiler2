#define STB_DS_IMPLEMENTATION
#include "all.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include "ansi.h"

// leak everything
const char* __asan_default_options(void) { return "detect_leaks=0"; }

err_diag_t err_diag;

void print_diag_with_pos(const char *type, loc_t loc, const char *fmt, ...) {
	char err_string[256];
	
	va_list args;
	va_start(args, fmt);
	vsnprintf(err_string, sizeof(err_string), fmt, args);
	va_end(args);

	lineinfo_t lineinfo = fs_reconstruct_lineinfo(loc);
	fs_file_t *file = &fs_files_queue[lineinfo.file];

	if (isatty(fileno(stdout))) {
		eprintf("\033[1;31m%s:\033[0m %s:%u:%u: %s\n", type, fs_make_relative(file->fp), lineinfo.line_nr + 1, lineinfo.col + 1, err_string);	
	} else {
		eprintf("%s: %s:%u:%u: %s\n", type, fs_make_relative(file->fp), lineinfo.line_nr + 1, lineinfo.col + 1, err_string);	
	}
}

void print_diag_without_pos(const char *type, const char *fmt, ...) {
	char err_string[256];
	
	va_list args;
	va_start(args, fmt);
	vsnprintf(err_string, sizeof(err_string), fmt, args);
	va_end(args);

	if (isatty(fileno(stdout))) {
		eprintf("\033[1;31m%s:\033[0m %s\n", type, err_string);
	} else {
		eprintf("%s: %s\n", type, err_string);
	}
}

rsym_t __main_init;

int main(int argc, const char *argv[]) {
	bool err = false;

	// const char *arch = "stdc99_64";
	const char *arch = "stdc_64";
	const char *platform = "libc";

	if (argc == 2) {
		if (setjmp(err_diag.unwind)) {
			err = true;
			goto ret;
		}

		// god this needs to be standardised in an interface
		fs_entrypoint(argv[1]);
		fs_target(arch, platform, false);

		sym_t main_init = {
			.key = sv_move("main.init"),
			.mod = main_module,
			.short_name = sv_move("init"),
			.loc = (loc_t){},
			.is_pub = true,
			.is_extern = false,
			.extern_symbol = ISTR_NONE,
			.kind = SYMBOL_PROC,
			.d_proc = {
				.type = type_new((tinfo_t){
					.kind = TYPE_FUNCTION,
					.d_fn = {
						.args = NULL,
						.ret = TYPE_UNIT,
					}
				}),
				.ret_type = TYPE_UNIT,
				.desc = {
					.hir = hir_dup((hir_expr_t){
						.kind = EXPR_DO_BLOCK,
						.d_do_block = {
							.exprs = NULL,
							.no_branch = true,
						},
					}),
					.locals = NULL,
				},
			}
		};

		__main_init = hmlen(symbols);
		hmputs(symbols, main_init);

		// queue can grow
		for (fs_rfile_t i = 0; i < fs_files_queue_len; i++) {
			u32 old_sz = fs_files_queue_len;
			eprintf("parsing file '%s'\n", fs_make_relative(fs_files_queue[i].fp));
			compiler_process_file(i);
			if (old_sz != fs_files_queue_len) {
				eprintf("  %u new files added\n", fs_files_queue_len - old_sz);
			}
		}
	} else {
		eprintf("usage: %s <file|dir>\n", argv[0]);
	}
ret:;
	// TODO: register_root() etc for module system

	fs_dump_tree();

	extern void hir_passes(void);
	extern void hir_cgen(FILE *file);

	if (!err && !setjmp(err_diag.unwind)) {
		hir_passes();
	} else {
		err = true;
	}

	eprintf("\n");
	table_dump_po();

	if (!err) {
		hir_cgen(stdout);
	}

	return err;
}
