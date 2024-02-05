#define STB_DS_IMPLEMENTATION
#include "all.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include "ansi.h"

const char* __asan_default_options(void) { return "detect_leaks=0"; }

err_diag_t err_diag;

// we aren't using the /proc filesystem, yet...
/* #ifndef __linux__ 
	#error "not portable to places other than linux"
#endif */

void print_diag_with_pos(const char *type, loc_t loc, const char *fmt, ...) {
	char err_string[256];
	
	va_list args;
	va_start(args, fmt);
	vsnprintf(err_string, sizeof(err_string), fmt, args);
	va_end(args);

	file_t *file = FILE_PTR(loc.file);

	if (isatty(fileno(stdout))) {
		eprintf("\033[1;31m%s:\033[0m %s:%u:%u: %s\n", type, file->fp, loc.line_nr + 1, loc.col + 1, err_string);	
	} else {
		eprintf("%s: %s:%u:%u: %s\n", type, file->fp, loc.line_nr + 1, loc.col + 1, err_string);	
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

int main(int argc, const char *argv[]) {
	char inp[1024];

	// TODO: register `lib/`

	bool err = 0;

	/* if (argc == 1) {
		// TODO: repl shoudln't tear down the parsing context every SINGLE time
		//       just edit pfile() instead
		// TODO: how would this work with error recovery?
		//       discard the stream, but keep the module etc (typedecls need to stay too)
		rfile_t repl = fs_set_entry_repl();
		file_t *replp = FILE_PTR(repl);

		rfile_t i = fs_files_queue_len - 1;
		
		while (true) {
			printf("> ");
			if (!fgets(inp, sizeof(inp), stdin)) {
				break;
			}

			if (setjmp(err_diag.unwind)) {
				continue;
			}

			replp->data = (u8*)inp;
			replp->len = strlen(inp);

			compiler_process_file(repl);

			// queue can grow
			for (; i < fs_files_queue_len; i++) {
				compiler_process_file(i);
			}
		}
		eprintf("exiting repl\n");
	} else  */if (argc == 2) {
		if (setjmp(err_diag.unwind)) {
			err = true;
			goto ret;
		}

		fs_set_entry_argp(argv[1]);
		
		// queue can grow
		for (rfile_t i = 0; i < fs_files_queue_len; i++) {
			u32 old_sz = fs_files_queue_len;
			printf("parsing file '%s'\n", fs_files_queue[i].fp);
			compiler_process_file(i);
			if (old_sz != fs_files_queue_len) {
				eprintf("  %u new files added\n", fs_files_queue_len - old_sz);
			}
		}
	} else {
		eprintf("usage: %s <file|dir>\n", argv[0]);
		eprintf("usage: %s\n", argv[0]);
	}
ret:;
	// TODO: register_root() etc for module system

	fs_dump_tree();

	extern void hir_reorder(void);
	extern void hir_typing(void);
	extern void hir_normalisation(void);

	typedef struct {
		void (*pass)(void);
		const char *name;
	} pass_t;

	#define X(x) { x, #x }

	pass_t hir_passes[] = {
		X(hir_reorder),
		X(hir_typing),
		X(hir_normalisation),
	};

	#undef X

	if (!err) for (u32 i = 0; i < ARRAYLEN(hir_passes); i++) {
		printf(BMAG "HIR pass: %s\n" RESET, hir_passes[i].name);

		if (!setjmp(err_diag.unwind)) {
			hir_passes[i].pass();
		} else {
			printf(BRED "HIR pass: %s failed\n" RESET, hir_passes[i].name);
			err = true;
			break;
		}

		table_dump_all();
	}

	return err;
}
