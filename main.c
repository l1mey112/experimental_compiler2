#include "all.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

const char* __asan_default_options(void) { return "detect_leaks=0"; }

err_diag_t err_diag;

#ifndef __linux__ 
	#error "not portable to places other than linux"
#endif

void NORETURN err_with_pos(loc_t loc, const char *fmt, ...) {
	char buf[256];
	
	va_list args;
	va_start(args, fmt);
	vsnprintf(buf, sizeof(buf), fmt, args);
	va_end(args);

	file_t *file = &fs_files_queue[loc.file];

	snprintf(err_diag.err_string, sizeof(err_diag.err_string), "%s:%u:%u: %s", file->fp, loc.line_nr + 1, loc.col + 1, buf);
	longjmp(err_diag.unwind, 1);
}

void NORETURN err_without_pos(const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vsnprintf(err_diag.err_string, sizeof(err_diag.err_string), fmt, args);
	va_end(args);
	longjmp(err_diag.unwind, 1);
}

int main(int argc, const char *argv[]) {
	char inp[1024];

	// used for quick concrete type lookups
	{
		extern istr_t typeinfo_concrete_istr[_TYPE_CONCRETE_MAX];
		extern u32 typeinfo_concrete_istr_size;
		u32 i = 0;
		#define X(_, lit) typeinfo_concrete_istr[i++] = sv_intern((u8*)lit, strlen(lit));
		TYPE_X_CONCRETE_LITERALS_LIST
		#undef X
		typeinfo_concrete_istr_size = i;
	}

	// TODO: register `lib/`

	if (argc == 1) {
		// repl
		rfile_t repl = fs_set_entry_repl();
		file_t *replp = &fs_files_queue[repl];

		rfile_t i = fs_files_queue_len - 1;
		
		while (true) {
			printf("> ");
			if (!fgets(inp, sizeof(inp), stdin)) {
				break;
			}

			if (setjmp(err_diag.unwind)) {
				// TODO: report error at proper area and print offending line
				//       err_string is good enough for now
				eprintf("error: %s\n", err_diag.err_string);
				continue;
			}

			replp->data = (u8*)inp;
			replp->len = strlen(inp);

			pentry(repl);

			// queue can grow
			for (; i < fs_files_queue_len; i++) {
				pentry(i);
			}
		}
	} else if (argc == 2) {
		if (setjmp(err_diag.unwind)) {
			// TODO: report error at proper area and print offending line
			//       err_string is good enough for now
			eprintf("error: %s\n", err_diag.err_string);
			return 1;
		}

		fs_set_entry_argp(argv[1]);
		
		// queue can grow
		for (rfile_t i = 0; i < fs_files_queue_len; i++) {
			u32 old_sz = fs_files_queue_len;
			eprintf("parsing file '%s'\n", fs_files_queue[i].fp);
			pentry(i);
			if (old_sz != fs_files_queue_len) {
				eprintf("  %u new files added\n", fs_files_queue_len - old_sz);
			}
		}
	} else {
		eprintf("usage: %s <file|dir>\n", argv[0]);
		eprintf("usage: %s\n", argv[0]);
	}
	
	// TODO: register_root() etc for module system
	
	eprintf("exiting\n");
}
