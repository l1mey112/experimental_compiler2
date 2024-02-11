#define __USE_GNU
#define _GNU_SOURCE

#include "all.h"

target_t target;

const char *target_string(target_t target) {
	const char *arch_string;
	const char *platform_string;
	
	switch (target.arch.kind) {
		#define X(arch, name) case arch: arch_string = name; break;
			X_ARCHS
		#undef X
	}

	switch (target.platform.kind) {
		#define X(platform, name) case platform: platform_string = name; break;
			X_PLATFORMS
		#undef X
	}

	char *out;

	asprintf(&out,"%s-%s", arch_string, platform_string);

	return out;
}

target_t target_host(void) {
	target_t target = {
		.arch = {
			.kind = ARCH_AMD64,
			.ptr_size = 8,
		},
		.platform = {
			.kind = PLATFORM_C,
		},
	};
	return target;
}
