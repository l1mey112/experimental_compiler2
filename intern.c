#include "all.h"

#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

#include "stb_ds.h"

typedef struct { char *key; } intern_key_t;
static intern_key_t *interns = NULL;

static char *sv_to_cstring(u8 *str, size_t len) {
	char *result = malloc(len + 1);
	memcpy(result, str, len);
	result[len] = '\0';
	return result;
}

// TODO: this API is inconsistent
//       for `sv_intern`, check if the string is already interned
//       don't clone every time.

static istr_t _sv_intern(char *str) {
	if (interns == NULL) {
		sh_new_arena(interns);
	}

	ptrdiff_t result = shgeti(interns, str);

	if (result == -1) {
		shputs(interns, (intern_key_t) { .key = str });
		result = shlen(interns) - 1;
	}

	assert(result >= 0 && (size_t)result <= UINT32_MAX);

	return (u32)result;
}

istr_t sv_move(const char *p) {
	return _sv_intern((char *)p);
}

istr_t sv_intern(u8 *sv, size_t len) {
	char *str = sv_to_cstring(sv, len);
	return _sv_intern(str);
}

const char *sv_from(istr_t str) {
	assert(str != ISTR_NONE);
	str &= ISTR_T_MASK;
	assert(str >= 0 && str < shlen(interns));
	return interns[str].key;
}

#define sv_sprintf(u8ptr_name, len_name, fmt, ...) \
	size_t len_name; \
	u8 *u8ptr_name = alloc_scratch(0); \
	len_name = sprintf((char *)u8ptr_name, fmt, __VA_ARGS__); \
	(void)alloc_scratch(len_name)
