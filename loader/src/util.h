#ifndef GAME_UTIL_H
#define GAME_UTIL_H 1

#include <stddef.h>
#include <stdbool.h>

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define AT __FILE__ ":" TOSTRING(__LINE__)

#define compile_assert(COND, NAME) extern char NAME[(COND) ? 1 : -1]

#define expect(COND, MSG) do { \
	fail_at_error(AT, error_add_msg(error_expect(COND, TOSTRING(COND)), \
				string_from_static_cstr(MSG))); \
} while(0)

#define expect_ok(EXPR, MSG) do { \
	fail_at_error(AT, error_add_msg(EXPR, string_from_static_cstr(MSG))); \
} while(0)

#define expect_errno_ok(EXPR, MSG) do { \
	fail_at_error(AT, error_add_msg(error_errno(EXPR), \
				string_from_static_cstr(MSG))); \
} while(0)

#define try(EXPR) do { \
		error try__error = (EXPR); \
		if(try__error.code != OK) \
			return try__error; \
	} while(0)

int min_int(int x, int y);
size_t min_size_t(size_t x, size_t y);
#define min(X, Y) _Generic((X), \
	int: min_int(X, Y), \
	size_t: min_size_t(X, Y))

typedef struct {
	size_t len;
	const char* data;
} string;

extern const string string_empty;

string string_from_cstr(const char*);
string string_from_static_cstr(const char*);
string string_cat(string, string);
int string_cmp(string, string);
string string_drop(string, size_t);
string string_take(string, size_t);
string string_sub(string, size_t, size_t);
char string_get(string, size_t);
size_t string_len(string);
char* cstr_from_string(string);

typedef unsigned long long hash;

hash djb2a(string);

typedef enum {
	OK = 0,
	EXPECTATION_FAILED,
	SYSCALL_FAILED,
	SYNTAX_ERROR,
	TYPE_ERROR
} error_code;

typedef struct {
	error_code code;
	string msg;
} error;

extern const error ok;
#define ERROR(CODE, MSG) (error) { .code = CODE, .msg = MSG }

string error_msg(error_code code);
void fail_at_error(const char* at, error err);
void fail_at_error_nocode(const char* at, error err);

__attribute__((warn_unused_result))
error error_add_msg(error err, string msg);

__attribute__((warn_unused_result))
error error_errno(int err);

__attribute__((warn_unused_result))
error error_expect(bool cond, const char* expr);

#endif
