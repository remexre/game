#ifndef GAME_UTIL_H
#define GAME_UTIL_H 1

#include <stdarg.h>
#include <stddef.h>
#include <stdbool.h>

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define AT __FILE__ ":" TOSTRING(__LINE__)

#define compile_assert(COND, NAME) extern char NAME[(COND) ? 1 : -1]
#define upto(I, N) for(size_t I = 0; I < (N); I++)
#define dotimes(N) upto(dotimes__i, N)
#define UNUSED(NAME) ((void)(NAME))

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

#define todo do { \
	return make_error(TODO, string_from_static_cstr(AT)); \
} while(0)

#define unreachable do { \
	fail_at_error(AT, (error) { \
		.code = TODO, \
		.msg = string_from_static_cstr("unreachable") \
	}); \
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

string stringf(const char*, ...) __attribute__((format(printf, 1, 2)));
string vstringf(const char*, va_list);
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
bool string_contains_char(string, char);

typedef unsigned long long hash;

hash djb2a(string);

typedef enum {
	OK = 0,
	ARGN_MISMATCH,
	EXPECTATION_FAILED,
	SYNTAX_ERROR,
	SYSCALL_FAILED,
	TODO,
	TYPE_ERROR,
	UNBOUND_FUNC,
	UNBOUND_VAR
} error_code;

typedef struct {
	error_code code;
	string msg;
} error;

#define error_return __attribute__((warn_unused_result)) error

extern const error ok;

error_return make_error(error_code code, string msg);
error_return errorf(error_code code, const char* format, ...) __attribute__((format(printf, 2, 3)));

void fail_at_error(const char* at, error err);
void fail_at_error_nocode(const char* at, error err);

error_return error_add_msg(error err, string msg);
error_return error_errno(int err);

error_return error_expect(bool cond, const char* expr);

__attribute__((noinline))
void bp(void);

#endif
