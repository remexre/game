#ifndef GAME_UTIL_H
#define GAME_UTIL_H 1

#include <stddef.h>
#include <stdbool.h>

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define AT __FILE__ ":" TOSTRING(__LINE__)

#define compile_assert(COND, NAME) extern char NAME[(COND) ? 1 : -1]

#define expect(COND, MSG) \
	do { fail_at_error(AT, error_add_msg(error_expect(COND, TOSTRING(COND)), \
				                         string_from_static_cstr(MSG))); } while(0)
#define expect_ok(EXPR, MSG) \
	do { fail_at_error(AT, error_add_msg(EXPR, string_from_static_cstr(MSG))); } while(0)
#define expect_errno_ok(EXPR, MSG) \
	do { fail_at_error(AT, error_add_msg(error_errno(EXPR), \
				                         string_from_static_cstr(MSG))); } while(0)

typedef struct {
	size_t len;
	const char* data;
} string;

extern const string string_empty;

string string_from_cstr(const char*);
string string_from_static_cstr(const char*);
string string_cat(string, string);
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
	SYSCALL_FAILED
} error_code;

typedef struct {
	error_code code;
	string msg;
} error;

extern const error error_none;
#define ERROR(CODE, MSG) (error) { .code = CODE, .msg = MSG }

string error_msg(error_code code);
void fail_at_error(const char* at, error err);
void fail_at_error_nocode(const char* at, error err);
error error_add_msg(error err, string msg);
error error_errno(int err);
error error_expect(bool cond, const char* expr);

#endif
