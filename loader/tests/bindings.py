from ctypes import *


class String(Structure):
    'A length and string pointer.'
    _fields_ = [('len', c_size_t), ('data', c_void_p)]


class Value(Structure):
    'A tagged Lisp value.'
    _fields_ = [('n', c_ulonglong)]


lib = cdll.LoadLibrary('../out/game.so')

# lisp.h

lib.add_tag.argtypes = [c_ulonglong, c_byte]
lib.add_tag.restype = Value


def add_tag(n: int, tag: int) -> Value:
    return lib.add_tag(n, tag)


lib.del_tag.argtypes = [Value]
lib.del_tag.restype = c_ulonglong


def del_tag(value: Value) -> int:
    return lib.del_tag(value)


lib.get_tag.argtypes = [Value]
lib.get_tag.restype = c_byte


def get_tag(value: Value) -> int:
    return lib.get_tag(value)

# util.h


lib.cstr_from_string.argtypes = [String]
lib.cstr_from_string.restype = c_char_p


def cstr_from_string(s: String) -> bytes:
    return lib.cstr_from_string(s)


lib.string_from_cstr.argtypes = [c_char_p]
lib.string_from_cstr.restype = String


def string_from_cstr(s: bytes) -> String:
    return lib.string_from_cstr(s)


lib.string_sub.argtypes = [String, c_size_t, c_size_t]
lib.string_sub.restype = String


def string_sub(s: String, start: int, end: int) -> String:
    return lib.string_sub(s, start, end)
