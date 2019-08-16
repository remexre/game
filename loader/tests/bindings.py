from ctypes import *


class Str(Structure):
    'A length and string pointer.'
    _fields_ = [('len', c_size_t), ('data', c_void_p)]


lib = cdll.LoadLibrary('../out/game.so')

# lisp.h

lib.add_tag.argtypes = [c_ulonglong, c_byte]
lib.add_tag.restype = c_ulonglong


def add_tag(n: int, tag: int) -> int:
    return lib.add_tag(n, tag)


lib.del_tag.argtypes = [c_ulonglong]
lib.del_tag.restype = c_ulonglong


def del_tag(value: int) -> int:
    return lib.del_tag(value)


lib.get_tag.argtypes = [c_ulonglong]
lib.get_tag.restype = c_byte


def get_tag(value: int) -> int:
    return lib.get_tag(value)

# util.h


lib.cstr_from_str.argtypes = [Str]
lib.cstr_from_str.restype = c_char_p


def cstr_from_str(s: Str) -> bytes:
    return lib.cstr_from_str(s)


lib.str_from_cstr.argtypes = [c_char_p]
lib.str_from_cstr.restype = Str


def str_from_cstr(s: bytes) -> Str:
    return lib.str_from_cstr(s)


lib.str_substr.argtypes = [Str, c_size_t, c_size_t]
lib.str_substr.restype = Str


def str_substr(s: Str, start: int, end: int) -> Str:
    return lib.str_substr(s, start, end)
