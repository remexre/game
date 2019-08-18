from ctypes import *
from typing import *


lib = cdll.LoadLibrary('../out/game.so')


class String(Structure):
    'A length and string pointer.'
    _fields_ = [('len', c_size_t), ('data', c_void_p)]

    def __bytes__(self):
        buf = create_string_buffer(self.len)
        lib.memcpy(buf, self.data, self.len)
        return buf.raw

    def __str__(self):
        return str(self.__bytes__(), 'utf-8')


class Error(Structure):
    _fields_ = [('code', c_int), ('msg', String)]

    def check(self):
        if self.code != 0:
            raise Exception(str(self.msg))


class Value(Structure):
    'A tagged Lisp value.'
    _fields_ = [('n', c_ulonglong)]

    @staticmethod
    def make_cons(hd: 'Value', tl: 'Value') -> 'Value':
        return lib.make_cons(hd, tl)

    @staticmethod
    def make_fixnum(n: int) -> 'Value':
        return lib.fixnum_to_value(n)

    @staticmethod
    def make_list(l: List['Value']) -> 'Value':
        out = Value.nil()
        for hd in reversed(l):
            out = Value.make_cons(hd, out)
        return out

    @staticmethod
    def nil() -> 'Value':
        v = Value()
        v.n = 0
        return v

    def as_cons(self) -> Tuple['Value', 'Value']:
        cons = Cons()
        lib.as_cons(self, byref(cons)).check()
        return (cons.hd, cons.tl)

    def as_fixnum(self) -> int:
        n = c_int()
        lib.as_fixnum(self, byref(n)).check()
        return n.value

    def as_python(self) -> any:
        tag = self.get_tag()
        if tag == 0:
            return [val.as_python() for val in self.as_list()]
        elif tag == 1:
            return self.as_fixnum()
        elif tag == 3:
            return self.as_float()
        elif tag == 5:
            return self.as_symbol()
        elif tag == 6:
            return self.as_string()
        elif tag == 7:
            return self.as_vector()
        else:
            raise Exception('Unknown tag: {}', tag)

    def as_list(self) -> List['Value']:
        if self.is_null():
            return []
        (hd, tl) = self.as_cons()
        return [hd] + tl.as_list()

    def get_tag(self) -> int:
        return get_tag(self)

    def is_null(self) -> bool:
        b = lib.null(self)
        return b


class Cons(Structure):
    'A head and tail value.'
    _fields_ = [('hd', Value), ('tl', Value)]


lib.make_context.argtypes = []
lib.make_context.restype = c_void_p
ctx = lib.make_context()


def def_foreign(name):
    foreign = getattr(lib, name)
    foreign.argtypes = [Value, c_void_p, c_void_p]
    foreign.restype = Error

    def func(*args, ctx=ctx):
        out = Value()
        foreign(Value.make_list(args), byref(out), ctx).check()
        return out

    globals()[name] = func


lib.memcpy.argtypes = [c_void_p, c_void_p, c_size_t]
lib.memcpy.restype = c_void_p


# lisp/lists.h


def_foreign('nreverse_list')

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
