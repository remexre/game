from ctypes import *
from typing import *


lib = cdll.LoadLibrary('../out/game.so')


class String(Structure):
    'A length and string pointer.'
    _fields_ = [('len', c_size_t), ('data', c_void_p)]


class Error(Structure):
    _fields_ = [('code', c_int), ('msg', String)]


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
        err = lib.as_cons(self, byref(cons))
        if err.code != 0:
            raise err
        return (cons.hd, cons.tl)

    def as_fixnum(self) -> int:
        n = c_int()
        err = lib.as_fixnum(self, byref(n))
        if err.code != 0:
            raise err
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


def def_foreign(name, n_args):
    foreign = getattr(lib, name)
    foreign.argtypes = [Value for _ in range(n_args)] + [c_void_p]
    foreign.restype = Error

    def func(*args):
        out = Value()
        fargs = list(args)
        fargs.append(byref(out))
        err = foreign.__call__(*fargs)
        if err.code != 0:
            raise err
        return out

    globals()[name] = func


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


# package make_package(string name);
# symbol make_symbol(package package, string name);
# symbol package_get_symbol(package pkg, string name);

lib.fixnum_to_value.argtypes = [c_int]
lib.fixnum_to_value.restype = Value

# value string_to_value(string);
# value symbol_to_value(symbol);

lib.make_cons.argtypes = [Value, Value]
lib.make_cons.restype = Value

lib.as_cons.argtypes = [Value, c_void_p]
lib.as_cons.restype = Error

lib.as_fixnum.argtypes = [Value, c_void_p]
lib.as_fixnum.restype = Error

lib.null.argtypes = [Value]
lib.null.restype = bool

# lisp/lists.h


def_foreign('nreverse_list', 1)

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
