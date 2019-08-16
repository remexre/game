from ctypes import *
from hypothesis import given
import hypothesis.strategies as st
import unittest

from lib import lib


def add_tag(n: int, tag: int) -> int:
    return lib.add_tag(c_ulonglong(n), c_ubyte(tag))


def del_tag(value: int) -> int:
    return int(lib.del_tag(c_ulonglong(value)))


def get_tag(value: int) -> int:
    return int(lib.get_tag(c_ulonglong(value)))


def sign_extended(n: int) -> bool:
    top = n >> 48
    return (top == 0) or (top == 0xffff)


@given(st.integers(min_value=0, max_value=0xffffffffffff),
       st.integers(min_value=0, max_value=7))
def test_tagging_high_0(n, tag):
    value = add_tag(n, tag)
    assert get_tag(value) == tag
    assert del_tag(value) == n
    assert sign_extended(del_tag(value))


@given(st.integers(min_value=0xffff000000000000, max_value=0xffffffffffffffff),
       st.integers(min_value=0, max_value=7))
def test_tagging_high_1(n, tag):
    value = add_tag(n, tag)
    assert get_tag(value) == tag
    assert del_tag(value) == n
    assert sign_extended(del_tag(value))
