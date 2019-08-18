#!/usr/bin/env python

import bindings
from bindings import Value
from hypothesis import given
import hypothesis.strategies as st
import unittest

num_tests = 0


def test(func):
    if __name__ == '__main__':
        func()
        global num_tests
        num_tests += 1


fixnums = st.integers(min_value=-0x80000000, max_value=0x7fffffff)
strings = st.characters(blacklist_categories=('Cs',), min_codepoint=1)


@test
@given(strings)
def test_str_roundtrip(s: str):
    bs = bytes(s, 'utf-8')
    s = bindings.string_from_cstr(bs)
    assert bindings.cstr_from_string(s) == bs


@test
@given(st.data())
def test_str_sub(data):
    bs = bytes(data.draw(strings), 'utf-8')
    s = bindings.string_from_cstr(bs)
    end = data.draw(st.integers(min_value=0, max_value=len(bs)))
    start = data.draw(st.integers(min_value=0, max_value=end))
    ss = bindings.cstr_from_string(bindings.string_sub(s, start, end))
    assert bs[start:end] == ss


@test
@given(st.lists(fixnums))
def test_nreverse_list(l):
    value = Value.make_list([Value.make_fixnum(n) for n in l])
    rev = bindings.nreverse_list(value)
    assert list(reversed(l)) == rev.as_python()


@test
def all_tests_passed():
    print('All tests passed! ({} total)'.format(num_tests))
