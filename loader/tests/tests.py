#!/usr/bin/env python

import bindings
from hypothesis import given
import hypothesis.strategies as st
import unittest

num_tests = 0


def test(func):
    if __name__ == '__main__':
        func()
        global num_tests
        num_tests += 1


def sign_extended(n: int) -> bool:
    top = n >> 48
    return (top == 0) or (top == 0xffff)


@test
@given(st.integers(min_value=0, max_value=0xffffffffffff),
       st.integers(min_value=0, max_value=7))
def test_tagging_high_0(n, tag):
    value = bindings.add_tag(n, tag)
    n2 = bindings.del_tag(value)
    assert bindings.get_tag(value) == tag
    assert n2 == n
    assert sign_extended(n2)


@test
@given(st.integers(min_value=0xffff000000000000, max_value=0xffffffffffffffff),
       st.integers(min_value=0, max_value=7))
def test_tagging_high_1(n, tag):
    value = bindings.add_tag(n, tag)
    n2 = bindings.del_tag(value)
    assert bindings.get_tag(value) == tag
    assert n2 == n
    assert sign_extended(n2)


strings = st.characters(blacklist_categories=('Cs',), min_codepoint=1)


@test
@given(strings)
def test_str_roundtrip(s: str):
    bs = bytes(s, 'utf-8')
    s = bindings.str_from_cstr(bs)
    assert bindings.cstr_from_str(s) == bs


@test
@given(st.data())
def test_str_substr(data):
    bs = bytes(data.draw(strings), 'utf-8')
    s = bindings.str_from_cstr(bs)
    end = data.draw(st.integers(min_value=0, max_value=len(bs)))
    start = data.draw(st.integers(min_value=0, max_value=end))
    ss = bindings.cstr_from_str(bindings.str_substr(s, start, end))
    assert bs[start:end] == ss


@test
def all_tests_passed():
    print('All tests passed! ({} total)'.format(num_tests))
