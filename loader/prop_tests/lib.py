from ctypes import *

lib = cdll.LoadLibrary('../out/game.so')
lib.add_tag.restype = c_ulonglong
lib.del_tag.restype = c_ulonglong
lib.get_tag.restype = c_byte
