#!/usr/bin/env python3

import xbvc_py as xp


fluff_ary = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

gr = xp.GetResponse(Error=123,
                    Target=456,
                    Index=-32,
                    Foo=128,
                    Result=-45,
                    Bar=5,
                    Version=0.07,
                    FloatList=[1.1, 2.2, 3.3, 4.4, 5.5])

lst = gr.encode()
new = xp.GetResponse(msg_pkt=lst)

assert gr.Error == new.Error
assert gr.Target == new.Target
assert gr.Index == new.Index
assert gr.Foo == new.Foo
assert gr.Result == new.Result
assert gr.Bar == new.Bar
print("Tests passed!")
