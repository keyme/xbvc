from XBVC.emitters.c_emitter import CEmitter
from XBVC.emitters.py_emitter import PyEmitter
from XBVC.emitters.nim_emitter import NimEmitter

emitter_registry = {
    'c':CEmitter,
    'python':PyEmitter,
    'nim': NimEmitter
}
