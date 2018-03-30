from XBVC.emitters.c_emitter import CEmitter
from XBVC.emitters.py_emitter import PyEmitter

emitter_registry = {
    'c':CEmitter,
    'python':PyEmitter
}
