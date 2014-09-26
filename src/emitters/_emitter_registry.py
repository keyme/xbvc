from c_emitter import CEmitter
from py_emitter import PyEmitter

emitter_registry = {
    'c':CEmitter,
    'python':PyEmitter
}
