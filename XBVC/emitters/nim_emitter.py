from XBVC.objects import CommSpec, Message, Enum
from XBVC.emitters.EmitterBase import SourceFile, EmitterBase

EMITTER_NAME = 'nim'

_TYPE_MAP = {
    'f32': 'float32',
    'f64': 'float64',
    'u32': 'uint32',
    's32': 'int32',
    'u16': 'uint16',
    's16': 'int16',
    'u8': 'uint8',
    's8': 'int8',
}


class Emitter(EmitterBase):
    def __init__(self):
        super().__init__('Nim')

    def _annotate_messages(self):
        for msg in self.cs.messages:
            for member in msg.members:
                member.nim_d_type = _TYPE_MAP[member.d_type]

    def generate_source(self, commspec, targets):
        self.cs = commspec
        source_files = []
        self._annotate_messages()
        source_files.append(self._generate_interface())

        return source_files

    def _generate_interface(self):
        src = self.expand_template('nim_interface.jinja2',
                                   {'messages': self.cs.messages,
                                    'enumerations': self.cs.enums})
        return SourceFile('xbvc.nim', src)
