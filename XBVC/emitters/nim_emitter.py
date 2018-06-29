from XBVC.objects import CommSpec, Message, Enum
from XBVC.emitters.EmitterBase import SourceFile, Emitter


class NimEmitter(Emitter):
    def __init__(self):
        super().__init__('Nim')

    def generate_source(self, commspec, targets):
        self.cs = commspec
        source_files = []
        source_files.append(self._generate_interface())

        return source_files

    def _generate_interface(self):
        src = self.expand_template('nim_interface.jinja2',
                                   {'messages': self.cs.messages,
                                    'enumerations': self.cs.enums})
        return SourceFile('xbvc.nim', src)
