from XBVC.objects import CommSpec, Message, Enum
from EmitterBase import SourceFile, Emitter


class PyEmitter(Emitter):
    def __init__(self):
        super(PyEmitter, self).__init__('Python')

    def generate_source(self, commspec, targets):
        self.cs = commspec
        source_files = []
        source_files.append(self._generate_interface())
        source_files.append(self._retrieve_cobs())

        return source_files

    def _retrieve_cobs(self):
        return SourceFile('cobs.py', self.expand_template('cobs.py'))

    def _generate_interface(self):
        src = self.expand_template('py_interface.jinja2',
                                   {'messages': self.cs.messages})
        return SourceFile('xbvc_py.py', src)
