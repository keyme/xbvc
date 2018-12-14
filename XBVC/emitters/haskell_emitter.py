from XBVC.objects import CommSpec, Message, Enum
from XBVC.emitters.EmitterBase import SourceFile, EmitterBase

EMITTER_NAME = 'haskell'

_TYPE_MAP = {
    'f32': 'Float',
    'f64': 'Double',
    'u32': 'Word32',
    's32': 'Int32',
    'u16': 'Word16',
    's16': 'Int16',
    'u8': 'Word8',
    's8': 'Int8',
}

_ENC_MAP = {
    'f32': 'Bitvec.encodeFloatS',
    'f64': 'encodeFloat',
    'u32': 'Bitvec.encodeS',
    's32': 'Bitvec.encodeS',
    'u16': 'Bitvec.encodeS',
    's16': 'Bitvec.encodeS',
    'u8': 'Bitvec.encodeS',
    's8': 'Bitvec.encodeS',
}

_DEC_MAP = {
    'f32': 'Bitvec.decodeFloatST',
    'f64': 'decodeFloat',
    'u32': 'Bitvec.decodeST',
    's32': 'Bitvec.decodeST',
    'u16': 'Bitvec.decodeST',
    's16': 'Bitvec.decodeST',
    'u8': 'Bitvec.decodeST',
    's8': 'Bitvec.decodeST',
}

_LIST_DEC_MAP = {
    'f32': 'Bitvec.decodeFloatListST',
    'f64': 'decodeFloatList',
    'u32': 'Bitvec.decodeListST',
    's32': 'Bitvec.decodeListST',
    'u16': 'Bitvec.decodeListST',
    's16': 'Bitvec.decodeListST',
    'u8': 'Bitvec.decodeListST',
    's8': 'Bitvec.decodeListST',
}


def _gen_decode_string(member):
    if member.d_len == 1:
        return '{}'.format(_DEC_MAP[member.d_type])

    return (
        '{} {}'
        .format(
            _LIST_DEC_MAP[member.d_type],
            member.d_len
        )
    )


def _gen_encode_string(member):
    if member.d_len == 1:
        return '{} {}'.format(_ENC_MAP[member.d_type], member.camel_name)

    return ('mapM_ {} $ take {} {}'
            .format(_ENC_MAP[member.d_type],
                    member.d_len,
                    member.camel_name))


def _total_line_len(line):
    return sum([len(x) for x in line]) + len(line) - 1


def _gen_msg_name_list(messages):
    names = [x.camel_name for x in messages]
    lines = []
    current_line = []
    MAX_LINE_LENGTH = 70
    for name in names:
        if _total_line_len(current_line) + len(name) > MAX_LINE_LENGTH:
            lines.append(', '.join(current_line) + ',')
            current_line = []
        current_line.append(name)
    lines.append(', '.join(current_line))

    # Trim trailing comma if there is one
    if lines[-1] == ',':
        lines = lines[:-1]

    return lines


_EXCLUDED_MEMBERS = ['randomId', 'responseTo']
class Emitter(EmitterBase):
    def __init__(self):
        super().__init__('Haskell')

    def _annotate_messages(self):
        for msg in self.cs.messages:
            msg.space_sep_member_list = " ".join(
                [m.camel_name
                 for m in msg.members
                 if m.camel_name not in _EXCLUDED_MEMBERS]
            )
            for member in msg.members:
                member.hask_d_type = _TYPE_MAP[member.d_type]
                if member.d_len == 1:
                    member.hask_type_sig = member.hask_d_type
                else:
                    member.hask_type_sig = '[{}]'.format(member.hask_d_type)

                member.encode_str = _gen_encode_string(member)
                member.decode_str = _gen_decode_string(member)

    def generate_source(self, commspec, targets):
        self.cs = commspec
        self._annotate_messages()

        return [
            self._generate_data(),
            self._generate_core(),
            SourceFile('Cobs.hs', self.expand_template('Cobs.hs')),
            SourceFile('Bitvec.hs', self.expand_template('Bitvec.hs')),
        ]

    def _generate_data(self):
        src = self.expand_template('haskell_data.jinja2',
                                   {'messages': self.cs.messages,
                                    'enumerations': self.cs.enums})
        return SourceFile('Data.hs', src)

    def _generate_core(self):
        src = self.expand_template(
            'haskell_core.jinja2',
            {
                'messages': self.cs.messages,
                'enumerations': self.cs.enums,
                'msg_name_list': _gen_msg_name_list(self.cs.messages)
            }
        )
        return SourceFile('XBVC.hs', src)
