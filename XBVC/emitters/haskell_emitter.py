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
    'f32': 'Bitvec.encodeFloat',
    'f64': 'encodeFloat',
    'u32': 'Bitvec.encode',
    's32': 'Bitvec.encode',
    'u16': 'Bitvec.encode',
    's16': 'Bitvec.encode',
    'u8': 'Bitvec.encode',
    's8': 'Bitvec.encode',
}

_DEC_MAP = {
    'f32': 'Bitvec.decodeFloat',
    'f64': 'decodeFloat',
    'u32': 'Bitvec.decode',
    's32': 'Bitvec.decode',
    'u16': 'Bitvec.decode',
    's16': 'Bitvec.decode',
    'u8': 'Bitvec.decode',
    's8': 'Bitvec.decode',
}

_LIST_DEC_MAP = {
    'f32': 'Bitvec.decodeFloatList',
    'f64': 'decodeFloatList',
    'u32': 'Bitvec.decodeList',
    's32': 'Bitvec.decodeList',
    'u16': 'Bitvec.decodeList',
    's16': 'Bitvec.decodeList',
    'u8': 'Bitvec.decodeList',
    's8': 'Bitvec.decodeList',
}


def _gen_decode_string(member):
    if member.d_len == 1:
        return '{} bs'.format(_DEC_MAP[member.d_type])

    return (
        '{} bs {}'
        .format(
            _LIST_DEC_MAP[member.d_type],
            member.d_len
        )
    )


def _gen_encode_string(member):
    if member.d_len == 1:
        return '{} {}'.format(_ENC_MAP[member.d_type], member.camel_name)

    return ('BS.concat $ {} <$> (take {} {})'
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
