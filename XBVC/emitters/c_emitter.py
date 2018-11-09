from XBVC.objects import HashableNameID
from XBVC.emitters.EmitterBase import SourceFile, EmitterBase

type_map = {
    'u32': 'uint32_t',
    's32': 'int32_t',
    'u16': 'uint16_t',
    's16': 'int16_t',
    'u8': 'uint8_t',
    's8': 'int8_t',
    'f32': 'float',
}

type_container_map = {
    'u64': 'E_64',
    's64': 'E_64',
    'u32': 'E_32',
    's32': 'E_32',
    'u16': 'E_16',
    's16': 'E_16',
    'u8': 'E_8',
    's8': 'E_8',
    'f32': 'E_32',
}

EMITTER_NAME = 'c'

FLOAT_ENCODE_STR = (
    "{indent}sf = split_float(src->{name}{index_marker}, MAX_PRECISION);\n"
    "{indent}index += xbvc_encode_bit_vector(&sf.whole, &dest[index], E_32);\n"
    "{idx_check}"
    "{indent}index += xbvc_encode_bit_vector(&sf.frac, &dest[index], E_32);\n"
    "{idx_check}"
)

FLOAT_DECODE_STR = (
    "{indent}sf.whole = 0;\n"
    "{indent}sf.frac = 0;\n"
    "{indent}index += xbvc_decode_bit_vector(&src[index], &sf.whole, E_32);\n"
    "{idx_check}"
    "{indent}index += xbvc_decode_bit_vector(&src[index], &sf.frac, E_32);\n"
    "{indent}dest->{name}{index_marker} = combine_float(sf);\n"
    "{idx_check}"
)


def gen_index_return_check(indent):
    return (
        "{}if (index >= max_len) {{\n{}return -1;\n{}}}\n"
        .format(' ' * indent, ' ' * indent * 2, ' ' * indent)
    )


def gen_bitvec_encode(indent, name, container_type, is_indexed=False):
    return (
        "{}index += xbvc_encode_bit_vector(&src->{}{}, &dest[index], {});\n"
        .format(' ' * indent,
                name,
                '[i]' if is_indexed else '',
                container_type)
    )


def gen_float_bitvec_encode(indent, name, is_indexed=False):
    result = FLOAT_ENCODE_STR.format(
        indent=' ' * indent,
        name=name,
        index_marker='[i]' if is_indexed else '',
        idx_check=gen_index_return_check(indent)
    )
    return result


def gen_bitvec_decode(indent, name, container_type, is_indexed=False):
    return (
        "{}index += xbvc_decode_bit_vector(&src[index], &dest->{}{}, {});\n"
        .format(' ' * indent,
                name,
                '[i]' if is_indexed else '',
                container_type)
    )


def gen_float_bitvec_decode(indent, name, is_indexed=False):
    result = FLOAT_DECODE_STR.format(
            indent=' ' * indent,
            name=name,
            index_marker='[i]' if is_indexed else '',
            idx_check=gen_index_return_check(indent)
    )

    return result


class CStructure(HashableNameID):
    def __init__(self, message, msg_id):
        self.name = message.name
        self.value_pairs = []
        self._msg = message
        self.msg_id = msg_id

        self._get_values()

    def _get_values(self):
        for mem in self._msg.members:
            # Get the type and length of the member
            if mem.d_type not in type_map:
                raise TypeError("Invalid Type: {}".format(mem.d_type))

            mem_type = type_map[mem.d_type]

            # Properly lay out the structure, if it is an array, make it so
            if mem.d_len == 1:
                self.value_pairs.append(
                    {'type': mem_type, 'field_name': mem.name}
                )
            else:
                self.value_pairs.append(
                    {'type': mem_type,
                     'field_name': '{}[{}]'.format(mem.name, mem.d_len)}
                )

    @property
    def encoder_body(self):
        result = ""
        if any([x.d_type == 'f32' for x in self._msg.members]):
            # Note the single braces here are actual valid C code, NOT
            # a field delimiter for python's `format` function
            result += "    struct splitfloat sf = {0};\n    (void)sf;\n"
        for mem in self._msg.members:
            container_type = type_container_map[mem.d_type]
            if int(mem.d_len) > 1:
                indent = 8
                result += (
                    '    for (int i = 0; i < {}; i++) {{\n'
                    .format(mem.d_len)
                )
                if mem.d_type == 'f32':
                    result += gen_float_bitvec_encode(indent, mem.name, True)
                else:
                    result += gen_bitvec_encode(indent, mem.name,
                                                container_type, True)
                result += gen_index_return_check(indent)
                result += '    }\n'
            else:
                indent = 4
                if mem.d_type == 'f32':
                    result += gen_float_bitvec_encode(indent, mem.name, False)
                else:
                    result += gen_bitvec_encode(indent, mem.name,
                                                container_type, False)
                result += gen_index_return_check(indent)

        return result

    @property
    def decoder_body(self):
        result = ""
        if any([x.d_type == 'f32' for x in self._msg.members]):
            result += "    struct splitfloat sf = {0};\n    (void)sf;\n"
        for mem in self._msg.members:
            container_type = type_container_map[mem.d_type]
            if int(mem.d_len) > 1:
                indent = 8
                result += (
                    '    for (int i = 0; i < {}; i++) {{\n'
                    .format(mem.d_len)
                )
                if mem.d_type == 'f32':
                    result += gen_float_bitvec_decode(indent, mem.name, True)
                else:
                    result += gen_bitvec_decode(indent, mem.name,
                                                container_type, True)
                result += gen_index_return_check(indent)
                result += '    }\n'
            else:
                indent = 4
                if mem.d_type == 'f32':
                    result += gen_float_bitvec_decode(indent, mem.name, False)
                else:
                    result += gen_bitvec_decode(indent, mem.name,
                                                container_type, False)
                result += gen_index_return_check(indent)

        return result


class Emitter(EmitterBase):
    def __init__(self):
        super().__init__('C')
        self.enc_msgs = []
        self.dec_msgs = []

    def generate_source(self, commspec, targets):
        self.targets = targets
        self.cs = commspec
        source_files = []

        for msg in self.cs.get_decode_messages(self.targets):
            self.dec_msgs.append(CStructure(msg, msg.msg_id))

        for msg in self.cs.get_encode_messages(self.targets):
            self.enc_msgs.append(CStructure(msg, msg.msg_id))

        source_files.append(self._generate_header_file())
        source_files.append(self._generate_encoder_decoder())

        source_files.extend(self._retrieve_cobs_files())
        return source_files

    @property
    def all_msgs(self):
        return set(self.dec_msgs + self.enc_msgs)

    def _retrieve_cobs_files(self):
        header = SourceFile('cobs.h', self.expand_template('cobs.h'))
        src = SourceFile('cobs.c', self.expand_template('cobs.c'))
        return [header, src]

    def _generate_handler_prototype(self, st_name):
        rs = ("void xbvc_handle_{0}(struct x_{0} *msg)"
              .format(st_name))

        return rs

    def _generate_encoder_decoder(self):
        content = self.expand_template('c_interface.jinja2',
                                       {'msgs': self.all_msgs,
                                        'dec_msgs': self.dec_msgs,
                                        'enc_msgs': self.enc_msgs})
        return SourceFile('xbvc_core.c', content)

    def _generate_header_file(self):
        # message ids
        protos = [self._generate_handler_prototype(x.name)
                  for x in self.dec_msgs]
        header_vars = {
            'enumerations': self.cs.enums,
            'prototypes': protos,
            'msgs': self.all_msgs,
        }

        header_content = self.expand_template('c_header.jinja2', header_vars)

        return SourceFile('xbvc_core.h', header_content)

    def _generate_redirect_stub(self, st_name):
        rs = "static void _xbvc_redirect_{}(void *s)\n{{\n".format(st_name)
        rs += "\txbvc_handle_{0}((struct x_{0} *)s);\n}}\n".format(st_name)
        return rs
