from XBVC.objects import CommSpec, Message, Enum
from jinja2 import FileSystemLoader
from XBVC.emitters.EmitterBase import SourceFile, Emitter

type_map = {
    'u32': 'uint32_t',
    's32': 'int32_t',
    'u16': 'uint16_t',
    's16': 'int16_t',
    'u8': 'uint8_t',
    's8': 'int8_t',
    'f32': 'float',
    'f64': 'double'
}

rl_alignment_map = {
    '8':  1,
    '16': 2,
    '32': 4,
    '64': 8,
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
    'f64': 'E_64'
}

class CStructure:
    def __init__(self, message, msg_id):
        self.name = message.name
        self.value_pairs = []
        self._msg = message
        self.msg_id = msg_id

        self._get_values()

    def _get_values(self):
        for mem in self._msg.members:
        #Get the type and length of the member
            if not mem.d_type in type_map:
                raise TypeError("Invalid Type: {}".format(mem.d_type))

            mem_type = type_map[mem.d_type]

            # Properly lay out the structure, if it is an array, make it so
            if mem.d_len == 1:
                self.value_pairs.append({'type':mem_type, 'field_name':mem.name})
            else:
                self.value_pairs.append({'type':mem_type,
                                         'field_name': '{}[{}]'.format(mem.name, mem.d_len)})

    @property
    def encoder_body(self):
        result = ""
        for mem in self._msg.members:
            container_type = type_container_map[mem.d_type]
            if int(mem.d_len) > 1:
                indent = 8
                result += (
                    '    for (int i = 0; i < {}; i++) {{\n'
                    .format(mem.d_len)
                )
                result += (
                    "{}index += xbvc_encode_bit_vector(&src->{}[i], &dest[index], {});\n"
                    .format(' ' * indent, mem.name, container_type)
                )
                result += (
                    "{}if (index >= max_len) {{\n{}return -1;\n{}}}\n"
                    .format(' ' * indent, ' ' * indent * 2, ' ' * indent)
                )
                result += '    }\n'
            else:
                indent = 4

                result += (
                    "{}index += xbvc_encode_bit_vector(&src->{}, &dest[index], {});\n"
                    .format(' ' * indent, mem.name, container_type)
                )
                result += (
                    "{}if (index >= max_len) {{\n{}return -1;\n{}}}\n"
                    .format(' ' * indent, ' ' * indent * 2, ' ' * indent)
                )

        return result

    @property
    def decoder_body(self):
        result = ""
        for mem in self._msg.members:
            container_type = type_container_map[mem.d_type]
            if int(mem.d_len) > 1:
                indent = 8
                result += (
                    '    for (int i = 0; i < {}; i++) {{\n'
                    .format(mem.d_len)
                )
                result += (
                    "{}index += xbvc_decode_bit_vector(&src[index], &dest->{}[i], {});\n"
                    .format(' ' * indent, mem.name, container_type)
                )
                result += (
                    "{}if (index >= max_len) {{\n{}return -1;\n{}}}\n"
                    .format(' ' * indent, ' ' * indent * 2, ' ' * indent)
                )
                result += '    }\n'
            else:
                indent = 4

                result += (
                    "{}index += xbvc_decode_bit_vector(&src[index], &dest->{}, {});\n"
                    .format(' ' * indent, mem.name, container_type)
                )
                result += (
                    "{}if (index >= max_len) {{\n{}return -1;\n{}}}\n"
                    .format(' ' * indent, ' ' * indent * 2, ' ' * indent)
                )

        return result


class CEmitter(Emitter):
    def __init__(self):
        super(CEmitter, self).__init__('C')

    def generate_source(self, commspec, targets):
        self.targets = targets
        self.cs = commspec
        source_files = []

        self.dec_msgs = self.cs.get_decode_messages(self.targets)
        self.enc_msgs = self.cs.get_encode_messages(self.targets)

        self.all_msgs = []

        for idx, msg in enumerate(self.cs.messages):
            self.all_msgs.append(CStructure(self.cs.messages[idx], msg.msg_id))

        self._verify_targets()

        source_files.append(self._generate_header_file())
        source_files.append(self._generate_encoder_decoder())

        source_files.extend(self._retrieve_cobs_files())
        return source_files

    def _verify_targets(self):
        cs_tgts = []

        for m in self.dec_msgs + self.enc_msgs:
            cs_tgts.extend(m.targets)

        diff = set(self.targets).difference(cs_tgts)
        if len(diff):
            raise Exception("Unknown targets: {}".format(list(diff)))


    def _retrieve_cobs_files(self):
        header = SourceFile('cobs.h', self.expand_template('cobs.h'))
        src = SourceFile('cobs.c', self.expand_template('cobs.c'))
        return [header, src]


    def _generate_handler_prototype(self, st_name):
        rs = "void xbvc_handle_{0}(struct x_{0} *msg)"\
            .format(st_name)

        return rs

    def _generate_encoder_decoder(self):
        content = self.expand_template('c_interface.jinja2',
                                       {'msgs': self.all_msgs,
                                        'dec_msgs': self.dec_msgs})
        return SourceFile('xbvc_core.c', content)

    def _generate_header_file(self):
        #message ids
        msgs = []
        msg_types = [x.name.upper() for x in self.cs.messages]

        protos = [self._generate_handler_prototype(x.name) for x in self.dec_msgs]
        header_vars = {'enumerations': self.cs.enums,
                       'prototypes': protos,
                       'msgs': self.all_msgs,
        }

        header_content = self.expand_template('c_header.jinja2', header_vars)

        return SourceFile('xbvc_core.h', header_content)

    def _generate_redirect_stub(self, st_name):
        rs = "static void _xbvc_redirect_{}(void *s)\n{{\n".format(st_name)
        rs += "\txbvc_handle_{0}((struct x_{0} *)s);\n}}\n".format(st_name)
        return rs
