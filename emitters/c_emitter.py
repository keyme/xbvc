from objects import CommSpec, Message, Enum
from jinja2 import FileSystemLoader
from EmitterBase import SourceFile, Emitter

type_map = {
    'u32': 'uint32_t',
    's32': 'int32_t',
    'u16': 'uint16_t',
    's16': 'int16_t',
    'u8': 'uint8_t',
    's8': 'int8_t',
}

rl_size_map = {
    # '0' is for padding bytes
    '0':  0b000,
    '8':  0b001,
    '16': 0b010,
    '32': 0b011,
    '64': 0b100,
}

rl_alignment_map = {
    '8':  1,
    '16': 2,
    '32': 4,
    '64': 8,
}

class AlignmentMember(object):
    def __init__(self, length, pos):
        self.d_len = length
        self.d_type = 'u8'
        self.name = '_align{}'.format(pos)

class CStructure:
    def __init__(self, message, msg_id):
        self.name = message.name
        self.value_pairs = []
        self._msg = message
        self.msg_id = msg_id

        self._generate_encode_array()
        self._get_values()

    def _get_values(self):
        for mem in self._msg.members:
        #Get the type and length of the member
            if not mem.d_type in type_map:
                raise TypeError("Invalid Type: {}".format(t_st))

            mem_type = type_map[mem.d_type]

            # Properly lay out the structure, if it is an array, make it so
            if mem.d_len == 1:
                self.value_pairs.append({'type':mem_type, 'field_name':mem.name})
            else:
                self.value_pairs.append({'type':mem_type,
                                         'field_name': '{}[{}]'.format(mem.name, mem.d_len)})

    def _generate_encode_array(self):
        msg_ary = []
        alignment = 0
        # We build a new member list dynamically to account for
        # alignment bytes
        new_mem = []
        align_count = 0

        for mem in self._msg.members:
            m_len = int(mem.d_len)
            rleb = rl_size_map[mem.d_type[1:]] << 5
            if m_len <=31:
                #run length encoder byte
                msg_ary.append(rleb | int(m_len))

                #adjust current alignment
                alignment += rl_alignment_map[mem.d_type[1:]] * m_len
            else:
                #loop through every 31 instances of the member
                #(2^5 bits) and add them to the list as a separate
                #run
                while m_len > 0:
                    if m_len >= 31:
                        msg_ary.append(rleb | 31)
                    else:
                        msg_ary.append(rleb | m_len)
                    alignment += rl_alignment_map[mem.d_type[1:]] * m_len
                    m_len -= 31

            new_mem.append(mem)

            #If we aren't on an even alignment, add some padding bytes
            if alignment % 4 and mem != self._msg.members[-1]:
                align_adjust = 4 - (alignment % 4)
                msg_ary.append(align_adjust)
                new_mem.append(AlignmentMember(align_adjust, align_count))
                alignment += align_adjust
                align_count += 1

        self._msg._member_list = new_mem

        self.enc_ary = msg_ary

    @property
    def encode_array(self):
        #This is a little nasty, but basically it takes an array of
        #integers: [1, 2, 3, 4, 5] and returns a hex string in c:
        # "\x01\x02\x03\x04\x05"
        rs = '"{}"'.format("".join(["\\x{0:02x}".format(x) for x in self.enc_ary]))
        return rs


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

        for idx in range(len(self.cs.messages)):
            self.all_msgs.append(CStructure(self.cs.messages[idx], idx))

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
