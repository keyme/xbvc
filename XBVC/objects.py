import yaml

_type_list = [
    'u64',
    's64',
    'u32',
    's32',
    'u16',
    's16',
    'u8',
    's8',
    'f32',
    'f64'
]

RANDOM_ID = {'random_id': 'u32'}
RESPONSE_TO = {'response_to': 'u32'}

MAX_USER_ID = 0xff000000


def camel_string(st):
    """Given a string st containing underscores, returns a string in which
    the first letter is lower case, all underscores are removed, and
    the letter following any underscore is capitalized.
    """
    # Make the first letter lowercase
    new_str = ""
    caps = False
    for c in st:
        if c == '_':
            caps = True
            continue
        new_str += c.upper() if caps else c
        caps = False
    return new_str[0].lower() + new_str[1:]


def pascal_string(st):
    """Given a string st containing underscores, returns a string in which
    the first letter is upper case, all underscores are removed, and
    the letter following any underscore is capitalized.
    """
    result = ""
    cstr = camel_string(st)
    result += cstr[0].upper() + cstr[1:]
    return result

def _map_to_tuple(m):
    if len(m) > 1:
        raise ValueError("_map_to_tuple expects a single entry dictionary")

    return list(m.keys())[0], list(m.values())[0]


class FlexibleNames:
    name = ""
    msg_id = 0

    @property
    def pascal_name(self):
        return pascal_string(self.name)

    @property
    def camel_name(self):
        return camel_string(self.name)


class HashableNameID:
    def __hash__(self):
        return hash(self.name) + hash(self.msg_id)

    def __eq__(self, other):
        return isinstance(other, type(self))


class DataMember(FlexibleNames):
    def __init__(self, dm):
        m_tup = list(dm.items())[0]
        self.name = m_tup[0]

        t = m_tup[1].split('[')

        if len(t) > 2:
            raise TypeError("Invalid type: {}".format(m_tup[1]))

        for x in range(len(t)):
            t[x] = t[x].strip(']')

        if not t[0] in _type_list:
            raise TypeError("Invalid Type: {}".format(t[0]))

        self.d_type = t[0]

        if len(t) > 1:
            self.d_len = t[1]
        else:
            self.d_len = 1

    def __str__(self):
        rs = 'Data Member: {}\n'.format(self.name)
        rs += '-Type: {}\n'.format(self.d_type)
        rs += '-Len: {}\n'.format(self.d_len)

        return rs


class Message(FlexibleNames):
    def __init__(self, msg, name):
        self._member_list = [
            DataMember(RANDOM_ID),
            DataMember(RESPONSE_TO)
        ]

        self.name = name
        self.encoders = []
        self.decoders = []
        self.msg_id = None

        self._parse_members(msg)

    def _parse_members(self, msg):
        for m in msg:
            k, v = _map_to_tuple(m)
            if k == '_encoders':
                self.encoders = v
            elif k == '_decoders':
                self.decoders = v
            elif k == '_id':
                self.msg_id = v
            else:
                self._member_list.append(DataMember(m))

        if self.msg_id is None:
            raise Exception("Msg {} requires a unique ID".format(self.name))

    def __str__(self):
        return self.name

    @property
    def members(self):
        return list(self._member_list)

    def __lt__(self, other):
        if self.msg_id < other.msg_id:
            return True
        return False


class Enum(FlexibleNames):
    def __init__(self, enm, name):
        self.enm_list = enm
        self.name = name

    @property
    def enum_pairs(self):
        result = zip(self.enm_list, range(len(self.enm_list)))
        return sorted(result, key=lambda x: x[1])

    @property
    def value_prefix(self):
        pas_name = pascal_string(self.name)
        starts = [x for x in pas_name if x.isupper()]
        return ''.join(starts).lower()

    @property
    def prefixed_camel_vals(self):
        return [camel_string('{}_{}'.format(self.value_prefix, x.lower()))
                for x in self.enm_list]

    @property
    def caps_prefixed_camel_vals(self):
        return ['{}{}'.format(self.value_prefix.upper(), pascal_string(x.lower()))
                for x in self.enm_list]


    def __str__(self):
        st = "enum {}:\n".format(self.name)
        for enm, val in self.enum_pairs:
            st += " -{}:{}\n".format(enm, val)
        return st


class CommSpec(object):
    def __init__(self, filename):
        self.members = []
        self.targets = []
        self._parse_file(filename)

    def _parse_file(self, filename):
        with open(filename, 'rb') as xf:
            data = yaml.load(xf)

        id_map = {}
        enums = data.get('enumerations', {})
        messages = data.get('messages', {})

        for enum_name, enum_content in enums.items():
            m = Enum(enum_content, enum_name)
            self.add_member(m)

        for message_name, message_content in messages.items():
            m = Message(message_content, message_name)
            if m.msg_id > MAX_USER_ID:
                raise Exception("Msg {}'s unique ID must be less than {}"
                                .format(self.name, MAX_USER_ID))
            elif m.msg_id not in id_map.keys():
                id_map[m.msg_id] = m.name
            else:
                raise Exception("Messages {} and {} share the same key!"
                                .format(m.name, id_map[m.msg_id]))
            self.add_member(m)
        self._add_builtin_members()

    def _add_builtin_members(self):
        m = Message([
            {'_encoders': ['*']},
            {'_decoders': ['*']},
            {'_id': MAX_USER_ID + 1}], 'decode_error')
        self.add_member(m)

    def add_member(self, mem):
        if not type(mem) in [Message, Enum]:
            raise TypeError("add_member requires a Message or an Enum")
        self.members.append(mem)

    @property
    def enums(self):
        enum_list = [x for x in self.members if type(x) == Enum]
        return sorted(enum_list, key=lambda x: x.pascal_name)

    @property
    def messages(self):
        ls = [x for x in self.members if type(x) == Message]
        ls.sort()
        return ls

    def get_encode_messages(self, targets):
        """
        Returns all messages that include a target will need to decode
        """
        rl = []
        for m in [x for x in self.messages]:
            if len(set(m.encoders).intersection(targets)) > 0:
                rl.append(m)
            elif '*' in m.encoders:
                rl.append(m)
        return rl

    def get_decode_messages(self, targets):
        """
        Returns all messages that do not include a target will need to encode
        """
        rl = []
        for m in self.messages:
            intersection_len = len(set(m.decoders).intersection(targets))
            if intersection_len > 0 and len(targets) > 1:
                rl.append(m)
            elif '*' in m.decoders:
                rl.append(m)

        return rl
