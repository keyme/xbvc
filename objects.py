
_type_list = [
    'u32',
    's32',
    'u16',
    's16',
    'u8',
    's8',
]

class DataMember(object):
    def __init__(self, dm):
        m_tup = dm.items()[0]
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


class Message(object):
    def __init__(self, msg, name):
        self._member_list = []
        self.name = name
        self.targets = []
        self.msg_id = None

        self._parse_members(msg)

    def _parse_members(self, msg):
        for m in msg:
            if m.keys()[0] == '_targets':
                self.targets = m.values()[0].keys()
            elif m.keys()[0] == '_id':
                self.msg_id = m.values()[0]
            else:
                self._member_list.append(DataMember(m))

        if self.msg_id == None:
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

class Enum(object):
    def __init__(self, enm, name):
        self.enm_list = enm
        self.name = name

    @property
    def enum_pairs(self):
        return zip(self.enm_list, range(len(self.enm_list)))

    def __str__(self):
        st = "enum {}:\n".format(self.name)
        for enm, val in self.enum_pairs:
            st+= " -{}:{}\n".format(enm, val)
        return st

class CommSpec(object):
    def __init__(self):
        self.members = []
        self.targets = []

    def add_member(self, mem):
        if not type(mem) in [Message, Enum]:
            raise TypeError("add_member requires a Message or an Enum")
        self.members.append(mem)

    @property
    def enums(self):
        return [x for x in self.members if type(x) == Enum]

    @property
    def messages(self):
        ls =  [x for x in self.members if type(x) == Message]
        ls.sort()
        return ls

    def get_encode_messages(self, targets):
        """
        Returns all messages that include a target will need to decode
        """
        rl = []
        for m in [x for x in self.messages]:
            if len(set(m.targets).intersection(targets)) > 0:
                rl.append(m)
        return rl

    def get_decode_messages(self, targets):
        """
        Returns all messages that do not include a target will need to encode
        """
        rl = []
        for m in [x for x in self.messages]:
            if len(set(m.targets).intersection(targets)) == 0:
                rl.append(m)

            if len(set(m.targets).intersection(targets)) > 0 and len(m.targets) > 1:
                rl.append(m)

        return rl
