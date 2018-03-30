#!/usr/bin/python

# Copyright 2013, Jeff Ciesielski <jeffciesielski@gmail.com>
# Redistribution and use in source and binary forms are permitted,
# with or without modification.
# This is based on the C implementation of COBS by Jacques Fortier

# Encodes a list or tuple of bytes (data) using COBS
# Returns the encoded data
def cobs_encode(data = list()):
    if not type(data) == list and not type(data) == tuple:
        raise TypeError("data must be a list or tuple")

    read_index = 0
    write_index = 1
    code_index = 0
    code = 1
    output = (len(data) + 1 + (len(data) / 255)) * [0x00]

    while read_index < len(data):
        if data[read_index] == 0:
            output[code_index] = code
            code = 1
            code_index = write_index
            write_index += 1
            read_index += 1

        else:
            output[write_index] = data[read_index]
            read_index += 1
            write_index += 1
            code += 1

            if code == 0xff:
                output[code_index] = code
                code = 1
                code_index = write_index
                write_index += 1

    output[code_index] = code

    return output

# Decodes a list or tuple of bytes (data) from COBS
# Returns a tuple containing the length of the decoded data
# and a list containing the data, or (0,[]) if the data
# is invalid
def cobs_decode(data = list()):
    if not type(data) == list and not type(data) == tuple:
        raise TypeError("data must be a list or tuple")

    read_index = 0
    write_index = 0
    code = 0
    output = len(data) * [0x00]

    while read_index < len(data):
        code = data[read_index]
        if (read_index + code) > len(data) and not code == 1:
            return (0, list())

        read_index += 1

        for i in range(code - 1):
            output[write_index] = data[read_index]
            write_index += 1
            read_index += 1

        if not code == 0xff and not read_index == len(data):
            output[write_index] = 0x00
            write_index += 1

    return output[:write_index]

if __name__ == '__main__':
    dat = (0x11, 0x00, 0x22, 0x42, 0x00, 0xff)
    print("Original Length: {0} | {1}".format(len(dat), [hex(x) for x in dat]))

    res = cobs_encode(dat)
    print("Encoded length:  {0} | {1}".format(len(res), [hex(x) for x in res]))

    dat = list(res)

    res = cobs_decode(dat)
    print("Decoded Length:  {0} | {1}".format(len(res), [hex(x) for x in res]))
