#!/usr/bin/env python
# -*- coding: utf-8 -*-

import gzip, json, struct, sys
import cuter_global as cglb
import cuter_logger as clg
import cuter_common as cc

class BinaryEOF(Exception):
    def __init__(self):
        pass
    def __str__(self):
        return 'EOF encountered'


class JsonReader:
    def __init__(self, filename, end):
        self.fd = gzip.open(filename, 'rb')
        self.cnt = 0
        # TODO Think about loss of precision due to casting.
        self.end = int(end)

    def read(self, sz):
        return self.fd.read(sz)

    # Decode 4 bytes that represent the size of the entry
    def size(self):
        x = [struct.unpack('B', self.read(1))[0] for z in range(4)]
        return (x[0] << 24) | (x[1] << 16) | (x[2] << 8) | x[3]

    # Decode 4 bytes that represent the tag of the entry
    def tag(self):
        x = [struct.unpack('B', self.read(1))[0] for z in range(4)]
        return (x[0] << 24) | (x[1] << 16) | (x[2] << 8) | x[3]

    # Decode 1 byte that represents the kind of the entry
    def kind(self):
        x = self.read(1)
        if (x == ""):
            raise BinaryEOF
        else:
            return struct.unpack('B', x)[0]

    # Decode 1 byte that represents the type of the entry
    def entry_type(self):
        x = self.read(1)
        if (x == ""):
            raise BinaryEOF
        else:
            return struct.unpack('B', x)[0]

    def __iter__(self):
        return self

    def next(self):
        if self.cnt == self.end:
            raise StopIteration
        try:
            k = self.kind()
            tp = self.entry_type()
            tag = self.tag()
            sz = self.size()
            data = self.read(sz)
            rev = False
            if (cc.is_reversible(k, tp)):
                self.cnt += 1
                if self.cnt == self.end:
                    rev = True
            json_data = json.loads(data)
            clg.json_loaded(self.cnt, k, tp, tag, json_data, rev)
            return tp, tag, json_data, rev
        except BinaryEOF:
            raise StopIteration

    def __del__(self):
        self.fd.close()

###############################################################################
# Unit Tests
###############################################################################

def sample_entries():
    return [
        {
            "kind": cc.NOT_CONSTRAINT,
            "opcode": cc.OP_PARAMS,
            "tag": 0,
            "data": {u'a': [{u's': u'0.0.0.44'}, {u's': u'0.0.0.45'}, {u's': u'0.0.0.46'}], u'c': 1}
        },
        {
            "kind": cc.CONSTRAINT_FALSE,
            "opcode": cc.OP_MATCH_EQUAL_FALSE,
            "tag": 50,
            "data": {u'a': [{u't': 3, u'v': [110, 101, 116]}, {u's': u'0.0.0.44'}], u'c': 6}
        },
        {
            "kind": cc.CONSTRAINT_TRUE,
            "opcode": cc.OP_MATCH_EQUAL_TRUE,
            "tag": 105,
            "data": {u'a': [{u't': 3, u'v': [101, 114, 108, 97, 110, 103]}, {u's': u'0.0.0.44'}], u'c': 5}
        },
        {
            "kind": cc.CONSTRAINT_FALSE,
            "opcode": cc.OP_MATCH_EQUAL_FALSE,
            "tag": 3344,
            "data": {u'a': [{u't': 3, u'v': [116, 114, 117, 101]}, {u's': u'0.0.0.39316'}], u'c': 6}
        }
    ]

def write_bytes(fd, bs):
    for b in bs:
        write_byte(fd, b)

def write_byte(fd, b):
    fd.write(struct.pack('B', b))

def integer_to_i32(num):
    return [num >> i & 0xff for i in (24, 16, 8, 0)]

def test_reader():
    fname = "tmp"
    es = sample_entries()
    try:
        # Prepare the sample file.
        fd = gzip.open(fname, "wb")
        for e in es:
            write_byte(fd, e["kind"])
            write_byte(fd, e["opcode"])
            write_bytes(fd, integer_to_i32(e["tag"]))
            data = json.dumps(e["data"])
            write_bytes(fd, integer_to_i32(len(data)))
            fd.write(data)
        fd.close()

        # Test the JsonReader.
        i = 0
        for tp, tag, data, _ in JsonReader(fname, 10000):
            e = es[i]
            assert tp == e["opcode"], "Opcode of entry {} is {} instead of {}".format(i, tp, e["opcode"])
            assert tag == e["tag"], "Tag of entry {} is {} instead of {}".format(i, tag, e["tag"])
            assert data == e["data"], "Data of entry {} is {} instead of {}".format(i, data, e["data"])
            i += 1
    finally:
        os.remove(fname)

if __name__ == '__main__':
    import os
    cglb.init()
    test_reader()
