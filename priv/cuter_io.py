#!/usr/bin/env python
# -*- coding: utf-8 -*-

import gzip, json, struct, sys
import cuter_global as cglb
import cuter_logger as clg
import cuter_common as cc
from cuter_proto_log_entry_pb2 import LogEntry

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
        bs = [self.read(1) for _ in range(4)]
        if any(map(lambda b: b == "", bs)):
            raise BinaryEOF
        x = [struct.unpack('B', b)[0] for b in bs]
        return (x[0] << 24) | (x[1] << 16) | (x[2] << 8) | x[3]

    def __iter__(self):
        return self

    def next(self):
        if self.cnt == self.end:
            raise StopIteration
        try:
            sz = self.size()
            data = self.read(sz)
            entry = LogEntry()
            entry.ParseFromString(data)
            rev = False
            if cc.is_reversible(entry):
                self.cnt += 1
                if self.cnt == self.end:
                    rev = True
            clg.json_loaded(self.cnt, entry, rev)
            return entry, rev
        except BinaryEOF:
            raise StopIteration

    def __del__(self):
        self.fd.close()

###############################################################################
# Unit Tests
###############################################################################

def sample_entries():
    es = [dict() for _ in range(4)]
    # 1st log entry.
    es[0]["type"] = LogEntry.OP_PARAMS
    es[0]["args"] = [
          cc.mk_symb("0.0.0.44")
        , cc.mk_symb("0.0.0.45")
        , cc.mk_symb("0.0.0.46")
    ]
    es[0]["is_constraint"] = False
    es[0]["tag"] = 0
    es[0]["message"] = cc.mk_log_entry(es[0]["type"], es[0]["args"])
    # 2nd log entry.
    es[1]["type"] = LogEntry.OP_MATCH_EQUAL_FALSE
    es[1]["args"] = [
          cc.mk_atom([110, 101, 116])
        , cc.mk_symb("0.0.0.46")
    ]
    es[1]["tag"] = 50
    es[1]["is_constraint"] = True
    es[1]["message"] = cc.mk_log_entry(es[1]["type"], es[1]["args"], es[1]["tag"], es[1]["is_constraint"])
    # 3rd log entry.
    es[2]["type"] = LogEntry.OP_MATCH_EQUAL_TRUE
    es[2]["tag"] = 105
    es[2]["is_constraint"] = True
    es[2]["args"] = [
          cc.mk_atom([101, 114, 108, 97, 110, 103])
        , cc.mk_symb("0.0.0.44")
    ]
    es[2]["message"] = cc.mk_log_entry(es[2]["type"], es[2]["args"], es[2]["tag"], es[2]["is_constraint"])
    # 4th log entry.
    es[3]["type"] = LogEntry.OP_MATCH_EQUAL_FALSE
    es[3]["tag"] = 105
    es[3]["is_constraint"] = True
    es[3]["args"] = [
          cc.mk_atom([116, 114, 117, 101])
        , cc.mk_symb("0.0.0.39316")
    ]
    es[3]["message"] = cc.mk_log_entry(es[3]["type"], es[3]["args"], es[3]["tag"], es[3]["is_constraint"])
    return es

def write_bytes(fd, bs):
    for b in bs:
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
            msg = e["message"].SerializeToString()
            write_bytes(fd, integer_to_i32(len(msg)))
            fd.write(msg)
        fd.close()

        # Test the JsonReader.
        i = 0
        for entry, _ in JsonReader(fname, 10000):
            e = es[i]
            assert entry  == e["message"], "Entry {} is {} instead of {}".format(i, str(entry), str(e["message"]))
            i += 1
    finally:
        os.remove(fname)

if __name__ == '__main__':
    import os
    cglb.init()
    test_reader()
