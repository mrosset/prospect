# This script is taken and slightly modified from https://en.bitcoin.it/wiki/Getblocktemplate. which is licensed under CC?

import binascii
import hashlib
import json

def dblsha(data):
        return hashlib.sha256(hashlib.sha256(data).digest()).digest()

template=json.load(open("test-suite/data.json"))['result']

# txnlist = [coinbase] + [binascii.a2b_hex(a['data']) for a in template['transactions']]
# the txnlist is incomplete since getblocktemplate does not return a 'coinbasetxn' field

txnlist = [binascii.a2b_hex(a['data']) for a in template['transactions']]
merklehashes = [dblsha(t) for t in txnlist]
while len(merklehashes) > 1:
        if len(merklehashes) % 2:
                merklehashes.append(merklehashes[-1])
        merklehashes = [dblsha(merklehashes[i] + merklehashes[i + 1]) for i in range(0, len(merklehashes), 2)]
merkleroot = merklehashes[0]

print(binascii.hexlify(merkleroot))
