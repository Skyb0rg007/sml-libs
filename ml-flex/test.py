#!/usr/bin/env python3

# assert('\uD800'.encode('utf-8', errors='surrogatepass')
#         == b'\xED\xA0\x80')

def matches(s):
    return len(s) == 3 and s[0] == 0xED and 0xA0 <= s[1] <= 0xBF and 0x80 <= s[2] <= 0xBF

def nomatches(s):
    return len(s) != 3 or (0xE0 <= s[0] <= 0xEC) or (0xEE <= s[0] <= 0xEF) or (s[0] == 0xED and 0x80 <= s[1] <= 0x9F)

# for i in range(0xD800, 0xE000):
#     bs = chr(i).encode('utf-8', errors='surrogatepass')
#     if not matches(bs):
#         print(hex(i))
#     if nomatches(bs):
#         print(hex(i))
# 
# for i in range(0, 0xD800):
#     bs = chr(i).encode('utf-8', errors='surrogatepass')
#     if matches(bs):
#         print(hex(i))
#     if not nomatches(bs):
#         print(hex(i))
# 
# for i in range(0xE000, 0x110000):
#     bs = chr(i).encode('utf-8', errors='surrogatepass')
#     if matches(bs):
#         print(hex(i))
#     if not nomatches(bs):
#         print(hex(i))

print(chr(0x10ffff).encode('utf-8'))
print(hex(ord(b'\xF4\x8F\xBF\xBF'.decode('utf-8'))))
