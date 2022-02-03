#!/usr/bin/env python

import re

f = open('words-all.txt')
f1 = open('words.txt', 'a')

for line in f.readlines():
	lenstr = str(len(line)-1)
	allLetters=re.search(r"[A-z]{"+ lenstr +"}", line)
	acronyms=re.search(r"[A-Z]{" + lenstr + "}", line)
	if allLetters != None and acronyms == None:
		f1.write(line)

f1.close()
f.close()
