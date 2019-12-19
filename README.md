# Project-of-representation-sharing-for-Prolog
Some study of ideas and implementations of representation sharing for findall/3.

There are three different files in this project, 'findall-hProlog', 'findall-SWI' and 'findall-XSB'.

'findall-XSB' implement the original findall/3 with XSB, and it can be run with XSB.

'findall-SWI' implement the original findall/3 and the copy-once findall/3, which can be run with SWI-Prolog smoothly. The experiments in our final report are performed with SWI-Prolog and use the code in this file.

'findall-hProlog' contains the code of findall/3 with input sharing. Moreover, another version of copy-once findall/3 is included in this file.
