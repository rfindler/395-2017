#lang s-exp "lang.rkt"
#:places
red1
green1
orange1
red2
green2
orange2
s
#:transitions
to-green1
to-orange1
to-red1
to-green2
to-orange2
to-red2
#:arcs
p:red1 -> t:to-green1
t:to-green1 -> p:green1
p:green1 -> t:to-orange1
t:to-orange1 -> p:orange1
p:orange1 -> t:to-red1
t:to-red1 -> p:red1

p:red2 -> t:to-green2
t:to-green2 -> p:green2
p:green2 -> t:to-orange2
t:to-orange2 -> p:orange2
p:orange2 -> t:to-red2
t:to-red2 -> p:red2

t:to-red1 -> p:s
t:to-red2 -> p:s
p:s -> t:to-green1
p:s -> t:to-green2
#:initials
1 * red1
0 * green1
0 * orange1
1 * red2
0 * green2
0 * orange2
1 * s
