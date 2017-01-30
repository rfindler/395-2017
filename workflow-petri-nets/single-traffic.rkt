#lang s-exp "lang.rkt"

#:places red green orange
#:transitions to-green to-orange to-red
#:arcs
p:red -> t:to-green
t:to-green -> p:green
p:green -> t:to-orange
t:to-orange -> p:orange
p:orange -> t:to-red
t:to-red -> p:red
#:initials
1 * red
0 * green
0 * orange
