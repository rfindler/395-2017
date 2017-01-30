#lang s-exp "lang.rkt"
#:places
start
good
bad
#:transitions
a
b
else
#:arcs
p:start -> t:a
t:a -> p:start
p:start -> t:b
t:b -> p:good
p:start -> t:else
t:else -> p:bad
#:initials
1 * start
0 * good
0 * bad
