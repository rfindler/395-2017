#lang s-exp "lang.rkt"
#:places
start
middle
good
bad
#:transitions
a
a*
b
else1
else2
#:arcs
p:start -> t:a
t:a -> p:middle
p:middle -> t:a*
t:a* -> p:middle
p:middle -> t:b
t:b -> p:good

p:start -> t:else1
p:middle -> t:else2
t:else1 -> p:bad
t:else2 -> p:bad
#:initials
1 * start
0 * good
0 * bad
0 * middle
