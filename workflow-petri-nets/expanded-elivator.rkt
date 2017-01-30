#lang s-exp "lang.rkt"
#:places
f1-precall
f1-preenter
f1

f2-precall
f2-preenter
f2
#:transitions
f1-call
f1-enter

f2-call
f2-enter

up
down
#:arcs
p:f1 -> t:up
t:up -> p:f2

p:f2 -> t:down
t:down -> p:f1

p:f1-precall -> t:f1-call
t:f1-call -> p:f1-preenter
p:f1-preenter -> t:f1-enter
t:f1-enter -> p:f1

p:f2-precall -> t:f2-call
t:f2-call -> p:f2-preenter
p:f2-preenter -> t:f2-enter
t:f2-enter -> p:f2

p:f1 -> t:f1-enter
p:f2 -> t:f2-enter

#:initials
1 * f1-precall
0 * f1-preenter
1 * f1

1 * f2-precall
0 * f2-preenter
0 * f2
