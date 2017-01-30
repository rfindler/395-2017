#lang s-exp "lang.rkt"
#:draw "dot"
#:places
start
middle/await
middle/final
good
bad
#:transitions
a
a*
b1
b2
else1
else2
else3
#:arcs
p:start -> t:a
p:start -> t:b
p:start -> t:else1

t:a -> p:middle/await

p:middle/await -> t:a*
p:middle/await -> t:else2
p:middle/await -> t:b1

t:a* -> p:middle/await

t:b1 -> p:middle/final

p:middle/final -> t:b2
p:middle/final -> t:else3

t:b2 -> p:good
t:b -> p:good

t:else1 -> p:bad
t:else2 -> p:bad
t:else3 -> p:bad

#:initials
1 * start
0 * good
0 * bad
0 * middle/await
0 * middle/final
