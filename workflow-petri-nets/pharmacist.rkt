#lang s-exp "lang.rkt"
#:draw "dot"
#:places
start
looking-at-options
finding-substitutes
error
made
awaiting-response
awaiting-customer
done
#:transitions
recieve
exact-match
no-exact-match
found-substitute
notify-doctor
notify-patient
pickup

err1
err2
err3
#:arcs
p:start -> t:recieve
t:recieve -> p:looking-at-options

p:looking-at-options -> t:exact-match
t:exact-match -> p:made

p:looking-at-options -> t:no-exact-match
t:no-exact-match -> p:finding-substitutes
p:finding-substitutes -> t:found-substitute
t:found-substitute -> p:made

p:made -> t:notify-doctor
t:notify-doctor -> p:awaiting-response
p:awaiting-response -> t:notify-patient
t:notify-patient -> p:awaiting-customer
p:awaiting-customer -> t:pickup
t:pickup -> p:done

p:finding-substitutes -> t:err1
p:awaiting-response -> t:err2
p:awaiting-customer -> t:err3

t:err1 -> p:error
t:err2 -> p:error
t:err3 -> p:error

#:initials
1 * start
0 * looking-at-options
0 * finding-substitutes
0 * error
0 * made
0 * awaiting-response
0 * awaiting-customer
0 * done
