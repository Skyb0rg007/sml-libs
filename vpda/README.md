
# Visibly Pushdown Languages and Automata

Specification of deterministic and nondeterministic visibily pushdown automata
and combinators.

Visibly pushdown languages are constructively closed under:

| Property | Definition |
| --- | --- |
| Union               | L₁ ∪ L₂ = { w \| w ∈ L₁ ∨ w ∈ L₂ }    |
| Intersection        | L₁ ∩ L₂ = { w \| w ∈ L₁ ∧ w ∈ L₂ }    |
| Complement          | ¬L = { w \| w ∉ L }                   |
| Concatenation       | L₁⋅L₂ = { wz \| w ∈ L₁ ∧ z ∈ L₂ }     |
| Kleene star         | L\* = {ε} ∪ { wz \| w ∈ L ∧ z ∈ L\* } |
| String homomorphism | h(L) = { h(w) \| w ∈ L }              |
| Reversal            | Lᴿ = { wᴿ \| w ∈ L }                  |

Sources:
- Visibly Pushdown Languages by Rajeev Alur and P. Madhusudan [source][VPL]
- Nested word languages on Wikipedia [source][wiki]

[VPL]: https://www.cis.upenn.edu/~alur/Stoc04.pdf
[wiki]: https://en.wikipedia.org/wiki/Nested_word#Visibly_pushdown_automaton
