This plug-in is to extend the abilities of CCGLAB tool. User provided list decides which verb morphemes to type-raise. Then these type-raised grammar rules are added at the end of already available grammar. The code has already been integrated to CCGLAB and the latest version can be used to utilize this plug-in. For the instructions, go to github.com/bozsahin/ccglab and in the docs folder look for the manual. 

What the compiler plug-in does is to first go over all the argument-takers you have specified through their POS tags, then for
every argument of every such element it generates the type-raised type. For example for the verbal category
(S\NP)/NP, it will first take the ‘/NP’ and raise it to (S\NP)\((S\NP)/NP), because of its directionality.
Then it will take the ‘\NP’ and raise it to S/(S\NP) because of S\NP that is left once we are done with the
‘/NP’. The semantics is always the same: λ p.p a0 for some argument a0.
