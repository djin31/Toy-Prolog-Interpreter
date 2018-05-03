%% Harry Potter Lexicon Database

wizard(harry).
wizard(ron).
wizard(arthur).
wizard(bill).
wizard(james).
wizard(severus).
wizard(albus).
wizard(sirius).
wizard(remus).
wizard(neville).
wizard(voldemort).
wizard(draco).
wizard(rubeus).
wizard(gellert).

witch(hermione).
witch(ginny).
witch(molly).
witch(fleur).
witch(lily).
witch(luna).
witch(bellatrix).
witch(minerva).
witch(cho).
witch(hanna).
witch(nymphadora).

elf(dobby).
elf(kreacher).

owl(hedwig).

married(harry, ginny).
married(ron, hermione).
married(arthur, molly).
married(james, lily).
married(remus, nymphadora).
married(bill, fleur).
married(neville, luna).

loves(severus, lily).
loves(james, lily).
loves(harry, ginny).
loves(ron, hermione).
loves(arthur, molly).
loves(remus, nymphadora).
loves(bill, fleur).
loves(neville, luna).
loves(harry, cho).
loves(voldemort, bellatrix).
loves(draco, hanna).
loves(rubeus, harry).
loves(rubeus, ron).
loves(rubeus, hermione).
loves(minerva, albus).

parent(arthur, ron).
parent(arthur, ginny).
parent(arthur, bill).
parent(molly, ron).
parent(molly, ginny).
parent(molly, bill).
parent(james, harry).
parent(lily, harry).

father(F,C) :- parent(F,C), wizard(F).

mother(M,C) :- parent(M,C), witch(M).

son(S,P) :- wizard(S), parent(P,S).

daughter(D,P) :- witch(D), parent(P,D).