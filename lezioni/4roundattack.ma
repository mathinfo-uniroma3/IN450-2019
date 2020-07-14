
plaintext = Table[IntegerDigits[i, 256, 2], {i, 0, 2^16 - 1}];

Setup[p_] := Module[{taux},
(
 
 taux = Table[0, {4}, {4}];
 taux[[1, 1]] = p[[1]];
  taux[[2, 1]] = p[[2]];
 taux
 )];

SumStates[A_, B_] := MapThread[BitXor, {A, B}]

(* DUMMY FUNCTIONS  --- To BE DONE*)

AESEncryption[key_, block_] := SumStates[key, block]

CypherTest[c1_, c2_] := c1[[1, 1]] < c2[[1, 1]]
TestMI[c1_, c2_] := c1[[1, 1]] < c2[[1, 1]]

(**********************************)
Seed[10];
a = RandomInteger[255];
P = a + Map[Setup, plaintext];


PC = ParallelMap[{#, AESEncryption[key, #]} &, P];


SC = Sort[PC, CypherTest[#1[[2]], #2[[2]]] &];

j = Count[
Map[TestMI[SumStates[#[[1]], #[[2 ]]]] &, Partition[SC, 2, 1]],
True]



