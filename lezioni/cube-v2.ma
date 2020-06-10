coefficients = {
			  {1, 1, 0, 0, 1},
			  {0, 0, 1, 1}
};

LFSRCombiner[x_,y_,z_]:=x y + x z ;
LFSRCombiner[s_,t_]:=s[[ 5-4]] s[[5-2]] t[[4-0]] + s[[5-4]] t[[4-0]] t[[4-2]] + s[[5-1]] t[[4-3]] ;


LFSRUpdate[coefficients_, state_] := Append[Drop[state, 1] , state.coefficients];

MultipleLFSRSetup[coefficients_, key_] :=
	Module[{lens, p, i},
		lens = Map[Length, coefficients];
		p = Table[Plus @@ Take[lens, i], {i, 1, Length[lens]}];
		MapThread[Take[key, {#1, #2}] &, {Prepend[Drop[p, -1], 0] + 1, p}]
]

MultipleLFSRUpdate[coefficients_, state_] := MapThread[LFSRUpdate, {coefficients, state}]

KeyStream[coefficients_, key_, n_] := Module[{orb},
  	(
   	orb = NestList[MultipleLFSRUpdate[coefficients, #] &, MultipleLFSRSetup[coefficients, key], n];
   	PolynomialMod[Map[LFSRCombiner@@(Map[Last,#])&, orb],2]
   )]
   
KeyStream[coefficients_, key_, n_] := Module[{orb},
  	(
   	orb = NestList[MultipleLFSRUpdate[coefficients, #] &, MultipleLFSRSetup[coefficients, key], n];
   	PolynomialMod[Map[LFSRCombiner@@#&, orb],2]
   )]



 
 
 
