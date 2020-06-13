(* as a second application BY GIVING THE POLYNOMIAL REPRESENTING THE \
ENCRYPTION FUNCTION *)

polynomial =
  x[1] + v[2] x[1] + v[6] x[1] + v[2] v[4] v[6] x[1] + v[7] x[1] +
   v[5] v[7] x[1] + v[1] x[2] + v[2] x[2] + v[3] v[5] x[2] +
   v[5] v[7] x[2] + v[8] x[2] + v[7] x[1] x[2] + v[5] v[7] x[1] x[2] +
    v[1] x[3] + v[2] x[3] + v[4] x[3] + v[2] v[6] x[3] +
   v[2] v[4] v[6] x[3] + v[7] x[3] + v[5] v[7] x[3] + v[7] x[1] x[3] +
    x[2] x[3] + v[3] v[5] x[2] x[3] + x[4] + v[1] x[4] + v[2] x[4] +
   v[2] v[6] x[4] + v[5] v[7] x[4] + v[8] x[4] + v[6] v[8] x[4] +
   v[5] v[7] x[1] x[4] + x[2] x[4] + v[3] v[5] x[2] x[4] +
   v[7] x[2] x[4] + v[3] v[5] x[3] x[4] + v[7] x[3] x[4] + v[2] x[5] +
    v[3] x[5] + v[2] v[4] v[6] x[5] + v[7] x[5] + x[2] x[5] +
   v[3] x[2] x[5] + v[3] v[5] x[2] x[5] + v[7] x[2] x[5] +
   v[5] v[7] x[2] x[5] + v[3] v[5] x[3] x[5] + v[5] v[7] x[3] x[5] +
   v[3] x[4] x[5] + v[7] x[4] x[5] + x[6] + v[1] x[6] + v[2] x[6] +
   v[4] x[6] + v[3] v[5] x[6] + v[2] v[6] x[6] + v[2] v[4] v[6] x[6] +
    v[6] v[8] x[6] + v[7] x[1] x[6] + x[2] x[6] + v[7] x[2] x[6] +
   v[3] v[5] x[3] x[6] + v[7] x[3] x[6] + v[3] v[5] x[4] x[6] +
   v[7] x[4] x[6] + v[5] v[7] x[4] x[6] + v[3] v[5] x[5] x[6] +
   v[7] x[5] x[6] + v[2] x[7] + v[3] x[7] + v[2] v[6] x[7] +
   v[2] v[4] v[6] x[7] + v[8] x[7] + v[5] v[7] x[1] x[7] +
   v[3] x[2] x[7] + v[3] v[5] x[2] x[7] + x[3] x[7] +
   v[3] v[5] x[3] x[7] + v[7] x[3] x[7] + v[5] v[7] x[3] x[7] +
   x[4] x[7] + v[3] x[4] x[7] + v[7] x[4] x[7] + x[5] x[7] +
   v[7] x[5] x[7] + x[6] x[7] + v[3] v[5] x[6] x[7] + v[2] x[8] +
   v[6] x[8] + v[2] v[4] v[6] x[8] + v[7] x[8] + v[6] v[8] x[8] +
   v[7] x[1] x[8] + v[5] v[7] x[1] x[8] + v[3] v[5] x[2] x[8] +
   x[3] x[8] + v[3] v[5] x[3] x[8] + x[4] x[8] + v[7] x[4] x[8] +
   x[5] x[8] + v[5] v[7] x[5] x[8] + x[6] x[8] + v[3] v[5] x[6] x[8] +
    v[7] x[6] x[8] + v[5] v[7] x[6] x[8] + v[7] x[7] x[8] +
   v[5] v[7] x[7] x[8];
   
   key = Sort@Union@Flatten@(p /. {Times -> List, Plus -> List});
   lens = Length[key];
   privatevars = Take[key, -lens/2];
   publicvars = Take[key, lens/2];
   
   EvalPolynomial[f_, key0_] := Mod[f /. MapThread[Rule, {key, key0}], 2];

(* RANDOM KEY ASSIGNEMENT FOR THE ONLINE PHASE *)
key0 =
 Table[RandomInteger[1], lens/2]
traffic =
  Table[{setup = Join[public = Table[RandomInteger[1], lens/2], key0];
     public, EvalPolynomial[p, setup]}, {1000}];

Print@TableForm[traffic[[{1, 2, 3}]], TableDepth -> 2];

PickIndex[stato_] := (
  If[Length[stato[[2]]] > 0,
   (
    chosen = 1 + RandomInteger[Length[stato[[2]]] - 1];
    {Append[stato[[1]], stato[[2, chosen]]],
     Drop[stato[[2]], {chosen}]}
    ),
   stato]
  );
LeaveIndex[stato_] := Reverse[PickIndex[Reverse[stato]]];

 Superpolynomial[f_, monomio_] :=
  Simplify[(f - PolynomialMod[f, monomio] )/monomio];

 LinearityTest[spoly_] :=
  Exponent[spoly /. Map[# -> z &, privatevars], z] == 1;

 ExtractMaxrankMatrix[cubes_] :=
  Module[{vrank, current},
   (
	vrank = {1};
	current = 2;
	While[(Length[vrank] < 8) && (current < Length[cubes]),
	 While[(current < Length[cubes]) &&
	   MatrixRank[tmp = Map[Last, cubes[[Append[vrank, current]]]],
		 Modulus -> 2] < (Length[vrank] + 1), current++];
	 If[current < Length[cubes], vrank = Append[vrank, current],
	  vrank]];
	vrank
	)
   ];

 FreeVariablesAssign[indici_, maxterm_, spoly_] :=
  Module[{M, freevars, lf, subs, M2},
   M = Normal@CoefficientArrays[spoly, privatevars][[2]];
   freevars =
	Select[Union@Flatten[M /. {Plus -> List, Times -> List}],
	 Head[#] == v &];
   lf = Length[freevars];
   subs = Table[
	 Thread[freevars -> IntegerDigits[i, 2, lf]], {i, 0, 2^lf - 1}];
   M2 = Map[{indici, maxterm, #, Mod[M /. #, 2]} &, subs]
   ];
   
   stato = {{0, 1}, {2, 3, 4, 5, 6, 7}};
   cubes = {};
   matrix = {};
   
   (* RANDOM WALK WITH THE SYMBOLIC EXPRESSION *)
   maxloops = 50;
   loops = 0;

   While[(cubes == {} || MatrixRank[matrix, Modulus -> 2] <= lens/2) &&
	 loops < maxloops,
	(
	 
	 loops++;
	 Print[stato[[1]]];
	 indici = lens/2 - stato[[1]];
	 
	 Print[indici];
	 
	 spoly = Superpolynomial[p, mono = Times @@ publicvars[[indici]]];
	 Print["monomio : ", mono, "\n superpolinomio : ", spoly];
	 If[spoly === 0, stato = LeaveIndex[stato],
	  If[! LinearityTest[spoly], stato = PickIndex[stato],
	   newcubes =
		FreeVariablesAssign[stato[[1]], Times @@ publicvars[[indici]],
		 spoly];
	   matrix = Join[matrix, Map[Last, newcubes]];
	   cubes = Join[cubes, newcubes];
	   stato = LeaveIndex[PickIndex[stato]]
	   ]]
	 )];
	 
	 Print@TableForm[cubes, TableDepth -> 2];
	 Print@MatrixForm@M;
	 Print@MatrixRank[M, Modulus -> 2];
	 
	 (* EXTRACTION of a SUBSYSTEM OF MAXIMAL RANK *)
	 Print@TableForm[
	  cubes[[ExtractMaxrankMatrix[cubes]]], TableDepth -> 2];


(* SIMULATION of ONLINE PHASE ... to be completed *)

Print@Select[traffic, (#[[1, 8]] == 0 && #[[1, 6]] == 0) &][[1]]
Print@Select[traffic, (#[[1, 8]] == 1 && #[[1, 6]] == 0) &][[1]]

testkey = key;
testkey[[8]] = 0;
testkey[[6]] = 0;
a = EvalPolynomial[p, testkey][[1]];
testkey = key;
testkey[[8]] = 1;
testkey[[6]] = 0;
b = EvalPolynomial[p, testkey][[1]];

Print@PolynomialMod[a + b, 2]
