
n = 6;
m = 4;

permutation = {
  {16, 7, 29, 12},
  { 1, 15, 5 , 18},
  { 2, 8, 32, 27},
  { 19, 13, 22, 11},
      {20, 21, 28, 17},
  { 23, 26, 31, 10},
  { 24, 14, 3, 9},
  { 30, 6, 4, 25}};

expansion =
 {
  {32, 1, 2, 3, 4, 5},
  { 4, 5, 6, 7, 8, 9},
  { 8, 9, 10, 11, 12, 13},
  {12, 13, 14, 15, 16, 17},
  { 16, 17, 18, 19, 20, 21},
  {20 , 21, 22, 23, 24, 25},
  { 24, 25, 26, 27, 28, 29},
  {28, 29, 30, 31, 32, 1}
  };


sbox[1] = {
   {14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7},
   {0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8},
   {4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0},
   {15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13}
   };


sbox[2] = {
   {15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10},
   {3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5},
   {0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15},
   {13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9}
   };

sbox[3] = {{
    10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8}, {
    13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1}, {
    13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7}, {
    1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12}};
sbox[4] = {{
    7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15}, {
    13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9}, {
    10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4}, {
    3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14}};
sbox[5] = {{
    2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9}, {
    14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6}, {
    4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14}, {
    11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3}};
sbox[6] = {{
    12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11}, {
    10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8}, {
    9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6}, {
    4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13}};
sbox[7] = {{
    4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1}, {
    13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6}, {
    1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2}, {
    6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12}};
sbox[8] = {{
    13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7}, {
    1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2}, {
    7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8}, {
    2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11}};


S[sbox_, i_] := Module[{row, col}, (
  row =   Mod[i, 2] + Mod[Floor[i/2^(n - 1)], 2] 2;
  col =  Mod[Floor[i/2], 2^(n - 2)];
  sbox[[row + 1]][[col + 1]]
 )
];

DifferentialCharacteristics[sbox_, n_, m_] :=
Module[{LS,output},
 (
  LS[i_] := S[sbox, i];
  SparseArray[Flatten@Table[
	 output = Table[
	   BitXor[LS[x], LS[BitXor[x, delta]]], {x, 0, 2^n - 1}];
	 Map[{delta + 1, #[[1]] + 1} -> (#[[2]]/2^n) &,
	  Tally[output]], {delta, 0, 2^n - 1}]]
  )];

DC = Table[DifferentialCharacteristics[sbox[i], 6, 4], {i, 1, 8}];

W1 = Select[Table[IntegerDigits[deltay, 2, m], {deltay, 0, 2^m - 1}],
   Plus @@ # == 1 &];
W2 = Select[Table[IntegerDigits[deltay, 2, m], {deltay, 0, 2^m - 1}],
   Plus @@ # == 2 &];



MergeTrails[trails_] := Module[{fs, bits, boxes,selected},
(
 fs[box_] := (
   selected = Select[bits, #[[1]] == box &];
   {box, Plus @@ Map[#[[2]] &, selected]}
   );
 
 bits = Flatten[Map[#[[1, 2]] &, trails], 1];
 boxes = Union@Map[First, bits];
 { Union[Map[#[[1, 1]] &, trails]] -> Sort[Map[fs[#] &, boxes]],
  Times @@ Map[Last, trails]}
 )];
 
 
(*
PropagateDeltaX[deltaspecification_, branch_]:=Module[{trails},
 trails=DeltayTrails[deltaspecification];
 
 PropagateDeltaX[trails[[Mod[branch,Length[trails]]+1]]]
 ];
*)

PropagateDeltaX[deltaspecification_]:=Module[{trails},
trails=DeltayTrails[deltaspecification];

Map[PropagateDeltaY,trails]
];


DeltayTrails[deltaspecification_] :=
  Module[{deltax,index,T0W1,T0W2,T0,trail,t0,deltay,box,outindex,prexpanded,expanded},
   
   DEBUG["Delta Input : ", deltaspecification];
   
   deltax = deltaspecification[[2]];
   index = deltaspecification[[1]];
   
   T0W1 = Reverse@
	 SortBy[Select[
	   Map[{{index, deltax} -> {index, i = FromDigits[#, 2]},
		  Normal@DC[[index, deltax + 1, i]]} &, W1], #[[2]] > 0 &],
	  #[[2]] &];
   T0W2 = Reverse@
	 SortBy[Select[
	   Map[{{index, deltax} -> {index, i = FromDigits[#, 2]},
		  Normal@DC[[index, deltax + 1, i]]} &, W2], #[[2]] > 0 &],
	  #[[2]] &];
   
   T0 = Join[T0W1, T0W2];
   DEBUG["Differential Characteristics with Hamming weight 1 :",T0W1];
   DEBUG["Differential Characteristics with Hamming weight 2 :",T0W2];
  
  T0
 
]

PropagateDeltaY[trail_] :=
   Module[{},
   DEBUG["Chosen trail ", trail];
   
   t0 = trail[[1, 2]];
   deltay = t0[[2]];
   box = t0[[1]];
   outindex =
	Select[IntegerDigits[deltay, 2, 4]*permutation[[box]], # > 0 &];
   
   DEBUG["Permuted trail : ", {trail[[1, 1]] -> outindex,
	 trail[[2]]}];
   
   prexpanded = {trail[[1, 1]] ->
	  Flatten@Map[ Flatten@Position[Flatten@expansion, #] &,
		outindex],
	 trail[[2]]};
   
   DEBUG["Pre-expanded trail : ", prexpanded];
   
   expanded = {prexpanded[[1, 1]] ->
	  Map[{Ceiling[#/6], If[(idx = Mod[#, 6]) == 0, 1, 2^(6 - idx)]} &,
		prexpanded[[1, 2]]],
	 prexpanded[[2]]};
   
   DEBUG["Expanded trail : ", expanded];
   
   expanded
   
   ];

 PropagateMultiDeltaX[deltaspecificationlist_] :=
  Module[{trails,xx},
   (
	trails = Map[PropagateDeltaX,deltaspecificationlist];
	xx = Flatten[Outer[List, Sequence @@ trails, 1], Length[deltaspecificationlist]-1];
	Map[MergeTrails, xx]
   )];
