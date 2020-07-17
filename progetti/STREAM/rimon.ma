QR[x_, y_, z_, n_] :=
Module[{tmp, tmp2},
 (
   tmp = Mod[y + z, 2^32];
		tmp2 =
   BitXor[BitAnd[BitShiftLeft[tmp, n], 2^32 - 1],
	BitShiftRight[tmp, 32 - n]];
		BitXor[x, tmp2]
  )]
  
  
  QuarterRound[a_, b_, c_, d_] :=
  Module[{bp, cp, dp, ap},
   (
	bp = QR[b, a, d, 7];
	cp = QR[c, bp, a, 9];
	dp = QR[d, cp, bp, 13];
	{QR[a, dp, cp, 18], bp, cp, dp}
	)]
	
Setup[key_, nonce_, counter_] :=
	(
	 {c0, c1, c2, c3} =
	  Map[FromDigits[#, 16] &,
	  {"61707865", "3320646e", "79622d32", "6b206574"}];
	 Partition[Join[
	   {c0}, Take[key, 4], {c1}, nonce, counter, {c2},
	   Take[key, -4], {c3}], 4]
	 )
	 
Salsa20[k_, v_, t_] :=
Flatten@Map[IntegerDigits[#, 2^8, 4] &,
  Flatten@Transpose@
	Nest[Transpose@(QuarterRound @@ #) &, Setup[k, v, t], 21]]
	
	
(*Salsa20[k_, v_] := Table[ Salsa20[k, v, i], {i, 0, 2^64 - 1}]*)


Salsa20Encryption[message_, key_, nonce_] := Module[{keystream},
   (
	keystream =
	 Take[Flatten@
	   Table[Salsa20[key, nonce,
		 IntegerDigits[counter - 1, 2^32, 2]], {counter, 1,
		 Ceiling[Length[message]/64]}], Length[message]];
	BitXor[message, keystream]
	)]
