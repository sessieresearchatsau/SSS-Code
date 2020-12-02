(* ::Package:: *)

BeginPackage["SSS`"]

(* is this public? *)

SSS::usage = "Functions related to Sequential Substitution Systems (SSS) & Causal Network Construction."
somethingRandom[string_String] := (ToCharacterCode[string] - 65);
Begin["`Private`"]
 
fromAlpha[string_String] :=(ToCharacterCode[string]-65);  
toAlpha[l:{___Integer}] := FromCharacterCode[l+65];

Attributes[s]=Flat;
SSSConvert[string_String] := s @@ fromAlpha[string];
SSSConvert[s[x__]] := toAlpha[{x}];
SSSConvert::usage="Converts SSS (sequential substitution system) states between s- and string-formats, using the functions \!\(\*StyleBox[\"fromAlpha\",FontSlant->\"Italic\"]\) and \!\(\*StyleBox[\"toAlpha\",FontSlant->\"Italic\"]\).";

 End[]
 
 (* is this public? *)
 
 (*
 SSSEvolve[x___] := SSS`Private`SSSEvolve[x];
 SSS[x___] := SSS`Private`SSS[x];
 SSSDisplay[x___] := SSS`Private`SSSDisplay[x];

 SSSInteractiveDisplay[x___] := SSS`Private`SSSInteractiveDisplay[x];
 SSSAnimate[x___] := SSS`Private`SSSAnimate[x];
 SSSAnimateByDistance[x___] := SSS`Private`SSSAnimateByDistance[x];
 SSSInteractiveHistory[x___] := SSS`Private`SSSInteractiveHistory[x];
 SSSInitialState[x___] := SSS`Private`SSSInitialState[x];
 fromReducedRank[x___] := SSS`Private`fromReducedRank[x];
 fromReducedRankShowSteps[x___] := SSS`Private`fromReducedRankShowSteps[x];
 toReducedRank[x___] := SSS`Private`toReducedRank[x];
 fromReducedRankQuinaryCode[x___] := SSS`Private`fromReducedRankQuinaryCode[x];
 toReducedRankQuinaryCode[x___] := SSS`Private`toReducedRankQuinaryCode[x];
*)
 
 EndPackage[]
 



