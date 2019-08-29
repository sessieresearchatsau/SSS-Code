(* ::Package:: *)

BeginPackage["SSS`"]

SSS::usage =
"Functions related to Sequential Substitution Systems (SSS)
 & Causal Network Construction."
 
 Begin["`Private`"]
 (* *)
 SSSConvert::usage="Converts SSS (sequential substitution system) states between s- and string-formats, using the functions \!\(\*
StyleBox[\"fromAlpha\",\nFontSlant->\"Italic\"]\) and \!\(\*
StyleBox[\"toAlpha\",\nFontSlant->\"Italic\"]\).";
 SSSConvert[string_String] := s @@ fromAlpha[string];
 SSSConvert[s[x__]] := toAlpha[{x}];
 fromAlpha[string_String] :=(ToCharacterCode[string]-65);
 toAlpha[l:{___Integer}] := FromCharacterCode[l+65];
 Attributes[s]=Flat;
 
 
 End[]
 
 EndPackage[]
 
