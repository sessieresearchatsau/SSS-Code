(* ::Package:: *)

BeginPackage["test`"]
fromAlpha[string_String] :=(ToCharacterCode[string]-65);

Begin["`Private`"]
fromAlpha1[string_String] :=(ToCharacterCode[string]-65)
End[]

EndPackage[]
