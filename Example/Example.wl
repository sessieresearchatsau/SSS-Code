(* ::Package:: *)

BeginPackage["Example`"]

fromAlphaPublic[string_String] := fromAlpha[string];

Begin["`Private`"]

fromAlpha[string_String] := (ToCharacterCode[string] - 65);

End[]

EndPackage[]



