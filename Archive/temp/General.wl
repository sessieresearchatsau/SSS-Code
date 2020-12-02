(* ::Package:: *)

BeginPackage["General`"]

General::usage = "A basic library of functions used everywhere."

Begin["`Private`"]

(* SSS & Causal Network Construction *)
fromAlpha::usage = "FromAlpha[string] returns a list if ints representing each character in the input string";
fromAlpha[string_String] := (ToCharacterCode[string] - 65);
toAlpha::usage = "toAlpha[{___Integer}] returns a string by converting each integer to a character based on ASCII";
toAlpha[l:{___Integer}] := FromCharacterCode(l + 65);

Attributes[s] = Flat;
SSSConvertPublic::usage = "Converts SSS (sequential substitution system) states between s- andstring-formats, 
				    using the functions \!\(\*StyleBox[\"fromAlpha\",FontSlant->\"Italic\"]\) and 
                    \!\(\*StyleBox[\"toAlpha\",FontSlant->\"Italic\"]\).";
SSSConvert[string_String[ := s @@ fromAlpha[string];
SSSConvert[s[x__]] := toAlpha[{x}];

(* Character Weight Calculation *)
ToCharacterWeights[s_String] := (1+fromAlpha[s]);
FromCharacterWeights[l:{___Integer}] := toAlpha[l-1];
(* Note: To avoid breaking the ruleset (un-)rank functions, avoid the temptation to define:  
ToCharacterWeights[""] = 0;  FromCharacterWeights[{0}]="";  *)
StringWeight[s_String] := Plus @@ ToCharacterWeights[s];
RuleSetWeight[rs_List] := Plus @@ (StringWeight /@ Flatten[rs /. Rule->List]);
RuleSetLength[rs_List] := Plus @@ (StringLength /@ Flatten[rs /. Rule->List]);


End[]

EndPackage[]



