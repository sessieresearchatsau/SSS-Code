(* ::Package:: *)

BeginPackage["SSS`"]

(* is this public? *)

SSS::usage = "Functions related to Sequential Substitution Systems (SSS) & Causal Network Construction."
 
 Begin["`Private`"]
 
(*  SSS & Casual Network Construction *)
fromAlpha[string_String] :=(ToCharacterCode[string]-65);  
toAlpha[l:{___Integer}] := FromCharacterCode[l+65];

Attributes[s]=Flat;
SSSConvert[string_String] := s @@ fromAlpha[string];
SSSConvert[s[x__]] := toAlpha[{x}];
SSSConvert::usage="Converts SSS (sequential substitution system) states between s- andstring-formats, 
				   using the functions \!\(\*StyleBox[\"fromAlpha\",FontSlant->\"Italic\"]\) and 
                   \!\(\*StyleBox[\"toAlpha\",FontSlant->\"Italic\"]\).";
                   
(* Character Weight Calculation *)        
ToCharacterWeights[s_String] := (1+fromAlpha[s]);
FromCharacterWeights[l:{___Integer}] := toAlpha[l-1];
(* Note: To avoid breaking the ruleset (un-)rank functions, avoid the temptation to define:  
ToCharacterWeights[""] = 0;  FromCharacterWeights[{0}]="";  *)
StringWeight[s_String] := Plus @@ ToCharacterWeights[s];
RuleSetWeight[rs_List] := Plus @@ (StringWeight /@ Flatten[rs /. Rule->List]);
RuleSetLength[rs_List] := Plus @@ (StringLength /@ Flatten[rs /. Rule->List]);

(* SSSRuleIcon *)
$MaxColor=6;  (* If you want to display SSSs with more than 6 symbols, just change this variable, 
				otherwise the colors wrap.  (Now SSSInititialize modifies this automatically.) *)
myColors=Sequence[ColorFunction->(Hue[(#-1)/$MaxColor]&),ColorFunctionScaling->False];
patternPrint[pattern_,opts___] := ArrayPlot[{{##}/. 0->LightGray}& @@pattern,myColors,Mesh->True,opts,
												ImageSize->{Automatic,20}];
Clear[SSSRuleIcon];
SSSRuleIcon[(rule_String|rule_Rule|rule_RuleDelayed),x___]:=SSSRuleIcon[{rule},x];

SSSRuleIcon[rules_List,x___]:=SSSRuleIcon[Map[SSSConvert,rules,{-1}],x] /; !FreeQ[rules,_String,Infinity];

SSSRuleIcon[rules_List,opts___] := Panel[Grid[Map[patternPrint[#,opts]&,rules,{2}] /. 
{Rule[x_,y_]:>{x,"\[AlignmentMarker]\[Rule]",y},RuleDelayed[x_,y_]:>{x,"\[AlignmentMarker]\[RuleDelayed]",y}}, (* invisible AlignmentMarkers! *)
Alignment->Left],"Substitution Rule"<>If[Length[rules]>1,"s:",":"]] /; FreeQ[rules,_String,Infinity];

SSSRuleIcon::usage="SSSRuleIcon[\!\(\*
StyleBox[\"rule\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"s\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)] generates an icon for a sequential substitution system (SSS) 
					rule or set of rules.";
					
(* Generate a ruleset for the tagged SSS: SSSNewRule *)
SSSNewRule[rule_Rule | rule_RuleDelayed] := Module[{lhs,rhs,lhsNames,newlhs,newrhs1,newrhs2},
{lhs,rhs}=List@@rule;
lhsNames = Table[Unique[lhsTag],{StringLength[lhs]}];
newlhs=ToString[s@@Transpose[{fromAlpha[lhs],ToString@#<>"_"& /@ lhsNames}]];
newrhs1=("AppendTo[$SSSConnectionList, "<>ToString[lhsNames]<>" \[Rule] $SSSTagIndex + "<>ToString[Range@StringLength@rhs-1]<>"]; ");
newrhs2=ToString[SSSConvert[rhs] /. n_Integer :> {n,"$SSSTagIndex++"}];
ToExpression[newlhs<>" \[RuleDelayed] ("<>newrhs1<>newrhs2<>")"]];
SSSNewRule[rules_List] := SSSNewRule/@rules;
SSSNewRule::usage= 
"SSSNewRule[\!\(\*
StyleBox[\"rule\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"s\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"generates\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"needed\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"rule\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"s\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"for\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"tagged\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"SSS\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)(sequential substitution system) 
 from the \!\(\*
StyleBox[\"rule\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"s\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\) given in string-format: e.g., \"BA\"\[Rule]\"ABA\"";

(* Stripping out tags *)
Clear[SSSStrip];
SSSStrip[x_s] := SSSConvert[x[[All,1]]] /; MatrixQ[List@@x]   (* if dim=2, take only 1st component, and convert *)
SSSStrip[s[]]=""   (* treat empty string case *)  ;
SSSStrip::usage="SSSStrip[\!\(\*
StyleBox[\"state\",\nFontSlant->\"Italic\"]\)] strips out tags from a \!\(\*
StyleBox[\"state\",\nFontSlant->\"Italic\"]\) given in tagged SSS 
				(sequential substitution system) format and returns it in string format.";
				
(* Boilerpolate for dealing with strings & cells *)
cellsDeleted[{l1_,l2_}] := Flatten[Position[l1,_?(!MemberQ[l2,#]& ),{1},Heads->False] ];

safeStringTake[s_String,{n1_Integer,n2_Integer}] := StringTake[s,{Max[1,n1],Min[StringLength[s],n2]}];
safeStringTake[s_String,{n1_Integer,n2_Integer}] := safeStringTake[s,{StringLength[s]+1+n1,n2}]  /; -StringLength[s]<=n1<0
safeStringTake[s_String,{n1_Integer,n2_Integer}] := safeStringTake[s,{n1,StringLength[s]+1+n2}]  /; -StringLength[s]<=n2<0

Clear[abbrev];
abbrev[s_String,n_Integer] :={safeStringTake[s,{1,n}],safeStringTake[s,{-n,-1}]}; 
 (* abbrev always returns a list of 2 strings, the beginning and ending of the string variable *)
abbrev[s_String,0]:= {"",""};
 (* use as "abbreviation" of long strings the list of substrings of the first & last n characters *)
abbrev[s_String,n_] := (Print[Stack[]];s)  (* anything else happens, let's debug! *)

envelopeSubtractStrings[big_String,little_String] := Module[{lenbig=StringLength[big],lenlittle=StringLength[little],i=1,j=1,bigc=Characters[big],littlec=Characters[little]},
If[lenbig<lenlittle,Return[Reverse[envelopeSubtractStrings[little,big]]]];
For[i=1,i<=lenlittle,i++,If[bigc[[i]]!=littlec[[i]],Break[]]];  (* now i is the first NON-matching character, or lenlittle + 1 *)
For[j=1,j<=lenlittle+1-i,j++,If[bigc[[-j]]!=littlec[[-j]],Break[]]]; (* now j is the first non-matching character starting from the end, or the first character that matched from the beginning *)
{StringJoin @ bigc[[;;i-1]],StringJoin /@ Rule[littlec[[i;;-j]],bigc[[i;;-j]]],StringJoin @ bigc[[-j+1;;]]};
{{i,-j},StringJoin /@ Rule[littlec[[i;;-j]],bigc[[i;;-j]]]};
StringJoin /@ Rule[littlec[[i;;-j]],bigc[[i;;-j]]]
]; (* returns a list of replacements made *)

(* SSS Initialize  *)
 
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
 



