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

Clear[SSSInitialize];
SSSInitialize[rs:{___Rule},state_String,mode_:"Silent" (* "Silent" | "Loud" *)] :=
(
(* Tweak $MaxColor *)
$MaxColor:=Max[Flatten[{6,ToCharacterWeights /@ Flatten[rs/.Rule->List]}]];
(* initial setup *)
$SSSNet={};
$SSSOutDegreePotential=$SSSOutDegreeRemaining=$SSSOutDegreeActual={};
$SSSInDegree={}; (* {0} will be starting with one node, of in-degree 0 *)
$SSSDistance={0}; (* The starting node is 0 steps away from the starting node *)
$SSSConnectionList={}; 
$SSSTagIndex=StringLength[state]+1;                     (* value of next tag to use *)
$SSSTEvolution={s@@Transpose[{#,Range[Length[#]]}& @ fromAlpha[state]]};
$SSSEvolution = {state};
$SSSRuleSet = rs;
$SSSTRuleSet =SSSNewRule[rs];
$SSSRuleSetWeight = RuleSetWeight[rs];
$SSSRuleSetLength = RuleSetLength[rs];
$SSSMatchLengthHistory = $SSSHistory = {};
$SSSRepetitionInterval = $SSSRepetitionStart = 0;
$SSSStart = 1;
$SSSVerdict = "";
$SSSRuleUsage=Table[0,{Length[rs]}];  (* none of the rules have been used as yet *)

(* now do first singlestep, so we have at least 2 strings for the 
comparison tests used to determine $SSSVerdict and to build $SSSOutDegreeActual and $SSSOutDegreeRemaining *)
AppendTo[$SSSTEvolution,Last[$SSSTEvolution]/.$SSSTRuleSet];
AppendTo[$SSSEvolution,SSSStrip[Last[$SSSTEvolution]]];

If[ !MatchQ[mode, ("Silent"|"Quiet")],
Switch[Length[$SSSConnectionList],
0,Print["Error: No evolution possible starting from \""<>state<>"\" using ruleset: ",rs];Return[False],
_,Print["Successful initialization of ruleset: ",rs,", evolution: ",$SSSEvolution]
]
];

(* or if the first step of the evolution worked, update the global variables: *)
updateHistory;  (* $SSSOutDegreePotential, $SSSOutDegreeRemaining, $SSSOutDegreeActual, $SSSMatchLengthHistory, $SSSHistory, $SSSRuleUsage *)
True  (* = Success *)
);

* Below is code that must be executed each time a step is performed by SSSSingleStep, but it must also be done after for the first step, done by SSSInitialize, so it has been put into a separate function that both can call, updating $SSSOutDegreePotential, $SSSOutDegreeRemaining, $SSSOutDegreeActual, $SSSMatchLengthHistory, $SSSHistory. *)

updateHistory := Module[{cd},
AppendTo[$SSSInDegree,Length[$SSSConnectionList[[-1,1]]]];  (* # cells killed by this event = in-degree *)
(* calculate potential outdegree of new event, append to list *)
AppendTo[$SSSOutDegreePotential,Length[$SSSConnectionList[[-1,-1]]]];  (* # of cells created by last rule *)
AppendTo[$SSSOutDegreeRemaining,Last[$SSSOutDegreePotential]];
AppendTo[$SSSOutDegreeActual,0];

(* find cells deleted by latest event, use info to update $SSSHistory and $SSSMatchLengthHistory *)
cd=cellsDeleted[$SSSTEvolution[[-2;;]]];  (* comparing last 2 states, find deleted cell positions *)
cd=If[Length[cd]==0,{1,0},{First@cd,Last@cd}];  (* Take first & last or if no deletions -- there was an insertion at beginning of state string: match at pos 1, length 0 *)

(* $SSSMatchLengthHistory gets 3 numbers: lengths of pre-match, match, and post-match substrings *)
AppendTo[$SSSMatchLengthHistory,{First[cd]-1,Last[cd]-First[cd]+1,StringLength[$SSSEvolution[[-2]]]-Last[cd]}];

(* $SSSHistory gets 4 items: pre-match abbreviation, match rule #, post-match abbreviation, 0 *)
AppendTo[$SSSHistory,{
abbrev[#,$SSSRuleSetLength]& @ safeStringTake[$SSSEvolution[[-2]],{1,First[cd]-1}],  (* pre-match substring *)
StringTake[$SSSEvolution[[-2]],cd] /. Thread[$SSSRuleSet[[All,1]]->Range[Length[$SSSRuleSet]]],  (* matching rule # *)
(* Print["Trying to do safeStringTake[$SSSEvolution\[LeftDoubleBracket]-2\[RightDoubleBracket],{Last[cd]+1,-1}]: safeStringTake[",$SSSEvolution\[LeftDoubleBracket]-2\[RightDoubleBracket],", {Last[",cd,"} ]+1,-1}]"]; *)
abbrev[#,$SSSRuleSetLength]& @ safeStringTake[$SSSEvolution[[-2]],{Last[cd]+1,-1}],     (* post-match substring *)
0  (* tag to be used later to indicate shrinkage before/after match *)
}];
If[$SSSVerdict=!="Dead",$SSSRuleUsage[[$SSSHistory[[-1,2]]]]++ ] (* increment the usage counter for the rule that was used on this step *)
];

SSInitialize::usage = "SSSInitialize[ruleset,string,(mode)] attempts to perform the necessary initializion steps to 
generate sequential substitution system (SSS) evolutions and networks,\nstarting with a ruleset 
(e.g., {\"BA\"\[Rule]\"ABA\"}) and an initial state string (e.g., \"BABA\").  The True|False return value indicates whether 
initialization was successful.\n\nIf omitted, mode defaults to \"Silent\", suppressing the short error or success message.\n\nThe following global variables are reset by this operation:\n\n$SSSNet:\t\t\t\tthe causal network of the current SSS,\n$SSSInDegree:\t\t\tthe list of in-degrees for each node,\n$SSSOutDegreeActual:\t\tthe list of currently found out-degrees for each node,\n$SSSOutDegreePotential:\t\tthe list of maximum possible out-degrees for each node,\n$SSSOutDegreeRemaining:\tthe list of numbers of possible remaining out-connections for each node,\n$SSSConnectionList:\t\tthe current list of all causal network connections,\n$SSSDistance:\t\t\tthe list of minimum distances from the current node back to the starting node.\n$SSSTagIndex:\t\t\tthe current tag index being used,\n$SSSTEvolution:\t\t\tthe complete evolution of the tagged SSS so far,\n$SSSEvolution:\t\t\tthe stripped (tagless) version of $SSSTEvolution,\n$SSSRuleSet:\t\t\tthe ruleset used for creating the SSS,\n$SSSTRuleSet:\t\t\tthe version of $SSSRuleSet (created by the function SSSNewRule) used to build $SSSTEvolution,\n$SSSRuleSetWeight:\t\tthe total weight of $SSSRuleSet,\n$SSSRuleSetLength:\t\tthe total length of $SSSRuleSet,\n$SSSHistory:\t\t\tan annotated/translated version of $SSSEvolution used to detect repeating patterns,\n$SSSMatchLengthHistory:\ta list of the match positions that occurred in forming the SSS,\n$SSSRuleUsage:\t\t\tthe list of rule usage counters, later adjusted to identified portion of SSS,\n$SSSRepetitionInterval:\t\tthe length of the repeating section (if any) of the SSS,\n$SSSRepetitionStart:\t\tthe start of the repeating section (if any) of the SSS,\n$SSSStart:\t\t\tthe starting position for displaying the SSS and its causal network,\n$SSSVerdict:\t\t\tset to \"Dead\" | \"Repeating\" | \"Pseudorepeating\" as soon as the future of the SSS becomes clear.";
 
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
 



