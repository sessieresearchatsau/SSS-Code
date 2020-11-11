(* ::Package:: *)

BeginPackage["Main`"]

(* SSSEvolve *)
Clear[SSSEvolve];
Options[SSSEvolve]={EarlyReturn->False, Mode->Silent};
SyntaxInformation[SSSEvolve]={"ArgumentsPattern"->{OptionsPattern[]}};

SSSEvolve[n_Integer/;n>0,opts:OptionsPattern[]] := (
If[OptionValue[EarlyReturn] ,
Do[If[MatchQ[$SSSVerdict, ("Dead"|"Repeating")],Break[],SSSSingleStep],{n}], (* check before each step *)
Do[SSSSingleStep,{n}]   (* just do it *)
];
If[OptionValue[Mode]==Loud,Print[$SSSVerdict]];
True
)

SSSEvolve::usage="SSSEvolve[\!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\)] generates an additional \!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\) levels of the \!\(\*
StyleBox[\"current\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"SSS\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)(sequential substitution system), 
					which must have been previously initialized using SSSInitialize.  Use the option EarlyReturn \[Rule] True 
					to allow early termination for repeating cases.  (SSSSinglestep immediately returns anyway if the SSS 
					is dead.)  In Loud mode, prints the current verdict, \"OK\" means none known.  Global variables set 
					up by SSSInitialize are updated, with $SSSEvolution containing the tagless SSS, $SSSConnectionList 
					the updated causal network connection list, etc.  mode can be Silent, Quiet, or Loud.";

(* Actual SSS (Object?) *)
SSS[rs:{___Rule},init_String,n_Integer?Positive,opts___] := 
If[SSSInitialize[rs,init,Mode->Silent], 
SSSEvolve[n-1,Sequence@@FilterRules[{opts},Options[SSSEvolve]]]; 
SSSDisplay[Sequence@@FilterRules[{opts},Options[SSSDisplay]]]
];
Options[SSS]=Join[Options[SSSEvolve],Options[SSSDisplay]];
SyntaxInformation[SSS]={"ArgumentsPattern"->{_,_,_,OptionsPattern[]}};

SSS::usage="SSS[\!\(\*
StyleBox[\"rule\",\nFontSlant->\"Italic\"]\)set\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"init\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"opts\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)creates and displays a sequential substitution system (SSS) 
			and its causal network, using \!\(\*
StyleBox[\"ruleset\",\nFontSlant->\"Italic\"]\) starting with the state \!\(\*
StyleBox[\"init\",\nFontSlant->\"Italic\"]\) (using string notation), 
			allowing the SSS to evolve for \!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\) steps.  Use the option EarlyReturn to give/deny permission to 
			quit early if the SSS can be identified as dead or (pseudo-)repeating.)  Any other options given 
			are passed on to SSSDisplay.

(After creation the current SSS can be displayed or manipulated without rebuilding, using SSSDisplay, SSSAnimate, or directly using the global variables $SSSRuleSet, $SSSEvolution and $SSSNet.)";

Begin["`Private`"]

(* Global Variables *)
$MaxColor = 6; (* If you want to display SSSs with more than 6 symbols, just change this variable, 
					otherwise the colors wrap.  (Now SSSInititialize modifies this automatically. Really?!) *)

(* Converts a string to a list of char values *)
Clear[FromAlpha,ToAlpha,SSSConvert,SSSStrip];
FromAlpha[string_String] :=(ToCharacterCode[string]-65);  
ToAlpha[l:{___Integer}] := FromCharacterCode[l+65];

Attributes[s]=Flat;
SSSConvert[string_String] := s @@ FromAlpha[string];
SSSConvert[s[x___]] := ToAlpha[{x}];
SSSConvert::usage="Converts SSS (sequential substitution system) states between s- and string-formats, 
					using the functions \!\(\*
StyleBox[\"FromAlpha\",\nFontSlant->\"Italic\"]\) and \!\(\*
StyleBox[\"ToAlpha\",\nFontSlant->\"Italic\"]\).";

SSSStrip[x_s] := SSSConvert[x[[All,1]]] /; MatrixQ[List@@x]    (* if dim=2, take only 1st component, and convert *)
SSSStrip[x_s] := ""  /; Length[List@@x]==0     (* treat empty string case *) 

(* I.e. SSSStrip[s[{0,1},{1,4},{2,5}]] outputs "ABC" *)
SSSStrip::usage="SSSStrip[\!\(\*StyleBox[\"state\",FontSlant->\"Italic\"]\)] strips out tags from a
					\!\(\*StyleBox[\"state\",FontSlant->\"Italic\"]\) given in tagged SSS 
					(sequential substitution system) format and returns it in string format."

(* Character Weights *)
ToCharacterWeights[s_String] := (1+FromAlpha[s]);
FromCharacterWeights[l:{___Integer}] := ToAlpha[l-1];
(* Note: To avoid breaking the ruleset (un-)rank functions, avoid the temptation to define:  
ToCharacterWeights[""] = 0;  FromCharacterWeights[{0}]="";  *)
StringWeight[s_String] := Plus @@ ToCharacterWeights[s];
RuleSetWeight[rs_List] := Plus @@ (StringWeight /@ Flatten[rs /. Rule->List]);
RuleSetLength[rs_List] := Plus @@ (StringLength /@ Flatten[rs /. Rule->List]);

(* Colors *)
myColors = Sequence[ColorFunction->(Hue[(#-1)/$MaxColor]&),ColorFunctionScaling->False];
patternPrint[pattern_,opts___] := 
	ArrayPlot[{{##}/. 0->LightGray}& @@pattern,myColors,Mesh->True,opts,ImageSize->{Automatic,20}];

(* SSSRuleIcons *)
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

(* SSS Rulesets *)
Clear[SSSNewRule];
SSSNewRule[rulenum_Integer,(rule_Rule | rule_RuleDelayed)] := Module[{lhs,rhs,lhsNames,newlhs,newrhs1,newrhs2},
{lhs,rhs}=List@@rule;
lhsNames = Table[Unique[lhsTag],{StringLength[lhs]}];
newlhs=ToString[s@@Transpose[{FromAlpha[lhs],ToString@#<>"_"& /@ lhsNames}]];
newrhs1=("AppendTo[$SSSConnectionList, "<>ToString[lhsNames]<>" \[Rule] $SSSTagIndex + "<>ToString[Range@StringLength@rhs-1]<>"]; ");
newrhs2=ToString[SSSConvert[rhs] /. n_Integer :> {n,"$SSSTagIndex++"}];
ToExpression[newlhs<>" \[RuleDelayed] ("<>"AppendTo[$SSSRulesUsed,"<>ToString@rulenum<>"];"<>newrhs1<>newrhs2<>")"]];
SSSNewRule[rules_List] := Append[MapIndexed[SSSNewRule[First[#2],#1]&,rules],___:>AppendTo[$SSSRulesUsed,0]];

SSSNewRule::usage="SSSNewRule[\!\(\*
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
StyleBox[\"rules\",\nFontSlant->\"Plain\"]\)\!\(\*
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
StyleBox[\"ruleset\",\nFontSlant->\"Italic\"]\) of rules given in string-format: e.g., \"BA\"\[Rule]\"ABA\"";
					
(* SSSInitialize *)
Clear[SSSInitialize];
Options[SSSInitialize]={Mode->Silent};
SyntaxInformation[SSSEvolve]={"ArgumentsPattern"->{OptionsPattern[]}};

SSSInitialize[rs:{___Rule},state_String,opts:OptionsPattern[] (* Mode \[Rule] Silent | Quiet | Loud *)] :=
(   (* initial setup *)
$MaxColor:=Max[Flatten[{6,ToCharacterWeights /@ Flatten[rs/.Rule->List]}]]; (* Tweak $MaxColor *)
$SSSNet={};
$SSSOutDegreePotential=$SSSOutDegreeRemaining=$SSSOutDegreeActual={};
$SSSInDegree={}; (* {0} will be starting with one node, of in-degree 0 *)
$SSSDistance={0}; (* The starting node is 0 steps away from the starting node *)
$SSSConnectionList={}; 
$SSSTagIndex=StringLength[state]+1;                     (* value of next tag to use *)
$SSSTEvolution={s@@Transpose[{#,Range[Length[#]]}& @ FromAlpha[state]]};
$SSSEvolution = {state};
$SSSRuleSet = rs;
$SSSTRuleSet =SSSNewRule[rs];
$SSSRuleSetWeight = RuleSetWeight[rs];
$SSSRuleSetLength = RuleSetLength[rs];
$SSSVerdict = "OK";
$SSSRulesUsed={};
$SSSCellsDeleted={};

AppendTo[$SSSTEvolution,Last[$SSSTEvolution]/.$SSSTRuleSet];
Switch[ Last[$SSSRulesUsed], (* can also test if Length[$SSSConnectionList]\[Equal]0 *)
0,$SSSTEvolution=Most[$SSSTEvolution]; (* toss last duplicate entry *)
      $SSSVerdict = "Dead";
      If[ OptionValue[Mode]==Loud,Print["Error: No evolution possible starting from \""<>state<>"\" using ruleset: ",rs]];
      Return[False],
_, AppendTo[$SSSEvolution,SSSStrip[Last[$SSSTEvolution]]]; (* add last entry *)
       If[ OptionValue[Mode]==Loud,Print["Successful initialization of ruleset: ",rs,", evolution: ",$SSSEvolution]]
];
updateDegrees;
True  (* = Success *)
);

updateDegrees := (
AppendTo[$SSSInDegree,Length[$SSSConnectionList[[-1,1]]]];  (* # cells killed by this event = in-degree *)
(* calculate potential outdegree of new event, append to list *)
AppendTo[$SSSOutDegreePotential,Length[$SSSConnectionList[[-1,-1]]]];  (* # of cells created by last rule *)
AppendTo[$SSSOutDegreeRemaining,Last[$SSSOutDegreePotential]];
AppendTo[$SSSOutDegreeActual,0];
AppendTo[$SSSCellsDeleted,Flatten[Position[$SSSTEvolution[[-2]],{_,#}]& /@ $SSSConnectionList[[-1,1]]]] (* Note positions of entries with tags indicated, add to the list *)
)

(* SSSSingleStep *)
Clear[SSSSingleStep];
SSSSingleStep := Module[{cd, ri, pri, rs, prs,len,startingEvents},
If[$SSSVerdict==="Dead",Return[False]];  (* if already dead, do nothing, return *)

(* do the actual evolution step *)
AppendTo[$SSSTEvolution,Last[$SSSTEvolution]/.$SSSTRuleSet];
 If[Last[$SSSRulesUsed]==0,$SSSVerdict="Dead";$SSSTEvolution=Most[$SSSTEvolution]; Return[False]];
AppendTo[$SSSEvolution,SSSStrip[Last[$SSSTEvolution]]];

If[!MatchQ[$SSSVerdict,"Repeating"],  (* to limit wasted time, don't do this if the verdict is already in! *)
If[Length[Flatten@Position[$SSSEvolution,Last[$SSSEvolution]]]>1, $SSSVerdict="Repeating"]]; 

(* Now update globals:  first the part that is done for each step, including the first one (by SSSInitialize): *)
updateDegrees; (* $SSSOutDegreePotential, $SSSOutDegreeRemaining, $SSSOutDegreeActual, [omitted: $SSSMatchLengthHistory, $SSSHistory] *)

(* now the steps that are only done for non-initial steps, comparing to previous entries in $SSSConnectionList: *)
len=Length[$SSSConnectionList];
startingEvents=Flatten[If[Length[#]>0,First[First[#]],#]& /@ (Position[$SSSConnectionList[[;;-2]],#]& /@ $SSSConnectionList[[-1,1]])];
$SSSOutDegreeActual[[#]]++& /@ startingEvents; (* update out-degee list for events involved *)
$SSSOutDegreeRemaining[[#]]--& /@ startingEvents; (* update out-degee list for events involved *)
$SSSNet=Join[$SSSNet,#->len& /@ startingEvents];           (* add new links to the causal network *)
AppendTo[$SSSDistance,Min[$SSSDistance[[startingEvents]]]+1];  (* Find minimum path length of cause nodes, add 1 for path lengths of result nodes *)
Return[True];
];

End[]

EndPackage[]



