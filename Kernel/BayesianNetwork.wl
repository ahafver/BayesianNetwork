(* ::Package:: *)

BeginPackage["AndreasHafver`BayesianNetwork`"]

(* Declare your package's public symbols here. *)
BayesianNetwork

Begin["`Private`"]
(* Define your public and private symbols here. *)

ClearAll[BayesianNetwork];

(* Test function *)
BayesianNetworkQ[asc_?AssociationQ] := AllTrue[{"Variables","Distribution","Edges"}, KeyExistsQ[asc, #]&]

(* Visual appearance of BayesianNetworkObject*)
BayesianNetwork/:MakeBoxes[obj:BayesianNetwork[asc_?BayesianNetworkQ],form:(StandardForm|TraditionalForm)]:=
Module[{info},
	info={{BoxForm`SummaryItem[{Graph[asc["Edges"],VertexLabels->None,VertexSize->Large,ImageSize->50]}]}};
	BoxForm`ArrangeSummaryBox[BayesianNetwork,obj,None,info,{},form,"Interpretable"->Automatic]
]

(* Creating Bayesian network models *)
BayesianNetwork[vars_?(VectorQ[#,AssociationQ]&)] := Module[{names,distributions,name2slot,a,parents,edges,domainmaps,transformationrules,pdf,ranges,distribution,x},
	(* Extract names and distributions *)
	names = vars[[All,"Name"]];
	distributions = vars[[All,"Distribution"]];
	name2slot = Association@@Table[names[[i]]->Slot[i],{i,Length@names}];
	
	(* Create BN graph*)
	parents = Table[DeleteDuplicates[Cases[dist,Slot[a_]->a,-1]],{dist,distributions}];
	edges = Flatten[Table[Thread[parents[[i]]->names[[i]]],{i,Length@names}]];
	
	(* Transform all distributions to integer domains *)
	domainmaps = Table[AssociationThread[#->Range@Length@#]&@DistributionDomain[dist[[1]]],{dist,distributions}];
	transformationrules = Flatten@Table[
		Module[{slot,dom},
			dom = domainmaps[[i]];
			slot = Slot@names[[i]];
			MapThread[Equal[slot,#1]->Equal[slot,#2]&,{Keys[dom],Values[dom]}]
		],
	{i,Length@distributions}];
	distributions = distributions/.transformationrules;
	
	(* Replace named variables by dummy variables *)
	distributions = Table[
		Module[{args,dist,dom,probs},
			args = Thread[name2slot[[parents[[i]]]]];
			dist = distributions[[i]][args];
			dom = DistributionDomain[dist];
			probs = Table[Evaluate[PDF[dist,y]],{y,dom}];
			EmpiricalDistribution[probs->Range@Length@dom]
		]
	,{i,Length@distributions}];	

	(* Create joint distribution *)
	pdf = Function[Evaluate[Times@@Table[PDF[distributions[[i]],Slot[i]],{i,Length@distributions}]]];
	ranges = Table[{x[i],Sequence@@MinMax[DistributionDomain[distributions[[i]]]],1},{i,Length@distributions}];
	distribution = ProbabilityDistribution[pdf@@Table[x[i],{i,Length@distributions}],Sequence@@ranges];

	(* Return BN object *)
	BayesianNetwork[<|"Variables"->Table[<|"Name"->names[[i]],"Parents"->parents[[i]],"Domain"->domainmaps[[i]],"Distribution"->distributions[[i]]|>,{i,Length@names}],"Distribution"->distribution,"Edges"->edges|>]
]

(* Create information object *)
Information`AddRegistry[BayesianNetwork, getBayesianNetworkInformation];
getBayesianNetworkInformation[BayesianNetwork[asc_?BayesianNetworkQ]] := <|
	"ObjectType" -> "Bayesian Network", <||>, 
	"Variable count" :> Length[asc["Variables"]],
	"Variables"->asc[["Variables",All,"Name"]],
	"Variable domains":>Map[Keys,asc[["Variables",All,"Domain"]]],
	"Edge count":>Length[asc["Edges"]],
	"Edges"->asc["Edges"]
|>


(* Calculation of probabilities *)
Unprotect[Probability];
BayesianNetwork/: Probability[expr_Function,BayesianNetwork[asc_?BayesianNetworkQ],opts:OptionsPattern[Probability]]:= Module[{variables,variableNames,variableSlots,variableAssociation,dom,dist,x,rules,newexpr,slot},
	variables = asc["Variables"];
	variableNames = variables[[All,"Name"]];
	variableSlots = Table[x[i],{i,Length@variableNames}];
	variableAssociation = Association[Thread[variableNames->variableSlots]];

	(* Transform expression to the BN dummy variables with integer domains *)
	newexpr = expr; 
	Do[
		dom = variables[[i]]["Domain"];
		slot = Slot@variableNames[[i]];
		rules = MapThread[Equal[slot,#1]->Equal[slot,#2]&,{Keys[dom],Values[dom]}];
		newexpr = (newexpr/.rules),
	{i,Length@variables}];

	(* Compute the probability *)
	Probability[newexpr[variableAssociation],variableSlots \[Distributed] asc["Distribution"],opts]
]
Protect[Probability];

(* Visualising Bayesian networks *)
Unprotect[Graph];
BayesianNetwork/: Graph[BayesianNetwork[asc_?BayesianNetworkQ],evidence_Function:Function[True],opts:OptionsPattern[Graph]]:= Module[{variables,edges,evidencenodes,a,colour,labels,probs,domain,outcomes,barcharts},
	variables = asc["Variables"];
	edges = asc["Edges"];
	
	If[OptionValue[FilterRules[{opts},VertexLabels],VertexLabels] ==="Probabilities",
		If[evidence===Function[True],
			evidencenodes = {},
			evidencenodes = DeleteDuplicates[Cases[evidence,Slot[a_]->a,-1]]
		];
		labels = Table[
			domain = {Slot[var["Name"]],Sequence@@MinMax[var["Domain"]],1};
			probs = Table[Probability[Evaluate[Slot[var["Name"]]==val\[Conditioned] evidence[[1]]]&,BayesianNetwork[asc]],{val,Keys[var["Domain"]]}];
			If[VectorQ[probs,NumericQ],
				var["Name"]->Labeled[BarChart[probs,PlotRange->{0,1},BarOrigin->Left,ImageSize->Tiny,Frame->True,FrameTicks->{None,Automatic},ChartLabels->Keys[var["Domain"]],ChartStyle->If[MemberQ[evidencenodes,var["Name"]],Blue,Orange]],var["Name"],Top],
				var["Name"]->Labeled[TableForm[probs,TableHeadings->{Keys[var["Domain"]],None}],var["Name"],Top]
			],
			{var,variables}];
			Graph[edges,VertexLabels->labels,opts],
			Graph[edges,opts]
		]
]
Protect[Graph];


End[] (* End `Private` *)

EndPackage[]
