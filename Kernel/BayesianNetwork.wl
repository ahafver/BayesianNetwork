(* ::Package:: *)

BeginPackage["ahafver`BayesianNetwork`"]

(* Declare your package's public symbols here. *)
BayesianNetwork
BayesianNetworkObject

Begin["`Private`"]
(* Define your public and private symbols here. *)

ClearAll[BayesianNetwork,BayesianNetworkObject];

(* Utility functions *)
BayesianNetworkQ[asc_?AssociationQ] := AllTrue[{"Variables","Distribution","Graph"}, KeyExistsQ[asc, #]&]

(* Visual appearance of BayesianNetworkObject*)
BayesianNetworkObject/:MakeBoxes[obj:BayesianNetworkObject[asc_?BayesianNetworkQ],form:(StandardForm|TraditionalForm)]:=
Module[{info},
	info={{BoxForm`SummaryItem[{Graph[asc["Graph"],VertexLabels->None,VertexSize->Large,ImageSize->50]}]}};
	BoxForm`ArrangeSummaryBox[BayesianNetworkObject,obj,None,info,{},form,"Interpretable"->Automatic]
]

(* Creating Bayesian network models *)
BayesianNetwork[vars_?(VectorQ[#,AssociationQ]&)] := Module[{names,name2slot,x,a,distributions,parents,edges,graph,args,originalDomains,newDomains,domainmaps,transformationrules,pdf,ranges,dist},
	names = vars[[All,"Name"]];
	distributions = vars[[All,"Distribution"]];
	name2slot = Association@@Table[names[[i]]->Slot[i],{i,Length@names}];
	
	(* Create BN graph*)
	parents = Table[DeleteDuplicates[Cases[dist,Slot[a_]->a,-1]],{dist,distributions}]; (* Extracts all variable slots in the distribution expressions *)
	edges = Flatten[Table[Table[parents[[i,j]]->names[[i]],{j,Length@parents[[i]]}],{i,Length@names}]];
	graph = Graph[edges,VertexLabels->Automatic];
	
	(* Transform all distributions to integer domains *)
	originalDomains = Map[DistributionDomain[#[[1]]]&,distributions];
	newDomains = Map[Range@Length@#&,originalDomains];	
	domainmaps = MapThread[Association@@Thread[#1->#2]&,{originalDomains,newDomains}];
	transformationrules = Flatten@Table[
		Module[{slot,dom},
			dom = domainmaps[[i]];
			slot = Slot@names[[i]];
			MapThread[Equal[slot,#1]->Equal[slot,#2]&,{Keys[dom],Values[dom]}]
		],
	{i,Length@distributions}];
	distributions = distributions/.transformationrules;
	
	(* Replace named variables by dummy variables*)		
	args = Map[Thread[name2slot[[#]]]&,parents];
	distributions = MapThread[#1[#2]&,{distributions,args}];
	distributions = Table[
		Module[{dom,probs},
			dom = DistributionDomain[dist];
			probs = Table[Evaluate[PDF[dist,y]],{y,dom}];
			EmpiricalDistribution[probs->Range@Length@dom]
		]
	,{dist,distributions}];

	(* Create joint distribution *)
	pdf = Function[Evaluate[Times@@Table[PDF[distributions[[i]],Slot[i]],{i,Length@distributions}]]];
	ranges = Table[{x[i],Sequence@@MinMax[DistributionDomain[distributions[[i]]]],1},{i,Length@distributions}];
	dist = ProbabilityDistribution[pdf@@Table[x[i],{i,Length@distributions}],Sequence@@ranges];

	(* Return BM object *)
	BayesianNetworkObject[<|"Variables"->Table[<|"Name"->names[[i]],"Parents"->parents[[i]],"Domain"->domainmaps[[i]],"Distribution"->distributions[[i]]|>,{i,Length@names}],"Distribution"->dist,"Graph"->graph|>]
]


(* Calculating probabilities *)
Unprotect[Probability];
BayesianNetworkObject/: Probability[expr_Function,BN_BayesianNetworkObject]:= Module[{variables,variableNames,variableSlots,variableAssociation,dom,dist,x,rules,newexpr,slot},
	variables = BN[[1,"Variables"]];
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
	Probability[newexpr[variableAssociation],variableSlots \[Distributed] BN[[1,"Distribution"]]]/.{0.->0,1.->1}
]
Protect[Probability];

(* Visualising Bayesian networks *)
Unprotect[Graph];
BayesianNetworkObject/: Graph[BN_BayesianNetworkObject,evidence_:None,OptionsPattern[]]:= Module[{options,variables,graph,evidencenodes,a,colour,labels,probs,domain,outcomes,barcharts},
	options = Options[Graph];
	variables = BN[[1,"Variables"]];
	graph = BN[[1,"Graph"]];
	If[evidence===None,
		evidencenodes = {},
		evidencenodes = DeleteDuplicates[Cases[evidence,Slot[a_]->a,-1]]
	];
	labels = Table[
		domain = {Slot[var["Name"]],Sequence@@MinMax[var["Domain"]],1};
		If[evidence===None,
			probs = Simplify[Table[Probability[Evaluate[Slot[var["Name"]]==val]&,BN],{val,Keys[var["Domain"]]}]/.{0.->0,1.->1}],
			probs = Simplify[Table[Probability[Evaluate[Slot[var["Name"]]==val\[Conditioned] evidence[[1]]]&,BN],{val,Keys[var["Domain"]]}]/.{0.->0,1.->1}]
		];
		If[VectorQ[probs,NumericQ],
			var["Name"]->Labeled[
				BarChart[probs,PlotRange->{0,1},BarOrigin->Left,ImageSize->Tiny,Frame->True,FrameTicks->{None,Automatic},ChartLabels->Keys[var["Domain"]],ChartStyle->If[MemberQ[evidencenodes,var["Name"]],Blue,Orange]],
				var["Name"],Top],
			var["Name"]->Labeled[TableForm[probs,TableHeadings->{Keys[var["Domain"]],None}],var["Name"],Top]
		],{var,variables}];
	Graph[graph,VertexLabels->labels,ImageSize->Large]
]
Protect[Graph];



End[] (* End `Private` *)

EndPackage[]
