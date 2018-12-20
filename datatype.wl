(* ::Package:: *)

(* Wolfram Language Server Data Type *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`DataType`"];

ClearAll["WolframLanguageServer`DataType`*"];


TypeCheck[v_?MissingQ,_] := v;
TypeCheck[_, p_?MissingQ] := p;
TypeCheck[val_, pat_] := If[MatchQ[val, pat], val,Missing["PatternMismatch", pat]];

DeclareType[typename_, typekey_Association]:=Module[
	{
	},
	
	typename[typedict_Association][key_String] := TypeCheck[typedict[key], typekey[key]];
];


EndPackage[];
