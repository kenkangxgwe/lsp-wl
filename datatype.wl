(* ::Package:: *)

(* Wolfram Language Server Data Type *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`DataType`"];

ClearAll["WolframLanguageServer`DataType`*"];


TypeCheck[v_?MissingQ, _] := v;
TypeCheck[_, p_?MissingQ] := p;
TypeCheck[val_, pat_] := If[MatchQ[val, pat], val, Missing["PatternMismatch", pat]];

ReplaceKey[_, {} -> value_] := value;
ReplaceKey[list_List, key_Integer -> value_] :=
	ReplaceKey[list, {key} -> value];
ReplaceKey[list_List, {key_Integer, keys___} -> value_] := 
	ReplacePart[list, key -> ReplaceKey[Extract[key][list], keys -> value]];
	
ReplaceKey[assoc_Association, key_ -> value_] :=
	ReplaceKey[assoc, {key} -> value];
ReplaceKey[assoc_Association, {key_, keys___} -> value_] := 
	ReplacePart[assoc, key -> ReplaceKey[Extract[key][assoc], keys -> value]];
		
DeclareType[typename_, typekey_Association] := Module[
	{
	},
	
	typename[typedict_Association][key_String] := TypeCheck[typedict[key], typekey[key]];
	ReplaceKey[typename[typedict_Association], key_String -> value_] :=
		ReplaceKey[typename[typedict], {key} -> value];
	ReplaceKey[typename[typedict_Association], {key_String, keys___} -> value_] := 
		typename[ReplacePart[typedict, key -> ReplaceKey[typedict[key], keys -> value]]];
];


EndPackage[];
