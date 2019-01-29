(* ::Package:: *)

(* Wolfram Language Server Data Type *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`DataType`"];
ClearAll[Evaluate[Context[] <> "*"]];


DeclareType::usage = "DeclareType[typename, <|key_String -> pattern...|>] declares a type given by its name and an association indicating every field and its pattern in the type. \
typename[key] gets access to the corresponding value. \
typename[key, default] gets access to the corresponding value. If it is missing, returns the default.";
Keys::usage = Keys::usage <> "\nKeys[typename] gives a list of the keys field_i in type typename.";
KeyPatterns::usage = "KeyPatterns[typename] returns the key-pattern pair of the type.";
ReplaceKey::usage = "ReplaceKey[object, key -> value] assigns the value to key in given object.
ReplaceKey[object, {key1, key2} -> value] assigns the value to object[key1][key2].
Replacekey[replaceRule_Rule] is an operator that can be applied to an object.";


(* ::Section:: *)
(*DeclareType*)


DeclareType[typename_Symbol, typekey:<|(_String -> _)...|>] := Module[
	{
	},
	(* Getter *)
	typename[typedict_Association][key_String] := TypeCheck[typedict[key], typekey[key]];
	typename[typedict_Association][key_String, default_] := With[
		{value = typename[typedict][key]},
		If[MissingQ[value], TypeCheck[default, typekey[key]], value]
	];
    
    (* Keys and Patterns *)
	Keys[typename] ^= Keys[typekey];
	KeyPatterns[typename] = typekey;
	
	(* ReplaceKey *)
	ReplaceKey[typename[typedict_Association], key_String -> value_] :=
		ReplaceKey[typename[typedict], {key} -> value];
	ReplaceKey[typename[typedict_Association], {key_String, keys___} -> value_] := 
		typename[Append[typedict, key -> ReplaceKey[typedict[key], {keys} -> value]]];
	
	(* SameQ *)
	typename /: SameQ[typename[typedict1_Association], typename[typedict2_Association]] := (
		AssociationSameQ[typedict1, typedict1]
	);
	
	(* usage *)
	Evaluate[typename]::usage = StringJoin[{
	    ToString[typename, InputForm],
	    "[<|",
	    Riffle[KeyValueMap[{#1, " -> ", ToString[#2]}&, typekey], ", "],
	    "|>]",
	    Replace[Evaluate[typename]::usage, _MessageName -> "."]
	}];
];


(* ::Section:: *)
(*TypeCheck*)


TypeCheck[v_?MissingQ, _] := v;
TypeCheck[_, p_?MissingQ] := p;
TypeCheck[val_, pat_] := If[MatchQ[val, pat], val, Missing["PatternMismatch", pat]];


(* ::Section:: *)
(*ReplaceKey*)


ReplaceKey[_, {} -> value_] := value;
ReplaceKey[rule_Rule][obj_] := ReplaceKey[obj, rule];

ReplaceKey[list_List, key_Integer -> value_] :=
	ReplaceKey[list, {key} -> value];
ReplaceKey[list_List, {key_Integer, keys___} -> value_] := 
	ReplacePart[list, key -> ReplaceKey[Extract[key][list], {keys} -> value]];
	
ReplaceKey[assoc_Association, key_ -> value_] :=
	ReplaceKey[assoc, {key} -> value];
ReplaceKey[assoc_Association, {key_, keys___} -> value_] := 
	Append[assoc, key -> ReplaceKey[assoc[key], {keys} -> value]];


(* ::Section:: *)
(*AssociationSameQ*)


AssociationSameQ[assoc1_Association, assoc2_Association] := Module[
	{
		keylist
	},
	
	keylist = Keys[assoc1];
	If[!ContainsExactly[keylist][Keys[assoc2]],
		Return[False]
	];
	
	MapThread[If[AssociationQ[#1] && AssociationQ[#2],
		AssociationSameQ[#1, #2],
		SameQ[#1, #2]
	]&, {
		assoc1 /@ keylist,
		assoc2 /@ keylist
	}] // ContainsOnly[{True}]
	
];


(* ::Section:: *)
(*TypeUsage*)


TypeUsage[typename_Symbol, usage_String] := (
    Evaluate[typename]::usage = StringJoin[{
        Evaluate[typename]::usage
        // Replace[{
            _MessageName -> "",
            s_String :> StringDrop[s, -1]
        }],
        " ",
        usage
    }]
);


EndPackage[];
