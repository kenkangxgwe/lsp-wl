(* ::Package:: *)

(* Matypetica *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com> *)


BeginPackage["DataType`"]
ClearAll[Evaluate[Context[] <> "*"]]


DeclareType::usage = "DeclareType[typename, <|key_String -> pattern...|>] declares a type given by its name and an association indicating every field and its pattern in the type. \
typename[key] gets access to the corresponding value. \
typename[key, default] gets access to the corresponding value. If it is missing, returns the default."
ConstructType::usage = "ConstructType[params_Association, type] constructs an object of the given type with specified params."
ToAssociation::usage = "ToAssociation[obj_typename] gives the association form of the object."
TypeUsage::usage = "TypeUsage[type, usage_String] append usage to current type."
Keys::usage = (Keys::usage // Replace[_MessageName -> ""]) <> "\nKeys[typename] gives a list of the keys field_i in type typename."
KeyPatterns::usage = "KeyPatterns[typename] returns the key-pattern pair of the type."
ReplaceKey::usage = "ReplaceKey[object, key -> value] assigns the value to key in given object.
ReplaceKey[object, {key1, key2} -> value] assigns the value to object[key1][key2].
ReplaceKey[replaceRule_Rule] is an operator that can be applied to an object."
ReplaceKeyBy::usage = "ReplaceKeyBy[object, key -> func] replaces the value at object[key] by applying a function on it.
ReplaceKeyBy[object, {key1, key2} -> func] replaces the value at object[key1][key2].
ReplaceKeyBy[replaceRule_Rule] is an operator that can be applied to an object."
DeleteKey::usage = "DeleteKey[object, key] deletes the key-value pair from object[key].
DeleteKey[object, {key1, key2}] deletes the key-value pair at object[key1][key2].
DeleteKey[keys] is an operator that can be applied to an object."
TypeCheck::usage = "TypeCheck[toggle_?BooleanQ] turns on/off the type checking."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


(* ::Section:: *)
(*DeclareType*)


DeclareType[typename_Symbol, typekey:<|(_String -> _)...|>] := (
	(* Getter *)
	typename[typedict_Association][key_String] := (
        If[$typeCheckQ,
            Which[
                MissingQ[typedict[key]],
                    Null,
                !KeyMemberQ[typekey, key],
                    Message[TypeCheck::miskey, typename, key],
                !MatchQ[typedict[key], typekey[key]],
                    Message[TypeCheck::mispat, typename, key, typedict[key]]
            ]
        ];
        typedict[key]
    );
	typename[typedict_Association][key_String, default_] := With[
		{value = typename[typedict][key]},
		If[MissingQ[value], typename[<|key -> default|>][key], value]
	];

    (* Deserializer*)
    ConstructType[parameters_Association, typename] := (
        typename[Association[
            (# -> ConstructType[parameters[#], typekey[#]]&)
            /@ Intersection[Keys[typekey], Keys[parameters]]
        ]]
    );

    (* Serializer*)
    ToAssociation[typename[typedict_Association]] := ToAssociation /@ typedict;

    (* Keys and Patterns *)
	Keys[typename] ^= Keys[typekey];
	KeyPatterns[typename] = typekey;
    typename /: Key[key_String][typename[typedict_Association]] := typename[typedict][key];

	(* ReplaceKey *)
	ReplaceKey[typename[typedict_Association], rule:((Rule|RuleDelayed)[(key_String|{key_String}), _])] := (
		If[MemberQ[Keys[typename], key],
            typename[ReplaceKey[typedict, rule]],
            typename[typedict]
        ]
    );

	ReplaceKey[typename[typedict_Association], rule:((Rule|RuleDelayed)[{key_String, __}, _])] := (
		If[MemberQ[Keys[typedict], key],
            typename[ReplaceKey[typedict, rule]],
            typename[typedict]
        ]
    );

    (* ReplaceKeyBy*)
	ReplaceKeyBy[typename[typedict_Association], rule:((Rule|RuleDelayed)[(key_String|{key_String}), _])] := (
		If[MemberQ[Keys[typename], key],
            typename[ReplaceKeyBy[typedict, rule]],
            typename[typedict]
        ]
    );
	ReplaceKeyBy[typename[typedict_Association], rule:((Rule|RuleDelayed)[{key_String, __}, _])] := (
		If[MemberQ[Keys[typedict], key],
            typename[ReplaceKeyBy[typedict, rule]],
            typename[typedict]
        ]
    );

    (* DeleteKey *)
	DeleteKey[typename[typedict_Association], key_String|{key_String}] := (
        typename[DeleteKey[typedict, key]]
    );

	DeleteKey[typename[typedict_Association], keys:{key_String, __}] := (
		If[MemberQ[Keys[typedict], key],
            typename[DeleteKey[typedict, keys]],
            typename[typedict]
        ]
    );


	(* SameQ *)
	typename /: SameQ[typename[typedict1_Association], typename[typedict2_Association]] := (
		AssociationSameQ[typedict1, typedict1]
	);

	(* usage *)
	typename::usage = StringJoin[{
	    ToString[typename, InputForm],
	    "[<|",
	    Riffle[KeyValueMap[{#1, " -> ", ToString[#2]}&, typekey], ", "],
	    "|>]",
	    Replace[typename::usage, _MessageName -> "."]
	}];
)


(* ::Section:: *)
(*Default Constructor*)


ConstructType[parameters_, pattern_] := If[MatchQ[parameters, pattern], parameters,
    Replace[pattern, {
        Verbatim[Pattern][_, obj_] :> ConstructType[parameters, obj],
        Verbatim[Blank][h_] :> ConstructType[parameters, h],
        _ :> Missing["ConstructorNotFound", {parameters, pattern}]
    }]
]

ConstructType[parameters_, pattern:Verbatim[Alternatives][ps__]] := (
    ConstructTypeAlternatives[parameters, {{ps}, Missing["ConstructorNotFound", {parameters, pattern}]}]
)

ConstructTypeAlternatives[parameters_, {{}, res_}] := res
ConstructTypeAlternatives[parameters_, {{p_, ps___}, res_}] := ConstructTypeAlternatives[
    parameters,
    ConstructType[parameters, p]
    // Replace[{
        _Missing :> {{ps}, res},
        newres_ :> {{}, newres}
    }]
]

ConstructType[parameters_List, pattern:List[(Verbatim[Repeated]|Verbatim[RepeatedNull])[p_]]] := (
    ConstructTypeList[p, {parameters, {}}] // Replace[_Missing :> Missing["ConstructorNotFound", {parameters, pattern}]]
)

ConstructType[parameters_List, pattern:List[(Verbatim[BlankSequence]|Verbatim[BlankNullSequence])[p_]]] := (
    ConstructTypeList[_p, {parameters, {}}] // Replace[_Missing :> Missing["ConstructorNotFound", {parameters, pattern}]]
)

ConstructTypeList[p_, {{}, res_}] := res
ConstructTypeList[p_, {{param_, params___}, res_}] := ConstructTypeList[p,
    ConstructType[param, p]
    // Replace[{
        _Missing :>  {{}, MissingQ["ConstructorNotFound"]},
        curRes_ :> {{params}, Append[res, curRes]}
    }]
]

ConstructType[parameters_Association, pattern:Association[(Verbatim[Repeated]|Verbatim[RepeatedNull])[Rule[key_, val_]]]] := (
    ConstructType[Keys[parameters], {key...}]
    // Replace[{
        _Missing :> Missing["ConstructorNotFound", {parameters, pattern}],
        res1_ :> (
            ConstructType[Values[parameters], {val...}]
            // Replace[{
                _Missing -> Missing["ConstructorNotFound", {parameters, pattern}],
                res2_ :> Association[Thread[res1 -> res2]]
            }]
        )
    }]
)


(* ::Section:: *)
(*TypeCheck*)


TypeCheck::miskey = "`1` doesn't contains key `2`."
TypeCheck::mispat = "`3` doesn't match the pattern of \"`2`\" in `1`."

TypeCheck[toggle_?BooleanQ] := ($typeCheckQ = toggle)
TypeCheck[True]


(* ::Section:: *)
(*ToAssociation*)


ToAssociation[primitive_:Null] := primitive
ToAssociation[list_List] := ToAssociation /@ list
ToAssociation[assoc_Association] := ToAssociation /@ assoc


(* ::Section:: *)
(*ReplaceKey*)


ReplaceKey[obj_, {} -> _] := obj
ReplaceKey[rule:(_Rule|_RuleDelayed)][obj_] := ReplaceKey[obj, rule]

ReplaceKey[list_List, key_Integer|{key_Integer} -> value_] := (
    If[0 < Abs[key] <= Length[list],
        ReplacePart[list, key -> value],
        list
    ]
)

ReplaceKey[list_List, {key_Integer, keys__} -> value_] := (
    If[0 < Abs[key] <= Length[list],
        ReplacePart[list, key -> ReplaceKey[Extract[key][list], {keys} -> value]],
        list
    ]
)

ReplaceKey[assoc_Association, (rulehd:(Rule|RuleDelayed))[((key:Except[_List])|{key_}), value_]] :=
	Append[assoc, rulehd[key, value]]
ReplaceKey[assoc_Association, (rulehd:(Rule|RuleDelayed))[{key_, keys__}, value_]] :=
    If[KeyMemberQ[assoc, key],
        Append[assoc, key -> ReplaceKey[assoc[key], rulehd[{keys}, value]]],
        assoc
    ]


(* ::Section:: *)
(*ReplaceKeyBy*)


ReplaceKeyBy[obj_, {} -> _] := obj
ReplaceKeyBy[rule:(_Rule|_RuleDelayed)][obj_] := ReplaceKeyBy[obj, rule]

ReplaceKeyBy[list_List, key_Integer|{key_Integer} -> func_] := (
    If[0 < Abs[key] <= Length[list],
        ReplacePart[list, key -> func[Part[list, key]]],
        list
    ]
)

ReplaceKeyBy[list_List, {key_Integer, keys__} -> func_] := (
    If[0 < Abs[key] <= Length[list],
        ReplacePart[list, key -> ReplaceKeyBy[Extract[key][list], {keys} -> func]],
        list
    ]
)

ReplaceKeyBy[assoc_Association, (rulehd:(Rule|RuleDelayed))[((key:Except[_List])|{key_}), func_]] :=
	Append[assoc, rulehd[key, func[assoc[key]]]]
ReplaceKeyBy[assoc_Association, (rulehd:(Rule|RuleDelayed))[{key_, keys__}, func_]] :=
    If[KeyMemberQ[assoc, key],
        Append[assoc, key -> ReplaceKeyBy[assoc[key], rulehd[{keys}, func]]],
        assoc
    ]


(* DeleteKey *)

DeleteKey[obj_, {}] := obj
DeleteKey[keys_][obj_] := DeleteKey[obj, keys]

DeleteKey[list_List, key_Integer|{key_Integer}] := (
    If[0 < Abs[key] <= Length[list],
        Delete[list, key],
        list
    ]
)

DeleteKey[list_List, {key_Integer, keys__}] := (
    If[0 < Abs[key] <= Length[list],
        ReplacePart[list, key -> DeleteKey[Extract[key][list], {keys}]],
        list
    ]
)

DeleteKey[assoc_Association, (key:Except[_List])|{key_}] :=
	KeyDrop[assoc, key]
DeleteKey[assoc_Association, {key_, keys__}] :=
    If[KeyMemberQ[assoc, key],
        Append[assoc, key -> DeleteKey[assoc[key], {keys}]],
        assoc
    ]


(* ::Section:: *)
(*AssociationSameQ*)


AssociationSameQ[assoc1_Association, assoc2_Association] := With[
	{
        keylist = Keys[assoc1]
	},

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

]


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
)


End[]


Begin["`Test`"]


AssociationSameQ::usage = "AssociationSameQ[assoc1_Association, assoc2_Association] returns True if two associations are same except for key order."
AssociationSameQ = DataType`Private`AssociationSameQ


End[]


EndPackage[]
