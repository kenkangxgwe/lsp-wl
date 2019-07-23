# Matypetica

An attempt to add type system to Wolfram Language.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Matypetica](#matypetica)
    - [Design Patterns](#design-patterns)
        - [Head-based](#head-based)
            - [Type Definition](#type-definition)
            - [Type Instance](#type-instance)
            - [Type Check](#type-check)
        - [PatternTest-based (TBD)](#patterntest-based-tbd)
        - [Association-based (TBD)](#association-based-tbd)
        - [UpValue-based (TBD)](#upvalue-based-tbd)
    - [RoadMap](#roadmap)

<!-- markdown-toc end -->

## Design Patterns

There are several possible design patterns to introduce type system into Wolfram
Language. The current code only provides the head-based solution based on the
type system implemented in [WolframLanguageServer](https://github.com/kenkangxgwe/lsp-wl).

### Head-based

An variable `a` with type `A` is represented as `a = A[data]` where type is the
head of its instance. This is of course trivial for intrinsic types that you can
find a Head in Wolfram Language, like `1` whose head is `Integer` and `"abc"`
whose head is `String`.

Thus, the head-based type imitates this pattern and provides the following features.

#### Type Definition

```Mathematica
DeclareType[Student, <|
    "id" -> _?NumberQ,
    "name" -> _String,
    "sex" -> "Male"|"Female",
    "courses"-> Association[(_Integer->_String)...]
|>];
```

This declares the type `Student` and its fields `"id"`, `"name"`, etc. Notice
that the names and patterns of the fields are passed in as an Association. This
ensures the completeness of of the type system.

#### Type Instance

To define an instance of a type, we may simply give an association wrapped with a
head.

```Mathematica
stu1 = Student[<|
    "id" -> 1,
    "name" -> "John Doe",
    "sex" -> "Male",
    "courses" -> <||>
|>]
```

Notice that, currently, we do not have a static type checking system, so the
association you give to the type may contain any key-value pair other than the fields
declared in the type definition (or even not an association at all).

```Mathematica
stuWithWrongField = Student[<|"id" -> "invalid", "salary" -> 0|>]
stuWithoutAssoc = Student[1, 2, 3]
```

The first one will not be complained unless you try to access the value of the fields
from the origin declaration, i.e., current implementation only gives a lazy type
check mechanism. The second one will stay just as it is.

For the robustness, we may also define some constructors to avoid passing the
association manully without checking.

```Mathematica
Student[id_Integer, name_String, sex:"Male"|"Female"] := 
    Student[<|"id" -> id, "name" -> name, "sex" -> sex, "courses" -> <||>|>]

Student[___] := Null (* This may stands for an invalid instance *)
```

So both cases listed above will return `Null`.

To access the field we pass the field name as the [down value](https://reference.wolfram.com/language/ref/DownValues.html) of the variable.

```
stu1["id"] (* == 1 *)
stu1["sex"] (* === "Male" *)
```

Although multiple paradigms are supported in Wolfram Language, we prefer to make
our data immutable, by this meaning, we need to construct a new instance in
order to change the value in the field. We may simply achieve this by using
`ReplaceKey`.

```Mathematica
(* give stu1 some courses to attend *)
stu1 = ReplaceKey[stu1, "courses" -> <|1-> "ECON101", 2->"COMP102", 3->"PHYS201"|>]

(* create a female version of stu1 while keep stu1 unchanged *)
stu2 = ReplaceKey[stu1, {
    "name" -> "Jane Doe",
    "sex" -> "Female"
}|>]
```

#### Type Check

As we mentioned above, a lazy type checking is performed when we try to access
the data in the field. If a variable of Type `A` does not contain the field or
the value of the field does not match the pattern given when we define the type,
a `Missing` will be returned.

```Mathematica
stuWithWrongField = Student[<|"id" -> "invalid", "salary" -> 0|>]

stuWithWrongField["id"] (* === Missing["PatternMismatch", {"id", _Integer}] *)
stuWithWrongField["salary"] (* === Missing["KeyAbsent", "salary"] *)
```

### PatternTest-based (TBD)

### Association-based (TBD)

### UpValue-based (TBD)

## RoadMap

- [ ] Implement basic framework for type definition and checking.
- [ ] Introduce subtypes and interfaces.
- [ ] Static type checking and code generation.

...
