(* ::Package:: *)
(*Archives*)

(* Wolfram Language Server Archived Functions *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Archives`"]
ClearAll[Evaluate[Context[] <> "*"]]


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


(* ::Section:: *)
(*GenSvgImg*)


Options[GenSvgImg] := {
	"Theme" -> "dark",
	"TempDir" -> $TemporaryDirectory
}


GenSvgImg[token_String, width_Integer, o:OptionsPattern[]] := Module[
	{
		tempImgPath, background, theme, tempDir
	},
	
	{theme, tempImgPath} = OptionValue[GenSvgImg, {o}, {"Theme", "TempDir"}];
	background = If[theme === "light", Black, White];
	tempImgPath = FileNameJoin[{tempDir, CreateUUID[] <> ".svg"}];
	(* Export[tempImgPath, Style[#, background]& @* (#::usage&) @ Symbol[token]]; *)
	Export[tempImgPath, 
		Style[
			Pane[StringReplace[#, StartOfLine -> "\[FilledSmallCircle] "], .85*width, Alignment -> Left], FontSize -> 13, background
		]& @ ToExpression[token <> "::usage"]
	];
	(* "![" <> "test" <> "](" <> tempImgPath <> ")" <> "\n" <> "```" <> StringRepeat[StringJoin[CharacterRange["a", "z"]], 4] <> "```" *)
	"![" <> "test" <> "](" <> tempImgPath <> ")" <> "\n\n" <> "```typescript" <> StringRepeat[StringRepeat["\t", 50] <> "\n", 20] <> "```" <> "\n\n"
	(* "![" <> ToString[(#::usage&) @ Symbol[token]] <> "](" <> tempImgPath <> ")" *)
] 


End[]


EndPackage[]