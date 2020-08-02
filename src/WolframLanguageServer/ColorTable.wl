(* ::Package:: *)

(* Wolfram Language Server Color Table *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>,
           huxianglong <hxianglong_at_gmail.com>
*)

BeginPackage["WolframLanguageServer`ColorTable`"]
ClearAll[Evaluate[Context[] <> "*"]]

WolframLanguageServer`ColorTable`Colorspace = {
    "RGB", "CMYK", "HSB", "XYZ", "LAB", "LCH", "LUV", "GrayScale"
}


WolframLanguageServer`ColorTable`ColorName = {
    "Red", "Green", "Blue", "Black", "White", "Gray", "Cyan", "Magenta",
    "Yellow", "Brown", "Orange", "Pink", "Purple", "LightRed", "LightGreen",
    "LightBlue", "LightGray", "LightCyan", "LightMagenta", "LightYellow",
    "LightBrown", "LightOrange", "LightPink", "LightPurple", "Transparent"
}


WolframLanguageServer`ColorTable`NamedColor = (
    WolframLanguageServer`ColorTable`ColorName
    // Map[ToExpression]
)


EndPackage[]
