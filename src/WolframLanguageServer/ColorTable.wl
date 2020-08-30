(* ::Package:: *)

(* Copyright 2019 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Color Table *)


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
