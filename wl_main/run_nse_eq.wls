#!/usr/bin/env wolframscript

Run["echo this is run_nse_eq.wls && echo && echo"];
Print["Script command line args:\n"];
Print[$ScriptCommandLine];



Get["nse_eq.wl"];

(*
NN = 4;
repFileName = "report.tex";
*)

NN = ToExpression[$ScriptCommandLine[[2]]];
repFileName = ToString[$ScriptCommandLine[[3]]];

RES = FRUN[NN, repFileName];

Run["./make_report.sh " <> repFileName];





