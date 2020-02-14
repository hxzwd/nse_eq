#!/usr/bin/env wolframscript

Run["echo this is run_nse_eq.wls && echo && echo"];
Print["Script command line args:\n"];
Print[$ScriptCommandLine];

Get["nse_eq.wl"];

NN = 4;
repFileName = "report.tex";

RES = FRUN[NN, repFileName];

Run["./make_report.sh"];





