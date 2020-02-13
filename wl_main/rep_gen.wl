

resols = { {}, { b -> 1337 }, { a2 -> 228 }, { omega -> a1 + C0 } };

tmpResols = Join@@resols;

texForm = TeXForm[TableForm[tmpResols]];

fMakeTexSols[NN_] := Module[ { tmpSS, strSS },

	tmpSS = Join@@ss;
	texSS = TeXForm[TableForm[tmpSS]];
	strSS = "$$\n" <> ToString[texSS] <> "\n$$\n";

	strSS

];

fRepHead0[NN_] := Module[ { RepHead0, u, x, t, k, omega, z, A, R, chi, C0 },

	RepHead0 = { 

		"N = " <> ToString[NN],
		u[x, t] -> y[z] * Exp[ I * (k * x - omega * t) ],
		z -> x - C0 * t,
		y[z] -> A * R[z]^NN,
		D[R[z], {z, 1}]^2 == R[z]^2 * ( 1 - chi * R[z]^2 )
	

	};

	RepHead0

];

fGetDocHeader[] := Module[ { docHeader },

	docHeader = "
\\documentclass[12pt,a4paper,draft]{article}
\\newcommand\\tab[1][1cm]{\\hspace*{#1}}
\\usepackage[utf8]{inputenc}
%\\usepackage[russian]{babel}
\\usepackage[OT1]{fontenc}
\\usepackage{cmap}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{amssymb}
%\\usepackage{txfonts}
%\\usepackage{mathptmx}
\\usepackage{setspace}
\\usepackage[left=2cm,right=2cm,top=2cm,bottom=2cm]{geometry}
\\usepackage{graphicx}
\\DeclareGraphicsExtensions{.pdf,.png,.jpg}
\\graphicspath{{imgs/}}
\\linespread{1.5}
\\onehalfspacing
\\begin{document}
";

	docHeader

];

fGetDocTail[] := Module[ { docTail },

	docTail = "\\end{document}";

	docTail

];

fRepEq[NN_] := Module[ { req, coeffliststring, strexp, coefflist, eqTail,
			qt, qlistRep, eqHeadList, eqHead, eq  }, 


	req = <| |>;

	coeffliststring = Map[(StringTemplate["a``"][#1])&, Range[1, 2 * NN]];
	strexp = "{" <> StringRiffle[coeffliststring, ","] <> "}";
	coefflist = ToExpression[strexp];

	eqTail = -b * Abs[u[x, t]]^2 * u[x, t];

	qt = D @@ { u[x, t], t };

	qlistRep = Map[( D @@ { u[x, t], {x, #1} } )&, Range[1, 2 * NN]];

	eqHeadList = Map[(qlistRep[[#1]] * (I)^Mod[#1, 2] * coefflist[[#1]] * (-1)^Mod[#1, 2])&, Range[1, 2 * NN]];
	eqHeadList = Join[eqHeadList, { I * qt }];
	eqHead = Total[eqHeadList];

	eq = eqHead + eqTail;

	req["coefflist"] = coefflist;
	req["qlist"] = qlistRep;
	req["eq tail"] = eqTail;
	req["eq head list"] = eqHeadList;
	req["eq head"] = eqHead;
	req["eq"] = eq;

	req

];

fMakeTex[rd_] := Module[ {texData},

	texData = "$$\n" <> rd <> "\n$$\n";

	texData

];

fRepTest[mainRes_] := Module[ {NN, repFileName, rePart, imPart,
				resols, imsols, imSolsStr, reSolsStr,
				reStr, imStr, req, repHead0, eqStr,
				srep, repData, docHeader, docTail},

	NN = 2;
	NN = mainRes["N"];

	repFileName = "report.txt";
	repFileName = "reports/report.tex";
	repFileName = ToString[mainRes["report file name"]];


	rePart = mainRes["re"];
	imPart = mainRes["im"];
	resols = mainRes["resols"];
	imsols = mainRes["imsols"];

	imSolsStr = fMakeTexSols[imsols];
	reSolsStr = fMakeTexSols[resols];

	reStr = ToString[TeXForm[rePart == 0]];
	reStr = fMakeTex[reStr];
	imStr = ToString[TeXForm[imPart == 0]];
	imStr = fMakeTex[imStr];


	req = fRepEq[NN];
	repHead0 = fRepHead0[NN];	

	eqStr = ToString[TeXForm[req["eq"] == 0]];
	eqStr = fMakeTex[eqStr];

	srep = OpenWrite[repFileName];

	repData = TeXForm[TableForm[repHead0]];

	repData = ToString[repData];
	repData = fMakeTex[repData];

	docHeader = fGetDocHeader[];
	docTail = fGetDocTail[];

	WriteString[srep, docHeader];

	WriteString[srep, "Target equation:\n"];
	WriteString[srep, eqStr];
	WriteString[srep, "Substitutions:\n"];
	WriteString[srep, repData];
	
	WriteString[srep, "Imaginary part of equation after substitutions:\n"];
	WriteString[srep, imStr];
	WriteString[srep, "Real part of equation after substitutions:\n"];
	WriteString[srep, reStr];

	WriteString[srep, "Constraints on coefficients from imaginary part of equation:\n"];
	WriteString[srep, imSolsStr];
	WriteString[srep, "Constraints on coefficients from real part of equation:\n"];
	WriteString[srep, reSolsStr];


	WriteString[srep, docTail];

	Close[srep];

];
