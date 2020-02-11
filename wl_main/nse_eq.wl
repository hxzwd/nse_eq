


fInit := Function[ {NN}, 

	q = <| |>;

	sube = y[z] * Exp[I * (k * x - omega * t)]; q["sube"] = sube;
	subez = x + C0 * t; q["subez"] = subez;
	ecoeff = k * x - omega * t; q["ecoeff"] = ecoeff;
	sube = sube/.{ z -> subez }; q["sube with subez"] = sube;
	(*coefflist = { a1, a2, a3, a4 };*)
	c0var = C0; q["c0var"] = C0;

	coeffliststring = Map[(StringTemplate["a``"][#1])&, Range[1, 2 * NN]];
	strexp = "{" <> StringRiffle[coeffliststring, ","] <> "}";
	coefflist0 = ToExpression[strexp];

	q["coefflist0"] = coefflist0;
	q["coefflist"] = coefflist0;

	q

];



fGetYSubs := Function[ { NN, genlimit },

	ysubs = A * R[z]^NN;

	rzsqSubs = { D[R[z], z]^2 -> R[z]^2 * ( 1 - chi * R[z]^2 ) };

	rzzSubs= { D[R[z], { z, 2 }] -> R[z] - 2 * chi * R[z]^3 };
	miscSubs = { rzsqSubs, rzzSubs };

	ysubslist = { { y[z] -> ysubs } };

	For[i = 1, i <= genlimit, i++, ysubslist = Append[ysubslist,
		{
			D[y[z], {z, i}] ->
			Expand
				[
					(D[ysubslist[[i]][[1]][[-1]], z]/.(Join@@ysubslist)/.(Join@@miscSubs))
				]
		}]
	];

	yfinalsubs = Join@@(Join[ysubslist, miscSubs]);


	yfinalsubs

];

fGetEq := Function[ {NN, misc},

	sube = misc["sube with subez"];
	ecoeff = misc["ecoeff"];
	subez = misc["subez"];
	coefflist = misc["coefflist"];
	c0var = misc["c0var"];


	resEq = <| |>;


	eq = -b * Abs[q[x, t]]^2 * q[x, t];

	resEq["eq tail"] = eq;

	eq = eq/.{ q[x, t] -> sube };
	eqr = eq;
	resEq["eqr"] = eqr;

	qf = q[x, t]/.{ q[x, t] -> sube };
	qt = D @@ { qf, t };

	qlist = Map[( D @@ { qf, {x, #1} } )&, Range[1, 2 * NN]];

	resEq["qf"] = qf;
	resEq["qlist"] = qlist;

	eqSuffix0 = Map[(qlist[[#1]] * (I)^Mod[#1, 2] * coefflist[[#1]] * (-1)^Mod[#1, 2])&, Range[1, 2 * NN]];
	resEq["eqSuffix0"] = eqSuffix0;
	resEq["eqSuffix"] = eqSuffix0;


	firstSubSeq = {
				Im[ ecoeff ] -> 0, 
				Abs[ y[subez] ] -> y[subez],
				Exp[ I * ( ecoeff ) ] -> 1,
				subez -> z
				};

	resEq["firstSubSeq"] = firstSubSeq;

	eq0 = Total[eqSuffix0] + I * qt + eqr;
	eq0 = Simplify[eq0]/.{ Im[ omega * t - k * x ] -> 0 };
	eq0 = Simplify[eq0]/.{ Exp[ I * ( - omega * t + k * x ) ] -> 1 };
	eq0 = Simplify[eq0]/.{ Exp[ I * (  omega * t - k * x ) ] -> 1 };
	eq0 = Simplify[eq0]/.{ Abs[ y[subez] ] -> y[subez] };
	eq0 = Simplify[eq0/.{ subez -> z }];

	resEq["eq"] = eq0;
	resEq["eq0"] = eq0;

	resEq
];

fGetReAndIm := Function[ {eq, misc},

	sube = misc["sube with subez"];
	ecoeff = misc["ecoeff"]
	subez = misc["subez"]
	coefflist = misc["coefflist"];
	c0var = misc["c0var"];


	q = <| |>;


	teq0 = eq/.{ Im[ ecoeff ] -> 0 };
	teq1 = teq0/.{ Abs[ y[subez] ] -> y[subez] };
	teq2 = teq1/.{ Exp[ I * ( ecoeff ) ] -> 1 };
	teq3 = Simplify[teq2]/.{ subez -> z };

	teq3CmplxExpanded = ComplexExpand[teq3];

	q["teq0"] = teq0;
	q["teq1"] = teq1;
	q["teq2"] = teq2;
	q["teq3"] = teq3;
	q["teq3 complex expanded"] = teq3CmplxExpanded;

	teq3Re = Simplify[teq3CmplxExpanded/.{ I -> 0 }];
	teq3Im = Simplify[teq3CmplxExpanded - teq3Re];
	teq3Im = Simplify[(ComplexExpand[teq3Im]/.{ I -> 1 }) * -1];

	q["re teq3"] = teq3Re;
	q["im teq3"] = teq3Im;

	q
];

fGetImSols := Function[ {teq3Im, misc},

	sube = misc["sube with subez"];
	ecoeff = misc["ecoeff"];
	subez = misc["subez"];
	coefflist = misc["coefflist"];
	c0var = misc["c0var"];

	q = <| |>;

	imPoly = teq3Im/.{ D[y[z], { z, p_ }] -> Z^p };
	imExp = Exponent[imPoly, Z, List];
	imCoeffs = Cases[CoefficientList[imPoly, Z], Except[0]];

	imTargets = Map[(coefflist[[#1]])&, imExp]/.{ coefflist[[1]] -> c0var };

	q["im poly"] = imPoly;
	q["im exp"] = imExp;
	q["im coeffs"] = imCoeffs;
	q["im targets"] = imTargets;

	imsols = { {} };
	imc = Reverse[imCoeffs];
	imt = Reverse[imTargets];

	For[i = 1, i <= Length[imt], i++, imsols = Append[imsols, Solve[(imc[[i]]/.(Join@@imsols)) == 0, imt[[i]]][[1]]]];

	q["imsols"] = imsols;
	q["imc"] = imc;
	q["imt"] = imt;

	q

];

fGetReCoeffs := Function[ { rePart, imsols, yfinalsubs },

	reallp = Simplify[rePart/.(Join@@imsols)];

	realls = Simplify[reallp/.yfinalsubs];
	realls = Simplify[realls/.Join@@imsols];

	rrec = Cases[CoefficientList[realls, R[z]], Except[0]];
	rec = Reverse[rrec];

	rec
];

fGetReSols := Function[ { rec, ret }, 

	resols = { {} };


	For[i = 1, i <= Length[ret], i++, resols = Append[resols, Solve[(rec[[i]]/.(Join@@resols)) == 0, ret[[i]]][[1]]]];

	resols
];

fMain := Function[ {},

	NN = 3;
	genlimit = 6;
	genlimit = NN * 2;
	ret = { b, a2, omega };

	misc = fInit[NN];
	yfinalsubs = fGetYSubs[NN, genlimit];

	resEq = fGetEq[NN, misc];

	tmp0 = fGetReAndIm[resEq["eq"], misc];

	teq3Im = tmp0["im teq3"];

	tmp1 = fGetImSols[teq3Im, misc];

	rePart = tmp0["re teq3"];
	imsols = tmp1["imsols"];

	rec = fGetReCoeffs[rePart, imsols, yfinalsubs];

	coefflist = misc["coefflist"];

	retPart = Reverse[Map[(coefflist[[#1]])&, Range[2, Length[coefflist] - 1, 2]]];
	ret = Join[{ b }, retPart, { omega }];

	resols = fGetReSols[rec, ret];
	
	resols	

];
