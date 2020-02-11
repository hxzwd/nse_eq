

resols = { {}, { b -> 1337 }, { a2 -> 228 }, { omega -> a1 + C0 } };

tmpResols = Join@@resols;

texForm = TeXForm[TableForm[tmpResols]];



fRepHead0 := Function[ { NN },

	RepHead0 = { 

		u[x, t] -> y[z] * Exp[ I * (k * x - omega * t) ],
		z -> x - C0 * t,
		y[z] -> A * R[z]^NN,
		D[R[z], {z, 1}]^2 == R[z]^2 * ( 1 - chi * R[z]^2 )
	

	};

	RepHead0

];

fRepEq := Function[ { NN }, 


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
