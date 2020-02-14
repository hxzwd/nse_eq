
Get["rep_gen.wl"];

fCheckEq[NN_, imsols_, resols_] := Module[ {
	RR, coefflist, eq, EQ0, EQ1, EQ2,
	TERMS, SIMP0, SIMP1, SIMP2, SIMP3, CHRES},
	Clear[u];
	Clear[y];
	Clear[R];

	RR = fRepEq[NN];
	coefflist = RR["coefflist"];
	eq = RR["eq"];
	u[x_, t_] := y[x + C0 * t] * Exp[I * (k * x - omega * t)];
	EQ0 = Simplify[eq, Element[x, Reals] && Element[omega, Reals] && Element[k, Reals] && Element[x, Reeals] && Element[t, Reals] ];
	EQ1 = Simplify[Collect[EQ0, Exp[I * (k * x - omega * t)]]]/.{ Exp[I * (k * x - omega * t)] -> 1 };
	EQ2 = Simplify[ComplexExpand[EQ1], 
		Element[x, Reals] && 
		Element[omega, Reals] && 
		Element[k, Reals] && 
		Element[x, Reals] && 
		Element[t, Reals] && 
		(*
		Element[a1, Reals] && 
		Element[a2, Reals] && 
		Element[a3, Reals] && 
		Element[a4, Reals] && 
		*)
		Fold[And, Map[(Element[#1, Reals])&, coefflist]] &&
		Element[k, Reals] && 
		Element[C0, Reals] && 
		Element[b, Reals] ];

	TERMS = Map[(Part[EQ2, #1])&, Range[1, Length[EQ2]]];

	SIMP0 = Simplify[Simplify[Simplify[EQ2/.{x + C0 * t -> z}/.Join@@imsols]]/.Join@@resols];

	
	y[z_] := A * R[z]^NN;

	SIMP1 = Simplify[SIMP0/.{ R'[z]^2 -> R[z]^2 * (1 - chi * R[z]^2)}];
	SIMP2 = Simplify[SIMP1/.{ R''[z] -> R[z] - 2 * chi * R[z]^3}];

	R[z_] := 4 * a / ( 4 * a^2 * Exp[z] + chi * Exp[-z]);

	SIMP3 = Simplify[SIMP2];

	CHRES = <| "res" -> SIMP3, "R" -> R[z], "EQ0" -> EQ0, "EQ1" -> EQ1, "EQ2" -> EQ2, "SIMP0" -> SIMP0, "SIMP1" -> SIMP1, "y" -> y[z], "u" -> u[x, t] |>;

	CHRES


];


