
fGetEq[N_] := {

	sube = y[z] * Exp[I * (k * x - omega * t)];
	subez = x - C0 * t;
	
	sube = sube/.{ z -> subez };

	eq = -b * Abs[q[x, t]]^2 * q[x, t];
	qt = D[q[x, t], t];

	eqSuffix = I * qt + I * a1 * D[q[x, t], x];
	eqSuffix += a2 * D[q[x, t], {x, 2}] + I * a3 * D[q[x, t], {x, 3}];
	eqSuffix += a4 * D[q[x, t], {x, 4}];

	eq += eqSuffix;

	N;
	eq
};




