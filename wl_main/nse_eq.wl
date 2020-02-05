

var["f get y subs"] = Function[ { NN_, genlimit_ },

	ysubs = A * R[z]^NN;

	var["rzsq subs"] = { D[R[z], z]^2 -> R[z]^2 * ( 1 - chi * R[z]^2 ) };
	var["rzz subs"] = { D[R[z], { z, 2 }] -> R[z] - 2 * chi * R[z]^3 };
	var["misc subs"] = { var["rzsq subs"], var["rzz subs"] };

	ysubslist = { { y[z] -> ysubs } };

	For[i = 1, i <= genlimit, i++, ysubslist = Append[ysubslist,
	{
	D[y[z], {z, i}] ->
	Expand[
	(D[ysubslist[[i]][[1]][[-1]], z]/.(Join@@ysubslist)/.(Join@@var["misc subs"]))
	]
	}
	]
	];

	yfinalsubs = Join@@(Join[ysubslist, var["misc subs"]]);

	yfinalsubs

];

