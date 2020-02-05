
sube = y[z] * Exp[I * (k * x - omega * t)];
subez = x + C0 * t;
ecoeff = k * x - omega * t;

sube = sube/.{ z -> subez };

eq = -b * Abs[q[x, t]]^2 * q[x, t];

eq = eq/.{ q[x, t] -> sube };

qf = q[x, t]/.{ q[x, t] -> sube };

qt = D @@ { qf, t };
qx = D @@ { qf, x };
qxx = D @@ { qf, { x, 2 } };
qxxx = D @@ { qf, { x, 3 } };
qxxxx = D @@ { qf, { x, 4 } };

eqSuffix = I * qt - I * a1 * qx;
eqSuffix += a2 * qxx - I * a3 * qxxx;
eqSuffix += a4 * qxxxx;

coefflist = { a1, a2, a3, a4 };
c0var = C0;

eq += eqSuffix;

teq0 = eq/.{ Im[ ecoeff ] -> 0 };
teq1 = teq0/.{ Abs[ y[subez] ] -> y[subez] };
teq2 = teq1/.{ Exp[ I * ( ecoeff ) ] -> 1 };
teq3 = Simplify[teq2]/.{ subez -> z };

var["teq3 complex expanded"] = ComplexExpand[teq3];

var["re teq3"] = Simplify[var["teq3 complex expanded"]/.{ I -> 0 }];
var["im teq3"] = Simplify[var["teq3 complex expanded"] - var["re teq3"]];
var["im teq3"] = Simplify[(ComplexExpand[var["im teq3"]]/.{ I -> 1 }) * -1];

var["im poly"] = var["im teq3"]/.{ D[y[z], { z, p_ }] -> Z^p };
var["im exp"] = Exponent[var["im poly"], Z, List];
var["im coeffs"] = Cases[CoefficientList[var["im poly"], Z], Except[0]];

var["im targets"] = Map[(coefflist[[#1]])&, var["im exp"]]/.{ coefflist[[1]] -> c0var };

imsols = { {} };
imc = Reverse[var["im coeffs"]];
imt = Reverse[var["im targets"]];

fims = Function[{i}, Solve[(imc[[i]]/.(Join@@imsols)) == 0, imt[[i]]][[1]]];
(*
For[i = 1, i <= Length[imt], i++, imsols = Append[imsols, Solve[(imc[[i]]/.(Join@@imsols)) == 0, imt[[i]]][[1]]]];
*)
For[i = 1, i <= Length[imt], i++, imsols = Append[imsols, fims[i]]];

reall = Simplify[var["re teq3"]/.(Join@@imsols)];

NN = 2;
ysub = A * R^NN;
ysublist = {};

Get["gen_y_subs.wl"];

realls = Simplify[reall/.yfinalsubs];
realls = Simplify[realls/.Join@@imsols];

var["re coeffs"] = Cases[CoefficientList[realls, R[z]], Except[0]];
rec = Reverse[var["re coeffs"]];
