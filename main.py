#!/usr/bin/env ipython3
#-*- coding: utf-8 -*-


import os
import sys


import sympy as sp

from sympy import Symbol
from sympy import symbols
from sympy import Function
from sympy import diff
from sympy import I
from sympy import exp
from sympy import solve
from sympy import Eq
from sympy import Poly
from sympy import sqrt
from sympy import tanh
from sympy import coth
from sympy import fraction

from IPython import embed


def print_hello_msg():
	print("[main]: mephim/eq\n")
	pass


def f_extract_from_eq_sys(eq_sys, param, eq_num, flag_return_new_eq_sys = True):
	if isinstance(param, str):
		param = eval(param)
	param_s = solve(eq_sys[eq_num], param)[0]
	if flag_return_new_eq_sys == True:
		new_eq_sys = [ eq.subs({ param : param_s }).doit() for eq in eq_sys ]
		return param_s, new_eq_sys
	return param_s, eq_sys


def f_extract_from_eq_sys_(eq_sys, param_list, eq_num_list):
	if eq_num_list == []:
		eq_num_list = list(range(0, len(eq_sys)))
	param_res_d = { }
	new_eq_sys = eq_sys
	for index, param in enumerate(param_list):
		param_s_, new_eq_sys = f_extract_from_eq_sys(new_eq_sys, param, eq_num_list[index])
		param_res_d[param] = param_s_
	return param_res_d



print_hello_msg()


def f_gen_target_eq(n):
	coeff_list = { }
	q_der_list = [ ]
	q = Function("q", nargs = 2)
	y = Function("y", nargs = 1)
	x = Symbol("x", real = True)
	t = Symbol("t", real = True)
	for i in range(1, n * 2 + 1):
		key = "a{}".format(i)
		value = eval("Symbol(\"{}\")".format(key))
		coeff_list[key] = value
	qt = diff(q(x, t), t, 1)
	q_der_list = [ qt ]
	for i in range(1, n * 2 + 1):
		q_der_list.append(diff(q(x, t), x, i))
	target_eq = qt * I
	for index, value in enumerate(q_der_list):
		if index == 0:
			continue
		key = "a{}".format(index)
		target_eq += coeff_list[key] * value * ( I if (index % 2 == 1) else 1 )
	return { "target_eq" : target_eq, "params" : coeff_list, "q" : q, "y" : y, "x" : x, "t" : t, "q_der_list" : q_der_list, "n" : n }


def f_subs_trav_wave(target_eq = None, params = { }, q = None, y = None, x = None, t = None, q_der_list = [ ], n = None):
	C0 = Symbol("C0")
	k = Symbol("k")
	omega = Symbol("omega")
	b = Symbol("b")
	z = Symbol("z")
	tmpp = [ "C0", "k", "omega", "b", "z" ]
	res_d = dict(zip(tmpp, map(eval, tmpp)))
	subs_fun = y(x - C0 * t) * exp( I * ( k * x - omega * t ) )
	res_d["subs_fun"] = subs_fun
	tmp1 = target_eq.subs({ q(x, t) : subs_fun }).doit().simplify().doit().expand().subs({ x - C0 * t : x }).doit()
	res_d["tmp1"] = tmp1
	tmp2 = tmp1.subs({ exp(I*k*x) : 1, exp(-I*omega*t) : 1}).doit()
	tmp2 = tmp2 - b * y(x)**3
	tmp2 = tmp2.subs({ x : z }).doit()
	res_d["tmp2"] = tmp2
	re_eq = tmp2.subs({ I : 0 }).doit()
	im_eq = tmp2.coeff(I)
	res_d["re_eq"] = re_eq
	res_d["im_eq"] = im_eq
	qq_re = [ i for i in re_eq.as_terms()[-1] if not i.is_Atom ]
	qq_im = [ i for i in im_eq.as_terms()[-1] if not i.is_Atom ]
	res_d["qq_re"] = qq_re
	res_d["qq_im"] = qq_im
	im_eq_coeff = [ im_eq.coeff(i) for i in qq_im ]
	im_eq_sys = [ Eq(i) for i in im_eq_coeff ]
	res_d["im_eq_coeff"] = im_eq_coeff
	res_d["im_eq_sys"] = im_eq_sys
	tt = [ [j for j in i.as_expr().as_ordered_terms()[0].atoms() if j.is_symbol ][0] for i in im_eq_coeff ]
	tt = list(reversed(tt))
	eq_num_list = list(reversed(range(0, len(im_eq_sys))))
	subs_im_d = f_extract_from_eq_sys_(im_eq_sys, tt, eq_num_list)
	res_d["tt"] = tt
	res_d["subs_im_d"] = subs_im_d
	res_d["n"] = n
	return res_d


def f_generate_y_subs(n, a, limit = 4):
	N = Symbol("N")
	A = Symbol("A")
	Xi = Symbol("Xi")
	a = Symbol("a")
	alpha = Symbol("alpha")
	C = Symbol("C")
	R = Function("R", nargs = 1)
	subs_ = { }
	res_subs_ = { }
	subs_[diff(R(z), z, 1)**2] =  R(z)**2 * (1 - Xi * R(z)**2)
	subs_[diff(R(z),z, 2)] = R(z) - 2 * Xi * R(z)**3
	y_subs = a * R(z)**n
	y_subs = A * R(z)**n
	tmp_list = [ y_subs ]
	tmp_keys_list = [ y(z) ]
	for i in range(0, limit):
		tmp_ = tmp_list[i]
		tmp_ = diff(tmp_, z, 1)
		tmp_list.append(tmp_.subs(subs_).simplify().doit())
		tmp_key = tmp_keys_list[i]
		tmp_keys_list.append(diff(tmp_key, z, 1))
	tmp_keys_list = reversed(tmp_keys_list)
	tmp_list = reversed(tmp_list)
	return dict(zip(tmp_keys_list, tmp_list))





q = Function("q", nargs = 2)
y = Function("y", nargs = 1)
x = Symbol("x", real = True)
t = Symbol("t", real = True)


a1, a2, a3, a4 = symbols("a1 a2 a3 a4")

qt = diff(q(x, t), t, 1)
qx = diff(q(x, t), x, 1)
qxx = diff(q(x, t), x, 2)
qxxx = diff(q(x, t), x, 3)
qxxxx = diff(q(x, t), x, 4)

target_eq = I * qt - I * a1 * qx + a2 * qxx - I * a3 * qxxx + a4 * qxxxx
target_eq = I * qt + I * a1 * qx + a2 * qxx + I * a3 * qxxx + a4 * qxxxx

C0 = Symbol("C0")
k = Symbol("k")
omega = Symbol("omega")
b = Symbol("b")
z = Symbol("z")

subs_fun = y(x - C0 * t) * exp( I * ( k * x - omega * t ) )

tmp1 = target_eq.subs({ q(x, t) : subs_fun }).doit().simplify().doit().expand().subs({ x - C0 * t : x }).doit()

tmp2 = tmp1.subs({ exp(I*k*x) : 1, exp(-I*omega*t) : 1}).doit()

tmp2 = tmp2 - b * y(x)**3
tmp2 = tmp2.subs({ x : z }).doit()

re_eq = tmp2.subs({ I : 0 }).doit()
im_eq = tmp2.coeff(I)


qq_re = [ i for i in re_eq.as_terms()[-1] if not i.is_Atom ]
qq_im = [ i for i in im_eq.as_terms()[-1] if not i.is_Atom ]

im_eq_coeff = [ im_eq.coeff(i) for i in qq_im ]

im_eq_sys = [ Eq(i) for i in im_eq_coeff ]


#C0_s = solve(im_eq_sys[0], C0)[0]
#a3_s = solve(im_eq_sys[1], a3)[0]
#
#subs_im_values = [ C0_s, a3_s ]
#subs_im_keys = [ "C0", "a3" ]

#subs_im_d = dict(zip(map(eval, subs_im_keys), subs_im_values))

param_list = [ a3, C0 ]
eq_num_list = [ 1, 0 ]

subs_im_d = f_extract_from_eq_sys_(im_eq_sys, param_list, eq_num_list)



N = Symbol("N")
A = Symbol("A")
Xi = Symbol("Xi")
a = Symbol("a")
alpha = Symbol("alpha")
C = Symbol("C")
R = Function("R", nargs = 1)



N = 2


#y_subs = A * R(z) ** N
#yz_subs = N * A * diff(R(z), z, 1) * R(z) ** (N - 1)
#yzz_subs = A * ( N**2 * R(z) ** N - N**2 * Xi * R(z) ** (N + 2) - N * Xi * R(z) ** (N + 2) )
#yzzz_subs = A * ( N**3 * R(z) ** (N - 1) - N**2 * (N + 2) * R(z) ** (N + 1) - N * (N + 2) *  R(z) ** (N + 1) ) * diff(R(z), z, 1)
#yzzzz_subs = (N * A * R(z) ** (N)) * ( ( N**3  + ( N**3 * Xi**2 + 6 * N**2 * Xi**2 + 11 * N * Xi**2 + 6 * Xi**2)) * R(z)**4 - (2 * N**3 * Xi + 6 * N**2 * Xi + 8 * N * Xi + 8 * N * Xi + 4 * Xi) * R(z)**2)
#
#subs_d_values = [ y_subs, yz_subs, yzz_subs, yzzz_subs, yzzzz_subs ]
#subs_d_keys = [ y(z) ] + [ diff(y(z), z, i) for i in range(1, 5) ]

#subs_d_keys_str = map(str, subs_d_keys)
#subs_d = dict(zip(subs_d_keys, subs_d_values))
#subs_d = dict(reversed(list(subs_d.items())))


def f_subs_R(re_eq = None, subs_im_d = { }, qq_re = [ ], n = None, **kwargs):
	res_d = { }
	nn = qq_re[-1].derivative_count
	subs_d = f_generate_y_subs(n, A, limit = nn)
	subs_d_re = dict([ [i, v] for i, v in subs_d.items() if i in qq_re ])
	subs_d_next = { diff(R(z), z, 1)**2 : R(z)**2 * (1 - Xi * R(z)**2) }
	w0 = re_eq
	w0 = re_eq.subs(subs_im_d).doit()
	for i, v in subs_d_re.items():
		w0 = w0.subs({ i : v }).expand().simplify().doit()
	w1 = w0.subs(subs_d_next).doit()
	w2 = w1.expand().simplify().doit()

	h0 = Poly(w2, R(z))

	r_coeffs = h0.coeffs()
	r_eqs = [ Eq(i) for i in r_coeffs ]
	res_d = { "nn" : nn, "subs_d" : subs_d, "subs_d_re" : subs_d_re,
		"subs_d_next" : subs_d_next, "w0" : w0, "w1" : w1, "w2" : w2,
		"h0" : h0, "r_coeffs" : r_coeffs, "r_eqs" : r_eqs }
	return res_d


subs_d = f_generate_y_subs(N, A, limit = 4)

subs_d_re = dict([ [i, v] for i, v in subs_d.items() if i in qq_re ])

subs_d_next = { diff(R(z), z, 1)**2 : R(z)**2 * (1 - Xi * R(z)**2) }



w0 = re_eq
w0 = re_eq.subs(subs_im_d).doit()

yzzzz = diff(y(z), z, 4)
yzz = diff(y(z), z, 2)

#re_eq_ = a4 * yzzzz + (a2 - 6*a4*k**2 + 3*a3*k)*yzz + (a2*k**2 + 5*a4*k**4 + omega)*y(z) - b*y(z)**3
#w0 = re_eq_.subs(subs_im_d).doit()
#w0 = re_eq_.subs({ a3 : 4 * a4 * k }).doit()

for i, v in subs_d_re.items():
	w0 = w0.subs({ i : v }).expand().simplify().doit()

#w0 = re_eq.subs(subs_d_re).doit()
w1 = w0.subs(subs_d_next).doit()
w2 = w1.expand().simplify().doit()

h0 = Poly(w2, R(z))

r_coeffs = h0.coeffs()
r_eqs = [ Eq(i) for i in r_coeffs ]





#b_s = solve(r_eqs[0], b)[0]
#a2_s = solve(r_eqs[1], a2)[0]
#omega_s = solve(r_eqs[2], omega)[0]
#subs_re_values = [ b_s, a2_s, omega_s ]
#subs_re_keys = [ "b", "a2", "omega" ]
#subs_re_d = dict(zip(map(eval, subs_re_keys), subs_re_values))


param_list = [ b, a2, omega ]
eq_num_list = [ 0, 1, 2 ]

subs_re_d = f_extract_from_eq_sys_(r_eqs, param_list, eq_num_list)



#R_subs = A * 4  * a * exp(-alpha * z) / (4 * a**2 + Xi * exp(-2 * alpha * z))
#R_subs_2 = 1/(Xi * ( 1 + C * exp(-2 * z)))
#R_subs_ = sqrt(R_subs_2)
#R_subs_ = -I * sqrt(1 - tanh(z - C)**2) * coth(z - C)/sqrt(Xi)

R_subs_ = 2 * exp(z) * C / ( C + Xi * exp(2 * z) )
R_subs_ = R_subs_.subs({ C : 1 })


all_coeff_subs_d = { }
all_coeff_subs_d.update(subs_re_d)
all_coeff_subs_d.update(subs_im_d)

target_eq_y = tmp2
target_eq_y_subs = subs_d[y(z)]

target_eq_y_ = target_eq_y.subs(all_coeff_subs_d).expand().simplify().doit()
TMP0 = target_eq_y_.subs({ y(z) : target_eq_y_subs }).doit()
TMP1 = TMP0.subs({ R(z) : R_subs_ }).simplify().factor().simplify().doit()

exact_sol = y(z) * exp( I * ( k * x - omega * t ) )
exact_sol_tmp = exact_sol.subs(all_coeff_subs_d).doit()
exact_sol_ = exact_sol_tmp.subs({ y(z) : target_eq_y_subs, z : x - C0 * t }).doit()

#embed()


