

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



C0 = Symbol("C0")
k = Symbol("k")
omega = Symbol("omega")
b = Symbol("b")
z = Symbol("z")

N = Symbol("N")
A = Symbol("A")
Xi = Symbol("Xi")
a = Symbol("a")
alpha = Symbol("alpha")
C = Symbol("C")
R = Function("R", nargs = 1)

subs_fun = y(x - C0 * t) * exp( I * ( k * x - omega * t ) )

R_subs_ = 2 * exp(z) * C / ( C + Xi * exp(2 * z) )
R_subs_ = R_subs_.subs({ C : 1 })




def print_hello_msg():
	print("[funcs]: mephim/eq\n")
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


def f_line_up_one_param(eq, not_include = [ ]):
		f_symb = eq.lhs.free_symbols
		a_symb = [ i for i in f_symb if "a" == i.name[0] and i not in not_include ]
		pass
		d_ = dict([ [ i, sp.degree(eq, i) ] for i in a_symb ])
		line_up_param = min(d_, key = lambda x : d_[x])
		d_min_ = [ ]
		for key in d_.keys():
				if d_[key] == d_[line_up_param]:
						d_min_.append(key)
		line_up_param = min(d_min_, key = lambda x : int(x.name[1:]))
		return { "f_symb" : f_symb, "a_symb" : a_symb, "d_" : d_, "d_min_" : d_min_, "line_up_param" : line_up_param }


def f_line_up_params(eqs):
		pass
		line_up_params_list = [ ]
		new_eqs = eqs[ 1 : -1 ]
		for eq in new_eqs:
				tmp_ = f_line_up_one_param(eq, not_include = line_up_params_list)
				line_up_params_list.append(tmp_["line_up_param"])
		return [ Symbol("b") ] + line_up_params_list + [ Symbol("omega") ]



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


def f_subs_trav_wave_old(target_eq = None, params = { }, q = None, y = None, x = None, t = None, q_der_list = [ ], n = None):
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
	embed()
	subs_im_d = f_extract_from_eq_sys_(im_eq_sys, tt, eq_num_list)
	res_d["tt"] = tt
	res_d["subs_im_d"] = subs_im_d
	res_d["n"] = n
	return res_d


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
	tt = [ C0 ]
	for eq in im_eq_sys[ 1: ]:
		lp_tmp = f_line_up_one_param(eq, not_include = tt)
		tt.append(lp_tmp["line_up_param"])
	#embed()
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


def f_line_up_one_param_old(eq, not_include = [ ]):
		f_symb = eq.lhs.free_symbols
		a_symb = [ i for i in f_symb if "a" == i.name[0] and i not in not_include ]
		pass
		d_ = dict([ [ i, sp.degree(eq, i) ] for i in a_symb ])
		line_up_param = min(d_, key = lambda x : d_[x])
		d_min_ = [ ]
		for key in d_.keys():
				if d_[key] == d_[line_up_param]:
						d_min_.append(key)
		line_up_param = min(d_min_, key = lambda x : int(x.name[1:]))
		return { "f_symb" : f_symb, "a_symb" : a_symb, "d_" : d_, "d_min_" : d_min_, "line_up_param" : line_up_param }

def f_line_up_params_old(eqs):
		pass
		line_up_params_list = [ ]
		new_eqs = eqs[ 1 : -1 ]
		for eq in new_eqs:
				tmp_ = f_line_up_one_param(eq, not_include = line_up_params_list)
				line_up_params_list.append(tmp_["line_up_param"])
		return [ Symbol("b") ] + line_up_params_list + [ Symbol("omega") ]


