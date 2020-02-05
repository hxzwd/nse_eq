
import os
import sys


from IPython import embed


from funcs import *


n_for_eq = 5

#print(sys.argv)
#sys.exit(0)

for arg in sys.argv:
	if "--n=" in arg:
		n_for_eq = int(arg.split("=")[-1])


print(f"n_for_eq = { n_for_eq }")

print("GENERATE TARGET EQ")
res_ = f_gen_target_eq(n_for_eq)

print("SUBS TRAVELING WAVE SOLUTION")
tt = f_subs_trav_wave(**res_)

tt1 = f_subs_R(**tt)

r_eqs = tt1["r_eqs"]
print("LINE PARAMS FOR REAL PART")
lp_params = f_line_up_params(r_eqs)

print("EXTRACT REAL SUBS")
subs_re_d = f_extract_from_eq_sys_(r_eqs, lp_params, list(range(0, len(lp_params))))


tmp2 = tt["tmp2"]
target_eq_y = tmp2

subs_d = tt1["subs_d"]
target_eq_y_subs = subs_d[y(z)]

subs_im_d = tt["subs_im_d"]

all_coeff_subs_d = { }
all_coeff_subs_d.update(subs_re_d)
all_coeff_subs_d.update(subs_im_d)

print(f"all_coeff_subs_d =\n{ all_coeff_subs_d }\n")

target_eq_y_ = target_eq_y.subs(all_coeff_subs_d).expand().simplify().doit()

print("COMPUTE TMP0")
TMP0 = target_eq_y_.subs({ y(z) : target_eq_y_subs }).doit()

print("COMPUTE TMP1")
TMP1 = TMP0.subs({ R(z) : R_subs_ }).simplify().factor().simplify().doit()
print("COMPUTE CHECK_RES")
CHECK_RES = TMP1.simplify()

print(f"CHECK_RES = { CHECK_RES }")

embed()
