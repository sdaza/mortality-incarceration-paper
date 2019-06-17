clear all

use "output/psid_gompertz_data_95.dta"

tabulate fracei, generate(fracei)
tabulate ieduc, generate(ieduc)

label variable iprison "\hspace{0.0cm}Prison"
label variable cage "\hspace{0.0cm}Age"
label variable gender "\hspace{0.0cm}Male"
label variable fracei2 "\hspace{0.2cm}Black"
label variable fracei3 "\hspace{0.2cm}Other race + unknown"
label variable liinc "\hspace{0.0cm}Log income, centered"
label variable ieduc2 "\hspace{0.2cm}High school"
label variable ieduc3 "\hspace{0.2cm}Some college"
label variable ieduc4 "\hspace{0.2cm}College"

stset stop, id(pid) failure(died) enter(time start)
eststo: streg iprison cage gender, nohr dist(gompertz)
/* vce(cluster family_id) */

stset stop, id(pid) failure(died) enter(time start)
eststo: streg iprison cage gender fracei2 fracei3, nohr dist(gompertz)
 /* vce(cluster family_id) */

stset stop, id(pid) failure(died) enter(time start)
eststo: streg iprison cage gender fracei2 fracei3 liinc ieduc2 ieduc3 ieduc4, nohr dist(gompertz)
/* vce(cluster family_id) */

* create table
esttab using output/gompertz_models.tex, replace label gaps nodepvars nonotes ///
title("Gompertz Models") ///
nonumbers mtitles("Model 1" "Model 2")  booktabs alignment(S S) ///
cells("b(fmt(2))" "se(fmt(2)par)") ///
collabels(none) ///
refcat(fracei2 "Race (ref. White)" ///
       ieduc2 "Education (ref. $<$ HS)", nolabel) ///
      stats(aic N_fail risk, fmt (0 0 0) ///
      layout("\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}") ///
      labels(`"AIC"' `"Deaths"' `"Person-years"')) ///
      addnotes("Standard errors in parenthesis.")
