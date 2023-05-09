use "/Users/jaredblack/Documents/PhD/Data/Mass 2012 Project/HospRevType2.dta", clear
codebook FiscalYear
* encode FiscalYear, gen(fiscalyear_cod) already encoded in dta
recast float FiscalYear
codebook FiscalYear
* codebook FiscalYear if FiscalYear == 01jan2008
codebook yearvar
gen lnrevtot = ln(revtot)
codebook revtot
summ revtot
encode STATE, gen(statecod)
codebook statecod, tab(100)
codebook yearvar
* encode yearvar, gen(yearcod) previously encoded

* CREATE DID PERIOD *
gen timedid = 0 if yearvar <=2011
replace timedid = 1 if yearvar >=2012
label variable timedid "DID Period"
label define didperiodlabel 0 "Pre" 1 "Post"
label values timedid didperiodlabel

codebook statecod, tab(100)

* Generate the encoded variable hospnamecod using Hospital_Names
egen hospnamecod = group(Hospital_Name), label

* Generate the new variable revtotmil by dividing revtot by one million
gen revtotmil = revtot / 1e6

* Save the dataset with the new variable
save "C:\Users\alons\Downloads\HospRevType2.dta" , replace 

*Remove from KY two hospitals: 1771 and 1776
gen samplevar = 1 if statecod == 22
replace samplevar = 0 if statecod == 44
replace samplevar = 0 if statecod == 34
replace samplevar = 0 if statecod == 20
drop if hospnamecod == 1771
drop if hospnamecod == 1776
drop if hospnamecod == 5256
drop if hospnamecod == 8464

* CREATING UNIQUE IDENTIFYING NAMES WITH STATE*
drop if yearvar== .
tab hospnamecod yearvar
codebook hospnamecod if statecod == 20, tab(1000)
codebook hospnamecod if statecod == 34, tab(1000)
codebook hospnamecod if statecod == 44 , tab(1000)
codebook hospnamecod if statecod == 22 , tab(1000)
egen hospstate = concat (Hospital_Name STATE)
encode hospstate, gen(hospstatecod)


* Set the panel data structure
xtset hospstatecod yearvar

***** CORRECT CUNNINGHAM OLS INTERACTIONS *****
* MA code: 22, NH code: 34 *
drop Treat interaction
gen Treat = 1 if statecod == 22
replace Treat = 0 if statecod == 34
gen interaction = Treat * timedid
label variable Treat "Mass"
label variable timedid "Post"
label variable interaction "Mass x Post"
reg revtotmil Treat timedid interaction
outreg2 using "/Users/jaredblack/Documents/PhD/Data/Mass 2012 Project/myreg.doc", replace label ctitle(MA & NH)

* MA code: 22, RI code: 44 *
drop Treat interaction
gen Treat = 1 if statecod == 22
replace Treat = 0 if statecod == 44
gen interaction = Treat * timedid
label variable Treat "Mass"
label variable timedid "Post"
label variable interaction "Mass x Post"
reg revtotmil Treat timedid interaction
outreg2 using "/Users/jaredblack/Documents/PhD/Data/Mass 2012 Project/myreg.doc", append label ctitle(MA & RI)

* MA code: 22, KY code: 20 *
drop Treat interaction
gen Treat = 1 if statecod == 22
replace Treat = 0 if statecod == 20
gen interaction = Treat * timedid
label variable Treat "Mass"
label variable timedid "Post"
label variable interaction "Mass x Post"
reg revtotmil Treat timedid interaction
outreg2 using "/Users/jaredblack/Documents/PhD/Data/Mass 2012 Project/myreg.doc", append label ctitle(MA & KY)


* Calculate the mean of revtotmil over time for each combination in the first regression (yearvar and MA)
preserve
collapse (mean) mean_revtotmil=revtotmil, by(yearvar manhdid)
twoway (line mean_revtotmil yearvar if manhdid==1) (line mean_revtotmil yearvar if manhdid==0), xline(2012) legend(label(1 "MA") label(2 "NH")) ytitle("Mean Hospital Revenue / Millions") xtitle("Year") title("DiD Graph MA and NH") subtitle("2008-2017") graphregion(color(white))
restore

* Calculate the mean of revtotmil over time for each combination in the second regression (yearvar and RI)
preserve
collapse (mean) mean_revtotmil=revtotmil, by(yearvar maridid)
twoway (line mean_revtotmil yearvar if maridid==1) (line mean_revtotmil yearvar if maridid==0), xline(2012) legend(label(1 "MA") label(2 "RI")) ytitle("Mean Hospital Revenue / Millions") xtitle("Year") title("DiD Graph MA and RI") subtitle("2008-2017") graphregion(color(white))
restore

* Calculate the mean of revtotmil over time for each combination in the third regression (yearvar and KY)
preserve
collapse (mean) mean_revtotmil=revtotmil, by(yearvar makydid)
twoway (line mean_revtotmil yearvar if makydid==1) (line mean_revtotmil yearvar if makydid==0), xline(2012) legend(label(1 "MA") label(2 "KY")) ytitle("Mean Hospital Revenue / Millions") xtitle("Year") title("DiD Graph MA and KY") subtitle("2008-2017") graphregion(color(white))
restore


*********** WRONG CODES THAT NEED TO BE FIXED BUT NOT NECESSARY FOR CAUSAL *************
* SETTING UP THE LONGITUDINAL DIFF IN DIFFS *
gen timedidxt = 1 if timedid == 1 & manhdid == 1
replace timedidxt = . if timedidxt == 0
gen manhdidxt = 0 if timedid == 0
replace manhdidxt = 1 if timedid == 1
replace manhdidxt = 0 if timedidxt == 0
replace timedidxt = 0 if timedidxt == .
replace manhdidxt = 0 if timedidxt == 0
replace timedidxt = 1 if yearvar >= 2012
tab manhdidxt timedidxt
xtdidregress (revtotmil) (manhdidxt), group( hospstatecod ) time(yearvar)
* MA code: 22, NH code:34 *
gen manhdid = 1 if statecod == 22
replace manhdid = 0 if statecod ==34
reg revtot c.manhdid#c.timedid 
label define manhdidlabel 0 "NH" 1 "MA"
label values manhdid manhdidlabel
* means look good *
mean revtot, over(manhdid timedid)
* MA code: 22, RI code:44 *
gen marididxt = manhdidxt if statecod == 22
replace marididxt = 0 if statecod == 44
xtdidregress (revtotmil) (marididxt), group( hospstatecod ) time(yearvar)
tab marididxt yearvar
reg revtotmil marididxt timedidxt


