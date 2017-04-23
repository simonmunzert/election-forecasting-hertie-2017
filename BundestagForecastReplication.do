********************************************************************************
* "A Predictive Test of Voters' Economic Benchmarking:
* The 2013 German Bundestag Election"		
* Mark Andreas Kayser and Arndt Leininger
* Replication Material											
********************************************************************************

clear all

cd "/home/arndt/Dropbox/01 PhD/02projects/election forecasting project/data/replication"

use BundestagForecastReplicationData

set scheme s1mono

set more off

********************************************************************************
* Definition of variables 
********************************************************************************

*Variables to hold output of out-of-sample estimation
gen outgovshare_hat=. //predicted vote share of outgoing government
gen perror=. //Error: acutal vote share - minus prediction
gen abperror=. //Absolute error: |perror|
gen sqperror=. //Squared error: (perror)²

gen b_cof1=. /*coefficient values... (for every observation it holds the coefficients 
of that out-of-sample estimatin not including that observation)*/
gen b_cof2=.
gen b_cof3=.
gen b_cof4=.

gen se_cof1=. //standard errors of the coefficients..
gen se_cof2=.
gen se_cof3=.
gen se_cof4=.

gen b_cof1ciu=. //upper bound of the cofindence interval for the coefficient
gen b_cof1cil=. //lower bound of the cofindence interval for the coefficient

gen b_cof2ciu=.
gen b_cof2cil=.

gen b_cof3ciu=.
gen b_cof3cil=.

gen b_cof4ciu=.
gen b_cof4cil=.

gen rmseciu=.
gen rmsecil=.

gen routgovshare=.
gen routgovshare_hat=.

*Variables for scatterplot
gen posog=3
gen posogh=3

* Scalars for out-of-sample estimation
scalar soutgovshare_hat=0 //holds point estimate of current out-of-sample estimation
scalar serror=0 //holds error of current point estimation: outgovshare[current observation]-soutgovshare_hat

********************************************************************************
* Loop function for out-of-sample estimation 
********************************************************************************

//It calculates one seperate regression per observation, excluding that obersvation and predicting vote share for the omitted variable
//Results are saved into variables so they can be plotted

forvalues lp=9/18 { 
//run the regression
quietly reg outgovshare prelecshare pid benchgrow logterms if wp!=`lp' //this should exclude the current wp from estimation of coefficient

mat cof=e(b) //matrix to read coefficient values from

/* outputs essential regression output, not needed for analysis
di "excluding " year[`lp'] ": N = " e(N) ", R2 = " e(r2) ", RMSE = " e(rmse)
di "prelecshare:" cof[1,1] ", logpid: " cof[1,2] ", unem: " cof[1,3] ", logterms: " cof[1,4]
di "prelecshare:" prelecshare[`lp'] ", uneme " unem[`lp'] ", logpid: " logpid[`lp'] ", logterms: " logterms[`lp']
*/

//save the prediction and the error
scalar soutgovshare_hat=cof[1,1]*prelecshare[`lp']+cof[1,2]*pid[`lp']+cof[1,3]*benchgrow[`lp']+cof[1,4]*logterms[`lp']+cof[1,5] 

//calculate the error of the prediction
scalar serror=outgovshare[`lp']-soutgovshare_hat

//saves the standard errors of the coefficients (excl. constant) in a matrix
mat define bserrors = (_se[prelecshare],_se[pid],_se[benchgrow],_se[logterms])

* input scalar values into respective variables
replace outgovshare_hat=soutgovshare_hat if wp==`lp'
replace perror=serror if wp==`lp'
replace abperror=abs(serror) if wp==`lp'
replace sqperror=serror^2 if wp==`lp'

replace b_cof1=cof[1,1] if wp==`lp'
replace se_cof1=bserrors[1,1] if wp==`lp'

replace b_cof2=cof[1,2] if wp==`lp'
replace se_cof2=bserrors[1,2] if wp==`lp'

replace b_cof3=cof[1,3] if wp==`lp'
replace se_cof3=bserrors[1,3] if wp==`lp'

replace b_cof4=cof[1,4] if wp==`lp'
replace se_cof4=bserrors[1,4] if wp==`lp'
}

//list year outgovcoa outgovshare outgovshare_hat perror abperror sqperror b_cof1 se_cof1 b_cof2 se_cof2 b_cof3 se_cof3 b_cof4 se_cof4 if wp>5

quietly sum abperror, mean
di "Mean Absolute Error (MAE): " r(mean) 

quietly sum sqperror, mean
local rmse=sqrt(r(mean))
di "Root Mean Square Error (RMSE): " `rmse' 


********************************************************************************
* Figure 1: Predictors of the vote
********************************************************************************

//define position variables and values
gen pos1=3
replace pos1=9 if year==2002 | year==1994
replace pos1=10 if year==1990
replace pos1=8 if year==2005
replace pos1=6 if year==1980
replace pos1=12 if year==1983
replace pos1=7 if year==1987
replace pos1=1 if year==2009

gen pos2=3
replace pos2=2 if year==2002 | year==1994 | year==1990
replace pos2=4 if year==1998
replace pos2=5 if year==1987

gen pos3=3
replace pos3=2 if year==1999 | year==1980
replace pos3=12 if year==2002
replace pos3=9 if year==1998
replace pos3=6 if year==1990

gen pos4=3
recode pos4 (3=2) if year==2002
recode pos4 (3=4) if year==1994 | year==1980


	corr outgovshare prelecshare if year>=1980 & year<=2013
	mat mcorr=r(C)
	local lcorr = string(mcorr[2,1], "%2.1f") 
	twoway lfit outgovshare prelecshare if year>=1980, lwidth(medthick) lcolor(cranberry) || scatter outgovshare prelecshare if year>=1980, mcolor(gray) mlabv(pos1) mlabel(year) msymbol(D) msize(medlarge) mlabsize(medium) || , text(31 52 "r = `lcorr'", place(e)) title("Vote share in previous election") ytitle() xtitle("") legend(off) name(scatter1, replace) nodraw 

	corr outgovshare pid if year>=1980 & year<=2013
	mat mcorr=r(C)
	local lcorr = string(mcorr[2,1], "%2.1f")
	twoway lfit outgovshare pid if year>=1980, lwidth(medthick) lcolor(cranberry) || scatter outgovshare pid if year>=1980, mcolor(gray) mlabv(pos2) mlabel(year) msymbol(D) msize(medlarge) mlabsize(medium) || , text(31 45 "r = `lcorr'", place(e)) title("Party Identification") ytitle() xscale(r(25 50)) xtitle("") legend(off)name(scatter2, replace) nodraw

	corr outgovshare benchgrow if year>=1980 & year<=2013
	mat mcorr=r(C)
	local lcorr = string(mcorr[2,1], "%2.1f")
	twoway lfit outgovshare benchgrow if year>=1980, lwidth(medthick) lcolor(cranberry) || scatter outgovshare benchgrow if year>=1980, mcolor(gray) mlabv(pos3) mlabel(year) msymbol(D) msize(medlarge) mlabsize(medium) || , text(31 3 "r = `lcorr'", place(e)) title("Benchmarked growth") ytitle() xtitle("") legend(off) name(scatter3, replace) nodraw

	corr outgovshare logterms if year>=1980 & year<=2013
	mat mcorr=r(C)
	local lcorr = string(mcorr[2,1], "%2.1f")
	twoway lfit outgovshare logterms if year>=1980, lwidth(medthick) lcolor(cranberry) || scatter outgovshare logterms if year>=1980, mcolor(gray) mlabv(pos4) mlabel(year) msymbol(D) msize(medlarge) mlabsize(medium) || , text(31 .65 "r = `lcorr'", place(e)) title("Terms in office (logged)") ytitle() xtitle("") legend(off) name(scatter4, replace) nodraw

	graph combine scatter1 scatter2 scatter3 scatter4, cols(2) ///
		l1title("Vote share obtained by outgoing government") ycommon
	* graph export fig1predictors.pdf, replace
	
	
********************************************************************************
* Table 1: Elections in model	
********************************************************************************

* Table 1 was produced manually


********************************************************************************
* Table 2: A regression model of past elections to predict future elections
********************************************************************************


//regression is estimate on 1969-2009 data (21 obs)
reg outgovshare prelecshare pid benchgrow logterms

quietly sum sqperror, mean
local rmse=sqrt(r(mean))

//Durbin-Watson test
tsset wp
estat dwatson
ereturn list

esttab

* produces and saves the regression table
* outreg2 using regoutput.tex, tex(fragment) sideway alpha(0.001, 0.01, 0.05) adds(Adj. R², e(r2_a), RMSE°, `rmse', Durbin-Watson d, r(dw)) addtext() addnote("Note: Model estimated on elections 1969-2009", "° calculated from out-of-sample predictions") ctitle("Coefficient"; "(S.E.)") replace  


********************************************************************************
* Figure 2: Stability of coefficient estimates
********************************************************************************


//calculate upper and lower bounds of the cis of the coefficients
replace b_cof1ciu=b_cof1+invttail(3,.025)*se_cof1
replace b_cof1cil=b_cof1-invttail(3,.025)*se_cof1
replace b_cof2ciu=b_cof2+invttail(3,.025)*se_cof2
replace b_cof2cil=b_cof2-invttail(3,.025)*se_cof2
replace b_cof3ciu=b_cof3+invttail(3,.025)*se_cof3
replace b_cof3cil=b_cof3-invttail(3,.025)*se_cof3
replace b_cof4ciu=b_cof4+invttail(3,.025)*se_cof4
replace b_cof4cil=b_cof4-invttail(3,.025)*se_cof4

gen year1 = year-.5
gen year2 = year+.5

*coefficients and their cis over time (to illustrate stability of coefficient estimates)
// coefficient values and their cis per year, serves to illustrate stability of coefficient estimates in out-of-sample prediction
twoway rcap b_cof1ciu b_cof1cil year1, color(black) || ///
	scatter b_cof1 year1, mcolor(black) msymbol(D) msize(medlarge) || ///
	 rcap b_cof2ciu b_cof2cil year2, color(black) || ///
	scatter b_cof2 year2, mcolor(black) msymbol(S) msize(medlarge) || ///
	rcap b_cof3ciu b_cof3cil year, color(black) || scatter b_cof3 year, mcolor(black) msymbol(O) msize(medlarge) ///
	|| if year>=1980, yscale(r(-1.2 2.7)) ylabel(-.5(.5)2.5) yline(0, lpattern(dash)) ///
	xmtick(1980(1)2013) xlabel(1980 1983 1987 1990(4)2002 2005(4)2013, angle(45) ///
	labsize(small)) xtitle("") legend(ring(0) position(4) order(2 4 6) size(medsmall) label(2 "Prev. Election Vote Share") ///
	label(4 "Party Identification") label(6 "Benchmarked growth") region(lwidth(none))) 
	
	*caption("Coefficient values and 95% confidence intervals obtained in 'out-of-sample'" "prediction")*/

* graph export fig3coeff.pdf, replace
* graphexportpdf fig3coeff


********************************************************************************
* Figure 3: Actual and predicted vote share
********************************************************************************

//Upper and lower bound of the confidence interval for the 2013 prediction (based on out-of-sample RMSE)
//need to obtain mean of squared error first
quietly sum sqperror, mean
replace rmseciu=outgovshare_hat+invttail(4,.025)*sqrt(r(mean)) if year==2013
replace rmsecil=outgovshare_hat-invttail(4,.025)*sqrt(r(mean)) if year==2013


//Create rounded values for scatterplot
replace routgovshare=round(outgovshare,.01)
replace routgovshare_hat=round(outgovshare_hat,.01)

//Reset positions of overlapping markers
gen pos5=9
replace pos5=4 if year==1980

replace pos4=3
replace pos4=7 if year==2013

*Scatterplot of actual election outcomes (outgovshare) and "out-of-sample" predictions (outgovshare_hat)
//plots actual and predicted vote share against year, includes 2013 prediction with ci
twoway scatter outgovshare year if year>=1980, mlabel(routgovshare) msymbol(O) mcolor(cranberry) mlabcolor(cranberry) msize(large) mlabsize(medium) mlabv(pos4) || scatter outgovshare_hat year if year>=1980 & year<=2009, mlabv(pos5) mlabel(routgovshare_hat) msymbol(Oh) mcolor(navy) mlabcolor(navy) msize(large) mlwidth(medthick) mlabsize(medium) || scatter outgovshare_hat year if year==2013, mlabv(pos5) mlabel(routgovshare_hat) msymbol(D) mcolor(emidblue) msize(medlarge) mlwidth(medthick) mlabsize(medium) || rcap rmseciu rmsecil year if year==2013 || , xscale(r(1980 2013)) xmtick(1980(1)2013) xlabel(1980 1983 1987 1990(4)2002 2005(4)2013, labsize(medsmall)) xtitle("") /*yline(50, lcolor(gs12) lpattern(shortdash))*/ title("") caption("Vote share of outgoing governing coalitions and corresponding point predictions" "from 'out-of-sample' prediction") ///
	legend(ring(0) position(8) order(1 2 3) label(1 "Actual vote share") label(2 "Predicted vote share") label(3 "Forecast") region(lwidth(none)))

* save graph to file
* graph export fig2predictions.pdf, replace


********************************************************************************
* Table 3: A comparison of benchmarked and non-benchmarked growth
********************************************************************************


reg outgovshare prelecshare pid benchgrow logterms
estimate store m1

reg outgovshare prelecshare pid gergrow logterms
estimate store m2

esttab m*, r2 ar2 mtitles("benchmarked" "not benchmarked") nonumbers

* writes table to file

/*
esttab m* using "t_growth.tex", booktabs mtitles("benchmarked" "not benchmarked") nonumbers r2 ar2 se ///
	coeflabels(prelecshare "Previous Vote Share" pid "Party ID" ///
		benchgrow "Benchmarked Growth" logterms "Log Terms" ///
		_cons "Constant" gergrow "Growth Germany") ///
		order(prelecshare pid benchgrow grgrow logterms)
*/

********************************************************************************
* Table 4: Election result, forecasts and final pre-election polls
********************************************************************************

* Table 4 was produced manually


********************************************************************************
* Figure 4: Timeline of election forecasts
********************************************************************************

use overtime.dta, clear

*** polls and forecasts
tw 	(line outgovshare date2 if source=="Forschungsgruppe", lcolor(navy) lpattern(longdash)) ///
	(line outgovshare date2 if source=="Forsa", lcolor(cranberry) lpattern(dash)) ///
	(line outgovshare date2 if source=="Allensbach", lcolor(gray) lpattern(longdash_dot)) ///
	(line result date2, lcolor(black)) ///
	(scatter outgovshare date2 if polling==0, ///
		msymbol(D) mlabel(source) mcolor(black) mlabvpos(pos)) ///
	|| if (year==2013)&(date2<19623) ///
	|| , xtitle("2013", margin(medsmall)) ytitle("Coalition vote share (%)") ///
	yscale(r(40.5 52)) yline(46.3, lcolor(navy)) ///
	legend(ring(0) position(4) label(1 "FG Wahlen") label(2 "Forsa") ///
		label(3 "Allensbach") label(4 "Election Outcome") ///
		label(5 "Forecasts") ///
		region(lwidth(none)) size(small)) ///
	xlabel(#9,  format(%tdm)) plotregion(margin(medium)) 


********************************************************************************
* Table 5: Model comparison
********************************************************************************

* Table 5 was produced manually
