log close
log using N:\QuantMeth\mylog.log, replace
cd N:\QuantMeth\ESS6PL.stata
*setting the directory so I can download specific user-written programs when using remote desktop
sysdir set PLUS "N:\Stata\ADO"
sysdir
ssc install corr_svy
ssc install estout
use "N:\QuantMeth\ESS6PL.stata\ESS6PL.dta", clear
describe
svyset _n [pw=pspwght]
svydescribe

*y=r_happy r_stflife
*x=trust: r_ppltrst r_pplfair r_pplhlp
*x, controls: agea r_hinctnta living_w_partner r_sclmeet rr_health
*x, controls, potential split of age groups: age16_24 age25_34 age35_44 age45_54 age55_64 age65_up

*RECODING TO SYSTEM MISSING
foreach var of varlist ppltrst pplfair pplhlp trstprl trstlgl trstplc trstplt trstprt trstep trstun tvtot tvpol hinctnta eduyrs eisced
lrscale stflife stfgov stfeco stfdem stfedu stfhlth rlgdgr rlgatnd implvdm dmcntov happy fairelcc dspplvtc sclmeet dfprtalc
oppcrgvc medcrgvc meprinfc rghmgprc votedirc cttresac gptpelcc gvctzpvc gvexpdcc grdfincc pltaviec {
gen r_`var'=`var'
replace r_`var'=. if `var'==99|`var'==88|`var'==77|`var'==55
}
foreach var of varlis polintr domicil health lfwrs sclact aesfdrk hincfel{
gen r_`var'=`var'
replace r_`var'=. if `var'==8|`var'==9
}

*http://www.tandfonline.com/doi/pdf/10.1080/00036840500368094 use categorical dependent variables without
transformation
*http://discovery.ucl.ac.uk/14315/1/14315.pdf uses EES survey, shows graphs you can use to show differences, frequency
tables, but only used correlation coefficients--but did show the means of attitudes
*http://www.jstor.org/stable/pdf/10.1086/588220.pdf?acceptTC=true&jpdConfirm=true does not use EES data but offers
great wording for how to justify the use of categorical variables, and the caveats hat come with them

*http://storre.stir.ac.uk/bitstream/1893/8830/1/Delaney_2007_Social_Capital_and_Self-Rated_Health.pdf uses multiple
regression, EES
*http://download-v2.springer.com/static/pdf/428/art%253A10.1007%252Fs11205-005-4859-
2.pdf?token2=exp=1430069938~acl=%2Fstatic%2Fpdf%2F428%2Fart%25253A10.1007%25252Fs11205-005-4859-
2.pdf*~hmac=5e954a7660d7a0a781295cd7b55da7d2f1d428b8166299d9f055fdb9dd346738 EES, multiple regression
*http://www.baylorisr.org/wp-content/uploads/2013-PRS-Religious-Behavior-Health-Well-Being.pdf : use of living with
partner as close approximation of marital status
*http://www.baylorisr.org/wp-content/uploads/2013-PRS-Religious-Behavior-Health-Well-Being.pdf use of EES data for
happiness, wellbeing and health
*http://www.ats.ucla.edu/stat/stata/faq/dummy.htm testing the inclusion of all categories of a variable

gen living_w_part=0
replace living_w_part=1 if icpart1==1

gen married=0
replace married=1 if maritalb==1
corr_svy living_w_part married [pw=pweight], pw star(0.0001)

*reverse code health (5 was worst)
gen rr_health=r_health
replace rr_health=1 if r_health==5
replace rr_health=2 if r_health==4
replace rr_health=3 if r_health==3
replace rr_health=4 if r_health==2
replace rr_health=5 if r_health==1

**reverse code hincfel (4 was worst)
gen rr_hincfel=r_hincfel
replace rr_hincfel=1 if r_hincfel==4
replace rr_hincfel=2 if r_hincfel==3
replace rr_hincfel=3 if r_hincfel==2
replace rr_hincfel=4 if r_hincfel==1

gen female=0
replace female=1 if gndr==2

gen employed_above20=0

replace employed_above20=1 if agea>=20 & pdwrk==1 & female==1

gen childhouse=0
replace childhouse=1 if chldhm==1

*generating age groupings
gen age15_27=0
replace age15_27=1 if agea>=15 & agea<=27
gen age28_39=0
replace age28_39=1 if agea>=28 & agea<=39
gen age40_54=0
replace age40_54=1 if agea>=40 & agea<=54
gen age55_64=0
replace age55_64=1 if agea>=55 & agea<=64
gen age65_up=0
replace age65_up=1 if agea>=65

kdensity agea, norm
kdensity r_hinct, norm
kdensity rr_health, norm
kdensity r_sclmeet, norm
kdensity r_rlgdgr, norm
kdensity r_ppltrst, norm
kdensity r_pplfair, norm
kdensity r_pplhlp, norm
kdensity r_eisced, norm
kdensity r_eduyr, norm
kdensity r_stflife, norm
kdensity r_happy, norm

*rr_health doesn't look too good; perhaps we can break it apart
tabulate rr_health, gen (h_)
*hinct doesn't look so great either, maybe we can use feelings of adequacy of income
tabulate r_hinctnta, generate(inc_)

tabulate rr_hincfel, gen (incfeel_)
*education, eisced doesn't look normal at all, use r_eduyr.
*considered splitting into 5 categories, as do http://www.sciencedirect.com/science/article/pii/S0167487008000809
corr_svy r_eduyr r_eisced [pw=pweight], pw star(0.0001)
corr r_hinctnta r_hincfel

twoway (scatter r_stflife r_pplfair) (lfit r_stflife r_pplfair) (lowess r_stflife r_pplfair)
*pplfair curves down a little at the end, looks like there are some outliers?
twoway (scatter r_stflife r_ppltrst) (lfit r_stflife r_ppltrst) (lowess r_stflife r_ppltrst)
*ppl trust curves down at the end, looks like there are some outliers?
twoway (scatter r_stflife agea) (lfit r_stflife agea) (lowess r_stflife agea)
*age is curvy, justifies adding the categories in as dummies (or transforming it)
twoway (scatter r_stflife r_hinctnta) (lfit r_stflife r_hinctnta) (lowess r_stflife r_hinctnta)
twoway (scatter r_stflife r_rlgdgr) (lfit r_stflife r_rlgdgr) (lowess r_stflife r_rlgdgr)

****running means for independent variables
foreach var of varlist agea female r_eduyr r_hinct rr_hincfel rr_health r_sclmeet living_w_part r_rlgdgr r_ppltrst r_pplfair
r_pplhlp {
svy: mean `var'
estat sd
}

graph box r_ppltrst r_pplfair r_pplhlp [pw=pspwght]
graph box r_hinct [pw=pspwght]
graph box r_eduyr [pw=pspwght]
graph box rr_health r_rlgdgr r_sclmeet [pw=pspwght]
graph box r_rlgdgr r_sclmeet [pw=pspwght]
graph box r_stflife r_stfeco r_stfgov r_stfdem r_happy [pw=pspwght]
graph box agea [pw=pspwght]

***INDEPENDENT VARIABLE CORRELATIONS
corr_svy agea female r_eduyr r_hinct h_1 h_2 h_3 h_4 h_5 r_sclmeet r_rlgdgr living_w_part r_ppltrst r_pplfair r_pplhlp
[pw=pweight], pw star(0.05)
corr_svy agea female r_eduyr r_hinct h_1 h_2 h_3 h_4 h_5 r_sclmeet r_rlgdgr living_w_part r_ppltrst r_pplfair r_pplhlp
[pw=pweight], pw star(0.01)
corr_svy agea female r_eduyr r_hinct h_1 h_2 h_3 h_4 h_5 r_sclmeet r_rlgdgr living_w_part r_ppltrst r_pplfair r_pplhlp
[pw=pweight], pw star(0.001)

corr_svy age15_27 age28_39 age40_54 age55_64 age65_up r_ppltrst r_pplfair r_pplhlp [pw=pweight], pw star(0.05)
corr_svy age15_27 age28_39 age40_54 age55_64 age65_up r_ppltrst r_pplfair r_pplhlp [pw=pweight], pw star(0.01)
corr_svy age15_27 age28_39 age40_54 age55_64 age65_up r_ppltrst r_pplfair r_pplhlp [pw=pweight], pw star(0.001)

***DEPENDENT VARIABLE CORRELATIONS
foreach var of varlist age15_27 age28_39 age40_54 age55_64 age65_up agea female r_eduyr r_hinct h_1 h_2 h_3 h_4 h_5
r_sclmeet r_rlgdgr living_w_part r_ppltrst r_pplfair r_pplhlp {
corr_svy `var' r_stflife [pw=pweight], pw star(0.05)
corr_svy `var' r_stflife [pw=pweight], pw star(0.01)
corr_svy `var' r_stflife [pw=pweight], pw star(0.001)
}
foreach var of varlist age15_27 age28_39 age40_54 age55_64 age65_up agea female r_eduyr r_hinct h_1 h_2 h_3 h_4 h_5
r_sclmeet r_rlgdgr living_w_part r_ppltrst r_pplfair r_pplhlp {
corr_svy `var' r_stfeco [pw=pweight], pw star(0.05)
corr_svy `var' r_stfeco [pw=pweight], pw star(0.01)
corr_svy `var' r_stfeco [pw=pweight], pw star(0.001)
}
foreach var of varlist age15_27 age28_39 age40_54 age55_64 age65_up agea female r_eduyr r_hinct h_1 h_2 h_3 h_4 h_5
r_sclmeet r_rlgdgr living_w_part r_ppltrst r_pplfair r_pplhlp {
corr_svy `var' r_stfgov [pw=pweight], pw star(.05)
corr_svy `var' r_stfeco [pw=pweight], pw star(0.01)
corr_svy `var' r_stfeco [pw=pweight], pw star(0.001)
}
foreach var of varlist age15_27 age28_39 age40_54 age55_64 age65_up agea female r_eduyr r_hinct h_1 h_2 h_3 h_4 h_5
r_sclmeet r_rlgdgr living_w_part r_ppltrst r_pplfair r_pplhlp {
corr_svy `var' r_stfdem [pw=pweight], pw star(.05)
corr_svy `var' r_stfdem [pw=pweight], pw star(.01)
corr_svy `var' r_stfdem [pw=pweight], pw star(.001)
}
foreach var of varlist age15_27 age28_39 age40_54 age55_64 age65_up agea female r_eduyr r_hinct h_1 h_2 h_3 h_4 h_5
r_sclmeet r_rlgdgr living_w_part r_ppltrst r_pplfair r_pplhlp {
corr_svy `var' r_happy [pw=pweight], pw star(.05)
corr_svy `var' r_happy [pw=pweight], pw star(.01)
corr_svy `var' r_happy [pw=pweight], pw star(.001)
}

***CORRELATIONS BETWEEN DEPENDENT VARIABLES
corr_svy r_stflife r_happy r_stfeco r_stfgov r_stfdem [pw=pweight], pw star(0.001)
corr_svy r_stflife r_happy r_stfeco r_stfgov r_stfdem [pw=pweight], pw star(0.01)
corr_svy r_stflife r_happy r_stfeco r_stfgov r_stfdem [pw=pweight], pw star(0.05)

svy: regress r_stfgov r_trstprl r_trstlgl r_trstplc r_trstplt r_trstprt r_trstep r_trstun r_tvtot r_tvpol

*___________________________________________________________________________________________________
*___________________________________________________________________________________________________

***LIFE SATISFACTION, PERCENTILE INCOME USED, START***
***slowly adding in sections, regression
***Regressions
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr
estimates store m1, title(demo)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct
estimates store m2, title(inc)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5
estimates store m3, title(health)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr
estimates store m4, title(social)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
estimates store m5, title(trust)

estout m1 m2 m3 m4 m5, cells(b(star fmt(3)) se(par fmt(2))) ///
legend label varlabels(_cons constant) ///
stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC))

*testing normal distribution of residuals
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
predict liferes, r
predict lifehat

scatter lifehat r_stflife

pnorm liferes
sktest liferes
kdensity liferes, norm

**they are NOT... instead of transforming anything I will try restricting the sample
svy, subpop(female): regress r_stflife age28_39 age40_54 age55_64 age65_up r_eduyr r_hinct h_2 h_3 h_4 h_5
living_w_part r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
predict f_satisfresid, r
sktest f_satisfresid
pnorm f_satisfresid

*still no good.

linktest
**testing for joint significance
*heteroskedasticity not tested in survey data? http://www.stata.com/statalist/archive/2011-03/msg01095.html
test age28_39 age40_54 age55_64 age65_up
test h_2 h_3 h_4 h_5
test r_ppltrst r_pplfair r_pplhlp

svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
test _b[r_ppltrst]=_b[r_pplfair]
test _b[r_pplhlp]=_b[r_pplfair]

***LIFE SATISFACTION, PERCENTILE INCOME USED, OVER***

*___________________________________________________________________________________________________
*___________________________________________________________________________________________________

***HAPPINESS, SPLIT EDU AND PERCENTILE INCOME USED, START***
svy: regress r_happy age28_39 age40_54 age55_64 age65_up female r_eduyr
estimates store m1, title(demo)

svy: regress r_happy age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct
estimates store m2, title(inc)
svy: regress r_happy age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5
estimates store m3, title(health)
svy: regress r_happy age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr
estimates store m4, title(social)
svy: regress r_happy age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
estimates store m5, title(trust)

estout m1 m2 m3 m4 m5, cells(b(star fmt(3)) se(par fmt(2))) ///
legend label varlabels(_cons constant) ///
stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC))

*testing normal distribution of residuals
svy: regress r_happy age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
predict happyres, r
predict happyhat
scatter happyhat r_happy

pnorm happyres
*sktest doesn't work with pweights
sktest happyres
kdensity liferes, norm

svy: regress r_happy age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
test _b[r_ppltrst]=_b[r_pplfair]
test _b[r_pplhlp]=_b[r_pplfair]

*___________________________________________________________________________________________________
*SATISFACTION WITH ECONOMY, GOVERNMENT, AND DEMOCRACY
*___________________________________________________________________________________________________
svy: regress r_stfeco age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp

estimates store m1, title(trust)
predict eco, r
pnorm eco
qnorm eco
kdensity eco, norm
sktest eco
test _b[r_ppltrst]=_b[r_pplfair]
test _b[r_pplhlp]=_b[r_pplfair]
test _b[r_pplhlp]=_b[r_ppltrst]

svy: regress r_stfgov age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
estimates store m2, title(trust)
predict gov, r
pnorm gov
qnorm gov
kdensity gov, norm
sktest gov
test _b[r_ppltrst]=_b[r_pplfair]
test _b[r_pplhlp]=_b[r_pplfair]
test _b[r_pplhlp]=_b[r_ppltrst]

svy: regress r_stfdem age28_39 age40_54 age55_64 age65_up female r_eduyr r_hinct h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
estimates store m3, title(trust)
predict dem, r
pnorm dem
qnorm dem
kdensity dem, norm
sktest dem
test _b[r_ppltrst]=_b[r_pplfair]
test _b[r_pplhlp]=_b[r_pplfair]
test _b[r_pplhlp]=_b[r_ppltrst]

estout m1 m2 m3, cells(b(star fmt(3)) se(par fmt(2))) ///
legend label varlabels(_cons constant) ///

stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC))

*****EXPLORATORY
*___________________________________________________________________________________________________

*lrtest to test between models doesn't work with weighted survey data, so will not compare between models 4 and 5.lrtest
m1 m2 (http://www.ats.ucla.edu/stat/stata/faq/nested_tests.htm)

***testing hypothesis 1, that the coefficients for FAIR is significantly different than the coefficient for TRUST and HELP.
*using adjusted Wald's test, which is a compatible post-estimation command with svy data.
*https://www3.nd.edu/~rwilliam/stats2/l42.pdf
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eisced r_hinct rr_health living_w_part r_sclmeet
r_rlgdgr r_ppltrst r_pplfair r_pplhlp
test age28_39 age40_54 age55_64 age65_up
*age is jointly significant, can't remove
test r_ppltrst r_pplfair r_pplhlp
*the variables on trust are jointly significant. Now, are they different?

svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eisced r_hinct rr_health living_w_part r_sclmeet
r_rlgdgr r_ppltrst r_pplfair r_pplhlp
test _b[r_ppltrst]=_b[r_pplfair]
*we cannot reject the null hypothesis that the coefficients for TRUST and FAIR are the same
test _b[r_pplhlp]=_b[r_pplfair]
*we can reject the null hypothesis that the coefficients for HELP and FAIR are the same, and accept the research
hypothesis that they differ (at p<0.05)
predict e, resid
kdensity e, norm
stem e
*other measures looking for outliers are not possible after survey estimatation (dfit, dfbeta...)

*rvf and lvr plots not possible with svy data, but done anyway to examine points with potential leverage
regress r_happy age28_39 age40_54 age55_64 age65_up female r_eisced r_hinct rr_health living_w_part r_sclmeet
r_rlgdgr r_ppltrst r_pplfair r_pplhlp
predict resid, r
rvfplot
lvr2plot

*in the pnorm plot, the residuals do not deviate much from the normal line.
graph twoway (lfit resid r_pplfair) (scatter resid r_pplfair)
graph twoway (lfit resid r_ppltrst) (scatter resid r_ppltrst)
graph twoway (lfit resid r_pplhlp) (scatter resid r_pplhlp)
graph twoway (lfit resid rr_health) (scatter resid rr_health)
graph twoway (lfit resid r_hinct) (scatter resid r_hinct)
graph twoway (lfit resid r_sclmeet) (scatter resid r_sclmeet)
graph twoway (lfit resid r_rlgdgr) (scatter resid r_rlgdgr)

scatter resid r_pplfair
scatter resid r_ppltrst
scatter resid r_hinct
scatter resid rr_health
scatter resid r_sclmeet
scatter resid r_eisced

stem e
gen happy_res=1
replace happy_res=0 if e<=-6
*only seven cases on the left tail equal to or lower than -6; doesn't seem worth it to remove them.

svy, subpop(happy_res): regress r_happy age28_39 age40_54 age55_64 age65_up female r_eisced r_hinct rr_health
living_w_part r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp

*___________________________________________________________________________________________________
*___________________________________________________________________________________________________

*APPENDIX B, SECOND INCOME VARIABLE
***using second measure of income, reverse coded

svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eisced
estimates store m1, title(demo)

svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eisced rr_hincfel
estimates store m2, title(inc)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eisced rr_hincfel h_2 h_3 h_4 h_5
estimates store m3, title(health)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eisced rr_hincfel h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr
estimates store m4, title(social)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eisced rr_hincfel h_2 h_3 h_4 h_5 living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
estimates store m5, title(trust)

estout m1 m2 m3 m4 m5, cells(b(star fmt(3)) se(par fmt(2))) ///
legend label varlabels(_cons constant) ///
stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC))

predict eisced_life, r
qnorm eisced_life

svy, subpop(female): regress age28_39 age40_54 age55_64 age65_up r_eisced rr_hincfel rr_health living_w_part
r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
predict eisced_life2, r
qnorm eisced_life2
sktest eisced_life2
*___________________________________________________________________________________________________
***LIFE SATISFACTION, INDIVIDUAL ADEQUACY OF INCOME USED, START***
***slowly adding in sections, regression
***Regressions
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr
estimates store m1, title(demo)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr incfeel_2 incfeel_3 incfeel_4
estimates store m2, title(inc)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr incfeel_2 incfeel_3 incfeel_4 h_2 h_3 h_4
h_5
estimates store m3, title(health)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr incfeel_2 incfeel_3 incfeel_4 h_2 h_3 h_4
h_5 living_w_part r_sclmeet r_rlgdgr

estimates store m4, title(social)
svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr incfeel_2 incfeel_3 incfeel_4 h_2 h_3 h_4
h_5 living_w_part r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
estimates store m5, title(trust)

estout m1 m2 m3 m4 m5, cells(b(star fmt(3)) se(par fmt(2))) ///
legend label varlabels(_cons constant) ///
stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC))

svy: regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr incfeel_2 incfeel_3 incfeel_4 h_2 h_3 h_4
h_5 living_w_part r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
linktest

svy, subpop(female): regress r_stflife age28_39 age40_54 age55_64 age65_up female r_eduyr incfeel_2 incfeel_3 incfeel_4
h_2 h_3 h_4 h_5 living_w_part r_sclmeet r_rlgdgr r_ppltrst r_pplfair r_pplhlp
predict poo, r
sktest poo

***LIFE SATISFACTION, INDIVIDUAL ADEQUACY OF INCOME USED, OVER***
translate mylog.smcl filename.log
translate mylog.smcl mylog.pdf
log close
