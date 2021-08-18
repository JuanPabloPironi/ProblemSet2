**************************
*  Applied Economics     *
*        Week #2         *
*     Least Squares      *
**************************

// Remember to open a log-file:

*log using week2, replace

************************
*** A "real" example ***
************************

global main "/Users/magibbons/Google Drive/Aplicada/2021/Tutorial2"
global output "$main/output"
global input "$main/input"

use "$input/russia.dta", clear
cd "$output"

* Summary Statistics tables 
tabstat gender econrk powrnk resprk satlif satecc highsc belief monage obese cmedin hprblm hosl3m htself wtchng evalhl operat hattac smokes alclmo height waistc hipsiz hhpres, save statistics(mean sd min max) columns(statistics)

* Save matrix
matrix A=r(StatTotal)
* Transpose
mat A=A'

* Select certain rows (not necessary in this case...) and save as matrix AA
matselrc A AA, r(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)

frmttable using Table1, varlabels statmat(AA) sdec(2,2,0,0) ctitles("Variable name","Mean", "SD", "Minimum","Maximum") replace 

frmttable using Table1, varlabels tex statmat(AA) sdec(2,2,0,0) ctitles("Variable name","Mean", "SD", "Minimum","Maximum") replace 

* Another option
estpost tabstat gender econrk powrnk resprk satlif satecc highsc belief monage obese cmedin hprblm hosl3m htself wtchng evalhl operat hattac smokes alclmo height waistc hipsiz hhpres, save statistics(mean sd min max) columns(statistics)

esttab . using example.tex, replace frag main(mean) aux(sd) nostar unstack noobs nonote label

esttab . using example.rtf, replace main(mean) aux(sd) nostar unstack noobs nonote label
		
* Suppose that you want to explain the Health Self-Evaluation indexes ("evalhl"; the larger the healthier) using the following variables: 
reg evalhl monage obese smokes

* Notice that Stata automatically puts the constant. If you wanted to exclude it, you would have to enter "nocons" as option: 
reg evalhl monage obese smokes, nocons

/* As any other command in Stata, "regress" can be applied to a subset of the observations. 
Suppose you want to run two separate regressions, one for males and the other for females, respectively:*/
reg evalhl monage obese smokes if gender==1
reg evalhl monage obese smokes if gender==0

***************************
*** Regressions' output ***
***************************
 
/* You need to show the regressions in tables similar to those utilized in most economics papers 
(e.g. one column per specification, and standard errors in parentheses). 
Use then the command "outreg". As it is not a "default" command in Stata, you need to install it first:*/
search outreg2

/* Then press "click here to install". It is important to set the folder you are working with as working 
directory so the new files are written there.*/ 

* We run a regression:
reg evalhl monage obese smokes satlif totexpr

/* And then we create an Excel or Word file with the output as a table in the folder chosen as “Working directory”:*/
outreg2 using regresion, excel
outreg2 using regresion.doc, word dec(3) replace

/* We can use the option "replace" to indicate to Stata that if the file already exists, then it must be replaced.
By default "outreg" shown in parentheses the t-values and puts one asterisk if the coefficient is significative at 
the 10%, two asterisks if it is significative at the 5%, and three asterisks if it is significative at the 1%.
The command can also show various regressions in the same table (as columns). We must add regression outputs using 
the option "append" instead of "replace". 
Let's run three regressions: one with only "monage" and "obese" as explanatory variables; another with only "smokes", 
"satlif" y "totexpr"; and finally a regression with all them.*/
reg evalhl monage obese 
outreg2 using regresion, excel replace
reg evalhl smokes satlif totexpr
outreg2 using regresion, excel append
reg evalhl monage obese smokes satlif totexpr
outreg2 using regresion, excel append

/* If you run a do file, sometimes Stata doesn’t have time to open, write and close the file before the next outreg 
command begins so it detects as read-only file. For this to not happen it is usefull to the the “set more on” option 
and/or use the sleep command to give Stata some tome to process.
If you do not want to show the coefficient for a set of dummy variables, you will find the command "areg" very useful.*/
reg evalhl monage obese smokes satlif totexpr marstat*
reg evalhl monage obese smokes satlif totexpr i.belief
areg evalhl monage obese smokes satlif totexpr, absorb (belief)

/*Let's create a table with two types of standard errors (homoscedastic and robust in this case, robust and clustered generally. */

reg evalhl monage obese smokes satlif totexpr belief
matrix b_se = get(_b)', vecdiag(cholesky(diag(vecdiag(get(VCE)))))'
mat list b_se

reg evalhl monage obese smokes satlif totexpr belief, robust
matrix b_r = vecdiag(cholesky(diag(vecdiag(get(VCE)))))'
mat list b_r

matrix b_se_r=b_se,b_r
mat list b_se_r

local bc = rowsof(b_se_r)
matrix p = J(`bc',2,.)
forvalues y = 1/`bc' {
mat p[`y',1]=b_se_r[`y',1] / b_se_r[`y',2]
mat p[`y',2]=b_se_r[`y',1] / b_se_r[`y',3]
}

local bc = rowsof(b_se_r)
matrix stars2 = J(`bc',3,0)
forvalues k = 1/`bc' {
matrix stars2[`k',2] = ((abs(p[`k',1]) >= 1.645) + (abs(p[`k',1]) >= 1.96) + (abs(p[`k',1]) >= 2.32))
matrix stars2[`k',3] = ((abs(p[`k',2]) >= 1.645) + (abs(p[`k',2]) >= 1.96) + (abs(p[`k',2]) >= 2.32))
}
matrix list stars2

*frmttable using Table2, statmat(b_se_r) sdec(3) substat(2) brackets((.)\[.]) annotate(stars2) asymbol(*,**,***) replace

frmttable using Table2, statmat(b_se_r) sdec(3) substat(2) sq annotate(stars2) asymbol(*,**,***) replace

*************
*** Tests ***
*************

/* After each estimation Stata automatically provides t-tests (for linear regressions) or z-tests 
(for nonlinear models) of the null hypothesis whether the coefficients are zero. Notwithstanding, 
other hypotheses on the coefficients can be tested.
For instance, using the command "test" (after the regression was run) 
you can test whether the effect of being obese equals -0.05:*/
reg evalhl monage obese smokes gender, robust
test obese=-0.05

/* We can test hypotheses that involve more than one variable. For instance, we can test if the 
coefficients on "smokes" and "obese" are both null, or if the sum of the coefficients on "obese" and "gender" equals 1:*/
test smokes obese
test obese + gender == 1

/* Here we would like to compare sample means with population means. I will make up zones and population means for this purpose. */

* We first generate fake zones:
egen zones=cut(site), at(0,20,40,60,80,100,120,140,160,180)
* We generate its dummy variables:
tab zones, gen (dzone)

* We generate an empty matrix with the number of zones+1 as the number of rows and 6 columns (Zone, mean_sample, N_sample, mean_pop, difference, p-value). You could have 8 if you have the population SD.
mat def A=J(9,6,.)
* We define a matrix with the population values and the zone numbers.
mat in B = (0.525, 0.580, 0.559, 0.550, 0.580, 0.550, 0.500, 0.507 \ 1, 2, 3, 4, 5, 6, 7, 8)
* We transpose it (we could have defined B initially as columns)
mat B=B'

* Loop for each zone:
forv y=1/8 {
local z =B[`y',1]
local w =B[`y',2]
ttest gender = `z' if dzone`w'==1
* Zones
mat A[`y',1]= B[`y',2]
* Population Mean
mat A[`y',3]= B[`y',1]
* N
mat A[`y',2]= `r(N_1)'
* Sample Mean
mat A[`y',4]= `r(mu_1)'
* Difference
mat A[`y',5]= A[`y',4]-A[`y',3]
* P-value
mat A[`y',6]=`r(p)'
}
local y 9
ttest gender = 0.50
* Population Mean
mat A[`y',3]= 0.50
* N
mat A[`y',2]= `r(N_1)'
* Sample Mean
mat A[`y',4]= `r(mu_1)'
* Difference
mat A[`y',5]= A[`y',4]-A[`y',3]
* P-value
mat A[`y',6]=`r(p)'

* We save the number of rows in a local:
local bc = rowsof(A)

* We generate a matrix with the same number of rows as the original matrix and 5 columns:
matrix stars2 = J(`bc',5,0)
forvalues k = 1/`bc' {
* We add +1 if each of the conditions hold. Then, if the p-value is less than 0.01, the cell we have a 3.
matrix stars2[`k',5] = ((A[`k',6] <= 0.1) + (A[`k',6] <= 0.05) + (A[`k',6] <= 0.01))
}
matrix list stars2

matselrc A A, c(1 2 3 4 5)

* If you would like to save is for Latex, add .tex to the filename and frag in the options (the frag options does not include the \begin{document} in the tex file.
frmttable using Table2, statmat(A) sdec(0,0,3,3,3) annotate(stars2) asymbol(*,**,***) sfmt(f) ctitles("Zone","Sample Observations", "Population proportion", "Sample Proportion", "Difference") replace


*********************************
*** Predictions and residuals ***
*********************************

/* After every estimation command some "predicted" values can be calculated: fitted values, residuals, influence statistics, etc. 
Those values will be stored in a new variable using the command "predict". For instance, in the last model the predictions are:*/
reg evalhl monage obese smokes gender, robust
gen yhat_hand=_b[_cons]+_b[monage]*monage+_b[obese]*obese+_b[smokes]*smokes+_b[gender]*gender
predict yhat
corr yhat yhat_hand

* Enter "browse evalhl yhat" to appreciate the fit of the model. 
br yhat evalhl

* The residual can be obtained:
gen res_hand=evalhl-[_b[_cons]+_b[monage]*monage+_b[obese]*obese+_b[smokes]*smokes+_b[gender]*gender]
predict res, residual
corr res_hand res

*******************************
*** Partial regression plot ***
*******************************

/* The command "avplot" graphs the relationship between the dependent variable and one of the explanatory 
variables conditional on the rest of the regressors. It is very useful to identify outliers. 
We will collapse the database to obtain the means for some variables within each geographical site 
(enter "tab site" to see the geographical distribution of people). We will use "preserve and restore" to avoid loading the database again: */
preserve
collapse (mean) satlif totexpr satecc powrnk, by(site)

* We run a regression of life satisfaction on three variables:
reg satlif totexpr satecc powrnk, robust

* Using the command "avplot", we show the partial relationship between "satecc" and "satlif", identifying each geographical site:
avplot satecc, mlabel(site) name(g1,replace)

* By hand
regress satlif totexpr powrnk

predict satlifNetrest, r

regress satecc totexpr powrnk

predict sateccNetrest, r

scatter satlifNetrest sateccNetrest, mlabel(site) || lfit satlifNetrest sateccNetrest, title("Life Satisfaction by Econ Satisfaction" "both net of Expenditures and Power Rank") legend(off) xtitle(Econ Satisf net of Expenditures and Power Rank) ytitle(Life Satisf net of Expenditures and Power Rank) name(g2, replace) 

/* For example, in the avplot for satecc, the graph shows satlif by satecc after both satlif and satecc have been adjusted for all other predictors in the model.  The line plotted has the same slope as the coefficient for satecc. This plot shows how the 
observations influence the coefficient.  You can see how the regression line is tugged downwards trying to fit through the extreme values of site 25 and 26.*/

* Let’s check the difference when these observations are dropped.
drop if site==25 | site==26
reg satlif totexpr satecc powrnk, robust
avplot satecc, mlabel(site)
restore

**********************************
*** Normality of the residuals ***
**********************************

/* The multiple regression model does not require normality of the residuals to make inference with large samples 
(depending on the version of the limit central theorem used, it only needs error terms i.i.d.). 
Nonetheless, normality is needed to make inference in small samples.
After running a regression, we can predict the residuals:*/
use "$input/russia.dta",clear
reg waistc monage height hipsiz gender
predict res, residual

* We can graph the nonparametric estimate of the density function of the residuals using Kernels:
kdensity res, norm

/* The option "norm" graphs a Gaussian bell with the sample mean and variance. 
We could also use the commands "qnorm" and "pnorm" to evaluate graphically the normality of the residuals.*/ 
pnorm res
qnorm res

/* Some tests for normality, such as the Shapiro-Wilk and Shapiro-Francia ("swilk"), can also be useful.  
The null hypothesis is that the variable is normally distributed.*/
swilk res

************************************
*** Tests for heteroskedasticity ***
************************************

/* The command "imtest" performs the White's test, and the command "hettest" performs the Breusch-Pagan's test. 
In both tests the null hypothesis is whether the variance of residuals is homogeneous.
Nevertheless, you should be very careful. These tests are pretty sensitive to the assumptions of the model 
(for instance, they suppose normality for the error term).*/
estat imtest 
estat hettest


****************************
*** "By hand" regression ***
****************************

* Since we will use mathematic elements, we need to maximize the room reserved for those objects:
set matsize 800

/* As Stata do not allow for more than 800 rows or columns for matrixes, it would be impossible 
to work directly with X or Y (as they have +2600 rows). But there is a rather simple trick: the command "mat accum". 
It executes an intermediate step, X'X, which creates a "small" matrix. 
First we have to eliminate the observations with missing values in the variables that will be included in the model:*/
drop if evalhl==. | monage==. | obese==. | smokes==.

* Then we run the regression in the "traditional" way:
reg evalhl monage obese smokes
sum monage if e(sample)==1

* Let's begin with calculating X'X and storing it in the matrix "XpX":
mat accum XpX = monage obese smokes

* You can see the result entering:
mat list XpX

* Calculate X'Y and store it in the matrix "XpY":
mat vecaccum YpX = evalhl monage obese smokes
mat list YpX

* Then transpose it (using an aphostrophe) to obtain X'Y:  
mat XpY = YpX'
mat list XpY

* We can get beta using the above formula:
matrix beta = invsym(XpX)*XpY
mat list beta

* To compare:
reg evalhl monage obese smokes

***********************************************
*** Creating Random Data and Random Samples ***
***********************************************

/* Before starting the exercises on least squares, you must learn how to create random samples. 
To "generate" observations from nothing, you must use "set obs":*/
clear

set seed 1234

set obs 100
br

/* If you want to generate a variable "number" containing random numbers distributed uniformly 
between 0 and 1, enter: */
gen number=uniform()
br

/* The function "uniform()" returns uniformly distributed pseudorandom numbers on the interval [0,1). 
Entering either "browse" or "list" you will be able to see the generated observations. 
If you want to generate uniformly distributed pseudorandom numbers on the interval [a,b), 
you must enter "gen var=uniform()*(b-a)+a". 
For instance, to generate numbers on the interval [3,7): */
set seed 1234
gen number_1=uniform()*4+3
br

* Check the desired properties (max, min and mean) entering "sum number_1". 
sum number_1

/* If you want to generate integers, then you must multiply the numbers by a multiple of ten 
and then truncate them towards zero (using "int"). For instance, to create integers between 0 and 9:*/
set seed 1234
gen number_2=int(uniform()*10)
br
sum number_2

/* Note 1: If you want to see what the command is doing, you can try with this lines:
clear
set obs 10
set seed 1234
gen number_3=uniform()
set seed 1234
gen number_3_10=(uniform()*10)
set seed 1234
gen number_3_int=int((uniform()*10))
list
*/

/* If you want to generate random numbers with different distributions, you have to evaluate the inverse 
density function at "uniform()". For instance, using "invnormal(uniform())" returns normally distributed 
random numbers with mean 0 and standard deviation 1. Sum and multiply accordingly in order to obtain 
different means and deviations.*/
 
/* If you want to repeat the same "succession" of pseudo-random numbers each time that you execute a do-file, 
then you must initialize the "seed" using "set seed" */

// Generating ten random numbers twice:
clear
set obs 10
gen n=uniform()
gen n1= uniform()
list

// Repeating the process using the same seed:
clear
set obs 10
set seed 1234
gen n=uniform()
set seed 1234
gen n1= uniform()
list

*Remember always to close the log-file:
log close
