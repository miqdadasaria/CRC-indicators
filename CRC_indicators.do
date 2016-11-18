cd "X:/SDO/HEPI/Data/HES Data/CRC/"
log using "Output/HEPI_CRC.smcl", replace
clear all
set memory 1024m

set more off

do "Do Files/load_data.do"
   
/***** Calculate Inequality Indicators *****/

/*In this do file, you will find the analysis for the main inequality indicators for CRC surgery:
* -Unadjusted time trends (Graph with All quitiles without Confidence Intervals)
* -Absolute Adjusted time trends Probit Analysis (Graph for Q1-Q5 and Confidence Intervals )
* -Relative Ajusted time trends Probit Analysis (Graph for (Q1-Q5)/Q5 and Confidence Intervals )*/

/* define mata function to calculate absolute and relative inequality indices 
 * for each year along with appropriate confidence intervals */
capture mata mata drop calcIndices()
mata:
real matrix calcIndices(real matrix B, real matrix varB, real matrix YEARS)
{
real matrix SE 
real matrix ABS
real matrix ABS_SE
real matrix ABS_LCI
real matrix ABS_UCI
real matrix REL
real matrix REL_SE
real matrix REL_LCI
real matrix REL_UCI
real matrix RESULTS
/* SE are just the square roots of the variances */
SE = sqrt(varB)
/* Calculate absolute difference IMD04INC_Q1-IMD04INC_Q5 */
ABS = (B[,1] - B[,5])
ABS_SE = sqrt(SE[,1]:^2 + SE[,5]:^2)
ABS_SE
ABS_LCI = ABS - 1.96*ABS_SE
ABS_UCI = ABS + 1.96*ABS_SE
/* Calculate relative difference (IMD04INC_Q1-IMD04INC_Q5)/IMD04INC_Q5 */
REL = ABS:/B[,5]
REL_SE = REL:*sqrt( (ABS_SE:/ABS):^2 + (SE[,5]:/B[,5]):^2 )
REL_SE
REL_LCI = REL - 1.96*REL_SE
REL_UCI = REL + 1.96*REL_SE
/* Combine to form convenient results matrix to return to stata */
RESULTS = YEARS,ABS,ABS_LCI,ABS_UCI,REL,REL_LCI,REL_UCI
return(RESULTS)
}
end

global indicator1 "patdiedinhos30"
global indicator_desc1 "In hospital mortality (30 days) Period 1998-2009:"
global indicator_prefix1 "IHM" 

global indicator2 "open_surgery"
global indicator_desc2 "Open Surgery Period 2000-2009:"
global indicator_prefix2 "OS" 

global indicator3 "emer_readmissionTEST9"
global indicator_desc3 "Emergency Re-admissions Period 2000-2009:"
global indicator_prefix3 "ERA"

global indicator4 "APE_dummy"
global indicator_desc4 "APE Surgery Period 2000-2009:"
global indicator_prefix4 "APE"

global indicator5 "epidur"
global indicator_desc5 "Length of Stay Period 2000-2009:"
global indicator_prefix5 "LOS"

forvalues j = 1/4 {
	local indicator = "${indicator`j'}" 
	local indicator_prefix = "${indicator_prefix`j'}" 
	local indicator_desc = "${indicator_desc`j'}"	 

	/*  Unadjusted time trends  */
	table imd04inc_qdv year2, c(m `indicator')
	sort year2 imd04inc_qdv
	capture drop `indicator'_byimdyear
	by year2 imd04inc_qdv : egen `indicator'_byimdyear = mean(`indicator') 
	graph twoway line `indicator'_byimdyear year2 if imd04inc_qdv==1, lpattern(solid)   /*
	   */ || line `indicator'_byimdyear year2 if imd04inc_qdv==2, lpattern(shortdash)   /*
	   */ || line `indicator'_byimdyear year2 if imd04inc_qdv==3, lpattern(dash_dot)   /*
	   */ || line `indicator'_byimdyear year2 if imd04inc_qdv==4, lpattern(longdash)   /*
	   */ || line `indicator'_byimdyear year2 if imd04inc_qdv==5, lpattern(shortdash_dot_dot) /*
	   */ title( "`indicator_desc'""unadjusted time trends")  /* 
	   */ ytitle("Probability") /*
	   */ xtitle("Year (two years average)")
	graph save Graph "Output/`indicator_prefix' Unadjusted.gph", replace
	graph export "Output/`indicator_prefix' Unadjusted.png", replace
	window manage close graph

	/*  Calculate adjusted time trends using probit regressions for each time period */
	capture matrix drop B
	capture matrix drop VAR
	capture matrix drop YEARS
	matrix YEARS = 1998\2000\2002\2004\2006\2008

	forvalues i=1998(2)2008 {
		probit patdiedinhos ib5.imd04inc_qdv  male el_* age17_54 age55_69 age70_79 emergadmis if year2==`i', robust
		/* calculate marginal impact of IMD 2004 income quintiles */
		margins imd04inc_qdv
		matrix B = (nullmat(B)\r(b))
		matrix VAR = (nullmat(VAR)\vecdiag(r(V)))
	}

	matrix rownames B = 1998 2000 2002 2004 2006 2008
	matrix colnames B = IMD04INC_Q1 IMD04INC_Q2 IMD04INC_Q3 IMD04INC_Q4 IMD04INC_Q5 
	matrix rownames VAR = 1998 2000 2002 2004 2006 2008
	matrix colnames VAR = IMD04INC_Q1 IMD04INC_Q2 IMD04INC_Q3 IMD04INC_Q4 IMD04INC_Q5 

	/* switch to mata to calculate absolute and relative inequalities and standard errors */
	mata: st_matrix("`indicator_prefix'_RESULTS", calcIndices(st_matrix("B"),st_matrix("VAR"),st_matrix("YEARS")))

	matrix colnames `indicator_prefix'_RESULTS = YEAR `indicator_prefix'_ABS `indicator_prefix'_ABS_LCI `indicator_prefix'_ABS_UCI `indicator_prefix'_REL `indicator_prefix'_REL_LCI `indicator_prefix'_REL_UCI
	matrix list `indicator_prefix'_RESULTS
	svmat `indicator_prefix'_RESULTS, names(col)

	graph twoway line `indicator_prefix'_ABS YEAR, lpattern(solid)   /*
	   */ || line `indicator_prefix'_ABS_LCI YEAR, lpattern(shortdash)   /*
	   */ || line `indicator_prefix'_ABS_UCI YEAR, lpattern(shortdash)   /*
	   */ title("`indicator_desc'""Absolute inequalities (adjusted gaps)")  /* 
	   */ ytitle("Probability Difference") /*
	   */ xtitle("Year (two years average)")
	   
	graph save Graph "Output/`indicator_prefix' Absolute.gph", replace
	graph export "Output/`indicator_prefix' Absolute.png", replace

	window manage close graph

	graph twoway line `indicator_prefix'_REL YEAR, lpattern(solid)   /*
	   */ || line `indicator_prefix'_REL_LCI YEAR, lpattern(shortdash)   /*
	   */ || line `indicator_prefix'_REL_UCI YEAR, lpattern(shortdash)   /*
	   */ title("`indicator_desc'""Relative inequalities (adjusted gaps)")  /* 
	   */ ytitle("Probability Difference") /*
	   */ xtitle("Year (two years average)")
	  
	graph save Graph "Output/`indicator_prefix' Relative.gph", replace
	graph export "Output/`indicator_prefix' Relative.png", replace
	window manage close graph  
 
}

log close
