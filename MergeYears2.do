/**********************************MSc thesis code- Effect of Sanctions on LFP of women in Iran********************************************
Using the cleaned data of Household expenditure and Income survey (HEIS) of families in Iran for the period of 2010-2012, 
I constructed a panel data for three consequitive years and ran diferent logit and probit regressions. The dependent 
variable is a binary variable indicating whether the woman is working or not. Treatment group are those women whose husbands 
worked in manufacturing sector before sanctions and the control group are those whose husbands worked in Service sector. 
During sanctions, Manufacturing were hit more than service sector according to the evidence and statistics.
So, we could employ the method of DID to compare treatment and control group before and after sanctions. 
I have added the three necessary data files used in this piece of code
*****************************************************************************************************************************/ 

/***********prepare data for merging for panel construction***************/
forvalues i=10(1)12{
	use "Individualinfo`i'.dta", replace
	ren gender gender`i'
	ren age age`i'
	ren pre_primary pre_primary`i'
	ren primary primary`i'
	ren secondary secondary`i'
	ren upper_secondary upper_secondary`i'
	ren tertiary tertiary`i'
	ren LFP LFP`i'
	ren CensusMonth census_month`i'
	ren Weight weight`i'
	ren husband_sector husband_sector`i'
	ren husband_income husband_income`i'
	ren Year year`i'
	ren sector sector`i'
	ren activity activity`i'
	replace year`i' = 1
	ren edu education`i'
	ren occupation occupation`i'
	egen household_size = total(num!=0), by(Address)
	if `i' >= 11{
		ren subsidy subsidy`i'
	}
	else{
		gen subsidy10 = 0
		}
	keep Address household_size num relation gender`i' age`i' pre_primary`i' primary`i' secondary`i' upper_secondary`i' tertiary`i' LFP`i' ///
		census_month`i' weight`i' husband_sector`i' husband_income`i' province urban year`i' sector`i' activity`i' education`i' occupation`i' subsidy`i'
	drop if missing(Address) | missing(age`i') | missing(LFP`i') | missing(census_month`i') | missing(weight`i') 
	save "ReadyIndividualinfo`i'.dta" , replace

}
/****************************************************/
/****Merge Data**************************************/
clear 
cls
use "ReadyIndividualinfo10.dta" , replace
merge 1:1 Address num using "ReadyIndividualinfo11.dta"
drop _merge
merge 1:1 Address num using  "ReadyIndividualinfo12.dta"
drop _merge
/*********************************************************/

//delete those individuals who have just one observation because panel construction is impossible
egen attrition = total(missing(year10)+missing(year11) + missing(year12)), by (Address)
drop if attrition >=2   


quietly bysort Address relation:  gen dup = cond(_N==1,0,_n)
drop if dup > 1 

forvalues i=10(1)12{
	destring occupation`i', replace
	egen husband_occupation`i' = total(occupation`i'*(relation==1 & gender`i'==1)), by(Address)
	replace husband_occupation`i' = husband_occupation`i'*(relation==2 & gender`i'==2)
}

//husband_sctor=1 or 2 is manufacturing and service sector - we delete other sectors because not treated by sanctions	
drop if husband_sector10==0 | husband_sector10==3 | ///
	(husband_sector11==0 & census_month11 <6) | (husband_sector11==3 & census_month11 <6)

	
//delete those individuals whose age change more than two years in consecutive years - data is not consistent in this case
gen not_matched_10_11 = cond(age11 >age10+2 | age11 < age10 | gender11!=gender10 ,1 , 0)
gen not_matched_11_12 = cond(age12 > age11+2 | age12 < age11 | gender12!=gender11, 1, 0)
gen not_matched_10_12 = cond(age12 > age10+3 | age12 < age10+1 | gender12!=gender10, 1, 0)

//keep focus group which is married women - gender =2 (woman) and relation=2(wife)
keep if (relation==2 & gender10==2) | (relation==2 & gender11==2 & census_month11 < 6)

//reshape wide data to long for regression
reshape long age gender subsidy pre_primary primary  activity secondary upper_secondary tertiary LFP census_month ///
	weight husband_income husband_sector sector education husband_occupation, i(Address num) j(Year)

//drop missing years
drop if missing(age)
drop if Year==10 & not_matched_10_11 & not_matched_10_12
drop if Year==11 & not_matched_11_12 & not_matched_10_11
drop if Year==12 & not_matched_11_12 & not_matched_10_12

//identify the year which sanctions hit which is the second half of 2011
gen sanction_year = cond(Year==12 | (Year==11 & census_month>=6), ///
1,0)

//construct treatment group based on husband's sector
gen treatment_group = cond(husband_sector==1 & sanction_year==0,1,0) //make treatment from husband sector before sanction
gen treated = cond(husband_sector==1 & sanction_year==1 ,1,0)

//delete repeated observations
quietly bysort Address relation Year:  gen dup2 = cond(_N==1,0,_n)
drop if dup2 > 1 

//delete if there are not one observation before sanction and one after sanction for an individual
replace year10 = 1 
egen num_observations = total(year10), by(Address)
egen num_sanction_years = total(sanction_year==1), by(Address)	
drop if num_observations==2 & (num_sanction_years==0 | num_sanction_years==2)

//generate age^2
gen age2 = age^2

//generate clusters which is the product of huband's sector and year- we should use clustred standard errors
replace Year=2010 if Year==10 
replace Year=2011 if Year==11
replace Year=2012 if Year==12
gen cluster_sector = 3*(Year-1)+husband_sector
gen cluster_occupation  = husband_occupation * Year


//make season and average subsidy 
gen season = ceil(census_month /3)
replace season = (Year-1)*4+season	
gen avg_subsidy = subsidy/household_size

//summary statistics for main variables
tabstat age education husband_income urban LFP [aweight=weight] , by (treatment_group) statistics(mean sd)

/********************************************Main Regressions********************************************************************/
/***probit for different education levels****/
//primary
probit LFP  i.province treatment_group sanction_year treated ///
   age age2 subsidy household_size husband_income urban if primary==1 [pweight=weigh] ///
  ,cluster(cluster_occupation) 
margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size husband_income urban) post
outreg2 using my_result_edu.doc, replace ctitle(Primary) addtext(Province FE, YES)

//secondary
probit LFP  i.province treatment_group sanction_year treated ///
   age age2 subsidy household_size husband_income urban if secondary==1 [pweight=weigh] ///
  ,cluster(cluster_occupation) 
margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size husband_income urban) post
outreg2 using my_result_edu.doc, append ctitle(Secondary) addtext(Province FE, YES)

//upper secondary
probit LFP  i.province treatment_group sanction_year treated ///
   age age2 subsidy household_size husband_income urban if upper_secondary==1 [pweight=weigh] ///
  ,cluster(cluster_occupation) 
margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size husband_income urban) post
outreg2 using my_result_edu.doc, append ctitle(Upper Secondary) addtext(Province FE, YES)

//tertiary
probit LFP  i.province treatment_group sanction_year treated ///
   age age2 subsidy household_size husband_income urban if tertiary==1 [pweight=weigh] ///
  ,cluster(cluster_occupation) 
margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size husband_income urban) post
outreg2 using my_result_edu.doc, append ctitle(Tertiary) addtext(Province FE, YES)


/***probit for 5 different income levels****/
xtile quant = husband_income [pweight=weight], nq(5)
forv i=1/5{
	probit LFP  i.province treatment_group sanction_year treated ///
   age age2 subsidy household_size primary secondary upper_secondary tertiary husband_income urban [pweight=weigh] if quant==`i' ///
  ,cluster(cluster_occupation) 
  margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size ///
	primary secondary upper_secondary tertiary husband_income urban) post
	outreg2 using my_result_income.doc, append ctitle(Quintile`i') addtext(Province FE, YES)
}

  probit LFP  i.province treatment_group sanction_year treated ///
   age age2 subsidy household_size primary secondary upper_secondary tertiary urban [pweight=weigh] if quant==1 ///
  ,cluster(cluster_occupation) 
  margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size ///
	primary secondary upper_secondary tertiary urban) post

//1-probit without control
probit LFP   treatment_group sanction_year treated [pweight=weigh] ,cluster(cluster_occupation)  
margins, dydx(treatment_group sanction_year treated) post
outreg2 using my_result.doc, replace ctitle(Probit without controls) addtext(Province FE, YES)

//2-probit with control
probit LFP  i.province treatment_group sanction_year treated ///
   age age2 subsidy household_size primary secondary upper_secondary tertiary husband_income urban [pweight=weigh] ///
  ,cluster(cluster_occupation)  
margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size ///
 primary secondary upper_secondary tertiary husband_income urban) post
outreg2 using my_result.doc, append ctitle(Probit) addtext(Province FE, YES)

//3 - logit
logit LFP  i.province treatment_group sanction_year treated ///
   age age2 subsidy household_size primary secondary upper_secondary tertiary husband_income urban [pweight=weigh] ///
  ,cluster(cluster_occupation)  
margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size ///
 primary secondary upper_secondary tertiary husband_income urban) post
outreg2 using my_result.doc, append ctitle(Logit) addtext(Province FE, YES)


//4-OLS
reg LFP  i.province treatment_group sanction_year treated ///
   age age2 subsidy household_size primary secondary upper_secondary tertiary husband_income urban [pweight=weigh] ///
  ,cluster(cluster_occupation)  
margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size ///
 primary secondary upper_secondary tertiary husband_income urban) post
outreg2 using my_result.doc, append ctitle(OLS) addtext(Province FE, YES)

drop if num_observations==3 & Year==2011

//5-Probit with few observations
probit LFP  i.province treatment_group sanction_year treated ///
   age age2 subsidy household_size primary secondary upper_secondary tertiary husband_income urban [pweight=weigh] ///
  ,cluster(cluster_occupation)  
margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size ///
 primary secondary upper_secondary tertiary husband_income urban) post
outreg2 using my_result.doc, append ctitle(Probit) addtext(Province FE, YES)


margins, dydx(treatment_group sanction_year treated age age2 subsidy household_size ///
 primary secondary upper_secondary tertiary husband_income) post
outreg2 using my_result2.doc, append ctitle(Rural) addtext(Province FE, YES)
/*********************************************End of Regressions****************************************************************************/
