/* prepare the dataset */
use "X:/SDO/C2D2/constructingHESdatasets/allHES2000to2009BurnsSOTEST/hesipall1997to2009_subset_v7_exc_duplicates_plusdatedeathMar2013.dta" , clear

/* keeping only individuals older than 17 year old */
drop if startage<17 
drop if startage>6999 & startage<7999
drop if startage==.

/* keep only consider electives & emergencies */
keep if admimeth2<3

/* LABELING */
label var diagsumm "diagnosis"
label var patdiedinhos30 "in hospital mortality so days"
label var reopelec "reoperation elcetive"
label var reopemer "emergency reoperation"
label var APE_dummy "abdomino perineal surgery"
label var epidur "episode duration/length of stay"
label var startage "age"
label var startage2 "age categories"
label var protype "type of provider"
label var resgor "government office region of residence"
label var admimeth2 "admision method"
label var imd04i "income deprivation index"

/* MALE */
gen male=0
recode male 0=1 if sex==1

/* AGE CATEGORIES: Folowing NHS guidelines (more than 80% of cases in people 60+ */
gen over60=0
recode over60 0=1 if startage>=60
tab over60
tab over60 if diagsumm2==1

/* Region */
codebook resgor
encode resgor, gen(region)
codebook region

/* North east, north west, yorkshire nad humber */
gen north=0
recode north 0=1 if region<=3
tab north
/* Midlands */
gen midlands=0
recode midlands 0=1 if region>=4 & region<=5
tab midlands
/* London and East London */
gen london=0
recode london 0=1 if region>=6 & region<=7
tab london
/* South East and South West */
gen south=0
recode south 0=1 if region>=8 & region<=9
gen othr_reg=0
/* Other: Wales, Scotland, N. Ireland, Unknown, Foreign */
recode othr_reg 0=1 if region>=10
tab othr_reg

gen region2=0
recode region2 0=1 if north==1
recode region2 0=2 if midlands==1
recode region2 0=3 if london==1
recode region2 0=4 if south==1
recode region2 0=5 if othr_reg==1
tab region2

label define reg 1"North" 2"Midlands" 3"London" 4"South" 5"Other region"
label values region2 reg
tab region2

/**********************************************************************************************************/
/* TABLES */

/***TABLES distribution of income deprivation for every outcome****/
/*(for all sample)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9, by(imd04inc_qdv) statistics(count sum mean) format(%9.0g) 
/*(for colorectal cancer only)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9 if diagsumm2==1, by(imd04inc_qdv) statistics(count sum mean) format(%9.0g)
/*(for non cancer diagnosis)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9 if diagsumm2!=1,  by(imd04inc_qdv) statistics(count sum mean) format(%9.0g)

/***TABLES distribution of region for every outcome****/
/*(for all sample)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9, by(region2) statistics(count sum mean) format(%9.0g)
/*(for colorectal cancer only)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9 if diagsumm2==1, by(region2) statistics(count sum mean) format(%9.0g)
/*(for non cancer diagnosis)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9 if diagsumm2!=1,  by(region2) statistics(count sum mean) format(%9.0g)

/***TABLES distribution of age for every outcome ****/
/*(for all sample)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9, by(startage2) statistics(count sum mean) format(%9.0g)
/*(for colorectal cancer only)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9 if diagsumm2==1, by(startage2) statistics(count sum mean) format(%9.0g)
/*(for non cancer diagnosis)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9 if diagsumm2!=1,  by(startage2) statistics(count sum mean) format(%9.0g)

/***TABLES distribution gender old for every outcome ****/
/*(for all sample)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9, by(male) statistics(count sum mean) format(%9.0g)
/*(for colorectal cancer only)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9 if diagsumm2==1, by(male) statistics(count sum mean) format(%9.0g)
/*(for non cancer diagnosis)*/
tabstat patdiedinhos30 reopelec reopemer open_surgery APE_dummy epidur emer_readmissionTEST9 if diagsumm2!=1,  by(male) statistics(count sum mean) format(%9.0g)

/* AGE CATEGORIES AS BURNS et al. PAPER */
sum startage2
tab startage2
/*NOTE: Age over 80 has the lowest percentage so it will be based category*/
gen age17_54=0
recode age17_54 0=1 if startage2==1
gen age55_69=0
recode age55_69 0=1 if startage2==2
gen age70_79=0
recode age70_79 0=1 if startage2==3
gen over80=0
recode over80 0=1 if startage2==4

/*Describing Elix Comorbidities */
desc ynel*

/*Rename Elix Comorbidities */
rename ynel1 el_heartfail
rename ynel2 el_arrhythmia
rename ynel3 el_valvular
rename ynel4 el_pulmonarydisord
rename ynel5 el_peripheral
rename ynel6 el_mhypertension
rename ynel7 el_paralysis
rename ynel8 el_neurolog
rename ynel9 el_pulmdisease
rename ynel10 el_diabmild
rename ynel11 el_diabserious
rename ynel12 el_hypothyr
rename ynel13 el_renalfail
rename ynel14 el_liverdis
rename ynel15 el_ulcer
rename ynel16 el_AIDSHIV
rename ynel17 el_lymphoma
rename ynel18 el_metastatic
rename ynel19 el_tumornometast
rename ynel20 el_rheumartritis
rename ynel21 el_coagulopathy
rename ynel22 el_obesity
rename ynel23 el_weightloss
rename ynel24 el_fluiddisord
rename ynel25 el_bloodlossanemia
rename ynel26 el_defanemia
rename ynel27 el_alcohol
rename ynel28 el_drug
rename ynel29 el_psychoses
rename ynel30 el_depression
rename ynel31 el_shyperten

/*Some cross-tab to see the proportion of patients who died in hospital admited by emergency */
tab	admimeth2 patdiedinhos
tab patdiedinhos if admimeth2==2
tab patdiedinhos if admimeth2!=2

/*gen emergency admisions only*/
gen emergadmis=0
recode emergadmis 0=1 if admimeth2==2
tab emergadmis
label variable emergadmis "emergency admisions only"

/* generation private patients*
*NOTE: No info on private patients before 1999*/
gen privatepat=0
recode privatepat 0=1 if admincat==2
tab privatepat
label variable privatepat "private patient"

/* Keeping individuals with Bowel Cancer only*/
keep if diagsumm2==1

/* Years */
replace year=year+1990
keep if year > 1997
/* Two years average to smooth the trends */
gen year2=0
replace year2=1998 if year==1998 | year==1999
replace year2=2000 if year==2000 | year==2001
replace year2=2002 if year==2002 | year==2003
replace year2=2004 if year==2004 | year==2005
replace year2=2006 if year==2006 | year==2007
replace year2=2008 if year==2008 | year==2009

label variable year2 "two years average, 1998=98_99, 2000=00_01..."

/*Sum of my dependent variables */
sum epidur patdiedinhos reopelec emer_readmissionTEST9 open_surgery APE_dummy

/*Sum of independent variables */
sum b5.imd04inc_qdv ndiags male el_* age17_54 age55_69 age70_79 emergadmis
