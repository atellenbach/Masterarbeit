//gii

clear all
cd "data"

import delimited using "gii_gdi\gii.csv"

rename v1 alpha3

merge m:1 alpha3 using iso_uebersetzungstabelle.dta 

drop if _merge==2 | _merge==1
drop _merge
drop v4
keep if inrange(v2,2011,2015)

rename v3 gii_
rename v2 dateyr

local new=_N+1
set obs `new'
replace dateyr=2015 in `new'
replace alpha3="ARG" in `new'
replace cntry="AR" in `new'

sort alpha3 dateyr

sa gii_gdi.dta, replace

clear all

//alle anderen und merge

global datalist "adobirth_gii bip_female_gdi bip_male_gdi educ_female_gii educ_male_gii eeducyrs_female_gdi eeducyrs_male_gdi elife_female_gdi elife_male_gdi hdi_female_gdi hdi_male_gdi lfp_female_gii lfp_male_gii meducyrs_female_gdi meducyrs_male_gdi mmratio_gii seatsparl_gii gdi"
foreach i in $datalist {
	clear all

	import delimited using "gii_gdi/`i'.csv"

	rename v1 alpha3

	merge m:1 alpha3 using iso_uebersetzungstabelle.dta //using only: TW

	drop if _merge==2 | _merge==1
	drop _merge
	capture drop v4
	capture drop v5
	keep if inrange(v2,2011,2015)

	rename v3 `i'
	rename v2 dateyr
	sort alpha3 dateyr

	sa "gii_gdi/`i'.dta", replace
	
	clear all
	use gii_gdi.dta
	merge 1:1 alpha3 dateyr using "gii_gdi/`i'.dta"
	drop _merge
	sa gii_gdi.dta, replace
}

