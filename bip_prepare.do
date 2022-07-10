clear all

cd "data"

import excel "BIPperCapitaPPP_Data_Extract_From_World_Development_Indicators.xlsx", first case(l)

drop seriescode seriesname yr1990 yr2000 yr2016 yr2017 yr2018 yr2019 yr2020
drop in 267/271
foreach i of var yr2011 yr2012 yr2013 yr2014 yr2015 {
	replace `i'="" if `i'==".."
}
destring yr2011 yr2012 yr2013 yr2014 yr2015, replace

//TODO: Taiwan: not listed seperately from China "Taiwan"

keep if inlist(countryname, "Argentina", "Australia", "Austria", "Bulgaria", "Canada", "Chile", "China", "Croatia", "Czech Republic") | inlist(countryname, "Denmark", "Finland", "France", "Germany", "United Kingdom", "Iceland", "India", "Ireland") | inlist(countryname, "Israel", "Japan", "Latvia", "Lithuania", "Mexico", "Norway", "Philippines", "Poland", "Portugal") | inlist(countryname, "Russian Federation", "Slovenia", "Slovak Republic", "South Africa", "Korea, Rep.", "Spain", "Sweden", "Switzerland") | inlist(countryname, "Turkey", "United States", "Venezuela, RB", "Belgium", "Hungary", "Netherlands")


//Venezuela: 2011 (last available)
//folgende nicht im codebook: "Belgium", "Hungary", "Netherlands", var dateyr aus main als Ersatz, aber wieso??

//bip pro person

reshape long yr, i(countryname) j(dateyr)

rename yr bip

sum bip if countrycode=="VEN"
replace bip=r(mean) if countrycode=="VEN"

//Bip als mittel pro land
// gen bip=.
//
// replace bip=yr2011 if inlist(countryname, "Bulgaria", "Venezuela, RB")
//
// replace bip=yr2012 if inlist(countryname, "Chile", "China", "Czech Republic", "Finland", "France", "Germany", "United Kingdom", "Japan", "Norway") | inlist(countryname, "Philippines", "Russian Federation", "Slovenia", "Slovak Republic", "Korea, Rep.", "Spain", "Sweden", "United States")
//
// replace bip=yr2013 if inlist(countryname, "Austria", "Croatia", "Denmark", "Latvia", "Lithuania", "Mexico", "Poland", "Switzerland", "Turkey") | inlist(countryname, "Hungary", "Netherlands")
//
// replace bip=yr2014 if inlist(countryname, "India")
// replace bip=(yr2011+yr2012)/2 if inlist(countryname, "Israel")
// replace bip=(yr2012+yr2013)/2 if inlist(countryname, "Argentina", "Australia", "Ireland", "South Africa")
//
// replace bip=(yr2013+yr2014)/2 if inlist(countryname, "Canada", "Iceland", "Belgium")
// replace bip=(yr2014+yr2015)/2 if inlist(countryname, "Portugal")
//
//
rename countrycode alpha3
sa bip.dta, replace











