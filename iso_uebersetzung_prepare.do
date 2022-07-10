clear all

cd "data"

import excel "iso_uebersetzungstabelle.xlsx", case(l) first

drop if alpha2==""

//TODO: fehelende in oecd medianinc einfügen als missing (da fehlen auch noch ein paar oecd-länder, liste von v4 oder excel benutzen)

keep if inlist(alpha2, "AR", "AU", "AT", "BG", "CA", "CL", "CN", "HR", "CZ") | inlist(alpha2, "DK", "FI", "FR", "DE", "GB", "IS", "IN", "IE", "IL") | inlist(alpha2, "JP", "LV", "LT", "MX", "NO", "PH", "PL", "PT", "RU") | inlist(alpha2, "SI", "SK", "ZA", "KR", "ES", "SE", "CH", "TW", "TR") | inlist(alpha2, "US", "VE", "BE", "HU", "NL")

rename alpha2 cntry
sa iso_uebersetzungstabelle.dta, replace