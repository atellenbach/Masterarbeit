clear all
cd "data"

import excel using ilo_ad_.xlsx, first case(l)

sa ilo_ad.dta, replace