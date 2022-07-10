***************************************************************************
***************************************************************************
***************************************************************************

*	Quellcode zur Masterarbeit "Geschlechterspezifische Hausarbeitsteilung"

***************************************************************************
***************************************************************************
***************************************************************************

clear all

// ssc install asdoc
// ssc install center
// ssc install valuesof
// ssc install mlt
// ssc install sencode

***Directory**************************************************************

global directory "[Speicherpfad eingeben]"
cd $directory


***************************************************************************
***Daten*******************************************************************
***************************************************************************

***Vorbereitung Datensätze Makrovariablen**********************************

do iso_uebersetzung_prepare.do
cd "$directory"
do bip_prepare.do
cd "$directory"
do gii_gdi_prepare.do
cd "$directory"
do ilo_ad_prepare.do

***ISSP-Daten und Merging**************************************************

cd "$directory"
use "Data\ZA5900_v4-0-0.dta"

rename *, lower
rename c_alphan cntry
replace cntry="GB" if cntry=="GB-GBN"
merge m:1 cntry using Data\iso_uebersetzungstabelle.dta
drop _merge
merge m:m alpha3 dateyr using Data\bip.dta, keepus(bip) keep(match master) //miss_cntry:TW
drop _merge
merge m:1 alpha3 dateyr using Data\gii_gdi.dta, keep(match master) //miss_cntry: TW
drop _merge
merge m:1 cntry using Data\ilo_ad.dta //miss_cntry: TW
drop _merge

***************************************************************************
***Datenaufbereitung und Variablenkonstruktion*****************************
***************************************************************************

***bip/gii_gdi*************************************************************

//Mittelwert des Landes für Beobachtungen ohne Angabe zum Intervewzeitpunkt
global datalist "bip adobirth_gii bip_female_gdi bip_male_gdi educ_female_gii educ_male_gii eeducyrs_female_gdi eeducyrs_male_gdi elife_female_gdi elife_male_gdi hdi_female_gdi hdi_male_gdi lfp_female_gii lfp_male_gii meducyrs_female_gdi meducyrs_male_gdi mmratio_gii seatsparl_gii gii gdi"
foreach u in $datalist {
	foreach i in BEL DNK {
		sum `u' if alpha3=="`i'"
		replace `u'=r(mean) if alpha=="`i'" & dateyr==9999
	}
}

la var bip "BIP"
rename gii_ gii
la var gii "GII"
la var gdi "GDI"


***Absence of discriminatory policy*****************************************

gen ad=ilo45+ilo89+ilo127

gen ad_inforce=ilo45_inforce+ilo89_inforce+ilo127_inforce

la var ad "Absenz diskriminierender Gesetze"


***alleinstehende/alleinlebende*********************************************

keep if partliv==1

//miss_cntry: DK, GB, TW >> ausgeschlossen


***Male*********************************************************************

rename sex male
recode male 9=.
recode male (2=0)
la def la_male 0 "Frau" 1 "Mann"
la val male la_male

la var male "Mann"


***Hausarbeitsteilung*******************************************************

rename v37 hwhrs
recode hwhrs (98 99=.)
replace hwhrs=70 if hwhrs>70 & hwhrs<.

rename v39 sphwhrs
recode sphwhrs (0 98 99=.) (96=0)
replace sphwhrs=70 if sphwhrs>70 & sphwhrs<.
la def la_sphwhrs 1 "or less" 70 "or more"
la val sphwhrs la_sphwhrs

//relative Hausarbeit Frau
gen relhw_female=.
gen hwfemale=.
replace hwfemale=hwhrs if male==0
replace hwfemale=sphwhrs if male==1
gen hwmale=.
replace hwmale=hwhrs if male==1
replace hwmale=sphwhrs if male==0
replace relhw_female= (hwfemale-hwmale)/(hwfemale+hwmale)
la def la_relhw_female -1 "Man does all" 1 "Woman does all"
la val relhw_female la_relhw_female
la var relhw_female "Rel. Hausarbeit"


***Hausarbeit total*********************************************************

gen hwhrs_tot=.
replace hwhrs_tot=hwhrs+sphwhrs
la var hwhrs_tot "Hausarbeit total"


***Rel. Arbeitspensum*********************************************************

//miss_cntry: BG, GB

recode wrkhrs (98 99=.) (71/96 =70)
recode spwrkhrs (97 98 99=.) (71/96 =70)
recode spwrkhrs (0=.) if cntry=="BG" | cntry=="GB"
gen relwhrs_female=.

gen whrsfemale=.
replace whrsfemale=wrkhrs if male==0
replace whrsfemale=spwrkhrs if male==1

gen whrsmale=.
replace whrsmale=wrkhrs if male==1
replace whrsmale=spwrkhrs if male==0
replace relwhrs_female= (whrsfemale-whrsmale)/(whrsfemale+whrsmale)
la def la_relwhrs_female -1 "Man does all" 1 "Woman does all"
la val relwhrs_female la_relwhrs_female
la var relwhrs_female "Rel. Arbeitspensum"

gen relwhrs_female2=relwhrs_female^2
gen relwhrs_female3=relwhrs_female^3
la var relwhrs_female2 "Rel. Arbeitspensum^2"
la var relwhrs_female3 "Rel. Arbeitspensum^3"


***Alter**********************************************************************

drop if age<18
recode age (999=.)
replace age=age-18
gen sqage=age^2
la var age "Alter"
la var sqage "Alter^2"
gen ageraw=age+18
la var ageraw "Alter"


***Anzahl Kinder*************************************************************

//miss_cntry: TK

recode hhtodd hhchildr (99 96=.)

gen childno=.
replace childno=hhtodd+hhchildr if hhtodd<. & hhchildr<.
la var childno "Anzahl Kinder"


***Kohabitation***************************************************************

recode marital (7 9 =.)

gen cohabit=.
replace cohabit=1 if inrange(marital,3,6)
replace cohabit=0 if marital==2 | marital==1
la var cohabit "Zusammenleben ohne Heirat"


***Einkommensschicht Vorbereitung********************************************

//Einkommen Median

//miss_cntry: TK

//missings definiert und monatlich zu jährlich
gen inc_yr=.
foreach i of var *_inc {
	qui recode `i' (999990 999997 999998 999999 9999990 9999997 9999998 99999999 99999990 99999998 		9999999 9999996 =.)
	qui replace inc_yr=`i'*12 if `i'<.
}

//Äquivalenzeinkommen, Median

recode hompop (0 1 99 =.)

gen adults=.
replace adults=hompop-hhchildr-hhtodd-1
recode adults (-3 -2 -1 0 =.)
replace adults=. if adults>6

	//Haushaltseinkommen zu einer Variable (in Landeswährung)
	gen hhinc=.
	foreach i of var *_inc {
		replace hhinc=`i' if `i'<.
	}

capture drop equinc
gen equinc=.
replace equinc= hhinc/(1+(adults*0.5)+(hhchildr*0.4)+(hhtodd*0.3))


***Relatives Einkommen*******************************************************

recode v50 (0 98 99=.)
recode v50 (10=4) //"both no income"(TW) to "equal" 
gen relinc_female=. //Geschlecht einbezogen
replace relinc_female=v50-1 if male==1
replace relinc_female=v50*(-1)+7 if male==0
replace relinc_female=relinc_female-3
la def la_relinc_female -3 "woman no income" -2 "man much higher income" ///
	-1 "man higher income" 0 "equal" 1 "woman higher income" ///
	2 "woman much higher income" 3 "man no income"
la val relinc_female la_relinc_female
gen relinc_female2=relinc_female^2
gen relinc_female3=relinc_female^3
la var relinc_female "Rel. Einkommen"
la var relinc_female2 "Rel. Einkommen^2"
la var relinc_female3 "Rel. Einkommen^3"


***Gender-Ideologie************************************************************

// miss_cntry: ES - missing "neither"

la def la_v5_11 0 "stongly agree" 1 "agree" 2 "neither" 3 "disagree" ///
	4 "storngly disagree"

//Spanien umkodieren
foreach i of var es_v5 es_v6 es_v7 es_v8 es_v9 es_v10 es_v11 {
	recode `i' (3=4) (4=5)
}

foreach i of var v5 v6 v7 v8 v9 v10 v11 {
	replace `i'=es_`i' if cntry=="ES"
	recode `i' (0 8 9=.)
	replace `i'=`i'-1
	la val `i' la_v5_11
}

replace v5 = v5*(-1)+4
replace v10 = v10*(-1)+4

la def la_v5_v10  4 "stongly agree" 3 "agree" 2 "neither" 1 "disagree" ///
	0 "storngly disagree"
la val v5 la_v5_v10
la val v10 la_v5_v10

//Indexbildung
alpha v5 v6 v7 v8 v9 v10 v11, gen(gideo) item
la def la_gideo 0 "traditionell" 4 "egalitär"
la val gideo la_gideo
la var gideo "Ind. Gender-Ideologie"


***relative Bildung***********************************************************

//miss_cntry: ZA, RU, BG, AT Imputation: siehe "Bildung"

// nach Geschlecht

gen educ_female=.
gen educ_male=.
gen releduc_female=.

replace educ_female=1 if inlist(degree,0,1,2) & male==0
replace educ_female=1 if inlist(v65,0,1,2) & male==1 | inlist(v65a,0,1,2) & male==1
replace educ_female=2 if inlist(degree,3,4) & male==0
replace educ_female=2 if inlist(v65,3,4) & male==1 | inlist(v65a,3,4) & male==1
replace educ_female=3 if inlist(degree,5,6) & male==0
replace educ_female=3 if inlist(v65,5,6) & male==1 | inlist(v65a,5,6) & male==1

replace educ_male=1 if inlist(degree,0,1,2) & male==1
replace educ_male=1 if inlist(v65,0,1,2) & male==0 | inlist(v65a,0,1,2) & male==0
replace educ_male=2 if inlist(degree,3,4) & male==1
replace educ_male=2 if inlist(v65,3,4) & male==0 | inlist(v65a,3,4) & male==0
replace educ_male=3 if inlist(degree,5,6) & male==1
replace educ_male=3 if inlist(v65,5,6) & male==0 | inlist(v65a,5,6) & male==0

replace releduc_female=0 if educ_female==educ_male
replace releduc_female=2 if educ_female-educ_male==2
replace releduc_female=1 if educ_female-educ_male==1
replace releduc_female=-1 if educ_male-educ_female==1
replace releduc_female=-2 if educ_male-educ_female==2

la def la_releduc_female -2 "Mann zwei Stufen höher" 0 "beide gleich" ///
	2 "Frau zwei Stufen höher"
la val releduc_female la_releduc_female
la var releduc_female "Rel. Bildung"

// Bildung befragte person
gen educ=.
replace educ=1 if inlist(degree,0,1,2)
replace educ=2 if inlist(degree,3,4)
replace educ=3 if inlist(degree,5,6)


***Sample******************************************************************

capture drop mis
gen mis=0
replace mis=1 if missing(male,relhw_female,relwhrs_female,educ,age,childno,cohabit,relinc_female,equinc,gideo)
replace mis=1 if inlist(cntry,"TW","DK","GB","TR","BG")

***Mean Gender-Ideology*******************************************************

gen meangideo=.

levelsof cntry, local(cntryval)
foreach l of local cntryval {
    qui sum gideo if cntry=="`l'" & mis==0
	replace meangideo=r(mean) if cntry=="`l'"
}
la var meangideo "Ges. Gender-Ideologie"


***Einkommensschicht**********************************************************

gen medianequinc=.
levelsof cntry, local(cntryval)
foreach l of local cntryval {
	qui sum equinc if cntry=="`l'" & mis==0, detail
	replace medianequinc=r(p50) if cntry=="`l'"
}

gen inclev_abs=.
replace inclev_abs= 100/medianequinc*equinc

qui sum inclev_abs, detail
local a=r(max)+1
egen inclev= cut(inclev_abs), at(0 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 250 260 270 280 290 300 `a') label
la var inclev "Einkommensschicht"
replace inclev=inclev*10

// zusammenfassen
egen inclev_big= cut(inclev_abs), ///
	at(0 25 50 75 100 125 150 175 200 225 250 275 300 `a') label
	
	
***Bildung******************************************************************

//miss_cntry: ZA, RU, BG, AT: Conditional Mean Imputation

reg releduc_female relinc_female relwhrs_female male##c.gideo age sqage inclev childno cohabit 
predict xb
gen releduc_female_cm = cond(releduc_female>=., xb, releduc_female)
rename releduc_female releduc_female_noncm
rename releduc_female_cm releduc_female
la var releduc_female "Rel. Bildung"

// Gesamtbildung Paar
capture drop educ_both
gen educ_both=.
replace educ_both=educ+(educ-releduc_female) if male==0
gen releduc_fe_male=releduc_female*(-1)
replace educ_both=educ+(educ-releduc_fe_male) if male==1
la var educ_both "Bildung total"

***Zentrieren****************************************************************

center educ_both inclev gideo bip meangideo gii if mis==0, prefix(c)

la var cmeangideo "Ges. Gender-Ideologie"
la var ceduc_both "Bildung total"
la var cgideo "Ind. Gender-Ideologie"
la var cbip "BIP"
la var cgii "GII"
la var cinclev "Einkommensschicht"


***Hilfsvariablen************************************************************

// Marker: Eine Beobachtung pro Land
capture drop cntrytag
egen cntrytag = tag(cntry) if mis==0

// Identifikation Beobachtung
gen id=_n

// exit

*****************************************************************************
***Deskriptive Statistik*****************************************************
*****************************************************************************
cd "Output"

***Reliabilität Index Gender-Ideologie***************************************
//TODO: richtiger begriff?
alpha v5 v6 v7 v8 v9 v10 v11 if mis==0


***Summary Statistics********************************************************

//Mikroebene
asdoc sum 	relhw_female relinc_female hwhrs_tot inclev gideo childno ///
			relwhrs_female releduc_female educ_both ageraw cohabit if mis==0 ///
			, save(sumstats_mikro.doc) fs(11) font(Arial) label replace

//Makroebene
capture drop hwrank_all_female
capture drop cntryrelhw_all_female

by cntry, sort: egen cntryrelhw_all_female = mean(relhw_female) if mis==0
egen hwrank_all_female = rank(cntryrelhw_all_female) if cntrytag==1, t

// betrachten
by cntry, sort: tab cntryrelhw_all_female hwrank_all_female if cntrytag==1
sort hwrank_all_female
list cntry if cntrytag==1

sort hwrank_all_female
asdoc, row(Land, N, relhw_female, relinc_female, hwhrs_tot, inclev, gideo, ///
	childno, relwhrs_female, releduc_female, educ_both, ageraw, cohabit) ///
	save(sumstats_cntry_mikro.doc) replace fs(11) font(Arial)
valuesof cntry if cntrytag==1
local cntryval = r(values)
foreach l of local cntryval {
    foreach var of var relhw relinc_female hwhrs_tot inclev gideo childno ///
		relwhrs_female releduc_female educ_both ageraw cohabit {
		sum `var' if mis==0 & cntry=="`l'"
		local `var'=r(mean)
	}
	asdoc, accum(`r(N)', `relhw_female', `relinc_female', `hwhrs_tot', `inclev', `gideo', ///
		`childno', `relwhrs_female', `releduc_female', `educ_both', `ageraw', `cohabit')
	asdoc, row(`l', $accum) save(sumstats_cntry_mikro.doc)
}

//Makroebene, Makrovariablen
sort cntry
asdoc, row(Nationen, bip, gii, meangideo, ad) save(sumstats_cntry_makro.doc) ///
	replace fs(11) font(Arial)
levelsof cntry, local(cntryval)
foreach l of local cntryval {
	    sum bip if mis==0 & cntry=="`l'"
		local bip=r(mean)
		sum gii if mis==0 & cntry=="`l'"
		local gii=r(mean)
		sum meangideo if mis==0 & cntry=="`l'"
		local meangideo=r(mean)
		sum ad if mis==0 & cntry=="`l'"
		local ad=r(mean)
	asdoc, accum(`bip', `gii', `meangideo', `ad')
	asdoc, row(`l', $accum) save(sumstats_cntry_makro.doc)
}

// ttest zum Gesamtmittelwert
capture drop p_mean_relhw_feamle_cntry
gen p_mean_relhw_feamle_cntry=.
levelsof cntry if mis==0, local(cntryval)
foreach l of local cntryval {
	sum relhw_female if mis==0
	local n=r(N)
	local mean=r(mean)
	local sd=r(sd)
	sum relhw_female if mis==0 & cntry=="`l'"
	local n2=r(N)
	local mean2=r(mean)
	local sd2=r(sd)
	ttesti `n' `mean' `sd' `n2' `mean2' `sd2'
	replace p_mean_relhw_feamle_cntry=r(p) if cntry=="`l'"
}

// Darstellung ttest
sort hwrank_all_female
asdoc, row(Nationen, N, relhw_feamle, p) ///
	save(diff_cntry_relhw_female.doc) replace fs(11) font(Arial)
valuesof cntry if cntrytag==1
local cntryval = r(values)
foreach l of local cntryval {
    foreach var of var relhw_female p_mean_relhw_feamle_cntry {
		sum `var' if mis==0 & cntry=="`l'"
		local `var'=r(mean)
	}
	asdoc, accum(`r(N)', `relhw_female', `p_mean_relhw_feamle_cntry')
	asdoc, row(`l', $accum) save(diff_cntry_relhw_female.doc)
}

//Rankings Makroebene, Makrovariablen
egen biprank = rank(bip) if cntrytag==1, t
egen meangideorank = rank(meangideo) if cntrytag==1, t
egen giirank = rank(gii) if cntrytag==1, t
sort hwrank_all_female

asdoc, row(Land, hw, GII, Meangideo, BIP, ad) ///
	save(rankings_makro.doc) replace fs(11) font(Arial)
valuesof cntry if cntrytag==1
local cntryval = r(values)
foreach l of local cntryval {
	foreach var of var hwrank_all_female giirank meangideorank biprank ad {
		sum `var' if mis==0 & cntry=="`l'"
		local `var'=r(mean)
	}
	asdoc, accum(`hwrank_all_female', `giirank', `meangideorank', `biprank' `ad')
	asdoc, row(`l', $accum) save(rankings_makro.doc)
}

//Korrelation Y und GII, Gender-Ideologie
bysort cntry: egen a= mean(relhw_female)
pwcorr a gii if cntrytag==1, sig
pwcorr a meangideo if cntrytag==1, sig

*****************************************************************************
***Modell********************************************************************
*****************************************************************************

***Leeres Modell*************************************************************

asdoc mixed relhw_female || cntry: if mis==0, save(nullmodel.doc) replace reml
estat icc

***Basismodell***************************************************************

mixed 	relhw_female relinc_female inclev releduc_female relwhrs_female ///
		educ_both hwhrs_tot childno gideo age sqage cohabit male ///
		|| cntry: if mis==0, cov(unstructured) reml
est sto ri

// slopes: LR-Test und Darstellung
asdoc, row(Variable, chi2, fd, p) ///
	save(lr_test_slopes.doc) replace fs(11) font(Arial) replace
foreach i of var relinc_female releduc_female relwhrs_female childno gideo {
	mixed 	relhw_female relinc_female inclev releduc_female relwhrs_female ///
		educ_both hwhrs_tot childno gideo age sqage cohabit male ///
		|| cntry: `i' if mis==0, cov(unstructured) reml
	est sto slope_`i'
	lrtest ri slope_`i'
	asdoc, row accum(`i', `r(chi2)', `r(df)', `r(p)')
	asdoc, row($accum) save(lr_test_slopes.doc)
}

// Visualisierung

foreach var of var relinc_female releduc_female relwhrs_female gideo {
    capture drop fit1
    mixed relhw_female `var' || cntry: `var' if mis==0, cov(unstructured) reml
    predict fit1, fitted
    sort cntry `var'
    twoway (line fit1 `var', connect(ascending)), ///
        xtitle(`var') ///
        ytitle(Empirical Bayes regression lines for random slope) ///
        saving(`var'_slope_unsorted.gph, replace)
    // graph länder
    capture drop fit2
    capture drop tag
    capture drop resid*
    predict resid*, reffects
    gen fit2 = fit1 - resid2
    gen fit2_`var'=fit2
    bysort cntry fit2: gen tag=_n==1
    qui levelsof cntry if mis==0 
    foreach l in `r(levels)'  {
        local plot_`var' `plot_`var'' ///
        (line fit2 `var' if cntry=="`l'", sort pstyle(p1))
    }
	local lab: variable label `var'
    sum `var'
    two `plot_`var'' ///
        (function y = _b[_cons] + x*_b[`var'], range(`r(min)' `r(max)') pstyle(p1) lc(red) lw(*3)) ///
        if tag, ///
        xti("`lab'") ///
        yti("Rel. Hausarbeit Frau") ///
        legend(off) ///
        scheme(sj) ///
        ylabel(, nogrid) ///
        graphregion(c(white)) ///
        bgcolor(white)
    graph export `var'_slopes.png, replace

}
drop fit1 fit2 tag resid1 resid2


// Visualisierung: Legende

foreach var of var relinc_female releduc_female relwhrs_female gideo {
	gen fit2_bei1=. // manuell Steigung aus Grafik berechnen:
	replace fit2_bei1=fit2_`var' if `var'==1
	levelsof cntry if mis==0
	foreach l in `r(levels)' {
		sum fit2_bei1 if cntry=="`l'"
		replace fit2_bei1 = r(mean) if cntry=="`l'"
	}
	gen slope_`var'=(fit2_bei1-_b[_cons])/1

	capture drop `var'_slope_rank // Ranking und Legende
	egen `var'_slope_rank = rank(slope_`var') if cntrytag==1, t

	sort `var'_slope_rank
	asdoc, row(Nationen, `var'_slope) ///
		save(`var'_slope_rank.doc) replace fs(11) font(Arial)
	valuesof cntry if cntrytag==1
	foreach l in `r(values)' {
		sum slope_`var' if cntrytag==1 & cntry=="`l'"
		asdoc, row(`l', `r(mean)') save(`var'_slope_rank.doc)
	}
	drop fit2_bei1 slope_`var' fit2_`var' `var'_slope_rank
}

*****************************************************************************
***Modellaufbau**************************************************************
*****************************************************************************

***Gesamttabelle*************************************************************

eststo clear

//Leer
eststo m_leer: mixed relhw_female || cntry: if mis==0, reml
estadd sca makrovarianz=exp([lns1_1_1]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2
sca base= (exp([lns1_1_1]_cons)^2) + (exp([lnsig_e]_cons)^2)
estadd sca mikrosd=exp([lnsig_e]_cons)
estadd sca makrosd=exp([lns1_1_1]_cons)

//Random Slopes
eststo m_slopes: mixed relhw_female relinc_female relwhrs_female cgideo ///
							|| cntry: relinc_female relwhrs_female cgideo ///
							if mis==0, cov(unstructured) reml
estadd sca relinc_female_slope=exp([lns1_1_1]_cons)^2
estadd sca relwhrs_female_slope=exp([lns1_1_2]_cons)^2
estadd sca gideo_slope=exp([lns1_1_3]_cons)^2
estadd sca makrovarianz=exp([lns1_1_4]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2
sca square= (exp([lns1_1_4]_cons)^2) + (exp([lnsig_e]_cons)^2)
estadd sca r2= (base-square)/base
sca drop square
estadd sca mikrosd=exp([lnsig_e]_cons)
estadd sca makrosd=exp([lns1_1_4]_cons)

//Ohne Makrovariablen
eststo m_mikro_quadint: mixed 	relhw_female ///
							(c.relinc_female c.relinc_female2 c.relinc_female3)##c.cinclev ///
							c.releduc_female##c.ceduc_both ///
							c.relwhrs_female c.relwhrs_female2 c.relwhrs_female3 ///
							childno c.cgideo##male age sqage cohabit hwhrs_tot ///
							|| cntry: relinc_female relwhrs_female cgideo ///
							if mis==0, cov(unstructured) reml
estadd sca relinc_female_slope=exp([lns1_1_1]_cons)^2
estadd sca relwhrs_female_slope=exp([lns1_1_2]_cons)^2
estadd sca gideo_slope=exp([lns1_1_3]_cons)^2
estadd sca makrovarianz=exp([lns1_1_4]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2
sca square= (exp([lns1_1_4]_cons)^2) + (exp([lnsig_e]_cons)^2)
estadd sca r2= (base-square)/base
sca drop square
estadd sca mikrosd=exp([lnsig_e]_cons)
estadd sca makrosd=exp([lns1_1_4]_cons)


//Test Polynome
testparm relinc_female*
testparm relwhrs_female*

//Gesamtmodell
eststo m_makro: mixed 	relhw_female ///
						c.relinc_female##c.cinclev ///
						c.relinc_female##(c.cmeangideo c.cbip c.ad c.cgii) ///
						c.releduc_female##c.ceduc_both ///
						c.relwhrs_female##(c.cmeangideo c.cbip c.ad c.cgii) ///
						c.cgideo##(male c.cmeangideo c.cbip c.ad c.cgii) ///
						childno age sqage cohabit hwhrs_tot ///
						|| cntry: relinc_female relwhrs_female cgideo ///
						if mis==0, cov(unstructured) reml
estadd sca relinc_female_slope=exp([lns1_1_1]_cons)^2
estadd sca relwhrs_female_slope=exp([lns1_1_2]_cons)^2
estadd sca gideo_slope=exp([lns1_1_3]_cons)^2
estadd sca makrovarianz=exp([lns1_1_4]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2
sca square= (exp([lns1_1_4]_cons)^2) + (exp([lnsig_e]_cons)^2)
estadd sca r2= (base-square)/base
sca drop square
estadd sca mikrosd=exp([lnsig_e]_cons)
estadd sca makrosd=exp([lns1_1_4]_cons)


//Tabelle
esttab m* using collected_model_text.rtf, ///
	scalar(relinc_female_slope relwhrs_female_slope gideo_slope mikrovarianz makrovarianz r2 mikrosd makrosd) ///
	varwidth(30) ///
	se ///
	noomit ///
	nonum mti("Modell 1" "Modell 2" "Modell 3" "Modell 4") ///
	ti ("mixed für rel hw frau") ///
	label ///
	nobaselevels ///
	interaction(" X ") ///
	compress ///
	one ///
	order(cgii cmeangideo cbip ad relinc_female c.relinc_female#c.cgii c.relinc_female#c.cmeangideo c.relinc_female#c.cbip c.relinc_female#c.ad relwhrs_female c.relwhrs_female#c.cgii c.relwhrs_female#c.cmeangideo c.relwhrs_female#c.cbip c.relwhrs_female#c.ad cgideo c.cgideo#c.cgii c.cgideo#c.cmeangideo c.cgideo#c.cbip c.cgideo#c.ad releduc_female c.releduc_female#* c.ceduc_both c.relinc_female#c.cinclev c.cinclev relinc_female2 relinc_female3 c.relinc_female2#c.cinclev c.relinc_female3#c.cinclev relwhrs_female2 relwhrs_female3 c.cgideo#1.male 1.male childno cohabit hwhrs_tot age sqage Varianzkomponenten) ///
	fonttbl(\f0\fnil Arial; ) ///
	noobs ///
	replace

***Tabellen Anhang************************************************************

eststo clear

//Ohne Ressourcenvariablen
eststo m_mikro_edu: mixed 	relhw_female c.releduc_female##c.ceduc_both ///
							childno c.cgideo##male age sqage cohabit hwhrs_tot ///
							|| cntry: releduc_female if mis==0, cov(unstructured) reml
estadd sca releduc=exp([lns1_1_1]_cons)^2
estadd sca makrovarianz=exp([lns1_1_2]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2

//Tabelle
esttab m* using nurBildung_model_anhang.rtf, ///
	scalar(releduc mikrovarianz makrovarianz) ///
	varwidth(30) ///
	modelwidth(14) ///
	se ///
	noomit ///
	nonum nomti ///
	label ///
	nobaselevels ///
	interaction(" X ") ///
	compress ///
	one ///
	order(cgii cmeangideo cbip ad relinc_female c.relinc_female#c.cgii c.relinc_female#c.cmeangideo c.relinc_female#c.cbip c.relinc_female#c.ad relwhrs_female c.relwhrs_female#c.cgii c.relwhrs_female#c.cmeangideo c.relwhrs_female#c.cbip c.relwhrs_female#c.ad cgideo c.cgideo#c.cgii c.cgideo#c.cmeangideo c.cgideo#c.cbip c.cgideo#c.ad releduc_female c.releduc_female#* c.ceduc_both c.relinc_female#c.cinclev c.cinclev relinc_female2 relinc_female3 c.relinc_female2#c.cinclev c.relinc_female3#c.cinclev relwhrs_female2 relwhrs_female3 c.cgideo#1.male 1.male childno cohabit hwhrs_tot age sqage Varianzkomponenten) ///
	fonttbl(\f0\fnil Arial; ) ///
	obslast ///
	replace

eststo clear

//Nur GII
eststo m_makro_gii: mixed 	relhw_female ///
						c.relinc_female##c.cinclev ///
						c.relinc_female##(c.cgii) ///
						c.releduc_female##c.ceduc_both ///
						c.relwhrs_female##(c.cgii) ///
						c.cgideo##(male c.cgii) ///
						childno age sqage cohabit hwhrs_tot ///
						|| cntry: relinc_female relwhrs_female cgideo ///
						if mis==0, cov(unstructured) reml
estadd sca relinc_female_slope=exp([lns1_1_1]_cons)^2
estadd sca relwhrs_female_slope=exp([lns1_1_2]_cons)^2
estadd sca gideo_slope=exp([lns1_1_3]_cons)^2
estadd sca makrovarianz=exp([lns1_1_4]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2

//Gesamtmodell mit GDI
eststo m_makro_gdi: mixed 	relhw_female gdi ///
						c.relinc_female##c.cinclev ///
						c.relinc_female##(c.cmeangideo c.cbip c.ad c.cgii) ///
						c.releduc_female##c.ceduc_both ///
						c.relwhrs_female##(c.cmeangideo c.cbip c.ad c.cgii) ///
						c.cgideo##(male c.cmeangideo c.cbip c.ad c.cgii) ///
						childno age sqage cohabit hwhrs_tot ///
						|| cntry: relinc_female relwhrs_female cgideo ///
						if mis==0, cov(unstructured) reml
estadd sca relinc_female_slope=exp([lns1_1_1]_cons)^2
estadd sca relwhrs_female_slope=exp([lns1_1_2]_cons)^2
estadd sca gideo_slope=exp([lns1_1_3]_cons)^2
estadd sca makrovarianz=exp([lns1_1_4]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2
estadd sca mikrosd=exp([lnsig_e]_cons)
estadd sca makrosd=exp([lns1_1_4]_cons)

//Gesamtmodell ohne Indien
eststo m_makro_indien: mixed 	relhw_female ///
						c.relinc_female##c.cinclev ///
						c.relinc_female##(c.cmeangideo c.cbip c.ad c.cgii) ///
						c.releduc_female##c.ceduc_both ///
						c.relwhrs_female##(c.cmeangideo c.cbip c.ad c.cgii) ///
						c.cgideo##(male c.cmeangideo c.cbip c.ad c.cgii) ///
						childno age sqage cohabit hwhrs_tot ///
						|| cntry: relinc_female relwhrs_female cgideo ///
						if mis==0 & cntry!="IN", cov(unstructured) reml
estadd sca relinc_female_slope=exp([lns1_1_1]_cons)^2
estadd sca relwhrs_female_slope=exp([lns1_1_2]_cons)^2
estadd sca gideo_slope=exp([lns1_1_3]_cons)^2
estadd sca makrovarianz=exp([lns1_1_4]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2
estadd sca mikrosd=exp([lnsig_e]_cons)
estadd sca makrosd=exp([lns1_1_4]_cons)

//Gesamtmodell ohne China
eststo m_makro_china: mixed 	relhw_female ///
						c.relinc_female##c.cinclev ///
						c.relinc_female##(c.cmeangideo c.cbip c.ad c.cgii) ///
						c.releduc_female##c.ceduc_both ///
						c.relwhrs_female##(c.cmeangideo c.cbip c.ad c.cgii) ///
						c.cgideo##(male c.cmeangideo c.cbip c.ad c.cgii) ///
						childno age sqage cohabit hwhrs_tot ///
						|| cntry: relinc_female relwhrs_female cgideo ///
						if mis==0 & cntry!="CN", cov(unstructured) reml
estadd sca relinc_female_slope=exp([lns1_1_1]_cons)^2
estadd sca relwhrs_female_slope=exp([lns1_1_2]_cons)^2
estadd sca gideo_slope=exp([lns1_1_3]_cons)^2
estadd sca makrovarianz=exp([lns1_1_4]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2
estadd sca mikrosd=exp([lnsig_e]_cons)
estadd sca makrosd=exp([lns1_1_4]_cons)


//Tabelle
esttab m* using collected_model_anhang.rtf, ///
	scalar(relinc_female_slope relwhrs_female_slope gideo_slope mikrovarianz makrovarianz) ///
	varwidth(30) ///
	modelwidth(14) ///
	se ///
	noomit ///
	nonum mti("Modell 1" "Modell 2" "Modell 3" "Modell 4") ///
	label ///
	nobaselevels ///
	interaction(" X ") ///
	compress ///
	one ///
	order(gdi cgii cmeangideo cbip ad relinc_female c.relinc_female#c.cgii c.relinc_female#c.cmeangideo c.relinc_female#c.cbip c.relinc_female#c.ad relwhrs_female c.relwhrs_female#c.cgii c.relwhrs_female#c.cmeangideo c.relwhrs_female#c.cbip c.relwhrs_female#c.ad cgideo c.cgideo#c.cgii c.cgideo#c.cmeangideo c.cgideo#c.cbip c.cgideo#c.ad releduc_female c.releduc_female#* c.ceduc_both c.relinc_female#c.cinclev c.cinclev relinc_female2 relinc_female3 c.relinc_female2#c.cinclev c.relinc_female3#c.cinclev relwhrs_female2 relwhrs_female3 c.cgideo#1.male 1.male childno cohabit hwhrs_tot age sqage Varianzkomponenten) ///
	fonttbl(\f0\fnil Arial; ) ///
	obslast ///
	replace


***Annahmenprüfung************************************************************
		
//Gesamtmodell Random Coefficients als Basis
mixed 	relhw_female relinc_female inclev releduc_female relwhrs_female ///
		educ_both childno gideo age sqage cohabit male cbip meangideo ad gii hwhrs_tot ///
		|| cntry: if mis==0, cov(unstructured) reml
		
capture drop r
predict r if e(sample), resid relevel(cntry)

//Nicht-Normalität

histogram r, 	scheme(sj) ///
				graphregion(c(white)) ///
				bgcolor(white) ///
				xti("Residuen") ///
				yti("Dichte")
graph export normality.png, replace


//Ausreisser Mikroebene

reg relhw_female relinc_female inclev releduc_female relwhrs_female ///
		educ_both childno gideo age sqage cohabit male, cluster(cntry)

foreach var of var relinc_female inclev releduc_female relwhrs_female ///
		educ_both childno gideo age sqage cohabit male {
		avplot `var'
		gr export `var'_avplot.png, replace
}

//Ausreisser Makroebene

xtmixed 	relhw_female relinc_female inclev releduc_female relwhrs_female ///
		educ_both childno gideo age sqage cohabit male cbip meangideo ad gii ///
		|| cntry: if mis==0, cov(unstructured) reml
		
mltcooksd, keepvar(beta)

foreach var of var relinc_female inclev releduc_female relwhrs_female ///
		educ_both childno gideo age sqage cohabit male cbip meangideo ad gii {
			local lab: variable label `var'
			la var beta_DFB_`var' "`lab'"
		}

graph hbox beta_DFB_relinc_female beta_DFB_inclev beta_DFB_releduc_female beta_DFB_relwhrs_female ///
	beta_DFB_educ_both beta_DFB_childno beta_DFB_gideo beta_DFB_age beta_DFB_cohabit ///
	beta_DFB_male beta_DFB_cbip beta_DFB_meangideo beta_DFB_ad beta_DFB_gii, ///
		m(1, mlabel(cntry) mlabposition(12)) m(2, mlabel(cntry) mlabposition(12)) ///
		m(3, mlabel(cntry) mlabposition(12)) m(4, mlabel(cntry) mlabposition(12)) ///
		m(5, mlabel(cntry) mlabposition(12)) m(6, mlabel(cntry) mlabposition(12)) ///
		m(7, mlabel(cntry) mlabposition(12)) m(8, mlabel(cntry) mlabposition(12)) ///
		m(9, mlabel(cntry) mlabposition(12)) m(10, mlabel(cntry) mlabposition(12)) ///
		m(11, mlabel(cntry) mlabposition(12)) m(12, mlabel(cntry) mlabposition(12)) ///
		m(13, mlabel(cntry) mlabposition(12)) m(14, mlabel(cntry) mlabposition(12)) ///
		graphregion(c(white)) ///
		legend(off) ///
		ytitle(DFBETAs) ///
		showyvars
		
graph export Ausreisser_makro.png, replace	


*****************************************************************************
***Vergleich zu Fuwa 2004****************************************************
*****************************************************************************

***Variablenkonstruktion*****************************************************

//Y nach Aufgaben
recode v42 v45 v44 v47 v46 (0 6 8 9=.)
la def la_newvar 1 "always man" 5 "Always woman"
foreach var of var v42 v45 v44 v47 v46 {
	gen new_`var' =`var'
    replace new_`var'= `var'*(-1)+6 if male==0
	la val new_`var' la_newvar
}

//ob Kinder im Haushalt
gen childdummy=.
replace childdummy=0 if hhtodd==0 & hhchildr==0
replace childdummy=1 if inrange(hhtodd,1,9) | inrange(hhchildr,1,21)
la var childdummy "Ob Kinder im Haushalt"

//Gender-Ideologie mit weniger Items
alpha v6 v7 v8 v9 v11 if !missing(male,whrsfemale,whrsmale,educ,age,childdummy,relinc_female), gen(gideo_fuwa) item
la val gideo_fuwa la_gideo
la var gideo_fuwa "Ind. Gender-Ideologie"

alpha new_v42 new_v44 new_v45 new_v47 if !missing(male,whrsfemale,whrsmale,educ,age,childdummy,relinc_female,gideo), gen(relhousework_fuwa) item
la var relhousework_fuwa "Rel. Hausarbeit"

gen meangideo_fuwa=.
levelsof cntry, local(cntryval)
foreach l of local cntryval {
    qui sum gideo_fuwa if cntry=="`l'" & !missing(male,whrsfemale,whrsmale,educ,age,childdummy,relinc_female)
	replace meangideo_fuwa=r(mean) if cntry=="`l'"
}
la var meangideo_fuwa "Ges. Gender-Ideologie"

//Labels
la var educ "Bildung"
la var whrsfemale "Arbeitspensum Frau"
la var whrsmale "Arbeitspensum Mann"

//Zentrieren
center meangideo_fuwa gideo_fuwa if mis==0, prefix(c)
la var cmeangideo_fuwa "Ges. Gender-Ideologie"
la var cgideo_fuwa "Ind. Gender-Ideologie"

***Modelle*******************************************************************

mixed relhousework_fuwa || cntry: if mis==0, reml 
estat icc

mixed relhw_female || cntry: if mis==0, reml
estat icc

eststo clear

eststo fuwa1: mixed relhw_female c.relinc_female##(c.cgii c.cmeangideo_fuwa) c.whrsfemale##(c.cgii c.cmeangideo_fuwa) cohabit whrsmale age sqage educ childdummy (c.cgii c.cmeangideo_fuwa male)##c.cgideo_fuwa || cntry: relinc_female whrsfemale cgideo_fuwa, reml
estadd sca relinc_female_slope=exp([lns1_1_1]_cons)^2
estadd sca whrs_female_slope=exp([lns1_1_2]_cons)^2
estadd sca gideo_slope=exp([lns1_1_3]_cons)^2
estadd sca makrovarianz=exp([lns1_1_4]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2

eststo fuwa2: mixed relhousework_fuwa c.relinc_female##(c.cgii c.cmeangideo_fuwa) c.whrsfemale##(c.cgii c.cmeangideo_fuwa) cohabit whrsmale age sqage educ childdummy (c.cgii c.cmeangideo_fuwa male)##c.cgideo_fuwa || cntry: relinc_female whrsfemale cgideo_fuwa, reml
estadd sca relinc_female_slope=exp([lns1_1_1]_cons)^2
estadd sca whrs_female_slope=exp([lns1_1_2]_cons)^2
estadd sca gideo_slope=exp([lns1_1_3]_cons)^2
estadd sca makrovarianz=exp([lns1_1_4]_cons)^2
estadd sca mikrovarianz=exp([lnsig_e]_cons)^2

//Tabelle
esttab fuwa* using collected_model_fuwa.rtf, ///
	scalar(relinc_female_slope whrs_female_slope gideo_slope mikrovarianz makrovarianz) ///
	varwidth(30) ///
	modelwidth(14) ///
	se ///
	noomit ///
	nonum mti("Modell 1" "Modell 2") ///
	label ///
	nobaselevels ///
	interaction(" X ") ///
	compress ///
	one ///
	order(cgii cmeangideo_fuwa relinc_female c.relinc_female#c.cgii c.relinc_female#c.cmeangideo_fuwa whrsfemale c.whrsfemale#c.cgii c.whrsfemale#c.cmeangideo_fuwa cgideo_fuwa c.cgideo_fuwa#c.cgii c.cgideo_fuwa#c.cmeangideo_fuwa c.gideo_fuwa#1.male educ childdummy cohabit whrsmale age sqage 1.male Varianzkomponenten) ///
	fonttbl(\f0\fnil Arial; ) ///
	obslast ///
	replace


***Vergleich Hausarbeitsteilung 2002-2012*************************************

gen relhw_female_2007=.
replace relhw_female_2007=0.25 if cntry=="LV"
replace relhw_female_2007=0.26 if cntry=="PL"
replace relhw_female_2007=0.30 if cntry=="SK"
replace relhw_female_2007=0.30 if cntry=="DK"
replace relhw_female_2007=0.30 if cntry=="AU"
replace relhw_female_2007=0.30 if cntry=="RU"
replace relhw_female_2007=0.32 if cntry=="SE"
replace relhw_female_2007=0.33 if cntry=="US"
replace relhw_female_2007=0.33 if cntry=="MX"
replace relhw_female_2007=0.35 if cntry=="FI"
replace relhw_female_2007=0.36 if cntry=="CZ"
replace relhw_female_2007=0.36 if cntry=="BG"
replace relhw_female_2007=0.37 if cntry=="GB"
replace relhw_female_2007=((0.39*252)+(0.49*509))/(252+509) if cntry=="DE"
replace relhw_female_2007=0.42 if cntry=="BE"
replace relhw_female_2007=0.43 if cntry=="HU"
replace relhw_female_2007=0.43 if cntry=="NO"
replace relhw_female_2007=0.48 if cntry=="SI"
replace relhw_female_2007=0.48 if cntry=="NL"
replace relhw_female_2007=0.48 if cntry=="IL"
replace relhw_female_2007=0.50 if cntry=="FR"
replace relhw_female_2007=0.52 if cntry=="CH"
replace relhw_female_2007=0.53 if cntry=="AT"
replace relhw_female_2007=0.56 if cntry=="IE"
replace relhw_female_2007=0.58 if cntry=="CL"
replace relhw_female_2007=0.60 if cntry=="ES"
replace relhw_female_2007=0.61 if cntry=="PT"
replace relhw_female_2007=0.79 if cntry=="JP"

//Gemeinsame Länder markieren
capture drop bothcntrytag
capture drop bothcntry
levelsof cntry if mis==0 & !missing(relhw_female_2007) & cntrytag==1, l(bothcountries)
gen bothcntry=.
quietly foreach l of local bothcountries {
       replace bothcntry = 1 if cntry == "`l'"
}
capture drop bothcntrytag
egen bothcntrytag = tag(cntry) if mis==0 & bothcntry==1

//Ränge erstellen
capture drop rank12
capture drop rank07
capture drop hwmean12
by cntry, sort: egen hwmean12 = mean(relhw_female) if mis==0
egen rank12 = rank(hwmean12) if bothcntrytag==1, t
egen rank07 = rank(relhw_female_2007) if bothcntrytag==1, t

//Zur Vereinfachung alles andere löschen
keep if bothcntrytag==1
keep rank12 rank07 hwmean12 relhw_female_2007 cntry
rename relhw_female_2007 hwmean7

//Grafik
capture drop numcntry
sort rank12
gen numcntry=_n
forvalues i=1/25 {
	label define lblname `i' "`: di cntry[`i']'", add
}
label values numcntry lblname
tw pcarrow hwmean7 numcntry hwmean12 numcntry, ///
	xlabel(1(1)25, valuelabel angle(45)) ///
	xtitle("") ///
	ytitle("Rel. Hausarbeit Frau", size(large)) ///
	scheme(sj) ///
	graphregion(c(white)) ///
	bgcolor(white)
graph export fuwa_heute.png, replace

//Ende Do-File