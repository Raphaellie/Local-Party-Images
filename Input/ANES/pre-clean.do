clear all

cd "/Users/chaoyuewang/Desktop/Local Party Images/Input/ANES"

use "/Users/chaoyuewang/Downloads/Surveys/ANES/ANES 2020.dta",clear

rename V200010a weight

rename V200010b weight_post


****************************
*** variable preparation ***
****************************

*** geography
clonevar state = V201014b
tab state

rename state statefip
rename V203001 state2
rename V203002 dist
rename V203003 region

*** racial identity

clonevar race  = V201549x
tab race
recode race -9/-8 = .

gen white = (race == 1)
gen black = (race == 2)
gen hispan = (race == 3)
gen asian = (race == 4)

**# racial identity importance

clonevar whiteid = V202499x
tab whiteid
recode whiteid -9/-1 = .
// revrs whiteid, replace

clonevar hispanid  = V202498x
tab hispanid
recode hispanid -9/-1 = .
// revrs hispanid, replace

clonevar blackid = V202500x
tab blackid
recode blackid -9/-1 = .
// revrs blackid, replace

label var blackid "Importance of Black Identity"

clonevar asianid = V202502x
tab asianid
recode asianid -9/-1 = .
// revrs asianid, replace


gen raceid = .
replace raceid = whiteid if white == 1
replace raceid = blackid if black == 1
replace raceid = hispanid if hispan == 1
replace raceid = asianid if asian == 1



// label define raceid 0 "Other race" /// 
// 1 "Not at all important" ///
// 2 "A little important" /// 
// 3 "Moderately important" /// 
// 4 "Very important" /// 
// 5 "Extremely important"
//
// foreach race in whiteid blackid hispanid{
// 	label values `race' raceid
// 	tab `race'
// }

*** political identity

clonevar pid7  = V201231x 
tab pid7
recode pid7 -9/-8 = .

clonevar pid3 = V201228
tab pid3 
recode pid3 -9/0 = . 5 = . 1 = -1 2 = 1 3 = 0

label define party -1 "Democrat" 0 "Independent" 1 "Republican"
label values pid3 party


clonevar ideo  = V201200 
tab ideo
recode ideo 99 = . -9/-8 = .

clonevar pid_importance = V201232
tab pid_importance
recode pid_importance min/0 = .

revrs pid_importance, replace
tab pid_importance

replace pid_importance = pid_importance * pid3


*** education level

clonevar education = V201510
tab education
recode education 20/max = . min/0 = . 
tab education


clonevar education5 = V201511x
tab education5
recode education5 min/0 = . 
tab education5


clonevar education4 = education
tab education4
recode education4 1/2 = 1 3/5 = 2 6 = 3 7/8 = 4

label var education "Highest Level of Education"

label define edu4cat 1 "No College" 2 "Some College" 3 "BA" 4 "Advanced"
label values education4 edu4cat
tab education4


*** other demographics

clonevar age = V201507x
tab age
recode age min/0 = .

clonevar income = V201617x
tab income

clonevar gender = V201600
tab gender

clonevar marital = V201508
tab marital 

*** polarization

clonevar fl_dem = V201156
tab fl_dem
recode fl_dem 998 = . -9 = .

clonevar fl_gop = V201157
tab fl_gop 
recode fl_gop 998 = . -9 = .

gen fl_ingroup = .
replace fl_ingroup = fl_dem if pid7 < 4
replace fl_ingroup = fl_gop if pid7 > 4

gen fl_outgroup = .
replace fl_outgroup = fl_dem if pid7 > 4
replace fl_outgroup = fl_gop if pid7 < 4


rename V202482 ft_white
recode ft_white 998 = . -9/-1 = .
rename V202480 ft_black
recode ft_black 998 = .  -9/-1 = .
rename V202479 ft_hispanic
recode ft_hispanic 998 = .  -9/-1 = .


*** Racial Resentment scale

clonevar condition_hard_for_blacks = V202301  

clonevar blacks_no_special_favor = V202300  

clonevar blacks_try_harder = V202303  

clonevar blacks_got_less = V202302  

foreach var in condition_hard_for_blacks blacks_no_special_favor blacks_try_harder blacks_got_less{
	recode `var' min/0 = .
}

replace blacks_no_special_favor =  5 - blacks_no_special_favor
replace blacks_try_harder = 5 - blacks_try_harder

gen rr =(blacks_no_special_favor + blacks_try_harder + condition_hard_for_blacks + blacks_got_less)/16


*** political activism

clonevar protest = V202025
tab protest
recode protest min/0 = .
tab protest 

clonevar petition = V202026
tab petition
recode petition min/0 = .
tab petition 

clonevar contribute_indv = V202017
tab contribute_indv
recode contribute_indv min/0 = .
tab contribute_indv 

clonevar contribute_party = V202019
tab contribute_party
recode contribute_party min/0 = .
tab contribute_party 

clonevar contribute_group = V202021
tab contribute_group
recode contribute_group min/0 = .
tab contribute_group 

clonevar work = V202016
tab work 
recode work min/0 = .
tab work

gen campaign_actvt = 8 - (contribute_indv + contribute_party + contribute_group + work)

drop V*

save "anes cleaned.dta",replace
outsheet using "anes cleaned.csv", replace nolabel comma






