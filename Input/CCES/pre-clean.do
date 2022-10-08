clear all

cd "/Users/chaoyuewang/Desktop/Local Party Images/Input/CCES"

use "/Users/chaoyuewang/Downloads/Surveys/CCES/CCES Cumulative.dta", clear

keep if inlist(year, 2016, 2018, 2020)

rename weight_cumulative weight2

**# Demograhpic

foreach i in race gender birthyr faminc educ {
	tab `i'
}

**# Geography

rename state statefip
decode statefip, gen(state)
decode st, gen(state2)
tab cd 
tab dist


**# Political Behavior

foreach i in pid7 pid3 ideo5 {
	tab `i'
	recode `i' max = . // recode 'Not Sure' as missing value
	tab `i'
} 

keep year weight* ///
race gender birthyr faminc educ ///
state state2 cd dist ///
pid7 pid3 ideo5

save "cces cleaned.dta",replace
outsheet using "cces cleaned.csv", replace comma nolabel
