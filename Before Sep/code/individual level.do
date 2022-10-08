clear all

cd "/Users/chaoyuewang/Desktop/Race-Party Outliners"
use "/Users/chaoyuewang/Documents/Data/CCES/cumulative_2006-2020.dta"

svyset [weight = weight_cumulative]

**# Prepare

replace pid7 = . if pid7 > 7
replace ideo5 = . if ideo5 > 5

replace pid3 = . 
replace pid3 = -1 if (pid7 < 4)
replace pid3 = 0 if (pid7 == 4)
replace pid3 = 1 if (pid7 > 4)

label define party -1 "Democrat" 0 "Indepdent" 1 "Republican"
label values pid3 party
tab pid3

gen istr = abs(ideo5 - 3)
gen pstr = abs(pid7 - 4)

drop hispanic

gen white = (race == 1)
gen black = (race == 2)
gen hispanic = (race == 3)
gen asian = (race == 4)
gen minority = (race > 1)


**# Analysis

merge m:n county using data/outliner.dta

reg pstr mgop if pid3 == 1,r
reg pstr mgop if pid3 == -1,r
