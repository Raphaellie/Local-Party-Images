clear

cd  "/Users/chaoyuewang/Documents/Data/ANES/anes_timeseries_cdf_stata_20211118/"

use anes_timeseries_cdf_stata_20211118.dta


*** 1. Replicate Basic Variables

* 1.1 survey related

clonevar year = VCF0004  

clonevar case_id = VCF0006  
//
// clonevar case_id_unique = VCF0006a  
//
// clonevar post_status = VCF0013  
//
// clonevar pre_status = VCF0014  

clonevar weight = VCF0009z   

* 1.2 demographic characteristics

clonevar race = VCF0105b  

gen white = (race == 1)
gen black = (race == 2)
gen hispan = (race == 3)
gen other = (race > 3 | race == 0)

//
// clonevar gender = VCF0104  
//
// clonevar age = VCF0102  
//
// clonevar age2 = VCF0101
//
// clonevar cohort = VCF0103  
//
// * 1.3 socioeconomic charactertistics
//
// clonevar education = VCF0140  
// clonevar education2 = VCF0110  
//
// clonevar religion = VCF0128  
//
// clonevar income = VCF0114  
//
// clonevar employment = VCF0118  
//
// clonevar class = VCF0148  
//
// clonevar marriage = VCF0147 
//
//
*** 2. Replicate Political Indicators

* 2.1 Party Identification

clonevar pid7 = VCF0301  

clonevar pid3 = VCF0303  

clonevar pstr = VCF0305  

* 2.2 Ideology

clonevar ideo7 = VCF0803  
replace ideo7 = . if ideo7 > 7
clonevar ideo3 = VCF0804  
replace ideo3 = . if ideo3 > 3

foreach i in pid7 pid3 pstr ideo3 ideo7 race {
	recode `i' 0 = .
}


*** 3. Replicate Feeling Thermoters

* White - Black

clonevar ft_black = VCF0206  
clonevar ft_white = VCF0207
clonevar ft_hispanic = VCF0217

* Republicans - Democrats

clonevar ft_gop = VCF0224

clonevar ft_dem = VCF0218


foreach i in ft_black ft_white ft_hispanic ft_dem ft_gop {
	recode `i' 98/max = .
}

* Racial Resentment

gen rr1 =  VCF9039-1 if VCF9039>0 & VCF9039<8
gen rr2 =  5-VCF9040  if VCF9040>0 & VCF9040<8
gen rr3 =  5-VCF9041  if VCF9041>0 & VCF9041<8
gen rr4 =  VCF9042-1   if VCF9042>0 & VCF9042<8
alpha rr1-rr4  
tab rr4

gen rr=(rr1+rr2+rr3+rr4)/16
sum rr

* socioeconomic conditions

clonevar education = VCF0110  
clonevar income = VCF0114  
clonevar age2 = VCF0102  
clonevar age = VCF0101
clonevar gender = VCF0104  

foreach i in education income age2 age gender {
	recode `i' 0 = .
}


drop VCF* Version


save "/Users/chaoyuewang/Desktop/Measure Party Image/data/ANES cleaned.dta",replace

save "/Users/chaoyuewang/Desktop/Grammar of Graphics/Coefficients/data/ANES cleaned.dta",replace












 
