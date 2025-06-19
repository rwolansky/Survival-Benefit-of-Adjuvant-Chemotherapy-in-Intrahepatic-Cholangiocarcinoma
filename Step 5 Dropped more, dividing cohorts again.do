clear
use "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 3 coding tumor size.dta"

* Analysis of time from surgery to chemotherapy start
* Create numeric variables for timing analysis
destring DX_SURG_STARTED_DAYS, generate(surg_start_days) force
destring DX_CHEMO_STARTED_DAYS, generate(chemo_start_days) force
* Calculate days from surgery to chemotherapy start
gen surg_to_chemo_days = chemo_start_days - surg_start_days
* Identify patients who had surgery followed by chemotherapy
gen adjuvant_chemo_patients = (tx_pattern == "Adjuvant chemo" | tx_pattern == "Adjuvant mixed")
* Display results for adjuvant chemotherapy patients
di "=== TIME FROM SURGERY TO CHEMOTHERAPY START ==="
di ""
* Count of patients
count if adjuvant_chemo_patients == 1 & surg_to_chemo_days != .
local n_patients = r(N)
di "Number of patients with surgery → chemotherapy: `n_patients'"
di ""
* Summary statistics
if `n_patients' > 0 {
    sum surg_to_chemo_days if adjuvant_chemo_patients == 1, detail
    
    * Get specific values
    qui sum surg_to_chemo_days if adjuvant_chemo_patients == 1
    local min_days = r(min)
    local max_days = r(max)
    local mean_days = round(r(mean), 0.1)
    local median_days = r(p50)
    
    di "Range: `min_days' to `max_days' days"
    di "Mean: `mean_days' days"
    di "Median: `median_days' days"
    di ""
    
    * Show distribution
    di "=== DISTRIBUTION OF DAYS FROM SURGERY TO CHEMOTHERAPY ==="
    tab surg_to_chemo_days if adjuvant_chemo_patients == 1, m
}

* Show current treatment patterns before reclassification
di ""
di "=== TREATMENT PATTERNS BEFORE RECLASSIFICATION ==="
tab tx_pattern, m

* Count patients who will be reclassified
count if surg_to_chemo_days > 112 & tx_pattern == "Adjuvant chemo"
local n_reclassify = r(N)
di ""
di "Number of 'Adjuvant chemo' patients with >112 days to chemo start: `n_reclassify'"
di "These patients will be reclassified to 'Surgery only'"

* Reclassify patients with delayed chemotherapy (>112 days) to surgery only
replace tx_pattern = "Surgery only" if surg_to_chemo_days > 112 & tx_pattern == "Adjuvant chemo"

* Show treatment patterns after reclassification
di ""
di "=== TREATMENT PATTERNS AFTER RECLASSIFICATION ==="
tab tx_pattern, m

* Verify the reclassification worked
di ""
di "=== VERIFICATION OF RECLASSIFICATION ==="
count if surg_to_chemo_days > 112 & tx_pattern == "Adjuvant chemo"
di "Remaining 'Adjuvant chemo' patients with >112 days (should be 0): " r(N)

* Show final adjuvant chemo timing distribution
di ""
di "=== FINAL ADJUVANT CHEMO TIMING (≤112 days only) ==="
sum surg_to_chemo_days if tx_pattern == "Adjuvant chemo", detail

* Clean up temporary variables
drop surg_start_days chemo_start_days


* Look at people that had another malignancy besides ICCA *
fre SEQUENCE_NUMBER
***Will lose approx 30% of cohort
drop if SEQUENCE_NUMBER != "00"

* Analysis of radiation timing relative to surgery
* Create numeric variables for timing analysis
destring DX_RAD_STARTED_DAYS, generate(rad_start_days) force
destring DX_DEFSURG_STARTED_DAYS, generate(surg_start_days) force

* Calculate days between radiation and surgery
gen rad_to_surg_days = surg_start_days - rad_start_days
gen surg_to_rad_days = rad_start_days - surg_start_days

* Create radiation timing categories
gen radiation_timing = ""
replace radiation_timing = "No radiation" if xrt == 0
replace radiation_timing = "Preoperative" if xrt == 1 & rad_to_surg_days > 0 & rad_to_surg_days != .
replace radiation_timing = "Postoperative" if xrt == 1 & surg_to_rad_days > 0 & surg_to_rad_days != .
replace radiation_timing = "Same day/Unknown timing" if xrt == 1 & rad_to_surg_days == 0
replace radiation_timing = "Missing timing data" if xrt == 1 & (rad_to_surg_days == . | surg_to_rad_days == .)

* Display radiation timing before exclusions
di "=== RADIATION TIMING BEFORE EXCLUSIONS ==="
tab radiation_timing, m

* Count patients who will be dropped
count if radiation_timing == "Preoperative"
local n_preop = r(N)
count if radiation_timing == "Same day/Unknown timing"
local n_unknown = r(N)
count if radiation_timing == "Missing timing data"
local n_missing = r(N)

di ""
di "Number of patients with preoperative radiation (will be dropped): `n_preop'"
di "Number of patients with same day/unknown timing (will be dropped): `n_unknown'"
di "Number of patients with missing timing data (will be dropped): `n_missing'"
local total_dropped = `n_preop' + `n_unknown' + `n_missing'
di "Total patients to be dropped: `total_dropped'"

* DROP patients who had radiation before surgery OR unknown/missing timing
drop if radiation_timing == "Preoperative"
drop if radiation_timing == "Same day/Unknown timing"
drop if radiation_timing == "Missing timing data"

* Display radiation timing after dropping excluded cases
di ""
di "=== RADIATION TIMING AFTER EXCLUSIONS ==="
tab radiation_timing, m

* Now reassign xrt variable based on postoperative radiation timing
* xrt = 1 if postoperative radiation within 112 days
* xrt = 0 if no radiation OR postoperative radiation after 112 days

* First, show current postoperative radiation timing
di ""
di "=== POSTOPERATIVE RADIATION TIMING ANALYSIS ==="
count if radiation_timing == "Postoperative"
local n_postop = r(N)

if `n_postop' > 0 {
    di "Number of patients with postoperative radiation: `n_postop'"
    sum surg_to_rad_days if radiation_timing == "Postoperative", detail
    
    * Count those within and beyond 112 days
    count if radiation_timing == "Postoperative" & surg_to_rad_days <= 112
    local n_within_112 = r(N)
    count if radiation_timing == "Postoperative" & surg_to_rad_days > 112
    local n_beyond_112 = r(N)
    
    di "Postoperative radiation ≤112 days: `n_within_112'"
    di "Postoperative radiation >112 days: `n_beyond_112'"
}

* Reassign xrt variable
* xrt = 1 for postoperative radiation within 112 days
* xrt = 0 for no radiation or postoperative radiation after 112 days
replace xrt = 0  // First set all to 0
replace xrt = 1 if radiation_timing == "Postoperative" & surg_to_rad_days <= 112 & surg_to_rad_days != .

* Display final xrt assignment
di ""
di "=== FINAL XRT ASSIGNMENT ==="
tab xrt, m

* Cross-check with radiation timing
di ""
di "=== CROSS-CHECK: XRT BY RADIATION TIMING ==="
tab radiation_timing xrt, m row col

* Verify the logic worked correctly
di ""
di "=== VERIFICATION ==="
count if xrt == 1 & radiation_timing != "Postoperative"
di "Patients with xrt=1 but not postoperative radiation (should be 0): " r(N)

count if xrt == 1 & surg_to_rad_days > 112
di "Patients with xrt=1 but radiation >112 days post-surgery (should be 0): " r(N)

count if xrt == 0 & radiation_timing == "Postoperative" & surg_to_rad_days <= 112
di "Patients with xrt=0 but postop radiation ≤112 days (should be 0): " r(N)

* Show final treatment patterns
di ""
di "=== FINAL TREATMENT PATTERNS ==="
tab tx_pattern, m

* Clean up temporary variables
drop rad_start_days surg_start_days rad_to_surg_days surg_to_rad_days radiation_timing

fre tx_pattern

save "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Additional Drop.dta", replace

** Redo cohorts of interest **
**# Pathologic stage, all stages

gen adjchemo = 0
replace adjchemo = 1 if tx_pattern == "Adjuvant chemo"
gen neoadjchemo = 0
replace neoadjchemo = 1 if tx_pattern == "Neoadjuvant chemo"
gen neoandadjchemo = 0 
replace neoandadjchemo = 1 if tx_pattern == "Neo + Adj chemo"
gen surgonly = 0
replace surgonly = 1 if tx_pattern == "Surgery only"

fre tx_pattern

fre margins
rename TUMOR_SIZE_GROUP tumorsizegroup, replace
rename PUF_VITAL_STATUS mortality, replace
rename DX_LASTCONTACT_DEATH_MONTHS lastcontact, replace
destring lastcontact, replace
drop if transplant == 1

****MORTALITY CODED BACKWARDS IN DATA DICTIONARY
replace mortality = "Dead" if mortality == "0"
replace mortality = "Alive" if mortality == "1"
fre mortality
replace mortality = "1" if mortality == "Dead"
replace mortality = "0" if mortality == "Alive"

fre GRADE
fre GRADE_CLIN
fre GRADE_PATH

* Create a new combined grade variable as a string
generate GRADE_COMBINED = ""

* Fill in the combined variable using the first non-missing value
* First try GRADE
replace GRADE_COMBINED = GRADE if GRADE != ""

* Then try GRADE_CLIN if GRADE is missing
replace GRADE_COMBINED = GRADE_CLIN if GRADE_COMBINED == "" & GRADE_CLIN != ""

* Finally try GRADE_PATH if both others are missing
replace GRADE_COMBINED = GRADE_PATH if GRADE_COMBINED == "" & GRADE_PATH != ""

* Label the new variable
label variable GRADE_COMBINED "Combined tumor grade from all sources"

* Check the results
tab GRADE_COMBINED, missing
drop if GRADE_COMBINED == "9"
replace GRADE_COMBINED = "Well diff" if GRADE_COMBINED == "1"
replace GRADE_COMBINED = "Moderately diff" if GRADE_COMBINED == "2"
replace GRADE_COMBINED = "Poorly diff" if GRADE_COMBINED == "3"
replace GRADE_COMBINED = "Undiff" if GRADE_COMBINED == "4"

keep if adjchemo == 1 | surgonly == 1

fre REGIONAL_NODES_EXAMINED
rename REGIONAL_NODES_EXAMINED LAD, replace
replace LAD = "not examined" if LAD == "00"
drop if LAD == "99"
fre LAD
replace LAD = "examined" if LAD != "not examined"

fre REGIONAL_NODES_POSITIVE
drop if REGIONAL_NODES_POSITIVE == "99"
replace LAD = "examined, negative" if REGIONAL_NODES_POSITIVE == "00"
replace LAD = "examined, positive" if REGIONAL_NODES_POSITIVE == "01" | REGIONAL_NODES_POSITIVE == "02" | REGIONAL_NODES_POSITIVE == "03" | REGIONAL_NODES_POSITIVE == "04" | REGIONAL_NODES_POSITIVE == "05" | REGIONAL_NODES_POSITIVE == "06" | REGIONAL_NODES_POSITIVE == "07" | REGIONAL_NODES_POSITIVE == "08" | REGIONAL_NODES_POSITIVE == "09" | REGIONAL_NODES_POSITIVE == "10" | REGIONAL_NODES_POSITIVE == "11" | REGIONAL_NODES_POSITIVE == "13" | REGIONAL_NODES_POSITIVE == "23" | REGIONAL_NODES_POSITIVE == "25" |REGIONAL_NODES_POSITIVE == "32" | REGIONAL_NODES_POSITIVE == "34" | REGIONAL_NODES_POSITIVE == "95" | REGIONAL_NODES_POSITIVE == "97"
fre LAD

fre TNM_PATH_T
fre AJCC_TNM_PATH_T
replace TNM_PATH_T = trim(TNM_PATH_T)
drop if TNM_PATH_T == "pX" | TNM_PATH_T == "p0" 
drop if AJCC_TNM_PATH_T == "pTX" | AJCC_TNM_PATH_T == "pTis" 

*need to combine pathological stage variables into one variable
gen pTStage = "."
replace pTStage = "1" if AJCC_TNM_PATH_T == "pT1" | AJCC_TNM_PATH_T == "pT1a" | AJCC_TNM_PATH_T == "pT1b" | TNM_PATH_T == "p1" 
replace pTStage = "2" if AJCC_TNM_PATH_T == "pT2" | TNM_PATH_T == "p2" | TNM_PATH_T == "p2A" | TNM_PATH_T == "p2B"
replace pTStage = "3" if AJCC_TNM_PATH_T == "pT3" | TNM_PATH_T == "p3"
replace pTStage = "4" if AJCC_TNM_PATH_T == "pT4"| TNM_PATH_T == "p4"
fre pTStage

count if TNM_PATH_T == "" & AJCC_TNM_PATH_T == ""
drop if TNM_PATH_T == "" & AJCC_TNM_PATH_T == ""

/* edit before using
table1_mc, by(adjchemo) vars (AGE cat \ SEX cat \ RACE cat \ hispanic cat \ INSURANCE_STATUS cat \ FACILITY_TYPE_CD cat \ CDS cat \ stage cat \ margins cat \ GRADE_COMBINED cat \ LAD cat \ tumorsizegroup cat \ xrt cat \ lastcontact conts \ mortality cat ) saving ("TABLE Adjuvant vs Surgery *** .xlsx", replace)
*/

drop if YEAR == "2022" | YEAR == "2021" | YEAR == "2020" | YEAR == "2019" | YEAR == "2018" 
replace margins = "positive" if margins == "R1" | margins == "R2"
replace margins = "negative" if margins == "R0"

fre mortality
fre tx_pattern

keep AGE SEX RACE INSURANCE_STATUS CDS pTStage margins GRADE_COMBINED LAD xrt lastcontact mortality adjchemo

export delimited "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 4 Adj vs Surgery pstage 2006-2017.csv", replace

**# Clinical Stage, all stages
clear
use "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Additional Drop.dta"


gen adjchemo = 0
replace adjchemo = 1 if tx_pattern == "Adjuvant chemo"
gen neoadjchemo = 0
replace neoadjchemo = 1 if tx_pattern == "Neoadjuvant chemo"
gen neoandadjchemo = 0 
replace neoandadjchemo = 1 if tx_pattern == "Neo + Adj chemo"
gen surgonly = 0
replace surgonly = 1 if tx_pattern == "Surgery only"

fre margins
rename TUMOR_SIZE_GROUP tumorsizegroup, replace
rename PUF_VITAL_STATUS mortality, replace
rename DX_LASTCONTACT_DEATH_MONTHS lastcontact, replace
destring lastcontact, replace

****MORTALITY CODED BACKWARDS IN DATA DICTIONARY
replace mortality = "Dead" if mortality == "0"
replace mortality = "Alive" if mortality == "1"
fre mortality
replace mortality = "1" if mortality == "Dead"
replace mortality = "0" if mortality == "Alive"

fre GRADE
fre GRADE_CLIN
fre GRADE_PATH

* Create a new combined grade variable as a string
generate GRADE_COMBINED = ""

* Fill in the combined variable using the first non-missing value
* First try GRADE
replace GRADE_COMBINED = GRADE if GRADE != ""

* Then try GRADE_CLIN if GRADE is missing
replace GRADE_COMBINED = GRADE_CLIN if GRADE_COMBINED == "" & GRADE_CLIN != ""

* Finally try GRADE_PATH if both others are missing
replace GRADE_COMBINED = GRADE_PATH if GRADE_COMBINED == "" & GRADE_PATH != ""

* Label the new variable
label variable GRADE_COMBINED "Combined tumor grade from all sources"

* Check the results
tab GRADE_COMBINED, missing
drop if GRADE_COMBINED == "9"
replace GRADE_COMBINED = "Well diff" if GRADE_COMBINED == "1"
replace GRADE_COMBINED = "Moderately diff" if GRADE_COMBINED == "2"
replace GRADE_COMBINED = "Poorly diff" if GRADE_COMBINED == "3"
replace GRADE_COMBINED = "Undiff" if GRADE_COMBINED == "4"

drop if transplant == 1
fre REGIONAL_NODES_EXAMINED
rename REGIONAL_NODES_EXAMINED LAD, replace
replace LAD = "not examined" if LAD == "00"
drop if LAD == "99"
fre LAD
replace LAD = "examined" if LAD != "not examined"

fre REGIONAL_NODES_POSITIVE
drop if REGIONAL_NODES_POSITIVE == "99"
replace LAD = "examined, negative" if REGIONAL_NODES_POSITIVE == "00"
replace LAD = "examined, positive" if REGIONAL_NODES_POSITIVE == "01" | REGIONAL_NODES_POSITIVE == "02" | REGIONAL_NODES_POSITIVE == "03" | REGIONAL_NODES_POSITIVE == "04" | REGIONAL_NODES_POSITIVE == "05" | REGIONAL_NODES_POSITIVE == "06" | REGIONAL_NODES_POSITIVE == "07" | REGIONAL_NODES_POSITIVE == "08" | REGIONAL_NODES_POSITIVE == "09" | REGIONAL_NODES_POSITIVE == "10" | REGIONAL_NODES_POSITIVE == "11" | REGIONAL_NODES_POSITIVE == "25" | REGIONAL_NODES_POSITIVE == "97"
fre LAD

keep if adjchemo == 1 | surgonly == 1
fre adjchemo

drop if YEAR == "2022" | YEAR == "2021" | YEAR == "2020" | YEAR == "2019" | YEAR == "2018" 

replace margins = "positive" if margins == "R1" | margins == "R2"
replace margins = "negative" if margins == "R0"

keep AGE SEX RACE INSURANCE_STATUS CDS stage margins GRADE_COMBINED LAD xrt lastcontact mortality adjchemo

export delimited "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 4 Adj vs Surgery 2006-2017 w clinical stage.csv", replace

**# Pathologic stage 3 and 4
clear 
use "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Additional Drop.dta"

gen adjchemo = 0
replace adjchemo = 1 if tx_pattern == "Adjuvant chemo"
gen neoadjchemo = 0
replace neoadjchemo = 1 if tx_pattern == "Neoadjuvant chemo"
gen neoandadjchemo = 0 
replace neoandadjchemo = 1 if tx_pattern == "Neo + Adj chemo"
gen surgonly = 0
replace surgonly = 1 if tx_pattern == "Surgery only"

fre tx_pattern

fre margins
rename TUMOR_SIZE_GROUP tumorsizegroup, replace
rename PUF_VITAL_STATUS mortality, replace
rename DX_LASTCONTACT_DEATH_MONTHS lastcontact, replace
destring lastcontact, replace
drop if transplant == 1

replace mortality = "Dead" if mortality == "0"
replace mortality = "Alive" if mortality == "1"
fre mortality
replace mortality = "1" if mortality == "Dead"
replace mortality = "0" if mortality == "Alive"

keep if adjchemo == 1 | surgonly == 1

fre REGIONAL_NODES_EXAMINED
rename REGIONAL_NODES_EXAMINED LAD, replace
replace LAD = "not examined" if LAD == "00"
drop if LAD == "99"
fre LAD
replace LAD = "examined" if LAD != "not examined"

fre REGIONAL_NODES_POSITIVE
drop if REGIONAL_NODES_POSITIVE == "99"
replace LAD = "examined, negative" if REGIONAL_NODES_POSITIVE == "00"
replace LAD = "examined, positive" if REGIONAL_NODES_POSITIVE == "01" | REGIONAL_NODES_POSITIVE == "02" | REGIONAL_NODES_POSITIVE == "03" | REGIONAL_NODES_POSITIVE == "04" | REGIONAL_NODES_POSITIVE == "05" | REGIONAL_NODES_POSITIVE == "06" | REGIONAL_NODES_POSITIVE == "07" | REGIONAL_NODES_POSITIVE == "08" | REGIONAL_NODES_POSITIVE == "09" | REGIONAL_NODES_POSITIVE == "10" | REGIONAL_NODES_POSITIVE == "11" | REGIONAL_NODES_POSITIVE == "13" | REGIONAL_NODES_POSITIVE == "23" | REGIONAL_NODES_POSITIVE == "25" |REGIONAL_NODES_POSITIVE == "32" | REGIONAL_NODES_POSITIVE == "34" | REGIONAL_NODES_POSITIVE == "95" | REGIONAL_NODES_POSITIVE == "97"
fre LAD

fre TNM_PATH_T
fre AJCC_TNM_PATH_T
replace TNM_PATH_T = trim(TNM_PATH_T)
drop if TNM_PATH_T == "pX" | TNM_PATH_T == "p0" 
drop if AJCC_TNM_PATH_T == "pTX" | AJCC_TNM_PATH_T == "pTis" 

*need to combine pathological stage variables into one variable
gen pTStage = "."
replace pTStage = "1" if AJCC_TNM_PATH_T == "pT1" | AJCC_TNM_PATH_T == "pT1a" | AJCC_TNM_PATH_T == "pT1b" | TNM_PATH_T == "p1" 
replace pTStage = "2" if AJCC_TNM_PATH_T == "pT2" | TNM_PATH_T == "p2" | TNM_PATH_T == "p2A" | TNM_PATH_T == "p2B"
replace pTStage = "3" if AJCC_TNM_PATH_T == "pT3" | TNM_PATH_T == "p3"
replace pTStage = "4" if AJCC_TNM_PATH_T == "pT4"| TNM_PATH_T == "p4"
fre pTStage

count if TNM_PATH_T == "" & AJCC_TNM_PATH_T == ""
drop if TNM_PATH_T == "" & AJCC_TNM_PATH_T == ""

/* edit before using
table1_mc, by(adjchemo) vars (AGE cat \ SEX cat \ RACE cat \ hispanic cat \ INSURANCE_STATUS cat \ FACILITY_TYPE_CD cat \ CDS cat \ stage cat \ margins cat \ GRADE_COMBINED cat \ LAD cat \ tumorsizegroup cat \ xrt cat \ lastcontact conts \ mortality cat ) saving ("TABLE Adjuvant vs Surgery *** .xlsx", replace)
*/

drop if YEAR == "2022" | YEAR == "2021" | YEAR == "2020" | YEAR == "2019" | YEAR == "2018" 
replace margins = "positive" if margins == "R1" | margins == "R2"
replace margins = "negative" if margins == "R0"

fre pTStage
count if pTStage == "3" & LAD == "examined, positive" | pTStage == "4" & LAD == "examined, positive"
gen pStage = "."
replace pStage = "1" if pTStage == "1" & LAD == "examined, negative"
replace pStage = "1" if pTStage == "1" & LAD == "not examined"
replace pStage = "2" if pTStage == "2" & LAD == "examined, negative"
replace pStage = "2" if pTStage == "2" & LAD == "not examined"
replace pStage = "3" if pTStage == "3" & LAD == "examined, negative"
replace pStage = "3" if pTStage == "3" & LAD == "not examined"
replace pStage = "4" if pTStage == "4" | LAD == "examined, positive"
fre pStage

keep if pStage == "3" | pStage == "4"
drop pTStage
drop pStage
drop LAD

fre margins

keep AGE SEX RACE INSURANCE_STATUS CDS margins xrt lastcontact mortality adjchemo


export delimited "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 4 Adj vs Surgery pstage 3 and 4.csv", replace

**# Clinical stage 3 and 4
clear 
use "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Additional Drop.dta"

gen adjchemo = 0
replace adjchemo = 1 if tx_pattern == "Adjuvant chemo"
gen neoadjchemo = 0
replace neoadjchemo = 1 if tx_pattern == "Neoadjuvant chemo"
gen neoandadjchemo = 0 
replace neoandadjchemo = 1 if tx_pattern == "Neo + Adj chemo"
gen surgonly = 0
replace surgonly = 1 if tx_pattern == "Surgery only"

fre tx_pattern

fre margins
rename TUMOR_SIZE_GROUP tumorsizegroup, replace
rename PUF_VITAL_STATUS mortality, replace
rename DX_LASTCONTACT_DEATH_MONTHS lastcontact, replace
destring lastcontact, replace
drop if transplant == 1

****MORTALITY CODED BACKWARDS IN DATA DICTIONARY
replace mortality = "Dead" if mortality == "0"
replace mortality = "Alive" if mortality == "1"
fre mortality
replace mortality = "1" if mortality == "Dead"
replace mortality = "0" if mortality == "Alive"

fre GRADE
fre GRADE_CLIN
fre GRADE_PATH

* Create a new combined grade variable as a string
generate GRADE_COMBINED = ""

* Fill in the combined variable using the first non-missing value
* First try GRADE
replace GRADE_COMBINED = GRADE if GRADE != ""

* Then try GRADE_CLIN if GRADE is missing
replace GRADE_COMBINED = GRADE_CLIN if GRADE_COMBINED == "" & GRADE_CLIN != ""

* Finally try GRADE_PATH if both others are missing
replace GRADE_COMBINED = GRADE_PATH if GRADE_COMBINED == "" & GRADE_PATH != ""

* Label the new variable
label variable GRADE_COMBINED "Combined tumor grade from all sources"

* Check the results
tab GRADE_COMBINED, missing
drop if GRADE_COMBINED == "9"
replace GRADE_COMBINED = "Well diff" if GRADE_COMBINED == "1"
replace GRADE_COMBINED = "Moderately diff" if GRADE_COMBINED == "2"
replace GRADE_COMBINED = "Poorly diff" if GRADE_COMBINED == "3"
replace GRADE_COMBINED = "Undiff" if GRADE_COMBINED == "4"

keep if adjchemo == 1 | surgonly == 1

fre REGIONAL_NODES_EXAMINED
rename REGIONAL_NODES_EXAMINED LAD, replace
replace LAD = "not examined" if LAD == "00"
drop if LAD == "99"
fre LAD
replace LAD = "examined" if LAD != "not examined"

fre REGIONAL_NODES_POSITIVE
drop if REGIONAL_NODES_POSITIVE == "99"
replace LAD = "examined, negative" if REGIONAL_NODES_POSITIVE == "00"
replace LAD = "examined, positive" if REGIONAL_NODES_POSITIVE == "01" | REGIONAL_NODES_POSITIVE == "02" | REGIONAL_NODES_POSITIVE == "03" | REGIONAL_NODES_POSITIVE == "04" | REGIONAL_NODES_POSITIVE == "05" | REGIONAL_NODES_POSITIVE == "06" | REGIONAL_NODES_POSITIVE == "07" | REGIONAL_NODES_POSITIVE == "08" | REGIONAL_NODES_POSITIVE == "09" | REGIONAL_NODES_POSITIVE == "10" | REGIONAL_NODES_POSITIVE == "11" | REGIONAL_NODES_POSITIVE == "13" | REGIONAL_NODES_POSITIVE == "23" | REGIONAL_NODES_POSITIVE == "25" |REGIONAL_NODES_POSITIVE == "32" | REGIONAL_NODES_POSITIVE == "34" | REGIONAL_NODES_POSITIVE == "95" | REGIONAL_NODES_POSITIVE == "97"
fre LAD

fre stage
keep if stage == "3" | stage == "4"

replace margins = "positive" if margins == "R1" | margins == "R2"
replace margins = "negative" if margins == "R0"

drop if YEAR == "2022" | YEAR == "2021" | YEAR == "2020" | YEAR == "2019" | YEAR == "2018" 

keep AGE SEX RACE INSURANCE_STATUS CDS margins LAD xrt lastcontact mortality adjchemo

export delimited "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 4 Adj vs Surgery clinical stage 3 and 4.csv", replace

**# ***Margin positive cohort
clear
use "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Additional Drop.dta"

gen adjchemo = 0
replace adjchemo = 1 if tx_pattern == "Adjuvant chemo"
gen neoadjchemo = 0
replace neoadjchemo = 1 if tx_pattern == "Neoadjuvant chemo"
gen neoandadjchemo = 0 
replace neoandadjchemo = 1 if tx_pattern == "Neo + Adj chemo"
gen surgonly = 0
replace surgonly = 1 if tx_pattern == "Surgery only"

fre tx_pattern

fre margins
rename TUMOR_SIZE_GROUP tumorsizegroup, replace
rename PUF_VITAL_STATUS mortality, replace
rename DX_LASTCONTACT_DEATH_MONTHS lastcontact, replace
destring lastcontact, replace
drop if transplant == 1

****MORTALITY CODED BACKWARDS IN DATA DICTIONARY
replace mortality = "Dead" if mortality == "0"
replace mortality = "Alive" if mortality == "1"
fre mortality
replace mortality = "1" if mortality == "Dead"
replace mortality = "0" if mortality == "Alive"

keep if adjchemo == 1 | surgonly == 1

fre REGIONAL_NODES_EXAMINED
rename REGIONAL_NODES_EXAMINED LAD, replace
replace LAD = "not examined" if LAD == "00"
drop if LAD == "99"
fre LAD
replace LAD = "examined" if LAD != "not examined"

fre REGIONAL_NODES_POSITIVE
drop if REGIONAL_NODES_POSITIVE == "99"
replace LAD = "examined, negative" if REGIONAL_NODES_POSITIVE == "00"
replace LAD = "examined, positive" if REGIONAL_NODES_POSITIVE == "01" | REGIONAL_NODES_POSITIVE == "02" | REGIONAL_NODES_POSITIVE == "03" | REGIONAL_NODES_POSITIVE == "04" | REGIONAL_NODES_POSITIVE == "05" | REGIONAL_NODES_POSITIVE == "06" | REGIONAL_NODES_POSITIVE == "07" | REGIONAL_NODES_POSITIVE == "08" | REGIONAL_NODES_POSITIVE == "09" | REGIONAL_NODES_POSITIVE == "10" | REGIONAL_NODES_POSITIVE == "11" | REGIONAL_NODES_POSITIVE == "13" | REGIONAL_NODES_POSITIVE == "23" | REGIONAL_NODES_POSITIVE == "25" |REGIONAL_NODES_POSITIVE == "32" | REGIONAL_NODES_POSITIVE == "34" | REGIONAL_NODES_POSITIVE == "95" | REGIONAL_NODES_POSITIVE == "97"
fre LAD

keep if margins == "R1" | margins == "R2"
fre margins

fre TNM_PATH_T
fre AJCC_TNM_PATH_T
replace TNM_PATH_T = trim(TNM_PATH_T)
drop if TNM_PATH_T == "pX" | TNM_PATH_T == "p0" 
drop if AJCC_TNM_PATH_T == "pTX" | AJCC_TNM_PATH_T == "pTis" 

gen pTStage = "."
replace pTStage = "1" if AJCC_TNM_PATH_T == "pT1" | AJCC_TNM_PATH_T == "pT1a" | AJCC_TNM_PATH_T == "pT1b" | TNM_PATH_T == "p1" 
replace pTStage = "2" if AJCC_TNM_PATH_T == "pT2" | TNM_PATH_T == "p2" | TNM_PATH_T == "p2A" | TNM_PATH_T == "p2B"
replace pTStage = "3" if AJCC_TNM_PATH_T == "pT3" | TNM_PATH_T == "p3"
replace pTStage = "4" if AJCC_TNM_PATH_T == "pT4"| TNM_PATH_T == "p4"
fre pTStage

count if TNM_PATH_T == "" & AJCC_TNM_PATH_T == ""
drop if TNM_PATH_T == "" & AJCC_TNM_PATH_T == ""

fre pTStage
count if pTStage == "3" & LAD == "examined, positive" | pTStage == "4" & LAD == "examined, positive"
gen pStage = "."
replace pStage = "1" if pTStage == "1" & LAD == "examined, negative"
replace pStage = "1" if pTStage == "1" & LAD == "not examined"
replace pStage = "2" if pTStage == "2" & LAD == "examined, negative"
replace pStage = "2" if pTStage == "2" & LAD == "not examined"
replace pStage = "3" if pTStage == "3" & LAD == "examined, negative"
replace pStage = "3" if pTStage == "3" & LAD == "not examined"
replace pStage = "4" if pTStage == "4" | LAD == "examined, positive"
fre pStage

drop if YEAR == "2022" | YEAR == "2021" | YEAR == "2020" | YEAR == "2019" | YEAR == "2018" 

keep AGE SEX RACE INSURANCE_STATUS CDS xrt lastcontact mortality adjchemo pStage

export delimited "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 4 Adj vs Surgery margin positive.csv", replace

**# 
****Node positive with path stage
clear
use "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Additional Drop.dta"

gen adjchemo = 0
replace adjchemo = 1 if tx_pattern == "Adjuvant chemo"
gen neoadjchemo = 0
replace neoadjchemo = 1 if tx_pattern == "Neoadjuvant chemo"
gen neoandadjchemo = 0 
replace neoandadjchemo = 1 if tx_pattern == "Neo + Adj chemo"
gen surgonly = 0
replace surgonly = 1 if tx_pattern == "Surgery only"

fre tx_pattern

fre margins
rename TUMOR_SIZE_GROUP tumorsizegroup, replace
rename PUF_VITAL_STATUS mortality, replace
rename DX_LASTCONTACT_DEATH_MONTHS lastcontact, replace
destring lastcontact, replace
drop if transplant == 1

****MORTALITY CODED BACKWARDS IN DATA DICTIONARY
replace mortality = "Dead" if mortality == "0"
replace mortality = "Alive" if mortality == "1"
fre mortality
replace mortality = "1" if mortality == "Dead"
replace mortality = "0" if mortality == "Alive"

fre GRADE
fre GRADE_CLIN
fre GRADE_PATH

* Create a new combined grade variable as a string
generate GRADE_COMBINED = ""

* Fill in the combined variable using the first non-missing value
* First try GRADE
replace GRADE_COMBINED = GRADE if GRADE != ""

* Then try GRADE_CLIN if GRADE is missing
replace GRADE_COMBINED = GRADE_CLIN if GRADE_COMBINED == "" & GRADE_CLIN != ""

* Finally try GRADE_PATH if both others are missing
replace GRADE_COMBINED = GRADE_PATH if GRADE_COMBINED == "" & GRADE_PATH != ""

* Label the new variable
label variable GRADE_COMBINED "Combined tumor grade from all sources"

* Check the results
tab GRADE_COMBINED, missing
drop if GRADE_COMBINED == "9"
replace GRADE_COMBINED = "Well diff" if GRADE_COMBINED == "1"
replace GRADE_COMBINED = "Moderately diff" if GRADE_COMBINED == "2"
replace GRADE_COMBINED = "Poorly diff" if GRADE_COMBINED == "3"
replace GRADE_COMBINED = "Undiff" if GRADE_COMBINED == "4"

keep if adjchemo == 1 | surgonly == 1

fre REGIONAL_NODES_EXAMINED
rename REGIONAL_NODES_EXAMINED LAD, replace
replace LAD = "not examined" if LAD == "00"
drop if LAD == "99"
fre LAD
replace LAD = "examined" if LAD != "not examined"

fre REGIONAL_NODES_POSITIVE
drop if REGIONAL_NODES_POSITIVE == "99"
replace LAD = "examined, negative" if REGIONAL_NODES_POSITIVE == "00"
replace LAD = "examined, positive" if REGIONAL_NODES_POSITIVE == "01" | REGIONAL_NODES_POSITIVE == "02" | REGIONAL_NODES_POSITIVE == "03" | REGIONAL_NODES_POSITIVE == "04" | REGIONAL_NODES_POSITIVE == "05" | REGIONAL_NODES_POSITIVE == "06" | REGIONAL_NODES_POSITIVE == "07" | REGIONAL_NODES_POSITIVE == "08" | REGIONAL_NODES_POSITIVE == "09" | REGIONAL_NODES_POSITIVE == "10" | REGIONAL_NODES_POSITIVE == "11" | REGIONAL_NODES_POSITIVE == "13" | REGIONAL_NODES_POSITIVE == "23" | REGIONAL_NODES_POSITIVE == "25" |REGIONAL_NODES_POSITIVE == "32" | REGIONAL_NODES_POSITIVE == "34" | REGIONAL_NODES_POSITIVE == "95" | REGIONAL_NODES_POSITIVE == "97"
fre LAD

fre TNM_PATH_T
fre AJCC_TNM_PATH_T
replace TNM_PATH_T = trim(TNM_PATH_T)
drop if TNM_PATH_T == "pX" | TNM_PATH_T == "p0" 
drop if AJCC_TNM_PATH_T == "pTX" | AJCC_TNM_PATH_T == "pTis" 

*need to combine pathological stage variables into one variable
gen pTStage = "."
replace pTStage = "1" if AJCC_TNM_PATH_T == "pT1" | AJCC_TNM_PATH_T == "pT1a" | AJCC_TNM_PATH_T == "pT1b" | TNM_PATH_T == "p1" 
replace pTStage = "2" if AJCC_TNM_PATH_T == "pT2" | TNM_PATH_T == "p2" | TNM_PATH_T == "p2A" | TNM_PATH_T == "p2B"
replace pTStage = "3" if AJCC_TNM_PATH_T == "pT3" | TNM_PATH_T == "p3"
replace pTStage = "4" if AJCC_TNM_PATH_T == "pT4"| TNM_PATH_T == "p4"
fre pTStage

count if TNM_PATH_T == "" & AJCC_TNM_PATH_T == ""
drop if TNM_PATH_T == "" & AJCC_TNM_PATH_T == ""

/* edit before using
table1_mc, by(adjchemo) vars (AGE cat \ SEX cat \ RACE cat \ hispanic cat \ INSURANCE_STATUS cat \ FACILITY_TYPE_CD cat \ CDS cat \ stage cat \ margins cat \ GRADE_COMBINED cat \ LAD cat \ tumorsizegroup cat \ xrt cat \ lastcontact conts \ mortality cat ) saving ("TABLE Adjuvant vs Surgery *** .xlsx", replace)
*/

drop if YEAR == "2022" | YEAR == "2021" | YEAR == "2020" | YEAR == "2019" | YEAR == "2018" 
replace margins = "positive" if margins == "R1" | margins == "R2"
replace margins = "negative" if margins == "R0"

keep if LAD == "examined, positive"
fre tx_pattern

keep AGE SEX RACE INSURANCE_STATUS CDS pTStage margins GRADE_COMBINED xrt lastcontact mortality adjchemo
fre adjchemo

export delimited "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 4 Adj vs Surgery pstage node positive 2006-2017.csv", replace

