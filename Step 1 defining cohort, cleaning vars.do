clear
use  "Y:\STATA\NCDB\Data Sets\2022\Intrahepatic Bile Duct 2022.dta"

codebook PUF_CASE_ID
fre HISTOLOGY
*8140 = adenocarcinoma, 8160=cholangiocarcinoma 8161=cystadenocarcinoma 8010=carcinoma 8020=carcinoma
keep if HISTOLOGY == "8140" | HISTOLOGY == "8160" | HISTOLOGY == "8161" | HISTOLOGY == "8010" | HISTOLOGY == "8020"
fre HISTOLOGY
fre PRIMARY_SITE
*All ICD c22.1 = intrahepatic bile duct

fre RX_SUMM_SURG_PRIM_SITE
gen transplant = 0
/* Transplant definitions
61 Total hepatectomy and transplant 
75 Extrahepatic bile duct and hepatectomy WITH transplant
*/
replace transplant = 1 if RX_SUMM_SURG_PRIM_SITE == "61" | RX_SUMM_SURG_PRIM_SITE == "75"

gen resection = 0
/* Resection definitions
20 Wedge or segmental resection, NOS 
21 Wedge resection 
22 Segmental resection, NOS 
	23 One 
	24 Two 
	25 Three 
	26 segmental resection and LTD
30 Lobectomy, NOS 
36 Right lobectomy 
37 Left lobectomy 
38 Lobectomy and LTD
50 Extended lobectomy, NOS (extended: resection of a single lobe plus a segment of another lobe)
51 Right lobectomy 
52 Left lobectomy 
59 extended lobectomy and LTD
60 hepatectomy
65 excision of bile duct
66 Excision of an intrahepatic bile duct PLUS partial hepatectomy
*/
replace resection = 1 if RX_SUMM_SURG_PRIM_SITE == "20" | RX_SUMM_SURG_PRIM_SITE == "21" | RX_SUMM_SURG_PRIM_SITE == "22" | RX_SUMM_SURG_PRIM_SITE == "23" | RX_SUMM_SURG_PRIM_SITE == "24" | RX_SUMM_SURG_PRIM_SITE == "25" | RX_SUMM_SURG_PRIM_SITE == "26" | RX_SUMM_SURG_PRIM_SITE == "30" | RX_SUMM_SURG_PRIM_SITE == "36" | RX_SUMM_SURG_PRIM_SITE == "37" | RX_SUMM_SURG_PRIM_SITE == "38" || RX_SUMM_SURG_PRIM_SITE == "50" | RX_SUMM_SURG_PRIM_SITE == "51" | RX_SUMM_SURG_PRIM_SITE == "52" | RX_SUMM_SURG_PRIM_SITE == "59" |   RX_SUMM_SURG_PRIM_SITE == "60" | RX_SUMM_SURG_PRIM_SITE == "65" | RX_SUMM_SURG_PRIM_SITE == "66" 

gen surgery = 0
replace surgery = 1 if transplant == 1 | resection == 1
fre surgery

fre TNM_CLIN_T
fre AJCC_TNM_CLIN_T
replace TNM_CLIN_T = trim(TNM_CLIN_T)
drop if TNM_CLIN_T == "88" | TNM_CLIN_T == "cX" | TNM_CLIN_T == "c0" | TNM_CLIN_T == "pIS"
drop if AJCC_TNM_CLIN_T == "88" | AJCC_TNM_CLIN_T == "cT0" | AJCC_TNM_CLIN_T == "cTX" | AJCC_TNM_CLIN_T == "cTis"

*need to combine clinical stage variables into one variable
gen cTStage = "."
replace cTStage = "1" if AJCC_TNM_CLIN_T == "cT1" | AJCC_TNM_CLIN_T == "cT1a" | AJCC_TNM_CLIN_T == "cT1b" | TNM_CLIN_T == "C1" | TNM_CLIN_T == "c1"
replace cTStage = "2" if AJCC_TNM_CLIN_T == "cT2" | TNM_CLIN_T == "c2" | TNM_CLIN_T == "c2A" | TNM_CLIN_T == "c2B" | TNM_CLIN_T == "C2B"
replace cTStage = "3" if AJCC_TNM_CLIN_T == "cT3" | TNM_CLIN_T == "c3" | TNM_CLIN_T == "c3A"
replace cTStage = "4" if AJCC_TNM_CLIN_T == "cT4"| TNM_CLIN_T == "c4"
fre cTStage

count if AJCC_TNM_CLIN_T == "" & TNM_CLIN_T == ""
***dropping those with missing T stage info
drop if cTStage == "."

fre TNM_CLIN_N
fre AJCC_TNM_CLIN_N
replace TNM_CLIN_N = trim(TNM_CLIN_N)
gen cNStage = "."
replace cNStage = "X" if TNM_CLIN_N == "cX" | AJCC_TNM_CLIN_N == "cNX"
replace cNStage = "0" if TNM_CLIN_N == "C0" | TNM_CLIN_N == "c0" | AJCC_TNM_CLIN_N == "cN0"
replace cNStage = "1" if TNM_CLIN_N == "C1" | TNM_CLIN_N == "c1" | AJCC_TNM_CLIN_N == "cN1"
replace cNStage = "2" if TNM_CLIN_N == "c2"
fre cNStage
drop if cNStage == "." | cNStage == "X" | cNStage == "2"

fre TNM_CLIN_M
fre AJCC_TNM_CLIN_M
replace TNM_CLIN_M = trim(TNM_CLIN_M)
gen cMStage = "."
replace cMStage = "X" if TNM_CLIN_M == "cX"
replace cMStage = "0" if TNM_CLIN_M == "0" | TNM_CLIN_M == "c0" | AJCC_TNM_CLIN_M == "cM0"
replace cMStage = "1" if TNM_CLIN_M == "C1" | TNM_CLIN_M == "c1" | AJCC_TNM_CLIN_M == "cM1" 
fre cMStage
count if AJCC_TNM_CLIN_M == "" & TNM_CLIN_M == ""
***Dropping those with missing info, those with distant mets, and those with pathological stage recorded as we then wouldn't know what their clinical stage would be
drop if cMStage == "." 
drop if cMStage == "1"
drop if cMStage == "X"
fre cMStage

gen stage = "."
replace stage = "1" if cTStage == "1" & cNStage == "0"
replace stage = "2" if cTStage == "2" & cNStage == "0"
replace stage = "3" if cTStage == "3" & cNStage == "0"
replace stage = "4" if cTStage == "4" | cNStage != "0"
fre stage

fre surgery
fre HISTOLOGY

fre RX_SUMM_IMMUNOTHERAPY
gen immunotx = 0
drop if RX_SUMM_IMMUNOTHERAPY == "" | RX_SUMM_IMMUNOTHERAPY == "99" | RX_SUMM_IMMUNOTHERAPY == "88" | RX_SUMM_IMMUNOTHERAPY == "85"
replace immunotx = 1 if RX_SUMM_IMMUNOTHERAPY == "01"
fre immunotx

fre REASON_FOR_NO_RADIATION
**Drop if unknown if got radiation or if died before getting it
drop if REASON_FOR_NO_RADIATION == "9" | REASON_FOR_NO_RADIATION == "5" | REASON_FOR_NO_RADIATION == "8"
gen xrt = 0
replace xrt = 1 if REASON_FOR_NO_RADIATION == "0"
fre xrt

fre AGE
destring AGE, replace
gen age_bin = "."
replace age_bin = "18-54" if AGE <55
replace age_bin = "55-64" if AGE >54 & AGE <65
replace age_bin = "65-74" if AGE >64 & AGE <75
replace age_bin = "75+" if AGE >74
fre age_bin
drop AGE
rename age_bin AGE, replace

fre SEX
replace SEX = "Female" if SEX == "2"
replace SEX = "Male" if SEX == "1"
 
fre RACE
replace RACE = "White" if RACE == "01"
replace RACE = "Black" if RACE == "02"
replace RACE = "Other" if RACE == "03" | RACE == "98" | RACE == "99"
replace RACE = "Other" if RACE == "07" | RACE == "25" | RACE == "27" | RACE == "97" | RACE == "21" | RACE == "22" | RACE == "28" | RACE == "30" | RACE == "31" | RACE == "20" 
replace RACE = "Other" if RACE == "04" | RACE == "05" | RACE == "06" | RACE == "08" | RACE == "10" | RACE == "11" | RACE == "13" | RACE == "14" | RACE == "15" | RACE == "16" | RACE == "17" | RACE == "96" | RACE == "12" 

fre SPANISH_HISPANIC_ORIGIN 
replace SPANISH_HISPANIC_ORIGIN = "1" if SPANISH_HISPANIC_ORIGIN == "2" | SPANISH_HISPANIC_ORIGIN == "3" | SPANISH_HISPANIC_ORIGIN == "4" | SPANISH_HISPANIC_ORIGIN == "5" | SPANISH_HISPANIC_ORIGIN == "6" | SPANISH_HISPANIC_ORIGIN == "7" | SPANISH_HISPANIC_ORIGIN == "8"
drop if SPANISH_HISPANIC_ORIGIN == "9"
rename SPANISH_HISPANIC_ORIGIN hispanic
fre hispanic

fre INSURANCE_STATUS
*Other = not insured, unknown
*government = other government, medicare, medicaid
replace INSURANCE_STATUS = "other" if INSURANCE_STATUS == "0" | INSURANCE_STATUS == "9"
replace INSURANCE_STATUS = "private" if INSURANCE_STATUS == "1"
replace INSURANCE_STATUS = "government" if INSURANCE_STATUS == "2" | INSURANCE_STATUS == "4" | INSURANCE_STATUS == "3"
fre INSURANCE_STATUS

fre MED_INC_QUAR_2020
count if MED_INC_QUAR_00 == "" & MED_INC_QUAR_12 == "" & MED_INC_QUAR_2016  == "" & MED_INC_QUAR_2020  == "" & surgery == 1

count if NO_HSD_QUAR_2020 == "" & NO_HSD_QUAR_2016 == "" & NO_HSD_QUAR_12 == "" & NO_HSD_QUAR_00 == "" & surgery == 1
**WIll lose 16% of surgery cohort if these variables are included

fre surgery

fre RX_SUMM_SURGICAL_MARGINS
drop if RX_SUMM_SURGICAL_MARGINS == "9" | RX_SUMM_SURGICAL_MARGINS == "1" | RX_SUMM_SURGICAL_MARGINS == "7"
replace RX_SUMM_SURGICAL_MARGINS = "No surgery" if RX_SUMM_SURGICAL_MARGINS == "8"
replace RX_SUMM_SURGICAL_MARGINS = "R0" if RX_SUMM_SURGICAL_MARGINS == "0"
replace RX_SUMM_SURGICAL_MARGINS = "R1" if RX_SUMM_SURGICAL_MARGINS == "2"
replace RX_SUMM_SURGICAL_MARGINS = "R2" if RX_SUMM_SURGICAL_MARGINS == "3"
rename RX_SUMM_SURGICAL_MARGINS margins, replace
fre margins

fre CDCC_TOTAL_BEST
rename CDCC_TOTAL_BEST CDS, replace

fre RX_SUMM_CHEMO
*dropping unknown, or died prior to receiving, or medically unfit to receive
drop if RX_SUMM_CHEMO == "99" | RX_SUMM_CHEMO == "88" | RX_SUMM_CHEMO == "85" | RX_SUMM_CHEMO == "82"
gen chemo = .
replace chemo = 1 if RX_SUMM_CHEMO == "01" | RX_SUMM_CHEMO == "02" | RX_SUMM_CHEMO == "03"
replace chemo = 0 if RX_SUMM_CHEMO == "82" | RX_SUMM_CHEMO == "86" | RX_SUMM_CHEMO == "87" | RX_SUMM_CHEMO == "00"
fre chemo

fre DX_CHEMO_STARTED_DAYS
count if DX_CHEMO_STARTED_DAYS == "" & chemo == 1
*dropping if we know they got chemo but don't know the days from dx to starting chemo
drop if DX_CHEMO_STARTED_DAYS == "" & chemo == 1

fre DX_IMMUNO_STARTED_DAYS 
count if immunotx == 1 & DX_IMMUNO_STARTED_DAYS  == ""
drop if DX_IMMUNO_STARTED_DAYS == "" & immunotx == 1

fre DX_DEFSURG_STARTED_DAYS
count if DX_DEFSURG_STARTED_DAYS == "" & surgery == 1
drop if DX_DEFSURG_STARTED_DAYS == "" & surgery == 1

fre METS_AT_DX_DISTANT_LN
drop if METS_AT_DX_DISTANT_LN == "1"

fre PUF_VITAL_STATUS
*don't have any survival info on people from 2022
count if PUF_VITAL_STATUS == "" & YEAR == "2022"

fre DX_LASTCONTACT_DEATH_MONTHS 
codebook DX_LASTCONTACT_DEATH_MONTHS 
replace DX_LASTCONTACT_DEATH_MONTHS = regexr(DX_LASTCONTACT_DEATH_MONTHS, "^0+", "")
fre DX_LASTCONTACT_DEATH_MONTHS

drop if YEAR == "2022"
drop if DX_LASTCONTACT_DEATH_MONTHS == ""

fre PALLIATIVE_CARE
**Drop those that had palliative surgery or radiation. Chemo definition is vague and so won't exlude people just based on if they receied "palliative" chemo
drop if PALLIATIVE_CARE == "1" | PALLIATIVE_CARE == "2" | PALLIATIVE_CARE == "5" | PALLIATIVE_CARE == "6"

fre surgery

save "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 1 define cohort.dta", replace