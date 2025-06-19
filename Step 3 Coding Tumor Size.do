clear
use "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 2 coded treatment sequence.dta"

fre TUMOR_SIZE
fre TUMOR_SIZE_SUMMARY_16

**Need to combine these two variables
* Stata code to combine TUMOR_SIZE and TUMOR_SIZE_SUMMARY_16 variables

* First, destring the tumor size variables if they're strings
capture confirm string variable TUMOR_SIZE
if !_rc {
    gen TUMOR_SIZE_num = .
    replace TUMOR_SIZE_num = 0 if TUMOR_SIZE == "000"
    replace TUMOR_SIZE_num = 999 if TUMOR_SIZE == "999"
    replace TUMOR_SIZE_num = real(TUMOR_SIZE) if TUMOR_SIZE != "000" & TUMOR_SIZE != "999" & TUMOR_SIZE != ""
}
else {
    rename TUMOR_SIZE TUMOR_SIZE_num
}

capture confirm string variable TUMOR_SIZE_SUMMARY_16
if !_rc {
    gen TUMOR_SIZE_SUMMARY_16_num = .
    replace TUMOR_SIZE_SUMMARY_16_num = 0 if TUMOR_SIZE_SUMMARY_16 == "000"
    replace TUMOR_SIZE_SUMMARY_16_num = 999 if TUMOR_SIZE_SUMMARY_16 == "999"
    replace TUMOR_SIZE_SUMMARY_16_num = real(TUMOR_SIZE_SUMMARY_16) if TUMOR_SIZE_SUMMARY_16 != "000" & TUMOR_SIZE_SUMMARY_16 != "999" & TUMOR_SIZE_SUMMARY_16 != ""
}
else {
    rename TUMOR_SIZE_SUMMARY_16 TUMOR_SIZE_SUMMARY_16_num
}

* Generate the combined variable
gen TUMOR_SIZE_COMBINED = .

* Process TUMOR_SIZE values (2004-2016)
gen temp_size_old = .
replace temp_size_old = 0 if TUMOR_SIZE_num == 0  // No tumor found
replace temp_size_old = . if TUMOR_SIZE_num == 999  // Unknown
* Remove leading zeros by converting to string, removing zeros, then back to numeric
tostring TUMOR_SIZE_num, gen(temp_str_old) force
replace temp_str_old = regexr(temp_str_old, "^0+", "") if TUMOR_SIZE_num != 0 & TUMOR_SIZE_num != 999 & !missing(TUMOR_SIZE_num)
replace temp_size_old = real(temp_str_old) if TUMOR_SIZE_num != 0 & TUMOR_SIZE_num != 999 & !missing(TUMOR_SIZE_num)
drop temp_str_old

* Process TUMOR_SIZE_SUMMARY_16 values (2016+)
gen temp_size_new = .
replace temp_size_new = 0 if TUMOR_SIZE_SUMMARY_16_num == 0  // No tumor found
replace temp_size_new = . if TUMOR_SIZE_SUMMARY_16_num == 999  // Unknown
* Remove leading zeros by converting to string, removing zeros, then back to numeric
tostring TUMOR_SIZE_SUMMARY_16_num, gen(temp_str_new) force
replace temp_str_new = regexr(temp_str_new, "^0+", "") if TUMOR_SIZE_SUMMARY_16_num != 0 & TUMOR_SIZE_SUMMARY_16_num != 999 & !missing(TUMOR_SIZE_SUMMARY_16_num)
replace temp_size_new = real(temp_str_new) if TUMOR_SIZE_SUMMARY_16_num != 0 & TUMOR_SIZE_SUMMARY_16_num != 999 & !missing(TUMOR_SIZE_SUMMARY_16_num)
drop temp_str_new

* Combine values based on availability (prefer newer values)
replace TUMOR_SIZE_COMBINED = temp_size_new if !missing(temp_size_new)
replace TUMOR_SIZE_COMBINED = temp_size_old if missing(TUMOR_SIZE_COMBINED) & !missing(temp_size_old)

* Clean up temporary variables
drop temp_size_old temp_size_new

* Label the combined variable
label variable TUMOR_SIZE_COMBINED "Combined tumor size (mm) from both TUMOR_SIZE and TUMOR_SIZE_SUMMARY_16"

* Convert to cm (divide by 10) and create categorical groups
gen TUMOR_SIZE_CM = TUMOR_SIZE_COMBINED / 10
label variable TUMOR_SIZE_CM "Tumor size in cm"

* Create categorical groups: 0-3cm, 3-5cm, >5cm
gen TUMOR_SIZE_GROUP = .
replace TUMOR_SIZE_GROUP = 1 if TUMOR_SIZE_CM >= 0 & TUMOR_SIZE_CM <= 3
replace TUMOR_SIZE_GROUP = 2 if TUMOR_SIZE_CM > 3 & TUMOR_SIZE_CM <= 5
replace TUMOR_SIZE_GROUP = 3 if TUMOR_SIZE_CM > 5 & !missing(TUMOR_SIZE_CM)
* Keep missing values as missing
label variable TUMOR_SIZE_GROUP "Tumor size group (1: 0-3cm, 2: 3-5cm, 3: >5cm)"

* Add value labels
label define size_group 1 "0-3 cm" 2 "3-5 cm" 3 ">5 cm"
label values TUMOR_SIZE_GROUP size_group

* Optional: create a string version if needed
gen TUMOR_SIZE_COMBINED_STR = string(TUMOR_SIZE_COMBINED)
replace TUMOR_SIZE_COMBINED_STR = "000" if TUMOR_SIZE_COMBINED == 0
replace TUMOR_SIZE_COMBINED_STR = "999" if missing(TUMOR_SIZE_COMBINED)
label variable TUMOR_SIZE_COMBINED_STR "Combined tumor size (string) from both TUMOR_SIZE and TUMOR_SIZE_SUMMARY_16"

* Clean up numeric temporary variables
drop TUMOR_SIZE_num TUMOR_SIZE_SUMMARY_16_num

* Optional: Show report of combined results
tab TUMOR_SIZE_COMBINED, miss
fre TUMOR_SIZE_GROUP
drop if TUMOR_SIZE_COMBINED == .
fre tx_pattern

save "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 3 coding tumor size.dta", replace