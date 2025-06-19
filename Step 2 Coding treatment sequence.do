clear
use "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 1 define cohort.dta"

fre RX_SUMM_SURG_PRIM_SITE
count if surgery == 1 & RX_SUMM_SURG_PRIM_SITE == "00"

fre RX_SUMM_SYSTEMIC_SUR_SEQ
count if RX_SUMM_SYSTEMIC_SUR_SEQ == "" & YEAR == "2005"
**RX_SUMM_SYSTEMIC_SUR_SEQ variable was added in 2006 so will need to drop years 2004, 2005
drop if YEAR == "2004" | YEAR == "2005" 
/*
0 = no surgery or no systemic
2 = systemic before surgery
3 = systemic after surgery
4 = systemic before and after surgery
5 = intraop systemic
6 = intraop and other systemic before or after surgery 
7 = surgery before and after systemic
9 = unknown
*Need to drop the intraop, surgery before and after, and unknown
*/
drop if RX_SUMM_SYSTEMIC_SUR_SEQ == "5" | RX_SUMM_SYSTEMIC_SUR_SEQ == "6" | RX_SUMM_SYSTEMIC_SUR_SEQ == "7" | RX_SUMM_SYSTEMIC_SUR_SEQ == "9"

*need to delineate order of treatment
fre DX_CHEMO_STARTED_DAYS
fre DX_DEFSURG_STARTED_DAYS
fre DX_IMMUNO_STARTED_DAYS 
fre RX_SUMM_SYSTEMIC_SUR_SEQ

* Convert RX_SUMM_SYSTEMIC_SUR_SEQ to numeric if it's a string
capture confirm numeric variable RX_SUMM_SYSTEMIC_SUR_SEQ
if _rc {
    destring RX_SUMM_SYSTEMIC_SUR_SEQ, generate(systemic_seq) force
}
else {
    gen systemic_seq = RX_SUMM_SYSTEMIC_SUR_SEQ
}

* Drop any existing variables to avoid conflicts
capture drop chemo_days surg_days immuno_days

* Convert string variables to numeric
destring DX_CHEMO_STARTED_DAYS, generate(chemo_days)
destring DX_DEFSURG_STARTED_DAYS, generate(surg_days)
destring DX_IMMUNO_STARTED_DAYS, generate(immuno_days)

* First, create indicators for each treatment
gen got_surg = (DX_DEFSURG_STARTED_DAYS != "")
gen got_chemo = (DX_CHEMO_STARTED_DAYS != "")
gen got_immuno = (DX_IMMUNO_STARTED_DAYS != "")

* Create surgery-only indicator
gen surgery_only = (got_surg == 1 & got_chemo == 0 & got_immuno == 0)

* Convert string variables to numeric for sequencing
destring DX_CHEMO_STARTED_DAYS, generate(seq_chemo) force
destring DX_DEFSURG_STARTED_DAYS, generate(seq_surg) force
destring DX_IMMUNO_STARTED_DAYS, generate(seq_immuno) force

replace seq_chemo = 999999 if seq_chemo == .
replace seq_surg = 999999 if seq_surg == .
replace seq_immuno = 999999 if seq_immuno == .

* Create a string variable to store the sequence
gen tx_sequence = ""

* Generate the sequence based on ordering
quietly {
    * Find which treatment came first
    gen temp = ""
    replace temp = "Chemo" if seq_chemo <= seq_surg & seq_chemo <= seq_immuno & seq_chemo != 999999
    replace temp = "Surgery" if seq_surg <= seq_chemo & seq_surg <= seq_immuno & seq_surg != 999999
    replace temp = "Immuno" if seq_immuno <= seq_chemo & seq_immuno <= seq_surg & seq_immuno != 999999
    replace tx_sequence = temp if temp != ""
    
    * Update values for first treatment
    replace seq_chemo = 999999 if temp == "Chemo"
    replace seq_surg = 999999 if temp == "Surgery"
    replace seq_immuno = 999999 if temp == "Immuno"
    drop temp
    
    * Find second treatment
    gen temp = ""
    replace temp = "Chemo" if seq_chemo <= seq_surg & seq_chemo <= seq_immuno & seq_chemo != 999999
    replace temp = "Surgery" if seq_surg <= seq_chemo & seq_surg <= seq_immuno & seq_surg != 999999
    replace temp = "Immuno" if seq_immuno <= seq_chemo & seq_immuno <= seq_surg & seq_immuno != 999999
    replace tx_sequence = tx_sequence + " → " + temp if temp != ""
    
    * Update values for second treatment
    replace seq_chemo = 999999 if temp == "Chemo"
    replace seq_surg = 999999 if temp == "Surgery"
    replace seq_immuno = 999999 if temp == "Immuno"
    drop temp
    
    * Find third treatment
    gen temp = ""
    replace temp = "Chemo" if seq_chemo <= seq_surg & seq_chemo <= seq_immuno & seq_chemo != 999999
    replace temp = "Surgery" if seq_surg <= seq_chemo & seq_surg <= seq_immuno & seq_surg != 999999
    replace temp = "Immuno" if seq_immuno <= seq_chemo & seq_immuno <= seq_surg & seq_immuno != 999999
    replace tx_sequence = tx_sequence + " → " + temp if temp != ""
    drop temp
}

* Handle no treatment cases
replace tx_sequence = "No treatment" if tx_sequence == ""

* Display results
tab surgery_only, m
tab tx_sequence
list DX_DEFSURG_STARTED_DAYS if surgery_only == 1

* Clean up intermediary variables but keep treatment indicators for later use
drop seq_chemo seq_surg seq_immuno

* Create treatment pattern variable
gen tx_pattern = ""

* No treatment bin
replace tx_pattern = "No treatment" if tx_sequence == "No treatment"

* Chemo only bin (no immuno, no surgery)
replace tx_pattern = "Chemo only" if tx_sequence == "Chemo"

* Chemo + Immuno (no surgery) bin
replace tx_pattern = "Chemo + Immuno only" if ///
    (tx_sequence == "Chemo → Immuno" | ///
     tx_sequence == "Immuno → Chemo" | ///
     tx_sequence == "Immuno")

* Surgery only bin
replace tx_pattern = "Surgery only" if tx_sequence == "Surgery"

* Neoadjuvant chemo + surgery bin
replace tx_pattern = "Neoadjuvant chemo" if ///
    tx_sequence == "Chemo → Surgery"

* Neoadjuvant chemo/immuno + surgery bin
replace tx_pattern = "Neoadjuvant chemo/immuno" if ///
    (tx_sequence == "Immuno → Surgery" | ///
     tx_sequence == "Chemo → Immuno → Surgery" | ///
     tx_sequence == "Immuno → Chemo → Surgery")

* Split adjuvant bin into chemo and immuno
* Adjuvant chemo
replace tx_pattern = "Adjuvant chemo" if ///
    (tx_sequence == "Surgery → Chemo" | ///
     tx_sequence == "Surgery → Chemo → Immuno")

* Adjuvant immuno 
replace tx_pattern = "Adjuvant immuno" if ///
    (tx_sequence == "Surgery → Immuno")
    
* Adjuvant mixed (immuno then chemo)
replace tx_pattern = "Adjuvant mixed" if ///
    (tx_sequence == "Surgery → Immuno → Chemo")

* Now we'll use systemic_seq to identify perioperative chemotherapy
* First, identify candidates for chemo before and after surgery (code 4)
gen potential_periop = (systemic_seq == 4)

* Check if the patient actually received chemotherapy (not just immunotherapy)
* For perioperative chemotherapy, we need:
* 1. RX_SUMM_SYSTEMIC_SUR_SEQ = 4 (systemic therapy before and after surgery)
* 2. got_chemo = 1 (patient received chemotherapy)
* 3. Surgery appears between two chemo treatments in the sequence, or
*    We're missing the detailed timing but know from RX_SUMM_SYSTEMIC_SUR_SEQ they had chemo before and after

* Create a new variable to identify true perioperative chemotherapy candidates
gen periop_chemo = 0

* Sequence patterns that clearly show perioperative chemo
replace periop_chemo = 1 if tx_sequence == "Chemo → Surgery → Chemo"

* For patients with systemic therapy before and after surgery (code 4)
* AND who received chemotherapy, they might have had perioperative chemo
* even if we don't see it clearly in the sequence due to data limitations
replace periop_chemo = 1 if potential_periop == 1 & got_chemo == 1 & got_surg == 1 & ///
    !(tx_sequence == "Chemo → Surgery → Immuno" | tx_sequence == "Immuno → Surgery → Chemo" | ///
      tx_sequence == "Immuno → Surgery → Immuno")

* Update the tx_pattern to include perioperative chemotherapy
replace tx_pattern = "Neo + Adj chemo" if periop_chemo == 1

* Display the results
tab tx_pattern, m
tab periop_chemo

* Cross-check with systemic_seq
tab tx_pattern systemic_seq, m

* Clean up
drop potential_periop

list tx_sequence if tx_pattern == ""
*this patient had chemo followed by surgery followed by immuno. This is n=1 so we will need to drop them.
drop if tx_pattern == ""

fre tx_pattern
drop if immunotx == 1
drop if tx_pattern == "Chemo only" | tx_pattern == "No treatment"
drop if surgery == 0
fre tx_pattern


save "Y:\Rachel Wolansky\Intrahepatic Cholangio\Data Out\Step 2 coded treatment sequence.dta", replace