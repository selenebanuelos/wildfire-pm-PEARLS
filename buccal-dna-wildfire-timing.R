### Author: Selene Banuelos
### Date: 6/15/2026
### Description: Identify which partiicipants had buccal DNA collected before,
### during, and after the Camp fire that lasted from 11/08/2018-11/25/2018

# setup
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)

# import data ------------------------------------------------------------------
# dates of biospecimen collection exam
exam_dates <- read.csv('data-raw/pearls_dataset_2022-07-08.csv')

# buccal DNA quality at each visit
dna <- read.csv('data-raw/buccal-dna-qc.csv')

# get match between specimen IDs and participant IDs
ids <- read.csv('data-raw/PEARLSBio-OralSwabDNAExtractio_DATA_2026-06-16_1517.csv')

# data wrangling ---------------------------------------------------------------
# get clean list of participant ID:specimen ID matches
ids_clean <- ids %>%
  # only keep participant IDs starting with 'P', these correspond to PEARLS
  filter(str_detect(subjectid, '^P')) %>%
  select(subjectid, specimenid, visitnum) %>%
  # rename participant ID variable for joining
  rename(pearls_id = subjectid)

# left off here 6/16/2026
# fill in any missing PEARLS IDs for buccal DNA data
dna_clean <- dna %>%
  # remove incomplete pearls ID variable from data set
  select(-pearls_id) %>%
  # add complete pearls ID back in 
  left_join(ids_clean, by = c('specimenid', 'visitnum')) %>%
  # reorder cols for easy viewing
  select(pearls_id, everything()) %>%
  # remove any rows missing either pearls ID and/or specimen ID
  filter( !(if_any(c('pearls_id', 'specimenid'), is.na)) ) %>%
  # only interested in visit numbers 2 and 4
  filter(visitnum == 2 | visitnum == 4) %>%
  # remove empty columns
  remove_empty('cols')

# create dates of Camp fire
start_fire <- as.Date('2018/11/08') #y/m/d
end_fire <- as.Date('2018/11/25') #y/m/d

# create variable that describes visit timing for each timepoint relative to fire
each_visit <- exam_dates %>%
  select(pearls_id, visitnum, form_date_exam_r) %>%
  # currently only interested in T2 and T4
  filter(visitnum == 2 | visitnum == 4) %>%
  # join collection dates with DNA QC data
  full_join(., dna_clean, by = c('pearls_id', 'visitnum')) %>%
  # convert date of exam from char to date variable type
  mutate(form_date_exam_r = as.Date(form_date_exam_r, format = '%m/%d/%Y'),
         # create new variable that categorizes collection relative to Camp fire
         exam_fire = case_when(
           form_date_exam_r < start_fire ~ 'before',
           start_fire <= form_date_exam_r & form_date_exam_r <= end_fire ~ 'during',
           form_date_exam_r > end_fire ~ 'after'),
         # categorize collection timing of buccal DNA relative to Camp fire
         # done as separate step from above to ensure that any missing DNA
         # samples are labeled as such (exam != DNA collected)
         dna_fire = case_when(
           dna_qc_passed == 1 & exam_fire == 'before' ~ 'before',
           dna_qc_passed == 1 & exam_fire == 'during' ~ 'during',
           dna_qc_passed == 1 & exam_fire == 'after' ~ 'after',
           dna_qc_passed == 0 | is.na(dna_qc_passed) ~ 'missing')
  )

# create variable that summarizes visit timing for T2 and T4 relative to wildfire
both_visits <- each_visit %>%
  pivot_wider(id_cols = pearls_id,
              names_from = visitnum,
              names_glue = '{.value}_{visitnum}',
              values_from = c(dna_fire)) %>%
  mutate(visits_timing = case_when(
    dna_fire_2 == 'before' & dna_fire_4 == 'before' ~ 1,#'t2 & t4 before'
    dna_fire_2 == 'before' & dna_fire_4 == 'during' ~ 2,#'t2 before, t4 during'
    dna_fire_2 == 'before' & dna_fire_4 == 'after' ~ 3,#'t2 before, t4 after'
    dna_fire_2 == 'during' & dna_fire_4 == 'after' ~ 4,#'t2 during, t4 after'
    dna_fire_2 == 'after' & dna_fire_4 == 'after' ~ 5,#'t2 & t4 after'
    # 'missing visit, no DNA collected, or DNA did not pass QC'
    dna_fire_2 == 'missing' | is.na(dna_fire_2) | dna_fire_4 == 'missing' | is.na(dna_fire_4) ~ 6
  ),
  visits_timing = factor(visits_timing)) %>%
  # keep only vars of interest for joining downstream
  select(pearls_id, visits_timing)

# create final data set
final <- left_join(each_visit, both_visits, by = 'pearls_id') %>%
  # remove unwanted/intermediate vars
  select(-c(exam_fire, dna_fire))

# summarize --------------------------------------------------------------------
# count how many participants fall in each visit timing category
table(both_visits$visits_timing)

# output -----------------------------------------------------------------------
write.csv(final, 'data-processed/buccal-dna-campfire-timing.csv', row.names = F)