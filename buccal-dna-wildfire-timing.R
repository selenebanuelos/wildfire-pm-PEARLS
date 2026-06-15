### Author: Selene Banuelos
### Date: 6/15/2026
### Description: Identify which partiicipants had buccal DNA collected before,
### during, and after the Camp fire that lasted from 11/08/2018-11/25/2018

# setup
library(dplyr)

# import data ------------------------------------------------------------------
# dates of biospecimen collection exam
exam_dates <- read.csv('data-raw/pearls_dataset_2022-07-08.csv')

# buccal DNA quality at each visit
dna <- read.csv('data-raw/buccal-dna-qc.csv')

# get match between specimen IDs and participant IDs
ids <- read.csv('data-raw/PEARLSBio-Plasma_DATA_2023-06-28_1451.csv')

# data wrangling ---------------------------------------------------------------
# fill in any missing PEARLS IDs for buccal DNA data
dna_clean <- ids %>%
  select(-plasma) %>%
  # rename participant ID variable for joining
  rename(pearls_id = subjectid)
# left off here 6/15/2026

# create dates of Camp fire
start_fire <- as.Date('2018/11/08') #y/m/d
end_fire <- as.Date('2018/11/25') #y/m/d

# create variable that describes visit timing for each timepoint relative to fire
each_visit <- exam_dates %>%
  select(pearls_id, visitnum, form_date_exam_r) %>%
  # currently only interested in T2 and T4
  filter(visitnum == 2 | visitnum == 4) %>%
  # join collection dates with plasma collection indicator
  full_join(., plasma, by = c('pearls_id', 'visitnum'))
