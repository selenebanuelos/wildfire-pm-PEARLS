### Author: Selene Banuelos
### Date: 6/3/2026
### Description: Identify which partiicipants had plasma collected before,
### during, and after the Camp fire that lasted from 11/08/2018-11/25/2018

# setup
library(dplyr)
library(tidyr)

# import data
################################################################################
# dates of biospecimen collection exam
exam_dates <- read.csv('data-raw/pearls_dataset_2022-07-08.csv')

# indicator of plasma collection at each visit
plasma <- read.csv('data-raw/PEARLSBio-Plasma_DATA_2023-06-28_1451.csv')

# data wrangling
################################################################################
# rename subject ID column for downstream joining
colnames(plasma)[colnames(plasma) == 'subjectid'] <- 'pearls_id'

# create dates of Camp fire
start_fire <- as.Date('2018/11/08') #y/m/d
end_fire <- as.Date('2018/11/25') #y/m/d

# create variable that describes visit timing for each timepoint relative to fire
each_visit <- exam_dates %>%
  select(pearls_id, visitnum, form_date_exam_r) %>%
  # join collection dates with plasma collection indicator
  full_join(., plasma, by = c('pearls_id', 'visitnum')) %>%
  # currently only interested in T2 and T4
  filter(visitnum == 2 | visitnum == 4) %>%
  # indicate all missing plasma with 0
  mutate(plasma = ifelse(is.na(plasma), 0, plasma),
         # convert date of exam from char to date variable type
         form_date_exam_r = as.Date(form_date_exam_r, format = '%m/%d/%Y'),
         # create new variable that categorizes collection relative to Camp fire
         exam_fire = case_when(
           form_date_exam_r < start_fire ~ 'before',
           start_fire <= form_date_exam_r & form_date_exam_r <= end_fire ~ 'during',
           form_date_exam_r > end_fire ~ 'after'),
         # categorize collection timing of plasma relative to Camp fire
         # done as separate step from above to ensure that any missing plasma
         # samples are labeled as such (exam != plasma collected)
         plasma_fire = case_when(
           plasma == 1 & exam_fire == 'before' ~ 'before',
           plasma == 1 & exam_fire == 'during' ~ 'during',
           plasma == 1 & exam_fire == 'after' ~ 'after',
           plasma == 0 ~ 'missing')
         ) 

# create variable that summarizes visit timing for T2 and T4 relative to wildfire
both_visits <- each_visit %>%
  pivot_wider(id_cols = pearls_id,
              names_from = visitnum,
              names_glue = '{.value}_{visitnum}',
              values_from = c(plasma_fire)) %>%
  mutate(visits_timing = case_when(
    plasma_fire_2 == 'before' & plasma_fire_4 == 'before' ~ 1,#'t2 & t4 before'
    plasma_fire_2 == 'before' & plasma_fire_4 == 'during' ~ 2,#'t2 before, t4 during'
    plasma_fire_2 == 'before' & plasma_fire_4 == 'after' ~ 3,#'t2 before, t4 after'
    plasma_fire_2 == 'during' & plasma_fire_4 == 'after' ~ 4,#'t2 during, t4 after'
    plasma_fire_2 == 'after' & plasma_fire_4 == 'after' ~ 5,#'t2 & t4 after'
    plasma_fire_2 == 'missing' | is.na(plasma_fire_2) | plasma_fire_4 == 'missing' | is.na(plasma_fire_4) ~ 6#'missing visits'
    ),
    visits_timing = as.factor(visits_timing)) %>%
  # keep only vars of interest for joining downstream
  select(pearls_id, visits_timing)

# create final data set
final <- left_join(each_visit, both_visits, by = 'pearls_id') %>%
  # remove unwanted/intermediate vars
  select(-c(exam_fire, plasma_fire))

# summarize
################################################################################
# count how many participants fall in each visit timing category
table(both_visits$visits_timing)
  
# output
################################################################################
write.csv(final, 'data-processed/plasma-campfire-visit-timing.csv', row.names = F)