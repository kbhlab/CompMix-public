library(tidyverse)
library(janitor)
library(data.table)
library(readxl)
library(here)


######## MONTREAL DATA ######## 

##### load data #####

### raw eye tracking data
load(here("data/Montreal/mtl_raw_gaze_anon.rda"))


### Master Subject List

mtl_msl <- read_csv(here("data/Montreal/mtl_msl_anon.csv")) %>% 
  clean_names()


mtl_msl_keepers <- mtl_msl %>% 
  # only data from those marked as keepers
  filter(keeper == 1) %>% 
  # add column determining whether lang of testing was child's dom or non-dominant language
  mutate(study_lang_dom = case_when(
    study_language == child_dom_lang ~ "dom",
    TRUE ~ "non"
  )) %>% 
  mutate(study_order_comp = as.factor(study_order_comp),
         study_language = as.factor(study_language),
         study_lang_dom = as.factor(study_lang_dom)) %>% 
  # add columns reframing vocab score by dominant language
  mutate(vocab_dom = case_when(
    child_dom_lang == "English" ~ vocab_eng,
    child_dom_lang == "French" ~ vocab_fr),
    vocab_non_dom = case_when(
      child_dom_lang == "English" ~ vocab_fr,
      child_dom_lang == "French" ~ vocab_eng
    )) %>% 
  # add testing location
  mutate(testing_location = "Montreal")

# select only relevant columns for analysis to merge with eye-tracking data
mtl_msl_keepers_eye <- mtl_msl_keepers %>% 
  select(study_id, gender, first_study, study_language, child_dom_lang, study_lang_dom, edu_years, vocab_total) 


# identify how many participants were excluded for each reason
mtl_msl_non_keepers <- mtl_msl %>% 
  # only data from CompMix study and those marked as not being keepers
  filter(study_group == "CompMix" & keeper == 0) %>% 
  mutate(exclusion = as.factor(exclusion)) %>% 
  group_by(exclusion) %>% 
  summarize(exclusion_n = n())

write_csv(mtl_msl_keepers, here("data_frames/mtl_msl_keepers.csv"))
write_csv(mtl_msl_non_keepers, here("data_frames/mtl_msl_non_keepers.csv"))




### load info on target - distractor pairs

target_distractor <- read_csv(here("2016_CompMix_Design/target-distractor-pairs.csv"))



## different AOIs by object

aois <- read_csv(here("data/Montreal/compmix_aois.csv")) %>% 
  # adjust aoi_names to match media name in data_cleaned df
  separate(aoi_name, into = c("language", "object_type", "object", "trial_type", "location"), sep = "_", remove = FALSE) 

# check to see if all boxes are marked in same order
aois_clockwise <- aois %>% 
  mutate(x_clockwise = case_when(
    x1 == x4 && x2 == x3 ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  mutate(y_clockwise = case_when(
    y1 == y2 && y3 == y4 ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  filter(x_clockwise == FALSE | y_clockwise == FALSE)
# appears that they all are, so x1 & y1 are top L, x2 & y2 are top R, x3 & y3 are bottom R, x4 & y4 are bottom L

# check if AOIs are the same for the same object in same location across conditions
aois_by_object <- aois %>% 
  select(object, location, x1:y4) %>% 
  group_by(object, location) %>% 
  distinct() %>%
  # AOIs are not always the same 
  # get width and height of each aoi
  mutate(x_width = x2 - x1,
         y_height = y3 - y2) %>% 
  # identify which AOIs are determined by 0.5 pixels, because that doesn't make sense
  mutate(half_px = case_when(
    str_ends(x1, "\\.5") | str_ends(y2, "\\.5") | str_ends(y3, "\\.5") | str_ends(y4, "\\.5") ~ TRUE,
    str_ends(y1, "\\.5") | str_ends(y2, "\\.5") | str_ends(y3, "\\.5") | str_ends(y4, "\\.5") ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  # remove AOIs determined by half pixels
  filter(half_px == FALSE) 

# find out how many AOIs each object has per side
object_n <- aois_by_object %>% 
  group_by(object, location) %>% 
  summarize(n_object = n()) %>% 
  # filter for only those that have 1 AOI per side
  filter(n_object == 1)

# for the objects that only have 1 AOI per side, join with pixel info. This is the final AOI for that object on that side
px_by_object <- object_n %>% 
  select(-n_object) %>% 
  left_join(select(aois_by_object, - half_px), by = c("object", "location"))

# get AOIs for remaining objects
aois_final <- aois_by_object %>% 
  # by matching it to the width of the AOI for it on the other side (height will match too, but is unnecessary code)
  filter(x_width %in% px_by_object$x_width,
         # remove this weird entry for Fish on the R as it's different than other AOIs and wasn't picked up by the other code
         y_height != 493) %>% 
  # remove unnecessary columns
  select(-half_px, -x3, -x4, -y2, -y4) %>% 
  rename(x_left = x1, x_right = x2, y_top = y1, y_bottom = y3)

# new dfs for AOIs so that they can be merged with data_cleaned for both target and distractor
aois_target <- aois_final %>% 
  rename(target = object, 
         target_location = location,
         target_x_left = x_left,
         target_x_right = x_right,
         target_y_top = y_top,
         target_y_bottom = y_bottom,
         target_x_width = x_width,
         target_y_height = y_height)

aois_distractor <- aois_final %>% 
  rename(distractor = object, 
         distractor_location = location,
         distractor_x_left = x_left,
         distractor_x_right = x_right,
         distractor_y_top = y_top,
         distractor_y_bottom = y_bottom,
         distractor_x_width = x_width,
         distractor_y_height = y_height)





##### prep eye tracking data #####

mtl_data_cleaned <- mtl_raw_data_anon %>% 
  # choose only needed columns
  select(recording_name, recording_duration, media_name, recording_timestamp, studio_event, fixation_index, gaze_event_type, gaze_event_duration, gaze_point_x_adc_spx, gaze_point_y_adc_spx, validity_left, validity_right, pupil_left, pupil_right) %>% 
  # rename columns for clarity
  rename(gaze_point_x = gaze_point_x_adc_spx, gaze_point_y = gaze_point_y_adc_spx, timestamp = recording_timestamp) %>% 
  # separate recording name to get subject and baby id's
  separate(recording_name, into = c("study_name", "study_id", "study_order"), sep = "_", remove = FALSE) %>% 
  mutate(recording_name = as.factor(recording_name)) %>% 
  # filter for only keepers
  filter(study_id %in% mtl_msl_keepers$study_id) %>% 
  # remove .wmv from file names
  mutate(media_name = gsub(".wmv", "", media_name),
         # One video file was misnamed, so rename it correctly here
         media_name = case_when( 
           media_name == "Cow_FrSingle_L" & study_order == "F1"  ~ "Cow_FrSingle_R",
           TRUE ~ as.character(media_name))) %>% 
  # separate media name into sub-components
  separate(media_name, into = c("target", "trial_type", "target_location"), sep = "_", remove = FALSE) %>% 
  mutate(media_name = as.factor(media_name)) %>% 
  # remove language info from trial type (will get info when merged with MSL)
  mutate(trial_type = gsub("Fr", "", trial_type),
         trial_type = gsub("Eng", "", trial_type)) %>% 
  # remove rows from attention getters, fillers, and spaces in between trials
  filter(trial_type == "Single" | trial_type == "Mixed") %>%
  # add unique trial column
  mutate(trial_unique = paste(recording_name, media_name)) %>% 
  # add trial numbers
  group_by(recording_name) %>% 
  mutate(trial_number = rleid(trial_unique)) %>% 
  # remove MovieStart and MovieEnd rows
  filter(is.na(studio_event)) %>%
  ungroup() %>% 
  # 0 time stamp for each trial
  group_by(trial_unique) %>% 
  mutate(trial_start = min(timestamp),
         time_from_zero = timestamp - trial_start) %>% 
  ungroup() %>% 
  # create trackloss column for eyetrackingR functions
  mutate(trackloss = case_when(is.na(gaze_point_x) ~ TRUE, 
                               is.na(gaze_point_y) ~ TRUE,
                               validity_left > 1 & validity_right > 1 ~ TRUE,
                               TRUE ~ FALSE)) %>% 
  # join with target/distractor info
  left_join(target_distractor, by = "target") %>% 
  # add distractor location
  mutate(distractor_location = case_when(
    target_location == "R" ~ "L",
    target_location  == "L" ~ "R"
  )) %>% 
  # join AOI info
  left_join(aois_target, by = c("target", "target_location")) %>% 
  left_join(aois_distractor, by = c("distractor", "distractor_location")) %>% 
  # AOI look per time point
  mutate(target_look = case_when(
    gaze_point_x > target_x_left & gaze_point_x < target_x_right & gaze_point_y > target_y_top & gaze_point_y < target_y_bottom ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(distractor_look = case_when(
    gaze_point_x > distractor_x_left & gaze_point_x < distractor_x_right & gaze_point_y > distractor_y_top & gaze_point_y < distractor_y_bottom ~ 1,
    TRUE ~ 0
  )) %>% 
  # join MSL columns
  left_join(mtl_msl_keepers_eye, by = "study_id")

# get trial numbers based on trial type (e.g., first Mixed trial gets a 1 even if it was really trial 2)
mtl_trial_number_type <- mtl_data_cleaned %>% 
  group_by(study_id, trial_number, trial_type) %>% 
  summarize(n_rows = n()) %>% 
  select(-n_rows) %>% 
  group_by(study_id, trial_type) %>% 
  mutate(trial_number_type = row_number())

# merge with rest of data
mtl_data_cleaned <- mtl_data_cleaned %>% 
  left_join(mtl_trial_number_type, by = c("study_id", "trial_number", "trial_type"))

write_csv(mtl_data_cleaned, here("data_frames/mtl_data_cleaned.csv"))







######## PRINCETON DATA ######## 

##### load data

## raw data
pct_raw_data <- read_csv(file = here("data/Princeton/3aMixData_col-names.csv"), na = c(".", "-"), col_types = cols(.default = "c")) 

## trial number info
pct_trial_numbers <- read_csv(here("data/Princeton/CompMix_trial-numbers_PCT.csv"))



## master subject log

pct_msl <- read_csv(here("data/Princeton/pct_msl_anon.csv")) %>% 
  mutate(study_id = as.character(study_id))


# language comprehension

pct_comp <- read_excel(here("data/Princeton/Princeton_LangComprehension.xlsx")) %>% 
  clean_names() %>% 
  rename(study_id = subj_id) %>% 
  mutate(study_id = as.character(study_id))


#keepers

pct_keepers_list <- read_csv(here("data/Princeton/Princeton_keepers.csv")) %>% 
  mutate(study_id = as.character(study_id))



# combine for MSL

pct_msl <- pct_msl %>% 
  left_join(pct_comp, by = "study_id") %>% 
  left_join(pct_keepers_list, by = "study_id") %>% 
  mutate(comp_e = as.numeric(comp_e),
         comp_s = as.numeric(comp_s),
         vocab_eng = as.numeric(vocab_eng),
         vocab_sp = as.numeric(vocab_sp),
         # steps to assign child's dominant language
         child_dom_lang = case_when(
           # first assign dominance based on language comprehension
           comp_e > comp_s ~ "English",
           comp_s > comp_e ~ "Spanish",
           # if comprehension equal, use vocabulary
           vocab_eng > vocab_sp ~ "English",
           vocab_sp > vocab_eng ~ "Spanish")) %>% 
  # add columns reframing vocab score by dominant language
  mutate(vocab_dom = case_when(
    child_dom_lang == "English" ~ vocab_eng,
    child_dom_lang == "Spanish" ~ vocab_sp),
    vocab_non_dom = case_when(
      child_dom_lang == "English" ~ vocab_sp,
      child_dom_lang == "Spanish" ~ vocab_eng),
    vocab_total = vocab_eng + vocab_sp
  ) %>% 
  # get study language
  mutate(study_language = case_when(
    order == "E1" | order == "E2" ~ "English",
    order == "S1" | order == "S2" ~ "Spanish"
  )) %>% 
  # determine whether child was tested in dom lang
  mutate(study_lang_dom = case_when(
    study_language == child_dom_lang ~ "dom",
    study_language != child_dom_lang ~ "non"
  )) %>% 
  # add testing location
  mutate(testing_location = "Princeton") %>% 
  # recode sex to match MTL
  mutate(gender = case_when(
    gender == "F" ~ "female",
    TRUE ~ "male"
  ))


# filter for keepers
## in original subject log, some lines were highlighted in red. via email with lab manager at princeton, found out this means participant should be excluded if red
pct_msl_keepers <- pct_msl %>% 
  filter(keeper == "Y")

write_csv(pct_msl_keepers, here("data_frames/pct_msl_keepers.csv"))

# non keepers
pct_msl_non_keepers <- pct_msl %>% 
  # not just inverse of keepers, because 1 participant is removed based on insufficient complete trials. this was identified in subject log by LK. Participant remains in keepers list until looking is analyzed in next script
  filter(keeper == "N") %>% 
  group_by(reason) %>% 
  summarize(reason_n = n())

write_csv(pct_msl_non_keepers, here("data_frames/pct_msl_non_keepers.csv"))

## one df that's pared down to merge with eye tracking data
pct_comp_eye <- pct_msl_keepers %>% 
  select(study_id, child_dom_lang, study_language, study_lang_dom, edu_years, vocab_total) 


##### prep eye tracking data

pct_data_cleaned <- pct_raw_data %>% 
  # get rid of duplicate rows with column names (artifact of export process)
  # get rid of trials with equipment malfunction
  filter(`Sub Num` != "Sub Num" & `Prescreen Notes` != "Equipment Malfunction") %>%
  # make looking data longer
  pivot_longer(cols = 18:254, names_to = "time_from_zero", values_to = "look") %>% 
  clean_names() %>% 
  # rename to match MTL data
  rename(study_id = sub_num, trial_type = condition, target = target_image) %>% 
  # change trial type to match MTL
  mutate(trial_type = case_when(trial_type == "Switch" ~ "Mixed",
                                TRUE ~ "Single")) %>% 
  # filter for only participants in keepers list
  filter(study_id %in% pct_msl_keepers$study_id) %>% 
  # add unique trial column
  mutate(trial_unique = paste(study_id, target, trial_type, sep = "_")) %>% 
  # change columns to match those in MTL data
  separate(order, into = c("study_name", "study_order"), sep = "_", remove = TRUE) %>% 
  # add trial numbers that count just for target trials, discount filler trials
  left_join(pct_trial_numbers, by = c("study_order", "target", "target_side", "trial_type")) %>% 
  # remove incorrect trial numbers
  select(-tr_num) %>% 
  # create columns needed for make_eyetracking_data: tackloss, target_look, distractor_look
  mutate(trackloss = case_when(
    is.na(look) ~ TRUE,
    TRUE ~ FALSE),
    target_look = case_when(
      look == 1 ~ 1,
      TRUE ~ 0),
    distractor_look = case_when(
      look == 0 ~ 1,
      TRUE ~ 0
    )) %>% 
  # join child language dominance
  left_join(pct_comp_eye, by = "study_id") 

pct_trial_number_type <- pct_data_cleaned %>% 
  group_by(study_id, trial_number, trial_type) %>% 
  summarize(n_rows = n()) %>% 
  select(-n_rows) %>% 
  group_by(study_id, trial_type) %>% 
  mutate(trial_number_type = row_number())

# merge with rest of data
pct_data_cleaned <- pct_data_cleaned %>% 
  left_join(pct_trial_number_type, by = c("study_id", "trial_number", "trial_type"))

write_csv(pct_data_cleaned, here("data_frames/pct_data_cleaned.csv"))
