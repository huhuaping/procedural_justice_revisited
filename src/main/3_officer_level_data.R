##########################################################################
# File: 3_officer_level_data.R
# Description: Generate outcomes at officer-level per month
#              Create unbalanced and balanced panel, where the latter
#              excludes officers who resigned during the study
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/2_join.RData"))

# generate training frame
training <- 
  training %>%
  # round assignment dates: this reduces the number of days on which an
  # officer's treatment status is "misclassified," i.e. where an officer
  # has training = 1 before undertaking trained or training = 0 after
  # undertaking training
  mutate(assigned_exact = assigned,
         assigned = round_date(assigned, "month"),
         resigned = floor_date(resigned, "month")) %>%
  remove_new_officers() %>%
  filter(assigned < resigned | is.na(resigned))

# outcome at officer-level, per month
y_complaints <-
  left_join(training, complaints, by = "uid") %>%
  group_by(uid, month = floor_date(date, "month")) %>%
  summarize(complaints = n_distinct(cr_id, na.rm = TRUE))

y_sustained <-
  left_join(training, filter(complaints, sustained == 1), by = "uid") %>%
  group_by(uid, month = floor_date(date, "month")) %>%
  summarize(sustained = n_distinct(cr_id, na.rm = TRUE))

y_force <-
  left_join(training, force, by = "uid") %>%
  group_by(uid, month = floor_date(date, "month")) %>%
  summarize(force = n_distinct(trr_id, na.rm = TRUE))

# join outcomes
pj_officer_level <-
  expand_grid(
    uid   = unique(training$uid),
    month = seq(ymd("2011-01-01"), ymd("2016-12-01"), "1 month")
  ) %>%
  left_join(training,     by = "uid") %>%
  left_join(y_complaints, by = c("uid", "month")) %>%
  left_join(y_sustained,  by = c("uid", "month")) %>%
  left_join(y_force,      by = c("uid", "month"))

# first, replace NA entries (e.g. no complaints) with zeroes
pj_officer_level <-
  replace_na(pj_officer_level, list(complaints = 0, sustained = 0, force = 0))

# second, ensure that outcomes are NA in months after officers resign
pj_officer_level <-
  mutate(
    pj_officer_level,
    complaints = ifelse(month >= resigned & !is.na(resigned), NA, complaints),
    sustained  = ifelse(month >= resigned & !is.na(resigned), NA, sustained),
    force      = ifelse(month >= resigned & !is.na(resigned), NA, force)
  )

# set first trained period (first_trained = 0 for untrained officers)
pj_officer_level <-
  mutate(pj_officer_level, period = frank(month, ties.method = "dense")) %>%
  group_by(uid) %>%
  mutate(
    first_trained = min(period[month >= assigned]),
    first_trained = ifelse(is.infinite(first_trained), 0, first_trained)
  ) %>%
  ungroup()

# balanced panel, excluding all resigning officers
pj_officer_level_balanced <-
  filter(pj_officer_level, resigned > max(month) | is.na(resigned))

nrow(pj_officer_level_balanced) ==
  length(unique(pj_officer_level_balanced$uid)) *
  length(unique(pj_officer_level_balanced$month))

# save
save.image(here("products/rdata/3_officer_level_data.RData"))




