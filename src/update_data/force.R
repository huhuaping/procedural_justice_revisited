# read use of force data
type <-
  here("data/force/trr_actions_responses_2004_2016.csv") %>%
  read_csv() %>%
  filter(person == "Member Action") %>%
  group_by(trr_id) %>%
  filter(action_sub_category == max(action_sub_category)) %>%
  ungroup() %>%
  mutate(a = str_to_lower(member_action)) %>%
  transmute(
    trr_id,
    category = case_when(
      a %in% c("member presence",
               "verbal commands") ~
        "force_mitigation",
      a %in% c("escort holds",
               "control instrument",
               "wristlock",
               "armbar",
               "pressure sensitive areas") ~
        "control_tactics",
      a %in% c("open hand strike",
               "take down/emergency handcuffing",
               "elbow strike",
               "kicks",
               "closed hand strike/punch",
               "knee strike") ~
        "action_without_weapons",
      a %in% c("firearm",
               "canine") |
        grepl("impact|taser|chemical", a) ~
        "action_with_weapons",
      TRUE ~ "other"
    )
  )

trr <-
  here("data/force/trr_main_2004_2016.csv") %>%
  read_csv() %>%
  transmute(trr_id, date = trr_date)

user <-
  here("data/force/trr_officers_2004_2016.csv") %>%
  read_csv() %>%
  transmute(uid = UID, trr_id)

# join use of force data
force <-
  user %>%
  left_join(trr,  by = "trr_id") %>%
  left_join(type, by = "trr_id") %>%
  distinct()

# import force data from 2016-03-01 to 2016-12-31
force_2016 <-
  here("data/force/P456008.xlsx") %>%
  read_excel(sheet = 1) %>%
  clean_names() %>%
  transmute(
    trr_id = trr_report_id, date = as_date(dte),
    last_name = remove_suffix(polast), first_name = pofirst,
    appointed = as_date(appointed_date)) %>%
  filter(date >= ymd("2016-03-01"), date <= ymd("2016-12-31")) %>%
  left_join(
    transmute(officers, last_name, first_name, appointed, uid),
    by = c("last_name", "first_name", "appointed")
  ) %>%
  transmute(uid, trr_id, date)

# bind force data
force <-
  bind_rows(
    filter(force, date < ymd("2016-03-01")),
    force_2016
  )
