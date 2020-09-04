# import all complaints data
case <-
  tibble(filename = list.files(
    here("data/complaints"), pattern = "complaints|case", full.names = TRUE
  )) %>%
  transmute(data = map(
    filename, function(x) 
      x %>% 
      read_data() %>%
      clean_names() %>%
      select(one_of("log_no", "cr_id", "incident_fromdate",
                    "incident_date", "incident_start_date")) %>%
      set_names(c("cr_id", "date")) %>%
      mutate(
        date = as_date(parse_date_time(date, orders = c("dmy", "mdy", "ymd")))
      )
  )) %>%
  unnest() %>%
  distinct()

accused <-
  tibble(filename = list.files(
    here("data/complaints"), pattern = "accused", full.names = TRUE
  )) %>%
  mutate(data = map(
    filename, function(x)
      x %>%
      read_data() %>%
      clean_names() %>%
      select(one_of("log_no", "cr_id", "uid", "officer_last_name", "last_name",
                    "officer_first_name", "first_name", "middle_initial",
                    "birth_year", "final_finding")) %>%
      rename(cr_id = 1) %>%
      left_join(case, by = "cr_id")
  ))

# join uid for post 2016-03 accused data
complaints_1 <-
  bind_rows(accused$data[[3]], accused$data[[4]]) %>%
  transmute(
    cr_id, last_name = remove_suffix(coalesce(officer_last_name, last_name)),
    first_name = coalesce(officer_first_name, first_name),
    mi = middle_initial, birth_year
  ) %>%
  distinct() %>%
  left_join(case, by = "cr_id") %>%
  left_join(transmute(officers, last_name, first_name, mi, birth_year, uid),
            by = c("last_name", "first_name"))  %>%
  resolve_join() %>%
  # bind to pre 2016-03 data
  bind_rows(
    .,
    bind_rows(accused$data[[1]], accused$data[[2]]) %>%
      group_by(cr_id, uid, date) %>%
      transmute(finding = ifelse(any(final_finding %in% c("SUSTAINED", "SU")),
                                 "sustained", "other_outcome"))
  ) %>%
  distinct()

complaints_2 <-
  left_join(
    here("data/complaints/allegation.xlsx") %>%
      read_excel(sheet = 2) %>%
      clean_names() %>%
      transmute(cr_id = as.numeric(crid), date = as_date(ymd_hms(incident_date)),
                officer_id),
    here("data/complaints/allegation.xlsx") %>%
      read_excel(sheet = 5) %>%
      clean_names() %>%
      transmute(officer_id, last_name = toupper(officer_last),
                first_name = toupper(officer_first),
                appointed = as_date(appt_date, origin = "1899-12-30"))
  ) %>%
  left_join(transmute(officers, last_name, first_name, appointed, uid),
            by = c("last_name", "first_name", "appointed")) %>%
  transmute(uid, cr_id, date)

# bind all complaints data
complaints <-
  bind_rows(complaints_1, complaints_2) %>%
  distinct()

# read settlements data
settlements <-
  here("data/settlements/settlements_1952_2016.csv") %>%
  read_csv() %>%
  transmute(uid = UID, date = incident_date, settlement = parse_number(settlement))

# join complaints and settlement data 
complaints <-
  left_join(complaints, settlements, by = c("uid", "date")) %>%
  mutate(sustained = ifelse(finding == "sustained" | !is.na(settlement), 1, 0))

