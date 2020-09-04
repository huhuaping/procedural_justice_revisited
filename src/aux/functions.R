# clean suffixes
remove_suffix <- 
  function(x) {
    suffix <- c("SR", "JR", "II", "III", "IV")
    split  <- stri_split(x, regex = "\\s|\\.|\\,|\\(|\\)")
    x <-
      sapply(split,
             function(z)
               paste(z[!(z %in% suffix)], collapse = " "),
             USE.NAMES = FALSE)
    x <- trimws(x)
    return(x)
  }

# read csv or excel files
read_data <-
  function(x) {
    if (tools::file_ext(x) == "csv") {
      read_csv(x)
    } else {
      read_excel(x)
    }
  }

# filter new officers
remove_new_officers <-
  function(x, end_date) {
    x %>%
      filter(min(assigned) %m-% months(12) >= appointed)
  }

# identify uid for officers receiving a complaint using names, then resolve
# duplicates using birth year followed by middle initial
resolve_join <-
  function(x) {
    r1 <-
      group_by(x, cr_id, last_name, first_name, date) %>%
      filter(n() == 1)
    
    r2 <-
      get_dupes(x, cr_id, date, last_name, first_name) %>%
      filter(birth_year.x == birth_year.y) %>%
      group_by(cr_id, date, last_name, first_name) %>%
      filter(n() == 1)
    
    r3 <-
      get_dupes(x, cr_id, date, last_name, first_name) %>%
      filter(mi.x == mi.y) %>%
      group_by(cr_id, date, last_name, first_name) %>%
      filter(n() == 1)
    
    r4 <-
      get_dupes(x, cr_id, date, last_name, first_name) %>%
      filter(year(date) - birth_year.x <= 65,
             year(date) - birth_year.y <= 65) %>%
      group_by(cr_id, date, last_name, first_name) %>%
      filter(n() == 1)
    
    bind_rows(r1, r2, r3, r4) %>%
      ungroup() %>%
      distinct(uid, cr_id, date) %>%
      na.omit()
  }

# event study plot
plot_es <-
  function(es, time = c(-Inf, Inf), ncol = 2, ylab = NULL) {
    
    res <-
      tibble(
        t    = es$egt,
        att  = es$att.egt,
        se   = es$se.egt,
        crit = es$crit.val.egt
      ) %>%
      mutate(
        lower      = att - qnorm(0.975) * se,
        upper      = att + qnorm(0.975) * se,
        lower_crit = att - crit * se,
        upper_crit = att + crit * se
      ) %>%
      filter(t >= time[1], t <= time[2])
    
    res %>%
      pivot_longer(cols = c(lower, upper, lower_crit, upper_crit)) %>%
      mutate(inference = ifelse(str_detect(pattern = "crit", name),
                                "Simultaneous intervals",
                                "Pointwise intervals"),
             name = word(name, 1, sep = "_")) %>%
      pivot_wider(names_from = "name", values_from = "value") %>%
      ggplot(aes(t, att, ymin = lower, ymax = upper)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_errorbar(width = 0, color = grey(0.6), size = 0.3) +
      geom_point(size = 1.2) +
      facet_wrap(~ inference, ncol = ncol) +
      scale_x_continuous("Month relative to training",
                         breaks = seq(-36, 36, by = 12)) +
      scale_y_continuous(ylab, breaks = scales::pretty_breaks())
    
  }

# calendar effects plot
plot_calendar <-
  function(cal, time = c(-Inf, Inf), ncol = 2, ylab = NULL) {
    
    res <-
      tibble(
        t    = cal$egt,
        att  = cal$att.egt,
        se   = cal$se.egt,
        crit = cal$crit.val.egt
      ) %>%
      mutate(
        lower      = att - qnorm(0.975) * se,
        upper      = att + qnorm(0.975) * se,
        lower_crit = att - crit * se,
        upper_crit = att + crit * se
      ) %>%
      filter(t >= time[1], t <= time[2])
    
    res %>%
      pivot_longer(cols = c(lower, upper, lower_crit, upper_crit)) %>%
      mutate(inference = ifelse(str_detect(pattern = "crit", name),
                                "Simultaneous intervals",
                                "Pointwise intervals"),
             name = word(name, 1, sep = "_"),
             t = as.factor(t)) %>%
      pivot_wider(names_from = "name", values_from = "value") %>%
      ggplot(aes(t, att, ymin = lower, ymax = upper)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_errorbar(width = 0, color = grey(0.6), size = 0.3) +
      geom_point(size = 1.2) + # shape = 21
      facet_wrap(~ inference, ncol = ncol) +
      scale_x_discrete("Month of training roll-out",
                       expand = expansion(add = c(3, 3)),
                       breaks = scales::pretty_breaks()) +
      scale_y_continuous(ylab,
                         breaks = scales::pretty_breaks())
    
  }

# plot theme
theme_set(
  theme_light(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.2))
)
