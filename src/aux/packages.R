library(pacman)
p_load(conflicted, data.table, did, glue, here, janitor, lubridate,
       readxl, rlang, stringi, tidyverse)

# resolve package::function conflicts
conflict_prefer("here",    "here",      quiet = TRUE)
conflict_prefer("filter",  "dplyr",     quiet = TRUE)
conflict_prefer("setdiff", "base",      quiet = TRUE)
conflict_prefer("lag",     "dplyr",     quiet = TRUE)
conflict_prefer("year",    "lubridate", quiet = TRUE)
conflict_prefer("month",   "lubridate", quiet = TRUE)
conflict_prefer("between", "dplyr",     quiet = TRUE)
