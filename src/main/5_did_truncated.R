##########################################################################
# File: 5_did_truncated.R
# Description: Estimate DID model using Callaway and Sant'Anna (2020)
#              procedure
#              Use balanced panel and data before March 2016
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/3_officer_level_data.RData"))

# model for balanced panel
truncated_model <-
  function(yname = "complaints", data = pj_officer_level_balanced) {
    
    if (yname == "sustained") {
      data <- filter(data, month <= settlement_terminus)
    }
    
    att_gt(
      yname            = yname,
      tname            = "period", 
      idname           = "uid",
      first.treat.name = "first_trained",
      data             = filter(data, month < ymd("2016-03-01")),
      bstrap           = TRUE,
      panel            = TRUE,
      control.group    = "notyettreated"
    )
    
  }

# estimate models
did_truncated_complaints <- truncated_model(yname = "complaints") 
did_truncated_sustained  <- truncated_model(yname = "sustained")
did_truncated_force      <- truncated_model(yname = "force")

# aggregate effects
truncated_effects <-
  tibble(outcome = c("complaints", "sustained", "force"),
         fit = list(did_truncated_complaints,
                    did_truncated_sustained,
                    did_truncated_force)) %>%
  mutate(
    dynamic     = map(fit, function(x) aggte(x, type = "dynamic")),
    simple      = map(fit, function(x) aggte(x, type = "simple")),
    overall_att = map_dbl(simple, ~ .$overall.att),
    overall_se  = map_dbl(simple, ~ .$overall.se)
  )

# save
rm(did_truncated_complaints, did_truncated_sustained, did_truncated_force)
save.image(here("products/rdata/5_did_truncated.RData"))


