##########################################################################
# File: 4_did_balanced.R
# Description: Estimate DID model using Callaway and Sant'Anna (2020)
#              procedure
#              Use balanced panel (excluding officers who retired during study)
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/3_officer_level_data.RData"))

# model for balanced panel
balanced_model <-
  function(yname = "complaints", data = pj_officer_level_balanced) {
    
    if (yname == "sustained") {
      data <- filter(data, month <= settlement_terminus)
    }

    att_gt(
      yname            = yname,
      tname            = "period", 
      idname           = "uid",
      first.treat.name = "first_trained",
      data             = data,
      bstrap           = TRUE,
      panel            = TRUE,
      control.group    = "notyettreated"
    )
    
  }

# estimate models (run time: approx 80 minutes on 16GB memory)
did_balanced_complaints <- balanced_model(yname = "complaints") 
did_balanced_sustained  <- balanced_model(yname = "sustained")
did_balanced_force      <- balanced_model(yname = "force")

# aggregate effects
balanced_effects <-
  tibble(outcome = c("complaints", "sustained", "force"),
         fit = list(did_balanced_complaints,
                    did_balanced_sustained,
                    did_balanced_force)) %>%
  mutate(
    dynamic     = map(fit, function(x) aggte(x, type = "dynamic")),
    simple      = map(fit, function(x) aggte(x, type = "simple")),
    overall_att = map_dbl(simple, ~ .$overall.att),
    overall_se  = map_dbl(simple, ~ .$overall.se)
  )

# save
rm(did_balanced_complaints, did_balanced_sustained, did_balanced_force)
save.image(here("products/rdata/4_did_balanced.RData"))

