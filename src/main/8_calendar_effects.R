##########################################################################
# File: 8_calendar_effects.R
# Description: Plot calendar effects
##########################################################################

load(here("products/rdata/4_did_balanced.RData"))
load(here("products/rdata/5_did_truncated.RData"))

# aggregate to calendar effects
# not calculated for sustained & settled complaints (returns error)
calendar_effects <-
  filter(balanced_effects, outcome != "sustained") %>%
  mutate(calendar = map(fit, function(x) aggte(x, type = "calendar")))

# plot calendar effects
patchwork::wrap_plots(
  plot_calendar(calendar_effects$calendar[[1]],
                ylab = "Calendar ATT on complaints"),
  plot_calendar(calendar_effects$calendar[[1]],
                ylab = "Calendar ATT on complaints",
                time = c(19, 60)),
  ncol = 1, heights = c(0.3, 0.7)
) +
  ggsave(filename = here("products/figures/calendar_complaints_updated.pdf"),
         width = 8, height = 7)

patchwork::wrap_plots(
  plot_calendar(calendar_effects$calendar[[2]],
                ylab = "Calendar ATT on force"),
  plot_calendar(calendar_effects$calendar[[2]],
                ylab = "Calendar ATT on force",
                time = c(19, 60)),
  ncol = 1, heights = c(0.3, 0.7)
) +
  ggsave(filename = here("products/figures/calendar_force_updated.pdf"),
         width = 8, height = 7)


