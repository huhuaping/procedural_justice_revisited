##########################################################################
# File: 6_simple_effects.R
# Description: Plot simple ATT from all models
##########################################################################

load(here("products/rdata/4_did_balanced.RData"))
load(here("products/rdata/5_did_truncated.RData"))

# bind and plot
bind_rows(balanced_effects, truncated_effects) %>%
  mutate(
    outcome = rep(c("Complaints", "Sustained & Settled", "Use of Force"), 2),
    data    = fct_relevel(rep(c("Updated", "Original"), each = 3),
                          "Updated", "Original"),
    lower   = overall_att - qnorm(0.975) * overall_se,
    upper   = overall_att + qnorm(0.975) * overall_se
  ) %>%
  ggplot(aes(outcome, overall_att, ymin = lower, ymax = upper,
             color = data, shape = data)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0, position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  scale_x_discrete(NULL) +
  scale_y_continuous("Weighted-average ATT") +
  scale_color_brewer("Data", palette = "Dark2") +
  scale_shape_discrete("Data") +
  ggsave(here("products/figures/simple_att.pdf"),
         width = 8, height = 5, device = cairo_pdf)


