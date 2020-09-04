##########################################################################
# File: 7_dynamic_effects.R
# Description: Plot dynamic effects
##########################################################################

load(here("products/rdata/4_did_balanced.RData"))
load(here("products/rdata/5_did_truncated.RData"))

# calculate mean outcomes in 12 months before training
pj_officer_level_balanced %>%
  mutate(relative = period - first_trained) %>%
  filter(between(relative, -12, -1)) %>%
  summarize_at(vars(complaints, sustained, force), mean)

# plot dynamic effects using updated data
plot_es(balanced_effects$dynamic[[1]], time = c(-36, 36),
        ylab = "Dynamic ATT, complaints per officer") +
  annotate("text", x = 24, y = 0.04,
           label = expression(bar(y)["[-12,-1]"]*"= 0.044"), parse = TRUE) +
  ggsave(filename = here("products/figures/dynamic_complaints_updated.pdf"),
         width = 8, height = 4)

plot_es(balanced_effects$dynamic[[2]], time = c(-36, 36),
        ylab = "Dynamic ATT, sustained complaints per officer") +
  annotate("text", x = 24, y = 0.013,
           label = expression(bar(y)["[-12,-1]"]*"= 0.004"), parse = TRUE) +
  ggsave(filename = here("products/figures/dynamic_sustained_updated.pdf"),
         width = 8, height = 4)

plot_es(balanced_effects$dynamic[[3]], time = c(-36, 36),
        ylab = "Dynamic ATT, use of force per officer") +
  annotate("text", x = 24, y = 0.03,
           label = expression(bar(y)["[-12,-1]"]*"= 0.047"), parse = TRUE) +
  ggsave(filename = here("products/figures/dynamic_force_updated.pdf"),
         width = 8, height = 4)

# plot dynamic effects using original data
plot_es(truncated_effects$dynamic[[1]], time = c(-36, 36),
        ylab = "Dynamic ATT, complaints per officer") +
  ggsave(filename = here("products/figures/dynamic_complaints_original.pdf"),
         width = 8, height = 4)

plot_es(truncated_effects$dynamic[[2]], time = c(-36, 36),
        ylab = "Dynamic ATT, sustained complaints per officer") +
  ggsave(filename = here("products/figures/dynamic_sustained_original.pdf"),
         width = 8, height = 4)

plot_es(truncated_effects$dynamic[[3]], time = c(-36, 36),
        ylab = "Dynamic ATT, use of force per officer") +
  ggsave(filename = here("products/figures/dynamic_force_original.pdf"),
         width = 8, height = 4)

