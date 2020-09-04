##########################################################################
# File: 9_other_figures.R
# Description: Plot officers per cluster and mean outcomes by month
##########################################################################

source(here::here("src/aux/packages.R"))
source(here("src/aux/functions.R"))
load(here("products/rdata/3_officer_level_data.RData"))

# plot officers per cluster
training %>%
  group_by(assigned_exact) %>%
  summarize(officers = n_distinct(uid)) %>%
  ggplot(aes(assigned_exact, officers)) +
  geom_point(shape = 21) +
  scale_x_date("Assigned to training") +
  scale_y_continuous("Officers in cluster") +
  ggsave(filename = here("products/figures/cluster_size.pdf"),
         width = 6, height = 5)

# plot mean outcomes by month
pj_officer_level_balanced %>%
  group_by(month) %>%
  summarize_at(vars(complaints, sustained, force), mean) %>%
  mutate(sustained = ifelse(month >= settlement_terminus, NA, sustained)) %>%
  pivot_longer(cols = c(complaints:force),
               names_to = "outcome", values_to = "mean") %>%
  mutate(data = ifelse(month >= ymd("2016-03-01"), "Updated", "Original"),
         outcome = fct_relevel(tools::toTitleCase(outcome),
                               "Complaints", "Sustained")) %>%
  ggplot(aes(month, mean, color = data)) +
  geom_point() +
  scale_x_date(NULL) +
  scale_y_continuous("Mean per officer",
                     breaks = scales::pretty_breaks()) +
  scale_color_brewer("Data", palette = "Set2", direction = -1) +
  facet_wrap(~ outcome) +
  theme(legend.position = "bottom") +
  ggsave(filename = here("products/figures/mean_outcomes.pdf"),
         width = 8, height = 5)
