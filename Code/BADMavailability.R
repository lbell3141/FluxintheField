library(tidyverse)

dat <- read_csv("./Data/AZ-NM_BADMavailability.csv")%>%
  select(-BADMgroup)

dat_long <- dat %>%
  pivot_longer(
    cols = starts_with("US-"),
    names_to = "site",
    values_to = "status"
  )

#===============================================================================
## Data availability by site

counts <- dat_long %>%
  mutate(status = as.integer(status)) %>%
  group_by(site, status) %>%
  summarize(n = n(), .groups = "drop") %>%
  mutate(
    status = factor(status, levels = c(0, 10, 11),
                    labels = c("Not Available", "Available, Not in BADM", "In BADM"))
  )

status_colors <- c(
  "Not Available" = "#D73027",              # red
  "Available, Not in BADM" = "#FC8D59",     # orange
  "In BADM" = "#1A9850"                     # green
)

ggplot(counts, aes(x = site, y = n, fill = status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = status_colors) +
  labs(
    x = "",
    y = "Number of Variables",
    fill = "Data Status",
    title = "Data Availability by Site"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
#===============================================================================
## Data availability by variable

filtdat <- dat[1:20,]

filtdat_long <- filtdat %>%
  pivot_longer(
    cols = starts_with("US-"),
    names_to = "site",
    values_to = "status"
  )

filtcounts <- dat_long %>%
  mutate(status = as.integer(status)) %>%
  group_by(BADMvariable, status) %>%
  summarize(n = n(), .groups = "drop") %>%
  mutate(
    status = factor(status, levels = c(0, 10, 11),
                    labels = c("Not Available", "Available, Not in BADM", "In BADM"))
  )

ggplot(filtcounts, aes(x = BADMvariable, y = n, fill = status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = status_colors) +
  labs(
    x = "BADM Variable Code",
    y = "Number of Sites",
    fill = "Data Status",
    title = "Data Availability by Variable"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
