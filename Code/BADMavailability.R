library(tidyverse)
library(patchwork)

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

p1 <- ggplot(counts, aes(x = n, y = site, fill = status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = status_colors) +
  scale_x_continuous(breaks = seq(0, max(counts$n), by = 25)) +
  labs(
    x = "Number of Variables",
    y = "",
    fill = "Data Status",
    title = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(size = 20))

#===============================================================================
## Data availability by variable

# Filter and reshape data
filtdat <- dat[1:24, ] %>%
  filter(!BADMvariable %in% c("HEIGHTC_DATE", "HEIGHTC_STATISTIC", "SA_DATE", "SOIL_CHEM_DATE")) %>%
  pivot_longer(
    cols = starts_with("US-"),
    names_to = "site",
    values_to = "status"
  ) %>%
  mutate(status = as.integer(status)) %>%
  group_by(BADMvariable, status) %>%
  summarize(n = n(), .groups = "drop") %>%
  mutate(
    status = factor(status, levels = c(0, 10, 11),
                    labels = c("Not Available", "Available, Not in BADM", "In BADM"))
  )

# BADM variable lookup table
badm_lookup <- data.frame(
  BADMvariable = c(
    "HEIGHTC", "SOIL_CHEM_BD", "SOIL_CHEM_C_ORG",
    "SOIL_TEX_CLAY", "SOIL_CHEM_N_TOT", "SOIL_TEX_SAND",
    "SOIL_TEX_SILT", "TREES_NUM", "SOIL_CHEM_PROFILE_MAX",
    "SOIL_CHEM_PROFILE_MIN", "SOIL_CLASSIFICATION", "SOIL_TEX_HORIZON",
    "PHEN_EVENT_DATE", "PHEN_EVENT_SPP", "PHEN_EVENT_TYPE",
    "SA", "SA_MAX", "SOIL_CHEM_HORIZON",
    "SOIL_TEX_PROFILE_MAX", "SOIL_TEX_PROFILE_MIN"
  ),
  BADMnames = c(
    "Canopy Height", "Soil Bulk Density", "Soil Organic Carbon Concentration", "Clay Content",
    "Soil Total Nitrogen Concentration", "Sand Content", "Silt Content", "Trees per Hectare",
    "Soil Chemistry Profile Maximum Depth", "Soil Chemistry Profile Minimum Depth", 
    "Soil Classification", "Soil Texture Profile by Horizon", "Phenology Event Date",
    "Phenology Event Species", "Phenology Event Type", "Stand Age", "Maximum Stand Age",
    "Soil Chemistry Profile by Horizon", "Soil Texture Profile Maximum Depth", 
    "Soil Texture Profile Minimum Depth"
  ),
  stringsAsFactors = FALSE
)

# Join descriptive names and reorder
filtcounts_named <- filtdat %>%
  left_join(badm_lookup, by = "BADMvariable") %>%
  group_by(BADMnames) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(BADMnames = reorder(BADMnames, total))

# Plot horizontal bar chart with descriptive names
p2 <- ggplot(filtcounts_named, aes(x = n, y = BADMnames, fill = status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = status_colors) +
  labs(
    x = "Number of Sites",
    y = "",
    fill = "Data Status",
    title = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))


(p1 + p2) + 
  plot_annotation(
    title = "BADM Data Availability",
    theme = theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
  )
