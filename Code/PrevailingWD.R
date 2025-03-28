#determine prevailing WD
#===============================================================================
#===Change filepaths as needed before proceeding================================

siteCoords <- c(31.8214, -110.8661)

PathtoFluxData <- "X:/moore/FluxNetData/AMF_US-SRM_FLUXNET_SUBSET_2004-2023_3-6/AMF_US-SRM_FLUXNET_SUBSET_HH_2004-2023_3-6.csv"
PathtoFluxData <- "X:/moore/FluxNetData/AMF_US-SRG_FLUXNET_SUBSET_2008-2023_4-6/AMF_US-SRG_FLUXNET_SUBSET_HH_2008-2023_4-6.csv"
SiteCode <- "US-SRM"
PathtoFileOutput <- file.path("./Data/Windroses", paste0(SiteCode, "_windrose.csv"))

#===============================================================================
#===============================================================================
#===Load files and necessary packages===========================================
library(lubridate)
library(dplyr)
library(openair)
library(ggplot2)
library(nasapower)

FluxData <- read.csv(PathtoFluxData, na.strings = -9999)
#===============================================================================
wind_data <- FluxData %>%
  mutate(TIMESTAMP_END = ymd_hm(TIMESTAMP_END),
         Hour = hour(TIMESTAMP_END)) %>%
  dplyr::select(TIMESTAMP_END, Hour, WS_F, WD) %>%
  filter(if_all(everything(), ~ !is.na(.)))%>%
  filter(Hour %in% 6:18)
  
windRose(wind_data, ws = "WS_F", wd = "WD",
         angle = 20,
         col = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f"),
         paddle = FALSE,
         key.position = "bottom",
         main = paste0(SiteCode, " Windrose"),
         grid.line = list(value = 5, lty = 5, col = "grey"),
         annotate = FALSE,
         ws.breaks = seq(0, 10, by = 2),
         key.footer = NULL)

#===============================================================================
wind_data <- wind_data %>%
  mutate(WD_binned = cut(WD, breaks = seq(0, 360, by = 10), include.lowest = TRUE, right = FALSE, labels = FALSE))

most_frequent_binned_direction <- wind_data %>%
  count(WD_binned) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(WD_binned)
print(most_frequent_binned_direction * 20)  #multiply by 20 to get actual angle
