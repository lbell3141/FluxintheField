library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

PathtoBADM <- "./Data/AMF_AA-Net_BIF_CCBY4_20250318.csv"

badm <- read.csv(PathtoBADM)

GradSites <- c("US-SRM", "US-Jo1","US-CMW","US-SRG","US-Whs","US-Jo2","US-Wkg",
               "US-Seg","US-Ses", "US-Wjs","US-Mpj","US-Vcp","US-MtB","US-Vcs","US-Vcm") #US-MPG excluded b/c it's unregistered

sitedat <- badm%>%
  filter(SITE_ID %in% GradSites )%>%
  distinct(SITE_ID, VARIABLE_GROUP, VARIABLE) %>%
  mutate(AVAILABLE_BADM = 1)

BadmCodes <- badm%>%
  distinct(VARIABLE_GROUP, VARIABLE)

site_var_combos <- expand_grid(
  SITE_ID = GradSites,
  BadmCodes)
ReportedInfo <- site_var_combos %>%
  left_join(sitedat, by = c("SITE_ID", "VARIABLE_GROUP", "VARIABLE")) %>%
  mutate(AVAILABLE_BADM = ifelse(is.na(AVAILABLE_BADM), 0, AVAILABLE_BADM))

soilveg_vars <- c(
  "SPP", "LAI", "HEIGHTC", "SA", "DBH", "BASAL_AREA", "TREES_NUM", 
  "ROOT_DEPTH", "PHEN_EVENT_TYPE",
  "SOIL_CHEM", "SOIL_STOCK", "SOIL_TEX", "PFCURVE", "WTD", "SWC",
  "SOIL_WRB_GROUP", "SOIL_ORDER", "SOIL_CLASSIFICATION", "SOIL_SERIES", "SOIL_DEPTH"
)

soil_vars <- c(
  "SOIL_CHEM", "SOIL_STOCK", "SOIL_TEX", "PFCURVE", "WTD", "SWC",
  "SOIL_WRB_GROUP", "SOIL_ORDER", "SOIL_CLASSIFICATION", "SOIL_SERIES", "SOIL_DEPTH"
)

veg_vars <- c(
  "SPP", "LAI", "HEIGHTC", "SA", "DBH", "BASAL_AREA", "TREES_NUM", 
  "ROOT_DEPTH", "PHEN_EVENT_TYPE")


SoilVegRep <- ReportedInfo %>%
  filter(str_remove(VARIABLE_GROUP, "^GRP_") %in% soilveg_vars) %>%
  left_join(
    badm %>% select(SITE_ID, VARIABLE_GROUP, VARIABLE, DATAVALUE),
    by = c("SITE_ID", "VARIABLE_GROUP", "VARIABLE")
  ) %>%
  mutate(DATAVALUE = ifelse(str_detect(VARIABLE, "DATE"), DATAVALUE, NA)) %>%
  mutate(DATAVALUE = as.numeric(DATAVALUE))%>%
  group_by(SITE_ID, VARIABLE_GROUP, VARIABLE) %>%
  slice_max(order_by = DATAVALUE, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  mutate(AVAILABLE_PI = NA,
         DATE_COLLECTED_PI = NA,
         COLLECT = NA)%>%
  rename(DATE_COLLECTED_BADM = DATAVALUE)

test_list <- split(SoilVegRep, SoilVegRep$SITE_ID)

##===save as excel workbook===
wb <- createWorkbook()

# Loop through your list and add each dataframe as a sheet
for (name in names(test_list)) {
  addWorksheet(wb, sheetName = name)
  writeData(wb, sheet = name, x = test_list[[name]])
}

# Save the workbook
saveWorkbook(wb, file = "Test_SiteData.xlsx", overwrite = T)
