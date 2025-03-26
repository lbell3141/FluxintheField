# Prepare a data frame for calculating a flux footprint prediction
# Using the Kljun et al. (2015) footprint model
# Obtain measurement height, canopy height, and meteorological data from Ameriflux
# Assume zero-plane displacement height is 2/3rds the canopy height
# Assume stable boundary layer conditions 
# Assume lateral wind speed variation ~1
# Use half hourly fluxes for accurate wind direction
# Use only daytime fluxes (from 8am to 5pm)

#===============================================================================
#===Change filepaths as needed before proceeding================================

PathtoFluxData <- "X:/moore/FluxNetData/AMF_US-SRM_FLUXNET_SUBSET_2004-2023_3-6/AMF_US-SRM_FLUXNET_SUBSET_HH_2004-2023_3-6.csv"
#PathtoFluxData <- "X:/moore/FluxNetData/AMF_US-SRG_FLUXNET_SUBSET_2008-2023_4-6/AMF_US-SRG_FLUXNET_SUBSET_HH_2008-2023_4-6.csv"
SiteCode <- "US-SRM"
PathtoFileOutput <- file.path("./Data/FFP_Input_Data", paste0(SiteCode, "_ffp_data.csv"))

#===============================================================================
#===============================================================================
#===Load files and necessary packages===========================================
library(lubridate)
library(dplyr)

PathtoBADM <- "Data/AMF_AA-Net_BIF_CCBY4_20250318.csv"
PathtoMeasHeight <- "Data/BASE_MeasurementHeight_20250318.csv"

FluxData <- read.csv(PathtoFluxData)
badm <- read.csv(PathtoBADM)
MeasHeightData <- read.csv(PathtoMeasHeight)

#===Pull Sensor and Canopy Height===============================================
CanHeight <- badm %>%
  filter(SITE_ID == SiteCode, 
         VARIABLE == "HEIGHTC")%>%
  pull(DATAVALUE)

MeasHeight <- MeasHeightData%>%
  filter(Site_ID == SiteCode,)%>%
  summarize(max(Height, na.rm = T))%>%
  pull()

DisplaceHeight <- (0.1) * MeasHeight

#===Format data frame for footprint calculation=================================
ffp_data <- FluxData %>%
  mutate(TIMESTAMP_END = ymd_hm(TIMESTAMP_END))%>%
  summarize(
    yyyy = year(TIMESTAMP_END),
    mm = month(TIMESTAMP_END),
    day = day(TIMESTAMP_END),
    HH_UTC = hour(TIMESTAMP_END),
    MM = minute(TIMESTAMP_END),
    zm = MeasHeight - DisplaceHeight,
    z0 = NaN,
    umean = mean(WS_F, na.rm = TRUE),
    h = 1000,
    ol = (-((USTAR^3) * (TA_F + 273)) / (0.4 * 9.8 * (H_F_MDS / (1.25 * 1004)))),
    sigmav = 0.75,
    ustar = USTAR,
    wind_dir = WD,
    test = zm/ol)
#additional stipulations (see kjlun's documentation)
ffp_inputs <- ffp_data%>%
  filter(test >= -15.5)%>%
  filter(ustar > 0.2)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% 8:17)%>%
  filter(yyyy %in% 2017)%>%
  select(!test)

#===Save Output as Needed=======================================================
#write.csv(ffp_inputs, PathtoFileOutput, row.names = F)

