# Generate a flux footprint prediction using the Kljun et al. (2015) footprint model
# Use a data frame made in Prep_FFP_Inputs.R 
# Load Kljun's calc_footprint_FPP_climatology.R prior to using this script

#===============================================================================
#===Change site name as needed before proceeding================================

SiteCode = "US-SRM"

#===============================================================================
#===Load files and necessary packages===========================================
PathtoFFPdata <- file.path("./Data/FFP_Input_Data", paste0(SiteCode, "_ffp_data.csv"))
KljunClimatologyPath <- "./Code/kljun_code/calc_footprint_FFP_climatology.R"
PathtoFileOutput <- file.path("./Data/FFP_Outputs", paste0(SiteCode, "_ffp.RDS"))

ffp_inputs <- read.csv(PathtoFFPdata)

library(fields)
library(EBImage)
source(KljunClimatologyPath)

#===Generate Flux Footprint Prediction==========================================
calc_ffp <- function(x) {
  calc_footprint_FFP_climatology(
    zm = x$zm, 
    z0 = x$z0, 
    umean = x$umean, 
    h = x$h, 
    ol = x$ol, 
    sigmav = x$sigmav,
    ustar = x$ustar,
    wind_dir = x$wind_dir, 
    fig = 1,
    domain = c(-200, 200, -200, 200), 
    r = seq(0, 80, by = 10)
  )
}

#ffp_inputs = ffp_inputs[1:10,]
ffp <- calc_ffp(ffp_inputs)

#===Save Output=================================================================
saveRDS(ffp, PathtoFileOutput)
