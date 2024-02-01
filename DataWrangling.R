# B. Kesner October 3, 2023
# Prelimnary script to create backwater specific dataframes for any analysis
# this must be run from a script with input parameters included

# Load useful lab functions
source("LabFunctions.R")

# Load data workspace or downlod and load if more than 30 days old
if(file.exists("data/BasinScanningIndex.RData")){
  data_info <- file.info("data/BasinScanningIndex.RData")
  data_date <- as.Date(data_info$mtime)
  if(data_date>Sys.Date() - 30){
    load("data/BasinScanningIndex.RData")
  } else {
    download_basin("data")
    load("data/BasinScanningIndex.RData")
  }
} else {
  download_basin("data")
  load("data/BasinScanningIndex.RData")
}


# Load data workspace or downlod and load if more than 30 days old
if(file.exists("data/NFWGTable.RData")){
  data_info <- file.info("data/NFWGTable.RData")
  data_date <- as.Date(data_info$mtime)
  if(data_date>Sys.Date() - 30){
    load("data/NFWGTable.RData")
  } else {
    download_nfwg("data")
    load("data/NFWGTable.RData")
  }
} else {
  download_nfwg("data")
  load("data/NFWGTable.RData")
}

# Assign Study Reach if not established
if(exists("StudyReach") == FALSE) {StudyReach = 2}

# Assign first scanning FY if not established 
if(exists("FirstScanFY") == FALSE) {FirstScanFY = 2013}

# Assign species if not established 
if(exists("Sp") == FALSE) {Sp = "XYTE"}

# Assign survival DAL if not established 
if(exists("SurvivalDAL") == FALSE) {SurvivalDAL = 120}

# Assign size classes if not established 
if(exists("SizeClass2") == FALSE) {
  SizeClass2 = 400
  SizeClass3 = 500
  }

# remove unnecessary functions
rm(euclid, split_hourly, download_nfwg, download_basin, download_backwater,
   Unit, TripTable, TagStocking, TagEffort)

packages(ggplot2)
packages(dplyr)     # data manipulation
packages(magrittr)  # allows use of %<>% assignment pipe
packages(glmmTMB) # General linear mixed model analysis built on TMB automatic differentiation engine
packages(lubridate)

ReportingFY <- if(exists("ReportingFY") == FALSE) {
  ReportingFY = ifelse(month(Sys.Date())>9, year(Sys.Date())+1, year(Sys.Date()))
}


# Restrict PITindex dataframe to study reach only
ReachPITIndex <- BasinPITIndex %>%
  filter(Reach == StudyReach) 

rm(BasinPITIndex)

# Create Reach specific dataframes and remove basinwide ones
ReachContacts <- BasinContacts %>% 
  filter(Reach == StudyReach) %>%
  rename(ScanZone = DecimalZone, ScanLocation = Location, ScanKm = RiverKm, 
         ScanDate = Date, ScanTime = DateTime) %>%
  left_join(ReachPITIndex %>% 
              select(Species, PIT, PITIndex, DateVerified, ReleaseZone, ReleaseDate,
                     ReleaseTL, Sex, ReleaseFY, ReleaseKm), by = "PIT") %>%
  mutate(DateVerified = as.Date(DateVerified),
         ScanYear = year(ScanDate),
         DAL = ifelse(is.na(DateVerified), 0, 
                      as.integer(difftime(ScanDate, DateVerified, unit = 'days')))) %>%
  select(Reach, Species, PIT, PITIndex, Sex, ScanYear, ScanDate, ScanHr, ScanTime, 
         ScanFY, ScanLocation, ScanZone, ScanKm, ReleaseZone, ReleaseKm, ReleaseDate, 
         ReleaseFY, ReleaseTL, DAL, PITPrefix, UnitType, DateVerified, EffortFY)

rm(BasinContacts)

ReachEffort <- BasinEffort %>%
  filter(Reach == StudyReach)

rm(BasinEffort)

ReachContactsNoIndex <- ReachContacts %>%
  filter(is.na(PITIndex)) %>%
  group_by(PIT) %>%
  summarise(Contacts = n(), FirstScan = min(ScanTime), LastScan = max(ScanTime)) %>%
  ungroup() %>%
  arrange(desc(Contacts))

# Create dataframe of only the most recent contact of all contacts from reach
ReachLastContact <- ReachContacts %>% 
  filter(!is.na(PITIndex)) %>%
  arrange(PITIndex, desc(ScanTime)) %>%
  group_by(PITIndex) %>%
  dplyr::slice(1) %>%
  mutate(LastScan = as.Date(ScanDate)) %>% 
  select(PITIndex, LastScan, ScanHr)

# Clean up table, add size classes, create event and disposition fields for future NFWG table structure,
# and add a recapture field based on actual previous records instead of relying on database classification
ReachNFWGTable <- NFWGTable %>% 
  select(Reach, Species, CollectionDate, PIT1 = First134PIT, PIT2 = Second134PIT, 
         Sex, TL, WT, Status, Recapture, DecimalZone, ReservoirKm, RiverKm, Latitude, Longitude, 
         RearingLocation, Method, SurfaceConnection) %>%
  filter(Species == Sp, Reach == StudyReach)

rm(NFWGTable, LocationTable)

ReachReleases <- BasinReleases %>% filter(Reach == StudyReach)

rm(ReachTable, BasinReleases, Zone)
