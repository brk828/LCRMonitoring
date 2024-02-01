## - Check for existence of main tables, and if they don't, install them.

# Load Scanning data tables if needed from file or run code to develop them
if(exists("ScanningTable")==FALSE){
  if(file.exists("dependencies/ReportDataTables.RData")){
    load("dependencies/ReportDataTables.RData")
  }
  else{source("ImportDataWrangling.R")}
}

packages(dplyr)
packages(stringr)
packages(rgdal)


#########
############################ FY Scanning Summaries
#########
#Turn "unit_type" blanks into "Submersible" (all blanks in dataset were actually submersibles)
# No blanks as of Oct 2022
#ScanEffortTable$unit_type[ScanEffortTable$unit_type == ""] <- "Submersible"


#Same for "Allflex", actually a submersible. Corrected on Ncreased.
# No Allflex as of Oct 2022
#ScanEffortTable$unit_type[ScanEffortTable$unit_type == "Allflex"] <- "Submersible"


############################ Total scan hours (Total, Submersible, and Shore Based)
#########

#-Reduce scan data
ScanEffortFY <- MohaveScanEffort %>% 
  filter(EffortFY == ReportingFY)

ScanDataFY <- MohaveScanning %>% 
  filter (EffortFY == ReportingFY)

##### Longest valid submersible scan time in scan hours
# pulls record, check results to see if valid deplopment
maxsub <- ScanEffortFY %>% 
  filter(str_detect(tolower(UnitType), pattern="submersible")) %>% 
filter(ScanTime == max(ScanTime))

# Prints out max scan time in hours
maxsub$ScanTime/60

#-Sum scanning minutes, convert to hours 
ScanTimeFY <- ScanEffortFY %>% 
  summarise(TotalTime = sum(ScanTime)/60)

ScanTimeFY$TotalTime


############
#################################################### BY ZONE PIT SCANNING SUMMARIES
############

############# Remote scan hours, by zone fy, by location fy
ScanTimeByZoneFY <- ScanEffortFY %>% 
  group_by(DecimalZone, tolower(UnitType)) %>% 
  summarise(TotalScanTime = sum(ScanTime/60), MeanScanTime = mean(ScanTime/60)) %>%
  ungroup()

ScanTimeByLocationFY <- ScanEffortFY %>%
  group_by(Location, tolower(UnitType)) %>%
  summarise(TotalScanTime = sum(ScanTime/60), MeanScanTime = mean(ScanTime/60)) %>%
  ungroup()
#######
############################### Total contacts FY all species
#######

#-Filter for FY


ContactsByLocationFY <- ScanDataFY %>%
  group_by(Location) %>%
  summarise(Contacts=n(), UniquePIT = n_distinct(PIT))

#######
##############################  Unique contacts FY
#######
### - Total uniques in FY

# unique PIT tags
n_distinct(ScanDataFY$PIT)

UniqueFishFY <- ScanDataFY %>%
  filter(!is.na(PITIndex)) %>%
  group_by(Species, ReleaseZone, ReleaseDate, PITIndex) %>%
  summarise(Contacts = n(), PITContacted = n_distinct(PIT)) %>%
  ungroup()

# Total PIT contacts with a record in the NFWG database
sum(UniqueFishFY$PITContacted)

# This gives the number of unique fish contacted
nrow(UniqueFishFY)

# Total number of fish contacted by two different PIT tags
nrow(filter(UniqueFishFY, PITContacted==2))

# Total number of fish contacted by three different PIT tags
nrow(filter(UniqueFishFY, PITContacted==3))


#######
############################  Fish with marking record History numbers
#######

##### Total Contacts GIEL and XYTE in FY
GIELandXYTETotalsFY <- UniqueFishFY %>% 
  mutate(Released = !is.na(ReleaseDate)) %>%
  group_by(Species, Released) %>% 
  count(Species, sort=TRUE)


##
################################## Contacts per zone fy
##

############ Total contacts fy, both species, by zone
TotalContactsByZoneFY <- ScanDataFY %>% count(DecimalZone, sort=TRUE)

############ Total UNIQUE contacts FY, both species, by zone
#-Unique contacts, but by zone 

ZoneSummaryFY <- ScanDataFY %>%
  group_by(Species, DecimalZone, PIT, PITIndex) %>%
  summarise(Contacts = n()) %>%
  ungroup() %>%
  group_by(DecimalZone) %>%
  summarise(Contacts = sum(Contacts), PITContacts = n_distinct(PIT), 
            WithRecord = sum(!is.na(PITIndex))) %>%
  ungroup() 

ZoneUniqueFY <- ScanDataFY %>%
  filter(Species == "XYTE", !is.na(PITIndex)) %>%
  mutate(Released = !is.na(ReleaseDate)) %>%
  group_by(DecimalZone, PITIndex, Released) %>%
  summarise(PITContacted = n_distinct(PIT),
            Released = max(Released)) %>%
                ungroup() %>%
  group_by(DecimalZone) %>%
  summarise(UniqueXYTE = n_distinct(PITIndex), DoubleContacts = sum(PITContacted == 2),
   TripleContacts = sum(PITContacted == 3), FishReleased = sum(Released >0)) %>%
  ungroup()
              
FishContactedBothZones <- ScanDataFY %>%
  filter(Species == "XYTE", !is.na(PITIndex)) %>%
  group_by(PITIndex) %>%
  summarise(River = sum(DecimalZone == 2.1), Basin = sum(DecimalZone == 2.3))
FishContactedBothZones <- FishContactedBothZones %>%
  filter(River > 0, Basin > 0)



