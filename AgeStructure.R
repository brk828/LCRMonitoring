# B. Kesner 29 January 2024
# This is an age structure analysis based on the 2023 Mohave report to be run
# as part of the general Reach analysis auto-generated report.

# If this script is run alone, data wrangling must be run first
if(exists("StudyReach") == FALSE) {
  source("DataWrangling.R")
  }
MinimumContacts = 200 # cutoff of total unique contacts per zone for inclusion in analysis

packages(dplyr)
packages(ggplot2)
packages(lubridate)
packages(lemon) # additional options for ggplot


# Cutoff is in CM convert from size class 2 cutoff
TLCMCut <- SizeClass2*.1

# Cutoff text for figures is mm in text format.
TLCutoffText <- paste0(TLCMCut, "0")

ReachReleaseSizes  <- ReachReleases %>%
  filter(ReleaseFY > ReportingFY - 14) %>%
  filter(ReleaseFY < ReportingFY - 2) %>%
  filter(!is.na(TLCM)) %>%
  mutate(Size = factor(ifelse(TLCM >= TLCMCut, 
                              paste0(">=", TLCutoffText, " mm TL"), 
                              paste0("<", TLCutoffText, " mm TL")))) %>%
  dplyr::select( PIT1, ReleaseFY, ReleaseZone, TLCM, Size)

ReleaseSizePlot <- ggplot(ReachReleaseSizes, aes(ReleaseFY)) + 
  geom_bar(aes(fill = Size), colour="black") +
  scale_x_continuous(limits = c(ReportingFY-15, ReportingFY-1), 
                     breaks = seq(ReportingFY-14, ReportingFY-1, 2)) +
  scale_y_continuous(limits = c(0, 10000)) +
  scale_fill_manual(values = c('#FFFFFF','#000011')) +
  labs(x = "Release FY", y = "Number of Fish Released") +
  coord_capped_cart(bottom='both', left = 'both') +
  theme_bw(base_size = 15) + theme(panel.border = element_blank(), axis.line=element_line())
ReleaseSizePlot <- ReleaseSizePlot + facet_grid(ReleaseZone ~.)

png(paste0("output/Reach", StudyReach, "ReleaseCohorts.png"), width = 8, height = 6, units = 'in', res = 300)   
ReleaseSizePlot
dev.off()

ReachYAL <- ReachContacts %>% 
  filter(ScanFY < ReportingFY)  %>%
  select(-PITIndex, -PITPrefix) %>%
  inner_join(ReachPITIndex %>%
               select(TLCM, PIT, PITIndex,
                      ReleaseReach = Reach, FirstCensus), 
             by = "PIT") %>%
  mutate(ReleaseAge = ScanFY - ReleaseFY - 1) %>% 
  filter(Species == Sp, !is.na(ReleaseFY), 
         ReleaseAge>0, ScanFY >= FirstScanFY) %>%
  dplyr::select(PITIndex, ScanFY, ReleaseFY, Sex, TLCM, ReleaseZone, 
                ScanZone, ReleaseAge) %>% 
  group_by(PITIndex, ScanFY, Sex, TLCM, ReleaseZone, ScanZone, ReleaseAge, ReleaseFY) %>%
  summarise(Contacts = n()) %>%
  ungroup()

Zones <- ReachYAL %>%
  group_by(ScanZone) %>%
  summarise(Count = n()) %>%
  filter(Count > MinimumContacts) 

ReachYALPlotData <- ReachYAL %>%
  inner_join(Zones %>% select(ScanZone), by = "ScanZone")

YearSplit <- median(unique(ReachYALPlotData$ScanFY))

ReachYALPlot1 <- ggplot(ReachYALPlotData %>% 
                          filter(ScanFY < YearSplit, ScanZone == Zones$ScanZone[1]), 
                        aes(ReleaseAge)) +
  geom_bar(aes(fill = ReleaseZone), colour="black") +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 1000))  +
  labs(x = "Years at Large", y = "Number of Unique Fish Scanned") +
  coord_capped_cart(bottom='both', left = 'both') +
  theme_bw(base_size = 15) + theme(panel.border = element_blank(), axis.line=element_line())
ReachYALPlot1 <- ReachYALPlot1 + facet_grid(ScanFY ~.)

png(paste0("output/Reach", StudyReach, "YALPlot1.png"), width = 6, height = 4, units = 'in', res = 300)   
ReachYALPlot1
dev.off()

ReachYALPlot2 <- ggplot(ReachYALPlotData %>% 
                          filter(ScanFY >= YearSplit, ScanZone == Zones$ScanZone[1]), 
                        aes(ReleaseAge)) +
  geom_bar(aes(fill = ReleaseZone), colour="black") +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 1000))  +
  labs(x = "Years at Large", y = "Number of Unique Fish Scanned") +
  coord_capped_cart(bottom='both', left = 'both') +
  theme_bw(base_size = 15) + theme(panel.border = element_blank(), axis.line=element_line())
ReachYALPlot2 <- ReachYALPlot2 + facet_grid(ScanFY ~.)

png(paste0("output/Reach", StudyReach, "YALPlot2.png"), width = 6, height = 4, units = 'in', res = 300)   
ReachYALPlot2
dev.off()

ReachYALPlot3 <- ggplot(ReachYALPlotData %>% 
                          filter(ScanFY < YearSplit, ScanZone == Zones$ScanZone[2]), 
                        aes(ReleaseAge)) +
  geom_bar(aes(fill = ReleaseZone), colour="black") +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 1000))  +
  labs(x = "Years at Large", y = "Number of Unique Fish Scanned") +
  coord_capped_cart(bottom='both', left = 'both') +
  theme_bw(base_size = 15) + theme(panel.border = element_blank(), axis.line=element_line())
ReachYALPlot3 <- ReachYALPlot3 + facet_grid(ScanFY ~.)

png(paste0("output/Reach", StudyReach, "YALPlot3.png"), width = 6, height = 4, units = 'in', res = 300)   
ReachYALPlot3
dev.off()

ReachYALPlot4 <- ggplot(ReachYALPlotData %>% 
                          filter(ScanFY >= YearSplit, ScanZone == Zones$ScanZone[2]), 
                        aes(ReleaseAge)) +
  geom_bar(aes(fill = ReleaseZone), colour="black") +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 1000))  +
  labs(x = "Years at Large", y = "Number of Unique Fish Scanned") +
  coord_capped_cart(bottom='both', left = 'both') +
  theme_bw(base_size = 15) + theme(panel.border = element_blank(), axis.line=element_line())
ReachYALPlot4 <- ReachYALPlot4 + facet_grid(ScanFY ~.)

png(paste0("output/Reach", StudyReach, "YALPlot4.png"), width = 6, height = 4, units = 'in', res = 300)   
ReachYALPlot4
dev.off()

# Add any needed dataframes into core if they should be retained.
#CoreDataFrames <- append(CoreDataFrames, c(""))

CurrentDataFrames <- ls()[vapply(ls(), function(x) is.data.frame(get(x)), logical(1))]

# Find the dataframes that were created by the script
NewDataFrames <- setdiff(CurrentDataFrames, CoreDataFrames)

# Remove the new dataframes
rm(list = NewDataFrames)

