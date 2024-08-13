# B. Kesner adapted from C. Ehlo 3 May 2023

# Post-Release contact analysis using logistic regression.  This script will take confirmed release records
# for a given species and reach and determine if they survived to a certain time after release based on 
# PIT scanning records.  The resultant survival data will be used to estimate the size apparent survival
# relationship differentiated across release seasons.  The resultant probabilities are a combination of 
# survival and detectability and are expected to be used as more to compare relationships across seasons, species
# and reaches and not to estimate actual survival. 


ContactLimit <- 10000 # contact cutoff to remove likely dead tags
MinReleaseYear <- FirstScanFY # Limit to fish release on or after this year
EarlySpringStartMonth <- 2 # Start of Early Spring month number (2 = February)
LateSpringStartMonth <- 4 # Start of Late Spring
SummerStartMonth <- 6 # Start of Summer/Fall usually few fish are released
WinterStartMonth <- 11 # Start of Winter month

# packages function will install and load libraries when needed, if already installed it will just load
# packages function should be a part of the BasinScanningIndex workspace
packages(ggplot2)
packages(dplyr)     # data manipulation
packages(magrittr)  # allows use of %<>% assignment pipe
packages(glmmTMB) # General linear mixed model analysis built on TMB automatic differentiation engine
packages(lubridate)

CurrentYear <- year(Sys.Date())

#Create release table adding size class, release month, and ensuring TL is present.
SpReachReleases <- ReachReleases %>% 
  select(PITIndex = PIT1, Reach, Species, ReleaseYear, ReleaseZone, ReleaseDate, ReleaseTL, TLCM, 
         ReleaseLocation, StockingID) %>%
  filter(Species == Sp, Reach > 1, ReleaseYear >= MinReleaseYear, !is.na(ReleaseTL), 
         ReleaseYear < CurrentYear-1) %>%
  mutate(ReleaseMonth = month(ReleaseDate), 
         SizeClass = case_when(ReleaseTL < SizeClass2 ~ 'Juvenile', ReleaseTL >= SizeClass2 ~ 'Adult'),
         Season = ifelse(ReleaseMonth >= WinterStartMonth | ReleaseMonth < EarlySpringStartMonth, "Winter", 
                ifelse(ReleaseMonth >= EarlySpringStartMonth & ReleaseMonth < LateSpringStartMonth, "Early Spring",
                       ifelse(ReleaseMonth >= LateSpringStartMonth & 
                                ReleaseMonth < SummerStartMonth, "Late Spring", "Summer-Fall"))))
         
 #        Season = case_when(ReleaseMonth %in% c("9", "10", "11", '12') ~ "Winter", 
  #                                   ReleaseMonth %in% c("1", "2", "3") ~ "Early Spring",
   #                                  ReleaseMonth %in% c("4", "5") ~ "Late Spring"))

# Join contacts with Index dataframe to match contacts with release record of given species
# grouping by PITIndex ensures that fish with more than one PIT tag scanned will be counted as one fish
ReachContactsSummary <- ReachContacts %>%
  group_by(PITIndex) %>%
  summarise(MaxDAL = max(DAL), Contacts = n()) %>%
  ungroup

  
# Survival is determined if an individual was contacted at least once after the SurvivalDAL
# set at the beginning of the script.  Fish with suspect contact records (over the ContactLimit)
# are removed entirely so as not to be counted as dead or alive (removed from release cohort)
SpeciesSurvival <- SpReachReleases %>% 
  select(ReleaseMonth, Reach, ReleaseYear, Season, SizeClass, PITIndex, ReleaseTL, TLCM) %>%
  left_join(ReachContactsSummary %>% select(MaxDAL, Contacts, PITIndex), 
            by = c("PITIndex" = "PITIndex")) %>% 
  filter(Contacts < ContactLimit| is.na(Contacts)) %>%
  mutate(MaxDAL = if_else(MaxDAL<0|is.na(MaxDAL),0,MaxDAL), 
         Contacts = if_else(is.na(Contacts), 0, Contacts),
         Alive = if_else(MaxDAL>=SurvivalDAL, 1, 0),
         ReleaseYear = as.factor(ReleaseYear),
         Reach = as.factor(Reach), 
         TLClass = TLCM*10)

SpeciesSurvivalSummary <- SpeciesSurvival %>% 
  group_by(Reach, ReleaseYear, Season, TLClass) %>%
  summarise(Released = n(), Survivors = sum(Alive), AliveProp = Survivors/Released) %>%
  ungroup() %>%
  filter(Released > 20, TLClass >= 300)
  
#zero inflated model by DAL and Season.  Could cluster by Zone and also add size at release
DALModel <- glmmTMB(Alive ~ ReleaseTL * Season,
                     family = binomial(link = 'logit'), 
                     data = SpeciesSurvival)

#creating a predictions data.frame from model for graphing
Predictors <- expand.grid(ReleaseTL = as.integer(seq(300, 600, by = 10)),
                           Season = c("Winter", "Early Spring", "Late Spring"))
                           
Predictions <- predict(DALModel, newdata = Predictors, type = "response", se.fit = TRUE)
lowerCI <- Predictions$fit - (1.96 * Predictions$se.fit)
upperCI <- Predictions$fit + (1.96 * Predictions$se.fit)
fit <- Predictions$fit
Predicted <- cbind(Predictors, fit, lowerCI, upperCI)

DALGraph <- ggplot(Predicted, 
                     aes(x = ReleaseTL, y = fit, color = Season)) + 
  geom_line(linewidth = 1) + 
 labs(x = 'Total Length (mm)', y = 'Detection Probability', color = 'Season') + 
 theme(plot.margin = margin(.75,.75,.75,.75, unit = 'cm'), 
     axis.title.x = element_text(vjust = -2), axis.title.y = element_text(vjust = 5)) +
 geom_ribbon(aes(x = ReleaseTL, ymin = lowerCI, ymax = upperCI, fill = Season), alpha = 0.3) +
  geom_point(data = SpeciesSurvivalSummary %>%
               filter(Reach == 2), 
             aes(x = TLClass, y = AliveProp))

DALGraph

Survived <- SpeciesSurvival %>% 
  filter(Alive == 1)

hist <- ggplot(SpeciesSurvival, aes(x = ReleaseTL, fill = Alive)) + 
  geom_histogram()  + facet_grid(Alive ~ Season)
hist


