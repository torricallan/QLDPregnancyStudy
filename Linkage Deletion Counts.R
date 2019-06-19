### Repeats the linkage deletion process
###, and outputs a count of the multiple linkages that are cleaned.
### Other relevant datasets are produced for summary of duplicate links.
set.seed(123)

library(dplyr)
library(readxl)
library(lubridate)

### Data Read In

perinatal <- read_excel("PregnancyOutcomeDataStudy/DeBattista_Summary_May2016_deidentified.xlsx", 
                        sheet = "Data PID")

Cohort1 <- read_excel("PregnancyOutcomeDataStudy/Detected_2000_2005_COHORT1 positivity data deidentified.xlsx")
Cohort2 <- read_excel("PregnancyOutcomeDataStudy/Cohort 2 NEGATIVE De-identified.xlsx")
Cohort3 <- read_excel("PregnancyOutcomeDataStudy/Control Cohort Proj ID deidentified.xlsx")

people.notCleaned <- perinatal[unique(perinatal$UNIQUE_NR),]

Cohort1$Age <- interval(Cohort1$DOB, ymd('20051231')) 
Cohort1$Age <- Cohort1$Age %/% years(1)

Cohort2$Age <- interval(Cohort2$DOB, ymd('20051231')) 
Cohort2$Age <- Cohort2$Age %/% years(1)

Cohort3$Age <- interval(Cohort3$DOB, ymd('20051231')) 
Cohort3$Age <- Cohort3$Age %/% years(1)


### Infection Types
Cohort1$testResults <- ifelse(is.na(Cohort1$Result), "DETECTED", ifelse(Cohort1$Result == "Not Detected", "Not Detected", "DETECTED"))

Cohort1$infectionStatusChlamydial <- ifelse(Cohort1$TestName == 'CHPCRT' & Cohort1$testResults == "DETECTED", 1, 0)
Cohort1$infectionStatusGonoccocal <- ifelse(Cohort1$TestName %in% c('NGPCRT', 'NGSPCR') & Cohort1$testResults == "DETECTED", 1, 0)

coInfection <- Cohort1 %>% group_by(ProjID, Collected_Date) %>% summarise(coInfection = ifelse(sum(infectionStatusChlamydial) > 0 & sum(infectionStatusGonoccocal) > 0, 1, 0))
personCoInfected <- coInfection %>% group_by(ProjID) %>% summarise(coInfectionEver = ifelse(sum(coInfection) > 0, 1, 0))
Cohort1 <- merge(Cohort1, personCoInfected, by='ProjID', all.x=TRUE)

infectionType <- Cohort1 %>% group_by(ProjID) %>% summarise(infectionType = ifelse(sum(infectionStatusChlamydial) > 0 & sum(infectionStatusGonoccocal) > 0, 'CTNG',
                                                                                   ifelse(sum(infectionStatusChlamydial) > 0 & sum(infectionStatusGonoccocal) == 0, 'CT', 
                                                                                          ifelse(sum(infectionStatusChlamydial) == 0 & sum(infectionStatusGonoccocal) > 0, 'NG', NA))))

Cohort1 <- merge(Cohort1, infectionType, by='ProjID', all.x=TRUE)

Cohort2$infectionStatusChlamydial <- 0
Cohort3$infectionStatusChlamydial <- 0

Cohort2$infectionStatusGonoccocal <- 0
Cohort3$infectionStatusGonoccocal <- 0

Cohort2$coInfectionEver <- 0
Cohort3$coInfectionEver <- 0

Cohort2$infectionType <- "Negative Only"
Cohort3$infectionType <- "Never Tested"

### Coding the number of tests
Cohort1$testDate <- as.Date(Cohort1$Collected_Date)

daysSinceLastTest <- Cohort1 %>% group_by(ProjID) %>% 
  arrange(Collected_Date) %>% 
  filter(Result == 'DETECTED') %>% 
  mutate(daysSinceLastTest = difftime(Collected_Date, lag(Collected_Date), units="days")) %>%
  filter(daysSinceLastTest > 30 | is.na(daysSinceLastTest))

numTests <- daysSinceLastTest %>% summarise(numTests = length(unique(testDate)))

Cohort1 <- merge(Cohort1, numTests, all.x=TRUE)

Cohort2$numTests <- 1
Cohort3$numTests <- 0


### ATSI Status at time of testing
ATSItestinggrouped <- Cohort1 %>% group_by(ProjID) %>% summarise(ATSIcount=sum(ifelse(LITMethnicity_name == 'Aboriginal - UPDATED' | LITMethnicity_name == 'TSI' | LITMethnicity_name == 'Both', 1, 0)))
Cohort1 <- merge(Cohort1, ATSItestinggrouped, by='ProjID', all.x = TRUE)
Cohort1$ATSItesting <- as.factor(ifelse(Cohort1$ATSIcount > 0, 'ATSI', 'nonATSI'))

Cohort2$ATSItesting <- as.factor(ifelse(Cohort2$LITMethnicity_name == 'Aboriginal - UPDATED' | Cohort2$LITMethnicity_name == 'TSI' | Cohort2$LITMethnicity_name == 'Both', 'ATSI', 'nonATSI'))
Cohort3$ATSItesting <- 'Unknown'

### Matching columns in each testing dataframe
Cohort1 <- Cohort1[ , colnames(Cohort1) %in% c('ProjID', 'DOB', 'Postal_Code', 'numTests', 'ATSItesting', 'infectionStatusChlamydial', 'infectionStatusGonoccocal', 'coInfectionEver', 'infectionType')]
Cohort2 <- Cohort2[ , colnames(Cohort2) %in% c('ProjID', 'DOB', 'Postal_Code', 'numTests', 'ATSItesting', 'infectionStatusChlamydial', 'infectionStatusGonoccocal', 'coInfectionEver', 'infectionType')]
Cohort3 <- Cohort3[ , colnames(Cohort3) %in% c('ProjID', 'DOB', 'Postal_Code', 'numTests', 'ATSItesting', 'infectionStatusChlamydial', 'infectionStatusGonoccocal', 'coInfectionEver', 'infectionType')]

Cohort1$COHORT <- 'Cohort1'
Cohort2$COHORT <- 'Cohort2'
Cohort3$COHORT <- 'Cohort3'

### Combining data to a single dataframe
testing <- rbind(rbind(Cohort1, Cohort2), Cohort3)
testing <- testing %>% group_by(ProjID) %>% slice(1) %>% ungroup()
testing$flagTested <- as.integer(1)


### Merging testing data to the perinatal dataframe
perinatal <- merge(perinatal, testing, by.x='PROJID', by.y='ProjID', all.x=TRUE)
perinatal$flagTested <- ifelse(is.na(perinatal$flagTested), 0, perinatal$flagTested)
perinatal <- perinatal[perinatal$flagTested==1,]

perinatal$COHORT.testing <- perinatal$COHORT.y
perinatal$COHORT.perinatal <- perinatal$COHORT.x
perinatal$COHORT.x <- NULL
perinatal$COHORT.y <- NULL

### Counting number of linkage ID's that each unique identifier has in the perinatal dataset
numTestingLinkages <- perinatal %>% group_by(UNIQUE_NR) %>% summarise(numTestingLinkages = length(unique(PROJID)),
                                                                      numCohortLinkages = length(unique(COHORT.perinatal)))
perinatal <- merge(perinatal, numTestingLinkages, by='UNIQUE_NR', all.x=TRUE)

### Deleting extra links to testing people
perinatal <- perinatal[sample(nrow(perinatal)),]

toDeduplicate <- perinatal[perinatal$numTestingLinkages!=1,]
notDeduplicating <- perinatal[perinatal$numTestingLinkages==1,]

toDeduplicate <- toDeduplicate %>% group_by(UNIQUE_NR) %>% filter(PROJID==first(PROJID)) %>% ungroup()

perinatalCleaned <- rbind(toDeduplicate, notDeduplicating)


### Summarising data at a person level
people <- perinatalCleaned[!duplicated(perinatalCleaned$PROJID),] # one row per person

### Outsputs the number of multiple linkages to be cleaned
summary(as.factor(subset(numTestingLinkages, numCohortLinkages>1)$numCohortLinkages))
