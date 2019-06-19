### Prepares Dataset for analysis from the raw files.
### Raw files are testing datasets for pos, neg and controls and perinatal datasets.
### Also merges postcode level variables (total number, proportion indigenous, remoteness, SEIFA)

set.seed(123)

library(readxl)
library(dplyr)
library(lubridate)
library(mice)

### Data Read In

perinatal <- read_excel("PregnancyOutcomeDataStudy/DeBattista_Summary_May2016_deidentified.xlsx", 
                        sheet = "Data PID")

Cohort1 <- read_excel("PregnancyOutcomeDataStudy/Detected_2000_2005_COHORT1 positivity data deidentified.xlsx")
Cohort2 <- read_excel("PregnancyOutcomeDataStudy/Cohort 2 NEGATIVE De-identified.xlsx")
Cohort3 <- read_excel("PregnancyOutcomeDataStudy/Control Cohort Proj ID deidentified.xlsx")


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


### Read in postcode information and join to dataset
QLDpostcodesCensusInformation2006 <- read.csv('postcode_data.csv')
people <- merge(people, QLDpostcodesCensusInformation2006, by.x='Postal_Code', by.y='Postal.Area..POA.', all.x=TRUE)

QLDpostcodesSEIFA2006 <- read_excel("QLD postcodes SEIFA 2006.xlsx")
people <- merge(people, QLDpostcodesSEIFA2006, by.x='Postal_Code', by.y='2006 Postal Area code (POA)', all.x=TRUE)

people$postcode <- as.factor(ifelse(grepl('^4\\d{3}', people$Postal_Code, perl=TRUE), people$Postal_Code, ifelse(is.na(people$Postal_Code), 'Missing', 'OutOfState')))

### Coding variables for the models
people$pregnantEver <- ifelse(is.na(people$BABY_DOB), 0, 1)

people$COHORT <- relevel(as.factor(people$COHORT.perinatal), ref='COHORT3') 

people$testedEver <- as.factor(ifelse(people$COHORT=='COHORT3', 0, 1))

people$testingCohort <- as.factor(ifelse(people$COHORT=='COHORT1' & people$numTests==1 , 'positiveOnce', ifelse(people$COHORT=='COHORT1' & people$numTests>1, 'positiveMultiple'
                                                                                                                , ifelse(people$COHORT == 'COHORT2', 'negativeOnly', 'neverTested'))))
people$testingCohort <- relevel(people$testingCohort, ref='neverTested')

people <- people[!is.na(people$DOB),] # create age variable and remove missing DOB
people$Age <- interval(people$DOB, ymd('20051231')) 
people$Age <- people$Age %/% years(1)

people$AgeCentered <- scale(people$Age, center=TRUE, scale=FALSE)
people$AgeGrouped <- ifelse(people$Age < 20, "15-19", 
                            ifelse(people$Age >= 20 & people$Age < 25, "20-24",
                                   ifelse(people$Age >= 25 & people$Age < 30, "25-29"
                                          , "30+")))

people$Remoteness <- as.character(people$Remoteness)
people$Remoteness[is.na(people$Remoteness)] <- "Missing"
people$Remoteness[people$Remoteness == "Migratory"] <- "Missing"
people$Remoteness <- relevel(as.factor(people$Remoteness), ref="Major City")

people$RemotenessGrouped <- as.factor(ifelse(people$Remoteness %in% c('Remote', 'Very Remote'), 'Remote',
                                             ifelse(people$Remoteness %in% c('Inner Regional', 'Outer Regional'), 'Regional',
                                                    ifelse(people$Remoteness == 'Major City', 'Major City', 'Missing'))))


people$ProportionATSI <- people$`Proportion ATSI`

people$SEIFAStatePercentile <- as.numeric(people$`State Percentile`)
people$SEIFAStateTernary <- ifelse(people$SEIFAStatePercentile < 34, "Low",
                                   ifelse(people$SEIFAStatePercentile >= 34 & people$SEIFAStatePercentile < 67, "Middle",
                                          ifelse(people$SEIFAStatePercentile >= 67, "High", "Missing")))

people$SEIFAStateTernary <- as.factor(ifelse(is.na(people$SEIFAStateTernary), "Missing", people$SEIFAStateTernary))

people$Location <- ifelse(people$RemotenessGrouped == 'Major City' & people$SEIFAStateTernary == 'Low', 'MajorCityLow'
                          , ifelse(people$RemotenessGrouped == 'Major City' & people$SEIFAStateTernary == 'Middle', 'MajorCityMiddle'
                                   , ifelse(people$RemotenessGrouped == 'Major City' & people$SEIFAStateTernary == 'High', 'MajorCityHigh'
                                            , ifelse(people$RemotenessGrouped == 'Regional' & people$SEIFAStateTernary == 'Low', 'RegionalLow'
                                                     , ifelse(people$RemotenessGrouped == 'Regional' & people$SEIFAStateTernary == 'Middle', 'RegionalMiddle'
                                                              , ifelse(people$RemotenessGrouped == 'Regional' & people$SEIFAStateTernary == 'High', 'RegionalHigh'
                                                                       , ifelse(people$RemotenessGrouped == 'Remote' & people$SEIFAStateTernary == 'Low', 'RemoteLow'
                                                                                , ifelse(people$RemotenessGrouped == 'Remote' & people$SEIFAStateTernary == 'Middle', 'RemoteMiddle'
                                                                                         , ifelse(people$RemotenessGrouped == 'Remote' & people$SEIFAStateTernary == 'High', 'RemoteHigh'
                                                                                                  , 'Missing')))))))))

people$Location <- as.factor(people$Location)

people.withPostcode <- people
people <- people[people$Location != "Missing",] # Remove missing postcodes

### Tested Only dataset
tested <- people[people$COHORT != 'COHORT3',]

tested$COHORT <- droplevels(tested$COHORT)
tested$ATSItesting <- droplevels(tested$ATSItesting)

tested$COHORT <- relevel(tested$COHORT, ref='COHORT2')
tested$testingCohort <- relevel(tested$testingCohort, ref='positiveOnce')
tested$ATSItesting <- relevel(tested$ATSItesting, ref='nonATSI')

tested$ATSIperinatal <- as.factor(ifelse(tested$Mother_indig_status == 'Indigenous and/or Torres Stait Islander', 'ATSI', 'nonATSI'))
tested$ATSIperinatal <- relevel(tested$ATSIperinatal, ref='nonATSI')

tested$ATSIimputed <- as.factor(ifelse(tested$pregnantEver==0, NA, tested$ATSIperinatal))

tested$AgeGrouped <- as.factor(tested$AgeGrouped)

### New variables for imputation
tested$AgeCentered <- NULL

postcodeStats <- tested %>% group_by(postcode) %>% summarise(Total.Postcode = n(), Proportion.ATSI.testing = mean(ATSItesting=='ATSI')) %>% ungroup()
postcodeStats2 <- tested %>% filter(!is.na(ATSIperinatal)) %>% group_by(postcode) %>% summarise(Proportion.ATSI.perinatal = mean(ATSIperinatal=='ATSI')) %>% ungroup()

tested <- merge(tested, postcodeStats, by = "postcode", all.x=TRUE)
tested <- merge(tested, postcodeStats2, by = "postcode", all.x=TRUE)

tested$Proportion.ATSI.perinatal <- ifelse(is.na(tested$Proportion.ATSI.perinatal), 0, tested$Proportion.ATSI.perinatal)  
tested$moreATSItestingPerinatal <- ifelse(tested$Proportion.ATSI.testing > tested$Proportion.ATSI.perinatal, 1, 0)
tested$moreATSIcensusPerinatal <- ifelse(tested$Proportion.ATSI > tested$Proportion.ATSI.perinatal, 1, 0)

forImputationInteractions <- model.matrix( ~ -1 + ATSItesting*Proportion.ATSI.perinatal + ATSItesting*testingCohort + Proportion.ATSI.perinatal*Total.Postcode, data=tested)
forImputationInteractions <- as.data.frame(forImputationInteractions)


forImputation <- tested[ , colnames(tested) %in% c('pregnantEver', 'testingCohort', 'infectionStatusGonoccocal', 'AgeGrouped', 'ATSItesting', 'ATSIimputed', 'Location','Proportion.ATSI.perinatal', 'Total.Postcode', 'moreATSItestingPerinatal', 'moreATSIcensusPerinatal')]
forImputation <- droplevels(forImputation)
colnames.forImputation <- colnames(forImputation)

forImputation <- cbind(forImputation, forImputationInteractions$`ATSItestingATSI:Proportion.ATSI.perinatal`, forImputationInteractions$`ATSItestingATSI:testingCohortnegativeOnly`
                       , forImputationInteractions$`ATSItestingATSI:testingCohortpositiveMultiple`, forImputationInteractions$`Proportion.ATSI.perinatal:Total.Postcode`)

colnames.forImputation <- cbind(colnames.forImputation, c('ATSITesting:Proportion.ATSI.perinatal', 'ATSItesting:negativeOnly', 'ATSItesting:multiplePositive', 'Proportion.ATSI.perinatal:Total.postcode'))

imp <- mice(forImputation, m=10, maxit=50)

### Cohort 1 people only dataset
cohort1People <- people[people$COHORT == "COHORT1",]
cohort1People$ATSItesting <- relevel(cohort1People$ATSItesting, ref='nonATSI')

cohort1People <- droplevels(cohort1People)

### Pregnant Only dataset
pregnant <- people[people$pregnantEver==1,]

pregnant$preTermBirth <- ifelse(pregnant$GEST_WEEKS < 37, 1, 0)

pregnant$lowBirthWeight <- ifelse(pregnant$BABY_WEIGHT < 2500, 1, 0)

pregnant$ART <- ifelse(is.na(pregnant$asst_concep), 0
                       , ifelse(pregnant$asst_concep == 'Yes', 1
                           , ifelse(pregnant$asst_concep == 'No', 0
                                , ifelse(is.na(pregnant$asst_concep), 0, 0))))

pregnant$stillBorn <- ifelse(pregnant$born_alive_status == "Stillborn", 1, 0)
### Save all datasets

datalist <- list(people, tested, cohort1People, pregnant, forImputation, imp, people.withPostcode)
saveRDS(datalist, "QLD_Study_Datasets_3.RDS")

rm(list=ls())
gc()
