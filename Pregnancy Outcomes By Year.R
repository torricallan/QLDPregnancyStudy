### Counts the occurence of pregnancies in the data by year group

set.seed(123)

datalist <- readRDS("QLD_Study_Datasets_1.RDS")

people <- datalist[[1]]
tested <- datalist[[2]]
cohort1People <- datalist[[3]]
pregnant <- datalist[[4]]
forImputation <- datalist[[5]]
imp <- datalist[[6]]


pregnancyByTimePeriod <- pregnant %>% group_by(UNIQUE_NR) %>% mutate(pregnant0001 = ifelse(BABY_DOB > "2000-01-01" & BABY_DOB < "2002-12-31", 1, 0)
                                                                             , pregnant0203 = ifelse(BABY_DOB > "2002-01-01" & BABY_DOB < "2003-12-31", 1, 0)
                                                                             , pregnant0405 = ifelse(BABY_DOB > "2004-01-01" & BABY_DOB < "2005-12-31", 1, 0)
                                                                             , pregnant0607 = ifelse(BABY_DOB > "2006-01-01" & BABY_DOB < "2007-12-31", 1, 0)
                                                                             , pregnant0809 = ifelse(BABY_DOB > "2008-01-01" & BABY_DOB < "2009-12-31", 1, 0)
                                                                             , pregnant1011 = ifelse(BABY_DOB > "2010-01-01" & BABY_DOB < "2011-12-31", 1, 0)
                                                                             , pregnant1213 = ifelse(BABY_DOB > "2012-01-01" & BABY_DOB < "2013-12-31", 1, 0))


pregnancyByTimePeriod$pregnant0001 <- ifelse(is.na(pregnancyByTimePeriod$pregnant0001), 0, pregnancyByTimePeriod$pregnant0001)
pregnancyByTimePeriod$pregnant0203 <- ifelse(is.na(pregnancyByTimePeriod$pregnant0203), 0, pregnancyByTimePeriod$pregnant0203)
pregnancyByTimePeriod$pregnant0405 <- ifelse(is.na(pregnancyByTimePeriod$pregnant0405), 0, pregnancyByTimePeriod$pregnant0405)
pregnancyByTimePeriod$pregnant0607 <- ifelse(is.na(pregnancyByTimePeriod$pregnant0607), 0, pregnancyByTimePeriod$pregnant0607)
pregnancyByTimePeriod$pregnant0809 <- ifelse(is.na(pregnancyByTimePeriod$pregnant0809), 0, pregnancyByTimePeriod$pregnant0809)
pregnancyByTimePeriod$pregnant1011 <- ifelse(is.na(pregnancyByTimePeriod$pregnant1011), 0, pregnancyByTimePeriod$pregnant1011)
pregnancyByTimePeriod$pregnant1213 <- ifelse(is.na(pregnancyByTimePeriod$pregnant1213), 0, pregnancyByTimePeriod$pregnant1213)



peoplePregnancyPerTime <- pregnancyByTimePeriod %>% group_by(UNIQUE_NR) %>% summarise(pregnant0001 = ifelse(sum(pregnant0001) > 0, 1, 0)
                                                                                      , pregnant0203 = ifelse(sum(pregnant0203) > 0, 1, 0)
                                                                                      , pregnant0405 = ifelse(sum(pregnant0405) > 0, 1, 0)
                                                                                      , pregnant0607 = ifelse(sum(pregnant0607) > 0, 1, 0)
                                                                                      , pregnant0809 = ifelse(sum(pregnant0809) > 0, 1, 0)
                                                                                      , pregnant1011 = ifelse(sum(pregnant1011) > 0, 1, 0)
                                                                                      , pregnant1213 = ifelse(sum(pregnant1213) > 0, 1, 0))
people$AgeCentered <- as.list(people$AgeCentered)
people <- left_join(people, peoplePregnancyPerTime, by = "UNIQUE_NR")

people$pregnant0005 <- with(people, ifelse(pregnant0001 + pregnant0203 + pregnant0405 > 0, 1, 0))

table(people$COHORT, people$pregnant0005)

people$pregnant0609 <- with(people, ifelse(pregnant0607 + pregnant0809 > 0, 1, 0))

table(people$COHORT, people$pregnant0609)

people$pregnant1013 <- with(people, ifelse(pregnant1011 + pregnant1213 > 0, 1, 0))

table(people$COHORT, people$pregnant1013)
