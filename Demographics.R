### Outputs the data summary necessary for the Demographics Table

set.seed(123)

datalist <- readRDS("QLD_Study_Datasets_1.RDS")

people <- datalist[[1]]
tested <- datalist[[2]]
cohort1People <- datalist[[3]]
pregnant <- datalist[[4]]
forImputation <- datalist[[5]]
imp <- datalist[[6]]

## Total Demographics

summary(as.factor(people$AgeGrouped))
round(100*summary(as.factor(people$AgeGrouped))/nrow(people),1)


summary(as.factor(people$Location))
round(100*summary(as.factor(people$Location))/nrow(people),1)


### Positive tests
positive <- subset(people, COHORT=='COHORT1')

summary(as.factor(positive$AgeGrouped))
round(100*summary(as.factor(positive$AgeGrouped))/nrow(positive),1)


summary(as.factor(positive$Location))
round(100*summary(as.factor(positive$Location))/nrow(positive),1)


summary(as.factor(positive$ATSItesting))
round(100*summary(as.factor(positive$ATSItesting))/nrow(positive),1)

summary(as.factor(positive$infectionType))
round(100*summary(as.factor(positive$infectionType))/nrow(positive),1)


### Negative tests
negative <- subset(people, COHORT=='COHORT2')

summary(as.factor(negative$AgeGrouped))
round(100*summary(as.factor(negative$AgeGrouped))/nrow(negative),1)

chisq.test(table(droplevels(subset(people, COHORT!='COHORT3')$COHORT), subset(people, COHORT!='COHORT3')$AgeGrouped))


summary(as.factor(negative$Location))
round(100*summary(as.factor(negative$Location))/nrow(negative),1)

chisq.test(table(droplevels(subset(people, COHORT!='COHORT3')$COHORT), droplevels(subset(people, COHORT!='COHORT3')$Location)))


summary(as.factor(negative$ATSItesting))
round(100*summary(as.factor(negative$ATSItesting))/nrow(negative),1)

chisq.test(table(droplevels(subset(people, COHORT!='COHORT3')$COHORT), droplevels(subset(people, COHORT!='COHORT3')$ATSItesting)))


### Tested Ever tests
peopleTested <- subset(people, testedEver==1)

summary(as.factor(peopleTested$AgeGrouped))
round(100*summary(as.factor(peopleTested$AgeGrouped))/nrow(peopleTested),1)

chisq.test(table(people$testedEver, people$AgeGrouped))


summary(as.factor(peopleTested$Location))
round(100*summary(as.factor(peopleTested$Location))/nrow(peopleTested),1)

chisq.test(table(people$testedEver, droplevels(people$Location)))


### Never Tested

peopleNeverTested <- subset(people, testedEver==0)

summary(as.factor(peopleNeverTested$AgeGrouped))
round(100*summary(as.factor(peopleNeverTested$AgeGrouped))/nrow(peopleNeverTested),1)


summary(as.factor(peopleNeverTested$Location))
round(100*summary(as.factor(peopleNeverTested$Location))/nrow(peopleNeverTested),1)




