### Outputs the data summary necessary for the Perinatal Demographics Table

set.seed(123)

datalist <- readRDS("QLD_Study_Datasets_1.RDS")

people <- datalist[[1]]
tested <- datalist[[2]]
cohort1People <- datalist[[3]]
pregnant <- datalist[[4]]
forImputation <- datalist[[5]]
imp <- datalist[[6]]

pregnant$ATSIperinatal <- as.factor(ifelse(pregnant$Mother_indig_status == 'Indigenous and/or Torres Stait Islander', 'ATSI', 'nonATSI'))
pregnant$ATSIperinatal <- relevel(pregnant$ATSIperinatal, ref = "nonATSI")

summary(as.factor(pregnant$AgeGrouped))
round(100*summary(as.factor(pregnant$AgeGrouped))/nrow(pregnant),1)


summary(as.factor(pregnant$Location))
round(100*summary(as.factor(pregnant$Location))/nrow(pregnant),1)


summary(as.factor(pregnant$ATSIperinatal))
round(100*summary(as.factor(pregnant$ATSIperinatal))/nrow(pregnant),1)

### Positive tests
positive <- subset(pregnant, COHORT=='COHORT1')

summary(as.factor(positive$AgeGrouped))
round(100*summary(as.factor(positive$AgeGrouped))/nrow(positive),1)


summary(as.factor(positive$Location))
round(100*summary(as.factor(positive$Location))/nrow(positive),1)


summary(as.factor(positive$ATSIperinatal))
round(100*summary(as.factor(positive$ATSIperinatal))/nrow(positive),1)

### Negative tests
negative <- subset(pregnant, COHORT=='COHORT2')

summary(as.factor(negative$AgeGrouped))
round(100*summary(as.factor(negative$AgeGrouped))/nrow(negative),1)

chisq.test(table(droplevels(subset(pregnant, COHORT!='COHORT3')$COHORT), subset(pregnant, COHORT!='COHORT3')$AgeGrouped))


summary(as.factor(negative$Location))
round(100*summary(as.factor(negative$Location))/nrow(negative),1)

chisq.test(table(droplevels(subset(pregnant, COHORT!='COHORT3')$COHORT), droplevels(subset(pregnant, COHORT!='COHORT3')$Location)))


summary(as.factor(negative$ATSIperinatal))
round(100*summary(as.factor(negative$ATSIperinatal))/nrow(negative),1)

chisq.test(table(droplevels(subset(pregnant, COHORT!='COHORT3')$COHORT), droplevels(subset(pregnant, COHORT!='COHORT3')$ATSIperinatal)))


### Tested Ever tests
pregnantTested <- subset(pregnant, testedEver==1)

summary(as.factor(pregnantTested$AgeGrouped))
round(100*summary(as.factor(pregnantTested$AgeGrouped))/nrow(pregnantTested),1)

chisq.test(table(pregnant$testedEver, pregnant$AgeGrouped))


summary(as.factor(pregnantTested$Location))
round(100*summary(as.factor(pregnantTested$Location))/nrow(pregnantTested),1)

chisq.test(table(pregnant$testedEver, droplevels(pregnant$Location)))


summary(as.factor(pregnantTested$ATSIperinatal))
round(100*summary(as.factor(pregnantTested$ATSIperinatal))/nrow(pregnantTested),1)

chisq.test(table(pregnant$testedEver, pregnant$ATSIperinatal))

### Never Tested

pregnantNeverTested <- subset(pregnant, testedEver==0)

summary(as.factor(pregnantNeverTested$AgeGrouped))
round(100*summary(as.factor(pregnantNeverTested$AgeGrouped))/nrow(pregnantNeverTested),1)


summary(as.factor(pregnantNeverTested$Location))
round(100*summary(as.factor(pregnantNeverTested$Location))/nrow(pregnantNeverTested),1)


summary(as.factor(pregnantNeverTested$ATSIperinatal))
round(100*summary(as.factor(pregnantNeverTested$ATSIperinatal))/nrow(pregnantNeverTested),1)


