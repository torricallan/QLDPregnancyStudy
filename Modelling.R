### Produces all models used in the final analysis given the final dataset

set.seed(123)


library(mice)

datalist <- readRDS("QLD_Study_Datasets_1.RDS")

people <- datalist[[1]]
tested <- datalist[[2]]
cohort1People <- datalist[[3]]
pregnant <- datalist[[4]]
forImputation <- datalist[[5]]
imp <- datalist[[6]]


### Pregnancy Ever Model by Testing

pregnantEver.testingEver <- glm(pregnantEver ~ testedEver
                                , data = people
                                , family = binomial(link='logit'))

summary(pregnantEver.testingEver)

testedEver.oddsRatios <- exp(cbind(coef(pregnantEver.testingEver), confint(pregnantEver.testingEver)))

### Pregnancy Ever Model by Testing controlled for Location

pregnantEver.testingEverLocation <- glm(pregnantEver ~ testedEver + AgeGrouped + Location
                                        , data = people
                                        , family = binomial(link='logit'))

summary(pregnantEver.testingEverLocation)

testedEverLocation.oddsRatios <- exp(cbind(coef(pregnantEver.testingEverLocation), confint(pregnantEver.testingEverLocation)))


pregnant$ATSIperinatal <- as.factor(ifelse(pregnant$Mother_indig_status == 'Indigenous and/or Torres Stait Islander', 'ATSI', 'nonATSI'))
pregnant$ATSIperinatal <- relevel(pregnant$ATSIperinatal, ref = "nonATSI")
### ART Model by Testing Controlled for Location

ART.testingEverLocation <- glm(ART ~ testedEver + AgeGrouped + Location + ATSIperinatal
                               , data = pregnant
                               , family = binomial(link='logit'))

summary(ART.testingEverLocation)

testedEverLocationART.oddsRatios <- exp(cbind(coef(ART.testingEverLocation), confint(ART.testingEverLocation)))

ART.testingTypeLocation <- glm(ART ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                               , data = pregnant
                               , family = binomial(link='logit'))

summary(ART.testingTypeLocation)

testedTypeLocationART.oddsRatios <- exp(cbind(coef(ART.testingTypeLocation), confint(ART.testingTypeLocation)))




### Pre-term birth (below 37 weeks)
preTermBirth.testingEverLocation <- glm(preTermBirth ~ testedEver + AgeGrouped + Location + ATSIperinatal
                                        , data = pregnant
                                        , family = binomial(link='logit'))
summary(preTermBirth.testingEverLocation)

testedEverLocationpreTermBirth.oddsRatios <- exp(cbind(coef(preTermBirth.testingEverLocation), confint(preTermBirth.testingEverLocation)))

preTermBirth.testingTypeLocation <- glm(preTermBirth ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                                        , data = pregnant
                                        , family = binomial(link='logit'))
summary(preTermBirth.testingTypeLocation)

testingTypeLocationpreTermBirth.oddsRatios <- exp(cbind(coef(preTermBirth.testingTypeLocation), confint(preTermBirth.testingTypeLocation)))


### Low birth weight (below 2500g)
lowBirthWeight.testingEverLocation <- glm(lowBirthWeight ~ testedEver + AgeGrouped + Location + ATSIperinatal
                                          , data = pregnant
                                          , family = binomial(link='logit'))
summary(lowBirthWeight.testingEverLocation)

testedEverLocationLowBirthWeight.oddsRatios <- exp(cbind(coef(lowBirthWeight.testingEverLocation), confint(lowBirthWeight.testingEverLocation)))

lowBirthWeight.testingTypeLocation <- glm(lowBirthWeight ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                                          , data = pregnant
                                          , family = binomial(link='logit'))
summary(lowBirthWeight.testingTypeLocation)

testingCohortLocationLowBirthWeight.oddsRatios <- exp(cbind(coef(lowBirthWeight.testingTypeLocation), confint(lowBirthWeight.testingTypeLocation)))



### Stillbirths
stillborn.testingEverLocation <- glm(stillBorn ~ testedEver + AgeGrouped + Location + ATSIperinatal
                                     , data = pregnant
                                     , family = binomial(link='logit'))

summary(stillborn.testingEverLocation)

testedEverLocationStillBorn.oddsRatios <- exp(cbind(coef(stillborn.testingEverLocation), confint(stillborn.testingEverLocation)))

stillborn.testingTypeLocation <- glm(stillBorn ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                                     , data = pregnant
                                     , family = binomial(link='logit'))

summary(stillborn.testingTypeLocation)

testingTypeLocationStillBorn.oddsRatios <- exp(cbind(coef(stillborn.testingTypeLocation), confint(stillborn.testingTypeLocation)))



### Coefficient Plot for tested ever models
 
pregnantEverCrude.est <- 1/exp(summary(pregnantEver.testingEver)$coef[2,1])
pregnantEverAdjusted.est <- 1/exp(summary(pregnantEver.testingEverLocation)$coef[2,1])
ART.est <- exp(summary(ART.testingEverLocation)$coef[2,1])
Stillborn.est <- exp(summary(stillborn.testingEverLocation)$coef[2,1])
lowBirthWeight.est <- exp(summary(lowBirthWeight.testingEverLocation)$coef[2,1])
preTermBirth.est <- exp(summary(preTermBirth.testingEverLocation)$coef[2,1])

pregnantEverCrude.se <- summary(pregnantEver.testingEver)$coef[2,2]
pregnantEverAdjusted.se <- summary(pregnantEver.testingEverLocation)$coef[2,2]
ART.se <- summary(ART.testingEverLocation)$coef[2,2]
Stillborn.se <- summary(stillborn.testingEverLocation)$coef[2,2]
lowBirthWeight.se <- summary(lowBirthWeight.testingEverLocation)$coef[2,2]
preTermBirth.se <- summary(preTermBirth.testingEverLocation)$coef[2,2]

estimates <- data.frame(modelName = c('Pregnant Ever (Crude)', 'Pregnant Ever (Adjusted)', 'ART', 'Stillborn', 'Low Birth Weight', 'Pre Term Birth')
                        , testedEverEffect = c(pregnantEverCrude.est, pregnantEverAdjusted.est, ART.est, Stillborn.est, lowBirthWeight.est, preTermBirth.est)
                        , testedEverSE = c(pregnantEverCrude.se, pregnantEverAdjusted.se, ART.se, Stillborn.se, lowBirthWeight.se, preTermBirth.se)
                        , modelType = c("Pregnancy Ever", "Pregnancy Ever", "Perinatal", "Perinatal", "Perinatal", "Perinatal"))

ggplot(estimates, aes(modelName, testedEverEffect, col=modelType)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = exp(log(testedEverEffect) + 1.96*testedEverSE), ymin = exp(log(testedEverEffect) - 1.96*testedEverSE))) + 
  scale_color_manual(values=c("black", "red")) +
  xlab("Outcome Name") + ylab("Tested Ever Effect (Odds Ratio with 95% CI") + 
  ggtitle("Comparison of tested ever effect across outcome measures")
  
### ART Model by Testing Controlled for Location by CT and NG status
pregnantCT <- subset(pregnant, infectionType != "NG")

ART.CTgroup.testedEver <- glm(ART ~ testedEver + AgeGrouped + Location + ATSIperinatal
                              , data = pregnantCT
                              , family = binomial(link='logit'))

summary(ART.CTgroup.testedEver)

ART.CTgroup.testedEver.oddsRatios <- exp(cbind(coef(ART.CTgroup.testedEver), confint(ART.CTgroup.testedEver)))


ART.CTgroup.testingCohort <- glm(ART ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                                 , data = pregnantCT
                                 , family = binomial(link='logit'))

summary(ART.CTgroup.testingCohort)

ART.CTgroup.oddsRatios <- exp(cbind(coef(ART.CTgroup.testingCohort), confint(ART.CTgroup.testingCohort)))

### ART for NG group

pregnantNG <- subset(pregnant, infectionType != "CT")


ART.NGgroup.testedEver <- glm(ART ~ testedEver + AgeGrouped + Location + ATSIperinatal
                              , data = pregnantNG
                              , family = binomial(link='logit'))

summary(ART.NGgroup.testedEver)

ART.NGgroup.testedEver.oddsRatios <- exp(cbind(coef(ART.NGgroup.testedEver), confint(ART.NGgroup.testedEver)))


ART.NGgroup.testingCohort <- glm(ART ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                                 , data = pregnantNG
                                 , family = binomial(link='logit'))

summary(ART.NGgroup.testingCohort)

ART.NGgroup.oddsRatios <- exp(cbind(coef(ART.NGgroup.testingCohort), confint(ART.NGgroup.testingCohort)))


### Perinatal Outcomes CT

stillborn.CTgroup <- glm(stillBorn ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                         , data = pregnantCT
                         , family = binomial(link='logit'))

summary(stillborn.CTgroup)

stillBorn.CTgroup.oddsRatios <- exp(cbind(coef(stillborn.CTgroup), confint(stillborn.CTgroup)))


preTermBirth.CTgroup <- glm(preTermBirth ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                            , data = pregnantCT
                            , family = binomial(link='logit'))
summary(preTermBirth.CTgroup)

preTermBirth.CTgroup.oddsRatios <- exp(cbind(coef(preTermBirth.CTgroup), confint(preTermBirth.CTgroup)))

lowBirthWeight.CTgroup <- glm(lowBirthWeight ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                              , data = pregnantCT
                              , family = binomial(link='logit'))
summary(lowBirthWeight.CTgroup)

lowBirthWeight.CTgroup.oddsRatios <- exp(cbind(coef(lowBirthWeight.CTgroup), confint(lowBirthWeight.CTgroup)))


### Perinatal Outcomes NG

stillborn.NGgroup <- glm(stillBorn ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                         , data = pregnantNG
                         , family = binomial(link='logit'))

summary(stillborn.NGgroup)

stillBorn.NGgroup.oddsRatios <- exp(cbind(coef(stillborn.NGgroup), confint(stillborn.NGgroup)))


preTermBirth.NGgroup <- glm(preTermBirth ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                            , data = pregnantNG
                            , family = binomial(link='logit'))
summary(preTermBirth.NGgroup)

preTermBirth.NGgroup.oddsRatios <- exp(cbind(coef(preTermBirth.NGgroup), confint(preTermBirth.NGgroup)))

lowBirthWeight.NGgroup <- glm(lowBirthWeight ~ testingCohort + AgeGrouped + Location + ATSIperinatal
                              , data = pregnantNG
                              , family = binomial(link='logit'))
summary(lowBirthWeight.NGgroup)

lowBirthWeight.NGgroup.oddsRatios <- exp(cbind(coef(lowBirthWeight.NGgroup), confint(lowBirthWeight.NGgroup)))



### Pregnancy Ever by Cohort
pregnantEver.cohort <- glm(pregnantEver ~ COHORT + Age #+ Location
                           , data=people
                           , family=binomial(link='logit'))

summary(pregnantEver.cohort)

cohort.oddsRatios <- exp(cbind(coef(pregnantEver.cohort), confint(pregnantEver.cohort)))

### Pregnancy Ever by Testing Type
                                                                                                              
pregnantEver.testingType <- glm(pregnantEver ~ testingCohort + AgeGrouped + Location
                                , data = people
                                , family = binomial(link='logit'))
summary(pregnantEver.testingType)

testingType.oddsRatios <- exp(cbind(coef(pregnantEver.testingType), confint(pregnantEver.testingType)))

### Pregnancy Ever by Testing Type for Chlamydia Only
peopleCT <- people[people$infectionType != "NG",]


pregnantEver.testingTypeCT <- glm(pregnantEver ~ testingCohort + AgeGrouped + Location
                                , data = peopleCT
                                , family = binomial(link='logit'))
summary(pregnantEver.testingTypeCT)

testingTypeCT.oddsRatios <- exp(cbind(coef(pregnantEver.testingTypeCT), confint(pregnantEver.testingTypeCT)))

### Pregnancy Ever by Testing Type for Gonorrhoea Only
peopleNG <- people[people$infectionType != "CT",]


pregnantEver.testingTypeNG <- glm(pregnantEver ~ testingCohort + AgeGrouped + Location
                                  , data = peopleNG
                                  , family = binomial(link='logit'))
summary(pregnantEver.testingTypeNG)

testingTypeNG.oddsRatios <- exp(cbind(coef(pregnantEver.testingTypeNG), confint(pregnantEver.testingTypeNG)))


### Pregnancy Ever by Testing Type on Tested Cohort
pregnantEver.testingTypeAndEverTested <- glm(pregnantEver ~ testingCohort + AgeGrouped + Location
                                , data = tested
                                , family = binomial(link='logit'))

summary(pregnantEver.testingTypeAndEverTested)
testingTypeAndEverTested.oddsRatios <- exp(cbind(coef(pregnantEver.testingTypeAndEverTested), confint(pregnantEver.testingTypeAndEverTested)))

1 - pchisq(sum(residuals(pregnantEver.testingTypeAndEverTested, type = "pearson")^2), df.residual(pregnantEver.testingTypeAndEverTested))

### Pregnancy Ever by Testing Type and ATSI at Testing
pregnantEver.ATSItesting <- glm(pregnantEver ~ testingCohort + ATSItesting + AgeGrouped + Location, data=tested, family=binomial(link='logit'))
summary(pregnantEver.ATSItesting)

ATSItesting.oddsRatio <- exp(cbind(coef(pregnantEver.ATSItesting), confint(pregnantEver.ATSItesting)))

1 - pchisq(sum(residuals(pregnantEver.ATSItesting, type = "pearson")^2), df.residual(pregnantEver.ATSItesting))

### Pregnancy Ever by Testing Type with Imputed ATSI
fit.main <- with(imp, exp=glm(pregnantEver ~ testingCohort + ATSIimputed + AgeGrouped + Location, family=binomial(link='logit')))
pregnantEver.ATSImain <- pool(fit.main)
summary(pregnantEver.ATSImain)

coef <- summary(pregnantEver.ATSImain)[,1]
se <- summary(pregnantEver.ATSImain)[,2]

ATSImain.oddsRatios <- exp(cbind(coef, coef-1.96*se, coef+1.96*se))
rownames(ATSImain.oddsRatios) <- rownames(summary(pregnantEver.ATSImain))

### Indigenous status and CT/NG difference
imputed_df <- mice::complete(imp, include = TRUE)

imputed_NG <- subset(imputed_df, infectionStatusGonoccocal == 1 | testingCohort == 'negativeOnly')
imputed_CT <- subset(imputed_df, infectionStatusGonoccocal == 0)

fit.imputedNG <- with(imputed_NG, exp=glm(pregnantEver ~ testingCohort + ATSIimputed + AgeGrouped + Location, family=binomial(link='logit')))
summary(fit.imputedNG)
imputedNG.oddsRatios <- exp(cbind(coef(fit.imputedNG), confint(fit.imputedNG)))


fit.imputedCT <- with(imputed_CT, exp=glm(pregnantEver ~ testingCohort + ATSIimputed + AgeGrouped + Location, family=binomial(link='logit')))
summary(fit.imputedCT)
imputedCT.oddsRatios <- exp(cbind(coef(fit.imputedCT), confint(fit.imputedCT)))

