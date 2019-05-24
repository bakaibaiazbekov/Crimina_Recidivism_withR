library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(corrplot)
library(DT)

# Missing Data
miss_pct <- map_dbl(CR, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Correlation 
internal_chars <- c('electronicMonitoring', 'recidivism', 'mostSeriousCrime', 'age', 'ageSquared',  'argentine',  'numberPreviousImprisonments', 'yearOfImprisonment', 'judicialDistrict', 'percJudgeSentToEM', 'judgeAlreadyUsedEM')

corrplot(cor(CR[, ..internal_chars], use="complete.obs"))

library(AER)
attach(judge10)

# judge offender > 10 
judge10 <- subset(CR, offendersPerJudge > 9)

# perform Tabel 2
EM1 <- lm(electronicMonitoring ~ percJudgeSentToEM + judicialDistrict, data = judge10)

coeftest(EM1, vcov = vcovHC, type = "HC1")

EM2 <- lm(electronicMonitoring ~ percJudgeSentToEM + mostSeriousCrime + judicialDistrict, data = judge10)

coeftest(EM2, vcov = vcovHC, type = "HC1")

EM3 <- lm(electronicMonitoring ~ percJudgeSentToEM + mostSeriousCrime + numberPreviousImprisonments + judicialDistrict, data = judge10)

coeftest(EM3, vcov = vcovHC, type = "HC1")

EM4 <- lm(electronicMonitoring ~ percJudgeSentToEM + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment  + judicialDistrict, data = judge10)

coeftest(EM4, vcov = vcovHC, type = "HC1")

EM5 <- lm(electronicMonitoring ~ percJudgeSentToEM + judgeAlreadyUsedEM + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment  + judicialDistrict, data = judge10)

coeftest(EM5, vcov = vcovHC, type = "HC1")

# Table 3 
judgeEM <- lm(percJudgeSentToEM ~ lowHighEMRateDummy + judicialDistrict)
coeftest(judgeEM, vcov. = vcovHC, type = "HC1")

# Table 4
# omit NAs in recidivism
subset <-  CR[complete.cases(CR$recidivism), ] 

rec1 <- lm(recidivism ~ electronicMonitoring, data = subset)
coeftest(rec1, vcov. = vcovHC, type = "HC1")

rec2 <- lm(recidivism ~ electronicMonitoring + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = subset)
coeftest(rec2, vcov. = vcovHC, type = "HC1")

Subs1<-subset(CR, (!is.na(CR$recidivism)) & ((CR$mostSeriousCrime == "05 - Aggravated robbery")))

# estimate the simple probit model
rec3 <- glm(recidivism ~  electronicMonitoring + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, 
            family = binomial(link = "probit"), 
            data = subset)

coeftest(rec3, vcov. = vcovHC, type = "HC1")

rec4 <- lm(recidivism ~ electronicMonitoring + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = Subs1)
coeftest(rec4, vcov. = vcovHC, type = "HC1")

rec5 <- lm(recidivism ~ electronicMonitoring + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = Subs1)
coeftest(rec5, vcov. = vcovHC, type = "HC1")

# Table 5

# estimate the model
subset1 <-  judge10[complete.cases(judge10$recidivism), ] 

# perform the first stage regression
EM_s1 <- lm(electronicMonitoring ~ court, data = CR, na.action=na.exclude)

coeftest(EM_s1, vcov = vcovHC, type = "HC1")

# store the predicted values
EM1_pred <- EM_s1$fitted.values

# run the stage 2 regression
EM_s2 <- lm(subset1$recidivism ~ EM1_pred)
coeftest(EM_s2, vcov = vcovHC)

# inspect the R^2 of the first stage regression
summary(EM_s1)$r.squared

rec_ivreg1 <- ivreg(recidivism ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + electronicMonitoring | electronicMonitoring + court, data = subset1)

coeftest(rec_ivreg1, vcov = vcovHC, type = "HC1")

rec_ivreg2 <- ivreg(recidivism ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + electronicMonitoring | electronicMonitoring + percJudgeSentToEM, data = subset1)

coeftest(rec_ivreg2, vcov = vcovHC, type = "HC1")


# inspect the R^2 of the first stage regression
summary(rec_ivreg2)$r.squared

rec_ivreg3 <- ivreg(recidivism ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + electronicMonitoring | electronicMonitoring + percJudgeSentToEM + judgeAlreadyUsedEM, data = subset1)

coeftest(rec_ivreg3, vcov = vcovHC, type = "HC1")






