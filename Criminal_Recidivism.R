library(tidyverse)
library(corrplot)


# Missing Data
miss_pct <- map_dbl(subset, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

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

rec4 <- lm(recidivism ~ electronicMonitoring + mostSeriousCrime + age + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict)
coeftest(rec4, vcov. = vcovHC, type = "HC1")
# Variance inflation Factor 
# 1 = not correlated
# 1 - 5 = moderatelz correlated 
# 5 > highly correlated 
rec4 <- vif(lm(recidivism ~ electronicMonitoring + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict))

rec5 <- lm(recidivism ~ electronicMonitoring +judgeEverUsedEM + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = subset1)
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

# subset data for year 1998  and year 2007
rec1998 <- subset(subset1, yearOfImprisonment == "1998")

rec2007 <- subset(subset1, yearOfImprisonment == "2007")

# define the rec difference 

rec_diff <- rec1998$recidivism - rec2007$recidivism 

elect_diff <- rec1998$electronicMonitoring - rec2007$electronicMonitoring

percJudgeSentToEM_diff <- rec1998$percJudgeSentToEM - rec2007$percJudgeSentToEM

# estimate the models

rec_ivreg_diff1 <- ivreg(rec_diff ~ elect_diff | elect_diff + percJudgeSentToEM_diff) 
coeftest(rec_ivreg_diff1, vcov = vcovHC, type = "HC1")


# first stage regressions 

mod_relevance1 <- lm(elect_diff ~ percJudgeSentToEM_diff)


# check instrument relevance for model (1)

linearHypothesis(mod_relevance1, "percJudgeSentToEM_diff = 0", vcov = vcovHC, type = "HC1")


-------------------------------------------------------------------------------------------------------
# Total crimes by week days 
# Convert the Date variable to a format that R will recognize:
CR$Date = strptime(CR$date, format="%Y-%m-%d")

# Extract the hour and the day of the week:
CR$Weekday = weekdays(CR$Date)


# Create a simple line plot - need the total number of crimes on each day of the week. We can get this information by creating a table:
table(CR$Weekday)

# Save this table as a data frame:
WeekdayCounts = as.data.frame(table(CR$Weekday))

str(WeekdayCounts) 


# Load the ggplot2 library:
library(ggplot2)

# Create our plot
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))  

# Make the "Var1" variable an ORDERED factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday"))

# Try again:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

# Change our x and y labels:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the Week") + ylab("Total Crimes")

# Change our x and y labels:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total crimes") + theme(axis.title.y = element_blank()) 

-------------------------------------------------------------------------------------
attach(subset)
subset1 <- data.frame(electronicMonitoring, recidivism, numberPreviousImprisonments, age, year, homicideDummy, attemptedHomicideDummy, sexualOffensesDummy, otherSeriousCrimesDummy, aggravatedRobberyDummy, attemptedAggravatedRobberyDummy, robberyDummy, attemptedRobberyDummy, possessionOfFirearmsDummy, larcenyDummy, otherMinorCrimesDummy)
  
  
# CART Model
# Split the data
install.packages("caTools")
library(caTools)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

set.seed(3000)
spl = sample.split(subset1$recidivism, SplitRatio = 0.7)
Train = subset(subset1, spl==TRUE)
Test = subset(subset1, spl==FALSE)

# extract year 
subset$year <- gsub('([-][0-9]{2}[-][0-9]{2})$', '', subset$entryDate)
# CART model
Tree = rpart(recidivism ~ electronicMonitoring + numberPreviousImprisonments + age + year + homicideDummy + attemptedHomicideDummy + sexualOffensesDummy + otherSeriousCrimesDummy + aggravatedRobberyDummy + attemptedAggravatedRobberyDummy + robberyDummy + attemptedRobberyDummy + possessionOfFirearmsDummy + larcenyDummy + otherMinorCrimesDummy, 
             data = Train, 
             method="class", 
             minbucket=15)

prp(Tree)

# Make predictions
PredictCART = predict(Tree, newdata = Test, type = "class")
table(Test$recidivism, PredictCART)
(363+6)/(363+3+86+6)

# ROC curve
install.packages("ROCR")
library(ROCR)

PredictROC = predict(Tree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$recidivism)
perf = performance(pred, "tpr", "fpr")
plot(perf)

install.packages("randomForest")
library(randomForest)

# Convert outcome to factor
Train$recidivism = as.factor(Train$recidivism)
Test$recidivism = as.factor(Test$recidivism)

# Build random forest model
RF = randomForest(recidivism ~ electronicMonitoring + numberPreviousImprisonments + age + year + homicideDummy + attemptedHomicideDummy + sexualOffensesDummy + otherSeriousCrimesDummy + aggravatedRobberyDummy + attemptedAggravatedRobberyDummy + robberyDummy + attemptedRobberyDummy + possessionOfFirearmsDummy + larcenyDummy + otherMinorCrimesDummy, 
             data = Train, 
             ntree = 200, 
             nodesize = 15)


# Make predictions
PredictForest = predict(RF, newdata = Test)
table(Test$recidivism, PredictForest)
(364+7)/(364+2+85+7)


# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(recidivism ~ electronicMonitoring + numberPreviousImprisonments + age + year + homicideDummy + attemptedHomicideDummy + sexualOffensesDummy + otherSeriousCrimesDummy + aggravatedRobberyDummy + attemptedAggravatedRobberyDummy + robberyDummy + attemptedRobberyDummy + possessionOfFirearmsDummy + larcenyDummy + otherMinorCrimesDummy, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
TreeCV = rpart(recidivism ~ electronicMonitoring + numberPreviousImprisonments + age + year + homicideDummy + attemptedHomicideDummy + sexualOffensesDummy + otherSeriousCrimesDummy + aggravatedRobberyDummy + attemptedAggravatedRobberyDummy + robberyDummy + attemptedRobberyDummy + possessionOfFirearmsDummy + larcenyDummy + otherMinorCrimesDummy, data = Train, method="class", cp = 0.02)

# Make predictions
PredictCV = predict(TreeCV, newdata = Test, type = "class")
table(Test$recidivism, PredictCV)
(363+6)/(363+3+86+6)

#



mean <- aggregate(subset, list(subset$recidivism), mean)




