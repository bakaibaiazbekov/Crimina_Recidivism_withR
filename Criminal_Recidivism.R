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
library(stargazer)

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

# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(EM1, type = "HC1"))),
               sqrt(diag(vcovHC(EM2, type = "HC1"))),
               sqrt(diag(vcovHC(EM3, type = "HC1"))),
               sqrt(diag(vcovHC(EM4, type = "HC1"))),
               sqrt(diag(vcovHC(EM5, type = "HC1"))))

# generate a LaTeX table using stargazer
stargazer(EM1, EM2, EM3, EM4, EM5,
          se = rob_se,
          digits = 3,
          header = F,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)"))


# Table 3 
#column 1
highEM <- subset(judge10, (!is.na(judge10$lowHighEMRateDummy)) & ((judge10$lowHighEMRateDummy == "1")))
summary(lowEM$age)

lowEM <- subset(judge10,(!is.na(judge10$lowHighEMRateDummy)) & ((judge10$lowHighEMRateDummy == "0")))
summary(highEM$age)


# Table 4
# omit NAs in recidivism
subset <-  judge10[complete.cases(judge10$recidivism), ] 
attach(subset)
# column 1
rec1 <- lm(recidivism ~ electronicMonitoring, data = subset)
coeftest(rec1, vcov. = vcovHC, type = "HC1", cluster = "group")
# column 2
rec2 <- lm(recidivism ~ electronicMonitoring + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment +`_IyearOfImp_1999` +  `_IyearOfImp_2000`+`_IyearOfImp_2001`+`_IyearOfImp_2002`+`_IyearOfImp_2003` +`_IyearOfImp_2004` +`_IyearOfImp_2005`+`_IyearOfImp_2006` + `_IyearOfImp_2007`+ judicialDistrict, data = subset)
coeftest(rec2, vcov. = vcovHC, type = "HC1", cluster = "group")
# column 3
subset2 <-  CR[complete.cases(CR$recidivism), ] 

df3 <- subset2[!(subset2$judicialDistrict == "NECOCHEA"),]
# estimate the simple probit model
rec3 <- glm(recidivism ~  electronicMonitoring + `_ImostSerio_2` + `_ImostSerio_3`+ `_ImostSerio_4`+ `_ImostSerio_5`+ `_ImostSerio_6`+ `_ImostSerio_7`+ `_ImostSerio_8`+ `_ImostSerio_9`+ `_ImostSerio_10`+ `_ImostSerio_11` + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment+`_IyearOfImp_1999` +  `_IyearOfImp_2000`+`_IyearOfImp_2001`+`_IyearOfImp_2002`+`_IyearOfImp_2003` +`_IyearOfImp_2004` +`_IyearOfImp_2005`+`_IyearOfImp_2006` + `_IyearOfImp_2007` + `_IjudicialD_2`+ `_IjudicialD_3`+ `_IjudicialD_4`+ `_IjudicialD_5`+ `_IjudicialD_6`+ `_IjudicialD_7`+ `_IjudicialD_8`+ `_IjudicialD_9`+ `_IjudicialD_10`+ `_IjudicialD_11`+ `_IjudicialD_12`+ `_IjudicialD_13`+ `_IjudicialD_14`+ `_IjudicialD_15`+ `_IjudicialD_16`+ `_IjudicialD_17` + `_IjudicialD_18` , 
            family = binomial(link = "probit"), 
            data = df3)

coeftest(rec3, vcov. = vcovHC, type = "HC1", cluster = "group")

mfxboot <- function(modform,dist,data,boot=1000,digits=3){
  x <- glm(modform, family=binomial(link=dist),data)
  # get marginal effects
  pdf <- ifelse(dist=="probit",
                mean(dnorm(predict(x, type = "link"))),
                mean(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf*coef(x)
  # start bootstrap
  bootvals <- matrix(rep(NA,boot*length(coef(x))), nrow=boot)
  set.seed(1111)
  for(i in 1:boot){
    samp1 <- data[sample(1:dim(data)[1],replace=T,dim(data)[1]),]
    x1 <- glm(modform, family=binomial(link=dist),samp1)
    pdf1 <- ifelse(dist=="probit",
                   mean(dnorm(predict(x1, type = "link"))),
                   mean(dlogis(predict(x1, type = "link"))))
    bootvals[i,] <- pdf1*coef(x1)
  }
  res <- cbind(marginal.effects,apply(bootvals,2,sd),marginal.effects/apply(bootvals,2,sd))
  if(names(x$coefficients[1])=="(Intercept)"){
    res1 <- res[2:nrow(res),]
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res1)),nrow=dim(res1)[1])     
    rownames(res2) <- rownames(res1)
  } else {
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep="")),nrow=dim(res)[1]))
    rownames(res2) <- rownames(res)
  }
  colnames(res2) <- c("marginal.effect","standard.error","z.ratio")  
  return(res2)
}
mfx1 <- mfxboot(modform,"probit",df3)
summary(mfx1)

# Column 4

Subs1<-subset(CR, (!is.na(CR$recidivism)) & ((CR$mostSeriousCrime == "05 - Aggravated robbery")))

rec4 <- lm(recidivism ~ electronicMonitoring + mostSeriousCrime + age + argentine + numberPreviousImprisonments + yearOfImprisonment + `_IyearOfImp_1999` +  `_IyearOfImp_2000`+`_IyearOfImp_2001`+`_IyearOfImp_2002`+`_IyearOfImp_2003` +`_IyearOfImp_2004` +`_IyearOfImp_2005`+`_IyearOfImp_2006` + `_IyearOfImp_2007` + judicialDistrict)
coeftest(rec4, vcov. = vcovHC, type = "HC1")
# Variance inflation Factor 
# 1 = not correlated
# 1 - 5 = moderatelz correlated 
# 5 > highly correlated 
rec4 <- vif(lm(recidivism ~ electronicMonitoring + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict))

# Column 5 
 df5 <- subset[!(subset$judicialDistrict == "NECOCHEA") & (subset$yearOfImprisonment == "1998") & (subset$mostSeriousCrime == "01 - Homicide"), ]

df5 <- subset%>% filter(!(judicialDistrict != "NECOCHEA" & mostSeriousCrime != "01 - Homicide" & yearOfImprisonment != "1998"))

df5 <- subset[!(subset$judicialDistrict == "AZUL"),]
df5 <- df5[!(df5$yearOfImprisonment == "1998"),]

df5 <- df5[!(df5$mostSeriousCrime == "01 - Homicide"),]

rec5 <- lm(recidivism ~ electronicMonitoring +judgeEverUsedEM + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + `_IyearOfImp_1999` +  `_IyearOfImp_2000`+`_IyearOfImp_2001`+`_IyearOfImp_2002`+`_IyearOfImp_2003` +`_IyearOfImp_2004` +`_IyearOfImp_2005`+`_IyearOfImp_2006` + `_IyearOfImp_2007` + `_IjudicialD_2`+ `_IjudicialD_3`+ `_IjudicialD_4`+ `_IjudicialD_5`+ `_IjudicialD_6`+ `_IjudicialD_7`+ `_IjudicialD_8`+ `_IjudicialD_9`+ `_IjudicialD_10`+ `_IjudicialD_11`+ `_IjudicialD_12`+ `_IjudicialD_13`+ `_IjudicialD_14`+ `_IjudicialD_15`+ `_IjudicialD_16`+ `_IjudicialD_17` + `_IjudicialD_18` , data = subset)
coeftest(rec5, vcov. = vcovHC, type = "HC1", cluster = judicialDistrict)

# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(rec1, type = "HC1"))),
               sqrt(diag(vcovHC(rec2, type = "HC1"))),
               sqrt(diag(vcovHC(rec3, type = "HC1"))),
               sqrt(diag(vcovHC(rec4, type = "HC1"))),
               sqrt(diag(vcovHC(rec5, type = "HC1"))))

# generate a LaTeX table using stargazer
stargazer(rec1, rec2, rec3, rec4, rec5,
          se = rob_se,
          digits = 3,
          header = F,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)"))




# Table 5

# estimate the model
subset1 <-  judge10[complete.cases(judge10$recidivism), ] 

# perform the first stage regression
EM_s1 <- lm(electronicMonitoring ~ court, data = subset)

coeftest(EM_s1, vcov = vcovHC, type = "HC1")

# store the predicted values
EM1_pred <- EM_s1$fitted.values

# run the stage 2 regression
EM_s2 <- lm(subset1$recidivism ~ EM1_pred)
coeftest(EM_s2, vcov = vcovHC)

# inspect the R^2 of the first stage regression
summary(EM_s1)$r.squared

# IV reg column 1
rec_ivreg1 <- ivreg(recidivism ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + electronicMonitoring |electronicMonitoring + court, data = subset)

coeftest(rec_ivreg1, vcov = vcovHC, type = "HC1")

rec_ivreg2 <- ivreg(recidivism ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + electronicMonitoring | electronicMonitoring + percJudgeSentToEM, data = subset)

coeftest(rec_ivreg2, vcov = vcovHC, type = "HC1")


# inspect the R^2 of the first stage regression
summary(rec_ivreg2)$r.squared

rec_ivreg3 <- ivreg(recidivism ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + electronicMonitoring | electronicMonitoring + percJudgeSentToEM + judgeAlreadyUsedEM, data = subset1)

coeftest(rec_ivreg3, vcov = vcovHC, type = "HC1")


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




