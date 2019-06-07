library(tidyverse)
library(corrplot)


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
attach(CR)
library(stargazer)

# Table 1
EM_1 <- subset(CR, (!is.na(CR$electronicMonitoring)) & ((CR$electronicMonitoring == "1")))
EM_11 <- EM_1 %>% select(homicideDummy, attemptedHomicideDummy, sexualOffensesDummy, otherSeriousCrimesDummy, aggravatedRobberyDummy, attemptedAggravatedRobberyDummy, robberyDummy, attemptedRobberyDummy, possessionOfFirearmsDummy, larcenyDummy, otherMinorCrimesDummy)
describe(EM_11)

EM_0 <- subset(CR, (!is.na(CR$electronicMonitoring)) & ((CR$electronicMonitoring == "0")))
EM_00 <- EM_0 %>% select(homicideDummy, attemptedHomicideDummy, sexualOffensesDummy, otherSeriousCrimesDummy, aggravatedRobberyDummy, attemptedAggravatedRobberyDummy, robberyDummy, attemptedRobberyDummy, possessionOfFirearmsDummy, larcenyDummy, otherMinorCrimesDummy)

latex(EM_11, title=NULL,
      file=paste('describe',first.word(expr=attr(EM_11,'descript')),'tex',sep='.'),
      append=FALSE, size='small', tabular=TRUE, greek=TRUE,
      spacing=0.7, lspace=c(0,0))

# judge offender > 10 
judge10 <- subset(CR, offendersPerJudge > 9)


# judge offender > 20 
judge20 <- subset(CR, offendersPerJudge > 19)

# judge offender > 30
judge30 <- subset(CR, offendersPerJudge > 29)


# perform Tabel 2
EM1 <- lm(electronicMonitoring ~ percJudgeSentToEM + judicialDistrict, data = judge10)

coeftest(EM1, vcov = vcovHC, type = "HC1")

EM2 <- lm(electronicMonitoring ~ percJudgeSentToEM + mostSeriousCrime + judicialDistrict, data = judge10)

coeftest(EM2, vcov = vcovHC, type = "HC1")

EM3 <- lm(electronicMonitoring ~ percJudgeSentToEM + mostSeriousCrime + numberPreviousImprisonments + judicialDistrict, data = judge10)

coeftest(EM3, vcov = vcovHC, type = "HC1")

# convert age in days to year 
judge10$age <- judge10$age / 365  
judge10$age <- round(judge10$age, 0)
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
subset20 <-  judge20[complete.cases(judge20$recidivism), ] 

subset30 <-  judge30[complete.cases(judge30$recidivism), ] 

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
rec3 <- glm(recidivism ~  electronicMonitoring + `_ImostSerio_2` + `_ImostSerio_3`+ `_ImostSerio_4`+ `_ImostSerio_5`+ `_ImostSerio_6`+ `_ImostSerio_7`+ `_ImostSerio_8`+ `_ImostSerio_9`+ `_ImostSerio_10`+ `_ImostSerio_11` + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment+`_IyearOfImp_1999` +  `_IyearOfImp_2000`+`_IyearOfImp_2001`+`_IyearOfImp_2002`+`_IyearOfImp_2003` +`_IyearOfImp_2004` +`_IyearOfImp_2005`+`_IyearOfImp_2006` + `_IyearOfImp_2007` , 
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
# 1 - 5 = moderately correlated 
# 5 > highly correlated 
rec4 <- vif(lm(recidivism ~ electronicMonitoring + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict))

# Column 5 
 df5 <- subset[!(subset$judicialDistrict == "AZUL") & (subset$yearOfImprisonment == "1998") & (subset$mostSeriousCrime == "01 - Homicide"), ]

df5 <- subset%>% filter(!(judicialDistrict != "NECOCHEA" & mostSeriousCrime != "01 - Homicide" & yearOfImprisonment != "1998"))

df5 <- subset[!(subset$judicialDistrict == "AZUL"),]
df5 <- df5[!(df5$yearOfImprisonment == "1998"),]
df5 <- df5[!(df5$mostSeriousCrime == "01 - Homicide"),]


rec5 <- lm(recidivism ~ electronicMonitoring +judgeEverUsedEM + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + `_IyearOfImp_1999` +  `_IyearOfImp_2000`+`_IyearOfImp_2001`+`_IyearOfImp_2002`+`_IyearOfImp_2003` +`_IyearOfImp_2004` +`_IyearOfImp_2005`+`_IyearOfImp_2006` + `_IyearOfImp_2007` , data = subset)
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
#subset1 <-  judge10[complete.cases(judge10$recidivism), ] 

# perform the first stage regression
EM_s1 <- lm(electronicMonitoring ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + court, data = subset30)

coeftest(EM_s1, vcov = vcovHC, type = "HC1")

# store the predicted values
EM1_pred <- EM_s1$fitted.values

# run the stage 2 regression
EM_s2 <- lm(recidivism ~ EM1_pred + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = subset30)
coeftest(EM_s2, vcov = vcovHC)
-0.11444 
# inspect the R^2 of the first stage regression
summary(EM_s1)$r.squared

# 1 stage column 2
EM_s12 <- lm(electronicMonitoring ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + percJudgeSentToEM, data = subset30)
coeftest(EM_s12, vcov = vcovHC, type = "HC1")

# store the predicted values
EM12_pred <- EM_s12$fitted.values

# run the stage 2 regression
EM_s22 <- lm(recidivism ~ EM12_pred + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = subset30)
coeftest(EM_s22, vcov = vcovHC)
-0.13841

# 1 stage 
#column 3
EM_s13 <- lm(electronicMonitoring ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + percJudgeSentToEM + judgeAlreadyUsedEM, data = subset30)
coeftest(EM_s13, vcov = vcovHC, type = "HC1")

# store the predicted values
EM13_pred <- EM_s13$fitted.values

# run the stage 2 regression
EM_s23 <- lm(recidivism ~ EM13_pred + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = subset30)
coeftest(EM_s23, vcov = vcovHC)
-0.15842

# Column 4
subset_em1 <-subset30[!(subset30$judicialDistrict == "NECOCHEA"),]

# first stage
EM_s14 <- glm(electronicMonitoring ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + percJudgeSentToEM + judgeAlreadyUsedEM, family = binomial(link = "probit"), data = subset_em1)

coeftest(EM_s14, vcov. = vcovHC, type = "HC1", cluster = "group")

# store the predicted values
EM14_pred <- EM_s14$fitted.values

# run the stage 2 regression
EM_s24 <- lm(recidivism ~ EM14_pred + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = subset_em1)
coeftest(EM_s24, vcov = vcovHC)
-0.17417

# 1 stage column 5
EM_s15 <- lm(electronicMonitoring ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = subset30)
coeftest(EM_s15, vcov = vcovHC, type = "HC1")

# store the predicted values
EM15_pred <- EM_s15$fitted.values

# run the stage 2 regression
EM_s25 <- lm(recidivism ~ EM13_pred + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = subset30)
coeftest(EM_s25, vcov = vcovHC)
-0.15842
-0.96417



# Column 4
attach(subset)
library(ivprobit)

year <- `_IyearOfImp_1999` +`_IyearOfImp_2000`+`_IyearOfImp_2001`+`_IyearOfImp_2002`+`_IyearOfImp_2003`+`_IyearOfImp_2004`+`_IyearOfImp_2005`+`_IyearOfImp_2006`+`_IyearOfImp_2007`
most_serious_crime <- `_ImostSerio_2` + `_ImostSerio_3` + `_ImostSerio_4` + `_ImostSerio_5` + `_ImostSerio_6` + `_ImostSerio_7` + `_ImostSerio_8` + `_ImostSerio_9` + `_ImostSerio_10` + `_ImostSerio_11` 
district <- `_IjudicialD_2` + `_IjudicialD_3` + `_IjudicialD_4` + `_IjudicialD_5` + `_IjudicialD_6` + `_IjudicialD_7` + `_IjudicialD_8` + `_IjudicialD_9` + `_IjudicialD_10` + `_IjudicialD_11` + `_IjudicialD_12` + `_IjudicialD_13` + `_IjudicialD_14` + `_IjudicialD_15` + `_IjudicialD_16` +`_IjudicialD_17` + `_IjudicialD_18` 


# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(EM_s2, type = "HC1"))),
               sqrt(diag(vcovHC(EM_s22, type = "HC1"))),
               sqrt(diag(vcovHC(EM_s23, type = "HC1"))),
               sqrt(diag(vcovHC(EM_s24, type = "HC1"))),
               sqrt(diag(vcovHC(EM_s25, type = "HC1"))))

# generate table
stargazer(EM_s2, EM_s22,EM_s23,EM_s24,EM_s25,
          header = FALSE, 
          type = "latex",
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("IV: court", "IV: percJudgeSentToEM", "IVs: percJudgeSentToEM, judgeAlreadyUsedEM","IV probit", "IV: largeSampleEstimate" ),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent Variable: recidivism",
          se = rob_se)

# Table 6 

# Column 1 
df61 <- as.data.frame(subset)
df61 <- df61[!is.na(df61$incomeProfession),]

df61$monthly_income <- df61$incomeProfession / 12 
df61$monthly_income <- round(df61$monthly_income, 2)
# 1 stage 
EM_s161 <- lm(electronicMonitoring ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict + monthly_income + percJudgeSentToEM + judgeAlreadyUsedEM, data = df61)
coeftest(EM_s161, vcov = vcovHC, type = "HC1")

# store the predicted values
EM161_pred <- EM_s161$fitted.values

# run the stage 2 regression
EM_s261 <- lm(recidivism ~ EM161_pred + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + monthly_income + judicialDistrict, data = df61)
coeftest(EM_s261, vcov = vcovHC)
-0.2434


df62 <- subset[!is.na(subset$spouse),]
# Column 2
EM_s162 <- lm(electronicMonitoring ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + spouse + familyVisits + judicialDistrict + percJudgeSentToEM + judgeAlreadyUsedEM, data = df62)
coeftest(EM_s162, vcov = vcovHC, type = "HC1")

# store the predicted values
EM162_pred <- EM_s162$fitted.values

# run the stage 2 regression
EM_s262 <- lm(recidivism ~ EM162_pred + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + spouse + familyVisits + judicialDistrict, data = df62)
coeftest(EM_s262, vcov = vcovHC)
-0.15063


# Column 3
EM_s163 <- lm(electronicMonitoring ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + totalDetentionLength + totalDetentionLengthSquared + yearOfImprisonment + judicialDistrict + percJudgeSentToEM + judgeAlreadyUsedEM, data = subset)
coeftest(EM_s163, vcov = vcovHC, type = "HC1")

# store the predicted values
EM163_pred <- EM_s163$fitted.values

# run the stage 2 regression
EM_s263 <- lm(recidivism ~ EM163_pred + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + totalDetentionLength + totalDetentionLengthSquared + yearOfImprisonment + judicialDistrict, data = subset)
coeftest(EM_s263, vcov = vcovHC)
-0.15372

df64 <- subset(subset6, offendersPerJudgeOnlyAggRobbery > 9)
subset6 <- subset[!is.na(subset),]
# Column 4
EM_s164 <- lm(electronicMonitoring ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment +  judicialDistrict + percJudgeSentToEMOnlyAggRobbery + judgeAlreadyUsedEMOnlyAggRobbery, data = df64)
coeftest(EM_s164, vcov = vcovHC, type = "HC1")

# store the predicted values
EM164_pred <- EM_s164$fitted.values

# run the stage 2 regression
EM_s264 <- lm(recidivism ~ EM164_pred + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + spouse + familyVisits + judicialDistrict, data = df62)
coeftest(EM_s264, vcov = vcovHC)


df65 <- subset(subset, offendersPerJudge > 9 & escapees == 0)
# Column 5 
EM_s165 <- lm(electronicMonitoring ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment +  judicialDistrict + percJudgeSentToEM + judgeAlreadyUsedEM, data = df65)
coeftest(EM_s165, vcov = vcovHC, type = "HC1")

# store the predicted values
EM165_pred <- EM_s165$fitted.values

# run the stage 2 regression
EM_s265 <- lm(recidivism ~ EM165_pred + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = df65)
coeftest(EM_s265, vcov = vcovHC)
-0.21045

# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(EM_s261, type = "HC1"))),
               sqrt(diag(vcovHC(EM_s262, type = "HC1"))),
               sqrt(diag(vcovHC(EM_s263, type = "HC1"))),
               sqrt(diag(vcovHC(EM_s265, type = "HC1"))))

# generate table
stargazer(EM_s261, EM_s262, EM_s263,EM_s265,
          header = FALSE, 
          type = "latex",
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("IVs: percJudgeSentToEM, judgeAlreadyUsedEM", "IVs: percJudgeSentToEM, judgeAlreadyUsedEM", "IVs: percJudgeSentToEM, judgeAlreadyUsedEM", "IVs: percJudgeSentToEM, judgeAlreadyUsedEM" ),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent Variable: recidivism",
          se = rob_se)


# Table 7 

# Column 1
df7<-subset(CR, (!is.na(CR$recidivism)) & ((CR$electronicMonitoring == "1")))

rec71 <- lm(recidivism ~ EMLengthTotalPrisonRatio + mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = df7)
coeftest(rec71, vcov = vcovHC, type = "HC1")
-0.08673

# Column 2
rec72 <- lm(recidivism ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = df7)
coeftest(rec72, vcov = vcovHC, type = "HC1")

# Column 3 
esc73 <- lm(escapees ~ mostSeriousCrime + age + ageSquared + argentine + numberPreviousImprisonments + yearOfImprisonment + judicialDistrict, data = df7)
coeftest(esc73, vcov = vcovHC, type = "HC1")

# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(rec71, type = "HC1"))),
               sqrt(diag(vcovHC(rec72, type = "HC1"))),
               sqrt(diag(vcovHC(esc73, type = "HC1"))))

# generate a LaTeX table using stargazer
stargazer(rec71, rec72, esc73,
          se = rob_se,
          digits = 3,
          header = F,
          column.labels = c("(I)", "(II)", "(III)"))

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
q <- ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total crimes") + theme(axis.title.y = element_blank() + coord_flip()) 

# q plot
q + coord_flip()
-------------------------------------------------------------------------------------
attach(subset)
subset1 <- data.frame(electronicMonitoring, recidivism, numberPreviousImprisonments, judgeEverUsedEM, age, familyVisits, escapees,  homicideDummy, attemptedHomicideDummy, sexualOffensesDummy, otherSeriousCrimesDummy, aggravatedRobberyDummy, attemptedAggravatedRobberyDummy, robberyDummy, attemptedRobberyDummy, possessionOfFirearmsDummy, larcenyDummy, otherMinorCrimesDummy)
subset1$age <- subset1$age / 365  
subset1$age <- round(subset1$age, 0)  
# extract year 
subset1$year <- gsub('([-][0-9]{2}[-][0-9]{2})$', '', subset$entryDate)

# CART Model
# Split the data
library(caTools)

# Install rpart library
library(rpart)
library(rpart.plot)

set.seed(3000)
spl = sample.split(subset1$recidivism, SplitRatio = 0.7)
Train = subset(subset1, spl==TRUE)
Test = subset(subset1, spl==FALSE)

# CART model
Tree = rpart(recidivism ~ electronicMonitoring + numberPreviousImprisonments + judgeEverUsedEM + age + year + familyVisits + escapees+ homicideDummy + attemptedHomicideDummy + sexualOffensesDummy + otherSeriousCrimesDummy + aggravatedRobberyDummy + attemptedAggravatedRobberyDummy + robberyDummy + attemptedRobberyDummy + possessionOfFirearmsDummy + larcenyDummy + otherMinorCrimesDummy, 
             data = Train, 
             method="class", 
             minbucket=20)

prp(Tree)

rpart.plot(Tree)
# Make predictions
PredictCART = predict(Tree, newdata = Test, type = "class")
table(Test$recidivism, PredictCART)
359 / (359+5+86)

# ROC curve
library(ROCR)

PredictROC = predict(Tree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$recidivism)
perf = performance(pred, "tpr", "fpr")
plot(perf)

library(randomForest)

# Convert outcome to factor
Train$recidivism = as.factor(Train$recidivism)
Test$recidivism = as.factor(Test$recidivism)

# Build random forest model
RF = randomForest(recidivism ~ electronicMonitoring + numberPreviousImprisonments + judgeEverUsedEM +age + year + homicideDummy + attemptedHomicideDummy + sexualOffensesDummy + otherSeriousCrimesDummy + aggravatedRobberyDummy + attemptedAggravatedRobberyDummy + robberyDummy + attemptedRobberyDummy + possessionOfFirearmsDummy + larcenyDummy + otherMinorCrimesDummy, 
             data = Train, 
             ntree = 200, 
             nodesize = 15)


# Make predictions
PredictForest = predict(RF, newdata = Test)
table(Test$recidivism, PredictForest)
(355 + 9) / (355+9+4+82)
[1] 0.8088889

# featue importants 
varImp(RF)

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(recidivism ~ electronicMonitoring + numberPreviousImprisonments + judgeEverUsedEM + age + year + homicideDummy + attemptedHomicideDummy + sexualOffensesDummy + otherSeriousCrimesDummy + aggravatedRobberyDummy + attemptedAggravatedRobberyDummy + robberyDummy + attemptedRobberyDummy + possessionOfFirearmsDummy + larcenyDummy + otherMinorCrimesDummy, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
TreeCV = rpart(recidivism ~ electronicMonitoring + numberPreviousImprisonments + judgeEverUsedEM + age + year + homicideDummy + attemptedHomicideDummy + sexualOffensesDummy + otherSeriousCrimesDummy + aggravatedRobberyDummy + attemptedAggravatedRobberyDummy + robberyDummy + attemptedRobberyDummy + possessionOfFirearmsDummy + larcenyDummy + otherMinorCrimesDummy, data = Train, method="class", cp = 0.02)

# Make predictions
PredictCV = predict(TreeCV, newdata = Test, type = "class")
table(Test$recidivism, PredictCV)
(354+6)/(354+5+85+6)
[1] 0.8

--------------------------------------------------------------------------------------------------

# Logistic regression Tree for EM == 1
subset12 <- EM_1 %>% select(recidivism, numberPreviousImprisonments, age, entryDate,yearOfImprisonment, familyVisits, escapees,homicideDummy, attemptedHomicideDummy, sexualOffensesDummy, otherSeriousCrimesDummy, aggravatedRobberyDummy, attemptedAggravatedRobberyDummy, robberyDummy, attemptedRobberyDummy, possessionOfFirearmsDummy, larcenyDummy, otherMinorCrimesDummy)
subset12$year <- gsub('([-][0-9]{2}[-][0-9]{2})$', '', subset12$entryDate)


# CART Model
# Split the data
library(caTools)

# Install rpart library
library(rpart)
library(rpart.plot)

set.seed(3000)
spl1 = sample.split(subset12$recidivism, SplitRatio = 0.7)
Train = subset(subset12, spl==TRUE)
Test = subset(subset12, spl==FALSE)

# CART model
Tree = rpart(recidivism ~  numberPreviousImprisonments + age + year + familyVisits + escapees+ homicideDummy + attemptedHomicideDummy + sexualOffensesDummy + otherSeriousCrimesDummy + aggravatedRobberyDummy + attemptedAggravatedRobberyDummy + robberyDummy + attemptedRobberyDummy + possessionOfFirearmsDummy + larcenyDummy + otherMinorCrimesDummy, 
             data = Train, 
             method="class", 
             minbucket=20)

prp(Tree)

# Make predictions
PredictCART = predict(Tree, newdata = Test, type = "class")
table(Test$recidivism, PredictCART)
359 / (359+5+86)

# ROC curve
library(ROCR)

PredictROC = predict(Tree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$recidivism)
perf = performance(pred, "tpr", "fpr")
plot(perf)

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
(357 + 4) / (357+4+2+87)
[1] 0.8022222

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

mean <- aggregate(subset, list(subset$recidivism), mean)




