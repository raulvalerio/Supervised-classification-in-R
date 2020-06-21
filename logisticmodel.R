
### ----    DATA SOURCE  ---- ####

https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records


- age: age of the patient (years)
- anaemia: decrease of red blood cells or hemoglobin (boolean)
- high blood pressure: if the patient has hypertension (boolean)
- creatinine phosphokinase (CPK): level of the CPK enzyme in the blood (mcg/L)
- diabetes: if the patient has diabetes (boolean)
- ejection fraction: percentage of blood leaving the heart at each contraction (percentage)
- platelets: platelets in the blood (kiloplatelets/mL)
- sex: woman or man (binary)
- serum creatinine: level of serum creatinine in the blood (mg/dL)
- serum sodium: level of serum sodium in the blood (mEq/L)
- smoking: if the patient smokes or not (boolean)
- time: follow-up period (days)
- [target] death event: if the patient deceased during the follow-up period (boolean)

#### --------------------------------------- DATA ------

misdatos =  read.csv( "~/Canal Yout/Practica R/heart_failure_clinical_records_dataset.csv",header=TRUE, sep=',')


# ---- split test and train -------

set.seed(15)
#misdatos = misdatos[,-1]
n = nrow(misdatos)
d_ind = sample(n, n*0.70)   ## 209 out of 299

records = misdatos[d_ind,]
d_test = misdatos[-d_ind,]



########
View(records)
names(records)

attach(records)


## -------   data exploration and visualization--------

summary(records)

cor(records[c(1,3,5,7,8,9)]  )    ## no strong correlation


## first graph
boxplot(age ~ DEATH_EVENT, 
        main="Age distribution by survival",
        ylab="Age",
        col=c("blue", "red"),
        lty=1:2)

legend("topleft", legend=c("Survived", "Dead"), col=c("blue", "red"), lty=1:2)  #, cex=2

t.test(age[DEATH_EVENT==1],age[DEATH_EVENT==0])   # --->> difference

## second graph
## A normal platelet count ranges from 150,000 to 450,000
boxplot(platelets ~ DEATH_EVENT, 
        main="platelets distribution by patients",
        ylab="platelets",
        col=c("blue", "red"),
        lty=1:2)

legend("topleft", legend=c("Survived", "Dead"), col=c("blue", "red"), lty=1:2)  #, cex=2

t.test(platelets[DEATH_EVENT==1],platelets[DEATH_EVENT==0])   #-->>  no difference

##  third graph
##enzyme 60 and 400 IU/L, catalyses  the conversion of  creatine ( metabolize energy)

boxplot(creatinine_phosphokinase ~ DEATH_EVENT, 
        main="creatinine phosphokinase by patients with diabetes",
        ylab="creatinine_phosphokinase",
        col=c("blue", "red"),
        lty=1:2)

legend("topleft", legend=c("Survived", "Dead"), col=c("blue", "red"), lty=1:2)  #, cex=2

t.test(creatinine_phosphokinase[DEATH_EVENT==1],creatinine_phosphokinase[DEATH_EVENT==0])    ##  no difference

##  4th graph

boxplot(serum_creatinine ~ DEATH_EVENT, 
        main="serum_creatinine by patients with diabetes",
        ylab="serum_creatinine",
        col=c("blue", "red"),
        lty=1:2)

legend("topleft", legend=c("Survived", "Dead"), col=c("blue", "red"), lty=1:2)  #, cex=2

t.test(serum_creatinine[DEATH_EVENT==1],serum_creatinine[ DEATH_EVENT ==0])    ##  difference

##  4th graph

boxplot(serum_sodium ~ DEATH_EVENT, 
        main="serum_sodium by patients with diabetes",
        ylab="serum_sodium",
        col=c("blue", "red"),
        lty=1:2)

legend("topleft", legend=c("Survived", "Dead"), col=c("blue", "red"), lty=1:2)  #, cex=2

t.test(serum_sodium[DEATH_EVENT==1],serum_sodium[DEATH_EVENT==0])    ##  difference

## fifth graph

boxplot(ejection_fraction ~ DEATH_EVENT, 
        main="ejection fraction by patients with diabetes",
        ylab="ejection_fraction",
        col=c("blue", "red"),
        lty=1:2)

legend("topleft", legend=c("Survived", "Dead"), col=c("blue", "red"), lty=1:2)  #, cex=2

t.test(ejection_fraction[DEATH_EVENT==1], ejection_fraction[DEATH_EVENT==0])    ##  difference

##   time
boxplot(time ~ DEATH_EVENT, 
        main="follow up time by patients with diabetes",
        ylab="time(days)",
        col=c("blue", "red"),
        lty=1:2)

legend("topleft", legend=c("Survived", "Dead"), col=c("blue", "red"), lty=1:2)  #, cex=2

## ---- CATEGORICAL VARIABLES---------

library(dplyr)


# 0  women,  1 men
table(sex, DEATH_EVENT)

prop <- with(records, table(sex, DEATH_EVENT)) %>% prop.table()
prop


barplot( table(sex, DEATH_EVENT), main = "Comparison", 
         legend = c("Survived", "Dead"),
         xlab = " Sex", beside = TRUE)


table(anaemia, DEATH_EVENT)

prop <- with(records, table(anaemia, DEATH_EVENT)) %>% prop.table()
prop


table(high_blood_pressure, DEATH_EVENT)

prop <- with(records, table(high_blood_pressure, DEATH_EVENT)) %>% prop.table()
prop

table(smoking, DEATH_EVENT)

prop <- with(records, table(smoking, DEATH_EVENT)) %>% prop.table()
prop


barplot( table(smoking, DEATH_EVENT), main = "Comparison", 
         legend = c("Survived", "Dead"),
         xlab = " Smoking", beside = TRUE)

######  ----  MODEL WITH LOGISTIC REGRESSION---------------

my_model = glm(data =  records, DEATH_EVENT ~ . , family = binomial)

summary( my_model )

## Remark that  ejection, creatinine, and time have the smallest p-value so far

library(car)
vif(my_model)              ####--- no vif >5,  no multicollinearity

## delete  smoking, high blood pressure, 
my_model2 = glm(data =  records, DEATH_EVENT ~ age + diabetes + creatinine_phosphokinase  + anaemia+
                  + ejection_fraction+ platelets+ serum_creatinine + serum_sodium + sex + time, family = binomial)

summary( my_model2 )

# delete diabetes, platelets, 
my_model3 = glm(data =  records, DEATH_EVENT ~ age + creatinine_phosphokinase + ejection_fraction + 
                 anaemia + serum_creatinine + serum_sodium + sex + time, family = binomial)

summary( my_model3 )

#  delete anaemia, creatine_phospho

my_model4 = glm(data =  records, DEATH_EVENT ~ age + ejection_fraction +
                serum_creatinine + serum_sodium + sex + time, family = binomial)

summary( my_model4 )

#delete  age, serum_sodium
my_model5 = glm(data =  records, DEATH_EVENT ~ ejection_fraction + serum_creatinine + time + sex, family = binomial)

summary( my_model5 )

my_model5$aic

# ---->   delete sex

my_model6 = glm(data =  records, DEATH_EVENT ~ ejection_fraction + serum_creatinine + time, family = binomial)

summary( my_model6)

my_model6$aic    # 164.53

my_model$aic     # 167.37 , we have reduced AIC

### ---------------     the model fits well??-------------------

library(lmtest)

waldtest(my_model,my_model6)

#Wald test

#Model 1: DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes + 
#  ejection_fraction + high_blood_pressure + platelets + serum_creatinine + 
#  serum_sodium + sex + smoking + time
#Model 2: DEATH_EVENT ~ ejection_fraction + serum_creatinine + time
#Res.Df Df      F Pr(>F)
#1    196                 
#2    205 -9 1.4031 0.1889

## Ho: modelos iguales,  model1 can be replaced by model2

## model 2 has less deviance
# observar residual deviance vrs null deviance.  Es menor la residual deviance?  buen modelo

#R2pseudo = ( 262.21 - 156.54) /262.21 = 0.403      40.3% explicacion 

#  ( 262.21 - 141.37) /262.21 = 0.461

library(ResourceSelection)

#Ho: modelo ajusta bien
hoslem.test( DEATH_EVENT , fitted(my_model6))
## pvalue> 0.05,   buen modelo

coef(my_model6)
#(Intercept) ejection_fraction  serum_creatinine              time 
#3.60170690       -0.07872279        0.69528171       -0.02212950

exp(coef(my_model6))

#(Intercept) ejection_fraction  serum_creatinine              time 
#36.6607574         0.9242961         2.0042736         0.9781136 


y=predict(my_model6, type="response")

pred = ifelse(as.double(y)>0.5,1,0)

library(MLmetrics)

##  TRAINING SET

Accuracy(y_pred = pred, y_true = DEATH_EVENT)    #  we can use  my_model6$fitted.values

                ## compared to full model
pred2 = ifelse(my_model$fitted.values>0.5,1,0)
Accuracy(y_pred = pred2, y_true = DEATH_EVENT)

##-- other metrics

ConfusionMatrix(y_pred = pred, y_true =DEATH_EVENT)

AUC(y_pred = pred, y_true = DEATH_EVENT)   # area under curve 

MSE(y_pred = pred, y_true = DEATH_EVENT)   # area under curve 

###  TEST SET

y_t=predict(my_model6, d_test[c("ejection_fraction","serum_creatinine","time")])

pred_test = ifelse(as.double(y_t)>0.5,1,0)

Accuracy(y_pred = pred_test, y_true = d_test["DEATH_EVENT"])

ConfusionMatrix(y_pred = pred_test, y_true = unlist(d_test["DEATH_EVENT"] ) )

AUC(y_pred = pred_test, y_true = d_test["DEATH_EVENT"])

MSE(y_pred = pred_test, y_true = d_test["DEATH_EVENT"] )
