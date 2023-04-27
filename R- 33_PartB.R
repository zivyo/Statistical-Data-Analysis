
library(ggplot2)
library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
#install.packages("dplyr") 
library("dplyr")
library(lazyeval)
#install.packages("car")
library("car")
library(parallel)
library(e1071)
library(plotly)
library(gplots)
library(ggplot2)
library(triangle)
library(sqldf)
library(simmer)
library(simmer.plot)
library(epiDisplay)
#install.packages("ggpubr")
#install.packages("strucchange")
#install.packages("lmtest")
library(strucchange)
library(lmtest)
library("ggpubr")
install.packages("SciViews")
library("SciViews")

# Loading the data
table<-read.csv(file.choose(),header = T)

summary(table)
cor(table$faculty_salary, table$demographics_age_entry)

cov(table$faculty_salary)



#תיאור קשרים בין משתנים# 

#הפיכת משתנים קטגוריאליים לרציפים
table$highest_degrees_Number<-ifelse(table$highest_degrees_awarded=="Certificate degree",2,
                                     ifelse(table$highest_degrees_awarded=="Associate degree",3,
                                            ifelse(table$highest_degrees_awarded=="Bachelor's degree",4,
                                                   ifelse(table$highest_degrees_awarded=="Non-degree-granting",1,5))))

table$online_binarey<-ifelse(table$online_only=="Not distance-education only",1,0)

table$ownership_Number<-ifelse(table$ownership=="Private nonprofit",1,
                               ifelse(table$ownership=="Public",2,3))
dataset <- table[c(3,4,6,7,9:13)]

#Plots Creating

fit<-plot(table$demographics_age_entry, table$highest_degrees_Number,xlab="Entry Age",ylab = "Highest Degree Given", col='orange', pch=1)
abline(fit)

fit<-plot(table$parents_highschool, table$highest_degrees_Number,xlab="Parents Education",ylab = "Highest Degree Given", col='orange', pch=1)
abline(fit)
cor(table$parents_highschool, table$highest_degrees_Number)

fit<-plot(table$demographics_age_entry, table$ft_faculty_rate,xlab="Entry Age",ylab = "Full-Time Job Rate", col='orange', pch=1)
abline(fit)
cor(table$ft_faculty_rate, table$demographics_age_entry)

fit<-plot(table$faculty_salary, table$ownership_Number,xlab="Staff Salaries",ylab = "Ownership", col='orange', pch=1)
abline(fit)
cor(table$faculty_salary, table$ownership_Number)

fit<-plot(table$demographics_female_share, table$faculty_salary,xlab="Female Student Rate",ylab = "Staff Salaries", col='orange', pch=1)
abline(fit)
cor(table$demographics_female_share, table$faculty_salary)





#ניתוח תיאורי של המשתנים#


#משתנים רציפים
#faculty_salary
summary(table$faculty_salary)
skewness(table$faculty_salary)
sd(table$faculty_salary)

#demographics_age_entry
summary(table$demographics_age_entry)
skewness(table$demographics_age_entry)
sd(table$demographics_age_entry)

#ft_faculty_rate
summary(table$ft_faculty_rate)
skewness(table$ft_faculty_rate)
sd(table$ft_faculty_rate)

#demographics_female_share
summary(table$demographics_female_share)
skewness(table$demographics_female_share)
sd(table$demographics_female_share)

#parents_highschool
summary(table$parents_highschool)
skewness(table$parents_highschool)
sd(table$parents_highschool)

#income
summary(table$income)
skewness(table$income)
sd(table$income)


#משתנים קטגוריאליים

#highest_degrees_Number==1
data1 <- subset(table,highest_degrees_Number==1)
summary(data1$income)
skewness(data1$income)
sd(data1$income)

#highest_degrees_Number==2
data2 <- subset(table,highest_degrees_Number==2)
summary(data2$income)
skewness(data2$income)
sd(data2$income)

#highest_degrees_Number==3
data3 <- subset(table,highest_degrees_Number==3)
summary(data3$income)
skewness(data3$income)
sd(data3$income)

#highest_degrees_Number==4
data4 <- subset(table,highest_degrees_Number==4)
summary(data4$income)
skewness(data4$income)
sd(data4$income)

#highest_degrees_Number==5
data5 <- subset(table,highest_degrees_Number==5)
summary(data5$income)
skewness(data5$income)
sd(data5$income)


#online_binarey==1
data6 <- subset(table,online_binarey==1)
summary(data6$income)
skewness(data6$income)
sd(data6$income)

#online_binarey==0
data7 <- subset(table,online_binarey==0)
summary(data7$income)
skewness(data7$income)
sd(data7$income)


#ownership_Number==1
data8 <- subset(table,ownership_Number==1)
summary(data8$income)
skewness(data8$income)
sd(data8$income)

#ownership_Number==2
data9 <- subset(table,ownership_Number==2)
summary(data9$income)
skewness(data9$income)
sd(data9$income)

#ownership_Number==3
data10 <- subset(table,ownership_Number==3)
summary(data10$income)
skewness(data10$income)
sd(data10$income)


# ניתוח חריגים#

#שכר סגל ממוצע
bp1<-boxplot(table$faculty_salary, main='faculty_salary', outline=T)
new_Salary <- subset(table,table$faculty_salary<11000)
boxplot(new_Salary$faculty_salary, main='faculty_salary', outline=T)
#שיעור הסגל במשרה מלאה
bp2<-boxplot(table$ft_faculty_rate, main='ft_faculty_rate', outline=T)

#גיל כניסה ממוצע
bp3<-boxplot(table$demographics_age_entry, main='demographics_age_entry', outline=T)

#אחוז סטודנטיות
bp4<-boxplot(table$demographics_female_share, main='demographics_female_share', outline=T)
female <- subset(table,table$demographics_female_share>0.35)
female <- subset(female,female$demographics_female_share<0.86)
boxplot(female$demographics_female_share, main='demographics_female_share2', outline=T)




#פונקציית צפיפות והתפלגות מצטברת#

#שכר סגל ממוצע 
# Density's Function of X4 with line - Mission5
hist(table$faculty_salary,prob=TRUE, main='faculty_salary',xlab = 'faculty_salary',col="orange")
lines(density(table$faculty_salary),col="black",lwd=2)

xx<-ecdf(table$faculty_salary)
plot(xx, main = "CDF of Faculty Salary")


#שיעור הסגל במשרה מלאה
c4<-ecdf(table$ft_faculty_rate)
plot(c4, main = "CDF of Faculty Rate")

hist(table$ft_faculty_rate,prob=TRUE, main='ft_faculty_rate',xlab = 'ft_faculty_rate',col="orange")
lines(density(table$ft_faculty_rate),col="black",lwd=2)


#גיל כניסה ממוצע
c4<-ecdf(table$demographics_age_entry)
plot(c4, main = "CDF of demographics age entry")

hist(table$demographics_age_entry,prob=TRUE, main='demographics_age_entry',xlab = 'demographics_age_entry',col="orange")
lines(density(table$demographics_age_entry),col="black",lwd=2)


#אחוז סטודנטיות
c4<-ecdf(table$demographics_female_share)
plot(c4, main = "CDF of demographics female share")

hist(table$demographics_female_share,prob=TRUE, main='demographics_female_share',xlab = 'demographics_female_share',col="orange")
lines(density(table$demographics_female_share),col="black",lwd=2)


#אחוז סטודנטים שהוריהם עם רמת השכלה תיכונית
c4<-ecdf(table$parents_highschool)
plot(c4, main = "CDF of parents highschool")

hist(table$parents_highschool,prob=TRUE, main='parents_highschool',xlab = 'parents_highschool',col="orange")
lines(density(table$parents_highschool),col="black",lwd=2)



#תרשימים מיוחדים#
#Mosaic Plots
degrees<-subset(table,table$highest_degrees_Number!=3)
degrees<-subset(degrees,degrees$highest_degrees_Number!=2)
totals<-table(degrees$ownership,degrees$highest_degrees_awarded)
mosaicplot(totals,xlab="Ownership",main = "",ylab="Highest Degree Given",col=c(2,3,4))


table$income_range<-ifelse(between(table$income,20,30),"20-30",
                           ifelse(between(table$income,31,40),"31-40",
                                  ifelse(between(table$income,41,50),"41-50","51-70")))

table1 <- table(table$income_range,table$highest_degrees_awarded) 
mosaicplot(table1,xlab="Income",main = "",ylab="Highest Degree Given",col=c(2,3,4,6,7),border = "chocolate",las=1)

#Scatter Plots With Regression Line

plot(table$demographics_age_entry, table$parents_highschool,
     xlab = "Entry Age", ylab = "Parents Education",
     pch = 19, frame = FALSE)
abline(lm(table$parents_highschool ~ table$demographics_age_entry, data = table), col = "blue")                                

plot(table$income, table$demographics_female_share,
     xlab = "Income", ylab = "Female Student Rate",col="dark red",
     pch = 19, frame = FALSE)
abline(lm(table$demographics_female_share ~ table$income, data = table), col = "blue")

plot(table$income, table$ft_faculty_rate,
     xlab = "Income", ylab = "Full Time Job",col="dark green",
     pch = 19, frame = FALSE)
abline(lm(table$ft_faculty_rate ~ table$income, data = table), col = "blue")


#טבלאות שכיחות#
#חד ממדיות
#טבלה 1
faculty_salary_list <- cbind(Freq=table(cut(table$faculty_salary, 
                                            breaks = seq(0,14000,2000))), relative=
                               round(prop.table(table(cut(table$faculty_salary,breaks = 
                                                            seq(0,14000,2000)))),2))

#טבלה 2
parents_highschool_list <- cbind(Freq=table(cut(table$parents_highschool, 
                                                breaks = seq(0,1,0.2))), relative=
                                   round(prop.table(table(cut(table$parents_highschool,breaks = 
                                                                seq(0,1,0.2)))),2))
#דו ממדיות
#טבלה 1
female_and_ownership_table <- cbind(Freq=table(cut(table$demographics_female_share, seq(0,1,0.2)), table$ownership)
                                    ,relative=round(prop.table(table(cut(table$demographics_female_share,
                                                                         breaks = seq(0,1,0.2)))),2))

#טבלה 2
Age_and_degrees_table <- cbind(Freq=table(cut(table$demographics_age_entry, seq(18,34,4)), table$highest_degrees_awarded)
                               ,relative=round(prop.table(table(cut(table$demographics_age_entry,
                                                                    breaks = seq(18,34,4)))),2))







#חלק ב#

#סעיף א#
cor.test(dataset$faculty_salary,dataset$income,method = "pearson")
cor.test(dataset$ft_faculty_rate,dataset$income,method = "pearson")
cor.test(dataset$demographics_age_entry,dataset$income,method = "pearson")
cor.test(dataset$demographics_female_share,dataset$income,method = "pearson")
cor.test(dataset$parents_highschool,dataset$income,method = "pearson")
cor.test(dataset$highest_degrees_Number,dataset$income,method = "pearson")
cor.test(dataset$online_binarey,dataset$income,method = "pearson")
cor.test(dataset$ownership_Number,dataset$income,method = "pearson")



#####סעיף ב#####
####הפיכת משתנה רציף למשתנה קטגוריאלי#######
dataset$demographics_female_share <- ifelse(dataset$demographics_female_share<0.4,1,
                                            ifelse(dataset$demographics_female_share>0.6,3,2)) 

tableWomenRateLow <-  subset(dataset,dataset$demographics_female_share==1)
summary(tableWomenRateLow$income)

tableWomenRateMedium <-  subset(dataset,dataset$demographics_female_share==2)
summary(tableWomenRateMedium$income)

tableWomenRateHigh <-  subset(dataset,dataset$demographics_female_share==3)
summary(tableWomenRateHigh$income)

plot(dataset$demographics_female_share,dataset$income,ylim = c(10,100)
     ,xlab = "Female Share Rate",ylab = "Income",col="blue")


####איחוד קטגוריות####

NonDegree <-  subset(dataset,dataset$highest_degrees_Number==1)
summary(NonDegree $income)

CertificateDegree <-  subset(dataset,dataset$highest_degrees_Number==2)
summary(CertificateDegree $income)

AssociateDegree <-  subset(dataset,dataset$highest_degrees_Number==3)
summary(AssociateDegree $income)

BachelorDegree <-  subset(dataset,dataset$highest_degrees_Number==4)
summary(BachelorDegree $income)

GraduateDegree<-  subset(dataset,dataset$highest_degrees_Number==5)
summary(GraduateDegree $income)


dataset$highest_degrees_Number <- ifelse(dataset$highest_degrees_Number==1,5,dataset$highest_degrees_Number)
dataset$highest_degrees_Number <- ifelse(dataset$highest_degrees_Number==2,3,dataset$highest_degrees_Number)
NotAcademicDegree  <-subset(dataset,dataset$highest_degrees_Number==3) 
summary(NotAcademicDegree$income)

plot(dataset$highest_degrees_Number,dataset$income,ylim = c(10,80),xlab = "Highest Degree",ylab = "Income",col="blue")


####הגדרת משתני דמה ואינטרקציה####

ownership_Dum <- factor(dataset$ownership_Number)
mod <- lm(dataset$income~dataset$faculty_salary*ownership_Dum)
summary(mod)

plot(dataset$faculty_salary[dataset$ownership_Number==1],dataset$income[dataset$ownership_Number==1],col="blue",
     xlab = "Faculty Salary",ylab = "Income",main = "Income vs. FacultySalary by Ownership")
points(dataset$faculty_salary[dataset$ownership_Number==2],dataset$income[dataset$ownership_Number==2],col="green")
points(dataset$faculty_salary[dataset$ownership_Number==3],dataset$income[dataset$ownership_Number==3],col="red")
legend(900,90,legend = c("1","2","3"),cex=0.5,text.width = 0.1,title.cex = 0.7,lwd = 2,col=c("blue","green","red"),
       title = "Ownership",box.col = "white")
abline(a=13.5161933,b=0.0038935,col="blue")
abline(a=13.5161933-2.1277927,b=0.0038935-0.0005816,col="green")
abline(a=13.5161933+14.8163438,b=0.0038935-0.0030150,col="red")



highest_degrees_Dum <- factor(dataset$highest_degrees_Number)    
mod <- lm(dataset$income~dataset$faculty_salary*highest_degrees_Dum)
summary(mod)

plot(dataset$faculty_salary[dataset$highest_degrees_Number==3],dataset$income[dataset$highest_degrees_Number==3],col="blue",
     xlab = "Faculty Salary", xlim= c(1000,12000),ylab = "Income",ylim = c(15,100),main = "Income vs. FacultySalary by degree")
points(dataset$faculty_salary[dataset$highest_degrees_Number==4],dataset$income[dataset$highest_degrees_Number==4],col="green")
points(dataset$faculty_salary[dataset$highest_degrees_Number==5],dataset$income[dataset$highest_degrees_Number==5],col="red2")
legend(1000,102,legend = c("3","4","5"),cex=0.5,text.width = 0.1,title.cex = 0.7,lwd = 2,col=c("blue","green","red"),
       title = "degree's type",box.col = "white")
abline(a=20.6786060,b=0.0014760 ,col="blue")
abline(a=20.6786060+3.5901039,b=0.0014760-0.0003063 ,col="green")
abline(a=20.6786060+5.5647133,b=0.0014760+0.0008507,col="red2")



female_Dum <- factor(dataset$demographics_female_share)
mod <- lm(dataset$income~dataset$faculty_salary*female_Dum)
summary(mod)

plot(dataset$faculty_salary[dataset$demographics_female_share==1],dataset$income[dataset$demographics_female_share==1],col="blue",
     ylim=c(15,100),xlab = "Faculty Salary",ylab = "Income",main = "Income vs. FacultySalary by Female demography")
points(dataset$faculty_salary[dataset$demographics_female_share==2],dataset$income[dataset$demographics_female_share==2],col="green")
points(dataset$faculty_salary[dataset$demographics_female_share==3],dataset$income[dataset$demographics_female_share==3],col="red")
legend(2800,101,legend = c("1","2","3"),cex=0.5,text.width = 0.1,title.cex = 0.7,lwd = 2,col=c("blue","green","red"),
       title = "femaleRate",box.col = "white")
abline(a=15.004446,b=0.004759,col="blue")
abline(a=15.004446-1.145286,b=0.004759-0.001534,col="green")
abline(a=15.004446+8.355827,b=0.004759-0.002961,col="red")

####התאמת המודל ובדיקת הנחות המודל######

#בחירת משתני המודל#

originalModel <- lm(dataset$income~dataset$faculty_salary*highest_degrees_Dum+dataset$faculty_salary*
                      female_Dum+dataset$faculty_salary*ownership_Dum) #Original Model

Emp <- lm(dataset$income~1,data=dataset)
Full <- lm(originalModel)

fwd.model <- step(Emp,direction = 'forward',scope = ~dataset$faculty_salary*highest_degrees_Dum+
                    dataset$faculty_salary*female_Dum+dataset$faculty_salary*ownership_Dum)
bwd.model <- step(Full,direction = 'backward',k=2,scope=~1)
sw.model <- step(Emp,direction = 'both',scope = ~dataset$faculty_salary*highest_degrees_Dum+
                   dataset$faculty_salary*female_Dum+dataset$faculty_salary*ownership_Dum)

summary(originalModel) #Adjusted R-squared = 0.4615 
extractAIC(originalModel,k=2) #AIC value for the original model = 623.86
extractAIC(originalModel,k=log(10)) #BIC value the original model = 628.1

summary(fwd.model) #Adjusted R-squared = 0.4553 
extractAIC(fwd.model,k=2) #AIC value for the fwd model = 619.9459
extractAIC(fwd.model,k=log(10)) #BIC value the fwd model = 622.3666

summary(bwd.model) #Adjusted R-squared = 0.4768 !MAX!
extractAIC(bwd.model,k=2) #AIC value for bwd model = 616.2531 !LOWEST!
extractAIC(bwd.model,k=log(10)) #BIC value for bwd model =  619.279 !LOWEST!

summary(sw.model) #Adjusted R-squared = 0.4553 
extractAIC(sw.model,k=2) #AIC value for the fwd model = 619.9459
extractAIC(sw.model,k=log(10)) #BIC value the fwd model = 622.3666

finalModel <- bwd.model

##בדיקת הנחות המודל###

#perform Chow test

dataset_high <- subset(dataset, dataset$faculty_salary > 5500)
dataset_low <- subset(dataset, dataset$faculty_salary < 5500)

xxx <- lm(dataset_high$income~dataset_high$faculty_salary, data=dataset_high)
anova(xxx) # SSE_High = 9681.8
xxy <- lm(dataset_low$income~dataset_low$faculty_salary, data=dataset_low)
anova(xxy) # SSE_Low = 4780.4

xxz <- lm(dataset$income~dataset$faculty_salary, data=dataset)
anova(xxz) #Residuals --> SSE_regular = 15457.3
# SSE_AVG= SSE_regular - SSE_Low - SSE_High = 15457.3 - 9681.8 - 4780.4 = 995.1

F_st <- (995.1/2)/((9681.8 + 4780.4)/134)  #(= 4.610066)
# F_cr = 3.072 (From F_table)
# F_cr < F_st ---> Rejected! No linear

summary(xxz)

#perform the Goldfeld Quandt test
gqtest(xxz, order.by = ~dataset$faculty_salary, data = dataset, fraction = 27)
# GQ = F_st = 2.1021, P_value = 0.003789
# F_cr by df1=54, df2=53 --> 1.5343
#F_cr < F_st --> Rejected! There is Heteroskedasticity

# K.s and Shapiro tests for normalized
mod1 <- lm(dataset$income~dataset$faculty_salary, data=dataset)
dataset$fitted<-fitted(mod1) # predicted values
dataset$residuals<-residuals(mod1) # residuals
s.e_res <- sqrt(var(dataset$residuals))
dataset$stan_residuals<-(residuals(mod1)/s.e_res)

shapiro.test(dataset$stan_residuals) # בדיקת נורמליות לא מתקיימת - p-value = 1.472e-11
ks.test(x=dataset$stan_residuals, y='pnorm', alternative = 'two.sided', exact=NULL)
#הנחת נורמליות לא מתקיימת -  p-value = 0.02807

qqnorm(dataset$stan_residuals)
abline(a=0, b=1)
hist(dataset$stan_residuals, xlab ="Normalized error", main="Histogram of normalized error")

x <- lm(dataset$fitted~dataset$stan_residuals)
plot(x)


####שיפור המודל ######

#יצירת שיוויון שונויות#

dataset$incomeSquareRoot <- sqrt(dataset$income) #הוספת עמודת שורש הרווח
dataset$incomeLan <- ln(dataset$income) #הוספת עמודת לאן הרווח

rootModel <- lm(dataset$incomeSquareRoot~dataset$faculty_salary, data=dataset)
lnModel<- lm(dataset$incomeLan~dataset$faculty_salary, data=dataset)

gqtest(rootModel, order.by = ~dataset$faculty_salary, data = dataset, fraction = 27)
#GQ = 1.2802, p-value = 0.1849 --> מתקיים שיוייון שונויות
gqtest(lnModel, order.by = ~dataset$faculty_salary, data = dataset, fraction = 27)
#GQ = 0.81186, p-value = 0.7761 --> מתקיים שיוייון שונויות - מובהקות טובה יותר

fullLanModel <- lm(dataset$incomeLan~dataset$faculty_salary*
                     highest_degrees_Dum+dataset$faculty_salary*
                     female_Dum+dataset$faculty_salary*ownership_Dum) #Original Model
summary(fullLanModel)

Emp <- lm(dataset$incomeLan~1,data=dataset)
Full <- lm(fullLanModel)

fwd.model <- step(Emp,direction = 'forward',scope = ~dataset$faculty_salary*
                    highest_degrees_Dum+dataset$faculty_salary*female_Dum+dataset$faculty_salary*ownership_Dum)
bwd.model <- step(Full,direction = 'backward',k=2,scope=~1)
sw.model <- step(Emp,direction = 'both',scope = ~dataset$faculty_salary*
                   highest_degrees_Dum+dataset$faculty_salary*female_Dum+dataset$faculty_salary*ownership_Dum)

summary(bwd.model)
extractAIC(bwd.model,k=log(10)) #BIC value of bwd.model
#bw.model is still the best model --> AIC= -417.22 , BIC = -413.58 , R-adj = 0.539


#בדיקת הליניאריות - Chow Test

dataset_high <- subset(dataset, dataset$faculty_salary > 5500)
dataset_low <- subset(dataset, dataset$faculty_salary < 5500)

xxx <- lm(dataset_high$incomeLan~dataset_high$faculty_salary, data=dataset_high)
anova(xxx) # SSE_High = 4.0899
xxy <- lm(dataset_low$incomeLan~dataset_low$faculty_salary, data=dataset_low)
anova(xxy) # SSE_Low = 5.0156 

xxz <- lm(dataset$incomeLan~dataset$faculty_salary, data=dataset)
anova(xxz) #Residuals --> SSE_regular = 9.4308

# SSE_AVG= SSE_regular - SSE_Low - SSE_High = 9.4308 - 5.0156 - 4.0899 = 0.3253
(0.3253/2)/((4.0899 + 5.0156)/134)
F_st <- (0.3253/2)/((4.0899 + 5.0156)/134)  #(= 2.393619)
# F_cr = 3.072 (From F_table)
#F_st= 2.3936 ,F_cr = 3.072 --> F_st < F_cr --> לא נדחה את השערת האפס ונאמר כי המודל לינארי



# טרנספורמציה למשתנה המסביר הרציף שכבר נמצא במודל
#פחות טוב לאחר טרנספורמציה

dataset$faculty_salary_X2 <- dataset$faculty_salary^2 

cor.test(dataset$faculty_salary,dataset$incomeLan,method = "pearson")  # X
cor.test(log(dataset$faculty_salary),dataset$incomeLan,method = "pearson") #log(X)
cor.test(dataset$faculty_salary_X2,dataset$incomeLan,method = "pearson") #X^2 --> הטוב ביותר
cor.test(sqrt(dataset$faculty_salary),dataset$incomeLan,method = "pearson") #X^0.5


fullModel <- lm(dataset$incomeLan~dataset$faculty_salary_X2*highest_degrees_Dum+dataset$faculty_salary_X2*female_Dum+dataset$faculty_salary_X2*ownership_Dum)
summary(fullModel)

Emp <- lm(dataset$incomeLan~1,data=dataset)
Full <- lm(fullModel)

fwd.model <- step(Emp,direction = 'forward',scope = ~dataset$faculty_salary_X2*(highest_degrees_Dum+female_Dum+ownership_Dum))
bwd.model <- step(Full,direction = 'backward',k=2,scope=~1)
sw.model <- step(Emp,direction = 'both',scope = ~dataset$faculty_salary_X2*(highest_degrees_Dum+female_Dum+ownership_Dum))

summary(fwd.model)
summary(bwd.model)
summary(sw.model) #  AIC=-419.19, BIC = -411.34, Adjusted R-squared = 0.5267 --> הטוב ביותר אך פחות טוב מהמודל הישן


#בדיקת משתנים מסבירים שהסרנו

cor.test(dataset$ft_faculty_rate,dataset$income,method = "pearson") # X 
cor.test(log(dataset$ft_faculty_rate),dataset$income,method = "pearson") #log(X)
cor.test(dataset$ft_faculty_rate^2,dataset$income,method = "pearson") #X^2 --> הטוב ביותר מביניהם
cor.test(sqrt(dataset$ft_faculty_rate),dataset$income,method = "pearson") #X^0.5

cor.test(dataset$demographics_age_entry,dataset$income,method = "pearson") # X
cor.test(log(dataset$demographics_age_entry),dataset$income,method = "pearson") #log(X) --> הטוב ביותר מביניהם
cor.test(dataset$demographics_age_entry^2,dataset$income,method = "pearson") #X^2
cor.test(sqrt(dataset$demographics_age_entry),dataset$income,method = "pearson") #X^0.5

cor.test(dataset$parents_highschool,dataset$income,method = "pearson") # X
cor.test(log(dataset$parents_highschool),dataset$income,method = "pearson") #log(X) --> הטוב ביותר מביניהם
cor.test(dataset$parents_highschool^2,dataset$income,method = "pearson") #X^2
cor.test(sqrt(dataset$parents_highschool),dataset$income,method = "pearson") #X^0.5


#ניסיון הוספת המשתנים המסבירים שהסרנו למודל

with_ft_rate_model <- lm(dataset$incomeLan~dataset$faculty_salary*(highest_degrees_Dum+female_Dum+ownership_Dum)+
                  dataset$ft_faculty_rate*(highest_degrees_Dum+female_Dum+ownership_Dum))
summary(with_ft_rate_model)


with_ft_and_parents_model <- lm(dataset$incomeLan~dataset$faculty_salary*(highest_degrees_Dum+female_Dum+ownership_Dum)+
                  dataset$ft_faculty_rate*(highest_degrees_Dum+female_Dum+ownership_Dum)+
                  log(dataset$parents_highschool)*(highest_degrees_Dum+female_Dum+ownership_Dum))
summary(with_ft_and_parents_model)


fullModel <- lm(dataset$incomeLan~dataset$faculty_salary*(highest_degrees_Dum+female_Dum+ownership_Dum)+
                  dataset$ft_faculty_rate*(highest_degrees_Dum+female_Dum+ownership_Dum)+
                  log(dataset$parents_highschool)*(highest_degrees_Dum+female_Dum+ownership_Dum)+
                  log(dataset$demographics_age_entry)*(highest_degrees_Dum+female_Dum+ownership_Dum)) #Original Model
summary(fullModel)

Emp <- lm(dataset$incomeLan~1,data=dataset)
Full <- lm(fullModel)

fwd.model <- step(Emp,direction = 'forward',scope = ~dataset$faculty_salary*(highest_degrees_Dum+female_Dum+ownership_Dum)+ dataset$ft_faculty_rate*(highest_degrees_Dum+female_Dum+ownership_Dum)+log(dataset$parents_highschool)*(highest_degrees_Dum+female_Dum+ownership_Dum)+log(dataset$demographics_age_entry)*(highest_degrees_Dum+female_Dum+ownership_Dum))
bwd.model <- step(Full,direction = 'backward',k=2,scope=~1)
sw.model <- step(Emp,direction = 'both',scope = ~dataset$faculty_salary*(highest_degrees_Dum+female_Dum+ownership_Dum)+ dataset$ft_faculty_rate*(highest_degrees_Dum+female_Dum+ownership_Dum)+log(dataset$parents_highschool)*(highest_degrees_Dum+female_Dum+ownership_Dum)+log(dataset$demographics_age_entry)*(highest_degrees_Dum+female_Dum+ownership_Dum))

summary(fwd.model) 
summary(bwd.model) # AIC=-444.44 ,BIC = -439.32, Adjusted R-squared = 0.6443 --> הטוב ביותר ויותר טוב מהמודל הישן
summary(sw.model) 

best_Model_In_Project <- bwd.model
summary(best_Model_In_Project)

dataset$fitted<-fitted(best_Model_In_Project) # predicted values
dataset$residuals<-residuals(best_Model_In_Project) # residuals
s.e_res <- sqrt(var(dataset$residuals))
dataset$stan_residuals<-(residuals(best_Model_In_Project)/s.e_res)

ks.test(x=dataset$stan_residuals, y='pnorm', alternative = 'two.sided',
        exact=NULL) #הנחת נורמליות כן מתקיימת -  p-value = 0.1541

qqnorm(dataset$stan_residuals)
abline(a=0, b=1)
hist(dataset$stan_residuals, xlab ="Normalized error", main="Histogram of normalized error")
