## RCT: working from home
rm(list=ls())

### Packages
library(foreign)
library(stargazer)
library(haven)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(chron)
library(lattice)
library(dummies)
library(lfe)
library(sandwich)
library(lmtest)
library(miceadds)
library(multiwayvcov)
library(gridExtra)

### import data
exdata <- read_dta("Working from home-20210518/Exhaustion.dta")
View(exdata)

### explore data
exdata_d <- data.frame(exdata)
stargazer(exdata_d,type="text", out = "Downloads/summary.txt")
View(exdata_d)

### data transformation
# introduce treatment period and create subset for treatment = 1
exdata_d<-within(exdata_d,{treatment<-ave(experiment_expgroup,year_week,FUN=max)})
exdata_dt<-subset(exdata_d,treatment==1)
# label expgroup as treated and control
exdata_dt<-within(exdata_dt,{D_group<-ifelse(expgroup==1, "treated", "control")})
# create family variable 
exdata_dt<-within(exdata_dt,{family<-ifelse(married==0 & children==0, "no family", "family")})
# create binary variable for "family" and "no family" 
exdata_dt<-within(exdata_dt,{family_code<-ifelse(family=="family",1,0)})
# check for missing values
exdata_d2<-na.omit(exdata_dt)
# there are no missing values as number of observations for exdata_dt and exdata_d2 are same

### find correlation between the explanatory variables
numericvars <- which(sapply(exdata_d2, is.numeric))
numericVarNames <- names(numericvars)
all_nvar <- exdata_d2[, numericvars]
cor_nvar <- cor(all_nvar, use = "pairwise.complete.obs")
corrplot.mixed(cor_nvar, tl.col = "black", tl.pos = "lt", 
               tl.cex = 1, cl.cex = 1)

### summarize data
# exhaustion vs age
cdur1<-ggplot(data=exdata_dt, aes(y=exhaustion, x=age)) + geom_point() + geom_smooth()
print(cdur1)

## trend
cdur2<-ggplot(data=exdata_dt, aes(y=exhaustion, x=age)) + 
  geom_point() + geom_smooth(method="lm")
print(cdur2)

# exhaustion vs age with higehr eduaction as factor
cdur3<-ggplot(data=exdata_dt, aes(y=exhaustion, x=age, col=factor(high_educ))) + 
  geom_point() + geom_smooth(method="lm") +
  scale_color_discrete(name="Higher Education",
                       breaks=c("0","1"),
                       labels=c("No","Yes"))
print(cdur3)

# exhaustion vs tenure with gender as factor
cdur4<-ggplot(data=exdata_dt, aes(y=exhaustion, x=tenure, col=factor(men))) + 
  geom_point() + geom_smooth(method="lm") +
  scale_color_discrete(name="Gender",
                       breaks=c("0","1"),
                       labels=c("Women","Men"))
print(cdur4)

# exhaustion vs grosswage with gender as factor
cdur5<-ggplot(data=exdata_dt, aes(y=exhaustion, x=grosswage, col=factor(men))) + 
  geom_point() + geom_smooth(method="lm") +
  scale_color_discrete(name="Gender",
                       breaks=c("0","1"),
                       labels=c("Women","Men"))
print(cdur5)

# exhaustion vs commute with volunteer as factor
cdur6<-ggplot(data=exdata_dt, aes(y=exhaustion, x=commute, col=factor(volunteer))) + 
  geom_point() + geom_smooth(method="lm") +
  scale_color_discrete(name="Volunteer",
                       breaks=c("0","1"),
                       labels=c("No","Yes"))
print(cdur6)

### hypothesis testing
# density plot to check effect of family on exhaustion
densityplot(~ lnexhaustion | family, data=exdata_dt, layout=c(1,2),
            panel=function(x,...){
              panel.densityplot(x,...)
              panel.abline(v=quantile(x,.5), col.line="red")
              panel.abline(v=mean(x), col.line="green")
            })
# density plot to check effect of family on exhaustion for control group
a<-densityplot(~ lnexhaustion | family, data=subset(exdata_dt,expgroup==0), layout=c(1,2), main="Control Group",
            panel=function(x,...){
              panel.densityplot(x,...)
              panel.abline(v=quantile(x,.5), col.line="red")
              panel.abline(v=mean(x), col.line="green")
            })
# density plot to check effect of family on exhaustion for treatment group
b<-densityplot(~ lnexhaustion | family, data=subset(exdata_dt,expgroup==1), layout=c(1,2), main="Treatment Group", 
            panel=function(x,...){
              panel.densityplot(x,...)
              panel.abline(v=quantile(x,.5), col.line="red")
              panel.abline(v=mean(x), col.line="green")
            })
grid.arrange(a,b,nrow=2)

### run regressions
# exhaustion vs family
C_M1<-lm(lnexhaustion ~ family_code*expgroup+year_week+personid, data=exdata_dt)
stargazer(C_M1, type="text", align=TRUE, omit=c("year_week", "personid"), out="Downloads/FirstRegression.txt")

C_M2<-lm(lnexhaustion ~ family_code*expgroup+age*high_educ+tenure*men+grosswage*men+volunteer*commute+bedroom+personid+year_week, data = exdata_dt)
stargazer(C_M1, C_M2, type="text", align=TRUE,omit = c("personid","year_week"), out="Downloads/SecondRegression.txt")

## clustered regression
C_M3<-miceadds::lm.cluster(lnexhaustion ~ family_code*expgroup+age*high_educ+tenure*men+grosswage*men+volunteer*commute+bedroom+personid+year_week, data = exdata_dt, cluster="personid")
cluster_se    <- sqrt(diag(vcov(C_M3)))
stargazer(C_M2,  type="text", align=TRUE,omit=c("year_week","personid"),se = list(NULL, cluster_se))

# exhaustion vs children
C_M4<-lm(lnexhaustion ~ children*expgroup+age*high_educ+tenure*men+grosswage*men+volunteer*commute+bedroom+personid+year_week, data = exdata_dt)
C_M5<-lm(lnexhaustion ~ children+age*high_educ+tenure*men+grosswage*men+volunteer*commute+bedroom+personid+year_week, data = subset(exdata_dt,expgroup==0))           
C_M6<-lm(lnexhaustion ~ children+age*high_educ+tenure*men+grosswage*men+volunteer*commute+bedroom+personid+year_week, data = subset(exdata_dt,expgroup==1))
stargazer(C_M4, C_M5, C_M6, type="text", align=TRUE,omit = c("personid","year_week"),out="Downloads/ThirdRegression.txt")

### for fixed effects
exdata_dt$year_week <- factor(exdata_dt$year_week)
exdata_dt$personid <- factor(exdata_dt$personid)
C_FM1<-felm(lnexhaustion ~ family_code*expgroup | year_week+personid, data=exdata_dt)
stargazer(C_FM1, type="text", align=TRUE, out="/Users/souravdutta/Downloads/FixedEffects1.txt")

C_FM2<-felm(lnexhaustion ~ family_code*expgroup+age*high_educ+tenure*men+grosswage*men+volunteer*commute+bedroom | year_week + personid, data = exdata_dt)
stargazer(C_FM2, type="text", align=TRUE, out="/Users/souravdutta/Downloads/FixedEffects2.txt")
