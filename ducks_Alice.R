
###
### Read in data ----
###

ducks <- read.csv("~/Box/EVE298/data.csv", header = T)
head(ducks)
str(ducks)

library(ggplot2)
library(tidyverse)

###
### Set up variables ----
###

# Q: How does body condition vary over time,
#    for different species, sex, age, molt, hunting.season
#    random effect = site; bird.id (in year)

ducks.df <- ducks[,c("lipid","coll.date")]
ducks.df$species <- as.factor(ducks$species)
summary(ducks.df$species)
ducks.df$sex <- as.factor(ducks$sex)
summary(ducks.df$sex)
ducks.df$age <- as.factor(ducks$age)
summary(ducks.df$age)
ducks.df$site <- as.factor(ducks$site)
summary(ducks.df$site)
ducks.df$year <- as.factor(ducks$year)
summary(ducks.df$year)
ducks.df$date <- as.numeric(ducks$days.since.oct)
summary(ducks.df$date)

## QUESTION: IS MOLT.SCORE>0 = MOLT=1???
ducks.df$molt <- as.factor(ifelse(!is.na(ducks$molt),
                        ducks$molt,
                        ifelse(ducks$molt.score>0,"Yes","No")))
summary(ducks.df$molt)

# hunting season - Oct 2 - Jan 12
# 277 (oct 4) - 365 OR 0 - 11 (Jan 11)
ducks.df$hunt <- as.factor(ifelse(ducks$jul.date>277 |
                                       ducks$jul.date<11,
                                     "hunt","no"))
summary(ducks.df$hunt)

ducks.df17 <- ducks.df[ducks.df$year==2017,]
ducks.df18 <- ducks.df[ducks.df$year==2018,]
ggplot(aes(date, lipid, col=species), data = ducks.df) + 
  geom_point() + 
  geom_smooth(aes(group = species), method = "loess")


###
### Data exploration ----
###

str(ducks.df)

# response variable
plot(ducks.df$lipid)
hist(ducks.df$lipid, col="hotpink")
dotchart(ducks.df$lipid)

# predictor variables
hist((ducks.df$date), col="lightblue")
dotchart(ducks.df$date)

ducks.df %>%
  ggplot(aes(x = species, y = lipid, fill = hunt, alpha = 0.5))+
  geom_boxplot()+
  facet_wrap(~year,2)

###
### Modeling ----
###

str(ducks.df)
ducksDF <- ducks.df[!is.na(ducks.df$molt),]

library(nlme)
mod.full <- lme(lipid ~ date + sex + species + 
             molt + age + year + hunt + species:date +
               sex:date + age:date,
           random = ~1|site, 
           data = ducksDF,
           method = "REML")
car::vif(mod.full)
mod.RE <- gls(lipid ~ date + sex + species + 
             molt + age + year + hunt +
               species:date + sex:date + age:date,
           data = ducksDF,
           method = "REML")
anova(mod.full,mod.RE)

mod.fullML <- lme(lipid ~ date + sex + species + 
                  molt + age + year + hunt +
                    species:date + sex:date + age:date,
                random = ~1|site, 
                data = ducksDF,
                method = "ML")
drop1(mod.fullML, test="Chisq")
mod.moltML <- lme(lipid ~ date + sex + species + 
                    age + year + hunt +
                    species:date + sex:date + age:date,
                  random = ~1|site, 
                  data = ducksDF,
                  method = "ML")
drop1(mod.moltML, test="Chisq")
mod.moltML <- lme(lipid ~ date + sex + species + 
                    age + year + hunt +
                    species:date + sex:date,
                  random = ~1|site, 
                  data = ducksDF,
                  method = "ML")
drop1(mod.moltML, test="Chisq")

mod.final <- lme(lipid ~ date + sex + species + 
                    age + year + hunt +
                      species:date + sex:date,
                  random = ~1|site, 
                  data = ducksDF,
                  method = "REML")
mod.final.gls <- gls(lipid ~ date + sex + species + 
                      age + year + hunt +
                        species:date + sex:date,
                    data = ducksDF,
                    method = "REML")
anova(mod.final.gls,mod.final)

summary(mod.final.gls)

###
### Model validation ----
###

'model diagnostics'

#1 variance homogeneity
plot(mod.final, pch=20)

#2 normality of errors
hist(resid(mod.final))

#3 variance homogeneity
coefficients(mod.final)
plot(resid(mod.final) ~ ducksDF$date, pch=20)
boxplot(resid(mod.final) ~ ducksDF$sex)
boxplot(resid(mod.final) ~ ducksDF$species)
boxplot(resid(mod.final) ~ ducksDF$hunt)
boxplot(resid(mod.final) ~ ducksDF$year)
boxplot(resid(mod.final) ~ ducksDF$age)
boxplot(resid(mod.final) ~ ducksDF$molt)

#4 plot predictor effects
terms(mod.final)
library(effects)
plot(effect(mod=mod.final,term="age"))
plot(effect(mod=mod.final,term="year"))
plot(effect(mod=mod.final,term="hunt"))
plot(effect(mod=mod.final,term="date:sex"))
plot(effect(mod=mod.final,term="date:species"))

#5 R2
summary(mod.final)
fixef(mod.final)
fix.matrix <- model.matrix(mod.final, data = ducksDF)

fixed <- (fixef(mod.final)[1] +
            fixef(mod.final)[2]*fix.matrix[,2] +
            fixef(mod.final)[3]*fix.matrix[,3] +
            fixef(mod.final)[4]*fix.matrix[,4] +
            fixef(mod.final)[5]*fix.matrix[,5] +
            fixef(mod.final)[6]*fix.matrix[,6] +
            fixef(mod.final)[7]*fix.matrix[,7] +
            fixef(mod.final)[8]*fix.matrix[,8] +
            fixef(mod.final)[9]*fix.matrix[,9] +
            fixef(mod.final)[10]*fix.matrix[,10] +
            fixef(mod.final)[11]*fix.matrix[,11] +
            fixef(mod.final)[12]*fix.matrix[,12] +
            fixef(mod.final)[13]*fix.matrix[,13] +
            fixef(mod.final)[14]*fix.matrix[,14] +
            fixef(mod.final)[15]*fix.matrix[,15] +
            fixef(mod.final)[16]*fix.matrix[,16] +
            fixef(mod.final)[17]*fix.matrix[,17] +
            fixef(mod.final)[18]*fix.matrix[,18] +
            fixef(mod.final)[19]*fix.matrix[,19]
)

varF <- var(fixed)

var.est <- as.numeric(VarCorr(mod.final))
varR <- var.est[1] #'variance among sites'
varE <- var.est[2] #'residual variance'

'marginal R2 - variance explained by fixed effects only'
varF/(varF + varR + varE)

'conditional R2 = variance explained by fixed and random'
(varF + varR)/(varF + varR + varE)

library(MuMIn)
r.squaredGLMM(mod.final)



'Give me an effect size!'

vardate <- var(fixef(mod.final)[2]*fix.matrix[,2])
varsex <- var(fixef(mod.final)[3]*fix.matrix[,3])

'variance explained by time'
vardate/(varF + varR + varE)

'variance explained by sex'
varsex/(varF + varR + varE)

