
###
### Read in data ----
###

# this is a test

ducks <- read.csv("~/Box/EVE298/data.csv", header = T)
head(ducks)
str(ducks)

library(ggplot2)
library(tidyverse)

#MAKE DATE FACTOR TO ALLOW NONLINEAR
#USE LME4 AND LMERTEST FOR P VALUES - OK TO USE LLTEST BC ERROR IS OK

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
boxplot(ducks.df$lipid ~ ducks.df$hunt, pch = 20)

ducks.df <- ducks.df[!is.na(ducks.df$molt),]
DUCKS <- ducks.df[ducks.df$species!="CITE",]

DUCKS17 <- DUCKS[DUCKS$year==2017,]
DUCKS18 <- DUCKS[DUCKS$year==2018,]

###
### Data exploration ----
###

str(DUCKS)

# response variable
plot(DUCKS$lipid)
hist(DUCKS$lipid, col="hotpink")
dotchart(DUCKS$lipid)

# predictor variables
hist((DUCKS$date), col="lightblue")
dotchart(DUCKS$date)

DUCKS %>%
  ggplot(aes(x = species, y = lipid, fill = hunt, alpha = 0.5))+
  geom_boxplot()+
  facet_wrap(~year,2)

ggplot(aes(date, lipid, col=sex), data = DUCKS) + 
  geom_point() + 
  geom_smooth(aes(group = sex), method = "loess", se=F) +
  facet_wrap(~species,2) +
  theme_bw()

###
### Modeling ----
###

str(DUCKS)

library(lme4)
library(lmerTest)

### * first collinearity ----

mod.vif <- lmer(lipid ~ 
                   date + species + age + sex + year + (1|site), 
                 data = DUCKS)
car::vif(mod.vif) #all good

### * full model ----

mod.full <- lmer(lipid ~ 
                   date:species + date:sex +
                   date + species + age + sex + year + (1|site), 
                 data = DUCKS)
summary(mod.full)

#or test polynomial date 
mod.poly <- lmer(lipid ~ 
                   poly(date,3):species + poly(date,3):sex +
                   poly(date,3) + species + age + sex + year + (1|site), 
                 data = DUCKS)
AIC(mod.poly,mod.full)

mod.full = mod.poly

### * terms significance ----

## RANDOM EFFECT = site

mod.full.nlme <- lme(lipid ~ 
                   poly(date,3):species + poly(date,3):sex +
                   poly(date,3) + species + age + sex + year, random=~1|site,
                   method="REML",
                 data = DUCKS)

mod.RE <- gls(lipid ~ 
                   poly(date,3):species + poly(date,3):sex +
                   poly(date,3) + species + age + sex + year, 
              method="REML",
                 data = DUCKS)
anova(mod.full.nlme,mod.RE) #significant


## FIXED EFFECTS -- refit using ML

mod.full.ML <- lmer(lipid ~ 
                   poly(date,3):species + poly(date,3):sex +
                   poly(date,3) + species + age + sex + year + (1|site), 
                   REML = FALSE,
                 data = DUCKS)

mod.year <- lmer(lipid ~ 
                   poly(date,3):species + poly(date,3):sex +
                   poly(date,3) + species + age + sex + (1|site), 
                 REML = FALSE,
                 data = DUCKS)
anova(mod.full.ML,mod.year) #year is significant

mod.age <- lmer(lipid ~ 
                   poly(date,3):species + poly(date,3):sex +
                   poly(date,3) + species + year + sex + (1|site), 
                 REML = FALSE,
                 data = DUCKS)
anova(mod.full.ML,mod.age) #age is significant

mod.ds <- lmer(lipid ~ 
                  poly(date,3):sex +
                  poly(date,3) + species + age + year + sex + (1|site), 
                REML = FALSE,
                data = DUCKS)
anova(mod.full.ML,mod.ds) #poly(date,3):species interaction

mod.dsex <- lmer(lipid ~ 
                      poly(date,3):species + 
                      poly(date,3) + species + age + sex + year + (1|site), 
                    REML = FALSE,
                    data = DUCKS)
anova(mod.full.ML,mod.dsex) #poly(date,3):sex interaction

mod.final <- mod.full
summary(mod.final)

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
plot(resid(mod.final) ~ DUCKS$date, pch=20)
boxplot(resid(mod.final) ~ DUCKS$sex)
boxplot(resid(mod.final) ~ DUCKS$species)
boxplot(resid(mod.final) ~ DUCKS$hunt)
boxplot(resid(mod.final) ~ DUCKS$year)
boxplot(resid(mod.final) ~ DUCKS$age)
boxplot(resid(mod.final) ~ DUCKS$molt)

#4 plot predictor effects
terms(mod.final)
library(effects)
plot(effect(mod=mod.final,term="poly(date,3):species"))
plot(effect(mod=mod.final,term="poly(date,3):sex"))
plot(effect(mod=mod.final,term="sex"))
plot(effect(mod=mod.final,term="species"))
plot(effect(mod=mod.final,term="poly(date,3)"))
plot(effect(mod=mod.final,term="age"))
plot(effect(mod=mod.final,term="year"))

#5 R2
summary(mod.final)
fixef(mod.final)
fix.matrix <- model.matrix(mod.final, data = DUCKS)

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

