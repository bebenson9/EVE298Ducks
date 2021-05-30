
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
ducks.df$age <- as.factor(ifelse(ducks$age=="A","Adult",
                                 "Immature"))
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
  ggplot(aes(x = species, fill = sex, alpha = 0.5))+
  geom_bar()+
  theme_bw()+
  facet_wrap(~year:age,2)

DUCKS %>%
  ggplot(aes(x = species, fill = sex, alpha = 0.5))+
  geom_bar()+
  theme_bw()+
  facet_wrap(~age,2)

DUCKS %>%
  ggplot(aes(x = species, y = lipid, fill = sex, alpha = 0.5))+
  geom_boxplot()+
  theme_bw() +
  facet_wrap(~year,2)

ggplot(aes(date, lipid, col=sex), data = DUCKS) + 
  geom_point() + 
  geom_smooth(aes(group = sex), method = "loess", se=F) +
  facet_wrap(~species,2) +
  theme_bw()

pairs(DUCKS)

###
### Modeling ----
###

str(DUCKS)

library(lme4)
library(lmerTest)

### * first collinearity ----

mod.vif <- lmer(lipid ~ 
                   date + species + age + sex + year + molt + (1|site), 
                 data = DUCKS)
car::vif(mod.vif) #all good

### * full model ----

mod.full <- lmer(lipid ~ poly(date,3):species:sex + 
#the effects of time differ btwn the species and this effect differs btwn the sexes
  poly(date,3):species + poly(date,3):sex + species:sex +
  poly(date,3) + species + sex + 
                   age + year + molt + #confounds
                   (1|site), 
                 REML = TRUE,
                 data = DUCKS)
summary(mod.full)

### * model selection ----

library(nlme)

## RANDOM EFFECT
mod.full.nlme <- lme(lipid ~ poly(date,3):species:sex +
                   poly(date,3):species + poly(date,3):sex + species:sex +
                   poly(date,3) + species + sex + 
                   age + year + molt,
                   random=~1|site, 
                 method = "REML",
                 data = DUCKS)
mod.RE <- gls(lipid ~ poly(date,3):species:sex +
                   poly(date,3):species + poly(date,3):sex + species:sex +
                   poly(date,3) + species + sex + 
                   age + year + molt, 
                method = "REML",
                data = DUCKS)
anova(mod.full.nlme,mod.RE)

## FIXED EFFECTS
#first test 3 way
mod.full.ML <- lmer(lipid ~ poly(date,3):species:sex +
                   poly(date,3):species + poly(date,3):sex + species:sex +
                   poly(date,3) + species + sex + 
                   age + year + molt + 
                   (1|site), 
                 REML = FALSE,
                 data = DUCKS)
drop1(mod.full.ML,test="Chisq")

# first test poly(date,3):species:sex effect
mod.3way <- lmer(lipid ~ 
                   poly(date,3):species + poly(date,3):sex + species:sex +
                   poly(date,3) + species + sex + 
                   age + year + molt + 
                   (1|site), 
                    REML = FALSE,
                    data = DUCKS)
anova(mod.full.ML, mod.3way) #3 way p = 0.03282 
#GET P-VALUES FITTING WITH REML??

#then test single fixed effects
mod.molt <- lmer(lipid ~ poly(date,3):species:sex +
                      poly(date,3):species + poly(date,3):sex + species:sex +
                      poly(date,3) + species + sex + 
                      age + year + 
                      (1|site), 
                    REML = FALSE,
                    data = DUCKS)
anova(mod.full.ML, mod.molt) #molt p = 0.15 *discuss

mod.age <- lmer(lipid ~ poly(date,3):species:sex +
                   poly(date,3):species + poly(date,3):sex + species:sex +
                   poly(date,3) + species + sex + 
                   year + molt + 
                   (1|site), 
                 REML = FALSE,
                 data = DUCKS)
anova(mod.full.ML, mod.age) #molt p = 0.01384

mod.year <- lmer(lipid ~ poly(date,3):species:sex +
                      poly(date,3):species + poly(date,3):sex + species:sex +
                      poly(date,3) + species + sex + 
                      age + molt + 
                      (1|site), 
                    REML = FALSE,
                    data = DUCKS)
anova(mod.full.ML, mod.year) #molt p = 0.01189

### * final model ----

mod.final <- lmer(lipid ~ poly(date,3):species:sex +
                    poly(date,3):species + poly(date,3):sex + species:sex +
                    poly(date,3) + species + sex + 
                    age + year + molt + 
                    (1|site), 
                  REML = TRUE,
                  data = DUCKS)

mod.final.nlme <- lme(lipid ~ poly(date,3):species:sex +
                    poly(date,3):species + poly(date,3):sex + species:sex +
                    poly(date,3) + species + sex + 
                    age + year + molt,
                    random=~1|site, 
                  method = "REML",
                  data = DUCKS)

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
boxplot(resid(mod.final) ~ DUCKS$sex, pch=20)
boxplot(resid(mod.final) ~ droplevels(DUCKS$species), pch=20)
boxplot(resid(mod.final) ~ DUCKS$hunt, pch=20)
boxplot(resid(mod.final) ~ DUCKS$year, pch=20)
boxplot(resid(mod.final) ~ DUCKS$age, pch=20)
boxplot(resid(mod.final) ~ DUCKS$molt, pch=20)
boxplot(resid(mod.final) ~ DUCKS$site, pch=20)

#4 plot predictor effects
terms(mod.final)
library(effects)
plot(effect(mod=mod.final,term="poly(date,3):species:sex"),
     x.var=1,multiline = T)
plot(effect(mod=mod.final,term="age"))
plot(effect(mod=mod.final,term="molt"))
plot(effect(mod=mod.final,term="year"))

#5 R2
library(MuMIn)
r.squaredGLMM(mod.final.nlme)
'marginal R2 - variance explained by fixed effects only'
'conditional R2 = variance explained by fixed and random'

###
### Post-hoc testing ----
###

summary(mod.final)
summary(DUCKS$date) 
#get lone fixed effects effect sizes (age, year, molt)

#for the 3-way do post hoc/
#emmeans to figure out which duck combos do what


'From kates class code - Give me an effect size!'

vardate <- var(fixef(mod.final)[2]*fix.matrix[,2])
varsex <- var(fixef(mod.final)[3]*fix.matrix[,3])

'variance explained by time'
vardate/(varF + varR + varE)

'variance explained by sex'
varsex/(varF + varR + varE)
















# * emmeans ----

# library(emmeans)
# contrast(emmeans(mod.final, ~species:sex, data=DUCKS), 
#          "consec", simple = "each", combine = TRUE, adjust = "mvt")
# #driven by NOPI-MALL & MALL M-F
# 




