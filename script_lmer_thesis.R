# STEP 1: load data
data_pitch<- read.csv("[file].csv")
data_duration <- read.csv("[file].csv")
data_behavior<- read.csv("[file].csv")

#STEP 2: install and load relevant packages
#install.packages('lme4')
#install.packages('lmerTest')
#install.packages('effects')
library('lme4')
library('lmerTest')
library('effects')

#STEP 3: build and analyze models 

# speech analysis
  # pitch range 
m0 <- lmer(range_f0 ~ (1|participant) + (1|text), data=data_pitch, REML=FALSE)
m1 <- lmer(range_f0 ~ group + (1|participant) + (1|text), data=data_pitch, REML=FALSE)
m2 <- lmer(range_f0 ~ group +  phase + (1|participant) + (1|text), data=data_pitch, REML=FALSE) 
m3 <- lmer(range_f0 ~ group + phase + phase:group + (1|participant) + (1|text), data=data_pitch, REML=FALSE)
anova(m0,m1)
anova(m1,m2)
anova(m2,m3)

  # duration analysis 
m4 <- lmer(duration ~ (1|participant) + (1|text), data=data_duration, REML=FALSE)
m5 <- lmer(duration ~ group + (1|participant) + (1|text), data=data_duration, REML=FALSE)
m6 <- lmer(duration ~ group + phase + (1|participant) + (1|text), data=data_duration, REML=FALSE)
m7 <- lmer(duration ~ group + phase + group:phase + (1|participant) + (1|text), data=data_duration, REML=FALSE)
anova(m4,m5)
anova(m5,m6)
anova(m7,m8)

# behavior analysis 
  # eye gaze
m8 <- lm (towards_robot ~ participant, data=data_behavior)
m9 <- lm (towards_robot ~ phase + participant, data=data_behavior)
m10 <- lm (towards_robot ~ phase + group + participant, data=data_behavior)
m11 <- lm (towards_robot ~ phase + group + participant + phase:group, data=data_behavior)
anova(m8,m9)
anova(m9,m10)
anova(m10,m11)

  # facial expression 
m12 <- lm (positive ~ participant, data=data_behavior)
m13 <- lm (positive ~ phase + participant, data=data_behavior)
m14 <- lm (positive ~ phase + group + participant, data=data_behavior)
m15 <- lm (positive ~ phase + group + participant + phase:group, data=data_behavior)
anova(m01,m11)
anova(m11,m21)
anova(m21,m31)

#-OPTIONAL- splitting datafiles for more detailed inspection 
data_split <- split(data_behavior, data$phase)
data_b_intro <- data_split$'1'
data_b_other <- data_split$'0'

mintro <- lm(towards_robot ~ group + participant, data=data_b_intro)
mother <- lm(towards_robot ~ group + participant, data=data_b_other)
summary(mintro)
summary(mother)

#STEP 4: obtain means\standard deviations and make plots using the following functions
mean()
sd()

plot(data_behavior$group, data_behavior$towards_robot, xlab="group", ylab="gaze towards robot") 
boxplot(towards_robot ~ phase, data=data_behavior, xlab = 'phase', ylab='gaze towards robot')
