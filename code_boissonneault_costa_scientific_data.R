#READ ME########################################################################
  #This R dofile contains code that allows to recreate the work described in 
  #'Experts’ assessments of migration scenarios between the Middle East & North 
  #Africa and Europe' by Michaël Boissonneault and Rafael Costa.
  #It is divided in two parts:
    #1.Survey presentation
      #code to produce the vignettes and graphs used in the survey (see method section of the paper)
    #2.Technical validation
      #code to produce the figures and tables included in the paper (see technical validation section of the paper)
#PACKAGES#######################################################################
rm(list=ls())

#read in packages
library(tidyverse)
library(openxlsx)
library(lme4)
library(AlgDesign)
library(RColorBrewer)
library(ggpubr)

################################################################################
#PART 1: SURVEY PRESENTATION
################################################################################
#Specification of the vignette parameters#######################################
#number of factors
factor_nb <- 7

#number of levels per factor
level_nb <- 2

#number of distinct vignettes
vignette_nb <- level_nb^factor_nb

#number of vignettes per block
vignettes_block <- 4

#number of blocks
block_nb <- vignette_nb / vignettes_block

#factors 
factors <- c("A","B","C","D","E","F","G")

#generate factorial design
dat <- gen.factorial(level_nb,factor_nb,varNames=factors)

#generate blocks
dat <- optBlock(~ A + B + C + D + E + F + G + 
                  A:B + A:C + A:D + A:E + A:F + A:G +
                  B:C + B:D + B:E + B:F + B:G +
                  C:D + C:E + C:F + C:G +
                  D:E + D:F + D:G +
                  E:F + E:G + 
                  F:G,
                withinData=dat, blocksizes=rep(vignettes_block,block_nb))

#Checking the confounding/aliasing structure#################################### 
dsgn <- dat$design                                         #extract the partition (design)
id <- paste(dat$rows)                                      #vignette ID
set <- as.factor(rep(1:block_nb, each = vignettes_block))  #generate a set indicator

#create a data frame
dat <- data.frame(id, dsgn, set)  

#transform dat$id into factor
dat$id <- as.factor(dat$id)

#generate a random outcome variable for checking the confounding structure in a saturated linear model
dat$y <- rnorm(length(id))

#estimation of the linear model
out.lm <- lm(y ~ set + 
               A*B*C*D*E*F*G,
             dat, x = TRUE)

#illustration of the results
result <- round(data.frame(alias(out.lm)$Complete),2)
rownames(result)
nchar(rownames(result))-floor(nchar(rownames(result))/2)

#only higher level interactions (including 4 or more dimensions) are confounded
#analyses including 3 or fewer interactions will not be confounded

#Generating the vignettes#######################################################
#Dimensions A & B: Demography
#MENA
dat <- mutate(dat,A=case_when(
  A==1 ~ "The proportion of young people has increased as women have been having more children.",
  A==-1 ~ "The proportion of young people has decreased as women have been having fewer children."
))

#EU
dat <- mutate(dat,B=case_when(
  B==1 ~ "The increase in the proportion of older people has accelerated as lifespans have been strongly increasing.",
  B==-1 ~ "The increase in the proportion of older people has slowed down as lifespans have been stagnating."
))

#Dimensions C & D: Culture
#MENA
dat <- mutate(dat,C=case_when(
  C==1 ~ "Religious fundamentalism has gained ground.",
  C==-1 ~ "Religious fundamentalism has lost ground."
))

#EU
dat <- mutate(dat,D=case_when(
  D==1 ~ "People have become more favorable to immigration.",
  D==-1 ~ "People have become less favorable to immigration."
))

#Dimension E & F: Politics
#MENA
dat <- mutate(dat,E=case_when(
  E==1 ~ "Countries have become more politically stable.",
  E==-1 ~ "Countries have become less politically stable."
))

#EU
dat <- mutate(dat,F=case_when(
  F==1 ~ "Immigration policies have become more restrictive.",
  F==-1 ~ "Immigration policies have become less restrictive."
))

#Dimension G: Economony
#MENA & EU
dat <- mutate(dat,G=case_when(
  G==1 ~ "Unemployment rates have reached much higher levels compared to Europe.",
  G==-1 ~ "Unemployment rates have reached similar levels compared to Europe."
))

#Create different series
dat <- bind_rows(dat,dat)
dat$series <-rep(1:2,each=128)

#give number 
dat$number <- rep(1:64,each=4)

#make the vignettes
dat$vignette <- ifelse(dat$series==1|dat$series==3|dat$series==5|dat$series==7|dat$series==9|dat$series==11,
                       paste("During the period 2021-2030,","In the Middle East & North Africa,",dat$A,dat$C,dat$E,dat$G,"In Europe,",dat$B,dat$D,dat$F,sep="\n"),
                       paste("During the period 2021-2030,","In Europe,",dat$B,dat$D,dat$F,"In the Middle East & North Africa,",dat$A,dat$C,dat$E,dat$G,sep="\n")
)

#show randomly chosen vignette
dat$vignette[round(runif(1, 1, 256))]

#Graphs in survey: example######################################################
  #Numbers are provided for illustration purposes only and may not correspond to
  #the actual data. Please refer to the reference provided in the article 
  #for the data used in the survey
  ##############################################################################
#data
pasttrends <- data.frame(year = 2010:2019, migrants = c(95, 83, 72, 69, 71, 85, 88, 97, 95, 90))

#number in final year
mig19 <- filter(pasttrends, year==2019)$migrants

#max value in graph
max <- round(mig19*5/100)*100

#jumps in scale
by <- floor(max/5/10)*10

#scale
scale <- c(NA,5,3,2,1.5,1.25,1,0.8,0.667,0.5,0.333,0.2)
  
#plot (pick random value from the scale for the future trend)
ggplot(pasttrends)+
  geom_line(aes(year, migrants), size=1.2)+
  geom_segment(aes(x=2030, y=mig19*scale[round(runif(1, 1, 12))],
                   xend=2021, yend=mig19),
               linetype="dotted", size=1.2)+
  ylab("Number of migrants (in thousands)")+
  xlab("Year")+
  theme_minimal()+
  scale_y_continuous(breaks = seq(0,max,by))+
  scale_x_continuous(breaks = seq(2010,2030,5))+
  geom_segment(aes(x=2010, xend=2010, y=-by/5, yend=max),color="dark grey")+
  geom_segment(aes(x=2009, xend=2030, y=0, yend=0), color="dark grey")
  
################################################################################
#PART 2: TECHNICAL VALIDATION
################################################################################
#Table 3: respondents' background & mean assessments############################
#load data
il <- read.xlsx("https://zenodo.org/record/7404130/files/QMsurvey_respondent.xlsx?download=1")
vl <- read.xlsx("https://zenodo.org/record/7404130/files/QMsurvey_respondentvignette.xlsx?download=1")

#recode years of experience into categorical variable
il <- il %>% mutate(years_exp = case_when(
  years_exp < 10 ~ "[0;10[",
  years_exp >=10 & years_exp<20 ~ "[10;20[",
  years_exp >=20 ~ "[20;50]" ,
  is.na(years_exp) ~ "[20;50]"
))

#put the one with secondary education with the bachelor's
il <- il %>% 
  mutate(edu = ifelse(edu == 'Secondary' | edu == "Bachelor's", "Sec./Bachelor's", edu))

#combine the two datasets
combi <- vl %>% 
  select(id, rank, family:compact) %>%
  left_join(il %>% select(-comment, -sector_other))

#make separate datasets for each individual-level variable
combi_list <- lapply(8:12, function(x) combi %>% select(c(1:7, x, 13:15)) %>% rename(var = 8))

#summarise info by var
bind_rows(lapply(1:5, function(x) 
  combi_list[[x]] %>% 
    group_by(var) %>%
    summarise(n = paste(n()/4, " (", round(n()/4/length(il$id)*100, 1), ")", sep=""), 
              nochange = paste(round(mean(mig_nochange), 2), " (", round(sd(mig_nochange), 2), ")", sep=""),
              certain = paste(round(mean(certain), 2), " (", round(sd(certain), 2), ")", sep=""),
              compact_nochange = paste(round(mean(compact_nochange), 2), " (", round(sd(compact_nochange), 2), ")", sep=""),
              family = paste(round(mean(family), 2), " (", round(sd(family), 2), ")", sep=""),
              work = paste(round(mean(work), 2), " (", round(sd(work), 2), ")", sep=""),
              refugees = paste(round(mean(refugees), 2), " (", round(sd(refugees), 2), ")", sep=""),
              return = paste(round(mean(return), 2), " (", round(sd(return), 2), ")", sep=""),
              compact = paste(round(mean(compact), 2), " (", round(sd(compact), 2), ")", sep="")))) %>%
  rbind(c("Total", length(il$id), 
  apply(combi %>% select(mig_nochange, certain, compact_nochange, family, work, refugees, return, compact), 2, function(x) paste(round(mean(x), 2), " (", round(sd(x), 2), ")", sep=""))))

#Figure 2: mean assessments, range, past trends#################################
#pivot vl data to long
vl_long <- vl %>% 
  pivot_longer(family:return, names_to = "flowtype", values_to = "assessment") 

#past trends
past <- read_csv("https://raw.githubusercontent.com/michaelboissonneault/quantmigsurveycode-eurostatdata/main/migr_resfas_reduced.csv") %>%
  filter(year<=2019) %>%
  bind_rows(data.frame(reason = "RET", year = 2010:2019, migrants = c(125, 130, 150, 155, 170, 182, 195, 188, 185, 198), countries = NA)) %>%
  mutate(flow = case_when(
    reason=="EMP" ~ "work",
    reason=="FAM" ~ "family",
    reason=="OTH" ~ "refugees",
    reason=="RET" ~ "return"
  )) %>% ungroup() %>% select(-reason, -countries)

#fixed effects
m <- vl_long %>% 
  group_by(flowtype) %>% 
  do(model = lmer(assessment ~ young + fundament + stable + employment + old + favourable + policies + (1|id), data =. ))

#predict at max and min (polygon df)
poly <- bind_rows(lapply(1:4, function(x) 
  data.frame(flow = c('family', 'refugees', 'return', 'work')[x],
             y = c(sum(coef(summary(m$model[[x]]))[,1]), coef(summary(m$model[[x]]))[1,1]),
             x = 2030))) 

#add value in 2019 (for polygon in 2021)
poly <- poly %>% 
  left_join(past %>% filter(year==2019) %>% rename(value19 = migrants) %>% select(-year)) %>%
  bind_rows(past %>% filter(year==2019) %>% rename(y = migrants) %>% mutate(x = 2021) %>% select(-year)) %>%
  mutate(y = ifelse(x==2030, y*value19, y)) %>% 
  arrange(flow) 

#medians
poly <- poly %>% 
  left_join(poly %>% filter(x==2030) %>% group_by(flow) %>% summarise(median = mean(y)))

#put together with past trends
viz <- bind_rows(poly, past %>% rename(x = year))

#change flow variable
viz <- viz %>% 
  mutate(flow = factor(
    case_when(
      flow == 'family' ~ "Family",
      flow == 'work' ~ "Work",
      flow == "refugees" ~ "Refugees",
      flow == "return" ~ "Return"),
    levels = c("Family", "Work", "Refugees", "Return")))
    
#make viz
ggplot(viz)+
  facet_wrap(~flow, scales = 'free_y')+
  theme_bw()+
  geom_line(aes(x = x, y = migrants))+
  geom_segment(aes(x = 2021, xend = 2030, y = value19, yend=median), linetype = 'dashed')+
  geom_polygon(aes(x = x, y = y, group = flow), alpha=0.2)+
  expand_limits(y = 0)+
  xlab("Year")+
  ylab("Number of migrants (in thousands)")

#Table 4: Effect sizes, each single dimension###################################
#fixed effects, logarithmic
m <- vl_long %>% 
  group_by(flowtype) %>% 
  do(model = lmer(log(assessment) ~ young + fundament + stable + employment + old + favourable + policies + (1|id), data =. ))

#compact
m_c <- lmer(compact ~ young + fundament + stable + employment + old + favourable + policies + (1|id), data = vl)

#make table
bind_rows(lapply(1:4, function(x) 
  data.frame(flow = c('family', 'refugees', 'return', 'work')[x],
             variable = c("Intercept", "Younger", "GainFundament.", "LessStable", "Diverg.Empl.", "Older", "MoreFavor.", "LessStrictPol."),
             estimate = coef(summary(m$model[[x]]))[,1],
             stderror = coef(summary(m$model[[x]]))[,2]))) %>%
  mutate(estimate = round(estimate, 3),
         stderror = round(stderror, 3),
         est_st = paste(estimate, " (", stderror, ")", sep="")) %>%
  select(-estimate, -stderror) %>%
  pivot_wider(values_from = est_st, names_from = flow) %>%
  left_join(data.frame(variable = c("Intercept", "Younger", "GainFundament.", "LessStable", "Diverg.Empl.", "Older", "MoreFavor.", "LessStrictPol."),
                       estimate = coef(summary(m_c))[,1],
                       stderror = coef(summary(m_c))[,2]) %>%
              mutate(estimate = round(estimate, 3), stderror = round(stderror, 3), compact = paste(estimate, " (", stderror, ")", sep="")) %>%
              select(-estimate, -stderror))

#Table 6: analysis of variance##################################################
#assessment values to ordinal scale
vl <- vl %>% 
  pivot_longer(family:return, names_to = "flowtype", values_to = "assessment") %>% 
  mutate(assessment = case_when(
    assessment == 1/5 ~ -5,
    assessment >1/5 & assessment<1/2 ~ -4,
    assessment == 1/2 ~ -3,
    assessment >1/2 & assessment<4/5 ~ -2,
    assessment == 1/1.25 ~ -1,
    assessment == 1 ~ 0,
    assessment == 1.25 ~ 1,
    assessment == 1.5 ~ 2,
    assessment == 2 ~ 3,
    assessment == 3 ~ 4,
    assessment == 5 ~ 5)) %>% 
  pivot_wider(names_from = flowtype, values_from = assessment)

#data frame, variance explained
data.frame(flow = rep(c("Family", "Work", "Refugees", "Return", "Compact"), each=2),
           group = rep(c("ID", "Residual"), 5),
           value = c(as.data.frame(VarCorr(lmer(family ~ (1 | id), vl)))$vcov,
                     as.data.frame(VarCorr(lmer(work ~ (1 | id), vl)))$vcov,
                     as.data.frame(VarCorr(lmer(refugees ~ (1 | id), vl)))$vcov,
                     as.data.frame(VarCorr(lmer(return ~ (1 | id), vl)))$vcov,
                     as.data.frame(VarCorr(lmer(compact ~ (1 | id), vl)))$vcov)) %>% 
  pivot_wider(values_from = value, names_from = group) 
