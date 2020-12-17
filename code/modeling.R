# =============================================================================
# Program Author: Jaeok Kim
# Start Date: Dec 10, 2020
# Update Date: Dec 17, 2020
# Content: Policy Modeling
# Notes: Prison Modeling
# =============================================================================

* START ORG/ESS/LaTeX SESSION
#+begin_src emacs-lisp :results silent
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (latex . t)
     (python . t)
     (shell . t)))
#+end_src
** Set org-mode confirmation of evaluation nil (Turn Off)
#+begin_src emacs-lisp :results silent
  ;; DANGER!
  ;; Do not Use Unless You Really, Really Mean It
  (setq org-confirm-babel-evaluate nil)
#+end_src

** Set org-mode confirmation of evaluation TRUE (Turn On)
#+begin_src emacs-lisp :results silent
  (setq org-confirm-babel-evaluate t)
#+end_src


* Load Package  ---------------------------------------------------------------------------
#+begin_src R :session :results silent

# .libPaths(c("/Documents/R",.libPaths()))


library(aws.s3)
library(reshape2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(openxlsx)
library(tibble)
library(foreign)
library(plyr)
library(stringr)
library(dtplyr)
library(data.table)
library(ggplot2)
library(Hmisc)
library(psych)
library(eeptools)
library(zoo)
library(ggrepel)
library(devtools)
library(ggpubr)
library(readxl)


# options(scipen=999)

#+end_src


* Setup Path and Read-in Data --------------------------------------------------------------
#+begin_src R :session :results silent

setwd("~/OneDrive - Vera Institute of Justice/GitHub/policy-modeling/")

# LOAD ORIGINAL DATA FROM S3

## Snapshot 2020-02-14
## "2020_02_14_DOCCS_current_clean.csv"

df.current <- s3read_using(FUN=read.csv,
                   object="2020_02_14_DOCCS_current_clean.csv",
		   bucket="vera-gjny/curated_data/ny_prison/in_custody/2020_02")

df.current <- df.current %>%
   mutate(id= seq(1, nrow(.), by=1))

## 2018 Data
doccs2018 <- read.csv("Data/DOCCSJan2018.csv")

rel.dist <- read.csv("Data/rel.dist.15yrs.csv")
rel.dist <- rel.dist[,-1]

## 2020 October
## "2020_10_01_DOCCS_current_clean.csv"

df.oct <- s3read_using(FUN=read.csv,
                   object="2020_10_01_DOCCS_current_clean.csv",
		   bucket="vera-gjny/curated_data/ny_prison/in_custody/2020_10")


#+end_src


* Projecting Future Population
#+begin_src R :session :results silent


## Projection of Overall Population
## 2010-2017: Pull the data from DOCC Annual Reports
## 2018: DOCCS Scraped Snapshot Data 
## 2019: Linear Interpolation between 2018 and 2020
## 2020: DOCCS Scraped Sanapshot Data (2020-02-14)

year <- seq(2010, 2020, by=1)
pop <- c(58378, 56315, 55979, 54865, 54141, 53103, 51744, 51466, 48932, 46244, 43555)

## Admission by Type from DOCCS Admission Reports 2010-2016

adm.total <- c(24374, 24116, 23738, 23317, 22516, 21483, 21675, NA, NA, NA, NA)
adm.new <- c(14754, 14486, 14089, 13692, 13271, 12633, 12878, NA, NA, NA, NA)
adm.tpv	<- c(7484, 7385, 7459,	7328,	6602,	6396,	6339, NA, NA, NA, NA)
adm.atp <- c(1524, 1754, 1805,	1940,	1888,	1830,	2058, NA, NA, NA, NA)
adm.other <- c(612 ,481, 385,	357,	755,	624,	405, NA, NA, NA, NA)


rel.total <- c(26397, 25327,	24935,	23989,	13544,	22354,	22628,  NA, NA, NA, NA)
rel.par <- c(8998, 8323,8335,	8045,	7971,	7492,	7458, NA, NA, NA, NA)
rel.cr <- c(11073, 10773,	10326,	9910,	9457,	9051,	8834, NA, NA, NA, NA)
rel.atp <- c(1418, 1708, 1830,	1833,	1959,	1716,	1981, NA, NA, NA, NA)
rel.max.exp <- c(2863, 22664,	2691,	2533,	2434,	2303,	2369, NA, NA, NA, NA)
rel.max.exp.prs <- c(1281,	1227,	1164,	1105,	1128,	1232,	1479, NA, NA, NA, NA)
rel.other <- c(764, 632, 589,	563,	595,	560,	607, NA, NA, NA, NA)


var <- list(year, pop,
            adm.total, adm.new, adm.tpv, adm.atp, adm.other,
            rel.total, rel.par, rel.cr, rel.atp, rel.max.exp, rel.max.exp.prs, rel.other)

report <- data.frame(var)

colnames(report) <- NA

var.name <-c("year", "pop",
             "adm.total", "adm.new", "adm.tpv", "adm.atp", "adm.other",
             "rel.total", "rel.par", "rel.cr", "rel.atp", "rel.max.exp",
	     "rel.max.exp.prs", "rel.other")

colnames(report) <- var.name

# Lag Variable

tmp <- report %>%
   mutate(pop.lag1 = dplyr:: lag(pop, n=1, default=NA),
          adm.lag1 = dplyr:: lag(adm.total, n=1, default=NA),
          rel.lag1 = dplyr:: lag(rel.total, n=1, default=NA),
          delta.pop=(pop-pop.lag1)/pop.lag1)

tmp <- t(tmp)

colnames(tmp) <- paste0("y", seq(2010, 2020, by=1))

agg  <- data.frame(tmp, var=rownames(tmp))

## Estimate Population Future Total Population 
x <- agg %>% filter(var=="pop") %>%
mutate(weighted.avg=(y2016*1+ y2017*1 + y2018*2 + y2019*2+ y2020*3)/9)

weighted.avg.pop <- x$weighted.avg

x <- agg %>% filter(var=="delta.pop") %>%
mutate(weighted.delta=((y2017*1 + y2018*2 + y2019*2 + y2020*3)/8)*4)

weighted.delta.pop <- x$weighted.delta


N2030 <- weighted.avg.pop*(1+weighted.delta.pop)
N2020 <- agg[agg$var=="pop","y2020"]

d <- (N2030-N2020)/10

proj.pop <-round(seq(N2020+d, N2030, by=d))
new.pop <- c(pop, proj.pop)

year<-seq(2010, 2030, by=1)

df20<- data.frame(year,  new.pop)

## Projecting Share of Technical Parole Violation
# Calculate N population for technical parole violation
# Datset to be replaced with a simler version

Ns <- read.csv("data/Ns.csv")

tmp <-Ns[1:2, -24]
tmp <- data.frame(t(tmp))
tmp <- tmp[-1,]

year <- seq(1995, 2016, by=1)

tmp <- data.frame(tmp, year)

tmp <-
tmp %>% select(year, original=X1, pol1=X2) %>%
mutate(n.TPV=original-pol1,
       prop.TPV=(original-pol1)/original) %>%
filter(year>=2010)

## technical parole violation+ parole with a  new sentence
## DOCCS SSCARPE DATA DO NOT DIFFEERENTIATE BETWEEN TECHNICAL PAROLE VIOLATION
## ADN TECHNICAL PAROLE VIOLATION WITH A NEW SENTENCE
## just aapply the proportion of TPV in the past


## Feb 2020

x <- ds %>% select(adm.type) %>% table() %>% data.frame() %>%
filter(.=="Parole/Cond.Rel Revoc") %>% select(Freq)

tpv.2020<- x$Freq

# Lag Variable

tmp2 <- tmp %>%
   mutate(pop.lag1 = dplyr:: lag(original, n=1, default=NA),
          TPV.lag1 = dplyr:: lag(n.TPV, n=1, default=NA),
          delta.TPV=(n.TPV-TPV.lag1)/TPV.lag1)

tmp2 <- t(tmp2)

colnames(tmp2) <- paste0("y", seq(2010, 2016, by=1))

agg.tpv  <- data.frame(tmp2, var=rownames(tmp2))

## Estimate Population
x <- agg.tpv %>% filter(var=="n.TPV") %>%
mutate(weighted.avg=(y2012*1+ y2013*1 + y2014*2 + y2015*2+ y2016*3)/9)

weighted.avg.tpv <- x$weighted.avg

x <- agg.tpv %>% filter(var=="delta.TPV") %>%
mutate(weighted.delta=((y2013*1 + y2014*2 + y2015*2 + y2016*3)/8)*4)

weighted.delta.tpv <- x$weighted.delta


N2026.tpv <- weighted.avg.tpv*(1+weighted.delta.tpv)
N2016.tpv <- agg.tpv[agg.tpv$var=="n.TPV","y2016"]


d <- (N2026.tpv-N2016.tpv)/10

proj.pop.tpv <-round(seq(N2016.tpv+d, N2016.tpv+d*14, by=d))


x <- agg.tpv[agg.tpv$var=="n.TPV",1:7]
x <- t(x)
n.TPV <- x[1:7]

new.pop.tpv <- c(n.TPV, proj.pop.tpv)

year <-seq(2010, 2030, by=1)

df20.tpv<- data.frame(year,  new.pop.tpv)

x <- merge(df20, df20.tpv, by="year")

write.csv(x, "data/doccs.pop2030.csv")


#+end_src

* Prospective Impact Estimation ------------------------------------------------------------
** Parole Reform
*** DONE Reform#1: No Admission for Technical Parole Violation (reform1)
#+begin_src R :session :results silent

# Reform#1: No Admission for Technical Parole Violation
# Identify those who were admitted for TPV between 2009-2016
# Create admission and release dates for an alternate scenario
p1a <- ds %>%
   mutate(
      p1.elig=ifelse(adm.type=="Parole/Cond.Rel Revoc",1,0),
      p1.elig.yr=ifelse(p1.elig==1 & year(date.adm)>=2021, year(date.adm), # Admission after 2021
                 ifelse(p1.elig==1 & year(date.adm)<2021, 2021, NA)), # Admission before 2021
      p1.date.adm=ifelse(p1.elig==1 & year(date.adm)>=2021, NA, date.adm),
      p1.date.rel=ifelse(p1.elig==1 & year(date.adm)>=2021, NA,
                  ifelse(p1.elig==1 & year(date.adm)<2021,
                         as.Date("2021-01-01", format="%Y-%m-%d"),
                         NA)),
      p1.date.adm=as.Date(p1.date.adm, format="%Y-%m-%d"),
      p1.date.rel=as.Date(p1.date.rel, format="%Y-%m-%d"))

reform1a <- p1a %>%
   select(id, p1.elig:p1.date.rel) %>%
   select(id, p1.elig, p1.elig.yr, p1.date.adm, p1.date.rel) %>%
   dplyr::rename(p1a.elig=p1.elig,
                 p1a.elig.yr=p1.elig.yr,
		 p1a.date.adm=p1.date.adm,
		 p1a.date.rel=p1.date.rel)

p1a.elig.rel.N <- p1a %>% filter(p1.elig==1) %>% select(p1.elig.yr) %>% table(useNA="always") %>% addmargins()


#+end_src

*** DONE Reform#2: Elderly parole release (reform2)
#+begin_src R :session :results silent

# Reform#2: Elderly parole release

## EXPLANATION OF ESTIMATING THE IMPACT OF ELDERLY PAROLE RELEASE

## IDENTIFY THOSE WHO WOULD 'BENEFIT' FROM THE REFORM
## APPLY DISTRIBUTION OF PAROLE RELEASE IN A SIMPLISTIC FORM
## LOOK AT THE DISTRIBUTION OF PAROLE RELEASE
##      - WHAT'S THE AVERAGE YEARS IT TAKES FOR INDIVIDUAL BEFORE RELEASE
## ESTIMATE PROJECTED RELEASE DATES BASED ON THE EXISTING PATTERNS
## EMPRICIAL EVIDENCE ON THE RELEASE RATE


# What does the distribution of release look like for those who served 15+ years ?  (rel.dist)
### For those who were on indeterminate sentence and were released,
### how long does it usually take before they are released?
### Those who were on indeterminate sentence AND
### were released to community (exclude death,transfer,other,etc) AND
### served 15 years or longer
### Exclude who were released before parole eligible date
### Also Exclude those who had parole eligible date before new date based on the policy
### since they are not relevant to the elderly parole release


## Scenario #1: Use the parole grant rate for those who went to parole board after serving 15 years for indeterminate sentence

# Identify those who meet the criteria each year:
## Age 55+ AND Served 15+ years AND not yet reached parole eligible date AND not yet released or

set.seed(4634)

prob<- as.vector(rel.dist[,2])
yrs <- as.vector(rel.dist[,1])

p2a <- ds %>%
   mutate(
      date.age55=as.Date(DOB+55*365), # date when age 55
      date.served15=as.Date(DOB+15*365), # date when served 15 yrs
      date.elig2=as.Date(ifelse(date.age55>date.served15, date.age55, date.served15)), # earlest date meeting both criteria
      p2.elig=ifelse(adm.type!="New Commitment", 0, # exclude anyone not serving a sentence on commitment crime
              ifelse(str_detect(sent.type, "Indeterminate")==FALSE, 0, # exclude if not indeterminate
              ifelse(date.elig2<par.elig.date, 1, 0))),  # available for earlier parole under this reform
      p2.elig.yr=ifelse(p2.elig==1 & year(date.elig2)<2021, 2021,
                 ifelse(p2.elig==1, year(date.elig2), NA)),
      date.elig2=ifelse(p2.elig==1 & year(date.elig2)<2021, ymd("2021-01-01"), date.elig2),
      date.elig2=as.Date(date.elig2, format="%Y-%m-%d"),
      p2.proj.yrs = ifelse(p2.elig==1,
                           sample(yrs, size=n(), prob=prob, replace=TRUE),
			   NA),
      p2.date.adm= date.adm,
      p2.date.rel= ifelse(p2.elig==1 & date.elig2+years(p2.proj.yrs)<cond.rel.date,
                          date.elig2+years(p2.proj.yrs),
                   ifelse(p2.elig==1 & date.elig2+years(p2.proj.yrs)<max.exp.date,
                          date.elig2+years(p2.proj.yrs),
		   ifelse(!is.na(cond.rel.date), cond.rel.date,
                   ifelse(!is.na(max.exp.date), max.exp.date, NA)))),
      p2.date.rel=ifelse(p2.elig==1 & is.na(p2.date.rel), date.elig2+years(p2.proj.yrs),
                         p2.date.rel),
      p2.date.rel=as.Date(p2.date.rel, format="%Y-%m-%d"),
      p2.rel.type = ifelse(p2.elig==1 & date.elig2+years(p2.proj.yrs)<cond.rel.date,
                          "cond:date.elig2+years(p2.proj.yrs)",
                   ifelse(p2.elig==1 & date.elig2+years(p2.proj.yrs)<max.exp.date,
                          "max:date.elig2+years(p2.proj.yrs)",
		   ifelse(!is.na(cond.rel.date), "cond.rel.date",
                   ifelse(!is.na(max.exp.date), "max.exp.date", NA)))),
      p2.rel.type = ifelse(p2.elig==1 & is.na(p2.rel.type),
                            "missing:date.elig2+years(p2.proj.yrs)", p2.rel.type))%>%
      mutate(p2.adm.yr=year(p2.date.adm),
             p2.rel.yr=year(p2.date.rel))

reform2a <- p2a %>%
      select(id, p2.elig, p2.elig.yr, p2.date.adm, p2.date.rel) %>%
      dplyr::rename(p2a.elig=p2.elig,
                    p2a.elig.yr=p2.elig.yr,
		    p2a.date.adm=p2.date.adm,
		    p2a.date.rel=p2.date.rel)

p2a.elig.rel.N <- p2a %>%
   filter(p2.elig==1 &str_detect(p2.rel.type, "p2\\.proj\\.yrs")) %>%
   select(p2.rel.yr) %>% table(useNA="always") %>% addmargins() %>% data.frame() %>%
   dplyr::rename(p2a= Freq)


## Scenario #2: Use the overall parole grant rate for indeterminate (release rate doubled)
prob<- as.vector(rel.dist[,2])
yrs <- as.vector(rel.dist[,1])

prob <- prob*2
index <- which(cumsum(prob)>1)
prob[index[1:length(index)]]<-0
prob[index[1]] <- 1-sum(prob)

yrs<- yrs[1:length(prob)]

# Identify those who meet the criteria each year:
## Age 55+ AND Served 15+ years AND not yet reached parole eligible date AND not yet released or

## Random assignment of release rate
## Change projected release date with the maximum expiration date, if projected release date is later than maximum expiration date

set.seed(4634)

p2b <- ds %>%
   mutate(
      date.age55=as.Date(DOB+55*365), # date when age 55
      date.served15=as.Date(DOB+15*365), # date when served 15 yrs
      date.elig2=as.Date(ifelse(date.age55>date.served15, date.age55, date.served15)), # earlest date meeting both criteria
      p2.elig=ifelse(adm.type!="New Commitment", 0, # exclude anyone not serving a sentence on commitment crime
              ifelse(str_detect(sent.type, "Indeterminate")==FALSE, 0, # exclude if not indeterminate
              ifelse(date.elig2<par.elig.date, 1, 0))),  # available for earlier parole under this reform
      p2.elig.yr=ifelse(p2.elig==1 & year(date.elig2)<2021, 2021,
                 ifelse(p2.elig==1, year(date.elig2), NA)),
      date.elig2=ifelse(p2.elig==1 & year(date.elig2)<2021, ymd("2021-01-01"), date.elig2),
      date.elig2=as.Date(date.elig2, format="%Y-%m-%d"),
      p2.proj.yrs = ifelse(p2.elig==1,
                           sample(yrs, size=n(), prob=prob, replace=TRUE),
			   NA),
      p2.date.adm= date.adm,
      p2.date.rel= ifelse(p2.elig==1 & date.elig2+years(p2.proj.yrs)<cond.rel.date,
                          date.elig2+years(p2.proj.yrs),
                   ifelse(p2.elig==1 & date.elig2+years(p2.proj.yrs)<max.exp.date,
                          date.elig2+years(p2.proj.yrs),
		   ifelse(!is.na(cond.rel.date), cond.rel.date,
                   ifelse(!is.na(max.exp.date), max.exp.date, NA)))),
      p2.date.rel=ifelse(p2.elig==1 & is.na(p2.date.rel), date.elig2+years(p2.proj.yrs),
                         p2.date.rel),
      p2.date.rel=as.Date(p2.date.rel, format="%Y-%m-%d"),
      p2.rel.type = ifelse(p2.elig==1 & date.elig2+years(p2.proj.yrs)<cond.rel.date,
                          "cond:date.elig2+years(p2.proj.yrs)",
                   ifelse(p2.elig==1 & date.elig2+years(p2.proj.yrs)<max.exp.date,
                          "max:date.elig2+years(p2.proj.yrs)",
		   ifelse(!is.na(cond.rel.date), "cond.rel.date",
                   ifelse(!is.na(max.exp.date), "max.exp.date", NA)))),
      p2.rel.type = ifelse(p2.elig==1 & is.na(p2.rel.type),
                            "missing:date.elig2+years(p2.proj.yrs)", p2.rel.type))%>%
      mutate(p2.adm.yr=year(p2.date.adm),
             p2.rel.yr=year(p2.date.rel))

reform2b <- p2b %>%
      select(id, p2.elig, p2.elig.yr, p2.date.adm, p2.date.rel) %>%
      dplyr::rename(p2b.elig=p2.elig,
                    p2b.elig.yr=p2.elig.yr,
		    p2b.date.adm=p2.date.adm,
		    p2b.date.rel=p2.date.rel)

p2b.elig.rel.N <- p2b %>%
   filter(p2.elig==1 &str_detect(p2.rel.type, "p2\\.proj\\.yrs")) %>%
   select(p2.rel.yr) %>% table(useNA="always") %>% addmargins() %>% data.frame() %>%
   dplyr::rename(p2b= Freq)


#+end_src

*** DONE Reform#3: Fair and Timely Parole (reform3) - Presumptive Release
#+begin_src R :session :results silent

## EXPLANATION OF ESTIMATING THE IMPACT OF PRESUMPTIVE RELEASE

## WHAT DOES PRESUMPTIVE RELEASE LOOK LIKE?
## START WITH THE MOST PROGRESSIVE REFORM

## Scenario #1:
## THE MOST PROGRESSIVE REFORM -- 100% RELEASE AT THE EARLIEST PAROLE DATE(Unrealistic)

p3a <- ds %>%
   mutate(
      date.elig3=ifelse(adm.type=="New Commitment" & str_detect(sent.type, "Indeterminate"), par.elig.date, NA),
      date.elig3= as.Date(date.elig3, format="%Y-%m-%d"),
      p3.elig=ifelse(adm.type!="New Commitment", 0, # Exclude anyone not serving a sentence
              ifelse(str_detect(sent.type, "Indeterminate") & !is.na(date.elig3), 1,
              0)),
      p3.elig.yr=ifelse(p3.elig==1 & year(date.elig3)>=2021, year(date.elig3),
                 ifelse(p3.elig==1 & year(date.elig3)<2021, 2021, NA)),
      date.elig3=ifelse(p3.elig==1 & year(date.elig3)<2021, as.Date("2021-01-01", format="%Y-%m-%d"),
                        date.elig3),
      date.elig3=as.Date(date.elig3, format="%Y-%m-%d"),
      p3.date.adm=date.adm,
      p3.date.rel=ifelse(p3.elig==1 & year(date.elig3)<2021,
                         as.Date("2021-01-01", format="%Y-%m-%d"), # Anyone eligible for parole before 2021 be released in 2021
                  ifelse(p3.elig==1 & date.elig3<cond.rel.date, date.elig3,
		  ifelse(p3.elig==1 & date.elig3>cond.rel.date, cond.rel.date,
                  ifelse(!is.na(cond.rel.date), cond.rel.date,
                  ifelse(!is.na(max.exp.date), max.exp.date, NA))))),
      p3.date.rel=ifelse(p3.elig==1 & is.na(p3.date.rel), date.elig3, p3.date.rel),
      p3.date.rel=as.Date(p3.date.rel, format="%Y-%m-%d"),
      p3.rel.type=ifelse(p3.elig==1 & year(date.elig3)<2021,
                         "2021-01-01", # Anyone eligible for parole before 2021 be released in 2021
                  ifelse(p3.elig==1 & date.elig3<cond.rel.date,
                         "(date.elig3<cond.rel.date)date.elig3",
		  ifelse(p3.elig==1 & date.elig3>cond.rel.date, "(date.elig3>cond.rel.date)cond.rel.date",
                  ifelse(!is.na(cond.rel.date), "cond.rel.date",
                  ifelse(!is.na(max.exp.date), "max.exp.date", NA))))),
      p3.rel.type=ifelse(p3.elig==1 & is.na(p3.rel.type), "(missing)date.elig3",
                              p3.rel.type)) %>%
      mutate(p3.rel.yr=year(p3.date.rel))

reform3a <- p3a %>%
      select(id, p3.elig, p3.elig.yr, p3.date.adm, p3.date.rel) %>%
      dplyr::rename(p3a.elig=p3.elig,
                    p3a.elig.yr=p3.elig.yr,
		    p3a.date.adm=p3.date.adm,
		    p3a.date.rel=p3.date.rel)


p3a.elig.rel.N <- p3a %>%
   filter(p3.elig==1 &str_detect(p3.rel.type, "\\)date.elig3")) %>%
   select(p3.rel.yr) %>% table(useNA="always") %>% addmargins() %>% data.frame() %>%
   dplyr::rename(p3a= Freq)


## Scenario #2:
## 70% RELEASE AT THE EARLIEST PAROLE DATE
## Assumption about release year distribution

prob <- c(0.4, 0.2, 0.1, 0.3)
yrs <- c(0, 2, 4, 90) ## 90 means no release under this reform

p3b <- ds %>%
   mutate(
      date.elig3=ifelse(adm.type=="New Commitment" & str_detect(sent.type, "Indeterminate"), par.elig.date, NA),
      date.elig3= as.Date(date.elig3, format="%Y-%m-%d"),
      p3.elig=ifelse(adm.type!="New Commitment", 0, # Exclude anyone not serving a sentence
              ifelse(str_detect(sent.type, "Indeterminate") & !is.na(date.elig3), 1,
              0)),
      p3.elig.yr=ifelse(p3.elig==1 & year(date.elig3)>=2021, year(date.elig3),
                 ifelse(p3.elig==1 & year(date.elig3)<2021, 2021, NA)),
      date.elig3=ifelse(p3.elig==1 & year(date.elig3)<2021, as.Date("2021-01-01", format="%Y-%m-%d"),
                        date.elig3),
      date.elig3=as.Date(date.elig3, format="%Y-%m-%d"),
      p3.proj.yrs = ifelse(p3.elig==1,
                           sample(yrs, size=n(), prob=prob, replace=TRUE), NA),
      p3.date.adm=date.adm,
      p3.date.rel= ifelse(p3.elig==1 & date.elig3+years(p3.proj.yrs)<cond.rel.date,
                          date.elig3+years(p3.proj.yrs),
                   ifelse(p3.elig==1 & date.elig3+years(p3.proj.yrs)<max.exp.date,
                          date.elig3+years(p3.proj.yrs),
		   ifelse(!is.na(cond.rel.date), cond.rel.date,
                   ifelse(!is.na(max.exp.date), max.exp.date, NA)))),
      p3.date.rel= ifelse(p3.elig==1 & is.na(p3.date.rel),
                          date.elig3+years(p3.proj.yrs), p3.date.rel),
      p3.date.rel=as.Date(p3.date.rel, format="%Y-%m-%d"),
      p3.rel.type= ifelse(p3.elig==1 & date.elig3+years(p3.proj.yrs)<cond.rel.date,
                          "(cond)date.elig3+years(p3.proj.yrs)",
                   ifelse(p3.elig==1 & date.elig3+years(p3.proj.yrs)<max.exp.date,
                          "(max)date.elig3+years(p3.proj.yrs)",
		   ifelse(!is.na(cond.rel.date), "cond.rel.date",
                   ifelse(!is.na(max.exp.date), "max.exp.date", NA)))),
      p3.rel.type= ifelse(p3.elig==1 & is.na(p3.rel.type),
                          "date.elig3+years(p3.proj.yrs)", p3.rel.type),
      p3.date.rel=as.Date(p3.date.rel, format="%Y-%m-%d")) %>%
      mutate(p3.rel.yr=year(p3.date.rel))

reform3b <- p3b %>%
      select(id, p3.elig, p3.elig.yr, p3.date.adm, p3.date.rel) %>%
      dplyr::rename(p3b.elig=p3.elig,
                    p3b.elig.yr=p3.elig.yr,
		    p3b.date.adm=p3.date.adm,
		    p3b.date.rel=p3.date.rel)

p3b.elig.rel.N <- p3b %>%
   filter(p3.elig==1 &str_detect(p3.rel.type, "p3\\.proj\\.yrs")) %>%
   select(p3.rel.yr) %>% table(useNA="always") %>% addmargins( ) %>% data.frame() %>%
   dplyr::rename(p3b= Freq)

#+end_src

*** DONE Reform#4: Second Look Bill (reform4)
#+begin_src R :session :results silent

# What does the distribution of release look like for those who would befefit from this law?
## Pure assumptions about release distribution (need to consult with the team)
## 20% release at the first application; 10% release at the second application(3yrs); 5% release at the third application(6years)
## 65% never released under thie law

prob <- c(0.1, 0.05, 0.05, 0.8)
yrs <- c(0, 3, 6, 90) ## 90 means no release under this reform

## Scenario #1 Brooklyn DA's Office Suggestion

# Identify those who meet the criteria:
## A Felony Conviction: Served 20+ years AND not yet reached parole eligible date AND not yet released
## Other Felony Conviction: Served 15+ years AND not yet reached parole eligible date AND not yet released
## Random assignment of resentencing rate (need some estimation about the distribution of release)
## If projected release date is after maximum expiration date, choose maximum expiration date

set.seed(4634)

p4a <- ds %>%
   mutate(
      date.served15=as.Date(sent.date+15*365, format="%Y-%m-%d"), # date when served 15 yrs
      date.served20=as.Date(sent.date+20*365, format="%Y-%m-%d"), # date when served 20 yrs
      date.elig4=as.Date(ifelse(str_detect(class1, "A"), date.served20,
                         ifelse(class1 %in% c("B", "C", "D", "E"), date.served15, NA))), # earlest date meeting criteria
      date.elig4=as.Date(date.elig4, format="%Y-%m-%d"),
      p4.elig=ifelse(adm.type!="New Commitment", 0,
              ifelse(!is.na(date.elig4), 1, 0)),
      p4.elig.yr=ifelse(p4.elig==1 & year(date.elig4)<2021, 2021,
                 ifelse(p4.elig==1, year(date.elig4), NA)),
      date.elig4=ifelse(p4.elig==1 & year(date.elig4)<2021, as.Date("2021-01-01", format="%Y-%m-%d"),
                        date.elig4),
      p4.proj.yrs=ifelse(p4.elig==1,
                         sample(yrs, size=n(), prob=prob, replace=TRUE), NA),
      p4.date.adm=date.adm,
      p4.date.rel=ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)< cond.rel.date,
                         date.elig4+years(p4.proj.yrs),
                  ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)< max.exp.date,
                         date.elig4+years(p4.proj.yrs),
	          ifelse(!is.na(cond.rel.date), cond.rel.date,
                  ifelse(!is.na(max.exp.date), max.exp.date, NA)))),
      p4.date.rel=ifelse(p4.elig==1 & is.na(p4.date.rel), date.elig4+years(p4.proj.yrs),
                        p4.date.rel),
      p4.date.rel=as.Date(p4.date.rel, format="%Y-%m-%d"),
      p4.rel.type=ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)< cond.rel.date,
                                 "(cond)date.elig4+years(p4.proj.yrs)",
                  ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)< max.exp.date,
                         "(max)date.elig4+years(p4.proj.yrs)",
	          ifelse(!is.na(cond.rel.date), "cond.rel.date",
                  ifelse(!is.na(max.exp.date), "max.exp.date", NA)))),
      p4.rel.type=ifelse(p4.elig==1 & is.na(p4.rel.type),
                         "date.elig4+years(p4.proj.yrs)",
                        p4.rel.type)) %>%
   mutate(p4.adm.yr=year(p4.date.adm),
          p4.rel.yr=year(p4.date.rel))

reform4a <- p4a %>%
   select(id, p4.elig, p4.elig.yr, p4.date.adm, p4.date.rel) %>%
   dplyr::rename(p4a.elig=p4.elig,
                 p4a.elig.yr=p4.elig.yr,
		 p4a.date.adm=p4.date.adm,
		 p4a.date.rel=p4.date.rel)

p4a.elig.rel.N <- p4a %>%
   filter(p4.elig==1 &str_detect(p4.rel.type, "4\\.proj\\.yrs")) %>%
   select(p4.rel.yr) %>% table(useNA="always") %>% addmargins()  %>% data.frame() %>%
   dplyr::rename(p4a= Freq)


## Scenario #2 Senate/Assembly bill
## People sentenced longer than 10 years of maximum sentence AND
## Served 1/3 of maximum sentence AND
## Not serving a sentece for list charges (e.g. 125, 130, 263, money laundering to support terrorism)
## Have 2 more years before eligible for conditional release
## Eligible for reapplication every three years

set.seed(4634)

prob <- c(0.1, 0.05, 0.05, 0.8)
yrs <- c(0, 3, 6, 90) ## 90 means no release under this reform


# Create Charge Exclusion List
pl125 <- unique(ds$charge1[which(str_detect(ds$charge1, "(HOMICIDE|MANSLAUGH|MURDER)"))])
money.laundering.terror <-
   unique(ds$charge1[str_detect(ds$charge1, "MONEY") & str_detect(ds$charge1, "TERROR")])
pl130 <- unique(ds$charge1[which(str_detect(ds$charge1, "(SEXUAL|RAPE)"))])

pl263 <- unique(ds$charge1[which(str_detect(ds$charge1, "CHILD") &
                                 str_detect(ds$charge1, "SEX"))])

exclusion.list <- c(pl125, pl130, pl263, money.laundering.terror)


p4b <- ds %>%
   mutate(
     sent10plus=ifelse(max.sent.yr>=10, 1,0),    # Sentenced longer than 10 years
     date.served.third=as.Date(sent.date+days(round(agg.max.sent/3)), format="%Y-%d-%d"), # date when served 1/3 max sentence
     p4.elig=ifelse(adm.type!="New Commitment", 0, # Exclude anyone not serving a sentence
             ifelse(charge1 %in% exclusion.list, 0, # Charge Exclusion
	     ifelse(date.served.third<cond.rel.date-years(2), 1,
	     ifelse(is.na(cond.rel.date) & date.served.third<max.exp.date-years(2),1,
                    0)))),
     date.elig4=ifelse(p4.elig==1, date.served.third, NA),
     date.elig4=as.Date(date.elig4, format="%Y-%m-%d"),
     p4.elig.yr=ifelse(p4.elig==1 & year(date.elig4)<2021, 2021,
                ifelse(p4.elig==1, year(date.elig4), NA)),
     date.elig4=ifelse(p4.elig==1 & year(date.elig4)<2021, as.Date("2021-01-01", format="%Y-%m-%d"),
                       date.elig4),
     p4.proj.yrs=ifelse(p4.elig==1,
          	        sample(yrs, size=n(), prob=prob, replace=TRUE),NA),
     p4.date.adm=date.adm,
     p4.date.rel=ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)< cond.rel.date,
                        date.elig4+years(p4.proj.yrs),
                 ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)< max.exp.date,
                        date.elig4+years(p4.proj.yrs),
                 ifelse(!is.na(cond.rel.date), cond.rel.date,
                 ifelse(!is.na(max.exp.date), max.exp.date, NA)))),
     p4.date.rel=ifelse(p4.elig==1 & is.na(p4.date.rel), date.elig4+years(p4.proj.yrs),
                        p4.date.rel),
     p4.date.rel=as.Date(p4.date.rel, format="%Y-%m-%d"),
     p4.rel.type=ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)< cond.rel.date,
                             "(cond)date.elig4+years(p4.proj.yrs)",
                      ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)< max.exp.date,
                             "(max)date.elig4+years(p4.proj.yrs)",
	              ifelse(!is.na(cond.rel.date), "cond.rel.date",
                      ifelse(!is.na(max.exp.date), "max.exp.date", NA)))),
      p4.rel.type=ifelse(p4.elig==1 & is.na(p4.rel.type),
                              "(missing)date.elig4+years(p4.proj.yrs)", p4.rel.type)) %>%
   mutate(p4.adm.yr=year(p4.date.adm),
          p4.rel.yr=year(p4.date.rel))

reform4b <- p4b %>%
   select(id, p4.elig, p4.elig.yr, p4.date.adm, p4.date.rel) %>%
   dplyr::rename(p4b.elig=p4.elig,
                 p4b.elig.yr=p4.elig.yr,
		 p4b.date.adm=p4.date.adm,
		 p4b.date.rel=p4.date.rel)

p4b.elig.rel.N <- p4b %>%
   filter(p4.elig==1 &str_detect(p4.rel.type, "4\\.proj\\.yrs")) %>%
   select(p4.rel.yr) %>% table(useNA="always") %>% addmargins() %>% data.frame() %>%
   dplyr::rename(p4b= Freq)


## Scenario #3 West Virginia
## People sentenced longer than 10 years of maximum sentence AND served 10+ years
## Eligible for reapplication after 5 years; then every 2 years until exhausting the sentencing modification process
# Presumption of release for those who are 50 years or older

set.seed(4634)

prob <- c(0.1, 0.05, 0.05, 0.8)
yrs <- c(0, 5, 7, 90) ## 90 means no release under this reform

# Create Charge Exclusion List
p4c <- ds %>%
   mutate(
     date.age50=as.Date(DOB+50*365.25), # date when age 50
     sent10plus=ifelse(max.sent.yr>=10, 1,0),    # Sentenced longer than 10 years
     date.served.10yrs=as.Date(sent.date+years(10), format="%Y-%d-%d"), # date when served 10 years in prison
     p4.elig=ifelse(adm.type!="New Commitment", 0, # Exclude anyone not serving a sentence
	     ifelse(sent10plus==1 & date.served.10yrs <cond.rel.date, 1, # sentenced 10+ yrs AND served 10+ yrs
	     ifelse(sent10plus==1 & date.served.10yrs <max.exp.date, 1,
                    0))),
     p4.elig2=ifelse(adm.type!="New Commitment", 0, # Exclude anyone not serving a sentence
	     ifelse(sent10plus==1 & date.served.10yrs <cond.rel.date, 1, # sentenced 10+ yrs AND served 10+ yrs
	     ifelse(sent10plus==1 & date.served.10yrs <max.exp.date, 1,
                    0))),
     date.elig4=ifelse(p4.elig==1, date.served.10yrs, NA),
     date.elig4=as.Date(date.elig4, format="%Y-%m-%d"),
     p4.elig.yr=ifelse(p4.elig==1 & year(date.elig4)<2021, 2021,
                ifelse(p4.elig==1, year(date.elig4), NA)),
     date.elig4=ifelse(p4.elig==1 & year(date.elig4)<2021, as.Date("2021-01-01", format="%Y-%m-%d"),
                       date.elig4),
     p4.proj.yrs=ifelse(p4.elig==1,
                        sample(yrs, size=n(), prob=prob, replace=TRUE), NA),
     p4.date.adm=date.adm,
     p4.date.rel=ifelse(p4.elig==1 & date.elig4>date.age50, date.elig4,
                 ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)> date.age50,
                        date.elig4+years(p4.proj.yrs),
		 ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)<cond.rel.date,
		        date.elig4+years(p4.proj.yrs),
		 ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)<max.exp.date,
		        date.elig4+years(p4.proj.yrs),
                 ifelse(!is.na(cond.rel.date), cond.rel.date,
                 ifelse(!is.na(max.exp.date), max.exp.date, date.age85)))))),
     p4.date.rel=ifelse(p4.elig==1 & is.na(p4.date.rel), date.elig4+years(p4.proj.yrs), p4.date.rel),
     p4.date.rel=as.Date(p4.date.rel, format="%Y-%m-%d"),
     p4.rel.type=ifelse(p4.elig==1 & date.elig4>date.age50, "(age50)date.elig4",
                      ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)> date.age50,
                             "(age50)date.elig4+years(p4.proj.yrs)",
		      ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)<cond.rel.date,
		             "(cond)date.elig4+years(p4.proj.yrs)",
		      ifelse(p4.elig==1 & date.elig4+years(p4.proj.yrs)<max.exp.date,
		             "(max)date.elig4+years(p4.proj.yrs)",
                      ifelse(!is.na(cond.rel.date), "cond.rel.date",
                      ifelse(!is.na(max.exp.date), "max.exp.date", NA)))))),
     p4.rel.type=ifelse(p4.elig==1 & is.na(p4.rel.type),
                             "(missing)date.elig4+years(p4.proj.yrs)", p4.rel.type)) %>%
   mutate(p4.adm.yr=year(p4.date.adm),
          p4.rel.yr=year(p4.date.rel))

reform4c <- p4c %>%
     select(id, p4.elig, p4.elig.yr, p4.date.adm, p4.date.rel) %>%
     dplyr::rename(p4c.elig=p4.elig,
                   p4c.elig.yr=p4.elig.yr,
   		   p4c.date.adm=p4.date.adm,
		   p4c.date.rel=p4.date.rel)

p4c.elig.rel.N <- p4c %>%
   filter(p4.elig==1 &str_detect(p4.rel.type, "date\\.elig4")) %>%
   select(p4.rel.yr) %>% table(useNA="always") %>% addmargins() %>% data.frame() %>%
   dplyr::rename(p4c= Freq)


#+end_src

*** DONE Reform#5: Youth Parole (reform5)
#+begin_src R :session :results silent

# What does the distribution of release look like for those who served 15+ years ?  (rel.dist)
### For those who were on indeterminate sentence and were released,
### how long does it usually take before they are released?
### Those who were on indeterminate sentence AND
### were released to community (exclude death,transfer,other,etc) AND
### served 15 years or longer
### Exclude who were released before parole eligible date
### Also Exclude those who had parole eligible date before new date based on the policy
### since they are not relevant to the elderly parole release

## Scenario #1: Use the overall parole grant rate for those who went to parole board after serving 15 years in prison

prob<- as.vector(rel.dist[,2])
yrs <- as.vector(rel.dist[,1])

## Committed Crime Age Age Under 21
## Served 15+ Years
## No distinction by crime type
## If projected release date is later than the conditional release date, choose conditional release date
## If projected release date is later than the maximum expiration date, choose maximum expiration date

set.seed(435872)

p5a <- ds %>%
   mutate(
      date.elig5=as.Date(sent.date+15*365), # date when served 15 yrs; date eligible for parole
      youth=ifelse(age.crime<21, 1, 0),
      p5.elig=ifelse(adm.type!="New Commitment", 0,
                ifelse(youth==1 & date.elig5<par.elig.date,1, 0)), # sentenced under age21 and eligible before parole eligiblity date
      p5.elig.yr=ifelse(p5.elig==1 & year(date.elig5)<2021, 2021,
                 ifelse(p5.elig==1, year(date.elig5), NA)),
      date.elig5=ifelse(p5.elig==1 & year(date.elig5)<2021, as.Date("2021-01-01", format="%Y-%m-%d"),
                        date.elig5),
      date.elig5=as.Date(date.elig5, format="%Y-%m-%d"),
      p5.proj.yrs = ifelse(p5.elig==1,
                           sample(yrs, size=n(), prob=prob, replace=TRUE), # Random assignment of release rate
			   NA),
      p5.date.adm = date.adm,
      p5.date.rel= ifelse(p5.elig==1 & date.elig5+years(p5.proj.yrs)<cond.rel.date,
                          date.elig5+years(p5.proj.yrs),
                   ifelse(p5.elig==1 & date.elig5+years(p5.proj.yrs)<max.exp.date,
                          date.elig5+years(p5.proj.yrs),
	           ifelse(!is.na(cond.rel.date), cond.rel.date,
                   ifelse(!is.na(max.exp.date), max.exp.date, NA)))),
      p5.date.rel= ifelse(p5.elig==1 & is.na(p5.date.rel), date.elig5+years(p5.proj.yrs),
                          p5.date.rel),
      p5.date.rel=as.Date(p5.date.rel, format="%Y-%m-%d"),
      p5.rel.type= ifelse(p5.elig==1 & date.elig5+years(p5.proj.yrs)<cond.rel.date,
                              "(cond)date.elig5+years(p5.proj.yrs)",
                   ifelse(p5.elig==1 & date.elig5+years(p5.proj.yrs)<max.exp.date,
                          "(max)date.elig5+years(p5.proj.yrs)",
	           ifelse(!is.na(cond.rel.date), "cond.rel.date",
                   ifelse(!is.na(max.exp.date), "max.exp.date", NA)))),
      p5.rel.type= ifelse(p5.elig==1 & is.na(p5.rel.type), "(missing)date.elig5+years(p5.proj.yrs)",
                          p5.rel.type)) %>%
      mutate(p5.adm.yr=year(p5.date.adm),
             p5.rel.yr=year(p5.date.rel))


reform5a <- p5a  %>%
   select(id, p5.elig, p5.elig.yr, p5.date.adm, p5.date.rel) %>%
   dplyr::rename(p5a.elig=p5.elig,
                 p5a.elig.yr=p5.elig.yr,
		 p5a.date.adm=p5.date.adm,
		 p5a.date.rel=p5.date.rel)


p5a.elig.rel.N <- p5a %>%
   filter(p5.elig==1 &str_detect(p5.rel.type, "date\\.elig5")) %>%
   select(p5.rel.yr) %>% table(useNA="always") %>% addmargins() %>% data.frame() %>%
   dplyr::rename(p5a= Freq)


## Scenario #1 (Illinois)
### Crime committed under 21 years AND
### Served 10+ years
### Charges EXCEPT aggravated criminal sexual assault, predatory criminal sexual assault of a child
### First-degree murder: eligible for parole after serving 20+ years, except a term of natural life imprisonment


## Use the overall parole grant rate for those who went to parole board after serving 15 years in prison

prob<- as.vector(rel.dist[,2])
yrs <- as.vector(rel.dist[,1])

# Create Charge Exclusion List

agg.sex <- unique(ds$charge1[which(str_detect(ds$charge1, "AGG") & str_detect(ds$charge1, "SEX"))])

pred.sex.child<- unique(ds$charge1[which(str_detect(ds$charge1, "CHILD") & str_detect(ds$charge1, "SEX") &
                        str_detect(ds$charge1, "PRED"))])

exclusion.list <- c(agg.sex, pred.sex.child)

murder1 <- unique(ds$charge1[which(str_detect(ds$charge1, "MURDER") & str_detect(ds$charge1, "1ST"))])


set.seed(435872)

p5b <- ds %>%
   mutate(
      date.elig5=ifelse(charge1 %in% exclusion.list, NA,  # Not eligible for youth parole
                ifelse(charge1 %in% murder1, as.Date(sent.date+20*365.25, format="%Y-%m-%d"), # date when served 20 years for murder 1st degree
                       as.Date(sent.date+10*365.25, format="%Y-%m-%d"))), # date when served 10 yrs for all other charge
      date.elig5=as.Date(date.elig5, format="%Y-%m-%d"),
      youth=ifelse(age.crime<21, 1, 0),
      p5.elig=ifelse(adm.type!="New Commitment", 0,
              ifelse(sent.type=="LWOP", 0,
              ifelse(youth==1 & !is.na(date.elig5) & date.elig5< par.elig.date, 1, 0))),
      p5.elig.yr=ifelse(p5.elig==1 & year(date.elig5)<2021, 2021,
                 ifelse(p5.elig==1, year(date.elig5), NA)),
      date.elig5=ifelse(p5.elig==1 & year(date.elig5)<2021, as.Date("2021-01-01", format="%Y-%m-%d"),
                        date.elig5),
      date.elig5=as.Date(date.elig5, format="%Y-%m-%d"),
      p5.proj.yrs = ifelse(p5.elig==1,
                           sample(yrs, size=n(), prob=prob, replace=TRUE), # Random assignment of release rate
			   NA),
      p5.date.adm = date.adm,
      p5.date.rel= ifelse(p5.elig==1 & date.elig5+years(p5.proj.yrs)<cond.rel.date,
                          date.elig5+years(p5.proj.yrs),
                   ifelse(p5.elig==1 & date.elig5+years(p5.proj.yrs)<max.exp.date,
                          date.elig5+years(p5.proj.yrs),
	           ifelse(p5.elig==1, date.elig5+years(p5.proj.yrs),
	           ifelse(!is.na(cond.rel.date), cond.rel.date,
                   ifelse(!is.na(max.exp.date), max.exp.date, NA))))),
      p5.date.rel= ifelse(p5.elig==1 & is.na(p5.date.rel), date.elig5+years(p5.proj.yrs),
                           p5.date.rel),
      p5.date.rel=as.Date(p5.date.rel, format="%Y-%m-%d"),
      p5.rel.type= ifelse(p5.elig==1 & date.elig5+years(p5.proj.yrs)<cond.rel.date,
                          "(cond)date.elig5+years(p5.proj.yrs)",
                   ifelse(p5.elig==1 & date.elig5+years(p5.proj.yrs)<max.exp.date,
                          "(max)date.elig5+years(p5.proj.yrs)",
	           ifelse(p5.elig==1, "date.elig5+years(p5.proj.yrs)",
	           ifelse(!is.na(cond.rel.date), "cond.rel.date",
                   ifelse(!is.na(max.exp.date), "max.exp.date", NA))))),
      p5.rel.type= ifelse(p5.elig==1 & is.na(p5.rel.type), "(miss)date.elig5+years(p5.proj.yrs)",
                           p5.rel.type))  %>%
   mutate(p5.adm.yr=year(p5.date.adm),
          p5.rel.yr=year(p5.date.rel))

reform5b <- p5b %>%
      select(id, p5.elig, p5.elig.yr, p5.date.adm, p5.date.rel) %>%
      dplyr::rename(p5b.elig=p5.elig,
                    p5b.elig.yr=p5.elig.yr,
		    p5b.date.adm=p5.date.adm,
		    p5b.date.rel=p5.date.rel)

p5b.elig.rel.N <- p5b %>%
   filter(p5.elig==1 &str_detect(p5.rel.type, "p5\\.proj\\.yrs")) %>%
   select(p5.rel.yr) %>% table(useNA="always") %>% addmargins() %>% data.frame() %>%
   dplyr::rename(p5b= Freq)



#+end_src

*** TODO Porjecting Policy Impact (Overall)
#+begin_src R :session :results silent


total <- c(58378, 56315, 55979, 54865, 54141, 53103, 51744, 51466, 48932, 46244,
           43555, 42998, 42442, 41885, 41328, 40772, 40215, 39658, 39102, 38545,
	   37988)

year <- seq(2010, 2030, by=1)

TPV <- c(4771, 4815, 4825, 4919, 4620, 4822, 4756, 4744, 4731, 4719,
         4707, 4695, 4682, 4670, 4658, 4646, 4633, 4621, 4609, 4597,
	 4584)

df.pop <- data.frame(year, total, TPV)

models <- c("p1a", "p2a", "p2b", "p3a", "p3b", "p4a", "p4b", "p4c", "p5a", "p5b")


p2a.rel <- p2a.elig.rel.N$p2a[1:10]
p2b.rel <- p2b.elig.rel.N$p2b[1:10]
p3a.rel <- p3a.elig.rel.N$p3a[1:10]
p3b.rel <- p3b.elig.rel.N$p3b[1:10]
p4a.rel <- p4a.elig.rel.N$p4a[1:10]
p4b.rel <- p4b.elig.rel.N$p4b[1:10]
p4c.rel <- p4c.elig.rel.N$p4c[1:10]
p5a.rel <- p5a.elig.rel.N$p5a[1:10]
p5b.rel <- p5b.elig.rel.N$p5b[1:10]

col.list <- paste0(models, ".rel")
df.pol <- data.frame(year=seq(2021, 2030, by=1),
                   p2a.rel, p2b.rel, p3a.rel, p3b.rel,
                   p4a.rel, p4b.rel, p4c.rel, p5a.rel, p5b.rel)

df.pol <- df.pol %>%
mutate(
p2a.cum= cumsum(p2a.rel),
p2b.cum= cumsum(p2b.rel),
p3a.cum= cumsum(p3a.rel),
p3b.cum= cumsum(p3b.rel),
p4a.cum= cumsum(p4a.rel),
p4b.cum= cumsum(p4b.rel),
p4c.cum= cumsum(p4c.rel),
p5a.cum= cumsum(p5a.rel),
p5b.cum= cumsum(p5b.rel)
)


df.merged <-left_join(df.pop[12:21,], df.pol, by="year")

df.merged <- df.merged %>%
mutate(
p2a.pop =total-p2a.cum,
p2b.pop =total-p2b.cum,
p3a.pop =total-p3a.cum,
p3b.pop =total-p3b.cum,
p4a.pop =total-p4a.cum,
p4b.pop =total-p4b.cum,
p4c.pop =total-p4c.cum,
p5a.pop =total-p5a.cum,
p5b.pop =total-p5b.cum)

write.csv(df.merged, "data/policy.projection.csv")

#+end_src

