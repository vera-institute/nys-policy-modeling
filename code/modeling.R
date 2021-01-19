# =============================================================================
# Program Author: Jaeok Kim
# Start Date: Dec 10, 2020
# Update Date: Jan 19, 2021
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

setwd("~/OneDrive - Vera Institute of Justice/GitHub/policy-modeling")

# LOAD ORIGINAL DATA FROM S3

## Snapshot 2020-02-14
## "2020_02_14_DOCCS_current_clean.csv"

df.current <- s3read_using(FUN=read.csv,
                   object="2020_02_14_DOCCS_current_clean.csv",
		   bucket="vera-gjny/curated_data/ny_prison/in_custody/2020_02")

df.current <- df.current %>%
   mutate(id= seq(1, nrow(.), by=1))

## Clean Snapshot 2020-02-14
ds <- readRDS("data/doccs.clean.2020.02.14.rds")

## clean variable
ds <- ds %>%
   mutate(age.current.group2=ifelse(age.current>=55, "age55+", "Under55"),
          race.group=ifelse(is.na(race.eth), "Other/Unknown",
                     ifelse(race.eth=="Other", "Other/Unknown", race.eth)),
          max.sent.group=ifelse(max.sent.yr<10 , "Less than 10yrs",
                         ifelse(max.sent.yr>=10 & max.sent.yr<150, "10yrs or longer",
			 ifelse(max.sent.yr==9999, "Life", NA))))


## 2018 Data
doccs2018 <- read.csv("data/DOCCSJan2018.csv")

## 2020 October
## "2020_10_01_DOCCS_current_clean.csv"

df.oct <- s3read_using(FUN=read.csv,
                   object="2020_10_01_DOCCS_current_clean.csv",
		   bucket="vera-gjny/curated_data/ny_prison/in_custody/2020_10")


## People In Custody (NYS Open Data)
custody <- read.csv("~/OneDrive - Vera Institute of Justice/Project/ESI/Modeling/Data/Inmates_Under_Custody__Beginning_2008.csv")


#+end_src


* Projecting Future Population accounting for stock and flow
#+begin_src R :session :results silent

# In Custody Population Data
custody <- read.csv("data/Inmates_Under_Custody__Beginning_2008.csv")
custody <- custody %>%
   dplyr::rename(
      year=Snapshot.Year,
      adm.type=Latest.Admission.Type,
      county=County.of.Indictment,
      gender=Gender,
      top.crime=Most.Serious.Crime,
      age=Current.Age,
      facility=Housing.Facility,
      security.level=Facility.Security.Level,
      race=Race.Ethnicity) %>%
   mutate(type="total") %>%
   select(year, type, adm.type, county, age, gender, top.crime)

# Yearly Admission Data
admission <- read.csv("data/Prison_Admissions__Beginning_2008.csv")
admission <- admission %>%
   dplyr::rename(
     adm.year=Admission.Year,
     adm.mo=Month.Code,
     adm.type=Admission.Type,
     county=County.of.Commitment,
     residence=Last.Known.Residence.County,
     gender=Gender,
     age=Age.at.Admission,
     top.crime=Most.Serious.Crime) %>%
   mutate(
      type="flow",
      year=ifelse(adm.mo<=3, adm.year, adm.year+1)) %>%
   select(year, type, adm.type, county, age, gender, top.crime)

## Admission File Missing Feb2019-Mar2020 Admission

sf <- bind_rows(admission, custody) %>%
   mutate(
      age.group=ifelse(age<18, "Under 18",
                ifelse(age>=18 & age<21, "age18-20",
		ifelse(age>=21 & age<25, "age21-24",
		ifelse(age>=25 & age<30, "age25-29",
		ifelse(age>=30 & age<35, "age30-34",
		ifelse(age>=35 & age<40, "age35-39",
		ifelse(age>=40 & age<45, "age40-44",
		ifelse(age>=45 & age<50, "age45-49",
		ifelse(age>=50 & age<55, "age50-54",
		ifelse(age>=55 & age<60, "age55-59",
		ifelse(age>=60, "60 or over", NA))))))))))),
      age.group=factor(age.group,
                       levels=c("Under 18", "age18-20", "age21-24", "age25-29",
           	             "age30-34", "age35-39", "age40-44", "age45-49",
			     "age50-54", "age55-59", "60 or over")),
      age.group2=ifelse(age>=55, "age55+", "Under55"),
      NYC=ifelse(county %in% c("KINGS", "QUEENS", "NEW YORK", "BRONX", "RICHMOND"), "NYC", "non-NYC"))

sf %>%
group_by(type, year) %>%
dplyr::summarise(N=n()) %>% data.frame()

## 2020 flow missing Jan2020-Mar2020

# PROJECTION 3: Admission Type

var_list <- c("adm.type", "age.group2", "NYC", "type", "year")

## Create a lag variable
df.overall <- sf %>%
   filter(year>=2016) %>%
   group_by_at(var_list) %>%
   dplyr::summarise(N=n()) %>%
   mutate(N=ifelse(type=="flow" & year==2020, round(N*1.25), N)) %>%
   mutate(N.lag1=dplyr::lag(N, n=1, default=NA)) %>%
   arrange_at(var_list) %>% data.frame() %>%
   mutate(year=paste0("y",year),
	  delta=(N-N.lag1)/N.lag1)

## Create a weighted average of pouplation
df1 <- df.overall %>%
   select(-(N.lag1:delta)) %>%
   spread(key=year, value=N, fill=NA) %>%
   mutate(wAvg=(y2016*1+y2017*1+y2018*2+y2019*2+y2020*3)/9)

## Create a weighted average of yearly percent changes
df2 <- df.overall %>%
   select(-(N:N.lag1)) %>%
   spread(key=year, value=delta) %>%
   select(-y2016) %>%
   dplyr::rename(d1617=y2017,
                 d1718=y2018,
		 d1819=y2019,
		 d1920=y2020) %>%
   mutate(wDelta=tanh((d1617*1+d1718*2+d1819*2+d1920*3)/8*4))

#  Estimate 2030 baseline: Multiply two to project the 2030 baseline

var_merge <- var_list[which(var_list!="year")]

df3 <- df1 %>%
   left_join(df2, by=var_merge) %>%
   mutate(y2030=wAvg*(1+wDelta),
          d=(y2030-y2020)/10)

#   summarise_at(vars(y2016:y2020, y2030), sum, na.rm=TRUE)

# Estimate population in 2021-2029: Linear interpolation bewteen 2020-2030

mylist <- list()

for (i in 1:10) {

   x <- df3$y2020+df3$d*i

   print(x)

   mylist[[i]] <-x

  print(mylist)
}

tmp <- do.call("rbind", mylist)
tmp <- t(tmp)
toMerge <- data.frame(tmp)

colnames(toMerge) <- paste0("y",seq(2021, 2030, by=1))
toMerge <- cbind(toMerge, df3[,var_merge])
toMerge <- toMerge[,-10]

# Merge files
proj <- df3 %>%
   left_join(toMerge, by=var_merge) %>%
   select(var_merge, y2016:y2020, y2021:y2029, y2030) %>%
   mutate_at(vars(y2021:y2030), function(x) (round(x)))

## EXPORT DATA POINTS FOR A Figure
write.csv(proj, "data/proj5.csv")


# Aggregate numbers
agg.proj <- proj %>%
group_by(type) %>%
summarise_at(vars(y2016:y2030), funs(sum)) %>% data.frame()

## EXPORT DATA POINTS FOR A Figure
write.csv(agg.proj, "data/agg.proj5.csv")

#+end_src


* Prospective Impact Estimation ------------------------------------------------------------
** Parole Reform Impact Estimation 
#+begin_src R :session :results silent

## CONDITION MATRIX (To Create)  ++ WORKING +++

AE2 <- 55 # P2_Elder Parole_Age at Eligible : 50, (55), 60, 65, 70
TS2 <- 15 # P2_Elder Parole_Time Served: 5, 10, (15), 20, 25
TS5 <- 10 # P5_Youth Parole_Time Served: 5, (10), 15, 20, 25
AC5 <- 21 # P5_Youth Parole_Age at Crime (Under): (21), 25
TS4 <- 10 # P4_Retroactive Sentencing: 5, (10), 15, 20, 25

## Release Rate Parameters

prob <- c(0.77, 0.16, 0.07)
group <- c("board", "CR", "NoRel")
addTime <- .18 

##
board   cr      noRel   addTime
0.77	0.16	0.07	0.18
0.80	0.14	0.06	0.10
0.90	0.07	0.03	0.5
1.00	0.00	0.00    0

## Create Charge Exclusion List
pl125 <- unique(ds$charge1[which(str_detect(ds$charge1, "(HOMICIDE|MANSLAUGH|MURDER)"))])
money.laundering.terror <-
   unique(ds$charge1[str_detect(ds$charge1, "MONEY") & str_detect(ds$charge1, "TERROR")])
pl130 <- unique(ds$charge1[which(str_detect(ds$charge1, "(SEXUAL|RAPE)"))])

pl263 <- unique(ds$charge1[which(str_detect(ds$charge1, "CHILD") &
                                 str_detect(ds$charge1, "SEX"))])

exclusion.list <- c(pl125, pl130, pl263, money.laundering.terror)

## Eligibility Flag by Policy

wd  <- ds %>%
   mutate(
     # P1: TPV
      p1.elig=ifelse(adm.type=="Parole/Cond.Rel Revoc",1,0),
      p1.elig.yr=ifelse(p1.elig==1 & year(date.adm)<2021, 2021, NA),

     # P2: Elder Parole
      date.age.c=as.Date(DOB+AE2*365), # date when age c (e.g.55)
      date.served.c=as.Date(DOB+TS2*365), # date when served c (e.g.15) yrs
      date.elig2=as.Date(ifelse(date.age.c>date.served.c, date.age.c, date.served.c)), # earlest date meeting both criteria

      p2.elig=ifelse(adm.type!="New Commitment", 0, # exclude anyone not serving a sentence on commitment crime
              ifelse(!is.na(par.elig.date) & (year(date.elig2)-year(par.elig.date))<0, 1, # available for earlier parole (indeterminate sentences)
              ifelse(is.na(par.elig.date) & (year(date.elig2)-year(max.exp.date))<0, 1, 0))), # available for parole (determinate sentences)
      p2.elig.yr=ifelse(p2.elig==1 & year(date.elig2)<2021, 2021,
                 ifelse(p2.elig==1, year(date.elig2), NA)),
      date.elig2=ifelse(p2.elig==1 & year(date.elig2)<2021, ymd("2021-01-01"), date.elig2),
      date.elig2=as.Date(date.elig2, format="%Y-%m-%d"),

     # P5: Youth Parole
      date.elig5=as.Date(sent.date+TS5*365), # date when served 15 yrs; date eligible for parole
      youth=ifelse(age.crime<AC5, 1, 0),
      p5.elig=ifelse(adm.type!="New Commitment", 0,
                ifelse(youth==1 & date.elig5<par.elig.date,1, 0)), # sentenced under age21 and eligible before parole eligiblity date
      p5.elig.yr=ifelse(p5.elig==1 & year(date.elig5)<2021, 2021,
                 ifelse(p5.elig==1, year(date.elig5), NA)),
      date.elig5=ifelse(p5.elig==1 & year(date.elig5)<2021, ymd("2021-01-01"), date.elig5),
      date.elig5=as.Date(date.elig5, format="%Y-%m-%d"),

     # P3: Fair and Timely Parole
      date.elig3=ifelse(adm.type=="New Commitment" & str_detect(sent.type, "Indeterminate"), par.elig.date, NA),
      date.elig3= as.Date(date.elig3, format="%Y-%m-%d"),
      p3.elig=ifelse(adm.type!="New Commitment", 0, # Exclude anyone not serving a sentence
              ifelse(str_detect(sent.type, "Indeterminate") & !is.na(date.elig3), 1,
              0)),
      p3.elig.yr=ifelse(p3.elig==1 & year(date.elig3)>=2021, year(date.elig3),
                 ifelse(p3.elig==1 & year(date.elig3)<2021, 2021, NA)),
      date.elig3=ifelse(p3.elig==1 & year(date.elig3)<2021, ymd("2021-01-01"), date.elig3),
      date.elig3=as.Date(date.elig3, format="%Y-%m-%d"),

     # P4: Retroactive Sentencing
      sent.c=ifelse(max.sent.yr>=TS4, 1,0),    # Sentenced longer than c years (e.g.10)
      date.served.third=as.Date(sent.date+days(round(agg.max.sent/3)), format="%Y-%d-%d"), # date when served 1/3 max sentence
      p4.elig=ifelse(adm.type!="New Commitment", 0, # Exclude anyone not serving a new sentence
              ifelse(charge1 %in% exclusion.list, 0, # Charge Exclusion
	      ifelse(sent.c==1 & date.served.third<cond.rel.date-years(2), 1,
	      ifelse(sent.c==1 & is.na(cond.rel.date) & date.served.third<max.exp.date-years(2),1,
                     0)))),
      date.elig4=ifelse(p4.elig==1, date.served.third, NA),
      date.elig4=as.Date(date.elig4, format="%Y-%m-%d"),
      p4.elig.yr=ifelse(p4.elig==1 & year(date.elig4)<2021, 2021,
                 ifelse(p4.elig==1, year(date.elig4), NA)),
      date.elig4=ifelse(p4.elig==1 & year(date.elig4)<2021, ymd("2021-01-01"), date.elig4),
      date.elig4=as.Date(date.elig4, format="%Y-%m-%d"))


## Estimate Release Date Under Policy Reform Scenario
set.seed(4634)

wd2 <- wd %>%
   mutate(
     # P1: TPV
      p1.date.adm=ifelse(p1.elig==1 & year(date.adm)>=2021, NA, date.adm),
      p1.date.rel=ifelse(p1.elig==1 & year(date.adm)>=2021, NA,
                  ifelse(p1.elig==1 & year(date.adm)<2021,
                         as.Date("2021-01-01", format="%Y-%m-%d"), NA)),
      p1.date.adm=as.Date(p1.date.adm, format="%Y-%m-%d"),
      p1.date.rel=as.Date(p1.date.rel, format="%Y-%m-%d"))

     # P2: Elder Parole
      p2.rel.group = ifelse(p2.elig==1,
                            sample(group, size=n(), prob=prob, replace=TRUE), NA),
      p2.date.adm = date.adm,
      p2.date.rel = ifelse(p2.rel.group=="board", date.elig2+(max.exp.date-date.elig2)*addTime,
                    ifelse(p2.rel.group=="CR", cond.rel.date,
                    ifelse(p2.rel.group=="NoRel", max.exp.date, NA))),

     # P5: Youth Parole
      p5.rel.group = ifelse(p5.elig==1,
                            sample(group, size=n(), prob=prob, replace=TRUE), NA),
      p5.date.adm = date.adm,
      p5.date.rel = ifelse(p5.rel.group=="board", date.elig5+(max.exp.date-date.elig5)*addTime,
                    ifelse(p5.rel.group=="CR", cond.rel.date,
                    ifelse(p5.rel.group=="NoRel", max.exp.date, NA))),

     # P3: Fair and Timely Parole
      p3.rel.group = ifelse(p3.elig==1,
                            sample(group, size=n(), prob=prob, replace=TRUE), NA),
      p3.date.adm = date.adm,
      p3.date.rel = ifelse(p3.rel.group=="board", date.elig3+(max.exp.date-date.elig3)*addTime,
                    ifelse(p3.rel.group=="CR", cond.rel.date,
                    ifelse(p3.rel.group=="NoRel", max.exp.date, NA))),

     # P4: Retroactive Sentencing
      p4.rel.group = ifelse(p4.elig==1,
                            sample(group, size=n(), prob=prob, replace=TRUE), NA),
      p4.date.adm = date.adm,
      p4.date.rel = ifelse(p4.rel.group=="board", date.elig4+(max.exp.date-date.elig4)*addTime,
                    ifelse(p4.rel.group=="CR", cond.rel.date,
                    ifelse(p4.rel.group=="NoRel", max.exp.date, NA)))) %>%
   mutate_at(vars(ends_with("date.rel")), funs(as.Date(., format="%Y-%m-%d"))) %>%
   mutate_at(vars(ends_with("date.rel")), funs("yr"=year(.))) %>%
   rename_with(., ~gsub("date.rel_yr", "rel.yr", .x, fixed=TRUE),  ends_with("date.rel_yr")) %>%
   select(ends_with("rel.yr") | ends_with(".elig"))

## Estimate Number of Release by Year For Each Combinations of Policy Reform Scenario

### Policy Reform Combinations
combo <- expand.grid(
          pol1=c(1,0),
          pol2=c(1,0),
          pol3=c(1,0),
          pol4=c(1,0),
          pol5=c(1,0))

combo$id <- rownames(combo)

combo <- combo %>% select(id, everything())
combo <- recast(combo, id ~ variable + value, id.var = 1, fun.aggregate = function(x) (length(x) > 0) + 0L)
combo <- combo %>% select(ends_with("_1")) %>%
         rename_with(., ~gsub("_1", "", .x, fixed=TRUE),  ends_with("_1"))

###

# +++ START HERE ++++++++

tmp2 <- wd2 %>%
   group_by(p2.rel.yr) %>%
   dplyr:: summarise(p2.rel=sum(p2.elig, na.rm=TRUE)) %>%
   dplyr:: rename(rel.yr=p2.rel.yr) %>%
   filter(rel.yr<=2030) %>% data.frame()

tmp3 <- wd2 %>%
   group_by(p3.rel.yr) %>%
   dplyr:: summarise(p3.rel=sum(p3.elig, na.rm=TRUE)) %>%
   dplyr:: rename(rel.yr=p3.rel.yr) %>%
   filter(rel.yr<=2030) %>% data.frame()

tmp4 <- wd2 %>%
   group_by(p4.rel.yr) %>%
   dplyr:: summarise(p4.rel=sum(p4.elig, na.rm=TRUE)) %>%
   dplyr:: rename(rel.yr=p4.rel.yr) %>%
   filter(rel.yr<=2030) %>% data.frame()

tmp5 <- wd2 %>%
   group_by(p5.rel.yr) %>%
   dplyr:: summarise(p5.rel=sum(p5.elig, na.rm=TRUE)) %>%
   dplyr:: rename(rel.yr=p5.rel.yr) %>%
   filter(rel.yr<=2030) %>% data.frame()

#+end_src
