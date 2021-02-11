# =============================================================================
# Program Author: Jaeok Kim
# Start Date: Dec 10, 2020
# Update Date: Feb 9, 2021
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
      type="admission",
      year=ifelse(adm.mo<=3, adm.year, adm.year+1)) %>%
   select(year, type, adm.type, county, age, gender, top.crime)

# Bind In-custody Data and Yearly Admission Data
## NOTE: Admission Data Missing Feb2019-Mar2020 Admission

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

# Charge Severity Clean

crime.code <- read_excel("data/DOCCS_Crime_Codes_DCJS.xlsx")
crime.code <- data.frame(crime.code)
names(crime.code) <- c("doccs.code", "crime.desc", "effective.date", "repeal.date", "legal",
                       "code", "class", "crime.type")


<- left_join(sf, crime.code) %>%






# PROJECTION (28 groups): Admission Type*Charge Severity/Age/NYC
## NOTE: Missing Admissions Jan2020-Mar2020 (impute the average admission of the last 9months)

var_list <- c("adm.type", "age.group2", "NYC", "type", "year")

## Create a lag variable
df.overall <- sf %>%
   filter(year>=2016) %>%
   group_by_at(var_list) %>%
   dplyr::summarise(N=n()) %>%
   mutate(N=ifelse(type=="admission" & year==2020, round(N+N*3/9), N)) %>%
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
write.csv(proj, "data/proj12.csv")

# Aggregate numbers
agg.proj <- proj %>%
group_by(type) %>%
summarise_at(vars(y2016:y2030), funs(sum)) %>% data.frame()

## EXPORT DATA POINTS FOR A Figure
write.csv(agg.proj, "data/agg.proj12.csv")

# Aggregate numbers
agg.sf <- proj %>%
group_by(type,adm.type) %>%
summarise_at(vars(y2016:y2030), funs(sum)) %>% data.frame()

write.csv(agg.sf, "data/agg.sf.proj12.csv")
#+end_src


* Prospective Impact Estimation ------------------------------------------------------------
** Parole Reform Impact Estimation
#+begin_src R :session :results silent

## CONDITION MATRIX (To Create)  ++ WORKING +++

AE2 <- 55 # P2_Elder Parole_Age at Eligible : 50, (55), 60, 65
TS2 <- 15 # P2_Elder Parole_Time Served: 5, 10, (15), 20
TS5 <- 10 # P5_Youth Parole_Time Served: 5, (10), 15, 20
AC5 <- 25 # P5_Youth Parole_Age at Crime (Under): 21, (25)
TS4 <- 10 # P4_Retroactive Sentencing: 5, (10), 15, 20, 25

RR.base <- c(0.44 0.13 0.07 0.06 0.27 0.03) # Release Rate Distribution for P2,P4,P5
                                            ## Needs to be updated if Fair and Timely Parole is implemented ##

## P3_Fair and Timely Parole_Release Rate: 60%, 70%, 80%, 90%, 100%
group <- c("initial", "re1", "re2", "re3", "CR", "noRelease")
# c(0.60, 0.09, 0.05, 0.04, 0.19, 0.02)  # 60% initial release rate ; 52% release rate for reappearanc
RR3 <- c(0.70, 0.07, 0.04, 0.03, 0.14, 0.02)
#c(0.80, 0.05, 0.03, 0.02, 0.10, 0.01)
#c(0.90, 0.02, 0.01, 0.01, 0.05, 0.01)
#c(1, 0, 0, 0, 0, 0)

## Create Charge Exclusion Criteria
## Retroactive sentencing
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
      p1.elig=ifelse(adm.type=="Parole/Cond.Rel Revoc",1,0), # identify people admitted for parole violation
      p1.elig.yr=ifelse(p1.elig==1 & year(date.adm)<2021, 2021, NA), # people who are already eleigible on Jan1,2021

     # P2: Elder Parole
      date.age.c=as.Date(DOB+AE2*365), # date when age c (e.g.55)
      date.served.c=as.Date(date.adm+TS2*365), # date when served c (e.g.15) yrs
      date.elig2=as.Date(ifelse(date.age.c>date.served.c, date.age.c, date.served.c)), # earlest date meeting both criteria

      p2.elig=ifelse(adm.type!="New Commitment", 0, # exclude anyone not serving a sentence on commitment crime
              ifelse(!is.na(par.elig.date) & (year(date.elig2)-year(par.elig.date))<0, 1, # available for earlier parole (indeterminate sentences)
              ifelse(is.na(par.elig.date) & (year(date.elig2)-year(max.exp.date))<0, 1, 0))), # available for parole (determinate sentences)
      p2.elig.yr=ifelse(p2.elig==1 & year(date.elig2)<2021, 2021,
                 ifelse(p2.elig==1, year(date.elig2), NA)),
      date.elig2=ifelse(p2.elig==1 & year(date.elig2)<2021, ymd("2021-01-01"), date.elig2),
      date.elig2=as.Date(date.elig2, format="%Y-%m-%d"),

     # P5: Youth Parole
      date.elig5=as.Date(sent.date+TS5*365), # date when served c (e.g. 10) yrs; date eligible for parole
      youth=ifelse(age.crime<AC5, 1, 0), # people who committmed crime before age c (e.g.25)
      p5.elig=ifelse(adm.type!="New Commitment", 0,
                ifelse(youth==1 & date.elig5<par.elig.date,1, 0)), # people who meet both criteria (age at crime and sentence length)
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
      date.age.life=as.Date(DOB+age.life*365, format="%Y-%m-%f"), # date when age X (life expectancy)
      new.max.exp.date=ifelse(max.sent.group=="Life", date.age.life, max.exp.date),
      new.max.exp.date=as.Date(new.max.exp.date, origin="1970-01-01"),

     # P1: TPV
      p1.date.adm=ifelse(p1.elig==1 & year(date.adm)>=2021, NA, date.adm),
      p1.date.rel=ifelse(p1.elig==1 & year(date.adm)>=2021, NA,
                  ifelse(p1.elig==1 & year(date.adm)<2021,
                         as.Date("2021-01-01", format="%Y-%m-%d"), NA)),
      p1.date.adm=as.Date(p1.date.adm, format="%Y-%m-%d"),
      p1.date.rel=as.Date(p1.date.rel, format="%Y-%m-%d"),

     # P2: Elder Parole
      p2.rel.group = ifelse(p2.elig==1,
                            sample(group, size=n(), prob=RR.base, replace=TRUE), NA),
      p2.date.adm = date.adm,
      p2.date.rel = ifelse(p2.rel.group=="initial", date.elig2,
                    ifelse(p2.rel.group=="re1", date.elig2+years(2),
                    ifelse(p2.rel.group=="re2", date.elig2+years(4),
                    ifelse(p2.rel.group=="re3", date.elig2+years(6), 
 		    ifelse(p2.rel.group=="CR", cond.rel.date, 
 		    ifelse(p2.rel.group=="noRelease", as.Date("2040-01-01", format="%Y-%m-%d"), NA)))))),
      p2.date.rel = ifelse(p2.elig==1 & year(as.Date(p2.date.rel,format="%Y-%m-%d"))<2021,
                           as.Date("2021-01-01", format="%Y-%m-%d"), p2.date.rel),

     # P5: Youth Parole
      p5.rel.group = ifelse(p5.elig==1,
                            sample(group, size=n(), prob=RR.base, replace=TRUE), NA),
      p5.date.adm = date.adm,
      p5.date.rel = ifelse(p5.rel.group=="initial", date.elig5,
                    ifelse(p5.rel.group=="re1", date.elig5+years(2),
                    ifelse(p5.rel.group=="re2", date.elig5+years(4),
                    ifelse(p5.rel.group=="re3", date.elig5+years(6), 
 		    ifelse(p5.rel.group=="CR", cond.rel.date, 
 		    ifelse(p5.rel.group=="noRelease", as.Date("2040-01-01", format="%Y-%m-%d"), NA)))))),
      p5.date.rel = ifelse(p5.elig==1 & year(as.Date(p5.date.rel,format="%Y-%m-%d"))<2021,
                           as.Date("2021-01-01", format="%Y-%m-%d"), p5.date.rel),

     # P4: Retroactive Sentencing
      p4.rel.group = ifelse(p4.elig==1,
                            sample(group, size=n(), prob=RR.base, replace=TRUE), NA),
      p4.date.adm = date.adm,
      p4.date.rel = ifelse(p4.rel.group=="initial", date.elig4,
                    ifelse(p4.rel.group=="re1", date.elig4+years(2),
                    ifelse(p4.rel.group=="re2", date.elig4+years(4),
                    ifelse(p4.rel.group=="re3", date.elig4+years(6), 
 		    ifelse(p4.rel.group=="CR", cond.rel.date, 
 		    ifelse(p4.rel.group=="noRelease", as.Date("2040-01-01", format="%Y-%m-%d"), NA)))))),
      p4.date.rel = ifelse(p4.elig==1 & year(as.Date(p4.date.rel,format="%Y-%m-%d"))<2021,
                           as.Date("2021-01-01", format="%Y-%m-%d"), p4.date.rel),

     # P3: Fair and Timely Parole
      p3.rel.group = ifelse(p3.elig==1,
                            sample(group, size=n(), prob=RR3, replace=TRUE), NA),
      p3.date.adm = date.adm,
      p3.date.rel = ifelse(p3.rel.group=="initial", date.elig3,
                    ifelse(p3.rel.group=="re1", date.elig3+years(2),
                    ifelse(p3.rel.group=="re2", date.elig3+years(4),
                    ifelse(p3.rel.group=="re3", date.elig3+years(6), 
 		    ifelse(p3.rel.group=="CR", cond.rel.date, 
 		    ifelse(p3.rel.group=="noRelease", as.Date("2040-01-01", format="%Y-%m-%d"), NA)))))),
      p3.date.rel = ifelse(p3.elig==1 & year(as.Date(p3.date.rel,format="%Y-%m-%d"))<2021,
                           as.Date("2021-01-01", format="%Y-%m-%d"), p3.date.rel))%>%
   mutate_at(vars(ends_with("date.rel")), funs(as.Date(., format="%Y-%m-%d"))) %>%
   mutate_at(vars(ends_with("date.rel")), funs("yr"=year(.))) %>%
   rename_with(., ~gsub("date.rel_yr", "rel.yr", .x, fixed=TRUE),  ends_with("date.rel_yr")) %>%
   select(ends_with("rel.yr") | ends_with(".elig"))


## Estimate Number of Release by Year For Each Combinations of Policy Reform Scenario



# +++ Summary  ++++++++

tmp2 <- wd2 %>%
    dplyr:: rename(rel.yr=p2.rel.yr) %>%
    mutate(p2.elig.total=sum(p2.elig, na.rm=TRUE),
           p2.rel.total=sum(p2.elig[!is.na(rel.yr)], na.rm=TRUE)) %>%
   group_by(rel.yr) %>%
   dplyr:: summarise(p2.rel=sum(p2.elig, na.rm=TRUE),
                     p2.rel.total=mean(p2.rel.total),
                     p2.elig.total=mean(p2.elig.total))  %>%
   mutate(cum.p2.rel=cumsum(p2.rel)) %>%
   filter(rel.yr<=2030) %>% select(rel.yr, p2.rel, cum.p2.rel, p2.rel.total, p2.elig.total) %>% data.frame()

tmp3 <- wd2 %>%
    dplyr:: rename(rel.yr=p3.rel.yr) %>%
    mutate(p3.elig.total=sum(p3.elig, na.rm=TRUE),
           p3.rel.total=sum(p3.elig[!is.na(rel.yr)], na.rm=TRUE)) %>%
   group_by(rel.yr) %>%
   dplyr:: summarise(p3.rel=sum(p3.elig, na.rm=TRUE),
                     p3.rel.total=mean(p3.rel.total),
                     p3.elig.total=mean(p3.elig.total))  %>%
   mutate(cum.p3.rel=cumsum(p3.rel)) %>%
   filter(rel.yr<=2030) %>% select(rel.yr, p3.rel, cum.p3.rel, p3.rel.total, p3.elig.total) %>% data.frame()

tmp5 <- wd2 %>%
    dplyr:: rename(rel.yr=p5.rel.yr) %>%
    mutate(p5.elig.total=sum(p5.elig, na.rm=TRUE),
           p5.rel.total=sum(p5.elig[!is.na(rel.yr)], na.rm=TRUE)) %>%
   group_by(rel.yr) %>%
   dplyr:: summarise(p5.rel=sum(p5.elig, na.rm=TRUE),
                     p5.rel.total=mean(p5.rel.total),
                     p5.elig.total=mean(p5.elig.total))  %>%
   mutate(cum.p5.rel=cumsum(p5.rel)) %>%
   filter(rel.yr<=2030) %>% select(rel.yr, p5.rel, cum.p5.rel, p5.rel.total, p5.elig.total) %>% data.frame()

#+end_src

