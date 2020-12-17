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


* Setup Path and Read-in Data -------------------------------------------------------------
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


* Dataset Cleanup  ------------------------------------------------------------------------
#+begin_src R :session :results silent

## DATE VARIABLES
### Admission Date (date.adm)
### Release Date (date.rel)
### Parole Eligible Date (date.parole.elig)
### Mandatory Prison Release Date (date.mandatory.rel)
### Projected Prison Release Date (date.proj.rel)
### Birth Date (assume 15 as the day of birth) (date.birth)
### Age at Admission (age.adm)
### Age at Release (age.rel)
### Age at Release (age.cur)
### County (county)
### NYC vs non-NYC (NYC)
### Race: Hispanic/White/Black/Other/Missing (race)
### Number of charges (ncharge)
### Length of prison incarceration (months.prison)
### Length of jail incarceration (months.jail)
### Length of total incarceration (months.incarc)
### Proportion of sentence they served (prop.max.served)
 
doccs <- df.current


# Date Variable Format Unifying
date.var <-c("dob", "origDateRec" , "currDateRec", "earRelDate", "parHearDate", "parEligDate",
         "parEligDate", "condRelDate", "maxExpDate", "maxExpDateParSup",
	 "parBoardDisDate", "latestRelease")

for (i in date.var) {

  print(i)

  doccs[,i] <- ifelse(str_detect(doccs[,i], "^[0-9]{4}\\-"),
       paste0(substr(doccs[,i], 6,7), "/", substr(doccs[,i], 9,10), "/",
              substr(doccs[,i], 1,4)), doccs[,i])
}

# Clean Variables
doccs <- doccs %>%
   mutate(

   # Race and Ethnicity
   race.eth=ifelse(str_detect(raceEth, "HISPANIC"), "Hispanic",
            ifelse(raceEth=="WHITE", "White Non-Hispanic",
            ifelse(raceEth=="BLACK", "Black Non-Hispanic",
            ifelse(raceEth=="UNKNOWN", "Unknown",
            "Other")))),

   # Sex
   sex=ifelse(str_detect(sex, "FEMALE"), "Female",
       ifelse(str_detect(sex, "MALE"), "Male", NA)),

   # Date Variables
   DOB=as.Date(dob, format="%m/%d/%Y"),
   orig.adm.date=as.Date(origDateRec, format="%m/%d/%Y"),
   curr.adm.date=as.Date(currDateRec, format="%m/%d/%Y"),
   earl.rel.date=as.Date(earRelDate, format="%m/%d/%Y"),
   par.hear.date=as.Date(parHearDate, format="%m/%d/%Y"),
   par.elig.date=as.Date(parEligDate, format="%m/%d/%Y"),
   cond.rel.date=as.Date(condRelDate, format="%m/%d/%Y"),
   max.exp.date=as.Date(maxExpDate, format="%m/%d/%Y"),
   max.exp.par.sup.date=as.Date(maxExpDateParSup, format="%m/%d/%Y"),
   par.dis.date=as.Date(parBoardDisDate, format="%m/%d/%Y"),
   latest.rel.date=as.Date(substr(latestRelease, 1,8), format="%m/%d/%y"),

   # Custody Status
   custody.status= ifelse(str_detect(custStatus, "RELEASED"), "Released",
                   ifelse(str_detect(custStatus, "DISCHARGED"), "Discharged",
		   ifelse(str_detect(custStatus, "IN CUSTODY"), "In Custody",
		   ifelse(str_detect(custStatus, "DETAINEE"), "Detainee", NA)))),

   # County of Commitment
   county=trimws(gsub(" ", "", countyCommit), which="both"),

   # Parole Hearing Type
   par.hear.type=trimws(gsub(" ", "", parHearType), which="both"),

   # Crime Type (crime.type)
   crime.type= ifelse(vfo=="VFO", "VFO",
               ifelse(str_detect(charge1, "^(MARIHUANA|CONTR\\ SUBSTANCE|CONTR\\ SUBS|METH|PRESCRIPTION)"), "Drug",
               ifelse(str_detect(charge1, "(SODOMY|SEX|RAPE|INCEST)"), "Sex",
	       "Other"))),

   # Admission Type (adm.type)
   adm.type= ifelse(admType=="RETURN FROM PAROLE/COND REL", "Parole/Cond.Rel Revoc",
             ifelse(admType=="NEW COMMITMENT", "New Commitment",
	     "Other")),

   # Current Age (age.current, age.current.group)
   age.current=round(as.numeric(as.Date("2020-02-14", format="%Y-%m-%d")-DOB)/365.25,1),
   age.current.group=ifelse(age.current<18, "Under 18",
                     ifelse(age.current>=18 & age.current<21, "age18-20",
		     ifelse(age.current>=21 & age.current<25, "age21-24",
		     ifelse(age.current>=25 & age.current<30, "age25-29",
		     ifelse(age.current>=30 & age.current<35, "age30-34",
		     ifelse(age.current>=35 & age.current<40, "age35-39",
		     ifelse(age.current>=40 & age.current<45, "age40-44",
		     ifelse(age.current>=45 & age.current<50, "age45-49",
		     ifelse(age.current>=50 & age.current<55, "age50-54",
		     ifelse(age.current>=55 & age.current<60, "age55-59",
		     ifelse(age.current>=60, "60 or over", NA))))))))))),
   age.current.group=factor(age.current.group,
                            levels=c("Under 18", "age18-20", "age21-24", "age25-29",
				     "age30-34", "age35-39", "age40-44", "age45-49",
				     "age50-54", "age55-59", "60 or over")),

   # Age at Admission (age.adm, age.adm.group)
   age.adm=round(as.numeric((curr.adm.date-DOB))/365.25,1),
   age.adm.group=ifelse(age.adm<18, "Under 18",
                 ifelse(age.adm>=18 & age.adm<21, "age18-20",
		 ifelse(age.adm>=21 & age.adm<25, "age21-24",
		 ifelse(age.adm>=25 & age.adm<30, "age25-29",
		 ifelse(age.adm>=30 & age.adm<35, "age30-34",
		 ifelse(age.adm>=35 & age.adm<40, "age35-39",
		 ifelse(age.adm>=40 & age.adm<45, "age40-44",
		 ifelse(age.adm>=45 & age.adm<50, "age45-49",
		 ifelse(age.adm>=50 & age.adm<55, "age50-54",
		 ifelse(age.adm>=55 & age.adm<60, "age55-59",
		 ifelse(age.adm>=60, "60 or over", NA))))))))))),
   age.adm.group=factor(age.adm.group,
                        levels=c("Under 18", "age18-20", "age21-24", "age25-29",
				 "age30-34", "age35-39", "age40-44", "age45-49",
				 "age50-54", "age55-59", "60 or over")),

   # NYC vs Non-NYC (NYC)
   NYC=ifelse(county %in%  c("BRONX", "KINGS", "QUEENS", "NEW YORK", "RICHMOND"), "NYC",
       ifelse(!is.na(county), "Non-NYC", NA)),

   # Latest Release Type
   latest.rel.type=str_replace(latestRelease, "[0-9]{2}\\/[0-9]{2}\\/[0-9]{2}\\s", ""),
   latest.rel.type=trimws(gsub(" ", "", latest.rel.type), which="both"),

   # Sentence Length
   min.sent.yr=ifelse(str_detect(aggMinSent, "LIFE"), 9999, aggMinSentYears),
   min.sent.mo=aggMinSentMonths,
   min.sent.day=aggMinSentDays,

   max.sent.yr=ifelse(str_detect(aggMaxSent, "LIFE"), 9999, aggMaxSentYears),
   max.sent.mo=aggMaxSentMonths,
   max.sent.day=aggMaxSentDays,

   # Total Sentence Length in days (agg.min.sent, agg.max.sent)
   agg.min.sent=min.sent.yr*365 + min.sent.mo*30 + min.sent.day,
   agg.max.sent=max.sent.yr*365 + max.sent.mo*30 + max.sent.day,

   # Min/Max Sentence Length Imposed (min.sent, max.sent)
   min.sent=paste0(min.sent.yr, " yr ", min.sent.mo, " mo ", min.sent.day, " day"),
   max.sent=paste0(max.sent.yr, " yr " , max.sent.mo, " mo ", max.sent.day, " day"),

   min.sent=str_replace(min.sent, "^0\\ yr", ""),
   min.sent=str_replace(min.sent, "\\ 0\\ mo", ""),
   min.sent=str_replace(min.sent, "\\ 0\\ day", ""),
   min.sent=trimws(min.sent, which="both"),
   min.sent=ifelse(str_detect(min.sent, "9999 yr"), "LIFE", min.sent),
   min.sent=ifelse(str_detect(min.sent, "NA"), NA, min.sent),

   max.sent=str_replace(max.sent, "^0\\ yr", ""),
   max.sent=str_replace(max.sent, "\\ 0\\ mo", ""),
   max.sent=str_replace(max.sent, "\\ 0\\ day", ""),
   max.sent=trimws(max.sent, which="both"),
   max.sent=ifelse(str_detect(max.sent, "9999 yr"), "LIFE", max.sent),
   max.sent=ifelse(str_detect(max.sent, "NA"), NA, max.sent),

   ## Min/Max Sentence length group (agg.min.sent.group, agg.max.sent.group)
   agg.min.sent.yr=round(agg.min.sent/365.25,1),
   agg.max.sent.yr=round(agg.max.sent/365.25,1),
   agg.min.sent.group=ifelse(agg.min.sent.yr<3, "Less than 3yrs",
                      ifelse(agg.min.sent.yr>=3 & agg.min.sent.yr<5, "3-4yrs",
                      ifelse(agg.min.sent.yr>=5 & agg.min.sent.yr<10, "5-9yrs",
                      ifelse(agg.min.sent.yr>=10 & agg.min.sent.yr<15, "10-14yrs",
                      ifelse(agg.min.sent.yr>=15 & agg.min.sent.yr<20, "15-19yrs",
                      ifelse(agg.min.sent.yr>=30 & agg.min.sent.yr<100, "20+yrs(non-Life)", NA)))))),
   agg.min.sent.group=ifelse(min.sent=="LIFE", "Life", agg.min.sent.group),
   agg.min.sent.group=factor(agg.min.sent.group,
	                     levels=c("Less than 3yrs","3-4yrs","5-9yrs","10-14yrs","15-19yrs",
	  			      "20+yrs(non-Life)", "Life")),
   agg.max.sent.group=ifelse(agg.max.sent.yr>=1 & agg.max.sent.yr<3, "Less than 3yrs",
                      ifelse(agg.max.sent.yr>=3 & agg.max.sent.yr<5, "3-4yrs",
                      ifelse(agg.max.sent.yr>=5 & agg.max.sent.yr<10, "5-9yrs",
                      ifelse(agg.max.sent.yr>=10 & agg.max.sent.yr<15, "10-14yrs",
                      ifelse(agg.max.sent.yr>=15 & agg.max.sent.yr<20, "15-19yrs",
                      ifelse(agg.max.sent.yr>=30 & agg.max.sent.yr<100, "30+yrs(non-Life)", NA)))))),
   agg.max.sent.group=ifelse(max.sent=="LIFE", "Life", agg.max.sent.group),
   agg.max.sent.group=factor(agg.max.sent.group,
	                     levels=c("Less than 3yrs","3-4yrs","5-9yrs","10-14yrs","15-19yrs",
	  			      "20+yrs(non-Life)", "Life")),

   # Sentence Date (sent.date)
   sent.date=ifelse(admType=="NEW COMMITMENT" & str_detect(aggMaxSent, "LIFE")==FALSE,
                    as.Date(maxExpDate, format="%m/%d/%Y")-agg.max.sent,  ## maximum expiration date-maximum sentence length
             ifelse(admType=="NEW COMMITMENT" & str_detect(aggMaxSent, "LIFE")==TRUE,
	            as.Date(parEligDate, format="%m/%d/%Y")-agg.min.sent, ## parole eligibility date-minimum sentence length
		    as.Date(origDateRec, format="%m/%d/%Y"))),
   sent.date=ifelse(admType=="NEW COMMITMENT" & is.na(sent.date),
                    as.Date(origDateRec, format="%m/%d/%Y"), sent.date),
   sent.date=as.Date(sent.date, format="%Y-%m-%d"),

   # Crime Date (crime.date)
   crime.date=ifelse(!is.na(sent.date),
                     as.Date(sent.date, format="%Y-%m-%d")-180,
                             NA), ## ASSUME 6 months between crime-sentence
   crime.date=as.Date(crime.date, format="%Y-%m-%d"),

   # Age of Crime (age.crime, age.crime.group)
   age.crime=round(as.numeric((crime.date-DOB))/365.25,1),
   age.crime.group=ifelse(age.crime<18, "Under 18",
                   ifelse(age.crime>=18 & age.crime<21, "age18-20",
		   ifelse(age.crime>=21 & age.crime<25, "age21-24",
		   ifelse(age.crime>=25 & age.crime<30, "age25-29",
		   ifelse(age.crime>=30 & age.crime<35, "age30-34",
		   ifelse(age.crime>=35 & age.crime<40, "age35-39",
		   ifelse(age.crime>=40 & age.crime<45, "age40-44",
		   ifelse(age.crime>=45 & age.crime<50, "age45-49",
		   ifelse(age.crime>=50 & age.crime<55, "age50-54",
		   ifelse(age.crime>=55 & age.crime<60, "age55-59",
		   ifelse(age.crime>=60, "60 or over", NA))))))))))),
   age.crime.group=factor(age.crime.group,
                          levels=c("Under 18", "age18-20", "age21-24", "age25-29",
				   "age30-34", "age35-39", "age40-44", "age45-49",
				   "age50-54", "age55-59", "60 or over"))
)

# Years Served (yrs.served, years.served.group)
# Proportion of min.max sentence served (prop.min.served, prop.max.served,
# prop.min.served.group, prop.max.served.group)

doccs <- doccs %>%
   mutate(

   ## Years served
   yrs.served=round(as.numeric(as.Date("2020-02-14", format="%Y-%m-%d")-sent.date)/365.25,1),
   yrs.served.group=ifelse(yrs.served>0 &  yrs.served<1, "Less than 3yrs",
		    ifelse(yrs.served>=3 &  yrs.served<5, "3-4yrs",
		    ifelse(yrs.served>=5 &  yrs.served<10, "5-9yrs",
		    ifelse(yrs.served>=10 &  yrs.served<15, "10-14yrs",
		    ifelse(yrs.served>=15 &  yrs.served<20, "15-19yrs",
		    ifelse(yrs.served>=30 , "20+yrs", NA)))))),
   yrs.served.group=factor(yrs.served.group,
                           levels=c("Less than 3yrs","3-4yrs","5-9yrs","10-14yrs","15-19yrs",
	  			      "20+yrs")),

   ## proportion of sentence served (prop.min.served.min, prop.max.served)
   prop.min.served=round(yrs.served/agg.min.sent.yr,2),
   prop.max.served=round(yrs.served/agg.max.sent.yr,2),

   ## proportion of sentence served (prop.min.served.group, prop.max.sent.group)
   prop.min.served.group=ifelse(agg.min.sent==0, NA,
                         ifelse(prop.min.served>=0 & prop.min.served<.25, "0-24.9%",
                         ifelse(prop.min.served>=.25 & prop.min.served<.5, "25-49.9%",
                         ifelse(prop.min.served>=.5 & prop.min.served<.75, "50-74.9%",
                         ifelse(prop.min.served>=.75 & prop.min.served<1, "75-99.9%",
                         ifelse(prop.min.served>=1, "100+", NA)))))),
   prop.min.served.group=factor(prop.min.served.group,
                                levels=c("0-24.9%", "25-49.9%", "50-74.9%", "75-99.9%", "100+")),

   prop.max.served.group=ifelse(agg.max.sent==3652704, NA,
                         ifelse(prop.max.served>=0 & prop.max.served<.25, "0-24.9%",
                         ifelse(prop.max.served>=.25 & prop.max.served<.5, "25-49.9%",
                         ifelse(prop.max.served>=.5 & prop.max.served<.75, "50-74.9%",
                         ifelse(prop.max.served>=.75 & prop.max.served<1, "75-99.9%",
                         ifelse(prop.max.served>=1, "100+", NA)))))),
   prop.max.served.group=factor(prop.max.served.group,
                                levels=c("0-24.9%", "25-49.9%", "50-74.9%", "75-99.9%", "100+")))


# SENTENCE TYPE in Detail (sent.type.detail)
# : Parole Violation, LWOP, Indeterminate-LIFE, Determinate-No Min, Detereminate-6/7 Max
#   Indeterminate-Not LIFE

## only for New Commitments (New Commitment)
## aggregate minimum sentence is life (365704) - LWOP
## aggregate maximum sentence is life (365704) - Indeterminate-Life
## zero aggregate minimum sentence - Determinate-No Min
## 6/7 maximum = Minimum - Determinate-6/7 Max
## If none of the above - Indeterminate-Not LIFE

## SENTENCE TYPE (sent.type)
# Parole Violation, LOWP, Indeterminate-Life,Determinate, Indeterminate-Not LIFE

## Combine "Determinate-No min" and "Determinate-6/7 Max"

# SENTENCE TYPE2 (sent.type2)
# : Parole Violation, LWOP, Indeterminate-LIFE, Indeterminate-Not LIFE, Detereminate

## SENTENCE TYPE (sent.type2)
## If original admission date is different from current admission year, parole violation
## If min sentence is life, life without parole
## If parole eligibility date exist + LIFE as maximum sentence, Indeterminate(w LIFE)
## If parole eligibility date exists, Indeterminate
## If none of the above and either conditional or max date exist, determinate


doccs <- doccs %>%
   mutate(

   ## Sentence Type in Detail
   sent.type.detail=ifelse(adm.type!="New Commitment", NA,
             ifelse(str_detect(aggMinSent, "LIFE"), "LWOP",
             ifelse(str_detect(aggMaxSent, "LIFE"), "Indeterminate-LIFE",
	     ifelse(agg.min.sent==0 , "Determinate-No Min",
             ifelse(agg.min.sent-5>round(agg.max.sent*6/7) & round(agg.max.sent*6/7)<agg.max.sent+5, "Determinate-6/7 Max",
             "Indeterminate-Not LIFE"))))),

   ## Sentence Type
   sent.type=ifelse(str_detect(sent.type.detail, "^Determinate"),
                     "Determinate", sent.type.detail),

   ## Sentence Type 2
   sent.type2=ifelse(!is.na(orig.adm.date) & orig.adm.date!=curr.adm.date, "Parole Violation",
              ifelse(str_detect(aggMinSent, "LIFE"), "LWOP",
              ifelse(!is.na(par.elig.date) & str_detect(aggMaxSent, "LIFE"), "Indeterminate-LIFE",
	      ifelse(!is.na(par.elig.date), "Indeterminate-Not LIFE",
	      ifelse(!is.na(cond.rel.date) | !is.na(max.exp.date), "Determinate",
	      NA))))))

## Years Before Parole Eligibility (yrs.to.par.elig, yrs.to.par.elig.group)
## Days Before Conditional Release date (yrs.to.cond.rel, yrs.to.cond.rel.group)

doccs <- doccs %>%
   mutate(

   yrs.to.par.elig= round(as.numeric(par.elig.date-as.Date("2020-02-14", format="%Y-%m-%d"))/365.25,2),
   yrs.to.cond.rel= round(as.numeric(cond.rel.date-as.Date("2020-02-14", format="%Y-%m-%d"))/365.25,2),

   yrs.to.par.elig.group =ifelse(yrs.to.par.elig<=0, "Passed parole eligible date",
                             ifelse(yrs.to.par.elig>0 & yrs.to.par.elig<1, "less than 1 year",
			     ifelse(yrs.to.par.elig>=1 & yrs.to.par.elig<5, "1-4 years",
			     ifelse(yrs.to.par.elig>=5 & yrs.to.par.elig<10, "5-9 years",
			     ifelse(yrs.to.par.elig>=10 & yrs.to.par.elig<14, "10-14 years",
			     ifelse(yrs.to.par.elig>=15, "15+ years", NA)))))),

   yrs.to.par.elig.group=factor(yrs.to.par.elig.group,
                                   levels=c("Passed parole eligible date",
				            "less than 1 year",
				            "1-4 years", "5-9 years",
					    "10-14 years", "15+ years")),

   yrs.to.cond.rel.group =ifelse(yrs.to.cond.rel<=0, "Passed conditional release date",
                          ifelse(yrs.to.cond.rel>0 & yrs.to.cond.rel<1, "less than 1 year",
			  ifelse(yrs.to.cond.rel>=1 & yrs.to.cond.rel<5, "1-4 years",
			  ifelse(yrs.to.cond.rel>=5 & yrs.to.cond.rel<10, "5-9 years",
			  ifelse(yrs.to.cond.rel>=10 & yrs.to.cond.rel<14, "10-14 years",
			  ifelse(yrs.to.cond.rel>=15, "15+ years", NA)))))),

   yrs.to.cond.rel.group=factor(yrs.to.cond.rel.group,
                                   levels=c("Passed conditional release date",
				            "less than 1 year",
					    "1-4 years", "5-9 years",
					    "10-14 years", "15+ years")))

# Number of charges
doccs <- doccs %>% mutate(tmp1=ifelse(!is.na(charge1),1, 0),
                          tmp2=ifelse(!is.na(charge2),1, 0),
			  tmp3=ifelse(!is.na(charge3),1, 0),
			  tmp4=ifelse(!is.na(charge4),1, 0),
			  ncharge=tmp1+tmp2+tmp3+tmp4) %>%
			  select(-c(tmp1, tmp2, tmp3, tmp4))

# Final Dataframe
vars <- c("id",
          "DIN"="din",
          "name",
          "sex",
	  "race.eth",
          "DOB",
          "county",
	  "NYC",
	  "crime.date",
	  "sent.date",
	  "orig.adm.date",
	  "curr.adm.date",
	  "earl.rel.date",
	  "earl.rel.type"="earRelType",
	  "par.hear.date",
	  "par.hear.type",
	  "par.elig.date",
	  "cond.rel.date",
	  "latest.rel.date",
          "latest.rel.type",
	  "max.exp.date",
	  "max.exp.par.sup.date",
	  "par.dis.date",
	  "custody.status",
	  "crime.type",
	  "adm.type",
	  "ncharge",
	  "charge1", "charge2", "charge3", "charge4",
	  "class1", "class2", "class3", "class4",
	  "age.current", "age.current.group",
	  "age.adm", "age.adm.group",
	  "age.crime", "age.crime.group",
	  "min.sent", "max.sent",
	  "agg.min.sent", "agg.max.sent",
	  "agg.min.sent.group", "agg.max.sent.group",
	  "yrs.served", "yrs.served.group",
	  "prop.min.served", "prop.max.served",
	  "prop.min.served.group", "prop.max.served.group",
	  "sent.type","sent.type.detail", "sent.type2",
	  "min.sent.yr", "min.sent.mo", "min.sent.day", "max.sent.yr", "max.sent.mo", "max.sent.day",
	  "yrs.to.par.elig", "yrs.to.par.elig.group",
	  "yrs.to.cond.rel", "yrs.to.cond.rel.group")

clean <- doccs %>%
  select(vars)

ds <- clean %>%
      dplyr::mutate(date.adm=curr.adm.date,
            date.age85=as.Date(DOB+85*365.25, format="%Y-%m-%d")) # date when age 85

#+end_src


* Write out working data file
#+begin_src R :session :results silent

saveRDS(ds, "data/doccs.clean.2020.02.14.rds")

#+end_src

