# 11/12/2016

# Get Public Plans Database (PPD), now (5/1/2015) sponsored cooperatively by:
#   Center for State and Local Government Excellence
#   Center for Retirement Research
#   National Association of Retirement Administrators

#****************************************************************************************************
#                Load packages ####
#****************************************************************************************************

library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr
library("hms") # hms, for times.
library("stringr") # stringr, for strings.
library("lubridate") # lubridate, for date/times.
library("forcats") # forcats, for factors.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)


#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
safe.ifelse <- function(cond, yes, no) {structure(ifelse(cond, yes, no), class = class(yes))} # so dates don't lose their class!


#****************************************************************************************************
#                Define directories, filenames, etc. ####
#****************************************************************************************************
# parent directory for PPD data:
#   http://publicplansdata.org/public-plans-database/download-full-data-set/
# Detailed documentation is at:
#   http://publicplansdata.org/public-plans-database/documentation/


# The latest data APPEAR to be at:
#   http://publicplansdata.org/wp-content/uploads/2016/04/PPD_PlanLevel.xlsx
# There are also data at:
#   http://publicplansdata.org/wp-content/uploads/2015/04/PPD_PlanLevelData.csv
# presumably, based on directory name, these are older.



# latest full links
# http://publicplansdata.org/wp-content/uploads/2016/04/PPD_PlanLevel.xlsx
# http://publicplansdata.org/wp-content/uploads/2015/04/Variable-List1.xlsx

ppd.dir <- "./data-raw/ppd/"

# ppd.webdir <- "http://publicplansdata.org/wp-content/uploads/2015/04/"
# ppd.webfn <- "PPD_PlanLevelData.csv"

# ppd.webdir <- "http://publicplansdata.org/wp-content/uploads/2016/02/"
# ppd.webfn <- "PPD_PlanLevelData.xlsx"

ppd.webdir <- "http://publicplansdata.org/wp-content/uploads/2016/04/"
ppd.webfn <- "PPD_PlanLevel.xlsx"

vlist.webdir <- "http://publicplansdata.org/wp-content/uploads/2015/04/"
vlist.webfn <- "Variable-List1.xlsx"


ppd.u <- paste0(ppd.webdir, ppd.webfn)
vlist.u <- paste0(vlist.webdir, vlist.webfn)

downloaddate <- format(Sys.time(), '%Y-%m-%d')
# downloaddate <- "2015-05-01" # or examine data from a specific date

# local filenames
ppd.localfn <- paste0(downloaddate, "_", ppd.webfn)
vlist.localfn <- paste0(downloaddate, "_", vlist.webfn)


#****************************************************************************************************
#                Download and save raw data, with download date prefixed to file names ####
#****************************************************************************************************

download.file(ppd.u, paste0(ppd.dir, ppd.localfn), mode="wb")
download.file(vlist.u, paste0(ppd.dir, vlist.localfn), mode="wb")


#****************************************************************************************************
#                Clean variable info slightly, then save in package data directory ####
#****************************************************************************************************
df <- read_excel(paste0(ppd.dir, vlist.localfn))
glimpse(df)
# doesn't really look to need cleaning
ppdvars <- df
comment(ppdvars) <- vlist.localfn
devtools::use_data(ppdvars, overwrite=TRUE)


#****************************************************************************************************
#                Clean ppd data slightly, then save in package data directory ####
#****************************************************************************************************
# read all data as character and then convert; this appears to work better than letting read_csv use its defaults
# ncols <- ncol(read_csv(paste0(ppd.dir, ppd.localfn), n_max=10)) # determine # columns
# df <- read_csv(paste0(ppd.dir, ppd.localfn), col_types=str_dup("c", ncols)) # read ALL columns as character and convert as needed
# df2 <- type_convert(df, col_types = NULL)
# glimpse(df2)
# problems(df2)

# ppd.localfn <- "2016-05-09_PPD_PlanLevel.xlsx"
# tmp <- read_excel(paste0(ppd.dir, ppd.localfn))
# tmp %>% mutate(month=month(fye)) %>% count(month)
# d <- 42185 # 6/30/2015
# as.Date(d, origin="1899-12-30") # NOT 1900-01-01 !!

ncols <- read_excel(paste0(ppd.dir, ppd.localfn)) %>% ncol(.)
df <- read_excel(paste0(ppd.dir, ppd.localfn), col_types=rep("text", ncols))

# clean date vars
dv2 <- df %>% select(ppd_id, fy, fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules, ActValDate_ActuarialCosts)
dv3 <- dv2 %>% mutate_each(funs(as.Date(as.numeric(.), origin="1899-12-30")), fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules) %>%
  mutate(ActValDate_ActuarialCosts=as.Date(mdy(ActValDate_ActuarialCosts)))
count(dv3, fy, fye)

df2 <- left_join(df %>% select(-c(fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules, ActValDate_ActuarialCosts)),
                 dv3)
glimpse(df2)
df3 <- type_convert(df2, col_types = NULL)
glimpse(df3)
problems(df3)

# any bad dates?
df3 %>% select(ppd_id, PlanName, fy, fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules, ActValDate_ActuarialCosts) %>% summary
baddates <- df3 %>% select(ppd_id, PlanName, fy, fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules, ActValDate_ActuarialCosts) %>%
  gather(datevar, date, -c(ppd_id, PlanName, fy)) %>%
  filter(year(date)>2016 | year(date)<2000)
baddates

df3 %>% select(ppd_id, PlanName, fy, fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules, ActValDate_ActuarialCosts) %>% glimpse

# Fix any obvious errors
df4 <- df3 %>%
  mutate(ActValDate_GASBAssumptions=safe.ifelse(ActValDate_GASBAssumptions=="2019-01-01",
                                                as.Date("2009-01-01"),
                                                ActValDate_GASBAssumptions)) %>%
  mutate(ActValDate_ActuarialCosts=safe.ifelse((ppd_id==161) & year(ActValDate_ActuarialCosts)==1905,
                                               as.Date(ymd(paste(fy, month(ActValDate_ActuarialCosts), day(ActValDate_ActuarialCosts)))),
                                               ActValDate_ActuarialCosts))
glimpse(df4)
baddates %>% select(ppd_id, fy) %>%
  left_join(df4) %>%
  select(ppd_id, PlanName, fy, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules, ActValDate_ActuarialCosts)


# df4 <- df3
# df4$ActValDate_GASBAssumptions[year(df4$ActValDate_GASBAssumptions)==2019 & !is.na(df4$ActValDate_GASBAssumptions)] <- as.Date("2009-01-03")


# Now create a few useful factors
# Classifiers:
# PlanType 1-PERS or SERS, 2- TRS, 3- Police/Fire/Safety
# AdministeringGovt  0-State, 1-County, 2-City, 5- School
ptlevs <- c(1, 2, 3, 4, 5)
ptlabs <- c("General-S&L", "Teachers", "Safety", "General-State", "General-Local")

adlevs <- c(0, 1, 2, 5)
adlabs <- c("State", "County", "City", "School")

ppd <- df4 %>% mutate(planf=factor(PlanType, levels=ptlevs, labels=ptlabs),
                      adminf=factor(AdministeringGovt, levels=adlevs, labels=adlabs),
                      pctdollf=factor(FundingMethCode1_GASB, levels=c(1, 0, NA), labels=c("levpercent", "levdollar")),
                      openclosedf=factor(FundingMethCode2_GASB, levels=1:3, labels=c("open", "fixed", "closed")),
                      assetmethf=factor(AssetValMethCode_GASB, levels=1:0, labels=c("market", "smoothed"))
                      )

comment(ppd) <- ppd.localfn
devtools::use_data(ppd, overwrite=TRUE)


