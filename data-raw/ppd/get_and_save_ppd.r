
# Get Public Plans Database (PPD), now (5/1/2015) sponsored cooperatively by:
#   Center for State and Local Government Excellence
#   Center for Retirement Research
#   National Association of Retirement Administrators

# parent directory for PPD data:
#   http://publicplansdata.org/public-plans-database/download-full-data-set/
# Detailed documentation is at:
#   http://publicplansdata.org/public-plans-database/documentation/

#****************************************************************************************************
#
#                Load packages ####
#
#****************************************************************************************************
library(btools)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(stringr)
library(tidyr)


#****************************************************************************************************
#
#                Define directories, filenames, etc. ####
#
#****************************************************************************************************

ppd.dir <- "./data-raw/ppd/"

ppd.webdir <- "http://publicplansdata.org/wp-content/uploads/2015/04/"
ppd.webfn <- "PPD_PlanLevelData.csv"
vlist.webfn <- "Variable-List1.xlsx"

ppd.u <- paste0(ppd.webdir, ppd.webfn)
vlist.u <- paste0(ppd.webdir, vlist.webfn)

downloaddate <- format(Sys.time(), '%Y-%m-%d')
# downloaddate <- "2015-05-01" # or examine data from a specific date

# local filenames
ppd.localfn <- paste0(downloaddate, "_", ppd.webfn)
vlist.localfn <- paste0(downloaddate, "_", vlist.webfn)




#****************************************************************************************************
#
#                Download and save raw data, with download date prefixed to file names ####
#
#****************************************************************************************************

download.file(ppd.u, paste0(ppd.dir, ppd.localfn), mode="wb")
download.file(vlist.u, paste0(ppd.dir, vlist.localfn), mode="wb")


#****************************************************************************************************
#
#                Clean variable info slightly, then save in package data directory ####
#
#****************************************************************************************************
df <- read_excel(paste0(ppd.dir, vlist.localfn))
glimpse(df)
# doesn't really look to need cleaning
ppdvars <- df
devtools::use_data(ppdvars, overwrite=TRUE)


#****************************************************************************************************
#
#                Clean ppd data slightly, then save in package data directory ####
#
#****************************************************************************************************
# read all data as character and then convert; this appears to work better than letting read_csv use its defaults
ncols <- ncol(read_csv(paste0(ppd.dir, ppd.localfn), n_max=1)) # determine # columns
df <- read_csv(paste0(ppd.dir, ppd.localfn), col_types=str_dup("c", ncols)) # read ALL columns as character and convert as needed
df2 <- type_convert(df, col_types = NULL)
glimpse(df2)
problems(df2)

# convert all of the date variables
grep("date", names(df2), ignore.case=TRUE, value=TRUE)
df3 <- df2 %>% mutate_each(funs(dmy(.)), fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules)
# which dates did not parse?
names(df2)
# get character version of dates
cdates <- df2 %>% select(ppd_id, fy, fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules) %>%
  gather(variable, cdate, -ppd_id, -fy) %>%
  filter(!is.na(cdate))
# get date version of dates
ddates <- df3 %>% select(ppd_id, fy, fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules) %>%
  gather(variable, ddate, -ppd_id, -fy) %>%
  filter(!is.na(ddate))
comp <- left_join(cdates, ddates) %>% filter(is.na(ddate), cdate!="")
comp
# this all looks like bad data and we probably need to delete the records
count(df2, is.na(ppd_id))
df2 %>% filter(is.na(ppd_id))
# judging by the csv file the bad records look related to:
# 41	Kentucky ERS	near 2001
# 110	TN State and Teachers	near 2013
# tell JP, but for now, delete
ppd <- df3 %>% filter(!is.na(ppd_id))

devtools::use_data(ppd, overwrite=TRUE)


