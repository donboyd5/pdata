
# Get Public Plans Database (PPD), now (5/1/2015) sponsored cooperatively by:
#   Center for State and Local Government Excellence
#   Center for Retirement Research
#   National Association of Retirement Administrators

# parent directory for PPD data:
#   http://publicplansdata.org/public-plans-database/download-full-data-set/
# Detailed documentation is at:
#   http://publicplansdata.org/public-plans-database/documentation/

#****************************************************************************************************
#                Load packages ####
#****************************************************************************************************
library("btools")
library("dplyr")
library("lubridate")
library("readr")
library("readxl")
library("stringr")
library("tidyr")


#****************************************************************************************************
#                Define directories, filenames, etc. ####
#****************************************************************************************************

ppd.dir <- "./data-raw/ppd/"

# ppd.webdir <- "http://publicplansdata.org/wp-content/uploads/2015/04/"
# ppd.webfn <- "PPD_PlanLevelData.csv"

ppd.webdir <- "http://publicplansdata.org/wp-content/uploads/2016/02/"
ppd.webfn <- "PPD_PlanLevelData.xlsx"

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
ncols <- ncol(read_csv(paste0(ppd.dir, ppd.localfn), n_max=1)) # determine # columns
df <- read_csv(paste0(ppd.dir, ppd.localfn), col_types=str_dup("c", ncols)) # read ALL columns as character and convert as needed
df2 <- type_convert(df, col_types = NULL)
glimpse(df2)
problems(df2)

ncols <- ncol(read_excel(paste0(ppd.dir, ppd.localfn))) # determine # columns
df <- read_excel(paste0(ppd.dir, ppd.localfn), col_types=rep("text", ncols)) # read ALL columns as character and convert as needed
df2 <- type_convert(df, col_types = NULL)
glimpse(df2)
problems(df2)


# convert all of the date variables
grep("date", names(df2), ignore.case=TRUE, value=TRUE)
# str_detect(names(df2), "date")
datedf <- df2 %>% select(ppd_id, fy, fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules)
datedfl <- datedf %>% gather(variable, value, -ppd_id, -fy) %>%
  mutate(chardate=grepl("[[:alpha:]]", value),
         dmyd=as.Date(dmy(ifelse(chardate, value, NA))),
         mdyd=as.Date(mdy(ifelse(!chardate, value, NA))),
         date=as.Date(ifelse(chardate, dmyd, mdyd), origin="1970-01-01"))
datew <- select(datedfl, ppd_id, fy, variable, date) %>% spread(variable, date)

# Now put the cleaned up dates into the data


df3 <- df2 %>% select(-c(fye, ActRptDate, ActValDate_GASBAssumptions, ActValDate_GASBSchedules)) %>%
  left_join(datew)

df4 <- df3 %>% filter(!is.na(ppd_id))
head(df4)

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


