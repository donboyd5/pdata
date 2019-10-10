# 10/10/2019

# Get Public Plans Database (PPD), now (5/1/2015 and later) sponsored cooperatively by:
#   Center for State and Local Government Excellence
#   Center for Retirement Research
#   National Association of Retirement Administrators

#****************************************************************************************************
#                Load packages ####
#****************************************************************************************************

library("devtools")
library("usethis")

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


#****************************************************************************************************
#                Define directories, filenames, etc. ####
#****************************************************************************************************
# djb 10/10/2019:
#  Go to https://publicplansdata.org/public-plans-database/download-full-data-set/#"
#  and manually download the data set to /data-raw/ppd

ppd.dir <- "./data-raw/ppd/"

vlist.webdir <- "http://publicplansdata.org/wp-content/uploads/2015/04/" # maybe update this?
vlist.fn <- "Variable-List1.xlsx"
download.file(paste0(vlist.webdir, vlist.fn), paste0(ppd.dir, vlist.fn), mode="wb")
# rename to 2019-10-10_Variable-List1.xlsx


#****************************************************************************************************
#                Download and save raw data, with download date prefixed to file names ####
#****************************************************************************************************
# download.file(ppd.u, paste0(ppd.dir, ppd.localfn), mode="wb")
# download.file(vlist.u, paste0(ppd.dir, vlist.localfn), mode="wb")


#****************************************************************************************************
#                Clean variable info slightly, then save in package data directory ####
#****************************************************************************************************
fn <- "2019-10-10_Variable-List1.xlsx"
df <- read_excel(paste0(ppd.dir, fn), sheet="Codebook_revised")
glimpse(df)
# doesn't really look to need cleaning
ppdvars <- df %>%
  rename(`Data sources_secondary`=`...8`,
         Imputations_secondary=`...10`) %>%
  filter(row_number()!=1)
glimpse(ppdvars)
comment(ppdvars) <- fn
usethis::use_data(ppdvars, overwrite=TRUE)


#****************************************************************************************************
#                Get raw ppd data and save in package data directory ####
#****************************************************************************************************
# read all as character, just to be safe
# ncols <- read_excel(paste0(ppd.dir, ppd.localfn)) %>% ncol(.)
# df <- read_excel(paste0(ppd.dir, ppd.localfn), col_types=rep("text", ncols))
ppd.fn <- "2019-10-10_ppd-data-latest.csv"
df <- read_csv(paste0(ppd.dir, ppd.fn), col_types = list(.default = col_character()))
glimpse(df)

vlook <- function(vname, df) df %>% .[[vname]] %>% unique

# take a first cut at col types using type_convert from readr
df2 <- type_convert(df)
# cols_condense(df2)
# spec(df2)
# cols_condense(spec(df2))
# df2 <- type_convert(df, col_types=(cols(.default = col_double())))
glimpse(df2)

tmp <- sapply(df2, class)
vtypes <- tibble(vname=names(tmp), vtype=unname(tmp))
count(vtypes, vtype)
vtypes

vtypes %>% filter(vtype=="Date")
vtypes %>% filter(vtype %in% c("character", "logical"))

f <- function(var) !is.double(var)
dfcheck <- df2 %>%
  select_if(f)
glimpse(dfcheck)

dfcheck2 <- df2 %>%
  select_if(is.logical)
dfcheck2 <- df %>% select(names(dfcheck2))
glimpse(dfcheck2) # no useful data in here

count(df, fye)
df$fye %>% unique %>% sort

(yearvars <- str_subset(names(df), coll("year", ignore_case = TRUE)))
for(var in yearvars) {
  print(var)
  df[[var]] %>% unique %>% sort %>% print
}
df %>%
  filter(InvestmentReturn_LTStartYear=="1") %>%
  group_by(ppd_id, PlanName, system_id) %>%
  summarise(n=n())

(datevars <- str_subset(names(df), coll("date", ignore_case = TRUE)))

dfcheck3 <- df2 %>%
  select_if(is.double)
glimpse(dfcheck3) # no useful data in here
summary(dfcheck3)
tmp <- precis2(dfcheck3)
print(tmp)


# now we are ready to create a fairly clean file
# start with df2, convert logical to character
(ltoc <- vtypes %>% filter(vtype=="logical") %>%.[["vname"]])

dfgood <- df2 %>%
  mutate_if(is.logical, as.character)
glimpse(dfgood)

precis2(dfgood) %>% as.data.frame

ppd.raw <- dfgood

comment(ppd.raw) <- ppd.fn
usethis::use_data(ppd.raw, overwrite=TRUE)


#****************************************************************************************************
#                Clean ppd data slightly and save in package data directory ####
#****************************************************************************************************
load("./data/ppd.raw.rda")
glimpse(ppd.raw)

ppd.raw %>%
  select_if(is.Date) %>%
  gather(vname, date) %>%
  group_by(vname) %>%
  summarise(min=min(date, na.rm=TRUE), max=max(date, na.rm=TRUE))

# now look for some outliers / bad values
df2 <- ppd.raw
names(df2)
str_subset(names(df2), coll("asset", ignore_case=TRUE))
vars <- c("ActFundedRatio_GASB", "MktAssets_net", "MktAssets_ActRpt")

f <- function(vars) {
  out <- df2 %>% select(fy, ppd_id, PlanName, one_of(vars)) %>%
    gather(variable, value, -fy, -ppd_id, -PlanName) %>%
    group_by(variable, fy) %>%
    do(qtiledf(.$value))
  return(out)
}
f("ActFundedRatio_GASB")
f("MktAssets_net")
f("MktAssets_ActRpt")
# better than they used to be

# apply any fixes
df3 <- df2


# Now create a few useful factors
# Classifiers:
# PlanType 1-PERS or SERS, 2- TRS, 3- Police/Fire/Safety
# AdministeringGovt  0-State, 1-County, 2-City, 5- School
ptlevs <- c(1, 2, 3, 4, 5)
ptlabs <- c("General-S&L", "Teachers", "Safety", "General-State", "General-Local")

adlevs <- c(0, 1, 2, 5)
adlabs <- c("State", "County", "City", "School")

ppd <- df3 %>% mutate(planf=factor(PlanType, levels=ptlevs, labels=ptlabs),
                      adminf=factor(AdministeringGovt, levels=adlevs, labels=adlabs),
                      pctdollf=factor(FundingMethCode1_GASB, levels=c(1, 0, NA), labels=c("levpercent", "levdollar")),
                      openclosedf=factor(FundingMethCode2_GASB, levels=1:3, labels=c("open", "fixed", "closed")),
                      assetmethf=factor(AssetValMethCode_GASB, levels=1:0, labels=c("market", "smoothed")))

count(ppd, planf, adminf)
count(ppd, pctdollf)
count(ppd, openclosedf)
count(ppd, assetmethf)

comment(ppd) <- comment(ppd.raw)
usethis::use_data(ppd, overwrite=TRUE)

