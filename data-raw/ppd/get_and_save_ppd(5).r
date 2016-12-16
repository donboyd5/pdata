# 11/12/2016

# Get Public Plans Database (PPD), now (5/1/2015) sponsored cooperatively by:
#   Center for State and Local Government Excellence
#   Center for Retirement Research
#   National Association of Retirement Administrators

#****************************************************************************************************
#                Load packages ####
#****************************************************************************************************

library("devtools")

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

# Detailed interactive documentation is at:
#   http://publicplansdata.org/public-plans-database/documentation/

ppd.dir <- "./data-raw/ppd/"

ppd.webdir <- "http://publicplansdata.org/wp-content/uploads/2016/12/"
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

## Older links
# ppd.webdir <- "http://publicplansdata.org/wp-content/uploads/2015/04/"
# ppd.webfn <- "PPD_PlanLevelData.csv"

# ppd.webdir <- "http://publicplansdata.org/wp-content/uploads/2016/02/"
# ppd.webfn <- "PPD_PlanLevelData.xlsx"



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
#                Get raw ppd data and save in package data directory ####
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

# read all as character, just to be safe
ncols <- read_excel(paste0(ppd.dir, ppd.localfn)) %>% ncol(.)
df <- read_excel(paste0(ppd.dir, ppd.localfn), col_types=rep("text", ncols))
glimpse(df)

vlook <- function(vname, df) df %>% .[[vname]] %>% unique

# take a first cut at col types using type_convert from readr
df2 <- type_convert(df)
# cols_condense(df2)
# spec(df2)
# cols_condense(spec(df2))
# df2 <- type_convert(df, col_types=(cols(.default = col_double())))
glimpse(df2)

# now reset types (pulling values from original df) where type_convert results aren't what we want
# reset all date variables as character because they need to be inspected and converted
datevars <- str_subset(names(df), coll("date", ignore_case = TRUE))
datevars
datevars <- c("fye", datevars)
datevars

# look for integers that should be doubles
glimpse(df2 %>% select_if(is.integer))
dblvars <- c("ActLiabilities_other", "expense_PrivateEquity")
# note that the membership data are integers; leave that way for now

# now replace and convert as needed
df3 <- df2
replacevars <- c(datevars, dblvars)
df3[, replacevars] <- df[, replacevars]
glimpse(df3)

df4 <- df3 %>% mutate_at(vars(one_of(dblvars)), funs(as.double))
glimpse(df4)
glimpse(df4[, dblvars])

# VERIFY that the ORDER of df and df4 are the same, as replacement relied on that assumption
o1 <- df %>% select(ppd_id, fy)
o2 <- df4 %>% select(ppd_id, fy) %>% mutate_all(as.character)
all.equal(o1, o2)

ppd.raw <- df4

comment(ppd.raw) <- ppd.localfn
devtools::use_data(ppd.raw, overwrite=TRUE)



#****************************************************************************************************
#                Clean ppd data slightly and save in package data directory ####
#****************************************************************************************************
load("./data/ppd.raw.rda")
glimpse(ppd.raw)

datevars <- c("fye", str_subset(names(ppd.raw), coll("date", ignore_case = TRUE)))
datevars

# tmp <- df %>% select(ppd_id, fy, one_of(datevars))

# clean date vars
dv2 <- ppd.raw %>% select(ppd_id, fy, one_of(datevars))
# because the date variables were in Excel they are a real mix of formats
mdyfmt <- "ActValDate_ActuarialCosts"
xlfmt <- setdiff(datevars, mdyfmt)


dv3 <- dv2 %>% mutate_at(vars(one_of(xlfmt)), funs(as.numeric(.) %>% as.Date(origin="1899-12-30"))) %>%
  mutate_at(vars(one_of(mdyfmt)), funs(mdy(.) %>% as.Date()))

# find the one that didn't parse
comp <- dv2 %>% select(ppd_id, fy, ActValDate_ActuarialCosts.c=ActValDate_ActuarialCosts) %>%
  filter(!is.na(ActValDate_ActuarialCosts.c)) %>%
  left_join(dv3 %>% select(ppd_id, fy, ActValDate_ActuarialCosts)) %>%
  filter(is.na(ActValDate_ActuarialCosts))
# good - was N/A in original

df2 <- ppd.raw
df2[, datevars] <- dv3[, datevars]

# again, verify that order is same as we count on that
o1 <- ppd.raw %>% select(ppd_id, fy)
o2 <- df2 %>% select(ppd_id, fy)
all.equal(o1, o2)
# good

# now look for bad data
# any bad dates?
df2 %>% select(ppd_id, PlanName, fy, one_of(datevars)) %>% summary
# no obvious problems

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

comment(ppd) <- comment(ppd.raw)
devtools::use_data(ppd, overwrite=TRUE)
