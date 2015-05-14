# MakeStateLevelSummaryDataFromVariousCensusRetirementFiles.r
# Don Boyd
# 5/14/2015

# This program creates state-level summary data on retirement systems using Census data, for as many years as possible.
# While I had wanted to build state summaries from details for individual systems, several years have imputations for
# individual systems that at present are not available publicly and thus I use summary data already compiled by the Census Bureau.

# The program gets the data from several different Census Bureau sources:

# 1) 1957 through 1992:
#    1957-2007 selected years are in the 59mb historical database found in the Census Bureau's "special60" directory at:
#      http://www2.census.gov/pub/outgoing/govs/special60/1_Emp_Retire_Historical_DB.zip
#    According to the Census Bureau documentation:

#         The primary purpose of this data base is to provide public employee-retirement
#         system data for years FY 1992 and earlier, the period predating the statistics
#         found on the Census Bureau web site. For comparison purposes, data from the census
#         of governments for 1997, 2002, 2007 are also included. Data for intercensal years

#    The file we need is the 7mb workbook:
#      1_Emp_Retire_Sys_Nat_State_Data.xls

#   I store the full archive at E:\Data\CensusRetirementData\RetireDatabase and include only this spreadsheet here.

# 2) 1993 through 2003:
#    State summary excel files available for download from the Census web site (http://www.census.gov/govs/retire/) as a set
#    of state-level tables (Tables 2-5, for revenues, expenditures, cash, membership), plus a national summary (Table 1).
#    The file names for 1993-2002 follow the format ret99t1.xls through ret99t5.xls, ..., ret02t1.xls through ret02t5.xls
#    For 2003 the files are 2003Ret01.xls through 2003Ret05.xls

#    There are some files available for later years in this same format, but we will use a different format for them (see #3 below).

# 3) 2004 through 2011
#    The data are already summarized at the state level in the files:
#    2004retest_data.xls through 2011retest_data.xls
#    These files are in the directory:
#    E:\Data\CensusRetirementData\SummaryTables\SummaryExcelFiles

# 4) 2012+
#    Currently it looks like I need to use the data available through American Fact Finder, available through the same
#    Census download site http://www.census.gov/govs/retire/



#************************************************************************************************************************
#
#                Load packages ####
#
#************************************************************************************************************************

library(plyr)
library(dplyr)
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library(ggplot2)
library(magrittr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

library(bdata)
library(btools)


#************************************************************************************************************************
#
#                Directories, filenames, constants ####
#
#************************************************************************************************************************

cendir <- paste0("./data-raw/Census/")
censs <- paste0(cendir, "StateSummaries/")
cenxl1 <- paste0(censs, "Excel1993to2003/")
cenxl2 <- paste0(censs, "Excel2004to2011/")
affdir <- paste0(censs, "AmericanFactFinder2012plus/")



#************************************************************************************************************************
#
#                Downloads, ONLY when needed ####
#
#************************************************************************************************************************
# get the Census historical database
histdb <- "1_Emp_Retire_Historical_DB.zip"
urldir <- "http://www2.census.gov/pub/outgoing/govs/special60/"
url <- paste0(urldir, histdb)
download.file(url, paste0(cendir, histdb), mode="wb")



#************************************************************************************************************************
#
#                1957-1992: Get and clean state summary data in the historical database ####
#
#************************************************************************************************************************
# admin type codes
acodesv <- "admin, adminf
0, National total
1, State-local total
2, State
3, Local
4, Fed govt
5, County
6, Municipal
7, Township
8, Special district
9, School district
x, Other"
acodes <- read_csv(acodesv) %>% mutate_each(funs(str_trim))
names(acodes) <- str_trim(names(acodes)) # just in case spaces crept into the names

# get finance data
ssdbfin <- read_excel(paste0(cendir, ssdbfn), sheet="Finances", col_names=FALSE) # superfast;
glimpse(ssdbfin)
sasnames <- ssdbfin[12, ] %>% unlist %>% unname
sasnames
sasnames[sasnames=="SAS VAR NAME>"] <- "stadmin"
names(ssdbfin) <- tolower(sasnames)
ht(ssdbfin)
count(ssdbfin, type)
count(ssdbfin, str_sub(id, 4))
# filter(ssdbfin, grepl("52.0", state)) # 52 is national govt
# filter(ssdbfin, grepl("32.0", state)) # NM


ssdbfin2 <- ssdbfin %>%
  mutate(year=as.numeric(year),
         id=str_trim(id),
         admin=round(as.numeric(str_trim(type)))) %>% # type=x will become NA but we only want admin types 1:3
  filter(admin %in% 1:3) %>%
  mutate(adminf=acodes$adminf[match(admin, acodes$admin)],
         stabbr=as.character(stcodes$stabbr[match(str_sub(id, 1, 2), stcodes$stcen)]),
         stabbr=ifelse(str_sub(id, 1, 2)=="52", "NATL", stabbr)) %>%
  filter(!is.na(year))

count(ssdbfin2, admin, adminf)
count(ssdbfin2, pasis)
count(ssdbfin2, stabbr) # good - just 50 states, DC, and US, with no NAs
glimpse(ssdbfin2)

# now create a long file
ssdbfl <- ssdbfin2 %>% select(-c(sortcode, id, state, fips, pasis, stadmin, type)) %>%
  gather(variable, value, -c(year, stabbr, admin, adminf)) %>%
  mutate(value=ifelse(grepl("-11111", value), NA, value),
         value=cton(value))
ht(ssdbfl)
count(ssdbfl, year)
count(ssdbfl, admin, adminf)
count(ssdbfl, stabbr)
count(ssdbfl, variable)

saveRDS(ssdbfl, paste0(cendir, "statesummaryhist_fin.rds"))


# get membership data
ssdbmem <- read_excel(paste0(cendir, ssdbfn), sheet="Membership", col_names=FALSE) # superfast;
glimpse(ssdbmem)
ssdbmem[10:14, 1:10]
sasnames <- ssdbmem[12, ] %>% unlist %>% unname
sasnames
sasnames[sasnames=="SAS VAR NAME>"] <- "stadmin"
names(ssdbmem) <- tolower(sasnames)
ht(ssdbmem)
count(ssdbmem, type)
count(ssdbmem, str_sub(id, 4))
# filter(ssdbmem, grepl("52.0", state)) # 52 is national govt (not same as US)
# filter(ssdbmem, grepl("32.0", state)) # NM

ssdbmem2 <- ssdbmem %>%
  mutate(year=as.numeric(year),
         id=str_trim(id),
         admin=round(as.numeric(str_trim(type)))) %>% # type=x will become NA but we only want admin types 1:3
  filter(admin %in% 1:3) %>%
  mutate(adminf=acodes$adminf[match(admin, acodes$admin)],
         stabbr=as.character(stcodes$stabbr[match(str_sub(id, 1, 2), stcodes$stcen)]),
         stabbr=ifelse(str_sub(id, 1, 2)=="52", "NATL", stabbr)) %>%
  filter(!is.na(year))


count(ssdbmem2, admin, adminf)

# now create a long file
ssdbml <- ssdbmem2 %>% select(-c(sortcode, id, state, fips, stadmin, type)) %>%
  gather(variable, value, -c(year, stabbr, admin, adminf)) %>%
  mutate(value=ifelse(grepl("-11111", value), NA, value),
         value=cton(value))
ht(ssdbml)
count(ssdbml, year)
count(ssdbml, admin, adminf)
count(ssdbml, stabbr)
count(ssdbml, variable)

saveRDS(ssdbml, paste0(cendir, "statesummaryhist_mem.rds"))


# combine finance and membership files
df1 <- readRDS(paste0(cendir, "statesummaryhist_fin.rds")) %>% mutate(source="finance")
df2 <- readRDS(paste0(cendir, "statesummaryhist_mem.rds")) %>% mutate(source="membership")
df3 <- bind_rows(df1, df2)
count(df3, variable)
count(df3, source)
saveRDS(df3, paste0(cendir, "statesummaryhist.rds"))



#************************************************************************************************************************
#
#                1993-2003: Get and clean state summary data ####
#
#************************************************************************************************************************
# gettab handles tables 2, 3, and 4, which have similar formats (but tab 4 has different variables over time)
gettab <- function(year, tabnum, valnames) {
  # create a uniform df constructed from the nonuniform input files
  tabnumc <- str_pad(tabnum, 2, side="left", pad="0")
  if(year==2003) fn <- paste0(year, "Ret", tabnumc, ".xls") else fn <- paste0("ret", str_sub(year, 3, 4), "t", tabnum, ".xls")

  # get the basic data frame in shape so that converting to long, etc., will be the same for all tables
  if(tabnum %in% c(2, 3)) { # same format
    vnames <- c("govt.admin", valnames)
    df <- read_excel(paste0(cenxl1, fn), col_names=vnames)
  } else if(tabnum==4) { # Table 4 investment categories have varied over the years JUST GET TOTAL FOR NOW
    df <- read_excel(paste0(cenxl1, fn), col_names = FALSE)
    names(df)[1:2] <- c("govt.admin", valnames)
    df %<>% select_("govt.admin", valnames)
  }
  # get state name and administration, convert to long format
  dfl <- df %>% mutate(govt.admin=str_trim(govt.admin),
                       admin=ifelse(govt.admin %in% c("State", "Local"), govt.admin, "Total"),
                       slag1=lag(govt.admin, 1),
                       slag2=lag(govt.admin, 2),
                       state=ifelse(govt.admin=="State", slag1, ifelse(govt.admin=="Local", slag2, govt.admin)),
                       stabbr=stcodes$stabbr[match(state, stcodes$stname)]) %>%
    filter(stabbr %in% stcodes$stabbr) %>%
    select(-govt.admin, -state, -slag1, -slag2) %>%
    gather(variable, value, -stabbr, -admin) %>%
    mutate(year=year, value=cton(value)) %>%
    select(stabbr, admin, year, variable, value)
  return(dfl)
}

tab2names <- c("totrev", "eec", "erc", "erc.sg", "erc.lg", "invinc")
dft2 <- ldply(1993:2003, gettab, 2, tab2names)

tab3names <- c("totpay", "benefits", "withdraw", "otherpay")
dft3 <- ldply(1993:2003, gettab, 3, tab3names)

tab4names <- c("assets")
dft4 <- ldply(1993:2003, gettab, 4, tab4names)

# qplot(year, value, data=filter(dft4, variable=="assets", admin=="Total", stabbr=="US"), geom=c("point", "line"))

# Tab 5 we must do separately because (a) for the nation it has mutiple admin types and (b) for states it only has state-local
gettab5 <- function(year) {
  if(year==2003) fn <- "2003Ret05.xls" else fn <- paste0("ret", str_sub(year, 3, 4), "t5.xls")
  df <- read_excel(paste0(cenxl1, fn), col_names = c("govt.admin", "nsystems", "totmem", "actives", "inactives", "beneficiaries"))
  usrow <- which(df$govt.admin=="United States")
  df2 <- df %>% filter(row_number() >= usrow) %>%
    mutate(govt.admin=str_trim(govt.admin),
           state=as.character(stcodes$stname[match(govt.admin, stcodes$stname)]),
           admin=ifelse(state %in% stcodes$stname, "Total", govt.admin),
           admin=ifelse(grepl("State", admin), "State", admin),
           admin=ifelse(grepl("Local", admin), "Local", admin),
           state=ifelse(is.na(state), "United States", state),
           stabbr=stcodes$stabbr[match(state, stcodes$stname)])
  dfl <- df2 %>% select(-govt.admin, -state) %>%
    gather(variable, value, -stabbr, -admin) %>%
    mutate(year=year, value=cton(value))
  return(dfl)
}

dft5<- ldply(1993:2003, gettab5)
# clean up admin var
count(dft5, admin)
dft5 %<>% mutate(admin=ifelse(grepl("Municipal", admin), "Municipality", admin),
                admin=ifelse(grepl("Count", admin), "County", admin),
                admin=ifelse(grepl("School", admin), "School district", admin),
                admin=ifelse(grepl("Special", admin), "Special district", admin),
                admin=ifelse(grepl("Town", admin), "Township", admin))
count(dft5, admin)

dfall <- bind_rows(dft2, dft3, dft4, dft5) %>% arrange(stabbr, admin, variable, year)
ht(dfall)
glimpse(dfall)
count(dfall, stabbr) # includes US, DC
count(dfall, admin)
count(dfall, variable)
count(dfall, year)
saveRDS(dfall, paste0(cendir, "statesummary1993to2003.rds"))


#************************************************************************************************************************
#
#                2004-2011:  Get and clean state summary data ####
#
#************************************************************************************************************************
# type codes
# I think these are the same as for the historical DB, but for now create them separately here just in case we need to vary them
# These appear to differ from the historical DB
# 1=US total, 2=State total, 3=Local total, 4=County total, 5=Municipality total, 6=Township total, 7=Special District total, 8=School District total
tcodesv <- "type, typef
1, State-local
2, State
3, Local
4, County
5, Municipal
6, Township
7, Special district
8, School district
x, Other"
tcodes <- read_csv(tcodesv) %>% mutate_each(funs(str_trim))
names(tcodes) <- str_trim(names(tcodes)) # just in case spaces crept into the names

getss <- function(year){
  fn <- paste0(year, "retest_data.xls")
  df <- read_excel(paste0(cenxl2, fn), col_names=FALSE)
  vnrow <- grep("Level", t(df[, 3])) # row in which var names are
  vnames <- df[vnrow, ] %>% unlist %>% unname
  vnames[vnames=="Amount"] <- "value"
  vnames <- gsub("[ _]", "", vnames) # sometimes there is space, sometimes _, so get rid of both
  names(df) <- vnames
  # sometimes there is CV, sometimes not (2007), so don't use it

  df2 <- df %>% mutate(year=year,
                       ItemID=as.numeric(ItemID),
                       type=as.numeric(Level),
                       typef=tcodes$typef[match(type, tcodes$type)],
                       StateID=as.character(round(as.numeric(StateID))),
                       StateID=str_pad(StateID, 2, pad="0"),
                       stabbr=stcodes$stabbr[match(StateID, stcodes$stcen)],
                       value=cton(value)
                       ) %>%
    filter(!is.na(ItemID)) %>%
    select(year, stabbr, ItemID, ItemName, type, typef, value)
  return(df2)
}
# note that we don't have ItemID 32 (# systems) in 2004

ssdf <- ldply(2004:2011, getss) %>% mutate(ItemName=str_to_title(ItemName)) # create more uniform names
count(ssdf, year)
count(ssdf, ItemID, ItemName)


# make further changes to conform itemnames
ssdf2 <- ssdf
ssdf2$ItemName[ssdf2$ItemName=="From State Government"] <- "State Government Contributions"
ssdf2$ItemName[ssdf2$ItemName=="From Local Government"] <- "Local Government Contributions"
ssdf2$ItemName[ssdf2$ItemName=="Earnings On Investment"] <- "Earnings On Investments"
ssdf2$ItemName[ssdf2$ItemName=="Withdrawal"] <- "Withdrawals"
ssdf2$ItemName[ssdf2$ItemName=="Total Cash And Deposits"] <- "Total Cash And Short-Term Investments"
ssdf2$ItemName[ssdf2$ItemName=="Time, Savings Deposits, And Nonfederal Short-Term Investment"] <- "Time, Savings Deposits And Nonfederal Short-Term Investments"
ssdf2$ItemName[ssdf2$ItemName=="Other Securities"] <- "Other Nongovernmental Securities"

saveRDS(ssdf2, paste0(cendir, "statesummary2004to2011.rds"))



#************************************************************************************************************************
#
#                2012+: Get and clean state summary data ####
#
#************************************************************************************************************************
# first get the data, then the metadata
# get the data
# first row is variable description
# variable name is 2 parts: 1) level, and 2) column id
# in 2012, lvl is 0, 1, or 2 for state-local, state, or local
# in 2013, lvl is 0a or 0b for state-local amount and CV, 1 for state, and 2a or 2b for local amount and CV
# column id APPEARS to mean the same thing in each year but I'm going to grab the description from the first row, to be save
# the first-row description has two parts - before semi-colon is the level, and after is the description
# the level description in 2013 identifies amount or CV, whereas in 2012 it does not

getaff <- function(year) {
  fn <- paste0("PP_", year, "_PP00SL_with_ann.csv")
  df <- read_csv(paste0(affdir, fn))
  names(df)[1:3] <- c("geoid", "geoid2", "geoname")

  df2 <- df %>% gather(variable, value, -c(geoid, geoid2, geoname)) %>%
    mutate(stabbr=stcodes$stabbr[match(geoname, stcodes$stname)]) %>%
    filter(stabbr %in% stcodes$stabbr) %>%
    separate(variable, c("lvl", "col"), remove=FALSE) %>%
    mutate(admin=as.character(factor(str_sub(lvl, 4, 4), levels=c("0", "1", "2"), labels=c("Total", "State", "Local"))),
           year=year) %>%
    select(-geoid, -geoid2, -geoname) %>%
    select(stabbr, admin, variable, lvl, col, year, value) %>%
    arrange(stabbr, admin, lvl, col, year)

  df3 <- df2 %>% mutate(type=ifelse(str_sub(lvl, -1) %in% c("0", "1", "2", "a"), "amount", "CV")) %>%
    filter(type=="amount") %>%
    select(-type, -lvl) %>%
    mutate(value=cton(value))
  return(df3)
}

df <- ldply(2012:2013, getaff)
count(df, stabbr)
count(df, admin)
count(df, year)
count(df, variable)

fn <- paste0("PP_", 2013, "_PP00SL_metadata.csv")

getmeta <- function(year){
  fn <- paste0("PP_", year, "_PP00SL_metadata.csv")
  meta <- read_csv(paste0(affdir, fn), col_names=FALSE) %>%
     rename(variable=X1, text=X2) %>%
     separate(variable, c("lvl", "col"), sep="_", remove=FALSE, extra="drop") %>%
     separate(text, c("type", "description"), sep=";", extra="drop") %>%
     mutate(year=year,
            description=str_trim(description))
  return(meta)
}
dfm <- ldply(2012:2013, getmeta)

df %<>% mutate(description=dfm$description[match(variable, dfm$variable)])

tmp <- count(df, year, description) %>% spread(year, n) # good - same in both years!
count(df, col, description) %>% filter(n!=312) # to identify any that have different cols -- none found

saveRDS(select(df, -variable), paste0(cendir, "statesummary2012plus.rds"))


#************************************************************************************************************************
#
#                1957-2013+ - Create unified summary data ####
#
#************************************************************************************************************************
# create a uniform file with variables stabbr, admin, adminf, year, variable, for 50 states, DC, US
df1 <- readRDS(paste0(cendir, "statesummaryhist.rds"))
df2 <- readRDS(paste0(cendir, "statesummary1993to2003.rds"))
df3 <- readRDS(paste0(cendir, "statesummary2004to2011.rds"))
df4 <- readRDS(paste0(cendir, "statesummary2012plus.rds"))

glimpse(df1)
glimpse(df2)
glimpse(df3)
glimpse(df4)
count(df1, variable)
count(df2, variable)
count(df3, ItemID, ItemName)
tmp <- count(df4, col, description)


# variable, variable, ItemID, col -are the "native" variables for df1-df4 respectively
# not all files have total contributions (erc + eec), so calculate that
# don't bother with total revenue (contrib + invinc) - can calc if needed
xwalk.s <- "uvname, file, fvalue
nsystems, df1, systems
nsystems, df2, nsystems
nsystems, df3, 32
nsystems, df4, C036

members, df1, totmemb
members, df2, totmem
members, df3, 28
members, df4, C038

actives, df1, active
actives, df2, actives
actives, df3, 29
actives, df4, C039

inactives, df1, inactive
inactives, df2, inactives
inactives, df3, 30
inactives, df4, C040

beneficiaries, df1, totbenef
beneficiaries, df2, beneficiaries
beneficiaries, df3, 31
beneficiaries, df4, C042

payroll, df4, C044

eec, df1, totempcontrb
eec, df2, eec
eec, df3, 1
eec, df4, C003

erc, df1, totgovcontrb
erc, df2, erc
erc, df3, 3
erc, df4, C004

erc.sg, df1, contrbstate
erc.sg, df2, erc.sg
erc.sg, df3, 4
erc.sg, df4, C005

erc.lg, df1, contrblocal
erc.lg, df2, erc.lg
erc.lg, df3, 5
erc.lg, df4, C006

invinc, df1, totalearns
invinc, df2, invinc
invinc, df3, 6
invinc, df4, C008

totexpend, df1, toterexp
totexpend, df2, totpay
totexpend, df3, 7
totexpend, df4, C010

benefits, df1, benefits
benefits, df2, benefits
benefits, df3, 8
benefits, df4, C011

assets, df1, totercashsec
assets, df2, assets
assets, df3, 11
assets, df4, C015
"
xwalk <- read_csv(xwalk.s) %>% mutate_each(funs(str_trim))
names(xwalk) <- str_trim(names(xwalk)) # just in case spaces crept into the names
xwalk %<>% filter(!is.na(file))
xwalk
df1xw <- select(filter(xwalk, file=="df1"), uvname, fvalue)
df2xw <- select(filter(xwalk, file=="df2"), uvname, fvalue)
df3xw <- select(filter(xwalk, file=="df3"), uvname, fvalue)
df4xw <- select(filter(xwalk, file=="df4"), uvname, fvalue)

glimpse(df1)
count(df1, variable)
count(df1, year)
count(df1, admin)
df1a <- df1 %>% filter(year<1993, admin %in% 1:3) %>%
  mutate(uvname=df1xw$uvname[match(variable, df1xw$fvalue)]) %>% # get the uniform variable name
  filter(!is.na(uvname)) %>%
  mutate(admin=as.character(factor(admin, levels=1:3, labels=c("State-local", "State", "Local")))) %>%
  select(year, stabbr, admin, uvname, value)


glimpse(df2)
count(df2, variable)
count(df2, year)
count(df2, admin)
df2a <- df2 %>% filter(admin %in% c("Total", "State", "Local")) %>%
  mutate(uvname=df2xw$uvname[match(variable, df2xw$fvalue)]) %>% # get the uniform variable name
  filter(!is.na(uvname)) %>%
  mutate(admin=ifelse(admin=="Total", "State-local", admin)) %>%
  select(year, stabbr, admin, uvname, value)


glimpse(df3)
count(df3, year)
count(df3, type)
count(df3, ItemID, ItemName)
df3a <- df3 %>% filter(type %in% 1:3) %>%
  mutate(uvname=df3xw$uvname[match(ItemID, df3xw$fvalue)]) %>% # get the uniform variable name
  filter(!is.na(uvname)) %>%
  mutate(admin=as.character(factor(type, levels=1:3, labels=c("State-local", "State", "Local")))) %>%
  select(year, stabbr, admin, uvname, value)


glimpse(df4)
count(df4, admin)
tmp <- count(df4, col, description)
df4a <- df4 %>%
  mutate(uvname=df4xw$uvname[match(col, df4xw$fvalue)]) %>% # get the uniform variable name
  filter(!is.na(uvname)) %>%
  mutate(admin=ifelse(admin=="Total", "State-local", admin)) %>%
  select(year, stabbr, admin, uvname, value)


# Now put the 4 files together year, admin, stabbr, uvname, value
glimpse(df1a)
glimpse(df2a)
glimpse(df3a)
glimpse(df4a)
count(df1a, year); count(df2a, year); count(df3a, year); count(df4a, year)

dfall <- bind_rows(df1a, df2a, df3a, df4a) %>%
  rename(variable=uvname) %>%
  arrange(stabbr, admin, variable, year)
glimpse(dfall)
count(dfall, variable)
count(dfall, stabbr)
count(dfall, admin)
count(dfall, year)

qplot(year, value, data=filter(dfall, variable=="nsystems", stabbr=="US", admin=="State-local"), geom=c("point", "line"))

cenretss <- dfall
devtools::use_data(cenretss, overwrite=TRUE)





