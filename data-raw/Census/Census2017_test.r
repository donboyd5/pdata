
# classification manual
# https://www.census.gov/topics/public-sector/about/classification-manual.html


#************************************************************************************************************************
#                Loads ####
#************************************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times.
library("lubridate") # lubridate, for date/times.
library("readxl") # readxl, for .xls and .xlsx files.

# load my packages last
# library("apitools")
library("bdata")
library("btools")


#************************************************************************************************************************
#                Globals ####
#************************************************************************************************************************
# D:\Dropbox\RPrograms PC\Packages\Pensions packages\pdata\data-raw\Census\Units-35char
uidd <- "./data-raw/Census/Units-35char/"

udir2017 <- "D:/Data/CensusRetirementData/Units/2017/"
ids2017 <- "aspp2017idsfile.txt"
u2017 <- "2017asppunitsfile.txt"


#************************************************************************************************************************
#                Get data ####
#************************************************************************************************************************

#..id file ----
# Item Field Positions Length
# PID (9 character ID number) 1-9 9
# Filler 10-10 1
# GIDID (14 character ID number)
# State code (states & the District of Columbia in alpha sequence) 11-12 2
# Type of government code: 13-13 1
# 0 = State
# 1 = County
# 2 = City
# 3 = Municipalities and Townships
# 4 = Special Districts
# 5 = Schools
# County Code 14-16 3
# Unit Identification Number 17-19 3
# Supp Code 20-22 3
# Sub Code 23-24 3
# Unit ID Name 26-110 85

ptypes <- read_csv("type, ptype
0, State
1, County
2, City
3, Municipalities and Townships
4, Special Districts
5, Schools")
ptypes

starts <- c(1, 11, 13, 14, 17, 20, 23, 26)
ends <- c(9, 12, 13, 16, 19, 22, 24, 110)
cnames <- c("pid", "stcode", "type", "cntycode", "unitid", "suppcode", "subcode", "uname")
ctypes <- "cciccccc"
ids <- read_fwf(paste0(udir2017, ids2017), fwf_positions(starts, ends, col_names=cnames), col_types = ctypes) %>%
  left_join(ptypes)
ht(ids)


#..units file ----
# Item Field Positions Length
# PID (9 character ID number) 1-9 9
# Line Item code 10-17 8
# Amount (thousands) 18-32 15
# Survey year 33-34 2
# Year of Data 35-36 2
# Source 37-38 2
# Item Dissemination Flag 39-39 1

# Source Source Explanation
# 11 F-11 Locally Administered Public Employee Retirement System
# 12 F-12 State Administered Public Employee Retirement System
# Item Dissemination Flag IDF Explanation
# R Reported
# I Imputed

starts <- c(1, 10, 18, 33, 35, 37, 39)
ends <- c(9, 17, 32, 34, 36, 38, 39)
cnames <- c("pid", "ic", "value", "survyear", "datayear", "source", "reptimpflag")
ctypes <- "ccncccc"
units <- read_fwf(paste0(udir2017, u2017), fwf_positions(starts, ends, col_names=cnames), col_types = ctypes)
ht(units)

count(units, survyear, datayear)
count(units, source)
length(unique(units$pid))
# units %>%
#   group_by(pid) %>%
#   summarise(n=n()) %>%
#   nrow


#************************************************************************************************************************
#                Get item codes ####
#************************************************************************************************************************
# D:\Data\CensusRetirementData\Units\2017\codes.xlsx
fn <- "D:/Data/CensusRetirementData/Units/2017/codes.xlsx"
codes <- read_excel(fn, sheet="codes_export") %>%
  filter(!is.na(ic))
ht(codes)

check <- unique(units %>% select(ic)) %>%
  mutate(src="units") %>%
  full_join(codes) %>%
  mutate(src=ifelse(is.na(src), "codes", src))
check %>% filter(is.na(ic) | is.na(var_description))

icodes <- codes %>%
  filter(category=="Cash and Security Holdings")


#************************************************************************************************************************
#                Get members ####
#************************************************************************************************************************
# Z01 Number of Active Members
# Z02 Number of Inactive Members
# Z03 Number of Beneficiaries Retired, Age or Service
# Z04 Number of Beneficiaries Retired, Disability
# Z05 Number of Beneficiaries, Survivors
mems <- paste0("Z0", 1:5)
memlabs <- c("actives", "inactives", "retnorm", "retdisb", "survivors")
members <- units %>%
  filter(ic %in% mems) %>%
  mutate(memlab=memlabs[match(ic, mems)]) %>%
  select(pid, memlab, value) %>%
  spread(memlab, value) %>%
  mutate(beneficiaries=retnorm + retdisb + survivors,
         members=actives + inactives,
         participants=members + beneficiaries)
members %>% select(-pid) %>% summarise_all(sum)
ht(members)


#************************************************************************************************************************
#                Get assets ####
#************************************************************************************************************************
assets <- units %>%
  filter(ic=="Z81") %>%
  left_join(ids %>% select(pid, stcode, uname, ptype)) %>%
  mutate(stabbr=stcodes$stabbr[match(stcode, stcodes$stcen)],
         stname=getstname(stabbr)) %>%
  select(pid, ptype, uname, stabbr, stname, assets=value) %>%
  mutate(assets=assets / 1e6)


#************************************************************************************************************************
#                Small plans by state ####
#************************************************************************************************************************
glimpse(units)
glimpse(assets)
glimpse(ids)
glimpse(members)

# assets %>%
#   left_join(members) %>%
#   # filter(assets >= 1, participants > 1) %>%
#   filter(assets > 0.5, participants > 1) %>%
#   nrow
#
# assets %>%
#   left_join(members) %>%
#   filter(assets < 0.5, participants > 1) %>%
#   arrange(assets, participants) %>%
#   select(uname, stname, assets, ptype, actives, inactives, beneficiaries, participants)


probs <- c(0, .05, .1, .25, .5, .75, .9, .95, 1)

all <- assets %>%
  select(pid, assets, stabbr, stname) %>%
  left_join(ids %>% select(pid, ptype, uname)) %>%
  left_join(members) %>%
  filter(participants >= 1)
quantile(all$assets, probs) %>% round(2)
quantile(all$participants, probs) %>% round(1)

all %>%
  mutate(groups=cut(participants, c(0, 1, 2, 3, 4, 5, 10, 20, 50, 100, 1000, 10e3, 100e3, Inf))) %>%
  group_by(groups) %>%
  summarise(n=n(), parts=sum(participants), assets=sum(assets)) %>%
  mutate_at(vars(n, parts, assets), funs(cum=cumsum(.))) %>%
  mutate_at(vars(n, parts, assets), funs(cumpct=cumsum(.) / sum(.) * 100)) %>%
  mutate_at(vars(n, parts, assets), funs(pct=. / sum(.) * 100))

# all %>%
#   filter(participants <= pany(all$participants, .9)) %>%
#   ggplot(aes(participants)) +
#   geom_histogram(binwidth=10, fill="blue") # binwidth=10,

alltot <- all %>%
  summarise(n=n(), assets=sum(assets),
            parts=sum(participants))
alltot

small <- all %>%
  # filter(participants <= pany(all$participants, .95))
  filter(participants <= 1e4)
quantile(small$assets, probs) %>% round(2)
quantile(small$participants, probs) %>% round(1)

smalltot <- small %>%
  summarise(n=n(), assets=sum(assets),
            parts=sum(participants))
smalltot


bigtot <- all %>%
  filter(!pid %in% small$pid) %>%
  summarise(n=n(), assets=sum(assets),
            parts=sum(participants))
bigtot

bigtot$n / alltot$n
bigtot$assets / alltot$assets
bigtot$parts / alltot$parts


glimpse(small)
count(small, ptype)
count(small, stabbr)

small %>%
  group_by(stabbr, stname) %>%
  summarise(n=n(), assets=sum(assets),
            parts=sum(participants)) %>%
  arrange(-n) #

all %>%
  group_by(stabbr, stname) %>%
  summarise(n=n(), assets=sum(assets),
            parts=sum(participants)) %>%
  arrange(-n) #

all %>%
  group_by(stabbr, stname) %>%
  summarise(n=n(), assets=sum(assets),
            parts=sum(participants)) %>%
  ungroup %>%
  summarise(n.sum=n(), n.mdn=median(n))

quantile(assets$assets, 0:10/10)
ht(assets)




#************************************************************************************************************************
#                OLD: Analyze small plans ####
#************************************************************************************************************************
ht(ids)
ht(units)

# X21, X30, Z77, Z78, X42, X44, X47 cash plus securities
cashsec <- c("X21", "X30", "Z77", "Z78", "X42", "X44", "X47")
# X46	Real Property
# X47	Other Investments
otherinv <- c("X46", "X47")
invest <- c(cashsec, otherinv)


units %>%
  group_by(ic) %>%
  summarise(value=sum(value)) %>%
  arrange(-value)

units %>%
  right_join(icodes %>% select(ic, var_description)) %>%
  group_by(ic, var_description) %>%
  summarise(value=sum(value)) %>%
  arrange(-value)

tmp <- units %>%
  filter(ic %in% invest, value!=0)
sum(tmp$value)

tmp %>%
  group_by(ic) %>%
  summarise(value=sum(value) / 1e9) %>%
  left_join(codes %>% select(ic, var_description)) %>%
  arrange(-value)

tmp %>%
  group_by(ic, reptimpflag) %>%
  summarise(value=sum(value) / 1e9) %>%
  left_join(codes %>% select(ic, var_description)) %>%
  spread(reptimpflag, value)

tmp %>%
  group_by(ic, reptimpflag) %>%
  summarise(value=sum(value) / 1e9) %>%
  arrange(ic, reptimpflag)


units %>%
  filter(ic %in% invest, value!=0) %>%
  summarise(assets=sum(value) / 1e6)

assets <- units %>%
  filter(ic=="Z81") %>%
  left_join(ids %>% select(pid, stcode, uname, ptype)) %>%
  select(pid, ptype, uname, assets=value) %>%
  mutate(assets=assets / 1e6)

assets_cum <- assets %>%
  left_join(members %>% select(pid, members, beneficiaries, participants)) %>%
  mutate(assets.all=sum(assets),
         n.all=n()) %>%
  filter(assets >= 1, participants > 0) %>% # $1 million
  mutate(n.sum=n(),
         assets.sum=sum(assets),
         assetspct=assets / assets.sum * 100,
         members.sum=sum(members),
         ben.sum=sum(beneficiaries),
         part.sum=sum(participants)) %>%
  # get cum sums going up
  arrange(assets) %>%
  mutate(n.up=row_number(),
         n.uppct=n.up / n.sum * 100,
         assets.uppct=cumsum(assets) / assets.sum * 100,
         members.uppct=cumsum(members) / members.sum *100,
         ben.uppct=cumsum(beneficiaries) / ben.sum * 100,
         part.uppct=cumsum(participants) / part.sum * 100,
         members.up=cumsum(members),
         ben.up=cumsum(beneficiaries),
         part.up=cumsum(participants)) %>%
  arrange(-assets) %>%
  mutate(n.down=row_number(),
         n.downpct=n.down / n.sum * 100,
         assets.downpct=cumsum(assets) / assets.sum * 100,
         members.downpct=cumsum(members) / members.sum *100,
         ben.downpct=cumsum(beneficiaries) / ben.sum * 100,
         part.downpct=cumsum(participants) / part.sum * 100,
         members.down=cumsum(members),
         ben.down=cumsum(beneficiaries),
         part.down=cumsum(participants))
glimpse(assets_cum)

ht(assets_cum)
head(assets_cum, 200)

vars <- c("pid", "ptype", "uname", "assets", "members", "beneficiaries")
assets_cum %>%
  filter(n.down %in% c(100, 200, 250, 300)) %>%
  # filter(assets >= .9e3, assets <= 1.1e3) %>%
  select(vars, starts_with("n.up"), starts_with("n.down"),
         starts_with("assets.up"), starts_with("assets.down"),
         members.up, ben.up, part.up)

assets_cum %>%
  filter(n.down %in% c(100, 200, 250, 300)) %>%
  glimpse



local <- units %>%
  left_join(ids) %>%
  filter(type!=0)
ht(local)

icodes <- codes %>%
  filter(category=="Cash and Security Holdings")

assets <- local %>%
  filter(ic=="Z81") %>%
  select(pid, ptype, uname, assets=value) %>%
  mutate(assets=assets / 1e6)
quantile(assets$assets, 0:10/10)
ht(assets)

assets %>%
  filter(assets < 1e3) %>%
  filter(assets>=1) %>%
  ggplot(aes(assets)) +
  geom_histogram(binwidth=10, fill="blue") #+ # geom_histogram(binwidth=.001, fill="blue") +
# geom_vline(xintercept = 1) +
# scale_x_continuous(breaks=seq(0, 2, .02))

assets %>%
  filter(assets < 1e3) %>%
  filter(assets>=1) %>%
  summarise(n=n(), assets=sum(assets))

assets %>%
  filter(assets < 2e3) %>%
  arrange(-assets)



