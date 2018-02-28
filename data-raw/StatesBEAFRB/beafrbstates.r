# 2/28/2018


#****************************************************************************************************
#                Libraries ####
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
library("bdata")


# Note - BEA used the following discount rates for the years of this data:
# 2000-2003 6.0%
# 2004-2009 5.5%
# 2010-2014 5.0%


#****************************************************************************************************
#                Get FRB data  - last year 2014 as of 10/9/2017 ####
#****************************************************************************************************
# https://www.federalreserve.gov/apps/fof/efa/efa-project-state-local-government-defined-benefit-pension-plans.htm
# Update (2/28/2018), now in the dataviz area:
# https://www.federalreserve.gov/releases/z1/dataviz/pension/index.html
# The FRB dollar data are in $ millions

# https://www.federalreserve.gov/releases/z1/dataviz/download/zips/efa-state-pension-tables-annual-historical.zip

# frb.states.url <- "https://www.federalreserve.gov/apps/fof/efa/files/efa-state-pension-tables-annual-historical.csv"
# frb.states.put <- "./data-raw/StatesBEAFRB/efa-state-pension-tables-annual-historical.csv"

frb.states.url <- "https://www.federalreserve.gov/releases/z1/dataviz/download/zips/efa-state-pension-tables-annual-historical.zip"
frb.states.put <- "./data-raw/StatesBEAFRB/efa-state-pension-tables-annual-historical.zip"
download.file(frb.states.url, frb.states.put, mode="wb")

unzip(frb.states.put, exdir="./data-raw/StatesBEAFRB", overwrite = TRUE)
csv.name <- "./data-raw/StatesBEAFRB/efa-state-pension-tables-annual-historical.csv"

df.frb <- read_csv(csv.name)
glimpse(df.frb)
vnames <- c("year", "stname", "assets", "liabilities", "ufl", "fr", "ufl.gdp", "ufl.staterev")
vdesc <- names(df.frb)

statepen.frb <- df.frb %>%
  setNames(vnames) %>%
  mutate(stabbr=stcodes$stabbr[match(stname, stcodes$stname)]) %>%
  gather(vname, value, -stabbr, -stname, -year) %>%
  mutate(vdesc=vdesc[match(vname, vnames)],
         value=as.numeric(value)) %>%
  select(stabbr, vname, vdesc, year, value)
glimpse(statepen.frb)
count(statepen.frb, stabbr)
count(statepen.frb, year)
count(statepen.frb, vname, vdesc)

statepen.frb %>% filter(is.na(value)) # ufl.staterev is missing in 2003, except for U.S.

cmt <- "FRB Enhanced Accounts State Pension Tables, $ Millions, FRB updated 2018-01-11. See package documentation for discount rates."
comment(statepen.frb) <- cmt
devtools::use_data(statepen.frb, overwrite=TRUE)

statepen.frb %>% filter(stabbr %in% c("US", "PA"), vname=="fr") %>%
  ggplot(aes(year, value, colour=stabbr)) + geom_line() + geom_point() + geom_hline(yintercept = 100)

statepen.frb %>% filter(stabbr %in% c("US", "PA"), vname=="ufl.staterev") %>%
  ggplot(aes(year, value, colour=stabbr)) + geom_line() + geom_point() + geom_hline(yintercept = 0) + geom_point()

statepen.frb %>% filter(stabbr %in% c("US", "PA", "MN"), vname=="ufl.gdp") %>%
  ggplot(aes(year, value, colour=stabbr)) + geom_line() + geom_hline(yintercept = 0) + geom_point()


#****************************************************************************************************
#                Get BEA data - last year 2015 as of 1/26/2018 ####
#****************************************************************************************************
# BEA data is in $ Thousands but I convert to $ Millions
# Landing page (look to the right for pensions): https://www.bea.gov/regional/

bea.states.url <- "http://www.bea.gov/regional/xls/PensionEstimatesByState.xlsx"
bea.states.put <- "./data-raw/StatesBEAFRB/PensionEstimatesByState.xlsx"
download.file(bea.states.url, bea.states.put, mode="wb")

df.bea.liabs <- read_excel(bea.states.put, sheet="BenefitEntitlements", skip=3)
glimpse(df.bea.liabs)
names(df.bea.liabs)[1:2] <- c("fips", "stname")

df.bea.ncer <- read_excel(bea.states.put, sheet="EmployerNormalCost", skip=3)
glimpse(df.bea.ncer)
names(df.bea.ncer)[1:2] <- c("fips", "stname")

statepen.bea <- df.bea.liabs %>%
  select(-fips) %>%
  mutate(vname="liabilities") %>%
  gather(year, value, -stname, -vname) %>%
  bind_rows(df.bea.ncer %>%
              select(-fips) %>%
              mutate(vname="nc.er") %>%
              gather(year, value, -stname, -vname))%>%
  filter(!is.na(value)) %>%
  mutate(stabbr=stcodes$stabbr[match(stname, stcodes$stname)],
         value=value / 1000,
         year=as.integer(year)) %>%
  select(stabbr, vname, year, value)
glimpse(statepen.bea)
count(statepen.bea, vname)
count(statepen.bea, year)
count(statepen.bea, stabbr) # includes US and DC

cmt <- "BEA State Pension Estimates Updated by BEA 2018-01-26. See package documentation for discount rates."
comment(statepen.bea) <- cmt
devtools::use_data(statepen.bea, overwrite=TRUE)


statepen.bea %>% filter(vname=="nc.er", stabbr=="CA") %>%
  ggplot(aes(year, value)) + geom_line() + geom_point()

statepen.bea %>% filter(vname=="nc.er", stabbr=="MN") %>%
  ggplot(aes(year, value)) + geom_line() + geom_point()

statepen.bea %>% filter(vname=="nc.er", stabbr %in% c("CA", "IL", "MN", "NY", "US")) %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  mutate(ivalue=value / value[year==2005] * 100) %>%
  ggplot(aes(year, ivalue, colour=stabbr)) + geom_line() + geom_point() + geom_hline(yintercept = 100)


#****************************************************************************************************
#                Compare liabilities, both sources ####
#****************************************************************************************************
# Good - the liabilities match (extremely closely)
count(statepen.frb, vname)
count(statepen.bea, vname)

df1 <- statepen.frb %>% filter(vname=="liabilities") %>%
  select(vname, stabbr, year, liab=value) %>%
  mutate(file="frb")
df2 <- statepen.bea %>% filter(vname=="liabilities") %>%
  select(vname, stabbr, year, liab=value) %>%
  mutate(file="bea")
df3 <- bind_rows(df1, df2)
df3 %>% filter(stabbr=="NY") %>%
  ggplot(aes(year, liab, colour=file)) + geom_line() + geom_point()
