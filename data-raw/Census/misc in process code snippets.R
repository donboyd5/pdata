
u35dir <- paste0(cendir, "Units-35char/")
u35web <- paste0(u35dir, "web-interface/")  # files obtained from the Census web interface
u35nonweb <- paste0(u35dir, "nonweb/")  # files obtained from the Census Bureau but NOT from the web interface


ssdbfn <- "1_Emp_Retire_Sys_Nat_State_Data.xls" # state summary data base file name


# TODO: Rewrite this info below
# 2b) 1993(?)-2001
#    1993-2013 unit-level data may be obtained, year-by-year, through drop-down menu at:
#      http://www.census.gov/govs/retire/historical_data.html
#    The actual files are located at:
#      http://www2.census.gov/govs/retire/
#    There also is an ftp site that appears to have identical info, but I am using the www2 site.
#    Because of imputations, discussed below, I believe we can only use these data for 1993-2001;
#    1993 still has issues to resolve

#    I have downloaded ALL of the unit files and all of the associated "id" files (names of systems) that appear through
#    the web interface, as this is what Census make available officially.
#    The downloads section below shows how to download all of the files through R.

#    CENSUS BUREAU IMPUTATIONS ####
#    From 2002 through 2011 the unit files are "no imputes" files.
#    There is a good discussion of this in the 2003 methodology document: http://www2.census.gov/govs/retire/2003surveymeth.pdf
#    It says that "Between years 2002 through 2007, individual government imputed data were not released to the public."

#    From 2003-2005 unit files have "noimputes" appended to the name, as in:  2003ret35noimputes.txt
#    The file for 2006 available through the web interface is called: 2006ret35imputes.txt
#      I need to determine if the 2006 file has imputations or not.
#    Beginning in 2007 filenames are labeled as "reported data" as in: 2007indiv_unit_reported_data.txt
#    Ideally I would like to use files WITH imputations.

#    Files NOT available through the web interface:
#    The website http://www2.census.gov/govs/retire/ has all of the files available through the web interface, PLUS
#    some files that appear to include imputations. For example, 2003Ret35.txt is much larger than 2003ret35noimputes.txt,
#    suggesting it includes imputations. (Note, also, that there is a file 2003ret35.txt - lower-case "r" - that is the)
#    same size as 2003Ret35.txt but has an earlier date.

# TENTATIVE CONCLUSION: We can use unit files for 1993-2001, and again from 2012+ (but not sure if we will)
# we need to use state summaries:

# 3) 2002-2003
#    Again use the state summary data in Tables 2-5 at the Census download site discussed in 2a http://www.census.gov/govs/retire/

# 4) 2004-2011 the data are already summarized at the state level in files such as:
#    2011retest_data.xls in the directory
#    E:\Data\CensusRetirementData\SummaryTables\SummaryExcelFiles



# 3) 2012+ I must summarize from the unit data
#    retsysunits.rds in the directory
#    cenretd "E:\\Data\\CensusRetirementData\\"

# Unit data
# http://www2.census.gov/govs/retire/93ret35.txt


# get files from the web THIS CODE IS OLD AND IS NOT VERIFIED YET!!!!
getfn<-function(year){
  if(year>=2000) y2<-year-2000 else y2<-year-1900
  y2<-sprintf("%02d",y2)
  if(year<=2002) fn<-paste(y2,"ret35.txt",sep="") else
    if(year<=2005) fn<-paste(year,"ret35noimputes.txt",sep="") else
      if(year==2006) fn<-paste(year,"ret35imputes.txt",sep="") else
        fn<-paste(year,"indiv_unit_reported_data.txt",sep="")
      return(fn)
}

for(year in 1993:2009){
  fn<-getfn(year)
  # print(fn)
  fget<-paste(web,fn,sep="")
  fput<-paste(cendir,fn,sep="")
  download.file(fget,fput,mode="wb")
}





#************************************************************************************************************************
#
#                Create variable mappings ####
#
#************************************************************************************************************************
# get the recipe file
fn <- "CensusRetirementDataAnalysis(4).xlsx"
df <- read_excel(paste0(cenretd, fn), sheet="Recipe", col_names=FALSE)
# find last data column
lastdcol <- grep("beneficiaries", df[6, ])
df <- df[, 1:lastdcol]

# store the vname, SAS var, itemname, and itemnumber mappings, then move on to create the recipes
mappings <- as.data.frame(t(df[3:6, ]))
names(mappings) <- t(mappings[1, ])
mappings <- mappings[-1, ]
saveRDS(mappings, paste0(cenretd, "vnamemappings.rds"))

# create recipes
# icdf <- readRDS(paste0(cenretd, "retitemcodes.rds"))
# retdf <- readRDS(paste0(cenretd, "retsysunits.rds"))
df2 <- df
names(df2) <- df2[6, ]
datastart <- grep("codes", t(df2[, 1]))
recipe <- df2 %>% filter(row_number()>=datastart) %>%
  select(-vname) %>%
  gather(variable, ic) %>%
  mutate(ic=trim(ic)) %>%
  filter(!is.na(ic))
saveRDS(recipe, paste0(cenretd, "recipe.rds"))


#************************************************************************************************************************
#
#                Summarise variables using recipes ####
#
#************************************************************************************************************************
ht(retdf)
count(retdf, stabbr)
# first, put Us on the file
retus <- retdf %>% group_by(year, ic, icf, type, typef) %>% summarise(value=sum(value, na.rm=TRUE), stabbr="US", id="US Total", pubname="US Total")
ht(retus)
ret2 <- bind_rows(select(retdf, year, stabbr, ic, icf, type, typef, pubname, value),
                  retus)
ret3 <- recipe %>% left_join(select(ret2, year, id, stabbr, type, typef, pubname, ic, value))

# system-level summaries of variables
ret4 <- ret3 %>% group_by(year, id, stabbr, type, typef, pubname, variable) %>%
  summarise(value=sum(value, na.rm=TRUE))
saveRDS(ret3, paste0(cenretd, "sysyearvar.rds"))

#************************************************************************************************************************
#
#                Create a time series of state-level summarized data ####
#
#************************************************************************************************************************
# sshistdb <- readRDS(paste0(cenretd, "statesummaryhist.rds")) # 1957:2007 selected years, state-level summary data, from historical database
# ssxl <- readRDS(paste0(cenretd, "statesummaryxl.rds")) # 2004-2011 state-level summary data from Excel state summary files
# retdf <- readRDS(paste0(cenretd, "retsysunits.rds")) # 2007-2013 system-level item code data, from most recent item-code files
# sysyearvar <- readRDS(paste0(cenretd, "sysyearvar.rds")) # 2007-2013 state-level summary data constructed from retsysunits.rds
# vmaps <- readRDS(paste0(cenretd, "vnamemappings.rds")) # mappings needed to create a time series from these files
# spop <- readRDS(file=paste0(rdat, "popst.rds")) # state population

sshistdb # year, type, typef, stabbr, variable (lower case sasvar), value
# count(sshistdb, variable)
ssxl # year, type, typef, stabbr, ItemID, ItemName
sysyearvar # year, id, type, typef, pubname, variable, value
vmaps # itemnumber, itemname, sasvarname, vname

# create conformed versions of the 3 files
# count(sshistdb, year, variable)
# count(sshistdb, variable)
df1 <- left_join(filter(mutate(sshistdb, type=as.numeric(type)), type %in% 1:3),
                 mutate(select(vmaps, variable=sasvarname, vname), variable=tolower(variable))) %>%
  select(-variable) %>%
  filter(!is.na(vname), !is.na(value)) %>%
  mutate(source="sshistdb")
ht(df1)
count(df1, year)
count(df1, year, vname)
count(df1, type, typef)

df2 <- left_join(select(filter(ssxl, type %in% 1:3), year, stabbr, ItemID, type, typef, value),
                 mutate(select(vmaps, ItemID=itemnumber, vname), ItemID=as.numeric(ItemID))) %>%
  select(-ItemID) %>%
  filter(!is.na(vname)) %>%
  mutate(source="ssxl")
count(df2, type, typef)

df3a <- sysyearvar %>% ungroup %>%
  mutate(type=ifelse(type==0, 2, 3)) %>%
  group_by(stabbr, year, type, vname=variable) %>%
  summarise(value=sum(value, na.rm=TRUE))
# add state-local
df3sl <- df3a %>% group_by(stabbr, year, vname) %>% summarise(value=sum(value, na.rm=TRUE)) %>% mutate(type=1)
df3 <- bind_rows(df3a, df3sl) %>%
  filter(!is.na(stabbr)) %>%
  mutate(typef=factor(type, levels=1:3, labels=c("State-local total", "State", "Local")),
         source="sysyearvar")
count(df3, type, typef)

stack <- bind_rows(df1, df2, df3)

str(df1)
str(df2)
str(df3)

ht(stack)
count(stack, year, source) %>% spread(source, n)

stack2 <- stack %>% filter((source=="sshistdb" & year<=2002) |
                             (source=="ssxl" & year %in% 2004:2006) |
                             (source=="sysyearvar" & year>=2007))
count(stack2, year, source) %>% spread(source, n)
ht(stack2)
count(stack2, year, type) %>% spread(type, n)
count(stack2, year, var=(stabbr=="US")) %>% spread(var, n) # no US for 2007+
count(filter(stack2, year>=2007), year, stabbr) %>% spread(year, n) # no US for 2007+
count(stack2, year, vname) %>% spread(vname, n)
glimpse(stack2)

saveRDS(stack2, paste0(cenretd, "state_type_year_var.rds"))

# now clean up the data and save
styv <- readRDS(paste0(cenretd, "state_type_year_var.rds"))
stackw <- styv %>% spread(vname, value)
count(stackw, year, is.na(erc))
count(stackw, year, is.na(contrib))
filter(stackw, is.na(eec))


# calc contrib from eec, erc; explore missing erc in 1982;
stackw2 <- stackw %>% mutate(contrib=ifelse(is.na(contrib), eec+erc, contrib),
                             xcf=contrib-expend,
                             contrib.pct=contrib/assets*100,
                             expend.pct=expend/assets*100,
                             xcf.pct=xcf/assets*100)
count(stackw2, year, is.na(contrib))
filter(stackw2, is.na(contrib)) %>% glimpse # WY 1982 all gov contrib missing - is so in xls file
stackw2
glimpse(stackw2 %>% tail)
dfl <- stackw2 %>% gather(variable, value, -c(year, type, typef, stabbr, source))
saveRDS(dfl, paste0(cenretd, "styvl.rds"))






#************************************************************************************************************************
#
#                Read 35-character files ####
#
#************************************************************************************************************************

read35 <- function(fn) {

}

fn <- paste0(u35web, "93ret35.txt")

wids <- c(2, 1, 3, 3, 3, 2, 3, 12, 2, 2, 2)
cn <- c("stcode", "admin", "cocode", "id", "suppcode", "subcode", "ic", "value", "syear", "dyear", "source")
# wids <- c(14, 3, 12, 2, 2, 2)
# cn <- c("id", "ic", "value", "syear", "dyear", "source")
unitdf <- read_fwf(fn, fwf_widths(wids, cn)) # , col_types="ccdiic"


# compare imputes and no imputes
# 2003ret35noimputes.txt 2003Ret35.txt  YES, this looks right
# 2008indiv_unit_reported_data.txt 2008unit file.txt  unit file has more recs but smaller $!
# 2009indiv_unit_reported_data.txt  2009unit file.txt  same here - suggests latter is local govt admin systems
df1 <- read_fwf(paste0(u35web, "2009indiv_unit_reported_data.txt"), fwf_widths(wids, cn)) # , col_types="ccdiic"
df2 <- read_fwf(paste0(u35nonweb, "2009unit file.txt"), fwf_widths(wids, cn)) # , col_types="ccdiic"
df3 <- bind_rows(mutate(df1, src="noimp"), mutate(df2, src="imp"))

df3 %>% group_by(ic, src) %>%
  summarise(n=n(), value=sum(value, na.rm=TRUE)) %>%
  select(-n) %>%
  spread(src, value) %>%
  mutate(pct=noimp/imp*100)


# look at assets:
df <- read_fwf(paste0(u35web, "95ret35.txt"), fwf_widths(wids, cn), col_types="ciiciicdiic")
# count(df, ic) %>% data.frame
df %>% filter(ic=="Z81") %>% # assets Z81 benefits X11
  summarise(n=n(), value=sum(value, na.rm=TRUE) / 1e6)


# Columns:
#    units add to universe? YES/NO
#    year
#    Total assets:
#       unit file name and (total assets from units, sum of Z81, $ billions)
#       summary file name and (total assets reported, $ billions, AFF=American Fact Finder)
#         if name ends in .html, it is url portion after http://www.census.gov/govs/retire/

# ??? 2013 2013indiv_unit_reported_data.txt(3278.489)   AFF(3287.365)
# YES 2012 2012indiv_unit_reported_data.txt(3024.571)   AFF(3024.603)
# NO  2011 2011indiv_unit_reported_data.txt(3011.601)   2011retest_data.xls(3050.401)
# NO  2010 2010indiv_unit_reported_data.txt(2646.949)   2010retest_data.xls(2671.233)
# NO  2009 2009indiv_unit_reported_data.txt(2372.848)   2009retest_data.xls(2415.671)
# NO  2008 2008indiv_unit_reported_data.txt(3076.251)   2008retest_data.xls(3130.390)
# NO  2007 2007indiv_unit_reported_data.txt(3293.399)   2007retest_data.xls(3305.379)
# NO  2006 2006ret35imputes.txt(2855.109)               2006retest_data.xls(2912.494)
# NO  2004 2004ret35noimputes.txt(2474.235)             2004retest_data.xls(2495.352)
# NO  2003 2003ret35noimputes.txt(2135.883)             ret03t4.xls(2172.002)
# NO  2002 02ret35.txt(2118.191)                        ret02t4.xls(2157.210)
# YES 2001 01ret35.txt(2157.629)                        ret01t4.html(2157.629)
# YES 2000 00ret35.txt(2168.643)                        ret00t4.html(2168.643)
# ...
# YES 1995 95ret35.txt(1118.353)                        ret95t4.html(1118.353)
# YES 1994 94ret35.txt(1006.411)                        ret94t4.html(1006.411)
# ??? 1993 93ret35.txt(920.572)                         ret93t4.html(909.850)



# 1993 93ret3.txt X11 benefits 48.32786




# E:\Dropbox (Personal)\RPrograms PC\Packages\pdata\data-raw\Census\Units-35char\web-interface
E:/Dropbox (Personal)/RPrograms PC/Packages/pdata/data-raw/Census/Units-35char/web-interface/93ret35.txt

list.files(u35web)

starts <- c(1, 3, 5, 9)
ends <- c(2, 3, 8, 20)

df <- read_fwf(paste0(d35, fn), fwf_positions(starts, ends, col_names=c("stcode", "level", "ic", "value")),
               col_types="cicd")



# 35-char format; NOTE that type code is DIFFERENT from historical database
# Unit ID (14 character ID number)
# State code (states & DC in alpha sequence) 1-2 2
# Type of government code: 3 1
# 0 = State
# 1 = County
# 2 = City
# 3 = Municipalities and Townships
# 4 = Special Districts
# 5 = Schools
# County Code 4-6 3
# Unit Identification Number  7-9 3
# Supp Code 10-12 3
# Sub Code 	13-14 2
# Line Item code 	15-17 	3
# Amount (thousands) 	18-29 	12
# Survey year 	30-31 	2
# Year of Data 	32-33 	2
# Source 	34-35 	2




read_fwf(fn, widths=c(14,3,12,-2,2,2), col.names=c("govid","ic","value","datayear","source"), colClasses="character", n=-1) # skip survyear




for(year in 1993:2009){
  fn<-getfn(year)
  df<-read.fwf(paste(cendir,fn,sep=""), widths=c(14,3,12,-2,2,2), col.names=c("govid","ic","value","datayear","source"), colClasses="character", n=-1) # skip survyear
  df$year<-year
  df$datayear<-ifelse(df$datayear<"70",as.numeric(df$datayear)+2000,as.numeric(df$datayear)+1900)
  df$value<-ctov(df$value)
  stateid<-substr(df$govid,1,2)
  df$stabbr<-as.character(factor(stateid,levels=stcen,labels=stabbr))
  df$type<-as.numeric(substr(df$govid,3,3))
  rm(stateid)
  # print(head(df)); print(tail(df))
  print(paste("Processing: ",year,sep=""))
  dfname<-paste("retsys",year,sep="")
  assign(dfname,df)
  save(list=dfname,file=paste(cendir,dfname,".RData",sep="")) # must use list= to use data frame name as character
}



#************************************************************************************************************************
#
#                Summarise, play, etc ####
#
#************************************************************************************************************************
dfl <- readRDS(paste0(cenretd, "styvl.rds")) # state tax year variable long
glimpse(dfl)
glimpse(spop)
gdppi
dfl2 <- dfl %>% mutate(pop=spop$pop[match(paste(year, stabbr), paste(spop$year, spop$stabbr))],
                       valpc=value/pop,
                       rvalue=value*gdppi$value[gdppi$year==2014]/gdppi$value[match(year, gdppi$year)],
                       rvalpc=rvalue/pop)
glimpse(tail(dfl2, 30))

vars <- c("contrib", "expend", "xcf")
st <- "NJ"
dfl2 %>% filter(stabbr==st, type==1, variable %in% vars) %>%
  filter(!year %in% 2004:2006) %>%
  qplot(year, rvalpc, data=., colour=variable, geom=c("point", "line"), main=paste0(st, ": pension cash flow")) + geom_hline(y=0)


count(dfl, type, typef)
vars <- c("contrib.pct", "expend.pct", "xcf.pct")
vars <- c("contrib", "expend", "xcf")
dfl %>% filter(stabbr=="US", type==1, variable %in% vars) %>%
  filter(!year %in% 2004:2006) %>%
  qplot(year, value/1e6, data=., colour=variable, geom=c("point", "line")) + geom_hline(y=0)


vars <- c("contrib.pct", "expend.pct", "xcf.pct")
vars <- c("contrib", "expend", "xcf")
dfl2 %>% filter(year==2013, type==1, variable %in% vars) %>%
  select(stabbr, year, type, variable, rvalpc) %>%
  spread(variable, rvalpc) %>%
  arrange(desc(xcf))




df <- readRDS(paste0(cenretd, "sysyearvar.rds"))


df %>% group_by(year, variable) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  filter(variable=="assets")

df %>% group_by(year, variable) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  spread(variable, value)


df %>% group_by(year, variable) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  spread(variable, value)


# get state per capita summaries
dfs <- df %>% group_by(year, variable, stabbr) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  left_join(select(spop, year, stabbr, pop)) %>%
  mutate(valpc=value/pop)
dfs

count(dfs, variable)
var <- "eec"  # benefits erc eec
dfs %>% filter(year==2012, variable==var) %>%
  arrange(desc(valpc))

dfs %>% filter(year==2012, variable %in% c("eec", "erc")) %>%
  select(stabbr, variable, value) %>%
  spread(variable, value) %>%
  mutate(eecpct=eec/(eec+erc)*100) %>%
  arrange(desc(eecpct))



dfs %>% filter(stabbr=="NY", variable=="benefits") %>%
  arrange(desc(valpc))


