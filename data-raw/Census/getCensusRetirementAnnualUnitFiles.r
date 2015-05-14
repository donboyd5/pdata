# getAnnualUnitFiles.r
# Don Boyd
# 3/22/2015

# get all of the annual data, WITH IMPUTATIONS
# not sure it's worth going back before 2007, given extensive changes in Census-defined universe and in measurement of vars

#************************************************************************************************************************
#
#                Get names for each unit id ####
#
#************************************************************************************************************************
# 2007-2011 look pretty clean; 2012 and 2013 use the same file, and that file has some problems ####
ids1 <- paste0(2007:2011, "retid.txt")
readid <- function(year, fn) {
  df <- read_fwf(paste0(uidd, fn), fwf_positions(c(1, 16, 101), c(14, 100, 200), col_names=c("id", "idname", "pubname")), col_types="ccc")
  return(df)
}


iddf1 <- data.frame(year=2007:2011, fn=ids1) %>%
  group_by(year) %>%
  do(readid(.$year, .$fn))

ht(iddf1)

# the 2012-2013 file is very messy so read it in as follows and then parse
chunk <- scan(file=paste0(uidd, "unit_id_file.txt"), what="", sep="\n", nlines=-1)
iddf2 <- data_frame(id=str_sub(chunk, 1, 14), idname=str_sub(chunk, 16, 100), pubname=str_sub(chunk, 101, nchar(chunk)))

# combine the files, trim, and update pubname
iddf <- bind_rows(iddf1, mutate(iddf2, year=2012), mutate(iddf2, year=2013)) %>% 
  mutate_each(funs(trim)) %>%
  mutate(pubname2=ifelse(pubname=="", idname, pubname))

count(iddf, year)
ht(iddf)
count(iddf, id, pubname2, year)
pubnameyear <- iddf %>% select(-pubname, -idname) %>% spread(year, pubname2)


#************************************************************************************************************************
#
#                Get item code descriptions ####
#
#************************************************************************************************************************
# TODO: Z13:Z16 are in the data, but not in the code descriptions; find descriptions
icdf1 <- read_excel(paste0(udocd, "2012_support.xls"), sheet="Pension Codes", col_names = FALSE) %>%
  select(ic=X1, icdesc=X2, f11=X3, f12=X4) %>%
  filter(!is.na(ic), !is.na(icdesc), ic!="Item code") %>%
  mutate(icdesc=gsub("[*']", "", icdesc)) %>%
  mutate_each(funs(trim))

# Exhibit Codes Related to Public Employee Retirement Systems Expenditure - Chap 8 Census 2006 Classification Manual
icsup <- "ic, icdesc
Z13, Retirement Benefits
Z14, Disability Benefits
Z15, Survivor Benefits
Z16, Other Benefits"
icdf2 <- read_csv(icsup) %>% mutate_each(funs(trim))
# icdf2 <- read.table(textConnection(icsup), sep=",", colClasses="character", header=TRUE, fill=TRUE) %>% mutate_each(funs(trim))

icdf <- bind_rows(icdf1, icdf2)
saveRDS(icdf, paste0(cenretd, "retitemcodes.rds"))


#************************************************************************************************************************
#
#                Get annual data, WITH IMPUTATIONS ####
#
#************************************************************************************************************************
read35 <- function(year, fn){
  wids <- c(14, 3, 12, 2, 2, 2)
  cn <- c("id", "ic", "value", "syear", "dyear", "source")
  df <- read_fwf(paste0(unit35d, fn), fwf_widths(wids, cn), col_types="ccdiic")
  return(df)
}

impfiles <- paste0(2007:2013, "indiv_unit_reported_data.txt") # names of files with imputations

udf <- data.frame(year=2007:2013, fn=impfiles) %>%
  group_by(year) %>%
  do(read35(.$year, .$fn))


#************************************************************************************************************************
#
#                Add item code info and system names ####
#
#************************************************************************************************************************
dfall <- udf %>% mutate(stabbr=stcodes$stabbr[match(str_sub(id, 1, 2), stcodes$stcen)],
                     icf=icdf$icdesc[match(ic, icdf$ic)],
                     type=as.numeric(str_sub(id, 3, 3)),
                     typef=factor(type, 0:5, c("State", "County", "City", "Municipal\town", "Special District", "School"))) %>%
  left_join(select(iddf, id, year, idname, pubname=pubname2)) # pretty slow
ht(dfall)

saveRDS(dfall, paste0(cenretd, "retsysunits.rds"))



#************************************************************************************************************************
#
#                Check out the data ####
#
#************************************************************************************************************************
retdf <- readRDS(paste0(cenretd, "retsysunits.rds"))
retdf %>% count(value==0)
count(retdf, year, stabbr) %>% spread(year, n)
retdf %>% select(year, id) %>% unique %>% count(., year) # systems by year
retdf %>% select(year, id, stabbr) %>% unique %>% count(., year, stabbr) %>% spread(year, n) # systems by state and year
df <- retdf %>% select(year, id, stabbr) %>% unique %>% count(., year, stabbr) %>% spread(year, n) %>% arrange(desc(`2012`))
df
sum(df$`2012`)
# Big drop in # of items 2007 to 2008, also # systems
# big increase # sys 2007 to 2012
# funny movement in AR


# # get the unit data ####
# # wids <- c(2, 1, 3, 3, 3, 2, 3, 12, 2, 2, 2)
# # cn <- c("stcode", "type", "cocode", "id", "suppcode", "subcode", "ic", "value", "syear", "dyear", "source")
# wids <- c(14, 3, 12, 2, 2, 2)
# cn <- c("id", "ic", "value", "syear", "dyear", "source")
# unitdf <- read_fwf(paste0(cenretd, ufn), fwf_widths(wids, cn), col_types="ccdiic")
# glimpse(unitdf)
# count(unitdf, ic)
# count(unitdf, id) %>% nrow # only 1608 rows!?
# 
# # get unit ids ####
# # cn <- c("id", "idname")
# # iddf <- read_fwf(paste0(d2012, idfn), fwf_positions(c(1, 16), c(14, 100), col_names=cn), col_types="cc")
# # cn <- c("id", "idname", "pubname")
# # iddf <- read_fwf(paste0(d2012, idfn), fwf_positions(c(1, 16, 101), c(14, 100, 161), col_names=cn), col_types="ccc")
# # the file is very messy so read it in as follows and then parse
# chunk <- scan(file=paste0(d2012, idfn), what="", sep="\n", nlines=-1)
# iddf <- data_frame(id=str_sub(chunk, 1, 14), idname=trim(str_sub(chunk, 16, 100)), pubname=trim(str_sub(chunk, 101, nchar(chunk))))
# iddf2 <- iddf %>% mutate(pubname2=ifelse(pubname=="", idname, pubname))


# 0 = State 
# 1 = County 
# 2 = City 
# 3 = Municipalities and Townships 
# 4 = Special Districts 
# 5 = Schools

# retdf <- left_join(unitdf, select(iddf2, -pubname, pubname=pubname2))
# retdf2 <- retdf %>% mutate(year=2012, stcode=str_sub(id, 1, 2), 
#                            stabbr=stcodes$stabbr[match(stcode, stcodes$stcen)],
#                            icf=icdf$icdesc[match(ic, icdf$ic)],
#                            type=as.numeric(str_sub(id, 3, 3)),
#                            typef=factor(type, 0:5, c("State", "County", "City", "Municipal\town", "Special District", "School"))) %>%
#   select(id, stabbr, year, type, typef, ic, value, idname, pubname, icf)
# glimpse(retdf2)
# ht(retdf2)

