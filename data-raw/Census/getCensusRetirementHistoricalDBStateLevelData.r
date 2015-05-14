










##################################### Get Census historical national and state-level annual data ###############################################
# 1957-1982 by 5's, then annual through 1992, then 1997, 2002, and 2007
# so must add annual files from 1993 forward
# Here are type codes:
# 0  P	Nat Total
# 4	P	 Fed Govt
# 1	P	 US, Total
# 2	P	  State-Admin
# 3	P	  Local-Admin
# 6	P	  Municipal
# x	P	  Other  

# First, get finance data
system.time(ret <- read.xls(paste(retdb, natl), sheet="Finances", colClasses="character",comment.char="",header=FALSE))
head(ret)
ret2<-ret
head(ret2,10); tail(ret2)
vnames<-unlist(as.vector(ret2[1,])) # with skip 11, first row is a (hidden) row of varnames
names(vnames)<-NULL
vnames
vnames[8]<-"admin"
names(ret2)
names(ret2)<-vnames
str(ret2)
ret2[1:15,c(1:9,ncol(ret2))]
# now start cleaning it up
ret2$stabbr<-factor(ret2$State, levels=stcen, labels=stabbr)
names(ret2)<-rencol(ret2,"Year","year")
names(ret2)<-rencol(ret2,"Type","type")
ret2$year<-as.numeric(ret2$year)
ret2$type<-as.numeric(ret2$type)
ret2<-ret2[-1,]
names(ret2)
ret2$admin<-trim(ret2$admin)
ret2$admin<-ifelse(grepl("Total",ret2$admin),"Total",ret2$admin) # remove stabbr, etc. from admin
ret2$admin<-ifelse(grepl("State-Admin",ret2$admin),"State",ret2$admin)
ret2$admin<-ifelse(grepl("Local-Admin",ret2$admin),"Local",ret2$admin)
count(ret2,c("type","admin"))

# now we are ready to create a long file
ret3<-subset(ret2,select=-c(SortCode,ID,State,FIPS,Pasis))
head(ret3)
count(ret3,c("type","admin"))
ret3[1:10,c(1:5,ncol(ret3))]
retlong<-melt(ret3,id=c("year","stabbr","type","admin"))
retlong$value<-ctov(retlong$value)
retlong<-subset(retlong, value!=0 & value!=-11111 & !is.na(value))
head(retlong); tail(retlong)
count(retlong,"variable")
count(retlong,c("type","admin"))
retlong<-retlong[order(retlong$stabbr,retlong$type,retlong$admin,retlong$variable,retlong$year),]
save(retlong,file=(paste(retdb,"retlong",".RData",sep="")))

load(file=(paste(retdb,"retlong",".RData",sep="")))
count(retlong,"variable")
tmp<-count(retlong,"variable")
grep("mem",tmp$variable,ignore.case=TRUE,value=TRUE)
# vars<-c("TotERRev","TotEmpContrb","TotGovContrb","TotERCashSec","TotERExp","TotalEarns")
# retw<-dcast(subset(retlong, type %in% c(1,2,3) & variable %in% vars),stabbr+year+admin~variable,sum,na.rm=TRUE ) # type: SL,S,L admin
retw<-dcast(subset(retlong, type %in% c(1,2,3)),stabbr+year+admin~variable,sum,na.rm=TRUE ) # type: SL,S,L admin
count(retw,"admin")
# retw$flowxii<-retw$TotEmpContrb+retw$TotGovContrb-retw$TotERExp
# retw$iineedpct<--retw$flowxii/retw$TotERCashSec*100
save(retw,file=(paste(centabs,"retw",".RData",sep="")))



# Now, get membership data # about 50 secs to read the sheet so save a copy before modifying
system.time(mem<-read.xls(paste(retdb,natl,sep=""),sheet="Membership",colClasses="character",skip=11,comment.char="",header=FALSE))
head(mem)
mem2<-mem
head(mem2,10); tail(mem2)
vnames<-unlist(as.vector(mem2[1,])) # with skip 11, first row is a (hidden) row of varnames
names(vnames)<-NULL
vnames
vnames[7]<-"admin"
names(mem2)
names(mem2)<-vnames
str(mem2)
mem2[1:15,c(1:9,ncol(mem2))]
# now start cleaning it up
mem2$stabbr<-factor(mem2$State, levels=stcen, labels=stabbr)
names(mem2)<-rencol(mem2,"Year","year")
names(mem2)<-rencol(mem2,"Type","type")
mem2$year<-as.numeric(mem2$year)
mem2$type<-as.numeric(mem2$type)
mem2<-mem2[-1,]
names(mem2)
mem2$admin<-trim(mem2$admin)
count(mem2,"admin")
mem2$admin<-ifelse(grepl("Total",mem2$admin),"Total",mem2$admin) # remove stabbr, etc. from admin
mem2$admin<-ifelse(grepl("State-Admin",mem2$admin),"State",mem2$admin)
mem2$admin<-ifelse(grepl("Local-Admin",mem2$admin),"Local",mem2$admin)
count(mem2,c("type","admin")) # some cleanup still to do here

# now we are ready to create a long file
mem3<-subset(mem2,select=-c(SortCode,ID,State,FIPS))
head(mem3)
count(mem3,c("type","admin"))
mem3[1:10,c(1:5,ncol(mem3))]
memlong<-melt(mem3,id=c("year","stabbr","type","admin"))
memlong$value<-ctov(memlong$value)
memlong<-subset(memlong, value!=0 & value!=-11111 & !is.na(value))
head(memlong); tail(memlong)
count(memlong,"variable")
count(memlong,c("type","admin"))
memlong<-memlong[order(memlong$stabbr,memlong$type,memlong$admin,memlong$variable,memlong$year),]
save(memlong,file=(paste(retdb,"memlong",".RData",sep="")))

# finally, make a wide version of the membership file
load(file=(paste(retdb,"memlong",".RData",sep="")))
count(memlong,"variable")
tmp<-count(memlong,"variable")
grep("mem",tmp$variable,ignore.case=TRUE,value=TRUE)
memw<-dcast(subset(memlong, type %in% c(1,2,3)),stabbr+year+admin~variable,sum,na.rm=TRUE ) # type: SL,S,L admin
count(memw,"admin")
# transformations as needed
save(memw,file=(paste(centabs,"memw",".RData",sep="")))



# merge the finance and membership files
load(file=paste(centabs,"retw",".RData",sep=""))
load(file=paste(centabs,"memw",".RData",sep=""))
retmemw<-merge(retw,memw)
save(retmemw,file=(paste(centabs,"retmemw",".RData",sep="")))
head(retmemw); tail(retmemw)
count(retmemw,"year")

qplot(year, TotBenef/1e6, data=subset(don, admin=="Total" & stabbr=="US"), geom=c("point","line"))
qplot(year, log(TotBenef), data=subset(don, admin=="Total" & stabbr=="US"), geom=c("point","line"))
qplot(year, AvgBenPmt, data=subset(don, admin=="Total" & stabbr=="US"), geom=c("point","line"))

don
comment(don)<-"miscellaneous"
attributes(don)


# djb pensim work with data ####
df<-load(file=(paste(centabs,"retmemw",".RData",sep="")))

