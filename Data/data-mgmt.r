library(ggplot2)
library(data.table)
library(stringr)
library(zoo)

# dt <- fread("Data/CMO-Historical-Data-Monthly.csv")
# 
# dt$DATE <- as.Date(dt$DATE,format="%d/%m/%Y")
# 
# ggplot(dt,aes(x=DATE,y=MAIZE))+
#   geom_line(color="indianred",size=.8)+
#   labs(x="Year",y="Maize Price (US$/MT)",caption="retrieved from the World Bank 'Pink Sheet' Data\nhttps://www.worldbank.org/en/research/commodity-markets")+
#   theme_classic()+
#   theme(axis.title = element_text(size=22),axis.text = element_text(size=18))

# corn futures
futures_dt <- fread("Data/Nasdaq-Corn-Futures-Historical-Data-Daily.csv")
wasde_dt <- fread("Data/wasde_dates.csv")

futures_dt[,`:=`(Date=as.Date(Date,format="%m/%d/%Y"))]

wasde_dt[,`:=`(Date=as.Date(paste(yr,mo,date,sep="-"),format="%Y-%m-%d"),Report=1)]

dt <- merge(futures_dt,wasde_dt[,.(Date,Report)],by="Date",all.x=T)
dt[is.na(Report)]$Report <- 0

dt[Volume=="N/A"]$Volume <- NA

dt[,`:=`(Volume=na.approx(Volume))]

fwrite(dt,"Data/futures.csv")


# corn cash
cash_dt <- fread("Data/CMO-Historical-Data-Monthly.csv")

cash_dt[,`:=`(Date=as.Date(DATE,format="%d/%m/%Y"))]

cash_dt <- cash_dt[,.(Date,Cash=MAIZE)]

futures_dt[,`:=`(yearmo=as.Date(paste0(substr(Date,1,7),"-01")))]

# transform futures cents/bu to dollars/mt
tr <- 0.3937

futures_dt <- futures_dt[,.(Futures=tr*mean(Close)),by=.(Date=yearmo)]

dt <- merge(cash_dt,futures_dt,by="Date")

fwrite(dt,"Data/monthly.csv")



dt[,`:=`(DateMo=as.Date(paste0(substr(Date,1,7),"-01")))]

futures_dt <- dt[,.(Futures=mean(Close)),by=.(date=DateMo)]
futures_dt <- futures_dt[order(date)]

# corn cash
dt <- fread("Data/CMO-Historical-Data-Monthly.csv")

dt$date <- as.Date(dt$DATE,format="%d/%m/%Y")

cash_dt <- dt[,.(date,Cash=MAIZE)]

maize_dt <- merge(cash_dt,futures_dt,by="date")


load("Data/Corn1.RData")



maize_dt[,`:=`(Futures=Futures*tr)]

ggplot(maize_dt,aes(x=date))+
  geom_line(aes(y=Cash),color="coral")+
  geom_line(aes(y=Futures),color="steelblue")+
  theme_classic()
