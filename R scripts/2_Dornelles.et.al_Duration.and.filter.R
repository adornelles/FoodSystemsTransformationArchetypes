# Andre Dornelles, August 2019.

# 2_Dornelles.et.al_Duration.and.filter:

# This script analyses the duration of all metrics acquired, and identifies the filter of best fit to be applied for the computation of the trend analysis (Script 3 and 4).

## This script has five parts: 
# 1. Upload collated metric files; 
# 2. Loop to calculate time intervals; 
# 3. Merge with country look up table and plot of the duration analysis;
# 4. Filter of best fit analysis from 1961 to 2015; 
# 5. Generate .csv files for each metric. 

## Instructions (repeat it for each metric individually):
# First: upload the country look up table (country look up table directory);
# Second: load in the data file of one metric in Part 1 (collated data directory);
# Third: run Part 2 to calculate the time intervals; 
# Fourth: use the country look up table to compile results by common country name of the respective metric and plot the duration analysis;
# Fifth: apply the filter of best fit in Part 4;
# Sixth: generate the .csv file for the respective metric in Part 5 (filter of best fit directory).

# Country look up table
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
country.look.up<-read.csv("Country name look up table_updated.csv",header=T)
names(country.look.up) 

# PART 1. Upload collated files   
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data\\Collated data files")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data\\Collated data files")

## 1.1 Structure metrics:

# Agricultural area:
fsdata<-read.csv("Collated_ind_Agricultural area.csv",header=T)

# Synthetic fertilizer use:
fsdata<-read.csv("Collated_ind_ATFP Synthetic Fertilizer consumption.csv",header=T)

# Pesticide use:
fsdata<-read.csv("Collated_ind_Pesticide use.csv",header=T) 

# Agricultural employment:
fsdata<-read.csv("Collated_der_WB ILO Agricultural employment, percent of total employment.csv",header=T)

# GAO:
fsdata<-read.csv("Collated_ind_Gross Agricultural Output.csv",header=T)

# ATFP:
fsdata<-read.csv("Collated_agr_Agricultural Total Factor Productivity.csv",header=T)
fsdata$value<-as.numeric(as.vector(fsdata$value)) 
fsdata[is.na(fsdata$value),]  # one NA added in by coercion for Kuwait which did have a value of 1,151
fsdata$value[fsdata$country=="Kuwait"&fsdata$year==2014]<-1151  # replace this with the corrected original value

# Food imports:
fsdata<-read.csv("Collated_ind_Food imports absolute.csv",header=T)

# Food exports:
fsdata<-read.csv("Collated_ind_Food exports absolute.csv",header=T)

# CPI, Food:
fsdata<-read.csv("Collated_agr_CPI, Food.csv",header=T)

# PPI: Agriculture:
fsdata<-read.csv("Collated_agr_Producer Price Index.csv",header=T)

## 1.2 Outcome metrics:

# Forested area:
fsdata<-read.csv("Collated_ind_Forest.area.csv",header=T)

# Red list index:
fsdata<-read.csv("Collated_agr_Red List Index.csv",header=T)

# NFA National Ecological Foodprint, Total Consumption (global hectares, gha) 
fsdata<-read.csv("Collated_agr_NFA Total Consumption.csv",header=T)
fsdata$year<-as.numeric(fsdata$year)
fsdata$value<-as.numeric(fsdata$value)

# Undernourishment:
fsdata<-read.csv("Collated_der_FAO Undernourishment.csv",header=T)
fsdata<-na.omit(fsdata) # remove NAs
fsdata$value<-as.character(fsdata$value)
fsdata$value[fsdata$value=="<2.5"]<-2.0 # replacing the "2.5" for "2.0" so they won't be framed as NAs when expressed as numeric
fsdata$value<-as.numeric(fsdata$value)
fsdata$value[fsdata$country=="Australia"] # Check if the replacement is adequate
fsdata$value[fsdata$country=="Brazil"] # Check if the replacement is adequate
fsdata[is.na(fsdata$value),]

# Obesity:
fsdata<-read.csv("Collated_der_Obesity prevalence, adults.csv",header=T)

# Total Agricultural GHG emissions:
fsdata<-read.csv("Collated_der_CAIT Agriculture total GHG Emissions.csv",header=T)

# Total Land-use change and Forestry GHG emissions:
fsdata<-read.csv("Collated_der_CAIT Land-Use Change and Forestry total GHG Emissions.csv",header=T)

# Total AFOLU GHG emissions (Agriculture + Forestry + Land-Use Change):
fsdata<-read.csv("Collated_der_CAIT AFOLU total GHG Emissions.csv",header=T)

# PART 2. Loop to analyse years of observation per metric. Generate files standardized by (14 columns): country; start.year; final.year; duration; duration.80; duration.90; duration.95; duration.00; duration.05; duration.61.05; duration.80.05; duration.90.05; duration.95.05; duration.00.05)

head(fsdata) ; nrow(fsdata)
fsdata<-na.omit(fsdata)  # remove rows with no data. In some cases, some countries have no data at all even though they appear in raw data tables, e.g. American Samoa in FAo import export data
# with(fsdata,plot(value~year,ylim=c(0,200),type="n"))
# 10 years trend
country.list<-names(with(fsdata,table(country)))[with(fsdata,table(country))>=10]# remove countries with less than 10 datapoints (years)
length(country.list)
country.list

#pick transparent colours
transp.blue <- rgb(col2rgb("blue")[1], col2rgb("blue")[2], col2rgb("blue")[3],max = 255,alpha = (30)*255/100)
transp.red <- rgb(col2rgb("red")[1], col2rgb("red")[2], col2rgb("red")[3],max = 255,alpha = (30)*255/100)
devAskNewPage(ask = FALSE)   #  asks user to press enter before seeing next plot. TRUE=yes and FALSE=no

results<-NULL  # creating an empty table
for (i in country.list[]){    
  country.data<-fsdata[fsdata$country==i,]
  country<-i
  with(country.data,plot(value~year,type="o",main=i,lwd=2)) 
  country.data<-country.data[order(country.data$year),]  
  max.year<-max(country.data$year)  
  min.year<-min(country.data$year)
  duration<-max.year-min.year+1
  min.year.80<-ifelse(min.year<=1980,min.year.80<-1980,min.year.80<-min.year)
  duration.80<-max.year-min.year.80+1
  min.year.90<-ifelse(min.year<=1990,min.year.90<-1990,min.year.90<-min.year)
  duration.90<-max.year-min.year.90+1
  min.year.95<-ifelse(min.year<=1995,min.year.95<-1995,min.year.95<-min.year)
  duration.95<-max.year-min.year.95+1
  min.year.00<-ifelse(min.year<=2000,min.year.00<-2000,min.year.00<-min.year)
  duration.00<-max.year-min.year.00+1
  min.year.05<-ifelse(min.year<=2005,min.year.05<-2005,min.year.05<-min.year)
  duration.05<-max.year-min.year.05+1
  max.year.05<-ifelse(max.year>=2005,max.year.05<-2005,max.year.05<-max.year)
  duration.61.05<-max.year.05 - min.year + 1
  duration.80.05<-max.year.05 - min.year.80 + 1
  duration.90.05<-max.year.05 - min.year.90 + 1
  duration.95.05<-max.year.05 - min.year.95 + 1
  duration.00.05<-max.year.05 - min.year.00 + 1
  results.temp<-data.frame(country,min.year,max.year,duration,duration.80,duration.90,duration.95,duration.00,
                           duration.05,duration.61.05,duration.80.05,duration.90.05,duration.95.05,duration.00.05,
                           min.year.80,min.year.90,min.year.95,min.year.00,min.year.05,max.year.05) #storing results line
  results<-rbind(results,results.temp) #  'growing' the results table by one line
} # end i in country loop
head(results) ; nrow(results)

# Add the vertical lines to the plots of duration analysis below
abline(v=c(1961,2015),lty=2,col="red",lwd=5)
abline(v=c(1980,2015),lty=2,col="red",lwd=5)
abline(v=c(1990,2015),lty=2,col="red",lwd=5)
abline(v=c(1995,2015),lty=2,col="red",lwd=5)
abline(v=c(2000,2015),lty=2,col="red",lwd=5)
abline(v=c(2005,2015),lty=2,col="red",lwd=5)
abline(v=c(1961,2005),lty=2,col="red",lwd=5)
abline(v=c(1980,2005),lty=2,col="red",lwd=5)
abline(v=c(1990,2005),lty=2,col="red",lwd=5)
abline(v=c(2000,2005),lty=2,col="red",lwd=5)

# PART 3. Merge with country look up table, plot the duration analysis, and prepare the dataframe for the filter of best fit analysis.
# Structure metrics:

# Agricultural area
duration.table.agr.area<-results # 234 countries
names(country.look.up)
country.look.up.for.ag.area<-country.look.up[,c(1,6)]
duration.table.agr.area<-merge(duration.table.agr.area,country.look.up.for.ag.area,by.x="country",by.y="Agric.area")
head(duration.table.agr.area); nrow(duration.table.agr.area) # 226 countries 
duration.table.agr.area<-duration.table.agr.area[,c(21,2:20)]
results.ag.area<-duration.table.agr.area
coverage.period<-results.ag.area
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),] 
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Agricultural Area",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.agr.area
library(dplyr)
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA))) # Replace all negative and 0 values by NA (also removes country names)
head(df);nrow(df) # df to be used in Part 4: table of best fit analysis.

# Synthetic fertilizers use
duration.table.synt.fert.use<-results # 173 countries
country.look.up.for.synt.fert.use<-country.look.up[,c(1,4)] 
duration.table.synt.fert.use<-merge(duration.table.synt.fert.use,country.look.up.for.synt.fert.use,by.x="country",by.y="Synthetic.fertilizers") 
head(duration.table.synt.fert.use); nrow(duration.table.synt.fert.use) # 170 countries  
duration.table.synt.fert.use<-duration.table.synt.fert.use[,c(21,2:20)]
results.synt.fert.use<-duration.table.synt.fert.use
coverage.period<-results.synt.fert.use
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Synthetic Fertilizer use",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.synt.fert.use
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Pesticide use
duration.table.pest.use<-results # 110 countries
country.look.up.for.pest.use<-country.look.up[,c(1,10)]
duration.table.pest.use<-merge(duration.table.pest.use,country.look.up.for.pest.use,by.x="country",by.y="Pesticide")
head(duration.table.pest.use); nrow(duration.table.pest.use) # 109 countries  
duration.table.pest.use<-duration.table.pest.use[,c(21,2:20)]
results.pest.use<-duration.table.pest.use
coverage.period<-results.pest.use
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Pesticide use",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="Pesticide use",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.pest.use
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Agricultural employment
duration.table.ag.empl<-results # 233 countries
country.look.up.for.ag.empl<-country.look.up[,c(1,34)]
unique(duration.table.ag.empl$country[!duration.table.ag.empl$country%in%country.look.up.for.ag.empl$WB.ILO.Agric.employ]) # different countries between the datasets (run before merging)
duration.table.ag.empl<-merge(duration.table.ag.empl,country.look.up.for.ag.empl,by.x="country",by.y="WB.ILO.Agric.employ") # add in common name
head(duration.table.ag.empl); nrow(duration.table.ag.empl) # 186 countries   
duration.table.ag.empl<-duration.table.ag.empl[,c(21,2:20)]
results.ag.empl<-duration.table.ag.empl
coverage.period<-results.ag.empl
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),] 
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Agricultural employment",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,final.year),main="Agricultural employment",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="Agricultural employment",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.ag.empl
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA))) # Replace all negative and 0 values by NA (also removes country names)
head(df);nrow(df)

# Gross Agricultural Output
duration.table.gao<-results # 215 countries
country.look.up.for.gao<-country.look.up[,c(1,8)] 
duration.table.gao<-merge(duration.table.gao,country.look.up.for.gao,by.x="country",by.y="Agric.output") # add in common name
head(duration.table.gao); nrow(duration.table.gao) # 212 countries  
duration.table.gao<-duration.table.gao[,c(21,2:20)]
coverage.period<-duration.table.gao
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Gross Agriculture Output",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.gao
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Agricultural Total Factor Productivity
duration.table.atfp<-results # 173 countries
country.look.up.for.atfp<-country.look.up[,c(1,11)] 
duration.table.atfp<-merge(country.look.up.for.atfp,duration.table.atfp, by.x="TFP", by.y="country")
head(duration.table.atfp); nrow(duration.table.atfp) # 170 countries   
duration.table.atfp<-duration.table.atfp[,c(2:21)]
coverage.period<-duration.table.atfp
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Agriculture Total Factor Productivity",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.atfp
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Food imports
duration.table.imports<-results # 225 countries
country.look.up.for.imports<-country.look.up[,c(1,5)]
duration.table.imports<-merge(duration.table.imports,country.look.up.for.imports,by.x="country",by.y="Food.import.export")
head(duration.table.imports); nrow(duration.table.imports) # 179 countries (lost 46 regions, but no countries)  
duration.table.imports<-duration.table.imports[,c(21,2:20)]
coverage.period<-duration.table.imports
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Food imports",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.imports
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Food exports
duration.table.exports<-results # 222 countries
country.look.up.for.exports<-country.look.up[,c(1,5)]
duration.table.exports<-merge(duration.table.exports,country.look.up.for.exports,by.x="country",by.y="Food.import.export")
head(duration.table.exports); nrow(duration.table.exports) # 177 countries (lost 45 regions, but no countries) 
duration.table.exports<-duration.table.exports[,c(21,2:20)]
coverage.period<-duration.table.exports
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Food exports",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.exports
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Consumer Price Index, Food
duration.table.cpi<-results # 134 countries
country.look.up.for.cpi<-country.look.up[,c(1,20)]
duration.table.cpi<-merge(duration.table.cpi,country.look.up.for.cpi,by.x="country",by.y="CPI")
head(duration.table.cpi); nrow(duration.table.cpi) # 133 countries  
duration.table.cpi<-duration.table.cpi[,c(21,2:20)]
coverage.period<-duration.table.cpi
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
# with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Consumer Price Index, Food",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="Consumer Price Index, Food",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,plot(country.id~min.year,xlim=c(1961,final.year),main="Consumer Price Index, Food",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.cpi
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Producer Price Index, Agriculture
duration.table.ppi<-results # 151 countries
country.look.up.for.ppi<-country.look.up[,c(1,21)]
duration.table.ppi<-merge(duration.table.ppi,country.look.up.for.ppi,by.x="country",by.y="Prod.Food.Price")
head(duration.table.ppi); nrow(duration.table.ppi) # 149 countries  
duration.table.ppi<-duration.table.ppi[,c(21,2:20)]
coverage.period<-duration.table.ppi
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Producer Price Index, Agriculture",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="Producer Price Index, Agriculture",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.ppi
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Domestic Food price
duration.table.dom.fd.price<-results #  145 countries
names(country.look.up)
country.look.up.for.dom.fd.price<-country.look.up[,c(1,13)]
duration.table.dom.fd.price<-merge(duration.table.dom.fd.price,country.look.up.for.dom.fd.price,by.x="country",by.y="Domestic.food.price")
head(duration.table.dom.fd.price); nrow(duration.table.dom.fd.price) #  145 countries 
duration.table.dom.fd.price<-duration.table.dom.fd.price[,c(21,2:20)]
results.dom.fd.price<-duration.table.dom.fd.price
coverage.period<-results.dom.fd.price
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),] 
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Domestic Food Price",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.dom.fd.price
library(dplyr)
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA))) # Replace all negative and 0 values by NA (also removes country names)
head(df);nrow(df) # df to be used in Part 4: table of best fit analysis.

# Domestic Food Price volatility:
duration.table.dom.fd.price.volat<-results #  167 countries
names(country.look.up)
country.look.up.for.dom.fd.price.volat<-country.look.up[,c(1,12)]
duration.table.dom.fd.price.volat<-merge(duration.table.dom.fd.price.volat,country.look.up.for.dom.fd.price.volat,by.x="country",by.y="Price.volatility")
head(duration.table.dom.fd.price.volat); nrow(duration.table.dom.fd.price.volat) #  135 countries 
duration.table.dom.fd.price.volat<-duration.table.dom.fd.price.volat[,c(21,2:20)]
results.dom.fd.price.volat<-duration.table.dom.fd.price.volat
coverage.period<-results.dom.fd.price.volat
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),] 
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Domestic Food Price Volatility",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.dom.fd.price.volat
library(dplyr)
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA))) # Replace all negative and 0 values by NA (also removes country names)
head(df);nrow(df) # df to be used in Part 4: table of best fit analysis.

# Outcome metrics:
# Forest area
duration.table.for.area<-results # 230 countries
country.look.up.for.forest<-country.look.up[,c(1,23)]
duration.table.for.area<-merge(duration.table.for.area,country.look.up.for.forest,by.x="country",by.y="Forest.area")
head(duration.table.for.area) ; nrow(duration.table.for.area)  # 223 countries 
duration.table.for.area<-duration.table.for.area[,c(21,2:20)]
coverage.period<-duration.table.for.area
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Forest Area",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="Forest Area",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.for.area
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Red List Index
duration.table.red.list<-results # 219 countries
country.look.up.for.red.list<-country.look.up[,c(1,24)]
duration.table.red.list<-merge(duration.table.red.list,country.look.up.for.red.list,by.x="country",by.y="Red.list")
head(duration.table.red.list) ; nrow(duration.table.red.list) # 194 countries 
duration.table.red.list<-duration.table.red.list[,c(21,2:20)]
coverage.period<-duration.table.red.list
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Red List Index",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="Red List Index",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,final.year),main="Red List Index",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.red.list
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# NFA National Ecological Foodprint, Total Consumption (global hectares, gha)
duration.nfa.footprint<-results # 187 countries
country.look.up.for.nfa.footprint<-country.look.up[,c(1,26)]
duration.nfa.footprint<-merge(duration.nfa.footprint,country.look.up.for.nfa.footprint,by.x="country",by.y="NFA")
head(duration.nfa.footprint) ; nrow(duration.nfa.footprint) # 185 countries 
duration.nfa.footprint<-duration.nfa.footprint[,c(21,2:20)]
coverage.period<-duration.nfa.footprint
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="NFA Ecological Footprint, Consumption",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="NFA Ecological Footprint, Consumption",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,final.year),main="NFA Ecological Footprint, Consumption",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.nfa.footprint
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Undernourishment
duration.table.undern<-results # 166 countries
country.look.up.for.undernourishment<-country.look.up[,c(1,28)]
duration.table.undern<-merge(duration.table.undern,country.look.up.for.undernourishment,by.x="country",by.y="Undernourishment")
head(duration.table.undern) ; nrow(duration.table.undern)   # 165 countries 
duration.table.undern<-duration.table.undern[,c(21,2:20)]
coverage.period<-duration.table.undern
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Undernourishment",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="Undernourishment",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,final.year),main="Undernourishment",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.undern
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Obesity 
duration.table.obesity<-results # 191 countries
country.look.up.for.obesity<-country.look.up[,c(1,27)]
duration.table.obesity<-merge(duration.table.obesity,country.look.up.for.obesity,by.x="country",by.y="Obesity")
head(duration.table.obesity) ; nrow(duration.table.obesity) # 189 countries 
duration.table.obesity<-duration.table.obesity[,c(21,2:20)]
coverage.period<-duration.table.obesity
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Obesity",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="Obesity",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,final.year),main="Obesity",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.obesity
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Agricultural total GHG emissions 
duration.table.agric.ghg<-results # 193 countries
country.look.up.for.GHG.emissions<-country.look.up[,c(1,35)]
duration.table.agric.ghg<-merge(duration.table.agric.ghg,country.look.up.for.GHG.emissions,by.x="country",by.y="CAIT.Agric.emissions")
head(duration.table.agric.ghg) ; nrow(duration.table.agric.ghg) # 191 countries 
duration.table.agric.ghg<-duration.table.agric.ghg[,c(21,2:20)]
coverage.period<-duration.table.agric.ghg
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Agricultural total GHG emissions",ylab="Country ID",xlab="Year",pch=20))
 with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="Agricultural total GHG emissions",ylab="Country ID",xlab="Year",pch=20))
 with(coverage.period,plot(country.id~min.year,xlim=c(1961,final.year),main="Agricultural total GHG emissions",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.agric.ghg
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Land-use change and Forestry total GHG emissions
duration.table.land.for.ghg<-results # 193 countries
country.look.up.for.GHG.emissions<-country.look.up[,c(1,35)]
duration.table.land.for.ghg<-merge(duration.table.land.for.ghg,country.look.up.for.GHG.emissions,by.x="country",by.y="CAIT.Agric.emissions")
head(duration.table.land.for.ghg) ; nrow(duration.table.land.for.ghg) # 191 countries  
duration.table.land.for.ghg<-duration.table.land.for.ghg[,c(21,2:20)]
coverage.period<-duration.table.land.for.ghg
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Land-use change and Forestry total GHG emissions",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="Land-use change and Forestry total GHG emissions",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,final.year),main="Land-use change and Forestry total GHG emissions",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.land.for.ghg
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# AFOLU total GHG emissions
duration.table.afolu.ghg<-results # 193 countries
country.look.up.for.GHG.emissions<-country.look.up[,c(1,35)]
duration.table.afolu.ghg<-merge(duration.table.afolu.ghg,country.look.up.for.GHG.emissions,by.x="country",by.y="CAIT.Agric.emissions")
head(duration.table.afolu.ghg) ; nrow(duration.table.afolu.ghg) # 191 countries  
duration.table.afolu.ghg<-duration.table.afolu.ghg[,c(21,2:20)]
coverage.period<-duration.table.afolu.ghg
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),]
length(coverage.period$Common.name)
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="AFOLU total GHG emissions",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,2015),main="AFOLU total GHG emissions",ylab="Country ID",xlab="Year",pch=20))
# with(coverage.period,plot(country.id~min.year,xlim=c(1961,final.year),main="Land-use change and Forestry total GHG emissions",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.afolu.ghg
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA)))
head(df);nrow(df)

# Per capita food supply variability:
duration.table.pcfs.variab<-results #  174 countries
names(country.look.up)
country.look.up.for.pcfs<-country.look.up[,c(1,14)]
duration.table.pcfs.variab<-merge(duration.table.pcfs.variab,country.look.up.for.pcfs,by.x="country",by.y="Per.capita.supply")
head(duration.table.pcfs.variab); nrow(duration.table.pcfs.variab) #  171 countries 
duration.table.pcfs.variab<-duration.table.pcfs.variab[,c(21,2:20)]
results.pcfs.variab<-duration.table.pcfs.variab
coverage.period<-results.pcfs.variab
coverage.period<-coverage.period[order(coverage.period$min.year,decreasing=TRUE),] 
coverage.period$country.id<-(1:length(coverage.period$Common.name))
start.year<-min(coverage.period$min.year)
final.year<-max(coverage.period$max.year)
windows(11,11)
par(mfrow=c(1,1))
with(coverage.period,plot(country.id~min.year,xlim=c(start.year,final.year),main="Per capita food supply variability",ylab="Country ID",xlab="Year",pch=20))
with(coverage.period,points(country.id~max.year,pch=20))
segments(coverage.period$min.year,coverage.period$country.id,coverage.period$max.year,coverage.period$country.id, lty=2, lwd=0.6)
df<-duration.table.pcfs.variab
library(dplyr)
df <- select_if(df,is.numeric) %>%
  mutate_all(funs(replace(., .<=0, NA))) # Replace all negative and 0 values by NA (also removes country names)
head(df);nrow(df) # df to be used in Part 4: table of best fit analysis.

# PART 4. Generate the table of Best fit analysis: the last year of the dataset was stablished as 2015 and the first date is 1961:
library(dplyr)
test.90<-as.numeric (df %>%
                       filter(duration>=(2015-1961+1)*0.9) %>%
                       tally) # tally computes the number of observations following the filter applied
test.80<-as.numeric (df %>%
                       filter(duration>=(2015-1961+1)*0.8) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(duration>=(2015-1961+1)*0.7) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
test.90<-as.numeric(df %>% # There's no need to filter by min and max year (e.g. 1980) because the duration loop already filters for that previously (Part 2)
                      filter(duration.80>=(2015-1980+1)*0.9) %>%
                      tally)
test.80<-as.numeric (df %>%
                       filter(duration.80>=(2015-1980+1)*0.8) %>%
                       tally)
test.70<-as.numeric (df %>%
                       filter(duration.80>=(2015-1980+1)*0.7) %>%
                       tally)
dur.table.temp<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(test.table,dur.table.temp)
test.90<-as.numeric (df %>%
                       filter(duration.90>=(2015-1990+1)*0.9) %>%
                       tally)
test.80<-as.numeric (df %>%
                       filter(duration.90>=(2015-1990+1)*0.8) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(duration.90>=(2015-1990+1)*0.7) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(duration.table.temp, test.table)
test.90<-as.numeric (df %>%
                       filter(duration.95>=(2015-1995+1)*0.9) %>%
                       tally)
test.80<-as.numeric (df %>%
                       filter(duration.95>=(2015-1995+1)*0.8) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(duration.95>=(2015-1995+1)*0.7) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(duration.table.temp, test.table)
test.90<-as.numeric (df %>%
                       filter(duration.00>=(2015-2000+1)*0.9) %>%
                       tally)
test.80<-as.numeric (df %>%
                       filter(duration.00>=(2015-2000+1)*0.8) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(duration.00>=(2015-2000+1)*0.7) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(duration.table.temp, test.table)
test.90<-as.numeric (df %>%
                       filter(duration.05>=(2015-2005+1)*0.9) %>%
                       tally)
test.80<-as.numeric (df %>%
                       filter(duration.05>=(2015-2005+1)*0.8) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(duration.05>=(2015-2005+1)*0.7) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(duration.table.temp, test.table)
test.90<-as.numeric (df %>%
                       filter(duration.61.05>=(2005-1961+1)*0.9) %>%
                       tally)
test.80<-as.numeric (df %>%
                       filter(duration.61.05>=(2005-1961+1)*0.8) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(duration.61.05>=(2005-1961+1)*0.7) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(duration.table.temp, test.table)
test.90<-as.numeric (df %>%
                       filter(duration.80.05>=(2005-1980+1)*0.9) %>%
                       tally)
test.80<-as.numeric (df %>%
                       filter(duration.80.05>=(2005-1980+1)*0.8) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(duration.80.05>=(2005-1980+1)*0.7) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(duration.table.temp, test.table)
test.90<-as.numeric (df %>%
                       filter(duration.90.05>=(2005-1990+1)*0.9) %>%
                       tally)
test.80<-as.numeric (df %>%
                       filter(duration.90.05>=(2005-1990+1)*0.8) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(duration.90.05>=(2005-1990+1)*0.7) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(duration.table.temp, test.table)
test.90<-as.numeric (df %>%
                       filter(duration.95.05>=(2005-1995+1)*0.9) %>%
                       tally)
test.80<-as.numeric (df %>%
                       filter(duration.95.05>=(2005-1995+1)*0.8) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(duration.95.05>=(2005-1995+1)*0.7) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(duration.table.temp, test.table)
test.90<-as.numeric (df %>%
                       filter(duration.00.05>=(2005-2000+1)*0.9) %>%
                       tally)
test.80<-as.numeric (df %>%
                       filter(duration.00.05>=(2005-2000+1)*0.9) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(duration.00.05>=(2005-2000+1)*0.9) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(duration.table.temp, test.table)
#duration.table.temp$period<-c("1961-2015","1980-2015","1990-2015","1995-2015","2000-2015","2005-2015","1961-2005","1980-2005","1990-2005","1995-2005","2000-2005")
#duration.table.temp<-duration.table.temp[c(4,1:3)]
duration.table.temp$initial.year<-c("1961","1980","1990","1995","2000","2005","1961","1980","1990","1995","2000")
duration.table.temp$last.year<-c("2015","2015","2015","2015","2015","2015","2005","2005","2005","2005","2005")
duration.table.temp<-duration.table.temp[c(4,5,1:3)]
head(duration.table.temp)

# PART 5: Generate files for the filter of best fit:

#Set working directory to save Filters of best fit:
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\Filter of best fit")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\Filter of best fit")

#Structure metrics
write.csv(duration.table.temp,"Filter_best_fit_Agriculture Area_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Synthetic Fertiliser_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Pesticide_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Agricultural employment_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Gross Agricultural Output_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Agricultural Total Factor Productivity_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Food imports_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Food exports_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Consumer Price Index, Food_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Producer Price Index, Agriculture_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Domestic Food Price_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Domestic Food Price Volatility_1961 to 2015.csv",row.names=F)
#Outcomes
write.csv(duration.table.temp,"Filter_best_fit_Forest area_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_IUCN Red List index_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_NFA,Ecological Footprint_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Undernourishment_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Obesity_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Agricultural total GHG emissions_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Land Forestry total GHG emissions_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_AFOLU total GHG emissions_1961 to 2015.csv",row.names=F)
write.csv(duration.table.temp,"Filter_best_fit_Per capita food supply variability_1961 to 2015.csv",row.names=F)