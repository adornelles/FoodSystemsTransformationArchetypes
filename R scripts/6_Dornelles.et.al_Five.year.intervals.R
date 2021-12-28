# Andre Dornelles, October 2019

# 6_Dornelles.et.al_Five.year.intervals

# This script calculates the longitudinal trends in the country groups (clusters), disaggregated in intervals of 5 years since 1995: a) 1995-2000; b) 2000-2005; c) 2005-2010; d) 2010-2014/15.
# Note: CACR in these 5 year-intervals does not extract the median values from start and end years, but the respective real values in the first and last years of each period.

## This script has four parts:
# 1. Upload the collated metrics;
# 2. Loop to calculate trend analysis per 5 year-intervals;
# 3. Generate .csv files for each metric;
# 4. 5 year-intervals of transformation archetypes;

## Instructions (repeat it for each metric individually):
# First: upload the country look up table (country look up table directory);
# Second: load in the data file of one metric in Part 1 (collated data directory); 
# Third: run Part 2 to calculate the 5-year interval trend analysis;
# Fourth: generate the .csv file for the respective metric in Part 3 (5 year ACCR trends directory);
# Fifth: Repeat the same steps until the 5-year interval trends have been calculate for all metrics;
# Sixth: Run Part 4 to plot the 5-year intervals trends for all metrics (group identifiers from Script 5 are required)

# 1. Upload collated files and generate trend results (expressed by Annual Compound Change Rate, ACCR) standardized by: "Common.name"; "1990-95"; "1995-2000"; "2000-2005"; "2005-2010"; "2010-14/15"

setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
country.look.up<-read.csv("Country name look up table_updated.csv",header=T)
names(country.look.up) 

# PART 1. Upload collated files   
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data\\Collated data files")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data\\Collated data files")

# 1.1 Structure metrics

# Agricultural area:
fsdata<-read.csv("Collated_ind_Agricultural area.csv",header=T)

# Synthetic fertilizer use:
fsdata<-read.csv("Collated_ind_ATFP Synthetic Fertilizer consumption.csv",header=T)

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

# PPI: Agriculture:
fsdata<-read.csv("Collated_agr_Producer Price Index.csv",header=T)

## 1.2 Outcome metrics:

# Forested area:
fsdata<-read.csv("Collated_ind_Forest.area.csv",header=T)

# Red list index:
fsdata<-read.csv("Collated_agr_Red List Index.csv",header=T)

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

# NFA National Ecological Foodprint, Total Consumption (global hectares, gha)
fsdata<-read.csv("Collated_agr_NFA Total Consumption.csv",header=T)

# PART 2. Loop to calculate trend results (expressed by Annual Compound Change Rate, ACCR) standardized by: "Common.name"; "1990-95"; "1995-2000"; "2000-2005"; "2005-2010"; "2010-14/15"

head(fsdata) ; nrow(fsdata)
fsdata<-na.omit(fsdata)  # remove rows with no data. In some cases, some countries have no data at all even though they appear in raw data tables, e.g. American Samoa in FAo import export data
library(dplyr)
fsdata<-fsdata %>%  # Filter from 1995 to 2015
  filter(year>=1995) %>%
  filter(year<=2015)
# 80% filter (duration.95>=(2015-1995+1)*0.8) = 16.8 years
country.list<-names(with(fsdata,table(country)))[with(fsdata,table(country))>=16.8]# remove countries with less than 16.8 datapoints (years), which corresponds to the 80% filter
length(country.list)
fsdata$year<-as.numeric(fsdata$year)
fsdata$value<-as.numeric(fsdata$value)
results<-NULL # creating an empty table
for (i in country.list[]){     # loop for each country  # country.list=="Netherlands Antilles (former)"
  # print(i) }
  country.data<-fsdata[fsdata$country==i,]
  country<-i
  # with(country.data,plot(value~year,type="o",main=i))   # to plot the country's data
  # calculate ACCR for frist five years (1990-95)
  base.90.95<-country.data[country.data$year%in%1990:1995,]
  year.1990<-min(base.90.95$year)
  year.1995<-max(base.90.95$year)
  value.1990<-country.data$value[country.data$year%in%min(base.90.95$year)]
  value.1995<-country.data$value[country.data$year%in%max(base.90.95$year)]
  ifelse(value.1990==0,value.1990<-0.00000001,value.1990<-value.1990) # cannot compute compound chnage with zero values, so set to non-zero (v.small)
  ifelse(value.1995==0,value.1995<-0.00000001,value.1995<-value.1995)
  accr.90.95<-((value.1995/value.1990)^(1/(year.1995-year.1990))-1)*100
  ifelse(length(accr.90.95)==0,accr.90.95<-NA,accr.90.95<-accr.90.95) # if there's no data for the period, replace the object of length zero by NA
  # calculate ACCR for the period of 1995-00
  base.95.00<-country.data[country.data$year%in%1995:2000,]
  year.1995.00<-min(base.95.00$year)
  year.2000<-max(base.95.00$year)
  value.1995.00<-country.data$value[country.data$year%in%min(base.95.00$year)]
  value.2000<-country.data$value[country.data$year%in%max(base.95.00$year)]
  ifelse(value.1995.00==0,value.1995.00<-0.00000001,value.1995.00<-value.1995.00) # cannot compute compound chnage with zero values, so set to non-zero (v.small)
  ifelse(value.2000==0,value.2000<-0.00000001,value.2000<-value.2000)
  accr.95.00<-((value.2000/value.1995.00)^(1/(year.2000-year.1995.00))-1)*100
  ifelse(length(accr.95.00)==0,accr.95.00<-NA,accr.95.00<-accr.95.00)
  # calculate ACCR for the period of 2000-05
  base.00.05<-country.data[country.data$year%in%2000:2005,]
  year.2000.05<-min(base.00.05$year)
  year.2005<-max(base.00.05$year)
  value.2000.05<-country.data$value[country.data$year%in%min(base.00.05$year)]
  value.2005<-country.data$value[country.data$year%in%max(base.00.05$year)]
  ifelse(value.2000.05==0,value.2000.05<-0.00000001,value.2000.05<-value.2000.05) # cannot compute compound chnage with zero values, so set to non-zero (v.small)
  ifelse(value.2005==0,value.2005<-0.00000001,value.2005<-value.2005)
  accr.00.05<-((value.2005/value.2000.05)^(1/(year.2005-year.2000.05))-1)*100
  ifelse(length(accr.00.05)==0,accr.00.05<-NA,accr.00.05<-accr.00.05)
  # calculate ACCR for the period of 2005-10
  base.05.10<-country.data[country.data$year%in%2005:2010,]
  year.2005.10<-min(base.05.10$year)
  year.2010<-max(base.05.10$year)
  value.2005.10<-country.data$value[country.data$year%in%min(base.05.10$year)]
  value.2010<-country.data$value[country.data$year%in%max(base.05.10$year)]
  ifelse(value.2005.10==0,value.2005.10<-0.00000001,value.2005.10<-value.2005.10) # cannot compute compound chnage with zero values, so set to non-zero (v.small)
  ifelse(value.2010==0,value.2010<-0.00000001,value.2010<-value.2010)
  accr.05.10<-((value.2010/value.2005.10)^(1/(year.2010-year.2005.10))-1)*100
  ifelse(length(accr.05.10)==0,accr.05.10<-NA,accr.05.10<-accr.05.10)
  # calculate ACCR for the period of 2010-15
  base.10.15<-country.data[country.data$year%in%2010:2015,]
  year.2010.15<-min(base.10.15$year)
  year.2015<-max(base.10.15$year)
  value.2010.15<-country.data$value[country.data$year%in%min(base.10.15$year)]
  value.2015<-country.data$value[country.data$year%in%max(base.10.15$year)]
  ifelse(value.2010.15==0,value.2010.15<-0.00000001,value.2010.15<-value.2010.15) # cannot compute compound chnage with zero values, so set to non-zero (v.small)
  ifelse(value.2015==0,value.2015<-0.00000001,value.2015<-value.2015)
  accr.10.15<-((value.2015/value.2010.15)^(1/(year.2015-year.2010.15))-1)*100
  ifelse(length(accr.10.15)==0,accr.10.15<-NA,accr.10.15<-accr.10.15)
  results.temp<-data.frame(country,accr.95.00,accr.00.05,accr.05.10,accr.10.15) #storing results line
  results<-rbind(results,results.temp) #  'growing' the results table by one line
}
head(results) ; nrow(results)

# PART 3. Set working directory to save trend results:      
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\5 year ACCR trends")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\5 year ACCR trends")

# Agriculture area:
results.ag.area<-results
names(country.look.up)
country.look.up.for.ag.area<-country.look.up[,c(1,6)]
results.ag.area<-merge(results.ag.area,country.look.up.for.ag.area,by.x="country",by.y="Agric.area")
head(results.ag.area); nrow(results.ag.area) # 216 countries 
results.ag.area<-results.ag.area[,c(6,2:5)]
write.csv(results.ag.area,"5.years_Trends_fs_structure_Agriculture area.csv",row.names=F)

# Synthetic fertilizer use:
results.synt.fert.use<-results
country.look.up.for.synt.fert.use<-country.look.up[,c(1,4)] 
results.synt.fert.use<-merge(results.synt.fert.use,country.look.up.for.synt.fert.use,by.x="country",by.y="Synthetic.fertilizers") 
head(results.synt.fert.use); nrow(results.synt.fert.use)   #170 countries 
results.synt.fert.use<-results.synt.fert.use[,c(6,2:5)]
write.csv(results.synt.fert.use,"5.years_Trends_fs_structure_Synthetic fertilizer use.csv",row.names=F)

# Agricultural employment:
results.ag.empl<-results
country.look.up.for.ag.empl<-country.look.up[,c(1,34)]
unique(results.ag.empl$country[!results.ag.empl$country%in%country.look.up.for.ag.empl$WB.ILO.Agric.employ]) # different countries between the datasets (run before merging)
results.ag.empl<-merge(results.ag.empl,country.look.up.for.ag.empl,by.x="country",by.y="WB.ILO.Agric.employ") # add in common name
head(results.ag.empl); nrow(results.ag.empl) # 186 countries  
results.ag.empl<-results.ag.empl[,c(6,2:5)]
write.csv(results.ag.empl,"5.years_Trends_fs_structure_Agriculture employment.csv",row.names=F)

# GAO:
results.gao<-results
country.look.up.for.gao<-country.look.up[,c(1,8)] 
unique(results.gao$country[!results.gao$country%in%country.look.up.for.gao$Agric.output]) # different countries between the datasets (run before merging)
results.gao<-merge(results.gao,country.look.up.for.gao,by.x="country",by.y="Agric.output") # add in common name
head(results.gao); nrow(results.gao) # 208 countries  
results.gao<-results.gao[,c(6,2:5)]
write.csv(results.gao,"5.years_Trends_fs_structure_Gross Agricultural Output.csv",row.names=F)

# ATFP:
results.atfp<-results
country.look.up.for.atfp<-country.look.up[,c(1,11)] # select correct column for the metric
unique(results.atfp$country[!results.atfp$country%in%country.look.up.for.atfp$TFP]) # different countries between the datasets (run before merging)
results.atfp<-merge(country.look.up.for.atfp,results.atfp, by.x="TFP", by.y="country")
head(results.atfp); nrow(results.atfp) #170 countries
results.atfp<-results.atfp[,c(2:6)]
write.csv(results.atfp,"5.years_Trends_fs_structure_Agriculture Total Factor Productivity Index.csv",row.names=F)

# Food imports:
results.imports<-results
country.look.up.for.imports<-country.look.up[,c(1,5)]
unique(results.imports$country[!results.imports$country%in%country.look.up.for.imports$Food.import.export]) # different countries between the datasets (run before merging)
results.imports<-merge(results.imports,country.look.up.for.imports,by.x="country",by.y="Food.import.export")
head(results.imports); nrow(results.imports) # 135 countries   
results.imports<-results.imports[,c(6,2:5)]
write.csv(results.imports,"5.years_Trends_fs_structure_Food imports.csv",row.names=F)

# Food exports:
results.exports<-results
country.look.up.for.exports<-country.look.up[,c(1,5)]
unique(results.exports$country[!results.exports$country%in%country.look.up.for.exports$Food.import.export]) # different countries between the datasets (run before merging)
results.exports<-merge(results.exports,country.look.up.for.exports,by.x="country",by.y="Food.import.export")
head(results.exports); nrow(results.exports) # 133 countries 
results.exports<-results.exports[,c(6,2:5)]
write.csv(results.exports,"5.years_Trends_fs_structure_Food exports.csv",row.names=F)

# PPI: Agriculture:
results.ppi<-results
country.look.up.for.ppi<-country.look.up[,c(1,21)]
unique(results.ppi$country[!results.ppi$country%in%country.look.up.for.ppi$Prod.Food.Price]) # different countries between the datasets (run before merging)
results.ppi<-merge(results.ppi,country.look.up.for.ppi,by.x="country",by.y="Prod.Food.Price")
head(results.ppi); nrow(results.ppi) # 141 countries  
results.ppi<-results.ppi[,c(6,2:5)]
write.csv(results.ppi,"5.years_Trends_fs_structure_Producer Price Index, Agriculture.csv",row.names=F)

## 1.2 Outcome metrics:

# Forested area:
results.for.area<-results
country.look.up.for.forest<-country.look.up[,c(1,23)]
results.for.area<-merge(results.for.area,country.look.up.for.forest,by.x="country",by.y="Forest.area")
head(results.for.area) ; nrow(results.for.area) # 216 countries  
results.for.area<-results.for.area[,c(6,2:5)]
write.csv(results.for.area,"5.years_Trends_fs_outcome_Forest area.csv",row.names=F)

# Red list index:
results.red.list<-results
country.look.up.for.red.list<-country.look.up[,c(1,24)]
results.red.list<-merge(results.red.list,country.look.up.for.red.list,by.x="country",by.y="Red.list")
head(results.red.list) ; nrow(results.red.list) # 195 countries
results.red.list<-results.red.list[,c(6,2:5)]
write.csv(results.red.list,"5.years_Trends_fs_outcome_Red LIst Index.csv",row.names=F)

# Undernourishment:
results.undern<-results
country.look.up.for.undernourishment<-country.look.up[,c(1,28)]
results.undern<-merge(results.undern,country.look.up.for.undernourishment,by.x="country",by.y="Undernourishment")
head(results.undern) ; nrow(results.undern)  # 165 countries
results.undern<-results.undern[,c(6,2:5)]
write.csv(results.undern,"5.years_Trends_fs_outcome_Undernourishment.csv",row.names=F)

# Obesity:
results.obesity<-results
country.look.up.for.obesity<-country.look.up[,c(1,27)]
results.obesity<-merge(results.obesity,country.look.up.for.obesity,by.x="country",by.y="Obesity")
head(results.obesity) ; nrow(results.obesity) # 189 countries 
results.obesity<-results.obesity[,c(6,2:5)]
write.csv(results.obesity,"5.years_Trends_fs_outcome_Obesity.csv",row.names=F)

# Total Agricultural GHG emissions:
results.agric.ghg<-results
country.look.up.for.GHG.emissions<-country.look.up[,c(1,35)]
results.agric.ghg<-merge(results.agric.ghg,country.look.up.for.GHG.emissions,by.x="country",by.y="CAIT.Agric.emissions")
head(results.agric.ghg) ; nrow(results.agric.ghg) # 191 countries 
results.agric.ghg<-results.agric.ghg[,c(6,2:5)]
write.csv(results.agric.ghg,"5.years_Trends_fs_outcome_Agriculture total GHG emissions.csv",row.names=F)

# Total Land-use change and Forestry GHG emissions:
results.land.for.ghg<-results
country.look.up.for.GHG.emissions<-country.look.up[,c(1,35)]
results.land.for.ghg<-merge(results.land.for.ghg,country.look.up.for.GHG.emissions,by.x="country",by.y="CAIT.Agric.emissions")
head(results.land.for.ghg) ; nrow(results.land.for.ghg) # 191 countries 
results.land.for.ghg<-results.land.for.ghg[,c(6,2:5)]
write.csv(results.land.for.ghg,"5.years_Trends_fs_outcome_Land-use change and Forestry total GHG emissions.csv",row.names=F)

# Total AFOLU GHG emissions (Agriculture + Forestry + Land-Use Change):
results.afolu.ghg<-results
country.look.up.for.GHG.emissions<-country.look.up[,c(1,35)]
results.afolu.ghg<-merge(results.afolu.ghg,country.look.up.for.GHG.emissions,by.x="country",by.y="CAIT.Agric.emissions")
head(results.afolu.ghg) ; nrow(results.afolu.ghg) # 191 countries 
results.afolu.ghg<-results.afolu.ghg[,c(6,2:5)]
write.csv(results.afolu.ghg,"5.years_Trends_fs_outcome_AFOLU total GHG emissions.csv",row.names=F)

# NFA National Ecological Foodprint, Total Consumption (global hectares, gha)
results.nfa<-results
country.look.up.for.nfa<-country.look.up[,c(1,26)]
results.nfa<-merge(results.nfa,country.look.up.for.nfa,by.x="country",by.y="NFA")
head(results.nfa) ; nrow(results.nfa) # 176 countries
results.nfa<-results.nfa[,c(6,2:5)]
write.csv(results.nfa,"5.years_Trends_fs_outcome_NFA Total Consumption.csv",row.names=F)

# PART 4: Plot 5 year-intervals of country groups, per metric (grops by cluster analysis from Script 4)
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\5 year ACCR trends")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\5 year ACCR trends")

#Structure metrics
#accr5y.ag.area<-results.ag.area
accr5y.ag.area<-read.csv("5.years_Trends_fs_structure_Agriculture area.csv",header=T)
#accr5y.synt.fert<-results.synt.fert.use
accr5y.synt.fert<-read.csv("5.years_Trends_fs_structure_Synthetic fertilizer use.csv",header=T)
#accr5y.ag.empl<-results.ag.empl
accr5y.ag.empl<-read.csv("5.years_Trends_fs_structure_Agriculture employment.csv",header=T)
#accr5y.gao<-results.gao
accr5y.gao<-read.csv("5.years_Trends_fs_structure_Gross Agricultural Output.csv",header=T)
#accr5y.atfp<-results.atfp
accr5y.atfp<-read.csv("5.years_Trends_fs_structure_Agriculture Total Factor Productivity Index.csv",header=T)
#accr5y.imports<-results.imports
accr5y.imports<-read.csv("5.years_Trends_fs_structure_Food imports.csv",header=T)
#accr5y.exports<-results.exports
accr5y.exports<-read.csv("5.years_Trends_fs_structure_Food exports.csv",header=T)
#accr5y.ppi<-results.ppi
accr5y.ppi<-read.csv("5.years_Trends_fs_structure_Producer Price Index, Agriculture.csv",header=T)
#Outcomes
#accr5y.for.area<-results.for.area
accr5y.for.area<-read.csv("5.years_Trends_fs_outcome_Forest area.csv",header=T)
#accr5y.red.list<-results.red.list
accr5y.red.list<-read.csv("5.years_Trends_fs_outcome_Red LIst Index.csv",header=T)
#accr5y.undern<-results.undern
accr5y.undern<-read.csv("5.years_Trends_fs_outcome_Undernourishment.csv",header=T)
#accr5y.obesity<-results.obesity
accr5y.obesity<-read.csv("5.years_Trends_fs_outcome_Obesity.csv",header=T)
#accr5y.agric.ghg<-results.agric.ghg
accr5y.agric.ghg<-read.csv("5.years_Trends_fs_outcome_Agriculture total GHG emissions.csv",header=T)
#accr5y.land.for.ghg<-results.land.for.ghg
accr5y.land.for.ghg<-read.csv("5.years_Trends_fs_outcome_Land-use change and Forestry total GHG emissions.csv",header=T)
#accr5y.afolu.ghg<-results.afolu.ghg
accr5y.afolu.ghg<-read.csv("5.years_Trends_fs_outcome_AFOLU total GHG emissions.csv",header=T)
#accr5.nfa<-results.nfa
accr5.nfa<-read.csv("5.years_Trends_fs_outcome_NFA Total Consumption.csv",header=T)

# Country groups
# cluster.groups<-all.results # all.results from script 5 (after running the inital code from Part 3)
# cluster.groups$Common.name<-row.names(cluster.groups)
# row.names(cluster.groups)<-c()
# cluster.groups<-cluster.groups[,c(6,7)] # 161 countries (after merging, this should be the n found for each metric below)
#write.csv(cluster.groups,"Cluster groups_country list.csv",row.names=F)
cluster.groups<-read.csv("Cluster groups_country list.csv",header=T)
head(cluster.groups)

# Standard codes for plots: Interval, colour
#orangecb<-rgb(230,159,0, maxColorValue = 255)
vermillioncb<-rgb(213,94,0, maxColorValue = 255)
#skybluecb<-rgb(86,180,233, maxColorValue = 255)
greencb<-rgb(0,158,115, maxColorValue = 255)
bluecb<-rgb(0,114,178, maxColorValue = 255)
group.colours<-c("1"=vermillioncb,"2"=greencb,"3"=bluecb)
group.colours.leg<-c("Rapidly expansionist"=vermillioncb,"Expansionist"=greencb,"Consolidative"=bluecb)
five.year.intervals<-c("95-00","00-05","05-10","10-15")
library(dplyr)
library(ggpubr)
library(svMisc)

# Structure metrics:

# Agricultural area:
library(reshape)
accr5y.ag.area.long<-melt(accr5y.ag.area,id.vars="Common.name")
head(accr5y.ag.area.long)
names(accr5y.ag.area.long)[2]<-"interval"
accr5y.ag.area.long.per.country.id<-merge(accr5y.ag.area.long,cluster.groups,by.x="Common.name",by.y="Common.name")
length(unique(accr5y.ag.area.long.per.country.id$Common.name)) # 161 countries (number of countries that entered the cluster analysis)
head(accr5y.ag.area.long.per.country.id)
windows(14,10)
par(mfrow=c(1,1))
accr5y.ag.area.long.per.country.id$group[accr5y.ag.area.long.per.country.id$group=="1"]<-"Rapidly expansionist"
accr5y.ag.area.long.per.country.id$group[accr5y.ag.area.long.per.country.id$group=="2"]<-"Expansionist"
accr5y.ag.area.long.per.country.id$group[accr5y.ag.area.long.per.country.id$group=="3"]<-"Consolidative"
library(ggplot2)
ag.area <- ggplot(accr5y.ag.area.long.per.country.id, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") + 
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Agricultural area") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="1,000 hectares (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours.leg) + theme(axis.text = element_text(size=5)) + 
  theme(axis.title = element_text(size = 7)) + theme(legend.position = "none")
# Plot range: from -8.73 to 7.54
min(accr5y.ag.area.long.per.country.id$value)
max(accr5y.ag.area.long.per.country.id$value)
#dev.print(tiff, "Structure_5yint_Agricultural area.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Synthetic fertilizer use
library(reshape)
accr5y.synt.fert.long<-melt(accr5y.synt.fert,id.vars="Common.name")
head(accr5y.synt.fert.long)
names(accr5y.synt.fert.long)[2]<-"interval"
accr5y.synt.fert.long<-merge(accr5y.synt.fert.long,cluster.groups,by.x="Common.name",by.y="Common.name")
unique(accr5y.synt.fert.long$Common.name) # 161 countries (number of countries that entered the cluster analysis)
head(accr5y.synt.fert.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
synt.fert <- ggplot(accr5y.synt.fert.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") + 
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Synthetic fertilizer") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="Tonnes (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours) + theme(axis.text = element_text(size=5)) + 
  theme(axis.title = element_text(size = 7)) + theme(legend.position = "none")
# Plot range: from -50.5 to 75.3
min(accr5y.synt.fert.long$value)
max(accr5y.synt.fert.long$value)
#dev.print(tiff, "Structure_5yint_Synthetic fertilizer use.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Agricultural employment
library(reshape)
accr5y.ag.empl.long<-melt(accr5y.ag.empl,id.vars="Common.name")
head(accr5y.ag.empl.long)
names(accr5y.ag.empl.long)[2]<-"interval"
accr5y.ag.empl.long<-merge(accr5y.ag.empl.long,cluster.groups,by.x="Common.name",by.y="Common.name")
unique(accr5y.ag.empl.long$Common.name) # 161 countries (number of countries that entered the cluster analysis)
head(accr5y.ag.empl.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
agric.empl <- ggplot(accr5y.ag.empl.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") + 
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Agricultural employment") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="Percentage of total employment (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours) + ylim(-15,15) +
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  theme(legend.position = "none") # Number of outliers with this scale = 2
# Plot range: from -12.9 to 13.7 (2 outliers = 29.9 and -27.1)
max(accr5y.ag.empl.long$value)
n<-length(accr5y.ag.empl.long$value)
sort(accr5y.ag.empl.long$value,partial=n-(0:4))[n-(0:4)] # top 5 highest values
min(accr5y.ag.empl.long$value)
sort(accr5y.ag.empl.long$value,partial=n-((n-1):(n-4)))[n-((n-1):(n-4))] # 5 lowest values
#dev.print(tiff, "Structure_5yint_Agricultural employment.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Gross Agricultural Output (GAO)
library(reshape)
accr5y.gao.long<-melt(accr5y.gao,id.vars="Common.name")
head(accr5y.gao.long)
names(accr5y.gao.long)[2]<-"interval"
accr5y.gao.long<-merge(accr5y.gao.long,cluster.groups,by.x="Common.name",by.y="Common.name")
unique(accr5y.gao.long$Common.name) # 161 countries (number of countries that entered the cluster analysis)
head(accr5y.gao.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
gao <- ggplot(accr5y.gao.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") + 
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Gross Agricultural Output") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="Constant International US$, 2004-2006 (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours) + theme(axis.text = element_text(size=5)) + 
  theme(axis.title = element_text(size = 7)) + theme(legend.position = "none")
# Plot range: from -15.5 to 19.9
min(accr5y.gao.long$value)
max(accr5y.gao.long$value)
#dev.print(tiff, "Structure_5yint_Gross Agricultural Output GAO.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Agricultural Total Factor Productivity (TFP)
library(reshape)
accr5y.atfp.long<-melt(accr5y.atfp,id.vars="Common.name")
head(accr5y.atfp.long)
names(accr5y.atfp.long)[2]<-"interval"
accr5y.atfp.long<-merge(accr5y.atfp.long,cluster.groups,by.x="Common.name",by.y="Common.name")
unique(accr5y.atfp.long$Common.name) # 161 countries (number of countries that entered the cluster analysis)
head(accr5y.atfp.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
tfp <- ggplot(accr5y.atfp.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +  
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Agricultural Total Factor Productivity") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="Index (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours) + theme(axis.text = element_text(size=5)) + 
  theme(axis.title = element_text(size = 7)) + theme(legend.position = "none")
# Plot range: from -18.3 to 15.4
max(accr5y.atfp.long$value)
n<-length(accr5y.atfp.long$value)
sort(accr5y.atfp.long$value,partial=n-(0:4))[n-(0:4)] # top 5 highest values
min(accr5y.atfp.long$value)
sort(accr5y.atfp.long$value,partial=n-((n-1):(n-4)))[n-((n-1):(n-4))] # 5 lowest values
#dev.print(tiff, "Structure_5yint_Agricultural Total Factor Productivity TFP.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Food imports
library(reshape)
accr5y.imports.long<-melt(accr5y.imports,id.vars="Common.name")
head(accr5y.imports.long)
names(accr5y.imports.long)[2]<-"interval"
accr5y.imports.long<-merge(accr5y.imports.long,cluster.groups,by.x="Common.name",by.y="Common.name")
length(unique(accr5y.imports.long$Common.name)) # 117 countries (number of countries that entered the cluster analysis)
accr5y.imports.long<-na.omit(accr5y.imports.long)
head(accr5y.imports.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
food.imports <- ggplot(accr5y.imports.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +  
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Food imports") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="International US$ (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours) + ylim(-40,40) +
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  theme(legend.position = "none") # Number of outliers with this scale = 1
# Plot range: from -38.2 to 37.8 (1 outlier = 106.4)
max(accr5y.imports.long$value)
n<-length(accr5y.imports.long$value)
sort(accr5y.imports.long$value,partial=n-(0:4))[n-(0:4)] # top 5 highest values
min(accr5y.imports.long$value)
sort(accr5y.imports.long$value,partial=n-((n-1):(n-4)))[n-((n-1):(n-4))] # 5 lowest values
#dev.print(tiff, "Structure_5yint_Food imports.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Food exports
library(reshape)
accr5y.exports.long<-melt(accr5y.exports,id.vars="Common.name")
head(accr5y.exports.long)
names(accr5y.exports.long)[2]<-"interval"
accr5y.exports.long<-merge(accr5y.exports.long,cluster.groups,by.x="Common.name",by.y="Common.name")
length(unique(accr5y.exports.long$Common.name)) # 116 countries (number of countries that entered the cluster analysis)
accr5y.exports.long<-na.omit(accr5y.exports.long)
head(accr5y.exports.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
food.exports <- ggplot(accr5y.exports.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Food exports") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="International US$ (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours) + ylim(-45,70) +
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  theme(legend.position = "none") # Number of outliers with this scale= 2
# Plot range: from -42.8 to 64.1 (2 outliers = 204.7 and 108.9) 
max(accr5y.exports.long$value)
n<-length(accr5y.exports.long$value)
sort(accr5y.exports.long$value,partial=n-(0:4))[n-(0:4)] # top 5 highest values
min(accr5y.exports.long$value)
sort(accr5y.exports.long$value,partial=n-((n-1):(n-4)))[n-((n-1):(n-4))] # 5 lowest values
#dev.print(tiff, "Structure_5yint_Food exports.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# PPI, Agriculture
library(reshape)
accr5y.ppi.long<-melt(accr5y.ppi,id.vars="Common.name")
head(accr5y.ppi.long)
names(accr5y.ppi.long)[2]<-"interval"
accr5y.ppi.long<-merge(accr5y.ppi.long,cluster.groups,by.x="Common.name",by.y="Common.name")
length(unique(accr5y.ppi.long$Common.name)) # 128 countries (number of countries that entered the cluster analysis)
accr5y.ppi.long<-na.omit(accr5y.ppi.long)
head(accr5y.ppi.long);nrow(accr5y.ppi.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
ppi <- ggplot(accr5y.ppi.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Producer Price Index, Agriculture") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="Index (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours) + ylim(-15,55) +
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  theme(legend.position = "none") # Number of outliers with this scale= 2
# Plot range: from -12.1 to 53.6 (2 outliers = 79.6 and 63.7) 
max(accr5y.ppi.long$value)
n<-length(accr5y.ppi.long$value)
sort(accr5y.ppi.long$value,partial=n-(0:4))[n-(0:4)] # top 5 highest values
min(accr5y.ppi.long$value)
sort(accr5y.ppi.long$value,partial=n-((n-1):(n-4)))[n-((n-1):(n-4))] # 5 lowest values
#dev.print(tiff, "Structure_5yint_PPI, Agriculture.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Save structure metrics plots:
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
windows(21,25)
dev.print(tiff, "Structure_5y.int.tiff", res=150, height=13.8, width=10.5, units="in")
red.bg<-rgb(255,0,0, maxColorValue = 255, alpha=30)
yellow.bg<-rgb(255,255,0, maxColorValue = 255, alpha=40)
grey.bg<-rgb(96,96,96, maxColorValue = 255, alpha=30)
struct <- ggarrange(ag.area, synt.fert, agric.empl, gao, tfp, food.imports, food.exports, ppi,
               ncol = 2, nrow = 4,
               legend = "bottom", common.legend = TRUE)
pdf("Structure.5y.int.pdf",height=13.3, width=10) 
#run pdf(), run all plots, run dev.off()
dev.off()

## Outcome metrics:

# Forest Area
library(reshape)
accr5y.for.area.long<-melt(accr5y.for.area,id.vars="Common.name")
head(accr5y.for.area.long)
names(accr5y.for.area.long)[2]<-"interval"
accr5y.for.area.long<-merge(accr5y.for.area.long,cluster.groups,by.x="Common.name",by.y="Common.name")
unique(accr5y.for.area.long$Common.name) # 161 countries (number of countries that entered the cluster analysis)
head(accr5y.for.area.long)
accr5y.for.area.long$group[accr5y.for.area.long$group=="1"]<-"Rapidly expansionist"
accr5y.for.area.long$group[accr5y.for.area.long$group=="2"]<-"Expansionist"
accr5y.for.area.long$group[accr5y.for.area.long$group=="3"]<-"Consolidative"
accr5y.for.area.long$group <- factor(accr5y.for.area.long$group,
                                     levels = c("Rapidly expansionist", "Expansionist", "Consolidative"))
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
forest.area <- ggplot(accr5y.for.area.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Forest area") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="1,000 hectares (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  scale_fill_manual(values=group.colours.leg) + theme(legend.position = "none")
# Plot range: from -8.1 to 6.9 
min(accr5y.for.area.long$value)
max(accr5y.for.area.long$value)
#dev.print(tiff, "Outcome_5yint_Forest area.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Red List Index
library(reshape)
accr5y.red.list.long<-melt(accr5y.red.list,id.vars="Common.name")
head(accr5y.red.list.long)
names(accr5y.red.list.long)[2]<-"interval"
accr5y.red.list.long<-merge(accr5y.red.list.long,cluster.groups,by.x="Common.name",by.y="Common.name")
length(unique(accr5y.red.list.long$Common.name)) # 145 countries (number of countries that entered the cluster analysis)
accr5y.red.list.long$value[accr5y.red.list.long$interval=="accr.95.00"]<-NA
accr5y.red.list.long<-na.omit(accr5y.red.list.long)
head(accr5y.red.list.long);nrow(accr5y.red.list.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
red.list.index <- ggplot(accr5y.red.list.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Red List Index") + labs(x="5 year intervals") + scale_x_discrete(labels= c("00-05","05-10","10-15")) + 
  labs(y="Index (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours) + ylim(-1.5,0.25) +
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  theme(legend.position = "none")
# Plot range: from -1.36 to 0.13 
min(accr5y.red.list.long$value)
max(accr5y.red.list.long$value)
#dev.print(tiff, "Outcome_5yint_Red List Index.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Undernourishment
library(reshape)
accr5y.undern.long<-melt(accr5y.undern,id.vars="Common.name")
head(accr5y.undern.long)
names(accr5y.undern.long)[2]<-"interval"
accr5y.undern.long<-merge(accr5y.undern.long,cluster.groups,by.x="Common.name",by.y="Common.name")
length(unique(accr5y.undern.long$Common.name)) # 144 countries (number of countries that entered the cluster analysis)
accr5y.undern.long$value[accr5y.undern.long$interval=="accr.95.00"]<-NA
accr5y.undern.long<-na.omit(accr5y.undern.long)
head(accr5y.undern.long);nrow(accr5y.undern.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
undern <- ggplot(accr5y.undern.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Undernourishment") + labs(x="5 year intervals") + scale_x_discrete(labels= c("00-05","05-10","10-15")) + 
  labs(y="Prevalence (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  scale_fill_manual(values=group.colours) + theme(legend.position = "none")
# Plot range: from -24.9 to 34.6 
min(accr5y.undern.long$value)
max(accr5y.undern.long$value)
#dev.print(tiff, "Outcome_5yint_Undernourishment.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Obesity
library(reshape)
accr5y.obesity.long<-melt(accr5y.obesity,id.vars="Common.name")
head(accr5y.obesity.long)
names(accr5y.obesity.long)[2]<-"interval"
accr5y.obesity.long<-merge(accr5y.obesity.long,cluster.groups,by.x="Common.name",by.y="Common.name")
unique(accr5y.obesity.long$Common.name) # 157 countries (number of countries that entered the cluster analysis)
head(accr5y.obesity.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
obesity <- ggplot(accr5y.obesity.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Obesity") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="Prevalence (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours) + ylim(-1,10) +
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  theme(legend.position = "none")
# Plot range: from 0.9 to 9.2 
min(accr5y.obesity.long$value)
max(accr5y.obesity.long$value)
#dev.print(tiff, "Outcome_5yint_Obesity.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Agricultural total GHG emissions
library(reshape)
accr5y.agric.ghg.long<-melt(accr5y.agric.ghg,id.vars="Common.name")
head(accr5y.agric.ghg.long)
names(accr5y.agric.ghg.long)[2]<-"interval"
accr5y.agric.ghg.long<-merge(accr5y.agric.ghg.long,cluster.groups,by.x="Common.name",by.y="Common.name")
unique(accr5y.agric.ghg.long$Common.name) # 158 countries (number of countries that entered the cluster analysis)
accr5y.agric.ghg.long<-na.omit(accr5y.agric.ghg.long)
head(accr5y.agric.ghg.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
agric.ghge <- ggplot(accr5y.agric.ghg.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Agricultural GHG emissions") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="Megatons of CO2eq (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  scale_fill_manual(values=group.colours) + ylim(-22,25) +
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  theme(legend.position = "none") # Number of outliers with this scale= 3
# Plot range: from -20.4 to 24 (3 outliers = 55.5, 43.4, and 37.2) 
max(accr5y.agric.ghg.long$value)
n<-length(accr5y.agric.ghg.long$value)
sort(accr5y.agric.ghg.long$value,partial=n-(0:4))[n-(0:4)] # top 5 highest values
min(accr5y.agric.ghg.long$value)
sort(accr5y.agric.ghg.long$value,partial=n-((n-1):(n-4)))[n-((n-1):(n-4))] # 5 lowest values
#dev.print(tiff, "Outcome_5yint_Agricultural GHG emissions.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Land-Use Change and Forestry total GHG emissions
library(reshape)
accr5y.land.for.ghg.long<-melt(accr5y.land.for.ghg,id.vars="Common.name")
head(accr5y.land.for.ghg.long)
names(accr5y.land.for.ghg.long)[2]<-"interval"
accr5y.land.for.ghg.long<-merge(accr5y.land.for.ghg.long,cluster.groups,by.x="Common.name",by.y="Common.name")
length(unique(accr5y.land.for.ghg.long$Common.name)) # 158 countries (number of countries that entered the cluster analysis)
accr5y.land.for.ghg.long<-na.omit(accr5y.land.for.ghg.long)
head(accr5y.land.for.ghg.long);nrow(accr5y.land.for.ghg.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
land.forest.ghge <- ggplot(accr5y.land.for.ghg.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Land-Use Change and Forestry GHG emissions") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="Megatons of CO2eq (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") +
  scale_fill_manual(values=group.colours) + ylim(-71,100) +
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  theme(legend.position = "none") # Number of outliers with this scale= 5
# Plot range: from -70 to 100 (5 outilers:3007.9, 175.9, 152.3, -98.6, -95.9) 
max(accr5y.land.for.ghg.long$value)
n<-length(accr5y.land.for.ghg.long$value)
sort(accr5y.land.for.ghg.long$value,partial=n-(0:4))[n-(0:4)] # top 5 highest values
min(accr5y.land.for.ghg.long$value)
sort(accr5y.land.for.ghg.long$value,partial=n-((n-1):(n-4)))[n-((n-1):(n-4))] # 5 lowest values
#dev.print(tiff, "Outcome_5yint_Land-use change and forestry GHG emissions.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# AFOLU total GHG emissions
library(reshape)
accr5y.afolu.ghg.long<-melt(accr5y.afolu.ghg,id.vars="Common.name")
head(accr5y.afolu.ghg.long)
names(accr5y.afolu.ghg.long)[2]<-"interval"
accr5y.afolu.ghg.long<-merge(accr5y.afolu.ghg.long,cluster.groups,by.x="Common.name",by.y="Common.name")
length(unique(accr5y.afolu.ghg.long$Common.name)) # 158 countries (number of countries that entered the cluster analysis)
accr5y.afolu.ghg.long<-na.omit(accr5y.afolu.ghg.long)
head(accr5y.afolu.ghg.long);nrow(accr5y.afolu.ghg.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
afolu <- ggplot(accr5y.afolu.ghg.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("AFOLU GHG emissions") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="Megatons of CO2eq (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") +
  scale_fill_manual(values=group.colours) + ylim(-60,100) +
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  theme(legend.position = "none") # Number of outliers with this scale = 2
# Plot range: from -55.2 to 97.4 (2 outliers: 225.7 and 113) 
max(accr5y.afolu.ghg.long$value)
n<-length(accr5y.afolu.ghg.long$value)
sort(accr5y.afolu.ghg.long$value,partial=n-(0:4))[n-(0:4)] # top 5 highest values
min(accr5y.afolu.ghg.long$value)
sort(accr5y.afolu.ghg.long$value,partial=n-((n-1):(n-4)))[n-((n-1):(n-4))] # 5 lowest values
#dev.print(tiff, "Outcome_5yint_AFOLU GHG emissions.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Ecological Foodprint of Consumption (global hectares, gha)
library(reshape)
accr5.nfa.long<-melt(accr5.nfa,id.vars="Common.name")
head(accr5.nfa.long)
names(accr5.nfa.long)[2]<-"interval"
accr5.nfa.long<-merge(accr5.nfa.long,cluster.groups,by.x="Common.name",by.y="Common.name")
length(unique(accr5.nfa.long$Common.name)) # 152 countries (number of countries that entered the cluster analysis)
accr5.nfa.long<-na.omit(accr5.nfa.long)
head(accr5.nfa.long);nrow(accr5.nfa.long)
windows(14,10)
par(mfrow=c(1,1))
library(ggplot2)
efc <- ggplot(accr5.nfa.long, aes(x=factor(interval),y=value)) + 
  geom_boxplot(notch = TRUE, aes(fill=as.factor(group)),outlier.size = 0.5) + labs(fill = "Group",shape = "Group") +
  theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Ecological footprint, Consumption") + labs(x="5 year intervals") + scale_x_discrete(labels= five.year.intervals) + 
  labs(y="Global hectares (% annual change)") + geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme(axis.text = element_text(size=5)) + theme(axis.title = element_text(size = 7)) +
  scale_fill_manual(values=group.colours) + theme(legend.position = "none")
# Plot range: from -18.3 to 34.6 
min(accr5.nfa.long$value)
max(accr5.nfa.long$value)
#dev.print(tiff, "Outcome_5yint_Ecological Footprint of Consumption.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image

# Save outcome metrics plots:
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
windows(21,25)
dev.print(tiff, "Outcome_5y.int.tiff", res=150, height=13.8, width=10.5, units="in")
light.green.bg<-rgb(102,255,102, maxColorValue = 255, alpha=50)
blue.bg<-rgb(0,114,178, maxColorValue = 255, alpha=30)
#green.bg<-rgb(0,158,115, maxColorValue = 255, alpha=35)
orange.bg<-rgb(255,128,0, maxColorValue = 255, alpha=30)
outcome <- ggarrange(forest.area, red.list.index, undern, obesity, agric.ghge, land.forest.ghge, afolu, efc,
                    ncol = 2, nrow = 4,
                    legend = "bottom", common.legend = TRUE)
pdf("Outcome.5y.int.pdf",height=13.3, width=10) 
#run pdf(), run all plots, run dev.off()
dev.off()
