# Tom Oliver and Andre Dornelles, July and August 2019

# 1_Dornelles.et.al_Data.standardisation:

# This script prepares raw data files on structural, outcome, correlational metrics in food systems by collating them into a standardised hierachical format (i.e. individual variables, derived variables, and aggregate indicators) by 'country', 'year', and 'value' for the temporal trend analysis.

## This script has three parts:
# 1. Structure metrics;
# 2. Outcome metrics;
# 3. Other metrics.

## Instructions (repeat it for each metric individually):
# First: upload the country look up table (country look up table directory) 
# Second: upload the raw metric file (raw data directory);
# Third: run the code for that metric;
# Fourth: generate the collated csv. file in the proper directory (collated data directory);

# Raw data directory
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw data")
# Collated data directory:
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data\\Collated data files")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data\\Collated data files")
# Country look up table - necessary for merging some derived variables from different data sources:
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
country.look.up<-read.csv("Country name look up table_updated.csv",header=T)

## PART 1. STRUCTURE METRICS

## 1.1 Individual variables

## 1.1.1 Agricultural area (1,000 ha)
fsdata<-read.csv("Agricultural area FAOSTAT_data_5-9-2018.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
fsdata<-fsdata[order(fsdata$country),] #arrange by country alphabetically before generating the csv file
write.csv(fsdata,"Collated_ind_Agricultural area.csv",row.names=F)

## 1.1.2 Gross Agricultural Output (Constant, 2010=100 Int.US$)
fsdata<-read.csv("Gross Agricultural Output (GAO) FAOSTAT_data_5-9-2018.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_ind_Gross Agricultural Output.csv",row.names=F)

## 1.1.3 Gross Agricultural Value (Current US$)
fsdata<-read.csv("FAOSTAT_Gross Agricultural Value (GAV).csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
fsdata$value<-fsdata$value*1000000 # Converted to US$ for standardized comparison (it was expressed in millions of US$) 
write.csv(fsdata,"Collated_ind_Gross Agricultural Value.csv",row.names=F)

## 1.1.4 Fertilizer consumption (tones)
fsdata<-read.csv("Fertilizer consumption, total - FAOSTAT_data_5-9-2018.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
fsdata<-fsdata[!fsdata$year==2002,]  # exclude 2002 as this is repeated in contemporary time series immediately below
fsdata.t2<-read.csv("Fertilizer consumption total from 2002 to present.csv",header=T) # Import second period
names(fsdata.t2)
fsdata.t2<-fsdata.t2[,-c(2,4)]
unique(fsdata.t2$Series.Name)
library(reshape)
fsdata.t2<-melt(fsdata.t2,id.vars=c("Country.Name","Series.Name"))
head(fsdata.t2)
names(fsdata.t2)<-c("country","series","year","value")
fsdata.t2$year<-substring(fsdata.t2$year,2,5)
fsdata.t2$value[fsdata.t2$value==".."]<-NA
fsdata.t2$value<-as.numeric(as.vector(fsdata.t2$value))
fsdata.t2.arable.area<-fsdata.t2[fsdata.t2$series=="Arable land (hectares)",]    # create a separate table of total area arable land
fsdata.t2.fert.cons<-fsdata.t2[fsdata.t2$series=="Fertilizer consumption (kilograms per hectare of arable land)",]  # create a separate table of fertiliser
nrow(fsdata.t2.arable.area); nrow(fsdata.t2.fert.cons)
fsdata.t2.arable.area<-fsdata.t2.arable.area[,-2]  # remove the column with series name
names(fsdata.t2.arable.area)[3]<-"arable.area"
head(fsdata.t2.arable.area)
fsdata.t2.fert.cons<-fsdata.t2.fert.cons[,-2] # remove the column with series name
names(fsdata.t2.fert.cons)[3]<-"fert.cons"
head(fsdata.t2.fert.cons)
fsdata.t2<-merge(fsdata.t2.arable.area,fsdata.t2.fert.cons)    # join the two tables
nrow(fsdata.t2) ;head(fsdata.t2) # check the row numbers 
fsdata.t2$value<-fsdata.t2$arable.area*fsdata.t2$fert.cons/1000   # create a measure of total fert in tonnes
fsdata.t2<-fsdata.t2[,c(1,2,5)]  # remove arable areas and fert consumption per hectare
head(fsdata.t2)
fsdata<-rbind(fsdata,fsdata.t2)  # use rbind to append (stack) two tables together
unique(fsdata$year)
head(fsdata)
fsdata<-fsdata[order(fsdata$country),] #arrange by country alphabetically after merging
write.csv(fsdata,"Collated_ind_Fertilizer consumption.csv",row.names=F)

## 1.1.4.X FAO Synthetic Fertilizer consumption (tonnes)
fsdata<-read.csv("FAOSTAT_Nitrogen.fertilizers.agric.use.csv",header=T)
head(fsdata)
fsdata<-fsdata[,c(4,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
fsdata$value<-fsdata$value/1000 # Converted to tons for standardized comparison (it was expressed in kgs)
write.csv(fsdata,"Collated_ind_FAO Nitrogen Fertilizer consumption.csv",row.names=F)

## 1.1.4.Y ATFP Synthetic Fertilizer consumption (tonnes)
fsdata<-read.csv("ATFP_Fertilizers.csv",header=T)
head(fsdata)
library(reshape)
fsdata<-melt(fsdata,id.vars="Country")
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
fsdata$year<-substring(fsdata$year,2,5)
fsdata<-fsdata[order(fsdata$country),] #arrange by country alphabetically before generating the csv file
write.csv(fsdata,"Collated_ind_ATFP Synthetic Fertilizer consumption.csv",row.names=F)

## 1.1.5  Pesticide use (tonnes)
fsdata<-read.csv("Pesticide use, total - FAOSTAT_data_5-9-2018.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_ind_Pesticide use.csv",row.names=F)

## 1.1.6 Water withdrawal (10^9 m^3)
fsdata<-read.csv("AQUASTAT_Agricultural water withdrawal_for.R.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(1,5,6)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_ind_Water withdrawal.csv",row.names=F)

## 1.1.7 Food import (US$)
# code found at ## 1.2.7 Food import-export ratio

## 1.1.8 Food export (US$)
# code found at ## 1.2.7 Food import-export ratio

## 1.1.9 Governmental agricultural expenditure (US$) 
fsdata<-read.csv("FAOSTAT_Central.governm.expend_value.and.%.csv",header=T)
names(fsdata)
head(fsdata)
fsdata<-fsdata[,c(4,6,10,12)]
names(fsdata)<-c("country","series","year","value")
unique(fsdata$series)
fsdata.abs.govern.agric.expenditure<-fsdata[fsdata$series=="Value US$",]    # create a separate table of absolute government expenditure in agriculutre
fsdata.percent.govern.agric.expenditure<-fsdata[fsdata$series=="Share of Total Outlays",]  # create a separate table of percent of GDP government expenditure in agriculture 
nrow(fsdata.abs.govern.agric.expenditure); nrow(fsdata.percent.govern.agric.expenditure)
fsdata.abs.govern.agric.expenditure<-fsdata.abs.govern.agric.expenditure[,-2]  # remove the column with series name
head(fsdata.abs.govern.agric.expenditure)
fsdata.abs.govern.agric.expenditure$value<-fsdata.abs.govern.agric.expenditure$value*1000000 # Converted to US$ for standardized comparison (it was expressed in millions of US$)
write.csv(fsdata.abs.govern.agric.expenditure,"Collated_ind_Government expenditure, asbolute.csv",row.names=F)
fsdata.percent.govern.agric.expenditure<-fsdata.percent.govern.agric.expenditure[,-2] # remove the column with series name
head(fsdata.percent.govern.agric.expenditure)
write.csv(fsdata.percent.govern.agric.expenditure,"Collated_der_Government expenditure, percent of GDP.csv",row.names=F)

## 1.1.10 Agricultural employment (individuals)

# 1.1.10.1 FAO Agricultural employment
fsdata<-read.csv("FAOSTAT_Agric.employement and value.added.per.worker.csv",header=T)
names(fsdata)
head(fsdata)
fsdata<-fsdata[,c(4,6,10,12)]
names(fsdata)<-c("country","series","year","value")
unique(fsdata$series)
fsdata.agri.employment<-fsdata[fsdata$series=="Employment in agriculture",]    # create a separate table of agricultural employment
fsdata.value.added.per.worker<-fsdata[fsdata$series=="Agriculture value added per worker (constant 2005 US$)",]  # create a separate table of agriculture value added per worker
nrow(fsdata.agri.employment); nrow(fsdata.value.added.per.worker)
fsdata.agri.employment<-fsdata.agri.employment[,-2]  # remove the column with series name
head(fsdata.agri.employment)
fsdata.agri.employment$value<-fsdata.agri.employment$value*1000 # Converted to individuals for standardized comparison (it was expressed in 1,000 individuals)
write.csv(fsdata.agri.employment,"Collated_ind_FAO Agricultural employment.csv",row.names=F)
fsdata.value.added.per.worker<-fsdata.value.added.per.worker[,-2] # remove the column with series name
head(fsdata.value.added.per.worker)
write.csv(fsdata.value.added.per.worker,"Collated_agr_Agricultural Value Added, per worker.csv",row.names=F)

# 1.1.10.2 ATFP Agricultural employment
# original format: (1000 persons economically active in agriculture, +15 yrs, male+female)
fsdata<-read.csv("ATFP_Agric.Labour.csv",header=T)
head(fsdata)
library(reshape)
fsdata<-melt(fsdata,id.vars="Country")
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
fsdata$year<-substring(fsdata$year,2,5)
fsdata<-fsdata[order(fsdata$country),] #arrange by country alphabetically before generating the csv file
fsdata<-na.omit(fsdata) # remove NAs
fsdata$value<-fsdata$value*1000 # Converted to individuals for standardized comparison (it was expressed in 1,000 individuals)
write.csv(fsdata,"Collated_ind_ATFP Agricultural employment.csv",row.names=F)

## 1.2 Derived variables

## 1.2.1 Agricultural area (% of total land area)
agric.area<-read.csv("Collated_ind_Agricultural area.csv",header=T) # setwd to the folder "Collated data files" 
head(agric.area)
names(agric.area)[3]<-"agric.area"
total.land<-read.csv("FAOSTAT_Land_Agric_Forest_Total.csv",header=T)
head(total.land)
total.land<-total.land[,c(4,8,10,12)]
names(total.land)<-c("country","series","year","value")
unique(total.land$series)
total.land<-total.land[total.land$series=="Land area",]    # create a separate table extracting total land only
total.land<-total.land[,-2]
head(total.land)
write.csv(fsdata,"Collated_ind_Total land area.csv",row.names=F)
names(total.land)[3]<-"total.land"
fsdata<-merge(agric.area,total.land, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
head(fsdata)
nrow(fsdata); nrow(agric.area); nrow(total.land)
total.land$country[!total.land$country%in%fsdata$country] #which rows did we lose (if any)?
fsdata$value<-fsdata$agric.area*100/fsdata$total.land # calculating % of total land area
fsdata<-fsdata[,c(1,2,5)]
fsdata<-na.omit(fsdata) # remove NAs
head(fsdata);nrow(fsdata)
write.csv(fsdata,"Collated_der_Agricultural area, percentage.csv",row.names=F)

## 1.2.2 Gross Agricultural Output (Constant 2010=100 Int. uS$ per 1,000 ha of agricultural land)
gao<-read.csv("Collated_ind_Gross Agricultural Output.csv",header=T)
head(gao)
names(gao)[3]<-"GAO"
agric.area<-read.csv("Collated_ind_Agricultural area.csv",header=T)
head(agric.area)
names(agric.area)[3]<-"agric.area"
names(country.look.up) #Use country look up table before merging
country.look.up.for.gao<-country.look.up[,c(1,8)]
gao<-merge(gao,country.look.up.for.gao,by.x="country",by.y="Agric.output")
head(gao); nrow(gao) # 214 countries
gao<-gao[,c(4,2:3)]
names(gao)[1]<-"country"
country.look.up.for.agric.area<-country.look.up[,c(1,6)]
agric.area<-merge(agric.area,country.look.up.for.agric.area,by.x="country",by.y="Agric.area")
head(agric.area); nrow(agric.area) # 228 countries
agric.area<-agric.area[,c(4,2:3)]
names(agric.area)[1]<-"country" #Proceed with merging next
fsdata<-merge(gao,agric.area, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
head(fsdata); nrow(fsdata); nrow(agric.area); nrow(gao) # 
unique(agric.area$country[!agric.area$country%in%gao$country]) #which countries did we lose (if any)?
unique(gao$country[!gao$country%in%agric.area$country])
fsdata$value<-fsdata$GAO/fsdata$agric.area
head(fsdata)
fsdata<-fsdata[,c(1,2,5)]
fsdata<-na.omit(fsdata) # remove NAs
head(fsdata);nrow(fsdata) 
length(unique(fsdata$country)) # 213 countries
write.csv(fsdata,"Collated_der_GAO per ha.csv",row.names=F)

## 1.2.3 Gross Agricultural Value (uS$ per ha of agricultural land)
gav<-read.csv("Collated_ind_Gross Agricultural Value.csv",header=T)
head(gav)
names(gav)[3]<-"gav"
agric.area<-read.csv("Collated_ind_Agricultural area.csv",header=T)
head(agric.area)
names(agric.area)[3]<-"agric.area"
names(country.look.up) #Use country look up table before merging
country.look.up.for.gav<-country.look.up[,c(1,9)]
gav<-merge(gav,country.look.up.for.gav,by.x="country",by.y="Agric.value")
head(gav); nrow(gav)
gav<-gav[,c(4,2:3)]
names(gav)[1]<-"country"
country.look.up.for.agric.area<-country.look.up[,c(1,6)]
agric.area<-merge(agric.area,country.look.up.for.agric.area,by.x="country",by.y="Agric.area")
head(agric.area); nrow(agric.area)
agric.area<-agric.area[,c(4,2:3)]
names(agric.area)[1]<-"country" #Proceed with merging next
fsdata<-merge(gav,agric.area, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
head(fsdata)
nrow(fsdata); nrow(agric.area); nrow(gav)
unique(agric.area$country[!agric.area$country%in%fsdata$country]) #which countries did we lose (if any)?
fsdata$agric.area<-fsdata$agric.area*1000 # Converting to ha (Agricultural area individual metric is expressed as 1,000 ha)
fsdata$value<-fsdata$gav/fsdata$agric.area
head(fsdata)
fsdata<-fsdata[,c(1,2,5)]
fsdata<-na.omit(fsdata) # remove NAs
head(fsdata); nrow(fsdata)
write.csv(fsdata,"Collated_der_GAV per ha.csv",row.names=F)

## 1.2.4 Fertilizer use (kg per ha of arable land)
# 1.2.4.1 Total fertlizers:
tot.fert.use<-read.csv("Collated_ind_Fertilizer consumption.csv",header=T)
head(tot.fert.use)
names(tot.fert.use)[3]<-"tot.fert.use"
arable.land<-read.csv("FAOSTAT_Arable.land.csv",header=T)
head(arable.land)
arable.land<-arable.land[,c(4,10,12)]
names(arable.land)<-c("country","year","value")
write.csv(arable.land,"Collated_ind_Arable land.csv",row.names=F)
names(arable.land)[3]<-"arable.land"
names(country.look.up) #Use country look up table before merging
country.look.up.for.tot.fert.use<-country.look.up[,c(1,2)]
tot.fert.use<-merge(tot.fert.use,country.look.up.for.tot.fert.use,by.x="country",by.y="Fertiliser.after2002")
head(tot.fert.use) ; nrow(tot.fert.use)
tot.fert.use<-tot.fert.use[,c(4,2:3)]
names(tot.fert.use)[1]<-"country"
country.look.up.for.arable.land<-country.look.up[,c(1,7)]
arable.land<-merge(arable.land,country.look.up.for.arable.land,by.x="country",by.y="Arable.land")
head(arable.land); nrow(arable.land)
arable.land<-arable.land[,c(4,2:3)]
names(arable.land)[1]<-"country" #Proceed with merging
fsdata<-merge(tot.fert.use,arable.land, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
head(fsdata)
nrow(fsdata); nrow(tot.fert.use); nrow(arable.land)
unique(tot.fert.use$country[!tot.fert.use$country%in%fsdata$country]) #which countries did we lose (if any)?
fsdata$value<-fsdata$tot.fert.use/fsdata$arable.land # Fertilizer user unity was tonnes; Arable land unity was 1,000 ha. Unity expressed here is kg per ha.
head(fsdata); nrow(fsdata)
fsdata<-fsdata[,c(1,2,5)]
fsdata<-na.omit(fsdata) # remove NAs
head(fsdata); nrow(fsdata)
write.csv(fsdata,"Collated_der_Total fertilizer use per ha of arable land.csv",row.names=F)

# 1.2.4.2 ATFP Synthetic fertlizers: 
synt.fert.use<-read.csv("Collated_ind_ATFP Synthetic Fertilizer consumption.csv",header=T)
head(synt.fert.use)
names(synt.fert.use)[3]<-"synt.fert.use"
arable.land<-read.csv("Collated_ind_Arable land.csv",header=T)
head(arable.land)
names(arable.land)[3]<-"arable.land"
names(country.look.up) #Use country look up table before merging (different datasources: FAO and ATFP)
country.look.up.for.synt.fert.use<-country.look.up[,c(1,4)]
synt.fert.use<-merge(synt.fert.use,country.look.up.for.synt.fert.use,by.x="country",by.y="Synthetic.fertilizers")
head(synt.fert.use) ; nrow(synt.fert.use) # 170 countries
synt.fert.use<-synt.fert.use[,c(4,2:3)]
names(synt.fert.use)[1]<-"country"
country.look.up.for.arable.land<-country.look.up[,c(1,7)]
arable.land<-merge(arable.land,country.look.up.for.arable.land,by.x="country",by.y="Arable.land")
head(arable.land); nrow(arable.land) # 200 countries
arable.land<-arable.land[,c(4,2:3)]
names(arable.land)[1]<-"country" #Proceed with merging
fsdata<-merge(arable.land,synt.fert.use, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
head(fsdata); nrow(fsdata); nrow(synt.fert.use); nrow(arable.land)
unique(arable.land$country[!arable.land$country%in%synt.fert.use$country]) #which countries did we lose (if any)?
unique(synt.fert.use$country[!synt.fert.use$country%in%arable.land$country])
fsdata$value<-fsdata$synt.fert.use/fsdata$arable.land
head(fsdata); nrow(fsdata)
fsdata<-fsdata[,c(1,2,5)]
fsdata<-na.omit(fsdata) # remove NAs
head(fsdata); nrow(fsdata) # 153 countries
write.csv(fsdata,"Collated_der_ATFP Synthetic fertilizer use per ha of arable land.csv",row.names=F)

# 1.2.4.3 FAO Synthetic fertlizers:
fao.synt.fert.use<-read.csv("Collated_ind_FAO Nitrogen Fertilizer consumption.csv",header=T)
head(fao.synt.fert.use)
names(fao.synt.fert.use)[3]<-"fao.synt.fert.use"
arable.land<-read.csv("Collated_ind_Arable land.csv",header=T)
head(arable.land)
names(arable.land)[3]<-"arable.land"
names(country.look.up) #Use country look up table before merging
country.look.up.for.fao.synt.fert.use<-country.look.up[,c(1,31)]
fao.synt.fert.use<-merge(fao.synt.fert.use,country.look.up.for.fao.synt.fert.use,by.x="country",by.y="FAO.synt.fert")
head(fao.synt.fert.use) ; nrow(fao.synt.fert.use)
fao.synt.fert.use<-fao.synt.fert.use[,c(4,2:3)]
names(fao.synt.fert.use)[1]<-"country"
country.look.up.for.arable.land<-country.look.up[,c(1,7)]
arable.land<-merge(arable.land,country.look.up.for.arable.land,by.x="country",by.y="Arable.land")
head(arable.land); nrow(arable.land)
arable.land<-arable.land[,c(4,2:3)]
names(arable.land)[1]<-"country" #Proceed with merging
fsdata<-merge(arable.land,fao.synt.fert.use, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
head(fsdata)
nrow(fsdata); nrow(fao.synt.fert.use); nrow(arable.land)
unique(fao.synt.fert.use$country[!fao.synt.fert.use$country%in%fsdata$country]) #which countries did we lose (if any)?
fsdata$value<-fsdata$fao.synt.fert.use/fsdata$arable.land
head(fsdata); nrow(fsdata)
fsdata<-fsdata[,c(1,2,5)]
fsdata<-na.omit(fsdata) # remove NAs
head(fsdata); nrow(fsdata)
write.csv(fsdata,"Collated_der_FAO Synthetic fertilizer use per ha of arable land.csv",row.names=F)

## 1.2.5 Pesticide use (kg per ha of arable land)
pest.use<-read.csv("Collated_ind_Pesticide use.csv",header=T)
head(pest.use)
names(pest.use)[3]<-"pest.use"
arable.land<-read.csv("Collated_ind_Arable land.csv",header=T)
head(arable.land)
names(arable.land)[3]<-"arable.land"
names(country.look.up) #Use country look up table before merging (different datasources: FAO and ATFP)
country.look.up.for.pest.use<-country.look.up[,c(1,10)]
pest.use<-merge(pest.use,country.look.up.for.pest.use,by.x="country",by.y="Pesticide")
head(pest.use) ; nrow(pest.use)
pest.use<-pest.use[,c(4,2:3)]
names(pest.use)[1]<-"country"
country.look.up.for.arable.land<-country.look.up[,c(1,7)]
arable.land<-merge(arable.land,country.look.up.for.arable.land,by.x="country",by.y="Arable.land")
head(arable.land); nrow(arable.land)
arable.land<-arable.land[,c(4,2:3)]
names(arable.land)[1]<-"country" #Proceed with merging
fsdata<-merge(pest.use,arable.land, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
head(fsdata)
nrow(fsdata); nrow(pest.use); nrow(arable.land)
unique(arable.land$country[!arable.land$country%in%fsdata$country]) #which countries did we lose (if any)?
fsdata$value<-fsdata$pest.use/fsdata$arable.land
head(fsdata); nrow(fsdata)
fsdata<-fsdata[,c(1,2,5)]
fsdata<-na.omit(fsdata) # remove NAs
head(fsdata); nrow(fsdata)
write.csv(fsdata,"Collated_der_Pesticide use per ha of arable land.csv",row.names=F)

## 1.2.6 Water withdrawal (% of total water withdrawal and % of renewable sources)
## 1.2.6.1 Water withdrawal, percent of total
fsdata<-read.csv("AQUASTAT_Agric water withdrawal as % of total_for.R.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(1,5,6)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_der_Water withdrawal, percent of total.csv",row.names=F)
## 1.2.6.2 Water withdrawal, percent of renewable sources
fsdata<-read.csv("AQUASTAT_Agric water withdrawal as % of renewable_for.R.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(1,5,6)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_der_Water withdrawal, percent of renewable.csv",row.names=F)

## 1.2.7 Food import-export ratio (ratio)
fsdata<-read.csv("Food import and export current US$_edited to absolute values only.csv",header=T)
nrow(fsdata) ; head(fsdata)
fsdata<-fsdata[fsdata$Import.export=="Food import, current US$"|fsdata$Import.export=="Food export, current US$",] # extracting only imports and exports
fsdata[3,] # checking row 3
names(fsdata)
library(reshape)
fsdata<-melt(fsdata,id.vars=c("Country","Import.export"))
names(fsdata)<-c("country","series","year","value")
fsdata$year<-substring(fsdata$year,2,5) # extracting only the year (e.g. 1960) from the vector (e.g. X1960..YR1960)
fsdata$value[fsdata$value==".."]<-NA
fsdata[22999,] # checking row 22999
fsdata$value<-as.numeric(as.vector(fsdata$value))
unique(fsdata$series)
fsdata.imports<-fsdata[fsdata$series=="Food import, current US$",]  # create import table (absolute US$)
fsdata.exports<-fsdata[fsdata$series=="Food export, current US$",]  # create export table (absolute US$)
nrow(fsdata.imports); nrow(fsdata.exports)
head(fsdata.imports); head(fsdata.exports)
fsdata.imports<-fsdata.imports[,-2] # remove colum 2 (series)
names(fsdata.imports)[3]<-"import.value" #giving the column 3 (value) the name "import.value"
# names(fsdata.imports)[3]<-"value" #standart name for the comlum before saving the csv for imports
head(fsdata.imports)
nrow(fsdata.imports)
# fsdata.imports<-fsdata.imports[order(fsdata.imports$country),] # ordering alphabetically before saving the csv file
fsdata.exports<-fsdata.exports[,-2]
names(fsdata.exports)[3]<-"export.value"
# names(fsdata.exports)[3]<-"value" #standart name for the comlum before saving the csv for exports
head(fsdata.exports)
nrow(fsdata.exports)
# fsdata.exports<-fsdata.exports[order(fsdata.exports$country),] # ordering alphabetically before saving the csv file
fsdata<-merge(fsdata.imports,fsdata.exports)  # join the two tables
nrow(fsdata) ; head(fsdata)
fsdata$value<-fsdata$import.value/fsdata$export.value  # create a new column of ratio
# names(fsdata)[5]<-"imp.exp.ratio.value"
names(fsdata)[5]<-"value"
head(fsdata)
fsdata.ratio<-fsdata[,c(1,2,5)]
write.csv(fsdata.ratio,"Collated_der_Food import-export ratio.absolute.csv",row.names=F)  # save ratio
write.csv(fsdata.imports,"Collated_ind_Food imports absolute.csv",row.names=F)  # save imports
write.csv(fsdata.exports,"Collated_ind_Food exports absolute.csv",row.names=F)  # save exports

## 1.2.8 Governmental agricultural expenditure (% of total government expenditure)
# Code found at ## 1.1.9 Governmental agricultural expenditure (millions US$)

## 1.2.9 Agricultural employment (% of population)

#1.2.9.1 FAO Agricultural employment
agric.employ<-read.csv("Collated_ind_agri.employment.csv",header=T)
head(agric.employ)
names(agric.employ)[3]<-"agric.employ"
total.pop<-read.csv("WDI_Population_total.and.density.csv",header=T) #File saved in Raw Data, not Collated Data files. Change working directory to read csv
head(total.pop); names(total.pop)
total.pop<-total.pop[total.pop$Series.Name=="Population, total",] # extracting only total population
total.pop[3,] # checking row 3
names(total.pop)
total.pop<-total.pop[,-c(2,4)] #remove columns 2 and 4
library(reshape)
total.pop<-melt(total.pop,id.vars=c("Country.Name","Series.Name"))
names(total.pop)<-c("country","series","year","value")
head(total.pop)
total.pop$year<-substring(total.pop$year,2,5) # extracting only the year (e.g. 1960) from the vector (e.g. X1960..YR1960)
total.pop$value[total.pop$value==".."]<-NA
total.pop[2999,] # checking row 2999
total.pop$value<-as.numeric(as.vector(total.pop$value))
total.pop<-total.pop[,-2] # remove colum 2 (series)
total.pop<-total.pop[order(total.pop$country),] # order alphabetically
write.csv(total.pop,"Collated_ind_Total population.csv",row.names=F)
names(total.pop)[3]<-"total.pop" #giving the column 3 (value) the name "total.pop"
names(country.look.up) ##Use country look up table before merging (different datasources: FAO and WDI)
country.look.up.for.agric.employ<-country.look.up[,c(1,22)]
agric.employ<-merge(agric.employ,country.look.up.for.agric.employ,by.x="country",by.y="Agric.employment")
head(agric.employ) ; nrow(agric.employ)
agric.employ<-agric.employ[,c(4,2:3)]
names(agric.employ)[1]<-"country"
country.look.up.for.total.pop<-country.look.up[,c(1,29)]
total.pop<-merge(total.pop,country.look.up.for.total.pop,by.x="country",by.y="Population")
head(total.pop); nrow(total.pop)
total.pop<-total.pop[,c(4,2:3)]
names(total.pop)[1]<-"country"
total.pop<-total.pop[order(total.pop$year),] # order by year
total.pop<-total.pop[order(total.pop$country),] # order alphabetically 
fsdata<-merge(agric.employ,total.pop, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE) #Proceed with merging
head(fsdata)
nrow(fsdata); nrow(agric.employ); nrow(total.pop)
unique(total.pop$country[!total.pop$country%in%fsdata$country]) #which countries did we lose (if any)?
fsdata$value<-fsdata$agric.employ*100/fsdata$total.pop
head(fsdata); nrow(fsdata)
fsdata<-fsdata[,c(1,2,5)]
fsdata<-na.omit(fsdata) # remove NAs
head(fsdata); nrow(fsdata)
write.csv(fsdata,"Collated_der_FAO Agricultural employment, percent of population.csv",row.names=F)

#1.2.9.2 ATFP Agricultural employment
agric.employ<-read.csv("Collated_ind_ATFP Agricultural employment.csv",header=T)
head(agric.employ)
names(agric.employ)[3]<-"agric.employ"
total.pop<-read.csv("Collated_ind_Total population.csv",header=T)
head(total.pop)
names(total.pop)[3]<-"total.pop"
names(country.look.up) ##Use country look up table before merging (different datasources: FAO and WDI)
country.look.up.for.agric.employ<-country.look.up[,c(1,32)]
unique(agric.employ$country[!agric.employ$country%in%country.look.up.for.agric.employ$ATFP.Agric.employ])
agric.employ<-merge(agric.employ,country.look.up.for.agric.employ,by.x="country",by.y="ATFP.Agric.employ")
head(agric.employ) ; nrow(agric.employ)
agric.employ<-agric.employ[,c(4,2:3)]
names(agric.employ)[1]<-"country"
country.look.up.for.total.pop<-country.look.up[,c(1,29)]
unique(total.pop$country[!total.pop$country%in%country.look.up.for.total.pop$Population])
total.pop<-merge(total.pop,country.look.up.for.total.pop,by.x="country",by.y="Population")
head(total.pop); nrow(total.pop)
total.pop<-total.pop[,c(4,2:3)]
names(total.pop)[1]<-"country"
total.pop<-total.pop[order(total.pop$year),] # order by year
total.pop<-total.pop[order(total.pop$country),] # order alphabetically # Proceeding with merging
fsdata<-merge(agric.employ,total.pop, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE) #Proceed with merging
head(fsdata); nrow(fsdata); 
nrow(agric.employ); nrow(total.pop)
unique(total.pop$country[!total.pop$country%in%fsdata$country]) #which countries did we lose (if any)?
unique(agric.employ[!agric.employ$country%in%fsdata$country]) #which countries did we lose (if any)?
fsdata$value<-fsdata$agric.employ*100/fsdata$total.pop
head(fsdata); nrow(fsdata)
fsdata<-fsdata[,c(1,2,5)]
fsdata[fsdata$value>=100,]<-NA # remove values above 100 (which would be wrong). Only applicable for 'Dominica'
fsdata<-na.omit(fsdata) # remove NAs (~300)
head(fsdata); nrow(fsdata)
write.csv(fsdata,"Collated_der_ATFP Agricultural employment, percent of population.csv",row.names=F)

#1.2.9.3 ILO Agricultural employment (% of total employment)
fsdata<-read.csv("WB_ILOmodel_Agricultural employment (% of employed pop).csv",header=T)
head(fsdata)
fsdata<-fsdata[,-2]
library(reshape)
fsdata<-melt(fsdata,id.vars="Country.Name")
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
fsdata$year<-substring(fsdata$year,2,5)
fsdata<-fsdata[order(fsdata$country),] #arrange by country alphabetically before generating the csv file
write.csv(fsdata,"Collated_der_WB ILO Agricultural employment, percent of total employment.csv",row.names=F)

## 1.2.10 Agricultural value added (current US$)
fsdata<-read.csv("WDI_Agriculture.production.indices.csv",header=T)
nrow(fsdata) ; head(fsdata)
fsdata<-fsdata[fsdata$Series.Name=="Agriculture, forestry, and fishing, value added (current US$)",] # extracting agriculture value added
fsdata[3,] # checking row 3
names(fsdata)
fsdata<-fsdata[,-c(2,4)]
library(reshape)
fsdata<-melt(fsdata,id.vars=c("Country.Name","Series.Name"))
names(fsdata)<-c("country","series","year","value")
fsdata$year<-substring(fsdata$year,2,5) # extracting only the year (e.g. 1960) from the vector (e.g. X1960..YR1960)
fsdata$value[fsdata$value==".."]<-NA
fsdata[22999,] # checking row 22999
fsdata$value<-as.numeric(as.vector(fsdata$value))
fsdata<-fsdata[,-2]
fsdata<-fsdata[order(fsdata$country),] # order alphabetically 
fsdata<-na.omit(fsdata) # remove NAs
write.csv(fsdata,"Collated_der_Agricultural Value Added, absolute.csv",row.names=F)

## 1.3 Aggregate indicators

## 1.3.1 Agricultural inputs (ratio, 1991=100)
fsdata<-read.csv("ATFP_Agric.Inputs.csv",header=T)
head(fsdata)
library(reshape)
fsdata<-melt(fsdata,id.vars="Country")
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
fsdata$year<-substring(fsdata$year,2,5)
fsdata<-fsdata[order(fsdata$country),] #arrange by country alphabetically before generating the csv file
write.csv(fsdata,"Collated_agr_ATFP Agricultural inputs.csv",row.names=F)

## 1.3.2 Agricultural Total Factor Productivity (ratio, 1991=100)
fsdata<-read.csv("AgTotal Factor Productivity individualcountries_simplified version.csv",header=T)
head(fsdata)
library(reshape)
fsdata<-melt(fsdata,id.vars="Country")
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
fsdata$year<-substring(fsdata$year,2,5)
fsdata<-fsdata[order(fsdata$country),] #arrange by country alphabetically before generating the csv file
write.csv(fsdata,"Collated_agr_Agricultural Total Factor Productivity.csv",row.names=F)

## 1.3.3 Agricultural value added (% of GDP)
fsdata<-read.csv("WDI_Agriculture.production.indices.csv",header=T)
nrow(fsdata) ; head(fsdata)
fsdata<-fsdata[fsdata$Series.Name=="Agriculture, forestry, and fishing, value added (% of GDP)",] # extracting percentual of GDP agriculture value added
fsdata[3,] # checking row 3
names(fsdata)
fsdata<-fsdata[,-c(2,4)]
library(reshape)
fsdata<-melt(fsdata,id.vars=c("Country.Name","Series.Name"))
names(fsdata)<-c("country","series","year","value")
fsdata$year<-substring(fsdata$year,2,5) # extracting only the year (e.g. 1960) from the vector (e.g. X1960..YR1960)
fsdata$value[fsdata$value==".."]<-NA
fsdata[22999,] # checking row 22999
fsdata$value<-as.numeric(as.vector(fsdata$value))
fsdata<-fsdata[,-2]
fsdata<-fsdata[order(fsdata$country),] # order alphabetically 
fsdata<-na.omit(fsdata) # remove NAs
write.csv(fsdata,"Collated_agr_Agricultural Value Added, percent of GDP.csv",row.names=F)

## 1.3.4 Agricultural value added per worker (constant 2005 US$)
# code found at ## 1.1.10 Agricultural employment

## 1.3.5 Consumer Price Index, Food (ratio, 2010=100)
fsdata<-read.csv("FAO_ Consumer Price Index, Food.csv",header=T)
head(fsdata)
fsdata<-fsdata[,c(4,6,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"month"
names(fsdata)[4]<-"value"
country.list<-unique(fsdata$country) # include all countries in the country.list
results<-NULL # creating an empty table
for (i in country.list []){     # loop for each country  # fsdata=="Netherlands Antilles (former)"
  #  print(i) }
  country.data<-fsdata[fsdata$country==i,]
  country<-i
  results.country<-NULL # creating an empty table for each country
  for (y in unique(country.data$year)){  # start loop for y in year  # loop for each year in each country 
    # print(y)}  
    country.data$mean.value[country.data$year==y]<-mean(country.data$value[country.data$year==y],na.rm=T) # mean of values from January to December in this unique year
    country.mean<-mean(country.data$value[country.data$year==y],na.rm=T)
    results.temp<-data.frame(country,y,country.mean) #storing results line
    results.country<-rbind(results.country,results.temp) #'growing' the results table by one line
    results<-rbind(results,results.temp) #'growing' the results table by one line
    } # end y in year for each country
  with(results.country,plot(country.mean~y,type="o",main=i))   # plot each country's mean annual CPI, Food. 
  }
head(results) ; nrow(results)
names(results)[1]<-"country"
names(results)[2]<-"year"
names(results)[3]<-"value"
write.csv(results,"Collated_agr_CPI, Food.csv",row.names=F)

## 1.3.6 Producer Price Index, Agriculture (ratio, 2004-2006=100)
fsdata<-read.csv("FAOSTAT_Producer Price Index_2004-2006=100.csv",header=T)
head(fsdata)
fsdata<-fsdata[,c(4,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_agr_Producer Price Index.csv",row.names=F)

## 1.3.7  Domestic Food Price (ratio)
fsdata<-read.csv("FAO_Domestic food price level index.csv",header=T)
head(fsdata)
fsdata<-fsdata[,c(3,5,6)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_agr_Domestic Food Price level.csv",row.names=F)

## 1.3.8  Domestic Food Price (volatility)
fsdata<-read.csv("domestic-food-price-volatility-index.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(1,3,4)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_agr_Domestic Food Price Volatility index.csv",row.names=F)

## 1.3.9 Agriculture Orientation Index (ratio)
fsdata<-read.csv("SDG.Indicator_Agricultural.Orientation.Index.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(7,8,9)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
fsdata<-na.omit(fsdata) # remove NAs
fsdata<-fsdata[order(fsdata$country),] # order alphabetically 
write.csv(fsdata,"Collated_agr_Agriculture Orientation Index.csv",row.names=F)

## PART 2. OUTCOMES METRICS

## 2.1 Individual variables

## 2.1.1 Forest cover (1,000 ha)
fsdata<-read.csv("FAOSTAT_Forest data_6-8-2018.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_ind_Forest.area.csv",row.names=F)

## 2.2 Derived variables

## 2.2.1 Forest land (% of total land area)
fsdata<-read.csv("FAOSTAT_Land_Agric_Forest_Total.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,8,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"series"
names(fsdata)[3]<-"year"
names(fsdata)[4]<-"value"
unique(fsdata$series)
fsdata.land<-fsdata[fsdata$series=="Land area",]  # create table for total land area
fsdata.forest<-fsdata[fsdata$series=="Forest land",]  # create table for forest area
fsdata.land<-fsdata.land[,-2] 
names(fsdata.land)[3]<-"land.area"
fsdata.forest<-fsdata.forest[,-2] 
names(fsdata.forest)[3]<-"forest.area"
fsdata<-merge(fsdata.forest,fsdata.land)  
nrow(fsdata) ; head(fsdata)
fsdata$value<-fsdata$forest.area*100/fsdata$land.area  
head(fsdata)
fsdata<-fsdata[,c(1,2,5)]
write.csv(fsdata,"Collated_der_Forest area, percentage.csv",row.names=F)

## 2.2.2 Agricultural GHG emissions, by gases (Gigagrams of CH4, N2O, and total CO2 equivalent)
fsdata<-read.csv("FAOSTAT_All GHG emissions.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,6,8,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"series"
names(fsdata)[3]<-"sector"
names(fsdata)[4]<-"year"
names(fsdata)[5]<-"value"
unique(fsdata$series)
unique(fsdata$sector)
fsdata<-fsdata[fsdata$sector=="Agriculture total",]
fsdata<-fsdata[,-3]
fsdata.ch4<-fsdata[fsdata$series=="Emissions (CO2eq) from CH4",]  # create table for CH4 emissions
fsdata.n2o<-fsdata[fsdata$series=="Emissions (CO2eq) from N2O",]  # create table for N20 emissions
fsdata.total<-fsdata[fsdata$series=="Emissions (CO2eq)",]  # create table for total CO2eq emissions
fsdata.ch4<-fsdata.ch4[,-2] 
fsdata.n2o<-fsdata.n2o[,-2] 
fsdata.total<-fsdata.total[,-2] 
head(fsdata.ch4); head(fsdata.n2o); head(fsdata.total)
write.csv(fsdata.total,"Collated_der_Agriculture total GHG Emissions.csv",row.names=F)
write.csv(fsdata.ch4,"Collated_der_Agriculture CH4 GHG Emissions.csv",row.names=F)
write.csv(fsdata.n2o,"Collated_der_Agriculture N2O GHG Emissions.csv",row.names=F)
#Total Agriculture GHG emissions, 1961-2016
fsdata<-read.csv("GHG Emissions FAOSTAT_data_5-9-2018.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_der_Agriculture total GHG Emissions, 1961-2016.csv",row.names=F)

## 2.2.3 Land use sources GHG emissions, by gases (Gigagrams of CH4, N2O, and CO2, in CO2 equivalent)
fsdata<-read.csv("FAOSTAT_All GHG emissions.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,6,8,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"series"
names(fsdata)[3]<-"sector"
names(fsdata)[4]<-"year"
names(fsdata)[5]<-"value"
unique(fsdata$series)
unique(fsdata$sector)
fsdata<-fsdata[fsdata$sector=="Land use sources",]
fsdata<-fsdata[,-3]
fsdata.ch4<-fsdata[fsdata$series=="Emissions (CO2eq) from CH4",]  # create table for CH4 emissions
fsdata.n2o<-fsdata[fsdata$series=="Emissions (CO2eq) from N2O",]  # create table for N20 emissions
fsdata.co2<-fsdata[fsdata$series=="Emissions (CO2eq) from CO2",]  # create table for CO2 emissions
fsdata.ch4<-fsdata.ch4[,-2] 
fsdata.n2o<-fsdata.n2o[,-2] 
fsdata.co2<-fsdata.co2[,-2] 
head(fsdata.ch4); head(fsdata.n2o); head(fsdata.co2)
write.csv(fsdata.co2,"Collated_der_Land sector CO2 GHG Emissions.csv",row.names=F)
write.csv(fsdata.ch4,"Collated_der_Land sector CH4 GHG Emissions.csv",row.names=F)
write.csv(fsdata.n2o,"Collated_der_Land sector N2O GHG Emissions.csv",row.names=F)

## 2.2.4 Total GHG emissions, by sector (total Gigagrams, CO2 equivalent)
fsdata<-read.csv("FAOSTAT_All GHG emissions.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,6,8,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"series"
names(fsdata)[3]<-"sector"
names(fsdata)[4]<-"year"
names(fsdata)[5]<-"value"
unique(fsdata$series)
unique(fsdata$sector)
fsdata<-fsdata[fsdata$series=="Emissions (CO2eq)",]
fsdata<-fsdata[,-2]
fsdata.agric<-fsdata[fsdata$sector=="Agriculture total",]  # create table for CH4 emissions
fsdata.land<-fsdata[fsdata$sector=="Land use sources",]  # create table for N20 emissions
fsdata.forest<-fsdata[fsdata$sector=="Forest",]  # create table for total CO2eq emissions
head(fsdata.agric); head(fsdata.land); head(fsdata.forest)
fsdata.agric<-fsdata.agric[,-2]
names(fsdata.agric)[3]<-"agric.ghg"
fsdata.land<-fsdata.land[,-2]
write.csv(fsdata.land,"Collated_der_Land sector, Total GHG Emissions.csv",row.names=F)
names(fsdata.land)[3]<-"land.ghg"
fsdata.forest<-fsdata.forest[,-2]
write.csv(fsdata.forest,"Collated_der_Forest sector, Total GHG Emissions.csv",row.names=F)
names(fsdata.forest)[3]<-"forest.ghg" #RESULTS FROM FOREST ARE NEGATIVE, DOUBLE-CHECK WHY
fsdata.sectors<-merge(fsdata.agric,fsdata.forest)
fsdata.sectors<-merge(fsdata.sectors,fsdata.land, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
head(fsdata.sectors)
fsdata.sectors$value<-fsdata.sectors$agric.ghg+fsdata.sectors$forest.ghg+fsdata.sectors$land.ghg
fsdata<-fsdata.sectors[,c(1,2,6)]
head(fsdata)
write.csv(fsdata,"Collated_der_AFOLU Agriculture + Forestry + Land use sources, Total GHG Emissions.csv",row.names=F)

## 2.2.X CAIT GHG emissions: Agriculture, Land-Use Change and Forestry, and AFOLU (total MtCO2eq)
fsdata<-read.csv("CAIT_historical_GHG_emissions_AFOLU.csv",header=T)
head(fsdata); names(fsdata)
fsdata<-fsdata[,-c(2,4,5)]
unique(fsdata$Sector)
agric.ghg<-fsdata[fsdata$Sector=="Agriculture",]
agric.ghg<-agric.ghg[,-2]
land.for<-fsdata[fsdata$Sector=="Land-Use Change and Forestry",]
land.for<-land.for[,-2]
library(reshape)
agric.ghg<-melt(agric.ghg,id.vars=c("Country"))
head(agric.ghg)
names(agric.ghg)<-c("country","year","value")
agric.ghg$year<-substring(agric.ghg$year,2,5)
agric.ghg<-agric.ghg[order(agric.ghg$year),]
agric.ghg<-agric.ghg[order(agric.ghg$country),]
typeof(agric.ghg$value)
agric.ghg$value<-as.numeric(agric.ghg$value)
write.csv(agric.ghg,"Collated_der_CAIT Agriculture total GHG Emissions.csv",row.names=F)
land.for<-melt(land.for,id.vars=c("Country"))
head(land.for)
names(land.for)<-c("country","year","value")
land.for$year<-substring(land.for$year,2,5)
land.for<-land.for[order(land.for$year),]
land.for<-land.for[order(land.for$country),]
typeof(land.for$value)
land.for$value<-as.numeric(land.for$value)
write.csv(land.for,"Collated_der_CAIT Land-Use Change and Forestry total GHG Emissions.csv",row.names=F)
afolu.ghg<-merge(agric.ghg,land.for, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
names(afolu.ghg)[3]<-"agric.ghg"
names(afolu.ghg)[4]<-"land.for"
afolu.ghg$total.ghg<-afolu.ghg$agric.ghg+afolu.ghg$land.for
head(afolu.ghg); names(afolu.ghg)
afolu.ghg<-afolu.ghg[,c(1,2,5)]
names(afolu.ghg)[3]<-"value"
write.csv(afolu.ghg,"Collated_der_CAIT AFOLU total GHG Emissions.csv",row.names=F)

## 2.2.5 Adult Obesity (% of population)
fsdata<-read.csv("share-of-adults-defined-as-obese.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(1,3,4)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_der_Obesity prevalence, adults.csv",row.names=F)

## 2.2.6 Undernourishment (% of population)
## 2.2.6.X FAO Undernourishment
fsdata<-read.csv("FAO_Prevalence of undernourishment.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,16,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_der_FAO Undernourishment.csv",row.names=F)
## 2.2.6.Y Our World In Data Undernourishment
fsdata<-read.csv("prevalence-of-undernourishment.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(1,3,4)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_der_OWD Undernourishment.csv",row.names=F)

## 2.2.7  Per capita Food supply variability
fsdata<-read.csv("Per capita food supply variability - FAOSTAT_data_5-9-2018.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(4,10,12)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
write.csv(fsdata,"Collated_agr_Per capita food supply variability.csv",row.names=F)

# 2.3 Aggregate indicators

## 2.3.1 Red List Index (ratio)
fsdata<-read.csv("SDG.Indicator_Red.List.Index.csv",header=T)
names(fsdata)
fsdata<-fsdata[,c(7,8,9,15)]
head(fsdata)
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
names(fsdata)[4]<-"bound"
fsdata<-subset(fsdata, bound=="MP")
fsdata<-fsdata[,-4]
names(country.look.up) #Use country look up table to keep countries only (exclude regions and territories)
country.look.up.for.red.list<-country.look.up[,c(1,24)]
fsdata<-merge(fsdata,country.look.up.for.red.list, by.x="country",by.y="Red.list")
head(fsdata) ; nrow(fsdata)
fsdata<-fsdata[,c(4,2:3)]
names(fsdata)[1]<-"country"
fsdata<-fsdata[order(fsdata$year),] #order by year
fsdata<-fsdata[order(fsdata$country),] # order alphabetically
write.csv(fsdata,"Collated_agr_Red List Index.csv",row.names=F)

## 2.3.2 Agricultural GHG emissions (tonnes per ha of CH4, N20, and CO2eq)
fsdata.co2eq<-read.csv("Collated_der_Agriculture total GHG Emissions.csv",header=T)
head(fsdata.co2eq)
names(fsdata.co2eq)[3]<-"co2eq.emissions"
fsdata.ch4<-read.csv("Collated_der_Agriculture CH4 GHG Emissions.csv",header=T)
head(fsdata.ch4)
names(fsdata.ch4)[3]<-"ch4.emissions"
fsdata.n2o<-read.csv("Collated_der_Agriculture N2O GHG Emissions.csv",header=T)
head(fsdata.n2o)
names(fsdata.n2o)[3]<-"n2o.emissions"
agric.area<-read.csv("Collated_ind_Agricultural area.csv",header=T)
head(agric.area)
names(agric.area)[3]<-"agric.area"
names(country.look.up) #Use country look up table before merging
country.look.up.for.ghg<-country.look.up[,c(1,25)]
fsdata.co2eq<-merge(fsdata.co2eq,country.look.up.for.ghg,by.x="country",by.y="Agric.emissions")
head(fsdata.co2eq); nrow(fsdata.co2eq)
fsdata.co2eq<-fsdata.co2eq[,c(4,2:3)]
names(fsdata.co2eq)[1]<-"country"
fsdata.ch4<-merge(fsdata.ch4,country.look.up.for.ghg,by.x="country",by.y="Agric.emissions")
head(fsdata.ch4); nrow(fsdata.ch4)
fsdata.ch4<-fsdata.ch4[,c(4,2:3)]
names(fsdata.ch4)[1]<-"country"
fsdata.n2o<-merge(fsdata.n2o,country.look.up.for.ghg,by.x="country",by.y="Agric.emissions")
head(fsdata.n2o); nrow(fsdata.n2o)
fsdata.n2o<-fsdata.n2o[,c(4,2:3)]
names(fsdata.n2o)[1]<-"country"
country.look.up.for.agric.area<-country.look.up[,c(1,6)]
agric.area<-merge(agric.area,country.look.up.for.agric.area,by.x="country",by.y="Agric.area")
head(agric.area); nrow(agric.area)
agric.area<-agric.area[,c(4,2:3)]
names(agric.area)[1]<-"country" #Proceed with merging next
fsdata.co2eq<-merge(fsdata.co2eq,agric.area, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
fsdata.ch4<-merge(fsdata.ch4,agric.area, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
fsdata.n2o<-merge(fsdata.n2o,agric.area, by.x=c("country", "year"), by.y=c("country", "year"), all.x=TRUE)
head(fsdata.co2eq); head(fsdata.ch4); head(fsdata.n2o)
fsdata.co2eq$value<-fsdata.co2eq$co2eq.emissions/fsdata.co2eq$agric.area # unity expressed as: tonnes per ha (gigarams/1,000ha)
fsdata.co2eq<-fsdata.co2eq[,c(1,2,5)]
fsdata.co2eq<-na.omit(fsdata.co2eq) # remove NAs
head(fsdata.co2eq); nrow(fsdata.co2eq)
write.csv(fsdata.co2eq,"Collated_agr_Agricultural total CO2eq GHG emissions, tonnes per ha.csv",row.names=F)
fsdata.ch4$value<-fsdata.ch4$ch4.emissions/fsdata.ch4$agric.area # unity expressed as: tonnes per ha (gigarams/1,000ha)
fsdata.ch4<-fsdata.ch4[,c(1,2,5)]
fsdata.ch4<-na.omit(fsdata.ch4) # remove NAs
head(fsdata.ch4); nrow(fsdata.ch4)
write.csv(fsdata.ch4,"Collated_agr_Agricultural CH4 GHG emissions, tonnes per ha.csv",row.names=F)
fsdata.n2o$value<-fsdata.n2o$n2o.emissions/fsdata.n2o$agric.area # unity expressed as: tonnes per ha (gigarams/1,000ha)
fsdata.n2o<-fsdata.n2o[,c(1,2,5)]
fsdata.n2o<-na.omit(fsdata.n2o) # remove NAs
head(fsdata.n2o); nrow(fsdata.n2o)
write.csv(fsdata.n2o,"Collated_agr_Agricultural N2O GHG emissions, tonnes per ha.csv",row.names=F)

## PART 3. OTHER METRICS:

## 3.1 Individual variables

## 3.1.1 Population growth (total population)
total.pop<-read.csv("WDI_Population_total.and.density.csv",header=T) #File saved in Raw Data, not Collated Data files. Change working directory to read csv
head(total.pop); names(total.pop)
total.pop<-total.pop[total.pop$Series.Name=="Population, total",] # extracting only total population
total.pop[3,] # checking row 3
names(total.pop)
total.pop<-total.pop[,-c(2,4)] #remove columns 2 and 4
library(reshape)
total.pop<-melt(total.pop,id.vars=c("Country.Name","Series.Name"))
names(total.pop)<-c("country","series","year","value")
head(total.pop)
total.pop$year<-substring(total.pop$year,2,5) # extracting only the year (e.g. 1960) from the vector (e.g. X1960..YR1960)
total.pop$value[total.pop$value==".."]<-NA
total.pop[2999,] # checking row 2999
total.pop$value<-as.numeric(as.vector(total.pop$value))
total.pop<-total.pop[,-2] # remove colum 2 (series)
total.pop<-total.pop[order(total.pop$country),] # order alphabetically
write.csv(total.pop,"Collated_ind_Total population.csv",row.names=F)

## 3.2 Derivate variables

## 3.2.1 Population growth (density, people per sq. km of area)
fsdata<-read.csv("WDI_Population_total.and.density.csv",header=T)
head(fsdata); names(fsdata)
fsdata<-fsdata[fsdata$Series.Name=="Population density (people per sq. km of land area)",] # extracting only population density
fsdata[3,] # checking row 3
names(fsdata)
fsdata<-fsdata[,-c(2,4)] #remove columns 2 and 4
library(reshape)
fsdata<-melt(fsdata,id.vars=c("Country.Name","Series.Name"))
names(fsdata)<-c("country","series","year","value")
head(fsdata)
fsdata$year<-substring(fsdata$year,2,5) # extracting only the year (e.g. 1960) from the vector (e.g. X1960..YR1960)
fsdata$value[fsdata$value==".."]<-NA
fsdata[2999,] # checking row 2999
fsdata$value<-as.numeric(as.vector(fsdata$value))
fsdata<-fsdata[,-2] # remove colum 2 (series)
fsdata<-fsdata[order(fsdata$country),] # order alphabetically
fsdata<-na.omit(fsdata) # remove NAs
write.csv(fsdata,"Collated_der_Population density.csv",row.names=F)

## 3.2.2 GDP per capita (current US$) #Nominal and Purchasing Power Parity (PPP)
fsdata<-read.csv("IMF_GDP.per.capita_Nominal and PPP.csv",header=T)
head(fsdata)
fsdata<-fsdata[,-c(2,4,5)]
library(reshape)
names(fsdata)
fsdata$Country<-as.character(fsdata$Country)
fsdata<-melt(fsdata,id.vars=c("Country","Units"))
names(fsdata)<-c("country","series","year","value")
head(fsdata)
fsdata$year<-substring(fsdata$year,2,5) # extracting only the year (e.g. 1960) from the vector (e.g. X1960..YR1960)
fsdata$value[fsdata$value=="n/a"]<-NA
fsdata[2999,]
unique(fsdata$series)
fsdata.nom<-fsdata[fsdata$series=="U.S. dollars",]
fsdata.nom<-fsdata.nom[,-2]
fsdata.nom<-na.omit(fsdata.nom) # remove NAs
fsdata.nom<-fsdata.nom[order(fsdata.nom$country),] # order alphabetically
head(fsdata.nom)
write.csv(fsdata.nom,"Collated_der_GDP per capita, nominal.csv",row.names=F)
fsdata.ppp<-fsdata[fsdata$series=="Purchasing power parity; international dollars",]
fsdata.ppp<-fsdata.ppp[,-2]
fsdata.ppp<-na.omit(fsdata.ppp) # remove NAs
fsdata.ppp<-fsdata.ppp[order(fsdata.ppp$country),] # order alphabetically
head(fsdata.ppp)
write.csv(fsdata.ppp,"Collated_der_GDP per capita, PPP.csv",row.names=F)

## 3.3 Aggregate indicators

## 3.3.1 Gini index (ratio)
fsdata<-read.csv("WDI_Gini.index.csv",header=T)
head(fsdata)
fsdata<-fsdata[,-c(2,4)]
library(reshape)
fsdata<-melt(fsdata,id.vars=c("Country.Name","Series.Name"))
names(fsdata)<-c("country","series","year","value")
head(fsdata)
fsdata$year<-substring(fsdata$year,2,5) # extracting only the year (e.g. 1960) from the vector (e.g. X1960..YR1960)
fsdata$value[fsdata$value==".."]<-NA
fsdata[2999,] # checking row 2999
fsdata$value<-as.numeric(as.vector(fsdata$value))
fsdata<-fsdata[,-2] # remove colum 2 (series)
fsdata<-fsdata[order(fsdata$country),] # order alphabetically
fsdata<-na.omit(fsdata) # remove NAs
write.csv(fsdata,"Collated_agr_Gini index.csv",row.names=F)

## 3.3.2 Socio-demographic index, SDI (ratio)
fsdata<-read.csv("GBD_SDI.csv",header=T)
head(fsdata)
fsdata<-fsdata[,-1]
names(fsdata)[1]<-"country"
names(fsdata)[2]<-"year"
names(fsdata)[3]<-"value"
unique(fsdata$country)
fsdata<-fsdata[order(fsdata$country),] # order alphabetically
fsdata<-na.omit(fsdata) # remove NAs
names(country.look.up) 
country.look.up.for.SDI<-country.look.up[,c(1,30)] # Keep countries only, remove territories
fsdata<-merge(fsdata,country.look.up.for.SDI,by.x="country",by.y="SDI")
head(fsdata) ; nrow(fsdata)
fsdata<-fsdata[,c(4,2:3)]
names(fsdata)[1]<-"country"
write.csv(fsdata,"Collated_agr_Socio-Demographic Index.csv",row.names=F)

## 3.3.3 FAO Food Price Index, FPI (2002-2004=100) 
fsdata<-read.csv("FAO_Food_price_indices_for.R.csv",header=T)
head(fsdata)
fsdata<-fsdata[,c(1,2)]
fsdata$Date<-substring(fsdata$Date,5,6)
names(fsdata)[1]<-"year"
names(fsdata)[2]<-"value"
unique(fsdata$year)
year.list<-unique(fsdata$year) # include all years in the year.list
results<-NULL # creating an empty table
for (i in year.list []){     # loop for each year
  year.data<-fsdata[fsdata$year==i,]
  year<-i
  mean.value<-mean(year.data$value) # mean of values from January to December for this year
  results.temp<-data.frame(year,mean.value) #storing results line
  results<-rbind(results,results.temp) #'growing' the results table by one line
}
head(results) ; nrow(results)
names(results)[2]<-"value"
results$full.year<-c(1990:2019)
results<-results[,c(3,2)]
names(results)[1]<-"year"
write.csv(results,"Collated_agr_FAO Food Price Index.csv",row.names=F)