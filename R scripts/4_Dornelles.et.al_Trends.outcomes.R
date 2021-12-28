# Tom Oliver and Andre Dornelles, July and August 2019

# 4_Dornelles.et.al_Trends.outcomes

# This script calculates longitudinal trends of adapted Compound Annual Change Rate (CACR) in food system outcome metrics (malnutrition, biosphere integrity, land-system change, and GHG emissions). See word document 'Dornelles et al_Food systems transformation archetypes_supplementary info' for Metadata.

## This script has three parts:
# 1. Upload the collated outcome metrics;
# 2. Loop to calculate trend analysis;
# 3. Generate .csv files for each metric, by common country name.

## Instructions (repeat it for each metric individually):
# First: upload the country look up table (country look up table directory);
# Second: load in the data file of one metric in Part 1 (collated data directory); 
# Third: run loop to calculate the trend analysis in Part 2,
# Fourth:lastly, use the country look up table to compile results by common country name of the respective metric in Part 3 (trend results directory).

# Country look up table
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
country.look.up<-read.csv("Country name look up table_updated.csv",header=T)
names(country.look.up) 

# Load in datasets
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data\\Collated data files")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data\\Collated data files")

### PART 1: load in the collated data files of structure metrics.

# 1. Biosphere integrity & Land-system change

# 1.1 Forest cover (1,000 ha)
fsdata<-read.csv("Collated_ind_Forest.area.csv",header=T)

# 1.2 Red List Index (ratio)
fsdata<-read.csv("Collated_agr_Red List Index.csv",header=T)

# 1.3 NFA National Ecological Footprint, Total Consumption (global hectares, gha) 
fsdata<-read.csv("Collated_agr_NFA Total Consumption.csv",header=T)

# 2. Malnutrition

# 2.1 Undernourishment (% of population)
fsdata<-read.csv("Collated_der_FAO Undernourishment.csv",header=T)
fsdata<-na.omit(fsdata) # remove NAs
fsdata$value<-as.character(fsdata$value)
fsdata$value[fsdata$value=="<2.5"]<-2.0 # replacing the "2.5" for "2.0" so they won't be framed as NAs when expressed as numeric
fsdata$value<-as.numeric(fsdata$value)
fsdata$value[fsdata$country=="Australia"] # Check if the replacement is adequate
fsdata$value[fsdata$country=="Brazil"] # Check if the replacement is adequate
fsdata[is.na(fsdata$value),]

# 2.2 Adult Obesity (% of population)
fsdata<-read.csv("Collated_der_Obesity prevalence, adults.csv",header=T)

# 3. Greenhouse gases emissions
# 3.1 CAIT Total GHG emissions from Agriculture (total MtCO2eq)
fsdata<-read.csv("Collated_der_CAIT Agriculture total GHG Emissions.csv",header=T)

# 3.2 CAIT Total GHG emissions from Land-Use Change and Forestry (total MtCO2eq)
fsdata<-read.csv("Collated_der_CAIT Land-Use Change and Forestry total GHG Emissions.csv",header=T)

# 3.3 CAIT Total GHG emissions from AFOLU (Agriculture + Forestry + Land-Use Change) (total MtCO2eq)
fsdata<-read.csv("Collated_der_CAIT AFOLU total GHG Emissions.csv",header=T)

### Part 2: calculate trend resulst standardized by "country", "minimum year", "maximum year", "mean value", "annual compound change rate", and "final value"

# Filtered by analysis of best fit
head(fsdata) ; nrow(fsdata)

library(dplyr)
# Filter from 1995 to 2015
fsdata<-fsdata %>% 
  filter(year>=1995) %>%
  filter(year<=2015)

head(fsdata) ; nrow(fsdata)
fsdata<-na.omit(fsdata)  # remove rows with no data. In some cases, some countries have no data at all even though they appear in raw data tables, e.g. American Samoa in FAo import export data
# 80% filter (duration.95>=(2015-1995+1)*0.8) = 16.8 years
country.list<-names(with(fsdata,table(country)))[with(fsdata,table(country))>=16.8]# remove countries with less than 16.8 datapoints (years), which corresponds to the 80% filter
length(country.list)
results<-NULL # creating an empty table
for (i in country.list[]){     # loop for each country  # country.list=="Netherlands Antilles (former)"
  # print(i) }
  country.data<-fsdata[fsdata$country==i,]
  country<-i
  #with(country.data,plot(value~year,type="o",main=i))   # to plot the country's data
  max.year<-max(country.data$year)
  min.year<-min(country.data$year)
  start.value<-country.data$value[country.data$year==min.year]
  final.value<-country.data$value[country.data$year==max.year]
  # calculate the median of the first five and last fiver years:
  start.values<-head(country.data$value,n=5)
  median.start.values<-median(start.values) # median of values for first five years
  end.values<-tail(country.data$value,n=5)
  median.end.values<-median(end.values) # median of values for last five years
  median.start.years<-min(country.data$year[country.data$value%in%median(start.values)]) # year if median start value for the last fiver years
  median.end.years<-max(country.data$year[country.data$value%in%median(end.values)]) # year of median end value for the last five years
  # calculate the compound annual change rate, i.e. the year on year change in percentage points:
  ifelse(median.start.values==0,median.start.values<-0.00000001,median.start.values<-median.start.values) # cannot compute compound chnage with zero values, so set to non-zero (v.small)
  ifelse(median.end.values==0,median.end.values<-0.00000001,median.end.values<-median.end.values)
  compound.chg.rate<-((median.end.values/median.start.values)^(1/(median.end.years-median.start.years))-1)*100
  results.temp<-data.frame(country,median.start.years,median.end.years,compound.chg.rate,median.start.values,median.end.values) #storing results line
  results<-rbind(results,results.temp) #  'growing' the results table by one line
}
head(results) ; nrow(results)

### Part 3: Compile results by country common name in the country look up table. CSV files saved in a standardized format by "Common.name", "minimum year", "maximum year", "mean value", "compound annual change rate", and "final value".

# Set working directory to save trend results of outcome metrics:      
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")

names(country.look.up)

# Forest cover (1,000 ha)
unique(fsdata$country) # 232 countries initially 
head(results) ; nrow(results) # 223 countries with data for >= 16.8 years (80% filter)
country.look.up.for.forest<-country.look.up[,c(1,23)]
results<-merge(results,country.look.up.for.forest,by.x="country",by.y="Forest.area")
head(results) ; nrow(results)  # 216 countries: 7 countries lost
results<-results[,c(7,2:6)]
results$Common.name[results$compound.chg.rate==0] # 44 Countries with stable values through the years (compound change rate = 0)
length(results$Common.name[results$compound.chg.rate==0])
results$Common.name[results$final.value==0] # 6 Countries with absent impact in the last year (final year value = 0)
length(results$Common.name[results$final.value==0])
write.csv(results,"Trends_fs_outcomes_filtered.15_Forest area.csv",row.names=F)

# Red List Index (ratio)
unique(fsdata$country) # 219 countries initially
head(results) ; nrow(results) # 219 countries with data for >= 80% filter
country.look.up.for.red.list<-country.look.up[,c(1,24)]
results<-merge(results,country.look.up.for.red.list,by.x="country",by.y="Red.list")
head(results) ; nrow(results)  # 195 countries: 24 countries lost
results<-results[,c(7,2:6)]
results$Common.name[results$compound.chg.rate==0] # 0 Countries with stable values through the years (compound change rate = 0)
length(results$Common.name[results$compound.chg.rate==0])
results$Common.name[results$final.value==0] # 0 Countries with absent impact in the last year (final year value = 0)
length(results$Common.name[results$final.value==0])
write.csv(results,"Trends_fs_outcomes_filtered.15_Red List Index.csv",row.names=F)

# Ecological Foodprint of Consumption (EFA), total (global hectares, gha)
unique(fsdata$country) # 185 countries initially
head(results) ; nrow(results) # 177 countries with data for >= 16.8 years (80% filter)
country.look.up.for.nfa<-country.look.up[,c(1,26)]
results<-merge(results,country.look.up.for.nfa,by.x="country",by.y="NFA")
head(results) ; nrow(results)  # 182 countries (1 country lost)
results<-results[,c(7,2:6)]
results$Common.name[results$compound.chg.rate==0] # 0 Countries with stable values through the years (compound change rate = 0)
length(results$Common.name[results$compound.chg.rate==0])
results$Common.name[results$final.value==0] # 0 Countries with absent impact in the last year (final year value = 0)
length(results$Common.name[results$final.value==0])
write.csv(results,"Trends_fs_outcomes_filtered.15_NFA Total Consumption Footprint.csv",row.names=F)

# Undernourishment (% of population)
# FAO Undernourishment
unique(fsdata$country) # 167 countries initially
head(results) ; nrow(results) # 164 countries with data for >= 80% filter
country.look.up.for.undernourishment<-country.look.up[,c(1,28)]
results<-merge(results,country.look.up.for.undernourishment,by.x="country",by.y="Undernourishment")
head(results) ; nrow(results)  # 163 countries: 1 countriy lost
results<-results[,c(7,2:6)]
results$Common.name[results$compound.chg.rate==0] # 36 Countries with stable values through the years (compound change rate = 0)
length(results$Common.name[results$compound.chg.rate==0])
results$Common.name[results$final.value==0] # 0 Countries with absent impact in the last year (final year value = 0)
length(results$Common.name[results$final.value==0])
write.csv(results,"Trends_fs_outcomes_filtered.15_FAO Undernourishment.csv",row.names=F)

# Adult Obesity (% of population)
unique(fsdata$country) # 191 countries initially
head(results) ; nrow(results) # 191 countries with data for >= 16.8 years (80% filter)
country.look.up.for.obesity<-country.look.up[,c(1,27)]
results<-merge(results,country.look.up.for.obesity,by.x="country",by.y="Obesity")
head(results) ; nrow(results)  # 189 countries: 2 countries lost
results<-results[,c(7,2:6)]
results$Common.name[results$compound.chg.rate==0] # 0 Countries with stable values through the years (compound change rate = 0)
length(results$Common.name[results$compound.chg.rate==0])
results$Common.name[results$final.value==0] # 0 Countries with absent impact in the last year (final year value = 0)
length(results$Common.name[results$final.value==0])
write.csv(results,"Trends_fs_outcomes_filtered.15_Adult obesity.csv",row.names=F)

# CAIT GHG emissions: Agriculture, Land-Use Change and Forestry, and AFOLU (total MtCO2eq)
# CAIT Total GHG emissions from Agriculture
unique(fsdata$country) # 193 countries initially
head(results) ; nrow(results) # 193 countries with data for >= 16.8 years (80% filter)
names(country.look.up)
country.look.up.for.GHG.emissions<-country.look.up[,c(1,35)]
results<-merge(results,country.look.up.for.GHG.emissions,by.x="country",by.y="CAIT.Agric.emissions")
head(results) ; nrow(results)  # 191 countries: 2 countries lost
results<-results[,c(7,2:6)]
results$Common.name[results$compound.chg.rate==0] # 13 Countries with stable values through the years (compound change rate = 0)
length(results$Common.name[results$compound.chg.rate==0])
results$Common.name[results$final.value==0] # 8 Countries with absent impact in the last year (final year value = 0)
length(results$Common.name[results$final.value==0])
write.csv(results,"Trends_fs_outcomes_filtered.15_CAIT Agriculture Total GHG Emissions.csv",row.names=F)

# CAIT Total GHG emissions from Land-Use Change and Forestry
unique(fsdata$country) # 193 countries initially
head(results) ; nrow(results) # 193 countries with data for >= 16.8 years (80% filter)
country.look.up.for.GHG.emissions<-country.look.up[,c(1,35)]
results<-merge(results,country.look.up.for.GHG.emissions,by.x="country",by.y="CAIT.Agric.emissions")
head(results) ; nrow(results)  # 191 countries: 2 countries lost
results<-results[,c(7,2:6)]
results$Common.name[results$compound.chg.rate==0] # ~60 Countries with stable values through the years (compound change rate = 0)
length(results$Common.name[results$compound.chg.rate==0])
results$Common.name[results$final.value==0] # 28 Countries with absent impact in the last year (final year value = 0)
length(results$Common.name[results$final.value==0])
write.csv(results,"Trends_fs_outcomes_filtered.15_CAIT Land-Use Change and Forestry Total GHG Emissions.csv",row.names=F)

# CAIT Total GHG emissions from AFOLU (Agriculture + Forestry + Land-use change)
unique(fsdata$country) # 193 countries initially
head(results) ; nrow(results) # 193 countries with data for >= 16.8 years (80% filter)
country.look.up.for.GHG.emissions<-country.look.up[,c(1,35)]
results<-merge(results,country.look.up.for.GHG.emissions,by.x="country",by.y="CAIT.Agric.emissions")
head(results) ; nrow(results)  # 191 countries: 2 countries lost
results<-results[,c(7,2:6)]
results$Common.name[results$compound.chg.rate==0] # 37 Countries with stable values through the years (compound change rate = 0)
length(results$Common.name[results$compound.chg.rate==0])
results$Common.name[results$final.value==0] # 7 Countries with absent impact in the last year (final year value = 0)
length(results$Common.name[results$final.value==0])
write.csv(results,"Trends_fs_outcomes_filtered.15_CAIT AFOLU Total GHG Emissions.csv",row.names=F)
