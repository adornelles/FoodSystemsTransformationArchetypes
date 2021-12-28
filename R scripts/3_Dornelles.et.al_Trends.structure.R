# Tom Oliver and Andre Dornelles, July and August 2019

# 3_Dornelles.et.al_Trends.structure

# This script calculates longitudinal trends of adapted Compound Annual Change Rate (CACR) in food system structure metrics: input, output, productivity, and economics (see word document 'Dornelles et al_Food systems transformation archetypes_supplementary info' for Metadata).

## This script has three parts:
# 1. Upload the collated structure metrics;
# 2. Loop to calculate trend analysis;
# 3. Generate .csv files for each metric.

## Instructions (repeat it for each metric individually):
# First: load in the data file of one metric in Part 1 (collated data directory); 
# Second: run Part 2 to calculate the trend analysis;
# Third: finish by generating the .csv file for the respective metric in Part 3 (trend results directory).

# Set working directory to load the collated files from Script 1:
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data\\Collated data files")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Raw Data\\Collated data files")

### PART 1: load in the collated data files of structure metrics.

# 1. Input

# 1.1 Agricultural area (1,000 ha)
fsdata<-read.csv("Collated_ind_Agricultural area.csv",header=T)

# 1.2 Synthetic Fertilizer consumption (tonnes)
fsdata<-read.csv("Collated_ind_ATFP Synthetic Fertilizer consumption.csv",header=T)

# 1.3  Pesticide use (tonnes)
fsdata<-read.csv("Collated_ind_Pesticide use.csv",header=T)

# 1.4 Agricultural employment (% of total employment)
fsdata<-read.csv("Collated_der_WB ILO Agricultural employment, percent of total employment.csv",header=T)

# 2. Output

# 2.1 Gross Agricultural Output (Constant, 2010=100 Int.US$)
fsdata<-read.csv("Collated_ind_Gross Agricultural Output.csv",header=T)

# 3. Productivity (input -> output)

# 3.1 Agricultural Total Factor Productivity (ratio, 1991=100)
fsdata<-read.csv("Collated_agr_Agricultural Total Factor Productivity.csv",header=T)
fsdata$value<-as.numeric(as.vector(fsdata$value)) 
fsdata[is.na(fsdata$value),]  # one NA added in by coercion for Kuwait which did have a value of 1,151
fsdata$value[fsdata$country=="Kuwait"&fsdata$year==2014]<-1151  # replace this with the corrected original value

# 4. Economics (trade and price)

# 4.1 Food import (US$)
fsdata<-read.csv("Collated_ind_Food imports absolute.csv",header=T)

# 4.2 Food export (US$)
fsdata<-read.csv("Collated_ind_Food exports absolute.csv",header=T)

# 4.3 Consumer Price Index, Food (ratio, 2010=100)
fsdata<-read.csv("Collated_agr_CPI, Food.csv",header=T)

# 4.4 Producer Price Index, Agriculture (ratio, 2004-2006=100)
fsdata<-read.csv("Collated_agr_Producer Price Index.csv",header=T)

## Part 2: calculate trend resulst standardized by "country", "p value of the linear model", "coefficient of the linear model", "annual compound change rate", "standard deviation in interannual change rate", and "median absolute deviation (MAD)"

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
country.list

#pick transparent colours
transp.blue <- rgb(col2rgb("blue")[1], col2rgb("blue")[2], col2rgb("blue")[3],max = 255,alpha = (30)*255/100)
transp.red <- rgb(col2rgb("red")[1], col2rgb("red")[2], col2rgb("red")[3],max = 255,alpha = (30)*255/100)
devAskNewPage(ask = FALSE)   #  asks user to press enter before seeing next plot. TRUE=yes and FALSE=no

results<-NULL  # creating an empty table
for (i in country.list[]){     # loop for each country  
  # country.list=="Sweden"
  # print(i) }
  country.data<-fsdata[fsdata$country==i,]
  country<-i
  with(country.data,plot(value~year,type="o",main=i,lwd=2))   # to plot the country's data
  model.lin<-lm(value~year,country.data)    # linear model
  # summary(model.lin) #  produce summary of regression model output
  # abline(model.lin)  # add line to plot
  model.lin.p<-summary(model.lin)[[4]][8]     # storing linear model coefficients
  model.lin.coef<-summary(model.lin)[[4]][2]
  country.data<-country.data[order(country.data$year),]  # rewrite the dataframe ordered by year
  max.year<-max(country.data$year)
  min.year<-min(country.data$year)
  # calculate the median of the first five and last fiver years:
  start.values<-head(country.data$value,n=5)
  median.start.values<-median(start.values) # median of values for first five years
  end.values<-tail(country.data$value,n=5)
  median.end.values<-median(end.values) # median of values for last five years
  median.start.years<-min(country.data$year[country.data$value%in%median(start.values)]) # year of median start value
  median.end.years<-max(country.data$year[country.data$value%in%median(end.values)]) # year of median end value
  # calculate the compound annual change rate, i.e. the year on year change in percentage points:
  ifelse(median.start.values==0,median.start.values<-0.00000001,median.start.values<-median.start.values) # cannot compute compound chnage with zero values, so set to non-zero (v.small)
  ifelse(median.end.values==0,median.end.values<-0.00000001,median.end.values<-median.end.values) # cannot compute compound chnage with zero values, so set to non-zero (v.small)
  compound.chg.rate<-((median.end.values/median.start.values)^(1/(median.end.years-median.start.years))-1)*100
  # loop to calculate the standard deviation and MAD in annual growth rates 
  country.data$value.t.minus.one<-NA  #  create an empty column
  for (y in unique(country.data$year)){  # start loop for y in year
      # print(y)}  
      if(length(country.data$value[country.data$year==y-1])>0) {   # if there is data for year before then...
      country.data$value.t.minus.one[country.data$year==y]<-country.data$value[country.data$year==y-1] }  # fill in the data from year before
      }  # end y in year
  country.data$annual.change.rate<-country.data$value-country.data$value.t.minus.one # calculate annual change rate
  sd.ann.chng<-sd(country.data$annual.change.rate,na.rm=T)  # variation in interannual change rate
  med.abs.chng<-median(abs(country.data$annual.change.rate),na.rm=T)  # median absolute deviation (MAD)
  results.temp<-data.frame(country,model.lin.p,model.lin.coef,compound.chg.rate,sd.ann.chng,med.abs.chng) #storing results line
  results<-rbind(results,results.temp) #  'growing' the results table by one line
} # end i in country loop
head(results) ; nrow(results)

# Check results that might impair the cluster analysis
results$country[results$compound.chg.rate==0] # Countries with stable values through the years (compound change rate = 0)
length(results$country[results$compound.chg.rate==0])
results$country[results$model.lin.coef<=0.005&results$model.lin.coef>=-0.005] # Countries with very low linear coefficients (between -0.05 and 0.05)
length(results$country[results$model.lin.coef<=0.005&results$model.lin.coef>=-0.005])

# PART 3: Generate files for the filter of best fit:

# Set working directory to save trend results of structure metrics:      
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")

# Generate files filtered from 1995 to 2015:
write.csv(results,"Trends_fs_structure_filtered.15_Agricultural Area.csv",row.names=F)
write.csv(results,"Trends_fs_structure_filtered.15_ATFP Synthetic Fertiliser.csv",row.names=F)
write.csv(results,"Trends_fs_structure_filtered.15_Pesticide.csv",row.names=F)
write.csv(results,"Trends_fs_structure_filtered.15_WB ILO Agricultural employment.csv",row.names=F)
write.csv(results,"Trends_fs_structure_filtered.15_Gross Agricultural Output.csv",row.names=F)
write.csv(results,"Trends_fs_structure_filtered.15_Agricultural Total Factor Productivity.csv",row.names=F)
write.csv(results,"Trends_fs_structure_filtered.15_Food imports.csv",row.names=F)
write.csv(results,"Trends_fs_structure_filtered.15_Food exports.csv",row.names=F)
write.csv(results,"Trends_fs_structure_filtered.15_CPI, Food.csv",row.names=F)
write.csv(results,"Trends_fs_structure_filtered.15_Producer Price Index, Agriculture.csv",row.names=F)
