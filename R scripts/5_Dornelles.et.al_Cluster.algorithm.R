# Tom Oliver and Andre Dornelles, September 2019

# 5_Dornelles.et.al_Cluster.algorithm

# This script aggregates countries into groups (clusters) based on the trends of key structure metrics of food systems and apply hypothesis testing to identify differences between country groups.

## This script has eight parts:
# 1. Upload trend results of structure metrics;
# 2. Cluster algorithm: transformation archetypes of key structure metrics;
# 3. Hypothesis testing: key structure metrics;
# 4. Transformation archetypes: structure metrics;
# 5. Transformation archetypes: outcome metrics;
# 6. Hypothesis testing: economic structure metrics and outcome metrics;
# 7. World map: transformation archetypes;
# 8. Carrying capacity.

## Instructions:
# First: upload the country look up table (country look up table directory);
# Second: select all code in Part 1 and run it (trend results directory); 
# Third: run Part 2 to activate the cluster algorithm;
# Fourth: run Part 3 to test for differences across contry groups in the key structure metrics;
# Fifth: run Part 4 to link the contry groups to the economic structure metrics and plot the transformation archetypes of all structure metrics;
# Sixth: run Part 5 to link the contry groups to the outcome metrics and plot their transformation archetypes;
# Seventh: lastly, run Part 6 to test for differences across country groups in the economic structure metrics and in the outcome metrics.
# Eight and nine: run parts 7 and 8 to generate the plots of world map and carrying capacity, respectively. 

## Load in each structure metric trend results file in Part A, run cluster analysis in Part B, and link clusters to their outcome trend results in Part C.

# Country look up table
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
country.look.up<-read.csv("Country name look up table_updated.csv",header=T)
names(country.look.up)

# Load in datasets
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")

### PART 1: load trend results for key structure metrics

## Input:

# 1. Agricultural area 
results.ag.area<-read.csv("Trends_fs_structure_filtered.15_Agricultural Area.csv",header=T)
names(results.ag.area) ; nrow(results.ag.area) # 222 countries (1995 to 2015)
results.ag.area<-results.ag.area[,c(1,4,5)]   # just select the metrics needed for cluster analysis
names(results.ag.area)[2:3]<-c("ag.area.compound.chg.rate","ag.area.sd.ann.chng.rate")
names(country.look.up)
country.look.up.for.ag.area<-country.look.up[,c(1,6)]
unique(results.ag.area$country[!results.ag.area$country%in%country.look.up.for.ag.area$Agric.area]) # different countries between the datasets (run before merging)
results.ag.area<-merge(results.ag.area,country.look.up.for.ag.area,by.x="country",by.y="Agric.area")
head(results.ag.area); nrow(results.ag.area)   # 216 countries (6 lost)
results.ag.area<-results.ag.area[,c(4,2,3)]

# 2. Synthetic Fertiliser consumption
results.synt.fert.use<-read.csv("Trends_fs_structure_filtered.15_ATFP Synthetic Fertiliser.csv",header=T)
names(results.synt.fert.use) ; nrow(results.synt.fert.use) #173 countries (1995 to 2015)
results.synt.fert.use<-results.synt.fert.use[,c(1,4,5)]   # just select the metrics needed for cluster analysis
names(results.synt.fert.use)[2:3]<-c("synt.fert.use.compound.chg.rate","synt.fert.use.sd.ann.chng.rate")
names(country.look.up)
country.look.up.for.synt.fert.use<-country.look.up[,c(1,4)] 
results.synt.fert.use<-merge(results.synt.fert.use,country.look.up.for.synt.fert.use,by.x="country",by.y="Synthetic.fertilizers") 
head(results.synt.fert.use); nrow(results.synt.fert.use)   # 170 countries (lost 3)
results.synt.fert.use<-results.synt.fert.use[,c(4,2,3)]

# 3. Agriculture employment (WB ILO, % of total employment)
results.ag.empl<-read.csv("Trends_fs_structure_filtered.15_WB ILO Agricultural employment.csv",header=T)
head(results.ag.empl); nrow(results.ag.empl) # 233 countries (1995 to 2015)
results.ag.empl<-results.ag.empl[,c(1,4,5)]   # just select the country and the metrics needed for cluster analysis
names(results.ag.empl)[2:3]<-c("ag.empl.compound.chg.rate","ag.empl.sd.ann.chng.rate")
names(country.look.up)
country.look.up.for.ag.empl<-country.look.up[,c(1,34)]
unique(results.ag.empl$country[!results.ag.empl$country%in%country.look.up.for.ag.empl$WB.ILO.Agric.employ]) # different countries between the datasets (run before merging)
results.ag.empl<-merge(results.ag.empl,country.look.up.for.ag.empl,by.x="country",by.y="WB.ILO.Agric.employ") # add in common name
head(results.ag.empl); nrow(results.ag.empl)   # 186 countries (47 regions lost, no contries)
results.ag.empl<-results.ag.empl[,c(4,2,3)]

## Output:

# 4. Gross Agricultural Output
results.gao<-read.csv("Trends_fs_structure_filtered.15_Gross Agricultural Output.csv",header=T)
head(results.gao); nrow(results.gao) # 211 countries (1995 to 2015)
results.gao<-results.gao[,c(1,4,5)]   # just select the country and the metrics needed for cluster analysis
names(results.gao)[2:3]<-c("gao.compound.chg.rate","gao.sd.ann.chng.rate") # re-label columns two and three
country.look.up.for.gao<-country.look.up[,c(1,8)]  # pick from look up table the relevant columns
unique(results.gao$country[!results.gao$country%in%country.look.up.for.gao$Agric.output]) # different countries between the datasets (run before merging)
results.gao<-merge(results.gao,country.look.up.for.gao,by.x="country",by.y="Agric.output") # add in common name
head(results.gao); nrow(results.gao)   # 208 countries (3 countries lost)
results.gao<-results.gao[,c(4,2,3)] 

## Productivity (input/output):

# 5. Agricultural Total Factor Productivity (ATFP)
results.atfp<-read.csv("Trends_fs_structure_filtered.15_Agricultural Total Factor Productivity.csv",header=T)
names(results.atfp) ; nrow(results.atfp) # 173 countries (1995 to 2015)
results.atfp<-results.atfp[,c(1,4,5)]   # just select the metrics needed for cluster analysis
names(results.atfp)[2:3]<-c("atfp.compound.chg.rate","atfp.sd.ann.chng.rate")
names(country.look.up)
country.look.up.for.atfp<-country.look.up[,c(1,11)] # select correct column for the metric
unique(results.atfp$country[!results.atfp$country%in%country.look.up.for.atfp$TFP]) # different countries between the datasets (run before merging)
results.atfp<-merge(country.look.up.for.atfp,results.atfp, by.x="TFP", by.y="country")
head(results.atfp); nrow(results.atfp)   # 170 countries (lost 3)
results.atfp<-results.atfp[,c(2,3,4)]

## Trade & Price

# 6. Food imports
results.imports<-read.csv("Trends_fs_structure_filtered.15_Food imports.csv",header=T)
head(results.imports) ; nrow(results.imports) # 172 countries (1995 to 2015)
results.imports<-results.imports[,c(1,4,5)]   # just select the metrics needed for cluster analysis
names(results.imports)[2:3]<-c("imports.compound.chg.rate","imports.sd.ann.chng.rate")
names(country.look.up)
country.look.up.for.imports<-country.look.up[,c(1,5)]
unique(results.imports$country[!results.imports$country%in%country.look.up.for.imports$Food.import.export]) # different countries between the datasets (run before merging)
results.imports<-merge(results.imports,country.look.up.for.imports,by.x="country",by.y="Food.import.export")
head(results.imports); nrow(results.imports)   # 135 countries (lost 44 regions, but no countries)
results.imports<-results.imports[,c(4,2,3)]

# 7. Food exports
results.exports<-read.csv("Trends_fs_structure_filtered.15_Food exports.csv",header=T)
names(results.exports) ; nrow(results.exports) # 172 countries (1995 to 2015)
results.exports<-results.exports[,c(1,4,5)]   # just select the metrics needed for cluster analysis
names(results.exports)[2:3]<-c("exports.compound.chg.rate","exports.sd.ann.chng.rate")
country.look.up.for.exports<-country.look.up[,c(1,5)]
unique(results.exports$country[!results.exports$country%in%country.look.up.for.exports$Food.import.export]) # different countries between the datasets (run before merging)
results.exports<-merge(results.exports,country.look.up.for.exports,by.x="country",by.y="Food.import.export")
head(results.exports); nrow(results.exports)   # 133 countries (lost 44 regions, but no countries)
results.exports<-results.exports[,c(4,2,3)]

# 8. Producer Price Index, Agriculture
results.ppi<-read.csv("Trends_fs_structure_filtered.15_Producer Price Index, Agriculture.csv",header=T)
head(results.ppi); nrow(results.ppi) # 143 countries (1995 to 2015)
results.ppi<-results.ppi[,c(1,4,5)]   # just select the country and the metrics needed for cluster analysis
names(results.ppi)[2:3]<-c("ppi.compound.chg.rate","ppi.sd.ann.chng.rate")
names(country.look.up)
country.look.up.for.ppi<-country.look.up[,c(1,21)]
unique(results.ppi$country[!results.ppi$country%in%country.look.up.for.ppi$Prod.Food.Price]) # different countries between the datasets (run before merging)
results.ppi<-merge(results.ppi,country.look.up.for.ppi,by.x="country",by.y="Prod.Food.Price")
head(results.ppi); nrow(results.ppi)   # 141 countries (lost 2)
results.ppi<-results.ppi[,c(4,2,3)]

# Structure metrics:
# Key structure metrics:
head(results.ag.area)  ; nrow(results.ag.area) # 216 countries
head(results.synt.fert.use)  ; nrow(results.synt.fert.use) # 170 countries
head(results.ag.empl); nrow(results.ag.empl) # 186 countries
head(results.gao)  ; nrow(results.gao) # 208 countries
head(results.atfp)  ; nrow(results.atfp) # 170 countries
# Economic structure metrics (not included in the cluster analysis):
head(results.imports) ; nrow(results.imports) # 135 countries
head(results.exports) ; nrow(results.exports) # 133 countries
head(results.ppi); nrow(results.ppi) # 141 countries

### PART 2: Cluster analysis

# Merging of key structure metrics:
names(country.look.up)
country.look.up.list<-country.look.up[,c(1,2)]
all.results<-merge(country.look.up.list,results.ag.area, by.x=c("Common.name"), by.y=c("Common.name"), all.x=TRUE)
all.results<-all.results[,-2]
nrow(na.omit(all.results))
all.results<-merge(all.results,results.synt.fert.use, by.x=c("Common.name"), by.y=c("Common.name"), all.x=TRUE)
nrow(na.omit(all.results))
all.results<-merge(all.results,results.ag.empl, by.x=c("Common.name"), by.y=c("Common.name"), all.x=TRUE)
nrow(na.omit(all.results))
all.results<-merge(all.results,results.gao, by.x=c("Common.name"), by.y=c("Common.name"), all.x=TRUE)
nrow(na.omit(all.results))
all.results<-merge(all.results,results.atfp, by.x=c("Common.name"), by.y=c("Common.name"), all.x=TRUE)
nrow(na.omit(all.results))
head(all.results) ;nrow(all.results) 
names(all.results)
all.results<-na.omit(all.results) # remove rows with no data: 161 countries with existing values for all key structure metrics.
all.results<-all.results[,c(1,2,4,6,8,10)]  # select compound change rate only
row.names(all.results)<-all.results[,1]  # allocate country names as row names
all.results<-all.results[,-1]    # remove country name column

# Test for correlations across metrics:
cor(all.results, use="complete.obs")  # check correlation between the trend metrics.  No strong correlations- ok to use all 16 metrics
cor.all.results<-cor(all.results)
cor.all.results[cor.all.results>=0.8&cor.all.results<=0.99] # looking for strong correlations (between 0.8 and 0.99)

# Scale metrics CACR values by median and MAD (meadian avarage deviation)
library(quantable)
all.results.robsc<-robustscale(all.results, center = TRUE, scale = TRUE,preserveScale = FALSE)
all.results.robsc<-all.results.robsc$data # robust rescale the variables (zero median by mad)
summary(all.results.robsc)
boxplot(all.results.robsc, xaxt="n") # plot robust scaled variables
axis(side=1,at=seq(1,5,1),labels = c("Ag.area","Synth.fert","Ag.empl","GAO","ATFP"))
abline(h=0,lty=2)

# Cluster algorithm: How many groups to cluster into?
install.packages("NbClust")
library(NbClust)
?NbClust
NbClust(all.results.robsc,distance="euclidean",min.nc = 2, max.nc = 10, method = "ward.D2") # No. of clusters = 2
NbClust(all.results.robsc,distance="euclidean",min.nc = 2, max.nc = 15, method = "ward.D2") # No. of clusters = 2
NbClust(all.results.robsc,distance="euclidean",min.nc = 2, max.nc = 5, method = "ward.D2") # No. of clusters =  4
NbClust(all.results.robsc,distance="euclidean",min.nc = 2, max.nc = 6, method = "ward.D2") # No. of clusters =  4
# index="alllong"
NbClust(all.results.robsc,distance="euclidean",min.nc = 2, max.nc = 10, method = "ward.D2", index="alllong") # No. of clusters = 2
NbClust(all.results.robsc,distance="euclidean",min.nc = 2, max.nc = 15, method = "ward.D2", index="alllong") # No. of clusters = 2
NbClust(all.results.robsc,distance="euclidean",min.nc = 2, max.nc = 5, method = "ward.D2", index="alllong") # No. of clusters = 4
NbClust(all.results.robsc,distance="euclidean",min.nc = 2, max.nc = 6, method = "ward.D2", index="alllong") # No. of clusters = 4
d <- dist(all.results.robsc, method = "euclidean")   # calculate dissimilarity matrix for robust scaled metrics
# ?dist
cluster.ward<- hclust(d, method = "ward.D2") # Robust scaled metrics
# ?hclust

# Plot the dendogram and create country groups:
windows(14,10)
par(mfrow=c(1,1))
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\Dendrogram branches sub-analysis")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\Dendrogram branches sub-analysis")
# Robust scaled metrics, 2 clusters:
plot(cluster.ward, cex = 0.6,hang=-1,ylab="Height",xlab="Countries",sub="")  # Plot the obtained dendrogram for robust scaled metrics
rect.hclust(cluster.ward, k = 2, border = 2:6) # No. of clusters = 2
grp <- cutree(cluster.ward, k = 2)  # REMEMBER TO: groups into k number of clusters (supported by NbClust analysis)
table(grp)
rownames(all.results.robsc)[grp == 1]  # giving a list of all countries in group 1
rownames(all.results.robsc)[grp == 2]  # giving a list of all countries in group 2
df.grp<-data.frame(grp)
df.grp<-cbind(rownames(df.grp),data.frame(df.grp,row.names=NULL))
write.csv(df.grp,"Dendogram Country groups_robust scaled metrics_sub2.csv",row.names=F)
# Robust scaled metrics, 3 clusters:
plot(cluster.ward, cex = 0.6,hang=-1,ylab="Height",xlab="Countries",sub="")  # Plot the obtained dendrogram for robust scaled metrics
rect.hclust(cluster.ward, k = 3, border = 2:6) # No. of clusters = 3
grp.sub.3 <- cutree(cluster.ward, k = 3)  # REMEMBER TO: groups into k number of clusters (supported by NbClust analysis)
table(grp.sub.3)
rownames(all.results.robsc)[grp.sub.3 == 1]  # giving a list of all countries in group 1
rownames(all.results.robsc)[grp.sub.3 == 2]  # giving a list of all countries in group 2
rownames(all.results.robsc)[grp.sub.3 == 3]  # giving a list of all countries in group 3
df.grp.sub.3<-data.frame(grp.sub.3)
df.grp.sub.3<-cbind(rownames(df.grp.sub.3),data.frame(df.grp.sub.3,row.names=NULL))
write.csv(df.grp.sub.3,"Dendogram Country groups_robust scaled metrics_sub3.csv",row.names=F)
# Robust scaled metrics, 4 clusters:
plot(cluster.ward, cex = 0.6,hang=-1,ylab="Height",xlab="Countries",sub="")  # Plot the obtained dendrogram for robust scaled metrics
rect.hclust(cluster.ward, k = 4, border = 2:6) # No. of clusters = 4
grp.sub.4 <- cutree(cluster.ward, k = 4)  # REMEMBER TO: groups into k number of clusters (supported by NbClust analysis)
table(grp.sub.4)
rownames(all.results.robsc)[grp.sub.4 == 1]  # giving a list of all countries in group 1
rownames(all.results.robsc)[grp.sub.4 == 2]  # giving a list of all countries in group 2
rownames(all.results.robsc)[grp.sub.4 == 3]  # giving a list of all countries in group 3
rownames(all.results.robsc)[grp.sub.4 == 4]  # giving a list of all countries in group 4
df.grp.sub.4<-data.frame(grp.sub.4)
df.grp.sub.4<-cbind(rownames(df.grp.sub.4),data.frame(df.grp.sub.4,row.names=NULL))
write.csv(df.grp.sub.4,"Dendogram Country groups_robust scaled metrics_sub4.csv",row.names=F)
# Merge Robust scaled metrics clusters and sub-clusters in one data frame (2, 3, and 4 branches):
plot(cluster.ward, lwd=1.5, cex = 0.6,hang=-1,ylab="Height",xlab="Countries",sub="")
rect(0,17,161,23, col= rgb(213,94,0, maxColorValue = 255, alpha=60))
rect(0,22,20,23, col= rgb(213,94,0, maxColorValue = 255, alpha=90))
center.1 <- c(mean(c(0, 20)), mean(c(22, 23)))
text(center.1[1], center.1[2], labels = '2 Branches')
rect(0,14,161,17, col= rgb(0,114,178, maxColorValue = 255,alpha=126))
rect(0,16,20,17, col= rgb(0,114,178, maxColorValue = 255,alpha=145))
center.2 <- c(mean(c(0, 20)), mean(c(16, 17)))
text(center.2[1], center.2[2], labels = '3 Branches')
rect(0,0,161,14, col= rgb(0,158,115, maxColorValue = 255,alpha=50))
rect(0,13,20,14, col= rgb(0,158,115, maxColorValue = 255,alpha=90))
center.3 <- c(mean(c(0, 20)), mean(c(13, 14)))
text(center.3[1], center.3[2], labels = '4 Branches')
dev.print(tiff, "Dendrogram Country groups_all branches.tiff", res=600, height=7.6, width=13.3, units="in") # Generate high res image for the dendrogram
df.all.results.robsc<-cbind(df.grp,df.grp.sub.3,df.grp.sub.4)
df.all.results.robsc<-df.all.results.robsc[,c(1,2,4,6)]
names(df.all.results.robsc)[1]<-"Common.name"
write.csv(df.all.results.robsc,"Dendogram Country groups_robust scaled metrics_all merged.csv",row.names=F)

### PART 3: Hypothesis testing - key structure metrics
# Hypothesis testing for 2 and 4 clusters are available in the file "5_Dornelles.et.al_Cluster.algorithm_supplementary.R"

# How different are the clusters? Run ANOVA
all.results<-data.frame(all.results)  # convert back to simple dataframe
all.results$group<-as.vector(grp.sub.3)  # add in group as a columns from robust scaled cluster analysis (3 groups)
names(all.results)
shapiro.test(all.results$ag.area.compound.chg.rate) # Non-normal distribution
shapiro.test(all.results$synt.fert.use.compound.chg.rate) # # Non-normal distribution
shapiro.test(all.results$ag.empl.compound.chg.rate)# # Non-normal distribution
shapiro.test(all.results$gao.compound.chg.rate) # Normally distributed
shapiro.test(all.results$atfp.compound.chg.rate) #  Non-normal distribution
summary(all.results)
# Summary of groups:
library(dplyr)
g1.all.results<- all.results %>%
  filter(group == 1)
g2.all.results<- all.results %>%
  filter(group == 2)
g3.all.results<- all.results %>%
  filter(group == 3)
summary(g1.all.results)
summary(g2.all.results)
summary(g3.all.results)
# Agriculture area:
aov.output.ag.area<-aov(ag.area.compound.chg.rate~as.factor(group),data=all.results)
summary(aov.output.ag.area)
TukeyHSD(aov.output.ag.area)
oneway.test(ag.area.compound.chg.rate~as.factor(group),data=all.results) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results$ag.area.compound.chg.rate,as.factor(all.results$group),p.adj="holm")
# Synthetic fertilizer use:
aov.output.synt.fert<-aov(synt.fert.use.compound.chg.rate~as.factor(group),data=all.results)
summary(aov.output.synt.fert)
TukeyHSD(aov.output.synt.fert)
oneway.test(synt.fert.use.compound.chg.rate~as.factor(group),data=all.results) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results$synt.fert.use.compound.chg.rate,as.factor(all.results$group),p.adj="holm") # significance between groups
# Agricultural employment:
aov.output.ag.empl<-aov(ag.empl.compound.chg.rate~as.factor(group),data=all.results)
summary(aov.output.ag.empl)
TukeyHSD(aov.output.ag.empl)
oneway.test(ag.empl.compound.chg.rate~as.factor(group),data=all.results) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results$ag.empl.compound.chg.rate,as.factor(all.results$group),p.adj="holm") # significance between groups
# GAO
aov.output.gao<-aov(gao.compound.chg.rate~as.factor(group),data=all.results)
summary(aov.output.gao)
TukeyHSD(aov.output.gao)
oneway.test(gao.compound.chg.rate~as.factor(group),data=all.results) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results$gao.compound.chg.rate,as.factor(all.results$group),p.adj="holm") # significance between groups
# TFP
aov.output.atfp<-aov(atfp.compound.chg.rate~as.factor(group),data=all.results)
summary(aov.output.atfp)
TukeyHSD(aov.output.atfp)
oneway.test(atfp.compound.chg.rate~as.factor(group),data=all.results) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results$atfp.compound.chg.rate,as.factor(all.results$group),p.adj="holm") # significance between groups

### PART 4: Transformation archetypes - structure metrics
# Plotting of 2 and 4 clusters are available in the file "5_Dornelles.et.al_Cluster.algorithm_supplementary.R"
head(all.results) ;names(all.results)
all.results$Common.name<-row.names(all.results)
#write.csv(all.results,"Trends_fs_structure_All.results.csv",row.names=F)

# Plot the key strucutre metrics:
all.results<-read.csv("Trends_fs_structure_All.results.csv",header=T)
all.results$Common.name<-as.character(all.results$Common.name)
vermillioncb<-rgb(213,94,0, maxColorValue = 255)
skybluecb<-rgb(86,180,233, maxColorValue = 255)
greencb<-rgb(0,158,115, maxColorValue = 255)
bluecb<-rgb(0,114,178, maxColorValue = 255)

windows(32.5,18.6)
# par(mfrow=c(2,3))  # how many panels (2 rows and 3 columns)?
par(mfrow=c(2,4))
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
dev.print(tiff, "Structure.tiff", res=330, height=7.6, width=13.3, units="in") # Generate high res image after plotting all structure metrics
pdf("Structure_plots.pdf",height=7.6, width=13.3)
#run pdf(), run all plots, run dev.off()
dev.off()

# Agricultural area
plot.text <- with(all.results,boxplot(ag.area.compound.chg.rate~as.factor(group),notch=TRUE,main="Agricultural area",
                      ylab="1,000 hectares (% annual change)",xlab="Country group (no. of countries)",
                      col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-4,3,1))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=72)","")) # number of countires in each cluster.
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "c")
# Synthetic fertiliser use
plot.text <- with(all.results,boxplot(synt.fert.use.compound.chg.rate~as.factor(group),notch=TRUE,ylim=c(-10,16),
                      main="Synthetic Fertiliser use",ylab="Tonnes (% annual change)",
                      xlab="Country group (no. of countries)",col=c(vermillioncb,greencb,bluecb,skybluecb),
                      frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-15,20,5))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=72)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]-0.5), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "b")
# Agricultural employment
plot.text <- with(all.results,boxplot(ag.empl.compound.chg.rate~as.factor(group),notch=TRUE,main="Agricultural employment",
                      ylab="Percent of total employment (% annual change)",xlab="Country group (no. of countries)",
                      col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-8,4,2))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=72)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]+0.3), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "c")
# Gross Agricultural Output
plot.text <- with(all.results,boxplot(gao.compound.chg.rate~as.factor(group),notch=TRUE,main="Gross Agricultural Output",
                      ylab="Constant International US$, 2004-2006 (% annual change)",xlab="Country group (no. of countries)",
                      col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-4,10,2))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=72)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "c")
# Agricultural Total Productivity Index
plot.text <- with(all.results,boxplot(atfp.compound.chg.rate~as.factor(group),notch=TRUE,ylim=c(-6,6),
                      main="Agriculture Total Factor Productivity",ylab="Index (% annual change)",
                      xlab="Country group (no. of countries)",col=c(vermillioncb,greencb,bluecb,skybluecb),
                      frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-8,8,2))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=72)",""))
# 1 outlier: decreasing < -7 in group 1
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")

# Link the key structure metrics to the economic structure metrics: 
# Food imports
outcome.data<-results.imports
head(outcome.data) ;nrow(outcome.data) # 135 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes)# 117 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(imports.compound.chg.rate~as.factor(group),notch=TRUE,main="Food imports",
                               ylim=c(-5,25),ylab="International US$ (% annual change)",xlab="Country group (no. of countries)",
                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=117)",cex=0.7)
axis(side = 2, at=seq(-10,30,5))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=16)","2 (n=39)","3 (n=62)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")
# Food exports
outcome.data<-results.exports
head(outcome.data) ;nrow(outcome.data) # 133 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 116 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(exports.compound.chg.rate~as.factor(group),notch=TRUE,main="Food exports",
                               ylim=c(-14,25),ylab="International US$ (% annual change)",xlab="Country group (no. of countries)",
                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=116)",cex=0.7)
axis(side = 2, at=seq(-30,40,10))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=15)","2 (n=38)","3 (n=63)",""))
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "b")
# 1 outlier: increasing >40 in group 1
abline(h=0,lty=2)
# PPI, Agriculture
outcome.data<-results.ppi
head(outcome.data) ;nrow(outcome.data) # 141 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 128 countries 
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(ppi.compound.chg.rate~as.factor(group),notch=TRUE,main="Producer Price Index",
                               ylab="Index (% annual change)",xlab="Country group (no. of countries)",
                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=128)",cex=0.7)
axis(side = 2, at=seq(-5,40,5))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=20)","2 (n=40)","3 (n=68)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")
# Create boxes (and test coordinates with abline):
par(xpd = NA)
#abline(h=71, lty=2)
#abline(h=30.5, lty=2)
#abline(h=-10, lty=2)
#abline(v=4, lty=2)
#abline(v=-0.4, lty=2)
#abline(v=-4.65, lty=2)
#abline(v=-9, lty=2)
#abline(v=-13.5, lty=2)
#red.bg<-rgb(255,0,0, maxColorValue = 255, alpha=30)
#yellow.bg<-rgb(255,255,0, maxColorValue = 255, alpha=40)
#grey.bg<-rgb(96,96,96, maxColorValue = 255, alpha=30)
#rect(-9,-10,4,30.5, col= red.bg, border=FALSE)
#rect(-13.5,-10,-9,30.5, col= yellow.bg, border=FALSE)
#rect(-0.4,30.5,4,71, col= grey.bg, border=FALSE)
#legend
white.bg <-rgb(255,255,255, maxColorValue = 255)
rect(-13,-8,4,-5, col= white.bg, border=FALSE)
rect(-10.9,-7.5,-10.5,-5.5, col= vermillioncb, border=TRUE)
rect(-5.5,-7.5,-5.1,-5.5, col= greencb, border=TRUE)
rect(0,-7.5,0.4,-5.5, col= bluecb, border=TRUE)
text(x= -9.5, y= -6.5, cex=1.3, labels= "Rapidly expansionist")
text(x= -4.4, y= -6.5, cex=1.3, labels= "Expansionist")
text(x= 1.1, -6.5, cex=1.3, labels= "Consolidative")

### PART 5: Transformation archetypes - outcome metrics:
all.results$Common.name<-row.names(all.results)                      
head(all.results); nrow(all.results)

# windows(14,8) # Adjust dimensions to fit the 8 outcome metrics (e.g. windows(14,14)).
windows(33.24,19)
par(mfrow=c(2,4)) 
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
dev.print(tiff, "Outcomes.tiff", res=330, height=7.6, width=13.3, units="in") # Generate high res image after plotting all outcome metrics
pdf("Outcomes_plots.pdf",height=7.6, width=13.3)
#run pdf(), run all plots, run dev.off()
dev.off()

# Forest area
head(all.results); nrow(all.results)
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_Forest area.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 216 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) #161 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Forest Area",
                                  ylab="1,000 hectares (% annual change)",xlab="Country group (no. of countries)",
                                  col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-6,6,2))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=72)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "b")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes)) 

# Red List Index
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_Red List Index.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 195 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 145 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Red List Index",
                                  ylim=c(-1.5,0.5),ylab="Index (% annual change)",xlab="Country group (no. of countries)",
                                  col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=145)",cex=0.7)
axis(side = 2, at=seq(-2,1,0.5))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=22)","2 (n=59)","3 (n=64)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]+0.1), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes))

# Undernourishment
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_FAO Undernourishment.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 163 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 144 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Undernourishment",
                               ylim=c(-13,7),ylab="Prevalence (% annual change)",xlab="Country group (no. of countries)",
                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=144)",cex=0.7)
axis(side = 2, at=seq(-20,10,5))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=25)","2 (n=56)","3 (n=63)",""))
# 1 outlier: increasing >10 in group 2
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.4, y= max(plot.text$stats[,2]), labels= "a,b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "b")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes)) 

# Obesity
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_Adult obesity.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 189 countries
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 157 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Obesity",
                               ylim=c(0,8),ylab="Prevalence (% annual change)",xlab="Country group (no. of countries)",
                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE)) # ylim=c(x,y) is the scale of the y axis. Adjust if necessary
mtext("(n=157)",cex=0.7)
axis(side = 2, at=seq(-2,10,2))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=26)","2 (n=62)","3 (n=69)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "c")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes)) 

# GHG emmissions
# Total GHG emissions from Agriculture
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_CAIT Agriculture Total GHG Emissions.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 191 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 158 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Agricultural GHGE",
                               ylab="Megatons of CO2eq (% annual change)",xlab="Country group (no. of countries)",
                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=158)",cex=0.7)
axis(side = 2, at=seq(-10,10,5))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=25)","2 (n=63)","3 (n=70)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "c")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes))

# Total GHG emissions from Land-Use Change and Forestry
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_CAIT Land-Use Change and Forestry Total GHG Emissions.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 191 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 158 countries
nrow(na.omit(all.results.outcomes)) # 135 countries (CAGR can be NaN for some countries because the end and start values can change from positive to negative, or from negative to positve)
all.results.outcomes<-na.omit(all.results.outcomes) # 135 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Land-use change and Forestry GHGE",
                               ylim=c(-18,17),ylab="Megatons of CO2eq (% annual change)",xlab="Country group (no. of countries)",
                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=135)",cex=0.7)
axis(side = 2, at=seq(-25,20,5))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=25)","2 (n=55)","3 (n=55)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes))

# Total GHG from AFOLU (Agriculture + Forestry + Land use sources)
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_CAIT AFOLU Total GHG Emissions.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 191 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 158 countries
nrow(na.omit(all.results.outcomes)) # 137 countries (CAGR can be NaN for some countries because the end and start values can change from positive to negative, or from negative to positve)
all.results.outcomes<-na.omit(all.results.outcomes) # 137 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="AFOLU GHGE",
                               ylim=c(-16,15),ylab="Megatons of CO2eq (% annual change)",xlab="Country group (no. of countries)",
                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=137)",cex=0.7)
axis(side = 2, at=seq(-20,20,5))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=25)","2 (n=57)","3 (n=55)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes))

# NFA National Ecological Foodprint, Total Consumption (global hectares, gha)
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_NFA Total Consumption Footprint.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 176 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 152 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Ecological Footprint, Consumption",
                                  ylab="Global hectares (% annual change)",xlab="Country group (no. of countries)",
                                  col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=152)",cex=0.7)
axis(side = 2, at=seq(-6,12,2))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=25)","2 (n=60)","3 (n=67)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.4, y= max(plot.text$stats[,2]), labels= "a,b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "b")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes))
# Create boxes (and test coordinates with abline):
par(xpd = NA)
#abline(h=37, lty=2)
#abline(h=15, lty=2)
#abline(h=-7.5, lty=2)
#abline(v=4, lty=2)
#abline(v=-0.4, lty=2)
#abline(v=-4.65, lty=2)
#abline(v=-9, lty=2)
#abline(v=-13.5, lty=2)
#light.green.bg<-rgb(102,255,102, maxColorValue = 255, alpha=50)
#blue.bg<-rgb(0,114,178, maxColorValue = 255, alpha=30)
#green.bg<-rgb(0,158,115, maxColorValue = 255, alpha=35)
#orange.bg<-rgb(255,128,0, maxColorValue = 255, alpha=30)
#rect(-13.5,-7.5,4,15, col= orange.bg, border=FALSE)
#rect(-4.65,15,4,37, col= blue.bg, border=FALSE)
#rect(-13.5,15,-4.65,37, col= light.green.bg, border=FALSE)
#legend
white.bg <-rgb(255,255,255, maxColorValue = 255)
rect(-13,-8,4,-5, col= white.bg, border=FALSE)
rect(-10.9,-6.3,-10.5,-5.3, col= vermillioncb, border=TRUE)
rect(-5.5,-6.3,-5.1,-5.3, col= greencb, border=TRUE)
rect(0,-6.3,0.4,-5.3, col= bluecb, border=TRUE)
text(x= -9.5, y= -5.8, cex=1.3, labels= "Rapidly expansionist")
text(x= -4.4, y= -5.8, cex=1.3, labels= "Expansionist")
text(x= 1.1, y= -5.8, cex=1.3, labels= "Consolidative")

### PART 6: Hypothesis testing - economic structure metrics and outcome metrics.

## Economic metrics - trade and price
# Food imports
outcome.data<-results.imports
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes)# 117 countries (1995 - 2015)
shapiro.test(all.results.outcomes$imports.compound.chg.rate) # Non-normal distribution
aov.output.imports<-aov(imports.compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.imports)
TukeyHSD(aov.output.imports)
oneway.test(imports.compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$imports.compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

# Food exports
outcome.data<-results.exports
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes)# 116 countries (1995 - 2015)
shapiro.test(all.results.outcomes$exports.compound.chg.rate) # Non-normal distribution
aov.output.imports<-aov(exports.compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.imports)
TukeyHSD(aov.output.imports)
oneway.test(exports.compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$exports.compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

# PPI, Agriculture
outcome.data<-results.ppi
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes)# 128 countries (1995 - 2015)
shapiro.test(all.results.outcomes$ppi.compound.chg.rate) # Non-normal distribution
aov.output.ppi<-aov(ppi.compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.ppi)
TukeyHSD(aov.output.ppi)
oneway.test(ppi.compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$ppi.compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

# Outcome metrics
# Forest area
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_Forest area.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 216 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) #161 countries
shapiro.test(all.results.outcomes$compound.chg.rate) # Non-normal distribution
aov.output.for.area<-aov(compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.for.area)
TukeyHSD(aov.output.for.area)
oneway.test(compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

# Red List Index
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_Red List Index.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 195 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 145 countries
shapiro.test(all.results.outcomes$compound.chg.rate) # Non-normal distribution
aov.output.red.list<-aov(compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.red.list)
TukeyHSD(aov.output.red.list)
oneway.test(compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

# NFA National Ecological Foodprint, Total Consumption (global hectares, gha)
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_NFA Total Consumption Footprint.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 176 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 152 countries
shapiro.test(all.results.outcomes$compound.chg.rate) # Non-normal distribution
aov.output.nfa<-aov(compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.nfa)
TukeyHSD(aov.output.nfa)
oneway.test(compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

# Undernourishment
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_FAO Undernourishment.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 163 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 144 countries
shapiro.test(all.results.outcomes$compound.chg.rate) # Non-normal distribution
aov.output.undern<-aov(compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.undern)
TukeyHSD(aov.output.undern)
oneway.test(compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

# Obesity
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_Adult obesity.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 189 countries 
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 157 countries
shapiro.test(all.results.outcomes$compound.chg.rate) # Non-normal distribution
aov.output.obes<-aov(compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.obes)
TukeyHSD(aov.output.obes)
oneway.test(compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

# GHG emmissions

# Total GHG emissions from Agriculture
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_CAIT Agriculture Total GHG Emissions.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 191 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 158 countries
shapiro.test(all.results.outcomes$compound.chg.rate) # Non-normal distribution
aov.output.agr.ghg<-aov(compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.agr.ghg)
TukeyHSD(aov.output.agr.ghg)
oneway.test(compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

# Total GHG emissions from Land-Use Change and Forestry
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_CAIT Land-Use Change and Forestry Total GHG Emissions.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 191 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 158 countries
shapiro.test(all.results.outcomes$compound.chg.rate) # Non-normal distribution
aov.output.land.for.ghg<-aov(compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.land.for.ghg)
TukeyHSD(aov.output.land.for.ghg)
oneway.test(compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

# Total GHG from AFOLU (Agriculture + Forestry + Land use sources)
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_CAIT AFOLU Total GHG Emissions.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 191 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 158 countries
shapiro.test(all.results.outcomes$compound.chg.rate) # Non-normal distribution
aov.output.afolu.ghg<-aov(compound.chg.rate~as.factor(group),data=all.results.outcomes)
summary(aov.output.afolu.ghg)
TukeyHSD(aov.output.afolu.ghg)
oneway.test(compound.chg.rate~as.factor(group),data=all.results.outcomes) # For variances not necessarily assumed to be equal
pairwise.t.test(all.results.outcomes$compound.chg.rate,as.factor(all.results.outcomes$group),p.adj="holm")

### PART 7: World map by Country Groups (Transformation Archetypes):
all.results<-read.csv("Trends_fs_structure_All.results.csv",header=T)
all.results$Common.name<-as.character(all.results$Common.name)
world.map<-data.frame(all.results)  # convert to simple dataframe
#world.map$group<-as.vector(grp.sub.3)  # add in group as a columns from robust scaled cluster analysis (3 groups)
#world.map$Common.name<-row.names(world.map)
install.packages("ggmap")
install.packages("maps")
library(maps)
library(ggmap)
library(ggplot2)
library(dplyr)
WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
unique(world.map$Common.name[!world.map$Common.name%in%WorldData$region]) # different countries between the datasets (run before merging)
unique(WorldData$region)
world.map$Common.name[world.map$Common.name=="Brunei Darussalam"]<-"Brunei"
world.map$Common.name[world.map$Common.name=="Cabo Verde"]<-"Cape Verde"
world.map$Common.name[world.map$Common.name=="Congo"]<-"Republic of Congo"
world.map$Common.name[world.map$Common.name=="Congo, Dem. Rep."]<-"Democratic Republic of the Congo"
world.map$Common.name[world.map$Common.name=="Cote d'Ivoire"]<-"Ivory Coast"
world.map$Common.name[world.map$Common.name=="Korea, Dem. People's Rep."]<-"North Korea"
world.map$Common.name[world.map$Common.name=="Korea, Rep."]<-"South Korea"
world.map$Common.name[world.map$Common.name=="Kyrgyz Republic"]<-"Kyrgyzstan"
world.map$Common.name[world.map$Common.name=="Russian Federation"]<-"Russia"
world.map$Common.name[world.map$Common.name=="Trinidad and Tobago"]<-"Trinidad"
world.map$Common.name[world.map$Common.name=="United Kingdom"]<-"UK"
world.map$Common.name[world.map$Common.name=="United States"]<-"USA"
vermillioncb<-rgb(213,94,0, maxColorValue = 255)
skybluecb<-rgb(86,180,233, maxColorValue = 255)
greencb<-rgb(0,158,115, maxColorValue = 255)
bluecb<-rgb(0,114,178, maxColorValue = 255)
group.colours<-c("1"=vermillioncb,"2"=greencb,"3"=bluecb)
ta.colours<-c("Rapidly expansionist"=vermillioncb,"Expansionist"=greencb,"Consolidative"=bluecb)
world.map$group[world.map$group=="1"]<-"Rapidly expansionist"
world.map$group[world.map$group=="2"]<-"Expansionist"
world.map$group[world.map$group=="3"]<-"Consolidative"
# Plot the groups:
windows(14,8)
par(mfrow=c(1,1))
fsdata <- ggplot() + 
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "gray", colour = "#999999", size=0.25) +
  geom_map(data = world.map, map=WorldData,
           aes(fill=as.factor(group), map_id=Common.name),
           colour="#999999", size=0.25) +
  scale_fill_manual(values=ta.colours) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="Transformation archetypes", title="Transformation archetypes in global food systems", x="", y="") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold")) +
  theme(legend.position = c(0.14, 0.1), legend.title = element_text(size=17,face = "bold"),
        legend.text = element_text(size = 15))
fsdata
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
dev.print(tiff, "World map_country groups_final.tiff", res=330, height=7.6, width=13.3, units="in") # Generate high res image for the dendrogram
pdf("World map_final.pdf",height=7.6, width=13.3)
fsdata
dev.off()

### PART 8: Carrying capacity function: agricultural extent
all.results<-read.csv("Trends_fs_structure_All.results.csv",header=T)
agric.exten<-read.csv("Trends_fs_Agricultural extension_3 clust.csv",header=T) # Upload the carrying capacity groups by no. of clusters indicated by the cluster algorithm
names(agric.exten); nrow(agric.exten) # 215 countries (from 1995 to 2015)
names(agric.exten)[5]<-"agr.ext.grp"
unique(all.results$Common.name[!all.results$Common.name%in%agric.exten$Common.name]) # "Swaziland" is not in the agric.ext file
all.results.agric.exten<-merge(all.results,agric.exten,by.x="Common.name",by.y="Common.name")
library(dplyr)
agr.ext.1<-all.results.agric.exten %>%
                       filter(agr.ext.grp==1)
min(agr.ext.1$ag.exten.median.start.values)
max(agr.ext.1$ag.exten.median.start.values)
IQR(agr.ext.1$ag.exten.median.start.values)
agr.ext.2<-all.results.agric.exten %>%
                      filter(agr.ext.grp==2)
min(agr.ext.2$ag.exten.median.start.values)
max(agr.ext.2$ag.exten.median.start.values)
IQR(agr.ext.2$ag.exten.median.start.values)
agr.ext.3<-all.results.agric.exten %>%
                      filter(agr.ext.grp==3)
min(agr.ext.3$ag.exten.median.start.values)
max(agr.ext.3$ag.exten.median.start.values)
IQR(agr.ext.3$ag.exten.median.start.values)
# Plots
windows(14,10)
par(mfrow=c(1,1))
names(all.results.agric.exten)
# Groups of agricultural extent:
with(all.results.agric.exten,plot(ag.exten.median.start.values~as.factor(agr.ext.grp),notch=TRUE,
                                  main="Groups of Agricultural extent",
                                  ylab="Percentage of agricultural extent (median start year)",
                                  xlab="Group (no. of countries)",
                                  col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=160)",cex=0.7)
axis(side = 2, at=seq(-20,100,20))
table(all.results.agric.exten$agr.ext.grp)
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=48)","2 (n=51)","3 (n=61)","")) # number of countries in each cluster.
#dev.print(tiff, "Groups_Agric_extent.tiff", res=150, height=7.6, width=9.5, units="in") # Generate high res image after plotting all structure metrics
# Agriculutrual extent by Country group:
plot.text <- with(all.results.agric.exten,boxplot(ag.exten.median.start.values~as.factor(group),notch=TRUE,
                                  main="Agricultural extent",
                                  ylab="Percentage of agricultural extent (median start year)",
                                  xlab="Country group (no. of countries)",
                                  col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=160)",cex=0.7)
axis(side = 2, at=seq(-20,100,20))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=26)","2 (n=62)","3 (n=72)","")) # number of countires in each cluster.
#dev.print(tiff, "Agric_extent.tiff", res=150, height=7.6, width=9.5, units="in") # Generate high res image after plotting all structure metrics
# Composition of 'Groups of Agricultural extent' by 'Country Group (Transformation Archetypes)'
df <- all.results.agric.exten
library(dplyr)
test.90<-as.numeric (df %>%
                       filter(group==1) %>%
                       filter(agr.ext.grp==1) %>%
                       tally) # tally computes the number of observations following the filter applied
test.80<-as.numeric (df %>%
                       filter(group==2) %>%
                       filter(agr.ext.grp==1) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(group==3) %>%
                      filter(agr.ext.grp==1) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
test.90<-as.numeric(df %>% 
                      filter(group==1) %>%
                      filter(agr.ext.grp==2) %>%
                      tally)
test.80<-as.numeric (df %>%
                       filter(group==2) %>%
                       filter(agr.ext.grp==2) %>%
                       tally)
test.70<-as.numeric (df %>%
                       filter(group==3) %>%
                       filter(agr.ext.grp==2) %>%
                       tally)
dur.table.temp<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(test.table,dur.table.temp)
test.90<-as.numeric (df %>%
                       filter(group==1) %>%
                       filter(agr.ext.grp==3) %>%
                       tally)
test.80<-as.numeric (df %>%
                       filter(group==2) %>%
                       filter(agr.ext.grp==3) %>%
                       tally)
test.70<-as.numeric(df %>%
                      filter(group==3) %>%
                      filter(agr.ext.grp==3) %>%
                      tally)
test.table<-data.frame(test.90,test.80,test.70)
duration.table.temp<-rbind(duration.table.temp, test.table)
duration.table.temp$agric.extension<-c("1","2","3")
duration.table.temp<-duration.table.temp[c(4,1:3)]
names(duration.table.temp)[1]<-"Agricultural extension"
names(duration.table.temp)[2]<-"Archetype 1"
names(duration.table.temp)[3]<-"Archetype 2"
names(duration.table.temp)[4]<-"Archetype 3"
head(duration.table.temp)
write.csv(duration.table.temp,"Transf_Archetypes_groups_by_Agric_Extens.csv",row.names=F)