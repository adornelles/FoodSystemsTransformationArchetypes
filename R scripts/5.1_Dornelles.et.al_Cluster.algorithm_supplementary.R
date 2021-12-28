# Run this script after part 2 of script "5_Dornelles.et.al_Cluster.algorithm.R"

# Country look up table
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Country look up table")
country.look.up<-read.csv("Country name look up table_updated.csv",header=T)
names(country.look.up)

# Load in datasets
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")

# Upload all.results
all.results<-read.csv("Trends_fs_structure_All.results.csv",header=T)
all.results$Common.name<-as.character(all.results$Common.name)

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

# Food exports
results.exports<-read.csv("Trends_fs_structure_filtered.15_Food exports.csv",header=T)
names(results.exports) ; nrow(results.exports) # 172 countries (1995 to 2015)
results.exports<-results.exports[,c(1,4,5)]   # just select the metrics needed for cluster analysis
names(results.exports)[2:3]<-c("exports.compound.chg.rate","exports.sd.ann.chng.rate")
country.look.up.for.exports<-country.look.up[,c(1,5)]
unique(results.exports$country[!results.exports$country%in%country.look.up.for.exports$Food.import.export]) # different countries between the datasets (run before merging)
results.exports<-merge(results.exports,country.look.up.for.exports,by.x="country",by.y="Food.import.export")
head(results.exports); nrow(results.exports)   # 133 countries (lost 44 regions, but no countries)
results.exports<-results.exports[,c(4,2,3)]

# Producer Price Index, Agriculture
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

# Part 1 - 2 clusters:
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\Dendrogram branches sub-analysis")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\Dendrogram branches sub-analysis")
df.grp<-read.csv("Dendogram Country groups_robust scaled metrics_sub2.csv",header=T)
names(df.grp)[1] <- "Common.name"
grp<-df.grp$grp
all.results$group<-as.vector(grp) # add in group as a columns from robust scaled cluster analysis  (2 groups)
shapiro.test(all.results$ag.area.compound.chg.rate) # Non-normal distribution
shapiro.test(all.results$synt.fert.use.compound.chg.rate) # # Non-normal distribution
shapiro.test(all.results$ag.empl.compound.chg.rate)# # Non-normal distribution
shapiro.test(all.results$gao.compound.chg.rate) # Normally distributed
shapiro.test(all.results$atfp.compound.chg.rate) #  Non-normal distribution
summary(all.results)
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

head(all.results) ;names(all.results)  
windows(32.5,18.6)
par(mfrow=c(2,4))
vermillioncb<-rgb(213,94,0, maxColorValue = 255)
skybluecb<-rgb(86,180,233, maxColorValue = 255)
greencb<-rgb(0,158,115, maxColorValue = 255)
bluecb<-rgb(0,114,178, maxColorValue = 255)
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
dev.print(tiff, "Structure_2clusters.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image after plotting all structure metrics
pdf("Structure_2clusters.pdf",height=7.6, width=13.3)
#run pdf(), run all plots, run dev.off()
dev.off()

# Agric. area:
plot.text <- with(all.results,boxplot(ag.area.compound.chg.rate~as.factor(group),notch=TRUE,main="Agricultural area",
                      ylab="1,000 hectares (% annual change)",xlab="Country group (no. of countries)",
                      col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-4,3,1))
table(grp)
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=89)","2 (n=72)","")) # number of countires in each cluster.
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
# Synt fert use:
plot.text <- with(all.results,boxplot(synt.fert.use.compound.chg.rate~as.factor(group),notch=TRUE,ylim=c(-10,16),
                      main="Synthetic Fertiliser use",ylab="Tonnes (% annual change)",
                      xlab="Country group (no. of countries)",col=c(vermillioncb,bluecb,skybluecb),
                      frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-15,20,5))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=89)","2 (n=72)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
# Agric. employment:
plot.text <- with(all.results,boxplot(ag.empl.compound.chg.rate~as.factor(group),notch=TRUE,
                                   main="Agricultural employment",ylab="Percent of total employment (% annual change)",
                                   xlab="Country group (no. of countries)",col=c(vermillioncb,bluecb,skybluecb),
                                   frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-8,4,2))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=89)","2 (n=72)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]+0.4), labels= "b")
# GAO
plot.text <- with(all.results,boxplot(gao.compound.chg.rate~as.factor(group),notch=TRUE,main="Gross Agricultural Output",
                                   ylab="Constant International US$, 2004-2006 (% annual change)",xlab="Country group (no. of countries)",
                                   col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-4,10,2))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=89)","2 (n=72)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]-0.25), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
## TFP
plot.text <- with(all.results,boxplot(atfp.compound.chg.rate~as.factor(group),notch=TRUE,ylim=c(-8,6),
                                   main="Agriculture Total Factor Productivity",ylab="Index (% annual change)",
                                   xlab="Country group (no. of countries)",col=c(vermillioncb,bluecb,skybluecb),
                                   frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-10,8,2))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=89)","2 (n=72)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
# Food imports
outcome.data<-results.imports
head(outcome.data) ;nrow(outcome.data) # 135 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes)# 117 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(imports.compound.chg.rate~as.factor(group),notch=TRUE,main="Food imports",ylim=c(-5,25),
                               ylab="International US$ (% annual change)",xlab="Country group (no. of countries)",
                               col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=117)",cex=0.7)
axis(side = 2, at=seq(-10,30,5))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=55)","2 (n=62)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
# Food exports
outcome.data<-results.exports
head(outcome.data) ;nrow(outcome.data) # 133 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 116 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(exports.compound.chg.rate~as.factor(group),notch=TRUE,main="Food exports",
                                            ylim=c(-14,25),ylab="International US$ (% annual change)",xlab="Country group (no. of countries)",
                                            col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=116)",cex=0.7)
axis(side = 2, at=seq(-30,40,10))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=53)","2 (n=63)",""))
# 1 outlier: increasing >40 in group 1
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
# PPI, Agriculture
outcome.data<-results.ppi
head(outcome.data) ;nrow(outcome.data) # 141 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 128 countries 
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(ppi.compound.chg.rate~as.factor(group),notch=TRUE,main="Producer Price Index",
                                            ylab="Index (% annual change)",xlab="Country group (no. of countries)",
                                            col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=128)",cex=0.7)
axis(side = 2, at=seq(-5,40,5))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=60)","2 (n=68)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
par(xpd = NA)
#abline(h=71, lty=2)
#abline(h=30.5, lty=2)
#abline(h=-10, lty=2)
#abline(v=3, lty=2)
#abline(v=-0.1, lty=2)
#abline(v=-3.0, lty=2)
#abline(v=-5.8, lty=2)
#abline(v=-8.8, lty=2)
#red.bg<-rgb(255,0,0, maxColorValue = 255, alpha=30)
#yellow.bg<-rgb(255,255,0, maxColorValue = 255, alpha=40)
#grey.bg<-rgb(96,96,96, maxColorValue = 255, alpha=30)
#rect(-5.8,-10,3,30.5, col= red.bg, border=FALSE)
#rect(-8.8,-10,-5.8,30.5, col= yellow.bg, border=FALSE)
#rect(-0.1,30.5,3,71, col= grey.bg, border=FALSE)
white.bg <-rgb(255,255,255, maxColorValue = 255)
rect(-13,-8,4,-5, col= white.bg, border=FALSE)
rect(-5.9,-7.5,-5.5,-5.5, col= vermillioncb, border=TRUE)
rect(-1.5,-7.5,-1.1,-5.5, col= bluecb, border=TRUE)
text(x= -5.1, y= -6.5, cex=1.3, labels= "Expansionist")
text(x= -0.7, -6.5, cex=1.3, labels= "Consolidative")

### Transformation archetypes - outcome metrics:
windows(32.5,18.6)
par(mfrow=c(2,4))
vermillioncb<-rgb(213,94,0, maxColorValue = 255)
skybluecb<-rgb(86,180,233, maxColorValue = 255)
greencb<-rgb(0,158,115, maxColorValue = 255)
bluecb<-rgb(0,114,178, maxColorValue = 255)
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
dev.print(tiff, "Outcomes_2clusters.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image after plotting all structure metrics
pdf("Outcomes_2clusters.pdf",height=7.6, width=13.3)
#run pdf(), run all plots, run dev.off()
dev.off()

# Forest area
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")
head(all.results); nrow(all.results)
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_Forest area.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 216 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) #161 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Forest Area",
                                               ylab="1,000 hectares (% annual change)",xlab="Country group (no. of countries)",
                                               col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-6,6,2))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=89)","2 (n=72)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes)) 

# Red List Index
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_Red List Index.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 195 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 145 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Red List Index",
                                               ylim=c(-1.5,0.5),ylab="Index (% annual change)",xlab="Country group (no. of countries)",
                                               col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=145)",cex=0.7)
axis(side = 2, at=seq(-2,1,0.5))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=81)","2 (n=64)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes))

# Undernourishment
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_FAO Undernourishment.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 163 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 144 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Undernourishment",
                                            ylim=c(-13,7),ylab="Prevalence (% annual change)",xlab="Country group (no. of countries)",
                                            col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=144)",cex=0.7)
axis(side = 2, at=seq(-20,10,5))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=81)","2 (n=63)",""))
# 1 outlier: increasing >10 in group 1
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes)) 

# Obesity
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_Adult obesity.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 189 countries
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 157 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Obesity",ylim=c(0,8),
                                               ylab="Prevalence (% annual change)",xlab="Country group (no. of countries)",
                                               col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE)) # ylim=c(x,y) is the scale of the y axis. Adjust if necessary
mtext("(n=157)",cex=0.7)
axis(side = 2, at=seq(-2,10,2))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=88)","2 (n=69)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
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
                                            col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=158)",cex=0.7)
axis(side = 2, at=seq(-10,10,5))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=88)","2 (n=70)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
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
                                               col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=135)",cex=0.7)
axis(side = 2, at=seq(-25,20,5))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=80)","2 (n=55)",""))
# 3 outlierS: 2 decreasing <-18 and 1 increasing >17, all in group 1 
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes))

# Total GHG from AFOLU (Agriculture + Forestry + Land use sources)
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_CAIT AFOLU Total GHG Emissions.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 191 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 158 countries
nrow(na.omit(all.results.outcomes)) # 137 countries (CAGR can be NaN for some countries because the end and start values can change from positive to negative, or from negative to positve)
all.results.outcomes<-na.omit(all.results.outcomes) # 137 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="AFOLU GHGE",ylim=c(-15,15),
                                            ylab="Megatons of CO2eq (% annual change)",xlab="Country group (no. of countries)",
                                            col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=137)",cex=0.7)
axis(side = 2, at=seq(-20,20,5))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=82)","2 (n=55)",""))
# 2 outliers: 1 decreasing <-15 in group 1 and 1 increasing >15 in group 2 
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes))

# NFA National Ecological Foodprint, Total Consumption (global hectares, gha)
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_NFA Total Consumption Footprint.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 176 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 152 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Ecological Footprint, Consumption",
                                               ylab="Global hectares (% annual change)",xlab="Country group (no. of countries)",
                                               col=c(vermillioncb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=152)",cex=0.7)
axis(side = 2, at=seq(-6,12,2))
axis(side = 1, at=seq(0,3,1), labels = c("","1 (n=85)","2 (n=67)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes)) 
par(xpd = NA)
#abline(h=37, lty=2)
#abline(h=15, lty=2)
#abline(h=-7.5, lty=2)
#abline(v=3, lty=2)
#abline(v=-0.1, lty=2)
#abline(v=-3.0, lty=2)
#abline(v=-5.8, lty=2)
#abline(v=-8.8, lty=2)
#light.green.bg<-rgb(102,255,102, maxColorValue = 255, alpha=50)
#blue.bg<-rgb(0,114,178, maxColorValue = 255, alpha=30)
#green.bg<-rgb(0,158,115, maxColorValue = 255, alpha=35)
#orange.bg<-rgb(255,128,0, maxColorValue = 255, alpha=30)
#rect(-8.8,-7.5,3,15, col= orange.bg, border=FALSE)
#rect(-3,15,3,37, col= blue.bg, border=FALSE)
#rect(-8.8,15,-3,37, col= light.green.bg, border=FALSE)
white.bg <-rgb(255,255,255, maxColorValue = 255)
rect(-13,-8,4,-5, col= white.bg, border=FALSE)
rect(-5.9,-6.5,-5.5,-5, col= vermillioncb, border=TRUE)
rect(-1.5,-6.5,-1.1,-5, col= bluecb, border=TRUE)
text(x= -5.1, y= -5.75, cex=1.3, labels= "Expansionist")
text(x= -0.7, -5.75, cex=1.3, labels= "Consolidative")

# Hypothesis testing - economic structure metrics and outcome metrics.

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

#------------------------------------------------------------------------------------------------------------

# Part 2 - 4 clusters:
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\Dendrogram branches sub-analysis")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results\\Dendrogram branches sub-analysis")
df.grp.4 <- read.csv("Dendogram Country groups_robust scaled metrics_sub4.csv",header=T)
names(df.grp.4)[1] <- "Common.name"
grp.sub.4<-df.grp.4$grp
all.results$group<-as.vector(grp.sub.4) # add in group as a columns from robust scaled cluster analysis  (4 groups)
shapiro.test(all.results$ag.area.compound.chg.rate) # Non-normal distribution
shapiro.test(all.results$synt.fert.use.compound.chg.rate) # # Non-normal distribution
shapiro.test(all.results$ag.empl.compound.chg.rate)# # Non-normal distribution
shapiro.test(all.results$gao.compound.chg.rate) # Normally distributed
shapiro.test(all.results$atfp.compound.chg.rate) #  Non-normal distribution
summary(all.results)
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

windows(32.5,18.6)
par(mfrow=c(2,4))
vermillioncb<-rgb(213,94,0, maxColorValue = 255)
skybluecb<-rgb(86,180,233, maxColorValue = 255)
greencb<-rgb(0,158,115, maxColorValue = 255)
bluecb<-rgb(0,114,178, maxColorValue = 255)
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
dev.print(tiff, "Structure_4clusters.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image after plotting all structure metrics
pdf("Structure_4clusters.pdf",height=7.6, width=13.3)
#run pdf(), run all plots, run dev.off()
dev.off()

# Plot the key strucutre metrics:
table(grp.sub.4)
# Agric area:
plot.text <- with(all.results,boxplot(ag.area.compound.chg.rate~as.factor(group),notch=TRUE,main="Agricultural area",ylab="1,000 hectares (% annual change)",
                      xlab="Country group (no. of countries)",col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-4,3,1))
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=71)","4 (n=1)","")) # number of countires in each cluster.
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "c")
text(x= 4.3, y= max(plot.text$stats[,4]+0.4), labels= "d")
# Synthetic fertiliser
plot.text <- with(all.results,boxplot(synt.fert.use.compound.chg.rate~as.factor(group),notch=TRUE,ylim=c(-10,16),
                                      main="Synthetic Fertiliser use",ylab="Tonnes (% annual change)",xlab="Country group (no. of countries)",
                                      col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-15,20,5))
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=71)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]-1), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "b")
text(x= 4.3, y= max(plot.text$stats[,4]-1), labels= "a")
# Agric employment:
plot.text <- with(all.results,boxplot(ag.empl.compound.chg.rate~as.factor(group),notch=TRUE,main="Agricultural employment",
                                      ylab="Percent of total employment (% annual change)",xlab="Country group (no. of countries)",
                                      col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-8,4,2))
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=71)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]+0.3), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]+0.1), labels= "c")
text(x= 4.3, y= max(plot.text$stats[,4]-0.2), labels= "a,b,c")
# GAO
plot.text <- with(all.results,boxplot(gao.compound.chg.rate~as.factor(group),notch=TRUE,main="Gross Agricultural Output",
                                      ylab="Constant International US$, 2004-2006 (% annual change)",xlab="Country group (no. of countries)",
                                      col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-4,10,2))
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=71)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "c")
text(x= 4.3, y= max(plot.text$stats[,4]+0.3), labels= "c")
# TFP
plot.text <- with(all.results,boxplot(atfp.compound.chg.rate~as.factor(group),notch=TRUE,ylim=c(-8,6),main="Agriculture Total Factor Productivity",
                                   ylab="Index (% annual change)",xlab="Country group (no. of countries)",
                                   col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=161)",cex=0.7)
axis(side = 2, at=seq(-10,8,2))
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=71)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")
text(x= 4.3, y= max(plot.text$stats[,4]+0.4), labels= "b")

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
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=16)","2 (n=39)","3 (n=61)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")
text(x= 4.3, y= max(plot.text$stats[,4]+1), labels= "a")

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
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=15)","2 (n=38)","3 (n=62)","4 (n=1)",""))
# 1 outlier: increasing >40 in group 1
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.35, y= max(plot.text$stats[,2]), labels= "a,b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "b")
text(x= 4.3, y= max(plot.text$stats[,4]+1), labels= "a,b")

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
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=20)","2 (n=40)","3 (n=67)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")
text(x= 4.3, y= max(plot.text$stats[,4]+1), labels= "a")
par(xpd = NA)
#abline(h=71, lty=2)
#abline(h=30.5, lty=2)
#abline(h=-10, lty=2)
#abline(v=5.5, lty=2)
#abline(v=-0.6, lty=2)
#abline(v=-6.3, lty=2)
#abline(v=-12.05, lty=2)
#abline(v=-17.8, lty=2)
#red.bg<-rgb(255,0,0, maxColorValue = 255, alpha=30)
#yellow.bg<-rgb(255,255,0, maxColorValue = 255, alpha=40)
#grey.bg<-rgb(96,96,96, maxColorValue = 255, alpha=30)
#rect(-12.05,-10,5.5,30.5, col= red.bg, border=FALSE)
#rect(-17.8,-10,-12.05,30.5, col= yellow.bg, border=FALSE)
#rect(-0.6,30.5,5.5,71, col= grey.bg, border=FALSE)
white.bg <-rgb(255,255,255, maxColorValue = 255)
black.bg <-rgb(0,0,0, maxColorValue = 255)
rect(-20,-8,4,-5, col= white.bg, border=FALSE)
rect(-15,-7.5,-14.5,-5.5, col= vermillioncb, border=TRUE)
rect(-10,-7.5,-9.5,-5.5, col= greencb, border=TRUE)
rect(-5,-7.5,-4.5,-5.5, col= bluecb, border=TRUE)
rect(0,-7.5,0.5,-5.5, col= black.bg, border=TRUE)
text(x= -13.3, y= -6.5, cex=1.3, labels= "Rapidly expansionist")
text(x= -8.75, y= -6.5, cex=1.3, labels= "Expansionist")
text(x= -3.75, -6.5, cex=1.3, labels= "Consolidative")
text(x= 1.7, -6.5, cex=1.3, labels= "Rapidly consolidative")

### Transformation archetypes - outcome metrics:
all.results$Common.name<-row.names(all.results)                      
head(all.results); nrow(all.results)

#windows(33.24,19)
#par(mfrow=c(2,4)) 
#dev.print(tiff, "Outcomes_4clusters.tiff", res=600, height=7.6, width=13.3, units="in") # Generate high res image after plotting all outcome metrics

windows(32.5,18.6)
#windows(32.5,24,19)
par(mfrow=c(2,4))
vermillioncb<-rgb(213,94,0, maxColorValue = 255)
skybluecb<-rgb(86,180,233, maxColorValue = 255)
greencb<-rgb(0,158,115, maxColorValue = 255)
bluecb<-rgb(0,114,178, maxColorValue = 255)
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Images")
dev.print(tiff, "Outcomes_4clusters.tiff", res=150, height=7.6, width=13.3, units="in") # Generate high res image after plotting all structure metrics
pdf("Outcomes_4clusters.pdf",height=7.6, width=13.3)
#run pdf(), run all plots, run dev.off()
dev.off()

# Forest area
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")
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
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=71)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "b")
text(x= 4.3, y= max(plot.text$stats[,4]+0.3), labels= "a,b")
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
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=22)","2 (n=59)","3 (n=63)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]+0.1), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")
text(x= 4.3, y= max(plot.text$stats[,4]+0.1), labels= "a")
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
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=25)","2 (n=63)","3 (n=63)",""))
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
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Obesity",ylim=c(0,8),
                                               ylab="Prevalence (% annual change)",xlab="Country group (no. of countries)",
                                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE)) # ylim=c(x,y) is the scale of the y axis. Adjust if necessary
mtext("(n=157)",cex=0.7)
axis(side = 2, at=seq(-2,10,2))
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=26)","2 (n=62)","3 (n=68)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "c")
text(x= 4.3, y= max(plot.text$stats[,4]+0.3), labels= "a,b,c")
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
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=25)","2 (n=63)","3 (n=69)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "c")
text(x= 4.3, y= max(plot.text$stats[,4]+0.5), labels= "a,b,c")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes))

# Total GHG emissions from Land-Use Change and Forestry
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_CAIT Land-Use Change and Forestry Total GHG Emissions.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 191 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 158 countries
nrow(na.omit(all.results.outcomes)) # 135 countries
all.results.outcomes<-na.omit(all.results.outcomes)
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Land-use change and Forestry GHGE",
                                            ylim=c(-18,17),ylab="Megatons of CO2eq (% annual change)",xlab="Country group (no. of countries)",
                                            col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=135)",cex=0.7)
axis(side = 2, at=seq(-25,20,5))
axis(side = 1, at=seq(0,4,1), labels = c("","1 (n=25)","2 (n=55)","3 (n=55)",""))
# 3 outlierS: 2 decreasing <-18 and 1 increasing >17, all in group 2 
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
nrow(na.omit(all.results.outcomes)) # 137 countries
all.results.outcomes<-na.omit(all.results.outcomes)
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="AFOLU GHGE",ylim=c(-15,15),
                                               ylab="Megatons of CO2eq (% annual change)",xlab="Country group (no. of countries)",
                                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=137)",cex=0.7)
axis(side = 2, at=seq(-20,20,5))
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=25)","2 (n=57)","3 (n=54)","4 (n=1)",""))
# 3 outliers: 1 decreasing <-15 in group 2 and 1 increasing >15 in group 3 
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.3, y= max(plot.text$stats[,2]), labels= "a")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "a")
text(x= 4.3, y= max(plot.text$stats[,4]+1), labels= "a")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes))

# NFA National Ecological Foodprint, Total Consumption (global hectares, gha)
outcome.data<-read.csv("Trends_fs_outcomes_filtered.15_NFA Total Consumption Footprint.csv",header=T)
head(outcome.data) ;nrow(outcome.data) # 176 countries (1995 - 2015)
all.results.outcomes<-merge(all.results,outcome.data,by.x="Common.name",by.y="Common.name")
head(all.results.outcomes); nrow(all.results.outcomes) # 158 countries
table(all.results.outcomes$group)
plot.text <- with(all.results.outcomes,boxplot(compound.chg.rate~as.factor(group),notch=TRUE,main="Ecological Footprint, Consumption",
                                               ylab="Global hectares (% annual change)",xlab="Country group (no. of countries)",
                                               col=c(vermillioncb,greencb,bluecb,skybluecb),frame.plot = FALSE,axes = FALSE))
mtext("(n=152)",cex=0.7)
axis(side = 2, at=seq(-6,12,2))
axis(side = 1, at=seq(0,5,1), labels = c("","1 (n=26)","2 (n=63)","3 (n=71)","4 (n=1)",""))
abline(h=0,lty=2)
text(x= 1.3, y= max(plot.text$stats[,1]), labels= "a")
text(x= 2.4, y= max(plot.text$stats[,2]), labels= "a,b")
text(x= 3.3, y= max(plot.text$stats[,3]), labels= "b")
text(x= 4.3, y= max(plot.text$stats[,4]+0.5), labels= "a,b")
summary(lm(compound.chg.rate~as.factor(group),all.results.outcomes)) 
par(xpd = NA)
#abline(h=37, lty=2)
#abline(h=15, lty=2)
#abline(h=-7.5, lty=2)
#abline(v=5.5, lty=2)
#abline(v=-0.6, lty=2)
#abline(v=-6.3, lty=2)
#abline(v=-12.05, lty=2)
#abline(v=-17.8, lty=2)
#light.green.bg<-rgb(102,255,102, maxColorValue = 255, alpha=50)
#blue.bg<-rgb(0,114,178, maxColorValue = 255, alpha=30)
#orange.bg<-rgb(255,128,0, maxColorValue = 255, alpha=30)
#rect(-17.8,-7.5,5.5,15, col= orange.bg, border=FALSE)
#rect(-6.3,15,5.5,37, col= blue.bg, border=FALSE)
#rect(-17.8,15,-6.3,37, col= light.green.bg, border=FALSE)
white.bg <-rgb(255,255,255, maxColorValue = 255)
black.bg <-rgb(0,0,0, maxColorValue = 255)
rect(-20,-8,4,-5, col= white.bg, border=FALSE)
rect(-15,-6.5,-14.5,-5.25, col= vermillioncb, border=TRUE)
rect(-10,-6.5,-9.5,-5.25, col= greencb, border=TRUE)
rect(-5,-6.5,-4.5,-5.25, col= bluecb, border=TRUE)
rect(0,-6.5,0.5,-5.25, col= black.bg, border=TRUE)
text(x= -13.3, y= -5.85, cex=1.3, labels= "Rapidly expansionist")
text(x= -8.75, y= -5.85, cex=1.3, labels= "Expansionist")
text(x= -3.75, -5.85, cex=1.3, labels= "Consolidative")
text(x= 1.7, -5.85, cex=1.3, labels= "Rapidly consolidative")

# Hypothesis testing - economic structure metrics and outcome metrics.

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
