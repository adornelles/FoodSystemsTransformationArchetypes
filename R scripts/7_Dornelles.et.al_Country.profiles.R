# Andre Dornelles, April 2020.

# 7_Dornelles.et.al_Country.profiles.R

# This script aggregates the trend results (expressed as Compound Annual Change Rate) and the longitudinal
# observations of all structure and outcome metrics for each of the 161 countries assesssed to navigate the 
# trasnformation archetypes in global food systems from 1995 to 2015.

## This script has 2 parts:
# 1. Upload all the trend results (expressed as CACR) and longitudinal observations;
# 2. Country profiles: Aggregate and generate pdf files with the longitudinal obervations of all structure 
# and outcome metrics for each of the 161 countries 

## Instructions:
# First: Load all packages and colour configurations;
# Second: Upload the .csv files for the a) trend results (expressed as CACR) and b) longditudinal observations file;
# Third: Run the loop to generate the .pdf file with country profiles for all of the 161 countries. 

# Load packages:
library(ggplot2)
library(dplyr)
library(ggpubr)
library(svMisc)
theme_set(theme_pubr())
# Plot colours:
vermillioncb<-rgb(213,94,0, maxColorValue = 255)
skybluecb<-rgb(86,180,233, maxColorValue = 255)
greencb<-rgb(0,158,115, maxColorValue = 255)
bluecb<-rgb(0,114,178, maxColorValue = 255)
group.colours<-c(vermillioncb,greencb,bluecb,skybluecb)
#group.pallete<-c("1"=vermillioncb,"2"=greencb,"3"=bluecb)
group.pallete<-c("Rapidly expansionist"=vermillioncb,"Expansionist"=greencb,"Consolidative"=bluecb)

### PART 1: Upload datasets
setwd("C:\\Users\\tv836863\\Desktop\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")
setwd("C:\\Users\\a_dor\\OneDrive - University of Reading\\Andre Zuanazzi Dornelles\\Food systems transformation archetypes\\Working with Tom\\Andre trend results")

# Trend results (expressed as CACR)
transf.arc.cacr<-read.csv("Transformation Archetypes_all_metrics_trend_results.csv",header=T)
# Longitudinal observations (timespan between 1961 and 2019)
transf.arc.long<-read.csv("Transformation Archetypes_all_metrics_longitudinal_observations_1961-2019.csv",header=T)

# Trends (CACR):
View(transf.arc.cacr)
names(transf.arc.cacr)

### PART 2: Longitudinal observations:
all.results<-transf.arc.long #  All trends, from 1961-2019
names(all.results)
all.results$group[all.results$group=="1"]<-"Rapidly expansionist"
all.results$group[all.results$group=="2"]<-"Expansionist"
all.results$group[all.results$group=="3"]<-"Consolidative"
# Filter from 1995-2015:
all.results <- all.results %>%
  filter(year>=1995) %>%
  filter(year<=2015)
# Loop and generate country profiles:
country.list<-as.character(unique(transf.arc.cacr$Common.name))
plots<-list()
pdf("Country_profiles.pdf")
for (i in seq_along(country.list)) {
  # Plot for each country
  progress(i, length(country.list))
  struc.1 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,agric.area,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) +  scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) + 
    ggtitle("Agricultural area") + labs(x="year") + scale_x_continuous("year") + labs(y="1,000 hectares") +
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    theme(legend.position = "none")
  struc.2 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,synt.fert,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) + scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) +
    ggtitle("Synthetic fertiliser use") + labs(x="year") + scale_x_continuous("year") + labs(y="Tonnes") +
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    theme(legend.position = "none")
  struc.3 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,agric.empl,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) +  scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) + 
    ggtitle("Agricultural employment") + labs(x="year") + scale_x_continuous("year") + 
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    labs(y="Percent of total employment") +
    theme(legend.position = "none")
  struc.4 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,gao,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) + scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) +
    ggtitle("Gross Agricultural Output") + labs(x="year") + scale_x_continuous("year") + 
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    labs(y="Constant International US$, 2010") +
    theme(legend.position = "none")
  struc.5 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,tfp,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) + scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 6, face = "bold")) +
    ggtitle("Agricultural Total Factor Productivity") + labs(x="year") + scale_x_continuous("year") + 
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    labs(y="Index") +
    theme(legend.position = "none")
  struc.6 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,food.import,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) + scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) +
    ggtitle("Food imports") + labs(x="year") + scale_x_continuous("year") + labs(y="International US$") +
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    theme(legend.position = "none")
  struc.7 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,food.export,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) + scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) +
    ggtitle("Food exports") + labs(x="year") + scale_x_continuous("year") + labs(y="International US$") +
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    theme(legend.position = "none")
  struc.8 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,ppi,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) + scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 6, face = "bold")) +
    ggtitle("Producer Price Index, Agriculture") + labs(x="year") + scale_x_continuous("year") + 
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    labs(y="Index") +
    theme(legend.position = "none")
  p <- ggarrange(struc.1, struc.2, struc.3, struc.4, struc.5, struc.6, struc.7, struc.8,
                 ncol = 4, nrow = 2)
  struc <- annotate_figure(p,
                           top = text_grob("Structure metrics", color = "black", face = "bold", size = 14))
  outc.1 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,forest.area,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) +  scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) + 
    ggtitle("Forest area") + labs(x="year") + scale_x_continuous("year") + labs(y="1,000 hectares") +
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    theme(legend.position = "none")
  outc.2 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,red.list,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) +  scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) + 
    ggtitle("Red List Index") + labs(x="year") + scale_x_continuous("year") + coord_cartesian(xlim = c(2000, 2015)) +
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    labs(y="Index") + theme(legend.position = "none")
  outc.3 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,undernour,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) +  scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) + 
    ggtitle("Undernourishment") + labs(x="year") + scale_x_continuous("Prevalence") + 
    coord_cartesian(xlim = c(2000, 2015)) + labs(y="Index") + 
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    theme(legend.position = "none")
  outc.4 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,obesity,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) +  scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) + 
    ggtitle("Obesity") + labs(x="year") + scale_x_continuous("year") + labs(y="Prevalence") +
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    theme(legend.position = "none")
  outc.5 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,agric.ghg,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) +  scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) + 
    ggtitle("Agricultural GHGE") + labs(x="year") + scale_x_continuous("year") + labs(y="Megatons of CO2eq") +
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    theme(legend.position = "none")
  outc.6 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,land.for.ghg,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) +  scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 6, face = "bold")) + 
    ggtitle("Land-use change and Forestry GHGE") + labs(x="year") + scale_x_continuous("year") + 
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    labs(y="Megatons of CO2eq") +
    theme(legend.position = "none")
  outc.7 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,afolu.ghg,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) +  scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 8, face = "bold")) + 
    ggtitle("AFOLU GHGE") + labs(x="year") + scale_x_continuous("year") + labs(y="Megatons of CO2eq") +
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    theme(legend.position = "none")
  outc.8 <- all.results %>% filter(Common.name == country.list[i]) %>%
    ggplot(aes(year,footp.cons,group=1)) +
    geom_line(aes(colour=as.factor(group)),size=1.5) + labs(colour = "Transformation archetype") +  
    scale_color_manual(values=group.pallete) +  scale_size_manual(values=c(2, 2)) + theme_classic() + 
    theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(size = 6, face = "bold")) + 
    ggtitle("Ecological Footprint, Consumption") + labs(x="year") + scale_x_continuous("year") + 
    theme(axis.text = element_text(size=3)) + theme(axis.title = element_text(size = 5)) +
    labs(y="Global hectares") +
    theme(legend.position = "none")
  o <- ggarrange(outc.1, outc.2, outc.3, outc.4, outc.5, outc.6, outc.7, outc.8,
                 ncol = 4, nrow = 2,
                 common.legend = TRUE, legend = "bottom")
  outc <- annotate_figure(o,
                          top = text_grob("Outcome metrics", color = "black", face = "bold", size = 14))
  all.metrics <- ggarrange(struc, outc,
                           ncol = 1, nrow = 2)
  country.profile <- all.results %>% filter(Common.name == country.list[i])
  cp <- annotate_figure(all.metrics,
                        top = text_grob(country.profile$Common.name, color = "black", face = "bold", size = 18))
  plots[[i]] = cp
  print(cp)
}
dev.off()
print(plots)
