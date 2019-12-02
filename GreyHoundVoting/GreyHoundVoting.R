
#Packages

library(devtools)
library(ggmap)
library(tidyverse)
library(sf)
library(rgdal)
library(spatialreg)
library(extrafont)
library(gtable)
library(ggpubr)
library(cowplot)

# Load in Precinct Shapefiles

## Precinct Shapefiles
setwd("*Set WD Here*")

FL.2018.Precs <- st_read("VEST18Gen.shp")
FL.2018.Precs<-st_zm(FL.2018.Precs, what = "ZM")

## Dog Tracks
tracks <- read_csv("dogtracks.csv",col_names = T)
tracks<- st_as_sf(tracks, coords = c("long","lat"),crs = 4269) #converts to spatial features


# Centroids

## Calculate
prec.cents<- st_centroid(FL.2018.Precs)

## Grab CRS
prec.cents.crs<-st_crs(prec.cents)

## Convert tracks to correct crs
tracks.right.crs<-st_transform(tracks,prec.cents.crs)

## Visualize

cent.pic<-ggplot()+geom_sf(data = FL.2018.Precs1, fill = "#E8EAE8")+
  geom_sf(data = prec.cents, color= "#0856AF", size = .7)+theme_nothing()+
  ggtitle("Centroids of Florida's 2018 Election Precincts")+
  theme(plot.title = element_text(hjust = .5, size = 14))

cent.pic


# Distances

## Create distance matrix
track.cent.dists<-st_distance(prec.cents,tracks.right.crs)
dists.df<-as.data.frame(track.cent.dists)

##Keeps the Minimum Distance to a Track By Row
dist.min<-dists.df %>%
  rowwise()%>%
  mutate(mindist =  min(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13)/1609)%>%
  select(mindist)

## Binds the distances with their IDS
dist.min.ids<-cbind(dist.min,Pct_std = as.character(prec.cents$Pct_std))

## Merge with State Results
Spatial.Doggies <- sp::merge(x = FL.2018.Precs, y = dist.min.ids, by = "Pct_std") 

# Analysis 

## Dependent Variable Creation
Spatial.Doggies <- Spatial.Doggies %>%
  mutate(per.yes = G18AM13Yes/G18AM13Sum,
         doggy.turnout = G18AM13Sum/SumRegis,
         doggy.turnout = case_when(
           doggy.turnout > 1 ~ 1,
           TRUE ~ doggy.turnout
         ))%>%
  filter(!(Pct_std == "PAL00NP"))

### per.yes proportion yes on AM 13 in precinct
### doggy.turnout is turnout proportion on AM 13 in precinct (topcoded at 1)


## Bivariate Comparisons

### Percent Yes by Distance
percent<-ggplot(data = Spatial.Doggies)+
  geom_point(aes(x = mindist, y = per.yes), shape = 1, color = "#295151", alpha = .3)+
  geom_smooth(aes(x = mindist, y = per.yes), color = "#E08662", se = F) + theme_tufte() + ggtitle("Support") +ylab("Proportion Voted Yes")+xlab("Distance (In Miles)")+theme(
    plot.title = element_text(hjust = .5, family = "Cambria", size = 12), axis.title = element_text(size=8)
  )

percent

### Precinct Turnout by Distance
turnout<-ggplot(data = Spatial.Doggies)+
  geom_point(aes(x = mindist, y = doggy.turnout), shape = 1, color = "#E08662", alpha = .3)+
  geom_smooth(aes(x = mindist, y = doggy.turnout), color = "#295151", se = F) + theme_tufte() + ggtitle("Turnout") + ylab("Proportion Voted")+xlab("Distance (In Miles)")+theme(
    plot.title = element_text(hjust = .5, family = "Cambria", size = 12), axis.title = element_text(size=8)
  )

turnout

## Linear Regression

### Control variables
Spatial.Doggies <- Spatial.Doggies %>%
  mutate(area = st_area(Spatial.Doggies),
         voter.density = SumRegis/area,
         gperc = G18GOVDGil/G18GOVSum,
         gturn = G18GOVSum/SumRegis,
         gturn = case_when(
           gturn > 1 ~ 1,
           TRUE ~ gturn
         ))
#### Percent Gillum, gillum turnout (topcoded at 1), polygon area, voter density

### Vote Choice (Percent yes model)

choice.reg <- lm(data = Spatial.Doggies, per.yes ~ mindist + area + voter.density + gperc+ gturn +SumRegis)
summarise(choice.reg)


turnout.reg <- lm(data = Spatial.Doggies, doggy.turnout ~ mindist + area + voter.density + gperc+ gturn +SumRegis)
summarise(turnout)




