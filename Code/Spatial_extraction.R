###############################
##    Spatial Extraction  #####
###############################

# Use WRST Spatial_T1_extraction.Rmd as a template to pull data for WEAR
# Starting with BELA (Bering Land Bridge National Preserve)

rm(list = ls())

library(stars);library(dplyr);library(ggplot2);library(ggthemes);library(viridis);library(here);library(ggrepel);library(rlang);#library(ggbiplot);
library(tmap); library(tmaptools)

# Initials

data.dir <- "D:/NCAR_AK/met/monthly/BCSD/" 
#vic.dir <- "D:/NCAR_AK/vic_hydro/monthly/BCSD"
plot.dir <- "C:/Users/adillon/Documents/RSS/WEAR/Plots"

historical.period <- as.character(seq(1950,1999,1))
future.period <- as.character(seq(2025,2055,1))

#area <- 'copper_plateau' #wrst_mtns, copper_plateau, coastal_mtns, eastern_coast


GCMs <- list.files(path = data.dir)
RCPs <- c("rcp45", "rcp85")
GCM.RCP <- sort(apply(expand.grid(GCMs, RCPs), 1, paste, collapse="."))


# Load Spatial Data

boundaries <- st_read('./Data/spatial-data/nps_boundary/nps_boundary_Albers.shp') 
shp <- filter(boundaries, UNIT_CODE == "BELA")

shp <- st_transform(shp, 3338)

# Plot

tmap_mode('view') # set to interactive viewing - can zoom in and out to make sure the park is in the right place

tm_shape(shp) + 
  tm_polygons()

# Create empty dataframes that will store climate summaries for each GCM.rcp as loop through data

variables <- c("Tmean_F", "Precip_in")

Baseline_Means <- as.data.frame(matrix(data=NA,nrow=length(GCMs)*length(RCPs),ncol=length(variables)+2))
names(Baseline_Means) <- c("GCM", "RCP", variables)

Future_Means <- as.data.frame(matrix(data=NA,nrow=length(GCMs)*length(RCPs),ncol=length(variables)+2))
names(Future_Means) <- c("GCM", "RCP", variables)

Deltas <- as.data.frame(matrix(data=NA,nrow=length(GCMs)*length(RCPs),ncol=length(variables)+2))
names(Deltas) <- c("GCM", "RCP", variables)

# Loop through met GCM.rcp and summarize each variable

source('./Code/met-variables.R')

#save.image("./Data/RData/BELA_environment.RData")

# Run scatterplot
scatterplot <- function(df, X, Y, title,xaxis,yaxis){
  plot <- ggplot(data=df, aes(x={{ X }}, y={{ Y }}))  +
    geom_text_repel(aes(label=paste(GCM,RCP,sep="."))) +
    geom_point(colour="black",size=4) +
    theme(axis.text=element_text(size=18),
          axis.title.x=element_text(size=18,vjust=-0.2),
          axis.title.y=element_text(size=18,vjust=0.2),
          plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
          legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
    ###
    labs(title =title, 
         x = paste0("Changes in ",xaxis), # Change
         y = paste0("Changes in ",yaxis)) + #change
    scale_color_manual(name="Scenarios", values=c("black")) +
    # scale_fill_manual(name="Scenarios",values = c("black")) + 
    theme(legend.position="none") 
  plot
}

scatterplot(df=Deltas,X=Tmean_F,Y=Precip_in,title="Tmean vs Precip scatterplot",xaxis="Avg Annual Temp (F)",yaxis="Avg Annual Precip (in)")

