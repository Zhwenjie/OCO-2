
library(FRK)
library(dplyr)
library(ggplot2)
library(ncdf4)

data_version <-"oco2v8"
data_version.path <- "/Volumes/Macintosh2TBHD/Wenjie_Sci/SatelliteData/OCO2/OCO2_L2_lite_FP.8r"
setwd(data_version.path)
inputfiles <- list.files(pattern="*.nc4$", full.names=TRUE, recursive=FALSE)
target.year <- "15"
target.month <- "09"
number.day <- "31"

target.files <- c()
i<-0
for (file in inputfiles) {
  year<-substr(unlist(strsplit(file,"_"))[3],1,2)
  month <- substr(unlist(strsplit(file,"_"))[3],3,4)
  day <- substr(unlist(strsplit(file,"_"))[3],5,6)
  
  if(year==target.year&month==target.month&as.integer(day)<=as.integer(number.day)){
    i<-i+1
    target.files[i]<-file
  }
}
print(length(target.files))

csv.output<- "/Volumes/Macintosh2TBHD/Wenjie_Sci/SatelliteData/OCO2/csv_test/"
for (i in 1:length(target.files)) {
  print(paste("reading",target.files[i]))
  nc <- nc_open(target.files[i])
  oneday <- data.frame(
    datetime=strptime(format(ncvar_get(nc,'sounding_id'), scientific=FALSE), "%Y%m%d%H%M%S", tz="UTC"),
    longitude=ncvar_get(nc,'longitude'),
    latitude=ncvar_get(nc,'latitude'),
    xco2=ncvar_get(nc,'xco2'),
    fs=ncvar_get(nc,'Retrieval/fs'),
    xco2_uncertainty=ncvar_get(nc,'xco2_uncertainty'),
    xco2_quality_flag=ncvar_get(nc,'xco2_quality_flag'),
    warn_level=ncvar_get(nc,'warn_level'),
    operation_mode=ncvar_get(nc,'Sounding/operation_mode'),
    stringsAsFactors = FALSE
  )
  
  # Filter the data before saving
  oneday <- oneday[complete.cases(oneday),]
  oneday<-oneday[oneday$fs>0,]
  oneday <- oneday[oneday$xco2_quality_flag==0,]
  #oneday <- oneday[oneday$warn_level<15,]
  #oneday <- oneday[oneday$operation_mode<2,]
  #oneday <- oneday[(oneday$xco2_uncertainty < 3),]
  #oneday$xco2_uncertainty <- pmax(oneday$xco2_uncertainty,2)
  
  if (i==1) {
    # If this is the first line, write column names to a new file.
    output_names <- TRUE
    output_append <- FALSE
  } else {
    output_names <- FALSE
    output_append <- TRUE
  }
  write.table(data.frame("day"=oneday$datetime, "lon"=oneday$longitude, "lat"=oneday$latitude, "fs"=oneday$fs,"xco2"=oneday$xco2, "std"=oneday$xco2_uncertainty),
              file=paste0(csv.output,data_version,target.year,target.month,'_lite.csv'), row.names=FALSE, col.names=output_names, sep=',', append=output_append)
}




###plotting 

theme_set(theme_grey(base_size = 20))

my_colours <- c("#03006d","#02008f","#0000b6","#0001ef","#0000f6","#0428f6","#0b53f7","#0f81f3",
                "#18b1f5","#1ff0f7","#27fada","#3efaa3","#5dfc7b","#85fd4e","#aefc2a","#e9fc0d",
                "#f6da0c","#f5a009","#f6780a","#f34a09","#f2210a","#f50008","#d90009","#a80109","#730005")

my_theme <- theme(panel.background = element_rect(fill = "white",colour = "white"), panel.grid = element_blank(), axis.ticks = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
                  plot.title = element_text(hjust = 0.5))


oco2parent.dir <- "/Volumes/Macintosh2TBHD/Wenjie_Sci/SatelliteData/OCO2/"
setwd(oco2parent.dir)
csv.path <- "/Volumes/Macintosh2TBHD/Wenjie_Sci/SatelliteData/OCO2/csv_test/"
data_version <- "oco2v8"
oco2lite <- read.csv(paste0(csv.output,data_version,target.year,target.month,'_lite.csv'))

oco2lite$day <- as.Date(oco2lite$day, tz="UTC")
selecteddates <- unique(as.Date(oco2lite$day, tz="UTC"))
#sixteendays <- oco2lite
Date.index <-0
for (Date.index in 1:length(selecteddates)) {
  
  oneday <- oco2lite[as.Date(oco2lite$day,tz="UTC")==as.Date(selecteddates[Date.index],tz="UTC"),]
  #print(oneday[1,])
  ggsave(
    (ggplot(oneday) +
       my_theme +
       geom_point(aes(lon,lat,colour=pmin(pmax(fs,0),1))) +
       #geom_point(aes(lon,lat,colour=pmin(pmax(xco2,390),410))) +
       lims(x = c(-180, 180), y = c(-90, 90)) +
       scale_colour_gradientn(colours=my_colours, limits=c(0,1))  +
       #labs(x="lon (deg)", y="lat (deg)", colour="XCO2\n(ppm)\n", title=paste(selecteddates[Date.index],data_version,"DAILY DATA"))+
       labs(x="lon (deg)", y="lat (deg)", colour="fs/W/m^2/um/sr", title=paste(selecteddates[Date.index],data_version,"DAILY DATA"))+
       coord_map("mollweide")) %>%
      draw_world(inc_border=TRUE),
    filename = file.path(paste0(data_version,"plots"),paste0("global_fs_",selecteddates[Date.index],"_data.png")), width=16, height=9, dpi=300)
  
}
