download <- "Pollution.zip"
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
dir <- "PM2.5_Dataset"
if(!file.exists(dir)){
  download.file(fileURL, download,mode = "wb")
}
unzip("Pollution.zip",files = NULL,exdir = ".")
NEI <- readRDS("Pollution/summarySCC_PM25.rds")
SCC <- readRDS("Pollution/Source_Classification_Code.rds")
##---------------------------------------------------------------------------------------------##
###-------------------------------------------------------------------------------------------###
####--------------------------------------PART-1---------------------------------------------####
###-------------------------------------------------------------------------------------------###
##---------------------------------------------------------------------------------------------##
years <- matrix(0,nrow = 4,ncol = 2)
years <- data.frame(years)
years[,1] <- as.character(unique(sub1$year))
for (i in 1:4){
  years[i,2] <- sum(NEI$Emissions[NEI$year==unique(NEI$year)[i]])
}
barplot(years[,2],names.arg = years[,1])
title(main = "PM 2.5 Emissions over the years",xlab="Years", ylab="PM 2.5 Emissions" )
dev.copy(png,"plot1.png")
dev.off()
##---------------------------------------------------------------------------------------------##
###-------------------------------------------------------------------------------------------###
####--------------------------------------PART-2---------------------------------------------####
###-------------------------------------------------------------------------------------------###
##-------------------------------------------------PPPP
years <- matrix(0,nrow = 4,ncol = 2)
years <- as.data.frame(years)
years[,1] <- as.character(unique(sub1$year))
for (i in 1:4){
  years[i,2] <- sum(sub1$Emissions[sub1$year==unique(sub1$year)[i]])
}
barplot(years[,2],names.arg = years[,1])
title(main = "PM 2.5 Emissions over the years",xlab="Years", ylab="PM 2.5 Emissions" )
dev.copy(png,"plot2.png")
dev.off()
##---------------------------------------------------------------------------------------------##
###-------------------------------------------------------------------------------------------###
####--------------------------------------PART-3---------------------------------------------####
###-------------------------------------------------------------------------------------------###
##---------------------------------------------------------------------------------------------##
sub1 <- subset(NEI, fips == "24510")
point <- data.frame(matrix(0,nrow = 4,ncol = 5))
point[,1] <- as.character(unique(sub1$year))
for (i in 1:4){
  point[i,2] <- mean(sub1$Emissions[sub1$year==unique(sub1$year)[i] & sub1$type == "POINT"])
  point[i,3] <- mean(sub1$Emissions[sub1$year==unique(sub1$year)[i] & sub1$type == "NONPOINT"])
  point[i,4] <- mean(sub1$Emissions[sub1$year==unique(sub1$year)[i] & sub1$type == "ON-ROAD"])
  point[i,5] <- mean(sub1$Emissions[sub1$year==unique(sub1$year)[i] & sub1$type == "NON-ROAD"])
}
names(point) <- c("Years","POINT","NON-POINT","ON-ROAD","NON-ROAD")
d <- melt(point,id.vars = "Years")
g1 <- ggplot(d,aes(Years,value,col=variable))+geom_point()+geom_line()+stat_smooth
