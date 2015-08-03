#*************************************Required Libraries******************************************
require(plyr)
require(grid)
require(reshape2)
require(stringr)
require(ggplot2)
# require(logging)
# debug(VariableNumericalFormat)
#*************************************Options*****************************************************
options(error=recover)
options(warn=1)
# basicConfig()
# logdebug("not shown, basic is INFO")
# logwarn("shown and timestamped")

# system("defaults write org.R-project.R force.LANG en_US.UTF-8")
# debug("CreateCSV")

# debug(apply_lookups)
# debug(CreateDuration)
#*************************************Lookup Files*****************************************************
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
# Path<-"~\\FPDS\\R scripts and data\\"
# Path<-"C:\\Users\\Greg Sanders\\SkyDrive\\Documents\\R Scripts and Data SkyDrive\\"
# setwd("C:\\Users\\Greg Sanders\\Documents\\Development\\Competition")
setwd("K:\\Development\\Competition")

legend.text.size<-5
axis.text.size<-5
strip.text.size<-5
legend.text.size<-5

table.text.size<-5.75
title.text.size<-6
geom.text.size<-1.5


source(paste(Path,"helper.r",sep=""))
source(paste(Path,"lookups.r",sep=""))
# source(paste(Path,"statistics_aggregators.r",sep=""))
source(paste(Path,"create_procedural_graphs.r",sep=""))



# undebug(create_procedural_graphs)
create_procedural_graphs("Defense Competition State","Overall",LayoutName="short half page")
# create_procedural_graphs("Defense Competition State","StateCode")
undebug(create_procedural_graphs)
# debug(apply_lookups)
create_procedural_graphs("Defense Competition MCC","Overall",LayoutName="short half page")
# debug(CreateChartOrTable)
# create_procedural_graphs("Defense Competition MCC","MajorCommandCode")


