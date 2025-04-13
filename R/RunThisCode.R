# MCSMAC analysis
# this project was made by 'project starter'
https://github.com/NIOZ-DOM-Analysis/ProjectStarter

#set working directory
wd.project <- "Documents/GitHub/Projects/MGIA/2018/MCSMAC/R"

#define folder references
dirCyto <- "../Cytoscape"
dirDoc <- "../doc"
dirFigs <- "../figures"
dirOutput <- "../output"
dirR <- "../R"
dirRAW <- "../RAWdata"
dirWrite <- "../output/write_read"


#when you put all the data in the raw folder we are first going to check if the metadata is accepted its good
metadata <- read_csv(file.path(dirRAW, "metadata.csv"))
if( "Sample Name" %in% colnames(metadata) ){
  print("metadatasheet accepted")
}else{
  stop("No column with \"Sample Name\" as columnname. check metadata.csv ")}


# data cleanup is done by 'datacleanup'
https://github.com/NIOZ-DOM-Analysis/DataCleanup

#now Check if data.cleanup.folder is referenced correctly
normalizePath(Data.cleanup.folder)

#if not redirect here:
#Data.cleanup.folder <- file.path("/Users/XXX/Documents/GitHub/DataCleanup")

# now run the data cleanup
# this code will open a new script to run, fill out information below in script.
file.edit(paste0(Data.cleanup.folder, "/DataCleanup.R"))

#### info on metabolomics for MCSMAC
#
# #Which MZmine version did you use?
# Mzmine<-3
#
# #Define if you your GNPS network was "Classic" or  "FBMN" or "IIN" (ion identity networking)
# networking.type <- "IIN"
# #Did you run MolNetEnhancer
# MolNetEnh <- "YES"
# #Did you run Dereplicator separately?
# Dereplicator <- "NO"
# #Did you run Dereplicator+?
# Dereplicator_plus <- "NO"

# First do the data cleanup
# MCSMAC Does not have the analog folder in Cytoscape!

#########

#when you are done with this whole script you should have the basic cleaned up metabolomics data

#################################################################
####                    USER ACTION Required                 ####
#################################################################
#define the two timepoints (column Time_Point)
Tmin <- "0"
Tmax <- "28"

#if you have more replicates of Tend then T0, how often do you have to copy the line to make the data sets the same size?
nr.rep<- 3

#what is the best way to sort the datasets in order to be the same?
sorting1<-"Treatment_ratio"
sorting2<-"Replicate"

#set factor for the minimum value to be considered an exudate
#if the feature has to be 2.5x the value of what was found in the control, factor should be 2.5
foldchange<-2
min_exudate_size<-(1*10^6)/60

#where do you want to do the exudate finder test on (raw area's[df.area] or
#log normalized with zeroes replaced? [df.norm.area]) !!! use the one with metadata!!
#dataset
DS<-df.area
#put the object name here as well as a character string
DS.name<-"raw peak areas"

#which columname defines the groups that you want to test?
test_group<-"Treatment_ratio"
test_against<-"Control"

#define exudates in timepoint 0
source(paste0(dirR,'/exudate_finder.R'))

#find features that are metabolized
source(paste0(dirR,'/metabolized_features_finder.R'))


# #lets create an upset plot and venn diagram of this exudate data
 source(paste0(dirR, '/MCSMAC_exudate_upset and venn.R'))


# now go to http://www.interactivenn.net/ and use the data from VennDiagram.Info.txt in the Figures folder
#to maka a venn diagram and save it in the figure folder.


source(paste0(dirR, '/Final_calculations.R'))

## hobo data
source(paste0(dirR, '/Hobo_data_analysis.R'))

#################################################################
####          FCM DATA                                       ####
#################################################################

#if you have microbial counts you can analyze them here but you need to define something, this script is not prefect yet
#how many gates do you have?
ngates<-5
#viral counts
# ngates.viral<-3

file.edit(paste0(dirR,'/microbial_counts_FCM.R'))

#define which time point you have been looking at
timepoint <- 21

file.edit(paste0(dirR,'/Microbial_size.R'))


# DOC analyis
file.edit(paste0(dirR, '/DOC_analysis.R'))


#run if you come back later and need to activate the packages
source(paste0(Data.cleanup.scripts, '/packages.R'))
