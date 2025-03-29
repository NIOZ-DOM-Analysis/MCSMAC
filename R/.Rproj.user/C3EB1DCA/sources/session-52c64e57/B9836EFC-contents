#################################################################
####                    USER DEFINED INPUT                   ####
#################################################################

#Which MZmine version did you use?
Mzmine<-3

#Define if you your GNPS network was "Classic" or  "FBMN" or "IIN" (ion identity networking)
networking.type <- "IIN"
#Did you run MolNetEnhancer
MolNetEnh <- "YES"
#Did you run Dereplicator separately?
Dereplicator <- "NO"
#Did you run Dereplicator+?
Dereplicator_plus <- "NO"

# #if you want to update your R, R studio you have to do by hand.
# source(paste0(Data.cleanup.scripts, '/updateR.R'))

#################################################################
####               END OF USER DEFINED INPUT                 ####
#################################################################


#################################################################
####                   LOAD PACKAGES                         ####
#################################################################
#ALWAYS RUN THIS
source(paste0(Data.cleanup.scripts,'/packages.R'))


#set seed for reproducibility
set.seed(25)

#prep the rawfiles for cleanup but select the one that fits your Cytoscape output.
# output after 2021 have a separate analog folder, instead of having to download those results seperately
# the old option will be deprecated

# for no analogs data folder use:
# source(paste0(Data.cleanup.scripts,'/read_prep_rawfiles.R'))
# # OR
# source(paste0(Data.cleanup.scripts,'/read_prep_rawfiles_with_analog_folder_2021.R'))

# You probably need
source(paste0(Data.cleanup.scripts,'/read_prep_rawfiles_with_mzmine3.R'))

source(paste0(Data.cleanup.scripts,'/Elemental_formula_parsing.R'))

#compare gapfilled and non-gapfilled data to determine background noise
 source(paste0(Data.cleanup.scripts,'/compare_gapfilled_non-gapfilled.R'))



#################################################################
####                    USER ACTION Required                 ####
#################################################################
#'if you have samples that were part of your mzmine run but you
#dont want to analyze, you can define here the common identifier
#you can define multiple sample codes
#
#if you dont want to delete samples do NOT run this code'
#if your orbitrapsequence only consist of the samples and blanks you want to analyze, but you ran MZMINE with more samples, fill out NA
samples2delete<- NA


#################################################################
####               END OF USER Required action               ####
#################################################################

source(paste0(Data.cleanup.scripts,'/clean_workspace_from_rawfiles.R'))


#clean your MS1 data

#option 1: clean based on average in all samples - environmental samples
source(paste0(Data.cleanup.scripts,'/flagging_average_all_samples.R'))


#option 2: clean based on area in all samples separate - time series
#source(paste0(Data.cleanup.scripts,'/flagging_per_sample.R')


#remove blanks
source(paste0(Data.cleanup.scripts,'/remove_blanks.R'))


#################################################################
####                    USER INPUT Required                 ####
#################################################################
#remove transient features
print(head(split_pregap))

#now set the background noise based on the pregap value in the first row
#(this is the first value that is not 0) before gapfilling.

background_noise <- 4090

#of how many samples does the smallest group consist?
W<-2 #this will be the minimum for transient_feature removal

#################################################################
####               END OF USER Required action               ####
#################################################################
source(paste0(Data.cleanup.scripts,'/transient_features.R'))

#normalize by TIC and transform sqrt by sample
source(paste0(Data.cleanup.scripts,'/TICNORM_and_SQRT.R'))

#normalize by TIC and transform sqrt by feature
#source(paste0(Data.cleanup.scripts,'/TICNORM_and_SQRT_by_FEATURE.R'))

#should your 0's in the dataset be replaced by something? (1000 = 3, 1 = 0)
zeroes<-1000

#or transform XIC by log 10
source(paste0(Data.cleanup.scripts,'/LOGNORM.R'))


#################################################################
####                   Data analysis file                    ####
#################################################################

#write data clean report
read.data_report<-(paste0(Data.cleanup.scripts,"/data_report.Rmd"))
rmarkdown::render(read.data_report, output_dir = dirDoc, output_format = "html_document")

