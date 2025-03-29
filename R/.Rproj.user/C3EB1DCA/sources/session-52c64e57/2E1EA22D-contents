'exudate_finder.R

•	Look at the T0 samples and compare the different groups to the control sample to know the things produced

Written by:
Milou Arts, NIOZ, NL, 2020

List of alterations:



'
library(readr)
library(rstatix)
library(tidyverse)
library(ggpubr)
# #set factor for the minimum value to be considered an exudate
# #if the feature has to be 2.5x the value of what was found in the control, factor should be 2.5
# foldchange<-2
#
# #where do you want to do the exudate finder test on (raw area's[df.area] or
# #log normalized with zeroes replaced? [df.norm.area]) !!! use the one with metadata!!
# #dataset
# DS<-df.area
# #put the object name here as well as a character string
# DS.name<-"raw peak areas"

# DS[,M:ncol(DS)] <- DS[,M:ncol(DS)] %>% dplyr::mutate_if(is.character, as.numeric)

# put all input information into analysis info object
analysis_info <- list()
analysis_info$exudate_finder_DS <- DS.name
analysis_info$exudate_fold_change <- foldchange
analysis_info$exudate_test_group <- test_group
analysis_info$exudate_test_group_control <- test_against
analysis_info$exudate_min_peak_area <- min_exudate_size

# select only the samples that have exudates (t-0)
# this could be made into something you can define in the main script
tmp <- dplyr::filter(DS, Time_Point == 0)

# split them into the two test groups and select only the features in a dataframe.
tmp.contr <- dplyr::filter(tmp, .data[[test_group]] == {{test_against}})
tmp.rest <- dplyr::filter(tmp, .data[[test_group]] != {{test_against}})

tmp.contr2 <- as.data.frame(t(tmp.contr[, M:ncol(tmp.contr)]))
colnames(tmp.contr2) <- unlist(tmp.contr[test_group])

tmp.rest2 <- as.data.frame(t(tmp.rest[, M:ncol(tmp.rest)]))
colnames(tmp.rest2) <- unlist(tmp.rest[test_group])

rm(tmp.contr, tmp.rest)

# set factor for the minimum value to be considered an exudate
# if the feature has to be 2.5x the value of what was found in the control, factor shoudl be 2.5
# foldchange<-2
# Also, the feature needs to be at least min_exudate_size big to be considered an exudate
minfactor <- log2(foldchange)
analysis_info$exudate_minfactor <- minfactor

# copy tmp.rest2 so you have a dataframe with the right dimensions.
exudates <- tmp.rest2
# Test if a feature is an exudate and replace the values in the object exudates
for (i in 1:nrow(tmp.rest2)) {
  for (j in 1:ncol(tmp.rest2)) {
    if (is.na(log2(tmp.rest2[i, j] / tmp.contr2 [i, 1]) > minfactor))
    {
      exudates[i, j] <- NA
    } else{
      if ((log2(tmp.rest2[i, j] / tmp.contr2 [i, 1]) > minfactor) &
          (tmp.rest2[i, j] > min_exudate_size)) {
        exudates[i, j] <- tmp.rest2[i, j]
      } else {
        exudates[i, j] <- NA
      }
    }
  }
}

# count how often something is regarded an exudate, print and store in analysis info
exudates$count <- apply(exudates, 1, function(c)
  sum(!is.na(c)))
counts <- as.factor(exudates$count)
print(summary(counts))
analysis_info$exudate_nr_groups <- summary(counts)

# export to output folder (unfiltered and filtered)
exudates <- rownames_to_column(exudates, var = "Feature_ID")
write_csv(exudates, file.path(dirOutput,"exudates_unfiltered.csv"))
exudates <- dplyr::filter(exudates, count != 0)
write_csv(exudates, file.path(dirOutput,"exudates_filtered.csv"))
# make a list of features that are exudates so we can select on them later
exudates_list <- exudates$Feature_ID
analysis_info$nr_exudates<-length(exudates_list)

# transpose data.
exudates_meta <- select(exudates,-count)
tmp <- colnames(exudates_meta[2:ncol(exudates_meta)])
exudates_meta <- as.data.frame(t(exudates_meta))
colnames(exudates_meta) <- exudates$Feature_ID
exudates_meta <- dplyr::slice(exudates_meta, 2:nrow(exudates_meta))
rownames(exudates_meta) <- tmp
exudates_meta <-
  rownames_to_column(exudates_meta, var = "Treatment_ratio")
write_csv(exudates_meta, file.path(dirOutput,"exudates_per_sample.csv"))

# make dataframe with only the features that are marked as an exudate
df.exudates <- select(df.area, all_of(exudates_list))
df.exudates <- cbind(df.area[1:(M - 1)], df.exudates)
df.exudates <- filter(df.exudates, Time_Point == 0)
write_csv(df.exudates, file.path(dirOutput,"df.exudates.csv"))

df.no.exudates <- select(df.area,-all_of(exudates_list))

# create a list of features and the group that they are an exudate of, so that we can compare with the metabolized ones.
Exudated_groups<- select(exudates, -count)
Exudated_groups<-pivot_longer(Exudated_groups, cols= -Feature_ID, values_to = "peak_area", names_to = "group" )
Exudated_groups<-filter(Exudated_groups, peak_area > 0)

#now make a new column with the combination of featureID and the group and remove duplicates
Exudated_groups<-tidyr::unite(Exudated_groups, shared.name, c(Feature_ID, group), sep = "_", remove = FALSE)
Exudated_groups<-Exudated_groups %>% select(- peak_area)

analysis_info$nr_exudate_treatment_combinations<-length(Exudated_groups$shared.name)

rm(tmp.contr2, tmp.rest2)
