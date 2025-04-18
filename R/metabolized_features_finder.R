'metabolized_features_finder.R

•	find features that change enough over time.


Written by:
Milou Arts, NIOZ, NL, 2019
Zach Quinlan, SDSU, USA, 2019



'
# again lets store all the input info in the analysis info object
analysis_info$metabolized_finder_DS<-DS.name
analysis_info$metabolized_fold_change<-foldchange
analysis_info$metabolized_minfactor <- log2(foldchange)
analysis_info$metabolized_test_group<-test_group
analysis_info$metabolized_Tmin<-Tmin
analysis_info$metabolized_Tmax<-Tmax
analysis_info$metabolized_sorting1<-sorting1
analysis_info$metabolized_sorting2<-sorting2
#make a copy of the dataset so we don't screw that one up
Metabolized_pass<-DS

# sort the data so when the dataset is split the corresponding samples are in the same row.
if(exists("sorting2")){
  tmp<-match(sorting2, colnames(Metabolized_pass))
  Metabolized_pass<-rename(Metabolized_pass, "sorting" = all_of(tmp))
  Metabolized_pass<-dplyr::arrange(Metabolized_pass, sorting)
  Metabolized_pass<-rename(Metabolized_pass, !!sorting2 := "sorting")}

tmp<-match(sorting1, colnames(Metabolized_pass))
Metabolized_pass<-rename(Metabolized_pass, "sorting" = all_of(tmp))
Metabolized_pass<-dplyr::arrange(Metabolized_pass, sorting)
Metabolized_pass<-rename(Metabolized_pass, !!sorting1 := "sorting")

# if there are samples that do not have a partner, remove them here.
if (exists("to.remove.value")){
  Metabolized_pass<-filter(Metabolized_pass, to.remove.column != to.remove.value)}

#seperate in T0 and Tend
metabolized_pass_T0<-dplyr::filter(Metabolized_pass, Time_Point == Tmin )
metabolized_pass_Tend<-dplyr::filter(Metabolized_pass, Time_Point == Tmax)


#if there is defined that rows should be copied, that happens now
ifelse(exists("nr.rep"), (
  metabolized_pass_T0<-metabolized_pass_T0 %>% slice(rep(1:n(), each= nr.rep))),
  FALSE
)

# make both datasets a dataframe of only the features (without metadata)
Metabolized_pass_T0<-as.data.frame(t(metabolized_pass_T0[,M:ncol(metabolized_pass_T0)]))
colnames(Metabolized_pass_T0)<- unlist(metabolized_pass_T0[test_group])

Metabolized_pass_Tend<-as.data.frame(t(metabolized_pass_Tend[,M:ncol(metabolized_pass_Tend)]))
colnames(Metabolized_pass_Tend)<-unlist(metabolized_pass_Tend[test_group])


# set factor for the minimum value to be Metabolized_pass
# if the feature has to be 2.5x the value of what was found in the T0 or /2.5, factor should be 2.5
minfactor<-log2(foldchange)
minfactor.neg<- -(log2(foldchange))

#if the log(Tfinal/Tstart) passes the set threshold, we fill in the difference of Tend-Tstart
Metabolized_pass<-Metabolized_pass_Tend
for (i in 1:nrow(Metabolized_pass)){
  for (j in 1:ncol(Metabolized_pass)){
    if(is.na(log2(Metabolized_pass_Tend[i,j]/Metabolized_pass_T0[i,j]) > minfactor | log2(Metabolized_pass_Tend[i,j]/Metabolized_pass_T0[i,j]) < minfactor.neg ))
    {Metabolized_pass[i,j]<-0} else {
      if(log2(Metabolized_pass_Tend[i,j]/Metabolized_pass_T0[i,j]) > minfactor | log2(Metabolized_pass_Tend[i,j]/Metabolized_pass_T0[i,j]) < minfactor.neg ){
        Metabolized_pass[i,j]<-(Metabolized_pass_Tend[i,j]-Metabolized_pass_T0[i,j])} else {Metabolized_pass[i,j]<-0}
    }}}

# now count how often a feature passes the metabolized threshold
Metabolized_pass$count<-apply(Metabolized_pass, 1, function(c)sum(c!=0))
counts<-as.factor(Metabolized_pass$count)
print(summary(counts))
analysis_info$metabolized_nr_of_samples<-summary(counts)


Metabolized_pass <- Metabolized_pass %>% as_tibble(.name_repair = "unique", rownames = NA) %>%  rownames_to_column("Feature_ID")
Metabolized_pass<-dplyr::filter(Metabolized_pass, count != 0)
write_csv(Metabolized_pass,file.path(dirOutput,"Metabolized_pass_filtered.csv"))

#make a list of the features and the group/sample it passed the threshold of
Metabolized_groups<- select(Metabolized_pass, -count)
Metabolized_groups<-pivot_longer(Metabolized_groups, cols= -Feature_ID, values_to = "difference", names_to = "group" )
Metabolized_groups<-filter(Metabolized_groups, difference != 0)
# remove all the suffixes that were added to the column headers.
Metabolized_groups$group<-gsub("...([0-9])([0-9])", "", Metabolized_groups$group)
Metabolized_groups$group<-gsub("...([0-9])", "", Metabolized_groups$group)
#now make a new column with the combination of featureID and the group but give it a temporary cut
Metabolized_groups<-tidyr::unite(Metabolized_groups, shared.name, c(Feature_ID, group), sep = "CUTHERE", remove = FALSE)
#Now we summarize the difference instead of only keeping one of the three
Metabolized_groups<-Metabolized_groups %>% group_by(shared.name) %>%
  dplyr::summarise(difference_mean = mean(difference), difference_median = median(difference), n())
#now get the featureID and group column back, and remake a right shared.name column since we are going to use that later
Metabolized_groups<-tidyr::separate(Metabolized_groups, shared.name, c("Feature_ID", "group"), sep = "CUTHERE", remove = TRUE)
Metabolized_groups<-tidyr::unite(Metabolized_groups, shared.name, c(Feature_ID, group), sep = "_", remove = FALSE)

# list features that are metabolized
Metabolized_pass_list<-Metabolized_pass$Feature_ID
analysis_info$metabolized_nr_of_features_unique<-length(Metabolized_pass_list)
analysis_info$metabolized_nr_of_features_x_treatment<-length(Metabolized_groups$Feature_ID)

Metabolized_pass_meta<-select(Metabolized_pass, - count)
Metabolized_pass_meta<-as.data.frame(t(Metabolized_pass_meta))
colnames(Metabolized_pass_meta)<-Metabolized_pass$Feature_ID
Metabolized_pass_meta<-dplyr::slice(Metabolized_pass_meta, 2:nrow(Metabolized_pass_meta))

Metabolized_pass_meta<-cbind(metabolized_pass_Tend[1:(M-1)], Metabolized_pass_meta)
write_csv(Metabolized_pass_meta, file.path(dirOutput,"Metabolized_pass_delta_meta.csv"))

df.metabolized<-select(df.area, all_of(Metabolized_pass_list))
df.metabolized<-cbind(df.area[1:(M-1)], df.metabolized)
write_csv(df.metabolized, file.path(dirOutput,"df.metabolized.csv"))

# remove objects we don't need anymore
rm(Metabolized_pass_T0, Metabolized_pass_Tend, metabolized_pass_T0, metabolized_pass_Tend, tmp)
setwd(wd.project)
