'read_prep_rawfiles.R

Goal: load rawfiles and prep them for normalisation and transformations

Used on a macbook, so fileformats are with / ; it can be that you have to change all path formats to \\

Written by:
Milou Arts, NIOZ, NL, 2019


'
##### load files from folders ####
wd.project<-getwd()
setwd(dirRAW) # set wd to raw folder to read in files

orbitrapsequence <- read_csv("orbitrapsequence.csv")
rawpeakareas <- read_csv("POSTgapfilled.csv")
PREgapfilled <- read_csv("PREgapfilled.csv")
metadata <- read_csv("metadata.csv")

setwd(wd.project)
# load in files from Cytoscape based on the type of networking
if (networking.type == "FBMN") {
  dir_library_hits <- paste0(dir_analogs_on, '/DB_result')
  dir_analogs_hits <- paste0(dir_analogs_on)
  dir_node_info <- paste0(dir_analogs_on, '/clusterinfo_summary')
}

if (networking.type == "IIN") {
  dir_library_hits <- paste0(dir_analogs_on, '/DB_result')
  dir_analogs_hits <- paste0(dir_analogs_on)
  dir_node_info <- paste0(dir_analogs_on, '/clusterinfo_summary')
}

if (networking.type == "Classic") {
  dir_library_hits <- paste0(dir_analogs_off, '/result_specnets_DB')
  dir_analogs_hits <- paste0(dir_analogs_on, '/result_specnets_DB')
  dir_node_info <- paste0(dir_analogs_on, '/clusterinfo_summary')
}

if (MolNetEnh == "YES") {
  dir_library_hits <- paste0(dirCyto, '/MolNetEnhancer/DB_result')
  dir_ClassyFire_hits <-
    paste0(dirCyto, '/MolNetEnhancer/output_network')
  ClassyFire_hits <-
    read_tsv(paste0(dir_ClassyFire_hits, "/ClassyFireResults_Network.txt"))
  dir_node_info <-
    paste0(dirCyto, '/MolNetEnhancer/clusterinfo_summary')
}

if (Dereplicator == "YES" & MolNetEnh == "YES") {
  dir_derep <- paste0(dirCyto, '/MolNetEnhancer/Derep_output')
}
if (Dereplicator == "YES" &
    (MolNetEnh != "YES" | !exists("MolNetEnh"))) {
  dir_derep <- paste0(dirCyto, '/Dereplicator')
}

if (Dereplicator_plus == "YES") {
  dir_derepplus <- paste0(dirCyto, '/Dereplicator-Plus')
  setwd(dir_derepplus)
  temp <- list.files(pattern = "*.tsv")
  derep_plus_hits <- read_tsv(temp)
  setwd(wd.project)
}

setwd(wd.project)
setwd(dir_node_info)
temp <- list.files(pattern = "*.tsv")
node_info <- read_tsv(temp)
setwd(wd.project)

setwd(dir_library_hits)
temp <- list.files(pattern = "*.tsv")
library_hits <- read_tsv(temp)
setwd(wd.project)

setwd(dir_analogs_hits)
temp <- list.files(pattern = "*.tsv")
analogs_hits <- read_tsv(temp, col_types = cols(tags = col_character()))
setwd(wd.project)

if (Dereplicator == "YES"){
  setwd(dir_derep)
  temp <- list.files(pattern = "*.tsv")
  derep_hits <- read_tsv(temp)
  setwd(wd.project)}

setwd(wd.project)
setwd(dirRAW) #set wd to raw folder to read in files


# load files from Sirius if present from RAWdata folder (if not present temp will have lenght 0)
# if you have multiple files from SIRIUS because you have a lot of data, it will put everyting in one file!
temp <- list.files(pattern = "*summary_view-main.tsv")
if (length(temp) != 0) {
  sirius_files <- lapply(temp, function(x) {
    read_tsv(x)
  })
  SIRIUS <- do.call("rbind", lapply(sirius_files, as.data.frame))
}

setwd(dirOutput)
if (exists("SIRIUS")) {
  write_csv(SIRIUS, "SIRIUS.csv")
}

setwd(wd.project)

####PREP FILES####
##### MZmine OUTPUT####

# remove the .mzXML.Peak.area string from the filenames
temp <- as.character(colnames(rawpeakareas)) #move first row to temp
tmp <-  as.data.frame(matrix(ncol = ncol(rawpeakareas), nrow = 1))#make dataframe to store colnames in
tmp[1, ] <- sub(".mzXML Peak area", "", temp)  #change temp by removing .mzXML.Peak area in insert in rawpeakareas

colnames(rawpeakareas) <- tmp[1, ]

# if there is an extra empty column imported, that we don't need now.
# there are standard 12 columns imported that we don't want.
# I'm going to delete them by name, to be sure that if in the future column names change we catch it and not accedentally remove the first samples.

rawpeakareas <-
  rawpeakareas %>% select(-any_of(c("row identity (main ID)",
                                  "row identity (all IDs)",
                                  "row identity (main ID + details)",
                                  "row comment",
                                  "row number of detected peaks",
                                  "correlation group ID" ,
                                  "annotation network number",
                                  "best ion",
                                  "auto MS2 verify",
                                  "identified by n=",
                                  "partners",
                                  "neutral M mass")))

# if there are other columns with NA's that we also want to remove now, therefore we loop through the first 5 lines to find all columns with NA
temp <- c()

for (i in 1:(ncol(rawpeakareas))) {
  if (all(is.na(rawpeakareas[1:5, i])))
  {
    y <- i
    temp <- c(temp, y)
  }
}
rawpeakareas <- subset(rawpeakareas, select = -c(temp))

# rename the naming of feature number
rawpeakareas <- dplyr::rename(rawpeakareas, "feature_nr" = "row ID")


##  Do the same with the pregapfilled file
# remove the .mzXML.Peak.area string from the filenames
# remove the .mzXML.Peak.area string from the filenames
temp <-
  as.character(colnames(PREgapfilled)) #move column headers to temp
tmp <-
  as.data.frame(matrix(ncol = ncol(PREgapfilled), nrow = 1))#make dataframe to store colnames in
tmp[1, ] <-
  sub(".mzXML Peak area", "", temp)  #change temp by removing .mzXML.Peak area in insert in rawpeakareas

colnames(PREgapfilled) <- tmp[1, ]

# if there is an extra empty column imported, that we don't need now.
# there are standard 12 columns imported that we don't want.
# I'm going to delete them by name, to be sure that if in the future column names change we catch it and not accedentally remove the first samples.

PREgapfilled <-
  PREgapfilled %>% select(-any_of(c("row identity (main ID)",
                                    "row identity (all IDs)",
                                    "row identity (main ID + details)",
                                    "row comment",
                                    "row number of detected peaks",
                                    "correlation group ID" ,
                                    "annotation network number",
                                    "best ion",
                                    "auto MS2 verify",
                                    "identified by n=",
                                    "partners",
                                    "neutral M mass")))


# if there is still an extra empty column imported, it will be full of NA and we want to remove it
temp <- c()

for (i in 1:(ncol(PREgapfilled))) {
  if (all(is.na(PREgapfilled[1:5, i])))
  {
    y <- i
    temp <- c(temp, y)
  }
}
PREgapfilled <- subset(PREgapfilled, select = -c(temp))

# rename the naming of feature number
PREgapfilled <- dplyr::rename(PREgapfilled, "feature_nr" = 'row ID')


##### PREP Cytoscape output #####
# rename the column of feature number/scan nr/clusterindex
tmp <-
  which(colnames(node_info) == "cluster index") #lookup which colnumber is clusterindex to rename
colnames(node_info)[tmp] <-
  "feature_nr" #rename cluster.index to feature_nr

tmp <-
  which(colnames(node_info) == "componentindex") #lookup which colnumber is componentindex to rename
colnames(node_info)[tmp] <-
  "network" #rename componentindex to network

node_info <- dplyr::select(node_info,!contains("GNPSGROUP:"))
node_info <- dplyr::select(node_info,!contains("ATTRIBUTE"))
node_info <- dplyr::select(node_info,!num_range("G", 1:6))

tmp <- which(colnames(library_hits) == "#Scan#")
colnames(library_hits)[tmp] <- "feature_nr"

tmp <- which(colnames(analogs_hits) == "#Scan#")
colnames(analogs_hits)[tmp] <- "feature_nr"

# rename the library hit of the analogs
tmp <- which(colnames(analogs_hits) == "Compound_Name")
colnames(analogs_hits)[tmp] <- "Analog_LibraryID"


if (exists("derep_hits")) {
  tmp <- which(colnames(derep_hits) == "Scan")
  colnames(derep_hits)[tmp] <- "feature_nr"

}

if (exists("derep_plus_hits")) {
  tmp <- which(colnames(derep_plus_hits) == "Scan")
  colnames(derep_plus_hits)[tmp] <- "feature_nr"

}

if (exists("ClassyFire_hits")) {
  tmp <-
    which(colnames(ClassyFire_hits) == "cluster index") # lookup which colnumber is clusterindex to rename
  colnames(ClassyFire_hits)[tmp] <-
    "feature_nr" #rename cluster.index to feature_nr
}


# remove the brackets in analog hits and library hits
analogs_hits$Analog_LibraryID <-
  textclean::replace_non_ascii(analogs_hits$Analog_LibraryID)
library_hits$Compound_Name <-
  textclean::replace_non_ascii(library_hits$Compound_Name)
node_info$LibraryID <-
  textclean::replace_non_ascii(node_info$LibraryID)
if (exists("derep_hits")) {
derep_hits$Name <- textclean::replace_non_ascii(derep_hits$Name) }
if (exists("derep_plus_hits")) {
derep_plus_hits$Name <-
  textclean::replace_non_ascii(derep_plus_hits$Name)}


# in case you  have more problems with the compound names.
# library_hits$Compound_Name<-gsub("/[", "_", library_hits$Compound_Name)
# library_hits$Compound_Name<-gsub("/]", "_", library_hits$Compound_Name)
# library_hits$Compound_Name<-gsub("/(", "_", library_hits$Compound_Name)
# library_hits$Compound_Name<-gsub("/)", "_", library_hits$Compound_Name)

# analogs_hits$Analog_LibraryID<-gsub("/[", "_", analogs_hits$Analog_LibraryID)
# analogs_hits$Analog_LibraryID<-gsub("/]", "_", analogs_hits$Analog_LibraryID)
# analogs_hits$Analog_LibraryID<-gsub("/(", "_", analogs_hits$Analog_LibraryID)
# analogs_hits$Analog_LibraryID<-gsub("/)", "_", analogs_hits$Analog_LibraryID)



##########################################################
#######NEEDS WORK WHEN FILES ARE THERE ###################
##########################################################

# if you have SIRIUS and CANOPUS and ZODIAC data, we also have to rename the scan number
if (exists("SIRIUS")) {
  SIRIUS <- dplyr::rename(SIRIUS, "feature_nr" = 'scan')
}

if (exists("canopus")) {
  colnames(canopus)[1] <- "feature_nr"
}

if (exists("zodiac")) {
  colnames(zodiac)[1] <- "feature_nr"
}

if (exists("sirius_zodiac_anotations")) {
  colnames(sirius_zodiac_anotations)[1] <- "feature_nr"
}


###### Make feature info file #####
feature_info1 <- as.data.frame(rawpeakareas[, 1:3])

# join library hits, analog hits and super computer predictions
feature_info2 <-
  full_join(node_info,
            full_join(
              library_hits,
              analogs_hits,
              by = "feature_nr",
              suffix = c("_Library", "_Analog")
            ),
            by = "feature_nr")

feature_info1$feature_nr <- as.numeric(feature_info1$feature_nr)
feature_info2$feature_nr <- as.numeric(feature_info2$feature_nr)
feature_info <-
  join(feature_info1, feature_info2, by = "feature_nr")
rm(feature_info1)
rm(feature_info2)

if (exists("derep_hits")) {
  tmp <- derep_hits
  colnames(tmp) <- paste(colnames(tmp), "derep", sep = "_")
  tmp2 <- which(colnames(tmp) == "feature_nr_derep")
  colnames(tmp)[tmp2] <- "feature_nr"
  feature_info <- left_join(feature_info, tmp, by = "feature_nr")
}

if (exists("derep_plus_hits")) {
  tmp <- derep_plus_hits
  colnames(tmp) <- paste(colnames(tmp), "derep_plus", sep = "_")
  tmp2 <- which(colnames(tmp) == "feature_nr_derep_plus")
  colnames(tmp)[tmp2] <- "feature_nr"
  feature_info <- left_join(feature_info, tmp, by = "feature_nr")
}

if (exists("ClassyFire_hits")) {
  feature_info <-
    left_join(feature_info, ClassyFire_hits, by = "feature_nr")
}

if (exists("SIRIUS")) {
  feature_info <- left_join(feature_info, SIRIUS, by = "feature_nr")
}

feature_info$feature_nr <- as.character(feature_info$feature_nr)

# setwd(wd.project)
# setwd(dirOutput)
# write.csv(feature_info,"feature_info.csv", row.names = FALSE) #write all feature info availible
# setwd(wd.project)

###### Make feature ID #####
# we first make a dataframe with all the columns that we want to use.

df.featureID <- feature_info[, 1:3]
df.featureID <-
  cbind(
    df.featureID,
    feature_info$network,
    feature_info$LibraryID,
    feature_info$Analog_LibraryID
  )

if (exists("derep_hits")) {
  df.featureID <- cbind(df.featureID, feature_info$Name_derep)
}

if (exists("derep_plus_hits")) {
  df.featureID <- cbind(df.featureID, feature_info$Name_derep_plus)
}

if (exists("ClassyFire_hits")) {
  df.featureID <-
    cbind(
      df.featureID,
      feature_info$CF_kingdom,
      feature_info$CF_superclass,
      feature_info$CF_class,
      feature_info$CF_subclass
    )
}

if (exists("SIRIUS")) {
  df.featureID <- cbind(df.featureID, feature_info$sirius_formula)
}

# remove all feature_info$ parts of the column names
temp <- as.character(colnames(df.featureID)) #move first row to temp
tmp <-
  as.data.frame(matrix(ncol = ncol(df.featureID), nrow = 1))#make dataframe to store colnames in
temp <- gsub("[^0-9A-Za-z///' ]", "'" , temp)
temp <- sub("feature'info'", "", temp)
tmp[1, ] <- gsub("'", "_", temp)
colnames(df.featureID) <- tmp[1, ]


# concatenate the feature names from the different files (row.ID, row.m.z, row.retention.time, Library ID and Analog ID and sirius_formula when present, Classifire classes)
featureID <- c(df.featureID, sep = "_") #get the columns to combine
featureID$`row m/z` <- as.numeric(featureID$`row m/z`, digits = 4)
featureID$`row m/z` <-
  round(featureID$`row m/z`, digits = 4) #round digits of m/z
featureID$`row retention time` <-
  as.numeric(featureID$`row retention time`)
featureID$`row retention time` <-
  round(featureID$`row retention time`, digits = 4) #round digits of retention time
featureID <- do.call(paste, featureID) #concatenate
featureID <- as.data.frame(featureID) #make data frame

# in between we will save all the info we have on the features
featureID_info <- cbind(featureID, df.featureID)
df.featureID <- featureID_info
featureID_info <-
  join(featureID_info, feature_info, by = "feature_nr", type = "left")
setwd(wd.project)
setwd(dirOutput)
write.csv(featureID_info, "feature_info_final.csv", row.names = FALSE) #write all feature info availible
setwd(wd.project)

# make a new df with combined names
df <- cbind(featureID, rawpeakareas[, 5:ncol(rawpeakareas)])
df <- t(df)
df <- as.data.frame(df)

# let's make a safe file for in between
setwd(dirOutput) #set wd to output
# write.csv(df,"rawpeakareas_V1.csv",row.names = TRUE) #write first version of data, not cleaned

# we name the columns and the rows by naming columns and transposing
colnames(df) <- as.character(unlist(df[1, ]))
df <- df[-1, ]



# combine rawpeakareas with orbitrap sequence
shared.name <- as.data.frame(rownames(df))
colnames(shared.name) <- 'File Name'
df <- cbind(shared.name, df)
df1 <-
  right_join(orbitrapsequence, df, by = "File Name") #join matching info from orbitrap sequence
setwd(wd.project)
setwd(dirOutput)
write.csv(df1, "rawpeakareas.csv", row.names = TRUE) #write third version of data, not cleaned
setwd(wd.project)

###### PREGAPFILLED combined with metadata etc.

# make a new df with combined names

class(df.featureID$feature_nr) <- "character"
class(PREgapfilled$feature_nr) <- "character"
tmp <- select(df.featureID, feature_nr, featureID)
tmp2 <- cbind(PREgapfilled[, 1], PREgapfilled[, 5:ncol(PREgapfilled)])
df.pregap <- left_join(tmp, tmp2, by = "feature_nr")
df.pregap <- t(df.pregap)
df.pregap <- as.data.frame(df.pregap)

# let's make a safe file for in between
# setwd(dirOutput) #set wd to output
# write.csv(df.pregap,"pregapfilling_V1.csv",row.names = TRUE) #write first version of data, not cleaned

# we name the columns and the rows by naming columns and transposing
colnames(df.pregap) <- as.character(unlist(df.pregap[2, ]))
df.pregap <- df.pregap[-2, ]
df.pregap <- df.pregap[-1, ]
# write.csv(df.pregap,"pregapfilling_V2.csv",row.names = TRUE) #write second version of data, not cleaned


# combine rawpeakareas with orbitrap sequence
shared.name <- as.data.frame(rownames(df.pregap))
colnames(shared.name) <- 'File Name'
df.pregap <- cbind(shared.name, df.pregap)
df1.pregap <-
  right_join(orbitrapsequence, df.pregap, by = "File Name") #join matching info from orbitrap sequence
setwd(wd.project)
setwd(dirOutput)
write.csv(df1.pregap, "pregapfilling.csv", row.names = TRUE) #write third version of data, not cleaned
setwd(wd.project)

rm(sirius_files)
