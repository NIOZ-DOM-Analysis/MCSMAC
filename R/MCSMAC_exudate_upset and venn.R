'MCSMAC_exudate_upset and venn.R'

# make an upset plot of exudates
# make a color pallet of the different treatments
cost.pal <- c("#4DAF4A", "#FF7F00", "#E41A1C", "#F781BF", "#984EA3")
All.figures <- list()
# create the input file for the upset plot (present absent data)
df.tmp <- exudates
df.tmp <- df.tmp[1:6]
df.tmp[which(df.tmp > 0, arr.ind = TRUE)] <- 1
df.tmp$Feature_ID <- exudates$Feature_ID
df.tmp[is.na(df.tmp)] <- 0
list1 <- colnames(df.tmp[, 2:ncol(df.tmp)])
for (i in list1) {
  df.tmp[[i]] <- as.integer(df.tmp[[i]])
}


upset.exudates <-
  UpSetR::upset(
    df.tmp,
    sets = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"),
    mainbar.y.label = "Number of Exudates \n Features Intersections",
    sets.x.label = "Exudates \n Features Per Treatment",
    #order.by = "freq",
    keep.order = TRUE,
    nintersects = NA,
    sets.bar.color = cost.pal,
    point.size = 3,
    line.size = 1,
    text.scale = 1.4
  )

upset.exudates

# save as pdf file in the figure folder
setwd(dirFigs)
pdf(
  file = "upset_exudates.pdf",
  onefile = FALSE,
  width = 11,
  height = 5.5
)
upset.exudates
dev.off()
setwd(wd.project)

# #save the plot in the figure object
All.figures[["Exudates"]]<-list()
All.figures$Exudates[["upset.plot"]]<-upset.exudates


# lets make a dataset which you can use to make the Venn Diagram where you have to put a total list of at http://www.interactivenn.net/
df.tmp <- exudates
df.tmp <- df.tmp[1:6]
df.tmp[which(df.tmp > 0, arr.ind = TRUE)] <- 1
df.tmp$Feature_ID <- exudates$Feature_ID
df.tmp$Feature_ID_nr<-1:nrow(df.tmp)

setwd(dirFigs)
fout2<-file(paste('VennDiagramInfo.txt', sep=""), 'w')
writeLines(c('Venn Diagram input for list of at http://www.interactivenn.net/'), fout2)
writeLines(c(" "), fout2)

tmp<-filter(df.tmp, Turf > 0)

writeLines(c("Turf", tmp$Feature_ID_nr), fout2)
writeLines(c(" "), fout2)

tmp<-filter(df.tmp, Coral_Turf > 0)

writeLines(c("Coral_Turf", tmp$Feature_ID_nr), fout2)
writeLines(c(" "), fout2)

tmp<-filter(df.tmp, Coral > 0)

writeLines(c("Coral", tmp$Feature_ID_nr), fout2)
writeLines(c(" "), fout2)

tmp<-filter(df.tmp, Coral_Dictyota > 0)

writeLines(c("Coral_Dictyota", tmp$Feature_ID_nr), fout2)
writeLines(c(" "), fout2)

tmp<-filter(df.tmp, Dictyota > 0)

writeLines(c("Dictyota", tmp$Feature_ID_nr), fout2)
writeLines(c(" "), fout2)

flush(fout2)
close(fout2)

setwd(wd.project)

rm(df.tmp, upset.exudates)




