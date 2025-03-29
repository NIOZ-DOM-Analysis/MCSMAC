'sum_peak_areas.R


goal: to calculate the summed peak areas (sort of TICs but from cleaned data), make plots and do stats on it. First general figures later the figure for the paper.


written by: Milou Arts, NIOZ 2020'

#######                                                     #######
##  PLOT per treatment exudate vs not exudate at two timepoints  ##
#######                                                     #######
library(cowplot)
library(magick)
library(pdftools)
# take only features marked as exudates
# the value of exudates is the peak area of the feature at T0 of the corresponding sample (not corrected for control)
tmp<-select(exudates, -count)
tmp<-pivot_longer(tmp, -Feature_ID, names_to = 'Treatment_ratio', values_to = "x")
tmp.list<-levels(as.factor(tmp$Treatment_ratio))


setwd(dirFigs)
All.figures$Exudates[["TIC_plots"]]<-list()

#loop through the treatments and select only the features that are an exudate for that treatment
for (i in tmp.list){
  tmp2<-filter(tmp, Treatment_ratio == i)
  tmp2<-filter(tmp2, !is.na(x))
  feature.list<-tmp2$Feature_ID

  #make 2 datasets, one with the selected features, and one without, and select only the samples from the treatment group and summarize
  #exudates
  tmp3<-select(df.area, 1:(all_of(M)-1) ,all_of(feature.list))
  tmp3<-filter(tmp3, Treatment_ratio ==i)
  tmp3<- tmp3 %>% rowwise() %>% mutate(sum = sum( c_across((all_of(M)):ncol(tmp3))))
  tmp3<-tmp3 %>% select(Time_Point, Replicate, sum) %>% dplyr::rename( exudate = sum)


  #not-exudates
  tmp4<-select(df.area, -all_of(feature.list))
  tmp4<-filter(tmp4, Treatment_ratio ==i)
  tmp4<-tmp4 %>% rowwise() %>% mutate(sum = sum( c_across((all_of(M)):ncol(tmp4))))
  tmp4<-tmp4 %>% select(Replicate, sum) %>% dplyr::rename( not.exudate = sum)

  # join the two summaries
  tmp3<-full_join(tmp3, tmp4, by="Replicate")
  tmp3<-pivot_longer(tmp3, cols = c(exudate, not.exudate), values_to = "TIC", names_to = "category")
  tmp3<-tmp3 %>% group_by(category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC))

  # plot the average TIC per timepoint for both exudates as non-exudates
  tmp.plot<-ggplot(tmp3, aes(x=category, y=mean, fill = Time_Point))+
    geom_col(position = position_dodge())+
    geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2, position = position_dodge(.9) )+
    theme_classic()+
    scale_x_discrete(name = i, labels = c("Exudate", "Not Exudate"))+
    scale_y_continuous(name = "Summed Peak Areas")+
    scale_fill_discrete(name = "Time Point")

  ggsave(paste0("Average_TIC_", i,"_exudate_vs_notexudate.png"))
  All.figures$Exudates$TIC_plots[[paste0("Average_TIC_", i,"_exudate_vs_notexudate")]] <- tmp.plot

}



#######                                                                                               #######
##  PLOT average TIC at two timepoints, general exudate vs not exudate of all samples and of treatment specific    ##
#######                                                                                               #######
#use df.area
#split in exudates and not exudates
tmp<-select(df.area, 1:(all_of(M)-1), all_of(exudates_list)) #exudates
tmp1<-select(df.area, -all_of(exudates_list)) #not exudates

#sum all peak areas.
tmp<-tmp %>% rowwise() %>% mutate(sum = sum( c_across((all_of(M)):ncol(tmp))))
tmp1<-tmp1 %>% rowwise() %>% mutate(sum = sum( c_across((all_of(M)):ncol(tmp1))))

#select time point, replicate, sum column, treatment ratio
tmp<-select(tmp, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% dplyr::rename( exudate = sum)
tmp1<-select(tmp1, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% dplyr::rename( not.exudate = sum)

tmp2<-full_join(tmp, tmp1, by = 'File Name', suffix = c("", ".y"))
tmp2<-select(tmp2, - contains(".y"))
tmp2<-pivot_longer(tmp2, cols = c(exudate, not.exudate), values_to = "TIC", names_to = "category")
tmp3<-tmp2 %>% group_by(category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC))
tmp4<-tmp2 %>% group_by(Treatment_ratio, category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC))

tmp.plot<-ggplot(tmp3, aes(x=category, y=mean, fill = Time_Point))+
  geom_col(position = position_dodge())+
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2, position = position_dodge(.9) )+
  theme_classic()+
  scale_x_discrete(name = "", labels = c("Exudate", "Not Exudate"))+
  scale_y_continuous(name = "Summed Peak Areas")+
  scale_fill_discrete(name = "Time Point")
ggsave(paste0("Average_all samples_TIC_exudate_vs_notexudate_per_timepoint.png"))
All.figures$Exudates$TIC_plots[[paste0("Average_all samples_TIC_exudate_vs_notexudate_per_timepoint")]] <- tmp.plot

tmp.plot<-ggplot(tmp4, aes(x=Treatment_ratio, y=mean, fill = Time_Point))+
  geom_col(position = position_dodge())+
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2, position = position_dodge(.9) )+
  theme_classic()+
  scale_x_discrete(name = "")+
  scale_y_continuous(name = "Summed Peak Areas")+
  scale_fill_discrete(name = "Time Point")+
  facet_wrap(~category)
ggsave(paste0("Average_TIC_per_timepoint_per_treatment_split_in_exudates_and_non_exudates.png"))
All.figures$Exudates$TIC_plots[[paste0("Average_TIC_per_timepoint_per_treatment_split_in_exudates_and_non_exudates")]] <- tmp.plot

#######                                                                                 #######
##  PLOT average specific TIC at two timepoints, per treatment vs. not treatment specific    ##
#######                                                                                 #######

####
#now split the exudates into exudate specific for that treatment and not
tmp<-select(exudates, -count)
tmp<-pivot_longer(tmp, -Feature_ID, names_to = 'Treatment_ratio', values_to = "x")
tmp.list<-levels(as.factor(tmp$Treatment_ratio))


for (i in tmp.list){
  #select the features that are specific fot that treatment
  tmp2<-filter(tmp, Treatment_ratio == i)
  tmp2<-filter(tmp2, !is.na(x))
  feature.list<-tmp2$Feature_ID

  # use df.area
  # split in exudates and not exudates
  tmp3<-select(df.area, 1:(all_of(M)-1), all_of(feature.list)) #specific exudates
  tmp4<-select(df.area, 1:(all_of(M)-1), all_of(exudates_list)) %>% select(- all_of(feature.list)) #exudates but not in that treatment
  tmp5<-select(df.area, -all_of(exudates_list)) # no exudates at all

  # sum all peak areas.
  tmp3<-tmp3 %>% rowwise() %>% mutate(sum = sum( c_across(all_of(M):ncol(tmp3))))
  tmp4<-tmp4 %>% rowwise() %>% mutate(sum = sum( c_across(all_of(M):ncol(tmp4))))
  tmp5<-tmp5 %>% rowwise() %>% mutate(sum = sum( c_across(all_of(M):ncol(tmp5))))

  # select the sum, time point and replicate.
  tmp3<-select(tmp3, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( specific.exudate = sum)
  tmp4<-select(tmp4, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( other.exudate = sum)
  tmp5<-select(tmp5, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( not.exudate = sum)

  # join datasets
  tmp6<-full_join(tmp3, tmp4, by = 'File Name', suffix = c("", ".y"))
  tmp6<-select(tmp6, - contains(".y"))
  tmp6<-full_join(tmp6, tmp5, by = 'File Name', suffix = c("", ".y"))
  tmp6<-select(tmp6, - contains(".y"))

  tmp6<-pivot_longer(tmp6, cols = c(specific.exudate, other.exudate, not.exudate), values_to = "TIC", names_to = "category")

  #summarize data
  tmp6<-tmp6 %>% group_by(Treatment_ratio, category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC))

  #plotdata
  tmp.plot<-ggplot(tmp6, aes(x=Treatment_ratio, y=mean, fill = Time_Point))+
    geom_col(position = position_dodge())+
    geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2, position = position_dodge(.9) )+
    theme_classic()+
    scale_x_discrete(paste0("Specific exudates of ", i))+
    scale_y_continuous(name = "Summed Peak Areas")+
    scale_fill_discrete(name = "Time Point")+
    facet_wrap(~category)
  ggsave(paste0("notexudate_vs_specific exudates of ", i, "_vs_other exudates per treatment.png"), width = 15, height = 8)
  All.figures$Exudates$TIC_plots[[paste0("notexudate_vs_specific exudates of ", i, "_vs_other exudates per treatment")]] <- tmp.plot

  tmp.plot<-ggplot(tmp6, aes(x = category, y = mean, fill = Time_Point)) +
    geom_col(position = position_dodge()) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                  width = 0.2,
                  position = position_dodge(.9)) +
    theme_classic() +
    scale_x_discrete(paste0("Features of ", i)) +
    scale_y_continuous(name = "Summed Peak Areas") +
    scale_fill_discrete(name = "Time Point") +
    facet_wrap( ~ Treatment_ratio)
  ggsave(paste0("TIC Features of ", i, " in other treatments.png"), width = 15, height = 8)
  All.figures$Exudates$TIC_plots[[paste0("TIC Features of ", i, " in other treatments")]] <- tmp.plot
}




setwd(wd.project)

#######                                      #######
##  PLOT figures for the paper and do stats       ##
#######                                      #######
# therefore we make a stats file for this.
setwd(dirOutput)
fout<-file(paste('stats summed peak areas.txt', sep=""), 'w')
writeLines(c("No exudates, without control group"), fout)
writeLines(c(" "), fout)

# Figure will consist of 3 barplots
# first no exudates.
tmp1<-select(df.area, -all_of(exudates_list)) #not exudates
tmp1<-tmp1 %>% rowwise() %>% mutate(sum = sum( c_across(all_of(M):ncol(tmp1)))) #sum all peak areas
tmp1<-select(tmp1, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( not.exudate = sum)
tmp1<-pivot_longer(tmp1, cols = not.exudate, values_to = "TIC", names_to = "category")
tmp1<-tmp1 %>% filter(Treatment_ratio != "Control")

#we are going to do stats on this part before we summarise tmp1
tmp<-pivot_wider(tmp1, names_from = Time_Point, values_from = TIC)
tmp <- tmp %>% rename("TP_28" = "28", "TP_0" = "0")
TP_28<-tmp$TP_28[!is.na(tmp$TP_28)]
TP_0<-tmp$TP_0[!is.na(tmp$TP_0)]
t.test.tmp<-t.test(TP_0, TP_28)
writeLines(c(t.test.tmp$method), fout)
writeLines(c(t.test.tmp$alternative), fout)
writeLines(c("t =", t.test.tmp$statistic[1]), fout)
writeLines(c("df =", t.test.tmp$parameter), fout)
writeLines(c("pvalue =", t.test.tmp$p.value), fout)
writeLines(c("conf.int %", t.test.tmp$conf.int[2]), fout)
writeLines(c("conf.int =", t.test.tmp$conf.int[1]), fout)
writeLines(c("estimate mean of x =", t.test.tmp$estimate[1]), fout)
writeLines(c("estimate mean of y =", t.test.tmp$estimate[2]), fout)
writeLines(c(" "), fout)
writeLines(c(" "), fout)

tmp1<-tmp1 %>% group_by(category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC))
#add column with "treatment ratio
tmp1$Treatment_ratio <-c("All Samples", "All Samples")
tmp1 <- tmp1 %>% relocate(Treatment_ratio)

#second. all exudates, split out over different groups
tmp2<-select(df.area, 1:(all_of(M)-1), all_of(exudates_list))
tmp2<-tmp2 %>% rowwise() %>% mutate(sum = sum( c_across(all_of(M):ncol(tmp2)))) #sum all peak areas
tmp2<-select(tmp2, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( all.exudates = sum)
tmp2<-pivot_longer(tmp2, cols = all.exudates, values_to = "TIC", names_to = "category")
#now do stats again
tmp.list<-levels(factor(tmp2$Treatment_ratio))
for (i in tmp.list){
  tmp<-tmp2 %>% dplyr::filter(Treatment_ratio == i)
tmp<-pivot_wider(tmp, names_from = Time_Point, values_from = TIC)
tmp <- tmp %>% rename("TP_28" = "28", "TP_0" = "0")
TP_28<-tmp$TP_28[!is.na(tmp$TP_28)]
TP_0<-tmp$TP_0[!is.na(tmp$TP_0)]
t.test.tmp<-t.test(TP_28, mu = TP_0)
writeLines(c("all exudates split over different groups"), fout)
writeLines(i, fout)
writeLines(c(t.test.tmp$method), fout)
writeLines(c(t.test.tmp$alternative), fout)
writeLines(c("t =", t.test.tmp$statistic[1]), fout)
writeLines(c("df =", t.test.tmp$parameter), fout)
writeLines(c("pvalue =", t.test.tmp$p.value), fout)
writeLines(c("conf.int %", t.test.tmp$conf.int[2]), fout)
writeLines(c("conf.int =", t.test.tmp$conf.int[1]), fout)
writeLines(c("estimate mean of x =", t.test.tmp$estimate[1]), fout)
writeLines(c("estimate mean of y =", t.test.tmp$estimate[2]), fout)
writeLines(c(" "), fout)
writeLines(c(" "), fout)
}

tmp2<-tmp2 %>% group_by(Treatment_ratio, category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC), n=n())
# drop the control group
tmp2 <- tmp2 %>% filter(Treatment_ratio != "Control")

#######
# third all the specific exudates we loop through the treatments with tmp.list
tmptmp<-select(exudates, -count)
tmptmp<-pivot_longer(tmptmp, -Feature_ID, names_to = 'Treatment_ratio', values_to = "x")
tmp.list<-levels(as.factor(tmptmp$Treatment_ratio))
rm(tmp4) #remove any old tmp4 before making this

for (i in tmp.list){
  #select the features that are specific fot that treatment
  tmp3<-filter(tmptmp, Treatment_ratio == i)
  tmp3<-filter(tmp3, !is.na(x))
  feature.list<-tmp3$Feature_ID

  #use df.area
  #split in exudates and not exudates
  tmp3<-select(df.area, 1:(all_of(M)-1), all_of(feature.list)) #specific exudates
    #sum all peak areas.
  tmp3<-tmp3 %>% filter(Treatment_ratio == i)
  tmp3<-tmp3 %>% rowwise() %>% mutate(sum = sum( c_across(M:ncol(tmp3))))
   #select the sum, time point and replicate.
  tmp3<-select(tmp3, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( specific.exudate = sum)
  tmp3<-pivot_longer(tmp3, cols = specific.exudate, values_to = "TIC", names_to = "category")

  #and here we do stats again in between
  tmp<-pivot_wider(tmp3, names_from = Time_Point, values_from = TIC)
  tmp <- tmp %>% rename("TP_28" = "28", "TP_0" = "0")
  TP_28<-tmp$TP_28[!is.na(tmp$TP_28)]
  TP_0<-tmp$TP_0[!is.na(tmp$TP_0)]
  t.test.tmp<-t.test(TP_28, mu = TP_0)
  writeLines(c("specific exudates per treatment"), fout)
  writeLines(i, fout)
  writeLines(c(t.test.tmp$method), fout)
  writeLines(c(t.test.tmp$alternative), fout)
  writeLines(c("t =", t.test.tmp$statistic[1]), fout)
  writeLines(c("df =", t.test.tmp$parameter), fout)
  writeLines(c("pvalue =", t.test.tmp$p.value), fout)
  writeLines(c("conf.int %", t.test.tmp$conf.int[2]), fout)
  writeLines(c("conf.int =", t.test.tmp$conf.int[1]), fout)
  writeLines(c("estimate mean of x =", t.test.tmp$estimate[1]), fout)
  writeLines(c("estimate mean of y =", t.test.tmp$estimate[2]), fout)
  writeLines(c(" "), fout)
  writeLines(c(" "), fout)


  tmp3<-tmp3 %>% group_by(Treatment_ratio, category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC))


  # store these values in a new object because we are going to loop over tmp3
  if (exists('tmp4')){
  tmp4 <-rbind(tmp4,tmp3)} else { tmp4 <- tmp3}
}


#######
# fourth all exudates - all the specific exudates we loop through the treatments with tmp.list

tmptmp<-select(exudates, -count)
tmptmp<-pivot_longer(tmptmp, -Feature_ID, names_to = 'Treatment_ratio', values_to = "x")
tmp.list<-levels(as.factor(tmptmp$Treatment_ratio))
rm(tmp5, tmp6) #remove any old tmp5 and 6 before making this

for (i in tmp.list){
  #select the features that are specific fot that treatment
  tmp5<-filter(tmptmp, Treatment_ratio == i)
  tmp5<-filter(tmp5, !is.na(x))
  feature.list<-tmp5$Feature_ID

  #use df.area
  #split in exudates and not exudates
  tmp5<-select(df.area, 1:(all_of(M)-1), all_of(exudates_list))
  tmp5<-select(tmp5, -all_of(feature.list)) #remove specific exudates
  #sum all peak areas.
  tmp5<-tmp5 %>% rowwise() %>% mutate(sum = sum( c_across(M:ncol(tmp5))))
  #select the sum, time point and replicate.
  tmp5<-select(tmp5, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( not.specific.exudate = sum)
  tmp5<-pivot_longer(tmp5, cols = not.specific.exudate, values_to = "TIC", names_to = "category")
  tmp5<-tmp5 %>% filter(Treatment_ratio == i)

  #and here we are going to do stats again
  tmp<-pivot_wider(tmp5, names_from = Time_Point, values_from = TIC)
  tmp <- tmp %>% rename("TP_28" = "28", "TP_0" = "0")
  TP_28<-tmp$TP_28[!is.na(tmp$TP_28)]
  TP_0<-tmp$TP_0[!is.na(tmp$TP_0)]
  t.test.tmp<-t.test(TP_28, mu = TP_0)
  writeLines(c("all exudates per treatment -specific exudates"), fout)
  writeLines(i, fout)
  writeLines(c(t.test.tmp$method), fout)
  writeLines(c(t.test.tmp$alternative), fout)
  writeLines(c("t =", t.test.tmp$statistic[1]), fout)
  writeLines(c("df =", t.test.tmp$parameter), fout)
  writeLines(c("pvalue =", t.test.tmp$p.value), fout)
  writeLines(c("conf.int %", t.test.tmp$conf.int[2]), fout)
  writeLines(c("conf.int =", t.test.tmp$conf.int[1]), fout)
  writeLines(c("estimate mean of x =", t.test.tmp$estimate[1]), fout)
  writeLines(c("estimate mean of y =", t.test.tmp$estimate[2]), fout)
  writeLines(c(" "), fout)
  writeLines(c(" "), fout)

  #back to making plots
  tmp5<-tmp5 %>% group_by(Treatment_ratio, category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC))


  # store these values in a new object because we are going to loop over tmp3
  if (exists('tmp6')){
    tmp6 <-rbind(tmp6,tmp5)} else { tmp6 <- tmp5}
}


F1<-factor(tmp1$Treatment_ratio)
F2<-factor(tmp2$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))
F3<-factor(tmp4$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))

# costumate color pallet for "Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"
cust.pal<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")

plot1 <-
  ggplot(tmp1, aes(
    x = F1,
    y = mean,
    fill = F1,
    alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Ambient features")) +
  scale_y_continuous(name = NULL, limits = c(0, 2.5E9)) +
  scale_fill_manual(values = c("#377EB8"))+
  scale_alpha_manual(values = c(0.45 , 1))+
  guides(color = "none", fill = "none", alpha = "none", y = "none")
plot1


plot2 <-
  ggplot(tmp2, aes(
    x = F2,
    y = mean,
    fill = F2,
    alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("All Exudates")) +
  scale_y_continuous(name = NULL, limits = c(0, 2.55E9), n.breaks = 6) +
  scale_fill_manual(name = "Time Point", values = cust.pal) +
  scale_alpha_manual(values = c(0.45 , 1))+
  guides(color = "none", fill = "none", alpha = "none")
plot2

plot3 <-
  ggplot(tmp4, aes(
    x = F3,
    y = mean,
    fill = F3,
    alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Treatment specific Exudates")) +
  scale_y_continuous(name = "Summed Peak Areas", limits = c(0, 2.55E9), n.breaks = 6) +
  scale_fill_manual(name = "Treatments", values = cust.pal) +
  scale_alpha_manual(name = "Time Point", values = c(0.45 , 1))+
  guides(color = "none", fill = "none")+
  theme(legend.position="bottom")
plot3

#exudates that are not specific
plot4 <-
  ggplot(tmp6, aes(
    x = F2,
    y = mean,
    fill = F2,
    alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Not treatment specific Exudates")) +
  scale_y_continuous(name = NULL, limits = c(0, 2.55E9), n.breaks = 6) +
  scale_fill_manual(name = "Time Point", values = cust.pal) +
  scale_alpha_manual(values = c(0.45 , 1))+
  guides(color = "none", fill = "none", alpha = "none", y = "none")
plot4


#get legend to plot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend<-get_legend(plot3)

plot3 <- plot3 + theme(legend.position="none")


#difference in peak area of exudates of all 5 treatments
tmp7<-filter(exudates, count == 5)
# tmp<-select(tmp, -count)
# tmp<-pivot_longer(tmp, -Feature_ID, names_to = 'Treatment_ratio', values_to = "x")
tmp.list<-levels(as.factor(tmp7$Feature_ID))

tmp7<-select(df.area, 1:(all_of(M)-1), all_of(tmp.list))
tmp7<-tmp7 %>% rowwise() %>% mutate(sum = sum( c_across(M:ncol(tmp7)))) #sum all peak areas
tmp7<-select(tmp7, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( all.exudates = sum)
tmp7<-pivot_longer(tmp7, cols = all.exudates, values_to = "TIC", names_to = "category")

tmp.list<-levels(factor(tmp7$Treatment_ratio))
for (i in tmp.list){
  tmp<-tmp7 %>% dplyr::filter(Treatment_ratio == i)
  tmp<-pivot_wider(tmp, names_from = Time_Point, values_from = TIC)
  tmp <- tmp %>% rename("TP_28" = "28", "TP_0" = "0")
  TP_28<-tmp$TP_28[!is.na(tmp$TP_28)]
  TP_0<-tmp$TP_0[!is.na(tmp$TP_0)]
  t.test.tmp<-t.test(TP_28, mu = TP_0)
  writeLines(c("exudates present in all 'specific exudates' groups "), fout)
  writeLines(i, fout)
  writeLines(c(t.test.tmp$method), fout)
  writeLines(c(t.test.tmp$alternative), fout)
  writeLines(c("t =", t.test.tmp$statistic[1]), fout)
  writeLines(c("df =", t.test.tmp$parameter), fout)
  writeLines(c("pvalue =", t.test.tmp$p.value), fout)
  writeLines(c("conf.int %", t.test.tmp$conf.int[2]), fout)
  writeLines(c("conf.int =", t.test.tmp$conf.int[1]), fout)
  writeLines(c("estimate mean of x =", t.test.tmp$estimate[1]), fout)
  writeLines(c("estimate mean of y =", t.test.tmp$estimate[2]), fout)
  writeLines(c(" "), fout)
  writeLines(c(" "), fout)
}


tmp7<-tmp7 %>% group_by(Treatment_ratio, category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC), n=n())
# drop the control group
#tmp <- tmp %>% filter(Treatment_ratio != "Control")
F4<-factor(tmp7$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))
cust.pal2<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3", "#98F5FF")

plot5<-ggplot(tmp7, aes(
  x = F4,
  y = mean,
  fill = F4,
  alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Marked as Exudates in all 5 treatments")) +
  scale_y_continuous(name = NULL, limits = c(0, 4E8), n.breaks = 4) +
  scale_fill_manual(name = "Time Point", values = cust.pal2) +
  scale_alpha_manual(values = c(0.45 , 1))+
  guides(color = "none", fill = "none", alpha = "none")
plot5

#without control
tmp8<-dplyr::filter(tmp7, Treatment_ratio != "Control")
F5<-factor(tmp8$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))
cust.pal<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")
tmp8$Label<-c("n=","321","n=", "321","n=", "321","n=", "321","n=", "321")


plot6<-ggplot(tmp8, aes(
  x = F5,
  y = mean,
  fill = F5,
  alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Marked as Exudates in all 5 treatments")) +
  scale_y_continuous(name = NULL, limits = c(0, 4E8), n.breaks = 4) +
  scale_fill_manual(name = "Time Point", values = cust.pal) +
  scale_alpha_manual(values = c(0.45 , 1))+
  geom_text(aes(label = Label, y = 4E8), position = position_dodge(0.7))+
  guides(color = "none", fill = "none", alpha = "none")
plot6


#now on the big scale
plot6.1<-ggplot(tmp8, aes(
  x = F5,
  y = mean,
  fill = F5,
  alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Marked as Exudates in all 5 treatments")) +
  scale_y_continuous(name = NULL, limits = c(0, 2.55E9), n.breaks = 6) +
  scale_fill_manual(name = "Time Point", values = cust.pal) +
  scale_alpha_manual(values = c(0.45 , 1))+
  guides(color = "none", fill = "none", alpha = "none")
plot6.1

#lay the features present in all treatments over the treatment specific columns
tmp9<-bind_cols(tmp2, tmp8)
cust.pal<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")
colnames(tmp9)<- c("Treatment_ratio", "category", "Time_point", "mean_all", "sd_all", "n_all", "Treatment_ratio2", "Category2", "Time_Point2", "mean_part", "sd_part", "n")
F6<-factor(tmp9$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))
tmp9$Label<-c("n=","6365","n=", "6365","n=", "6365","n=", "6365","n=", "6365")

plot7<-ggplot()+
  geom_col(data = tmp9, aes(F6, y = mean_all, alpha = Time_point, fill = F5),position = position_dodge(), width = 0.9) +
  geom_col(data = tmp9, aes(F6, y = mean_part, alpha = Time_point),position = position_dodge(), width = 0.9)+
  geom_errorbar(data = tmp9, aes(x= F5, ymin = mean_all - sd_all, ymax = mean_all + sd_all), position = position_dodge2(width = 0.2, padding = 0.8)) +
  geom_errorbar(data = tmp9, aes(x= F5, ymin = mean_part - sd_part, ymax = mean_part + sd_part), position = position_dodge2(width = 0.2, padding = 0.8)) +
  theme_classic() +
  scale_x_discrete(paste0("All Exudates")) +
  scale_y_continuous(name = NULL, limits = c(0, 2.55E9), n.breaks = 6) +
  scale_fill_manual(name = "Time Point", values = cust.pal) +
  scale_alpha_manual(values = c(0.45 , 1))+
  geom_text(aes(label = Label, y = 2.5E9, x=F6, alpha = Time_point), data=tmp9,  position = position_dodge(0.8), vjust = -0.5)+
  guides(color = "none", fill = "none", alpha = "none")
plot7



#difference in peak area of unique exudates of all 5 treatments
tmp10<-filter(exudates, count == 1)
tmp10<-select(tmp10, -count)
tmp10<-pivot_longer(tmp10, -Feature_ID, names_to = 'Treatment_ratio', values_to = "x")
tmp.list<-levels(as.factor(tmp10$Treatment_ratio))
rm(tmp11, tmp12) #remove any old tmp11 or 12 before making this

for (i in tmp.list){
  #select the features that are specific fot that treatment
  tmp11<-filter(tmp10, Treatment_ratio == i)
  tmp11<-filter(tmp11, !is.na(x))
  feature.list<-tmp11$Feature_ID

  #use df.area
  #split in exudates and not exudates
  tmp11<-select(df.area, 1:(all_of(M)-1), all_of(feature.list))
  #sum all peak areas.
  tmp11<-tmp11 %>% rowwise() %>% mutate(sum = sum( c_across(M:ncol(tmp11))))
  tmp11<-tmp11 %>% filter(Treatment_ratio == i)
  #select the sum, time point and replicate.
  tmp11<-select(tmp11, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( not.specific.exudate = sum)
  tmp11<-pivot_longer(tmp11, cols = not.specific.exudate, values_to = "TIC", names_to = "category")

  #lets do stats again
  # tmp<-pivot_wider(tmp11, names_from = Time_Point, values_from = TIC)
  # tmp <- tmp %>% dplyr::rename("TP_28" = "28", "TP_0" = "0")
  # TP_28<-tmp$TP_28[!is.na(tmp$TP_28)]
  # TP_0<-tmp$TP_0[!is.na(tmp$TP_0)]
  # t.test.tmp<-t.test(TP_28, mu = TP_0)
  # writeLines(c("unique treatment exudates per treatment"), fout)
  # writeLines(i, fout)
  # writeLines(c(t.test.tmp$method), fout)
  # writeLines(c(t.test.tmp$alternative), fout)
  # writeLines(c("t =", t.test.tmp$statistic[1]), fout)
  # writeLines(c("df =", t.test.tmp$parameter), fout)
  # writeLines(c("pvalue =", t.test.tmp$p.value), fout)
  # writeLines(c("conf.int %", t.test.tmp$conf.int[2]), fout)
  # writeLines(c("conf.int =", t.test.tmp$conf.int[1]), fout)
  # writeLines(c("estimate mean of x =", t.test.tmp$estimate[1]), fout)
  # writeLines(c("estimate mean of y =", t.test.tmp$estimate[2]), fout)
  # writeLines(c(" "), fout)
  # writeLines(c(" "), fout)

  #back to making plots
  tmp11<-tmp11 %>% group_by(Treatment_ratio, category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC))


  # store these values in a new object because we are going to loop over tmp3
  if (exists('tmp12')){
    tmp12 <-rbind(tmp12,tmp11)} else { tmp12 <- tmp11}
}

flush(fout)
close(fout)

setwd(wd.project)

F7<-factor(tmp12$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))
cust.pal<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3", "#98F5FF")
tmp12$Label<-c("n=","269","n=", "571","n=", "363","n=", "534","n=", "596")

plot8<-ggplot(tmp12, aes(
  x = F7,
  y = mean,
  fill = F7,
  alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Unique Exudates in specific treatments")) +
  scale_y_continuous(name = NULL, limits = c(0, 2.55E9))+
  scale_fill_manual(name = "Time Point", values = cust.pal) +
  scale_alpha_manual(values = c(0.45 , 1))+
  guides(color = "none", fill = "none", alpha = "none")

plot8.1<-ggplot(tmp12, aes(
  x = F7,
  y = mean,
  fill = F7,
  alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Unique Exudates in specific treatments")) +
  scale_y_continuous(name = "Summed Peak Areas") +
  scale_fill_manual(name = "Time Point", values = cust.pal) +
  scale_alpha_manual(values = c(0.45 , 1))+
  # geom_text(aes(label = Label, y = 4E8, x=F7, alpha = Time_Point),  position = position_dodge(0.7))+
  guides(color = "none", fill = "none", alpha = "none")
plot8.1

tmp9.1<-bind_cols(tmp4, tmp12)
cust.pal<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")
colnames(tmp9.1)<- c("Treatment_ratio", "category", "Time_point", "mean_all", "sd_all", "Treatment_ratio2", "Category2", "Time_Point2", "mean_part", "sd_part")
F6<-factor(tmp9.1$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))
tmp9.1$Label<-c("n=","1705","n=", "2659","n=", "3309","n=", "3191","n=", "3444")


plot7.1<-ggplot()+
  geom_col(data = tmp9.1, aes(F6, y = mean_all, alpha = Time_point, fill = F5),position = position_dodge(), width = 0.9) +
  geom_col(data = tmp9.1, aes(F6, y = mean_part, alpha = Time_point),position = position_dodge(), width = 0.9)+
  geom_errorbar(data = tmp9.1, aes(x= F5, ymin = mean_all - sd_all, ymax = mean_all + sd_all), position = position_dodge2(width = 0.2, padding = 0.8)) +
  geom_errorbar(data = tmp9.1, aes(x= F5, ymin = mean_part - sd_part, ymax = mean_part + sd_part), position = position_dodge2(width = 0.2, padding = 0.8)) +
  theme_classic() +
  scale_x_discrete(paste0("Treatment specific Exudates")) +
  scale_y_continuous(name = "Summed Peak Areas", limits = c(0, 2.55E9), n.breaks = 6) +
  scale_fill_manual(name = "Time Point", values = cust.pal) +
  scale_alpha_manual(values = c(0.45 , 1))+
  geom_text(aes(label = Label, y = 2.5E9, x=F6, alpha = Time_point), data=tmp9.1,  position = position_dodge(0.8), vjust = -0.5)+
  guides(color = "none", fill = "none", alpha = "none")
plot7.1




plot9<-plot_grid(plot3, plot8+guides(color = "none", y ="none", fill = "none"), plot6, legend, ncol = 4, nrow = 1, rel_widths=c(2,2,2,0.5)) #version with all exudates
plot10<-plot_grid(plot7, plot4, plot1, legend, ncol = 4, nrow = 1, rel_widths=c(2.2,2.2,0.6,0.5)) #version with non-specific exudates

plot11<-plot_grid(plot7.1, plot7, plot8.1, plot6, ncol = 2, nrow = 2 , labels = c("B", "C", "D", "E"), align = "v")


plot1
plot2
plot3
plot4
plot5
plot6
plot7
plot8
plot9
plot10

ggsave(paste0("Fig2_plot1_TIC_All_samples_Not_Exudates.png"), plot1, path = dirFigs, width = 4, height = 5)
ggsave(paste0("Fig2_plot2_TIC_Per_treatments_All_Exudates.png"), plot2, path = dirFigs, width = 6, height = 5)
ggsave(paste0("Fig2_plot3_TIC_Per_treatments_Treatment Specific_Exudates.png"), plot3, path =dirFigs, width = 6, height = 5)
ggsave(paste0("Fig2_plot4_TIC_Per_treatments_NOT Treatment Specific_Exudates.png"), plot4, path =dirFigs, width = 6, height = 5)
ggsave(paste0("Fig2_plot5_TIC_Per_treatments plus control_Marked Exudates in all 5 treatments.png"), path =dirFigs, plot5, width = 6, height = 5)
ggsave(paste0("Fig2_plot6_TIC_Per_treatments_Marked Exudates in all 5 treatments.png"), plot6, path =dirFigs, width = 6, height = 5)
ggsave(paste0("Fig2_plot7_TIC_Per_treatments_Treatment Specific Exudates with shared exudates marked.png"), plot7, path =dirFigs, width = 6, height = 5)
ggsave(paste0("Fig2_plot8_TIC_Per_treatments_Unique Exudate in specific treatment.png"), plot8, path =dirFigs, width = 6, height = 5)
ggsave(paste0("Fig2_plot8.1_TIC_Per_treatments_Unique Exudate in specific treatment no set scale.png"), plot8.1, path =dirFigs, width = 6, height = 5)
ggsave(paste0("Fig2_plot9_TIC_Per_treatments_Treatment specific exudates split in unique and shared exudates.png"), plot9, path =dirFigs, width = 15, height = 5)
ggsave(paste0("Fig2_plot10_TIC_Per_treatments_Treatment specific exudates split in unique and shared exudates.png"), plot10, path =dirFigs, width = 15, height = 5)

All.figures$Exudates$Summed_Peaks[["Figure 1"]]<-list()
All.figures$Exudates$Summed_Peaks[["Plot1_TIC_All_samples_Not_Exudates"]]<-plot1
All.figures$Exudates$Summed_Peaks[["Plot2_TIC_Per_treatments_All_Exudates"]]<-plot2
All.figures$Exudates$Summed_Peaks[["Plot3_TIC_Per_treatments_Treatment Specific_Exudates"]]<-plot3
All.figures$Exudates$Summed_Peaks[["Plot4_TIC_Per_treatments_NOT Treatment Specific_Exudates"]]<-plot4
All.figures$Exudates$Summed_Peaks[["Plot5_TIC_Per_treatments plus control_Marked Exudates in all 5 treatments"]]<-plot5
All.figures$Exudates$Summed_Peaks[["Plot6_TIC_Per_treatments_Marked Exudates in all 5 treatments"]]<-plot6
All.figures$Exudates$Summed_Peaks[["Plot7_TIC_Per_treatments_Treatment Specific Exudates with shared exudates marked"]]<-plot7
All.figures$Exudates$Summed_Peaks[["Plot8_TIC_Per_treatments_Unique Exudate in specific treatment"]]<-plot8
All.figures$Exudates$Summed_Peaks[["Plot8.1_TIC_Per_treatments_Unique Exudate in specific treatment no set scale"]]<-plot8.1
All.figures$Exudates$Summed_Peaks[["Plot9_TIC_Per_treatments_Treatment specific exudates split in unique and shared exudates"]]<-plot9
All.figures$Exudates$Summed_Peaks[["Plot10_TIC_Per_treatments_Treatment specific exudates split in unique and shared exudates"]]<-plot10
All.figures$Exudates$Summed_Peaks[["legend"]]<-legend



#
# venn.img<-image_read_pdf(paste0(dirFigs,"/Fig2_A.pdf"))
# #venn.img<-image_scale(venn.img, "400")
# venn.img<- ggdraw()+
#   draw_image(venn.img, scale = 1)
# All.figures$Exudates$Summed_Peaks[["venn"]]<-venn.img
#
# venn.img2<-image_read_pdf(paste0(dirFigs,"/Fig2A-2.pdf"))
# #venn.img<-image_scale(venn.img, "400")
# venn.img2<- ggdraw()+
#   draw_image(venn.img2, scale = 1)
# venn.img2
#
# venn.img.complete <- plot_grid(venn.img, venn.img2, ncol = 2, nrow = 1, labels = c("A", ""))
# venn.img.complete
# All.figures$Exudates$Summed_Peaks[["venn_and_expl"]]<-venn.img.complete
#
# figure.venn.and.barplots.without.info<-plot_grid(venn.img.complete, plot11, legend, ncol = 1, nrow = 3, rel_heights = c(10,20,1))
# figure.venn.and.barplots.without.info
# save_plot(paste0("figure1.pdf"), plot=figure.venn.and.barplots.without.info,  path = dirFigs, base_height = 13, base_width = 9)
# save_plot(paste0("figure1.jpg"), plot=figure.venn.and.barplots.without.info, path = dirFigs, base_height = 13, base_width = 9)


# plot relative change of summed peak areas.
# # 1B
# view(tmp8)



### make extra plot of all ambient per treatment
#now split the exudates into exudate specific for that treatment and not
tmp<-select(exudates, -count)
tmp<-pivot_longer(tmp, -Feature_ID, names_to = 'Treatment_ratio', values_to = "x")
tmp.list<-levels(as.factor(tmp$Treatment_ratio))

rm(tmp7)
for (i in tmp.list){
  #select the features that are specific fot that treatment
  tmp2<-filter(tmp, Treatment_ratio == i)
  tmp2<-filter(tmp2, !is.na(x))
  feature.list<-tmp2$Feature_ID

  # use df.area
  # split in exudates and not exudates
  tmp3<-select(df.area, 1:(all_of(M)-1), all_of(feature.list)) #specific exudates
  tmp4<-select(df.area, 1:(all_of(M)-1), all_of(exudates_list)) %>% select(- all_of(feature.list)) #exudates but not in that treatment
  tmp5<-select(df.area, -all_of(exudates_list)) # no exudates at all

  # sum all peak areas.
  tmp3<-tmp3 %>%
    filter( Treatment_ratio == {{i}}) %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp3))))
  tmp4<-tmp4 %>%
    filter( Treatment_ratio == {{i}}) %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp4))))
  tmp5<-tmp5 %>%
    filter( Treatment_ratio == {{i}}) %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp5))))

  # select the sum, time point and replicate.
  tmp3<-select(tmp3, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( specific.exudate = sum)
  tmp4<-select(tmp4, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( other.exudate = sum)
  tmp5<-select(tmp5, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( not.exudate = sum)

  # join datasets
  tmp6<-full_join(tmp3, tmp4, by = 'File Name', suffix = c("", ".y"))
  tmp6<-select(tmp6, - contains(".y"))
  tmp6<-full_join(tmp6, tmp5, by = 'File Name', suffix = c("", ".y"))
  tmp6<-select(tmp6, - contains(".y"))

  tmp6<-tmp6 %>% mutate(ambient = other.exudate + not.exudate) %>%
    select(-other.exudate, - not.exudate)
  tmp6<-pivot_longer(tmp6, cols = c(specific.exudate, ambient), values_to = "TIC", names_to = "category")

  #summarize data
  tmp6<-tmp6 %>% group_by(Treatment_ratio, category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC))


  if(exists("tmp7")){
    tmp7 <- bind_rows(tmp7, tmp6)
  }else{
    tmp7<-tmp6
  }

}


tmp7.1 <- tmp7 %>% filter(category == "ambient")
F4<-factor(tmp7.1$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))
cust.pal2<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")
plot11<-ggplot(tmp7.1, aes(
  x = F4,
  y = mean,
  fill = F4,
  alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Ambient Features")) +
  scale_y_continuous(name = "Summed Peak Areas", limits = c(0, 3.5E9), n.breaks = 6)+
  scale_fill_manual(name = "Time Point", values = cust.pal2) +
  scale_alpha_manual(values = c(0.45 , 1))+
  guides(color = "none", fill = "none", alpha = "none")
plot11
ggsave(paste0("Fig2_plot11_Ambient_Features.png"), plot11, path = dirFigs, width = 4, height = 5)

tmp7.1 <- tmp7 %>% filter(category == "specific.exudate")
F4<-factor(tmp7.1$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))
cust.pal2<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")
plot12<-ggplot(tmp7.1, aes(
  x = F4,
  y = mean,
  fill = F4,
  alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("specific exudates Features")) +
  scale_y_continuous(name = "Summed Peak Areas", n.breaks = 6)+
  scale_fill_manual(name = "Time Point", values = cust.pal2) +
  scale_alpha_manual(values = c(0.45 , 1))+
  guides(color = "none", fill = "none", alpha = "none")
plot12
ggsave(paste0("Fig2_plot12_Specific exudate_Features.png"), plot12, path = dirFigs, width = 4, height = 5)

#clean environement
rm(plot1, plot2,plot3, plot4, plot5, plot6, plot7, plot8, plot8.1, plot9, plot10, venn.img2, venn.img, legend, figure.venn.and.barplots, figure.venn.and.barplots.without.info)
rm(temp, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmptmp, tmp.plot,
   t.test.tmp)
rm(plot11,plot12, plot6.1, plot7.1, tmp7.1, tmp9.1)

### make extra plot of all shared exudate features (2-4 shared)
#now split the exudates into exudate specific for that treatment and not
tmp<-select(exudates, -count)
tmp<-pivot_longer(tmp, -Feature_ID, names_to = 'Treatment_ratio', values_to = "x")
tmp.list<-levels(as.factor(tmp$Treatment_ratio))

rm(tmp4)
for (i in tmp.list){
  tmp1<-exudates %>%
    filter(!is.na(.data[[i]])) %>%
    filter(count != 1)
  feature.list<-tmp1$Feature_ID

  # use df.area
  #
  tmp3<-select(df.area, 1:(all_of(M)-1), all_of(feature.list)) #shared exudates

  # sum all peak areas.
  tmp3<-tmp3 %>%
    filter( Treatment_ratio == {{i}}) %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp3))))

  # select the sum, time point and replicate.
  tmp3<-select(tmp3, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>% rename( shared.exudate = sum)

  # join datasets

  tmp3<-pivot_longer(tmp3, cols = c(shared.exudate), values_to = "TIC", names_to = "category")

  #summarize data
  tmp3<-tmp3 %>% group_by(Treatment_ratio, category, Time_Point) %>% summarise(mean = mean(TIC), sd = sd(TIC))


  if(exists("tmp4")){
    tmp4 <- bind_rows(tmp4, tmp3)
  }else{
    tmp4<-tmp3
  }

}

F4<-factor(tmp4$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))
cust.pal2<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")
plot13<-ggplot(tmp4, aes(
  x = F4,
  y = mean,
  fill = F4,
  alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Shared Features")) +
  scale_y_continuous(name = "Summed Peak Areas", limits = c(0, 1.5E9))+
  scale_fill_manual(name = "Time Point", values = cust.pal2) +
  scale_alpha_manual(values = c(0.45 , 1))+
  guides(color = "none", fill = "none", alpha = "none")
plot13
ggsave(paste0("Fig2_plot13_Shared exudate_Features.png"), plot13, path = dirFigs, width = 4, height = 5)
