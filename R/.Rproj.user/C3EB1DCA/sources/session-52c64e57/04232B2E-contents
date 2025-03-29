# for each treatment define seperately what the ambient and exudate featues are and how they behave over time
# install.packages("ggpattern")
library(ggpattern)
tmp.list<-c("Coral", "Coral_Dictyota", "Coral_Turf", "Dictyota","Turf")

rm(tmp6)
for (i in tmp.list){
  #search in exudates

  tmp<-exudates %>%
    filter(!is.na(.data[[i]]))
  exudates.to.select <- tmp$Feature_ID

  # tmp1 is exudates
  tmp1<- df.area %>%
    filter (Treatment_ratio == i) %>%
    select (1:all_of(M)-1, any_of(exudates.to.select))

  tmp1<-tmp1 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp1)))) %>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Exudate = sum) %>%
    pivot_longer(cols = c(Exudate), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = length(exudates.to.select))

  # tmp2 is ambient features
  tmp2<- df.area %>%
    filter (Treatment_ratio == {{i}}) %>%
    select (-any_of(exudates.to.select))

  tmp2<-tmp2 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp2)))) %>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Ambient = sum) %>%
    pivot_longer(cols = c(Ambient), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = (ncol(tmp2)-M+1) )

  # tmp3 is shared features
  tmp3 <- tmp %>%
    filter(count != 1)
  exudates.to.select <- tmp3$Feature_ID

  tmp3<- df.area %>%
    filter (Treatment_ratio == {{i}}) %>%
    select (1:all_of(M)-1, any_of(exudates.to.select))

  tmp3<-tmp3 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp3)))) %>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Shared = sum) %>%
    pivot_longer(cols = c(Shared), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = length(exudates.to.select))

  # tmp4 is ubiquituous features
  tmp4 <- tmp %>%
    filter(count == 5)
  exudates.to.select <- tmp4$Feature_ID

  tmp4<- df.area %>%
    filter (Treatment_ratio == {{i}}) %>%
    select (1:all_of(M)-1, any_of(exudates.to.select))

  tmp4<-tmp4 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp4)))) %>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Ubiquitus = sum) %>%
    pivot_longer(cols = c(Ubiquitus), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = length(exudates.to.select))


  # tmp5 is unique features
  tmp5 <- tmp %>%
    filter(count == 1)
  exudates.to.select <- tmp5$Feature_ID

  tmp5<- df.area %>%
    filter (Treatment_ratio == {{i}}) %>%
    select (1:all_of(M)-1, any_of(exudates.to.select))

  tmp5<-tmp5 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp5))))%>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Unique = sum) %>%
    pivot_longer(cols = c(Unique), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = length(exudates.to.select))






  if(exists("tmp6")){
    tmp6 <- bind_rows(tmp6, tmp1, tmp2, tmp3, tmp4, tmp5)
  }else{
    tmp6<-bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5)
  }

}


for (i in levels(factor(tmp6$category))){
  tmp <- tmp6 %>%
    filter(category == {{i}}) %>%
    ungroup() %>%
    mutate(Label = case_when(Time_Point == 0 ~ "n = ",
           .default = as.character(n.features)))

  F1<-factor(tmp$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))

  # costumate color pallet for "Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"
  cust.pal<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")
  label.y <- max(tmp$mean)*1.1

  fig.bars<-ggplot(tmp, aes(
    x = F1,
    y = mean,
    fill = F1,
    alpha = Time_Point)) +
    geom_col(position = position_dodge(), width = 0.9) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                  width = 0.2,
                  position = position_dodge(.9)) +
    theme_classic() +
    scale_x_discrete(paste0( i, " features")) +
    scale_y_continuous(name = NULL, n.breaks = 8) +
    scale_fill_manual(name = "Time Point", values = cust.pal) +
    scale_alpha_manual(values = c(0.45 , 1))+
    geom_text(aes(label = Label, y = label.y), position = position_dodge(0.7))+
    guides(color = "none", fill = "none", alpha = "none")

  ggsave(paste0(Sys.Date(), "_barplots_", i,"_features.jpg"), fig.bars, path = dirFigs, width = 5, height = 5)

  }

#change label hight for ubiquitous
i<- c("Ubiquitus")
tmp <- tmp6 %>%
  filter(category == {{i}}) %>%
  ungroup() %>%
  mutate(Label = case_when(Time_Point == 0 ~ "n = ",
                           .default = as.character(n.features)))

ggplot(tmp, aes(
  x = F1,
  y = mean,
  fill = F1,
  alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0( i, " features")) +
  scale_y_continuous(name = NULL, n.breaks = 8, limits = c(0, 3.1e8)) +
  scale_fill_manual(name = "Time Point", values = cust.pal) +
  scale_alpha_manual(values = c(0.45 , 1))+
  geom_text(aes(label = Label, y = 3.05E8), position = position_dodge(0.7))+
  guides(color = "none", fill = "none", alpha = "none")
ggsave(paste0(Sys.Date(), "_barplots_", i,"_features.jpg"), path = dirFigs, width = 5, height = 5)



#calculate ratios
tmp7 <- tmp6 %>%
  select ( -sd, -n.features) %>%
  pivot_wider(names_from = c(category, Time_Point), values_from = mean) %>%
  mutate(ratio_Ex_AM_T0 = Ambient_0/Exudate_0,
         ratio_Ex_AM_T28 = Ambient_28/Exudate_28,
         ratio_sh_un_T0 = Shared_0/Unique_0,
         ratio_sh_un_T28 = Shared_28/Unique_28)

tmp37 <- tmp6 %>%
  select ( -sd, -mean) %>%
  pivot_wider(names_from = c(category, Time_Point), values_from = n.features) %>%
  mutate(ratio_Ex_AM_nF = Ambient_0/Exudate_0,
         ratio_sh_un_nF = Shared_0/Unique_0) %>%
  select(Treatment_ratio, ratio_Ex_AM_nF, ratio_sh_un_nF)

tmp7 <- full_join(tmp7, tmp37)

tmp37<-tmp7 %>%
  arrange(factor(Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota","Coral", "Coral_Turf", "Turf" )))



library(knitr)
library(kableExtra)
knitr::kable(tmp7 %>%
               select(Treatment_ratio, ratio_Ex_AM_T0, ratio_Ex_AM_T28, ratio_Ex_AM_nF) %>%
               arrange(factor(Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota","Coral", "Coral_Turf", "Turf" ))),
             col.names = c('Treatment', 'T=0', 'T=28', "n"),
             digits = 2,
             caption = "Ratio Ambient/Exudate feature peakarea",
) %>%
  kable_classic()


knitr::kable(tmp7 %>%
               select(Treatment_ratio, ratio_sh_un_T0, ratio_sh_un_T28, ratio_sh_un_nF) %>%
               arrange(factor(Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota","Coral", "Coral_Turf", "Turf" ))),
             col.names = c('Treatment', 'T=0', 'T=28', "n"),
             digits = 2,
             caption = "Ratio Shared/Unique feature peakarea",
) %>%
  kable_classic()



# now i want to know how many of these features are decreasing over time
# i should be able to compare it with data in tmp6.
# therefore i use the metabolized groups, if the mean difference is negative, and in the same group then i have to select those if present in one of the other selections
#thus i can copy most of the first code, but change the tmp name

rm(tmp16, tmp35, tmp8)
for (i in tmp.list){
  #search in exudates
  tmp9 <- Metabolized_groups %>%
    filter(difference_mean < 0) %>%
    filter(group == {{i}})
  decreasingfeat.to.select <- tmp9$Feature_ID

  tmp10<-exudates %>%
    filter(!is.na(.data[[i]]))
  exudates.to.select <- tmp10$Feature_ID

  #tmp 8 is all features
  tmp8 <- df.area %>%
    filter(Treatment_ratio == i) %>%
    select(1:all_of(M)-1, any_of(decreasingfeat.to.select))

  tmp8<-tmp8 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp8)))) %>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Total = sum) %>%
    pivot_longer(cols = c(Total), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = (ncol(tmp8)-M+1))

  # tmp11 is exudates
  tmp11<- df.area %>%
    filter (Treatment_ratio == i) %>%
    select (1:all_of(M)-1, any_of(exudates.to.select)) %>%
    select (1:all_of(M)-1, any_of(decreasingfeat.to.select))

  tmp11<-tmp11 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp11)))) %>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Exudate = sum) %>%
    pivot_longer(cols = c(Exudate), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = (ncol(tmp11)-M+1))

  # tmp12 is ambient features
  tmp12<- df.area %>%
    filter (Treatment_ratio == {{i}}) %>%
    select (-any_of(exudates.to.select)) %>%
    select (1:all_of(M)-1, any_of(decreasingfeat.to.select))

  tmp12<-tmp12 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp12)))) %>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Ambient = sum) %>%
    pivot_longer(cols = c(Ambient), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = (ncol(tmp12)-M+1) )

  # tmp13 is shared features
  tmp13 <- tmp10 %>%
    filter(count != 1)
  exudates.to.select <- tmp13$Feature_ID

  tmp13<- df.area %>%
    filter (Treatment_ratio == {{i}}) %>%
    select (1:all_of(M)-1, any_of(exudates.to.select)) %>%
    select (1:all_of(M)-1, any_of(decreasingfeat.to.select))

  tmp13<-tmp13 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp13)))) %>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Shared = sum) %>%
    pivot_longer(cols = c(Shared), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = (ncol(tmp13)-M+1))

  # tmp14 is ubiquituous features
  tmp14 <- tmp10 %>%
    filter(count == 5)
  exudates.to.select <- tmp14$Feature_ID

  tmp14<- df.area %>%
    filter (Treatment_ratio == {{i}}) %>%
    select (1:all_of(M)-1, any_of(exudates.to.select)) %>%
    select (1:all_of(M)-1, any_of(decreasingfeat.to.select))

  tmp34 <- as.data.frame(colnames(tmp14[M:ncol(tmp14)]))
  tmp34$treatment <- i
  if (exists("tmp35")){
    tmp35 <- bind_rows(tmp35, tmp34)}else
      {tmp35<-tmp34}

  tmp14<-tmp14 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp14)))) %>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Ubiquitus = sum) %>%
    pivot_longer(cols = c(Ubiquitus), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = (ncol(tmp14)-M+1))


  # tmp15 is shared features
  tmp15 <- tmp10 %>%
    filter(count == 1)
  exudates.to.select <- tmp15$Feature_ID

  tmp15<- df.area %>%
    filter (Treatment_ratio == {{i}}) %>%
    select (1:all_of(M)-1, any_of(exudates.to.select)) %>%
    select (1:all_of(M)-1, any_of(decreasingfeat.to.select))

  tmp15<-tmp15 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp15))))%>%
    select('File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( Unique = sum) %>%
    pivot_longer(cols = c(Unique), values_to = "TIC", names_to = "category") %>%
    group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC), n.features = (ncol(tmp15)-M+1))






  if(exists("tmp16")){
    tmp16 <- bind_rows(tmp16, tmp11, tmp12, tmp13, tmp14, tmp15, tmp8)
  }else{
    tmp16<-bind_rows(tmp11, tmp12, tmp13, tmp14, tmp15, tmp8)
  }

}

# calculate total decreasing features per treatment
tmp16 %>%
  filter(category == "Total") %>%
  select(-sd) %>%
  pivot_wider(names_from = Time_Point, values_from = mean) %>%
  mutate(difference = `28`-`0`)

#calculate ratios
tmp36 <- tmp16 %>%
  select ( -sd, -n.features ) %>%
  pivot_wider(names_from = c(category, Time_Point), values_from = mean) %>%
  mutate(ratio_Ex_AM_T0 = Ambient_0/Exudate_0,
         ratio_Ex_AM_T28 = Ambient_28/Exudate_28,
         ratio_sh_un_T0 = Shared_0/Unique_0,
         ratio_sh_un_T28 = Shared_28/Unique_28)

tmp38 <- tmp16 %>%
  select ( -sd, -mean) %>%
  pivot_wider(names_from = c(category, Time_Point), values_from = n.features) %>%
  mutate(ratio_Ex_AM_nF = Ambient_0/Exudate_0,
         ratio_sh_un_nF = Shared_0/Unique_0) %>%
  select(Treatment_ratio, ratio_Ex_AM_nF, ratio_sh_un_nF)

tmp36<-full_join(tmp36, tmp38)

tmp36 <-tmp36 %>%
  arrange(factor(Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota","Coral", "Coral_Turf", "Turf" )))

tmp37 <- full_join(tmp37, tmp36, by = join_by(Treatment_ratio), suffix = c("_all", "_decreasing"))

knitr::kable(tmp37 %>%
               select(Treatment_ratio, ratio_Ex_AM_T0_all, ratio_Ex_AM_T28_all, ratio_Ex_AM_nF_all, ratio_Ex_AM_T0_decreasing, ratio_Ex_AM_T28_decreasing, ratio_Ex_AM_nF_decreasing) %>%
               arrange(factor(Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota","Coral", "Coral_Turf", "Turf" ))),
             col.names = c('Treatment', 'T=0', 'T=28', "n", 'T=0', 'T=28', "n"),
             digits = 2,
             caption = "Ratio Ambient/Exudate feature peakarea") %>%
  add_header_above(header = c(" " = 1, "All" = 3, "Decreasing" = 3)) %>%
  kable_classic()


knitr::kable(tmp37 %>%
               select(Treatment_ratio, ratio_sh_un_T0_all, ratio_sh_un_T28_all, ratio_sh_un_nF_all, ratio_sh_un_T0_decreasing, ratio_sh_un_T28_decreasing, ratio_sh_un_nF_decreasing) %>%
               arrange(factor(Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota","Coral", "Coral_Turf", "Turf" ))),
             col.names = c('Treatment', 'T=0', 'T=28', "n", 'T=0', 'T=28', "n"),
             digits = 2,
             caption = "Ratio Shared/Unique feature peakarea") %>%
  add_header_above(header = c(" " = 1, "All" = 3, "Decreasing" = 3)) %>%
  kable_classic()



tmp17<-full_join(tmp6,tmp16, by = join_by(Treatment_ratio, category, Time_Point), suffix = c("_all", "_decreasing"))

tmp18<- tmp17 %>%
  mutate(perc_dif_mean = ((mean_decreasing / mean_all)*100),
         perc_dif_features = ((n.features_decreasing / n.features_all)*100), .keep = "unused") %>%
  select(-sd_all, -sd_decreasing) %>%
  pivot_wider(names_from = c(category), values_from = c(perc_dif_mean, perc_dif_features))


for (i in levels(factor(tmp17$category))){
  tmp <- tmp17 %>%
    filter(category == {{i}}) %>%
    ungroup() #%>%
    # mutate(Label = case_when(Time_Point == 0 ~ "n = ",
    #                          .default = as.character(n.features)))
    #
  F1<-factor(tmp$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))

  # costumate color pallet for "Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"
  cust.pal<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")
  # label.y <- max(tmp$mean)*1.1

  fig.bars<-ggplot()+
    geom_col_pattern(data = tmp, aes(F1, y = mean_all, pattern_fill = F1, pattern_fill2 = F1, alpha = Time_Point, fill = F1, pattern_alpha = Time_Point), pattern = 'weave', pattern_type = 'plain', pattern_density = c(0.1), pattern_key_scale_factor = 0.1, color = "black",  position = position_dodge(), width = 0.9) +
    geom_col(data = tmp, aes(F1, y = mean_decreasing, group = Time_Point), fill = "white", position = position_dodge(), width = 0.9)+
    geom_col(data = tmp, aes(F1, y = mean_decreasing, alpha= Time_Point, fill = F1 ), color = "black", position = position_dodge(), width = 0.9, linewidth = 1)+
    # geom_col_pattern(data = tmp, aes(F1, y = mean_decreasing, alpha = Time_Point, fill = F1), pattern = 'wave', pattern_type = 'sine', pattern_density = c(0.1), pattern_spacing = c(0.1), color = "black", position = position_dodge(), width = 0.9)+
    geom_errorbar(data = tmp, aes(x= F1, ymin = mean_all - sd_all, ymax = mean_all + sd_all), position = position_dodge2(width = 0.2, padding = 0.8), linewidth = 1) +
    geom_errorbar(data = tmp, aes(x= F1, ymin = mean_decreasing - sd_decreasing, ymax = mean_decreasing + sd_decreasing), position = position_dodge2(width = 0.2, padding = 0.8), linewidth = 1) +
    theme_classic() +
    scale_x_discrete(paste0(i, " Features")) +
    scale_y_continuous(name = "Summed Peak Areas", n.breaks = 6) +
    scale_fill_manual(name = "Time Point", values = cust.pal) +
    scale_pattern_fill_manual(values = cust.pal)+
    scale_pattern_fill2_manual(values = cust.pal)+
    scale_pattern_alpha_manual(values = c(0.30 , 0.8) ) +
    scale_alpha_manual(values = c(0.45 , 1))+
    guides(color = "none", fill = "none", alpha = "none", pattern_fill = "none", pattern_fill2 = "none", pattern_alpha = 'none')



  ggsave(paste0(Sys.Date(), "_barplots_", i,"_features_and_decreasing_part.jpg"), fig.bars, path = dirFigs, width = 5, height = 5)

}


#now we want to know what these decreasing exudates are
Treatments <- c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf")
library(ggridges)
rm(mzRT.exudate)
for (i in Treatments){
  tmp21<-exudates %>%
    filter(!is.na(.data[[i]]))
  exudates.to.select <- tmp21$Feature_ID

  tmp20 <- Metabolized_groups %>%
    filter(difference_mean < 0) %>%
    filter(group == {{i}})
  decreasingfeat.to.select <- tmp9$Feature_ID

  tmp22<- df.featureID %>%
    filter (featureID %in% exudates.to.select) %>%
    filter (featureID %in% decreasingfeat.to.select) %>%
    select (featureID, `row m/z`, `row retention time`) %>%
    mutate (Treatment = {{i}})


  if(exists("mzRT.exudate")){
    mzRT.exudate<-bind_rows(mzRT.exudate, tmp22)
  }else{
    mzRT.exudate<-tmp22
  }


}

mzRT.exudate$Treatment <- factor(mzRT.exudate$Treatment, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))
ggplot(mzRT.exudate, aes(`row m/z`, Treatment, fill = Treatment))+
  geom_density_ridges()+
  scale_fill_manual(values = cost.pal)+
  theme_bw()


ggplot(mzRT.exudate, aes(`row retention time`, Treatment, fill = Treatment))+
  geom_density_ridges()+
  scale_fill_manual(values = cost.pal)+
  theme_bw()


rm(mzRT)
for (i in Treatments){
  tmp21<-exudates %>%
    filter(!is.na(.data[[i]])) %>%
    filter(count == 1)
  exudates.to.select <- tmp21$Feature_ID

  tmp20 <- Metabolized_groups %>%
    filter(difference_mean < 0) %>%
    filter(group == {{i}})
  decreasingfeat.to.select <- tmp9$Feature_ID

  tmp22<- df.featureID %>%
    filter (featureID %in% exudates.to.select) %>%
    filter (featureID %in% decreasingfeat.to.select) %>%
    select (featureID, `row m/z`, `row retention time`) %>%
    mutate (Treatment = {{i}})


  if(exists("mzRT")){
    mzRT<-bind_rows(mzRT, tmp22)
  }else{
    mzRT<-tmp22
  }


}

mzRT$Treatment <- factor(mzRT$Treatment, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))
ggplot(mzRT, aes(`row m/z`, Treatment, fill = Treatment))+
  geom_density_ridges()+
  scale_fill_manual(values = cost.pal)+
  theme_bw()


ggplot(mzRT, aes(`row retention time`, Treatment, fill = Treatment))+
  geom_density_ridges()+
  scale_fill_manual(values = cost.pal)+
  theme_bw()


# unique features in other experiments
tmp<-select(exudates, -count)
tmp<-pivot_longer(tmp, -Feature_ID, names_to = 'Treatment_ratio', values_to = "x")
tmp.list<-levels(as.factor(tmp$Treatment_ratio))

rm(tmp4)
for (i in tmp.list){
  tmp1<-exudates %>%
    filter(!is.na(.data[[i]])) %>%
    filter(count == 1)
  feature.list<-tmp1$Feature_ID

  # use df.area
  #
  tmp3<-select(df.area, 1:(all_of(M)-1), all_of(feature.list)) #shared exudates

  # sum all peak areas.
  tmp3<-tmp3 %>%
    rowwise() %>%
    mutate(sum = sum( c_across(all_of(M):ncol(tmp3))))

  # select the sum, time point and replicate.
  tmp3<-select(tmp3, 'File Name', Time_Point, Treatment_ratio, Replicate, sum) %>%
    rename( shared.exudate = sum)
  # join datasets

  tmp3<-pivot_longer(tmp3, cols = c(shared.exudate), values_to = "TIC", names_to = "category")

  #summarize data
  tmp3<-tmp3 %>% group_by(Treatment_ratio, category, Time_Point) %>%
    summarise(mean = mean(TIC), sd = sd(TIC))  %>%
    mutate ( group = {{i}})


  if(exists("tmp4")){
    tmp4 <- bind_rows(tmp4, tmp3)
  }else{
    tmp4<-tmp3
  }

}

F4<-factor(tmp4$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))

tmp4$group<-factor(tmp4$group, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf"))
cust.pal2<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3",  "#98F5FF")
Figure.S3<-ggplot(tmp4, aes(
  x = F4,
  y = mean,
  fill = F4,
  alpha = Time_Point)) +
  geom_col(position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_x_discrete(paste0("Unique Features")) +
  scale_y_continuous(name = "Summed Peak Areas")+
  scale_fill_manual(name = "Time Point", values = cust.pal2) +
  scale_alpha_manual(values = c(0.45 , 1))+
  guides(color = "none", fill = "none", alpha = "none") +
  facet_wrap(~ group, ncol = 1, scales = "free_y")

ggsave(paste0("FigS2_Unique exudate Features in other treatments.png"), Figure.S3, path = dirFigs, width = 4, height = 5)


# now make those circle plots


#we first need to make an edge list with a from and to column on all levels that can connect to eachother.
#lets get all the decreasing exudates that we already selected with mzRT

rm(decreasing_exudates)
for (i in Treatments){
  tmp21<-exudates %>%
    filter(!is.na(.data[[i]]))
  exudates.to.select <- tmp21$Feature_ID

  print(paste0("number of exudates in ", i, " = ", length(exudates.to.select)))

  tmp20 <- Metabolized_groups %>%
    filter(difference_mean < 0) %>%
    filter(group == {{i}})
  decreasingfeat.to.select <- tmp20$Feature_ID

  print(paste0("number of decreasing features in ", i, " = ", length(decreasingfeat.to.select)))

  tmp22<- df.featureID %>%
    filter (featureID %in% exudates.to.select) %>%
    filter (featureID %in% decreasingfeat.to.select) %>%
    select (featureID, `row m/z`, `row retention time`, CF_superclass, CF_class) %>%
    mutate (Treatment = {{i}})
  select.these <- tmp22$featureID

  print(paste0("number of decreasing exudates in ", i, " = ", length(select.these)))

  tmp23 <- df.area %>%
    select(Treatment_ratio, Time_Point, all_of(select.these) ) %>%
    filter(Treatment_ratio == {{i}}) %>%
    pivot_longer(cols = all_of(select.these), values_to = "XIC", names_to = "featureID") %>%
    group_by(featureID, Time_Point) %>%
    summarise(mean = mean(XIC)) %>%
    pivot_wider(names_from = Time_Point, values_from = mean) %>%
    mutate(difference = `28` - `0`, absolute_diff = abs(difference))

  tmp24 <-full_join(tmp22, tmp23, by = "featureID")

  if(exists("decreasing_exudates")){
    decreasing_exudates<-bind_rows(decreasing_exudates, tmp24)
  }else{
    decreasing_exudates<-tmp24
  }


}

# we need to make two files
# we need to make the edges file
#filter out the no matches
decreasing_exudates <- decreasing_exudates %>%
  filter(CF_superclass != "no matches")

tmp1 <- decreasing_exudates %>%
  select (Treatment, CF_superclass) %>%
  distinct() %>%
  unite(name_SC, c("Treatment", "CF_superclass"), remove = F, sep = ".") %>%
  rename ("From" = "Treatment",
          "To" = "name_SC") %>%
  select(From, To)



tmp2 <- decreasing_exudates %>%
  select (Treatment, CF_superclass, CF_class) %>%
  distinct() %>%
  unite(name_SC, c("Treatment", "CF_superclass"), remove = F, sep = ".") %>%
  unite(name_C, c("Treatment", "CF_superclass", "CF_class"), remove = F, sep = ".") %>%
  rename ("From" = "name_SC",
          "To" = "name_C") %>%
  select(From, To)

edges <- bind_rows(tmp1, tmp2)

decreasing_exudates <- decreasing_exudates %>%
  unite(name_SC, c("Treatment", "CF_superclass"), remove = F, sep = ".") %>%
  unite(name_C, c("Treatment", "CF_superclass", "CF_class"), remove = F, sep = ".")


tmp3 <- decreasing_exudates %>%
  group_by( name_C, CF_class) %>%
  summarise(size = sum(absolute_diff)) %>%
  rename("name" = "name_C",
         "short.name" = "CF_class")  %>%
  mutate(color = case_when(grepl("Coral_Dictyota", name, fixed = TRUE) ~ "#FF7F00",
                           grepl("Dictyota", name, fixed = TRUE) ~ "#4DAF4A",
                           grepl("Coral_Turf", name, fixed = TRUE) ~ "#F781BF",
                           grepl("Turf", name, fixed = TRUE) ~ "#984EA3",
                           grepl("Coral.", name) ~ "#E41A1C")) %>%
  mutate(line.col = "black", line.with = 0.4)


tmp4 <- decreasing_exudates %>%
  group_by( name_SC, CF_superclass) %>%
  summarise(size = sum(absolute_diff))%>%
  rename("name" = "name_SC",
         "short.name" = "CF_superclass") %>%
  mutate(color = case_when(grepl("Coral_Dictyota", name, fixed = TRUE) ~ "#FF7F00",
                           grepl("Dictyota", name, fixed = TRUE) ~ "#4DAF4A",
                           grepl("Coral_Turf", name, fixed = TRUE) ~ "#F781BF",
                           grepl("Turf", name, fixed = TRUE) ~ "#984EA3",
                           grepl("Coral.", name) ~ "#E41A1C")) %>%
  mutate(line.col = "black", line.with = 0.4)



tmp5 <- decreasing_exudates %>%
  group_by( Treatment) %>%
  summarise(size = sum(absolute_diff))%>%
  rename("name" = "Treatment") %>%
  mutate(color = case_when(
    grepl("Coral_Dictyota", name, fixed = TRUE) ~ "#FF7F00",
    grepl("Dictyota", name, fixed = TRUE) ~ "#4DAF4A",
    grepl("Coral_Turf", name, fixed = TRUE) ~ "#F781BF",
    grepl("Turf", name, fixed = TRUE) ~ "#984EA3",
    .default = "#E41A1C" )) %>%
  mutate(line.col = case_when(
    grepl("Coral_Dictyota", name, fixed = TRUE) ~ "#FF7F00",
    grepl("Dictyota", name, fixed = TRUE) ~ "#4DAF4A",
    grepl("Coral_Turf", name, fixed = TRUE) ~ "#F781BF",
    grepl("Turf", name, fixed = TRUE) ~ "#984EA3",
    .default = "#E41A1C" )) %>%
  mutate(line.with = 1.5)

tmp5$short.name <- tmp5$name

circle.data <- bind_rows(tmp3, tmp4, tmp5)
#decreasing.exudates.circle.data<-circle.data


tmp6<-as.data.frame(decreasing_exudates$CF_superclass, decreasing_exudates$CF_class)
colnames(tmp6)<-"Superclass"
tmp6 <- tmp6 %>%
  rownames_to_column("Class") %>%
  distinct() %>%
  arrange(Superclass, Class)

tmp7<-as.data.frame(levels(factor(tmp6$Superclass)))
colnames(tmp7) <- "short.name"
tmp7$col.code <- c(
  "#E74C3C",
  "darkgreen",
  "skyblue1",
  "green",
  "palevioletred2",
  "navy",
  "#FFC300",
  "purple",
  "hotpink1")


tmp6$col.code <- c(
  "#FF8C33", "darkorange", "tomato1", "tomato4", "#B03A2E", "#FF5733", "#FF4A1C", "#FF3E06", "red", "red3",
  "forestgreen",
  "#5DADE2", "#3498DB", "#2980B9",
  "palegreen", "#2ECC71", "#27AE60",
  "palevioletred",
  "navyblue",
  "gold","gold4", "gold2","gold3", "#FFDC66","goldenrod3","goldenrod","goldenrod4","gold1",  "darkgoldenrod", "goldenrod1", "goldenrod2",
  "purple4",
  "hotpink", "hotpink2"
)

# tmp6$col.code <- c(
#   "#FF8C33",  "tomato1", "tomato4", "#B03A2E", "#FF4A1C", "#FF3E06", "red",
#   "forestgreen",
#   "#5DADE2", "#3498DB", "#2980B9",
#   "palegreen", "#27AE60",
#   "palevioletred",
#   "navyblue",
#   "gold","gold3", "#FFDC66","goldenrod3","goldenrod","goldenrod4","gold1",  "darkgoldenrod", "goldenrod1",
#   "purple4",
#   "hotpink2"
# )

tmp6<-tmp6 %>%
  select(-Superclass) %>%
  rename("short.name" = "Class")

tmp8<- data.frame(
  short.name = c(
    "Coral_Dictyota", "Dictyota","Coral_Turf","Turf","Coral"
  ),
  col.code = c(
    "grey90", "grey91","grey92","grey93","grey94"))

# tmp8<- data.frame(
#   short.name = c(
#     "Coral_Dictyota", "Dictyota","Coral_Turf","Turf","Coral"
#   ),
#   col.code = c(
#     "#FF7F00", "#4DAF4A","#F781BF","#984EA3", "#E41A1C"))

tmp9 <- bind_rows(tmp8, tmp7, tmp6)

circle.data <- left_join(circle.data, tmp9, by = "short.name")

labels <- tmp9$short.name
values <- tmp9$col.code
colorcodes <- factor(circle.data$col.code, levels = tmp9$col.code)
linecodes <- factor(circle.data$line.col)
linesize <- factor(circle.data$line.with)
#
# tmp10 <- tmp9 %>%
#   arrange(col.code)

library(igraph)
library(ggraph)

# Then we have to make a 'graph' object using the igraph library:
mygraph <- graph_from_data_frame( edges, vertices=circle.data )


# Make the plot
set.seed(25)
ggraph(mygraph, layout = 'circlepack', weight=size) +
  geom_node_circle() +
  theme_void()

legend.circleplot <- ggraph(mygraph, layout = 'circlepack', weight=size) +
  geom_node_circle(aes(fill = colorcodes, group = depth)) +
  theme_void()+
  scale_fill_manual(name = "Coloring", values = values, labels = labels)+
  theme(legend.position = "right")

legend.circleplot<-get_legend(legend.circleplot)

plot(legend.circleplot)
ggsave("legend_circleplot_decreasing_exudates.jpg", legend.circleplot, path = dirFigs, width = 20, height  = 20, units = c("cm"))

circleplot <- ggraph(mygraph, layout = 'circlepack', weight=size) +
  geom_node_circle(aes(fill = col.code, colour = line.col, group = depth, linewidth = factor(line.with))) +
  # geom_node_text( aes(label= short.name, filter = leaf),check_overlap = T, repel = T, size = 2)+
  theme_void()+
  scale_color_manual(values = levels(factor(circle.data$line.col)))+
  scale_fill_manual(values = levels(factor(circle.data$col.code)))+
  scale_linewidth_manual(values = c(0.4, 1.5))+
  theme(legend.position = "none")
circleplot
ggsave("circleplot_decreasing_exudates.jpg", circleplot, path = dirFigs, width = 15, height  = 15, units = c("cm"))
ggsave("circleplot_decreasing_exudates.eps", circleplot, path = dirFigs, width = 15, height  = 18, units = c("cm"))


decreasing.exudates.circle.data<-circle.data
edges.exudates<- edges


#now we make circle plots for unique exudates

#we first need to make an edge list with a from and to column on all levels that can connect to eachother.
#lets get all the uniquely decreasing exudates that we already selected with mzRT

rm(decreasing_exudates)
for (i in Treatments){
  tmp21<-exudates %>%
    filter(!is.na(.data[[i]])) %>%
    filter(count == 1)
  exudates.to.select <- tmp21$Feature_ID

  print(paste0("number of exudates in ", i, " = ", length(exudates.to.select)))

  tmp20 <- Metabolized_groups %>%
    filter(difference_mean < 0) %>%
    filter(group == {{i}})
  decreasingfeat.to.select <- tmp20$Feature_ID

  print(paste0("number of decreasing features in ", i, " = ", length(decreasingfeat.to.select)))

  tmp22<- df.featureID %>%
    filter (featureID %in% exudates.to.select) %>%
    filter (featureID %in% decreasingfeat.to.select) %>%
    select (featureID, `row m/z`, `row retention time`, CF_superclass, CF_class) %>%
    mutate (Treatment = {{i}})
  select.these <- tmp22$featureID

  print(paste0("number of decreasing exudates in ", i, " = ", length(select.these)))

  tmp23 <- df.area %>%
    select(Treatment_ratio, Time_Point, select.these ) %>%
    filter(Treatment_ratio == {{i}}) %>%
    pivot_longer(cols = all_of(select.these), values_to = "XIC", names_to = "featureID") %>%
    group_by(featureID, Time_Point) %>%
    summarise(mean = mean(XIC)) %>%
    pivot_wider(names_from = Time_Point, values_from = mean) %>%
    mutate(difference = `28` - `0`, absolute_diff = abs(difference))

  tmp24 <-full_join(tmp22, tmp23, by = "featureID")

  if(exists("decreasing_exudates")){
    decreasing_exudates<-bind_rows(decreasing_exudates, tmp24)
  }else{
    decreasing_exudates<-tmp24
  }


}

# we need to make two files
# we need to make the edges file
#filter out the no matches
decreasing_exudates <- decreasing_exudates %>%
  filter(CF_superclass != "no matches")

tmp1 <- decreasing_exudates %>%
  select (Treatment, CF_superclass) %>%
  distinct() %>%
  unite(name_SC, c("Treatment", "CF_superclass"), remove = F, sep = ".") %>%
  rename ("From" = "Treatment",
          "To" = "name_SC") %>%
  select(From, To)



tmp2 <- decreasing_exudates %>%
  select (Treatment, CF_superclass, CF_class) %>%
  distinct() %>%
  unite(name_SC, c("Treatment", "CF_superclass"), remove = F, sep = ".") %>%
  unite(name_C, c("Treatment", "CF_superclass", "CF_class"), remove = F, sep = ".") %>%
  rename ("From" = "name_SC",
          "To" = "name_C") %>%
  select(From, To)

edges <- bind_rows(tmp1, tmp2)

decreasing_exudates <- decreasing_exudates %>%
  unite(name_SC, c("Treatment", "CF_superclass"), remove = F, sep = ".") %>%
  unite(name_C, c("Treatment", "CF_superclass", "CF_class"), remove = F, sep = ".")


tmp3 <- decreasing_exudates %>%
  group_by( name_C, CF_class) %>%
  summarise(size = sum(absolute_diff)) %>%
  rename("name" = "name_C",
         "short.name" = "CF_class")  %>%
  mutate(color = case_when(grepl("Coral_Dictyota", name, fixed = TRUE) ~ "#FF7F00",
                           grepl("Dictyota", name, fixed = TRUE) ~ "#4DAF4A",
                           grepl("Coral_Turf", name, fixed = TRUE) ~ "#F781BF",
                           grepl("Turf", name, fixed = TRUE) ~ "#984EA3",
                           grepl("Coral.", name) ~ "#E41A1C")) %>%
  mutate(line.col = "black", line.with = 0.4)


tmp4 <- decreasing_exudates %>%
  group_by( name_SC, CF_superclass) %>%
  summarise(size = sum(absolute_diff))%>%
  rename("name" = "name_SC",
         "short.name" = "CF_superclass") %>%
  mutate(color = case_when(grepl("Coral_Dictyota", name, fixed = TRUE) ~ "#FF7F00",
                           grepl("Dictyota", name, fixed = TRUE) ~ "#4DAF4A",
                           grepl("Coral_Turf", name, fixed = TRUE) ~ "#F781BF",
                           grepl("Turf", name, fixed = TRUE) ~ "#984EA3",
                           grepl("Coral.", name) ~ "#E41A1C")) %>%
  mutate(line.col = "black", line.with = 0.4)



tmp5 <- decreasing_exudates %>%
  group_by( Treatment) %>%
  summarise(size = sum(absolute_diff))%>%
  rename("name" = "Treatment") %>%
  mutate(color = case_when(
                           grepl("Coral_Dictyota", name, fixed = TRUE) ~ "#FF7F00",
                           grepl("Dictyota", name, fixed = TRUE) ~ "#4DAF4A",
                           grepl("Coral_Turf", name, fixed = TRUE) ~ "#F781BF",
                           grepl("Turf", name, fixed = TRUE) ~ "#984EA3",
                           .default = "#E41A1C" )) %>%
  mutate(line.col = case_when(
    grepl("Coral_Dictyota", name, fixed = TRUE) ~ "#FF7F00",
    grepl("Dictyota", name, fixed = TRUE) ~ "#4DAF4A",
    grepl("Coral_Turf", name, fixed = TRUE) ~ "#F781BF",
    grepl("Turf", name, fixed = TRUE) ~ "#984EA3",
    .default = "#E41A1C" )) %>%
  mutate(line.with = 1.5)

tmp5$short.name <- tmp5$name

circle.data <- bind_rows(tmp3, tmp4, tmp5)



tmp6<-as.data.frame(decreasing_exudates$CF_superclass, decreasing_exudates$CF_class)
colnames(tmp6)<-"Superclass"
tmp6 <- tmp6 %>%
  rownames_to_column("Class") %>%
  distinct() %>%
  arrange(Superclass, Class)

tmp7<-as.data.frame(levels(factor(tmp6$Superclass)))
colnames(tmp7) <- "short.name"
tmp7$col.code <- c(
     "#E74C3C",
     "darkgreen",
     "skyblue1",
     "green",
     "palevioletred2",
     "navy",
     "#FFC300",
     "purple",
     "hotpink1")


# tmp6$col.code <- c(
#   "#FF8C33", "darkorange", "tomato1", "tomato4", "#B03A2E", "#FF5733", "#FF4A1C", "#FF3E06", "red", "red3",
#   "forestgreen",
#   "#5DADE2", "#3498DB", "#2980B9",
#   "palegreen", "#2ECC71", "#27AE60",
#   "palevioletred",
#   "navyblue",
#   "gold","gold4", "gold2","gold3", "#FFDC66","goldenrod3","goldenrod","goldenrod4","gold1",  "darkgoldenrod", "goldenrod1", "goldenrod2",
#   "purple4",
#   "hotpink", "hotpink2"
# )

tmp6$col.code <- c(
  "#FF8C33",  "tomato1", "tomato4", "#B03A2E", "#FF4A1C", "#FF3E06", "red",
  "forestgreen",
  "#5DADE2", "#3498DB", "#2980B9",
  "palegreen", "#27AE60",
  "palevioletred",
  "navyblue",
  "gold","gold3", "#FFDC66","goldenrod3","goldenrod","goldenrod4","gold1",  "darkgoldenrod", "goldenrod1",
  "purple4",
   "hotpink2"
)

tmp6<-tmp6 %>%
  select(-Superclass) %>%
  rename("short.name" = "Class")

tmp8<- data.frame(
  short.name = c(
    "Coral_Dictyota", "Dictyota","Coral_Turf","Turf","Coral"
  ),
  col.code = c(
  "grey90", "grey91","grey92","grey93","grey94"))

# tmp8<- data.frame(
#   short.name = c(
#     "Coral_Dictyota", "Dictyota","Coral_Turf","Turf","Coral"
#   ),
#   col.code = c(
#     "#FF7F00", "#4DAF4A","#F781BF","#984EA3", "#E41A1C"))

tmp9 <- bind_rows(tmp8, tmp7, tmp6)

circle.data <- left_join(circle.data, tmp9, by = "short.name")

labels <- tmp9$short.name
values <- tmp9$col.code
colorcodes <- factor(circle.data$col.code, levels = tmp9$col.code)
linecodes <- factor(circle.data$line.col)
linesize <- factor(circle.data$line.with)
#
# tmp10 <- tmp9 %>%
#   arrange(col.code)

library(igraph)
library(ggraph)

# Then we have to make a 'graph' object using the igraph library:
mygraph <- graph_from_data_frame( edges, vertices=circle.data )


# Make the plot
set.seed(25)
ggraph(mygraph, layout = 'circlepack', weight=size) +
  geom_node_circle() +
  theme_void()

legend.circleplot <- ggraph(mygraph, layout = 'circlepack', weight=size) +
  geom_node_circle(aes(fill = colorcodes, group = depth)) +
  theme_void()+
  scale_fill_manual(name = "Coloring", values = values, labels = labels)+
  theme(legend.position = "right")

legend.circleplot<-get_legend(legend.circleplot)

plot(legend.circleplot)
ggsave("legend_circleplot_decreasing_unique_exudates.jpg", legend.circleplot, path = dirFigs, width = 20, height  = 20, units = c("cm"))

circleplot <- ggraph(mygraph, layout = 'circlepack', weight=size) +
  geom_node_circle(aes(fill = col.code, colour = line.col, group = depth, linewidth = factor(line.with))) +
  # geom_node_text( aes(label= short.name, filter = leaf),check_overlap = T, repel = T, size = 2)+
  theme_void()+
  scale_color_manual(values = levels(factor(circle.data$line.col)))+
  scale_fill_manual(values = levels(factor(circle.data$col.code)))+
  scale_linewidth_manual(values = c(0.4, 1.5))+
  theme(legend.position = "none")
circleplot
ggsave("circleplot_decreasing_unique_exudates.jpg", circleplot, path = dirFigs, width = 15, height  = 15, units = c("cm"))
ggsave("circleplot_decreasing_unique_exudates.eps", circleplot, path = dirFigs, width = 15, height  = 18, units = c("cm"))

decreasing.unique.exudates.circle.data<-circle.data
edges.unique<-edges

######
decreasing.exudates.circle.data <- decreasing.exudates.circle.data %>%
  mutate(extra_Col = "exudates")
decreasing.unique.exudates.circle.data <- decreasing.unique.exudates.circle.data %>%
  mutate(extra_Col = "unique")

new.circle.data <- bind_rows(decreasing.exudates.circle.data, decreasing.unique.exudates.circle.data) %>%
  unite(name_2, all_of(c("extra_Col", "name")), sep = ".")

edges.unique <- edges.unique %>%
  mutate(extra_Col = "unique") %>%
  unite( From, all_of(c("extra_Col", "From")), sep = "." , remove = F) %>%
  unite( To, all_of(c("extra_Col", "To")), sep = "." , remove = F) %>%
  select(-extra_Col)

edges.exudates <- edges.exudates %>%
  mutate(extra_Col = "exudates") %>%
  unite( From, all_of(c("extra_Col", "From")), sep = "." , remove = F) %>%
  unite( To, all_of(c("extra_Col", "To")), sep = "." , remove = F) %>%
  select(-extra_Col)

edges.new <- bind_rows(edges.exudates, edges.unique)

tmp9 <- as.data.frame(new.circle.data$short.name,new.circle.data$col.code) %>%
  rownames_to_column()
colnames(tmp9) <- c("col.code", "short.name")
tmp9 <- tmp9 %>% distinct()


labels <- new.circle.data$short.name
values <- new.circle.data$col.code
colorcodes <- factor(new.circle.data$col.code, levels = tmp9$col.code)
linecodes <- factor(new.circle.data$line.col)
linesize <- factor(new.circle.data$line.with)

# Then we have to make a 'graph' object using the igraph library:
mygraph.new <- graph_from_data_frame( edges.new, vertices=new.circle.data )
set.seed(25)

ggraph(mygraph.new, layout = 'circlepack', weight=size) +
  geom_node_circle() +
  geom_node_text( aes(label= short.name, filter = leaf),check_overlap = T, repel = T, size = 2)+
  theme_void()

legend.circleplot.relative <- ggraph(mygraph.new, layout = 'circlepack', weight=size) +
  geom_node_circle(aes(fill = colorcodes, group = depth)) +
  theme_void()+
  scale_fill_manual(name = "Coloring", values = values, labels = labels)+
  theme(legend.position = "right")

legend.circleplot.relative<-get_legend(legend.circleplot.relative)

tmp9.2<-new.circle.data[order(new.circle.data$name_2),]
plot(100, type="n", axes=F, xlab="", ylab="")
legend("center", fill=tmp9.2$col.code, legend=tmp9.2$short.name, cex=0.5)


plot(legend.circleplot.relative)
ggsave("legend_circleplot_decreasing_unique_exudates_relative.jpg", legend.circleplot.relative, path = dirFigs, width = 20, height  = 20, units = c("cm"))
ggsave("legend_circleplot_decreasing_unique_exudates_relative.eps", legend.circleplot.relative, path = dirFigs, width = 20, height  = 20, units = c("cm"))


circleplot.relative <- ggraph(mygraph.new, layout = 'circlepack', weight=size) +
  geom_node_circle(aes(fill = col.code, colour = line.col, group = depth, linewidth = factor(line.with))) +
  # geom_node_text( aes(label= short.name, filter = leaf),check_overlap = T, repel = T, size = 2)+
  theme_void()+
  scale_color_manual(values = levels(factor(new.circle.data$line.col)))+
  scale_fill_manual(values = levels(factor(new.circle.data$col.code)))+
  scale_linewidth_manual(values = c(0.4, 1.5))+
  theme(legend.position = "none")
circleplot.relative

ggsave("circleplot_decreasing_unique_exudates_relative.jpg", circleplot.relative, path = dirFigs, width = 15, height  = 15, units = c("cm"))
ggsave("circleplot_decreasing_unique_exudates_relative.eps", circleplot.relative, path = dirFigs, width = 18, height  = 18, units = c("cm"))
ggsave("circleplot_decreasing_unique_exudates_relative.eps", circleplot.relative, path = dirFigs, width = 50, height  = 38, units = c("cm"))

