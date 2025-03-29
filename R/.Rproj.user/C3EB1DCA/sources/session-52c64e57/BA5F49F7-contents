#load and analyze DOC values
DirDOC<-paste0(dirRAW, "/DOC")

setwd(DirDOC)
#import all files except the metatable
tmp<-grep(list.files(), pattern = "metatable", invert = TRUE, value = TRUE)
DOC_files<-vector("list", length(tmp))
rm(DOC_values)
if (length(tmp) != 0) {
  for (i in 1:length(tmp)){
    temp<-as.data.frame(read_xlsx(tmp[i]))
    DOC_files[[i]]<-temp
  }}


for (i in 1:length(DOC_files)){
  tmp<-DOC_files[[i]] %>% dplyr::select("Sample Name", "av TN (µM)", "sd TN (µM)", "n (TN)", "av DOC (µM)","sd DOC (µM)","n (DOC)")
  if (exists("DOC_values")){
    DOC_values<-dplyr::bind_rows(DOC_values, tmp)
  }else{DOC_values <- tmp}
}

DOC_values<- DOC_values %>% dplyr::filter(!is.na(`av DOC (µM)`))
DOC_values$Type <- "Sample"
DOC_values$Type = ifelse(grepl("CRM",DOC_values$`Sample Name`),"Standard", "Sample")

#kick out any value above 400
DOC_values <- DOC_values %>% filter(`Sample Name` != "CU18_355")

#plot standards to check
tmp<-DOC_values %>% filter(grepl("CRM",DOC_values$`Sample Name`))
ggplot(tmp, aes(x=as.factor(`Sample Name`), y=`av DOC (µM)`))+
  geom_boxplot()
ggplot(tmp, aes(x=as.factor(`Sample Name`), y=`av TN (µM)`))+
  geom_boxplot()

DOC_meta<-read_xlsx(list.files(pattern = "metatable"))

#combine both datasets

DOC_data<-left_join(DOC_meta, DOC_values, by = c("Sample_ID" = "Sample Name"))

#select only MCSMAC2
#Post soup making
tmp<- DOC_data %>% filter(MCSMAC == 2) %>% filter(Time_Point == -1) %>% filter(!is.na(`av DOC (µM)`))
Treatment2<-factor(tmp$Treatment, levels = c("Blank", "Control", "Dictyota", "Coral", "Turf"))
cost.pal<-c("#FFF8DC", "#98F5FF", "#4DAF4A","#E41A1C","#984EA3")
tmp$`sd DOC (µM)`<-as.numeric(tmp$`sd DOC (µM)`)
fig.DOC.soup<-ggplot(tmp, aes(x=Treatment2,
                y=`av DOC (µM)`,
                fill = Treatment2))+
  geom_bar(stat = "identity",color = "grey")+
  geom_errorbar(aes(ymin=`av DOC (µM)`-`sd DOC (µM)`, ymax=`av DOC (µM)`+`sd DOC (µM)`), width=.2)+
  scale_x_discrete(paste0("After Exudation")) +
  scale_y_continuous(name = "av DOC (µM)") +
  scale_fill_manual(name = "Treatment", values = cost.pal) +
  theme_classic()
fig.DOC.soup

# T0
tmp<- DOC_data %>% filter(MCSMAC == 2) %>% filter(Time_Point == 0) %>% filter(!is.na(`av DOC (µM)`))
Treatment2<-factor(tmp$Treatment, levels = c("Blank", "Control", "Dictyota", "Coral_Dictyota", "Coral","Coral_Turf","Turf"))
cost.pal<-c("#FFF8DC", "#98F5FF", "#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")
tmp$`sd DOC (µM)`<-as.numeric(tmp$`sd DOC (µM)`)

fig.DOC.T0<-ggplot(tmp, aes(
    x=Treatment2,
    y=`av DOC (µM)`,
    fill = Treatment2))+
  geom_bar(stat = "identity",color = "grey")+
  geom_errorbar(aes(
    ymin=`av DOC (µM)`-`sd DOC (µM)`,
    ymax=`av DOC (µM)`+`sd DOC (µM)`),
    width=.2)+
  scale_x_discrete(paste0("Start of Incubation")) +
  scale_y_continuous(name = "av DOC (µM)") +
  scale_fill_manual(name = "Treatment", values = cost.pal) +
  theme_classic()

fig.DOC.T0

# make table of this data
knitr::kable(tmp %>%
               select(Sample_ID, Time_Point, Treatment, 'av DOC (µM)', "sd DOC (µM)", "n (DOC)", "av TN (µM)", "sd TN (µM)", "n (TN)")

) %>%
  column_spec(4, bold = T) %>%
  row_spec(0, bold = T) %>%
  kable_classic()
#save manually to figure folder

#remove blank
tmp <- tmp %>% filter(Treatment != "Blank")
summarise(tmp, mean = mean(`av DOC (µM)`), sd = sd(`av DOC (µM)`))

# T28
tmp<- DOC_data %>% filter(MCSMAC == 2) %>% filter(Time_Point == 28) %>% filter(!is.na(`av DOC (µM)`)) %>% filter(is.na(Notes))
Treatment<-factor(tmp$Treatment, levels = c("Control", "Dictyota", "Coral_Dictyota", "Coral","Coral_Turf","Turf"))
tmp <- tmp %>% dplyr::group_by(Treatment) %>% summarise(mean = mean(`av DOC (µM)`), sd = sd(`av DOC (µM)`), n=n())
Treatment2<-factor(tmp$Treatment, levels = c("Control", "Dictyota", "Coral_Dictyota", "Coral","Coral_Turf","Turf"))
cost.pal<-c("#98F5FF", "#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")

fig.DOC.Tend<-ggplot(tmp, aes(x=Treatment2,
                y=mean,
                fill = Treatment2))+
  geom_bar(stat = "identity",color = "grey")+
  geom_errorbar(aes(ymin=mean-sd, ymax= mean+sd), width=.2)+
  scale_x_discrete(paste0("End of Incubation")) +
  scale_y_continuous(name = "av DOC (µM)") +
  scale_fill_manual(name = "Treatment", values = cost.pal) +
  theme_classic()
fig.DOC.Tend

tmp2<-tmp
tmp2$Time_Point <- 28
rm(Treatment)
tmp.1<-tmp.1 %>% select(Treatment, `av DOC (µM)`, `sd DOC (µM)`, `n (DOC)`, Time_Point)
colnames(tmp.1)<-colnames(tmp2)
tmp2$Time_Point<-as.character(tmp2$Time_Point)
tmp.1<-bind_rows(tmp.1, tmp2)

Treatment2<-factor(tmp.1$Treatment, levels = c("Blank", "Control", "Dictyota", "Coral_Dictyota", "Coral","Coral_Turf","Turf"))
cost.pal<-c("#FFF8DC", "#98F5FF", "#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")

ggplot(tmp.1, aes(x=Time_Point,
                y=mean,
                fill = Treatment2))+
  geom_col(position = "dodge",color = "grey")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
  scale_y_continuous(name = "av DOC (µM)") +
  scale_fill_manual(name = "Treatment", values = cost.pal) +
  theme_classic()+
  facet_wrap(Treatment2)






#plot everything together
tmp<- DOC_data %>% filter(MCSMAC == 2) %>% filter(!is.na(`av DOC (µM)`)) %>% filter(is.na(Notes))
Treatment2<-factor(tmp$Treatment, levels = c("Blank", "Control", "Dictyota", "Coral_Dictyota", "Coral","Coral_Turf","Turf"))
TimePoint.rep<-factor(tmp$Timepoint_Rep, levels = c("-1", "0", "28.1", "28.2", "28.3"))
cost.pal<-c("#FFF8DC", "#98F5FF", "#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3")
tmp$`sd DOC (µM)`<-as.numeric(tmp$`sd DOC (µM)`)
ggplot(tmp, aes(x=TimePoint.rep,
                y=`av DOC (µM)`,
                fill = Treatment2))+
  geom_col(position = "dodge",color = "grey")+
  geom_errorbar(aes(ymin=`av DOC (µM)`-`sd DOC (µM)`, ymax=`av DOC (µM)`+`sd DOC (µM)`), width=.2)+
  scale_y_continuous(name = "av DOC (µM)") +
  scale_fill_manual(name = "Treatment", values = cost.pal) +
  theme_classic()+
  facet_wrap(Treatment2)

