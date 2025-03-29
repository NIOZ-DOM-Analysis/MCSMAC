'Microbial size.R


Infer the microbial biomass. originally based on their length and width,

now width will be calculated based on their length (=max feret), and area.

assumptions are:  microbes are rod shaped/capsule, geometrical that means a cylinder with two hemispherical ends (2 half.
that means that with the microscope pictures being 2D, the area is the same as the intersection through the middel of the capsule,
this area is similar to the area of 2x half a circel (= 1 circel) + the area of a rectangle (=intersection of a cylinder)

this script takes the output/export table file from the Zen Blue 3.1 Carl Zeiss software

Milou G.I. Arts, 2020

'
'
Math:

Uses l (length) and w (width) to calculate V (volume)

V = pi/4 * w^2 * (l - w/3)

uses A (area) and l (lenght) to estimate w

w = 2/(pi-4)(sqrt(l^2 + (pi -4)A ) - l)

Uses A (area) and P (perimeter) to estimate w

w = (P - sqrt(P^2 - 4PiA))/Pi
l = P/2 + w(1-(pi/2))

when V with estimated w and l from A and P fails
V for sphere = 4/3(sqrt((A^3)/pi))

Massana et al. 1997, and

McDole et al. 2012:

Volume V can be converted to dry Weight (x) using Simon and Azam (1989)

log (V) = 1.72log(x) -12.63

(x = e^(1/172 (1263 + 100 log(V)))
(log (x) = (log(V) + 12.63)/1.72

Then cell
wet weight (z) was calculated using the linear relation

log(z) =


'
library(ggridges)
library(stringi)
library(car)
# We are going to read all the files in the folder so we can put them together.
#define the folder

setwd(paste0(dirRAW,'/Microbial_size'))
file_list <- list.files()

for (file in file_list){

  #if the merged dataset doesn't exist, create it
  if (!exists("Micr_size")){
    Micr_size <- read_csv(file)
    #change colnames because they are shitty
    tmp <- colnames(Micr_size)
    tmp <- tmp %>% stri_split_regex(pattern = "!!|::")

    for (n in 1:length(tmp)){
      colnames(Micr_size)[n]<- tmp[[n]][2]
    }

    Micr_size <- Micr_size %>% filter(!is.na(ID))
    Micr_size$ID <- 1:nrow(Micr_size)
    Micr_size <- Micr_size %>% mutate_if(is.character,as.numeric)

    #now add a column with the file name and then we are going te split that one in two.
    Micr_size <- Micr_size %>% add_column( FileName = file) %>%
      tidyr::separate(FileName, c("ExpName", "SampleNr", "PictureNr", "Magnification", "rest"), sep = "_") %>%
      select(-rest) %>%
      unite("Sample_Name", ExpName:SampleNr, sep = "_")
  }
  # if it does exist, we want put all the info in a temporary file, and bind it to the existing data.

  if (exists("Micr_size")){
    Micr_size_tmp <- read_csv(file)
    #change colnames because they are shitty
    tmp <- colnames(Micr_size_tmp)
    tmp <- tmp %>% stri_split_regex(pattern = "!!|::")

    for (n in 1:length(tmp)){
      colnames(Micr_size_tmp)[n]<- tmp[[n]][2]
    }

    Micr_size_tmp <- Micr_size_tmp %>% filter(!is.na(ID))
    Micr_size_tmp$ID <- 1:nrow(Micr_size_tmp)
    Micr_size_tmp <- Micr_size_tmp %>% mutate_if(is.character,as.numeric)

    #now add a column with the file name and then we are going te split that one in two.
    Micr_size_tmp <- Micr_size_tmp %>% add_column( FileName = file) %>%
      tidyr::separate(FileName, c("ExpName", "SampleNr", "PictureNr", "Magnification", "rest"), sep = "_") %>%
      select(-rest) %>%
      unite("Sample_Name", ExpName:SampleNr, sep = "_")

    #now bind the two files and remove the tmp file

    Micr_size <- bind_rows(Micr_size, Micr_size_tmp)
    rm(Micr_size_tmp)
  }
}

#move columns Sample, PictureNr, and Magnification to the left side of the table.
Micr_size <- relocate(Micr_size, Sample_Name, PictureNr, Magnification)

#a summary of the number of microbes per sample and how many per picture of each sample
Micr_size %>% group_by(Sample_Name) %>% summarise(n()) %>% view()
Micr_size %>% group_by(Sample_Name, PictureNr) %>% summarise(n()) %>% view()


#we take the feret maximum as length to calculate w
#w = 2/(pi-4)(sqrt(l^2 + (pi -4)A ) - l)
Micr_size <- mutate(Micr_size, width = (2/(pi-4))*(sqrt(`Feret Maximum`^2 + (pi-4)*`Area` ) - `Feret Maximum`) )

# calculate volume
Micr_size <- mutate(Micr_size, Volume = (pi/4) * `width`^2 * (`Feret Maximum` - (`width`/3)))

#calculate dry Weight from volume
Micr_size <- mutate(Micr_size, `log(Dry Weight)` =  (log(`Volume`) + 12.63)/1.72 )
Micr_size <- mutate(Micr_size, `Dry Weight` = exp((1/172) * (1263 + 100*log(`Volume`))))
Micr_size <- mutate(Micr_size, `log(Wet Weight)` =  1.63*`log(Dry Weight)`-2.0)
Micr_size <- mutate(Micr_size, `Wet Weight` = exp(`log(Wet Weight)`))

setwd(paste0("../", dirOutput))
write_csv(Micr_size, "Microbe_size.csv")

#load in the metadata for the microbial samples.

setwd(dirRAW)
Micr_meta <- read_csv("metadata_microbe_size.csv")

Micr_size <- full_join(Micr_meta, Micr_size, by = "Sample_Name")
list.col<-colnames(Micr_size)[15]
Treatment.summ<-Micr_size %>% group_by(Treatment_ratio) %>%  summarise(across(Area:`Wet Weight`, list(mean = mean, sd = sd), .names = "{.col}.fn.{.fn}"), n = n())
Bottle.summ<-Micr_size %>% group_by(Bottle_Nr) %>%  summarise(across(Area:`Wet Weight`, list(mean = mean, sd = sd), .names = "{.col}.fn.{.fn}"), n = n())


setwd(wd.project)

#now let's combine this with the FlowCyto data
#define which time point you have been looking at

#timepoint <- 21


if (exists("FlowCyto_Bact")){

  tmp <- dplyr::filter(FlowCyto_Bact, Time_Point == timepoint)
  vol.summ <- left_join(Bottle.summ, tmp, by = "Bottle_Nr")
  vol.summ <- vol.summ %>% dplyr::mutate(biomass = Volume.fn.mean * Concentration)
  vol.summ <- vol.summ%>% dplyr::filter(!is.na(biomass))

  tmp <- tmp %>% group_by(Treatment_ratio) %>% summarize(mean_con = mean(Concentration, sd_con = sd(Concentration)))
}

#or 1) combine mean concentration of tmp, with treatment.summ volume for biomass
#or 2) calculate mean biomass by using the vol.summ/bottle averages of volume x concentration of that bottle.

#option 1)
tmp <- left_join(tmp, Treatment.summ, by = "Treatment_ratio")
tmp$Treatment_ratio <- factor(tmp$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))
tmp <- tmp %>% dplyr::mutate(biomass = Volume.fn.mean * mean_con)

ggplot(tmp, aes(Treatment_ratio, biomass, fill = Treatment_ratio))+
  geom_col()+
  scale_fill_manual(values = cost.pal)


#option 2
tmp2<-vol.summ %>% group_by(Treatment_ratio) %>% summarize(sum_bio = sum(biomass), sd_bio = sd(biomass), mean_bio = mean(biomass))
tmp2$Treatment_ratio <- factor(tmp2$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))

ggplot(tmp2, aes(Treatment_ratio, mean_bio, fill = Treatment_ratio))+
  geom_col()+
  geom_errorbar(aes(ymin = mean_bio - sd_bio, ymax = mean_bio + sd_bio),
                width = 0.2)+
  scale_fill_manual(values = cost.pal)

#put option 2 on different scales
vol.summ$Treatment_ratio <- factor(vol.summ$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))
vol.summ <- vol.summ %>% mutate(log10_biomass = log10(biomass), nlog_biomass = log(biomass))
tmp3<-vol.summ %>% group_by(Treatment_ratio) %>% summarize(mean_bio = mean(biomass), sd_bio = sd(biomass), mean_log10bio = mean(log10_biomass), sd_log10bio = sd(log10_biomass), mean_nlogbio = mean(nlog_biomass), sd_nlogbio = sd(nlog_biomass))

ggplot(tmp3, aes(Treatment_ratio, mean_bio, fill = Treatment_ratio))+
  geom_col()+
  geom_errorbar(aes(ymin = mean_bio - sd_bio, ymax = mean_bio + sd_bio),
                width = 0.2)+
  scale_y_continuous(name = "Mean biomass +sd")+
  scale_fill_manual(values = cost.pal)

ggplot(tmp3, aes(Treatment_ratio, mean_log10bio, fill = Treatment_ratio))+
  geom_col()+
  geom_errorbar(aes(ymin = mean_log10bio - sd_log10bio, ymax = mean_log10bio + sd_log10bio),
                width = 0.2)+
  scale_y_continuous(name = "Mean log10(biomass) +sd")+
  scale_fill_manual(values = cost.pal)

ggplot(tmp3, aes(Treatment_ratio, mean_nlogbio, fill = Treatment_ratio))+
  geom_col()+
  geom_errorbar(aes(ymin = mean_nlogbio - sd_nlogbio, ymax = mean_nlogbio + sd_nlogbio),
                width = 0.2)+
  scale_y_continuous(name = "Mean nlog(biomass) +sd")+
  scale_fill_manual(values = cost.pal)

ggplot(vol.summ, aes(Treatment_ratio, biomass, fill = Treatment_ratio))+
  geom_boxplot()+
  geom_jitter()+
  scale_y_continuous(name = "Biomass")+
  scale_fill_manual(name = "Treatment", values = cost.pal)+
  scale_x_discrete(name = " ")+
  theme_bw()
ggsave("biomass.jpg", width = 6, height = 5, path = dirFigs)

ggplot(vol.summ, aes(Treatment_ratio, log10_biomass, fill = Treatment_ratio))+
  geom_boxplot()+
  scale_y_continuous(name = "log10(biomass)")+
  scale_fill_manual(values = cost.pal)

ggplot(vol.summ, aes(Treatment_ratio, nlog_biomass, fill = Treatment_ratio))+
  geom_boxplot()+
  scale_y_continuous(name = "nlog(biomass)")+
  scale_fill_manual(values = cost.pal)

#####
#do stats on this
model <- lm(biomass ~ Treatment_ratio, data = vol.summ)
ggqqplot(residuals(model))
model <- lm(biomass ~ Bottle_Nr, data = vol.summ)
ggqqplot(residuals(model))

#shapiro
vol.summ %>%
  group_by(Treatment_ratio) %>%
  summarise(mean = mean(biomass), n = n())

tmp<-vol.summ %>% dplyr::filter(Treatment_ratio != "Coral") %>%
  dplyr::filter(Treatment_ratio != "Turf")
tmp %>% group_by(Treatment_ratio) %>%
  summarise(mean = mean(biomass), n = n())

group.resid <- tmp %>%
  group_by(Treatment_ratio) %>%
  shapiro_test(biomass)
group.resid



#shapiro test is significant in coral_turf an coral_dictyota, so not normally distributed
# coral and turf could not be tested because n=2

#levene homogeneity test for equal variences
lev.tst <- vol.summ %>% rstatix::levene_test(biomass ~ Treatment_ratio)
lev.tst
#variences are equal

#anova
res.aov <- vol.summ %>% anova_test(biomass ~ Treatment_ratio)
view(res.aov)
#tukey
pwc <- vol.summ %>% tukey_hsd(biomass ~ Treatment_ratio)
view(pwc)

#now lets treat every bottle as a
if (exists("FlowCyto_Bact")){

  tmp <- dplyr::filter(FlowCyto_Bact, Time_Point == timepoint)
  tmp <- dplyr::select(tmp, Bottle_Nr, 'Nr of Events':Concentration_LDNA)
  tmp <- left_join(Micr_size, tmp, by = "Bottle_Nr")
  tmp <- tmp %>% dplyr::mutate(biomass = Volume * Concentration)
  tmp <- tmp %>% dplyr::filter(!is.na(biomass))
}


hist(tmp$biomass)
tmp$Treatment_ratio <- factor(tmp$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))
type<- factor(tmp$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))

ggplot(tmp, aes(x=biomass, fill=type)) +
  geom_histogram(alpha=0.8, binwidth = 500) +
  scale_fill_manual(values = cost.pal) +
  labs(fill="")+
  facet_wrap(~Treatment_ratio)

ggplot(tmp, aes(x=log2(biomass), y=type, fill = type)) +
  geom_density_ridges(alpha = 0.8) +
  scale_fill_manual(values = cost.pal) +
  scale_y_discrete(name = "")+
  scale_x_continuous(limits = c(12,25))+
  theme_bw()+
  labs(fill="")

ggplot(Micr_size, aes(x=log2(Volume), y=Treatment_ratio, fill = Treatment_ratio)) +
  geom_density_ridges(alpha = 0.8) +
  scale_fill_manual(values = cost.pal) +
  scale_y_discrete(name = "")+
  scale_x_continuous(limits = c(-3,1))+
  theme_bw()+
  labs(fill="")



BotNr<-factor(Micr_size$Bottle_Nr)
type<- factor(Micr_size$Treatment_ratio, levels = c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))

ggplot(Micr_size, aes(x=log2(Volume), y=BotNr, fill = type)) +
  geom_density_ridges(alpha = 0.8) +
  scale_fill_manual(values = cost.pal) +
  scale_y_discrete(name = "")+
  scale_x_continuous(limits = c(-10,5))+
  theme_bw()+
  labs(fill="")

ggplot(Micr_size, aes(x=log2(Volume), y=type, fill = type)) +
  geom_density_ridges(alpha = 0.8) +
  scale_fill_manual(values = cost.pal) +
  scale_y_discrete(name = "")+
  scale_x_continuous(limits = c(-10,5))+
  theme_bw()+
  labs(fill="")

tmp<-cld(glht(aov(log2(Volume) ~ type, data = Micr_size), linfct = mcp(type = "Tukey")))
tmp<-tmp$mcletters
tmp<-as.data.frame(tmp$Letters)
tmp$`tmp$Letters`<-toupper(tmp$`tmp$Letters`)
tmp<-rownames_to_column(tmp)
colnames(tmp) <- c("type", "Letters")

plot5B<-ggplot(Micr_size, aes(x=log2(Volume), y=type, fill = type)) +
  geom_density_ridges(alpha = 0.8) +
  scale_fill_manual(values = cost.pal) +
  scale_y_discrete(name = "")+
  scale_x_continuous(limits = c(-10,5))+
  theme_bw()+
  geom_text(data = tmp, aes(5, type, label = Letters))+
  labs(fill="")+
  theme(legend.position="none")

tmp<-cld(glht(aov(Circularity ~ type, data = Micr_size), linfct = mcp(type = "Tukey")))
tmp<-tmp$mcletters
tmp<-as.data.frame(tmp$Letters)
tmp$`tmp$Letters`<-toupper(tmp$`tmp$Letters`)
tmp<-rownames_to_column(tmp)
colnames(tmp) <- c("type", "Letters")

plot5C<-ggplot(Micr_size, aes(x=Circularity, y=type, fill = type)) +
  geom_density_ridges(alpha = 0.8) +
  scale_fill_manual(values = cost.pal) +
  scale_y_discrete(name = "")+
  # scale_x_continuous(limits = c(-10,5))+
  theme_bw()+
  geom_text(data = tmp, aes(1.02, type, label = Letters))+
  labs(fill="")+
  theme(legend.position="none")

ggplot(Micr_size, aes(x= asin(sqrt(Circularity)), y=type, fill = type)) +
  geom_density_ridges(alpha = 0.8) +
  scale_fill_manual(values = cost.pal) +
  scale_y_discrete(name = "")+
  # scale_x_continuous(limits = c(-10,5))+
  theme_bw()+
  labs(fill="")

ggplot(Micr_size, aes(x=type, y=Circularity, fill = type)) +
  geom_boxplot() +
  scale_fill_manual(values = cost.pal) +
  # scale_y_discrete(name = "")+
  # scale_x_continuous(limits = c(-10,5))+
  theme_bw()+
  labs(fill="")

if (exists("FlowCyto_Bact")){

  tmp <- dplyr::filter(FlowCyto_Bact, Time_Point == timepoint)
  tmp <- dplyr::select(tmp, Bottle_Nr, 'Nr of Events':Concentration_LDNA)
  tmp <- left_join(Micr_size, tmp, by = "Bottle_Nr")
  tmp <- tmp %>% dplyr::mutate(biomass = Volume * Concentration)
  tmp <- tmp %>% dplyr::filter(!is.na(biomass))
}

group.resid <- tmp %>%
  group_by(Bottle_Nr) %>%
  shapiro_test(biomass)
view(group.resid)

group.resid <- tmp %>%
  group_by(Treatment_ratio) %>%
  shapiro_test(biomass)
view(group.resid)




#levene homogeneity test for equal variences
tmp$Bottle_Nr <- factor(tmp$Bottle_Nr)
lev.tst <- tmp %>% rstatix::levene_test(biomass ~ Bottle_Nr)
lev.tst

lev.tst <- tmp %>% rstatix::levene_test(biomass ~ Treatment_ratio)
lev.tst
#variences are not equal

# in case of not eaqual variances
# Welch One way ANOVA test
res.aov2 <- tmp %>% welch_anova_test(biomass ~ Treatment_ratio)
res.aov3 <- tmp %>% kruskal_test(biomass ~ Treatment_ratio)
res.aov4 <- tmp %>% welch_anova_test(biomass ~ Bottle_Nr)
res.aov5 <- tmp %>% kruskal_test(biomass ~ Bottle_Nr)
# Pairwise comparisons (Games-Howell)
pwc2 <- tmp %>% games_howell_test(biomass ~ Treatment_ratio)
pwc3 <- tmp %>% dunn_test(biomass ~ Treatment_ratio)
pwc4 <- tmp %>% games_howell_test(biomass ~ Bottle_Nr)
pwc5 <- tmp %>% dunn_test(biomass ~ Bottle_Nr)



###circularity as mean of mean
tmp3<-Micr_size %>%
  group_by(Bottle_Nr, Treatment_ratio) %>%
  summarise(meanCirc = mean(Circularity), n)

group.resid <- tmp3 %>%
  group_by(Treatment_ratio) %>%
  filter(Treatment_ratio != "Coral") %>%
  filter(Treatment_ratio != "Turf") %>%
  shapiro_test(meanCirc)
view(group.resid)

tmp3$Treatment_ratio <- as.factor(tmp3$Treatment_ratio)
leveneTest(tmp3$meanCirc ~ tmp3$Treatment_ratio)

anova(lm(tmp3$meanCirc~tmp3$Treatment_ratio))

###circularity on cell level
group.resid <- Micr_size %>%
  group_by(Treatment_ratio) %>%
  shapiro_test(Circularity)
view(group.resid)
#not normal
leveneTest( asin(sqrt(Micr_size$Circularity)) ~ Micr_size$Treatment_ratio)
#not equal
# in case of not eaqual variances
# Welch One way ANOVA test
res.aov2 <- Micr_size %>% welch_anova_test(Circularity ~ Treatment_ratio)
res.aov3 <- Micr_size %>% kruskal_test(Circularity ~ Treatment_ratio)
# res.aov4 <- Micr_size %>% welch_anova_test(Circularity ~ Bottle_Nr)
# res.aov5 <- Micr_size %>% kruskal_test(Circularity ~ Bottle_Nr)
# Pairwise comparisons (Games-Howell)
pwc2 <- Micr_size %>% games_howell_test(Circularity ~ Treatment_ratio)
pwc3 <- Micr_size %>% dunn_test(Circularity ~ Treatment_ratio)
# pwc4 <- Micr_size %>% games_howell_test(Circularity ~ Bottle_Nr)
# pwc5 <- Micr_size %>% dunn_test(Circularity ~ Bottle_Nr)
res.aov2
res.aov3
view(pwc2)
view(pwc3)
Micr_size %>% group_by(Treatment_ratio) %>% dplyr::summarise(mean(Circularity), sd(Circularity), n())



###Volume on cell level
group.resid <- Micr_size %>%
  group_by(Treatment_ratio) %>%
  shapiro_test(Volume)
view(group.resid)
#not normal
leveneTest( asin(sqrt(Micr_size$Volume)) ~ Micr_size$Treatment_ratio)
leveneTest( log10(Micr_size$Volume) ~ Micr_size$Treatment_ratio)
#not equal
# in case of not eaqual variances
# Welch One way ANOVA test
res.aov2 <- Micr_size %>% welch_anova_test(Volume ~ Treatment_ratio)
res.aov3 <- Micr_size %>% kruskal_test(Volume ~ Treatment_ratio)
# res.aov4 <- Micr_size %>% welch_anova_test(Volume ~ Bottle_Nr)
# res.aov5 <- Micr_size %>% kruskal_test(Volume ~ Bottle_Nr)
# Pairwise comparisons (Games-Howell)
pwc2 <- Micr_size %>% games_howell_test(Volume ~ Treatment_ratio)
pwc3 <- Micr_size %>% dunn_test(Volume ~ Treatment_ratio)
# pwc4 <- Micr_size %>% games_howell_test(Volume ~ Bottle_Nr)
# pwc5 <- Micr_size %>% dunn_test(Volume ~ Bottle_Nr)
res.aov2
res.aov3
view(pwc2)
view(pwc3)

Micr_size %>% group_by(Treatment_ratio) %>% dplyr::summarise(mean(Volume), sd(Volume), n())



library(cowplot)
plot5<-cowplot::plot_grid(plot5A, plot_grid(plot5B, plot5C , nrow = 1, labels = c("B", "C")), nrow = 2, rel_heights = c(1.5,1), labels = c("A", ""))


plot5
ggsave('Fig5_microbial growth and size.jpg', plot5, path = dirFigs, units = c("cm"), width = 19, height = 20)
