"microbial_counts_FCM.R


Goal:  Analyze FCM data

Written by:
Milou Arts, NIOZ, NL, 2020

List of alterations:


"


FlowCyto_Bact<-read_csv(paste0(dirRAW,'//FlowCyto_Bacteria_metadata.csv'))

# flowrate<-0.046 #in ml, so 46 uL is 0.046
# toconcentration <- function(x, na.rm = FALSE) (x*(1/flowrate))
# FlowCyto_Bact<-FlowCyto_Bact %>% mutate_at(select(starts_with("Nr")), list(toconcentration, x*FlowCyto_Bact$Dilution ))


#tmp<-dplyr::filter(FlowCyto_Bact, Bottle_Nr == 2)
tmp<-FlowCyto_Bact
tmp<-dplyr::filter(FlowCyto_Bact, Treatment_ratio != "BLANK")

tmp$Treatment_ratio<-factor(tmp$Treatment_ratio, levels=c("Dictyota", "Coral_Dictyota", "Coral", "Coral_Turf", "Turf", "Control"))

cost.pal<-c("#4DAF4A","#FF7F00","#E41A1C","#F781BF","#984EA3","#377EB8")

growthcurve<-ggplot(tmp, aes(Time_Point, Concentration_Gate1, group = Replicate, color = Treatment_ratio))+
  #geom_point()+
  #geom_smooth()+
  geom_line()+
  geom_text(data = tmp %>% filter(Time_Point == last(Time_Point)), aes(label = Replicate,
                                                               x = Time_Point + 1.5,
                                                               y = Concentration_Gate1,
                                                               color = Treatment_ratio)) +
  scale_color_manual(values = cost.pal)+
  theme_classic()+
  #scale_y_log10()+
  facet_wrap(~Treatment_ratio, ncol=6)
growthcurve<-growthcurve+
  xlab("Time (hours)")+
  ylab("Concentration (events/mL)")
growthcurve
ggsave('growthcurve.jpeg', plot = growthcurve, path = dirFigs, dpi = 320, width = 15, height = 5)

#calculate natural log of gate1
tmp <- tmp %>% dplyr::mutate(nlog_Gate1 = log(Concentration_Gate1))
growthcurve.nlog<-ggplot(tmp, aes(Time_Point, nlog_Gate1, group = Replicate, color = Treatment_ratio))+
  geom_point()+
  geom_smooth(method = "lm", fill = NA)+
  # geom_line()+
  # geom_text(data = tmp %>% filter(Time_Point == last(Time_Point)), aes(label = Replicate,
  #                                                                      x = Time_Point + 1.5,
  #                                                                      y = nlog_Gate1,
  #                                                                      color = Treatment_ratio)) +
  scale_color_manual(values = cost.pal)+
  theme_classic()+
  #scale_y_log10()+
  facet_wrap(Treatment_ratio~Replicate, ncol=6)+
  xlab("Time (hours)")+
  ylab("log(Concentration")
growthcurve.nlog
ggsave('growthcurve.nlog.jpeg', plot = growthcurve.nlog, path = dirFigs, dpi = 320, width = 15, height = 5)

growthcurve.nlog<-ggplot(tmp, aes(Time_Point, nlog_Gate1, group = Replicate, color = Treatment_ratio))+
  # geom_point()+
  # geom_smooth(method = "lm", fill = NA)+
  geom_line()+
  geom_text(data = tmp %>% filter(Time_Point == last(Time_Point)), aes(label = Replicate,
                                                                        x = Time_Point + 1.5,
                                                                        y = nlog_Gate1,
                                                                        color = Treatment_ratio)) +
  scale_color_manual(values = cost.pal)+
  theme_classic()+
  #scale_y_log10()+
  facet_wrap(~Treatment_ratio, ncol=6)+
  xlab("Time (hours)")+
  ylab("log(Concentration)")
growthcurve.nlog



#select only one bottle
tmp2<-data.frame("Bottle_Nr","Intercept", "slope","correlation")
for(i in levels(factor(tmp$Bottle_Nr))){
tmp1<-tmp %>% dplyr::filter(Bottle_Nr == i)
#remove timepoint 0 and 28
# tmp1<-tmp1 %>% dplyr::filter(Time_Point != 0 & Time_Point != 28)
# linear model
lm_bot1<-lm(tmp1$nlog_Gate1~tmp1$Time_Point)
lm_bot1
print(cor(tmp1$Time_Point,tmp1$nlog_Gate1))

tmp2[nrow(tmp2)+1,]<-c(i,lm_bot1$coefficients[[1]],lm_bot1$coefficients[[2]],cor(tmp1$Time_Point,tmp1$nlog_Gate1))


plot(tmp1$Time_Point,tmp1$nlog_Gate1, xlab="x values", ylab="y values",
     main=paste("Linear Regression Line for Bottle ", i ),
     xlim=c(0,30), ylim=c(11,16),
     xaxp=c(0,80,16), yaxp=c(0,60,12),
     pch=19, col="green", las=1, cex.axis=0.7
)
# abline( v=seq(0,80,5),col="darkgray", lty=3)
# abline( h=seq(0,60,5),col="darkgray", lty=3)
abline(lm_bot1, col="red", lwd=2)

}
colnames(tmp2)<-tmp2[1,]
tmp2<-tmp2[-1,]
tmp2$Bottle_Nr<-as.character(tmp2$Bottle_Nr)
Micr_meta2<-metadata %>% dplyr::filter(MCSMAC ==2)
Micr_meta2$Bottle_Nr<-as.character(Micr_meta2$Bottle_Nr)
tmp2<-right_join(Micr_meta2, tmp2, by ="Bottle_Nr")

# ggplot(tmp)+
#   geom_abline(Intercept~slope)

########


tmp3 <- tmp %>% dplyr::group_by(Treatment_ratio, Time_Point) %>%
  summarise(mean_conG1 = mean(Concentration_Gate1),
            sd_conG1 = sd(Concentration_Gate1),
                          mean_logG1 = mean(nlog_Gate1),
                          sd_logG1 = sd(nlog_Gate1))

tmp4 <- tmp3 %>% dplyr::filter (Time_Point != 0 ) %>%
  dplyr::filter (Time_Point != 28 )

ggplot()+
  geom_smooth(data = tmp4, aes(Time_Point, mean_logG1), method = "lm", color = "grey" , size = 4, fill = NA)+
  geom_point(data = tmp3, aes(Time_Point, mean_logG1, color = Treatment_ratio, fill = Treatment_ratio), size = 3)+
  geom_errorbar(data = tmp3, aes(x=Time_Point, ymin=mean_logG1-sd_logG1, ymax=mean_logG1+sd_logG1, color = Treatment_ratio), size = 1)+
  geom_line(data = tmp3, aes(Time_Point, mean_logG1, color = Treatment_ratio, fill = Treatment_ratio), size =1)+
  scale_color_manual(values = cost.pal)+
  scale_y_continuous(limits = c(10,16))+
  theme_classic()+
  xlab("Time (hours)")+
  ylab("log(Concentration)")+
  facet_wrap(~Treatment_ratio, ncol=6)
ggsave('growthcurve.nlog.pertreatment.jpeg', path = dirFigs, dpi = 320, width = 15, height = 5)


#use for paper
plot5A<-ggplot(tmp3, aes(Time_Point, mean_logG1, color = Treatment_ratio, fill = Treatment_ratio))+
  geom_point(size=4)+
  geom_errorbar(aes(x=Time_Point, ymin=mean_logG1-sd_logG1, ymax=mean_logG1+sd_logG1),)+
  geom_line(size=2)+
  scale_color_manual(values = cost.pal)+
  theme_classic()+
  xlab("Time (hours)")+
  ylab("log(Concentration)")
plot5A
ggsave('Fig5A_growthcurve.jpeg', path = dirFigs, dpi = 320, width = 15, height = 5)

ggplot(tmp3, aes(Time_Point, mean_logG1, color = Treatment_ratio, fill = Treatment_ratio))+
  geom_point(size=4)+
  geom_smooth(method = "lm", fill = NA)+
  scale_color_manual(values = cost.pal)+
  theme_classic()+
  xlab("Time (hours)")+
  ylab("log(Concentration)")


# get the average slope for each treatment
tmp2$Intercept<-as.numeric(tmp2$Intercept)
tmp2$slope<-as.numeric(tmp2$slope)
tmp4<- tmp2 %>% group_by(Treatment_ratio) %>% dplyr::summarise(mean_slope = mean(slope), sd_slope= sd(slope), mean_intercept = mean(Intercept), sd_intercept = sd(Intercept))

co1<-c(unlist(tmp4[1,4]),unlist(tmp4[1,2]))
co2<-c(unlist(tmp4[2,4]),unlist(tmp4[2,2]))
co3<-c(unlist(tmp4[3,4]),unlist(tmp4[3,2]))
co4<-c(unlist(tmp4[4,4]),unlist(tmp4[4,2]))
co5<-c(unlist(tmp4[5,4]),unlist(tmp4[5,2]))
co6<-c(unlist(tmp4[6,4]),unlist(tmp4[6,2]))

plot(NULL, NULL, xlab="x values", ylab="y values",
     main=paste("Linear Regression Lines Treatments" ),
     xlim=c(0,30), ylim=c(11,16),
     pch=19, col="green", las=1, cex.axis=0.7
)
abline(co1, col="blue", lwd=2)
abline(co2, col="red", lwd=2)
abline(co3, col="orange", lwd=2)
abline(co4, col="pink", lwd=2)
abline(co5, col="green", lwd=2)
abline(co6, col="purple", lwd=2)

#stats on slop
model <- lm(slope ~ Treatment_ratio, data = tmp2)
ggqqplot(residuals(model))

#normality
group.resid <- tmp2 %>%
  group_by(Treatment_ratio) %>%
  shapiro_test(slope)
group.resid
#all are normal distributed
#levene test for homogenety  = not signif
tmp2 %>% rstatix::levene_test(slope ~ Treatment_ratio)

#so we do an anova
res.aov<-tmp2 %>% anova_test(slope ~ Treatment_ratio)
print(res.aov)
#not significant
# and tukey
view(tmp2 %>% tukey_hsd(slope ~ Treatment_ratio))



colNames <- names(tmp)[(ncol(tmp)-ngates):ncol(tmp)]



for (i in colNames) {
tmp5<-ggplot(tmp, aes(x=Time_Point, y=.data[[i]], group = Replicate, color = Treatment_ratio))+
  #geom_point()+
  #geom_smooth()+
  geom_line()+
  scale_color_manual(values = cost.pal)+
  theme_classic()+
  #scale_y_log10()+
  facet_wrap(~Treatment_ratio, ncol=6)+
  xlab("Time (hours)")+
  ylab("Concentration (events/mL)")
print(tmp5)
}

