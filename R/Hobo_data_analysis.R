'Hobo_data_analysis.R

Milou Arts, 2022'

#show files
dir(paste0(dirRAW, "/HOBO"), pattern = ".csv")

#exududation part  on the 27th
solar_noon <- as.POSIXct("2018-11-27 12:23")
exudation_start <- as.POSIXct("2018-11-27 10:00")
exudation_finish <- as.POSIXct("2018-11-27 16:00")
added_water_to_logger <- as.POSIXct("2018-11-27 11:46")

#import PAR loggers

#outside comparison
outside1 <- read_csv(paste0(dirRAW, "/HOBO/181121_PARlogger_OUTSIDE_001_001.csv"), skip = 4)
outside1 <- outside1 %>% rename('Date' = 'Date and Time',
                                'Time' = 'Integrating Light',
                                'Intergrating_light_Raw_value' = '...4',
                                'Intergrating_light_Calibrated_value' = '...5') %>%
                      dplyr::filter(!is.na(`Scan No`))

outside2 <- read_csv(paste0(dirRAW, "/HOBO/181121_PARlogger_OUTSIDE_002_001.csv"), skip = 4)
outside2 <- outside2 %>% rename('Date' = 'Date and Time',
                                'Time' = 'Integrating Light',
                                'Intergrating_light_Raw_value' = '...4',
                                'Intergrating_light_Calibrated_value' = '...5') %>%
  dplyr::filter(!is.na(`Scan No`))


#fix the date and time so we can plot it.
outside1$Date<-as.Date(outside1$Date, "%d/%m/%Y")
outside2$Date<-as.Date(outside2$Date, "%d/%m/%Y")
outside1$Date_Time <- as.POSIXct(paste(outside1$Date, outside1$Time))
outside2$Date_Time <- as.POSIXct(paste(outside2$Date, outside2$Time))

#now plot both over eachother
Outside1_2<-ggplot()+
  geom_line(aes(outside1$Date_Time, outside1$Intergrating_light_Calibrated_value, color = "Logger 1"))+
  geom_line(aes(outside2$Date_Time, outside2$Intergrating_light_Calibrated_value, color = "Logger 2"))+
  scale_color_manual(name = "Logger", values = c("Logger 1" = "blue", "Logger 2" = "red"))+
  scale_x_datetime(name = "Time")+
  scale_y_continuous(name = "Logger Signal")+
  theme_bw()
Outside1_2

######
#Greenouse vs reef comparison vs outside
Reef_Greenhouse <- read_csv(paste0(dirRAW, "/HOBO/181122_REEF_and_greenhouse_001_001.csv"), skip = 4)
Reef_Greenhouse <- Reef_Greenhouse %>% rename('Date' = 'Date and Time',
                                'Time' = 'Integrating Light',
                                'Intergrating_light_Raw_value' = '...4',
                                'Intergrating_light_Calibrated_value' = '...5') %>%
  dplyr::filter(!is.na(`Scan No`))

Outside3 <- read_csv(paste0(dirRAW, "/HOBO/181122_OUTSIDE2211_001_001.csv"), skip = 4)
Outside3 <- Outside3 %>% rename('Date' = 'Date and Time',
                                'Time' = 'Integrating Light',
                                'Intergrating_light_Raw_value' = '...4',
                                'Intergrating_light_Calibrated_value' = '...5') %>%
  dplyr::filter(!is.na(`Scan No`))


#fix the date and time so we can plot it.
Reef_Greenhouse$Date<-as.Date(Reef_Greenhouse$Date, "%d/%m/%Y")
Outside3$Date<-as.Date(Outside3$Date, "%d/%m/%Y")
Reef_Greenhouse$Date_Time <- as.POSIXct(paste(Reef_Greenhouse$Date, Reef_Greenhouse$Time))
Outside3$Date_Time <- as.POSIXct(paste(Outside3$Date, Outside3$Time))
Reef_Greenhouse$date <- as.factor(Reef_Greenhouse$Date)

tmp<-Reef_Greenhouse %>% dplyr::select(Date, Time, Intergrating_light_Calibrated_value)
tmp <- pivot_wider(tmp, names_from = c(Date), values_from = Intergrating_light_Calibrated_value)
tmp2 <- pivot_longer(tmp, cols = -Time, names_to = "Date")
tmp2 <- tmp2 %>% filter(!is.na(value))
tmp2$Time <-as.POSIXct(strptime(tmp2$Time, "%H:%M"))
tmp2$date <-as.factor(tmp2$Date)


#now plot both over each other

Reef_Greenhouse<- ggplot(tmp2, aes(Time, value, color = date))+
  geom_line()+
  scale_color_manual(name = "Location", values = c("red", "green"), labels = c("Reef", "Greenhouse"))+
  scale_y_continuous(name = "Logger Signal")+
  scale_x_datetime(name = "Time", date_breaks = "3 hours", date_labels = "%H:%M")+
  facet_grid(date ~ .)+
  theme_bw()
Reef_Greenhouse

tmp<-Outside3 %>% dplyr::select(Date, Time, Intergrating_light_Calibrated_value)
tmp <- pivot_wider(tmp, names_from = c(Date), values_from = Intergrating_light_Calibrated_value)
tmp2 <- pivot_longer(tmp, cols = -Time, names_to = "Date")
tmp2 <- tmp2 %>% filter(!is.na(value))
tmp2$Time <-as.POSIXct(strptime(tmp2$Time, "%H:%M"))
tmp2$date <-as.factor(tmp2$Date)

Outside3<-ggplot(tmp2, aes(Time, value, color = "blue"))+
  geom_line()+
  scale_color_manual(name = "Location", values = c("blue"), labels = c("Outside"))+
  scale_y_continuous(name = "Logger Signal")+
  scale_x_datetime(name = "Time", date_breaks = "3 hours", date_labels = "%H:%M")+
  facet_grid(facets = date ~ .)+
  theme_bw()

library(cowplot)

plot_grid(Reef_Greenhouse, Outside3, labels = c('A', 'B'), label_size = 12)

### plot the measurements of the experiment

#import PAR loggers

#outside comparison
Par.exp <- read_csv(paste0(dirRAW, "/HOBO/181128_PARlogger_EXPERIMENT_001_001.csv"), skip = 4)
Par.exp <- Par.exp %>% rename('Date' = 'Date and Time',
                                'Time' = 'Integrating Light',
                                'Intergrating_light_Raw_value' = '...4',
                                'Intergrating_light_Calibrated_value' = '...5') %>%
  dplyr::filter(!is.na(`Scan No`))


#fix the date and time so we can plot it.
Par.exp$Date<-as.Date(Par.exp$Date, "%d/%m/%Y")
Par.exp$Date_Time <- as.POSIXct(paste(Par.exp$Date, Par.exp$Time))
Par.exp$date <- as.factor(Par.exp$Date)

tmp<-Par.exp %>% dplyr::select(Date, Time, Intergrating_light_Calibrated_value)
tmp <- pivot_wider(tmp, names_from = c(Date), values_from = Intergrating_light_Calibrated_value)
tmp2 <- pivot_longer(tmp, cols = -Time, names_to = "Date")
tmp2 <- tmp2 %>% filter(!is.na(value))
tmp2$Time <-as.POSIXct(strptime(tmp2$Time, "%H:%M"))
tmp2$date <-as.factor(tmp2$Date)
tmp2 <- tmp2 %>% filter(date == "2018-11-27")
tmp4 <- tmp2

Experiment<-ggplot(tmp2, aes(Time, value, color = "plum"))+
  geom_line()+
  scale_color_manual(name = "Location", values = c("plum"), labels = c("Greenhouse"))+
  scale_y_continuous(name = "Logger Signal")+
  scale_x_datetime(name = "Time", date_breaks = "3 hours", date_labels = "%H:%M")+
  facet_grid(facets = date ~ .)+
  theme_bw()
Experiment



hobo.exp <- read_csv(paste0(dirRAW, "/HOBO/181128_Hobo_experiment_adjusted.csv"), skip = 1)
colnames(hobo.exp)<-c("Scan No", "Date_Time", "Temp", "Intensity (Lux)", "re", "move", "this","column")
hobo.exp <- hobo.exp %>% dplyr::select("Scan No", "Date_Time", "Temp", "Intensity (Lux)")

hobo.exp$Date<-strptime(hobo.exp$Date_Time,  c("%m/%d/%y %I:%M:%S %p"))
hobo.exp$Date<-as.Date(hobo.exp$Date, "%m/%d/%y")

hobo.exp$Time<-format(strptime(hobo.exp$Date_Time,  c("%m/%d/%y %I:%M:%S %p")), "%H:%M")
hobo.exp$Date_time <- as.POSIXct(paste(hobo.exp$Date, hobo.exp$Time))
hobo.exp$date <- as.factor(hobo.exp$Date)

tmp<-hobo.exp %>% dplyr::select(Date, Time, `Intensity (Lux)`)
tmp <- pivot_wider(tmp, names_from = c(Date), values_from = `Intensity (Lux)`)
tmp2 <- pivot_longer(tmp, cols = -Time, names_to = "Date")
tmp2 <- tmp2 %>% filter(!is.na(value))
tmp2$Time <-as.POSIXct(strptime(tmp2$Time, "%H:%M"))
tmp2$date <-as.factor(tmp2$Date)
tmp2 <- tmp2 %>% filter(date == "2018-11-27")
tmp3 <- tmp2

Experiment.lux<-ggplot(tmp2, aes(Time, value, color = "blue"))+
  geom_line()+
  scale_color_manual(name = "", values = c("blue"), labels = c("Intensity (Lux)"))+
  scale_y_continuous(name = "Logger Signal")+
  scale_x_datetime(name = "Time", date_breaks = "3 hours", date_labels = "%H:%M")+
  facet_grid(facets = date ~ .)+
  theme_bw()
Experiment.lux


tmp<-hobo.exp %>% dplyr::select(Date, Time, Temp)
tmp <- pivot_wider(tmp, names_from = c(Date), values_from = Temp)
tmp2 <- pivot_longer(tmp, cols = -Time, names_to = "Date")
tmp2 <- tmp2 %>% filter(!is.na(value))
tmp2$Time <-as.POSIXct(strptime(tmp2$Time, "%H:%M"))
tmp2$date <-as.factor(tmp2$Date)
tmp2 <- tmp2 %>% filter(date == "2018-11-27")

Experiment.temp<-ggplot(tmp2, aes(Time, value, color = "red"))+
  geom_line()+
  geom_vline(xintercept = solar_noon, color = "blue")+
  scale_color_manual(name = "", values = c("red"), labels = c("Temperature"))+
  scale_y_continuous(name = "Logger Signal")+
  scale_x_datetime(name = "Time", date_breaks = "3 hours", date_labels = "%H:%M")+
  facet_grid(facets = date ~ .)+
  theme_bw()
Experiment.temp

tmp5<-full_join(tmp3, tmp4, by = "Time")
color.lux<- "red"
color.PAR <- " black"

ggplot(data = tmp5, aes( x=Time))+
  geom_line(aes(y=value.x), color = color.lux)+
  geom_line(aes(y=value.y*35),linetype = 2, color = color.PAR)+
  scale_y_continuous(name = "Intensity (Lux)", sec.axis = sec_axis(~./35, name = "PAR logger"))+
  guides(linetype = "legend", labels = c("Lux", "PAR"))+
  theme_bw()

