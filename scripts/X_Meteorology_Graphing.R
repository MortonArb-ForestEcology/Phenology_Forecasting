# -------------------------------------
# Doing some quick graphing
# -------------------------------------
day.labels <- data.frame(Date=seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
summary(day.labels)

if(Sys.Date()<=as.Date(paste0(lubridate::year(Sys.Date()), "-06-20"))){
  dat.ghcn$threshA <- dat.ghcn$GDD0.cum 
  dat.ghcn$threshB <- dat.ghcn$GDD5.cum 
} else {
  dat.ghcn$threshA <- dat.ghcn$CDD0.cum 
  dat.ghcn$threshB <- dat.ghcn$CDD2.cum 
}

plot.threshA <- ggplot(data=dat.ghcn, aes(x=YDAY, y=threshA)) +
  stat_summary(fun.y=mean, color="black", geom="line", size=1) +
  geom_line(aes(group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(color=as.factor(lubridate::year(Sys.Date()))), size=2) +
  scale_color_manual(name="Year", values = "blue2") +
  theme_bw() +
  guides(color=F) +
  theme(legend.position = c(0.1, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

plot.threshB <- ggplot(data=dat.ghcn, aes(x=YDAY, y=threshB)) +
  stat_summary(fun.y=mean, color="black", geom="line", size=1) +
  geom_line(aes(group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(color=as.factor(lubridate::year(Sys.Date()))), size=2) +
  scale_color_manual(name="Year", values = "blue2") +
  theme_bw() +
  guides(color=F) +
  theme(legend.position = c(0.1, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

if(Sys.Date()<=as.Date(paste0(lubridate::year(Sys.Date()), "-06-20"))) {
  plot.threshA <- plot.threshA + 
    scale_y_continuous(name="Cum. GrowDD, base 0 C" ,expand=c(0,0), limits=c(0, max(dat.ghcn$threshA[dat.ghcn$YDAY<=180], na.rm=T))) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(1, 180), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)]) 
  
  plot.threshB <- plot.threshB + 
    scale_y_continuous(name="Cum. GrowDD, base 5 C" ,expand=c(0,0), limits=c(0, max(dat.ghcn$threshB[dat.ghcn$YDAY<=180], na.rm=T))) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(1, 180), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)]) 
} else {
  plot.threshA <- plot.threshA + 
    scale_y_continuous(name="Cum. ChillDD, base 0 C" ,expand=c(0,0), limits=c(0, max(dat.ghcn$threshA[dat.ghcn$YDAY>=155], na.rm=T))) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(155, 366), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)]) 
  
  plot.threshB <- plot.threshB + 
    scale_y_continuous(name="Cum. ChillDD, base -2 C" ,expand=c(0,0), limits=c(0, max(dat.ghcn$threshB[dat.ghcn$YDAY>=155], na.rm=T))) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(155, 366), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)]) 
}  

plot.prcp <- ggplot(data=dat.ghcn, aes(x=YDAY, y=PRCP.cum)) +
  stat_summary(fun.y=mean, color="black", geom="line", size=1) +
  geom_line(aes(group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(color=as.factor(lubridate::year(Sys.Date()))), size=2) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)]) +
  scale_y_continuous(name="Cum. Precip (mm)" ,expand=c(0,0)) +
  scale_color_manual(name="Year", values = "blue2") +
  theme_bw() +
  guides(color=F) +
  theme(legend.position = c(0.1, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

plot.tmean <- ggplot(data=dat.ghcn, aes(x=YDAY, y=TMEAN)) +
  stat_summary(fun.y=mean, color="black", geom="line", size=1) +
  geom_line(aes(group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(color=as.factor(lubridate::year(Sys.Date()))), size=1) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)]) +
  scale_y_continuous(name="Mean Daily Temp (C)" ,expand=c(0,0)) +
  scale_color_manual(name="Year", values = "blue2") +
  theme_bw() +
  guides(color=F) +
  theme(legend.position = c(0.1, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

plot.dat <- cowplot::plot_grid(plot.tmean, plot.prcp, plot.threshA, plot.threshB)

library(cowplot)
plot.title <- ggdraw() + 
  draw_label(paste("The Morton Arboretum weather, last updated:", max(dat.ghcn$DATE)),
             fontface = 'bold', x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 1)
  )


png(file.path(path.out, "figures", "Weather_latest.png"), height=6, width=6, units="in", res=220)
cowplot::plot_grid(plot.title, plot.dat,ncol=1, rel_heights = c(0.1, 1))
dev.off()
# -------------------------------------
