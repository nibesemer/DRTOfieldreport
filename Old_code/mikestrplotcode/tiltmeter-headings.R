# playing with openair package
library(RColorBrewer)
library(scales)
library(lubridate)
library(openair)
library(magick)
library(ggplot2)

fileAny <- "~/Desktop/DRTO Data Entry/2102061_DRTO-2021-06_(0)_Current.csv"
pH <- read_csv('Data/2102061_DRTO-2021-06_(0)_Current.csv',col_types = cols())
# hardcoded limits for this DRTO file are
# ymd_hms("2021/06/25 18:30:00") and ymd_hms("2021/06/28 19:45:00")
# (future enhancement: incorporate Hannah Barkley's dataset trimming code;
#  also, eliminate all the hardcoding below and calculate from data)

dataAny <- read_csv('Data/2102061_DRTO-2021-06_(0)_Current.csv',col_types = cols())
dataAny$date = ymd_hms(paste0(dataAny$Date, dataAny$Time), tz = "UTC")
dataAny <- dataAny[, c("date","Speed..cm.s.","Heading..degrees.")]
colnames(dataAny) <- c("date","ws","wd")
dataAny$DateTime<-NA
dataAny <- subset(dataAny, date >= ymd_hms("2021/06/25 18:30:00") & date <= ymd_hms("2021/06/28 19:45:00"))
dataAny$ <- lubridate::with_tz(dataAny$date, "America/New_York")


# 2021/07/09 working up more tiltmeter graphs completely in R
# (not just openair's windRose plot, which was suggested by Ian originally)

# line plot (Current Speed)
speedPlot = ggplot(data = dataAny) +
  geom_line(aes(x = DateTime, y = ws), col = 'dodgerblue') +
  theme_bw() +
  theme(plot.margin = unit(c(5.5, 15, 5.5, 5.5), "points")) +
  scale_x_datetime(breaks = date_breaks("8 hours"),
                   labels = date_format("%m/%d\n%H:%M"),
                   limits = c(ymd_hms("2021/06/25 16:00:00"),ymd_hms("2021/06/28 22:00:00")),
                   expand=c(0,0)) +
  ylab(expression(atop("Current Speed (cm/s)"))) +
  xlab(expression(atop("Date and Time (EDT)"))) +
  scale_y_continuous(expand=c(0,0),limits=c(0,12))

speedPlot


# histogram (Current Direction)
dirPlot = ggplot(data = dataAny) +
  geom_histogram(aes(x=wd), color = 'black', fill = 'dodgerblue',
                 binwidth=10, boundary=0) +
  coord_cartesian(xlim = c(0, 360)) +
  scale_x_continuous(breaks=seq(0,360, by=30), expand=c(0,00)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,125)) +
  theme_bw() +
  theme(plot.margin = unit(c(5.5, 15, 5.5, 5.5), "points")) +
  ylab(expression(atop("Count"))) +
  xlab(expression(atop(paste("Current Direction (", degree, ")"))))


dirPlot

#ggarrange(speedPlot, dirPlot, ncol=1, nrow=2)

# save the speed and direction plots to a file
ggsave(
  filename = paste0(dirname(fileAny), "/tilt-plot1-speed.png"),
  plot = speedPlot,
  width = 12, height = 3)
ggsave(
  filename = paste0(dirname(fileAny), "/tilt-plot2-direction.png"),
  plot = dirPlot,
  width = 12, height = 3)

# use the png function to write the windRose plot directly to a file
roseFile=paste0(dirname(fileAny), "/tilt-plot3-rose.png")
png(filename=roseFile,
    width = 8, height = 8, units = "in", res = 144)
rosePlot = windRose(dataAny, ws = "ws", wd = "wd", ws.int=2, cols = brewer_pal(palette="Spectral", direction=-1)(6), breaks=6, paddle=FALSE, key.header="(cm/s)", key.footer="NCRMP - DRTO")
dev.off()

rosePlot


# crop off the "Frequency of counts by wind direction (%)" label
# (which doesn't seem to be optional, in the openair package)
roseCrop=image_read(roseFile) # 1151x1152
roseCrop=image_crop(roseCrop,"11152x1120+0+0")
image_write(roseCrop, roseFile)
