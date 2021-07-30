# by Mike Jankulak (4/29/2020)

# Throw a list of directories at this script where it might find files
# named *_processed.csv (the outputs of processSTR.r by Hannah Barkley).
# This script will step through all those files once to figure out min/max
# timestamps and temperatures for the entire collection, then step through
# a second time to replot Hannah's PNG graphs but this time with consistent
# axes and a label at the top.

# load libraries
library(lubridate)
library(plotly)
library(scales)
library(ggpubr)

# Set environment variables to UTC
Sys.setenv(TZ = 'UTC')

# list of directories (FLKeys / BNP / Broward)
# dirList <- c("~/Documents/aACCRETE/instruments/aRecovery/20191126-keys-strs/day-1",
#              "~/Documents/aACCRETE/instruments/aRecovery/20200422-one-str",
#              "~/Documents/aACCRETE/instruments/aRecovery/20191126-keys-strs/day-2",
#              "~/Documents/aACCRETE/instruments/aRecovery/20200129-middle-keys-str",
#              "~/Documents/aACCRETE/instruments/aRecovery/20191126-keys-strs/day-3",
#              "~/Documents/aACCRETE/instruments/aRecovery/20200109-keys-recovery/STRs",
#              "~/Documents/aACCRETE/instruments/aRecovery/20200113-strs-bisc-bnp-urb/biscayne",
#              "~/Documents/aACCRETE/instruments/aRecovery/20200113-strs-bisc-bnp-urb/broward")

# list of directories (FGB)
# dirList <- c("~/Documents/aACCRETE/instruments/aRecovery/20190603-fgb-work",
#              "~/Documents/aACCRETE/instruments/aRecovery/20200730-fgb-strs")

# list of directories (FLKeys / BNP / Broward)
# dirList <- c("~/Documents/aACCRETE/instruments/aRecovery/20200113-strs-bisc-bnp-urb/urban")

# list of directories (DRTO 2021)
dirList <- c("/Users/LDutra/Documents/R/DRTOFieldreport/DRTOSTRs")

globalMinDate <- globalMaxDate <- NULL
globalMinTemp <- globalMaxTemp <- NULL

# first loop: just determine global axes bounds
for (thisDir in dirList) {
  fileList <- list.files(thisDir)
  processedFiles <- fileList[grep("processed.*\\.csv$", fileList, ignore.case = TRUE)]
  
  for (thisFile in processedFiles) {
    processedData = read.csv(paste(thisDir, thisFile, sep = "/"))
    rows <- nrow(processedData)
    if (rows >= 2) {
      newMinTemp <- min(processedData$Temperature)
      newMaxTemp <- max(processedData$Temperature)
      # remember minimum dates
      if (is.null(globalMinDate)) {
        globalMinDate <- ymd_hms(processedData$UTCDateTime[1])
      } else if (globalMinDate > ymd_hms(processedData$UTCDateTime[1])) {
        globalMinDate <- ymd_hms(processedData$UTCDateTime[1])
      }
      # remember maximum dates
      if (is.null(globalMaxDate)) {
        globalMaxDate <- ymd_hms(processedData$UTCDateTime[rows])
      } else if (globalMaxDate < ymd_hms(processedData$UTCDateTime[rows])) {
        globalMaxDate <- ymd_hms(processedData$UTCDateTime[rows])
      }
      # remember minimum temperatures
      if (is.null(globalMinTemp)) {
        globalMinTemp <- newMinTemp
      } else if (globalMinTemp > newMinTemp) {
        globalMinTemp <- newMinTemp
      }
      # remember maximum temperatures
      if (is.null(globalMaxTemp)) {
        globalMaxTemp <- newMaxTemp
      } else if (globalMaxTemp < newMaxTemp) {
        globalMaxTemp <- newMaxTemp
      }
    }
    # debug: make sure max/min are tracking correctly
    # print(paste("File:", thisFile))
    # print(paste(" this:", ymd_hms(processedData$UTCDateTime[1]), ymd_hms(processedData$UTCDateTime[rows]), newMinTemp, newMaxTemp))
    # print(paste(" glob:", globalMinDate, globalMaxDate, globalMinTemp, globalMaxTemp))
  }
}

# how big should the breaks in the x axis be?
if ((globalMaxDate-globalMinDate) > 365.25) {
  xWidth <- "4 months"
} else {
  xWidth <- "1 month"
}

# second loop: replot data
for (thisDir in dirList) {
  fileList <- list.files(thisDir)
  processedFiles <- fileList[grep("processed.*\\.csv$", fileList, ignore.case = TRUE)]
  
  for (thisFile in processedFiles) {
    plotFile = paste0(sub("processed.*\\.csv$","",thisFile), "labeled.png")
    plotTitle = sub("_[0-9_-]*_processed.*\\.csv$","",thisFile)
    
    # report which plot we're working on
    print(paste("Now processing:", plotTitle))
    
    # read in the data again
    processedData = read.csv(paste(thisDir, thisFile, sep = "/"))
    
    # Hannah's plotting code, adjusted
    plot = ggplot(data = processedData) +
      geom_line(aes(x = ymd_hms(UTCDateTime), y = Temperature), col = 'dodgerblue') +
      theme_bw() +
      theme(plot.margin = unit(c(5.5, 15, 5.5, 5.5), "points")) +
      scale_x_datetime(breaks = breaks_width(xWidth),
                       labels = date_format("%m/%Y"),
                       limits = c(globalMinDate, globalMaxDate)) +
      scale_y_continuous(limits = c(globalMinTemp, globalMaxTemp)) +
      ylab(expression(atop(
        paste("Temperature (", degree, "C)")
      ))) +
      theme(axis.title.x = element_blank())
    
    box = ggplot(data = processedData) +
      geom_boxplot(aes(x = "", y = Temperature), fill = 'dodgerblue') +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())+
      scale_y_continuous(limits = c(globalMinTemp, globalMaxTemp))
    
    comb = ggarrange(plot, box, widths = c(6, 1))
    
    final = annotate_figure(comb, top = plotTitle)
    
    ggsave(
      filename = paste(thisDir, plotFile, sep = "/"),
      plot = final,
      width = 12,
      height = 3
    )
  }
}
