library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggthemes)
library(stringr)



options(scipen=999) #Add this code for removal of sci notation
datacomb <- read_csv("COMBINED copy.csv") #Upload File
#############DATA CLEANING###################

#Turn Month Column into a Two Digit.  Add 0 padding

datacomb$MONTH <- sprintf("%02d", as.numeric(datacomb$MONTH))

#Combine Month and Date Columns

data1 <- unite(datacomb, "date", c("YEAR", "MONTH"), sep = "-")

#Put in Date Format

data1$date <- ymd(paste(data1$date, "-01"))

#rename column to remove space for easy filtering
data1$PART <- data1$`PART #`
data1$PART <- as.numeric(data1$PART)
data1$BRANCH <- data1$PART
data1$DATE <- data1$date



###Filter Datatable
data1 <- data1[,c(11,12,2,9)]
data1 <- data1[!is.na(data1),]
data1 <- data1%>%
  filter(BRANCH != 0)

#Add ID
UID <- 1:6676
data1 <- cbind(UID, data1)

#Distinct Data
distinctlineitems <- data1%>%
  distinct(REMARKS, .keep_all = TRUE)


distinctlineitems <- distinctlineitems[,c(1,2,4)]

distinctlineitems[distinctlineitems$UID == 1,]
data1[data1$UID == 1,]



############CSV DOWNLOAD CODE##############
csvDownloadUI <- function(id, label = "Download CSV") {
  ns <- NS(id)

  downloadButton(ns("download"), label)
}


csvDownload <- function(input, output, session, data,
                        filename = paste0("AGG_TOTALS", Sys.Date(), ".csv")) {

  output$download <- downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
}

