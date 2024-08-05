# international-vpn-access.R

# geolocate the output from AOVPN log files
# andrew.harrison11@nhs.net
# developed for internal use at Dorset HealthCare University NHS Foundation Trust

# SET COMMON VARIABLES ----

# set a working directory on your computer
setwd("C:/b/")

# LOAD LIBRARIES AND FUNCTIONS ----

# Install the pacman package to call all the other packages
if (!require("pacman")) install.packages("pacman")

# Use pacman to install and load all the required packages
pacman::p_load(
  dplyr,
  lubridate,
  tidyverse,
  ggthemes,
  data.table,
  ggtext,
  devtools,
  mapview,
  webshot,
  xlsx
)

#++++++++++++++++++++++++
# Helper function to add titles
#++++++++++++++++++++++++
# - sheet : sheet object to contain the title
# - rowIndex : numeric value indicating the row to 
#contain the title
# - title : the text to use as title
# - titleStyle : style object to use for title
xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

xlsx.addTitle2<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=2)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

# SET VARIABLES ----

# prompt for user input - unused
# filenamesuffix <- readline(prompt="Enter filename suffix: ")

# extract the current date
current_month <- Sys.Date()

# format as a string for naming files
filenamesuffix <- format(current_month, "%Y_%m_%B")

# format as a string for printing
current_month_print <- format(current_month, "%B %Y")

# IMPORT DATA ----

# import remote access statistics export files
# one csv file per always-on VPN node
# add or remove lines as necessary
# the csv files need to be located in your working directory
# df_csv1 <- read.csv(file = "csv1.csv")
df_csv2 <- read.csv(file = "csv2.csv")
df_csv3 <- read.csv(file = "csv3.csv")

# bind these files together into a single table
# arguments for rbind should reflect the variables above
dt_ips <- rbind(df_csv2, df_csv3)

# import the ip2location csv you have downloaded from
#  https://lite.ip2location.com/
# this csv file needs to be located in your working directory
dt_ip2location <- read.csv(file = "IP2LOCATION-LITE-DB9.CSV", header = FALSE)

# the ip2location csv does not come with column headers so we will add these manually
names(dt_ip2location) <- c("IP_FROM", "IP_TO", "COUNTRY_CODE", "COUNTRY_NAME", "STATE_NAME", "LOCALITY_NAME", "LATITUDE", "LONGITUDE", "POSTCODE")

# TRANSFORM DATA ----

# convert these frames to a data table
dt_ips <- as.data.table(dt_ips)
dt_ip2location <- as.data.table(dt_ip2location)

# Function to convert IP address to numeric
# IP addresses in the AOVPN logs are dotted decimal
# IP addresses in the ip2location file are numeric
# therefore we need to convert the IP addresses in the AOVPN logs from
#  dotted decimal to numeric
ip_to_numeric <- function(ip_address) {
  as.numeric(strsplit(ip_address, ".", fixed = TRUE)[[1]]) %*% c(256^3, 256^2, 256^1, 256^0)
}

# Use 'mutate' to add a new column 'Numeric_IP' to the data table
dt_ips <- dt_ips %>% rowwise() %>% mutate(ip = ip_to_numeric(ClientExternalAddress))

# fix the name of the numeric IP address column in the AOVPN logs table
names(dt_ips)[names(dt_ips) == "ip[,1]"] <- "ip"

# convert to data table again as this seems to break
dt_ips <- as.data.table(dt_ips)
dt_ip2location <- as.data.table(dt_ip2location)

# obtain country information using technique at
# https://stackoverflow.com/questions/35672380/optimization-of-converting-ip-address-to-country-in-r
dt_ip2location <- dt_ip2location[, .(COUNTRY_CODE, COUNTRY_NAME, LOCALITY_NAME, IP_FROM, IP_TO, LATITUDE, LONGITUDE)]

dt_ips[, `:=`(IP_FROM = ip, IP_TO = ip)]

setkey(dt_ip2location, IP_FROM, IP_TO)
setkey(dt_ips, IP_FROM, IP_TO)

# use foverlaps to create a new data frame dtm with geolocation against all 
#  AOVPN log entries
dtm <- foverlaps(dt_ips, dt_ip2location)

# from dtm, create a new table for all connections where country code does not 
#  equal GB or "-"
# GB connections are from within the UK
# "-" connections are from within HSCN
# dtm_abroad shows all international AOVPN connections
dtm_abroad <- dtm %>%
  filter(COUNTRY_CODE != "GB" & COUNTRY_CODE != "-") %>%
  subset(select = c("COUNTRY_CODE", "COUNTRY_NAME", "LOCALITY_NAME", "Username", "ConnectionStartTime", "ClientIPAddress", "ClientExternalAddress", "LATITUDE", "LONGITUDE")) %>%
  arrange(COUNTRY_CODE)

dtm_abroad_person <- dtm_abroad %>%
  subset(select = c("Username", "COUNTRY_CODE", "COUNTRY_NAME", "LOCALITY_NAME", "ConnectionStartTime", "ClientIPAddress", "ClientExternalAddress", "LATITUDE", "LONGITUDE")) %>%
  arrange(Username)

# from dtm, create a new table for all connections within the UK
dtm_gb <- dtm %>%
  filter(COUNTRY_CODE == "GB")

# create country summarised group - limited use but could be extended
dtm_group <- dtm_abroad %>% 
  distinct(COUNTRY_NAME, LOCALITY_NAME, Username, .keep_all = TRUE) %>%
  subset(select = c("COUNTRY_NAME", "LOCALITY_NAME", "Username")) %>%
  arrange(COUNTRY_NAME)

# PRESENT AND EXPORT DATA ----

# export to csv - all
write.csv(dtm, file = paste("vpn_all_", filenamesuffix, ".csv" , sep = ""), row.names = FALSE)

# export to csv - international only
write.csv(dtm_abroad, file = paste("vpn_intl_", filenamesuffix, ".csv" , sep = ""), row.names = FALSE)

# export to csv - GB only
write.csv(dtm_gb, file = paste("vpn_gb_", filenamesuffix, ".csv" , sep = ""), row.names = FALSE)

# export international to xlsx
# xlsx::write.xlsx(dtm_abroad, file = paste("vpn_intl_", filenamesuffix, ".xlsx" , sep = ""), sheetName = "By Country", append = FALSE, row.names = FALSE)
# xlsx::write.xlsx(dtm_abroad_person, file = paste("vpn_intl_", filenamesuffix, ".xlsx" , sep = ""), sheetName = "By Person", append = TRUE, row.names = FALSE)

# DO NOT SELECT BELOW THIS LINE UNLESS YOU WANT TO CREATE MAPS ----
# PROCESSOR INTENSIVE

# INTERNATIONAL CONNECTIONS MAP ----
# create a map for international connections
mapviewOptions(fgb = FALSE)
map_abroad <- mapview::mapview(dtm_abroad, xcol = "LONGITUDE", ycol = "LATITUDE", crs = 4326, grid = FALSE)

# draw map
map_abroad
webshot::install_phantomjs()
mapview::mapshot(map_abroad, file = paste("vpn_intl_map_", filenamesuffix, ".png" , sep = ""))

# GB CONNECTIONS MAP ----
# create a map for GB connections - takes a while
map_gb <- mapview::mapview(dtm_gb, xcol = "LONGITUDE", ycol = "LATITUDE", crs = 4326, grid = FALSE)

# draw map - takes a while
map_gb

# EXCEL FILE ----

intl_workbook <- xlsx::createWorkbook(type = "xlsx")

# Title and sub title styles
TITLE_STYLE <- CellStyle(intl_workbook)+ Font(intl_workbook,  heightInPoints=16, 
                                   color="blue", isBold=TRUE, underline=0)

SUB_TITLE_STYLE <- CellStyle(intl_workbook) + 
  Font(intl_workbook,  heightInPoints=12, 
       isItalic=FALSE, isBold=FALSE)

# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE <- CellStyle(intl_workbook) + Font(intl_workbook, isBold=TRUE)
TABLE_COLNAMES_STYLE <- CellStyle(intl_workbook) + Font(intl_workbook, isBold=TRUE) +
  Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
  Border(color="black", position=c("TOP", "BOTTOM"), 
         pen=c("BORDER_THIN", "BORDER_THICK")) 

sheet_front <- xlsx::createSheet(intl_workbook, sheetName = "Cover")
xlsx::addPicture("DHC_logo_small.png", sheet_front, scale = 1, startRow = 2, startColumn = 2)
# Add title
xlsx.addTitle2(sheet_front, rowIndex = 10, title = "International remote worker access report", titleStyle = TITLE_STYLE)
xlsx.addTitle2(sheet_front, rowIndex = 11, title = paste(current_month_print), titleStyle = TITLE_STYLE)
xlsx.addTitle2(sheet_front, rowIndex = 14, title = paste0("Automated report plotted at", Sys.time(), sep = " "), titleStyle = SUB_TITLE_STYLE)
xlsx.addTitle2(sheet_front, rowIndex = 14, title = "Contact point: andrew.harrison11@nhs.net", titleStyle = SUB_TITLE_STYLE)



sheet_by_country <- xlsx::createSheet(intl_workbook, sheetName = "By Country")
xlsx::addDataFrame(dtm_abroad, sheet_by_country, startRow = 3, startColumn = 1, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE, row.names = FALSE)
xlsx::setColumnWidth(sheet_by_country, colIndex = c(1:ncol(dtm_abroad)), colWidth = 30)
# Add title
xlsx.addTitle(sheet_by_country, rowIndex=1, title="Connections by country",
              titleStyle = TITLE_STYLE)

sheet_by_name <- xlsx::createSheet(intl_workbook, sheetName = "By Name")
xlsx::addDataFrame(dtm_abroad_person, sheet_by_name, startRow = 3, startColumn = 1, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE, row.names = FALSE)
xlsx::setColumnWidth(sheet_by_name, colIndex = c(1:ncol(dtm_abroad)), colWidth = 30)
# Add title
xlsx.addTitle(sheet_by_name, rowIndex=1, title="Connections by name",
              titleStyle = TITLE_STYLE)

sheet_map <- xlsx::createSheet(intl_workbook, sheetName = "Map")
xlsx::addPicture(paste("vpn_intl_map_", filenamesuffix, ".png" , sep = ""), sheet_map, scale = 1, startRow = 4, startColumn = 1)
# Add title
xlsx.addTitle(sheet_map, rowIndex=1, title="Connections map",
              titleStyle = TITLE_STYLE)

# save
xlsx::saveWorkbook(intl_workbook, paste("vpn_intl_", filenamesuffix, ".xlsx" , sep = ""))
