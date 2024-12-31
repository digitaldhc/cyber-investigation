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
#  ggthemes,
  data.table,
#  ggtext,
  devtools,
  mapview,
  webshot,
  openxlsx2,
  httr
)

# SET VARIABLES ----

# extract the current date
current_month <- Sys.Date()

# format as a string for naming files
filenamesuffix <- format(current_month, "%Y_%m_%d")

# IMPORT VPN DATA ----

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

# ACQUIRE IP2LOCATION DATA ----

# set ip2location zipfile path
# api token is stored as environment variable ip2loc_api
# more about environment variables and secret management
# https://cran.r-project.org/web/packages/httr/vignettes/secrets.html
zip_path <- paste("https://www.ip2location.com/download/?token=", Sys.getenv("ip2loc_api"), "&file=DB9LITECSV", sep = "")

# check temp folder exists
# and create it if not
if (file.exists("./temp")) { 
  } else
  {
    dir.create("./temp")
  }

# set download file path and name for downloaded data
file_name <- "temp/IP2LOCATION-LITE-DB9.ZIP"

# download the zipfile
httr::GET(zip_path, write_disk(file_name, overwrite = TRUE))

# extract the csv file
unzip(file_name, files = "IP2LOCATION-LITE-DB9.CSV", overwrite = TRUE, exdir = "./temp/")

# import the ip2location csv you have downloaded from
#  https://lite.ip2location.com/
dt_ip2location <- read.csv(file = "temp/IP2LOCATION-LITE-DB9.CSV", header = FALSE)

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

# split the date and time in dtm so they are separate
dtm <- dtm %>%
  tidyr::separate_wider_delim(ConnectionStartTime, delim = " ", names = c("ConnectionStartDate", "ConnectionStartTime1"))

# format the date
dtm$ConnectionStartDate <- as.Date(dtm$ConnectionStartDate, "%d/%m/%Y")

# from dtm, create a new table for all connections where country code does not 
#  equal GB or "-"
# GB connections are from within the UK
# "-" connections are from within HSCN
# dtm_abroad shows all international AOVPN connections
dtm_abroad <- dtm %>%
  filter(COUNTRY_CODE != "GB" & COUNTRY_CODE != "-") %>%
  subset(select = c("COUNTRY_CODE", "COUNTRY_NAME", "LOCALITY_NAME", "Username", "ConnectionStartDate", "ConnectionStartTime1", "ClientIPAddress", "ClientExternalAddress", "LATITUDE", "LONGITUDE")) %>%
  arrange(COUNTRY_CODE)

dtm_abroad_person <- dtm_abroad %>%
  subset(select = c("Username", "COUNTRY_CODE", "COUNTRY_NAME", "LOCALITY_NAME", "ConnectionStartDate", "ConnectionStartTime1", "ClientIPAddress", "ClientExternalAddress", "LATITUDE", "LONGITUDE")) %>%
  arrange(Username)

# from dtm, create a new table for all connections within the UK
dtm_gb <- dtm %>%
  filter(COUNTRY_CODE == "GB")

# create country summarised group - limited use but could be extended
dtm_group <- dtm_abroad %>% 
  distinct(COUNTRY_NAME, LOCALITY_NAME, Username, .keep_all = TRUE) %>%
  subset(select = c("COUNTRY_NAME", "LOCALITY_NAME", "Username")) %>%
  arrange(COUNTRY_NAME)

# PRESENT AND EXPORT DATA AS CSV ----

# all commented out for Dorset HealthCare use but can be enabled

# export to csv - all
# write.csv(dtm, file = paste("vpn_all_", filenamesuffix, ".csv" , sep = ""), row.names = FALSE)

# export to csv - international only
# write.csv(dtm_abroad, file = paste("vpn_intl_", filenamesuffix, ".csv" , sep = ""), row.names = FALSE)

# export to csv - GB only
# write.csv(dtm_gb, file = paste("vpn_gb_", filenamesuffix, ".csv" , sep = ""), row.names = FALSE)

# export international to xlsx
# xlsx::write.xlsx(dtm_abroad, file = paste("vpn_intl_", filenamesuffix, ".xlsx" , sep = ""), sheetName = "By Country", append = FALSE, row.names = FALSE)
# xlsx::write.xlsx(dtm_abroad_person, file = paste("vpn_intl_", filenamesuffix, ".xlsx" , sep = ""), sheetName = "By Person", append = TRUE, row.names = FALSE)

# MAPS ----
# CAN BE PROCESSOR INTENSIVE

# INTERNATIONAL CONNECTIONS MAP ----
# create a map for international connections
mapviewOptions(fgb = FALSE)
map_abroad <- mapview::mapview(dtm_abroad, xcol = "LONGITUDE", ycol = "LATITUDE", crs = 4326, grid = FALSE)

# draw map
map_abroad
webshot::install_phantomjs()
mapview::mapshot(map_abroad, file = paste("vpn_intl_map_", filenamesuffix, ".png" , sep = ""))

# create a var with the file name to use later
img_map <- paste("vpn_intl_map_", filenamesuffix, ".png" , sep = "")

# GB CONNECTIONS MAP ----
# create a map for GB connections - takes a while
# and is not all that reliable
# map_gb <- mapview::mapview(dtm_gb, xcol = "LONGITUDE", ycol = "LATITUDE", crs = 4326, grid = FALSE)

# draw map - takes a while
# map_gb

# EXCEL FILE ----

# create the various date strings for the spreadsheet report
date_start <- min(dtm_abroad$ConnectionStartDate)
date_end <- max(dtm_abroad$ConnectionStartDate)
date_start_print <- format(date_start, "%d %B %Y")
date_end_print <- format(date_end, "%d %B %Y")
date_string <- paste(date_start_print, "to", date_end_print, sep = " ")
date_report <- paste("Report generated on", format(Sys.Date(), "%d %B %Y"), sep = " ")



# Using openxlsx2, initialise a workbook
#  and add worksheets
wb <- openxlsx2::wb_workbook()
wb$add_worksheet("Cover")
wb$add_worksheet("By country")
wb$add_worksheet("By name")
wb$add_worksheet("Map")

# Create the cover worksheet
# data
wb$add_image("Cover", file = "DHC_logo_small.png", dims = "B2", width = 5.93, height = 3.32, units = "cm")
wb$add_data("Cover", "International traveller access report", start_col = 2, start_row = 9)
wb$add_data("Cover", date_string, start_col = 2, start_row = 10)
wb$add_data("Cover", date_report, start_col = 2, start_row = 11)
wb$add_data("Cover", "Contact point: andrew.harrison11@nhs.net", start_col = 2, start_row = 13)
# formatting
wb$add_font("Cover", dims = "B9:B10", name = "Arial", color = wb_colour(hex = "900C3F"), size = 14, bold = "double")
wb$add_font("Cover", dims = "B11:B13", name = "Arial", color = wb_colour(hex = "900C3F"), size = 11, bold = "")
wb$add_fill("Cover", dims = "A1:J25", color = wb_colour(hex = "DBFFF9"), pattern = "solid")

# Create the by country worksheet
# data
wb$add_data("By country", "Connections by country", start_col = 1, start_row = 1)
wb$add_data_table("By country", dtm_abroad, start_col = 1, start_row = 3, col_names = TRUE, row_names = FALSE, with_filter = TRUE, total_row = FALSE)
# formatting
wb$set_col_widths("By country", cols = 1:10, widths = "auto")
wb$add_font("By country", dims = "A1", name = "Arial", color = wb_colour(hex = "900C3F"), size = 14, bold = "double")
wb$add_font("By country", dims = "A3:J3", bold = "single", color = wb_colour(hex = "FFFFFF"))
wb$add_fill("By country", dims = "A1:J2", color = wb_colour(hex = "DBFFF9"), pattern = "solid")

# Create the by name worksheet
# data
wb$add_data("By name", "Connections by name", start_col = 1, start_row = 1)
wb$add_data_table("By name", dtm_abroad_person, start_col = 1, start_row = 3, col_names = TRUE, row_names = FALSE, with_filter = TRUE, total_row = FALSE)
# formatting
wb$set_col_widths("By name", cols = 1:10, widths = "auto")
wb$add_font("By name", dims = "A1", name = "Arial", color = wb_colour(hex = "900C3F"), size = 14, bold = "double")
wb$add_font("By name", dims = "A3:J3", bold = "single", color = wb_colour(hex = "FFFFFF"))
wb$add_fill("By name", dims = "A1:J2", color = wb_colour(hex = "DBFFF9"), pattern = "solid")

# Create the map worksheet
# data
wb$add_data("Map", "Map of connections", start_col = 1, start_row = 1)
wb$add_image("Map", file = img_map, dims = "A3", width = 26.24, height = 20.11, units = "cm")
# formatting
wb$add_font("Map", dims = "A1", name = "Arial", color = wb_colour(hex = "900C3F"), size = 14, bold = "double")
wb$add_fill("Map", dims = "A1:P42", color = wb_colour(hex = "DBFFF9"), pattern = "solid")

# opens the workbook in Excel
# needed for testing only
# wb$open()

# save the workbook
wb$save(file = paste("International_travellers_", filenamesuffix, ".xlsx" , sep = ""))
