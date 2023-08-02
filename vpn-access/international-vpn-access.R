# international-vpn-access.R

# geolocate the output from AOVPN log files
# andrew.harrison11@nhs.net
# developed for internal use at Dorset HealthCare University NHS Foundation Trust

# SET COMMON VARIABLES ----

# set a working directory on your computer
setwd("C:/b/")

# LOAD LIBRARIES ----

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
  mapview
)


# IMPORT DATA ----

# import remote access statistics export files
# one csv file per always-on VPN node
# add or remove lines as necessary
# the csv files need to be located in your working directory
df_csv1 <- read.csv(file = "csv1.csv")
df_csv2 <- read.csv(file = "csv2.csv")
df_csv3 <- read.csv(file = "csv3.csv")

# bind these files together into a single table
# arguments for rbind should reflect the variables above
dt_ips <- rbind(df_csv1, df_csv2, df_csv3)

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
  filter(COUNTRY_CODE != "GB" & COUNTRY_CODE != "-")

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
write.csv(dtm, file = "vpn_all.csv", row.names = FALSE)

# export to csv - international only
write.csv(dtm_abroad, file = "vpn_intl.csv", row.names = FALSE)

# export to csv - GB only
write.csv(dtm_gb, file = "vpn_gb.csv", row.names = FALSE)

# DO NOT SELECT BELOW THIS LINE UNLESS YOU WANT TO CREATE MAPS ----
# PROCESSOR INTENSIVE

# INTERNATIONAL CONNECTIONS MAP ----
# create a map for international connections
map_abroad <- mapview::mapview(dtm_abroad, xcol = "LONGITUDE", ycol = "LATITUDE", crs = 4326, grid = FALSE)

# draw map
map_abroad

# GB CONNECTIONS MAP ----
# create a map for GB connections - takes a while
map_gb <- mapview::mapview(dtm_gb, xcol = "LONGITUDE", ycol = "LATITUDE", crs = 4326, grid = FALSE)

# draw map - takes a while
map_gb
