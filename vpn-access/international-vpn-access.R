# SET COMMON VARIABLES ----

# set working directory
setwd("C:/b/")

# LOAD LIBRARIES ----

# Install the pacman package to call all the other packages
if (!require("pacman")) install.packages("pacman")

# Use pacman to install (if req) and load required packages
pacman::p_load(
  dplyr,
  lubridate,
  tidyverse,
  ggthemes,
  data.table,
  ggtext,
  devtools
)


# IMPORT DATA ----

# import remote access statistics export files
df_se <- read.csv(file = "sevpn01-July.csv")
df_sr <- read.csv(file = "srvpn01-July.csv")
df_for <- read.csv(file = "forvpn01-July.csv")

# bind these files together into a single table
dt_ips <- rbind(df_se, df_sr, df_for)

# import ip2location csv
DATA <- read.csv(file = "IP2LOCATION-LITE-DB9.CSV")

# TRANSFORM DATA ----

# convert to data table
dt_ips <- as.data.table(dt_ips)
DATA <- as.data.table(DATA)

# Function to convert IP address to numeric
ip_to_numeric <- function(ip_address) {
  as.numeric(strsplit(ip_address, ".", fixed = TRUE)[[1]]) %*% c(256^3, 256^2, 256^1, 256^0)
}

# Use 'mutate' to add a new column 'Numeric_IP' to the data frame
dt_ips <- dt_ips %>% rowwise() %>% mutate(ip = ip_to_numeric(ClientExternalAddress))

# rename columns
names(DATA)[names(DATA) == "X0"] <- "IP_FROM"
names(DATA)[names(DATA) == "X16777215"] <- "IP_TO"
names(DATA)[names(DATA) == "X."] <- "COUNTRY_CODE"
names(DATA)[names(DATA) == "X..1"] <- "COUNTRY_NAME"
names(DATA)[names(DATA) == "X..2"] <- "STATE_NAME"
names(DATA)[names(DATA) == "X..3"] <- "LOCALITY_NAME"

names(dt_ips)[names(dt_ips) == "ip[,1]"] <- "ip"

# convert to data table again as this seems to break
dt_ips <- as.data.table(dt_ips)
DATA <- as.data.table(DATA)

# obtain country information
# https://stackoverflow.com/questions/35672380/optimization-of-converting-ip-address-to-country-in-r
DATA <- DATA[, .(COUNTRY_CODE, LOCALITY_NAME, IP_FROM, IP_TO)]

dt_ips[, `:=`(IP_FROM = ip, IP_TO = ip)]

setkey(DATA, IP_FROM, IP_TO)
setkey(dt_ips, IP_FROM, IP_TO)

dtm <- foverlaps(dt_ips, DATA)

# create a new table for all connections where country code does not equal GB
dtm_abroad <- dtm %>%
  filter(COUNTRY_CODE != "GB")

# export to csv
write.csv(dtm_abroad, file = "vpn_intl_july.csv", row.names = FALSE)
