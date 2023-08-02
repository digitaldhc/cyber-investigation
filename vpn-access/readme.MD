# Geolocate your AOVPN logs using R

This technique uses PowerShell to extract access logs from your Microsoft Always-On VPN nodes, and an R script to merge them together and compare against an external IP address data file to help you geolocate your remote access.

# Prerequisites

Before you start you will need the following:

-   R and RStudio;

-   access to run PowerShell commands against your AOVPN nodes;

-   the IP2location csv data file;

-   the R script in this repository.

## R and RStudio

R is a programming language statistical computing and data mining. It is augmented by a large number of extensions packages and is freely available on Windows, MacOS and Linux.The easiest way to get going with R is to download R and RStudio from <https://www.rstudio.com/products/rstudio/download/#download> or request it from your IT team.

## PowerShell access

You must have access to run PowerShell commands (namely `Get-RemoteAccessConnectionStatistics`) against your AOVPN nodes. A command that will produce the correct output is reproduced below. You must also set `$startdate` and `$enddate` variables to limit the query as follows:

`$startdate = Get-Date -Date "01/06/2023"`

`$enddate = Get-Date -Date "26/07/2023"`

`Get-RemoteAccessConnectionStatistics -ComputerName **hostname.fqdn** -StartDateTime $startdate -EndDateTime $enddate | Select-Object -Property ClientIPAddress, ClientExternalAddress, ConnectionStartTime, @{n='Username';e={($_.Username|Out-String).trim()}}, TunnelType | export-csv csv1.csv -NoTypeInformation`

You should run this command against each AOVPN node, creating a separate csv file for each one.

## IP2location csv data file

To obtain the IP address geolocation you will need to register for a free account at <https://lite.ip2location.com> and download the DB9.LITE IP-COUNTRY-REGION-CITY-LATITUDE-LONGITUDE-ZIPCODE csv file - this will be provided in the form of a zip archive. Once unzipped you should see that the file is named `IP2LOCATION-LITE-DB9.CSV`.

## R script

The R script [international-vpn-access.R](https://github.com/digitaldhc/cyber-investigation/blob/main/vpn-access/international-vpn-access.R) needs to be downloaded to your computer, either by [cloning this repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) or by downloading the file directly from GitHub.

# Running the script

First, ensure your AOVPN csv files and IP2LOCATION-LITE-DB9.CSV are in the same directory on your computer. This will be your working directory.

Load RStudio and open international-vpn-access.R.

The first command in the file is `setwd("C:/b/")` - change this path to that of your working directory. Note that even on Windows, R will expect to see forward slashes in directory paths.

Now scroll down to the IMPORT DATA section and look for the line `df_csv1 <- read.csv(file = "csv1.csv")` . Ensure there is a line for each AOVPN csv file you have to import. If you have fewer than three files, delete the lines for csv files that do not exist.

Now look at the next section, where we use the `rbind` command to merge the dataframe we will create for each csv file. Ensure the arguments for the `rbind` command match the dataframes that you will create. If you only have one csv file to import you can ignore this command and comment it out.

Now select all the lines in the script up to the line `# DO NOT SELECT BELOW THIS LINE UNLESS YOU WANT TO CREATE MAPS ----` . The maps can take a long time to build and you may not require them.

Click the Run button at the top right of the code window. The code will execute in the console window of RStudio. Depending on your computer and the amount of data involved this may take a short while - on my computer and with my data it takes approximately one minute to run.

# Output

If all goes well, you will have three new csv files in your working directory once the script completes:

-   vpn_all.csv showing geo-location for all VPN connections

-   vpn_intl.csv showing geo-location for international connections only

-   vpn_gb.csv showing geo-location for UK connections only

If you wish to include or exclude other countries this is reasonably easy to achieve using the relevant filter commands. All csv files can of course be opened in Excel or another spreadsheet for further processing.

# Maps

if you wish, you can select the commands to create either an international or a GB map. These will be displayed in the **viewer** pane in RStudio where you can move around the map or zoom in or out. Click the Export button at the top of the viewer pane to save your map as an image file.

Note - the mapping can be quite processor intensive

# A caveat about IP geolocation

While IP geolocation can be very accurate, the various things ISPs do on their networks can introduce some complications that will affect the way you can use this data. As an example, geolocation has previously shown me to be in both Cambridge and Warrington on days when I worked at home - many, many miles from both these places! I can only put this down to the internal routeings and NAT-ing of my ISP although there may be other sundry reasons.

# Questions, comments and issues

Please [raise an issue](https://github.com/digitaldhc/cyber-investigation/issues) or send me an email - andrew (dot) harrison11 (at) nhs (dot) net
