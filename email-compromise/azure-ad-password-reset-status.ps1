# azure-ad-password-reset-status.ps1
# ----------------------------------
# ......|.........|.........|.........|.........|.........|.........|.........|
# This script takes a CSV file containing email addresses and obtains for each
# one the LastPasswordChangeTimeStamp from Azure Active Directory. This helps
# us determine when individuals potentially implicated in phishing or other
# email compromise changed their passwords.

# Set the path to the CSV file that will be used by the script
# The file must have a column of email addresses named "email" to work
$filepath = "C:\b\email106.csv"

# Initiate a connection to Azure Active Directory - this is interactive and
# you will be prompted to authenticate
connect-msolservice

# Import the CSV file to a variable named $tempcsv
$tempcsv = Import-Csv $filepath

# Step through each line in the file
$tempcsv |
ForEach-Object {

# $email = $_.email

# Get the value of LastPasswordChangeTimeStamp from AAD and place it in a 
# variable called $reset_status
 $reset_status = get-msoluser -userprincipalname $_.email |Select-Object LastPasswordChangeTimeStamp

# Write the retrieved time stamp to a new column called LastPasswordChangeTimeStamp
  Add-Member -InputObject $_ -MemberType NoteProperty -Name LastPasswordChangeTimeStamp -Value $reset_status.LastPasswordChangeTimeStamp -PassThru

 } |

 # Output the data to the file
Export-CSV -NoTypeInformation -Path $filepath