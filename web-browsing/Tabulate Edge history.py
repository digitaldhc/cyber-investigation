import pandas as pd
import sqlite3

# Create a SQL connection to our SQLite database
con = sqlite3.connect("c:/b/browser_forensics/History")

df_visits = pd.read_sql_query("SELECT * from visits", con)
df_urls = pd.read_sql_query("SELECT * from urls", con)

# Verify that result of SQL query is stored in the dataframe
print(df_visits.head())

print(df_urls.head())

df_visits['url_visited'] = df_visits['url'].map(df_urls.set_index('id')['url'])

print(df_visits.head())

# Be sure to close the connection
con.close()

df_visits.to_csv("c:/b/browser_forensics/outfile.csv")