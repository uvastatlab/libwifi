# libwifi

The final code analyzing the library wifi connection data is in analysis_code and includes:

1. combine_files.R: reads in the many excel files, combines them, and replaces hashed user ids and mac ids with new pseudo-ids
2. wifi_unique_visitors.Rmd: the initial analysis of visitors, without reference to user affiliation
3. wifi_user_affiliations.Rmd: assigning user affiliations and further examination
4. spells.py: to create connections spells -- consecutive half-hour increments in which a user is connected -- for use in analyzing duration of visit, distinct visits
5. spells.R: reads in derived spells data from each library and combines into common file
6. wifi_duration.Rmd: analyzes duration of connections (spells), joins to user affiliation
7. report_jan2020.Rmd: generates background/framing report
8. lib_closed.R: generates a data file that denotes whether a given library is open or closed for each half-hour increment across the year -- still a bit speculative (used in some exploratory analysis we didn't pursue)
