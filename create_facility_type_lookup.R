# Load necessary libraries
library(googlesheets4)

# Read data for lookup table from Google Sheet
# Authorisation needed - run interactively
lookup_tb <- read_sheet("https://docs.google.com/spreadsheets/d/1esITXJxb2ph63sA3nPXmK795PUs34Vsu16YGSIGJ47U/edit?usp=sharing")

# Save as .rda
save(lookup_tb, file = "data/facility_type_lookup.rda" )

#initial attempt at lookup with shortnames
library(afrihealthsites)

fts <- lookup_tb

names(fts) <- c('country','type_who','type_new')


sftogo <- afrihealthsites::afrihealthsites('togo', datasource = 'who', plot=FALSE)

#sftogo$`Facility type`

#match returns a vector of the positions of (first) matches of its first argument in its second.

#cool this works
sftogo$facility_type <- fts$type_new[ match(sftogo$`Facility type`,fts$type_who) ]

#test for whole dataset just to check it doesn't take ages
sfall <- afrihealthsites::afrihealthsites('all', datasource = 'who', plot=FALSE)

#it's super quick :-)
sfall$facility_type <- fts$type_new[ match(sfall$`Facility type`,fts$type_who) ]

#tomorrow work out how best to put this in afrihealthsites
#can add the column to the data stored df_who_sites 


