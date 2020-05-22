# Load necessary libraries
library(googlesheets4)

# Read data for lookup table from Google Sheet
# Authorisation needed - run interactively
lookup_tb <- read_sheet("https://docs.google.com/spreadsheets/d/1esITXJxb2ph63sA3nPXmK795PUs34Vsu16YGSIGJ47U/edit?usp=sharing")

# Save as .rda
save(lookup_tb, file = "data/facility_type_lookup.rda" )
