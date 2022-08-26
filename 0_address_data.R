# 0_address_data.R
# get all the paper and reviewer data and assign a timezone based on the geolocated address
# adapted from previous working weekends code
# August 2022
library(smallsets) # for data management diagram
library(janitor)
library(readxl)
library(dplyr)
library(forcats)
library(ggmap) # for geocoding cities
library(geonames) # for turning lat and long into timezones
source('../working.weekends/98_google_API.R') # register API with google (NOT SHARED ON GITHUB)
source('../working.weekends/98_paste_without_NA.R')
source('../working.weekends/0_address_edits.R') # for common edits of address data


# start smallset mydata

## a) get the submission data
data = read_excel('data/Data_JPC.xlsx') %>%
    rename(
      'city' = 'Author City',
      'state' = 'Author State/Province',
      'country' = 'Author Country/Region') %>%
  clean_names() %>%
  select(-author_type_corresponding_author) # not needed

# remove one empty row and 7 with missing country
data = filter(data, 
              !is.na(submission_date),
              !is.na(country))
  
# common clean up of address data:
data = address.edits(data)

## geolocate all places - takes a while
# create geo data
exists = dir('data', pattern='places.RData') > 0
if(exists == FALSE){
  places = select(data, city, state, country) %>%
    distinct() %>% # just unique places
    mutate(address = paste3(city, ', ', state, ', ', country, sep=''),
         address = stringr::str_replace(address, pattern='^, ', replacement = ''), # replace starting comma twice
         address = stringr::str_replace(address, pattern='^, ', replacement = ''),
         address = stringr::str_replace(address, pattern=', , ', replacement = ', '))
  codes = mutate(geocode(places$address)) # takes a while
  places = bind_cols(places, codes) # add lat and long back to places
  save(places, file = 'data/places.RData') # saved because repeated database use will cost money
}
if(exists == TRUE){
  load('data/places.RData')
}

# now find time zone for every place based on lat and long
# exclude places with missing lon/lat
missing.lat.long = sum(is.na(places$lat))
places = filter(places, is.na(lat) == FALSE)
places$timezoneId = places$countryName = ''
places$gmtOffset = NA
for (i in 1:nrow(places)) { # loop, takes a long while
  # loop through each place
  res = GNtimezone(lat = places$lat[i],
                   lng = places$lon[i],
                   radius = 0) %>%
    select(gmtOffset, timezoneId, countryName)
  places$gmtOffset[i] = res$gmtOffset
  places$timezoneId[i] = as.character(res$timezoneId)
  places$countryName[i] = as.character(res$countryName) # used as a check
  Sys.sleep(9) # wait for this many secs
  if(i%%500 == 0){cat('Up to ',i,'\n', append=TRUE, sep='')}
}

# Save for use by 1_?_data.R
save(places, file = 'data/places.RData') # 

# end smallset mydata
