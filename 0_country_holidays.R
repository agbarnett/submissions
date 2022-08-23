# 0_country_holidays.R
# get the data on country holidays for 2015 to 2022
# August 2022
library("rjson")
library(dplyr)
years = 2015:2022

## get holiday data
# from https://date.nager.at/Home/Countries; "100 countries are supported"
country.codes = read.table(file = '../working.weekends/data/country.codes.holidays.txt', header=T, stringsAsFactors = FALSE, sep='\t') %>%
  mutate(country = ifelse(country == 'United States', 'United states', country), # change wording to match BMJ data
         country = ifelse(country == 'United Kingdom', 'United kingdom', country), 
         country = ifelse(country == 'South Africa', 'South africa', country),
         country = ifelse(country == 'New Zealand', 'New zealand', country)
  )

# Only progress with countries that are in data
load('data/places.RData')

# Download the holiday data
holidays = NULL
for (c in 1:nrow(country.codes)){ # loop through countries
  for (y in years){ # loop through years
    url = paste("https://date.nager.at/api/v2/PublicHolidays/", y, "/", country.codes$code[c], sep='')
    download.file(url, destfile = 'temp.txt') # download file from net
    json_data = fromJSON(file='temp.txt') # read JSON data
    for (k in 1:length(json_data)){ # loop through holidays
      this = data.frame(t(json_data[[k]])) 
      frame = data.frame(country = country.codes$country[c],
                         date = this$date[[1]],
                         localName = this$localName[[1]],
                         name = this$name[[1]],
                         fixed = this$fixed[[1]],
                         global = this$global[[1]],
                         counties = NA,
                         type = this$type[[1]], stringsAsFactors = FALSE)
      if(is.null(this$counties[[1]]) == FALSE){frame$counties = paste(this$counties[[1]], collapse=',')}
      holidays = bind_rows(holidays, frame)
    }
  }
}

# final formats
holidays = mutate(holidays,
  date = as.Date(date),
  country = ifelse(country == 'United states', 'United States', country), # change wording back !
  country = ifelse(country == 'United kingdom', 'United Kingdom', country), 
  country = ifelse(country == 'South africa', 'South Africa', country),
  country = ifelse(country == 'New zealand', 'New Zealand', country)
)

# save
save(holidays, file='data/holidays.RData')
