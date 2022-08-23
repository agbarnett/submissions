# 1_prepare_data.R
# read the JPC submission data and prepare it for analysis
# August 2022
library(readxl)
library(tidyr)
library(janitor)
library(dplyr)
library(lubridate) # for force_tz and with_tz
source('../working.weekends/0_address_edits.R') # for common edits of address data

# date WHO declared worldwide pandemic
pandemic_date = as.Date('2020-03-11')
pandemic_yrmon = 2020 + season::yrfraction(pandemic_date)

# start smallset jpc

## get the data from Excel, covers the dates 2015-01-02 to 2022-08-01
# 15,558 rows
jpc = read_excel("data/Data_JPC.xlsx", col_names=TRUE) %>%
  rename(
    'city' = 'Author City',
    'state' = 'Author State/Province',
    'country' = 'Author Country/Region',
    'datetime' = "Submission Date") %>% # 
  clean_names() %>%
  filter(!is.na(manuscript_id)) %>%
  separate(col = manuscript_id, sep='\\.', into=c('id','version'), remove=FALSE) %>% # get version from ID
  mutate(
    version = str_remove(version, pattern='R'),
    version = as.numeric(version)+1,
    version = ifelse(is.na(version), 1, version),
    datetime = force_tz(datetime, tzone = 'EST'),# change to NA # use force_tz because we only want to change timezone, not the hours and minutes
    decision = case_when( # combine categories
      str_detect(decision, 'Amend') ~ 'Revise',
      str_detect(decision, 'Reject') ~ 'Reject',
      decision=='' | is.na(decision) ~ "Awaiting",
      TRUE ~ as.character(decision)
    )) %>% 
  select(-id, -submission_month, -author_type_corresponding_author, -author_postal_code) %>% # unlikely to use these variables
  unique() # remove duplicates where all rows are the same
# numbers for CONSORT flow chart
n.submitted = nrow(jpc)

# snap jpc

# exclude missing address
jpc = filter(jpc, !is.na(country))
n.missing.address = nrow(jpc)

# common clean up of address data:
jpc = address.edits(jpc)

# exclude missing date/time
jpc = filter(jpc, !is.na(datetime))
n.missing.date = nrow(jpc)

# snap jpc

## merge with geolocation data
# load address data
load('data/places.RData') # from 0_address_data.R
# merge geocoded places back with jpc data, exclude papers that could not be geocoded
jpc = left_join(jpc, places, by = c('country', 'state', 'city')) %>% 
  filter(is.na(lat) == FALSE) %>% # remove those that could not be geocoded
  mutate(country = ifelse(country=='United kingdom', 'United Kingdom', country), # fix few missing capital letters
         country = ifelse(country=='United states', 'United States', country),
         country = ifelse(country=='Republic of korea', 'South Korea', country),
         country = ifelse(country=='South africa', 'South Africa', country),
         country = ifelse(country=='New zealand', 'New Zealand', country),
         country = ifelse(country=='Hong kong', 'Hong Kong', country))
n.after.geomatch = nrow(jpc)

# snap jpc

# check differences between country in jpc system and Google country
check = filter(jpc, tolower(country) != tolower(countryName),
               str_detect(address, pattern='Hong kong') == FALSE,
               str_detect(countryName, pattern='Hong Kong') == FALSE)  # ignore differences in China/Hong Kong
table(check$country) # almost all discrepencies are for UK, and google data is more believable
View(select(check, country, countryName))
# only bad matches are for United States; other bad matches are small mismatches in presentation
not.wrong = filter(jpc, 
                   (country == countryName) |
                   (country != countryName & country !='United States')
                   ) %>%
  select(-countryName)
jpc = not.wrong
n.post.exclude.differing.country = nrow(jpc) # 

## change date/time to local timezone
# had to do in loops per time zones (around 100)
empty = NULL
for (tz in unique(jpc$timezoneId)) {
  one.tz = filter(jpc, timezoneId == tz) %>%
    mutate(
      local.datetime = with_tz(datetime, tzone = timezoneId[1]), # change the time zone, which also changes the time ...
      local.date = as.Date(local.datetime),  # ... now calculate local dates, hours, etc (can't do below because time zone is changed when data are pasted together)
      local.hour = as.numeric(format(local.datetime, '%H')) + as.numeric(format(local.datetime, '%M')) / 60   # local hours and minutes (not bothered with seconds)
    ) %>% #
    select(-local.datetime, -datetime) # drop local data time because it gets changed when data are concatenated with other time zones
  empty = bind_rows(empty, one.tz) # concatenate
}

## add country-specific weekend, see here: https://en.wikipedia.org/wiki/Workweek_and_weekend
# Malaysia varies by region, kept with Sat/Sun weekend
jpc = mutate(empty, 
                weekday = format(local.date, '%u'),
                weekend = weekday %in% c(6,7), # Sat/Sun
                weekend = ifelse(country %in% c('Afghanistan','Algeria','Bahrain','Bangladesh','Egypt','Iraq','Israel','Jordon','Kuwait','Libya','Maldives','Oman','Qatar','Saudi Arabia','Sudan','Syria','UAE','Yemen'), weekday %in% c(5,6), weekend), # Friday to Saturday
                weekend = ifelse(country %in% c('Iran','Somalia'), weekday==5, weekend), # Fri only
                weekend = ifelse(country %in% c('India','Hong Kong','Mexico','Uganda'), weekday==7, weekend), # Sun only
    weekend = factor(as.numeric(weekend), levels = 0:1, labels = c('Weekday', 'Weekend'))) %>%
    dplyr::select(-weekday)

# snap jpc

# remove rows with same manuscript ID and timezone
jpc = group_by(jpc, manuscript_id, timezoneId) %>%
  slice(1) %>%
  ungroup()
n.after.duplicates = nrow(jpc)

# remove both entries if there's a different time zone for the same manuscript ID
dups = get_dupes(jpc, manuscript_id) %>% 
  select(manuscript_id) %>%
  unique()
jpc = anti_join(jpc, dups, by='manuscript_id')
n.after.double.timezones = nrow(jpc)

# snap jpc

# add pandemic
jpc = mutate(jpc,
             pandemic = as.numeric(local.date > pandemic_date),
             pandemic = factor(pandemic, levels=0:1, labels=c('Pre-pandemic','Post-pandemic'))
)

## add public holidays
load('data/holidays.RData') # from 0_country_holidays.R
holidays = filter(holidays, is.na(counties)==TRUE) %>% # just national holidays, so nothing in counties
  select(country, date) %>%
  mutate(holiday=1) %>%
  unique() # remove duplicates
jpc = left_join(jpc, holidays, by=c('country'='country', 'local.date'='date')) %>% # could also merge by state, but need to reformat all states 
  mutate(holiday = ifelse(is.na(holiday)==TRUE, 0, holiday),
         holiday = factor(holiday, levels = 0:1, labels = c('No', 'Yes'))) %>%
  select(-state, -gmtOffset) # don't need these variables

## add windows for weeks for regression analysis
windows = data.frame(local.date = seq(min(jpc$local.date), max(jpc$local.date), 1)) %>% # range of observed dates
  mutate(monday = weekdays(local.date) == 'Monday', # new windows start on Monday
         window = cumsum(monday)) %>%
  filter(window > 0, window < max(window)) %>% # remove first and last windows that do not contain full weeks
  select(-monday) # no longer needed
jpc = left_join(windows, jpc, by='local.date') %>% # lose a few because of windows
  filter(!is.na(lat)) # windows with no papers
n.after.windows = nrow(jpc)

# end smallset jpc

### set up the data in counts per week for the regression models ###
# first create dates from windows for seasonal analysis (used below)
for.merge = data.frame(window = seq(1, max(jpc$window), 1)) %>%
  mutate(date = as.Date('2015-01-02')+(window*7)) # start on Jan 2nd
# weekly counts for weekends
jpc.weekend = arrange(jpc, local.date) %>%
  group_by(window, country, weekend) %>%
  summarise(count = n()) %>%
  tidyr::spread(key=weekend, value=count) %>% # put weekends and weekdays on same row
  mutate(Weekday = ifelse(is.na(Weekday), 0, Weekday), # replace missing with zero
         Weekend = ifelse(is.na(Weekend), 0, Weekend),
         denom = Weekday + Weekend) 
jpc.weekend = left_join(jpc.weekend, for.merge, by='window')

## Save
jpc.numbers = data.frame(n.jpc = n.submitted,
                         n.missing.address = n.missing.address,
                         n.missing.date = n.missing.date,
                     n.after.geomatch = n.after.geomatch,
                     n.post.exclude.differing.country = n.post.exclude.differing.country,
                     n.after.duplicates = n.after.duplicates,
                     n.after.double.timezones = n.after.double.timezones, 
                     n.after.windows = n.after.windows)
save(jpc, jpc.weekend, jpc.numbers, 
     pandemic_date, pandemic_yrmon,
     file='data/jpcAnalysisReady.RData')

# version for sharing on github
#reviewer = dplyr::select(reviewer, local.date, local.hour, window, country, journal, timezoneId, weekend, holiday)
#save(reviewer, file='data/jpcReviewers.RData')


