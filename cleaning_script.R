library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

load_tobacco_data <- function(){
  path = 'original_data/Washington/'
  df <- NULL
  for (f in list.files(path)){
    len <- str_length(f)
    YEAR <- substr(f,len - 7, len - 4 )   # blahblah_YYYY.csv
    target <- paste0(path, f)
    df <- readr::read_csv(target) %>%
      dplyr::mutate(YEAR = YEAR) %>%
      dplyr::select(YEAR, everything() ) %>%
      dplyr::bind_rows(df)
    #    print(df[1,'YEAR'])
  }
  #  print(unique(df$YEAR) )
  return(df)
}


replace_percent <- function(x){
  return(as.numeric(str_replace(x, '%', '') ) / 100)
}


df1 <- load_tobacco_data()
df2 <- readr::read_csv('original_data/tobacco.csv')
df3 <- readr::read_csv('original_data/Youth_Tobacco_Survey__YTS__Data.csv')

names(df1) <- str_replace_all(str_replace_all(toupper(names(df1)), ' ', '_'), '[.]', '')
names(df2) <- str_replace_all(str_replace_all(toupper(names(df2)), ' ', '_'), '[.]', '')
names(df3) <- str_replace_all(str_replace_all(toupper(names(df3)), ' ', '_'), '[.]', '')



df1 <- df1 %>%
  filter(SUBPOP == 'County') %>%
#  filter(!grepl('marijuana',tolower(Label) ) ) %>%   # remove marijuana
  filter(grepl('tobacco', tolower(LABEL) ) ) %>%      # only tobacco
  rename(COUNTY = GEOGRAPHY) %>%
  mutate(COUNTY = paste(COUNTY, 'County') )


names(df1) <- str_replace_all(str_replace_all(toupper(names(df1)), ' ', '_'), '[.]', '')
names(df2) <- str_replace_all(str_replace_all(toupper(names(df2)), ' ', '_'), '[.]', '')
names(df3) <- str_replace_all(str_replace_all(toupper(names(df3)), ' ', '_'), '[.]', '')


df_fips <- readr::read_csv('original_data/cPop10.csv') %>%
  filter(SUMLEV == 50) %>%
  select(STATE, COUNTY, STNAME, CTYNAME) %>%
  mutate(STATE = str_pad(STATE, 2, side = 'left', pad = '0'),
         COUNTY = str_pad(COUNTY, 3, side = 'left', pad = '0') ) %>%
  mutate(FIPS = paste0(STATE, COUNTY) ) %>%
  select(- STATE, - COUNTY) %>%
  rename(COUNTY = CTYNAME)


df1 <- df_fips %>%
  filter(STNAME == 'Washington') %>%
  full_join(df1, by = 'COUNTY') %>%
  arrange(COUNTY, YEAR, LABEL)


#replace_percent
df2 <- df2 %>%
  mutate(SMOKE_EVERYDAY = replace_percent(SMOKE_EVERYDAY),
         SMOKE_SOME_DAYS = replace_percent(SMOKE_SOME_DAYS),
         FORMER_SMOKER = replace_percent(FORMER_SMOKER),
         NEVER_SMOKED = replace_percent(NEVER_SMOKED) ) 


readr::write_csv(df1, 'cleaned_data/WA.csv')
readr::write_csv(df2, 'cleaned_data/tobacco_youth.csv')
readr::write_csv(df3, 'cleaned_data/tobacco_consumption.csv')