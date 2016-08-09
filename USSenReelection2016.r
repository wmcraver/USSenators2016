library(rvest)
library(stringr)
library(lubridate)
library(dplyr)

url <-
  "https://en.wikipedia.org/wiki/List_of_current_United_States_Senators"
senators <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/table[6]') %>%
  html_table()
senators <- as.data.frame((senators))

# Sanders had a footnote ("[1]") by his party name, so I extracted the text up to the footnote for all senators
senators$Party = str_extract(senators$Party, "([A-Z][a-z]+)")

# Remove unnecessary columns from dataset
senators = senators[, c(-1,-4)]

# Change the class to Factor
senators$State = as.factor(senators$State)
senators$Party = as.factor(senators$Party)
senators$Class = as.factor(senators$Class)
senators$Seat.Up = as.factor(senators$Seat.Up)

# Convert class to date
senators$Assumed.Office = mdy(senators$Assumed.Office)

# Extract name using regex.  Names prior to this looked like: "Lastname, FirstnameFirstName Lastname"
senators$Name = str_extract(senators$Name, "(\\w+,\\s+)([A-Z][a-z]+)")

# Extract birth date from column using regex
senators$Born = ymd(str_extract(senators$Born, "\\d+-\\d+-\\d+"))

# Calculate the number of years in office for each senator
senators$YearsInOffice = trunc(((today() - senators$Assumed.Office) / 365))
senators$YearsInOffice = as.numeric(senators$YearsInOffice)

# Calculate the age of each senator
senators$Age = trunc(((today() - senators$Born) / 365))
senators$Age = as.numeric(senators$Age)

# Calculate the senator's age when they were elected into office
senators$AgeWhenAssumedOffice = trunc(((senators$Assumed.Office - senators$Born) / 365))
senators$AgeWhenAssumedOffice = as.numeric(senators$AgeWhenAssumedOffice)

# Summarize the data ------------------------------------------------------
# What senators are up for election in 2016
seatUp =senators %>% select(State, Name, Party, Assumed.Office, Age, YearsInOffice, Seat.Up) %>% filter(Seat.Up == "2016") %>% arrange(State, Name)

# How many senators have seats that are up for reelection in 2016 by Party
seatUpParty = senators %>% select(Party, Seat.Up) %>% filter(Seat.Up == "2016") %>%  group_by(Party) %>% summarise(n = n())

seatUpState = senators %>% select(State, Seat.Up) %>% filter(Seat.Up == "2016") %>% group_by(State) %>% summarise(n = n())

avgAgeServiceParty = senators %>%
  select(Party, Age, YearsInOffice, Seat.Up) %>%
  filter(Seat.Up == "2016") %>% 
  group_by(Party) %>%
  summarise(
  AvgAge = round(mean(Age), digits = 0),
  AvgYearsInOffice = round(mean(YearsInOffice), digits = 0),
  n = n())

