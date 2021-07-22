#Install required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(lme4)) install.packages("lme4", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(knitr)
library(kableExtra)
library(janitor)
library(e1071)
library(lme4)

#Load data
OlympicResults <- 
  read.csv("Data/athlete_events.csv")

#Explore data
head(OlympicResults)

#Filter Summer Olympic Results Only
OlympicSummer <- 
  OlympicResults %>% 
  filter(Season=="Summer")

rm(OlympicResults)

#Records in Summer Olympics
nrow(OlympicSummer)

#Medallists
MedallistsSummer <- 
  OlympicSummer %>% 
  filter(Medal != "NA")

#Percentage of medallists in Summer Olympics (15.3%)
nrow(MedallistsSummer)/nrow(OlympicSummer)

#Gold Medallists
GoldSummer <- 
  OlympicSummer %>% 
  filter(Medal == "Gold")

#Percentage of Gold medallists in Summer Olympics (5.1%)
nrow(GoldSummer)/nrow(OlympicSummer)

#Number of Olympics games (29)
length(unique(OlympicSummer$Games))
       
#Number of unique athletes (116122)
length(unique(OlympicSummer$Name))

#Olympics per person (most is Ian Miller @ 10)
OlympicSummer %>% 
  group_by(Name) %>% 
  summarise(number=n_distinct(Games)) %>%
  arrange(desc(number)) %>% 
  head()


#Number of unique medalists
length(unique(MedallistsSummer$Name))



#split into a few sports - rowing as athletic sport and one other sport (team sport?)
#distribution of height/weight and age in males and females.
#same distribution with medallists



#correlation of height vs. medals won, age vs. medals, region vs. medals, weight vs. medals.


#Olympic Representation====
#Mutate new column with a tally for the number of games attended (inclusive of current games i.e. 1st games = 1)
#group by athlete ID and year and then filter out unique entries (i.e. one row for one athlete at one games), order from old to new
games_rep <- 
  OlympicSummer %>% 
  group_by(ID,Year) %>% 
  distinct(Year) %>% 
  arrange(Year)

#create new column that uses row number within a grouping of athlete ID to label olympic attendance in order
games_rep <- 
  games_rep %>% 
  group_by(ID) %>% 
  mutate(olympic_rep = row_number())

#add this new column back into original data frame
OlympicSummer <- 
  left_join(OlympicSummer,games_rep,by=c("ID","Year"))

#remove transition data frame
rm(games_rep)

#Number of Events====
#Calculate number of events an athlete competes in at each games, more events means more chance for medals.
#group by athlete and games and summarise as total number of events, remove specific event reference so all athlete can be compared
events <- OlympicSummer %>%
  group_by(ID,Year) %>%
  count(name="events")

#add this new column back into original data frame
OlympicSummer <- 
  left_join(OlympicSummer,events,by=c("ID","Year"))

#remove transition data frame
rm(events)

#Medals Won====
#Mutate new column with a tally of number of medals won (prior to current games, i.e. coming into this games you have won how many medals prior)
#First add new column to full data frame to indicate by row if a medal (gold,silver or bronze) was won (1) or not (0)
medals <- 
  OlympicSummer %>% 
  mutate(Medal_Won = if_else(is.na(Medal),0,1))

#group by athlete and year to mutate a sum of total number of medals won
medals <- 
  medals %>% 
  group_by(ID, Year) %>% 
  summarise(medal_count=sum(Medal_Won))

#group by ID and arrange by year to calculate cumulative medal total, then offset (lag)  by one year to calculate medals prior to each event
medals <-
  medals %>%
  group_by(ID) %>%
  arrange(Year) %>%
  mutate(cum_medals = cumsum(medal_count)) %>%
  mutate(prior_medals = lag(cum_medals,n=1,default=0)) %>%
  select(-cum_medals)


#add this new column back into original data frame
OlympicSummer <- 
  left_join(OlympicSummer,medals,by=c("ID","Year"))

#remove transition data frame
rm(medals)


#By Country====
#Success by country, represented by national Olympic committee (NOC)
country <- 
  OlympicSummer %>%
  group_by(ID,Year,NOC,Team) %>%
  summarise(country_medals = sum(medal_count))

#most successful NOC in USA with 9559 medals 
country %>%
  group_by(NOC,Team) %>%
  arrange(desc(country_medals)) %>%
  head()
  
#83 countries yet to win a  medal
country %>%
  filter(country_medals==0) %>%
  nrow()

#1157 different countries/teams listed
length(unique(country$Team))

#Group countries by region/continental associations to reduce degrees of freedom in model
#Possible thAT regions i.e. Europe ARE more successful in certain sports (i.e. Rowing)
#Based on International Olympic Commitee, there are 5 main identified regions (Africa, Amercia, Asia, Europe and Oceania)
#Use IOC resources to assign Teams and Country codes to the regions identified
#https://en.wikipedia.org/wiki/National_Olympic_Committee
#https://en.wikipedia.org/wiki/List_of_IOC_country_codes

Africa <- c("ALG","ANG","BEN","BOT","BUR","BDI","CMR","CPV","CAF","CHA","COM","CGO","COD","CIV","DJI","EGY","GEQ","ERI","SWZ","ETH","GAB","GAM","GHA","GUI","GBS","KEN","LES","LBR","LBA","LIB","MAD","MAW","MLI","MTN","MRI","MAR","MOZ","NAM","NIG","NGR","RHO","RWA","STP","SEN","SEY","SLE","SOM","RSA","SUD","SSD","TAN","TOG","TUN","UGA","ZAM","ZIM")

America <- c("ANT","ARG","ARU","BAH","BAR","BIZ","BER","BOL","BRA","IVB","CAN","CAY","CHI","COL","CRC","CUB","DMA","DOM","ECU","ESA","GRN","GUA","GUY","HAI","HON","JAM","MEX","NCA","NFL","PAN","PAR","PER","PUR","SKN","LCA","VIN","SUR","TTO","USA","URU","VEN","ISV","BWI","WIF")

Asia <- c("AFG","BRN","BAN","BHU","BRU","CAM","CHN","HKG","IND","INA","IRI","IRQ","JPN","JOR","KAZ","PRK","KOR","KUW","KGZ","LAO","LBN","MAS","MAL","MDV","MGL","MYA","NBO","NEP","OMA","PAK","PLE","PHI","QAT","KSA","SGP","SRI","SYR","TPE","TJK","THA","TKM","UAE","UZB","VIE","YEM","YAR","YMD","TLS","MAC","RVN","BOH","UAR","VNM")

Europe <- c("ALB","AND","ARM","AUT","AZE","BLR","BEL","BIH","BUL","CRO","CYP","CZE","DEN","EST","FIN","FRA","GEO","GER","GBR","GRE","CRT","HUN","ISL","IRL","ISR","ITA","KOS","LAT","LIE","LTU","LUX","MLY","MDA","MON","MNE","MLT","NED","MKD","NOR","POL","POR","ROU","RUS","SMR","SRB","SVK","SLO","ESP","SAA","SWE","SUI","TUR","UKR","AHO","EUA","EUN","FRG","GDR","RUI","SCG","TCH","URS","YUG")

Oceania <- c("ASA","AUS","COK","FIJ","GUM","KIR","MHL","FSM","NRU","NZL","PLW","PNG","SAM","SOL","TGA","TUV","VAN","ANZ","AUA")

#Assign region by each listed National Olympic Committee (NOC)
OlympicSummer <- OlympicSummer %>%
  mutate(Region=case_when(NOC %in% Africa ~ "Africa",
                        NOC %in% America ~ "America",
                        NOC %in% Asia ~ "Asia",
                        NOC %in% Europe ~ "Europe",
                        NOC %in% Oceania ~ "Oceania"))

#list and find any remaining n/a's
OlympicSummer %>% filter(is.na(Region))

#Individual olympic athletes, refugee olympic athletes and unknown, group into 'other'

Other <- c("IOA","ROT","UNK")

OlympicSummer <- OlympicSummer %>%
  mutate(Region=case_when(NOC %in% Africa ~ "Africa",
                        NOC %in% America ~ "America",
                        NOC %in% Asia ~ "Asia",
                        NOC %in% Europe ~ "Europe",
                        NOC %in% Oceania ~ "Oceania",
                        NOC %in% Other ~ "Other"))

#confirm no N/A's remaining
OlympicSummer %>% filter(is.na(Region))

#most successful region is Europe with 38980 medals and Africa the least with 834
OlympicSummer %>%  group_by(Region) %>%
  summarise(region_medals = sum(medal_count)) %>%
  arrange(desc(region_medals))

#remove transition data frame
rm(country)

#Tidy Data Frame for Analysis====
#Refine data frame to include 1 entry for each athlete at each games, remove rows not used in analysis.
OlympicSummer <- 
  OlympicSummer %>%
  distinct(ID,Year, .keep_all = TRUE) %>%
  select(-Name,-Team,-NOC,-Games,-Season,-City,-Event)

#Change Medal column to success outcome of Medal_Won (1=yes or 0=no) to simplify outcome variable to medallist vs. placing.
OlympicSummer <- 
  OlympicSummer %>%
  mutate(Medal_Won = if_else(is.na(Medal),0,1)) %>%
  select(-Medal)

#transform into functional classes, numeric and factor (5 levels for region)
sapply(OlympicSummer,class)

table(OlympicSummer$Medal_Won)

table(OlympicSummer$Medal_Won)/nrow(OlympicSummer)

OlympicSummer$Sex <- as.factor(OlympicSummer$Sex)
OlympicSummer$Region <- as.factor(OlympicSummer$Region)
OlympicSummer$Medal_Won <- as.factor(OlympicSummer$Medal_Won)
OlympicSummer$medal_count <- as.integer(OlympicSummer$medal_count)
OlympicSummer$prior_medals <- as.integer(OlympicSummer$prior_medals)

sapply(OlympicSummer,class)
str(OlympicSummer)

#Explore Data====
OlympicSummer %>%
  filter(Sport=="Swimming", Sex=="M") %>%
  ggplot(aes(Height,Medal_Won)) +
  geom_point() +
  geom_smooth()

OlympicSummer %>%
  filter(Sport=="Swimming", Sex=="M") %>%
  ggplot(aes(Weight,Medal_Won)) +
  geom_point() +
  geom_smooth()

OlympicSummer %>%
  filter(Sport=="Swimming", Sex=="M") %>%
  ggplot(aes(Age,Medal_Won)) +
  geom_point() +
  geom_smooth()


cor(OlympicSummer$Medal_Won,OlympicSummer$Weight,use="complete.obs")
cor(OlympicSummer$Medal_Won,OlympicSummer$Height,use="complete.obs")
cor(OlympicSummer$Medal_Won,OlympicSummer$Age,use="complete.obs")
cor(OlympicSummer$Medal_Won,OlympicSummer$prior_medals,use="complete.obs")
cor(OlympicSummer$Medal_Won,OlympicSummer$events,use="complete.obs")
cor(OlympicSummer$Medal_Won,OlympicSummer$olympic_rep,use="complete.obs")


#Split Data into Test and Train====
#Reserve 2016 Olympics for Final Hold out Test
Validation_2016 <- 
  OlympicSummer %>%
  filter(Year=="2016")

#Randomly divide remaining data set (1896-2012) into 20% test and 80% train data set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = OlympicSummer$Medal_Won, times = 1, p = 0.2, list = FALSE)
Olympic_Train <- OlympicSummer[-test_index,]
Olympic_Test <-OlympicSummer[test_index,]

#remove transition data frame
rm(test_index)







#Train Models====
#Logistic Regression====

train_1 <- 
  Olympic_Train %>%
  filter(!is.na(Weight),!is.na(Height))

test_1 <- 
  Olympic_Test %>%
  filter(!is.na(Weight),!is.na(Height))

glm <- 
  train_1 %>%
  filter(Sport=="Swimming") %>%
  glm(Medal_Won ~ Weight + Height, data=., family = "binomial")

glm_predict <- 
  predict(glm, newdata =  test_1, type="response")  

glm_outcome <- factor(if_else(glm_predict >0.5,1,0))

confusionMatrix(glm_outcome, test_1$Medal_Won)$overall["Accuracy"]


#Mixed Effect Logistic Regression====
glmer()

#Classification (Decision) Tree====


#Random Forest====
