library(readr)
library(mongolite)
library(RSocrata)
library(RJSONIO)

library(dplyr)
library(lubridate)
library(tibble)
library(ggplot2)
library(gganimate)
library(gifski)

# MongoDB and R together workshop.
# 1. Install MongoDB server using MongoDB documentation and not old packages in repo
#   + Version 4.4.1
#   + systemctl start mongod
#   + mongo to enter shell (db.help())
#   + url = "mongodb://127.0.0.1:27017"
# 2. Connect and create MongoDB collection from online data source 
# 3. Import the data back from MongoDB if the data from source is current
# 5. Use gganimate to publish trending bar chart 

# read in state popluations and save to mongo (used for per capita calculations)
# state_abbreviations <- jsonlite::fromJSON(txt = "Data/state_abbreviations.json")
# pop <- read.csv(file = "Data/State_Popluations.csv", header = FALSE)
# 
# state_populations <- pop %>% 
#   select(State = V3, Population = V4) %>% 
#   inner_join(., state_abbreviations, by = "State")
# 
mongo_population <- mongo(db = "covid", collection = "population" )
# population$insert(state_populations)
state_populations <- mongo_population$find()

#read method does not return _id
m <- mongo(db = "covid", collection = "covid" )

#returns all records
covid <- m$find()

#returns n records
# df <- m$find('{}', limit = 10)
#example of filtered query with selected fields
# df <- m$find(query = '{"state": "IN"}')

#another method would be to dump the json file from mongo and read it in with _ids
#json <- fromJSON("covid.json")

#the last record is the date it was last created
df <- m$find(skip = m$count() -1)

#skip if imported already today and read from mongo data instead
if(!df$date == today()) {
  covid <- read.socrata("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv")
  m$drop()
  m$insert(covid)
  #set date of api read
  m$insert(paste0('{"date": "', Sys.time(), '"}'))
} else {
  covid <- m$find(limit = m$count() - 1) #skip last obs which is the date of last socrata import
}

#unique values of field
# m$distinct("submission_date")
# m$distinct("_id")

#commands, probably can only list collections when the collection isn't specifed on the conn? 
#m$run(command = '{"listCollections" : 1.0}') 

#this doesn't list dbs for some reason, not important right now
# admin <- mongo(db = "admin")
# admin$run('{"listDatabases": 1}')
# admin$disconnect()

#m$run('{"count":"indiana"}')

covid1 <- covid %>% 
  mutate(state = if_else(state == "NYC", "NY", state)) %>% 
  inner_join(., state_populations, by = c("state" = "Code")) %>% 
  select(submission_date, state_name = State, state_code = state, new_death, Population)

#Last 30 Days 
covid2 <- covid1  %>% 
  filter(submission_date > today() - 30) %>%
  group_by(state_name) %>% 
  mutate(acc_sum = cumsum(new_death)) %>% 
  mutate(population = parse_number(Population)) %>% 
  mutate(Deaths = acc_sum / (population/1000000)) %>% 
  ungroup()  
  

# Weekly
# c <- covid2 %>%
#   mutate(weekNo = week(submission_date)) %>%
#   filter(weekNo > week(today()) - 12) %>% 
#   group_by(state_name, weekNo) %>%
#   summarise(Deaths = sum(death_per_million)) %>% 
#   ungroup()

c_slice <-  covid2 %>%
  group_by(submission_date) %>% 
  mutate(rank = rank(-Deaths),
         Value_rel = Deaths/Deaths[rank==1 | rank==1.5],
         Value_lbl = paste0(" ",round(Deaths))) %>%
  filter(rank <=20) %>%
  ungroup()

p <- ggplot(c_slice, aes(rank, group = state_name, 
                         fill = as.factor(state_name), color = as.factor(state_name))) +
  geom_tile(aes(y = Deaths/2,
                height = Deaths,
                width = 0.8), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(state_name, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Deaths,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=20, hjust=0.5, face="bold", colour="grey"),
        plot.subtitle=element_text(size=14, hjust=0.5, color="grey"),
        plot.caption =element_text(size=10, hjust=0.5, color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim <- p + transition_states(submission_date, transition_length = 6, state_length = 1) +
# anim = p + transition_states(weekNo, transition_length = 6, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'COVID-19 deaths per million population',  
       subtitle  =  "Cumulative 30 Days {closest_state}",
       caption  = "Data Source: Scorata") +
  exit_fade()

animate(anim, nframes = 400, fps = 10,  width = 800, height = 600,
        renderer = gifski_renderer("gganim.gif"))

