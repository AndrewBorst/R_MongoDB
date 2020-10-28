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
# 3. Import the data back from MongoDB
# 4. Grab new data and append to the collection
# 5. Use gganimate to publish shiny horizontal bar chart 

#read in state popluations used for per capita calculations
<- read.csv(file = "Data/State_Popluations.csv")

#this read method does not return _id
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

m$run('{"count":"indiana"}')


c <- covid %>%
    mutate(weekNo = week(submission_date)) %>%
    filter(weekNo > week(today()) - 12) %>% 
    select(state, weekNo, new_death) %>% 
    group_by(state, weekNo) %>%
    summarise(Deaths = sum(new_death)) %>% 
    ungroup()

# c_percent_increase <- c %>%
#   group_by(state) %>%
#   mutate(Percent = Deaths / lag(Deaths)) %>%
#   filter(is.finite(Percent)) %>% 
#   ungroup() 

c_slice <-  c %>%
  group_by(weekNo) %>%
  mutate(rank = rank(-Deaths),
         Value_rel = Deaths/Deaths[rank==1 | rank==1.5],
         Value_lbl = paste0(" ",round(Deaths/1e9))) %>%
  group_by(state) %>% 
  filter(rank <=10) %>%
  ungroup()
  

p <- ggplot(c_slice, aes(rank, group = state, 
                fill = as.factor(state), color = as.factor(state))) +
  geom_tile(aes(y = Deaths/2,
                height = Deaths,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(state, " ")), vjust = 0.2, hjust = 1) +
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
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim = p + transition_states(weekNo, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Deaths per Week : {closest_state}',  
       subtitle  =  "Top 10 States",
       caption  = "Data Source: Scorata")

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))

