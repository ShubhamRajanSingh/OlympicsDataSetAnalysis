# Olpmpics dataset analysis
# author: Shubham Singh

library(dplyr)
library(ggplot2)

# reads the athlete data 
olymics_data <- read.csv("E:\\RWorkplace\\Blog\\Olympics\\athlete_events.csv", header=TRUE,fill = TRUE , sep=",")
View(olymics_data)


#Separting the data of India 
ind_data <- olymics_data %>% filter(NOC=='IND')
View(ind_data)


# Separting the data of Summer and Winter Olympics
summer_data <- olymics_data %>% filter(Season=='Summer')
winter_data <- olymics_data %>% filter(Season=='Winter')


# Separating the data of Summer and Winter olympics of India
winter_ind_data <- olymics_data %>% filter(NOC=='IND',Season=='Winter')
View(winter_ind_data)
summer_ind_data <- olymics_data %>% filter(NOC=='IND',Season=='Summer')
View(summer_ind_data)


# Removing the events column and forming a new data frame 
without_event_Summer <- summer_data[,-(13:14),drop=FALSE]

without_event_Winter <- winter_data[,-(13:14),drop=FALSE]

without_event_Ind_Summer <- summer_ind_data[,-(13:14),drop=FALSE]

without_event_Ind_Winter <- winter_ind_data[,-(13:14),drop=FALSE]





# removing the duplicate values
unique_without_event_Summer <- unique(without_event_Summer)

unique_without_event_Winter <- unique(without_event_Winter)

unique_without_event_Ind_Summer <- unique(without_event_Ind_Summer)

unique_without_event_Ind_Winter <- unique(without_event_Ind_Winter)


#plotting graph of Male  and Female in Olympics



# getting data of male and female in summer Olympics

male_olympics <- subset(unique_without_event_Summer, select="Year", Sex=='M')
male_olympics <- male_olympics %>% 
  group_by(Year) %>% 
  mutate(frequency = n()) %>% 
  select(Year, frequency) %>% 
  distinct()

male_olympics <- arrange(male_olympics,Year)


female_olympics <-subset(unique_without_event_Summer, select="Year", Sex=='F')
female_olympics <- female_olympics %>% 
  group_by(Year) %>% 
  mutate(frequency = n()) %>% 
  select(Year, frequency) %>% 
  distinct() 






male_ind_olympics <- subset(unique_without_event_Ind_Summer, select="Year", Sex=='M')
male_ind_olympics <- male_ind_olympics %>% 
  group_by(Year) %>% 
  mutate(frequency = n()) %>% 
  select(Year, frequency) %>% 
  distinct()

male_ind_olympics <- arrange(male_ind_olympics,Year)



female_ind_olympics <- subset(unique_without_event_Ind_Summer, select="Year", Sex=='F')
female_ind_olympics <- female_ind_olympics %>% 
  group_by(Year) %>% 
  mutate(frequency = n()) %>% 
  select(Year, frequency) %>% 
  distinct()

# getting data of male and female in Winter Olympics

male_winter_olympics <- subset(unique_without_event_Winter, select="Year", Sex=='M')
male_winter_olympics <- male_winter_olympics %>% 
  group_by(Year) %>% 
  mutate(frequency = n()) %>% 
  select(Year, frequency) %>% 
  distinct()


female_Winter_olympics <-subset(unique_without_event_Winter, select="Year", Sex=='F')
female_Winter_olympics <- female_Winter_olympics %>% 
  group_by(Year) %>% 
  mutate(frequency = n()) %>% 
  select(Year, frequency) %>% 
  distinct() 

male_ind_winter_olympics <- subset(unique_without_event_Ind_Winter, select="Year", Sex=='M')
male_ind_winter_olympics <- male_ind_winter_olympics %>% 
  group_by(Year) %>% 
  mutate(frequency = n()) %>% 
  select(Year, frequency) %>% 
  distinct()



female_ind_Winter_olympics <- subset(unique_without_event_Ind_Winter, select="Year", Sex=='F')
female_ind_Winter_olympics <- female_ind_Winter_olympics %>% 
  group_by(Year) %>% 
  mutate(frequency = n()) %>% 
  select(Year, frequency) %>% 
  distinct()

setwd("E:\\RWorkplace\\Blog\\Olympics\\")
# plot for overall gender participation in summer olpmpics
plotgender<-ggplot() +geom_line(male_olympics,mapping=aes(x=male_olympics$Year,y=male_olympics$frequency,color='blue'))+geom_line(female_olympics,mapping=aes(x=female_olympics$Year,y=female_olympics$frequency,color='red'))
plotgender<-plotgender+labs(title="Gender Participation",y='Number of players',x='Year')
plotgender<-plotgender+labs(title = 'Summer Gender Participation')
plotgender<-plotgender+labs(col = 'Gender')
plotgender<-plotgender+scale_fill_manual(name="Gender",labels=c("Male", "Female"))
plotgender<-plotgender+scale_colour_manual(labels = c("Male", "Female"), values = c("blue", "red"))
plotgender


# plot for overall gender participation in summer olpmpics

plotgender<-ggplot() +geom_line(male_ind_olympics,mapping=aes(x=male_ind_olympics$Year,y=male_ind_olympics$frequency,color='blue'))+geom_line(female_ind_olympics,mapping=aes(x=female_ind_olympics$Year,y=female_ind_olympics$frequency,color='red'))
plotgender<-plotgender+labs(title="Gender Participation",y='Number of players',x='Year')
plotgender<-plotgender+labs(title = 'Summer Gender Participation')
plotgender<-plotgender+labs(col = 'Gender')
plotgender<-plotgender+scale_fill_manual(name="Gender",labels=c("Male", "Female"))
plotgender<-plotgender+scale_colour_manual(labels = c("Male", "Female"), values = c("blue", "red"))
plotgender


# plot for overall gender participation in Winter olpmpics


plotgender<-ggplot() +geom_line(male_winter_olympics,mapping=aes(x=male_winter_olympics$Year,y=male_winter_olympics$frequency,color='blue'))+geom_line(female_Winter_olympics,mapping=aes(x=female_Winter_olympics$Year,y=female_Winter_olympics$frequency,color='red'))
plotgender<-plotgender+labs(title="Gender Participation",y='Number of players',x='Year')
plotgender<-plotgender+labs(title = 'Winter Olympics Gender Participation')
plotgender<-plotgender+labs(col = 'Gender')
plotgender<-plotgender+scale_fill_manual(name="Gender",labels=c("Male", "Female"))
plotgender<-plotgender+scale_colour_manual(labels = c("Male", "Female"), values = c("blue", "red"))
plotgender

# plot for overall gender participation in Winter olpmpics


plotgender<-ggplot() +geom_line(male_ind_winter_olympics,mapping=aes(x=male_ind_winter_olympics$Year,y=male_ind_winter_olympics$frequency,color='blue'))+geom_line(female_ind_Winter_olympics,mapping=aes(x=female_ind_Winter_olympics$Year,y=female_ind_Winter_olympics$frequency,color='red'))
plotgender<-plotgender+labs(title="Gender Participation",y='Number of players',x='Year')
plotgender<-plotgender+labs(title = 'Winter Olympics India Gender Participation')
plotgender<-plotgender+labs(col = 'Gender')
plotgender<-plotgender+scale_fill_manual(name="Gender",labels=c("Male", "Female"))
plotgender<-plotgender+scale_colour_manual(labels = c("Male", "Female"), values = c("blue", "red"))
plotgender
