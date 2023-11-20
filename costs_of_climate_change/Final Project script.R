# Samuel Harris Final Project


rm(list=ls())


library(tidyverse)
library(sf)

load("/Users/samharris/Desktop/Data Science/FINAL PROJECT/Data for Project/county_shapefile.RData")
load("/Users/samharris/Desktop/Data Science/FINAL PROJECT/Data for Project/state_shapefile.RData")

# t =  temperature data for counties in 2020
# p =  preciptation data for couties in 2020

t= read_csv("/Users/samharris/Desktop/Data Science/FINAL PROJECT/Data for Project/Temp Avg 2020.csv", skip=3)

p= read_csv("/Users/samharris/Desktop/Data Science/FINAL PROJECT/Data for Project/Preciptation 2020 .csv", skip=3)

names(t)
names(p)

t=t %>%
  rename(
    value_t = Value,
    rank_t = Rank,
    anomaly_t ="Anomaly (1901-2000 base period)",
    base_mean_t="1901-2000 Mean"
  )


p=p %>%
  rename(
    value_p = Value,
    rank_p = Rank,
    anomaly_p ="Anomaly (1901-2000 base period)",
    base_mean_p="1901-2000 Mean"
  )

d=left_join(
  t,
  p
)

d = d %>%
  mutate(
    `Location ID` = gsub("-...", "", `Location ID`)
  )

summary(d$anomaly_p, na.rm=T)

summary(d$anomaly_t, na.rm =T)

s_d=d %>% 
  filter(2.7< anomaly_t, -2.440 >anomaly_p) 

table(s_d$`Location ID`)

#co= number of counties for each state data

co= read_csv("/Users/samharris/Desktop/Data Science/FINAL PROJECT/Data for Project/Number of counties.csv")

co = co %>%
  mutate(
    State = state.abb[match(State, state.name)]
  )

co=co %>% 
  filter(State!="AK", State!="HI")

s_d=left_join(
  co,
  s_d,
  by= c(State= "Location ID")
)

s_d=s_d %>% 
  drop_na() %>% 
  select(-Pop)


h=as.data.frame(table(s_d$State))

h=left_join(
  h,
  co,
  by= c(Var1= "State")
)


h=h %>% 
  rename( State=Var1) %>% 
  mutate(Percent = Freq/counties)

h=h %>% 
  filter(Percent>.5)

h %>% 
  group_by(State) %>% 
 summarize(Percent)

# ^^^^^ This analysis highlights the percentage of counties in each state where the temperature change from-
#-the base mean is in the upper quartile of all counties in the US and the precipitation change from the-
# base mean is in the lower quartile of all counties. In other words this analysis shows the states that have
# have had the most drastic changes in terms of higher temperatures and lower rainfall rates.


west_dis= read_csv("/Users/samharris/Desktop/Data Science/FINAL PROJECT/Data for Project/West Climate Reigon Disaster .csv", skip=2)

south_west_dis= read_csv("/Users/samharris/Desktop/Data Science/FINAL PROJECT/Data for Project/South West Climate Region Disasters .csv", skip=2)


west_dis = west_dis %>%
  mutate(
    plot_coordinate = `All Disasters Count`*5
  )

  ggplot(west_dis) + 
    geom_bar(aes(x=Year, y=plot_coordinate),stat="identity", fill="black", color="grey")+
    labs(title = "Billion Dollar Disasters by Cost and Year for the West", y = "All Disasters Cost (In Billions)")+
    geom_line(aes(x=Year, y=`All Disasters Cost`), size=1, color="red", stat="identity")+
    scale_y_continuous(sec.axis = sec_axis( ~ .* .2, name="All Disasters Count"))+
    theme_classic()
  
  
  ggplot(south_west_dis) + 
    geom_bar(aes(x=Year, y=`All Disasters Count`),stat="identity", fill="black", color="grey")+
    labs(title = "Billion Dollar Disasters by Cost and Year for the South West", y = "All Disasters Count")+
    geom_line(aes(x=Year, y=`All Disasters Cost`), size=1, color="red", stat="identity")+
    scale_y_continuous(sec.axis = sec_axis( ~ .* 1, name="All Disasters Cost (In Billions)"))+
    theme_classic()

  #Both Graphs show correlation between the number of disaster events and the cost in Billions
  d = d %>%
    mutate(
      Location=gsub(" County| City| Parish","", d$Location)
    )
  
  d = d %>%
    mutate(
      Location=ifelse(`Location ID`=="LA" & Location=="La Salle","LaSalle",Location),
      Location=ifelse(Location=="Dona Ana","Doña Ana",Location)
      )

  
  d=d %>% 
   rename(NAME=Location)
  
 fip_code =read_csv("/Users/samharris/Desktop/Data Science/FINAL PROJECT/Data for Project/us-state-ansi-fips.csv")
 
 fip_code=fip_code %>% 
   rename("Location ID"=stusps)
  
  d=left_join(d,fip_code)
  
  
  d_map = left_join(county_shapefile, d, by = c(STATEFP="st", NAME="NAME"))

  
  d_map=d_map %>% 
    drop_na()
 

  
  temp_map = 
    d_map %>% 
    #filter(`Location ID`=="CA") %>%
    ggplot(aes(fill = anomaly_t)) +
    geom_sf()+
    theme_void() + 
    scale_fill_gradient(low = "black", high = "red")+
  labs(fill = "Change from Base Mean",
       title = "Temperature Change")+
    theme(legend.position = c(.2, .07),
          legend.direction = "vertical",
          plot.title = element_text(size = 16, hjust = .5, margin = margin(10, 0, 5, 0)))  
  
  temp_map
  
  precip_map = 
    d_map %>% 
    #filter(`Location ID`=="OR") %>%
    ggplot(aes(fill = anomaly_p)) +
    geom_sf()+
    theme_void() + 
    scale_fill_gradient(low = "red", high = "black")+
    labs(fill = "Change from Base Mean",
         title = "Precipitation Change")+
    theme(legend.position = c(.2, .07),
          legend.direction = "vertical",
          plot.title = element_text(size = 16, hjust = .5, margin = margin(10, 0, 5, 0))) 
  
  precip_map
  
  
  
  #Map of Preciptation change and Temperature Change from  base means are above   
  
  
  rm(list=ls())
  
  library(tidyverse)
  library(sf)
  library(estimatr)
  
  cal_d =read_csv("/Users/samharris/Desktop/Data Science/FINAL PROJECT/Data for Project/New Data/Disasters-CA-1980-2021.csv", skip=2)
  
  cal_p = read_csv("/Users/samharris/Desktop/Data Science/FINAL PROJECT/Data for Project/New Data/Cali Preciptation numbers from 1980.csv", skip=4)

  #possibly go back and look at some regression between 2 variables  
  
  cal_p= cal_p %>% 
    mutate(
      Date=gsub("12$","", Date)
    ) 
  
    cal_p$Date= as.numeric(cal_p$Date)
  
  cal_d = cal_d %>% 
    rename(Date="Year")
  
  
  
  cal_full = left_join(cal_p, cal_d)
  
  
  cal_full= cal_full %>% 
    mutate(
      `Wildfire Cost Range`=gsub("-.+","", `Wildfire Cost Range`)
    ) 
  
  cal_full$`Wildfire Cost Range`= as.numeric(cal_full$`Wildfire Cost Range`)
  
  cal_full= cal_full %>% 
    mutate(
      `Drought Cost Range`=gsub("-.+","", `Drought Cost Range`)
    ) 
  
  cal_full$`Drought Cost Range`= as.numeric(cal_full$`Drought Cost Range`)
  
  cal_wildcost_plot = cal_full %>%
    ggplot(aes(x = Date, y =`Wildfire Cost Range`)) +
    geom_smooth(se=FALSE) +
    theme_bw()+
    labs(title="Wildfire Cost Over Time",
         x ="Year", y = "Wildfire Cost (millions)")
  
  cal_wildcost_plot
  
  cal_droughtcost_plot = cal_full %>%
    ggplot(aes(x = Date, y =`Drought Cost Range`)) +
    geom_smooth(se=FALSE) +
    theme_bw()+
    labs(title="Drought Cost Over Time",
         x ="Year", y = "Drought Cost (millions)")
  
  cal_droughtcost_plot
  
  cal_precip_plot = cal_full %>%
    ggplot(aes(x = Date, y =Anomaly)) +
    geom_smooth(se=FALSE) +
    theme_bw()+
   expand_limits(y=c(-3, 6))+
    labs(title="Preciptation Change from Base Mean",
         x ="Year", y = "Inches of Rain Anomaly")
  
  cal_precip_plot
  

  library(patchwork)
  
  cal_wildfire = cal_precip_plot / cal_wildcost_plot / cal_droughtcost_plot
  
  cal_wildfire

  
  
  