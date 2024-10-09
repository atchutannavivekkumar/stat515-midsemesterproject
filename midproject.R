#libraries
library(tidyverse)
library(dplyr) 
library(readr)
library(maps)
library(mapproj)
library(plotly)


#theme
hw <- theme_gray()+ theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  plot.caption=element_text(hjust=-.5),
  
  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), linewidth =.2),
  
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.10,"cm"),
  panel.spacing.y = unit(0.05,"cm"),
  
  # axis.ticks.y= element_blank()
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(margin=margin(0,3,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)

#load state abbrevations
states <- read.csv("statess.csv") 

#load data and cbind abbrevations
job <- read.csv("dataset1.csv") %>%
  inner_join(states, by.x = states, by.y = state)

joba <- cbind(job, Abbreviation = states$Abbreviation)


ggplot(data = joba, aes(x=State, y=job_loss))+
  geom_bar(stat="identity", color="blue", fill="white")

#changing column names
names(joba) 
names(joba) [2] <- "job_loss"
names(joba) [3] <- "job_gains"

#bar graph in plotly

plot_ly(
  data = joba,
  x= ~State,
  y = ~job_loss,
  type = "bar",
  name = "Job loss"
) %>%
  add_trace(y=~job_gains, name = "job gain")


#map bar of job loss

 states <- map_data("state")

 view(states)
 rm(joba)
 
 joba$State <- tolower(joba$State)
 joba$job_losses =  joba$job_loss / 100
 usa_jobloss <- usa_tbl %>%
   left_join(joba, by = c("region"= "State"))%>%
   
 
 usa_jobloss

 
 usa_jobloss %>%
   ggplot(aes(long, lat, group = group)) +
   geom_map(
     aes(map_id = region),
     map = usa_tbl,
     color = "gray80", fill = "gray30", size = 0.3)+
   coord_map("ortho", orientation = c(39, -98, 0)) +
   geom_polygon(aes( fill = job_loss), color = "black") +
   theme_minimal() +
   labs( title = "Job Loss in USA",x = "", y = "", fill = "Job Loss per state") +
   theme(
     plot.title = element_text(size = 26, face = "bold", color = "blue"),
     legend.position = "left")
 
 usa_jobloss %>%
   ggplot(aes(long, lat, group = group)) +
   geom_map(
     aes(map_id = region),
     map = usa_tbl,
     color = "gray80", fill = "gray30", size = 0.3)+
   coord_map("ortho", orientation = c(39, -98, 0)) +
   geom_polygon(aes( fill = job_gains), color = "black") +
   theme_minimal() +
   labs( title = "Job Gain in USA",x = "", y = "", fill = "Job gain per state") +
   theme(
     plot.title = element_text(size = 26, face = "bold", color = "blue"),
     legend.position = "left")
 
 #dataset1 completed
 
 datasett <- read.csv("dataset3.csv") 
 
rm(dataset)

#to remove null values

 dataset <- na.omit(datasett)
 
 names(dataset)
 names(dataset)[4] <- "Employment_level"
 
 #bar chart with hours output in industry
 plot_ly(
   data = dataset,
   x= ~Industry,
   y = ~Hours,
   type = "bar",
   name = "Hours"
 ) %>%
   add_trace(y=~Output, name = "Output")
 
 plot_ly(
   data = dataset,
   x= ~Industry,
   y = ~Employment_level,
   type = "bar",
   name = "Employment level"
) 

 
 
 plot_ly(
     data = dataset, 
     y= ~Hours,
     type = "scatter",
     mode = 'scatt',
     name = "Hours"
   )%>%
     add_trace(y=~Output, name = "Output",mode ="lines+markers")

 
 plot_ly(
   data = dataset, 
   y= ~Employment_level,
   type = "scatter",
   mode = 'lines',
   name = "Hours"
 ) 
 