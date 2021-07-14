
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(shiny)

# FUNCTION: get_cases_data ----------------------------------------------------------


get_cases_data<-function(area){
  endpoint_regions <- paste0('https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=',area,'&structure={"Date":"date","Area":"areaName","NewCases":"newCasesBySpecimenDate"}')
  httr::GET(
    url = endpoint_regions,
    httr::timeout(10)
  ) -> response
  return(response)
  
  if (response$status_code >= 400) {
    err_msg <- httr::http_status(response)
    stop(err_msg)
    return(err_msg)
  }
}


# FUNCTION: convert_to_json -----------------------------------------------


convert_to_json<-function(response){
  json_text <- httr::content(response, "text")
  data      <- jsonlite::fromJSON(json_text)
  dataset<-as.data.frame(data[["data"]])
  dataset$Area<-as.factor(dataset$Area)
  dataset$Date<-as.Date(dataset$Date)
  return(dataset)}


# FUNCTION: calculate_rolling_avg -----------------------------------------


calculate_rolling_avg<-function(dataset){
  area_breakdown<-purrr::map(levels(dataset$Area), ~filter(dataset, dataset$Area==.x))
  names(area_breakdown)<-levels(dataset$Area)
  area_breakdown_ordered<-lapply(area_breakdown, 
                                 function(df){arrange(df,Date)})
  moving_averages<- lapply(area_breakdown_ordered, 
                           function(df){(rolling_average=stats::filter(df, rep(1/7,7), sides=2))})
  moving_averages_2<-lapply(moving_averages,
                            function(df){(df[,3])})

  average_data<-lapply(area_breakdown_ordered, 
                       function(df){cbind(df,moving_averages_2[unique(as.character(df$Area))])})
  avg_dataset<-lapply(average_data,
                      function(df){rename(df,moving_avg=eval(names(df)[length(df)]))})
  combined_dataset<-data.frame(matrix(ncol=4)) 
  colnames(combined_dataset)<-c("Date", "Area", "NewCases", "moving_avg")
  for (i in 1:length(avg_dataset)) {
    combined_dataset<-rbind(na.omit(avg_dataset[[i]]),combined_dataset)
  }
  return(combined_dataset)
}


# FUNCTION: create_graph --------------------------------------------------

create_graph<-function(dataset, focus_area, start_date, end_date){
  
  filtered_data_1<-dataset%>%
    filter(Date>=as.Date(start_date) & Date<=as.Date(end_date))%>%
    filter(Area==focus_area)%>%
    select(Date, Cases=NewCases)
  filtered_data_2<-dataset%>%
    filter(Date>=as.Date(start_date) & Date<=as.Date(end_date))%>%
    filter(Area==focus_area)%>%
    select(Date, Cases=moving_avg)
  
  ggplot() + 
    geom_col(data=filtered_data_1, mapping=aes(x=Date,
                                               y=Cases))+
    geom_line(data=filtered_data_2, mapping=aes(x=Date,
                                                y=Cases, colour="7 Day Rolling Average"),
              size=2) +
    labs(x="Specimen Date", y="New Cases", colour = "Key")
    #theme(legend.title = element_blank())
    #ylab("New Cases") +
    #xlab("Specimen Date")
}


# FUNCTION: list_poss_focus_areas ----------------------------------------------------

list_poss_focus_areas<-function(area){
  get_cases_data(area) %>% 
  convert_to_json() %>%
  select(Area) %>%
  unique() }
