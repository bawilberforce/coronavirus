
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(shiny)

# FUNCTION: get_data ----------------------------------------------------------


get_data<-function(area){
  endpoint_regions <- paste0('https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=',area,
  '&structure={"Date":"date","Area":"areaName","New_Cases":"newCasesBySpecimenDate","Cumulative_Cases_per_100k":"cumCasesBySpecimenDateRate","New_Admissions":"newAdmissions","New_Deaths":"newDeaths28DaysByDeathDate"}')
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
                            function(df){(df[,3:ncol(df)])})

  average_data<-lapply(area_breakdown_ordered, 
                       function(df){cbind(df,moving_averages_2[unique(as.character(df$Area))])})
  average_data_2<-lapply(average_data,
                      function(df){
                        x<-colnames(area_breakdown_ordered[[1]])
                        y<-paste0(colnames(
                        area_breakdown_ordered[[1]])[3:ncol(area_breakdown_ordered[[1]])],
                        "_Moving_Avg")
                        colnames(df)<-c(x,y)
                        return(df)})
  combined_dataset<-data.frame(matrix(ncol=10)) 
  colnames(combined_dataset)<-colnames(average_data_2[[1]])
  for (i in 1:length(average_data_2)) {
    combined_dataset<-rbind((average_data_2[[i]]),combined_dataset)
  }
  return(combined_dataset)}



# FUNCTION: create_graph --------------------------------------------------

create_graph<-function(dataset, var, var_label, focus_area, xlabel, start_date, end_date){

  filtered_data_1<-dataset%>%
    filter(Date>=as.Date(start_date) & Date<=as.Date(end_date))%>%
    filter(Area==focus_area)%>%
    select(Date, Area, var)%>%
    na.exclude()%>%
    select(Date, var_label=var)
  validate(need(nrow(filtered_data_1)!=0,"Data is not available for this area type."))
  filtered_data_2<-dataset%>%
    filter(Date>=as.Date(start_date) & Date<=as.Date(end_date))%>%
    filter(Area==focus_area)%>%
    select(Date, var_label=eval(paste0(var,"_Moving_Avg")))
  ggplot() + 
    geom_col(data=filtered_data_1, mapping=aes(x=Date,
                                               y=var_label))+
    geom_line(data=filtered_data_2, mapping=aes(x=Date,
                                                y=var_label, colour="7 Day Rolling Average"),
              size=2) +
    labs(x=xlabel, y=var_label, colour = "Key")}



# FUNCTION: list_poss_focus_areas ----------------------------------------------------

list_poss_focus_areas<-function(area){
  get_data(area) %>% 
  convert_to_json() %>%
  select(Area) %>%
  unique() }


# FUNCTION: output_time_series ---------------------------------------------------

output_time_series<-function(var,var_label,xlabel,focus_area, area,date_range){validate(need(focus_area!="" &
                                              focus_area %in% sapply(list_poss_focus_areas(area), 
                                                                           function(level){as.character(level)}) == TRUE, 
                                            ""))
  get_data(area=area)%>%
    convert_to_json()%>%
    calculate_rolling_avg()%>%
    create_graph(., var=var, var_label=var_label, xlabel=xlabel, focus_area=focus_area, 
                 start_date=min(date_range),
                 end_date=max(date_range))}
