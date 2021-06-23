
# Load Packages -----------------------------------------------------------

library(dplyr)

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
                                   function(df){dplyr::arrange(df,Date)})
    moving_averages<- lapply(area_breakdown_ordered, 
                             function(df){(rolling_average=accelerometry::movingaves(df$NewCases,window=7))})
    average_data<-lapply(area_breakdown_ordered, 
                         function(df){cbind(df[7:length(df$Date),],moving_averages[unique(as.character(df$Area))])})
    avg_dataset<-lapply(average_data,
                         function(df){dplyr::rename(df,moving_avg=eval(names(df)[length(df)]))})
combined_dataset<-data.frame(matrix(ncol=4)) 
colnames(combined_dataset)<-c("Date", "Area", "NewCases", "moving_avg")
    for (i in 1:length(avg_dataset)) {
        combined_dataset<-rbind(avg_dataset[[i]],combined_dataset) %>%
            na.omit()
    }
return(combined_dataset)
}


# FUNCTION: create_graph --------------------------------------------------

create_graph<-function(dataset, start_date, end_date){
    
    filtered_data<-dataset%>%
        dplyr::filter(Date>=as.Date(start_date) & Date<=as.Date(end_date))
    filtered_data%>%ggplot2::ggplot(ggplot2::aes(x = Date, y = moving_avg)) + 
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::facet_grid(rows=filtered_data$Area, ) +
        ggplot2::ylab("Rolling 7 Day Average of New Cases") +
        ggplot2::xlab("Specimen Date")
        }




# Create Shiny App --------------------------------------------------------

ui <- shiny::fluidPage(
    shiny::titlePanel("New Coronavirus Cases in the UK"),
    shiny::fluidPage(
        shiny::dateRangeInput("date_range", "Please select a date range:",
                              "2020-03-01", Sys.Date(),
                              max=Sys.Date(),min="2020-02-01"),
        shiny::selectInput("area", "Plot data by:",
                           choices = c("nation","region","ltla"))
    ),
    shiny::fluidPage(
        shiny::plotOutput("Plot1")
        
    )
)

server <- function(input, output){
    output$Plot1<-shiny::renderPlot(get_cases_data(area=input$area)
                                    %>% convert_to_json()
                                    %>% calculate_rolling_avg()
                                    %>% create_graph(.,start_date=min(input$date_range),
                                                     end_date=max(input$date_range)))
}


# Run App -----------------------------------------------------------------


shiny::shinyApp(ui, server)
