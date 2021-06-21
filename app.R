
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
    return(dataset)
}


# FUNCTION: create_graph --------------------------------------------------

create_graph<-function(dataset, start_date, end_date){
    
    filtered_data<-dataset%>%
        dplyr::filter(Date>=as.Date(start_date) & Date<=as.Date(end_date))
    filtered_data%>%ggplot2::ggplot(ggplot2::aes(x = Date, y = NewCases)) + 
        ggplot2::geom_point(ggplot2::aes(colour=Area))+
        ggplot2::facet_grid(rows=filtered_data$Area)+
        ggplot2::ylab("New Cases")+
        ggthemes::theme_few()
}




# Create Shiny App --------------------------------------------------------

ui <- shiny::fluidPage(
    shiny::titlePanel("New Coronavirus Cases in the UK"),
    shiny::sidebarPanel(
        shiny::dateRangeInput("date_range", "Please select a date range:",
                              "2020-03-01", Sys.Date(),
                              max=Sys.Date(),min="2020-02-01"),
        shiny::selectInput("area", "Plot data by:",
                           choices = c("nation","region","ltla"))
    ),
    shiny::mainPanel(
        shiny::plotOutput("Plot1")
    )
)

server <- function(input, output){
    output$Plot1<-shiny::renderPlot(get_cases_data(area=input$area)
                                    %>% convert_to_json()
                                    %>% create_graph(.,start_date=min(input$date_range),
                                                     end_date=max(input$date_range)))
}


# Run App -----------------------------------------------------------------


shiny::shinyApp(ui, server)
