box::use(
  app/logic/data
)

box::use(
  app/view/SingleTsCards
)

box::use(
  shiny[NS,tabPanel,fluidRow,column,moduleServer,reactive],
  dplyr[filter],
  echarts4r[e_charts,e_line,e_legend,e_tooltip,e_show_loading,
            echarts4rOutput,renderEcharts4r,e_toolbox_feature,e_datazoom,
            e_y_axis]
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Forecasting",
           fluidRow(
             column(3,
                    SingleTsCards$card1(ns("country_select"),ns("product_select"),ns("store_select"))
             ),
             column(9,
                    SingleTsCards$card2("Forecast of a single time series",echarts4rOutput(ns("singleforecast")))
             )
           )
         ) 
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    forecast_select <- reactive({
    
    data$fetch_forecast() |>
    filter(country == input$country_select &
           product == input$product_select &
           store == input$store_select)
      
    })
    
    output$singleforecast <- renderEcharts4r({
    
      forecast_select() |>
      e_charts(date) |>
      e_line(Sales) |>
      e_legend(show = FALSE) |>
      e_tooltip(trigger = "axis") |>
      e_show_loading()|>
        e_toolbox_feature(
          feature = c("saveAsImage",
                      "dataZoom")
        )|>
        e_datazoom(x_index = 0)|>
        e_y_axis(scale = TRUE)
    
    })
  })
}