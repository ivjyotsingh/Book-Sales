box::use(
  app/logic/data
)

box::use(
  app/view/CountrySelect
)

box::use(
  shiny[NS,plotOutput,moduleServer,renderPlot,tabPanel,fluidRow,column,reactive],
  dplyr[filter,mutate,group_by,summarise,ungroup],
  lubridate[wday,month,year],
  echarts4r[e_charts,e_line,e_legend,renderEcharts4r,echarts4rOutput,
            e_tooltip,e_show_loading,e_bar,e_histogram,e_toolbox,
            e_toolbox_feature,e_datazoom,e_y_axis]

)

box::use(
  app/view/SingleTsCards
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  
  
tabPanel("Explore a single time series",
           fluidRow(
             column(3,
                    SingleTsCards$card1(ns("country_select"),ns("product_select"),ns("store_select"))
                    ),
             column(9,
                    SingleTsCards$card2("Single Time Series",echarts4rOutput(ns("singlets")))
                    )
           ),
           fluidRow(
             column(3,
                    SingleTsCards$card2("Distribution",echarts4rOutput(ns("dist")))
                    ),
             column(3,
                    SingleTsCards$card2("Weekly Seasonality",echarts4rOutput(ns("weekseas")))
                    ),
             column(6,
                    SingleTsCards$card2("Yearly Seasonality",echarts4rOutput(ns("yearseas")))
                    )
           )
  ) 

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ReactiveData <- reactive({
      
        data$fetch_data() |>
        mutate(date = as.Date(date)) |>
        filter(country == input$country_select &
               store == input$store_select &
               product == input$product_select)
      
    })
    
    
    output$singlets <- renderEcharts4r({
      
        ReactiveData() |>
        e_charts(date) |>
        e_line(Sales) |>
        e_legend(show = FALSE) |>
        e_tooltip(trigger = "axis") |>
        e_show_loading() |>
        e_toolbox_feature(
          feature = c("saveAsImage")
        )|>
        e_datazoom(x_index = 0)|>
        e_y_axis(scale = TRUE)
      
    })
    
    output$dist <- renderEcharts4r({
      
        ReactiveData() |>
        e_charts() |>
        e_histogram(Sales, name = "histogram") |>
        e_tooltip(trigger = "axis") |>
        e_legend(show = FALSE) |>
        e_show_loading()|>
        e_toolbox_feature(
          feature = c("saveAsImage")
        )
        
      
      
    })
    
    output$weekseas <- renderEcharts4r({
      
        ReactiveData() |>
        mutate(DayOfWeek = wday(date,label = T,abbr = T)) |>
        group_by(DayOfWeek) |>
        summarise(Sales = round(mean(Sales))) |>
        mutate(Mean = mean(Sales)) |>
        mutate(DayOfWeek = as.factor(as.character(DayOfWeek))) |>
        e_charts(DayOfWeek) |>
        e_bar(Sales) |>
        e_legend(show = FALSE) |>
        e_show_loading() |>
        e_tooltip(trigger = "axis")|>
        e_toolbox_feature(
          feature = "saveAsImage") |>
        e_toolbox_feature(
          feature = "magicType",
          type = list("line", "bar"))|>
        e_y_axis(scale = TRUE)
      
    })
    
    output$yearseas <- renderEcharts4r({
      
        ReactiveData() |>
        mutate(Month = month(date,label = T,abbr = T)) |>
        mutate(Year = year(date)) |>
        group_by(Year,Month) |>
        summarise(Sales = round(mean(Sales)),.groups = "drop") |>
        ungroup() |>
        mutate(Month = as.factor(as.character(Month))) |>
        echarts4r::group_by(Year) |>
        e_charts(Month) |>
        e_line(Sales) |>
        e_tooltip(trigger = "axis") |>
        e_show_loading()|>
        e_toolbox_feature(
          feature = c("saveAsImage",
                      "dataZoom")
        )|>
        e_y_axis(scale = TRUE)
        
      
    })
    
  })
}