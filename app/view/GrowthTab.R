box::use(
  shiny[NS,tabPanel,fluidRow,column,moduleServer],
  dplyr[filter,mutate,select,group_by,summarise],
  echarts4r[echarts4rOutput,renderEcharts4r,group_by,e_charts,
            e_line,e_facet,e_group,e_connect_group,e_tooltip,
            e_toolbox_feature],
  lubridate[year]
)

box::use(
  app/view/GrowthTab/GrowthCards,
  app/logic/data
)



#' @export
ui <- function(id) {
  ns <- NS(id)

  tabPanel("Trend",
           fluidRow(
             column(2,
                    GrowthCards$card1(ns("country_select"))
                    ),
             column(5,
                    GrowthCards$card2("KaggleRama",echarts4rOutput(ns("growthr")))
                    ),
             column(5,
                    GrowthCards$card2("KaggleMart",echarts4rOutput(ns("growthm")))
             )
           )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$growthr <- renderEcharts4r({
    
        data$fetch_data() |>
        filter(country == input$country_select) |>
        filter(store == "KaggleRama") |>
        mutate(Year = year(date)) |>
        select(product,Sales,Year) |>
        dplyr::group_by(Year,product) |>
        summarise(Trend = mean(Sales),.groups = "drop") |>
        mutate(Year = as.factor(Year)) |>
        mutate(product = as.factor(product)) |>
        echarts4r::group_by(product) |>
        e_charts(Year) |>
        e_line(Trend) |>
        e_facet(cols = 2,row=2) |>
        e_group("grp") |>
        e_tooltip(trigger = "axis") |>
        e_toolbox_feature(
          feature = c("saveAsImage")
        )
    
    })
    
    output$growthm <- renderEcharts4r({
      
        data$fetch_data() |>
        filter(country == input$country_select) |>
        filter(store == "KaggleMart") |>
        mutate(Year = year(date)) |>
        select(product,Sales,Year) |>
        dplyr::group_by(Year,product) |>
        summarise(Trend = mean(Sales),.groups = "drop") |>
        mutate(Year = as.factor(Year)) |>
        mutate(product = as.factor(product)) |>
        echarts4r::group_by(product) |>
        e_charts(Year) |>
        e_line(Trend) |>
        e_facet(cols = 2,row=2) |>
        e_group("grp") |>
        e_connect_group("grp")|>
        e_tooltip(trigger = "axis") |>
        e_toolbox_feature(
          feature = c("saveAsImage")
        )
      
    })
    
    
  })
}

