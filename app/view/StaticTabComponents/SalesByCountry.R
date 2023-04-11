box::use(
  app/logic/data
)

box::use(
  echarts4r[group_by,e_charts,e_line,renderEcharts4r,echarts4rOutput,
            e_tooltip,e_toolbox_feature,e_datazoom],
  shiny[plotOutput,renderPlot,NS,moduleServer]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  echarts4rOutput(ns("sbc"))
}

#' @export
server <- function(id){
  moduleServer(id,function(input,output,session){
    
    output$sbc <- renderEcharts4r({
      
        data$fetch_sbc() |>
        echarts4r::group_by(country) |>
        e_charts(date) |>
        e_line(Sales) |>
        e_tooltip(trigger = "axis")|>
        e_toolbox_feature(
          feature = c("saveAsImage",
                      "dataZoom")
        ) |>
        e_datazoom(x_index = 0)
      
    })
    
  })
  
}




