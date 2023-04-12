box::use(
  app/logic/data
)

box::use(
  echarts4r[group_by,e_charts,e_line,echarts4rOutput,renderEcharts4r,
            e_tooltip,e_toolbox_feature,e_datazoom,e_y_axis],
  shiny[NS,moduleServer]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  echarts4rOutput(ns("ysbp"))
}

#' @export
server <- function(id){
  moduleServer(id,function(input,output,session){
   
    output$ysbp <- renderEcharts4r({
      
      data$fetch_ysbp() |>
      echarts4r::group_by(product) |>
      e_charts(date) |>
      e_line(Sales) |>
      e_tooltip(trigger = "axis")|>
        e_toolbox_feature(
          feature = c("saveAsImage",
                      "dataZoom")
        )|>
        e_datazoom(x_index = 0) |>
        e_y_axis(scale = TRUE)
      
      
    })
 
})
  
}
  
  
  