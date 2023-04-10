box::use(
  app/logic/data
)

box::use(
  echarts4r[group_by,e_charts,e_line,echarts4rOutput,renderEcharts4r],
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
      e_line(Sales)
      
      
    })
 
})
  
}
  
  
  