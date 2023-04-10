box::use(
  app/logic/data
)

box::use(
  echarts4r[group_by,e_charts,e_line,renderEcharts4r,echarts4rOutput],
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
        e_line(num_sold) 
      
    })
    
  })
  
}




