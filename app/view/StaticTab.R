box::use(
  shiny[NS,moduleServer,tabPanel,fluidRow,column],
  bslib[layout_column_wrap,card,card_header,card_body_fill]
)

box::use(
  app/view/StaticTabComponents/StaticTabCards,
  app/view/StaticTabComponents/YearSeasByProd,
  app/view/StaticTabComponents/SalesByCountry
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tabPanel("Overview",
           fluidRow(
             layout_column_wrap(
               width = 1/4, height = 200,
               StaticTabCards$card1(),StaticTabCards$card2(),StaticTabCards$card3(),StaticTabCards$card4()
             )
           ),
           fluidRow(
             layout_column_wrap(
               width = 1/2,height = 600,
               card(
                 height = 500,
                 full_screen = TRUE,
                 card_header("Yearly seasonality of a product"),
                 card_body_fill(YearSeasByProd$ui(ns("ysbp")))
               ),
               card(
                 height = 500,
                 full_screen = TRUE,
                 card_header("Daily sales of a country"),
                 card_body_fill(SalesByCountry$ui(ns("sbc")))
               )
               
             ),
             style = 'padding-top:20px ; padding-bottom:20px'
             
           )
  )
           
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
     YearSeasByProd$server("ysbp")
     SalesByCountry$server("sbc")
      
  })
}

