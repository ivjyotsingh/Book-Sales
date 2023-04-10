box::use(
  shiny[NS,moduleServer,tabPanel]
)

box::use(
  
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Trend",
    
    
    
    
  )
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}