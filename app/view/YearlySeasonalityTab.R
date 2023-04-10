box::use(
  app/view/YearSeasTabComponents/YearSeasCards,
  app/logic/data
)


box::use(
  shiny[NS,moduleServer,tabPanel,fluidRow,column,plotOutput,renderPlot],
  dplyr[filter,mutate,group_by,summarise,ungroup],
  ggplot2[ggplot,geom_line,aes,facet_wrap,labs,theme,element_rect,element_blank,
          element_text],
  lubridate[month,year]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Yearly Seasonality",
           fluidRow(
             column(3,
                    YearSeasCards$card1(ns("product_select"))
             ),
             column(9,
                    YearSeasCards$card2("Yearly Seasonality Of Each Product",plotOutput(ns("yearseas")))
             )
           )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$yearseas <- renderPlot({
      
        data$fetch_data() |>
        filter(product == input$product_select) |>
        mutate(date = as.Date(date)) |>
        mutate(Month = month(date,label = T,abbr = T)) |>
        mutate(Year = year(date)) |>
        group_by(country,store,Year,Month) |>
        summarise(num_sold = mean(num_sold),.groups = "drop") |>
        ungroup() |>
        ggplot() +
        geom_line(mapping = aes(x = Month,y = num_sold,group = as.factor(Year),
                                color = as.factor(Year)),lwd = 1) +
        facet_wrap(.~country + store ,ncol = 3,scales = "free") +
        labs(title = input$product_select,
             x = "",
             y = "Books Sold",
             color = "Product") +
        theme(panel.background = element_rect(fill = NA),
              axis.ticks = element_blank(),
              plot.title = element_text(size = 18),
              axis.text.x = element_text(size = 11),
              strip.text = element_text(
                size = 13,color = "black"
              ),
              strip.background = element_rect(
                color = "black",fill = "white",linetype = "solid"
              ))
        
      
    })
  })
}

