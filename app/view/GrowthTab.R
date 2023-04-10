box::use(
  shiny[NS,tabPanel,fluidRow,column,moduleServer,plotOutput,renderPlot],
  dplyr[filter,mutate,select,group_by,summarise],
  ggplot2[ggplot,geom_point,geom_line,aes,facet_wrap,labs,theme,
          element_rect,element_text,element_blank],
  lubridate[year]
)

box::use(
  app/view/GrowthTab/GrowthCards,
  app/logic/data
)



#' @export
ui <- function(id) {
  ns <- NS(id)

  tabPanel("Growth",
           fluidRow(
             column(3,
                    GrowthCards$card1(ns("country_select"))
                    ),
             column(9,
                    GrowthCards$card2("Yearly Growth Graph By Country",plotOutput(ns("growth")))
                    )
           )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$growth <- renderPlot({
    
    data$fetch_data() |>
    filter(country == input$country_select) |>
    mutate(date = as.Date(date)) |>
    mutate(Year = year(date)) |>
    select(country,store,product,num_sold,Year) |>
    group_by(Year,country,store,product) |>
    summarise(Trend = mean(num_sold),.groups = "drop") |>
    ggplot() +
    geom_point(mapping = aes(x = Year,y = Trend),size = 3) +
    geom_line(mapping = aes(x = Year,y = Trend),color = "#193964",lwd = 1.2) +
    facet_wrap(.~store+product,ncol = 3,scales = "free") +
    labs(title = input$country_select,
         y = "",
         x = "")+
    theme(panel.background = element_rect(fill = NA),
          strip.text = element_text(
            size = 13,color = "black"
          ),
          strip.background = element_rect(
            color = "black",fill = "white",linetype = "solid"
          ),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 18),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 15)
    )
        

      
    })
  })
}

