box::use(
  app/logic/data
)

box::use(
  app/view/CountrySelect
)

box::use(
  shiny[NS,plotOutput,moduleServer,renderPlot,tabPanel,fluidRow,column,reactive],
  dplyr[filter,mutate,group_by,summarise,ungroup],
  ggplot2[ggplot,geom_line,aes,geom_histogram,geom_vline,geom_col,geom_hline,scale_fill_identity,
          theme_classic,theme,element_text,labs,element_rect,element_blank,margin],
  lubridate[wday,month,year]

)

box::use(
  app/view/SingleTsCards
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  
  
tabPanel("Single Time Series",
           fluidRow(
             column(3,
                    SingleTsCards$card1(ns("country_select"),ns("product_select"),ns("store_select"))
                    ),
             column(9,
                    SingleTsCards$card2("Single Time Series",plotOutput(ns("singlets")))
                    )
           ),
           fluidRow(
             column(3,
                    SingleTsCards$card2("Distribution",plotOutput(ns("dist")))
                    ),
             column(3,
                    SingleTsCards$card2("Weekly Seasonality",plotOutput(ns("weekseas")))
                    ),
             column(6,
                    SingleTsCards$card2("Yearly Seasonality",plotOutput(ns("yearseas")))
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
    
    
    output$singlets <- renderPlot({
      
        ReactiveData() |>
        ggplot() +
        geom_line(mapping = aes(x = date,y = num_sold)) +
        labs(x = "",
             y = "Books Sold") +
        theme(panel.background = element_rect(fill = NA),
              axis.ticks = element_blank(),
              axis.text = element_text(size = 15),
              axis.title.y = element_text(size = 15,margin = margin(0,10,0,0)))
      
    })
    
    output$dist <- renderPlot({
      
        ReactiveData() |>
        ggplot() +
        geom_histogram(mapping = aes(x = num_sold)
                       ,bins = 40
                       ,color = "black"
                       ,fill = "white") +
        geom_vline(aes(xintercept = mean(num_sold))
                   ,linetype = "dashed"
                   ,color = "blue"
                   ,size = 1.4) +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              panel.background = element_rect(fill = NA),
              axis.ticks = element_blank(),
              axis.title.x = element_text(size = 15,margin = margin(5,0,0,0)),
              axis.title.y = element_text(size = 15)) +
        labs(x = "Books Sold",
             y = "Count")
      
      
    })
    
    output$weekseas <- renderPlot({
      
        ReactiveData() |>
        mutate(DayOfWeek = wday(date,label = T,abbr = T)) |>
        group_by(DayOfWeek) |>
        summarise(num_sold = mean(num_sold)) |>
        mutate(Mean = mean(num_sold)) |>
        mutate(color = c("blue", "#CCCCCC","#CCCCCC","#CCCCCC","#CCCCCC","#3e9c15","blue")) |>
        ggplot() +
        geom_col(mapping = aes(x = DayOfWeek, y = num_sold,fill = color)) +
        geom_hline(aes(yintercept = Mean),color = "red",lwd = 1) +
        scale_fill_identity() +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.y =element_text(size=14,margin = margin(0,10,0,0)),
              panel.background = element_rect(fill = NA),
              axis.ticks = element_blank()) +
        labs(x = "",
             y = "Books sold")
      
    })
    
    output$yearseas <- renderPlot({
      
        ReactiveData() |>
        mutate(Month = month(date,label = T,abbr = T)) |>
        mutate(Year = year(date)) |>
        group_by(Year,Month) |>
        summarise(num_sold = mean(num_sold),.groups = "drop") |>
        ungroup() |>
        ggplot() +
        geom_line(mapping = aes(x=Month,y=num_sold,group = as.factor(Year),color = as.factor(Year)),lwd=1) +
        labs(x = "",
             y = "Books sold",
             color = "Year") +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.y =element_text(size=14,margin = margin(0,10,0,0)),
              legend.position = "bottom",
              panel.background = element_rect(fill = NA),
              axis.ticks = element_blank()) 
        
      
    })
    
  })
}