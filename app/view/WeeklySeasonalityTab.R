box::use(
  app/view/WeekSeasTabComponents/WeekSeasCards,
  app/logic/data
)

box::use(
  shiny[NS,moduleServer,tabPanel,fluidRow,column,plotOutput,renderPlot],
  dplyr[mutate,filter,group_by,summarise,ungroup],
  ggplot2[ggplot,geom_col,aes,geom_hline,labs,facet_wrap,theme,
          element_text,element_rect,scale_fill_identity,element_blank],
  lubridate[wday]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Weekly Seasonality",
           fluidRow(
             column(3,
                    WeekSeasCards$card1(ns("product_select"))
             ),
             column(9,
                    WeekSeasCards$card2("Weekly Seasonality By Each Product",plotOutput(ns("weekseas")))
             )
           )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$weekseas <- renderPlot({
      
        data$fetch_data() |>
        mutate(date = as.Date(date)) |>
        filter(product == input$product_select)|>
        mutate(DayOfWeek = wday(date,label = T,abbr = T)) |>
        group_by(country,store,DayOfWeek) |>
        summarise(num_sold = mean(num_sold)) |>
        mutate(Mean = mean(num_sold)) |>
        ungroup() |>
        mutate(color = ifelse(DayOfWeek == "Sun" | DayOfWeek == "Sat","blue",
                              ifelse(DayOfWeek == "Fri","#3e9c15","#CCCCCC")))|>
        ggplot() +
        geom_col(mapping = aes(x = DayOfWeek, y = num_sold,fill = color)) +
        geom_hline(aes(yintercept = Mean),color = "red",lwd = 1) +
        labs(title = input$product_select,
             y = "Books Sold",
             x = "") +
        theme(axis.text.x = element_text(size = 10),
              legend.position="none",
              axis.ticks = element_blank(),
              strip.text = element_text(
                size = 13,color = "black"
              ),
              strip.background = element_rect(
                color = "black",fill = "white",linetype = "solid"
              ),
              panel.background = element_rect(fill = NA),
              plot.title = element_text(size = 18)) +
        facet_wrap(.~ country + store,ncol=3,scales = "free") +
        scale_fill_identity() 
      
    })
  })
}
