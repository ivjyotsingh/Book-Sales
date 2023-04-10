box::use(
  app/logic/data,
  app/logic/forecast
)

box::use(
  app/view/SingleTsCards
)

box::use(
  shiny[NS,tabPanel,fluidRow,column,plotOutput,moduleServer,renderPlot,reactive],
  dplyr[filter,select,mutate,pull,rename],
  tsibble[as_tsibble],
  fabletools[features],
  prophet[prophet,add_seasonality,add_regressor,fit.prophet,make_future_dataframe],
  ggplot2[ggplot,geom_line,aes]
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Forecasting",
           fluidRow(
             column(3,
                    SingleTsCards$card1(ns("country_select"),ns("product_select"),ns("store_select"))
             ),
             column(9,
                    SingleTsCards$card2("Forecast of a single time series",plotOutput(ns("singleforecast")))
             )
           )
         ) 
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    output$singleforecast <- renderPlot({
    
    lambda_df <- reactive({
      
        data$fetch_data() |>
        filter(country == input$country_select &
                 store == input$store_select &
                 product == input$product_select) |>
        select(date,num_sold) |>
        mutate(date = as.Date(date)) |>
        as_tsibble(index = date)
      
    })
    
    lambda <- reactive({
      
        lambda_df() |>
        features(num_sold,features = guerrero) |>
        pull(lambda_guerrero)
      
    })
    
    df <- reactive({
      
      data$fetch_data() |>
        filter(country == input$country_select &
                 store == input$store_select &
                 product == input$product_select) |>
        select(date,num_sold) |>
        mutate(num_sold = ((num_sold^lambda()-1)/lambda())) |>
        mutate(date = as.Date(date)) |>
        rename(ds = date) |>
        rename(y = num_sold) |>
        mutate(post_covid = forecast$diff_wseason(ds)) |>
        mutate(lockdown_end = !forecast$diff_wseason(ds)) |>
        mutate(year_end = forecast$anomaly_function(ds)) 
      
    }) 
    
    
    m <- prophet(holidays = forecast$fetch_covid(),
                 weekly.seasonality = FALSE,
                 daily.seasonality = FALSE) 
    
    m <- add_seasonality(m,
                         name='weekly_post_covid',
                         period=7,
                         fourier.order=3,
                         condition.name='post_covid')
    
    m <- add_seasonality(m,
                         name='weekly_lockdown_end',
                         period=7,
                         fourier.order=3,
                         condition.name='lockdown_end')
    
    m <- add_regressor(m,"year_end")
    
    m <-  reactive({
      
                    fit.prophet(m, df())
      
                  })
    
    future  <-  reactive({
                make_future_dataframe(m(),periods = 365)
    })
    
    
    future <-  reactive({
      
      future() |>
      mutate(post_covid = forecast$diff_wseason(ds)) |>
      mutate(lockdown_end = !forecast$diff_wseason(ds)) |>
      mutate(year_end = forecast$anomaly_function(ds))
    
    })
      
    forecast <- reactive({
      predict(m(),future())
    })
    
    
    forecast <- reactive({
      
      forecast() |>
      mutate(Year = year(ds)) |>
      filter(Year == 2021) |>
      select(ds,yhat) |>
      mutate(yhat = ((lambda()*yhat+1)^(1/lambda())))
      
    })
    
    
        forecast() |>
          ggplot() +
          geom_line(mapping = aes(x = ds, y = yhat))
        
        
    })
    
    
  })
}