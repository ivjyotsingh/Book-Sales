box::use(
  app/logic/data
)

box::use(
  dplyr[select,pull],
  shiny[selectInput]
)


#' @export
selection <- function(countryargument){
  
    data$fetch_data() |>
    select(country) |>
    unique() |>
    pull() -> countries
  
    selectInput(inputId = countryargument,
                "Select a country",
                choices = countries,
                selected = 1)
  
}



