box::use(
  app/logic/data
)

box::use(
  dplyr[select,pull],
  shiny[selectInput]
)


#' @export
selection <- function(productargument){
  
  data$fetch_data() |>
    select(product) |>
    unique() |>
    pull() -> products
  
  selectInput(inputId = productargument,
              "Select a Product",
              choices = products,
              selected = 1)
  
}

