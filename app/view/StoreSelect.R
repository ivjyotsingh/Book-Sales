box::use(
  app/logic/data
)

box::use(
  dplyr[select,pull],
  shiny[selectInput]
)


#' @export
selection <- function(storeargument){
  
  data$fetch_data() |>
    select(store) |>
    unique() |>
    pull() -> stores
  
  selectInput(inputId = storeargument,
              "Select a store",
              choices = stores,
              selected = 1)
  
}

