box::use(
  bslib[value_box],
  bsicons[bs_icon],
  shiny[p]
)


#' @export
card1 <- function(){
  
  value_box(
  title = "Countries",
  value = "6",
  showcase = bs_icon("globe-europe-africa"),
  p("Belgium, France, Germany, Italy, Spain, Poland")
)
  
}

#' @export
card2 <- function(){
  
  value_box(
  title = "Stores",
  value = "2",
  showcase = bs_icon("shop"),
  p("KaggleRama, KaggleMart")
)

}  
  
#' @export
card3 <- function() {
  
  value_box(
  title = "Products",
  value = "4",
  showcase = bs_icon("journal-bookmark"),
  p("Advanced Techniques, Getting Started, Recipe Book, For Kids: One Smart Goose")
)
  
}

#' @export
card4 <- function() {
  
  value_box(
  title = "Unique Time Series",
  value = "48",
  showcase = bs_icon("graph-up-arrow")
)
  
}






