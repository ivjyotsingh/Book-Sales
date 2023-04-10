box::use(
  app/view/ProductSelect
)

box::use(
  bslib[card,card_header,card_body_fill]
)

#' @export
card1 <- function(productargument){
  card(
    height = 330,
    card_header("Control"),
    card_body_fill(ProductSelect$selection(productargument))
  )
}

#' @export
card2 <- function(titleargument,plotargument) {
  card(
    height = 800,
    card_header(titleargument),
    card_body_fill(plotargument)
  )
}