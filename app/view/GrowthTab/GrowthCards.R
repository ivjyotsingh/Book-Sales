box::use(
  bslib[card,card_header,card_body_fill]
)

box::use(
  app/view/CountrySelect
)

#' @export
card1 <- function(countryargument){
  card(
    height = 360,
    card_header("Control"),
    full_screen = TRUE,
    card_body_fill(CountrySelect$selection(countryargument))
  )
}


#' @export
card2 <- function(titleargument,plotargument) {
  card(
    height = 800,
    full_screen = TRUE,
    card_header(titleargument),
    card_body_fill(plotargument)
  )
}