box::use(
  bslib[card,card_header,card_body_fill]
)

box::use(
  app/view/CountrySelect,
  app/view/ProductSelect,
  app/view/StoreSelect
)



#' @export
card1 <- function(countryargument,productargument,storeargument)
  
  card(
  height = 400,
  card_header("Controls"),
  card_body_fill(CountrySelect$selection(countryargument),
                 ProductSelect$selection(productargument),
                 StoreSelect$selection(storeargument))
  
)

#' @export
card2 <- function (titleargument,plotargument)
  
  card(
  height = 400,
  full_screen = TRUE,
  card_header(titleargument),
  card_body_fill(plotargument)
  
)


