box::use(
  utils[read.csv]
)

box::use(
  here
)


#' @export
fetch_data <- function(){
  read.csv(here::here("Data","train.csv"))
}

#' @export
fetch_test <- function(){
  read.csv(here::here("Data","test.csv"))
}

#' @export
fetch_sbc <- function(){
  read.csv(here::here("Data","sbc.csv"))
}

#' @export
fetch_ysbp <- function(){
  read.csv(here::here("Data","ysbp.csv"))
}

#' @export
fetch_forecast <- function(){
  read.csv(here::here("Data","forecast.csv"))
}