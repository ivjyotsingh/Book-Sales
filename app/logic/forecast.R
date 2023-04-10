box::use(
  tibble[data_frame],
  lubridate[year,month,day]
)



#' @export

fetch_covid <- function(){
  
             data_frame(holiday = "Covid",
                        ds = as.Date(c("2020-02-01")),
                        lower_window = 0,
                        upper_window = 182)
             
}

#' @export
diff_wseason <- function(ds) {
  
  Year <- year(ds)
  Month <- month(ds)
  as.numeric((Month > 7 & Year == 2020) | (Year == 2021))
  
}


#' @explort
anomaly_function <- function(ds){
  
  Day <- day(ds)
  Month <- month(ds,abbr = T,label = T)
  as.numeric(
    
    ((Day == 27 | Day == 28 | Day == 29 | Day == 30 | Day == 31) & (Month == "Dec"))
    |
      ((Day == 1) & (Month == "Jan")))
  
}