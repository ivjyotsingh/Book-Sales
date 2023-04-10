box::use(
  shiny[navbarPage, moduleServer, NS],
  bslib[bs_theme,bs_themer],
  thematic[thematic_shiny]
)

box::use(
  app/view/singlets,
  app/view/StaticTab,
  app/view/GrowthTab,
  app/view/YearlySeasonalityTab,
  app/view/WeeklySeasonalityTab,
  app/view/ForecastTab
)

#' @export
ui <- function(id) {
  thematic::thematic_shiny()
  ns <- NS(id)
  navbarPage(
    "Sale of Books",
    theme = bs_theme(bootswatch = "zephyr"),
    StaticTab$ui(ns("statictab")),
    singlets$ui(ns("trendtab")),
    GrowthTab$ui(ns("growthtab")),
    YearlySeasonalityTab$ui(ns("yearseas")),
    WeeklySeasonalityTab$ui(ns("weekseas")),
    ForecastTab$ui(ns("forecasting"))
    
  )
}

#' @export
server <- function(id) {
  #bslib::bs_themer()
  moduleServer(id, function(input, output, session) {
    singlets$server("trendtab")
    StaticTab$server("statictab")
    GrowthTab$server("growthtab")
    YearlySeasonalityTab$server("yearseas")
    WeeklySeasonalityTab$server("weekseas")
    ForecastTab$server("forecasting")
    
  })
}
