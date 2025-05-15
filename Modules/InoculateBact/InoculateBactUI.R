InoculateBactUI<-function(theme, id = "InoculateBact"){
  ns<-NS(id)
  tagList(
    accordion(
      accordion_panel(
        title = "Потребности в расходных материалах",
        icon = bsicons::bs_icon("geo"),
        fluidPage(
          theme = theme,
          fluidRow(
            column(11, DTOutput(ns("DishesTable"))),
            column(1, PrimaryButton(ns("UpdateDishes"), "Обновить"))
          )
        )
      ),
      accordion_panel(
        title = "",
        icon = bsicons::bs_icon("table"),
        fluidPage(
          theme = theme,
          fluidRow(
            column(11, DTOutput(ns("SpecimensTable"))),
            column(1,
                   PrimaryButton(ns("UpdateSpecimens"), "Обновить"),
                   SecondaryButton(ns("AxToClipboard"), "Копировать"))
          )
        )
      )
    )
  )
}