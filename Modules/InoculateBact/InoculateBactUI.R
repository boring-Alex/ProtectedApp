InoculateBactUI<-function(theme, id = "InoculateBact"){
  ns<-NS(id)
  tagList(
    rclipboardSetup(),
    accordion(
      PrimaryButton(ns("Update"), "Обновить данные", icon("refresh")),
      accordion_panel(
        title = "Потребности в расходных материалах",
        icon = bsicons::bs_icon("save"),
        fluidPage(
          theme = theme,
          fluidRow(
            column(12, DTOutput(ns("dishesRequest")))
          )
        )
      ),
      accordion_panel(
        title = "Принятые заявки на посев",
        icon = bsicons::bs_icon("database-fill"),
        fluidPage(
          theme = theme,
          fluidRow(
            column(11, DTOutput(ns("SpecimensTable"))),
            column(1,
                   uiOutput(ns("AxToClipboard")),
                   div(style = "height:2.5px"),
                   SuccessButton(ns("CompleteTask"),
                                 "",
                                 icon("thumbs-up")))
          )
        )
      )
    )
  )
}