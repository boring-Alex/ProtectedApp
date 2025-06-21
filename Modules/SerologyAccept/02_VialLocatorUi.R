VialLocatorUi<-function(theme, id = "VialLocator"){
  ns<-NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        span(textOutput(ns("selectedTaskLocator"),inline = TRUE), style="color:blue"),
        accordion(accordion_panel("Настройки штатива",
                                  numericInput(ns("rowsNum"), "Количество рядов", min = 1, value = 4),
                                  numericInput(ns("colsNum"), "Количество пробирок в ряду", min = 1, value = 8))),
        textInput(ns("VialCode"),
                  "Штрих код пробирки",
                  placeholder = "000000000000")
      ),
      mainPanel(h4(icon("file-contract", style = "color: rgb(18, 49, 204);"), "- есть направление  ",
                   icon("vial", style = "color: rgb(145, 28, 45);"), "- пробирку ставить сюда  ",
                   icon("vial-circle-check", style = "color: rgb(120, 235, 122);"), "- пробирка есть  "),
                span(textOutput(ns("selectedAxNum"),inline = TRUE)),
                div(style = "height:2.5px"),
                span(textOutput(ns("selectedType"),inline = TRUE)),
                div(style = "height:2.5px"),
                span(textOutput(ns("selectedSpecNum"),inline = TRUE)),
                div(style = "height:2.5px"),
                span(textOutput(ns("selectedPlateNum"),inline = TRUE)),
                div(style = "height:2.5px"),
                uiOutput(ns("AcceptVial")),
                DTOutput(ns("TestPlate")))
    )
  )
}