AcceptBactUI<-function(theme, id = "AcceptBact"){
  ns<-NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("SpecimenType"),
                    "Тип образца",
                    choices = SpecTypesTable$Name),
        numericInput(ns("SpecNum"),
                     "Следующий номер",
                     value = 1,
                     min = 1),
        span(textOutput(ns("SpNumAlert"),inline = TRUE), style="color:red"),
        textInput(ns("AxaptaCode"),
                  "Код Axapta",
                  placeholder = "M0000000"),
        span(textOutput(ns("AxAlert"),inline = TRUE), style="color:red"),
        textInput(ns("SpecimenCode"),
                  "Код образца из отделения",
                  placeholder = "000000000000"),
        span(textOutput(ns("SpCodeAlert"),inline = TRUE), style="color:red"),
        PrimaryButton(ns("AddSampleButton"),
                     "Добавить образец")
      ),
      mainPanel(
        fluidRow(
          column(10, DTOutput(ns("SearchResult"))),
          column(2,
                 DangerButton(ns("DeleteButton"),
                              "Удалить"),
                 div(style = "height:2.5px"),
                 SuccessButton(ns("SaveToDbButton"),
                               "На посев"))
        ),
      )
    )
  )
}