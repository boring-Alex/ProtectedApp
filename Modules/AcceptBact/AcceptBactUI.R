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
        textInput(ns("AxaptaCode"),
                  "Код Axapta",
                  placeholder = "M0000000"),
        textInput(ns("SpecimenCode"),
                  "Код образца из отделения",
                  placeholder = "000000000000"),
        PrimaryButton(ns("AddSampleButton"),
                     "Добавить образец")
      ),
      mainPanel(
        fluidRow(
          column(10, DTOutput(ns("SearchResult"))),
          column(2,
                 DangerButton(ns("DeleteButton"),
                              "Удалить",
                              icon("trash"),
                              explanation = "Удалить не закрытые записи"),
                 div(style = "height:2.5px"),
                 SuccessButton(ns("SaveToDbButton"),
                               "На посев",
                               icon("save"),
                               explanation = "Утвердить и отправить на посев"))
        )
      )
    )
  )
}