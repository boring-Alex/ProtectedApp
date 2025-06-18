RegistrationUi<-function(theme, id = "Registration"){
  ns<-NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4("Выберите разбор или введите новый"),
        fluidRow(
          column(10, selectInput(ns("ChooseTaskNum"),
                                 NULL,
                                 choices = c("<Новый>","Гепы1","Гепы2"))),
          column(2, LightButton(ns("AddNewTask"), "", icon("plus")))
        ),
        selectInput(ns("SpecimenType"),
                    "Исследования ИФА",
                    choices = c("Гепатиты", "Микрореакция", "Корь")),
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
        fluidRow(column(4, DangerButton(ns("DeleteButton"),
                                        "Удалить",
                                        icon("trash"),
                                        explanation = "Удалить не закрытые записи")),
                 column(4,SuccessButton(ns("SaveToDbButton"),
                                        "Принять",
                                        icon("save"),
                                        explanation = "Утвердить"))),
        fluidRow(
          column(12, DTOutput(ns("SearchResult")))
        )
      )
    )
  )
}