InoculateBactServer<-function(theme, id = "InoculateBact"){
  moduleServer(
    id,
    function(input, output, session){
      ns<-NS(id)
      rowCallback <- JS(
        'function(row, data, displayIdx, node){',
        '   $(row).attr("data-index", data[data.length-1]);', # последнее значение - это originalRowNum
        '}'
      )
      dishesData<-reactiveVal(CountDishes())
      specimensData<-reactiveVal(LoadBacterilogyByStatus(appData, FALSE))
      selectedAxNum<-reactive({
        selectedData <- input$SpecimensTable_rows_selected
        resAx <- ""
        if(!is.null(selectedData)){
          resAx <- specimensData()[selectedData,1]
        }
        resAx
      })
      output$dishesRequest<-renderDT({
        sumData<-dishesData()
        dt<-DT::datatable(sumData,
                          selection = "single",
                          escape = FALSE,
                          options = list(ordering = TRUE,
                                         language = ruDT,
                                         dom = 't'))
        dt
      })
      output$SpecimensTable<-renderDT({
        tab<-specimensData()[,-6]
        dates<-as.POSIXct(as.numeric(tab$AccDate), origin = "1970-01-01")
        tab$AccDate<-strftime(dates,format = "%d.%m.%Y %H:%M")
        colnames(tab)<-c("Номер Аксапта",
                         "Код образца",
                         "Дата поступления",
                         "Группа",
                         "Журнальный номер")
        DT::datatable(tab,
                      selection = "single",
                      options = list(ordering = FALSE,
                                     language = ruDT,
                                     columnDefs = list(list(targets = ncol(tab), visible = FALSE)), # скрываем колонку с индексами
                                     rowCallback = rowCallback))
      })
      output$AxToClipboard<-renderUI({
        rclipButton(
          inputId = ns("clipbtn"),
          label = "",
          clipText = selectedAxNum(), 
          icon = icon("clipboard"),
          tooltip = "Копировать номер для Axapta",
          style = "width: 100%",
          options = list(delay = list(show = 800, hide = 100), trigger = "hover")
        )
      })
      observeEvent(input$Update, {
        tryCatch({
          dishesData(CountDishes())
          specimensData(LoadBacterilogyByStatus(appData, FALSE))
          SuccessAlert("Данные обновлены", Sys.time())
        },
        error = function(cond){
          ErrorAlert("Ошибка!", conditionMessage(cond))
        },
        warning = function(cond){
          WarningAlert("Предупреждение", conditionMessage(cond))
        })
      })
      observeEvent(input$clipbtn, {
        if(selectedAxNum() == ""){
          WarningAlert("Внимание!","строка не выбрана")
          return()
        }
        tryCatch({
          SuccessAlert("Скопировано", "")
        },
        error = function(cond){
          ErrorAlert("Ошибка!", conditionMessage(cond))
        },
        warning = function(cond){
          WarningAlert("Предупреждение", conditionMessage(cond))
        })
      })
    }
  )
}