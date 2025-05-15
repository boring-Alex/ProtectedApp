AcceptBactServer<-function(myTheme, id = "AcceptBact"){
  moduleServer(
    id,
    function(input, output, session){
      ns<-NS(id)
      acceptedData<-reactiveVal(LoadBacterilogyByStatus(appData,status = FALSE))
      typesAndNums<-reactiveVal(SpecTypesTable)
      axCodes<-reactiveVal(LoadBacterilogyByStatus(appData,status = FALSE)[1,])
      wrongNum<-function(){
        if(input$SpecNum <= 0){
          output$SpNumAlert <- renderText({"Номер не может равняться нулю или быть отрицательным"})
          return(TRUE)
        }
        output$SpNumAlert <- renderText(NULL)
        return(FALSE)
      }
      wrongAx<-function(){
        if(!stri_detect(input$AxaptaCode, regex = "^M\\d{7}$")){
          output$AxAlert <- renderText({"Неверный формат номера Аксапта"})
          return(TRUE)
        }
        output$AxAlert <- renderText({NULL})
        return(FALSE)
      }
      wrongSpCode<-function(){
        if(!stri_detect(input$SpecimenCode, regex = "^\\d{12}$")){
          output$SpCodeAlert <- renderText({"Неверный формат номера направления"})
          return(TRUE)
        }
        output$SpCodeAlert <- renderText(NULL)
        return(FALSE)
      }
      updateSpecimenNumbers<-function(nextCode){
        typesAndNums({
          tmp<-typesAndNums()
          tmp[SpecTypesTable$Name == input$SpecimenType,2]<-nextCode
          tmp
        })
        updateNumericInput(session, "SpecNum", value = nextCode)
        UpdateSpecimenMaxNum(input$SpecimenType, typesAndNums(), appData)
      }
      observeEvent(input$AddSampleButton, {
        if(wrongNum()){ return() }
        if(wrongAx()){ return() }
        if(wrongSpCode()){ return() }
        acceptedData({
          newRow<-c(input$AxaptaCode,
                  input$SpecimenCode,
                  Sys.time(),
                  input$SpecimenType,
                  input$SpecNum,
                  FALSE)
          cNames<-colnames(acceptedData())
          tmp<-rbind(acceptedData(), newRow)
          colnames(tmp)<-cNames
          tmp
          })
        nextC <- input$SpecNum + 1
        updateSpecimenNumbers(nextC)
        updateTextInput(session, "AxaptaCode", value = "")
        updateTextInput(session, "SpCodeAlert", value = "")
      })
      output$SearchResult<-renderDT({
        tab<-acceptedData()[,-6]
        dates<-as.POSIXct(as.numeric(tab$AccDate), origin = "1970-01-01")
        tab$AccDate<-strftime(dates,format = "%d.%m.%Y %H:%M")
        colnames(tab)<-c("Номер Аксапта",
                         "Код образца",
                         "Дата поступления",
                         "Группа",
                         "Журнальный номер")
        dt<-datatable(tab, selection = "single",
                  options = list(ordering = FALSE,
                                 language = ruDT,
                                 dom = 't'),
                  class = list(stripe = FALSE))
        if(length(axCodes())>0){
          dt <- dt %>% formatStyle(1,
                          target = "row",
                          backgroundColor = styleEqual(axCodes(), 'green4'))
        }
        dt
        })
      observeEvent(input$SpecimenType,{
        currVal<-typesAndNums()[typesAndNums()$Name == input$SpecimenType, 2]
        updateNumericInput(session, "SpecNum", value = currVal)
      })
      observeEvent(input$DeleteButton, {
        tryCatch({
          selectedRow<-input$SearchResult_rows_selected
          if(selectedRow == 0){
            return()
          }
          rowToDelete<-acceptedData()[selectedRow,]
          if(rowToDelete[1,1] %in% axCodes()){
            InfoAlert("Предупреждение", "нельзя удалять уже принятые результаты")
            return()
          }
          tmp<-acceptedData()[-selectedRow,]
          typeToDelete<-acceptedData()[selectedRow,"Type"]
          numToDelete<-acceptedData()[selectedRow,"CurrentNum"]
          cNames<-1:nrow(tmp)
          nextC<-as.numeric(numToDelete)
          if(nrow(tmp)>1){
            rownames(tmp)<-cNames
            for(i in 1:nrow(tmp)){
              if(tmp[i,]$Type == typeToDelete && tmp[i,]$CurrentNum > numToDelete){
                tmp[i,]$CurrentNum = as.numeric(tmp[i,]$CurrentNum) - 1
              }
            }
            nextC<-as.numeric(max(tmp$CurrentNum)) + 1
          }
          updateSpecimenNumbers(nextC)
          acceptedData(tmp)
          SuccessAlert("Успешно", "Данные удалены")
        },
        error = function(cond){
          ErrorAlert("Ошибка!", conditionMessage(cond))
        },
        warning = function(cond){
          WarningAlert("Предупреждение", conditionMessage(cond))
        })
        
      })
      observeEvent(input$SaveToDbButton,{
        datToSave<-acceptedData() %>% filter(!AxaptaCode %in% axCodes())
        if(nrow(datToSave) > 0){
          AddBactAcceptedRecord(datToSave, appData)
          newDat<-LoadBacterilogyByStatus(appData,status = FALSE)
          acceptedData(newDat)
          axCodes(newDat$AxaptaCode)
        }
        else{
          InfoAlert("Информация", "новых данных не введено")
        }
      })
    }
  )
}