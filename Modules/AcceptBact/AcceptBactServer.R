AcceptBactServer<-function(myTheme, id = "AcceptBact"){
  moduleServer(
    id,
    function(input, output, session){
      ns<-NS(id)
      acceptedData<-reactiveVal(LoadBacterilogyByStatus(appData,status = FALSE))
      typesAndNums<-reactiveVal(SpecTypesTable)
      changedCodeGroups<-reactiveVal(NULL)
      errorMessage<-reactiveVal(as.character())
      axCodes<-reactiveVal(LoadBacterilogyByStatus(appData,status = FALSE)[,1])
      wrongNum<-function(){
        errorMsg <- ""
        if(input$SpecNum <= 0){
          errorMsg <- "Номер не может равняться нулю или быть отрицательным"
          errorMessage(c(errorMessage(),errorMsg))
        }
      }
      wrongAx<-function(){
        errorMsg<-""
        if(!stri_detect(input$AxaptaCode, regex = "^M\\d{7}")){
          errorMsg <- "Неверный формат номера Аксапта"
          errorMessage(c(errorMessage(),errorMsg))
        }
        usedNums<-rbind(LoadUsedAxNums(appData, "BactAccepted"),acceptedData()[,1])
        if(input$AxaptaCode %in% usedNums[,1]){
          errorMsg <- "Номер Аксапта уже использовался"
          errorMessage(c(errorMessage(),errorMsg))
        }
      }
      wrongSpCode<-function(){
        errorMsg <- ""
        if(!stri_detect(input$SpecimenCode, regex = "^\\d{12}$")){
          errorMsg <- "Неверный формат номера направления"
          errorMessage(c(errorMessage(),errorMsg))
        }
        usedNums<-rbind(LoadUsedBarcodes(appData, "BactAccepted"),acceptedData()[,2])
        if(input$SpecimenCode %in% usedNums[,1]){
          errorMsg <- "Код направления уже использовался"
          errorMessage(c(errorMessage(),errorMsg))
        }
      }
      updateChangedGroups<-function(){
        newValue<-input$SpecimenType
        if(is.null(changedCodeGroups())){
          changedCodeGroups(newValue)
        }else{
          vals<-c(changedCodeGroups(), newValue) %>% unique()
          changedCodeGroups(vals)
        }
      }
      updateSpecimenNumbers<-function(nextCode){
        typesAndNums({
          tmp<-typesAndNums()
          tmp[tmp$Name == input$SpecimenType,2]<-nextCode
          tmp
        })
        updateNumericInput(session, "SpecNum", value = nextCode)
        updateChangedGroups()
      }
      updateDbNumbers<-function(){
        groupsToSave<-changedCodeGroups()
        if(is.null(groupsToSave)){
          return()
        }
        UpdateSpecimenMaxNum(groupsToSave, typesAndNums(), appData)
        changedCodeGroups(NULL)
      }
      observeEvent(input$AddSampleButton, {
        wrongNum()
        wrongAx()
        wrongSpCode()
        if(length(errorMessage()) > 0){
          for(err in errorMessage()){
            showNotification(err, duration = 5, type = "error")
          }
          errorMessage(as.character())
          return()
        }
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
        updateTextInput(session, "SpecimenCode", value = "")
      })
      output$SearchResult<-renderDT({
        tab<-acceptedData()[,-6]
        dates<-as.POSIXct(as.numeric(tab$AccDate), origin = "1970-01-01")
        tab$AccDate<-strftime(dates,format = "%d.%m.%Y %H:%M")
        icoList<-character()
        cNames<-c("Номер Аксапта",
                  "Код образца",
                  "Дата поступления",
                  "Группа",
                  "Журнальный номер")
        if(nrow(tab) > 0){
          for(i in 1:nrow(tab)){
            if(tab[i,1] %in% axCodes()){
              icoList<-c(icoList, as.character(icon("lock", lib = "font-awesome")))
            }else{
              icoList<-c(icoList, as.character(icon("lock-open", lib = "font-awesome")))
            }
          }
          tab$icons<-icoList
          cNames<-c(cNames, "Статус")
        }
        colnames(tab)<-cNames
        dt<-DT::datatable(tab, selection = "single",
                          escape = FALSE,
                          options = list(ordering = FALSE,
                                        language = ruDT,
                                        dom = 't',
                                        paging = FALSE,
                                        scrollX = TRUE))
        dt
        })
      observeEvent(input$SpecimenType,{
        currVal<-typesAndNums()[typesAndNums()$Name == input$SpecimenType, 2]
        updateNumericInput(session, "SpecNum", value = currVal)
      })
      observeEvent(input$DeleteButton, {
        tryCatch({
          selectedRow<-input$SearchResult_rows_selected
          if(length(selectedRow) == 0){
            InfoAlert("Предупреждение", "строки не вырбаны, выберите одну")
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
          updateDbNumbers()
        }
        else{
          InfoAlert("Информация", "новых данных не введено")
        }
      })
    }
  )
}