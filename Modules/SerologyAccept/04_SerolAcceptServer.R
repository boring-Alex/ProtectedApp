SerolAcceptServer<-function(theme, id = "SerolAccept"){
  moduleServer(
    id, function(input, output, session){
      ns<-NS(id)
      serolNums<-reactiveVal(LoadDbData(appData, "SerolNumbers"))
      tasksNumbers<-reactiveVal(c("<Новый>",LoadTasksNums(appData)[,1]))
      acceptedData<-reactiveVal(LoadSerology(appData,Sys.Date()))
      currTaskData<-reactiveVal(NULL)
      currentShownData<-reactiveVal(NULL)
      lastSelectedType<-reactiveVal("Гепатиты")
      lastSelectedTask<-reactiveVal("<Новый>")
      axCodes<-reactiveVal(LoadSerologyByStatus(appData,status = FALSE)[,1])
      changedCodeGroups<-reactiveVal(NULL)
      errorMessage<-reactiveVal(as.character())
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
        usedNums<-rbind(LoadUsedAxNums(appData, "SerolAccepted"),acceptedData()[,1])
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
        usedNums<-rbind(LoadUsedBarcodes(appData, "SerolAccepted"),acceptedData()[,2])
        if(input$SpecimenCode %in% usedNums[,1]){
          errorMsg <- "Код направления уже использовался"
          errorMessage(c(errorMessage(),errorMsg))
        }
      }
      emptyTask<-function(){
        errorMsg <- ""
        if(input$ChooseTaskNum == "<Новый>"){
          errorMsg <- "Неверный номер разбора"
          errorMessage(c(errorMessage(),errorMsg))
        }
      }
      updateChangedGroups<-function(code){
        newValue<-code
        if(is.null(changedCodeGroups())){
          changedCodeGroups(newValue)
        }else{
          vals<-c(changedCodeGroups(), newValue) %>% unique()
          changedCodeGroups(vals)
        }
      }
      updateSpecimenNumbers<-function(nextCode){
        serolNums({
          tmp<-serolNums()
          tmp[tmp$SpecimenType == input$SpecimenType,2]<-nextCode
          tmp
        })
        updateNumericInput(session, "SpecNum", value = nextCode)
        updateChangedGroups(input$SpecimenType)
      }
      updateDbNumbers<-function(){
        groupsToSave<-changedCodeGroups()
        if(is.null(groupsToSave)){
          return()
        }
        UpdateSerolTypeMaxNum(groupsToSave, serolNums(), appData)
        changedCodeGroups(NULL)
      }
      newTaskMd<-modalDialog(
        textInput(ns("NewTaskName"),"Новый разбор", width= "100%"),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          SuccessButton(ns("AddNewTaskName"), "Ok")
        )
      )
      observe({
        updateSelectInput(session, "ChooseTaskNum",
                          choices = tasksNumbers(),
                          selected = lastSelectedTask())
      })
      observe({
        updateSelectInput(session, "SpecimenType",
                          choices = serolNums()$SpecimenType,
                          selected = lastSelectedType())
      })
      observeEvent(input$SpecimenType,{
        lastSelectedType(input$SpecimenType)
        currVal<-serolNums()[serolNums()$SpecimenType == input$SpecimenType, 2]
        updateNumericInput(session, "SpecNum", value = currVal)
      })
      observeEvent(input$AddNewTaskName,{
        tmp<-tasksNumbers()
        tasksNumbers(c(tmp, input$NewTaskName))
        lastSelectedTask(input$NewTaskName)
        removeModal(session)
      })
      observeEvent(input$AddSampleButton, {
        emptyTask()
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
                    FALSE,
                    input$ChooseTaskNum,
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
      observeEvent(input$ChooseTaskNum,{
        lastSelectedTask(input$ChooseTaskNum)
        if(input$ChooseTaskNum != "<Новый>"){
          output$selectedTaskLocator = renderText({paste("Разбор №", input$ChooseTaskNum)})
        }
      })
      observeEvent(input$AddNewTask,{
        showModal(newTaskMd)
      })
      output$SearchResult<-renderDT({
        tab<-acceptedData() %>% filter(TaskName == input$ChooseTaskNum & Type == input$SpecimenType)
        currentShownData(tab)
        tab<-tab[,-c(6:7)]
        dates<-as.POSIXct(as.numeric(tab$AccDate), origin = "1970-01-01")
        tab$AccDate<-strftime(dates,format = "%d.%m.%Y %H:%M")
        icoList<-character()
        vialAccepted<-character()
        vialIco<-character()
        cNames<-c("Номер Аксапта",
                  "Код образца",
                  "Дата поступления",
                  "Группа",
                  "Журнальный номер",
                  "Пробирка найдена")
        if(nrow(tab) > 0){
          for(i in 1:nrow(tab)){
            if(tab[i,1] %in% axCodes()){
              icoList<-c(icoList, as.character(icon("lock", lib = "font-awesome")))
            }else{
              icoList<-c(icoList, as.character(icon("lock-open", lib = "font-awesome")))
            }
            if(tab[i, 6] == 1){
              vialIco<-as.character(icon("vial-circle-check", style = "color: rgb(120, 235, 122);"))
            }else{
              vialIco <- as.character(icon("file-contract", style = "color: rgb(18, 49, 204);"))
            }
            tab[i, 6] <- vialIco
          }
          tab$icons<-icoList
          cNames<-c(cNames, "Статус")
        }
        colnames(tab)<-cNames
        dt<-DT::datatable(tab, selection = "single",
                          escape = FALSE,
                          options = list(ordering = FALSE,
                                         language = ruDT,
                                         dom = 't'))
        dt
      })
      observeEvent(input$DeleteButton, {
        tryCatch({
          selectedRow<-input$SearchResult_rows_selected
          if(length(selectedRow) == 0){
            InfoAlert("Предупреждение", "строки не вырбаны, выберите одну")
            return()
          }
          rowToDelete<-currentShownData()[selectedRow,]
          if(rowToDelete[1,1] %in% axCodes()){
            InfoAlert("Предупреждение", "нельзя удалять уже принятые результаты")
            return()
          }
          tmp<-acceptedData()
          codeToDelete<-rowToDelete[1,"AxaptaCode"]
          typeToDelete<-rowToDelete[1,"Type"]
          numToDelete<-as.numeric(rowToDelete[1,"CurrentNum"])
          tmp<-tmp %>% filter(AxaptaCode != codeToDelete)
          cNames<-1:nrow(tmp)
          nextC<-as.numeric(numToDelete)
          if(nrow(tmp) > 0){
            for(i in 1:nrow(tmp)){
              if(tmp$Type[i] == typeToDelete && tmp$CurrentNum[i] > numToDelete){
                tmp[i,]$CurrentNum = as.numeric(tmp[i,]$CurrentNum) - 1
              }
            }
          }
          if(nrow(tmp) > 1){ rownames(tmp) <- cNames }
          groupTab<-tmp %>% filter(Type == input$SpecimenType)
          if(nrow(groupTab)>0){
            nextC<-as.numeric(max(groupTab$CurrentNum)) + 1
          }
          updateSpecimenNumbers(nextC)
          acceptedData(tmp)
          currentShownData({
            acceptedData() %>% filter(Type == input$SpecimenType & TaskName == input$ChooseTaskNum)
          })
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
          AddSerolAcceptedRecord(datToSave, appData)
          newDat<-LoadSerology(appData,Sys.Date())
          acceptedData(newDat)
          axCodes(newDat$AxaptaCode)
          updateDbNumbers()
        }
        else{
          InfoAlert("Информация", "новых данных не введено")
        }
      })
      observe({
        if(is.null(currTaskData())){return()}
        updateDataView<-function(record, firstNum){
          if(nrow(record) == 0){
            output$selectedAxNum = renderText({""})
            output$selectedType = renderText({""})
            output$selectedSpecNum = renderText({""})
            output$selectedPlateNum = renderText({""})
            return()
          }
          plateNum <- GetTubePlateNum(firstNum, record$CurrentNum[1], input$rowsNum, input$colsNum)
          output$selectedAxNum = renderText({paste("Номер аксапта:", record$AxaptaCode[1])})
          output$selectedType = renderText({paste("Тип исследования:", record$Type[1])})
          output$selectedSpecNum = renderText({paste("Номер журнальный:", record$CurrentNum[1])})
          output$selectedPlateNum = renderText({paste("Номер штатива:", plateNum)})
          output$AcceptVial <-renderUI({
            tagList(
              SecondaryButton(ns("AcceptVialBttn"),
                              "Установить пробирку")
            )
          })
        }
        firstNum<-min(currTaskData()$CurrentNum)
        selectedRecord <- currTaskData()[currTaskData()$SampleCode == input$VialCode,]
        updateDataView(selectedRecord, firstNum)
        if(nrow(selectedRecord) == 0){
          output$TestPlate<-renderDT({NULL})
          return()
        }
        targetPlateNum <- GetTubePlateNum(firstNum, selectedRecord$CurrentNum[1], input$rowsNum, input$colsNum)
        smls<-GetPlateNums(currTaskData(),selectedRecord$Type[1], input$rowsNum, input$colsNum, targetPlateNum)
        output$TestPlate<-PlateShower(input$rowsNum, input$colsNum, smls, selectedRecord$CurrentNum[1])
      })
      observeEvent(input$VialCode,{
        if(input$VialCode == ""){
          return()
        }
        temp<-acceptedData() %>% filter(TaskName == input$ChooseTaskNum)
        errorMsg <- ""
        if(!stri_detect(input$VialCode, regex = "^\\d{12}$")){
          errorMsg <- "Неверный формат номера направления"
          errorMessage(c(errorMessage(),errorMsg))
          for(err in errorMessage()){
            showNotification(err, duration = 5, type = "error")
          }
          errorMessage(as.character())
          return()
        }
        if(!input$VialCode %in% temp$SampleCode){
          errorMsg <- paste("Такого направления нет в разборе",input$ChooseTaskNum)
          errorMessage(c(errorMessage(),errorMsg))
          for(err in errorMessage()){
            showNotification(err, duration = 5, type = "error")
          }
          if(input$VialCode %in% acceptedData()$SampleCode){
            taskNum<-acceptedData()[acceptedData()$SampleCode == input$VialCode,]$TaskName[1]
            errorMsg <- paste("Такое направление находится в разборе",taskNum)
            showNotification(errorMsg, duration = 5, type = "warning")
          }
          errorMessage(as.character())
          return()
        }
        currTaskData(temp)
      })
      observeEvent(input$AcceptVialBttn,{
        tryCatch({
          dat<-acceptedData()
          dat[dat$SampleCode == input$VialCode,"HasVial"] <- TRUE
          acceptedData(dat)
          currTaskData(acceptedData() %>% filter(TaskName == input$ChooseTaskNum))
          recUpdated<-AddVialToDb(input$VialCode,"SerolAccepted",appData)
          msg<-"БД обновится при сохранении разбора"
          if(recUpdated>0){
            msg<-paste("Обновлено записей в БД:", recUpdated)
          }
          SuccessAlert("Пробирка установлена",msg)
        },
        error = function(cond){
          ErrorAlert("Ошибка!", conditionMessage(cond))
        },
        warning = function(cond){
          WarningAlert("Предупреждение", conditionMessage(cond))
        })
        dat<-acceptedData()
        dat[dat$SampleCode == input$VialCode,"HasVial"] <- TRUE
        acceptedData(dat)
        currTaskData(acceptedData() %>% filter(TaskName == input$ChooseTaskNum))
      })
    })
}