#Подсчёт количества чашек для посева
CountDishes <- function(){
  analyses <- LoadBacterilogyByStatus(appData, FALSE)
  outputTab<-data.frame(Dish = character(), Amount = numeric())
  if(nrow(analyses) == 0){
    colnames(outputTab)<-c("Среда", "Количество")
    return(outputTab)
  }
  dishes <- LoadDbData(appData, "DishPerType")
  outputTab <- merge(analyses, dishes, by.x = "Type", by.y = "Type")
  outputTab <- aggregate(outputTab$DishPerSample ~ outputTab$Dish, FUN = sum)
  outputTab <- outputTab[outputTab[,2]>0,]
  colnames(outputTab)<-c("Среда", "Количество")
  return(outputTab)
}

#Region Функции для создания виртуального планшета

CalcTubePosition <- function(rowNum, colNum, tubeNum){
  plateCount <- rowNum * colNum
  tubePlace <- tubeNum %% plateCount
  plateNum <- as.integer(tubeNum / plateCount)
  result$PlateNum <- PlateNum
  result$Position <- tubePlace
  if(tubePlace == 0){
    result$PlateNum <- PlateNum - 1
    result$Position <- plateCount
  }
  return(result)
}

CreateEmptyPlate<-function(rowsN, colsN){
  outDf<-as.data.frame(matrix("", nrow = rowsN, ncol = colsN))
  rownames(outDf) <- 1:rowsN
  colnames(outDf) <- 1:colsN
  return(outDf)
}

PutInPlate <- function(dataFrame, posNumber, inputType = c("empty", "blank", "newTube", "oldTube")){
  getIcon <- function(objType = c("empty", "blank", "newTube", "oldTube")){
    if(objType == "empty"){
      return("")
    }
    if(objType == "blank"){
      return(as.character(icon("file-contract", style = "font-size: 30px; color: rgb(18, 49, 204);")))
    }
    if(objType == "newTube"){
      return(as.character(icon("vial", style = "font-size: 30px; color: rgb(145, 28, 45);")))
    }
    if(objType == "oldTube"){
      return(as.character(icon("vial-circle-check", style = "font-size: 30px; color: rgb(120, 235, 122);")))
    }
    return("")
  }
  df <- dataFrame
  cellsInRow <- ncol(df)
  columnPosition <- posNumber %% cellsInRow
  rowPosition <- nrow(df) - as.integer(posNumber / cellsInRow)
  if(columnPosition == 0){
    columnPosition = ncol(df)
    rowPosition <- rowPosition + 1
  }
  df[rowPosition, columnPosition] <- getIcon(inputType)
  return(df)
}

GetTubeQueryNum<-function(firstNum, tubeNum){
  if(firstNum > tubeNum){
    stop("Первый номер не может быть больше текущего")
  }
  return(tubeNum + 1 - firstNum)
}

GetTubePlateNum<-function(firstNum, tubeNum, rowsN, colsN){
  if(firstNum > tubeNum){
    stop("Первый номер не может быть больше текущего")
  }
  return(as.integer((tubeNum + 1 - firstNum) / (rowsN*colsN)) + 1)
}

#EndRegion 

#Region разбор пробирок по планшетам
GetPlateNums <- function(mainData, specType, rowNum, colNum, necessaryPlate){
  numInPlate <- rowNum * colNum
  tmp <- mainData %>% filter(Type == specType)
  tmp$Plate <- lapply(tmp$CurrentNum, function(x,y){
    minNum<-min(x)
    as.integer((x+1-minNum) / y) + 1
  },y = numInPlate)
  tmp <- tmp %>% filter(Plate == necessaryPlate)
  output<-data.frame(Num = tmp$CurrentNum, Tube = tmp$HasVial)
  return(output)
}
#EndRegion разбор пробирок по планшетам