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
  colnames(outdf) <- 1:colsN
  return(outDf)
}

PutInPlate <- function(dataFrame, posNumber, inputType = c("empty", "blank", "newTube", "oldTube")){
  getIcon <- function(objType = c("empty", "blank", "newTube", "oldTube")){
    if(objType == "empty"){
      return("")
    }
    if(objType == "blank"){
      return(as.character(icon("square")))
    }
    if(objType == "newTube"){
      return(as.character(icon("circle-check")))
    }
    if(objType == "oldTube"){
      return(as.character(icon("circle")))
    }
    return("")
  }
  df <- dataFrame
  cellsInRow <- ncol(df)
  columnPosition <- posNumber %% cellsInRow
  rowPosition <- as.integer(posNumber / cellsInRow) + 1
  if(columnPosition == 0){
    columnPosition = ncol(df)
    rowPosition = nrow(df)
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

#EndRegion 