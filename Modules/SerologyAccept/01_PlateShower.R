PlateShower<-function(rNum, cNum, samples, currSample = 0){
  renderDT({
  currData<-CreateEmptyPlate(rNum, cNum)
  currTubePosition <- numeric()
  firstNum <- samples$Num[1]
  inputTypeTypes <- c("empty", "blank", "newTube", "oldTube")
  logo<-character()
  for(n in 1:nrow(samples)){
    logo = inputTypeTypes[2]
    if(samples$Tube[n] == TRUE){
      logo = inputTypeTypes[4]
    }
    if(samples$Num[n] == currSample){
      logo = inputTypeTypes[3]
    }
    currTubePosition <- GetTubeQueryNum(firstNum, samples$Num[n])
    currData <- PutInPlate(currData, currTubePosition, logo)
  }
  rownames(currData)<-rNum:1
  colnames(currData)<-1:cNum
    DT::datatable(
      currData,
      selection = "none",
      escape = FALSE,
      options = list(ordering = FALSE,
                     language = ruDT,
                     dom = 't')
    )
  })
}