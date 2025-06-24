UserCabinetUi<-function(theme, id = "UserCabinet"){
  ns<-NS(id)
  return(tagList(
    InfoButton(ns("UserInfo"), "", icon("user-nurse"))
  ))
}