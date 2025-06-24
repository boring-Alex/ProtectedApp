UserCabinetServer<-function(theme, id = "UserCabinet", idResult){
  moduleServer(id,function(input, output, session){
    uDialog<-modalDialog(
      renderUI(UserDataUi(theme, id)),
      easyClose = TRUE,
      footer = NULL
    )
    observeEvent(input$UserInfo,{
      showModal(uDialog)
      name<-idResult$user
      if(length(name)==0){return()}
      usr<-Employes %>% filter(Id == name)
      output$Name = renderText({usr$Name[1]})
      output$Job = renderText(usr$Job[1])
      output$email = renderText(paste0(usr$Id[1],"@szgmu.ru"))
    })
  })
}