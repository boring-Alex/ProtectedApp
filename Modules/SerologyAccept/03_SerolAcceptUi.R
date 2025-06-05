SerolAcceptUi<-function(theme, id = "SerolAccept"){
  ns<-NS(id)
  tagList(
    navset_card_underline(
      nav_panel(title = "Первичная регистрация направлений",
                RegistrationUi(theme = theme, id = id)),
      nav_panel(title = "Расстановка пробирок")
    )
  )
}