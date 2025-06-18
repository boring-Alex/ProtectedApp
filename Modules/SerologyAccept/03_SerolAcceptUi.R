SerolAcceptUi<-function(theme, id = "SerolAccept"){
  ns<-NS(id)
  tagList(
    navset_card_underline(
      nav_panel(title = "Первичная регистрация направлений",
                RegistrationUi(theme = theme, id = id),
                icon = icon("file-contract")),
      nav_panel(title = "Расстановка пробирок",
                VialLocatorUi(theme = theme, id = id),
                icon = icon("vials"))
    )
  )
}