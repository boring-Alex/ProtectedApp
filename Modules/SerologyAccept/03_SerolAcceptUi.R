SerolAcceptUi<-function(theme, id = "SerolAccept"){
  ns<-NS(id)
  tagList(
    tags$head(
      tags$script('
      // JavaScript-функция для установки фокуса
      function setFocusToElement(selector) {
        var element = document.querySelector(selector);
        if(element !== null) {
          element.focus();
        }
      }
      
      // Добавляем обработчик сообщений
      Shiny.addCustomMessageHandler("setFocus",
        function(message) {
          setFocusToElement(message.selector); // Выполняем нашу функцию JavaScript
        });
    ')
    ),
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