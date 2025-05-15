SuccessAlert<-function(header, textMsg){
  alert<-shinyalert(
    title = header,
    text = textMsg,
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "success",
    showConfirmButton = FALSE,
    showCancelButton = FALSE,
    timer = 3000,
    imageUrl = "",
    animation = TRUE
  )
  return(alert)
}

ErrorAlert<-function(header, textMsg){
  alert<-shinyalert(
    title = header,
    text = textMsg,
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "error",
    showConfirmButton = FALSE,
    showCancelButton = FALSE,
    timer = 3000,
    imageUrl = "",
    animation = TRUE
  )
  return(alert)
}

WarningAlert<-function(header, textMsg){
  alert<-shinyalert(
    title = header,
    text = textMsg,
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "warning",
    showConfirmButton = FALSE,
    showCancelButton = FALSE,
    timer = 3000,
    imageUrl = "",
    animation = TRUE
  )
  return(alert)
}

InfoAlert<-function(header, textMsg){
  alert<-shinyalert(
    title = header,
    text = textMsg,
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "info",
    showConfirmButton = FALSE,
    showCancelButton = FALSE,
    timer = 3000,
    imageUrl = "",
    animation = TRUE
  )
  return(alert)
}