getButton<-function(bttnId, bttnName, bttnClass, bttnIcon = NULL, tTip = NULL){
  ui<-tooltip(actionButton(inputId = bttnId,
                 label = bttnName,
                 class = bttnClass,
                 icon = bttnIcon,
                 style = "width: 100%"),
              tTip)
  if(is.null(tTip)){
    ui<-actionButton(inputId = bttnId,
                     label = bttnName,
                     class = bttnClass,
                     icon = bttnIcon,
                     style = "width: 100%")
  }
  return(ui)
}

PrimaryButton<-function(buttonId, buttonName, ic = NULL, explanation = NULL){
  return(getButton(buttonId, buttonName, "btn btn-primary", ic, explanation))
}

SecondaryButton<-function(buttonId, buttonName, ic = NULL, explanation = NULL){
  return(getButton(buttonId, buttonName, "btn btn-secondary", ic, explanation))
}

SuccessButton<-function(buttonId, buttonName, ic = NULL, explanation = NULL){
  return(getButton(buttonId, buttonName, "btn btn-success", ic, explanation))
}

InfoButton<-function(buttonId, buttonName, ic = NULL, explanation = NULL){
  return(getButton(buttonId, buttonName, "btn btn-info", ic, explanation))
}

WarningButton<-function(buttonId, buttonName, ic = NULL, explanation = NULL){
  return(getButton(buttonId, buttonName, "btn btn-warning", ic, explanation))
}

DangerButton<-function(buttonId, buttonName, ic = NULL, explanation = NULL){
  return(getButton(buttonId, buttonName, "btn btn-danger", ic, explanation))
}

DarkButton<-function(buttonId, buttonName, ic = NULL, explanation = NULL){
  return(getButton(buttonId, buttonName, "btn btn-dark", ic, explanation))
}

LightButton<-function(buttonId, buttonName, ic = NULL, explanation = NULL){
  return(getButton(buttonId, buttonName, "btn btn-light", ic, explanation))
}

LinkButton<-function(buttonId, buttonName, ic = NULL, explanation = NULL){
  return(getButton(buttonId, buttonName, "btn btn-link", ic, explanation))
}