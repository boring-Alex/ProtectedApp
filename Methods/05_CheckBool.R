setGeneric("CheckBool", function(value){
  standardGeneric("CheckBool")
})

setMethod("CheckBool", signature("character"), function(value){
  if(toupper(value) == "FALSE"){
    return(FALSE)
  }
  if(value == "0"){
    return(FALSE)
  }
  return(TRUE)
})

setMethod("CheckBool", signature("numeric"), function(value){
  return(as.logical(value))
})

setMethod("CheckBool", signature("logical"), function(value){
  return(value)
})