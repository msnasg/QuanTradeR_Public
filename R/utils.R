

existsymbol <- function(symbol, session){
  if(is.null(symbol)){
    showModal(modalDialog(
      size = "s", easyClose = T, fade = TRUE,
      title = "Warning !!!",
      "Please set the data first.",
      footer = NULL
    ))
    return(F)
  }else{ return(T) }
}
 

