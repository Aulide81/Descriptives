var.lab<-function(x,label){
  name<-as.character(substitute(x))
  tmp <- substitute({
    attr(x, "var.lab") <- label
    names(attr(x, "var.lab")) <- as.character(name)[3]
  })
  eval(tmp, parent.frame())   
}
