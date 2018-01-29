val.lab<-function(x,labels){
  tmp <- substitute({
      if (!is.numeric(labels)) stop("El vector ha de ser numerico")
      if (length(names(labels)) == 0) stop("La variable def se ha introducido sin etiquetas")
      if (length(unique(labels)) != length(labels)) stop("valores repetidos")
      attr(x, "val.lab") <- sort(labels)
    })
    eval(tmp, parent.frame())
}
