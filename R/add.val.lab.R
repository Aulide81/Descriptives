add.val.lab<-function(x,labels){
  tmp <- substitute({
    if (!is.numeric(labels)) stop("x must be numeric")
    if (length(names(labels)) == 0) stop("labels without labels")
    if (length(unique(labels)) != length(labels)) stop("labels with duplicated values")
    attr(x, "val.lab") <- sort(c(sort(labels), attr(x, 
                                                      "val.lab"))[!duplicated(c(sort(labels), attr(x, 
                                                                                                "val.lab")))])
  })
  eval(tmp, parent.frame())
}
