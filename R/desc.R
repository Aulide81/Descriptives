desc<-function(x,w,stat,dec){
  UseMethod("desc", x)
}

desc.data.frame<-function(x,w,stat,dec){
  t(sapply(x,.desc,w,stat,dec))
}

desc.numeric<-function(x,w,stat,dec){
 .desc(x,w,stat,dec)
}

desc.logical<-function(x,w,stat,dec){
  .desc(x,w,stat,dec)
}
