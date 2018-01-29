Count<-function(x,w){
if(missing(w)){
    sum(!is.na(x),na.rm=T)
  }else{
    sum(w[!is.na(x)],na.rm=T)
  }
}
