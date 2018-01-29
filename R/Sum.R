Sum<-function(x,w){
  if(missing(w)){
    sum(x,na.rm=T)
  }else{
    sum(x*w,na.rm=T)
  }
}
