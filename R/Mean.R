Mean<-function(x,w){
  if(missing(w)){
    Sum(x)/Count(x)
  }else{
    Sum(x,w)/Count(x,w)
  }
}
