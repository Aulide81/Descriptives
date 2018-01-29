Std.dev<-function(x,w){
  if(missing(w)){
    sqrt(Var(x))
  }else{
    sqrt(Var(x,w))
  }
}
