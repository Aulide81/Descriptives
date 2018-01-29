crostab<-function(x,y,w,cells,dec,value,title){
  
if(missing(cells)) cells<-"count"
if(missing(dec)) dec<-1
if(missing(title)) title<-""
  
if(is.data.frame(x) & !missing(value)) {
  if(missing(w)) w<-rep(1,nrow(x))  
    if(missing(y)){
      .crostab.d(x=x,w=w,cells=cells,dec=dec,value=value,title=title)
    }else{
      .crostab.d(x=x,y=y,w=w,cells=cells,dec=dec,value=value,title=title)
      }
}else if (is.data.frame(x) & missing(value)) {
    if(missing(w)) w<-rep(1,nrow(x))
    lapply(x,function(k).crostab(x=k,y=y,w=w,dec=dec,cells=cells))
}else{
  if(missing(w)) w<-rep(1,length(x))
  .crostab(x=x,y=y,w=w,dec=dec,cells=cells)
  }
}
