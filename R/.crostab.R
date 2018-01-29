.crostab<-function(x,y,w,cells,dec){
  
  absolutos<-tapply(w,list(x,y),sum,na.rm=T)
  absolutos[is.na(absolutos)]<-0
  
  if(is.numeric(x) & !is.null(attributes(x)$val.lab)){
    labelsx<-attributes(x)$val.lab
    name<-as.numeric(rownames(absolutos))
    labelsx<-names(labelsx[match(name,labelsx)])
    labelsx[is.na(labelsx)]<-""
    rownames(absolutos)<-paste(rownames(absolutos),labelsx)
  }
  
  if(is.numeric(y) & !is.null(attributes(y)$val.lab)){
    labelsy<-attributes(y)$val.lab
    name<-as.numeric(colnames(absolutos))
    labelsy<-names(labelsy[match(name,labelsy)])
    labelsy[is.na(labelsy)]<-""
    colnames(absolutos)<-paste(colnames(absolutos),labelsy)
  }
  
  pcol<-prop.table(absolutos,2)*100
  prow<-prop.table(absolutos,1)*100
  ptot<-prop.table(absolutos)*100
  absolutos<-addmargins(absolutos,FUN=list("Total"=sum),quiet=T)
  ptot<-addmargins(ptot,FUN=list("Total"=sum),quiet=T)
  pcol<-addmargins(pcol,margin=1,FUN=list("Total"=sum),quiet=T)
  pcol<-cbind(pcol,Total=ptot[,ncol(ptot)])
  prow<-addmargins(prow,margin=2,FUN=list("Total"=sum),quiet=T)
  prow<-rbind(prow,Total=ptot[nrow(ptot),])
  
  tabla<-NULL
  for(i in 1:nrow(absolutos)){
    tabla<-rbind(tabla,absolutos[i,],prow[i,],pcol[i,],ptot[i,])
  }
 
  indices<-c(c("count","row","col","tot")%in%cells)
  cells<-c("count","row","col","tot")[indices]
  indices<-rep(indices,nrow(absolutos))
  tabla<-tabla[indices,,drop=F]
  
  
  rownames(tabla)<-rep(rownames(absolutos),each=length(cells))
  rownames(tabla)[duplicated(rownames(tabla))]<-""
  tabla<-structure(tabla,
                   cells=cells,
                   title=c(names(attr(x,"var.lab")),attr(x,"var.lab"),names(attr(y,"var.lab")),attr(y,"var.lab")),
                   dec=dec,
                   class=c(class(tabla),"CrossTable"))
  return(tabla)
  
}
