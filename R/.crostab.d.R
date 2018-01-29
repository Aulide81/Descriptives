.crostab.d<-function(x,y,w,dec,value,cells,title){
  
  #if (!is.data.frame(df)) stop("x must be data.frame")
  #if (missing(value)) stop("Insert value")
  #if (missing(w)) w<-rep(1,nrow(x))
  #if (missing(cells)) cells="count"
  
  x[x!=value | is.na(x)]<-0
  x<-x[,colSums(x)>0,drop=F]
  
  names(x)<-paste(names(x),sapply(x,function(k)attributes(k)$var.lab))
  
  if(missing(y)){
    
    absolutos<-lapply(x,function(k){
      tapply(w,list(k),sum,na.rm=T)})
    labelsy<-NULL
    toty<-sum(absolutos[[1]],na.rm=T)
    tot<-sum(toty,na.rm=T)
    absolutos<-t(sapply(absolutos,function(k)k))
    
    if (is.numeric(value)){
      absolutos<-absolutos[,as.numeric(colnames(absolutos))==value,drop=F]
    }else{
      absolutos<-absolutos[,colnames(absolutos)==value,drop=F]
    }
    
    pcol<-(absolutos/toty)*100
    prow<-prop.table(absolutos,1)*100
    ptot<-(absolutos/tot)*100
    absolutos<-rbind(absolutos,"Total"=c(tot))
    prow<-rbind(prow,"Total"=(toty/tot)*100)
    pcol<-rbind(pcol,"Total"=100)
    ptot<-rbind(ptot,"Total"=(toty/tot)*100)
    colnames(absolutos)<-colnames(prow)<-colnames(pcol)<-colnames(ptot)<-"Total"
    
  }else{
    absolutos<-lapply(x,function(k){
      tapply(w,list(k,y),sum,na.rm=T)})
    name<-colnames(absolutos[[1]])
    toty<-colSums(absolutos[[1]],na.rm=T)
    tot<-sum(toty,na.rm=T)
    
    if (is.numeric(value)){
      absolutos<-lapply(absolutos,function(k){
        k[as.numeric(rownames(k))==value,,drop=F]})
    }else{
      absolutos<-lapply(absolutos,function(k){
        k[rownames(k)==value,,drop=F]})
    }
    absolutos<-t(sapply(absolutos,function(k)k))
    absolutos[is.na(absolutos)]<-0
    if(is.numeric(y) & !is.null(attributes(y)$val.lab)){
      labelsy<-attributes(y)$val.lab
      name<-as.numeric(name)
      labelsy<-names(labelsy[match(name,labelsy)])
      labelsy[is.na(labelsy)]<-""
      colnames(absolutos)<-paste(name,labelsy)
    }
    
    
    pcol<-sweep(absolutos,MARGIN=2,toty,FUN="/")*100
    prow<-prop.table(absolutos,1)*100
    ptot<-(absolutos/tot)*100
    
    absolutos<-addmargins(absolutos,margin=2,FUN=list("Total"=sum),quiet=T)
    absolutos<-rbind(absolutos,"Total"=c(toty,tot))
    
    prow<-cbind(prow,"Total"=100)
    prow<-rbind(prow,"Total"=c((toty/tot)*100,100))
    
    pcol<-cbind(pcol,"Total"=c(rowSums(ptot)))
    pcol<-rbind(pcol,"Total"=100)
    ptot<-addmargins(ptot,margin=2,FUN=list("Total"=sum),quiet=T)
    ptot<-rbind(ptot,"Total"=c((toty/tot)*100,100))
  }
  
  tabla<-NULL
  for(i in 1:nrow(absolutos)){
    tabla<-rbind(tabla,absolutos[i,,drop=F],prow[i,,drop=F],pcol[i,,drop=F],ptot[i,,drop=F])
  }
  
  indices<-c(c("count","row","col","tot")%in%cells)
  cells<-c("count","row","col","tot")[indices]
  indices<-rep(indices,nrow(absolutos))
  tabla<-tabla[indices,,drop=F]
  rownames(tabla)<-rep(rownames(absolutos),each=length(cells))
  rownames(tabla)[duplicated(rownames(tabla))]<-""
  
  tabla<-structure(tabla,
                   cells=cells,
                   dec=dec,
                   title=title,
                   class=c(class(tabla),"CrossTable"))
  if (!missing(y)) 
    attributes(tabla)$title<-c("",attributes(tabla)$title,names(attr(y,"var.lab")),attr(y,"var.lab"))
  return(tabla)
  
}
