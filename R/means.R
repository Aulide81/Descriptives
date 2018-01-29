means<-function(x,y,w,stat,dec){
if (missing(stat)) stat<-c("Min","Mean","Std.dev","Max","Sum","Count")
if(missing(dec)) dec<-2
i<-unique(y)
if(missing(w)){
  tabla<-t(sapply(i,function(k)
    desc(x[y==k],stat=stat,dec)))
}else{
  tabla<-t(sapply(i,function(k)
    desc(x[y==k],w[y==k],stat,dec)))
}

rownames(tabla)<-i

if(is.numeric(y) & !is.null(attributes(y)$val.lab)){
  labelsy<-attributes(y)$val.lab
  labelsy<-names(labelsy[match(i,labelsy)])
  labelsy[is.na(labelsy)]<-""
  rownames(tabla)<-paste(rownames(tabla),labelsy)
}

delete<-is.na(rowSums(tabla,na.rm=T))
tabla<-tabla[!delete,,drop=F]
if(nrow(tabla)==0) stop("Means table is empty")

return(tabla)
}
