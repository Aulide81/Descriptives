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

print.CrossTable<-function(x){

sep<-rep(0,ncol(x))
jumps<-length(attributes(x)$cells)
Seq<-seq(1,nrow(x)-jumps,jumps)

if(jumps>1){
tablep<-NULL
for(i in Seq){
  j<-i+jumps-1
  tablep<-rbind(tablep,x[i:j,,drop=F]," "=sep)
}
tablep<-rbind(tablep,x[(i+jumps):nrow(x),,drop=F])
}else{
  tablep<-x
}
tablep<-round(tablep,attributes(x)$dec)
colnames(tablep)<-substr(colnames(tablep),1,15)
rownames(tablep)<-substr(rownames(tablep),1,18)
cat(attributes(x)$title[1],attributes(x)$title[2],"by",attributes(x)$title[3],attributes(x)$title[4])
print(knitr::kable(tablep,format.args=list(zero.print="",nsmall=0L),format="rst",padding = 0))
cat("\n")
}

var.lab<-function(x,label){
  name<-as.character(substitute(x))
  tmp <- substitute({
    attr(x, "var.lab") <- label
    names(attr(x, "var.lab")) <- as.character(name)[3]
  })
  eval(tmp, parent.frame())   
}

val.lab<-function(x,labels){
  tmp <- substitute({
      if (!is.numeric(labels)) stop("El vector ha de ser numerico")
      if (length(names(labels)) == 0) stop("La variable def se ha introducido sin etiquetas")
      if (length(unique(labels)) != length(labels)) stop("valores repetidos")
      attr(x, "val.lab") <- sort(labels)
    })
    eval(tmp, parent.frame())
}

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

crostab<-function(x,y,w,cells,dec,value,title){
  
if(missing(cells)) cells<-"count"
if(missing(dec)) dec<-1
if(missing(title)) title<-""
  
if(is.list(x) & !missing(value)) {
  if(missing(w)) w<-rep(1,nrow(x))  
    if(missing(y)){
      .crostab.d(x=x,w=w,cells=cells,dec=dec,value=value,title=title)
    }else{
      .crostab.d(x=x,y=y,w=w,cells=cells,dec=dec,value=value,title=title)
      }
}else if (is.list(x) & missing(value)) {
    if(missing(w)) w<-rep(1,nrow(x))
    if(is.list(y)){
        lapply(x,function(k){
        lapply(y,function(l).crostab(x=k,y=l,w=w,dec=dec,cells=cells))   
        })
    }else{
        lapply(x,function(k).crostab(x=k,y=y,w=w,dec=dec,cells=cells))    
    }
    
}else{
  if(missing(w)) w<-rep(1,length(x))
  if(is.list(y)){
      lapply(y,function(k).crostab(x=x,y=k,w=w,dec=dec,cells=cells))
  }else{
      .crostab(x=x,y=y,w=w,dec=dec,cells=cells)      
  }
}
}
             
Count<-function(x,w){
if(missing(w)){
    sum(!is.na(x),na.rm=T)
  }else{
    sum(w[!is.na(x)],na.rm=T)
  }
}

Sum<-function(x,w){
  if(missing(w)){
    sum(x,na.rm=T)
  }else{
    sum(x*w,na.rm=T)
  }
}

Mean<-function(x,w){
  if(missing(w)){
    Sum(x)/Count(x)
  }else{
    Sum(x,w)/Count(x,w)
  }
}

Var<-function(x,w){
  if(missing(w)){
    value<-sum(x^2,na.rm=T)-(2*Count(x)*Mean(x)^2)+(Count(x)*Mean(x)^2)
    value/(Count(x)-1)
  }else{
    value<-sum(w*x^2,na.rm=T)-(2*Count(x,w)*Mean(x,w)^2)+(Count(x,w)*Mean(x,w)^2)
    value/(Count(x,w)-1)
  }
}

Std.dev<-function(x,w){
  if(missing(w)){
    sqrt(Var(x))
  }else{
    sqrt(Var(x,w))
  }
}

.desc<-function(x,w,stat,dec){
if (missing(stat)) stat<-c("Min","Mean","Std.dev","Max","Sum","Count")
if(missing(dec)) dec<-2
  
if(missing(w)){  
vector<-suppressWarnings(round(sapply(stat,function(k){    
    switch(k,
           Mean = Mean(x),
           Var=Var(x),
           Std.dev= Std.dev(x),
           Count=Count(x),
           Sum=Sum(x),
           Min=min(x,na.rm=T),
           Max=max(x,na.rm=T),
           Range=range(x,na.rm=T)
    )}),dec))
  }else{
 
vector<-suppressWarnings(round(sapply(stat,function(k){    
      switch(k,
             Mean = Mean(x,w),
             Var=Var(x,w),
             Std.dev= Std.dev(x,w),
             Count=Count(x,w),
             Sum=Sum(x,w),
             Min=min(x,na.rm=T),
             Max=max(x,na.rm=T),
             Range=range(x,na.rm=T)
      )}),dec))
  }
vector<-structure(vector,title=attr(x,"var.lab"),class=c("Descriptive",class(vector)))
return(vector)
}
           
print.Descriptive<-function(x){
  row_names<-attr(x,"title")
  col_names<-names(x)
  attributes(x)<-NULL
  dim(x)<-c(1,length(x))
  rownames(x)<-row_names
  colnames(x)<-col_names
  print(x)
}

desc<-function(x,...){
  UseMethod("desc", x)
}

desc.list<-function(x,...){
  clases<-sapply(x,class)
del_var<-clases%in%c("factor","character")
row_names<-sapply(x,function(k)attr(k,"var.lab"))

if(sum(del_var)==0){
  matriz<-t(sapply(x,.desc,...))
  rownames(matriz)<-paste(rownames(matriz),substr(row_names,1,30))
  return(matriz)
  cat("\n")
  
  return(matriz)
}else{
  matriz<-t(sapply(x[,!del_var,drop=F],.desc,...))
  rownames(matriz)<-paste(rownames(matriz),substr(row_names[!del_var],1,30))
  cat("Not numeric variables:",names(x)[del_var],"\n\n")
  return(matriz)
  cat("\n")
}
}

desc.matrix<-function(x,...){
  t(apply(x,2,.desc,...))
}

desc.numeric<-function(x,...){
 .desc(x,...)
}

desc.logical<-function(x,...){
  .desc(x,...)
}

means<-function(x,y,w,stat,dec){
if (missing(stat)) stat<-c("Min","Mean","Std.dev","Max","Sum","Count")
if(missing(dec)) dec<-2
i<-unique(y)
if(missing(w)){
  tabla<-t(sapply(i,function(k)
    desc(x[y==k],stat=stat,dec=dec)))
}else{
  tabla<-t(sapply(i,function(k)
    desc(x[y==k],w[y==k],stat=stat,dec=dec)))
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

Knn<-function(x,y,w,k,classif){
  if(k<2) stop("K must be greater than one")
  
  if(missing(classif)){  
    x<-apply(x,1,function(k){rowSums(t(t(x)-k)^2)})
    diag(x)<-NA
    x<-t(apply(x,1,order))[,c(1:k),drop=F]
    
    if(missing(w)){
      candidatos<-t(apply(x,1,function(k)y[k]))
      grupos<-apply(candidatos,1,function(k){
        a<-table(k)
        names(a)[which.max(a)]
      })
    }else{
      candidatos<-t(apply(orden,1,function(k)
        c(y[k],w[k])))
      grupos<-apply(candidatos,1,function(j){
        a<-tapply(j[(k+1):(k*2)],list(j[1:k]),sum)
        names(a)[which.max(a)]
      })
    }
    return(grupos)
  }else{
    
    x<-apply(x,1,function(k){rowSums(t(t(classif)-k)^2)})
    diag(x)<-NA
    x<-t(apply(x,1,order))[,c(1:k),drop=F]
    
    if(missing(w)){
      candidatos<-t(apply(x,1,function(k)y[k]))
      grupos<-apply(candidatos,1,function(k){
        a<-table(k)
        names(a)[which.max(a)]
      })
    }else{
      candidatos<-t(apply(orden,1,function(k)
        c(y[k],w[k])))
      grupos<-apply(candidatos,1,function(j){
        a<-tapply(j[(k+1):(k*2)],list(j[1:k]),sum)
        names(a)[which.max(a)]
      })
    }
    return(grupos)
    
  }
}

kmedias<-function(x,k,init,classif=F,iter=10){
  
  if(missing(init)){
  if(missing(k)) stop("missing k numbers clusters")
  
  # orden<-order(rowSums(x))
  # c_init<-round(seq(length(orden)/(k*2),length(orden),length(orden)/k),0)
  # c_init<-orden[c_init]
  # c_init<-x[c_init,,drop=F]
  c_init<-sample(1:nrow(x),k)
  c_init<-x[c_init,,drop=F]
  }else{
    if(ncol(x)!=ncol(init)) stop("ncol(x) diferent ncol(init)")
    k<-nrow(init)
    c_init<-init
  }
  
  grupo_i<-apply(x,1,function(k){
    which.min(rowSums(t(t(c_init)-k)^2))})
  
  cambios<-T
  i<-0
  
  if (classif==T & !missing(init)){
    print(i)
    return(grupo_i)}
  
  while (cambios==T & i<iter){
    
  c_fin<-apply(x,2,function(k)tapply(k,list(grupo_i),mean))
  
  grupo_f<-apply(x,1,function(k){
    which.min(rowSums(t(t(c_fin)-k)^2))})
  
  cambios<-!all(grupo_i==grupo_f)
  c_init<-c_fin
  grupo_i<-grupo_f
  i<-i+1
  }
cat(i,cambios,"\n\n")
return(grupo_f)
}

.covariance<-function(x,y,w,cor=F){
  xy<-!is.na(x)&!is.na(y)
  if(missing(w)){
    value<-sum(x*y,na.rm=T)-(Mean(x)*Mean(y)*sum(xy,na.rm=T))
    value<-value/(sum(xy,na.rm=T)-1)
    if (cor==F){
      return(value)
    }else{
      value<-value/(Std.dev(x)*Std.dev(y))
    }
  }else{
    value<-sum(x*y*w,na.rm=T)-(Mean(x,w)*Mean(y,w)*sum(w[xy],na.rm=T))
    value<-value/(sum(w[xy],na.rm=T)-1)
    if (cor==F){
      return(value)
    }else{
      value<-value/(Std.dev(x,w)*Std.dev(y,w))
    }
  }
}

covariance<-function(x,y,w,cor=F){
  if (length(dim(x))==2){
    if(missing(w)){
      apply(x,2,function(k){apply(x,2,function(j).covariance(k,j,cor=cor))})
    }else{
      apply(x,2,function(k){apply(x,2,function(j).covariance(k,j,w,cor=cor))})
    }
  }else if(is.vector(x)){
    if(missing(x)){
      .covariance(k,j,cor=cor)
    }else{
      .covariance(k,j,w,cor=cor)
    }
  }
}
            
freq<-function (x, ...) {UseMethod("freq", x)}
freq.character<-function (x, ...) {.frequencies(x, ...)}
freq.factor<-function (x, ...) {.frequencies(x, ...)}
freq.numeric<-function (x, ...) {.frequencies(x, ...)}
freq.logical<-function (x, ...) {.frequencies(x, ...)}
freq.data.frame<-function (x, ...) {lapply(x, .frequencies, ...)}
freq.matrix<-function (x, ...) {apply(x,2, .frequencies, ...)}

.frequencies<-function(x,w,order,dec=1){
  if (missing(w)) w<-rep(1,length(x))
  n<-sum(w[!is.na(x)],na.rm = T)
  N<-sum(w,na.rm = T)
  
  absolutos<-suppressWarnings(rowsum(w,x))
  pct<-(absolutos/N)*100
  vpct<-(absolutos/n)*100
  if (n!=N) vpct[length(vpct)]<-NA
  
  tabla<-cbind(round(absolutos,0),pct,vpct)
  
  if(is.numeric(x) & !is.null(attributes(x)$val.lab)){
    labelsx<-attributes(x)$val.lab
    name<-as.numeric(rownames(tabla))
    labelsx<-names(labelsx[match(name,labelsx)])
    labelsx[is.na(labelsx)]<-""
    rownames(tabla)<-paste(rownames(tabla),labelsx)
  }
  if (n!=N) rownames(tabla)[length(rownames(tabla))]<-" missing"
  if (!missing(order)){
    if(order=="d"){
      tabla<-tabla[order(-vpct),]  
    }else if (order=="a"){
      tabla<-tabla[order(vpct),]  
    }
  }
  
  tabla<-cbind(tabla,cumsum(tabla[,3]))
  colnames(tabla)<-c("Frec","Pct","Val.Pct","Cum.Pct")
  tabla[,1]<-round(tabla[,1],0)
  tabla[,2:4]<-round(tabla[,2:4],dec)
   
  names(tabla)<-c("Frec","Pct","Val.Pct","Cum.Pct")
  
  tabla<-structure(tabla,
                   title=c(names(attr(x,"var.lab")),attr(x,"var.lab")),
                   resumen=paste("Total Cases:",round(N,0)," Valid Cases:",round(n,0)),
                   class=c(class(tabla),"Frequencies"))
  return(tabla)
  
}
            
print.Frequencies<-function(x){
  dimension<-dim(x)
  col_names<-colnames(x)
  row_names<-rownames(x)      
  title<-attr(x,"title")
  resumen<-attr(x,"resumen")
  attributes(x) <- NULL
  x<-structure(x,dim=dimension,dimnames=list(row_names,col_names))
  cat(title,"\n\n")
  print(x,na.print="")
  cat("\n",resumen,"\n\n")
}


##analisis factorial por componentes principales
#x<-as.matrix(iris[,c(1:4)])
#x_cor<-covariance(x,cor=T)
#descomp<-eigen(x_cor)
#cargas<-descomp$vectors[,c(1:3)]%*%sqrt(diag(descomp$values[1:3]))
#cargas_rot<-varimax(cargas,normalize=F)
#print(cargas_rot$loadings,sort=F,cutoff=0)
#cargas<-rep(NA,length(cargas_rot$loadings))
#for(i in 1:length(cargas)){
#  cargas[i]<-cargas_rot$loadings[[i]]
#}
#cargas<-matrix(cargas,ncol=4)
#apply(cargas,2,function(k)sum(k^2))
#betas<-solve(x_cor,cargas)
#scores<-zx%*%betas
#covariance(scores,cor=T)




