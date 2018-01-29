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
