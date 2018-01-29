.desc<-function(x,w,stat,dec){
if (missing(stat)) stat<-c("Min","Mean","Std.dev","Max","Sum","Count")
if(missing(dec)) dec<-2
  
if(missing(w)){  
  suppressWarnings(round(sapply(stat,function(k){    
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
 
    suppressWarnings(round(sapply(stat,function(k){    
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
}
