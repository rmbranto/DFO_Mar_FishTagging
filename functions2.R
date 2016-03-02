makehist<-function(experiment,f.vect,f.min='',f.max='',f.int='',f.fun='sum',f.all=F,f.pre=''){
  Experiment<-experiment[!is.na(f.vect)]
  f.vect<-f.vect[!is.na(f.vect)]
  if (f.min!='') {f.vect[f.vect<=f.min]<-f.min}
  if (f.max!='') {f.vect[f.vect>=f.max]<-f.max}
  if (f.int!='') {f.vect<-floor(f.vect/f.int)*f.int}
  f.hist<-tapply(rep(1,length(f.vect)),list(Experiment,f.vect),FUN=f.fun)
  f.hist[is.na(f.hist)]<-0
  if(f.all){f.hist<-cbind(f.hist,all=apply(f.hist,1,sum))}
  dimnames(f.hist)[2]<-list(paste(f.pre,unlist(dimnames(f.hist)[2]),sep=''))
  data.frame(Experiment=row.names(f.hist),f.hist,row.names=NULL,stringsAsFactors=F)
}

make.shapefile<-function(dd,ddTable,ddName,type=3,id="Id"){
  ddShapefile <- convert.to.shapefile(dd, ddTable, id, type)
  write.shapefile(ddShapefile, ddName, arcgis=T)
  system(paste('rm ', ddName, '/*.*', sep=''))
  system(paste('zip ', ddName, '.zip ',ddName, '.*',sep=''))
  system(paste('rm ', ddName, '.dbf ', ddName, '.shp ', ddName, '.shx',sep=''))
  system(paste('unzip ',ddName, '.zip -d ', ddName,sep=''))
}

make.dd.r<-function(dd=dd.sl){
  dd.r<-NULL
  for (i in ddTable2$Id){
    dd.Id<-ddTable$Id[ddTable$sgrid==ddTable2$sgrid[i]&ddTable$egrid==ddTable2$egrid[i]]
    dd.r<-rbind(dd.r,data.frame(Id=i,X=dd$X[dd$Id==dd.Id],Y=dd$Y[dd$Id==dd.Id],stringsAsFactors=F))
  }
  dd.r
}

make.season.date<-function(EventDate,Year){
my.d<-as.Date(EventDate[!is.na(Year)])
my.s<-floor(as.numeric(format(my.d,'%m'))/3)
Season.Date<-rep('',length(EventDate))
Season.Date[!is.na(Year)]<-
  as.character(as.Date(
    ifelse(my.s==0,paste(format(my.d,'%Y'),'1','15',sep='-'),
           ifelse(my.s==1,paste(format(my.d,'%Y'),'4','15',sep='-'),
                  ifelse(my.s==2,paste(format(my.d,'%Y'),'7','15',sep='-'),
                         ifelse(my.s==3,paste(format(my.d,'%Y'),'10','15',sep='-'),
                                paste(1+as.numeric(format(my.d,'%Y')),'1','15',sep='-')))))
  ))
}

f.grid<-function(g){
  d<-NULL
  for (i in 1:dim(g)[1]){
    d<-rbind(d,data.frame(Id=i,X=g$lon[i]-.5,Y=g$lat[i]-.25,stringsAsFactors=F))
    d<-rbind(d,data.frame(Id=i,X=g$lon[i]-.5,Y=g$lat[i]+.25,stringsAsFactors=F))
    d<-rbind(d,data.frame(Id=i,X=g$lon[i]+.5,Y=g$lat[i]+.25,stringsAsFactors=F))
    d<-rbind(d,data.frame(Id=i,X=g$lon[i]+.5,Y=g$lat[i]-.25,stringsAsFactors=F))
  }
d    
}

f.line<-function(g){
  d<-rbind(
  data.frame(sl$Id,X=sl$lon.x,Y=sl$lat.x,stringsAsFactors=F),
  data.frame(sl$Id,X=sl$lon.y,Y=sl$lat.y,stringsAsFactors=F))
  d
}
  
merge.fact<-function(t,u,n){
  s<-m$measurementType==t&m$measurementUnit==u
  m<-data.frame(id=m$id[s],value=m$measurementValue[s],stringsAsFactors=F)  
  dimnames(m)[2]<-list(c('id',n))
  merge(x=o,y=m,by='id',all.x=T)
}
