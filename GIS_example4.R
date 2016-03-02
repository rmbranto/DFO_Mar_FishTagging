# RMBranto: Oct 2015

# create recapture points layers for GIS based analysis

#m<-read.delim("~/Downloads/dwca-dfo_nl_codtagging-v1/measurementorfact.txt",as.is=T)
#m<-read.csv("NL_Cod_Tagging_Facts.csv",as.is=T)[,2:9]
#save(m,file='measurementorfact.RData')
#o<-read.delim("~/Downloads/dwca-dfo_nl_codtagging-v1/occurrence.txt",as.is=T)
#save(o,file='occurrence.RData')

setwd("~/Desktop/NFtags")
library(shapefiles)
library(marmap)
library(geosphere)
library(maps)
source('functions2.R')

load('measurementorfact.RData')
load('occurrence.RData')

# merge occurrence and measurement data
#######################################

o<-merge.fact('Area Type','coded','areaType')
o<-merge.fact('Duration','years','yearsFree')
o<-merge.fact('Duration','days','daysFree')
o<-merge.fact('Length','cm','length')

#o<-o[o$eventID==5401|o$eventID==8002|o$eventID==9106,]

# release summary
#################

t<-o[o$occurrenceRemarks=='Tagged animal released',]
t$experiment<-t$eventID
t$sDate<-as.Date(t$eventDate)
t$lon<-.5+floor(t$decimalLongitude)
t$lat<-.25+floor(t$decimalLatitude*2)/2
t$sGrid<-substr(t$verbatimLocality,9,11)
t$releases<-1

g<-aggregate(releases~experiment+sGrid+lat+lon,FUN=sum,data=t)
g$Id<-1:dim(g)[1]
make.shapefile(
  f.grid(g),
  g[,c('Id','experiment','sGrid','releases')],
  'releases_by_experiment_grid',
  5)

# recapture summary table
#########################

r<-merge(
  x=o[o$occurrenceRemarks=='Tagged animal released',],
  y=o[o$occurrenceRemarks=='Tagged animal recaptured',],
  by='organismID')

r$season<-floor(r$month.y/3)
r$season[r$season==0]<-4
r$season<-as.character(factor(r$season,labels=c('Spring','Summer','Fall','Winter')))

r$seasonDate<-as.Date(make.season.date(r$eventDate.y,r$year.y))

r$yearsFree<-r$year.y-r$year.x
r$yearsFree[r$month.y<=2]<-r$yearsFree[r$month.y<=2]-1
r$yearsFree[r$yearsFree<=-1]<--1
r$yearsFree<-as.character(r$yearsFree)

r<-data.frame(
  experiment=r$eventID.x,
  yearsFree=r$yearsFree,
  season=r$season,
  sGrid=substr(r$verbatimLocality.x,10,30),
  eGrid=substr(r$verbatimLocality.y,10,30),
  sUnitArea=substr(r$locality.x,6,11),
  eUnitArea=substr(r$locality.y,6,11),
  areaType=r$areaType.y,
  eDate=as.Date(r$seasonDate),
  lon=.5+floor(r$decimalLongitude.y),
  lat=.25+floor(r$decimalLatitude.y*2)/2,
  recaptures=as.integer(1),
  reDaFrLE30=as.integer(ifelse(as.Date(r$eventDate.y)-as.Date(r$eventDate.x)<=30,1,0)),
  reDaFrGT30=as.integer(ifelse(as.Date(r$eventDate.y)-as.Date(r$eventDate.x)>30,1,0)),
  stringsAsFactors=F)

# recapture grids and points
############################

n<-'recaptures_by_experiment_grid'
g<-aggregate(cbind(recaptures,reDaFrLE30,reDaFrGT30)~experiment+eGrid+lon+lat,FUN=sum,data=r)
g$Id<-1:dim(g)[1]
make.shapefile(f.grid(g),g[,c('Id','experiment','eGrid','recaptures','reDaFrLE30','reDaFrGT30')],n,5)

n<-'recaptures_by_experiment_year_season_grid'
g<-aggregate(cbind(recaptures,reDaFrLE30,reDaFrGT30)~experiment+yearsFree+season+eDate+eGrid+lat+lon,FUN=sum,data=r)
g$Id<-1:dim(g)[1]
make.shapefile(f.grid(g),g[,c('Id','experiment','yearsFree','season','eDate','eGrid','recaptures','reDaFrLE30','reDaFrGT30')],n,5)

n<-'recaptures_by_experiment_year_season_points'
make.shapefile(g[,c('Id','lon','lat')],g[,c('Id','experiment','yearsFree','season','eDate','eGrid','recaptures','reDaFrLE30','reDaFrGT30')],n,1)

# straight lines
################

n<-'recaptures_by_experiment_year_season_slPaths'
sl<-merge(
  x=merge(
    x=aggregate(sDate~experiment,FUN=min,data=t),
    y=aggregate(cbind(lon,lat)~experiment,FUN=mean,data=t),
    by='experiment'),
  y=aggregate(cbind(recaptures,reDaFrLE30,reDaFrGT30)~experiment+yearsFree+season+eDate+eUnitArea+eGrid+lon+lat,FUN=sum,data=r),
  by='experiment'
)
sl$Id<-1:dim(sl)[1]
make.shapefile(f.line(sl),sl[,c('Id','experiment','sDate','yearsFree','season','eDate','eUnitArea','eGrid','recaptures','reDaFrLE30','reDaFrGT30')],n,3)

# experiment summary
####################

e<-merge(
  x=read.csv("localNames.csv",as.is=T)[,c(1,2,3,5)],
  y=aggregate(
    cbind(releases=ifelse(occurrenceRemarks=='Tagged animal released',1,0),
          recaptures=ifelse(occurrenceRemarks=='Tagged animal recaptured',1,0))~eventID,
    FUN=sum,data=o),
  by='eventID')

e<-merge(
  x=e,
  y=(merge(
    x=data.frame(
        eventID=aggregate(eventDate~eventID,FUN=min,data=o,subset=occurrenceRemarks=='Tagged animal released')[,1],
        sTagDate=aggregate(eventDate~eventID,FUN=min,data=o,subset=occurrenceRemarks=='Tagged animal released')[,2],
        eTagDate=aggregate(eventDate~eventID,FUN=max,data=o,subset=occurrenceRemarks=='Tagged animal released')[,2],
        stringsAsFactors=F),
    y=data.frame(
        eventID=aggregate(eventDate~eventID,FUN=min,data=o,subset=occurrenceRemarks=='Tagged animal recaptured')[,1],
        sRecDate=aggregate(eventDate~eventID,FUN=min,data=o,subset=occurrenceRemarks=='Tagged animal recaptured')[,2],
        eRecDate=aggregate(eventDate~eventID,FUN=max,data=o,subset=occurrenceRemarks=='Tagged animal recaptured')[,2],
        stringsAsFactors=F),
    by='eventID',
    all.x=T)),
  by='eventID')

e<-merge(x=e,y=aggregate(cbind(decimalLongitude,decimalLatitude)~eventID,FUN=mean,data=o,subset=occurrenceRemarks=='Tagged animal released'),by='eventID')

dimnames(e)[2]<-list(c('experiment','period','year','locale','releases','recaptures','sTagDate','eTagDate','sRecDate','eRecDate','longitude','latitude'))
e$longitude<-round(e$longitude,2)
e$latitude<-round(e$latitude,2)

# length composition
####################

l<-o[!is.na(o$length)&o$occurrenceRemarks=='Tagged animal released',c('eventID','length')]
l<-makehist(l$eventID,as.numeric(l$length),f.min=40,f.max=85,f.int=5,f.pre='l',f.all=T)
names(l)[1]<-list(c("experiment"))
e<-merge(x=e,y=l,by='experiment')

write.csv(e,'experiment_metadata.csv',row.names=F)

#least cost paths
#################

p<-read.csv(file='experiment_metadata.csv',header=T)[,c('experiment','longitude','latitude')]
dimnames(p)[2]<-list(c('experiment','lon','lat'))
p<-merge(x=p,y=r,by='experiment')
p$sGrid<-p$experiment
p<-merge(
  x=merge(
    x=aggregate(eDate~sGrid+eGrid,data=p,FUN=max,na.rm=T),
    y=aggregate(cbind(recaptures,reDaFrLE30,reDaFrGT30)~sGrid+eGrid,data=p,FUN=sum,na.rm=T),
    by=c('sGrid','eGrid')),
  y=aggregate(cbind(lon.x,lat.x,lon.y,lat.y)~sGrid+eGrid,data=p,FUN=mean,na.rm=T),
  by=c('sGrid','eGrid'))
p<-data.frame(p[order(p$sGrid,p$eGrid),],row.names=1:dim(p)[1],stringsAsFactors=F)
p<-p[!( (.25+floor(p$lat.x*2)/2)==p$lat.y & (.5+floor(p$lon.x))==p$lon.y ),]
p<-cbind(experiment=p$sGrid,p)

make.map<-T
res<-10
min.d<--50
max.d<--600

dd.sl<-NULL
dd.lc<-NULL
kms<-NULL
Id<-0

for (i.sGrid in unique(p$sGrid)){  
  cat(i.sGrid,'\n')    
  pp<-p[p$sGrid==i.sGrid,]
  
  getNOAA.bathy(
    lon1=min(c(pp$lon.x,pp$lon.y))-3,lon2=max(c(pp$lon.x,pp$lon.y))+3,
    lat1=min(c(pp$lat.x,pp$lat.y))-4,lat2=max(c(pp$lat.x,pp$lat.y))+4,
    resolution=res)->my.area
  
  trans1<-trans.mat(my.area,min.depth=min.d,max.depth=max.d)
  
  if(make.map){
    plot(my.area,deep=-800,shallow=0,step=200,lty=c(3,3,3,3,1),col=c(rep('gray',4),'black'))
    title(main=paste('NLTagging / Resolution =',res,'/ Experiment =',i.sGrid))
  }
  
  for (i.eGrid in 1:dim(pp)[1]){
    Id<-Id+1
    sites<-data.frame(x=c(pp$lon.x[i.eGrid],pp$lon.y[i.eGrid]),y=c(pp$lat.x[i.eGrid],pp$lat.y[i.eGrid]),stringsAsFactors=F)    
    out<-lc.dist(trans1,loc=sites,res="path")[[1]]
    if(make.map){dummy<-lapply(list(out),lines,col='green')}
    sl<-round(distm(sites)/1000)[2]
    lc<-lc.dist(trans1,loc=sites,res="dist")[1]
    if(lc>=100000){lc<-NA}
    cat(paste(Id,paste(i.sGrid,pp$eGrid[i.eGrid],sep='-'),'sl.kms=',sl,' lc.kms=',lc,'\n'))
    dd.sl<<-rbind(dd.sl,data.frame(Id=Id,X=sites[,1],Y=sites[,2])) 
    dd.lc<<-rbind(dd.lc,data.frame(Id=Id,X=out[,1],Y=out[,2])) 
    kms<<-rbind(kms,data.frame(Id=Id,experiment=i.sGrid,eGrid=pp$eGrid[i.eGrid],sl.kms=sl,lc.kms=lc,stringsAsFactors=F))
  }
  cat(paste(Id,'done!\n'))
}

###########

ddTable<-merge(
  x=kms[,c('experiment','eGrid','sl.kms','lc.kms')],
  y=t,
  by=c('experiment','eGrid'))
ddTable<-data.frame(Id=1:dim(ddTable)[1],ddTable,stringsAsFactors=F)

save(dd.sl,dd.lc,ddTable,file='dd2.RData')
load(file='dd2.RData')

make.shapefile(dd.lc,ddTable,'recapture_by_experiment_lcpaths',3)

