
rm(list=ls())
library(chron)
library(ggplot2)
library(JurgenPKPD)
library(lme4)
library(rms)
library(plyr)
library(xtable)

setwd('~/documents/triathlons/uitslagen/ranking')
rs<-list.files()
rs<-rs[rs!='Oud']

#summarize data
adj<-do.call('rbind',lapply(rs,function(x){
db<-read.csv(x)

title<-gsub('.csv','',gsub('Uitslag_','',x))

colnames(db)<-gsub('\\.','',colnames(db))
colnames(db)<-gsub('X','',colnames(db))
if('Woonplaats'%in%colnames(db)){colnames(db)[colnames(db)%in%c('Woonplaats')]<-'Team'}
if('Total'%in%colnames(db)){colnames(db)[colnames(db)%in%c('Total')]<-'Totaal'}
if('Tot'%in%colnames(db)){colnames(db)[colnames(db)%in%c('Tot')]<-'Totaal'}
if('Run'%in%colnames(db)){colnames(db)[colnames(db)%in%c('Run')]<-'Loop'}
if('Swim'%in%colnames(db)){colnames(db)[colnames(db)%in%c('Swim')]<-'Zwem'}
if('Bike'%in%colnames(db)){colnames(db)[colnames(db)%in%c('Bike')]<-'Fiets'}
if('Chg1'%in%colnames(db)){colnames(db)[colnames(db)%in%c('Chg1')]<-'Wis1'}
if('Chg2'%in%colnames(db)){colnames(db)[colnames(db)%in%c('Chg2')]<-'Wis2'}
if('T1'%in%colnames(db)){colnames(db)[colnames(db)%in%c('T1')]<-'Wis1'}
if('T2'%in%colnames(db)){colnames(db)[colnames(db)%in%c('T2')]<-'Wis2'}
if('Name'%in%colnames(db)){colnames(db)[colnames(db)%in%c('Name')]<-'Naam'}
if('Teamnaam'%in%colnames(db)){colnames(db)[colnames(db)%in%c('Teamnaam')]<-'Team'}
if('ClubWoonplaats'%in%colnames(db)){colnames(db)[colnames(db)%in%c('ClubWoonplaats')]<-'Team'}

if(grepl('triple',x)){
  colnames(db)[grepl('Bike',colnames(db))]<-paste0('Fiets',1:3)
  colnames(db)[grepl('Swim',colnames(db))]<-paste0('Zwem',1:3)
  colnames(db)[grepl('Run',colnames(db))]<-paste0('Loop',1:3)
  colnames(db)[grepl('_T',colnames(db))]<-paste0('Wis',1:6)
  for(i in c(paste0('Fiets',1:3),paste0('Wis',1:6),paste0('Zwem',1:3),paste0('Loop',1:3))){
    db[,i]<-as.character(db[,i])
    db[!is.na(db[,i]),i]<-paste0("00:",db[!is.na(db[,i]),i])
    }
  for(i in c(paste0('Wis',1:5))){db[!grepl(":01:",db[,i])&!is.na(db[,i]),i]<-paste0("00:",db[!grepl(":01:",db[,i])&!is.na(db[,i]),i])}
  db$Zwem3[db$Zwem3=="00:16:59:12"]<-"16:59:12"
  }

cols<-colnames(db)[grepl('Fiets',colnames(db))|grepl('Zwem',colnames(db))|grepl('Loop',colnames(db))|grepl('Totaal',colnames(db))|grepl('Wis',colnames(db))]
for(i in cols){
  if(class(db[,i])!='numeric'){
  db[,i]<-24*60*60*as.numeric(times(as.character(db[,i])))
  }
}

db$Naam<-as.character(db$Naam)
# db$Naam<-do.call('rbind',lapply(strsplit(db$Naam," "),function(x){return(x[1])}))

if(grepl('nijmegen',x)){
db$loop<-db$Loop1+db$Loop2
db$fiets<-db$Fiets1+db$Fiets2
db$zwem<-db$Zwem1+db$Zwem2
db$wissel<-db$Wis1+db$Wis2+db$Wis3+db$Wis4
} else{
  if(grepl('triple',x)){
    db$loop<-db$Loop1+db$Loop2+db$Loop3
    db$fiets<-db$Fiets1+db$Fiets2+db$Fiets3
    db$zwem<-db$Zwem1+db$Zwem2+db$Zwem3
  } else{
  db$loop<-db$Loop
  db$fiets<-db$Fiets
  db$zwem<-db$Zwem}
  if(!grepl('Rutbeek',x)){
    if(grepl('triple',x)){db$wissel<-db$Wis1+db$Wis2+db$Wis3+db$Wis4+db$Wis5+db$Wis6}
    if(grepl('almere',x)|grepl('groningen',x)){db$wissel<-db$Wiss1+db$Wiss2} else {db$wissel<-db$Wis1+db$Wis2}
    } else {db$wissel<-db$Wissel1}
}


df<-db[,c('Naam','Team','zwem','fiets','loop','wissel','Totaal','Divisie')]
df$wedstrijd<-title

return(df)
}))

#percentage verschil
adj$Divisie[adj$Divisie=='4e Divisie']<-'4e divisie'
div<-lapply(unique(adj$wedstrijd),function(i){
  mult<-unique(adj$Divisie[adj$wedstrijd==i])
  if(length(mult)>1){
    t=lapply(mult,function(k){
      times=adj$Totaal[adj$Divisie==k&adj$wedstrijd==i]
      time<-mean(times[order(times)][1:20])
      return(time)
      })
    names(t)<-mult
    return(t)
  }
})
names(div)<-unique(adj$wedstrijd)
div[sapply(div, is.null)] <- NULL

div<-as.data.frame(do.call('rbind',lapply(div,function(i){
  for(j in unique(adj$Divisie)){
    if(!j%in%names(i)){
      i[[length(i)+1]]<-0
      names(i)[length(i)]<-j
    }
  }
  corr=c('1e divisie','2e divisie','3e divisie','4e divisie','ere divisie')
  y<-i[match(corr,names(i))]
  return(y)
})))

corr23=mean(unlist(div$`3e divisie`)[c(1,3)]/unlist(div$`2e divisie`)[c(1,3)])
corr34=mean(unlist(div$`4e divisie`)[c(1:3)]/unlist(div$`3e divisie`)[c(1:3)])
corr21=div$`1e divisie`[[3]]/div$`2e divisie`[[3]]
corr24=mean(mean(unlist(div$`4e divisie`)[c(1,3)]/unlist(div$`2e divisie`)[c(1,3)]),corr34*corr23)

corr03=mean(mean(unlist(div$`ere divisie`)[c(2)]/unlist(div$`3e divisie`)[c(2)]))
corr04=mean(mean(unlist(div$`ere divisie`)[c(2)]/unlist(div$`4e divisie`)[c(2)]))
corr01=mean(mean(unlist(div$`ere divisie`)[c(4)]/unlist(div$`1e divisie`)[c(4)]))

#middelen voor ere divisie
corr20=mean(corr04*corr24,corr03*corr23,corr01*corr21)


uitslag<-do.call('rbind',lapply(unique(adj$wedstrijd), function(i){
  ds=adj[adj$wedstrijd==i,]
  y=do.call('rbind',lapply(unique(ds$Divisie),function(j){
    df=ds[ds$Divisie==j,]
    corr=1
    if(j=='ere divisie'){corr<-corr20}
    if(j=='1e divisie'){corr<-corr21}
    if(j=='3e divisie'){corr<-corr23}
    if(j=='4e divisie'){corr<-corr24}
    streef=mean(df$Totaal[order(df$Totaal)][1:5])/corr
    szwem=mean(df$zwem[order(df$Totaal)][1:5])#/corr
    sloop=mean(df$loop[order(df$Totaal)][1:5])#/corr
    sfiets=mean(df$fiets[order(df$Totaal)][1:5])#/corr
    swissel=mean(df$wissel[order(df$Totaal)][1:5])#/corr
    
    df$zwem<-(df$zwem-szwem)/szwem
    df$loop<-(df$loop-sloop)/sloop
    df$fiets<-(df$fiets-sfiets)/sfiets
    df$wissel<-(df$wissel-swissel)/swissel
    df$Totaal<-(df$Totaal-streef)/streef
    return(df[grepl('Hellas',df$Team)|grepl('Juul van de Kruijs',df$Naam),])
  }))
  return(y)
  
}))

def<-do.call('rbind',lapply(unique(uitslag$Naam),function(i){
  y<-uitslag[uitslag$Naam==i,]
  
  ind<-abs((y$Totaal-mean(y$Totaal))/mean(y$Totaal))
  if(length(ind[ind>0.5])==1){cut=y$Totaal[ind==max(ind)]} else {cut=999}
  if(i=='Luuk Vermunt'){y<-y[y$wedstrijd!='almereDUIN_sprint',]} #individuele exclusie
  z<-y[y$Totaal!=cut,]
  if(nrow(z)<nrow(y)){excl=y$wedstrijd[ind>0.5]} else {excl='none'}
  x<-as.data.frame(t(as.data.frame(apply(z[,3:7],2,mean,na.rm=TRUE))))
  x$Naam<-i
  x$nowedstrijd=nrow(z)
  incl<-paste(paste0(y$wedstrijd[y$Totaal!=cut]," (",round(100*y$Totaal[y$Totaal!=cut],digits=1),")"),collapse = ", ")
  x$incl<-incl
 if(excl!='none'){ x$excl=paste0(y$wedstrijd[y$Totaal==cut]," (",round(100*y$Totaal[y$Totaal==cut],digits=1),")")} else { x$excl=excl}
  
  return(x)
}))
row.names(def)<-1:nrow(def)

def<-def[order(def$Totaal),]

for(i in 1:5){
  def[,i]<-round(def[,i]*100,digits = 2)
}
hlines=c(0,0,1:nrow(def))

def$incl<-NULL
def$excl<-NULL

setwd('/Users/jurgenlangenhorst/Documents/Triathlons/')
print.xtable(xtable(def,caption = 'Team-Triathlon ranking 2018'),
             hline.after = hlines,
             include.colnames = T,include.rownames=FALSE,
             # sanitize.text.function = identity,
             type="latex", file="Ranking_2018.tex",row.names=F)
