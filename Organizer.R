require(foreach)

unfactor=function(X){return(as.numeric(as.character(X)))}


setwd('/Users/yoaneynaud/Documents/Charlotte/GCRMN/')

setwd('/Users/yoaneynaud/Documents/Charlotte/GCRMN/Rawdata/')
rawdatalist=dir()

verif=data.frame(foreach(i=rawdatalist,.combine=rbind)%do%{
  cat(i,fill=T)
  b=read.csv(i,sep=';')
  if(length(which(colnames(b)=='cover'))){
    tocheck=unique(as.character(unique(b$DataLevel)))
    ppp=foreach(p=tocheck,.combine=rbind)%do%{
      if(length(which(b$DataLevel==p))!=0){
        cbind(p,unique(as.character(b$taxon[which(b$DataLevel==p)])))
      }
    }
    colnames(ppp)=c('level','labels')
  }
  rm(b)
  gc()
  ppp
})
bb=split(verif$labels,verif$level)
tt=list()
foreach(p=bb)%do%{
  if(length(p)){
    tt[[length(tt)+1]]=unique(p)
  }
}

names(tt)=c( "fun","gen","genus","low")

##############################################################################################################
##############################################################################################################


rawdatalist=dir('Rawdata/')

legendfish=read.csv('AdditionalFiles/TaxaFishComplete.csv',sep=';',header=T)
legendbenthos=read.csv('/Users/yoaneynaud/Documents/Charlotte/GCRMN/AdditionalFiles/TaxaCompleteGroup.csv',sep=';',header=T)

benthic=0
fish=0
invert=0

##############################################################################################################

for(i in 1:length(rawdatalist)){
  cat(i,fill=T)
  # reading the original file
  test=read.csv(paste('Rawdata/',rawdatalist[i],sep=''),header=T,sep=';')
  
  
  # benthic files
  if(length(which(colnames(test)=='cover'))){
    
    
    duplicate_killer=function(x){
      unite=unlist(apply(x,1,function(X) paste(X,collapse='')))
      #unite=unlist(apply(data.frame(x$SurveyID,x$TransID,x$RepID,x$taxon,x$cover),1,function(X) paste(X,collapse='_')))
      doublon=names(which(table(unite)!=1))
      X=x
      if(length(doublon)){
        cat('WARNING: I found ',length(doublon),'duplicates, I am correcting the dataset accordingly',fill=T)
        X=X[-c(foreach(xx=doublon,.combine=c)%do%{which(unite==xx)}),]
        X=rbind(X,x[foreach(xx=doublon,.combine=c)%do%{which(unite==xx)[1]},])}
      return(X)
    }
    
    test=duplicate_killer(test)
    #test=duplicate_killer(test)
    benthic=benthic+1
    test$Group=NA
    test$Subgroup=NA
    test$calcifier=NA 
    exist=unique(as.character(test$taxon))
    for(gr in 1:length(exist)){
      if(exist[gr]!="Taxon"&exist[gr]!="Cover"){
        where=which(test$taxon==exist[gr])
        adonde=which(legendbenthos$taxon==exist[gr])
        if(!is.na(legendbenthos$group[adonde])){
          test$Group[where]=names(which(table(as.character(legendbenthos$group[adonde]))==max(table(as.character(legendbenthos$group[adonde])))))
        }
        if(!is.na(legendbenthos$subgroup[adonde])){
          test$Subgroup[where]=names(which(table(as.character(legendbenthos$subgroup[adonde]))==max(table(as.character(legendbenthos$subgroup[adonde])))))
        }
        if(!is.na(legendbenthos$flesh.cal[adonde])){
          test$calcifier[where]=names(which(table(as.character(legendbenthos$flesh.cal[adonde]))==max(table(as.character(legendbenthos$flesh.cal[adonde])))))
        }
      }}
    
    test$community='Problematic'
    test$globalcover=NA
    communitycheck=function(Y){
      mastermind=function(X){
        
        doublecheck=function(x){
          if(sum(x$cover,na.rm=T)==100){x$community='Complete'
          x$globalcover=sum(x$cover,na.rm=T)
          return(x)}
          if(sum(x$cover,na.rm=T)>105){x$community='Problematic'
          x$globalcover=sum(x$cover,na.rm=T)
          return(x)}
          if(sum(x$cover,na.rm=T)<0){x$community='Problematic'
          x$globalcover=sum(x$cover,na.rm=T)
          return(x)}
          if(sum(x$cover,na.rm=T)>0&sum(x$cover,na.rm=T)<100){x$community='Incomplete'
          x$globalcover=sum(x$cover,na.rm=T)
          return(x)}
        }
        if(length(split(X,X$TransID))>1){
          do.call(rbind,lapply(split(X,X$TransID),doublecheck))}else{
            doublecheck(X)
          }
        
      }
      
      return(do.call(rbind,lapply(split(Y,Y$SurveyID) ,mastermind)))}
    bbb=communitycheck(test)
    
    write.csv(bbb,paste('OrganizeData/Benthic_',benthic,'_',unlist(strsplit(rawdatalist[i],'.csv'))[1],'.csv',sep=''),row.names = FALSE)
  }
  if(0){
    # fish files
    if(length(which(colnames(test)=='Biomass'))){
      fish=fish+1
      taxon=test$Taxon
      test=test[,-c(1,3,5,6,7,8,12,36,39,40,41,42)]
      
      test=data.frame(foreach(j=unique(as.character(taxon)),.combine=rbind)%do%{
        if(length(which(taxon==j))!=1){
          a=cbind(test[which(taxon==j),],sapply(legendfish[which(legendfish$name==j)[1],c(4:9)],function(x) rep(x,length(which(taxon==j)))))
        }
        if(length(which(taxon==j))==1){
          a=cbind(test[which(taxon==j),],legendfish[which(legendfish$name==j)[1],c(4:9)])
        }
        a
      })
      
      test$calculated_biomass=NA
      test$calculated_biomass=is.na(test$Biomass)&!is.na(test$Size)
      where=which(is.na(test$Biomass)&!is.na(test$Size))
      test$Biomass[where]=test$Abundance[where]*unfactor(test$a[where])*unfactor(test$Size[where])^(unfactor(test$b[where]))
      write.csv(test,paste('OrganizeData/Fish_',fish,'_',rawdatalist[i],'.csv',sep=''),row.names = FALSE)
    }
    
    # invert files
    if(length(which(colnames(test)=='abundance'))){
      invert=invert+1
      write.csv(test,paste('OrganizeData/Invert_',invert,'_',rawdatalist[i],'.csv',sep=''),row.names = FALSE)}
  }
  rm(test)
  rm(bbb)
  gc()
}

##############################################################################################################
##############################################################################################################


setwd('/Users/yoaneynaud/Documents/Charlotte/GCRMN/OrganizeData/')

benthos=dir(pattern='Benthic')

verif=foreach(i=benthos,.combine=rbind)%do%{
  cat(i,fill=T)
  b=read.csv(i)
  b=with(b, data.frame(SurveyID,TransID,community,globalcover))
  gc()
  b=aggregate(b$globalcover,list(b$community,b$SurveyID,b$TransID),mean)
  colnames(b)=c('Community','SurveyID','TransID','cover_sum')
  b$file=i
  b
}
100*table(verif$Community)/nrow(verif)

length(unique(verif$SurveyID[which(verif$cover_sum>100)]))/length(unique(verif$SurveyID))


