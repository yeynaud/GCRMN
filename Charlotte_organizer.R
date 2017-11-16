## This code will allow you to reorganize the raw dataset into a more homogeneous format. It does alter the data while 
## doing so, so it is not just a direct transfer of the dataset into a new folder. 

# set the path of your working directory
the_path=setwd('/Users/yoaneynaud/Documents/Charlotte/GCRMN/')

# set the path to the folder containing all the raw csv files
rawdatalist=dir('Rawdata/', pattern='csv')

# set the path to the folder containing the informations related to the fish
legendfish=read.csv('AdditionalFiles/TaxaFishComplete.csv',sep=';',header=T)

# set the path to the folder containing the informations related to the corals
legendbenthos=read.csv('AdditionalFiles/TaxaCompleteGroup.csv',sep=';',header=T)



################################ RUN LINES 1 TO 152 FOLLOWING IN ONE SHOT ####################################
##############################################################################################################
require(foreach)
unfactor=function(X){return(as.numeric(as.character(X)))}
benthic=0
fish=0
invert=0
pb <- txtProgressBar(min = 0, max = length(rawdatalist), style = 3)
for(i in 1:length(rawdatalist)){
  setTxtProgressBar(pb, i)
  
  
  
  
  if(dir.exists('OrganizeData/')==FALSE){
    dir.create('OrganizeData/')
  }
  
  
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
        if(!is.na(legendbenthos$group[adonde][1])){
          test$Group[where]=names(which(table(as.character(legendbenthos$group[adonde]))==max(table(as.character(legendbenthos$group[adonde])))))
        }
        if(!is.na(legendbenthos$subgroup[adonde][1])){
          test$Subgroup[where]=names(which(table(as.character(legendbenthos$subgroup[adonde]))==max(table(as.character(legendbenthos$subgroup[adonde])))))
        }
        if(!is.na(legendbenthos$flesh.cal[adonde][1])){
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
  
  
  # fish files
  if(length(which(colnames(test)=='Biomass'))){
    
    fish=fish+1
    taxon=test$Taxon
    existing_bio=which(!is.na(test$Biomass))
    if(length(which(!is.na(test$Biomass)))){cat(rawdatalist[i],fill=T)}
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
    write.csv(test,paste('OrganizeData/Fish_',fish,'_',unlist(strsplit(rawdatalist[i],'.csv'))[1],'.csv',sep=''),row.names = FALSE)
    
    
  }
  
  
  if(1){
    # invert files
    if(length(which(colnames(test)=='abundance'))){
      invert=invert+1
      write.csv(test,paste('OrganizeData/Invert_',invert,'_',unlist(strsplit(rawdatalist[i],'.csv'))[1],'.csv',sep=''),row.names = FALSE)
    }}
  rm(test)
  rm(bbb)
  gc()
}
close(pb)
##############################################################################################################
##############################################################################################################

