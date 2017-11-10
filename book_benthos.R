require(foreach)

unfactor=function(X){
  return(as.numeric(as.character(X)))
}

legendbenthos=read.csv('/Users/yoaneynaud/Documents/Charlotte/GCRMN/AdditionalFiles/TaxaCompleteGroup.csv',sep=';',header=T)


setwd('/Users/yoaneynaud/Documents/Charlotte/GCRMN/OrganizeData/')


benthos=dir(pattern='Benthic')

species_link=foreach(i=1:length(benthos))%do%{
  file=read.csv(benthos[i])
  unique(paste(file$Group,file$Subgroup,file$calcifier,file$Region,file$Island,file$Year,file$Country))
}
names(species_link)=benthos
save(species_link,file='/Users/yoaneynaud/Documents/Charlotte/GCRMN/benthos_links.Rdata')


#####################


# checker que la variable d'etat existe dans le fichier final.
# pour calculer biomasse totale il faut rajouter une colonne pour voir si toute la communaute a ete compte. 
## enlever RORC, Palau, Fiji, RMI, FSM, AmSamoa.
# passer year en character - DONE
# ajouter ReefZone
# ajouter people/no people
# checker a/b - DONE
# methode de compatge a corriger: SPC, Belt et Distance sampling
load('/Users/yoaneynaud/Documents/Charlotte/GCRMN/benthos_links.Rdata')

#independent=TRUE,per_site=FALSE,
ExtractGCRMNbenthos=function(nameCover='group',perisland=TRUE,nacover=TRUE,Group=NULL,Region=NULL,Island=NULL,Subgroup=NULL,Calcifier=NULL,Year=NULL,Country=NULL){
  require(foreach)
  legendbenthos=read.csv('/Users/yoaneynaud/Documents/Charlotte/GCRMN/AdditionalFiles/TaxaCompleteGroup.csv',sep=';',header=T)
  aggregatorcover=function(X){
    #return(mean(sapply(split(unfactor(X$Biomass),as.character(X$TransID)),sum)))
    if(length(unlist(X))){
      if(!is.null(nrow(X))){
        x1=with(X, aggregate(unfactor(cover),list(Site_name,Island,Region,Country,Year,TransID),sum))
        names(x1)=c('Site_name','Island','Region','Country','Year','TransID','Cover')
        x2=with(x1, aggregate(unfactor(Cover),list(Site_name,Island,Region,Country,Year),mean))
        names(x2)=c('Site_name','Island','Region','Country','Year','Cover')
        return(x2)
      }
      if(is.null(nrow(X))){
        z=c(as.character(X$Site_name),as.character(X$Island),as.character(X$Region),
            as.character(X$Country),as.character(X$Year),as.character(X$cover))
        
        names(z)=c('Site_name','Island','Region','Country','Year','Cover')
        return(data.frame(t(z),row.names = NULL))
      }}
  }
  whereisit=function(keywords){
    if(!is.null(nrow(keywords))){
      where=unique(foreach(i=nrow(keywords),.combine=c)%do%{
        key=keywords[i,]
        res=table(foreach(cherche=key,.combine=c)%do%{
          lecheck=function(x){if(length(which(x==cherche))!=0){return(TRUE)}else{return(FALSE)}} 
          
          presence=function(X){
            pres=length(which(unlist(lapply(strsplit(X,' '),function(y) lecheck(y)))==TRUE))
            
            if(pres!=0){return(TRUE)}else{return(FALSE)}
          }
          names(sapply(species_link, presence)[which(sapply(species_link, presence)==TRUE)])
        })
        
        where_it_is=names(res[which(res==length(key))])
        where_it_is
      })}
    
    if(is.null(nrow(keywords))){
      
      res=table(foreach(cherche=keywords,.combine=c)%do%{
        lecheck=function(x){if(length(which(x==cherche))!=0){return(TRUE)}else{return(FALSE)}} 
        
        presence=function(X){
          pres=length(which(unlist(lapply(strsplit(X,' '),function(y) lecheck(y)))==TRUE))
          
          if(pres!=0){return(TRUE)}else{return(FALSE)}
        }
        names(sapply(species_link, presence)[which(sapply(species_link, presence)==TRUE)])
      })
      
      where=names(res[which(res==length(keywords))])
      
    }
    
    return(where)
  }
  
  
  les_mots_clefs=list()
  rem=foreach(x=1:length(list(Subgroup,Region,Island,Group,Calcifier,Year,Country)))%do%{
    if(!is.null(list(Subgroup,Region,Island,Group,Calcifier,Year,Country)[[x]])){les_mots_clefs[[length(les_mots_clefs)+1]]=list(Subgroup,Region,Island,Group,Calcifier,Year,Country)[[x]]}
  }
  rm(rem)
  keywords=as.matrix(expand.grid(les_mots_clefs))
  as.matrix(keywords)
  
  
  cat('I am looking for the right files to open',fill=TRUE)
  
  loc=whereisit(keywords)
  if(length(loc)>=1){
    final=as.data.frame(foreach(i=1:length(loc),.combine=rbind)%do%{
      gc()
      cat(paste(100*round(i/length(loc),2),"%, I am extracting data from ",loc[i],sep=''), fill=T)
      donnee=read.csv(paste('/Users/yoaneynaud/Documents/Charlotte/GCRMN/OrganizeData/',loc[i],sep=''))
      if(length(which(donnee$community=='Problematic'))!=0){donnee=donnee[-c(which(donnee$community=='Problematic')),]}
      
      #les_noms[which(donnee$species=='Acanthurus achilles X nigricans : hybrid= rackliffei')]='Acanthurus rackliffei'
      #cat(length(unlist(strsplit(as.character(donnee$species),' '))),fill=T)
      leslignes=list()
      # Species
      if(!is.null(Group)){
        leslignes[[length(leslignes)+1]]=foreach(lesregion=Group,.combine=c)%do%{which(donnee$Group==lesregion)}}
      
      # Region
      if(!is.null(Subgroup)){
        leslignes[[length(leslignes)+1]]=foreach(lesregion=Subgroup,.combine=c)%do%{which(donnee$Subgroup==lesregion)}}
      
      # Island
      if(!is.null(Calcifier)){
        leslignes[[length(leslignes)+1]]=foreach(lesiles=Calcifier,.combine=c)%do%{which(donnee$calcifier==lesiles)}}
      
      # Island
      if(!is.null(Island)){
        leslignes[[length(leslignes)+1]]=foreach(lesiles=Island,.combine=c)%do%{which(donnee$Island==lesiles)}}
      
      # Trophic
      if(!is.null(Region)){
        leslignes[[length(leslignes)+1]]=foreach(trophique=Region,.combine=c)%do%{which(donnee$Region==trophique)}}
      
      if(!is.null(Country)){
        leslignes[[length(leslignes)+1]]=foreach(trophique=Country,.combine=c)%do%{which(donnee$Country==trophique)}}
      
      if(!is.null(Year)){
        leslignes[[length(leslignes)+1]]=foreach(annee=Year,.combine=c)%do%{which(donnee$Year==annee)}}
      
      
      b=donnee[as.numeric(names(table(unlist(leslignes))[which(table(unlist(leslignes))==length(leslignes))])),]
      #cat(unique(sapply(strsplit(as.character(unique(b$species)),' '),function(x) x[1])),fill=T)
      as.matrix(b)
    },row.names=NULL)
  }else{'Sorry, I do not have this information'}
  
  
  
  
  #if(!is.null(Year)){
  #  finalr=final[foreach(y=Year,.combine=c)%do%{
  #    which(final$Year==y)
  #  },]}else{finalr=final}
  
  if(nacover){
    if(length(which(is.na(final$cover)))){
      final=final[-which(is.na(final$cover)),]
    }
  }
  splitted=split(final, final$SurveyID)
  cat('I am now creating independent data points',fill=TRUE)
  cestbeau=do.call(rbind,lapply(splitted,aggregatorcover))
  #names(cestbeau)=c('Country','Region','Island','Year',paste(nameBiomass,' Biomass',sep=''))
  if(perisland==TRUE){
    cat('I am now creating means for each island',fill=TRUE)
    cestbeau=with(cestbeau, aggregate(Cover,list(Country,Region,Island,Year),mean))
    names(cestbeau)=c('Country','Region','Island','Year',paste(nameCover,' Cover',sep=''))
  }
  
  return(cestbeau)
  
}


test=ExtractGCRMNbenthos(nameCover = 'Coral',Group = 'coral')


# per ile
plot(unfactor(test$Year),as.numeric(as.character(test$`Coral Cover`)),las=1,col='grey')
moyenne=tapply(test$`Coral Cover`,list(test$Year),mean)
model=lm(moyenne~as.numeric(names(moyenne)))
abline(model,lty=3,lwd=2)
summary(model)
points(names(moyenne),moyenne,pch=16,col='dodgerblue')


test2=ExtractGCRMNbenthos(nameCover = 'Coral',Group = 'coral',perisland = FALSE)
# per survey
plot(unfactor(test2$Year),as.numeric(as.character(test2$Cover)),las=1,col='grey')
moyenne=tapply(test2$Cover,list(test2$Year),mean)
model=lm(moyenne~as.numeric(names(moyenne)))
abline(model,lty=3,lwd=2)
summary(model)
points(names(moyenne),moyenne,pch=16,col='dodgerblue')



