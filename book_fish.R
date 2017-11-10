require(foreach)

unfactor=function(X){
  return(as.numeric(as.character(X)))
}

legendfish=read.csv('/Users/yoaneynaud/Documents/Charlotte/GCRMN/AdditionalFiles/TaxaFishComplete.csv',sep=';',header=T)


setwd('/Users/yoaneynaud/Documents/Charlotte/GCRMN/OrganizeData/')


poissons=dir(pattern='Fish')

species_link=foreach(i=1:length(poissons))%do%{
  file=read.csv(poissons[i])
  unique(paste(file$species,file$Region,file$Island,file$trophic,file$family,file$Year,file$Country))
}
names(species_link)=poissons



#####################


# checker que la variable d'etat existe dans le fichier final.
# pour calculer biomasse totale il faut rajouter une colonne pour voir si toute la communaute a ete compte. 
## enlever RORC, Palau, Fiji, RMI, FSM, AmSamoa.
# passer year en character - DONE
# ajouter ReefZone
# ajouter people/no people
# checker a/b - DONE
# methode de compatge a corriger: SPC, Belt et Distance sampling
load('/Users/yoaneynaud/Documents/Charlotte/GCRMN/fish_links.Rdata')

#independent=TRUE,per_site=FALSE,
ExtractGCRMN=function(nameBiomass='group',perisland=TRUE,removeBIG=TRUE,nabiomass=TRUE,Species=NULL,Region=NULL,Island=NULL,Trophic=NULL,Family=NULL,Year=NULL,Country=NULL,allfish=0){
  require(foreach)
  legendfish=read.csv('/Users/yoaneynaud/Documents/Charlotte/GCRMN/AdditionalFiles/TaxaFishComplete.csv',sep=';',header=T)
  aggregator=function(X){
    #return(mean(sapply(split(unfactor(X$Biomass),as.character(X$TransID)),sum)))
    if(length(unlist(X))){
      if(!is.null(nrow(X))){
        x1=with(X, aggregate(unfactor(Biomass),list(Site_name,Island,Region,Country,Year,TransID),sum))
        names(x1)=c('Site_name','Island','Region','Country','Year','TransID','Biomass')
        x2=with(x1, aggregate(unfactor(Biomass),list(Site_name,Island,Region,Country,Year),mean))
        names(x2)=c('Site_name','Island','Region','Country','Year','Biomass')
        return(x2)
      }
      if(is.null(nrow(X))){
        z=c(as.character(X$Site_name),as.character(X$Island),as.character(X$Region),
            as.character(X$Country),as.character(X$Year),as.character(X$Biomass))
        
        names(z)=c('Site_name','Island','Region','Country','Year','Biomass')
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
  
  # dealing with all fish
  if(!is.null(Species)){
    if(Species=='all'){
      Species=NULL
      Trophic=as.character(unique(legendfish$trophic))
      Family=NULL
      allfish=TRUE
    }}
  
  les_mots_clefs=list()
  rem=foreach(x=1:length(list(Species,Region,Island,Trophic,Family,Year,Country)))%do%{
    if(!is.null(list(Species,Region,Island,Trophic,Family,Year,Country)[[x]])){les_mots_clefs[[length(les_mots_clefs)+1]]=list(Species,Region,Island,Trophic,Family,Year,Country)[[x]]}
  }
  rm(rem)
  keywords=as.matrix(expand.grid(les_mots_clefs))
  as.matrix(keywords)
  
  
  #if(!is.null(Year)){
  #  Year=as.character(Year)
  #  keywords=foreach(Y=Year,.combine=rbind)%do%{unlist(strsplit(paste(Species,Region,Island,Trophic,Family,Y,Country),
  #                                                           ' '))[which(unlist(strsplit(paste(Species,Region,Island,Trophic,Family,Y,Country),' '))!="")] }
  #  }else{
  #   keywords=unlist(strsplit(paste(Species,Region,Island,Trophic,Family,Year,Country),
  #                         ' '))[which(unlist(strsplit(paste(Species,Region,Island,Trophic,Family,Year,Country),' '))!="")]  
  #}
  
  cat('I am looking for the right files to open',fill=TRUE)
  
  loc=whereisit(keywords)
  if(length(loc)>=1){
    final=as.data.frame(foreach(i=1:length(loc),.combine=rbind)%do%{
      gc()
      cat(paste(100*round(i/length(loc),2),"%, I am extracting data from ",loc[i],sep=''), fill=T)
      donnee=read.csv(paste('/Users/yoaneynaud/Documents/Charlotte/GCRMN/OrganizeData/',loc[i],sep=''))
      
      if(removeBIG==TRUE){
        #removing bigger species
        big<-c("Chelonia mydas","Himantura fai","Manta alfredi","Manta birostris","Manta sp",
               "Aetobatus narinari","Aetobatus ocellatus","Aetobatus sp","Pastinachus sephen",
               "Carcharhinus albimarginatus","Carcharhinus amblyrhynchos","Carcharhinus galapagensis",
               "Carcharhinus melanopterus","Carcharhinus sp","Galeocerdo cuvier","Sphyrna lewini",
               "Negaprion acutidens","Triaenodon obesus","Euthynnus affinis","Thunnus sp","Thunnus albacares",
               "Dasyatidae sp","Dasyatis kuhlii","Dasyatis sephen","Neotrygon kuhlii","Taeniura meyeni","Caranx ignobilis")
        for(interdit in big){
          #cat(interdit,fill=T)
          ouca=which(donnee$species==interdit)
          if(length(ouca)){donnee=donnee[-ouca,]}
        }}
      
      if(allfish==TRUE){
        notfullcm=c('RORC', 'Palau', 'Fiji', 'RMI', 'FSM', 'AmSamoa')
        
        for(interdit in notfullcm){
          ouca=which(donnee$Country==interdit)
          if(length(ouca)){donnee=donnee[-ouca,]}
        }
      }
      
      #les_noms[which(donnee$species=='Acanthurus achilles X nigricans : hybrid= rackliffei')]='Acanthurus rackliffei'
      #cat(length(unlist(strsplit(as.character(donnee$species),' '))),fill=T)
      leslignes=list()
      # Species
      if(!is.null(Species)){
        les_noms=as.character(donnee$species)
        les_noms[which(is.na(as.character(donnee$species)))]='NA NA'
        genus=sapply(strsplit(les_noms,' '),function(x) x[1])
        if(length(unlist(strsplit(Species,' ')))==1){leslignes[[length(leslignes)+1]]=which(genus==Species)}
        if(length(unlist(strsplit(Species,' ')))>1){leslignes[[length(leslignes)+1]]=which(donnee$species==Species)}
      }
      # Region
      if(!is.null(Region)){
        leslignes[[length(leslignes)+1]]=foreach(lesregion=Region,.combine=c)%do%{which(donnee$Region==lesregion)}}
      
      # Island
      if(!is.null(Island)){
        leslignes[[length(leslignes)+1]]=foreach(lesiles=Island,.combine=c)%do%{which(donnee$Island==lesiles)}}
      
      # Trophic
      if(!is.null(Trophic)){
        leslignes[[length(leslignes)+1]]=foreach(trophique=Trophic,.combine=c)%do%{which(donnee$trophic==trophique)}}
      
      # Family
      if(!is.null(Family)){
        leslignes[[length(leslignes)+1]]=foreach(famille=Family,.combine=c)%do%{which(donnee$Family==famille)}}
      
      if(!is.null(Year)){
        leslignes[[length(leslignes)+1]]=foreach(annee=Year,.combine=c)%do%{which(donnee$Year==annee)}}
      
      # Country
      if(!is.null(Country)){
        leslignes[[length(leslignes)+1]]=foreach(pays=Country,.combine=c)%do%{which(donnee$Country==pays)}}
      
      b=donnee[as.numeric(names(table(unlist(leslignes))[which(table(unlist(leslignes))==length(leslignes))])),]
      #cat(unique(sapply(strsplit(as.character(unique(b$species)),' '),function(x) x[1])),fill=T)
      as.matrix(b)
    },row.names=NULL)
  }else{'Sorry, I do not have this information'}
  
  
  
  
  #if(!is.null(Year)){
  #  finalr=final[foreach(y=Year,.combine=c)%do%{
  #    which(final$Year==y)
  #  },]}else{finalr=final}
  
  if(nabiomass){
    if(length(which(is.na(final$Biomass)))){
      final=final[-which(is.na(final$Biomass)),]
    }
  }
  splitted=split(final, final$SurveyID)
  cat('I am now creating independent data points',fill=TRUE)
  cestbeau=do.call(rbind,lapply(splitted,aggregator))
  #names(cestbeau)=c('Country','Region','Island','Year',paste(nameBiomass,' Biomass',sep=''))
  if(perisland==TRUE){
    cat('I am now creating means for each island',fill=TRUE)
    cestbeau=with(cestbeau, aggregate(Biomass,list(Country,Region,Island,Year),mean))
    names(cestbeau)=c('Country','Region','Island','Year',paste(nameBiomass,' Biomass',sep=''))
  }
  
  return(cestbeau)
  
}

test2=ExtractGCRMN(nameBiomass = 'All Herbivores',Trophic = c('Herbivore','HD','HM'))
write.csv(test2,file='/Users/yoaneynaud/Documents/Charlotte/GCRMN/All_herbivore.csv',row.names=FALSE)

# Carnivore+Piscivore
# Herbivore+HD+HM


laregression=function(x){
  
  if(nrow(x)>2){
    plot(log(unfactor(x$'All Herbivores Biomass'))~x$Year)
    
    abline(lm(log(unfactor(x$'All Herbivores Biomass'))~x$Year))
    
  }
}

per_ile=split(test2,as.character(test2$Island))
essai=per_ile$Hawaii

