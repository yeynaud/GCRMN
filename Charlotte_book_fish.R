require(foreach)

# set the path of your working directory
setwd('/Users/yoaneynaud/Documents/Charlotte/GCRMN/')

# set the path to the folder containing the informations related to the fish
legendfish=read.csv('AdditionalFiles/TaxaFishComplete.csv',sep=';',header=T)

########################################## CREATING THE YELLOWBOOK ###########################################################
################################################ ONLY RUN ONCE ###############################################################
poissons=dir(path = 'OrganizeData/',pattern='Fish')
pb=txtProgressBar(min = 0, max = length(poissons), style = 3)
species_link=foreach(i=1:length(poissons))%do%{
  file=read.csv(paste('OrganizeData/',poissons[i],sep=''))
  setTxtProgressBar(pb, i)
  unique(paste(file$species,file$Region,file$Island,file$trophic,file$family,file$Year,file$Country))
}
close(pb)
names(species_link)=poissons
save(species_link,file='fish_links.Rdata')


########################################## THE FUNCTION ITSELF ###############################################################
##############################################################################################################################
# If you already have created and saved the yellowbook, this next line automatically loads it
if(file.exists('fish_links.Rdata'))load('fish_links.Rdata')
# Now we can load the function itself
source('ExtractGCRMNfish.R')
## here you can play and extract what you want and then save it as a csv etc.
## 'nameBiomass: it is the name of the last column
## 'Group': What group, Region, Island etc. are you interested in ? just put one 
#   or multiple arguments for what you would like to extract - OPEN THE FUNCTION TO SEE ALL POSSIBLE ARGUMENTS
############# here is an example for the group of all herbivores ########################
test1=ExtractGCRMNfish(nameBiomass = 'All Herbivores',Trophic = c('Herbivore','HD','HM'))
