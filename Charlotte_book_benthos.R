require(foreach)

# set the path of your working directory
setwd('/Users/yoaneynaud/Documents/Charlotte/GCRMN/')

# set the path to the folder containing the informations related to the corals
legendbenthos=read.csv('AdditionalFiles/TaxaCompleteGroup.csv',sep=';',header=T)

########################################## CREATING THE YELLOWBOOK ###########################################################
################################################ ONLY RUN ONCE ###############################################################
benthos=dir(path = 'OrganizeData/',pattern='Benthic')
pb=txtProgressBar(min = 0, max = length(benthos), style = 3)
species_link=foreach(i=1:length(benthos))%do%{
  file=read.csv(paste('OrganizeData/',benthos[i],sep=''))
  setTxtProgressBar(pb, i)
  unique(paste(file$Group,file$Subgroup,file$calcifier,file$Region,file$Island,file$Year,file$Country))
}
close(pb)
names(species_link)=benthos
save(species_link,file='benthos_links.Rdata')

########################################## THE FUNCTION ITSELF ###############################################################
##############################################################################################################################
# If you already have created and saved the yellowbook, this next line automatically loads it
if(file.exists('benthos_links.Rdata'))load('benthos_links.Rdata')
# Now we can load the function itself
source('ExtractGCRMNbenthos.R')
## here you can play and extract what you want and then save it as a csv etc.
## 'nameCover': it is the name of the last column
## 'Group': What group, Region, Island etc. are you interested in ? just put one 
#   or multiple arguments for what you would like to extract - OPEN THE FUNCTION TO SEE ALL POSSIBLE ARGUMENTS
############# here is an example for the group 'coral' ########################
test1=ExtractGCRMNbenthos(nameCover = 'Coral',Group = 'coral',perisland = TRUE)
############# here is an example for the group 'macroalgae' ########################
test2=ExtractGCRMNbenthos(nameCover = 'Macroalgae',Group = 'macroalgae',perisland = TRUE)
############# here is an example for the group 'macroalgae' ########################
test3=ExtractGCRMNbenthos(nameCover = 'Stylophora',Subgroup = 'Stylophora',perisland = TRUE)

