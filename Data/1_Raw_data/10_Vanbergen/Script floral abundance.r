#Version 2 of the R-script
#Now we don't sum up floral abundance of all plant species anymore, but we just show the amount of flowers per plant species
#We also don't pool per landscape but give the scores per transect
#We still keep the weighting, as transect lengths differ
#Remember, each site was visited 3 times (sampling period A, B, C), so each habitat can have up to three different transects per site
#Also: use the Habitat.EUNIS column, not the Transect column. These are linked to each other, but it will avoid confusion.
#Load libraries
library(reshape2) #For dcast function
library(dplyr)
library(vegan)   #Necessary for the diversity function

#Now read the files with the insects, the flower quadrats and the sampling effort per transect
insectfile = read.csv("Data/1_Raw_data/10_Vanbergen/Interaction_data.csv")
plantfile = read.csv("Data/1_Raw_data/10_Vanbergen/Unprocessed_floral_counts.csv")
transectfile = read.csv("Data/1_Raw_data/10_Vanbergen/Sampling_effort.csv")


#Now look at the plantfile
#Within each site and sampling time, vegetation quadrats are nested in 3 levels:
#1: the transects (= habitat types)
#2: transects are divided in numbered subtransects
#3: in each subtransect, multiple vegetation plots were laid out (more in longer or more heterogenous subtransects)

#I am very naively going to assume that you just want a count of flowers, but you can do the following for any metric you would like:

#Using the dcast function, you can sort samples per plot and sum the floral abundance per species. This gives you a metric of floral abundance per species per plot (may be redundant as each plant species
#only has one record per plot, but I'll leave the code here for now)
#I have also included type of inflorescence counted in the resulting tables, as this may be informative
Perplot = dcast(plantfile, Site + Period + Habitat.EUNIS + Subtransect + Plot.ID + Flower.species + Inflorescence.Type.counted ~ ., fun.aggregate = sum, value.var="Nr.inflorescences.per.plot") 
names(Perplot)[names(Perplot) == '.'] = 'Floral.abundance'


#Now average over those to get the mean floral abundance per subtransect:
Persubtransect = dcast(Perplot, Site + Period + Habitat.EUNIS + Subtransect + Flower.species + Inflorescence.Type.counted ~ ., fun.aggregate = mean, value.var="Floral.abundance")
names(Persubtransect)[names(Persubtransect) == '.'] = 'Floral.abundance'


#Now averaging over all subtransects to get average floral abundance per plot in each single transect
Pertransect = dcast(Persubtransect, Site + Period + Habitat.EUNIS + Flower.species + Inflorescence.Type.counted ~ ., fun.aggregate = mean, value.var="Floral.abundance")
names(Pertransect)[names(Pertransect) == '.'] = 'Floral.abundance'


#Now, as said before, sampling efforts were different per transect. This is where the transectfile comes into play. Let's link both files using the field habitat.EUNIS (and of course Site and Period)
Pertransect.weighted = left_join(Pertransect, transectfile, by = c("Site", "Period", "Habitat.EUNIS"))

#We can weight by distance or by time. This does not make any difference, so we'll take distance as it normally should sum to 1000 m for each site
Pertransect.weighted$Weighted.floral.ab = Pertransect.weighted$Floral.abundance * Pertransect.weighted$Distance..m. /10
#And now sum this up over all transects per site and period.
#As you prefer to have the flower data at transect level and not at site level, I'll blank this code out for now.
#Final.floral = dcast(Pertransect.weighted, Site + Period ~ ., fun.aggregate = sum, value.var = "Weighted.floral.ab")
#names(Pertransect)[names(Pertransect) == '.'] = 'Floral.abundance'

#In this new version, the Floral abundance now refers to the mean amount of flowers per floral quadrat/plot per transect, multiplied by transect length.
#Before, floral abundance was divided by 1000 to get a metric of flowers per metre, but I have removed this division, as this would lead to very low numbers
#Since there are plants that are rare, were visited by pollinators but were not observed in the plots, you could give these a value of 1. It would be best to avoid 
#having other plants with a 'floral abundance' below 1 then, so we do not divide by 1000 (but feel free to change this)

#Save floral count data
write_csv(Pertransect.weighted, "Data/1_Raw_data/10_Vanbergen/Flower_count.csv")


