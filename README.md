# Alien-Vertebrates-in-Mountains
Data and codes for the statistical analyses implemented in: The global status of alien vertebrates in mountains. Garcia-Rodriguez et al. 

This repository contains:
1. Spp.nat.realms.csv: A dataset with information on native realms of over 37K vertebrate species. This data was used as realm native pools to perform the random draws of potentially introduced species and define the distribution of expected flows.
 
2. Table_Flows.csv: Information on the number of species donated and received by each real, overall and for each taxonomic group

3. AllFromTo.csv: Specific realm to realm flows, showing the overall number of species involved for each of the 55 flows detected (both inter and intra-realms)

4. Nullmod_Flows.R: Null models developed to identify higher or lower than expected flow, based on data in points 1,2 and 3.
   
5. DataPredictors.csv: Values of overall alien richness and alien richness for each taxonomic group as well as the quantified predictors for the > 2500 mountains studied)
   
6. GLM.R: GLMs implemented to identify the predictors that better explain the variation in alien richness across mountains worlwide, using the data provided in point 5.
