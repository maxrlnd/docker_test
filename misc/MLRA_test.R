"
Scratchpad for estimating forage growth potential by 
Major Land Resource Area (MLRA) plant community 
production. 

Sources: 

  1) http://directives.sc.egov.usda.gov/OpenNonWebContent.aspx?content=17737.wba

Data: 

 1) https://esis.sc.egov.usda.gov/WelcomeFSG/pgFSGSelectFormat.aspx

"

# Get COOP sites if not present
if(!exists("coops")){
  source("R/coop_scraper.R") 
}
  
####Compute forage potential weights####
forage_mlra=read.table("misc/co_mlra_test.txt",sep="|")
forage_mlra[,1]=substr(forage_mlra[,1],3,4) # Get MRLA code 
forage_mlra=forage_mlra[!forage_mlra$V1 %in% c("00","01","99"),] # Remove placeholder plant growth sites
forage_mlra=forage_mlra[,c(1,which(names(forage_mlra)=="V4"):ncol(forage_mlra))] # subset ID and plant growth curve
names(forage_mlra)=c("MLRA","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

forage_mlra=aggregate(.~MLRA,data=forage_mlra,FUN=mean) # Compute mean plant growth curves by MLRA
forage_mlra[,-1]=forage_mlra[,-1]/100 # convert to decimal to match drought calculator fgp weights

####Assign forage potential weights to COOP sites####
library(rgdal)
mlra=readOGR("misc","mlra_v42")

getMRLAWeights<-function(coop){
  
  coop.pt=SpatialPoints(t(rev(coop$loc)),proj4string=CRS(proj4string(mlra)))
  fwt=forage_mlra[forage_mlra$MLRA==(coop.pt %over% mlra)$MLRARSYM,][,-1]
  
}