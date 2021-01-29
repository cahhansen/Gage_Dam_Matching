library(rgdal)
library(spatialEco)
library(nhdplusTools)
library(sf)
library(dataRetrieval)
library(pbapply)
library(dplyr)
library(ggplot2)
library(plotly)


#Simple approach to matching dams and gages and then retrieving data.
#Demonstrate in one HUC4 (0304) (This sub-region contains the Yadkin River Reservoir System, so it represents a relatively complex case, with multiple reservoirs and multiple gages)
#Test gage matching to select dams 

#Load and format spatial data
dams <- readOGR(dsn='ReservoirGageMatching.gpkg','GRanD_Dams_v1_1_US',stringsAsFactors = F)
dams$ID <- paste0(dams$EHAID,'-',dams$NIDID) #Compound identifier based on existing hydropower assets id and the id used in the National Inventory of Dams
dams[dams$OTHERSTRUCTUREID=="NA" & !is.na(dams$OTHERSTRUCTUREID),"OTHERSTRUCTUREID"] <- NA #Keep only main dams
HUCs <- readOGR(dsn='ReservoirGageMatching.gpkg','WBD_SubBasin_TestHUC0304',stringsAsFactors = F)

#Add HUC information to the dams with a quick spatial join
dams_usgsalbers <- spTransform(dams,crs(HUCs))
dams_usgsalbers <- point.in.poly(dams_usgsalbers,HUCs)

#Subset to HUC used for testing
dams_test <- dams_usgsalbers[dams_usgsalbers$HUC_4=='0304' & !is.na(dams_usgsalbers$HUC_4) & is.na(dams_usgsalbers$OTHERSTRUCTUREID),]
plot(dams_test)


#Function to trace to gages upstream (upstreamTributaries) or downstream (downstreamMain)
tracedam2gage <- function(inputdams,myid,searchmode,searchdistance,plotting=F){
  damtemp <- inputdams[inputdams$ID==myid,] #Select an individual reservoir
  #Pull Coordinates from the reservoir and match to comid
  start_point <- st_sfc(st_point(c(as.numeric(damtemp$LON), as.numeric(damtemp$LAT))), crs = 4269)
  start_comid <- discover_nhdplus_id(start_point) #Determines the comid most closely matched to the dam point. This assumes the coordinates of the dam are accurate.

  if(start_comid >1 & start_comid<2147483647 & !is.null(start_comid)){ #Check it's a valid comid
    nldi_feature_temp <- list(featureSource = "comid", featureID = start_comid)
    
    #Return the NWIS sites (gages) upstream or downstream of the point, within the specified distance
    matched_gages <- navigate_nldi(nldi_feature_temp,
                                   mode = searchmode,
                                   data_source = "nwissite", 
                                   distance_km = searchdistance)

    #Get flowlines upstream or downstream of point
    flowline_nldi <- navigate_nldi(nldi_feature_temp,
                                   mode = searchmode,
                                   distance_km = searchdistance)
    
    #Subset NHD data to get attribute information about the flowlines/river network
    nhdsubset <- subset_nhdplus(comids = flowline_nldi$nhdplus_comid,
                             output_file = NULL,
                             nhdplus_data = "download",
                             flowline_only = TRUE,
                             return_data = TRUE)
    
    #Get attributes of the NHD network for the dam
    dam_attr <- nhdsubset$NHDFlowline_Network[nhdsubset$NHDFlowline_Network$comid==start_comid,c("levelpathi","arbolatesu","totdasqkm")]
    
    #Combine all of the info into a single data frame
    alldamgagedata <- bind_cols(data.frame(c(damtemp[,c("ID","DAM_NAME","LAT","LON","HUC_8")],dam_comid=start_comid,levelpathid=dam_attr$levelpathi,arbolatesum=dam_attr$arbolatesu,totdasqkm=dam_attr$totdasqkm)),matched_gages[,c("identifier","comid")])
    
    if(plotting==T){
      plot(sf::st_geometry(flowline_nldi), col = "blue",main=damtemp$DAM_NAME)
      plot(start_point, cex = 2.5, lwd = 3, col = "red",pch=16, add = TRUE)
      plot(matched_gages,cex = 1.5, lwd = 2, col = "black",pch=17, add = TRUE)
    }
    
    return(alldamgagedata)
  }else{
    print(paste(damtemp$ID,'did not match to a valid comid. Check lat/lon.'))
  }
  
}

#Apply the tracedam2gage function to dams to find the downstream gages
allmatcheddamgagedata_dnstrm <- pblapply(dams_test@data$ID,FUN=tracedam2gage,inputdams=dams_test@data,searchmode='downstreamMain',searchdistance=50)
allmatcheddamgagedata_dnstrm <- bind_rows(allmatcheddamgagedata_dnstrm)

#If a gage is listed more than once, this means the upper dam(s) need to be removed (another dam downstream is also tied to this gage, and more directly controls its flow)
#If gage is listed for >1 dams with the same LevelPathI (they share the same downstream-most flowline), only keep the one with the highest arbolate sum
allmatcheddamgagedata_dnstrm_final <- allmatcheddamgagedata_dnstrm %>% 
    group_by(identifier) %>%
    top_n(1,arbolatesum)
allmatcheddamgagedata_dnstrm_final$Dataset <- 'Downstream NWIS Site'

#Apply the tracedam2gage function to dams to find the upstream gages
allmatcheddamgagedata_upstrm <- pblapply(dams_test@data$ID,FUN=tracedam2gage,inputdams=dams_test@data,searchmode='upstreamTributaries',searchdistance=50)
allmatcheddamgagedata_upstrm <- bind_rows(allmatcheddamgagedata_upstrm)

#If gage is listed for >1 dams with the same LevelPathI, only keep the one with the smallest arbolate sum (the most upstream dam)
allmatcheddamgagedata_upstrm_final <- allmatcheddamgagedata_upstrm %>% 
  group_by(identifier) %>%
  top_n(-1,arbolatesum)
allmatcheddamgagedata_upstrm_final$Dataset <- 'Upstream NWIS Site'

allmatchedsites <- bind_rows((allmatcheddamgagedata_dnstrm_final),(allmatcheddamgagedata_upstrm_final)) #This is a list of all upstream and downstream matched gages listed in NWIS. There may not be recent/current data at all of these sites.

#Gather information about water quality (temperature) and flow from the matched gages
fetchwqdata <- function(inputsites,startDate,endDate,statCodes,paramCodes){
  tempsites <-  unique(substr(inputsites,start = 6,stop = nchar(inputsites)))
  WQ.ts   <- readNWISdv(tempsites, paramCodes, startDate, endDate, statCodes)
  WQ.ts <- renameNWISColumns(WQ.ts)
  WQ.ts.sub <- subset(WQ.ts, select=-grep('cd',names(WQ.ts)))
  WQ.locs <- data.frame(site_no=attributes(WQ.ts)$siteInfo$site_no,lat=attributes(WQ.ts)$siteInfo$dec_lat_va,lon=attributes(WQ.ts)$siteInfo$dec_lon_va,siteType=attributes(WQ.ts)$siteInfo$siteTypeCd)
  
  WQ.avg <- subset(WQ.ts.sub, select=-c(Date)) %>% group_by(site_no) %>% summarise(across(everything(), mean, na.rm=T))
  WQ.avg <- merge(WQ.avg,WQ.locs)
  
  return(WQ.avg)
}

#Apply the fetchwqdata function to get average flow (code=00060) and temperature (code=00003) data. If there is no data at a site (given the parameters and time period), the sites are dropped.
wqgagedata <- fetchwqdata(inputsites=allmatchedsites$identifier,
                          startDate='2001-01-01',endDate='2020-12-31',statCodes=c('00003'),paramCodes=c('00010','00060'))


wqgagedata_sf <- st_as_sf(wqgagedata, coords = c("lon", "lat"), crs = 4326)


#Quick look at the returned sites
p1 <- ggplot() +
  geom_sf(data=wqgagedata_sf,size=3) +
  geom_sf(data=st_as_sf(spTransform(dams_test,crs(wqgagedata_sf))),col='red')+
  geom_text(data=dams_test@data ,aes(x=LON, y=LAT, label=NIDID),
            color = "black", check_overlap = F)+
  geom_text(data=wqgagedata ,aes(x=lon, y=lat, label=site_no),
            color = "grey", check_overlap = F)

ggplotly(p1)

#Output results
write.csv(allmatchedsites,'Data/MatchedHydropowerDams_USGSGages.csv',row.names = F)
write.csv(wqgagedata,'Data/USGSGages_Q_Temp_00-20avg.csv',row.names = F)
