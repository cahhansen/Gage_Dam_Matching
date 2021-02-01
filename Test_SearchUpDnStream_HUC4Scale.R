library(rgdal)
library(spatialEco)
library(nhdplusTools)
library(sf)
library(raster)
library(dataRetrieval)
library(pbapply)
library(dplyr)
library(ggplot2)
library(plotly)


#Simple approach to matching dams and gages and then retrieving data.
#Demonstrate in one HUC4 (0304) (This sub-region contains the Yadkin River Reservoir System, so it represents a relatively complex case, with multiple reservoirs and multiple gages)
#Test gage matching to select dams 

#Load and format spatial data
dams <- readOGR(dsn='Data/ReservoirGageMatching.gpkg','GRanD_Dams_v1_1_US',stringsAsFactors = F)
dams$ID <- paste0(dams$EHAID,'-',dams$NIDID) #Compound identifier based on existing hydropower assets id and the id used in the National Inventory of Dams
dams[dams$OTHERSTRUCTUREID=="NA" & !is.na(dams$OTHERSTRUCTUREID),"OTHERSTRUCTUREID"] <- NA #Keep only main dams, not supporting structures
HUCs <- readOGR(dsn='Data/ReservoirGageMatching.gpkg','WBD_SubBasin_TestHUC0304',stringsAsFactors = F)

#Add HUC information to the dams with a quick spatial join
dams <- spTransform(dams,crs(HUCs))
dams <- point.in.poly(dams,HUCs)

#Subset to HUC used for testing
dams_test <- dams[dams$HUC_4=='0304' & !is.na(dams$HUC_4) & is.na(dams$OTHERSTRUCTUREID),]
plot(dams_test)


#Function to trace to gages upstream (upstreamTributaries) or downstream (downstreamMain)
tracedam2gage <- function(inputdams,myid,idcol,namecol,xcoord,ycoord,searchmode,searchdistance,plotting=F){
  damtemp <- inputdams[inputdams[,idcol]==myid,] #Select an individual reservoir
  #Pull Coordinates from the reservoir and match to comid
  start_point <- st_sfc(st_point(c(as.numeric(damtemp[,xcoord]), as.numeric(damtemp[,ycoord]))), crs = 4269)
  start_comid <- discover_nhdplus_id(start_point) #Determines the comid most closely matched to the dam point. This assumes the coordinates of the dam are accurate.

  if(start_comid >1 & start_comid<2147483647 & !is.null(start_comid)){ #Check it's a valid comid
    nldi_feature_temp <- list(featureSource = "comid", featureID = start_comid)
    
    #Return the NWIS sites (gages) upstream or downstream of the point, within the specified distance
    matched_gages <- navigate_nldi(nldi_feature_temp,
                                   mode = searchmode,
                                   data_source = "nwissite", 
                                   distance_km = searchdistance)
    
    if(nrow(matched_gages)>0){
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
    alldamgagedata <- bind_cols(data.frame(c(damtemp[,c(idcol,namecol,ycoord,xcoord,"HUC_8")],dam_comid=start_comid,levelpathid=dam_attr$levelpathi,arbolatesum=dam_attr$arbolatesu,totdasqkm=dam_attr$totdasqkm)),matched_gages[,c("identifier","comid")])
    alldamgagedata <- alldamgagedata[,-c(12)]
    colnames(alldamgagedata) <- c("ID","NAME","LAT","LON","HUC_8","DAM_COMID","LEVELPATHID","ARBOLATE_SUM","TOTDA_SQKM","GAGE_ID","GAGE_COMID")
    if(plotting==T){
      plot(sf::st_geometry(flowline_nldi), col = "blue",main=damtemp$DAM_NAME)
      plot(start_point, cex = 2.5, lwd = 3, col = "red",pch=16, add = TRUE)
      plot(matched_gages,cex = 1.5, lwd = 2, col = "black",pch=17, add = TRUE)
    }
    
    return(alldamgagedata)
    }else{
      return(alldamgagedata=NULL)
      print((paste(myid,'did not match to any gages.')))
    }
  }else{
    return(alldamgagedata=NULL)
    print(paste(myid,'did not match to a valid comid. Check lat/lon.'))
  }
  
}

#Apply the tracedam2gage function to dams to find the downstream gages
allmatcheddamgagedata_dnstrm <- pblapply(dams_test@data$grand_id,FUN=tracedam2gage,inputdams=dams_test@data,idcol='grand_id',namecol='dam_name',xcoord='long_dd',ycoord='lat_dd',searchmode='downstreamMain',searchdistance=50)
allmatcheddamgagedata_dnstrm <- bind_rows(allmatcheddamgagedata_dnstrm)

#If a gage is listed more than once, this means the upper dam(s) need to be removed (another dam downstream is also tied to this gage, and more directly controls its flow)
#If gage is listed for >1 dams with the same LevelPathID (they share the same downstream-most flowline), only keep the one with the highest arbolate sum
allmatcheddamgagedata_dnstrm_final <- data.frame(allmatcheddamgagedata_dnstrm %>% 
    group_by(GAGE_ID) %>%
    top_n(1,ARBOLATE_SUM))
allmatcheddamgagedata_dnstrm_final$DATASET <- 'Downstream NWIS Site'

#Apply the tracedam2gage function to dams to find the upstream gages
allmatcheddamgagedata_upstrm <- pblapply(dams_test@data$grand_id,FUN=tracedam2gage,inputdams=dams_test@data,idcol='grand_id',namecol='dam_name',xcoord='long_dd',ycoord='lat_dd',searchmode='upstreamTributaries',searchdistance=100)
allmatcheddamgagedata_upstrm <- bind_rows(allmatcheddamgagedata_upstrm)

#If gage is listed for >1 dams with the same LevelPathID, only keep the one where it matched to the dam with the smallest arbolate sum (the most upstream dam)
allmatcheddamgagedata_upstrm_final <- data.frame(allmatcheddamgagedata_upstrm %>% 
  group_by(GAGE_ID) %>%
  top_n(-1,ARBOLATE_SUM))
allmatcheddamgagedata_upstrm_final$DATASET <- 'Upstream NWIS Site'

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
wqgagedata <- fetchwqdata(inputsites=allmatchedsites$GAGE_ID,
                          startDate='2001-01-01',endDate='2020-12-31',statCodes=c('00003'),paramCodes=c('00010','00060'))

wqgagedata$site_no <- paste0('USGS-',wqgagedata$site_no)
wqgagedata_sf <- st_as_sf(wqgagedata, coords = c("lon", "lat"), crs = 4326) #I believe the NWIS data coords are in CRS=4326

#Filter out matches that had no water quality or flow data. Note that a gage could be matched as downstream of one dam and upstream of another.
allmatchedsites_wq <- merge(allmatchedsites,wqgagedata,by.x='GAGE_ID',by.y='site_no',all.x=F)


#Quick look at the returned sites. Note, to save on time/memory, the flow line network is not kept during the tracedam2gage function. This only shows the dams and the gages. 
p1 <- ggplot() +
  geom_sf(data=wqgagedata_sf,size=2) +
  geom_sf(data=st_as_sf(spTransform(dams_test,crs(wqgagedata_sf))),col='red',size=3)+
  geom_text(data=dams_test@data ,aes(x=long_dd, y=lat_dd, label=grand_id),
            color = "black", check_overlap = F)+
  geom_text(data=wqgagedata ,aes(x=lon, y=lat, label=site_no),
            color = "grey", check_overlap = F)

ggplotly(p1)

#Output results
write.csv(allmatchedsites_wq,'Data/MatchedDams_Gages_Q_Temp_2001_2020.csv',row.names = F)
