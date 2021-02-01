# NHD_Matching
Use existing tools (nhdplustools, dataRetrieval) and a simple approach to match information from streamflow gages and other points (i.e. dams) on the NHD river network. 


## Description of data in the geopackage:
### Dams:
- GRanD Dams v1.1 (Limited to the US). Includes powered and non-powered dams.
- NHAAP_NPD_FY11_1MW (Non-powered dams identified as having >1MW capacity potential, based on a 2012 Resource Assessment and info from the 2010 NID).
- NID 2019 (publicly available dam data from the USACE, last updated and published in early 2020. Contains powered and non-powered dams).
### Hydrography:
- WBD SubBasins for HUC 0304 (subset of the watershed boundary dataset with HUC8s contained within the HUC4 sub-region).
### Gages:
- USGS Gages with streamflow (parameter code=00060) and/or temperature (parameter code=00010) data from 2001-2020.


## To get a feel for the NHD network:
- NHDPlusV2 flowlines can be downloaded from https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data but these are really massive datasets. (There are >2.7 million segments in the US). This is why the nhdplustools R package is so great to work with - the segment data don't have to be downloaded locally. The unique identifier of each segment is the COMID, but there are other ID's that are useful to help navigate the network (Hydroseq indicates the up/downstream segment). This has some of the key attributes, but not all. See the user guide for more info on all attributes that can be associated with each river segment: https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/Documentation/NHDPlusV2_User_Guide.pdf)
