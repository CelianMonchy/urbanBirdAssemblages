# Urban bird assemblages are shaped by the regional pool
R scripts supporting the study "Urban bird assemblages are shaped by the regional pool" by C. Monchy &amp; A. Rodrigues.

### Data processing
#### 1_Data_Extraction.R
* **Input :** data downloaded from eBird database (checklists + observations) between 2015 and 2021
* Filtering data (sampling + ebd) according criteria : complete checklists, geographical area in America, protocol (stationnary and transect), duration (15min and 5h), transect distance (less than 5km).
* **Output :** first round of filtered data + list of "valid" observers (with at least 10 complete checklists across the continent)
    
#### 2_Data_Split.R
* **Input :** first round of filtered data from *1_Data_Extraction.R* + ecoregion shapefile[^1] + list of studied species (acceptedTaxa.txt)
* Split data by ecoregion
* Removing checklists (sampling) created during night-time and with more than 2 observers
* Filtering observations (ebd) according to the species (see acceptedTaxa.txt)
* **Output :** second round of filtered data (sampling + ebd) split by ecoregions (283).
    
#### 3_Calculation_Urban-Rich.R
* **Input :** second round of filtered data from *2_Data_Split.R* + built-up raster layer[^2]
* Filtering checklists according the number of observers (only one)
* Computing average urbanisation rate with two buffers (1 an 3km) around the checklist
* Filtering checklists with an urban rate "NotAttributed" (sea, lakes)
* Computing an observer's score based on the richness of its lists
* **Output :** third round of filtered data (sampling + ebd) split by ecoregions.
    
#### 3bis_Filtering_season.R 
* **Input :** third round of filtered data from *3_Calculation_Urban-Rich.R*
* Seasonal filtering according to the geographic centroid of the ecoregion
* **Output :** fourth round of filtered data (sampling + ebd) split by ecoregions (236).

#### 4_Filtering_ScoreDurationDistance.R
* **Input :** fourth round second of filtered data
* Final filtering based on checklist duration (15min-1h), ditance (less than 2km) and score of observers (median 80\%)
* Defining "urban", "nonurban" (rural) checklists according to the average urban rate
* **Output :** fifth round of filtered data (sampling + ebd) split by ecoregions (226).
    
#### 5_Selection_UrbanNurban.R
* **Input :** fifth round of filtered data from *4_Filtering_ScoreDurationDistance.R* + vector layer of the coastline[^3] (50m resolution) + digital elevation model[^4] (1km resolution)
* Defininf coastline distance and elevation of checklist
* Computing euclidean distance between urban and non-urban checklists
* **Output :** appaired checklists (urban and rural) for 138 ecoregions (including 69 with less than 20 pairs)

#### 6_Finalizing_AbundanceMatrix.R
* **Input :** fifth round of filtered observations from *4_Filtering_ScoreDurationDistance.R* and appaired checklists from *5_Selection_UrbanNurban.R*
* Get the species of selected checklists and counting their occurence to design the final dataset
* **Output :** two files per ecoregion containing the observations of the 20 urban and 20 non-urban selected checklists.


### Data analysis and visualization
#### 7_PRE-ANALYSIS_DistanceComputation.R
* **Input :** urban and non-urban observations from [*6_Finalizing_AbundanceMatrix.R*](/'Data Processsing'/6_Finalizing_AbundanceMatrix.R).
* Performing basic statistics about the urban and non-urban assemblages of each ecoregion (number of strictly urban and strictly non-urban species, number of species common between assemblages).
* Computing Jaccard and Bray-Curtis distance between urban and non-urban assemblages of each ecoregion.
* **Output :** statistics concerning assemblages of each ecoregion (in rows) + final dataset presenting the frequency of each species (in rows) in the 20 selected pairs of checklists (urban and non-urban) defining the assemblage, for each ecoregion (two columns per ecoregion). This dataset is available here...

The other scripts perform analysis and produce plot related to the studied patterns. They used data produced with *7_PRE-ANALYSIS_DistanceComputation.R* and the ecoregion shapefile[^1].


[^1]: Olson, D.M., Dinerstein, E., Wikramanayake, E.D., Burgess, N.D., Powell, G.V.N., Underwood, E.C., et al. (2001). *Terrestrial ecoregions of the world: A new map of life on Earth.* **Bioscience**, 51, 933–938.

[^2]: Buchhorn, M., Smets, B., Bertels, L., Roo, B.D., Lesiv, M., Tsendbazar, N.-E., et al. (2020). Copernicus Global Land Service: Land Cover 100m: collection 3: epoch 2015: Globe.

[^3]: Natural Earth. (2009). *Coastline - Free vector and raster map data at 1:50m scale. Version 4.0.0.* Available at: https://www.naturalearthdata.com/downloads/50m-physical-vectors/50m-coastline/. Last accessed 1 March 2022.

[^4]: Earth Resources Observation and Science (EROS) Center. (2017). Global 30 Arc-Second Elevation (GTOPO30).
