# geospatial_misc 
Harmonize data from raster files of different grid-cell resolution for agricultural output, metereological data, soil quality, fertilizer use. 

Agricultural data from: [MapSPAM](https://mapspam.info/) and [FAO GAEZ](https://data.apps.fao.org/catalog/iso/2ebbb81b-8e1e-46a7-adde-088a8dbe8b4b)

## loop.R 
Extract from raster files: 

* Crop yield (41 crop types), production, physical area, and harvested area for different administrative levels (e.g., country, state- and county-equivalent)
* Elevation, slope, aspect, soil quality, precipitation, temperature, fertilizer use, air quality, etc. 

## loop_row.R 
Same as above. 

## gaez_data.R 
* Crop yield, production, physical area, and harvested area for different administrative levels (e.g., country, state- and county-equivalent) from FAO GAEZ v4

