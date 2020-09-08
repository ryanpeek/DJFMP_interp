# merge datasets

1. add in DWR dataset to the IEP dataset

  - Data is here: *DWR_invert_SHR_STTD* (DJFMP/R/R_guts/Files/lower_trophic_sampling).
  - Take data and group by date/order, add zeros for dates sampled with no cladocerans
  - filter to only cladocerans
  - merge with IEP dataset (add X/Y)
  - add to plots (Jan-June, 2014-2018)
  
2. Update with 2018 data from IEP

3. plot static maps of Jan - June, size standardized/binned by given breaks (set_breaks), color by log(AllCladoceran)



*From Anna (Aug 20, 2019)*

see: **all_clad_leaflet_map.html**

 - A facet of maps in 5 rows (2014-18) by 3 cols (winter = Jan-Feb, early_spring = mar-apr, late_spring = may-Jun)? 
 - Circle radius and colors show log(total cpue cladocera) and black spots show where sites were sampled but zero cladocera found.
 - In this fig it would be great if you could combine the CDFW iep data with the DWR *zoop* data for lower sac and Yolo (SHR and STTP right?). 

 
 Sorry I forgot to send u the code to set the spot size breakpoints and add these to the legend... but if you want to see the ugly code I was talking about it's linked in the About tab of the bay delta - > fish - > hatchery-releases page. 

 > Move forward w/ fig containing both datasets in 2014-17 and just have the yolo/lower sac missing from 2018 for now. 
 I am not sure I'm even going to include 2018 at all in the paper yet so we can just excl the final row if needs be. 
 **The 2014-17 DWR zoop data is at:** DJFMP/R/R_guts/Files/lower_trophic_sampling/2014_17_YBZoop_MATRIX_MASTER_071019.xlsx
