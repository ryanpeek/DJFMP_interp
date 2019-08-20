# merge datasets

1. add in DWR dataset to the IEP dataset

  - Data is here: *DWR_invert_SHR_STTD* (DJFMP/R/R_guts/Files/lower_trophic_sampling).
  - Take data and group by date/order, add zeros for dates sampled with no cladocerans
  - filter to only cladocerans
  - merge with IEP dataset (add X/Y)
  - add to plots (Jan-June, 2014-2018)
  
2. Update with 2018 data from IEP

3. plot static maps of Jan - June, size standardized/binned by given breaks (set_breaks), color by log(AllCladoceran)

