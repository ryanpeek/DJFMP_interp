# get cdec
#### FUNCTION TO PULL DATA FROM CDEC STATIONS
#### RYAN PEEK, 2016, 
#### CENTER FOR WATERSHED SCIENCES UC DAVIS

get.CDEC<-function( # Function to pull CDEC data
  station,  # Station is 3 letter abbreviation
  sensor,   # sensor is number, see below
  duration, # Duration is E=event, D=Daily, H=Hourly
  start,    # "YYYY-MM-DD"
  end,      # "YYYY-MM-DD"
  csv=F){     # export to CSV?
  
  # List of Real-Time Stations: http://cdec.water.ca.gov/misc/realStations.html
  # List of Daily Stations: http://cdec.water.ca.gov/misc/dailyStations.html 
  # List of sensors:  http://cdec.water.ca.gov/misc/senslist.html
  
  ## STATIONS USED FREQUENTLY
  # SGP: Sugar Pine (AMR)
  # HYS: Huysink (AMR)
  # TGC: Tuolumne Grand Canyon (TUO)
  # DNP: Don Pedro (TUO)
  
  ## SENSORS MOST COMMONLY USED
  # 1  stage (ft)
  # 20 flow cfs
  
  # 2  rain accum (in)
  # 16 precip tippingbucket (in)
  # 45 ppt incremental
  
  # 3  snow water content (in)
  # 18 snow depth (in)
  
  # 6  reservoir elevation (ft)
  # 15 reservoir storage (ac-ft)
  # 76 reservoir inflow
  
  # 25 water temp 
  # 4  air temp 
  
  ## EXAMPLE URL:  
  # http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=OXB&dur_code=E&sensor_num=20&start_date=2011/10/01&end_date=2012/09/30
  if(!require(readr)){
    install.packages("readr")
    library(readr)}
  
  if(!require(stringr)){
    install.packages("stringr")
    library(stringr)
  }
  
  data <- read_csv(paste("http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=", station, 
                         "&SensorNums=", sensor,
                         "&dur_code=", duration, 
                         "&Start=", start, 
                         "&End=", end, sep=""))
  
  names(data) <- stringr::str_replace_all(names(data), c(" " = "_"))
  #data <- make.names(names(data),unique = TRUE)

  if(csv){write_csv(data, path = paste("cdec_", station,"_sensor-",sensor,"_",start,"_to_",end,".csv",sep=""))
    print(paste("file downloaded and saved here: ", getwd(), sep="")) # show message
  
  } else{
    cat("Output to dataframe only\n")
  }
  
  cdec<-paste0(station)
  assign(cdec, data,envir = .GlobalEnv) # print to workspace
  cat("All Finished! Available in current dataframe...\n")
}

