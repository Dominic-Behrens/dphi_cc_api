#Author: Dominic Behrens
#Project: 
#Purpose: Get DPHI CC data from the API 
#Notes:

pacman::p_load(
  tidyverse,
  sf,
  tmap,
  httr,
  jsonlite,
  scales,
  janitor,
  shiny,
  magrittr,
  systemfonts,
  purrr
)

#Basic Setup and Useful things
rm(list=ls())
options(scipen=1000)
timeout(480)

#Function to clean up API response
#Note: Excludes cancelled applications
clean_cc_output<-function(df){
  df%>%
    select(where(~!is.data.frame(.x)))%>%
    filter(ApplicationStatus!="Cancelled")%>%
    remove_empty("cols")
}
#Loop to extract all CCs in NSW database, 1000 at a time.
#initialise index
i<-1
more_pages<-T
results_list<-list()
while(more_pages==T){
cat(paste0("Pulling data from the DPHI API, page number: ",i,"\n"))
#Set up query headers
headers <- c(
  'PageSize' = '1000',
  'PageNumber' = as.character(i),
  'filters' = '{ "filters": {"CostOfDevelopmentFrom":100000} }'
)
#Run response
res <- VERB("GET",
            url = "https://api.apps1.nsw.gov.au/eplanning/data/v0/OnlineCC",
            add_headers(headers))%>%
  content(as='text', encoding='UTF-8')%>%
  fromJSON(flatten=TRUE)
#get details — if NULL, no more data
details<-res$Application
if(is.null(details)||length(details)==0){
  cat("No more results returned, ending loop.\n")
  more_pages<-F}else{
#clean up using clean_cc_data function
results<-clean_cc_output(details)
cat(paste0("Pulled ",nrow(results)," non-cancelled construction certificates.\n"))
#collect results in list (bind once at end)
results_list[[i]]<-results
#Build rolling output so partial results are available if interrupted
out_frame<-bind_rows(results_list)
cat(paste0("Total rows so far: ",nrow(out_frame),"\n"))
#update index
i<-i+1
  }
}

#Clean up output dataframe
clean_cc_data<-clean_names(out_frame)%>%
  select(1:14,#initial key information
         intersect(starts_with("location"),c(ends_with("x"),ends_with('y'),ends_with('full_address'))),#Select only the first location listed
         contains('building_code_class_building_code_class'),#drop BC descriptions, don't need these
         c('development_type','lodgement_date','date_submitted','determination_date'))%>%
  unite("building_code_types",contains('building_code'),sep=",",na.rm=T)#merge building codes into one list

#save data
write.csv(clean_cc_data,'./Outputs/construction_certs_over_100k.csv')
