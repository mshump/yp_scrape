
# make class driver file
# iterate through it
# load back into R for further processing
# Do  unique at the end accross classes
# only keep domains that are non - ambiguous



#####################
# Load Libraries
#####################

#install.packages("rvest")
#install.packages("data.table")
#install.packages("plyr")
library(rvest)
library(data.table)
library(plyr)

###############
# Start Functions
###############

gather_sites <- function(site_type,city,state,page_n) {
  
  links_store <- NULL
  url_in <- paste0("https://www.yellowpages.com/search?search_terms=", site_type, "&geo_location_terms=", city,"%2C+", state, "&page=", page_n)
  yp_html <- read_html(url_in)
  
  #yp_html <- read_html("https://www.yellowpages.com/search?search_terms=Restaurants&geo_location_terms=Boston%2C+MA")
  
  #biz_names <- yp_html %>%
  #  html_nodes("[class=business-name]") %>%
  #  html_text()
  
  links <- yp_html %>%
    html_nodes("a") %>%
    html_attr("href")
  
  #attr_data <- yp_html %>%
  #  html_nodes("a") %>%
  #  html_attrs()
  
  #links[1:100]
  
  links <- grep("http://", links, perl=TRUE, value=TRUE)
  links <- grep("com$", links, perl=TRUE, value=TRUE)
  links <- grep("^(?!.*.yp.com).*$", links, perl=TRUE, value=TRUE)
  
  unique_links <- unique(links)
  
  links_store <- cbind( site_type = rep(site_type,length(unique_links)),
                        city = rep(city,length(unique_links)),
                        state = rep(state,length(unique_links)),
                        page_n = rep(page_n,length(unique_links)),
                        unique_links)


return(links_store)
}

###############
# End Functions
###############

#####################
#Input Parametrs start
#####################

# Load in driver file with locations (city and states)
setwd("C:/Users/mshump/Documents/Projects/Domain Classification/YP_SCRAPE/inputs/")
locations_tbl <- read.csv("locations_all.csv",header=TRUE, sep="," , stringsAsFactors=FALSE)

# randomly sort prior to search to try to protect from geography bias
# IMPROVEMENT OPPTY: can sample  by weights of population density
locations_tbl_sorted <- locations_tbl[sample(nrow(locations_tbl)),]

# Don't need to change this as loop with auto break
page_start <- 1
page_stop <- 10

setwd("C:/Users/mshump/Documents/Projects/Domain Classification/YP_SCRAPE/inputs/")
classifications_tbl <- read.csv("classifications.csv",header=TRUE, sep="," , stringsAsFactors=FALSE)


# If increase, consider adding a jittered pause between x iteartions in case YP has bot detection
sites_needed <- NULL #100
# Top level Class - not used in search
site_class <- NULL #"middle Schools" 
# key word used in search
# IMPROVEMENT OPPTY: Some classes need multiplel key words to search
class_keyword <- NULL #"middle Schools" 




#####################
#Input Parametrs stop
#####################

for( keyword_it in 1:nrow(classifications_tbl) ){
  #keyword_it <- 1
  message(paste0("keyword: ",keyword_it))
  
  site_class_tmp <- classifications_tbl[keyword_it,2]
  site_class <- gsub(" ", "+", site_class_tmp, fixed=TRUE)
  
  class_keyword_tmp<- classifications_tbl[keyword_it,3]
  class_keyword <- gsub(" ", "+", class_keyword_tmp, fixed=TRUE)
  
  sites_needed <- classifications_tbl[keyword_it,4]
  

site_class_all <- NULL
site_class_holder <- NULL
break_page <- 0

# begin loops
# Outter loop: at page 1 of results per location.
# Inner loop: locations
# break on samples needed reached


for( page_i in page_start:page_stop ){
  
  message(paste0("page number: ",page_i))
  
  if (break_page == 1) {
    break_page <- 0
    message("break the page loop")
    
    break #stop("Let's break out!")
    
  } 
  
  for( i in 1:nrow(locations_tbl_sorted) ){
    
    message(paste0("location number: ",i))
    
    city_tmp <- locations_tbl_sorted[i,1]
    city_in <- gsub(" ", "+", city_tmp, fixed=TRUE)
    
    state_tmp <- locations_tbl_sorted[i,2]
    state_in <- gsub(" ", "+", state_tmp, fixed=TRUE)
    
    message(paste0("city: ", city_tmp))
    message(paste0("state: ",state_tmp))
    
    possibleError <- tryCatch(
      gather_sites(class_keyword,city_in,state_in,page_i),
      error=function(e) e
    )
    
    if(!inherits(possibleError, "error")){
      site_class_holder <- gather_sites(class_keyword,city_in,state_in,page_i) 
      
      site_class_all <- rbind(site_class_all,site_class_holder)
      
      message(paste0("collected sites: ", nrow(site_class_all)) )
      
      if (nrow(site_class_all) > sites_needed) {
        break_page <- 1
        message(paste0("sites needed reached: ", sites_needed))
        
        break #stop("Let's break out!")
        
      } 
    }
  }


  
}


# Create final output table
site_class_all <- cbind(site_class= rep(site_class, nrow(site_class_all)), site_class_all)

# write file with search term in file name
setwd("C:/Users/mshump/Documents/Projects/Domain Classification/YP_SCRAPE/outputs/")
fwrite( as.data.table(site_class_all), paste0("site_class_all_", class_keyword,".csv" ) , append = FALSE)

message(paste0("file out: ",class_keyword ))
message(paste0("sites out: ", nrow(site_class_all) ))

Sys.sleep(runif(1,1,10))

}




### load all files back in for processing
setwd("C:/Users/mshump/Documents/Projects/Domain Classification/YP_SCRAPE/outputs/")
file_names <- list.files(pattern='*.csv') #where you have your files
site_class_all_in <- do.call(rbind,lapply(file_names,read.csv))

## make unique within classifications
site_class_all_unique <- unique(site_class_all_in[,c("site_class", "unique_links")])
site_ct_tmp <- count(site_class_all_unique, "unique_links")

#clean ambiguous domains (membership to mulitple classifications)
site_class_clean <- site_class_all_unique[site_class_all_unique$unique_links %in% site_ct_tmp[site_ct_tmp$freq == 1, c("unique_links")] ,]



setwd("C:/Users/mshump/Documents/Projects/Domain Classification/YP_SCRAPE/outputs/")
fwrite( as.data.table(site_class_clean), paste0("site_class_clean.csv" ) , append = FALSE)

