#################################### Cleaner Script ######################################
##########################################################################################
##########################################################################################


library(openalexR)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(tidyverse)
library(readr)
options(openalexR.mailto = "kgulati@ucmerced.edu")
setwd("C:/Users/kgulati/Desktop/NewProject")


#################################### Step One: Find Authors' ORCID/OA ID #################
##########################################################################################
##########################################################################################


# Bring in the authors -- this process is a manual process in the util file where I find the authors ORCID and/or OA ID
source("utils/import-authors.R")
list_dfs <- import_Authors() # Assign the util file to a list. We have a list of dataframes

incident_year <- c("austin_holland"=2013, "aline_gubrium" = 2014, "bart" = 2015, "ron" = 2011, "michael" = 2010)

# If you want to view one of the dfs
#View(list_dfs[[1]]) 


#################################### Step Two: Clean the Authors' data #################
##########################################################################################
##########################################################################################



# But, the list of dataframes is fairly noisy, i.e. duplicates etc. So, we want to clean all of them!

clean_dfs <- lapply(list_dfs, function(df){
  print(class(df))
  df_clean <- df %>% drop_na(doi) # Drop if there's no doi
  df_clean <- df_clean[(df_clean$type == "journal-article") ,] # Drop if the paper isn't categorised as a journal article
  
  
  # df_clean <- df_clean[complete.cases(df_clean[,c("volume","issue")]),]
  # I've commented this out because I don't think we need it for now because later I'll focus on page numbers and ISSN numbers.
  
  
  df_clean <- df_clean %>% drop_na(type) # Drop, if the type is considered as na
  df_clean <- df_clean %>% drop_na(issn) # Drop if there's no ISSN number, which we need later
  df_clean[, "issn_clean"] <- sapply(df_clean$issn, "[[", 1) # We just take the first ISSN number, and create a new column for this
  
  return(df_clean)
})

names(clean_dfs) = names(list_dfs)

#View(clean_dfs[[1]])
#View(clean_dfs[[2]])
#View(clean_dfs[[3]])
#View(clean_dfs[[4]])
#View(clean_dfs[[5]])


#################################### Step Three: Create Control Group #################
##########################################################################################
##########################################################################################

# Create a function to sandwich the particular publication in question, to extract all other articles from that place
# Take an author, find all the other authors that published in the same day (capturing same volume)
source("utils/create_control.R")


### Apply that function to every publication in the treatment group
all_data <- data.frame()

### Read file if all_data if it exists, otherwise go on with the for loop below

#write_csv(all_data, "Outputs/all_data.csv", na = "")
#all_data_temp <- read_delim("Outputs/all_data.csv", delim=",")
#If (!???filename??? %in% list.files(???file path???) { ???then do loop }


for (j in 1:length(clean_dfs)){
  
  cat("getting author ", names(clean_dfs)[j], " " , j,  " of", length(clean_dfs), "\n")

      
  
  for (i in 1:nrow(clean_dfs[[j]])) {
    
    cat("\tgetting publications : ", i, " of", nrow(clean_dfs[[j]]), "\n")
    
    issn <- clean_dfs[[j]]$issn_clean[i]
    to_date <- clean_dfs[[j]]$publication_date[i]
    from_date <- clean_dfs[[j]]$publication_date[i]
    paper_id <- clean_dfs[[j]]$id[i]
    
    
    control_group <- create_control(issn, to_date, from_date)
    control_group[, "author_link"] <- names(clean_dfs)[j]
    control_group[, "issn_link"] <- issn
    control_group[, "treatment_link"] <- paper_id
    
    control_group[, "treatment_or_control"] <- control_group$id %in% paper_id
    
    
    control_group <- control_group %>% drop_na(doi) # Drop if there's no doi
    control_group <- control_group[(control_group$type == "journal-article") ,] # Drop if the paper isn't categorised as a journal article
    control_group <- control_group %>% drop_na(type) # Drop, if the type is considered as na
    control_group <- control_group %>% drop_na(issn) # Drop if there's no ISSN number, which we need later
    control_group[, "issn_clean"] <- sapply(control_group$issn, "[[", 1) # We just take the first ISSN number, and create a new column for this
    
    control_group <- control_group %>% 
      filter(!is.na(counts_by_year))
    
    ### Step One: Initialise year columns
    
    new_cols <- paste0("citations_n_", 1960:2022)
    control_group[, new_cols] <- 0
  
    
    ### Step Two: Populate the columns with citations year-by-year
    
    # Go to counts_by_year col in control_group df, we go to the first row, then for every row within the df
    # populate the year with the citation number 
    
    if (nrow(control_group) > 0) {
      for (k in 1:nrow(control_group)){
        year_new_cols <- paste0("citations_n_", control_group$counts_by_year[[k]]$year)
        citation_values <- control_group$counts_by_year[[k]]$cited_by_count
        control_group[k, year_new_cols] <- as.list(citation_values)
      }
    }
    

    
    ### Create caseid in control_group here
    
    control_group[, "caseid"] <- j 
    
    ### Add pre column here
    
    ### Create incident year column
    control_group[, "incident_year"] <- incident_year[names(clean_dfs)[j]]
  
    control_group[, "pre"] <- ifelse(control_group$publication_year < incident_year[names(clean_dfs)[j]], 1, 0)
    
    all_data <- rbind(all_data, control_group)
    
    Sys.sleep(1) ## If get errors from OA, change time.
    
  }
  
  
}



########Step Four: Expand citations column to include citations per year #################
##########################################################################################
##########################################################################################



write_csv(all_data,"C:/Users/kgulati/Desktop/NewProject\\small_sample.csv")

#Figure out why control_group has gone to 0 :(