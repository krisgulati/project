library(openalexR)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(tidyverse)
options(openalexR.mailto = "kgulati@ucmerced.edu")

setwd("C:/Users/kgulati/Desktop/NewProject")


### Goal: The previous free_speech script had a lot of incidents but many were social sciences/humanities, which is too close to their research.
#### So for now, let's just focus on STEM, research Professors, possible R1/R2 unis, and 2017 or before -- this will limit the sample to something really small...!!
##### Despite the small sample size, this goal will provide a small script that can be generalised later.
###### Lets first extract the citations and publications from certain scholars, , then clean the data, then construct the control group papers later, and then finally do the regressions

########################################## Step One: Extract pubs/citations from a small subset of scholars that fits the criteria above! ##############################

### So, my list includes:

# Austin Holland
# Aline Gubrium
# Bart Knijnenburg
# Ron Heiniger
# Michael L. Chikindas
# Raynor Mullins (removed because too few pubs)
# Allan Josephson (couldn't find)


source("utils/import-authors.R")
list_dfs <- import_Authors()
#names(list_dfs)


########################################## Step Two: Clean the data ##############################

### What we're working with
#austin_holland_merged 
#aline_gubrium_oa
#bart_k_oa
#ron_h_oa 
#michael_l_oa 

clean_dfs <- lapply(list_dfs, function(df){
  print(class(df))
  df_clean <- df %>% drop_na(doi)
  df_clean <- df_clean[(df_clean$type == "journal-article") ,]
  df_clean <- df_clean[complete.cases(df_clean[,c("volume","issue")]),]
  df_clean <- df_clean %>% drop_na(type)
  df_clean <- df_clean %>% drop_na(issn)
  df_clean[, "issn_clean"] <- sapply(df_clean$issn, "[[", 1)
  
  return(df_clean)
})

names(clean_dfs) = names(list_dfs)



View(clean_dfs[[1]])
View(clean_dfs[[2]])
View(clean_dfs[[3]])
View(clean_dfs[[4]])
View(clean_dfs[[5]])



### Need to come back to automating what was below
       

#=== Below is manual, trying to automate this
# 
# austin_holland_cleaned_1 <- austin_holland_merged %>% drop_na(doi)
# austin_holland_cleaned_2 <- austin_holland_cleaned_1[(austin_holland_cleaned_1$type == "journal-article") ,]
# austin_holland_cleaned_3 <- austin_holland_cleaned_2[complete.cases(austin_holland_cleaned_2[,c("volume","issue")]),]
# cleaned_austin_4 <- austin_holland_cleaned_3 %>% drop_na(type)
# 
# 
# aline_gubrium_cleaned_1 <- aline_gubrium_oa %>% drop_na(doi)
# aline_gubrium_cleaned_2  <- aline_gubrium_cleaned_1[(aline_gubrium_cleaned_1$type == "journal-article") ,]
# aline_gubrium_cleaned_3 <- aline_gubrium_cleaned_2[complete.cases(aline_gubrium_cleaned_2[,c("volume","issue")]),]
# aline_gubrium_cleaned_4 <- aline_gubrium_cleaned_3 %>% drop_na(type)
# 
# 
# bart_k_cleaned_1 <- bart_k_oa %>% drop_na(doi)
# bart_k_cleaned_2  <- bart_k_cleaned_1[(bart_k_cleaned_1$type == "journal-article") ,]
# bart_k_cleaned_3 <- bart_k_cleaned_2[complete.cases(bart_k_cleaned_2[,c("volume","issue")]),]
# bart_k_cleaned_4 <- bart_k_cleaned_3 %>% drop_na(type)
# 
# ### This one has letters for the issues, so need to deal with this.... easy solution is to drop!###
# 
# ron_h_cleaned_1 <- ron_h_oa %>% drop_na(doi)
# ron_h_cleaned_2  <- ron_h_cleaned_1[(ron_h_cleaned_1$type == "journal-article") ,]
# ron_h_cleaned_3 <- ron_h_cleaned_2[complete.cases(ron_h_cleaned_2[,c("volume","issue")]),]
# ron_h_cleaned_4 <- ron_h_cleaned_3 %>% drop_na(type)
# 
# michael_cleaned_1 <- michael_l_oa %>% drop_na(doi)
# michael_cleaned_2  <- michael_cleaned_1[(michael_cleaned_1$type == "journal-article") ,]
# michael_cleaned_3 <- michael_cleaned_2[complete.cases(michael_cleaned_2[,c("volume","issue")]),]
# michael_cleaned_4 <- michael_cleaned_3 %>% drop_na(type)
# 

#### This was when I used the ISSN (Venued ID didn't seem to work with Works)

View(clean_dfs[[1]])
### Test, with this SSN 0036-8075, i.e. the first one from cleaned austin holland

source("utils/create_control.R")

control_group <- create_control("0036-8075", "2015-02-20", "2015-02-20")


# control_groups <- list()
control_groups <- data.frame()

for (j in 1:length(clean_dfs)){
  
  for (i in 1:nrow(clean_dfs[[j]])){
    
    issn <- clean_dfs[[j]]$issn_clean[i]
    to_date <- clean_dfs[[j]]$publication_date[i]
    from_date <- clean_dfs[[j]]$publication_date[i]
    
    control_group <- create_control(issn, to_date, from_date)
    control_group[, "author_link"] <- names(clean_dfs)[j]
    control_group[, "issn_link"] <- issn
    
    control_groups <- rbind(control_groups, control_group)
    
  }
  
}

### Probably add sleep function. Sys.sleep(0.5) -- line 28

View(clean_dfs[[1]])

View(control_groups[[1]])
class(control_groups)

#class(journal_2$referenced_works)

### I didn't drop any articles that were cited 0 times, but this can be done if needed in the future.
##### TO DO: Need to drop the treatment group article from this bunch of articles.


#View(journal$referenced_works[[1]])
#print(journal[1,24])
#as.character(journal[1,24])


### Trying something else

journal_test_2 <- oa_fetch(
  entity = "works",
  host_venue.issn = "1944-8007",
  from_publication_date = "2015-04-28",
  to_publication_date = "2015-04-28",
  verbose = TRUE
)

journal_test_2_1 <- journal_test_2 [(journal_test_2$type == "journal-article"), ] # If not a journal article
journal_test_2_2 <- journal_test_2_1 %>% drop_na(referenced_works) # Drop any NAs
journal_test_2_3 <- journal_test_2_2 [(!journal_test_2_2$referenced_works == "NA"), ] # Sometimes ther NAs are characters, and so remove these

### Seems to be working for this example, but again need to drop the treatment group article from this bunch of articles.




#Other Code

### How to get the first element for every list ### sapply(austin_holland_merged$issn, "[[", 1)



