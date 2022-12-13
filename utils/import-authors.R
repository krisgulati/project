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



import_Authors <- function (){
  ### 1. Austin Holland
  
  
  austin_holland_auth <- oa_fetch(
    entity = "authors",
    display_name = c("Austin Holland"),
    verbose = TRUE
  )
  
  austin_holland_doi <- oa_fetch(
    doi = c("https://doi.org/10.1002/2014GL062730"),
    entity = "works",
    verbose = TRUE
  )
  
  austin_holland_oa <- oa_fetch(
    author.id = "A2134238098",
    entity = "works",
    verbose = TRUE
  )
  
  austin_holland_doi_2 <- oa_fetch(
    doi = c("https://doi.org/10.1785/gssrl.74.1.20"),
    entity = "works",
    verbose = TRUE
  )
  
  austin_holland_oa_2 <- oa_fetch(
    author.id = "A2516966732",
    entity = "works",
    verbose = TRUE
  )
  
  
  ### Check if both are dfs
  class (austin_holland_oa)
  class (austin_holland_oa_2)
  
  austin_holland_merged <- rbind(austin_holland_oa, austin_holland_oa_2)
  
  ### Combined, but need to clean because of duplicates etc. but we'll do this after we finish the other authors.
  
  
  ### 2. Aline Gubrium
  
  aline_gubrium_auth <- oa_fetch(
    entity = "authors",
    display_name = c("Aline C. Gubrium"),
    verbose = TRUE
  )
  
  aline_gubrium_oa <- oa_fetch(
    author.id = "A3175719893",
    entity = "works",
    verbose = TRUE
  )
  
  aline_gubrium_doi <- oa_fetch(
    doi = c("https://doi.org/10.2105/AJPH.2015.302900"),
    entity = "works",
    verbose = TRUE
  )
  
  ### Aline Gubrium ORCID 0000-0001-6012-365X
  
  aline_gubrium_orcid <- oa_fetch(
    entity = "works",
    author.orcid = "https://orcid.org/0000-0001-6012-365X",
    verbose = TRUE
  )
  
  aline_gubrium_oa <- oa_fetch(
    author.id = "A266378171",
    entity = "works",
    verbose = TRUE
  )
  
  ### Note, both ORCID and OA ID give the same results 
  
  ### 3. Bart Knijnenburg
  
  bart_k_doi <- oa_fetch(
    doi = c("https://doi.org/10.1007/s11257-011-9118-4"),
    entity = "works",
    verbose = TRUE
  )
  
  bart_k_orcid <- oa_fetch(
    entity = "works",
    author.orcid = "https://orcid.org/0000-0003-1341-0669",
    verbose = TRUE
  )
  
  bart_k_oa <- oa_fetch(
    author.id = "A1236026310",
    entity = "works",
    verbose = TRUE
  )
  
  ### Note, both ORCID and OA ID give the same results 
  
  ### 4. Ron Heiniger
  
  ron_h_doi <- oa_fetch(
    doi = c("https://doi.org/10.1007/s12155-014-9445-5"),
    entity = "works",
    verbose = TRUE
  )
  
  ron_h_oa <- oa_fetch(
    author.id = "A2168691789",
    entity = "works",
    verbose = TRUE
  )
  
  ### 5. Michael L. Chikindas
  
  michael_l_doi <- oa_fetch(
    doi = c("https://doi.org/10.1016/j.ijfoodmicro.2006.07.008"),
    entity = "works",
    verbose = TRUE
  )
  
  michael_l_orcid <- oa_fetch(
    entity = "works",
    author.orcid = "https://orcid.org/0000-0001-7265-4562",
    verbose = TRUE
  )
  
  michael_l_oa <- oa_fetch(
    author.id = "A630868860",
    entity = "works",
    verbose = TRUE
  )
  
  ### Again, the ORCID and OAID match, which is good news 
  
  ### 6. Raynor Mullins
  
  # 
  # raynor_m_doi <- oa_fetch(
  #   doi = c("https://doi.org/10.14219/jada.archive.1990.0197"),
  #   entity = "works",
  #   verbose = TRUE
  # )
  # 
  # raynor_m_oa <- oa_fetch(
  #   author.id = "A2098062119",
  #   entity = "works",
  #   verbose = TRUE
  # )
  
  ### Doesn't have too many publications, so I removed him from my dataset.
  
list_dfs <- list("austin_holland"=austin_holland_merged, "aline_gubrium" = aline_gubrium_oa, "bart" = bart_k_oa, "ron" = ron_h_oa, "michael" = michael_l_oa)
return(list_dfs)
}