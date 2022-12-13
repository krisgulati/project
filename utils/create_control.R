create_control <- function (issn, from, to){
  
  journal <- oa_fetch(
    entity = "works",
    host_venue.issn = issn,
    from_publication_date = from,
    to_publication_date = to,
    verbose = TRUE
  )
  
  ### So, we have the journal, but it has a lot of stuff that's not particularly useful to us. 
  ##### So, to try and filter these, I'm thinking we remove the following:
  #1. Remove if not a journal article, under type
  #2. Remove if doesn't cite anything else
  #3. see below
  
  journal <- journal [(journal$type == "journal-article"), ] # If not a journal article
  #journal <- journal %>% drop_na(referenced_works) # Drop any NAs
  #journal <- journal [(!journal$referenced_works == "NA"), ] # Sometimes ther NAs are characters, and so remove these
  
  journal [, "originalissn"] = issn
  
  return (journal)
  
}

### In this function, I'm dropping works that haven't referenced anything (referenced_works) not sure this is the right approach, sometimes dropping articles which are seemingly important
## If oa_fetch() is accessing the Crossref DOI metadata, reference data is spotty and depends a LOT on the publisher
