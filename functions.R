library(XML)
library(RCurl)

search_author <- function(author_full_name, retmax=99999, trail=FALSE) {
  # search for author name, return xml summary of every articles.
  if (trail) {
    author_full_name <- paste("Clinical Trial[ptyp]", author_full_name, 
                              sep=" AND ")
  }
  
  esearch <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  page <- getForm(esearch, db = 'pubmed', 
                  usehistory='y',
                  retmax=retmax,
                  term = paste(author_full_name, "[fau]", sep=""))
  doc <- xmlParse(page, asText = TRUE)
  qk <- xmlValue(doc[["//QueryKey"]])
  we <- xmlValue(doc[["//WebEnv"]])
  print("search finished")
  # fetch it using the history server
  efetch <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
  page <- getForm(efetch, db = 'pubmed', query_key = qk,
                  WebEnv = we, retmode = 'xml')
  return(xmlRoot(xmlTreeParse(page)))
}

locate <- function(address) {
  # clear the address
  address <- unlist(strsplit(address, split=" "))
  address.clean <- address[1]
  for (i in 1:length(address)) {
    if (!grepl(pattern="(@)", x=address[i])) {
      address.clean <- paste(address.clean, address[i], sep=" ")
    }
  }
  
  # search using geocoding
  geocoding <- "http://maps.googleapis.com/maps/api/geocode/xml"
  page <- getForm(geocoding, address = address.clean, sensor = 'false')
  doc <- xmlParse(page, asText = TRUE)
  if (xmlValue(doc[["//status"]]) == "OK") {
    lng <- as.numeric(xmlValue(doc[["//location/lng"]]))
    lat <- as.numeric(xmlValue(doc[["//location/lat"]]))
    return(c(lng, lat))
  } else {
    return(c(NA, NA))
  }
}


extract_author_info <- function(root) { 
  # article information
  article_title <- sapply(getNodeSet(root, "//ArticleTitle"), xmlValue)
  journal_title <- sapply(getNodeSet(root, "//Title", ), xmlValue)
  
  year <- sapply(getNodeSet(root, 
                            "//PubMedPubDate[@PubStatus='medline']/Year"), 
                 xmlValue)
  
  month <- sapply(getNodeSet(root, 
                            "//PubMedPubDate[@PubStatus='medline']/Month"), 
                 xmlValue)
  
  day <- sapply(getNodeSet(root, 
                            "//PubMedPubDate[@PubStatus='medline']/Day"), 
                 xmlValue)
  author_list <- list()
  first_author <- character()
  last_author <- character()
  solo_author <- logical()
  # find author type
  for (i in 1:length(root)) {
    ln <- sapply(getNodeSet(root[[i]], "//LastName"), xmlValue)
    fn <- sapply(getNodeSet(root[[i]], "//ForeName"), xmlValue)
    author_list[[i]] <- paste(ln, fn, sep=",")
    first_author[[i]] <- author_list[[i]][1]
    last_author[[i]] <- author_list[[i]][length(author_list[[i]])]
    solo_author[[i]] <- length(author_list[[i]]) == 1
  }
  
  
  # search for coordinates
  affiliation <- sapply(getNodeSet(root, "//Affiliation"), xmlValue)
  # coordinates <- t(sapply(affiliation, locate))
  
  
  article.info <- list(article_title, journal_title, year, month, day,
                       author_list, first_author, last_author, solo_author, 
                       affiliation)
  names(article.info) <- c('at', 'jt', 'year', 'month', 'day', 'al',
                           'fa', 'la', 'sa', 'aff')
  
  return(article.info)
}


locate_coauthor <- function(coauthor) {
  root <- search_author(coauthor, 20)
  print("Found author")
  address <- sapply(getNodeSet(root, "//Affiliation"), FUN=xmlValue)
  positions <- t(sapply(address, locate))
  print("located")
  positions <- matrix(positions[!is.na(positions[ , 1]), ], byrow=TRUE, ncol=2)
  positions <- paste(positions[ , 1], positions[ , 2])
  coordinate <- names(which.max(summary(as.factor(positions))))
  coordinate <- as.numeric(strsplit(coordinate, split=" ")[[1]])
  return(coordinate)
}
