
#' makeDTM
#'
#' make DTM
#' @param docs dataframe. Every row is document.
#' @param key character. Vector type Keywords. if none, all words would be keywords.
#' @param LABEL logical. In need of LABEL column.
#' @param weight character. tf or tfidf
#' @param TEXT.name character. Column name which whould be TEXT.
#' @param LABEL.name character. Column name which would be LABEL.
#' @param RHINO logical. In need of morphological anaysis about TEXT column.
#' @param pos character. ALL(all Part-Of-Speech), noun(NNG, NNP, NP), verb(VV, VA, XR), NV(noun, verb), END(EC, EF), NNG, NNP, NP, NNB, VV, VA, XR, VX, EC, EF, EP
#' @return matrix or dataframe(if LABEL=TRUE)
#' @export
#' @examples
#' makeDTM(docs = docs, key = c("excel", "computer"), pos="noun")
#' makeDTM(docs = docs, key = c("excel", "computer"), LABEL = TRUE, weight = "tfidf", TEXT.name="body", LABEL.name="tag", pos="verb")
#' makeDTM(docs = docs, key = c("excel", "computer"), weight = "tfidf", TEXT.name="body", LABEL.name="tag", pos="verb")

makeDTM <- function(docs, key="all", LABEL=FALSE, weight = "tf", TEXT.name=NULL, LABEL.name=NULL, RHINO=FALSE, pos="ALL")
{
  # Change column name
  doc.col <- colnames(docs)
  if(!is.null(TEXT.name))
    doc.col <- replace(doc.col, doc.col==TEXT.name, "TEXT")
  if(!is.null(LABEL.name))
    doc.col <- replace(doc.col, doc.col==LABEL.name, "LABEL")
  colnames(docs) <- doc.col
  
  if(!any(grepl("TEXT", colnames(docs))))
    print("Please use TEXT.name argument")
  if(LABEL==TRUE||!is.null(LABEL.name))
  {
    if(!any(grepl("LABEL", colnames(docs))))
      print("Please use LABEL.name argument")
  }
  
  docs$TEXT <- as.character(docs$TEXT)
  
  # Morphological Analysis for TEXT column
  if(RHINO)
  {
    library(RHINO)
    #initRhino()
    docs$TEXT <- lapply(docs$TEXT, getMorph, pos)
    for(i in 1:length(docs$TEXT))
      docs$TEXT[i] <- paste(docs$TEXT[[i]], collapse = " ")
    docs$TEXT <- as.character(docs$TEXT)
  }
  
  # If not set for keyword, make every word as keyword
  if(length(key)==1)
  {
    if(key[1] == "all")
      key <- unique(unlist(strsplit(docs$TEXT, split = " ")))
  }
  
  # Calculate tf or tf-idf
  dtm <- matrix(, nrow = NROW(docs), ncol = length(key))          # null matrix for final result
  dtm.inter <- matrix(, nrow = NROW(docs), ncol = length(key))    # matrix for idf 
  
  if(weight=="tf"){
    
    for(i in 1:NROW(docs))
    {
      docwords <- unlist(strsplit(docs$TEXT[i], split = " "))
      dtm[i,] <- table(docwords[which(docwords %in% key)])[key]
    }
    
    rownames(dtm) <- 1:NROW(docs)
    colnames(dtm) <- key
    dtm[is.na(dtm)] <- 0    # Change every NA to 0 
    
    
  }else if(weight=="tfidf"){
    
    # Calculate tf
    for(i in 1:NROW(docs)) {
      docwords <- unlist(strsplit(docs$TEXT[i], split = " "))
      dtm[i,] <- table(docwords[which(docwords %in% key)])[key]
    }
    
    rownames(dtm) <- 1:NROW(docs)
    colnames(dtm) <- key
    dtm[is.na(dtm)] <- 0    # Change every NA to 0 
    
    # Calculate idf
    for(i in 1:NROW(docs)) {
      inter <- intersect(unlist(strsplit(docs$TEXT[i], split = " ")), key)  # intersection
      dtm.inter[i,] <- table(inter[which(inter %in% key)])[key]
    }

    rownames(dtm.inter) <- 1:NROW(docs)
    colnames(dtm.inter) <- key
    dtm.inter[is.na(dtm.inter)] <- 0
    
    dtm.inter.df <- as.data.frame(dtm.inter)
    doc_word_freq <- sapply(dtm.inter.df, sum)   # The number of documents which has term t
    
    idf <- log(NROW(docs) / doc_word_freq)       # idf value
    idf[is.infinite(idf)] <- 0
    
    for(i in 1:ncol(dtm))
      dtm[,i] <- dtm[,i] * idf[i]                # tf * idf
  }
  
  if(LABEL||!is.null(LABEL.name))                # Make dataframe first, then paste LABEL
  {
    dtm <- as.data.frame(dtm)
    dtm <- cbind(dtm, LABEL=docs$LABEL)
  }
  
  return(dtm)
}



# This code is just for saving
# This code speed is very slow 
'
makeDTM_origin <- function(docs, key="all", LABEL=FALSE, weight = "tf", TEXT.name=NULL, LABEL.name=NULL, RHINO=FALSE, pos="ALL")
{
  # Change Column name
  doc.col <- colnames(docs)
  if(!is.null(TEXT.name))
    doc.col <- replace(doc.col, doc.col==TEXT.name, "TEXT")
  if(!is.null(LABEL.name))
    doc.col <- replace(doc.col, doc.col==LABEL.name, "LABEL")
  colnames(docs) <- doc.col

  if(!any(grepl("TEXT", colnames(docs))))
    print("Please use TEXT.name argument")
  if(LABEL==TRUE)
  {
    if(!any(grepl("LABEL", colnames(docs))))
      print("Please use LABEL.name argument")
  }

  docs$TEXT <- as.character(docs$TEXT)

  # Morphological analysis for TEXT column
  if(RHINO)
  {
    library(RHINO)
    initRhino()
    docs$TEXT <- lapply(docs$TEXT, getMorph, pos)
    for(i in 1:length(docs$TEXT))
      docs$TEXT[i] <- paste(docs$TEXT[[i]], collapse = " ")
    docs$TEXT <- as.character(docs$TEXT)
  }

  # If not set for keyword, make every word as keyword
  if(length(key)==1)
  {
    if(key[1] == "all")
      key <- unique(unlist(strsplit(docs$TEXT, split = " ")))
  }

  # Calculate tf or tf-idf
  dtm <- c()
  if(weight=="tf"){
    for(i in 1:nrow(docs)){
      docwords <- unlist(strsplit(docs$TEXT[i], split = " "))
      i_word <- docwords[which(docwords %in% key)]
      dtm <- rbind(dtm, table(i_word)[key])
    }

    rownames(dtm) <- 1:nrow(docs)
    colnames(dtm) <- key
    dtm[is.na(dtm)] <- 0
  }else if(weight=="tfidf"){

    # Calculate tf
    for(i in 1:nrow(docs)){
      docwords <- unlist(strsplit(docs$TEXT[i], split = " "))
      i_word <- docwords[which(docwords %in% key)]
      dtm <- rbind(dtm, table(i_word)[key])
    }

    # Calculate idf
    contents <- c()
    for(i in 1:nrow(docs))
    {
      inter <- intersect(unlist(strsplit(docs$TEXT[i], split = " ")), key)  # intersection
      contents <- rbind(contents, table(inter)[key])
    }

    rownames(dtm) <- 1:nrow(docs)
    colnames(dtm) <- key
    dtm[is.na(dtm)] <- 0

    rownames(contents) <- 1:nrow(docs)
    colnames(contents) <- key
    contents[is.na(contents)] <- 0

    contents.df <- as.data.frame(contents)
    doc_word_freq <- sapply(contents.df, sum)   # The number of documents which has term t

    idf <- log(nrow(docs) / doc_word_freq)      # idf value
    idf[is.infinite(idf)] <- 0

    for(i in 1:ncol(dtm))
      dtm[,i] <- dtm[,i] * idf[i]               # tf * idf
  }

  if(LABEL)             # Make dataframe first, then paste LABEL
  {
    dtm <- as.data.frame(dtm)
    dtm <- cbind(dtm, LABEL=docs$LABEL)
  }

  return(dtm)
}
'


#' findAssocTwo
#'
#' find association of two terms
#' @param dtm matrix. Document Term Matrix
#' @param term1 character. keyword 1.
#' @param term2 character. keyword 2.
#' @return numeric. correlation of two terms.
#' @export
#' @examples
#' findAssocTwo(dtm = dtm, term1 = "home", term2 = "family")

findAssocTwo <- function(dtm, term1, term2)
{
  word1 <- as.vector(dtm[, term1])
  word2 <- as.vector(dtm[, term2])
  
  return(cor(word1, word2))
}


#' findAssocs
#'
#' find association by one keyword
#' @param dtm matrix. Document Term Matrix
#' @param term character. keyword
#' @param corlimit numeric. lower correlation limit
#' @return character. correlation above corlimit.
#' @export
#' @examples
#' findAssocs(dtm = dtm, term = "home", corlimit = 0.8)

findAssocs <- function(dtm, term, corlimit=0.3)
{
  result <- c()
  names <- c()
  k <- 1
  var.colnames <- colnames(dtm)
  
  for(i in 1:NCOL(dtm))
  {
    if(term!=var.colnames[i])
    {
      var.cor <- findAssocTwo(dtm, term, var.colnames[i])
      if(abs(var.cor) >= corlimit)
      {
        result[k] <- var.cor
        names[k] <- var.colnames[i]
        k <- k+1
      }
    }
  }
  
  names(result) <- names
  return(result)
}


#' findAssocsAll
#'
#' find association of all terms
#' @param dtm matrix. Document Term Matrix
#' @param corlimit numeric. lower correlation limit
#' @return character. correlation above corlimit.
#' @export
#' @examples
#' findAssocsAll(dtm = dtm, corlimit = 0.8)

findAssocsAll <- function(dtm, corlimit=0.3)
{
  result <- c()
  names <- c()
  k <- 1
  var.colnames <- colnames(dtm)
  
  for(i in 1:NCOL(dtm))
  {
    for(j in 1:NCOL(dtm))
    {
      if(var.colnames[i]!=var.colnames[j])
      {
        var.cor <- findAssocTwo(dtm, var.colnames[i], var.colnames[j])
        if(abs(var.cor) >= corlimit)
        {
          result[k] <- var.cor
          names[k] <- paste(var.colnames[i], "-", var.colnames[j], sep = "")
          k <- k+1
        }
      }
    }
  }
  
  names(result) <- names
  return(result)
}


#' findFreqTerms
#'
#' find frequently used words(lowfreq) or below maximum frequency words(highfreq)
#' @param dtm matrix. Document Term Matrix
#' @param lowfreq numeric. low frequency limit
#' @param highfreq numeric. high frequency limit
#' @return character. words which met frequency condition
#' @export
#' @examples
#' findFreqTerms(dtm = dtm, lowfreq=3, highfreq=4)

findFreqTerms <- function(dtm, lowfreq=0, highfreq=0)
{
  result <- c()
  var.colnames <- colnames(dtm, do.NULL = FALSE)
  k <- 1
  
  if(lowfreq!=0 & highfreq!=0)
  {
    print("Choose only one between lowfreq and highfreq!!")
  }else
  {
    if(lowfreq != 0)
    {
      for(i in 1:NCOL(dtm))
      {
        if(sum(dtm[,i]) >= lowfreq)
        {
          result[k] <- var.colnames[i] 
          k <- k+1
        }
      } 
    }else if(highfreq != 0)
    {
      for(i in 1:NCOL(dtm))
      {
        if(sum(dtm[,i]) <= highfreq)
        {
          result[k] <- var.colnames[i] 
          k <- k+1
        }
      } 
    }
  }
  
  return(result)
}