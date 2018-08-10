
#' makeDTM
#'
#' make DTM
#' @param docs dataframe. Every row is document.
#' @param key character. Vector type Keywords. if none, all words would be keywords.
#' @param LABEL logical.
#' @param weight character. tf or tfidf
#' @param TEXT.name character. Column name which whould be TEXT.
#' @param LABEL.name character. Column name which would be LABEL.
#' @param RHINO logical. In case of morphological anaysis need for TEXT column.
#' @param pos character. Among Right: ALL(all Part-Of-Speech), noun(NNG, NNP, NP), verb(VV, VA, XR), NNG, NNP, NP, NNB, VV, VA, XR, VX
#' @return matrix or dataframe(LABEL=TRUE)
#' @export
#' @examples
#' makeDTM(docs = docs, key = c("excel", "computer"), pos="noun")
#' makeDTM(docs = docs, key = c("excel", "computer"), LABEL = TRUE, weight = "tfidf", TEXT.name="body", LABEL.name="tag", pos="verb")

makeDTM <- function(docs, key="all", LABEL=FALSE, weight = "tf", TEXT.name=NULL, LABEL.name=NULL, RHINO=FALSE, pos="ALL")
{
  # 컬럼의 이름을 바꾸기
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

  # TEXT 컬럼에 대한 형태소 분석
  if(RHINO)
  {
    library(RHINO)
    initRhino()
    docs$TEXT <- lapply(docs$TEXT, getMorph, pos)
    for(i in 1:length(docs$TEXT))
      docs$TEXT[i] <- paste(docs$TEXT[[i]], collapse = " ")
    docs$TEXT <- as.character(docs$TEXT)
  }

  # keyword 설정이 없는 경우, 모든 단어를 keyword로 삼기
  if(length(key)==1)
  {
    if(key[1] == "all")
      key <- unique(unlist(strsplit(docs$TEXT, split = " ")))
  }

  # tf 또는 tf-idf 값 구하기
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

    # tf 구하기
    for(i in 1:nrow(docs)){
      docwords <- unlist(strsplit(docs$TEXT[i], split = " "))
      i_word <- docwords[which(docwords %in% key)]
      dtm <- rbind(dtm, table(i_word)[key])
    }

    # idf 구하기
    contents <- c()
    for(i in 1:nrow(docs))
    {
      inter <- intersect(unlist(strsplit(docs$TEXT[i], split = " ")), key)  # 교집합
      contents <- rbind(contents, table(inter)[key])
    }

    rownames(dtm) <- 1:nrow(docs)
    colnames(dtm) <- key
    dtm[is.na(dtm)] <- 0

    rownames(contents) <- 1:nrow(docs)
    colnames(contents) <- key
    contents[is.na(contents)] <- 0

    contents.df <- as.data.frame(contents)
    doc_word_freq <- sapply(contents.df, sum)   # t가 나오는 문서의 수

    idf <- log(nrow(docs) / doc_word_freq)      # idf 값
    idf[is.infinite(idf)] <- 0

    for(i in 1:ncol(dtm))
      dtm[,i] <- dtm[,i] * idf[i]               # tf * idf
  }

  if(LABEL)             # 먼저 데이터프레임으로 만들고, LABEL을 붙인다
  {
    dtm <- as.data.frame(dtm)
    dtm <- cbind(dtm, LABEL=docs$LABEL)
  }

  return(dtm)
}



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


#' findAssocsAll
#'
#' find association of all terms
#' @param dtm matrix. Document Term Matrix
#' @param corlimit numeric. lower correlation limit
#' @return character. correlation above corlimit.
#' @export
#' @examples
#' findAssocsAll(dtm = dtm, corlimit = 0.8)

findFreqTerms <- function(dtm, lowfreq=2, highfreq)
{
  result <- c()
  var.colnames <- colnames(dtm)
  k <- 1
  
  for(i in 1:NCOL(dtm))
  {
    if(sum(dtm[,i] >= lowfreq))
      result[k] <- colnames(dtm[,i])
  }
  
  names(result) <- names
  return(result)
}