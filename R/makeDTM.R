
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
  if(!any(grepl("LABEL", colnames(docs))))
    print("Please use LABEL.name argument")

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

