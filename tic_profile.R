# this code was pulled using the purl command from knitr:
knitr:: purl("tic_profile.Rmd")

## ----message=FALSE, warning=FALSE----------------------------------------
library(stringdist)
library(rvest)
library(tm)
library(igraph)
library(visNetwork)

## ------------------------------------------------------------------------
tics <- c("JPM","BAC","GOOG","AAPL","MMM","AAC","T","VZ","XOM","CVX","KO","BUD")

## ------------------------------------------------------------------------
read_profile <- function(tic) {
  theurl <- paste("https://finance.yahoo.com/quote/",tic,"/profile?p=",tic,sep = "")
  file1 <- read_html(theurl)
  text1 <- file1 %>%
    html_nodes("section") %>%
    html_text()
  
  text1 <- text1[grep("description",text1,ignore.case = T)]
  text1 <- text1[2]

  return(text1)
  }

## ------------------------------------------------------------------------
profile.list <- lapply(tics, read_profile)
names(profile.list) <- tics

## ------------------------------------------------------------------------
profile.list <- lapply(profile.list,tolower)
profile.list <- lapply(profile.list,function(x) removeWords(x, stopwords("english")) )
profile.list <- lapply(profile.list,removePunctuation)
profile.list <- lapply(profile.list,removeNumbers)


## ------------------------------------------------------------------------
words_u <- lapply(profile.list, function(x) unique(unlist(strsplit(x, " "))) )
words_u <- lapply(words_u, function(x) x[nchar(x)>0] )

## ------------------------------------------------------------------------
intersect(words_u$JPM,words_u$BAC)

## ------------------------------------------------------------------------
intersect(words_u$JPM,words_u$BUD)

## ------------------------------------------------------------------------
s1 <- "Majeed loves R programming"
s2 <- "Majeed loves Sichuan food"
stringdist(s1,s2,method = "jw")

## ------------------------------------------------------------------------
s3 <- "Majeed loves food"
stringdist(s1,s3,method = "jw")

## ------------------------------------------------------------------------
M <- data.frame(t(combn(tics,2)))
dim(M)

## ------------------------------------------------------------------------
M$D <- apply(M,1,function(x) stringdist(profile.list[[x[1]]],profile.list[[x[2]]],method = "jw")   )
head(M,11)

## ------------------------------------------------------------------------
n <- length(tics)
W <- matrix(NA,n,n)
rownames(W) <- colnames(W) <- tics
W[lower.tri(W)] <- M$D
W[upper.tri(W)] <- t(W)[upper.tri(W)]

## ------------------------------------------------------------------------
W[W > 0.25] <- NA
logit <- function(p) log(p)/log(1-p)
W <- logit(W)
W[is.na(W)] <- 0
data.frame(W)

## ------------------------------------------------------------------------
WW=graph.adjacency(W,diag=TRUE,weighted = TRUE,mode = "undirected" ) 
data <- toVisNetworkData(WW)

# get the edges/links
vis.links <- data$edges
vis.links$value <- log(vis.links$weight)

# get the nodes
vis.nodes <- data$nodes
vis.nodes$label  <- vis.nodes$label 
vis.nodes$font.size  <-30
vis.nodes$font.color <- "black"

# add 6 different colors to highlight industries
pal <- colorRampPalette(c("yellow","blue"))
cols <- sort(rep(pal(n/2),2))
vis.nodes$color.background <- cols
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.border <- "darkred"

# finally, visualize the network
Net <- visNetwork(vis.nodes, vis.links) %>%
  visOptions(highlightNearest = T) %>%
  visLayout(randomSeed = 11)  %>% 
  visPhysics(stabilization = FALSE)
  # %>% visIgraphLayout(layout = "layout_with_fr")
Net %>% visSave(file = "Net.html")

## ------------------------------------------------------------------------
htmltools::includeHTML("Net.html")

