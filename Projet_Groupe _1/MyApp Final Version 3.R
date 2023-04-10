# libreray download if not already done
if (!require(rsconnect)) {
  install.packages('rsconnect')
  require(rsconnect)
}

if (!require(shiny)) {
  install.packages('shiny')
  require(shiny)
}

if (!require(DT)) {
  install.packages('DT')
  require(DT)
}

if (!require(shinyjs)) {
  install.packages('shinyjs')
  require(shinyjs)
}



# call library
library(shiny)
library(rsconnect)
library(tidyr)
library(recommenderlab)
library(shinyjs)

#Def functions 
top5recom = function (recommendation, limit = 6){
  for (i in 1:length(recommendation)){
    a=1
    stop = 1 
    y = artists$charid[artists$charid %in% recommendation[[i]]]
    recommendation[[i]] = 0
    while (a < limit){
      if (!is.na(y[a])){
        recommendation[[i]] = append(recommendation[[i]],y[a],after=1)
        a = 1 + a
      }
    }
  }
  return(recommendation)
}

# mes identifients
rsconnect::setAccountInfo(name='antoinebimont',
                          token='49EE54847B3C96C225E811C483E08706',
                          secret='u/5oPm+T1EVccOkuSzML+n88O+pnwb1N166sAKou')
#rsconnect::deployApp('C:/Users/angel/Desktop/Projet R shiny')
# Load data
artists <- read.table("artists_gp1.dat", sep="\t", stringsAsFactors=FALSE, comment.char="", quote="", header=TRUE)
user_artists <- read.table("user_artists_gp1.dat", sep="\t", header=TRUE)
write.csv(user_artists,"user_artists_gp1.csv")
write.csv(artists,"artists_gp1.csv")

# Data transformation
user_artists_wide <- spread(user_artists, key=artistID, value=weight)
artists$charid <- paste0("I", artists$id)
userids <- user_artists_wide$userID
user_artists_wide$IuserID <- NULL
rownames(user_artists_wide) <- paste0("U", userids)
colnames(user_artists_wide) <- paste0("I", colnames(user_artists_wide))
#list_id <- as.list(rownames(user_artists_wide))

visits_byitem <- colSums(user_artists_wide[, -1], na.rm=TRUE)
visits_1k <- user_artists_wide[, order(visits_byitem, decreasing=TRUE)[1:4000]]
num_visits <- apply(visits_1k, 1, function(x) sum(!is.na(x)))
visits_1k <- visits_1k[num_visits > 10, ]
visits_1k <- t(scale(t(visits_1k))[, ])

##dim(visits_1k)
list_id <- as.list(rownames(visits_1k))[1472:1660]
set.seed(100)


# Convert visits_1k into a recommenderlab sparse matrix
visits_1k_rrm <- as(as.matrix(visits_1k), "realRatingMatrix")

### POPULAR RECOMENDATAION
# Create recommender from the first 1472 users
rr_pop <- Recommender(visits_1k_rrm[1:1472], method = "POPULAR")
recom_pop <- predict(rr_pop, visits_1k_rrm[1472:1660], n = 10)
Myrecommendations_pop <- as(recom_pop, "list")

Myrecommendations_pop = top5recom(Myrecommendations_pop)


### UBCF RECOMENDATAION
# Create recommender from the first 1472 users
rr_UBCF <- Recommender(visits_1k_rrm[1:1472], method = "UBCF")
recom_UBCF <- predict(rr_pop, visits_1k_rrm[1472:1660], n = 10)
Myrecommendations_UBCF <- as(recom_UBCF, "list")

Myrecommendations_UBCF = top5recom(Myrecommendations_UBCF)


### ALS RECOMENDATAION
# Create recommender from the first 1472 users
rr_als <- Recommender(visits_1k_rrm[1:1472], method = "ALS")
recom_als <- predict(rr_als, visits_1k_rrm[1472:1660], n = 10)
Myrecommendations_als <- as(recom_als, "list")

Myrecommendations_als = top5recom(Myrecommendations_als)

names(Myrecommendations_als) = rownames(visits_1k)[1472:1660]

### Random RECOMENDATAION
# Create recommender from the first 1472 users
rr_random <- Recommender(visits_1k_rrm[1:1472], method = "Random")
recom_random <- predict(rr_random, visits_1k_rrm[1472:1660], n = 50)
Myrecommendations_random <- as(recom_random, "list")

Myrecommendations_random = top5recom(Myrecommendations_random)

names(Myrecommendations_random) = rownames(visits_1k)[1472:1660]

### Listened to Artists
user_row = setNames(as(visits_1k_rrm,'list')[1472:1660], rownames(visits_1k)[1472:1660])

for (i in 1:length(user_row)){
  user_row[[i]] = sort(user_row[[i]], decreasing=TRUE)
  user_row[[i]] = rownames(as.data.frame(user_row[[i]]))
}

# UI
source('ui1.R')

# Server
source('server5.R')

# Run the app
shinyApp(ui, server)

