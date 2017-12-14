DATA_PATH="/Users/kviwoto/Documents/CoursHEC/LogicielsStatistiques/Projet"
DATA_FILE_NAME="movie_metadata.csv"

setwd(DATA_PATH)

##### installation des packages via le menu R
install.packages("sqldf")
install.packages("ggplot2")

##### chargement du package ggplot2 et sqldf
library(ggplot2)
library(sqldf)


########################################################################
#     Question 1.                                                      #
#     Section 1 : Nettoyage de données (principalement avec SQL et R)  # 
#                                                                      #
########################################################################


## Lecture des données.
movie_metadata = read.csv(DATA_FILE_NAME, 
                         header=TRUE, sep=",",
                         blank.lines.skip = TRUE,
                         stringsAsFactors = FALSE)  # read csv file

## Nettoyage
# suppression des doublons 
movie_metadata <- unique(movie_metadata)

# Sort columns names to ease reading
movie_metadata <- movie_metadata[ ,order(names(movie_metadata))]

deleteUnnecessaryColumn <- function(data, columnsToRemove){
  return(data[, !(names(data) %in% columnsToRemove)])
}
movie_metadata <- deleteUnnecessaryColumn(movie_metadata, c("movie_imdb_link"))

# vérifions les données chargées
str(movie_metadata)
# pas de colonne avec des variables incohérentes. 
# "color" comprend 3 valeurs au lieu de color et black and white
# "budget" est en num?rique et "gross" est en chiffre entier => P.E erreur de calcul

summary(movie_metadata)
#vide dans les couleurs (19), les nom de directeurs(104), num_critic_for_reviews 50, duration 15, director_fb_likes 104, actor3fblike 23, actor2name 13,
# actor1fblike 7, gross 884, genres 0, actor1name 0, movietitle 0, numvotedusers 0, castttlfblike 0, actor3name 0, fbposter 13, plotkeyword 0, movieimdblink 0, 
# numusr4review 21, language 0, country 0, contentrating 303, budget 492, titleyear 108, actor2fblike 13, imdbscore 0, aspectratio 329,

##Definition de la mesure de rentabilite
# Nous definissons comme rentabilite la difference gross-budget.

#Verifions si nous avons ces deux valeurs pour toutes les lignes. Si le budget est NA on enleve la ligne.

creation_colonne_rentabilite <- function(data){
  for (i in 1:nrow(data)){
    if(is.na(data$budget[i]) || is.na(data$gross[i]) || data$gross[i] <= data$budget[i]){
      data$rentabilite[i] <- 0;
    }
    else {
      data$rentabilite[i] <- data$gross[i] - data$budget[i];
    }
  }
  return(data)
}

movie_metadata <- creation_colonne_rentabilite(movie_metadata)

# Trions par rentabilite
movie_metadata <- movie_metadata[order(-movie_metadata$rentabilite),]

meilleurFilmParRentabilite <- head(movie_metadata,1);

# Decouvertes
#1- Est ce que le film le plus liker est le plus rentable ?
# Creons une colonne likes = movie_facebook_likes + director_facebook_likes + actor_3_facebook_likes + actor_2_facebook_likes + actor1_facebook_likes
getValueSafely <- function(value){
  if(is.null(value) || is.na(value)){
    return(0);
  }
  
  return(value)
}

creation_colonne_likes <- function(data){
  for (i in 1:nrow(data)){
    movie_facebook_likes = getValueSafely(data$movie_facebook_likes[i]);
    director_facebook_likes = getValueSafely(data$director_facebook_likes[i]);
    actor_3_facebook_likes = getValueSafely(data$actor_3_facebook_likes[i]);
    actor_2_facebook_likes = getValueSafely(data$actor_2_facebook_likes[i]);
    actor_1_facebook_likes = getValueSafely(data$actor1_facebook_likes[i]);
    
    data$likes[i] <- movie_facebook_likes + director_facebook_likes + actor_3_facebook_likes + actor_2_facebook_likes + actor_1_facebook_likes;
  }
  return(data)
}

movie_metadata <- creation_colonne_likes(movie_metadata);

#2 Quelle est la proportion du facebook like de l'acteur principal par rapport au facebook like total du film ?
creation_ratioLike <- function(data){
  for (i in 1:nrow(data)){
    actor_1_facebook_likes = getValueSafely(data$actor_1_facebook_likes[i]);
    if (getValueSafely(data$likes[i]) != 0 && actor_1_facebook_likes != 0){
      data$ratioLike[i] <- (actor_1_facebook_likes * 100 )/data$likes[i]
    }
    else {
      data$ratioLike[i] <- 0
    }
  }
  return(data)
}
movie_metadata <- creation_ratioLike(movie_metadata);

#3 Quelle est la liste complete de categories sans doublons dans le dataset

## Enlever les blancs en debut en fin de chaines de caractères
trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

splitCategoriesLine <- function(line){
  lineWithoutSpace = trim(line);
  if(length(lineWithoutSpace) == 0){
    return ("EMPTY");
  }
  return (unlist(strsplit(lineWithoutSpace, "[|]")))
}

getAllUniqueGenres <- function(data){
  
  allCategories <- c()
  for (i in 1:length(data)){
    newCategories <- splitCategoriesLine(data[i]);
    if (newCategories[1] != "EMPTY"){
      for(j in 1:length(newCategories)){
         allCategories <- c(allCategories, c(newCategories[[j]]))
      }
    }
  }
  return(unique(allCategories))
}

allUniqueGenres <- getAllUniqueGenres(movie_metadata$genres);

#4 Quels sont les genres de film qui a plus de facebook likes?
genresByLikesDf = data.frame("Genres"=allUniqueGenres)
genresByLikesDf$count <- 0

getGenresByLikes <- function(data){

  for (i in 1:nrow(data)){
    categories <- splitCategoriesLine(data$genres[i]);
    if (categories[1] != "EMPTY"){
      for(j in 1:length(categories)){
        genresByLikesDf[which(genresByLikesDf$Genres==categories[[j]]),]$count <- genresByLikesDf[which(genresByLikesDf$Genres==categories[[j]]),]$count + data$likes[i]
      }
    }
  }
  return(genresByLikesDf)
}

genresByLikesDf <-getGenresByLikes(movie_metadata)

genresByLikesDf <- genresByLikesDf[order(-genresByLikesDf$count),]

#5 Quel est l'acteur qui apparait dans plus de films?

getActorNameWithoutSpace <- function(line){
  if(is.na(line)){
    return ("EMPTY");
  }
  
  lineWithoutSpace = trim(line);
  if(length(lineWithoutSpace) == 0){
    return ("EMPTY");
  }
  
  return (lineWithoutSpace)
}

getAllActors <- function(data){
  
  allActors <- c()
  for (i in 1:nrow(data)){
    actor1 <- getActorNameWithoutSpace(data$actor_1_name[i]);
    actor2 <- getActorNameWithoutSpace(data$actor_2_name[i]);
    actor3 <- getActorNameWithoutSpace(data$actor_3_name[i]);
    if (actor1 != "EMPTY" && length(actor1) > 0){
      allActors <- c(allActors, c(actor1))
    }
    if (actor2 != "EMPTY" && length(actor2)){
      allActors <- c(allActors, c(actor2))
    }
    if (actor3 != "EMPTY" && length(actor3)){
      allActors <- c(allActors, c(actor3))
    }
  }
  return(sort(unique(allActors)))
}

allUniqueActors <-getAllActors(movie_metadata)

actorsByPresenceCountDf = data.frame("Actors"=allUniqueActors)
actorsByPresenceCountDf$presence <- 0

getActorsByPresenceCount <- function(data){
  
  for (i in 1:nrow(data)){
    actor1 <- getActorNameWithoutSpace(data$actor_1_name[i]);
    actor2 <- getActorNameWithoutSpace(data$actor_2_name[i]);
    actor3 <- getActorNameWithoutSpace(data$actor_3_name[i]);
    
    if (actor1 != "EMPTY"){
      actorsByPresenceCountDf[which(actorsByPresenceCountDf$Actors==actor1),]$presence <- actorsByPresenceCountDf[which(actorsByPresenceCountDf$Actors==actor1),]$presence + 1
    }
    if (actor2 != "EMPTY"){
      actorsByPresenceCountDf[which(actorsByPresenceCountDf$Actors==actor2),]$presence <- actorsByPresenceCountDf[which(actorsByPresenceCountDf$Actors==actor2),]$presence + 1
    }
    if (actor3 != "EMPTY"){
      actorsByPresenceCountDf[which(actorsByPresenceCountDf$Actors==actor3),]$presence <- actorsByPresenceCountDf[which(actorsByPresenceCountDf$Actors==actor3),]$presence + 1
    }
    
  }
  return(actorsByPresenceCountDf)
}

actorsByPresenceCountDf <- getActorsByPresenceCount(movie_metadata)

actorsByPresenceCountDf <- actorsByPresenceCountDf[order(-actorsByPresenceCountDf$presence),]












# vérification s'il n'y a pas de valeur abb.rante dans les variables numériques
# Création d'un programme qui parcourent les colonnes d'une table. Pour chaque colonne de type numérique, 3 graphiques sont crées ainsi que le
# comptage de 0 et de NA.
verif_col_quant <- function(data_x){
  for (i in 1:length(names(data_x)))
  {
    nomcol=names(data_x)
    if (is.numeric(data_x[,i]))
    {
      info_colonne <- data_x[,i]
      # création d'un histogramme
      jpeg(filename=paste("myhist_0",i,".jpeg",sep=""), width = 630, height = 552)
      myhist <- hist(info_colonne,xlab = paste("nombre de", nomcol[i]),  breaks=10, main=paste("histogramme",nomcol[i]),probability = F)
      myhist$xname <- nomcol[i]
      dev.off() 
      
      # création d'un histogramme en fréquence
      jpeg(filename=paste("myhist_0",i,"_avec_freq.jpeg",sep=""), width = 630, height = 552)
      myhist <- hist(info_colonne,col="grey",xlab = paste("nombre de", nomcol[i]),  breaks=10, main=paste("histogramme",nomcol[i]),probability = T)
      myhist$xname <- nomcol[i]
      lines(density(info_colonne, na.rm=T), lwd=2,col="red")
      dev.off() 
      
      #création d'un graphique en plot
      jpeg(filename=paste("myplot",i,".jpeg",sep=""), width = 630, height = 552)
      myplot <- plot(info_colonne, main=paste("graphique",nomcol[i]))
      myplot$xname <- nomcol[i]
      dev.off() 
      
      #comptabiliser le nombre de 0, de vide et de NA
      qtt_na <- sum(is.na(data_x[,i]))
      qtt_zero <- nrow(subset(data_x,data_x[,i]==0))
      print(paste("Dans la colonne n?",i,"intitul?",nomcol[i],"il y a",qtt_na,"NA et",qtt_zero,"z?ro"))
    }
  }
}

verif_col_quant(movie_metadata)

# vérification s'il n'y a pas de valeur abbérante dans les variables caractères
# Création d'un programme qui parcourent les colonnes d'une table. Pour chaque colonne de type non numérique, on comptabilise le nombre de 
# cellule vide, de NA et le nombre de valeurs. le programme a 2 paramètres. Le premier est le nom de la table, le second, le nombre max de 
# valeur que l'on veut afficher dans nos r?sultats.  

verif_col_qualita <- function(data_x,limit){
  
  nb_col <- length(names(data_x))
  for (i in 1:nb_col)
  {
    nomcol=names(data_x)
    if (!is.numeric(data_x[,i]) & length(unique(data_x[,i]))<limit)
    {
      # effectif
      valeur_ds_col <- (sort(unique(data_x[,i])))
      #comptabiliser le nombre de 0, de vide et de NA
      qtt_na <- sum(is.na(data_x[,i]))
      qtt_vide <- nrow(subset(data_x,data_x[,i]==""))
      print(paste("Dans la colonne n?",i,"intitul?",nomcol[i],"il y a",qtt_na,"NAs,",qtt_vide,"cellules vides et",length(valeur_ds_col),"diff?rentes valeurs"))
      print("les valeurs existantes dans cette variable sont :")
      print(valeur_ds_col)
      print("", quote=F)
    }
  }}
verif_col_qualita(movie_metadata,100)

