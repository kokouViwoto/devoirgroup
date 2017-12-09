DATA_PATH="CHEMIN VERS LE DOSSIER CONTENANT LE FICHIER EX: C://TPgroupe"
DATA_FILE_NAME="movie_metadata.csv"

setwd(DATA_PATH)

movie_metadata = read.csv(DATA_FILE_NAME, 
                         header=TRUE, sep=",",
                         blank.lines.skip = TRUE,
                         stringsAsFactors = FALSE)  # read csv file

# Chham - suppression des doublons 
movie_metadata <- unique(movie_metadata)

# Sort columns names to ease reading
movie_metadata <- movie_metadata[ , order(names(movie_metadata))]