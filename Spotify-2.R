# https://www.kaggle.com/datasets/vicsuperman/prediction-of-music-genre

spot1 <- read.csv(file="music_genre.csv", na.strings=(c("","NA","?","0/4")))
spot1$duration_ms[spot1$duration_ms==-1] <- NA

library(dplyr)
library(tools)
library(ggplot2)

scatterplotcolor <- function(x = "energy", y = "tempo", colvar = "danceability", df = spot1, genre="all"){
  genre_name <- "all"
  if (genre!="all" && genre %in% unique(df$music_genre)){
    df <- df[df$music_genre==genre,]
    genre_name <- genre
  }
  ggplot(data = df, aes(x = !!rlang::sym(x), y = !!rlang::sym(y), color = !!rlang::sym(colvar))) +
    geom_point(size = 2, alpha = 0.25) +
    labs(x = toTitleCase(x), y = toTitleCase(y), color = toTitleCase(colvar)) +
    scale_color_gradient(low = "blue", high="red") +
    ggtitle(paste("Danceability of", genre_name, "songs")) +
    theme(plot.title = element_text(face = "bold"))
}

scatterplotcolor("energy", "tempo", "danceability", spot1, "Electronic") #Example

genre_index <- !is.na(unique(spot1$music_genre))
genres <- unique(spot1$music_genre)[genre_index]

for (genre in genres){
  scatterplotcolor(genre = genre)
  ggsave(
    paste(genre, "_plot.jpg", sep=""),
    device="jpg",
  )
}


spot2 <- arrange(spot1, desc(danceability))[200,]
head(spot2)
mean(spot2$tempo); mean(spot2$energy) #mean tempo and energy of top 200 danceable songs
mean(spot1$tempo, na.rm=T); mean(spot1$energy, na.rm=T) #mean tempo and energy of all songs
#possible conclusion: in order for song to be danceable, it has to be relatively energetic, and slow enough 
#that people can keep up but not much slower than that