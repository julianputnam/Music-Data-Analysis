# Goal: Visualize common trends in energy, tempo, and daceability for a variety of genres.

library(dplyr)
library(tools)
library(ggplot2)

# Import Kaggle dataset:
songs.df <- read.csv(file="music_genre.csv", na.strings=(c("","NA","?","0/4")))
songs.df$duration_ms[songs.df$duration_ms==-1] <- NA

# Function: create visualization.
scatterplotcolor <- function(x = "energy", y = "tempo", colvar = "danceability", df = songs.df, genre="all"){
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

# Execute function:
scatterplotcolor("energy", "tempo", "danceability", songs.df, "Electronic") #Example

# Get exhaustive and unique vector of genres
genre_index <- !is.na(unique(songs.df$music_genre))
genres <- unique(songs.df$music_genre)[genre_index]

# Generate a visualization for each genre
for (genre in genres){
  scatterplotcolor(genre = genre)
  ggsave(
    paste("Plots/Genre Plots/", genre, "_plot.jpg", sep=""),
    device="jpg",
  )
}
