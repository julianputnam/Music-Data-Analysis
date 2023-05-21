library(dplyr)
library(tools)
library(ggplot2)
library(ggrepel)

#goal: make chart of danceability vs valence for 100 most popular (indicated by color) artists

#import data
s <- read.csv("music_genre.csv", na.strings=c("NA","","-1","0/4"))
s$duration_ms[s$duration_ms==-1] <- NA

#convert to df of 100 most popular artists by averaging their top 3 songs
s <- s %>% group_by(artist_name) %>% slice_max(popularity, n=3) %>%
  summarize(pop = mean(popularity, na.rm=T), dance = mean(danceability, na.rm=T), valence = mean(valence, na.rm=T)) %>% 
  slice_max(pop, n=40)
s <- s[-5,]

nrow(s) 
head(s, 10)

#function that generates plot, variables may be changed to make different plots as long as df contains "artist_name"
popchart <- function(x = "dance", y = "valence", colvar = "pop", df = s){
  plot <- ggplot(data=df, aes(x = !!rlang::sym(x), y = !!rlang::sym(y), color = !!rlang::sym(colvar))) +
    geom_point(size=3, alpha = 1) +
    labs(x = "Danceability", y = "Valence (Happiness)", color = "Popularity") +
    scale_color_gradient(low = "purple", high = "orange") +
    ggtitle("Mood and motion of top 40 Spotify artists", "Based on 2021 data about artists' top 3 songs") +
    theme(plot.title = element_text(face = "bold")) #+
    #geom_text(aes(label=ifelse(!!rlang::sym(colvar)>85.5, artist_name, ""), hjust=0.08, vjust=-1.20))
  plot +
    geom_label_repel(aes(label=ifelse(!!rlang::sym(colvar)>85.5, artist_name, "")), 
                     box.padding = 0.40, point.padding = 0.45, segment.color = 'grey50') +
    theme_light()
}

#generate plot:
popchart()

#save plot:
ggsave("popchart.jpg", device="jpg")
