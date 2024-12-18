
## ---------------------------
## Purpose of script: R code for INF4000 data visualisations
## Author: Ned Blackburn
## Date Created: 2024-12-11

options(scipen = 6, digits = 5) 
library(tidyverse)
library(hrbrthemes)
library(ggbump)

## ---------------------------
# Load in data ------------------------------------------------------------


artist_meta <- read_delim('Datasets/musicoset_metadata/artists.csv', delim = '\t') |>
  select(c('artist_id','main_genre'))

song_pop <- read_delim('Datasets/musicoset_popularity/song_pop.csv', delim = '\t') |>
  select(c('song_id','year_end_score', 'year')) |>
  distinct(song_id, .keep_all = TRUE)

#remove extraneous characters from artist name column and add in artist main genres from artist_meta, 
#splitting up artist ideas for collaborative songs. Also add in year-end song popularity from song_pop

hits <- hits |>
  mutate(name_artists = str_remove_all(name_artists, "\\['|'\\]|'")) |>
  mutate(id_artists = str_remove_all(id_artists, "\\['|'\\]|'")) |>
  separate(id_artists, into = c("id_artists", "id_collaborators"), sep = ", ", fill = "right") |>
  left_join(artist_meta, by = c('id_artists' = 'artist_id')) |>
  left_join(song_pop, by = 'song_id')

#do the same for non-hits

non_hits <- non_hits |>
  mutate(name_artists = str_remove_all(name_artists, "\\['|'\\]|'")) |>
  mutate(id_artists = str_remove_all(id_artists, "\\['|'\\]|'")) |>
  separate(id_artists, into = c("id_artists", "id_collaborators"), sep = ", ", fill = "right") |>
  left_join(artist_meta, by = c('id_artists' = 'artist_id'))


# EDA and data processing ---------------------------------------------------------------------
#explore top genres across the dataset (other than 'no genre' which is #1)

top_genres <- hits |>
  group_by(main_genre) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  slice(2:11)

top_genres_plot <- top_genres |>
  ggplot(aes(x = main_genre, y= count, fill = main_genre)) +
  geom_col() +
  theme_ipsum_rc() +
  scale_fill_viridis_d()

#We have way too many niche genres. Let's amalgamate them based on keywords

#define keywords (based on genres from allmusic.com)

hip_hop <- c('hip hop', 'hip-hop', 'rap', 'g funk')
jazz_funk <- c('jazz', 'funk')
soul_randb <- c('r&b', 'soul', 'funk', 'jack', 'gospel', 'quiet storm', 'disco')
country <- c('country', 'bluegrass')
blues <- c('blues')
folk <- c('folk')
rock_metal <- c('rock', 'indie','permanent wave','metal','nu','emo','punk','core','screamo', 'british invasion') 
pop <- c('pop', 'neo mellow', 'boy band')
electronic <- c('house', 'techno', 'dance', 'edm', 'electro','big room','dubstep','brostep','downtempo', 'reggaeton', 'miami')
adult_contemporary <- c('adult', 'standards')


hits <- hits |>
  filter(main_genre != '-') |>
  mutate(main_genre = str_to_lower(main_genre)) |>
  mutate(
    genre_agg = case_when(
      str_detect(main_genre, str_c(hip_hop, collapse = "|")) ~ "Hip-Hop",
      str_detect(main_genre, str_c(jazz_funk, collapse = "|")) ~ "Jazz/Funk",
      str_detect(main_genre, str_c(soul_randb, collapse = "|")) ~ "Soul/R&B",
      str_detect(main_genre, str_c(country, collapse = "|")) ~ "Country",
      str_detect(main_genre, str_c(blues, collapse = "|")) ~ "Blues",
      str_detect(main_genre, str_c(folk, collapse = "|")) ~ "Folk",
      str_detect(main_genre, str_c(rock_metal, collapse = "|")) ~ "Rock/Metal",
      str_detect(main_genre, str_c(pop, collapse = "|")) ~ "Pop",
      str_detect(main_genre, str_c(electronic, collapse = "|")) ~ "Electronic",
      str_detect(main_genre, str_c(adult_contemporary, collapse = "|")) ~ "Adult Contemporary",
      TRUE ~ "Other"
    )
  )

#now let's examine the top genres again

top_genres_agg <- hits |>
  group_by(genre_agg) |>
  summarise(count = n()) |>
  arrange(desc(count))

top_genres_plot_agg <- top_genres_agg |>
  mutate(genre_agg = fct_reorder(genre_agg, count, .desc = TRUE)) |>
  ggplot(aes(x = genre_agg, y= count)) +
  geom_col(fill = 'chartreuse') +
  theme_ipsum_rc() +
  scale_fill_viridis_d()


# Plot 1: bump plot comparing popularity of genres in five year intervals --------

#create new dataframe showing average song popularity per genre per decade, and convert to a ranking variable for each genre
genres_over_time <- hits |>
  mutate(decade = floor(year / 10) * 10) |>
  filter(decade != 2010) |>
  filter(genre_agg != 'Other') |>
  select(c('song_id','genre_agg','decade','year_end_score')) |>
  group_by(genre_agg, decade) |>                    
  summarise(popularity = sum(year_end_score, na.rm = TRUE), .groups = "drop") |>
  group_by(decade) |>                         
  mutate(ranking = dense_rank(desc(popularity))) |>
  ungroup()

genre_focus <- c('Hip-Hop', 'Adult Contemporary', 'Rock/Metal', 'Pop', 'Electronic')

bump_plot <- genres_over_time |>
  ggplot(aes(x = decade, y = ranking, group = genre_agg)) +
    geom_bump(linewidth = 0.7, smooth = 6, color = 'gray90') +
    geom_bump(aes(color = genre_agg), linewidth = 1.0,smooth = 6, 
              data = ~. |> filter(genre_agg %in% genre_focus)) +
    geom_point(color = "white", size = 4) +
    geom_point(color = "gray90", size = 2) +
    geom_point(aes(color = genre_agg), size = 2, 
               data = ~. |> filter(genre_agg %in% genre_focus)) +
    scale_y_reverse(breaks = c(11:1), 
                    expand = c(0.02,0)) +
    geom_text(aes(label = genre_agg), x = 2001, hjust = 0,
              color = "gray50", size = 3.5,
              data = ~. |> slice_max(decade, by = genre_agg) |> 
                filter(!genre_agg %in% genre_focus)) +
    geom_text(aes(label = genre_agg), x = 2001, hjust = 0,
              color = "black", size = 3.5, fontface = 'bold',
              data = ~. |> slice_max(decade, by = genre_agg) |> 
                filter(genre_agg %in% genre_focus)) +
    scale_x_continuous(limits = c(1958, 2010), expand = c(0.01,0),
                  breaks = c(1960, 1970, 1980, 1990, 2000)) + 
    coord_cartesian(clip = "off") +
    labs(
      title = "Average Popularity by Genre Over Decades",
      x = "",
      y = "Popularity Ranking",
      color = "Genre"
      ) + 
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.ticks = element_line(),
          plot.title.position = "plot") +
    scale_colour_viridis_d()


# Plot 2: features of hits and non hits -----------------------------------
#create datasets with the average values of each musical characteristic for hits and non hits, then join and reshape

hits_properties <- hits |>
  mutate(across(c(energy, danceability, valence, acousticness, instrumentalness, liveness, speechiness), as.numeric)) |>
  summarise(across(c(energy, danceability, valence, acousticness, instrumentalness, liveness, speechiness), mean, na.rm = TRUE))

non_hits_properties <- non_hits |>
  mutate(across(c(energy, danceability, valence, acousticness, instrumentalness, liveness, speechiness), as.numeric)) |>
  summarise(across(c(energy, danceability, valence, acousticness, instrumentalness, liveness, speechiness), mean, na.rm = TRUE))

#reshaping the dataframe for plotting, also calculating the differences between hits/non hits so we can order the graph better

hits_v_not <- bind_rows(hits_properties,non_hits_properties, .id = 'type') |>
  pivot_longer(cols = 2:8, names_to = "musical_feature", values_to = "average_value") |>
  pivot_wider(names_from = type ,values_from = average_value) |>
  rename('hits' = 2, 'non_hits' = 3) |>
  mutate(diff = hits - non_hits) |>
  mutate(musical_feature = as.factor(musical_feature)) |>
  group_by(musical_feature) |>
  mutate(max=max(hits, non_hits)) |>
  mutate(musical_feature=fct_reorder(musical_feature, diff)) |>
  ungroup() 

hits_v_not <- hits_v_not |>
  mutate(diff_max=if_else(hits_v_not$hits==max, "Hits","Non-hits"),
         diff_label= paste0("+", abs(diff), diff_max) |>
         fct_inorder())

  

#plot the final lollipop chart, adding data labels to each point and a text geom on the right to show the percentage difference

hits_vs_not_plot <- hits_v_not |> 
  ggplot() +
  geom_segment(aes(x=reorder(musical_feature, diff), 
                   xend=reorder(musical_feature, diff), 
                   y=hits, yend=non_hits), 
               color="lightgrey", 
               linewidth = 3.5) +
  geom_point(aes(x=reorder(musical_feature, diff), 
                 y=hits), 
             color='chartreuse', 
             size=3.5 ) +
  geom_point(aes(x=reorder(reorder(musical_feature, diff), diff), 
                 y=non_hits), 
             color='red', 
             size=3.5 ) +
  coord_flip() +
  theme_ipsum() +
  scale_fill_discrete(labels=c('Hits','Non-hits')) +
  theme(panel.grid.major.y = element_blank()) +
  xlab("Musical property") +
  ylab("Index value (0 to 1)")






text_label <- hits_v_not |>
  ggplot(aes(x=diff,y=musical_feature)) +
  geom_text(aes(x=0, label=gap_label, color=gap_party_max),
            fontface="bold",
            size=3.25) +
  
  geom_text(aes(x=0, y=7), # 7 because that's the # of y-axis values
            label="Diff",
            nudge_y =.5, # match the nudge value of the main plot legend    
            fontface="bold",
            size=3.25) +
  
  theme_void() +
  coord_cartesian(xlim = c(-.05, 0.05), 
                  ylim=c(1,7.5) # needs to match main plot
  )+
  theme(
    plot.margin = margin(l=0, r=0, b=0, t=0), #otherwise it adds too much space
    panel.background = element_rect(fill="#EFEFE3", color="#EFEFE3"),
    legend.position = "none"
  )+
  scale_color_manual(values=c("#436685", "#BF2F24"))
