
## ---------------------------
## Purpose of script: R code for INF4000 data visualisations
## Author: Ned Blackburn
## Date Created: 2024-12-11

options(scipen = 6, digits = 5) 
library(tidyverse)
library(hrbrthemes)
library(ggbump)
library(circlize)
library(ComplexUpset)
library(ggupset)
library(tidytext)

## ---------------------------
# Load in data ------------------------------------------------------------


artist_meta <- read_delim('Datasets/musicoset_metadata/artists.csv', delim = '\t') |>
  select(c('artist_id','main_genre'))

song_pop <- read_delim('Datasets/musicoset_popularity/song_pop.csv', delim = '\t') |>
  select(c('song_id','year_end_score', 'year')) |>
  distinct(song_id, .keep_all = TRUE)

hits <- read_delim('Datasets/hits_dataset.csv', delim = '\t')
non_hits <- read_delim('Datasets/nonhits_dataset.csv', delim = '\t')

#remove extraneous characters from artist name column and add in artist main genres from artist_meta, 
#splitting up artist ideas for collaborative songs. Also add in year-end song popularity from song_pop

hits_master <- hits |>
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

top_genres <- hits_master |>
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

hip_hop <- c('hip hop', 'hip-hop', 'rap', 'g funk', 'crunk')
jazz_funk <- c('jazz', 'funk', 'bebop')
soul_randb <- c('r&b', 'soul', 'funk', 'jack', 'gospel', 'quiet storm', 'disco', 'groove','motown')
country <- c('country', 'bluegrass')
blues <- c('blues')
folk <- c('folk')
rock_metal <- c('rock', 'indie','permanent wave','metal','nu','emo','punk','core','screamo', 'british invasion') 
pop <- c('pop', 'neo mellow', 'boy band', 'girl group')
electronic <- c('house', 'techno', 'dance', 'edm', 'electro','big room','dubstep','brostep','downtempo', 'reggaeton', 'miami', 'bounce')
adult_contemporary_classical <- c('adult', 'standards', 'easy','soundtrack','classical','symphony','orchestra')


hits_master <- hits_master |>
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
      str_detect(main_genre, str_c(adult_contemporary_classical, collapse = "|")) ~ "Adult Contemporary",
      TRUE ~ "Other"
    )
  )

#now let's examine the top genres again

top_genres_agg <- hits_master |>
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
genres_over_time <- hits_master |>
  mutate(decade = floor(year / 10) * 10) |>
  filter(decade != 2010) |>
  filter(genre_agg != 'Other') |>
  select(c('song_id','genre_agg','decade','year_end_score')) |>
  group_by(genre_agg, decade) |>                    
  summarise(popularity = sum(year_end_score, na.rm = TRUE), .groups = "drop") |>
  group_by(decade) |>                         
  mutate(ranking = dense_rank(desc(popularity))) |>
  ungroup()

genre_focus <- c('Hip-Hop', 'Country', 'Rock/Metal', 'Pop', 'Soul/R&B')

bump_plot <- genres_over_time |>
  ggplot(aes(x = decade, y = ranking, group = genre_agg)) +
    geom_bump(linewidth = 0.9, smooth = 6, color = 'gray90') +
    geom_bump(aes(color = genre_agg), linewidth = 1.5,smooth = 6, 
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
      title = "Popularity ranking of genres",
      subtitle = "Average ranking by decade, 1960s - 2000s",
      x = "",
      y = "Popularity Ranking (1 = most popular)",
      color = "Genre",
      caption = "Data from MusicOset based on chart year-end scores. Top 10 overall most popular genres only."
      ) + 
    theme_ipsum_rc(grid = FALSE, ticks = TRUE) +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.ticks = element_line(),
          plot.title.position = "plot") +
    scale_colour_viridis_d()


# Plot 2: features of hits and non hits -----------------------------------
#create datasets with the average values of each musical characteristic for hits and non hits, then join and reshape

hits_properties <- hits_master |>
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
               linewidth = 5.5) +
  geom_point(aes(x=reorder(musical_feature, diff), 
                 y=hits), 
             color='#7ad151', 
             size=6 ) +
  geom_point(aes(x=reorder(reorder(musical_feature, diff), diff), 
                 y=non_hits), 
             color='#440154', 
             size=6 ) +
  coord_flip() +
  scale_fill_discrete(labels=c('Hits','Non-hits')) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = ) +
  labs(title = 'Musical properties of artists\'\ charting vs. non-charting songs',
       subtitle = '1960 - 2018, averaged across all genres',
       caption = 'Data from MusicOset',
       x = 'Index value (0 to 1, 1 = more)',
       y = 'Musical property'
       ) +
  theme_ipsum_rc(axis_text_size = 11, axis_title_size = 10)

#INPROGRESS

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



# collaborations chord chart --------------------------

#create flag for whether song is in the top 25% most popular songs
hits_master <- hits_master |>
  mutate(top_hit = ifelse(popularity >= quantile(popularity, 0.75),
                          1, 0))
#filter for collabs only
collabs_only <- hits_master |>
  select(c('id_artists','id_collaborators','name_artists','popularity','top_hit')) |>
  filter(id_collaborators != is.na(id_collaborators))

#manually correct additional commas in names to we can separate out the artists and collaborators
collabs_only[15,3] = 'Tyler The Creator, A$AP Rocky'
collabs_only[68,3] <- 'The Hit Co., The Tribute Co.'
collabs_only[137,3] <- 'The Hit Co., The Tribute Co.'
collabs_only[158,3] <- 'The Hit Co., The Tribute Co.'
collabs_only[161,3] <- 'Ray Parker Jr., Raydio'
collabs_only[163,3] <- 'Ray Parker Jr., Raydio'
collabs_only[164,3] <- 'Ray Parker Jr., Raydio'
collabs_only[374,3] <- 'Earth Wind & Fire, The Emotions'

#get genres for artist and collaborator from metadata table and filter only where both genres can be identified

collabs_only <- collabs_only |>
  separate(name_artists,c('artist', 'collaborator'), sep = ',') 

collabs_only <- collabs_only |>
  left_join(artist_meta, by = c('id_collaborators' = 'artist_id')) 

collabs_only <- collabs_only |>
  rename('collaborator_genre' = 'main_genre') |>
  left_join(artist_meta, by = c('id_artists' = 'artist_id')) 

collabs_only <- collabs_only |>
  rename('artist_genre' = 'main_genre') |>
  filter(artist_genre != "-" & collaborator_genre != "-")

#aggregate the genres as before

collabs_only <- collabs_only |>
  mutate(artist_genre = str_to_lower(artist_genre)) |>
  mutate(
    genre_agg_artist = case_when(
      str_detect(artist_genre, str_c(hip_hop, collapse = "|")) ~ "Hip-Hop",
      str_detect(artist_genre, str_c(jazz_funk, collapse = "|")) ~ "Jazz/Funk",
      str_detect(artist_genre, str_c(soul_randb, collapse = "|")) ~ "Soul/R&B",
      str_detect(artist_genre, str_c(country, collapse = "|")) ~ "Country",
      str_detect(artist_genre, str_c(blues, collapse = "|")) ~ "Blues",
      str_detect(artist_genre, str_c(folk, collapse = "|")) ~ "Folk",
      str_detect(artist_genre, str_c(rock_metal, collapse = "|")) ~ "Rock/Metal",
      str_detect(artist_genre, str_c(pop, collapse = "|")) ~ "Pop",
      str_detect(artist_genre, str_c(electronic, collapse = "|")) ~ "Electronic",
      str_detect(artist_genre, str_c(adult_contemporary_classical, collapse = "|")) ~ "Adult Contemporary",
      TRUE ~ "Other")
  ) |>
  mutate(
      genre_agg_collaborator = case_when(
        str_detect(collaborator_genre, str_c(hip_hop, collapse = "|")) ~ "Hip-Hop",
        str_detect(collaborator_genre, str_c(jazz_funk, collapse = "|")) ~ "Jazz/Funk",
        str_detect(collaborator_genre, str_c(soul_randb, collapse = "|")) ~ "Soul/R&B",
        str_detect(collaborator_genre, str_c(country, collapse = "|")) ~ "Country",
        str_detect(collaborator_genre, str_c(blues, collapse = "|")) ~ "Blues",
        str_detect(collaborator_genre, str_c(folk, collapse = "|")) ~ "Folk",
        str_detect(collaborator_genre, str_c(rock_metal, collapse = "|")) ~ "Rock/Metal",
        str_detect(collaborator_genre, str_c(pop, collapse = "|")) ~ "Pop",
        str_detect(collaborator_genre, str_c(electronic, collapse = "|")) ~ "Electronic",
        str_detect(collaborator_genre, str_c(adult_contemporary_classical, collapse = "|")) ~ "Adult Contemporary",
        TRUE ~ "Other")
  )
  




#create the adjacency matrix showing summed instances of collaboration between each included genre

included_genres <- c('Pop','Soul/R&B','Hip-Hop','Rock/Metal','Country')

adj_matrix_long <- collabs_only |>
  filter(
    genre_agg_artist %in% included_genres & 
      genre_agg_collaborator %in% included_genres
  ) |>
  rowwise() |>
  mutate(pair = list(sort(c(genre_agg_artist, genre_agg_collaborator)))) |>
  unnest_wider(pair, names_sep = "_") |>
  group_by(pair_1, pair_2) |>
  summarise(
    total_songs = n(),
    top_hits = sum(top_hit, na.rm = TRUE),
    top_hit_proportion = top_hits / total_songs,
    .groups = "drop"
  )
  

# Prepare data for upset chart by converting each genre pair into a list column
upset_data <- adj_matrix_long |>
  rowwise() |>  # Ensure row-wise operations for list column creation
  mutate(genre_pair = list(c(pair_1, pair_2))) |>
  ungroup() |>  
  select(genre_pair, total_collabs = total_songs, hit_prop = top_hit_proportion  
  ) |>
  arrange(desc(total_collabs))
  


# Create the Upset plot
ggplot(upset_data, aes(x = genre_pair)) +
  geom_bar(aes(y = total_collabs, fill = 'Other charting songs'), stat = "identity") +
  geom_col(aes(y = total_collabs * hit_prop, fill = 'Top hits'), position = "identity") +
  scale_x_upset(order_by = 'degree') +
  theme_ipsum_rc() +
  labs(
    title = 'Frequency of collaborations on Billboard hot 100 songs',
    subtitle = '1960 to 2018, across the top 5 most popular genres',
    x = 'Genre pairs',
    y = "Number of collaborations",
    fill = "",
    Caption = 'Data from MusicOSet. Top hits includes any songs in the upper quartile of popularity score'
  ) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 11, family = 'Roboto Condensed')) +
  theme_combmatrix(combmatrix.label.text = element_text(family = 'Roboto Condensed', color = 'black', face = 'bold', size = 11)) +
  scale_fill_viridis_d()








