
## ---------------------------
## Purpose of script: R code for INF4000 data visualisations
## Author: Ned Blackburn
## Date Created: 2024-12-11

options(scipen = 6, digits = 5) 
library(tidyverse)
library(hrbrthemes)
library(ggbump)
library(ggupset)
library(patchwork)
library(season)


#0. load in data ----------------------------------

track_meta <- read.csv("Data/musicoset_metadata/tracks.csv", sep = "\t") |>
  select(!c('album_id', 'track_number'))

artist_meta <- read_delim("Data/musicoset_metadata/artists.csv",
                          delim = "\t",          # Main delimiter seems to be tabs based on a visual inspection
                          escape_double = TRUE,  # Handle stray quotes
                          col_names = TRUE,
                          trim_ws = TRUE) 

artist_meta <- artist_meta |>
  select(artist_id, main_genre)

#read in song popularity data
song_pop <- read_delim("Data/musicoset_popularity/song_pop.csv", delim = "\t") |>
  arrange(year) |>
  select(song_id, year_end_score) |>
  distinct(song_id, .keep_all = TRUE)

#read in data on musical fingerprints, drop unneeded columns
song_features <- read_delim("Data/musicoset_songfeatures/acoustic_features.csv", delim = "\t") |>
  select(-c(duration_ms,key,mode,time_signature,tempo)) |>
  drop_na()

#read in song metadata. This particular CSV isn't formatted properly so will require additional cleaning steps

song_meta <- read_delim(
  "Data/musicoset_metadata/songs.csv",
  delim = "\t",          # Main delimiter seems to be tabs based on a visual inspection
  escape_double = TRUE,  # Handle stray quotes
  col_names = TRUE,
  trim_ws = TRUE
)

# function to clean extraneous symbols and text from the csv using regex

clean_csv <- function(x) {
  if (is.character(x)) {
    x <- str_remove_all(x, '^[",]+|[",]+$')
  }
  return(x)
}

# apply cleaning function to the whole csv and then split the csv manually using known column names from the musicOset data schema

song_meta <- song_meta |> 
  mutate(across(everything(), clean_csv)) |>
  separate(col = names(song_meta)[1], 
           into = c("song_id", "song_name", "billboard", "artists", "popularity", "explicit", "song_type"), 
           sep = "\t", 
           fill = "right", 
           extra = "merge") |>
  select(!c("billboard"))

#split up 'artists' column so to isolate the artist_ids for matching purposes and to separate out multiple artists, in the case of collaborations, then clean up extraneous characters

song_meta <- song_meta |>
  separate(artists, into = c("artist1", "artist2"), sep = ", ", remove = FALSE) |>
  separate(artist1, into = c('artist1_id','artist1_name'), sep = ':', remove = FALSE) |>
  separate(artist2, into = c('artist2_id','artist2_name'), sep = ':', remove = FALSE)

song_meta$artist1_id <- gsub("[{}']", "", song_meta$artist1_id)
song_meta$artist1_name <- gsub("[{}']", "", song_meta$artist1_name)  
song_meta$artist1_name <- trimws(song_meta$artist1_name)  

song_meta$artist2_id <- gsub("[{}']", "", song_meta$artist2_id)
song_meta$artist2_name <- gsub("[{}']", "", song_meta$artist2_name)  
song_meta$artist2_name <- trimws(song_meta$artist2_name)  

#Join everything together to create the master dataset for hits

hits_master <- inner_join(song_meta, track_meta, by = 'song_id') |>
  inner_join(song_pop, by = 'song_id') |>
  inner_join(artist_meta, by = c('artist1_id' = 'artist_id')) |>
  inner_join(song_features, by = 'song_id') |>
  select(!c(artists,artist1,artist2))

#split out 'year' value from release date for consistency across releases in terms of granularity, and split out month for seasonal analysis, removing leading zeros
#filter for only songs released after 1962, when the dataset is supposed to begin

hits_master <- hits_master |>
  mutate(release_year = str_sub(release_date,1,4)) |>
  mutate(release_month = str_sub(release_date,6,7)) |>
  mutate(release_month = str_remove(release_month, "^0+")) |>
  filter(release_year > 1961) |>
  mutate(era = ifelse(release_year >= 1991,1,0))


#convert column types to more sensible ones

hits_master$popularity <- as.numeric(hits_master$popularity)
hits_master$release_year <- as.numeric(hits_master$release_year)
hits_master$release_date <- as.Date(hits_master$release_date)

#also load in musical fingerprint data for non-hits for the bump chart, convert variables to numeric
#drop rows without musical fingerprint data by converting them to NAs, and remove other impossible values (tempos of 0)

non_hits_subset <- colnames(non_hits)[16:23]

non_hits <- read_delim('Data/nonhits_dataset.csv', delim = '\t') |>
  filter(!if_any(all_of(non_hits_subset), ~ . == "-")) |>
  filter(tempo != 0) |>
  mutate(across(16:23, as.numeric)) |>
  drop_na() |>
  filter(!if_all(all_of(non_hits_subset), ~ . == 0))

#combine loudness column from both hits and non-hits so that they can be rescaled to between 0 and 1 consistently 
loudness_hits <- hits_master |>
  select(song_id,loudness) |>
  mutate(source = 'hits')

loudness_nonhits <- non_hits |>
  select(song_id,loudness) |>
  mutate(source = 'nonhits')

#rescale loudness values, then join back into individual hits/non-hits datasets
loudness_combined <- bind_rows(loudness_hits,loudness_nonhits) |>
  mutate(loudness_scaled = ((loudness - min(loudness)) / (max(loudness) - min(loudness)))) |>
  select(song_id, loudness_scaled )

hits_master <- hits_master |>
  left_join(loudness_combined, by = 'song_id')

non_hits <- non_hits |>
  left_join(loudness_combined, by = 'song_id')


# 0.5: EDA and additional data processing ---------------------------------------------------------------------
#explore top genres across the dataset (other than 'no genre' which is #1)

top_genres <- hits_master |>
  group_by(main_genre) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  slice(1:11)

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


# 1: bump plot comparing popularity of genres by decade --------

#create new dataframe showing average song popularity per genre per decade, and convert to a ranking variable for each genre
genres_over_time <- hits_master |>
  mutate(decade = floor(release_year / 10) * 10) |>
  #filter(decade != 2010) |>
  filter(genre_agg != 'Other') |>
  select(c('song_id','genre_agg','decade','year_end_score')) |>
  group_by(genre_agg, decade) |>                    
  summarise(popularity = sum(year_end_score, na.rm = TRUE), .groups = "drop") |>
  group_by(decade) |>                         
  mutate(ranking = dense_rank(desc(popularity))) |>
  ungroup()

#highlight the top 5 most popular genres for the graph
genre_focus <- c('Hip-Hop', 'Country', 'Rock/Metal', 'Pop', 'Soul/R&B')

bump_plot <- genres_over_time |>
  ggplot(aes(x = decade, y = ranking, group = genre_agg)) +
    geom_bump(linewidth = 0.9, smooth = 6, color = 'gray90') +
    geom_bump(aes(color = genre_agg), linewidth = 1.5,smooth = 6, 
              data = ~. |> filter(genre_agg %in% genre_focus)) +
    geom_point(color = "white", size = 4) +
    geom_point(color = "gray90", size = 2.5) +
    geom_point(aes(color = genre_agg), size = 2.5, 
               data = ~. |> filter(genre_agg %in% genre_focus)) +
    scale_y_reverse(breaks = c(11:1), 
                    expand = c(0.02,0)) +
    geom_text(aes(label = genre_agg), x = 2012, 
              hjust = 0,
              color = "gray30", 
              family = 'Roboto Condensed',
              size = 3.5,
              data = ~. |> slice_max(decade, by = genre_agg) |> 
                filter(!genre_agg %in% genre_focus)) +
    geom_text(aes(label = genre_agg), x = 2012, 
              hjust = 0,
              color = "black", 
              size = 3.5, 
              fontface = 'bold', 
              family = 'Roboto Condensed',
              data = ~. |> slice_max(decade, by = genre_agg) |> 
                filter(genre_agg %in% genre_focus)) +
    scale_x_continuous(
      limits = c(1959, 2021), 
      expand = c(0.01, 0),
      breaks = c(1960, 1970, 1980, 1990, 2000, 2010),
      labels = c("1960s", "1970s", "1980s", "1990s", "2000s","2010s")
    ) + 
    coord_cartesian(clip = "off") +
    labs(
      title = "How have the most popular genres changed over time?",
      subtitle = "The 1980s-1990s transition saw a dramatic shift in genre popularity with the emergence of hip hop and electronic music",
      x = '',
      y = "Popularity Ranking (1 = most popular)",
      color = "Genre",
      caption = "Rankings based on the number and popularity of charting songs in each genre in each decade"
      ) + 
    theme_ipsum_rc(grid = FALSE, ticks = TRUE) +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(),
          axis.text.x = element_text(margin = margin(t = 5), size = 10), 
          axis.text.y = element_text(margin = margin(r = -5), size = 10),
          axis.title.y = element_text(margin = margin(r = 15)),
          ) +
    scale_colour_viridis_d(option = 'H')

# 2: features of hits and non hits -----------------------------------
#create datasets with the average values of each musical characteristic for hits and non hits, then join and reshape

hits_properties_6089 <- hits_master |>
  filter(release_year >= 1960 & release_year <= 1989) |>
  mutate(across(c(energy, danceability, valence, acousticness, liveness, speechiness, loudness_scaled), as.numeric)) |>
  summarise(across(c(energy, danceability, valence, acousticness, liveness, speechiness, loudness_scaled), mean, na.rm = TRUE))

hits_properties_9020 <- hits_master |>
  filter(release_year >= 1990 & release_year <= 2020) |>
  mutate(across(c(energy, danceability, valence, acousticness, liveness, speechiness, loudness_scaled), as.numeric)) |>
  summarise(across(c(energy, danceability, valence, acousticness, liveness, speechiness, loudness_scaled), mean, na.rm = TRUE))


non_hits_properties <- non_hits |>
  summarise(across(c(energy, danceability, valence, acousticness, liveness, speechiness, loudness_scaled), mean, na.rm = TRUE))


#reshaping the dataframe for plotting, also calculating the differences between hits/non hits so we can order the graph better

hits_v_not <- bind_rows(hits_properties_6089,hits_properties_9020, non_hits_properties, .id = 'type') |>
  pivot_longer(cols = 2:8, names_to = "musical_feature", values_to = "average_value") |>
  pivot_wider(names_from = type ,values_from = average_value) |>
  rename('hits_6089' = 2, 'hits_9020' = 3, 'non_hits' = 4) |>
  mutate(diff = hits_9020 - hits_6089) |>
  mutate(diffbin = ifelse(hits_9020 - hits_6089 > 0 , 'A','B')) |>
  mutate(musical_feature = as.factor(musical_feature)) |>
  group_by(musical_feature)



#plot the final lollipop charts for hits from 1990-2020 and then from 1960-1980

hits_vs_not_plot <- hits_v_not |> 
  ggplot() +
  geom_segment(aes(x=reorder(musical_feature, diff), 
                   xend=reorder(musical_feature, diff), 
                   y=hits_9020, yend=hits_6089, color = diffbin), 
               linewidth = 9, alpha = 1) +
  geom_point(aes(x=reorder(musical_feature, diff), 
                 y=hits_9020, fill = 'Hits_9020'), 
             size=7 , shape = 23) +
  geom_point(aes(x=reorder(musical_feature, diff), 
                 y=hits_6089, fill = 'Hits_6089'), 
             size=7 , shape = 23) +
  geom_point(aes(x=reorder(reorder(musical_feature, diff), diff), 
                 y=non_hits, fill = 'Non-hits'), 
             size=4.5, shape = 21) +
  coord_flip() +
  labs(title = 'How do the musical features of charting songs compare to non-hits, and across eras?',
       subtitle = 'Post-1991 charting songs are louder, more energetic and more electronic',
       caption = 'Average score across all genres. Non-hits data taken from across both eras.',
       y = 'Index value (0 to 1, 1 = more of that feature)',
       x = ''
  ) +
  theme_ipsum_rc(grid = 'X,x', ticks = TRUE) +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  scale_fill_manual(values = c('Hits_9020' = '#fde725', 'Hits_6089' = '#440154', 'Non-hits' = 'white'),
                    labels = c('1961-1989 charting','1990-2019 charting','Non-charting')) +
  scale_color_manual(values = c('B' = '#ea9bfd','A' ='#ffff66'), guide = 'none')





# 3. collaborations upset chart --------------------------

#filter for collabs only
collabs_only <- hits_master |>
  select(c('artist1_id','artist2_id','artist1_name','artist2_name','popularity','era')) |>
  filter(artist2_id != is.na(artist2_id)) |>
  drop_na()

#get genres for artist and collaborator from metadata table and filter only where both genres can be identified

collabs_only <- collabs_only |>
  left_join(artist_meta, by = c('artist2_id' = 'artist_id')) 

collabs_only <- collabs_only |>
  rename('artist2_genre' = 'main_genre') |>
  left_join(artist_meta, by = c('artist1_id' = 'artist_id')) 

collabs_only <- collabs_only |>
  rename('artist1_genre' = 'main_genre') |>
  filter(artist1_genre != "-" & artist2_genre != "-")

#aggregate the genres as before

collabs_only <- collabs_only |>
  mutate(artist1_genre = str_to_lower(artist1_genre)) |>
  mutate(
    genre_agg_artist1 = case_when(
      str_detect(artist1_genre, str_c(hip_hop, collapse = "|")) ~ "Hip-Hop",
      str_detect(artist1_genre, str_c(jazz_funk, collapse = "|")) ~ "Jazz/Funk",
      str_detect(artist1_genre, str_c(soul_randb, collapse = "|")) ~ "Soul/R&B",
      str_detect(artist1_genre, str_c(country, collapse = "|")) ~ "Country",
      str_detect(artist1_genre, str_c(blues, collapse = "|")) ~ "Blues",
      str_detect(artist1_genre, str_c(folk, collapse = "|")) ~ "Folk",
      str_detect(artist1_genre, str_c(rock_metal, collapse = "|")) ~ "Rock/Metal",
      str_detect(artist1_genre, str_c(pop, collapse = "|")) ~ "Pop",
      str_detect(artist1_genre, str_c(electronic, collapse = "|")) ~ "Electronic",
      str_detect(artist1_genre, str_c(adult_contemporary_classical, collapse = "|")) ~ "Adult Contemporary",
      TRUE ~ "Other")
  ) |>
  mutate(
      genre_agg_artist2 = case_when(
        str_detect(artist2_genre, str_c(hip_hop, collapse = "|")) ~ "Hip-Hop",
        str_detect(artist2_genre, str_c(jazz_funk, collapse = "|")) ~ "Jazz/Funk",
        str_detect(artist2_genre, str_c(soul_randb, collapse = "|")) ~ "Soul/R&B",
        str_detect(artist2_genre, str_c(country, collapse = "|")) ~ "Country",
        str_detect(artist2_genre, str_c(blues, collapse = "|")) ~ "Blues",
        str_detect(artist2_genre, str_c(folk, collapse = "|")) ~ "Folk",
        str_detect(artist2_genre, str_c(rock_metal, collapse = "|")) ~ "Rock/Metal",
        str_detect(artist2_genre, str_c(pop, collapse = "|")) ~ "Pop",
        str_detect(artist2_genre, str_c(electronic, collapse = "|")) ~ "Electronic",
        str_detect(artist2_genre, str_c(adult_contemporary_classical, collapse = "|")) ~ "Adult Contemporary",
        TRUE ~ "Other")
  )
  

#create the adjacency matrix showing summed instances of collaboration between each included genre

included_genres <- c('Pop','Soul/R&B','Hip-Hop','Rock/Metal','Adult Contemporary')

adj_matrix_long <- collabs_only |>
  filter(
    genre_agg_artist1 %in% included_genres & 
      genre_agg_artist2 %in% included_genres
  ) |>
  rowwise() |>
  mutate(pair = list(sort(c(genre_agg_artist1, genre_agg_artist2)))) |>
  unnest_wider(pair, names_sep = "_") |>
  group_by(pair_1, pair_2) |>
  summarise(
    total_songs = n(),
    post91 = sum(era, na.rm = TRUE),
    post91_proportion = post91 / total_songs,
    .groups = "drop"
  )
  

# Prepare data for upset chart by converting each genre pair into a list column
upset_data <- adj_matrix_long |>
  rowwise() |>  # Ensure row-wise operations for list column creation
  mutate(genre_pair = list(c(pair_1, pair_2))) |>
  ungroup() |>  
  select(genre_pair, total_collabs = total_songs, post91_proportion) |>
  arrange(desc(total_collabs))
  

# Create the Upset plot
upset_plot <- ggplot(upset_data, aes(x = genre_pair)) +
  geom_col(aes(y = total_collabs, fill = '1991-2018'), position = "identity") +
  geom_col(aes(y = total_collabs - (total_collabs * post91_proportion), fill = '1964-1990'), position = "dodge2") +
  scale_x_upset(order_by = 'degree') +
  theme_ipsum_rc(grid = 'Yy', axis_title_size = 11) +
  scale_y_continuous(breaks = seq(0,150,25)) +
  labs(
    title = 'Which genres collaborate the most, and in which era?',
    subtitle = 'The number of charting collaborations increased markedly post-1991, particularly across Pop and Hip-Hop',
    x = 'Collaborations (including top 5 genres with most collaborations)',
    y = "Number of charting collaborations",
    fill = "Release Date",
    Caption = 'Data from MusicOSet. Top hits includes any songs in the upper quartile of popularity score'
  ) +
  theme(
    legend.position = c(0.92, 0.89), # Position legend at top-right
    legend.box.background = element_rect(color = "black", fill = "white"), # White box with black outline
    legend.box.margin = margin(5, 5, 5, 5), # Add some padding to the box
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(size = 10) # Adjust text size
  )  +
  theme_combmatrix(combmatrix.label.text = element_text(family = 'Roboto Condensed', color = 'black', face = 'bold', size = 11),
                   combmatrix.panel.margin = unit(c(-10, 0), "pt"),
                   combmatrix.label.total_extra_spacing = unit(5, "pt")) +
  scale_fill_viridis_d(direction = -1)


# 4. chart comparing seasonality effects on release volumes and popularity across eras --------

#join month-precise release date info to hits dataset on song_id, dropping any songs that don't have a match and dropping unneeded columns
#I've also made the (opinionated) choice to remove all songs released on the first of january - this is clearly a default date given when the real release date isn't known

hits_by_month <- hits_master |>
  filter(release_month != '') |>
  filter(!str_detect(release_date, "-01-01")) |>
  select(song_id,year_end_score,release_month,era) |>
  group_by(era, release_month) |>
  summarise(total_releases = n(), pop = mean(year_end_score)) |>
  ungroup() |>
  group_by(era) |>
  mutate(releases_prop = total_releases/sum(total_releases)) |>
  mutate(release_month = month.abb)



#create the final tile and line plots

#tile density plot 

seasonal_release_heatmap <-  hits_by_month |>
  ggplot(aes(era, release_month, fill = releases_prop)) + 
  geom_tile(colour="white",size = 1, stat="identity") + 
  scale_fill_viridis(option="G") +
  scale_x_continuous(breaks = c(0,1), labels = c('1960-1990','1991-2019'))+
  xlab("") + 
  ylab("") +
  theme(
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text = element_text(size=12, family = 'Roboto Condensed'),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.text = element_text(color="black", size= 10),
    legend.background = element_rect(fill="white"),
    legend.position = "none",
    legend.title=element_blank(),
    axis.text.y = element_text(margin = margin(r = 10), hjust = 1),
    plot.margin = margin(5, 5, 5, 5)) + 
  coord_flip()


# line plot showing average popularity by month

seasonal_release_popularity <- hits_by_month |>
  ggplot(aes(x = release_month, y = pop, group = era)) +
  geom_line(aes(color = era)) +
  geom_point(aes(color = era),size = 3) +
  theme_ipsum_rc(grid = 'XY') +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.y = element_text(hjust = 1)) +
  labs(title = 'Average popularity and number of releases, by month',
       subtitle = 'Most songs are released in July, but the most popular are released in April') +
  scale_colour_viridis_c()

#using patchwork to stitch the plots together. I ended up doing this manually for the final report as I couldn't get the axes aligned using patchwork,
#but leaving the code in in case whoever's marking needs to verify what it looks like






