library(tidyverse)


################################################################################
# local functions
################################################################################
# function to check if two confidence intervals overlap
doOverlap <- function(ci1, ci2) {
  return(ci1[2] >= ci2[1] & ci1[1] <= ci2[2])
}
# funciton to find overlapping videos based on means and 95% CI
check_overlapping_videos = function(subset){
  selected_videos <- numeric(0)
  
  for (i in 1:nrow(subset)) {
    current_ci <- c(subset$lower_ci[i], subset$upper_ci[i])
    
    # Check if the current video overlaps with all other videos
    all_overlap <- all(sapply(1:nrow(subset), function(j) {
      if (i != j) {
        other_ci <- c(subset$lower_ci[j], subset$upper_ci[j])
        return(doOverlap(current_ci, other_ci))
      }
      return(TRUE)  # Ignore self-comparison
    }))
    
    # If the current video overlaps with all others, add it to the selected videos
    if (all_overlap) {
      selected_videos <- c(selected_videos, subset$vid[i])
    }
  }
  return(selected_videos)
}


################################################################################
# data
################################################################################
video_ratings <- read_csv("data/video-ratings/video-ratings.csv")
video_ids <- read_csv("data/video-ratings/video_ids.csv")
vector_video_ids = as_vector(video_ids$video_label)

video_ratings_select = video_ratings %>% 
  select(pid, contains(vector_video_ids)) 

# remove wierd numbers
colnames(video_ratings_select) <- str_remove(colnames(video_ratings_select), "\\.{3}\\d+$")


video_ratings_select_long = video_ratings_select %>% 
  pivot_longer(cols = U_cooking_f_s_IV_preference:L_music_s_m_s_III_80_familiarity, 
               names_to = "videos", values_to = "ratings")  %>% 
  drop_na(ratings) %>% 
  left_join(video_ids, by = c("videos" = "video_label")) %>% 
  rename(old_label = videos) %>% 
  relocate(new_label, .before = ratings) %>% 
  mutate(vid = paste0("vid", video_id)) %>% 
  group_by(pid) %>% 
  mutate(z_rating = scale(ratings)) %>% 
  mutate(conditions = paste0(nationality,"-",condition))


table(video_ratings_select_long$rating)


# select rating scale
rating_scale = "preference" # preference or valence

video_ratings_sum = video_ratings_select_long %>% 
  filter(rating == rating_scale) %>% 
  group_by(vid, nationality, condition, composition, conditions, new_label) %>% 
  summarise(
    n = n(),
    mean = mean(ratings),
    sd = sd(ratings),
    lower_ci = mean - qnorm(0.975) * (sd / sqrt(n)),
    upper_ci = mean + qnorm(0.975) * (sd / sqrt(n))
  ) %>% 
  arrange(desc(mean))

length(table(video_ratings_sum$vid)) # 80

video_ratings_sum %>% 
  ggplot(aes(x = reorder(vid, -mean), y = mean, fill = conditions)) +
  stat_summary(fun.data = mean_cl_normal, geom = "bar", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.25, position = position_dodge(width = 0.8)) +
  labs(
       x = "Video ID",
       y = paste(rating_scale, "rating")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

as_vector(video_ratings_sum$vid)

################################################################################
# Analysis: finding similar videos to select for the experiment
################################################################################
# Based on the plot, try to find videos in each condition that are the same
manually_selected_videos = c(
  "vid35", "vid61","vid37","vid58","vid28",
  "vid26","vid33","vid62","vid55","vid56",
  "vid19","vid50","vid64","vid15",
  "vid44","vid39","vid2","vid57","vid54",
  "vid63","vid78","vid40","vid59","vid31",
  "vid49","vid1","vid24","vid60","vid67",
  "vid14","vid41","vid21","vid73","vid74",
  "vid45","vid11","vid65","vid5","vid46",
  "vid34","vid51","vid69","vid8","vid29",
  "vid80","vid10", "vid48", 
  "vid66","vid3")
length(manually_selected_videos)

video_selection = video_ratings_sum %>% 
  filter(vid %in% manually_selected_videos)

out = check_overlapping_videos(video_selection)
length(out)
out

video_ratings_sum$selected <- video_ratings_sum$vid %in% out
video_ratings_sum %>% write.csv("video_preference_ids_selected.csv")

video_ratings_sum %>%
  ggplot(aes(x = reorder(vid, -mean), y = mean, fill = conditions, color = factor(selected))) +
  stat_summary(fun.data = mean_cl_normal, geom = "bar", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.25, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = FALSE) +  # Customize colors
  labs(
    x = "Video ID",
    y = "Valence Rating"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave("plot_preference_video_selection.png", height = 8, width = 20, units = "cm",
       bg = "white")


video_ratings_sum %>% 
  filter(selected == TRUE) %>% 
  group_by(conditions) %>% 
  summarise(n=n()) 
