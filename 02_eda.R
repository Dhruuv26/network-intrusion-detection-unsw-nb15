library(tidyverse)
theme_set(theme_minimal(base_size = 12))

combined <- read_csv("C:/Users/Dhruuv/Downloads/unsw_combined_raw.csv", show_col_types = FALSE)

combined %>%
  count(attack_cat) %>%
  ggplot(aes(x = reorder(attack_cat, n), y = n, fill = attack_cat)) +
  geom_col() +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Class Distribution by Attack Category (log scale)",
       x = "Attack Category", y = "Count (log10)") +
  theme(legend.position = "none")

combined %>%
  filter(dur > 0) %>%
  ggplot(aes(x = attack_cat, y = dur, fill = attack_cat)) +
  geom_boxplot() +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Connection Duration by Attack Category (log scale)",
       x = "Attack Category", y = "Duration (seconds, log10)") +
  theme(legend.position = "none")

ct_cols <- combined %>%
  select(starts_with("ct_")) %>%
  select(-ct_ftp_cmd)  

cor_matrix <- cor(ct_cols, use = "complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix))

ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "darkred",
                        midpoint = 0, limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Among Connection-History (ct_*) Features",
       x = NULL, y = NULL, fill = "Correlation")

combined %>%
  group_by(attack_cat) %>%
  summarise(avg_sbytes = mean(sbytes, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(attack_cat, avg_sbytes), y = avg_sbytes, fill = attack_cat)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Source Bytes by Attack Category",
       x = "Attack Category", y = "Average Source Bytes") +
  theme(legend.position = "none")

top_states <- combined %>% count(state, sort = TRUE) %>% slice_head(n = 5) %>% pull(state)

combined %>%
  filter(state %in% top_states) %>%
  count(attack_cat, state) %>%
  ggplot(aes(x = attack_cat, y = n, fill = state)) +
  geom_col(position = "fill") +
  coord_flip() +
  labs(title = "Connection State Composition by Attack Category",
       x = "Attack Category", y = "Proportion", fill = "State")