source("antenna analysis_1.R")

dat_norm %>% filter(frame <= floor(frame_total/2) & roach_id == 10) %>% # ascending
  ggplot(aes(strain,angle, color = distance)) +
  geom_point() +
  facet_grid(''~side, labeller = facs) +
  sca_col +
  theme_light()

dat_hy %>% group_by(ascending) %>%
  filter(roach_id == 10) %>%
  ggplot(aes(weight,angle, color = distance, shape = ascending)) +
  geom_point() +
  geom_line() +
  facet_grid(''~side, labeller = labeller(side = lis1)) +
  sca_col +
  labw +
  theme(legend.position = 'bottom') + 
  guides(shape = 'none') +
  sca_col

dat_norm %>% filter(frame <= floor(frame_total/2), distance == "1") %>% # ascending
  ggplot(aes(weight,angle, color = max_prop, fill = roach_id)) +
  geom_point() +
  geom_line() +
  facet_grid(side~distance, labeller = labeller(side = lis1, distance = lis2)) +
  xlab("weight added (g)") +
  ylab("angle") +
  ggtitle("Deflection of antenna based\n on maximum compression") +
  labs(color = c('normalized\n size of\n compressed\n antenna')) +
  guides(fill = 'none')
