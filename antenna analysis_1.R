# set working directory
working_dir <- "C:/temp/antenna_model"
setwd(working_dir)

# load libraries
library(tidyverse)
library(devtools)
library(gghalves)
devtools::install_github('smin95/smplot2')

library(smplot2)

# size of slides mm
slide_len <- 0.2976

# clean data
{# import and format data
source_main <- "joined_data.csv"
source_dex <- "cockroach_dex_data.csv"
source_scale <- "cockroach_scale_data.csv"
source_diam <- "cockroach_diam_data.csv"

dat_raw_main <- read_csv(file = source_main)
dat_raw_dex <- read_csv(file = source_dex) %>% 
    rename(name = file, 
           slide_pix = slide_pixels,
           init_pix_in = antenna_pixels_inner, 
           init_pix_out = antenna_pixels_outer,
           mid_pix_in = min_diam)
dat_raw_diam <- read_csv(file = source_diam)
dat_raw_scale <- read_csv(file = source_scale) %>%
  rename(name = file)




# main data
  {
    # set up exploratory data
    dat_dex <- dat_raw_dex %>% 
      mutate(init_diam_in = slide_len / slide_pix * init_pix_in,
             init_diam_out = slide_len / slide_pix * init_pix_out,
             mid_diam_in = slide_len / slide_pix * mid_pix_in,
             init_prop = abs((init_diam_in/init_diam_out)-1),
             max_prop = mid_diam_in/init_diam_out)
    # select scale data
    dat_merge_scale <- dat_raw_scale %>%
      select(name,ref_len,ref_pix,antenna_pix,mark_pix)
    # long diam data
    dat_longd <- dat_raw_diam %>% 
      pivot_longer(-frame, values_to = "diam") %>% 
      na.omit()
    # convert wide to long format
    dat_long <- dat_raw_main %>% # main
      pivot_longer(-frame, values_to = "angle") %>% 
      na.omit() %>% 
      left_join(dat_longd, by = c("name","frame")) %>% # diam
      left_join(dat_dex,by = c("name")) %>% # dex
      left_join(dat_merge_scale,by = c("name")) # scale
    
    dat_long2 <- dat_long %>% drop_na(diam)
    # create categories from data
    sep_id <- c("roach_id", "roach_antenna","side")
    
    dat_sep_1 <- dat_long %>% 
      separate(name, c("name", "distance"), "_") 
    dat_sep <- dat_sep_1 %>% 
      extract(name, sep_id,
              regex = "([0-9]+)([[:alnum:]])([[:alnum:]])") 
    
    # check for correct grouping, add total frames
    check_dat <- dat_sep %>% 
      group_by(roach_id,roach_antenna,side,distance) %>% 
      summarise(frame_total = n(), .groups = 'drop')
    
    exp_id <- c(sep_id,"distance")
    dat_set <- dat_sep %>%
      full_join(check_dat, by = exp_id)
  }
  
# exploratory diam data
  {
    # calculations with antenna diameter
    
    plot (dat_dex)
    
    dat_cep_1 <- dat_dex %>% 
      separate(name, c("name", "distance"), "_") 
    dat_cep <- dat_cep_1 %>% 
      extract(name, sep_id,
              regex = "([0-9]+)([[:alnum:]])([[:alnum:]])") 
  }

  
# diam data
  {
    dat_diam <- dat_raw_diam %>% 
      pivot_longer(-frame, values_to = "diam") %>% 
      na.omit() 
    
    dat_dep_1 <- dat_diam %>% 
      separate(name, c("name", "distance"), "_") 
    dat_dep <- dat_dep_1 %>% 
      extract(name, sep_id,
              regex = "([0-9]+)([[:alnum:]])([[:alnum:]])") 
  } 

# scale data
  {
    dat_scale <- dat_raw_scale
    
    dat_xep_1 <- dat_diam %>% 
      separate(name, c("name", "distance"), "_") 
    dat_xep <- dat_xep_1 %>% 
      extract(name, sep_id,
              regex = "([0-9]+)([[:alnum:]])([[:alnum:]])") 
  } 

# exp_id <- c(sep_id,"distance")
# 
# dat_setl <- dat_sep %>% 
#   full_join(check_dat, by = exp_id)
# 
# dat_setm <- dat_sep %>% 
#   full_join(dat_xep, by = exp_id)
# 
# dat_setn <- dat_setm %>% 
#   full_join(dat_cep, by = exp_id)
# 
# dat_set <- dat_setn %>% 
#   full_join(dat_dep, by = exp_id)
}

# create weight vector and setup
  {
    # convert frames to weight amount
    order <- function(frame_total,frame){
      total_weights <- floor(frame_total/2)
      num_weights <- total_weights - abs(total_weights-frame)
      return(num_weights)
    }
    
    num_w <- c(0:20)
    weight_mass <- c(4.5368,4.5042,4.7763,18.4192,18.8666,17.8947,18.2045)
    weight <- numeric(21)
    for(i in c(1:20)){
      if(floor(num_w[i]/4) > 0){weight[i] <- sum(weight_mass[4:(3 + floor(num_w[i]/4))])}  
      
      if(num_w[i] %% 4 > 0){weight[i] <- weight[i] + sum(weight_mass[1:(num_w[i] %% 4)])}
    }
    weight[21] <- weight[20] + 4.5
  }

# for(i in 1:length(num_w)){n[i] <- toString(num_w[i])}
# for(i in 1:length(weight)){w[i] <- toString(weight[i])}

# arrange data
{
  # classify ascending rows and add weight numbers to data
  dat_result <- dat_set %>% 
    group_by(roach_id,roach_antenna,side,distance) %>% 
    mutate(num_weights = order(frame_total,frame), 
           ascending = (frame <= floor(frame_total/2)))
  #  mutate(weight = str_replace(num_weights,corr_tab(n,w)))
  
  # convert weight number to weight
  size <- length(dat_set$frame)
  x <- numeric(size)
  index <- dat_result$num_weights[1:size]
  x <- weight[index+1]
  
  
  dat_norm <- dat_result %>% ungroup() %>%
    mutate(weight = as.vector(x)) %>% 
    group_by(roach_id,  roach_antenna, side, distance) %>%
    mutate(angle = angle - angle[1], 
           strain = (diam[1] - diam) / diam[1])
  
  test <- dat_norm %>% filter(strain < 0)
}

# shortcuts
sca_col <- scale_color_discrete(labels = c('proximal', 'medium', 'distal'))
sca_side <- scale_x_discrete(labels = c('dorsal', 'ventral'))

labw <- xlab('weight (g)')

theme_set(theme_light())
update_geom_defaults('line', list(linewidth = .7))
update_geom_defaults('point', list(size = 2.3))
update_geom_defaults('boxplot', list(linewidth = .5))

lis1 <- c('dorsal','ventral')
lis2 <- 'proximal'
names(lis1) <-c('d','v')
names(lis2) <- '1'

facs <- labeller(side = lis1)
  
  # plot basic angle vs weight
{
  norm_1 <- dat_norm %>% filter(frame <= floor(frame_total/2)) %>% # ascending
    ggplot(aes(weight,angle, color = distance)) +
    geom_point() +
    geom_line() +
    facet_grid(roach_id~side) +
    sca_col +
    ylab('angle') +
    ggtitle("angle from increasing weight") + 
    sca_side
  
  norm_ex <- dat_norm %>% filter(frame <= floor(frame_total/2), distance == "1") %>% # ascending
    ggplot(aes(weight,angle, color = max_prop, fill = roach_id)) +
    geom_point() +
    geom_line() +
    facet_grid(side~distance, labeller = labeller(side = lis1, distance = lis2)) +
    xlab("weight added (g)") +
    ylab("angle") +
    ggtitle("Deflection of antenna based\n on maximum compression") +
    labs(color = c('normalized\n size of\n compressed\n antenna')) +
    guides(fill = 'none')
  
  
  
  norm_2 <- dat_norm %>% filter(frame > floor(frame_total/2)) %>% # descending
    ggplot(aes(-weight,angle, color = distance)) +
    geom_point() +
    geom_line() +
    facet_grid(roach_id~side) +
    sca_col +
    guides(fill = 'none') +
    ylab('angle') +
    ggtitle("angle from decreasing weight") + 
    sca_side
}

# hysteresis graph
{
  # link data for hysteresis
  hy_comp <- dat_norm %>% filter(frame == floor(frame_total/2))
  
  dat_hy <- dat_norm %>% 
    mutate(ascending = 
             ifelse(frame == floor(frame_total/2), FALSE, ascending)) %>%
    bind_rows(hy_comp)
  
  # 4x7
  norm_hy <- dat_hy %>% group_by(ascending) %>%
    filter(roach_id == 10) %>%
    ggplot(aes(weight,angle, color = distance, shape = ascending)) +
    geom_point() +
    geom_line() +
    facet_grid(roach_id~side, labeller = labeller(side = lis1)) +
    sca_col +
    ylab('angle') +
    ggtitle("angle from weight") +
    theme(legend.position = 'bottom')
}

norm_hy
facs
norm_ex
norm_1
norm_2 


dat_norm %>% group_by(ascending) %>%
  filter(roach_id == 10) %>%
  ggplot(aes(weight,angle, color = distance, shape = ascending)) +
  geom_point() +
  geom_line() +
  facet_grid(roach_id~side, labeller = labeller(side = lis1)) +
  sca_col +
  ylab('angle') +
  ggtitle("angle from weight")


# fail graph
{# for(i in 1:20){
#   y1 <- min(dat_norm %>% mean(angle[dat_norm$weight==weight[i]] & dat_norm$roach_id ==))
# }
# 
# norm_b <- dat_norm %>%
#   ggplot(aes(weight,y1 = min(angle[dat_norm$weight==weight]) ,y2 = max(angle[dat_norm$weight==weight]), color = distance, shape = ascending, fill = 'red')) +
#   geom_line(aes(y=y1)) +
#   geom_line(aes(y=y2)) +
#   facet_grid(roach_id~side) +
#   sca_col
}

# AUC calculations
{
  library(zoo)

  tes <- dat_norm %>% unite('roach_id',  'roach_id', 'side', 'distance')
  AUC1 <- numeric(length(unique(tes$roach_id)))
  for(i in 1:length(unique(tes$roach_id))){
    v <- unique(tes$roach_id)[i]
    exp <- tes %>% 
      filter(roach_id == v) %>% 
      filter((frame <= floor(frame_total/2)))
    x <- exp$frame
    y <- exp$angle
    AUC1[i] <- sum(rollmean(y,2))
  }
  AUC1
  
  AUC2 <- numeric(length(unique(tes$roach_id)))
  for(i in 1:length(unique(tes$roach_id))){
    v <- unique(tes$roach_id)[i]
    exp <- tes %>% 
      filter(roach_id == v) %>% 
      filter((frame >= floor(frame_total/2)))
    x <- exp$frame
    y <- exp$angle
    AUC2[i] <- sum(rollmean(y,2))
  }
  AUC2
  
  AUC <- (AUC2-AUC1)*4.6
  AUC
}

# fail AUC
{# dat_no <- dat_norm %>% ungroup() %>% tibble(AUC)

# area_1_d <- dat_norm[dat_norm$side == 'd',] %>% filter((frame <= floor(frame_total/2))) %>% sm_auc_all(subjects = 'roach_id', conditions = c('distance'), x = 'frame', values = 'angle')
# area_2_d <- dat_norm[dat_norm$side == 'd',] %>% filter((frame >= floor(frame_total/2))) %>% sm_auc_all(subjects = 'roach_id', groups = c('distance'), x = 'frame', values = 'angle')
# area_1_v <- dat_norm[dat_norm$side == 'v',] %>% filter((frame <= floor(frame_total/2))) %>% sm_auc_all(subjects = 'roach_id', groups = c('distance'), x = 'frame', values = 'angle')
# area_2_v <- dat_norm[dat_norm$side == 'v',] %>% filter((frame >= floor(frame_total/2))) %>% sm_auc_all(subjects = 'roach_id', groups = c('distance'), x = 'frame', values = 'angle')

# hister_d <- full_join(area_1_d,area_2_d, by = c('roach_id','distance')) %>% mutate(AUC_diff = AUC_angle.y-AUC_value.x)
# hister_v <- full_join(area_1_v,area_2_v, by = c('roach_id','distance')) %>% mutate(AUC_diff = AUC_value.y-AUC_value.x)
# 
# select_groups <- function(data,groups, ...){
#   data[sort(unlist(attr(data,"indices")[ groups ])) + 1, ]
# }
# 
# tes <- dat_norm %>% unite("roach_id", c(roach_id,side)) %>% filter((frame <= floor(frame_total/2)))
# tes %>% ungroup() %>% sm_auc_all(subjects = 'roach_id', conditions = 'distance', x = 'frame', values = 'angle')
  }

# summary
{
  res1 <- dat_norm %>% 
    group_by(roach_id,  roach_antenna, side, distance) %>% 
    summarize(angle_max = max(angle), weight_max = max(weight))
  
  res2 <- dat_norm %>% filter(frame > floor(frame_total/2)) %>% 
    group_by(roach_id,  roach_antenna, side, distance) %>% 
    summarize(angle_min = min(angle))
  
  res <- full_join(res1,res2,by = 
                     c('roach_id',  'roach_antenna', 'side', 'distance')) %>%
    mutate(return_rate = 1-angle_min/angle_max)
  
  res_d <- res %>% ungroup() %>% 
    group_by(distance) %>% 
    summarize(return_rate = mean(return_rate))
  
  res_w <- res %>% ungroup() %>% 
    group_by(weight_max, distance) %>% 
    summarize(return_rate = mean(return_rate)) %>% 
    arrange(desc(-weight_max))
}

# grouped data
{
  res_w %>% ggplot(aes(weight_max,return_rate)) +
  geom_point() + theme_bw() 

  res_w_1 <- res %>% ungroup() %>% 
    group_by(weight_max) %>% 
    mutate(return_rate = mean(return_rate)) %>% 
    arrange(desc(-weight_max))
  
  res_w_2 <- res %>% ungroup() %>% 
    group_by(weight_max, side) %>% 
    mutate(return_rate = mean(return_rate)) %>% 
    arrange(desc(-weight_max))
  
  h_w <- res_w %>% summarize(return_rate = mean(return_rate), distance = distance)
  
  rbplot <- res %>% filter(return_rate < 1)
  
  res_a <- res %>% ungroup() %>% 
    group_by(angle_max) %>% 
    mutate(return_rate = mean(return_rate)) %>% 
    arrange(desc(-angle_max))
  
  plot(res_a)
}



# plot facet
{
  ph_1 <- rbplot %>% ggplot(aes(side,return_rate)) +
    geom_boxplot() + theme_light() + 
    sca_side +
    ylab("average angle change\n upon descent vs ascent")
  ph_2 <- res %>% ggplot(aes(side,weight_max)) +
    geom_boxplot() + theme_light() + 
    sca_side +
    ylab("maximum weight added (g)")
  ph_1
  ph_2
  
  ph_1c <- rbplot %>% filter(return_rate < 1) %>% ggplot(aes(side,return_rate,color = distance)) +
    geom_boxplot() + theme_light() + 
    sca_side +
    ylab("average angle change\n upon descent vs ascent")
  ph_2c <- rbplot %>% ggplot(aes(side,weight_max,color = distance)) +
    geom_boxplot() + theme_light() + 
    sca_side +
    ylab("maximum weight added (g)")
}
ph_1c
ph_2c

# plot facet strain

# plot facet with color
{
  p_res_w_1 <- rbplot %>% ggplot(aes(weight_max,return_rate, color = distance, shape = side)) +
    geom_point() + theme_light() +
    xlab("maximum weight added (g)") +
    ylab("average angle change\n upon descent vs ascent") +
    ggtitle("Weight added against average angle change") +
    labs(color = c('distance')) +
    sca_col +
    scale_shape(labels = c('dorsal', 'ventral'))
  
  p_res_w_2 <- rbplot %>% ggplot(aes(weight_max,angle_max, color = distance, shape = side)) +
    geom_point() + theme_light() +
    xlab("maximum weight added (g)") +
    ylab("maximum measured angle") +
    ggtitle("Weight added against maximum angle") +
    labs(color = c('distance')) +
    sca_col +
    scale_shape(labels = c('dorsal', 'ventral'))
    
    pres_a_1 <- res_a %>% ggplot(aes(angle_max,return_rate, color = res_a$distance, shape = side)) +
      geom_point() + theme_light() +
      xlab("maximum angle") +
      ylab("average angle change\n upon descent vs ascent") +
      ggtitle("Maximum angle and angle change") +
      labs(color = c('distance')) +
      sca_col +
      scale_shape(labels = c('dorsal', 'ventral'))
}
p_res_w_1+facet_grid(distance~'')
p_res_w_2+facet_grid(distance~'')
pres_a_1+facet_grid(distance~'')

# plot facet strain with color



tibble(num_w,weight)

hister <- tibble(res,AUC)

# AUC plot
{phis_1 <- hister %>% ggplot(aes(return_rate,AUC, color = distance, shape = side)) +
  geom_point() + theme_light() +
  xlab("return rate") +
  ggtitle("Return rate and AUC") +
  labs(color = c('distance')) +
  sca_col +
  scale_shape(labels = c('dorsal', 'ventral'))

phis_1d <- hister %>% ggplot(aes(return_rate,AUC, color = distance, shape = side)) +
  geom_point() + theme_light() +
  facet_grid(distance~'')

phis_2 <- hister %>% ggplot(aes(weight_max,AUC, color = distance, shape = side)) +
  geom_point() + theme_light() +
  xlab("maximum weight") +
  ggtitle("Maximum weight and AUC") +
  labs(color = c('distance')) +
  sca_col +
  scale_shape(labels = c('dorsal', 'ventral'))

phis_3 <- hister %>% ggplot(aes(angle_max,AUC, color = distance, shape = side)) +
  geom_point() + theme_light() +
  xlab("maximum angle") +
  ggtitle("Maximum angle and AUC") +
  labs(color = c('distance')) +
  sca_col +
  scale_shape(labels = c('dorsal', 'ventral'))

phis_2d <- hister %>% ggplot(aes(weight_max,AUC, color = distance, shape = side)) +
  geom_point() + theme_light() +
  facet_grid(distance~'')

phis_3d <-hister %>% ggplot(aes(angle_max,AUC, color = distance, shape = side)) +
  geom_point() + theme_light() +
  facet_grid(distance~'')

phis_4 <- hister %>% ggplot(aes(side,AUC, color = distance, shape = side)) +
  geom_boxplot() + theme_light() +
  ggtitle("AUC vs side of applied pressure") +
  labs(color = c('side')) +
  sca_col +
  scale_shape(labels = c('dorsal', 'ventral'))}

phis_1 + facet_grid(distance~'')
phis_2 + facet_grid(distance~'')
phis_3 + facet_grid(distance~'')
phis_4 

phis_2 <- hister %>% ggplot(aes(side,return_rate, color = distance, shape = side)) + geom_boxplot() + theme_light() 
phis_2
