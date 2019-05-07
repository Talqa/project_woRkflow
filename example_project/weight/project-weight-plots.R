library('tidyverse')

#get data-----------------------------------------------------------------------
data <- read_csv('example_project/weight/cleandata/project-weight-cleandata.csv')
# View(data)

#HELPER FUNCTIONS-----
data_summary_SEM <- function(x) {
  #calculates mean and SEM used in plots
  m <- mean(x)
  sem <- sd(x)/sqrt(length(x))
  ymin <- m - sem
  ymax <- m + sem
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#END helper functions---

#PLOTS--------------------------------------------------------------------------

# PLOT 1------------------------------------------------------------
#weight at first measurement
ggplot(data, aes(x = treatment, 
                 y = `2019-03-04`, 
                 shape = treatment, 
                 color = treatment)) + 
  # add boxplot
  geom_boxplot() +
  # add individual points
  geom_jitter(position = position_jitter(0.2), cex = 3) +
  # add point for mean and SE lines
  stat_summary(fun.data = data_summary_SEM, geom = "pointrange", color = "black", size = 0.5) +
  # customize colors, axes, shapes
  scale_color_manual(values = c('orange', 'blue'), 
                     breaks = c('A', 'B')) +
  scale_x_discrete(limits = c('A', 'B')) +  
  scale_shape_manual(values = c(16, 17),
                     breaks = c('A', 'B')) +
  # customize theme
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(face = "italic", hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank()) +
  # add labels
  labs(title = 'Weight of parts at first measurement', 
       subtitle = 'Sample size: A = 100, B = 97', 
       y = 'Weight [g]',
       caption = 'boxplot: box = IQR25-75%, line = median, whisker = 1.5*IQR, small dot = outlier; 
       black line with point: mean +/- SE')

# PLOT 2-------------------------------------------------
#weight over time 

#change data to long format
weight <- data %>%
  gather(week, weight, starts_with("2019")) %>%
  # remove NAs
  filter(weight != is.na(weight))

# View(weight)

# summarise data before plotting
weight_summary <- weight %>% 
  group_by(week, treatment) %>%
  summarise(avg_w = mean(weight),
            sd_w = sd(weight),
            se_w = sd(weight)/sqrt(length(weight))) 

# plot data
weight_plot <- ggplot(weight_summary, aes(x = week, 
                       y = avg_w, 
                       colour = treatment,
                       group = treatment)) +
  # add lineplot
  geom_line() +
  # add dots on the line
  geom_point() +
  # add error bars
  geom_errorbar(aes(ymin = avg_w - se_w,
                    ymax = avg_w + se_w), width = 0.25) +
  # customize color, shape, axes
  scale_color_manual(values = c('orange', 'blue'), 
                     breaks = c('A', 'B')) +
  scale_shape_manual(values = c(16, 17),
                     breaks = c('A', 'B')) +
  scale_x_discrete(breaks = c("2019-03-04", "2019-03-11",
                              "2019-03-18", "2019-03-25", "2019-04-01", "2019-04-08",
                              "2019-04-15", "2019-04-22", "2019-04-29"),
                   labels = c(1,2,3,4,5,6,8,9,11)) +
  # add vertical line to separate months
  geom_vline(xintercept = 5, linetype = 'dashed') +
  # add text to mark months
  annotate('text', x = 4, y = 34, label = 'March') +
  annotate('text', x = 6, y = 34, label = 'April') +
  # customize theme
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 18), 
        plot.subtitle = element_text(face = "italic", hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18)) +
  # add labels
  labs(title = 'Weight of parts',
       subtitle = 'Sample size: A = 100, B = 97', 
       caption = 'mean +/- SE',
       y = 'average weight [g]',
       x = 'week')

weight_plot

#save plot
ggsave('example_project/weight/plots/project-weight.tiff', width = 12, height = 10.5, weight_plot)

