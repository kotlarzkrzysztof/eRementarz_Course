library(tidyverse)

dane <- read.csv("ncn_grants.csv")
dane <- na.omit(dane)

ggplot(data = dane, mapping = aes(x = panel, label = ..count..)) + 
  geom_bar() + 
  geom_label(stat = 'count')

dane %>% filter(panel == 'ST') %>% 
  ggplot(aes(x = subpanel)) + 
  geom_bar()

filter(dane, panel == 'ST') %>% 
  group_by(subpanel) %>% 
  summarise(n = length(budget),
            total_budget = sum(budget)) %>% 
  ggplot(aes(x = n, y = total_budget, color = subpanel, label = subpanel)) +
  geom_point(size = 4)



filter(dane, panel == 'ST') %>% 
  group_by(subpanel) %>% 
  summarise(n = length(budget),
            total_budget = sum(budget)) %>% 
  ggplot(aes(x = n, y = total_budget, label = subpanel)) +
  geom_point() + geom_smooth(method = 'lm') + geom_label()

library(ggrepel)

dane %>% group_by(subpanel, panel) %>% 
  summarise(n = length(budget),
            total_budget = sum(budget)) %>% 
  ggplot(aes(x = n, y = total_budget, label = subpanel)) +
  geom_point() + geom_smooth(method = 'lm') + geom_label_repel() +
  facet_wrap(~ panel, scales = 'free')


p <- dane %>%  
  ggplot(aes(x = subpanel, y = budget)) + geom_boxplot(outlier.colour = NA) + geom_violin(fill = NA) +
  facet_wrap(~ panel, ncol = 1, scales = 'free_x')


pdf('nazwa.pdf', width = 8, height = 5)
p + ggtitle('Tytuł')
dev.off()


bp <- dane %>% filter(subpanel %in% c('NZ2', "NZ8", 'ST1', 'ST6')) %>% 
  ggplot(aes(x= panel, fill = subpanel, label = ..count.., color = subpanel)) + 
  geom_bar(position = position_dodge(width = 1)) + 
  geom_label(stat = 'count', position = position_dodge(width = 1), fill = 'white', vjust = 1, show.legend = FALSE)

bp + theme_bw() + 
  theme(legend.position = 'bottom') +
  scale_x_discrete('Panel NCN') + 
  scale_y_continuous('Liczba projektów') + 
  scale_color_manual('Subpanel', values = grey(1:6/7)) + 
  scale_fill_manual('Subpanel', values = grey(1:6/7))


dane_filter <- dane %>% filter(subpanel %in% c('NZ2', 'NZ8', 'ST1', 'ST6')) %>% 
  group_by(panel, subpanel) %>% 
  summarise(n = length(panel)) %>% 
  arrange(desc(n))
  
bar_colors <- setNames(grey(0L:3/5), as.character(dane_filter$subpanel))
  
p <- dane_filter %>% ggplot(aes(x = panel, fill = subpanel, y = n)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual(values = bar_colors) + 
  theme(legend.position = 'bottom')

p + coord_flip()


dane %>% ggplot(aes(x = duration,y = coinvestigators)) +
  geom_point()

