library(ggplot2)
library(tidyr)
library(dplyr)

x <- rnorm(10000, 0, 1)
diff <- rnorm(10000, -1, 0.5)
y <- x + diff

df <- data.frame( x = x , y = y , diff = diff ) %>% gather( type, est, x:diff)
summary <- df %>% group_by(type) %>% summarise( ucl = quantile(est, 0.975), lcl = quantile(est, 0.025)) %>% gather( limit, val, ucl:lcl) 

ggplot( df %>% filter( type != 'diff') , aes( x = est, color = type ) ) + 
  geom_density() + 
  geom_vline(data = summary %>% filter( type != 'diff'), aes(xintercept = val, color = type), linetype = 2) + theme_bw() 

ggplot( df %>% filter( type == 'diff') , aes( x = est, color = type ) ) + 
  geom_density() + 
  geom_vline(data = summary %>% filter( type == 'diff'), aes(xintercept = val, color = type), linetype = 2) + theme_bw() 
