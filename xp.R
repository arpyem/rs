library(tidyverse)

xp = read_csv('xp.csv')

xp %>% ggplot(aes(x = Level, y = XP)) + geom_line()
xp %>% ggplot(aes(x = Level, y = Difference)) + geom_line()

# xp milestones by xp rates/level

xp %>%
    mutate(percent = lag(XP, 1) / XP) %>% 
    ggplot(aes(x = Level, y = percent)) + 
    geom_line()
