library(highcharter)
library(caret)
library(recipes)
library(corrr)
library(glue)
library(rlang)
library(tidyquant)

#Split the data
data_split<- createDataPartition(adult$class, p=0.8,list = F,times = 1)

head(data_split)

training<-adult[data_split,]
testing<-adult[-data_split,]

#Prepare
recipe_cor <- recipe(class ~ ., data = training) %>% 
  step_dummy(all_nominal(), all_outcomes(), one_hot = T) %>% 
  step_center(all_predictors(), all_outcomes()) %>%
  step_scale(all_predictors(), all_outcomes()) %>%
  prep()

#Bake
train_tbl <- bake(recipe_cor, training)
test_tbl  <- bake(recipe_cor, testing)

#Correlation df
get_cor<- function(data,target,use = 'pairwise.complete.obs',method = 'pearson') {
 
  target1<-enquo(target)
  
  correlation<- data %>% mutate(class=!!target1) %>%  correlate(use=use,method = method)  %>% 
    focus(class) %>% arrange(abs(class)) %>%
    rename(feature = rowname) %>% arrange(class)
  return(correlation)
}

get_cor(test_tbl %>% select(class_Yes,contains('occupation'),contains('workclass'),
                             contains('education')), class_Yes) 



#Visualisation of correlation

get_cor <- function(data,target,use = 'pairwise.complete.obs',method = 'pearson') {
  
  target1<-enquo(target)
  
  correlation<- data %>% mutate(class=!!target1) %>%  correlate(use=use,method = method)  %>% 
    focus(class) %>% arrange(abs(class)) %>% mutate(class = base::round(class,2)) %>% 
    rename(feature = rowname) %>% arrange(class) %>% 
    mutate(feature_names=str_split(feature, '_', simplify = T) %>% .[,1]) %>% 
    mutate(choose_color=case_when(
      class > 0 ~ 'Positive',
      TRUE ~ 'Negative'
    )) %>% filter(!(feature %in% glue(enexpr(target))))
  
  hc<-hchart(enexpr(correlation),"bar", hcaes(y = class, x = feature, group=choose_color),
             colorByPoint=F) %>% hc_add_theme(hc_theme_darkunica(chart = list(
               backgroundColor = "transparent",
               divBackgroundImage = "https://media.giphy.com/media/t7Qb8655Z1VfBGr5XB/giphy.gif",
               style = list(fontFamily = "Lato")),
               title = list(
                 style = list(
                   color = "white"
                 ) 
               ))) %>% 
    hc_title(text=glue('{unique(correlation$feature_names)}')) %>% 
    hc_subtitle(text = glue('against {enexpr(target)}')) %>% hc_colors(colors = c('red','green'))
  
  return(hc)
}

get_cor(test_tbl %>% select(class_Yes,contains('occupation'),contains('workclass'),
                             contains('education')), class_Yes)

get_cor(test_tbl %>% select(class_Yes,contains('marital'),contains('race'),
                            contains('sex')), class_Yes)


