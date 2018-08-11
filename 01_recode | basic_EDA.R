library(tidyquant)
library(highcharter)
library(plyr)
library(htmltools)

#Recode the target ('class') as 'No'- will NOT make 50k and 'Yes' - will MAKE 50K

adult %>% mutate(class = plyr::mapvalues(class,from = c(" <=50K",  " >50K",   " <=50K.", " >50K."), 
                                         to =c('No','Yes','No','Yes'))) %>% 
  mutate(age = case_when(
    age < 20 ~ '17-20yo',
    age < 30 ~ '<30yo',
    age < 40 ~ '<40yo',
    age < 50 ~ '<50yo',
    age < 60 ~ '<60yo',
    TRUE ~ 'Over_60yo'
  )) -> adult


#Factor variables

adult %>% mutate_if(is.character,as.factor) %>% select_if(is.factor) %>% 
  map_df(~unique(.) %>% length) %>% gather() %>% arrange(desc(value))-> general


highchart() %>% hc_chart(type = "bar") %>% 
  hc_add_series(general$value,colorByPoint = TRUE,
                colors = c(rep('red',4),rep('green',3),rep('yellow',2),rep('grey',3))) %>% 
  hc_xAxis(categories = as.factor(general$key)) %>%
  hc_add_theme(hc_theme_ggplot2()) %>% hc_title(text = 'The number of unique factors')

#Table
adult %>% select_if(is.factor) %>% map(~table(.)) 

#Visualise all factor variables
map(names(adult %>% select_if(is.factor)), function(x){
  adult[[x]] %>% 
    hchart(showInLegend = FALSE,color='blue') %>% 
    hc_add_theme(hc_theme_ggplot2()) %>% 
    hc_title(text = x) %>% 
    hc_yAxis(title = list(text = ""))
}) %>% 
  hw_grid(rowheight = 300)  %>% browsable()

#Numeric variables

adult %>% select_if(is.numeric) %>% map_df(~unique(.) %>% length) %>% 
  gather() %>% arrange(desc(value))-> general_num

highchart() %>% hc_chart(type = "bar") %>% 
  hc_add_series(general_num$value,colorByPoint = TRUE) %>% 
  hc_xAxis(categories = as.factor(general_num$key)) %>%
  hc_add_theme(hc_theme_ggplot2()) %>% hc_title(text = 'Numeric Data')

#Visualise the distribution of numeric variables

map(names(adult %>% select_if(is.numeric)), function(x){
  adult[[x]] %>% 
    hchart(showInLegend = FALSE,color='blue') %>% 
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = x) %>% 
    hc_yAxis(title = list(text = ""))
}) %>% 
  hw_grid(rowheight = 300)  %>% browsable()

#Function for intersection between 2 variables

x_highcharter<-function(data,...) {
  
  dots<-quos(...)
  
  data2<-data %>% count(!!!dots) %>% `colnames<-`(c('feature1','feature2','n'))
  
  hc<-hchart(enexpr(data2), "bar", hcaes(x = feature1, y = n, group=feature2)) %>% 
    hc_plotOptions(series = list(stacking = "normal")) %>% hc_xAxis(visible=T) %>% hc_yAxis(visible=T) %>% 
    hc_title(text=glue('{dots}')) 
  print(hc)
  
}

p1<-x_highcharter(adult,workclass,sex)  
p2<-x_highcharter(adult,education,maritalstatus)
p3<-x_highcharter(adult,class,sex)
p4<-x_highcharter(adult,age,sex)

#It is possible to add any combination

hw_grid(p1,p2,p3,p4,rowheight = 500)  %>% browsable()

 
