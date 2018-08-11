library(tidyquant)
library(h2o)
library(glue)
library(rlang)

#Run h2o
h2o.init()

h2o_data<-as.h2o(adult)

h2o_data<-h2o.splitFrame(h2o_data,ratios = c(0.8,0.15),seed=1)

train<-h2o_data[[1]]
test<-h2o_data[[2]]
validation<-h2o_data[[3]]

outcome<-'class'
features<-setdiff(names(train), outcome)

aml<-h2o.automl(x=features,y=outcome,
                training_frame = train,
                validation_frame = validation,
                leaderboard_frame = test,max_runtime_secs = 120)
aml@leader

aml@leaderboard

aml@leaderboard %>% as.tibble() %>% select(model_id,auc,logloss)-> auc_log

hchart(auc_log %>% gather(key=key,value = value, -model_id),
       "bar", hcaes(x = model_id, y = value, group=key)) %>% 
  hc_plotOptions(series = list(stacking = "normal")) %>% 
  hc_xAxis(visible=T) %>% hc_yAxis(visible=T) %>% hc_colors(colors = c('green','red'))

#Save model but change "path"
h2o.saveModel(aml@leader,path = '/Users/turgut_a/Downloads/ADULT_data_set',force = T)
load_model<-h2o.loadModel(path='/Users/turgut_a/Downloads/ADULT_data_set/GBM_grid_0_AutoML_20180810_114028_model_0')


#Load
load_model@model_id %>% str_split(.,'_',simplify = T) %>% .[,1]->leader

#Confusion matrix visualisation (accuracy)
h2o.confusionMatrix(load_model,test) %>% as.tibble() %>% select(No,Yes) %>% as.matrix() %>% .[1:2,] %>% 
  fourfoldplot(conf.level = 0, color = c("#ed3b3b", "#0099ff"),
               margin = 1,main = paste(leader,
                                       round(sum(diag(.))/sum(.)*100,0),"%",sep = ' '))

#H2o metrics
h2o.performance(load_model,newdata = test) %>% h2o.metric() %>% select(threshold,precision,recall,tpr,fpr) %>% 
  add_column(tpr_r=runif(400,min=0.003,max=1)) %>% mutate(fpr_r=tpr_r) %>% arrange(tpr_r,fpr_r)->deep_metrics

#AUC
perf<-h2o.performance(load_model,newdata = test) %>% h2o.auc() %>% round(2)

highchart() %>% 
  hc_add_series(deep_metrics, "scatter", hcaes(y = tpr, x = fpr), color='green',name='TPR') %>%
  hc_add_series(deep_metrics, "line", hcaes(y = tpr_r, x = fpr_r), color='red',name='Random Guess') %>% 
  hc_add_annotation(
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 0.75,
          y = 0.25
        ),
        text = "Worse than guessing"
      ),  list( point = list(
        xAxis = 0,
        yAxis = 0,
        x = 0.3,
        y = 0.6
      ),
      text = glue('Better than guessing, AUC = {enexpr(perf)}')))) %>%
  hc_title(text = "ROC Curve") %>% hc_subtitle(text = "Model is performing much better than random guessing") 

#f1 - the harmonic average of the precision and recall

h2o.find_threshold_by_max_metric(h2o.performance(load_model,newdata = test),'f1') ->intercept


highchart() %>% 
  hc_add_series(deep_metrics, "scatter", hcaes(y = precision,x=threshold), color='blue',name='Precision') %>% 
  hc_add_series(deep_metrics, "scatter", hcaes(y = recall,x=threshold), color='red',name='Recall') %>% 
  hc_xAxis(
    opposite = F,
    plotLines = list(
      list(label = list(text = glue('Max_threshold - {round(intercept,2)}')),
           color = "#FF0000",
           width = 3,
           value = glue('{intercept}'))))

#Gain plot
h2o.gainsLift(h2o.performance(load_model,newdata = test)) %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>% 
  select(-contains('lift')) %>% 
  mutate(base = cumulative_data_fraction) %>% 
  rename(gain = cumulative_capture_rate) %>% 
  gather(key = key, value = value, gain, base) %>% as.tibble()%>% mutate(choose_col=case_when(
    key == 'gain' ~ 'red',
    TRUE  ~ 'black'
  )) ->gain_chart

highchart() %>% hc_add_series(gain_chart,"line",hcaes(y=value,x=cumulative_data_fraction,group=choose_col),
                              showInLegend=F,name='Gain') %>% hc_yAxis(title=list(text = "Gain")) %>% 
                                              hc_xAxis(title=list(text='Cumulative Data Fraction')) %>% 
 hc_title(text='Gain chart', useHTML=T, align="center") %>% hc_add_theme(hc_theme_ffx(title = list(
   style = list(color = "red"))))%>% hc_colors(colors = c('black','red'))


 #Lift plot 
 
h2o.gainsLift(h2o.performance(load_model,newdata = test)) %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>% 
  select(-cumulative_capture_rate) %>% 
  mutate(baseline = 1) %>% 
  rename(lift = cumulative_lift) %>% 
  gather(key = key, value = value, lift, baseline) %>% mutate(choose_col=case_when(
    value == 1 ~ 'black',
    TRUE  ~ 'red'
  )) ->lift_chart
  

highchart() %>% hc_add_series(lift_chart,"line",hcaes(y=value,x=cumulative_data_fraction,group=choose_col),showInLegend=F,
                              name='Lift') %>% hc_yAxis(title=list(text = "Lift")) %>% 
  hc_xAxis(title=list(text='Cumulative Data Fraction')) %>% 
  hc_title(text='Lift chart', useHTML=T ,align="center") %>% hc_add_theme(hc_theme_ffx(title = list(
    style = list(
      color = "red",align='center')))) %>% hc_colors(colors = c('black','red'))
  
