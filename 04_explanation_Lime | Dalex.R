library(lime)
library(DALEX)
library(rlang)
library(highcharter)
library(h2o)
library(tidyquant)


explainer <- as.data.frame(train) %>% select(-class) %>% lime::lime(load_model,
      bin_continuous = T, n_bins = 4, n_permutations = 5000)

# Explain new observations
explanation <- as.data.frame(test[1:4,])  %>% select(-class) %>% lime::explain(explainer,
          n_labels = 1, n_features = 12, kernel_width   = 1)

plot_features(explanation)

#Visualisation by highcharter
function_for_lime <- function(explanation,case_num) {
  data <- explanation %>% filter(case %in% enexpr(case_num)) %>%  mutate(choose_color=case_when(
    feature_weight > 0 ~ 'Supports',
    TRUE ~ 'Contradicts'
  )) 
  
  prob<-explanation %>% filter(case %in% enexpr(case_num)) %>% select(case,label_prob) %>% 
    mutate(prob=glue('{round(label_prob,2)}')) %>% unique(.) %>% .[,3]
  
  label<-explanation %>% filter(case %in% enexpr(case_num)) %>% select(case,label) %>% 
    mutate(prob=glue('{label}')) %>% unique(.) %>% .[,3]
  
  explanation_fit<-explanation %>% filter(case %in% enexpr(case_num)) %>% select(case,model_r2) %>% 
    mutate(prob=glue('{round(model_r2,2)}')) %>% unique(.) %>% .[,3]
  
  hc<-hchart(enexpr(data),
             "bar", hcaes(y = feature_weight, x = feature_desc, group=choose_color),
             colorByPoint=F) %>% hc_colors(colors = c('red','green')) %>% 
    hc_add_theme(hc_theme_elementary(chart = list(
      backgroundColor = "transparent",
      divBackgroundImage = "https://pixy.org/src/12/125033.jpg",
      style = list(fontFamily = "Lato")),
      title = list(
        style = list(
          color = "#666600",backgroundColor = "transparent"
        ) 
      ))) %>% 
    hc_title(text=glue("
                       <b>Case:</b> {enexpr(case_num)} <br>
                       <b>Label:</b> {label}  <br>
                       <b>Label_prob:</b> {prob} <br>
                       <b>Explanation Fit:</b> {explanation_fit}
                       "),useHTML=T) %>% 
    hc_legend(align='right')
  
  print(hc)
}

l1<-function_for_lime(explanation = explanation,case_num = 1)

l2<-function_for_lime(explanation = explanation,case_num = 2)

l3<-function_for_lime(explanation = explanation,case_num = 3)

l4<-function_for_lime(explanation = explanation,case_num = 4)


hw_grid(l1,l2,l3,l4,rowheight = 400) %>% browsable()


#Dalex explanation

x_valid <- as.data.frame(validation)

y_valid <- ifelse(pull(x_valid,class)=='Yes',1,0)

pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[3L]])
}

pred(load_model, x_valid) %>% head()

load_model@model_id %>% str_split(.,'_',simplify = T) %>% .[,1] -> leader

explainer_stacked <- DALEX::explain(
  model = load_model,
  data = x_valid,
  y = y_valid,
  predict_function = pred,
  label = glue('{leader}')
)

summary(explainer_stacked)

# compute predictions & residuals
resids_stacked <- DALEX::model_performance(explainer_stacked)
resids_stacked

p1 <- plot(resids_stacked)
p2 <- plot(resids_stacked, geom = "boxplot")

gridExtra::grid.arrange(p1, p2, nrow = 1)

vip_stacked <- DALEX::variable_importance(explainer_stacked, n_sample = -1, loss_function = loss_root_mean_square) 

plot(vip_stacked, max_vars = 15)


capitalgain  <- variable_response(explainer_stacked, variable =  "capitalgain", type = "pdp")

plot(capitalgain)

capitalloss  <- variable_response(explainer_stacked, variable =  "capitalloss", type = "pdp")

plot(capitalloss)

maritalstatus  <- variable_response(explainer_stacked, 
                                  variable = "maritalstatus", type = "factor")

plot(maritalstatus)

relationship  <- variable_response(explainer_stacked, 
                                  variable = "relationship", type = "factor")

plot(relationship)


education <- variable_response(explainer_stacked, 
                                   variable = "education", type = "factor")

plot(education)


occupation  <- variable_response(explainer_stacked, 
                                   variable = "occupation", type = "factor")

plot(occupation)



new_cust <- as.data.frame(validation[2, ])
new_cust_gbm <- prediction_breakdown(explainer_stacked, observation = new_cust)
class(new_cust_gbm)

new_cust_gbm[1:10, 1:5] %>%
  knitr::kable()

plot(new_cust_gbm)
