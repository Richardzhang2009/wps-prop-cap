
lift_chart<-function(dat=test, 
                     model = train_model_v2,
                     gp_des='ALL',
                     num_group=5){
  
  dat$fited_0 <- predict(model, newdata = dat, type = "response")
  dat$predicted_gp <-  cut(dat$fited_0,
                           breaks = quantile(dat$fited_0,probs = seq(0,1,1/num_group)),
                           include.lowest = T)  
  tab <- dat %>%
    group_by(predicted_gp) %>%
    summarise(total_pol=n(),
              pred_sum =sum(fited_0),
              actual_sum =sum(prop_r_curr),
              actual_rate = actual_sum/total_pol)
  
  tab1 <- tab %>%
    arrange(desc(predicted_gp)) %>%
    mutate(cum_total=cumsum(total_pol),
           cum_pred = cumsum(pred_sum),
           cum_actual = cumsum(actual_sum),
           cum_pred_rate = cum_pred/cum_total,
           cum_act_rate = cum_actual/cum_total,
           base_rate = last(cum_act_rate),
           lift = cum_act_rate/base_rate,
           lift_crt = lift-1)
  
  table <- tab1 %>%
    select(predicted_gp,
           base_rate,
           cum_act_rate,
           lift
    ) %>%
    knitr::kable(format = "html") %>%
    kableExtra::kable_styling(full_width = F) 
  lift_l = min(tab1$lift)
  lift_h = max(tab1$lift)
  g <- ggplot(tab1,aes(x = 1:num_group, y = lift)) + 
    geom_point(size = 1, color = "blue") + 
    geom_line(size = 0.4, color = "blue") +
    geom_text(aes(label=round(lift,2), x=1:num_group, y=lift), colour="blue", vjust = -1) +
    expand_limits(y=c(lift_l-0.1,lift_h+0.1)) +
    scale_x_continuous(breaks = seq(1, num_group, 1)) +
    geom_hline(yintercept=1, linetype="dashed", color = "red")+
    geom_text(x=1.5, y=1, label=paste("Reference Line \n",1),color = "red") +
    theme(
      axis.title.y = element_text(color = "black"),
      axis.title.y.right = element_text(color = "blue"),
      axis.text.x = element_text(angle=90)) +
    xlab("Prediction Decile") +
    ylab("Lift") +
    ggtitle(paste('Lift Chart (tenure: ',gp_des,')',sep=''))
  return(list(tab = table, plot = g, acc_score = sum(tab1$lift_crt)))
}