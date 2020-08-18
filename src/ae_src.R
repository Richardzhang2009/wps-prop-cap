library(rlang)
ae_analysis <- function(model = train_model_v1,
                        dat_train = mydata_3,
                        ymin=0,
                        ymax=17000,
                        var="industry",
                        train_ind = 1,
                        scale = 10000,
                        whole_ind = 0,
                        ...){
  
  if(train_ind){dat_train$fit<-fitted(model);data_type = "Train"
  }else{dat_train$fit<-predict(model,newdata = dat_train, type = "response");data_type = "Test"}
  dat_train$ae <- dat_train$response/dat_train$fit
  
  dat_train_sum <- dat_train %>%
    group_by(!!var) %>%
    summarise(total=n(),
              actual_mean = mean(response),
              pred_mean = mean(fit),
              ae_groupwise = round(actual_mean/pred_mean,2),
              ae = round(mean(ae),2))
  print(dat_train_sum)
  if(!whole_ind){
    
    p <- ggplot(dat_train_sum, aes(x=!!var, y=ae*scale, group=!!var)) +
      geom_bar(mapping = aes(x = !!var, y = total), stat = "identity", fill = "tan1", colour="sienna3")+
      geom_hline(yintercept=scale, linetype="dashed", color = "red")+
      geom_point(size = 1, color = "blue") + 
      geom_line(size = 0.4, color = "blue") +
      geom_text(aes(label=ae, x=!!var, y=ae*scale), colour="blue", vjust = 2)+
      geom_text(aes(label=total, x=!!var, y=total), colour="black", vjust = -1)+
      scale_y_continuous(name = "Total Number of Policies", 
                         sec.axis = sec_axis(~./scale, name = "A/E",
                                             labels = function(b) { paste0(round(b, 2), "")})) + 
      ggtitle(paste('Prop Capture Rate Actual v.s. Expected (',data_type,')',sep='')) +
      theme(
        axis.title.y = element_text(color = "black"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.x = element_text(angle=90)) +
      xlab(var) +
      coord_cartesian(ylim = c(ymin,ymax)) 
    print(p)
    
  }else{
    p <- ggplot(dat_train_sum, aes(x=!!var, y=ae*scale, group=!!var)) +
      geom_bar(mapping = aes(x = !!var, y = total), stat = "identity", fill = "tan1", colour="sienna3")+
      geom_hline(yintercept=scale, linetype="dashed", color = "red")+
      geom_point(size = 1, color = "blue") + 
      geom_line(size = 0.4, color = "blue") +
      geom_text(aes(label=ae, x=!!var, y=ae*scale), colour="blue", vjust = 2)+
      geom_text(aes(label=total, x=!!var, y=total), colour="black", vjust = -1)+
      scale_y_continuous(name = "Total Number of Policies", 
                         sec.axis = sec_axis(~./scale, name = "A/E",
                                             labels = function(b) { paste0(round(b, 2), "")})) + 
      ggtitle('Prop Capture Rate Actual v.s. Expected (All)') +
      theme(
        axis.title.y = element_text(color = "black"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.x = element_text(angle=90)) +
      xlab(var)+
      coord_cartesian(ylim = c(ymin,ymax)) 
    print(p)
  }
}

