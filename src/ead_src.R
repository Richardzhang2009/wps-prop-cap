ead_analysis <- function(dat = mydata_3, 
                         response_var = "response_model",
                         var="tenure_y",
                         smooth_ind_ori = 0,
                         smooth_ind = 1,
                         vjust_m=2,
                         title = "Total Number of Policies Owned",...){
  
  if(smooth_ind_ori){
    q <- ggplot(dat, aes(x=!!var, y=!!response_var, group=!!var)) + 
      geom_point() +  
      geom_smooth(method = "loess", se= FALSE, aes(group=1, color = "loess")) +
      geom_smooth(method = lm, se = FALSE, aes(group=1, color = "lm"))+
      labs(color = "Fitting Methods") +
      xlab(title) +
      ylab("Prop Capture Rate") +
      ggtitle(paste('Prop Capture Rate v.s. ',title,sep=''))
  }else{
  q <- ggplot(dat, aes(x=!!var, y=!!response_var, group=!!var)) + 
    geom_point() +  
    xlab(title) +
    ylab("Prop Capture Rate") +
    ggtitle(paste('Prop Capture Rate v.s. ',var,sep=''))
  }
  tab <- dat %>% 
    group_by(!!var) %>%
    summarise(total_pol = n(),
              prop_avg = round(mean(!!response_var)*100,0))
  
  g <- ggplot(tab,aes(x = !!var, y = prop_avg*100)) + 
    geom_bar(mapping = aes(x = !!var, y = total_pol), stat = "identity", fill = "tan1", colour="sienna3") +
    geom_point(size = 1, color = "blue") + 
    geom_line(size = 0.4, color = "blue") +
    geom_text(aes(label=prop_avg, x=!!var, y=prop_avg*100), colour="blue", vjust = -1)+
    geom_text(aes(label=total_pol, x=!!var, y=total_pol), colour="black", vjust = vjust_m)+
    scale_y_continuous(name = "Total Number of Policies", 
                       sec.axis = sec_axis(~./100, name = "Mean of Prop (%)",
                                           labels = function(b) { paste0(round(b, 0), "%")})) + 
    theme(
      axis.title.y = element_text(color = "black"),
      axis.title.y.right = element_text(color = "blue")) +
    xlab(title) +
    ggtitle(paste(title," and Mean of Prop Capture",sep=''))
  
  p <- ggplot(dat, aes(x=!!var, y=!!response_var, group=!!var))

  if(smooth_ind){
  plot_f = g +
    p + geom_boxplot() +
    geom_smooth(method = "loess", se= FALSE, aes(group=1, color = "loess")) +
    geom_smooth(method = lm, se = FALSE, aes(group=1, color = "lm"))+
    labs(color = "Fitting Methods") +
    stat_summary(fun=mean, geom="point", shape=20, size=3, color="blue", fill="red") +
    xlab(title) +
    ylab("Prop Capture Rate") +
    ggtitle(paste('Prop Capture Rate v.s.',title,sep=''))
  }else{
    plot_f = g +
      p + geom_boxplot() +
      stat_summary(fun=mean, geom="point", shape=20, size=3, color="blue", fill="red") +
      xlab(title) +
      ylab("Prop Capture Rate") +
      ggtitle(paste('Prop Capture Rate v.s.',title,sep=''))
  }
  return(list(table=tab, plot_ori = q, plot = plot_f))
}
