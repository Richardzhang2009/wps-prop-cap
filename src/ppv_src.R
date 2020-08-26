ppv_summary <- function(dat = test, model = train_model_v2){

  dat$fited_0 <- predict(model, newdata = test, type = "response")
  dat$predicted_gp <-  cut(dat$fited_0,
                            breaks = quantile(dat$fited_0,probs = seq(0,1,0.1)),
                            include.lowest = T)  
tab <- dat %>%
  group_by(predicted_gp) %>%
  summarise(total=n(),
            pred = round(mean(fited_0),3),
            actual = round(mean(prop_r_curr),3))
tab1 <- tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F) 
g<- ggplot(tab,aes(x = 10:1, y = actual)) + 
  geom_point(size = 1, color = "blue") + 
  geom_line(size = 0.4, color = "blue") +
  geom_text(aes(label=actual, x=10:1, y=actual), colour="blue", vjust = -2) +
  expand_limits(y=c(0.15,0.55)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  geom_hline(yintercept=mean(test$prop_r_curr), linetype="dashed", color = "red")+
  geom_text(x=8.5, y=0.315, label=paste("Mean Prop Capture \n",round(mean(test$prop_r_curr),3)),color = "red") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue")) +
  xlab("Predicted Prop Capture Rate Group") +
  ylab("Actual Prop Capture") +
  ggtitle('Predicted Power Plot (Test)')
return(list(table=tab1, plot=g))
}