
ppv_summary <- function(dat = test,
                        model = train_model_v2,
                        gp_des = "",
                        num_group = 5,
                        ref_pos = 2) {
  dat$fited_0 <- predict(model, newdata = dat, type = "response")
  group_break <- unique(quantile(dat$fited_0, probs = seq(0, 1, 1 / num_group)))
  dat$predicted_gp <- cut(dat$fited_0,
    breaks = group_break,
    include.lowest = T
  )
  tab <- dat %>%
    group_by(predicted_gp) %>%
    summarise(
      total = n(),
      pred = round(mean(fited_0), 3),
      actual = round(mean(prop_r_curr), 3)
    )
  tab1 <- tab %>%
    knitr::kable(format = "html") %>%
    kableExtra::kable_styling(full_width = F)
  ylim_l <- min(tab$actual)
  ylim_h <- max(tab$actual)
  g <- ggplot(tab, aes(x = (length(group_break) - 1):1, y = actual)) +
    geom_point(size = 1, color = "blue") +
    geom_line(size = 0.4, color = "blue") +
    geom_text(aes(label = actual, x = (length(group_break) - 1):1, y = actual), colour = "blue", vjust = -1) +
    expand_limits(y = c(ylim_l, ylim_h + 0.2)) +
    scale_x_continuous(breaks = seq(1, (length(group_break) - 1), 1)) +
    geom_hline(yintercept = mean(dat$prop_r_curr), linetype = "dashed", color = "red") +
    geom_text(x = ref_pos, y = ylim_h + 0.1, label = paste("Mean Prop Capture \n", round(mean(dat$prop_r_curr), 3)), color = "red") +
    theme(
      axis.title.y = element_text(color = "black"),
      axis.title.y.right = element_text(color = "blue")
    ) +
    xlab("Predicted Prop Capture Rate Group") +
    ylab("Actual Prop Capture") +
    ggtitle(paste("Predicted Power Plot (tenure: ", gp_des, ")", sep = ""))
  return(list(table = tab1, plot = g))
}