library(ggplot2)
monty_hall_func <- function(no_options) {
  doors <- rep(0, no_options)
  doors[sample(no_options, 1)] <- 1
  ans <- which(doors == 1)
  # print(ans)
  # length(doors)
  monty <- matrix(NA, nrow = ceiling(sqrt(length(doors))), ncol = ceiling(sqrt(length(doors))))
  monty[1:length(doors)] <- doors
  monty_plot <- as.data.frame.table(monty)
  monty_plot <- na.omit(monty_plot)
  monty_plot$door_no <- 1:nrow(monty_plot)
  p <- ggplot(monty_plot, aes(x = Var1, y = Var2, fill = sample(10, nrow(monty_plot), replace = T) ))+
    geom_tile(stat = "identity", width = 0.9, height = 0.9)+
    geom_label(aes(label = door_no), fill = 'white')+
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank())
  print(p)

  choice <- gtools::ask("Welcome to the Monty Hall problem! Behind one of these doors is a reward, will you guess which one it is behind? Input a number between 1 and the number of options")
  choice <- as.numeric(choice)

  doors_reveal <- 1:no_options
  doors_reveal <- doors_reveal[!doors_reveal %in% choice & !doors_reveal %in% ans]
  
  final_doors_idx <- 1:no_options
  if (ans != choice) {
  final_doors_idx <- final_doors_idx[!final_doors_idx %in% doors_reveal]
  } else {
    final_doors_idx <- final_doors_idx[c(choice, sample(final_doors_idx[!final_doors_idx %in% ans], 1))]
  }
#  length(doors_reveal)

  monty_plot2 <- monty_plot[final_doors_idx, ]
  p2 <- ggplot(monty_plot2, aes(x = Var1, y = Var2, fill = 10 ))+
    geom_tile(stat = "identity", width = 0.9, height = 0.9)+
    geom_label(aes(label = door_no), fill = 'white')+
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank())
  print(p2)

  Sys.sleep(2)

  if(askYesNo("I have now opened all but one of the other doors to reveal no reward behind them. There are two doors remaining: the one you originally chose and one of the many others! You now have another choice: you may change your mind and open the other door or hold fast to your original decision!. Do you want to switch doors?")) {
    choice <- final_doors_idx[!final_doors_idx %in% choice]
  }

  #final_doors <- doors[-doors_reveal]

  result <- doors[choice]

  if (result == 1) {
    cat("\n\nCongradulations, you win!")
  } else {
    cat("\n\nLoooooser, it was behind the other door!!!")
  }


  p3 <- ggplot(monty_plot2, aes(x = Var1, y = Var2, fill = Freq ))+
    geom_tile(stat = "identity", width = 0.9, height = 0.9)+
    geom_label(aes(label = door_no), fill = 'white')+
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank())
  print(p3)

}
monty_hall_func(30)









