incorrect_figure <- function(culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm, shape = sex)) + 
    geom_point(colour = "yellow", show.legend = FALSE, size = 1)+
    theme_minimal()+
    scale_shape_manual(values = c(3, 4,8))
}

lr_location_plot <- function (culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm, colour = island)) + 
    geom_point(show.legend = TRUE, size = 1.4, alpha = 0.7)+
    labs(title = "The correlation between flipper length and culmen length", 
         subtitle = "The correlation of flipper length and culmen length according to the species and the island",
         x = "Culmen length (mm)", y = "Flipper length (mm)",
         colour = "Island")+
    theme_bw()+
    scale_colour_manual(values = c("Biscoe" = "#45B5A8",
                                   "Dream"= "#002147",
                                   "Torgersen" = "#E2679F"))+
    facet_wrap(~species)+
    geom_smooth(aes(x = culmen_length_mm, y = flipper_length_mm), 
                method = "lm", se = FALSE, inherit.aes = FALSE)
} 

save_misleading_plot_png <- function(culmen_l_flipper_na, 
                                     filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, units   =  "cm", res =  res, scaling =  scaling)
  misleading_plot <- incorrect_figure(culmen_l_flipper_na)
  print(misleading_plot)
  dev.off()
}

save_location_plot_png <- function(culmen_l_flipper_na, 
                                  filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  location_plot <- lr_location_plot(culmen_l_flipper_na)
  print(location_plot)
  dev.off()
}