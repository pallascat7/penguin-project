incorrect_figure <- function(culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm, shape = sex)) + 
    geom_point(colour = "yellow", show.legend = FALSE, size = 1)+
    theme_minimal()+
    scale_shape_manual(values = c(3, 4,8))
}

simple_plot<- function(culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm)) + 
    geom_point(show.legend = TRUE, size = 1, colour = "blue")+
    labs(x = "Culmen length (mm)", y = "Flipper length (mm)")+
    theme_bw()+
    geom_hline(yintercept = 200.9, linetype = "dashed") +
    geom_vline(xintercept = 43.92 , linetype = "dashed")
} #just culmen length and flipper length

more_complex_plot1<- function(culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm, colour = species)) + 
    geom_point(show.legend = TRUE, size = 1)+
    labs(x = "Culmen length (mm)", y = "Flipper length (mm)")+
    theme_bw()+
    scale_colour_viridis(discrete = TRUE)
  
} #c~F with species coloured in in viridis


more_complex_plot2<- function(culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm, colour = species)) + 
    geom_point(show.legend = TRUE, size = 1)+
    labs(x = "Culmen length (mm)", y = "Flipper length (mm)")+
    theme_bw()+
    scale_colour_viridis(discrete = TRUE)+
    facet_wrap(~species)
  
}#c~F with species coloured in in viridis with facet_wrap

location_plot1 <- function(culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm, colour = island)) + 
    geom_point(show.legend = TRUE, size = 1)+
    labs(x = "Culmen length (mm)", y = "Flipper length (mm)")+
    theme_bw()+
    scale_colour_manual(values = c("Biscoe" = "#45B5A8",
                        "Dream"= "#002147",
                        "Torgersen" = "#E2679F"))
} #c~F with location in different colours

location_plot2 <- function(culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm, colour = island)) + 
    geom_point(show.legend = TRUE, size = 1.4, aplha = 0.6)+
    labs(title = "The correlation between flipper length and culmen length", 
         subtitle = "The correlation of flipper length and culmen length according to the species and the island",
         x = "Culmen length (mm)", y = "Flipper length (mm)",
         colour = "Island where the penguin is found")+
    theme_bw()+
    scale_colour_manual(values = c("Biscoe" = "#45B5A8",
                                   "Dream"= "#002147",
                                   "Torgersen" = "#E2679F"))+
    facet_wrap(~species)
} #c~F with location in different colours with species in face_wrap
#-------------------------------------------------------------------------------
##linear regression plots 

lr_plot_CF <- function(culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm)) + 
    geom_point(show.legend = TRUE, size = 1, colour = "blue")+
    labs(x = "Culmen length (mm)", y = "Flipper length (mm)")+
    theme_bw()+
    geom_hline(yintercept = 200.9, linetype = "dashed") +
    geom_vline(xintercept = 43.92 , linetype = "dashed")+
    geom_smooth(method = "lm")
} #just c and f

lr_plot_species1 <- function(culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm, colour = species)) + 
    geom_point(show.legend = TRUE, size = 1)+
    labs(x = "Culmen length (mm)", y = "Flipper length (mm)")+
    theme_bw()+
    scale_colour_viridis(discrete = TRUE)+
    geom_smooth(aes(x = culmen_length_mm, y = flipper_length_mm), 
                method = "lm", se = FALSE, inherit.aes = FALSE, colour = "purple")
} 
#just c and f coloured by species with one main lm line in purple

lr_plot_species2 <- function (culmen_l_flipper_na){
  culmen_l_flipper_na %>%
    ggplot(aes(y = flipper_length_mm, x = culmen_length_mm, colour = species)) + 
    geom_point(show.legend = TRUE, size = 1)+
    labs(x = "Culmen length (mm)", y = "Flipper length (mm)")+
    theme_bw()+
    scale_colour_viridis(discrete = TRUE)+
    facet_wrap(~species)+
    geom_smooth(aes(x = culmen_length_mm, y = flipper_length_mm), 
                method = "lm", se = TRUE)
    
} # lm c and f coloured by species and split by species

#---------------------------------------------------------------- 
# the figure
# it has c~f coloured by location and split by species with hopefully 3 lm line. there's title and subtitle

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

-----------------------------------------------
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