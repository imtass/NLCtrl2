theme_media = function(){
  theme_bw() +  theme(panel.border = element_blank()) + 
    theme(axis.line = element_line(colour = "black")) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
}
