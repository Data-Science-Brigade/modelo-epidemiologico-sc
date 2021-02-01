dsb_color_pallete <- c(
  "#ED297C",
  "#008FD5",
  "#FF2700",
  "#FFAB40",
  "#810F7C",
  "#619656",
  "#11C8ED",
  "#ED5434",
  "#11ED98",
  "#C61EF7",
  "#78342A",
  "#FC5900",
  "#AB5F00",
  "#414D91",
  "#032738",
  "#193AF7",
  "#243820"
)

theme_dsb_base <- function(default_color='#1b1b1b', default_text_color='#fcfcfc', grid_color='#595959',
                          base_size=10, margin=24){
  axis_text_margin = margin/2

  dsb_text_default <- theme(text=element_text(family='sans', face='plain', size=base_size, color=default_text_color,
                                              # linespacing=1.1, , ha='center', va='baseline',
                                              angle=0,
                                              margin=margin(t=margin, b=margin, l= margin, r=margin)),
                            plot.background = element_rect(fill=default_color, color=NA),
                            panel.border = element_blank(),
                            panel.background = element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            plot.title = element_text(size=12, face="plain"),
                            title = element_text(size=12, face="plain"),
                            axis.text.x = element_text(angle = 45, hjust = 1, size=10),
                            axis.ticks = element_line(size=0.1),
                            axis.text.y = element_text(size=10),
                            axis.title.x = element_text(size=12),
                            axis.title.y = element_text(size=12),
                            panel.spacing=unit(.45, "points"),
                            legend.title=element_text(size=10),
                            legend.text=element_text(size=9),
                            legend.position = "top",
                            legend.spacing=unit(0, "points"),
                            axis.line = element_line(color = '#212121', size=0.4))

  return(theme_minimal() + dsb_text_default)
}

theme_dsb_dark <- function(){
  dsb_dark <- theme_dsb_base(default_color='#212121', default_text_color='#fcfcfc', grid_color='#595959')
  return(dsb_dark)
}



theme_dsb_light <- function(){
  dsb_dark <- theme_dsb_base(default_color='#ffffff', default_text_color='#212121', grid_color='#bbbbbb')
  return(dsb_dark)
}




