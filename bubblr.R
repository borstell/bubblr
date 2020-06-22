library(RColorBrewer)
library(wesanderson)
library(ggplot2)
library(broman)
library(tidyr)
library(raster)
library(stringr)
library(patchwork)
library(ggforce)
library(data.table)

setwd("")

make_bubble <- function(palette=17, xmax=100, ymax=100) {
  if (palette > 0 & palette < 19) {
    palette <- wes_palettes[palette][[1]]
    start_color <- hex2dec(gsub("#", "", sample(palette, 1)))
  }
  else {
    start_color <- round(runif(1, min=0, max=16777215))
  }
  bubble_size <- sample(1:round(xmax/10), 1)
  x <- sample(round(xmax/20):(xmax-round(xmax/20)), 1)
  y <- sample(round(ymax/20):(ymax-round(ymax/20)), 1)
  rs <- c()
  xs <- c()
  ys <- c()
  cols <- c()
  num <- start_color
  for (r in 1:bubble_size) {
    xs <- c(xs, x+r/2)
    ys <- c(ys, y+r/2)
    rs <- c(rs, bubble_size-(r-1))
    col <- paste0("#", str_pad(dec2hex(num), width = 6, pad = "0"))
    cols <- c(cols, col)
    num <- num-abs(num-round(rnorm(1, mean=num, sd=sqrt(r))))
  }
  return(data.frame(x=xs, y=ys, r=rs, color=cols))
}

make_bubbles <- function(palette=17, xmax=100, ymax=100, n) {
  bubbles <- list()
  for (b in 1:n) {
    bubble <- make_bubble(palette, xmax, ymax)
    bubbles[[b]] <- bubble
  }
  return(rbindlist(bubbles))
}

plot_bubbles <- function(n=10, bubble_max=100, xmax=500, ymax=500) {
  for (i in 1:n) {
    pal <- sample(1:19, 1)
    bg_pal <- sample(1:19, 1)
    bg <- sample(wes_palettes[bg_pal][[1]], 1)
    bubble_n <- sample(round(bubble_max/2):bubble_max, 1)
    bubbles <- make_bubbles(palette=pal, xmax=xmax, ymax=ymax, n=bubble_n)
    ggplot(bubbles) +
      annotate("rect", xmin=0, xmax=500, ymin=0, ymax=500, alpha=0.2, fill=bg) +
      geom_circle(aes(x0 = x, y0 = y, r = r, fill = color), alpha=.2, linetype=0) +
      scale_fill_identity() +
      theme_void() +
      coord_fixed() +
      theme(plot.margin = unit(c(-50,-50,-50,-50), "pt"))
    ggsave(paste0("bubble_", format(Sys.time(), "%Y%m%d-%H%M%S"),".png"), width=5, height=5, units = "in", dpi = 300)
  }
}
plot_bubbles()
