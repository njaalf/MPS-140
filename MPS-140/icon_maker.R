library(ggplot2)
library(hexSticker)
library("scatterplot3d")
library(tidyverse)
set.seed(11)
n <-20
mys <- c(3, 6, 9)
beta = -1; gamma = -1.2

ws <- lapply(mys, \(my){
  s = rnorm(n)+my
  c = my - s + 0.2*rnorm(n)
  w = 10+beta * s + gamma * c + .2*rnorm(n)
  data.frame(my_s=my, s,c,w)
})

df <- do.call(rbind, ws)
df <- data.frame(time_ind=df$s, time_group=df$my_s, w=df$w)
df %>% group_by(time_group) %>% summarise(mean(time_ind), mean(w))

colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[df$time_group/3]

png("images/3dscatter.png", width=300, height=300)
p <- scatterplot3d(df, xlab="Sosiale medier", ylab="", 
              zlab="Individuell trivsel", 
              pch=df$time_group/3+15,
              color =colors, type="h", lwd=.3, box=F, ylim=c(0,9))
dev.off()              

p <- ggplot(df, aes(time_ind, w, color=factor(time_group)))+geom_point()+guides(color="none")+
  xlab("Time online")+ylab("Well-being")+theme(axis.text.x=element_blank(), 
                                               axis.ticks.x=element_blank(), 
                                               axis.text.y=element_blank(), 
                                               axis.ticks.y=element_blank()) 


p.sticker <- sticker(
  p+theme_minimal(), package="MPS 140", p_size=20, s_x=1, s_y=.75, s_width=1.2, s_height=.8,
  filename="images/icon.png",h_fill="seagreen", h_color="orange"
)

sticker(p, package="hexSticker", p_size=20, s_x=1, s_y=.75, s_width=1.3, s_height=1,
        filename="inst/figures/ggplot2.png")


