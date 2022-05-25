library(ggplot2)
library(tibble)
library(magick)
library(ggtextures)
library(grid)
library(rsvg)

yes <- c(562,1854,1094,753)
no <- c(2869,10496,7013,5325)

d <- data.frame(yes,no)
sum(d$yes)
sum(d$no)


d$scale_yes <- (d$yes/200)
d$scale_n <- (d$no/1100)

data1 <- tibble(
  count = d$scale_yes,
  sport = c("0 days", "1-3 days", "4-6 days", "Everyday"),
  image = list(
    image_read_svg("https://www.svgrepo.com/show/106892/couch.svg"),
    image_read_svg("https://www.svgrepo.com/show/40471/american-football.svg"),
    image_read_svg("https://www.svgrepo.com/show/142452/soccer-ball.svg"),
    image_read_svg("https://www.svgrepo.com/show/19878/bike.svg")
  )
)

p1 <- ggplot(data1, aes(sport, count, image = image)) +
  geom_isotype_col() + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10), labels = c(0, 2, 4, 6, 8, 10))+
  labs(title = "Physical Activity For Children With Asthma",x ="Number of Physically Active Days", y = "Count", caption = "Each image is equal to 200 survey responses")+ 
  theme_classic() +
  theme(axis.text.x = element_text(size = 13)) +
  theme(axis.title.x = element_text(size = 17, vjust = -1)) +
  theme(axis.title.y = element_text(size = 17, vjust = 2)) +
  theme(axis.text.y = element_text(size = 13)) +
  theme(plot.title = element_text(size = 23))+
  theme(plot.caption = element_text(size = 13, vjust = -.5))
p1




data2 <- tibble(
  count = d$scale_n,
  sport = c("0 days", "1-3 days", "4-6 days", "Everyday"),
  image = list(
    image_read_svg("https://www.svgrepo.com/show/106892/couch.svg"),
    image_read_svg("https://www.svgrepo.com/show/44610/bowling.svg"),
    image_read_svg("https://www.svgrepo.com/show/142452/soccer-ball.svg"),
    image_read_svg("https://www.svgrepo.com/show/19878/bike.svg")
  )
)

p2 <- ggplot(data2, aes(sport, count, image = image)) +
  geom_isotype_col() + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10), labels = c(0, 2, 4, 6, 8, 10)) +
  labs(title = "Physical Activity For People Without Asthma",x ="Number of Physically Active Days", y = "Count", caption = "Each image is equal to 1,100 survey responses")
p2

d2 <- read.csv("~/DSC 465/d2.csv")
library(ggmosaic)
library(lemon)

p3<- ggplot(data = d2) +
  geom_mosaic(aes(weight = count, x = product( attend_.event,asthma), fill = asthma)) +
  labs(y="Attend Events ", title='Attend Events') + guides(fill=guide_legend(title = "Asthma", reverse = TRUE)) +
  theme(plot.title = element_text(size = rel(1)))
p3


p4<- ggplot(data = d2) +
  geom_mosaic(aes(weight = count, x = product( sport,asthma), fill = asthma)) +
  labs(y="Sports ", title='Sports') + guides(fill=guide_legend(title = "Asthma", reverse = TRUE)) +
  theme(plot.title = element_text(size = rel(1)))
p4


p5 <- grid_arrange_shared_legend(p3, p4, ncol = 2, nrow = 1, position = "right")
p5

p6 <- ggplot(data = d2) +
  geom_mosaic(aes(weight = count, x = product( attend_.event,sport), fill = asthma)) +
  labs( y = " Attended Events", x = "Asthma : Play Sports", title='Asthma Against Playing Sports and Attended Events') + guides(fill=guide_legend(title = "Asthma", reverse = TRUE)) +
  theme(plot.title = element_text(size = rel(1)))
p6










