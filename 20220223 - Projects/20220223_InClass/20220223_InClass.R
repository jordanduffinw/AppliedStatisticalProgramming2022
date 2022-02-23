getwd()

library(ggplot2)
ggplot(diamonds,
       aes(x = carat, y = price))+
  geom_hex()

ggsave("diamonds.pdf")
write.csv(diamonds, "diamonds.csv")

library(here)
here()
