library(readxl)
founders<-read_excel("founders.xlsx")

names(founders)

library(ggplot2)
a<-ggplot(founders, aes(x = Born, fill = Sex)) +
  geom_bar(position = "dodge")+
  labs(x = "Year of Birth", y = "Count")+
  theme_minimal()

b<-ggplot(founders, aes(x = Born, y = sum_gencont, color = Sex)) +
  geom_point() +
  labs(x = "Year of Birth", y = "Summary genetic contribution to RP") +
  theme_minimal()

c<-ggplot(founders, aes(x = Born, y = av_gencont, color = Sex)) +
  geom_point() +
  labs(x = "Year of Birth", y = "Average genetic contribution to RP") +
  theme_minimal()


plot1_label <- a + annotate("text", x = -Inf, y = -Inf, label = "A", 
                                 hjust = 2, vjust = 0, size = 14)
plot2_label <- b + annotate("text", x = -Inf, y = -Inf, label = "B", 
                                    hjust = 2, vjust = 0, size = 14)
plot3_label <- c + annotate("text", x = -Inf, y = -Inf, label = "B", 
                            hjust = 2, vjust = 0, size = 14)

library(gridExtra)
grid.arrange(a, c, ncol=1)


