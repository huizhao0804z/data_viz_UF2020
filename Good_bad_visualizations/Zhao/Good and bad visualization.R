rm(list = ls())
library(ggplot2)
library(dplyr)
library(wesanderson)
#Clean data
tree <- read.csv("Alachua county trees.csv", header = TRUE)
class(tree)
str(tree)

#transfer to tbl_class
df <- tbl_df(tree) 
head(df)
df <- select(df,-CN) #reduce column 'CN'

#convert to factor
tree_dropna <- na.omit(df) #romove NA
tree_dropna$INVYR <- as.factor(tree_dropna$INVYR)
tree_dropna$CONDID <- as.factor(tree_dropna$CONDID)
tree_dropna$PREVCOND <- as.factor(tree_dropna$PREVCOND)
tree_dropna$STATUSCD <- as.factor(tree_dropna$STATUSCD)
tree_dropna$SPGRPCD <- as.factor(tree_dropna$SPGRPCD)
str(tree_dropna)

#bad visualization
t <- ggplot(data = tree_dropna, aes(x = HT, y = ba, colour = SPGRPCD))+
  geom_point()+
  geom_smooth(method = 'lm')
t + ggtitle("Relationship between height and basal")+
  xlab("Height of Tree (Feet)")+
  ylab("Basal of Tree (square Feet)")


#good visualization
#remove small species (sample size < 50)
tree_clean <- tree_dropna %>%
  group_by(SPGRPCD) %>%
  filter(n()>50)
#remove outline <5% and >95%
tree_clean <- tree_clean %>% filter(HT < quantile(tree_clean$HT,0.95) & 
                                    HT > quantile(tree_clean$HT,0.05))
tree_clean <- tree_clean %>% filter(ba < quantile(tree_clean$ba,0.95) & 
                                    ba > quantile(tree_clean$ba,0.05))
tree_clean <- rename(tree_clean, Species = SPGRPCD)


#plot

p <- ggplot(data = tree_clean, aes(x = HT, y = ba, color = Species))+
  #scale_color_manual(values = wes_palette('Royal1'))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = lm, formula = y ~ poly(x, 2), se = TRUE, size = 1.5)+
  labs(title = "Height-Basal Relationship",
         x = "Height of Tree (Feet)", 
         y = "Basal of Tree (Square Feet)")+
  scale_colour_manual(values = c("#3B9AB2", "#EBCC2A", "#F21A00"),
                   labels=c("Longleaf and slash pines",
                            "Loblolly and shortleaf pines",
                            "Other red oaks"))+
  theme(plot.margin = unit(rep(2,4), 'lines'),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.background = element_rect(fill = "grey92", size = 0.5))
p
