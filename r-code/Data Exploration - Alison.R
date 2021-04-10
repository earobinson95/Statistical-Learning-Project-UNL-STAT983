winequality <- read.csv("C:/Users/AlisonPC/OneDrive - University of Nebraska-Lincoln/Documents/Statistical-Learning-Project-UNL-STAT983/data/winequality_binary.csv")
winequality <- winequality[,-1]

#Class Distribution
winequality %>%
  mutate(qualityclass = factor(qualityclass, levels = c("Low", "Normal", "High"))) %>%
  group_by(type, qualityclass) %>%
  summarise(count = n()) %>%
  mutate(prop = count/sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = qualityclass, y = prop, fill = qualityclass, label = paste(100*round(prop, 3), "%", sep = ""))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(vjust = -0.5, size = 3) +
  facet_grid(~type) +
  theme_bw() +
  theme(aspect.ratio = 0.8) +
  scale_fill_viridis_d(begin = 0.4, end = 0.8) +
  scale_x_discrete("Quality Class \n (Low - 3, 4; Normal - 5,6,7; High - 8, 9)") +
  scale_y_continuous("Proportion", limits = c(0, 1.1), breaks = seq(0, 1, 0.2), labels = scales::percent)


#Correlation Plot
corr <- round(cor(winequality[c(1, 4:14)]), 3)
p.mat <- cor_pmat(winequality[c(1, 4:14)])
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           outline.col = "white",
           colors = c("#6D9EC1", "white", "#E46726"),
           p.mat = p.mat,
           lab = TRUE,
           insig = "blank")



#Boxplot
winequality_bp <- winequality[,-c(1,15)]
ggplot(data_long, aes(x = qualityclass, y = value)) +            
  geom_boxplot() +
  facet_wrap(~variable, scales = "free")


#Distribution of Normal vs Low/High
winequality %>%
  mutate(binary_class = factor(binary_class )) %>%
  group_by(type, binary_class ) %>%
  summarise(count = n()) %>%
  mutate(prop = count/sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = binary_class , y = prop, fill = binary_class )) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_bw() +
  theme(aspect.ratio = 0.8) +
  scale_fill_viridis_d(end = 0.75) +
  scale_x_discrete("NonRare (0) vs Rare Class (1)") +
  scale_y_continuous("Proportion", limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent)


table(winequality$qualityclass)
table(winequality$binary_class)
