##########################################
## plotting routine for
## average littersize comparisons
## created by :
## Amanda K. Weller & Olivia S. Chapman
##########################################

library(ggplot2)


ggplot(Species.littersize.means, fill= Species.littersize.means$pantheria_percent,
       aes(x = ernest2003_percent)) +
  geom_histogram(bins = 10, color = "white") + 
  labs(title = "Percent difference from Ernest (2003)",
       x = "Percent difference") + ylim(0,18)+xlim(-50,50)
ggplot(Species.littersize.means,
       aes(x = pantheria_percent)) +
  geom_histogram(bins = 10, color = "white") +
  labs(title = "Percent difference from Pantheria (2008)",
       x = "Percent difference") + ylim(0,18)+ xlim(-50,50)
ggplot(Species.littersize.means, 
       aes(x = combine_percent)) +
  geom_histogram(bins = 10, color = "white") +
  labs(title = "Percent difference from Combine",
       x = "Percent difference") + ylim(0,18)+xlim(-50,50)


Species.littersize.means$Clade <- as.factor(Species.littersize.means$Clade)
is.factor(Species.littersize.means$Clade)
levels(Species.littersize.means$Clade)

Species.littersize.means$Clade <- factor(Species.littersize.means$Clade, 
                                         levels = c("Shrews", "Rabbits and Pikas ",
                                                    "Squirrels", "Kangaroo Rats and Beavers",
                                                    "Jumping Mice", "Old World Mice", "New World Mice and Rats",
                                                    "Voles and Lemmings "))

hist1 <- ggplot(Species.littersize.means, 
                aes(x = ernest2003_percent, fill = Clade)) +
  geom_histogram(bins = 10, color = "black") + 
  labs(title = "This study vs. Ernest (2003)",
       x = "Percent Difference", y = "Count") + ylim(0,18) + xlim(-50,50) +
  scale_fill_manual(values = c("#FCBA65", "#8C9D57", "#40663F", "#497381", "#163343", 
                                        "#8D7F99", "#EC8FA3", "firebrick4")) +
                                          theme_bw() +
  theme(axis.text = element_text(size = 12)) + theme(axis.title.y = element_text(size = 12)) +
  theme(legend.position = "none")

hist2 <- ggplot(Species.littersize.means,
                aes(x = pantheria_percent, fill = Clade)) +
  geom_histogram(bins = 10, color = "black") +
  labs(title = "This study vs. PanTHERIA (2008)",
       y = "Count", x = "Percent Differece") + ylim(0,18) + xlim(-50,50) +
  scale_fill_manual(values = c("#FCBA65", "#8C9D57", "#40663F", "#497381", "#163343", 
                                        "#8D7F99", "#EC8FA3", "firebrick4")) +
                                          theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12)) + theme(axis.title.y = element_text(size = 12))

hist3 <- ggplot(Species.littersize.means,
                aes(x = combine_percent, fill = Clade)) +
  geom_histogram(bins = 10, color = "black") +
  labs(title = "This study vs. COMBINE (2021)",
       y = "Count", x = "Percent Difference") + ylim(0,18) + xlim(-50,50) +
  scale_fill_manual(values = c("#FCBA65", "#8C9D57", "#40663F", "#497381", "#163343", 
                                        "#8D7F99", "#EC8FA3", "firebrick4")) +
                                          theme_bw() +
  theme(legend.position = "right") +
  theme(axis.text = element_text(size = 12)) + theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size=12))

hist4 <- hist(Species.littersize.means$ernest2003_percent, 
              main = "Percent difference from Ernest (2003)", xlab = "Percent difference",
              ylim = c(0,20), xlim = c(-30,40))
hist5 <- hist(Species.littersize.means$pantheria_percent,
              main = "Percent difference from Pantheria (2008)",
              xlab = "Percent difference", ylim = c(0,20), xlim = c(-40,60))
hist6 <- hist(Species.littersize.means$combine_percent, 
              main = "Percent difference from Combine (2021)",
              xlab = "Percent difference", ylim = c(0,20), xlim = c(-30, 40))

par(mfrow = c(3,1))

hist1 + hist2 + hist3

hist1final <- hist1 + xlim(c(-40, 50)) + ylim(c(0,18)) 
print(hist1final)
hist2final <- hist2 + xlim(c(-40,50)) + ylim(c(0,18))
print(hist2final)
hist3final <- hist3 + xlim(c(-40,50)) + ylim(c(0,18))
print(hist3final)

library(patchwork)

hist1final + hist2final + hist3final


hist1final
hist2final
hist3final

#lower case to upper
colnames(Species.littersize.means)[2]  <- "Clade"
