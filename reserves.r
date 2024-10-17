# Info: https://en.wikipedia.org/wiki/Diversity_index#Simpson_index
#       https://en.wikipedia.org/wiki/Alpha_diversity#Scale_considerations
#       https://en.wikipedia.org/wiki/Beta_diversity
#       https://en.wikipedia.org/wiki/Gamma_diversity
#
# Sample: https://training.galaxyproject.org/training-material/topics/microbiome/tutorials/diversity/tutorial.html
#
# source("C:/Projects/__Garden/papers/biodiversity/reserves/reserves.r")
#
library(data.table)
library(vegan)
library(ggplot2)
Sys.setlocale(locale = "Russian")

# Clear
rm(list = ls())

res = read.csv("C:/Projects/__Garden/papers/biodiversity/reserves/reserves.csv", encoding = "UTF-8")
res = res[,4:5]
res[res=="-"] = 0
res[res=="+"] = 1
res[res=="+ "] = 1
res[res==" + "] = 1
res <- data.frame(apply(res, 2, function(x) as.numeric(as.character(x))))
res = transpose(res)

cat("\n-- Species richness --\n\n")
S <- specnumber(res) ## rowSums(res > 0) does the same...
S = transpose(data.frame(S))
colnames(S) <- c("Visimskiy", "Zhigulyovsky")
print(S, row.names = FALSE)

res.env <- data.frame(Reserve=c("Visimskiy", "Zhigulyovsky")) # Make environment

(alpha <- with(res.env, tapply(diversity(res), Reserve, mean))) # Average
cat("\n-- Alpha diversity --\n\n")
print(alpha, row.names = FALSE)

(gamma <- with(res.env, specnumber(res, Reserve)))
cat("\n-- Gamma diversity --\n\n")
print(gamma, row.names = FALSE)

cat("\n-- Beta diversity --\n\n")
print(gamma/alpha - 1, row.names = FALSE)

cat("\n-- Diversity indices --\n")
cat("\n- Shannon\n")
d <- transpose(data.frame(diversity(res, "shannon")))
colnames(d) <- c("Visimskiy", "Zhigulyovsky")
print(d, row.names = FALSE)

cat("\n- Simpson\n")
simp <- transpose(data.frame(diversity(res, "simpson"))) 
colnames(simp) <- c("Visimskiy", "Zhigulyovsky")
print(simp, row.names = FALSE)

cat("\n- Inverse Simpson\n")
d <- transpose(data.frame(diversity(res, "inv")))
colnames(d) <- c("Visimskiy", "Zhigulyovsky")
print(d, row.names = FALSE)

cat("\n- Unbiased Simpson\n")
d <- transpose(data.frame(simpson.unb(res)))
colnames(d) <- c("Visimskiy", "Zhigulyovsky")
print(d, row.names = FALSE)

# Richness chart
data = data.frame(Reserve = names(S), Richness = transpose(S)[,1])
p <- ggplot(data, aes(x=Reserve, y=Richness, fill=Reserve)) +
       ggtitle("Species richness") +
       geom_bar(stat = "identity") +
       scale_fill_manual(values=c("#0B0", "#56B4E9", "#E69F00")) +
       theme(text=element_text(size=16, family="sans", face="italic"),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             legend.position="none" )
print(p)
