mask_list = c()
terms <- read.csv("./UN/term_list.csv") 
# %>% filter(!(Name %in% mask_list))

# Country_list <- read.csv("./process/abstract_country_occurrence_s30.csv")


# project <- "agriculture"
# term_list <- read.csv(paste0("./list/term_list_", project, ".csv"))
library(stringr)

term_count_total <- c()
# term_Country_matrix <- matrix(data = 0, nrow = nrow(term_list), ncol = 30)
term_year_matrix    <- matrix(data = 0, nrow = nrow(terms), ncol = 21)

for (i in 1:nrow(terms)) {
  print(i)
  cnt = 0
  term_splits <- terms$Alias[i] %>% str_split(",") %>% unlist
  tmp <- c()
  for (word in term_splits) {
    tmp <- c(tmp, inverted_index[[word]])
  }
  tmp <- tmp %>% unique %>% setdiff(mask_total)
  if (tmp %>% length > 0) {
    for (k in tmp) {
      year <- final_res$Year[k]
      term_year_matrix[i, year - 2002] <- term_year_matrix[i, year - 2002] + 1
    }
  }
  term_count_total <- c(term_count_total, tmp %>% length)
}
term_df <- data.frame(term_count_total, row.names = terms$Name)
# term_Country_df <- term_Country_matrix %>% as.data.frame(row.names = term_list$Name)
# colnames(term_Country_df) <- Country_list$Countries
term_year_df <- term_year_matrix %>% as.data.frame(row.names = terms$Name)
colnames(term_year_df) <- 2003:2023
term_df_total <- term_year_df %>% mutate(Total = term_df$term_count_total, Group = terms$Group)
write.csv(term_df_total, "./filtration/terms_occurrence_by_year_full.csv")
# write.csv(term_df, paste0("./res/term_cnt_", project, ".csv"))
# write.csv(term_Country_df, paste0("./res/term_country_", project, ".csv"))
# write.csv(term_year_df, paste0("./res/term_year_", project, ".csv"))

library(ComplexHeatmap)
library(extrafont)
pdf("./filtration/AI/heatmap_stages.pdf", family = "ArialMT",height = 9)
cl = kmeans(data.matrix(term_year_df %>% select(-`2023`) %>% t), centers = 3)
ComplexHeatmap::pheatmap(data.matrix(term_year_df %>% select(-`2023`)),
                         color = colorRampPalette(c("navy", "white", "red"), bias = 1.01)(50),
                         scale = "row",
                         # angle_col = c("45"),
                         show_rownames = FALSE,
                         column_km = 3,
                         column_km_repeats = 100,
                         row_split = term_df_total$Group,
                         cellwidth = 15,
                         cellheight = 2,
                         column_gap = unit(c(2,2), "mm"),
                         row_gap = unit(1, "mm"),
                         legend = F,
                         # border = TRUE,
                         # left_annontation = rowAnnotation(foo = anno_block(gp = gpar(fill = 1:5))))
                         # annotation_colors = list(
                         #   Group = c("Human" = "#85B22E",
                         #             "Animal" = "#5F80B4",
                         #             "Plant" = "#E29827",
                         #             "Agri-food" = "#922927",
                         #             "Environment" = "#E8C32EFF"))
                         )
dev.off()



nck<-NbClust(scale(wine[,-1],center = T),distance = "euclidean",min.nc = 2,max.nc = 15,method = "kmeans")

# install.packages("NbClust")
library(rattle)

data(wine, package = "rattle")
library(NbClust)
dd <- data.matrix(term_year_df %>% select(-`2023`) %>% t)
nck<-NbClust(scale(term_year_df %>% select(-`2023`) %>% t, center = T),distance = "euclidean",min.nc = 2,max.nc = 15,method = "kmeans")

table(nck$Best.n[1,])
barplot(table(nck$Best.n[1,]))
scale(wine[1:3,2:4],center = T)

x<-rbind(matrix(rnorm(100,sd=0.1),ncol=2),
         matrix(rnorm(100,mean=1,sd=0.2),ncol=2),
         matrix(rnorm(100,mean=5,sd=0.1),ncol=2),
         matrix(rnorm(100,mean=7,sd=0.2),ncol=2))

res<-NbClust(x, distance = "euclidean", min.nc=2, max.nc=8, 
             method = "complete", index = "ch")
