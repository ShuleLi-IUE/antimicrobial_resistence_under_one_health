## ARG Country
# ARG_list_f <- read.csv("./UN/node_paths_risk_statistics_final.csv")$name
ARG_list_f <- read.csv("/Users/lishule/Documents/Papers/list/ARG_List_by_CARD_NLP.csv")

country_list <- read.csv("./res_heatmap/term_cnt_Country.csv")
country_names <- read.csv("/Users/lishule/Documents/Papers/list/Country_classification_by_world_bank_processed.csv") 
  # filter(Name %in% country_list$X)

ARG_Country_matrix <- matrix(data = 0, nrow = nrow(ARG_list_f), ncol = nrow(country_names))
ARG_cnt <- c()
for (i in 1:nrow(ARG_list_f)) {
  if (i %% 10 == 0) print(i)
  ARG_obj <- ARG_list_f$Alias[i] %>% str_split(",") %>% unlist %>% tolower
  args <- c()
  for (j in ARG_obj) {
    args <- c(args, inverted_index[[j]])
  }
  args <- args %>% unique %>% setdiff(mask_total) 
  # for (j in 1:nrow(country_names)) {
  #   tmp <- c()
  #   term_splits <- country_names$Alias_UpperCase[j] %>% str_split(",") %>% unlist
  #   for (word in term_splits) {
  #     term_indexs <- inverted_index_ne[[word]]
  #     if (term_indexs %>% length == 0) next
  #     tmp <- c(tmp, term_indexs)
  #   }
  #   tmp <- tmp %>% unique %>% setdiff(mask_total)                        
  #   
  #   ARG_Country_matrix[i, j] = intersect(args, tmp) %>% length()
  # }
  ARG_cnt <- c(ARG_cnt, args %>% length)
}
# ARG_Country_df[i, ] = ARG_Country_matrix[i, ]
ARG_Country_df <- ARG_Country_matrix %>% as.data.frame(row.names = ARG_list_f$ARG_Name)
colnames(ARG_Country_df) = country_names$Name
write.csv(ARG_Country_df, "./process/ARG_Country_occurrence.csv")

ARG_cnt_df <- ARG_cnt %>% as.data.frame(row.names = ARG_list_f$ARG_Name)
write.csv(ARG_cnt_df, "./process/ARG_Country_cnt.csv")

# write.csv(ARG_Country_df %>% t, "./process/ARG_Country_occurrence_t.csv")
tmp <- ARG_Country_df %>% t %>% as.data.frame %>% mutate(Name = country_names$Name) %>%
  left_join(read.csv("/Users/lishule/Documents/Papers/list/Country_classification_by_world_bank_processed.csv") %>% 
              select(Name, Code), by = "Name")
write.csv(tmp, "./process/ARG_Country_occurrence_t.csv")

tmp <- read.csv("./process/ARG_Country_occurrence_t.csv", check.names = F) 
tmp_f <- tmp %>% mutate(cnt = tmp %>% select(-Name, -Code) %>% rowSums) %>% select(c("Name", "Code", "cnt", all_of(x1)))
write.csv(tmp_f, "./process/ARG_Country_occurrence_t_s30.csv")
# new
x1 <- c("mecA","gyrA","ampc","sul1","parC","vanA","sul2","OXA-48","NDM-1","mcr-1","tetA","gyrB","QnrS","katG","QnrB","rpob","tetO","KPC-2","CTX-M-15","TolC","inhA","tetB","TEM-1","aadA2","tetW","ErmB","floR","OXA-23","dfrA1","QnrA")
# old
x2 <- c("mecA","gyrA","ampc","sul1","parC","OXA-48","NDM-1","MCR-1","sul2","vanA","tetA","katG","CTX-M-15","gyrB","KPC-2","tetB","inhA","rpob","aadA2","tetO","dfrA1","floR","ErmB","QnrS1","TEM-1","OXA-23","CMY-2","armA","SHV-12","CTX-M-14")
x2 %>% setdiff(x1)
x1 %>% setdiff(x2)
## 下面的没改 不知正误
rownames(tmp) <- tmp$Code
ARG_Country_df_t <- tmp %>% as.data.frame %>% dplyr::select(-c("Name", "Code" ,"Animal health", "Plant health", "Human health", "Agri-food systems", "Environment")) %>%
  filter(rowSums(.) >= 20)
ARG_Country_df_t_ag <- ARG_Country_df_t / rowSums(ARG_Country_df_t)
write.csv(ARG_Country_df_t_ag , "./process/ARG_Country_occurrence_t_proportion_f20.csv")


ARG_list_f <- read.csv("./UN/node_paths_risk_statistics_final.csv") %>% 
  filter(!name %in% c("Animal health", "Plant health", "Human health", "Agri-food systems", "Environment"))  
  # select(risk_score_3_1)
ARG_list_f_risk <- ARG_list_f %>% select(colnames(ARG_list_f)[grepl("^risk_", colnames(ARG_list_f))])
res <- as.matrix(ARG_Country_df_t_ag) %*% as.matrix(ARG_list_f_risk)
# View(res)
write.csv(res, "./UN/country_risk_scores.csv")
# as.data.frame(res) %>% mutate(Name = rownames(.)) %>% left_join(country_names %>% select(Name, Income_group), by = "Name") %>%
#   group_by(Income_group) %>% summarise(risk_score = median(risk_score_1_2))
   

