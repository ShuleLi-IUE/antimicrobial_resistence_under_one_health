# term_list <- read.csv("./UN/term_list_AST_new.csv")
term_list <- read.csv("/Users/lishule/Documents/namespace/ARG_surveillance/nlptask/list/term_list_methods.csv")
ast <- c(inverted_index[["ast"]], c(inverted_index[["testing"]], inverted_index[["test"]]) %>% intersect(inverted_index[["susceptibility"]])) %>% unique %>% setdiff(mask_total)
ast %>% length
inverted_index[["16s"]] %>% intersect(ast) %>% length

term_count_total <- c()
term_year_matrix    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_human    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_plant    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_animal    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_food      <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_envir    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)

for (i in 1:nrow(term_list)) {
  print(i)
  tmp <- c()
  term_splits <- term_list$Alias[i] %>% str_split(",") %>% unlist
  for (word in term_splits) {
    term_indexs <- inverted_index[[word]]
    if (term_indexs %>% length == 0) next
    tmp <- c(tmp, term_indexs)
    
  }
  tmp <- tmp %>% unique 
  # %>% intersect(ast)
  if (tmp %>% length > 0)  {
    for (k in tmp) {
      year <- final_res$Year[k]
      if (year <= 2023 & year >= 2003) {
        term_year_matrix[i, year - 2002] <- term_year_matrix[i, year - 2002] + 1
        if (k %in% p_human) term_year_matrix_human[i, year - 2002] <- term_year_matrix_human[i, year - 2002] + 1
        if (k %in% p_plant) term_year_matrix_plant[i, year - 2002] <- term_year_matrix_plant[i, year - 2002] + 1
        if (k %in% p_animal) term_year_matrix_animal[i, year - 2002] <- term_year_matrix_animal[i, year - 2002] + 1
        if (k %in% p_agri) term_year_matrix_food[i, year - 2002] <- term_year_matrix_food[i, year - 2002] + 1
        if (k %in% p_environ) term_year_matrix_envir[i, year - 2002] <- term_year_matrix_envir[i, year - 2002] + 1
      }
    }
  }
  # term_count_total <- c(term_count_total, tmp %>% length)
}

# term_df <- data.frame(term_count_total, row.names = term_list$Name)

term_year_df <- term_year_matrix %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_df) <- 2003:2023

term_year_matrix_human_df <- term_year_matrix_human %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_human_df) <- 2003:2023
term_year_matrix_plant_df <- term_year_matrix_plant %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_plant_df) <- 2003:2023
term_year_matrix_animal_df <- term_year_matrix_animal %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_animal_df) <- 2003:2023
term_year_matrix_food_df <- term_year_matrix_food %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_food_df) <- 2003:2023
term_year_matrix_envir_df <- term_year_matrix_envir %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_envir_df) <- 2003:2023
# term_df <- term_df %>% filter(term_count_total > 0)
# term_year_df <- term_year_df[rownames(term_df), ]
# term_df <- read.csv(paste0("./res_heatmap/term_cnt_", project, ".csv"), row.names = 1, check.names = F)
# term_year_df <- read.csv(paste0("./res_heatmap/term_year_", project, ".csv"), row.names = 1, check.names = F)
write.csv(term_df, paste0("./res_heatmap/term_cnt_ast_new.csv"))
# write.csv(term_Country_df, paste0("./res_heatmap/term_country_", project, ".csv"))
write.csv(term_year_df, paste0("./res_heatmap/term_year_methods.csv"))

write.csv(term_year_matrix_human_df, paste0("./res_heatmap/term_year_methods_human.csv"))
write.csv(term_year_matrix_plant_df, paste0("./res_heatmap/term_year_methods_plant.csv"))
write.csv(term_year_matrix_animal_df, paste0("./res_heatmap/term_year_methods_animal.csv"))
write.csv(term_year_matrix_food_df, paste0("./res_heatmap/term_year_methods_food.csv"))
write.csv(term_year_matrix_envir_df, paste0("./res_heatmap/term_year_methods_envir.csv"))

# write.csv(term_year_matrix_human_df, paste0("./filtration/AI/term_year_ast_new_human.csv"))
# write.csv(term_year_matrix_plant_df, paste0("./filtration/AI/term_year_ast_new_plant.csv"))
# write.csv(term_year_matrix_animal_df, paste0("./filtration/AI/term_year_ast_new_animal.csv"))
# write.csv(term_year_matrix_food_df, paste0("./filtration/AI/term_year_ast_new_food.csv"))
# write.csv(term_year_matrix_envir_df, paste0("./filtration/AI/term_year_ast_new_envir.csv"))
term_year_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_methods.csv"))
term_year_matrix_human_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_methods_human.csv"))
term_year_matrix_plant_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_methods_plant.csv"))
term_year_matrix_animal_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_methods_animal.csv"))
term_year_matrix_food_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_methods_food.csv"))
term_year_matrix_envir_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_methods_envir.csv"))

# for upset
ast_publications_human <- c()
ast_publications_animal <- c()
ast_publications_plant <- c()
ast_publications_food <- c()
ast_publications_envir <- c()

for (i in 4:4) {
  print(i)
  tmp <- c()
  term_splits <- term_list$Alias[i] %>% str_split(",") %>% unlist
  for (word in term_splits) {
    term_indexs <- inverted_index[[word]]
    if (term_indexs %>% length == 0) next
    tmp <- c(tmp, term_indexs)
    
  }
  tmp <- tmp %>% unique %>% intersect(ast)
  if (tmp %>% length > 0)  {
    for (k in tmp) {
      year <- final_res$Year[k]
      if (year <= 2023 & year >= 2003) {
        # term_year_matrix[i, year - 2002] <- term_year_matrix[i, year - 2002] + 1
        if (k %in% p_human) ast_publications_human <- c(ast_publications_human, k)
        if (k %in% p_plant) ast_publications_plant <- c(ast_publications_plant, k)
        if (k %in% p_animal) ast_publications_animal <- c(ast_publications_animal, k)
        if (k %in% p_agri) ast_publications_food <- c(ast_publications_food, k)
        if (k %in% p_environ) ast_publications_envir <- c(ast_publications_envir, k)
      }
    }
  }
  # term_count_total <- c(term_count_total, tmp %>% length)
}
library(UpSetR)
lt4Upset <- list(`Human health` = ast_publications_human,
                 `Animal health` = ast_publications_animal,
                 `Plant health` = ast_publications_plant,
                 `Food systems` = ast_publications_food,
                 `Environment` = ast_publications_envir)
# pdf("./UN/Upset_AST_categories_4.pdf", family = "ArialMT")
pdf("./filtration/AI/Upset_AST_categories_4.pdf", family = "ArialMT")
upset(fromList(lt4Upset), order.by = "freq",
      text.scale = 1,
      # mb.ratio = c(0.5, 0.3)
      )
dev.off()

## AST categories divided by income
term_year_matrix_HIC    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_UMIC    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_LMIC    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_LIC      <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix      <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)

for (i in 1:nrow(term_list)) {
  print(i)
  tmp <- c()
  term_splits <- term_list$Alias[i] %>% str_split(",") %>% unlist
  for (word in term_splits) {
    term_indexs <- inverted_index[[word]]
    if (term_indexs %>% length == 0) next
    tmp <- c(tmp, term_indexs)
    
  }
  tmp <- tmp %>% unique %>% intersect(ast)
  if (tmp %>% length > 0)  {
    for (k in tmp) {
      year <- final_res$Year[k]
      if (year <= 2023 & year >= 2003) {
        term_year_matrix[i, year - 2002] <- term_year_matrix[i, year - 2002] + 1
        if (k %in% publications_HIC) term_year_matrix_HIC[i, year - 2002] <- term_year_matrix_HIC[i, year - 2002] + 1
        if (k %in% publications_LIC) term_year_matrix_LIC[i, year - 2002] <- term_year_matrix_LIC[i, year - 2002] + 1
        if (k %in% publications_UMIC) term_year_matrix_UMIC[i, year - 2002] <- term_year_matrix_UMIC[i, year - 2002] + 1
        if (k %in% publications_LMIC) term_year_matrix_LMIC[i, year - 2002] <- term_year_matrix_LMIC[i, year - 2002] + 1
      }
    }
  }
  # term_count_total <- c(term_count_total, tmp %>% length)
}

term_year_matrix_HIC_df <- term_year_matrix_HIC %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_HIC_df) <- 2003:2023
term_year_matrix_LIC_df <- term_year_matrix_LIC %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_LIC_df) <- 2003:2023
term_year_matrix_UMIC_df <- term_year_matrix_UMIC %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_UMIC_df) <- 2003:2023
term_year_matrix_LMIC_df <- term_year_matrix_LMIC %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_LMIC_df) <- 2003:2023

term_df <- term_year_matrix %>% as.data.frame(row.names = term_list$Name)
colnames(term_df) <- 2003:2023

write.csv(term_df, paste0("./res_heatmap/term_year_ast_new.csv"))
term_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_ast_new.csv"))
# write.csv(term_Country_df, paste0("./res_heatmap/term_country_", project, ".csv"))
# write.csv(term_year_df, paste0("./res_heatmap/term_year_ast_new.csv"))

write.csv(term_year_matrix_HIC_df, paste0("./res_heatmap/term_year_ast_new_HIC.csv"))
write.csv(term_year_matrix_LIC_df, paste0("./res_heatmap/term_year_ast_new_LIC.csv"))
write.csv(term_year_matrix_UMIC_df, paste0("./res_heatmap/term_year_ast_new_UMIC.csv"))
write.csv(term_year_matrix_LMIC_df, paste0("./res_heatmap/term_year_ast_new_LMIC.csv"))

# write.csv(term_year_matrix_HIC_df, paste0("./filtration/AI/term_year_ast_new_HIC.csv"))
# write.csv(term_year_matrix_LIC_df, paste0("./filtration/AI/term_year_ast_new_LIC.csv"))
# write.csv(term_year_matrix_UMIC_df, paste0("./filtration/AI/term_year_ast_new_UMIC.csv"))
# write.csv(term_year_matrix_LMIC_df, paste0("./filtration/AI/term_year_ast_new_LMIC.csv"))
term_year_matrix_HIC_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_ast_new_HIC.csv"))
term_year_matrix_LIC_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_ast_new_LIC.csv"))
term_year_matrix_UMIC_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_ast_new_UMIC.csv"))
term_year_matrix_LMIC_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_ast_new_LMIC.csv"))

## methods list
term_list <- read.csv("./list/term_list_methods.csv")

term_year_matrix_HIC    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_UMIC    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_LMIC    <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)
term_year_matrix_LIC      <- matrix(data = 0, nrow = nrow(term_list), ncol = 21)

for (i in 1:nrow(term_list)) {
  print(i)
  tmp <- c()
  term_splits <- term_list$Alias[i] %>% str_split(",") %>% unlist
  for (word in term_splits) {
    term_indexs <- inverted_index[[word]]
    if (term_indexs %>% length == 0) next
    tmp <- c(tmp, term_indexs)
    
  }
  tmp <- tmp %>% unique
  if (tmp %>% length > 0)  {
    for (k in tmp) {
      year <- final_res$Year[k]
      if (year <= 2023 & year >= 2003) {
        if (k %in% publications_HIC) term_year_matrix_HIC[i, year - 2002] <- term_year_matrix_HIC[i, year - 2002] + 1
        if (k %in% publications_LIC) term_year_matrix_LIC[i, year - 2002] <- term_year_matrix_LIC[i, year - 2002] + 1
        if (k %in% publications_UMIC) term_year_matrix_UMIC[i, year - 2002] <- term_year_matrix_UMIC[i, year - 2002] + 1
        if (k %in% publications_LMIC) term_year_matrix_LMIC[i, year - 2002] <- term_year_matrix_LMIC[i, year - 2002] + 1
      }
    }
  }
  # term_count_total <- c(term_count_total, tmp %>% length)
}

term_year_matrix_HIC_df <- term_year_matrix_HIC %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_HIC_df) <- 2003:2023
term_year_matrix_LIC_df <- term_year_matrix_LIC %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_LIC_df) <- 2003:2023
term_year_matrix_UMIC_df <- term_year_matrix_UMIC %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_UMIC_df) <- 2003:2023
term_year_matrix_LMIC_df <- term_year_matrix_LMIC %>% as.data.frame(row.names = term_list$Name)
colnames(term_year_matrix_LMIC_df) <- 2003:2023

# write.csv(term_df, paste0("./res_heatmap/term_cnt_ast_new.csv"))
# write.csv(term_Country_df, paste0("./res_heatmap/term_country_", project, ".csv"))
# write.csv(term_year_df, paste0("./res_heatmap/term_year_ast_new.csv"))

write.csv(term_year_matrix_HIC_df, paste0("./res_heatmap/term_year_methods_HIC.csv"))
write.csv(term_year_matrix_LIC_df, paste0("./res_heatmap/term_year_methods_LIC.csv"))
write.csv(term_year_matrix_UMIC_df, paste0("./res_heatmap/term_year_methods_UMIC.csv"))
write.csv(term_year_matrix_LMIC_df, paste0("./res_heatmap/term_year_methods_LMIC.csv"))
term_year_matrix_HIC_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_methods_HIC.csv"))
term_year_matrix_LIC_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_methods_LIC.csv"))
term_year_matrix_UMIC_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_methods_UMIC.csv"))
term_year_matrix_LMIC_df %>% rowSums() %>% data.frame %>% rename(term_cnt_total = ".") %>% 
  write.csv(paste0("./res_heatmap/term_cnt_methods_LMIC.csv"))


term_year_matrix_tmp <- matrix(data = 0, nrow = 10, ncol = 21)
for (k in ast) {
  year <- final_res$Year[k]
  if (year <= 2023 & year >= 2003) {
    term_year_matrix_tmp[1, year - 2002] <- term_year_matrix_tmp[1, year - 2002] + 1
    if (k %in% p_human) term_year_matrix_tmp[2, year - 2002] <- term_year_matrix_tmp[2, year - 2002] + 1
    if (k %in% p_plant) term_year_matrix_tmp[3, year - 2002] <- term_year_matrix_tmp[3, year - 2002] + 1
    if (k %in% p_animal) term_year_matrix_tmp[4, year - 2002] <- term_year_matrix_tmp[4, year - 2002] + 1
    if (k %in% p_agri) term_year_matrix_tmp[5, year - 2002] <- term_year_matrix_tmp[5, year - 2002] + 1
    if (k %in% p_environ) term_year_matrix_tmp[6, year - 2002] <- term_year_matrix_tmp[6, year - 2002] + 1
    if (k %in% publications_HIC) term_year_matrix_tmp[7, year - 2002] <- term_year_matrix_tmp[7, year - 2002] + 1
    if (k %in% publications_LIC) term_year_matrix_tmp[8, year - 2002] <- term_year_matrix_tmp[8, year - 2002] + 1
    if (k %in% publications_UMIC) term_year_matrix_tmp[9, year - 2002] <- term_year_matrix_tmp[9, year - 2002] + 1
    if (k %in% publications_LMIC) term_year_matrix_tmp[10, year - 2002] <- term_year_matrix_tmp[10, year - 2002] + 1
  }
}
write.csv(term_year_matrix_tmp, "./res_heatmap/term_cnt_methods_tmp.csv")




methods_Country_matrix <- matrix(data = 0, nrow = nrow(term_list), ncol = nrow(country_names))
for (i in 1:nrow(term_list)) {
  if (i %% 10 == 0) print(i)
  tmp <- term_list$Alias[i] %>% str_split(",") %>% unlist %>% tolower
  args <- c()
  for (j in tmp) {
    args <- c(args, inverted_index[[j]])
  }
  args <- args %>% unique %>% setdiff(mask_total) 
  for (j in 1:nrow(country_names)) {
    tmp <- c()
    term_splits <- country_names$Alias_UpperCase[j] %>% str_split(",") %>% unlist
    for (word in term_splits) {
      term_indexs <- inverted_index_ne[[word]]
      if (term_indexs %>% length == 0) next
      tmp <- c(tmp, term_indexs)
    }
    tmp <- tmp %>% unique %>% setdiff(mask_total)

    methods_Country_matrix[i, j] = intersect(args, tmp) %>% length()
  }
}
# ARG_Country_df[i, ] = ARG_Country_matrix[i, ]
methods_Country_df <- methods_Country_matrix %>% as.data.frame(row.names = term_list$Name)
colnames(methods_Country_df) = country_names$Name
write.csv(methods_Country_df, "./res_heatmap/term_country_methods.csv")




