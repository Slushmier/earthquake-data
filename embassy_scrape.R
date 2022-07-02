library(rvest)
library(dplyr)

emb2 <- read_html("https://www.usembassy.gov/")
emb2parse <- emb2 %>% html_nodes("a") %>% html_attr("href")
emb2parse <- emb2parse[31:262]
emb2parse <- emb2parse[!emb2parse %in% c("#", NA)]

df_all <- data.frame(Country = character(),
                     Type = character(),
                     Location = character(),
                     stringsAsFactors=FALSE)
for(i in 1:length(emb2parse)){
  emb_loop <- read_html(emb2parse[i]) %>%
    html_nodes("a") %>%
    html_text()
  emb_logical <- emb_loop %>% str_detect("U.S. Embassy")
  cons_logical <- emb_loop %>% str_detect("U.S. Consulate")
  
  if(sum(emb_logical) > 0){
    df_emb <- data.frame(Country = str_sub(emb2parse[i], 27, -2),
                         Type = "Embassy",
                         Location = emb_loop[emb_logical])
    df_all <- rbind(df_all, df_emb)
    }
  
  if(sum(cons_logical) > 0){
    df_cons <- data.frame(Country = str_sub(emb2parse[i], 27, -2),
               Type = "Consulate",
               Location = c(emb_loop[cons_logical]))
    df_all <- rbind(df_all, df_cons)
  }
}
