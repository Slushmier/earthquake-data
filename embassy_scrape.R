library(rvest)
library(dplyr)

# Read unique URLs off usembassy.gov website
emb2 <- read_html("https://www.usembassy.gov/")
emb2parse <- emb2 %>% html_nodes("a") %>% html_attr("href")
emb2parse <- emb2parse[31:262]
emb2parse <- emb2parse[!emb2parse %in% c("#", NA)]

# Create empty data.frame and then loop through each URL to extract 
# information
df_all <- data.frame(Country = character(),
                     Type = character(),
                     Location = character(),
                     stringsAsFactors=FALSE)
for(i in 1:length(emb2parse)){
  emb_loop <- read_html(emb2parse[i]) %>%
    html_nodes("strong") %>%
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
df_all <- data.frame(lapply(df_all, as.character), stringsAsFactors=FALSE)
df_all$Country <- if_else(df_all$Country == "republic-of-congo", "Rwanda", df_all$Country)

df_all_2 <- df_all %>% 
  mutate(Country = case_when(
    Country == "republic-of-congo" ~ "Rwanda",
    Country == "san-marino-2" ~ "San Marino",
    TRUE ~ Country)) %>% 
  mutate(Location = str_remove_all(.$Location, "U.S. Embassy in ")) %>% 
  mutate(Location = str_remove_all(.$Location, "U.S. Embassy ")) %>% 
  mutate(Location = str_remove_all(.$Location, "U.S. Consulate General in ")) %>% 
  mutate(Location = str_remove_all(.$Location, "U.S. Consulate in ")) %>% 
  mutate(Location = str_remove_all(.$Location, "U.S. Consulate General ")) %>% 
  mutate(Location = str_remove_all(.$Location, "U.S. Consulate ")) %>% 
  mutate(Location = str_remove_all(.$Location, " Azores")) %>% 
  mutate(Country = str_replace_all(.$Country, "-", " ")) %>% 
  mutate(Country = str_to_title(.$Country)) %>% 
  mutate(Location = case_when(
    Location == "Somalia" ~ "Mogadishu",
    Country == "Croatia" ~ "Zagreb",
    Country == "Holy See" ~ "Vatican City",
    Location == "Austria" ~ "Vienna",
    Location == "Finland" ~ "Helsinki",
    Location == "Guadalajar" ~ "Guadalajara",
    Location == "Tonga" ~ "Suva",
    TRUE ~ Location
  ))
  
write_csv(df_all_2, "Data\\embassies_consulates.csv")
# Alternate URL to scrape embassies, but the list is incomplete
# 
# simple <- read_html("https://travel.state.gov/content/travel/en/us-visas/visa-information-resources/list-of-posts.html")
# df <- simple %>% html_nodes("a") %>% html_text() %>% as.data.frame()
# df <- df[64:220,]
# df <- as.character(df)
# df <- df %>% as.data.frame()
# colnames(df) <- c("Text")
# df <- df %>% dplyr::filter(Text != "" & Text != "Back to Top") %>% 
#   mutate(abriviation = str_sub(.$Text, start = -3, end = -1),
#          Text = str_sub(Text, start = 1, end = -7)) %>% 
#   arrange(Text) %>% dplyr::filter(Text != "Tashkent-DV")