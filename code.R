
pacman::p_load('readxl','stringr','tidyverse')


# Load words

big.list <- read_excel("words_lists.xlsx", 
                          sheet = "big.list")
scrable.list <- read_excel("words_lists.xlsx", 
                          sheet = "scrable.list")
wordle.list <- read_excel("words_lists.xlsx", 
                          sheet = "wordle.list")


make.word.list <- function(df){

df$word <- tolower(df$word)
df <- cbind(
  df, 
  as.data.frame(str_split_fixed(df$word,"",5))
)

p <- c("V1","V2","V3","V4","V5")
df[p] <- lapply(df[p], as.factor)

df <- df %>% 
  group_by(V1) %>% 
  mutate(F1 = n()) %>% 
  group_by(V2) %>% 
  mutate(F2 = n()) %>% 
  group_by(V3) %>% 
  mutate(F3 = n()) %>% 
  group_by(V4) %>% 
  mutate(F4 = n()) %>% 
  group_by(V5) %>% 
  mutate(F5 = n()) %>% 
  ungroup() %>% 
  mutate(S1 = F1 + F2 + F3 + F4 + F5) %>% 
  mutate(S2 = F1 / str_count(word,as.character(V1)) +
           F2 / str_count(word,as.character(V2)) +
           F3 / str_count(word,as.character(V3)) +
           F4 / str_count(word,as.character(V4)) +
           F5 / str_count(word,as.character(V5))
         )
  

position.freq <- data.frame(letters) %>% 
  left_join(select(df,V1,F1), by = c("letters" = "V1")) %>% 
  unique() %>% 
  left_join(select(df,V2,F2), by = c("letters" = "V2")) %>% 
  unique() %>% 
  left_join(select(df,V3,F3), by = c("letters" = "V3")) %>% 
  unique() %>% 
  left_join(select(df,V4,F4), by = c("letters" = "V4")) %>% 
  unique() %>% 
  left_join(select(df,V5,F5), by = c("letters" = "V5")) %>% 
  unique() %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

position.freq %>% ggplot(aes(y=F1,x=letters)) +
  geom_bar(stat = "identity") +
  coord_flip()

df$word[which.max(df$S1)]
df$word[which.max(df$S2)]


ret <- list("Guess S1" = df$word[which.max(df$S1)],
            "Guess S2" = df$word[which.max(df$S2)],
            "Position Frequency" = position.freq,
            "Word List" = df)
return(ret)
}

make.word.list(df=big.list)$"Guess S1"


df <- make.word.list(df=big.list)$"Word List"

