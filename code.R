
pacman::p_load('readxl','stringr','tidyverse')


# Load words


big.list <- read_excel("words_lists.xlsx", 
                          sheet = "big.list")%>% 
            select(-freq)
scrable.list <- read_excel("words_lists.xlsx", 
                          sheet = "scrable.list")%>% 
                select(-freq)
wordle.list <- read_excel("words_lists.xlsx", 
                          sheet = "wordle.list") 

frequency.table = data.frame("Letters" = letters)

df <- data.frame("word" = tolower(wordle.list$word))
df <- cbind(df,as.data.frame(str_split_fixed(df$word, "", 5)))

frequency.table %>%  
  left_join(as.data.frame(table(df$V1)),by = c("Letters"="Var1")) %>%
  rename("Pos 1" = "Freq") %>% 
  left_join(as.data.frame(table(df$V2)),by = c("Letters"="Var1")) %>% 
  rename("Pos 2" = "Freq") %>%
  left_join(as.data.frame(table(df$V3)),by = c("Letters"="Var1")) %>% 
  rename("Pos 3" = "Freq") %>%
  left_join(as.data.frame(table(df$V4)),by = c("Letters"="Var1")) %>% 
  rename("Pos 4" = "Freq") %>%
  left_join(as.data.frame(table(df$V5)),by = c("Letters"="Var1")) %>% 
  rename("Pos 5" = "Freq")

make.word.list <- function(df) {
  df$word <- tolower(df$word)
  df <- cbind(df,
              as.data.frame(str_split_fixed(df$word, "", 5)))
  
  p <- c("V1", "V2", "V3", "V4", "V5")
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
    mutate(
      S2 = F1 / str_count(word, as.character(V1)) +
        F2 / str_count(word, as.character(V2)) +
        F3 / str_count(word, as.character(V3)) +
        F4 / str_count(word, as.character(V4)) +
        F5 / str_count(word, as.character(V5))
    )
  
  
  position.freq <- data.frame(letters) %>%
    left_join(select(df, V1, F1), by = c("letters" = "V1")) %>%
    unique() %>%
    left_join(select(df, V2, F2), by = c("letters" = "V2")) %>%
    unique() %>%
    left_join(select(df, V3, F3), by = c("letters" = "V3")) %>%
    unique() %>%
    left_join(select(df, V4, F4), by = c("letters" = "V4")) %>%
    unique() %>%
    left_join(select(df, V5, F5), by = c("letters" = "V5")) %>%
    unique() %>%
    mutate_if(is.numeric, ~ replace(., is.na(.), 0))
  
  position.freq %>% ggplot(aes(y = F1, x = letters)) +
    geom_bar(stat = "identity") +
    coord_flip()
  
  df$word[which.max(df$S1)]
  df$word[which.max(df$S2)]
  
  
  ret <- list(
    "Guess S1" = df$word[which.max(df$S1)],
    "Guess S2" = df$word[which.max(df$S2)],
    "Position Frequency" = position.freq,
    "Word List" = df
  )
  return(ret)
}

guess.eval <- function(words, guess, feedback, exact) {
  word.split <- str_split_fixed(guess, "", 5)
  
  exact <- word.split[feedback==1]
  
  
  for (n in 1:5) {
    words <- words %>%
      mutate(
        "filter{n}" := case_when(
          feedback[n] == 1 ~
            .[n + 1] == word.split[n],
          feedback[n] == 0 ~
           !str_detect(.$word, word.split[n])|word.split[n] %in% exact,
          feedback[n] == -1 ~
            str_detect(.$word, word.split[n]) & .[n + 1] != word.split[n],
        )
      )
  }
  word.filter <- words%>%
    mutate(filter0 = filter1 & filter2 & filter3 & filter4 & filter5)
  words <- words %>%
    mutate(filter0 = filter1 & filter2 & filter3 & filter4 & filter5) %>%
    filter(filter0 == TRUE) %>% 
    select(word) %>%
    as.data.frame()
  return(list("Word List" = words,
              "Word Filter" = word.filter,
              "Exact" = exact))
}

get.feedback <- function(){
  Numbers<-c()
  for (i in 1:5){
    prompt_str = paste0("Enter feedback for position ",i,": ");
    num <-readline(prompt_str);
    Numbers[i]<-as.numeric(num)
  }
  return(Numbers)
}



word.list <- make.word.list(df=wordle.list)
words <- word.list$`Word List` 
guess <- word.list$`Guess S2`

exact = NULL

for(g in 1:6){
  cat(paste0("\nGuess ",g," : ",guess,"\nWords remaining: ", nrow(words),"\n" ));
  feedback = get.feedback();
  out <- guess.eval(words,guess,feedback,exact)
  df <- out$"Word List"
  exact = out$"Exact"
  view(out$"Word Filter")
  word.list <- make.word.list(df=df)
  view(word.list$"Word List")
  words <- word.list$`Word List`
  guess <- word.list$`Guess S2`
  if(nrow(words)<=1){cat("Final guess is ",guess);break}
}
