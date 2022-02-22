library(dplyr)
library(readr)
library(stringr)
library(stringi)

# SCOWL
words = read_tsv('dict/scowl', col_names = c('Word'))

words$Word = iconv(words$Word, "UTF-8", "UTF-8",sub='')

words %>% mutate(Word = stri_enc_toutf8(Word)) %>% 
  distinct() %>% 
  mutate(Length = str_length(Word), Word = str_to_upper(Word)) %>% 
  filter(Length >= 4 & Length <= 6) %>%
  write_tsv('./dict/word_dict.tsv')

# Project Guttenberg (https://en.wiktionary.org/wiki/Wiktionary%3aFrequency_lists#Project_Gutenberg)
words = read_tsv('dict/PG.txt')

words %>% 
  mutate(Length = str_length(Word), Word = str_to_upper(Word)) %>% 
  filter(Length >= 4 & Length <= 6) %>%
  write_tsv('./dict/word_dict.tsv')
