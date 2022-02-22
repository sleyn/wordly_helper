# Wordly Helper
Shiny app aims to help choose words for WORDLY game

The app deployed on [https://sleyn.shinyapps.io/wordly_helper/](https://sleyn.shinyapps.io/wordly_helper/).

As a words database the app uses Project Guttenberg frequency list of 40000 most used words: [Project Guttenberg Wiki lists](https://en.wiktionary.org/wiki/Wiktionary%3aFrequency_lists#Project_Gutenberg).

## Interface

The interface consists in several segments:

1. Choose number of letters in a word. Currently app supports only 4, 5 and 6-letter words.
2. For each letter a Select Input menu will appear. You can select a letter **[A-Z]** if you know a letter on the selected position or **"?"** sign if letter is unkown.
3. Virtual Keyboard. The key has 3 states:
 - No icon: the letter could be in the word.
 - Red cancel icon: the letter must not be in the word.
 - Green check icon: the letter must be in the word.
4. "Search Words" button. Generates table with words satisfying all selected parameters.
5. "Reset" button. Resets all parameters to default values.
6. Table with words, their frequency rank and frequency calculated as counts per billion words.
