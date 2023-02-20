library(sentimentr)
library(tidytext)
library(textdata)
library(syuzhet)

darwin <- base::readRDS(url("https://slcladal.github.io/data/origindarwin.rda", "rb"))
twain <- base::readRDS(url("https://slcladal.github.io/data/twainhuckfinn.rda", "rb"))
orwell <- base::readRDS(url("https://slcladal.github.io/data/orwell.rda", "rb"))
lovecraft <- base::readRDS(url("https://slcladal.github.io/data/lovecraftcolor.rda", "rb"))

head(darwin)
head(twain)
head(orwell)
head(lovecraft)

txtclean <- function(x, title){
  require(dplyr)
  require(stringr)
  require(tibble)
  x <- x %>%
    iconv(to = "UTF-8") %>%
    base::tolower() %>%
    paste0(collapse = " ") %>%
    stringr::str_squish()%>%
    stringr::str_split(" ") %>%
    unlist() %>%
    tibble::tibble() %>%
    dplyr::select(word = 1, everything()) %>%
    dplyr::mutate(novel = title) %>%
    dplyr::anti_join(stop_words) %>%
    dplyr::mutate(word = str_remove_all(word, "\\W")) %>%
    dplyr::filter(word != "")
}
darwin_clean <- txtclean(darwin, "darwin")
lovecraft_clean <- txtclean(lovecraft, "lovecraft")
orwell_clean <- txtclean(orwell, "orwell")
twain_clean <- txtclean(twain, "twain")


novels_anno <- rbind(darwin_clean, twain_clean, orwell_clean, lovecraft_clean)

novels_anno <- novels_anno %>%
  # mutate(words=n()) %>%
  left_join(tidytext::get_sentiments("nrc")) %>%
  mutate(sentiment = factor(sentiment))
  # mutate(novel = factor(novel), 
  #        sentiment = factor(sentiment))
View(novels_anno)






novels_anno %>%
  left_join(tidytext::get_sentiments("nrc")) %>%
  
  mutate(sentiment=factor(sentiment))

  
