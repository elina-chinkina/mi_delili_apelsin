library(tesseract)
library(qpdf)
library(pdftools)
install.packages("remotes") #технический пакет, чтобы ставить пакеты из гитхаба
remotes::install_github("arcruz0/tesseractgt")
library(tesseractgt)
library(magick)
library(purrr)
library(tidyverse)
library(tidytext)
library(udpipe)

# 1: качаем нужный движок tesseract - французский, потому что наши тексты на французском языке
tesseract_download("fra")

# 2 
recognize_and_clean <- function(directory) {
  files_Jac <- list.files(path = directory, pattern = "png", full.names = TRUE)
  Jac_recognized <- map(files_Jac, ocr, engine = tesseract('fra'))
  text_conc_Jac <- paste(Jac_recognized, collapse = " ")
  
  text_conc_Jac_clean <- text_conc_Jac |> 
    # удаляет переносы
    str_replace_all("(?<=\\p{L})-\\s*\\n\\s*(?=\\p{L})",'') |>
    # удаляет пробел+двоеточие
    str_replace_all('\\s+:', '') |> 
    # удаляет пробел + черту вертикальную
    str_replace_all('\\s+\\|', '') |> 
    # удаляет слова из водяного знака
    str_replace_all("(?im)^.*\\b\\w*sca\\w*\\b.*$",'') |> 
    # удаляет цифры
    str_replace_all('[[:digit:]]', '') |> 
    # удаляем строки, в которых только один символ, потому что видим, что таких много 
    str_replace_all('(?m)^\\s*\\S\\s*$', '') |> 
    # удаляет переносы построчные, превращает их в пробелы
    str_replace_all("\n", " ") |>
    # схлопывает множественные пробелы до одного
    str_replace_all(" +", " ") 
}

# writeLines(tmp, con = 'J_clear.txt', sep = ' ')

# создаем список папок с текстами, идем по ним, распознаем тексты-картинки с французской моделькой написанной функцией, складываем в отдельные текстовые файлы
jaccottet_dirs <- list.files('./corpus', full.names=TRUE)
# проходим мапом, возвращается символьный вектор длиной 10, где каждый элемент - отдельный текст корпуса склеенный и очищенный
data <- map(jaccottet_dirs, recognize_and_clean)

# мы подготовили тексты, теперь возьмем названия папок с текстами-картинками и создадим тиббл, в котором будут в одном столбце названия произведений, в другом - сами произведения
jaccottet_tibble <- tibble(title = jaccottet_dirs, text = data)
jaccotet_tibble_correct_titles <- jaccottet_tibble |> 
  mutate(title = str_remove(title, "./corpus/"))

# скачиваем нужную модель для лемматизации, в этой по описанию больше всего предложений и токенов, пробуем ее
udpipe_download_model(language = "french-gsd")
# загружаем модель, проводим лемматизацию (аннотируем)
french_gsd <- udpipe_load_model(file = "french-gsd-ud-2.5-191206.udpipe")
character_text <- as.character(jaccotet_tibble_correct_titles$text)
character_title <- as.character(jaccotet_tibble_correct_titles$title)

jaccottet_ann <- udpipe_annotate(french_gsd, character_text, doc_id = character_title)
anno_tbl <- as_tibble(jaccottet_ann)

# удалим стоп-слова
library(stopwords)
sw <- stopwords("fr")
anno_without_sw <- anno_tbl|> 
  filter(!lemma %in% sw)

# удалим из лемм пунктуацию
lemm_clean_withput_punct <- anno_without_sw |> 
  filter(!upos =='PUNCT')

# посмотрим, какие вообще части речи у нас встречаются
upos_tags <- lemm_clean_withput_punct |> 
  select(upos) |> 
  unique()

# уберем и другие не особо значимые слова по их upos-тегам
final_tibble <- lemm_clean_withput_punct |> 
  filter(!upos %in% c("PROPN", "PRON", "DET", "SCONJ", "CCONJ", "INTJ", "SYM","X", 'ADP'))

# составляем словарь частотных слов для Жаккоте
jaccottet_word_counts <- final_tibble  |> 
  count(lemma, sort = TRUE) 

# выдача не очень репрезентативна, нам мешают вспомогательные глаголы. Изначально мы их не убирали, потому что часть etre/avoir определялась не только как AUX, но и как смысловое VERB, теперь попробуем убрать их, чтобы получить более осмысленные результаты. За одно уберем пару других неважных слов. 
jaccottet_word_counts_2 <- final_tibble  |> 
  filter(!lemma %in% c('avoir', 'être', 'où', 'plus', 'tout', 'encore', 'autre', 'faire')) |> 
  count(lemma, sort = TRUE)

# Делаем график, получается интересно: видим много слов, связанных с темой природы, для поэтики Жаккоте это очень характерно в связи с его поэтической установкой.
jaccottet_word_counts_2 |> 
  slice_head(n = 30) |> 
  ggplot(aes(reorder(lemma, n), n, fill = lemma)) +
  geom_col(show.legend = F) + 
  coord_flip() +
  theme_light()