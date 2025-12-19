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

# 1: Отображаем файлы нашего корпуса
my_files <- list.files(pattern = "pdf", full.names = TRUE)

# 2: качаем нужный движок tesseract - французский, потому что наши тексты на французском языке
tesseract_download("fra")

files_J1 <- list.files(path = 'Bois_et_bles', pattern = "png", full.names = TRUE)
J1 <- map(files_J1, ocr, engine = tesseract('fra'))
text_conc_J1 <- str_c(J1)
writeLines(text_conc_J1, con = 'J1.txt')


text_conc_J1_clear <- text_conc_J1 |> 
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

writeLines(text_conc_J1_clear, con = 'J1_clear.txt', sep = ' ')




















text4 <- pdf_ocr_text("flattened.pdf", language="eng")

text3 <- ocr("png1.png", engine=tesseract("fra"))
text3 <- pdf_ocr_text("pdf.pdf", pages=,language="eng")
writeLines(text1, con = 'J1.txt')

engine <- tesseract::tesseract("eng")
images <- pdf_convert(pdf = "pdf.pdf", verbose=TRUE)
images <- pdf_convert(pdf = "pdf.pdf", format="png",verbose=TRUE)
pdf_info("pdf.pdf")
vapply(images, tesseract::ocr, character(1), engine = engine)
text3 <- ocr(images, engine = tesseract("eng"))

cat(text1)
#видим что-то смешное, потому что нет дорев орфографии


# укажите свой путь или одно только имя
writeLines(text1, con = "rosalia_1.txt")
#первый аргмуент - переменная в которой хранится строка, вторая - коннекшн 
# так мы сохраняем в виде текстового документа текст, который мы извлекли из пдф

#бывает так, когда пдф не распознан, тогда работаем с пдф как с картинкой
# нам нужен тесеракт, он превращает картинку в текст





writeLines(text2, con = 'rosalia_2.txt')

pdf_convert("test.pdf", 
            format = "png", 
            dpi = 300,
            pages = NULL,  # все страницы, или c(1,3,5) для конкретных
            filenames = NULL) #сюда нужен вектор с названиями такой длины, сколько картинок внутри

#формат, разрешение, какие нужны страницы, файлнеймс - он придумает сам как назватб
# можно перевести в картинку, обработать их как нам надо, чтобы было удобнее распознавать

#ПРЕПРОЦЕССИНГ ИЗОБРАЖЕНИЙ
input <- image_read("IMG_5539.jpg")
# читаем изображение

# Обработка изображения, хз просто тыкаем, можно экспериментировать
processed <- input |>
  image_deskew() |> 
  image_resize("2000x") |>          
  image_convert(colorspace = "gray") |> 
  image_trim(fuzz = 40)  |> 
  image_modulate(brightness = 120, saturation = 0)  |>   # Коррекция яркости
  image_contrast(sharpen = 1) |>    # Повышение контрастности
  image_normalize() |>             # Нормализация гистограммы
  image_despeckle()                  # Подавление шума

# ТАК МОЖНО ПРОЧИТАТЬ, НО НАМ ПОКА НЕ НАДО
# text3 <- input |> 
#   tesseract::ocr(engine = "rus") 

dir.create("orus-ground-truth/") #эта директория появляется в рабочей папке
# создаем папку с эталонными данными картинка с распознанным текстом, который к ней относится, на этом тексте машина будет учиться

#нам нужны картинки, можно разрезать текст на слова и строки, на предыдущем этапе мы сгенерировали пнг и сейчас мы будем резать их на слова
# мы скачали crop_words - скрипт, который написала О.В.
# можно самим написать функцию, положить ее в файл и самим ее вызывать

# на входе принимает путь до изображения, имя директории, куда сохраняются слова, базовая модель, пэд - белые места вокруг слова, оверрайт - их перезаписать или создать новые
# 
# crop_words <- function(image_path,
#                        out_dir = "words",
#                        lang = "rus",
#                        pad = 2L,
#                        overwrite = FALSE) 

source('crop_words.R')
dir.create('words') #создаем папку
words_data <- crop_words(image_path = "test_1.png",
                         out_dir = "words",
                         lang = "rus",
                         pad = 2L,
                         overwrite = TRUE)
words_data
#это табличка со словами, у нас лежат нарезанные слова, в табличке просто записаны эти словва

img_paths <- list.files(pattern = "png", full.names = TRUE)
img_paths

#передаем список путей, доморощенную функцию и имя папки, путь до папки, куда надо сохранить фотографии
walk(img_paths, crop_words, 
     out_dir = "orus-ground-truth")
#эта функция из пакета пуррр, пройдись по нашим изображениям и разрежь
#раньше мы взаимодействовали с одним файлом, а теперь мы эту функцию применяем ко всем файлам, говорим пройдись по файлам в папке и примени функцию кроп_вордс
#все равно что запустить кроп вордс несколько раз в ручную

#сгенерируем распознанный текст, потому что обучать можно по эталонным файлам картинка + распознанный текст

# create_gt_txt(
#   folder = "orus-ground-truth",
#   extension = "png", 
#   engine = tesseract::tesseract(language = "rus")
# )

#You can also specify engine = NULL to generate empty gt.txt files.
# три аргумента: 1)папка 2)расширение 3)енжин - движок, русская модель 
# на каждую картинку создался текстовый файл

create_gt_txt(
  folder = "orus-ground-truth",
  extension = "png",
  engine =  NULL)

correct_gt_txt()
#появляется интерактивное окно - программка для корректуры

# git clone https://github.com/tesseract-ocr/tesstrain.git
# так мы забрали репозиторий с гитхаба, там лежит файл мейк файл, для работы с которыми мы ставили мейк

#ДОПОЛНИТЕЛЬНЫЙ РАЗБОР
#чтобы нарезать на слова несколько файлов: 


img_paths <- list.files(".", pattern = "png", full.names = TRUE)
img_paths

walk(img_paths, crop_words, 
     out_dir = "../ocr/orus-ground-truth",
     overwrite = FALSE)

# walk в отличие от map_chr, который создает функцию просто проходится, ничего не сохраняет, не записываеи


