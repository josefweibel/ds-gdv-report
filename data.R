# Import Libraries
library(dplyr)

load_data <- function() {
  # Import Data

  #file <- 'https://swissvotes.ch/page/dataset/swissvotes_dataset.csv'
  file <- 'data.csv'
  data <- read.csv(
    file,
    header = TRUE,
    sep = ';',
    strip.white = TRUE,
    na.strings = c('NA', '.', '', '9999'),
    encoding = 'UTF-8'
  )


  # Data Wrangling

  # Remove upcoming votes
  data <- data %>%
    filter(!is.na(annahme))

  # Remove unnecessary columns
  data <- data %>%
    #select(-(X:X.63)) %>%
    select(-titel_kurz_f) %>%
    select(-(titel_off_f:swissvoteslink)) %>%
    select(-anneepolitique) %>%
    select(-(bkchrono.de:bkchrono.fr)) %>%
    select(-(legisjahr:curiavista.fr)) %>%
    #select(-(poster_ja:poster_nein)) %>%
    select(-ends_with(".gultig")) %>%
    select(-(bkresults.de:bfsmap.fr)) %>%
    select(-(p.others_yes:p.others_free)) %>%
    select(-urheber)

  rechtsform_values <- c(1:5)
  rechtsform_labels <- c(
    'Obligatorisches Referendum',
    'Fakultatives Referendum',
    'Volksinitiative',
    'Direkter Gegenvorschlag zu einer Volksinitiative',
    'Stichfrage'
  )

  categories_values <- c(1:12)
  categories_labels <- c(
    'Staatsordnung',
    'Aussenpolitik',
    'Sicherheitspolitik',
    'Wirtschaft',
    'Landwirtschaft',
    'Öffentliche Finanzen',
    'Energie',
    'Verkehr und Infrastruktur',
    'Umwelt und Lebensraum',
    'Soziale Fragen, Sozialpolitik',
    'Bildung und Forschung',
    'Kultur, Religion, Medien'
  )

  stand_values <- c(0, 1, 3)
  stand_labels <- c(
    'Mehrheit nicht erreicht',
    'Mehrheit erreicht',
    'Mehrheit nicht notwendig'
  )

  pos_values <- c(1, 2, 3)
  pos_labels <- c(
    'Befürwortend',
    'Ablehnend',
    'Keine'
  )

  annahme_values <- c(1, 0)
  annahme_labels <- c('Accepted', 'Rejected')

  # Convert values to factors
  data <- data %>%
    mutate(rechtsform = factor(rechtsform, levels = rechtsform_values, labels = rechtsform_labels)) %>%
    mutate(d1e1 = factor(d1e1, levels = categories_values, labels = categories_labels)) %>%
    mutate(d1e2 = as.factor(d1e2)) %>%
    mutate(d1e3 = as.factor(d1e3)) %>%
    mutate(d2e1 = factor(d2e1, levels = categories_values, labels = categories_labels)) %>%
    mutate(d2e2 = as.factor(d2e2)) %>%
    mutate(d2e3 = as.factor(d2e3)) %>%
    mutate(d3e1 = factor(d3e1, levels = categories_values, labels = categories_labels)) %>%
    mutate(d3e2 = as.factor(d3e2)) %>%
    mutate(d3e3 = as.factor(d3e3)) %>%
    mutate(dep = as.factor(dep)) %>%
    mutate(br.pos = factor(br.pos, levels = pos_values, labels = pos_labels)) %>%
    mutate(bv.pos = factor(bv.pos, levels = pos_values, labels = pos_labels)) %>%
    mutate(nr.pos = factor(nr.pos, levels = pos_values, labels = pos_labels)) %>%
    mutate(sr.pos = factor(sr.pos, levels = pos_values, labels = pos_labels)) %>%
    mutate_if(grepl("^p(dev)?\\.", names(.), TRUE), as.factor) %>%
    mutate(volk = factor(volk, levels = annahme_values, labels = annahme_labels)) %>%
    mutate(stand = factor(stand, levels = stand_values, labels = stand_labels)) %>%
    mutate(annahme = factor(annahme, levels = annahme_values, labels = annahme_labels)) %>%
    mutate_if(grepl("\\.annahme$", names(.), TRUE), function(x) factor(x, levels = annahme_values, labels = annahme_labels)) %>%
    separate("datum", into = c("tag","monat","jahr"), sep = "([.])", remove = FALSE, convert = TRUE) %>%
    mutate(datum = as.Date(datum, '%d.%m.%Y'))

  return(data)
}
