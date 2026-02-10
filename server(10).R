# Projet: Webscraping
# Objet: server.R
# Auteurs:
# - Nom1, Prénom1
# - Nom2, Prénom2
# - Nom3, Prénom3
# - Nom4, Prénom4
# - Nom5, Prénom5

###############################################################################.
# SETUP ------------------------------------------------------------------------
###############################################################################.
library(shiny)
library(data.table)
library(dplyr)
library(stringr)
library(plotly)
library(readxl)
library(leaflet)

# Chargement des données
jobs_df <- data.table::fread("www/data_jobs.csv")
sector_map <- data.table::fread("www/Sector_category.csv", sep = ";", encoding = "UTF-8")

# 1) Corrige un éventuel BOM dans le nom de colonne "Sector"
nm <- names(sector_map)
if (!"Sector" %in% nm) {
  i <- which(grepl("Sector", nm, fixed = TRUE))[1]
  if (length(i) == 1 && !is.na(i)) data.table::setnames(sector_map, nm[i], "Sector")
}

# 2) Nettoyage robuste des textes (espaces, NBSP)
clean_txt <- function(x){
  x <- as.character(x)
  x <- gsub("\u00A0", " ", x)      # espace insécable
  x <- trimws(x)
  x
}

jobs_df[,    Sector := clean_txt(Sector)]
sector_map[, Sector := clean_txt(Sector)]
sector_map[, Category := clean_txt(Category)]

# Sécurité noms colonnes
stopifnot(all(c("Sector", "Category") %in% names(sector_map)))
stopifnot("Sector" %in% names(jobs_df))
# Merge : on ajoute Category à jobs_df
jobs_df <- merge(
  jobs_df,
  sector_map,
  by = "Sector",
  all.x = TRUE,
  sort = FALSE
)

# Sécurité : tout doit être catégorisé
if (any(is.na(jobs_df$Category))) {
  missing <- jobs_df[is.na(Category), unique(Sector)]
  stop("Secteurs sans catégorie dans le mapping :\n", paste(missing, collapse = "\n"))
}


###############################################################################.
# HELPERS ----------------------------------------------------------------------
###############################################################################.
# Vérifie pour chaque offre (src_vec) si au moins une de ses sources (séparées par split_tokens)
# est présente dans la liste des sources sélectionnées (selected_sources).
match_any_source <- function(src_vec, selected_sources){
  if (is.null(selected_sources) || length(selected_sources) == 0) return(rep(FALSE, length(src_vec)))
  sel <- tolower(trimws(as.character(selected_sources)))
  vapply(src_vec, function(x){
    toks <- tolower(trimws(split_tokens(x)))  # "LinkedIn, Indeed" -> c("linkedin","indeed")
    any(toks %in% sel)
  }, logical(1))
}

# Pastilles : classe selon actif (bleu) / inactif (gris)
pill_cls <- function(active) paste("pill", if (isTRUE(active)) "blue" else "gray")

# Vérifie si un token (ex: une compétence) correspond à l'un des termes sélectionnés par l'utilisateur.
token_in_selected <- function(token, selected_terms){
  if (is.null(selected_terms) || length(selected_terms) == 0) return(FALSE)
  tok <- norm_txt(token)
  any(vapply(selected_terms, function(one){
    one <- norm_txt(one)
    nzchar(one) && (tok == one || str_detect(tok, fixed(one)) || str_detect(one, fixed(tok)))
  }, logical(1)))
}

# Vérifie si une colonne spécifique (col) existe dans les noms de colonnes du dataframe (df).
has_col <- function(df, col) col %in% names(df)

# Normalise une entrée texte pour faciliter les comparaisons : convertit en chaîne,
# supprime les espaces inutiles (début/fin) et passe tout en minuscules.
norm_txt <- function(x) tolower(trimws(as.character(x)))

# Traduit le texte de l'interface (ex: "Date croissant") en code interne (ex: "date_asc") 
normalize_sort_mode <- function(x){
  if (is.null(x) || length(x) == 0) return("relevance")
  xx <- tolower(trimws(as.character(x[1])))
  xx <- gsub("\\s+", " ", xx)
  if (xx == "recent") return("date_desc")
  if (xx == "salary") return("salary_desc")
  # Traduction des libellés UI (Date)
  if (xx %in% c("date : ordre décroissant","date : ordre decroissant","date décroissant","date decroissant")) return("date_desc")
  if (xx %in% c("date : ordre croissant","date croissant")) return("date_asc")
  # Traduction des libellés UI (Salaire)
  if (xx %in% c("salaire : ordre décroissant","salaire : ordre decroissant","salaire décroissant","salaire decroissant")) return("salary_desc")
  if (xx %in% c("salaire : ordre croissant","salaire croissant")) return("salary_asc")
  # Par défaut : pertinence
  if (xx %in% c("pertinence","relevance")) return("relevance")
  
  # Sinon on renvoie tel quel (si tu utilises déjà date_desc, salary_asc, etc.)
  xx
}

# Détecte les textes "vides" : Renvoie TRUE si c'est "non spécifié", vide ou NA
is_missing_txt <- function(x){
  x <- tolower(trimws(as.character(x)))
  is.na(x) | x == "" | x %in% c("non spécifié","non specifie","non spécifie",
                                "non déterminé","non determine","non determinée",
                                "indéterminée","indeterminee",
                                "non déterminée","non determinee"
  )
}

# Gère les espaces, virgules et nettoie les caractères invisibles
num_clean <- function(x) {
  x <- as.character(x)
  x[is_missing_txt(x)] <- NA_character_
  x <- gsub("\u00A0", " ", x) # Enlève espace insécable
  x <- gsub("\\s+", "", x) # Enlève tous les espaces
  x <- gsub(",", ".", x) # Remplace virgule par point
  x <- gsub("[^0-9.]", "", x)  # Ne garde que chiffres et point
  suppressWarnings(as.numeric(x))
}

# Normalisation des coordonnées géographiques
if (has_col(jobs_df, "Latitude")) {
  jobs_df[, Latitude := num_clean(Latitude)]
}
if (has_col(jobs_df, "Longitude")) {
  jobs_df[, Longitude := num_clean(Longitude)]
}

# Date du jour (Paris)
today_paris <- function() as.Date(Sys.time(), tz = "Europe/Paris")

# Capable de lire : "Hier", "Il y a 3 jours", "20/01/2024"
parse_publish_date <- function(x, today = Sys.Date()) {
  # Si c'est déjà une date, on ne touche à rien
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  
  xx <- trimws(as.character(x))
  xx[is.na(xx)] <- NA_character_
  
  low <- tolower(xx)
  low[low %in% c("", "na", "n/a", "null", "none", "nan", "non spécifié", "non specifie",
                 "indéterminé", "indetermine", "unknown", "0", "0000-00-00")] <- NA_character_
  xx[is.na(low)] <- NA_character_
  
  out <- as.Date(rep(NA_character_, length(xx)))
  m_today <- !is.na(low) & grepl("aujourd|today", low)
  out[m_today] <- today
  
  m_yest <- !is.na(low) & grepl("hier|yesterday", low)
  out[m_yest] <- today - 1
  
  # "Il y a X jours"
  m_ago <- !is.na(low) & grepl("il y a\\s*\\d+\\s*jour|\\d+\\s*days?\\s*ago", low)
  if (any(m_ago)) {
    n <- suppressWarnings(as.integer(gsub(".*?(\\d+).*", "\\1", low[m_ago])))
    out[m_ago] <- today - n
  }
  
  # Formats standards (ex: DD/MM/YYYY)
  # On nettoie ce qui traîne après la date (ex: l'heure "T10:00:00")
  xx2 <- xx
  xx2 <- sub("T.*$", "", xx2)
  xx2 <- sub("\\s+.*$", "", xx2)
  
  # formats classiques
  fmts <- c(
    "%d/%m/%Y", "%d/%m/%y",
    "%Y-%m-%d",
    "%d-%m-%Y", "%d-%m-%y",
    "%Y/%m/%d",
    "%m/%d/%Y", "%m/%d/%y"  # utile si une source te renvoie du US
  )
  
  for (fmt in fmts) {
    m <- is.na(out) & !is.na(xx2)
    if (!any(m)) break
    out[m] <- suppressWarnings(as.Date(xx2[m], format = fmt))
  }
  
  mnum <- is.na(out) & !is.na(xx2) & grepl("^\\d+(\\.\\d+)?$", xx2)
  if (any(mnum)) {
    n <- suppressWarnings(as.numeric(xx2[mnum]))
    ms <- is.finite(n) & n > 1e12
    if (any(ms)) out[mnum][ms] <- as.Date(as.POSIXct(n[ms] / 1000, origin = "1970-01-01", tz = "UTC"))
    sec <- is.finite(n) & n > 1e9 & n <= 1e12
    if (any(sec)) out[mnum][sec] <- as.Date(as.POSIXct(n[sec], origin = "1970-01-01", tz = "UTC"))
    # Format Excel (nombre de jours)
    day <- is.finite(n) & n > 0 & n <= 60000
    if (any(day)) {
      n2 <- n[day]
      guess_epoch <- n2 < 30000
      dd <- as.Date(rep(NA_real_, length(n2)), origin = "1970-01-01")
      dd[!guess_epoch] <- as.Date(n2[!guess_epoch], origin = "1899-12-30")
      out[mnum][day] <- dd
    }
  }
  # sécurité : on met à NA les dates futures ou trop anciennes
  out[out > today] <- NA
  out[out < (today - 3650)] <- NA   # > 10 ans probablement une mauvaise conversion
  out
}

if (has_col(jobs_df, "Publish_Date")) {
  jobs_df[, Publish_Date := parse_publish_date(Publish_Date)]
}

# Transforme une date en texte relatif : "aujourd'hui", "il y a 1 jour", "il y a X jours".
# Renvoie vide ("") si la date est inconnue ou dans le futur.
posted_ago_txt <- function(publish_date, today = Sys.Date()) {
  if (is.null(publish_date) || length(publish_date) == 0) return("")
  pd <- as.Date(publish_date[1])
  if (is.na(pd)) return("")
  # Calcul du nombre de jours d'écart
  d <- as.integer(today - pd)
  # Si la date est infinie ou dans le futur, on ne l'affiche pas
  if (!is.finite(d)) return("")
  # Formatage du texte selon l'écart
  if (d < 0) return("")
  if (d == 0) return("aujourd’hui")
  if (d == 1) return("il y a 1 jour")
  paste0("il y a ", d, " jours")
}

# Découpe une liste de mots
# Transforme "Java, SQL; Python" en vecteur c("Java", "SQL", "Python")
split_tokens <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(character(0))
  out <- unlist(strsplit(x, "[,;|]"))
  out <- trimws(out)
  out[out != ""]
}

# Retourne TRUE si le texte contient au moins un des mots cherchés
match_any_term <- function(vec, terms) {
  if (is.null(terms) || length(terms) == 0) return(rep(TRUE, length(vec)))
  v <- tolower(ifelse(is.na(vec), "", as.character(vec)))
  t <- tolower(terms)
  # Vérifie la présence de chaque terme
  Reduce(`|`, lapply(t, function(one) str_detect(v, fixed(one))))
}

# BLOC RECHERCHE ###############################################################
# "Intitulé du poste, mots clefs, entreprise..." -------------------------------

# Liste métiers/mots-clés
ROLE_CHOICES_GROUPED <- list(
  "Data" = c(
    "Data Analyst","Data Scientist","Data Engineer","Analytics Engineer",
    "Data Consultant","Architecte Data","Big Data Engineer","Database Engineer / DBA",
    "Lead Data Engineer","Lead Data Scientist","Head of Data"
  ),
  "IA / ML" = c(
    "IA","AI Engineer / Ingénieur IA","Machine Learning Engineer","MLOps Engineer",
    "NLP","Computer Vision","LLM / GenAI","Prompt Engineering","RAG"
  ),
  "BI / Analytics" = c(
    "Business Intelligence (BI)","BI Engineer","BI Analyst","Business Analyst",
    "Power BI","Tableau","Looker","Qlik"
  ),
  "Cloud / Ops" = c(
    "Cloud Engineer","DevOps Engineer","DataOps","Platform Engineer","SRE","Kubernetes"
  ),
  "Dev" = c(
    "Software Engineer","Backend Engineer","Frontend Engineer","Fullstack","Tech Lead"
  ),
  "Technologies" = c(
    "Python","SQL","AWS","Azure","GCP","Kafka","Airflow","dbt","Terraform"
  )
)

# Entreprises (depuis jobs_df$Company)
COMPANY_CHOICES <- character(0)
if (has_col(jobs_df, "Company")) {
  cc <- trimws(as.character(jobs_df$Company))
  cc <- cc[!is.na(cc) & nzchar(cc) & !is_missing_txt(cc)]
  COMPANY_CHOICES <- sort(unique(cc))
}

SEARCH_CHOICES <- c(ROLE_CHOICES_GROUPED, list("Entreprises" = COMPANY_CHOICES))

COMPANY_NORM_SET <- unique(norm_txt(COMPANY_CHOICES))

###############################################################################.

# Détection du statut Télétravail : Convertit 0/1 en TRUE/FALSE (NA-safe)
is_remote_true <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  !is.na(v) & v == 1
}


pick_col <- function(job, cols) {
  for (cc in cols) {
    # Si la colonne existe dans le tableau...
    if (cc %in% names(job)) {
      v <- as.character(job[[cc]])
      # ...et qu'elle contient au moins une vraie valeur (pas juste NA ou vide)
      ok <- !is.na(v) & nzchar(trimws(v))
      if (any(ok)) return(v) # On a trouvé, on renvoie cette colonne
    }
  }
  # Si aucune colonne valide n'est trouvée, on renvoie du vide 
  if (is.data.frame(job) && nrow(job) > 1) return(rep("", nrow(job)))
  ""
}

# Utilité : normalise les noms de source vers un libellé canonique
norm_source <- function(x){
  x <- tolower(trimws(as.character(x)))
  x <- gsub("\\s+", " ", x)
  
  if (x %in% c("linkedin","linked in")) return("LinkedIn")
  if (x %in% c("indeed")) return("Indeed")
  
  # wttj (plusieurs écritures possibles)
  if (x %in% c("wttj","welcome to the jungle") || stringr::str_detect(x, "welcome")) {
    return("Welcome to the Jungle")
  }
  
  NA_character_
}

# Récupère la liste propre des plateformes (ex: c("LinkedIn", "Indeed"))
# en cherchant dans plusieurs colonnes possibles et en nettoyant les données.
get_offer_sources <- function(job_row){
  raw <- pick_col(job_row, "occurrence_sites")
  toks <- split_tokens(raw)
  toks <- toks[!is_missing_txt(toks) & nzchar(toks)]
  unique(na.omit(vapply(toks, norm_source, character(1))))
}

# Affichage des Logos
source_logo_tag <- function(src){
  if (is.null(src) || length(src) == 0) return(NULL)
  src <- as.character(src[1])
  if (is.na(src) || !nzchar(trimws(src))) return(NULL)
  
  cls_extra <- dplyr::case_when(
    src == "LinkedIn" ~ "is-linkedin",
    src == "Indeed" ~ "is-indeed",
    src == "Welcome to the Jungle" ~ "is-wttj",
    TRUE ~ ""
  )
  if (src == "LinkedIn") {
    return(tags$img(
      src   = "icons/LinkedIn-Logo.wine.svg",
      class = paste("src-logo", cls_extra),
      title = "LinkedIn"
    ))
  }
  if (src == "Indeed") {
    return(tags$img(
      src   = "icons/indeed-logo.jpg",
      class = paste("src-logo", cls_extra),
      title = "Indeed"
    ))
  }
  if (src == "Welcome to the Jungle") {
    return(tags$img(
      src   = "icons/wttj_logo.svg",
      class = paste("src-logo", cls_extra),
      title = "Welcome to the Jungle"
    ))
  }
  NULL
}

# Affiche les logos des sources
render_source_logos <- function(sources){
  if (is.null(sources) || length(sources) == 0) return(NULL)
  div(class = "offer-sources",
      lapply(sources, source_logo_tag)
  )
}

# Utilité : "Voir l'offre : logos cliquables vers l'URL
render_source_logo_links <- function(sources, url){
  if (is.null(sources) || length(sources) == 0) return(NULL)
  url <- as.character(url %||% "")
  url <- trimws(url[1])
  if (!nzchar(url)) return(NULL)
  
  div(class = "offer-sources-links",
      lapply(sources, function(s){
        tags$a(
          href = url, target = "_blank",
          class = "src-link",
          source_logo_tag(s)
        )
      })
  )
}

# "Ville, département, code postal..." #########################################
# Convertit tout format (75001.0, 75001, "75001") en chaîne de 5 caractères ("75001").
format_postal_code <- function(x) { 
  if (is.null(x) || length(x) == 0) return("") 
  x <- as.character(x[1]) 
  x <- trimws(x) 
  if (is_missing_txt(x)) return("") 
  
  x_num <- suppressWarnings(as.numeric(x)) 
  if (is.finite(x_num)) { 
    cp <- as.integer(round(x_num)) 
    if (is.finite(cp) && cp >= 0 && cp <= 99999) { 
      return(sprintf("%05d", cp)) 
    } 
  } 
  x_digits <- gsub("[^0-9]", "", x) 
  if (nchar(x_digits) > 0) { 
    if (nchar(x_digits) < 5) x_digits <- sprintf("%05s", x_digits) 
    return(x_digits) 
  } 
  "" 
}
get_cp_col <- function(df){
  "Code_Postal"
}

# Nettoie la chaîne "Location" brute : retire tout ce qui est après une virgule,
# supprime les codes postaux inclus dans le texte et les parenthèses.
clean_city_from_location <- function(loc_raw){
  x <- trimws(as.character(loc_raw))
  x[is.na(x)] <- ""
  x <- sub(",.*$", "", x)           # garde avant virgule
  x <- gsub("\\b\\d{5}\\b", "", x)  # retire CP dans le texte si présent
  x <- gsub("[()]", " ", x)
  x <- trimws(gsub("\\s+", " ", x))
  if (is_missing_txt(x) || !nzchar(x)) return("")
  x
}

# Transforme "Paris" en "paris" pour éviter les doublons.
norm_city_key <- function(x){
  x <- tolower(trimws(as.character(x)))
  x <- gsub("_", " ", x)
  x <- gsub("[’`´]", "'", x)
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  } else {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  }
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Met la première lettre en majuscule (ex: "paris" -> "Paris").
pretty_city <- function(x){
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  if (!nzchar(x)) return("")
  if (requireNamespace("stringr", quietly = TRUE)) {
    x <- stringr::str_to_title(x)
  }
  x
}

# Génère pour chaque ligne du tableau la chaîne "Ville (CP)" ou "Ville".
# C'est ce vecteur qui sera comparé au choix de l'utilisateur dans le filtre.
loc_key_vec <- function(df){
  if (!has_col(df, "Location")) return(rep("", nrow(df)))
  
  loc_raw <- trimws(as.character(df$Location))
  loc_raw[is.na(loc_raw)] <- ""
  
  # CP depuis Code_Postal uniquement
  cp <- rep("", length(loc_raw))
  if (has_col(df, "Code_Postal")) {
    cp <- vapply(df$Code_Postal, format_postal_code, character(1))
  }
  
  city <- vapply(loc_raw, clean_city_from_location, character(1))
  city <- vapply(city, pretty_city, character(1))
  city[grepl("^\\d+$", city)] <- ""
  
  out <- ifelse(nzchar(city) & nzchar(cp), paste0(city, " (", cp, ")"),
                ifelse(nzchar(city), city, ""))
  
  out
}

# Crée la liste des choix uniques pour le menu déroulant.
build_loc_choices <- function(df){
  if (!has_col(df, "Location")) return(setNames(character(0), character(0)))
  
  loc_raw <- trimws(as.character(df$Location))
  loc_raw[is.na(loc_raw)] <- ""
  
  cp <- rep("", length(loc_raw))
  if (has_col(df, "Code_Postal")) {
    cp <- vapply(df$Code_Postal, format_postal_code, character(1))
  }
  
  city <- vapply(loc_raw, clean_city_from_location, character(1))
  city <- vapply(city, pretty_city, character(1))
  city[grepl("^\\d+$", city)] <- ""
  
  keep <- nzchar(city)
  city <- city[keep]
  cp   <- cp[keep]
  
  dt <- data.table::data.table(
    city     = city,
    city_key = vapply(city, norm_city_key, character(1)),
    cp       = ifelse(nzchar(cp), cp, NA_character_)
  )
  
  dt[, has_cp := any(!is.na(cp)), by = "city_key"]
  dt <- dt[(!has_cp & is.na(cp)) | (has_cp & !is.na(cp))]
  
  dt <- unique(dt, by = c("city_key", "cp"))
  
  lab <- ifelse(!is.na(dt$cp), paste0(dt$city, " (", dt$cp, ")"), dt$city)
  lab <- sort(unique(lab))
  
  setNames(lab, lab) 
}

###############################################################################.

# Vérifie si un token (ex : compétence) matche avec une liste de termes.
token_matches_any <- function(token, terms) {
  if (is.null(terms) || length(terms) == 0) return(FALSE)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (length(terms) == 0) return(FALSE)
  tok <- norm_txt(token)
  any(vapply(terms, function(one) {
    one <- norm_txt(one)
    nzchar(one) && (tok == one || str_detect(tok, fixed(one)) || str_detect(one, fixed(tok)))
  }, logical(1)))
}

# Renvoie TRUE si le tableau contient au moins une colonne de salaire ou de taux horaire.
salary_cols_ok <- function(df){
  ("Salary_Min" %in% names(df)) || ("Salary_Max" %in% names(df)) ||
    ("Hourly_Rate_Min" %in% names(df)) || ("Hourly_Rate_Max" %in% names(df))
}

# Récupère Min/Max et comble les trous (si Min est vide, on utilise Max et vice-versa).
get_monthly_range <- function(df){
  smin <- if ("Salary_Min" %in% names(df)) num_clean(df$Salary_Min) else rep(NA_real_, nrow(df))
  smax <- if ("Salary_Max" %in% names(df)) num_clean(df$Salary_Max) else rep(NA_real_, nrow(df))
  smin2 <- ifelse(is.na(smin) & !is.na(smax), smax, smin)
  smax2 <- ifelse(is.na(smax) & !is.na(smin), smin, smax)
  list(min = smin2, max = smax2)
}

# Idem que ci-dessus, mais pour les colonnes "Hourly_Rate" (pour les freelances).
get_hourly_range <- function(df){
  hmin <- if ("Hourly_Rate_Min" %in% names(df)) num_clean(df$Hourly_Rate_Min) else rep(NA_real_, nrow(df))
  hmax <- if ("Hourly_Rate_Max" %in% names(df)) num_clean(df$Hourly_Rate_Max) else rep(NA_real_, nrow(df))
  hmin2 <- ifelse(is.na(hmin) & !is.na(hmax), hmax, hmin)
  hmax2 <- ifelse(is.na(hmax) & !is.na(hmin), hmin, hmax)
  list(min = hmin2, max = hmax2)
}

# Regarde si la colonne "Contract_Type" contient "freelance".
is_freelance_row <- function(df){
  if (!("Contract_Type" %in% names(df))) return(rep(FALSE, nrow(df)))
  ct <- tolower(trimws(as.character(df$Contract_Type)))
  !is.na(ct) & ct == "freelance"
}

# Génère le texte "Min - Max" ou juste "Min".
format_pay <- function(job_row){
  is_fr <- is_freelance_row(job_row)[1]
  # Choix de la source de données (Horaire vs Mensuel)
  rr <- if (is_fr) get_hourly_range(job_row) else get_monthly_range(job_row)
  unit <- if (is_fr) "h" else "mois"
  smin <- rr$min[1]; smax <- rr$max[1]
  if (!is.finite(smin) && !is.finite(smax)) return(list(txt = "", unit = unit))
  if (!is.finite(smin)) smin <- smax
  if (!is.finite(smax)) smax <- smin
  # Affichage : "2000" ou "2000 - 3000"
  txt <- if (smin == smax) as.character(smin) else paste0(smin, " - ", smax)
  list(txt = txt, unit = unit)
}


# Harmonisation ###############################################################
# Harmonisation : Hard skills ##################################################
library(data.table)
library(stringr)

# Enlève majuscules, underscores, espaces en trop (+ accents si possible)
normalize_skill <- function(x){
  x <- as.character(x)
  x <- gsub("\uFEFF", "", x)              # BOM éventuel
  x <- str_replace_all(x, "_", " ")
  x <- str_to_lower(x)
  x <- str_squish(x)
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- str_to_lower(str_squish(x))
  }
  x
}

# Lit un fichier "hardskills.txt" de la forme :
# "python": "python",
# "python programming": "python",
read_hardskills_synonyms <- function(path){
  if (!file.exists(path)) stop("Fichier introuvable: ", path)
  
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- gsub("\uFEFF", "", lines)          # BOM éventuel
  lines <- trimws(lines)
  
  # Retire lignes vides + commentaires (# ou //)
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^\\s*#|^\\s*//", lines)]
  
  # On extrait "from" : "to" (avec ou sans virgule finale)
  rx <- '^\\s*"(.*?)"\\s*:\\s*"(.*?)"\\s*,?\\s*$'
  ok <- grepl(rx, lines)
  if (!any(ok)) stop("Aucune règle reconnue dans ", path,
                     "\nAttendu: \"alias\": \"canon\", (1 par ligne)")
  
  from_vec <- sub(rx, "\\1", lines[ok])
  to_vec   <- sub(rx, "\\2", lines[ok])
  
  dt <- data.table(from = from_vec, to = to_vec)
  dt[, from := normalize_skill(from)]
  dt[, to   := normalize_skill(to)]
  dt <- dt[!is.na(from) & nzchar(from) & !is.na(to) & nzchar(to)]
  
  # Doublons: on garde la dernière occurrence
  dt[, line_id := .I]
  dt <- dt[order(line_id)]
  dt <- dt[, .SD[.N], by = "from"]
  dt[, line_id := NULL]
  
  dt
}

# ---- Charge le mapping depuis le txt
syn_path <- "www/hardskills.txt"
skill_syn <- read_hardskills_synonyms(syn_path)

# Map alias -> canon
.syn_map <- setNames(skill_syn$to, skill_syn$from)

apply_synonyms <- function(x){
  x <- normalize_skill(x)
  ifelse(x %in% names(.syn_map), unname(.syn_map[x]), x)
}

# Filet de sécurité (optionnel, tu peux garder)
collapse_contains_rules <- function(x){
  x <- normalize_skill(x)
  x <- ifelse(str_detect(x, "\\bpython\\b"), "python", x)
  x <- ifelse(str_detect(x, "\\bpower\\s*bi\\b"), "power bi", x)
  x <- ifelse(str_detect(x, "\\bci\\s*/\\s*cd\\b|\\bci\\s*cd\\b|\\bci_cd\\b"), "ci/cd", x)
  x <- ifelse(str_detect(x, "\\bscikit\\b|\\bsklearn\\b"), "scikit-learn", x)
  x
}

canon_basic <- function(x){
  x <- apply_synonyms(x)
  x <- collapse_contains_rules(x)
  x
}

# joli libellé (affichage)
# Transforme l'ID technique ("sql") en nom propre ("SQL") pour l'interface.
pretty_skill <- function(canon){
  x <- normalize_skill(canon)
  exact <- c(
    "sql"="SQL","nlp"="NLP","aws"="AWS","gcp"="GCP",
    "ci/cd"="CI/CD","etl"="ETL","elt"="ELT","ocr"="OCR",
    "llm"="LLM","gpt"="GPT","github"="GitHub","gitlab"="GitLab",
    "c#"="C#","c++"="C++","r"="R","dbt"="dbt","pyspark"="PySpark",
    "opencv"="OpenCV","xgboost"="XGBoost","lightgbm"="LightGBM","catboost"="CatBoost",
    "scikit-learn"="Scikit-learn","tensorflow"="TensorFlow","pytorch"="PyTorch",
    "power bi"="Power Bi","bi tools"="BI Tools"
  )
  out <- ifelse(x %in% names(exact), unname(exact[x]), str_to_title(x))
  out
}

# Scanne toutes les offres pour lister toutes les compétences uniques existantes.
hard_raw_tokens <- split_tokens(jobs_df$Hard_Skills)
hard_raw_tokens <- hard_raw_tokens[!is_missing_txt(hard_raw_tokens)]

# Crée la table de référence
hard_ref <- data.table(raw = hard_raw_tokens)
hard_ref[, canon := canon_basic(raw)]
hard_ref <- hard_ref[!is.na(canon) & nzchar(canon)]
hard_ref <- unique(hard_ref[, .(canon)])
hard_ref[, label := pretty_skill(canon)]
setorder(hard_ref, label)

# alias -> canon (pour ce que l’utilisateur tape)
hard_alias <- rbindlist(list(
  data.table(alias = hard_raw_tokens, canon = canon_basic(hard_raw_tokens)),
  data.table(alias = hard_ref$label,  canon = hard_ref$canon),
  data.table(alias = hard_ref$canon,  canon = hard_ref$canon),
  data.table(alias = gsub(" ", "_", hard_ref$canon), canon = hard_ref$canon)
), use.names = TRUE, fill = TRUE)

hard_alias[, alias_norm := normalize_skill(alias)]
hard_alias[, canon := canon_basic(canon)]
hard_alias <- unique(hard_alias[!is.na(canon) & nzchar(canon)], by = "alias_norm")

.map_alias <- setNames(hard_alias$canon, hard_alias$alias_norm)
.map_label <- setNames(hard_ref$label, hard_ref$canon)

map_to_canon <- function(x){
  k <- normalize_skill(x)
  if (k %in% names(.map_alias)) return(unname(.map_alias[k]))
  canon_basic(x)
}

canonize_vec <- function(v){
  v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) return(character(0))
  unique(vapply(v, map_to_canon, character(1)))
}

labelize_vec <- function(v){
  v <- canonize_vec(v)
  out <- .map_label[v]
  out[is.na(out)] <- v[is.na(out)]
  unname(out)
}

jobs_df[, Hard_Skills_Canon := vapply(Hard_Skills, function(x){
  toks <- split_tokens(x)
  can  <- canonize_vec(toks)
  can  <- can[!is.na(can) & nzchar(can)]
  paste(sort(unique(can)), collapse = ", ")
}, character(1))]

jobs_df[, Hard_Skills_Label := vapply(Hard_Skills_Canon, function(x){
  can <- split_tokens(x)
  lab <- labelize_vec(can)
  paste(sort(unique(lab)), collapse = ", ")
}, character(1))]

# Vérifie si une offre contient l'une des compétences sélectionnées
match_any_skill <- function(vec, selected){
  if (is.null(selected) || length(selected) == 0) return(rep(TRUE, length(vec)))
  sel <- canonize_vec(selected)
  vapply(vec, function(x){
    toks <- split_tokens(x)
    any(toks %in% sel)
  }, logical(1))
}

get_hard_can <- function(job_row){
  toks <- split_tokens(pick_col(job_row, c("Hard_Skills_Canon","Hard_Skills")))
  canonize_vec(toks)
}
get_hard_lbl <- function(job_row, n = Inf){
  can <- get_hard_can(job_row)
  lab <- labelize_vec(can)
  if (is.finite(n)) lab <- head(lab, n)
  lab
}

hard_choices <- setNames(hard_ref$canon, hard_ref$label)

token_in_selected <- function(token, selected_terms){
  if (is.null(selected_terms) || length(selected_terms) == 0) return(FALSE)
  token %in% selected_terms || map_to_canon(token) %in% selected_terms
}
token_matches_any <- token_in_selected


# Harmonisation : Soft skills ##################################################
norm_soft_key <- function(x){
  x <- tolower(trimws(as.character(x)))
  x <- gsub("_", " ", x)
  x <- gsub("[’`´]", "'", x)                 # apostrophes
  x <- gsub("[^[:alnum:]' ]", " ", x)        # enlève ponctuation
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- tolower(trimws(x))
  } else {
    x <- tolower(trimws(iconv(x, from = "", to = "ASCII//TRANSLIT")))
  }
  
  x
}

soft_taxo <- data.table::data.table(
  pattern = c(
    # Langues
    "\\benglish\\b|anglais",
    "\\bfrench\\b|francais|français",
    
    # Communication / présentation / vulgarisation
    "communication|oral communication|written communication|presentation|vulgarisation|data storytelling|storytelling|technical communication",
    
    # Analyse / esprit critique
    "analytical|analytique|esprit d'analyse|bon esprit d'analyse|analytical thinking",
    "critical thinking|esprit critique",
    
    # Adaptabilité / flexibilité
    "adaptability|capacite d'adaptation|capacite a s'adapter|capacit[eé] d'adaptation|flexibilite",
    
    # Autonomie / ownership / responsabilité
    "autonomy|autonomie|self starter|self driven",
    "ownership|take ownership|responsabilite|responsabilisation",
    
    # Rigueur / qualité / détail
    "rigor|rigueur|scientific rigor|qualite|exigence qualite|attention to detail",
    
    # Travail en équipe / collaboration / stakeholder
    "teamwork|esprit d'equipe|esprit d’équipe|collaboration|co construction|entraide|solidarite|cohesion",
    "stakeholder management",
    
    # Leadership / mentoring / coaching / gestion d’équipe
    "leadership|gestion d'equipe|gestion d’équipe|encadrement|mentoring|coaching",
    
    # Organisation / priorisation / gestion du temps
    "organization|organisation",
    "priorisation|prioritization|capacite a prioriser",
    "time management",
    
    # Gestion du stress / pression
    "stress management|work under pressure",
    
    # Résolution de problèmes / structuration
    "problem solving|resoudre des problemes|resolution de problemes|problem structuring",
    
    # Prise de décision / influence / négociation
    "decision making|prise de decision",
    "influence|ability to influence",
    "negotiation",
    
    # Curiosité / apprentissage continu
    "curiosity|curiosite|curiosite technique|curiosite scientifique|curiosite pour l'ia",
    "continuous learning|apprentissage|apprendre rapidement|capacite d'apprentissage rapide|capacite a apprendre rapidement",
    
    # Empathie / écoute
    "empathy|empathie|ecoute|sens de l'ecoute",
    
    # Orientation client / résultat / produit
    "customer orientation|sens du service client|sens du contact",
    "impact orientation|orientation resultat|orientation resultats|oriente resultats|sens du resultat|orientation solution",
    "product mindset|product oriented|vision produit|working with product teams|product oriented"
  ),
  soft_key = c(
    "anglais",
    "francais",
    
    "communication",
    "esprit_analyse",
    "esprit_critique",
    
    "adaptabilite",
    
    "autonomie",
    "responsabilite",
    
    "rigueur",
    
    "esprit_equipe",
    "gestion_parties_prenantes",
    
    "leadership",
    
    "organisation",
    "priorisation",
    "gestion_temps",
    
    "gestion_stress",
    
    "resolution_problemes",
    
    "prise_decision",
    "influence",
    "negociation",
    
    "curiosite",
    "apprentissage_continu",
    
    "empathie_ecoute",
    
    "orientation_client",
    "orientation_resultats",
    "culture_produit"
  ),
  label_fr = c(
    "Anglais",
    "Français",
    
    "Communication & présentation",
    "Esprit d’analyse",
    "Esprit critique",
    
    "Adaptabilité",
    
    "Autonomie",
    "Sens des responsabilités",
    
    "Rigueur & qualité",
    
    "Esprit d’équipe & collaboration",
    "Gestion des parties prenantes",
    
    "Leadership & management",
    
    "Organisation",
    "Priorisation",
    "Gestion du temps",
    
    "Gestion du stress",
    
    "Résolution de problèmes",
    
    "Prise de décision",
    "Influence",
    "Négociation",
    
    "Curiosité",
    "Apprentissage continu",
    
    "Écoute & empathie",
    
    "Orientation client",
    "Orientation résultats",
    "Culture produit"
  )
)

map_soft_one <- function(tok){
  if (is.null(tok) || !nzchar(tok)) return(NA_character_)
  if (is_missing_txt(tok)) return(NA_character_)
  
  t <- norm_soft_key(tok)
  hit <- soft_taxo[stringr::str_detect(t, pattern)][1]
  if (!is.null(hit) && nrow(hit) > 0) return(hit$soft_key)
  t
}

map_soft_vec <- function(tokens){
  tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
  out <- vapply(tokens, map_soft_one, character(1))
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

labelize_soft_vec <- function(keys){
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (!length(keys)) return(character(0))
  # lookup dans taxo
  m <- match(keys, soft_taxo$soft_key)
  out <- ifelse(!is.na(m), soft_taxo$label_fr[m], keys)
  
  # fallback joli : "esprit analyse" -> "Esprit Analyse"
  out <- gsub("_", " ", out)
  out <- trimws(gsub("\\s+", " ", out))
  out
}

if (has_col(jobs_df, "Soft_Skills") && !has_col(jobs_df, "Soft_Skills_Canon")) {
  jobs_df[, Soft_Skills_Canon := vapply(Soft_Skills, function(x){
    toks <- split_tokens(x)
    can  <- map_soft_vec(toks)
    paste(can, collapse = ", ")
  }, character(1))]
}

get_soft_can <- function(job_row){
  raw <- pick_col(job_row, c("Soft_Skills_Canon","Soft_Skills"))
  map_soft_vec(split_tokens(raw))
}

get_soft_lbl <- function(job_row, n = 12){
  can <- get_soft_can(job_row)
  labelize_soft_vec(head(can, n))
}

build_soft_choices <- function(df){
  if (!has_col(df, "Soft_Skills")) return(setNames(character(0), character(0)))
  
  all_tok <- split_tokens(df$Soft_Skills)
  all_tok <- all_tok[!is_missing_txt(all_tok)]
  
  soft_keys <- unique(map_soft_vec(all_tok))
  lbls <- labelize_soft_vec(soft_keys)
  
  ord <- order(lbls)
  
  setNames(soft_keys[ord], lbls[ord])
}

# Harmonisation : Benefits ######################################################
get_adv_col <- function(df){
  c("Benefits","Advantages","Perks")[c("Benefits","Advantages","Perks") %in% names(df)][1]
}

norm_adv_key <- function(x){
  x <- tolower(trimws(as.character(x)))
  x <- gsub("_", " ", x)
  x <- gsub("[’`´]", "'", x)
  x <- gsub("[^[:alnum:]' ]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  # enlever accents
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- tolower(trimws(x))
  } else {
    x <- tolower(trimws(iconv(x, from = "", to = "ASCII//TRANSLIT")))
  }
  x
}

adv_taxo <- data.table::data.table(
  pattern = c(
    # Télétravail
    "teletravail|t[eé]l[eé]travail|telework|remote|hybrid|travail a domicile|home office",
    
    # Congés / RTT / CET
    "cong[eé]s|rtt|paid leave|extra holidays|20 jours|cet|compte epargne temps|epargne temps",
    
    # Mutuelle / Santé / Prévoyance
    "mutuelle|assurance sante|assurance sant[eé]|health insurance|sante|provident insurance|pr[eé]voyance",
    
    # Rémunération / Primes / Intéressement / Stocks
    "bonus|prime|primes|int[eé]ressement|participation|profit sharing|salary review|competitive salary|r[eé]mun[eé]ration attractive|actionnariat|stock options|prime d'anciennet[eé]|systeme de recompense|epargne",
    
    # Formation / Carrière / Certifs / Mentoring
    "formation|training|certifications|career development|opportunit[eé]s de carri[eè]res|internal mobility|mentoring|coaching|onboarding|conferences|meetups|hackathons",
    
    # Transport / Mobilité / Parking / Vélo / Voiture
    "transport|mobility|prise en charge du transport|transport coverage|transports en commun|parking|company car|bike facilities",
    
    # Titres resto / repas / snacks / café / fruits
    "titre restaurant|meal vouchers|d[eé]jeuners d'[eé]quipe|free coffee|coffee|tea|free snacks|snacks|fruits",
    
    # Bien-être (sport, yoga…)
    "salle de sport|gym|gym membership|abonnement.*sport|yoga|programme de bien[- ]?etre|well[- ]?being",
    
    # Horaires / flexibilité
    "flexible hours|flextime|horaires|forfait jours|flex office",
    
    # Matériel / équipement / bureau
    "laptop|phone|double screen|ergonomic workstation|home office equipment|modern offices",
    
    # Culture d'entreprise / événements
    "afterwork|happy hour|team events|evenements d'entreprise|e[eé]v[eé]nements d'entreprise|seminars|offsite|team culture|work environment|friendly environment|startup environment",
    
    # Diversité / inclusion
    "diversity|inclusion|equal opportunity|inclusive environment",
    
    # Parental / childcare
    "parental leave|childcare|garde d'enfants",
    
    # CDI / permanent
    "cdi|permanent contract|possibilit[eé] de cdi",
    
    # Jours bénévolat
    "volunteering days"
  ),
  adv_key = c(
    "teletravail_hybride",
    "conges_rtt_cet",
    "mutuelle_sante_prevoyance",
    "remuneration_primes",
    "formation_carriere",
    "transport_mobilite",
    "repas_boissons",
    "bien_etre_sport",
    "flexibilite_horaires",
    "equipement_bureau",
    "culture_evenements",
    "diversite_inclusion",
    "parental_childcare",
    "cdi",
    "volontariat"
  ),
  label_fr = c(
    "Télétravail / hybride",
    "Congés / RTT / CET",
    "Mutuelle / santé / prévoyance",
    "Rémunération & primes",
    "Formation & évolution",
    "Transport & mobilité",
    "Repas / boissons",
    "Bien-être",
    "Flexibilité horaires",
    "Équipement & bureau",
    "Culture d’entreprise & événements",
    "Diversité & inclusion",
    "Congés parentaux / garde d’enfants",
    "CDI",
    "Jours de bénévolat"
  )
)

map_adv_one <- function(tok){
  if (is.null(tok) || !nzchar(tok)) return(NA_character_)
  if (is_missing_txt(tok)) return(NA_character_)
  
  t <- norm_adv_key(tok)
  
  hit <- adv_taxo[stringr::str_detect(t, pattern)][1]
  if (!is.null(hit) && nrow(hit) > 0) return(hit$adv_key)
  t
}

map_adv_vec <- function(tokens){
  tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
  out <- vapply(tokens, map_adv_one, character(1))
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

labelize_adv_vec <- function(keys){
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (!length(keys)) return(character(0))
  
  m <- match(keys, adv_taxo$adv_key)
  out <- ifelse(!is.na(m), adv_taxo$label_fr[m], keys)
  
  out <- gsub("_", " ", out)
  out <- trimws(gsub("\\s+", " ", out))
  out
}

adv_col <- get_adv_col(jobs_df)
if (!is.na(adv_col) && !has_col(jobs_df, "Advantages_Canon")) {
  
  jobs_df[, Advantages_Canon := vapply(.SD[[1]], function(x){
    toks <- split_tokens(x)
    can  <- map_adv_vec(toks)
    paste(sort(unique(can)), collapse = ", ")
  }, character(1)), .SDcols = adv_col]
  
  jobs_df[, Advantages_Label := vapply(Advantages_Canon, function(x){
    can <- split_tokens(x)
    lab <- labelize_adv_vec(can)
    paste(sort(unique(lab)), collapse = ", ")
  }, character(1))]
}

build_adv_choices <- function(df){
  adv_col <- get_adv_col(df)
  if (is.na(adv_col)) return(setNames(character(0), character(0)))
  
  all_tok <- split_tokens(df[[adv_col]])
  all_tok <- all_tok[!is_missing_txt(all_tok)]
  
  keys <- unique(map_adv_vec(all_tok))
  lbls <- labelize_adv_vec(keys)
  ord <- order(lbls)
  
  setNames(keys[ord], lbls[ord])
}

match_any_adv <- function(vec, selected){
  if (is.null(selected) || length(selected) == 0) return(rep(TRUE, length(vec)))
  sel <- unique(selected[!is.na(selected) & nzchar(selected)])
  
  vapply(vec, function(x){
    toks <- split_tokens(x)
    any(toks %in% sel)
  }, logical(1))
}

token_in_selected_adv <- function(token, selected_terms){
  if (is.null(selected_terms) || length(selected_terms) == 0) return(FALSE)
  token %in% selected_terms || map_adv_one(token) %in% selected_terms
}

get_adv_can <- function(job_row){
  raw <- pick_col(job_row, c("Advantages_Canon","Benefits","Advantages","Perks"))
  map_adv_vec(split_tokens(raw))
}

get_adv_lbl <- function(job_row, n = 3){
  can <- get_adv_can(job_row)
  lab <- labelize_adv_vec(can)
  if (is.finite(n)) lab <- head(lab, n)
  lab
}

# Choix finaux
SOFT_CHOICES <- build_soft_choices(jobs_df)
ADV_CHOICES  <- build_adv_choices(jobs_df)
LOC_CHOICES  <- build_loc_choices(jobs_df)


###############################################################################.
# SERVER -----------------------------------------------------------------------
###############################################################################.
server <- function(input, output, session) {
  
  updateSelectizeInput(
    session, "exp_sector",
    choices = sort(unique(jobs_df$Category)),
    server = TRUE
  ) 
  
  
  ## Etat global --------------------------------------------------------------
  rv <- reactiveValues(favorites = c(),
                       compare_ids = c(),
                       current_page = 1,
                       exp_page = 1,
                       exp_map_page = 1,
                       exp_area_active = FALSE,
                       exp_area_bounds = NULL,
                       mp_page = 1,
                       mp_cv_terms = character(0),
                       mp_run_ok = 0,
                       fav_page = 1,
                       cmp_a = NA_real_,
                       cmp_b = NA_real_
  )
  
  ## NAVIGATION ###############################################################
  observeEvent(input$go_explorateur, {
    updateNavbarPage(session, "nav", selected = "Explorateur")
    shinyjs::runjs("setTimeout(function(){ window.scrollTo(0,0); }, 0);")
  })
  
  observeEvent(input$go_match, {
    updateNavbarPage(session, "nav", selected = "Match Parfait")
    shinyjs::runjs("setTimeout(function(){ window.scrollTo(0,0); }, 0);")
  })
  
  observeEvent(input$go_favoris, {
    updateNavbarPage(session, "nav", selected = "Favoris et comparateur")
    shinyjs::runjs("setTimeout(function(){ window.scrollTo(0,0); }, 0);")
  })
  
  toggle_slider <- function(slider_id, disabled) {
    session$sendCustomMessage("toggleIonRange", list(id = slider_id, disabled = disabled))
  }
  set_text <- function(id, text) {
    session$sendCustomMessage("setText", list(id = id, text = text))
  }
  
  #############################################################################.
  # ONGLET 2 : EXPLORATEUR #####################################################
  #############################################################################.
  # UI: pager ------------------------------------------------------------------
  pager_numbers_exp <- function(cur, total){
    if (total <= 7) return(seq_len(total))
    if (cur <= 4) return(c(1,2,3,4,5, NA, total))
    if (cur >= total - 3) return(c(1, NA, total-4, total-3, total-2, total-1, total))
    c(1, NA, cur-1, cur, cur+1, NA, total)
  }
  
  output$exp_pager <- renderUI({
    total <- total_pages()
    cur <- rv$exp_page
    
    d <- sorted_jobs()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    if (total <= 1) return(NULL)
    
    nums <- pager_numbers_exp(cur, total)
    
    div(class = "exp-pager-wrap",
        div(class = "exp-pager",
            tags$button(
              class = paste("pg-arrow", if (cur == 1) "is-disabled" else ""),
              id = "exp_page_prev",
              onclick = "Shiny.setInputValue('exp_page_prev', Date.now(), {priority:'event'})",
              HTML("&#x2039;")
            ),
            
            lapply(nums, function(x){
              if (is.na(x)) return(div(class = "pg-ellipsis", "..."))
              tags$button(
                class = paste("pg-btn", if (x == cur) "is-active" else ""),
                onclick = sprintf("Shiny.setInputValue('exp_page_goto', %d, {priority:'event'})", x),
                x
              )
            }),
            
            tags$button(
              class = paste("pg-arrow", if (cur == total) "is-disabled" else ""),
              id = "exp_page_next",
              onclick = "Shiny.setInputValue('exp_page_next', Date.now(), {priority:'event'})",
              HTML("&#x203A;")
            )
        )
    )
  })
  
  # UI: liste des résultats ----------------------------------------------------
  output$exp_results_list <- renderUI({
    d <- sorted_jobs()
    if (is.null(d) || nrow(d) == 0) return(h4("Aucun résultat."))
    
    # Pagination (10 par page)
    start <- (rv$exp_page - 1) * PER_PAGE + 1
    end   <- min(start + PER_PAGE - 1, nrow(d))
    dd <- d[start:end]
    
    tagList(
      lapply(seq_len(nrow(dd)), function(i){
        job <- dd[i]
        
        title <- pick_col(job, c("Job_Title","Title"))
        comp  <- pick_col(job, c("Company","Company_Name"))
        loc   <- pick_col(job, c("Location","City","Region"))
        cp_raw <- pick_col(job, c("Code_Postal","CP","Postal_Code"))  # AJOUT
        cp_fmt <- format_postal_code(cp_raw)                          # AJOUT
        loc_txt <- paste0(loc, if (nzchar(cp_fmt)) paste0(" (", cp_fmt, ")") else "")  # AJOUT
        
        ct    <- pick_col(job, c("Contract_Type","Contract"))
        ago   <- if (has_col(job, "Publish_Date")) posted_ago_txt(job$Publish_Date) else ""
        url   <- pick_col(job, c("Job_URL","URL","Link","Offer_URL"))
        sources <- get_offer_sources(job)
        
        pay <- format_pay(job)
        pay_txt <- if (nzchar(pay$txt)) paste0(pay$txt, " € / ", pay$unit) else ""
        
        # Hard skills (rapide)
        if (has_col(job, "Hard_Skills_Canon") && has_col(job, "Hard_Skills_Label")) {
          hs_can <- head(split_tokens(job$Hard_Skills_Canon), 3)
          hs_lbl <- head(split_tokens(job$Hard_Skills_Label), 3)
        } else {
          hs_can <- head(get_hard_can(job), 3)
          hs_lbl <- labelize_vec(hs_can)
        }
        
        # Avantages (rapide)
        if (has_col(job, "Advantages_Canon") && has_col(job, "Advantages_Label")) {
          adv_keys <- head(split_tokens(job$Advantages_Canon), 3)
          adv_lbl  <- head(split_tokens(job$Advantages_Label), 3)
        } else {
          adv_keys <- head(get_adv_can(job), 3)
          adv_lbl  <- labelize_adv_vec(adv_keys)
        }
        
        
        is_fav <- as.numeric(job$id) %in% rv$favorites
        contract_active <- !is.null(applied$exp_contract) && applied$exp_contract != "Tous"
        remote_active   <- isTRUE(applied$exp_remote)
        salary_active   <- isTRUE(applied$exp_salary_only)
        hard_active     <- length(applied$exp_hard_skills) > 0
        adv_active      <- length(applied$exp_advantages) > 0
        
        div(
          class = "offer-card js-offer-card",
          onclick = sprintf(
            "Shiny.setInputValue('open_offer', %d, {priority:'event'})",
            as.numeric(job$id)
          ),
          div(class="offer-head",
              div(class="offer-left",
                  tags$h3(class="offer-title", title),
                  div(class="offer-sub",
                      tags$p(class="offer-company", comp),
                      tags$p(class="offer-location", loc_txt)
                  ),
                  
                  div(class="pills",
                      if (nzchar(ct)) {
                        is_ct_selected <- contract_active &&
                          tolower(trimws(ct)) == tolower(trimws(applied$exp_contract))
                        span(class = pill_cls(is_ct_selected), ct)
                      },
                      if (has_col(job,"Is_Remote") && is_remote_true(job$Is_Remote)) {
                        span(class = pill_cls(remote_active), "Télétravail possible")
                      },
                      if (nzchar(pay_txt)) {
                        span(class = pill_cls(salary_active), pay_txt)
                      }
                  ),
                  
                  if (length(hs_can) > 0) div(class="offer-line",
                                              span(class="offer-label", "Stack :"),
                                              div(class="pills",
                                                  lapply(seq_along(hs_can), function(j){
                                                    span(
                                                      class = pill_cls(hard_active && (hs_can[j] %in% applied$exp_hard_skills)),
                                                      hs_lbl[j]
                                                    )
                                                  })
                                              )
                  ),
                  
                  if (length(adv_keys) > 0) div(class="offer-line",
                                                span(class="offer-label", "Le(s) + :"),
                                                div(class="pills",
                                                    lapply(seq_along(adv_keys), function(j){
                                                      span(
                                                        class = pill_cls(adv_active && (adv_keys[j] %in% applied$exp_advantages)),
                                                        adv_lbl[j]
                                                      )
                                                    })
                                                )
                  )
              ),
              
              div(class="offer-right",
                  tags$button(
                    class = paste("fav-btn", if (is_fav) "is-on" else ""),
                    onclick = sprintf(
                      "event.stopPropagation(); Shiny.setInputValue('toggle_fav', %d, {priority:'event'})",
                      as.numeric(job$id)
                    ),
                    tags$i(class = if (is_fav) "fas fa-heart" else "far fa-heart")
                  ),
                  
                  if (nzchar(ago)) div(class="offer-time", ago),
                  
                  div(class = "offer-sources-bottom",
                      render_source_logos(sources)
                  )
              )
          )
        )
      })
    )
  })
  
  exp_jobs_in_view <- reactive({
    d <- exp_map_data()
    if (is.null(d) || nrow(d) == 0) return(d)
    
    if (!isTRUE(rv$exp_area_active) || is.null(rv$exp_area_bounds)) return(d)
    
    b <- rv$exp_area_bounds
    if (!all(c("north","south","east","west") %in% names(b))) return(d)
    
    d[Latitude <= b$north & Latitude >= b$south &
        Longitude <= b$east & Longitude >= b$west]
  })
  
  apply_explorer_ui_state <- function() {
    sal_only <- FALSE
    if (!is.null(isolate(input$exp_salary_only))) sal_only <- isTRUE(isolate(input$exp_salary_only))
    contract <- "Tous"
    if (!is.null(isolate(input$exp_contract))) contract <- isolate(input$exp_contract)
    dur_only <- FALSE
    if (!is.null(isolate(input$exp_duration_only))) dur_only <- isTRUE(isolate(input$exp_duration_only))
    toggle_slider("exp_salary_range", disabled = !sal_only)
    dur_ok <- dur_only && (contract %in% c("Stage", "CDD"))
    toggle_slider("exp_duration_range", disabled = !dur_ok)
    if (isTRUE(tolower(contract) == "freelance")) {
      set_text("exp_salary_title", "Fourchette de salaire (€ / h)")
    } else {
      set_text("exp_salary_title", "Fourchette de salaire (€ / mois)")
    }
  }
  
  ## Initialisation filtres ----------------------------------------------------
  .did_init_explorer <- FALSE
  session$onFlushed(function() {
    if (.did_init_explorer) return()
    .did_init_explorer <<- TRUE
    
    # Type de contrat (MODIF : retire "non spécifié")
    if (has_col(jobs_df, "Contract_Type")) {
      ct <- trimws(as.character(jobs_df$Contract_Type))
      ct <- ct[!is.na(ct) & nzchar(ct) & !is_missing_txt(ct)]
      ct <- sort(unique(ct))
      updateSelectInput(session, "exp_contract", choices = c("Tous", ct), selected = "Tous")
    }
    
    # Recherche (métiers/mots-clés + entreprises)
    updateSelectizeInput(
      session, "exp_q",
      choices = SEARCH_CHOICES,
      server  = FALSE, # recherche plus souple
      options = list(
        placeholder = "Métier, mot-clé, ou entreprise",
        create = FALSE,
        persist = FALSE,
        delimiter = ";",
        maxOptions = 200,
        plugins = list("remove_button", "restore_on_backspace")  # croix pour supprimer l'offre
      )
    )
    
    
    # Localisation : Ville (CP) + recherche CP
    if (has_col(jobs_df, "Location")) {
      
      updateSelectizeInput(
        session, "exp_loc",
        choices = LOC_CHOICES,
        server  = TRUE,
        options = list(
          placeholder = "Ville, département, code postal...",
          plugins = list("remove_button"),
          create = FALSE,
          persist = FALSE,
          openOnFocus = TRUE,
          maxOptions = 200
        )
      )
      
    }
    
    
    # MP - Recherche (métier/mots-clés/entreprise)
    updateSelectizeInput(
      session, "mp_title",
      choices = SEARCH_CHOICES,
      server  = FALSE,
      options = list(
        placeholder = "Métier, mot-clé, ou entreprise",
        create = FALSE,
        persist = FALSE,
        delimiter = ";",
        maxOptions = 200,
        plugins = list("remove_button", "restore_on_backspace"),
        openOnFocus = TRUE
      )
    )
    
    # MP - Localisation
    updateSelectizeInput(
      session, "mp_loc",
      choices = LOC_CHOICES,
      server  = TRUE,
      options = list(
        placeholder = "Ville, département, code postal...",
        plugins = list("remove_button"),
        create = FALSE,
        persist = FALSE,
        openOnFocus = TRUE,
        maxOptions = 200
      )
    )
    
    
    
    # Secteur 
    if (has_col(jobs_df, "Category")) {
      
      cats <- trimws(as.character(jobs_df$Category))
      cats <- cats[!is.na(cats) & nzchar(cats) & !is_missing_txt(cats)]
      
      updateSelectizeInput(
        session, "exp_sector",
        choices = sort(unique(cats)),
        server  = TRUE
      )
    }
    
    # Hard skills
    if (has_col(jobs_df, "Hard_Skills_Canon")) {
      updateSelectizeInput(session, "exp_hard_skills",
                           choices = hard_choices,
                           server = TRUE)
    }
    
    # Soft skills 
    if (has_col(jobs_df, "Soft_Skills")) {
      updateSelectizeInput(session, "exp_soft_skills",
                           choices = SOFT_CHOICES,
                           server = TRUE)
    }
    
    
    # Avantages
    updateSelectizeInput(session, "exp_advantages",
                         choices = ADV_CHOICES,
                         server = TRUE)
    
    
    # Slider salaire (€/mois par défaut)
    if (salary_cols_ok(jobs_df)) {
      mo <- get_monthly_range(jobs_df)
      max_vec <- mo$max[is.finite(mo$max)]
      if (length(max_vec) > 0) {
        max_s <- ceiling(max(max_vec) / 500) * 500
        updateSliderInput(session, "exp_salary_range", min = 0, max = max_s, value = c(0, max_s), step = 500)
      }
    }
    apply_explorer_ui_state()
  }, once = TRUE)
  
  observeEvent(input$nav, {
    if (identical(input$nav, "Explorateur")) apply_explorer_ui_state()
  }, ignoreInit = FALSE)
  
  observeEvent(input$exp_salary_only, {
    toggle_slider("exp_salary_range", disabled = !isTRUE(input$exp_salary_only))
  }, ignoreInit = TRUE)
  
  observeEvent(input$exp_contract, {
    req(input$exp_contract)
    if (tolower(input$exp_contract) == "freelance") {
      set_text("exp_salary_title", "Fourchette de salaire (€ /h)")
    } else {
      set_text("exp_salary_title", "Fourchette de salaire (€ /mois)")
    }
    if (!salary_cols_ok(jobs_df)) {
      apply_explorer_ui_state()
      return()
    }
    if (tolower(input$exp_contract) == "freelance") {
      hr <- get_hourly_range(jobs_df)
      mx <- max(hr$max[is.finite(hr$max)], na.rm = TRUE)
      if (is.finite(mx)) {
        updateSliderInput(session, "exp_salary_range", min = 0, max = ceiling(mx), value = c(0, ceiling(mx)), step = 1)
      }
    } else {
      mo <- get_monthly_range(jobs_df)
      mx <- max(mo$max[is.finite(mo$max)], na.rm = TRUE)
      if (is.finite(mx)) {
        mx2 <- ceiling(mx / 500) * 500
        updateSliderInput(session, "exp_salary_range", min = 0, max = mx2, value = c(0, mx2), step = 500)
      }
    }
    apply_explorer_ui_state()
  }, ignoreInit = TRUE)
  
  # Sources : "Tous"
  observeEvent(input$exp_source_all, {
    v <- isTRUE(input$exp_source_all)
    updateCheckboxInput(session, "exp_source_li", value = v)
    updateCheckboxInput(session, "exp_source_in", value = v)
    updateCheckboxInput(session, "exp_source_wttj", value = v)
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$exp_source_li, input$exp_source_in, input$exp_source_wttj), {
    all_on <- isTRUE(input$exp_source_li) && isTRUE(input$exp_source_in) && isTRUE(input$exp_source_wttj)
    if (!identical(isTRUE(input$exp_source_all), all_on)) {
      updateCheckboxInput(session, "exp_source_all", value = all_on)
    }
  }, ignoreInit = TRUE)
  
  # Durée Stage/CDD
  duration_limits <- reactiveValues(min = 0, max = 24)
  
  observeEvent(input$exp_contract, {
    req(input$exp_contract)
    
    if (!(input$exp_contract %in% c("Stage", "CDD"))) {
      updateCheckboxInput(session, "exp_duration_only", value = FALSE)
      toggle_slider("exp_duration_range", disabled = TRUE)
      return()
    }
    
    mn <- NA_real_; mx <- NA_real_
    if (input$exp_contract == "Stage" &&
        has_col(jobs_df, "Stage_Duration_Min") && has_col(jobs_df, "Stage_Duration_Max")) {
      mn <- suppressWarnings(min(num_clean(jobs_df$Stage_Duration_Min), na.rm = TRUE))
      mx <- suppressWarnings(max(num_clean(jobs_df$Stage_Duration_Max), na.rm = TRUE))
    } else if (input$exp_contract == "CDD" &&
               has_col(jobs_df, "CDD_Duration_Min") && has_col(jobs_df, "CDD_Duration_Max")) {
      mn <- suppressWarnings(min(num_clean(jobs_df$CDD_Duration_Min), na.rm = TRUE))
      mx <- suppressWarnings(max(num_clean(jobs_df$CDD_Duration_Max), na.rm = TRUE))
    } else {
      toggle_slider("exp_duration_range", disabled = TRUE)
      return()
    }
    if (!is.finite(mn) || !is.finite(mx) || mx < mn) {
      toggle_slider("exp_duration_range", disabled = TRUE)
      return()
    }
    
    duration_limits$min <- mn
    duration_limits$max <- mx
    
    updateSliderInput(session, "exp_duration_range", min = mn, max = mx, value = c(mn, mx), step = 1)
    toggle_slider("exp_duration_range", disabled = !isTRUE(input$exp_duration_only))
  }, ignoreInit = TRUE)
  
  observeEvent(input$exp_duration_only, {
    if (!isTRUE(input$exp_duration_only)) {
      updateSliderInput(session, "exp_duration_range", value = c(duration_limits$min, duration_limits$max))
    }
    toggle_slider(
      "exp_duration_range",
      disabled = !(isTRUE(input$exp_duration_only) && (input$exp_contract %in% c("Stage","CDD")))
    )
  }, ignoreInit = TRUE)
  
  ## Etat filtres appliqués ----------------------------------------------------
  applied <- reactiveValues(
    exp_q = character(0),
    exp_loc = character(0),
    
    exp_contract = "Tous",
    exp_duration_only = FALSE,
    exp_duration_range = NULL,
    
    exp_experience_level = "Tous",
    
    exp_salary_only = FALSE,
    exp_salary_range = NULL,
    
    exp_remote = FALSE,
    
    exp_sector = character(0),
    exp_company_category = "Tous",
    exp_hard_skills = character(0),
    exp_soft_skills = character(0),
    exp_advantages = character(0),
    
    exp_date = "Toutes",
    exp_source = c("LinkedIn","Indeed","Welcome to the Jungle")
  )
  
  observeEvent(input$exp_search_btn, {
    applied$exp_q   <- input$exp_q
    applied$exp_loc <- input$exp_loc
  }, ignoreInit = TRUE)
  
  observeEvent(input$exp_apply_filters, {
    applied$exp_q   <- input$exp_q
    applied$exp_loc <- input$exp_loc
    
    applied$exp_contract       <- input$exp_contract
    applied$exp_duration_only  <- isTRUE(input$exp_duration_only)
    applied$exp_duration_range <- input$exp_duration_range
    
    applied$exp_experience_level <- input$exp_experience_level
    
    applied$exp_salary_only  <- isTRUE(input$exp_salary_only)
    applied$exp_salary_range <- input$exp_salary_range
    
    applied$exp_remote <- isTRUE(input$exp_remote)
    
    applied$exp_sector           <- input$exp_sector
    applied$exp_company_category <- input$exp_company_category
    applied$exp_hard_skills <- canonize_vec(input$exp_hard_skills)
    applied$exp_soft_skills      <- input$exp_soft_skills
    applied$exp_advantages       <- input$exp_advantages
    applied$exp_date             <- input$exp_date
    
    src <- c()
    if (isTRUE(input$exp_source_li)) src <- c(src, "LinkedIn")
    if (isTRUE(input$exp_source_in)) src <- c(src, "Indeed")
    if (isTRUE(input$exp_source_wttj)) src <- c(src, "Welcome to the Jungle")
    applied$exp_source <- src
  }, ignoreInit = TRUE)
  
  # Mise à jour immédiate du filtre "Secteur / Catégorie"
  observeEvent(input$exp_sector, {
    applied$exp_sector <- input$exp_sector
    rv$exp_page <- 1
  }, ignoreInit = TRUE)
  
  
  ## Filtrage, tri et pagination -----------------------------------------------
  filtered_jobs <- reactive({
    data <- jobs_df
    
    # Search top
    if (length(applied$exp_q) > 0) {
      
      q <- applied$exp_q
      qn <- norm_txt(q)
      
      # termes qui correspondent à des entreprises connues
      is_comp <- qn %in% COMPANY_NORM_SET
      q_comp  <- q[is_comp]
      q_other <- q[!is_comp]
      
      ok_comp_exact <- rep(FALSE, nrow(data))
      if (length(q_comp) > 0 && has_col(data, "Company")) {
        c_norm <- norm_txt(data$Company)
        ok_comp_exact <- c_norm %in% norm_txt(q_comp)
      }
      
      ok_title <- if (length(q_other) > 0 && has_col(data, "Job_Title"))   match_any_term(data$Job_Title, q_other) else rep(FALSE, nrow(data))
      ok_comp  <- if (length(q_other) > 0 && has_col(data, "Company"))     match_any_term(data$Company,   q_other) else rep(FALSE, nrow(data))
      ok_hard  <- if (length(q_other) > 0 && has_col(data, "Hard_Skills")) match_any_term(data$Hard_Skills, q_other) else rep(FALSE, nrow(data))
      
      data <- data[ok_comp_exact | ok_title | ok_comp | ok_hard, ]
    }
    
    
    # Localisation (Ville (CP))
    if (length(applied$exp_loc) > 0 && has_col(data, "Location")) {
      loc_search <- loc_key_vec(data)  # ex: "Bordeaux (33000)"
      data <- data[match_any_term(loc_search, applied$exp_loc), ]
    }
    
    
    
    
    # Type contrat
    if (!is.null(applied$exp_contract) && applied$exp_contract != "Tous" && has_col(data, "Contract_Type")) {
      sel <- toupper(trimws(applied$exp_contract))
      keep <- toupper(trimws(as.character(data$Contract_Type))) == sel
      data <- data[keep, ]
    }
    
    # Durée Stage/CDD
    if (isTRUE(applied$exp_duration_only) &&
        !is.null(applied$exp_duration_range) &&
        (applied$exp_contract %in% c("Stage", "CDD"))) {
      
      umin <- as.numeric(applied$exp_duration_range[1])
      umax <- as.numeric(applied$exp_duration_range[2])
      
      if (applied$exp_contract == "Stage" &&
          has_col(data, "Stage_Duration_Min") && has_col(data, "Stage_Duration_Max")) {
        dmin <- num_clean(data$Stage_Duration_Min)
        dmax <- num_clean(data$Stage_Duration_Max)
        keep <- !is.na(dmin) & !is.na(dmax) & (dmin <= umax) & (dmax >= umin)
        data <- data[keep, ]
      }
      
      if (applied$exp_contract == "CDD" &&
          has_col(data, "CDD_Duration_Min") && has_col(data, "CDD_Duration_Max")) {
        dmin <- num_clean(data$CDD_Duration_Min)
        dmax <- num_clean(data$CDD_Duration_Max)
        keep <- !is.na(dmin) & !is.na(dmax) & (dmin <= umax) & (dmax >= umin)
        data <- data[keep, ]
      }
    }
    
    # Expérience
    if (!is.null(applied$exp_experience_level) &&
        applied$exp_experience_level != "Tous" &&
        has_col(data, "Experience_Level")) {
      
      exp_raw <- tolower(trimws(as.character(data$Experience_Level)))
      target  <- tolower(trimws(as.character(applied$exp_experience_level)))
      keep <- !is.na(exp_raw) & exp_raw == target
      data <- data[keep, ]
    }
    
    # Salaire
    if (isTRUE(applied$exp_salary_only) && salary_cols_ok(data) && !is.null(applied$exp_salary_range)) {
      sel_min <- as.numeric(applied$exp_salary_range[1])
      sel_max <- as.numeric(applied$exp_salary_range[2])
      
      if (!is.null(applied$exp_contract) && tolower(applied$exp_contract) == "freelance") {
        hr <- get_hourly_range(data)
        keep <- is.finite(hr$min) & is.finite(hr$max) & (hr$max >= sel_min) & (hr$min <= sel_max)
        data <- data[keep, ]
      } else {
        mo <- get_monthly_range(data)
        keep <- is.finite(mo$min) & is.finite(mo$max) & (mo$max >= sel_min) & (mo$min <= sel_max)
        data <- data[keep, ]
      }
    }
    
    # Télétravail
    if (isTRUE(applied$exp_remote) && has_col(data, "Is_Remote")) {
      keep <- vapply(data$Is_Remote, is_remote_true, logical(1))
      data <- data[keep, ]
    }
    
    # Secteur
    #if (length(applied$exp_sector) > 0 && has_col(data, "Sector")) {
    #  data <- data[match_any_term(data$Sector, applied$exp_sector), ]
    #}
    # Catégorie (regroupe plusieurs secteurs)
    if (length(applied$exp_sector) > 0 && has_col(data, "Category")) {
      data[, Category := clean_txt(Category)]
      sel <- clean_txt(applied$exp_sector)
      data <- data[Category %in% sel]
    }
    
    
    # Taille entreprise
    if (!is.null(applied$exp_company_category) &&
        applied$exp_company_category != "Tous" &&
        has_col(data, "Company_Category")) {
      keep <- as.character(data$Company_Category) == applied$exp_company_category
      data <- data[keep, ]
    }
    
    # Hard skills
    if (length(applied$exp_hard_skills) > 0 && has_col(data, "Hard_Skills_Canon")) {
      data <- data[match_any_skill(data$Hard_Skills_Canon, applied$exp_hard_skills), ]
    }
    
    # Soft skills
    # Soft skills (canon)
    if (length(applied$exp_soft_skills) > 0) {
      if (has_col(data, "Soft_Skills_Canon")) {
        data <- data[match_any_term(data$Soft_Skills_Canon, applied$exp_soft_skills), ]
      } else if (has_col(data, "Soft_Skills")) {
        # fallback si la colonne canon n'existe pas
        data <- data[match_any_term(data$Soft_Skills, applied$exp_soft_skills), ]
      }
    }
    
    # Avantages
    if (length(applied$exp_advantages) > 0) {
      if (has_col(data, "Advantages_Canon")) {
        data <- data[match_any_adv(data$Advantages_Canon, applied$exp_advantages), ]
      } else {
        adv_col <- get_adv_col(data)
        if (!is.na(adv_col)) {
          data <- data[match_any_term(data[[adv_col]], applied$exp_advantages), ]
        }
      }
    }
    
    
    # Date publication
    if (!is.null(applied$exp_date) && applied$exp_date != "Toutes" && has_col(data, "Publish_Date")) {
      keep_days <- switch(applied$exp_date,
                          "Depuis 24h"       = 1,
                          "Depuis 3 jours"   = 3,
                          "Depuis 1 semaine" = 7,
                          "Depuis 1 mois"    = 30,
                          NA_integer_
      )
      if (!is.na(keep_days)) {
        cutoff <- Sys.Date() - keep_days
        data <- data[!is.na(Publish_Date) & Publish_Date >= cutoff, ]
      }
    }
    
    # Source site
    if (!is.null(applied$exp_source) && has_col(data, "occurrence_sites")) {
      if (length(applied$exp_source) == 0) {
        data <- data[0, ]
      } else {
        keep <- match_any_source(data$occurrence_sites, applied$exp_source)
        data <- data[keep, ]
      }
    }
    
    data
  })
  
  sorted_jobs <- reactive({
    data <- data.table::copy(filtered_jobs())
    if (is.null(data) || nrow(data) == 0) return(data)
    
    data[, .idx := .I]
    
    if (has_col(data, "Publish_Date")) {
      data <- data[order(is.na(Publish_Date), .idx)]
    }
    
    sm <- normalize_sort_mode(input$exp_sort)
    
    if (!is.null(sm)) {
      
      # Salaire : décroissant
      if (sm == "salary_desc" && salary_cols_ok(data)) {
        if (!is.null(applied$exp_contract) && tolower(applied$exp_contract) == "freelance") {
          hr <- get_hourly_range(data)
          data[, .sal := hr$max]
        } else {
          mo <- get_monthly_range(data)
          data[, .sal := mo$max]
        }
        data <- data[order(is.na(.sal), -.sal, .idx)]
        data[, .sal := NULL]
      }
      
      # Salaire : croissant
      if (sm == "salary_asc" && salary_cols_ok(data)) {
        if (!is.null(applied$exp_contract) && tolower(applied$exp_contract) == "freelance") {
          hr <- get_hourly_range(data)
          data[, .sal := hr$max]
        } else {
          mo <- get_monthly_range(data)
          data[, .sal := mo$max]
        }
        data <- data[order(is.na(.sal), .sal, .idx)]
        data[, .sal := NULL]
      }
      
      # Date : décroissant (plus récent)
      if (sm == "date_desc" && has_col(data, "Publish_Date")) {
        data <- data[order(is.na(Publish_Date), -as.numeric(Publish_Date), .idx)]
      }
      
      # Date : croissant (plus ancien)
      if (sm == "date_asc" && has_col(data, "Publish_Date")) {
        data <- data[order(is.na(Publish_Date),  as.numeric(Publish_Date), .idx)]
      }
    }
    
    data[, .idx := NULL]
    data
  })
  
  PER_PAGE <- 10
  
  # CARTE ----------------------------------------------------------------------
  exp_map_data <- reactive({
    d <- sorted_jobs()  # la carte suit recherche + filtres + tri
    if (is.null(d) || nrow(d) == 0) return(d)
    
    if (!has_col(d, "Latitude") || !has_col(d, "Longitude")) return(d[0])
    
    d <- d[is.finite(Latitude) & is.finite(Longitude)]
    d
  })
  
  CLUSTER_OPTS <- leaflet::markerClusterOptions(
    disableClusteringAtZoom = 12,   # à partir de ce zoom >> plus de paquets
    spiderfyOnMaxZoom = TRUE,
    showCoverageOnHover = FALSE,
    zoomToBoundsOnClick = TRUE,
    chunkedLoading = TRUE
  )
  
  output$exp_map <- leaflet::renderLeaflet({
    d <- exp_map_data()
    
    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
      leaflet::setView(lng = 2.2137, lat = 46.2276, zoom = 5)
    
    n <- if (is.null(d)) 0 else nrow(d)
    m <- m %>% leaflet::addControl(
      html = paste0("<div class='map-count'>", n, " offre", ifelse(n > 1, "s", ""), "</div>"),
      position = "topright"
    )
    
    m <- m %>% leaflet::addControl(
      html = paste0("<button class='map-search-btn' onclick=\"Shiny.setInputValue('exp_search_area', Date.now(), {priority:'event'})\">Rechercher dans cette zone</button>"),
      position = "bottomleft",
      className = "map-search-control"
    )
    
    
    if (!is.null(d) && n > 0) {
      
      title <- if (has_col(d, "Job_Title")) as.character(d$Job_Title) else ""
      comp  <- if (has_col(d, "Company"))   as.character(d$Company)   else ""
      loc   <- if (has_col(d, "Location"))  as.character(d$Location)  else ""
      
      popup <- mapply(function(id, t, c, l){
        t <- ifelse(is.na(t), "", t)
        c <- ifelse(is.na(c), "", c)
        l <- ifelse(is.na(l), "", l)
        
        paste0(
          "<div class='map-popup' ",
          "onclick=\"Shiny.setInputValue('open_offer', ", id, ", {priority:'event'})\">",
          "<b>", htmltools::htmlEscape(t), "</b><br>",
          htmltools::htmlEscape(c), "<br>",
          htmltools::htmlEscape(l),
          "</div>"
        )
      }, d$id, title, comp, loc, SIMPLIFY = TRUE, USE.NAMES = FALSE)
      
      
      label_vec <- if (has_col(d, "Job_Title")) as.character(d$Job_Title) else NULL
      
      m <- m %>%
        leaflet::addCircleMarkers(
          data = d,
          lng = ~Longitude, lat = ~Latitude,
          radius = 6,
          stroke = TRUE, weight = 1,
          fillOpacity = 0.8,
          popup = popup,
          label = label_vec,
          group = "jobs_pts",
          clusterOptions = CLUSTER_OPTS,
          popupOptions = leaflet::popupOptions(className = "job-popup")
        )
    }
    
    m
  })
  
  observeEvent(exp_map_data(), {
    d <- exp_map_data()
    
    proxy <- leaflet::leafletProxy("exp_map", session = session, deferUntilFlush = FALSE) %>%
      leaflet::clearGroup("jobs_pts") %>% 
      leaflet::clearControls()
    
    proxy %>% leaflet::addControl(
      html = paste0("<button class='map-search-btn' onclick=\"Shiny.setInputValue('exp_search_area', Date.now(), {priority:'event'})\">Rechercher dans cette zone</button>"),
      position = "bottomleft",
      className = "map-search-control"
    )
    
    n <- if (is.null(d)) 0 else nrow(d)
    
    proxy %>% leaflet::addControl(
      html = paste0("<div class='map-count'>", n, " offre", ifelse(n > 1, "s", ""), "</div>"),
      position = "topright"
    )
    
    if (is.null(d) || n == 0) return()
    
    title <- if (has_col(d, "Job_Title")) as.character(d$Job_Title) else ""
    comp  <- if (has_col(d, "Company"))   as.character(d$Company)   else ""
    loc   <- if (has_col(d, "Location"))  as.character(d$Location)  else ""
    
    url_col <- intersect(c("Url","Job_URL","URL","Link","Offer_URL"), names(d))[1]
    url <- if (!is.na(url_col)) as.character(d[[url_col]]) else ""
    
    popup <- mapply(function(t, c, l, u){
      t <- ifelse(is.na(t), "", t)
      c <- ifelse(is.na(c), "", c)
      l <- ifelse(is.na(l), "", l)
      u <- ifelse(is.na(u), "", u)
      
      link <- if (nzchar(u)) paste0("<br><a href='", u, "' target='_blank'>Candidature</a>") else ""
      paste0("<b>", htmltools::htmlEscape(t), "</b><br>",
             htmltools::htmlEscape(c), "<br>",
             htmltools::htmlEscape(l),
             link)
    }, title, comp, loc, url, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    
    label_vec <- if (has_col(d, "Job_Title")) as.character(d$Job_Title) else NULL
    
    proxy %>%
      leaflet::addCircleMarkers(
        data = d,
        lng = ~Longitude, lat = ~Latitude,
        radius = 6,
        stroke = TRUE, weight = 1,
        fillOpacity = 0.8,
        popup = popup,
        label = label_vec,
        group = "jobs_pts",
        clusterOptions = CLUSTER_OPTS,
        popupOptions = leaflet::popupOptions(className = "job-popup")
      )
    
  }, ignoreInit = FALSE)
  
  observeEvent(input$exp_search_area, {
    b <- input$exp_map_bounds
    if (is.null(b)) return()
    
    rv$exp_area_active <- TRUE
    rv$exp_area_bounds <- b
    rv$exp_map_page <- 1
  }, ignoreInit = TRUE)
  
  exp_jobs_in_view <- reactive({
    d <- exp_map_data()
    if (is.null(d) || nrow(d) == 0) return(d)
    
    if (!isTRUE(rv$exp_area_active) || is.null(rv$exp_area_bounds)) return(d)
    
    b <- rv$exp_area_bounds
    if (!all(c("north","south","east","west") %in% names(b))) return(d)
    
    d[Latitude <= b$north & Latitude >= b$south &
        Longitude <= b$east & Longitude >= b$west]
  })
  
  total_pages <- reactive({
    d <- sorted_jobs()
    if (is.null(d) || nrow(d) == 0) return(1L)
    as.integer(ceiling(nrow(d) / PER_PAGE))
  })
  
  observeEvent(list(input$exp_search_btn, input$exp_apply_filters, input$exp_sort), {
    rv$exp_page <- 1
  }, ignoreInit = TRUE)
  
  observeEvent(total_pages(), {
    tp <- total_pages()
    if (rv$exp_page < 1) rv$exp_page <- 1
    if (rv$exp_page > tp) rv$exp_page <- tp
  }, ignoreInit = TRUE)
  
  observeEvent(input$exp_page_goto, {
    p <- as.integer(input$exp_page_goto)
    tp <- total_pages()
    if (!is.na(p)) rv$exp_page <- max(1, min(tp, p))
  }, ignoreInit = TRUE)
  
  observeEvent(input$exp_page_prev, {
    rv$exp_page <- max(1, rv$exp_page - 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$exp_page_next, {
    rv$exp_page <- min(total_pages(), rv$exp_page + 1)
  }, ignoreInit = TRUE)
  
  # PAGER CARTE
  PER_PAGE_MAP <- 5
  
  exp_map_total_pages <- reactive({
    d <- exp_jobs_in_view()
    if (is.null(d) || nrow(d) == 0) return(1L)
    as.integer(ceiling(nrow(d) / PER_PAGE_MAP))
  })
  
  observeEvent(list(input$exp_search_btn, input$exp_apply_filters, input$exp_sort, input$exp_search_area), {
    rv$exp_map_page <- 1
  }, ignoreInit = TRUE)
  
  observeEvent(exp_map_total_pages(), {
    tp <- exp_map_total_pages()
    if (rv$exp_map_page < 1) rv$exp_map_page <- 1
    if (rv$exp_map_page > tp) rv$exp_map_page <- tp
  }, ignoreInit = TRUE)
  
  observeEvent(input$exp_map_page_goto, {
    p <- as.integer(input$exp_map_page_goto)
    tp <- exp_map_total_pages()
    if (!is.na(p)) rv$exp_map_page <- max(1, min(tp, p))
  }, ignoreInit = TRUE)
  
  observeEvent(input$exp_map_page_prev, {
    rv$exp_map_page <- max(1, rv$exp_map_page - 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$exp_map_page_next, {
    rv$exp_map_page <- min(exp_map_total_pages(), rv$exp_map_page + 1)
  }, ignoreInit = TRUE)
  
  output$exp_map_pager <- renderUI({
    total <- exp_map_total_pages()
    cur <- rv$exp_map_page
    d <- exp_jobs_in_view()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    if (total <= 1) return(NULL)
    
    nums <- pager_numbers_exp(cur, total)  # <-- tu l’as déjà plus haut
    
    div(class = "exp-pager-wrap",
        div(class = "exp-pager",
            tags$button(
              class = paste("pg-arrow", if (cur == 1) "is-disabled" else ""),
              onclick = "Shiny.setInputValue('exp_map_page_prev', Date.now(), {priority:'event'})",
              HTML("&#x2039;")
            ),
            
            lapply(nums, function(x){
              if (is.na(x)) return(div(class = "pg-ellipsis", "..."))
              tags$button(
                class = paste("pg-btn", if (x == cur) "is-active" else ""),
                onclick = sprintf("Shiny.setInputValue('exp_map_page_goto', %d, {priority:'event'})", x),
                x
              )
            }),
            
            tags$button(
              class = paste("pg-arrow", if (cur == total) "is-disabled" else ""),
              onclick = "Shiny.setInputValue('exp_map_page_next', Date.now(), {priority:'event'})",
              HTML("&#x203A;")
            )
        )
    )
  })
  
  output$exp_results_list_map <- renderUI({
    d <- exp_jobs_in_view()
    if (is.null(d) || nrow(d) == 0) return(h4("Aucun résultat dans cette zone."))
    
    start <- (rv$exp_map_page - 1) * PER_PAGE_MAP + 1
    end   <- min(start + PER_PAGE_MAP - 1, nrow(d))
    dd <- d[start:end]
    
    tagList(
      lapply(seq_len(nrow(dd)), function(i){
        job <- dd[i]
        
        title <- pick_col(job, c("Job_Title","Title"))
        comp  <- pick_col(job, c("Company","Company_Name"))
        loc   <- pick_col(job, c("Location","City","Region"))
        cp_raw <- pick_col(job, c("Code_Postal","CP","Postal_Code"))
        cp_fmt <- format_postal_code(cp_raw)
        loc_txt <- paste0(loc, if (nzchar(cp_fmt)) paste0(" (", cp_fmt, ")") else "")
        
        ct    <- pick_col(job, c("Contract_Type","Contract"))
        ago   <- if (has_col(job, "Publish_Date")) posted_ago_txt(job$Publish_Date) else ""
        url   <- pick_col(job, c("Job_URL","URL","Link","Offer_URL","Url"))
        sources <- get_offer_sources(job)
        
        pay <- format_pay(job)
        pay_txt <- if (nzchar(pay$txt)) paste0(pay$txt, " € / ", pay$unit) else ""
        
        # Hard skills
        if (has_col(job, "Hard_Skills_Canon") && has_col(job, "Hard_Skills_Label")) {
          hs_can <- head(split_tokens(job$Hard_Skills_Canon), 3)
          hs_lbl <- head(split_tokens(job$Hard_Skills_Label), 3)
        } else {
          hs_can <- head(get_hard_can(job), 3)
          hs_lbl <- labelize_vec(hs_can)
        }
        
        # Avantages
        if (has_col(job, "Advantages_Canon") && has_col(job, "Advantages_Label")) {
          adv_keys <- head(split_tokens(job$Advantages_Canon), 3)
          adv_lbl  <- head(split_tokens(job$Advantages_Label), 3)
        } else {
          adv_keys <- head(get_adv_can(job), 3)
          adv_lbl  <- labelize_adv_vec(adv_keys)
        }
        
        
        is_fav <- as.numeric(job$id) %in% rv$favorites
        contract_active <- !is.null(applied$exp_contract) && applied$exp_contract != "Tous"
        remote_active   <- isTRUE(applied$exp_remote)
        salary_active   <- isTRUE(applied$exp_salary_only)
        hard_active     <- length(applied$exp_hard_skills) > 0
        adv_active      <- length(applied$exp_advantages) > 0
        
        div(
          class = "offer-card js-offer-card",
          onclick = sprintf(
            "Shiny.setInputValue('open_offer', %d, {priority:'event'})",
            as.numeric(job$id)
          ),
          
          div(class="offer-head",
              div(class="offer-left",
                  tags$h3(class="offer-title", title),
                  div(class="offer-sub",
                      tags$p(class="offer-company", comp),
                      tags$p(class="offer-location", loc_txt)
                  ),
                  
                  div(class="pills",
                      if (nzchar(ct)) {
                        is_ct_selected <- contract_active &&
                          tolower(trimws(ct)) == tolower(trimws(applied$exp_contract))
                        span(class = pill_cls(is_ct_selected), ct)
                      },
                      if (has_col(job,"Is_Remote") && is_remote_true(job$Is_Remote)) {
                        span(class = pill_cls(remote_active), "Télétravail possible")
                      },
                      if (nzchar(pay_txt)) {
                        span(class = pill_cls(salary_active), pay_txt)
                      }
                  ),
                  
                  if (length(hs_can) > 0) div(class="offer-line",
                                              span(class="offer-label", "Stack :"),
                                              div(class="pills",
                                                  lapply(seq_along(hs_can), function(j){
                                                    span(
                                                      class = pill_cls(hard_active && (hs_can[j] %in% applied$exp_hard_skills)),
                                                      hs_lbl[j]
                                                    )
                                                  })
                                              )
                  ),
                  
                  if (length(adv_keys) > 0) div(class="offer-line",
                                                span(class="offer-label", "Le(s) + :"),
                                                div(class="pills",
                                                    lapply(seq_along(adv_keys), function(j){
                                                      span(
                                                        class = pill_cls(adv_active && (adv_keys[j] %in% applied$exp_advantages)),
                                                        adv_lbl[j]
                                                      )
                                                    })
                                                )
                  )
              ),
              
              
              div(class="offer-right",
                  tags$button(
                    class = paste("fav-btn", if (is_fav) "is-on" else ""),
                    onclick = sprintf(
                      "event.stopPropagation(); Shiny.setInputValue('toggle_fav', %d, {priority:'event'})",
                      as.numeric(job$id)
                    ),
                    tags$i(class = if (is_fav) "fas fa-heart" else "far fa-heart")
                  ),
                  
                  if (nzchar(ago)) div(class="offer-time", ago),
                  
                  div(class = "offer-sources-bottom",
                      render_source_logos(sources)
                  )
              )
          )
        )
      })
    )
  })
  
  output$exp_count <- renderText({
    d <- sorted_jobs()
    n <- if (is.null(d)) 0 else nrow(d)
    paste(n, "offres trouvées")
  })
  
  # Favoris --------------------------------------------------------------------
  observeEvent(input$toggle_fav, {
    id <- as.numeric(input$toggle_fav)
    if (id %in% rv$favorites) rv$favorites <- setdiff(rv$favorites, id)
    else rv$favorites <- c(rv$favorites, id)
  })
  
  # Pop-up détail offre ---------------------------------------------------------
  observeEvent(input$open_offer, {
    id_val <- suppressWarnings(as.numeric(input$open_offer))
    if (!is.finite(id_val)) return()
    
    job <- jobs_df[id == id_val]
    if (nrow(job) == 0) return()
    job <- job[1]
    
    title <- pick_col(job, c("Job_Title","Title"))
    comp  <- pick_col(job, c("Company","Company_Name"))
    loc   <- pick_col(job, c("Location","City","Region"))
    cp_raw <- pick_col(job, c("Code_Postal","CP","Postal_Code"))
    cp_fmt <- format_postal_code(cp_raw)
    ct    <- pick_col(job, c("Contract_Type","Contract"))
    url   <- pick_col(job, c("Url","Job_URL","URL","Link","Offer_URL"))
    
    sources <- get_offer_sources(job)
    
    pay <- format_pay(job)
    pay_txt  <- if (nzchar(pay$txt)) paste0(pay$txt, " € / ", pay$unit) else ""
    tele_txt <- if (has_col(job,"Is_Remote") && is_remote_true(job$Is_Remote)) "Télétravail possible" else ""
    
    desc <- pick_col(job, c("Job_Description","Description"))
    if (!nzchar(desc)) desc <- "—"
    
    # Nettoyage des "Show more/less" parasites (souvent scrapés)
    desc <- gsub("\\bshow\\s*more\\b|\\bshow\\s*less\\b", "", desc, ignore.case = TRUE)
    desc <- gsub("\\bvoir\\s*plus\\b|\\bvoir\\s*moins\\b", "", desc, ignore.case = TRUE)
    desc <- trimws(desc)
    
    # Utilité : échappe le texte
    esc_inline <- function(x){
      x <- as.character(x %||% "")
      x <- gsub("\r\n|\n|\r", " ", x)
      x <- gsub("\\s+", " ", x)
      htmltools::htmlEscape(x)
    }
    
    # Utilité : tronque si trop long (pour afficher "Voir plus")
    truncate_txt <- function(x, n = 900){
      x <- as.character(x %||% "")
      x <- gsub("\r\n|\n|\r", " ", x)
      x <- gsub("\\s+", " ", x)
      x <- trimws(x)
      
      if (!nzchar(x) || nchar(x) <= n) return(list(is_long = FALSE, short = x, full = x))
      
      short <- substr(x, 1, n)
      sp <- gregexpr("\\s", short)[[1]]
      sp <- sp[sp != -1]
      if (length(sp) > 0) short <- substr(short, 1, max(sp))
      
      list(is_long = TRUE, short = paste0(trimws(short), "…"), full = x)
    }
    
    # texte description
    dd <- truncate_txt(desc, n = 900)
    
    desc_short_html <- htmltools::HTML(esc_inline(dd$short))
    desc_full_html  <- htmltools::HTML(esc_inline(dd$full))
    
    # Hard skills (labels)
    hs_lbl <- get_hard_lbl(job, n = 12)
    
    # Soft skills (labels)
    ss_lbl <- get_soft_lbl(job, n = 12)
    
    # Avantages (labels)
    adv_lbl <- get_adv_lbl(job, n = 12)
    
    showModal(modalDialog(
      title = NULL,
      easyClose = TRUE,
      footer = NULL,
      
      div(class="offer-modal",
          
          # En-tête
          div(class="offer-modal-top",
              tags$h2(class="offer-modal-title", title),
              tags$div(class="offer-modal-sub",
                       paste0(comp, " • ", loc, if (nzchar(cp_fmt)) paste0(" (", cp_fmt, ")") else ""))
              ,
              
              div(class="pills",
                  if (nzchar(ct) && !is_missing_txt(ct)) span(class="pill gray", ct),
                  if (nzchar(tele_txt)) span(class="pill gray", tele_txt),
                  if (nzchar(pay_txt))  span(class="pill gray", pay_txt)
              )
          ),
          
          # Corps scrollable
          div(class="offer-modal-body",
              
              tags$h4("Description de l'offre"),
              div(class="offer-desc",
                  if (!dd$is_long) {
                    tags$span(class="desc-full", style="display:inline;", desc_full_html)
                  } else {
                    tagList(
                      # "…" et "Voir plus" sur la même ligne
                      tags$span(class="desc-short", style="display:inline;", desc_short_html),
                      tags$a(
                        href="#", class="desc-toggle",
                        "Voir plus",
                        onclick="var box=this.closest('.offer-desc'); \
                        box.classList.add('is-expanded'); \
                        box.scrollTop=0; \
                        box.querySelector('.desc-short').style.display='none'; \
                        box.querySelector('.desc-full').style.display='inline'; \
                        box.querySelector('.desc-less').style.display='inline'; \
                        this.style.display='none'; return false;"
                      ),
                      
                      div(class="desc-full", style="display:none;", desc_full_html),
                      tags$a(
                        href="#", class="desc-less", style="display:none; margin-left:12px;",
                        "Voir moins",
                        onclick="var box=this.closest('.offer-desc'); \
                        box.classList.remove('is-expanded'); \
                        box.scrollTop=0; \
                        box.querySelector('.desc-short').style.display='inline'; \
                        box.querySelector('.desc-full').style.display='none'; \
                        box.querySelector('.desc-toggle').style.display='inline'; \
                        this.style.display='none'; return false;"
                      )
                      
                    )
                  }
              ),
              
              tags$h4(class="offer-sec-title sec-hard",
                      tags$span(class="sec-ico", icon("chart-bar")),
                      "Hard skills"
              ),
              if (length(hs_lbl) > 0) div(class="pills",
                                          lapply(head(hs_lbl, 12), function(x) span(class="pill gray", x))
              ) else tags$p("—"),
              
              tags$h4(class="offer-sec-title sec-soft",
                      tags$span(class="sec-ico", icon("lightbulb")),
                      "Soft skills"
              ),
              if (length(ss_lbl) > 0) div(class="pills",
                                          lapply(ss_lbl, function(x) span(class="pill gray", x))
              ) else tags$p("—"),
              
              
              tags$h4(class="offer-sec-title sec-adv",
                      tags$span(class="sec-ico", icon("gem")),
                      "Avantages"
              ),
              if (length(adv_lbl) > 0) div(class="pills", lapply(adv_lbl, function(x) span(class="pill gray", x))) else tags$p("—")
              
          ),
          
          # Footer collé en bas
          div(class="offer-modal-footer",
              div(class="offer-visit",
                  tags$span(class="offer-visit-label", "Voir l’offre : "),
                  render_source_logo_links(sources, url)
              )
          )
      ),
      
      size = "l",
      class = "offer-modal-dialog"
    ))
    
  }, ignoreInit = TRUE)
  
  #############################################################################.
  # ONGLET 3 : MATCH PARFAIT ###################################################
  #############################################################################.
  ## Etat filtres appliqués (Match) --------------------------------------------
  applied_mp <- reactiveValues(
    mp_title = "",
    mp_loc   = "",
    
    mp_contract = "Tous",
    mp_duration_only = FALSE,
    mp_duration_range = NULL,
    
    mp_experience_level = "Tous",
    
    mp_salary_only  = FALSE,
    mp_salary_range = NULL,
    
    mp_remote = FALSE,
    
    mp_sector = character(0),
    mp_company_category = "Tous",
    mp_hard_skills = character(0),
    mp_soft_skills = character(0),
    mp_advantages = character(0),
    
    mp_date = "Toutes",
    mp_source = c("LinkedIn","Indeed","Welcome to the Jungle")
  )
  
  ## Initialisation filtres Match ----------------------------------------------
  .did_init_match <- FALSE
  session$onFlushed(function() {
    if (.did_init_match) return()
    .did_init_match <<- TRUE
    
    # Type de contrat
    if (has_col(jobs_df, "Contract_Type")) {
      ct <- trimws(as.character(jobs_df$Contract_Type))
      ct <- ct[!is.na(ct) & nzchar(ct) & !is_missing_txt(ct)]
      ct <- sort(unique(ct))
      updateSelectInput(session, "mp_contract", choices = c("Tous", ct), selected = "Tous")
    }
    
    # Secteur 
    if (has_col(jobs_df, "Sector")) {
      sectors <- trimws(as.character(jobs_df$Sector))
      sectors <- sectors[!is.na(sectors) & nzchar(sectors) & !is_missing_txt(sectors)]
      updateSelectizeInput(session, "mp_sector", choices = sort(unique(jobs_df$Category)), server = TRUE)
    }
    
    # Hard skills
    if (has_col(jobs_df, "Hard_Skills_Canon")) {
      updateSelectizeInput(session, "mp_hard_skills",
                           choices = hard_choices,
                           server = TRUE)
    }
    
    # Soft skills
    if (has_col(jobs_df, "Soft_Skills")) {
      updateSelectizeInput(session, "mp_soft_skills",
                           choices = SOFT_CHOICES,
                           server = TRUE)
    }
    
    
    # Avantages
    updateSelectizeInput(session, "mp_advantages",
                         choices = ADV_CHOICES,
                         server = TRUE)
    
    # Slider salaire (€/mois par défaut)
    if (salary_cols_ok(jobs_df)) {
      mo <- get_monthly_range(jobs_df)
      max_vec <- mo$max[is.finite(mo$max)]
      if (length(max_vec) > 0) {
        max_s <- ceiling(max(max_vec) / 500) * 500
        updateSliderInput(session, "mp_salary_range", min = 0, max = max_s, value = c(0, max_s), step = 500)
      }
    }
  }, once = TRUE)
  
  ## UI State Match (toggles, freelance €/h et durée) --------------------------
  apply_match_ui_state <- function() {
    
    sal_only <- FALSE
    if (!is.null(isolate(input$mp_salary_only))) sal_only <- isTRUE(isolate(input$mp_salary_only))
    
    contract <- "Tous"
    if (!is.null(isolate(input$mp_contract))) contract <- isolate(input$mp_contract)
    
    dur_only <- FALSE
    if (!is.null(isolate(input$mp_duration_only))) dur_only <- isTRUE(isolate(input$mp_duration_only))
    
    toggle_slider("mp_salary_range", disabled = !sal_only)
    
    dur_ok <- dur_only && (contract %in% c("Stage", "CDD"))
    toggle_slider("mp_duration_range", disabled = !dur_ok)
    
    if (isTRUE(tolower(contract) == "freelance")) {
      set_text("mp_salary_title", "Fourchette de salaire (€ / h)")
    } else {
      set_text("mp_salary_title", "Fourchette de salaire (€ / mois)")
    }
  }
  
  observeEvent(input$nav, {
    if (identical(input$nav, "Match Parfait")) apply_match_ui_state()
  }, ignoreInit = FALSE)
  
  observeEvent(input$mp_salary_only, {
    toggle_slider("mp_salary_range", disabled = !isTRUE(input$mp_salary_only))
  }, ignoreInit = TRUE)
  
  observeEvent(input$mp_contract, {
    req(input$mp_contract)
    
    if (isTRUE(tolower(input$mp_contract) == "freelance")) {
      set_text("mp_salary_title", "Fourchette de salaire (€ /h)")
    } else {
      set_text("mp_salary_title", "Fourchette de salaire (€ /mois)")
    }
    
    if (!salary_cols_ok(jobs_df)) {
      apply_match_ui_state()
      return()
    }
    
    if (isTRUE(tolower(input$mp_contract) == "freelance")) {
      hr <- get_hourly_range(jobs_df)
      mx <- max(hr$max[is.finite(hr$max)], na.rm = TRUE)
      if (is.finite(mx)) {
        updateSliderInput(session, "mp_salary_range", min = 0, max = ceiling(mx), value = c(0, ceiling(mx)), step = 1)
      }
    } else {
      mo <- get_monthly_range(jobs_df)
      mx <- max(mo$max[is.finite(mo$max)], na.rm = TRUE)
      if (is.finite(mx)) {
        mx2 <- ceiling(mx / 500) * 500
        updateSliderInput(session, "mp_salary_range", min = 0, max = mx2, value = c(0, mx2), step = 500)
      }
    }
    
    apply_match_ui_state()
  }, ignoreInit = TRUE)
  
  # Sources : "Tous" (Match)
  observeEvent(input$mp_source_all, {
    v <- isTRUE(input$mp_source_all)
    updateCheckboxInput(session, "mp_source_li", value = v)
    updateCheckboxInput(session, "mp_source_in", value = v)
    updateCheckboxInput(session, "mp_source_wttj", value = v)
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$mp_source_li, input$mp_source_in, input$mp_source_wttj), {
    all_on <- isTRUE(input$mp_source_li) && isTRUE(input$mp_source_in) && isTRUE(input$mp_source_wttj)
    if (!identical(isTRUE(input$mp_source_all), all_on)) {
      updateCheckboxInput(session, "mp_source_all", value = all_on)
    }
  }, ignoreInit = TRUE)
  
  # Durée Stage/CDD (Match)
  mp_duration_limits <- reactiveValues(min = 0, max = 24)
  
  observeEvent(input$mp_contract, {
    req(input$mp_contract)
    
    if (!(input$mp_contract %in% c("Stage", "CDD"))) {
      updateCheckboxInput(session, "mp_duration_only", value = FALSE)
      toggle_slider("mp_duration_range", disabled = TRUE)
      return()
    }
    
    mn <- NA_real_; mx <- NA_real_
    if (input$mp_contract == "Stage" &&
        has_col(jobs_df, "Stage_Duration_Min") && has_col(jobs_df, "Stage_Duration_Max")) {
      mn <- suppressWarnings(min(num_clean(jobs_df$Stage_Duration_Min), na.rm = TRUE))
      mx <- suppressWarnings(max(num_clean(jobs_df$Stage_Duration_Max), na.rm = TRUE))
    } else if (input$mp_contract == "CDD" &&
               has_col(jobs_df, "CDD_Duration_Min") && has_col(jobs_df, "CDD_Duration_Max")) {
      mn <- suppressWarnings(min(num_clean(jobs_df$CDD_Duration_Min), na.rm = TRUE))
      mx <- suppressWarnings(max(num_clean(jobs_df$CDD_Duration_Max), na.rm = TRUE))
    } else {
      toggle_slider("mp_duration_range", disabled = TRUE)
      return()
    }
    
    if (!is.finite(mn) || !is.finite(mx) || mx < mn) {
      toggle_slider("mp_duration_range", disabled = TRUE)
      return()
    }
    
    mp_duration_limits$min <- mn
    mp_duration_limits$max <- mx
    
    updateSliderInput(session, "mp_duration_range", min = mn, max = mx, value = c(mn, mx), step = 1)
    toggle_slider("mp_duration_range", disabled = !isTRUE(input$mp_duration_only))
  }, ignoreInit = TRUE)
  
  observeEvent(input$mp_duration_only, {
    if (!isTRUE(input$mp_duration_only)) {
      updateSliderInput(session, "mp_duration_range", value = c(mp_duration_limits$min, mp_duration_limits$max))
    }
    toggle_slider(
      "mp_duration_range",
      disabled = !(isTRUE(input$mp_duration_only) && (input$mp_contract %in% c("Stage","CDD")))
    )
  }, ignoreInit = TRUE)
  
  ## Extraction skills depuis PDF ----------------------------------------------
  extract_skills_from_cv <- function(pdf_path, skills_vocab){
    if (is.null(pdf_path) || !nzchar(pdf_path)) return(character(0))
    if (!requireNamespace("pdftools", quietly = TRUE)) return(character(0))
    
    txt <- paste(pdftools::pdf_text(pdf_path), collapse = " ")
    txt <- tolower(txt)
    
    vocab <- unique(skills_vocab)
    vocab <- vocab[!is.na(vocab) & nzchar(vocab)]
    
    vocab[vapply(vocab, function(s){
      str_detect(txt, fixed(tolower(s)))
    }, logical(1))]
  }
  
  # Extraction soft skills depuis PDF (basé sur soft_taxo patterns)
  extract_softskills_from_cv <- function(pdf_path){
    if (is.null(pdf_path) || !nzchar(pdf_path)) return(character(0))
    if (!requireNamespace("pdftools", quietly = TRUE)) return(character(0))
    if (!exists("soft_taxo")) return(character(0))
    
    txt <- paste(pdftools::pdf_text(pdf_path), collapse = " ")
    txt_low <- tolower(txt)
    
    txt_ascii <- txt_low
    if (requireNamespace("stringi", quietly = TRUE)) {
      txt_ascii <- stringi::stri_trans_general(txt_low, "Latin-ASCII")
    } else {
      txt_ascii <- iconv(txt_low, from = "", to = "ASCII//TRANSLIT")
    }
    
    pat <- soft_taxo$pattern
    keys <- soft_taxo$soft_key
    
    hit <- vapply(seq_along(pat), function(i){
      stringr::str_detect(txt_low, pat[i]) || stringr::str_detect(txt_ascii, pat[i])
    }, logical(1))
    
    unique(keys[hit])
  }
  
  has_mp_cv <- reactive({
    !is.null(input$mp_cv) &&
      !is.null(input$mp_cv$datapath) &&
      nzchar(input$mp_cv$datapath)
  })
  
  ## Quand on change/ajoute un CV, on enlève l'erreur visuelle
  observeEvent(input$mp_cv, {
    session$sendCustomMessage("mpCvError", list(on = FALSE))
  }, ignoreInit = TRUE)
  
  
  ## mp_run : applique tout et parse CV
  observeEvent(input$mp_run, {
    
    cv_name <- if (!is.null(input$mp_cv) && !is.null(input$mp_cv$name)) {
      as.character(input$mp_cv$name)
    } else ""
    
    cv_path <- if (!is.null(input$mp_cv) && !is.null(input$mp_cv$datapath)) {
      as.character(input$mp_cv$datapath)
    } else ""
    
    has_name <- nzchar(cv_name)
    has_path <- nzchar(cv_path) && file.exists(cv_path)
    
    # Aucun fichier sélectionné >> erreur rouge et stop
    if (!has_name) {
      session$sendCustomMessage("mpCvError", list(on = TRUE))
      session$sendCustomMessage("mpLoading", FALSE)
      return()
    }
    
    # Fichier sélectionné mais upload pas encore prêt >> pas de rouge, pas de spinner
    if (!has_path) {
      session$sendCustomMessage("mpCvError", list(on = FALSE))
      return()
    }
    
    # CV OK > on enlève l'erreur + on lance le spinner
    session$sendCustomMessage("mpCvError", list(on = FALSE))
    session$sendCustomMessage("mpLoading", TRUE)
    
    rv$mp_page <- 1
    
    tt <- input$mp_title
    ll <- input$mp_loc
    
    if (is.null(tt)) tt <- character(0)
    if (is.null(ll)) ll <- character(0)
    
    applied_mp$mp_title <- trimws(as.character(tt))
    applied_mp$mp_loc   <- trimws(as.character(ll))
    
    applied_mp$mp_contract       <- input$mp_contract
    applied_mp$mp_duration_only  <- isTRUE(input$mp_duration_only)
    applied_mp$mp_duration_range <- input$mp_duration_range
    
    applied_mp$mp_experience_level <- input$mp_experience_level
    
    applied_mp$mp_salary_only  <- isTRUE(input$mp_salary_only)
    applied_mp$mp_salary_range <- input$mp_salary_range
    
    applied_mp$mp_remote <- isTRUE(input$mp_remote)
    
    applied_mp$mp_sector           <- input$mp_sector
    applied_mp$mp_company_category <- input$mp_company_category
    applied_mp$mp_hard_skills <- canonize_vec(input$mp_hard_skills)
    applied_mp$mp_soft_skills      <- input$mp_soft_skills
    applied_mp$mp_advantages       <- input$mp_advantages
    applied_mp$mp_date             <- input$mp_date
    
    src <- c()
    if (isTRUE(input$mp_source_li))   src <- c(src, "LinkedIn")
    if (isTRUE(input$mp_source_in))   src <- c(src, "Indeed")
    if (isTRUE(input$mp_source_wttj)) src <- c(src, "Welcome to the Jungle")
    applied_mp$mp_source <- src
    
    # Parse CV (on est sûr que cv_path existe ici)
    vocab <- sort(unique(split_tokens(jobs_df$Hard_Skills)))
    rv$mp_cv_terms <- extract_skills_from_cv(cv_path, vocab)
    rv$mp_cv_hard_terms <- canonize_vec(rv$mp_cv_terms)
    rv$mp_cv_soft_terms <- extract_softskills_from_cv(cv_path)
    
    rv$mp_run_ok <- input$mp_run
    
    # Coupe le spinner dès que Shiny a flush le rendu
    session$onFlushed(function() {
      session$sendCustomMessage("mpLoading", FALSE)
    }, once = TRUE)
    
  }, ignoreInit = TRUE)
  
  ## Filtrage Match (copie Explorateur, version Match) -------------------------
  mp_filtered_jobs <- reactive({
    data <- jobs_df
    
    # Objectif titre
    if (length(applied_mp$mp_title) > 0) {
      title_terms <- as.character(applied_mp$mp_title %||% character(0))
      title_terms <- trimws(title_terms)
      title_terms <- title_terms[!is.na(title_terms) & nzchar(title_terms)]
      
      ok_title <- if (has_col(data, "Job_Title"))   match_any_term(data$Job_Title, title_terms) else rep(FALSE, nrow(data))
      ok_comp  <- if (has_col(data, "Company"))     match_any_term(data$Company, title_terms) else rep(FALSE, nrow(data))
      ok_hard  <- if (has_col(data, "Hard_Skills")) match_any_term(data$Hard_Skills, title_terms) else rep(FALSE, nrow(data))
      data <- data[ok_title | ok_comp | ok_hard, ]
    }
    
    
    # Objectif localisation (NA-safe + accepte multi-valeurs)
    loc_terms <- applied_mp$mp_loc
    loc_terms <- as.character(loc_terms %||% character(0))
    loc_terms <- trimws(loc_terms)
    loc_terms <- loc_terms[!is.na(loc_terms) & nzchar(loc_terms)]
    if (length(loc_terms) > 0 && has_col(data, "Location")) {
      loc_search <- loc_key_vec(data)
      data <- data[match_any_term(loc_search, loc_terms), ]
    }
    
    
    # Type contrat
    if (!is.null(applied_mp$mp_contract) && applied_mp$mp_contract != "Tous" && has_col(data, "Contract_Type")) {
      sel <- toupper(trimws(applied_mp$mp_contract))
      keep <- toupper(trimws(as.character(data$Contract_Type))) == sel
      data <- data[keep, ]
    }
    
    # Durée Stage/CDD
    if (isTRUE(applied_mp$mp_duration_only) &&
        !is.null(applied_mp$mp_duration_range) &&
        (applied_mp$mp_contract %in% c("Stage", "CDD"))) {
      
      umin <- as.numeric(applied_mp$mp_duration_range[1])
      umax <- as.numeric(applied_mp$mp_duration_range[2])
      
      if (applied_mp$mp_contract == "Stage" &&
          has_col(data, "Stage_Duration_Min") && has_col(data, "Stage_Duration_Max")) {
        dmin <- num_clean(data$Stage_Duration_Min)
        dmax <- num_clean(data$Stage_Duration_Max)
        keep <- !is.na(dmin) & !is.na(dmax) & (dmin <= umax) & (dmax >= umin)
        data <- data[keep, ]
      }
      
      if (applied_mp$mp_contract == "CDD" &&
          has_col(data, "CDD_Duration_Min") && has_col(data, "CDD_Duration_Max")) {
        dmin <- num_clean(data$CDD_Duration_Min)
        dmax <- num_clean(data$CDD_Duration_Max)
        keep <- !is.na(dmin) & !is.na(dmax) & (dmin <= umax) & (dmax >= umin)
        data <- data[keep, ]
      }
    }
    
    # Expérience
    if (!is.null(applied_mp$mp_experience_level) &&
        applied_mp$mp_experience_level != "Tous" &&
        has_col(data, "Experience_Level")) {
      
      exp_raw <- tolower(trimws(as.character(data$Experience_Level)))
      target  <- tolower(trimws(as.character(applied_mp$mp_experience_level)))
      keep <- !is.na(exp_raw) & exp_raw == target
      data <- data[keep, ]
    }
    
    # Salaire
    if (isTRUE(applied_mp$mp_salary_only) && salary_cols_ok(data) && !is.null(applied_mp$mp_salary_range)) {
      sel_min <- as.numeric(applied_mp$mp_salary_range[1])
      sel_max <- as.numeric(applied_mp$mp_salary_range[2])
      
      if (!is.null(applied_mp$mp_contract) && tolower(applied_mp$mp_contract) == "freelance") {
        hr <- get_hourly_range(data)
        keep <- is.finite(hr$min) & is.finite(hr$max) & (hr$max >= sel_min) & (hr$min <= sel_max)
        data <- data[keep, ]
      } else {
        mo <- get_monthly_range(data)
        keep <- is.finite(mo$min) & is.finite(mo$max) & (mo$max >= sel_min) & (mo$min <= sel_max)
        data <- data[keep, ]
      }
    }
    
    # Télétravail
    if (isTRUE(applied_mp$mp_remote) && has_col(data, "Is_Remote")) {
      keep <- vapply(data$Is_Remote, is_remote_true, logical(1))
      data <- data[keep, ]
    }
    
    # Secteur
    if (length(applied_mp$mp_sector) > 0 && has_col(data, "Category")) {
      data[, Category := clean_txt(Category)]
      sel <- clean_txt(applied_mp$mp_sector)
      data <- data[Category %in% sel]
    }
    
    # Taille entreprise
    if (!is.null(applied_mp$mp_company_category) &&
        applied_mp$mp_company_category != "Tous" &&
        has_col(data, "Company_Category")) {
      keep <- as.character(data$Company_Category) == applied_mp$mp_company_category
      data <- data[keep, ]
    }
    
    # Hard skills
    if (length(applied_mp$mp_hard_skills) > 0 && has_col(data, "Hard_Skills_Canon")) {
      data <- data[match_any_skill(data$Hard_Skills_Canon, applied_mp$mp_hard_skills), ]
    }
    
    # Soft skills
    if (length(applied_mp$mp_soft_skills) > 0) {
      if (has_col(data, "Soft_Skills_Canon")) {
        data <- data[match_any_term(data$Soft_Skills_Canon, applied_mp$mp_soft_skills), ]
      } else if (has_col(data, "Soft_Skills")) {
        data <- data[match_any_term(data$Soft_Skills, applied_mp$mp_soft_skills), ]
      }
    }
    
    
    # Avantages
    if (length(applied_mp$mp_advantages) > 0) {
      if (has_col(data, "Advantages_Canon")) {
        data <- data[match_any_adv(data$Advantages_Canon, applied_mp$mp_advantages), ]
      } else {
        adv_col <- get_adv_col(data)
        if (!is.na(adv_col)) data <- data[match_any_term(data[[adv_col]], applied_mp$mp_advantages), ]
      }
    }
    
    
    
    # Date publication
    if (!is.null(applied_mp$mp_date) && applied_mp$mp_date != "Toutes" && has_col(data, "Publish_Date")) {
      keep_days <- switch(applied_mp$mp_date,
                          "Depuis 24h"       = 1,
                          "Depuis 3 jours"   = 3,
                          "Depuis 1 semaine" = 7,
                          "Depuis 1 mois"    = 30,
                          NA_integer_
      )
      if (!is.na(keep_days)) {
        cutoff <- Sys.Date() - keep_days
        data <- data[!is.na(Publish_Date) & Publish_Date >= cutoff, ]
      }
    }
    
    # Source site
    if (!is.null(applied_mp$mp_source) && has_col(data, "occurrence_sites")) {
      if (length(applied_mp$mp_source) == 0) {
        data <- data[0, ]
      } else {
        keep <- match_any_source(data$occurrence_sites, applied_mp$mp_source)
        data <- data[keep, ]
      }
    }
    
    data
  })
  
  ## Tri Match -----------------------------------------------------------------
  mp_sorted_jobs <- reactive({
    data <- data.table::copy(mp_filtered_jobs())
    if (is.null(data) || nrow(data) == 0) return(data)
    
    data[, .idx := .I]
    
    sm <- normalize_sort_mode(input$mp_sort)
    if (is.null(sm) || is.na(sm) || !nzchar(sm) || identical(sm, "relevance")) {
      data[, .idx := NULL]
      return(data)
    }
    
    # Salaire : décroissant
    if (identical(sm, "salary_desc") && salary_cols_ok(data)) {
      if (!is.null(applied_mp$mp_contract) && !is.na(applied_mp$mp_contract) &&
          tolower(applied_mp$mp_contract) == "freelance") {
        hr <- get_hourly_range(data)
        data[, .sal := hr$max]
      } else {
        mo <- get_monthly_range(data)
        data[, .sal := mo$max]
      }
      data <- data[order(is.na(.sal), -.sal, .idx)]
      data[, .sal := NULL]
    }
    
    # Salaire : croissant
    if (identical(sm, "salary_asc") && salary_cols_ok(data)) {
      if (!is.null(applied_mp$mp_contract) && !is.na(applied_mp$mp_contract) &&
          tolower(applied_mp$mp_contract) == "freelance") {
        hr <- get_hourly_range(data)
        data[, .sal := hr$max]
      } else {
        mo <- get_monthly_range(data)
        data[, .sal := mo$max]
      }
      data <- data[order(is.na(.sal), .sal, .idx)]
      data[, .sal := NULL]
    }
    
    # Date : décroissant
    if (identical(sm, "date_desc") && has_col(data, "Publish_Date")) {
      data <- data[order(is.na(Publish_Date), -as.numeric(Publish_Date), .idx)]
    }
    
    # Date : croissant
    if (identical(sm, "date_asc") && has_col(data, "Publish_Date")) {
      data <- data[order(is.na(Publish_Date),  as.numeric(Publish_Date), .idx)]
    }
    
    data[, .idx := NULL]
    data
  })
  
  ## Graphique -----------------------------------------------------------------
  radar_cats <- c(
    "Langages & Scripting",
    "Manipulation & Stockage",
    "IA & Statistiques",
    "Visualisation & BI",
    "Big Data & Cloud",
    "Méthodologie & Workflow"
  )
  
  skill_taxo <- data.table::data.table(
    pattern = c(
      "power\\s*bi|tableau|looker|qlik|superset",
      "sql|postgres|mysql|bigquery|snowflake",
      "nosql|mongodb|cassandra|redis",
      "python|\\br\\b|pandas|numpy",
      "spark|pyspark|hadoop|databricks",
      "aws|azure|gcp|google\\s*cloud",
      "ml|machine\\s*learning|scikit|tensorflow|pytorch|nlp|stat|statistics",
      "git|github|gitlab|airflow|dbt|etl|pipeline|workflow|agile|scrum|kanban"
    ),
    cat = c(
      "Visualisation & BI",
      "Manipulation & Stockage",
      "Manipulation & Stockage",
      "Langages & Scripting",
      "Big Data & Cloud",
      "Big Data & Cloud",
      "IA & Statistiques",
      "Méthodologie & Workflow"
    )
  )
  
  norm_skill <- function(x) tolower(trimws(as.character(x)))
  
  map_cat_one <- function(tok){
    t <- norm_skill(tok)
    hit <- skill_taxo[stringr::str_detect(t, pattern)][1]
    if (is.null(hit) || nrow(hit) == 0) return(NA_character_)
    hit$cat
  }
  
  ## Mesure "marché" et score radar --------------------------------------------
  ### Fréquence des skills demandées sur les offres filtrées ----
  market_skill_freq <- function(df_offers){
    if (is.null(df_offers) || nrow(df_offers) == 0) return(data.table::data.table())
    if (!("Hard_Skills" %in% names(df_offers))) return(data.table::data.table())
    if (!("id" %in% names(df_offers))) return(data.table::data.table())
    
    toks <- lapply(df_offers$Hard_Skills, function(x) unique(norm_skill(split_tokens(x))))
    
    long <- data.table::data.table(
      offer_id = rep(df_offers$id, lengths(toks)),
      skill    = unlist(toks, use.names = FALSE)
    )
    
    long <- unique(long, by = c("offer_id", "skill"))
    long[, .N, by = skill][order(-N)]
  }

  # Variante "catégorisée" (hard + soft) utilisée par mp_advice
  # Doit renvoyer au minimum: cat, skill, N
  market_skill_freq_cat <- function(df_offers){
    if (is.null(df_offers) || nrow(df_offers) == 0) return(data.table::data.table())
    if (!("id" %in% names(df_offers))) return(data.table::data.table())

    out_list <- list()

    # Hard skills
    hard_col <- if ("Hard_Skills_Canon" %in% names(df_offers)) "Hard_Skills_Canon" else "Hard_Skills"
    if (hard_col %in% names(df_offers)) {
      toks <- lapply(df_offers[[hard_col]], function(x) unique(norm_skill(split_tokens(x))))
      long <- data.table::data.table(
        offer_id = rep(df_offers$id, lengths(toks)),
        cat      = "hard",
        skill    = unlist(toks, use.names = FALSE)
      )
      out_list <- c(out_list, list(long))
    }

    # Soft skills
    soft_col <- if ("Soft_Skills_Canon" %in% names(df_offers)) "Soft_Skills_Canon" else "Soft_Skills"
    if (soft_col %in% names(df_offers)) {
      toks <- lapply(df_offers[[soft_col]], function(x) unique(norm_skill(split_tokens(x))))
      long <- data.table::data.table(
        offer_id = rep(df_offers$id, lengths(toks)),
        cat      = "soft",
        skill    = unlist(toks, use.names = FALSE)
      )
      out_list <- c(out_list, list(long))
    }

    if (length(out_list) == 0) return(data.table::data.table())

    long_all <- data.table::rbindlist(out_list, use.names = TRUE, fill = TRUE)
    long_all <- long_all[!is.na(skill) & nzchar(skill)]
    if (nrow(long_all) == 0) return(data.table::data.table())

    long_all <- unique(long_all, by = c("offer_id", "cat", "skill"))
    long_all[, .N, by = .(cat, skill)][order(-N)]
  }
  
  ### Calcul note basé sur la couverture de demande ----
  compute_radar_scores <- function(df_offers, user_skills){
    if (is.null(df_offers) || nrow(df_offers) == 0) return(rep(0, length(radar_cats)))
    if (!("Hard_Skills" %in% names(df_offers)))      return(rep(0, length(radar_cats)))
    if (!("id" %in% names(df_offers)))              return(rep(0, length(radar_cats)))
    
    toks <- lapply(df_offers$Hard_Skills, function(x) unique(norm_skill(split_tokens(x))))
    long <- data.table::data.table(
      offer_id = rep(df_offers$id, lengths(toks)),
      skill    = unlist(toks, use.names = FALSE)
    )
    
    long[, cat := vapply(skill, map_cat_one, character(1))]
    long <- long[!is.na(cat) & cat %in% radar_cats]
    
    freq <- unique(long, by = c("offer_id","cat","skill"))[, .N, by = .(cat, skill)]
    totals <- freq[, .(total = sum(N)), by = cat]
    
    u <- unique(norm_skill(user_skills))
    hits <- freq[skill %in% u, .(hit = sum(N)), by = cat]
    
    out <- merge(totals, hits, by = "cat", all.x = TRUE)
    out[is.na(hit), hit := 0]
    out[, score := ifelse(total > 0, 10 * hit / total, 0)]
    
    res <- out$score[match(radar_cats, out$cat)]
    res[is.na(res)] <- 0
    pmin(10, pmax(0, res))
  }

  # Profil idéal = intensité de demande par catégorie (selon offres filtrées)
  # Normalisé sur [0,10] avec max(cat)=10 pour refléter la "forme" du marché ciblé.
  compute_ideal_profile <- function(df_offers){
    if (is.null(df_offers) || nrow(df_offers) == 0) return(rep(0, length(radar_cats)))
    if (!("Hard_Skills" %in% names(df_offers)))      return(rep(0, length(radar_cats)))
    if (!("id" %in% names(df_offers)))              return(rep(0, length(radar_cats)))

    toks <- lapply(df_offers$Hard_Skills, function(x) unique(norm_skill(split_tokens(x))))
    long <- data.table::data.table(
      offer_id = rep(df_offers$id, lengths(toks)),
      skill    = unlist(toks, use.names = FALSE)
    )

    long[, cat := vapply(skill, map_cat_one, character(1))]
    long <- long[!is.na(cat) & cat %in% radar_cats]
    if (nrow(long) == 0) return(rep(0, length(radar_cats)))

    freq <- unique(long, by = c("offer_id","cat","skill"))[, .N, by = .(cat, skill)]
    totals <- freq[, .(total = sum(N)), by = cat]

    tvec <- totals$total[match(radar_cats, totals$cat)]
    tvec[is.na(tvec)] <- 0
    mx <- suppressWarnings(max(tvec, na.rm = TRUE))
    if (!is.finite(mx) || mx <= 0) return(rep(0, length(radar_cats)))

    pmin(10, pmax(0, 10 * tvec / mx))
  }
  
  ## Output radar --------------------------------------------------------------
  output$mp_radar <- plotly::renderPlotly({
    req(rv$mp_run_ok > 0)
    
    
    offers_used <- mp_sorted_jobs()
    
    # Skills utilisateur = sélection + CV
    user_skills <- unique(c(applied_mp$mp_hard_skills, rv$mp_cv_terms))
    user_skills <- user_skills[!is.na(user_skills) & nzchar(user_skills)]
    
    r_profil <- compute_radar_scores(offers_used, user_skills)
    r_ideal  <- compute_ideal_profile(offers_used)
    
    plotly::plot_ly(type = "scatterpolar", fill = "toself") %>%
      plotly::add_trace(
        r = r_profil, theta = radar_cats,
        name = "Votre profil",
        line = list(color = "#006eef", width = 2),
        marker = list(color = "#006eef"),
        fillcolor = "rgba(0,110,239,0.25)"
      ) %>%
      plotly::add_trace(
        r = r_ideal, theta = radar_cats,
        name = "Profil idéal",
        line = list(color = "#6a7375", width = 2),
        marker = list(color = "#6a7375"),
        fillcolor = "rgba(106,115,117,0.20)"
      ) %>%
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        margin = list(l = 30, r = 30, t = 10, b = 70),
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.25, yanchor = "top",
          traceorder = "normal"
        ),
        polar = list(
          bgcolor = "rgba(0,0,0,0)",
          radialaxis = list(visible = TRUE, range = c(0, 10))
        )
      )
  })
  
  output$mp_radar_box <- renderUI({
    # AVANT clic
    if (is.null(rv$mp_run_ok) || !is.finite(rv$mp_run_ok) || rv$mp_run_ok <= 0) {
      div(class = "mp-radar-placeholder",
          tags$h4("Votre diagnostic apparaîtra ici"),
          tags$p("Déposez votre CV (PDF) puis cliquez sur “Lancer l’analyse”."),
          tags$p("Nous comparerons vos compétences aux offres filtrées pour estimer votre adéquation et proposer des axes de progression."),
          tags$p(tags$strong("Ensuite, en bas de la page, apparaîtront les 3 meilleures offres"),
                 " selon votre profil et le type d’offre visé.")
      )
    } else {
      # APRES clic
      plotly::plotlyOutput("mp_radar", height = "260px")
    }
  })
  
  ## Output conseils -----------------------------------------------------------
  output$mp_advice <- renderUI({
    req(rv$mp_run_ok > 0)
    
    offers_used <- mp_sorted_jobs()
    
    # CV (hard/soft) : référence principale pour points forts / manquants
    cv_hard_can <- canonize_vec(rv$mp_cv_hard_terms %||% rv$mp_cv_terms %||% character(0))
    cv_soft_key <- unique(as.character(rv$mp_cv_soft_terms %||% character(0)))
    cv_soft_key <- cv_soft_key[!is.na(cv_soft_key) & nzchar(cv_soft_key)]
    
    # Si CV vide, fallback sur sélection utilisateur
    sel_hard_can <- canonize_vec(applied_mp$mp_hard_skills %||% character(0))
    sel_soft_key <- map_soft_vec(applied_mp$mp_soft_skills %||% character(0))
    
    cv_empty <- (length(cv_hard_can) == 0 && length(cv_soft_key) == 0)
    
    user_hard_base <- if (cv_empty) sel_hard_can else cv_hard_can
    user_soft_base <- if (cv_empty) sel_soft_key else cv_soft_key
    
    # Fréquence = dans combien d'offres le skill apparaît (selon offres filtrées)
    freq_hard <- data.table::data.table()
    if (!is.null(offers_used) && nrow(offers_used) > 0) {
      hard_toks <- lapply(seq_len(nrow(offers_used)), function(i){
        unique(get_hard_can(offers_used[i]))
      })
      freq_hard <- data.table::data.table(
        offer_id = rep(offers_used$id, lengths(hard_toks)),
        key      = unlist(hard_toks, use.names = FALSE)
      )
      freq_hard <- freq_hard[!is.na(key) & nzchar(key)]
      freq_hard <- unique(freq_hard, by = c("offer_id","key"))[, .N, by = key][order(-N)]
    }
    
    freq_soft <- data.table::data.table()
    if (!is.null(offers_used) && nrow(offers_used) > 0) {
      soft_toks <- lapply(seq_len(nrow(offers_used)), function(i){
        unique(get_soft_can(offers_used[i]))
      })
      freq_soft <- data.table::data.table(
        offer_id = rep(offers_used$id, lengths(soft_toks)),
        key      = unlist(soft_toks, use.names = FALSE)
      )
      freq_soft <- freq_soft[!is.na(key) & nzchar(key)]
      freq_soft <- unique(freq_soft, by = c("offer_id","key"))[, .N, by = key][order(-N)]
    }
    
    # Assemble + labels
    freq_all <- data.table::rbindlist(list(
      if (nrow(freq_hard) > 0) data.table::data.table(type = "hard", key = freq_hard$key, N = freq_hard$N) else NULL,
      if (nrow(freq_soft) > 0) data.table::data.table(type = "soft", key = freq_soft$key, N = freq_soft$N) else NULL
    ), use.names = TRUE, fill = TRUE)
    
    if (nrow(freq_all) > 0) {
      freq_all[, in_cv := (type == "hard" & key %in% user_hard_base) | (type == "soft" & key %in% user_soft_base)]
      freq_all[, label := ifelse(
        type == "hard",
        labelize_vec(key),
        labelize_soft_vec(key)
      )]
    }
    
    # Points forts = Top 3 très demandés ET présents dans le CV (ou sélection si CV vide)
    strong <- character(0)
    if (nrow(freq_all) > 0) {
      # Convertit en data.frame pour éviter toute ambiguïté data.table sur j/i
      ff <- as.data.frame(freq_all)
      ff_in  <- ff[isTRUE(ff$in_cv) | ff$in_cv == TRUE, , drop = FALSE]
      ff_in  <- ff_in[order(-ff_in$N), , drop = FALSE]
      strong <- head(ff_in$label, 3)
    }
    strong <- strong[!is.na(strong) & nzchar(strong)]
    
    # Axes de progression = Top 3 très demandés MAIS absents du CV (ou sélection si CV vide)
    missing <- character(0)
    if (nrow(freq_all) > 0) {
      ff <- as.data.frame(freq_all)
      ff_out <- ff[!(ff$in_cv %in% TRUE), , drop = FALSE]
      ff_out <- ff_out[order(-ff_out$N), , drop = FALSE]
      missing <- head(ff_out$label, 3)
    }
    missing <- missing[!is.na(missing) & nzchar(missing)]
    
    tagList(
      tags$h3("Conseils personnalisés"),
      
      tags$div(style="margin-top:10px;", tags$strong("Vos Points Forts :")),
      if (length(strong) > 0) div(class="pills", lapply(strong, function(x) span(class="pill blue", x))) else tags$p("—"),
      
      tags$div(style="margin-top:14px;", tags$strong("Axes de Progression :")),
      if (length(missing) > 0) div(class="pills", lapply(missing, function(x) span(class="pill blue", x))) else tags$p("—")
    )
  })
  
  #############################################################################.
  # TOP 3 RECOMMANDATIONS (AVEC SCORE) #########################################
  #############################################################################.
  
  # Score (0-100) basé sur la couverture des hard skills de l'offre
  mp_offer_score <- function(job_row, user_hard_can){
    offer_can <- get_hard_can(job_row)
    offer_can <- offer_can[!is.na(offer_can) & nzchar(offer_can)]
    if (!length(offer_can)) return(0)
    
    user_hard_can <- canonize_vec(user_hard_can)
    user_hard_can <- user_hard_can[!is.na(user_hard_can) & nzchar(user_hard_can)]
    if (!length(user_hard_can)) return(0)
    
    100 * length(intersect(unique(offer_can), unique(user_hard_can))) / length(unique(offer_can))
  }
  
  mp_top3_jobs <- reactive({
    req(rv$mp_run_ok > 0)
    
    d <- data.table::copy(mp_filtered_jobs())
    if (is.null(d) || nrow(d) == 0) return(d)
    
    # Skills utilisateur = sélection + CV (hard skills)
    user_hard <- unique(c(applied_mp$mp_hard_skills, rv$mp_cv_terms))
    user_hard <- user_hard[!is.na(user_hard) & nzchar(user_hard)]
    user_hard_can <- canonize_vec(user_hard)
    
    d[, .mp_score := vapply(seq_len(.N), function(i) mp_offer_score(d[i], user_hard_can), numeric(1))]
    d <- d[order(-.mp_score)]
    head(d, 3)
  })
  
  output$mp_count <- renderText({
    if (is.null(rv$mp_run_ok) || !is.finite(rv$mp_run_ok) || rv$mp_run_ok <= 0) return("")
    d <- mp_top3_jobs()
    n <- if (is.null(d)) 0 else nrow(d)
    paste0(n, " recommandation", ifelse(n > 1, "s", ""))
  })
  
  output$mp_top3 <- renderUI({
    if (is.null(rv$mp_run_ok) || !is.finite(rv$mp_run_ok) || rv$mp_run_ok <= 0) {
      return(div(class = "mp-reco-empty", "Lancez l’analyse pour afficher vos 3 meilleures recommandations."))
    }
    
    d <- mp_top3_jobs()
    if (is.null(d) || nrow(d) == 0) {
      return(div(class = "mp-reco-empty", "Aucune offre trouvée avec les filtres actuels."))
    }
    
    # Classe badge cohérente avec le reste de l'app
    badge_cls_from_score <- function(p){
      if (!is.finite(p)) return("is-gray")
      if (p >= 70) return("is-green")
      if (p >= 45) return("is-orange")
      if (p >  0) return("is-red")
      "is-gray"
    }
    
    # Petit helper "safe" pour éviter if(NA)
    is_nz1 <- function(x){
      if (is.null(x) || length(x) == 0) return(FALSE)
      x <- as.character(x[1])
      isTRUE(!is.na(x) && nzchar(trimws(x)))
    }
    
    tagList(
      lapply(seq_len(nrow(d)), function(i){
        job <- d[i]
        
        score <- job$.mp_score
        badge_txt <- if (is.finite(score)) paste0(round(score), "% Match") else "Match —"
        badge_cls <- badge_cls_from_score(score)
        
        is_fav <- as.numeric(job$id) %in% rv$favorites
        
        title <- pick_col(job, c("Job_Title","Title"))
        comp  <- pick_col(job, c("Company","Company_Name"))
        loc   <- pick_col(job, c("Location","City","Region"))
        cp_raw <- pick_col(job, c("Code_Postal","CP","Postal_Code"))
        cp_fmt <- format_postal_code(cp_raw)
        loc_txt <- paste0(loc, if (nzchar(cp_fmt)) paste0(" (", cp_fmt, ")") else "")
        
        ct    <- pick_col(job, c("Contract_Type","Contract"))
        ago   <- if (has_col(job, "Publish_Date")) posted_ago_txt(job$Publish_Date) else ""
        sources <- get_offer_sources(job)
        
        pay <- format_pay(job)
        pay_txt <- if (is_nz1(pay$txt)) paste0(as.character(pay$txt), " € / ", as.character(pay$unit)) else ""
        
        hs_lbl <- head(get_hard_lbl(job, n = 3), 3)
        
        div(class = "offer-card",
            onclick = sprintf("Shiny.setInputValue('open_offer', %d, {priority:'event'})",
                              as.numeric(job$id)
            ),
            div(class = "offer-head",
                div(class = "offer-left",
                    tags$h3(class = "offer-title", title),
                    div(class = "offer-sub",
                        tags$p(class = "offer-company", comp),
                        tags$p(class = "offer-location", loc_txt)
                    ),
                    div(class="pills",
                        if (nzchar(ct)) span(class = pill_cls(FALSE), ct),
                        if (has_col(job,"Is_Remote") && isTRUE(is_remote_true(job$Is_Remote))) {
                          span(class = pill_cls(FALSE), "Télétravail possible")
                        },
                        if (is_nz1(pay_txt)) span(class = pill_cls(FALSE), pay_txt)
                    ),
                    if (length(hs_lbl) > 0) div(class="offer-line",
                                                span(class="offer-label", "Stack :"),
                                                div(class="pills",
                                                    lapply(hs_lbl, function(x) span(class = pill_cls(FALSE), x))
                                                )
                    )
                ),
                div(class="offer-right",
                    tags$button(
                      class = paste("fav-btn", if (is_fav) "is-on" else ""),
                      onclick = sprintf(
                        "event.stopPropagation(); Shiny.setInputValue('toggle_fav', %d, {priority:'event'})",
                        as.numeric(job$id)
                      ),
                      tags$i(class = if (is_fav) "fas fa-heart" else "far fa-heart")
                    ),
                    div(class=paste("match-badge", badge_cls), badge_txt),
                    if (is_nz1(ago)) div(class="offer-time", ago),
                    div(class = "offer-sources-bottom",
                        render_source_logos(sources)
                    )
                )
            )
        )
      })
    )
  })
  
  #############################################################################.
  # ONGLET 4 : FAVORIS & COMPARATEUR ###########################################
  #############################################################################.
  FAV_PER_PAGE <- 5
  
  # petit helper: badge match
  get_match_percent <- function(job_row){
    raw <- pick_col(job_row, c("Match", "Match_Score", "Match_Percent", "Score_Match"))
    if (!nzchar(raw)) return(NA_real_)
    v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", raw)))
    if (!is.finite(v)) return(NA_real_)
    if (v <= 1) v <- v * 100
    v <- max(0, min(100, v))
    v
  }
  
  match_badge_class <- function(p){
    if (!is.finite(p)) return("is-gray")
    if (p >= 70) return("is-green")
    if (p >= 45) return("is-orange")
    "is-red"
  }
  
  # Favoris (dans l’ordre d’ajout)
  fav_df <- reactive({
    if (length(rv$favorites) == 0) return(jobs_df[0])
    ids <- unique(as.numeric(rv$favorites))
    d <- jobs_df[id %in% ids]
    if (nrow(d) == 0) return(d)
    d[, .ord := match(id, ids)]
    d <- d[order(.ord)]
    d[, .ord := NULL]
    d
  })
  
  # tri favoris
  fav_sorted <- reactive({
    d <- fav_df()
    if (nrow(d) == 0) return(d)
    
    sort_mode <- input$fav_sort %||% "date_desc"
    # Compat ancienne UI
    if (sort_mode == "recent") sort_mode <- "date_desc"
    if (sort_mode == "salary") sort_mode <- "salary_desc"
    
    d[, .idx := .I]
    
    # Date : desc / asc
    if (sort_mode == "date_desc" && has_col(d, "Publish_Date")) {
      d <- d[order(is.na(Publish_Date), -as.numeric(Publish_Date), .idx)]
    }
    if (sort_mode == "date_asc" && has_col(d, "Publish_Date")) {
      d <- d[order(is.na(Publish_Date),  as.numeric(Publish_Date), .idx)]
    }
    
    # Salaire : desc / asc
    if (sort_mode %in% c("salary_desc","salary_asc") && salary_cols_ok(d)) {
      
      # salaire : €/h si freelance, sinon €/mois
      d[, .sal := {
        ct <- tolower(trimws(as.character(Contract_Type)))
        is_fr <- !is.na(ct) & ct == "freelance"
        
        mo <- get_monthly_range(.SD)
        hr <- get_hourly_range(.SD)
        
        ifelse(is_fr, hr$max, mo$max)
      }]
      
      if (sort_mode == "salary_desc") d <- d[order(is.na(.sal), -.sal, .idx)]
      if (sort_mode == "salary_asc")  d <- d[order(is.na(.sal),  .sal, .idx)]
      
      d[, .sal := NULL]
    }
    
    if (sort_mode == "match") {
      d[, .m := vapply(seq_len(.N), function(i) get_match_percent(d[i]), numeric(1))]
      d <- d[order(-.m, .idx)]
      d[, .m := NULL]
    }
    
    d[, .idx := NULL]
    d
  })
  
  fav_total_pages <- reactive({
    n <- nrow(fav_sorted())
    if (n == 0) return(1L)
    as.integer(ceiling(n / FAV_PER_PAGE))
  })
  
  # clamp page
  observeEvent(list(fav_total_pages(), rv$favorites, input$fav_sort), {
    tp <- fav_total_pages()
    rv$fav_page <- max(1, min(tp, rv$fav_page))
  }, ignoreInit = TRUE)
  
  # pager actions
  observeEvent(input$fav_page_prev, {
    rv$fav_page <- max(1, rv$fav_page - 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$fav_page_next, {
    rv$fav_page <- min(fav_total_pages(), rv$fav_page + 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$fav_page_goto, {
    p <- as.integer(input$fav_page_goto)
    if (!is.na(p)) rv$fav_page <- max(1, min(fav_total_pages(), p))
  }, ignoreInit = TRUE)
  
  # compteur (0/1 singulier)
  output$fav_count <- renderText({
    n <- length(rv$favorites)
    paste0(n, " offre", ifelse(n > 1, "s", ""), " en favoris")
  })
  
  # UI: pager
  pager_numbers <- function(cur, total){
    if (total <= 7) return(seq_len(total))
    if (cur <= 4) return(c(1,2,3,4,5, NA, total))
    if (cur >= total - 3) return(c(1, NA, total-4, total-3, total-2, total-1, total))
    c(1, NA, cur-1, cur, cur+1, NA, total)
  }
  
  output$fav_pager <- renderUI({
    total <- fav_total_pages()
    cur <- rv$fav_page
    if (nrow(fav_sorted()) == 0) return(NULL)
    
    nums <- pager_numbers(cur, total)
    
    div(class = "exp-pager-wrap",
        div(class = "exp-pager",
            tags$button(
              class = paste("pg-arrow", if (cur == 1) "is-disabled" else ""),
              id = "fav_page_prev",
              onclick = "Shiny.setInputValue('fav_page_prev', Date.now(), {priority:'event'})",
              HTML("&#x2039;")
            ),
            
            lapply(nums, function(x){
              if (is.na(x)) return(div(class = "pg-ellipsis", "..."))
              tags$button(
                class = paste("pg-btn", if (x == cur) "is-active" else ""),
                onclick = sprintf("Shiny.setInputValue('fav_page_goto', %d, {priority:'event'})", x),
                x
              )
            }),
            
            tags$button(
              class = paste("pg-arrow", if (cur == total) "is-disabled" else ""),
              id = "fav_page_next",
              onclick = "Shiny.setInputValue('fav_page_next', Date.now(), {priority:'event'})",
              HTML("&#x203A;")
            )
        )
    )
  })
  
  # UI: liste favoris (5 par page)
  output$fav_list <- renderUI({
    d <- fav_sorted()
    if (nrow(d) == 0) {
      return(div(class="fav-empty", "Aucun favori pour le moment."))
    }
    
    start <- (rv$fav_page - 1) * FAV_PER_PAGE + 1
    end   <- min(start + FAV_PER_PAGE - 1, nrow(d))
    dd <- d[start:end]
    
    tagList(
      lapply(seq_len(nrow(dd)), function(i){
        job <- dd[i]
        
        is_fav <- as.numeric(job$id) %in% rv$favorites
        contract_active <- !is.null(applied$exp_contract) && applied$exp_contract != "Tous"
        remote_active   <- isTRUE(applied$exp_remote)
        salary_active   <- isTRUE(applied$exp_salary_only)
        hard_active     <- length(applied$exp_hard_skills) > 0
        adv_active      <- length(applied$exp_advantages) > 0
        
        title <- pick_col(job, c("Job_Title","Title"))
        comp  <- pick_col(job, c("Company","Company_Name"))
        loc   <- pick_col(job, c("Location","City","Region"))
        cp_raw <- pick_col(job, c("Code_Postal","CP","Postal_Code"))
        cp_fmt <- format_postal_code(cp_raw)
        loc_txt <- paste0(loc, if (nzchar(cp_fmt)) paste0(" (", cp_fmt, ")") else "")
        
        ct    <- pick_col(job, c("Contract_Type","Contract"))
        ago   <- if (has_col(job, "Publish_Date")) posted_ago_txt(job$Publish_Date) else ""
        url   <- pick_col(job, c("Job_URL","URL","Link","Offer_URL"))
        sources <- get_offer_sources(job)
        
        pay <- format_pay(job)
        pay_txt <- if (nzchar(pay$txt)) paste0(pay$txt, " € / ", pay$unit) else ""
        
        hs_can <- head(get_hard_can(job), 3)
        hs_lbl <- labelize_vec(hs_can)
        
        # Avantages (rapide)
        if (has_col(job, "Advantages_Canon") && has_col(job, "Advantages_Label")) {
          adv_keys <- head(split_tokens(job$Advantages_Canon), 3)
          adv_lbl  <- head(split_tokens(job$Advantages_Label), 3)
        } else {
          adv_keys <- head(get_adv_can(job), 3)
          adv_lbl  <- labelize_adv_vec(adv_keys)
        }
        
        
        mp <- get_match_percent(job)
        badge_txt <- if (is.finite(mp)) paste0(round(mp), "% Match") else "Match —"
        badge_cls <- match_badge_class(mp)
        
        div(class = "offer-card",
            onclick = sprintf("Shiny.setInputValue('open_offer', %d, {priority:'event'})",
                              as.numeric(job$id)
            ),
            div(class = "offer-head",
                div(class = "offer-left",
                    tags$h3(class = "offer-title", title),
                    div(class = "offer-sub",
                        tags$p(class = "offer-company", comp),
                        tags$p(class = "offer-location", loc_txt)
                    ),
                    div(class="pills",
                        if (nzchar(ct)) {
                          is_ct_selected <- contract_active &&
                            tolower(trimws(ct)) == tolower(trimws(applied$exp_contract))
                          span(class = pill_cls(is_ct_selected), ct)
                        },
                        if (has_col(job,"Is_Remote") && is_remote_true(job$Is_Remote)) {
                          span(class = pill_cls(remote_active), "Télétravail possible")
                        },
                        if (nzchar(pay_txt)) span(class = pill_cls(salary_active), pay_txt)
                    ),
                    
                    if (length(hs_can) > 0) div(class="offer-line",
                                                span(class="offer-label", "Stack :"),
                                                div(class="pills",
                                                    lapply(seq_along(hs_can), function(j){
                                                      span(
                                                        class = pill_cls(hard_active && (hs_can[j] %in% applied$exp_hard_skills)),
                                                        hs_lbl[j]
                                                      )
                                                    })
                                                )
                    ),
                    
                    if (length(adv_keys) > 0) div(class="offer-line",
                                                  span(class="offer-label", "Le(s) + :"),
                                                  div(class="pills",
                                                      lapply(seq_along(adv_keys), function(j){
                                                        span(
                                                          class = pill_cls(adv_active && (adv_keys[j] %in% applied$exp_advantages)),
                                                          adv_lbl[j]
                                                        )
                                                      })
                                                  )
                    )
                    
                ),
                
                div(class="offer-right",
                    tags$button(
                      class = paste("fav-btn", if (is_fav) "is-on" else ""),
                      onclick = sprintf(
                        "event.stopPropagation(); Shiny.setInputValue('toggle_fav', %d, {priority:'event'})",
                        as.numeric(job$id)
                      ),
                      tags$i(class = if (is_fav) "fas fa-heart" else "far fa-heart")
                    ),
                    
                    div(class=paste("match-badge", badge_cls), badge_txt),
                    if (nzchar(ago)) div(class="offer-time", ago),
                    div(class = "offer-sources-bottom",
                        render_source_logos(sources)
                    )
                )
            )
        )
      })
    )
  })
  
  observeEvent(list(rv$favorites, input$fav_sort), {
    d <- fav_sorted()
    
    if (nrow(d) == 0) {
      updateSelectizeInput(session, "fav_cmp_a", choices = character(0), selected = character(0), server = TRUE)
      updateSelectizeInput(session, "fav_cmp_b", choices = character(0), selected = character(0), server = TRUE)
      rv$cmp_a <- NA_real_
      rv$cmp_b <- NA_real_
      rv$compare_ids <- c()
      return()
    }
    
    labels  <- paste0(d$id, " — ", pick_col(d, "Job_Title"))
    choices <- stats::setNames(as.character(d$id), labels)
    
    sel_a <- if (is.finite(rv$cmp_a)) as.character(rv$cmp_a) else ""
    sel_b <- if (is.finite(rv$cmp_b)) as.character(rv$cmp_b) else ""
    
    updateSelectizeInput(session, "fav_cmp_a", choices = choices, selected = sel_a, server = TRUE)
    updateSelectizeInput(session, "fav_cmp_b", choices = choices, selected = sel_b, server = TRUE)
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$fav_cmp_a, input$fav_cmp_b), {
    a <- suppressWarnings(as.numeric(input$fav_cmp_a))
    b <- suppressWarnings(as.numeric(input$fav_cmp_b))
    
    a_ok <- is.finite(a) && a %in% jobs_df$id
    b_ok <- is.finite(b) && b %in% jobs_df$id
    
    rv$cmp_a <- if (a_ok) a else NA_real_
    rv$cmp_b <- if (b_ok && (!a_ok || b != a)) b else NA_real_
    
    rv$compare_ids <- c(rv$cmp_a, rv$cmp_b)
    rv$compare_ids <- rv$compare_ids[is.finite(rv$compare_ids)]
  }, ignoreInit = TRUE)
  
  # Comparateur 
  safe_txt <- function(x, placeholder = "—"){
    x <- as.character(x %||% "")
    x <- trimws(x)
    if (!nzchar(x)) return(placeholder)
    x
  }
  
  get_job_by_id <- function(id_val){
    if (!is.finite(id_val)) return(NULL)
    rr <- jobs_df[id == id_val]
    if (nrow(rr) == 0) return(NULL)
    rr[1]
  }
  
  format_stack_key <- function(job_row){
    hs_lbl <- get_hard_lbl(job_row, n = 3)
    if (length(hs_lbl) == 0) return("—")
    paste(hs_lbl, collapse = ", ")
  }
  
  format_adv <- function(job_row){
    adv_col <- c("Benefits","Advantages","Perks")[c("Benefits","Advantages","Perks") %in% names(jobs_df)][1]
    if (is.na(adv_col)) return("—")
    aa <- unique(split_tokens(pick_col(job_row, adv_col)))
    if (length(aa) == 0) return("—")
    paste(head(aa, 3), collapse = ", ")
  }
  
  format_remote_txt <- function(job_row){
    if (!has_col(job_row, "Is_Remote")) return("—")
    if (is_remote_true(job_row$Is_Remote)) return("Oui")
    "—"
  }
  
  output$compare_table <- renderUI({
    
    # On lit les 2 selectize (pour respecter colonne A / colonne B)
    ida <- suppressWarnings(as.numeric(input$fav_cmp_a))
    idb <- suppressWarnings(as.numeric(input$fav_cmp_b))
    
    a_ok <- is.finite(ida) && ida %in% jobs_df$id
    b_ok <- is.finite(idb) && idb %in% jobs_df$id
    
    j1 <- if (a_ok) jobs_df[id == ida][1] else NULL
    j2 <- if (b_ok) jobs_df[id == idb][1] else NULL
    
    dash <- function(x){
      x <- as.character(x %||% "")
      if (!nzchar(trimws(x))) "-" else x
    }
    
    # Titres colonnes
    t1 <- if (!is.null(j1)) dash(pick_col(j1, c("Job_Title","Title"))) else "-"
    t2 <- if (!is.null(j2)) dash(pick_col(j2, c("Job_Title","Title"))) else "-"
    
    # Valeurs A
    p1 <- if (!is.null(j1)) format_pay(j1) else list(txt = "", unit = "")
    m1 <- if (!is.null(j1)) get_match_percent(j1) else NA_real_
    
    v_match_1 <- if (is.finite(m1)) span(class=paste("match-badge", match_badge_class(m1)),
                                         paste0(round(m1), "% Match")) else "-"
    v_ct_1    <- if (!is.null(j1)) dash(pick_col(j1, "Contract_Type")) else "-"
    v_sal_1   <- if (!is.null(j1) && nzchar(p1$txt)) paste0(p1$txt, " € / ", p1$unit) else "-"
    v_rem_1   <- if (!is.null(j1)) dash(format_remote_txt(j1)) else "-"
    v_stk_1   <- if (!is.null(j1)) dash(format_stack_key(j1)) else "-"
    v_adv_1   <- if (!is.null(j1)) dash(format_adv(j1)) else "-"
    
    # Valeurs B
    p2 <- if (!is.null(j2)) format_pay(j2) else list(txt = "", unit = "")
    m2 <- if (!is.null(j2)) get_match_percent(j2) else NA_real_
    
    v_match_2 <- if (is.finite(m2)) span(class=paste("match-badge", match_badge_class(m2)),
                                         paste0(round(m2), "% Match")) else "-"
    v_ct_2    <- if (!is.null(j2)) dash(pick_col(j2, "Contract_Type")) else "-"
    v_sal_2   <- if (!is.null(j2) && nzchar(p2$txt)) paste0(p2$txt, " € / ", p2$unit) else "-"
    v_rem_2   <- if (!is.null(j2)) dash(format_remote_txt(j2)) else "-"
    v_stk_2   <- if (!is.null(j2)) dash(format_stack_key(j2)) else "-"
    v_adv_2   <- if (!is.null(j2)) dash(format_adv(j2)) else "-"
    
    div(class="compare-card",
        tags$table(class="compare-table",
                   tags$thead(
                     tags$tr(
                       tags$th(""),
                       tags$th(t1),
                       tags$th(t2)
                     )
                   ),
                   tags$tbody(
                     tags$tr(tags$td("Score de Match"), tags$td(v_match_1), tags$td(v_match_2)),
                     tags$tr(tags$td("Type de contrat"), tags$td(v_ct_1),    tags$td(v_ct_2)),
                     tags$tr(tags$td("Salaire"),        tags$td(v_sal_1),   tags$td(v_sal_2)),
                     tags$tr(tags$td("Télétravail"),    tags$td(v_rem_1),   tags$td(v_rem_2)),
                     tags$tr(tags$td("Stack Clé"),      tags$td(v_stk_1),   tags$td(v_stk_2)),
                     tags$tr(tags$td("Avantages"),      tags$td(v_adv_1),   tags$td(v_adv_2))
                   )
        )
    )
  })
  
}

