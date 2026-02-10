# Projet: Webscraping
# Objet: _BUT DE CE DOCUMENT_
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
library(shiny)
library(plotly)
library(shinyjs)
library(readxl)
library(leaflet) 

# Chargement des données
# (robuste) : certains environnements n'ont pas de dossier `www/`
jobs_path <- if (file.exists("www/data_jobs.csv")) "www/data_jobs.csv" else "data_jobs.csv"
jobs_df <- data.table::fread(jobs_path)

col_hard <- "Hard_Skills"
col_soft <- "Soft_Skills"
col_sector <- "Sector"
col_source <- "Source_Site"

# Split 
split_unique <- function(x, sep = ",") {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(character(0))
  tokens <- unlist(strsplit(x, sep, fixed = TRUE))
  tokens <- trimws(tokens)
  tokens <- tokens[tokens != ""]
  sort(unique(tokens))
}

# Lists dynamiques
unique_sources <- split_unique(jobs_df[[col_source]], sep = ",")
unique_hard_skills <- split_unique(jobs_df[[col_hard]], sep = ",")
unique_soft_skills <- split_unique(jobs_df[[col_soft]], sep = ",")
unique_sectors <- sort(unique(na.omit(jobs_df[[col_sector]])))


###############################################################################.
# UI ---------------------------------------------------------------------------
###############################################################################.
library(shiny)

app_footer <- function() {
  tags$footer(class = "app-footer",
              div(class = "footer-top",
                  div(class = "footer-brand",
                      div(class = "footer-brand-row",
                          img(src = "icons/logo-brand.webp",
                              height = "60px",
                              class = "footer-logo-white"
                          ),
                          div(class = "footer-brand-text",
                              div("Data Career", class = "footer-brand-line1"),
                              div("Navigator", class = "footer-brand-line2")
                          )
                      ),
                      p("La plateforme de référence pour propulser votre carrière dans la donnée", 
                        class = "footer-tagline")
                  ),
                  div(class = "footer-links",
                      div(class = "footer-col",
                          h4("Navigation"),
                          tags$ul(tags$li(actionLink("go_explorateur", "Explorateur", class = "footer-link")),
                                  tags$li(actionLink("go_match", "Match Parfait", class = "footer-link")),
                                  tags$li(actionLink("go_favoris", "Favoris et Comparateur", class = "footer-link")))
                      ),
                      div(class = "footer-col",
                          h4("Légal"),
                          tags$ul(tags$li(tags$a("Mentions légales", href = "#", class = "footer-link")),
                                  tags$li(tags$a("Politique de confidentialité", href = "#", class = "footer-link")),
                                  tags$li(tags$a("CGU", href = "#", class = "footer-link")))
                      )
                  )
              ),
              div(class = "footer-sep"),
              div(class = "footer-bottom",
                  div("© 2025 DATA CAREER NAVIGATOR. TOUS DROITS RÉSERVÉS.", 
                      class = "footer-copy"
                  ),
                  div(HTML("FAIT AVEC &hearts;"), 
                      class = "footer-made"
                  )
              )
  )
}

navbarPage(
  shinyjs::useShinyjs(),
  title = div(class = "logo-container",
              img(src = "icons/logo-brand.webp",
                  height = "55px", style = "margin-right:15px;"),
              # Bloc texte empilé
              div(class = "title-stack",
                  div("Data Career", class = "title-line-1"),
                  div("Navigator", class = "title-line-2"))),
  id = "nav",
  position = "fixed-top",
  windowTitle = "Data Career Navigator",
  header = tags$head(
    # Polices : Roboto + Poppins + Fira Code + Source Code Pro + Inter + Madani Arabic (Google Fonts)
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700;900&family=Poppins:wght@700;800;900&family=Fira+Code:wght@400;600&family=Source+Code+Pro:wght@400;600;700;800;900&family=Inter:wght@400;600;700;800&family=Madani+Arabic:wght@400;600;700&display=swap",
              rel  = "stylesheet"
    ),
    # Police : Open Sauce One (Fontsource via unpkg)
    tags$link(href = "https://unpkg.com/@fontsource/open-sauce-one@latest/index.css",
              rel  = "stylesheet"),
    # CSS et JS du projet
    # (robuste) : si pas de dossier `www/`, on injecte les fichiers locaux en inline
    if (file.exists("style.css")) {
      tags$style(HTML(paste(readLines("style.css", warn = FALSE), collapse = "\n")))
    } else {
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    },
    if (file.exists("script.js")) {
      tags$script(HTML(paste(readLines("script.js", warn = FALSE), collapse = "\n")))
    } else {
      tags$script(src = "script.js")
    }
  ),
  footer = app_footer(),
  
  # Page Acceuil ###############################################################
  tabPanel("Accueil",
           ## Partie HERO ------------------------------------------------------
           div(class = "hero-section",
               div(class = "hero-content",
                   h1("Visez juste. Postulez mieux."),
                   h2("Comparez les meilleures offres et trouvez la vôtre."),
                   p("Explorez plus de 1000 opportunités agrégées, analysez la compatibilité avec votre profil et générez vos candidatures en un clic."))
           ),
           ## Partie route -----------------------------------------------------
           div(class = "road-main-container",
               div(class = "road-header-black",
                   h2(shiny::HTML("Comment ça <strong>marche</strong> ?")),
                   shiny::icon("chevron-down", class = "bounce-arrow")
               ),
               div(class = "road-visual-area",
                   # voiture
                   img(id = "car-icon",
                       src = "icons/car-icon.webp",
                       class = "car-sprite"
                   ),
                   # Route
                   shiny::HTML('<svg id="road-svg" viewBox="0 0 10 330" class="road-svg-element" preserveAspectRatio="none">
            <path id="motion-path"
            d="M 3 0
               L 3 44
               C 3 80, 6.6 80, 6.6 110
               L 6.6 136
               C 6.6 168, 3.6 168, 3.6 198
               L 3.6 216
               C 3.6 246, 6.2 246, 6.2 272
               C 6.2 296, 5 296, 5 314
               L 5 340"
            fill="none" stroke="none" />
            <path class="road-base"
            d="M 3 0
               L 3 44
               C 3 80, 6.6 80, 6.6 110
               L 6.6 136
               C 6.6 168, 3.6 168, 3.6 198
               L 3.6 216
               C 3.6 246, 6.2 246, 6.2 272
               C 6.2 296, 5 296, 5 314
               L 5 340"
            fill="none" vector-effect="non-scaling-stroke" />
            <path class="road-dash"
            d="M 3 0
               L 3 44
               C 3 80, 6.6 80, 6.6 110
               L 6.6 136
               C 6.6 168, 3.6 168, 3.6 198
               L 3.6 216
               C 3.6 246, 6.2 246, 6.2 272
               C 6.2 296, 5 296, 5 314
               L 5 340"
            fill="none" vector-effect="non-scaling-stroke" />
            </svg>'),
                   div(class = "step-block side-right",
                       style="top: 6%; left: 40%;",
                       h3("Explorez"),
                       p(shiny::HTML("Rendez-vous sur l'onglet <strong>Explorateur</strong> pour filtrer les 1000+ offres selon vos critères (Lieu, Salaire, Contrat...)."))
                   ),
                   div(class = "step-block side-left",
                       style="top: 34%; right: 45%;",
                       h3("Sélectionnez"),
                       p(shiny::HTML("Cliquez sur une offre pour voir les détails et ajoutez-la à vos <strong>Favoris</strong> ❤️ si elle vous intéresse."))
                   ),
                   div(class = "step-block side-right",
                       style="top: 60%; left: 45%;",
                       h3("Matchez"),
                       p(shiny::HTML("Déposez votre CV dans l'onglet <strong>Match Parfait</strong> pour découvrir votre score de compatibilité et vos points à réviser."))
                   ),
                   div(class = "step-block side-left",
                       style="top: 80%; right: 50%;",
                       h3("Postulez"),
                       p(shiny::HTML("Comparez vos favoris, générez votre lettre avec <strong>Auto-Plume</strong> et cliquez sur \"Postuler\" pour finaliser."))
                   )
               ),
               div(class = "cta-final",
                   h3(shiny::HTML('Votre futur job en <span class="cta-hl">Data Science</span> est à portée de clic.')),
                   p("Découvrez tout le potentiel de l'outil dès maintenant."),
                   actionButton("go_tuto", "LANCER LE TUTORIEL", class = "btn-gradient")
               )
           )
  ),
  
  # Page Explorateur ###########################################################
  tabPanel("Explorateur",
           div(class = "explorer-page",
               
               tags$section(class = "explorer-hero",
                            ## Partie HERO -------------------------------------
                            div(class = "hero-inner",
                                tags$h1("Fouillez, Filtrez, Trouvez"),
                                tags$h2("Ciblez votre futur parmi +1000 opportunités."),
                                
                                ## Partie recherche ----------------------------
                                div(class = "search-pill",
                                    div(class = "search-seg",
                                        icon("search"),
                                        selectizeInput(
                                          "exp_q",
                                          label = NULL,
                                          choices = NULL,
                                          multiple = TRUE,
                                          options = list(
                                            placeholder = "Intitulé du poste, mots clefs, entreprise...",
                                            plugins = list("remove_button"),
                                            create = TRUE,
                                            persist = FALSE,
                                            openOnFocus = TRUE
                                          )
                                        )
                                    ),
                                    div(class = "search-divider"),
                                    div(class = "search-seg",
                                        icon("map-marker-alt"),
                                        selectizeInput(
                                          "exp_loc",
                                          label = NULL,
                                          choices = NULL,
                                          multiple = TRUE,
                                          options = list(
                                            placeholder = "Ville, département, code postal...",
                                            plugins = list("remove_button"),
                                            create = TRUE,
                                            persist = FALSE,
                                            openOnFocus = TRUE
                                          )
                                        )
                                    ),
                                    actionButton("exp_search_btn", "Rechercher", class = "search-btn")
                                )
                            )
               ),
               
               # Contenu : filtres à gauche et résultats à droite
               div(class = "explorer-wrap",
                   fluidRow(
                     column(width = 3,
                            ## Partie filtres ---------------------------
                            div(class = "filters-card",
                                div(class = "filters-title", "Filtres avancés"),
                                
                                ### Type de contrat ----------------------------
                                selectInput("exp_contract", 
                                            "Type de contrat",
                                            choices  = c("Tous", "CDI", "CDD", "Stage", "Alternance", "Freelance"),
                                            selected = "Tous"
                                ),
                                
                                ### Durée (si stage ou CDD) --------------------
                                conditionalPanel(condition = "input.exp_contract === 'Stage' || input.exp_contract === 'CDD'",
                                                 div(class = "duration-block",
                                                     div(class = "duration-title", 
                                                         "Durée (mois)"
                                                     ),
                                                     div(class = "duration-toggle-row",
                                                         span(class = "duration-toggle-text", 
                                                              "Afficher uniquement les offres avec une durée"),
                                                         div(class = "switch",
                                                             tags$input(id = "exp_duration_only",
                                                                        type = "checkbox",
                                                                        class = "shiny-input-checkbox"),
                                                             tags$label(`for` = "exp_duration_only", 
                                                                        class = "switch-label")
                                                         )
                                                     ),
                                                     div(id = "exp_duration_wrap", 
                                                         class = "salary-slider-wrap",
                                                         sliderInput("exp_duration_range",
                                                                     label = NULL,
                                                                     min = 0, max = 24,
                                                                     value = c(0, 24),
                                                                     step = 1)
                                                     )
                                                 )
                                ),
                                
                                ### Niveau d'expérience ------------------------
                                selectInput("exp_experience_level", 
                                            "Niveau d’expérience",
                                            choices = c(
                                              "Tous" = "Tous",
                                              "Junior (≤ 2 ans)" = "junior",
                                              "Mid (3 à 6 ans)" = "mid",
                                              "Senior (≥ 7 an)" = "senior"
                                            ),
                                            selected = "Tous"
                                ),
                                
                                ### Salaire ------------------------------------
                                tags$div(
                                  style = "margin-top:10px;",
                                  div(class = "salary-block",
                                      div(id = "exp_salary_title", 
                                          class = "salary-title", 
                                          "Fourchette de salaire (€ /mois)"
                                      ),
                                      div(class = "salary-toggle-row",
                                          span(class = "salary-toggle-text", 
                                               "Afficher uniquement les offres avec un salaire"),
                                          div(class = "switch",
                                              tags$input(id = "exp_salary_only",
                                                         type = "checkbox",
                                                         class = "shiny-input-checkbox"
                                              ),
                                              tags$label(`for` = "exp_salary_only", 
                                                         class = "switch-label")
                                          )
                                      ),
                                      div(id = "exp_salary_wrap",
                                          class = "salary-slider-wrap",
                                          sliderInput("exp_salary_range",
                                                      label = NULL,
                                                      min = 0, max = 10000,
                                                      value = c(0, 10000),
                                                      step = 500)
                                      )
                                  )
                                ),
                                
                                
                                ### Télétravail --------------------------------
                                div(class = "toggle-row",
                                    span(class = "salary-title", "Télétravail possible"),
                                    div(class = "switch",
                                        tags$input(id = "exp_remote",
                                                   type = "checkbox",
                                                   class = "shiny-input-checkbox"),
                                        tags$label(`for` = "exp_remote", 
                                                   class = "switch-label")
                                    )
                                ),
                                
                                ### Secteur d'activité -------------------------
                                selectizeInput("exp_sector",
                                               label = "Secteur d’activité",
                                               choices = NULL,
                                               multiple = TRUE,
                                               options = list(placeholder = "Tapez un secteur...",
                                                              plugins = list("remove_button"),
                                                              create = FALSE, # uniquement secteurs existants 
                                                              persist = FALSE,
                                                              openOnFocus = TRUE)
                                ),
                                
                                ### Taille entreprise --------------------------
                                selectInput("exp_company_category",
                                            "Taille de l’entreprise",
                                            choices = c("Tous" = "Tous",
                                                        "Très petite entreprise (TPE) < 10" = "TPE",
                                                        "Petite entreprise (PE) 10 - 49" = "PE",
                                                        "Moyenne entreprise (ME) 50 - 249" = "ME",
                                                        "Grande entreprise (GE) ≥ 250" = "GE"
                                            ),
                                            selected = "Tous"
                                ),
                                
                                ### Hard skills --------------------------------
                                selectizeInput("exp_hard_skills", 
                                               "Hard skills",
                                               choices = NULL, 
                                               multiple = TRUE,
                                               options = list(placeholder = "Écrire une compétence...",
                                                              plugins = list("remove_button"),
                                                              create = TRUE, 
                                                              persist = FALSE, 
                                                              openOnFocus = TRUE)
                                ),
                                ### Soft skills --------------------------------
                                selectizeInput("exp_soft_skills", 
                                               "Soft skills",
                                               choices = NULL, 
                                               multiple = TRUE,
                                               options = list(placeholder = "Écrire une compétence...",
                                                              plugins = list("remove_button"),
                                                              create = TRUE,
                                                              persist = FALSE,
                                                              openOnFocus = TRUE)
                                ),
                                ### Avantages ----------------------------------
                                selectizeInput("exp_advantages", 
                                               "Avantages",
                                               choices = NULL, 
                                               multiple = TRUE,
                                               options = list(placeholder = "Écrire un avantage...",
                                                              plugins = list("remove_button"),
                                                              create = TRUE, 
                                                              persist = FALSE, 
                                                              openOnFocus = TRUE)
                                ),
                                
                                ### Date ---------------------------------------
                                selectInput("exp_date", 
                                            "Date de publication",
                                            choices  = c("Toutes", "Depuis 24h", "Depuis 3 jours", "Depuis 1 semaine", "Depuis 1 mois"),
                                            selected = "Toutes"
                                ),
                                
                                ### Source de l'offre --------------------------
                                div(class = "filter-block",
                                    div(class = "salary-title", "Source de l’offre"),
                                    div(id = "exp_source_grid", class = "source-grid",
                                        div(class = "sg-item",
                                            checkboxInput("exp_source_all", "Tous", value = TRUE)
                                        ),
                                        div(class = "sg-item",
                                            checkboxInput("exp_source_li",  "LinkedIn", value = TRUE)
                                        ),
                                        div(class = "sg-item",
                                            checkboxInput("exp_source_in",  "Indeed",   value = TRUE)
                                        ),
                                        div(class = "sg-item",
                                            checkboxInput("exp_source_wttj",  "Welcome to the Jungle",value = TRUE)
                                        )
                                    )
                                ),
                                
                                ### Bouton Actualiser --------------------------
                                actionButton("exp_apply_filters", 
                                             "Actualiser la recherche", 
                                             class = "apply-btn w-100")
                            )
                     ),
                     column(width = 9,
                            ## Partie offres -----------------------------------
                            div(class = "results-top",
                                div(class = "view-toggle",
                                    radioButtons("exp_view",
                                                 label = NULL,
                                                 choices = c("Liste" = "list", "Carte" = "map"),
                                                 selected = "list",
                                                 inline = TRUE
                                    )
                                ),
                                div(class = "meta-right",
                                    textOutput("exp_count", inline = TRUE),
                                    div(style = "width: 250px;", 
                                        selectInput("exp_sort",
                                                    label = NULL,
                                                    choices = c("Trier" = "relevance",
                                                                "Date : ordre décroissant" = "date_desc",
                                                                "Date : ordre croissant" = "date_asc",
                                                                "Salaire : ordre décroissant" = "salary_desc",
                                                                "Salaire : ordre croissant"  = "salary_asc"),
                                                    selected = "relevance"
                                        )
                                    )
                                )
                            ),
                            conditionalPanel(
                              condition = "input.exp_view === 'list'",
                              uiOutput("exp_results_list"),
                              uiOutput("exp_pager")
                            ),
                            conditionalPanel(
                              condition = "input.exp_view === 'map'",
                              leafletOutput("exp_map", height = "520px"),
                              tags$p(class = "map-note",
                                     "Note : les offres en 100% télétravail ne sont pas affichées sur la carte car elles n’ont pas de localisation."
                              ),
                              div(style = "margin-top:14px;",
                                  tags$h3(class="offers-title","Offres trouvées"),
                                  div(style="padding-right:6px;",
                                      uiOutput("exp_results_list_map"),
                                      uiOutput("exp_map_pager")
                                  )
                              )
                            )
                            
                            
                     )
                   )
               )
           )
  ),
  # Page Match Parfait #########################################################
  tabPanel("Match Parfait",
           div(class = "match-page",
               ## Partie HERO ---------------------------------------------------------
               tags$section(class = "match-hero",
                            div(class = "hero-inner",
                                tags$h1("Diagnostic de Carrière"),
                                tags$h2("Révélez votre potentiel : Analysez la compatibilité de votre profil avec vos objectifs métiers"),
                                ## Partie recherche -------------------------------------
                                div(class = "diagnostic-pill",
                                    ### Profil ------------------------------------------
                                    div(class = "profile-uploader",
                                        div(class = "uploader-text", 
                                            "Glissez votre CV (pdf)"
                                        ),
                                        div(class = "uploader-sub", 
                                            "ou"
                                        ),
                                        div(class = "mp-browse-btn", 
                                            "Parcourir"
                                        ),
                                        div(class = "mp-cv-input",
                                            fileInput("mp_cv", 
                                                      label = NULL, 
                                                      accept = c(".pdf"), 
                                                      buttonLabel = "Parcourir")
                                        )
                                    ),
                                    ### Objectif ----------------------------------------
                                    div(class = "diag-seg diag-objective",
                                        div(class = "diag-label", 
                                            "Votre objectif"
                                        ),
                                        div(class = "diag-fields",
                                            div(class = "diag-field",
                                                icon("search"),
                                                textInput("mp_title", 
                                                          label = NULL,
                                                          placeholder = "Intitulé du poste, mots clefs, entreprise...")
                                            ),
                                            div(class = "diag-divider"),
                                            div(class = "diag-field",
                                                icon("map-marker-alt"),
                                                textInput("mp_loc", 
                                                          label = NULL,
                                                          placeholder = "Ville, département, code postal...")
                                            )
                                        )
                                    ),
                                    div(class = "diag-actions",
                                        div(class = "mp-run-row",
                                            actionButton("mp_run",
                                                         "LANCER L'ANALYSE",
                                                         class = "search-btn"
                                            ),
                                            div(id = "mp_run_spinner",
                                                class = "mp-spinner",
                                                `aria-hidden` = "true"
                                            )
                                        )
                                    )
                                )
                            )
               ),
               div(class = "explorer-wrap match-wrap",
                   fluidRow(
                     ## Partie filtres ----------------------------------------------------
                     column(width = 3,
                            div(class = "filters-card",
                                div(class = "filters-title", 
                                    "Filtres avancés"),
                                selectInput("mp_contract",
                                            "Type de contrat",
                                            choices = c("Tous", "CDI", "CDD", "Stage", "Alternance", "Freelance"),
                                            selected = "Tous"),
                                conditionalPanel(condition = "input.mp_contract === 'Stage' || input.mp_contract === 'CDD'",
                                                 div(class = "duration-block",
                                                     div(class = "duration-title", 
                                                         "Durée (mois)"
                                                     ),
                                                     div(class = "duration-toggle-row",
                                                         span(class = "duration-toggle-text", 
                                                              "Afficher uniquement les offres avec une durée"),
                                                         div(class = "switch",
                                                             tags$input(id = "mp_duration_only", 
                                                                        type = "checkbox", 
                                                                        class = "shiny-input-checkbox"),
                                                             tags$label(`for` = "mp_duration_only", 
                                                                        class = "switch-label")
                                                         )
                                                     ),
                                                     div(id = "mp_duration_wrap",
                                                         class = "salary-slider-wrap",
                                                         sliderInput("mp_duration_range",
                                                                     label = NULL,
                                                                     min = 0, max = 24,
                                                                     value = c(0, 24),
                                                                     step = 1)
                                                     )
                                                 )
                                ),
                                selectInput("mp_experience_level",
                                            "Niveau d’expérience",
                                            choices = c("Tous" = "Tous",
                                                        "Junior (≤ 2 ans)" = "junior",
                                                        "Mid (3 à 6 ans)" = "mid",
                                                        "Senior (≥ 7 an)" = "senior"),
                                            selected = "Tous"),
                                tags$div(style = "margin-top:10px;",
                                         div(class = "salary-block",
                                             div(id = "mp_salary_title", 
                                                 class = "salary-title", 
                                                 "Fourchette de salaire (€ /mois)"
                                             ),
                                             div(class = "salary-toggle-row",
                                                 span(class = "salary-toggle-text", 
                                                      "Afficher uniquement les offres avec un salaire"),
                                                 div(class = "switch",
                                                     tags$input(id = "mp_salary_only", 
                                                                type = "checkbox", 
                                                                class = "shiny-input-checkbox"),
                                                     tags$label(`for` = "mp_salary_only", 
                                                                class = "switch-label")
                                                 )
                                             ),
                                             div(id = "mp_salary_wrap",
                                                 class = "salary-slider-wrap",
                                                 sliderInput("mp_salary_range",
                                                             label = NULL,
                                                             min = 0, max = 10000,
                                                             value = c(0, 10000),
                                                             step = 500)
                                             )
                                         )
                                ),
                                div(class = "toggle-row",
                                    span(class = "salary-title", 
                                         "Télétravail"),
                                    div(class = "switch",
                                        tags$input(id = "mp_remote", 
                                                   type = "checkbox", 
                                                   class = "shiny-input-checkbox"),
                                        tags$label(`for` = "mp_remote", 
                                                   class = "switch-label")
                                    )
                                ),
                                selectizeInput("mp_sector",
                                               label = "Secteur d’activité",
                                               choices = NULL,
                                               multiple = TRUE,
                                               options = list(placeholder = "Tapez un secteur...",
                                                              plugins = list("remove_button"),
                                                              create = FALSE,
                                                              persist = FALSE,
                                                              openOnFocus = TRUE)
                                ),
                                selectInput("mp_company_category",
                                            "Taille de l’entreprise",
                                            choices = c("Tous" = "Tous",
                                                        "Très petite entreprise (TPE) < 10" = "TPE",
                                                        "Petite entreprise (PE) 10 - 49" = "PE",
                                                        "Moyenne entreprise (ME) 50 - 249" = "ME",
                                                        "Grande entreprise (GE) ≥ 250" = "GE"),
                                            selected = "Tous"
                                ),
                                selectizeInput("mp_hard_skills", 
                                               "Hard skills",
                                               choices = NULL, 
                                               multiple = TRUE,
                                               options = list(placeholder = "Écrire une compétence...",
                                                              plugins = list("remove_button"),
                                                              create = TRUE,
                                                              persist = FALSE,
                                                              openOnFocus = TRUE)
                                ),
                                selectizeInput("mp_soft_skills", 
                                               "Soft skills",
                                               choices = NULL,
                                               multiple = TRUE,
                                               options = list(placeholder = "Écrire une compétence...",
                                                              plugins = list("remove_button"),
                                                              create = TRUE,
                                                              persist = FALSE,
                                                              openOnFocus = TRUE)
                                ),
                                selectizeInput("mp_advantages",
                                               "Avantages",
                                               choices = NULL,
                                               multiple = TRUE,
                                               options = list(placeholder = "Écrire un avantage...",
                                                              plugins = list("remove_button"),
                                                              create = TRUE,
                                                              persist = FALSE,
                                                              openOnFocus = TRUE)
                                ),
                                selectInput("mp_date",
                                            "Date de publication",
                                            choices = c("Toutes", "Depuis 24h", "Depuis 3 jours", "Depuis 1 semaine", "Depuis 1 mois"),
                                            selected = "Toutes"
                                ),
                                div(class = "filter-block",
                                    div(class = "salary-title", 
                                        "Source de l’offre"),
                                    div(id = "mp_source_grid", 
                                        class = "source-grid",
                                        div(class = "sg-item", 
                                            checkboxInput("mp_source_all", "Tous",
                                                          value = TRUE)
                                        ),
                                        div(class = "sg-item", 
                                            checkboxInput("mp_source_li",  "LinkedIn",
                                                          value = TRUE)
                                        ),
                                        div(class = "sg-item", 
                                            checkboxInput("mp_source_in",  "Indeed",
                                                          value = TRUE)
                                        ),
                                        div(class = "sg-item", 
                                            checkboxInput("mp_source_wttj",  "Welcome to the Jungle",
                                                          value = TRUE)
                                        )
                                    )
                                ),
                                actionButton("mp_apply_filters", 
                                             "Actualiser la recherche", 
                                             class = "apply-btn w-100")
                            )
                     ),
                     column(width = 9,
                            ## Partie Dashboard ------------------------------------------------
                            div(class = "mp-dashboard",
                                div(class = "mp-dashboard-title", 
                                    "DASHBOARD DIAGNOSTIC"),
                                div(class = "mp-dash-grid",
                                    div(class = "mp-dash-left",
                                        plotly::plotlyOutput("mp_radar", height = "260px")
                                    ),
                                    div(
                                      class = "mp-dash-right",
                                      uiOutput("mp_advice")
                                    )
                                )
                            ),
                            
                            ## Partie recommandations ------------------------------------------
                            div(class = "mp-reco-wrap",
                                div(class = "mp-reco-head",
                                    tags$h2("RECOMMANDATIONS  POUR VOUS"),
                                    tags$p("Voici les offres du marché qui correspondent le mieux à votre profil et à votre objectif de carrière actuel.")
                                ),
                                div(class = "results-top",
                                    div(id = "mp_view_toggle",
                                        class = "view-toggle is-list",
                                        radioButtons("mp_view",
                                                     label = NULL,
                                                     choices = c("Liste" = "list", "Carte" = "map"),
                                                     selected = "list",
                                                     inline = TRUE)
                                    ),
                                    div(class = "meta-right",
                                        textOutput("mp_count", 
                                                   inline = TRUE),
                                        div(style = "width: 250px;",
                                            selectInput("mp_sort",
                                                        label = NULL,
                                                        choices = c("Trier" = "relevance",
                                                                    "Date : ordre décroissant" = "date_desc",
                                                                    "Date : ordre croissant" = "date_asc",
                                                                    "Salaire : ordre décroissant" = "salary_desc",
                                                                    "Salaire : ordre croissant"  = "salary_asc"),
                                                        selected = "match")
                                        )
                                    )
                                ),
                                conditionalPanel(condition = "input.mp_view === 'list'",
                                                 uiOutput("mp_results_list")
                                ),
                                conditionalPanel(
                                  condition = "input.mp_view === 'map'",
                                  div(class = "map-wrapper",
                                      leafletOutput("mp_map", height = "650px")
                                  ),
                                  
                                  tags$p(
                                    class = "map-note",
                                    "Note : les offres en 100% télétravail ne sont pas affichées sur la carte car elles n’ont pas de localisation."
                                  ),
                                  
                                  div(style = "margin-top: 16px;",
                                      uiOutput("mp_results_list"),
                                      uiOutput("exp_pager") 
                                  )
                                )
                                
                            )
                     )
                   )
               )
           )
  ),
  # Page Favoris et comparateur ################################################
  tabPanel("Favoris et comparateur",
           div(class = "fav-page",
               ## Partie HERO --------------------------------------------------
               div(class = "fav-hero",
                   div(class = "fav-hero-inner",
                       div(class = "fav-hero-left",
                           tags$h1("Votre Sélection", tags$br(), "Stratégique"),
                           tags$p("Gérez vos coups de cœur et comparez pour mieux choisir.")
                       ),
                       div(class = "fav-hero-right",
                           div(class = "fav-hero-visual",
                               tags$img(src = "icons/fav-hero-img.webp",
                                        class = "fav-hero-img",
                                        alt   = "Illustration favoris")
                           )
                       )
                   )
               ),
               ## Partie Transition --------------------------------------------
               div(class = "fav-transition",
                   div(class = "fav-transition-inner",
                       p("Affinez votre choix. Comparez les détails techniques et préparez votre candidature personnalisée.",
                         class = "fav-transition-text"),
                       icon("chevron-down", 
                            class = "bounce-arrow fav-bounce")
                   )
               ),
               ## Partie Favoris -----------------------------------------------
               div(class = "fav-fullbleed",
                   div(class = "fav-favs-section",
                       div(class = "fav-wrap",
                           div(class = "fav-topbar",
                               tags$h3("MES FAVORIS"),
                               div(class = "fav-topbar-right",
                                   textOutput("fav_count", inline = TRUE),
                                   selectInput("fav_sort", NULL,
                                               choices = c("Trier" = "relevance",
                                                           "Date : ordre décroissant" = "date_desc",
                                                           "Date : ordre croissant" = "date_asc",
                                                           "Salaire : ordre décroissant" = "salary_desc",
                                                           "Salaire : ordre croissant"  = "salary_asc"),
                                               selected = "recent")
                               )
                           ),
                           uiOutput("fav_list"),
                           uiOutput("fav_pager")
                       )
                   )
               ),
               ## Partie Comparateur -------------------------------------------
               div(class = "fav-compare-section",
                   div(class = "fav-compare-wrap",
                       tags$h2("Comparez vos offres favorites"),
                       div(class = "fav-compare-pickers",
                           selectizeInput("fav_cmp_a", label = NULL, choices = NULL,
                                          options = list(placeholder = "Choisir une offre...", create = FALSE)),
                           selectizeInput("fav_cmp_b", label = NULL, choices = NULL,
                                          options = list(placeholder = "Choisir une offre...", create = FALSE))
                       ),
                       div(class = "compare-card",
                           uiOutput("compare_table")
                       )
                   )
               )
           )
  )
)



