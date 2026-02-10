# Server "CE" : réutilise la logique du serveur principal
# (évite divergences/interférences entre `ui_ce.R` et un serveur de démo)
source("server.R", local = TRUE)

# `server.R` définit `server <- function(input, output, session) { ... }`
server