# server_ce.R
# Cette version réutilise la logique de l'app principale (server.R),
# incluant désormais le calcul + l'affichage des TOP 3 recommandations
# dans l'onglet "Match Parfait" après clic sur "LANCER L'ANALYSE".

source("server.R", local = TRUE)

# IMPORTANT: Shiny attend que la dernière expression du fichier soit la fonction serveur.
server