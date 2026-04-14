# ============================================================
# PROJET - RECORD LINKAGE SANS IDENTIFIANT COMMUN
# ============================================================
# Consigne
# Apparier 50 000 naissances hospitalieres avec un registre
# d'etat civil de 42 000 enregistrements.
# Aucun identifiant commun.
# Noms saisis differemment, dates approximatives,
# transcriptions wolof/bambara variables.
#
# Donnees disponibles
# - prenom, nom, sexe, date_naissance
# - prenom_mere, nom_mere
# - prenom_pere, nom_pere
# - commune
#
# Etape 1 : simulation des donnees
# Etape 2 : blocking multi-passes pour reduire le nombre
#           de comparaisons candidates
# ============================================================

charger_packages <- function(packages) {
  packages_manquants <- packages[!vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]

  if (length(packages_manquants) > 0) {
    install.packages(packages_manquants)
  }

  invisible(lapply(
    packages,
    function(pkg) suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  ))
}

charger_packages(c("tidyverse", "stringi", "openxlsx"))

set.seed(42)

# ============================================================
# 1. LISTES DE NOMS
# ============================================================

noms_famille <- c(
  "Diallo", "Diop", "Ndiaye", "Fall", "Sow", "Kane", "Ba", "Sy",
  "Mbaye", "Sarr", "Thiam", "Gueye", "Diouf", "Faye", "Cisse",
  "Coulibaly", "Traore", "Keita", "Kouyate", "Camara", "Dembele",
  "Toure", "Sangare", "Kone", "Sissoko", "Diabate", "Doumbia",
  "Ndoye", "Badji", "Manga", "Tendeng", "Diatta", "Sambou"
)

prenoms_garcons <- c(
  "Mamadou", "Ibrahima", "Oumar", "Moussa", "Abdoulaye", "Modou",
  "Cheikh", "Seydou", "Boubacar", "Aliou", "Lamine", "Samba",
  "Ousmane", "Babacar", "Pape", "Serigne", "Idrissa", "Malick",
  "Souleymane", "Amadou", "Tidiane", "Demba", "Daouda", "Ismaila"
)

prenoms_filles <- c(
  "Fatou", "Aminata", "Mariama", "Aissatou", "Rokhaya", "Ndeye",
  "Khady", "Coumba", "Adja", "Fatoumata", "Binta", "Maimouna",
  "Oumou", "Kadiatou", "Mariam", "Hawa", "Fanta", "Salimata",
  "Ramata", "Djeneba", "Awa", "Nafi", "Kadija", "Tenin"
)

communes <- c(
  "Dakar", "Pikine", "Guediawaye", "Rufisque", "Thies", "Kaolack",
  "Ziguinchor", "Saint-Louis", "Tambacounda", "Kolda", "Louga",
  "Fatick", "Kaffrine", "Sedhiou", "Kedougou", "Matam", "Diourbel",
  "Mbour", "Touba", "Tivaouane"
)

# ============================================================
# 2. FONCTIONS D'ERREURS
# ============================================================

variantes_transcription <- function(nom) {
  if (is.na(nom)) {
    return(NA_character_)
  }

  variantes <- list(
    "Oumar" = c("Omar", "Umar", "Oumare"),
    "Mamadou" = c("Mamadu", "Mamadoux", "Amadou"),
    "Ibrahima" = c("Ibrahim", "Ibrahiema", "Braima"),
    "Fatoumata" = c("Fatou", "Fatouma", "Fatimata"),
    "Mariama" = c("Mariam", "Maryam", "Mariamma"),
    "Abdoulaye" = c("Abdoulay", "Abdullahi", "Abdoulai"),
    "Moussa" = c("Musa", "Mousa", "Moussa"),
    "Aissatou" = c("Aichatou", "Aissata", "Aichata"),
    "Souleymane" = c("Suleyman", "Soulaymane", "Souleyman"),
    "Coulibaly" = c("Kulibaly", "Coulibali", "Coulibaley"),
    "Ndiaye" = c("Ndiay", "Ndiae", "Niaye"),
    "Diallo" = c("Dialo", "Dyallo", "Diallou"),
    "Traore" = c("Traore", "Traoreh", "Trawre"),
    "Cheikh" = c("Cheick", "Shaikh", "Cheikh")
  )

  if (nom %in% names(variantes)) {
    return(sample(variantes[[nom]], 1))
  }

  nom
}

introduire_faute <- function(chaine, prob = 0.3) {
  if (is.na(chaine) || runif(1) > prob || nchar(chaine) < 3) {
    return(chaine)
  }

  type_faute <- sample(c("substitution", "suppression", "doublon"), 1)
  pos <- sample(2:(nchar(chaine) - 1), 1)
  lettres <- strsplit(chaine, "", fixed = TRUE)[[1]]

  if (type_faute == "substitution") {
    lettres[pos] <- sample(letters, 1)
    return(paste(lettres, collapse = ""))
  }

  if (type_faute == "suppression") {
    return(paste(lettres[-pos], collapse = ""))
  }

  lettres <- append(lettres, lettres[pos], after = pos)
  paste(lettres, collapse = "")
}

decaler_date <- function(date, max_jours = 30) {
  if (is.na(date)) {
    return(as.Date(NA))
  }

  date + sample(seq.int(-max_jours, max_jours), 1)
}

normaliser_casse <- function(texte) {
  if (is.na(texte)) {
    return(NA_character_)
  }

  choix <- runif(1)

  if (choix < 0.7) {
    return(toupper(texte))
  }

  if (choix < 0.9) {
    return(str_to_title(texte))
  }

  tolower(texte)
}

appliquer_avec_prob_chr <- function(x, prob, fn) {
  out <- x
  idx <- !is.na(out) & runif(length(out)) < prob

  if (any(idx)) {
    out[idx] <- vapply(out[idx], fn, FUN.VALUE = character(1))
  }

  out
}

decaler_dates_prob <- function(x, prob, max_jours = 30) {
  out <- x
  idx <- !is.na(out) & runif(length(out)) < prob

  if (any(idx)) {
    out[idx] <- out[idx] + sample(
      seq.int(-max_jours, max_jours),
      sum(idx),
      replace = TRUE
    )
  }

  out
}

mettre_na_avec_prob <- function(x, prob) {
  out <- x
  idx <- runif(length(out)) < prob
  out[idx] <- NA
  out
}

# ============================================================
# 3. ETAPE 1 - SIMULATION DES DONNEES
# ============================================================

n_hopital <- 50000L
n_vrais <- 38000L
n_faux <- 4000L

stopifnot(n_vrais + n_faux == 42000L)

nom_pere_vec <- sample(noms_famille, n_hopital, replace = TRUE)
prenom_pere_vec <- sample(prenoms_garcons, n_hopital, replace = TRUE)
prenom_mere_vec <- sample(prenoms_filles, n_hopital, replace = TRUE)
nom_mere_vec <- sample(noms_famille, n_hopital, replace = TRUE)
sexe_vec <- sample(c("M", "F"), n_hopital, replace = TRUE)

prenom_vec <- ifelse(
  sexe_vec == "M",
  sample(prenoms_garcons, n_hopital, replace = TRUE),
  sample(prenoms_filles, n_hopital, replace = TRUE)
)

nom_vec <- nom_pere_vec

hopital <- tibble(
  id_hopital = paste0("H", str_pad(seq_len(n_hopital), 6, pad = "0")),
  prenom = prenom_vec,
  nom = nom_vec,
  sexe = sexe_vec,
  date_naissance = sample(
    seq(as.Date("2018-01-01"), as.Date("2023-12-31"), by = "day"),
    n_hopital,
    replace = TRUE
  ),
  prenom_mere = prenom_mere_vec,
  nom_mere = nom_mere_vec,
  prenom_pere = prenom_pere_vec,
  nom_pere = nom_pere_vec,
  commune = sample(communes, n_hopital, replace = TRUE)
)

cat("Liste Hopital generee :", nrow(hopital), "enregistrements\n")
cat(
  "Verification coherence nom enfant = nom pere :",
  all(hopital$nom == hopital$nom_pere),
  "\n"
)

idx_vrais <- sample(seq_len(n_hopital), n_vrais, replace = FALSE)
base_ec <- hopital[idx_vrais, ]

prenom_ec <- base_ec$prenom
nom_ec <- base_ec$nom
inversion_idx <- runif(n_vrais) < 0.05

prenom_ec[inversion_idx] <- base_ec$nom[inversion_idx]
nom_ec[inversion_idx] <- base_ec$prenom[inversion_idx]

prenom_ec <- appliquer_avec_prob_chr(prenom_ec, 0.30, variantes_transcription)
nom_ec <- appliquer_avec_prob_chr(nom_ec, 0.20, variantes_transcription)

prenom_ec <- appliquer_avec_prob_chr(prenom_ec, 0.25, introduire_faute)
nom_ec <- appliquer_avec_prob_chr(nom_ec, 0.25, introduire_faute)
prenom_mere_ec <- appliquer_avec_prob_chr(base_ec$prenom_mere, 0.20, introduire_faute)
nom_mere_ec <- appliquer_avec_prob_chr(base_ec$nom_mere, 0.20, introduire_faute)
prenom_pere_ec <- appliquer_avec_prob_chr(base_ec$prenom_pere, 0.20, introduire_faute)
nom_pere_ec <- appliquer_avec_prob_chr(base_ec$nom_pere, 0.20, introduire_faute)

date_naissance_ec <- decaler_dates_prob(base_ec$date_naissance, 0.40, max_jours = 30)

prenom_ec <- vapply(prenom_ec, normaliser_casse, FUN.VALUE = character(1))
nom_ec <- vapply(nom_ec, normaliser_casse, FUN.VALUE = character(1))

commune_ec <- mettre_na_avec_prob(base_ec$commune, 0.08)
prenom_pere_ec <- mettre_na_avec_prob(prenom_pere_ec, 0.05)
nom_pere_ec <- mettre_na_avec_prob(nom_pere_ec, 0.05)

etat_civil_vrais <- tibble(
  id_vrai_hopital = base_ec$id_hopital,
  id_etat_civil = paste0("EC", str_pad(seq_len(n_vrais), 6, pad = "0")),
  prenom = prenom_ec,
  nom = nom_ec,
  sexe = base_ec$sexe,
  date_naissance = date_naissance_ec,
  prenom_mere = prenom_mere_ec,
  nom_mere = nom_mere_ec,
  prenom_pere = prenom_pere_ec,
  nom_pere = nom_pere_ec,
  commune = commune_ec
)

nom_pere_faux <- sample(noms_famille, n_faux, replace = TRUE)
prenom_pere_faux <- sample(prenoms_garcons, n_faux, replace = TRUE)
sexe_faux <- sample(c("M", "F"), n_faux, replace = TRUE)

etat_civil_faux <- tibble(
  id_vrai_hopital = NA_character_,
  id_etat_civil = paste0(
    "EC",
    str_pad((n_vrais + 1):(n_vrais + n_faux), 6, pad = "0")
  ),
  prenom = ifelse(
    sexe_faux == "M",
    sample(prenoms_garcons, n_faux, replace = TRUE),
    sample(prenoms_filles, n_faux, replace = TRUE)
  ),
  nom = nom_pere_faux,
  sexe = sexe_faux,
  date_naissance = sample(
    seq(as.Date("2018-01-01"), as.Date("2023-12-31"), by = "day"),
    n_faux,
    replace = TRUE
  ),
  prenom_mere = sample(prenoms_filles, n_faux, replace = TRUE),
  nom_mere = sample(noms_famille, n_faux, replace = TRUE),
  prenom_pere = prenom_pere_faux,
  nom_pere = nom_pere_faux,
  commune = sample(communes, n_faux, replace = TRUE)
)

etat_civil <- bind_rows(etat_civil_vrais, etat_civil_faux) %>%
  slice_sample(prop = 1)

cat("Liste Etat Civil generee :", nrow(etat_civil), "enregistrements\n")
cat(
  "  dont",
  n_vrais,
  "vrais appariements et",
  n_faux,
  "sans correspondance\n"
)

cat("\n--- Apercu Liste Hopital ---\n")
print(head(hopital, 5))

cat("\n--- Apercu Liste Etat Civil ---\n")
print(head(etat_civil %>% select(-id_vrai_hopital), 5))

cat("\n--- Taux de donnees manquantes Etat Civil (%) ---\n")
etat_civil %>%
  summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 1))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_NA") %>%
  filter(pct_NA > 0) %>%
  print()

cat("\n--- Exemple : meme individu dans les deux listes ---\n")
exemple_id <- etat_civil %>%
  filter(!is.na(id_vrai_hopital)) %>%
  slice(1) %>%
  pull(id_vrai_hopital)

cat(">> Dans l'hopital :\n")
print(
  hopital %>%
    filter(id_hopital == exemple_id) %>%
    select(prenom, nom, date_naissance, prenom_pere, nom_pere, commune)
)

cat(">> Dans l'etat civil (avec erreurs) :\n")
print(
  etat_civil %>%
    filter(id_vrai_hopital == exemple_id) %>%
    select(prenom, nom, date_naissance, prenom_pere, nom_pere, commune)
)

write_csv(hopital, "liste_hopital.csv")
write_csv(etat_civil, "liste_etat_civil.csv")
openxlsx::write.xlsx(hopital, file = "hopital.xlsx", overwrite = TRUE)
openxlsx::write.xlsx(etat_civil, file = "etat_civil.xlsx", overwrite = TRUE)

# ============================================================
# 4. ETAPE 2 - BLOCKING MULTI-PASSES
# ============================================================
# Objectif : reduire les 50 000 x 42 000 = 2,1 milliards
#            de paires a un ensemble gerable de candidats
# ============================================================

nettoyer <- function(x) {
  x %>%
    str_to_upper() %>%
    str_trim() %>%
    stringi::stri_trans_general("Latin-ASCII")
}

preparer_cles <- function(df, prefix) {
  out <- df %>%
    mutate(
      nom_clean = nettoyer(nom),
      prenom_clean = nettoyer(prenom),
      nom_pere_clean = if_else(is.na(nom_pere), NA_character_, nettoyer(nom_pere)),
      commune_clean = if_else(is.na(commune), NA_character_, nettoyer(commune)),
      init_nom = str_sub(nom_clean, 1, 1),
      init_prenom = str_sub(prenom_clean, 1, 1),
      annee_naiss = as.integer(format(date_naissance, "%Y")),
      nom_pere_cl = nom_pere_clean,
      commune_cl = commune_clean
    )

  names(out) <- paste0(prefix, "_", names(out))
  out
}

A <- hopital %>%
  mutate(id_A = id_hopital) %>%
  preparer_cles("A")

B <- etat_civil %>%
  mutate(id_B = id_etat_civil) %>%
  preparer_cles("B")

cat("\nCles de blocking preparees\n")
cat("  Liste A :", nrow(A), "enregistrements\n")
cat("  Liste B :", nrow(B), "enregistrements\n\n")

passe1 <- inner_join(
  A %>% select(A_id_A, A_init_nom, A_init_prenom, A_annee_naiss, A_commune_cl),
  B %>% select(B_id_B, B_init_nom, B_init_prenom, B_annee_naiss, B_commune_cl),
  by = c(
    "A_init_nom" = "B_init_nom",
    "A_init_prenom" = "B_init_prenom",
    "A_annee_naiss" = "B_annee_naiss",
    "A_commune_cl" = "B_commune_cl"
  ),
  na_matches = "never"
) %>%
  select(id_A = A_id_A, id_B = B_id_B) %>%
  mutate(passe = 1L)

cat("Passe 1 (init_nom + init_prenom + annee + commune) :", nrow(passe1), "paires\n")

passe2 <- inner_join(
  A %>% select(A_id_A, A_init_nom, A_annee_naiss, A_nom_pere_cl),
  B %>%
    filter(!is.na(B_nom_pere_cl)) %>%
    select(B_id_B, B_init_nom, B_annee_naiss, B_nom_pere_cl),
  by = c(
    "A_init_nom" = "B_init_nom",
    "A_annee_naiss" = "B_annee_naiss",
    "A_nom_pere_cl" = "B_nom_pere_cl"
  ),
  na_matches = "never"
) %>%
  select(id_A = A_id_A, id_B = B_id_B) %>%
  mutate(passe = 2L)

cat("Passe 2 (init_nom + annee + nom_pere)             :", nrow(passe2), "paires\n")

A_annees <- A %>% select(A_id_A, A_init_nom, A_init_prenom, A_annee_naiss)
B_annees <- B %>% select(B_id_B, B_init_nom, B_init_prenom, B_annee_naiss)

passe3 <- bind_rows(
  inner_join(
    A_annees,
    B_annees,
    by = c(
      "A_init_nom" = "B_init_nom",
      "A_init_prenom" = "B_init_prenom",
      "A_annee_naiss" = "B_annee_naiss"
    ),
    na_matches = "never"
  ),
  inner_join(
    A_annees,
    B_annees %>% mutate(B_annee_naiss = B_annee_naiss - 1L),
    by = c(
      "A_init_nom" = "B_init_nom",
      "A_init_prenom" = "B_init_prenom",
      "A_annee_naiss" = "B_annee_naiss"
    ),
    na_matches = "never"
  ),
  inner_join(
    A_annees,
    B_annees %>% mutate(B_annee_naiss = B_annee_naiss + 1L),
    by = c(
      "A_init_nom" = "B_init_nom",
      "A_init_prenom" = "B_init_prenom",
      "A_annee_naiss" = "B_annee_naiss"
    ),
    na_matches = "never"
  )
) %>%
  distinct(A_id_A, B_id_B) %>%
  select(id_A = A_id_A, id_B = B_id_B) %>%
  mutate(passe = 3L)

cat("Passe 3 (init_nom + init_prenom + annee +/-1)     :", nrow(passe3), "paires\n")

passe4 <- inner_join(
  A %>% select(A_id_A, A_init_prenom, A_annee_naiss, A_nom_pere_cl),
  B %>%
    filter(!is.na(B_nom_pere_cl)) %>%
    select(B_id_B, B_init_prenom, B_annee_naiss, B_nom_pere_cl),
  by = c(
    "A_init_prenom" = "B_init_prenom",
    "A_annee_naiss" = "B_annee_naiss",
    "A_nom_pere_cl" = "B_nom_pere_cl"
  ),
  na_matches = "never"
) %>%
  select(id_A = A_id_A, id_B = B_id_B) %>%
  mutate(passe = 4L)

cat("Passe 4 (init_prenom + annee + nom_pere)          :", nrow(passe4), "paires\n")

paires_candidates <- bind_rows(passe1, passe2, passe3, passe4) %>%
  group_by(id_A, id_B) %>%
  summarise(passe_min = min(passe), .groups = "drop")

cat(
  "\nPaires candidates (apres union + dedoublonnage) :",
  nrow(paires_candidates),
  "\n"
)

vrais_liens <- etat_civil %>%
  filter(!is.na(id_vrai_hopital)) %>%
  select(id_A = id_vrai_hopital, id_B = id_etat_civil)

n_vrais_total <- nrow(vrais_liens)

vrais_retrouves <- vrais_liens %>%
  semi_join(paires_candidates, by = c("id_A", "id_B"))

n_retrouves <- nrow(vrais_retrouves)
n_candidates <- nrow(paires_candidates)
n_paires_totales <- nrow(hopital) * nrow(etat_civil)

rappel_blocking <- round(n_retrouves / n_vrais_total * 100, 2)
precision_paires <- if (n_candidates > 0) {
  round(n_retrouves / n_candidates * 100, 2)
} else {
  NA_real_
}

reduction_ratio <- round(
  (1 - n_candidates / n_paires_totales) * 100,
  4
)

cat("\n======================================\n")
cat("  EVALUATION DU BLOCKING\n")
cat("======================================\n")
cat("Vrais appariements totaux          :", n_vrais_total, "\n")
cat("Vrais appariements retrouves       :", n_retrouves, "\n")
cat("Rappel du blocking                 :", rappel_blocking, "%\n")
cat("% vrais liens parmi les candidats  :", precision_paires, "%\n")
cat("Reduction des comparaisons         :", reduction_ratio, "%\n")
cat(
  "(sans blocking : 50k x 42k =",
  format(n_paires_totales, big.mark = " "),
  "paires)\n"
)

cat("\n--- Vrais liens recuperes par passe ---\n")
paires_candidates %>%
  left_join(
    vrais_liens %>% mutate(vrai = TRUE),
    by = c("id_A", "id_B")
  ) %>%
  mutate(vrai = replace_na(vrai, FALSE)) %>%
  group_by(passe_min) %>%
  summarise(
    n_paires = n(),
    n_vrais = sum(vrai),
    pct_vrais = round(mean(vrai) * 100, 1),
    .groups = "drop"
  ) %>%
  rename(passe = passe_min) %>%
  print()

write_csv(paires_candidates, "paires_candidates.csv")

cat("\nPret pour l'etape 3 - calcul des scores de similarite\n")
