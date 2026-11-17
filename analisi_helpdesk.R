# ==============================================================================
# ANALISI DEI FLUSSI TELEFONICI DI UN HELP DESK - CONFRONTO TRA DUE ANNI
#
# Parte da un dataset normalizzato (una riga per chiamata) e produce:
#   - KPI mensili, per operatore, per periodo e per fase organizzativa;
#   - confronto su mesi omogenei tra i due anni;
#   - sette scenari di sterilizzazione (S0-S6) per misurare quanto contano i filtri;
#   - boxplot, outlier di Tukey e registro delle anomalie tracciabile;
#   - approfondimento sulle chiamate lunghe escluse dal cap;
#   - controlli di qualita' del dato, sensibilita' alle soglie, grafici PNG e note.
#
# Requisiti: R >= 4.1, nessun pacchetto (solo 'readxl' se si leggono gli XLSX).
# Esecuzione:  Rscript analisi_helpdesk.R [percorso/dati.csv]
# Output:      cartella Risultati_R (tabelle CSV, Grafici/, LEGGIMI_metodo.txt)
#
# Il dataset NON e' incluso nel repository. Per provare lo script con dati
# inventati: Rscript genera_dati_esempio.R
#
# Colonne richieste nel CSV:
#   timestamp (AAAA-MM-GG hh:mm:ss), attesa_sec, conversazione_sec, operatore,
#   classificazione (Servite e Trasferite / Rifiutata / Abbandonate /
#   Fuori tempo massimo / Fuori servizio)
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 50)

# ------------------------------------------------------------------------------
# CONFIGURAZIONE
# ------------------------------------------------------------------------------
ANNO_1 <- 2025L                      # anno di partenza
ANNO_2 <- 2026L                      # anno a confronto
MESI_COMPARABILI <- 3:10             # mesi presenti in modo completo in entrambi gli anni
MESI_PARZIALI <- c("2025-02", "2026-11")   # mesi con estrazione incompleta (solo per i grafici)

MIN_SECONDI <- 30                    # sotto questa durata la conversazione non e' "valida"
QUANTILE_OUTLIER <- 0.995            # le conversazioni oltre questo percentile sono outlier
# Cap usato nei KPI principali: "P995" (percentile) oppure "TUKEY" (Q3 + 1,5 x IQR globale)
METODO_OUTLIER <- "P995"
MIN_SCENARIO_BASSO <- 15             # soglia minima dello scenario S5
MIN_SCENARIO_ALTO <- 60              # soglia minima dello scenario S6

# Fasi organizzative: mesi da confrontare separatamente (esempio)
FASI <- list("Mar-Ago" = 3:8, "Set-Ott" = 9:10)
# Organico nominale: operatori previsti a partire dal mese indicato (esempio)
ORGANICO_NOMINALE <- data.frame(da_mese = c("0000-01", "2025-09"),
                                operatori = c(3L, 2L), stringsAsFactors = FALSE)

# Riservatezza: sostituisce i nomi degli operatori con "Operatore A", "B", ...
# prima di qualunque elaborazione, cosi' tabelle e grafici non contengono nomi.
ANONIMIZZA_OPERATORI <- TRUE
N_OPERATORI_GRAFICI <- 3             # operatori mostrati nei grafici per operatore
SOGLIA_ZERO_ANOMALIA <- 50           # % di servite a durata zero oltre cui segnalare l'operatore

# Controllo opzionale: totali stampati nel riepilogo delle esportazioni originali.
# NULL = controllo disattivato. Esempio:
# RIEPILOGO_ESTRAZIONI <- list(
#   "2025" = c(totale = NA, servite = NA, fuori_servizio = NA, rifiutate = NA, fuori_tempo = NA, abbandonate = NA),
#   "2026" = c(totale = NA, servite = NA, fuori_servizio = NA, rifiutate = NA, fuori_tempo = NA, abbandonate = NA))
RIEPILOGO_ESTRAZIONI <- NULL

# Lettura diretta degli XLSX (alternativa al CSV): ADATTARE al tracciato dell'esportazione
XLSX_PATTERN <- "%d_chiamate.xlsx"   # nome file: anno -> file
XLSX_RIGA_INIZIO <- 14               # prima riga con dati
XLSX_COLONNE <- list(                # colonne di: data, attesa, conversazione, operatore,
  "2025" = c(2, 5, 7, 10, 15, 18, 21),   #   classificazione, causa terminazione, operatori loggati
  "2026" = c(2, 4, 6, 9, 14, 17, 20))

if (!METODO_OUTLIER %in% c("P995", "TUKEY")) stop("METODO_OUTLIER deve essere P995 o TUKEY")

# ------------------------------------------------------------------------------
# PERCORSI
# ------------------------------------------------------------------------------
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
base_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[1]), mustWork = FALSE)) else getwd()
args <- commandArgs(trailingOnly = TRUE)
data_path <- if (length(args)) args[1] else file.path(base_dir, "HelpDesk_dati_per_R.csv")
out_dir <- file.path(base_dir, "Risultati_R")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fig_dir <- file.path(out_dir, "Grafici")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# IMPORTAZIONE
# ------------------------------------------------------------------------------
sec_from_hms <- function(x) {
  x <- as.character(x)
  p <- strsplit(x, ":", fixed = TRUE)
  vapply(p, function(a) {
    if (length(a) != 3L || any(is.na(suppressWarnings(as.numeric(a))))) return(NA_real_)
    z <- as.numeric(a)
    z[1] * 3600 + z[2] * 60 + z[3]
  }, numeric(1))
}

read_xlsx <- function(year) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Installare 'readxl': install.packages('readxl')")
  p <- file.path(base_dir, sprintf(XLSX_PATTERN, year))
  if (!file.exists(p)) stop(paste("File non trovato:", p))
  ix <- XLSX_COLONNE[[as.character(year)]]
  if (is.null(ix)) stop("Colonne XLSX non configurate per l'anno ", year)
  raw <- as.data.frame(readxl::read_excel(p, range = readxl::cell_limits(c(XLSX_RIGA_INIZIO, 1), c(NA, 26)),
                                          col_names = FALSE, .name_repair = "minimal"))
  valid_date <- grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:", as.character(raw[[ix[1]]]))
  riga_xlsx <- which(!is.na(valid_date) & valid_date) + XLSX_RIGA_INIZIO - 1L   # riga nell'esportazione
  raw <- raw[!is.na(valid_date) & valid_date, , drop = FALSE]
  dt <- as.POSIXct(as.character(raw[[ix[1]]]), format = "%d/%m/%Y %H:%M:%S", tz = "Europe/Rome")
  data.frame(
    anno = year,
    riga_origine = riga_xlsx,
    timestamp = format(dt, "%Y-%m-%d %H:%M:%S"),
    attesa_sec = sec_from_hms(raw[[ix[2]]]),
    conversazione_sec = sec_from_hms(raw[[ix[3]]]),
    operatore = sub("^[0-9]+ - ", "", as.character(raw[[ix[4]]])),   # toglie il prefisso numerico
    classificazione = as.character(raw[[ix[5]]]),
    causa_terminazione = as.character(raw[[ix[6]]]),
    operatori_loggati = suppressWarnings(as.numeric(raw[[ix[7]]])),
    stringsAsFactors = FALSE
  )
}

if (file.exists(data_path)) {
  d <- read.csv(data_path, check.names = FALSE, fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))
} else {
  message("CSV assente: lettura diretta delle esportazioni XLSX con readxl")
  d <- rbind(read_xlsx(ANNO_1), read_xlsx(ANNO_2))
}
required <- c("timestamp", "attesa_sec", "conversazione_sec", "operatore", "classificazione")
if (!all(required %in% names(d))) stop("Mancano colonne indispensabili: ", paste(setdiff(required, names(d)), collapse = ", "))

d$anno <- as.integer(substr(d$timestamp, 1, 4))
# Tracciabilita': riga di origine e identificativo del record (senza numeri di telefono)
if (!"fonte" %in% names(d)) d$fonte <- paste0("estrazione_", d$anno)
if (!"riga_origine" %in% names(d)) d$riga_origine <- ave(seq_len(nrow(d)), d$anno, FUN = seq_along)
d$riga_origine <- suppressWarnings(as.integer(d$riga_origine))
if (!"id_chiamata" %in% names(d)) d$id_chiamata <- sprintf("HD%d-%05d", d$anno, d$riga_origine)
d$mese <- substr(d$timestamp, 1, 7)
d$mese_num <- as.integer(substr(d$timestamp, 6, 7))
d$data <- substr(d$timestamp, 1, 10)
d$attesa_sec <- as.numeric(d$attesa_sec)
d$conversazione_sec <- as.numeric(d$conversazione_sec)
d$operatore[is.na(d$operatore)] <- ""
d$classificazione[is.na(d$classificazione)] <- ""
d$servita <- d$classificazione == "Servite e Trasferite"
d$comparabile <- d$mese_num %in% MESI_COMPARABILI
if (any(is.na(d$conversazione_sec)) || any(is.na(d$attesa_sec))) warning("Sono presenti durate mancanti: verificare le estrazioni")

# ------------------------------------------------------------------------------
# ANONIMIZZAZIONE DEGLI OPERATORI (in memoria: la corrispondenza non viene salvata)
# ------------------------------------------------------------------------------
if (ANONIMIZZA_OPERATORI) {
  presenti <- nzchar(d$operatore)
  ordine <- names(sort(table(d$operatore[presenti & d$servita]), decreasing = TRUE))
  ordine <- c(ordine, setdiff(unique(d$operatore[presenti]), ordine))
  etichette <- if (length(ordine) <= 26) paste("Operatore", LETTERS[seq_along(ordine)]) else paste("Operatore", seq_along(ordine))
  d$operatore[presenti] <- etichette[match(d$operatore[presenti], ordine)]
}

# ------------------------------------------------------------------------------
# DEFINIZIONI: servita, breve, outlier, valida
# La soglia outlier e' comune ai due anni e stimata SOLO sui mesi comparabili.
# ------------------------------------------------------------------------------
reference <- d$conversazione_sec[d$comparabile & d$servita & d$conversazione_sec >= MIN_SECONDI]
CAP_P995 <- as.numeric(quantile(reference, QUANTILE_OUTLIER, na.rm = TRUE, type = 7))
Q_GLOBALI <- as.numeric(quantile(reference, c(.25, .5, .75), na.rm = TRUE, type = 7))
IQR_GLOBALE <- Q_GLOBALI[3] - Q_GLOBALI[1]
CAP_TUKEY <- Q_GLOBALI[3] + 1.5 * IQR_GLOBALE
CAP_OUTLIER <- if (METODO_OUTLIER == "TUKEY") CAP_TUKEY else CAP_P995
d$zero <- d$servita & d$conversazione_sec == 0
d$breve <- d$servita & d$conversazione_sec < MIN_SECONDI
d$outlier <- d$servita & d$conversazione_sec >= MIN_SECONDI & d$conversazione_sec > CAP_OUTLIER
d$valida <- d$servita & d$conversazione_sec >= MIN_SECONDI & d$conversazione_sec <= CAP_OUTLIER
# Le chiamate non servite e quelle brevi/outlier restano SEMPRE nel totale ricevuto.

# Outlier di Tukey MENSILI: Q1 e Q3 di ogni mese, calcolati sulle servite >= soglia minima
# PRIMA di qualsiasi filtro superiore. Sono segnalazioni diagnostiche, non errori accertati.
eligible <- d$servita & !is.na(d$conversazione_sec) & d$conversazione_sec >= MIN_SECONDI
d$q1_mese <- NA_real_; d$q3_mese <- NA_real_; d$tukey_sup_mese <- NA_real_; d$tukey_inf_mese <- NA_real_
for (m in unique(d$mese)) {
  ix <- d$mese == m
  v <- d$conversazione_sec[ix & eligible]
  if (length(v) >= 4) {
    q <- as.numeric(quantile(v, c(.25, .75), type = 7)); iq <- q[2] - q[1]
    d$q1_mese[ix] <- q[1]; d$q3_mese[ix] <- q[2]
    d$tukey_sup_mese[ix] <- q[2] + 1.5 * iq; d$tukey_inf_mese[ix] <- q[1] - 1.5 * iq
  }
}
d$flag_tukey_mensile <- eligible & !is.na(d$tukey_sup_mese) &
  (d$conversazione_sec > d$tukey_sup_mese | d$conversazione_sec < d$tukey_inf_mese)

# ------------------------------------------------------------------------------
# FUNZIONI DI SUPPORTO
# ------------------------------------------------------------------------------
mmean <- function(x) if (length(x)) mean(x, na.rm = TRUE) else NA_real_
median0 <- function(x) if (length(x)) median(x, na.rm = TRUE) else NA_real_
p90 <- function(x) if (length(x)) as.numeric(quantile(x, .90, na.rm = TRUE, type = 7)) else NA_real_
percentage <- function(a, b) if (b > 0) 100 * a / b else NA_real_
nomi_mesi <- c("Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago", "Set", "Ott", "Nov", "Dic")
etichetta_mesi <- function(mm) paste(nomi_mesi[min(mm)], nomi_mesi[max(mm)], sep = "-")

# Fase organizzativa di un mese (secondo FASI)
periodo <- function(m) {
  for (n in names(FASI)) if (m %in% FASI[[n]]) return(n)
  "Altro"
}
# Organico nominale in vigore in un mese (secondo ORGANICO_NOMINALE)
nominal <- function(y, m) {
  chiave <- sprintf("%04d-%02d", y, m)
  utili <- ORGANICO_NOMINALE[ORGANICO_NOMINALE$da_mese <= chiave, , drop = FALSE]
  utili$operatori[nrow(utili)]
}

# ------------------------------------------------------------------------------
# KPI MENSILI (percentuali sul totale ricevuto, non sulla risposta registrata)
# ------------------------------------------------------------------------------
month_records <- lapply(split(d, d$mese), function(g) {
  mm <- as.integer(substr(g$mese[1], 6, 7)); y <- g$anno[1]
  h <- g[g$valida, , drop = FALSE]; s <- g[g$servita, , drop = FALSE]
  active_days <- length(unique(g$data))
  data.frame(mese = g$mese[1], anno = y, mese_num = mm,
    periodo = periodo(mm), confrontabile = as.integer(mm %in% MESI_COMPARABILI),
    parziale = as.integer(g$mese[1] %in% MESI_PARZIALI),
    chiamate_totali = nrow(g), servite_registrate = nrow(s), gestite_valide = nrow(h),
    servite_zero_sec = sum(g$zero), servite_brevi_sotto_soglia = sum(g$breve), servite_outlier = sum(g$outlier),
    rifiutate = sum(g$classificazione == "Rifiutata"), abbandonate = sum(g$classificazione == "Abbandonate"),
    fuori_tempo = sum(g$classificazione == "Fuori tempo massimo"), fuori_servizio = sum(g$classificazione == "Fuori servizio"),
    tasso_servite_pct = percentage(nrow(s), nrow(g)), tasso_gestite_valide_pct = percentage(nrow(h), nrow(g)),
    quota_valide_su_servite_pct = percentage(nrow(h), nrow(s)),
    tasso_rifiutate_pct = percentage(sum(g$classificazione == "Rifiutata"), nrow(g)),
    tasso_abbandonate_pct = percentage(sum(g$classificazione == "Abbandonate"), nrow(g)),
    tasso_non_risposte_pct = percentage(sum(g$classificazione %in% c("Rifiutata", "Abbandonate", "Fuori tempo massimo", "Fuori servizio")), nrow(g)),
    durata_media_sec = mmean(h$conversazione_sec), durata_mediana_sec = median0(h$conversazione_sec),
    durata_p90_sec = p90(h$conversazione_sec), minuti_conversazione = sum(h$conversazione_sec) / 60,
    attesa_media_sec = mmean(h$attesa_sec), attesa_p90_sec = p90(h$attesa_sec),
    attesa_entro_60_sec_pct = percentage(sum(h$attesa_sec <= 60), nrow(h)),
    giorni_con_chiamate = active_days, chiamate_per_giorno_attivo = nrow(g) / active_days,
    gestite_valide_per_giorno_attivo = nrow(h) / active_days,
    prima_chiamata = min(g$timestamp), ultima_chiamata = max(g$timestamp),
    operatori_nominali = nominal(y, mm), gestite_valide_per_operatore_nominale = nrow(h) / nominal(y, mm))
})
M <- do.call(rbind, month_records); rownames(M) <- NULL
M <- M[order(M$anno, M$mese_num), ]

# Variazioni mese su mese: % per volumi e durate, punti percentuali per i tassi
changes <- c("chiamate_totali", "servite_registrate", "gestite_valide", "tasso_gestite_valide_pct",
             "durata_media_sec", "tasso_rifiutate_pct", "tasso_abbandonate_pct")
for (v in changes) {
  pp <- grepl("pct$", v); nm <- paste0("var_mensile_", v, if (pp) "_pp" else "_pct")
  M[[nm]] <- NA_real_
  for (y in unique(M$anno)) {
    ix <- which(M$anno == y)
    if (length(ix) < 2) next
    last <- M[[v]][ix[-length(ix)]]; current <- M[[v]][ix[-1]]
    M[[nm]][ix[-1]] <- if (pp) current - last else ifelse(last != 0, 100 * (current / last - 1), NA_real_)
  }
}
write.csv(M, file.path(out_dir, "KPI_mensili.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
C <- M[M$confrontabile == 1, , drop = FALSE]
write.csv(C, file.path(out_dir, "KPI_mensili_comparabili.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# KPI PER OPERATORE: servite registrate e conversazioni valide sono due cose diverse
# ------------------------------------------------------------------------------
sv <- d[d$servita & nzchar(d$operatore), , drop = FALSE]
keys <- unique(sv[c("mese", "operatore")]); keys <- keys[order(keys$mese, keys$operatore), ]
operator_records <- lapply(seq_len(nrow(keys)), function(i) {
  ym <- keys$mese[i]; op <- keys$operatore[i]
  g <- sv[sv$mese == ym & sv$operatore == op, , drop = FALSE]; h <- g[g$valida, , drop = FALSE]
  m <- M[M$mese == ym, , drop = FALSE]
  data.frame(mese = ym, anno = as.integer(substr(ym, 1, 4)), mese_num = as.integer(substr(ym, 6, 7)),
    operatore = op, confrontabile = m$confrontabile, servite_registrate = nrow(g), gestite_valide = nrow(h),
    quota_valide_su_totale_mese_pct = percentage(nrow(h), m$gestite_valide),
    quota_su_ricevute_pct = percentage(nrow(h), m$chiamate_totali),
    servite_zero_sec = sum(g$zero), servite_brevi = sum(g$breve), servite_outlier = sum(g$outlier),
    durata_media_sec = mmean(h$conversazione_sec), durata_mediana_sec = median0(h$conversazione_sec),
    durata_p90_sec = p90(h$conversazione_sec), minuti_conversazione = sum(h$conversazione_sec) / 60,
    attesa_media_sec = mmean(h$attesa_sec))
})
O <- do.call(rbind, operator_records); rownames(O) <- NULL
op_changes <- c("gestite_valide", "quota_valide_su_totale_mese_pct", "durata_media_sec", "minuti_conversazione")
for (v in op_changes) {
  pp <- grepl("pct$", v); nm <- paste0("var_mensile_", v, if (pp) "_pp" else "_pct")
  O[[nm]] <- NA_real_
  for (op in unique(O$operatore)) {
    ix <- which(O$operatore == op)
    if (length(ix) < 2) next
    for (j in 2:length(ix)) {
      prev <- ix[j - 1]; curr <- ix[j]
      if (O$anno[curr] * 12 + O$mese_num[curr] - O$anno[prev] * 12 - O$mese_num[prev] != 1) next   # solo mesi consecutivi
      last <- O[[v]][prev]; new <- O[[v]][curr]
      O[[nm]][curr] <- if (pp) new - last else if (is.finite(last) && last != 0) 100 * (new / last - 1) else NA_real_
    }
  }
}
write.csv(O, file.path(out_dir, "KPI_operatori_mensili.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

# Qualita' del dato: quota di chiamate "servite" con conversazione a zero secondi.
# Un valore molto alto su un operatore suggerisce un problema di instradamento o
# di registrazione telefonica: i KPI individuali vanno letti con cautela.
Q <- do.call(rbind, lapply(split(sv, list(sv$anno, sv$operatore), drop = TRUE), function(g) {
  data.frame(anno = g$anno[1], operatore = g$operatore[1], servite = nrow(g),
             servite_zero_sec = sum(g$zero), pct_zero_sec = percentage(sum(g$zero), nrow(g)),
             servite_brevi = sum(g$breve), gestite_valide = sum(g$valida),
             pct_valide_su_servite = percentage(sum(g$valida), nrow(g)))
}))
Q <- Q[order(Q$anno, Q$operatore), ]; rownames(Q) <- NULL
write.csv(Q, file.path(out_dir, "Qualita_dati_operatori.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# CONFRONTI OMOGENEI: per mese di calendario e per fase organizzativa
# ------------------------------------------------------------------------------
cv <- c("chiamate_totali", "servite_registrate", "gestite_valide", "tasso_servite_pct", "tasso_gestite_valide_pct",
        "tasso_rifiutate_pct", "tasso_abbandonate_pct", "durata_media_sec", "attesa_media_sec", "minuti_conversazione",
        "chiamate_per_giorno_attivo", "gestite_valide_per_operatore_nominale")
comp_rows <- lapply(MESI_COMPARABILI, function(mm) {
  a <- C[C$anno == ANNO_1 & C$mese_num == mm, , drop = FALSE]; b <- C[C$anno == ANNO_2 & C$mese_num == mm, , drop = FALSE]
  rec <- data.frame(mese_num = mm, mese_nome = nomi_mesi[mm])
  for (v in cv) {
    rec[[paste0(v, "_", ANNO_1)]] <- a[[v]]; rec[[paste0(v, "_", ANNO_2)]] <- b[[v]]
    pp <- grepl("pct$", v)
    rec[[paste0(v, if (pp) "_var_pp" else "_var_pct")]] <- if (pp) b[[v]] - a[[v]] else if (a[[v]] != 0) 100 * (b[[v]] / a[[v]] - 1) else NA_real_
  }
  rec
})
YOY <- do.call(rbind, comp_rows)
write.csv(YOY, file.path(out_dir, "Confronto_mensile.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

period_function <- function(year, month_set, label) {
  g <- d[d$anno == year & d$mese_num %in% month_set, , drop = FALSE]; h <- g[g$valida, , drop = FALSE]; s <- g[g$servita, , drop = FALSE]
  data.frame(anno = year, periodo = label, mesi = length(month_set), ricevute = nrow(g), servite_registrate = nrow(s), gestite_valide = nrow(h),
    servite_zero_sec = sum(g$zero), servite_brevi = sum(g$breve), servite_outlier = sum(g$outlier),
    rifiutate = sum(g$classificazione == "Rifiutata"), abbandonate = sum(g$classificazione == "Abbandonate"),
    fuori_tempo = sum(g$classificazione == "Fuori tempo massimo"), fuori_servizio = sum(g$classificazione == "Fuori servizio"),
    tasso_servite_pct = percentage(nrow(s), nrow(g)), tasso_gestite_valide_pct = percentage(nrow(h), nrow(g)),
    quota_valide_su_servite_pct = percentage(nrow(h), nrow(s)),
    tasso_rifiutate_pct = percentage(sum(g$classificazione == "Rifiutata"), nrow(g)),
    tasso_abbandonate_pct = percentage(sum(g$classificazione == "Abbandonate"), nrow(g)),
    durata_media_sec = mmean(h$conversazione_sec), durata_mediana_sec = median0(h$conversazione_sec),
    attesa_media_sec = mmean(h$attesa_sec), minuti_conversazione = sum(h$conversazione_sec) / 60,
    media_gestite_valide_mensili = nrow(h) / length(month_set), media_ricevute_mensili = nrow(g) / length(month_set))
}
parts <- c(setNames(list(MESI_COMPARABILI), etichetta_mesi(MESI_COMPARABILI)), FASI)
P <- do.call(rbind, unlist(lapply(names(parts), function(n)
  lapply(c(ANNO_1, ANNO_2), function(y) period_function(y, parts[[n]], n))), recursive = FALSE))
write.csv(P, file.path(out_dir, "Confronto_periodi.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# AUDIT (opzionale): totali del riepilogo dell'esportazione contro le righe di dettaglio
# ------------------------------------------------------------------------------
AU <- NULL
if (!is.null(RIEPILOGO_ESTRAZIONI)) {
  AU <- list()
  for (y in c(ANNO_1, ANNO_2)) {
    ref <- RIEPILOGO_ESTRAZIONI[[as.character(y)]]
    if (is.null(ref)) next
    g <- d[d$anno == y, , drop = FALSE]
    counts <- c(totale = nrow(g), servite = sum(g$servita), fuori_servizio = sum(g$classificazione == "Fuori servizio"),
                rifiutate = sum(g$classificazione == "Rifiutata"), fuori_tempo = sum(g$classificazione == "Fuori tempo massimo"),
                abbandonate = sum(g$classificazione == "Abbandonate"))
    AU[[as.character(y)]] <- data.frame(anno = y, voce = names(counts), riepilogo_file = as.numeric(ref[names(counts)]),
                                        righe_di_dettaglio = as.numeric(counts),
                                        differenza = as.numeric(ref[names(counts)]) - as.numeric(counts))
  }
  AU <- do.call(rbind, AU)
  write.csv(AU, file.path(out_dir, "Audit_estrazioni.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

# ------------------------------------------------------------------------------
# SENSIBILITA': la differenza tra anni deve reggere a soglie diverse
# ------------------------------------------------------------------------------
S <- list()
for (minimum in c(15, 30, 60)) for (q in c(.99, .995, 1.0)) {
  ref_time <- d$conversazione_sec[d$comparabile & d$servita & d$conversazione_sec >= minimum]
  cap <- as.numeric(quantile(ref_time, q, na.rm = TRUE, type = 7))
  for (y in c(ANNO_1, ANNO_2)) {
    g <- d[d$anno == y & d$comparabile, , drop = FALSE]
    h <- g[g$servita & g$conversazione_sec >= minimum & g$conversazione_sec <= cap, , drop = FALSE]
    S[[length(S) + 1]] <- data.frame(minimo_sec = minimum, percentile_cap = q, cap_sec = cap, anno = y,
      ricevute = nrow(g), gestite_valide = nrow(h), tasso_valide_pct = percentage(nrow(h), nrow(g)),
      durata_media_sec = mmean(h$conversazione_sec))
  }
}
S <- do.call(rbind, S)
write.csv(S, file.path(out_dir, "Sensibilita_soglie.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# GRAFICI (R base): i due anni condividono stessa scala e stessi mesi di calendario
# ------------------------------------------------------------------------------
col25 <- "#24476C"; col26 <- "#168A88"; colG <- "#C48B3B"
MC <- MESI_COMPARABILI
month_names <- nomi_mesi[MC]
LEG_ANNI <- as.character(c(ANNO_1, ANNO_2))
v25 <- C[C$anno == ANNO_1, ]; v26 <- C[C$anno == ANNO_2, ]
v25 <- v25[match(MC, v25$mese_num), ]; v26 <- v26[match(MC, v26$mese_num), ]

make_png <- function(filename, expression, width = 1550, height = 750) {
  png(file.path(fig_dir, filename), width = width, height = height, res = 160, bg = "white")
  tryCatch(eval.parent(substitute(expression)), finally = dev.off())
}
style_plot <- function(main, ylab, ylim) {
  plot(NA, xlim = c(min(MC) - .2, max(MC) + .2), ylim = ylim, xaxt = "n", xlab = "Mese", ylab = ylab, main = main, bty = "n")
  axis(1, at = MC, labels = month_names); abline(h = pretty(ylim), col = "#EBEFF2", lty = 1)
}
leg_year <- function() legend("topright", legend = LEG_ANNI, col = c(col25, col26), pch = 16, lty = 1, bty = "n", horiz = TRUE)

make_png("00_Serie_storica_completa.png", {
  xx <- seq_len(nrow(M))
  lim <- max(M$chiamate_totali, na.rm = TRUE) * 1.17
  plot(xx, M$chiamate_totali, type = "b", pch = 16, lwd = 2.8, col = col25, xaxt = "n", xlab = "Mese", ylab = "Chiamate",
       ylim = c(0, lim), bty = "n", main = "Serie completa: mesi non omogenei segnalati")
  axis(1, at = seq(1, nrow(M), 2), labels = gsub("^20", "'", M$mese[seq(1, nrow(M), 2)]), cex.axis = .8)
  abline(h = pretty(c(0, lim)), col = "#EBEFF2")
  lines(xx, M$gestite_valide, type = "b", pch = 16, lwd = 2.8, col = col26)
  if (any(M$anno == ANNO_1)) abline(v = max(which(M$anno == ANNO_1)) + .5, col = colG, lty = 3, lwd = 2)   # cambio d'anno
  ii <- which(M$parziale == 1)
  points(xx[ii], M$chiamate_totali[ii], pch = 21, cex = 1.7, col = colG, bg = "white", lwd = 2)
  points(xx[ii], M$gestite_valide[ii], pch = 21, cex = 1.7, col = colG, bg = "white", lwd = 2)
  legend("topright", legend = c("Ricevute", "Gestite valide", "Mesi parziali"), col = c(col25, col26, colG),
         pch = c(16, 16, 21), lty = c(1, 1, NA), bty = "n", cex = .85)
})
make_png("01_Volumi_ricevuti.png", {
  z <- rbind(v25$chiamate_totali, v26$chiamate_totali)
  barplot(z, beside = TRUE, names.arg = month_names, col = c(col25, col26), border = NA,
          ylim = c(0, max(z, na.rm = TRUE) * 1.17), ylab = "Chiamate ricevute", main = "Volume mensile - mesi comparabili")
  legend("topright", legend = LEG_ANNI, fill = c(col25, col26), bty = "n", horiz = TRUE)
})
make_png("02_Gestite_valide_numero.png", {
  z <- rbind(v25$gestite_valide, v26$gestite_valide)
  barplot(z, beside = TRUE, names.arg = month_names, col = c(col25, col26), border = NA,
          ylim = c(0, max(z, na.rm = TRUE) * 1.17), ylab = "Conversazioni valide",
          main = paste0("Chiamate con conversazione significativa (", MIN_SECONDI, " s - cap)"))
  legend("topright", legend = LEG_ANNI, fill = c(col25, col26), bty = "n", horiz = TRUE)
})
make_png("03_Tasso_servite_e_valide.png", {
  style_plot("Tasso servite registrate vs conversazioni valide", "% delle ricevute", c(0, 90))
  lines(MC, v25$tasso_servite_pct, col = col25, lty = 2, lwd = 2.6)
  lines(MC, v26$tasso_servite_pct, col = col26, lty = 2, lwd = 2.6)
  lines(MC, v25$tasso_gestite_valide_pct, col = col25, lty = 1, lwd = 3, type = "b", pch = 16)
  lines(MC, v26$tasso_gestite_valide_pct, col = col26, lty = 1, lwd = 3, type = "b", pch = 16)
  legend("topleft", legend = c(paste(ANNO_1, "valide"), paste(ANNO_2, "valide"), paste(ANNO_1, "servite"), paste(ANNO_2, "servite")),
         col = c(col25, col26, col25, col26), lty = c(1, 1, 2, 2), bty = "n", horiz = TRUE, cex = .83)
})

# Operatori mostrati nei grafici: i piu' attivi per conversazioni valide
vv <- d[d$valida & nzchar(d$operatore), , drop = FALSE]
operatori <- head(names(sort(table(vv$operatore), decreasing = TRUE)), N_OPERATORI_GRAFICI)
op_cols <- c(col25, col26, colG, "#7A5C99", "#B5524F", "#5E7F3E")[seq_along(operatori)]
# Legenda sovrapposta alla figura intera (utile per grafici a piu' pannelli)
leg_over <- function(pos, ...) {
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot.new()
  legend(pos, ...)
}
# Legenda in testa alla figura (sopra i riquadri), valida per grafici a piu' pannelli
leg_fig <- function(testo, colori) {
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot.new()
  legend("top", legend = testo, fill = colori, bty = "n", horiz = TRUE, cex = .9)
}
op_val <- function(year) {
  matrix(sapply(MC, function(mm) sapply(operatori, function(op) {
    z <- O$gestite_valide[O$anno == year & O$mese_num == mm & O$operatore == op]
    if (length(z)) z[1] else 0
  })), nrow = length(operatori))
}
if (length(operatori) > 0) {
  make_png("04_Volumi_per_operatore.png", {
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 1), oma = c(1, 1, 3, 1))
    lim <- max(colSums(op_val(ANNO_1)), colSums(op_val(ANNO_2))) * 1.2
    for (y in c(ANNO_1, ANNO_2)) {
      barplot(op_val(y), beside = FALSE, names.arg = month_names, col = op_cols, border = NA, ylim = c(0, lim),
              ylab = "Gestite valide", main = paste("Ripartizione mensile per operatore -", y))
    }
    leg_fig(operatori, op_cols)
  }, height = 1100)
  make_png("05_Quota_per_operatore.png", {
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 1), oma = c(1, 1, 3, 1))
    for (y in c(ANNO_1, ANNO_2)) {
      z <- op_val(y); z <- sweep(z, 2, colSums(z), "/") * 100
      barplot(z, beside = FALSE, names.arg = month_names, col = op_cols, border = NA, ylim = c(0, 105),
              ylab = "% sul totale gestito", main = paste("Quota mensile conversazioni valide -", y))
    }
    leg_fig(operatori, op_cols)
  }, height = 1100)
}
make_png("06_Durata_media.png", {
  ylim <- range(c(v25$durata_media_sec, v26$durata_media_sec, v25$durata_mediana_sec, v26$durata_mediana_sec), na.rm = TRUE) / 60
  style_plot("Durata della conversazione valida - media e mediana", "Minuti", c(0, max(ylim) * 1.28))
  lines(MC, v25$durata_media_sec / 60, col = col25, type = "b", pch = 16, lwd = 2.8)
  lines(MC, v26$durata_media_sec / 60, col = col26, type = "b", pch = 16, lwd = 2.8)
  lines(MC, v25$durata_mediana_sec / 60, col = col25, lty = 3, lwd = 2)
  lines(MC, v26$durata_mediana_sec / 60, col = col26, lty = 3, lwd = 2)
  legend("topleft", legend = c(paste(ANNO_1, "media"), paste(ANNO_2, "media"), paste(ANNO_1, "mediana"), paste(ANNO_2, "mediana")),
         col = c(col25, col26, col25, col26), lty = c(1, 1, 3, 3), bty = "n", horiz = TRUE, cex = .82)
})
if (length(operatori) > 0) {
  make_png("07_Durata_media_operatori.png", {
    par(mfrow = c(length(operatori), 1), mar = c(3, 5, 3, 2), oma = c(2, 0, 1, 0))
    maxdur <- max(O$durata_media_sec, na.rm = TRUE) / 60 * 1.18
    for (op in operatori) {
      plot(NA, xlim = range(MC), ylim = c(0, maxdur), xaxt = "n", xlab = "", ylab = "Minuti", bty = "n", main = op)
      axis(1, at = MC, labels = month_names); abline(h = pretty(c(0, maxdur)), col = "#EBEFF2")
      for (y in c(ANNO_1, ANNO_2)) {
        z <- rep(NA_real_, length(MC))
        for (i in seq_along(MC)) {
          v <- O$durata_media_sec[O$anno == y & O$mese_num == MC[i] & O$operatore == op]
          if (length(v)) z[i] <- v[1] / 60
        }
        lines(MC, z, col = if (y == ANNO_1) col25 else col26, type = "b", pch = 16, lwd = 2)
      }
    }
    legend("topright", legend = LEG_ANNI, col = c(col25, col26), lty = 1, pch = 16, bty = "n", horiz = TRUE)
  }, height = 450 * max(length(operatori), 1) + 150)
}
make_png("08_Rifiuti_abbandoni.png", {
  par(mfrow = c(2, 1), mar = c(4, 5, 3, 2))
  for (v in c("tasso_rifiutate_pct", "tasso_abbandonate_pct")) {
    lim <- max(c(v25[[v]], v26[[v]]), na.rm = TRUE) * 1.25
    plot(MC, v25[[v]], type = "b", pch = 16, lwd = 2.8, col = col25, xaxt = "n", xlab = "", ylab = "% delle chiamate",
         ylim = c(0, lim), bty = "n", main = if (v == "tasso_rifiutate_pct") "Chiamate rifiutate" else "Chiamate abbandonate")
    axis(1, at = MC, labels = month_names); abline(h = pretty(c(0, lim)), col = "#EBEFF2")
    lines(MC, v26[[v]], col = col26, type = "b", pch = 16, lwd = 2.8)
  }
  legend("topright", legend = LEG_ANNI, col = c(col25, col26), lty = 1, pch = 16, bty = "n", horiz = TRUE)
}, height = 1050)
make_png("09_Attesa_media.png", {
  style_plot("Attesa media delle conversazioni valide", "Secondi", c(0, max(v25$attesa_media_sec, v26$attesa_media_sec, na.rm = TRUE) * 1.3))
  lines(MC, v25$attesa_media_sec, col = col25, type = "b", pch = 16, lwd = 2.8)
  lines(MC, v26$attesa_media_sec, col = col26, type = "b", pch = 16, lwd = 2.8)
  leg_year()
})
make_png("10_Minuti_conversazione.png", {
  z <- rbind(v25$minuti_conversazione, v26$minuti_conversazione)
  barplot(z, beside = TRUE, names.arg = month_names, col = c(col25, col26), border = NA,
          ylim = c(0, max(z, na.rm = TRUE) * 1.16), ylab = "Minuti totali validi", main = "Carico documentato in minuti di conversazione")
  legend("topright", legend = LEG_ANNI, fill = c(col25, col26), bty = "n", horiz = TRUE)
})
make_png("11_Sensibilita.png", {
  ss <- S[S$percentile_cap == QUANTILE_OUTLIER, , drop = FALSE]
  z <- rbind(ss$tasso_valide_pct[ss$anno == ANNO_1], ss$tasso_valide_pct[ss$anno == ANNO_2])
  barplot(z, beside = TRUE, names.arg = paste(c(15, 30, 60), "secondi"), col = c(col25, col26), border = NA,
          ylim = c(0, max(z, na.rm = TRUE) * 1.2), ylab = "% gestite valide / ricevute", main = "Sensibilita' alla durata minima")
  legend("topright", legend = LEG_ANNI, fill = c(col25, col26), bty = "n", horiz = TRUE)
})

# ------------------------------------------------------------------------------
# SCENARI DI STERILIZZAZIONE S0-S6
# Stessi mesi comparabili, stesso cap per tutti gli scenari: cambia un solo
# criterio per volta, cosi' l'effetto di ogni filtro e' isolato.
# ------------------------------------------------------------------------------
SCENARI <- c(
  S0 = "Servite registrate (comprese le durate a 0 s)",
  S1 = "Servite con durata > 0 s",
  S2 = paste0("Servite con durata >= ", MIN_SECONDI, " s"),
  S3 = paste0(">= ", MIN_SECONDI, " s e <= P", QUANTILE_OUTLIER * 100, " (criterio dei KPI principali)"),
  S4 = paste0(">= ", MIN_SECONDI, " s, esclusi gli outlier di Tukey mensili"),
  S5 = paste0(">= ", MIN_SCENARIO_BASSO, " s e <= P", QUANTILE_OUTLIER * 100),
  S6 = paste0(">= ", MIN_SCENARIO_ALTO, " s e <= P", QUANTILE_OUTLIER * 100))

mask_scenario <- function(g, sc) {
  s <- g$servita & !is.na(g$conversazione_sec)
  x <- g$conversazione_sec
  switch(sc,
    S0 = s,
    S1 = s & x > 0,
    S2 = s & x >= MIN_SECONDI,
    S3 = s & x >= MIN_SECONDI & x <= CAP_P995,
    S4 = s & x >= MIN_SECONDI & !g$flag_tukey_mensile,
    S5 = s & x >= MIN_SCENARIO_BASSO & x <= CAP_P995,
    S6 = s & x >= MIN_SCENARIO_ALTO & x <= CAP_P995)
}

cmp <- d[d$comparabile & d$anno %in% c(ANNO_1, ANNO_2), , drop = FALSE]
sc_rows <- list()
for (sc in names(SCENARI)) for (y in c(ANNO_1, ANNO_2)) {
  g <- cmp[cmp$anno == y, , drop = FALSE]
  h <- g[mask_scenario(g, sc), , drop = FALSE]
  sc_rows[[length(sc_rows) + 1]] <- data.frame(scenario = sc, descrizione = SCENARI[[sc]], anno = y,
    ricevute = nrow(g), gestite = nrow(h), tasso_pct = percentage(nrow(h), nrow(g)),
    durata_media_sec = mmean(h$conversazione_sec), durata_mediana_sec = median0(h$conversazione_sec),
    minuti_conversazione = sum(h$conversazione_sec) / 60)
}
SC <- do.call(rbind, sc_rows)
write.csv(SC, file.path(out_dir, "Scenari_annuali.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

SCW <- do.call(rbind, lapply(names(SCENARI), function(sc) {
  a <- SC[SC$scenario == sc & SC$anno == ANNO_1, ]; b <- SC[SC$scenario == sc & SC$anno == ANNO_2, ]
  r <- data.frame(scenario = sc, descrizione = SCENARI[[sc]],
    gestite_1 = a$gestite, gestite_2 = b$gestite, var_gestite_pct = 100 * (b$gestite / a$gestite - 1),
    tasso_1 = a$tasso_pct, tasso_2 = b$tasso_pct, var_tasso_pp = b$tasso_pct - a$tasso_pct,
    media_1 = a$durata_media_sec, media_2 = b$durata_media_sec,
    var_media_pct = 100 * (b$durata_media_sec / a$durata_media_sec - 1))
  r
}))
names(SCW) <- sub("_1$", paste0("_", ANNO_1), sub("_2$", paste0("_", ANNO_2), names(SCW)))
write.csv(SCW, file.path(out_dir, "Scenari_confronto.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

scm_rows <- list()
for (sc in names(SCENARI)) for (y in c(ANNO_1, ANNO_2)) for (mm in MESI_COMPARABILI) {
  g <- d[d$anno == y & d$mese_num == mm, , drop = FALSE]
  h <- g[mask_scenario(g, sc), , drop = FALSE]
  scm_rows[[length(scm_rows) + 1]] <- data.frame(scenario = sc, anno = y, mese_num = mm, ricevute = nrow(g),
    gestite = nrow(h), tasso_pct = percentage(nrow(h), nrow(g)), durata_media_sec = mmean(h$conversazione_sec))
}
SCM <- do.call(rbind, scm_rows)
write.csv(SCM, file.path(out_dir, "Scenari_mensili.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

# Scomposizione delle servite: le classi sono mutuamente esclusive e sommano alle servite registrate
DEC <- do.call(rbind, lapply(c(ANNO_1, ANNO_2), function(y) {
  g <- cmp[cmp$anno == y & cmp$servita & !is.na(cmp$conversazione_sec), , drop = FALSE]; x <- g$conversazione_sec
  data.frame(anno = y, servite = nrow(g), durata_zero = sum(x == 0),
    da_1_sec_a_soglia = sum(x > 0 & x < MIN_SECONDI), da_soglia_a_cap = sum(x >= MIN_SECONDI & x <= CAP_P995),
    oltre_cap = sum(x > CAP_P995))
}))
write.csv(DEC, file.path(out_dir, "Scomposizione_servite.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

# KPI annuali per operatore sul periodo comparabile
OA <- do.call(rbind, lapply(c(ANNO_1, ANNO_2), function(y) {
  g <- cmp[cmp$anno == y & cmp$servita & nzchar(cmp$operatore), , drop = FALSE]
  tot <- sum(g$valida)
  do.call(rbind, lapply(split(g, g$operatore), function(z) data.frame(anno = y, operatore = z$operatore[1],
    servite = nrow(z), servite_zero_sec = sum(z$zero), gestite_valide = sum(z$valida),
    durata_media_valide_sec = mmean(z$conversazione_sec[z$valida]), quota_valide_pct = percentage(sum(z$valida), tot))))
}))
rownames(OA) <- NULL
write.csv(OA, file.path(out_dir, "KPI_operatori_annuali.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# BOXPLOT, OUTLIER E TRACCIABILITA'
# - P99,5 (cap unico dei due anni): esclusione effettiva dalle gestite valide;
# - Tukey (1,5 x IQR): SOLO segnalazione diagnostica, calcolata per mese sulle
#   servite >= soglia minima prima di qualsiasi filtro superiore.
# Un outlier statistico non e' un errore accertato.
# ------------------------------------------------------------------------------
format_sec <- function(v) {
  v <- as.integer(round(v))
  ifelse(is.na(v), NA_character_, sprintf("%02d:%02d:%02d", v %/% 3600L, (v %% 3600L) %/% 60L, v %% 60L))
}
d$flag_p995 <- eligible & d$conversazione_sec > CAP_P995
d$flag_tukey_globale <- eligible & d$conversazione_sec > CAP_TUKEY
d$durata_hms <- format_sec(d$conversazione_sec)
d$scostamento_p995_sec <- ifelse(d$flag_p995, d$conversazione_sec - CAP_P995, NA_real_)

registry_cols <- intersect(c("id_chiamata", "fonte", "riga_origine", "timestamp", "mese", "operatore", "classificazione",
  "causa_terminazione", "conversazione_sec", "durata_hms", "attesa_sec", "operatori_loggati", "comparabile",
  "flag_p995", "flag_tukey_mensile", "flag_tukey_globale", "q1_mese", "q3_mese", "tukey_sup_mese",
  "scostamento_p995_sec"), names(d))
register <- function(z, filename) {
  z <- z[order(z$timestamp), registry_cols, drop = FALSE]
  write.csv(z, file.path(out_dir, filename), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
# I registri contengono ID e riga di origine per la verifica, MAI numeri di telefono
register(d[d$flag_p995, , drop = FALSE], "Registro_outlier_P995.csv")
register(d[d$flag_tukey_mensile | d$flag_p995, , drop = FALSE], "Registro_anomalie.csv")

get_dur <- function(g) g$conversazione_sec[g$servita & !is.na(g$conversazione_sec) & g$conversazione_sec >= MIN_SECONDI]
calc_q <- function(z, p) if (length(z)) as.numeric(quantile(z, p, na.rm = TRUE, type = 7)) else NA_real_
collect_box <- function(g, mes, op = NA_character_) {
  v <- get_dur(g)
  if (!length(v)) return(NULL)
  q1 <- calc_q(v, .25); q2 <- calc_q(v, .50); q3 <- calc_q(v, .75)
  v995 <- v[v <= CAP_P995]
  data.frame(mese = mes, anno = as.integer(substr(mes, 1, 4)), mese_num = as.integer(substr(mes, 6, 7)), operatore = op,
    n_almeno_soglia = length(v), n_outlier_p995 = sum(v > CAP_P995),
    n_segnalazioni_tukey_mensile = sum(g$flag_tukey_mensile), n_segnalazioni_tukey_globale = sum(v > CAP_TUKEY),
    q1_sec = q1, mediana_sec = q2, q3_sec = q3, iqr_sec = q3 - q1, limite_tukey_mensile_sec = q3 + 1.5 * (q3 - q1),
    limite_tukey_globale_sec = CAP_TUKEY, limite_p995_sec = CAP_P995,
    durata_media_non_filtrata_sec = mmean(v), durata_media_p995_sec = mmean(v995),
    delta_media_p995_sec = mmean(v) - mmean(v995))
}
B <- do.call(rbind, lapply(split(d, d$mese), function(g) collect_box(g, g$mese[1])))
B <- B[order(B$mese), ]; rownames(B) <- NULL
write.csv(B, file.path(out_dir, "Boxplot_e_outlier_mensili.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
sg <- d[d$servita & nzchar(d$operatore), , drop = FALSE]
BO <- do.call(rbind, lapply(split(sg, interaction(sg$mese, sg$operatore, drop = TRUE, lex.order = TRUE)),
                            function(g) collect_box(g, g$mese[1], g$operatore[1])))
BO <- BO[order(BO$mese, BO$operatore), ]; rownames(BO) <- NULL
write.csv(BO, file.path(out_dir, "Boxplot_e_outlier_operatori_mensili.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

# Grafici degli scenari
make_png("12_Scenari_numerosita.png", {
  par(mar = c(4.5, 4, 3, 1))
  z <- rbind(SC$gestite[SC$anno == ANNO_1], SC$gestite[SC$anno == ANNO_2])
  barplot(z, beside = TRUE, horiz = TRUE, names.arg = names(SCENARI), las = 1, col = c(col25, col26), border = NA,
          xlim = c(0, max(z) * 1.1), xlab = "Chiamate considerate gestite",
          main = "Quanto cambia il volume delle gestite con ciascun criterio")
  legend("bottomright", legend = LEG_ANNI, fill = c(col25, col26), bty = "n")
})
make_png("13_Scenari_durata_media.png", {
  par(mar = c(4.5, 4, 3, 5))
  z <- rbind(SC$durata_media_sec[SC$anno == ANNO_1], SC$durata_media_sec[SC$anno == ANNO_2]) / 60
  b <- barplot(z, beside = TRUE, horiz = TRUE, names.arg = names(SCENARI), las = 1, col = c(col25, col26), border = NA,
               xlim = c(0, max(z, na.rm = TRUE) * 1.25), xlab = "Durata media (minuti)",
               main = "Effetto dei criteri sulla durata media")
  text(pmax(z[1, ], z[2, ]) + .05, colMeans(b), labels = sprintf("%+.1f%%", SCW$var_media_pct), pos = 4, cex = .8, xpd = NA)
  legend("bottomright", legend = LEG_ANNI, fill = c(col25, col26), bty = "n")
})
make_png("14_Scenari_serie_mensile.png", {
  par(mfrow = c(2, 1), mar = c(4, 5, 3, 1))
  sc_col <- c(S0 = "#8A99A8", S2 = colG, S3 = col25, S4 = "#B5524F")
  ymax <- max(SCM$durata_media_sec[SCM$scenario %in% names(sc_col)], na.rm = TRUE) / 60 * 1.15
  for (y in c(ANNO_1, ANNO_2)) {
    plot(NA, xlim = range(MC), ylim = c(0, ymax), xaxt = "n", xlab = "", ylab = "Minuti", bty = "n",
         main = paste("Durata media mensile per scenario -", y))
    axis(1, at = MC, labels = month_names); abline(h = pretty(c(0, ymax)), col = "#EBEFF2")
    for (sc in names(sc_col)) {
      a <- SCM[SCM$scenario == sc & SCM$anno == y, ]; a <- a[match(MC, a$mese_num), ]
      lines(MC, a$durata_media_sec / 60, type = "b", pch = 16, lwd = 2, col = sc_col[[sc]])
    }
  }
  leg_over("bottom", legend = names(sc_col), col = sc_col, lty = 1, pch = 16, horiz = TRUE, bty = "n")
}, height = 1100)

# Grafici boxplot
make_png("15_Boxplot_mensili_grezzi.png", {
  par(mfrow = c(2, 1), mar = c(4.2, 5, 3.2, 1.5), oma = c(2.5, 0, 0, 0))
  for (yy in c(ANNO_1, ANNO_2)) {
    z <- lapply(MC, function(mm) get_dur(d[d$anno == yy & d$mese_num == mm, , drop = FALSE]) / 60)
    names(z) <- month_names
    boxplot(z, log = "y", ylim = c(.45, max(unlist(z), 5, na.rm = TRUE) * 1.3), outline = FALSE, ylab = "Minuti (scala log)",
            main = paste("Distribuzione grezza delle conversazioni -", yy),
            col = if (yy == ANNO_1) "#C8D6E5" else "#B9DCD8", border = col25)
    abline(h = CAP_P995 / 60, col = "#D65A4A", lty = 2, lwd = 2)
    abline(h = CAP_TUKEY / 60, col = colG, lty = 3, lwd = 2)
    for (j in seq_along(MC)) {
      g <- d[d$anno == yy & d$mese_num == MC[j] & d$flag_p995, , drop = FALSE]
      if (nrow(g)) points(rep(j, nrow(g)), g$conversazione_sec / 60, pch = 18, col = "#D65A4A", cex = 1.1)
    }
  }
  leg_over("bottom", legend = c("P99,5: esclusione", "Tukey globale: segnalazione", "Casi esclusi"),
           col = c("#D65A4A", colG, "#D65A4A"), lty = c(2, 3, NA), pch = c(NA, NA, 18), horiz = TRUE, bty = "n", cex = .85)
}, height = 1150)
make_png("16_Boxplot_mensili_sterilizzati.png", {
  combos <- list(); coords <- numeric(0); colors <- character(0)
  for (i in seq_along(MC)) for (yy in c(ANNO_1, ANNO_2)) {
    g <- d[d$anno == yy & d$mese_num == MC[i], , drop = FALSE]
    combos[[length(combos) + 1]] <- g$conversazione_sec[mask_scenario(g, "S3")] / 60
    coords <- c(coords, MC[i] + if (yy == ANNO_1) -.18 else .18)
    colors <- c(colors, if (yy == ANNO_1) col25 else col26)
  }
  boxplot(combos, at = coords, boxwex = .29, col = colors, border = colors, outline = FALSE, xaxt = "n",
          xlim = c(min(MC) - .5, max(MC) + .5), ylim = c(0, 16), ylab = "Minuti", main = "Boxplot delle conversazioni valide (scenario S3)")
  axis(1, at = MC, labels = month_names)
  points(coords, vapply(combos, function(x) mmean(x), numeric(1)), pch = 18, cex = 1.2, col = "#283745")
  legend("topright", legend = c(LEG_ANNI, "Media"), fill = c(col25, col26, NA), border = NA, pch = c(NA, NA, 18), bty = "n", horiz = TRUE)
})
if (length(operatori) > 0) {
  make_png("17_Boxplot_operatori.png", {
    par(mfrow = c(1, length(operatori)), mar = c(5.2, 4.6, 3.8, 1.2), oma = c(0, 0, 1, 0))
    for (op in operatori) {
      z <- lapply(c(ANNO_1, ANNO_2), function(yy)
        get_dur(d[d$anno == yy & d$comparabile & d$operatore == op, , drop = FALSE]) / 60)
      boxplot(z, names = LEG_ANNI, log = "y", ylim = c(.45, max(unlist(z), 5, na.rm = TRUE) * 1.3), outline = FALSE,
              col = c("#C8D6E5", "#B9DCD8"), border = col25, main = op, ylab = "Minuti (log)")
      abline(h = CAP_P995 / 60, col = "#D65A4A", lty = 2, lwd = 2)
      for (j in 1:2) {
        g <- d[d$anno == c(ANNO_1, ANNO_2)[j] & d$comparabile & d$operatore == op & d$flag_p995, , drop = FALSE]
        if (nrow(g)) points(rep(j, nrow(g)), g$conversazione_sec / 60, pch = 18, col = "#D65A4A")
      }
    }
  }, width = 600 * max(length(operatori), 2) + 200, height = 700)
}
make_png("18_Outlier_per_mese.png", {
  par(mfrow = c(2, 1), mar = c(4, 5, 3, 1))
  for (colname in c("n_outlier_p995", "n_segnalazioni_tukey_mensile")) {
    x <- sapply(c(ANNO_1, ANNO_2), function(yy) sapply(MC, function(mm) {
      z <- B[B$anno == yy & B$mese_num == mm, colname]; if (length(z)) z[1] else 0 }))
    barplot(t(x), beside = TRUE, names.arg = month_names, col = c(col25, col26), border = NA, ylab = "Numero di chiamate",
            main = if (colname == "n_outlier_p995") "Esclusi P99,5" else "Segnalati Tukey mensile")
  }
  legend("topright", legend = LEG_ANNI, fill = c(col25, col26), bty = "n", horiz = TRUE)
}, height = 1050)

# ------------------------------------------------------------------------------
# APPROFONDIMENTO: CHIAMATE LUNGHE ESCLUSE DAL CAP (solo conversazioni > P99,5)
# Descrive peso e distribuzione degli eventi esclusi; non e' un indicatore di
# produttivita'. Il registro ha campi vuoti per annotare l'esito delle verifiche.
# ------------------------------------------------------------------------------
d$S2 <- d$servita & !is.na(d$conversazione_sec) & d$conversazione_sec >= MIN_SECONDI
d$lunghe <- d$S2 & d$conversazione_sec > CAP_P995
l <- d[d$lunghe, , drop = FALSE]
l <- l[order(l$timestamp, l$id_chiamata), , drop = FALSE]
l$durata_minuti <- l$conversazione_sec / 60
l$superamento_soglia_sec <- l$conversazione_sec - CAP_P995
l$stato_verifica <- "Da verificare"
l$motivazione_accertata <- ""
l$note_verifica <- ""
l$ticket <- ""
long_cols <- intersect(c("id_chiamata", "fonte", "riga_origine", "timestamp", "anno", "mese", "comparabile", "operatore",
  "classificazione", "attesa_sec", "conversazione_sec", "durata_minuti", "superamento_soglia_sec", "operatori_loggati",
  "stato_verifica", "motivazione_accertata", "note_verifica", "ticket"), names(l))
write.csv(l[, long_cols, drop = FALSE], file.path(out_dir, "Dataset_ridotto_sole_lunghe.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

agg_lunghe <- function(g) {
  n <- nrow(g)
  c(n_lunghe = n, minuti_esclusi = sum(g$conversazione_sec) / 60,
    media_minuti = if (n) mean(g$conversazione_sec) / 60 else NA_real_,
    mediana_minuti = if (n) median(g$conversazione_sec) / 60 else NA_real_,
    massimo_minuti = if (n) max(g$conversazione_sec) / 60 else NA_real_,
    attesa_media_sec = if (n) mean(g$attesa_sec, na.rm = TRUE) else NA_real_)
}
lm_rows <- list()
for (y in sort(unique(d$anno))) for (mo in sort(unique(d$mese[d$anno == y]))) {
  a <- d[d$mese == mo, , drop = FALSE]; b <- a[a$lunghe, , drop = FALSE]; s2 <- a[a$S2, , drop = FALSE]
  lm_rows[[length(lm_rows) + 1L]] <- data.frame(anno = y, mese = mo, confrontabile = (as.integer(substr(mo, 6, 7)) %in% MESI_COMPARABILI),
    ricevute = nrow(a), servite_almeno_soglia = nrow(s2), t(agg_lunghe(b)),
    pct_chiamate = percentage(nrow(b), nrow(s2)), pct_minuti = percentage(sum(b$conversazione_sec), sum(s2$conversazione_sec)))
}
write.csv(do.call(rbind, lm_rows), file.path(out_dir, "KPI_mensili_lunghe.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

la_rows <- list(); lo_rows <- list()
for (scope in c("Comparabile", "Disponibile")) for (y in sort(unique(d$anno))) {
  mask <- d$anno == y & (if (scope == "Comparabile") d$comparabile else rep(TRUE, nrow(d)))
  a <- d[mask, , drop = FALSE]; b <- a[a$lunghe, , drop = FALSE]; s2 <- a[a$S2, , drop = FALSE]
  la_rows[[length(la_rows) + 1L]] <- data.frame(perimetro = scope, anno = y, t(agg_lunghe(b)), servite_almeno_soglia = nrow(s2),
    pct_eventi = percentage(nrow(b), nrow(s2)), pct_minuti = percentage(sum(b$conversazione_sec), sum(s2$conversazione_sec)),
    media_con_lunghe_sec = mmean(s2$conversazione_sec), media_senza_lunghe_sec = mmean(s2$conversazione_sec[s2$conversazione_sec <= CAP_P995]))
  for (op in sort(unique(s2$operatore[nzchar(s2$operatore)]))) {
    z <- s2[s2$operatore == op, , drop = FALSE]; zb <- z[z$lunghe, , drop = FALSE]
    lo_rows[[length(lo_rows) + 1L]] <- data.frame(perimetro = scope, anno = y, operatore = op, servite_almeno_soglia = nrow(z),
      t(agg_lunghe(zb)), frequenza_per_1000 = 1000 * nrow(zb) / nrow(z),
      pct_minuti = percentage(sum(zb$conversazione_sec), sum(z$conversazione_sec)))
  }
}
write.csv(do.call(rbind, la_rows), file.path(out_dir, "KPI_annuali_lunghe.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
write.csv(do.call(rbind, lo_rows), file.path(out_dir, "KPI_operatori_lunghe.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
stopifnot(all(l$conversazione_sec > CAP_P995), !anyDuplicated(l$id_chiamata))

make_png("19_Lunghe_mensile.png", {
  mat <- sapply(c(ANNO_1, ANNO_2), function(y) sapply(MC, function(m) sum(l$mese == sprintf("%d-%02d", y, m))))
  barplot(t(mat), beside = TRUE, col = c(col25, col26), border = NA, names.arg = month_names, ylab = "Numero chiamate lunghe",
          ylim = c(0, max(mat, 1) * 1.2), main = paste0("Eventi > P", QUANTILE_OUTLIER * 100, " - solo mesi comparabili"))
  legend("topright", legend = LEG_ANNI, fill = c(col25, col26), bty = "n", horiz = TRUE)
})
make_png("20_Lunghe_boxplot.png", {
  z <- lapply(c(ANNO_1, ANNO_2), function(y) l$durata_minuti[l$anno == y & l$comparabile])
  boxplot(z, names = paste("Periodo comparabile", LEG_ANNI), col = c(col25, col26), border = "#283745",
          ylab = "Minuti", main = "Durata delle chiamate lunghe escluse")
  abline(h = CAP_P995 / 60, lty = 2, col = "#D65A4A", lwd = 2)
}, width = 1300)
make_png("21_Lunghe_cronologia.png", {
  xx <- as.POSIXct(l$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Rome")
  plot(xx, l$durata_minuti, pch = 19, col = ifelse(l$anno == ANNO_1, col25, col26), xlab = "Data registrata", ylab = "Minuti",
       main = paste("Distribuzione cronologica:", nrow(l), "chiamate lunghe"), bty = "n")
  abline(h = CAP_P995 / 60, col = "#D65A4A", lty = 2)
  legend("topright", legend = LEG_ANNI, col = c(col25, col26), pch = 19, bty = "n")
})

# ------------------------------------------------------------------------------
# NOTE METODOLOGICHE
# ------------------------------------------------------------------------------
prima <- tapply(d$timestamp, d$anno, min); ultima <- tapply(d$timestamp, d$anno, max)
anomali <- unique(Q$operatore[!is.na(Q$pct_zero_sec) & Q$pct_zero_sec > SOGLIA_ZERO_ANOMALIA & Q$servite >= 50])
notes <- c(
  "HELP DESK - ISTRUZIONI, DEFINIZIONI E LIMITI",
  paste0("Data elaborazione: ", Sys.Date()),
  "FONTE: esportazioni del centralino, una riga per chiamata, due anni a confronto.",
  paste0("COPERTURA ", ANNO_1, ": dal ", prima[as.character(ANNO_1)], " al ", ultima[as.character(ANNO_1)]),
  paste0("COPERTURA ", ANNO_2, ": dal ", prima[as.character(ANNO_2)], " al ", ultima[as.character(ANNO_2)]),
  paste0("CONFRONTO PRINCIPALE: mesi ", etichetta_mesi(MC), " di entrambi gli anni; i mesi parziali (",
         paste(MESI_PARZIALI, collapse = ", "), ") restano fuori dal confronto."),
  paste0("GESTITA VALIDA: classificazione Servite e Trasferite, durata >= ", MIN_SECONDI, " sec e <= ",
         round(CAP_OUTLIER, 2), " sec (metodo ", METODO_OUTLIER, ", soglia globale sul periodo comparabile)."),
  "Le chiamate molto brevi e gli outlier sono esclusi dai KPI delle gestite, NON dal numero totale delle ricevute.",
  "TASSO SERVITE: chiamate classificate Servite e Trasferite / chiamate ricevute.",
  "TASSO GESTITE VALIDE: gestite valide / chiamate ricevute. E' un proxy operativo, NON una misura di problemi risolti.",
  "DURATA MEDIA e MEDIANA: solo gestite valide, medie ponderate per chiamata.",
  "VARIAZIONI: per i volumi e le durate variazione %; per i tassi variazione in punti percentuali.",
  "OPERATORI NOMINALI: da configurazione (ORGANICO_NOMINALE). Non e' una misura di presenza o ore lavorate.",
  if (ANONIMIZZA_OPERATORI) "OPERATORI: i nomi sono sostituiti da etichette generiche (Operatore A, B, ...) in ordine di volume di chiamate servite." else NULL,
  if (length(anomali)) paste0("ATTENZIONE: per ", paste(anomali, collapse = ", "), " oltre il ", SOGLIA_ZERO_ANOMALIA,
                              "% delle chiamate servite ha durata zero (vedi Qualita_dati_operatori.csv). ",
                              "I KPI 'gestite valide' non sono una valutazione individuale affidabile finche' non si verifica la registrazione telefonica.") else NULL,
  if (!is.null(AU)) "AUDIT: confronto tra riepilogo e righe di dettaglio in Audit_estrazioni.csv." else NULL,
  "I dati non consentono di calcolare ore presidiate, FTE effettivi, risoluzione al primo contatto, ticket risolti e qualita' degli esiti.",
  "Le variazioni sono descrittive e non dimostrano un effetto causale di cambiamenti organizzativi.",
  "",
  "SCENARI DI STERILIZZAZIONE (stessi mesi comparabili, stesso cap):",
  paste0("  ", names(SCENARI), ": ", SCENARI),
  paste0("Cap P", QUANTILE_OUTLIER * 100, " = ", round(CAP_P995, 2), " sec; limite di Tukey globale = ", round(CAP_TUKEY, 2),
         " sec (Q1 = ", Q_GLOBALI[1], ", mediana = ", Q_GLOBALI[2], ", Q3 = ", Q_GLOBALI[3], ", IQR = ", IQR_GLOBALE, ")."),
  "Il cap e' unico per i due anni e per tutti gli operatori; S5 e S6 cambiano solo la soglia minima.",
  "Includere le durate a zero secondi (S0) puo' invertire il segno della variazione della durata media: confrontare sempre numero e media.",
  "",
  "BOXPLOT E OUTLIER:",
  paste0("Segnalazioni Tukey mensili: ", sum(d$flag_tukey_mensile), "; outlier P", QUANTILE_OUTLIER * 100, ": ", sum(d$flag_p995),
         ". Gli insiemi non coincidono: il Tukey e' piu' severo e segnala anche chiamate che restano nei KPI principali."),
  "Registro_anomalie.csv e Registro_outlier_P995.csv riportano ID e riga di origine per la verifica nel sistema sorgente; non contengono numeri di telefono.",
  "Un outlier statistico NON implica errore o comportamento improprio: verificare trasferimenti, attese in linea, ticket e log del centralino.",
  paste0("CHIAMATE LUNGHE ESCLUSE: ", nrow(l), " eventi oltre il cap (", sum(l$comparabile), " nei mesi comparabili). ",
         "Dataset_ridotto_sole_lunghe.csv ha campi vuoti per annotare l'esito delle verifiche.")
)
writeLines(notes, file.path(out_dir, "LEGGIMI_metodo.txt"), useBytes = TRUE)
cat("ELABORAZIONE COMPLETATA\n", "Soglia minima", MIN_SECONDI, "sec; cap outlier", round(CAP_OUTLIER, 2), "sec\n", "Output:", out_dir, "\n")
print(P[, c("anno", "periodo", "ricevute", "servite_registrate", "gestite_valide", "tasso_gestite_valide_pct", "durata_media_sec")], row.names = FALSE)
