# Analisi dei flussi telefonici di un help desk (R)

Script in R per confrontare **due anni di chiamate** di un help desk telefonico su **mesi omogenei**. Misura quanto i risultati dipendono dai criteri di "sterilizzazione" del dato (durate a zero, conversazioni brevi, valori anomali), con **boxplot**, **outlier**, **registro delle anomalie** tracciabile e un approfondimento sulle **chiamate più lunghe**.

> ⚠️ Questo repository contiene **solo codice**. Non include dati reali: nessuna esportazione del centralino, nessun nome di operatori o di strutture, nessun numero di telefono. Per provare lo script c'è un generatore di **dati inventati**.

---

## Cosa fa

1. Legge un dataset normalizzato, **una riga per chiamata** (CSV), oppure le esportazioni XLSX del centralino.
2. **Sostituisce i nomi degli operatori** con etichette generiche (`Operatore A`, `B`, …) prima di ogni calcolo.
3. Calcola i **KPI** (mensili, per operatore, per periodo e per fase organizzativa) su mesi di calendario comparabili, senza attribuire a un cambiamento operativo le differenze di copertura tra i due estratti.
4. Confronta **sette scenari di sterilizzazione** (S0–S6) per isolare l'effetto di ogni filtro.
5. Costruisce **boxplot** e individua gli **outlier**, con un registro dei casi identificabile tramite ID e riga di origine.
6. Descrive le **chiamate lunghe** escluse dal cap: peso sui minuti, distribuzione, operatori.
7. Segnala le **anomalie di registrazione** (operatori con molte chiamate "servite" a durata zero) e verifica la **sensibilità** alle soglie.
8. Scrive tabelle CSV, 22 grafici PNG e una nota metodologica generata dai dati.

---

## Contenuto del repository

| File | Descrizione |
|---|---|
| `analisi_helpdesk.R` | Script con l'intera analisi |
| `genera_dati_esempio.R` | Crea `dati_esempio.csv`, circa 21.000 chiamate **inventate** |
| `README.md` | Questo documento |

## Esecuzione

```bash
Rscript genera_dati_esempio.R                 # crea dati_esempio.csv (dati fittizi)
Rscript analisi_helpdesk.R dati_esempio.csv   # produce la cartella Risultati_R
```

Con i propri dati: `Rscript analisi_helpdesk.R percorso/dati.csv`. Se il percorso non è indicato, lo script cerca `HelpDesk_dati_per_R.csv` accanto a sé e, in sua assenza, prova a leggere gli XLSX (serve il pacchetto `readxl`).

**Requisiti:** R ≥ 4.1. Nessun pacchetto per il CSV.

---

## Dati attesi

| Colonna | Contenuto |
|---|---|
| `timestamp` | `AAAA-MM-GG hh:mm:ss` |
| `attesa_sec` | secondi di attesa prima della risposta |
| `conversazione_sec` | durata della conversazione in secondi (0 se non risposta) |
| `operatore` | operatore che ha risposto (vuoto se nessuno) |
| `classificazione` | `Servite e Trasferite`, `Rifiutata`, `Abbandonate`, `Fuori tempo massimo`, `Fuori servizio` |
| *(opzionali)* `id_chiamata`, `fonte`, `riga_origine` | per la tracciabilità; se mancano, lo script li genera dalla posizione nel file |
| *(opzionali)* `causa_terminazione`, `operatori_loggati` | riportate nei registri se presenti |

Il dataset non deve contenere numeri di telefono né altri identificativi dei chiamanti.

---

## Definizioni

| Gruppo | Criterio | Ruolo |
|---|---|---|
| **Ricevute** | tutte le classificazioni | denominatore dei tassi, sempre incluse |
| **Servite registrate** | `Servite e Trasferite` | indicatore telefonico lordo |
| **Brevi** | servite con durata < 30 s | escluse dalle valide |
| **Outlier** | servite oltre il cap (99,5° percentile) | escluse dalle valide |
| **Gestite valide** | servite con durata ≥ 30 s e entro il cap | numeratore dei KPI "sterilizzati" |

Il cap è calcolato **una sola volta**, sui mesi comparabili di entrambi gli anni e sulle servite ≥ 30 s: i due anni usano la stessa soglia. Brevi e outlier **non** si sottraggono alle ricevute. Il tasso di gestite valide è un *proxy* operativo: misura conversazioni di durata significativa, non ticket risolti.

### Scenari di sterilizzazione

| Scenario | Criterio |
|---|---|
| **S0** | servite registrate, comprese le durate a 0 s |
| **S1** | servite con durata > 0 s |
| **S2** | servite con durata ≥ 30 s, senza limite superiore |
| **S3** | ≥ 30 s e ≤ cap: **criterio dei KPI principali** |
| **S4** | ≥ 30 s, esclusi gli outlier di Tukey **mensili** |
| **S5** | ≥ `MIN_SCENARIO_BASSO` (15 s) e ≤ cap |
| **S6** | ≥ `MIN_SCENARIO_ALTO` (60 s) e ≤ cap |

Il cap resta fisso in tutti gli scenari: cambia un criterio per volta. Includere le durate a zero (S0) può **invertire il segno** della variazione della durata media, quindi numero e media vanno sempre letti insieme.

### Outlier

- **P99,5** (cap globale): esclusione effettiva dalle gestite valide.
- **Tukey** (Q3 + 1,5 × IQR): solo **segnalazione diagnostica**. Q1 e Q3 sono calcolati mese per mese sulle servite ≥ 30 s prima di qualunque filtro superiore. Esiste anche il limite di Tukey globale, mostrato nei grafici.
- Gli insiemi **non coincidono**: Tukey è più severo e segnala anche chiamate che restano nei KPI principali. Un outlier statistico **non è un errore accertato**.

---

## Configurazione

All'inizio di `analisi_helpdesk.R`:

| Parametro | Significato |
|---|---|
| `ANNO_1`, `ANNO_2` | anni a confronto |
| `MESI_COMPARABILI` | mesi completi in entrambi gli anni |
| `MESI_PARZIALI` | mesi con estrazione incompleta (evidenziati nei grafici) |
| `MIN_SECONDI`, `QUANTILE_OUTLIER` | soglia minima di durata e percentile del cap |
| `METODO_OUTLIER` | `"P995"` oppure `"TUKEY"`: cap usato nei KPI principali |
| `MIN_SCENARIO_BASSO`, `MIN_SCENARIO_ALTO` | soglie minime di S5 e S6 |
| `FASI` | gruppi di mesi da confrontare separatamente (es. prima/dopo un cambio di assetto) |
| `ORGANICO_NOMINALE` | operatori previsti per periodo; serve per le conversazioni per operatore nominale |
| `ANONIMIZZA_OPERATORI` | `TRUE` = etichette generiche al posto dei nomi |
| `N_OPERATORI_GRAFICI` | operatori mostrati nei grafici per operatore |
| `SOGLIA_ZERO_ANOMALIA` | % di servite a durata zero oltre la quale si segnala l'operatore |
| `RIEPILOGO_ESTRAZIONI` | opzionale: totali dichiarati nelle esportazioni, per il confronto con le righe di dettaglio |
| `XLSX_*` | tracciato delle esportazioni XLSX, **da adattare** al proprio centralino |

---

## Output (cartella `Risultati_R`)

| Gruppo | File |
|---|---|
| **KPI** | `KPI_mensili.csv`, `KPI_mensili_comparabili.csv`, `KPI_operatori_mensili.csv`, `KPI_operatori_annuali.csv` |
| **Confronti** | `Confronto_mensile.csv` (stesso mese nei due anni), `Confronto_periodi.csv` (periodo comparabile e fasi) |
| **Scenari** | `Scenari_annuali.csv`, `Scenari_confronto.csv`, `Scenari_mensili.csv`, `Scomposizione_servite.csv` |
| **Boxplot e outlier** | `Boxplot_e_outlier_mensili.csv`, `Boxplot_e_outlier_operatori_mensili.csv` |
| **Registri** | `Registro_anomalie.csv` (Tukey mensile o P99,5), `Registro_outlier_P995.csv`: ID, riga di origine, operatore, durata, soglie e flag |
| **Chiamate lunghe** | `Dataset_ridotto_sole_lunghe.csv` (con campi vuoti per annotare le verifiche), `KPI_mensili_lunghe.csv`, `KPI_annuali_lunghe.csv`, `KPI_operatori_lunghe.csv` |
| **Qualità e sensibilità** | `Qualita_dati_operatori.csv`, `Sensibilita_soglie.csv`, `Audit_estrazioni.csv` (se configurato) |
| **Grafici** | `Grafici/` (00–21): volumi, tassi, rifiuti e abbandoni, durate, attesa, scenari, boxplot, outlier, chiamate lunghe |
| **Note** | `LEGGIMI_metodo.txt`: definizioni, soglie, copertura e avvertenze generate dai dati |

I tassi si confrontano in **punti percentuali**; volumi e durate in **variazione %**.

---

## Scelte metodologiche

- **Mesi omogenei:** si confrontano solo mesi presenti in modo completo in entrambi gli anni, mai l'anno intero.
- **Denominatore fisso:** i tassi sono sempre sulle chiamate ricevute, non su quelle registrate come risposte.
- **Qualità del dato prima dell'interpretazione:** un operatore con la maggior parte delle servite a durata zero fa pensare a un problema di registrazione o instradamento. Lo script lo segnala e avverte di non usare i KPI come valutazione individuale.
- **Un criterio per volta:** negli scenari cambia un solo filtro, con il cap fisso.
- **Sensibilità:** il confronto si ripete cambiando soglia minima e percentile.
- **Tracciabilità senza dati personali:** i registri riportano ID e riga di origine per la verifica nel sistema sorgente, mai i numeri di telefono.
- **Organico nominale:** è un rapporto illustrativo, non una produttività per FTE.

---

## Limiti noti

- L'analisi è **descrittiva**: non dimostra effetti causali di cambiamenti organizzativi.
- I dati non contengono ticket, turni, ore presidiate né esiti: non si misurano risoluzione al primo contatto né qualità.
- Gli outlier non spiegano la causa: servono log del centralino, trasferimenti e ticket.
- Il tracciato XLSX è un esempio da adattare.
- Un operatore con poche chiamate ha medie e boxplot poco affidabili.
- Se un'esportazione è parziale rispetto al riepilogo, i mesi coinvolti vanno esclusi dal confronto (`MESI_COMPARABILI`).

---

## Privacy

- Non pubblicare esportazioni del centralino, dataset normalizzati né **i risultati prodotti da dati reali**: i registri contengono data e ora, operatore, durata e riga di origine di ciascun evento, e senza anonimizzazione i nomi degli operatori.
- Con `ANONIMIZZA_OPERATORI <- TRUE` i nomi vengono sostituiti in memoria e la corrispondenza non viene salvata.
- Anche con etichette generiche, in team piccoli gli operatori restano riconoscibili dal contesto: per file condivisi all'esterno valutare un'ulteriore aggregazione.
- I risultati calcolati sui dati di esempio sono casuali e non descrivono alcun servizio reale.
- `.gitignore` consigliato:

```gitignore
*.csv
*.xlsx
*.xls
Risultati_R/
```

## Licenza

Scegli una licenza (ad esempio MIT) e aggiungi un file `LICENSE`.
