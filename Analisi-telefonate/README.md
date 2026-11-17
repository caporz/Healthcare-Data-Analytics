# Help Desk Call Volume Analysis (R)

R script for comparing **two years of telephone help desk calls** over **comparable months**. It measures how results depend on data "cleaning" criteria (zero-duration calls, short conversations, outliers), using **boxplots**, **outlier detection**, a traceable **anomaly log**, and a dedicated analysis of **the longest calls**.

> ⚠️ This repository contains **code only**. It does not include real data: no call center exports, operator or facility names, or phone numbers. A generator of **synthetic data** is provided to test the script.

---

## What It Does

1. Reads a normalized dataset, with **one row per call** (CSV), or XLSX exports from the call center.
2. **Replaces operator names** with generic labels (`Operator A`, `B`, …) before performing any calculations.
3. Calculates **KPIs** (monthly, by operator, by period, and by organizational phase) over comparable calendar months, without attributing differences in data coverage between the two extracts to operational changes.
4. Compares **seven data-cleaning scenarios** (S0–S6) to isolate the impact of each filter.
5. Generates **boxplots** and identifies **outliers**, with a case log traceable through IDs and source row numbers.
6. Describes **long calls** excluded by the upper cap, including their share of total minutes, distribution, and operators.
7. Flags **recording anomalies** (operators with a high number of "answered" calls recorded with zero duration) and tests **threshold sensitivity**.
8. Exports CSV tables, 22 PNG charts, and a data-driven methodological note.

---

## Repository Contents

| File | Description |
|---|---|
| `analisi_helpdesk.R` | Script containing the full analysis |
| `genera_dati_esempio.R` | Generates `dati_esempio.csv`, containing approximately 21,000 **synthetic** calls |
| `README.md` | This document |

## Execution

```bash
Rscript genera_dati_esempio.R                 # creates dati_esempio.csv (synthetic data)
Rscript analisi_helpdesk.R dati_esempio.csv   # generates the Risultati_R folder
```

To use your own data: `Rscript analisi_helpdesk.R path/to/data.csv`. If no path is specified, the script looks for `HelpDesk_dati_per_R.csv` in the same directory and, if it is not found, attempts to read XLSX files (the `readxl` package is required).

**Requirements:** R ≥ 4.1. No additional packages are required for CSV files.

---

## Expected Data

| Column | Contents |
|---|---|
| `timestamp` | `YYYY-MM-DD hh:mm:ss` |
| `attesa_sec` | Waiting time before the call is answered, in seconds |
| `conversazione_sec` | Conversation duration in seconds (0 if unanswered) |
| `operatore` | Operator who answered the call (blank if none) |
| `classificazione` | `Servite e Trasferite`, `Rifiutata`, `Abbandonate`, `Fuori tempo massimo`, `Fuori servizio` |
| *(optional)* `id_chiamata`, `fonte`, `riga_origine` | For traceability; if missing, the script generates them from the file position |
| *(optional)* `causa_terminazione`, `operatori_loggati` | Included in the logs if available |

The dataset must not contain phone numbers or any other caller identifiers.

---

## Definitions

| Group | Criteria | Purpose |
|---|---|---|
| **Received calls** | All classifications | Rate denominator; always included |
| **Recorded answered calls** | `Servite e Trasferite` | Gross telephony indicator |
| **Short calls** | Answered calls lasting less than 30 seconds | Excluded from valid calls |
| **Outliers** | Answered calls exceeding the upper cap (99.5th percentile) | Excluded from valid calls |
| **Valid handled calls** | Answered calls lasting ≥ 30 seconds and not exceeding the cap | Numerator for "cleaned" KPIs |

The cap is calculated **only once**, using comparable months from both years and answered calls lasting ≥ 30 seconds. Both years therefore use the same threshold. Short calls and outliers are **not** deducted from received calls. The valid handled call rate is an operational *proxy*: it measures conversations of meaningful duration, not resolved tickets.

### Data-Cleaning Scenarios

| Scenario | Criteria |
|---|---|
| **S0** | Recorded answered calls, including zero-duration calls |
| **S1** | Answered calls lasting more than 0 seconds |
| **S2** | Answered calls lasting ≥ 30 seconds, with no upper limit |
| **S3** | ≥ 30 seconds and ≤ cap: **main KPI criteria** |
| **S4** | ≥ 30 seconds, excluding monthly **Tukey outliers** |
| **S5** | ≥ `MIN_SCENARIO_BASSO` (15 seconds) and ≤ cap |
| **S6** | ≥ `MIN_SCENARIO_ALTO` (60 seconds) and ≤ cap |

The cap remains fixed across all scenarios: only one criterion changes at a time. Including zero-duration calls (S0) may **reverse the direction** of the change in average call duration. Therefore, call volumes and average durations must always be interpreted together.

### Outliers

- **P99.5** (global cap): actual exclusion from valid handled calls.
- **Tukey** (Q3 + 1.5 × IQR): used for **diagnostic flagging only**. Q1 and Q3 are calculated month by month on answered calls lasting ≥ 30 seconds, before applying any upper threshold. A global Tukey threshold is also available and displayed in the charts.
- The two sets **do not coincide**: Tukey is more restrictive and also flags calls that remain included in the main KPIs. A statistical outlier **is not necessarily a confirmed error**.

---

## Configuration

At the beginning of `analisi_helpdesk.R`:

| Parameter | Description |
|---|---|
| `ANNO_1`, `ANNO_2` | Years to compare |
| `MESI_COMPARABILI` | Months with complete data in both years |
| `MESI_PARZIALI` | Months with incomplete extracts (highlighted in charts) |
| `MIN_SECONDI`, `QUANTILE_OUTLIER` | Minimum call duration and percentile used for the cap |
| `METODO_OUTLIER` | `"P995"` or `"TUKEY"`: cap method used for the main KPIs |
| `MIN_SCENARIO_BASSO`, `MIN_SCENARIO_ALTO` | Minimum duration thresholds for S5 and S6 |
| `FASI` | Groups of months to compare separately (e.g. before/after an organizational change) |
| `ORGANICO_NOMINALE` | Planned number of operators per period; used to calculate calls per nominal operator |
| `ANONIMIZZA_OPERATORI` | `TRUE` = use generic labels instead of names |
| `N_OPERATORI_GRAFICI` | Number of operators displayed in operator-level charts |
| `SOGLIA_ZERO_ANOMALIA` | Percentage of zero-duration answered calls above which an operator is flagged |
| `RIEPILOGO_ESTRAZIONI` | Optional: totals reported in the exports, for comparison with detailed records |
| `XLSX_*` | XLSX export structure; **must be adapted** to the specific call center system |

---

## Output (`Risultati_R` folder)

| Group | Files |
|---|---|
| **KPIs** | `KPI_mensili.csv`, `KPI_mensili_comparabili.csv`, `KPI_operatori_mensili.csv`, `KPI_operatori_annuali.csv` |
| **Comparisons** | `Confronto_mensile.csv` (same month across the two years), `Confronto_periodi.csv` (comparable period and phases) |
| **Scenarios** | `Scenari_annuali.csv`, `Scenari_confronto.csv`, `Scenari_mensili.csv`, `Scomposizione_servite.csv` |
| **Boxplots and outliers** | `Boxplot_e_outlier_mensili.csv`, `Boxplot_e_outlier_operatori_mensili.csv` |
| **Logs** | `Registro_anomalie.csv` (monthly Tukey or P99.5), `Registro_outlier_P995.csv`: ID, source row, operator, duration, thresholds, and flags |
| **Long calls** | `Dataset_ridotto_sole_lunghe.csv` (with blank fields for recording verification notes), `KPI_mensili_lunghe.csv`, `KPI_annuali_lunghe.csv`, `KPI_operatori_lunghe.csv` |
| **Data quality and sensitivity** | `Qualita_dati_operatori.csv`, `Sensibilita_soglie.csv`, `Audit_estrazioni.csv` (if configured) |
| **Charts** | `Grafici/` (00–21): volumes, rates, refusals and abandoned calls, durations, waiting times, scenarios, boxplots, outliers, long calls |
| **Notes** | `LEGGIMI_metodo.txt`: definitions, thresholds, data coverage, and warnings generated from the data |

Rates are compared in **percentage points**; volumes and durations are compared using **percentage changes**.

---

## Methodological Choices

- **Comparable months:** only months with complete data in both years are compared; full-year comparisons are never used.
- **Fixed denominator:** rates are always calculated against received calls, not calls recorded as answered.
- **Data quality before interpretation:** an operator with most answered calls recorded as zero-duration may indicate a recording or routing issue. The script flags this and warns against using the KPIs for individual performance evaluation.
- **One criterion at a time:** each scenario changes only one filter, while keeping the cap fixed.
- **Sensitivity analysis:** comparisons are repeated using different minimum duration thresholds and percentile settings.
- **Traceability without personal data:** logs contain IDs and source row numbers to enable verification in the source system, but never phone numbers.
- **Nominal staffing:** this is an illustrative ratio, not an FTE-based productivity measure.

---

## Known Limitations

- The analysis is **descriptive**: it does not establish causal effects of organizational changes.
- The data do not include tickets, shifts, staffed hours, or outcomes. Therefore, first-contact resolution and service quality cannot be measured.
- Outliers do not explain their own causes: call center logs, transfer records, and tickets are needed for further investigation.
- The XLSX structure is an example and must be adapted.
- Average durations and boxplots are less reliable for operators handling only a small number of calls.
- If an export is incomplete compared with the reported summary, the affected months must be excluded from the comparison (`MESI_COMPARABILI`).

---

## Privacy

- Do not publish call center exports, normalized datasets, or **results generated from real data**: the logs contain the date and time, operator, duration, and source row of each event, as well as operator names if anonymization is disabled.
- With `ANONIMIZZA_OPERATORI <- TRUE`, operator names are replaced in memory and the mapping is not saved.
- Even when generic labels are used, operators may remain identifiable from context in small teams. Consider further aggregation before sharing files externally.
- Results generated from the sample data are random and do not describe any real service.
- Recommended `.gitignore`:

```gitignore
*.csv
*.xlsx
*.xls
Risultati_R/
```
