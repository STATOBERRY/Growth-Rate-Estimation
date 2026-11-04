# =============================================================================
# chat_table_builder.R  ·  RAISINS Repeated Measures One-way ANOVA
# =============================================================================
# Renders analysis result tables inside the chat (like chat_csv_generator.R).
# Self-contained: detection, CSS, HTML table builder, all in one file.
#
# PUBLIC FUNCTIONS (called by chat_server.R):
#   crd_table_css()                               → CSS string for table cards
#   crd_detect_table_request(msg)                 → list(key, char) or NULL
#   crd_build_table_card(results, key, char)      → tagList (styled card)
#   crd_table_caption(key, char)                  → caption string
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# 1. TABLE REQUEST DETECTION
# ─────────────────────────────────────────────────────────────────────────────
# Returns a list(key, char):
#   key  → "anova", "results", "grouping", "means", "summary", "individual"
#   char → character name string (e.g. "yield") or NULL for all characters
# Returns NULL if the message is not a table request.

.TABLE_KEYWORDS <- list(
  # ── Multivariate keys (placed first so multi-word matches win over generic) ──
  multivariate    = c("multivariate results", "multivariate analysis",
                      "all multivariate", "multivariate tables",
                      "show multivariate", "give multivariate",
                      "manova and pca", "pca and manova"),
  manova          = c("manova", "manova table", "multivariate anova",
                      "show manova", "give manova", "pillai", "pillai trace",
                      "eta squared", "wilks", "wilks lambda"),
  pca_eigen       = c("eigen value", "eigenvalue", "eigen values", "eigenvalues",
                      "pca eigen", "variance explained", "scree", "cumulative variance",
                      "pca variance", "principal component variance"),
  pca_loadings    = c("loading", "loadings", "pca loading", "pca loadings",
                      "factor loading", "variable loading",
                      "show loadings", "give loadings", "loadings table"),
  pca_index       = c("index score", "index scores", "pca index", "pca scores",
                      "pc1 index", "pc2 index", "scaled index",
                      "show index", "give index", "principal component score",
                      "pc score", "pc scores"),
  pca             = c("pca", "pca results", "pca analysis",
                      "principal component analysis", "principal components",
                      "show pca", "give pca"),
  correlation     = c("correlation matrix", "correlation table", "correlations",
                      "cor matrix", "pearson correlation",
                      "show correlation", "give correlation"),
  # ── Original CRD keys ───────────────────────────────────────────────────────
  # "all" → render every table at once with individual download buttons
  all        = c("all tables", "all the tables", "downloadable tables",
                 "download tables", "give me tables", "give me all tables",
                 "show me all tables", "show all tables", "all result tables",
                 "download all", "give downloadable", "all downloadable",
                 "every table", "all analysis tables", "give me downloadable",
                 "show downloadable", "tables download", "download the tables",
                 "tables","show tables","give tables","the tables", "results table" ),
  anova      = c("anova","anova table", "show anova", "give anova", "display anova",
                 "give me anova", "show me anova", "f test table",
                 "mean square table", "source of variation table",
                 "download anova", "anova download"),
  results    = c("results table", "result table", "show results table",
                 "give results table", "display results", "show the table",
                 "full table", "complete table", "analysis table",
                 "show me the table", "give me the table",
                 "download results", "results download", "download result table"),
  grouping   = c("grouping table", "letter grouping", "group table",
                 "show grouping", "give grouping", "letter group",
                 "dmrt table", "lsd table", "tukey table",
                 "comparison table", "mean comparison table",
                 "download grouping", "grouping download"),
  means      = c("treatment means", "show means", "mean table",
                 "give means", "display means", "mean values table",
                 "download means", "means download"),
  summary    = c("summary table", "show summary table", "summary statistics",
                 "descriptive statistics", "descriptive table",
                 "download summary", "summary download"),
  individual = c("individual anova", "character wise anova", "variable wise anova",
                 "separate anova", "individual analysis table",
                 "individual result", "individual table",
                 "result for", "results for", "table for",
                 "anova for", "analysis for",
                 "download individual", "individual download",
                 "character table", "per character", "each character")
)

# Phrases that signal the user wants results for a specific character.
# The character name is extracted from what follows these phrases.
.CHAR_TRIGGER_PHRASES <- c(
  "result for", "results for", "table for",
  "anova for", "analysis for",
  "result of", "results of",
  "show result for", "show results for",
  "give result for", "give results for",
  "individual result for", "individual table for",
  "character wise", "character-wise",
  "variable wise", "variable-wise"
)

crd_detect_table_request <- function(msg) {
  m <- tolower(trimws(msg))
  
  # Keys that legitimately support a "for <character>" filter without being
  # redirected to the per-character "individual" card.
  .keys_supporting_char_filter <- c("pca_loadings", "pca_index", "correlation")
  
  # ── Step 1: check for a specific-character request ──────────────────────────
  for (phrase in .CHAR_TRIGGER_PHRASES) {
    if (grepl(phrase, m, fixed = TRUE)) {
      after <- trimws(sub(paste0(".*", phrase), "", m))
      after <- gsub("[^a-z0-9 _%-]", "", after)
      after <- trimws(after)
      if (nchar(after) > 0) {
        return(list(key = "individual", char = after))
      } else {
        return(list(key = "individual", char = NULL))
      }
    }
  }
  
  # ── Step 2: standard keyword detection ──────────────────────────────────────
  for (key in names(.TABLE_KEYWORDS))
    for (kw in .TABLE_KEYWORDS[[key]])
      if (grepl(kw, m, fixed = TRUE)) {
        # Extract optional "for <name>" tail
        char_match <- regmatches(m, regexpr("\\bfor\\s+([a-z0-9_%-]+)\\s*$", m, perl = TRUE))
        char_name <- if (length(char_match) == 1 && nchar(char_match) > 0) {
          trimws(sub("^for\\s+", "", char_match))
        } else NULL
        
        if (!is.null(char_name) && nchar(char_name) > 0) {
          # Multivariate keys accept a char filter but keep their own key.
          if (key %in% .keys_supporting_char_filter) {
            return(list(key = key, char = char_name))
          }
          # Otherwise route to per-character individual ANOVA (original behaviour).
          return(list(key = "individual", char = char_name))
        }
        return(list(key = key, char = NULL))
      }
  
  NULL
}


# ─────────────────────────────────────────────────────────────────────────────
# 2. CSS FOR TABLE CARDS
# ─────────────────────────────────────────────────────────────────────────────
crd_table_css <- function() {
  "
/* ── RAISINS Chat Table Card ────────────────────────────── */
.rch-tbl-card {
  margin-top:8px; border-radius:12px; overflow:hidden;
  border:1px solid var(--border); background:white;
  box-shadow:0 2px 12px rgba(86,4,102,0.08);
}
.rch-tbl-header {
  display:flex; align-items:center; gap:8px; padding:10px 14px;
  background:#faf0fc; border-bottom:1px solid var(--border);
  font-size:12.5px; color:var(--txt); font-weight:600;
}
.rch-tbl-header .fa { font-size:14px; color:var(--accent); }
.rch-tbl-body {
  padding:12px 14px; overflow-x:auto; background:white;
}
.rch-tbl-body table {
  border-collapse:collapse; width:100%; font-size:12.5px;
  font-family:var(--font);
}
.rch-tbl-body thead th {
  background:linear-gradient(135deg,#2d1a30,#560466); color:#fff;
  padding:8px 12px; font-size:11.5px; font-weight:600;
  text-align:left; white-space:nowrap; letter-spacing:.02em;
  border:1px solid #3d0050;
}
.rch-tbl-body tbody td {
  padding:7px 12px; border:1px solid #e8d9ed; color:var(--txt);
  font-size:12px;
}
.rch-tbl-body tbody tr:nth-child(even) { background:#fdf8fe; }
.rch-tbl-body tbody tr:hover { background:#f3eaf6; }
.rch-tbl-body sup { color:#d4380d; font-weight:700; }
.rch-tbl-body .rch-sig { color:#d4380d; font-weight:700; }
.rch-tbl-body .rch-ns  { color:#8c8c8c; }
.rch-tbl-footer {
  padding:8px 14px; border-top:1px solid #f0e4f5;
  font-size:11px; color:var(--txt3); background:#fdf8fe;
  font-style:italic;
}
.rch-tbl-note {
  font-size:11px; color:var(--txt3); margin-top:6px;
  padding:6px 14px; font-style:italic;
}
/* ── Download button ── */
.rch-tbl-dl-bar {
  padding:6px 14px 10px; background:#fdf8fe;
  border-top:1px solid #f0e4f5; text-align:right;
}
.rch-tbl-dl-btn {
  display:inline-flex; align-items:center; gap:5px;
  padding:5px 13px; border-radius:7px; font-size:11.5px;
  font-weight:600; cursor:pointer; border:none;
  background:linear-gradient(135deg,#560466,#8b1a9a);
  color:#fff; transition:opacity .15s;
}
.rch-tbl-dl-btn:hover { opacity:.85; }
"
}


# ─────────────────────────────────────────────────────────────────────────────
# 3. HTML TABLE BUILDER HELPERS
# ─────────────────────────────────────────────────────────────────────────────

## safe escape that preserves <sup>/<sub> tags already in data
.safe_cell <- function(txt) {
  # Keep cells that already contain safe inline tags (sup, sub, br) as raw HTML.
  if (grepl("<su[pb]>|<br\\s*/?>", txt, ignore.case = TRUE)) return(txt)
  htmltools::htmlEscape(txt)
}

## Extract clean plain text for CSV export:
## strips <sup>/<sub> tags, HTML entities, ± symbol kept as-is
.plain_cell <- function(txt) {
  txt <- as.character(txt)
  # <br> -> single space, so "33.705<br>(1.528)" becomes "33.705 (1.528)" in CSV
  txt <- gsub("<br\\s*/?>", " ", txt, ignore.case = TRUE, perl = TRUE)
  # Unwrap <sup> and <sub>: keep the text inside, remove only the tags
  txt <- gsub("<sup>(.*?)</sup>", "\\1", txt, perl = TRUE)
  txt <- gsub("<sub>(.*?)</sub>", "\\1", txt, perl = TRUE)
  txt <- gsub("<[^>]+>", "", txt, perl = TRUE)           # strip any remaining tags
  txt <- gsub("&plusmn;",  "\u00b1", txt, fixed = TRUE) # &plusmn; -> ±
  txt <- gsub("&amp;",     "&",      txt, fixed = TRUE)
  txt <- gsub("&lt;",      "<",      txt, fixed = TRUE)
  txt <- gsub("&gt;",      ">",      txt, fixed = TRUE)
  txt <- gsub("&nbsp;",    " ",      txt, fixed = TRUE)
  txt <- gsub("\u00c2\u00b1", "\u00b1", txt, fixed = TRUE)  # Â± -> ±
  txt <- gsub("\\s+", " ", txt)  # collapse stray double-spaces from br removal
  trimws(txt)
}

# Convert a data.frame to an HTML table string.
# Every <td> and <th> gets a data-csv attribute with clean plain text
# so the download button reads that instead of parsing rendered HTML.
.df_to_html <- function(df, caption = NULL, row_names = TRUE) {
  if (is.null(df) || nrow(df) == 0) return("<p>No data available.</p>")
  
  col_names <- colnames(df)
  if (row_names && !is.null(rownames(df)) &&
      !identical(rownames(df), as.character(seq_len(nrow(df))))) {
    col_names <- c("", col_names)
    has_rn <- TRUE
  } else {
    has_rn <- FALSE
  }
  
  # Headers with data-csv
  th_cells <- paste0(
    "<th data-csv=\"", gsub("\"", "&quot;", col_names), "\">",
    htmltools::htmlEscape(col_names), "</th>",
    collapse = ""
  )
  
  body_rows <- sapply(seq_len(nrow(df)), function(i) {
    rn_cell <- if (has_rn) {
      rn <- rownames(df)[i]
      paste0("<td data-csv=\"", gsub("\"", "&quot;", rn), "\" ",
             "style='font-weight:600;background:#f3eaf6;white-space:nowrap;'>",
             htmltools::htmlEscape(rn), "</td>")
    } else ""
    
    cells <- sapply(seq_len(ncol(df)), function(j) {
      val  <- df[i, j]
      txt  <- if (is.na(val)) "-" else as.character(val)
      csv_val <- gsub("\"", "&quot;", .plain_cell(txt))
      paste0("<td data-csv=\"", csv_val, "\">", .safe_cell(txt), "</td>")
    })
    paste0("<tr>", rn_cell, paste(cells, collapse = ""), "</tr>")
  })
  
  cap_html <- if (!is.null(caption))
    paste0("<caption style='caption-side:top;font-weight:600;font-size:12px;",
           "color:#560466;padding:6px 0;text-align:left;'>",
           htmltools::htmlEscape(caption), "</caption>") else ""
  
  paste0("<table>", cap_html,
         "<thead><tr>", th_cells, "</tr></thead><tbody>",
         paste(body_rows, collapse = "\n"),
         "</tbody></table>")
}


# ─────────────────────────────────────────────────────────────────────────────
# 4. DOWNLOAD BUTTON HELPER
# ─────────────────────────────────────────────────────────────────────────────
# Generates a client-side CSV download button.
# Collects all <table> elements inside the nearest .rch-tbl-card parent.

# One-time script injector — inserts the delegated click handler only once per page.
# The handler reads data-filename from the button and collects tables from the
# nearest .rch-tbl-card ancestor, avoiding inline onclick issues in Shiny.
.dl_script <- function() {
  tags$script(HTML("
(function() {
  if (window._rchDlReady) return;
  window._rchDlReady = true;

  document.addEventListener('click', function(e) {
    var btn = e.target.closest('.rch-tbl-dl-btn');
    if (!btn) return;
    var filename = btn.getAttribute('data-filename') || 'table.csv';
    var card = btn.closest('.rch-tbl-card');
    var tbls = card ? card.querySelectorAll('table') : [];
    var csv  = [];
    tbls.forEach(function(tbl) {
      var rows = tbl.querySelectorAll('tr');
      rows.forEach(function(row) {
        var cells = row.querySelectorAll('th, td');
        var line  = Array.from(cells).map(function(c) {
          // Use data-csv (set by R with clean plain text) — fall back to textContent
          var t = c.hasAttribute('data-csv')
                    ? c.getAttribute('data-csv')
                    : (c.textContent || '').trim();
          t = t.replace(/\\r?\\n|\\t/g, ' ').replace(/\\s+/g, ' ').trim();
          return '\"' + t.replace(/\"/g, '\"\"') + '\"';
        });
        csv.push(line.join(','));
      });
      csv.push('');
    });
    var BOM  = '\\uFEFF';   // UTF-8 BOM so Excel opens ± correctly
    var blob = new Blob([BOM + csv.join('\\n')], { type: 'text/csv;charset=utf-8;' });
    var a    = document.createElement('a');
    a.href   = URL.createObjectURL(blob);
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    setTimeout(function() {
      document.body.removeChild(a);
      URL.revokeObjectURL(a.href);
    }, 100);
  });
})();
"))
}

.dl_button <- function(filename = "table.csv") {
  tagList(
    .dl_script(),   # no-op if already injected (guarded by window._rchDlReady)
    div(class = "rch-tbl-dl-bar",
        tags$button(
          class            = "rch-tbl-dl-btn",
          `data-filename`  = filename,
          icon("download"), " Download CSV"
        ))
  )
}


# ─────────────────────────────────────────────────────────────────────────────
# 5. MAIN TABLE CARD BUILDER
# ─────────────────────────────────────────────────────────────────────────────
# @param results  The combined_results list from rcbd_results_store()
# @param key      Table key from crd_detect_table_request()$key
# @param char     Character name string or NULL (from crd_detect_table_request()$char)
# @return tagList (card UI) or NULL

crd_build_table_card <- function(results, key, char = NULL) {
  if (is.null(results)) return(NULL)
  
  switch(key,
         "all"          = .build_all_card(results),
         "anova"        = .build_anova_card(results),
         "results"      = .build_results_card(results),
         "grouping"     = .build_grouping_card(results),
         "means"        = .build_means_card(results),
         # "summary"    = .build_summary_card(results),
         "summary"      = NULL,
         "individual"   = .build_individual_card(results, char),
         # ── Multivariate ────────────────────────────────────────────────────
         "multivariate" = .build_multivariate_all_card(results),
         "manova"       = .build_manova_card(results),
         "pca"          = .build_pca_all_card(results),
         "pca_eigen"    = .build_pca_eigen_card(results),
         "pca_loadings" = .build_pca_loadings_card(results, char),
         "pca_index"    = .build_pca_index_card(results, char),
         "correlation"  = .build_correlation_card(results, char),
         NULL
  )
}

# Caption text for the chat response
crd_table_caption <- function(key, char = NULL) {
  if (key == "individual" && !is.null(char)) {
    return(paste0("Here's the **individual result table for '", char, "'**:"))
  }
  base <- switch(key,
                 "all"          = "Here are **all your analysis tables** \u2014 each with its own Download CSV button:",
                 "anova"        = "Here's the **ANOVA table** from your Repeated Measures One-way ANOVA:",
                 "results"      = "Here's the **results table** with treatment means, grouping, and test statistics:",
                 "grouping"     = "Here's the **mean comparison / grouping table**:",
                 "means"        = "Here's the **treatment means** table:",
                 # "summary"      = "Here's the **summary statistics** table:",
                 "summary"      = "Summary statistics (descriptive stats) are available as a separate tab inside the app. Go to the Analysis section and click the **Summary Stats** tab to view Mean, SD, SE, CV%, Skewness, Kurtosis and more for each variable.",
                 "individual"   = "Here's the **individual ANOVA** for each character:",
                 "multivariate" = "Here are **all multivariate results** \u2014 MANOVA, PCA eigenvalues, loadings, index scores, and correlation matrix:",
                 "manova"       = "Here is the **MANOVA table** (multivariate ANOVA across all response variables):",
                 "pca"          = "Here are the **PCA results** \u2014 eigenvalues, loadings, and PC1 / PC1+PC2 index scores:",
                 "pca_eigen"    = "Here are the **PCA eigenvalues and variance explained** by each principal component:",
                 "pca_loadings" = "Here are the **PCA loadings** (variable contributions to each PC):",
                 "pca_index"    = "Here are the **PCA index scores** (PC1 and PC1+PC2-based treatment ranking):",
                 "correlation"  = "Here is the **correlation matrix** of the selected response variables:",
                 "Here's the requested table:"
  )
  if (!is.null(char) && nzchar(char)) {
    base <- paste0(base, "\n\nFiltered to: **", char, "**")
  }
  base
}


# ─────────────────────────────────────────────────────────────────────────────
# 6. INDIVIDUAL TABLE BUILDERS
# ─────────────────────────────────────────────────────────────────────────────

## ---------------------------------------------------------------------------
## Repeated measures carries THREE significance sources - Treatment (A), Day (B)
## and Treatment x Day (AB). The CRD original only ever read `df1` / `rcbd_groups`
## / `rcbd_trt_means`, so the Day and interaction tables the store already
## publishes never reached the chat. Each card now walks all three and silently
## skips whichever the store does not carry.
## ---------------------------------------------------------------------------
## One-way repeated measures has a SINGLE within-subject source, so the
## Treatment / Day / Treatment x Day triple of the two-way module collapses
## to one entry. The stack helper below is kept so adding a source later is
## a one-line change.
.RM_SOURCES <- list(
  list(res = "df1", grp = "rcbd_groups", mean = "emm_meanA", lev = "levelnamesA",
       label = "Time")
)

## Stack one card per source; returns NULL if the store has none of them.
.rm_stack <- function(results, builder) {
  cards <- lapply(.RM_SOURCES, function(src) builder(results, src))
  cards <- Filter(Negate(is.null), cards)
  if (length(cards) == 0) return(NULL)
  tagList(cards)
}

## Label a matrix of per-character values (rows = factor levels, cols = characters)
.rm_label_matrix <- function(mat, results, lev_key) {
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  chars <- results$rcbd_char
  if (!is.null(chars) && length(chars) == ncol(df)) colnames(df) <- chars
  if (!is.null(lev_key)) {
    lev <- results[[lev_key]]
    if (!is.null(lev) && length(lev) == nrow(df)) rownames(df) <- lev
  }
  df
}

.build_all_card <- function(results) {
  # Collect each sub-card (skip NULLs silently)
  cards <- list(
    .build_results_card(results),
    .build_anova_card(results),
    .build_grouping_card(results),
    .build_individual_card(results, char = NULL),
    .build_means_card(results)
  )
  cards <- Filter(Negate(is.null), cards)
  if (length(cards) == 0) return(NULL)

  labels <- c("Results Tables", "ANOVA Table", "Letter Grouping",
              "Individual ANOVA (per character)", "Estimated Marginal Means")

  sections <- mapply(function(card, lbl) {
    div(style = "margin-bottom:18px;",
        div(style = paste0("font-size:11px;font-weight:700;letter-spacing:.06em;",
                           "color:#560466;text-transform:uppercase;margin-bottom:4px;"),
            lbl),
        card)
  }, cards, labels[seq_along(cards)], SIMPLIFY = FALSE)

  tagList(sections)
}

.build_anova_card <- function(results) {
  df4 <- results$df4
  if (is.null(df4)) return(NULL)

  html <- .df_to_html(df4, caption = "Analysis of Variance (One-way Repeated Measures)", row_names = FALSE)
  note <- paste0(
    "ℹ️ SoV = Source of Variation, DF = Degrees of Freedom. ",
    "Model: response ~ Day + Error(Subject/Day) - a one-way repeated measures ANOVA. ",
    "The within-subject factor is tested against the Subject x Day residual."
  )

  div(class = "rch-tbl-card",
      div(class = "rch-tbl-header", icon("square-root-variable"), " ANOVA Table"),
      div(class = "rch-tbl-body", HTML(html)),
      div(class = "rch-tbl-footer", note),
      .dl_button("anova_table.csv"))
}

.build_results_card <- function(results) {
  .rm_stack(results, function(res, src) {
    d <- res[[src$res]]
    if (is.null(d)) return(NULL)

    html <- .df_to_html(d, caption = paste0("Results Table — ", src$label))
    note_parts <- c()
    if (isTRUE(res$allow_letter_grouping))
      note_parts <- c(note_parts, "Means followed by the same letter are not significantly different.")
    note_parts <- c(note_parts, "* p<0.05, ** p<0.01, ns = not significant.")

    div(class = "rch-tbl-card",
        div(class = "rch-tbl-header", icon("table"), paste0(" Results — ", src$label)),
        div(class = "rch-tbl-body", HTML(html)),
        div(class = "rch-tbl-footer", paste(note_parts, collapse = " ")),
        .dl_button(paste0("results_", gsub("[^A-Za-z]", "", src$label), ".csv")))
  })
}

.build_grouping_card <- function(results) {
  .rm_stack(results, function(res, src) {
    d <- res[[src$res]]
    if (!is.null(d)) {
      html <- .df_to_html(d, caption = paste0("Mean Comparison & Grouping — ", src$label))
      return(div(class = "rch-tbl-card",
                 div(class = "rch-tbl-header", icon("layer-group"),
                     paste0(" Grouping — ", src$label)),
                 div(class = "rch-tbl-body", HTML(html)),
                 div(class = "rch-tbl-footer",
                     "Means followed by the same letter are not significantly different."),
                 .dl_button(paste0("grouping_", gsub("[^A-Za-z]", "", src$label), ".csv"))))
    }

    grp <- res[[src$grp]]
    if (is.null(grp)) return(NULL)
    grp_df <- .rm_label_matrix(grp, res, src$lev)

    html <- .df_to_html(grp_df, caption = paste0("Letter Grouping — ", src$label))
    div(class = "rch-tbl-card",
        div(class = "rch-tbl-header", icon("layer-group"),
            paste0(" Grouping — ", src$label)),
        div(class = "rch-tbl-body", HTML(html)),
        div(class = "rch-tbl-footer",
            "Means followed by the same letter are not significantly different."),
        .dl_button(paste0("grouping_", gsub("[^A-Za-z]", "", src$label), ".csv")))
  })
}

.build_means_card <- function(results) {
  .rm_stack(results, function(res, src) {
    m <- res[[src$mean]]
    if (is.null(m)) return(NULL)
    mean_df <- .rm_label_matrix(m, res, src$lev)

    html <- .df_to_html(mean_df, caption = paste0("Estimated Marginal Means — ", src$label))

    div(class = "rch-tbl-card",
        div(class = "rch-tbl-header", icon("calculator"),
            paste0(" Means — ", src$label)),
        div(class = "rch-tbl-body", HTML(html)),
        div(class = "rch-tbl-footer",
            "Estimated marginal means (emmeans) across all response variables."),
        .dl_button(paste0("means_", gsub("[^A-Za-z]", "", src$label), ".csv")))
  })
}

.build_summary_card <- function(results) {
  df1 <- results$df1
  if (is.null(df1)) return(.build_means_card(results))
  
  html <- .df_to_html(df1, caption = "Summary Statistics")
  
  div(class = "rch-tbl-card",
      div(class = "rch-tbl-header", icon("chart-bar"), " Summary Statistics"),
      div(class = "rch-tbl-body", HTML(html)),
      div(class = "rch-tbl-footer",
          "Includes means, grouping, F stat, p value, MSE, SE(m), SE(d), CV%."),
      .dl_button("summary_statistics.csv"))
}


# ─────────────────────────────────────────────────────────────────────────────
# Individual ANOVA card — supports filtering by a single character name
# ─────────────────────────────────────────────────────────────────────────────
.build_individual_card <- function(results, char = NULL) {
  indv <- results$individual_anova
  if (is.null(indv)) return(NULL)
  
  # Simple case: already a single data.frame (no per-character filtering)
  if (is.data.frame(indv)) {
    html <- .df_to_html(indv, caption = "Individual Character ANOVA")
    return(div(class = "rch-tbl-card",
               div(class = "rch-tbl-header", icon("list"), " Individual ANOVA"),
               div(class = "rch-tbl-body", HTML(html)),
               .dl_button("individual_anova.csv")))
  }
  
  if (!is.list(indv)) return(NULL)
  
  char_names <- names(indv)
  
  # ── Filter by character name when user asks for a specific one ────────────
  if (!is.null(char) && nchar(trimws(char)) > 0) {
    char_lc       <- tolower(trimws(char))
    char_names_lc <- tolower(char_names)
    
    # 1. Exact match
    matched <- char_names[char_names_lc == char_lc]
    
    # 2. Partial / substring match
    if (length(matched) == 0)
      matched <- char_names[grepl(char_lc, char_names_lc, fixed = TRUE)]
    
    # 3. Token overlap (e.g. "grain yield" → "Grain Yield (t/ha)")
    if (length(matched) == 0) {
      tokens <- strsplit(char_lc, "\\s+")[[1]]
      scores <- sapply(char_names_lc, function(nm)
        sum(sapply(tokens, function(tok) grepl(tok, nm, fixed = TRUE))))
      best <- which(scores == max(scores) & scores > 0)
      matched <- char_names[best]
    }
    
    if (length(matched) == 0) {
      return(div(class = "rch-tbl-card",
                 div(class = "rch-tbl-header", icon("list"),
                     paste0(" No character matching '\u2018", char, "\u2019'")),
                 div(class = "rch-tbl-body",
                     tags$p(style = "color:#888; font-size:12.5px;",
                            paste0("Could not find a character named '", char, "'. ",
                                   "Available: ",
                                   paste(char_names, collapse = ", "), ".")))))
    }
    
    char_names <- matched
  }
  
  # ── Build one section per (selected) character ───────────────────────────
  cards <- lapply(char_names, function(nm) {
    entry <- indv[[nm]]
    if (is.null(entry)) return(NULL)
    
    sections <- list()
    
    if (!is.null(entry$ANOVA) && is.data.frame(entry$ANOVA)) {
      sections <- c(sections, list(
        div(style = "margin-bottom:8px;",
            HTML(.df_to_html(entry$ANOVA,
                             caption = paste0("ANOVA \u2014 ", nm),
                             row_names = FALSE)))
      ))
    }
    
    trt_tbl <- entry$Summary$Treatment
    if (!is.null(trt_tbl) && is.data.frame(trt_tbl)) {
      sections <- c(sections, list(
        div(style = "margin-bottom:8px;",
            HTML(.df_to_html(trt_tbl,
                             caption = paste0("Treatment Means \u2014 ", nm),
                             row_names = FALSE)))
      ))
    }
    
    stat_tbl <- entry$stat_summary$Treatment
    if (!is.null(stat_tbl) && is.data.frame(stat_tbl)) {
      sections <- c(sections, list(
        div(style = "margin-bottom:12px;",
            HTML(.df_to_html(stat_tbl,
                             caption = paste0("Statistics \u2014 ", nm),
                             row_names = FALSE)))
      ))
    }
    
    # Fallback: entry is itself a data.frame
    if (length(sections) == 0 && is.data.frame(entry)) {
      sections <- list(
        div(style = "margin-bottom:12px;",
            HTML(.df_to_html(entry, caption = paste0("ANOVA \u2014 ", nm))))
      )
    }
    
    if (length(sections) == 0) return(NULL)
    
    div(style = "margin-bottom:16px; padding-bottom:12px; border-bottom:1px solid #e8d9ed;",
        tags$div(style = "font-size:13px; font-weight:700; color:#560466; margin-bottom:6px;",
                 paste0("\U0001f4ca ", nm)),
        tagList(sections))
  })
  
  cards <- Filter(Negate(is.null), cards)
  if (length(cards) == 0) return(NULL)
  
  # Header and filename reflect whether we are showing one or all characters
  header_label <- if (!is.null(char) && length(char_names) == 1)
    paste0(" Individual ANOVA \u2014 ", char_names[1])
  else
    " Individual ANOVA (per character)"
  
  dl_name <- if (!is.null(char) && length(char_names) == 1)
    paste0("individual_anova_", gsub("\\s+", "_", tolower(char_names[1])), ".csv")
  else
    "individual_anova_all.csv"
  
  div(class = "rch-tbl-card",
      div(class = "rch-tbl-header", icon("list"), header_label),
      div(class = "rch-tbl-body", tagList(cards)),
      .dl_button(dl_name))
}


# ─────────────────────────────────────────────────────────────────────────────
# 7. MULTIVARIATE TABLE BUILDERS (MANOVA + PCA + Correlation)
# ─────────────────────────────────────────────────────────────────────────────
# These rely on data injected into rcbd_results_store by the additive observe
# block in crd_server.R (Part A).  If those fields are NULL the cards silently
# return NULL — they will only appear once the user actually clicks the MANOVA
# or PCA buttons inside the Multivariate tab.

# Helper that strips HTML from PCA narrative strings so the chat shows plain text.
.strip_html_narr <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return("")
  txt <- gsub("<br\\s*/?>", "\n", txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("</p>",  "\n\n", txt, ignore.case = TRUE)
  txt <- gsub("<[^>]+>", "",   txt, perl = TRUE)
  txt <- gsub("&nbsp;",  " ",  txt, fixed = TRUE)
  txt <- gsub("&amp;",   "&",  txt, fixed = TRUE)
  txt <- gsub("\\s+\n",  "\n", txt)
  trimws(txt)
}

# Tries several plausible column names to find a column in `df` containing
# variable/trait names (used by PCA loadings and correlation cards).
.match_pca_rows <- function(df, char) {
  if (is.null(df) || is.null(char) || !nzchar(char)) return(NULL)
  rn <- rownames(df)
  if (is.null(rn)) return(NULL)
  ch <- tolower(trimws(char))
  rn_lc <- tolower(rn)
  hit <- rn[rn_lc == ch]
  if (length(hit) == 0) hit <- rn[grepl(ch, rn_lc, fixed = TRUE)]
  if (length(hit) == 0) NULL else hit
}

# ── MANOVA table ────────────────────────────────────────────────────────────
.build_manova_card <- function(results) {
  manova_df <- results$rcbd_manova_table
  if (is.null(manova_df) || !is.data.frame(manova_df) || nrow(manova_df) == 0) return(NULL)
  
  html <- .df_to_html(as.data.frame(manova_df),
                      caption = "MANOVA (multivariate ANOVA)",
                      row_names = TRUE)
  
  note_parts <- c("MANOVA tests whether treatment means differ across all response variables jointly.")
  pval <- results$rcbd_manova_pval
  if (!is.null(pval) && nzchar(as.character(pval))) {
    pnum <- suppressWarnings(as.numeric(pval))
    if (!is.na(pnum)) {
      sig <- if (pnum <= 0.05) "Significant \u2014 treatments differ in multivariate response space." 
      else "Not significant \u2014 no joint treatment effect detected."
      note_parts <- c(note_parts, paste0("Pillai p = ", pval, ". ", sig))
    }
  }
  eta <- results$rcbd_manova_eta_sq
  if (!is.null(eta) && !is.na(suppressWarnings(as.numeric(eta)))) {
    note_parts <- c(note_parts, paste0("Eta\u00b2 = ", eta,
                                       " (effect size for the treatment factor)."))
  }
  
  div(class = "rch-tbl-card",
      div(class = "rch-tbl-header", icon("vials"), " MANOVA Table"),
      div(class = "rch-tbl-body", HTML(html)),
      div(class = "rch-tbl-footer", paste(note_parts, collapse = " ")),
      .dl_button("manova_table.csv"))
}

# ── PCA Eigenvalues table ───────────────────────────────────────────────────
.build_pca_eigen_card <- function(results) {
  eig <- results$rcbd_pca_eigen
  if (is.null(eig) || !is.data.frame(eig) || nrow(eig) == 0) return(NULL)
  
  html <- .df_to_html(as.data.frame(eig),
                      caption = "PCA \u2014 Eigenvalues and Variance Explained",
                      row_names = TRUE)
  
  narr <- .strip_html_narr(results$rcbd_pca_abt_eigen)
  note <- if (nzchar(narr)) narr else
    "Eigenvalue > 1 \u2192 component retains more variance than a single original variable (Kaiser criterion)."
  
  div(class = "rch-tbl-card",
      div(class = "rch-tbl-header", icon("chart-line"), " PCA Eigenvalues"),
      div(class = "rch-tbl-body", HTML(html)),
      div(class = "rch-tbl-footer", note),
      .dl_button("pca_eigenvalues.csv"))
}

# ── PCA Loadings table ──────────────────────────────────────────────────────
.build_pca_loadings_card <- function(results, char = NULL) {
  load_df <- results$rcbd_pca_loadings
  if (is.null(load_df) || !is.data.frame(load_df) || nrow(load_df) == 0) return(NULL)
  
  display <- as.data.frame(load_df)
  if (!is.null(char) && nzchar(char)) {
    hit <- .match_pca_rows(display, char)
    if (!is.null(hit) && length(hit) > 0) {
      display <- display[hit, , drop = FALSE]
    }
  }
  if (nrow(display) == 0) return(NULL)
  
  html <- .df_to_html(display,
                      caption = "PCA \u2014 Variable Loadings on Principal Components",
                      row_names = TRUE)
  
  narr <- .strip_html_narr(results$rcbd_pca_abt_loadings)
  note <- if (nzchar(narr)) narr else
    "Loadings show how strongly each variable contributes to each PC. Larger absolute values \u2192 stronger contribution. Variables with same-sign loadings move together on that PC."
  
  div(class = "rch-tbl-card",
      div(class = "rch-tbl-header", icon("compass"), " PCA Loadings"),
      div(class = "rch-tbl-body", HTML(html)),
      div(class = "rch-tbl-footer", note),
      .dl_button("pca_loadings.csv"))
}

# ── PCA Index Scores (PC1 and PC1+PC2) ──────────────────────────────────────
.build_pca_index_card <- function(results, char = NULL) {
  idx1 <- results$rcbd_pca_index1
  idx2 <- results$rcbd_pca_index2
  if ((is.null(idx1) || !is.data.frame(idx1) || nrow(idx1) == 0) &&
      (is.null(idx2) || !is.data.frame(idx2) || nrow(idx2) == 0)) return(NULL)
  
  filter_rows <- function(df, ch) {
    if (is.null(df) || is.null(ch) || !nzchar(ch)) return(df)
    hit <- .match_pca_rows(df, ch)
    if (is.null(hit) || length(hit) == 0) df else df[hit, , drop = FALSE]
  }
  idx1 <- filter_rows(idx1, char)
  idx2 <- filter_rows(idx2, char)
  
  parts <- c()
  if (!is.null(idx1) && nrow(idx1) > 0) {
    parts <- c(parts,
               "<p style='font-size:11.5px;font-weight:700;color:#560466;margin:8px 0 4px;'>Index based on PC1</p>",
               .df_to_html(as.data.frame(idx1), row_names = TRUE))
  }
  if (!is.null(idx2) && nrow(idx2) > 0) {
    parts <- c(parts,
               "<p style='font-size:11.5px;font-weight:700;color:#560466;margin:12px 0 4px;'>Index based on PC1 + PC2</p>",
               .df_to_html(as.data.frame(idx2), row_names = TRUE))
  }
  if (length(parts) == 0) return(NULL)
  
  narr <- .strip_html_narr(results$rcbd_pca_abt_indexscore)
  note <- if (nzchar(narr)) narr else
    "Index Score = raw PC score; Scaled Index = rescaled to [0, 1] for cross-treatment comparison. Higher scaled index indicates better performance on the combined PCs."
  
  div(class = "rch-tbl-card",
      div(class = "rch-tbl-header", icon("ranking-star"), " PCA Index Scores"),
      div(class = "rch-tbl-body", HTML(paste(parts, collapse = ""))),
      div(class = "rch-tbl-footer", note),
      .dl_button("pca_index_scores.csv"))
}

# ── PCA combined card (eigen + loadings + indices) ──────────────────────────
.build_pca_all_card <- function(results) {
  cards <- list(
    .build_pca_eigen_card(results),
    .build_pca_loadings_card(results),
    .build_pca_index_card(results)
  )
  labels <- c("Eigenvalues & Variance",
              "Variable Loadings",
              "Index Scores (PC1 / PC1+PC2)")
  keep <- !vapply(cards, is.null, logical(1))
  cards  <- cards[keep]; labels <- labels[keep]
  if (length(cards) == 0) return(NULL)
  
  sections <- mapply(function(card, lbl) {
    div(style = "margin-bottom:18px;",
        div(style = paste0("font-size:11px;font-weight:700;letter-spacing:.06em;",
                           "color:#560466;text-transform:uppercase;margin-bottom:4px;"), lbl),
        card)
  }, cards, labels, SIMPLIFY = FALSE)
  tagList(sections)
}

# ── Correlation matrix ──────────────────────────────────────────────────────
.build_correlation_card <- function(results, char = NULL) {
  cm <- results$rcbd_correlation_matrix
  # Fallback: if Part A didn't compute it, try to derive from data_corr now
  if ((is.null(cm) || !is.data.frame(cm) || nrow(cm) == 0)) {
    raw <- results$data_corr
    if (!is.null(raw) && is.data.frame(raw) && ncol(raw) >= 2) {
      cdec <- suppressWarnings(as.integer(results$sel_digit %||% results$decimals))
      if (length(cdec) == 0 || is.na(cdec)) cdec <- 3L
      cm <- tryCatch(
        as.data.frame(round(stats::cor(raw, use = "pairwise.complete.obs"), cdec)),
        error = function(e) NULL
      )
    }
  }
  if (is.null(cm) || !is.data.frame(cm) || nrow(cm) == 0) return(NULL)
  
  display <- cm
  if (!is.null(char) && nzchar(char)) {
    hit <- .match_pca_rows(display, char)
    if (!is.null(hit) && length(hit) > 0) {
      display <- display[hit, , drop = FALSE]
    }
  }
  
  html <- .df_to_html(as.data.frame(display),
                      caption = "Correlation Matrix \u2014 Response Variables",
                      row_names = TRUE)
  note <- "Pearson correlation among selected response variables (pairwise complete observations). Values near +1 / -1 indicate strong linear association; near 0 indicates none."
  
  div(class = "rch-tbl-card",
      div(class = "rch-tbl-header", icon("link"), " Correlation Matrix"),
      div(class = "rch-tbl-body", HTML(html)),
      div(class = "rch-tbl-footer", note),
      .dl_button("correlation_matrix.csv"))
}

# ── Combined "multivariate" card (everything together) ──────────────────────
.build_multivariate_all_card <- function(results) {
  cards <- list(
    .build_manova_card(results),
    .build_pca_eigen_card(results),
    .build_pca_loadings_card(results),
    .build_pca_index_card(results),
    .build_correlation_card(results)
  )
  labels <- c("MANOVA",
              "PCA \u2014 Eigenvalues",
              "PCA \u2014 Loadings",
              "PCA \u2014 Index Scores",
              "Correlation Matrix")
  keep <- !vapply(cards, is.null, logical(1))
  cards  <- cards[keep]; labels <- labels[keep]
  if (length(cards) == 0) {
    return(div(class = "rch-tbl-card",
               div(class = "rch-tbl-header", icon("vials"), " Multivariate Results"),
               div(class = "rch-tbl-body",
                   tags$p(style = "color:#888;font-size:12.5px;",
                          paste("No multivariate results yet. Open the",
                                tags$b("Multivariate"),
                                "tab and click",
                                tags$b("MANOVA"), "and/or",
                                tags$b("PCA"),
                                "to compute these tables, then ask me again.")))))
  }
  
  sections <- mapply(function(card, lbl) {
    div(style = "margin-bottom:18px;",
        div(style = paste0("font-size:11px;font-weight:700;letter-spacing:.06em;",
                           "color:#560466;text-transform:uppercase;margin-bottom:4px;"), lbl),
        card)
  }, cards, labels, SIMPLIFY = FALSE)
  tagList(sections)
}


# ─────────────────────────────────────────────────────────────────────────────
# Local fallback
# ─────────────────────────────────────────────────────────────────────────────
if (!exists("%||%")) `%||%` <- function(a, b) if (!is.null(a)) a else b