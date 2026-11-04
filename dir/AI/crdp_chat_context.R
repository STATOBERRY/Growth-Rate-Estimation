crdp_chat_context <- function(results_rv,
                              submit_rv,
                              file_rv,
                              treatment_input,
                              selvar_input,
                              mcp_input,
                              level_input,
                              padj_input,
                              tutorial_content_rv = reactive(NULL)) {
  reactive({
    
    # ── Gate 1: data not uploaded ─────────────────────────────────────────────
    if (is.null(file_rv())) {
      return(NULL)
    }
    
    # ── Gate 2: analysis not yet run ─────────────────────────────────────────
    if (!isTRUE(submit_rv())) {
      return(NULL)
    }
    
    # ── Gate 3: results not ready ─────────────────────────────────────────────
    results <- tryCatch(results_rv(), error = function(e) NULL)
    if (is.null(results) || length(results) == 0) {
      return(NULL)
    }
    
    # ── Helper: safe number formatting ────────────────────────────────────────
    fmt <- function(x, d = 4) {
      x <- suppressWarnings(as.numeric(x))
      if (is.na(x)) return("NA")
      formatC(x, format = "f", digits = d)
    }
    
    # ── Extract user inputs ───────────────────────────────────────────────────
    treatment_col <- tryCatch(treatment_input(), error = function(e) "unknown")
    selvar_cols   <- tryCatch(selvar_input(),   error = function(e) character(0))
    mcp_method    <- tryCatch(mcp_input(),      error = function(e) "LSD")
    sig_level     <- tryCatch(level_input(),    error = function(e) "0.05")
    padj_method   <- tryCatch(padj_input(),     error = function(e) "none")
    
    # ── Extract results from crdp_table() ────────────────────────────────────
    no_trt       <- results$no_trt
    all_equal    <- results$all_equal
    r_counts     <- results$r
    crdp_char    <- results$crdp_char      # character/variable names
    crdp_star    <- results$crdp_star      # significance stars per character
    crdp_pval    <- results$crdp_pval      # p-values per character
    crdp_groups  <- results$crdp_groups    # grouping letters per character
    trt_means    <- results$crdp_trt_means # treatment means matrix
    allow_grouping <- results$allow_letter_grouping
    
    # ── Dataset overview ──────────────────────────────────────────────────────
    df        <- file_rv()
    n_rows    <- nrow(df)
    n_cols    <- ncol(df)
    col_names <- paste(names(df), collapse = ", ")
    
    dataset_block <- paste0(
      "=== DATASET OVERVIEW ===\n",
      "Rows: ", n_rows, " | Columns: ", n_cols, "\n",
      "All columns: ", col_names, "\n",
      "Treatment column: ", treatment_col, "\n",
      "Response variable(s): ", paste(selvar_cols, collapse = ", "), "\n",
      "Number of treatments: ", no_trt, "\n",
      if (isTRUE(all_equal)) {
        paste0("Replication: Equal (", unique(as.numeric(r_counts)), " replications per treatment)\n")
      } else {
        paste0("Replication: Unequal - ", paste(names(r_counts), r_counts, sep = "=", collapse = ", "), "\n")
      }
    )
    
    # ── ANOVA Settings Block ──────────────────────────────────────────────────
    mcp_full <- switch(mcp_method,
                       "LSD"   = "Least Significant Difference (LSD)",
                       "TUKEY" = "Tukey's Honestly Significant Difference (HSD)",
                       "DMRT"  = "Duncan's Multiple Range Test (DMRT)",
                       mcp_method
    )
    
    padj_text <- if (mcp_method == "LSD" && !is.null(padj_method) && padj_method != "none") {
      paste0("P-value adjustment method: ", padj_method, "\n")
    } else {
      ""
    }
    
    settings_block <- paste0(
      "\n=== ANALYSIS SETTINGS ===\n",
      "Design: Completely Randomized Design (CRD / One-Way ANOVA)\n",
      "Multiple comparison procedure: ", mcp_full, "\n",
      "Level of significance (α): ", sig_level, "\n",
      padj_text
    )
    
    # ── Significance Summary Block ────────────────────────────────────────────
    sig_block <- "\n=== SIGNIFICANCE SUMMARY (per character) ===\n"
    
    if (!is.null(crdp_char) && length(crdp_char) > 0) {
      for (i in seq_along(crdp_char)) {
        char_name <- crdp_char[i]
        star      <- if (!is.null(crdp_star) && length(crdp_star) >= i) crdp_star[i] else "NA"
        pval      <- if (!is.null(crdp_pval) && length(crdp_pval) >= i) {
          tryCatch(fmt(crdp_pval[i], 4), error = function(e) "NA")
        } else "NA"
        
        sig_status <- if (star %in% c("*", "**")) "SIGNIFICANT" else "NOT SIGNIFICANT"
        
        sig_block <- paste0(
          sig_block,
          "  ", char_name, ": p = ", pval, "  ", star,
          "  [", sig_status, "]\n"
        )
      }
    }
    
    # ── Treatment Means Block ─────────────────────────────────────────────────
    means_block <- ""
    
    if (!is.null(trt_means) && is.data.frame(trt_means) && nrow(trt_means) > 0) {
      means_block <- paste0(
        "\n=== TREATMENT MEANS ===\n",
        "(Raw means for each treatment × character combination)\n\n"
      )
      
      for (i in 1:nrow(trt_means)) {
        trt_name <- rownames(trt_means)[i]
        vals <- sapply(colnames(trt_means), function(col) {
          v <- tryCatch(as.numeric(trt_means[i, col]), warning = function(w) NA)
          if (is.na(v)) trt_means[i, col] else fmt(v, 3)
        })
        
        means_block <- paste0(
          means_block,
          "  Treatment: ", trt_name, "\n",
          paste0("    ", names(vals), ": ", vals, collapse = "\n"),
          "\n\n"
        )
      }
    }
    
    # ── Letter Grouping Block ─────────────────────────────────────────────────
    grouping_block <- ""
    
    if (!is.null(crdp_groups) && !is.null(crdp_char)) {
      grouping_block <- paste0(
        "\n=== LETTER GROUPING (MCP Results) ===\n",
        "Treatments sharing the same letter grouping are NOT significantly different.\n\n"
      )
      
      if (isTRUE(allow_grouping)) {
        # crdp_groups is a vector of group labels per character
        for (i in seq_along(crdp_char)) {
          char_name <- crdp_char[i]
          grp <- if (is.matrix(crdp_groups) || is.data.frame(crdp_groups)) {
            tryCatch(crdp_groups[, i], error = function(e) NULL)
          } else if (is.list(crdp_groups)) {
            crdp_groups[[i]]
          } else {
            NULL
          }
          
          if (!is.null(grp) && length(grp) > 0) {
            grouping_block <- paste0(
              grouping_block,
              "  Character: ", char_name, "\n",
              paste0("    ", names(grp), ": ", grp, collapse = "\n"),
              "\n\n"
            )
          }
        }
      } else {
        grouping_block <- paste0(
          grouping_block,
          "  Note: Letter grouping is disabled because the number of treatments exceeds 80.\n",
          "  Please refer to the Individual ANOVA tab for grouping details.\n"
        )
      }
    }
    
    ## df1 = crdp_final_table: has treatment means, grouping, F, p, CD, MSE, SE(m), SE(d), CV, Cohen's F
    tables_block <- ""
    df1 <- results$df1
    if (!is.null(df1) && is.data.frame(df1) && nrow(df1) > 0) {
      tables_block <- paste0(tables_block, "\n=== FULL RESULTS TABLE (with CD, MSE, SE, CV) ===\n")
      tables_block <- paste0(tables_block, "Row | ", paste(colnames(df1), collapse = " | "), "\n")
      for (r in seq_len(nrow(df1))) {
        rn <- rownames(df1)[r]
        row_vals <- sapply(seq_len(ncol(df1)), function(j) {
          val <- df1[r, j]
          if (is.na(val)) "-" else as.character(val)
        })
        tables_block <- paste0(tables_block, rn, " | ", paste(row_vals, collapse = " | "), "\n")
      }
    }
    
    ## df4 = ANOVA table (SoV, DF, MS per character)
    df4 <- results$df4
    if (!is.null(df4) && is.data.frame(df4) && nrow(df4) > 0) {
      tables_block <- paste0(tables_block, "\n=== ANOVA TABLE ===\n")
      tables_block <- paste0(tables_block, paste(colnames(df4), collapse = " | "), "\n")
      for (r in seq_len(nrow(df4))) {
        row_vals <- sapply(seq_len(ncol(df4)), function(j) as.character(df4[r, j]))
        tables_block <- paste0(tables_block, paste(row_vals, collapse = " | "), "\n")
      }
    }
    
    ## inject per-character ANOVA statistics (CD, CV, MSE, SE, etc.)
    indv_stats_block <- ""
    indv <- results$individual_anova
    if (!is.null(indv) && is.list(indv) && length(indv) > 0) {
      indv_stats_block <- "\n=== INDIVIDUAL CHARACTER ANALYSIS ===\n"
      
      for (nm in names(indv)) {
        entry <- indv[[nm]]
        if (is.null(entry)) next
        
        indv_stats_block <- paste0(indv_stats_block, "\n--- ", nm, " ---\n")
        
        # ANOVA table
        if (!is.null(entry$ANOVA) && is.data.frame(entry$ANOVA)) {
          anova_df <- entry$ANOVA
          for (r in seq_len(nrow(anova_df))) {
            row_vals <- paste(colnames(anova_df), "=", sapply(anova_df[r, ], as.character), collapse = ", ")
            indv_stats_block <- paste0(indv_stats_block, "  ", row_vals, "\n")
          }
        }
        
        # Treatment means + grouping
        trt_tbl <- entry$Summary$Treatment
        if (!is.null(trt_tbl) && is.data.frame(trt_tbl) && nrow(trt_tbl) > 0) {
          indv_stats_block <- paste0(indv_stats_block, "  Treatment Means:\n")
          for (r in seq_len(nrow(trt_tbl))) {
            row_vals <- paste(colnames(trt_tbl), "=", sapply(trt_tbl[r, ], as.character), collapse = ", ")
            indv_stats_block <- paste0(indv_stats_block, "    ", row_vals, "\n")
          }
        }
        
        # Statistics: CD, CV, MSE, SE(m), SE(d)
        stat_tbl <- entry$stat_summary$Treatment
        if (!is.null(stat_tbl) && is.data.frame(stat_tbl) && nrow(stat_tbl) > 0) {
          stat_pairs <- paste(stat_tbl$Stat, "=", stat_tbl$Value, collapse = ", ")
          indv_stats_block <- paste0(indv_stats_block, "  Statistics: ", stat_pairs, "\n")
        }
      }
    }
    
    # ── Helper: data.frame -> piped plain-text block ─────────────────────────
    .df_to_pipe <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return("")
      out <- paste(c("Row", colnames(df)), collapse = " | ")
      out <- paste0(out, "\n")
      rn <- rownames(df)
      for (r in seq_len(nrow(df))) {
        row_label <- if (!is.null(rn)) rn[r] else as.character(r)
        vals <- sapply(seq_len(ncol(df)), function(j) {
          v <- df[r, j]
          if (is.na(v)) "-" else as.character(v)
        })
        out <- paste0(out, row_label, " | ", paste(vals, collapse = " | "), "\n")
      }
      out
    }
    
    .strip_html_ctx <- function(txt) {
      if (is.null(txt) || !nzchar(txt)) return("")
      txt <- gsub("<br\\s*/?>", " ",  txt, ignore.case = TRUE, perl = TRUE)
      txt <- gsub("</p>",       "\n", txt, ignore.case = TRUE)
      txt <- gsub("<[^>]+>",    "",   txt, perl = TRUE)
      txt <- gsub("&nbsp;",     " ",  txt, fixed = TRUE)
      txt <- gsub("&amp;",      "&",  txt, fixed = TRUE)
      txt <- gsub("\\s+",       " ",  txt)
      trimws(txt)
    }
    
    # ── Multivariate Block (MANOVA + PCA + Correlation) ──────────────────────
    multivar_block <- ""
    
    manova_df <- results$crdp_manova_table
    if (!is.null(manova_df) && is.data.frame(manova_df) && nrow(manova_df) > 0) {
      multivar_block <- paste0(multivar_block,
                               "\n=== MANOVA TABLE (multivariate ANOVA across all response variables) ===\n",
                               .df_to_pipe(manova_df))
      pillai <- results$crdp_manova_pillai
      F_val  <- results$crdp_manova_F
      pval   <- results$crdp_manova_pval
      eta_sq <- results$crdp_manova_eta_sq
      multivar_block <- paste0(multivar_block,
                               "Pillai trace: ", pillai %||% "NA",
                               " | F: ",         F_val  %||% "NA",
                               " | p-value: ",   pval   %||% "NA",
                               " | eta-squared: ", eta_sq %||% "NA", "\n")
      if (!is.null(pval)) {
        pnum <- suppressWarnings(as.numeric(pval))
        if (!is.na(pnum)) {
          verdict <- if (pnum <= 0.05) "SIGNIFICANT (treatments differ in multivariate response space)"
          else "NOT SIGNIFICANT (no joint multivariate treatment effect)"
          multivar_block <- paste0(multivar_block, "Verdict: ", verdict, "\n")
        }
      }
    }
    
    eig <- results$crdp_pca_eigen
    if (!is.null(eig) && is.data.frame(eig) && nrow(eig) > 0) {
      multivar_block <- paste0(multivar_block,
                               "\n=== PCA EIGENVALUES & VARIANCE EXPLAINED ===\n",
                               .df_to_pipe(eig))
      e1 <- results$crdp_pca_e1
      e2 <- results$crdp_pca_e2
      if (!is.null(e1) || !is.null(e2)) {
        multivar_block <- paste0(multivar_block,
                                 "PC1 variance %: ", e1 %||% "NA",
                                 " | PC2 variance %: ", e2 %||% "NA", "\n")
      }
      narr <- .strip_html_ctx(results$crdp_pca_abt_eigen)
      if (nzchar(narr))
        multivar_block <- paste0(multivar_block, "Interpretation note: ", narr, "\n")
    }
    
    load_df <- results$crdp_pca_loadings
    if (!is.null(load_df) && is.data.frame(load_df) && nrow(load_df) > 0) {
      multivar_block <- paste0(multivar_block,
                               "\n=== PCA LOADINGS (variable contributions to each PC) ===\n",
                               .df_to_pipe(load_df))
      narr <- .strip_html_ctx(results$crdp_pca_abt_loadings)
      if (nzchar(narr))
        multivar_block <- paste0(multivar_block, "Loadings note: ", narr, "\n")
    }
    
    idx1 <- results$crdp_pca_index1
    idx2 <- results$crdp_pca_index2
    if ((!is.null(idx1) && is.data.frame(idx1) && nrow(idx1) > 0) ||
        (!is.null(idx2) && is.data.frame(idx2) && nrow(idx2) > 0)) {
      multivar_block <- paste0(multivar_block,
                               "\n=== PCA INDEX SCORES (per treatment) ===\n")
      if (!is.null(idx1) && nrow(idx1) > 0) {
        multivar_block <- paste0(multivar_block,
                                 "--- PC1-based index ---\n", .df_to_pipe(idx1))
      }
      if (!is.null(idx2) && nrow(idx2) > 0) {
        multivar_block <- paste0(multivar_block,
                                 "--- PC1+PC2-based index ---\n", .df_to_pipe(idx2))
      }
      narr <- .strip_html_ctx(results$crdp_pca_abt_indexscore)
      if (nzchar(narr))
        multivar_block <- paste0(multivar_block, "Index score note: ", narr, "\n")
    }
    
    cm <- results$crdp_correlation_matrix
    if (is.null(cm) || !is.data.frame(cm) || nrow(cm) == 0) {
      raw <- results$data_corr
      if (!is.null(raw) && is.data.frame(raw) && ncol(raw) >= 2) {
        cm <- tryCatch(
          as.data.frame(round(stats::cor(raw, use = "pairwise.complete.obs"), 3)),
          error = function(e) NULL
        )
      }
    }
    if (!is.null(cm) && is.data.frame(cm) && nrow(cm) > 0) {
      multivar_block <- paste0(multivar_block,
                               "\n=== CORRELATION MATRIX (Pearson, pairwise complete obs) ===\n",
                               .df_to_pipe(cm))
    }
    
    
    # ── Tutorial Content ──────────────────────────────────────────────────────
    tutorial_content <- tryCatch(tutorial_content_rv(), error = function(e) NULL)
    tutorial_block   <- ""
    
    if (!is.null(tutorial_content) && nchar(tutorial_content) > 0) {
      tutorial_block <- paste0(
        "\n\n=== RAISINS CRD TUTORIAL ===\n",
        "Use this reference to answer conceptual questions about:\n",
        "- What ANOVA and CRD are and how to interpret results\n",
        "- Multiple comparison procedures (LSD, TUKEY, DMRT)\n",
        "- How to read treatment means and significance stars\n",
        "- Letter grouping interpretation\n",
        "- Data formatting requirements for RAISINS CRD\n\n",
        tutorial_content, "\n"
      )
    }
    
    # ── Assemble full context string ──────────────────────────────────────────
    paste0(
      dataset_block,
      settings_block,
      sig_block,
      means_block,
      grouping_block,
      tables_block,
      multivar_block,
      tutorial_block,
      
      "\n\n=== INSTRUCTIONS FOR ASSISTANT ===\n",
      "You are RA-One, a specialist assistant for CRD (One-Way ANOVA) analysis in RAISINS.\n",
      "STRICT RULES:\n",
      "1. Answer ONLY from the results, data blocks.\n",
      "2. Do NOT invent numbers, p-values, or group letters not present in the data blocks. If a value is missing or not computed, say so honestly.\n",
      "3. When a user asks for CD, CV, MSE, SE(m), SE(d), or similar statistics:\n",
      "  - F stat, CD, MSE, SE(m), SE(d), CV(%), Cohen's F values are given in the df1, take the value from df1\n",
      "4. Keep answers concise and cite actual values from the data blocks.\n",
      "5. For conceptual questions (e.g. 'what is LSD test?'),\n",
      "   answer only from the tutorial block above.\n",
      "6. Don't add unwanted line break in the reply.\n",
      "7. For result questions, cite the exact values from the data blocks.\n\n",
      "8. When producing a numbered list, write the items as consecutive lines\n",
      "   (one '\\n' between items, NOT a blank line).  Do NOT restart numbering\n",
      "   at 1 for every item.  Example of the correct format:\n",
      "     1. First point.\n",
      "     2. Second point.\n",
      "     3. Third point.\n",
      "When a user asks for CD, CV, MSE, SE(m), SE(d), or similar statistics:\n",
      "  - Present them in a markdown downloadable table\n",
      "  - Use the exact values from the data; do NOT say 'not provided' if the values exist in any block above\n",
      "  - If asked about a specific character, extract its column from df1/df3/df4\n",
      "  - The ANOVA TABLE has F and MS values per character\n",
      "  - The FULL RESULTS TABLE has means, grouping, significance, F stat, CD, MSE, SE(m), SE(d), CV(%), Cohen's F\n",
      "  - The STATISTICS TABLE has CD, CV, MSE, SE(m), SE(d) per character\n\n",
      "  - Cross check with the result tables and give the answer",
      
      "When a user asks to 'interpret results', summarise:\n",
      "  1. Number of treatments and replication structure\n",
      "  2. Characters studied and their significance status (p-value and star)\n",
      "  3. Which characters are significant vs non-significant\n",
      "  4. Treatment with highest and lowest mean for each significant character\n",
      "  5. Letter grouping pattern (which treatments are on par)\n\n",
      
      "  - The table rows include: F stat, p value, CD, MSE, SE(m), SE(d), CV(%), Cohen's F\n",
      "  - Each column is a character/variable name (yield, Obs1, etc.)\n",
      "  - Present values in a markdown table if multiple characters are involved\n",
      "  - NEVER say 'not provided' or 'not available' if the value exists in the FULL RESULTS TABLE\n",
      "  - If CD shows '-' it means CD is not applicable (non-significant character)\n\n",
      
      "Multivariate results handling (MANOVA, PCA, Correlation):\n",
      "  - MANOVA block (if present) tests joint treatment effect across all response variables. Report Pillai trace, F, p-value, eta-squared and the SIGNIFICANT/NOT SIGNIFICANT verdict.\n",
      "  - PCA EIGENVALUES block lists each PC with eigenvalue, % variance, cumulative variance. Components with eigenvalue > 1 are typically retained.\n",
      "  - PCA LOADINGS block shows variable contributions to PC1/PC2. Larger |loading| => stronger contribution. Same-sign loadings on a PC indicate variables that move together.\n",
      "  - PCA INDEX SCORES block has per-treatment PC1 and PC1+PC2 indices, each with a raw score and a 0-1 scaled version. Higher scaled index = better overall performance on those PCs.\n",
      "  - CORRELATION MATRIX block is a Pearson matrix of selected response variables. Cite the actual r values.\n",
      "  - If a multivariate block is missing, say the user must click the MANOVA or PCA button on the Multivariate tab first; do NOT fabricate values.\n\n",
      
      "How to report in a paper:\n",
      "  - Mention: One-Way ANOVA (CRD), number of treatments, replications\n",
      "  - State significant characters with F-statistic, p-value, and MCP used\n",
      "  - Report treatment means with grouping letters in a table\n",
      "  - Cite RAISINS software: Hisham et al. (2025), R Core Team (2024)\n\n",
      
      "Key reference:\n",
      "  * p < 0.05 \u2192 *significant; p < 0.01 \u2192 **highly significant; NS \u2192 not significant\n",
      "  * Treatments sharing the same grouping letter are not significantly different\n",
      "  * LSD: liberal; TUKEY: conservative; DMRT: intermediate\n",
      "  * Cohen's F < 0.1 = very small; < 0.25 = small; < 0.4 = medium; \u2265 0.4 = large\n"
    )
    
  })
}