# Finalisation Checklist
Deadline: 2026-06-12 at 17:00 on ILIAS

Each item is assigned to the contributor responsible for that model section or task.
Items marked **[ALL]** require input from all three members before the section can be closed.

---

## Critical — must fix before submission

### Emanuel Lemma

- [x] **GLM Binomial: add coefficient table.** Added `kable` with term,
  estimate, OR = exp(estimate), 95% CI lower and upper, and p-value before the
  OR plot.
- [x] **GLM Binomial: quantify coefficient interpretations.** All OR values
  now stated with exact numbers via inline R (e.g. ground-source: OR computed
  from model, building_typehouse p-value stated).
- [x] **EDA: add a chapter-level conclusion paragraph.** 5-sentence
  client-facing summary added after the third EDA plot.
- [x] **GAM: refit using same predictors as extended LM.** New formula uses
  `s(HDD, by=building_type)` + `s(HDD, by=heatpump_type)` + sunshine hours +
  humidity + living area + n_residents. Model saved as `gam_model_v2.rds`.
  Full section rewrite with updated smooth/parametric tables, new prediction
  plots (HDD-by-building, HDD-by-heatpump), and real-number interpretations.
- [x] **CV script updated and verified.** `R/07_model_comparison_cv.R`
  updated to use the new GAM formula and correct model name
  "GAM (same predictors)". Re-run confirms LM: MAE 10.96 / R² 0.274,
  GAM: MAE 10.56 / R² 0.310.
- [x] **Model Comparison — regression section.** Prose updated: predictor sets
  are identical, inline R model name lookup fixed (was silently failing),
  improvement figures corrected to 3.6% MAE gain and 3.6pp R² gain.
- [x] **Conclusions section written.** 10 non-technical sentences covering
  key drivers (HDD non-linearity for houses, −13% ground-source, +14% per
  resident), best models, and client takeaway.

- [ ] **GLM Binomial: confirm new threshold definition with Tharrmeehan.**
  The threshold was changed from household median to **per-household 75th
  percentile**. Tharrmeehan must refit the SVM to this same definition so that
  the classification comparison is valid. Agree on which threshold to use as
  the shared standard and confirm in a team message.
- [ ] **Regenerate `cv_class_results.rds` once SVM is refitted.** The current
  file was computed before the GLM Binomial threshold change and still uses the
  global 75th percentile for both models. Once Tharrmeehan refits the SVM to
  the per-household 75th percentile, rerun the classification CV script so both
  rows use the same threshold. The table caption already flags this as stale.

---

### Tharrmeehan Krishnathasan

- [ ] **SVM: refit the model with the per-household 75th percentile
  threshold.** *(Description updated — no longer household median.)* The GLM
  Binomial now defines high consumption as each household's own 75th percentile
  of `log_kWh_total`. Refit the SVM to the same definition: derive
  `high_consumption` per household using `group_by(Household_ID) |>
  summarise(hh_q75 = quantile(log_kWh_total, 0.75))`, join back, and use that
  binary variable as the SVM response. Overwrite `models/svm/heapo_svm.rds`
  and update all performance metrics and the Data Preparation subsection.
- [x] **SVM: fix the doParallel reference in the report.** No reference to
  doParallel or parallelisation remains anywhere in the report.
- [x] **LM: replace RMSE on log scale with MAE in kWh.** The comparison table
  already uses `CV MAE (kWh/day)` and the prose references MAE throughout.

---

### Raphaël Spagolla

- [ ] **Neural Network: add AUC.** The ROC curve is plotted but the AUC value
  is never stated in the NN section itself. The AUC is already computed inline
  in the Model Comparison chunk (`nn_auc`). Report it explicitly in the NN
  performance summary and in the text.
- [ ] **Neural Network: update the CV paragraph.** The Conclusion subsection
  currently states that "more robust evaluation methods, such as
  cross-validation, could provide more stable performance estimates". Replace
  this with a sentence pointing to the Model Comparison section, e.g.
  "Cross-validation results for the comparable models are reported in the Model
  Comparison section; the Neural Network is evaluated on a held-out 80/20
  split due to the pre-computed prediction format."
- [ ] **Neural Network: add a feature importance note.** Add a short paragraph
  acknowledging that nnet weights are not directly interpretable as
  coefficients, explaining why (non-linear transformations between layers), and
  noting that model performance in the comparison section provides the practical
  evaluation instead.
- [ ] **GLM Poisson: write the standalone CV sub-section.** The
  `## GLM Poisson (Standalone)` sub-section in Model Comparison is currently
  empty. Add 5-fold CV RMSE on `n_high_days`, present the result, and state
  explicitly that the Poisson model targets a different aggregation level
  (monthly count) and cannot be placed in the classification table.

---

### All three members

- [ ] **Classification CV: regenerate once SVM threshold is fixed.** See
  Emanuel's open item above. All three must agree on the shared threshold
  before regenerating.
- [ ] **Check all plots have a written explanation.** Scan the compiled HTML
  and flag any figure not followed by an interpretation paragraph. Pay
  particular attention to SVM and NN diagnostic plots.
- [ ] **Check for em dashes and semicolons in prose.** The report convention
  forbids em dashes (—), en dashes (–), and semicolons in running body text.
  Note: the AI Usage paragraph currently contains em dashes and must be fixed
  before submission. Each contributor checks their own sections.
- [ ] **Verify building_type levels in the Variable Description table.** The
  table in the Introduction still lists "flat, house, or other". The data and
  all model outputs show only two levels: "appartment" (note the spelling) and
  "house". Correct the table entry.

---

## Submission checklist — complete before zipping

- [ ] **Create README.md.** List: raw data location and download instructions,
  the .Rmd source file, the compiled PDF or HTML, all model .rds files, and the
  data processing pipeline scripts.
- [ ] **Upload the team agreement to ILIAS.** Pass/fail administrative
  requirement — separate ILIAS folder from the main submission.
- [ ] **Compile report.Rmd to PDF and verify page count.** Hard limit: 30
  pages. Delete `report/report_cache/` first for a clean knit. If over 30
  pages, shorten diagnostic sections (e.g. reduce the 8-panel GAM smooth plot
  to 4 panels, or trim the SVM diagnostics table).
- [ ] **Confirm zip naming convention.**
  `FamilyNameStudent1_FamilyNameStudent2_FamilyNameStudent3_MatriculationNumber.zip`
- [ ] **Confirm zip contents.** (1) raw data or download instructions,
  (2) compiled PDF or HTML, (3) `report/report.Rmd`, (4) README.md.
- [ ] **Submit on ILIAS before 17:00.** Upload at least one day early.
