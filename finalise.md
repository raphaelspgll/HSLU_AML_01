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
- [x] **Threshold consistency verified.** Full audit (2026-05-25) confirmed
  that all four models (GLM Binomial, NN, SVM, and the CV classification script)
  define `high_consumption` identically: per-household 75th percentile of
  `log_kWh_total` via `quantile(0.75)` per `Household_ID`. All prose references
  are consistent. No changes were needed.
- [x] **Punctuation fixes — Intro, EDA, GLM Binomial, GAM.** All em dashes,
  semicolons, and sentence-connecting colons replaced with plain sentences.
  building_type table corrected to "appartment" or "house".
  Changes are on branch `feature/punctuation-fixes` — merge PR before submission.

---

### Tharrmeehan Krishnathasan

- [x] **SVM: refit the model with the per-household 75th percentile threshold.**
  Confirmed (2026-05-25): the report code computes `hh_q75` per `Household_ID`
  using `quantile(0.75)` and loads `heapo_svm_hh_q75.rds`. The old global
  threshold model is no longer referenced anywhere.
- [x] **SVM: fix the doParallel reference in the report.** No reference to
  doParallel or parallelisation remains anywhere in the report.
- [x] **LM: replace RMSE on log scale with MAE in kWh.** The comparison table
  already uses `CV MAE (kWh/day)` and the prose references MAE throughout.
- [ ] **Fix punctuation in LM and SVM sections.** Check for and remove em
  dashes (—), en dashes (–), semicolons, and sentence-connecting colons in
  running prose. Each contributor is responsible for their own sections.

---

### Raphaël Spagolla

- [x] **Neural Network: add AUC.** AUC is stated in the performance table and
  in the prose of the NN section.
- [x] **Neural Network: update the CV paragraph.** Conclusion now points to the
  Model Comparison section instead of suggesting future CV work.
- [x] **Neural Network: add a feature importance note.** Paragraph added
  explaining that nnet weights are not directly interpretable and that model
  performance metrics serve as the practical evaluation.
- [x] **GLM Poisson: write the standalone CV sub-section.** 5-fold CV RMSE on
  `n_high_days` is computed inline in the Model Comparison section, result
  presented, and incomparability with classification models stated explicitly.
- [ ] **Fix punctuation in Poisson and NN sections.** Check for and remove em
  dashes (—), en dashes (–), semicolons, and sentence-connecting colons in
  running prose. Each contributor is responsible for their own sections.

---

### All three members

- [ ] **Merge `feature/punctuation-fixes` PR.** Emanuel's punctuation and
  building_type table fixes are on this branch. Merge before final knit.
- [ ] **Check all plots have a written explanation.** Scan the compiled HTML
  and flag any figure not followed by an interpretation paragraph. Pay
  particular attention to SVM and NN diagnostic plots.
- [ ] **Regenerate `cv_class_results.rds`.** Threshold is confirmed consistent
  across all models. Run `R/08_model_comparison_cv_classification.R` to
  regenerate a clean version and confirm the classification CV table renders
  correctly with all three models.

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
