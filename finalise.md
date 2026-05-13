# Finalisation Checklist
Deadline: 2026-06-12 at 17:00 on ILIAS

Each item is assigned to the contributor responsible for that model section or task.
Items marked **[ALL]** require input from all three members before the section can be closed.

---

## Critical — must fix before submission

### Emanuel Lemma

- [ ] **GLM Binomial: add coefficient table.** Before the odds ratio plot, add a `kable` showing term, estimate, OR = exp(estimate), 95% CI lower and upper, and p-value. This is required because the text references a table that does not exist in the rendered output.
- [ ] **GLM Binomial: quantify coefficient interpretations.** The prose currently says "houses have substantially higher odds" without stating the actual OR value. Add the number for every predictor discussed (e.g. "OR = 3.2, meaning houses have 220% higher odds of high consumption than flats").
- [ ] **GLM Binomial: confirm response definition with Tharrmeehan.** The Binomial model uses the household-level median (50th percentile per household) as the high-consumption threshold. This is the agreed definition going forward. Tharrmeehan will refit the SVM to match.
- [ ] **EDA: add a chapter-level conclusion paragraph.** After the third plot (consumption by building type), add a standalone paragraph summarising what the three plots collectively reveal and how this motivates the modelling strategy. This is required by the convention that every chapter must end with a brief conclusion.

### Tharrmeehan Krishnathasan

- [~] **SVM threshold decision — keeping 25%.** The SVM stays on the global 75th percentile threshold (top 25%). The GLM Binomial uses the household median instead — their individual sections use different thresholds. The CV comparison script (`R/08_model_comparison_cv_classification.R`) uses the 25% threshold for both models so the comparison table is internally consistent.
- [X] **SVM: fix the doParallel reference in the report.** Line ~1355 of report.Rmd states that cross-validation is "parallelised across cores via doParallel". doParallel was removed from renv.lock when the SVM was refactored to e1071::tune(). Replace this sentence with an accurate description of e1071::tune() and its built-in grid search.
- [X] **LM: replace RMSE on log scale with MAE in kWh.** The internal comparison table (simple MLR vs extended MLR) uses RMSE on the log scale. The agreed convention for continuous response models is MAE in kWh and R². Recompute and replace the RMSE column with MAE in kWh. Update the surrounding prose accordingly.

### Raphaël Spagolla

- [ ] **Neural Network: add AUC.** The ROC curve is plotted but the AUC value is never stated anywhere in the NN section. Compute it using the same `compute_auc` function already defined in the GLM Binomial chunk (or use ROCR::performance with "auc") and report it inline in the performance table and in the text.
- [ ] **Neural Network: update the CV paragraph.** The Conclusion subsection currently states that "more robust evaluation methods, such as cross-validation, could provide more stable performance estimates". Once CV is implemented in the Model Comparison section, replace this with a sentence pointing the reader there: e.g. "Cross-validation results for the Neural Network are reported in the Model Comparison section."
- [ ] **Neural Network: add a feature importance note.** The course requires coefficient interpretation for every model. Add a short paragraph acknowledging that nnet weights are not directly interpretable as coefficients, explaining why (high-dimensional, non-linear transformations between layers), and noting that model performance in the comparison section provides the practical evaluation instead.

### All three members

- [X] **5-fold CV — Regression (LM, GAM):** Implemented in `R/07_model_comparison_cv.R`. Outputs `models/cv_reg_results.rds`, loaded in the Model Comparison section.
- [X] **5-fold CV — Classification (GLM Binomial, SVM):** Implemented in `R/08_model_comparison_cv_classification.R`. Uses fixed SVM hyperparameters (C=100, σ=0.1429). Outputs `models/cv_class_results.rds`, loaded in the Model Comparison section. **Allow 30–60 min to run** (SVM on ~75k rows per fold).
- [ ] **5-fold CV — Neural Network (BLOCKER — requires Python retraining).** The NN predictions come from pre-saved files (`data_processed/splits/nn/`) produced by an external Python/Keras pipeline. Adding the NN to the CV table requires retraining five separate models — one per fold — and saving each fold's held-out predictions. This cannot be done from within R. The report currently notes this limitation in the Classification comparison section and treats the NN's single-split metrics as indicative. **Whoever owns the NN training script needs to: (1) expose the fold indices from `R/08_model_comparison_cv_classification.R` or replicate them (`set.seed(42)`, `caret::createFolds` on `high_consumption_int`, k=5), (2) retrain five NN models, (3) save fold predictions to `data_processed/splits/nn/nn_cv_pred_fold{1-5}.rds`, and (4) add a loading chunk to the report.**
- [ ] **5-fold CV — Poisson GLM (BLOCKER — different data granularity).** The Poisson model targets `n_high_days`, a monthly household-level aggregate. This is a fundamentally different unit of observation from the daily rows used by all other models. Sharing fold indices with the daily-level CV is not valid. The Poisson model needs its own CV loop on the monthly aggregated dataset (one row per household-month). Whoever owns this section should create `R/09_model_comparison_cv_poisson.R` following the same pattern as `07` and `08`, stratified on the monthly count outcome.

- [ ] **Write the Model Comparison section.** The section is currently an empty placeholder. Structure it as three sub-sections:
  1. **Regression (LM vs GAM):** 5-fold CV MAE in kWh and R² on `log_kWh_total`. Present as a two-row summary table. Discuss whether the GAM's flexibility translates into a meaningful MAE improvement over the LM.
  2. **Classification (GLM Binomial vs SVM):** 5-fold CV AUC and accuracy on `high_consumption` (global 75th percentile / top 25% threshold). Present as a two-row summary table. Discuss which model achieves the best AUC and whether the gain justifies the loss of interpretability. Note: NN cannot be included — see blocker note in the CV section above.
  3. **GLM Poisson (standalone):** 5-fold CV RMSE on `n_high_days`. This model targets a monthly count at a different aggregation level and cannot be placed in the classification table. Evaluate separately and state this limitation explicitly.
  Close with a paragraph discussing the interpretability vs performance trade-off across all models.

- [ ] **Write the Conclusions section.** 5–10 sentences. Non-technical and actionable. Cover: what the data shows about the drivers of electricity consumption, which model performed best and why, and what the client (energy supplier or grid operator) should take away. Do not use jargon. Do not repeat the model results in detail — summarise the story.

- [ ] **Write the AI Usage paragraph.** Required by the course. One paragraph covering: (1) how generative AI was used in this project (code generation, text drafting, concept explanations, debugging); (2) what was easy, what was hard, and what was impossible using AI; (3) what had to be critically evaluated or corrected when using AI-generated output.

- [ ] **Add an introduction to the Modelling chapter.** Before the Linear Model subsection, add a short paragraph (3–5 sentences) explaining: which models are fitted, which response variable each targets, and how the models will be compared. This gives the reader a map before they enter six sub-sections.

- [ ] **Verify building_type levels in the Variable Description table.** The table (Introduction section) lists three levels: "flat, house, or other". The data and all model outputs show only two levels: "appartment" (note the spelling) and "house". Correct the table and note the exact spelling.

---

## Submission checklist — complete before zipping

- [ ] **Create README.md.** The submission zip must contain a README describing every file. Create `README.md` in the project root listing: the raw data location and download instructions, the .Rmd source file, the compiled PDF or HTML, all model .rds files, and the data processing pipeline scripts.
- [ ] **Upload the team agreement to ILIAS.** Fill in and upload the team agreement form to the dedicated ILIAS folder (separate from the main submission). This is a pass/fail administrative requirement.
- [ ] **Compile report.Rmd to PDF and verify page count.** The hard limit is 30 pages. Render to PDF (or use print preview for HTML) and count the pages before zipping. If over 30 pages, shorten the diagnostic sections (e.g. reduce the number of GAM smooth plots or trim the SVM diagnostics table).
- [ ] **Confirm zip naming convention.** The zip must be named: `FamilyNameStudent1_FamilyNameStudent2_FamilyNameStudent3_MatriculationNumber.zip` — all in the order agreed by the team.
- [ ] **Confirm zip contents.** The zip must contain: (1) raw data or download instructions, (2) compiled PDF or HTML, (3) `report/report.Rmd` source file, (4) README.md. The standalone chapter Rmd files in `report/` (1.2, 1.3, 1.4, 1.5) are not the primary deliverable — decide as a team whether to include or exclude them, but do not submit them instead of the main report.
- [ ] **Submit on ILIAS before 17:00.** Email submissions are not accepted. Only the last uploaded version is graded. Upload at least one day early to avoid last-minute technical problems.

---

## Minor — polish and consistency

- [ ] **[ALL] Check all plots have a written explanation.** Every figure in the report must be followed by a paragraph that describes what is shown, interprets the finding, and explains what it means for the client. Scan the full compiled PDF and flag any orphaned plots.
- [ ] **[ALL] Check for em dashes and semicolons in prose.** The report convention forbids em dashes (—), en dashes (–), and semicolons in running body text. Each contributor checks their own sections and replaces any with plain sentences.
- [ ] **[ALL] Final knit check.** Compile report.Rmd from scratch (clear the knitr cache first: delete `report/report_cache/`) to confirm the full document renders without errors and all saved .rds files load correctly.
