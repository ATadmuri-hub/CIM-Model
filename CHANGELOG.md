# Changelog

All notable changes to the Calisthenics Integration Model (CIM) are recorded here. Versions refer to the NetLogo model and its accompanying R analysis pipeline.

## v6.4 (May 2026)

Final release accompanying the master's thesis.

### Added
- Reproducibility: pinned random seeds, so every reported result is reproducible bit-for-bit by re-running the named BehaviorSpace experiment. The full simulation budget is 14,110 runs (8,410 main pipeline plus 5,700 framework-generality runs).
- Configuration layer: a CSV configuration loader that re-calibrates the model to a new domain without editing code. Two calibrations are provided, an Istanbul calisthenics programme and a Berlin BAMF integration course.
- Second-domain test: the Berlin calibration is used to test how far the programme-design rankings transfer beyond community sport.
- Network analysis: structural analysis of the friendship graph (weak-tie bridging and edge betweenness) plus a static link-prediction validation.
- Robustness: global sensitivity analysis (PRCC), a motivation-decay bracket test, an open-population extension with arrivals and departures, a dose-response sweep over group composition, and an alternative-mechanism benchmark.
- An interactive replay dashboard for exploring scenario outcomes.

### Changed
- Scenario set expanded to 22 policy levers plus 1 contextual variable (23 conditions in total), covering infrastructure, support services, targeting accuracy, group composition, and seasonal conditions.
- Statistical analysis uses family-wise Welch t-tests with Holm correction, Cox survival analysis with a disclosed proportional-hazards check, difference-in-differences, and randomization inference.
- Documentation aligned across the README, the reproducibility guide, the ODD protocol, and the citation metadata.

## v6.1 to v6.3 (2025 to early 2026)

Model development: core agent behaviour (attendance, motivation dynamics, language acquisition, social-tie formation, and dropout), the 52-week seasonal structure, validation against six stylised facts from the literature, and successive refinements to the calibration and the R analysis pipeline.
