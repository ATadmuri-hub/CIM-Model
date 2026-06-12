# Configuration schema: CIM v6.4

## Purpose

This directory contains the **configuration layer** of the CIM v6.4 framework. The configuration file documents every empirically calibrated, heuristic, or policy-lever parameter with provenance and rationale, so that researchers can adapt the mechanism layer (`CIM_v6_4.nlogo`) to other community-sport contexts by authoring a new configuration file.

The canonical configuration for the Istanbul calisthenics case is `calisthenics-istanbul.csv`. This file reproduces every default value hardcoded in `CIM_v6_4.nlogo` (which in turn matches `CIM_v6_3.nlogo` by design), so that loading it at model setup produces behaviour bit-identical to v6.3 Baseline on seeded runs. A second-domain configuration, `language-course-berlin.csv`, is shipped as a worked example (used for the Berlin cross-domain transfer test in the thesis). Further configurations (e.g., `community-gym-helsinki.csv`, `boxing-copenhagen.csv`) are follow-up work.

## File format

A configuration file is a UTF-8 CSV with a fixed header and one row per parameter.

```
name,value,unit,range,source,description,calibration-tier,scenario-scope
```

### Column semantics

- **name**: kebab-case identifier matching a corresponding global in `CIM_v6_4.nlogo` (prefixed `cfg-` for parameters introduced in the Tier 2 refactor; existing slider-backed globals retain their historical names).
- **value**: numeric default. If value is a list (e.g., range bounds), write as `[low;high]` with semicolons (commas are the CSV delimiter).
- **unit**: dimensional unit (count, probability, scalar, week, hours, EUR, per-week, CEFR/hour, multiplier).
- **range**: the parameter's plausible (admissible) bound, or `fixed` if held constant. Same bracket notation as value. For the four globally-swept parameters (`motivation-decay-rate`, `peer-influence-coefficient`, `tie-formation-probability`, `dropout-threshold`) this bound contains the narrower three-level grid actually used in the thesis sensitivity sweep (see thesis Section 4.7); other ranged parameters document plausible bounds for reference and domain adaptation and are not swept.
- **source**: short citation of the origin (author-year, programme document, or "programme design" / "implementation choice").
- **description**: one-line human-readable explanation.
- **calibration-tier**: one of `empirical`, `heuristic`, `policy-lever`.
  - `empirical`: value derived from a specific published study; citation is load-bearing.
  - `heuristic`: value chosen by urban-planning, programme-design, or implementation convention; the precise number is weaker evidence than the direction.
  - `policy-lever`: a design choice that is manipulated in the scenario set (e.g., group composition, indoor facility probability).
- **scenario-scope**: `all` if the parameter is active under every scenario, or the scenario name if the value is scenario-specific (e.g., `Weak Peer Influence` for the scenario-overriding β).

## Loading semantics

`CIM_v6_4.nlogo` configures itself at `setup` time in two steps. **First**, it applies the inline copy of the selected domain (`load-config-inline`), which sets the correct per-domain values in every environment. **Second**, it attempts to read `config/<domain>.csv` from disk via `load-config`; where the read succeeds (e.g. headless), the file's values (including any a third party has edited) override the inline copy. If a row is missing, the inline/default value is retained; an invalid (non-numeric) value prints a warning and is skipped. This inline-first design guarantees the named domain is always configured correctly, even when the file cannot be read.

> **macOS note: important if you edit the CSV.** macOS app sandboxing (TCC) blocks the NetLogo *GUI* from *reading* files inside protected folders such as `~/Downloads`, even though `file-exists?` succeeds. In that case the model logs `Config file ... not read` and runs on the **inline** copy of the named domain, so behaviour stays correct, but an *edited* CSV is ignored in the GUI. To make an edited CSV take effect, do one of: **(a)** run the model **headless** (`org.nlogo.headless.Main` / BehaviorSpace, which has file access; this is how the thesis results were generated); **(b)** move the model and `config/` out of `~/Downloads` (e.g. to `~/Documents`); or **(c)** grant NetLogo Full Disk Access in System Settings → Privacy & Security. Because the inline copy is the GUI fallback, a *permanent* parameter change should be made in **both** the CSV and the matching inline loader (`load-istanbul-inline` / `load-berlin-inline`) in `CIM_v6_4.nlogo`.

The BehaviorSpace experiments embedded in `CIM_v6_4.nlogo` override a subset of configuration values via `<enumeratedValueSet>` elements (e.g., `scenario-type = "Baseline"`, `motivation-decay-rate = 0.018`). These overrides take precedence over the CSV: BehaviorSpace reads → hardcoded defaults → CSV → BehaviorSpace overrides.

## Calibration-tier summary

| Tier | Count | Examples |
|---|---|---|
| empirical | 7 | motivation-decay-rate (Gjestvang 2020), peer-influence-coefficient (Centola 2010), tie-formation-probability (Kossinets & Watts 2006), prior-exercise-probability (Dishman 1988), language-gain-rate-per-hour (CEFR acquisition rates), dropout-prob-motivation (Granovetter threshold), dropout-threshold (SDT + Granovetter) |
| heuristic | 27 | initial-tie-strength, distance-penalty-500m, female-outdoor-safety-prob, refugee-initial-childcare-prob, arrival-cohort-mean-months, indoor-facility-probability, annual-budget-per-park, dropout-prob-work, dropout-prob-winter, dropout-prob-distance, reentry-prob-base, reentry-prob-cap, winter-no-return-threshold-weeks, interaction-quality-floor, language-efficiency-multiplier, language-friendship-multiplier, ... |
| policy-lever | 10 | num-parks, refugees-per-group, locals-per-group, sessions-per-week, session-duration-hours, group-target-size, weak-peer-beta (scenario-specific), indoor-facility-probability (scenario-specific) |

## Adapting to a new domain

The model ships a ready-to-use **`custom`** slot in the `config-domain` chooser plus a `config/custom.csv` template (a copy of the Istanbul config).

**Quickest (GUI): the `Load Config CSV` button.** Click it, pick any `.csv` from anywhere via the file dialog: the model copies it into `config/custom.csv`, sets `config-domain` to `custom`, and re-runs setup. Then click `go`. (It writes `config/custom.csv`, so it needs write access to `config/`: works outside `~/Downloads` or with Full Disk Access; under `~/Downloads` the GUI blocks the write and the button says so.)

The manual path needs **no model editing** either:

1. **Edit `config/custom.csv`**: change the `value` column to your context's parameters. (See the calibration guidance below.)
2. **Select `custom`** in the `config-domain` dropdown (GUI), or set `config-domain="custom"` in your BehaviorSpace experiment (headless).
3. **Run.** For a custom domain the CSV is the **full source of truth**: *every* parameter, including the GUI-slider-backed ones (`num-parks`, group sizes, `motivation-decay-rate`, `peer-influence-coefficient`, `tie-formation-probability`, `dropout-threshold`), is applied from the CSV, **even under BehaviorSpace**. You do not need to set anything in `enumeratedValueSet`. *(Verified empirically: a `custom` run takes `num-parks` and `γ` straight from the CSV; the two built-in presets are unaffected and remain bit-identical.)*

Headless is recommended (file reads always work). In the GUI, if the model sits under `~/Downloads`, macOS TCC may block the read; relocate the model + `config/` (e.g. to `~/Documents`) or grant NetLogo Full Disk Access; otherwise the model falls back to the Istanbul defaults and **raises a dialog warning you** (it does not fail silently).

**More than one custom domain?** Add your name to the `config-domain` chooser value list (one line in `CIM_v6_4.nlogo`, beside `"custom"`) and create `config/<your-name>.csv`. The chooser gates which names the GUI and BehaviorSpace accept; a name not in the chooser is silently skipped.

**Calibrating the values:**
- For each `empirical` parameter, locate the corresponding published source for your domain; update `value`, `source`, and `description`.
- For each `heuristic` parameter, check whether the domain convention differs (e.g. a language café has no outdoor/indoor season: set `indoor-facility-probability = 1.0` and the winter dropout probability to 0).
- For `policy-lever` parameters, choose design values that reflect the actual programme.
- If your domain has no analogue for a parameter, set the corresponding scenario weight to zero rather than delete the row.
- **Re-validate:** confirm the six pattern-oriented validation targets (Section 3.10) remain reachable for your domain, or document the relaxations; the Istanbul targets are not assumed to transfer.

## What a configuration can and cannot change

A configuration file **re-parameterises** an existing model; it does not redefine it.

- **Can change:** parameter *values*: the empirical coefficients (motivation decay, peer influence, tie formation), the heuristic barriers and probabilities, and the policy-lever design choices (group sizes, number of parks, budget, indoor provision, targeting accuracy). This re-points the model at a new community-sport context.
- **Cannot change:** the model's *structure and mechanisms*: the weekly attendance → peer-influence → tie-formation → dropout cycle, the agent types, the season logic, and the scenario definitions live in the NetLogo code, not the CSV. A key the loader does not recognise is reported and ignored; the CSV cannot add a new parameter or rule.
- **Not a data-import tool:** the model *generates* synthetic agents from distributions; it does not ingest an empirical dataset of real participants. A configuration sets the *distribution parameters*, not individual records.
- **Slider-backed parameters: presets vs. custom domains.** About ten parameters are also GUI sliders: `num-parks`, `refugees-per-group`, `locals-per-group`, and the four sensitivity parameters (`motivation-decay-rate`, `peer-influence-coefficient`, `tie-formation-probability`, `dropout-threshold`). For the **two built-in presets** (`calisthenics-istanbul`, `language-course-berlin`) under BehaviorSpace, the loader **deliberately skips** the CSV value for these so the experiment's `enumeratedValueSet` takes precedence; this is what keeps the 36 shipped experiments reproducible. For a **custom domain** (`config-domain` not a preset), the loader applies them from the CSV like every other key, even under BehaviorSpace, so **the CSV alone fully defines a custom run**. Practical consequence: for a *custom* run, put everything in the CSV (don't also set the same param in `enumeratedValueSet`, or the CSV will win and surprise you); for the *presets*, the slider params come from the experiment XML.

A new domain therefore requires (i) a structurally similar group-based, in-person programme; (ii) domain-specific re-calibration of the empirical parameters; and (iii) re-validation against that domain's *own* pattern-oriented targets: the six Istanbul targets in Section 3.10 are not assumed to transfer. The shipped `language-course-berlin` domain is a worked example: its second-domain test in the thesis (Appendix G.4) transfers two of four ranking relations, illustrating that transfer is **partial and must be checked, not assumed**.

## Versioning

This schema applies to v6.4. Future versions may add columns (e.g., `uncertainty`, `elasticity`) without breaking backward compatibility; existing rows remain valid under the wider schema. Removing columns or renaming parameters requires a minor version bump.
