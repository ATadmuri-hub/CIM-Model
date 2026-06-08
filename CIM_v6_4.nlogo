;; ============================================================================
;; CALISTHENICS INTEGRATION MODEL (CIM) v6.4
;; Agent-Based Model for Sport-Driven Migrant Integration
;;
;; Aligns with: Thesis v6.4 (Tier 1 feedback incorporation + Tier 2 configuration
;; layer + Tier 3 framework-generality experiments + code-prose audit pass +
;; Phase 3 robustness extensions + verification round 2 controls)
;; UC3M Master's Thesis - Prof. Anxo Sánchez
;; Date: April 2026 (release 2026-04-16; all tiers consolidated under v6.4);
;;       May 2026 (Phase 3 robustness extensions + verification round 2:
;;                 dose-response sweep, link prediction, open-population
;;                 multi-metric SNA, late-pairing buddy controls)
;;
;; KEY SPECIFICATIONS:
;; - Temporal resolution: 1 tick = 1 week (52 weeks = 1 year pilot)
;; - Heterogeneity: Gender, arrival cohort, prior exercise experience
;; - 23 policy scenarios + 1 auxiliary (Equifinality contagion benchmark);
;;   confirmatory family (4 scenarios: H1-H4); exploratory family (11 scenarios);
;;   robustness family (7 Phase 3 + verification round scenarios). 100-500 runs each.
;; - Sensitivity analysis: 810 runs via 3-level factorial design
;;
;; FIXES vs v6.0 (Feb 2026):
;; [FIX 1] Peer influence normalized by mean tie-strength (not sum) → removes degree bias
;; [FIX 2] Language learning uses linear ceiling factor → diminishing returns near C2
;; [FIX 3] export-final-results adds weeks-46-52 stabilization-window averages
;; [FIX 4] New procedure export-agent-crosssection → agent-level CSV for R survival models
;; ============================================================================

;; ----------------------------------------------------------------------------
;; v6.3 → v6.4 CHANGELOG (Tier 1 feedback-incorporation release, April 2026)
;; ----------------------------------------------------------------------------
;; [COMMENT] Lines 600, 604, 667, 710: stale composition comments ("≤3 locals per park")
;;           corrected to reflect baseline composition of 5 locals per park.
;;           Info tab at line 2680 was already correct ("instead of 5").
;; [COMMENT] Line 51 (param-peer-influence-coef): citation updated after T1.8 bib verification.
;; [COMMENT] Line 55 (param-tie-formation-prob): citation updated after T1.9 bib verification.
;; [LOGIC]   No logic changes in Tier 1. NetLogo behavior on Baseline + seeded run is
;;           bit-identical to CIM_v6_3.nlogo. Tier 2 config refactor and Tier 3
;;           second-domain + POWER-UP experiments all landed under the v6.4 umbrella.
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; BREEDS (Agent Types)
;; ----------------------------------------------------------------------------

breed [refugees refugee]
breed [locals local]
breed [trainers trainer]
breed [parks park]

;; ----------------------------------------------------------------------------
;; GLOBAL VARIABLES
;; ----------------------------------------------------------------------------

globals [
  config-file-loaded?                   ;; TRUE once config/<domain>.csv is read at setup (custom-domain safeguard)
  ;; === TIME (1 tick = 1 week) ===
  current-week                          ;; 0-52 (1 year pilot)
  current-season                        ;; "outdoor" or "indoor"
  indoor-season-start                   ;; Week 9 (October — winter begins)
  indoor-season-end                     ;; Week 28 (March — spring begins)
  winter-paused-count                   ;; Agents paused for winter (may re-enter in spring)
  
  ;; === POLICY PARAMETERS (Fixed) ===
  sessions-per-week                     ;; 2 sessions/week
  session-duration-hours                ;; 1.0 hour per session
  group-target-size                     ;; 11 (target group size)
  annual-budget-per-park                ;; €28,000 per park
  
  ;; === CALIBRATED PARAMETERS (from Table 4.1 in outline) ===
  param-motivation-decay-base           ;; 0.018 per week (Gjestvang 2020)
  param-peer-influence-coef             ;; 0.08 (Centola 2010 Science 329:1194--1197; peer-reinforced behaviour spread in clustered networks)
  param-language-gain-base-per-hour     ;; 0.019 per hour (CEFR + embodied cognition)
  param-language-efficiency-multiplier  ;; 0.70 (informal discount)
  param-language-friendship-multiplier  ;; 1.15 (peer translation bonus)
  param-tie-formation-prob              ;; 0.05 per session (Kossinets & Watts 2006 Science 311:88--90; evolving social network tie-formation rates)
  param-dropout-threshold               ;; 0.20 (SDT literature)
  param-distance-penalty-500m           ;; 0.85 (urban planning heuristic)
  
  ;; === HETEROGENEITY PARAMETERS ===
  prior-exercise-probability            ;; 0.30 (30% have prior experience)
  arrival-cohort-mean-months            ;; 12 months (mean time in country)
  
  ;; === OUTCOME METRICS (Section 5.3 in outline) ===
  ;; Primary outcomes
  total-active-participants
  total-refugees-active
  total-locals-active
  avg-motivation-level
  avg-language-proficiency              ;; CEFR scale (0-6)
  cross-group-tie-count
  cross-group-tie-ratio
  total-dropouts
  total-refugees-dropouts
  total-locals-dropouts
  cost-per-participant-retained
  
  ;; Heterogeneity metrics
  female-participation-rate
  male-participation-rate
  female-dropout-rate
  male-dropout-rate
  recent-cohort-language-gain           ;; <6 months
  established-cohort-language-gain      ;; 6-24 months
  settled-cohort-language-gain          ;; >24 months
  prior-exercise-retention-rate
  no-exercise-retention-rate
  
  ;; Target achievement (Boolean)
  meets-motivation-target?              ;; ≥0.7 at week 24?
  meets-language-target?                ;; ≥50% at A2+ (2.0)?
  meets-integration-target?             ;; ≥40% cross-group ties?
  meets-attendance-target?              ;; ≥75% attendance?
  meets-cost-target?                    ;; ≤€3,000 per retained?
  overall-success?                      ;; 4+ targets met?
  
  ;; === TRACKING HISTORY (for plots) ===
  weekly-participation-list
  weekly-motivation-list
  weekly-language-list
  weekly-integration-list
  weekly-dropouts-list
  weekly-cost-list
  weekly-female-participation-list
  weekly-male-participation-list
  
  ;; === SCENARIO CONFIGURATION ===
  scenario-name                         ;; Current scenario being run

  ;; === NEW MECHANISM PARAMETERS (v6.3) ===
  ;; run-start-index, targeting-accuracy, buddy-program-k, rotation-period-weeks, use-contagion-influence?
  ;; are declared via interface INPUTBOX/SWITCH widgets (allows BehaviorSpace to set them)

  ;; === TIER 2 CONFIG PARAMETERS (v6.4, plan T2.2) ===
  ;; Loaded from config/calisthenics-istanbul.csv via load-config at setup.
  ;; Hardcoded defaults below match v6.3 so that no-config path is bit-identical.
  cfg-annual-budget-per-park              ;; 28000 EUR
  cfg-indoor-season-start                 ;; 9
  cfg-indoor-season-end                   ;; 28
  cfg-motivation-boost-per-session        ;; 0.03
  cfg-indoor-facility-probability         ;; 0.80
  cfg-refugee-initial-childcare-prob      ;; 0.40
  cfg-refugee-initial-transport-prob      ;; 0.70
  cfg-local-initial-childcare-prob        ;; 0.80
  cfg-local-initial-transport-prob        ;; 0.90
  cfg-refugee-initial-tie-prob            ;; 0.20
  cfg-local-initial-tie-prob              ;; 0.15
  cfg-initial-tie-strength                ;; 0.20
  cfg-tie-strength-growth                 ;; 0.03
  cfg-interaction-quality-floor           ;; 0.20
  cfg-female-outdoor-safety-prob          ;; 0.20
  cfg-female-outdoor-safety-multiplier    ;; 0.90
  cfg-dropout-prob-motivation             ;; 0.20
  cfg-dropout-prob-work-refugee           ;; 0.02
  cfg-dropout-prob-work-local             ;; 0.01
  cfg-dropout-prob-winter                 ;; 0.05
  cfg-dropout-prob-distance-refugee       ;; 0.03
  cfg-dropout-prob-distance-local         ;; 0.015
  cfg-reentry-prob-base                   ;; 0.25
  cfg-reentry-prob-cap                    ;; 0.70
  cfg-winter-no-return-threshold-weeks    ;; 6
  cfg-weak-peer-beta                      ;; 0.03 (scenario-scope: Weak Peer Influence)

  ;; === TIER 3 BLOCK J PARAMETERS (v6.4 umbrella, plan T3.A Open Population) ===
  ;; Defaults are 0 so Baseline behaviour is bit-identical to v6.4.
  ;; The OpenPopulation scenario sets non-zero values via apply-scenario-configuration.
  cfg-inflow-rate-per-week                ;; 0.0 default; 0.30 under OpenPopulation (prob of 1 new arrival/week)
  cfg-outflow-rate-per-week               ;; 0.0 default; 0.20 under OpenPopulation (prob of 1 departure/week)

]

;; ----------------------------------------------------------------------------
;; REFUGEE VARIABLES (Section 4.2 in outline)
;; ----------------------------------------------------------------------------

refugees-own [
  ;; === IDENTITY ===
  participant-id
  
  ;; === DEMOGRAPHICS (HETEROGENEITY) ===
  gender                                ;; "male" or "female"
  months-in-country                     ;; 0-60 months (capped at 5 years)
  arrival-cohort                        ;; "recent" (<6mo) / "established" (6-24mo) / "settled" (>24mo)
  prior-exercise-experience?            ;; Boolean (30% probability)
  
  ;; === CORE ATTRIBUTES ===
  motivation                            ;; 0-1 scale (dynamic)
  language-skill-cefr                   ;; 0-6 scale (A0=0, A1=1, A2=2, B1=3, B2=4, C1=5, C2=6)
  ses-level                             ;; 0-1 socioeconomic status
  
  ;; === SOCIOECONOMIC BARRIERS ===
  work-hours-conflict?                  ;; Has work schedule conflict
  has-childcare?                        ;; Has childcare support
  receives-stipend?                     ;; Gets financial support (€100/month)
  can-access-transport?                 ;; Can reach park easily
  
  ;; === PROGRAM PARTICIPATION ===
  assigned-training-group               ;; Park ID (0 to num-parks-1)
  weeks-attended                        ;; Cumulative attendance count
  current-week-attendance               ;; Attended this week? (boolean)
  sessions-attended-this-week           ;; 0, 1, or 2
  
  ;; === SPATIAL ===
  home-x
  home-y
  distance-to-park                      ;; Distance in patches (1 patch = 100m)
  
  ;; === SOCIAL NETWORK ===
  local-friends                         ;; agentset
  refugee-friends                       ;; agentset
  total-friend-count
  cross-group-friend-count
  
  ;; === OUTCOMES ===
  has-dropped-out?
  dropout-week
  dropout-reason                        ;; "motivation" / "work" / "winter" / "distance"
  
  ;; === WINTER PAUSE (re-entry mechanism) ===
  winter-paused?                        ;; suspended during winter (NOT permanently dropped)
  weeks-since-pause                     ;; weeks since winter pause began

  ;; === BUDDY PROGRAMME (v6.3) ===
  buddy-local-id                        ;; who of assigned local buddy (-1 if none)

  ;; === TRACKING (for analysis) ===
  initial-motivation
  initial-language
  motivation-trajectory                 ;; list of weekly values (populated each tick)
  language-trajectory                   ;; list of weekly values
  language-gain-total                   ;; Total CEFR gain over program
]

;; ----------------------------------------------------------------------------
;; LOCAL VARIABLES (Section 4.2 in outline)
;; ----------------------------------------------------------------------------

locals-own [
  participant-id
  
  ;; Demographics
  gender
  prior-exercise-experience?
  
  ;; Core attributes
  motivation
  ses-level
  
  ;; Barriers (harmonized with refugees; lower rates — per Sánchez feedback)
  work-hours-conflict?
  has-childcare?             ;; locals can have childcare responsibilities
  receives-stipend?          ;; always false for locals (programme-specific)
  can-access-transport?      ;; locals generally have better transport access
  
  ;; Participation
  assigned-training-group
  weeks-attended
  current-week-attendance
  sessions-attended-this-week
  
  ;; Spatial
  home-x
  home-y
  distance-to-park
  
  ;; Social network
  refugee-friends
  local-friends
  total-friend-count
  cross-group-friend-count
  
  ;; Outcomes
  has-dropped-out?
  dropout-week
  dropout-reason
  
  ;; Winter pause (re-entry mechanism)
  winter-paused?                        ;; suspended during winter
  weeks-since-pause

  ;; Tracking
  initial-motivation
  motivation-trajectory
]

;; ----------------------------------------------------------------------------
;; TRAINER VARIABLES
;; ----------------------------------------------------------------------------

trainers-own [
  training-group-id                     ;; Same as park ID
  assigned-park-agent                   ;; Link to park
  trainer-gender                        ;; "male" or "female"
  
  current-group-size
  refugees-in-group
  locals-in-group
  females-in-group
  males-in-group
  
  group-composition-optimal?            ;; Meets 1:2-3:6-9 ratio?
  uses-bilingual-instruction?           ;; Always true
  facilitates-peer-translation?         ;; Always true
]

;; ----------------------------------------------------------------------------
;; PARK VARIABLES
;; ----------------------------------------------------------------------------

parks-own [
  park-id                               ;; 0 to num-parks-1
  park-name
  
  ;; Location characteristics
  location-neighborhood                 ;; "low-income" / "mixed" / "affluent"
  
  ;; Infrastructure
  has-outdoor-space?                    ;; Always true for pilot
  has-indoor-partner?                   ;; CrossFit box for Nov-Mar
  indoor-rental-cost-monthly            ;; €500/month
  
  ;; Gender segregation (Scenario 7 - optional)
  womens-only-group?                    ;; Designated for women-only
  
  ;; Current state
  current-active-count                  ;; People training here this week
  total-assigned-count                  ;; People assigned to this park
]

;; ----------------------------------------------------------------------------
;; LINKS (Friendships)
;; ----------------------------------------------------------------------------

undirected-link-breed [friendships friendship]

friendships-own [
  tie-strength                          ;; 0-1 (starts at 0.3, grows with interaction)
  is-cross-group?                       ;; refugee-local tie (true) or within-group (false)
  formed-week
  weeks-active
  last-contact-week                     ;; For decay tracking
]

;; ============================================================================
;; SETUP PROCEDURES
;; ============================================================================

to setup
  clear-all
  reset-ticks

  ;; Reproducible seeding: under BehaviorSpace, pin a distinct-but-reproducible seed per
  ;; run so the published numbers are exactly reproducible by anyone re-running the repo.
  ;; run-start-index (an interface global, preserved across clear-all) offsets top-up runs
  ;; so they get fresh seeds (301-500) rather than duplicating the base runs (1-300).
  ;; Interactive runs (behaviorspace-run-number = 0) keep NetLogo's default fresh seed,
  ;; leaving manual exploration unaffected.
  if behaviorspace-run-number > 0 [ random-seed (behaviorspace-run-number + run-start-index) ]
  
  ;; Initialize time
  set current-week 0
  set current-season "outdoor"
  ;; Seasonal structure: autumn (wk1-8) → winter (wk9-28) → spring/summer (wk29-52)
  ;; Maps to Sep-Oct outdoor, Nov-Mar indoor, Apr-Aug outdoor (Mediterranean climate)
  set indoor-season-start 9
  set indoor-season-end 28
  set winter-paused-count 0
  
  ;; Set fixed policy parameters
  set sessions-per-week 2
  set session-duration-hours 1.0
  set group-target-size 11
  set annual-budget-per-park 28000
  
  ;; Load calibrated parameters from sliders (with defaults from Table 4.1)
  set param-motivation-decay-base motivation-decay-rate
  set param-peer-influence-coef peer-influence-coefficient
  set param-language-gain-base-per-hour language-gain-rate-per-hour
  set param-language-efficiency-multiplier language-efficiency-multiplier
  set param-language-friendship-multiplier language-friendship-multiplier
  set param-tie-formation-prob tie-formation-probability
  set param-dropout-threshold dropout-threshold
  set param-distance-penalty-500m 0.85  ;; Fixed at 0.85 for >500m
  
  ;; Heterogeneity parameters
  set prior-exercise-probability 0.30
  set arrival-cohort-mean-months 12

  ;; === TIER 2 CONFIG DEFAULTS (v6.4, plan T2.2) ===
  ;; These defaults are overridden by load-config if config/calisthenics-istanbul.csv is present.
  set cfg-annual-budget-per-park             28000
  set cfg-indoor-season-start                9
  set cfg-indoor-season-end                  28
  set cfg-motivation-boost-per-session       0.03
  set cfg-indoor-facility-probability        0.80
  set cfg-refugee-initial-childcare-prob     0.40
  set cfg-refugee-initial-transport-prob     0.70
  set cfg-local-initial-childcare-prob       0.80
  set cfg-local-initial-transport-prob       0.90
  set cfg-refugee-initial-tie-prob           0.20
  set cfg-local-initial-tie-prob             0.15
  set cfg-initial-tie-strength               0.20
  set cfg-tie-strength-growth                0.03
  set cfg-interaction-quality-floor          0.20
  set cfg-female-outdoor-safety-prob         0.20
  set cfg-female-outdoor-safety-multiplier   0.90
  set cfg-dropout-prob-motivation            0.20
  set cfg-dropout-prob-work-refugee          0.02
  set cfg-dropout-prob-work-local            0.01
  set cfg-dropout-prob-winter                0.05
  set cfg-dropout-prob-distance-refugee      0.03
  set cfg-dropout-prob-distance-local        0.015
  set cfg-reentry-prob-base                  0.25
  set cfg-reentry-prob-cap                   0.70
  set cfg-winter-no-return-threshold-weeks   6
  set cfg-weak-peer-beta                     0.03

  ;; Tier 3 Block J defaults (Open Population rates; 0 = closed, bit-identical to v6.4)
  set cfg-inflow-rate-per-week               0.0
  set cfg-outflow-rate-per-week              0.0

  ;; Tier 3 Block I: domain selector. If the interface chooser has not been initialized
  ;; (NetLogo default is 0 for numeric globals; string chooser returns empty), default
  ;; to Istanbul calisthenics so Baseline behaviour is bit-identical to v6.4.
  if not is-string? config-domain or config-domain = "" or config-domain = "0" [
    set config-domain "calisthenics-istanbul"
  ]

  ;; Load domain configuration. INLINE-FIRST: the inline loader sets the correct
  ;; per-domain values in EVERY environment -- including the macOS GUI, where TCC blocks
  ;; file-open on files under ~/Downloads even though file-exists? succeeds. We then
  ;; attempt to read the CSV from disk: where file access works (e.g. headless, or the
  ;; model relocated out of a protected folder), the file's values -- including any a
  ;; third party has edited -- override the inline copy. This guarantees the named domain
  ;; (e.g. language-course-berlin) is always configured correctly and never silently
  ;; left on another domain's defaults if the file read is blocked.
  set config-file-loaded? false
  load-config-inline
  let cfg-path (word "config/" config-domain ".csv")
  carefully [ if file-exists? cfg-path [ load-config cfg-path ] ] [ ]
  ;; Safeguard (interactive GUI only): warn loudly if a custom domain's CSV was not
  ;; actually read, so the run is not SILENTLY left on the Istanbul-baseline defaults
  ;; (a custom domain has no inline copy). Headless / BehaviorSpace is unaffected --
  ;; file reads succeed there, so config-file-loaded? is true and this never fires.
  if (behaviorspace-run-number = 0) and (not member? config-domain ["calisthenics-istanbul" "language-course-berlin"]) and (not config-file-loaded?) [
    user-message (word "config-domain \"" config-domain "\" has no built-in inline copy, and config/" config-domain ".csv was not read (commonly macOS sandboxing blocking file access under ~/Downloads). This run uses the Istanbul-baseline defaults, NOT your configuration. To load your CSV: run it headless / via BehaviorSpace, or move the model and config/ out of ~/Downloads (or grant NetLogo Full Disk Access).")
  ]
  
  ;; Initialize metrics
  set total-active-participants 0
  set total-refugees-active 0
  set total-locals-active 0
  set avg-motivation-level 0
  set avg-language-proficiency 0
  set cross-group-tie-count 0
  set cross-group-tie-ratio 0
  set total-dropouts 0
  set total-refugees-dropouts 0
  set total-locals-dropouts 0
  set cost-per-participant-retained 0
  
  ;; Heterogeneity metrics
  set female-participation-rate 0
  set male-participation-rate 0
  set female-dropout-rate 0
  set male-dropout-rate 0
  set recent-cohort-language-gain 0
  set established-cohort-language-gain 0
  set settled-cohort-language-gain 0
  set prior-exercise-retention-rate 0
  set no-exercise-retention-rate 0
  
  ;; Target achievement
  set meets-motivation-target? false
  set meets-language-target? false
  set meets-integration-target? false
  set meets-attendance-target? false
  set meets-cost-target? false
  set overall-success? false
  
  ;; Initialize tracking lists
  set weekly-participation-list []
  set weekly-motivation-list []
  set weekly-language-list []
  set weekly-integration-list []
  set weekly-dropouts-list []
  set weekly-cost-list []
  set weekly-female-participation-list []
  set weekly-male-participation-list []
  
  ;; Set scenario name from interface chooser
  set scenario-name scenario-type

  ;; Initialize v6.3 parameters
  ;; targeting-accuracy = 0 (uninitialized default) → set to 1.0 (perfect)
  if targeting-accuracy = 0 [ set targeting-accuracy 1.0 ]
  ;; buddy-program-k and rotation-period-weeks default to 0 (disabled)
  ;; use-contagion-influence? defaults to false (Mechanism A = default)
  if not is-boolean? use-contagion-influence? [ set use-contagion-influence? false ]

  ;; Setup environment
  setup-environment
  
  ;; Create agents
  setup-parks-pilot
  setup-trainers-pilot
  recruit-refugees-pilot
  recruit-locals-pilot
  
  ;; Form groups
  form-training-groups
  
  ;; Provide support services (tiered by SES)
  provide-support-services
  
  ;; Initialize minimal networks
  initialize-friendship-networks
  
  ;; Apply scenario configurations
  apply-scenario-configuration

  ;; Buddy programme: assign local buddies if enabled
  if buddy-program-k > 0 [ setup-buddy-connections ]

  ;; Initial calculations
  update-all-metrics
  record-weekly-data
  update-visualization
end

;; ----------------------------------------------------------------------------
;; Setup Environment
;; ----------------------------------------------------------------------------

to setup-environment
  ask patches [
    set pcolor white
  ]
end

;; ----------------------------------------------------------------------------
;; Setup Parks
;; ----------------------------------------------------------------------------

to setup-parks-pilot
  create-parks num-parks [
    set shape "house"
    set size 4
    set color green
    
    set park-id who
    set park-name (word "Park-" (park-id + 1))
    
    ;; Randomly assign neighborhood type
    set location-neighborhood one-of ["low-income" "mixed" "affluent"]
    
    ;; All parks have outdoor space
    set has-outdoor-space? true
    
    ;; Indoor partner availability (80% baseline)
    set has-indoor-partner? (random-float 1.0 < cfg-indoor-facility-probability)
    set indoor-rental-cost-monthly 500  ;; €500/month off-peak
    
    ;; Gender segregation (set by scenario)
    set womens-only-group? false
    
    set current-active-count 0
    set total-assigned-count 0
    
    ;; Position spatially (distributed evenly in circle)
    let angle (park-id * 360 / num-parks)
    let radius (max-pxcor * 0.35)
    setxy (max-pxcor / 2 + radius * sin angle) 
          (max-pycor / 2 + radius * cos angle)
  ]
end

;; ----------------------------------------------------------------------------
;; Setup Trainers (1 per park)
;; ----------------------------------------------------------------------------

to setup-trainers-pilot
  create-trainers num-parks [
    set shape "star"
    set size 2.5
    set color yellow
    
    ;; Assign to park
    let my-park-id (who - num-parks)  ;; Adjust for park who numbers
    set training-group-id my-park-id
    set assigned-park-agent park my-park-id
    
    ;; Gender (50/50 split)
    set trainer-gender one-of ["male" "female"]
    
    ;; Move to park location
    if assigned-park-agent != nobody [
      move-to assigned-park-agent
    ]
    
    set current-group-size 0
    set refugees-in-group 0
    set locals-in-group 0
    set females-in-group 0
    set males-in-group 0
    set group-composition-optimal? false
    
    ;; Bilingual curriculum features
    set uses-bilingual-instruction? true
    set facilitates-peer-translation? true
  ]
end

;; ----------------------------------------------------------------------------
;; Recruit Refugees (with Heterogeneity - Section 4.2)
;; ----------------------------------------------------------------------------

to recruit-refugees-pilot
  ;; Target: 15 refugees per park (use slider refugees-per-group)
  let target-count (refugees-per-group * num-parks)
  
  create-refugees target-count [
    set shape "person"
    set size 1.5
    set participant-id who
    
    ;; === DEMOGRAPHICS (HETEROGENEITY) ===
    
    ;; Gender (50/50 split, adjustable in scenarios)
    set gender one-of ["male" "female"]
    
    ;; Arrival cohort (exponential distribution, mean = 12 months)
    set months-in-country random-exponential arrival-cohort-mean-months
    set months-in-country min (list 60 months-in-country)  ;; Cap at 5 years
    
    ;; Categorize cohort (Section 4.2 in outline)
    ifelse months-in-country < 6 [
      set arrival-cohort "recent"
    ][
      ifelse months-in-country < 24 [
        set arrival-cohort "established"
      ][
        set arrival-cohort "settled"
      ]
    ]
    
    ;; Prior exercise experience (30% probability)
    set prior-exercise-experience? (random-float 1.0 < prior-exercise-probability)
    
    ;; Color by gender
    ifelse gender = "female" [
      set color red - 1
    ][
      set color red
    ]
    
    ;; === INITIAL ATTRIBUTES ===
    
    ;; Language: Based on arrival cohort (Table 4.1 heterogeneity params)
    if arrival-cohort = "recent" [
      set language-skill-cefr random-float 0.5  ;; A0-A1 (0-0.5)
    ]
    if arrival-cohort = "established" [
      set language-skill-cefr 0.5 + random-float 1.0  ;; A1-A2+ (0.5-1.5)
    ]
    if arrival-cohort = "settled" [
      set language-skill-cefr 1.5 + random-float 1.5  ;; A2-B1 (1.5-3.0)
    ]
    
    set initial-language language-skill-cefr
    set language-trajectory (list language-skill-cefr)
    set language-gain-total 0
    
    ;; Motivation: Base 0.3-0.8, +0.10 bonus if prior exercise
    set motivation 0.3 + random-float 0.5
    if prior-exercise-experience? [
      set motivation motivation + 0.10  
    ]
    set motivation min (list 1.0 motivation)
    
    set initial-motivation motivation
    set motivation-trajectory (list motivation)
    
    ;; Socioeconomic status (uniform distribution)
    set ses-level random-float 1.0
    
    ;; === SOCIOECONOMIC BARRIERS ===
    ;; Lower SES = higher probability of barriers
    
    set work-hours-conflict? (random-float 1.0 < (0.6 - ses-level * 0.3))
    set has-childcare? (random-float 1.0 < cfg-refugee-initial-childcare-prob)  ;; default 40% (cfg-refugee-initial-childcare-prob)
    set can-access-transport? (random-float 1.0 < cfg-refugee-initial-transport-prob)  ;; default 70% (cfg-refugee-initial-transport-prob)
    set receives-stipend? false  ;; Assigned in provide-support-services
    
    ;; Participation
    set weeks-attended 0
    set current-week-attendance false
    set sessions-attended-this-week 0
    
    ;; Spatial - random home location
    set home-x random-xcor
    set home-y random-ycor
    setxy home-x home-y
    
    ;; Social
    set local-friends no-turtles
    set refugee-friends no-turtles
    set total-friend-count 0
    set cross-group-friend-count 0
    
    ;; Outcomes
    set has-dropped-out? false
    set dropout-week -1
    set dropout-reason "none"
    set winter-paused? false
    set weeks-since-pause 0
    
    ;; Assigned later
    set assigned-training-group -1
    set distance-to-park 0
    set buddy-local-id -1              ;; no buddy by default
  ]
end

;; ----------------------------------------------------------------------------
;; Recruit Locals (5 per park baseline; Suboptimal scenario reduces to 1 per park)
;; ----------------------------------------------------------------------------

to recruit-locals-pilot
  ;; Target: 5 locals per park (baseline); Suboptimal Composition scenario reduces to 1 per park
  let target-count (locals-per-group * num-parks)
  
  create-locals target-count [
    set shape "person"
    set size 1.5
    set participant-id who
    
    ;; Demographics
    set gender one-of ["male" "female"]
    
    ;; Prior exercise (higher rate than refugees: 40%)
    set prior-exercise-experience? (random-float 1.0 < 0.4)
    
    ifelse gender = "female" [
      set color blue - 1
    ][
      set color blue
    ]
    
    ;; Locals: higher initial motivation (voluntary participation)
    set motivation 0.5 + random-float 0.4
    if prior-exercise-experience? [
      set motivation motivation + 0.10
    ]
    set motivation min (list 1.0 motivation)
    
    set initial-motivation motivation
    set motivation-trajectory (list motivation)
    
    set ses-level random-float 1.0
    
    ;; Barriers — lower rates than refugees (harmonized design)
    set work-hours-conflict?   (random-float 1.0 < 0.30)  ;; 30% flat
    set has-childcare?         (random-float 1.0 < cfg-local-initial-childcare-prob)  ;; default 80% (cfg-local-initial-childcare-prob)
    set receives-stipend?      false                       ;; locals never receive stipend
    set can-access-transport?  (random-float 1.0 < cfg-local-initial-transport-prob)  ;; default 90% (cfg-local-initial-transport-prob)
    
    set weeks-attended 0
    set current-week-attendance false
    set sessions-attended-this-week 0
    
    set home-x random-xcor
    set home-y random-ycor
    setxy home-x home-y
    
    set refugee-friends no-turtles
    set local-friends no-turtles
    set total-friend-count 0
    set cross-group-friend-count 0
    
    set has-dropped-out? false
    set dropout-week -1
    set dropout-reason "none"
    set winter-paused? false
    set weeks-since-pause 0
    
    set assigned-training-group -1
    set distance-to-park 0
  ]
end

;; ----------------------------------------------------------------------------
;; Form Training Groups (target composition: 1 trainer : 5 locals : 15 migrants per park)
;; ----------------------------------------------------------------------------

to form-training-groups
  ;; Standard assignment: nearest park
  
  ask refugees [
    let nearest-park min-one-of parks [distance myself]
    if nearest-park != nobody [
      set assigned-training-group [park-id] of nearest-park
      set distance-to-park distance nearest-park
    ]
  ]
  
  ask locals [
    let nearest-park min-one-of parks [distance myself]
    if nearest-park != nobody [
      set assigned-training-group [park-id] of nearest-park
      set distance-to-park distance nearest-park
    ]
  ]
  
  ;; Update park counts and trainer stats
  ask parks [
    set total-assigned-count count (turtle-set
      refugees with [assigned-training-group = [park-id] of myself]
      locals with [assigned-training-group = [park-id] of myself]
    )
  ]
  
  ask trainers [
    set refugees-in-group count refugees with [assigned-training-group = [training-group-id] of myself]
    set locals-in-group count locals with [assigned-training-group = [training-group-id] of myself]
    set current-group-size (refugees-in-group + locals-in-group)
    
    let my-group-members (turtle-set
      refugees with [assigned-training-group = [training-group-id] of myself]
      locals with [assigned-training-group = [training-group-id] of myself]
    )
    
    set females-in-group count my-group-members with [gender = "female"]
    set males-in-group count my-group-members with [gender = "male"]
    
    ;; Check optimal composition (1 trainer : 5 locals : 15 migrants per park)
    set group-composition-optimal? (
      current-group-size >= 16 and
      current-group-size <= 24 and
      locals-in-group >= 4 and
      locals-in-group <= 6 and
      refugees-in-group >= 12 and
      refugees-in-group <= 18
    )
  ]
end

;; ----------------------------------------------------------------------------
;; Provide Support Services (Tiered by SES - Section 7.4.4 in outline)
;; ----------------------------------------------------------------------------

to provide-support-services
  ask refugees [
    ;; Targeting accuracy: if <1.0, some agents are misclassified and receive no SES-based support
    ;; Distance-based support is always applied (observable criterion)
    let correctly-targeted? (random-float 1.0 < targeting-accuracy)

    if correctly-targeted? [
      if ses-level < 0.3 [
        set receives-stipend? true
        if not has-childcare? [set has-childcare? true]
        if not can-access-transport? [set can-access-transport? true]
      ]
      if ses-level >= 0.3 and ses-level < 0.6 [
        if not has-childcare? [ set has-childcare? true ]
        if not can-access-transport? and random-float 1.0 < 0.5 [
          set can-access-transport? true
        ]
      ]
    ]

    ;; Distance-based transport support always applied (observable need)
    if distance-to-park > 5 and not can-access-transport? [
      set can-access-transport? true
      set receives-stipend? true
    ]
  ]
end

;; ----------------------------------------------------------------------------
;; Initialize Friendship Networks (minimal starting ties)
;; ----------------------------------------------------------------------------

to initialize-friendship-networks
  ;; Start with minimal ties (some people know each other from community)
  ask refugees [
    ;; 20% chance of initial tie with someone at same park
    let same-park-people (turtle-set
      other refugees with [assigned-training-group = [assigned-training-group] of myself]
      locals with [assigned-training-group = [assigned-training-group] of myself]
    )
    
    if count same-park-people > 0 and random-float 1.0 < cfg-refugee-initial-tie-prob [
      let new-friend one-of same-park-people
      if new-friend != nobody and not friendship-neighbor? new-friend [
        create-friendship-with new-friend [
          set tie-strength cfg-initial-tie-strength
          set is-cross-group? ([breed] of end1 != [breed] of end2)
          set formed-week 0
          set weeks-active 0
          set last-contact-week 0
          set color ifelse-value is-cross-group? [orange][gray]
          set thickness 0.1
        ]
      ]
    ]
  ]
  
  ;; Also for locals
  ask locals [
    let same-park-people (turtle-set
      refugees with [assigned-training-group = [assigned-training-group] of myself]
      other locals with [assigned-training-group = [assigned-training-group] of myself]
    )
    
    if count same-park-people > 0 and random-float 1.0 < cfg-local-initial-tie-prob [
      let new-friend one-of same-park-people
      if new-friend != nobody and not friendship-neighbor? new-friend [
        create-friendship-with new-friend [
          set tie-strength cfg-initial-tie-strength
          set is-cross-group? ([breed] of end1 != [breed] of end2)
          set formed-week 0
          set weeks-active 0
          set last-contact-week 0
          set color ifelse-value is-cross-group? [orange][gray]
          set thickness 0.1
        ]
      ]
    ]
  ]
  
  ;; Update friend counts
  ask (turtle-set refugees locals) [
    update-friend-lists
  ]
end

;; ----------------------------------------------------------------------------
;; Apply Scenario Configuration (Section 5.1 in outline - Table 5.1)
;; ----------------------------------------------------------------------------

to apply-scenario-configuration
  ;; Modify model based on scenario type
  
  if scenario-type = "Baseline" [
    ;; All support measures active (default)
    ;; Do nothing - already configured
  ]
  
  if scenario-type = "No Indoor Continuity" [
    ;; Scenario 1: Remove indoor continuity (tests H3)
    ask parks [
      set has-indoor-partner? false
    ]
  ]
  
  if scenario-type = "Minimal Support" [
    ;; Scenario 2: Remove stipends, reset childcare to baseline (tests H4)
    ask refugees [
      set receives-stipend? false
      set has-childcare? (random-float 1.0 < cfg-refugee-initial-childcare-prob)  ;; Reset to baseline (cfg-refugee-initial-childcare-prob)
    ]
  ]
  
  if scenario-type = "Low Park Density" [
    ;; Scenario 3: Increase distances (simulate fewer parks)
    ask refugees [
      set distance-to-park distance-to-park * 1.5  ;; 50% farther
    ]
    ask locals [
      set distance-to-park distance-to-park * 1.3  ;; 30% farther
    ]
  ]
  
  if scenario-type = "Weak Peer Influence" [
    ;; Scenario 4: Lower peer influence coefficient (tests H1)
    set param-peer-influence-coef cfg-weak-peer-beta  ;; vs baseline 0.08 (cfg-weak-peer-beta)
  ]
  
  if scenario-type = "Suboptimal Composition" [
    ;; Scenario 5: Force poor group composition (tests H2)
    ;; Remove some locals to create 1-local groups
    let excess-locals locals with [assigned-training-group >= 0]
    if count excess-locals > num-parks [
      ask n-of (count excess-locals - num-parks) excess-locals [
        dropout-procedure "removed-for-scenario"
        hide-turtle
      ]
    ]
  ]

  if scenario-type = "Composition2" [
    ;; Phase 3 Item 13: dose-response sweep, keeps 2 locals per park (avg)
    let excess-locals locals with [assigned-training-group >= 0]
    if count excess-locals > (2 * num-parks) [
      ask n-of (count excess-locals - (2 * num-parks)) excess-locals [
        dropout-procedure "removed-for-scenario"
        hide-turtle
      ]
    ]
  ]

  if scenario-type = "Composition3" [
    ;; Phase 3 Item 13: dose-response sweep, keeps 3 locals per park (avg)
    let excess-locals locals with [assigned-training-group >= 0]
    if count excess-locals > (3 * num-parks) [
      ask n-of (count excess-locals - (3 * num-parks)) excess-locals [
        dropout-procedure "removed-for-scenario"
        hide-turtle
      ]
    ]
  ]

  if scenario-type = "Composition4" [
    ;; Phase 3 Item 13: dose-response sweep, keeps 4 locals per park (avg)
    let excess-locals locals with [assigned-training-group >= 0]
    if count excess-locals > (4 * num-parks) [
      ask n-of (count excess-locals - (4 * num-parks)) excess-locals [
        dropout-procedure "removed-for-scenario"
        hide-turtle
      ]
    ]
  ]

  if scenario-type = "High SES Heterogeneity" [
    ;; Scenario 6: Increase SES variance (bimodal distribution)
    ask refugees [
      ;; 30% very low, 30% very high, 40% middle
      let rand random-float 1.0
      ifelse rand < 0.3 [
        set ses-level random-float 0.3  ;; Low SES
      ][
        ifelse rand < 0.6 [
          set ses-level 0.7 + random-float 0.3  ;; High SES
        ][
          set ses-level 0.3 + random-float 0.4  ;; Middle SES
        ]
      ]
      ;; Recalculate work-conflict with new SES
      set work-hours-conflict? (random-float 1.0 < (0.6 - ses-level * 0.3))
    ]
    ;; Reapply support services with new SES distribution
    provide-support-services
  ]
  
  if scenario-type = "Women-Only Groups" [
    ;; Scenario 7: Women-only parks (tests EQ1)
    let parks-to-convert n-of (round (num-parks / 2)) parks
    ask parks-to-convert [
      set womens-only-group? true
      set color pink
    ]
    ask refugees with [gender = "female"] [
      let womens-parks parks with [womens-only-group?]
      if any? womens-parks [
        let nearest-womens min-one-of womens-parks [distance myself]
        set assigned-training-group [park-id] of nearest-womens
        set distance-to-park distance nearest-womens
      ]
    ]
  ]

  ;; === NEW SCENARIOS (v6.3) ===

  if scenario-type = "NoIndoor Minimal" [
    ;; 2x2 cell: No indoor continuity + minimal support
    ask parks [ set has-indoor-partner? false ]
    ask refugees [
      set receives-stipend? false
      set has-childcare? (random-float 1.0 < cfg-refugee-initial-childcare-prob)
    ]
  ]

  if scenario-type = "Targeting50" [
    set targeting-accuracy 0.50
    ask refugees [ set receives-stipend? false ]
    provide-support-services
  ]

  if scenario-type = "Targeting70" [
    set targeting-accuracy 0.70
    ask refugees [ set receives-stipend? false ]
    provide-support-services
  ]

  if scenario-type = "Targeting90" [
    set targeting-accuracy 0.90
    ask refugees [ set receives-stipend? false ]
    provide-support-services
  ]

  if scenario-type = "BuddyProgram" [
    set buddy-program-k 8
    setup-buddy-connections
  ]

  if scenario-type = "CentralityBuddy" [
    ;; Phase 3 Item 10: pair buddies at week 8 (mid-programme) using
    ;; degree-based local-buddy selection instead of distance-based.
    ;; buddy-program-k=16 so the 8-week boost covers weeks 8-15
    ;; (matches BuddyProgram's 8-week effective boost duration, but at mid-programme).
    ;; The actual pairing is triggered from the go procedure at current-week = 8,
    ;; AFTER initial social structure has formed.
    set buddy-program-k 16
  ]

  if scenario-type = "RandomBuddy" [
    ;; Phase 3 Item 10 follow-up (verification round): random pairing at week 8.
    ;; Identical timing and dose to CentralityBuddy, but RANDOM local selection
    ;; instead of degree-based. Isolates the pairing-CRITERION effect from the
    ;; pairing-WINDOW effect (week 0 BuddyProgram vs week 8 buddies).
    set buddy-program-k 16
  ]

  if scenario-type = "SuboptimalOpen" [
    ;; Phase 3 Item 12 follow-up (verification round): Suboptimal Composition
    ;; combined with OpenPopulation to test whether the Baseline-vs-Suboptimal
    ;; ranking preserves under open-cohort dynamics.
    let excess-locals locals with [assigned-training-group >= 0]
    if count excess-locals > num-parks [
      ask n-of (count excess-locals - num-parks) excess-locals [
        dropout-procedure "removed-for-scenario"
        hide-turtle
      ]
    ]
    set cfg-inflow-rate-per-week  0.30
    set cfg-outflow-rate-per-week 0.20
  ]

  if scenario-type = "RotatingGroups" [
    set rotation-period-weeks 4
  ]

  if scenario-type = "Winter50" [
    ask parks [ set has-indoor-partner? false ]
    let n-indoor round (num-parks * 0.5)
    if n-indoor > 0 [ ask n-of n-indoor parks [ set has-indoor-partner? true ] ]
  ]


  if scenario-type = "Equifinality" [
    ;; No parameter change — same baseline setup, different peer influence mechanism
    ;; use-contagion-influence? is set true via BehaviorSpace enumeratedValueSet
  ]

  if scenario-type = "OpenPopulation" [
    ;; Tier 3 Block J T3.A: arrival + departure dynamics during the 52-week programme year.
    ;; Rates are weekly Bernoulli probabilities. With p_in=0.30 we expect ~15 arrivals over 52 weeks.
    ;; With p_out=0.20 we expect ~10 departures. The mechanism fires inside the go procedure.
    set cfg-inflow-rate-per-week  0.30
    set cfg-outflow-rate-per-week 0.20
  ]

  if scenario-type = "WomenChildcare" [
    ;; Women-only groups + universal childcare
    let parks-to-convert n-of (round (num-parks / 2)) parks
    ask parks-to-convert [
      set womens-only-group? true
      set color pink
    ]
    ask refugees with [gender = "female"] [
      let womens-parks parks with [womens-only-group?]
      if any? womens-parks [
        let nearest-womens min-one-of womens-parks [distance myself]
        set assigned-training-group [park-id] of nearest-womens
        set distance-to-park distance nearest-womens
      ]
      set has-childcare? true
    ]
  ]
end

;; ============================================================================
;; MAIN GO PROCEDURE (Weekly Timestep: 1 tick = 1 week)
;; ============================================================================

to go
  if current-week >= 52 [
    stop
  ]

  ;; Phase 3 Item 10: CentralityBuddy mid-programme pairing hook
  ;; Pairs each refugee with the highest-friendship-degree local in their group
  ;; once initial social structure has formed (week 8). For all other scenarios
  ;; this is a no-op with zero RNG consumption (preserves bit-identity).
  if scenario-type = "CentralityBuddy" and current-week = 8 [
    setup-centrality-buddy
  ]

  ;; Phase 3 Item 10 verification: RandomBuddy mid-programme pairing hook
  ;; Same timing as CentralityBuddy but RANDOM selection. Isolates pairing
  ;; criterion (degree vs random) from pairing window (week 0 vs week 8).
  if scenario-type = "RandomBuddy" and current-week = 8 [
    setup-random-buddy
  ]

  ;; Update season
  update-season
  if rotation-period-weeks > 0 [ check-group-rotation ]
  check-spring-reentry  ;; winter-paused agents may re-enter in spring

  ;; Tier 3 Block J T3.A: arrival and departure dynamics (Open Population scenario).
  ;; Under Baseline (both rates = 0), this is a no-op — zero RNG consumption, bit-identical to v6.4.
  apply-inflow-outflow
  
  ;; Weekly decision and participation cycle
  weekly-participation-decisions
  conduct-training-sessions  ;; 2 sessions per week
  
  ;; Social dynamics
  update-peer-influence
  update-friendship-networks
  
  ;; Language learning (refugees only)
  ask refugees with [not has-dropped-out? and current-week-attendance] [
    language-learning-from-sessions
  ]
  
  ;; Motivation decay (everyone, stronger if not attending)
  apply-motivation-decay
  
  ;; Check dropouts
  check-weekly-dropouts
  
  ;; Advance time
  set current-week current-week + 1
  
  ;; Update metrics and visualization
  update-all-metrics
  record-weekly-data
  update-visualization
  
  tick
end

;; ----------------------------------------------------------------------------
;; Update Season (Indoor weeks 9-28, Outdoor weeks 1-8 and 29-52)
;; ----------------------------------------------------------------------------

to update-season
  ;; Winter = weeks 9-28 (October → March); outdoor = weeks 1-8 and 29-52
  ifelse (current-week >= indoor-season-start and current-week <= indoor-season-end) [
    set current-season "indoor"
  ][
    set current-season "outdoor"
  ]
end

;; ----------------------------------------------------------------------------
;; Weekly Participation Decisions (Section 4.3 in outline)
;; ----------------------------------------------------------------------------

to weekly-participation-decisions
  ;; Reset weekly attendance
  ask (turtle-set refugees locals) [
    set current-week-attendance false
    set sessions-attended-this-week 0
  ]
  
  ;; Refugees decide
  ask refugees with [not has-dropped-out?] [
    decide-weekly-attendance-refugee
  ]
  
  ;; Locals decide
  ask locals with [not has-dropped-out?] [
    decide-weekly-attendance-local
  ]
end

to decide-weekly-attendance-refugee  ;; refugee procedure
  if winter-paused? [
    set current-week-attendance false
    set sessions-attended-this-week 0
    stop
  ]
  ;; Decision factors (Section 4.8 submodels in outline)
  let base-prob motivation * 0.75
  
  ;; Barriers reduce probability
  if work-hours-conflict? [set base-prob base-prob * 0.7]
  if not has-childcare? [
    ;; Childcare more critical for women (Table 4.1 heterogeneity)
    ifelse gender = "female" [
      set base-prob base-prob * 0.6  ;; 40% reduction
    ][
      set base-prob base-prob * 0.8  ;; 20% reduction
    ]
  ]
  
  ;; Distance effects (Table 4.1 - distance penalty)
  if distance-to-park > 10 [  ;; >1km (10 patches)
    set base-prob base-prob * 0.50
  ]
  if distance-to-park > 7.5 and distance-to-park <= 10 [  ;; 750m-1km
    set base-prob base-prob * 0.70
  ]
  if distance-to-park > 5 and distance-to-park <= 7.5 [  ;; 500-750m
    set base-prob base-prob * param-distance-penalty-500m  ;; 0.85
  ]
  
  ;; Support increases probability
  if receives-stipend? [set base-prob base-prob * 1.2]
  if can-access-transport? [set base-prob base-prob * 1.1]
  
  ;; Season effect (winter continuity)
  if current-season = "indoor" [
    let my-park-agent one-of parks with [park-id = [assigned-training-group] of myself]
    ifelse my-park-agent != nobody and [has-indoor-partner?] of my-park-agent [
      set base-prob base-prob * 0.90  ;; 10% reduction with indoor
    ][
      set base-prob base-prob * 0.65  ;; 35% reduction without (H3 mechanism)
    ]
  ]
  
  ;; Safety concerns for women in outdoor evening sessions
  if gender = "female" and current-season = "outdoor" and random-float 1.0 < cfg-female-outdoor-safety-prob [
    set base-prob base-prob * cfg-female-outdoor-safety-multiplier  ;; 10% reduction
  ]

  ;; Buddy programme boost: +15% if buddy is active local during programme window
  if buddy-program-k > 0 and current-week < buddy-program-k and buddy-local-id >= 0 [
    let my-buddy turtle buddy-local-id
    if my-buddy != nobody and is-local? my-buddy and not [has-dropped-out?] of my-buddy [
      set base-prob base-prob * 1.15
    ]
  ]

  ;; Decide (2 sessions per week if attending)
  set current-week-attendance (random-float 1.0 < base-prob)
  if current-week-attendance [
    set sessions-attended-this-week sessions-per-week
    set weeks-attended weeks-attended + 1
  ]
end

to decide-weekly-attendance-local  ;; local procedure
  if winter-paused? [
    set current-week-attendance false
    set sessions-attended-this-week 0
    stop
  ]
  ;; Locals have fewer barriers
  let base-prob motivation * 0.80
  
  if work-hours-conflict? [set base-prob base-prob * 0.8]
  
  ;; Distance (less affected than refugees)
  if distance-to-park > 10 [set base-prob base-prob * 0.70]
  if distance-to-park > 5 and distance-to-park <= 10 [
    set base-prob base-prob * 0.90
  ]
  
  if current-season = "indoor" [
    set base-prob base-prob * 0.93  ;; 7% reduction
  ]
  
  set current-week-attendance (random-float 1.0 < base-prob)
  if current-week-attendance [
    set sessions-attended-this-week sessions-per-week
    set weeks-attended weeks-attended + 1
  ]
end

;; ----------------------------------------------------------------------------
;; Conduct Training Sessions (2× per week)
;; ----------------------------------------------------------------------------

to conduct-training-sessions
  ;; Simulate both sessions (benefits applied per session)
  repeat sessions-per-week [
    ask (turtle-set refugees locals) with [current-week-attendance] [
      ;; Small motivation boost from exercising
      set motivation min (list 1.0 (motivation + cfg-motivation-boost-per-session))
      
      ;; Tie formation opportunity (Section 4.8.4)
      attempt-tie-formation
    ]
  ]
end

;; ----------------------------------------------------------------------------
;; Attempt Tie Formation (Section 4.8.4 - Table 4.1)
;; ----------------------------------------------------------------------------

to attempt-tie-formation  ;; turtle procedure
  if random-float 1.0 < param-tie-formation-prob [
    ;; Find potential friends in same training group who are attending
    let my-group assigned-training-group
    let potential-friends (turtle-set
      refugees with [
        assigned-training-group = my-group and
        current-week-attendance and
        self != myself and
        not friendship-neighbor? myself
      ]
      locals with [
        assigned-training-group = my-group and
        current-week-attendance and
        self != myself and
        not friendship-neighbor? myself
      ]
    )
    
    if any? potential-friends [
      let new-friend one-of potential-friends
      create-friendship-with new-friend [
        set tie-strength 0.3  ;; Start weak
        set is-cross-group? ([breed] of end1 != [breed] of end2)
        set formed-week current-week
        set weeks-active 0
        set last-contact-week current-week
        set color ifelse-value is-cross-group? [orange][gray]
        set thickness 0.2
      ]
      
      ;; Update friend lists for both
      update-friend-lists
      ask new-friend [update-friend-lists]
    ]
  ]
end

;; ----------------------------------------------------------------------------
;; Update Peer Influence (Section 4.8.2 - Table 4.1)
;; ----------------------------------------------------------------------------

to update-peer-influence
  ;; Switch between two peer influence mechanisms for equifinality check
  ifelse use-contagion-influence? [
    ;; Mechanism B: Contact-presence contagion
    ;; If >= 1 friend attended this week, agent gets a fixed boost (no tie-strength weighting)
    ask (turtle-set refugees locals) with [not has-dropped-out?] [
      receive-contagion-boost
    ]
  ][
    ;; Mechanism A (default): Tie-strength-weighted average (FIX 1)
    ask (turtle-set refugees locals) with [not has-dropped-out?] [
      receive-peer-influence-boost
    ]
  ]
end

to receive-contagion-boost  ;; turtle procedure — equifinality alternative mechanism
  ;; Simple contact-presence contagion: attending peers increase motivation by fixed amount
  ;; Produces similar macro retention as Mechanism A but different network structure
  if any? friendship-neighbors [
    let attending-friends friendship-neighbors with [current-week-attendance]
    if any? attending-friends [
      ;; Fixed boost proportional to fraction of friends attending (no tie-strength weight)
      let frac-attending count attending-friends / count friendship-neighbors
      let boost frac-attending * param-peer-influence-coef * 0.5
      set motivation min (list 1.0 max (list 0 (motivation + boost)))
    ]
  ]
end

to receive-peer-influence-boost  ;; turtle procedure
  if any? friendship-neighbors [
    ;; Get friends who are also attending this week
    let active-friends friendship-neighbors with [
      current-week-attendance
    ]
    
    if any? active-friends [
      let avg-friend-motivation mean [motivation] of active-friends
      
      ;; [FIX 1] Normalized peer influence: use mean tie-strength (not sum)
      ;; so highly-connected agents get the same per-friend nudge as isolated ones.
      ;; Formula: influence = (avg_friend_motivation - motivation) * mean_tie_strength * beta
      let active-friendships my-friendships with [[current-week-attendance] of other-end]
      let mean-active-tie-strength mean [tie-strength] of active-friendships
      let influence-effect (avg-friend-motivation - motivation) * mean-active-tie-strength * param-peer-influence-coef
      set motivation min (list 1.0 max (list 0 (motivation + influence-effect)))
    ]
  ]
end

;; ----------------------------------------------------------------------------
;; Update Friendship Networks
;; ----------------------------------------------------------------------------

to update-friendship-networks
  ask friendships [
    set weeks-active weeks-active + 1
    
    ;; Check if both ends still attending
    let both-active? (
      [current-week-attendance] of end1 and
      [current-week-attendance] of end2
    )
    
    ifelse both-active? [
      ;; Strengthen tie
      set tie-strength min (list 1.0 (tie-strength + cfg-tie-strength-growth))
      set thickness tie-strength * 0.3
      set last-contact-week current-week
    ][
      ;; Weaken tie if no contact
      let weeks-since-contact current-week - last-contact-week
      if weeks-since-contact > 2 [  ;; No contact for 2+ weeks
        set tie-strength max (list 0 (tie-strength - 0.02))
      ]
      
      ;; Remove very weak ties
      if tie-strength < 0.1 [
        ask end1 [update-friend-lists]
        ask end2 [update-friend-lists]
        die
      ]
    ]
  ]
end

;; ----------------------------------------------------------------------------
;; Language Learning (Refugees only, Section 4.8.3 - Table 4.1)
;; ----------------------------------------------------------------------------

to language-learning-from-sessions  ;; refugee procedure
  let my-group assigned-training-group
  let locals-present locals with [
    assigned-training-group = my-group and
    current-week-attendance and
    not has-dropped-out?
  ]
  
  ;; Base quality: trainer + peer learning floor when no locals present
  let interaction-quality cfg-interaction-quality-floor
  if any? locals-present [
    set interaction-quality min (list 1.0 ((count locals-present) / locals-per-group))
  ]
  
  ;; Base gain per hour
  let gain-per-hour param-language-gain-base-per-hour * interaction-quality * param-language-efficiency-multiplier
  
  ;; Friendship bonus (peer translation facilitation)
  if any? local-friends with [current-week-attendance] [
    set gain-per-hour gain-per-hour * param-language-friendship-multiplier
  ]
  
  ;; Linear ceiling factor: diminishing returns near C2
  let ceiling-factor (1.0 - language-skill-cefr / 6.0)
  let total-gain gain-per-hour * sessions-attended-this-week * session-duration-hours * ceiling-factor
  set language-skill-cefr min (list 6.0 (language-skill-cefr + total-gain))
  set language-gain-total language-gain-total + total-gain
end

;; ----------------------------------------------------------------------------
;; Apply Motivation Decay (Section 4.8.1 - Table 4.1)
;; ----------------------------------------------------------------------------

to apply-motivation-decay
  ask (turtle-set refugees locals) with [not has-dropped-out?] [
    ;; SES affects decay rate (Section 4.8.1 formula)
    let decay-rate param-motivation-decay-base * (1.2 - ses-level * 0.4)
    
    ;; Stronger decay if not attending
    if not current-week-attendance [
      set decay-rate decay-rate * 1.20  ;; 20% stronger decay when absent
    ]
    
    ;; Prior exercise experience makes more resilient (all breeds)
    if prior-exercise-experience? [
      set decay-rate decay-rate * 0.9  ;; 10% less decay
    ]
    
    set motivation max (list 0 (motivation - decay-rate))
  ]
end

;; ----------------------------------------------------------------------------
;; Check Weekly Dropouts (Section 4.8.5 - multiple triggers)
;; ----------------------------------------------------------------------------

to check-weekly-dropouts
  ask (turtle-set refugees locals) with [not has-dropped-out? and not winter-paused?] [
    
    ;; Dropout reason 1: Low motivation (Section 4.8.5, Table 4.1 threshold)
    if motivation < param-dropout-threshold and not current-week-attendance [
      ;; Prior exercise makes more resilient
      let adjusted-threshold param-dropout-threshold
      if breed = refugees [
        if prior-exercise-experience? [
          set adjusted-threshold adjusted-threshold - 0.05
        ]
      ]
      
      if motivation < adjusted-threshold [
        if random-float 1.0 < cfg-dropout-prob-motivation [  ;; cfg-dropout-prob-motivation (default 0.20)
          dropout-procedure "motivation"
          stop
        ]
      ]
    ]
    
    ;; Dropout reason 2: Work conflict without support (only when absent)
    ;; Refugees: 2%/week; Locals: 1%/week when not attending
    if work-hours-conflict? and not receives-stipend? and not current-week-attendance [
      let work-dropout-prob ifelse-value (breed = refugees) [cfg-dropout-prob-work-refugee] [cfg-dropout-prob-work-local]
      if random-float 1.0 < work-dropout-prob [
        dropout-procedure "work"
        stop
      ]
    ]
    
    ;; Dropout reason 3: Winter without indoor access → winter pause (not permanent)
    ;; Agents suspended in winter may re-enter in spring (H3 mechanism)
    if current-season = "indoor" [
      let my-park-agent one-of parks with [park-id = [assigned-training-group] of myself]
      if my-park-agent != nobody and not [has-indoor-partner?] of my-park-agent [
        if random-float 1.0 < cfg-dropout-prob-winter [  ;; cfg-dropout-prob-winter (default 0.05)
          winter-pause-procedure
          stop
        ]
      ]
    ]
    
    ;; Dropout reason 4: Distance barrier (only when absent, reflects sustained inability)
    ;; Refugees: 3%/week; Locals: 1.5%/week when not attending and no transport
    if distance-to-park > 5 and not can-access-transport? and not current-week-attendance [
      let dist-dropout-prob ifelse-value (breed = refugees) [cfg-dropout-prob-distance-refugee] [cfg-dropout-prob-distance-local]
      if random-float 1.0 < dist-dropout-prob [
        dropout-procedure "distance"
        stop
      ]
    ]
  ]
end


;; ----------------------------------------------------------------------------
;; Winter Pause Procedure (re-entry submodel — Section 4.8.5)
;; ----------------------------------------------------------------------------

to winter-pause-procedure  ;; turtle procedure
  ;; Agent suspends participation for winter — NOT a permanent dropout
  ;; Friendships are PRESERVED so social pull remains for spring re-entry
  set winter-paused? true
  set weeks-since-pause 0
  set current-week-attendance false
  set sessions-attended-this-week 0
  set color orange  ;; visual distinction: orange = paused, gray = permanently dropped
end

;; ----------------------------------------------------------------------------
;; Spring Re-Entry Check (called every outdoor week)
;; ----------------------------------------------------------------------------

to check-spring-reentry
  ;; Only fires in outdoor season (weeks 1-8 and 29-52)
  if current-season = "outdoor" [
    ask (turtle-set refugees locals) with [winter-paused? and not has-dropped-out?] [
      set weeks-since-pause weeks-since-pause + 1

      ;; Re-entry probability: base 25%, +6% per friend (capped at 70%)
      ;; Social network preserves motivation to return (Granovetter threshold)
      let reentry-prob min (list cfg-reentry-prob-cap (cfg-reentry-prob-base + total-friend-count * 0.06))

      ifelse random-float 1.0 < reentry-prob [
        ;; Re-enter: resume participation with partial motivation recovery
        set winter-paused? false
        set motivation max (list motivation (param-dropout-threshold + 0.05))
        set color ifelse-value (breed = refugees)
          [ifelse-value (gender = "female") [red - 1] [red]]
          [ifelse-value (gender = "female") [blue - 1] [blue]]
      ][
        ;; Give up permanently after 6 outdoor weeks of failed re-entry attempts
        if weeks-since-pause > cfg-winter-no-return-threshold-weeks [
          dropout-procedure "winter-no-return"
        ]
      ]
    ]
  ]
end

to dropout-procedure [reason-str]  ;; turtle procedure
  set has-dropped-out? true
  set dropout-week current-week
  set dropout-reason reason-str
  set color gray
  set current-week-attendance false
  set sessions-attended-this-week 0
  
  ;; Remove all friendships
  let former-neighbors friendship-neighbors
  ask my-friendships [die]
  update-friend-lists
  ask former-neighbors [update-friend-lists]
end

;; ----------------------------------------------------------------------------
;; Helper: Update Friend Lists
;; ----------------------------------------------------------------------------

to update-friend-lists  ;; turtle procedure
  if breed = refugees [
    set local-friends turtle-set [other-end] of my-friendships with [[breed] of other-end = locals]
    set refugee-friends turtle-set [other-end] of my-friendships with [[breed] of other-end = refugees]
    set cross-group-friend-count count local-friends
  ]
  if breed = locals [
    set refugee-friends turtle-set [other-end] of my-friendships with [[breed] of other-end = refugees]
    set local-friends turtle-set [other-end] of my-friendships with [[breed] of other-end = locals]
    set cross-group-friend-count count refugee-friends
  ]
  set total-friend-count count my-friendships
end

;; ============================================================================
;; METRICS AND TRACKING (Section 5.3 in outline)
;; ============================================================================

to update-all-metrics
  ;; === BASIC PARTICIPATION ===
  set total-active-participants count (turtle-set refugees locals) with [
    not has-dropped-out? and
    current-week-attendance
  ]
  
  set total-refugees-active count refugees with [
    not has-dropped-out? and current-week-attendance
  ]
  
  set total-locals-active count locals with [
    not has-dropped-out? and current-week-attendance
  ]
  
  ;; === AVERAGE MOTIVATION ===
  let active-agents (turtle-set refugees locals) with [not has-dropped-out?]
  ifelse any? active-agents [
    set avg-motivation-level mean [motivation] of active-agents
  ][
    set avg-motivation-level 0
  ]
  
  ;; === AVERAGE LANGUAGE (refugees only) ===
  let active-refugees refugees with [not has-dropped-out?]
  ifelse any? active-refugees [
    set avg-language-proficiency mean [language-skill-cefr] of active-refugees
  ][
    set avg-language-proficiency 0
  ]
  
  ;; === CROSS-GROUP TIES ===
  set cross-group-tie-count count friendships with [is-cross-group?]
  let total-ties count friendships
  ifelse total-ties > 0 [
    set cross-group-tie-ratio (cross-group-tie-count / total-ties)
  ][
    set cross-group-tie-ratio 0
  ]
  
  ;; === DROPOUTS ===
  set total-dropouts count (turtle-set refugees locals) with [has-dropped-out?]
  set winter-paused-count count (turtle-set refugees locals) with [winter-paused? and not has-dropped-out?]
  set total-refugees-dropouts count refugees with [has-dropped-out?]
  set total-locals-dropouts count locals with [has-dropped-out?]
  
  ;; === COST PER PARTICIPANT ===
  let total-budget (annual-budget-per-park * num-parks)
  let retained count (turtle-set refugees locals) with [not has-dropped-out?]
  ifelse retained > 0 [
    set cost-per-participant-retained (total-budget / retained)
  ][
    set cost-per-participant-retained total-budget
  ]
  
  ;; === HETEROGENEITY METRICS ===
  
  ;; Gender participation rates
  let total-females count (turtle-set refugees locals) with [
    gender = "female" and not has-dropped-out?
  ]
  let active-females count (turtle-set refugees locals) with [
    gender = "female" and not has-dropped-out? and current-week-attendance
  ]
  ifelse total-females > 0 [
    set female-participation-rate (active-females / total-females) * 100
  ][
    set female-participation-rate 0
  ]
  
  let total-males count (turtle-set refugees locals) with [
    gender = "male" and not has-dropped-out?
  ]
  let active-males count (turtle-set refugees locals) with [
    gender = "male" and not has-dropped-out? and current-week-attendance
  ]
  ifelse total-males > 0 [
    set male-participation-rate (active-males / total-males) * 100
  ][
    set male-participation-rate 0
  ]
  
  ;; Gender dropout rates
  let total-females-enrolled count (turtle-set refugees locals) with [gender = "female"]
  let females-dropped count (turtle-set refugees locals) with [
    gender = "female" and has-dropped-out?
  ]
  ifelse total-females-enrolled > 0 [
    set female-dropout-rate (females-dropped / total-females-enrolled) * 100
  ][
    set female-dropout-rate 0
  ]
  
  let total-males-enrolled count (turtle-set refugees locals) with [gender = "male"]
  let males-dropped count (turtle-set refugees locals) with [
    gender = "male" and has-dropped-out?
  ]
  ifelse total-males-enrolled > 0 [
    set male-dropout-rate (males-dropped / total-males-enrolled) * 100
  ][
    set male-dropout-rate 0
  ]
  
  ;; Arrival cohort language gains
  let recent-refugees refugees with [arrival-cohort = "recent" and not has-dropped-out?]
  ifelse any? recent-refugees [
    set recent-cohort-language-gain mean [language-gain-total] of recent-refugees
  ][
    set recent-cohort-language-gain 0
  ]
  
  let established-refugees refugees with [arrival-cohort = "established" and not has-dropped-out?]
  ifelse any? established-refugees [
    set established-cohort-language-gain mean [language-gain-total] of established-refugees
  ][
    set established-cohort-language-gain 0
  ]
  
  let settled-refugees refugees with [arrival-cohort = "settled" and not has-dropped-out?]
  ifelse any? settled-refugees [
    set settled-cohort-language-gain mean [language-gain-total] of settled-refugees
  ][
    set settled-cohort-language-gain 0
  ]
  
  ;; Prior exercise retention
  let prior-exercise-total count (turtle-set refugees locals) with [prior-exercise-experience?]
  let prior-exercise-retained count (turtle-set refugees locals) with [
    prior-exercise-experience? and not has-dropped-out?
  ]
  ifelse prior-exercise-total > 0 [
    set prior-exercise-retention-rate (prior-exercise-retained / prior-exercise-total) * 100
  ][
    set prior-exercise-retention-rate 0
  ]
  
  let no-exercise-total count (turtle-set refugees locals) with [not prior-exercise-experience?]
  let no-exercise-retained count (turtle-set refugees locals) with [
    not prior-exercise-experience? and not has-dropped-out?
  ]
  ifelse no-exercise-total > 0 [
    set no-exercise-retention-rate (no-exercise-retained / no-exercise-total) * 100
  ][
    set no-exercise-retention-rate 0
  ]
  
  ;; === TARGET ACHIEVEMENT (Section 5.3) ===
  update-target-achievements
end

to update-target-achievements
  ;; Target 1: Motivation ≥0.7 after week 24 (6 months)
  set meets-motivation-target? (current-week >= 24 and avg-motivation-level >= 0.7)
  
  ;; Target 2: ≥50% of refugees reach A2 (2.0)
  let refugees-at-a2-plus count refugees with [
    not has-dropped-out? and language-skill-cefr >= 2.0
  ]
  let total-active-refugees count refugees with [not has-dropped-out?]
  let percent-at-a2 0
  if total-active-refugees > 0 [
    set percent-at-a2 (refugees-at-a2-plus / total-active-refugees) * 100
  ]
  set meets-language-target? (avg-language-proficiency >= 1.0)  ;; matches thesis TARGET_LANGUAGE = 1.0
  
  ;; Target 3: Cross-group tie ratio ≥40%
  set meets-integration-target? (cross-group-tie-ratio >= 0.4)
  
  ;; Target 4: Attendance rate ≥75%
  let total-people count (turtle-set refugees locals) with [not has-dropped-out?]
  let attending count (turtle-set refugees locals) with [
    not has-dropped-out? and current-week-attendance
  ]
  let attendance-pct 0
  if total-people > 0 [
    set attendance-pct (attending / total-people) * 100
  ]
  set meets-attendance-target? (attendance-pct >= 75)
  
  ;; Target 5: Cost ≤€3,000 per retained participant
  set meets-cost-target? (cost-per-participant-retained <= 3500)  ;; matches thesis TARGET_COST = 3500
  
  ;; Overall success: 4+ targets met
  let targets-met 0
  if meets-motivation-target? [set targets-met targets-met + 1]
  if meets-language-target? [set targets-met targets-met + 1]
  if meets-integration-target? [set targets-met targets-met + 1]
  if meets-attendance-target? [set targets-met targets-met + 1]
  if meets-cost-target? [set targets-met targets-met + 1]
  
  set overall-success? (targets-met >= 4)
end

to record-weekly-data
  set weekly-participation-list lput total-active-participants weekly-participation-list
  set weekly-motivation-list lput avg-motivation-level weekly-motivation-list
  set weekly-language-list lput avg-language-proficiency weekly-language-list
  set weekly-integration-list lput cross-group-tie-ratio weekly-integration-list
  set weekly-dropouts-list lput total-dropouts weekly-dropouts-list
  set weekly-cost-list lput cost-per-participant-retained weekly-cost-list
  set weekly-female-participation-list lput female-participation-rate weekly-female-participation-list
  set weekly-male-participation-list lput male-participation-rate weekly-male-participation-list

  ;; Per-agent trajectory tracking (v6.3): append motivation for active agents
  ask refugees with [not has-dropped-out?] [
    set motivation-trajectory lput motivation motivation-trajectory
  ]
end

;; ============================================================================
;; VISUALIZATION
;; ============================================================================

to update-visualization
  ;; Color agents by motivation level
  ask refugees with [not has-dropped-out?] [
    ifelse gender = "female" [
      set color scale-color (red - 1) motivation 0 1
    ][
      set color scale-color red motivation 0 1
    ]
  ]
  
  ask locals with [not has-dropped-out?] [
    ifelse gender = "female" [
      set color scale-color (blue - 1) motivation 0 1
    ][
      set color scale-color blue motivation 0 1
    ]
  ]
  
  ;; Update park colors by activity
  ask parks [
    let active-here count (turtle-set
      refugees with [
        assigned-training-group = [park-id] of myself and
        not has-dropped-out? and
        current-week-attendance
      ]
      locals with [
        assigned-training-group = [park-id] of myself and
        not has-dropped-out? and
        current-week-attendance
      ]
    )
    set current-active-count active-here
    set color scale-color green active-here 0 group-target-size
  ]
  
  ;; Update friendships visibility
  ask friendships [
    ifelse is-cross-group? [
      set color orange
      set thickness tie-strength * 0.4
    ][
      set color gray
      set thickness tie-strength * 0.2
    ]
  ]
end

;; ============================================================================
;; REPORTERS (For Interface Monitors and Plots)
;; ============================================================================

to-report attendance-rate
  let total count (turtle-set refugees locals) with [not has-dropped-out?]
  ifelse total > 0 [
    report (total-active-participants / total) * 100
  ][
    report 0
  ]
end

to-report refugee-participation-rate
  let total count refugees with [not has-dropped-out?]
  let active count refugees with [not has-dropped-out? and current-week-attendance]
  ifelse total > 0 [
    report (active / total) * 100
  ][
    report 0
  ]
end

to-report local-participation-rate
  let total count locals with [not has-dropped-out?]
  let active count locals with [not has-dropped-out? and current-week-attendance]
  ifelse total > 0 [
    report (active / total) * 100
  ][
    report 0
  ]
end

to-report language-a2-percentage
  let total count refugees with [not has-dropped-out?]
  let at-a2 count refugees with [not has-dropped-out? and language-skill-cefr >= 2.0]
  ifelse total > 0 [
    report (at-a2 / total) * 100
  ][
    report 0
  ]
end

to-report integration-index
  ;; Composite measure (weighted average)
  report (cross-group-tie-ratio * 0.4 +
          (avg-language-proficiency / 6) * 0.3 +
          (attendance-rate / 100) * 0.3)
end

to-report total-program-cost
  report annual-budget-per-park * num-parks
end

to-report dropout-rate-percent
  let total count (turtle-set refugees locals)
  ifelse total > 0 [
    report (total-dropouts / total) * 100
  ][
    report 0
  ]
end

to-report retention-rate-percent
  report 100 - dropout-rate-percent
end

to-report total-participants
  report count (turtle-set refugees locals)
end

to-report total-retained
  report count (turtle-set refugees locals) with [not has-dropped-out?]
end




;; ============================================================================
;; TIER 3 BLOCK J T3.A: OPEN POPULATION (inflow/outflow dynamics)
;; ============================================================================
;; Addresses the 8-classmate static-population convergent critique. Under the
;; OpenPopulation scenario, new refugees arrive at rate cfg-inflow-rate-per-week
;; and active refugees depart at rate cfg-outflow-rate-per-week. Rates are weekly
;; Bernoulli probabilities. Under any scenario with rates = 0 (including Baseline),
;; the procedure is a no-op with zero RNG consumption, preserving bit-identity to v6.4.
;; ============================================================================

to apply-inflow-outflow
  ;; Inflow: prob of 1 new refugee arrival this week
  if cfg-inflow-rate-per-week > 0 [
    if random-float 1.0 < cfg-inflow-rate-per-week [
      create-refugees 1 [ recruit-new-arrival ]
    ]
  ]
  ;; Outflow: prob of 1 random active refugee departure this week
  if cfg-outflow-rate-per-week > 0 [
    if random-float 1.0 < cfg-outflow-rate-per-week [
      let eligible refugees with [not has-dropped-out? and not winter-paused?]
      if any? eligible [
        ask one-of eligible [
          dropout-procedure "departure"
          hide-turtle
        ]
      ]
    ]
  ]
end

to recruit-new-arrival  ;; refugee procedure
  ;; Simplified recruitment for mid-programme arrivals. Reuses heterogeneity
  ;; parameters from pilot recruitment (prior-exercise-probability, childcare/transport
  ;; init, SES, etc.). All new arrivals are "recent" cohort (just arrived in country).
  set participant-id who
  set gender one-of ["male" "female"]
  set months-in-country random-float 3.0
  set arrival-cohort "recent"
  set prior-exercise-experience? (random-float 1.0 < prior-exercise-probability)
  set language-skill-cefr random-float 0.5
  set initial-language language-skill-cefr
  set language-trajectory (list language-skill-cefr)
  set language-gain-total 0
  set motivation 0.3 + random-float 0.5
  if prior-exercise-experience? [ set motivation motivation + 0.10 ]
  set motivation min (list 1.0 motivation)
  set initial-motivation motivation
  set motivation-trajectory (list motivation)
  set ses-level random-float 1.0
  set work-hours-conflict? (random-float 1.0 < (0.6 - ses-level * 0.3))
  set has-childcare? (random-float 1.0 < cfg-refugee-initial-childcare-prob)
  set can-access-transport? (random-float 1.0 < cfg-refugee-initial-transport-prob)
  set receives-stipend? false
  set weeks-attended 0
  set current-week-attendance false
  set sessions-attended-this-week 0
  set home-x random-xcor
  set home-y random-ycor
  setxy home-x home-y
  set shape "person"
  set size 1.5
  ifelse gender = "female" [ set color red - 1 ] [ set color red ]
  set local-friends no-turtles
  set refugee-friends no-turtles
  set total-friend-count 0
  set cross-group-friend-count 0
  set has-dropped-out? false
  set dropout-week -1
  set dropout-reason "none"
  set winter-paused? false
  set weeks-since-pause 0
  ;; Assign to nearest park
  let nearest-park min-one-of parks [distance myself]
  if nearest-park != nobody [
    set assigned-training-group [park-id] of nearest-park
    set distance-to-park distance nearest-park
  ]
  set buddy-local-id -1
end

;; ============================================================================
;; BUDDY PROGRAMME SETUP (v6.3)
;; ============================================================================

to setup-buddy-connections
  ;; Assign each refugee one local buddy from the same training group
  ask refugees [
    set buddy-local-id -1
    let group-locals locals with [
      assigned-training-group = [assigned-training-group] of myself
      and not has-dropped-out?
    ]
    if any? group-locals [
      let my-buddy min-one-of group-locals [distance myself]
      set buddy-local-id [who] of my-buddy
      ;; Create mandated buddy friendship link
      if not friendship-neighbor? my-buddy [
        create-friendship-with my-buddy [
          set tie-strength 0.20
          set is-cross-group? true
          set formed-week current-week
          set weeks-active 0
          set last-contact-week current-week
          set color orange
          set thickness 0.2
        ]
        update-friend-lists
        ask my-buddy [update-friend-lists]
      ]
    ]
  ]
end

;; ============================================================================
;; CENTRALITY BUDDY PROGRAMME (Phase 3 Item 10)
;; ============================================================================
;; Pairs each non-dropped refugee with the highest-friendship-degree local in
;; their training group at week 8 (mid-programme), once initial social
;; structure has formed. Differs from setup-buddy-connections (which uses
;; physical distance) by using degree centrality as the matching criterion.
;; ============================================================================

to setup-centrality-buddy
  ask refugees with [not has-dropped-out?] [
    set buddy-local-id -1
    let group-locals locals with [
      assigned-training-group = [assigned-training-group] of myself
      and not has-dropped-out?
    ]
    if any? group-locals [
      ;; Pick highest-friendship-degree local in group
      let my-buddy max-one-of group-locals [count my-friendships]
      set buddy-local-id [who] of my-buddy
      ;; Create mandated buddy friendship link if not already friends
      if not friendship-neighbor? my-buddy [
        create-friendship-with my-buddy [
          set tie-strength 0.20
          set is-cross-group? true
          set formed-week current-week
          set weeks-active 0
          set last-contact-week current-week
          set color orange
          set thickness 0.2
        ]
        update-friend-lists
        ask my-buddy [update-friend-lists]
      ]
    ]
  ]
end

;; ============================================================================
;; RANDOM BUDDY PROGRAMME (Phase 3 Item 10 verification round)
;; ============================================================================
;; Pairs each non-dropped refugee with a RANDOM local in their training group
;; at week 8 (same timing as CentralityBuddy). Isolates the pairing-CRITERION
;; effect (degree-based vs random) from the pairing-WINDOW effect (week 0 vs 8).
;; ============================================================================

to setup-random-buddy
  ask refugees with [not has-dropped-out?] [
    set buddy-local-id -1
    let group-locals locals with [
      assigned-training-group = [assigned-training-group] of myself
      and not has-dropped-out?
    ]
    if any? group-locals [
      ;; Pick a RANDOM local in the group (vs max-degree for CentralityBuddy)
      let my-buddy one-of group-locals
      set buddy-local-id [who] of my-buddy
      if not friendship-neighbor? my-buddy [
        create-friendship-with my-buddy [
          set tie-strength 0.20
          set is-cross-group? true
          set formed-week current-week
          set weeks-active 0
          set last-contact-week current-week
          set color orange
          set thickness 0.2
        ]
        update-friend-lists
        ask my-buddy [update-friend-lists]
      ]
    ]
  ]
end

;; ============================================================================
;; GROUP ROTATION (v6.3)
;; ============================================================================

to check-group-rotation
  ;; Rotate refugee group assignments every rotation-period-weeks weeks
  if current-week > 0 and (current-week mod rotation-period-weeks) = 0 [
    ask refugees with [not has-dropped-out?] [
      let new-group (assigned-training-group + 1) mod num-parks
      set assigned-training-group new-group
      let new-park one-of parks with [park-id = new-group]
      if new-park != nobody [ set distance-to-park distance new-park ]
    ]
  ]
end

;; ============================================================================
;; EXPORT: AGENT-WEEK PANEL (v6.3)
;; Person-period format for discrete-time hazard models in R
;; ============================================================================

to export-agent-panel
  let run-id (behaviorspace-run-number + run-start-index)
  let filename (word "data/" scenario-type "/CIM_panel_" scenario-type "_" run-id ".csv")
  file-open filename
  file-print "run,scenario,agent_id,week,motivation,event,is_female,ses_level,arrival_cohort,prior_exercise,distance"
  ask refugees [
    let traj motivation-trajectory
    let n-traj length traj
    let w 0
    while [w < n-traj] [
      file-print (word
        run-id "," scenario-type ","
        participant-id ","
        w "," precision (item w traj) 3 ","
        0 ","
        ifelse-value (gender = "female") [1] [0] ","
        precision ses-level 3 ","
        arrival-cohort ","
        ifelse-value prior-exercise-experience? [1] [0] ","
        precision distance-to-park 2
      )
      set w w + 1
    ]
    ;; Dropout event row
    if has-dropped-out? and dropout-week >= 0 [
      file-print (word
        run-id "," scenario-type ","
        participant-id ","
        dropout-week "," precision (ifelse-value (n-traj > 0) [last traj] [motivation]) 3 ","
        1 ","
        ifelse-value (gender = "female") [1] [0] ","
        precision ses-level 3 ","
        arrival-cohort ","
        ifelse-value prior-exercise-experience? [1] [0] ","
        precision distance-to-park 2
      )
    ]
  ]
  file-close
  print (word "Agent panel exported to: " filename)
end

;; ============================================================================
;; EXPORT: FRIENDSHIP EDGE LIST (v6.3)
;; ============================================================================

to export-edge-list
  let run-id (behaviorspace-run-number + run-start-index)
  let filename (word "data/" scenario-type "/CIM_edges_" scenario-type "_" run-id ".csv")
  file-open filename
  file-print "run,scenario,end1_id,end2_id,end1_breed,end2_breed,tie_strength,is_cross_group,formed_week,weeks_active"
  ask friendships [
    let b1 ifelse-value (is-refugee? end1) ["refugee"] ["local"]
    let b2 ifelse-value (is-refugee? end2) ["refugee"] ["local"]
    file-print (word
      run-id "," scenario-type ","
      [participant-id] of end1 "," [participant-id] of end2 ","
      b1 "," b2 ","
      precision tie-strength 3 ","
      is-cross-group? "," formed-week "," weeks-active
    )
  ]
  file-close
  print (word "Edge list exported to: " filename)
end

;; ============================================================================
;; EXPORT PROCEDURES (For BehaviorSpace and Analysis)
;; ============================================================================

to export-final-results
  let run-id (behaviorspace-run-number + run-start-index)
  let filename (word "data/" scenario-type "/CIM_results_" scenario-type "_" run-id ".csv")
  
  file-open filename
  file-print "metric,value"
  file-print (word "scenario," scenario-type)
  file-print (word "run," run-id)
  file-print (word "final_week," current-week)
  file-print (word "retention_rate," retention-rate-percent)
  file-print (word "avg_motivation," avg-motivation-level)
  file-print (word "avg_language_cefr," avg-language-proficiency)
  file-print (word "cross_group_tie_ratio," cross-group-tie-ratio)
  file-print (word "total_dropouts," total-dropouts)
  file-print (word "cost_per_retained," cost-per-participant-retained)
  file-print (word "female_dropout_rate," female-dropout-rate)
  file-print (word "male_dropout_rate," male-dropout-rate)
  file-print (word "recent_cohort_lang_gain," recent-cohort-language-gain)
  file-print (word "established_cohort_lang_gain," established-cohort-language-gain)
  file-print (word "settled_cohort_lang_gain," settled-cohort-language-gain)
  file-print (word "prior_exercise_retention," prior-exercise-retention-rate)
  file-print (word "no_exercise_retention," no-exercise-retention-rate)
  file-print (word "winter_paused_count," winter-paused-count)
  file-print (word "overall_success," ifelse-value overall-success? [1] [0])
  
  ;; [FIX 3] Stabilization-window averages: mean over weeks 46-52 (last 7 ticks)
  ;; Reduces single-tick noise; Izquierdo IV-5 recommends measuring at stationarity.
  let list-length length weekly-participation-list
  let start-idx max (list 0 (list-length - 7))
  let stable-participation mean sublist weekly-participation-list start-idx list-length
  let stable-motivation    mean sublist weekly-motivation-list    start-idx list-length
  let stable-language      mean sublist weekly-language-list      start-idx list-length
  let stable-integration   mean sublist weekly-integration-list   start-idx list-length
  file-print (word "stable_participation_wk46_52," stable-participation)
  file-print (word "stable_motivation_wk46_52,"    stable-motivation)
  file-print (word "stable_language_wk46_52,"      stable-language)
  file-print (word "stable_integration_wk46_52,"   stable-integration)
  
  file-close
  
  print (word "Results exported to: " filename)
end

to export-weekly-timeseries
  let run-id (behaviorspace-run-number + run-start-index)
  let filename (word "data/" scenario-type "/CIM_timeseries_" scenario-type "_" run-id ".csv")
  
  file-open filename
  file-print "week,participation,motivation,language,integration,dropouts,cost"
  
  let i 0
  while [i < length weekly-participation-list] [
    file-print (word
      i ","
      item i weekly-participation-list ","
      item i weekly-motivation-list ","
      item i weekly-language-list ","
      item i weekly-integration-list ","
      item i weekly-dropouts-list ","
      item i weekly-cost-list
    )
    set i i + 1
  ]
  
  file-close
  print (word "Timeseries exported to: " filename)
end

to export-agent-crosssection  ;; agent-level CSV for R survival / hazard models
  let run-id (behaviorspace-run-number + run-start-index)
  let filename (word "data/" scenario-type "/CIM_agents_" scenario-type "_" run-id ".csv")
  file-open filename
  file-print "run,scenario,breed,gender,ses,arrival_cohort,prior_exercise,initial_motivation,final_motivation,weeks_attended,dropped_out,dropout_week,dropout_reason,language_gain,cross_group_friends,distance_to_park"
  ask refugees [
    file-print (word
      run-id "," scenario-type ","
      "refugee" "," gender "," precision ses-level 3 ","
      arrival-cohort "," prior-exercise-experience? ","
      precision initial-motivation 3 "," precision motivation 3 ","
      weeks-attended "," has-dropped-out? ","
      dropout-week "," dropout-reason ","
      precision language-gain-total 4 ","
      cross-group-friend-count "," precision distance-to-park 2
    )
  ]
  ask locals [
    file-print (word
      run-id "," scenario-type ","
      "local" "," gender "," precision ses-level 3 ","
      "NA" "," prior-exercise-experience? ","
      precision initial-motivation 3 "," precision motivation 3 ","
      weeks-attended "," has-dropped-out? ","
      dropout-week "," dropout-reason ","
      0 ","
      cross-group-friend-count "," precision distance-to-park 2
    )
  ]
  file-close
  print (word "Agent crosssection exported to: " filename)
end

;; ============================================================================
;; END OF CODE
;; ============================================================================


;; ============================================================================
;; TIER 2 (plan T2.3): CONFIGURATION-FILE LOADER
;; ============================================================================
;; load-config reads config/<domain>.csv at setup time and overrides hardcoded
;; cfg-* defaults. Absence of the file leaves defaults in effect, producing
;; bit-identical behaviour to v6.3 on seeded runs.
;;
;; File format: name,value,unit,range,source,description,calibration-tier,scenario-scope
;; Only name and value are consumed; other columns are documentation.
;; Slider-backed globals (num-parks, refugees-per-group, locals-per-group,
;; motivation-decay-rate, peer-influence-coefficient, language-gain-rate-per-hour,
;; language-efficiency-multiplier, language-friendship-multiplier,
;; tie-formation-probability, dropout-threshold) are NOT overridden here, so that
;; BehaviorSpace enumeratedValueSet overrides retain precedence.
;; ============================================================================

to load-config [filename]
  ;; Defensive loader: every I/O step wrapped in carefully so that a missing or
  ;; unresolvable path (e.g., NetLogo working-directory mismatch, Unicode path quirk)
  ;; degrades to hardcoded defaults rather than halting setup. The hardcoded defaults
  ;; in setup are bit-identical to v6.3, so v6.4 behaviour is preserved when loading fails.
  let n-loaded 0
  let n-skipped-slider 0
  let n-unknown 0
  let load-failed? false
  let fail-reason ""

  ;; Step 1: existence check (cheap pre-filter).
  carefully [
    if not file-exists? filename [
      set load-failed? true
      set fail-reason (word "file-exists? returned false for '" filename "'")
    ]
  ] [
    set load-failed? true
    set fail-reason (word "file-exists? raised: " error-message)
  ]

  ;; Step 2: if existence check passed, try reading.
  if not load-failed? [
    carefully [
      file-open filename
      ;; Skip header row if present
      if not file-at-end? [ let _header file-read-line ]
      while [not file-at-end?] [
        let line file-read-line
        if is-string? line and length line > 0 [
          let fields csv-split-line line
          if length fields >= 2 [
            let key trim-string item 0 fields
            let raw-val trim-string item 1 fields
            let status apply-config-value key raw-val
            ifelse status = "loaded" [ set n-loaded n-loaded + 1 ] [
              ifelse status = "slider" [ set n-skipped-slider n-skipped-slider + 1 ] [
                set n-unknown n-unknown + 1
                print (word "[CIM v6.4] Unknown config key skipped: '" key "'")
              ]
            ]
          ]
        ]
      ]
      file-close
    ] [
      set load-failed? true
      set fail-reason (word "read error: " error-message)
      carefully [ file-close ] [ ]
    ]
  ]

  ;; Step 3: report outcome and return.
  ifelse load-failed? [
    print (word "[CIM v6.4] Config file '" filename "' not read (" fail-reason "); the inline config values for the current domain are in effect. To read an edited CSV, run headless or relocate the model out of a macOS-protected folder (e.g. ~/Downloads).")
  ] [
    set config-file-loaded? true
    print (word "[CIM v6.4] Config '" filename "' loaded: " n-loaded " parameters set, " n-skipped-slider " slider-backed keys respected, " n-unknown " unknown keys ignored.")
  ]
end

to-report csv-split-line [str]
  ;; Basic CSV split on commas. Does not support quoted fields (not needed for our schema).
  let result []
  let current ""
  let i 0
  while [i < length str] [
    let ch substring str i (i + 1)
    ifelse ch = "," [
      set result lput current result
      set current ""
    ] [
      set current (word current ch)
    ]
    set i i + 1
  ]
  set result lput current result
  report result
end

to-report trim-string [s]
  ;; Trim leading/trailing spaces. NetLogo has no built-in trim.
  let start 0
  let end-pos length s
  while [start < end-pos and (substring s start (start + 1)) = " "] [ set start start + 1 ]
  while [end-pos > start and (substring s (end-pos - 1) end-pos) = " "] [ set end-pos end-pos - 1 ]
  report substring s start end-pos
end

to import-config
  ;; GUI "Load Config CSV" button: pick a CSV via the native file dialog, copy it into
  ;; config/custom.csv, switch to the "custom" domain, and re-run setup so it takes effect.
  ;; Requires write access to config/ (works headless and outside macOS-protected folders;
  ;; under ~/Downloads the GUI may block the write, in which case it reports and warns).
  let f user-file
  if not is-string? f [ stop ]                 ;; user cancelled the dialog
  let n apply-imported-config f
  ifelse n >= 0 [
    setup
    user-message (word "Imported " n " rows from:\n" f "\n\nDomain set to \"custom\" and setup re-run -- now click go.")
  ] [
    user-message "Could not write config/custom.csv. macOS may be blocking writes under ~/Downloads: move the model+config out of ~/Downloads (e.g. ~/Documents) or grant NetLogo Full Disk Access, then retry."
  ]
end

to-report apply-imported-config [src]
  ;; Reads the CSV at `src`, writes it verbatim to config/custom.csv, and selects the
  ;; "custom" domain. Returns the number of rows written, or -1 on any I/O failure.
  ;; Pure file I/O + chooser set -- no interactive parts, so it is testable headless.
  let lines []
  let ok? true
  carefully [
    file-open src
    while [not file-at-end?] [ set lines lput file-read-line lines ]
    file-close
  ] [ set ok? false  carefully [ file-close ] [ ] ]
  if not ok? [ report -1 ]
  carefully [
    if file-exists? "config/custom.csv" [ file-delete "config/custom.csv" ]
    file-open "config/custom.csv"
    foreach lines [ row -> file-print row ]
    file-close
  ] [ set ok? false  carefully [ file-close ] [ ] ]
  if not ok? [ report -1 ]
  set config-domain "custom"
  report length lines
end

to-report custom-domain-active?
  ;; A custom (non-built-in) domain is active when config-domain is neither shipped preset.
  ;; In that case the on-disk CSV is the full source of truth: apply-config-value applies
  ;; even the slider-backed keys (below), including under BehaviorSpace. The two presets
  ;; (calisthenics-istanbul, language-course-berlin) keep the skip so the 36 shipped
  ;; experiments remain bit-identical (their slider params come from enumeratedValueSet).
  report not member? config-domain ["calisthenics-istanbul" "language-course-berlin"]
end

to-report apply-config-value [key raw-val]
  ;; Returns "loaded", "slider" (skipped), or "unknown".
  ;; Slider-backed keys that BehaviorSpace may override: skip.
  ;; Slider-backed keys. Under BehaviorSpace, skip them so enumeratedValueSet overrides
  ;; retain precedence (preserves all experiment results bit-identically). Outside
  ;; BehaviorSpace (GUI / headless third-party use), fall through and set them from the
  ;; config file so an outside researcher can fully configure the model from their CSV.
  if member? key ["num-parks" "refugees-per-group" "locals-per-group"
                  "motivation-decay-rate" "peer-influence-coefficient"
                  "language-gain-rate-per-hour" "language-efficiency-multiplier"
                  "language-friendship-multiplier" "tie-formation-probability"
                  "dropout-threshold"] [
    if (behaviorspace-run-number > 0) and (not custom-domain-active?) [ report "slider" ]
  ]
  ;; Parse numeric value
  let v 0
  let parse-ok? false
  carefully [
    set v read-from-string raw-val
    set parse-ok? true
  ] [
    print (word "[CIM v6.4] Could not parse value '" raw-val "' for key '" key "'")
  ]
  if not parse-ok? [ report "unknown" ]

  ;; Dispatch on key name. Each stanza ends with `report "loaded"`.
  if key = "sessions-per-week" [ set sessions-per-week v report "loaded" ]
  if key = "session-duration-hours" [ set session-duration-hours v report "loaded" ]
  if key = "group-target-size" [ set group-target-size v report "loaded" ]
  if key = "annual-budget-per-park" [ set cfg-annual-budget-per-park v set annual-budget-per-park v report "loaded" ]
  if key = "indoor-season-start" [ set cfg-indoor-season-start v set indoor-season-start v report "loaded" ]
  if key = "indoor-season-end" [ set cfg-indoor-season-end v set indoor-season-end v report "loaded" ]
  if key = "motivation-boost-per-session" [ set cfg-motivation-boost-per-session v report "loaded" ]
  if key = "indoor-facility-probability" [ set cfg-indoor-facility-probability v report "loaded" ]
  if key = "refugee-initial-childcare-prob" [ set cfg-refugee-initial-childcare-prob v report "loaded" ]
  if key = "refugee-initial-transport-prob" [ set cfg-refugee-initial-transport-prob v report "loaded" ]
  if key = "local-initial-childcare-prob" [ set cfg-local-initial-childcare-prob v report "loaded" ]
  if key = "local-initial-transport-prob" [ set cfg-local-initial-transport-prob v report "loaded" ]
  if key = "refugee-initial-tie-probability" [ set cfg-refugee-initial-tie-prob v report "loaded" ]
  if key = "local-initial-tie-probability" [ set cfg-local-initial-tie-prob v report "loaded" ]
  if key = "initial-tie-strength" [ set cfg-initial-tie-strength v report "loaded" ]
  if key = "tie-strength-growth" [ set cfg-tie-strength-growth v report "loaded" ]
  if key = "interaction-quality-floor" [ set cfg-interaction-quality-floor v report "loaded" ]
  if key = "female-outdoor-safety-prob" [ set cfg-female-outdoor-safety-prob v report "loaded" ]
  if key = "female-outdoor-safety-multiplier" [ set cfg-female-outdoor-safety-multiplier v report "loaded" ]
  if key = "dropout-prob-motivation" [ set cfg-dropout-prob-motivation v report "loaded" ]
  if key = "dropout-prob-work-refugee" [ set cfg-dropout-prob-work-refugee v report "loaded" ]
  if key = "dropout-prob-work-local" [ set cfg-dropout-prob-work-local v report "loaded" ]
  if key = "dropout-prob-winter" [ set cfg-dropout-prob-winter v report "loaded" ]
  if key = "dropout-prob-distance-refugee" [ set cfg-dropout-prob-distance-refugee v report "loaded" ]
  if key = "dropout-prob-distance-local" [ set cfg-dropout-prob-distance-local v report "loaded" ]
  if key = "reentry-prob-base" [ set cfg-reentry-prob-base v report "loaded" ]
  if key = "reentry-prob-cap" [ set cfg-reentry-prob-cap v report "loaded" ]
  if key = "winter-no-return-threshold-weeks" [ set cfg-winter-no-return-threshold-weeks v report "loaded" ]
  if key = "weak-peer-beta" [ set cfg-weak-peer-beta v report "loaded" ]
  if key = "inflow-rate-per-week" [ set cfg-inflow-rate-per-week v report "loaded" ]
  if key = "outflow-rate-per-week" [ set cfg-outflow-rate-per-week v report "loaded" ]
  if key = "distance-penalty-500m" [ set param-distance-penalty-500m v report "loaded" ]
  if key = "prior-exercise-probability" [ set prior-exercise-probability v report "loaded" ]
  if key = "arrival-cohort-mean-months" [ set arrival-cohort-mean-months v report "loaded" ]

  ;; Slider-backed keys (only reached OUTSIDE BehaviorSpace; see guard above). Set both
  ;; the slider global AND its param-* working var, because setup copies slider->param at
  ;; lines 367-373 BEFORE this loader runs (load-config is called at ~line 423), so the
  ;; param-* must be overridden here too or the change would not take effect.
  if key = "num-parks" [ set num-parks v report "loaded" ]
  if key = "refugees-per-group" [ set refugees-per-group v report "loaded" ]
  if key = "locals-per-group" [ set locals-per-group v report "loaded" ]
  if key = "motivation-decay-rate" [ set motivation-decay-rate v set param-motivation-decay-base v report "loaded" ]
  if key = "peer-influence-coefficient" [ set peer-influence-coefficient v set param-peer-influence-coef v report "loaded" ]
  if key = "language-gain-rate-per-hour" [ set language-gain-rate-per-hour v set param-language-gain-base-per-hour v report "loaded" ]
  if key = "language-efficiency-multiplier" [ set language-efficiency-multiplier v set param-language-efficiency-multiplier v report "loaded" ]
  if key = "language-friendship-multiplier" [ set language-friendship-multiplier v set param-language-friendship-multiplier v report "loaded" ]
  if key = "tie-formation-probability" [ set tie-formation-probability v set param-tie-formation-prob v report "loaded" ]
  if key = "dropout-threshold" [ set dropout-threshold v set param-dropout-threshold v report "loaded" ]

  report "unknown"
end

;; ============================================================================
;; INLINE CONFIG LOADERS - generated fallback when file I/O is blocked.
;; Generated automatically from config/*.csv. Contents match the CSVs byte-for-byte
;; for the name/value columns. Use load-config-inline instead of load-config when
;; macOS TCC / sandbox blocks file-open even though file-exists? succeeds.
;; ============================================================================

to load-config-inline
  ;; Dispatches on config-domain chooser. Use this instead of load-config
  ;; when file I/O is blocked by macOS TCC / sandbox permissions.
  if config-domain = "calisthenics-istanbul" [ load-istanbul-inline stop ]
  if config-domain = "language-course-berlin" [ load-berlin-inline stop ]
  print (word "[CIM v6.4] No inline loader for config-domain " config-domain "; hardcoded defaults remain in effect.")
end

to load-istanbul-inline
  ;; Inline config loader for 'calisthenics-istanbul' - bypasses file I/O.
  ;; Generated from calisthenics-istanbul.csv (44 rows).
  ;; Calls apply-config-value for each row so slider-backed keys are still skipped.
  let n-loaded 0  let n-skipped 0  let n-unknown 0  let s ""
  set s apply-config-value "num-parks" "5"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "refugees-per-group" "15"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "locals-per-group" "5"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "sessions-per-week" "2"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "session-duration-hours" "1.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "group-target-size" "11"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "annual-budget-per-park" "28000"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "indoor-season-start" "9"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "indoor-season-end" "28"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "motivation-decay-rate" "0.018"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "peer-influence-coefficient" "0.08"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "motivation-boost-per-session" "0.03"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-threshold" "0.20"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "tie-formation-probability" "0.05"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "refugee-initial-tie-probability" "0.20"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "local-initial-tie-probability" "0.15"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "initial-tie-strength" "0.20"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "tie-strength-growth" "0.03"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "language-gain-rate-per-hour" "0.019"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "language-efficiency-multiplier" "0.70"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "language-friendship-multiplier" "1.15"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "prior-exercise-probability" "0.30"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "arrival-cohort-mean-months" "12"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "indoor-facility-probability" "0.80"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "refugee-initial-childcare-prob" "0.40"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "refugee-initial-transport-prob" "0.70"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "local-initial-childcare-prob" "0.80"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "local-initial-transport-prob" "0.90"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "distance-penalty-500m" "0.85"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "female-outdoor-safety-prob" "0.20"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "female-outdoor-safety-multiplier" "0.90"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-motivation" "0.20"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-work-refugee" "0.02"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-work-local" "0.01"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-winter" "0.05"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-distance-refugee" "0.03"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-distance-local" "0.015"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "reentry-prob-base" "0.25"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "reentry-prob-cap" "0.70"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "winter-no-return-threshold-weeks" "6"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "interaction-quality-floor" "0.20"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "weak-peer-beta" "0.03"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "inflow-rate-per-week" "0.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "outflow-rate-per-week" "0.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  print (word "[CIM v6.4] Inline config calisthenics-istanbul applied: " n-loaded " loaded, " n-skipped " slider-deferred, " n-unknown " unknown.")
end

to load-berlin-inline
  ;; Inline config loader for 'language-course-berlin' - bypasses file I/O.
  ;; Generated from language-course-berlin.csv (44 rows).
  ;; Calls apply-config-value for each row so slider-backed keys are still skipped.
  let n-loaded 0  let n-skipped 0  let n-unknown 0  let s ""
  set s apply-config-value "num-parks" "5"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "refugees-per-group" "20"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "locals-per-group" "1"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "sessions-per-week" "4"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "session-duration-hours" "3.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "group-target-size" "21"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "annual-budget-per-park" "45000"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "indoor-season-start" "0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "indoor-season-end" "0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "motivation-decay-rate" "0.015"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "peer-influence-coefficient" "0.08"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "motivation-boost-per-session" "0.02"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-threshold" "0.15"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "tie-formation-probability" "0.07"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "refugee-initial-tie-probability" "0.15"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "local-initial-tie-probability" "0.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "initial-tie-strength" "0.20"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "tie-strength-growth" "0.04"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "language-gain-rate-per-hour" "0.024"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "language-efficiency-multiplier" "0.95"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "language-friendship-multiplier" "1.10"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "prior-exercise-probability" "0.25"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "arrival-cohort-mean-months" "18"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "indoor-facility-probability" "1.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "refugee-initial-childcare-prob" "0.40"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "refugee-initial-transport-prob" "0.85"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "local-initial-childcare-prob" "0.80"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "local-initial-transport-prob" "0.95"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "distance-penalty-500m" "0.90"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "female-outdoor-safety-prob" "0.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "female-outdoor-safety-multiplier" "1.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-motivation" "0.15"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-work-refugee" "0.025"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-work-local" "0.01"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-winter" "0.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-distance-refugee" "0.02"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "dropout-prob-distance-local" "0.01"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "reentry-prob-base" "0.30"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "reentry-prob-cap" "0.75"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "winter-no-return-threshold-weeks" "0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "interaction-quality-floor" "0.5"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "weak-peer-beta" "0.03"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "inflow-rate-per-week" "0.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  set s apply-config-value "outflow-rate-per-week" "0.0"  ifelse s = "loaded" [ set n-loaded n-loaded + 1 ] [ ifelse s = "slider" [ set n-skipped n-skipped + 1 ] [ set n-unknown n-unknown + 1 ] ]
  print (word "[CIM v6.4] Inline config language-course-berlin applied: " n-loaded " loaded, " n-skipped " slider-deferred, " n-unknown " unknown.")
end

@#$#@#$#@
GRAPHICS-WINDOW
460
10
973
524
-1
-1
5.0
1
10
1
1
1
0
0
0
1
0
100
0
100
1
1
1
weeks
30.0

BUTTON
10
10
85
50
NIL
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
95
10
170
50
NIL
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

BUTTON
180
10
260
50
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
10
60
220
93
num-parks
num-parks
3
8
5.0
1
1
NIL
HORIZONTAL

SLIDER
10
100
220
133
refugees-per-group
refugees-per-group
6
25
15.0
1
1
NIL
HORIZONTAL

SLIDER
10
140
220
173
locals-per-group
locals-per-group
2
8
5.0
1
1
NIL
HORIZONTAL

SLIDER
10
185
220
218
motivation-decay-rate
motivation-decay-rate
0.01
0.03
0.018
0.001
1
NIL
HORIZONTAL

SLIDER
10
225
220
258
peer-influence-coefficient
peer-influence-coefficient
0.03
0.2
0.08
0.01
1
NIL
HORIZONTAL

SLIDER
10
265
220
298
language-gain-rate-per-hour
language-gain-rate-per-hour
0.01
0.025
0.019
0.001
1
NIL
HORIZONTAL

SLIDER
10
305
220
338
language-efficiency-multiplier
language-efficiency-multiplier
0.5
0.9
0.7
0.05
1
NIL
HORIZONTAL

SLIDER
10
345
220
378
language-friendship-multiplier
language-friendship-multiplier
1.0
1.3
1.15
0.05
1
NIL
HORIZONTAL

SLIDER
10
385
220
418
tie-formation-probability
tie-formation-probability
0.02
0.15
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
10
425
220
458
dropout-threshold
dropout-threshold
0.1
0.4
0.2
0.05
1
NIL
HORIZONTAL

CHOOSER
230
10
450
55
config-domain
config-domain
"calisthenics-istanbul" "language-course-berlin" "custom"
0

CHOOSER
230
60
450
105
scenario-type
scenario-type
"Baseline" "No Indoor Continuity" "Minimal Support" "Low Park Density" "Weak Peer Influence" "Suboptimal Composition" "Composition2" "Composition3" "Composition4" "SuboptimalOpen" "High SES Heterogeneity" "Women-Only Groups" "NoIndoor Minimal" "Targeting50" "Targeting70" "Targeting90" "BuddyProgram" "CentralityBuddy" "RandomBuddy" "RotatingGroups" "Winter50" "WomenChildcare" "Equifinality" "OpenPopulation"
0

MONITOR
230
115
310
160
Week
current-week
0
1
11

MONITOR
320
115
400
160
Season
current-season
0
1
11

MONITOR
230
170
310
215
Attendance %
attendance-rate
1
1
11

MONITOR
320
170
450
215
Avg Motivation
avg-motivation-level
3
1
11

MONITOR
230
225
310
270
Language (CEFR)
avg-language-proficiency
2
1
11

MONITOR
320
225
450
270
Cross-Group Tie %
cross-group-tie-ratio * 100
1
1
11

MONITOR
230
280
310
325
Total Dropouts
total-dropouts
0
1
11

MONITOR
320
280
450
325
Cost/Retained (€)
cost-per-participant-retained
0
1
11

MONITOR
230
335
310
380
Female Drop %
female-dropout-rate
1
1
11

MONITOR
320
335
450
380
Male Drop %
male-dropout-rate
1
1
11

MONITOR
230
390
450
435
Overall Success?
overall-success?
0
1
11

PLOT
985
10
1265
180
Participation Over Time
Week
Count
0.0
52.0
0.0
50.0
true
true
"" ""
PENS
"Total" 1.0 0 -16777216 true "" "plot total-active-participants"
"Refugees" 1.0 0 -2674135 true "" "plot total-refugees-active"
"Locals" 1.0 0 -13345367 true "" "plot total-locals-active"

PLOT
985
190
1265
360
Motivation Dynamics
Week
Motivation
0.0
52.0
0.0
1.0
true
false
"" ""
PENS
"Avg Motivation" 1.0 0 -10899396 true "" "plot avg-motivation-level"

PLOT
985
370
1265
540
Language Learning (CEFR)
Week
CEFR Level
0.0
52.0
0.0
3.0
true
true
"" ""
PENS
"Avg CEFR" 1.0 0 -955883 true "" "plot avg-language-proficiency"
"A2 Target" 1.0 0 -7500403 true "" "plot 2.0"

PLOT
1275
10
1555
180
Integration (Cross-Group Ties)
Week
Ratio
0.0
52.0
0.0
1.0
true
true
"" ""
PENS
"Tie Ratio" 1.0 0 -8630108 true "" "plot cross-group-tie-ratio"
"Target (0.4)" 1.0 0 -7500403 true "" "plot 0.4"

PLOT
1275
190
1555
360
Cumulative Dropouts
Week
Count
0.0
52.0
0.0
20.0
true
false
"" ""
PENS
"Dropouts" 1.0 0 -16777216 true "" "plot total-dropouts"

PLOT
1275
370
1555
540
Gender Participation
Week
%
0.0
52.0
0.0
100.0
true
true
"" ""
PENS
"Female %" 1.0 0 -2064490 true "" "plot female-participation-rate"
"Male %" 1.0 0 -13345367 true "" "plot male-participation-rate"

BUTTON
270
10
380
50
Export Results
export-final-results
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
390
10
450
50
Export TS
export-weekly-timeseries
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
10
555
220
615
run-start-index
0.0
1
0
Number

INPUTBOX
10
620
220
680
targeting-accuracy
1.0
1
0
Number

INPUTBOX
10
685
220
745
buddy-program-k
0.0
1
0
Number

INPUTBOX
10
750
220
810
rotation-period-weeks
0.0
1
0
Number

BUTTON
230
580
450
620
Load Config CSV
import-config
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
230
455
450
488
use-contagion-influence?
use-contagion-influence?
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

The **Calisthenics Integration Model (CIM) v6.4** is an agent-based model that explores how structured community sport programmes can help migrants integrate into a host society. Specifically, it simulates a one-year calisthenics training programme where migrants and locals train together in public parks.

The model grew out of first-hand observations of calisthenics communities in Istanbul, Turkey, between 2013 and 2022. It is designed as a **decision-support tool**: policymakers can test different programme designs in simulation before committing to expensive real-world pilots.

**v6.4 configuration layer.** As of v6.4, every empirically calibrated, heuristic, or policy-lever parameter is documented in `config/calisthenics-istanbul.csv` (44 rows: 7 empirical, 27 heuristic, 10 policy-lever). At setup the model applies this configuration inline-first (guaranteeing correct per-domain behaviour in every environment, including the macOS GUI where file reads are sandbox-blocked); where file access is available (e.g. headless, or the model relocated out of a protected folder) an edited CSV is read from disk and overrides the inline copy, preserving bit-identical behaviour to v6.3 on seeded runs. Researchers adapting the framework to other community-sport contexts (e.g., language cafés, community gyms) can use the ready-made **`custom`** domain: edit `config/custom.csv` and select `custom` in the config-domain chooser (or set `config-domain="custom"` headless), or click the **Load Config CSV** button to import any CSV in one step. For a custom domain the CSV is the FULL source of truth — every parameter, including the slider-backed ones, is taken from the file (even under BehaviorSpace), whereas the two built-in presets keep their slider params from the experiment so the shipped results stay reproducible. See `config/schema.md` for the file format, column semantics, calibration tiers, and the adaptation guide.

**Scope caveat for v6.4.** The model is scoped to the migrant population under Turkey's 2014 Temporary Protection Regulation (predominantly Syrian nationals) who are candidates for outdoor community-sport participation. The 50% female initialisation is a modelling idealisation that isolates post-arrival barrier effects; it does not capture gender-based venue self-selection at programme entry, which empirical research [Koskela 1999; Phadke, Khan & Ranade 2011; Kaya 2017] shows is structurally non-trivial for outdoor male-predominant venues. Findings concerning gender equity should be read as conditional on attendance, not as population-wide estimates. See Section 5.3 Limitations for the corresponding methodological statement.


## HOW IT WORKS

### Agents

Each simulation run creates **100 participants** across **5 parks**, plus **5 trainers** (1 per park):

- **Migrants** (75 total, 15 per park): Each has a gender, an arrival cohort (recent, established, or settled), prior exercise experience (30%), initial motivation drawn from U[0.3, 0.8], socioeconomic status (SES) drawn from U[0, 1], and a starting language level (CEFR 0-6 depending on cohort).
- **Locals** (25 total, 5 per park): Same structure as migrants but with higher baseline motivation U[0.5, 0.9], 40% prior exercise rate, and no language variable (native speakers).
- **Trainers** (5 total, 1 per park): Facilitate sessions using bilingual instruction.

### Weekly Cycle (8 steps per tick)

Every week, the model runs through these steps in order:

1. **Update season** - check if it is outdoor or indoor season
2. **Attendance decisions** - each agent decides whether to attend based on motivation and barriers
3. **Training sessions** - attendees get a motivation boost (+0.03 per session, 2 sessions/week)
4. **Peer influence** - motivation updated via tie-weighted influence from friends
5. **Friendship network** - ties form between co-attendees, strengthen with contact, weaken with absence
6. **Language learning** - migrants gain CEFR points each session (base quality 0.20 from trainer/peer interaction; enhanced when locals attend)
7. **Motivation decay** - all active agents lose motivation each week (rate: 0.018/week)
8. **Dropout checks** - agents face stochastic dropout from four triggers (low motivation, work conflict, winter, distance)

### Seasons

- **Outdoor** (Weeks 1-8): Autumn start, all parks active outdoors
- **Indoor** (Weeks 9-28): Winter, parks with indoor partners continue; others lose access
- **Outdoor** (Weeks 29-52): Spring/summer return, all parks active again

The winter period (weeks 9-28) is when the biggest dropout spikes occur, especially for scenarios without indoor facility access.

## HOW TO USE IT

### Basic Operation

1. Set parameters using the sliders on the left
2. Choose a scenario from the **scenario-type** dropdown
3. Click **setup** to create agents and parks
4. Click **go** to run for 52 weeks (one full programme year)

### All 23 Scenarios (see Section 3.9 Scenarios in the thesis; 3-family Holm-corrected)

**Confirmatory family - Original hypothesis tests (4 scenarios + Baseline):**

```
Scenario                 Tests       What changes
-----------              -----       ------------
Baseline                 Control     All support active, default parameters
Weak Peer Influence      H1          Peer coefficient reduced to 0.03
Suboptimal Composition   H2          Only 1 local per park (instead of 5)
No Indoor Continuity     H3          All indoor winter partnerships removed
Minimal Support          H4          No stipends, childcare reset to baseline
```

**Exploratory family - Original v6.3 scenarios (11 scenarios):**

```
Scenario                 Tests        What changes
-----------              -----        ------------
Low Park Density         Spatial      Migrant distances to park x1.5
High SES Heterogeneity   Inequality   Bimodal SES distribution
Women-Only Groups        EQ1          50% of parks are women-only
NoIndoor Minimal         H3+H4        No indoor + no support (combined stress test)
Targeting50              Targeting    SES targeting accuracy = 50%
Targeting70              Targeting    SES targeting accuracy = 70%
Targeting90              Targeting    SES targeting accuracy = 90%
BuddyProgram             Social       Migrant paired with local buddy for 8 weeks (week-0 distance)
RotatingGroups           Composition  Groups reshuffled every 4 weeks
Winter50                 H3 partial   50% of parks have indoor partners
WomenChildcare           EQ1+H4       Women-only groups + universal childcare
```

**Robustness family - Phase 3 + verification round (7 scenarios, May 2026):**

```
Scenario                 Tests        What changes
-----------              -----        ------------
Composition2             H2 dose-2    2 locals per park (dose-response sweep)
Composition3             H2 dose-3    3 locals per park (dose-response sweep)
Composition4             H2 dose-4    4 locals per park (dose-response sweep)
OpenPopulation           Open-cohort  Continuous churn: p_in=0.30, p_out=0.20 per week
SuboptimalOpen           Open-cohort  Suboptimal + OpenPopulation (ranking-preservation test)
CentralityBuddy          Buddy timing Buddy paired at week 8 by highest local degree
RandomBuddy              Buddy timing Buddy paired at week 8 by random selection (timing-vs-criterion control)
```

**Auxiliary (mechanism robustness check):**

```
Scenario                 Tests        What changes
-----------              -----        ------------
Equifinality             Mechanism    Contact-presence contagion (vs tie-weighted peer influence)
```

### Key Parameters (see Section 3.7 Parameter Table in the thesis)

```
Parameter                      Default  Range           Source
---------                      -------  -----           ------
motivation-decay-rate          0.018    [0.010, 0.040]  Exercise adherence
peer-influence-coefficient     0.080    [0.010, 0.200]  Social contagion
tie-formation-probability      0.050    [0.020, 0.150]  Contact Hypothesis
dropout-threshold              0.200    [0.100, 0.400]  SDT engagement
language-gain-rate-per-hour    0.019    [0.010, 0.025]  CEFR acquisition rates
```

### Attendance Barriers

Each barrier multiplies a refugee's base attendance probability (0.75). Locals face lighter barriers (base 0.80, reduced work/distance penalties):

```
Barrier                     Multiplier       Who it affects
-------                     ----------       --------------
Work conflict               x0.70            Agents with work obligations
Childcare (female)          x0.60            Females without childcare
Childcare (male)            x0.80            Males without childcare
Distance 5-7.5 patches      x0.85            Moderately far from park
Distance 7.5-10 patches     x0.70            Far from park
Distance >10 patches        x0.50            Very far from park
No indoor (winter)          x0.65            Parks without indoor partner
Indoor access (winter)      x0.90            Parks with indoor partner
Stipend bonus               x1.20            Receiving financial support
Transport bonus             x1.10            With transport access
Female outdoor safety       x0.90 (20%)      Females during outdoor season
BuddyProgram boost          x1.15 (8 wks)   Migrants with local buddy
```

## THINGS TO NOTICE

- **Winter onset (week 9)**: Watch for a dropout spike when outdoor training stops and parks without indoor partners lose participants
- **Cross-group ties**: Links between agents show migrant-local friendships forming over time
- **Motivation colours**: Brighter colours mean higher motivation; watch them fade during winter for the No Indoor Continuity scenario
- **Top-retention scenarios**: CentralityBuddy and RandomBuddy both reach 50.6% retention (buddy-timing verification round); BuddyProgram (48.5%), High SES Heterogeneity (48.4%), and WomenChildcare (45.8%) also beat Baseline (45.1%)
- **Phase 3 finding (TIMING vs CRITERION)**: CentralityBuddy and RandomBuddy are statistically indistinguishable on every outcome (Cohen's d = -0.01, Holm-p = 1.00). The +2.1 pp uplift over BuddyProgram is from pairing TIME (week 8 vs week 0), not pairing CRITERION (degree vs random)

## PHASE 3 ROBUSTNESS FINDINGS (May 2026)

- **Dose-Response**: Retention scales linearly +4.31 pp per additional local (linear preferred over quadratic, ΔAIC = -1.96). Cross-group tie ratio plateaus at dose 3 (saturation, ΔAIC = +179)
- **Open Population**: Six-of-six graph-structure metrics show no significant difference under churn after Holm correction (modularity, clustering, giant-component fraction, breed assortativity, within/cross tie strengths; all p_Holm > 0.2). Quantitative outcomes shift: retention -4.5 pp, CEFR -0.19 levels. Baseline > Suboptimal ranking preserved in BOTH cohort regimes (closed gap 17.2 pp, open gap 14.6 pp)
- **Link Prediction**: Triadic-closure predictors (Adamic-Adar, common neighbours, Jaccard) yield AUC 0.74-0.77 (p < 1e-28 vs chance). Preferential attachment is at chance (~0.50). Network has triadic structure, not Barabasi-Albert preferential attachment
- **Buddy Timing**: Late-pairing (week 8) outperforms early-pairing (week 0) by +2.1 pp retention regardless of selection rule. Targeting criterion (degree vs random) makes no detectable difference at week 8

## THINGS TO TRY

1. Compare **Baseline** vs **No Indoor Continuity** - how many more agents drop out without winter access?
2. Run **Suboptimal Composition** (1 local per park) - watch cross-group ties collapse
3. Compare **BuddyProgram** vs **CentralityBuddy** vs **RandomBuddy** - is the +2.1 pp uplift driven by timing (week 8 vs week 0) or criterion (degree vs random)?
4. Run the dose-response sweep: **Suboptimal Composition** -> **Composition2** -> **Composition3** -> **Composition4** -> **Baseline** - is the response linear or saturating?
5. Compare **Baseline** vs **OpenPopulation** vs **SuboptimalOpen** - does the Baseline-vs-Suboptimal ranking preserve under cohort churn?
6. Compare **Women-Only Groups** vs **WomenChildcare** - does adding childcare close the gender dropout gap?
7. Run the three **Targeting** scenarios (50/70/90) - at what accuracy does targeting recover Baseline performance?

## EXTENDING THE MODEL

The thesis identifies several directions for future work:

- Trainer quality heterogeneity (currently all trainers are identical)
- New migrant arrivals during the programme year (population turnover)
- External shocks (policy changes, economic crises)
- Multi-city replication with different park densities and climates
- Intent-to-treat analysis (currently only survivor outcomes are reported)

## RELATED MODELS

- **Schelling Segregation Model** - shows how micro-level preferences produce macro-level segregation; CIM explores the opposite direction (integration)
- **Team Assembly Model** - network formation through collaboration
- **Ethnocentrism Model** - cross-group interaction dynamics in the NetLogo Models Library

## CREDITS AND REFERENCES

**Author**: Abdullah Tadmuri
**Programme**: Master's in Computational Social Science, UC3M
**Advisor**: Prof. Anxo Sanchez
**Date**: April 2026 (initial v6.4 release); May 2026 (Phase 3 robustness extensions + verification round 2: dose-response sweep, link-prediction validation, open-population multi-metric SNA, late-pairing buddy controls)

**Theoretical Framework**:
- Self-Determination Theory (Deci & Ryan, 2000) - motivation dynamics
- Contact Hypothesis (Allport, 1954) - cross-group tie formation
- Zone of Proximal Development (Vygotsky, 1978) - language acquisition
- Threshold Model (Granovetter, 1978) - dropout cascades

**Key Parameter Sources**:
- Gjestvang et al. (2020): 12-month exercise adherence rates; source for motivation-decay-rate (γ = 0.018/week)
- Centola (2010, *Science* 329:1194-1197): peer-reinforced behaviour spread; source for peer-influence-coefficient (β = 0.08)
- Kossinets & Watts (2006, *Science* 311:88-90): evolving social network tie-formation rates; source for tie-formation-probability (p_tie = 0.05/session)
- Dishman (1988, *Exercise Adherence*, Human Kinetics): 50% six-month exercise dropout benchmark; source for prior-exercise-probability
- Meadows (1999): leverage-point hierarchy used in H2 > H3 > H4 > H1 ranking interpretation
- Council of Europe (2001): CEFR language proficiency framework
- Putnam (2000): Social capital and community participation
- Kosyakova et al. (2021): Migrant arrival cohort distributions
- Koskela (1999, *Geografiska Annaler B* 81:111-124): women's fear and spatial exclusion; scope caveat for gendered venue use
- Phadke, Khan & Ranade (2011, *Why Loiter?*): gendered public-space use; scope caveat
- Kaya (2017, *Southeastern Europe* 41:333-358): Syrian refugees in Istanbul, cultural affinity; scope caveat
- Adamic & Adar (2003, *Social Networks* 25:211-230): Adamic-Adar predictor used in the link-prediction validation
- Kim et al. (2015, *The Lancet* 386:145-153): network-targeted intervention via friendship-paradox sampling; reference for the CentralityBuddy/RandomBuddy comparison (the high-degree-targeting effect did not replicate in this model; the active mechanism is pairing timing, not pairing criterion)

**Full documentation**: See the accompanying thesis (86 pages) and the R analysis pipeline (33 scripts: R/00 setup through R/29 SuboptimalOpen ranking-preservation test, plus constants.R and extra_agent_fate.R) for complete methodology, validation, and results. Total simulation budget: 7,500 policy runs (16 original scenarios + 7 Phase 3 robustness extensions, all at 300-500 runs) + 810 sensitivity runs + 100 auxiliary runs = 8,410 runs in v6.4 main pipeline.

@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Baseline_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Baseline&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="NoIndoor_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;No Indoor Continuity&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="MinimalSupport_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Minimal Support&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="LowParkDensity_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Low Park Density&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="WeakPeer_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Weak Peer Influence&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="SuboptimalComp_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Suboptimal Composition&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="HighSES_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;High SES Heterogeneity&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="WomenOnly_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Women-Only Groups&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity_3level" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Baseline&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <steppedValueSet variable="motivation-decay-rate" first="0.010" step="0.010" last="0.030"/>
    <steppedValueSet variable="peer-influence-coefficient" first="0.03" step="0.085" last="0.200"/>
    <steppedValueSet variable="tie-formation-probability" first="0.02" step="0.065" last="0.150"/>
    <steppedValueSet variable="dropout-threshold" first="0.10" step="0.150" last="0.400"/>
  </experiment>
  <experiment name="NoIndoorMinimal_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;NoIndoor Minimal&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="Targeting50_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Targeting50&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="targeting-accuracy"><value value="0.5"/></enumeratedValueSet>
  </experiment>
  <experiment name="Targeting70_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Targeting70&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="targeting-accuracy"><value value="0.7"/></enumeratedValueSet>
  </experiment>
  <experiment name="Targeting90_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Targeting90&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="targeting-accuracy"><value value="0.9"/></enumeratedValueSet>
  </experiment>
  <experiment name="BuddyProgram_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;BuddyProgram&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="buddy-program-k"><value value="8"/></enumeratedValueSet>
  </experiment>
  <experiment name="RotatingGroups_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;RotatingGroups&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="rotation-period-weeks"><value value="4"/></enumeratedValueSet>
  </experiment>
  <experiment name="Winter50_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Winter50&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="WomenChildcare_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;WomenChildcare&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="LowPark_topup200runs" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Low Park Density&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="run-start-index"><value value="300"/></enumeratedValueSet>
  </experiment>
  <experiment name="WomenOnly_topup200runs" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Women-Only Groups&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="run-start-index"><value value="300"/></enumeratedValueSet>
  </experiment>
  <experiment name="WeakPeer_topup200runs" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Weak Peer Influence&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="run-start-index"><value value="300"/></enumeratedValueSet>
  </experiment>
  <experiment name="Equifinality_ContactContagion_100runs" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>integration-index</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Equifinality&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="use-contagion-influence?"><value value="true"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="OpenPopulation_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;OpenPopulation&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="GammaBracket_Low_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Baseline&quot;"/><value value="&quot;BuddyProgram&quot;"/><value value="&quot;Suboptimal Composition&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.011"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="GammaBracket_High_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Baseline&quot;"/><value value="&quot;BuddyProgram&quot;"/><value value="&quot;Suboptimal Composition&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.025"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="Berlin_AllScenarios_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>integration-index</metric>
    <enumeratedValueSet variable="config-domain"><value value="&quot;language-course-berlin&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="scenario-type">
      <value value="&quot;Baseline&quot;"/>
      <value value="&quot;Minimal Support&quot;"/>
      <value value="&quot;Low Park Density&quot;"/>
      <value value="&quot;Weak Peer Influence&quot;"/>
      <value value="&quot;Suboptimal Composition&quot;"/>
      <value value="&quot;High SES Heterogeneity&quot;"/>
      <value value="&quot;Women-Only Groups&quot;"/>
      <value value="&quot;Targeting50&quot;"/>
      <value value="&quot;Targeting70&quot;"/>
      <value value="&quot;Targeting90&quot;"/>
      <value value="&quot;BuddyProgram&quot;"/>
      <value value="&quot;RotatingGroups&quot;"/>
      <value value="&quot;WomenChildcare&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="20"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="1"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.015"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.024"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.95"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.10"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.07"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.15"/></enumeratedValueSet>
  </experiment>
  <experiment name="Composition3_pilot5" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Composition3&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="Composition2_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Composition2&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="Composition3_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Composition3&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="Composition4_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Composition4&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="CentralityBuddy_pilot5" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;CentralityBuddy&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="buddy-program-k"><value value="16"/></enumeratedValueSet>
  </experiment>
  <experiment name="CentralityBuddy_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>recent-cohort-language-gain</metric>
    <metric>established-cohort-language-gain</metric>
    <metric>settled-cohort-language-gain</metric>
    <metric>prior-exercise-retention-rate</metric>
    <metric>no-exercise-retention-rate</metric>
    <metric>integration-index</metric>
    <metric>female-participation-rate</metric>
    <metric>male-participation-rate</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;CentralityBuddy&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="buddy-program-k"><value value="16"/></enumeratedValueSet>
  </experiment>
  <experiment name="RandomBuddy_pilot5" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>integration-index</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;RandomBuddy&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="buddy-program-k"><value value="16"/></enumeratedValueSet>
  </experiment>
  <experiment name="RandomBuddy_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>integration-index</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;RandomBuddy&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
    <enumeratedValueSet variable="buddy-program-k"><value value="16"/></enumeratedValueSet>
  </experiment>
  <experiment name="SuboptimalOpen_pilot5" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>integration-index</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;SuboptimalOpen&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="SuboptimalOpen_300runs" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-final-results
export-weekly-timeseries
export-agent-crosssection
export-agent-panel
export-edge-list</postRun>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <metric>total-dropouts</metric>
    <metric>cost-per-participant-retained</metric>
    <metric>female-dropout-rate</metric>
    <metric>male-dropout-rate</metric>
    <metric>integration-index</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;SuboptimalOpen&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
  <experiment name="ParseSmokeTest_1run" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>retention-rate-percent</metric>
    <metric>avg-motivation-level</metric>
    <metric>avg-language-proficiency</metric>
    <metric>cross-group-tie-ratio</metric>
    <enumeratedValueSet variable="scenario-type"><value value="&quot;Baseline&quot;"/></enumeratedValueSet>
    <enumeratedValueSet variable="num-parks"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="refugees-per-group"><value value="15"/></enumeratedValueSet>
    <enumeratedValueSet variable="locals-per-group"><value value="5"/></enumeratedValueSet>
    <enumeratedValueSet variable="motivation-decay-rate"><value value="0.018"/></enumeratedValueSet>
    <enumeratedValueSet variable="peer-influence-coefficient"><value value="0.08"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-gain-rate-per-hour"><value value="0.019"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-efficiency-multiplier"><value value="0.7"/></enumeratedValueSet>
    <enumeratedValueSet variable="language-friendship-multiplier"><value value="1.15"/></enumeratedValueSet>
    <enumeratedValueSet variable="tie-formation-probability"><value value="0.05"/></enumeratedValueSet>
    <enumeratedValueSet variable="dropout-threshold"><value value="0.2"/></enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
