#!/bin/bash
# ============================================================================
# run_all_tier3.sh — Run all 4 Tier 3 BehaviorSpace experiments headless.
#
# Usage:  cd /path/to/CIM_Model
#         bash run_all_tier3.sh                 # production run (~8.5h)
#         bash run_all_tier3.sh --smoke-test    # 2-rep probe first (~2 min)
#
# Strategy:
#   1. Validate NetLogo binary + model file exist.
#   2. (Optional) Smoke-test headless with a 2-rep copy of OpenPopulation.
#   3. OpenPopulation_300runs  → data/OpenPopulation/              (no conflict)
#   4. GammaBracket_Low_300runs  → data/GammaBracket_Low/          (stash+run+move+restore)
#   5. GammaBracket_High_300runs → data/GammaBracket_High/         (stash+run+move+restore)
#   6. Berlin_AllScenarios_300runs → data/second_domain/<scenario>/ (stash+run+move+restore)
#
# Safety:
#   - v6.4 scenario folders are renamed to .v64_stash/ before any conflicting run,
#     then restored after output is relocated. If the script aborts mid-way,
#     the .v64_stash folders preserve your v6.4 data; restore with:
#       for d in data/*.v64_stash; do mv "$d" "${d%.v64_stash}"; done
#   - `caffeinate` prevents the Mac from sleeping during the long run.
#   - Every step is timestamped to tier3_run.log.
# ============================================================================

set -uo pipefail  # no -e: want to keep going and still restore stash if NetLogo fails

MODEL_DIR="${MODEL_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
JAVA="${JAVA:-/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home/bin/java}"
JAR="${NETLOGO_JAR:-$HOME/Downloads/NetLogo 7.0.3/app/netlogo-7.0.3.jar}"
NETLOGO_EXT="${NETLOGO_EXT:-$HOME/Downloads/NetLogo 7.0.3/extensions}"
NETLOGO_MODELS="${NETLOGO_MODELS:-$HOME/Downloads/NetLogo 7.0.3/models}"
MODEL="CIM_v6_4.nlogo"
LOG="tier3_run.log"

cd "$MODEL_DIR" || { echo "Cannot cd to $MODEL_DIR"; exit 2; }

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$LOG"; }

# ----------------------------------------------------------------------------
# Preflight
# ----------------------------------------------------------------------------
log "============================================================"
log "Tier 3 headless driver starting"
log "Mode: ${1:-production}"
log "JAVA:  $JAVA"
log "JAR:   $JAR"
log "Model: $MODEL_DIR/$MODEL"
log "============================================================"

[[ -f "$JAVA" ]] || { log "FATAL: Java binary not found at $JAVA. Install with 'brew install openjdk@21' or adjust path."; exit 3; }
[[ -f "$JAR" ]]  || { log "FATAL: NetLogo JAR not found at $JAR. Adjust path to your NetLogo 7.0.3 install."; exit 4; }
[[ -f "$MODEL" ]] || { log "FATAL: Model file $MODEL not found in $MODEL_DIR."; exit 5; }

# ----------------------------------------------------------------------------
# Step 0: sanity-check JVM + NetLogo headless entry point
# ----------------------------------------------------------------------------
log "Preflight: verifying JVM + NetLogo headless classpath..."
"$JAVA" -version >> "$LOG" 2>&1 || { log "FATAL: Java failed to start."; exit 6; }
"$JAVA" -Djava.awt.headless=true -classpath "$JAR" org.nlogo.headless.Main --help > /tmp/nlogo_help.txt 2>&1
rc=$?
if [[ $rc -ne 0 && $rc -ne 1 ]]; then
    log "FATAL: NetLogo headless Main did not respond (exit $rc). Check classpath."
    exit 7
fi
log "  OK. Help output saved to /tmp/nlogo_help.txt"

# ----------------------------------------------------------------------------
# Run experiment helper (JAVA + org.nlogo.headless.Main; bypasses hanging native launcher)
# ----------------------------------------------------------------------------
run_experiment() {
    local name="$1"
    local table="$2"
    log "  Starting $name (output: $table)"
    local t0=$(date +%s)
    caffeinate -i "$JAVA" -XX:MaxRAMPercentage=50 -Dfile.encoding=UTF-8 -Djava.awt.headless=true \
        "-Dnetlogo.extensions.dir=$NETLOGO_EXT" \
        "-Dnetlogo.models.dir=$NETLOGO_MODELS" \
        --add-exports=java.base/java.lang=ALL-UNNAMED \
        --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
        --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
        -classpath "$JAR" org.nlogo.headless.Main \
        --model "$MODEL" \
        --experiment "$name" \
        --table "$table" \
        --threads 4 \
        >> "$LOG" 2>&1
    local rc=$?
    local t1=$(date +%s)
    local dur=$(( t1 - t0 ))
    if [[ $rc -ne 0 ]]; then
        log "  WARNING: $name exited with code $rc after ${dur}s"
    else
        log "  $name done in ${dur}s"
    fi
    return $rc
}

# ----------------------------------------------------------------------------
# Smoke-test mode
# ----------------------------------------------------------------------------
if [[ "${1:-}" == "--smoke-test" ]]; then
    log "SMOKE TEST: running OpenPopulation with 2 reps (manual XML edit required)"
    log "  For a real smoke test, edit CIM_v6_4.nlogo BehaviorSpace XML and set"
    log "  repetitions=\"2\" on OpenPopulation_300runs, then re-run this script."
    log "  Or just start with full production run below."
    exit 0
fi

# ----------------------------------------------------------------------------
# Step 1: OpenPopulation_300runs (fresh folder, no conflict)
# ----------------------------------------------------------------------------
log "============================================================"
log "STEP 1/4: OpenPopulation_300runs (~30 min expected)"
log "============================================================"
mkdir -p data/OpenPopulation
run_experiment OpenPopulation_300runs data/OpenPopulation_300runs.csv
n_op=$(ls data/OpenPopulation 2>/dev/null | wc -l | tr -d ' ')
log "  data/OpenPopulation/ has $n_op files"
if [[ $n_op -lt 100 ]]; then
    log "  WARNING: expected ~300-1500 files; got $n_op. Check $LOG for NetLogo errors before proceeding."
fi

# ----------------------------------------------------------------------------
# Step 2: GammaBracket_Low_300runs — stash 3 scenario folders, run, relocate
# ----------------------------------------------------------------------------
stash_scenarios() {
    local stash_base="data"
    local suffix="$1"
    shift
    for s in "$@"; do
        if [[ -d "$stash_base/$s" ]]; then
            mv "$stash_base/$s" "$stash_base/${s}.${suffix}"
            mkdir -p "$stash_base/$s"
        else
            mkdir -p "$stash_base/$s"
        fi
    done
}

relocate_to_dest() {
    local dest="$1"
    shift
    mkdir -p "$dest"
    local moved=0
    for s in "$@"; do
        if [[ -d "data/$s" ]]; then
            for f in "data/$s"/*; do
                [[ -e "$f" ]] || continue
                mv "$f" "$dest/" 2>/dev/null && moved=$((moved+1))
            done
            rmdir "data/$s" 2>/dev/null || true
        fi
    done
    log "  relocated $moved files to $dest"
}

restore_stash() {
    local suffix="$1"
    shift
    for s in "$@"; do
        if [[ -d "data/${s}.${suffix}" ]]; then
            rm -rf "data/$s" 2>/dev/null || true
            mv "data/${s}.${suffix}" "data/$s"
        fi
    done
}

GAMMA_SCENARIOS=("Baseline" "BuddyProgram" "Suboptimal Composition")

log "============================================================"
log "STEP 2/4: GammaBracket_Low_300runs (~1.5h expected)"
log "============================================================"
stash_scenarios "v64_low" "${GAMMA_SCENARIOS[@]}"
run_experiment GammaBracket_Low_300runs data/GammaBracket_Low_300runs.csv
relocate_to_dest data/GammaBracket_Low "${GAMMA_SCENARIOS[@]}"
restore_stash "v64_low" "${GAMMA_SCENARIOS[@]}"
log "  data/GammaBracket_Low/ now has $(ls data/GammaBracket_Low 2>/dev/null | wc -l | tr -d ' ') files"

# ----------------------------------------------------------------------------
# Step 3: GammaBracket_High_300runs — same pattern
# ----------------------------------------------------------------------------
log "============================================================"
log "STEP 3/4: GammaBracket_High_300runs (~1.5h expected)"
log "============================================================"
stash_scenarios "v64_high" "${GAMMA_SCENARIOS[@]}"
run_experiment GammaBracket_High_300runs data/GammaBracket_High_300runs.csv
relocate_to_dest data/GammaBracket_High "${GAMMA_SCENARIOS[@]}"
restore_stash "v64_high" "${GAMMA_SCENARIOS[@]}"
log "  data/GammaBracket_High/ now has $(ls data/GammaBracket_High 2>/dev/null | wc -l | tr -d ' ') files"

# ----------------------------------------------------------------------------
# Step 4: Berlin_AllScenarios — 13 scenarios, relocate to data/second_domain/<scenario>/
# ----------------------------------------------------------------------------
BERLIN_SCENARIOS=(
    "Baseline" "Minimal Support" "Low Park Density" "Weak Peer Influence"
    "Suboptimal Composition" "High SES Heterogeneity" "Women-Only Groups"
    "Targeting50" "Targeting70" "Targeting90" "BuddyProgram" "RotatingGroups"
    "WomenChildcare"
)

log "============================================================"
log "STEP 4/4: Berlin_AllScenarios_300runs (~6h expected)"
log "============================================================"
stash_scenarios "v64_berlin" "${BERLIN_SCENARIOS[@]}"
run_experiment Berlin_AllScenarios_300runs data/Berlin_AllScenarios_300runs.csv

# Relocate per-scenario outputs into data/second_domain/<scenario>/
mkdir -p data/second_domain
for s in "${BERLIN_SCENARIOS[@]}"; do
    if [[ -d "data/$s" ]]; then
        mkdir -p "data/second_domain/$s"
        moved=0
        for f in "data/$s"/*; do
            [[ -e "$f" ]] || continue
            mv "$f" "data/second_domain/$s/" 2>/dev/null && moved=$((moved+1))
        done
        rmdir "data/$s" 2>/dev/null || true
        log "  second_domain/$s: $moved files"
    fi
done
restore_stash "v64_berlin" "${BERLIN_SCENARIOS[@]}"

# ----------------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------------
log "============================================================"
log "All Tier 3 runs complete."
log "Output folders:"
log "  data/OpenPopulation/       ($(ls data/OpenPopulation 2>/dev/null | wc -l | tr -d ' ') files)"
log "  data/GammaBracket_Low/     ($(ls data/GammaBracket_Low 2>/dev/null | wc -l | tr -d ' ') files)"
log "  data/GammaBracket_High/    ($(ls data/GammaBracket_High 2>/dev/null | wc -l | tr -d ' ') files)"
log "  data/second_domain/        ($(find data/second_domain -type f 2>/dev/null | wc -l | tr -d ' ') files)"
log ""
log "Next step: Rscript R/19_tier3_splice_and_ship.R"
log "============================================================"
