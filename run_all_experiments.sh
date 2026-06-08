#!/bin/bash
# CIM v6.4 — Full experiment run script
# Runs all BehaviorSpace experiments (16 scenarios, 5,400 runs)
# Usage: cd CIM_Model && bash run_all_experiments.sh
set -e

# Configure these for your system (defaults are for macOS ARM + NetLogo 7.0.3)
export JAVA_HOME="${JAVA_HOME:-/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home}"
JAVA="${JAVA:-$JAVA_HOME/bin/java}"
JAR="${NETLOGO_JAR:-$HOME/Downloads/NetLogo 7.0.3/app/netlogo-7.0.3.jar}"
MODEL="${CIM_MODEL:-$(cd "$(dirname "$0")" && pwd)/CIM_v6_4.nlogo}"

echo "========================================"
echo "Starting: Baseline_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "Baseline_300runs" \
  --table /dev/null 2>&1
echo "Done: Baseline_300runs at $(date)"

echo "========================================"
echo "Starting: NoIndoor_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "NoIndoor_300runs" \
  --table /dev/null 2>&1
echo "Done: NoIndoor_300runs at $(date)"

echo "========================================"
echo "Starting: MinimalSupport_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "MinimalSupport_300runs" \
  --table /dev/null 2>&1
echo "Done: MinimalSupport_300runs at $(date)"

echo "========================================"
echo "Starting: LowParkDensity_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "LowParkDensity_300runs" \
  --table /dev/null 2>&1
echo "Done: LowParkDensity_300runs at $(date)"

echo "========================================"
echo "Starting: WeakPeer_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "WeakPeer_300runs" \
  --table /dev/null 2>&1
echo "Done: WeakPeer_300runs at $(date)"

echo "========================================"
echo "Starting: SuboptimalComp_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "SuboptimalComp_300runs" \
  --table /dev/null 2>&1
echo "Done: SuboptimalComp_300runs at $(date)"

echo "========================================"
echo "Starting: HighSES_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "HighSES_300runs" \
  --table /dev/null 2>&1
echo "Done: HighSES_300runs at $(date)"

echo "========================================"
echo "Starting: WomenOnly_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "WomenOnly_300runs" \
  --table /dev/null 2>&1
echo "Done: WomenOnly_300runs at $(date)"

echo "========================================"
echo "Starting: Sensitivity_3level"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "Sensitivity_3level" \
  --table /dev/null 2>&1
echo "Done: Sensitivity_3level at $(date)"

echo "========================================"
echo "Starting: NoIndoorMinimal_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "NoIndoorMinimal_300runs" \
  --table /dev/null 2>&1
echo "Done: NoIndoorMinimal_300runs at $(date)"

echo "========================================"
echo "Starting: Targeting50_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "Targeting50_300runs" \
  --table /dev/null 2>&1
echo "Done: Targeting50_300runs at $(date)"

echo "========================================"
echo "Starting: Targeting70_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "Targeting70_300runs" \
  --table /dev/null 2>&1
echo "Done: Targeting70_300runs at $(date)"

echo "========================================"
echo "Starting: Targeting90_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "Targeting90_300runs" \
  --table /dev/null 2>&1
echo "Done: Targeting90_300runs at $(date)"

echo "========================================"
echo "Starting: BuddyProgram_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "BuddyProgram_300runs" \
  --table /dev/null 2>&1
echo "Done: BuddyProgram_300runs at $(date)"

echo "========================================"
echo "Starting: RotatingGroups_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "RotatingGroups_300runs" \
  --table /dev/null 2>&1
echo "Done: RotatingGroups_300runs at $(date)"

echo "========================================"
echo "Starting: Winter50_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "Winter50_300runs" \
  --table /dev/null 2>&1
echo "Done: Winter50_300runs at $(date)"

echo "========================================"
echo "Starting: WomenChildcare_300runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "WomenChildcare_300runs" \
  --table /dev/null 2>&1
echo "Done: WomenChildcare_300runs at $(date)"

echo "========================================"
echo "Starting: LowPark_topup200runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "LowPark_topup200runs" \
  --table /dev/null 2>&1
echo "Done: LowPark_topup200runs at $(date)"

echo "========================================"
echo "Starting: WomenOnly_topup200runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "WomenOnly_topup200runs" \
  --table /dev/null 2>&1
echo "Done: WomenOnly_topup200runs at $(date)"

echo "========================================"
echo "Starting: WeakPeer_topup200runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "WeakPeer_topup200runs" \
  --table /dev/null 2>&1
echo "Done: WeakPeer_topup200runs at $(date)"

echo "========================================"
echo "Starting: Equifinality_ContactContagion_100runs"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "Equifinality_ContactContagion_100runs" \
  --table /dev/null 2>&1
echo "Done: Equifinality_ContactContagion_100runs at $(date)"


# ============================================================================
# Phase 3 robustness extensions (May 2026)
# ============================================================================

echo "========================================"
echo "Starting: Composition2_300runs (Phase 3 Item 13 dose 2)"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "Composition2_300runs" \
  --table /dev/null 2>&1
echo "Done: Composition2_300runs at $(date)"

echo "========================================"
echo "Starting: Composition3_300runs (Phase 3 Item 13 dose 3)"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "Composition3_300runs" \
  --table /dev/null 2>&1
echo "Done: Composition3_300runs at $(date)"

echo "========================================"
echo "Starting: Composition4_300runs (Phase 3 Item 13 dose 4)"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "Composition4_300runs" \
  --table /dev/null 2>&1
echo "Done: Composition4_300runs at $(date)"

echo "========================================"
echo "Starting: CentralityBuddy_300runs (Phase 3 Item 10 targeted matching)"
echo "Start: $(date)"
"$JAVA" -XX:MaxRAMPercentage=20 -Dfile.encoding=UTF-8 \
  "-Dnetlogo.extensions.dir=$HOME/Downloads/NetLogo 7.0.3/extensions" \
  "-Dnetlogo.models.dir=$HOME/Downloads/NetLogo 7.0.3/models" \
  --add-exports=java.base/java.lang=ALL-UNNAMED \
  --add-exports=java.desktop/sun.awt=ALL-UNNAMED \
  --add-exports=java.desktop/sun.java2d=ALL-UNNAMED \
  -classpath "$JAR" org.nlogo.headless.Main \
  --model "$MODEL" \
  --experiment "CentralityBuddy_300runs" \
  --table /dev/null 2>&1
echo "Done: CentralityBuddy_300runs at $(date)"

echo "ALL EXPERIMENTS COMPLETE"