# --------------------------------------- #
# --------------------------------------- #
##### Script for all Graphab analyses #####
# --------------------------------------- #
# --------------------------------------- #

### This script is for reproducibility, but treatments were actually done by Gilles Vuidel
# on Linux using a bash script (shown below). As I do not have the time to wrap it up in working
# R-CLI functions, I simply paste the script here.
# If you want to make it work though, you'll probable need to adapt it to your system. You'll
# also need to have the Graphab software (.jar) in your desired working repository, you can
# download it from the Graphab website. Be sure to use the same version (here, Graphab 2.8)!
# You'll also need the input landcover raster file (lc_dijon_v8_simple.tif).
# NOTE: As this landcover file is too heavy, I cannot put it on my Github account. If you want
# to get it, you should then either contact me directly or find it in the project's Zenodo
# archive (*** WHEN READY).

### WARNING: DO NOT TRY TO RUN THIS CODE IN R, it won't work (as explained above)!





# ------------------------------ #
# START OF THE BASH-Linux script #
# ------------------------------ #

#!/bin/bash

# directory containing datasources
dir="couches_sources"
cmd_java="java -Xmx50g -jar graphab-2.8.1.jar -proc 12 "
cmd_java_metric="java -Xmx30g -jar graphab-2.8.1.jar -proc 16 "

# parameter 1 must be pmrf or pmarea
capa=$1

project=$capa"_gpb"

# create graphab project
#$cmd_java --create $project $dir/lc_dijon_v8_simple.tif habitat=31,32,33,34,35,36 nodata=0 minarea=0.001 maxsize=200

if [ "$capa" == "pmrf" ]; then
#./extract_hab_update.sh
# set capacities
$cmd_java --project $project/$project.xml --capa file=avg_capa.csv id=id_patch capa=capa
fi
# Finally, we decided to only use AREA as capacity proxy.


# --------------------------------------------------- #
##### 1. Connectivity models for PM (Parus major) #####
# --------------------------------------------------- #

# ** 1.1.2. Computing relevant link-set and graph ----
# Updated (survey-based) ranking and costs for landcover classes:
$cmd_java --project $project/$project.xml --linkset distance=cost name="$capa"_survcost 31,32,33,34,35,36=1 51=10 25=12.5 1,2=15 71,81=20 62=30 11=50 61=60 12=70 --graph name="$capa"_survcost_g750 threshold={750}
# The tested resistance values are stored in `ppl_survey_results_simplified.xlsx` (including
# the previously tested values).***

# This threshold value was chosen because it is half the maximum value answered in our survey
# as well as almost twice the second highest value answered, thus representing an acceptable
# compromise for a maximal value, especially considering that 3 respondents gave maximal
# distances <= 50.


# ** 1.1.4. Computing local or delta metrics ----
$cmd_java_metric --project $project/$project.xml --usegraph "$capa"_survcost_g750 --lmetric F maxcost=2500 "d={15,60}" p=0.5 beta=0,1,2 --lmetric F maxcost=6000 d={140} p=0.5 beta=0,1,2 --lmetric F maxcost=12000 d={270} p=0.5 beta=0,1,2
# Finally, we decided to drop BC (because it does not seem relevant for our research hypothesis).

# Among our 6 respondents, there was a clear divide in estimated gap-crossing distances with
# one group that always gave very conservative values (median gap-crossing values ranging from
# 5 to 25m) while the other half gave much higher values (ranging from 100 to 500m). It was
# thus hard to choose a single central parameter to summarise PM gap-crossing abilities.
# Consequently, I chose to test for various distance estimates:
#   - 15 = the mean value for 'median gap-crossing distance' estimated by the "conservative
# half" of the respondents.
#   - 60 = roughly the median value estimated by all respondents.
#   - 140 = roughly the mean value estimated by all respondents.
#   - 270 = roughly the mean value for 'median gap-crossing distance' estimated by the "liberal
# half" of the respondents.

# For the beta parameter, three values were tested representing three hypotheses:
#   - beta=0 - Tits disregard patches capacity when deciding whether to cross a gap.
#   - beta=1 - Implying that the weight of patches' capacity acts linearly with the weight of
# distances (i.e. local patch quality affects proportionately the ability or willingness or
# tits to cross a gap).
#   - beta=2 - Implying that the weight of patches' capacity is squared compared to the weight of
# distances (i.e. birds are more likely to cross a given distance if patches are of good quality).



# ----------------------------------------------------------- #
##### 2. Connectivity models for CC (Cyanistes caeruleus) #####
# ----------------------------------------------------------- #

# As we did not build a RF model for CC, we only need to compute metrics based on the "area"
# approximation of patch capacity. As such, we can use the same project as before.

# ---------------------------------------------------------------------------- #
if [ "$capa" == "pmarea" ]; then
### * 2.1. Project with area-based capacity --------------------------------------
# ** 2.1.1. Loading and computing relevant link-sets ----
# Updated (survey-based) ranking and costs for landcover classes:
$cmd_java --project $project/$project.xml --linkset distance=cost name=cc_survcost 31,32,33,34,35,36=1 51=15 25=17.5 1,2=20 71,81=25 62=40 11=50 61=65 12=70 --graph name=cc_survcost_g500 threshold={500}

# ** 2.1.3. Computing local metrics ----
$cmd_java_metric --project $project/$project.xml --usegraph cc_survcost_g500 --lmetric F maxcost=2000 "d={10,30}" p=0.5 beta=0,1,2 --lmetric F maxcost=6000 d={130} p=0.5 beta=0,1,2 --lmetric F maxcost=13000 d={250} p=0.5 beta=0,1,2
# Same rationale as for PM (but with slightly lower values for CC).
# Finally, we decided to drop BC (because it does not seem relevant for our research hypothesis).
fi

$cmd_java_metric --project $project/$project.xml --pointset tits_boxloc_dijon.gpkg id=id_nestbox name=tits
# ---------------------------- #
# END OF THE BASH-Linux script #
# ---------------------------- #


