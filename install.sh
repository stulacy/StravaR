#!/bin/sh

# Shouldn't need to modify any of these
DATA_DIR=data
SETUP_DIR=setup
DB=$DATA_DIR/strava.db
MAP_CACHE=$DATA_DIR/maps.rds
PATH_TO_FITCSV_JAR=/opt/fit_sdk/java/FitCSVTool.jar
EXTRACT_DIR=$DATA_DIR/extracts
UNZIPPED_DIR=$DATA_DIR/raw
CLEAN_DIR=$DATA_DIR/clean
GPX_DIR=$CLEAN_DIR/gpx
FIT_DIR=$CLEAN_DIR/fit
CONFIG=$SETUP_DIR/config.json
TMP_DIR=data/tmp

mkdir -p $CLEAN_DIR
mkdir -p $UNZIPPED_DIR
mkdir -p $GPX_DIR
mkdir -p $FIT_DIR

# Filename *should* be constant as account ID
unzip -uo -d $UNZIPPED_DIR $EXTRACT_DIR/export_*.zip

# Copy the main activities file over
cp $UNZIPPED_DIR/activities.csv $CLEAN_DIR/

# Convert fit files to CSV using FiTCSVTool
mkdir $TMP_DIR
find $UNZIPPED_DIR/activities -name '*.fit.gz' | while read inf; do
    bn=$(basename $inf .fit.gz)
    if [ ! -f "$FIT_DIR/$bn.csv" ]; then
        echo "Found a new FIT activity with ID $bn."
        gunzip -f $inf && java -jar $PATH_TO_FITCSV_JAR --defn none --data record -b $UNZIPPED_DIR/activities/$bn.fit $TMP_DIR/$bn.csv && mv $TMP_DIR/${bn}_data.csv $FIT_DIR/$bn.csv
    fi
done
rm -r $TMP_DIR

# GPX files
# Shouldn't ever find zipped gpx files, but worth accounting for possibility
find $UNZIPPED_DIR/activities -name '*.gpx.gz' | while read inf; do
    bn=$(basename $inf .gpx.gz)
    if [ ! -f "$GPX_DIR/$bn.csv" ]; then
        echo "Found a new archived GPX activity with ID $bn."
        gunzip $inf
    fi
done

# convert GPX to CSV using gpsbabel
find $UNZIPPED_DIR/activities -name '*.gpx' | while read inf; do
    bn=$(basename $inf .gpx)
    if [ ! -f "$GPX_DIR/$bn.csv" ]; then
        echo "Found a new GPX activity with ID $bn."
        gpsbabel -t -i gpx -f $inf -o unicsv -F $GPX_DIR/$bn.csv
    fi
done

# Populate DB and down
if [ -f "$DB" ] ; then
    rm $DB
fi
sqlite3 $DB < $SETUP_DIR/schema.sql > /dev/null
Rscript --vanilla $SETUP_DIR/populate_db.R $DB $CONFIG $CLEAN_DIR/activities.csv $GPX_DIR $FIT_DIR

# Download maps
Rscript --vanilla $SETUP_DIR/download_maps.R $DB $CONFIG $MAP_CACHE