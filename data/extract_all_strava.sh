#!/bin/sh

DB=strava.db
MAP_DIR=maps
PATH_TO_FITCSV_JAR=/opt/fit_sdk/java/FitCSVTool.jar
EXTRACT_DIR=extracts
UNZIPPED_DIR=raw
CLEAN_DIR=clean
CONFIG=../config.json

# Filename *should* be constant as account ID
unzip -uo -d $UNZIPPED_DIR $EXTRACT_DIR/export_*.zip

# Copy the main activities file over
cp $UNZIPPED_DIR/activities.csv $CLEAN_DIR/

# Convert fit files to CSV using FiTCSVTool
mkdir tmp
find $UNZIPPED_DIR/activities -name '*.fit.gz' | while read inf; do
    bn=$(basename $inf .fit.gz)
    if [ ! -f "$CLEAN_DIR/from_fit/$bn.csv" ]; then
        echo "Found a new FIT activity with ID $bn."
        gunzip -f $inf && java -jar $PATH_TO_FITCSV_JAR --defn none --data record -b $UNZIPPED_DIR/activities/$bn.fit tmp/$bn.csv && mv tmp/${bn}_data.csv $CLEAN_DIR/from_fit/$bn.csv
    fi
done
rm -r tmp

# GPX files
# Shouldn't ever find zipped gpx files, but worth accounting for possibility
find $UNZIPPED_DIR/activities -name '*.gpx.gz' | while read inf; do
    bn=$(basename $inf .gpx.gz)
    if [ ! -f "$CLEAN_DIR/from_gpx/$bn.csv" ]; then
        echo "Found a new archived GPX activity with ID $bn."
        gunzip $inf
    fi
done

# convert GPX to CSV using gpsbabel
find $UNZIPPED_DIR/activities -name '*.gpx' | while read inf; do
    bn=$(basename $inf .gpx)
    if [ ! -f "$CLEAN_DIR/from_gpx/$bn.csv" ]; then
        echo "Found a new GPX activity with ID $bn."
        gpsbabel -t -i gpx -f $inf -o unicsv -F $CLEAN_DIR/from_gpx/$bn.csv
    fi
done

# Populate DB
rm $DB
sqlite3 $DB < create_schema.sql > /dev/null
Rscript --vanilla populate_db.R $DB $CLEAN_DIR $CONFIG

# Download maps
mkdir -p $MAP_DIR
Rscript --vanilla download_maps.R $CONFIG $MAP_DIR

