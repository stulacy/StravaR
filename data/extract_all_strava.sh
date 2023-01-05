#!/bin/sh

DB=strava.db

# Filename *should* be constant as it's tied to my account
unzip -uo -d raw extracts/export_32344668.zip

# Fit files
mkdir tmp
find raw/activities -name '*.fit.gz' | while read inf; do
    bn=$(basename $inf .fit.gz)
    if [ ! -f "clean/from_fit/$bn.csv" ]; then
        echo "Found a new FIT activity with ID $bn."
        gunzip -f $inf && java -jar /opt/fit_sdk/java/FitCSVTool.jar --defn none --data record -b raw/activities/$bn.fit tmp/$bn.csv && mv tmp/${bn}_data.csv clean/from_fit/$bn.csv
    fi
done
rm -r tmp

# GPX files
# Shouldn't ever find zipped gpx files, but worth accounting for possibility
find raw/activities -name '*.gpx.gz' | while read inf; do
    bn=$(basename $inf .gpx.gz)
    if [ ! -f "clean/from_gpx/$bn.csv" ]; then
        echo "Found a new archived GPX activity with ID $bn."
        gunzip $inf
    fi
done

# Now have unzipped any previously unseen GPX files can 
find raw/activities -name '*.gpx' | while read inf; do
    bn=$(basename $inf .gpx)
    if [ ! -f "clean/from_gpx/$bn.csv" ]; then
        echo "Found a new GPX activity with ID $bn."
        gpsbabel -t -i gpx -f $inf -o unicsv -F clean/from_gpx/$bn.csv
    fi
done

# Populate DB
rm $DB
sqlite3 $DB < create_schema.sql > /dev/null
Rscript --vanilla populate_db.R $DB
