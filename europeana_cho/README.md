# Europeana CHO data

This repository contains documentation for the workflow for selecting a subset of Europeana CHO RDF data (received as a data dump `export-europeana-intavia.zip`) that will contain CHO's created by persons who are included in the InTaVia knowledge graph (persons are identified/matched using Wikidata identifiers) and converting it into IDM RDF.

Files:
- README.md - documentation
- load_data.sh - script for loading a subset of Europeana CHO RDF data into [Apache Jena Fuseki](https://jena.apache.org/documentation/fuseki2/) triplestore
- europeana-cho-construct.sparql - SPARQL CONSTRUCT query for converting Europeana CHO data into IDM RDF

## Requirements

Copy the following files to this directory:
- export-europeana-intavia.zip (data dump, zip file size: 17 GB)
- merge_at_nl_fi_si.txt (list of Wikidata URIs of InTaVia persons)

## Unzip Europeana export

```
unzip export-europeana-intavia.zip
cd export-intavia
mkdir unzipped
cd unzipped
for file in ../*.zip; do unzip -o $file; done
```

## Create a text file containg lines of the ttl files that have "wikidata" in them: "filename   line"

```
OLD_IFS="$IFS"; IFS=$'\n'; for file in *.ttl; do for line in `grep wikidata $file`; do echo $file"   "$line >> ../../wikidata-lines.txt; done; done; IFS="$OLD_IFS"
cd ../..
```

## Create a text file listing the ttl files that have data on InTaVia persons

```
grep -Ff merge_at_nl_fi_si.txt wikidata-lines.txt | grep -Eo '^[^ ]+' | uniq > ttl-files-with-intavia-persons.txt
echo "" >> ttl-files-with-intavia-persons.txt
```

## Concatenate ttl files that have data on InTaVia persons into one ttl file

```
i=0; while read FILE; do i=`expr $i + 1`; cat export-intavia/unzipped/$FILE >> europeana-export-for-intavia-persons.ttl; if [ `expr $i % 1000` == 0 ]; then echo Files processed: $i; fi; done <ttl-files-with-intavia-persons.txt
```

## Fix ttl syntax errors in the source data

```
sed -E 's/@ spa/@es/; s/( <[^>]*)<([^>]*)>([^>]*> )/\1%3C\2%3E\3/g' europeana-export-for-intavia-persons.ttl > europeana-export-for-intavia-persons-fixed.ttl
```

## Load ttl file that has data on InTaVia persons into Fuseki triplestore

```
mkdir fuseki-data
chmod a+w fuseki-data

docker run -it --rm -v "$(pwd)"/europeana-export-for-intavia-persons-fixed.ttl:/vol-data/europeana-export-for-intavia-persons-fixed.ttl -v "$(pwd)"/fuseki-data:/fuseki-base/databases -v "$(pwd)"/load_data.sh:/load_data.sh --name intavia-europeana-fuseki-data-loader --entrypoint sh secoresearch/fuseki /load_data.sh
```

## Run Fuseki with SPARQL endpoint

```
docker run -it --rm -p 3030:3030 -v "$(pwd)"/fuseki-data:/fuseki-base/databases --name intavia-europeana-fuseki secoresearch/fuseki
```

SPARQL endpoint available at http://localhost:3030/ds/sparql

## Run SPARQL CONSTRUCT query to add Europeana CHO data into InTaVia knowledge graph

```
sed 's/[[:space:]]#.*$//' europeana-cho-construct.sparql | curl -H "Content-Type: application/sparql-query" -d @- -o europeana-cho-idm.ttl http://localhost:3030/ds/sparql
```

The resulting file `europeana-cho-idm.ttl` can be ingested into InTaVia triplestore.
