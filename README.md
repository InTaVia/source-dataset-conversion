# source-dataset-conversion
scripts and converted datasets accordng to IDM RDF (see IDM RDF repository)

## Converted Turtle file -> Named graph mappings

BS_dataset/bs2intavia.ttl - http://ldf.fi/nbf/data

APIS_dataset/apisdata_18-04-2022_edited.ttl - http://apis.acdh.oeaw.ac.at/data

## Enrichment flows

### External person ID's

Enrich person entities with external person ID's from Wikidata SPARQL endpoint using source data's existing owl:sameAs links:

* Wikidata ID -> GND & VIAF (BS, Bionet)
* GND ID -> Wikidata & VIAF (APIS)
* SBI ID -> Wikidata, GND & VIAF (SBI)

See the [Jupyter Notebook](enrichment/PersonIDEnrichment.ipynb).