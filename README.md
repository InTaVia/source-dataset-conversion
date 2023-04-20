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


## EDM Object relations

### EDM conversion
These python notebooks are used to convert Europeana EDM creator and contributor data to Intavia Data Model (IDM). As input the script takes a list of intavia-wikidata sameas triples, the europeana metadata in various subfolders

#### EDM cleaning
Using SWI-prolog, the following lines clean the unwanted triples:
?- forall((rdf(A,bioc:had_participant_in_role,R),not(sub_atom(R,_,_,_,'www.wikidata.org'))),(rdf_retractall(R,_,_),rdf_retractall(_,_,R))).
?- forall((rdf(A,bioc:had_participant_in_role,R),atomic_list_concat([_,WD],'producing_artist',R),atomic_list_concat(['http://',WD],WDU)),rdf_assert(A,ns1:'P11_had_participant',WDU)).
