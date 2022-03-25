# intavia_biographynet

This repo contains  scripts to convert XML files from Biografisch portaal into Intavia data model RDF data. The scripts are based on earlier work in the biographynet project (http://biographynet.nl)

Conversion is done using the XMLRDF library (https://semanticweb.cs.vu.nl/xmlrdf/) of the ClioPatria (https://cliopatria.swi-prolog.org/home) semantic toolkit. 

## Content
A description of the most important files: 

### bio
- bionet_xml_preprocess_batch bactch script to convert biografisch portaal XML files into grouped XML files with necessary identifier  information 
- run_bioned.pl: loads a) necessary prolog predicates for loading and saving the relevant files and b) several auxiliary predicates used in the conversion
- rewrite_bioned.pl: the rewrite script, consisting of declarative rewrite rules as defined by the XMLRDF library
- map_schema.ttl: RDF-turtle file defining several predicates used in the initial conversion step (to be loaded before initial conversion)
- ./data/rdf/bgn_schema.ttl: RDF-turtle file with biographynet-specific triples defining classes and properties (to be loaded after initial conversion)
- ./data/xml/test/AnnaTest.xml: synthetic biografisch portaal XML document for testing purposes


## Conversion steps

1. Download, install SWI-Prolog and Cliopatria (see https://cliopatria.swi-prolog.org/home)
2. In the repository folder, start a new ClioPatria instance, this results in a run.pl file
3. ./run.pl: starts ClioPatria server (on localhost:3020 by default)
4. in the Prolog shell: ?- [run_bioned]. 
5.  ?- [rewrite_bioned]. 
6.  ?- rdf_load('map_schema.ttl').
7.  ?- load_xml_test_dir.
8.  ?- rewrite.
9.  ?- rdf_load('./data/rdf/bgn_schema.ttl').

Steps 4-8 load the XML testfile, converts it to crude RDF using the map_schema file, executes all the rewrite-rules and loads the schema file.

For full conversion: 
1. Get XML file from biografisch portaal (not provided here)
2. In folder ./data/xml/full_preprocessed
3. Run bionet_xml_preprocess_batch script
4. In run_bioned.pl: adjust conversion source to the above folder
