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
- ./data/rdf/AnnaTest.ttl: result of the conversion of the above file


## Conversion steps

-  Download, install SWI-Prolog and Cliopatria (see https://cliopatria.swi-prolog.org/home)
-  In the repository folder, start a new ClioPatria instance, this results in a run.pl file
-  ./run.pl: starts ClioPatria server (on localhost:3020 by default). Also opens a prolog shell where the following commands are to be run:
- ?- cpack_install(xmlrdf).    
- ?- [run_bioned]. 
- ?- [rewrite_bioned]. 
- ?- rdf_load('mapschema.ttl').
- ?- load_xml_test_dir.
- ?- rewrite.
- ?- rdf_load('./data/rdf/bgn_schema.ttl').

You can view the results in the cliopatria web UI (Places->Graphs->bioned). 

Steps 4-8 load the XML testfile, converts it to crude RDF using the map_schema file, executes all the rewrite-rules and loads the schema file.

To save the result as RDF turtle, use 
1. ?- rdf_save_turtle(FILENAME, [graph(bioned)]).

### iterative conversion (seems to have resolved some issues with buggy loading and deleting of graphs)
- ?- run_bioned_it.


For full conversion: 
1. Get XML file from biografisch portaal (not provided here)
2. In folder ./data/xml/full_preprocessed
3. Run bionet_xml_preprocess_batch script
4. In run_bioned.pl: adjust conversion source to the above folder
