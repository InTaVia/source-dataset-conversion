from cProfile import label
from operator import index, le
from pydoc import ModuleScanner
from typing import Any
from unittest import result
import requests
import pandas as pd
import json
import pprint
from rdflib import Graph, Literal, RDF, Namespace, URIRef
from rdflib.namespace import RDFS, FOAF
from SPARQLWrapper import JSON, SPARQLWrapper
from lxml import etree 
import re
import urllib.parse
import datetime
import numpy as np
import sys

sparql = SPARQLWrapper("https://query.wikidata.org/sparql")


sparql.setQuery(
    """
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
    PREFIX wdt: <http://www.wikidata.org/prop/direct/>
    PREFIX wd: <http://www.wikidata.org/entity/>
    PREFIX p: <http://www.wikidata.org/prop/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX wikibase: <http://wikiba.se/ontology#>
    PREFIX bd: <http://www.bigdata.com/rdf#>
    PREFIX idm: <https://www.intavia.org/idm/>
    PREFIX idmrole: <https://www.intavia.org/idm/role/>
    PREFIX bioc: <http://www.ldf.fi/schema/bioc/>
    PREFIX psv: <http://www.wikidata.org/prop/statement/value/>
    CONSTRUCT {
        ?cho a crm:E24_Physical_Human_Made_Thing,
               idm:CHO_Proxy;
             bioc:bearer_of ?choProductionEventRole;
             crm:P2_has_type ?class;
             crm:P1_is_identified_by ?cho_1476Title;
             rdfs:label ?chordfsLabel.
        ?class rdfs:label ?classLabel;
               a crm:E55_Type.
        ?cho_1476Title rdfs:label ?choTitle .
        ?cho_1476Title a crm:E35_Title .
        ?choProductionEventRole a bioc:Thing_Role .
        ?choProductionEvent bioc:occured_in_the_presence_of_in_role ?choProductionEventRole;
                                 bioc:had_participant_in_role ?artistProductionEventRole .
        ?artist bioc:bearer_of ?artistProductionEventRole .
        ?artistProductionEventRole a idmrole:producing_artist ,
                                     bioc:Event_Role .
        ?artistProductionEventRole rdfs:label "producing artist"@en .
        ?choProductionEvent crm:P4_has_time-span ?choProductionTimespan .
        ?choProductionEvent a crm:E12_Production .
        ?choTitle a crm:E41_E33_Linguistic_Appellation .
        ?cho crm:P45_consists_of ?cho_material .
        ?cho_material a crm:E57_Material .
        ?cho_material rdfs:label ?materialLabel .
        ?collection crm:P46_is_composed_of ?cho .
        ?collection a crm:E78_Curated_Holding .
        ?collection rdfs:label ?collectionLabel.
        ?cho_measurement_event crm:P39_measured ?cho.
        ?cho_measurement_event crm:P40_observed_dimension ?cho_measurement.
        ?cho_measurement a crm:E54_Dimension.
        ?cho_measuerement crm:P91_has_unit ?qunit.
        ?qunit a crm:E58_Measurement_Unit.
        ?cho_measurement crm:P90_has_value ?heightvalue.
    }
    WHERE {
        ?artistUri wdt:P6194 ?oeblid .
          ?cho wdt:P170 ?artistUri .
          #?cho wdt:P31 wd:Q3305213.
          OPTIONAL{?cho wdt:P1476 ?choTitle}
          OPTIONAL{?cho wdt:P31 ?class}
          OPTIONAL{?cho wdt:P186 ?material}
          OPTIONAL{?cho rdfs:label ?chordfsLabel
                        FILTER(lang(?chordfsLabel) = "en")}
          OPTIONAL{?cho wdt:P276 ?location}
          OPTIONAL{?location rdfs:label ?locationLabel
                             FILTER(lang(?chordfsLabel) = "en")}
          OPTIONAL{?cho wdt:P2048 ?heightvalue}
          OPTIONAL{?cho p:P2048 ?heightstatement.
                   ?heightstatement psn:P2048 ?quantityvaluestmt.
                   ?quantityvaluestmt wikibase:quantityUnit ?qunit}
          OPTIONAL{?cho wdt:P2049 ?widthvalue}
          OPTIONAL{?cho p:2049 ?widthstatement}
          OPTIONAL{?widthstatement wikibase:quantityUnit ?qwunit}
          OPTIONAL{?cho wdt:P195 ?collection}
          }
            BIND(REPLACE(STR(?cho), "http://www.wikidata.org/entity/", "https://www.intavia.org/chomeasurement/") as ?cho_MeasurementEventStr)
            BIND(IRI(?cho_MeasurementEventStr) as ?cho_measurement_event)
            BIND(REPLACE(STR(?cho), "http://www.wikidata.org/entity/", "https://www.intavia.org/chotitle/") as ?cho_TitleStr)
            BIND(IRI(?cho_TitleStr) as ?cho_1476Title)
            BIND(REPLACE(STR(?material), "http://www.wikidata.org/entity/", "https://www.intavia.org/chomaterial/") as ?cho_materialStr)
            BIND(IRI(?cho_materialStr) as ?cho_material)
            BIND(REPLACE(STR(?class), "http://www.wikidata.org/entity/", "https://www.intavia.org/cho_instanceofclass/") as ?cho_instanceofclassStr)
            BIND(IRI(?cho_instanceofclassStr) as ?cho_instanceofclass)
            BIND(REPLACE(STR(?cho), "http://www.wikidata.org/entity/", "https://www.intavia.org/productionthingrole/") as ?choProductionEventRoleStr)      
            BIND(IRI(?choProductionEventRoleStr) as ?choProductionEventRole)
            BIND(REPLACE(STR(?artistUri), "http://www.wikidata.org/entity/", "https://www.intavia.org/role/responsibleArtist/") as ?artistProductionEventRoleStr)      
            BIND(IRI(?artistProductionEventRoleStr) as ?artistProductionEventRole)
            BIND(REPLACE(STR(?cho), "http://www.wikidata.org/entity/", "https://www.intavia.org/production_event/") as ?choProductionEventStr)      
            BIND(IRI(?choProductionEventStr) as ?choProductionEvent)
            OPTIONAL{?cho wdt:P571 ?pInception .
                BIND(REPLACE(STR(?cho), "http://www.wikidata.org/entity/", "https://www.intavia.org/timespan_production/") as ?choProductionTimespanStr)      
                BIND(IRI(?choProductionTimespanStr) as ?choProductionTimespan)
                }
            }
            LIMIT 10
    """
    )
 

def querya():
    try:
        ret = sparql.queryAndConvert()
        ex_cho_wiki_apis = ret.serialize(destination=f'testserialization_wiki_apis_cho{datetime.datetime.now().strftime("%d-%m-%Y")}.ttl', format='turtle')
    except Exception as e:
        print(e)

if __name__ == "__main__":
    querya()




    # OPTIONAL{?material rdfs:label ?materialLabel .
    #         FILTER(lang(?materialLabel) = "en")
    #         }
    #         OPTIONAL {?class rdfs:label ?classLabel .
    #         FILTER(lang(?classLabel) = "en")
    #         }
    #         OPTIONAL {?collection rdfs:label ?collectionLabel .
    #         FILTER(lang(?collectionLabel) = "en")
    #         }
