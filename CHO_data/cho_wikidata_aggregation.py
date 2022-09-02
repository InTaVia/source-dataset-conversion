from cProfile import label
from operator import index, le
from pydoc import ModuleScanner
from typing import Any
from unittest import result
from numpy import r_
from SPARQLTransformer import sparqlTransformer
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

"""Import libraries."""

crm=Namespace('http://www.cidoc-crm.org/cidoc-crm/')
"""Defines namespace for CIDOC CRM."""
ex=Namespace('http://www.intavia.eu/')
"""Defines namespace for own ontology."""
idmcore=Namespace('http://www.intavia.eu/idm-core/')
"""Defines namespace for own ontology."""
idmrole=Namespace('http://www.intavia.eu/idm-role/')
"""Defines namespace for role ontology."""
idmapis=Namespace('http://www.intavia.eu/apis/')
"""Namespace for InTaVia named graph"""
idmwd=Namespace('http://www.intavia.eu/wikidata/')
"""Namespace for Wikidata named graph"""
idmbibl=Namespace('http://www.intavia.eu/idm-bibl/')
"""Namespace for bibliographic named graph"""
idmrelations=Namespace('http://www.intavia.eu/idm-relations')
"""Defines namespace for relation ontology."""
intavia_shared=Namespace('http://www.intavia.eu/shared-entities')
"""Defines namespace for relation ontology."""
ore=Namespace('http://www.openarchives.org/ore/terms/')
"""Defines namespace for schema.org vocabulary."""
edm=Namespace('http://www.europeana.eu/schemas/edm/')
"""Defines namespace for Europeana data model vocabulary."""
ore=Namespace('http://www.openarchives.org/ore/terms/')
"""Defines namespace for Europeana data model vocabulary."""
owl=Namespace('http://www.w3.org/2002/07/owl#')
"""Defines namespace for Europeana data model vocabulary."""
rdf=Namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#')
"""Defines namespace for Europeana data model vocabulary."""
xml=Namespace('http://www.w3.org/XML/1998/namespace')
"""Defines namespace for Europeana data model vocabulary."""
xsd=Namespace('http://www.w3.org/2001/XMLSchema#')
"""Defines namespace for Europeana data model vocabulary."""
bioc=Namespace('http://www.ldf.fi/schema/bioc/')
"""Defines namespace for Europeana data model vocabulary."""
rdfs=Namespace('http://www.w3.org/2000/01/rdf-schema#')
"""Defines namespace for Europeana data model vocabulary."""
apis=Namespace('https://www.apis.acdh.oeaw.ac.at/')
"""Defines namespace for APIS database."""
owl=Namespace('http://www.w3.org/2002/07/owl#')
"""Defines OWL namespace."""
bf=Namespace('http://id.loc.gov/ontologies/bibframe/')
"""Defines bibframe namespace."""
wd=Namespace('http://www.wikidata.org/entity/')
"""Defines wikidata namespace."""

#Create undirected graph, assigned to h

def merge_graph_apis_wikidata(h, wiki_mergecheck, oeblxml_mergecheck, merge_df):
    for index, row in merge_df.iterrows():
        #print('wiki_person_graph: '+ str(index), str(row) +" + " + str(type(row)))
        wikidata_person_URI = URIRef(f"{str(row['item'])}")
        person_apis_id = str(row['apis_id'])[:-2]
        person_oeblxml_id = row['oeblid']
        Provided_Person = URIRef(f"{intavia_shared}/providedperson/{person_apis_id}")
        apis_artist_person = URIRef(f"{idmapis}personproxy/{person_apis_id}")
        wiki_artist_person_proxy = URIRef(f"{idmapis}personproxy/{person_apis_id}")
        wiki_oebl_artist_person = URIRef(f"{idmwd}personproxy/{person_oeblxml_id}")
        ##
        h.add((wiki_oebl_artist_person, idmcore.person_proxy_for, Provided_Person))
        h.add((wiki_oebl_artist_person, RDF.type, idmcore.Person_Proxy))
        h.add((wiki_oebl_artist_person, RDF.type, crm.E21_Person))
        h.add((wiki_oebl_artist_person, owl.sameAs, wikidata_person_URI))
        if person_apis_id != "n":
            h.add((wiki_oebl_artist_person, owl.sameAs, URIRef(apis + person_apis_id)))
        else:
            pass
        #h.add((URIRef(wiki_apis_artist_person), owl.sameAs, wiki_oebl_artist_person))
        wiki_mergecheck.append(wikidata_person_URI)
        oeblxml_mergecheck.append(wikidata_person_URI)
        #print("merge executed for: " + str(wiki_apis_artist_person) + " : " + str(wikidata_person_URI))
    return h, wiki_mergecheck, oeblxml_mergecheck


def namespaces(h):
    h.bind('crm', crm)
    h.bind('intaviashared', intavia_shared)
    h.bind('ore', ore)
    h.bind('edm', edm)
    h.bind('owl', owl)
    h.bind('rdf', rdf)
    h.bind('xml', xml)
    h.bind('xsd', xsd)
    h.bind('bioc', bioc)
    h.bind('rdfs', rdfs)
    h.bind('apis', apis)
    h.bind('idmcore', idmcore)
    h.bind('idmrole', idmrole)
    h.bind('idmrelations', idmrelations)
    h.bind('owl', owl)
    h.bind('wikidata', wd)
    return h

def serdata(h):
    h.serialize("current_intavia_wikidata_aw.ttl",fomat ='turtle')
    return h

def aw_query_a(x):
    squery = """
    PREFIX idmcore:<http://www.intavia.eu/idm-core/>
    PREFIX crm:<http://www.cidoc-crm.org/cidoc-crm/>
    PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
    CONSTRUCT
        {{
            wd:{x} a crm:E21_Person
            wd:{x} crm:P1_is_identified_by ?creatorLabel.
            ?creatorLabel a crm:E41_Appellation.
            ?aw a idmcore:CHO_Proxy.
            ?aw crm:P1_is_identified_by ?awLabel.
        }}
    WHERE
        {{
            wd:{x} a wd:Q5
            wd:{x} wdt:P106 wd:Q3391743.
            wd:{x} wdt:P800 ?aw.
            #SERVICE wikibase:label 
            #filter(lang(?awLabel) = "de")
            #?aw wdt:P170 ?creator.
        }}
    LIMIT 1
    """    





# query = """
# PREFIX idmcore:<http://www.intavia.eu/idm-core/>
# PREFIX crm:<http://www.cidoc-crm.org/cidoc-crm/>
# PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#> 
# CONSTRUCT
# {{
# wd:{x} crm:P1_is_identified_by ?creatorLabel.
# ?creatorLabel a crm:E41_Appellation.
# ?aw a idmcore:CHO_Proxy.
# ?aw crm:P1_is_identified_by ?awLabel.
# }}
# WHERE
# {{
#     wd:{x} 
#     ?aw a wd:
# ?aw wdt:P170 wd:{x}.
# filter(lang(?bLabel) = "de")
# }}
# LIMIT 10"""

def wikiq(x):
    user_agent = "intavia_dataaggregation_python_bot/%s.%s" % (sys.version_info[0], sys.version_info[1])
    wd_endpoint_url = "https://query.wikidata.org/sparql"
    # TODO adjust user agent; see https://w.wiki/CX6
    sparql = SPARQLWrapper(wd_endpoint_url, agent=user_agent)
    reslist = []
    print(x)
    print("begin artwork query")
    aw_query_a(x)
    try:
        results = sparql.queryAndConvert()
        print(x)
        print(results.serialize())
        return(results)
    except:
        results = "Error Incomplete Read?"
        print(results)
    finally:
        return(results)
    #sparql.setQuery(query)
    #sparql.setQuery(f"""PREFIX idmcore: <http://www.intavia.eu/idm-core/> PREFIX crm:<http://www.cidoc-crm.org/cidoc-crm/> PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#> CONSTRUCT {{wd:{x} rdfs:label ?artistLabel. ?aw a idmcore:CHO_Proxy. ?aw rdfs:label ?awLabel.}} WHERE {{?aw wdt:P170 wd:{x}. wd:{x} rdfs:label ?artistLabel. ?aw rdfs:label ?awLabel. filter(lang(?bLabel) = "de".}} }}""")
    # sparql.setReturnFormat(JSON)
    # resp = sparql.query().convert()
    # print(resp)
    # return(resp)


def wikisparql(wiki_id_list, update_wiki_id_list):
    for w in wiki_id_list:
        if w != None:
            x = w.split('/')
            x = x[(len(x))-1]
            x = wikiq(str(x))
            update_wiki_id_list.append(x)
        elif w == None:
            w = str("no_merge_yet")
            update_wiki_id_list.append(w)
        else:
            print("update list: " + update_wiki_id_list)
    return update_wiki_id_list


def main():
    h = Graph()
    wiki_mergecheck=[]
    oeblxml_mergecheck=[]
    update_wiki_id_list=[]
    merge2_wiki_apis = pd.read_csv('apis_wiki_id_merge.csv')
    headers = {"accept": "application/json"}
    merge2_wiki_apis_df = merge2_wiki_apis.applymap(str)
    merge2_wiki_apis_df.head()
    h, wiki_mergecheck, oeblxml_mergecheck = merge_graph_apis_wikidata(h, wiki_mergecheck, oeblxml_mergecheck, merge2_wiki_apis_df)
    w_id_list = wikisparql(wiki_mergecheck, update_wiki_id_list) 
    namespaces(h)
    exapisawdata = h.serialize(destination=f'intavia_wikidata_aw_{datetime.datetime.now().strftime("%d-%m-%Y")}.ttl', format='turtle')
    print("h serialization Done")




if __name__ == "__main__":
    main()