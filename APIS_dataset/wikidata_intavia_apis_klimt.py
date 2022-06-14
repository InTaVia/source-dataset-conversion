from cProfile import label
from operator import index, le
from pydoc import ModuleScanner
from typing import Any
from unittest import result
from numpy import r_
import requests
import pandas as pd
import json
import pprint
from rdflib import Graph, Literal, RDF, Namespace, URIRef
from rdflib.namespace import RDFS, FOAF
from SPARQLWrapper import SPARQLWrapper, N3
from lxml import etree
import re
import urllib.parse
import datetime
import numpy as np

# data=[]
# data_institutions=[]
# da=[]
# dapr= []
# mostprofessions=[]
# placedata=[]
# placecheck=set()
# placerelationscheck=set()
# familyrelationidlist=['5411', '5412', '5413', '5414', '5870']
# headers = {"accept": "application/json"}
# not_included=[]
wiki_oebl = pd.read_csv('basic_oebl_aw.csv')
wiki_aw = pd.read_csv('klimtbasicquery.csv')
#remove 5412 and add these relationships without roles? (it's just a "has family relation" role for undefined family relations), all relations checked, no further family relations in DB

#base_url_apis = "https://apis.acdh.oeaw.ac.at/apis/api/"
#base_url_apis_b = "https://apis.acdh.oeaw.ac.at/apis/api2/"
#next_page = f"{base_url_apis}entities/person/?limit=50&offset=50"
#next_page = f"{base_url_apis}entities/person/?limit=50&offset=30850"
"""initial json url to get apis data"""
#first_response = requests.get(next_page, headers=headers)
"""get data for this URL from REST API"""
#re_list=first_response.json()
"""get data from REST API in JSON format"""
#previous_page = re_list.get('previous')
"""previous json url to get apis data"""
#next_page = re_list.get('next')
"""following REST API url to get apis data"""
#response_list = re_list.get('results')
"""get list with all datasets from the url as dictionaries"""


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
idmwd=Namespace('http://www.intavia.eu/wd/')
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


h = Graph()
#Create undirected graph, assigned to h


# def wiki_person_graph(wo_df):
#     for index, row in wo_df.iterrows():
#         h.add((URIRef(idmapis+'personproxy/'+row['surname']+'/'+row['first name']), owl.sameAs , URIRef(row['item'])))
#         h.add((URIRef(idmapis+'personproxy/'+row['surname']+'/'+row['first name']), RDF.type, idmcore.Person_Proxy))
#         h.add((URIRef(idmapis+'personproxy/'+row['surname']+'/'+row['first name']), idmcore.person_proxy_for, URIRef(ex+'person/'+row['surname']+'/'+row['first name'])))
#         return h

def wiki_aw_graph(waw):
        h.add((URIRef(idmwd+'personproxy/Q34661'), idmcore.person_proxy_for, URIRef(ex+'person/100100')))
        #connect Person Proxy (ID in URL is wikidata item id) and person in named graph (random ID)
        h.add((URIRef(idmwd+'personproxy/Q34661'), RDF.type, idmcore.Person_Proxy))
        h.add((URIRef(idmwd+'personproxy/Q34661'), RDF.type, crm.E21_Person))
        #define Person Proxy
        h.add((URIRef(idmwd+'personproxy/Q34661'), owl.sameAs, URIRef(wd+'Q34661')))
        #define that wikidata entity proxy and wikidata entity are the same
        h.add((URIRef(idmwd+'appellation/'+'1/'+ 'Q3466'), rdfs.label, (Literal('Gustav Klimt'))))
        #label for (main) name
        for index, row in waw.iterrows():
            instance_id = str(row['instance'])[31:]
            h.add((URIRef(idmwd+'personproxy/Q34661'), idmcore.bearer_of, URIRef(idmwd+'eventrole/production/'+row['aw_wiki_id']+'/Q34661')))
            h.add((URIRef(idmwd+'eventrole/production/'+row['aw_wiki_id']+'/Q34661'), RDF.type, URIRef(idmrole + 'responsibleArtist')))
            h.add((URIRef(idmrole + 'responsibleArtist'), RDF.type, URIRef(idmrole + '/eventrole')))
            h.add((URIRef(idmrole + 'responsibleArtist'), RDFS.label, Literal("responsible artist")))
            h.add(((URIRef(idmwd+'event/production/'+row['aw_wiki_id']+'/Q34661')), idmcore.had_participant_in_role, URIRef(idmwd+'eventrole/production/'+row['aw_wiki_id']+'/Q34661')))
            h.add(((URIRef(idmwd+'event/production/'+row['aw_wiki_id']+'/Q34661')), RDFS.label, Literal("Gustav Klimt created: " + row['awLabel'])))
            h.add(((URIRef(idmwd+'event/production/'+row['aw_wiki_id']+'/Q34661')), crm.P108_has_produced, URIRef(idmwd+'choproxy/'+row['aw_wiki_id'])))
            h.add((URIRef(idmwd+'choproxy/'+row['aw_wiki_id']), RDFS.label, Literal(row['awLabel'])))
            h.add((URIRef(idmwd+'event/production/'+row['aw_wiki_id']+'/Q34661'), RDF.type, URIRef(crm+'E12_Production')))
            h.add((URIRef(idmwd+'event/production/'+row['aw_wiki_id']+'/Q34661'), crm.P32_used_general_technique, URIRef(idmwd+instance_id)))
            h.add((URIRef(idmwd+instance_id), RDFS.label, Literal(row['instanceLabel'])))
            h.add(((URIRef(idmwd+'event/production/'+row['aw_wiki_id']+'/Q34661')), URIRef(crm + "P4_has_time-span"), URIRef(idmwd+'creation/timespan/'+row['aw_wiki_id']+'/Q34661')))
            #add time-span to event
            h.add((URIRef(idmwd+'creation/timespan/'+row['aw_wiki_id']+'/Q34661'), crm.P82a_begin_of_the_begin, (Literal(row['inception']))))
            # check if necessary to add function to convert date formats
            #add begin of time-span
            #h.add((URIRef(idmwd+'creation/timespan/'+row['aw_wiki_id']+'/Q34661')), crm.P82b_end_of_the_end, (Literal(edate+'T23:59:59'))))
            #add end of time-span?
            #h.add((URIRef(idmwd+'choproxy/'+row['aw_wiki_id']), crm.))
        return h

def main():
    wiki_aw_df = wiki_aw.applymap(str)
    wiki_aw_df.head()
    h =  wiki_aw_graph(wiki_aw_df)
    #wiki_oebl_df=wiki_person_graph(wiki_oebl)
    print("Dataframes from CSV Done")

if __name__ == "__main__":
    main()



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
#Bind namespaces to prefixes for readable output
 
exapis = h.serialize(destination=f'intavia_wikidata_klimt_{datetime.datetime.now().strftime("%d-%m-%Y")}.ttl', format='turtle')

# # def parallelproperties(apis_id, crmtype, urltype):
# #     if str(crmtype) == "http://www.cidoc-crm.org/cidoc-crm/E67_Birth":
# #         g.add((URIRef(idmapis+urltype+'/'+row['apis_id']), crm.P98_brought_into_life, URIRef(idmapis+'personproxy/'+row['apis_id'])))
# #     elif str(crmtype) == "http://www.cidoc-crm.org/cidoc-crm/E69_Death":
# #         g.add((URIRef(idmapis+urltype+'/'+row['apis_id']), crm.P100_was_death_of, URIRef(idmapis+'personproxy/'+row['apis_id'])))
# #     return g

# # def placedetails(r_dataframe):
# #     for index, row in r_dataframe.iterrows():
# #         g.add((URIRef(idmapis+'place/'+row['relatedentityid']), RDF.type, crm.E53_Place))
# #         #define place as Cidoc E53 Place
# #         relatedentitylabel = urllib.parse.quote_plus(row['relatedentitylabel'])
# #         g.add((URIRef(idmapis+'place/'+row['relatedentityid']), crm.P1_is_identified_by, (URIRef(idmapis+'placeappellation/'+row['relatedentityid']+'/'+relatedentitylabel))))
# #         #add appellation to place
# #         g.add(((URIRef(idmapis+'placeappellation/'+row['relatedentityid']+'/'+relatedentitylabel)), RDF.type, crm.E33_E41_Linguistic_Appellation))
# #         #define appellation as linguistic appellation 
# #         g.add(((URIRef(idmapis+'placeappellation/'+row['relatedentityid']+'/'+relatedentitylabel)), RDFS.label, Literal(row['relatedentitylabel'])))
# #         #add label to appellation
# #         g.add((URIRef(idmapis+'place/'+row['relatedentityid']), crm.P1_is_identified_by, URIRef(idmapis+'placeidentifier/'+row['relatedentityid'])))
# #         #add APIS Identifier as Identifier
# #         g.add((URIRef(idmapis+'placeidentifier/'+row['relatedentityid']), RDF.type, crm.E_42_Identifier))
# #         #define APIS Identifier as E42 Identifier (could add a class APIS-Identifier or model a Identifier Assignment Event)
# #         g.add((URIRef(idmapis+'placeidentifier/'+row['relatedentityid']), RDFS.label, Literal(row['relatedentityid'])))
# #         #add label to APIS Identifier
# #         g.add((URIRef(idmapis+'place/'+row['relatedentityid']), owl.sameAs, (URIRef(row['relatedentityurl']))))
# #         #define that individual in APIS named graph and APIS entity are the same
# #     return(g)


# # def events(relation_id, apis_id, edate, crmtype, urltype, roletype, relationlabel):
# #     """add events according to BioCRM Model"""
# #     #urltype = 'birthevent'
# #     roletype = str(urllib.parse.quote_plus(roletype))
# #     #print(apis_id, edate, crmtype, urltype, roletype, relationlabel)
# #     g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.inheres_in, URIRef((idmapis+'{}/eventrole/{}/').format(urltype, relation_id))))
# #     #print(row['apis_id'])
# #     #add eventrole to person proxy
# #     g.add(((URIRef((idmapis+'{}/eventrole/{}/').format(urltype, relation_id))), RDF.type, (URIRef(idmrole+roletype))))
# #     g.add(((URIRef(idmrole+roletype), rdfs.subClassOf, idmcore.Event_Role)))
# #     #suggestion to add specific event role
# #     g.add(((URIRef(idmapis+urltype+'/'+relation_id)), idmcore.had_participant_in_role, (URIRef(((idmapis+'{}/eventrole/{}/').format(urltype, relation_id))))))
# #     #connect event and event role
# #     g.add(((URIRef(idmapis+urltype+'/'+relation_id)), RDF.type, crmtype))
# #     #define crm classification
# #     if isinstance(roletype, str):
# #         roletype = Literal(roletype)
# #     g.add((((URIRef((idmapis+'{}/eventrole/{}/').format(urltype, relation_id))), RDFS.label, Literal(roletype))))
# #     g.add((((URIRef(idmapis+urltype+'/'+relation_id))), RDFS.label, Literal(relationlabel)))
# #     if edate != "None":
# #         g.add((URIRef(idmapis+urltype+'/'+row['apis_id']), URIRef(crm + "P4_has_time-span"), URIRef(idmapis+urltype+'/timespan/'+row['apis_id'])))
# #         #add time-span to event
# #         g.add((URIRef(idmapis+urltype+'/timespan/'+row['apis_id']), crm.P82a_begin_of_the_begin, (Literal(edate+'T00:00:00'))))
# #         #add begin of time-span
# #         g.add((URIRef(idmapis+urltype+'/timespan/'+row['apis_id']), crm.P82b_end_of_the_end, (Literal(edate+'T23:59:59'))))
# #         #add end of time-span
# #     parallelproperties(apis_id, crmtype, urltype)
# #     # if place != "None":
# #     #     placedetails()
# #     return g



# for index, row in apis_df.iterrows():
#     #print(index)
#     """Create RDF Triples, according to IDM ontology."""
#     g.add(((URIRef(ex+'person/'+str(index))), RDF.type, idmcore.Provided_Person))
#     #print(row['apis_id'], index)
#     #Person Entity in shared InTaVia named graph
#     g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.person_proxy_for, URIRef(ex+'person/'+str(index))))
#     #connect Person Proxy and person in named graph
#     g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), RDF.type, idmcore.Person_Proxy))
#     g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), RDF.type, crm.E21_Person))
#     #define Person Proxy
#     g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), owl.sameAs, (URIRef(row['url']))))
#     #define that individual in APIS named graph and APIS entity are the same
#     g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), crm.P1_is_identified_by, URIRef(idmapis+'appellation/'+'1/'+row['apis_id'])))
#     #add main appellation from APIS data set
#     g.add((URIRef(idmapis+'appellation/'+'1/'+row['apis_id']), rdfs.label, (Literal(row['surname']+'  '+row['name']))))
#     #label for (main) name
#     g.add((URIRef(idmapis+'appellation/'+'1/'+row['apis_id']), RDF.type, crm.E33_E41_Linguistic_Appellation))
#     #define name as linguistic appellation
#     g.add((URIRef(idmapis+'appellation/'+'1/'+row['apis_id']), crm.P148_has_component, URIRef(idmapis+'appellation/'+'2/'+row['apis_id'])))
#     #add lastname component
#     g.add((URIRef(idmapis+'appellation/'+'2/'+row['apis_id']), rdfs.label, (Literal(row['name']))))
#     #label for lastname component
#     g.add((URIRef(idmapis+'appellation/'+'1/'+row['apis_id']), crm.P148_has_component, URIRef(idmapis+'appellation/'+'3/'+row['apis_id'])))
#     #add surname component
#     g.add((URIRef(idmapis+'appellation/'+'3/'+row['apis_id']), rdfs.label, (Literal(row['surname']))))
#     #label for surname component
#     g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), crm.P1_is_identified_by, URIRef(idmapis+'identifier'+row['apis_id'])))
#     #add APIS ID as identifier
#     g.add((URIRef(idmapis+'identifier'+row['apis_id']), RDF.type, crm.E42_Identifier))
#     #define APIS ID as E42 Identifier
#     g.add((URIRef(idmapis+'identifier'+row['apis_id']), rdfs.label, (Literal(row['apis_id']))))
#     #add label for APIS ID
#     #g.add
#     events(row['apis_id'], row['apis_id'], row['bdate'], crm.E67_Birth, 'birthevent', Literal('born_person'), (Literal("Birth of "+row['surname']+'  '+row['name'])))
#     #add birth event according to APIS
#     events(row['apis_id'], row['apis_id'], row['ddate'], crm.E69_Death, 'deathevent', Literal('deceased_person'), (Literal("Death of "+row['surname']+'  '+row['name'])))
#     #add death event according to APIS
#     g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.has_gender, URIRef(idmrole+row['gender'])))
#     #add gender to person
#     g.add((URIRef(idmrole+row['gender']), RDF.type, idmcore.Gender))
#     #define gender as idmcore gender
#     # g.add((URIRef(idmbibl+'source'+row['apis_id']), crm.P70_documents, URIRef(idmapis+'personproxy/'+row['apis_id'])))
#     # g.add((URIRef(idmbibl+'source'+row['apis_id']), RDF.type, crm.E31_Document))
#     # g.add((URIRef(idmbibl+'source'+row['apis_id']), RDF.type, bf.Work))
#     # sourceid = (row['sourceuri'])[-4:-1]
#     # g.add((URIRef(idmbibl+'source'+row['apis_id']), owl.sameAs, URIRef(row['sourceuri'])))
#     # g.add((URIRef(idmbibl), bf.hasPart, URIRef(idmbibl+'source'+row['apis_id'])))

# # for col in relations_df.columns:
# #     if col.endswith("id"):
# #         relations_df[col] = relations_df[col].astype(float).fillna(0).astype(int).astype(str)
# #     else:
# #         relations_df[col] = relations_df[col].astype(str)
# # for index, row in relations_df.iterrows():
# #     rtype = row['relationtypeurl']
# #     relation_id = str(row['relationid'])
# #     relationtype_id = str(row['relationtypeid'])
# #     relationtype_parentid = row['relationtype_parentid']
# #     if rtype == f"{base_url_apis}vocabularies/personplacerelation/595/":
# #         #define serialization for "person born in place relations"
# #         g.add((URIRef(idmapis+'birthevent/'+relation_id), crm.P7_took_place_at, URIRef(idmapis+'place/'+row['relatedentityid'])))
# #         placedetails(relations_df)
# #         break
# #     elif rtype == f"{base_url_apis}vocabularies/personplacerelation/596/":
# #         #define serialization for "person died in place relations"
# #         g.add((URIRef(idmapis+'deathevent/'+relation_id), crm.P7_took_place_at, URIRef(idmapis+'place/'+row['relatedentityid'])))
# #         placedetails(relations_df)
# #         break
# #     elif re.search(r'vocabularies/personplacerelation/.*', rtype):
# #         events(relation_id, str(row['apis_id']), 'None', crm.E5_Event, 'event', str(row['relationtypelabel']), str(row['relationlabel']))
# #         g.add((URIRef(idmapis+'event/'+relation_id), crm.P7_took_place_at, URIRef(idmapis+'place/'+row['relatedentityid'])))
# #         placedetails(relations_df)
# #         break
# #     elif relationtype_id in familyrelationidlist:
# #          """serializes parent/child family relations"""
# #          g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.has_family_relation, URIRef(idmapis+'familyrelation/'+row['relationid'])))
# #          g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(idmcore.Family_Relationship_Role)))
# #          #print(row['relationurl'], row['relationtypeurl'])
# #          g.add((URIRef(idmapis+'familyrelation/'+relation_id), RDF.type, URIRef(idmrelations+relationtype_id)))
# #          if relationtype_parentid != '0':
# #             g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(idmrelations+relationtype_parentid)))
# #             g.add((URIRef(idmrelations+relationtype_parentid), RDFS.subClassOf, URIRef(idmcore.Family_Relationship_Role)))
# #          #reltype= str(urllib.parse.quote_plus(row['relationtypelabel']))
# #          g.add((URIRef(idmapis+'familyrelation/'+relation_id), RDFS.label, Literal(row['relationtypelabel'])))
# #          g.add((URIRef(idmapis+'personproxy/'+row['relatedentityid']), idmcore.inheres_in, URIRef(idmapis+'familyrelation/'+row['relationid'])))
# #          g.add((URIRef(idmapis+'personproxy/'+row['relatedentityid']), idmcore.person_proxy_for, URIRef(intavia_shared+'person/'+str(index+50000))))
# #          break
# #          #adds other person-part of the family relation
# #     elif re.search(r'vocabularies/personinstitutionrelation/.*' , rtype):
# #         #connect personproxy and institutions with grouprelationship
# #         g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.has_group_relation, URIRef(idmapis+'grouprelation/'+relation_id)))
# #         # Person has a specific group relation
# #         g.add((URIRef(idmapis+'grouprelation/'+relation_id), RDF.type, URIRef(idmrelations+relationtype_id)))
# #         #define type of grouprelation
# #         if relationtype_parentid != '0':
# #             # if the relationtype has a superclass, it is added here
# #             g.add((URIRef(idmrelations+relationtype_id), rdfs.subClassOf, URIRef(idmrelations+relationtype_parentid)))   
# #         g.add((URIRef(idmapis+'grouprelation/'+relation_id), rdfs.label, Literal(row['relationtypelabel'])))
# #         # add label to relationtype
# #         g.add((URIRef(idmapis+'groupproxy/'+row['relatedentityid']), idmcore.inheres_in, URIRef(idmapis+'grouprelation/'+relation_id)))
# #         #group which is part of this relation
# #         g.add((URIRef(idmapis+'career/'+row['relationid']), RDF.type, idmcore.Career))
# #         # add career event of type idmcore:career
# #         g.add((URIRef(idmapis+'career/'+row['relationid']), rdfs.label, Literal(row['relationlabel'])))
# #         # label for career event
# #         g.add((URIRef(idmapis+'career/'+row['relationid']), idmcore.had_participant_in_role, URIRef(idmapis+'personrole/'+row['relationid']+'/'+row['apis_id'])))
# #         # role of participating person in the career event
# #         g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.inheres_in, URIRef(idmapis+'personrole/'+row['relationid']+'/'+row['apis_id'])))
# #         # person which inheres this role
# #         g.add((URIRef(idmapis+'career/'+row['relationid']), idmcore.had_participant_in_role, URIRef(idmapis+'grouprole/'+row['relationid']+'/'+row['relatedentityid'])))
# #         # role of institution/ group in the career event
# #         g.add((URIRef(idmapis+'groupproxy/'+row['relatedentityid']), idmcore.inheres_in, URIRef(idmapis+'grouprole/'+row['relationid']+'/'+row['relatedentityid'])))
# #         # institution/ group which inheres this role
# #         g.add((URIRef(idmapis+'career/'+row['relationid']), URIRef(crm + "P4_has_time-span"), URIRef(idmapis+'career/timespan/'+row['relationid'])))
# #         if (row['rd_start_date'] != 'None') and (row['rd_end_date'] != 'None'):
# #             g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), rdfs.label, Literal(row['rd_start_date_written'])+' - '+ row['rd_end_date_written']))
# #         elif ((row['rd_start_date'] != 'None') and (row['rd_end_date']=='None')):
# #             g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), rdfs.label, Literal(row['rd_start_date_written'])))
# #         elif ((row['rd_start_date'] == 'None') and (row['rd_end_date']!='None')):
# #             g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), rdfs.label, Literal('time-span end:' + row['rd_end_date_written'])))
# #         else:
# #             continue
# #         #g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), crm.P82a_begin_of_the_begin, (Literal(row['rd_start_date']+'T00:00:00'))))
# #         #if re.search('-01-01', row['institution_start_date']) != None:
# #             #start_date_year = (row['institution_start_date'])[0:4]
# #             #g.add((URIRef(idmapis+'groupstart/timespan/'+row['institution_id']), crm.P82b_end_of_the_end, (Literal(start_date_year+'-12-31'+'T23:59:59'))))
# #         #else:
# #     # if row['rd_end_date'] != "None":
# #     #     g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), crm.P82b_end_of_the_end, (Literal(row['rd_end_date']+'T23:59:59'))))
# #     elif re.search(r'vocabularies/personpersonrelation/.*' , rtype):
# #         g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.has_person_relation, URIRef(idmapis+'personrelation/' +row['relationid'])))
# #         g.add((URIRef(idmapis+'personrelation/'+row['relationid']), RDF.type, URIRef(idmrelations+relationtype_id)))
# #         g.add((URIRef(idmapis+'personrelation/'+row['relationid']), RDFS.label, Literal(row['relationtypelabel'])))
# #         g.add(((URIRef(idmrelations+relationtype_id)), RDFS.subClassOf, (URIRef(idmcore.Person_Relationship_Role))))
# #         g.add((URIRef(idmapis+'personproxy/'+row['relatedentityid']), idmcore.inheres_in, URIRef(idmapis+'personrelation/'+row['relationid'])))
# #         #connect personproxy and institutions with grouprelationship
# #         g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.has_group_relation, URIRef(idmapis+'grouprelation/'+row['relationid'])))
# #         g.add((URIRef(idmapis+'grouprelation/'+relation_id), RDF.type, URIRef(idmrelations+relationtype_id)))
# #         if relationtype_parentid != '0':
# #             #adds parent class for relationship types
# #             g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(idmrelations+relationtype_parentid)))
# #             g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(idmcore.Group_Relationship_Role)))
# #         #defines relationship as idm group relationship
# #         g.add((URIRef(idmapis+'grouprelation/'+relation_id), RDFS.label, Literal(row['relationtypelabel'])))
# #         g.add((URIRef(idmapis+'groupproxy/'+row['relatedentityid']), idmcore.inheres_in, URIRef(idmapis+'grouprelation/'+relation_id)))
# #         #group which is part of this relation
# #     elif re.search(r'vocabularies/personeventrelation/.*', rtype):
# #         events(relation_id, str(row['apis_id']), 'None', crm.E5_Event, 'event', str(row['relationtypelabel']), str(row['relationlabel']))
# #         #add general event according to APIS
# #     else:
# #         not_included.append(row['relationid']+row['relationtypeurl'])
# #         #print("not included: "+row['relationid']+row['relationtypeurl'])




# # for index, row in institutions_df.iterrows():
# #     g.add(((URIRef(ex+'group/'+str(index))), RDF.type, idmcore.Provided_Group))
# #     g.add((URIRef(idmapis+'groupproxy/'+row['institution_id']), idmcore.group_proxy_for, URIRef(ex+'group/'+str(index))))
# #     #connect Group Proxy and person in named graphbgn:BioDes
# #     g.add((URIRef(idmapis+'groupproxy/'+row['institution_id']), RDF.type, idmcore.Group))
# #     #defines group class
# #     g.add((URIRef(idmapis+'groupproxy/'+row['institution_id']), owl.sameAs, URIRef(row['institution_url'])))
# #     #defines group as the same group in the APIS dataset
# #     g.add((URIRef(idmapis+'groupproxy/'+row['institution_id']), crm.P1_is_identified_by, URIRef(idmapis+'groupappellation/'+row['institution_id'])))
# #     g.add((URIRef(idmapis+'groupappellation/'+row['institution_id']), rdfs.label, Literal(row['institution_name'])))
# #     g.add((URIRef(idmapis+'groupappellation/'+row['institution_id']), RDF.type, crm.E33_E41_Linguistic_Appellation))
# #     #add group appellation and define it as linguistic appellation
# #     if row['institution_start_date'] != "None":
# #         #print(row['institution_name'], ':', row['institution_start_date'], row['institution_end_date'], row['institution_start_date_written'], row['institution_end_date_written'])
# #         g.add((URIRef(idmapis+'groupstart/'+row['institution_id']), RDF.type, crm.E63_Beginning_of_Existence))
# #         g.add((URIRef(idmapis+'groupstart/'+row['institution_id']), crm.P92_brought_into_existence, URIRef(idmapis+'groupproxy/'+row['institution_id'])))
# #         g.add((URIRef(idmapis+'groupstart/'+row['institution_id']), URIRef(crm + "P4_has_time-span"), URIRef(idmapis+'groupstart/timespan/'+row['institution_id'])))
# #         g.add((URIRef(idmapis+'groupstart/timespan/'+row['institution_id']), crm.P82a_begin_of_the_begin, (Literal(row['institution_start_date']+'T00:00:00'))))
# #         if re.search('-01-01', row['institution_start_date']) != None:
# #             start_date_year = (row['institution_start_date'])[0:4]
# #             g.add((URIRef(idmapis+'groupstart/timespan/'+row['institution_id']), crm.P82b_end_of_the_end, (Literal(start_date_year+'-12-31'+'T23:59:59'))))
# #         else:
# #             g.add((URIRef(idmapis+'groupstart/timespan/'+row['institution_id']), crm.P82b_end_of_the_end, (Literal(row['institution_start_date']+'T23:59:59'))))
# #     #Beginning of existence of this group
# #     if row['institution_end_date'] != "None":
# #         g.add((URIRef(idmapis+'groupend/'+row['institution_id']), RDF.type, crm.E64_End_of_Existence))
# #         g.add((URIRef(idmapis+'groupend/'+row['institution_id']), crm.P93_took_out_of_existence, URIRef(idmapis+'groupproxy/'+row['institution_id'])))
# #         g.add((URIRef(idmapis+'groupend/'+row['institution_id']), URIRef(crm + "P4_has_time-span"), URIRef(idmapis+'groupend/timespan/'+row['institution_id'])))
# #         g.add((URIRef(idmapis+'groupend/timespan/'+row['institution_id']), crm.P82a_begin_of_the_begin, (Literal(row['institution_end_date']+'T00:00:00'))))
# #         if re.search('-01-01', row['institution_end_date']) != None:
# #             end_date_year = (row['institution_end_date'])[0:4]
# #             g.add((URIRef(idmapis+'groupend/timespan/'+row['institution_id']), crm.P82b_end_of_the_end, (Literal(end_date_year+'-12-31'+'T23:59:59'))))
# #         else:
# #             g.add((URIRef(idmapis+'groupend/timespan/'+row['institution_id']), crm.P82b_end_of_the_end, (Literal(row['institution_end_date']+'T23:59:59'))))
# #     #End of Existence for this group

# # for index, row in occupations_df.iterrows():
# #     g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.has_occupation, URIRef(idmapis+'occupation/'+row['professionid'])))
# #     g.add((URIRef(idmapis+'occupation/'+row['professionid']), rdfs.label, Literal(row['professionlabel'])))
# #     g.add((URIRef(idmapis+'occupation/'+row['professionid']), rdfs.subClassOf, idmcore.Occupation))
# #     if row['professionparentid'] != "nan":
# #         professionparentid = row['professionparentid'][:-2]
# #         g.add((URIRef(idmapis+'occupation/'+row['professionid']), rdfs.subClassOf, URIRef(idmapis+'occupation/'+professionparentid)))
# #         g.add((URIRef(idmapis+'occupation/'+professionparentid), rdfs.subClassOf, idmcore.Occupation))



# # # reactivate when server problem is solved!
# # # for index, row in places_df.iterrows():
# # #     g.add((URIRef(idmapis+'place/'+row['place_id']), crm.P168_place_is_defined_by, Literal(row['place_lat']+' '+row['place_lng'])))
# # #     #suggestion for serialization of space primitives according to ISO 6709, to be discussed
# # #     #more place details will be added (references, source, place start date, place end date, relations to other places(?))

# g.bind('crm', crm)
# g.bind('intaviashared', intavia_shared)
# g.bind('ore', ore)
# g.bind('edm', edm)
# g.bind('owl', owl)
# g.bind('rdf', rdf)
# g.bind('xml', xml)
# g.bind('xsd', xsd)
# g.bind('bioc', bioc)
# g.bind('rdfs', rdfs)
# g.bind('apis', apis)
# g.bind('idmcore', idmcore)
# g.bind('idmrole', idmrole)
# g.bind('idmrelations', idmrelations)
# g.bind('owl', owl)
# #Bind namespaces to prefixes for readable output
 
# exapis = g.serialize(destination=f'wikidata20220609_{datetime.datetime.now().strftime("%d-%m-%Y")}.ttl', format='turtle')