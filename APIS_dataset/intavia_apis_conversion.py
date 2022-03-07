from operator import index, le
from pydoc import ModuleScanner
from unittest import result
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

data=[]
da=[]
dapr= []
mostprofessions=[]
placedata=[]
placecheck=set()
familyrelationidlist=['5411', '5412', '5413', '5414', '5870']
headers = {"accept": "application/json"}
#remove 5412 and add these relationships without roles? (it's just a "has family relation" role for undefined family relations), all relations checked, no further family relations in DB

base_url_apis = "https://apis.acdh.oeaw.ac.at/apis/api/"
next_page = f"{base_url_apis}entities/person/?limit=50&offset=0"
#next_page = f"{base_url_apis}entities/person/?limit=50&offset=30850"
"""initial json url to get apis data, when not testing(!!!): https://apis.acdh.oeaw.ac.at/apis/api/entities/person/?limit=10&offset=0"""
first_response = requests.get(next_page, headers=headers)
"""get data for this URL from REST API"""
re_list=first_response.json()
"""get data from REST API in JSON format"""
#previous_page = re_list.get('previous')
"""previous json url to get apis data"""
#next_page = re_list.get('next')
"""following REST API url to get apis data"""
response_list = re_list.get('results')
"""get list with all datasets from the url as dictionaries"""

def professions(id, profy):
    """create df with all data about a persons occupations"""
    if profy != None:
        for k in range(len(profy)):
            p = profy[k]
            dapr.append({
                'apis_id':id,
                'professionlabel':p.get('label'),
                'professionid':p.get('id'),
                'professionparentid':p.get('parent_id'),
                'professionurl':p.get('url')
            })
        return(dapr)
    
# reactivate when server response problem is solved
# def places(pc):
#     for p in pc:
#         place_response = requests.get(p, headers=headers)
#         place_response = place_response.json()
#          #print(place_response['url'])
#          #place_url, place_id, place_name, place_start_date, place_end_date, place_references, place_lat, place_lng, place_source = place_response['related_entity']
#         placedata.append({
#             'place_name':place_response['name'],
#             'place_id':place_response['id'],
#             'place_start_date':place_response['start_date'], 
#             'place_end_date':place_response['end_date'], 
#             'place_references':place_response['references'], 
#             'place_lat':place_response['lat'], 
#             'place_lng':place_response['lng'], 
#             'place_source':place_response['source']
#             })
#     return(placedata)



def relations(person):
    """create df with all data about a persons relation"""
    apis_id = person.get('id')
    #get person ID (APIS ID)
    rs = person.get('relations')
    #get relations for this person from REST API
    relvalues = []
    for l in range(len(rs)):
        y = rs[l]
        # y is one relation for a person
        relationsid = (y.get('id'))
        relationslabel = y.get('label')
        relationsurl = y.get('url')
        relationtype = y.get('relation_type')
        relatedentity = y.get('related_entity')
        da.append({
            'apis_id':apis_id,
            'relationid':relationsid,
            'relationlabel':relationslabel,
            'relationurl':relationsurl,
            'relationtypeid':relationtype.get('id'),
            'relationtypelabel':relationtype.get('label'),
            'relationtypeurl':relationtype.get('url'),
            'relationtype_parentid':relationtype.get('parent_id'),
            'relatedentityid':relatedentity['id'],
            'relatedentitylabel':relatedentity.get('label'),
            'relatedentityurl':relatedentity.get('url'),
            'relatedentitytype':relatedentity.get('type')
         })
        for d in da:
            if d['relatedentitytype']=="Place":
                placecheck.add(d['relatedentityurl'])
        return(da, placecheck)

def sources(sourcy):
    #get sources for one biography article
    if sourcy != None:
        sourceuri = sourcy['url']
        source = requests.get(sourceuri, headers=headers)
        source = source.json()
        pubinfo = source.get('pubinfo')
        return(sourceuri, pubinfo)

def datareturn (d, re):
    for n in range(len(re)):
        x = re[n]
        apisid = x.get('id')
        profx = (x.get('profession'))
        professions(apisid, profx)
        personuri = (x.get('url'))
        person_response = requests.get(personuri, headers=headers)
        person_response = person_response.json()
        sourcx = (x.get('source'))
        if sourcx != None:
            sourceuri, source_pubinfo = sources(sourcx)
        else:
            sourceuri = None
            source_pubinfo = None
        if person_response != None:
            datarelations, pc = relations(person_response)
        d.append({
                'apis_id':x.get('id'),
                'name':x.get('name'),
                'surname':x.get('first_name'),
                'url':x.get('url'),
                'sourceuri':sourceuri,
                'publication_info':source_pubinfo,
                'references':x.get('references'),
                'bdate':x.get('start_date'),
                'ddate':x.get('end_date'),
                'gender':x.get('gender')
            })
    # reactivate when server problem is solved!
    # places(pc)
    return d,datarelations, pc

#print(next_page)
while next_page != f"{base_url_apis}entities/person/?limit=50&offset=100":
#define the point when iterating stops (for test serialization)
#while next_page != None:
    """iterate over JSON API urls"""
    first_response = requests.get(next_page, headers=headers)
    re_list=first_response.json()
    response_list = re_list.get('results')
    """get data from REST API in JSON format"""
    next_page = re_list.get('next')
    datageneral, datarelations, placeset = datareturn(data, response_list)
else:
    """stop iterating over JSON API urls"""
    re_list=first_response.json()
    print('Done')
    response_list = re_list.get('results')
    datageneral, datarelations, placeset  = datareturn(data, response_list)
    

apis_df=pd.DataFrame(datageneral)
apis_df.head()
"""dataframe with biography data"""
relations_df=pd.DataFrame(datarelations)
relations_df.head()
"""dataframe with relations data"""
occupations_df=pd.DataFrame(dapr)
occupations_df.head()
"""dataframe with occupations data"""
# reactivate when server problem is solved!
# places_df=pd.DataFrame(placedata)
# places_df.head()



"""Import libraries."""

crm=Namespace('http://www.cidoc-crm.org/cidoc-crm/')
"""Defines namespace for CIDOC CRM."""
ex=Namespace('https://www.intavia.org/')
"""Defines namespace for own ontology."""
idmcore=Namespace('https://www.intavia.org/idm-core/')
"""Defines namespace for own ontology."""
idmrole=Namespace('https://www.intavia.org/idm-role/')
"""Defines namespace for role ontology."""
idmapis=Namespace('https://www.intavia.org/apis/')
"""Namespace for InTaVia named graph"""
idmbibl=Namespace('https://www.intavia.org/idm-bibl/')
"""Namespace for bibliographic named graph"""
idmrelations=Namespace('https://www.intavia.org/idm-relations')
"""Defines namespace for relation ontology."""
intavia_shared=Namespace('https://www.intavia.org/shared-entities')
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

apis_df = apis_df.applymap(str)
relations_df = relations_df.applymap(str)
occupations_df = occupations_df.applymap(str)
# reactivate when server problem is solved!
# places_df = places_df.applymap(str)

g = Graph()
#Create undirected graph, assigned to g

def events(apis_id, edate, crmtype, urltype, roletype):
    """add events according to BioCRM Model"""
    #urltype = 'birthevent'
    g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.inheres_in, URIRef(idmapis+'{}/eventrole/1/'.format(urltype)+row['apis_id'])))
    #print(row['apis_id'])
    #add eventrole to person proxy
    g.add((URIRef(idmapis+'{}/eventrole/1/'.format(urltype)+row['apis_id']), RDF.type, roletype))
    g.add((roletype, rdfs.subClassOf, idmcore.Event_Role))
    #suggestion to add specific event role
    g.add((URIRef(idmapis+urltype+'/'+row['apis_id']), idmcore.had_participant_in_role, (URIRef(idmapis+urltype+'/eventrole/1/'+row['apis_id']))))
    #connect event and event role
    g.add((URIRef(idmapis+urltype+'/'+row['apis_id']), RDF.type, crmtype))
    #define crm classification
    g.add((URIRef(idmapis+urltype+'/'+row['apis_id']), crm.P4_has_time_span, URIRef(idmapis+urltype+'/timespan/'+row['apis_id'])))
    #add time-sspan to event
    g.add((URIRef(idmapis+urltype+'/timespan/'+row['apis_id']), crm.P82a_begin_of_the_begin, (Literal(edate+'T00:00:00'))))
    #add begin of time-span
    g.add((URIRef(idmapis+urltype+'/timespan/'+row['apis_id']), crm.P82b_end_of_the_end, (Literal(edate+'T23:59:59'))))
    #add end of time-span
    return g



for index, row in apis_df.iterrows():
    #print(index)
    """Create RDF Triples, according to IDM ontology."""
    g.add(((URIRef(ex+'person/'+str(index))), RDF.type, idmcore.Provided_Person))
    #print(row['apis_id'], index)
    #Person Entity in shared InTaVia named graph
    g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.person_proxy_for, URIRef(ex+'person/'+str(index))))
    #connect Person Proxy and person in named graph
    g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), RDF.type, idmcore.Person_Proxy))
    #define Person Proxy
    g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), owl.sameAs, (URIRef(row['url']))))
    #define that individual in APIS named graph and APIS entity are the same
    g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), crm.P1_is_identified_by, URIRef(idmapis+'appellation/'+'1/'+row['apis_id'])))
    #add main appellation from APIS data set
    g.add((URIRef(idmapis+'appellation/'+'1/'+row['apis_id']), rdfs.label, (Literal(row['surname']+'  '+row['name']))))
    #label for (main) name
    g.add((URIRef(idmapis+'appellation/'+'1/'+row['apis_id']), RDF.type, crm.E33_E41_Linguistic_Appellation))
    #define name as linguistic appellation
    g.add((URIRef(idmapis+'appellation/'+'1/'+row['apis_id']), crm.P148_has_component, URIRef(idmapis+'appellation/'+'2/'+row['apis_id'])))
    #add lastname component
    g.add((URIRef(idmapis+'appellation/'+'2/'+row['apis_id']), rdfs.label, (Literal(row['name']))))
    #label for lastname component
    g.add((URIRef(idmapis+'appellation/'+'1/'+row['apis_id']), crm.P148_has_component, URIRef(idmapis+'appellation/'+'3/'+row['apis_id'])))
    #add surname component
    g.add((URIRef(idmapis+'appellation/'+'3/'+row['apis_id']), rdfs.label, (Literal(row['surname']))))
    #label for surname component
    g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), crm.P1_is_identified_by, URIRef(idmapis+'identifier'+row['apis_id'])))
    #add APIS ID as identifier
    g.add((URIRef(idmapis+'identifier'+row['apis_id']), RDF.type, crm.E42_Identifier))
    #define APIS ID as E42 Identifier
    g.add((URIRef(idmapis+'identifier'+row['apis_id']), rdfs.label, (Literal(row['apis_id']))))
    #add label for APIS ID
    #g.add
    events(row['apis_id'], row['bdate'], crm.E67_Birth, 'birthevent', idmrole.born_person)
    #add birth event according to APIS
    events(row['apis_id'], row['ddate'], crm.E69_Death, 'deathevent', idmrole.deceased_person)
    #add death event according to APIS
    g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.has_gender, URIRef(idmrole+row['gender'])))
    #add gender to person
    g.add((URIRef(idmrole+row['gender']), RDF.type, idmcore.Gender))
    #define gender as idmcore gender

for index, row in relations_df.iterrows():
    rtype = row['relationtypeurl']
    relation_id = str(row['relationid'])
    relationtype_id = str(row['relationtypeid'])
    relationtype_parentid = str(row['relationtype_parentid'])
    #print(rtype)
    if rtype == f"{base_url_apis}vocabularies/personplacerelation/595/":
        #define serialization for "person born in place relations"
        g.add((URIRef(idmapis+'birthevent/'+row['apis_id']), crm.P7_took_place_at, URIRef(idmapis+'place/'+row['relatedentityid'])))
        #add place to birthevent, if available
        g.add((URIRef(idmapis+'place/'+row['relatedentityid']), RDF.type, crm.E53_Place))
        #define place as Cidoc E53 Place
        relatedentitylabel = urllib.parse.quote_plus(row['relatedentitylabel'])
        g.add((URIRef(idmapis+'place/'+row['relatedentityid']), crm.P1_is_identified_by, (URIRef(idmapis+'placeappellation/'+row['relatedentityid']+'/'+relatedentitylabel))))
        #add appellation to place
        g.add(((URIRef(idmapis+'placeappellation/'+row['relatedentityid']+'/'+relatedentitylabel)), RDF.type, crm.E33_E41_Linguistic_Appellation))
        #define appellation as linguistic appellation 
        g.add(((URIRef(idmapis+'placeappellation/'+row['relatedentityid']+'/'+relatedentitylabel)), RDFS.label, Literal(row['relatedentitylabel'])))
        #add label to appellation
        g.add((URIRef(idmapis+'place/'+row['relatedentityid']), crm.P1_is_identified_by, URIRef(idmapis+'placeidentifier/'+row['relatedentityid'])))
        #add APIS Identifier as Identifier
        g.add((URIRef(idmapis+'placeidentifier/'+row['relatedentityid']), RDF.type, crm.E_42_Identifier))
        #define APIS Identifier as E42 Identifier (could add a class APIS-Identifier or model a Identifier Assignment Event)
        g.add((URIRef(idmapis+'placeidentifier/'+row['relatedentityid']), RDFS.label, Literal(row['relatedentityid'])))
        #add label to APIS Identifier
        g.add((URIRef(idmapis+'place/'+row['relatedentityid']), owl.sameAs, (URIRef(row['relatedentityurl']))))
        #define that individual in APIS named graph and APIS entity are the same
    elif relationtype_id in familyrelationidlist:
         """serializes parent/child family relations"""
         g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.has_family_relation, URIRef(idmapis+'familyrelation/'+row['relationid'])))
         g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(idmcore.Family_Relationship_Role)))
         #print(row['relationurl'], row['relationtypeurl'])
         g.add((URIRef(idmapis+'familyrelation/'+relation_id), RDF.type, URIRef(idmrelations+relationtype_id)))
         if relationtype_parentid != 'nan':
            g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(idmrelations+relationtype_parentid)))
            g.add((URIRef(idmrelations+relationtype_parentid), RDFS.subClassOf, URIRef(idmcore.Family_Relationship_Role)))
         #reltype= str(urllib.parse.quote_plus(row['relationtypelabel']))
         g.add((URIRef(idmapis+'familyrelation/'+relation_id), RDFS.label, Literal(row['relationtypelabel'])))
         g.add((URIRef(idmapis+'personproxy/'+row['relatedentityid']), idmcore.inheres_in, URIRef(idmapis+'familyrelation/'+row['relationid'])))
         g.add((URIRef(idmapis+'personproxy/'+row['relatedentityid']), idmcore.person_proxy_for, URIRef(intavia_shared+'person/'+str(index+50000))))
    #     #adds other person-part of the family relation
    elif re.search(r'vocabularies/personinstitutionrelation/.*' , rtype):
        g.add((URIRef(idmapis+'personproxy/'+row['apis_id']), idmcore.has_group_relation, URIRef(idmapis+'grouprelation/'+row['relationid'])))
        g.add((URIRef(idmapis+'grouprelation/'+relation_id), RDF.type, URIRef(idmrelations+relationtype_id)))
        if relationtype_parentid != 'nan':
            g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(idmrelations+relationtype_parentid)))
            g.add((URIRef(idmrelations+relationtype_parentid), RDFS.subClassOf, URIRef(idmcore.Group_Relationship_Role)))
        g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(idmcore.Group_Relationship_Role)))
        g.add((URIRef(idmapis+'grouprelation/'+relation_id), RDFS.label, Literal(row['relationtypelabel'])))
        g.add((URIRef(idmapis+'group/'+row['relatedentityid']), idmcore.inheres_in, URIRef(idmapis+'grouprelation/'+relation_id)))
        g.add((URIRef(idmapis+'group/'+row['relatedentityid']), RDF.type, idmcore.Group))
        g.add((URIRef(idmapis+'group/'+row['relatedentityid']), owl.sameAs, URIRef(row['relatedentityurl'])))
        g.add((URIRef(idmapis+'group/'+row['relatedentityid']), crm.P1_is_identified_by, URIRef(idmapis+'groupappellation/'+row['relatedentityid'])))
        g.add((URIRef(idmapis+'groupappellation/'+row['relatedentityid']), rdfs.label, Literal(row['relatedentitylabel'])))
        g.add((URIRef(idmapis+'groupappellation/'+row['relatedentityid']), RDF.type, crm.E33_E41_Linguistic_Appellation))
    else:
        ("!!!")

# reactivate when server problem is solved!
# for index, row in places_df.iterrows():
#     g.add((URIRef(idmapis+'place/'+row['place_id']), crm.P168_place_is_defined_by, Literal(row['place_lat']+' '+row['place_lng'])))
#     #suggestion for serialization of space primitives according to ISO 6709, to be discussed
#     #more place details will be added (references, source, place start date, place end date, relations to other places(?))

g.bind('crm', crm)
g.bind('intaviashared', intavia_shared)
g.bind('ore', ore)
g.bind('edm', edm)
g.bind('owl', owl)
g.bind('rdf', rdf)
g.bind('xml', xml)
g.bind('xsd', xsd)
g.bind('bioc', bioc)
g.bind('rdfs', rdfs)
g.bind('apis', apis)
g.bind('idmcore', idmcore)
g.bind('idmrole', idmrole)
g.bind('idmrelations', idmrelations)
g.bind('owl', owl)
#Bind namespaces to prefixes for readable output
 
exapis = g.serialize(destination='apisdata.ttl', format='turtle')

# exapis = g.serialize(format='turtle')   

# with open('apisdata.ttl', 'w') as f:
#     f.write(str(exapis))
# #"""Write graph data in file (!!!).""" 