from cProfile import label
from operator import index, le
from pydoc import ModuleScanner
from unittest import result
from numpy import r_
import requests
import pandas as pd
import json
import pprint
from rdflib import Graph, Literal, RDF, Namespace, URIRef
from rdflib.namespace import RDFS, FOAF, XSD
from SPARQLWrapper import SPARQLWrapper, N3
from lxml import etree
import re
import urllib.parse
import datetime

base_url_apis = "http://127.0.0.1:8000/apis/api/"
base_url_apis2 = "http://127.0.0.1:8000/apis/api2/"
familyrelationidlist=['5411', '5412', '5413', '5414', '5870']
data=[]
data_institutions=[]
da=[]
dapr= []
mostprofessions=[]
placedata=[]
placecheck=set()
familyrelationidlist=['5411', '5412', '5413', '5414', '5870']
headers = {"accept": "application/json"}
#remove 5412 and add these relationships without roles? (it's just a "has family relation" role for undefined family relations), all relations checked, no further family relations in DB

next_page = f"{base_url_apis}entities/person/?limit=50&offset=0&collection=86"
#next_page = f"{base_url_apis}entities/person/?limit=50&offset=30850"
"""initial json url to get apis data"""
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

next_page_institution = f"{base_url_apis}entities/institution/?limit=50&offset=0"
first_response_institution = requests.get(next_page_institution, headers=headers)
"""get data for this URL from REST API"""
re_list_institution=first_response_institution.json()
"""get data from REST API in JSON format"""
response_list_institution = re_list_institution.get('results')
"""get list with all datasets from the url as dictionaries"""

urls_places_already_fetched = []


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
owl=Namespace('http://www.w3.org/2002/7/owl#')
"""Defines namespace for Europeana data model vocabulary."""
rdf=Namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#')
"""Defines namespace for Europeana data model vocabulary."""
xml=Namespace('http://www.w3.org/XML/1998/namespace')
"""Defines namespace for Europeana data model vocabulary."""
xsd=Namespace('http://www.w3.org/2001/XMLSchema#')
"""Defines namespace for Europeana data model vocabulary."""
bioc=Namespace('http://ldf.fi/schema/bioc/')
"""Defines namespace for Europeana data model vocabulary."""
rdfs=Namespace('http://www.w3.org/2000/01/rdf-schema#')
"""Defines namespace for Europeana data model vocabulary."""
apis=Namespace('https://www.apis.acdh.oeaw.ac.at/')
"""Defines namespace for APIS database."""
owl=Namespace('http://www.w3.org/2002/07/owl#')
"""Defines OWL namespace."""
bf=Namespace('http://id.loc.gov/ontologies/bibframe/')
"""Defines bibframe namespace."""
geo=Namespace("http://www.opengis.net/ont/geosparql#")



def datareturn_institution (data_institutions, re):
    for n in range(len(re)):
        x = re[n]
        try:
            institutionid = x.get('id')
            data_institutions.append({
                    'institution_id':x.get('id'),
                    'institution_name':x.get('name'),
                    'institution_url':x.get('url'),
                    'institution_source':x.get('source'),
                    'institution_references':x.get('references'),
                    'institution_start_date':x.get('start_date'),
                    'institution_end_date':x.get('end_date'),
                    'institution_start_date_written':x.get('start_date_written'),
                    'institution_end_date_written':x.get('end_date_written'),
                    'institution_text':x.get('text')
                })
        except:
            print(f"exception in institutions: {x}")
    return(data_institutions)

def professions(id, profy, dapr):
    """create df with all data about a persons occupations"""
    if profy != None:
        for k in range(len(profy)):
            p = profy[k]
            try:
                dapr.append({
                    'apis_id':id,
                    'professionlabel':p.get('label'),
                    'professionid':p.get('id'),
                    'professionparentid':p.get('parent_id'),
                    'professionurl':p.get('url')
                })
            except:
                print(f"exception in professions: {p}")
        return(dapr)
    
def geonamesref(place_id, headers):
    if place_id != None:
        try:
            placeurl = f"{base_url_apis2}entity/{place_id}/?format=json"
            pl_ref_response = requests.get(placeurl, headers=headers)
            pl_ref_json = pl_ref_response.json()
            pl_ref = pl_ref_json.get('uris')
        except json.decoder.JSONDecodeError:
            print(f"TIMEOUT in geonamesref function (api2) for: {place_id}")
            return(None)
        for uri in pl_ref:
            if (uri['uri'].startswith("https://apis-edits.acdh-dev.oeaw.ac.at/")) or (uri['uri'].startswith("https://apis.acdh.oeaw.ac.at/")):
                georef = None
            else:
                return(uri["uri"])

#reactivate when server response problem is solved
def places(placedata, placecheck, urls_places_already_fetched, headers):
    #print("getting places from apis API")
    for p in placecheck:
        if p in urls_places_already_fetched:
            continue
        else:
            try:
                urls_places_already_fetched.append(p)
                place_response = requests.get(p, headers=headers)
                place_response = place_response.json()
                place_id = place_response['id']
                georef = geonamesref(place_id, headers)
            except json.decoder.JSONDecodeError:
                print(f"TIMEOUT (api1) for: {p}")
                return(placedata, placecheck, urls_places_already_fetched)
        try:
            placedata.append({
                'apis_place_url':place_response['url'],
                'place_name':place_response['name'],
                'place_id':place_response['id'],
                'place_start_date':place_response['start_date'], 
                'place_end_date':place_response['end_date'], 
                'place_references':place_response['references'], 
                'place_lat':place_response['lat'], 
                'place_lng':place_response['lng'], 
                'place_source':place_response['source'],
                'georef': georef
                })
        except:
            print(f"exception in places: {p}")
    return(placedata, placecheck, urls_places_already_fetched)

def reldetails(relationuri, headers):
    if relationuri != None:
        rd_response = requests.get(relationuri, headers=headers)
        rd_json = rd_response.json()
        rd_url = rd_json.get('url')
        rd_id = rd_json.get('id')
        rd_start_date = rd_json.get('start_date')
        rd_start_start_date = rd_json.get('start_start_date')
        rd_start_date_written = rd_json.get('start_start_date')
        rd_end_date = rd_json.get('end_date')
        rd_end_date_written = rd_json.get('end_date_written')
        rd_end_end_date = rd_json.get('end_end_date')
        #rd_institution = None
        #if re.search(r'/personinstitutionrelation/.*' , rd_url):
            #rd_institution = rd_json.get('related_institution')
        return (rd_url, rd_id, rd_start_date, rd_start_start_date, rd_start_date_written, rd_end_date, rd_end_date_written, rd_end_end_date)


def relations(person, da, placecheck, placerelationscheck, headers):
    """create df with basic data about a persons relation"""
    apis_id = person.get('id')
    #get person ID (APIS ID)
    rs = person.get('relations')
    # print(rs)
    #get relations for this person from REST API
    #relvalues = []
    for l in range(len(rs)):
        y = rs[l]
        # y is one relation for a person
        try:
            relationsid = (y.get('id'))
            relationslabel = y.get('label')
            relationsurl = y.get('url')
            rd_url, rd_id, rd_start_date, rd_start_start_date, rd_start_date_written, rd_end_date, rd_end_date_written, rd_end_end_date= reldetails(relationsurl, headers)
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
                'relatedentitytype':relatedentity.get('type'),
                'rd_id': rd_id,
                'rd_start_date':rd_start_date,
                'rd_start_date_written':rd_start_date_written,
                'rd_start_start_date':rd_start_start_date,
                'rd_end_date':rd_end_date,
                'rd_end_date_written':rd_end_date_written,
                'rd_end_end_date':rd_end_end_date,
            })
        except:
            print('exception in relations:')
            print(y)
            pass
        for d in da:
            if d['relatedentitytype']=="Place":
                placecheck.add(f"{d['relatedentityurl']}")
                #placerelationscheck.add(str(d['relationtypelabel']) + str(d['relationtypeurl']))
                placerelationscheck.add(f"{d['relationtypelabel']}, {d['relationtypeurl']}")
    return(da, placecheck, placerelationscheck)

""" def sources(sourcy):
    #get sources for one biography article
    if sourcy != None:
        sourceuri = sourcy['url']
        source = requests.get(sourceuri, headers=headers)
        source = source.json()
        pubinfo = source.get('pubinfo')
        #print(sourceuri, pubinfo)
        return(sourceuri, pubinfo) """

def linkentity(apisid, headers):
    while apisid != None:
        personlink = f"{base_url_apis2}entity/{apisid}/?format=json"
        person2_response = requests.get(personlink, headers=headers)
        person2_response = person2_response.json()
        uris = person2_response.get('uris')
        for uri in uris:
            if (uri["uri"].startswith("https://apis-edits.acdh-dev.oeaw.ac.at/")) or (uri["uri"].startswith("https://apis.acdh.oeaw.ac.at/apis/api2/")):
                return(None)
            else:
                return(uri["uri"])


def datareturn (d, re):
    for n in range(len(re)):
        x = re[n]
        apisid = x.get('id')
        profx = (x.get('profession'))
        professions(apisid, profx, dapr)
        externallink = linkentity(apisid, headers)
        personuri = (x.get('url'))
        person_response = requests.get(personuri, headers=headers)
        person_response = person_response.json()
        # sourcx = (x.get('source'))
        # if sourcx != None:
        #     sourceuri, source_pubinfo = sources(sourcx)
        # else:
        #     sourceuri = None
        #     source_pubinfo = None
        if person_response != None:
            datarelations, placecheck, placerelationscheck = relations(person_response, da, placecheck, placerelationscheck, headers)
        try:
            d.append({
                    'apis_id':x.get('id'),
                    'name':x.get('name'),
                    'surname':x.get('first_name'),
                    'url':x.get('url'),
                    #'sourceuri':sourceuri,
                    #'publication_info':source_pubinfo,
                    'references':x.get('references'),
                    'bdate':x.get('start_date'),
                    'ddate':x.get('end_date'),
                    'gender':x.get('gender'),
                    'sameAs':externallink
                })
        except:
            print('exception in persons')
            print(x)
    placedata, placecheck, urls_places_already_fetched = places(placedata, placecheck, urls_places_already_fetched, headers)
    return d,datarelations, placecheck
    #return d,datarelations

def personaggregation(first_page, next_page, headers, data, dapr, da, placecheck, placerelationscheck, placedata, urls_places_already_fetched):
    #while next_page != f"{base_url_apis}entities/person/?limit=50&offset=200":
    #define the point when iterating stops (for test serialization)
    while next_page != None:
        """iterate over JSON API urls"""
        first_response = requests.get(next_page, headers=headers)
        print(f"getting {next_page}")
        re_list=first_response.json()
        response_list = re_list.get('results')
        """get data from REST API in JSON format"""
        next_page = re_list.get('next')
        datageneral, datarelations, placecheck = datareturn(data, response_list, dapr, headers, da, placecheck, placerelationscheck, placedata, urls_places_already_fetched)
        return datageneral, datarelations, placecheck, dapr
        #datageneral, datarelations = datareturn(data, response_list)
    else:
        """stop iterating over JSON API urls"""
        first_response = requests.get(next_page, headers=headers)
        re_list=first_response.json()
        response_list = re_list.get('results')
        datageneral, datarelations, placeset  = datareturn(data, response_list, dapr, headers, da, placecheck, placerelationscheck, placedata, urls_places_already_fetched)
        return datageneral, datarelations, placecheck, dapr

def institutionaggregation(next_page_institution, headers, data, dapr, da, not_included, data_institutions):
    #while next_page_institution != f"{base_url_apis}entities/institution/?limit=50&offset=200":
    #     """iterate over APIS dataset (Institutions)"""
    #     #define the point when iterating over institutions stops 
    while next_page_institution != None:
        """iterate over JSON API urls (institutions)"""
        print(f'getting {next_page_institution}')
        first_response_institution = requests.get(next_page_institution, headers=headers)
        re_list_institution=first_response_institution.json()
        response_list_institution = re_list_institution.get('results')
        """get data from REST API in JSON format"""
        next_page_institution = re_list_institution.get('next')
        data_institutions = datareturn_institution(data_institutions, response_list_institution)
        return data_institutions
    else:
        """stop iterating over JSON API urls"""
        first_response_institution = requests.get(next_page_institution, headers=headers)
        re_list_institution=first_response_institution.json()
        print('Institutions Done')
        response_list_institution = re_list_institution.get('results')
        data_institutions  = datareturn_institution(data_institutions, response_list_institution)
        return data_institutions

def datatodf(datageneral, datarelations, data_institutions, dapr, placedata):
    apis_df=pd.DataFrame(datageneral)
    apis_df.head()
    """dataframe with biography data"""
    relations_df=pd.DataFrame(datarelations)
    relations_df.head()
    """dataframe with relations data"""
    occupations_df=pd.DataFrame(dapr)
    occupations_df.head()
    """dataframe with occupations data"""
    institutions_df=pd.DataFrame(data_institutions)
    institutions_df.head
    places_df=pd.DataFrame(placedata)
    places_df.head()
    apis_df = apis_df.applymap(str)
    relations_df = relations_df.applymap(str)
    occupations_df = occupations_df.applymap(str)
    institutions_df = institutions_df.applymap(str)
    places_df = places_df.applymap(str)
    return apis_df, relations_df, occupations_df, institutions_df, places_df

def parallelproperties(g, apis_id, crmtype, urltype):
    if str(crmtype) == "http://www.cidoc-crm.org/cidoc-crm/E67_Birth":
        g.add((URIRef(f"{idmapis}{urltype}/{apis_id}"), crm.P98_brought_into_life, URIRef(f"{idmapis}personproxy/{apis_id}")))
        return g
    elif str(crmtype) == "http://www.cidoc-crm.org/cidoc-crm/E69_Death":
        g.add((URIRef(f"{idmapis}{urltype}/{apis_id}"), crm.P100_was_death_of, URIRef(f"{idmapis}personproxy/{apis_id}")))
        return g
    else:
        return g

def events(g, relation_id, apis_id, edate, crmtype, urltype, roletype, relationlabel):
    """add events according to BioCRM Model"""
    roletype = str(urllib.parse.quote_plus(roletype))
    print(apis_id, edate, crmtype, urltype, roletype, relationlabel)
    g.add((URIRef(f"idmapis/{urltype}/eventrole/{relation_id}"), bioc.inheres_in, URIRef(f"{idmapis}personproxy/{apis_id}")))
    g.add((URIRef(f"idmapis/{urltype}/eventrole/{relation_id}"), bioc.inheres_in, URIRef(f"{idmapis}personproxy/{apis_id}")))
    g.add((URIRef(f"idmapis/{urltype}/eventrole/{relation_id}"), RDF.type, URIRef(idmrole+roletype)))
    g.add(((URIRef(idmrole+roletype), rdfs.subClassOf, bioc.Event_Role)))
    g.add(((URIRef(idmrole+roletype), RDFS.label, Literal(roletype))))
    #suggestion to add specific event role
    g.add(((URIRef(idmapis+urltype+'/'+relation_id)), bioc.had_participant_in_role, (URIRef(f"idmapis/{urltype}/eventrole/{relation_id}"))))
    #connect event and event role
    g.add(((URIRef(idmapis+urltype+'/'+relation_id)), RDF.type, crmtype))
    #define crm classification
    if isinstance(roletype, str):
        roletype = Literal(roletype)
    g.add((((URIRef((idmapis+'{}/eventrole/{}').format(urltype, relation_id))), RDFS.label, Literal(roletype))))
    g.add((((URIRef(idmapis+urltype+'/'+relation_id))), RDFS.label, Literal(relationlabel)))
    if edate != "None":
        g.add((URIRef(idmapis+urltype+'/'+ apis_id), URIRef(crm + "P4_has_time-span"), URIRef(idmapis+urltype+'/timespan/'+ apis_id)))
        #add time-span to event
        g.add((URIRef(idmapis+urltype+'/timespan/'+apis_id), crm.P82a_begin_of_the_begin, (Literal(edate+'T00:00:00', datatype=XSD.dateTime))))
        #add begin of time-span
        g.add((URIRef(idmapis+urltype+'/timespan/'+apis_id), crm.P82b_end_of_the_end, (Literal(edate+'T23:59:59', datatype=XSD.dateTime))))
        #add end of time-span
    parallelproperties(g, apis_id, crmtype, urltype)
    return g


def persondata_graph(g, apis_df):
    for index, row in apis_df.iterrows():
        #print(index)
        """Create RDF Triples, according to IDM ontology."""
        #g.add(((URIRef(ex+'person/'+str(index))), RDF.type, idmcore.Provided_Person))
        #print(row['apis_id'], index)
        #Person Entity in shared InTaVia named graph
        #sg.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), idmcore.person_proxy_for, URIRef(ex+'person/'+str(index))))
        #connect Person Proxy and person in named graph
        g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), RDF.type, idmcore.Person_Proxy))
        g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), RDF.type, crm.E21_Person))
        g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), RDFS.label, (Literal(row['surname']+'  '+row['name']))))
        #define Person Proxy
        g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), owl.sameAs, (URIRef(row['url']))))
        #define that individual in APIS named graph and APIS entity are the same
        if row['sameAs'] != "None":
            g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), owl.sameAs, (URIRef(row['sameAs']))))
            #define that individual in APIS named graph and entity from the external source are the same
        g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), crm.P1_is_identified_by, URIRef(idmapis+'appellation/'+'1/'+row['apis_id'])))
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
        g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), crm.P1_is_identified_by, URIRef(idmapis+'identifier/'+row['apis_id'])))
        #add APIS ID as identifier
        g.add((URIRef(idmapis+'identifier/'+row['apis_id']), RDF.type, crm.E42_Identifier))
        #define APIS ID as E42 Identifier    # reactivate when server problem is solved!
        g.add((URIRef(idmapis+'identifier/'+row['apis_id']), rdfs.label, (Literal(row['apis_id']))))
        #add label for APIS ID
        g = events(g, row['apis_id'], row['apis_id'], row['bdate'], crm.E67_Birth, 'birthevent', Literal('born_person'), (Literal("Birth of "+row['surname']+'  '+row['name'])))
        #add birth event according to APIS
        g = events(g, row['apis_id'], row['apis_id'], row['ddate'], crm.E69_Death, 'deathevent', Literal('deceased_person'), (Literal("Death of "+row['surname']+'  '+row['name'])))
        #add death event according to APIS
        g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), bioc.has_gender, URIRef(bioc+row['gender'])))
        #add gender to person
        g.add((URIRef(bioc+row['gender']), RDF.type, bioc.Gender))
    return g
        #define gender as idmcore gender
        # g.add((URIRef(idmbibl+'source'+row['apis_id']), crm.P70_documents, URIRef(f"{idmapis}personproxy/{row['apis_id']}")))
        # g.add((URIRef(idmbibl+'source'+row['apis_id']), RDF.type, crm.E31_Document))
        # g.add((URIRef(idmbibl+'source'+row['apis_id']), RDF.type, bf.Work))
        # sourceid = (row['sourceuri'])[-4:-1]
        # g.add((URIRef(idmbibl+'source'+row['apis_id']), owl.sameAs, URIRef(row['sourceuri'])))
        # g.add((URIRef(idmbibl), bf.hasPart, URIRef(idmbibl+'source'+row['apis_id'])))

# for col in relations_df.columns:
#     if col.endswith("id"):
#         relations_df[col] = relations_df[col].astype(float).fillna(0).astype(int).astype(str)
#     else:
#         relations_df[col] = relations_df[col].astype(str)

while next_page != f"{base_url_apis}entities/person/?limit=50&offset=150":
#define the point when iterating stops (for test serialization)
#while next_page != None:
    """iterate over JSON API urls"""
    first_response = requests.get(next_page, headers=headers)
    print(f"getting {next_page}")
    re_list=first_response.json()
    response_list = re_list.get('results')
    """get data from REST API in JSON format"""
    next_page = re_list.get('next')
    #datageneral, datarelations, placeset = datareturn(data, response_list)
    datageneral, datarelations = datareturn(data, response_list)
else:
    """stop iterating over JSON API urls"""
    re_list=first_response.json()
    response_list = re_list.get('results')
    #datageneral, datarelations, placeset  = datareturn(data, response_list)
    datageneral, datarelations = datareturn(data, response_list)


def relationdata_graph(g, relations_df, familyrelationidlist, not_included, places_df):
    for index, row in relations_df.iterrows():
        rtype = row['relationtypeurl']
        relation_id = str(row['relationid'])
        relationtype_id = str(row['relationtypeid'])
        if row['relationtype_parentid'] != "nan":
                # if the relationtype has a superclass, it is added here
                relationtype_parentid = row['relationtype_parentid'][:-2]
        else:
            relationtype_parentid = 'nan'
        if relationtype_id == '595':
        #if rtype == f"{base_url_apis}vocabularies/personplacerelation/595/":
            #define serialization for "person born in place relations"
            g.add((URIRef(idmapis+'birthevent/'+relation_id), crm.P7_took_place_at, URIRef(idmapis+'place/'+row['relatedentityid'])))
            places_graph(g, places_df)
            #return g
            print (f" relation 595 serialized for: {row['apis_id']}")
        elif relationtype_id == '596':
            #define serialization for "person died in place relations"
            g.add((URIRef(idmapis+'deathevent/'+relation_id), crm.P7_took_place_at, URIRef(idmapis+'place/'+row['relatedentityid'])))
            places_graph(g, places_df)
            #return g
            print (f" relation 596 serialized for: {row['apis_id']}")
        elif relationtype_id in familyrelationidlist:
            """serializes family relations"""
            g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), bioc.has_family_relation, URIRef(f"{idmapis}familyrelation/{relation_id}")))
            # Person has this specific family relation
            g.add((URIRef(idmapis+'familyrelation/'+row['relationid']), bioc.inheres_in, (URIRef(f"{idmapis}personproxy/{row['relatedentityid']}"))))
            # the other person that has this family relation is defined
            g.add((URIRef(idmapis+'familyrelation/'+relation_id), RDFS.label, Literal(row['relationlabel'])))
            # the specific familyrelation gets a label
            g.add((URIRef(idmapis+'familyrelation/'+relation_id), RDF.type, URIRef(idmrelations+relationtype_id)))
            # defines type of familyrelation
            g.add((URIRef(idmrelations+relationtype_id), RDFS.label, Literal(row['relationtypelabel'])))
            g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(bioc.Family_Relationship_Role)))
            #print(row['relationurl'], row['relationtypeurl'])
            if relationtype_parentid != 'nan':
                g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(idmrelations+relationtype_parentid)))
                g.add((URIRef(idmrelations+relationtype_parentid), RDFS.subClassOf, URIRef(bioc.Family_Relationship_Role)))
            print (f" familyrelation serialized for: {row['apis_id']}")
            #return g
            #reltype= str(urllib.parse.quote_plus(row['relationtypelabel']))
            #g.add((URIRef(idmapis+'personproxy/'+row['relatedentityid']), idmcore.person_proxy_for, URIRef(intavia_shared+'person/'+str(index+50000))))
        # elif re.search(r'vocabularies/personplacerelation/.*', rtype):
        elif "personplacerelation" in rtype:
            g = events(g, relation_id, str(row['apis_id']), 'None', crm.E5_Event, 'event', str(row['relationtypelabel']), str(row['relationlabel']))
            g.add((URIRef(idmapis+'event/'+relation_id), crm.P7_took_place_at, URIRef(idmapis+'place/'+row['relatedentityid'])))
            places_graph(g, places_df)
            print (f" personplacerelation serialized for: {row['apis_id']}")
            #return g
        #elif re.search(r'vocabularies/personinstitutionrelation/.*' , rtype):
        elif "/personinstitutionrelation/" in rtype:
            #connect personproxy and institutions with grouprelationship
            g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), bioc.has_group_relation, URIRef(idmapis+'grouprelation/'+relation_id)))
            # Person has a specific group relation
            g.add((URIRef(idmapis+'grouprelation/'+relation_id), RDF.type, URIRef(idmrelations+relationtype_id)))
            #define type of grouprelation
            if relationtype_parentid != "nan":
                # if the relationtype has a superclass, it is added here
                g.add((URIRef(idmrelations+relationtype_id), rdfs.subClassOf, URIRef(idmrelations+relationtype_parentid)))
            g.add((URIRef(idmapis+'grouprelation/'+relation_id), rdfs.label, Literal(row['relationtypelabel'])))
            # add label to relationtype
            g.add((URIRef(idmapis+'grouprelation/'+relation_id), bioc.inheres_in, URIRef(idmapis+'groupproxy/'+row['relatedentityid'])))
            #group which is part of this relation
            g.add((URIRef(idmapis+'career/'+row['relationid']), RDF.type, idmcore.Career))
            # add career event of type idmcore:career
            g.add((idmcore.Career, rdfs.subClassOf, crm.E5_Event,))
            g.add((URIRef(idmapis+'career/'+row['relationid']), rdfs.label, Literal(row['relationlabel'])))
            # label for career event
            g.add((URIRef(idmapis+'career/'+row['relationid']), bioc.had_participant_in_role, URIRef(idmapis+'personrole/'+row['relationid']+'/'+row['apis_id'])))
            # role of participating person in the career event
            g.add((URIRef(idmapis+'personrole/'+row['relationid']+'/'+row['apis_id']), bioc.inheres_in, URIRef(f"{idmapis}personproxy/{row['apis_id']}")))
            # person which inheres this role
            g.add((URIRef(idmapis+'career/'+row['relationid']), bioc.had_participant_in_role, URIRef(idmapis+'grouprole/'+row['relationid']+'/'+row['relatedentityid'])))
            # role of institution/ group in the career event
            g.add((URIRef(idmapis+'grouprole/'+row['relationid']+'/'+row['relatedentityid']), RDF.type, bioc.Group_Relationship_Role))
            g.add((URIRef(idmapis+'grouprole/'+row['relationid']+'/'+row['relatedentityid']), bioc.inheres_in, URIRef(idmapis+'groupproxy/'+row['relatedentityid'])))
            # role which inheres the institution/ group
            g.add((URIRef(idmapis+'career/'+row['relationid']), URIRef(crm + "P4_has_time-span"), URIRef(idmapis+'career/timespan/'+row['relationid'])))
            print (f" personinstitutionrelation serialized for: {row['apis_id']}")
            if (row['rd_start_date'] != 'None') and (row['rd_end_date'] != 'None'):
                g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), crm.P82a_begin_of_the_begin, (Literal(row['rd_start_date']+'T00:00:00', datatype=XSD.dateTime))))
                g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), crm.P82b_end_of_the_end, (Literal(row['rd_end_date']+'T23:59:59', datatype=XSD.dateTime))))
                g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), rdfs.label, Literal(row['rd_start_date_written'])+' - '+ row['rd_end_date_written']))
            elif ((row['rd_start_date'] != 'None') and (row['rd_end_date']=='None')):
                g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), crm.P82a_begin_of_the_begin, (Literal(row['rd_start_date']+'T00:00:00', datatype=XSD.dateTime))))
                g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), rdfs.label, Literal(row['rd_start_date_written'])))
            elif ((row['rd_start_date'] == 'None') and (row['rd_end_date']!='None')):
                g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), crm.P82b_end_of_the_end, (Literal(row['rd_end_date']+'T23:59:59', datatype=XSD.dateTime))))
                g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), rdfs.label, Literal('time-span end:' + row['rd_end_date_written'])))
                #return g
            else:
                continue
            # if re.search('-01-01', row['institution_start_date']) != None:
            #     start_date_year = (row['institution_start_date'])[0:4]
                #g.add((URIRef(idmapis+'groupstart/timespan/'+row['institution_id']), crm.P82b_end_of_the_end, (Literal(start_date_year+'-12-31'+'T23:59:59', datatype=XSD.dateTime))))
            # else:
            #     if row['rd_end_date'] != "None":
            #         g.add((URIRef(idmapis+'career/timespan/'+row['relationid']), crm.P82b_end_of_the_end, (Literal(row['rd_end_date']+'T23:59:59', datatype=XSD.dateTime))))
        #elif re.search(r'vocabularies/personpersonrelation/.*' , rtype):
        elif "personpersonrelation" in rtype:
            g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), bioc.has_person_relation, URIRef(idmapis+'personrelation/' +row['relationid'])))
            g.add((URIRef(idmapis+'personrelation/'+row['relationid']), RDF.type, URIRef(idmrelations+relationtype_id)))
            g.add((URIRef(idmapis+'personrelation/'+row['relationid']), RDFS.label, Literal(row['relationtypelabel'])))
            g.add(((URIRef(idmrelations+relationtype_id)), RDFS.subClassOf, (URIRef(bioc.Person_Relationship_Role))))
            g.add((URIRef(idmapis+'personproxy/'+row['relatedentityid']), bioc.inheres_in, URIRef(idmapis+'personrelation/'+row['relationid'])))
            #connect personproxy and institutions with grouprelationship
            g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), bioc.has_group_relation, URIRef(idmapis+'grouprelation/'+row['relationid'])))
            g.add((URIRef(idmapis+'grouprelation/'+relation_id), RDF.type, URIRef(idmrelations+relationtype_id)))
            if relationtype_parentid != "nan":
                #adds parent class for relationship types
                g.add((URIRef(idmrelations+relationtype_id), RDFS.subClassOf, URIRef(idmrelations+relationtype_parentid)))
                g.add((URIRef(idmrelations+relationtype_parentid), RDFS.subClassOf, URIRef(bioc.Group_Relationship_Role)))
            #defines relationship as idm group relationship
            g.add((URIRef(idmapis+'grouprelation/'+relation_id), RDFS.label, Literal(row['relationtypelabel'])))
            g.add((URIRef(idmapis+'groupproxy/'+row['relatedentityid']), bioc.inheres_in, URIRef(idmapis+'grouprelation/'+relation_id)))
            print (f" personpersonrelation serialized for: {row['apis_id']}")
            #return g
            #group which is part of this relation
        # elif re.search(r'vocabularies/personeventrelation/.*', rtype):
        elif "personeventrelation" in rtype:
            g = events(g, relation_id, str(row['apis_id']), 'None', crm.E5_Event, 'event', str(row['relationtypelabel']), str(row['relationlabel']))
            print (f" personeventsrelation serialized for: {row['apis_id']}")
            #add general event according to APIS
            return g
        else:
            not_included.append(row['relationid']+row['relationtypeurl'])
            print(f"not included in relations: {row['relationid']} {row['relationtypeurl']}")
            return g



def institutiondata_graph(g, institutions_df):
    for index, row in institutions_df.iterrows():
        #g.add(((URIRef(ex+'group/'+str(index))), RDF.type, idmcore.Provided_Group))
        #g.add((URIRef(idmapis+'groupproxy/'+row['institution_id']), idmcore.group_proxy_for, URIRef(ex+'group/'+str(index))))
        #connect Group Proxy and person in named graphbgn:BioDes
        g.add((URIRef(idmapis+'groupproxy/'+row['institution_id']), RDF.type, crm.E74_Group))
        #defines group class
        g.add((URIRef(idmapis+'groupproxy/'+row['institution_id']), owl.sameAs, URIRef(row['institution_url'])))
        #defines group as the same group in the APIS dataset
        g.add((URIRef(idmapis+'groupproxy/'+row['institution_id']), crm.P1_is_identified_by, URIRef(idmapis+'groupappellation/'+row['institution_id'])))
        g.add((URIRef(idmapis+'groupappellation/'+row['institution_id']), rdfs.label, Literal(row['institution_name'])))
        g.add((URIRef(idmapis+'groupappellation/'+row['institution_id']), RDF.type, crm.E33_E41_Linguistic_Appellation))
        #add group appellation and define it as linguistic appellation
        if row['institution_start_date'] != "None":
            #print(row['institution_name'], ':', row['institution_start_date'], row['institution_end_date'], row['institution_start_date_written'], row['institution_end_date_written'])
            g.add((URIRef(idmapis+'groupstart/'+row['institution_id']), RDF.type, crm.E63_Beginning_of_Existence))
            g.add((URIRef(idmapis+'groupstart/'+row['institution_id']), crm.P92_brought_into_existence, URIRef(idmapis+'groupproxy/'+row['institution_id'])))
            g.add((URIRef(idmapis+'groupstart/'+row['institution_id']), URIRef(crm + "P4_has_time-span"), URIRef(idmapis+'groupstart/timespan/'+row['institution_id'])))
            g.add((URIRef(idmapis+'groupstart/timespan/'+row['institution_id']), crm.P82a_begin_of_the_begin, (Literal(row['institution_start_date']+'T00:00:00', datatype=XSD.dateTime))))
            if re.search('-01-01', row['institution_start_date']) != None:
                start_date_year = (row['institution_start_date'])[0:4]
                g.add((URIRef(idmapis+'groupstart/timespan/'+row['institution_id']), crm.P82b_end_of_the_end, (Literal(start_date_year+'-12-31'+'T23:59:59'))))
            else:
                g.add((URIRef(idmapis+'groupstart/timespan/'+row['institution_id']), crm.P82b_end_of_the_end, (Literal(row['institution_start_date']+'T23:59:59'))))
        #Beginning of existence of this group
        if row['institution_end_date'] != "None":
            g.add((URIRef(idmapis+'groupend/'+row['institution_id']), RDF.type, crm.E64_End_of_Existence))
            g.add((URIRef(idmapis+'groupend/'+row['institution_id']), crm.P93_took_out_of_existence, URIRef(idmapis+'groupproxy/'+row['institution_id'])))
            g.add((URIRef(idmapis+'groupend/'+row['institution_id']), URIRef(crm + "P4_has_time-span"), URIRef(idmapis+'groupend/timespan/'+row['institution_id'])))
            g.add((URIRef(idmapis+'groupend/timespan/'+row['institution_id']), crm.P82a_begin_of_the_begin, (Literal(row['institution_end_date']+'T00:00:00', datatype=XSD.dateTime))))
            if re.search('-01-01', row['institution_end_date']) != None:
                end_date_year = (row['institution_end_date'])[0:4]
                g.add((URIRef(idmapis+'groupend/timespan/'+row['institution_id']), crm.P82b_end_of_the_end, (Literal(end_date_year+'-12-31'+'T23:59:59', datatype=XSD.dateTime))))
            else:
                g.add((URIRef(idmapis+'groupend/timespan/'+row['institution_id']), crm.P82b_end_of_the_end, (Literal(row['institution_end_date']+'T23:59:59', datatype=XSD.dateTime))))
        #End of Existence for this group

def occupations_graph(g, occupations_df):
    for index, row in occupations_df.iterrows():
        g.add((URIRef(f"{idmapis}personproxy/{row['apis_id']}"), bioc.has_occupation, URIRef(idmapis+'occupation/'+row['professionid'])))
        g.add((URIRef(idmapis+'occupation/'+row['professionid']), rdfs.label, Literal(row['professionlabel'])))
        if row['professionparentid'] != "nan":
            professionparentid = row['professionparentid'][:-2]
            g.add((URIRef(idmapis+'occupation/'+row['professionid']), rdfs.subClassOf, URIRef(idmapis+'occupation/'+professionparentid)))
            g.add((URIRef(idmapis+'occupation/'+professionparentid), rdfs.subClassOf, bioc.Occupation))
        else:
            g.add((URIRef(idmapis+'occupation/'+row['professionid']), rdfs.subClassOf, bioc.Occupation))



def places_graph(g, places_df):
    for index, row in places_df.iterrows():
        nameforuri = str(urllib.parse.quote_plus(row['place_name']))
        g.add((URIRef(idmapis+'place/'+row['place_id']), RDF.type, crm.E53_Place))
        #define place as Cidoc E53 Place
        g.add((URIRef(idmapis+'place/'+row['place_id']), crm.P1_is_identified_by, (URIRef(idmapis+'placeappellation/'+row['place_id']+'/'+nameforuri))))
        #add appellation to place
        g.add(((URIRef(idmapis+'placeappellation/'+row['place_id']+'/'+nameforuri)), RDF.type, crm.E33_E41_Linguistic_Appellation))
        #define appellation as linguistic appellation 
        g.add(((URIRef(idmapis+'placeappellation/'+row['place_id']+'/'+nameforuri)), RDFS.label, Literal(row['place_name'])))
        #add label to appellation
        g.add((URIRef(idmapis+'place/'+row['place_id']), crm.P1_is_identified_by, URIRef(idmapis+'placeidentifier/'+row['place_id'])))
        #add APIS Identifier as Identifier
        g.add((URIRef(idmapis+'placeidentifier/'+row['place_id']), RDF.type, crm.E_42_Identifier))
        #define APIS Identifier as E42 Identifier (could add a class APIS-Identifier or model a Identifier Assignment Event)
        g.add((URIRef(idmapis+'placeidentifier/'+row['place_id']), RDFS.label, Literal(row['place_id'])))
        #add label to APIS Identifier
        g.add((URIRef(idmapis+'place/'+row['place_id']), owl.sameAs, (URIRef(row['apis_place_url']))))
        #define that individual in APIS named graph and APIS entity are the same
        if row['place_lat'] != "nan":
            g.add((URIRef(idmapis+'place/'+row['place_id']), crm.P168_place_is_defined_by, URIRef(idmapis+'spaceprimitive/'+row['place_id'])))
            g.add((URIRef(idmapis+'spaceprimitive/'+row['place_id']), rdf.type, crm.E94_Space_Primitive))
            g.add((URIRef(idmapis+'spaceprimitive/'+row['place_id']), crm.P168_place_is_defined_by, Literal(("POINT " + row['place_lat']+' '+row['place_lng']), datatype=geo.wktLiteral)))
            if row['georef'] != "None":
                g.add((URIRef(idmapis+'place/'+row['place_id']), owl.sameAs, (URIRef(row['georef']))))
            #define that individual in APIS named graph and APIS entity are the same
        # suggestion for serialization of space primitives according to ISO 6709, to be discussed
        # more place details will be added (references, source, place start date, place end date, relations to other places(?))


def serializeto_ttl(g):
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
    g.bind("geo", geo)
    #Bind namespaces to prefixes for readable output
    exapis = g.serialize(destination=f'apisdata_c_{datetime.datetime.now().strftime("%d-%m-%Y")}.ttl', format='turtle')


def main():
    data=[]
    data_institutions=[]
    da=[]
    dapr= []
    mostprofessions=[]
    placedata=[]
    placecheck=set()
    placerelationscheck=set()
    headers = {"accept": "application/json"}
    not_included=[]
    urls_places_already_fetched = []
    first_page = f"{base_url_apis}entities/person/?limit=50&offset=0&collection=86"
    """initial json url to get apis data"""
    first_response = requests.get(first_page, headers=headers)
    """get data for this URL from REST API"""
    re_list = first_response.json()
    """get data from REST API in JSON format"""
    previous_page = re_list.get('previous')
    """previous json url to get apis data"""
    next_page = re_list.get('next')
    """following REST API url to get apis data"""
    response_list = re_list.get('results')
    """get list with all datasets from the url as dictionaries"""
    datageneral, datarelations, placecheck, dapr = personaggregation(first_page, next_page, headers, data, dapr, da, placecheck, placerelationscheck, placedata, urls_places_already_fetched)
    first_page_institution = f"{base_url_apis}entities/institution/?limit=50&offset=0"
    first_response_institution = requests.get(first_page_institution, headers=headers)
    """get data for this URL from REST API"""
    re_list_institution=first_response_institution.json()
    """get data from REST API in JSON format"""
    response_list_institution = re_list_institution.get('results')
    """get list with all datasets from the url as dictionaries"""
    next_page_institution = re_list.get('next')
    datainstitution = institutionaggregation(next_page_institution, headers, data, dapr, da, not_included, data_institutions)
    apis_df, relations_df, occupations_df, institutions_df, places_df = datatodf(datageneral, datarelations, data_institutions, dapr, placedata)
    g = Graph()
    persondata_graph(g, apis_df)
    relationdata_graph(g, relations_df, familyrelationidlist, not_included, places_df)
    institutiondata_graph(g, institutions_df)
    occupations_graph(g, occupations_df)
    places_graph(g, places_df)
    serializeto_ttl(g)

if __name__=="__main__":
    main()
