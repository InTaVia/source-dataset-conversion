import asyncio
import requests
import plac
from rdflib import Graph, Literal, RDF, Namespace, URIRef
from rdflib.namespace import RDFS, XSD
import logging

BASE_URL_API = 'http://localhost:8000/apis/api'
BASE_URI_SERIALIZATION = 'https://apis.acdh.oeaw.ac.at/'

crm = Namespace('http://www.cidoc-crm.org/cidoc-crm/')
"""Defines namespace for CIDOC CRM."""
ex = Namespace('http://www.intavia.eu/')
"""Defines namespace for own ontology."""
idmcore = Namespace('http://www.intavia.eu/idm-core/')
"""Defines namespace for own ontology."""
idmrole = Namespace('http://www.intavia.eu/idm-role/')
"""Defines namespace for role ontology."""
idmapis = Namespace('http://www.intavia.eu/apis/')
"""Namespace for InTaVia named graph"""
idmbibl = Namespace('http://www.intavia.eu/idm-bibl/')
"""Namespace for bibliographic named graph"""
idmrelations = Namespace('http://www.intavia.eu/idm-relations')
"""Defines namespace for relation ontology."""
intavia_shared = Namespace('http://www.intavia.eu/shared-entities')
"""Defines namespace for relation ontology."""
ore = Namespace('http://www.openarchives.org/ore/terms/')
"""Defines namespace for schema.org vocabulary."""
edm = Namespace('http://www.europeana.eu/schemas/edm/')
"""Defines namespace for Europeana data model vocabulary."""
ore = Namespace('http://www.openarchives.org/ore/terms/')
"""Defines namespace for Europeana data model vocabulary."""
owl = Namespace('http://www.w3.org/2002/7/owl#')
"""Defines namespace for Europeana data model vocabulary."""
rdf = Namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#')
"""Defines namespace for Europeana data model vocabulary."""
xml = Namespace('http://www.w3.org/XML/1998/namespace')
"""Defines namespace for Europeana data model vocabulary."""
xsd = Namespace('http://www.w3.org/2001/XMLSchema#')
"""Defines namespace for Europeana data model vocabulary."""
bioc = Namespace('http://ldf.fi/schema/bioc/')
"""Defines namespace for Europeana data model vocabulary."""
rdfs = Namespace('http://www.w3.org/2000/01/rdf-schema#')
"""Defines namespace for Europeana data model vocabulary."""
apis = Namespace('https://www.apis.acdh.oeaw.ac.at/')
"""Defines namespace for APIS database."""
owl = Namespace('http://www.w3.org/2002/07/owl#')
"""Defines OWL namespace."""
bf = Namespace('http://id.loc.gov/ontologies/bibframe/')
"""Defines bibframe namespace."""
geo = Namespace("http://www.opengis.net/ont/geosparql#")


async def render_person_relation(pers_uri, rel, g):
    #connect personproxy and institutions with grouprelationship
    n_rel_type = URIRef(f"{idmapis}grouprelation/{rel['relation_type']['id']}")
    g.add((pers_uri, bioc.has_group_relation, n_rel_type))
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

async def render_person(person, g):
    """renders person object as RDF graph

    Args:
        person (_type_): _description_
        g (_type_): _description_
    """
    if (URIRef(f"{BASE_URI_SERIALIZATION}personproxy/{person['id']}"), None, None) in g:
        return g
    g.add((URIRef(
        f"{BASE_URI_SERIALIZATION}personproxy/{person['id']}"), RDF.type, crm.E21_Person))
    g.add((URIRef(
        f"{BASE_URI_SERIALIZATION}personproxy/{person['id']}"), RDF.type, idmcore.Person_Proxy))
    g.add((URIRef(f"{BASE_URI_SERIALIZATION}personproxy/{person['id']}"), RDFS.label, Literal(
        f"{person['first_name']} {person['name']}")))
    #define that individual in APIS named graph and APIS entity are the same
    g.add((URIRef(f"{BASE_URI_SERIALIZATION}personproxy/{person['id']}"), owl.sameAs, URIRef(person['url'].split("?")[0])))
    #add sameAs
    for uri in person['sameAs']:
        g.add((URIRef(f"{BASE_URI_SERIALIZATION}personproxy/{person['id']}"), owl.sameAs, URIRef(uri))) 
    person_rel = await get_person_relations(person['id'])
    tasks = []
    for rel in person_rel:
        tasks.append(asyncio.create_task(render_person_relation(rel, g)))
    await asyncio.gather(*tasks)
    return g


async def render_organization(organization, g):
    """renders organization object as RDF graph

    Args:
        organization (_type_): _description_
        g (_type_): _description_
    """


async def render_event(event, g):
    """renders event object as RDF graph

    Args:
        event (_type_): _description_
        g (_type_): _description_
    """


async def render_place(place, g):
    """renders place object as RDF graph

    Args:
        place (_type_): _description_
        g (_type_): _description_
    """


async def get_persons(filter_params, g):
    """gets persons from API

    Args:
        filter_params (_type_): _description_
    """
    if "limit" not in filter_params:
        filter_params["limit"] = 100
    if "format" not in filter_params:
        filter_params["format"] = "json"
    res = requests.get(BASE_URL_API + "/entities/person", params=filter_params)
    if res.status_code != 200:
        raise Exception("Error getting persons")
    res = res.json()
    tasks = []
    while res:
        logging.info(f"working on offset {res['offset']}")
        for person in res["results"]:
            tasks.append(asyncio.create_task(render_person(person, g)))
        if "offset" in res:
            if int(res["offset"]) > 500:
                break
        if "next" in res:
            res = requests.get(res["next"]).json()
        else:
            break
    await asyncio.gather(*tasks)


async def get_organizations(organization_id):
    """gets organization object from API

    Args:
        organization_id (_type_): _description_
    """


async def get_event(event_id):
    """gets event object from API

    Args:
        event_id (_type_): _description_
    """


async def get_place(place_id):
    """gets place object from API

    Args:
        place_id (_type_): _description_
    """


async def get_person_relations(person_id):
    """gets person relations from API

    Args:
        person_id (_type_): _description_
    """


async def get_organization_relations(organization_id):
    """gets organization relations from API

    Args:
        organization_id (_type_): _description_
    """


async def get_place_relations(place_id):
    """gets place relations from API

    Args:
        place_id (_type_): _description_
    """


@plac.opt('additional_filters', type=str, help='Additional filters for limiting the API call')
@plac.flg("use_cache", "Whether to use cached data from the API")
async def main(use_cache: bool = False, additional_filters: str = None):
    """main function
    """
    logging.basicConfig(level=logging.DEBUG)
    if use_cache:
        pass
    else:
        g = Graph()
        g.bind("rdf", RDF)
        g.bind("rdfs", RDFS)
        g.bind("xsd", XSD)
        g.bind("apis", apis)
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
        await get_persons({"collection": 86}, g)
        g.serialize(destination="persons.rdf", format="turtle")

if __name__ == "__main__":
    asyncio.run(main())
