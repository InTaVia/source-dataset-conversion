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


async def render_personinstitution_relation(pers_uri, rel, g):
    # connect personproxy and institutions with grouprelationship
    n_rel_type = URIRef(f"{idmapis}grouprelation/{rel['relation_type']['id']}")
    g.add((pers_uri, bioc.has_group_relation, n_rel_type))
    # Person has a specific group relation
    g.add((n_rel_type, RDF.type, URIRef(
        f"{idmrelations}/{rel['relation_type']['id']}")))
    # define type of grouprelation
    if rel['relation_type']['parent_id'] is not None:
        # if the relationtype has a superclass, it is added here
        g.add((URIRef(f"{idmrelations}/{rel['relation_type']['id']}"),
              rdfs.subClassOf, URIRef(f"{idmrelations}{rel['relation_type']['parent_id']}")))
    g.add((n_rel_type, rdfs.label, Literal(rel['relation_type']['label'])))
    # add label to relationtype
    g.add((n_rel_type, bioc.inheres_in, URIRef(
        f"{idmapis}groupproxy/{rel['related_institution']['id']}")))
    # group which is part of this relation
    g.add((URIRef(f"{idmapis}career/{rel['id']}"), RDF.type, idmcore.Career))
    # add career event of type idmcore:career
    g.add((idmcore.Career, rdfs.subClassOf, crm.E5_Event,))
    g.add((URIRef(f"{idmapis}career/{rel['id']}"), rdfs.label, Literal(
        f"{rel['related_person']['label']} {rel['relation_type']['label']} {rel['related_institution']['label']}")))
    # label for career event
    g.add((URIRef(f"{idmapis}career/{rel['id']}"), bioc.had_participant_in_role, URIRef(
        f"{idmapis}personrole/{rel['id']}/{rel['related_person']['id']}")))
    # role of participating person in the career event
    g.add((URIRef(f"{idmapis}personrole/{rel['id']}/{rel['related_person']['id']}"),
          bioc.inheres_in, URIRef(f"{idmapis}personproxy/{rel['related_person']['id']}")))
    # person which inheres this role
    g.add((URIRef(f"{idmapis}career/{rel['id']}"), bioc.had_participant_in_role, URIRef(
        f"{idmapis}grouprole/{rel['id']}/{rel['related_institution']['id']}")))
    # role of institution/ group in the career event
    g.add((URIRef(
        f"{idmapis}grouprole/{rel['id']}/{rel['related_institution']['id']}"), RDF.type, bioc.Group_Relationship_Role))
    g.add((URIRef(f"{idmapis}grouprole/{rel['id']}/{rel['related_institution']['id']}"),
          bioc.inheres_in, URIRef(f"{idmapis}groupproxy/{rel['related_institution']['id']}")))
    # role which inheres the institution/ group
    g.add((URIRef(f"{idmapis}career/{rel['id']}"), URIRef(
        f"{crm}P4_has_time-span"), URIRef(f"{idmapis}career/timespan/{rel['id']}")))
    logging.info(
        f" personinstitutionrelation serialized for: {rel['related_person']['id']}")
    if (rel['start_date'] is not None) and (rel['end_date'] is not None):
        g.add((URIRef(f"{idmapis}career/timespan/{rel['id']}"), crm.P82a_begin_of_the_begin, (Literal(
            rel['start_date']+'T00:00:00', datatype=XSD.dateTime))))
        g.add((URIRef(f"{idmapis}career/timespan/{rel['id']}"), crm.P82b_end_of_the_end, (Literal(
            rel['end_date']+'T23:59:59', datatype=XSD.dateTime))))
        g.add((URIRef(f"{idmapis}career/timespan/{rel['id']}"), rdfs.label, Literal(
            rel['start_date_written'])+' - ' + rel['end_date_written']))
    elif ((rel['start_date'] is not None) and (rel['end_date'] is not None)):
        g.add((URIRef(f"{idmapis}career/timespan/{rel['id']}"), crm.P82a_begin_of_the_begin, (Literal(
            rel['start_date']+'T00:00:00', datatype=XSD.dateTime))))
        g.add((URIRef(
            f"{idmapis}career/timespan/{rel['id']}"), rdfs.label, Literal(rel['start_date_written'])))
    elif ((rel['start_date']is not None) and (rel['end_date'] is not None)):
        g.add((URIRef(f"{idmapis}career/timespan/{rel['id']}"), crm.P82b_end_of_the_end, (Literal(
            rel['end_date']+'T23:59:59', datatype=XSD.dateTime))))
        g.add((URIRef(f"{idmapis}career/timespan/{rel['id']}"), rdfs.label, Literal('time-span end:' + rel['end_date_written'])))
    return g


async def render_person(person, g):
    """renders person object as RDF graph

    Args:
        person (_type_): _description_
        g (_type_): _description_
    """
    pers_uri = URIRef(f"{BASE_URI_SERIALIZATION}personproxy/{person['id']}")
    if (pers_uri, None, None) in g:
        return g
    g.add((pers_uri, RDF.type, crm.E21_Person))
    g.add((pers_uri, RDF.type, idmcore.Person_Proxy))
    g.add((pers_uri, RDFS.label, Literal(
        f"{person['first_name']} {person['name']}")))
    # define that individual in APIS named graph and APIS entity are the same
    g.add((pers_uri, owl.sameAs, URIRef(person['url'].split("?")[0])))
    # add sameAs
    for uri in person['sameAs']:
        g.add((pers_uri, owl.sameAs, URIRef(uri)))
    person_rel = await get_person_relations(person['id'], kinds=['personinstitution'])
    tasks = []
    for rel_type, rel_list in person_rel.items():
        if rel_type == 'personinstitution':
            for rel in rel_list:
                tasks.append(asyncio.create_task(
                    render_personinstitution_relation(pers_uri, rel, g)))
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
            if int(res["offset"]) > 10:
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


async def get_person_relations(person_id, kinds=["personplace", "personinstitution", "personevent", "personwork"]):
    """gets person relations from API

    Args:
        person_id (_type_): _description_
    """
    res_full = {}   # TODO: add looping through pagination
    query_params = {"related_person": person_id}
    for kind in kinds:
        res = requests.get(f"{BASE_URL_API}/relations/{kind}/", params=query_params)
        if res.status_code != 200:
            logging.error(f"Error getting {kind} for person {person_id}")
            continue
        res = res.json()
        res_full[kind] = res["results"]
    return res_full


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
