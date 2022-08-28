import asyncio
from copy import deepcopy
import re
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


async def render_personplace_relation(pers_uri, rel, g):
    """renders personplace relation as RDF graph

    Args:
        pers_uri (_type_): _description_
        rel (_type_): _description_
        g (_type_): _description_
    """
    #prepare nodes
    place_uri = URIRef(f"{idmapis}place/{rel['related_place']['id']}")
    if rel['relation_type']['id'] == 595:
        #define serialization for "person born in place relations"
        if (place_uri, None, None) not in g:
            await render_place(rel['related_place']['id'], g)
        g.add((URIRef(f"{idmapis}birthevent/{rel['id']}"), crm.P7_took_place_at, place_uri))
    elif rel['relation_type']['id'] == 596:
        #define serialization for "person born in place relations"
        if (place_uri, None, None) not in g:
            await render_place(rel['related_place']['id'], g)
        g.add((URIRef(f"{idmapis}deathevent/{rel['id']}"), crm.P7_took_place_at, place_uri))
    else:
        event_uri = URIRef(f"{idmapis}event/{rel['id']}")
        if (event_uri, None, None) not in g:
            await render_event(rel, g)
        g.add((event_uri, crm.P7_took_place_at, place_uri))
    return g

async def render_personperson_relation(pers_uri, rel, g):
    """renders personperson relation as RDF graph

    Args:
        pers_uri (_type_): _description_
        rel (_type_): _description_
        g (_type_): _description_
    """
    #prepare nodes
    n_rel_type = URIRef(f"{idmapis}personrelation/{rel['id']}")
    n_relationtype = URIRef(f"{idmrelations}{rel['relation_type']['id']}")
    g.add((pers_uri, bioc.has_person_relation, n_rel_type))
    g.add((n_rel_type, RDF.type, n_relationtype))
    g.add((n_rel_type, RDFS.label, Literal(f"{rel['relation_type']['label']}")))
    g.add((n_relationtype, RDFS.subClassOf, bioc.Person_Relationship_Role))
    g.add((URIRef(f"{idmapis}personproxy/{rel['related_personB']['id']}"), bioc.inheres_in, n_rel_type)) #TODO: add check if person already in triplestore
    #TODO: add hiarachy of person relations
    if rel['relation_type'] is not None:
        if rel['relation_type']['label'] != "undefined":
            if rel['relation_type']['parent_id'] is not None:
                g.add((n_relationtype, RDFS.subClassOf, URIRef(f"{idmrelations}{rel['relation_type']['parent_id']}")))  
                g.add((URIRef(f"{idmrelations}{rel['relation_type']['parent_id']}"), RDFS.subClassOf, URIRef(bioc.Group_Relationship_Role)))
    logging.info(f" personpersonrelation serialized for: {rel['related_personA']['id']}")
    #return g
    #group which is part of this relation
    return g

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
    if (URIRef(f"{idmapis}groupproxy/{rel['related_institution']['id']}"), None, None) not in g:
        await render_organization(rel['related_institution']['id'], g)
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
    for prof in person['professeion']:
        prof_node = URIRef(f"{idmapis}occupation/{prof['id']}")
        g.add((pers_uri, bioc.has_occupation, prof_node))
        g.add((prof_node, rdfs.label, Literal(prof['label'])))
        if prof['parend_id'] is not None:
            parent_prof_node = URIRef(f"{idmapis}occupation/{prof['parend_id']}")
            g.add((prof_node, rdfs.subClassOf, parent_prof_node))
            g.add((prof_node, rdfs.subClassOf, bioc.Occupation))
        else:
            g.add((prof_node, rdfs.subClassOf, bioc.Occupation))
    for uri in person['sameAs']:
        g.add((pers_uri, owl.sameAs, URIRef(uri)))
    # add occupations

    person_rel = await get_person_relations(person['id'], kinds=['personinstitution', 'personperson', 'personplace'])
    tasks = []
    for rel_type, rel_list in person_rel.items():
        if rel_type == 'personinstitution':
            for rel in rel_list:
                tasks.append(asyncio.create_task(
                    render_personinstitution_relation(pers_uri, rel, g)))
        elif rel_type == 'personperson':
            for rel in rel_list:
                tasks.append(asyncio.create_task(
                    render_personperson_relation(pers_uri, rel, g)))
        elif rel_type == 'personplace':
            for rel in rel_list:
                tasks.append(asyncio.create_task(
                    render_personplace_relation(pers_uri, rel, g)))
    await asyncio.gather(*tasks)
    return g


async def render_organization(organization, g):
    """renders organization object as RDF graph

    Args:
        organization (_type_): _description_
        g (_type_): _description_
    """
    res = await get_entity(organization, "institution")
    # setup basic nodes
    node_org = URIRef(f"{BASE_URI_SERIALIZATION}groupproxy/{organization}")
    appelation_org = URIRef(f"{idmapis}groupappellation/{organization}")
    #connect Group Proxy and person in named graphbgn:BioDes
    g.add((node_org, RDF.type, crm.E74_Group))
    #defines group class
    for uri in res['sameAs']:
        g.add((node_org, owl.sameAs, URIRef(uri)))
    #defines group as the same group in the APIS dataset
    g.add((node_org, crm.P1_is_identified_by, appelation_org))
    g.add((appelation_org, rdfs.label, Literal(res['name'])))
    g.add((appelation_org, RDF.type, crm.E33_E41_Linguistic_Appellation))
    #add group appellation and define it as linguistic appellation
    if res["start_date_written"] is not None:
        if len(res['start_date_written']) >= 4:
            start_date_node = URIRef(f"{idmapis}groupstart/{organization}")
            start_date_time_span = URIRef(f"{idmapis}groupstart/timespan/{organization}")
            #print(row['institution_name'], ':', row['institution_start_date'], row['institution_end_date'], row['institution_start_date_written'], row['institution_end_date_written'])
            g.add((start_date_node, RDF.type, crm.E63_Beginning_of_Existence))
            g.add((start_date_node, crm.P92_brought_into_existence, node_org))
            g.add((start_date_node, URIRef(crm + "P4_has_time-span"), start_date_time_span))
            g.add((start_date_time_span, rdfs.label, Literal(res['start_date_written'])))
            if len(res['start_date_written']) == 4 and res['start_end_date'] is not None:
                #check whether only a year has bin given for the start date and add according nodes
                g.add((start_date_time_span, crm.P82a_begin_of_the_begin, (Literal(f"{res['start_start_date']}T00:00:00", datatype=XSD.dateTime))))
                g.add((start_date_time_span, crm.P82b_end_of_the_end, (Literal(f"{res['start_end_date']}T23:59:59", datatype=XSD.dateTime))))
            else:
                g.add((start_date_time_span, crm.P82a_begin_of_the_begin, (Literal(f"{res['start_date']}T00:00:00", datatype=XSD.dateTime))))
                g.add((start_date_time_span, crm.P82b_end_of_the_end, (Literal(f"{res['start_date']}T23:59:59", datatype=XSD.dateTime))))
    if res["end_date_written"] is not None: 
        if len(res['end_date_written']) >= 4:
            end_date_node = URIRef(f"{idmapis}groupend/{organization}")
            end_date_time_span = URIRef(f"{idmapis}groupend/timespan/{organization}")
            #print(row['institution_name'], ':', row['institution_start_date'], row['institution_end_date'], row['institution_start_date_written'], row['institution_end_date_written'])
            g.add((end_date_node, RDF.type, crm.E64_End_of_Existence))
            g.add((end_date_node, crm.P93_took_out_of_existence, node_org))
            g.add((end_date_node, URIRef(crm + "P4_has_time-span"), end_date_time_span))
            g.add((end_date_time_span, rdfs.label, Literal(res['end_date_written'])))
            if len(res['end_date_written']) == 4 and res['end_end_date'] is not None:
                #check whether only a year has bin given for the start date and add according nodes
                g.add((end_date_time_span, crm.P82a_begin_of_the_begin, (Literal(f"{res['end_start_date']}T00:00:00", datatype=XSD.dateTime))))
                g.add((end_date_time_span, crm.P82b_end_of_the_end, (Literal(f"{res['end_end_date']}T23:59:59", datatype=XSD.dateTime))))
            else:
                g.add((end_date_time_span, crm.P82a_begin_of_the_begin, (Literal(f"{res['end_date']}T00:00:00", datatype=XSD.dateTime))))
                g.add((end_date_time_span, crm.P82b_end_of_the_end, (Literal(f"{res['end_date']}T23:59:59", datatype=XSD.dateTime))))
    return g

async def render_event(event, g):
    """renders event object as RDF graph

    Args:
        event (_type_): _description_
        g (_type_): _description_
    """
    #prepare basic node types
    #TODO: left of here with implementing
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


async def render_place(place, g):
    """renders place object as RDF graph

    Args:
        place (_type_): _description_
        g (_type_): _description_
    """
    res = await get_entity(place, 'place')
    # setup basic nodes
    node_place = URIRef(f"{idmapis}place/{res['id']}")
    node_appelation = URIRef(f"{idmapis}placeappellation/{res['id']}")
    node_plc_identifier = URIRef(f"{idmapis}placeidentifier/{res['id']}")

    g.add((node_place, RDF.type, crm.E53_Place))
    #define place as Cidoc E53 Place
    g.add((node_place, crm.P1_is_identified_by, node_appelation))
    #add appellation to place
    g.add((node_appelation, RDF.type, crm.E33_E41_Linguistic_Appellation))
    #define appellation as linguistic appellation 
    g.add((node_appelation, RDFS.label, Literal(res['name'])))
    #add label to appellation
    for uri in res['sameAs']:
        g.add((node_place, owl.sameAs, URIRef(uri)))
    g.add((node_place, crm.P1_is_identified_by, URIRef(f"{idmapis}placeidentifier/{res['id']}")))
    #add APIS Identifier as Identifier
    g.add((node_plc_identifier, RDF.type, crm.E_42_Identifier))
    #define APIS Identifier as E42 Identifier (could add a class APIS-Identifier or model a Identifier Assignment Event)
    g.add((node_plc_identifier, RDFS.label, Literal(res['id'])))
    #add label to APIS Identifier
    #define that individual in APIS named graph and APIS entity are the same
    if res['lat'] is not None and res['lng'] is not None:
        node_spaceprimitive = URIRef(f"{idmapis}spaceprimitive/{res['id']}")
        g.add((node_place, crm.P168_place_is_defined_by, node_spaceprimitive))
        g.add((node_spaceprimitive, rdf.type, crm.E94_Space_Primitive))
        g.add((node_spaceprimitive, crm.P168_place_is_defined_by, Literal((f"POINT {res['lat']} {res['lng']})"), datatype=geo.wktLiteral)))
        #define that individual in APIS named graph and APIS entity are the same
    # suggestion for serialization of space primitives according to ISO 6709, to be discussed
    # more place details will be added (references, source, place start date, place end date, relations to other places(?))

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
        logging.warn(f"Error getting persons: {res.status_code} / {res.text}")
    res = res.json()
    tasks = []
    while res:
        logging.info(f"working on offset {res['offset']}")
        for person in res["results"]:
            tasks.append(asyncio.create_task(render_person(person, g)))
        if "offset" in res:
            if int(res["offset"]) > 20:
                break
        if "next" in res:
            res = requests.get(res["next"]).json()
        else:
            break
    await asyncio.gather(*tasks)


async def get_entity(entity_id, entity_type):
    """gets organization object from API

    Args:
        organization_id (_type_): _description_
    """
    params = {"format": "json", "include_relations": "false"}
    res = requests.get(f"{BASE_URL_API}/entities/{entity_type}/{entity_id}/", params=params)
    if res.status_code != 200:
        logging.warn(f"Error getting {entity_type}: {res.status_code} / {res.text}")
    else:
        return res.json()


async def get_person_relations(person_id, kinds=["personplace", "personinstitution", "personevent", "personwork"]):
    """gets person relations from API

    Args:
        person_id (_type_): _description_
    """
    res_full = {}

    query_params = {"related_person": person_id}
    for kind in kinds:
        if kind == "personperson":
            query_params_pp = deepcopy(query_params)
            del query_params_pp["related_person"]
            query_params_pp["related_personA"] = person_id
            res = requests.get(f"{BASE_URL_API}/relations/{kind}/", params=query_params_pp)
        else:
            res = requests.get(f"{BASE_URL_API}/relations/{kind}/", params=query_params)
        if res.status_code != 200:
            logging.error(f"Error getting {kind} for person {person_id}")
            continue
        res = res.json()
        while res:
            res_full[kind] = res_full.get(kind, []) + res["results"]
            if res["next"] is not None:
                res = requests.get(res["next"]).json()
            else:
                break
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
