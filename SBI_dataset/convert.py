#!/usr/bin/env python

"""SBI data converter.

Usage:
    convert.py <sbi-data.xml> <output.ttl> [--count=<count>]

Options:
    -h --help       Show this screen.
    --count=<count> Limit the number of people to convert.
"""

import urllib
from lxml import etree
from docopt import docopt
from decimal import Decimal
from dataclasses import dataclass
from rdflib import Graph, Literal, Namespace, URIRef


def main(file_name, output_file_name, count=None):
    people = load_people(file_name, count)
    graph = create_graph(people)
    graph.serialize(destination=output_file_name, format='turtle')


# -----------------------------------------------------------------------------
# Create the InTavia RDF graph from the available people data.

def create_graph(people):
    """Create RDF graph of the provided people."""

    g = Graph()

    def name_uri(suffix):
        return f'{Namespaces.intavia_sbi}name/{suffix}'

    for index, person in enumerate(people):
        person_uri = URIRef(f'{Namespaces.intavia}person/{index}')
        person_proxy_uri = URIRef(f'{Namespaces.intavia_sbi}personproxy/{person.id}')

        # Person entity in the shared InTaVia named graph
        g.add((person_uri, Namespaces.rdf.type, Namespaces.idm_core.Provided_Person))

        # Connect the Person proxy and the person in the named graph
        g.add((person_proxy_uri, Namespaces.idm_core.person_proxy_for, person_uri))

        # Define the Person proxy
        g.add((person_proxy_uri, Namespaces.rdf.type, Namespaces.idm_core.Person_Proxy))
        g.add((person_proxy_uri, Namespaces.rdf.type, Namespaces.crm.E21_Person))

        # Connect the person to the SBI person
        g.add((person_proxy_uri, Namespaces.owl.sameAs, (URIRef(person.uri))))

        # Add the person label
        g.add((person_proxy_uri, Namespaces.rdfs.label, Literal(f'{person.full_name}')))

        # ---------------------------------------------------------------------
        # Gender

        g.add((person_proxy_uri, Namespaces.bioc.has_gender, URIRef(f'{Namespaces.bioc}{person.gender}')))
        g.add((URIRef(f'{Namespaces.bioc}{person.gender}'), Namespaces.rdf.type, Namespaces.bioc.Gender))

        # ---------------------------------------------------------------------
        # Name

        # Add the name entity
        g.add((person_proxy_uri, Namespaces.crm.P1_is_identified_by, URIRef(name_uri(f'1/{person.id}'))))
        g.add((URIRef(name_uri(f'1/{person.id}')), Namespaces.rdf.type, Namespaces.crm.E33_E41_Linguistic_Appellation))
        g.add((URIRef(name_uri(f'1/{person.id}')), Namespaces.rdfs.label, Literal(person.full_name)))

        # Add the first_name
        if person.first_name:
            g.add((URIRef(name_uri(f'1/{person.id}')), Namespaces.crm.P148_has_component, URIRef(name_uri(f'2/{person.id}'))))
            g.add((URIRef(name_uri(f'2/{person.id}')), Namespaces.rdf.type, Namespaces.crm.E33_E41_Linguistic_Appellation))
            g.add((URIRef(name_uri(f'2/{person.id}')), Namespaces.rdfs.label, Literal(person.first_name)))
            g.add((URIRef(name_uri(f'2/{person.id}')), Namespaces.crm.P2_has_type, Namespaces.idm_nametype.forename))

        # Add the last_name
        if person.last_name:
            g.add((URIRef(name_uri(f'1/{person.id}')), Namespaces.crm.P148_has_component, URIRef(name_uri(f'3/{person.id}'))))
            g.add((URIRef(name_uri(f'3/{person.id}')), Namespaces.rdf.type, Namespaces.crm.E33_E41_Linguistic_Appellation))
            g.add((URIRef(name_uri(f'3/{person.id}')), Namespaces.rdfs.label, Literal(person.last_name)))
            g.add((URIRef(name_uri(f'3/{person.id}')), Namespaces.crm.P2_has_type, Namespaces.idm_nametype.surname))

        # Add the name
        if person.name:
            g.add((URIRef(name_uri(f'1/{person.id}')), Namespaces.crm.P148_has_component, URIRef(name_uri(f'3/{person.id}'))))
            g.add((URIRef(name_uri(f'4/{person.id}')), Namespaces.rdf.type, Namespaces.crm.E33_E41_Linguistic_Appellation))
            g.add((URIRef(name_uri(f'4/{person.id}')), Namespaces.rdfs.label, Literal(person.name)))
            g.add((URIRef(name_uri(f'4/{person.id}')), Namespaces.crm.P2_has_type, Namespaces.idm_nametype.name))

        # ---------------------------------------------------------------------
        # Birth and death

        for birth in person.birth:
            birth_event_uri = URIRef(f'{Namespaces.intavia_sbi}birthevent/{person.id}')
            g.add((birth_event_uri, Namespaces.rdf.type, Namespaces.crm.E67_Birth))
            g.add((birth_event_uri, Namespaces.crm.P98_brought_into_life, person_proxy_uri))
            if birth.date:
                g.add((birth_event_uri, Namespaces.crm["P4_has_time-span"], URIRef(f'{Namespaces.intavia_sbi}timespan/birth/{person.id}')))
            if birth.place:
                g.add((birth_event_uri, Namespaces.crm.P7_took_place_at, URIRef(f'{Namespaces.intavia_sbi}placeproxy/TODO-PLACE-ID')))

        for death in person.death:
            death_event_uri = URIRef(f'{Namespaces.intavia_sbi}deathevent/{person.id}')
            g.add((death_event_uri, Namespaces.rdf.type, Namespaces.crm.E69_Death))
            g.add((death_event_uri, Namespaces.crm.P100_was_death_of, person_proxy_uri))
            if death.date:
                g.add((death_event_uri, Namespaces.crm["P4_has_time-span"], URIRef(f'{Namespaces.intavia_sbi}timespan/death/{person.id}/1')))
            if death.place:
                g.add((death_event_uri, Namespaces.crm.P7_took_place_at, URIRef(f'{Namespaces.intavia_sbi}placeproxy/TODO-PLACE-ID')))

    # TODO
    # birth and death
    # - date
    # - place
    # occupation
    # relations

    Namespaces.bind_to_graph(g)

    return g


# -----------------------------------------------------------------------------
# Load and parse people from the SBI data file.

def load_people(file_name, count=None):
    """Load data from the provided URL or file, pase it and convert it to the Person class."""

    if is_url(file_name):
        xml = etree.fromstring(urllib.request.urlopen(file_name).read())
    else:
        xml = etree.parse(file_name)

    people = list(xpath_element(xml, '//tei:text/tei:body/tei:listPerson/tei:person[@role="main"]'))
    if count:
        people = people[:int(count)]

    for xml_person in people:
        persName = xpath_element(xml_person, 'tei:persName')[0]

        person = Person(
            id=xml_person.get('{http://www.w3.org/XML/1998/namespace}id'),
            name=xpath_value(persName, 'tei:name'),
            first_name=xpath_value(persName, 'tei:forename'),
            last_name=xpath_value(persName, 'tei:surname'),
            gender=parse_gender(xml_person),
            birth=list(parse_events(xpath_element(xml_person, 'tei:birth'))),
            death=list(parse_events(xpath_element(xml_person, 'tei:death'))),
        )

        yield person


# -----------------------------------------------------------------------------
# Helper functions ans classes

def xpath_element(element, xpath):
    return element.xpath(xpath, namespaces={'tei': 'http://www.tei-c.org/ns/1.0'})


def xpath_value(element, xpath):
    element = xpath_element(element, xpath)
    if element:
        return element[0].text
    else:
        return None


def is_url(string):
    try:
        result = urllib.parse.urlparse(string)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False


def parse_gender(person):
    try:
        match xpath_element(person, 'tei:sex')[0].get('value'):
            case '1':
                return 'male'
            case '2':
                return 'female'
            case _:
                return None
    except IndexError:
        return None


def parse_date(xml_date):
    if not xml_date:
        return None
    else:
        if not xml_date[0].get('when'):
            return None
        else:
            match xml_date[0].get('when').split('-'):
                case [year, month, day]:
                    return Date(year=int(year), month=int(month), day=int(day))
                case [year, month]:
                    return Date(year=int(year), month=int(month), day=None)
                case [year]:
                    return Date(year=int(year), month=None, day=None)


def parse_place(xml_place):

    def parse_place_parts(xml_place):
        for part in ['tei:settlement', 'tei:region', 'tei:country']:
            xml_part = xpath_element(xml_place, part)
            if xml_part:
                yield xml_part[0].text

    def parse_location(xml_location):
        if not xml_location:
            return None
        else:
            location = xml_location[0].text.split(' ')
            return Location(lat=Decimal(location[0]), lng=Decimal(location[1]))

    if not xml_place:
        return None
    else:
        xml_place = xml_place[0]

        name_parts = list(parse_place_parts(xml_place))
        if not name_parts:
            name = None
        else:
            name = ', '.join(name_parts)

        return Place(
            name=name,
            location=parse_location(xpath_element(xml_place, 'tei:geo')),
        )


def parse_events(xml_events):
    for xml_event in xml_events:
        yield Event(
            date=parse_date(xpath_element(xml_event, 'tei:date')),
            place=parse_place(xpath_element(xml_event, 'tei:placeName')),
        )


@dataclass
class Date:
    year: int
    month: int
    day: int


@dataclass
class Location:
    lat: Decimal
    lng: Decimal


@dataclass
class Place:
    name: str
    location: Location


@dataclass
class Event:
    date: Date
    place: Place


@dataclass
class Person:
    """Class respresenting a person in the SBI."""
    id: str
    name: str
    first_name: str
    last_name: str
    gender: str
    birth: list[Event]
    death: list[Event]

    def __str__(self) -> str:
        if self.name:
            return str(self.name)
        else:
            return f'{self.last_name} {self.first_name}'

    @property
    def uri(self):
        return f'https://www.slovenska-biografija.si/oseba/{self.id}/'

    @property
    def full_name(self):
        if self.name:
            return self.name
        else:
            return f'{self.last_name}, {self.first_name}'


class Namespaces:
    """Class respresenting RDF namespaces."""

    rdf = Namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#')
    rdfs = Namespace('http://www.w3.org/2000/01/rdf-schema#')
    owl = Namespace('http://www.w3.org/2002/07/owl#')

    intavia = Namespace('http://www.intavia.eu/')
    intavia_sbi = Namespace('http://www.intavia.eu/sbi/')

    crm = Namespace('http://www.cidoc-crm.org/cidoc-crm/')
    bioc = Namespace('http://www.ldf.fi/schema/bioc/')

    idm_core = Namespace('http://www.intavia.eu/idm-core/')
    idm_role = Namespace('http://www.intavia.eu/idm-role/')
    idm_nametype = Namespace('http://www.intavia.eu/nametype/')

    @classmethod
    def bind_to_graph(cls, graph):
        graph.bind('owl', cls.owl)
        graph.bind('crm', cls.crm)
        graph.bind('bioc', cls.bioc)
        graph.bind('idm_core', cls.idm_core)
        graph.bind('idm_role', cls.idm_role)
        graph.bind('idm_nametype', cls.idm_nametype)


# -----------------------------------------------------------------------------
# Entry point

if __name__ == '__main__':
    arguments = docopt(__doc__, version='SBI data converter')
    main(arguments['<sbi-data.xml>'], arguments['<output.ttl>'], count=arguments['--count'])
