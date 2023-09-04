#!/usr/bin/env python

"""SBI data converter.

Usage:
    convert.py <sbi-data.xml> <output.ttl> [--count=<count>]

Options:
    -h --help       Show this screen.
    --count=<count> Limit the number of people to convert.
"""

import urllib
import hashlib
import calendar
from lxml import etree
from docopt import docopt
from decimal import Decimal
from dataclasses import dataclass
from rdflib import Graph, Literal, Namespace, URIRef


def main(file_name, output_file_name, count=None):
    xml = load_xml(file_name)
    occupations = dict((occupation.code, occupation) for occupation in parse_occupations_taxonomy(xml))
    people = parse_people(xml, occupations, count)
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
        person_proxy_uri = URIRef(f'{Namespaces.intavia_sbi}personproxy/{person.id}')

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

        for index, birth in enumerate(person.birth, 1):
            birth_event_uri = URIRef(f'{Namespaces.intavia_sbi}birthevent/{person.id}/{index}')
            g.add((birth_event_uri, Namespaces.rdf.type, Namespaces.crm.E67_Birth))
            g.add((birth_event_uri, Namespaces.crm.P98_brought_into_life, person_proxy_uri))
            if birth.date:
                g.add((birth_event_uri, Namespaces.crm["P4_has_time-span"], URIRef(birth.date.uri)))
                add_date_to_graph(g, birth.date)
            if birth.place:
                g.add((birth_event_uri, Namespaces.crm.P7_took_place_at, birth.place.proxy_uri))
                add_place_to_graph(g, birth.place)

        for index, death in enumerate(person.death, 1):
            death_event_uri = URIRef(f'{Namespaces.intavia_sbi}deathevent/{person.id}/{index}')
            g.add((death_event_uri, Namespaces.rdf.type, Namespaces.crm.E69_Death))
            g.add((death_event_uri, Namespaces.crm.P100_was_death_of, person_proxy_uri))
            if death.date:
                g.add((death_event_uri, Namespaces.crm["P4_has_time-span"], URIRef(death.date.uri)))
                add_date_to_graph(g, death.date)
            if death.place:
                g.add((death_event_uri, Namespaces.crm.P7_took_place_at, death.place.proxy_uri))
                add_place_to_graph(g, death.place)

        # ---------------------------------------------------------------------
        # Occupation

        for occupation in person.occupations:
            occupation_uri = URIRef(f'{Namespaces.intavia_sbi}occupation/{occupation.code}')

            # Add occupation
            g.add((occupation_uri, Namespaces.rdf.type, Namespaces.bioc.Occupation))
            g.add((occupation_uri, Namespaces.rdfs.label, Literal(occupation.code, lang='sl')))
            if occupation.male_label:
                g.add((occupation_uri, Namespaces.skos.altLabel, Literal(occupation.male_label, lang='sl')))
            if occupation.female_label:
                g.add((occupation_uri, Namespaces.skos.altLabel, Literal(occupation.female_label, lang='sl')))

            # Add occupation to the person
            g.add((person_proxy_uri, Namespaces.bioc.has_occupation, occupation_uri))

    # TODO
    # relations

    Namespaces.bind_to_graph(g)

    return g


# -----------------------------------------------------------------------------
# XML parsing funcions

def load_xml(file_name):
    """Load data from the provided URL or file."""

    if is_url(file_name):
        return etree.fromstring(urllib.request.urlopen(file_name).read())
    else:
        return etree.parse(file_name)


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
                return 'Male'
            case '2':
                return 'Female'
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
        for part in ['tei:settlement', 'tei:region', 'tei:country', 'tei:geogName']:
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

        location = parse_location(xpath_element(xml_place, 'tei:geo'))

        if not (name or location):
            return None
        else:
            return Place(name=name, location=location)


def parse_events(xml_events):
    for xml_event in xml_events:
        yield Event(
            date=parse_date(xpath_element(xml_event, 'tei:date')),
            place=parse_place(xpath_element(xml_event, 'tei:placeName')),
        )


def parse_occupations(person, occupations_taxonomy):
    for occupation in xpath_element(person, 'tei:occupation'):
        code = occupation.get('code')[1:]
        try:
            yield occupations_taxonomy[code]
        except KeyError:
            pass


def parse_occupations_taxonomy(xml):
    for occupation in xpath_element(xml, '//tei:taxonomy[@xml:id="occupation"]//tei:category'):
        yield(Occupation(
            code=occupation.get('{http://www.w3.org/XML/1998/namespace}id'),
            male_label=xpath_value(occupation, 'tei:desc[@ana="#masc"]'),
            female_label=xpath_value(occupation, 'tei:desc[@ana="#fem"]'),
        ))


def parse_people(xml, occupations_taxonomy, count=None):
    """Convert XML to the list of Person classes."""

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
            occupations=list(parse_occupations(xml_person, occupations_taxonomy)),
        )

        yield person


# -----------------------------------------------------------------------------
# Graph adding functions

def add_place_to_graph(g, place):
    g.add((place.proxy_uri, Namespaces.rdf.type, Namespaces.crm.E53_Place))
    g.add((place.proxy_uri, Namespaces.rdf.type, Namespaces.idm_core.Place_Proxy))
    g.add((place.proxy_uri, Namespaces.rdfs.label, Literal(place.name, lang='sl')))

    g.add((place.proxy_uri, Namespaces.crm.P1_is_identified_by, place.appelation_uri))
    g.add((place.appelation_uri, Namespaces.rdf.type, Namespaces.crm.E33_E41_Linguistic_Appellation))
    g.add((place.appelation_uri, Namespaces.rdfs.label, Literal(place.name, lang='sl')))

    if place.location:
        g.add((place.proxy_uri, Namespaces.crm.P168_place_is_defined_by, Literal(f'POINT {place.location.lng} {place.location.lat}', datatype=Namespaces.geo.wktLiteral)))


def add_date_to_graph(g, date):
    g.add((date.uri, Namespaces.rdf.type, Namespaces.crm['E52_Time-Span']))
    g.add((date.uri, Namespaces.rdfs.label, Literal(date.uid)))
    g.add((date.uri, Namespaces.crm.P82a_begin_of_the_begin, Literal(date.start_time, datatype=Namespaces.xsd.dateTime)))
    g.add((date.uri, Namespaces.crm.P82b_end_of_the_end, Literal(date.end_time, datatype=Namespaces.xsd.dateTime)))


# -----------------------------------------------------------------------------
# Entity classes

@dataclass
class Date:
    year: int
    month: int
    day: int

    @property
    def uid(self):
        match (self.year, self.month, self.day):
            case (year, None, None):
                return year
            case (year, month, None):
                return f'{year}-{month:02}'
            case (year, month, day):
                return f'{year}-{month:02}-{day:02}'

    @property
    def uri(self):
        return URIRef(f'{Namespaces.intavia_sbi}timespan/{self.uid}')

    @property
    def start_time(self):
        match (self.year, self.month, self.day):
            case (year, None, None):
                return f'{year}-01-01T00:00:00'
            case (year, month, None):
                return f'{year}-{month:02}-01T00:00:00'
            case (year, month, day):
                return f'{year}-{month:02}-{day:02}T00:00:00'

    @property
    def end_time(self):
        match (self.year, self.month, self.day):
            case (year, None, None):
                return f'{year}-12-31T23:59:59'
            case (year, month, None):
                return f'{year}-{month:02}-{calendar.monthrange(year, month)[1]}T23:59:59'
            case (year, month, day):
                return f'{year}-{month:02}-{day:02}T23:59:59'


@dataclass
class Location:
    lat: Decimal
    lng: Decimal


@dataclass
class Place:
    name: str
    location: Location

    @property
    def uid(self):
        return hashlib.md5(self.name.encode()).hexdigest()

    @property
    def proxy_uri(self):
        return URIRef(f'{Namespaces.intavia_sbi}placeproxy/{self.uid}')

    @property
    def appelation_uri(self):
        return URIRef(f'{Namespaces.intavia_sbi}placeappellation/{self.uid}')


@dataclass
class Event:
    date: Date
    place: Place


@dataclass
class Occupation:
    code: str
    male_label: str
    female_label: str


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
    occupations: list[Occupation]

    def __str__(self) -> str:
        if self.name:
            return str(self.name)
        else:
            return f'{self.last_name} {self.first_name}'

    @property
    def uri(self):
        return URIRef(f'https://www.slovenska-biografija.si/oseba/{self.id}/')

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
    xsd = Namespace('http://www.w3.org/2001/XMLSchema#')

    owl = Namespace('http://www.w3.org/2002/07/owl#')

    intavia = Namespace('http://www.intavia.eu/')
    intavia_sbi = Namespace('http://www.intavia.eu/sbi/')

    crm = Namespace('http://www.cidoc-crm.org/cidoc-crm/')
    bioc = Namespace('http://ldf.fi/schema/bioc/')

    idm_core = Namespace('http://www.intavia.eu/idm-core/')
    idm_role = Namespace('http://www.intavia.eu/idm-role/')
    idm_nametype = Namespace('http://www.intavia.eu/nametype/')

    geo = Namespace('http://www.opengis.net/ont/geosparql#')
    skos = Namespace('http://www.w3.org/2004/02/skos/core#')

    @classmethod
    def bind_to_graph(cls, graph):
        graph.bind('owl', cls.owl)
        graph.bind('crm', cls.crm)
        graph.bind('bioc', cls.bioc)
        graph.bind('idm_core', cls.idm_core)
        graph.bind('idm_role', cls.idm_role)
        graph.bind('idm_nametype', cls.idm_nametype)
        graph.bind('geo', cls.geo)
        graph.bind('skos', cls.skos)


# -----------------------------------------------------------------------------
# Entry point

if __name__ == '__main__':
    arguments = docopt(__doc__, version='SBI data converter')
    main(arguments['<sbi-data.xml>'], arguments['<output.ttl>'], count=arguments['--count'])
