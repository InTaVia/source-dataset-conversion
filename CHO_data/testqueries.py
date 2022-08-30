import sys
from SPARQLWrapper import SPARQLWrapper, JSON

endpoint_url = "https://query.wikidata.org/sparql"

example_id_list = ["Q762", "Q34661"]

query = """SELECT ?aw ?instance ?instanceLabel
where {
    wd:Q34661 wdt:P800 ?aw .
    OPTIONAL { ?aw wdt:P31 ?instance } 
      SERVICE wikibase:label { bd:serviceParam wikibase:language "en,nl" }
}
LIMIT 10"""


def get_results(endpoint_url, example_id_list):
        user_agent = "WDQS-example Python/%s.%s" % (sys.version_info[0], sys.version_info[1])
        # TODO adjust user agent; see https://w.wiki/CX6
        sparql = SPARQLWrapper(endpoint_url, agent=user_agent)
        reslist = []
        # sparql.setQuery(f"""SELECT ?aw WHERE {{wd:{example_id} wdt:P800 ?aw .}}""")
        for example_id in example_id_list:
            print(example_id)
            sparql.setQuery(query)
            sparql.setQuery(f"""SELECT ?aw WHERE {{wd:{example_id} wdt:P800 ?aw .}} """)
            sparql.setReturnFormat(JSON)
            resp = sparql.query().convert()
        return resp



results = get_results(endpoint_url, example_id_list)

for result in results["results"]["bindings"]:
    print(result)