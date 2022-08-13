# Prioritization of CHO metadata

| Priority for frontend (Johannes)       | CHO Metadata for InTaVia           | Wikidata Mapping  |  wikidatata reference  |
| :-------------: |:-------------| -----:| -----:|
| person metadata  |
| !!!      | creator created artwort | Wikidata Item| [P170](https://www.wikidata.org/wiki/Property:P170) |
| !!!      | human/person is creator | Creator:Creator Entity| [Commons Creator page P11472](https://www.wikidata.org/wiki/Property:P1472) |
| !!!      | creator/person has identifier| Wikidata URI (Item Identifier) | [Wikidata human Q5](https://www.wikidata.org/wiki/Q5) |  
| !!!      | creator/person has appellation| text: Literal| [Wikidata name in native language](https://www.wikidata.org/wiki/Property:P1559P1559) |
| ?      | creator/person has image representation| Wikidata Item P18: Commons media file  |  [subclass of Q18610173: Wikidata property to link to Commons](https://www.wikidata.org/wiki/Q18610173) [Wikidata Item P18](https://www.wikidata.org/wiki/Property:P18) |
| ?      | copyright status as a creator |  copyright status as a creator  | [Wikidata Item P7763](https://www.wikidata.org/wiki/Property:P7763) |
| object metadata  |
| !!!      | object has identifier| Wikidata Item URI (e.g. Gos table, e.g. item Q12418) |  Commons Creator page Wikidata Item P1472 |  [P1472](https://www.wikidata.org/wiki/Property:P1472) |
| !!!     | object identifier assignment (to attach responsible actor, date, maybe place) | Wikidata: list with URIs of canonical Authority Documents name of Commons Infobox template residing in "Creator" namespace on Wikimedia Commons? |  discuss: [Authority Control?](https://en.wikipedia.org/wiki/Authority_control) |
| !!! | current location of object      |  CHO_Proxy -- P55_has_current_location--> E53 Place OR CHO_Proxy--edm:currentLocation--> crm:E1 Entity  | [Wikidata Property P276 location](https://www.wikidata.org/wiki/Property:P276)  |
| !      | Measurement|  |   |
| !     | Measurement Unit     |   |   |
| !! | Material      |    |   |
| !! | Title of Object      |    |   |
| !!! | language of textual object (on CHO)   |    |   |
| !!! | title of object (on CHO)   |    |
| !! | has part (other object)   |    |   |
| !!! | object has subject   |    |   |
| !!! | depicts   |    |
| !!! | container for all informal descriptions about an object   |    |
| !! | transfer of physical custody or the legal responsibility for the physical custody of objects  |    |
| !! | transfer of legal ownership of objects  |    |
| !!	| current owner  |    |
| !!	| is part of collection  |    |
| 	 CHO Event metadata      |
| 	 Production Event      |
| !!!	| Person/Group has produced Cultural Heritage Object| | 
| !!!	| Technique used in production event  |    |
| !!!	| time-span of creation of work  |  Wikidata Inception |   |
| !!!	| time-span of creation of manifestation |  start date: [Wikidata Item P139 earliest date](https://www.wikidata.org/wiki/Property:P1319) / end date: [Wikidata Item P1319 latest date](https://www.wikidata.org/wiki/Property:P1319) |   |
| !!!	| Place of creation  |    |
| 	 Destruction/ Modification of CHO      |
| !!!	| destruction of object  |    |
| 	 Person/Group has modified Cultural Heritage Object      |
| !!	| Type of CHO modification  |    |
| 	 References      |
| !	| same object in other resource (same as) / best to use with archive timestamps |  see first merging/mapping tables of of [netherlands] (Gos merge table), [finnish]() and [austrian]() datasets, number of entities will probably increase |  [P973 described in URL](https://www.wikidata.org/wiki/Property:P973)  |
| !!!	| link(s) to media file representation (external link, europeana doesn't store the data)  |  Wikidata Item P18: Commons media file | [Wikidata Item P18](https://www.wikidata.org/wiki/Property:P18)  |
