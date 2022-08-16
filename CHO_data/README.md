# Prioritization of CHO metadata

| Priority for frontend (Johannes)       | CHO Metadata for InTaVia  *or* Intavia description          | Wikidata Mapping  |  wikidatata reference  |
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
| !!! | current location of object      |  artwork -- P55_has_current_location-- E53 Place *or* artwork--edm:currentLocation--> crm:E1 Entity  | [Wikidata Property P276 location](https://www.wikidata.org/wiki/Property:P276)  |
| !      | Measurement Event| E16 Measurement --P39 measured--> CHO_Proxy --P40 observed dimension (Type of dimension)--> E54 Dimension --P2 has type--> E55 Type | Wikidata properties [width P 2049](https://www.wikidata.org/wiki/Property:P2049) and [height P2048](https://www.wikidata.org/wiki/Property:P2048)|
| !     | Measurement Unit     |  Data Cleaning for: E16 Measurement --P39 measured--> CHO_Proxy --P40 observed dimension (Type of dimension)--> E54 Dimension --P90 has value--> rdfs:Literal | in Wikidata: Literal (e.g. "100 cm" or "100 centimetre")  |
| !! | Material      |    | made from material [P186](https://www.wikidata.org/w/index.php?title=Property:P168&action=edit&redlink=1) (-> this property was removed from Wikidata = find equivalent in other datasets/Wikidata examples): material the subject or the object is made of or derived from (do not confuse with P10672 which is used for processes) |
| !! | Title of Object      |    |   |
| !!! | language of textual object (on CHO)   |  language of work or name  P407  |  [P407](https://www.wikidata.org/wiki/Property:P407)  |
| !!! | title of object (on CHO)   |  wikidata label  |
| ? | named after   |   P138 named after   |   [named after](https://www.wikidata.org/wiki/Property:P138)   |
| !! | has part (other object)   |   [P527 has part(s)](P527) (reverse:  [P361 part of](https://www.wikidata.org/wiki/Property:P361))  |  [P527](https://www.wikidata.org/wiki/Property:P527)  |
| !!! | object has subject   |  [P180 depicts](https://www.wikidata.org/wiki/Property:P180): "entity visually depicted in an image, literarily described in a work, or otherwise incorporated into an audiovisual or other medium; see also P921, 'main subject'" |  [P180](https://www.wikidata.org/wiki/Property:P180)  |
| !!! | depicts   | (see above) diverse properties, more detailed approach, e.g. for Mona Lisa discussion: Wikidata Item [Q11879536 person depicted in Mona Lisa](https://www.wikidata.org/wiki/Q11879536)   |
| !!! | container for all informal descriptions about an object   |  e.g. name or *Wikidata Description* (The description on a Wikidata entry is a short phrase designed to disambiguate items with the same or similar labels. A description does not need to be unique; multiple items can have the same description, however no two items can have both the same label and the same description. )  |
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
