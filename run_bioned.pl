:- module(run_bioned,
	  [ run_bioned/0,
	    delete_bioned/0,
	    make_rand_uri/2,
	    make_rand_uri/3,
	    make_rand_uri_bnode/1,
	    sex_to_gender/2,
	    name_list_to_str/3,
	    load_xml_test_dir/0,
	    load_ontologies/0
	  ]).

user:file_search_path(data,       data).

:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(bgn,    'http://data.biographynet.nl/rdf/').
:- rdf_register_ns(edm,    'http://www.europeana.eu/schemas/edm/').
:- rdf_register_ns(ore,    'http://www.openarchives.org/ore/terms/').
:- rdf_register_ns(prov,   'http://www.w3.org/ns/prov#').
:- rdf_register_ns(pplan,  'http://purl.org/net/p-plan#').
:- rdf_register_ns(bioc,  'http://ldf.fi/schema/bioc/').
:- rdf_register_ns(crm, 'http://www.cidoc-crm.org/cidoc-crm/') .
:- rdf_register_ns(idm, 'http://www.intavia.eu/idm-core/').
:- rdf_register_ns(skos, 'http://www.w3.org/2004/02/skos/core#').


:- use_module([ library(xmlrdf/xmlrdf),
		library(semweb/rdf_cache),
		library(semweb/rdf_library),
		library(semweb/rdf_turtle_write)
	      ]).
:- use_module(rewrite_bioned).
:- dynamic rc/1.


load_ontologies :-
	rdf_load_library(dc),
	rdf_load_library(skos),
	rdf_load_library(rdfs),
     /*   rdf_load('prov-o.ttl'),    */
	rdf_load_library(owl),
	rdf_load('./data/rdf/cidoc.rdf'),
	rdf_load('./data/rdf/bgn_schema.ttl'),
	rdf_load('./data/rdf/intavia_idm1.ttl').

:- initialization			% run *after* loading this file
	rdf_set_cache_options([ global_directory('cache/rdf'),
				create_global_directory(true)
			      ]),
	load_ontologies.

load_biodes_file(XmlFile):-
        absolute_file_name(XmlFile, File,
			   [ access(read)
			   ]),
	load(File).
/*
	extractFileInfo(XmlFile, FileID, PersID, Suffix),
	setup_call_cleanup(
	    b_setval(bgn_context, bionedfileinfo(FileID, PersID, Suffix)),
	    rewrite(addidsandlinktoperson),
	    b_setval(bgn_context, [])
	).
*/

load_xml_dir:-
	expand_file_name('data/xml/full_preprocessed/*.xml', X),
%	expand_file_name('data/xml/full_preprocessed/*.XML', Y),
%	append(X, Y, Z),
	maplist(load_biodes_file, X).

load_xml_test_dir:-
	expand_file_name('data/xml/test/*.xml', X),
%	expand_file_name('data/xml/full_preprocessed/*.XML', Y),
%	append(X, Y, Z),
	maplist(load_biodes_file, X).

load(File) :-
	rdf_current_ns(bgn, Prefix),
	load_xml_as_rdf(File,
			[ dialect(xmlns), % this is dialect, others are xmlns
			  unit(bioport), % the root unit
			  prefix(Prefix), % prefix to use
			  graph(bioned) % save as graph name
			]).

% extractFileInfo(File, FileID, PersID, Suffix) :-
%	atomic_list_concat([_Path, FileName], 'data/xml/full/', File),
%	atomic_list_concat([FileID, _Extention], '.', FileName),
%	atomic_list_concat([PersID, Suffix], '_', FileID).



run_bioned:-
        rdf_load('mapschema.ttl'),
	load_xml_dir,
	rewrite,
	add_xsddates,
	save_bioned.


save_bioned:-
	absolute_file_name(data('rdf/biographynet.ttl'), File,
			   [ access(write)
			   ]),
	rdf_save_turtle(File,[graph(bioned)]).




delete_bioned:-
	rdf_retractall(_,_,_,bioned).


% UTILITY predicates
%
%"random" counter, for generating URIs
rc(0).
reset_rc:-
	retractall(rc(_)),
	assert(rc(0)).

next_rc(Next):-
	rc(A),
	retract(rc(A)),
	Next is A + 1,
	assert(rc(Next)).

make_rand_uri(InputURI,OutputURI):-
	%random(1,99999999,Rand),
	next_rc(Rand),
	atomic_list_concat([InputURI,'-',Rand],OutputURI).
make_rand_uri(InputURI,List,OutputURI):-
	next_rc(Rand),
	%random(1,99999999,Rand),
	atomic_list_concat([InputURI,'-',Rand|List],OutputURI).

make_rand_uri_bnode(OutputURI):-
	random(1,99999999,Rand),
	atomic_list_concat(['http://data.biographynet.nl/rdf/','bnode-',Rand],OutputURI).


sex_to_gender('1', 'http://ldf.fi/schema/bioc/Male').
sex_to_gender('2', 'http://ldf.fi/schema/bioc/Female').
%sex_to_gender('', 'http://ldf.fi/schema/bioc/Other'):-true,!.



% For a name URI, generate the concatenated name PLUS the
% individual name parts
name_list_to_str(_NameURI, [],''):-true,!.
name_list_to_str(NameURI, [element(name,[type=Type],[H])|T],Str):-
	name_list_to_str(NameURI,T,Rest),
	make_rand_uri(NameURI,['-',Type],NamePartURI),
	rdf_assert(NameURI,crm:'P148_has_component',NamePartURI,bioned),
	rdf_assert(NamePartURI,crm:'P2_has_type',literal(Type),bioned),
	rdf_assert(NamePartURI,rdf:type,crm:'E89_Propositional_Object',bioned),
	rdf_assert(NamePartURI,rdfs:label,literal(H),bioned),
	atomic_list_concat([H,' ',Rest],Str).
name_list_to_str(N,[H|T],Str):-
	atom(H),
	name_list_to_str(N,T,Rest),
	atomic_list_concat([H,' ',Rest],Str).


% add datetimes
%
%
add_xsddates:-
	forall(rdf_db:rdf(S,crm:'P82a_begin_of_the_begin',literal(Date)),
	       (rdf_retractall(S, crm:'P82a_begin_of_the_begin',literal(Date)),
		rdf_assert(S, crm:'P82a_begin_of_the_begin', literal(type('http://www.w3.org/2001/XMLSchema#date', Date))))),
	forall(rdf(S,crm:'P82b_end_of_the_end',literal(Date)),
	       (rdf_retractall(S, crm:'P82b_end_of_the_end',literal(Date)),
		rdf_assert(S, crm:'P82b_end_of_the_end', literal(type('http://www.w3.org/2001/XMLSchema#date', Date)))))	.


% Data enrichment predicates
%
% wikipedia_uri(?A,-B). returns a pair Provided person URI and wikipedia
% URL
wikipedia_uri(ProvidedPerson,WikipediaURL):-
	rdf(A,bgn:target,literal(WikipediaURL)),
	sub_atom(WikipediaURL,_,_,_,'http://nl.wikipedia.org/wiki/'), rdf(FD,bgn:ref,A),
	rdf(FD,rdf:type,_Type),
	rdf(BD,bgn:hasFileDes,FD),
	rdf(BD,bgn:aggregatedPerson,ProvidedPerson).

% input is a wiki url, output is a dbpedia url
wiki2db(Wiki,DB):-
	atomic_list_concat(Subs,'/',Wiki),
	reverse(Subs,[Term|_]),
	atomic_list_concat(['http://nl.dbpedia.org/resource/',Term],DB).


% for a name (last part of wiki url), fetch the wikidata Q item
wikipedia2wikidata(NameAtom,WDURI):-
 atomic_list_concat(['https://nl.wikipedia.org/w/api.php?action=query&prop=pageprops&format=json&titles=',NameAtom],URI),
 http_open(URI,S,[]),
json_read(S,json([_,query=json([_,(pages=json([(_=json(L))]))])])),reverse(L,[pageprops=json(List)|_]),reverse(List,[wikibase_item=Qiki|_]),
	atomic_list_concat(['http://www.wikidata.org/entity/',Qiki],WDURI).

