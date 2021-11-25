:- module(rewrite_bioned,
	  [ rewrite/0,
	    rewrite/1,
	    rewrite/2,
	    list_rules/0
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xmlrdf/rdf_convert_util)).
:- use_module(library(xmlrdf/cvt_vocabulary)).
:- use_module(library(xmlrdf/rdf_rewrite)).

:- debug(rdf_rewrite).

%%	rewrite
%
%	Apply all rules on the graph =data=

rewrite :-
	rdf_rewrite(bioned).

%%	rewrite(+Rule)
%
%	Apply the given rule on the graph =data=

rewrite(Rule) :-
	rdf_rewrite(bioned, Rule).

%%	rewrite(+Graph, +Rule)
%
%	Apply the given rule on the given graph.

rewrite(Graph, Rule) :-
	rdf_rewrite(Graph, Rule).

%%	list_rules
%
%	List the available rules to the console.

list_rules :-
	rdf_rewrite_rules.

:- discontiguous
	rdf_mapping_rule/5.



clean
@@
{_, bgn:biography, ""}
<=>
true.




% REWRITE RULES FOR FIXING THE BIODES OBJECT
% This is currently not changed

biodescapitalizeobject
@@
{S, P, bgn:'Biodes'}
<=>
true,
{S, P, bgn:'BioDes'}.

biodescapitalizesubject
@@
{bgn:'Biodes', P, O}
<=>
true,
{bgn:'BioDes', P, O}.

makebiodesuris
@@
{S, bgn:xmlFileID, literal(FileID)}\
{S}
<=>
literal_to_id(['BioDes-' ,FileID], bgn, URI),
{URI}.

addtypestobiodes
@@
{S, rdf:type, bgn:'BioDes'}
==>
true,
{S, rdf:type, ore:'Aggregation'},
{S, rdf:type, prov:'Entity'},
{S, rdf:type, pplan:'Entity'}.



% REWRITE RULES FOR FIXING THE PERSON DESCRIPTION SECTION

fixpersonpredicate
@@
{S, rdf:type, bgn:'BioDes'} \
{S, bgn:person, O}
<=>
true,
{S, bgn:hasPersonDes, O}.

fixpersonsubject
@@
{bgn:'Person', P, O}
<=>
true,
{bgn:'PersonDes', P, O}.

fixpersonobject
@@
{S, P, bgn:'Person'}
<=>
true,
%{S, P, bgn:'PersonDes'}.
{S, P, crm:'E21_Person'}.

addproxytopersondes
@@
{S, rdf:type, crm:'E21_Person'}
==>
true,
{S, rdf:type, idm:'Person_Proxy'}.

makepersondesuris
@@
{S, bgn:xmlFileID, literal(FileID)},
{S, bgn:hasPersonDes, O}\
{O}
<=>
literal_to_id(['PersonDes-' ,FileID], bgn, URI),
{URI}.





% REWRITE RULES FOR FIXING THE FILE DESCRIPTION SECTION
% untouched for now
fixfiledespredicate
@@
{S, rdf:type, bgn:'BioDes'} \
{S, bgn:fileDesc, O}
<=>
true,
{S, bgn:hasFileDes, O}.

fixfiledessubject
@@
{bgn:'FileDesc', P, O}
<=>
true,
{bgn:'FileDes', P, O}.

fixfiledesobject
@@
{S, P, bgn:'FileDesc'}
<=>
true,
{S, P, bgn:'FileDes'}.

makefiledesuris
@@
{S, bgn:xmlFileID, literal(FileID)},
{S, bgn:hasFileDes, O}\
{O}
<=>
literal_to_id(['FileDes-' ,FileID], bgn, URI),
{URI}.




% REWRITE RULES FOR FIXING THE BIOGRAPHY SECTION
% untouched for now

fixbiopartspredicate
@@
{S, rdf:type, bgn:'BioDes'} \
{S, bgn:biography, O}
<=>
true,
{S, bgn:hasBioParts, O}.

fixbiopartssubject
@@
{bgn:'Biography', P, O}
<=>
true,
{bgn:'BioParts', P, O}.

fixbiopartsobject
@@
{S, P, bgn:'Biography'}
<=>
true,
{S, P, bgn:'BioParts'}.

makebiopartsdesuris
@@
{S, bgn:xmlFileID, literal(FileID)},
{S, bgn:hasBioParts, O}\
{O}
<=>
literal_to_id(['BioParts-' ,FileID], bgn, URI),
{URI}.




% REWRITE RULES FOR RELATION CREATION

createpersonobjectsandlinkbiodesobjects
@@
{S, bgn:personID, literal(PersonID)}
<=>
true,
literal_to_id(['Person-', PersonID], bgn, P),
{S, bgn:aggregatedPerson, P}, %relation between biodes and person
{S, edm:aggregatedCHO, P},
{P, bgn:personID, literal(PersonID)}, %leave this for now, could be cidoc
{P, rdf:type, idm:'Provided_Person'}.

addproxyrelations
@@
{S, bgn:hasPersonDes, O},
{S, bgn:aggregatedPerson, P}
==>
true,
{O, ore:proxyIn, S},
{O, ore:proxyFor, P},
{O, idm:personProxyFor,P}.





% REWRITE RULES TO FIX FIGURE OBJECTS
% leave this for now
graphiconbiopartstofigure
@@
{bgn:'BioParts', bgn:graphic, G}
<=>
true,
{bgn:'BioParts', bgn:figure, F},
{F, rdf:type, bgn:'Figure'},
{F, bgn:graphic, G}.

figuretohasfigure
@@
{S, bgn:figure, O}
<=>
true,
{S, bgn:hasFigure, O}.

graphictohasgraphic
@@
{S, bgn:graphic, O}
<=>
true,
{S, bgn:hasGraphic, O}.



% Person names to CIDOC

fixpersonname
@@
{S, bgn:persname, O}
<=>
true,
{S, bgn:personName, O}.

fixpersonnameagain
@@
{S, bgn:persName, O}
<=>
true,
{S, bgn:personName, O}.


% BIRTH AND DEATH EVENTS
%

birthevent
@@
{S, bgn:event, E},
{E, bgn:type, "birth"},
	{E, bgn:date, Date} ?,
	{E, bgn:notAfter, NA} ?, % not used, todo
	{E, bgn:notBefore,NB} ?, % not used, todo
	{E, bgn:place, Place}?
<=>
true,
make_rand_uri(S,['birth'],Evt),
{Evt, crm:'P98_brought_into_life', S},
{Evt, rdf:type, crm:'E67_Birth'},
{Evt, crm:'P7_took_place_at', Place},
{Evt, crm:'P4_has_time_span', Date}.

deathevent
@@
{S, bgn:event, E},
{E, bgn:type, "death"},
	{E, bgn:date, Date} ?,
	{E, bgn:notAfter, NA} ?, % not used, todo
	{E, bgn:notBefore,NB} ?, % not used, todo
	{E, bgn:place, Place}?
<=>
true,
make_rand_uri(S,['death'],Evt),
{Evt, crm:'P100_was_death_of', S},
{Evt, rdf:type, crm:'E5_Event'},
{Evt, crm:'P7_took_place_at', Place},
{Evt, crm:'P4_has_time_span', Date}.


baptismevent
@@
{S, bgn:event, E},
{E, bgn:type, "baptism"},
	{E, bgn:date, Date} ?,
	{E, bgn:notAfter, NA} ?, % not used, todo
	{E, bgn:notBefore,NB} ?, % not used, todo
	{E, bgn:place, Place}?
<=>
true,
make_rand_uri(S,['baptism'],Evt),
{Evt, crm:'P39_Actor', S},
{Evt, rdf:type, crm:'E5_Event'},
{Evt, crm:'P7_took_place_at', Place},
{Evt, crm:'P4_has_time_span', Date}.

marriageevent
@@
{S, bgn:event, E},
{E, bgn:type, "marriage"},
	{E, bgn:date, Date} ?,
	{E, bgn:notAfter, NA} ?, % not used, todo
	{E, bgn:notBefore,NB} ?, % not used, todo
	{E, bgn:place, Place}?
<=>
true,
make_rand_uri(S,['marriage'],Evt),
{Evt, crm:'P39_Actor', S},
{Evt, rdf:type, crm:'E5_Event'},
{Evt, crm:'P7_took_place_at', Place},
{Evt, crm:'P4_has_time_span', Date}.

funeralevent
@@
{S, bgn:event, E},
{E, bgn:type, "funeral"},
	{E, bgn:date, Date} ?,
	{E, bgn:notAfter, NA} ?, % not used, todo
	{E, bgn:notBefore,NB} ?, % not used, todo
	{E, bgn:place, Place}?
<=>
true,
make_rand_uri(S,['funeral'],Evt),
{Evt, crm:'P39_Actor', S},
{Evt, rdf:type, crm:'E5_Event'},
{Evt, crm:'P7_took_place_at', Place},
{Evt, crm:'P4_has_time_span', Date}.


% TODO


% bgn:snippet on bgn:BioParts generally points to a resource of type bgn:Snippet giving bgn:sourceId
% bgn:text instead of bgn:snippet on bgn:BioParts
% bgn:figure points to resource of type bgn:Figure giving a bgn:graphic and a bgn:head (=caption); bgn:graphic points to resource of type bgn:Graphic giving bgn:url











