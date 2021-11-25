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






persontopersondespredicate
@@
{S, bgn:person, O}
<=>
true,
{S, bgn:hasPersonDes, O}.


persontopersondesresourcesub
@@
{bgn:'Person', P, O}
<=>
true,
{bgn:'PersonDes', P, O}.


persontopersondesresourceobj
@@
{S, P, bgn:'Person'}
<=>
true,
{S, P, bgn:'PersonDes'}.




addproxytopersondes
@@
{S, rdf:type, bgn:'PersonDes'}
==>
true,
{S, rdf:type, ore:'Proxy'}.


addidsandlinktoperson
@@
{S, rdf:type, bgn:'Biodes'}
==>
\+ rdf(S, bgn:bioFileID, _),
	b_getval(bgn_context, bionedfileinfo(FileID, PersID, Suffix)),
        {S, rdf:type, ore:'Aggregation'},
        {S, rdf:type, prov:'Entity'},
        {S, rdf:type, pplan:'Entity'},
	{S, bgn:bioFileID, literal(FileID) },
	{S, bgn:bioSuffix, literal(Suffix) },
	literal_to_id(['Person-', PersID], bgn, P),
	{S, bgn:aggregatedPerson, P},
	{S, edm:aggregatedCHO, P},
	{P, bgn:personRepID, literal(PersID)},
	{P, rdf:type, bgn:'Person'},
	{P, rdf:type, edm:'ProvidedCHO'}.


makebiodesuris
@@
{S, bgn:bioFileID, literal(FileID)}\
{S}
<=>
literal_to_id(['biodes-' ,FileID], bgn, URI),
{URI}.


biographytohasbioparts
@@
{S, bgn:biography, O}
<=>
true,
{S, bgn:hasBioParts, O}.


biographytobiopartssub
@@
{bgn:'Biography', P, O}
<=>
true,
{bgn:'BioParts', P, O}.

biographytobiopartsobj
@@
{S, O, bgn:'Biography'}
<=>
true,
{S, O, bgn:'BioParts'}.


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



% bgn:snippet on bgn:BioParts generally points to a resource of type bgn:Snippet giving bgn:sourceId
% bgn:text instead of bgn:snippet on bgn:BioParts
% bgn:figure points to resource of type bgn:Figure giving a bgn:graphic and a bgn:head (=caption); bgn:graphic points to resource of type bgn:Graphic giving bgn:url 











