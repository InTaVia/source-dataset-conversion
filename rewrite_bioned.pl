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


% FIX GENDER
%
sextogender
@@
{S, bgn:sex, literal(Sex)}
<=>
true,
sex_to_gender(Sex,Gender),
{S, bioc:gender, Gender},
	{Gender, rdf:type, bioc:'Gender'} .


sextogender
@@
{S, bgn:sex, O},
{O, bgn:value, literal(Sex)}
<=>
true,
sex_to_gender(Sex,Gender),
{S, bioc:gender, Gender},
	{Gender, rdf:type, bioc:'Gender'} .



% NATIONALITY
%
nationality
@@
{S, rdf:type, crm:'E21_Person'}
==>
{S, bioc:nationality, bioc:dutch}.

% Person names to CIDOC
% TODO: this needs some refining

personnameCidoc
@@
{S, bgn:hasPersName, literal(type(_,[Val]))} %simple names
<=>
make_rand_uri(S,['-name'],NameURI),
atom(Val),
{S, crm:'P1_is_identified_by', NameURI},
	{NameURI, rdf:type, crm:'E41_Appellation'},
	{NameURI, rdfs:label, literal(Val)}.


personnameCidocComplex
@@
{S, bgn:hasPersName, literal(type(_,List))} %complex names
<=>
make_rand_uri(S,['-name'],NameURI),
name_list_to_str(NameURI, List,Val),
{S, crm:'P1_is_identified_by', NameURI},
	{NameURI, rdf:type, crm:'E41_Appellation'},
	{NameURI, rdfs:label, literal(Val)}.

cleanpersname
@@
{_,bgn:hasPersName,_}
<=>
true.

cleanpersname
@@
{_,rdf:value,literal(type(_,_))}
<=>
true.

% BIRTH AND DEATH EVENTS
%

birthevent
@@
{S, bgn:event, E},
{E, bgn:type, "birth"},
	{E, bgn:date, Date} ?,
	{E, bgn:notAfter, _NA} ?, % not used, todo
	{E, bgn:notBefore,_NB} ?, % not used, todo
	{E, bgn:place, Place}?
<=>
true,
make_rand_uri(S,['birth'],Evt),
{Evt, crm:'P98_brought_into_life', S},
{Evt, rdf:type, crm:'E67_Birth'},
{Evt, crm:'P7_took_place_at', Place},
{Evt, crm:'P4_has_time-span', Date}.

deathevent
@@
{S, bgn:event, E},
{E, bgn:type, "death"},
	{E, bgn:date, Date} ?,
	{E, bgn:notAfter, _NA} ?, % not used, todo
	{E, bgn:notBefore,_NB} ?, % not used, todo
	{E, bgn:place, Place}?
<=>
true,
make_rand_uri(S,['death'],Evt),
{Evt, crm:'P100_was_death_of', S},
{Evt, rdf:type, crm:'E5_Event'},
{Evt, crm:'P7_took_place_at', Place},
{Evt, crm:'P4_has_time-span', Date}.


baptismevent
@@
{S, bgn:event, E},
{E, bgn:type, "baptism"},
	{E, bgn:date, Date} ?,
	{E, bgn:notAfter, _NA} ?, % not used, todo
	{E, bgn:notBefore,_NB} ?, % not used, todo
	{E, bgn:place, Place}?
<=>
true,
make_rand_uri(S,['baptism'],Evt),
{Evt, crm:'P39_Actor', S},
{Evt, rdf:type, crm:'E5_Event'},
{Evt, crm:'P7_took_place_at', Place},
{Evt, crm:'P4_has_time-span', Date}.

marriageevent
@@
{S, bgn:event, E},
{E, bgn:type, "marriage"},
	{E, bgn:date, Date} ?,
	{E, bgn:notAfter, _NA} ?, % not used, todo
	{E, bgn:notBefore,_NB} ?, % not used, todo
	{E, bgn:place, Place}?
<=>
true,
make_rand_uri(S,['marriage'],Evt),
{Evt, crm:'P39_Actor', S},
{Evt, rdf:type, crm:'E5_Event'},
{Evt, crm:'P7_took_place_at', Place},
{Evt, crm:'P4_has_time-span', Date}.

funeralevent
@@
{S, bgn:event, E},
{E, bgn:type, "funeral"},
	{E, bgn:date, Date} ?,
	{E, bgn:notAfter, _NA} ?, % not used, todo
	{E, bgn:notBefore,_NB} ?, % not used, todo
	{E, bgn:place, Place}?
<=>
true,
make_rand_uri(S,['funeral'],Evt),
{Evt, crm:'P39_Actor', S},
{Evt, rdf:type, crm:'E5_Event'},
{Evt, crm:'P7_took_place_at', Place},
{Evt, crm:'P4_has_time-span', Date}.




% STATES

% States with potential temporal qualifiers. These include the
% following states: residence (to,from,when) floruit occupation
% (to,when,from) category, faith (to, from), religion, education,
% claim_to_fame
% TODO: potentially remove events for non-qualified occupations to
% reduce triples?
occupationstate_event
@@
{S, bgn:state, E},
{E, bgn:type, "occupation"},
{E, rdf:value, Val},
{E, bgn:to, ToDate} ?,
{E, bgn:from,FromDate} ?, % not used, todo
{E, bgn:when,WhenDate} ?
<=>
true,
literal_to_id(['Occupation-' ,Val], bgn, OURI), % first make the occupation instance
	{OURI, rdf:type, bgn:'Occupation'},  % TODO: should be profession?
	{OURI, rdfs:label, Val},
	{S, bioc:has_occupation, OURI},
make_rand_uri(S,['actorrole'],ARURI), %then make the actorrole instance
	{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	{ARURI, rdf:type, bioc:'Actor_Role'},
	{S,bioc:has_relation,ARURI},
make_rand_uri(S,['occupationevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type,crm:'E5_Event'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant_in',ARURI},
make_rand_uri(S,['occupationevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P79_beginning_is_qualified_by', FromDate},
	{Time, crm:'P80_end_is_qualified_by', ToDate},
	{Time, crm:'P78_is_identified_by', WhenDate}.  %TODO: make complex object?



educationstate_event
@@
{S, bgn:state, E},
{E, bgn:type, "education"},
{E, rdf:value, Val},
{E, bgn:to, ToDate} ?,
{E, bgn:from,FromDate} ?, % not used, todo
{E, bgn:when,WhenDate} ?
<=>
true,
literal_to_id(['Education-' ,Val], bgn, OURI), % first make the occupation instance
	{OURI, rdf:type, bgn:'Education'},
	{OURI, rdfs:label, Val},
	{S, bgn:has_education, OURI}, % TODO: fix correct property for this
make_rand_uri(S,['actorrole'],ARURI), %then make the actorrole instance
	{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	{ARURI, rdf:type, bioc:'Actor_Role'},
	{S,bioc:has_relation,ARURI},
make_rand_uri(S,['educationevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type,crm:'E5_Event'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant_in',ARURI},
make_rand_uri(S,['educationevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P79_beginning_is_qualified_by', FromDate},
	{Time, crm:'P80_end_is_qualified_by', ToDate},
	{Time, crm:'P78_is_identified_by', WhenDate}.  %TODO: make complex object?


residencestate_event
@@
{S, bgn:state, E},
{E, bgn:type, "residence"},
{E, rdf:value, Val},
{E, bgn:to, ToDate} ?,
{E, bgn:from,FromDate} ?, % not used, todo
{E, bgn:when,WhenDate} ?
<=>
true,
literal_to_id(['Residence-' ,Val], bgn, OURI), % first make the occupation instance
	{OURI, rdf:type, bgn:'Residence'},
	{OURI, rdfs:label, Val},
	{S, bgn:has_residence, OURI}, % TODO: fix correct property for this
make_rand_uri(S,['actorrole'],ARURI), %then make the actorrole instance
	{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	{ARURI, rdf:type, bioc:'Actor_Role'},
	{S,bioc:has_relation,ARURI},
make_rand_uri(S,['residenceevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type,crm:'E5_Event'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant_in',ARURI},
make_rand_uri(S,['residenceevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P79_beginning_is_qualified_by', FromDate},
	{Time, crm:'P80_end_is_qualified_by', ToDate},
	{Time, crm:'P78_is_identified_by', WhenDate}.  %TODO: make complex object?

%TODO: What is Floruit?
floruitstate_event
@@
{S, bgn:state, E},
{E, bgn:type, "floruit"},
{E, rdf:value, Val},
{E, bgn:to, ToDate} ?,
{E, bgn:from,FromDate} ?, % not used, todo
{E, bgn:when,WhenDate} ?
<=>
true,
literal_to_id(['Residence-' ,Val], bgn, OURI), % first make the occupation instance
	{OURI, rdf:type, bgn:'Floruit'},
	{OURI, rdfs:label, Val},
	{S, bgn:has_floruit, OURI}, % TODO: fix correct property for this
make_rand_uri(S,['actorrole'],ARURI), %then make the actorrole instance
	{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	{ARURI, rdf:type, bioc:'Actor_Role'},
	{S, bioc:has_relation,ARURI},
make_rand_uri(S,['floruitevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type,crm:'E5_Event'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant_in',ARURI},
make_rand_uri(S,['floruitevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P79_beginning_is_qualified_by', FromDate},
	{Time, crm:'P80_end_is_qualified_by', ToDate},
	{Time, crm:'P78_is_identified_by', WhenDate}.  %TODO: make complex object?

faithstate_event
@@
{S, bgn:state, E},
{E, bgn:type, "faith"},
{E, rdf:value, Val},
{E, bgn:to, ToDate} ?,
{E, bgn:from,FromDate} ?, % not used, todo
{E, bgn:when,WhenDate} ?
<=>
true,
literal_to_id(['Faith-' ,Val], bgn, OURI), % first make the occupation instance
	{OURI, rdf:type, bgn:'Faith'},
	{OURI, rdfs:label, Val},
	{S, bgn:has_faith, OURI}, % TODO: fix correct property for this
make_rand_uri(S,['actorrole'],ARURI), %then make the actorrole instance
	{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	{ARURI, rdf:type, bioc:'Actor_Role'},
	{S, bioc:has_relation,ARURI},
make_rand_uri(S,['faithevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type,crm:'E5_Event'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant_in',ARURI},
make_rand_uri(S,['faithevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P79_beginning_is_qualified_by', FromDate},
	{Time, crm:'P80_end_is_qualified_by', ToDate},
	{Time, crm:'P78_is_identified_by', WhenDate}.  %TODO: make complex object?


% Seems to be empty, for now delete, otherwise use commented stuff below
religionstate_event
@@
{_S, bgn:state, E},
{E, bgn:type, "religion"},
	{E,_P,_O}
<=>
true.

/*{E, rdf:value, Val},
{E, bgn:to, ToDate} ?,
{E, bgn:from,FromDate} ?, % not used, todo
{E, bgn:when,WhenDate} ?
<=>
true,
literal_to_id(['Faith-' ,Val], bgn, OURI), % first make the occupation instance
	{OURI, rdf:type, bgn:'Religion'},
	{OURI, rdfs:label, Val},
	{S, bgn:has_religion, OURI}, % TODO: fix correct property for this
make_rand_uri(S,['actorrole'],ARURI), %then make the actorrole instance
	{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	{S, bioc:has_relation,ARURI},
make_rand_uri(S,['religionevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type,crm:'E5_Event'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant_in',ARURI},
make_rand_uri(S,['religionevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P79_beginning_is_qualified_by', FromDate},
	{Time, crm:'P80_end_is_qualified_by', ToDate},
	{Time, crm:'P78_is_identified_by', WhenDate}.  %TODO: make complex object?
*/

claimtofamestate_event
@@
{S, bgn:state, E},
{E, bgn:type, "claim_to_fame"},
{E, rdf:value, Val},
{E, bgn:to, ToDate} ?,
{E, bgn:from,FromDate} ?, % not used, todo
{E, bgn:when,WhenDate} ?
<=>
true,
literal_to_id(['Faith-' ,Val], bgn, OURI), % first make the occupation instance
	{OURI, rdf:type, bgn:'Claim_to_fame'},
	{OURI, rdfs:label, Val},
	{S, bgn:has_claim_to_fame, OURI}, % TODO: fix correct property for this
make_rand_uri(S,['claimtofamerole'],ARURI), %then make the actorrole instance
	{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	{S, bioc:has_relation,ARURI},
	{ARURI, rdf:type, bioc:'Actor_Role'},
make_rand_uri(S,['claimtofameevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type,crm:'E5_Event'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant_in',ARURI},
make_rand_uri(S,['claimtofameevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P79_beginning_is_qualified_by', FromDate},
	{Time, crm:'P80_end_is_qualified_by', ToDate},
	{Time, crm:'P78_is_identified_by', WhenDate}.  %TODO: make complex object?

categorystate_event
@@
{S, bgn:state, E},
{E, bgn:type, "category"},
{E, rdf:value, Val},
{E, bgn:to, ToDate} ?,
{E, bgn:from,FromDate} ?, % not used, todo
{E, bgn:when,WhenDate} ?
<=>
true,
literal_to_id(['Faith-' ,Val], bgn, OURI), % first make the occupation instance
	{OURI, rdf:type, bgn:'Category'},
	{OURI, rdfs:label, Val},
	{S, bgn:has_category, OURI}, % TODO: fix correct property for this
make_rand_uri(S,['categoryrole'],ARURI), %then make the actorrole instance
	{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	{ARURI, rdf:type, bioc:'Actor_Role'},
	{S, bioc:has_relation,ARURI},
make_rand_uri(S,['categoryevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type,crm:'E5_Event'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant_in',ARURI},
make_rand_uri(S,['categoryevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P79_beginning_is_qualified_by', FromDate},
	{Time, crm:'P80_end_is_qualified_by', ToDate},
	{Time, crm:'P78_is_identified_by', WhenDate}.  %TODO: make complex object?





% RELATIONS
%

personrelation
@@
{S, bgn:relation, R},
{R, bgn:name, Type},
{R, bgn:active, Code}
<=>
make_rand_uri(S,['-relation'],RelURI),
rdf(A, bgn:id, Code),
{S, bioc:has_family_relation, RelURI},
{RelURI, rdf:type, bioc:'Family_Relation'},
{RelURI, crm:'P2_has_type', Type},
{RelURI, bioc:inheres_in, A}.




% IDNO to IDENTIFIER
%
%

idno
@@
{S, rdf:type, 'E21_Person'}\
{S, bgn:idno, ID},
{ID, bgn:type, Type}?,
{ID, rdf:value, Val}?
<=>
make_rand_uri(S,['-id'],IDURI),
{S, bioc:'P48_has_preferred_identifier', IDURI},
{IDURI, rdf:type, crm:'E42_Identifier'},
{IDURI, crm:'P2_has_type', Type},
{IDURI, crm:'P3_has_note', Val}.




% OLD TODO


% bgn:snippet on bgn:BioParts generally points to a resource of type bgn:Snippet giving bgn:sourceId
% bgn:text instead of bgn:snippet on bgn:BioParts
% bgn:figure points to resource of type bgn:Figure giving a bgn:graphic and a bgn:head (=caption); bgn:graphic points to resource of type bgn:Graphic giving bgn:url











