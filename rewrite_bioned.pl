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
{S, rdf:type, ore:'Aggregation'}.
%{S, rdf:type, prov:'Entity'},
%{S, rdf:type, pplan:'Entity'}.



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
%{S, edm:aggregatedCHO, P},
{P, bgn:personID, literal(PersonID)}, %leave this for now, could be cidoc
{P, rdf:type, idm:'Provided_Person'}.

addproxyrelations
@@
{S, bgn:hasPersonDes, O},
{S, bgn:aggregatedPerson, P}
==>
true,
{O, ore:proxyIn, S},
%{O, ore:proxyFor, P},
{O, idm:person_proxy_for,P}.





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
{S, bioc:has_gender, Gender},
	{Gender, rdf:type, bioc:'Gender'} .


sextogender
@@
{S, bgn:sex, O},
{O, bgn:value, literal(Sex)}
<=>
true,
sex_to_gender(Sex,Gender),
{S, bioc:has_gender, Gender},
	{Gender, rdf:type, bioc:'Gender'} .



% NATIONALITY
% changed to idm rather than bioc
nationality
@@
{S, rdf:type, crm:'E21_Person'}
==>
{S, bioc:has_nationality, bioc:dutch}.

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
{E, rdf:type, bgn:'Event'},
{E, bgn:type, "birth"},
	{E, bgn:date, Date} ?,
	{E, bgn:when, WhenDate} ?,
	{E, bgn:notAfter, NA} ?, % TODO figure out how to do these
	{E, bgn:notBefore,NB} ?, % not used, todo
	{E, bgn:place, Place}?,
	{E, rdf:value, Value}?
<=>
true,
make_rand_uri(S,['-birth'],Evt),
{Evt, crm:'P98_brought_into_life', S},
{Evt, rdf:type, crm:'E67_Birth'},
% {Evt, crm:'P2_has_type', bgn:birth}, Now no longer uses p2, but rdf
% type
{Evt, rdfs:label, Value}, %use Label for now
{Evt, crm:'P7_took_place_at', Place},
make_rand_uri(S,['-birth-time'],Time), % make the time t instance
{Evt, crm:'P4_has_time-span', Time},
{Time, rdf:type, crm:'E52_Time-Span'},
{Time, rdfs:label, Date},
{Time, rdfs:label, WhenDate},
	{Time,  bgn:notAfter, NA},
	{Time,  bgn:notBefore, NB}.


deathevent
@@
{E, bgn:type, " death"}
<=>
{E, bgn:type, "death"}.



deathevent
@@
{S, bgn:event, E},
{E, rdf:type, bgn:'Event'},
{E, bgn:type, "death"},
	{E, bgn:date, Date} ?,
	{E, bgn:when, WhenDate} ?,
	{E, bgn:notAfter, NA} ?,
	{E, bgn:notBefore,NB} ?,
	{E, bgn:place, Place}?,
	{E, rdf:value, Value}?
<=>
true,
make_rand_uri(S,['-death'],Evt),
{Evt, crm:'P100_was_death_of', S},
{Evt, rdf:type, bgn:'Death'},
%{Evt, rdf:type, crm:'E5_Event'},
%{Evt, crm:'P2_has_type', bgn:death},
{Evt, rdfs:label, Value}, %use Label for now
{Evt, crm:'P7_took_place_at', Place},
%{Evt, crm:'P4_has_time-span', Date},
	make_rand_uri(S,['-death-time'],Time), % make the time t instance
{Evt, crm:'P4_has_time-span', Time},
{Time, rdf:type, crm:'E52_Time-Span'},
{Time, crm:'P78_is_identified_by', Date},
{Time, rdfs:label, WhenDate},
	{Time,  bgn:notAfter, NA},
	{Time,  bgn:notBefore, NB}.


baptismevent
@@
{S, bgn:event, E},
{E, rdf:type, bgn:'Event'},
{E, bgn:type, "baptism"},
	{E, bgn:date, Date} ?,
	{E, bgn:when, WhenDate} ?,
	{E, bgn:notAfter, NA} ?,
	{E, bgn:notBefore,NB} ?,
	{E, bgn:place, Place}?,
	{E, rdf:value, Value}?
<=>
true,
make_rand_uri(S,['-baptism'],Evt),
{Evt, crm:'P11_had_participant', S},
{Evt, rdf:type, bgn:'Baptism'},
%{Evt, rdf:type, crm:'E5_Event'},
%{Evt, crm:'P2_has_type', bgn:baptism},
{Evt, rdfs:label, Value}, %use Label for now
{Evt, crm:'P7_took_place_at', Place},
%{Evt, crm:'P4_has_time-span', Date},
	make_rand_uri(S,['-baptism-time'],Time), % make the time t instance
{Evt, crm:'P4_has_time-span', Time},
{Time, rdf:type, crm:'E52_Time-Span'},
{Time, rdfs:label, Date},
{Time, rdfs:label, WhenDate},
	{Time,  bgn:notAfter, NA},
	{Time,  bgn:notBefore, NB}.


marriageevent
@@
{S, bgn:event, E},
{E, rdf:type, bgn:'Event'},
{E, bgn:type, "marriage"},
	{E, bgn:date, Date} ?,
	{E, bgn:when, WhenDate} ?,
	{E, bgn:notAfter, NA} ?,
	{E, bgn:notBefore,NB} ?,
	{E, bgn:place, Place}?,
	{E, rdf:value, Value}?
<=>
true,
make_rand_uri(S,['marriage'],Evt),
{Evt, crm:'P11_had_participant', S},
{Evt, rdf:type, bgn:'Marriage'},

%{Evt, rdf:type, crm:'E5_Event'},
%{Evt, crm:'P2_has_type', bgn:marriage},
{Evt, rdfs:label, Value}, %use Label for now
{Evt, crm:'P7_took_place_at', Place},
%{Evt, crm:'P4_has_time-span', Date},
	make_rand_uri(S,['-marriage-time'],Time), % make the time t instance
{Evt, crm:'P4_has_time-span', Time},
{Time, rdf:type, crm:'E52_Time-Span'},
{Time, rdfs:label, Date},
{Time, rdfs:label, WhenDate},
	{Time,  bgn:notAfter, NA},
	{Time,  bgn:notBefore, NB}.

funeralevent
@@
{S, bgn:event, E},
{E, rdf:type, bgn:'Event'},
{E, bgn:type, "funeral"},
	{E, bgn:date, Date} ?,
	{E, bgn:when, WhenDate} ?,
	{E, bgn:notAfter,NA} ?,
	{E, bgn:notBefore,NB} ?,
	{E, bgn:place, Place}?,
	{E, rdf:value, Value}?
<=>
true,
make_rand_uri(S,['funeral'],Evt),
{Evt, crm:'P11_had_participant', S},
{Evt, rdf:type, bgn:'Funeral'},
%{Evt, rdf:type, crm:'E5_Event'},
%{Evt, crm:'P2_has_type', bgn:funeral},
{Evt, rdfs:label, Value}, %use Label for now
{Evt, crm:'P7_took_place_at', Place},
%{Evt, crm:'P4_has_time-span', Date},
	make_rand_uri(S,['-funeral-time'],Time), % make the time t instance
{Evt, crm:'P4_has_time-span', Time},
{Time, rdf:type, crm:'E52_Time-Span'},
{Time, rdfs:label, Date},
{Time, rdfs:label, WhenDate},
	{Time,  bgn:notAfter, NA},
	{Time,  bgn:notBefore, NB}.


restevent %TODO
@@
{S, bgn:event, E},
{E, rdf:type, bgn:'Event'},
{E, bgn:type, Other},
	{E, bgn:date, Date} ?,
	{E, bgn:when, WhenDate} ?,
	{E, bgn:notAfter,NA} ?,
	{E, bgn:notBefore,NB} ?,
	{E, bgn:place, Place}?,
	{E, rdf:value, Value}?
<=>
true,
make_rand_uri(S,['-other'],Evt),
{Evt, crm:'P11_had_participant', S},
{Evt, rdf:type, crm:'E5_Event'},
{Evt, crm:'P2_has_type', Other},
{Evt, rdfs:label, Value}, %use Label for now
{Evt, crm:'P7_took_place_at', Place},
%{Evt, crm:'P4_has_time-span', Date},
	make_rand_uri(S,['-other-time'],Time), % make the time t instance
{Evt, crm:'P4_has_time-span', Time},
{Time, rdf:type, crm:'E52_Time-Span'},
{Time, rdfs:label, Date},
{Time, rdfs:label, WhenDate},
	{Time,  bgn:notAfter, NA},
	{Time,  bgn:notBefore, NB}.


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
	{OURI, rdf:type, bioc:'Occupation'},
	{OURI, rdfs:label, Val}, % previously prefLabel
%	{OURI, rdfs:label, Val},
	{S, bioc:has_occupation, OURI},
	% make_rand_uri(S,['-actorrole'],ARURI), %NO longer needed
        %{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	%{ARURI, rdf:type, bioc:'Actor_Role'},
	%{S, bioc:bearer_of,ARURI},
make_rand_uri(S,['-occupationevent'],Evt), %then make the Event instance
%	{Evt, rdf:type,crm:'E5_Event' },
	{Evt, rdf:type, bgn:'OccupationStateEvent'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant',S},
	{Evt, bioc:had_participant_in_role, OURI},
make_rand_uri(S,['-occupationevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P82a_begin_of_the_begin', FromDate},
	{Time, crm:'P82b_end_of_the_end', ToDate},
	{Time, rdfs:label, WhenDate}.  %TODO: make complex object?



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
	{OURI, rdf:type, bioc:'Education'},
	{OURI, rdfs:label, Val},
	{S, bgn:has_education, OURI}, % TODO: fix correct property for this
% make_rand_uri(S,['-actorrole'],ARURI),
%	{ARURI, bgn:roletype,OURI}, %	{ARURI, rdf:type, bioc:'Actor_Role'},
%	{S, bioc:bearer_of,ARURI},
make_rand_uri(S,['-educationevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type, bgn:'EducationStateEvent'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant',S},
	{Evt, bioc:had_participant_in_role, OURI},
make_rand_uri(S,['-educationevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P82a_begin_of_the_begin', FromDate},
	{Time, crm:'P82b_end_of_the_end', ToDate},
	{Time, rdfs:label, WhenDate}.  %TODO: make complex object?

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
	{S, bgn:has_residence, OURI},
       %make_rand_uri(S,['-actorrole'],ARURI), %then make the actorrole instance
	%{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	%{ARURI, rdf:type, bioc:'Actor_Role'},
	%{S, bioc:bearer_of,ARURI},
make_rand_uri(S,['-residenceevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type, bgn:'ResidenceStateEvent'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant',S},
	{Evt, bioc:had_participant_in_role, OURI},
make_rand_uri(S,['-residenceevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P82a_begin_of_the_begin', FromDate},
	{Time, crm:'P82b_end_of_the_end', ToDate},
	{Time, rdfs:label, WhenDate}.  %TODO: make complex object?


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
% make_rand_uri(S,['-actorrole'],ARURI), %then make the actorrole
% instance
	%{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	%{ARURI, rdf:type, bioc:'Actor_Role'},
	%{S, bioc:bearer_of,ARURI},
make_rand_uri(S,['-floruitevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type, bgn:'FloruitStateEvent'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant',S},
	{Evt, bioc:had_participant_in_role, OURI},
make_rand_uri(S,['-floruitevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P82a_begin_of_the_begin', FromDate},
	{Time, crm:'P82b_end_of_the_end', ToDate},
	{Time, rdfs:label, WhenDate}.  %TODO: make complex object?

% DELETE empty floruit?
floruitstate_event
@@
{_, bgn:state, E},
{E, bgn:type, "floruit"},
{E, _,_}
<=>
true.

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
% make_rand_uri(S,['-actorrole'],ARURI), %then make the actorrole
% instance
%	{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for
%	this
%	{ARURI, rdf:type, bioc:'Actor_Role'},
%	{S, bioc:bearer_of,ARURI},
make_rand_uri(S,['-faithevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type, bgn:'FaithStateEvent'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant',S},
	{Evt, bioc:had_participant_in_role, OURI},
make_rand_uri(S,['-faithevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P82a_begin_of_the_begin', FromDate},
	{Time, crm:'P82b_end_of_the_end', ToDate},
	{Time, rdfs:label, WhenDate}.  %TODO: make complex object?


% Seems to be empty, for now delete, otherwise use commented stuff below
religionstate_event
@@
{_, bgn:state, E},
{E, bgn:type, "religion"},
	{E,_,_}
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
	{Evt, crm:'P11_had_participant',ARURI},
make_rand_uri(S,['religionevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P79_beginning_is_qualified_by', FromDate},
	{Time, crm:'P80_end_is_qualified_by', ToDate},
	{Time, rdfs:label, WhenDate}.  %TODO: make complex object?
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
% make_rand_uri(S,['-claimtofamerole'],ARURI), %then make the actorrole
% instance
	%{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	%{S, bioc:bearer_of,ARURI},
	%{ARURI, rdf:type, bioc:'Actor_Role'},
make_rand_uri(S,['-claimtofameevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type, bgn:'ClaimtofameStateEvent'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant',S},
	{Evt, bioc:had_participant_in_role, OURI},
make_rand_uri(S,['-claimtofameevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P82a_begin_of_the_begin', FromDate},
	{Time, crm:'P82b_end_of_the_end', ToDate},
	{Time, rdfs:label, WhenDate}.  %TODO: make complex object?

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
literal_to_id(['Category-' ,Val], bgn, OURI), % first make the occupation instance
	{OURI, rdf:type, bgn:'Category'},
	{OURI, rdfs:label, Val},
	{S, bgn:has_category, OURI}, % TODO: fix correct property for this
% make_rand_uri(S,['-categoryrole'],ARURI), %then make the actorrole
% instance
	%{ARURI, bgn:roletype,OURI}, % TODO: fix correct property for this
	%{ARURI, rdf:type, bioc:'Actor_Role'},
	%{S, bioc:bearer_of,ARURI},
make_rand_uri(S,['-categoryevent'],Evt), %then make the actorroleevent instance
	{Evt, rdf:type, bgn:'CategoryStateEvent'},
	{Evt, rdfs:label, Val},
	{Evt, crm:'P11_had_participant',S},
	{Evt, bioc:had_participant_in_role, OURI},
make_rand_uri(S,['-categoryevent_time'],Time), % then make the Event instance
	{Time, rdf:type, crm:'E52_Time-Span'},
	{Evt, crm:'P4_has_time-span', Time},
	{Time, crm:'P82a_begin_of_the_begin', FromDate},
	{Time, crm:'P82b_end_of_the_end', ToDate},
	{Time, rdfs:label, WhenDate}.  %TODO: make complex object?





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


% PLACE Literals to resources
place_to_resource
@@
{Evt, crm:'P7_took_place_at', Place}
<=>
literal_to_id(['place-' ,Place], bgn, OURI),
{OURI, rdf:type, crm:'E53_Place'},
{OURI, rdfs:label, Place},
{Evt, crm:'P7_took_place_at', OURI}.

place_appellation
@@
{OURI, rdf:type, crm:'E53_Place'},
{OURI, rdfs:label, Place}
==>
literal_to_id(['place-appellation-' ,Place], bgn, APURI),
{OURI, crm:'P1_is_identified_by', APURI},
{APURI, rdf:type, crm:'E41_Appellation'},
{APURI, rdfs:label, Place}.


% IDNO to IDENTIFIER
%
%

idno
@@
{S, rdf:type, crm:'E21_Person'}\
{S, bgn:idno, ID},
{ID, bgn:type, Type}?,
{ID, rdf:value, Val}?
<=>
make_rand_uri(S,['-id'],IDURI),
{S, crm:'P48_has_preferred_identifier', IDURI},
{IDURI, rdf:type, crm:'E42_Identifier'},
{IDURI, crm:'P2_has_type', Type},
{IDURI, rdfs:label, Val}.


% CLEAN

clean_empty
@@
{_,_,""}
<=>
true.

clean_empty_timespans
@@
{_,crm:'P4_has_time-span',T},
{T, rdf:type, crm:'E52_Time-Span'}
<=>
not((   rdf(T,P,_),
P\= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type')),
true.

clean_bioport
@@
{_,rdf:type,bgn:'Bioport'}
<=>
true.

clean_sex
@@
{_,rdf:type,bgn:'Sex'}
<=>
true.

clean_state
@@
{S,rdf:type,bgn:'State'},
{S,bgn:idno,_}
<=>
true.

%TODO: map to idm
bnodes_to_randids
@@
{S,_,_}\
{S}
<=>
rdf_is_bnode(S),
make_rand_uri_bnode(URI),
{URI}.


% OLD TODO


% bgn:snippet on bgn:BioParts generally points to a resource of type bgn:Snippet giving bgn:sourceId
% bgn:text instead of bgn:snippet on bgn:BioParts
% bgn:figure points to resource of type bgn:Figure giving a bgn:graphic and a bgn:head (=caption); bgn:graphic points to resource of type bgn:Graphic giving bgn:url











