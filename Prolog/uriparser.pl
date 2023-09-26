:- use_module(library(lists)).

is_uri(UriReference) :-
    (is_abs_uri(UriReference) ;
        is_rel_uri(UriReference)
    ),
    !.
is_uri(UriReference) :-
    split_string(UriReference, "#", "", [Uri, Fragment]),
    (is_abs_uri(Uri) ;
        is_rel_uri(Uri)
    ),
    is_fragment(Fragment).

is_rel_uri("") :- !, false.
is_rel_uri(Uri) :-
    split_string(Uri, "?", "", [Path, Query]),
    (is_netpath(Path) ;
        is_abs_path(Path),
        is_rel_path(Path)
    ),
    is_query(Query),
    !.
is_rel_uri(Uri) :-
    (is_netpath(Uri) ;
        is_abs_path(Uri) ;
        is_rel_path(Uri)
    ).

is_abs_uri(Uri) :-
    Uri \= "",
    split_string(Uri, ":", "", [Scheme, Part]),
    is_scheme(Scheme),
    (is_hier_part(Part) ;
        is_opaque_part(Part)
    ).

is_opaque_part("") :- !, false.
is_opaque_part(Char) :-
    string_length(Char, 1),
    !,
    is_uric_no_slash(Char).
is_opaque_part(OpaquePart) :-
    string_chars(OpaquePart, [FirstChar | Rest]),
    is_uric_no_slash(FirstChar),
    !,
    string_chars(RestStr, Rest),
    is_query(RestStr).    %not because is query, but use the same code as a query
is_opaque_part(OpaquePart) :-
    string_chars(OpaquePart, [First, Second, Third | Rest]),
    string_chars(Escaped, [First, Second, Third]),
    is_uric_no_slash(Escaped),
    string_chars(RestStr, Rest),
    is_query(RestStr).     %same as upper predicate

is_rel_path("") :- !, false.
is_rel_path(Segment) :-
    is_rel_segment(Segment),
    !.
is_rel_path(Path) :-
    string_concat(Segment, AbsPath, Path),
    is_segment(Segment),
    is_abs_path(AbsPath).

is_rel_segment("") :- !, false.
is_rel_segment(Char) :-
    string_length(Char, 1),
    !,
    (Char == ";" ;
        Char == "@" ;
        Char == "&" ;
        Char == "=" ;
        Char == "+" ;
        Char == "$" ;
        Char == "," ;
        is_unreserved(Char)).
is_rel_segment(Segment) :-
    is_escaped(Segment),
    !.
is_rel_segment(Segment) :-
    string_chars(Segment, [FirstChar, Rest]),
    string_chars(FirstCharStr, [FirstChar]),
    string_chars(RestStr, Rest),
    is_rel_segment(FirstCharStr),
    is_rel_segment(RestStr).

is_hier_part("") :- !, false.
is_hier_part(HierPart) :-
    (is_netpath(HierPart) ;
        is_abs_path(HierPart)
    ),
    !.
is_hier_part(HierPart) :-
    split_string(HierPart, "?", "", [Path, Query]),
    (is_netpath(Path) ;
        is_abs_path(Path)
    ),
    is_query(Query).

is_netpath("") :- !, false.
is_netpath(Path) :-
    string_concat("//", Authority, Path),
    is_authority(Authority),
    !.
is_netpath(Path) :-
    string_concat("//", Rest, Path),
    string_concat(Authority, AbsPath, Rest),
    is_authority(Authority),
    is_abs_path(AbsPath).

is_scheme("") :- !, false.
is_scheme(Char) :-
    string_length(Char, 1),
    !,
    is_alpha(Char).
is_scheme(Scheme) :-
    string_chars(Scheme, [FirstChar | SchemeList]),
    is_alpha(FirstChar),
    foreach(member(Char, SchemeList), (is_alnum(Char) ;
                                        Char == "+" ;
                                        Char == "-" ;
                                        Char == ".")).

is_authority("") :- !, false.
is_authority(Authority) :-
    (is_serverbased(Authority) ;
        is_regbased(Authority)).

is_regbased("") :- !, false.
is_regbased(Char) :-
    string_length(Char, 1),
    !,
    (Char == "$" ;
        Char == "," ;
        Char == ";" ;
        Char == ":" ;
        Char == "@" ;
        Char == "&" ;
        Char == "=" ;
        Char == "+" ;
        is_unreserved(Char)).
is_regbased(RegBased) :-
    is_escaped(RegBased),
    !.
is_regbased(RegBased) :-
    string_chars(RegBased, [FirstChar | Rest]),
    string_chars(RestStr, Rest),
    is_regbased(FirstChar),
    is_regbased(RestStr).


is_serverbased("") :- !, false.
is_serverbased(ServerBased) :-
    split_string(ServerBased, "@", "", [UserInfo, HostPort]),
    !,
    is_userinfo(UserInfo),
    is_hostport(HostPort).
is_serverbased(ServerBased) :-
    is_hostport(ServerBased).
    
is_userinfo("") :- !.
is_userinfo(Char) :-
    string_length(Char, 1),
    !,
    (Char == ";" ;
        Char == ":" ;
        Char == "&" ;
        Char == "=" ;
        Char == "+" ;
        Char == "$" ;
        Char == "," ;
        is_unreserved(Char)).
is_userinfo(UserInfo) :-
    is_escaped(UserInfo),
    !.
is_userinfo(UserInfo) :-
    string_concat(FirstChar, Rest, UserInfo),
    is_userinfo(FirstChar),
    is_userinfo(Rest).

is_hostport("") :- !, false.
is_hostport(HostPort) :-
    split_string(HostPort, ":", "", [Host, Port]),
    !,
    is_host(Host),
    is_port(Port).
is_hostport(Host) :-
    is_host(Host).

is_host(Hostname) :-
    Hostname \= "",
    (is_hostname(Hostname) ;
        is_ipaddress(Hostname)).


is_hostname(Hostname) :-
    string_concat(Pre, ".", Hostname),
    !,
    is_hostname(Pre).
is_hostname(Hostname) :-
    not(sub_string(Hostname, _, _, _, ".")),
    !,
    is_toplabel(Hostname).
is_hostname(Hostname) :-        %QUI POSSONO ESSERCI ERRORI CON INDICE E LUNGHEZZA
    split_string(Hostname, ".", "", [DomainLabel | _]),
    is_domain_label(DomainLabel),
    !,
    string_length(DomainLabel, Index),
    string_length(Hostname, Tmp),
    Length is Tmp - Index,
    sub_string(Hostname, Index, Length, _, Rest),
    is_hostname(Rest).
    
is_domain_label(Char) :-
    string_length(Char, 1),
    !,
    is_alnum(Char).
is_domain_label(Label) :-
    string_chars(Label, [FirstChar, SecondChar]),
    !,
    is_alnum(FirstChar),
    is_alnum(SecondChar).
is_domain_label(Label) :-
    string_chars(Label, [FirstChar | Rest]),
    last(Rest, LastChar),
    is_alnum(FirstChar),
    is_alnum(LastChar),
    foreach(member(Char, Rest), (is_alnum(Char) ;
                                        Char == "-")).

is_toplabel(Char) :-
    string_length(Char, 1),
    !,
    is_alpha(Char).
is_toplabel(Label) :-
    string_chars(Label, [FirstChar, SecondChar]),
    !,
    is_alpha(FirstChar),
    is_alnum(SecondChar).
is_toplabel(Label) :-
    string_chars(Label, [FirstChar | Rest]),
    last(Rest, LastChar),
    is_alpha(FirstChar),
    is_alnum(LastChar),
    foreach(member(Char, Rest), (is_alnum(Char) ;
                                        Char == "-")).

is_ipaddress(Ipv4) :-
    Ipv4 \= "",
    split_string(Ipv4, ".", "", [First, Second, Third, Fourth]),
    foreach(member(Octet, [First, Second, Third, Fourth]),
            is_octet(Octet)).

is_octet(Octet) :-
    string_length(Octet, Length), Length > 0, Length < 4,
    number_string(OctetNumber, Octet),
    integer(OctetNumber).

is_port(Port) :-
    number_string(PortNumber, Port),
    integer(PortNumber).

is_path(Path) :-
    Path \= "",
    (is_abs_path(Path) ;
        is_opaque_part(Path)).

is_abs_path(AbsPath) :-
    AbsPath \= "",
    string_concat("/", Segments, AbsPath),
    split_string(Segments, "/", "", [Segment | SegmentsList]),
    Segment \= "",
    foreach(member(S, [Segment | SegmentsList]), is_segment(S)).

is_segment("").
is_segment(Segment) :-
    split_string(Segment, ";", "", ParamList),
    foreach(member(P, ParamList), is_param(P)).

is_param("").
is_param(Param) :-
    string_chars(Param, ParamList),
    foreach(member(Char, ParamList), is_pchar(Char)).

is_query("").
is_query(Query) :-
    string_chars(Query, [FirstChar | Rest]),
    string_chars(RestStr, Rest),
    is_uric(FirstChar),
    !,
    is_query(RestStr).
is_query(Query) :-
    string_chars(Query, [First, Second, Third | Rest]),
    string_chars(Escaped, [First, Second, Third]),
    string_chars(RestStr, Rest),
    is_uric(Escaped),
    is_query(RestStr).

is_fragment("").
is_fragment(Fragment) :-
    string_chars(Fragment, [FirstChar | Rest]),
    string_chars(RestStr, Rest),
    is_uric(FirstChar),
    !,
    is_fragment(RestStr).
is_fragment(Fragment) :-
    string_chars(Fragment, [First, Second, Third | Rest]),
    string_chars(Escaped, [First, Second, Third]),
    string_chars(RestStr, Rest),
    is_uric(Escaped),
    is_fragment(RestStr).

is_uric(Char) :-
    string_length(Char, 1),
    !,
    (is_reserved(Char) ;
        is_unreserved(Char)).
is_uric(Escaped) :-
    is_escaped(Escaped).

is_uric_no_slash(Char) :-
    string_length(Char, 1),
    !,
    (Char == ";" ;
        Char == "?" ;
        Char == ":" ;
        Char == "@" ;
        Char == "&" ;
        Char == "=" ;
        Char == "+" ;
        Char == "$" ;
        Char == "," ;
        is_unreserved(Char)).
is_uric_no_slash(Escaped) :-
    is_escaped(Escaped).

is_pchar(Char) :-
    (Char == ':' ;
        Char == '@' ;
        Char == '&' ;
        Char == '=' ;
        Char == '+' ;
        Char == '$' ;
        Char == ',' ;
        is_unreserved(Char) ;
        is_escaped(Char)
    ).

is_reserved(Char) :-
    (Char == ';' ;
        Char == '/' ;
        Char == '?' ;
        Char == ':' ;
        Char == '@' ;
        Char == '&' ;
        Char == '=' ;
        Char == '+' ;
        Char == '$' ;
        Char == ',').

is_unreserved(Char) :-
    (is_alnum(Char) ;
        is_mark(Char)).

is_mark(Char) :-
    (Char == '-' ;
        Char == '_' ;
        Char == '.' ;
        Char == '!';
        Char == '~' ;
        Char == '*' ;
        Char == "'" ;
        Char == '(' ;
        Char == ')').

is_escaped(String) :- 
    string_length(String, 3),
    string_chars(String, ["%", FirstHex, SecondHex]),
    is_hex_digit(FirstHex),
    is_hex_digit(SecondHex).
is_hex_digit(Char) :-
    (is_digit(Char) ;
        Char == 'A' ;
        Char == 'B' ;
        Char == 'C' ;
        Char == 'D' ;
        Char == 'E' ;
        Char == 'F' ;
        Char == 'a' ;
        Char == 'b' ;
        Char == 'c' ;
        Char == 'd' ;
        Char == 'e' ;
        Char == 'f').

loop(0).
loop(N) :-
    is_uri('https://www.example.com/path/to;param?query#fragment'),
    Tmp is N - 1,
    loop(Tmp).

:- begin_tests(is_uri).

test(loop) :-
    loop(100).

:- end_tests(is_uri).
