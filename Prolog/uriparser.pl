is_uri(UriReference) :-
    (is_abs_uri(UriReference) ;
        is_rel_uri(UriReference)
    ).
is_uri(UriReference) :-
    split_string(UriReference, '#', "", [Uri, Fragment]),
    (is_abs_uri(Uri) ;
        is_rel_uri(Uri)
    ),
    is_fragment(Fragment).

is_rel_uri(Uri) :-
    (is_netpath(Uri) ;
        is_abs_path(Uri) ;
        is_rel_path(Uri)
    ).
is_rel_uri(Uri) :-
    split_string(Uri, '?', "", [Path, Query]),
    (is_netpath(Path) ;
        is_abs_path(Path),
        is_rel_path(Path)
    ),
    is_query(Query).

is_abs_uri(Uri) :-
    split_string(Uri, ":", "", [Scheme, Part]),
    is_scheme(Scheme),
    (is_hier_part(Part) ;
        is_opaque_part(Part)
    ).

is_opaque_part(Char) :-
    is_uric_no_slash(Char).
is_opaque_part(OpaquePart) :-
    string_chars(OpaquePart, [FirstChar | OpaqueList]),
    is_uric_no_slash(FirstChar),
    foreach(member(Char, OpaqueList), is_uric(Char)).

is_uric_no_slash(Char) :-
    (Char == ';' ;
        Char == '?' ;
        Char == ':' ;
        Char == '@' ;
        Char == '&' ;
        Char == '=' ;
        Char == '+' ;
        Char == '$' ;
        Char == ',' ;
        is_unreserved(Char) ;
        is_escaped(Char)
    ).

is_rel_path(Segment) :-
    is_rel_segment(Segment).
is_rel_path(Path) :-
    string_concat(Segment, AbsPath, Path),
    is_segment(Segment),
    is_abs_path(AbsPath).

is_rel_segment(Char) :-
    (Char == ';' ;
        Char == '@' ;
        Char == '&' ;
        Char == '=' ;
        Char == '+' ;
        Char == '$' ;
        Char == ',' ;
        is_unreserved(Char) ;
        is_escaped(Char)
    ).
is_rel_segment(Segment) :-
    string_concat(FirstChar, Rest, Segment),
    is_rel_segment(FirstChar),
    is_rel_segment(Rest).

is_hier_part(HierPart) :-
    (is_netpath(HierPart) ;
        is_abs_path(HierPart)
    ).
is_hier_part(HierPart) :-
    split_string(HierPart, '?', "", [Path, Query]),
    (is_netpath(Path) ;
        is_abs_path(Path)
    ),
    is_query(Query).

is_netpath(Path) :-
    string_concat("//", Authority, Path),
    is_authority(Authority).
is_netpath(Path) :-
    string_concat("//", Rest, Path),
    string_concat(Authority, AbsPath, Rest),
    is_authority(Authority),
    is_abs_path(AbsPath).

is_scheme(Scheme) :-
    string_chars(Scheme, [Char]),
    char_type(Char, alpha).
is_scheme(Scheme) :-
    string_chars(Scheme, [FirstChar | SchemeList]),
    char_type(FirstChar, alpha),
    foreach(member(Char, SchemeList), (char_type(Char, alnum) ;
                                        Char == '+' ;
                                        Char == '-' ;
                                        Char == '.')).

is_authority(Authority) :-
    is_serverbased(Authority).
is_authority(Authority) :-
    is_regbased(Authority).

is_regbased(RegBased) :-
    string_chars(RegBased, RegBasedList),
    length(RegBasedList, Length), Length > 0,
    foreach(member(Char, RegBasedList), (Char == '$' ;
                                            Char == ',' ;
                                            Char == ';' ;
                                            Char == ':' ;
                                            Char == "@" ;
                                            Char == "&" ;
                                            Char == "=" ;
                                            Char == "+" ;
                                            is_unreserved(Char) ;
                                            is_escaped(Char))).

is_serverbased(ServerBased) :-
    is_hostport(ServerBased).
is_serverbased(ServerBased) :-
    string_concat(Tmp, Hostport, ServerBased),
    string_concat(UserInfo, '@', Tmp),
    is_userinfo(UserInfo),
    is_hostport(Hostport).
    
is_userinfo("").
is_userinfo(UserInfo) :-
    string_chars(UserInfo, UserInfoList),
    foreach(member(Char, UserInfoList), (Char == ';' ;
                                            Char == ':' ;
                                            Char == '&' ;
                                            Char == '=' ;
                                            Char == '+' ;
                                            Char == '$' ;
                                            Char == ',' ;
                                            is_unreserved(Char) ;
                                            is_escaped(Char))).

is_hostport(Host) :-
    is_host(Host).
is_hostport(HostPort) :-
    string_concat(Tmp, Port, HostPort),
    string_concat(Host, ':', Tmp),
    is_host(Host),
    is_port(Port).

is_host(Hostname) :-
    is_hostname(Hostname).
is_host(Ipv4) :-
    is_ipaddress(Ipv4).

%DA SISTEMARE
is_hostname(Hostname) :-
    is_toplabel(Hostname).
is_hostname(Hostname) :-
    string_concat(TopLabel, '.', Hostname),
    is_toplabel(TopLabel).
is_hostname(Hostname) :-
    string_concat(DomainLabel, '.', Tmp),
    string_concat(Tmp, Rest, Hostname),
    is_domain_label(DomainLabel),
    is_hostname(Rest).
    

is_domain_label(Char) :-
    char_type(Char, alnum).
is_domain_label(Label) :-
    string_concat(FirstChar, SecondChar, Label),
    char_type(FirstChar, alnum),
    char_type(SecondChar, alnum).
is_domain_label(Label) :-
    string_concat(FirstChar, Rest, Tmp),
    string_concat(Tmp, LastChar, Label),
    char_type(FirstChar, alnum),
    char_type(LastChar, alnum),
    string_chars(Rest, RestList),
    foreach(member(Char, RestList), (char_type(Char, alnum) ;
                                        Char == '-')).

is_toplabel(Char) :-
    char_type(Char, alpha).
is_toplabel(Label) :-
    string_concat(FirstChar, SecondChar, Label),
    char_type(FirstChar, alpha),
    char_type(SecondChar, alnum).
is_toplabel(Label) :-
    string_concat(FirstChar, Rest, Tmp),
    string_concat(Tmp, LastChar, Label),
    char_type(FirstChar, alpha),
    char_type(LastChar, alnum),
    string_chars(Rest, RestList),
    foreach(member(Char, RestList), (char_type(Char, alnum) ;
                                        Char == '-')).

is_ipaddress(Ipv4) :-
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
    is_abs_path(Path).
is_path(Path) :-
    is_opaque_part(Path).

is_abs_path(AbsPath) :-
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
    string_chars(Query, QueryList),
    foreach(member(Char, QueryList), is_uric(Char)).

is_fragment("").
is_fragment(Fragment) :-
    string_chars(Fragment, FragmentList),
    foreach(member(Char, FragmentList), is_uric(Char)).

is_uric(Char) :-
    (is_reserved(Char) ;
        is_unreserved(Char) ;
        is_escaped(Char)
    ).

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
    (char_type(Char, alnum) ;
        is_mark(Char)).

is_mark(Char) :-
    (Char == '-' ;
        Char == '_' ;
        Char == '.' ;
        Char == '!' ;
        Char == '~' ;
        Char == '*' ;
        Char == "'" ;
        Char == '(' ;
        Char == ')').

is_escaped(String) :- 
    string_chars(String, ['%', FirstHex, SecondHex]),
    is_hex_digit(FirstHex),
    is_hex_digit(SecondHex).
is_hex_digit(Char) :-
    (char_type(Char, digit) ;
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

:- begin_tests(is_uri).

test(is_abs_uri) :-
    is_uri('https://www.example.com').

test(is_rel_uri) :-
    is_uri('/path/to/resource').

test(is_uri_with_fragment) :-
    is_uri('https://www.example.com#fragment').

test(is_uri_with_fragment_and_rel_uri) :-
    is_uri('/path/to/resource#fragment').

:- end_tests(is_uri).
