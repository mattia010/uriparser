:- use_module(library(lists)).

uri_parts(Uri, Scheme, Hierpart, "", "") :-
    string_concat(Tmp, Hierpart, Uri),
    string_concat(Scheme, ":", Tmp),
    scheme(Scheme),
    hier_part(Hierpart),
    !.
uri_parts(Uri, Scheme, Hierpart, Query, "") :-
    string_concat(Tmp, Query, Uri),
    string_concat(Pre, "?", Tmp),
    query(Query),
    uri_parts(Pre, Scheme, Hierpart, "", ""),
    !.
uri_parts(Uri, Scheme, Hierpart, Query, Fragment) :-
    string_concat(Tmp, Fragment, Uri),
    string_concat(Pre, "#", Tmp),
    fragment(Fragment),
    uri_parts(Pre, Scheme, Hierpart, Query, ""),
    !.


uri(Uri) :-
    split_string(Uri, ':', "", [Scheme, Hierpart]),
    scheme(Scheme),
    hier_part(Hierpart),
    !.
uri(Uri) :-
    split_string(Uri, '?', "", [Prefix, Query]),
    query(Query),
    uri(Prefix),
    !.
uri(Uri) :-
    split_string(Uri, '#', "", [Prefix, Fragment]),
    fragment(Fragment),
    uri(Prefix).

hier_part(Path) :-
    path_absolute(Path),
    !.
hier_part(Path) :-
    path_rootless(Path),
    !.
hier_part(Path) :-
    path_empty(Path),
    !.
hier_part(String) :-
    string_concat("//", Rest, String),
    string_concat(Authority, Path, Rest),
    authority(Authority),
    path_abempty(Path).

uri_reference(Uri) :-
    uri(Uri),
    !.
uri_reference(RelRef) :-
    relative_ref(RelRef).

absolute_uri(Uri) :-
    split_string(Uri, ':', "", [Scheme, Hierpart]),
    scheme(Scheme),
    hier_part(Hierpart),
    !.
absolute_uri(Uri) :-
    split_string(Uri, '?', "", [Prefix, Query]),
    query(Query),
    absolute_uri(Prefix).

relative_ref(RelPart) :-
    relative_part(RelPart),
    !.
relative_ref(Ref) :-
    split_string(Ref, '?', "", [RelPart, Query]),
    query(Query),
    relative_part(RelPart),
    !.
relative_ref(Ref) :-
    split_string(Ref, '#', "", [Prefix, Fragment]),
    fragment(Fragment),
    relative_ref(Prefix).

relative_part(String) :-
    string_concat("//", Rest, String),
    !,
    string_concat(Authority, Path, Rest),
    authority(Authority),
    path_abempty(Path).
relative_part(Path) :-
    path_absolute(Path),
    !.
relative_part(Path) :-
    path_noscheme(Path),
    !.
relative_part(Path) :-
    path_empty(Path).

scheme(Char) :-
    alpha(Char),
    !.
scheme(Scheme) :-
    string_concat(FirstChar, Rest, Scheme),
    alpha(FirstChar),
    scheme_char(Rest).
scheme_char("+") :- !.
scheme_char("-") :- !.
scheme_char(",") :- !.
scheme_char(Char) :-
    alpha(Char),
    !.
scheme_char(Char) :-
    digit(Char),
    !.
scheme_char(String) :-
    string_concat(FirstChar, Rest, String),
    string_length(FirstChar, 1),
    scheme_char(FirstChar),
    scheme_char(Rest).

%DA TESTARE
authority(Host) :-
    host(Host),
    !.
authority(Hostport) :-
    split_string(Hostport, ':', "", [Host, Port]),
    port(Port),
    host(Host),
    !.
authority(Authority) :-
    split_string(Authority, '@', "", [Userinfo, Rest]),
    userinfo(Userinfo),
    authority(Rest).

userinfo("") :- !.
userinfo(Userinfo) :-
    string_concat(Chars, Rest, Userinfo),
    userinfo_char(Chars),
    userinfo(Rest),
    !.
userinfo_char(":") :- !.
userinfo_char(Char) :-
    unreserved(Char),
    !.
userinfo_char(Char) :-
    sub_delims(Char),
    !.
userinfo_char(Encoded) :-
    pct_encoded(Encoded).
%RITESTARE QUESTO CASO CHE ORA è COMMENTATO
%host(IpLit) :-
    %ip_literal(IpLit),
    %!.
host(Ip) :-
    ipv4address(Ip),
    !.
host(Reg) :-
    reg_name(Reg).

port("") :- !.
port(Port) :-
    string_concat(FirstDigit, Rest, Port),
    digit(FirstDigit),
    port(Rest).

%DA TESTARE
ip_literal(String) :-
    string_concat('[', Rest, String),
    string_concat(IP, ']', Rest),
    (%ipv6address(IP);
    ipvfuture(IP)).

%%DA TESTARE
ipvfuture(String) :-
    string_concat("v", Rest, String),
    split_string(Rest, '.', "", [Pre, Post]), Pre \= "", Post \= "",
    hex_digits(Pre),
    ipvfuture_char(Post).
ipvfuture_char(":") :- !.
ipvfuture_char(Char) :-
    unreserved(Char),
    !.
ipvfuture_char(Char) :-
    sub_delims(Char),
    !.
ipvfuture_char(String) :-
    string_concat(FirstChar, Rest, String),
    string_length(FirstChar, 1),
    ipvfuture_char(FirstChar),
    ipvfuture_char(Rest).
%
%
% IMPLEMENTARE IPV6 ADDRESS
% E DA SISTEMARE E TESTARE
%
%
ipv6address(Ipv6) :-
    split_string(Ipv6, ':', "", [H1, H2, H3, H4, H5, H6, Ls]),
    foreach(member(H, [H1, H2, H3, H4, H5, H6]), h16(H)),
    ls32(Ls).
ipv6address(Ipv6) :-
    string_concat("::", Rest, Ipv6),
    split_string(Rest, ':', "", [H1, H2, H3, H4, H5, Ls]),
    foreach(member(H, [H1, H2, H3, H4, H5]), h16(H)),
    ls32(Ls).
ipv6address(Ipv6) :-
    string_concat("::", Rest, Ipv6),
    split_string(Rest, ':', "", [H1, H2, H3, H4, Ls]),
    foreach(member(H, [H1, H2, H3, H4]), h16(H)),
    ls32(Ls).
ipv6address(Ipv6) :-
    string_concat("::", Rest, Ipv6),
    split_string(Rest, ':', "", [H1, H2, H3, Ls]),
    foreach(member(H, [H1, H2, H3]), h16(H)),
    ls32(Ls).
ipv6address(Ipv6) :-
    string_concat("::", Rest, Ipv6),
    split_string(Rest, ':', "", [H1, H2, Ls]),
    foreach(member(H, [H1, H2]), h16(H)),
    ls32(Ls).
ipv6address(Ipv6) :-
    string_concat("::", Rest, Ipv6),
    split_string(Rest, ':', "", [H, Ls]),
    h16(H),
    ls32(Ls).
ipv6address(Ipv6) :-
    string_concat("::", Ls, Ipv6),
    ls32(Ls).
ipv6address(Ipv6) :-
    string_concat("::", H, Ipv6),
    h16(H).
ipv6address("::").
ipv6address(Ipv6) :-
    string_concat(H, "::", Ipv6),
    h16(H).
%ipv6address(Ipv6) :-
%   string_concat(Pre, "::", Ipv6),
%   string_concat(H6s, H, Pre),
%   h16(H).



h16(String) :-
    string_length(String, 4),
    hex_digits(String),
    !.
h16(String) :-
    string_concat(Quad, Rest, String),
    string_length(Quad, 4),
    hex_digits(Quad),
    h16(Rest).

ls32(Ipv4) :- 
    ipv4address(Ipv4),
    !.
ls32(Ls) :-
    split_string(Ls, ':', "", [H1, H2]),
    h16(H1),
    h16(H2).

ipv4address(Ipv4) :-
    split_string(Ipv4, '.', "", [Oct1, Oct2, Oct3, Oct4]),
    foreach(member(Octet, [Oct1, Oct2, Oct3, Oct4]), dec_octet(Octet)).

dec_octet(Digit) :-
    digit(Digit),
    !.
dec_octet(Octet) :-
    string_concat(Digit1, Digit2, Octet),
    digit(Digit1), digit(Digit2),
    Digit1 \= '0',
    !.
dec_octet(Octet) :-
    string_concat("1", Rest, Octet),
    !,
    string_concat(Digit1, Digit2, Rest),
    digit(Digit1),
    digit(Digit2).
dec_octet(Octet) :-
    string_concat("2", Rest, Octet),
    string_concat(Digit1, Digit2, Rest),
    digit(Digit1),
    number_string(Num, Digit1), Num < 5,
    !,
    digit(Digit2).
dec_octet(Octet) :-
    string_concat("25", Digit, Octet),
    digit(Digit),
    number_string(Num, Digit), Num < 6.

reg_name("") :- !.
reg_name(Name) :-
    string_concat(Chars, Rest, Name),
    reg_helper(Chars),
    reg_name(Rest),
    !.
reg_helper(Char) :-
    unreserved(Char),
    !.
reg_helper(Char) :-
    sub_delims(Char),
    !.
reg_helper(Chars) :-
    pct_encoded(Chars).

path(Path) :-
    path_abempty(Path),
    !.
path(Path) :-
    path_absolute(Path),
    !.
path(Path) :-
    path_noscheme(Path),
    !.
path(Path) :-
    path_rootless(Path),
    !.
path(Path) :-
    path_empty(Path).

path_abempty("") :- !.
path_abempty(Path) :-
    string_concat("/", Segment, Path),
    segment(Segment),
    !.
path_abempty(Path) :-
    string_concat(FirstSegment, Rest, Path),
    string_concat("/", Segment, FirstSegment),
    segment(Segment),
    path_abempty(Rest).

path_absolute("/") :- !.
path_absolute(Path) :-
    string_concat("/", PathRootless, Path),
    path_rootless(PathRootless).

path_noscheme(Path) :-
    string_concat(SegmentNzNc, Rest, Path),
    segment_nz_nc(SegmentNzNc),
    path_abempty(Rest).

path_rootless(Path) :-
    string_concat(SegmentNz, Rest, Path),
    segment_nz(SegmentNz),
    path_abempty(Rest).

path_empty("").

segment("") :- !.
segment(Segment) :-
    string_concat(Pchar, Rest, Segment),
    pchar(Pchar),
    segment(Rest).

segment_nz(Pchar) :-
    pchar(Pchar),
    !.
segment_nz(Segment) :-
    string_concat(Pchar, Rest, Segment),
    Pchar \= "", Rest \= "",
    pchar(Pchar),
    segment_nz(Rest).

segment_nz_nc("@") :- !.
segment_nz_nc(Char) :-
    unreserved(Char),
    !.
segment_nz_nc(Char) :-
    sub_delims(Char),
    !.
segment_nz_nc(Encoded) :-
    pct_encoded(Encoded),
    !.
segment_nz_nc(Segment) :-
    string_concat(Chars, Rest, Segment),
    Chars \= "", Rest \= "",
    segment_nz_nc(Chars),
    segment_nz_nc(Rest).

pchar(":") :- !.
pchar("@") :- !.
pchar(Char) :-
    unreserved(Char),
    !.
pchar(Char) :-
    sub_delims(Char),
    !.
pchar(Encoded) :-
    pct_encoded(Encoded).

query(Query) :-
    fragment(Query).   %Query and Fragment use the exact same code, so this is code reusing, Query and Fragment are not related

fragment("") :- !.
fragment(Fragment) :-
    string_concat(Chars, Rest, Fragment),
    fragment_char(Chars),
    fragment(Rest),
    !.
fragment_char("/") :- !.
fragment_char("?") :- !.
fragment_char(Pchar) :-
    pchar(Pchar).


pct_encoded(String) :- 
    string_length(String, 3),
    string_concat("%", Rest, String),
    string_concat(Hex1, Hex2, Rest),
    hex_digit(Hex1),
    hex_digit(Hex2).

hex_digit("A") :- !.
hex_digit("B") :- !.
hex_digit("C") :- !.
hex_digit("D") :- !.
hex_digit("E") :- !.
hex_digit("F") :- !.
hex_digit("a") :- !.
hex_digit("b") :- !.
hex_digit("c") :- !.
hex_digit("d") :- !.
hex_digit("e") :- !.
hex_digit("f") :- !.
hex_digit(Char) :-
    digit(Char).

hex_digits(Char) :-
    hex_digit(Char),
    !.
hex_digits(String) :-
    string_concat(FirstChar, Rest, String),
    string_length(FirstChar, 1),
    hex_digit(FirstChar),
    hex_digits(Rest).

unreserved("-") :- !.
unreserved(".") :- !.
unreserved("_") :- !.
unreserved("~") :- !.
unreserved(Char) :-
    alpha(Char),
    !.
unreserved(Char) :-
    digit(Char).

reserved(Char) :-
    gen_delims(Char),
    !.
reserved(Char) :-
    sub_delims(Char).

gen_delims(":") :- !.
gen_delims("/") :- !.
gen_delims("?") :- !.
gen_delims("#") :- !.
gen_delims("[") :- !.
gen_delims("]") :- !.
gen_delims("@").

sub_delims("!") :- !.
sub_delims("$") :- !.
sub_delims("&") :- !.
sub_delims("'") :- !.
sub_delims("(") :- !.
sub_delims(")") :- !.
sub_delims("*") :- !.
sub_delims("+") :- !.
sub_delims(",") :- !.
sub_delims(";") :- !.
sub_delims("=").

alpha("A") :- !.
alpha("B") :- !.
alpha("C") :- !.
alpha("D") :- !.
alpha("E") :- !.
alpha("F") :- !.
alpha("G") :- !.
alpha("H") :- !.
alpha("I") :- !.
alpha("J") :- !.
alpha("K") :- !.
alpha("L") :- !.
alpha("M") :- !.
alpha("N") :- !.
alpha("O") :- !.
alpha("P") :- !.
alpha("Q") :- !.
alpha("R") :- !.
alpha("S") :- !.
alpha("T") :- !.
alpha("U") :- !.
alpha("V") :- !.
alpha("W") :- !.
alpha("X") :- !.
alpha("Y") :- !.
alpha("Z") :- !.
alpha("a") :- !.
alpha("b") :- !.
alpha("c") :- !.
alpha("d") :- !.
alpha("e") :- !.
alpha("f") :- !.
alpha("g") :- !.
alpha("h") :- !.
alpha("i") :- !.
alpha("j") :- !.
alpha("k") :- !.
alpha("l") :- !.
alpha("m") :- !.
alpha("n") :- !.
alpha("o") :- !.
alpha("p") :- !.
alpha("q") :- !.
alpha("r") :- !.
alpha("s") :- !.
alpha("t") :- !.
alpha("u") :- !.
alpha("v") :- !.
alpha("w") :- !.
alpha("x") :- !.
alpha("y") :- !.
alpha("z").

digit("0") :- !.
digit("1") :- !.
digit("2") :- !.
digit("3") :- !.
digit("4") :- !.
digit("5") :- !.
digit("6") :- !.
digit("7") :- !.
digit("8") :- !.
digit("9").
