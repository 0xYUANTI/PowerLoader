%%%
%%%   dBBBBBb dBBBBBb   dBBBBBb dBBBBBb     dBBBBb  dBBBBP dBP dBBBBBb
%%%       dB'      BB       dBP      BB        dBP dB'.BP           BB
%%%   dBBBP'   dBP BB   dBBBBK'  dBP BB   dBP dBP dB'.BP dBP    dBP BB
%%%  dBP      dBP  BB  dBP  BB  dBP  BB  dBP dBP dB'.BP dBP    dBP  BB
%%% dBP      dBBBBBBB dBP  dB' dBBBBBBB dBP dBP dBBBBP dBP    dBBBBBBB
%%%
%%% A PowerLoader extension module for playing Paranoia XP over IRC.
%%% $Id: paranoia.pl,v 1.7 2008/11/07 19:39:42 j Exp $
%%%

:- module(paranoia, []).
:- use_module(library(assoc)).
:- set_prolog_flag(double_quotes, chars).
%% [powerloader].

%% Interface ------------------------------------------------------------------

eval(Msg, Reply, St0, St) :-
        cmd(Msg, Handler, Edge),       % Resolve command
        transition(Edge, St0, St1),    % Check & record state change
        call(Handler, Reply, St1, St). % Get reply

init(St) :- state(St).

%% Implementation -------------------------------------------------------------

%%   Who  Command             Handler          Edge
cmd(   _> ["!target"      ],  target,          -      ).
cmd(   _> ["!target", N0  ],  target(N),       target ) :- string_to_int(N0, N).
cmd(   _> ["!oops", N0    ],  target(N),       oops   ) :- string_to_int(N0, N).
cmd( Who> ["!bid", N0     ],  bid(Who, N),     bid    ) :- string_to_int(N0, N).
cmd( Who> ["!roll"        ],  roll(Who),       roll   ).
cmd(   _> ["!reset"       ],  reset,           -      ).
cmd( Who> ["!printchar"   ],  printchar(Who),  -      ).
cmd( Who> ["!scene", N0   ],  scene(Who, N),   -      ) :- string_to_int(N0, N).
cmd(   _> ["!award", P, N0],  award(P, N),     -      ) :- string_to_int(N0, N).
cmd( Who> ["!register"    ],  register(Who),   -      ).

string_to_int(Str, Int) :- catch(number_chars(Int, Str), _Exn, fail).

%% Conflict resolution:
%%   (1) GM sets target number
%%   (2) Optionally, players bid points
%%   (3) Player makes the roll
fsm(s0, target, s1).
fsm(s1, oops,   s1).
fsm(s1, bid,    s2).
fsm(s1, roll,   s3).
fsm(s2, bid,    s2).
fsm(s2, roll,   s3).
fsm(s3, target, s1).
fsm(S,  -,       S).

transition(Edge, St0, St1) :-
        get_fsmstate(St0, State0, St1, State1),
        fsm(State0, Edge, State1).

%% State record
state(s(s0, none, Chars)) :- empty_assoc(Chars).

get_fsmstate(s(S0,T,C), S0, s(S1,T,C), S1).

is_target(T) :- integer(T), between(1, 20, T).
is_target(none).

get_target(s(_,T,_), T).
get_target(s(S,T0,C), T0, s(S,T1,C), T1) :- is_target(T1).
set_target(T, s(S,_,C), s(S,T,C)) :- is_target(T).

get_points(Who, s(_,_,C), Points) :-
        get_assoc(Who, C, Points).
get_points(Who, s(S,T,C0), Points0, s(S,T,C1), Points1) :-
        get_assoc(Who, C0, Points0, C1, Points1).
set_points(Who, s(S,T,C0), Points, s(S,T,C1)) :-
        \+ get_assoc(Who, C0, _),
        put_assoc(Who, C0, Points, C1).
inc_points(Points, s(S,T,C0), s(S,T,C1)) :-
        map_assoc(plus(Points), C0, C1).

%% Messages
msg1("Target: ~w").
msg2("~s: ~w [~w] => ~w (~s)").
msg3("~s: Points: ~w").
msg4("~s: The operation succeeded.").

%% Commands
target(Msg<=[Num], St, St)        :- msg1(Msg), get_target(St, Num).
target(Num, Msg<=[Num], St0, St1) :- msg1(Msg), set_target(Num, St0, St1).

bid(Who, N, Msg<=[Num], St0, St) :-
        %% Check & record point expenditure
        Spend is abs(N),
        get_points(Who, St0, Points0, St1, Points1),
        Points0 >= Spend,
        Points1 is Points0 - Spend,
        %% Modify target number
        msg1(Msg),
        get_target(St1, Num0),
        Num is Num0 + N,
        set_target(Num, St1, St).

roll(Who, Msg<=[Who,Roll,Num,Margin,Result], St0, St1) :-
        msg2(Msg),
        get_target(St0, Num, St1, none),
        Roll is random(20) + 1,
        Margin is Num - Roll,
        (   Roll =< Num
        ->  Result = "success"
        ;   Result = "failure"
        ).

reset("Yes", _, St) :- init(St).

printchar(Who, Msg<=[Who,Points], St, St) :-
        msg3(Msg),
        get_points(Who, St, Points).

scene(Who, Points, Msg<=[Who], St0, St1) :-
        msg4(Msg),
        inc_points(Points, St0, St1).

award(Player, Award, Msg<=[Player,Points1], St0, St1) :-
        msg3(Msg),
        get_points(Player, St0, Points0, St1, Points1),
        Points1 is Points0 + Award.

register(Who, Msg<=[Who], St0, St1) :-
        msg4(Msg),
        set_points(Who, St0, 25, St1).

%%% eof
