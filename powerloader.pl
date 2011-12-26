%%%
%%%  ####                          ##                  ##
%%%  ##  #   ##  # # #   ##   # #  ##    ##   ###    ###    ##   # #
%%%  ## ##  #### # # #  ## #  ###  ##   ####    ##  ####   ## #  ###
%%% #####  ## ## ### # #####  #   ##   ## ##  #### ## ##  #####  #
%%% ##     ####  ####  ##    ##   ##   ####  ## #  ## #   ##    ##
%%% ##      ##   # #    ###  ##   ####  ##   #####  ###    ###  ##
%%%
%%% A Simple IRC Bot.
%%% $Id: powerloader.pl,v 1.7 2008/11/07 19:39:42 j Exp $
%%%
%%% References:
%%%     http://www.ietf.org/rfc/rfc1459.txt
%%%     http://www.cse.unsw.edu.au/~dons/irc/bot.html
%%%

:- module(powerloader, [start/0]).

:- use_module(library(socket)).
:- use_module(library(readutil)).
:- use_module(library(lists)).

:- op(900, xfx, user:(<=)).          % sformat/2
:- op(900, xfx, user:(>)).           % internal messages

:- set_prolog_flag(double_quotes, chars).

%% Constants ------------------------------------------------------------------

host('irc.freenode.net').
%host('irc.magicstar.net').
port(6667).
handle("blargbot").
chan("#testblarg").

%% No configurables beyond this point.

%% main() ---------------------------------------------------------------------

start :- connect, init, run.

connect :-
        host(Host), port(Port),
        log("connecting to ~w:~w... ", [Host,Port]),
        tcp_socket(Socket),
        tcp_connect(Socket, Host:Port),
        tcp_open_socket(Socket, In, Out),
        log("done~n", []),
        set_input(In), set_output(Out).

disconnect :- close(current_input), close(current_output).

init :-
        host(Host), handle(Handle), chan(Chan),
        nick(Handle), user(Handle, Host), join(Chan).

run :- state(St), loop(St).

loop(exit) :- !.
loop(St0) :-
        recv(Line),
        log("~s~n", [Line]),
        split(Line, Meta, Content),
        sender(Meta, Sender),
        scan(Content, Tokens),
        eval(Sender>Tokens, St0, St1),
        loop(St1).

%% Command interpreter --------------------------------------------------------

%% eval(+Msg: chars>[chars], +St0: term, -St1: term)
%% is always true.

%% Basic commands
eval(Msg, St0, St1) :-
        cmd(Msg, Handler), !,
        call(Handler, Reply, St0, St1),
        herald(Reply).
%% Extension support
eval(Msg, St0, St1) :-
        forward(Msg, Reply, St0, St1), !,
        herald(Reply).
%% Ignore non-command lines
eval(_, St, St).


%% Extensions are regular Prolog modules which define two predicates:
%% eval/4 should provide replies to any commands the module wishes to
%% support and fail otherwise. The final two arguments may be
%% used to save module state between invocations (the bot will never
%% touch these).
%% init/1 describes the initial module state (called once, at module
%% load time).


%% forward(+Msg: chars>[chars],
%%         -Reply: chars | chars<=[term] | 'none',
%%         +St0: term,
%%         -St1: term)
%% is true when Reply is some module's reply to Msg.
%% Registered modules are queried in most-recently-added-first order.
forward(Msg, Reply, St0, St1) :-
        loaded_modules(St0, Mods0, St1, Mods1),
        forward_loop(Mods0, Mods1, Msg, Reply).

forward_loop([mod(M,S0)|Ms], [mod(M,S1)|Ms], Msg, Reply) :-
        M:eval(Msg, Reply, S0, S1).
forward_loop([M|Ms], [M|NewMs], Msg, Reply) :-
        forward_loop(Ms, NewMs, Msg, Reply).
forward_loop([], _, _, _) :- fail.

%% Basic commands -------------------------------------------------------------

cmd( _> ["PING", Rand   ],  pong(Rand)  ).
cmd( _> ["!quit"        ],  quit        ).
cmd( _> ["!load", Mod0  ],  load(Mod)   ) :- string_to_atom(Mod0, Mod).
cmd( _> ["!unload", Mod0],  unload(Mod) ) :- string_to_atom(Mod0, Mod).

pong(   Rand, none,  St,  St  ) :- pong(Rand).
quit(         none,  _,   exit) :- quit, disconnect.
load(   Mod,  "Yes", St0, St1 ) :- load_module(Mod, St0, St1), !.
load(  _Mod,  "No",  St,  St  ).
unload( Mod,  "Yes", St0, St1 ) :- unload_module(Mod, St0, St1), !.
unload(_Mod,  "No",  St,  St  ).

%% Extension management -------------------------------------------------------

state([]).

loaded_modules(Mods0, Mods0, Mods1, Mods1).

load_module(Mod, Mods, [mod(Mod,S)|Mods]) :-
        \+ member(mod(Mod,_), Mods),
        catch(consult(Mod), _Exn, fail),
        Mod:init(S).

unload_module(Mod, Mods0, Mods1) :-
        select(mod(Mod,_), Mods0, Mods1).

%% Utility predicates ---------------------------------------------------------

herald(Msg0) :- chan(Chan), sformat(Msg0, Msg), privmsg(Chan, Msg).
herald(none).

%% IRC client messages
nick(Nick)         :- send("NICK", Nick).
user(Id, Host)     :- send("USER", "~s_u ~w ~s_h :~s_r"<=[Id,Host,Id,Id]).
join(Chan)         :- send("JOIN", Chan).
pong(Rand)         :- send("PONG", Rand).
privmsg(Chan, Msg) :- send("PRIVMSG", "~s :~s"<=[Chan,Msg]).
quit               :- send("QUIT", ":Exiting").

send(Command, Args0) :-
        sformat(Args0, Args),
        format("~s ~s\r\n", [Command,Args]),
        flush_output,
        log("> ~s ~s~n", [Command,Args]).

sformat(Str, Str)       :- is_list(Str).
sformat(Fmt<=Args, Str) :- format(chars(Str), Fmt, Args).

log(Fmt, Args) :- format(user, Fmt, Args).

recv(Line) :-
        read_line_to_codes(current_input, Line0),
        atom_codes(Line1, Line0),
        atom_chars(Line1, Line).

%% Server messages look roughly as follows:
%%   :<nick>!i=<identity> <command> <args> :<actual content>
split([':'|Line], Meta, Content) :- append(Meta, [':'|Content], Line), !.
split(Line, [], Line).

sender(Meta, Sender) :- append(Sender, ['!','i','='|_Discarded], Meta), !.
sender(Meta, Meta).

%% Lexer
scan(Line, Tokens)   :- phrase(tokenize(Tokens), Line).

tokenize([Tok|Toks]) --> whitespace, token(Tok), !, tokenize(Toks).
tokenize([])         --> whitespace.

token([C|Cs])        --> nonwhitespace(C), token(Cs).
token([C])           --> nonwhitespace(C).

whitespace           --> [C], {char_type(C, white)}, !, whitespace.
whitespace           --> [].

nonwhitespace(C)     --> [C], {\+ char_type(C, white)}.

%%% eof
