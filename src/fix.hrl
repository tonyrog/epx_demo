%%
%% Fix-point arithmetics
%%
-ifndef(__FIX_HRL__).
-define(__FIX_HRL__, true).

-export([new/1]).
-export([add/2]).
-export([sub/2]).
-export([multiply/2]).
-export([divide/2]).
-export([format/1]).

%% when including this hrl file define.
%% IBITS = number integer bits
%% FBITS = number of fraction bits
%%
-define(IBASE, (1 bsl ?IBITS)).
-define(FBASE, (1 bsl ?FBITS)).
-define(FIX(I,F), (((I) bsl ?FBITS) bor (F))).

%% float_to_fix is only defined for F less than IBASE!
-define(float_to_fix(F),
	?FIX(trunc(F),trunc((F-trunc(F))*?FBASE))).

new(F) when is_float(F) ->
    if F < 0.0 ->
	    bnot abs_new(abs(F));
       true ->
	    abs_new(F)
    end.

abs_new(Float) when Float >= ?IBASE ->
    I = trunc(Float),
    Frac = (Float - I),
    F = trunc(Frac * ?FBASE),
    ?FIX(?IBASE-1,F);
abs_new(Float) ->
    I = trunc(Float),
    Frac = (Float - I),    
    F = trunc(Frac * ?FBASE),
    ?FIX(I,F).

add(A,B) -> A+B.
sub(A,B) -> A-B.
multiply(A,B) -> (A*B) bsr ?FBITS.
divide(A,B) -> ((A bsl ?FBITS) div B).

    
format(A) when A < 0 ->
    "-"++format(-A);
format(A) ->
    I = (A bsr ?FBITS) band (?IBASE-1),
    F = A band (?FBASE-1),
    io_lib_format:fwrite_g(I + F/?FBASE).



-endif.
