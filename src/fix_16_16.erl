%%
%% Signed 16.16 fixpoint number
%% 
-module(fix_16_16).

-define(IBITS, 16).
-define(FBITS, 16).

-export([exp/1]).
-export([log/1]).
-export([log_/1]).

-include("fix.hrl").



log_tab() ->
    {
      ?float_to_fix(math:log(65536)),
      ?float_to_fix(math:log(256)),
      ?float_to_fix(math:log(16)),
      ?float_to_fix(math:log(4)),
      ?float_to_fix(math:log(2)),
      ?float_to_fix(math:log(3/2)),
      ?float_to_fix(math:log(5/4)),
      ?float_to_fix(math:log(9/8)),
      ?float_to_fix(math:log(17/16)),
      ?float_to_fix(math:log(33/32)),
      ?float_to_fix(math:log(65/64)),
      ?float_to_fix(math:log(129/128))
    }.

exp(X) ->
    Y0 = ?float_to_fix(1.0),
    exp(2,8,X,Y0,log_tab()).

exp(I,S,X,Y,Log) when I =< 5 ->
    T = X-element(I,Log),
    if T >= 0 ->
	    exp(I+1,S bsr 1,T,Y bsl S, Log);
       true ->
	    exp(I+1,S bsr 1,X,Y,       Log)
    end;
exp(I,S,X,Y,Exp) when I =< 12 -> %% I=5 => S=0
    T = X - element(I,Exp),
    if T >= 0 ->
	    exp(I+1,S+1,T,Y+(Y bsr (S+1)),Exp);
       true ->
	    exp(I+1,S+1,X,Y,Exp)
    end;
exp(I,S,X,Y,Exp) when I =< 21 ->
    if X band (1 bsl (15-I)) =/= 0 ->
	    exp(I+1,S+1,X,Y+(Y bsr (S+1)),Exp);
       true ->
	    exp(I+1,S+1,X,Y,Exp)
    end;
exp(_X,Y,_I,_S,_Exp) ->
    Y.

log(X) ->
    Y0 = ?float_to_fix(10.397177190355384), %% log(32767) 
    log(1,16,X,Y0,log_tab()).

log(I,S,X,Y,Log) when I =< 5 ->
    J = (32-S)-1,
    if X < (1 bsl J) ->
	    log(I+1,S bsr 1,X bsl S, Y-element(I,Log),Log);
       true ->
	    log(I+1,S bsr 1,X,Y,Log)
    end;
log(I,S,X,Y,Log) when I =< 12 ->
    T = X + (X bsr (S+1)),
    if T bsr 31 =:= 0 ->
	    log(I+1,S+1,T, Y-element(I,Log),Log);
       true ->
	    log(I+1,S+1,X,Y,Log)
    end;
log(_I,_S,X,Y,_Log) ->
    X1 = 16#80000000 - X,
    Y1 = Y - (X1 bsr 15),
    Y1.

log_(X0) ->
    Y0 = ?float_to_fix(math:log(32767)),
    {X1,Y1} = if (X0 < (1 bsl 15)) -> 
		      {X0 bsl 16, Y0 - ?float_to_fix(math:log(65536))};
		 true ->
		      {X0,Y0}
	      end,
    {X2,Y2} = if (X1 < (1 bsl 23)) -> 
		      {X1 bsl 8, Y1 - ?float_to_fix(math:log(256))};
		 true ->
		      {X1,Y1}
	      end,
    {X3,Y3} = if (X2 < (1 bsl 27)) -> 
		      {X2 bsl 4, Y2 - ?float_to_fix(math:log(16))};
		 true ->
		      {X2,Y2}
	      end,
    {X4,Y4} = if (X3 < (1 bsl 29)) ->
		      {X3 bsl 2, Y3 - ?float_to_fix(math:log(4))};
		 true ->
		      {X3,Y3}
	      end,
    {X5,Y5} = if (X4 < (1 bsl 30)) ->
		      {X4 bsl 1, Y4 - ?float_to_fix(math:log(2))};
		 true ->
		      {X4,Y4}
	      end,
    T6 = X5+(X5 bsr 1),
    {X6,Y6} = if T6 bsr 31 =:= 0 -> {T6,Y5-?float_to_fix(math:log(3/2))};
		 true -> {X5,Y5}
	      end,
    T7 = X6+(X6 bsr 2),
    {X7,Y7} = if T7 bsr 31 =:= 0 -> {T7,Y6-?float_to_fix(math:log(5/4))};
		 true -> {X6,Y6}
	      end,
    T8 = X7+(X7 bsr 3),
    {X8,Y8} = if T8 bsr 31 =:= 0 -> {T8,Y7-?float_to_fix(math:log(9/8))};
		 true -> {X7,Y7}
	      end,
    T9 = X8+(X8 bsr 4),
    {X9,Y9} = if T9 bsr 31 =:= 0 -> {T9,Y8-?float_to_fix(math:log(17/16))};
		 true -> {X8,Y8}
	      end,
    T10 = X9+(X9 bsr 5),
    {X10,Y10} = if T10 bsr 31 =:= 0 -> {T10,Y9-?float_to_fix(math:log(33/32))};
		 true -> {X9,Y9}
	      end,
    T11 = X10+(X10 bsr 6),
    {X11,Y11} = if T11 bsr 31 =:= 0 -> {T11,Y10-?float_to_fix(math:log(65/64))};
		   true -> {X10,Y10}
		end,
    T12 = X11+(X11 bsr 7),
    {X12,Y12} = if T12 bsr 31 =:= 0 -> {T12,Y11-?float_to_fix(math:log(129/128))};
		   true -> {X11,Y11}
		end,
    X13 = 16#80000000 - X12,
    Y13 = Y12 - (X13 bsr 15),
    Y13.
