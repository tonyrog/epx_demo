%%%-------------------------------------------------------------------
%%% File    : edraw.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : drawing with epic
%%%
%%% Created :  9 Jun 2009 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(edraw).

-compile(export_all).
-import(lists, [reverse/1]).

-define(AXIS_X,  1).
-define(AXIS_Y,  2).
-define(AXIS_XY, 3).

%%
%% {line,{X0,Y0},{X1,Y1}}
%%
-record(line,
	{
	  label,  %% 1 .. 8
	  x=0,    %% 0 -> dx
	  y=0,    %% 0 -> dy
	  f,      %% delta error
	  n,      %% number of steps remain (dx)
	  axis,   %% last step (x, y or both)
	  %% constants
	  pt,     %% fun(X,Y) -> {X',Y'} end
	  d=0,    %% 2*Dy        (f < 0)
	  df=0,   %% 2*Dy - 2*Dx (f >= 0)

	  %% anti alias parameters
	  ap = fun() -> {0,0} end,  %% fun() -> {sign,sign} end
	  dx=0,
	  invL,    %% 1/length(Dx,Dy)
	  invL2dx  %% 2*dx/length(Dx,Dy)
	 }).

%%
%% Draw segment of ellipse:  x^2/a^2 + y^2/b^2 = 1
%%
%% x^2*b^2 + y^2*a^2 = a^2*b^2
%%  x = a*sqrt(1 - y^2/b^2)
%% 
%%
-record(round,
	{
	  x,      %% a -> 0
	  y,      %% 0 -> b
	  f,      %% delta error
	  pya,    %% a^2
	  pxb,    %% (2*a - 1)*b^2
	  axis,   %% last step (x, y or both)
	  %% constants
	  pt,     %% fun(X,Y) -> {X',Y'} end
	  a,      %% ellipse a parameter
	  b,      %% ellipse a parameter
	  a2,     %% a^2 
	  b2,     %% b^2
	  %% anti alias
	  ap = fun() -> {0,0} end  %% fun() -> {sign,sign} end
	 }).

%%
%% Segment
%%
-record(segment,
	{
	  x=0,    %% current (internal) x value
	  y=0,    %% current (internal) y value
	  f,      %% delta error
	  pf,     %% point function: pf:  (X,Y) -> (X,Y)'
	  step,   %% step function:  sf:  S -> S1
	  data    %% segment type data #line | #round
	 }).


-record(s,
	{
	  win,
	  pix,
	  image,
	  width,
	  height,
	  func,
	  arg = [],
	  tmo = infinity,
	  aalias = false,
	  bg    = {0,0,0},
	  fg1   = {127,127,127},
	  fg2   = {0,127,0},
	  fill1 = {255,255,255},
	  fill2 = {127,127,200},
	  dummy
	 }).

-define(assert(Cond, Reason),
	case (Cond) of
	    false -> erlang:error(Reason);
	    true -> ok
	end).
		

-define(recf(Name,R), recfmt(list_to_tuple(record_info(fields, Name)), R)).

recfmt(Fs, R) ->
    lists:foldl(
      fun(I,Acc) ->
	      A=io_lib:format("~s: ~p", [element(I,Fs),element(I+1,R)]),
	      if I == 1 -> [A|Acc]; true -> [",",A|Acc] end
      end, [], lists:seq(size(Fs), 1, -1)).

point_add({X0,Y0},{X1,Y1}) ->    
    {X0+X1,Y0+Y1}.

point_subtract({X0,Y0},{X1,Y1}) ->    
    {X0-X1,Y0-Y1}.

point_scale({X0,Y0}, S) ->
    {trunc(X0*S), trunc(Y0*S)}.

point_less({X0,Y0},{X1,Y1}) ->
    (Y0 < Y1) orelse ((Y0==Y1) andalso (X0 < X1)).

point_greater({X0,Y0},{X1,Y1}) ->
    (Y0 > Y1) orelse ((Y0==Y1) andalso (X0 > X1)).

%% Scale and offset vector
vector_scale_add({Vx,Vy},Scale,{X,Y}) ->
    {round(X + Vx*Scale), round(Y + Vy*Scale)}.

vector_dot({X0,Y0},{X1,Y1}) ->
    X0*X1 + Y0*Y1.

%% check if P0 is left of P1
point_left({X0,_Y0},{X1,_Y1}) ->  (X0 < X1).

%% check if P0 is right of P1
point_right({X0,_Y0},{X1,_Y1}) -> (X0 > X1).

%% Adjust index I in Pts to fall in 1..size(Pts)
%% For all integer values
pindex(I, Pts) when is_integer(I), is_tuple(Pts) ->
    N = size(Pts),
    ((((I-1) rem N)+N) rem N) + 1.
    

sign(0) -> 0;
sign(X) when X < 0 -> -1;
sign(_X) -> 1.

mk_ln(Label,Dx, Dy, Pt, Ap) ->
    %% anti-alias
    InvL = 1/(2*math:sqrt(Dy*Dy + Dx*Dx)),
    #line { label=Label,    %% debug
	    n  = Dx,        %% number of steps
	    f  = 2*Dy - Dx,
	    d  = 2*Dy,        %% straight delta
	    df = 2*(Dy -Dx),  %% diagonal delta
	    axis = 0,
	    pt=Pt,            %% coordinate function f:(X,Y) -> (X',Y')
	    %% anti-alias
	    ap=Ap,
	    dx=Dx, 
	    invL = InvL,
	    invL2dx = 2*Dx*InvL 
	   }.

    
%% step data structure - will always step from X0,Y0 to X1,Y1
mk_line(_S, {X0,Y0}, {X1,Y1}) ->
    Dy = abs(Y1 - Y0),
    Dx = abs(X1 - X0),
    if Dx == 0 ->
	    if Y0 < Y1 ->
		    #line { label=v1,n=Dy,f=-1, pt=fun(X,_Y) -> {X0,Y0+X} end};
	       true ->
		    #line { label=v2,n=Dy, f=-1, pt=fun(X,_Y) -> {X0,Y0-X} end}
	    end;
       Dy == 0 ->
	    if X0 < X1 ->
		    #line { label=h1,n=Dx, f=-1, pt=fun(X,_Y) -> {X0+X,Y0} end};
	       true ->
		    #line { label=h2,n=Dx, f=-1, pt=fun(X,_Y) -> {X0-X,Y0} end}
	    end;
       Dx > Dy ->
	    if X0 < X1 ->
		    if Y0 < Y1 ->  %% 1
			    mk_ln(1, Dx, Dy, 
				  fun(X,Y) -> {X0+X, Y0+Y} end,
				  fun() -> {0,1} end
				 );
		       true ->  %% 8
			    mk_ln(8, Dx, Dy, 
				  fun(X,Y) -> {X0+X, Y0-Y} end,
				  fun() -> {0,-1} end)
		    end;
	       true ->
		    if Y0 < Y1 ->  %% 4
			    mk_ln(4, Dx, Dy, 
				  fun(X,Y) -> {X0-X, Y0+Y} end,
				  fun() -> {0,1} end);
		       true ->  %% 5
			    mk_ln(5, Dx, Dy,
				  fun(X,Y) -> {X0-X, Y0-Y} end,
				  fun() -> {0,-1} end)
		    end
	    end;
       true ->
	    if Y0 < Y1 ->
		    if X0 < X1 -> %% 2
			    mk_ln(2, Dy, Dx,
				  fun(X,Y) -> {X0+Y, Y0+X} end,
				  fun() -> {1,0} end);
		       true ->  %% 3
			    mk_ln(3, Dy, Dx, 
				  fun(X,Y) -> {X0-Y, Y0+X} end,
				  fun() -> {-1,0} end)
		    end;
	       true ->
		    if X0 < X1 -> %% 7
			    mk_ln(7, Dy, Dx, 
				  fun(X,Y) -> {X0+Y, Y0-X} end,
				  fun() -> {1,0} end);
		       true ->  %% 6
			    mk_ln(6, Dy, Dx, 
				  fun(X,Y) -> {X0-Y, Y0-X} end,
				  fun() -> {-1,0} end)
		    end
	    end
    end.


%%
%% Draw round shape from P0 to P1
%% 
mk_ab(A, B, Pt) ->
    #round { a  = abs(A),
	     b  = abs(B),
	     a2 = A*A,
	     b2 = B*B,
	     pya = A*A,
	     pxb = (2*abs(A)-1)*B*B,
	     axis = 0,
	     f=0,
	     x=abs(A), 
	     y=0,
	     pt=Pt
	    }.

%% clockwise 
mk_round(_S,1,{X0,Y0},{X1,Y1}) ->
    A = (X1 - X0),
    B = (Y1 - Y0),
    if B >= 0, A < 0 ->
	    %% io:format("CW-Q1: (~w,~w)\n", [X1,Y0]),
	    mk_ab(A, B, fun(X,Y) -> {X1+X,Y0+Y} end);
       B < 0,  A < 0 ->
	    %% io:format("CW-Q2: (~w,~w)\n", [X0,Y1]),
	    mk_ab(B, A, fun(X,Y) -> {X0-Y,Y1+X} end);
       B < 0, A >= 0 ->
	    %% io:format("CW-Q3: (~w,~w)\n", [X1,Y0]),
	    mk_ab(A, B, fun(X,Y) -> {X1-X,Y0-Y} end);	    
       B >= 0, A >= 0 ->
	    %% io:format("CW-Q4: (~w,~w)\n", [X0,Y1]),
	    mk_ab(B, A, fun(X,Y) -> {X0+Y,Y1-X} end)
    end;
%% counter clockwise
mk_round(_S,-1,{X0,Y0},{X1,Y1}) ->
    A = (X1 - X0),
    B = (Y1 - Y0),
    if B < 0, A >= 0 ->
	    %% io:format("CCW-Q1: (~w,~w)\n", [X0,Y1]),
	    mk_ab(B, A, fun(X,Y) -> {X0+Y,Y1+X} end);
       B >= 0, A >= 0 ->
	    %% io:format("CCW-Q2: (~w,~w)\n", [X1,Y0]),
	    mk_ab(A, B, fun(X,Y) -> {X1-X,Y0+Y} end);
       B >= 0, A < 0 ->
	    %% io:format("CCW-Q3: (~w,~w)\n", [X0,Y1]),
	    mk_ab(B, A, fun(X,Y) -> {X0-Y,Y1-X} end);	    
       B < 0,  A < 0 ->
	    %% io:format("CCW-Q4: (~w,~w)\n", [X1,Y0]),
	    mk_ab(A, B, fun(X,Y) -> {X1+X,Y0-Y} end)
    end.

mk_segment(S, {line,P0,P1}) ->
    mk_line(S, P0, P1);
mk_segment(S, {round,Q,P0,P1}) ->
    mk_round(S,Q,P0,P1).

offset_segments(Segments, {0,0}) ->
    Segments;
offset_segments(Segments, Offs) ->
    offset_segments1(Segments, Offs).

%% offset segments
offset_segments1([Segment|Segments], Offs) ->
    case Segment of
	{move_to,P1} ->
	    Q1 = point_add(P1, Offs),
	    [{move_to,Q1}|offset_segments1(Segments, Offs)];

	{move,_WH} ->
	    [Segment|offset_segments1(Segments, Offs)];

	{line_to,P1} ->
	    Q1 = point_add(P1, Offs),
	    [{line_to,Q1}|offset_segments1(Segments, Offs)];		    

	{line,_WH} ->
	    [Segment|offset_segments1(Segments, Offs)];

	{line,P0,P1} ->
	    Q0 = point_add(P0, Offs),
	    Q1 = point_add(P1, Offs),
	    [{line,Q0,Q1} | offset_segments1(Segments,Offs)];

	{round_to,Q,P1} ->
	    Q1 = point_add(P1, Offs),
	    [{round_to,Q,Q1}|offset_segments1(Segments, Offs)];		    

	{round,_Q,_WH} ->
	    [Segment | offset_segments1(Segments, Offs)];
	{round,Q,P0,P1} ->
	    Q0 = point_add(P0, Offs),
	    Q1 = point_add(P1, Offs),
	    [{round,Q,Q0,Q1} | offset_segments1(Segments,Offs)]
    end;
offset_segments1([], _Offs) ->
    [].


%% scale segments
scale_segments([Segment|Segments], Scale) ->
    case Segment of
	{move_to,P1} ->
	    Q1 = point_scale(P1, Scale),
	    [{move_to,Q1}|scale_segments(Segments, Scale)];

	{move,{W,H}} ->
	    [{move,{trunc(W*Scale),trunc(H*Scale)}}|
	     scale_segments(Segments, Scale)];

	{line_to,P1} ->
	    Q1 = point_scale(P1, Scale),
	    [{line_to,Q1}|scale_segments(Segments, Scale)];		    

	{line,{W,H}} ->
	    [{line,{trunc(W*Scale),trunc(H*Scale)}}|
	     scale_segments(Segments, Scale)];

	{line,P0,P1} ->
	    Q0 = point_scale(P0, Scale),
	    Q1 = point_scale(P1, Scale),
	    [{line,Q0,Q1} | scale_segments(Segments,Scale)];

	{round_to,Q,P1} ->
	    Q1 = point_scale(P1, Scale),
	    [{round_to,Q,Q1}|scale_segments(Segments,Scale)];		    

	{round,Q,{W,H}} ->
	    [{round,Q,{trunc(W*Scale),trunc(H*Scale)}}|
	     scale_segments(Segments, Scale)];

	{round,Q,P0,P1} ->
	    Q0 = point_scale(P0, Scale),
	    Q1 = point_scale(P1, Scale),
	    [{round,Q,Q0,Q1} | scale_segments(Segments,Scale)]
    end;
scale_segments([], _Offs) ->
    [].

%%
%% Calculate the vector border points
%% Given P0 - P1 calculate P2 - P3 parallel to P0 - P1
%% at a distance of Length
%%
xparallel(P0,P1,Length) ->
    {Dx,Dy} = point_subtract(P1,P0),
    V = {-Dy, Dx},
    Dot = vector_dot(V, V),
    Scale = Length/math:sqrt(Dot),
    P2 = vector_scale_add(V,Scale,P0),
    P3 = vector_scale_add(V,Scale,P1),
    {P2,P3}.

%%
%% extend vector (0,0)-P0
%% as a line segment with Length length in Y/X direction
%%
xvector(P0, Length) ->
    Dot = vector_dot(P0,P0),
    Scale = Length/math:sqrt(Dot),
    vector_scale_add(P0,Scale,P0).

%% 
%% move a line segment Length units outwards from (0,0)
%% hereby moveing the point P0 and P1 on respective vectors
%% by Length units.
%%
pmove(P0,P1,Length) ->
    {xvector(P0, Length), xvector(P1,Length)}.

%% parallel move segments
pmove_segments(Segments, Length) ->
    pmove_segments(Segments, {0,0}, Length).

pmove_segments([Segment|Segments],P0,Length) ->
    case Segment of
	{move_to,P1} ->
	    [Segment|pmove_segments(Segments,P1,Length)];

	{move,{W,H}} ->
	    P1 = point_add(P0, {W-sign(W),H-sign(H)}),
	    [Segment|pmove_segments(Segments,P1,Length)];

	{line_to,P1} ->
	    {_P2,P3} = pmove(P0,P1,Length),
	    [{line_to,P3}|pmove_segments(Segments,P3,Length)]; 

	{line,{W,H}} ->
	    P1 = point_add(P0, {W-sign(W),H-sign(H)}),
	    {P2,P3} = pmove(P0,P1,Length),
	    {W1,H1} = point_subtract(P3,P2),
	    [{line,W1+sign(W1),H1+sign(H)}|
	     pmove_segments(Segments,P3,Length)]; 

	{line,P1,P2} ->
	    {Q1,Q2} = pmove(P1,P2,Length),
	    [{line,Q1,Q2} | pmove_segments(Segments,Q2,Length)];

	{round_to,Q,P1} ->
	    {_Q0,Q1} = pmove(P0,P1,Length),
	    [{round_to,Q,Q1}|pmove_segments(Segments,Q1,Length)];

	{round,Q,{W,H}} ->
	    P1 = point_add(P0, {W-sign(W),H-sign(H)}),
	    {P2,P3} = pmove(P0,P1,Length),
	    {W1,H1} = point_subtract(P3,P2),
	    [{round,Q,W1+sign(W1),H1+sign(H)}|
	     pmove_segments(Segments,P3,Length)];

	{round,Q,P1,P2} ->
	    {Q1,Q2} = pmove(P1,P2,Length),
	    [{round,Q,Q1,Q2} | pmove_segments(Segments,Q2,Length)]
    end;
pmove_segments([],_P0,_Length) ->
    [].

%% expand relative segments to absolute form
expand_segments(Segments) ->
    expand_segments(Segments, {0,0}).

expand_segments([Segment|Segments], P0) ->
    case Segment of
	{move_to,P1} ->
	    expand_segments(Segments, P1);

	{move,{W,H}} ->
	    P1 = point_add(P0, {W-sign(W),H-sign(H)}),
	    expand_segments(Segments, P1);

	{line_to,P1} ->
	    [{line,P0,P1} | expand_segments1(Segments,P1)];

	{round_to,Q,P1} ->
	    [{round,Q,P0,P1} | expand_segments1(Segments,P1)];

	{line,{W,H}} ->
	    P1 = point_add(P0, {W-sign(W),H-sign(H)}),
	    [{line,P0,P1} | expand_segments1(Segments,P1)];

	{round,Q,{W,H}} ->
	    P1 = point_add(P0, {W-sign(W),H-sign(H)}),
	    [{round,Q,P0,P1} | expand_segments1(Segments,P1)];

	{line,_P0,P1} ->
	    [Segment | expand_segments1(Segments,P1)];

	{round,_Q,_P0,P1} ->
	    [Segment | expand_segments1(Segments,P1)]
    end;
expand_segments([], _P0) ->
    [].

expand_segments1(Segments,P0) ->
    expand_segments(Segments, point_add(P0,{0,1})).

%%
%% Mirror (expanded) segments in x=Xm
%% You can use this generate (for example) Right segments
%% if Left segments are specified.
%%
mirror_segments([Segment|Segments], Xm) ->
    T = 2*Xm,
    case Segment of
	{move_to,{X,Y}} ->
	    [{move_to,{T-X,Y}}|mirror_segments(Segments,Xm)];
	{move,{W,H}} ->
	    [{move,-W,H}|mirror_segments(Segments,Xm)];
	{line_to,{X,Y}} ->	
	    [{line_to,{T-X,Y}}|mirror_segments(Segments,Xm)];
	{line,{W,H}} ->
	    [{move,-W,H}|mirror_segments(Segments,Xm)];
	{line,{X0,Y0},{X1,Y1}} ->
	    [{line,{T-X0,Y0},{T-X1,Y1}} | mirror_segments(Segments,Xm)];
	{round_to,Q,{X,Y}} ->
	    [{round_to,-Q,{T-X,Y}} | mirror_segments(Segments,Xm)];
	{round,Q,{W,H}} ->
	    [{round,-Q,{-W,H}} | mirror_segments(Segments,Xm)];
	{round,Q,{X0,Y0},{X1,Y1}} ->
	    [{round,-Q,{T-X0,Y0},{T-X1,Y1}} | mirror_segments(Segments,Xm)]
    end;
mirror_segments([],_Xm) ->
    [].

%% Find the next point after termination of round drawing
%% in the same direction as the termination point
round_next_point({round,Q,{X0,Y0},End={X1,Y1}}) ->
    Dx = X1-X0,
    Dy = Y1-Y0,
    case {Q, sign(Dx), sign(Dy)} of
	{1, Sx, Sy} when Sx==Sy -> %% dx=0
	    point_add(End,{0,Sy});
	{1, Sx, _Sy} -> %% dy=0
	    point_add(End,{Sx,0});
	{-1, Sx, Sy} when Sx==Sy -> %% dy=0
	    point_add(End,{Sx,0});
	{-1, _Sx, Sy} -> %% dx=0
	    point_add(End,{0,Sy})
    end.
    

mk_segments(S, Segments) ->
    mk_segments(S, Segments, {0,0}).

mk_segments(S, Segments, P0) ->
    Expanded = expand_segments(Segments, P0),
    lists:map(fun(Seg) -> mk_segment(S, Seg) end, Expanded).
%%
%% Segment shapes confined in a rectangle
%% from top-left corner {0,0} and width W and height H
%%

%% 
%% Some nice segment definitions
%%
%% top right ellipse part
s_ellipse_top_right(W, H) ->
    Color = {127,0,255,0},
    L = [{move_to,{0,0}},
	 {line,{0,H}}],
    R = [{move_to,{0,0}},
	 {round,1,{W,H}}],
    [{L,R,Color}].

%% bot right ellipse part
s_ellipse_bot_right(W, H) ->
    Color = {127,0,255,0},
    L = [{move_to,{0,0}},
	 {line,{0,H}}],
    R = [{move_to,{W-1,0}},
	 {round,1,{-W,H}}],
    [{L,R,Color}].

%% top left ellipse part
s_ellipse_top_left(W, H) ->
    Color = {127,0,255,0},
    L = [{move_to,{W-1,0}},
	 {round,-1,{-W,H}}],
    R = [{move_to,{W-1,0}},
	 {line,{0,H}}],
    [{L,R,Color}].

%% Bot-left ellipse part
s_ellipse_bot_left(W, H) ->
    Color = {127,0,255,0},
    L = [{move_to,{0,0}},
	 {round,-1,{W,H}}],
    R = [{move_to,{W-1,0}},
	 {line,{0,H}}],
    [{L,R,Color}].

%% Circle
s_circle(W, H) ->
    Color = {127,0,255,0},
    D = erlang:min(W,H),
    Wr  = D div 2,
    L = [{move_to, {Wr-1,0}},
	 {round,-1,{-Wr,Wr}},
	 {round,-1,{Wr,Wr}}],
    R = [{move_to,{Wr,0}},
	 {round,1,{Wr,Wr}},
	 {round,1,{-Wr,Wr}}],
    [{L,R,Color}].

%% Simple ellipse
s_ellipse(W, H) ->
    Color = {127,0,255,0},
    A1 = W div 2,  A2 = W - A1,
    B1 = H div 2,  B2 = H - B1,
    L = [{move_to,{A1-1,0}},
	 {round,-1,{-A1,B1}},
	 {round,-1,{A1,B2}}],
    R = [{move_to,{(W-A2),0}},
	 {round,1,{A2,B1}},
	 {round,1,{-A2,B2}}],
    [{L,R,Color}].

%% Round rectangle
s_round_rect(W, H) ->
    s_ellipse_rect(W, H, 5, 5).

s_ellipse_rect(W, H, Rw, Rh) ->
    Color = {127,0,255,0},
    Hn = (H - 2*Rh),
    L = [{move_to,{Rw-1,0}},
	 {round,-1,{-Rw,Rh}},
	 {line,{0,Hn}},
	 {round,-1,{Rw,Rh}}],
    R = [
	 {move_to,{(W-Rw),0}},
	 {round,1,{Rw,Rh}},
	 {line, {0,Hn}},
	 {round,1,{-Rw,Rh}}],
    [{L,R,Color}].

%% Fat circle contained in W,H with radius R and border width Bw
s_circle_border(W, H, R, Bw) when Bw > 0, Bw < R ->
    Color1 = {127,0,255,0},
    Color2 = {127,0,0,255},
    D = erlang:min(W,H),
    Wro  = D div 2,        %% outer radius
    Wri  = (Wro - Bw),     %% inner radius
    Wxi  = Wri+1,
    L1 = [{move_to, {Wro-1,0}},
	  {round,-1,{-Wro,Wro}},
	  {round,-1,{Wro,Wro}}],
    R1 = [{move_to, {Wro-1,0}},
	  {line,    {0,Bw-1}},
	  {round,-1,{-Wxi,Wri}},
	  {line,    {0,1}},
	  {round,-1,{Wxi,Wri}},
	  {line,    {0,Bw}}],

    L2 = [{move_to, {Wro,0}},
	  {line,    {0,Bw-1}},
	  {round,1, {Wxi,Wri}},
	  {line,    {0,1}},
	  {round,1, {-Wxi,Wri}},
	  {line,    {0,Bw}}],
    R2 = [{move_to,{Wro,0}},
	  {round,1,{Wro,Wro}},
	  {round,1,{-Wro,Wro}}],
    [{L1,R1,Color1},{L2,R2,Color2}].

s_round_rect_border(W, H, Rw, Bw) ->
    s_ellipse_rect_border(W, H, Bw, Rw, Rw).

%%
s_ellipse_rect_border(W, H, Bw, Rw, Rh) when Bw > 0, Bw < Rw, Bw < Rh ->
    Color1 = {127,0,255,0},
    Color2 = {127,0,0,255},
    Hn = (H - 2*Rh),
    Rwi = (Rw-Bw),
    Rhi = (Rh-Bw),
    Rxi = Rwi+1,
    L1 = [{move_to,{Rw-1,0}},
	  {round,-1,{-Rw,Rh}},
	  {line,{0,Hn}},
	  {round,-1,{Rw,Rh}}],
    R1 = [{move_to,{Rw-1,0}},
	  {line,{0,Bw-1}},
	  {round,-1,{-Rxi,Rhi}},
	  {line,{0,Hn+1}},
	  {round,-1,{Rxi,Rhi}},
	  {line,{0,Bw}}],
    L2 = [{move_to,{(W-Rw),0}},
	  {line,{0,Bw-1}},
	  {round,1,{Rxi,Rhi}},
	  {line,{0,Hn+1}},
	  {round,1,{-Rxi,Rhi}},
	  {line,{0,Bw}}],
    R2 = [{move_to,{(W-Rw),0}},
	  {round,1,{Rw,Rh}},
	  {line,{0,Hn}},
	  {round,1,{-Rw,Rh}}],

    L3 = [{move_to,{Rw,0}},
	  {line,{0,Bw}}],
    R3 = [{move_to,{(W-Rw)-1,0}},
	  {line,{0,Bw}}],

    L4 = [{move_to,{Rw,(H-Bw)}},
	  {line,{0,Bw}}],
    R4 = [{move_to,{(W-Rw)-1,(H-Bw)}},
	  {line,{0,Bw}}],
    [{L1,R1,Color1},{L2,R2,Color1},{L3,R3,Color2},{L4,R4,Color2}].

%%
%% Generate two segments representing the polygon
%% offseted around a calculated center
%%
s_poly_c(Ps) ->
    %% calculate a mid point
    N = length(Ps),
    ?assert(N >= 3, to_few_points),
    {Xs,Ys} = lists:foldl(fun({X,Y},{Xs,Ys}) -> {X+Xs,Y+Ys} end,{0,0},Ps),
    C = {Xs div N,Ys div N},
    %% io:format("Center: ~p\n", [C]),
    %% re-offset all points
    Ps1 = lists:map(fun(P) -> point_subtract(P,C) end, Ps),
    %% io:format("Ps1: ~p\n", [Ps1]),
    %% Ps1 contains point around (0,0)
    %% verify that polygon is ordered 
    Orientation = verify_poly_orientation(Ps1),
    ?assert(Orientation /= 0, bad_orientation),
    %% verify that polygon is convex
    ?assert(verify_poly_convex(Ps1), none_convex),
    %% generate Left and Right segment lists
    PsI = lists:zipwith(fun({X,Y},I) -> {{X,Y},I} end, Ps1, lists:seq(1,N)),
    {Min,I} = hd(lists:sort(fun({P,_},{Q,_}) -> point_less(P,Q) end, PsI)),
    %% io:format("Min: ~p (~w)\n", [Min,I]),    
    {Max,J} = hd(lists:sort(fun({P,_},{Q,_}) -> point_greater(P,Q) end, PsI)),
    %% io:format("Max: ~p (~w)\n", [Max,J]),
    %% Generate Left and Right segment until I and J meet
    {L,R} = s_poly(list_to_tuple(Ps1), I, J),
    {C,L,R}.

%%
%% creat polygon segments from points Ps
%%
s_poly(Ps) ->
    {C,L,R} = s_poly_c(Ps),
    %% ?assert(verify_monotonic_y(L), none_monotonic),
    %% ?assert(verify_monotonic_y(R), none_monotonic),
    [{offset_segments(L, C),  offset_segments(R, C)}].

s_poly_border(Ps,Bw) ->
    {C,L,R} = s_poly_c(Ps),
    L1 = pmove_segments(L, Bw),
    R1 = pmove_segments(R, Bw),
    %% L2 = offset_segments(L1, C),
    %% R2 = offset_segments(R1, C),
    {line,Min1,_} = hd(L1),
    {line,Min2,_} = hd(L),

    {line,_,Max1} = lists:last(L1),
    {line,_,Max2} = lists:last(L),

    Ls = {offset_segments(L1,C),
	  offset_segments([{line,Min1,Min2}|L]++[{line,Max2,Max1}], C)},

    Rs = {offset_segments([{line,Min1,Min2}|R]++[{line,Max2,Max1}], C),
	  offset_segments(R1, C)},
    [Ls, Rs].
    


s_poly(Pts, I, J) ->
    P = element(I, Pts),   %% min point
    Li = pindex(I-1,Pts),
    L = element(Li, Pts),
    Ri = pindex(I+1,Pts),
    R = element(Ri, Pts),
    case point_left(P,R) of
	true ->
	    %% io:format("P=~p left of R=~p\n", [P,R]),
	    Ls = s_poly_seg(Pts,Li,L,[{line,P,L}],-1,J),
	    Rs = s_poly_seg(Pts,Ri,R,[{line,P,R}],1,J),
	    {s_poly_seg_adjust(Pts,J,Ls),lists:reverse(Rs)};
	false ->
	    case point_right(P,R) of
		true ->
		    %% io:format("P=~p right of R=~p\n", [P,R]),
		    Ls = s_poly_seg(Pts,Ri,R,[{line,P,R}],1,J),
		    Rs = s_poly_seg(Pts,Li,L,[{line,P,L}],-1,J),
		    {s_poly_seg_adjust(Pts,J,Ls),lists:reverse(Rs)};
		false ->
		    case point_left(P,L) of
			true ->
			    %% io:format("P=~p left of L=~p\n", [P,L]),
			    Ls = s_poly_seg(Pts,Ri,R,[{line,P,R}],1,J),
			    Rs = s_poly_seg(Pts,Li,L,[{line,P,L}],-1,J),
			    {s_poly_seg_adjust(Pts,J,Ls),lists:reverse(Rs)};
			false ->
			    %% io:format("P=~p right of L=~p\n", [P,L]),
			    Ls = s_poly_seg(Pts,Li,L,[{line,P,L}],-1,J),
			    Rs = s_poly_seg(Pts,Ri,R,[{line,P,R}],1,J),
			    {s_poly_seg_adjust(Pts,J,Ls),lists:reverse(Rs)}
		    end
	    end
    end.

%% travers left segments - when J is found then backtrack
%% and remove segments with same Y as point J
s_poly_seg(Pts,Pi,P,Ps,S,J) ->
    io:format("s_poly_seg: P=~p Pi=~w\n", [P,Pi]),
    if Pi == J ->
	    Ps;
       true ->
	    Qi = pindex(Pi+S,Pts),
	    Q = element(Qi,Pts),
	    s_poly_seg(Pts,Qi,Q,[{line,P,Q}|Ps],S,J)
    end.

s_poly_seg_adjust(_Pts,_J,Ps) ->
    lists:reverse(Ps).

%% s_poly_seg_adjust(Pts,J,Ps) ->
%%    Pj = element(J,Pts),
%%    s_poly_seg_adjust(Pj,Ps).

%% s_poly_seg_adjust(Pj={_,Pjy},[{line,{_,Pjy},{_,Pjy}}|Ps]) ->
%%    io:format("segment: removed\n"),
%%    s_poly_seg_adjust(Pj,Ps);
%% s_poly_seg_adjust(_Pj,Ps) ->
%%    lists:reverse(Ps).


%% Check that poly is either clockwise or counter clockwise
verify_poly_orientation([{X1,Y1},{X2,Y2}|Ps]) ->
    A1 = math:atan2(Y1, X1)*(180/math:pi()),
    A2 = math:atan2(Y2, X2)*(180/math:pi()),
    D = angle_diff(A1,A2),
    %% io:format("A1=~p, A2=~p, D1 = ~p\n", [A1,A2,D]),
    if D == 0.0 ->
	    0;
       true ->
	    verify_poly_orientation(Ps, 2, A2, sign(D))
    end.

verify_poly_orientation([{X,Y}|Ps], I, A1, Sign) ->
    A2 = math:atan2(Y, X)*(180/math:pi()),
    D = angle_diff(A1, A2),
    %% io:format("A1=~p, A2=~p, D~w = ~p\n", [A1,A2,I,D]),    
    Sign1 = sign(D),
    if D == 0.0 ->
	    0;
       Sign == Sign1 ->
	    verify_poly_orientation(Ps, I+1, A2, Sign);
       true ->
	    0
    end;
verify_poly_orientation([], _I, _A1, Sign) ->
    Sign.

%% verify that the polygon is convex
%% no segment edge (line Pi Pi+1) intersect
%% other intersect any other segments! (not line)
verify_poly_convex(_Ps) ->
    %% not yet.
    true.


%% return angle diff (B-A)  (-180, 180)  
angle_diff(A, B) ->
    A1 = angle_mod(A),
    B1 = angle_mod(B),
    D1 = abs(B1 - A1),
    S1 = sign(B1 - A1),
    S1*(if D1 > 180 -> D1 - 360; true -> D1 end).

angle_mod(A) when A < 0 ->
    fmod(fmod(A,360)+360, 360);
angle_mod(A) ->
    fmod(A, 360).

fmod(N, D) ->
    Q = trunc(N / D),
    N - Q*D.


%% Code to generate a convex polygon (point on circle edge) 
poly_circle_points(R, N) ->
    poly_circle_points(0, 0, 0, R, N).

poly_circle_points(A, R, N) ->
    poly_circle_points(A, 0, 0, R, N).

poly_circle_points(A, Cx, Cy, R, N) ->
    Ai  = 360.0 / N,
    lists:map(
      fun(I) -> 
	      Deg = A + I*Ai,
	      Ar = rad(Deg),
	      X = trunc(math:cos(Ar)*R + Cx),
	      Y = trunc(math:sin(Ar)*R + Cy),
	      {X,Y}
      end,
      lists:seq(0,N-1)).

%%
%% Do a single step along the line
%% lstep run from (0,0) -> (dx,dy)
%%
lstep(L=#line{ n=N, x=X, f=F}) ->
    if F >= 0 ->
	    L#line { n=N-1,
		     x=X+1,
		     y=L#line.y+1,
		     f=F+L#line.df,
		     axis = ?AXIS_XY
		    };
       true ->
	    L#line { n=N-1,
		     x=X+1,
		     f=F+L#line.d,
		     axis = ?AXIS_X
		    }
    end.

%%
%% Do a single step along the elliptic curve
%% rstep run from (a,0) towards (0,b)
%%
rstep(R) ->
    Fx =  R#round.f - R#round.pxb,  %% fx  = f - pxb
    Fxy = Fx + R#round.pya,         %% fxy = f - pxb + pya
    Fy  = R#round.f + R#round.pya,  %% fy  = f + pya
    if abs(Fx) < abs(Fxy), abs(Fx) < abs(Fy) ->
	    R#round { x = R#round.x - 1,
		      f = Fx,
		      axis = ?AXIS_X,
		      pxb = R#round.pxb - 2*R#round.b2 };
       abs(Fxy) < abs(Fy) ->
	    R#round { x = R#round.x - 1,
		      y = R#round.y + 1,
		      f = Fxy,
		      axis = ?AXIS_XY,
		      pya = R#round.pya + 2*R#round.a2,
		      pxb = R#round.pxb - 2*R#round.b2 };
       true ->
	    R#round { y = R#round.y + 1,
		      f = Fy,
		      axis = ?AXIS_Y,
		      pya = R#round.pya + 2*R#round.a2 }
    end.

%%
%% General segment step
%%
step(L = #line {}) ->   lstep(L);
step(R = #round{}) ->   rstep(R).

%%
%% Check if drawing is done
%%
done(#line {n=N}) -> (N =< 0);
done(#round {x=X,y=Y,b=B}) -> (X =< 0) andalso (Y >= B);
done(_) -> false.

%%
%% Get current coordinate
%%    
coord(#line  { x=X, y=Y, pt=Pt}) -> Pt(X, Y);
coord(#round { x=X, y=Y, pt=Pt}) -> Pt(X, Y).

%% Return anti alias alignment
alias(#line  { ap=Ap }) -> Ap();
alias(#round { ap=Ap }) -> Ap().

%% Get internal x coordinate
segment_x(#line { x=X }) -> X;
segment_x(#round { x=X }) -> X.

segment_y(#line {  y=X }) -> X;
segment_y(#round { y=X }) -> X.


%% draw vertical line at x=X
vline(Pix,I,Ys,X,Y) ->
    if I =< 0 -> ok;
       true ->
	    epixmap:draw_point(Pix, X, Y+Ys),
	    vline(Pix,I-1,Ys,X,Y+Ys)
    end.

%% draw horizontal line at y=Y
hline(Pix,I,Xs,X,Y) ->
    if I =< 0 ->
	    ok;
       true ->
	    epixmap:draw_point(Pix, X, Y),
	    hline(Pix,I-1,Xs,X+Xs,Y)
    end.

trace_line(S, P0, P1) ->
    L = mk_line(S, P0,P1),
    Fmt = ?recf(line,L), io:format("~s\n", [Fmt]),
    trace_out_line(S, L).

trace_out_line(S, F) ->
    plot(S, F, S#s.fg1),
    trace_out_line1(S, F).

trace_out_line1(S, F) ->
    F1 = step(F),
    plot(S, F1, S#s.fg1),
    %% update(S),
    %% timer:sleep(1),
    case done(F1) of
	true ->
	    ok;
	false ->
	    trace_out_line1(S, F1)
    end.

trace_round(S, Q, P0, P1) ->
    R = mk_round(S, Q, P0, P1),
    trace_out_line(S, R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Trace two Y-oriented segments
%% fill each "scan" line 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fill_2_segments(S, As=[A|_], Bs=[B|_]) ->
    {Ax,Ay} = coord(A),
    {Bx,By} = coord(B),
    Ay = By,  %% assert
    fill(S#s.pix,Ax,Bx,Ay),
    fill_2_segments_step(S, As, Bs).


fill_2_segments_step(_S,[],[]) ->
    ok;
fill_2_segments_step(S,As,Bs) ->
    As1 = step_y(S,As),
    Bs1 = step_y(S,Bs),
    if As1==[]; Bs1 == [] ->
	    ok;
       true ->
	    {Ax,Ay} = coord(hd(As1)),
	    {Bx,By} = coord(hd(Bs1)),
	    if Ay == By ->
		    fill(S#s.pix,Ax,Bx,Ay),
		    %% update(S),
		    %% timer:sleep(10),
		    fill_2_segments_step(S, As1, Bs1);
	       true ->
		    io:format("BadY: Ay=~w, By=~w\n", [Ay,By]),
		    erlang:error(badval)
	    end
    end.

%% step_y - y must be increase >= along the segments
step_y(_S,[]) ->
    [];
step_y(S,Segments=[Segment|_]) ->
    {_,Y} = coord(Segment), %% current coordinate
    step_y(S,Y,Segments).
       
step_y(S,Y0,[Segment|SegmentTail]) ->
    case done(Segment) of
	true ->
	    case SegmentTail of
		[] -> [];
		[Segment1|_] ->
		    {_,Y1} = coord(Segment1),
		    if Y1 == Y0 ->
			    step_y(S,Y0,SegmentTail);
		       Y1 == Y0+1 ->
			    SegmentTail;
		       true ->
			    io:format("Y0=~w, Y1=~w\n", [Y0,Y1]),
			    erlang:error(bad_increment)
		    end
	    end;
	false ->
	    Segment1 = step(Segment),
	    {_,Y1} = coord(Segment1),
	    if Y1 == Y0 ->
		    %% epixmap:draw_point(S#s.pix, X1, Y1), ?
		    step_y(S,Y0,[Segment1|SegmentTail]);
	       Y1 == Y0+1 ->
		    [Segment1|SegmentTail];
	       true ->
		    io:format("Y0=~w, Y1=~w\n", [Y0,Y1]),
		    erlang:error(bad_increment)
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Trace two Y-oriented segments
%% plot each point along the way
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot_2_segments(S, As=[A|_], Bs=[B|_]) ->
    {Ax,Ay} = coord(A),
    {Bx,By} = coord(B),
    Ay = By,  %% assert
    epixmap:draw_point(S#s.pix, Ax, Ay),
    update(S),
    plot_2_segments_step(S, As, Bs).


plot_2_segments_step(_S,[],[]) ->
    ok;
plot_2_segments_step(S,As,Bs) ->
    As1 = step_yx(S,As),
    Bs1 = step_yx(S,Bs),
    if As1==[]; Bs1 == [] ->
	    ok;
       true ->
	    {Ax,Ay} = coord(hd(As1)),
	    {Bx,By} = coord(hd(Bs1)),
	    if Ay == By ->
		    epixmap:draw_point(S#s.pix, Ax, Ay),
		    epixmap:draw_point(S#s.pix, Bx, By),
		    update(S), 
		    plot_2_segments_step(S, As1, Bs1);
	       true ->
		    io:format("BadY: Ay=~w, By=~w\n", [Ay,By]),
		    erlang:error(badval)
	    end
    end.

%% step_y - y must be increase >= along the segments
%% plot x,y values while y is fixed
step_yx(_S,[]) ->
    [];
step_yx(S,Segments=[Segment|_]) ->
    {_,Y} = coord(Segment), %% current coordinate
    step_yx(S,Y,Segments).
       
step_yx(S,Y0,[Segment|SegmentTail]) ->
    case done(Segment) of
	true ->
	    case SegmentTail of
		[] -> [];
		[Segment1|_] ->
		    {_X1,Y1} = coord(Segment1),
		    if Y1 == Y0 ->
			    step_yx(S,Y0,SegmentTail);
		       Y1 == Y0+1 ->
			    SegmentTail;
		       true ->
			    io:format("Y0=~w, Y1=~w\n", [Y0,Y1]),
			    erlang:error(bad_increment)
		    end
	    end;
	false ->
	    Segment1 = step(Segment),
	    {X1,Y1} = coord(Segment1),
	    if Y1 == Y0 ->
		    epixmap:draw_point(S#s.pix, X1, Y1),
		    update(S),
		    step_yx(S,Y0,[Segment1|SegmentTail]);
	       Y1 == Y0+1 ->
		    [Segment1|SegmentTail];
	       true ->
		    io:format("Y0=~w, Y1=~w\n", [Y0,Y1]),
		    erlang:error(bad_increment)
	    end
    end.

    
%% Fill pixels from X0 to X1 with color Color
fill(Pix,X0,X1,Y) ->
    Dx = abs(X1 - X0),
    Xs = sign(X1 - X0),
    hline(Pix,Dx+1,Xs,X0,Y).

plot(S,F,Color) ->
    if S#s.aalias ->
	    plota(S,F,Color);
       true ->
	    {X,Y} = coord(F),
	    epixmap:put_pixel(S#s.pix,X,Y,[blend],Color)
    end.

plota(S,F,Color) ->
    {X,Y} = coord(F),
    {Sx,Sy} = alias(F),
    Pix = S#s.pix,
    if Sx==0, Sy==0 ->
	    epixmap:put_pixel(S#s.pix,X,Y,[blend],Color);
       true ->
	    Fx = if F#line.axis == ?AXIS_XY ->
			 F#line.f - F#line.df - F#line.dx;
		    F#line.axis == ?AXIS_X  ->
			 F#line.f - F#line.d + F#line.dx;
		    true ->
			 0
		 end,
	    T = Fx*F#line.invL,
	    %% fix axis (X+1,X-1) | X+1 | X-1 | (Y+1,Y-1) | Y+1 | Y-1

	    D3 = (1 - (abs(T)*(2/3))),
	    A3 = trunc(255*D3*D3),
	    %% A3 = (255 - (A1+A2)),
	    plotaa(Pix,X,Y,  Color,A3),

	    D1 = (1 - (F#line.invL2dx - T)*(2/3)),
	    A1 = trunc(255*D1*D1),
	    plotaa(Pix,X+Sx,Y+Sy,Color,A1),

	    D2 = (1 - (F#line.invL2dx + T)*(2/3)),
	    A2 = trunc(255*D2*D2),
	    plotaa(Pix,X-Sx,Y-Sy,Color,A2)

    end.

plotaa(Pix,X,Y,{R,G,B},A) when A >= 0, A =< 255 ->
    ColorA = {A,R,G,B},
    epixmap:put_pixel(Pix,X,Y,[blend],ColorA).



%%
%% Test code
%%
start() ->
    start(poly).

start(Func) ->
    start(Func, 640, 480).

start(Func, W, H) ->
    epic:start([]),
    {ok,Win} = ewindow:create(50,50,W,H,[button_press,key_press]),
    ewindow:attach(Win),
    [{_Delay,Image}|_] = eimage:file(filename:join(code:priv_dir(epic),
						   "erlang.png")),
    {ok,Pix} = epixmap:create(W,H),
    epixmap:attach(Pix),

    S0 = #s { func=Func, tmo=infinity, win = Win, pix = Pix, 
	      image = Image, width = W, height = H },
    S1 = ?MODULE:Func(init, S0),
    S2 = draw(S1),
    loop(S2).

loop(S) ->
    Win = S#s.win,
    receive
	{eevent,Win, destroy} ->
	    stop(S);
	{eevent,Win, close} ->
	    stop(S);
	{eevent,_Win,{key_press, $q, _Mod, _Code}} ->
	    stop(S);
	{eevent,_Win,{key_press, Key, _Mod, _Code}} ->
	    case Key of
		$1 -> loop(S#s { tmo=1000 });
		$2 -> loop(S#s { tmo=500 });
		$3 -> loop(S#s { tmo=250 });
		$4 -> loop(S#s { tmo=125 });
		$5 -> loop(S#s { tmo=75 });
		$6 -> loop(S#s { tmo=50 });
		$7 -> loop(S#s { tmo=25 });
		$8 -> loop(S#s { tmo=10 });
		$9 -> loop(S#s { tmo=1 });
		_ -> loop(S)
	    end;
	{eevent,Win,{button_press, [left], _Where}} ->
	    if is_integer(S#s.tmo) ->
		    loop(S#s { tmo = infinity });
	       true ->
		    S1 = draw(S),
		    loop(S1)
	    end;
	Event ->
	    io:format("Event: ~p\n", [Event]),
	    loop(S)
    after S#s.tmo ->
	    S1 = draw(S),
	    loop(S1)
    end.

stop(S) ->
    epixmap:destroy(S#s.pix),
    ewindow:destroy(S#s.win),
    ok.

update(S) ->
    epixmap:draw(S#s.pix,S#s.win,0,0,0,0,S#s.width,S#s.height),
    S.

draw(S) ->
    epixmap:fill(S#s.pix, S#s.bg), %% clear 
    S1 = ?MODULE:(S#s.func) (next, S),
    update(S1).

%%
%% Util
%%
draw_lrs(S, [LR|LRs], Offset) ->
    {L0,R0,C} = case LR of
		    {Li,Ri} -> {Li,Ri,S#s.fill1};
		    {_,_,_} -> LR
		end,
    L = offset_segments(L0,Offset),
    R = offset_segments(R0,Offset),
    io:format("L0: ~p\n", [expand_segments(L)]),
    io:format("R0: ~p\n", [expand_segments(R)]),
    Left  = mk_segments(S, L),
    Right = mk_segments(S, R),
    egc:set_line_style([blend]),
    egc:set_foreground_color(C),
    try fill_2_segments(S,Left,Right) of
	_ -> ok
    catch
	error:Err ->
	    io:format("L: ~p\n", [expand_segments(L)]),
	    io:format("R: ~p\n", [expand_segments(R)]),
	    erlang:error(Err)
    end,
    draw_lrs(S, LRs, Offset);
draw_lrs(_S, [],_) ->
    ok.


%%
%% Round Rect
%%
round_rect(init, S) ->
    S#s { arg = 0 };
round_rect(next, S) ->
    W = random_interval(80, S#s.width-10),
    H = random_interval(80, S#s.height-10),
    Rw = random_interval(10, 39),
    Rh = random_interval(10, 39),
    X = (S#s.width - W) div 2,
    Y = (S#s.height - H) div 2,
    io:format("X=~w, Y=~w, W=~w, H=~w, Rw=~w, Rh=~w\n", [X,Y,W,H,Rw,Rh]),
    {L,R} = s_ellipse_rect(W, H, Rw, Rh),
    io:format("L = ~p\n", [expand_segments(L)]),
    io:format("R = ~p\n", [expand_segments(R)]),
    L1 = offset_segments(L, {X,Y}),
    R1 = offset_segments(R, {X,Y}),
    Left  = mk_segments(S, L1),
    Right = mk_segments(S, R1),
    egc:set_foreground_color(S#s.fill1),
    fill_2_segments(S,Left,Right),
    S.

%%
%% Segments test
%%
segments(init, S) ->
    S#s { arg = 0 };
segments(next, S) ->
    W = random_interval(80, S#s.width-10),
    H = random_interval(80, S#s.height-10),
    Rw = random_interval(10, 39),                  %% x radius A
    Rh = random_interval(10, 39),                  %% y radius B
    Bw = random_interval(2, erlang:min(Rw,Rh)-1),  %% border width
    X = (S#s.width - W)  div 2,  %% X for centered box
    Y = (S#s.height - H) div 2,  %% Y for centered box
    I = S#s.arg rem 11,
    io:format("~w: X=~w, Y=~w, W=~w, H=~w, Rw=~w, Rh=~w Bw=~w\n", 
	      [I,X,Y,W,H,Rw,Rh,Bw]),
    egc:draw(
      fun() ->
	      {ok,{IW,IH}} = epixmap:get_size(S#s.image),
	      XI = (S#s.width - IW)  div 2,
	      YI = (S#s.height - IH) div 2,
	      epixmap:copy_area(S#s.image, S#s.pix,
				0, 0, XI, YI, IW, IH),
	      egc:set_border_color({255,0,0}),
	      %% egc:set_fill_color({255,0,0}),
	      egc:set_fill_style([none]),
	      epixmap:draw_rectangle(S#s.pix, X-1,Y-1,W+2,H+2)
      end),
    LRs = case I of
	      0 ->
		  io:format("Circle Border\n"),
		  s_circle_border(W, H, Rw, Bw);
	      1 ->
		  io:format("Round-Rect Border\n"),
		  s_round_rect_border(W, H, Rw, Bw);
	      2 ->
		  io:format("Ellipse-Rect Border\n"),
		  s_ellipse_rect_border(W, H, Bw, Rw, Rh);
	      3 ->
		  io:format("Ellipse Top-Right\n"),
		  s_ellipse_top_right(W,H);
	      4 ->
		  io:format("Ellipse Top-Left\n"),
		  s_ellipse_top_left(W,H);
	      5 ->
		  io:format("Ellipse Bot-Left\n"),
		  s_ellipse_bot_left(W,H);
	      6 ->
		  io:format("Ellipse Bot-Right\n"),
		  s_ellipse_bot_right(W,H);
	      7 ->
		  io:format("Ellipse\n"),
		  s_ellipse(W,H);
	      8 ->
		  io:format("Circle\n"),
		  s_circle(W,H);
	      9 ->
		  io:format("Round-Rect\n"),
		  s_round_rect(W,H);
	      10 ->
		  io:format("Ellipse-Rect\n"),
		  s_ellipse_rect(W,H,Rw,Rh)
	  end,
    draw_lrs(S, LRs, {X,Y}),
    S#s { arg = I+1 }.


%%
%% POLY DEMO
%%
poly(init, S) ->
    S#s { arg = 0 };
poly(next, S) ->
    A = S#s.arg,
    Cx = S#s.width div 2,
    Cy = S#s.height div 2,
    Radius  = 200,
    N       = 10,
    Bw      = 5,
    Poly = poly_circle_points(A,Cx,Cy,Radius,N),
    LRs = if A band 1 == 0 ->
		  s_poly(Poly);
	     true ->
		  s_poly_border(Poly, Bw)
	  end,
    draw_lrs(S, LRs, {0,0}),
    %% fill_2_segments(S,Left,Right),
    %% plot_2_segments(S,Left,Right),
    S#s { arg = (A+1) rem 360 }.

%%
%% ROUND DEMO
%%
round(init, S) ->
    S#s { arg = 0 };
round(next, S) ->
    A = 70,
    B = 90,
    Xc = S#s.width div 2,
    Yc = S#s.height div 2,
    Q = S#s.arg+1,
    io:format("round: q=~w\n", [Q]),
    case Q of
	1 -> trace_round(S, 1, {Xc+A,Yc}, {Xc,Yc+B});
	2 -> trace_round(S, 1, {Xc,Yc+B}, {Xc-A,Yc});
	3 -> trace_round(S, 1, {Xc-A,Yc}, {Xc,Yc-B});
	4 -> trace_round(S, 1, {Xc,Yc-B}, {Xc+A,Yc});
	5 -> trace_round(S, -1, {Xc+A,Yc}, {Xc,Yc-B});
	6 -> trace_round(S, -1, {Xc,Yc-B}, {Xc-A,Yc});
	7 -> trace_round(S, -1, {Xc-A,Yc}, {Xc,Yc+B});
	8 -> trace_round(S, -1, {Xc,Yc+B}, {Xc+A,Yc})
    end,
    S#s { arg = Q rem 8 }.

%%
%% TRIANGLE DEMO
%%
triangle(init, S) ->
    S#s { arg = 0 };
triangle(next, S) ->
    A = S#s.arg,
    P0 = random_point(S),
    P1 = random_point(S),
    P2 = random_point(S),
    Bw = 5,
    Poly = [P0,P1,P2],
    io:format("Poly = ~p\n", [Poly]),
    LRs = case A band 1 of
	      0 -> s_poly(Poly);
	      1 -> s_poly_border(Poly, Bw)
	  end,
    draw_lrs(S, LRs, {0,0}),
    S#s { arg = A + 1}.

%%
%% LINE DEMO
%%
line(init, S) ->
    S#s { arg = 0, bg={255,255,255}, fg1={0,0,0} };
line(next, S) ->
    A = S#s.arg,
    Cx = S#s.width div 2,
    Cy = S#s.height div 2,
    R  = 200,
    P0 = {Cx,Cy},
    P1 = {Cx + round(R*math:cos(rad(A))), 
	  Cy + round(R*math:sin(rad(A)))},
    io:format("~w: ~w -> ~w\n", [A,P0,P1]),
    trace_line(S,P0,P1),
    S#s { arg = (A+1) rem 360 }.

%%
%% FATLINE DEMO
%%
fatline(init, S) ->
    S#s { arg = 10, bg={0,0,0}, fill1={255,0,0} };
fatline(next, S) ->
    A = S#s.arg,
    Cx = S#s.width div 2,
    Cy = S#s.height div 2,
    R  = 200,
    Width = 5,
    P0 = {Cx,Cy},
    P1 = {Cx + round(R*math:cos(rad(A))), 
	  Cy + round(R*math:sin(rad(A)))},
    {P2,P3} = xparallel(P0,P1,Width),
    LRs = s_poly([P0,P2,P3,P1]),
    draw_lrs(S, LRs, {0,0}),    
    S#s { arg = (A+1) rem 360 }.

%%
%% ALINE DEMO
%%
aaline(init, S) ->
    S#s { aalias=true, arg = 0, bg={255,255,255}, fg1={0,0,0} };
aaline(next, S) ->
    A = S#s.arg,
    Cx = S#s.width div 2,
    Cy = S#s.height div 2,
    R  = 200,
    P0 = {Cx,Cy},
    P1 = {Cx + round(R*math:cos(rad(A))), 
	  Cy + round(R*math:sin(rad(A)))},
    io:format("~w: ~w -> ~w\n", [A,P0,P1]),
    trace_line(S,P0,P1),
    S#s { arg = (A+1) rem 360 }.

%%
%% LINES DEMO
%%
lines(init, S) ->
    {A,B,C} = erlang:now(),
    random:seed(A, B, C),
    {X0,Y0} = random_point(S),
    {X1,Y1} = random_point(S),
    X0s = random:uniform(3),
    Y0s = random:uniform(3),
    X1s = random:uniform(3),
    Y1s = random:uniform(3),
    Color = random_color(),
    L = {{X0,Y0,X1,Y1},{X0s,Y0s,X1s,Y1s},Color},
    S#s { arg = {10,1.1,[L]}};
lines(next, S) ->
    {N,K,[L|Ls]} = S#s.arg,
    C1 = random_color(),
    {P1={X0,Y0,X1,Y1},S1,_C} = update_line(S, L),
    L1 = {P1,S1,C1},
    Ls1 = update_trail(N, K, [L|Ls]),
    trace_line(S#s { fg1 = C1 }, {X0,Y0}, {X1,Y1}),
    lists:foreach(fun({{LX0,LY0,LX1,LY1},_,Lc}) ->
			  trace_line(S#s { fg1=Lc}, {LX0,LY0},{LX1,LY1})
		  end, Ls1),
    S#s { arg = {N,K,[L1|Ls1]}}.


bounce(V,Step,Min,_Max) when V < Min -> -Step;
bounce(V,Step,_Min,Max) when V > Max -> -Step;
bounce(_V,Step,_Min,_Max) -> Step.
    

update_line(S,{{X0,Y0,X1,Y1},{X0s,Y0s,X1s,Y1s},C}) ->
    X0_1 = X0 + X0s,
    Y0_1 = Y0 + Y0s,
    X1_1 = X1 + X1s,
    Y1_1 = Y1 + Y1s,
    X0s_1 = bounce(X0_1, X0s, 0, S#s.width),
    X1s_1 = bounce(X1_1, X1s, 0, S#s.width),
    Y0s_1 = bounce(Y0_1, Y0s, 0, S#s.height),
    Y1s_1 = bounce(Y1_1, Y1s, 0, S#s.height),
    {{X0_1,Y0_1,X1_1,Y1_1}, {X0s_1,Y0s_1,X1s_1,Y1s_1},C}.


update_trail(_,_K,[]) ->
    [];
update_trail(0,_K,_) ->
    [];
update_trail(I,K,[{P,S,{R,G,B}}|Ls]) ->
    R1 = trunc(R / K),
    G1 = trunc(G / K),
    B1 = trunc(B / K),
    [{P,S,{R1,G1,B1}}|update_trail(I-1,K+0.1,Ls)].

random_color() ->
    {random:uniform(200)+50,
     random:uniform(200)+50,
     random:uniform(200)+50}.

random_point(S) ->
    X = random:uniform(S#s.width)-1,
    Y = random:uniform(S#s.height)-1,
    {X,Y}.

random_interval(Min,Max) ->
    V = random:uniform((Max - Min)+1)-1,  %% 0 .. (Max-Min)
    Min + V.

rad(Deg) ->
    (math:pi()/180)*Deg.

clamp(V) when V > 255 -> 255;
clamp(V) when V < 0 -> 0;
clamp(V) -> V.

add(_Value, undefined) ->
    undefined;
add(Value, {R,G,B}) ->
    {clamp(R+Value),clamp(G+Value),clamp(B+Value)}.
    
