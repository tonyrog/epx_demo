%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Basic bresenham line drawing algorithm
%%% @end
%%% Created :  4 Oct 2018 by Tony Rogvall <tony@rogvall.se>

-module(bresenham).

-export([draw_line/4]).
-export([draw_triangle/5]).
-export([test_sort_2/0, test_sort_3/0]).

draw_line(Pixmap,{X0,Y0},{X1,Y1},Color) ->
    draw_line(Pixmap,{X0,Y0,1.0},{X1,Y1,1.0},Color);
draw_line(Pixmap, P0, P1, Color) ->
    {{X0,Y0,_},{X1,Y1,_}} = sort_y(P0,P1),
    Dx0 = X1-X0,
    Dy  = Y1-Y0,
    Dx  = abs(Dx0),
    if Dx > Dy ->
	    drawx_(Pixmap,X0,sign(Dx0),X1,Y0,2*Dy-Dx,Dx,Dy,Color);
       true ->
	    drawy_(Pixmap,Y0,Y1,X0,sign(Dx0),2*Dx-Dy,Dx,Dy,Color)
    end.

drawy_(Pixmap,Y,Y1,X,Xi,D,Dx,Dy,Color) ->
    epx:pixmap_put_pixel(Pixmap,X,Y,Color),
    if  Y >= Y1 ->
	    {X,Y};
	D > 0 ->
	    drawy_(Pixmap,Y+1,Y1,X+Xi,Xi,D-2*(Dy-Dx),Dx,Dy,Color);
       true ->
	    drawy_(Pixmap,Y+1,Y1,X,Xi,D+2*Dx,Dx,Dy,Color)
    end.

drawx_(Pixmap,X,Xi,X1,Y,D,Dx,Dy,Color) ->
    epx:pixmap_put_pixel(Pixmap,X,Y,Color),
    if  Xi > 0, X >= X1 -> {X,Y};
	Xi < 0, X =< X1 -> {X,Y};
	D > 0 ->
	    drawx_(Pixmap,X+Xi,Xi,X1,Y+1,D-2*(Dx-Dy),Dx,Dy,Color);
       true ->
	    drawx_(Pixmap,X+Xi,Xi,X1,Y,D+2*Dy,Dx,Dy,Color)
    end.

draw_triangle(Pixmap, {X0,Y0}, {X1,Y1}, {X2,Y2}, Color) ->
    draw_triangle(Pixmap, {X0,Y0,1.0}, {X1,Y1,1.0}, {X2,Y2,1.0}, Color);
draw_triangle(Pixmap, P0, P1, P2, Color) ->
    {Q00,Q01,Q02} = sort_y(P0,P1,P2),
    epx_gc:set_foreground_color(Color),
    if element(2,Q00) =:= element(2,Q01) ->
	    if element(2,Q00) =:= element(2,Q02) ->
		    %% degenerate triangel just a line 
		    ok;
	       true ->  %% Q0.y == Q1.y < Q2.y
		    {Q0,Q1} = sort_x(Q00,Q01),
		    L01 = line_init(Q0, Q02),
		    L02 = line_init(Q1, Q02),
		    fill_steps(Pixmap, L01, L02)
	    end;
       true -> %% Q0.y < (Q1.y and Q2.y)
	    Q0 = Q00,
	    {Q1,Q2} = sort_x(Q01,Q02),
	    %% draw along left edge E1=Q0-Q1 to right edge E2=Q0-Q2 until 
	    %% if E1 termiante before E2 then set E1=Q1-Q2 or if
	    %% E2 terminates first then E2=Q2-Q1
	    if  element(2,Q1) =:= element(2,Q2) ->  %% top triangle
		    L01 = line_init(Q0, Q1),
		    L02 = line_init(Q0, Q2),
		    fill_steps(Pixmap, L01, L02);  %% fixme draw border last
		true ->
		    L01 = line_init(Q0, Q1),
		    L02 = line_init(Q0, Q2),
		    {L11,L12} = fill_steps(Pixmap, L01, L02),
		    fill_steps(Pixmap, L11, L12)
	    end
    end.

fill_steps(Pixmap, E1, E2) ->
    io:format("line: ~w - ~w\n", [line_cur(E1), line_cur(E2)]),
    epx:draw_line(Pixmap, line_cur(E1), line_cur(E2)),
    case line_ystep(Pixmap,E1) of
	{eol,P1} ->
	    {line_init(P1, line_end(E2)), E2};
	E11 ->
	    plot_border(Pixmap,E11),
	    case line_ystep(Pixmap,E2) of
		{eol,P2} ->
		    {E1, line_init(P2,line_end(E1))};
		E22 ->
		    plot_border(Pixmap,E22),
		    fill_steps(Pixmap, E11, E22)
	    end
    end.

plot_border(Pixmap,{_,X,Y,_X1,_Y1,Xi,_D,_D0,_D1}) ->
    epx:pixmap_put_pixel(Pixmap, X+Xi, Y, black).

line_init({X0,Y0,_},{X1,Y1,_}) ->
    line_init_(trunc(X0),trunc(Y0),trunc(X1),trunc(Y1));
line_init({X0,Y0},{X1,Y1}) -> 
    line_init_(trunc(X0),trunc(Y0),trunc(X1),trunc(Y1)).

line_init_(X0,Y0,X1,Y1) ->
    Dx0 = X1-X0,
    Dy  = Y1-Y0,
    Dx  = abs(Dx0),
    Xi  = sign(Dx0),
    if Dx > Dy ->
	    {x,X0,Y0,X1,Y1,Xi,2*Dy-Dx,-2*(Dx-Dy),2*Dy};
       true ->
	    {y,X0,Y0,X1,Y1,Xi,2*Dx-Dy,-2*(Dy-Dx),2*Dx}
    end.

line_ystep(Pixmap,E) ->
    case line_step(E) of
	{eol,XY} -> {eol,XY};
	{ystep, E1} -> line_xstep(Pixmap,E1);  %% move x as far as possible
	{xstep, E1} -> %% only x moved 
	    plot_border(Pixmap,E),
	    line_ystep(Pixmap,E1);
	{xystep,E1} -> E1
    end.

%% move as far as possible on x-axis without moving y-axis
line_xstep(Pixmap,E) ->
    case line_step(E) of
	{eol,_XY} -> E;
	{xstep,E1} -> 
	    plot_border(Pixmap,E),
	    line_xstep(Pixmap,E1);
	{xystep,_} -> E;
	{ystep,_} -> E
    end.

line_cur({_,X,Y,_X1,_Y1,_Xi,_D,_D0,_D1}) -> {X,Y}.
line_end({_,_X,_Y,X1,Y1,_Xi,_D,_D0,_D1}) -> {X1,Y1}.

line_step({x,X,Y,X1,Y1,Xi,D,D0,D1}) ->
    if Xi > 0, X >= X1 -> {eol,{X1,Y1}};
       Xi < 0, X =< X1 -> {eol,{X1,Y1}};
       D > 0 -> {xystep,{x,X+Xi,Y+1,X1,Y1,Xi,D+D0,D0,D1}};
       true  -> {xstep, {x,X+Xi,Y,   X1,Y1,Xi,D+D1,D0,D1}}
    end;
line_step({y,X,Y,X1,Y1,Xi,D,D0,D1}) ->
    if Y >= Y1 -> {eol,{X1,Y1}};
       D > 0   -> {xystep,{y,X+Xi,Y+1,X1,Y1,Xi,D+D0,D0,D1}};
       true    -> {ystep, {y,X,   Y+1,X1,Y1,Xi,D+D1,D0,D1}}
    end.

sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.

sort_x(P0={X0,_,_},P1={X1,_,_}) when X0 < X1 -> {P0,P1};
sort_x(P0,P1) -> {P1,P0}.

sort_y(P0={_,Y0,_},P1={_,Y1,_}) when Y0 < Y1 -> {P0,P1};
sort_y(P0,P1) -> {P1,P0}.

sort_y(P0={_,Y0,_},P1={_,Y1,_},P2={_,Y2,_}) ->
    if Y0 =< Y1 -> %% Y0 Y1 x | Y0 x Y1 | x Y0 Y1
	    if Y1 =< Y2 ->  %% Y0 Y1 Y2
		    {P0,P1,P2};
	       Y2 =< Y0 ->
		    {P2,P0,P1};
	       true ->
		    {P0,P2,P1}
	    end;
       %% Y1 Y0 x | Y1 x Y0 | x Y1 Y0
       Y0 =< Y2 -> {P1,P0,P2};
       Y1 =< Y2 -> {P1,P2,P0};
       true -> {P2,P1,P0}
    end.


test_sort_2() ->
    A = random_point_3d(),
    B = random_point_3d(),
    {A1,B1} = sort_y(A,B),
    element(2,A1) =< element(2,B1).

test_sort_3() ->
    A = random_point_3d(),
    B = random_point_3d(),
    C = random_point_3d(),
    {A1,B1,C1} = sort_y(A,B,C),
    (element(2,A1) =< element(2,B1)) andalso
	(element(2,B1) =< element(2,C1)).

random_point_3d() ->
    {rand:uniform(65536),rand:uniform(65536),rand:uniform(65536)}.




    
    
