%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Basic bresenham line drawing algorithm
%%% @end
%%% Created :  4 Oct 2018 by Tony Rogvall <tony@rogvall.se>

-module(bresenham).

-export([draw_line/4]).
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

draw_triangle(Pixmap, P0, P1, P2, Color) ->
    {Q0,Q01,Q02} = sort_y(P0,P1,P2),
    {Q1,Q2} = sort_x(Q01,Q02),
    %% left edge is Q0-Q1 right edge is Q0-Q2
    ok.


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




    
    
