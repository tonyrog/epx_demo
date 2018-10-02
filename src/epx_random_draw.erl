%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%     Draw various stuff
%%% @end
%%% Created : 22 Feb 2015 by Tony Rogvall <tony@rogvall.se>

-module(epx_random_draw).
-compile(export_all).



draw_triangle_1() ->
    draw_triangle_1(640,480,{320,10},{40,400},{600,400}).

draw_triangle_1(W,H,P1,P2,P3) ->
    start(1,
	  fun(Pixmap) ->
		  Color = {127,255,0,0},
		  draw_poly3_1(Pixmap, P1, P2, P3, Color)
	  end, W, H).

draw_triangle_2() ->
    draw_triangle_2(640,480,{100,10},{10,100},{200,200}).

draw_triangle_2(W,H,P1,P2,P3) ->
    start(1,
	  fun(Pixmap) ->
		  Color = {127,255,0,0},
		  draw_poly3_2(Pixmap, P1, P2, P3, Color)
	  end, W, H).
    

draw_strip_solid(M) ->
    draw_strip(gen_circle_strip(M), 640, 480, solid).

draw_strip_blend(M) ->
    draw_strip(gen_circle_strip(M), 640, 480, blend).

draw_strip(Strip, W, H, Style) ->
    epx_gc:set_fill_style(Style),
    start(1,
	  fun(Pix) ->
		  epx_gc:set_fill_color({127,255,0,0}),
		  draw_triangle_strip(Pix, Strip)
	  end, W, H).

draw_triangle_strip(Pix, [V0,V1 | Vs=[V2,V3|_]]) ->
    epx:draw_triangle(Pix, V0, V1, V2),
    epx:draw_triangle(Pix, V2, V1, V3),
    draw_triangle_strip(Pix, Vs);
draw_triangle_strip(Pix, [V0,V1,V2]) ->
    epx:draw_triangle(Pix, V0, V1, V2);
draw_triangle_strip(_Pix, [_V0,_V1]) -> ok;
draw_triangle_strip(_Pix, [_V0]) -> ok;
draw_triangle_strip(_Pix, []) -> ok.

gen_circle_strip(M) ->
    gen_circle_strip(M, 320, 240, 200, 150).

gen_circle_strip(M, Xc, Yc, R1, R2) ->
    V = (2*math:pi())/M,
    L1 = [begin A  = (I rem M)*V,
		 [ {trunc(Xc + R1*math:cos(A)),
		    trunc(Yc + R1*math:sin(A)) },
		   {trunc(Xc + R2*math:cos(A)),
		    trunc(Yc + R2*math:sin(A)) } ]
	   end || I <- lists:seq(0, M)],
    lists:append(L1).


draw_mesh_solid(N) ->
    draw_mesh(N, 640, 480, solid).

draw_mesh_blend(N) ->
    draw_mesh(N, 640, 480, blend).

draw_mesh(N, W, H, Style) ->
    epx_gc:set_fill_style(Style),
    start(N,
	  fun(Pix) ->
		  R = random_interval(10, 100),
		  M = random_interval(3, 30),
		  V = (2*math:pi())/M,
		  P1 = {Xc,Yc} = random_point(W, H),
		  epx_gc:set_fill_color(random_color()),
		  lists:foreach(
		    fun(I) ->
			    %% case I rem 2 of
			    %% 0 -> epx_gc:set_fill_color(red);
			    %% 1 -> epx_gc:set_fill_color(black)
			    %% end,
			    A1 = I*V,
			    A2 = ((I+1) rem M)*V,
			    X1 = trunc(Xc + R*math:cos(A1)),
			    Y1 = trunc(Yc + R*math:sin(A1)),
			    X2 = trunc(Xc + R*math:cos(A2)),
			    Y2 = trunc(Yc + R*math:sin(A2)),
			    epx:draw_triangle(Pix, P1, {X1,Y1}, {X2,Y2})
		    end, lists:seq(0, M-1))
	  end, W, H).
    

draw_triangles_solid(N) ->
    draw_triangles(N, 640, 480, solid).

draw_triangles_blend(N) ->
    draw_triangles(N, 640, 480, blend).

draw_triangles(N, W, H, Style) ->
    epx_gc:set_fill_style(Style),
    start(N,
	  fun(Pix) ->
		  epx_gc:set_fill_color(random_color()),
		  P1 = random_point(W, H),
		  P2 = random_point(W, H),
		  P3 = random_point(W, H),
		  epx:draw_triangle(Pix, P1, P2, P3)
	  end, W, H).

draw_rectangles_solid(N) ->
    draw_rectangles(N, 640, 480, solid).

draw_rectangles_blend(N) ->
    draw_rectangles(N, 640, 480, blend).

draw_rectangles(N, W, H, Style) ->
    epx_gc:set_fill_style(Style),
    start(N,
	  fun(Pix) ->
		  epx_gc:set_fill_color(random_color()),
		  {X1,Y1} = random_point(W, H),
		  {X2,Y2} = random_point(W, H),
		  X = min(X1,X2),
		  Y = min(X2,Y2),
		  Width = (max(X1,X2)-X)+1,
		  Height = (max(Y1,Y2)-Y)+1,
		  epx:draw_rectangle(Pix, X, Y, Width, Height)
	  end, W, H).

start(N, Func, W, H) ->
    epx:start(),
    Win = epx:window_create(50,50,W,H,[button_press,key_press]),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(W,H,argb),
    epx:pixmap_fill(Pix, white),
    epx:pixmap_attach(Pix),
    loop(N, Func, Win, Pix, W, H),
    receive
	{epx_event,Win, close} ->
	    epx:window_detach(Win),
	    epx:pixmap_detach(Pix),
	    ok
    end.

loop(0, _Func, _Win, _Pix, _W, _H) ->
    ok;
loop(I, Func, Win, Pix, W, H) ->
    Func(Pix),
    epx:pixmap_draw(Pix,Win,0,0,0,0,W, H),
    epx:sync(Pix,Win),
    loop(I-1, Func, Win, Pix, W, H).

random_interval(L, H) when L =< H ->
    L + rand:uniform((H - L) + 1) - 1.

random_point(W,H) ->
    {rand:uniform(W)-1, rand:uniform(H)-1}.

random_color() ->
    {rand:uniform(200)+50,
     rand:uniform(200)+50,     
     rand:uniform(200)+50,
     rand:uniform(200)+50}.

draw_poly3_1(Pixmap,P0={X0,Y0},P1={X1,Y1},P2={X2,Y2},Color) ->
    T0 = erlang:monotonic_time(),
    Xl = min(X0,X1,X2),
    Xr = max(X0,X1,X2),
    Yu = min(Y0,Y1,Y2),
    Yd = max(Y0,Y1,Y2),
    V1 = {X1-X0, Y1-Y0},
    V2 = {X2-X0, Y2-Y0},
    C12 = cross(V1,V2),
    scan_y(Yu,Yd,Xl,Xr,P0,V1,V2,C12,Pixmap,Color),
    T1 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T1 - T0, native, microsecond),
    io:format("time = ~wus\n", [Time]).

scan_y(Y,Yd,_Xl,_Xr,_P0,_V1,_V2,_C12,_Pixmap,_Color) when Y > Yd ->
    ok;
scan_y(Y,Yd,Xl,Xr,P0={X0,Y0},V1={V1x,V1y},V2={V2x,V2y},C12,Pixmap,Color) ->
    Dx = Xl - X0,
    Dy = Y  - Y0,
    S0 = (Dx*V2y - Dy*V2x),
    Si = V2y,
    T0 = (V1x*Dy - V1y*Dx),
    Ti = -V1y,
    scan_x(Xl,Xr,Y,C12,S0,Si,T0,Ti,Pixmap,Color),
    scan_y(Y+1,Yd,Xl,Xr,P0,V1,V2,C12,Pixmap,Color).

scan_x(X,Xr,_Y,_C12,_S,_Si,_T,_Ti,_Pixmap,_Color) when X > Xr ->
    ok;
scan_x(X,Xr,Y,C12,S0,Si,T0,Ti,Pixmap,Color) ->
    if C12>0, S0>=0, T0>=0, T0+S0 =< C12;
       C12<0, S0<0, T0<0, T0+S0 >= C12 ->
	    %% S=S0/C12, T=T0/C12,
	    %% C0 = {1,0,0}, C1 = {0,1,0}, C2 = {0,0,1},
	    %% Color1 = add(add(mult(1-(S+T),C0),mult(S,C1)),mult(T,C2)),
	    %% epx:pixmap_put_pixel(Pixmap,X,Y,rgb8(Color1));
	    epx:pixmap_put_pixel(Pixmap,X,Y,Color);
       true ->
	    ok
    end,
    scan_x(X+1,Xr,Y,C12,S0+Si,Si,T0+Ti,Ti,Pixmap,Color).


mult({X0,Y0},{X1,Y1}) ->  {X0*X1,Y0*Y1};
mult(A,{X1,Y1}) ->  {A*X1,A*Y1};
mult({X0,Y0},B) ->  {X0*B,Y0*B};
mult({X0,Y0,Z0},{X1,Y1,Z1}) ->  {X0*X1,Y0*Y1,Z0*Z1};
mult(A,{X1,Y1,Z1}) ->  {A*X1,A*Y1,A*Z1};
mult({X0,Y0,Z0},B) ->  {X0*B,Y0*B,Z0*B}.

add({X0,Y0},{X1,Y1}) ->  {X0+X1,Y0+Y1};
add(A,{X1,Y1}) ->  {A+X1,A+Y1};
add({X0,Y0},B) ->  {X0+B,Y0+B};
add({X0,Y0,Z0},{X1,Y1,Z1}) ->  {X0+X1,Y0+Y1,Z0+Z1};
add(A,{X1,Y1,Z1}) ->  {A+X1,A+Y1,A+Z1};
add({X0,Y0,Z0},B) ->  {X0+B,Y0+B,Z0+B}.

sub({X0,Y0},{X1,Y1}) ->  {X0-X1,Y0-Y1};
sub(A,{X1,Y1}) ->  {A-X1,A-Y1};
sub({X0,Y0},B) ->  {X0-B,Y0-B};
sub({X0,Y0,Z0},{X1,Y1,Z1}) ->  {X0-X1,Y0-Y1,Z0-Z1};
sub(A,{X1,Y1,Z1}) ->  {A-X1,A-Y1,A-Z1};
sub({X0,Y0,Z0},B) ->  {X0-B,Y0-B,Z0-B}.
     
rgb8({R,G,B}) -> {trunc(R*255),trunc(G*255),trunc(B*255)}.
    

cross({X0,Y0},{X1,Y1}) -> X0*Y1 - Y0*X1.

max(A,B,C) -> max(C,max(A,B)).
min(A,B,C) -> min(C,min(A,B)).

sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.


draw_poly3_2(Pixmap,P0,P1,P2,Color) ->
    draw_line(Pixmap, P0, P1, green),
    draw_line(Pixmap, P1, P2, red),
    draw_line(Pixmap, P2, P0, blue),
    ok.

draw_line(Pixmap, P0={X0,Y0}, P1={X1,Y1}, Color) ->
    Dx = X1-X0,
    Dy = Y1-Y0,
    if abs(Dy) < abs(Dx) ->
	    if X0 < X1 ->
		    line_low(Pixmap,P0,P1,Dx,Dy,Color);
	       true ->
		    line_low(Pixmap,P1,P0,-Dx,Dy,Color)
	    end;
       true ->
	    if Y0 < Y1 ->
		    line_high(Pixmap,P0,P1,Dx,Dy,Color);
	       true ->	       
		    line_high(Pixmap,P1,P0,Dx,-Dy,Color)
	    end
    end.


line_low(Pixmap,{X0,Y0},{X1,Y1},Dx,Dy,Color) ->
    true = Dx > 0,
    Yi = sign(Dy),
    Dy1 = 2*abs(Dy),
    D = Dy1 - Dx,
    line_low_(Pixmap,X0,X1,Y0,Yi,D,2*Dx,Dy1,Color).

line_low_(Pixmap,X,X1,Y,Yi,D,Dx,Dy,Color) when X > X1 ->
    ok;
line_low_(Pixmap,X,X1,Y,Yi,D,Dx,Dy,Color) ->
    epx:pixmap_put_pixel(Pixmap,X,Y,Color),
    if D > 0 ->
	    line_low_(Pixmap,X+1,X1,Y+Yi,Yi,D-Dx+Dy,Dx,Dy,Color);
       true ->
	    line_low_(Pixmap,X+1,X1,Y,Yi,D+Dy,Dx,Dy,Color)
    end.

line_high(Pixmap,{X0,Y0},{X1,Y1},Dx,Dy,Color) ->
    true = Dy > 0,
    Xi = sign(Dx),
    Dx1 = 2*abs(Dx),
    D = Dx1 - Dy,
    line_high_(Pixmap,Y0,Y1,X0,Xi,D,Dx1,2*Dy,Color).

line_high_(Pixmap,Y,Y1,X,Xi,D,Dx,Dy,Color) when Y > Y1 ->
    ok;
line_high_(Pixmap,Y,Y1,X,Xi,D,Dx,Dy,Color) ->
    epx:pixmap_put_pixel(Pixmap,X,Y,Color),
    if D > 0 ->
	    line_high_(Pixmap,Y+1,Y1,X+Xi,Xi,D-Dy+Dx,Dx,Dy,Color);
       true ->
	    line_high_(Pixmap,Y+1,Y1,X,Xi,D+Dx,Dx,Dy,Color)
    end.

