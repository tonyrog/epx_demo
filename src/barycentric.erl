%%
%% Draw triangle using barycentric algorithm
%%
-module(barycentric).

-export([draw_triangle/5]).
-export([draw_triangle_line/6]).

%% v1 = p2 - p1;
%% v2 = p3 - p1;
%% for (x = minX, x <= maxX; x++) {
%%   for (y = minY, y <= maxY; y++) {
%%     q = (x,y) - p1;
%%     s = cross(q,v1)/cross(v1,v2)
%%     t = cross(v2,q)/cross(v1,v2)
%%     if ((s >= 0) && (t >= 0) && (s+t <= 1))
%%        plot(x,y)
%%   }
%% }
%%
%% S = S0 + Si*I
%% T = T0 + Tj*J
%% S>=0, T>=0, T+S =< K
%% S = S0 + Si*I = 0  => I = -S0/Si
%% T = T0 + Tj*J = 0  => J = -T0/Sj
%% L = max(I,J)
%% T+S = S0 + Si*I + T0 +Tj*J =< K
%% 
%%
%% S=S0/K, T=T0/K,
%% C0 = {1,0,0}, C1 = {0,1,0}, C2 = {0,0,1},
%% Color1 = add(add(mult(1-(S+T),C0),mult(S,C1)),mult(T,C2)),
%% epx:pixmap_put_pixel(Pixmap,X,Y,rgb8(Color1));
-record(triangle,
	{
	 p0,
	 p1,
	 p2,
	 color,
	 k
	}).

-define(SCALE, 1).

-define(PUT_PIXEL(Pixmap,X,Y,Color),
	if ?SCALE =:= 1 ->
		epx:pixmap_put_pixel((Pixmap),(X),(Y),(Color),[blend]);
	   true ->
		epx:pixmap_fill_area((Pixmap),?SCALE*(X),?SCALE*(Y),
				     ?SCALE,?SCALE,(Color),[blend])
	end).

%%
%% We should be able to run several triangles at one!!!
%% possibly using simd vector operations

draw_triangle(Pixmap,{X0,Y0},{X1,Y1},{X2,Y2},Color) ->
    draw_triangle(Pixmap,{X0,Y0,1.0},{X1,Y1,1.0},{X2,Y2,1.0},Color);
draw_triangle(Pixmap,P0,P1,P2,Color) ->
    {X0,Y0,_} = point_3d(?SCALE, P0),
    {X1,Y1,_} = point_3d(?SCALE, P1),
    {X2,Y2,_} = point_3d(?SCALE, P2),
    Xl = min(X0,X1,X2),
    Xr = max(X0,X1,X2),
    Yu = min(Y0,Y1,Y2),
    Yd = max(Y0,Y1,Y2),
    V1 = {V1x=(X1-X0), V1y=(Y1-Y0)},
    V2 = {V2x=(X2-X0), V2y=(Y2-Y0)},
    K = cross(V1,V2),
    Qx = Xl - X0,
    Qy = Yu - Y0,
    S0 = (Qx*V2y - Qy*V2x),
    T0 = (V1x*Qy - V1y*Qx),
    Tri = #triangle{ p0=P0, p1=P1, p2=P2, color=Color, k=K },
    scan_y(Pixmap,trunc(Yu),trunc(Yd),S0,T0,trunc(Xl),trunc(Xr),V1,V2,K,Tri).

%% do one line only in the barycentric algorithm
draw_triangle_line(Pixmap,P,P0,P1,P2,Color) ->
    {X,Y,_}   = point_3d(?SCALE, P),
    {X0,Y0,_} = point_3d(?SCALE, P0),
    {X1,Y1,_} = point_3d(?SCALE, P1),
    {X2,Y2,_} = point_3d(?SCALE, P2),
    Yu = trunc(min(Y0,Y1,Y2)),
    Yd = trunc(max(Y0,Y1,Y2)),
    Xl = trunc(min(X0,X1,X2)),
    Xr = trunc(max(X0,X1,X2)),
    if Y < Yu; Y > Yd -> ok;
       X < Xl; X > Xr -> ok;
       true ->
	    V1 = {V1x=(X1-X0), V1y=(Y1-Y0)},
	    V2 = {V2x=(X2-X0), V2y=(Y2-Y0)},
	    K = cross(V1,V2),
	    Qx = X - X0,
	    Qy = Y - Y0,
	    S0 = (Qx*V2y - Qy*V2x),
	    T0 = (V1x*Qy - V1y*Qx),
	    Tri = #triangle{ p0=P0, p1=P1, p2=P2, color=Color, k=K },
	    if K > 0 ->
		    scan_x_kgto(Pixmap,X,Xr,Y,K,S0,V2y,T0,-V1y,Tri);
	       true ->
		    scan_x_klto(Pixmap,X,Xr,Y,K,S0,V2y,T0,-V1y,Tri)
	    end
    end.

scan_y(Pixmap,Y,Yd,S0,T0,Xl,Xr,V1={V1x,V1y},V2={V2x,V2y},K,Tri) ->
    if Y > Yd ->
	    ok;
       true ->
	    if K > 0 ->
		    scan_x_kgto(Pixmap,Xl,Xr,Y,K,S0,V2y,T0,-V1y,Tri);
	       true ->
		    scan_x_klto(Pixmap,Xl,Xr,Y,K,S0,V2y,T0,-V1y,Tri)
	    end,
	    scan_y(Pixmap,Y+1,Yd,S0-V2x,T0+V1x,Xl,Xr,V1,V2,K,Tri)
    end.

%% outside
scan_x_kgto(Pixmap,X,Xr,Y,K,S,Si,T,Tj,Tri) ->
    if X > Xr -> ok;
       S<0; T<0; T+S>K ->
	    scan_x_kgto(Pixmap,X+1,Xr,Y,K,S+Si,Si,T+Tj,Tj,Tri);
       true -> %% inside
	    %% io:format("Y:~w,K>0:X:~w - ", [Y,X]),
	    scan_x_kgti(Pixmap,X,Xr,Y,K,S,Si,T,Tj,Tri)
    end.

scan_x_kgti(Pixmap,X,Xr,Y,K,S,Si,T,Tj,Tri) ->
    if X > Xr -> 
	    %% io:format("x=~w (out)\n", [X]),
	    ok;
       S>=0, T>=0, T+S=<K ->
	    plot(Pixmap,X,Y,S,T,K,Tri),
	    scan_x_kgti(Pixmap,X+1,Xr,Y,K,S+Si,Si,T+Tj,Tj,Tri);
       true ->
	    %% fixme: plot antialiased last point
	    %% io:format("x=~w\n", [X]),
	    ok
    end.

scan_x_klto(Pixmap,X,Xr,Y,K,S,Si,T,Tj,Tri) ->
    if  X > Xr -> ok;
	S>=0; T>=0; T+S=<K ->
	    scan_x_klto(Pixmap,X+1,Xr,Y,K,S+Si,Si,T+Tj,Tj,Tri);
	true ->
	    %% io:format("Y:~w,K<0:X:~w - ", [Y,X]),
	    scan_x_klti(Pixmap,X,Xr,Y,K,S,Si,T,Tj,Tri)
    end.

scan_x_klti(Pixmap,X,Xr,Y,K,S,Si,T,Tj,Tri) ->
    if  X > Xr -> 
	    %% io:format("x=~w (out)\n", [X]),
	    ok;
	S<0, T<0, T+S>K ->
	    plot(Pixmap,X,Y,S,T,K,Tri),
	    scan_x_klti(Pixmap,X+1,Xr,Y,K,S+Si,Si,T+Tj,Tj,Tri);
	true ->
	    %% io:format("x=~w\n", [X]),
	    ok	    
    end.

plot_0(Pixmap,X,Y,_S,_T,_K,Tri) ->
    Color = Tri#triangle.color,
    ?PUT_PIXEL(Pixmap,X,Y,Color).

plot(Pixmap,X,Y,S0,T0,K,_Tri) ->
    C0 = {1,0,0}, C1 = {0,1,0}, C2 = {0,0,1},
    S = S0/K, T = T0/K,
    Color1 = rgb8(add(add(mult(1-(S+T),C0),mult(S,C1)),mult(T,C2))),
    ?PUT_PIXEL(Pixmap,X,Y,Color1).

plot_2(Pixmap,X,Y,S0,T0,K,Tri) ->
    {_,_,P0z} = Tri#triangle.p0,
    {_,_,P1z} = Tri#triangle.p1,
    {_,_,P2z} = Tri#triangle.p2,
    S = S0/K, T = T0/K,
    Z = 1/((1/P0z)*(1-(S+T)) + (1/P1z)*S + (1/P2z)*T),
    MinZ = min(P0z,P1z,P2z),
    MaxZ = max(P0z,P1z,P2z),
    G = (Z-MinZ) / (MaxZ-MinZ),
    Color1 = rgb8({G,G,G}),
    ?PUT_PIXEL(Pixmap,X,Y,Color1).

cross({X0,Y0},{X1,Y1}) -> X0*Y1 - Y0*X1.

max(A,B,C) -> max(C,max(A,B)).
min(A,B,C) -> min(C,min(A,B)).

point_3d(Scale,{X,Y}) -> mult(Scale,{X,Y,1.0});
point_3d(Scale,P={_X,_Y,_Z}) -> mult(Scale,P).

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

sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.
