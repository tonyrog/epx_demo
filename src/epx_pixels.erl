%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%   Example where a lot of processes update the screen
%%%   Each pixel is represented by a process!!!
%%% @end
%%% Created :  6 Nov 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_pixels).

-compile(export_all).
-import(lists, [foreach/2]).
-include_lib("epx/include/epx_image.hrl").

-define(BGCOLOR, {255,255,255}).

run() ->
    run(100,100).
run(W,H) ->
    epx:start(),
    spawn(fun() -> init(W,H) end).

load_image(File) ->
    case epx_image:load(File) of
	{ok,Image} ->
	    case Image#epx_image.pixmaps of
		[Pixmap] ->
		    Width = ets:lookup_element(pixmap, width, 2),
		    Height = ets:lookup_element(pixmap, height, 2),
		    SourceNext = ets:lookup_element(pixmap, source_next, 2),
		    epx:pixmap_scale(Pixmap, SourceNext, Width, Height),
		    Source = ets:lookup_element(pixmap, source, 2),
		    ets:insert(pixmap, {source, SourceNext}),
		    ets:insert(pixmap, {source_next, Source}),
		    ok
	    end;
	Error ->
	    Error
    end.
%%
%%  
%%
init(Width, Height) ->
    Window = epx:window_create(50, 50, Width, Height),
    Bg1  = epx:pixmap_create(Width, Height, argb),
    Bg2  = epx:pixmap_create(Width, Height, argb),
    Fg1  = epx:pixmap_create(Width, Height, argb),
    Fg2  = epx:pixmap_create(Width, Height, argb), 
    epx:window_attach(Window),
    epx:pixmap_fill(Bg1, ?BGCOLOR),
    epx:pixmap_fill(Bg2, ?BGCOLOR),
    epx:pixmap_fill(Fg1, ?BGCOLOR),
    epx:pixmap_fill(Fg2, ?BGCOLOR),
    epx:pixmap_attach(Fg1),
    epx:pixmap_attach(Fg2),
    ets:new(pixmap,    [named_table, public]),
    ets:new(task_info, [named_table, public]),
    ets:insert(pixmap, {source,Bg1}),
    ets:insert(pixmap, {source_next,Bg2}),
    ets:insert(pixmap, {destination,Fg1}),
    ets:insert(pixmap, {width,Width}),
    ets:insert(pixmap, {height,Width}),
    tasks_start(Width, Height),
    loop(Fg1,Fg2, Window, Width, Height).


loop(Fg1, Fg2, Window, Width, Height) ->
    epx:pixmap_fill(Fg2, ?BGCOLOR),
    ets:insert(pixmap, {destination,Fg2}),
    epx:pixmap_draw(Fg1, Window, 0, 0, 0, 0, Width, Height),
    timer:sleep(40),
    loop(Fg2, Fg1, Window, Width, Height).

tasks_start(Width, Height) ->
    [spawn(fun() -> task_init(X,Y) end) ||
	X <- lists:seq(0, Width-1),
	Y <- lists:seq(0, Height-1)].


-define(MAX_RADIUS, 16).
-define(INIT_RADIUS, 8).
-define(EPS, 0.00001).

task_init(X,Y) ->
    update_info(X,Y),
    task_run(3,X,Y,40).

task_run(I,X,Y,TFrame) ->
    Source      = ets:lookup_element(pixmap, source, 2),
    Destination = ets:lookup_element(pixmap, destination, 2),
    C = epx:pixmap_get_pixel(Source, X, Y),
    Sin = math:sin(math:pi()*I*X*Y),
    Cos = math:cos(math:pi()*I*X*Y),
    F = 1/math:log(I),  %% Decay
    %% MR = {0.0, 0.299*F,0.587*F, 0114*F },
    %% MF = {MR,MR,MR,{1.0, 0.299*F,0.587*F, 0114*F  }},
    %% MC = {{1.0,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}},
    %% R = rand:uniform(),
    %% RC = {{R,0,1-R,0},{0,R,0,1-R},{0,1-R,R,0},{1-R,0,0,R}},
    %% C1 = color_mult(RC, C),
    MR = {0.0, 0.299*F,0.587*F, 0114*F },
    MC = {Sin,Cos,-Sin,Cos},
    C1 = color_mult({MR,MC,MR,MC}, C),
    epx:pixmap_put_pixel(Destination,X,Y,color_trunc(C1)),
    timer:sleep(TFrame),
    task_run(I+1,X,Y,TFrame).

update_info(X,Y) ->
    ets:insert(task_info, {self(),{X,Y}}),
    ets:insert(task_info, {{X,Y},self()}).

clamp(X) when X > 255 -> 255;
clamp(X) -> X.

color_random() ->
    {rand:uniform(256)-1,rand:uniform(256)-1,rand:uniform(256)-1}.

%% T=0 => C0,  T=1 => C1
color_interp(_, C0,C0)  -> C0;
color_interp(0, C0,_C1) -> C0;
color_interp(1, _C0,C1) -> C1;
color_interp(T, {R0,G0,B0},{R1,G1,B1}) when T >= 0, T =< 1 ->
    R = R0 + T*(R1-R0),
    G = G0 + T*(G1-G0),
    B = B0 + T*(B1-B0),
    {trunc(R), trunc(G), trunc(B)}.

color_trunc({A,R,G,B}) ->    
    {trunc(A),trunc(R),trunc(G),trunc(B)};
color_trunc({R,G,B}) ->    
    {trunc(R),trunc(G),trunc(B)}.

color_add({R0,G0,B0},{R1,G1,B1}) ->
    {clamp(R0+R1),clamp(G0+G1),clamp(B0+B1)}.

color_avg({R0,G0,B0},{R1,G1,B1}) ->
    { (R0+R1) div 2,(G0+G1) div 2, (B0+B1) div 2}.


within(A, A0, A1) ->
    (A0 =< A) andalso (A =< A1).

clamp_switch(A, A0, A1, V) ->
    if A < A0 -> {A0,-V};
       A > A1 -> {A1,-V};
       true -> {A,V}
    end.
	    
delete_task() ->
    delete_info(),
    exit(normal).

delete_info() ->
    ets:delete(task_info, self()).


%% z order 2D coordinate 
vector_z_order({X,Y}) ->
    vector_z_order(X,Y).

vector_z_order(X, Y) ->
    vector_z_order(X, Y, 0, 0).

vector_z_order(_X, _Y, 10, Z) -> 
    Z;
vector_z_order(X, Y, I, Z) ->
    Zi = ((X band 1) bsl 1) bor (Y band 1),
    vector_z_order(X bsr 1, Y bsr 1, I+1, Z + (Zi bsl (2*I))).
    
%% Vector utils
%% generate a {-1..1, -1..1} random vector
vector_random() ->
    vector_random(-1,1,-1,1).

vector_random(X0,X1,Y0,Y1) ->
    {(X1-X0)*rand:uniform()+X0, (Y1-Y0)*rand:uniform()+Y0}.

vector_norm({X,Y}) ->
    L = math:sqrt(X*X + Y*Y),
    if L < ?EPS -> {1.0, 0.0};
       true -> {X/L, Y/L}
    end.

vector_trunc({X,Y}) ->
    {trunc(X),trunc(Y)}.

vector_add({X0,Y0},{X1,Y1}) ->
    {X0+X1, Y0+Y1}.

vector_sub({X0,Y0},{X1,Y1}) ->
    {X0-X1, Y0-Y1}.

vector_scale(S, {X,Y}) ->
    {S*X, S*Y}.
    
vector_angle({X,Y}) ->
    math:atan2(Y, X).

vector_dir(Angle) ->
    {math:cos(Angle),math:sin(Angle)}.

dot({A,B,C,D}, {E,F,G,H}) ->
    A*E + B*F + C*G + D*H.

color_mult({M1, M2, M3, M4}, Color) ->
    { dot(Color,M1), dot(Color,M2), dot(Color,M3), dot(Color,M4) }.
