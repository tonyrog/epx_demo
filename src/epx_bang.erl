%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%   Example where a lot of processes update the screen
%%% @end
%%% Created :  6 Nov 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_bang).

-compile(export_all).
-import(lists, [foreach/2]).

-define(BGCOLOR, blue).

run() -> run(100).

run(N) ->
    run(640,480,N).
run(W,H,N) ->
    epx:start(),
    spawn(fun() -> init(W,H,N) end).

%%
%% Position map
%%   X = x9x8x7x6x5x4x3x2x1x0
%%   Y = y9y8y7y6y5y4y3y2y1y0
%%
%% generates leveled poistion map (5 level)
%%   Z0 = x9y9-x8y8-x7y7-x6y6-x5y5-x4y4-x3y3-x2y2-x1y1-x0y0
%%   Z1 = x9y9-x8y8-x7y7-x6y6-x5y5-x4y4-x3y3-x2y2
%%   Z2 = x9y9-x8y8-x7y7-x6y6-x5y5-x4y4
%%   Z3 = x9y9-x8y8-x7y7-x6y6
%%   Z4 = x9y9-x8y8
%%
%% Task is stored on each level
%%  
%%
init(Width, Height, N) ->
    Window     = epx:window_create(50, 50, Width, Height),
    Back1      = epx:pixmap_create(Width, Height, argb),
    Back2      = epx:pixmap_create(Width, Height, argb),
    epx:window_attach(Window),
    epx:pixmap_fill(Back1, ?BGCOLOR),
    epx:pixmap_fill(Back2, ?BGCOLOR),
    epx:pixmap_attach(Back1),
    epx:pixmap_attach(Back2),
    ets:new(pixmap,    [named_table, protected]),
    ets:new(task_info, [named_table, public]),
    ets:new(lod0,      [named_table, public, bag]),
    ets:new(lod1,      [named_table, public, bag]),
    ets:new(lod2,      [named_table, public, bag]),
    ets:new(lod3,      [named_table, public, bag]),
    ets:new(lod4,      [named_table, public, bag]),
    ets:insert(pixmap, {current,Back1}),
    tasks_start(N),
    loop(Back1, Back2, Window, Width, Height).


loop(Current, Next, Window, Width, Height) ->
    epx:pixmap_fill(Next, ?BGCOLOR),
    ets:insert(pixmap, {current,Next}),
    epx:pixmap_draw(Current, Window, 0, 0, 0, 0, Width, Height),
    timer:sleep(16),
    loop(Next, Current, Window, Width, Height).

tasks_start(0) ->		
    ok;
tasks_start(I) ->
    spawn(fun() -> task_init(I) end),
    tasks_start(I-1).

-define(MAX_RADIUS, 16).
-define(INIT_RADIUS, 8).
-define(EPS, 0.00001).

task_init(_I) ->
    Color = color_random(),
    V0 = vector_random(-1,1,  -1, 1),
    V  = vector_norm(V0),
    Back = ets:lookup_element(pixmap, current, 2),
    W = epx:pixmap_info(Back, width),
    H = epx:pixmap_info(Back, height),
    R = ?INIT_RADIUS,
    P  = vector_random(R, W-R, R, H-R),
    Seed = rand:export_seed(),
    task_init(Seed, P, V, R, Color).

task_init(Seed, P, V, R, Color) ->
    rand:seed(Seed),
    Back = ets:lookup_element(pixmap, current, 2),
    W = epx:pixmap_info(Back, width),
    H = epx:pixmap_info(Back, height),
    Pk = vector_trunc(P), 
    Zk = vector_z_order(Pk),
    set_location(Zk),
    task_run(1,Zk,P,V,W,H,R,Color).

-define(DRAW_N, 4096).

task_run(I,Zi,P0,V0,W,H,R,Color) ->
    P = vector_add(P0, V0),
    Pk = vector_trunc(P),
    {Pk1,V1} = task_wall_collision(Pk,V0,W,H,R),
    task_draw(Pk1,R,Color),
    Zj = vector_z_order(Pk1),
    update_location(Zi,Zj),
    update_info(Pk1,V1,R,Color),
    {V2,R1,Color1} = task_wait(Zj,P,V1,R,1,Color),
    task_broadcast(Zj),
    task_run(I+1,Zj,P,V2,W,H,R1,Color1).

update_info(Pk,V,R,Color) ->
    ets:insert(task_info, {self(),Pk,V,R,Color}).

%% random broadcast a random color stamp
task_broadcast(Zj) ->
    Draw = rand:uniform(10000),
    if Draw =< 3 ->
	    Lod   = rand:uniform(?DRAW_N),
	    Color = color_random(),
	    if Lod >= ?DRAW_N - (?DRAW_N div 16) ->
		    broadcast(4, Zj, {color,Color});
	       Lod >= ?DRAW_N - (?DRAW_N div 8) ->
		    broadcast(3, Zj, {color,Color});
	       Lod >= ?DRAW_N - (?DRAW_N div 4) ->
		    broadcast(2, Zj, {color,Color});
	       Lod >= ?DRAW_N - (?DRAW_N div 2) ->
		    broadcast(1, Zj, {color,Color});
	       true ->
		    broadcast(0, Zj, {color,Color})
	    end;
       true ->
	    ok
    end.

%% draw the "sprite" on the background pixmap
task_draw({Xk,Yk},R,Color) ->
    Back = ets:lookup_element(pixmap, current, 2),
    epx_gc:set_fill_color(Color),
    epx_gc:set_fill_style(solid),
    epx:draw_ellipse(Back, Xk-R, Yk-R, R+R, R+R).

%% wait and receive orders
task_wait(Z,P,V,R,Time,Color0) ->
    receive
	kill ->
	    delete_task(Z);

	{grow,Rd} ->
	    R1 = R+Rd,
	    if R1 > ?MAX_RADIUS ->
		    task_explode(Z,P,V,R1,Color0);
	       true ->
		    task_wait(Z,P,V,R1,0,Color0)
	    end;
	    
	{color,Color1} ->
	    task_wait(Z,P,V,R,0,Color1)
    after Time ->
	    case task_collision(Z,V,R,Color0) of
		false -> 
		    {V,R,Color0};
		Res -> Res
	    end
    end.

%% explode by starting N-1 new tasks and set them of 
%% in different angles
task_explode(Z,P,V,R,_Color0) ->
    N = R-?INIT_RADIUS,
    A = (2*math:pi())/N,
    lists:foreach(
      fun(I) ->
	      Color = {255,255,255},
	      Vi = vector_add(V, vector_dir(A*I)),
	      Pi = vector_add(P,vector_scale(2,Vi)),
	      spawn(fun() ->
			    rand:uniform(),
			    task_init(rand:export_seed(),Pi,Vi,
				      ?INIT_RADIUS,Color)
		    end)
      end, lists:seq(0,N-2)),
    V1 = vector_dir(A*(N-1)),
    task_wait(Z,P,V1,?INIT_RADIUS,0,{255,255,255}).

%% check if we collide with other task
task_collision(Z,V,R,Color) ->
    Self = self(),
    case ets:lookup(lod0, Z) of
	[{_,Self}] -> false;
	[] -> false;
	[{_,Self},{_,Pid}|_] ->
	    collision(Z,V,R,Color,Pid);
	[{_,Pid}|_] ->
	    collision(Z,V,R,Color,Pid)
    end.

collision(Z,V,R,Color,Pid) ->
    case ets:lookup(task_info, Pid) of
	[] ->
	    false;
	[{_,_Pk,V1,_R1,Color1}] ->
	    Vd = vector_sub(V, V1),
	    Angle = vector_angle(Vd),
	    Dir = vector_angle(V),
	    V2 = vector_dir(Dir-Angle),
	    if self() < Pid ->
		    Pid ! {grow,(R-?INIT_RADIUS)+1},
		    delete_task(Z);
	       true ->
		    Color2 = color_avg(Color,Color1),
		    {V2,R,Color2}
	    end
    end.


do_blowup(Z,Color={255,255,255}) ->
    broadcast(1,Z,kill), Color;
do_blowup(_Z,Color) -> Color.


task_wall_collision(Pi={Xi,Yi},Vi={Vx,Vy},W,H,R) ->
    case clamp_switch(Xi, R, W-R, Vx) of
	false ->
	    case clamp_switch(Yi,R,H-R,Vy) of
		false -> {Pi,Vi};
		{Yi1,Vy1} -> {{Xi,Yi1},{Vx,Vy1}}
	    end;
	{Xi1,Vx1} ->
	    case clamp_switch(Yi,R,H-R,Vy) of
		false -> {{Xi1,Yi},{Vx1,Vy}};
		{Yi1,Vy1} -> {{Xi1,Yi1},{Vx1,Vy1}}
	    end
    end.


%% send to all but self
broadcast(Lod,Z,Message) ->
    SELF = self(),
    {Zi, Tab} = lod_map(Z,Lod),
    foreach(
      fun({_,Pid}) when Pid =/= SELF -> Pid ! Message;
	 (_) -> ok
      end, ets:lookup(Tab, Zi)).


clamp(X) when X > 255 -> 255;
clamp(X) -> X.

color_random() ->
    {rand:uniform(256)-1,rand:uniform(256)-1,rand:uniform(256)-1}.

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
	    
set_location(Z0) ->
    ets:insert(lod0, {Z0, self()}),
    ets:insert(lod1, {Z0 bsr 4, self()}),
    ets:insert(lod2, {Z0 bsr 8, self()}),
    ets:insert(lod3, {Z0 bsr 12, self()}),
    ets:insert(lod4, {Z0 bsr 16, self()}).

delete_task(Z) ->
    delete_location(Z),
    delete_info(),
    exit(normal).

delete_info() ->
    ets:delete(task_info, self()).

delete_location(Z) ->
    delete_location(Z, [lod0,lod1,lod2,lod3,lod4]).

delete_location(_Z, []) ->
    ok;
delete_location(Z, [Map|Maps]) ->
    ets:delete_object(Map, {Z,self()}),
    delete_location(Z bsr 4, Maps).


%% change location from Z0 to Z1 - in each level
update_location(Z0, Z1) ->
    update_location(Z0, Z1, [lod0,lod1,lod2,lod3,lod4]).

update_location(Z0, Z0, _) ->
    ok;
update_location(_Z0, _Z1, []) ->
    ok;
update_location(Z0, Z1, [Map|Maps]) ->
    ets:delete_object(Map, {Z0,self()}),
    ets:insert(Map, {Z1,self()}),
    update_location(Z0 bsr 4, Z1 bsr 4, Maps).

%% coords - z coord map
lod_map(Z, 0) -> { Z, lod0};
lod_map(Z, 1) -> { Z bsr 4, lod1};
lod_map(Z, 2) -> { Z bsr 8, lod2};
lod_map(Z, 3) -> { Z bsr 12, lod3};
lod_map(Z, 4) -> { Z bsr 16, lod4 }.

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

vector_length({X,Y}) ->
    math:sqrt(X*X + Y*Y).

vector_norm(V={X,Y}) ->
    L = vector_length(V),
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
