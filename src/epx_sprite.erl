%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%     epx sprite drawing kit
%%% @end
%%% Created : 27 Feb 2013 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(epx_sprite).

-include_lib("epx/include/epx_image.hrl").
-include("sprite.hrl").

-behaviour(gen_server).

-define(DEFAULT_WIDTH,  640).
-define(DEFAULT_HEIGHT, 480).
-define(DEFAULT_COLOR,  {255,0,0,255}).
-define(DEFAULT_FORMAT, argb).
-define(DEFAULT_INTERVAL, 10).

-define(G, 6.673E-11).
%% API
-export([start_link/0, start_link/1]).

-export([set_backdrop_image/2]).
-export([set_backdrop_offset/3]).
-export([set_backdrop_velocity/3]).
-export([create/4]).
-export([set_position/4]).
-export([set_velocity/4]).
-export([set_acceleration/4]).
-export([set_mass/3]).
-export([set_rotation_velocity/3]).
-export([set_rotation/3]).
-export([set_anglef/3]).
-export([update_pixels/3]).
-export([fill/3]).
-export([set_image_file/3]).
-export([set_image/3]).
-export([sprite_table/1]).

-export([get_velocity/2]).

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type unsigned() :: non_neg_integer().
-type sprite_id() :: unsigned().
-type sprite_option() ::
	{x, number()} |
	{y, number()} |
	{vx, number()} |
	{vy, number()} |
	{fx, number()} |
	{fy, number()} |
	{ax, number()} |
	{ay, number()} |
	{m, number()} |
	{va, number()} |
	{anglef, function() | undefined} |
	{angle, number()} |
	{wrap, boolean()} |
	{pixels, epx:epx_pixmap()} .

-type option() ::
	{width, unsigned()} |
	{height, unsigned()} |
	{color, epx:epx_color()} |
	{format, epx:epx_pixel_format()} |
	{interval, unsigned()}.

-record(state,
	{
	 win        :: epx:epx_window(),  %% attached window
	 screen     :: epx:epx_pixmap(),  %% attached pixels
	 background :: epx:epx_pixmap(),  %% background pixmap
	 width      :: unsigned(),        %% width of window
	 height     :: unsigned(),        %% height of window
	 alloc = [] :: [unsigned()],      %% id number allocation
	 sprites,     %% ets(#sprite{})
	 interval,    %% refresh interval = fps
	 %% background color / backdrop image
	 color      :: epx:epx_color(),   %% background color /
	 backdrop   :: epx:epx_pixmap(),  %% backdrop image
	 damping    :: float(),           %% global damping factor (0,1)
	 bx         :: float(),           %% backdrop top x
	 by         :: float(),           %% backdrop top y
	 bsx        :: float(),           %% backdrop x speed
	 bsy        :: float(),           %% backdrop y speed
	 box        :: float(),           %% backdrop x offset
	 boy        :: float()            %% backdrop y offset
	}).

-define(is_id(ID), (is_integer((ID)) andalso ((ID) > 0))).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

-spec start_link() -> {ok,pid()} | {error,Reason::atom()}.
start_link() ->
    start_link([]).

-spec start_link(Options::[option()]) ->
			{ok,pid()} | {error,Reason::atom()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

set_backdrop_image(Game, Pixels) ->
    gen_server:call(Game, {set_backdrop_image, Pixels}).

set_backdrop_offset(Game, X, Y) ->
    gen_server:call(Game, {set_backdrop_offset,X,Y}).

set_backdrop_velocity(Game, Sx, Sy) ->
    gen_server:call(Game, {set_backdrop_velocity,Sx,Sy}).

-spec create(Game::pid(),Width::unsigned(),Hieght::unsigned(),
	     Options::[sprite_option()]) -> {ok,Sprite::sprite_id()}.

create(Game,W,H,Opts) when is_pid(Game) ->
    gen_server:call(Game, {create,W,H,Opts}).

%% @doc			   
%%   Move sprite to position x,y
%% @end
-spec set_position(Game::pid(),ID::sprite_id(),
		   X::number(),Y::number()) -> ok.

set_position(Game,ID,X,Y) when is_pid(Game),
			       ?is_id(ID),
			       is_number(X),
			       is_number(Y) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, ID, [{#sprite.x,X},{#sprite.y,Y}]),
    ok.

%% set sprite x,y speed
-spec set_velocity(Game::pid(),ID::sprite_id(),
		   Vx::number(),Vy::number()) -> ok.

set_velocity(Game,ID,Vx,Vy) when is_pid(Game),
				 ?is_id(ID),
				 is_number(Vx),
				 is_number(Vy) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, ID, [{#sprite.vx,Vx},{#sprite.vy,Vy}]),
    ok.

%% set sprite x,y acceleration
-spec set_acceleration(Game::pid(),ID::sprite_id(),
		       Vx::number(),Vy::number()) -> ok.

set_acceleration(Game,ID,Ax,Ay) when 
      is_pid(Game),
      ?is_id(ID),
      is_number(Ax),
      is_number(Ay) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, ID, [{#sprite.ax,Ax},{#sprite.ay,Ay}]),
    ok.

%% set sprite mass
-spec set_mass(Game::pid(),ID::sprite_id(),M::number()) -> ok.

set_mass(Game,ID,M) when is_pid(Game),
			  ?is_id(ID),
			  is_number(M) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, ID, [{#sprite.m,M}]),
    ok.


%% set sprite rotation velocity
-spec set_rotation_velocity(Game::pid(),Ref::sprite_id(),
			   Va::number()) -> ok.
set_rotation_velocity(Game,ID,Va) when is_pid(Game),
				       ?is_id(ID),
				       is_number(Va) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, ID, [{#sprite.va,Va}]),
    ok.

set_rotation(Game,ID,Angle) when is_number(Angle) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, ID, [{#sprite.angle,Angle}]),
    ok.

set_anglef(Game,ID,AngleF) when is_function(AngleF, 1) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, ID, [{#sprite.anglef,AngleF}]),
    ok.

%% get sprite x,y speed
-spec get_velocity(Game::pid(),ID::sprite_id()) -> 
			  {Vx::number(),Vy::number()}.

get_velocity(Game,ID) when is_pid(Game),
			   ?is_id(ID) ->
    Tab = sprite_table(Game),
    Vx = ets:lookup_element(Tab, ID, #sprite.vx),
    Vy = ets:lookup_element(Tab, ID, #sprite.vy),
    {Vx,Vy}.

update_pixels(Game, ID, Fun) ->
    Tab = sprite_table(Game),
    [S] = ets:lookup(Tab, ID),
    case S#sprite.wr of
	undefined ->
	    error;   %% can not write to static map
	Wr ->
	    Rd = S#sprite.rd,
	    Fun(Wr),  %% user update the Wr pixmap
	    %% swap in sprite
	    ets:update_element(Tab, ID, [{#sprite.rd,Wr},{#sprite.wr,Rd}]),
	    epx:pixmap_copy_to(Wr, Rd)
    end.
    
fill(Game, ID, Color) ->
    update_pixels(Game, ID,
		  fun(Pixmap) ->
			  epx:pixmap_fill(Pixmap, Color)
		  end).

set_image_file(Game, ID, File) ->
    case epx_image:load(File) of
	{ok,Img} ->
	    [Pixmap] = epx_image:pixmaps(Img),
	    set_image(Game, ID, Pixmap);
	Error ->
	    Error
    end.

set_image(Game, Ref, Image) ->
    update_pixels(Game, Ref,
		  fun(Pixmap) ->
			  epx:pixmap_scale(Image, Pixmap,
					   epx:pixmap_info(Pixmap,width),
					   epx:pixmap_info(Pixmap,height))
		  end).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Options) ->
    Width  = proplists:get_value(width, Options, ?DEFAULT_WIDTH),
    Height = proplists:get_value(height, Options, ?DEFAULT_HEIGHT),
    Color  = proplists:get_value(color, Options, ?DEFAULT_COLOR),
    Format = proplists:get_value(format, Options, ?DEFAULT_FORMAT),
    Interval = proplists:get_value(interval, Options, ?DEFAULT_INTERVAL),
    Damping  = proplists:get_value(damping, Options, 1.0),
    Screen   = epx:pixmap_create(Width,Height,Format),
    epx:pixmap_attach(Screen),
    Background = epx:pixmap_create(Width,Height,Format),
    epx:pixmap_fill(Background, Color),
    Win = epx:window_create(30, 30, Width, Height,
			    [button_press, button_release]),
    epx:window_attach(Win),
    Sprites = ets:new(sprites, [public,{keypos,#sprite.id}]),
    State =  #state{ win        = Win,
		     screen     = Screen,
		     color      = Color,
		     interval   = Interval,
		     width      = Width, 
		     height     = Height,
		     sprites    = Sprites,
		     background = Background,
		     damping    = Damping,
		     bx = 0.0,
		     by = 0.0,
		     bsx = 0.0,
		     bsy = 0.0,
		     box = 0.0,
		     boy = 0.0
		   },
    update_window(State),
    erlang:start_timer(Interval,self(),{refresh,time_us()}),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_sprites, _From, State) ->
    {reply, {ok,State#state.sprites}, State};

handle_call({create,W,H,Opts},_From,State) ->
    Format = epx:pixmap_info(State#state.screen,pixel_format),
    {Rd,Wr} =
	case proplists:get_value(pixels, Opts, undefined) of
	    undefined ->
		{epx:pixmap_create(W,H,Format),
		 epx:pixmap_create(W,H,Format)};
	    Pixels ->
		{Pixels, undefined}
	end,
    {ID,State1} = make_id(State),
    %% FIXME make matrix version !
    io:format("create ~w: Rd = ~p\n", [ID, Rd]),

    Sprite = #sprite { id = ID,
		       x  = proplists:get_value(x, Opts, 0.0),
		       y  = proplists:get_value(y, Opts, 0.0),
		       xo = proplists:get_value(xo, Opts, W / 2),
		       yo = proplists:get_value(yo, Opts, H / 2),
		       width  = W,
		       height = H,
		       vx = proplists:get_value(vx, Opts, 0.0),
		       vy = proplists:get_value(vy, Opts, 0.0),
		       fx = proplists:get_value(fx, Opts, 1.0),
		       fy = proplists:get_value(fy, Opts, 1.0),
		       ax = proplists:get_value(ax, Opts, 0.0),
		       ay = proplists:get_value(ay, Opts, 0.0),
		       m  = proplists:get_value(m, Opts,  0.0),
		       va = proplists:get_value(va, Opts, 0.0),
		       anglef = proplists:get_value(anglef, Opts, undefined),
		       angle = proplists:get_value(angle, Opts, 0.0),
		       wrap = proplists:get_bool(wrap,Opts),
		       rd = Rd, 
		       wr = Wr },
    ets:insert(State1#state.sprites, Sprite),
    {reply, {ok, ID}, State1};
    
handle_call({set_backdrop_image,Pixels}, _From, State) ->
    {reply, ok, State#state { backdrop = Pixels }};

handle_call({set_backdrop_offset,X,Y}, _From, State) ->
    {reply, ok, State#state { box = float(X), boy = float(Y) }};
handle_call({set_backdrop_velocity,Sx,Sy}, _From, State) ->
    {reply, ok, State#state { bsx = float(Sx), bsy = float(Sy) }};

handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info={epx_event,Win,close}, State) when State#state.win =:= Win ->
    io:format("Stopped: ~p\n", [_Info]),    
    {stop, normal, State};
handle_info({timeout,_Ref,{refresh,Then}}, State) ->
    Now = time_us(),
    T   = Now - Then,
    State1 = redraw_window(State,T),
    update_window(State1),
    I = State1#state.interval,
    E = (T - I*1000) div 1000, %% >= 0,
    I1 = max(10, max(0,I-E)),
    erlang:start_timer(I1, self(),{refresh,Now}),
    {noreply, State1};
handle_info(_Info, State) ->
    io:format("Got: ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    io:format("terminate: ~p\n", [_Reason]),
    epx:window_detach(State#state.win),
    epx:pixmap_detach(State#state.screen),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_id(State) ->
    case State#state.alloc of
	[ID|Alloc] ->
	    {ID, State#state { alloc = Alloc }};
	[] ->
	    ID = ets:info(State#state.sprites, size) + 1,
	    {ID, State}
    end.

%% function to cache the sprite table in local process
sprite_table(Game) ->
    case get({epx_sprites,Game}) of
	undefined ->
	    {ok,Table} = gen_server:call(Game, get_sprites),
	    put({epx_sprite,Game}, Table),
	    Table;
	Table ->
	    Table
    end.

redraw_window(State,T) ->
    epx:pixmap_fill(State#state.background, State#state.color),  %% clear
    Ts  = T / 1000000,
    {Center,Sprites0} = update_sprites(State, Ts),
    State1 = update_backdrop(State, Ts),
    Sprites1 = bounce_sprites(Sprites0, State1),
    redraw_sprites(Sprites1, Center, State1).


redraw_sprites(Sprites, {Cx,Cy}, State) ->
    epx_gc:set_line_width(5),
    epx_gc:set_foreground_color(white),
    epx:draw_line(State#state.background,
		  {Cx-32, Cy}, {Cx+32, Cy}),
    epx:draw_line(State#state.background,
		  {Cx, Cy-32}, {Cx, Cy+32}),

    Tab = State#state.sprites,
    [
     begin
	 #sprite{x=X,y=Y,vx=Vx,vy=Vy,angle=A} = Si,
	 ets:update_element(Tab, Si#sprite.id,
			    [{#sprite.x,X},
			     {#sprite.y,Y},
			     {#sprite.vx,Vx},
			     {#sprite.vy,Vy},
			     {#sprite.angle, A}]),
	 Angle = radians(A),
	 if Angle == 0 ->
		 epx:pixmap_copy_area(
		   Si#sprite.rd,
		   State#state.background,
		   0, 0, 
		   X, Y, Si#sprite.width, Si#sprite.height,
		   [blend]);
	    true ->
		 epx:pixmap_rotate_area(
		   Si#sprite.rd,
		   State#state.background,
		   Angle,
		   0, 0,
		   Si#sprite.xo,
		   Si#sprite.yo,
		   X + Si#sprite.xo,
		   Y + Si#sprite.yo,
		   Si#sprite.width, Si#sprite.height,
		   [blend])
	 end
      end || Si <- Sprites],
    State.

bounce_sprites(Sprites, State) ->
    Width  = State#state.width,
    Height = State#state.height,

    WX0 = 0, WX1 = Width-1,
    WY0 = 0, WY1 = Height-1,

    %% limit the sprites against walls (fixme make walls dynamic)
    [begin
	 SX0 = Si#sprite.x, SX1 = Si#sprite.x + Si#sprite.width - 1,
	 SY0 = Si#sprite.y, SY1 = Si#sprite.y + Si#sprite.height - 1,
	 if Si#sprite.wrap ->
		 {X,Vx} =
		     if SX0 >= WX1 ->
			     {WX0, Si#sprite.vx};
			SX1 =< WX0 ->
			     {WX1, Si#sprite.vx};
			true ->
			     {Si#sprite.x, Si#sprite.vx}
		     end,
		 {Y,Vy} = 
		     if SY0 >= WY1 ->
			     {WY0, Si#sprite.vy};
			SY1 =< WY0 ->
			     {WY1, Si#sprite.vy};
			true ->
			     {Si#sprite.y, Si#sprite.vy}
		     end,
		 Si#sprite{x=X,y=Y,vx=Vx,vy=Vy};
	    true ->
		 {X,Vx} =
		     if SX1 >= WX1 ->
			     {SX0-((SX1-WX1)+1),-Si#sprite.vx};
			SX0 =< WX0 ->
			     {SX0+((WX0-SX0)+1),-Si#sprite.vx};
			true ->
			     {Si#sprite.x, Si#sprite.vx}
		     end,
		 {Y,Vy} = 
		     if SY1 >= WY1 ->
			     {SY0-((SY1-WY1)+1),-Si#sprite.vy};
			SY0 =< WY0 ->
			     {SY0+((WY0-SY0)+1),-Si#sprite.vy};
			true ->
			     {Si#sprite.y, Si#sprite.vy}
		     end,
		 Si#sprite{x=X,y=Y,vx=Vx,vy=Vy}
	 end
     end || Si <- Sprites ].


%% update acceleration, velocity, position and rotation
update_sprites(State, Ts) ->
    Tab = State#state.sprites,
    %% first calculate the total mass and mass center
    D = State#state.damping,
    case ets:tab2list(Tab) of
	[] -> {{0.0,0.0},[]};
	Sprites ->
	    M = total_mass(Sprites),
	    C = mass_center(Sprites),
	    Sprites1 = calc_accel(Sprites),
	    {scale(1/M, C),
	     [begin
		  #sprite{x=X,y=Y,vx=Vx,vy=Vy,fx=Fx,fy=Fy,ax=Ax,ay=Ay} = Si,
		  Vx1 = (Vx + Ax*Ts)*D*Fx,
		  Vy1 = (Vy + Ay*Ts)*D*Fy,
		  
		  X1 = X + Vx1*Ts,
		  Y1 = Y + Vy1*Ts,
		  
		  Av = if is_function(Si#sprite.anglef) ->
			       (Si#sprite.anglef)(Si);
			  true ->
			       Si#sprite.angle
		       end,
		  A0 = Av + Si#sprite.va*Ts,
		  A1 = fmod(A0, 360),
		  Si#sprite{x=X1,y=Y1,vx=Vx1,vy=Vy1,angle=A1}
	      end || Si <- Sprites1]}
    end.

total_mass([Si|Sprites]) ->
    Si#sprite.m + total_mass(Sprites);
total_mass([]) ->
    0.0.

mass_center([#sprite{x=X,y=Y,xo=Xo,yo=Yo,m=M}]) ->
    {M*(X+Xo), M*(Y+Yo)};
mass_center([#sprite{x=X,y=Y,xo=Xo,yo=Yo,m=M}|Sprites]) ->
    add({M*(X+Xo), M*(Y+Yo)}, mass_center(Sprites)).

%%
%% calculate crossing point between:
%% P(t) =  ((X1-X0)*t + X0, (Y1-Y0)*t + Y0)
%% Q(s) =  ((X3-X2)*s + X2, (Y3-Y2)*s + Y2)
%% return false if not crossing or {X,Y} the crossing point
%%
line_intersect(X0,Y0,X1,Y1, X2,Y2,X3,Y3) ->
    S10X = (X1-X0), S10Y = (Y1-Y0),
    S32X = (X3-X2), S32Y = (Y3-Y2),
    Sd   = S10X*S32Y - S32X*S10Y,
    if Sd == 0 -> %% fixme: check lines overlap
	    false;
       true ->
	    S02X = (X0-X2), 
	    S02Y = (Y0-Y2),
	    Sn = S10X*S02Y - S10Y*S02X,
	    %% Tn = S32X*S02Y - S32Y*S02X,
	    S = Sn/Sd,
	    if S < 0; S > 1 -> {false,S};
	       true -> {S32X*S + X2, S32Y*S + Y2}
	    end
    end.

%%
%% Plain simple quadratic gravity calculation
%%
calc_accel(Sprites) ->
    G = calc_gravity_(Sprites, #{}),  %% get all Fij i<j (kind of)
    Indices = [S#sprite.id || S <- Sprites],
    calc_accel_(Sprites, [], Indices, G).

calc_accel_([S=#sprite{id=I}|Sprites], Acc, Indices, G) ->
    {Ax,Ay} = calc_accel_i(Indices, I, {0.0,0.0}, G),
    %% io:format("~w: accel=~g,~g\n", [I, Ax, Ay]),
    calc_accel_(Sprites, [S#sprite{ax=Ax,ay=Ay}|Acc], Indices, G);
calc_accel_([], Acc, _Indices, _G) ->
    Acc.

calc_accel_i([I|Is], I, A, G) -> %% =0
    calc_accel_i(Is, I, A, G);
calc_accel_i([J|Is], I, A, G) ->
    case maps:find({I,J}, G) of
	error ->
	    {Fij,Nx,Ny} = maps:get({J,I},G),
	    calc_accel_i(Is, I, add({-Fij*Nx,-Fij*Ny}, A), G);
	{ok,{Fij,Nx,Ny}} ->
	    calc_accel_i(Is, I, add({Fij*Nx,Fij*Ny}, A), G)
    end;
calc_accel_i([], _I, A, _G) ->
    A.
    
%% calculate Fij = Fji = -G*Mi*Mj/Dij^2
calc_gravity_([#sprite{id=I,x=Xi,y=Yi,xo=Xoi,yo=Yoi,m=Mi}|Sprites], G) ->
    G1 = calc_gravity_(Sprites,I,Xi+Xoi,Yi+Yoi,Mi,G),
    calc_gravity_(Sprites, G1);
calc_gravity_([], G) ->
    G.

calc_gravity_([#sprite{id=J,x=Xj,y=Yj,xo=Xoj,yo=Yoj,m=Mj}|Sprites],I,X,Y,M,G) ->
    Dx = X-(Xj+Xoj),
    Dy = Y-(Yj+Yoj),
    D = max(max(Xoj,Yoj),math:sqrt(Dx*Dx + Dy*Dy)),
    if abs(D) < 0.01 ->
	    calc_gravity_(Sprites,I,X,Y,M,G#{ {I,J} => {0.0,0.0,0.0}});
       true ->
	    Nx = Dx/D,
	    Ny = Dy/D,
	    Fij = (-?G*M*Mj)/(D*D),
	    calc_gravity_(Sprites,I,X,Y,M,G#{ {I,J} => {Fij,Nx,Ny}})
    end;
calc_gravity_([],_I,_X,_Y,_M,G) ->
    G.


%% newton mass equation F1 = F2 = G*(M1*M2)/R^2, a = F/m
%% calc_gravity(F,#sprite{id=ID},#sprite{id=ID}) ->
%%     F;
%% calc_gravity(_F={Fx,Fy},
%% 	     #sprite{x=Xi,y=Yi,xo=Xoi,yo=Yoi}, 
%% 	     #sprite{x=Xj,y=Yj,xo=Xoj,yo=Yoj,m=Mj}) ->
%%     Dx = (Xi+Xoi)-(Xj+Xoj),
%%     Dy = (Yi+Yoi)-(Yj+Yoj),
%%     R0 = math:sqrt(Dx*Dx + Dy*Dy),
%%     %% disallow that distance is too small, fixme make better 
%%     Wij = max(Xoi,Xoj),
%%     R  = max(R0, Wij),
%%     Nx = Dx/R,
%%     Ny = Dy/R,
%%     Fij = (-?G*Mj)/(R*R),
%%     {Fx+Fij*Nx, Fy+Fij*Ny}.


%% update backdrop image
update_backdrop(State, _Ts) when State#state.backdrop =:= undefined ->
    State;
update_backdrop(State, Ts) ->
    Backdrop = State#state.backdrop,
    Bx = State#state.bx + State#state.bsx * Ts,
    By = State#state.by + State#state.bsy * Ts,
    epx:pixmap_scroll(Backdrop, Backdrop, Bx, By, true, black),
    epx:pixmap_copy_area(Backdrop, State#state.background,
			 State#state.box,
			 State#state.boy,
			 0, 0, State#state.width, State#state.height, []),
    State#state { bx = Bx-floor(Bx), by = By-floor(By) }.
    

update_window(State) ->
    epx:pixmap_copy_to(State#state.background, State#state.screen),
    epx:pixmap_draw(State#state.screen, 
		    State#state.win, 0, 0, 0, 0, 
		    State#state.width, 
		    State#state.height).


may_overlap(#sprite{x=Xi,y=Yi,width=Wi,height=Hi},
	    #sprite{x=Xj,y=Yj,width=Wj,height=Hj}) ->
    case epx_rect:intersect({Xi-Wi/2,Yi-Hi/2,Wi,Hi},
			    {Xj-Wj/2,Yj-Hj/2,Wj,Hj}) of
	{0,0,0,0} ->
	    false;
	_ ->
	    true
    end.

time_step(Time, N, Tr, Fun, Acc) ->
    Td = trunc(Time*1000000),  %% number of us to reach target
    T0 = time_us(),            %% start time
    T1 = T0 + Td,              %% stop time
    Ts = Td / N,               %% time step
    time_step_(T0, T0, Ts, Td, Tr, T0, T1, Fun, Acc).

time_step_(Ti, Tj, Ts, Td, Tr, T0, T1, Fun, Acc) ->
    if Ti >= T1 ->  %% we are done
	    Fun(1.0,Acc);
       true ->
	    T = (Ti-T0)/Td,
	    Acc1 = Fun(T, Acc),
	    Tk = trunc(Ti+Ts),
	    if Tk >= T1 ->
		    Fun(1.0,Acc1);
	       true ->
		    Te = (Tj-Ti), %% error
		    Tmo = max(Tr, trunc((Ts + Te) / 1000)),
		    receive after Tmo -> ok end,
		    time_step_(time_us(),Tk,Ts, Td, Tr, T0, T1, Fun, Acc1)
	    end
    end.

time_us() ->
    {Ms,S,Us} = os:timestamp(),
    ((Ms*1000000 + S)*1000000) + Us.

scale(S, {X, Y}) ->
    {S*X, S*Y}.

subtract({X1,Y1}, {X0,Y0}) ->
    {X1-X0, Y1-Y0}.

add({X1,Y1}, {X0,Y0}) ->
    {X1+X0, Y1+Y0}.

add({X0,Y0}, X1,Y1) ->
    {X1+X0, Y1+Y0};
add(X1,Y1, {X0,Y0}) ->
    {X1+X0, Y1+Y0}.

add(X1,Y1, X0,Y0) ->
    {X1+X0, Y1+Y0}.    

distance({X1,Y1},{X2,Y2}) ->
    distance(X1,Y1,X2,Y2).
distance(X1,Y1,X2,Y2) ->
    Dx = X2-X1,
    Dy = Y2-Y1,
    math:sqrt(Dx*Dx + Dy*Dy).

radians(Degree) when Degree == 0 ->
    Degree;
radians(Degree) when Degree > 0 ->
    D0 = Degree / 360.0,
    A0 = trunc(D0),
    (D0-A0)*2*math:pi();
radians(Degree) when Degree < 0 ->
    D0 = Degree / 360.0,
    A0 = trunc(D0),
    (D0-(A0-1))*2*math:pi().

isign(X, Y) when X < 0 -> -trunc(Y);
isign(_X, Y) -> trunc(Y).

clamp(X, Min, _Max) when X < Min -> Min;
clamp(X, _Min, Max) when X > Max -> Max;
clamp(X, _Min, _Max) -> X.

wrap(X, Min, Max) when X > Max ->
    L = (Max-Min)+1,
    fmod(X-Min, L) + Min;
wrap(X, Min, Max) when X < Min ->
    L = (Max-Min)+1,
    Max - fmod(Min-X, L);
wrap(X, _Min, _Max) -> X.

fmod(A, B) when is_number(A), is_number(B), B =/= 0 ->
    AB = abs(A / B),
    C = (AB - trunc(AB))*abs(B),
    if A < 0 -> -C;
       true -> C
    end.
