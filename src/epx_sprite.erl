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

-behaviour(gen_server).

-define(DEFAULT_WIDTH,  640).
-define(DEFAULT_HEIGHT, 480).
-define(DEFAULT_COLOR,  {255,0,0,255}).
-define(DEFAULT_FORMAT, argb).
-define(DEFAULT_INTERVAL, 10).

-define(NUM_SPRITES, 10).
%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-compile(export_all).

-type unsigned() :: non_neg_integer().

-record(sprite,
	{
	  id,                          %% uniq id / ref
	  x      :: float(),           %% center x-position
	  y      :: float(),           %% center y-position
	  xo     :: float(),           %% center x-offset in pixmap
	  yo     :: float(),           %% center y-offset in pixmap
	  width  :: float(),
	  height :: float(),
	  anglef :: function(),        %% angle function
	  angle  :: float(),           %% rotation angle (degree 0-360)
	  sx     :: float(),           %% speed x pixels/s
	  sy     :: float(),           %% speed y pixels/s
	  sa     :: float(),           %% speed degree/s
	  wrap   :: boolean(),         %% wrap or bounce
	  rd     :: epx:epx_pixmap(),  %% read pixmap
	  wr     :: epx:epx_pixmap()   %% write pixmap
	}).

-record(state,
	{
	  win        :: epx:epx_window(),  %% attached window
	  pixels     :: epx:epx_pixmap(),  %% attached pixels
	  background :: epx:epx_pixmap(),  %% background pixmap
	  width      :: unsigned(),        %% width of window
	  height     :: unsigned(),        %% height of window
	  sprites,     %% ets(#sprite{})
	  interval,    %% refresh interval = fps
	  %% background color / backdrop image
	  color      :: epx:epx_color(),   %% background color /
	  backdrop   :: epx:epx_pixmap(),  %% backdrop image
	  bx         :: float(),           %% backdrop top x
	  by         :: float(),           %% backdrop top y
	  bsx        :: float(),           %% backdrop x speed
	  bsy        :: float(),           %% backdrop y speed
	  box        :: float(),           %% backdrop x offset
	  boy        :: float()            %% backdrop y offset
	}).

%%%===================================================================
%%% API
%%%===================================================================

main() ->
    {ok,Game} = start_link(),
    LibDir = code:lib_dir(epx_demo),
    BackdropFile = filename:join([LibDir,"images", 
				  "the-blue-ocean-hd-wallpapers.png"]),
    {ok,Pixels} = load_pixmap(BackdropFile),

    set_backdrop_offset(Game, 0.0, 200.0),
    set_backdrop_image(Game, Pixels),
    set_backdrop_speed(Game,  30, 0.0),

    smc:start(),
    [begin
	 X = random(0, 640-32-1),
	 Y = random(0, 480-32-1),
	 {ok, S} = create_sprite(Game, 64, 64, 
				 [{x,X},
				  {y,Y},
				  {wrap,(I band 1) =:= 1},
				  {anglef,
				   fun(_S) ->
					   {Xv,_,_} = smc:read_motion(),
					   -Xv*45
				   end}
				 ]),
	 if I =:= 1 ->
		 set_image_file(Game, S, 
				filename:join([os:getenv("HOME"),
					       "Pictures","Tony","Tony.png"]));
	    true ->
		 {_,R,G,B} = random_rgb(),
		 fill_sprite(Game,S,{127,R,G,B})
	 end,
	 Sx = random(-50, 50),
	 Sy = random(-50, 50),
	 speed_sprite(Game, S, Sx, Sy), 
	 Sa = 0, %% random(0, 15),
	 rotate_speed_sprite(Game, S, Sa)
     end || I <- lists:seq(1, ?NUM_SPRITES)].

set_backdrop_image(Game, Pixels) ->
    gen_server:call(Game, {set_backdrop_image, Pixels}).

set_backdrop_offset(Game, X, Y) ->
    gen_server:call(Game, {set_backdrop_offset,X,Y}).

set_backdrop_speed(Game, Sx, Sy) ->
    gen_server:call(Game, {set_backdrop_speed,Sx,Sy}).

create_sprite(Game,W,H,Opts) when is_pid(Game) ->
    gen_server:call(Game, {create_sprite,W,H,Opts}).

%% move sprite to position x,y
moveto_sprite(Game,Ref,X,Y) when is_pid(Game),
				 is_reference(Ref),
				 is_number(X),
				 is_number(Y) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, Ref, [{#sprite.x,X},{#sprite.y,Y}]),
    ok.

%% set sprite x,y speed
speed_sprite(Game,Ref,X,Y) when is_pid(Game),
				is_reference(Ref),
				is_number(X),
				is_number(Y) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, Ref, [{#sprite.sx,X},{#sprite.sy,Y}]),
    ok.

%% set sprite rotation speed
rotate_speed_sprite(Game,Ref,A) when is_pid(Game),
				     is_reference(Ref),
				     is_number(A) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, Ref, [{#sprite.sa,A}]),
    ok.

rotate_sprite(Game,Ref,Angle) when is_number(Angle) ->
    Tab = sprite_table(Game),
    ets:update_element(Tab, Ref, [{#sprite.angle,Angle}]),
    ok.

fill_sprite(Game, Ref, Color) ->
    Tab = sprite_table(Game),
    [S] = ets:lookup(Tab, Ref),
    Wr = S#sprite.wr,
    Rd = S#sprite.rd,
    epx:pixmap_fill(Wr, Color),
    %% swap in sprite
    ets:update_element(Tab, Ref, [{#sprite.rd,Wr},{#sprite.wr,Rd}]),
    epx:pixmap_copy_to(Wr, Rd).

set_image_file(Game, Ref, File) ->
    case epx_image:load(File) of
	{ok,Img} ->
	    [Pixmap] = epx_image:pixmaps(Img),
	    set_image(Game, Ref, Pixmap);
	Error ->
	    Error
    end.

set_image(Game, Ref, Pixmap) ->
    Tab = sprite_table(Game),
    [S] = ets:lookup(Tab, Ref),
    Wr = S#sprite.wr,
    Rd = S#sprite.rd,
    epx:pixmap_scale(Pixmap, Wr, 
		     epx:pixmap_info(Wr, width),
		     epx:pixmap_info(Wr,height)),
    %% swap in sprite
    ets:update_element(Tab, Ref, [{#sprite.rd,Wr},{#sprite.wr,Rd}]),
    epx:pixmap_copy_to(Wr, Rd).
    

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

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
    application:start(epx),
    Width  = proplists:get_value(width, Options, ?DEFAULT_WIDTH),
    Height = proplists:get_value(height, Options, ?DEFAULT_HEIGHT),
    Color  = proplists:get_value(color, Options, ?DEFAULT_COLOR),
    Format = proplists:get_value(format, Options, ?DEFAULT_FORMAT),
    Interval = proplists:get_value(interval, Options, ?DEFAULT_INTERVAL),
    random:seed(erlang:now()),
    Pixels = epx:pixmap_create(Width,Height,Format),
    epx:pixmap_attach(Pixels),
    Background = epx:pixmap_create(Width,Height,Format),
    epx:pixmap_fill(Background, Color),
    Win = epx:window_create(30, 30, Width, Height,
			    [button_press, button_release]),
    epx:window_attach(Win),
    Sprites = ets:new(sprites, [public,{keypos,#sprite.id}]),
    State =  #state{ win = Win,
		     pixels = Pixels,
		     color  = Color,
		     interval = Interval,
		     width = Width, 
		     height = Height,
		     sprites = Sprites,
		     background = Background,
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

handle_call({create_sprite,W,H,Opts},_From,State) ->
    Format = epx:pixmap_info(State#state.pixels,pixel_format),
    Rd     = epx:pixmap_create(W,H,Format),
    Wr     = epx:pixmap_create(W,H,Format),
    Ref    = make_ref(),
    Sprite = #sprite { id = Ref, 
		       x  = proplists:get_value(x, Opts, 0.0),
		       y  = proplists:get_value(y, Opts, 0.0),
		       xo = W / 2, 
		       yo = H / 2,
		       width  = W,
		       height = H,
		       sx = proplists:get_value(sx, Opts, 0.0),
		       sy = proplists:get_value(sy, Opts, 0.0),
		       sa = proplists:get_value(sa, Opts, 0.0),
		       anglef = proplists:get_value(anglef, Opts, undefined),
		       angle = proplists:get_value(angle, Opts, 0.0),
		       wrap = proplists:get_bool(wrap,Opts),
		       rd = Rd, 
		       wr = Wr },
    ets:insert(State#state.sprites, Sprite),
    {reply, {ok, Ref}, State};
    
handle_call({set_backdrop_image,Pixels}, _From, State) ->
    {reply, ok, State#state { backdrop = Pixels }};

handle_call({set_backdrop_offset,X,Y}, _From, State) ->
    {reply, ok, State#state { box = float(X), boy = float(Y) }};
handle_call({set_backdrop_speed,Sx,Sy}, _From, State) ->
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
    io:format("Got: ~p\n", [_Info]),
    {stop, normal, State};
handle_info({timeout,_Ref,{refresh,Then}}, State) ->
    Now = time_us(),
    T   = Now - Then,
    State1 = redraw_window(State,T),
    update_window(State1),
    I = State1#state.interval,
    E = (T - I*1000) div 1000, %% >= 0,
    I1 = min(10, max(0,I-E)),
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
    epx:pixmap_detach(State#state.pixels),
    epx:window_detach(State#state.win),
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

%% may fail for some gif images
load_pixmap(File) ->
    case epx_image:load(File) of
	{ok,Img} ->
	    [Pixmap] = Img#epx_image.pixmaps,
	    {ok,Pixmap};
	Error ->
	    Error
    end.

redraw_window(State,T) ->
    Width  = State#state.width,
    Height = State#state.height,
    Tab = State#state.sprites,
    Ts  = T / 1000000,
    epx:pixmap_fill(State#state.background, State#state.color),
    State1 =
	case State#state.backdrop of
	    undefined -> 
		State;
	    Backdrop ->
		Bx = if State#state.bsx == 0 ->
			     State#state.bx;
			true ->
			     State#state.bx + State#state.bsx * Ts
		     end,
		By = if State#state.bsy == 0 ->
			     State#state.by;
			true ->
			     State#state.by + State#state.bsy * Ts
		     end,
		Bxi = trunc(Bx),
		Byi = trunc(By),
		%% io:format("backdrop: Bx=~p,By=~p,Bxi=~p,Byi=~p\n", 
		%%   [Bx,By,Bxi,Byi]),
		if Bxi =/= 0 ->
			epx:pixmap_scroll(Backdrop, Backdrop, Bxi, 0, 
					  true, black);
		   true ->
			ok
		end,
		if Byi =/= 0 ->
			epx:pixmap_scroll(Backdrop, Backdrop, 0, Byi, 
					  true, black);
		   true ->
			ok
		end,
		epx:pixmap_copy_area(Backdrop, State#state.background,
				     trunc(State#state.box),
				     trunc(State#state.boy),
				     0, 0, Width, Height, []),
		State#state { bx = Bx-Bxi, by = By-Byi }
	end,
    ets:foldl(
      fun(S,_Acc) ->
	      Ref = S#sprite.id,
	      Wi   = trunc(S#sprite.width),
	      Hi   = trunc(S#sprite.height),

	      X0 = if S#sprite.sx == 0 ->
			  S#sprite.x;
		     true ->
			  S#sprite.x + S#sprite.sx * Ts
		  end,
	      Y0 = if S#sprite.sy == 0 -> 
			   S#sprite.y;
		      true ->
			   S#sprite.y + S#sprite.sy * Ts
		   end,
	      Av = if is_function(S#sprite.anglef) ->
			   (S#sprite.anglef)(S);
		      true ->
			   S#sprite.angle
		   end,
	      A0 = if S#sprite.sa == 0 ->
			   Av;
		      true ->
			   Av + S#sprite.sa * Ts
		   end,
	      A = math2:fmod(A0, 360),
	      Li = max(Wi,Hi),
	      X = if S#sprite.wrap ->
			  if X0 < -Li -> Width+Li;
			     X0 >= Width+Li -> -Li; 
			     true -> X0 
			  end;
		     true ->
			  X0
		  end,
	      Y = if S#sprite.wrap ->
			  if Y0 < -Li -> Height+Li; 
			     Y0 >= Height+Li -> -Li;
			     true -> Y0 
			  end;
		     true ->
			  Y0
		  end,
	      Sx0 = S#sprite.sx,
	      Sx = if not S#sprite.wrap ->
			   if X0 < 0, Sx0<0 -> -Sx0;
			      X0 >= Width-Li,Sx0>0 -> -Sx0;
			      true -> Sx0
			   end;
		      true ->
			   Sx0
		   end,
	      Sy0 = S#sprite.sy,
	      Sy = if not S#sprite.wrap ->
			   if Y0 < 0, Sy0<0 -> -Sy0;
			      Y0 >= Height-Li,Sy0>0 -> -Sy0;
			      true -> Sy0
			   end;
		      true ->
			   Sy0
		   end,
	      Angle = radians(A),

	      ets:update_element(Tab, Ref, [{#sprite.x,X},
					    {#sprite.y,Y},
					    {#sprite.sx,Sx},
					    {#sprite.sy,Sy},
					    {#sprite.angle, A}]),

	      if Angle == 0 ->
		      epx:pixmap_copy_area(
			S#sprite.rd,
			State#state.background,
			0, 0, 
			trunc(X), trunc(Y),
			Wi, Hi, [blend]);
		 true ->
		      epx:pixmap_rotate_area(
			S#sprite.rd,
			State#state.background,
			Angle,
			0, 0,
			trunc(S#sprite.xo),
			trunc(S#sprite.yo),
			trunc(X + S#sprite.xo),
			trunc(Y + S#sprite.yo),
			Wi, Hi, [blend])
	      end
      end, [], Tab),
    State1.


    
update_window(State) ->
    epx:pixmap_copy_to(State#state.background, State#state.pixels),
    epx:pixmap_draw(State#state.pixels, 
		    State#state.win, 0, 0, 0, 0, 
		    State#state.width, 
		    State#state.height).

    
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
    math2:fmod(X-Min, L) + Min;
wrap(X, Min, Max) when X < Min ->
    L = (Max-Min)+1,
    Max - math2:fmod(Min-X, L);
wrap(X, _Min, _Max) -> X.


random_rgb() ->
    { 255, random(0,255), random(0,255), random(0,255) }.

random_argb() ->
    { random(0,255), random(0,255), random(0,255), random(0,255) }.

random(A, B) when is_integer(A), A =< B ->   
    random:uniform((B - A) + 1) - 1 + A.
