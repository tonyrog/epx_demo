%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2013 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(epx_graph).

-behaviour(gen_server).

-compile(export_all). %% debug
%% API
-export([start_link/1]).
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type unsigned() :: non_neg_integer().

-record(epx_graph,
	{
	  pid       :: pid(),                %% pid of sample process
	  x         :: integer(),            %% computed
	  y         :: integer(),            %% computed
	  width     = 128 :: integer(),      %% width in pixels
	  height    = 64  :: integer(),      %% height in pixels
	  background = white :: epx:color(), %% background color
	  foreground = black :: epx:color(), %% foreground color
	  pixels    :: epx:pixmap(),         %% stored pixels
	  min_value :: number(),             %% known minimum value | undefined
	  max_value :: number(),             %% known maximum value | undefined
	  type      :: counter | gauge,
	  name      :: term(),              %% name of gauge / counter
	  probe     :: atom() | function(),
	  sample_interval = 1000 :: integer(),   %% sample interval in ms
	  scroll_type = left :: left | write | overwrite | restart,
	  plot_type   = histo :: point | line | histo
	}).

-record(state,
	{
	  win        :: epx:epx_window(),    %% attached window
	  pixels     :: epx:epx_pixmap(),    %% attached pixels
	  width      :: unsigned(),          %% width of window
	  height     :: unsigned(),          %% height of window
	  graphs     :: [#epx_graph{}],      %% sub graphs
	  samples    :: ets:tab()
	}).

-record(pstate,
	{
	  win,
	  pixels,
	  font,
	  header_height
	}).
	  

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
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

start(Opts) ->
    gen_server:start(?MODULE, Opts, []).
 
interfaces() ->
    {ok,IFs} = inet:getiflist(),
    interfaces(IFs).

interfaces(IFs) ->
    MaxBytesPerSec = (2*1024*1024) div 8,  %% assume max around 2Mbit
    Counters =
	[ {counter,
	   [{name,I++".ibytes"},
	    {probe,sysctl_net},
	    {width, 300},
	    {sample_interval, 100},
	    {foreground, red},
	    {min_value, 0},
	    {max_value, MaxBytesPerSec div 10}
	   ]} ||
	    I <- IFs ],
    start_link(Counters).

temp() ->
    smc:start(),
    Thermals =
	[
	 {gauge,[{name, cpu_die},
		 {foreground, green},
		 {min_value, 0.0},
		 {max_value, 100.0},
		 {probe, fun(_) -> 
				 case smc:read_key(<<"TC0D">>) of
				     {ok,{_,_,T}} -> [{cpu_die,T}];
				     _ -> []
				 end
			 end}
		]},
	 {gauge,[{name, cpu_proximity},
		 {foreground, green},
		 {min_value, 0.0},
		 {max_value, 100.0},
		 {probe, fun(_) -> 
				 case smc:read_key(<<"TC0P">>) of
				     {ok,{_,_,T}} -> [{cpu_proximity,T}];
				     _ -> []
				 end
			 end}
		]}
	],
    start_link(Thermals).
    

	   
			   
	 
    
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
init(Opts) ->
    io:format("Opts = ~p\n", [Opts]),
    epx:start(),
    %% pick out all gauges / counters  from Option list
    {Gs0,Opts1} = parse_opts(Opts, [], []),
    io:format("Gs0 = ~p, Opts1 = ~p\n", [Gs0,Opts1]),
    {ok,Font} = epx_font:match([{size,10}]),
    Hdr = epx:font_info(Font, descent) + epx:font_info(Font, ascent) + 2,
    Gs = 
	case proplists:get_value(orientation, Opts1, vertical) of
	    vertical   -> place_graphs(Gs0, Hdr, 0, 0, vertical);
	    horizontal -> place_graphs(Gs0, Hdr, 0, 0, horizontal)
	end,
    io:format("Gs = ~p\n", [Gs]),
    Width = lists:max([G#epx_graph.x + G#epx_graph.width - 1  ||
			  G <- Gs]),
    Height = lists:max([G#epx_graph.y + G#epx_graph.height - 1  ||
			   G <- Gs]),
    Pixels = epx:pixmap_create(Width,Height),
    epx:pixmap_attach(Pixels),
    Win = epx:window_create(50, 50, Width, Height,
			    [button_press, button_release]),
    epx:window_attach(Win),

    Gs1 = lists:map(
	    fun(G) -> 
		    Pid = spawn_opt(
			    fun() ->
				    P = #pstate { win = Win,
						  pixels = Pixels,
						  font   = Font,
						  header_height = Hdr },
				    sample_process(P,
						   G#epx_graph { pid=self() })
			    end, [link]),
		    G#epx_graph { pid=Pid }
	    end, Gs),

    {ok, #state { win     = Win,
		  pixels  = Pixels,
		  width   = Width,
		  height  = Height,
		  graphs  = Gs1
		}
    }.


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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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

parse_opts([Opt | Opts], Gs, Acc) ->
    case Opt of
	{gauge,GOpts} ->
	    G = parse_graph_opts(GOpts, #epx_graph{type=gauge}),
	    parse_opts(Opts, [G|Gs], Acc);
	{counter,GOpts} ->
	    G = parse_graph_opts(GOpts, #epx_graph{type=counter}),
	    parse_opts(Opts, [G|Gs], Acc);
	_ ->
	    parse_opts(Opts, Gs, [Opt|Acc])
    end;
parse_opts([], Gs, Acc) ->
    {lists:reverse(Gs), lists:reverse(Acc)}.

parse_graph_opts([Opt|Opts], G) ->
    case Opt of
	{width, W} when is_integer(W), W >= 0 ->
	    parse_graph_opts(Opts, G#epx_graph { width = W });
	{height, H} when is_integer(H), H >= 0 ->
	    parse_graph_opts(Opts, G#epx_graph { height = H });
	{background, Color} ->
	    parse_graph_opts(Opts, G#epx_graph { background = Color });
	{foreground, Color} ->
	    parse_graph_opts(Opts, G#epx_graph { foreground = Color });
	{min_value, Value} when is_number(Value) ->
	    parse_graph_opts(Opts, G#epx_graph { min_value = Value });
	{max_value, Value} when is_number(Value) ->
	    parse_graph_opts(Opts, G#epx_graph { max_value = Value });
	{name, Name} ->
	    parse_graph_opts(Opts, G#epx_graph { name = Name });
	{probe, Func} when is_atom(Func);
			   is_function(Func) ->
	    parse_graph_opts(Opts, G#epx_graph { probe = Func });
	{sample_interval, IVal} when is_integer(IVal), IVal > 0 ->
	    parse_graph_opts(Opts, G#epx_graph { sample_interval = IVal });
	{scroll_type, Type} when is_atom(Type) -> %% fixme, check args
	    parse_graph_opts(Opts, G#epx_graph { scroll_type = Type })
    end;
parse_graph_opts([], G) ->
    Pixels = epx:pixmap_create(G#epx_graph.width, G#epx_graph.height),
    epx:pixmap_fill(Pixels, G#epx_graph.background),
    G#epx_graph { pixels = Pixels }.


place_graphs([G|Gs], H, X, Y, vertical) ->
    [ G#epx_graph { x = X, y = H+Y } |
      place_graphs(Gs, H, X, Y+H+G#epx_graph.height, vertical) ];
place_graphs([G|Gs], H, X, Y, horizontal) ->
    [ G#epx_graph { x = X, y = H+Y } |
      place_graphs(Gs, H, X+G#epx_graph.width, Y, horizontal) ];
place_graphs([], _H, _X, _Y, _Orientation) ->
    [].


    
    
get_sample(Name, Probe) when is_atom(Probe) ->
    [{_,Value}] = apply(Probe, get_value, [Name]),
    {Value, os:timestamp()};
get_sample(Name, Probe) when is_function(Probe,1) ->
    [{_,Value}] = Probe(Name),
    {Value, os:timestamp()}.


sample_process(P,G) ->
    %% initialize the sample table
    {V0,T0} = get_sample(G#epx_graph.name, G#epx_graph.probe),
    epx_gc:set_font(P#pstate.font),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(G#epx_graph.background),
    epx_gc:set_foreground_color(black),
    update_graph_label(P,G),
    epx_gc:set_foreground_color(G#epx_graph.foreground),
    sample_process(P,G,0,V0,T0).

sample_process(P,G,I,Vi,_Ti) ->
    Interval = G#epx_graph.sample_interval,
    receive
    after Interval ->
	    {Vj,Tj} = get_sample(G#epx_graph.name, G#epx_graph.probe),
	    %% Td = Tj - Ti
	    J = plot_position(G, I),
	    if G#epx_graph.type =:= counter ->
		    plot_value(G, Vj - Vi, J);
	       true ->
		    plot_value(G, Vj, J)
	    end,
	    update_graph(P,G),
	    sample_process(P,G,J,Vj,Tj)
    end.

update_graph_label(P,G) ->
    String = if is_atom(G#epx_graph.name) -> atom_to_list(G#epx_graph.name);
		is_binary(G#epx_graph.name) -> binary_to_list(G#epx_graph.name);
		is_list(G#epx_graph.name) -> G#epx_graph.name
	     end,
    H = P#pstate.header_height,
    %% Header location
    X0 = G#epx_graph.x,
    X1 = G#epx_graph.x + G#epx_graph.width - 1,
    Y0 = G#epx_graph.y - H,
    Y1 = G#epx_graph.y - 1,
    Font = P#pstate.font,
    {W,_H} = epx_font:dimension(P#pstate.font, String),
    X = X0 + erlang:max(0,(G#epx_graph.width - W) div 2),
    Y = Y0 + epx:font_info(Font, ascent),
    %% io:format("draw string ~s at (~w,~w)\n", [String, X, Y]),
    epx:draw_rectangle(P#pstate.pixels, X0, Y0, G#epx_graph.width, H),
    %% need to set alpha = 0 to get font blending to work!
    Color = {_Ac,Rc,Gc,Bc} = epx_gc:get_foreground_color(),
    epx_gc:set_foreground_color({0,Rc,Gc,Bc}),
    epx:draw_string(P#pstate.pixels, X, Y, String),
    epx_gc:set_foreground_color(Color),

    epx:draw_line(P#pstate.pixels, X0, Y1, X1, Y1),
    epx:pixmap_draw(P#pstate.pixels, P#pstate.win,
		    X0, Y0, X0, Y0,
		    G#epx_graph.width, H).
    

update_graph(P,G) ->
    epx:pixmap_copy_area(G#epx_graph.pixels, P#pstate.pixels,
			 0, 0, G#epx_graph.x, G#epx_graph.y,
			 G#epx_graph.width, G#epx_graph.height),
    epx:pixmap_draw(P#pstate.pixels, P#pstate.win,
		    G#epx_graph.x, G#epx_graph.y, 
		    G#epx_graph.x, G#epx_graph.y,
		    G#epx_graph.width, G#epx_graph.height).

plot_value(G, Yi, X) ->
    Height = G#epx_graph.height,
    MinY = if G#epx_graph.min_value =:= undefined -> 0;
	      true -> 
		   G#epx_graph.min_value
	   end,
    MaxY = if G#epx_graph.max_value =:= undefined ->
		   Height-1;
	     true -> 
		   G#epx_graph.max_value
	  end,
    Yv  = erlang:min(MaxY, erlang:max(MinY, Yi)),
    %% (MinY < Y < MaxY) -> (0 <= Yi < Height)
    Ys = (Yv - MinY) / (MaxY - MinY),
    Y0 = Height - 1,
    Y1  = trunc((1-Ys)*Height),
    case G#epx_graph.plot_type of
	point ->
	    epx:draw_point(G#epx_graph.pixels,X,Y1);
	line -> %% fixme passin the previous value
	    epx:draw_point(G#epx_graph.pixels,X-1,Y1,X,Y1);
	histo ->
	    epx:draw_line(G#epx_graph.pixels,X,Y0,X,Y1)
    end.
    

plot_position(G, I) ->
    Width = G#epx_graph.width,
    case G#epx_graph.scroll_type of
	overwrite ->
	    Next = I + 1,
	    if Next >= Width -> 0;
	       true -> Next
	    end;
	restart ->
	    Next = I + 1,
	    if Next >= Width ->
		    epx:pixmap_fill(G#epx_graph.pixels, G#epx_graph.background),
		    0;
	       true ->
		    Next
	    end;
	left ->
	    Next = I + 1,
	    if Next >= Width ->
		    epx:pixmap_scroll(G#epx_graph.pixels,G#epx_graph.pixels,
				      -1, 0, false, G#epx_graph.background),
		    Width - 1;
	       true ->
		    Next
	    end;
	right ->
	    Next = I - 1,
	    if Next < 0 ->
		    epx:pixmap_scroll(G#epx_graph.pixels,G#epx_graph.pixels,
				      1, 0, false, G#epx_graph.background),
		    0;
	       true ->
		    Next
	    end
    end.
