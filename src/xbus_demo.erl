%% mini xbus demo app

-module(xbus_demo).
-export([start/0, start/1]).
-export([show/0]).

start() ->
    start(#{}).

start(Options) ->
    setup(Options),
    spawn(fun() -> producer(Options) end),
    spawn(fun() -> main() end).

%% create meta channels
setup(Options) ->
    Min = maps:get(min, Options, 5000),
    Max = maps:get(max, Options, Min+5000),
    xbus:start(),
    xbus:pub_meta(<<"analog.value">>, [{comment,"Analog reading"},
				       {color, black},
				       {unit, "unsigned16"},
				       {min, Min}, {max, Max}
				      ]),
    xbus:pub_meta(<<"analog.filter">>, [{comment,"filter over analog.value"},
					{color, gray},
					{unit, "unsigned16"},
					{min, Min}, {max, Max}]),
    xbus:pub_meta(<<"voltage">>, [{comment,"Voltage for analog.avg"},
				  {unit,"V"},
				  {color, green},
				  {min, 0.0}, {max, 24.0}]).
    
main() ->
    xbus:sub(<<"analog.value">>),
    main_loop([0,0,0,0]).

main_loop(Vs) ->
    receive
	{xbus,_Pattern,#{ topic := <<"analog.value">>,
			  value := V }} when is_integer(V) ->
	    [_A,B,C,D] = Vs,
	    Sum = V+B+C+D,
	    Avg = Sum / 4,
	    xbus:pub(<<"analog.filter">>, Avg),
	    xbus:pub(<<"voltage">>, (Avg/65535)*16.5),
	    main_loop([B,C,D,V]);
	_Other ->
	    io:format("got ~w\n", [_Other]),
	    main_loop(Vs)
    end.

%% send a sine wave  5000 - 10000 about 10Hz
producer(Options) ->
    Min = maps:get(min, Options, 5000),
    Max = maps:get(max, Options, Min+5000),
    Freq = maps:get(freq, Options, 2.0),
    SampleRate = maps:get(sample_rate, Options, 4.0),
    producer_loop(Min,Max,Freq,SampleRate,0.0).

producer_loop(Min,Max,Freq,SampleRate,T) ->
    Y0 = math:sin(2*math:pi()*T/Freq),
    Y = ((Max+Min)+Y0*(Max-Min))/2,
    xbus:pub(<<"analog.value">>, trunc(Y)),
    timer:sleep(max(trunc(1/SampleRate*1000),10)),
    producer_loop(Min,Max,Freq,SampleRate,T+1/SampleRate).


%% show using epx_graph!    

show() ->
    show("*").

show(Pattern) ->
    Xbus = 
	[make_gauge(Topic,Meta) || {Topic,Meta} <- xbus:topics(Pattern)],
    epx_graph:start_link(Xbus).

make_gauge(Topic, Meta) ->
    Name = iolist_to_binary([Topic," ",proplists:get_value(unit,Meta,"")]),
    Min  = proplists:get_value(min,Meta,0),
    Max  = proplists:get_value(max,Meta,Min+10), %%?
    {gauge,[{name, Name},
	    {foreground, proplists:get_value(color,Meta,black)},
	    {min_value, Min},
	    {max_value, Max},
	    {sample_interval, 100},
	    {probe,
	     fun(_) ->
		     case xbus:read(Topic) of
			 [{_, Value, _Ts}] ->
			     [{Topic, Value}];
			 _ ->
			     []
		     end
	     end}]}.
