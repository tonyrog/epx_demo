%%% File    : mandel.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Mandelbrot
%%% Created :  2 Apr 2009 by Tony Rogvall <tony@rogvall.se>

-module(mandel).

-compile(export_all).
-import(lists, [reverse/1, map/2, foreach/2]).

-record(s,
	{
	  w,h,           %% width and hieght of window
	  win,           %% window
	  pix,           %% pixmap
	  spix,          %% pixmap back store
	  view,          %% current view params
	  ps = [],       %% parameter stack
	  opts,          %% options used
	  bg = {0,0,0},  %% background color
	  colors,        %% colormap
	  %% OpenCL stuff (optional)
	  use_double,    %% if cl_khr_fp64 on all devices used
	  cl_clu,        %% clu context
	  cl_program,    %% z2 program
	  cl_kernel,     %% z2 kernel
	  cl_queues      %% cl command
	 }).

-record(view,
	{
	  x,y,      %% left in area
	  w,h,      %% width and height of area
	  x0,y0,    %% top coordinate
	  x1,y1,    %% bottom coordinate
	  iter      %% max iteration
	 }).

-define(debug(F,A), ok).
%% -define(debug(F,A), io:format((F),(A))).


start() ->
    start([]).

start(Opts) when is_list(Opts) ->
    start(640, 480, Opts).

start(W, H) when is_integer(W), is_integer(H), W>0, H>0 ->
    start(W, H, []).

start(W, H, Opts) ->
    start(50, 50, W, H, Opts).

start(X, Y, W, H, Opts) ->
    epx:start(),
    spawn_link(fun() -> init(X,Y,W,H,Opts) end).

view0(W,H,Opts) ->
    #view { x=0, y=0, w=W, h=H, 
	    x0 = -2.0, x1 = 1.0, 
	    y0 = -1.0, y1 = 1.0,
	    iter = proplists:get_value(maxiter,Opts,30)
	    }.

init(X,Y,W,H,Opts) ->
    %% create the window
    Win = epx:window_create(X, Y, W, H, [key_press,key_release,
					 motion, left,  %% motion-left-button
					 resize,
					 button_press,button_release]),
    epx:window_attach(Win),  %% connect to default backend
    %% create the drawing pixmap
    Pix  = epx:pixmap_create(W, H, argb),
    SPix = epx:pixmap_create(W, H, argb),
    epx:pixmap_attach(Pix),   %% used for window drawing
    %% fill with some color
    View = view0(W,H,Opts),
    Colors = colors(View#view.iter),
    S0 = #s { w=W, h=H, win=Win, pix=Pix, spix=SPix,
	      view = View, 
	      opts=Opts,
	      bg = proplists:get_value(bg,Opts,{0,0,0}),
	      colors = Colors },
    S1 = init_cl(S0),
    epx:pixmap_fill(Pix, S1#s.bg),
    update_win(S1),
    self() ! redraw,
    loop(S1).

init_cl(S) ->
    case proplists:get_value(opencl, S#s.opts) of
	undefined ->
	    S;
	Val ->
	    Clu = case Val of
		      true -> clu:setup();
		      all -> clu:setup(all);
		      gpu -> clu:setup(gpu);
		      cpu -> clu:setup(cpu);
		      accel -> clu:setup(accel)
		  end,
	    case Clu of
		{error,Err} -> exit({clu_error, Err});
		_ -> ok
	    end,
	    Filename = filename:join([code:priv_dir(epx_demo),"z2_float.cl"]),
	    ?debug("build: ~s\n", [Filename]),
	    UseDouble = clu:devices_has_extension(Clu, "cl_khr_fp64"),
	    Options = if UseDouble -> "-DCONFIG_USE_DOUBLE=1";
			 true -> ""
		      end,
	    {ok, Program} = clu:build_source_file(Clu, Filename, Options),
	     ?debug("program built\n",[]),
	    {ok, Kernel} = cl:create_kernel(Program, "z2"),
	    ?debug("kernel created: ~p\n", [Kernel]),
	    %% Create the command queue for the first device
	    Qs = [begin {ok,Q}=cl:create_queue(clu:context(Clu),D,[]),Q end ||
		     D <- clu:device_list(Clu)],
	    ?debug("queues created\n",[]),
	    S#s { use_double = UseDouble,
		  cl_clu = Clu,
		  cl_program = Program,
		  cl_kernel = Kernel,
		  cl_queues = Qs }
    end.
	    

update_win(S) ->
    epx:pixmap_draw(S#s.pix, S#s.win, 0, 0, 0, 0, S#s.w, S#s.h).

update_win(S, X, Y, W, H) ->
    epx:pixmap_draw(S#s.pix, S#s.win, X, Y, X, Y, W, H).

close(S) ->
    if S#s.cl_clu == undefined ->
	    ok;
       true ->
	    cl:release_kernel(S#s.cl_kernel),
	    lists:foreach(fun(Q) -> cl:release_queue(Q) end, S#s.cl_queues),
	    cl:release_program(S#s.cl_program),
	    clu:teardown(S#s.cl_clu),
	    ok
    end.

loop(S) ->
    receive
	{epx_event,_Win, destroy} ->
	    ?debug("DESTROY\n",[]),
	    close(S);
	{epx_event,_Win, close} ->
	    ?debug("CLOSE\n",[]),
	    close(S);
	{epx_event,_Win,{button_press, [left], _Where={X0,Y0,_Z0}}} ->
	    ?debug("START SELECTION: where=~w\n", [_Where]),
	    epx_gc:set_fill_color({50,255,255,255}),
	    R = S#s.w / S#s.h,
	    epx:pixmap_copy_to(S#s.pix,S#s.spix), %% make a copy of the image
	    case select(S, {X0,Y0}, {X0,Y0,1,1},false,R) of
		{select, _Selection={{X0,Y0},{X1,Y1}}} ->
		    ?debug("STOP SELECTION: area=~w\n", [_Selection]),
		    epx_gc:set_fill_style(none),
		    P = new_view(S#s.view, rectangle(X0,Y0,X1,Y1),R),
		    draw(S#s.win, S#s.pix, P, S),
		    %% set new parameter and push the old
		    loop(S#s { view=P, ps=[S#s.view|S#s.ps]});
		{center, {X,Y}} ->
		    Cx = S#s.w div 2,
		    Cy = S#s.h div 2,
		    S1 = move(S, Cx-X, 0),
		    S2 = move(S1, 0, -(Cy-Y)),
		    loop(S2);
		_Error ->
		    loop(S)
	    end;
	{epx_event,_Win,{resize, {W,H,_D}}} ->
	    P0 = S#s.view,
	    P1 = P0#view { w=W, h=H },
	    S1 = S#s { w=W, h=H, view=P1 },
	    ?debug("RESIZE: ~w ~w\n", [W, H]),
	    %% redraw later... - fixme
	    loop(S1);

	{epx_event,_Win,{key_press, Sym, _Mod, _Code}} ->
	    case Sym of
		left  ->  
		    S1 = move(S, +(S#s.w div 3), 0),
		    loop(S1);
		right ->   
		    S1 = move(S, -(S#s.w div 3), 0),
		    loop(S1);
		up ->      
		    S1 = move(S, 0, -(S#s.h div 3)),
		    loop(S1);
		down ->
		    S1 = move(S, 0, +(S#s.h div 3)),
		    loop(S1);
		$+ ->
		    R = S#s.w / S#s.h,
		    Step = 10,
		    X0 = Step,
		    X1 = S#s.w-Step,
		    Y0 = Step,
		    Y1 = S#s.h-Step,
		    P = new_view(S#s.view,rectangle(X0,Y0,X1,Y1),R),
		    draw(S#s.win, S#s.pix, P, S),
		    loop(S#s { view=P });

		$- ->
		    R = S#s.w / S#s.h,
		    Step = -10,
		    X0 = Step,
		    X1 = S#s.w-Step,
		    Y0 = Step,
		    Y1 = S#s.h-Step,
		    P = new_view(S#s.view,rectangle(X0,Y0,X1,Y1),R),
		    draw(S#s.win, S#s.pix, P, S),
		    loop(S#s { view=P });
		
		27   ->
		    case S#s.ps of
			[] -> 
			    P = view0(S#s.w, S#s.h, S#s.opts),
			    S1 = S#s { view=P, ps=[] },
			    draw(S#s.win, S#s.pix, P,  S1),
			    loop(S1);
			[P|Ps] ->
			    S1 = S#s { view=P, ps=Ps },
			    draw(S#s.win, S#s.pix, P,  S1),
			    loop(S1)
		    end;
		_Key ->
		    ?debug("Got key: ~p\n", [_Key]),
		    loop(S)
	    end;

	redraw ->
	    draw(S#s.win,S#s.pix,S#s.view,S),
	    loop(S);
	_Other ->
	    %% io:format("Got: ~p\n", [_Other]),
	    loop(S)
    end.

msign(X,A) when X < 0 -> -A;
msign(X,A) when X > 0 ->  A;
msign(_,_A) -> 0.

move(S, Mx, My) ->
    Dx = abs(Mx),
    Dy = abs(My),
    Rx = Dx / S#s.w,
    Ry = Dy / S#s.h,
    P = S#s.view,
    Lx = Rx*(P#view.x1 - P#view.x0),
    Ly = Ry*(P#view.y1 - P#view.y0),
    %% scroll and make revealed pixel area black
    epx:pixmap_scroll(S#s.pix, S#s.pix, Mx, My, 0, S#s.bg),
    update_win(S),
    %% setup redraw area
    {X,W,X0,X1} =
	if %% left arrow - scroll right (move left)
	    Mx > 0 -> {P#view.x, Dx, P#view.x0-Lx, P#view.x0 };
	    %% right arrow - scroll left (move right)
	    Mx < 0 -> {P#view.w-Dx, Dx, P#view.x1, P#view.x1+Lx};
	    %%
	    true   -> {P#view.x,P#view.w, P#view.x0, P#view.x1 }
	end,
    {Y,H,Y0,Y1} =
	if  
	    %% down arrow - scroll up (move down)
	    My > 0 -> {P#view.h-Dy, Dy, P#view.y1, P#view.y1+Ly };
	    %% up arrow - scroll down (move up)
	    My < 0 -> {P#view.y, Dy, P#view.y0-Ly, P#view.y0 };
	    true   -> {P#view.y, P#view.h, P#view.y0, P#view.y1}
	end,
    T =P#view { x=X, y=Y, w=W, h=H, x0=X0, x1=X1, y0=Y0, y1=Y1 },
    draw(S#s.win, S#s.pix, T, S),  %% update area
    P1 = P#view { x0=P#view.x0 - msign(Mx,Lx), 
		  x1=P#view.x1 - msign(Mx,Lx),
		  y0=P#view.y0 + msign(My,Ly),
		  y1=P#view.y1 + msign(My,Ly) },
    S#s { view=P1 }.
    
    

%% scale P with the new rectangle (R is width height ratio)
new_view(P, {X,Y,_W1,H1}, R) ->
    H = max(2, H1),
    W = trunc(H*R),
    Xf = X / P#view.w,
    Yf = Y / P#view.h,

    Sx = W / P#view.w,
    Sy = H / P#view.h,

    X0 = P#view.x0 - Xf*(P#view.x0-P#view.x1),
    Y0 = P#view.y0 - Yf*(P#view.y0-P#view.y1),

    X1 = X0 + Sx*(P#view.x1-P#view.x0),
    Y1 = Y0 + Sy*(P#view.y1-P#view.y0),
    P#view { x0=X0, y0=Y0, x1=X1, y1=Y1 }.
    

select(S,Start={X0,Y0}, Selection0,Motion,R) ->
    receive
	{epx_event,_Win,{button_release, [left], {X,Y,_Z}}} ->
	    %% "restore" pixmap 
	    {X1,Y1,W1,H1} = Selection0,
	    epx:pixmap_copy_area(S#s.spix, S#s.pix, X1,Y1,X1,Y1,W1,H1),
	    epx:draw_rectangle(S#s.pix,X1,Y1,W1,H1),
	    update_win(S, X1, Y1, W1, H1),
	    if Motion ->
		    {select, {Start,{X,Y}}};
	       true ->
		    {center, Start}
	    end;
	{epx_event,_Win,{motion, [left], {X,Y,_}}} ->
	    %% "restore" pixmap
	    {X1,Y1,W1,H1} = Selection0,
	    epx:pixmap_copy_area(S#s.spix, S#s.pix, X1,Y1,X1,Y1,W1,H1),
	    %% new selection
	    {X2,Y2,_W2,H2}= rectangle(X0,Y0,X,Y),
	    %% R = Width/Height
	    W3 = trunc(H2*R),
	    %% Draw selection area
	    epx_gc:set_fill_style(blend),
	    epx:draw_rectangle(S#s.pix,X2,Y2,W3,H2),
	    epx_gc:set_fill_style(none),
	    %% Update screen
	    Selection1 = {X2,Y2,W3,H2},
	    {Xr,Yr,Wr,Hr} = epx_rect:union(Selection0, Selection1),
	    update_win(S, Xr, Yr, Wr, Hr),
	    select(S,Start,Selection1,true,R)
    end.


rectangle(X0,Y0,X1,Y1) ->
    { min(X0,X1), min(Y0,Y1), abs(X0-X1)+1,  abs(Y0-Y1)+1 }.

draw(Win,Pix,View,S) ->
    %% io:format("draw: view = ~p\n", [{view,View#view.x,View#view.y,View#view.w,View#view.h,float(View#view.x0),float(View#view.y0),float(View#view.x1),float(View#view.y1),View#view.iter}]),
    case proplists:get_bool(parallel,S#s.opts) of
	true ->
	    N = proplists:get_value(patch,S#s.opts,64),
	    pdraw(Win,Pix,View,S,N);
	false ->
	    draw(Pix,View,S),
	    epx:pixmap_draw(Pix,Win,
			    View#view.x,View#view.y,
			    View#view.x,View#view.y,
			    View#view.w,View#view.h)
    end.

pdraw(Win,Pix,P,S,N) ->
    if P#view.w == 0; P#view.h == 0 ->
	    ok;
       P#view.w < N; P#view.h < N ->
	    draw(Pix, P, S),
	    epx:pixmap_draw(Pix,Win,
			    P#view.x,P#view.y,
			    P#view.x,P#view.y,
			    P#view.w,P#view.h);
       true ->
	    %% start draw top left color
	    D = z2(P#view.iter, P#view.x0, P#view.y0),
	    epx_gc:set_fill_style(solid),
	    <<A,R,G,B>> = get_color(D,S#s.colors),
	    epx_gc:set_fill_color({A,R,G,B}),
	    epx:draw_rectangle(Pix, P#view.x, P#view.y, P#view.w, P#view.h),
	    epx:pixmap_draw(Pix,Win,
			    P#view.x,P#view.y,
			    P#view.x,P#view.y,
			    P#view.w,P#view.h),
	    %% split and update
	    SELF = self(),
	    Pids = map(
		     fun(Pxy) -> 
			     {spawn(fun() ->
					    pdraw(Win,Pix,Pxy,S,N), 
					    SELF ! {self(),ok} 
				    end),
			      Pxy}
		     end, split_view(P)),
	    wait(Pids, Win, Pix)
    end.
	    

wait([],_Win,_Pix) ->
    ok;
wait(Pids,Win,Pix) ->
    receive
	{Pid,ok} ->
	    case lists:keysearch(Pid,1,Pids) of
		false ->
		    wait(Pids,Win,Pix);
		{value,E={Pid,_P}} ->
		    wait(Pids--[E],Win,Pix)
	    end
    end.


%% divide view in 4 qudrants
split_view(P) ->
    W1 = P#view.w div 2,
    W2 = P#view.w - W1,
    H1 = P#view.h div 2,
    H2 = P#view.h - H1,
    Xm = (P#view.x1+P#view.x0) / 2.0,
    Ym = (P#view.y1+P#view.y0) / 2.0,
    [ 
      #view { x = P#view.x, y = P#view.y, w=W1, h=H1,
	       x0 = P#view.x0, x1 = Xm,
	       y0 = P#view.y0, y1 = Ym,
	       iter=P#view.iter },

      #view { x = P#view.x+W1, y = P#view.y, w=W2, h=H1,
	       x0 = Xm, x1 = P#view.x1,
	       y0 = P#view.y0, y1 = Ym,
	       iter=P#view.iter },

      #view { x = P#view.x, y = P#view.y+H1, w=W1, h = H2,
	       x0 = P#view.x0, x1 = Xm,
	       y0 = Ym, y1=P#view.y1,
	       iter=P#view.iter },

      #view { x = P#view.x+W1, y = P#view.y+H1, w=W2, h=H2,
	       x0 = Xm, x1 = P#view.x1,
	       y0 = Ym, y1=P#view.y1,
	       iter=P#view.iter } 
     ].


draw(Pix,P,S) when is_record(P,view) ->
    if P#view.w == 0; P#view.h == 0 ->
	    ok;
       true ->
	    Xs =(P#view.x1-P#view.x0)/P#view.w,
	    Ys =(P#view.y1-P#view.y0)/P#view.h,
	    if S#s.cl_clu == undefined ->
		    draw_y(Pix,P#view.h,P#view.y,P#view.y0,Xs,Ys,P,S);
	       true ->
		    draw_cl(Pix,Xs,Ys,P,S)
	    end
    end;
draw(Pix,[P|Ps],S) ->
    draw(Pix,P,S),
    draw(Pix,Ps,S);
draw(_Pix,[],_S) ->
    ok.

draw_cl(Pix,Xs,Ys,P,S) ->
    %% Create the output memory
    X0 = float(P#view.x0),  %% force float
    Y0 = float(P#view.y0),  %% force float
    W = P#view.w,
    H = P#view.h,
    %% N = W * H,
    Ctx = clu:context(S#s.cl_clu),
    Qs  = S#s.cl_queues,
    Ds  = clu:device_list(S#s.cl_clu),
    NumDevices = length(Ds),
    Hi  = (H+NumDevices-1) div NumDevices,
    VHi = (float(P#view.y1) - float(P#view.y0))/NumDevices,
    %% number of buffer bytes per device
    NumBytes = Hi*W*4,
    Bufs =
	[begin
	     {ok,Buf} = cl:create_buffer(Ctx,[read_write],NumBytes),Buf
	 end || _ <- lists:seq(1,NumDevices)],
    
    Es = 
	[begin
	     ?debug("device = ~p\n", [D]),
	     Yi = Y0+I*VHi,
	     ?debug("X0,Y0 = ~w, Hi=~w\n", [{X0,Yi},Hi]),
	     Real_t = if S#s.use_double -> double;
			 true -> float
		      end,
	     clu:apply_kernel_args(S#s.cl_kernel,
				   [{Real_t,X0}, {Real_t,Yi},
				    {Real_t,float(Xs)},{Real_t,float(Ys)},
				    {uint,W},{uint,Hi},
				    {uint,P#view.iter},Buf]),
	     Global = [W, H],
	     ?debug("global = ~w\n", [Global]),
	     Local = calc_local(D, Global),
	     ?debug("local = ~w\n", [Local]),
	     {ok,Event1} = cl:enqueue_nd_range_kernel(Q,
						      S#s.cl_kernel,
						      Global, Local, []),
	     ?debug("kernel queued event1 = ~p\n", [Event1]),
	     {ok,Event2} = cl:enqueue_read_buffer(Q,Buf,0,NumBytes,[Event1]),
	     ?debug("read buffer queued event2 = ~p\n", [Event2]),
	     {Event1,Event2}
	 end || {I,D,Q,Buf} <- zip(lists:seq(0, NumDevices-1), Ds, Qs, Bufs)],
    lists:foreach(fun(Q) -> cl:flush(Q) end, Qs),
    ?debug("flushed\n",[]),
    map(fun({E1,_}) ->  
		cl:wait(E1,1000)
	end, Es),
    ?debug("calculations are done\n",[]),
    PixelsLists =
	map(fun({_,E2}) ->
		    {ok,OutData} = cl:wait(E2,3000),
		    [ get_color(D,S#s.colors) || <<D:32/native>> <= OutData ]
	    end, Es),
    ?debug("got pixles\n",[]),
    foreach(
      fun({I,Pixels}) ->
	      epx:pixmap_put_pixels(Pix,
				    P#view.x,P#view.y+I*Hi,
				    P#view.w,Hi,argb,Pixels)
      end, lists:zip(lists:seq(0,NumDevices-1), PixelsLists)),

    foreach(fun(Buf) -> cl:release_mem_object(Buf) end, Bufs).

zip([A|As],[B|Bs],[C|Cs],[D|Ds]) ->
    [{A,B,C,D} | zip(As,Bs,Cs,Ds)];
zip([],[],[],[]) ->
    [].

%% calc_local(_D,_Global) ->
%%    [];
calc_local(D,[W,H]) -> %% disabled right now
    %% Max work group size
    {ok,MaxWorkGroupSize}=cl:get_device_info(D,max_work_group_size),
    %% ?debug("max_work_group_size = ~w\n", [MaxWorkGroupSize]),
    {ok,[LW,LH|_]} = cl:get_device_info(D, max_work_item_sizes),
    ?debug("1.local = ~w,~w\n", [LW,LH]),
    LW1 = imath:gcd(LW, W),
    LH1 = imath:gcd(LH, H),
    ?debug("2.local = ~w,~w\n", [LW1,LH1]),
    {LW2,LH2} = scale_down_work_item_sizes(LW1,LH1,MaxWorkGroupSize),
    ?debug("3.local = ~w,~w\n", [LW2,LH2]),
    %% Local1 = 1,
    [LW2,LH2].

scale_down_work_item_sizes(1,  H,  Max) -> {1,min(H,Max)};
scale_down_work_item_sizes(W,  1,  Max) -> {min(W,Max),1};
scale_down_work_item_sizes(W,  H,  Max) ->
    if W*H =< Max ->
	    {W,H};
       W band 1 =:= 0, H band 1 =:= 0 ->
	    scale_down_work_item_sizes(W div 2, H div 2, Max)
    end.


draw_y(_Pix,0,_Yi,_Y,_Xs,_Ys,_P,_S) ->
    ok;
draw_y(Pix,H,Yi,Y,Xs,Ys,P,S) ->
    Pixels = line_x(P#view.w,P#view.x0,Y,Xs,P,S),
    %% draw_x(Pix,P#view.w,P#view.x,Yi,P#view.x0,Y,Xs,P,S),
    epx:pixmap_put_pixels(Pix,P#view.x,Yi,P#view.w,1,argb,Pixels),
    draw_y(Pix,H-1,Yi+1,Y+Ys,Xs,Ys,P,S).

%% alternative that plot every x
draw_x(_Pix,0,_Xi,_Yi,_X,_Y,_Xs,_P,_S) ->
    ok;
draw_x(Pix,W,Xi,Yi,X,Y,Xs,P,S) ->
    D = z2(P#view.iter,X,Y),
    Pixel = get_color(D, S#s.colors),
    epx:pixmap_put_pixel(Pix, Xi, Yi, Pixel),
    draw_x(Pix,W-1,Xi+1,Yi,X+Xs,Y,Xs,P,S).

line_x(0,_X,_Y,_Xs,_P,_S) ->
    [];
line_x(W,X,Y,Xs,P,S) ->
    D = z2(P#view.iter,X,Y),
    [get_color(D, S#s.colors) | line_x(W-1,X+Xs,Y,Xs,P,S)].

get_color(0, Palette) -> element(1,Palette);
get_color(I, Palette) -> 
    Size = size(Palette)-1,  %% skip base color (element(0))
    Ix = ((I-1) rem Size) + 1,
    element(Ix+1, Palette).
    
%% Generate N+1 colors
colors(_N) ->
    %% colors_interp(N,{255,0,0},{0,255,255}).
    %% colors_random(N).
    colors_xaos().

colors_random(N) ->
    colors_random(N,[]).

colors_random(0,Acc) -> 
    list_to_tuple([<<255,0,0,0>>|Acc]);
colors_random(I,Acc) ->
    colors_random(I-1,[ <<255,(rand:uniform(16#1000000)-1):24>> | Acc]).

%% interpolate N colors between ColorA and ColorB
colors_interp(N,ColorA,ColorB) ->
    colors(N, 0.0, ColorB, ColorA, 1/N, []). %% generate backwards!

colors(0, _X, _ColorA, _ColorB, _Step, Acc) ->
    list_to_tuple([<<255,0,0,0>> | Acc]);
colors(I, X, ColorA={R0,G0,B0}, ColorB={R1,G1,B1}, Step, Acc) ->
    R = trunc(R0*(1-X) + R1*X),
    G = trunc(G0*(1-X) + G1*X),
    B = trunc(B0*(1-X) + B1*X),
    colors(I-1, X+Step, ColorA, ColorB, Step, [<<255,R,G,B>>|Acc]).

colors_xaos() -> 
    {
      << 255, 0, 0, 0 >>,
      << 255, 15, 14, 29 >>,
      << 255, 30, 29, 59 >>,
      << 255, 45, 44, 89 >>,
      << 255, 60, 59, 119 >>,
      << 255, 75, 74, 148 >>,
      << 255, 90, 89, 178 >>,
      << 255, 105, 104, 208 >>,
      << 255, 120, 119, 238 >>,
      << 255, 108, 105, 211 >>,
      << 255, 96, 91, 184 >>,
      << 255, 84, 77, 158 >>,
      << 255, 72, 63, 131 >>,
      << 255, 60, 49, 104 >>,
      << 255, 48, 35, 78 >>,
      << 255, 36, 21, 51 >>,
      << 255, 24, 7, 25 >>,
      << 255, 45, 14, 25 >>,
      << 255, 67, 21, 25 >>,
      << 255, 88, 29, 26 >>,
      << 255, 110, 36, 26 >>,
      << 255, 132, 43, 26 >>,
      << 255, 153, 51, 27 >>,
      << 255, 175, 58, 27 >>,
      << 255, 197, 66, 28 >>,
      << 255, 176, 60, 25 >>,
      << 255, 155, 54, 23 >>,
      << 255, 134, 48, 21 >>,
      << 255, 113, 42, 19 >>,
      << 255, 92, 36, 17 >>,
      << 255, 71, 30, 15 >>,
      << 255, 50, 24, 13 >>,
      << 255, 29, 18, 11 >>,
      << 255, 42, 21, 18 >>,
      << 255, 55, 25, 26 >>,
      << 255, 68, 28, 33 >>,
      << 255, 82, 32, 41 >>,
      << 255, 95, 35, 48 >>,
      << 255, 108, 39, 56 >>,
      << 255, 121, 42, 63 >>,
      << 255, 135, 46, 71 >>,
      << 255, 121, 43, 63 >>,
      << 255, 107, 41, 56 >>,
      << 255, 93, 38, 49 >>,
      << 255, 79, 36, 42 >>,
      << 255, 65, 34, 34 >>,
      << 255, 51, 31, 27 >>,
      << 255, 37, 29, 20 >>,
      << 255, 24, 27, 13 >>,
      << 255, 51, 52, 27 >>,
      << 255, 78, 77, 41 >>,
      << 255, 105, 103, 56 >>,
      << 255, 132, 128, 70 >>,
      << 255, 159, 153, 84 >>,
      << 255, 186, 179, 99 >>,
      << 255, 213, 204, 113 >>,
      << 255, 241, 230, 128 >>,
      << 255, 213, 205, 115 >>,
      << 255, 185, 180, 102 >>,
      << 255, 157, 155, 89 >>,
      << 255, 129, 130, 76 >>,
      << 255, 101, 105, 63 >>,
      << 255, 73, 80, 50 >>,
      << 255, 45, 55, 37 >>,
      << 255, 17, 31, 24 >>,
      << 255, 44, 47, 38 >>,
      << 255, 72, 63, 52 >>,
      << 255, 100, 80, 67 >>,
      << 255, 128, 96, 81 >>,
      << 255, 156, 112, 95 >>,
      << 255, 184, 129, 110 >>,
      << 255, 212, 145, 124 >>,
      << 255, 240, 162, 139 >>,
      << 255, 211, 142, 125 >>,
      << 255, 182, 122, 111 >>,
      << 255, 154, 102, 98 >>,
      << 255, 125, 83, 84 >>,
      << 255, 96, 63, 70 >>,
      << 255, 68, 43, 57 >>,
      << 255, 39, 23, 43 >>,
      << 255, 11, 4, 30 >>,
      << 255, 22, 14, 49 >>,
      << 255, 34, 24, 69 >>,
      << 255, 46, 35, 89 >>,
      << 255, 58, 45, 109 >>,
      << 255, 70, 55, 129 >>,
      << 255, 82, 66, 149 >>,
      << 255, 94, 76, 169 >>,
      << 255, 106, 87, 189 >>,
      << 255, 96, 78, 167 >>,
      << 255, 86, 70, 145 >>,
      << 255, 77, 62, 123 >>,
      << 255, 67, 54, 101 >>,
      << 255, 57, 45, 79 >>,
      << 255, 48, 37, 57 >>,
      << 255, 38, 29, 35 >>,
      << 255, 29, 21, 14 >>,
      << 255, 26, 35, 27 >>,
      << 255, 24, 50, 40 >>,
      << 255, 22, 65, 53 >>,
      << 255, 20, 80, 66 >>,
      << 255, 18, 95, 79 >>,
      << 255, 16, 110, 92 >>,
      << 255, 14, 125, 105 >>,
      << 255, 12, 140, 118 >>,
      << 255, 11, 123, 106 >>,
      << 255, 11, 106, 95 >>,
      << 255, 11, 89, 84 >>,
      << 255, 11, 73, 73 >>,
      << 255, 10, 56, 62 >>,
      << 255, 10, 39, 51 >>,
      << 255, 10, 22, 40 >>,
      << 255, 10, 6, 29 >>,
      << 255, 15, 23, 35 >>,
      << 255, 20, 40, 41 >>,
      << 255, 25, 57, 47 >>,
      << 255, 30, 75, 53 >>,
      << 255, 35, 92, 59 >>,
      << 255, 40, 109, 65 >>,
      << 255, 45, 126, 71 >>,
      << 255, 50, 144, 77 >>,
      << 255, 46, 126, 70 >>,
      << 255, 43, 108, 63 >>,
      << 255, 39, 90, 57 >>,
      << 255, 36, 72, 50 >>,
      << 255, 32, 54, 43 >>,
      << 255, 29, 36, 37 >>,
      << 255, 25, 18, 30 >>,
      << 255, 22, 0, 24 >>,
      << 255, 37, 23, 51 >>,
      << 255, 53, 47, 78 >>,
      << 255, 69, 70, 106 >>,
      << 255, 85, 94, 133 >>,
      << 255, 100, 117, 160 >>,
      << 255, 116, 141, 188 >>,
      << 255, 132, 164, 215 >>,
      << 255, 148, 188, 243 >>,
      << 255, 130, 168, 213 >>,
      << 255, 112, 149, 184 >>,
      << 255, 94, 129, 154 >>,
      << 255, 76, 110, 125 >>,
      << 255, 58, 90, 95 >>,
      << 255, 40, 71, 66 >>,
      << 255, 22, 51, 36 >>,
      << 255, 4, 32, 7 >>,
      << 255, 32, 46, 7 >>,
      << 255, 60, 60, 8 >>,
      << 255, 89, 74, 9 >>,
      << 255, 117, 89, 10 >>,
      << 255, 145, 103, 11 >>,
      << 255, 174, 117, 12 >>,
      << 255, 202, 131, 13 >>,
      << 255, 231, 146, 14 >>,
      << 255, 203, 129, 14 >>,
      << 255, 175, 112, 15 >>,
      << 255, 148, 96, 16 >>,
      << 255, 120, 79, 17 >>,
      << 255, 92, 62, 17 >>,
      << 255, 65, 46, 18 >>,
      << 255, 37, 29, 19 >>,
      << 255, 10, 13, 20 >>,
      << 255, 31, 29, 26 >>,
      << 255, 53, 46, 32 >>,
      << 255, 75, 63, 38 >>,
      << 255, 97, 80, 44 >>,
      << 255, 118, 96, 50 >>,
      << 255, 140, 113, 56 >>,
      << 255, 162, 130, 62 >>,
      << 255, 184, 147, 68 >>,
      << 255, 162, 132, 59 >>,
      << 255, 141, 117, 51 >>,
      << 255, 119, 102, 43 >>,
      << 255, 98, 87, 35 >>,
      << 255, 77, 72, 27 >>,
      << 255, 55, 57, 19 >>,
      << 255, 34, 42, 11 >>,
      << 255, 13, 28, 3 >>,
      << 255, 32, 55, 21 >>,
      << 255, 52, 83, 40 >>,
      << 255, 71, 110, 58 >>,
      << 255, 91, 138, 77 >>,
      << 255, 110, 165, 96 >>,
      << 255, 130, 193, 114 >>,
      << 255, 149, 220, 133 >>,
      << 255, 169, 248, 152 >>,
      << 255, 148, 217, 137 >>,
      << 255, 127, 186, 122 >>,
      << 255, 107, 155, 107 >>,
      << 255, 86, 124, 93 >>,
      << 255, 65, 93, 78 >>,
      << 255, 45, 62, 63 >>,
      << 255, 24, 31, 48 >>,
      << 255, 4, 0, 34 >>,
      << 255, 11, 10, 35 >>,
      << 255, 18, 20, 37 >>,
      << 255, 25, 31, 39 >>,
      << 255, 33, 41, 41 >>,
      << 255, 40, 51, 42 >>,
      << 255, 47, 62, 44 >>,
      << 255, 54, 72, 46 >>,
      << 255, 62, 83, 48 >>,
      << 255, 55, 75, 44 >>,
      << 255, 48, 67, 41 >>,
      << 255, 41, 59, 38 >>,
      << 255, 34, 52, 35 >>,
      << 255, 27, 44, 31 >>,
      << 255, 20, 36, 28 >>,
      << 255, 13, 28, 25 >>,
      << 255, 7, 21, 22 >>,
      << 255, 25, 30, 42 >>,
      << 255, 43, 40, 62 >>,
      << 255, 61, 49, 82 >>,
      << 255, 79, 59, 103 >>,
      << 255, 97, 68, 123 >>,
      << 255, 115, 78, 143 >>,
      << 255, 133, 87, 163 >>,
      << 255, 152, 97, 184 >>,
      << 255, 134, 85, 162 >>,
      << 255, 116, 73, 141 >>,
      << 255, 98, 61, 119 >>,
      << 255, 80, 50, 98 >>,
      << 255, 62, 38, 76 >>,
      << 255, 44, 26, 55 >>,
      << 255, 26, 14, 33 >>
    }.

%%
%% Pc : Z(n+1) -> Z(n)^2 + Z(0)
%% Z(0) = (X+Yi)
%% Z^2 + Z(0) = (A+Bi)^2 + (X+Yi) = ((A^2-B^2)+X) + (2AB+Y)i
%%
z2(Max,Cx,Cy) ->
    z2(Max,0,Cx,Cy,0.0,0.0,0.0,0.0).

z2(Max,K,Cx,Cy,A,B,A2,B2) ->
    if K >= Max ->
	    0;
       true ->
	    if (A2+B2) < 4.0 ->
		    R   = (A2-B2) + Cx,
		    I   = 2.0*A*B + Cy,
		    z2(Max,K+1,Cx,Cy,R,I,R*R,I*I);
	       true ->
		    K
	    end
    end.
