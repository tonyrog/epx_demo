%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Move an image down in the Z plane using two eye system
%%% @end
%%% Created :  7 Jul 2013 by Tony Rogvall <tony@rogvall.se>

-module(epx_zimage).

-compile(export_all).

-include_lib("epx/include/epx_image.hrl").

-import(lists, [foldl/3, reverse/1, foreach/2, map/2]).

-record(opt,
	{
	  left  = {0.0, 0.0, -10.0},
	  right = {0.0, 0.0, -10.0},
	  pixels,            %% the pixmap
	  image_width  = 0,  %% define by image
	  image_height = 0,  %% define by image
	  image_xoffs,
	  image_yoffs,
	  image_z =  1.0,        %% image plane (image size)
	  screen_z = 0.0,        %% screen plane (window size)
	  screen_width  = 640,
	  screen_height = 480,
	  screen_xoffs  = 320,
	  screen_yoffs  = 240
	}).

%%
%% Example:
%%
%%  f(Ez), Ez=-1.0, epx_zimage:show("/Users/tony/Pictures/Tony/Tony.png", [{left,{-200,0,Ez}},{right,{200,0,Ez}}, {image_z, 1}]).
%%
%%
show(File) ->
    show(File, []).


show(File, Opts) ->
    epx:start(),
    case epx_image:load(File) of
	{ok,Image} ->
	    [Pixels|_] =  Image#epx_image.pixmaps,
	    Opts1 = set_opts(Opts, #opt{ pixels = Pixels }),
	    W0 = epx:pixmap_info(Pixels, width),
	    H0 = epx:pixmap_info(Pixels, height),
	    Opts2 = if Opts1#opt.image_width =:= 0,
		       Opts1#opt.image_height =:= 0 ->
			    Opts1#opt { image_width = W0,
					image_height = H0 };
		       Opts1#opt.image_width =:= 0 -> 
			    %% fixme: scale height!
			    Opts1#opt { image_width = W0 };
		       Opts1#opt.image_height =:= 0 -> 
			    %% fixme: scale width!
			    Opts1#opt { image_height = H0 }
		    end,
	    spawn(fun() -> init_(Opts2) end);
	Error ->
	    Error
    end.


init_(Opts) ->
    Window = epx:window_create(50, 50, 
			       Opts#opt.screen_width, 
			       Opts#opt.screen_height),
    Screen = epx:pixmap_create(Opts#opt.screen_width, 
			       Opts#opt.screen_height,
			       argb),
    epx:window_attach(Window),
    epx:pixmap_attach(Screen),
    render_pixels(Opts#opt.pixels, Opts, Screen),
    redraw_window(Screen, Opts, Window),
    loop(Screen, Window).

render_pixels(Pixels, Opts, Screen) ->
    epx:pixmap_fill(Screen, {255,0,0,0}),
    %% go trough every pixel in Pixels and 
    %% render them onto screen using sum flag
    Pw = epx:pixmap_info(Pixels, width),
    Ph = epx:pixmap_info(Pixels, height),
    Sw = epx:pixmap_info(Screen, width),
    Sh = epx:pixmap_info(Screen, height),
    Opts1 = Opts#opt { screen_xoffs = Sw div 2,
		       screen_yoffs = Sh div 2,
		       image_xoffs = Pw div 2,
		       image_yoffs = Ph div 2 },
    render_rows(0, Ph, Pw, Opts1, Pixels, Screen).

render_rows(Y, YMax, Width, Opts, Pixels, Screen) ->
    if  Y>=YMax -> 
	    ok;
	true ->
	    render_row(0, Width, Y, Opts, Pixels, Screen),
	    render_rows(Y+1, YMax, Width, Opts, Pixels, Screen)
    end.

render_row(X, XMax, Y, Opts, Pixels, Screen) ->
    if X >= XMax ->
	    true;
       true ->
	    Pix = epx:pixmap_get_pixel(Pixels, X, Y),
	    Px = X - Opts#opt.image_xoffs,
	    Py = Y - Opts#opt.image_yoffs,
	    Pz = Opts#opt.image_z,
	    %% map P=(Px,Py,Pz) to screen S=(Sx,Sy,Sz) to the eyes 
	    %% L=(Lx,Ly,Lz) and R=(Rx,Ry,Rz)
	    Sz = Opts#opt.screen_z,
	    {Lx,Ly,Lz} = Opts#opt.left,
	    Lt = (Sz - Pz) / (Lz - Pz),
	    Sx1 = trunc(Px + Lt*(Lx - Px) + Opts#opt.screen_xoffs),
	    Sy1 = trunc(Py + Lt*(Ly - Py) + Opts#opt.screen_yoffs),
	    Lpix = epx:pixmap_get_pixel(Screen, Sx1, Sy1),
	    Lpix2 = sum(Pix,Lpix),
	    epx:pixmap_put_pixel(Screen,Sx1,Sy1,Lpix2),
	    
	    {Rx,Ry,Rz} = Opts#opt.right,
	    Rt = (Sz - Pz) / (Rz - Pz),
	    Sx2 = trunc(Px + Rt*(Rx - Px) + Opts#opt.screen_xoffs),
	    Sy2 = trunc(Py + Rt*(Ry - Py) + Opts#opt.screen_yoffs),
	    Rpix = epx:pixmap_get_pixel(Screen, Sx2, Sy2),
	    Rpix2 = sum(Pix,Rpix),
	    epx:pixmap_put_pixel(Screen,Sx2,Sy2,Rpix2),

	    render_row(X+1,XMax,Y,Opts,Pixels,Screen)
    end.

sum({_A0,R0,G0,B0}, {_A1,R1,G1,B1}) ->
    R = max(R0, (R0+R1) div 2),
    G = max(G0, (G0+G1) div 2),
    B = max(B0, (B0+B1) div 2),
    {255, R, G, B }.


redraw_window(Screen, Opts, Window) ->
    epx:pixmap_draw(Screen, Window, 0, 0, 0, 0, 
		    Opts#opt.screen_width, Opts#opt.screen_height).

loop(Screen,Window) ->
    receive
	{epx_event,Window,close} ->
	    epx:pixmap_detach(Screen),
	    epx:window_detach(Window);
	Other ->
	    io:format("other: ~p\n", [Other]),
	    loop(Screen,Window)
    end.

set_opts([{Key,Value}|T], Opts) ->
    case Key of
	left_x ->
	    {_,Y,Z} = Opts#opt.left,
	    set_opts(T, Opts#opt { left = {float(Value),Y,Z}});
	left_y ->
	    {X,_,Z} = Opts#opt.left,
	    set_opts(T, Opts#opt { left = {X,float(Value),Z}});
	left_z ->
	    {X,Y,_} = Opts#opt.left,
	    set_opts(T, Opts#opt { left = {X,Y,float(Value)}});
	left ->
	    set_opts(T, Opts#opt { left = coord(Opts#opt.left,Value)});

	right_x ->
	    {_,Y,Z} = Opts#opt.right,
	    set_opts(T, Opts#opt { right = {float(Value),Y,Z}});
	right_y ->
	    {X,_,Z} = Opts#opt.right,
	    set_opts(T, Opts#opt { right = {X,float(Value),Z}});
	right_z ->
	    {X,Y,_} = Opts#opt.right,
	    set_opts(T, Opts#opt { right = {X,Y,float(Value)}});
	right -> 
	    set_opts(T, Opts#opt { right = coord(Opts#opt.right,Value) });

	image_width -> 
	    set_opts(T, Opts#opt { image_width = Value });
	image_height ->
	    set_opts(T, Opts#opt { image_height = Value });
	image_z ->  
	    set_opts(T, Opts#opt { image_z = Value });
	screen_width -> 
	    set_opts(T, Opts#opt { screen_width = Value });
	screen_height -> 
	    set_opts(T, Opts#opt { screen_height = Value });
	screen_z ->  
	    set_opts(T, Opts#opt { screen_z = Value })
    end;
set_opts([], Opts) ->
    Opts.


coord({_X0,_Y0,Z0},{X1,Y1}) when is_number(X1),is_number(Y1) ->
    {float(X1),float(Y1),Z0};
coord({X0,Y0,_Z0},Z) when is_number(Z) ->
    {X0,Y0,float(Z)};
coord(_,{X1,Y1,Z1}) when is_number(X1),is_number(Y1), is_number(Z1) ->
    {float(X1),float(Y1),float(Z1)}.
