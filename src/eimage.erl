%%% File    : eimage.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Image file interface
%%% Created : 28 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(eimage).

-compile(export_all).

-include_lib("epx/include/epx_image.hrl").

-define(WIDTH,  512).
-define(HEIGHT, 512).

-import(lists, [foldl/3, reverse/1, foreach/2, map/2]).

start([File]) when is_atom(File) ->
    start(File,[copy]);
start([File|Opts]) when is_atom(File) ->
    start(File,Opts);
start(File) when is_list(File), is_integer(hd(File)) ->
    start(File,[copy]).

start(File,Transform) ->
    spawn(?MODULE, show1, [File, Transform]).

image(IMG) -> image(IMG, [copy]).
image(IMG, Transform) when is_record(IMG, epx_image) ->
    spawn(?MODULE, show_img, [IMG, Transform]).
    

show1(File, Transform) ->
    epx:start(),
    case epx_image:load(File) of
	{ok,Image} ->
	    show_img(Image, Transform);
	Error ->
	    io:format("Image load error: ~p\n", [Error])
    end.

show_img(IMG, Transform) ->
    [Pixmap0|_] = IMG#epx_image.pixmaps,
    Width  = epx:pixmap_info(Pixmap0, width),
    Height = epx:pixmap_info(Pixmap0, height),
    Ratio = Height / Width,
    Width1 = ?WIDTH,
    Height1 = trunc(Width1 * Ratio),
    Window = epx:window_create(50, 50, Width1, Height1),
    Screen = epx:pixmap_create(Width, Height, argb),
    epx:window_attach(Window),
    epx:pixmap_attach(Screen),
    Sx = epx:pixmap_info(Screen, width) div 2,
    Sy = epx:pixmap_info(Screen, height) div 2,
    transform_loop(normalize(Transform), Pixmap0, Sx, Sy, Screen, Window).

%% command line style [rotate,'30',spin,'0','90'] => [{rotate,30},{spin,0,90}]
normalize([rotate,A|T]) -> [{rotate,number(A)} | normalize(T)];
normalize([spin,A,B|T]) -> [{spin,number(A),number(B)} | normalize(T)];
normalize([T|Ts]) -> [T | normalize(Ts)];
normalize([]) -> [].

number(A) when is_number(A) -> A;
number(A) when is_atom(A) -> number(atom_to_list(A));
number(A) when is_list(A) ->
    try list_to_integer(A) catch error:_ -> list_to_float(A) end.
	    
transform_loop(Transform, Pixmap, X, Y, Screen, Window) ->
    epx:pixmap_fill(Screen, {255,0,0,0}),
    transform(Transform, Pixmap, X, Y, Screen, Window),
    ScreenWidth  = epx:pixmap_info(Screen, width),
    ScreenHeight = epx:pixmap_info(Screen, height),
    epx:pixmap_draw(Screen, Window, 0, 0, 0, 0, ScreenWidth, ScreenHeight),
    case next_transform(Transform) of
	[] ->
	    receive
		{epx_event,Window,close} ->
		    epx:pixmap_detach(Screen),
		    epx:window_detach(Window)
	    end;
	Transform1 ->
	    receive after 10 -> ok end,
	    transform_loop(Transform1, Pixmap, X, Y, Screen, Window)
    end.

next_transform([{spin,From,From}|T]) -> T;
next_transform([{spin,From,To}|T]) when From < To -> [{spin,From+1,To}|T];
next_transform([_|T]) -> T;
next_transform([]) -> [].

transform([], _Pixmap, _X, _Y, _Screen, _Window) ->
    ok;
transform([copy|_], Pixmap, X, Y, Screen, Window) ->
    Dw = epx:window_info(Window, width),
    Dh = epx:window_info(Window, height),

    W  = epx:pixmap_info(Pixmap, width),
    H = epx:pixmap_info(Pixmap, height),

    R = H / W,
    Width1 = ?WIDTH,
    Height1 = trunc(Width1 * R),

    Cx = X - (W div 2),
    Cy = Y - (H div 2),
    epx:pixmap_scale_area(Pixmap, Screen, Cx, Cy, Width1, Height1);
%% epx:pixmap_copy_area(Pixmap, Screen, 0, 0, Cx, Cy, Pw, Ph, []);
transform([lightmap|_], Pixmap, X, Y, Screen, _Window) ->
    %% calculate pixmap to light map ARGB => A
    Width  = epx:pixmap_info(Pixmap, width),
    Height = epx:pixmap_info(Pixmap, height),    
    Lmap = epx:pixmap_create(Width, Height, l8),
    epx:pixmap_copy_to(Pixmap, Lmap),
    LmapData = epx:pixmap_get_pixels(Lmap, 0, 0, Width, Height),
    %% the image may be rgb, l8 etc, convert to argb before unpacking
    Argb = epx:pixmap_create(Width, Height, argb),
    epx:pixmap_copy_to(Pixmap, Argb),
    ArgbData = epx:pixmap_get_pixels(Argb, 0, 0, Width, Height),
    ZRGB = << <<0,R,G,B>> || <<_A,R,G,B>> <= ArgbData >>,
    Pixmap2 = epx:pixmap_create(Width, Height, argb),
    Cx = X - (Width div 2),
    Cy = Y - (Height div 2),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color({0,255,0}),
    epx:draw_rectangle(Screen,Cx,Cy,Width,Height),
    epx:pixmap_put_pixels(Pixmap2, 0, 0, Width, Height, argb, ZRGB, []),
    epx:pixmap_put_pixels(Pixmap2, 0, 0, Width, Height, a8, LmapData, [sum]),
    epx:pixmap_copy_area(Pixmap2, Screen, 0, 0, Cx, Cy, Width, Height, [blend]);
transform([vlines|_], Pixmap, _X, _Y, Screen, _Window) ->
    Width  = epx:pixmap_info(Pixmap, width),
    Height = epx:pixmap_info(Pixmap, height),
    Dst1 = epx:pixmap_create(Width, Height, l8),
    extract_vlines(Pixmap, Width, Height, Dst1),
    epx:pixmap_copy_to(Dst1, Screen);
transform([hlines|_], Pixmap, _X, _Y, Screen, _Window) ->
    Width  = epx:pixmap_info(Pixmap, width),
    Height = epx:pixmap_info(Pixmap, height),
    Dst2 = epx:pixmap_create(Width, Height, l8),
    extract_hlines(Pixmap, Width, Height, Dst2),
    epx:pixmap_copy_to(Dst2, Screen);

transform([{spin,Angle,_EndAngle}|T],Pixmap, X, Y, Screen, Window) ->
    transform([{rotate,Angle}|T],Pixmap, X, Y, Screen, Window);
transform([{rotate,Angle}|_], Pixmap, X, Y, Screen, _Window) ->
    Width  = epx:pixmap_info(Pixmap, width),
    Height = epx:pixmap_info(Pixmap, height),
    Ar = Angle*(2*math:pi() / 360),
    %% rotate the image around its center and place it at (X,Y)
    epx:pixmap_rotate_area(Pixmap, Screen, Ar, 0, 0,
			   Width div 2, Height div 2, X, Y,
			   Width, Height, [blend]).


extract_vlines(Src, Width, Height, Dst) ->
    extract_vy(Src, 0, Width, Height, Dst).

extract_vy(Src, Xi, Width, Height, Dst) ->
    if Xi < Width ->
	    extract_vx(Src, Xi, 0, Width, Height, Dst),
	    extract_vy(Src, Xi+1, Width, Height, Dst);
       true ->
	    ok
    end.

extract_vx(Src, Xi, Yj, Width, Height, Dst) ->
    Color = epx:pixmap_get_pixel(Src, Xi, Yj),
    epx:pixmap_put_pixel(Dst, Xi, Yj, 0),
    extract_vx(Src, Xi, Yj+1, Width, Height, Dst, Yj, Color).

extract_vx(Src, Xi, Yj, Width, Height, Dst, Y0,Color) ->
    if Yj < Height ->
	    Color1 = epx:pixmap_get_pixel(Src, Xi, Yj),
	    Dist = color_distance(Color, Color1),
	    epx:pixmap_put_pixel(Dst, Xi, Yj, 0),
	    if Dist < 3 ->
		    extract_vx(Src, Xi, Yj+1, Width, Height, Dst, Y0, Color);
	       (Yj-Y0) < 5 ->
		    extract_vx(Src, Xi, Yj+1, Width, Height, Dst, Yj, Color1);
	       true ->
		    Y = (Yj+Y0) div 2,
		    epx:pixmap_put_pixel(Dst, Xi, Y, 16#ffffff),
		    extract_vx(Src, Xi, Yj+1, Width, Height, Dst,
			       Yj, Color1)
	    end;
       true ->
	    Y = (Yj+Y0) div 2,
	    epx:pixmap_put_pixel(Dst, Xi, Y, 16#ffffff)
    end.

extract_hlines(Src, Width, Height, Dst) ->
    extract_hx(Src, 0, Width, Height, Dst).

extract_hx(Src, Yj, Width, Height, Dst) ->
    if Yj < Height ->
	    extract_hy(Src, 0, Yj, Width, Height, Dst),
	    extract_hx(Src, Yj+1, Width, Height, Dst);
       true ->
	    ok
    end.

extract_hy(Src, Xi, Yj, Width, Height, Dst) ->
    Color = epx:pixmap_get_pixel(Src, Xi, Yj),
    epx:pixmap_put_pixel(Dst, Xi, Yj, 0),
    extract_hy(Src, Xi+1, Yj, Width, Height, Dst, Xi, Color).

extract_hy(Src, Xi, Yj, Width, Height, Dst, X0,Color) ->
    if Xi < Height ->
	    Color1 = epx:pixmap_get_pixel(Src, Xi, Yj),
	    Dist = color_distance(Color, Color1),
	    epx:pixmap_put_pixel(Dst, Xi, Yj, 0),
	    if Dist < 3 -> %% "same" color keep
		    extract_hy(Src, Xi+1, Yj, Width, Height, Dst,
			       X0, Color);
	       (Xi-X0) < 5 -> %% ignore to short
		    extract_hy(Src, Xi+1, Yj, Width, Height, Dst,
			       Xi, Color1);
	       true ->
		    X = (Xi+X0) div 2,
		    epx:pixmap_put_pixel(Dst, X, Yj, 16#ffffff),
		    extract_hy(Src, Xi+1, Yj, Width, Height, Dst,
			       Xi, Color1)
	    end;
       true ->
	    X = (Xi+X0) div 2,
	    epx:pixmap_put_pixel(Dst, X, Yj, 16#ffffff)
    end.

color_distance({_A1,R1,G1,B1},{_A2,R2,G2,B2}) ->
    Dr = (R2-R1),
    Dg = (G2-G1),
    Db = (B2-B1),
    math:sqrt(Dr*Dr + Dg*Dg + Db*Db).
