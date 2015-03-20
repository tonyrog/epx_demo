%%% File    : eimage.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Image file interface
%%% Created : 28 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(eimage).

-compile(export_all).

-include_lib("epx/include/epx_image.hrl").

-import(lists, [foldl/3, reverse/1, foreach/2, map/2]).

show(File) ->
    show(File,[copy]).

show(File,Transform) ->
    spawn(?MODULE, show1, [File, Transform]).

show1(File, Transform) ->
    epx:start(),
    case epx_image:load(File) of
	{ok,Image} ->
	    Width = Image#epx_image.width,
	    Height = Image#epx_image.height,
	    Diag = trunc(math:sqrt(Width*Width + Height*Height)+4.5),
	    BgW = Diag,
	    BgH = Diag,
	    X = Diag div 2,
	    Y = Diag div 2,
	    Win = epx:window_create(50, 50, BgW, BgH),
	    Bg  = epx:pixmap_create(BgW, BgH, argb),
	    epx:window_attach(Win),
	    epx:pixmap_attach(Bg),
	    [Pixmap|_] = Image#epx_image.pixmaps,
	    transform_loop(Transform, Pixmap, X, Y, Width, Height, 
			   Bg, BgW, BgH, Win);
	Error ->
	    io:format("Image load error: ~p\n", [Error])
    end.
	    
transform_loop(Transform, Pixmap, X, Y, Width, Height, Bg, BgW, BgH, Win) ->
    epx:pixmap_fill(Bg, {255,0,0,0}),
    transform(Transform, Pixmap, X, Y, Width, Height, Bg),
    epx:pixmap_draw(Bg, Win, 0, 0, 0, 0, BgW, BgH),
    case next_transform(Transform) of
	[] ->
	    receive
		{epx_event,Win,close} ->
		    epx:pixmap_detach(Bg),
		    epx:window_detach(Win)
	    end;
	Transform1 ->
	    receive after 10 -> ok end,
	    transform_loop(Transform1, Pixmap, X, Y, Width, Height, 
			   Bg, BgW, BgH, Win)
    end.

next_transform([{spin,From,From}|T]) -> T;
next_transform([{spin,From,To}|T]) when From < To -> [{spin,From+1,To}|T];
next_transform([_|T]) -> T;
next_transform([]) -> [].

transform([], _Pixmap, _X, _Y, _Width, _Height, _Bg) ->
    ok;
transform([copy|_], Pixmap, X, Y, Width, Height, Bg) ->
    Cx = X - (Width div 2), 
    Cy = Y - (Height div 2),
    epx:pixmap_copy_area(Pixmap, Bg, 0, 0, Cx, Cy, Width, Height, []);
transform([lightmap|_], Pixmap, X, Y, Width, Height, Bg) ->
    %% calculate pixmap to light map ARGB => A
    Lmap = epx:pixmap_create(Width, Height, l8),
    epx:pixmap_copy_to(Pixmap, Lmap),
    LmapData = epx:pixmap_get_pixels(Lmap, 0, 0, Width, Height),
    ArgbData = epx:pixmap_get_pixels(Pixmap, 0, 0, Width, Height),
    ZRGB = << <<0,R,G,B>> || <<_A,R,G,B>> <= ArgbData >>,
    Pixmap2 = epx:pixmap_create(Width, Height, argb),
    Cx = X - (Width div 2),
    Cy = Y - (Height div 2),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color({0,255,0}),
    epx:pixmap_draw_rectangle(Bg, epx_gc:current(),Cx,Cy,Width,Height),
    epx:pixmap_put_pixels(Pixmap2, 0, 0, Width, Height, argb, ZRGB, []),
    epx:pixmap_put_pixels(Pixmap2, 0, 0, Width, Height, a8, LmapData, [sum]),
    epx:pixmap_copy_area(Pixmap2, Bg, 0, 0, Cx, Cy, Width, Height, [blend]);

transform([{spin,Angle,_EndAngle}|T],Pixmap, X, Y, Width, Height, Bg)  ->
    transform([{rotate,Angle}|T],Pixmap, X, Y, Width, Height, Bg);
transform([{rotate,Angle}|_], Pixmap, X, Y, Width, Height, Bg) ->
    XOffs = (Width div 2),
    X0 =  -XOffs,
    X1 = X0 + Width - 1,
    YOffs = (Height div 2),
    Y0 = -YOffs,
    Y1 = Y0 + Height - 1,
    Ai = trunc(Angle),
    Ad = (Ai rem 360) + (Angle - Ai),
    Ar = Ad*(2*math:pi() / 360),
    CosA = math:cos(Ar),
    SinA = math:sin(Ar),
    rotate(Pixmap, X0, Y0, X1, Y1, X, Y, XOffs, YOffs, CosA, SinA, Bg).


rotate(_Pixmap, _X0, Y0, _X1, Y1, _Xc, _Yc, _XOffs, _YOffs, _CosA, _SinA, _Bg) when Y0>Y1->
    ok;
rotate(Pixmap, X0, Y0, X1, Y1, Xc, Yc, XOffs, YOffs, CosA, SinA, Bg) ->
    Px = trunc(X0*CosA - Y0*SinA + Xc),
    Py = trunc(Y0*CosA + X0*SinA + Yc),

    Qx = trunc(X1*CosA - Y0*SinA + Xc),
    Qy = trunc(Y0*CosA + X1*SinA + Yc),
    Tx0 = X0 + XOffs,
    Tx1 = X1 + XOffs,
    Ty = float(Y0+YOffs),
    %% io:format("Ty = ~w\n", [Ty]),
    epx:pixmap_tex_line(Bg, Px, Py, Qx, Qy, Pixmap, Tx0, Tx1, Ty),
    rotate(Pixmap, X0, Y0+1, X1, Y1, Xc, Yc, XOffs, YOffs, CosA, SinA, Bg).

