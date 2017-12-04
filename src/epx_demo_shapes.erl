%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Demonstrate basic drawing elements
%%% @end
%%% Created : 15 Nov 2015 by Tony Rogvall <tony@rogvall.se>

-module(epx_demo_shapes).

-compile(export_all).

start() ->
    start(1, fun draw_shapes/1, 640, 480).

draw_shapes(Pixmap) ->
    X0 = 10,
    Y0 = 10,
    H = 30, Hf = 10,
    W = 100, Wf = 10,
    epx_gc:draw(fun() ->
			epx_gc:set_fill_style(none),
			epx_gc:set_border_color(orange),
			epx:draw_rectangle(Pixmap, X0, Y0+0*(H+Hf), W, H)
		end),
    epx_gc:draw(fun() ->
			epx_gc:set_fill_style(solid),
			epx_gc:set_fill_color(green),
			epx:draw_rectangle(Pixmap, X0, Y0+1*(H+Hf), W, H)
		end),
    epx_gc:draw(fun() ->
			epx_gc:set_fill_style(solid),
			epx_gc:set_border_color(orange),
			epx_gc:set_border_width(3),
			epx_gc:set_fill_color(green),
			epx:draw_rectangle(Pixmap, X0, Y0+2*(H+Hf), W, H)
		end),
    epx_gc:draw(fun() ->
			epx_gc:set_fill_style(blend),
			epx_gc:set_border_color(orange),
			epx_gc:set_border_width(3),
			epx_gc:set_fill_color(green),
			epx:draw_rectangle(Pixmap, X0, Y0+3*(H+Hf), W, H)
		end),

    X1 = X0+W+Wf,

    epx_gc:draw(fun() ->
			epx_gc:set_fill_style(none),
			epx_gc:set_border_color(orange),
			epx:draw_roundrect(Pixmap, X1, Y0+0*(H+Hf), W, H, 8, 8)
		end),
    epx_gc:draw(fun() ->
			epx_gc:set_fill_style(solid),
			epx_gc:set_fill_color(green),
			epx:draw_roundrect(Pixmap, X1, Y0+1*(H+Hf), W, H, 8, 8)
		end),
    epx_gc:draw(fun() ->
			epx_gc:set_fill_style(solid),
			epx_gc:set_border_color(orange),
			epx_gc:set_border_width(3),
			epx_gc:set_fill_color(green),
			epx:draw_roundrect(Pixmap, X1, Y0+2*(H+Hf), W, H, 8, 8)
		end),
    epx_gc:draw(fun() ->
			epx_gc:set_fill_style(blend),
			epx_gc:set_border_color(orange),
			epx_gc:set_border_width(3),
			epx_gc:set_fill_color(green),
			epx:draw_roundrect(Pixmap, X1, Y0+3*(H+Hf), W, H, 8, 8)
		end),
    ok.


start(N, Func, W, H) ->
    epx:start(),
    Win = epx:window_create(50,50,W,H,[button_press,key_press]),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(W,H,argb),
    epx:pixmap_fill(Pix, white),
    epx:pixmap_attach(Pix),
    loop(N, Func, Win, Pix, W, H),
    receive
	{epx_event,Win, close} ->
	    epx:window_detach(Win),
	    epx:pixmap_detach(Pix),
	    ok
    end.

loop(0, _Func, _Win, _Pix, _W, _H) ->
    ok;
loop(I, Func, Win, Pix, W, H) ->
    Func(Pix),
    epx:pixmap_draw(Pix,Win,0,0,0,0,W, H),
    epx:sync(Pix,Win),
    loop(I-1, Func, Win, Pix, W, H).
