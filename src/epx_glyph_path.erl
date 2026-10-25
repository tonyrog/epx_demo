%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2026, Tony Rogvall
%%% @doc
%%%    Draw glyphs tracing a path (see epx/doc/TEXT.md)
%%%
%%%    start/0 runs text around an ellipse, below it every glyph of a
%%%    string is placed by a fun (epx_text:draw_glyphs/6) as a wave.
%%%    Press q or close the window to stop.
%%% @end
%%% Created : 24 Oct 2026 by Tony Rogvall <tony@rogvall.se>

-module(epx_glyph_path).

-export([start/0, start/2]).

-define(W, 640).
-define(H, 480).

%% path: T in 0..1 -> {X,Y}
ellipse(T) ->
    A = T*math:pi()*2 + math:pi(),
    {?W/2+(?W/3)*math:cos(A), ?H/2.6+(?H/4)*math:sin(A)}.

start() ->
    start("times", 40).

start(FontName, Size) ->
    epx:start(),
    Sft = epx_text:open(FontName, Size),
    Window = epx:window_create(50,50,?W,?H,[button_press,key_press]),
    epx:window_attach(Window),
    Pixels = epx:pixmap_create(?W,?H,argb),
    epx:pixmap_attach(Pixels),
    Gc = epx:gc_create(),
    Table = epx_text:path_table(fun ellipse/1, 256),
    Length = epx_text:path_length(Table),
    loop(Window, Pixels, Gc, Sft, Table, Length, 0.0),
    epx:pixmap_detach(Pixels),
    epx:window_detach(Window),
    ok.

loop(Window, Pixels, Gc, Sft, Table, Length, Offset) ->
    epx:pixmap_fill(Pixels, {255,20,30,90}),
    draw_path_line(Pixels, Gc, Table),
    %% text running along the ellipse, the offset moves it
    epx:gc_set(Gc, foreground_color, {0,255,255,255}),
    Text = "Hello world, round and round we go...",
    epx_text:draw_path(Pixels, Gc, Sft, Text, fun ellipse/1,
		       [{offset, Offset}, {baseline, 6}]),
    %% the part of the text that has wrapped past the end of the path
    epx_text:draw_path(Pixels, Gc, Sft, Text, fun ellipse/1,
		       [{offset, Offset - Length}, {baseline, 6}]),
    %% glyphs placed by a fun: a wave with growing glyphs
    epx:gc_set(Gc, foreground_color, {0,255,220,120}),
    Phase = Offset/40,
    epx_text:draw_glyphs(
      Pixels, Gc, Sft, "draw_glyphs places each glyph",
      fun(#{ glyph := G, pen := Pen, advance := Adv, width := Width }) ->
	      Xc = (?W - Width)/2 + Pen + Adv/2,
	      Y = 420 + 14*math:sin(Pen/30 + Phase),
	      Angle = 0.4*math:cos(Pen/30 + Phase),
	      Scale = 0.6 + 0.4*(Pen/Width),
	      %% the middle of the advance on the baseline is put at (Xc,Y)
	      epx_text:glyph_transform(Sft, G,
				       [{x,Xc},{y,Y},{rotate,Angle},
					{scale,Scale},
					{anchor,baseline_center}])
      end, []),
    epx:pixmap_draw(Pixels, Window, 0, 0, 0, 0, ?W, ?H),
    epx:sync(Window),
    receive
	{epx_event,Window,close} -> ok;
	{epx_event,Window,{key_press,$q,_,_}} -> ok;
	{epx_event,Window,_} ->
	    loop(Window, Pixels, Gc, Sft, Table, Length, Offset)
    after 30 ->
	    loop(Window, Pixels, Gc, Sft, Table, Length,
		 fmod(Offset + 2, Length))
    end.

fmod(A, B) when A >= B -> A - B;
fmod(A, _B) -> A.

draw_path_line(Pixels, Gc, Table) ->
    epx:gc_set(Gc, foreground_color, {255,80,90,160}),
    Pts = [{X,Y} || {_S,X,Y} <- tuple_to_list(Table)],
    lists:foldl(fun({X,Y}, {X0,Y0}) ->
			epx:pixmap_draw_line(Pixels, Gc, X0, Y0, X, Y),
			{X,Y}
		end, hd(Pts), tl(Pts)).
