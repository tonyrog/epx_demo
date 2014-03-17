%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Pixmap operation view
%%% @end
%%% Created : 17 Mar 2014 by Tony Rogvall <tony@rogvall.se>

-module(picop_view).

-compile(export_all).

-define(X_OFFS,  20).
-define(Y_OFFS,  20).
-define(X_SPACE, 20).
%%
%% render image A and image B and the operation result C
%% images should be 128x128 in size
-record(state,
	{
	  width,
	  height,
	  window,
	  background_pixels
	}).

start(File1, File2, Op) ->
    epx:start(),
    {ok,Image1} = epx_image:load(File1),
    {ok,Image2} = epx_image:load(File2),
    Format = argb,
    Width  = 128*3 + 2*?X_OFFS + 2*?X_SPACE,
    Height = 128 + 2*?Y_OFFS,
    Window = epx:window_create(40, 40, Width, Height),
%%  ForegroundPx = epx:pixmap_create(Width, Height, Format),
    BackgroundPx = epx:pixmap_create(Width, Height, Format),
    epx:window_attach(Window),
    epx:pixmap_attach(BackgroundPx),
    S0 = #state { width = Width,
		  height = Height,
		  window = Window, 
		  background_pixels = BackgroundPx },
    draw_images(Image1, Image2, Op, S0),
    update_window(S0),
    loop(S0).



draw_images(Image1, Image2, Op, S) ->
    epx:pixmap_fill(S#state.background_pixels, black),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(white),
    %% clear areas where we draw images
    X1 = ?X_OFFS,
    Y1 = ?Y_OFFS,
    epx:draw_rectangle(S#state.background_pixels, X1, Y1, 128, 128),
    X2 = X1 + 128 + ?X_SPACE,
    Y2 = ?Y_OFFS,
    epx:draw_rectangle(S#state.background_pixels, X2, Y2, 128, 128),
    X3 = X2 + 128 + ?X_SPACE,
    Y3 = ?Y_OFFS,
    epx:draw_rectangle(S#state.background_pixels, X3, Y3, 128, 128),
    
    [A] = epx_image:pixmaps(Image1),
    io:format("A = ~p\n", [A]),
    epx:pixmap_copy_area(A, S#state.background_pixels, 0, 0, X1, Y1, 128, 128),
    
    [B] = epx_image:pixmaps(Image2),
    io:format("B = ~p\n", [B]),
    epx:pixmap_copy_area(B, S#state.background_pixels, 0, 0, X2, Y2, 128, 128),

    C = epx:pixmap_copy(B),
    epx:pixmap_operation_area(A, C, Op, 0, 0, 0, 0, 128, 128),
    
    io:format("C = ~p\n", [C]),
    epx:pixmap_copy_area(C, S#state.background_pixels, 0, 0, X3, Y3, 128, 128),
    ok.

update_window(S) ->    
    epx:pixmap_draw(S#state.background_pixels, S#state.window,
		    0, 0, 0, 0, 
		    S#state.width, S#state.height).


loop(S) ->
    receive
	{epx_event, Win, close} when Win =:= S#state.window ->
	    io:format("Got window1 close\n", []),
	    epx:pixmap_detach(S#state.background_pixels),
	    epx:window_detach(S#state.window),
	    ok
    end.



    
    
    
    
    
