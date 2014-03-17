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
-define(Y_MENU,  50).
%%
%% render image A and image B and the operation result C
%% images should be 128x128 in size
-record(menu_item,
	{
	  text,
	  op,
	  pos,
	  dim,
	  rect
	}).

-record(color_item,
	{
	  pos,
	  color,
	  rect
	}).

-record(state,
	{
	  width  :: integer(),
	  height :: integer(),
	  window :: epx:epx_window(),
	  image_a, %% #epx_image{},
	  image_b, %% #epx_image{},
	  font   :: epx:epx_font(),
	  foreground_pixels :: epx:epx_pixmap(),
	  background_pixels :: epx:epx_pixmap(),
	  color_menu= []   :: [#color_item{}],
	  item_menu = []   :: [#menu_item{}], 
	  color_selected = 0 :: integer(),
	  item_selected = 0 :: integer()
	}).

start() ->
    Dir = filename:join(code:lib_dir(epx_demo),"images"),
    start(filename:join(Dir,"square_128x128.png"),
	  filename:join(Dir,"circle_128x128.png")).

start0() ->
    Dir = filename:join(code:lib_dir(epx_demo),"images"),
    start(filename:join(Dir,"red_128x128.png"),
	  filename:join(Dir,"blue_128x128.png")).

start(File1, File2) ->
    epx:start(),
    {ok,ImageA} = epx_image:load(File1),
    {ok,ImageB} = epx_image:load(File2),
    Format = argb,
    Width  = 128*3 + 2*?X_OFFS + 2*?X_SPACE,
    Height = 128 + 2*?Y_OFFS + ?Y_MENU + ?Y_OFFS,
    Window = epx:window_create(40, 40, Width, Height,
			       [button_press,button_release]),
    Fg = epx:pixmap_create(Width, Height, Format),
    Bg = epx:pixmap_create(Width, Height, Format),
    epx:window_attach(Window),
    epx:pixmap_attach(Bg),
    {ok,Font} = epx_font:match([{name,"Arial"},{size,10}]),
    S0 = #state { width = Width,
		  height = Height,
		  window = Window,
		  image_a = ImageA,
		  image_b = ImageB,
		  foreground_pixels = Fg,
		  background_pixels = Bg,
		  font = Font
		},
    Menu = define_menu(S0),
    Colors = define_color_menu(S0),
    S1 = S0#state { item_menu = Menu, color_menu = Colors },
    draw(S1),
    update_window(S1),
    loop(S1).

loop(S) ->
    receive
	{epx_event, Win, close} when Win =:= S#state.window ->
	    io:format("Got window1 close\n", []),
	    epx:pixmap_detach(S#state.background_pixels),
	    epx:window_detach(S#state.window),
	    ok;
	{epx_event, Win, {button_press,[left],{X,Y,_}}} 
	  when Win =:= S#state.window ->
	    case select_item_menu(X, Y, S#state.item_menu) of
		0 ->
		    case select_color_menu(X, Y, S#state.color_menu) of
			0 ->
			    loop(S);
			I ->
			    S1 = S#state { color_selected = I },
			    draw(S1),
			    update_window(S1),
			    loop(S1)
		    end;
		I ->
		    S1 = S#state { item_selected = I },
		    draw(S1),
		    update_window(S1),
		    loop(S1)
	    end;
	{epx_event, Win, {button_release,_Button,_Where}}
	  when Win =:= S#state.window ->
	    loop(S);
	Event ->
	    io:format("got ~p\n", [Event]),
	    loop(S)
    end.


draw(S) ->
    draw_images(S),
    draw_color_menu(S),
    draw_item_menu(S).
    

draw_images(S) ->
    epx:pixmap_fill(S#state.foreground_pixels, black),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(white),
    %% clear areas where we draw images
    X1 = ?X_OFFS,
    Y1 = ?Y_OFFS,
    epx:draw_rectangle(S#state.foreground_pixels, X1, Y1, 128, 128),
    X2 = X1 + 128 + ?X_SPACE,
    Y2 = ?Y_OFFS,
    epx:draw_rectangle(S#state.foreground_pixels, X2, Y2, 128, 128),
    X3 = X2 + 128 + ?X_SPACE,
    Y3 = ?Y_OFFS,
    epx:draw_rectangle(S#state.foreground_pixels, X3, Y3, 128, 128),
    
    [A] = epx_image:pixmaps(S#state.image_a),
    epx:pixmap_copy_area(A, S#state.foreground_pixels, 0, 0, X1, Y1, 128, 128,
			 [blend]),
    
    [B] = epx_image:pixmaps(S#state.image_b),
    epx:pixmap_copy_area(B, S#state.foreground_pixels, 0, 0, X2, Y2, 128, 128,
			 [blend]),

    if S#state.item_selected =:= 0 ->
	    ok;
       true ->
	    Item=lists:nth(S#state.item_selected, S#state.item_menu),
	    C = epx:pixmap_copy(B),
	    epx:pixmap_operation_area(A, C, Item#menu_item.op, 
				      0, 0, 0, 0, 128, 128),
	    epx:pixmap_copy_area(C, S#state.foreground_pixels, 0, 0, 
				 X3, Y3, 128, 128,
				 [blend])
    end.

draw_color_menu(S) ->
    lists:foreach(
      fun(#color_item{pos=Pos,color=Color,rect=Rect}) ->
	      epx_gc:set_fill_style(solid),
	      if Pos =:= S#state.color_selected ->
		      epx_gc:set_fill_color(white);
		 true ->
		      epx_gc:set_fill_color(Color)
	      end,
	      epx:draw_rectangle(S#state.foreground_pixels,Rect)
      end, S#state.color_menu).

draw_item_menu(S) ->
    lists:foreach(
      fun(#menu_item{text=Text,pos=Pos,dim={TxW,TxH},rect=Rect={Rx,Ry,Rw,Rh}}) ->
	      epx_gc:set_font(S#state.font),
	      epx_gc:set_fill_style(solid),
	      if Pos =:= S#state.item_selected ->
		      epx_gc:set_fill_color({16#0,16#0,16#ff});
		 true ->
		      epx_gc:set_fill_color({16#c0,16#c0,16#ff})
	      end,
	      epx:draw_rectangle(S#state.foreground_pixels,Rect),

	      epx_gc:set_foreground_color(16#00ffffff), %% white
	      Xd = (Rw - TxW) div 2,
	      Yd = (Rh - TxH) div 2,
	      X = Rx + Xd,
	      Y = Ry + Yd + epx:font_info(S#state.font, ascent),
	      epx:draw_string(S#state.foreground_pixels, X, Y, Text),
	      %% draw white border
	      epx_gc:set_foreground_color({16#ff,16#ff,16#ff}),
	      epx_gc:set_fill_style(none),
	      epx:draw_rectangle(S#state.foreground_pixels,Rect)
      end, S#state.item_menu).

update_window(S) ->    
    epx:pixmap_copy_to(S#state.foreground_pixels,S#state.background_pixels),
    epx:pixmap_draw(S#state.background_pixels, S#state.window,
		    0, 0, 0, 0, 
		    S#state.width, S#state.height).

%%
%% Find menu item
%%
select_item_menu(X, Y, [#menu_item{pos=Pos,rect={Rx,Ry,Rw,Rh}}|Menu]) ->
    if X >= Rx,Y >= Ry,X =< Rx+Rw-1,Y =< Ry+Rh-1 ->
	    Pos;
       true ->
	    select_item_menu(X,Y,Menu)
    end;
select_item_menu(_X, _Y, []) ->
    0.

%%
%% Find menu item
%%
select_color_menu(X, Y, [#color_item{pos=Pos,rect={Rx,Ry,Rw,Rh}}|Menu]) ->
    if X >= Rx,Y >= Ry,X =< Rx+Rw-1,Y =< Ry+Rh-1 ->
	    Pos;
       true ->
	    select_color_menu(X,Y,Menu)
    end;
select_color_menu(_X, _Y, []) ->
    0.

define_color_menu(S) ->
    N = 2,
    M = (1 bsl 2),
    W = S#state.width - 64*6,
    Rx = W div 2,
    Ry = ?Y_OFFS + 128 + 8,
    [#color_item { pos=I+1,
		   color=color_from_pos(N,I),
		   rect={Rx+6*I,Ry,6,8}} || I <- lists:seq(0, M*M*M-1)].

color_from_pos(N, I) ->
    M = (1 bsl N) - 1,
    <<R:N,G:N,B:N>> = <<I:(3*N)>>,
    {trunc(R*(255/M)), trunc(G*(255/M)), trunc(B*(255/M))}.
			        
%%
%% Define menu coordinates 
%%    
define_menu(S) ->    
    Ms = [
	  {"clear",clear},
	  {"src",src},
	  {"dst",dst},
	  {"src-over",src_over},
	  {"dst-over", dst_over},
	  {"src-in",src_in},
	  {"dst-in", dst_in},
	  {"src-out",src_out},
	  {"dst-out", dst_out},
	  {"src-atop",src_atop},
	  {"dst-atop", dst_atop},
	  {"src-blend", src_blend},
	  {"dst-blend", dst_blend},
	  {"xor", 'xor'},
	  {"copy", copy},
	  {"add",  add},
	  {"sub",  sub}
	 ],
    %% fill, fill_blend, copy0, sum, blend, shadow, shadow_blend,
    %% alpha, fade, color, color_blend
    Ms1 = lists:map(
	    fun({String,Op}) ->
		    {String,Op,epx_font:dimension(S#state.font, String)}
	    end, Ms),
    %% calculate item box
    MaxWidth  = lists:max(lists:map(fun({_,_,{W,_}}) -> W end, Ms1)),
    MaxHeight = lists:max(lists:map(fun({_,_,{_,H}}) -> H end, Ms1)),
    ItemWidth = MaxWidth   + 8,
    ItemHeight = MaxHeight + 4,
    MenuX     = ?X_OFFS,
    MenuY     = S#state.height - (?Y_MENU+?Y_OFFS),
    MenuWidth = S#state.width - 2*?X_OFFS,
    MenuSize = length(Ms),
    _TotPixels = MenuSize*ItemWidth,
    Cols      = MenuWidth div ItemWidth,
    _Rows     = (MenuSize+Cols-1) div Cols,
    position_menu(Ms1, 0, Cols, MenuX, MenuY, ItemWidth, ItemHeight).

position_menu([{Text,Op,Dim}|Menu],Pos,Cols,X,Y,W,H) ->
    C = Pos rem Cols,
    R = Pos div Cols,
    Mx = X + C*W+1,
    My = Y + R*H+1,
    Item = #menu_item { text=Text, op=Op, pos=Pos+1, dim=Dim,
			rect={Mx,My,W-2,H-2}	 },
    [Item | position_menu(Menu,Pos+1,Cols,X,Y,W,H)];
position_menu([],_Pos,_Cols,_X,_Y,_W,_H) ->
    [].

    
    
    
    
