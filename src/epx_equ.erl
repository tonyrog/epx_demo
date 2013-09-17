%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    EPX interactive linear equation solver (kids education)
%%% @end
%%% Created : 24 Oct 2012 by Tony Rogvall <tony@rogvall.se>

-module(epx_equ).

-export([start/1]).
-compile(export_all).
-import(lists, [reverse/1]).

%% test case:  4*(((2y*x*y) - (6y^2 + x + 2*2))/2)
-record(leaf,
	{
	  rect :: epx:epx_rect(),         %% bounding rect
	  selected = false :: boolean(),  %% leaf selected
	  string :: string(),             %% string representation
	  value :: integer()|atom(),      %% 'x','y',1,2,....
	  size = 1 :: integer()           %% font size (index)
	}).

-record(node2,
	{
	  rect :: epx:epx_rect(),          %% bounding rect
	  lprect :: epx:epx_rect(),        %% left parentheses
	  rprect :: epx:epx_rect(),        %% right parentheses
	  selected = false :: boolean(),   %% expression selected
	  string :: string(),              %% string representation
	  operator :: atom(),              %% '+','-','*','/','^'
	  orect :: epx:epx_rect(),         %% position and bound of operator
	  lexpr :: expr(),                 %% left 
	  rexpr :: expr(),                 %% right
	  size = 1 :: integer()            %% font size (index)
	}).

-record(node1,
	{
	  rect :: epx:epx_rect(),          %% bounding rect
	  lprect :: epx:epx_rect(),        %% left parentheses
	  rprect :: epx:epx_rect(),        %% right parentheses
	  selected = false :: boolean(),   %% expression selected
	  string :: string(),              %% string representation
	  operator :: atom(),              %% '+','-'
	  orect :: epx:epx_rect(),         %% position and bound of operator
	  expr :: expr(),                  %% expression
	  size = 1 :: integer()            %% font size (index)
	}).

-type expr() :: #leaf{} | #node2{} | #node1{}.

-record(s,
	{
	  backend,
	  window,
	  backing,
	  fonts,   %% [{Font,Ascent}]  ont for each size
	  fg_gc,
	  bg_gc,
	  ln_gc,
	  op_gc,
	  x = 40,
	  y = 40,
	  width = 640,
	  height = 480,
	  bgcolor = black,
	  fgcolor = white,
	  pixels,
	  %% only works when '=' is top node!
	  expr  :: expr(),
	  invalid = [] :: [epx:epx_rect()]
	}).

rect_x({X,_,_,_}) -> X.
rect_y({_,Y,_,_}) -> Y.
rect_width({_,_,W,_}) -> W.
rect_height({_,_,_,H}) -> H.


start() ->
    start("2x+3=4x-4").

start(Equation) ->
    case erl_scan:string(Equation) of
	{ok,Ts,_} ->
	    case parse_expr(Ts) of
		{ok,Expr,[]} ->
		    io:format("Expr=~p\n", [Expr]),
		    init(#s{expr=Expr},[]);
		{ok,_Expr,Ts1} ->
		    {error, syntax, Ts1};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.
    
init(S,_Opts) ->
    epx:start(),
    B = epx_backend:default(),
    Events = [button_press, button_release],
    Window = epx:window_create(S#s.x, S#s.y, S#s.width, S#s.height, Events),
    Back = epx:pixmap_create(S#s.width, S#s.height, argb),
    Fore = epx:pixmap_create(S#s.width, S#s.height, argb),
    Fonts = init_fonts(),
    Fg = epx_gc:create(),
    set_color(Fg, S#s.fgcolor),
    Bg = epx_gc:create(),
    set_color(Bg, S#s.bgcolor),

    Ln = epx_gc:create(),
    epx_gc:set(Ln,[{border_color,{0,0,192,0}},
		   {line_style,[solid]},
		   {fill_style,none}]),
    
    Op = epx_gc:create(),
    epx_gc:set(Op,[{border_color,{0,192,0,0}},
		   {line_style,[solid]},
		   {fill_style,none}]),

    epx:pixmap_fill(Back, S#s.bgcolor),
    epx:pixmap_fill(Fore, S#s.bgcolor),
    epx:pixmap_attach(Back, B),
    epx:window_attach(Window, B),
    S1 = S#s {backend=B,
	      window=Window,
	      backing=Back,
	      pixels=Fore,
	      fonts=Fonts,
	      ln_gc = Ln,
	      op_gc = Op,
	      fg_gc = Fg,
	      bg_gc = Bg,
	      invalid = [{0,0,S#s.width,S#s.height}]
	     },
    Expr = place_node(S1, S1#s.expr, false),
    {Ex,Ey,Ew,Eh} = get_bound_rect(Expr),
    X0   =  (S#s.width  - Ew) div 2,
    Y0   =  (S#s.height - Eh) div 2,
    Expr1 = set_bound_rect(Expr, {X0+Ex,Y0+Ey,Ew,Eh}),
    io:format("Placed = ~p\n", [Expr1]),
    S2 = redraw_expr(S1#s { expr=Expr1 }),
    loop(S2),
    epx:pixmap_detach(Back).

init_fonts() ->
    list_to_tuple(init_fonts([24,20,16,12])).

init_fonts([H|T]) ->
    {ok,Font}   = epx_font:match([{name,"Arial"},{size,H}]),
    Ascent = epx:font_info(Font, ascent),
    [{Font,Ascent}|init_fonts(T)];
init_fonts([]) ->
    [].


update(S) ->
    if S#s.invalid =:= [] ->
	    S;
       true ->
	    lists:foreach(
	      fun({X,Y,W,H}) ->
		      epx:pixmap_draw(S#s.backing, S#s.window,
				      X, Y, X, Y, W, H)
	      end, S#s.invalid),
	    S#s { invalid = [] }
    end.

invalidate(S, R) ->
    Rs = [R|S#s.invalid],
    L = length(Rs),
    if L >= 10 ->  %% adjust when needed
	    S#s { invalid=[epx_rect:union(Rs)]};
       true ->
	    S#s { invalid=Rs }
    end.


loop(S) ->
    receive
	{epx_event, Win, close} when Win =:= S#s.window ->
	    epx:window_detach(Win),
	    ok;
	{epx_event, Win, Event} when Win =:= S#s.window ->
	    io:format("Event = ~p\n", [Event]),
	    S1 = handle_event(S, Event),
	    loop(S1);
	Event ->
	    io:format("Got: event ~p\n",[Event])
    end.

handle_event(S, {button_press,[left],{X,Y,_}}) ->
    io:format("select (~w,~w)\n", [X,Y]),
    Expr = select_node(X,Y,S#s.expr),
    redraw_expr(S#s { expr=Expr });
handle_event(S, {button_release,_Buttons,{_X,_Y,_}}) ->
    S;
handle_event(S, _Event) ->
    S.


%%
%% apply Fun on each node overlapping Rect 
%% assume subnode are inbedded in node bound rect!!!!
%%
each_node_in_rect(Fun, Rect, Expr) ->
    filter_node_in_rect(fun(E) -> {true,Fun(E)} end, Rect, Expr).

%%
%% select/deselect node 
%%
select_node(X,Y,Expr) ->
    select_nodes({X,Y,1,1},Expr).

select_nodes(Rect,Expr) ->
    filter_node_in_rect(
      fun(X0,Y0,R0,E) -> %% leaf/node1/node2
	      {Ex0,Ey0,_Ew,_Eh} = get_bound_rect(E),
	      {Ex,Ey,Ew,Eh}     = get_local_rect(E),
	      R1 = {Ex0+Ex+X0,Ey0+Ey+Y0,Ew,Eh},
	      Ri = epx_rect:intersect(R0,R1),
	      io:format("select_node: Ri=~w : R0=~w, R1=~w\n", [Ri,R0,R1]),
	      case Ri of
		  {0,0,0,0} ->
		      true;
		  _ ->
		      io:format("select node = ~p\n", [E]),
		      Selected = not get_node_selected(E),
		      E1 = map_node(
			     fun(Ei) -> set_node_selected(Ei, Selected) end, 
			     E),
		      %% E1 = set_node_selected(E, Selected),
		      {false,E1}
	      end
      end, Rect, Expr).

%%
%% apply Fun on each node overlapping Rect 
%% assume subnode are inbedded in node bound rect!!!!
%%
filter_node_in_rect(Fun, Rect, Expr) ->
    filter_node_in_rect(Fun, 0, 0, Rect, Expr).

filter_node_in_rect(Fun,X,Y,Rect,Expr) ->
    {Ex,Ey,Ew,Eh} = get_bound_rect(Expr),
    X1 = X+Ex,
    Y1 = Y+Ey,
    case epx_rect:intersect(Rect,{X1,Y1,Ew,Eh}) of
	{0,0,0,0} ->
	    Expr;
	_Rect1 ->
	    case Fun(X,Y,Rect,Expr) of
		false -> Expr;
		true -> filter_nodes_in_rect(Fun,X1,Y1,Rect,Expr);
		{false,Expr1} -> Expr1;
		{true,Expr1} -> filter_nodes_in_rect(Fun,X1,Y1,Rect,Expr1)
	    end
    end.

filter_nodes_in_rect(Fun,X,Y,Rect,Expr) ->
    N = get_number_of_sub_nodes(Expr),
    lists:foldl(
      fun(I,E) ->
	      S = filter_node_in_rect(Fun,X,Y,Rect,
				      get_sub_node(E,I)),
	      set_sub_node(E,I,S)
      end, Expr, lists:seq(1,N)).

%% map fun over all nodes and children
map_node(Fun, Node) ->
    filter_node(fun(Node1) -> {true,Fun(Node1)} end, Node).

%% map over children (recursive)
map_nodes(Fun, Node) ->
    filter_children(fun(Node1) -> {true,Fun(Node1)} end, Node).

%% run fun over the node tree
filter_node(Fun, Node) ->
    case Fun(Node) of
	false -> Node;
	{false,Node1} -> Node1;
	true -> filter_children(Fun, Node);
	{true,Node1} -> filter_children(Fun, Node1)
    end.
	
filter_children(Fun, Node) ->
    N = get_number_of_sub_nodes(Node),
    lists:foldl(
      fun(I,Node1) ->
	      Ni = filter_node(Fun, get_sub_node(Node1,I)),
	      set_sub_node(Node1,I,Ni)
      end, Node, lists:seq(1,N)).




redraw_expr(S) ->
    draw_node(S, 0, 0, S#s.expr),
    update(S),
    S.

draw_node(S, X, Y, E=#leaf{}) ->
    {Fg,Bg} = node_gc(S,E),
    draw_string(S, Fg, Bg, X, Y, E#leaf.rect,
		E#leaf.size,
		E#leaf.string),
    debug_rect(S, X, Y, E#leaf.rect);
draw_node(S, X, Y, E=#node1{}) ->
    {Ex,Ey,_Ew,_Eh} = E#node1.rect,
    {Fg,Bg} = node_gc(S,E),
    draw_string(S, Fg, Bg, X+Ex, Y+Ey, E#node1.orect,
		E#node1.size,
		atom_to_list(E#node1.operator)),
    draw_node(S, X+Ex, Y+Ey, E#node1.expr),
    debug_rect(S, X, Y, E#node1.rect),
    debug_orect(S, X+Ex, Y+Ey, E#node1.orect);
draw_node(S, X, Y, E=#node2{}) ->
    {Ex,Ey,_Ew,_Eh} = E#node2.rect,
    {Fg,Bg} = node_gc(S,E),
    draw_rect(S, Fg, X+Ex, Y+Ey, E#node2.lprect),
    draw_rect(S, Fg, X+Ex, Y+Ey, E#node2.rprect),
    draw_node(S, X+Ex, Y+Ey, E#node2.lexpr),
    case E#node2.operator of
	'/' ->
	    draw_rect(S, Fg, X+Ex, Y+Ey, E#node2.orect);
	_ ->
	    draw_string(S, Fg, Bg, X+Ex, Y+Ey, E#node2.orect,
			E#node2.size,
			atom_to_list(E#node2.operator))
    end,
    draw_node(S, X+Ex, Y+Ey, E#node2.rexpr),
    debug_rect(S, X, Y, E#node2.rect),
    debug_orect(S, X+Ex, Y+Ey, E#node2.orect).
    
    

node_gc(S,E) ->
    case get_node_selected(E) of
	false ->{S#s.fg_gc, S#s.bg_gc};
	true -> {S#s.bg_gc, S#s.fg_gc}
    end.

%% make sure alpha=0 for font color
%% also set fill color, when using inverted color scheme
set_color(Gc, Color) ->
    epx_gc:set_foreground_color(Gc, Color),
    {_A,R,G,B} = epx_gc:get_foreground_color(Gc),
    epx_gc:set_foreground_color(Gc, {0,R,G,B}),
    epx_gc:set_fill_color(Gc, Color),
    epx_gc:set_fill_style(Gc, [solid]).    

draw_rect(S, Fg, X0, Y0, {X,Y,W,H}) ->
    if W>0, H>0 ->
	    epx:pixmap_draw_rectangle(S#s.backing, Fg, X0+X, Y0+Y, W, H);
       true ->
	    ok
    end.

debug_rect(_S, _X0, _Y0, _R) ->
    draw_rect(_S, _S#s.ln_gc, _X0, _Y0, _R),
    ok.

debug_orect(_S, _X0, _Y0, _R) ->
    draw_rect(_S, _S#s.op_gc, _X0, _Y0, _R),
    ok.

draw_string(S, Fg, Bg,  X0, Y0, {X,Y,W,H}, Size, String) ->
    if W>0, H>0 ->
	    X1 = X0+X, Y1 = Y0+Y,
	    epx:pixmap_draw_rectangle(S#s.backing, Bg, X1, Y1, W, H),
	    {Font,Ascent} = element(Size, S#s.fonts),
	    epx_gc:set_font(Fg, Font),
	    epx:font_draw_string(S#s.backing, Fg, X1, Y1+Ascent, String),
	    ok;
       true ->
	    ok
    end.

-define(HGAP, 2).   %% horizontal
-define(VGAP, 2).   %% vertical

%% do a relative placements of expression nodes
place_node(S, E=#leaf{}, _Parentheses) ->
    {Font,_Ascent} = element(E#leaf.size, S#s.fonts),
    {W,H} = epx_font:dimension(Font, E#leaf.string),
    E#leaf { rect={0,0,W,H} };
place_node(S, E=#node1{}, Parentheses) ->
    M = place_node(S, E#node1.expr, is_record(E#node1.expr, leaf)),
    {Font,_Ascent} = element(E#node1.size, S#s.fonts),
    {Ow,Oh} = epx_font:dimension(Font, atom_to_list(E#node1.operator)),
    %% Lx,Ly,Rx,Ry are normally 0 but may be used to offset parts a bit
    {Mx,My,Mw,Mh} = get_bound_rect(M),
    Ew = Ow + ?HGAP + Mw,
    Eh = max(Mh,Oh),
    LPRect = place_lprect(Parentheses, Ew, Eh),
    RPRect = place_rprect(Parentheses, Ew, Eh),
    Tw  = Ew+rect_width(LPRect)+rect_width(RPRect),
    Th = Eh,
    X0 = 0,
    X1 = Oh+?HGAP,
    XL = rect_x(LPRect),
    ORect = {X0+XL,(Eh-Oh) div 2, Ow, Oh},
    MRect = {Mx+X1+XL,My+(Eh-Mh) div 2,Mw, Mh},
    E#node1{ rect = {0,0,Tw,Th},
		lprect = LPRect,
		rprect = RPRect,
		orect=ORect, %% operator rectangle
		expr=set_bound_rect(M, MRect) };
place_node(S, E=#node2{}, Parentheses) ->
    L = place_node(S, E#node2.lexpr, need_parentheses(E, E#node2.lexpr)),
    R = place_node(S, E#node2.rexpr, need_parentheses(E, E#node2.rexpr)),
    %% Lx,Ly,Rx,Ry are normally 0 but may be used to offset parts a bit
    {Lx,Ly,Lw,Lh} = get_bound_rect(L),
    {Rx,Ry,Rw,Rh} = get_bound_rect(R),
    case E#node2.operator of
	'/' -> %% vertical arrangement
	    Y0  = 0,
	    Y1  = Lh + ?VGAP,
	    Ew  = max(Lw,Rw),   %% expression width
	    Eh  = Y1+?VGAP+Rh,  %% expression height
	    LPRect = place_lprect(Parentheses, Ew, Eh),
	    RPRect = place_rprect(Parentheses, Ew, Eh),
	    XL = rect_x(LPRect),
	    LRect = {Lx+XL+(Ew-Lw) div 2, Ly+Y0,   Lw, Lh},
	    ORect = {?HGAP+XL,Y1,Ew-2*?HGAP, ?HGAP},
	    RRect = {Rx+XL+(Ew-Rw) div 2, Ry+?VGAP+Y1, Rw, Rh},
	    Tw  = Ew+rect_width(LPRect)+rect_width(RPRect),
	    Th = Eh,
	    E#node2{ rect = {0,0,Tw,Th},
			lprect = LPRect,
			rprect = RPRect,
			orect=ORect,
			lexpr=set_bound_rect(L, LRect),
			rexpr=set_bound_rect(R, RRect) };
	'^' -> %% super script arrangement
	    X0  = 0,
	    Y0  = 0,
	    Ew  = Lw+Rw,
	    YOffs = 3, %% ?VGAP,
	    Eh  = Lh+YOffs,
	    LPRect = place_lprect(Parentheses, Ew, Eh),
	    RPRect = place_rprect(Parentheses, Ew, Eh),
	    XL = rect_x(LPRect),
	    LRect = {Lx+X0+XL, Ly+Y0, Lw, Lh},
	    ORect = {0,0,0,0},
	    RRect = {Lx+X0+XL+Lw, Ly+Y0-YOffs, Rw, Rh},
	    Tw  = Ew+rect_width(LPRect)+rect_width(RPRect),
	    Th = Eh,
	    E#node2{ rect = {0,0,Tw,Th},
			lprect = LPRect,
			rprect = RPRect,
			orect=ORect,
			lexpr=set_bound_rect(L, LRect),
			rexpr=set_bound_rect(R, RRect) };
	    
	_ -> %% horizontal
	    {Ow,Oh} = place_operator(S, E, L, R),
	    HGap = if Ow =:= 0 -> 0;
		      true -> ?HGAP
		   end,
	    X1 = Lw + HGap,
	    X2 = X1 + Ow + HGap,
	    Ew = X2+Rw,                %% expression width
	    Eh = max(max(Lh,Rh),Oh),   %% expression height
	    LPRect = place_lprect(Parentheses, Ew, Eh),
	    RPRect = place_rprect(Parentheses, Ew, Eh),
	    XL = rect_x(LPRect)+rect_width(LPRect),
	    
	    LRect = {Lx+XL,    Ly+(Eh-Lh) div 2,   Lw, Lh},
	    ORect = {X1+XL,    ((Eh - Oh) div 2),  Ow, Oh},
	    RRect = {Rx+X2+XL, Ry+((Eh-Rh) div 2), Rw, Rh},
	    Tw = Ew+rect_width(LPRect)+rect_width(RPRect),
	    Th = Eh,
	    E#node2{ rect = {0,0,Tw,Th},
		     lprect = LPRect,
		     rprect = RPRect,
		     orect=ORect, %% operator rectangle
		     lexpr=set_bound_rect(L, LRect),
		     rexpr=set_bound_rect(R, RRect) }
    end.

%% need operator?
place_operator(S, E, L, R) ->
    {Font,_Ascent} = element(E#node2.size, S#s.fonts),
    case E#node2.operator of
	'*' ->
	    if is_record(L, leaf), is_record(R, leaf) ->
		    if is_integer(L#leaf.value),
		       is_atom(R#leaf.value) ->
			    {0,0};
		       true ->
			    epx_font:dimension(Font, "*")
		    end;
	       is_record(L, leaf) ->
		    {0,0};
	       is_record(R, leaf) ->
		    {0,0};
	       true ->
		    epx_font:dimension(Font, "*")
	    end;
	Op ->
	    epx_font:dimension(Font, atom_to_list(Op))
    end.
    

place_lprect(false, _W, _H) ->
    {0,0,0,0};
place_lprect(true, _W, H) ->
    {0,0,3,H}.

place_rprect(false, _W, _H) ->
    {0,0,0,0};
place_rprect(true, W, H) ->
    {3+W,0,3,H}.

%%
%% Format expression as string
%%
node_to_string(E=#leaf{}) -> 
    E#leaf.string;
node_to_string(E=#node1{}) -> 
    atom_to_list(E#node1.operator) ++
    case is_record(E#node1.expr, leaf) of
	true -> node_to_string(E#node1.expr);
	false ->
	    "(" ++ node_to_string(E#node1.expr) ++ ")"
    end;
node_to_string(E=#node2{}) -> 
    case need_parentheses(E, E#node2.lexpr) of
	true ->
	    "(" ++ node_to_string(E#node2.lexpr) ++ ")";
	false ->
	    node_to_string(E#node2.lexpr)
    end ++
	atom_to_list(E#node1.operator) ++
    case need_parentheses(E, E#node2.rexpr) of
	true ->
	    "(" ++ node_to_string(E#node2.rexpr) ++ ")";
	false ->
	    node_to_string(E#node2.rexpr)
    end.


need_parentheses(P, C) ->
    Pi = get_node_priority(P),
    Ci = get_node_priority(C),
    if Pi > Ci ->
	    case (get_node_operator(P) =:= '/') of
		true -> false;
		false -> true
	    end;
       Pi =:= Ci ->
	    (get_node_operator(P) =:= '-') 
		andalso
		  (get_node_operator(C) =:= '+');
       true ->
	    false
    end.

%%
%% Fixme: add support for LaTex expression
%% \sqrt{x}
%% \frac{x}{y}
%% x+y
%% x-y
%% x/y
%% \cdot    (centered dot | multiplication)
%% \times
%% \cdots    (ellipsis)
%% x^y       super script
%% x_y       subscript
%% {x}       enclosure 
%% \sum_{i=1}^{n}   sum(i=1..n 
    
%% expr = 
%%      integer() atom()  - special 2x = 2*x
%%      integer()
%%      atom()
%%   |  - expr
%%   |  + expr
%%   |  expr '+' expr
%%   |  expr '-' expr
%%   |  expr '*' expr
%%   |  expr '/' expr
%%   |  expr '^' expr
%%   |  '(' expr ')'
%%

scan_and_parse_expr(String) ->
    case erl_scan:string(String) of
	{ok,Ts,_} ->
	    parse_expr(Ts);
	Error ->
	    Error
    end.

parse_expr(Ts) ->
    parse_expr_10(Ts).

parse_expr_10(Ts) ->
    case parse_expr_20(Ts) of
	{ok,L,[{Op,_}|Ts1]} 
	  when Op =:= '='; 
	       Op =:= '<';
	       Op =:= '<=';
	       Op =:= '>';
	       Op =:= '>=';
	       Op =:= '<>' ->
	    case parse_expr_10(Ts1) of
		{ok,R,Ts2} ->
		    {ok,make_expr(Op,L,R),Ts2};
		Error ->
		    Error
	    end;
	Result -> Result
    end.

parse_expr_20(Ts) ->
    case parse_expr_30(Ts) of
	{ok,L,[{Op,_}|Ts1]} when Op =:= '+'; Op =:= '-' ->
	    case parse_expr_20(Ts1) of
		{ok,R,Ts2} ->
		    {ok,make_expr(Op,L,R),Ts2};
		Error ->
		    Error
	    end;
	Result -> Result
    end.

parse_expr_30(Ts) ->
    case parse_expr_40(Ts) of
	{ok,L,[{Op,_}|Ts1]} when Op =:= '*'; Op =:= '/' ->
	    case parse_expr_30(Ts1) of
		{ok,R,Ts2} ->
		    {ok,make_expr(Op,L,R),Ts2};
		Error -> Error
	    end;
	{ok,L,Ts0=[{'(',_}|_Ts1]} ->
	    case parse_expr_30(Ts0) of
		{ok,R,Ts2} ->
		    {ok,make_expr('*',L,R),Ts2};
		Error -> Error
	    end;
	{ok,L,Ts0=[{atom,_,_V}|_Ts1]} ->
	    case parse_expr_30(Ts0) of
		{ok,R,Ts2} ->
		    {ok,make_expr('*',L,R),Ts2};
		Error -> Error
	    end;	    
	Result -> Result
    end.

parse_expr_40(Ts) ->
    case parse_expr_prim(Ts) of
	{ok,L,[{Op,_}|Ts1]} when Op =:= '^' ->
	    case parse_expr_40(Ts1) of
		{ok,R,Ts2} ->
		    {ok,make_expr(Op,L,R),Ts2};
		Error -> Error
	    end;
	Result -> Result
    end.

parse_expr_prim([{integer,_,C}|Ts]) -> {ok, make_expr(C), Ts};
parse_expr_prim([{atom,_,V}|Ts]) ->    {ok, make_expr(V), Ts};
parse_expr_prim([{Op,_Ln}|Ts]) when Op =:= '+'; Op =:= '-' ->
    case parse_expr(Ts) of
	{ok,E,Ts1} ->
	    {ok,make_expr(Op,E),Ts1};
	_ ->
	    {error,syntax,Ts}
    end;
parse_expr_prim([{'(',_Ln}|Ts]) ->
    case parse_expr(Ts) of
	{ok,E,[{')',_}|Ts1]} ->
	    {ok,E,Ts1};
	_ ->
	    {error,syntax,Ts}
    end.

make_expr(V) when is_integer(V) ->
    #leaf { value=V, string=integer_to_list(V)};
make_expr(V) when is_atom(V) ->
    #leaf { value=V, string=atom_to_list(V)}.

make_expr(Op,E) ->
    #node1 { expr=E, operator=Op}.

make_expr('^',L,R) ->
    R1 = set_tree_size(R, get_node_size(L)+1),
    #node2 { lexpr=L, rexpr=R1, operator='^'};
make_expr(Op,L,R) ->
    #node2 { lexpr=L, rexpr=R, operator=Op}.

get_number_of_sub_nodes(#leaf{}) -> 0;
get_number_of_sub_nodes(#node1{}) -> 1;
get_number_of_sub_nodes(#node2{}) -> 2.

get_sub_nodes(#leaf{}) -> [];
get_sub_nodes(#node1{expr=E}) -> [E];
get_sub_nodes(#node2{lexpr=L,rexpr=R}) -> [L,R].

get_sub_node(#node1{expr=E},1) -> E;
get_sub_node(#node2{lexpr=L},1) -> L;
get_sub_node(#node2{rexpr=R},2) -> R.

set_sub_node(N=#node1{},1,E) -> N#node1{expr=E};
set_sub_node(N=#node2{},1,E) -> N#node2{lexpr=E};
set_sub_node(N=#node2{},2,E) -> N#node2{rexpr=E}.

get_bound_rect(#leaf{rect=R}) -> R;
get_bound_rect(#node2{rect=R}) -> R;
get_bound_rect(#node1{rect=R}) -> R.

set_bound_rect(N=#leaf{},R) -> N#leaf{rect=R};
set_bound_rect(N=#node2{},R)  -> N#node2{rect=R};
set_bound_rect(N=#node1{},R)  -> N#node1{rect=R}.

get_local_rect(#leaf{rect={_,_,W,H}}) -> {0,0,W,H};
get_local_rect(#node2{orect=R}) -> R;
get_local_rect(#node1{orect=R}) -> R.

get_node_string(#leaf{string=S}) -> S;
get_node_string(#node2{string=S}) -> S;
get_node_string(#node1{string=S}) -> S.

get_node_selected(#leaf{selected=S}) -> S;
get_node_selected(#node2{selected=S}) -> S;
get_node_selected(#node1{selected=S}) -> S.
    
set_node_selected(N=#leaf{},S) -> N#leaf{selected=S};
set_node_selected(N=#node2{},S) -> N#node2{selected=S};
set_node_selected(N=#node1{},S) -> N#node1{selected=S}.

toggle_node_selected(N=#leaf{}) ->
    N#leaf{selected=not N#leaf.selected};
toggle_node_selected(N=#node2{}) ->
    N#node2{selected=not N#node2.selected};
toggle_node_selected(N=#node1{}) -> 
    N#node1{selected=not N#node1.selected}.

get_node_size(#leaf{size=Sz}) -> Sz;
get_node_size(#node2{size=Sz}) -> Sz;
get_node_size(#node1{size=Sz}) -> Sz.

%% recursivly set size value (font size)
set_tree_size(N=#leaf{}, Sz) -> 
    N#leaf { size=Sz };
set_tree_size(N=#node2{},Sz) ->
    if N#node2.operator =:= '^' ->
	    L = set_tree_size(N#node2.lexpr, Sz),
	    R = set_tree_size(N#node2.rexpr, Sz+1),
	    N#node2 { lexpr=L, rexpr=R, size=Sz };
       true ->
	    L = set_tree_size(N#node2.lexpr, Sz),
	    R = set_tree_size(N#node2.rexpr, Sz),
	    N#node2 { lexpr=L, rexpr=R, size=Sz }
    end;
set_tree_size(N=#node1{},Sz) ->
    M = set_tree_size(N#node1.expr, Sz),
    N#node1 { expr=M, size=Sz }.

get_node_operator(#leaf{}) -> '';
get_node_operator(#node1{operator=Op}) -> Op;
get_node_operator(#node2{operator=Op}) -> Op.

get_node_priority(#leaf{}) -> 0;
get_node_priority(#node1{})  -> 1;
get_node_priority(E=#node2{})  ->
    case E#node2.operator of
	'=' -> 10;
	'<' -> 10;
	'>' -> 10;
	'<=' -> 10;
	'>=' -> 10;
	'<>' -> 10;
	'+'  -> 20;
	'-'  -> 20;
	'*'  -> 30;
	'/'  -> 30;
	'^'  -> 40
    end.
