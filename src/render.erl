%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%    2D render
%%% @end
%%% Created : 22 Sep 2010 by Tony Rogvall <tony@rogvall.se>

-module(render).

-record(coord, {x, y}).
-record(slope, {sina,cosa}).
-record(rect,  {x0,y0,x1,y1}).

-record(point,
	{
	  p0
	}).

-record(segment,
	{
	  p0,      %% start point
	  p1,      %% end point
	  s0,      %% slope
	  length,  %% segment length
	  width    %% segment width
	}).

-record(triangle,
	{
	  p0,
	  p1,
	  p2,
	  s0,
	  s1,
	  s2
	}).
	
-record(circle,
	{
	  p0,
	  r
	}).

-record(ellipse,
	{
	  p0,
	  a,
	  b
	}).

%% union(A,B)
-record(union,
	{
	  a,
	  b
	}).

%% intersect(A,B)
-record(intersection,
	{
	  a,
	  b
	}).

%% A-B
-record(difference,
	{
	  a,
	  b
	}).


slope(X0, Y0, X1, Y1) ->
    Dx = X1-X0,
    Dy = Y1-Y0,
    H = math:sqrt(Dx*Dx + Dy*Dy),
    #slope { sa = Dy / H, ca = Dx / A }.
