%%
%% Calculate cos/sin 
%%

-module(fix_trig).
-compile(export_all).

-define(PI, 3.141592653589793).

cos(X) ->
    sin(?PI/2 - X).

sin(X) ->
    X1 = angle_norm(X),
    if X1 < ?PI/2 ->
	    sin_(X1);
       X1 < ?PI ->
	    sin_(?PI - X1);
       X1 < 3*?PI/2 ->
	    -sin_(X1 - ?PI);
       true ->
	    -sin_(2*?PI - X1)
    end.

%%
%% calculate sin when X is in range [0-pi/2)
%% sin(X+Y) = sin(X)*cos(Y) + cos(X)*sin(Y)
%%
%% sin(1.57079)  = 1.0  = 1
%% sin(0.848026) = 0.75 = 3/4
%% sin(0.523598) = 0.5  = 1/2
%% sin(0.252680) = 0.25 = 1/4
%%
%% sin(X-Y) = sin(X)*cos(Y) - cos(X)*sin(Y)
%% cos(X-Y) = cos(X)*cos(Y) + sin(X)*sin(Y)
%% 
%% sin(3+Y) = sin(3)*cos(Y) + cos(3)*sin(Y)
%% 
sin_(X) ->

%% normalize argument to range 0 - 2pi
angle_norm(X) when X >= 2*?PI ->
    Xi = trunc(X/(2*?PI)),
    X - 2*?PI*Xi;
angle_norm(X) when X < 0 ->
    Xi = trunc(X/(2*?PI)) - 1,
    X - 2*?PI*Xi;
angle_norm(X) ->
    X.



    
    


    
    
    
