%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Create massive number of objects and rotate around axes
%%% @end
%%% Created :  2 Dec 2017 by Tony Rogvall <tony@rogvall.se>

-module(epx_random_3d_plot).

-compile(export_all).

%%  Test use of matrix and epx

%% create random sample within the -0.5 - 0.5 cube
make_random_points(N) ->
    %% random vector is 4xN vector each column is a 3D point
    Pts = matrix:uniform(4, N, float32),
    %% Pts = matrix:normal(4, N, float32),
    %% Orient values around 0,0,0 keep
    matrix:subtract(Pts, 0.5).

%% create N random points on the unit globe
make_globe_points(N) ->
    Ptr = matrix:create(4, N, float32, <<>>),
    set_globe_points_(Ptr, N),
    Ptr.

%% x = sqrt(1.0 - y^2)
%% x = sqrt(1.0 - (x^2 + y^2))

set_globe_points_(_Ptr, 0) ->
    ok;
set_globe_points_(Ptr, J) ->
    Ax = rand:uniform()*360,
    Ay = rand:uniform()*360,
    Az = rand:uniform()*360,
    P0 = matrix:from_list([[1.0,0.0,0.0,0.0]],float32),
    [[X,Y,Z,W]] = matrix:to_list(abs_transform(Ax,Ay,Az,P0)),
    matrix:setelement(1,J,Ptr,X),
    matrix:setelement(2,J,Ptr,Y),
    matrix:setelement(3,J,Ptr,Z),
    matrix:setelement(4,J,Ptr,W),
    set_globe_points_(Ptr, J-1).
    
%% create perspective projection
perspective() ->
    perspective({0.0,0.0,4.0}, {0.0,0.0,0.0}, {0.0,0.0,2.0}).

perspective(_Camera={Cx,Cy,Cz},_Orientation={Ax,Ay,Az},_Eye={Ex,Ey,Ez}) ->
    Rx = rotate(Ax, 1.0, 0.0, 0.0),
    Ry = rotate(Ay, 0.0, 1.0, 0.0),
    Rz = rotate(Az, 0.0, 0.0, 1.0),
    CameraTransform = matrix:multiply(Rx,matrix:multiply(Ry,Rz)),
    Fow = 2*math:atan(1/Ez),
    EyeMatrix = matrix:from_list([[1.0, 0.0, -Ex/Ez, 0.0],
				  [0.0, 1.0, -Ey/Ez, 0.0],
				  [0.0, 0.0, 1.0,    0.0],
				  [0.0, 0.0, -1/Ez,  1.0]],float32),
    CameraVec = matrix:from_list([[Cx],[Cy],[Cz],[0.0]],float32),
    {Fow,CameraVec,EyeMatrix,CameraTransform}.

%% transform points for display
draw_points(Pixmap,{_Fow,CameraVec,EyeMatrix,CameraTransform},Ps) ->
    W = epx:pixmap_info(Pixmap, width),
    H = epx:pixmap_info(Pixmap, height),
    {4,M} = matrix:size(Ps),  %% M points to transform
    Ps1 = matrix:subtract(Ps,matrix:rep(1,M,CameraVec)),
    Ds = matrix:multiply(CameraTransform, Ps1),
    Fs = matrix:multiply(EyeMatrix, Ds),
    plot_points(Pixmap,W,H,1,M,Ps,Fs).

plot_points(Pixmap,W,H,J,M,Ps,Fs) when J =< M ->
    Fx = matrix:element(1,J,Fs),
    Fy = matrix:element(2,J,Fs),
    %% Fz = matrix:element(3,J,Fs),
    Fw = matrix:element(4,J,Fs),
    Az = matrix:element(3,J,Ps),
    %% get 2D coordinate
    X = W*(1 + Fx/Fw)/2,
    Y = H*(1 + Fy/Fw)/2,
    L = min(255,max(0,trunc(255*(Az+0.5)))),
    %% Z = 255,
    epx:pixmap_put_pixel(Pixmap,X,Y,0,{L,L,L}),
    plot_points(Pixmap,W,H,J+1,M,Ps,Fs);
plot_points(_Pixmap,_W,_H,_J,_,_Ps,_FsT) ->
    ok.

-spec translate(Tx::float(),Ty::float(),Tz::float()) -> matrix:matrix().

translate(Tx,Ty,Tz) when is_float(Tx), is_float(Ty), is_float(Tz) ->
    Zero = 0.0,
    One = 1.0,
    matrix:from_list(
      [[ One,  Zero, Zero, Zero ],
       [ Zero, One,  Zero, Zero ],
       [ Zero, Zero, One,  Zero ],
       [ Tx,   Ty,   Tz,   One  ]], float32).

-spec scale(Sx::float(),Sy::float(),Sz::float()) -> matrix:matrix().

scale(Sx,Sy,Sz) when is_float(Sx), is_float(Sy), is_float(Sz) ->
    Zero = 0.0,
    matrix:from_list(
      [[ Sx,   Zero, Zero, Zero],
       [Zero, Sy,   Zero, Zero],
       [Zero, Zero, Sz,   Zero],
       [Zero, Zero, Zero, 1.0 ]], float32).

-spec rotate(Angle::float(),Ux::float(),Uy::float(),Uz::float()) ->
		    matrix:matrix().
rotate(A0,Ux,Uy,Uz) when is_float(Ux), is_float(Uy), is_float(Uz) ->
    A = A0*(math:pi()/180),
    CosA = math:cos(A),
    SinA = math:sin(A),
    NCosA = (1-CosA),
    Uxy    = Ux*Uy,
    Uxz    = Ux*Uz,
    Uyz    = Uy*Uz,
    UxSinA = Ux*SinA,
    UySinA = Uy*SinA,
    UzSinA = Uz*SinA,
    Z = 0.0,
    matrix:from_list(
      [[Ux*Ux*NCosA +   CosA, Uxy*  NCosA - UzSinA, Uxz*  NCosA + UySinA, Z],
       [Uxy*  NCosA + UzSinA, Uy*Uy*NCosA +   CosA, Uyz*  NCosA - UxSinA, Z],
       [Uxz*  NCosA - UySinA, Uyz*  NCosA + UxSinA, Uz*Uz*NCosA +   CosA, Z],
       [Z,                   Z,                     Z,                    1.0]],
      float32).


start() ->
    start(640, 480, 4096).

start(W, H, N) ->
    epx:start(),
    Window = epx:window_create(50,50,W,H,[button_press,
					  button_release,
					  key_press]),
    epx:window_attach(Window),
    Pixmap = epx:pixmap_create(W,H,argb),
    epx:pixmap_fill(Pixmap, black),
    epx:pixmap_attach(Pixmap),
    %% Ps = make_random_points(N),
    Ps = make_globe_points(N),
    View = perspective(),
    T0  = matrix:identity(4, 4, float32),
    egear_subscribe(),
    loop(Window,Pixmap,W,H,Ps,View,0.0,0.0,0.0,T0).

egear_subscribe() ->
    catch egear_server:subscribe([{type,dial}]).

loop(Window,Pixmap,W,H,Ps,View,X0,Y0,Z0,T) ->
    Ps1 = matrix:multiply(T, Ps),
    epx:pixmap_fill(Pixmap, black),
    draw_points(Pixmap, View, Ps1),
    epx:pixmap_draw(Pixmap,Window,0,0,0,0,W,H),
    epx:sync(Pixmap,Window),
    receive
	{epx_event,Winow, close} ->
	    epx:window_detach(Winow),
	    epx:pixmap_detach(Pixmap),
	    ok;

	{epx_event,Window,{button_press,[left],{X,Y,Z}}} ->
	    epx:window_enable_events(Window,[motion]),
	    io:format("MOTION ON\n"),
	    loop(Window,Pixmap,W,H,Ps,View,X,Y,Z,T);

	{epx_event,Window,{button_release,[left],{X,Y,Z}}} ->
	    epx:window_disable_events(Window,[motion]),
	    io:format("MOTION OFF\n"),
	    loop(Window,Pixmap,W,H,Ps,View,X,Y,Z,T);

	{epx_event,Window,{button_press,[wheel_down],{X,Y,Z}}} ->
	    %% Z axis zoom-in
	    View1 = zmove(View, -0.2),
	    flush_wheel(Window),
	    loop(Window,Pixmap,W,H,Ps,View1,X,Y,Z,T);
	
	{epx_event,Window,{button_press,[wheel_up],{X,Y,Z}}} ->
	    %% Z axis zoom-out
	    View1 = zmove(View, +0.2),
	    flush_wheel(Window),
	    loop(Window,Pixmap,W,H,Ps,View1,X,Y,Z,T);

	{epx_event,Window,{button_press,[wheel_left],{X,Y,Z}}} ->
	    %% + Z-axis
	    Tn = matrix:multiply(rotate(4.0, 0.0, 0.0, 1.0), T),
	    flush_wheel(Window),
	    loop(Window,Pixmap,W,H,Ps,View,X,Y,Z,Tn);
	
	{epx_event,Window,{button_press,[wheel_right],{X,Y,Z}}} ->
	    %% - Z-axis
	    Tn = matrix:multiply(rotate(-4.0, 0.0, 0.0, 1.0), T),
	    flush_wheel(Window),
	    loop(Window,Pixmap,W,H,Ps,View,X,Y,Z,Tn);

	{epx_event,Window,{motion,[left],{X1,Y1,Z1}}} ->
	    %% test, better to use Ax,Ay,Az!
	    T2 = case X1 - X0 of
		     0 -> T;
		     Dx -> matrix:multiply(rotate(Dx, 0.0, 1.0, 0.0),T)
		 end,
	    Tn = case Y1 - Y0 of
		     0 -> T2;
		     Dy -> matrix:multiply(rotate(-Dy, 1.0, 0.0, 0.0),T2)
		 end,
	    flush_motions(Window),
	    loop(Window,Pixmap,W,H,Ps,View,X1,Y1,Z1,Tn);

	{egear_event,ERef,Info} ->
	    _Value = proplists:get_value(value,Info,0),
	    Dir    = proplists:get_value(direction,Info,1),
	    case proplists:get_value(index,Info,0) of
		2 when is_integer(Dir) ->
		    Tn = matrix:multiply(rotate(Dir, 1.0, 0.0, 0.0),T),
		    flush_egear(ERef),
		    loop(Window,Pixmap,W,H,Ps,View,X0,Y0,Z0,Tn);
		3 when is_integer(Dir) ->
		    Tn = matrix:multiply(rotate(Dir, 0.0, 1.0, 0.0),T),
		    flush_egear(ERef),
		    loop(Window,Pixmap,W,H,Ps,View,X0,Y0,Z0,Tn);
		4 when is_integer(Dir) ->
		    Tn = matrix:multiply(rotate(Dir, 0.0, 0.0, 1.0),T),
		    flush_egear(ERef),
		    loop(Window,Pixmap,W,H,Ps,View,X0,Y0,Z0,Tn);
		_ ->
		    loop(Window,Pixmap,W,H,Ps,View,X0,Y0,Z0,T)
	    end;

	{epx_event,Window, Event} ->
	    io:format("event = ~p\n", [Event]),
	    loop(Window,Pixmap,W,H,Ps,View,X0,Y0,Z0,T)
    end.

zmove({Fow,CameraVec,EyeMatrix,CameraTransform}, Dz) ->
    CameraVec1 = matrix:add(CameraVec,
			    matrix:from_list([[0],[0],[Dz],[0]],float32)),
    {Fow,CameraVec1,EyeMatrix,CameraTransform}.


%% rotate around x, y and z-axis
abs_transform(Ax, Ay, Az, T0) ->
    T1 = matrix:multiply(T0,rotate(Ax, 0.0, 1.0, 0.0)), %% Ax is around Y-axis!
    T2 = matrix:multiply(T1,rotate(Ay, 1.0, 0.0, 0.0)), %% Ay is around X-axis!
    T3 = matrix:multiply(T2,rotate(Az, 0.0, 0.0, 1.0)),
    T3.

flush_egear(ERef) ->
    receive
	{egear_event,ERef,_Info} ->
	    flush_egear(ERef)
    after 0 ->
	    ok
    end.

%% catch up with motions
flush_motions(Window) ->
    receive
	{epx_event,Window,{motion,_,_}} ->
	    flush_motions(Window)
    after 0 ->
	    ok
    end.

flush_wheel(Window) ->
    receive
	{epx_event,Window,{_,[wheel_down],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_left],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_right],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_up],_}} ->
	    flush_wheel(Window)
    after 0 ->
	    ok
    end.
