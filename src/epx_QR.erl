%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Generate QR codes
%%% @end
%%% Created :  7 Jun 2012 by Tony Rogvall <tony@rogvall.se>

-module(epx_QR).

-include_lib("qrcode/include/qrcode.hrl").

-compile(export_all).

-define(TTY(Term), io:format(user, "[~p] ~p~n", [?MODULE, Term])).

-record(tick,
	{
	  window,
	  bg,
	  width,
	  height,
	  scale = 1,
	  password = <<"">>
	}).

tick() ->
    tick(4, 1000).

tick(Scale,RefreshIval) ->
    tick(Scale,RefreshIval,<<"password">>).
tick(Scale,RefreshIval,Password) ->
    tick(Scale,RefreshIval,Password,fun() -> format_datetime() end).

tick(Scale,RefreshIval,Password,Fun) ->
    epx:start(),
    {Width,Height} = tick_size(Scale,Fun),
    Win = epx:window_create(50, 50, Width, Height),
    epx:window_attach(Win),
    Bg = epx:pixmap_create(Width, Height, argb),
    epx:pixmap_fill(Bg, {255,255,255,255}),
    epx:pixmap_attach(Bg),
    Tick = #tick { window=Win,bg=Bg,scale=Scale,
		   password=Password,
		   width=Width, height=Height },
    tick_update(Tick, Fun),
    tick_loop(Tick,RefreshIval,Fun).


tick_loop(Tick,Refresh,Fun) ->
    receive
	{epx_event,Win,close} when Win =:= Tick#tick.window ->
	    epx:pixmap_detach(Tick#tick.bg),
	    epx:window_detach(Tick#tick.window)
    after Refresh ->
	    tick_update(Tick, Fun),
	    tick_loop(Tick, Refresh, Fun)
    end.

tick_size(Scale,Fun) ->
    Data = iolist_to_binary(Fun()),
    Dummy = epx_plot(Scale,qrcode:encode(Data)),
    W = epx:pixmap_info(Dummy, width),
    H = epx:pixmap_info(Dummy, height),
    {W + Scale*10, H + Scale*10 }.

tick_update(Tick,Fun) ->
    Data = iolist_to_binary(Fun()),
    %% io:format("data: ~p\n", [Data]),
    QRCode = qrcode:encode(Data),
    Image = epx_plot(Tick#tick.scale,QRCode),
    W = epx:pixmap_info(Image, width),
    H = epx:pixmap_info(Image, height),
    X0 = Tick#tick.scale*5,
    Y0 = Tick#tick.scale*5,
    ClockH = 32,
    ClockW = 32,
    Clock = epx:pixmap_create(ClockH,ClockW),
    epx:pixmap_fill(Clock, {0, 255,255,255}),  %% white transparent
    draw_clock(Clock),

    epx:pixmap_copy_area(Image, Tick#tick.bg, 0, 0, X0, Y0, W, H),

    epx:pixmap_copy_area(Clock, Tick#tick.bg, 0, 0, 
			 X0+(W-ClockW) div 2, Y0+(H-ClockH) div 2,
			 ClockW, ClockH, [blend]),

    epx:pixmap_draw(Tick#tick.bg, Tick#tick.window, 
		    0, 0, 0, 0, Tick#tick.width, Tick#tick.height).

format_datetime() ->
    format_datetime(datetime()).
format_datetime({{YYYY,MM,DD},{H,M,S}}) ->
    %% fixme: cache date part in process dict?
    [d4(YYYY),$-,d2(MM),$:,d2(DD),$T,d2(H),$:,d2(M),$:,d2(S)].

d2(X) when is_integer(X), X >= 0, X < 100 ->
    tl(integer_to_list(100+X)).

d4(X) when is_integer(X), X >= 0, X < 10000 ->
    tl(integer_to_list(10000+X)).

datetime() ->
    calendar:now_to_datetime(os:timestamp()).

run() ->
    run(1).

run(Scale) ->
    Passcode = crypto:hash(sha, <<"password">>),
    run(Scale, <<"demo@mydomain.com">>, Passcode, 60).

run(Scale,Domain,Passcode,Seconds) ->
    PasscodeBase32 = base32:encode(Passcode),
    Period = list_to_binary(integer_to_list(Seconds)),
    Token = <<"otpauth://totp/", Domain/binary, "?period=", Period/binary, 
	      "&secret=", PasscodeBase32/binary>>,
    ?TTY({token, Token}),
    QRCode = qrcode:encode(Token),
    Image = epx_plot(Scale,QRCode),
    show(Scale,Image).

mail() ->
    data(2, "mailto:tony@rogvall.se").

mail(Addr) ->
    data(1, Addr).

data(Scale, Data) ->
    QRCode = qrcode:encode(erlang:iolist_to_binary(Data)),
    Image = epx_plot(Scale,QRCode),
    show(Scale,Image).

show(Scale,Image) ->
    epx:start(),
    W = epx:pixmap_info(Image, width),
    H = epx:pixmap_info(Image, height),
    Width = W + Scale*10,
    Height = H + Scale*10,
    Win = epx:window_create(50, 50, Width, Height),
    epx:window_attach(Win),
    Bg = epx:pixmap_create(Width, Height, argb),
    epx:pixmap_fill(Bg, {255,255,255,255}),
    epx:pixmap_attach(Bg),
    epx:pixmap_copy_area(Image, Bg, 0, 0, Scale*5, Scale*5, W, H),
    epx:pixmap_draw(Bg, Win, 0, 0, 0, 0, Width, Height),
    receive
	{epx_event,Win,close} ->
	    epx:pixmap_detach(Bg),
	    epx:window_detach(Win)
    end.


%% Very simple PNG encoder for demo purposes
epx_plot(Scale,#qrcode{dimension = Dim, data = Data}) ->
    Pixmap = epx:pixmap_create(Scale*Dim,Scale*Dim,argb),
    put_pixel_data(Data, Scale, Dim, Pixmap).


put_pixel_data(Bin, Scale, Dim, Pixmap) ->
    put_pixel_row(Bin, Scale, 0, Dim*Scale, Dim, Pixmap).

put_pixel_row(_Bin, _Scale, Y, Ymax, _Dim, Pixmap) when Y >= Ymax ->
    Pixmap;
put_pixel_row(Bin, Scale, Y, Ymax, Dim, Pixmap) ->
    <<RowBits:Dim/bits, Bits/bits>> = Bin,
    Width = Scale*Dim,
    Pixmap1 = put_pixel_bits(RowBits, Scale, Y, 0, Width, Pixmap),
    lists:foreach(fun(I) ->
			  epx:pixmap_copy_area(Pixmap,Pixmap,0,Y,0,Y+I,Width,1)
		  end, lists:seq(1,Scale-1)),
    put_pixel_row(Bits, Scale, Y+Scale, Ymax, Dim, Pixmap1).


put_pixel_bits(_Bits, _Scale, _Y, X, Xmax, Pixmap) when X >= Xmax ->
    Pixmap;
put_pixel_bits(<<1:1, Bits/bits>>, Scale, Y, X, Xmax, Pixmap) ->
    lists:foreach(fun(I) ->
			  epx:pixmap_put_pixel(Pixmap, X+I, Y, black)
		  end, lists:seq(0,Scale-1)),
    put_pixel_bits(Bits, Scale, Y, X+Scale, Xmax, Pixmap);
put_pixel_bits(<<0:1, Bits/bits>>, Scale, Y, X, Xmax, Pixmap) ->
    lists:foreach(fun(I) ->
			  epx:pixmap_put_pixel(Pixmap, X+I, Y, white)
		  end, lists:seq(0,Scale-1)),
    put_pixel_bits(Bits, Scale, Y, X+Scale, Xmax, Pixmap).

%%
totp() ->
    Key = crypto:hash(sha, <<"password">>),	
    totp(Key, 60).
totp(Key, Period) ->
    T = unow() div Period,
    {hotp(Key, T - 1), hotp(Key, T), hotp(Key, T + 1)}.
%% RFC-4226 "HOTP: An HMAC-Based One-Time Password Algorithm"
%% @ref <http://tools.ietf.org/html/rfc4226>
hotp(Key, Count) when is_binary(Key), is_integer(Count) ->
    HS = crypto:hmac(sha, Key, <<Count:64>>),
    <<_:19/binary, _:4, Offset:4>> = HS,
    <<_:Offset/binary, _:1, P:31, _/binary>> = HS,
    HOTP = integer_to_list(P rem 1000000),
    Pad = lists:duplicate(6 - length(HOTP), $0),
    list_to_binary([Pad, HOTP]).

-define(UNIX_TIME_ZERO, 62167219200).

unow() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 
	?UNIX_TIME_ZERO.

%% draw a simple 12 hour clock
draw_clock(Dst) ->
    draw_clock(Dst,time()).

draw_clock(Dst, {Hour24,Minute,Second}) ->
    W = epx:pixmap_info(Dst, width),
    H = epx:pixmap_info(Dst, height),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(black),
    epx:draw_ellipse(Dst, 0, 0, W, H),
    epx_gc:set_fill_color(white),
    epx:draw_ellipse(Dst, 2, 2, W-4, H-4),
    X = W div 2,
    Y = H div 2,
    Hour12 = Hour24 rem 12,
    PI2 = 2*math:pi(),
    PI3H = 3*(math:pi()/2),
    epx_gc:set_foreground_color(black),
    draw_line(Dst, Hour12*(PI2/12)+PI3H, X, Y, X-9),
    epx_gc:set_foreground_color(blue),
    draw_line(Dst, Minute*(PI2/60)+PI3H, X, Y, X-6),
    epx_gc:set_foreground_color(red),
    draw_line(Dst, Second*(PI2/60)+PI3H, X, Y, X-2).

draw_line(Dst, A, X, Y, L) ->
    X1 = round(X + math:cos(A)*L),
    Y1 = round(Y + math:sin(A)*L),
    epx:draw_line(Dst, X, Y, X1, Y1).
