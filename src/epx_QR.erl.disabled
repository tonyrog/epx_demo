%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Generate QR codes
%%% @end
%%% Created :  7 Jun 2012 by Tony Rogvall <tony@rogvall.se>

-module(epx_QR).


%% Shows how to achieve HOTP/SHA1 with a mobile phone using Google Authenticator.
%%
%% This module is a rag-bag of supporting functions, many of which are simplified 
%% extracts from the core libs (?_common, ?_crypto, ?_math, ?_image). This is to 
%% allow a full-cycle demo without requiring open-sourcing of the entire platform.
%% 
%% @ref QR Code: ISO/IEC 18004 (2000, 1st Edition)

%% Google Authenticator Phone App 
%% iPhone:  <http://itunes.apple.com/us/app/google-authenticator/id388497605?mt=8>
%% Android: <https://market.android.com/details?id=com.google.android.apps.authenticator>

%% Google Authenticator URL Specification 
% @ref <http://code.google.com/p/google-authenticator/wiki/KeyUriFormat>
%  otpauth://TYPE/LABEL?PARAMETERS
%  TYPE: hotp | totp
%  LABEL: string() (usually email address)
%  PARAMETERS:
%    digits = 6 | 8 (default 6)
%    counter = integer() (hotp only, default 0?)
%    period = integer() (in seconds, totp only, default 30)
%    secret = binary() base32 encoded
%    algorithm = MD5 | SHA1 | SHA256 | SHA512 (default SHA1)


-include_lib("qrcode/src/qrcode.hrl").

-compile(export_all).

-define(TTY(Term), io:format(user, "[~p] ~p~n", [?MODULE, Term])).

run() ->
    run(1).

run(Scale) ->
    Passcode = crypto:sha(<<"password">>),
    run(Scale, <<"demo@mydomain.com">>, Passcode, 60).

run(Scale,Domain, Passcode, Seconds) ->
    PasscodeBase32 = base32:encode(Passcode),
    Period = list_to_binary(integer_to_list(Seconds)),
    Token = <<"otpauth://totp/", Domain/binary, "?period=", Period/binary, 
	      "&secret=", PasscodeBase32/binary>>,
    ?TTY({token, Token}),
    QRCode = qrcode:encode(Token),
    Image = epx_plot(Scale,QRCode),
    show(Scale,Image).

mail() ->
    mail(2, "mailto:tony@rogvall.se").

mail(Addr) ->
    mail(1, Addr).

mail(Scale, Addr) ->
    QRCode = qrcode:encode(list_to_binary([Addr])),
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
    loop(Bg,Win).

loop(Bg,Win) ->
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
    Key = crypto:sha(<<"password">>),	
    totp(Key, 60).
totp(Key, Period) ->
    T = unow() div Period,
    {hotp(Key, T - 1), hotp(Key, T), hotp(Key, T + 1)}.
%% RFC-4226 "HOTP: An HMAC-Based One-Time Password Algorithm"
%% @ref <http://tools.ietf.org/html/rfc4226>
hotp(Key, Count) when is_binary(Key), is_integer(Count) ->
    HS = crypto:sha_mac(Key, <<Count:64>>),
    <<_:19/binary, _:4, Offset:4>> = HS,
    <<_:Offset/binary, _:1, P:31, _/binary>> = HS,
    HOTP = integer_to_list(P rem 1000000),
    Pad = lists:duplicate(6 - length(HOTP), $0),
    list_to_binary([Pad, HOTP]).

-define(UNIX_TIME_ZERO, 62167219200).

unow() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 
	?UNIX_TIME_ZERO.
