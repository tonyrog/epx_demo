%%% File    : evt_buffer.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Double event buffering
%%% Created : 29 Sep 2009 by Tony Rogvall <tony@rogvall.se>

-module(evt_buffer).

-export([new/1, new/2,
	 add_element/2,
	 to_list/1]).
-import(lists, [reverse/1,sublist/3]).

%% Fixed size event buffer
-record(evt_buffer,
	{
	  max_size,
	  cur_size,
	  buf1,
	  buf2
	 }).

new(Size) ->
    #evt_buffer { max_size=Size, cur_size=0, buf1=[], buf2=[] }.

new(Size,Buffer) ->
    Buf1 = lists:sublist(Buffer,1,Size),
    CurSize = length(Buf1),
    #evt_buffer { max_size=Size, cur_size=CurSize, buf1=reverse(Buf1),buf2=[] }.

add_element(_Evt,EvtBuf) when EvtBuf#evt_buffer.max_size == 0->
    EvtBuf;
add_element(Evt,EvtBuf) ->
    Size = EvtBuf#evt_buffer.cur_size+1,
    if Size > EvtBuf#evt_buffer.max_size ->
	    %% swap
	    EvtBuf#evt_buffer { cur_size=1, 
				buf1=[Evt], buf2=EvtBuf#evt_buffer.buf1};
       true ->
	    EvtBuf#evt_buffer { cur_size=Size,
				buf1=[Evt|EvtBuf#evt_buffer.buf1]}
    end.

to_list(EvtBuf) when EvtBuf#evt_buffer.max_size == 0 ->
    [];
to_list(EvtBuf) ->
    reverse(sublist(EvtBuf#evt_buffer.buf2,1,
		    EvtBuf#evt_buffer.max_size -
		    EvtBuf#evt_buffer.cur_size)) ++ 
	reverse(EvtBuf#evt_buffer.buf1).


				

    




