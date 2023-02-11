-ifndef(__SPRITE_HRL__).
-define(__SPRITE_HRL__, true).

-record(sprite,
	{
	 id,                          %% uniq id / ref
	 x      :: float(),           %% center x-position
	 y      :: float(),           %% center y-position
	 xo     :: float(),           %% center x-offset in pixmap
	 yo     :: float(),           %% center y-offset in pixmap
	 width  :: float(),
	 height :: float(),
	 anglef :: function(),        %% angle function
	 angle  :: float(),           %% rotation angle (degree 0-360)
	 vx     :: float(),           %% velocity x pixels/s
	 vy     :: float(),           %% velocity y pixels/s
	 fx     :: float(),           %% damping of vx
	 fy     :: float(),           %% damping of vy
	 ax     :: float(),           %% accelertion x (pixels/s)/s
	 ay     :: float(),           %% accelertion y (pixels/s)/s
	 m      :: float(),           %% sprite mass
	 va     :: float(),           %% rotation angular speed degree/s
	 wrap   :: boolean(),         %% wrap or bounce
	 rd     :: epx:epx_pixmap(),  %% read pixmap
	 wr     :: epx:epx_pixmap()   %% write pixmap
	}).

-endif.
