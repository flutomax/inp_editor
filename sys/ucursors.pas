{
    Copyright (c) 2016-2019 by Vasily Makarov

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation(version 2);


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License version 2 for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit uCursors;

{$mode objfpc}{$H+}

interface

uses
  LCLType;

type

  TUserCursor = (
    crZoom    = 250,
    crZoomIn  = 251,
    crZoomInH = 252,
    crZoomInV = 253,
    crPanHand = 254,
    crPanGrab = 255,
    crRotate  = 256
  );


implementation

{$R cursors.res}

uses
  Graphics, Forms;

function LoadCursorFromRes(Cursor: TUserCursor): THandle;
var
  Cur: TCursorImage;
begin
  Cur:=TCursorImage.Create;
  try
    Cur.LoadFromResourceID(HInstance,PtrInt(Cursor));
    result:=Cur.ReleaseHandle;
  finally
    Cur.Free;
  end;
end;

procedure LoadUsersCursors;
var
  cr: TUserCursor;
begin
  for cr:=Low(TUserCursor) to High(TUserCursor) do
    Screen.Cursors[Ord(cr)]:=LoadCursorFromRes(cr);
end;

initialization
  LoadUsersCursors;

end.

