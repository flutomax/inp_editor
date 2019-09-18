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

unit uUiComp;

{$mode objfpc}{$H+}
{$if not defined(Windows) or (FPC_FULLVERSION >= 30001)}
{$define PollIPCMessage}
{$endif}

interface

uses
  {$IF DEFINED(MSWINDOWS)}Windows, LCLIntf,{$ENDIF}
  Forms, Classes, SysUtils, simpleipc, ExtCtrls;
  
type

  TOnOtherInstance = procedure(Sender: TObject; ParamCount: Integer;
    const Parameters: array of String) of object;

  { TUniqueInstance }

  TUniqueInstance = class(TComponent)
  private
    fIdentifier: string;
    fEnabled: Boolean;
    fPriorInstanceRunning: Boolean;
    fOnOtherInstance: TOnOtherInstance;
    {$IF DEFINED(MSWINDOWS)}
    fFileMapObj: THandle;
    fHandle: HWND;
    procedure WndProc(var Msg: TMessage);
    {$ELSE}
    fUpdateInterval: Cardinal;
    procedure ReceiveMessage(Sender: TObject);
    {$ifdef PollIPCMessage}
    procedure CheckMessage(Sender: TObject);
    {$endif}
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property PriorInstanceRunning: Boolean read fPriorInstanceRunning;
  published
    property Enabled: Boolean read fEnabled write fEnabled default False;
    property Identifier: string read fIdentifier write fIdentifier;
    property OnOtherInstance: TOnOtherInstance read fOnOtherInstance write fOnOtherInstance;
  end;

implementation

uses
  StrUtils, {$IF NOT DEFINED(MSWINDOWS)}uUiBase,{$ENDIF} uConsts;


{ TUniqueInstance }


constructor TUniqueInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IF DEFINED(MSWINDOWS)}
  fFileMapObj := 0;
  fHandle := LCLIntf.AllocateHWnd(@WndProc);
  {$ELSE}
  fUpdateInterval := 1000;
  {$ENDIF}
end;

destructor TUniqueInstance.Destroy;
begin
  {$IF DEFINED(MSWINDOWS)}
  if fFileMapObj <> 0 then
    CloseHandle(fFileMapObj);
  if fHandle <>0 then
    LCLIntf.DeallocateHWnd(fHandle);
  {$ENDIF}
  inherited Destroy;
end;

{$IF DEFINED(MSWINDOWS)}

procedure TUniqueInstance.WndProc(var Msg: TMessage);
var
  d: TCopyDataStruct;
  p: array of string;
  n: Integer;
begin
  if (Msg.msg = WM_COPYDATA) and Assigned(fOnOtherInstance) then begin
    d := PCopyDataStruct(Msg.lParam)^;
    if d.dwData <> ID_OTHER_INSTANCE then
      Exit;
    n := Ord(d.cbData>0);
    if n>0 then begin
      SetLength(p, 1);
      p[0] := string(PChar(d.lpData));
    end;
    fOnOtherInstance(self, n, p);
  end;
end;

{$ELSE}

procedure TUniqueInstance.ReceiveMessage(Sender: TObject);
var
  ParamsArray: array of string;
  Params: string;
  Count, i: Integer;
begin
  if Assigned(fOnOtherInstance) then
  begin
    //MsgType stores ParamCount
    Count := FIPCServer.MsgType;
    SetLength(ParamsArray, Count);
    Params := FIPCServer.StringMessage;
    for i := 1 to Count do
      ParamsArray[i - 1] := ExtractWord(i, Params, [ParamsSeparator]);
    FOnOtherInstance(Self, Count, ParamsArray);
  end;
end;

{$ifdef PollIPCMessage}
procedure TUniqueInstance.CheckMessage(Sender: TObject);
begin
  FIPCServer.PeekMessage(1, True);
end;
{$endif}
{$ENDIF}

procedure TUniqueInstance.Loaded;
var
  {$IF DEFINED(MSWINDOWS)}
  i: Integer;
  s: string;
  h: HWND;
  p: Pointer;
  d: TCopyDataStruct;
  {$ELSE}
  IPCClient: TSimpleIPCClient;
  {$ifdef PollIPCMessage}
  Timer: TTimer;
  {$endif}
  {$ENDIF}
begin
  if not (csDesigning in ComponentState) and fEnabled then
  begin
    {$IF DEFINED(MSWINDOWS)}
    fFileMapObj := CreateFileMapping(MAXDWORD, nil, PAGE_READWRITE, 0,
      SizeOf(HWND), PChar(fIdentifier));
    fPriorInstanceRunning := (GetLastError = ERROR_ALREADY_EXISTS);
    if fPriorInstanceRunning then begin
      h := 0;
      p := MapViewOfFile(fFileMapObj, FILE_MAP_READ, 0, 0, SizeOf(HWND));
      Move(p^, h, SizeOf(HWND));
      UnMapViewOfFile(p);
      FillChar(d, SizeOf(d), 0);
      d.dwData := ID_OTHER_INSTANCE;
      s:='';
      if ParamCount>0 then begin
        // format arguments of command line
        for i:=1 to ParamCount do
          s:=Format('%s%s ',[s, ParamStr(i)]);
        s:=TrimRight(s);
        d.cbData := 1 + Length(s);
        d.lpData := PChar(s);
      end;
      SendMessage(h, WM_COPYDATA, fHandle, {%H-}LPARAM(@d));
      Application.ShowMainForm := False;
      Application.Terminate;
    end else begin
      // send to hile mapping mainform handle
      p := MapViewOfFile(fFileMapObj, FILE_MAP_WRITE, 0, 0, SizeOf(HWND));
      Move(fHandle, p^, SizeOf(HWND));
      UnMapViewOfFile(p);
    end;
    {$ELSE}
    IPCClient := TSimpleIPCClient.Create(Self);
    IPCClient.ServerId := GetServerId(FIdentifier);
    if not Assigned(FIPCServer) and IPCClient.ServerRunning then
    begin
      //A older instance is running.
      FPriorInstanceRunning := True;
      //A instance is already running
      //Send a message and then exit
      if Assigned(FOnOtherInstance) then
      begin
        IPCClient.Active := True;
        IPCClient.SendStringMessage(ParamCount, GetFormattedParams);
      end;
      Application.ShowMainForm := False;
      Application.Terminate;
    end
    else
    begin
      if not Assigned(FIPCServer) then
        InitializeUniqueServer(IPCClient.ServerID);
      FIPCServer.OnMessage := @ReceiveMessage;
      //there's no more need for IPCClient
      IPCClient.Destroy;
      {$ifdef PollIPCMessage}
      if Assigned(fOnOtherInstance) then
      begin
        Timer := TTimer.Create(Self);
        Timer.Interval := FUpdateInterval;
        Timer.OnTimer := @CheckMessage;
      end;
      {$endif}
    end;
    {$ENDIF}
  end;//if
  inherited;
end;


end.

