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
  Forms, Classes, SysUtils, simpleipc, ExtCtrls;
  
type

  TOnOtherInstance = procedure(Sender : TObject; ParamCount: Integer; const Parameters: array of String) of object;

  { TUniqueInstance }

  TUniqueInstance = class(TComponent)
  private
    FIdentifier: String;
    FOnOtherInstance: TOnOtherInstance;
    FUpdateInterval: Cardinal;
    FEnabled: Boolean;
    FPriorInstanceRunning: Boolean;
    procedure ReceiveMessage(Sender: TObject);
    {$ifdef PollIPCMessage}
    procedure CheckMessage(Sender: TObject);
    {$endif}
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    property PriorInstanceRunning: Boolean read FPriorInstanceRunning;
  published
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Identifier: String read FIdentifier write FIdentifier;
    property UpdateInterval: Cardinal read FUpdateInterval write FUpdateInterval default 1000;
    property OnOtherInstance: TOnOtherInstance read FOnOtherInstance write FOnOtherInstance;
  end;

implementation

uses
  StrUtils, uUiBase;

{ TUniqueInstance }

procedure TUniqueInstance.ReceiveMessage(Sender: TObject);
var
  ParamsArray: array of String;
  Params: String;
  Count, i: Integer;
begin
  if Assigned(FOnOtherInstance) then
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

procedure TUniqueInstance.Loaded;
var
  IPCClient: TSimpleIPCClient;
  {$ifdef PollIPCMessage}
  Timer: TTimer;
  {$endif}
begin
  if not (csDesigning in ComponentState) and FEnabled then
  begin
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
      if Assigned(FOnOtherInstance) then
      begin
        Timer := TTimer.Create(Self);
        Timer.Interval := FUpdateInterval;
        Timer.OnTimer := @CheckMessage;
      end;
      {$endif}
    end;
  end;//if
  inherited;
end;

constructor TUniqueInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpdateInterval := 1000;
end;

end.

