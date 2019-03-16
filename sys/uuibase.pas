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

unit uUiBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc;

const
  ParamsSeparator = #13;

var
  FIPCServer: TSimpleIPCServer;

procedure InitializeUniqueServer(const ServerId: String);

function GetFormattedParams: String;

function GetServerId(const Identifier: String): String;

implementation

uses
  LazUTF8;

const
  BaseServerId = 'tuniqueinstance_';

procedure InitializeUniqueServer(const ServerId: String);
begin
  //It's the first instance. Init the server
  if FIPCServer = nil then
  begin
    FIPCServer := TSimpleIPCServer.Create(nil);
    FIPCServer.ServerID := ServerId;
    FIPCServer.Global := True;
    FIPCServer.StartServer;
  end;
end;

function GetFormattedParams: String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to ParamCount do
    Result := Result + ParamStrUTF8(i) + ParamsSeparator;
end;

function GetServerId(const Identifier: String): String;
begin
  if Identifier <> '' then
    Result := BaseServerId + Identifier
  else
    Result := BaseServerId + ExtractFileName(ParamStrUTF8(0));
end;

finalization
  FIPCServer.Free;

end.

