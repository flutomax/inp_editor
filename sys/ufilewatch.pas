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

unit uFileWatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TWatchFileInfo = record
    Exists: Boolean;
    Time: Integer;
    Size: Int64;
  end;

  TWatchStateChange = (wscNone, wscDeleted, wscModified);
  TWatchStateEvent = procedure(Sender: TObject; FileName: TFileName;
    Data: Pointer; State: TWatchStateChange) of object;

  { TWatchItem }

  TWatchItem = class
  private
    fFileName: TFileName;
    fFileInfo: TWatchFileInfo;
    fData: Pointer;
    function GetFileInfo(const aFileName: string): TWatchFileInfo;
  public
    constructor Create(const aFileName: TFilename; Data: Pointer = nil);
    function CheckFile: TWatchStateChange;
    procedure Reset;
  end;

  TWatchList = specialize TFPGMap<string,TWatchItem>;

  { TWatchNotifyer }

  TWatchNotifyer = class
  private
    fWatchList: TWatchList;
    fOnFileStateChange: TWatchStateEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile(const aFileName: TFileName; Data:Pointer);
    procedure RemoveFile(const aFileName: TFileName);
    procedure Update(const aFileName: TFileName);
    procedure CheckFiles;
    property OnFileStateChange: TWatchStateEvent read fOnFileStateChange write fOnFileStateChange;
  end;

implementation

{ TWatchItem }

constructor TWatchItem.Create(const aFileName: TFilename; Data: Pointer);
begin
  fFileName:=aFileName;
  fData:=Data;
  Reset;
end;

procedure TWatchItem.Reset;
begin
  fFileInfo:=GetFileInfo(fFileName);
end;

function TWatchItem.GetFileInfo(const aFileName: string): TWatchFileInfo;
var
  i: Integer;
  Info: TSearchRec;
begin
  FillChar(Result,SizeOf(Result),0);
  for i:=1 to Length(aFileName) do
    if CharInSet(aFileName[i],['?','*']) then
      Exit;
  i:=0;
  Result.Exists:=FindFirst(aFileName,i,Info)=0;
  if Result.Exists then begin
    Result.Time:=Info.Time;
    Result.Size:=Info.Size;
    FindClose(Info);
  end;
end;

function TWatchItem.CheckFile: TWatchStateChange;
var
  fi: TWatchFileInfo;
begin
  Result:=wscNone;
  fi:=GetFileInfo(fFileName);
  try
    if not fi.Exists then begin
      if fFileInfo.Exists then
        Exit(wscDeleted);
    end else begin
      if not fFileInfo.Exists then
         Exit(wscModified);
    end;
    if fi.Exists and
      ((fi.Size<>fFileInfo.Size) or (fi.Time<>fFileInfo.Time)) then
      Result:=wscModified;
  finally
    fFileInfo:=fi;
  end;
end;


{ TWatchNotifyer }

constructor TWatchNotifyer.Create;
begin
  fWatchList:=TWatchList.Create;
end;

destructor TWatchNotifyer.Destroy;
var
  i:integer;
begin
  for i:=fWatchList.Count-1 downto 0 do begin
    fWatchList.Data[i].Free;
    fWatchList.Remove(fWatchList.Keys[i]);
  end;
  fWatchList.Free;
  inherited Destroy;
end;

procedure TWatchNotifyer.CheckFiles;
var
  f: TWatchStateChange;
  i: integer;
begin
  for i:=0 to fWatchList.Count-1 do begin
    f:=fWatchList.Data[i].CheckFile;
    if (f<>wscNone) and Assigned(fOnFileStateChange) then
      fOnFileStateChange(Self,fWatchList.Keys[i],fWatchList.Data[i].fData,f);
  end;
end;

procedure TWatchNotifyer.Update(const aFileName: TFileName);
var
  i: Integer;
begin
  i:=fWatchList.IndexOf(aFileName);
  if i>=0 then
    fWatchList.Data[i].Reset;
end;

procedure TWatchNotifyer.AddFile(const aFileName: TFileName; Data: Pointer);
var
  m: TWatchItem;
begin
  if fWatchList.IndexOf(aFileName)>=0 then
    exit;
  m:=TWatchItem.Create(aFileName,Data);
  fWatchList.Add(aFileName,m);
end;

procedure TWatchNotifyer.RemoveFile(const aFileName: TFileName);
var
  i: Integer;
begin
  i:=fWatchList.IndexOf(aFileName);
  if i<0 then
    exit;
  fWatchList.Data[i].Free;
  fWatchList.Delete(i);
end;



end.

