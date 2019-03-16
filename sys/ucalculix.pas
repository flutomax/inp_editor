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

unit uCalculix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TCGXPreFlags = (pfC, pfB, pfDuns2d, pfDuns3d, pfIsaac2d, pfIsaac3d, pfFoam,
    pfNg, pfStep, pfStl);

  TCalculixOper = (coCCX, coCGXPre, coCGXPost);

  function StrToCGXPreFlag(const s: string): TCGXPreFlags;
  function CGXPreFlagToStr(const f: TCGXPreFlags): string;
  procedure RunCalculix(const aOper: TCalculixOper; const f: TCGXPreFlags;
    aFileName: string);

implementation

uses
  {$IfDef Windows}Windows,{$EndIf}
  Forms, FileUtil, Dialogs, uFrmMain, uFileUtils, uConsts;

const
  sPreFlags: array[TCGXPreFlags] of string = (
    '-c','-b','-duns2d','-duns3d','-isaac2d','-isaac3d','-foam',
    '-ng','-step','-stl');


function StrToCGXPreFlag(const s: string): TCGXPreFlags;
var
  i: TCGXPreFlags;
begin
  for i:=Low(TCGXPreFlags) to High(TCGXPreFlags) do
    if s=sPreFlags[i] then
      Exit(i);
  result:=pfC;
end;

function CGXPreFlagToStr(const f: TCGXPreFlags): string;
begin
  result:=sPreFlags[f];
end;


procedure RunCalculix(const aOper: TCalculixOper; const f: TCGXPreFlags;
  aFileName: string);
var
  term, path, cmd, app: string;
  std_term: Boolean;
begin
  if not FrmMain.Config.TerminalExists then
    Exit;
  if aOper=coCCX then begin
    path:=FrmMain.Config.CCXPath;
    app:='CCX';
  end else begin
    path:=FrmMain.Config.CGXPath;
    app:='CGX';
  end;

  if Length(path)=0 then begin
    MessageDlg(Format(sCalculixNotExists1,[app]),mtError,[mbNo],0);
    Exit;
  end;

  if not ZFileExists(path) then begin
    MessageDlg(Format(sCalculixNotExists2,[path,app]),mtError,[mbNo],0);
    Exit;
  end;
  term:=FrmMain.Config.TerminalPath;
  FrmMain.apCalculix.Executable:=term;
  FrmMain.apCalculix.Parameters.Clear;
  FrmMain.apCalculix.CurrentDirectory:=ExtractFileDir(aFileName);

  {$IfDef Windows}
  case aOper of
    coCCX:  path:=Format('/k ""%s" "%s""',[path,ExtractFileNameWithoutExt(aFileName)]);
    coCGXPre: path:=Format('/k ""%s" %s "%s""',[path,CGXPreFlagToStr(f),aFileName]);
    coCGXPost: path:=Format('/k ""%s" -v "%s""',[path,ChangeFileExt(aFileName,'.frd')]);
  end;
  FrmMain.apCalculix.Parameters.Add(path);

  {$Else}
  case aOper of
    // CCX used input files without extension
    coCCX: aFileName:=ExtractFileNameWithoutExt(aFileName);
    coCGXPost: aFileName:=ChangeFileExt(aFileName,'.frd');
  end;

  std_term:=ExtractFileName(term)<>'zterm';

  if std_term then begin
    // xterm not allowed quoted arguments on cmdline
    FrmMain.apCalculix.Parameters.Add('-hold');
    FrmMain.apCalculix.Parameters.Add('-e');
  end else begin
    // zterm allowed quoted arguments on cmdline
    aFileName:=AnsiQuotedStr(aFileName,'"');
    path:=AnsiQuotedStr(path,'"');
  end;

  FrmMain.apCalculix.Parameters.Add(path);
  case aOper of
    coCGXPre: FrmMain.apCalculix.Parameters.Add(CGXPreFlagToStr(f));
    //coCGXPost: FrmMain.apCalculix.Parameters.Add('-v');
    coCGXPost: FrmMain.apCalculix.Parameters.Add('-r'); //work oh xterm!
  end;
  FrmMain.apCalculix.Parameters.Add(aFileName);
  {$EndIf}

  FrmMain.apCalculix.Execute;
end;



end.


