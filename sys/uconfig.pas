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

unit uConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Forms, uDerivedClasses;

type

  { TConfig }

  TConfig = class(TComponent)
  private
    fMainForm: TForm;
    fMonitorScanTime: Integer;
    fMonitorShowLegend: Boolean;
    fShowHints: Boolean;
    fStoreFormPlacement: Boolean;
    fUniqueInstance: Boolean;
    fFileAssociation: Boolean;
    fBCAddComment: Boolean;
    fBCAutoInsert: Boolean;
    fBCMakeInclude: Boolean;
    fCGXPreFlag: string;
    fCCXPath: string;
    fCCXPaths: TStrings;
    fCGXPath: string;
    fCGXPaths: TStrings;
    fTemplatesPath: string;
    fTemplatesPaths: TStrings;
    fDefTemplatesPath: string;
    fTerminalPath: string;
    fTerminalPaths: TStrings;
    fTerminalExists: Boolean;
    fExportAsFile: Boolean;
    fStatusbarVisible: Boolean;
    fWarnings: TStringList;
    function GetShowWarning(const aName: string): Boolean;
    procedure SetCCXPath(AValue: string);
    procedure SetCGXPath(AValue: string);
    procedure SetFileAssociation(aValue: Boolean);
    procedure MaximizeMainForm(Data: PtrInt);
    procedure SetShowWarning(const aName: string; AValue: Boolean);
    procedure SetTemplatesPath(aValue: string);
    procedure ReformStringList(aList: TStrings; const aText: string);
    procedure SetTerminalPath(AValue: string);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromIni(Ini: TIniFileEx; const aSection: string);
    procedure SaveToIni(Ini: TIniFileEx; const aSection: string);
    procedure LoadCoolBar(Ini: TIniFileEx; const aSection: string; cb: TCoolBar);
    procedure SaveCoolBar(Ini: TIniFileEx; const aSection: string; cb: TCoolBar);
    procedure UpdateConfig;
    property ShowWarning[const aName: string]: Boolean read GetShowWarning write SetShowWarning;
  published
    property MainForm: TForm read fMainForm write fMainForm;
    property TemplatesPath: string read fTemplatesPath write SetTemplatesPath;
    property TemplatesPaths: TStrings read fTemplatesPaths;
    property DefTemplatesPath: string read fDefTemplatesPath;
    property CCXPath: string read fCCXPath write SetCCXPath;
    property CCXPaths: TStrings read fCCXPaths;
    property CGXPath: string read fCGXPath write SetCGXPath;
    property CGXPaths: TStrings read fCGXPaths;
    property CGXPreFlag: string read fCGXPreFlag write fCGXPreFlag;
    property TerminalPath: string read fTerminalPath write SetTerminalPath;
    property TerminalPaths: TStrings read fTerminalPaths;
    property TerminalExists: Boolean read fTerminalExists;
    property ShowHints: Boolean read fShowHints write fShowHints;
    property StoreFormPlacement: Boolean read fStoreFormPlacement write fStoreFormPlacement;
    property UniqueInstance: Boolean read fUniqueInstance write fUniqueInstance;
    property FileAssociation: Boolean read fFileAssociation write SetFileAssociation;
    property ExportAsFile: Boolean read fExportAsFile write fExportAsFile;
    property StatusbarVisible: Boolean read fStatusbarVisible write fStatusbarVisible;
    property BCAutoInsert: Boolean read fBCAutoInsert write fBCAutoInsert;
    property BCAddComment: Boolean read fBCAddComment write fBCAddComment;
    property BCMakeInclude: Boolean read fBCMakeInclude write fBCMakeInclude;
    property MonitorShowLegend: Boolean read fMonitorShowLegend write fMonitorShowLegend;
    property MonitorScanTime: Integer read fMonitorScanTime write fMonitorScanTime;
  end;

implementation

uses
  {$IfDef Windows}
    Windows, JwaWinBase, uFileAssoc, uCalculix,
  {$EndIf}
  Math, LazUTF8, FileUtil, uFileUtils, uConsts;

function GetCmdShell: string;
{$IfDef Windows}
var
  i: Integer;
  s: UnicodeString;
begin
  i:=GetEnvironmentVariableW('ComSpec',nil,0);
  if i>0 then begin
    SetLength(s,i);
    GetEnvironmentVariableW('ComSpec',PWideChar(s),i);
  end;
  Delete(s,i,1);
  Result:=UTF16ToUTF8(s);
{$Else}
begin
  result:='';
{$EndIf}
end;

{ TConfig }

constructor TConfig.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fStoreFormPlacement:=true;
  fShowHints:=true;
  fUniqueInstance:=true;
  fFileAssociation:=false;
  fExportAsFile:=false;
  fStatusbarVisible:=true;
  fBCAutoInsert:=true;
  fBCAddComment:=true;
  fBCMakeInclude:=true;
  fDefTemplatesPath:=Format('%shlp%stemplates%s',[ProgramDirectory,PathDelim,PathDelim]);
  {
  if not DirectoryExists(fDefTemplatesPath) then
    fDefTemplatesPath:=Format('%shlp%sTemplates%s',[ProgramDirectory,PathDelim,PathDelim]);
  }
  fTemplatesPath:=fDefTemplatesPath;
  fTemplatesPaths:=TStringList.Create;
  fTerminalPaths:=TStringList.Create;
  fCCXPaths:=TStringList.Create;
  fCGXPaths:=TStringList.Create;
  fWarnings:=TStringList.Create;
  fCGXPreFlag:='-c';
  {$IfDef Windows}
    fTerminalPath:=GetCmdShell;
    fCCXPath:=ProgramDirectory+'bin\ccx\ccx212.exe';
    fCGXPath:=ProgramDirectory+'bin\cgx210\cgx.exe';
  {$Else}
    fCCXPath:=ProgramDirectory+'bin/ccx212';
    fCGXPath:=ProgramDirectory+'bin/cgx2.15';
    fTerminalPath:='/usr/bin/xterm';
  {$EndIf}
  fMonitorShowLegend:=true;
  fMonitorScanTime:=3;
end;

destructor TConfig.Destroy;
begin
  FreeAndNil(fTemplatesPaths);
  FreeAndNil(fTerminalPaths);
  FreeAndNil(fCCXPaths);
  FreeAndNil(fCGXPaths);
  FreeAndNil(fWarnings);
  inherited Destroy;
end;

procedure TConfig.MaximizeMainForm(Data: PtrInt);
begin
  fMainForm.WindowState:=wsMaximized;
  fMainForm.Visible:=true;
end;

procedure TConfig.ReformStringList(aList: TStrings; const aText: string);
var
  i: Integer;
begin
  if aText<>'' then begin
    i:=aList.IndexOf(aText);
    if i<0 then
      aList.Insert(0,aText)
    else
      aList.Move(i,0);
    while aList.Count>20 do
      aList.Delete(aList.Count-1);
  end;
end;

procedure TConfig.SetTemplatesPath(aValue: string);
begin
  if fTemplatesPath=AValue then
    Exit;
  fTemplatesPath:=AValue;
  ReformStringList(fTemplatesPaths,fTemplatesPath);
end;

procedure TConfig.SetTerminalPath(AValue: string);
begin
  if fTerminalPath=AValue then
    Exit;
  fTerminalPath:=AValue;
  ReformStringList(fTerminalPaths,fTerminalPath);
end;

procedure TConfig.SetCCXPath(AValue: string);
begin
  if fCCXPath=AValue then
    Exit;
  fCCXPath:=AValue;
  ReformStringList(fCCXPaths,fCCXPath);
end;

procedure TConfig.SetCGXPath(AValue: string);
begin
  if fCGXPath=AValue then
    Exit;
  fCGXPath:=AValue;
  ReformStringList(fCGXPaths,fCGXPath);
end;

procedure TConfig.LoadFromIni(Ini: TIniFileEx; const aSection: string);
var
  ws: TWindowState;
begin
  fTemplatesPath:=ini.ReadString(aSection,'TemplatesPath',fDefTemplatesPath);
  ini.ReadStringList(aSection,'TemplatesPaths',fTemplatesPaths);
  fCCXPath:=ini.ReadString(aSection,'CCXPath',fCCXPath);
  ini.ReadStringList(aSection,'CCXPaths',fCCXPaths);
  fCGXPath:=ini.ReadString(aSection,'CGXPath',fCGXPath);
  ini.ReadStringList(aSection,'CGXPaths',fCGXPaths);
  fCGXPreFlag:=ini.ReadString(aSection,'CGXPreFlag',fCGXPreFlag);
  {$IfNDef Windows}
    fTerminalPath:=ini.ReadString(aSection,'TerminalPath',fTerminalPath);
    ini.ReadStringList(aSection,'TerminalPaths',fTerminalPaths);
  {$EndIf}
  fMonitorScanTime:=EnsureRange(ini.ReadInteger('Monitor','ScanInterval',3),1,5);
  fMonitorShowLegend:=ini.ReadBool('Monitor','ShowLegend',true);
  ini.ReadStringList(aSection,'Warnings',fWarnings);
  fBCAutoInsert:=ini.ReadBool(aSection,'BCAutoInsert',fBCAutoInsert);
  fBCAddComment:=ini.ReadBool(aSection,'BCAddComment',fBCAddComment);
  fBCMakeInclude:=ini.ReadBool(aSection,'BCMakeInclude',fBCMakeInclude);
  fShowHints:=ini.ReadBool(aSection,'ShowHints',fShowHints);
  fStoreFormPlacement:=ini.ReadBool(aSection,'StoreFormPlacement',fStoreFormPlacement);
  fUniqueInstance:=ini.ReadBool(aSection,'UniqueInstance',fUniqueInstance);
  fFileAssociation:=ini.ReadBool(aSection,'FileAssociation',fFileAssociation);
  fExportAsFile:=ini.ReadBool(aSection,'ExportAsFile',fExportAsFile);
  fStatusbarVisible:=ini.ReadBool(aSection,'StatusbarVisible',fStatusbarVisible);
  if fStoreFormPlacement then
    with fMainForm do begin
      Visible:=false;
      ws:=TWindowState(ini.ReadInteger(aSection,'WindowState',Ord(WindowState)));
      if ws=wsMaximized then begin
        WindowState:=wsNormal;
        BoundsRect:=Bounds(
          ini.ReadInteger(aSection,'RestoredLeft',RestoredLeft),
          ini.ReadInteger(aSection,'RestoredTop',RestoredTop),
          ini.ReadInteger(aSection,'RestoredWidth',RestoredWidth),
          ini.ReadInteger(aSection,'RestoredHeight',RestoredHeight));
        Application.QueueAsyncCall(@MaximizeMainForm,0);
      end else begin
        WindowState:=wsNormal;
        BoundsRect:=Bounds(
          ini.ReadInteger(aSection,'NormalLeft',Left),
          ini.ReadInteger(aSection,'NormalTop',Top),
          ini.ReadInteger(aSection,'NormalWidth',Width),
          ini.ReadInteger(aSection,'NormalHeight',Height));
        Visible:=true;
      end;
    end;
  UpdateConfig;
end;

procedure TConfig.SaveToIni(Ini: TIniFileEx; const aSection: string);
begin
  ini.WriteString(aSection,'TemplatesPath',fTemplatesPath);
  ini.WriteStringList(aSection,'TemplatesPaths',fTemplatesPaths);
  ini.WriteString(aSection,'CCXPath',fCCXPath);
  ini.WriteStringList(aSection,'CCXPaths',fCCXPaths);
  ini.WriteString(aSection,'CGXPath',fCGXPath);
  ini.WriteStringList(aSection,'CGXPaths',fCGXPaths);
  ini.WriteString(aSection,'CGXPreFlag',fCGXPreFlag);
  {$IfNDef Windows}
    ini.WriteString(aSection,'TerminalPath',fTerminalPath);
    ini.WriteStringList(aSection,'TerminalPaths',fTerminalPaths);
  {$EndIf}
  ini.WriteInteger('Monitor','ScanInterval',fMonitorScanTime);
  ini.WriteBool('Monitor','ShowLegend',fMonitorShowLegend);
  ini.WriteStringList(aSection,'Warnings',fWarnings);
  ini.WriteBool(aSection,'ShowHints',fShowHints);
  ini.WriteBool(aSection,'BCAutoInsert',fBCAutoInsert);
  ini.WriteBool(aSection,'BCAddComment',fBCAddComment);
  ini.WriteBool(aSection,'BCMakeInclude',fBCMakeInclude);
  ini.WriteBool(aSection,'StoreFormPlacement',fStoreFormPlacement);
  ini.WriteBool(aSection,'UniqueInstance',fUniqueInstance);
  ini.WriteBool(aSection,'FileAssociation',fFileAssociation);
  ini.WriteBool(aSection,'ExportAsFile',fExportAsFile);
  ini.WriteBool(aSection,'StatusbarVisible',fStatusbarVisible);
  if fStoreFormPlacement then
    with fMainForm do begin
      ini.WriteInteger(aSection,'NormalLeft',Left);
      ini.WriteInteger(aSection,'NormalTop',Top);
      ini.WriteInteger(aSection,'NormalWidth',Width);
      ini.WriteInteger(aSection,'NormalHeight',Height);
      ini.WriteInteger(aSection,'RestoredLeft',RestoredLeft);
      ini.WriteInteger(aSection,'RestoredTop',RestoredTop);
      ini.WriteInteger(aSection,'RestoredWidth',RestoredWidth);
      ini.WriteInteger(aSection,'RestoredHeight',RestoredHeight);
      ini.WriteInteger(aSection,'WindowState',Ord(WindowState));
    end;
end;

procedure TConfig.LoadCoolBar(Ini: TIniFileEx; const aSection: string;
  cb: TCoolBar);
var
  i,j,p: integer;
begin
  cb.DisableAutoSizing;
  try
    for i:=0 to cb.Bands.Count-1 do begin
      p:=ini.ReadInteger(aSection,Format('Band%d.Position',[i]),i);
      if p<>cb.Bands[i].ID then
        for j:=i+1 to cb.Bands.Count-1 do
          if cb.Bands[j].ID=p then begin
            cb.Bands[j].Index:=i;
            break;
          end;
    end;
    for i:=0 to cb.Bands.Count-1 do begin
      cb.Bands[i].Break:=ini.ReadBool(aSection,Format('Band%d.Break',[i]),false);
      cb.Bands[i].Visible:=ini.ReadBool(aSection,Format('Band%d.Visible',[i]),true);
    end;

  finally
    cb.EnableAutoSizing;
  end;
end;

procedure TConfig.SaveCoolBar(Ini: TIniFileEx; const aSection: string; cb: TCoolBar);
var
  i: integer;
begin
  for i:=0 to cb.Bands.Count-1 do begin
    ini.WriteInteger(aSection,Format('Band%d.Position',[i]),cb.Bands[i].ID);
    ini.WriteBool(aSection,Format('Band%d.Visible',[i]),cb.Bands[i].Visible);
    ini.WriteBool(aSection,Format('Band%d.Break',[i]),cb.Bands[i].Break);
  end;
end;

procedure TConfig.SetFileAssociation(aValue: Boolean);
{$IfDef Windows}
var
  a: TFileAssociation;
begin
  if fFileAssociation=aValue then
    Exit;
  fFileAssociation:=aValue;
  a:=TFileAssociation.Create(nil);
  try
    a.UnReg:=not fFileAssociation;
    a.ApplicationName:=sAppTitle;
    a.ApplicationDescription:=sDescription;
    a.Extension:='.inp';
    a.ExtensionName:='CCX Input File';
    a.ExtensionIcon:=Format('"%s",1',[Application.ExeName]);
    a.Action:=Format('"%s" "%%1"',[Application.ExeName]);
    a.ActionName:='Open';
    a.ActionIcon:=Format('"%s",0',[Application.ExeName]);
    a.RegisterForAllUsers:=true; // you can change it to False and register for current user only
    if a.Execute then
      a.ClearIconCache;
  finally
    a.Free;
  end;
end;


{$Else}
begin
  fFileAssociation:=false;
end;
{$EndIf}

procedure TConfig.UpdateConfig;
begin
  fTerminalExists:=ZFileExists(fTerminalPath);
end;

procedure TConfig.SetShowWarning(const aName: string; AValue: Boolean);
begin
  fWarnings.Values[aName]:=BoolToStr(AValue,true);
end;

function TConfig.GetShowWarning(const aName: string): Boolean;
begin
  result:=StrToBoolDef(fWarnings.Values[aName],true);
end;

end.

