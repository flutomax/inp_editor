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
    fIni: TIniFileEx;
    fIniPath: string;
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
    procedure LoadFromIni(const aSection: string);
    procedure SaveToIni(const aSection: string);
    procedure LoadCoolBar(const aSection: string; cb: TCoolBar);
    procedure SaveCoolBar(const aSection: string; cb: TCoolBar);
    procedure LoadFormLayout(aForm: TForm; const aSection: string);
    procedure SaveFormLayout(aForm: TForm; const aSection: string);
    procedure UpdateConfig;
    property Ini: TIniFileEx read fIni;
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
  fIniPath:=ChangeFileExt(Application.ExeName,'.ini');
  fIni:=TIniFileEx.Create(fIniPath);
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
  FreeAndNil(fIni);
  inherited Destroy;
end;

procedure TConfig.MaximizeMainForm(Data: PtrInt);
begin
  if Assigned(fMainForm) then begin
    fMainForm.WindowState:=wsMaximized;
    fMainForm.Visible:=true;
  end;
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

procedure TConfig.LoadFromIni(const aSection: string);
begin
  fTemplatesPath:=fIni.ReadString(aSection,'TemplatesPath',fDefTemplatesPath);
  fIni.ReadStringList(aSection,'TemplatesPaths',fTemplatesPaths);
  fCCXPath:=fIni.ReadString(aSection,'CCXPath',fCCXPath);
  fIni.ReadStringList(aSection,'CCXPaths',fCCXPaths);
  fCGXPath:=fIni.ReadString(aSection,'CGXPath',fCGXPath);
  fIni.ReadStringList(aSection,'CGXPaths',fCGXPaths);
  fCGXPreFlag:=fIni.ReadString(aSection,'CGXPreFlag',fCGXPreFlag);
  {$IfNDef Windows}
    fTerminalPath:=fIni.ReadString(aSection,'TerminalPath',fTerminalPath);
    fIni.ReadStringList(aSection,'TerminalPaths',fTerminalPaths);
  {$EndIf}
  fMonitorScanTime:=EnsureRange(fIni.ReadInteger('Monitor','ScanInterval',3),1,5);
  fMonitorShowLegend:=fIni.ReadBool('Monitor','ShowLegend',true);
  fIni.ReadStringList(aSection,'Warnings',fWarnings);
  fBCAutoInsert:=fIni.ReadBool(aSection,'BCAutoInsert',fBCAutoInsert);
  fBCAddComment:=fIni.ReadBool(aSection,'BCAddComment',fBCAddComment);
  fBCMakeInclude:=fIni.ReadBool(aSection,'BCMakeInclude',fBCMakeInclude);
  fShowHints:=fIni.ReadBool(aSection,'ShowHints',fShowHints);
  fStoreFormPlacement:=fIni.ReadBool(aSection,'StoreFormPlacement',fStoreFormPlacement);
  fUniqueInstance:=fIni.ReadBool(aSection,'UniqueInstance',fUniqueInstance);
  fFileAssociation:=fIni.ReadBool(aSection,'FileAssociation',fFileAssociation);
  fExportAsFile:=fIni.ReadBool(aSection,'ExportAsFile',fExportAsFile);
  fStatusbarVisible:=fIni.ReadBool(aSection,'StatusbarVisible',fStatusbarVisible);
  if fStoreFormPlacement and Assigned(fMainForm) then
    LoadFormLayout(fMainForm, aSection);
  UpdateConfig;
end;

procedure TConfig.SaveToIni(const aSection: string);
begin
  fIni.WriteString(aSection,'TemplatesPath',fTemplatesPath);
  fIni.WriteStringList(aSection,'TemplatesPaths',fTemplatesPaths);
  fIni.WriteString(aSection,'CCXPath',fCCXPath);
  fIni.WriteStringList(aSection,'CCXPaths',fCCXPaths);
  fIni.WriteString(aSection,'CGXPath',fCGXPath);
  fIni.WriteStringList(aSection,'CGXPaths',fCGXPaths);
  fIni.WriteString(aSection,'CGXPreFlag',fCGXPreFlag);
  {$IfNDef Windows}
    fIni.WriteString(aSection,'TerminalPath',fTerminalPath);
    fIni.WriteStringList(aSection,'TerminalPaths',fTerminalPaths);
  {$EndIf}
  fIni.WriteInteger('Monitor','ScanInterval',fMonitorScanTime);
  fIni.WriteBool('Monitor','ShowLegend',fMonitorShowLegend);
  fIni.WriteStringList(aSection,'Warnings',fWarnings);
  fIni.WriteBool(aSection,'ShowHints',fShowHints);
  fIni.WriteBool(aSection,'BCAutoInsert',fBCAutoInsert);
  fIni.WriteBool(aSection,'BCAddComment',fBCAddComment);
  fIni.WriteBool(aSection,'BCMakeInclude',fBCMakeInclude);
  fIni.WriteBool(aSection,'StoreFormPlacement',fStoreFormPlacement);
  fIni.WriteBool(aSection,'UniqueInstance',fUniqueInstance);
  fIni.WriteBool(aSection,'FileAssociation',fFileAssociation);
  fIni.WriteBool(aSection,'ExportAsFile',fExportAsFile);
  fIni.WriteBool(aSection,'StatusbarVisible',fStatusbarVisible);
  if fStoreFormPlacement and Assigned(fMainForm) then
    SaveFormLayout(fMainForm,aSection);
end;

procedure TConfig.LoadCoolBar(const aSection: string;
  cb: TCoolBar);
var
  i,j,p: integer;
begin
  cb.DisableAutoSizing;
  try
    for i:=0 to cb.Bands.Count-1 do begin
      p:=fIni.ReadInteger(aSection,Format('Band%d.Position',[i]),i);
      if p<>cb.Bands[i].ID then
        for j:=i+1 to cb.Bands.Count-1 do
          if cb.Bands[j].ID=p then begin
            cb.Bands[j].Index:=i;
            break;
          end;
    end;
    for i:=0 to cb.Bands.Count-1 do begin
      cb.Bands[i].Break:=fIni.ReadBool(aSection,Format('Band%d.Break',[i]),false);
      cb.Bands[i].Visible:=fIni.ReadBool(aSection,Format('Band%d.Visible',[i]),true);
    end;

  finally
    cb.EnableAutoSizing;
  end;
end;

procedure TConfig.SaveCoolBar(const aSection: string; cb: TCoolBar);
var
  i: integer;
begin
  for i:=0 to cb.Bands.Count-1 do begin
    fIni.WriteInteger(aSection,Format('Band%d.Position',[i]),cb.Bands[i].ID);
    fIni.WriteBool(aSection,Format('Band%d.Visible',[i]),cb.Bands[i].Visible);
    fIni.WriteBool(aSection,Format('Band%d.Break',[i]),cb.Bands[i].Break);
  end;
end;

procedure TConfig.LoadFormLayout(aForm: TForm; const aSection: string);
var
  ws: TWindowState;
begin
  with aForm do begin
    if not (fsModal in FormState) then
      Visible:=false;
    ws:=TWindowState(fIni.ReadInteger(aSection,'WindowState',Ord(WindowState)));
    if ws=wsMaximized then begin
      WindowState:=wsNormal;
      BoundsRect:=Bounds(
        fIni.ReadInteger(aSection,'RestoredLeft',RestoredLeft),
        fIni.ReadInteger(aSection,'RestoredTop',RestoredTop),
        fIni.ReadInteger(aSection,'RestoredWidth',RestoredWidth),
        fIni.ReadInteger(aSection,'RestoredHeight',RestoredHeight));
      Application.QueueAsyncCall(@MaximizeMainForm,0);
    end else begin
      WindowState:=wsNormal;
      BoundsRect:=Bounds(
        fIni.ReadInteger(aSection,'NormalLeft',Left),
        fIni.ReadInteger(aSection,'NormalTop',Top),
        fIni.ReadInteger(aSection,'NormalWidth',Width),
        fIni.ReadInteger(aSection,'NormalHeight',Height));
    if not (fsModal in FormState) then
       Visible:=true;
    end;
  end;
end;

procedure TConfig.SaveFormLayout(aForm: TForm; const aSection: string);
begin
  with aForm do begin
    fIni.WriteInteger(aSection,'NormalLeft',Left);
    fIni.WriteInteger(aSection,'NormalTop',Top);
    fIni.WriteInteger(aSection,'NormalWidth',Width);
    fIni.WriteInteger(aSection,'NormalHeight',Height);
    fIni.WriteInteger(aSection,'RestoredLeft',RestoredLeft);
    fIni.WriteInteger(aSection,'RestoredTop',RestoredTop);
    fIni.WriteInteger(aSection,'RestoredWidth',RestoredWidth);
    fIni.WriteInteger(aSection,'RestoredHeight',RestoredHeight);
    fIni.WriteInteger(aSection,'WindowState',Ord(WindowState));
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

