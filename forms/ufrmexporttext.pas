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

unit uFrmExportText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLVersion, uInpEditor;

type

  TExportFormat = (efRTF, efHTML);

  { TFrmExportText }

  TFrmExportText = class(TForm)
    btOK: TButton;
    btCancel: TButton;
    rgMethod: TGroupBox;
    rbClipboard: TRadioButton;
    rbFile: TRadioButton;
    dlgExport: TSaveDialog;
  private

  public

  end;

var
  FrmExportText: TFrmExportText;

  function ShowExportDlg(Editor: TInpEditor; const fmt: TExportFormat): Boolean;

implementation

{$R *.lfm}

uses
  {$IF LCL_FULLVERSION<1080000}LCLType, LCLIntf,{$ENDIF}
  StrUtils, SynEditExport, SynExportRTF, SynExportHTML, uConsts, uFrmMain,
  uSysInfo;

type

  { TExporterHTML }

  TExporterHTML = class(TSynExporterHTML)
  protected
    function GetHeader: string; override;
  end;

{$IF LCL_FULLVERSION<1080000}
function ColorToHTML(AColor: TColor): string;
var
  RGBColor: TColorRef;
  RGBValue: Byte;
const
  Digits: array[0..15] of char = '0123456789ABCDEF';
begin
  Result:='';
  case AColor of
    clRed:     Result:='red';
    clGreen:   Result:='green';
    clBlue:    Result:='blue';
    clPurple:  Result:='purple';
    clYellow:  Result:='yellow';
    clBlack:   Result:='black';
    clWhite:   Result:='white';
    clGray:    Result:='gray';
    clMaroon:  Result:='maroon';
    clFuchsia: Result:='fuchsia';
    clLime:    Result:='lime';
    clNavy:    Result:='navy';
    clAqua:    Result:='aqua';
    clTeal:    Result:='teal';
    clSilver:  Result:='silver';
  end;
  if Result<>'' then
    Exit;
  RGBColor:=ColorToRGB(AColor);
  Result:='#000000';
  RGBValue:=GetRValue(RGBColor);
  if RGBValue>0 then begin
    Result[2]:=Digits[RGBValue shr  4];
    Result[3]:=Digits[RGBValue and 15];
  end;
  RGBValue:=GetGValue(RGBColor);
  if RGBValue>0 then begin
    Result[4]:=Digits[RGBValue shr  4];
    Result[5]:=Digits[RGBValue and 15];
  end;
  RGBValue:=GetBValue(RGBColor);
  if RGBValue>0 then begin
    Result[6]:=Digits[RGBValue shr  4];
    Result[7]:=Digits[RGBValue and 15];
  end;
end;
{$ENDIF}


  { TExporterHTML }

function TExporterHTML.GetHeader: string;
begin
  Result:='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"'+LineEnding+
          '"http://www.w3.org/TR/html4/loose.dtd">'+LineEnding;
  Result:=Result+'<html>'+LineEnding+'<head>'+LineEnding;
  Result:=Result+Format('<meta name="generator" content="%s %s">',
    [sAppTitle,GetProgramVersion])+LineEnding;
  Result:=Result+'<meta http-equiv="content-type" content="text/html; charset=utf-8">'+LineEnding;
  Result:=Result+Format('<title>%s</title>',[Title])+LineEnding;
  Result:=Result+'</head>'+LineEnding;
  Result:=Result+Format('<body text="%s" bgcolor="%s">',
    [ColorToHtml(fFont.Color),ColorToHTML(fBackgroundColor)])+LineEnding;
  Result:=Result+'<pre><code>';
  Result:=Result+Format('<font size="%d" face="%s">',[1+Ord(fs02),fFont.Name]);
end;

const

  S_FMT: array[TExportFormat] of string =('RTF','HTML');
  S_EXT: array[TExportFormat] of string =('.rtf','.html');
  S_FLT: array[TExportFormat] of string =(
    'Rich Text Format file (*.rtf)|*.rtf|All Files (*.*)|*.*',
    'Hyper Text Markup Language file (*.htm;*.html)|*.htm;*.html|All Files (*.*)|*.*');

procedure ExportTo(Editor: TInpEditor; const fmt: TExportFormat;
  const FileName: string = '');
var
  e: TSynCustomExporter;
begin
  Screen.Cursor:=crHourGlass;
  try
    case fmt of
      efRTF:  e:=TSynExporterRTF.Create(nil);
      efHTML: e:=TExporterHTML.Create(nil);
    end;
    try
      e.Highlighter:=Editor.Highlighter;
      e.Title:=ChangeFileExt(ExtractFileName(Editor.FileName),'');
      e.ExportAll(Editor.Lines);
      if FileName='' then
        e.CopyToClipboard
      else
        e.SaveToFile(FileName);
    finally
      e.Free;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

function ShowExportDlg(Editor: TInpEditor; const fmt: TExportFormat): Boolean;
begin
  with TFrmExportText.Create(Application) do
    try
      Caption:=Format('Fxport To %s',[S_FMT[fmt]]);
      rbFile.Checked:=FrmMain.Config.ExportAsFile;
      result:=ShowModal=mrOK;
      if result then begin
        FrmMain.Config.ExportAsFile:=rbFile.Checked;
        if rbFile.Checked then begin
          dlgExport.Filter:=S_FLT[fmt];
          dlgExport.DefaultExt:=S_EXT[fmt];
          dlgExport.FileName:=ChangeFileExt(ExtractFileName(Editor.FileName),'');
          result:=dlgExport.Execute;
          Application.ProcessMessages;
        end;
        if result then
          ExportTo(Editor,fmt,dlgExport.FileName);
      end;
    finally
      Release;
    end;
end;



end.

