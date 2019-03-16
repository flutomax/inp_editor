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

unit SynExportRTF;

{$I SynDefines.inc}

interface

uses
  Classes, LCLIntf, LCLType, Graphics, ClipBrd, SynEditExport;

type

  { TSynExporterRTF }

  TSynExporterRTF = class(TSynCustomExporter)
  private
    fAttributesChanged: boolean;
    fListColors: TList;
    function ColorToRTF(AColor: TColor): string;
    function GetColorIndex(AColor: TColor): integer;
  protected
    procedure FormatAfterLastAttribute; override;
    procedure FormatAttributeDone(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatAttributeInit(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatNewLine; override;
    function GetFooter: string; override;
    function GetFormatName: string; override;
    function GetHeader: string; override;
    procedure FormatBeforeFirstAttribute(BackgroundChanged,
      ForegroundChanged: boolean; FontStylesChanged: TFontStyles); override;
{$IFDEF SYN_MBCSSUPPORT}
    function ReplaceMBCS(Char1, Char2: char): string; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
  published
    property Color;
    property DefaultFilter;
    property Font;
    property Highlighter;
    property Title;
    property UseBackground;
  end;

implementation

uses
  SysUtils,
  SynEditStrConst;

{ TSynExporterRTF }

constructor TSynExporterRTF.Create(AOwner: TComponent);
{$ifdef windows}
const ClipboardFormat_RTF = 'Rich Text Format';
{$endif}
{$ifdef Linux}
const ClipboardFormat_RTF = 'text/richtext';
{$endif}
{$ifdef darwin}
const ClipboardFormat_RTF = 'text/rtf';
{$endif}
begin
  inherited Create(AOwner);
  fListColors := TList.Create;
  fDefaultFilter := SYNS_FilterRTF;
  fClipboardFormat := RegisterClipboardFormat(ClipboardFormat_RTF);
  // setup array of chars to be replaced
  fReplaceReserved['\'] := '\\';
  fReplaceReserved['{'] := '\{';
  fReplaceReserved['}'] := '\}';
end;

destructor TSynExporterRTF.Destroy;
begin
  fListColors.Free;
  fListColors := nil;
  inherited Destroy;
end;

procedure TSynExporterRTF.Clear;
begin
  inherited Clear;
  if Assigned(fListColors) then
    fListColors.Clear;
end;

function TSynExporterRTF.ColorToRTF(AColor: TColor): string;
var
  Col: Integer;
begin
  Col := ColorToRGB(AColor);
  Result := Format('\red%d\green%d\blue%d;', [GetRValue(Col), GetGValue(Col),
    GetBValue(Col)]);
end;

procedure TSynExporterRTF.FormatAfterLastAttribute;
begin
  // no need to reset the font style here...
end;

procedure TSynExporterRTF.FormatAttributeDone(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
const
  FontTags: array[TFontStyle] of string = ('\b0', '\i0', '\ul0', '\strike0');
var
  AStyle: TFontStyle;
begin
  // nothing to do about the color, but reset the font style
  for AStyle := Low(TFontStyle) to High(TFontStyle) do begin
    if AStyle in FontStylesChanged then begin
      fAttributesChanged := TRUE;
      AddData(FontTags[AStyle]);
    end;
  end;
end;

procedure TSynExporterRTF.FormatAttributeInit(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
const
  FontTags: array[TFontStyle] of string = ('\b', '\i', '\ul', '\strike');
var
  AStyle: TFontStyle;
begin
  // background color
  if BackgroundChanged then begin
    AddData(Format('\cb%d', [GetColorIndex(fLastBG)]));
    fAttributesChanged := TRUE;
  end;
  // text color
  if ForegroundChanged then begin
    AddData(Format('\cf%d', [GetColorIndex(fLastFG)]));
    fAttributesChanged := TRUE;
  end;
  // font styles
  for AStyle := Low(TFontStyle) to High(TFontStyle) do
    if AStyle in FontStylesChanged then begin
      AddData(FontTags[AStyle]);
      fAttributesChanged := TRUE;
    end;
  if fAttributesChanged then begin
    AddData(' ');
    fAttributesChanged := FALSE;
  end;
end;

procedure TSynExporterRTF.FormatNewLine;
begin
  AddData(#13#10'\par ');
end;

function TSynExporterRTF.GetColorIndex(AColor: TColor): integer;
begin
  Result := fListColors.IndexOf(pointer(ptruint(AColor)));
  if Result = -1 then
    Result := fListColors.Add(pointer(PtrUInt(AColor)));
end;

function TSynExporterRTF.GetFooter: string;
begin
  Result := '}';
end;

function TSynExporterRTF.GetFormatName: string;
begin
  Result := SYNS_ExporterFormatRTF;
end;

function TSynExporterRTF.GetHeader: string;
var
  i: integer;

  function GetFontTable: string;
{$IFDEF SYN_MBCSSUPPORT}
  var
    IsSpace: boolean;
{$ENDIF}
  begin
{$IFDEF SYN_MBCSSUPPORT}
    if ByteType(Font.Name, 1) <> mbSingleByte then begin
      Result := '{\fonttbl{\f0\fnil\fcharset134 ' +
        ReplaceReservedChars(Font.Name, IsSpace)
    end else
{$ENDIF}
      Result := '{\fonttbl{\f0\fmodern ' + Font.Name;
    Result := Result + ';}}'#13#10;
  end;

begin
  Result := '{\rtf1\ansi\deff0\deftab720' + GetFontTable;
  // all the colors
  Result := Result + '{\colortbl';
  for i := 0 to fListColors.Count - 1 do
    Result := Result + ColorToRTF(TColor(UIntPtr(fListColors[i])));
  Result := Result + '}'#13#10;
  // title and creator comment
  Result := Result + '{\info{\comment Generated by the SynEdit RTF ' +
    'exporter}'#13#10;
  Result := Result + '{\title ' + fTitle + '}}'#13#10;
  if fUseBackground then
    Result := Result + { TODO } #13#10;
  Result := Result + Format('\deflang1033\pard\plain\f0\fs%d ', [2 * Font.Size]);
end;

procedure TSynExporterRTF.FormatBeforeFirstAttribute(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
const
  FontTags: array[TFontStyle] of string = ('\b', '\i', '\ul', '\strike');
var
  AStyle: TFontStyle;
begin
  // background color
  if BackgroundChanged then begin
    AddData(Format('\cb%d', [GetColorIndex(fLastBG)]));
    fAttributesChanged := TRUE;
  end;
  // text color
  if ForegroundChanged then begin
    AddData(Format('\cf%d', [GetColorIndex(fLastFG)]));
    fAttributesChanged := TRUE;
  end;
  // font styles
  for AStyle := Low(TFontStyle) to High(TFontStyle) do
    if AStyle in FontStylesChanged then begin
      AddData(FontTags[AStyle]);
      fAttributesChanged := TRUE;
    end;
  if fAttributesChanged then begin
    AddData(' ');
    fAttributesChanged := FALSE;
  end;
end;

{$IFDEF SYN_MBCSSUPPORT}
function TSynExporterRTF.ReplaceMBCS(Char1, Char2: char): string;
begin
  Result := Format('\''%x\''%x ', [Char1, Char2]);
end;
{$ENDIF}

end.
