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

unit uSynEditorOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEdit, SynGutter, SynGutterBase, SynEditKeyCmds,
  SynEditMiscClasses, SynEditHighlighter, SynEditPointClasses, uDerivedClasses,
  uInpEditor;

type

  { TSynEditorOptionsStorage }

  TSynEditorOptionsStorage = class(TComponent)
  private
    fCompletionOnAsterick: Boolean;
    fHideSelection: Boolean;
    fWantTabs: Boolean;
    fMaxUndo: Integer;
    fExtraLineSpacing: Integer;
    fExtraCharSpacing: Integer;
    fTabWidth: Integer;
    fRightEdge: Integer;
    fSelectedColor: TSynSelectedColor;
    fHighlightActiveLine: Boolean;
    fHighlightActiveLineColor: TColor;
    fHighlightMatches: Boolean;
    fHighlightMatchesColor: TColor;
    fRightEdgeColor: TColor;
    fFont: TFont;
    fBookmarks: TSynBookMarkOpt;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    fScrollToEnd: Boolean;
    fOptions: TSynEditorOptions;
    fDefOptions: TSynEditorOptions;
    fOptions2: TSynEditorOptions2;
    fDefOptions2: TSynEditorOptions2;
    fMouseOptions: TSynEditorMouseOptions;
    fDefMouseOptions: TSynEditorMouseOptions;
    fWordBreakChars: string;
    fColor: TColor;
    fKeystrokes: TSynEditKeystrokes;
    fShowGutter: Boolean;
    fGutterColor: TColor;
    fGutterAutoSize: Boolean;
    fShowLineNumbers: Boolean;
    fLeadingZeros: Boolean;
    fZeroStart: Boolean;
    fShowOnlyLineNumbersMultiplesOf: Integer;
    fModifiedColor: TColor;
    fSavedColor: TColor;
    fShowChanges: Boolean;
    fShowCodeFolding: Boolean;
    procedure SetBookMarks(const Value: TSynBookMarkOpt);
    procedure SetFont(const Value: TFont);
    procedure SetKeystrokes(const Value: TSynEditKeystrokes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure LoadFromIni(IniFile: TIniFileEx; const SectionName: string);
    procedure SaveToIni(IniFile: TIniFileEx; const SectionName: string);
    procedure SetupGutter(Gutter: TSynGutter);
    procedure DataFromGutter(Gutter: TSynGutter);
  published
    property Options: TSynEditorOptions read fOptions write fOptions;
    property DefOptions: TSynEditorOptions read fDefOptions;
    property Options2: TSynEditorOptions2 read fOptions2 write fOptions2;
    property DefOptions2: TSynEditorOptions2 read fDefOptions2;
    property MouseOptions: TSynEditorMouseOptions read fMouseOptions write fMouseOptions;
    property DefMouseOptions: TSynEditorMouseOptions read fDefMouseOptions;
    property BookMarkOptions: TSynBookMarkOpt read fBookmarks write SetBookMarks;
    property Color: TColor read fColor write fColor;
    property Font: TFont read fFont write SetFont;
    property ExtraLineSpacing: Integer read fExtraLineSpacing write fExtraLineSpacing;
    property ExtraCharSpacing: Integer read fExtraCharSpacing write fExtraCharSpacing;
    property RightEdge: Integer read fRightEdge write fRightEdge;
    property RightEdgeColor: TColor read fRightEdgeColor write fRightEdgeColor;
    property WantTabs: Boolean read fWantTabs write fWantTabs;
    property InsertCaret: TSynEditCaretType read fInsertCaret write fInsertCaret;
    property OverwriteCaret: TSynEditCaretType read fOverwriteCaret write fOverwriteCaret;
    property ScrollToEnd: Boolean read fScrollToEnd write fScrollToEnd;
    property HideSelection: Boolean read fHideSelection write fHideSelection;
    property MaxUndo: Integer read fMaxUndo write fMaxUndo;
    property SelectedColor: TSynSelectedColor read fSelectedColor write fSelectedColor;
    property HighlightActiveLine: Boolean read fHighlightActiveLine write fHighlightActiveLine;
    property HighlightActiveLineColor: TColor read fHighlightActiveLineColor write fHighlightActiveLineColor;
    property HighlightMatches: Boolean read fHighlightMatches write fHighlightMatches;
    property HighlightMatchesColor: TColor read fHighlightMatchesColor write fHighlightMatchesColor;
    property TabWidth: Integer read fTabWidth write fTabWidth;
    property WordBreakChars: string read fWordBreakChars write fWordBreakChars;
    property Keystrokes: TSynEditKeystrokes read fKeystrokes write SetKeystrokes;
    property CompletionOnAsterick: Boolean read fCompletionOnAsterick write fCompletionOnAsterick;
    property ShowGutter: Boolean read fShowGutter write fShowGutter;
    property GutterColor: TColor read fGutterColor write fGutterColor;
    property GutterAutoSize: Boolean read fGutterAutoSize write fGutterAutoSize;
    property ShowLineNumbers: Boolean read fShowLineNumbers write fShowLineNumbers;
    property LeadingZeros: Boolean read fLeadingZeros write fLeadingZeros;
    property ZeroStart: Boolean read fZeroStart write fZeroStart;
    property ShowChanges: Boolean read fShowChanges write fShowChanges;
    property ShowCodeFolding: Boolean read fShowCodeFolding write fShowCodeFolding;
    property ShowOnlyLineNumbersMultiplesOf: Integer read fShowOnlyLineNumbersMultiplesOf write fShowOnlyLineNumbersMultiplesOf;
    property ModifiedColor: TColor read fModifiedColor write fModifiedColor;
    property SavedColor: TColor read fSavedColor write fSavedColor;
  end;

implementation

uses
  SynGutterLineNumber, SynGutterChanges, SynGutterCodeFolding, SynEditMouseCmds;

{ TSynEditorOptionsStorage }

constructor TSynEditorOptionsStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBookmarks:=TSynBookMarkOpt.Create(Self);
  fKeystrokes:=TSynEditKeyStrokes.Create(Self);
  //fKeystrokes.ResetDefaults;
  fSelectedColor:=TSynSelectedColor.Create;
  fSelectedColor.Foreground:=clHighlightText;
  fSelectedColor.Background:=clHighlight;
  fHighlightActiveLine:=true;
  fHighlightActiveLineColor:=$00FFE8E8;
  fHighlightMatches:=true;
  fHighlightMatchesColor:=$009BFF9B;
  fFont:=TFont.Create;
  fFont.Name:=SynDefaultFontName;
  fFont.Size:=SynDefaultFontSize;
  fFont.Height:=SynDefaultFontHeight;
  fFont.Pitch:=SynDefaultFontPitch;
  fFont.Quality:=fqDraft;
  fColor:=clWindow;
  fDefOptions:=[eoAutoIndent,eoDragDropEditing,eoDropFiles,eoScrollPastEol,
    eoShowScrollHint,eoSmartTabs,eoAltSetsColumnMode, eoTabsToSpaces,
    eoTrimTrailingSpaces, eoKeepCaretX];
  fOptions:=fDefOptions;
  fDefOptions2:=[eoFoldedCopyPaste,eoOverwriteBlock];
  fOptions2:=fDefOptions2;
  fDefMouseOptions:=[emShowCtrlMouseLinks,emCtrlWheelZoom];
  fMouseOptions:=fDefMouseOptions;
  fExtraLineSpacing:=0;
  fExtraCharSpacing:=0;
  fHideSelection:=false;
  fInsertCaret:=ctVerticalLine;
  fOverwriteCaret:=ctBlock;
  fScrollToEnd:=true;
  fMaxUndo:=32768;
  fShowGutter:=true;
  fGutterColor:=clBtnFace;
  fGutterAutoSize:=true;
  fShowLineNumbers:=true;
  fLeadingZeros:=false;
  fZeroStart:=false;
  fShowOnlyLineNumbersMultiplesOf:=5;
  fModifiedColor:=$0000E9FC;
  fSavedColor:=clGreen;
  fShowChanges:=true;
  fShowCodeFolding:=true;
  fRightEdge:=80;
  fRightEdgeColor:=clSilver;
  fTabWidth:=4;
  fWantTabs:=true;
  fCompletionOnAsterick:=true;
end;

destructor TSynEditorOptionsStorage.Destroy;
begin
  fBookMarks.Free;
  fFont.Free;
  fKeystrokes.Free;
  inherited Destroy;
end;

procedure TSynEditorOptionsStorage.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TInpEditor) then begin
    Self.Font.Assign(TInpEditor(Source).Font);
    Self.BookmarkOptions.GlyphsVisible:=TInpEditor(Source).BookmarkOptions.GlyphsVisible;
    Self.BookmarkOptions.EnableKeys:=TInpEditor(Source).BookmarkOptions.EnableKeys;
    Self.SelectedColor.Assign(TInpEditor(Source).SelectedColor);
    Self.HighlightActiveLine:=TInpEditor(Source).HighlightActiveLine;
    Self.HighlightActiveLineColor:=TInpEditor(Source).HighlightActiveLineColor;
    Self.HighlightMatches:=TInpEditor(Source).HighlightMatches;
    Self.HighlightMatchesColor:=TInpEditor(Source).HighlightMatchesColor;
    DataFromGutter(TInpEditor(Source).Gutter);

    Self.Color:=TInpEditor(Source).Color;
    Self.Options:=TInpEditor(Source).Options;
    Self.Options2:=TInpEditor(Source).Options2;
    Self.MouseOptions:=TInpEditor(Source).MouseOptions;
    Self.CompletionOnAsterick:=TInpEditor(Source).CompletionOnAsterick;
    Self.ExtraLineSpacing:=TInpEditor(Source).ExtraLineSpacing;
    Self.ExtraCharSpacing:=TInpEditor(Source).ExtraCharSpacing;
    Self.HideSelection:=TInpEditor(Source).HideSelection;
    Self.InsertCaret:=TInpEditor(Source).InsertCaret;
    Self.OverwriteCaret:=TInpEditor(Source).OverwriteCaret;
    Self.ScrollToEnd:=TInpEditor(Source).ScrollToEnd;
    //Self.MaxLineWidth:=TInpEditor(Source).MaxLineWidth;
    Self.MaxUndo:=TInpEditor(Source).MaxUndo;
    Self.RightEdge:=TInpEditor(Source).RightEdge;
    Self.RightEdgeColor:=TInpEditor(Source).RightEdgeColor;
    Self.TabWidth:=TInpEditor(Source).TabWidth;
    Self.WantTabs:=TInpEditor(Source).WantTabs;
    Self.Keystrokes.Assign(TInpEditor(Source).Keystrokes);
//!!    Self.WordBreakChars := TSynEdit(Source).WordBreakChars;
  end else
    inherited Assign(Source);
end;

procedure TSynEditorOptionsStorage.AssignTo(Dest: TPersistent);
begin
  if Assigned(Dest) and (Dest is TInpEditor) then begin
    TInpEditor(Dest).Font.Assign(Self.Font);
    TInpEditor(Dest).BookmarkOptions.GlyphsVisible:=Self.BookmarkOptions.GlyphsVisible;
    TInpEditor(Dest).BookmarkOptions.EnableKeys:=Self.BookmarkOptions.EnableKeys;
    SetupGutter(TInpEditor(Dest).Gutter);

    TInpEditor(Dest).SelectedColor.Assign(Self.SelectedColor);
    TInpEditor(Dest).HighlightActiveLine:=Self.HighlightActiveLine;
    TInpEditor(Dest).HighlightActiveLineColor:=Self.HighlightActiveLineColor;
    TInpEditor(Dest).HighlightMatches:=Self.HighlightMatches;
    TInpEditor(Dest).HighlightMatchesColor:=Self.HighlightMatchesColor;

    TInpEditor(Dest).Color:=Self.Color;
    TInpEditor(Dest).Options:=Self.Options;
    TInpEditor(Dest).Options2:=Self.Options2;
    TInpEditor(Dest).MouseOptions:=Self.MouseOptions;
    TInpEditor(Dest).CompletionOnAsterick:=Self.CompletionOnAsterick;
    TInpEditor(Dest).ExtraLineSpacing:=Self.ExtraLineSpacing;
    TInpEditor(Dest).ExtraCharSpacing:=Self.ExtraCharSpacing;
    TInpEditor(Dest).HideSelection:=Self.HideSelection;
    TInpEditor(Dest).InsertCaret:=Self.InsertCaret;
    TInpEditor(Dest).OverwriteCaret:=Self.OverwriteCaret;
    TInpEditor(Dest).ScrollToEnd:=Self.ScrollToEnd;

    TInpEditor(Dest).MaxUndo:=Self.MaxUndo;
    TInpEditor(Dest).RightEdge:=Self.RightEdge;
    TInpEditor(Dest).RightEdgeColor:=Self.RightEdgeColor;
    TInpEditor(Dest).TabWidth:=Self.TabWidth;
    TInpEditor(Dest).WantTabs:=Self.WantTabs;
    TInpEditor(Dest).Keystrokes.Assign(Self.Keystrokes);
  end else
    inherited AssignTo(Dest);
end;

procedure TSynEditorOptionsStorage.LoadFromIni(IniFile: TIniFileEx;
  const SectionName: string);
var
  fs: TFontStyles;
  so: TSynEditorOptions;
  so2: TSynEditorOptions2;
  mo: TSynEditorMouseOptions;
  //mems: TMemoryStream;
begin
  with IniFile do begin
    so:=fOptions;
    if not ReadObject(SectionName,'EditorOptions',fOptions,SizeOf(TSynEditorOptions)) then
      fOptions:=so;

    so2:=fOptions2;
    if not ReadObject(SectionName,'EditorOptions2',fOptions2,SizeOf(TSynEditorOptions2)) then
      fOptions2:=so2;

    mo:=fMouseOptions;
    if not ReadObject(SectionName,'MouseOptions',fMouseOptions,SizeOf(TSynEditorMouseOptions)) then
      fMouseOptions:=mo;

    fCompletionOnAsterick:=ReadBool(SectionName,'CompletionOnAsterick',fCompletionOnAsterick);
    fBookmarks.EnableKeys:=ReadBool(SectionName,'BookmarksEnableKeys',fBookmarks.EnableKeys);
    fBookmarks.GlyphsVisible:=ReadBool(SectionName,'BookmarksGlyphsVisible',fBookmarks.GlyphsVisible);
    fColor:=ReadColor(SectionName,'Color',fColor);
    fFont.Name:=ReadString(SectionName,'FontName',fFont.Name);
    fFont.Size:=ReadInteger(SectionName,'FontSize',fFont.Size);
    fFont.Color:=ReadColor(SectionName,'FontColor',fFont.Color);
    fs:=fFont.Style;
    if ReadObject(SectionName,'EditorFontStyle',fs,SizeOf(TFontStyles)) then
      fFont.Style:=fs;

    fSelectedColor.Background:=ReadColor(SectionName,'SelectedBackground',fSelectedColor.Background);
    fSelectedColor.Foreground:=ReadColor(SectionName,'SelectedForeground',fSelectedColor.Foreground);
    fHighlightActiveLine:=ReadBool(SectionName,'HighlightActiveLine',fHighlightActiveLine);
    fHighlightActiveLineColor:=ReadColor(SectionName,'HighlightActiveLineColor',fHighlightActiveLineColor);
    fHighlightMatches:=ReadBool(SectionName,'HighlightMatches',fHighlightMatches);
    fHighlightMatchesColor:=ReadColor(SectionName,'HighlightMatchesColor',fHighlightMatchesColor);

    fExtraLineSpacing:=ReadInteger(SectionName,'ExtraLineSpacing',fExtraLineSpacing);
    fExtraCharSpacing:=ReadInteger(SectionName,'ExtraCharSpacing',fExtraCharSpacing);
    fShowGutter:=ReadBool(SectionName,'GutterVisible',fShowGutter);
    fGutterColor:=ReadColor(SectionName,'GutterColor',fGutterColor);
    fGutterAutoSize:=ReadBool(SectionName,'GutterAutoSize',fGutterAutoSize);
    fShowLineNumbers:=ReadBool(SectionName,'GutterShowLineNumbers',fShowLineNumbers);
    fLeadingZeros:=ReadBool(SectionName,'GutterLeadingZeros',fLeadingZeros);
    fZeroStart:=ReadBool(SectionName,'GutterZeroStart',fZeroStart);
    fShowOnlyLineNumbersMultiplesOf:=ReadInteger(SectionName,'GutterShowOnlyLineNumbersMultiplesOf',fShowOnlyLineNumbersMultiplesOf);
    fModifiedColor:=ReadColor(SectionName,'GutterModifiedColor',fModifiedColor);
    fSavedColor:=ReadColor(SectionName,'GutterSavedColor',fSavedColor);
    fShowChanges:=ReadBool(SectionName,'GutterShowChanges',fShowChanges);
    fShowCodeFolding:=ReadBool(SectionName,'GutterShowCodeFolding',fShowCodeFolding);
    fRightEdge:=ReadInteger(SectionName,'RightEdge',fRightEdge);
    fRightEdgeColor:=ReadColor(SectionName,'RightEdgeColor',fRightEdgeColor);
    fInsertCaret:=TSynEditCaretType(ReadInteger(SectionName,'InsertCaret',
      Ord(fInsertCaret)));
    fOverwriteCaret:=TSynEditCaretType(ReadInteger(SectionName,'OverwriteCaret',
      Ord(fOverwriteCaret)));
    fScrollToEnd:=ReadBool(SectionName,'ScrollToEnd',fScrollToEnd);
    fTabWidth:=ReadInteger(SectionName,'TabWidth',fTabWidth);
    fMaxUndo:=ReadInteger(SectionName,'MaxUndo',fMaxUndo);

    { // unreleased feature
    mems:=TMemoryStream.Create;
    try
      if ReadMemoryStream(SectionName,'Keystrokes',mems,true) then
        fKeystrokes.LoadFromStream(mems);
    finally
      mems.Free;
    end;
    }

    if fKeystrokes.Count=0 then
      fKeystrokes.ResetDefaults;
  end;
end;

procedure TSynEditorOptionsStorage.SaveToIni(IniFile: TIniFileEx;
  const SectionName: string);
var
  fs: TFontStyles;
  //mems: TMemoryStream;
begin
  with IniFile do begin
    WriteObject(SectionName,'EditorOptions',fOptions,SizeOf(TSynEditorOptions));
    WriteObject(SectionName,'EditorOptions2',fOptions2,SizeOf(TSynEditorOptions2));
    WriteObject(SectionName,'MouseOptions',fMouseOptions,SizeOf(TSynEditorMouseOptions));
    WriteBool(SectionName,'CompletionOnAsterick',fCompletionOnAsterick);
    WriteBool(SectionName,'BookmarksEnableKeys',fBookmarks.EnableKeys);
    WriteBool(SectionName,'BookmarksGlyphsVisible',fBookmarks.GlyphsVisible);
    WriteColor(SectionName,'Color',fColor);
    WriteString(SectionName,'FontName',fFont.Name);
    WriteInteger(SectionName,'FontSize',fFont.Size);
    WriteColor(SectionName,'FontColor',fFont.Color);
    fs:=fFont.Style;
    WriteObject(SectionName,'EditorFontStyle',fs,SizeOf(TFontStyles));
    WriteColor(SectionName,'SelectedBackground',fSelectedColor.Background);
    WriteColor(SectionName,'SelectedForeground',fSelectedColor.Foreground);
    WriteBool(SectionName,'HighlightActiveLine',fHighlightActiveLine);
    WriteColor(SectionName,'HighlightActiveLineColor',fHighlightActiveLineColor);
    WriteBool(SectionName,'HighlightMatches',fHighlightMatches);
    WriteColor(SectionName,'HighlightMatchesColor',fHighlightMatchesColor);
    WriteInteger(SectionName,'ExtraLineSpacing',fExtraLineSpacing);
    WriteInteger(SectionName,'ExtraCharSpacing',fExtraCharSpacing);
    WriteBool(SectionName,'GutterVisible',fShowGutter);
    WriteColor(SectionName,'GutterColor',fGutterColor);
    WriteBool(SectionName,'GutterAutoSize',fGutterAutoSize);
    WriteBool(SectionName,'GutterShowLineNumbers',fShowLineNumbers);
    WriteBool(SectionName,'GutterLeadingZeros',fLeadingZeros);
    WriteBool(SectionName,'GutterZeroStart',fZeroStart);
    WriteInteger(SectionName,'GutterShowOnlyLineNumbersMultiplesOf',fShowOnlyLineNumbersMultiplesOf);
    WriteColor(SectionName,'GutterModifiedColor',fModifiedColor);
    WriteColor(SectionName,'GutterSavedColor',fSavedColor);
    WriteBool(SectionName,'GutterShowChanges',fShowChanges);
    WriteBool(SectionName,'GutterShowCodeFolding',fShowCodeFolding);
    WriteInteger(SectionName,'RightEdge',fRightEdge);
    WriteColor(SectionName,'RightEdgeColor',fRightEdgeColor);
    WriteInteger(SectionName,'InsertCaret',Ord(fInsertCaret));
    WriteInteger(SectionName,'OverwriteCaret',Ord(fOverwriteCaret));
    WriteBool(SectionName,'ScrollToEnd',fScrollToEnd);
    WriteInteger(SectionName,'TabWidth',fTabWidth);
    WriteInteger(SectionName,'MaxUndo',fMaxUndo);

    {// unreleased feature
    mems:=TMemoryStream.Create;
    try
      fKeystrokes.SaveToStream(mems);
      WriteMemoryStream(SectionName,'Keystrokes',mems,true);
    finally
      mems.Free;
    end;
    }
  end;
end;

procedure TSynEditorOptionsStorage.SetupGutter(Gutter: TSynGutter);
var
  i: integer;
  linenum: TSynGutterLineNumber;
  changes: TSynGutterChanges;
  codfold: TSynGutterCodeFolding;
begin
  Gutter.Visible:=fShowGutter;
  Gutter.Color:=fGutterColor;
  Gutter.AutoSize:=fGutterAutoSize;
  for i:=0 to Gutter.Parts.Count-1 do begin
    if Gutter.Parts[i] is TSynGutterLineNumber then begin
      linenum:=Gutter.Parts[i] as TSynGutterLineNumber;
      linenum.Visible:=fShowLineNumbers;
      linenum.LeadingZeros:=fLeadingZeros;
      linenum.ZeroStart:=fZeroStart;
      linenum.ShowOnlyLineNumbersMultiplesOf:=fShowOnlyLineNumbersMultiplesOf;
    end;
    if Gutter.Parts[i] is TSynGutterChanges then begin
      changes:=Gutter.Parts[i] as TSynGutterChanges;
      changes.ModifiedColor:=fModifiedColor;
      changes.SavedColor:=fSavedColor;
      changes.Visible:=fShowChanges;
    end;
    if Gutter.Parts[i] is TSynGutterCodeFolding then begin
      codfold:=Gutter.Parts[i] as TSynGutterCodeFolding;
      codfold.Visible:=fShowCodeFolding;
    end;
  end;
end;

procedure TSynEditorOptionsStorage.DataFromGutter(Gutter: TSynGutter);
var
  i: integer;
  linenum: TSynGutterLineNumber;
  changes: TSynGutterChanges;
  codfold: TSynGutterCodeFolding;
begin
  fShowGutter:=Gutter.Visible;
  fGutterColor:=Gutter.Color;
  fGutterAutoSize:=Gutter.AutoSize;
  for i:=0 to Gutter.Parts.Count-1 do begin
    if Gutter.Parts[i] is TSynGutterLineNumber then begin
      linenum:=Gutter.Parts[i] as TSynGutterLineNumber;
      fShowLineNumbers:=linenum.Visible;
      fLeadingZeros:=linenum.LeadingZeros;
      fZeroStart:=linenum.ZeroStart;
      fShowOnlyLineNumbersMultiplesOf:=linenum.ShowOnlyLineNumbersMultiplesOf;
    end;
    if Gutter.Parts[i] is TSynGutterChanges then begin
      changes:=Gutter.Parts[i] as TSynGutterChanges;
      fModifiedColor:=changes.ModifiedColor;
      fSavedColor:=changes.SavedColor;
      fShowChanges:=changes.Visible;
    end;
    if Gutter.Parts[i] is TSynGutterCodeFolding then begin
      codfold:=Gutter.Parts[i] as TSynGutterCodeFolding;
      fShowCodeFolding:=codfold.Visible;
    end;
  end;
end;

procedure TSynEditorOptionsStorage.SetBookMarks(const Value: TSynBookMarkOpt);
begin
  fBookmarks.EnableKeys:=Value.EnableKeys;
  fBookmarks.GlyphsVisible:=Value.GlyphsVisible;
end;

procedure TSynEditorOptionsStorage.SetFont(const Value: TFont);
begin
  fFont.Assign(Value);
end;

procedure TSynEditorOptionsStorage.SetKeystrokes(const Value: TSynEditKeystrokes);
begin
  fKeystrokes.Assign(Value);
end;




end.

