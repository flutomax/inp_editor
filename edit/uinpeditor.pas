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

unit uInpEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, ComCtrls, Forms, Graphics, Dialogs,
  LCLType, LCLVersion, ExtendedNotebook, SynEdit, SynEditTypes, SynEditMarks,
  SynGutterBase, SynPluginMultiCaret, SynPluginSyncroEdit, SynEditHighlighter,
  SynCompletion, uHighliter, uFileWatch, uDerivedClasses, uEncodingFunc;

type

  EInpEditor = class(Exception);

  TInpEditTabSheet = class;
  TInpEditZoomMode = (zmReset, zmIn, zmOut);

  TTextLevel = (tlSelection, tlLines, tlFullText);
  TTextLevels = set of TTextLevel;

const
  DefaultLevels = [tlSelection, tlLines];

type

  TTextOperation = function(const Text: string): string;

  { TInpEditor }

  TInpEditor = class(TSynEdit)
  private
    fUnnamed: boolean;
    fFileName: TFilename;
    fSheet: TInpEditTabSheet;
    fCaretPos: TPoint;
    fSyncEdit: TSynPluginSyncroEdit;
    fMultiCaret: TSynPluginMultiCaret;
    fWatch: TWatchNotifyer;
    fCompletionOnAsterick: boolean;
    fHighlightActiveLine: boolean;
    fHighlightActiveLineColor: TColor;
    fHighlightMatches: boolean;
    fHighlightMatchesColor: TColor;
    fEncoding: TStreamEncoding;
    function GetLastPt: TPoint;
    function GetCharCursor: char;
    function GetLineBreakStyle: TTextLineBreakStyle;
    function GetWordAtCursor: string;
    procedure SetFileName(const aValue: TFileName);
    procedure SetHighlightActiveLine(AValue: boolean);
    procedure SetHighlightActiveLineColor(AValue: TColor);
    procedure SetHighlightMatches(AValue: boolean);
    procedure SetHighlightMatchesColor(AValue: TColor);
    procedure SetTextLineBreakStyle(AValue: TTextLineBreakStyle);
    procedure SetSheet(const aValue: TInpEditTabSheet);
    procedure SetUnnamed(const aValue: boolean);
    procedure DoCompletionOnAsterick(Data: PtrInt);
    procedure SetLineText(Index: integer; NewText: string);
    procedure SetText(NewText: string);
    procedure UpdateHighlightMatches;
    procedure UpdateHighlightActiveLine;
    procedure DoClickLink(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DoMouseLink(Sender: TObject; X, Y: integer;
      var AllowMouseLink: boolean);
  protected
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
    procedure DoOnStatusChange(Changes: TSynStatusChanges); override;
    procedure SetHighlighter(const Value: TSynCustomHighlighter); override;
    property Watch: TWatchNotifyer read fWatch write fWatch;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open(const aFileName: TFileName);
    procedure OpenFileAtCursor;
    procedure Reload;
    procedure StoreCaretPos;
    procedure RestoreCaretPos;
    procedure FoldCurrent;
    procedure UnfoldCurrent;
    procedure Zoom(const Mode: TInpEditZoomMode);
    procedure TextOperation(Operation: TTextOperation;
      const Level: TTextLevels = DefaultLevels);
    procedure TextToNewLine(const aText: string);
    procedure SplitLines;
    procedure JoinLines;
    procedure DuplicateLine;
    procedure InsertLine(const aText: string; const aLine: integer;
      const aCol: integer = 0);
    procedure InsertLineHere(const aText: string);
    procedure RemoveEmptyLines;
    procedure ClearBookmarks;
    procedure CaretToEOF;
    function CanZoomIn: boolean;
    function CanZoomOut: boolean;
    function CanOpenFileAtCursor: boolean;
    function Save: boolean;
    function SaveAs(const aFileName: TFileName): boolean;
    property LastPt: TPoint read GetLastPt;
    property WordAtCursor: string read GetWordAtCursor;
    property Encoding: TStreamEncoding read fEncoding;
  published
    property FileName: TFileName read fFileName write SetFileName;
    property Unnamed: boolean read fUnnamed write SetUnnamed;
    property Sheet: TInpEditTabSheet read fSheet write SetSheet;
    property CompletionOnAsterick: boolean read fCompletionOnAsterick
      write fCompletionOnAsterick;
    property TextLineBreakStyle: TTextLineBreakStyle
      read GetLineBreakStyle write SetTextLineBreakStyle;
    property HighlightActiveLine: boolean read fHighlightActiveLine
      write SetHighlightActiveLine;
    property HighlightActiveLineColor: TColor
      read fHighlightActiveLineColor write SetHighlightActiveLineColor;
    property HighlightMatches: boolean read fHighlightMatches write SetHighlightMatches;
    property HighlightMatchesColor: TColor read fHighlightMatchesColor
      write SetHighlightMatchesColor;
  end;

  { TInpEditTabSheet }

  TInpEditTabSheet = class(TTabSheet)
  private
    fEditor: TInpEditor;
    function GetTitle: string;
    procedure SetTitle(AValue: string);
  public
    property Editor: TInpEditor read fEditor;
    property Title: string read GetTitle write SetTitle;
  end;

  TOnBeforeClose = procedure(Editor: TInpEditor; var Cancel: boolean) of object;

  { TInpEditPager }

  TInpEditPager = class(TExtendedNotebook)
  private
    fWatch: TWatchNotifyer;
    fHighliter: TInpHighlighter;
    fCompletion: TSynCompletion;
    fTabCloseBtnVisible: boolean;
    fCloseTabIndex: integer;
    fOnBeforeClose: TOnBeforeClose;
    fOnStatusChange: TStatusChangeEvent;
    function GetActiveEditor: TInpEditor;
    function EditorFromIndex(const Index: integer): TInpEditor;
    function IndexFromFilename(const aFileName: TFileName): integer;
    function IndexOfSpareSheet: integer;
    function CreateFile(const aFileName: TFileName): boolean;
    procedure SetTabCloseBtnVisible(AValue: boolean);
    procedure WatchFileChange(Sender: TObject; FileName: TFileName;
      Data: Pointer; State: TWatchStateChange);
    procedure UpdateEditorOptions(Editor: TInpEditor);
    procedure CompletionCodeCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure CompletionExecute(Sender: TObject);
    procedure CompletionSearchPosition(var APosition: integer);
    function CompletionMeasureItem(const AKey: string; ACanvas: TCanvas;
      Selected: boolean; Index: integer): TPoint;
    function CompletionPaintItem(const AKey: string; ACanvas: TCanvas;
      X, Y: integer; Selected: boolean; Index: integer): boolean;
  protected
    {$IfDef Windows}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure PaintWindow(DC: HDC); override;
    procedure DoChange; override;
    {$EndIf}
    procedure DoCloseTabClicked(APage: TCustomPage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open(const aFileName: TFilename = '';
      const FromImport: boolean = False): TInpEditor;
    function Close(Editor: TInpEditor; Force: boolean = False): boolean;
    function CloseAll: boolean;
    function SaveAll: boolean;
    procedure CheckFileChanges;
    procedure SaveHighlighterToIni(Ini: TIniFileEx; const aSection: string);
    procedure LoadHighlighterFromIni(Ini: TIniFileEx; const aSection: string);
    procedure UpdateOptions;
    property Highliter: TInpHighlighter read fHighliter;
  published
    property ActiveEditor: TInpEditor read GetActiveEditor;
    property TabCloseBtnVisible: boolean read fTabCloseBtnVisible
      write SetTabCloseBtnVisible;
    property OnBeforeClose: TOnBeforeClose read fOnBeforeClose write fOnBeforeClose;
    property OnStatusChange: TStatusChangeEvent
      read fOnStatusChange write fOnStatusChange;
  end;

implementation

uses
  Math, StrUtils, LazFileUtils, SynEditKeyCmds, SynEditMouseCmds, SynExportRTF,
  SynEditHighlighterFoldBase, SynEditMarkupWordGroup, InterfaceBase, LCLIntf,
  uConsts, uFrmMain, uEditorMisc, uFileUtils {$IfDef Windows}, Themes{$EndIf};

const
  BUTTON_SPACES = '     ';

var
  UnnamedCount: integer = 0;

{ TInpEditTabSheet }

function TInpEditTabSheet.GetTitle: string;
begin
  {$IfDef Windows}
  GetTitle := TrimRight(Caption);
  {$Else}
  GetTitle := Caption;
  {$EndIf}
end;

procedure TInpEditTabSheet.SetTitle(AValue: string);
begin
  {$IfDef Windows}
  if TInpEditPager(PageControl).TabCloseBtnVisible then
    Caption := TrimRight(AValue) + BUTTON_SPACES
  else
    Caption := AValue;
  {$Else}
  Caption := AValue;
  {$EndIf}
end;

{ TInpEditor }

constructor TInpEditor.Create(AOwner: TComponent);
var
  b: TBitmap;
  i: integer;
  ma: TSynEditMouseAction;
begin
  inherited Create(AOwner);
  Options := Options + [eoAltSetsColumnMode];
  fHighlightMatches := True;
  fHighlightActiveLine := True;
  fCompletionOnAsterick := True;

  fMultiCaret := TSynPluginMultiCaret.Create(Self);
  fMultiCaret.EnableWithColumnSelection := True;
  fMultiCaret.DefaultMode := mcmMoveAllCarets;
  fMultiCaret.DefaultColumnSelectMode := mcmCancelOnCaretMove;
  MouseOptions := [emShowCtrlMouseLinks, emCtrlWheelZoom];
  MouseLinkColor.Style := [fsUnderline];
  ma := MouseActions.Add;
  ma.Command := emcMouseLink;
  ma.Shift := [ssCtrl];
  ma.ShiftMask := [ssCtrl];
  OnMouseLink := @DoMouseLink;
  OnClickLink := @DoClickLink;

  b := TBitmap.Create;
  try
    FrmMain.IlMain.GetBitmap(30, b);
    fSyncEdit := TSynPluginSyncroEdit.Create(Self);
    fSyncEdit.Editor := Self;
    fSyncEdit.GutterGlyph.Assign(b);
    fSyncEdit.CaseSensitive := False;
  finally
    b.Free;
  end;

  BookMarkOptions.BookmarkImages := FrmMain.IlBookmark;
  PopupMenu := FrmMain.PmEditor;

  // !!! bugfix dwawing red frame in MarkupWordGroup
  for i := 0 to MarkupManager.Count - 1 do
    if MarkupManager.Markup[i] is TSynEditMarkupWordGroup then
      MarkupManager.Markup[i].MarkupInfo.FrameColor := clNone;

  fHighlightMatchesColor := $009BFF9B;
  HighlightAllColor.Background := fHighlightMatchesColor;
  HighlightAllColor.Foreground := clBlack;

  fHighlightActiveLineColor := $00FFE8E8;
  LineHighlightColor.Background := fHighlightActiveLineColor;
end;

destructor TInpEditor.Destroy;
begin
  FreeAndNil(fSyncEdit);
  FreeAndNil(fMultiCaret);
  inherited Destroy;
end;

procedure TInpEditor.StoreCaretPos;
begin
  fCaretPos := CaretXY;
end;

procedure TInpEditor.RestoreCaretPos;
begin
  CaretXY := fCaretPos;
end;

procedure TInpEditor.SetFileName(const aValue: TFileName);
var
  s: string;
begin
  if fFileName = AValue then
    Exit;
  fFileName := AValue;
  fUnnamed := fFileName = '';
  if not fUnnamed then
    fSheet.Title := ExtractFileName(fFileName);
end;

procedure TInpEditor.UpdateHighlightActiveLine;
begin
  if fHighlightActiveLine then
    LineHighlightColor.Background := fHighlightActiveLineColor
  else
    LineHighlightColor.Background := clNone;
end;

procedure TInpEditor.DoClickLink(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  s: string;
begin
  s := GetWordAtRowCol(PixelsToRowColumn(Point(X, Y)));
  s := CreateAbsolutePath(s, ExtractFileDir(FileName));
  if ZFileExists(s) then
    FrmMain.Open(s)
  else
    MessageDlg(Format(sFileNotFound, [s]), mtWarning, [mbOK], 0);
end;

procedure TInpEditor.DoMouseLink(Sender: TObject; X, Y: integer;
  var AllowMouseLink: boolean);
var
  s: string;
  Attr: TSynHighlighterAttributes;
begin
  GetHighlighterAttriAtRowCol(Point(X, Y), s, Attr);
  AllowMouseLink := Assigned(Attr) and (Attr.StoredName = SYNS_AttrIncludeFName);
end;

procedure TInpEditor.SetHighlightActiveLine(AValue: boolean);
begin
  if fHighlightActiveLine = AValue then
    Exit;
  fHighlightActiveLine := AValue;
  UpdateHighlightActiveLine;
end;

procedure TInpEditor.SetHighlightActiveLineColor(AValue: TColor);
begin
  if fHighlightActiveLineColor = AValue then
    Exit;
  fHighlightActiveLineColor := AValue;
  UpdateHighlightActiveLine;
end;

procedure TInpEditor.UpdateHighlightMatches;
begin
  if fHighlightMatches then
  begin
    HighlightAllColor.Background := fHighlightMatchesColor;
    HighlightAllColor.Foreground := clBlack;
  end
  else
  begin
    HighlightAllColor.Background := clNone;
    HighlightAllColor.Foreground := clNone;
  end;
end;

procedure TInpEditor.SetHighlightMatches(AValue: boolean);
begin
  if fHighlightMatches = AValue then
    Exit;
  fHighlightMatches := AValue;
  UpdateHighlightMatches;
end;

procedure TInpEditor.SetHighlightMatchesColor(AValue: TColor);
begin
  if fHighlightMatchesColor = AValue then
    Exit;
  fHighlightMatchesColor := AValue;
  UpdateHighlightMatches;
end;

procedure TInpEditor.SetTextLineBreakStyle(AValue: TTextLineBreakStyle);
begin
  if Lines.TextLineBreakStyle = AValue then
    Exit;
  Lines.TextLineBreakStyle := AValue;
  Modified := True;
end;

function TInpEditor.GetLineBreakStyle: TTextLineBreakStyle;
begin
  Result := Lines.TextLineBreakStyle;
end;

function TInpEditor.GetCharCursor: char;
var
  Line: string;
  x: integer;
begin
  Line := MarkupManager.Lines[CaretXY.Y - 1];
  x := CaretXY.x - 1;
  if (Length(Line) >= x) and (x > 0) then
    Result := Line[x]
  else
    Result := #0;
end;

function TInpEditor.GetWordAtCursor: string;
begin
  Result := GetWordAtRowCol(CaretXY);
end;

function TInpEditor.GetLastPt: TPoint;
begin
  Result.Y := MarkupManager.Lines.Count;
  Result.X := Length(MarkupManager.Lines[Result.Y - 1]) + 1;
end;

procedure TInpEditor.SetSheet(const aValue: TInpEditTabSheet);
begin
  if fSheet = aValue then
    Exit;
  fSheet := aValue;
  if Assigned(fSheet) then
    fSheet.fEditor := self;
end;

procedure TInpEditor.SetUnnamed(const aValue: boolean);
begin
  if fUnnamed = aValue then
    Exit;
  fUnnamed := aValue;
  if fUnnamed then
    fFileName := '';
end;

procedure TInpEditor.DoCompletionOnAsterick(Data: PtrInt);
var
  key: word;
begin
  key := 32;
  KeyDown(key, [ssCtrl]);
end;

procedure TInpEditor.UTF8KeyPress(var Key: TUTF8Char);
begin
  if fCompletionOnAsterick and (Key = '*') then
    Application.QueueAsyncCall(@DoCompletionOnAsterick, 0);
  if (GetCharCursor = '*') then
  begin
    Application.ProcessMessages;
  end;

  inherited UTF8KeyPress(Key);
end;

procedure TInpEditor.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  if fHighlightMatches and (scSelection in Changes) then
    SetHighlightSearch(SelText, [ssoWholeWord]);

  inherited DoOnStatusChange(Changes);
end;

procedure TInpEditor.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  inherited SetHighlighter(Value);
  if Value is TInpHighlighter then
  begin
    MouseLinkColor.Foreground := TInpHighlighter(Value).IncludeFNAttri.Foreground;
    MouseLinkColor.Background := TInpHighlighter(Value).IncludeFNAttri.Background;
  end;
end;

procedure TInpEditor.Open(const aFileName: TFileName);
var
  fs: TFileStreamEx;
begin
  fs := TFileStreamEx.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    // correct encoding if needed
    fEncoding := AutoDetectEncoding(fs);
    StreamToUTF8Lines(fs, fEncoding, Lines);
  finally
    fs.Free;
  end;
  SetFileName(aFileName);
  CaretToEOF;
end;

procedure TInpEditor.OpenFileAtCursor;
var
  s: string;
  Attr: TSynHighlighterAttributes;
begin
  GetHighlighterAttriAtRowCol(CaretXY, s, Attr);
  if (Attr = nil) or (Attr.StoredName <> SYNS_AttrIncludeFName) then
    Exit;
  s := GetWordAtRowCol(CaretXY);
  s := CreateAbsolutePath(s, ExtractFileDir(FileName));
  if ZFileExists(s) then
    TInpEditPager(fSheet.PageControl).Open(s)
  else
    MessageDlg(Format(sFileNotFound, [s]), mtWarning, [mbOK], 0);
end;

function TInpEditor.CanOpenFileAtCursor: boolean;
var
  s: string;
  Attr: TSynHighlighterAttributes;
begin
  GetHighlighterAttriAtRowCol(CaretXY, s, Attr);
  Result := Assigned(Attr) and (Attr.StoredName = SYNS_AttrIncludeFName);
end;

procedure TInpEditor.Reload;
begin
  StoreCaretPos;
  Open(FileName);
  Modified := False;
  RestoreCaretPos;
end;

function TInpEditor.Save: boolean;
begin
  Result := SaveAs(fFileName);
  Watch.Update(fFileName);
end;

function TInpEditor.SaveAs(const aFileName: TFileName): boolean;
var
  r: boolean;
  fs: TFileStreamEx;
begin
  repeat
    r := False;
    try
      if fFileName <> '' then
        Watch.RemoveFile(fFileName);
      fs := TFileStreamEx.Create(aFileName, fmCreate);
      try
        Lines.SaveToStream(fs);
      finally
        fs.Free;
      end;
      SetFileName(aFileName);
      Watch.AddFile(aFileName, Self);
      Modified := False;
      Unnamed := False;
      Result := True;
    except
      Result := False;
    end;
    if not Result then
    begin
      case MessageDlg(Format(sCannotSave, [aFileName]), mtError, mbAbortRetryIgnore, 0) of
        mrAbort: Exit;
        mrIgnore: Exit(True);
        mrRetry: r := True;
      end;
    end;
  until not r;
end;

procedure TInpEditor.FoldCurrent;
begin
  ExecuteCommand(EcFoldCurrent, #0, nil);
end;

procedure TInpEditor.UnfoldCurrent;
begin
  ExecuteCommand(EcUnFoldCurrent, #0, nil);
end;

procedure TInpEditor.Zoom(const Mode: TInpEditZoomMode);
begin
  case Mode of
    zmIn: ExecuteCommand(ecZoomIn, #0, nil);
    zmOut: ExecuteCommand(ecZoomOut, #0, nil);
    zmReset: ExecuteCommand(ecZoomNorm, #0, nil);
  end;
end;

function TInpEditor.CanZoomIn: boolean;
begin
  Result := abs(Font.Height) < 50;
end;

function TInpEditor.CanZoomOut: boolean;
begin
  Result := abs(Font.Height) > 3;
end;

procedure TInpEditor.SetLineText(Index: integer; NewText: string);
begin
  TextBetweenPoints[Point(1, Index + 1),
    PhysicalToLogicalPos(Point(Length(Lines[Index]) + 1, Index + 1))] := NewText;
end;

procedure TInpEditor.SetText(NewText: string);
begin
  TextBetweenPoints[Point(1, 1),
    PhysicalToLogicalPos(Point(Length(Lines[Lines.Count - 1]) + 1, Lines.Count))] :=
    NewText;
end;

procedure TInpEditor.TextOperation(Operation: TTextOperation; const Level: TTextLevels);
var
  i: integer;
  t: TStringList;
begin
  if (tlSelection in Level) and SelAvail then
  begin
    t := TStringList.Create;
    t.Text := SelText;
    for i := 0 to t.Count - 1 do
      t[i] := Operation(t[i]);
    SelText := Copy(t.Text, 1, Length(t.Text) - Length(LineEnding));
  end
  else
  if (tlLines in Level) then
  begin
    BeginUpdate(True);
    try
      for i := 0 to Lines.Count - 1 do
        SetLineText(i, Operation(Lines[i]));
    finally
      EndUpdate;
    end;
  end
  else
  begin
    BeginUpdate(True);
    try
      SetText(Operation(Text));
    finally
      EndUpdate;
    end;
  end;
end;

procedure TInpEditor.TextToNewLine(const aText: string);
begin
  CaretX := 0;
  SelText := aText;
end;

procedure TInpEditor.SplitLines;
begin
  if not SelAvail then
    SelectLine;
  SelText := WrapToList(SelText, RightEdge);
  CaretX := 1;
end;

procedure TInpEditor.JoinLines;
begin
  SelText := JoinText(SelText);
end;

procedure TInpEditor.DuplicateLine;
var
  ss: integer;
  s: string;
begin
  if not SelAvail then
    SelectLine;
  ss := self.SelStart;
  s := SelText + LineEnding;
  SelStart := ss;
  SelEnd := ss;
  TextToNewLine(s);
end;

procedure TInpEditor.InsertLine(const aText: string; const aLine: integer;
  const aCol: integer);
begin
  CaretX := aCol;
  CaretY := aLine;
  SelText := aText + Lines.LineBreak;
end;

procedure TInpEditor.InsertLineHere(const aText: string);
begin
  InsertLine(aText, CaretY, CaretX);
end;

procedure TInpEditor.RemoveEmptyLines;
var
  i: integer;
  s: TStringList;
begin
  s := TStringList.Create;
  try
    if SelAvail then
      s.Text := SelText
    else
      s.Assign(Lines);
    for i := s.Count - 1 downto 0 do
    begin
      if Trim(s[i]) = '' then
        s.Delete(i);
    end;
    if SelAvail then
      SelText := s.Text
    else
      SetText(s.Text);
  finally
    s.Free;
  end;
end;

procedure TInpEditor.ClearBookmarks;
var
  i: integer;
begin
  for i := 0 to 9 do
    ClearBookMark(i);
end;

procedure TInpEditor.CaretToEOF;
begin
  ExecuteCommand(ecEditorBottom, #0, nil);
end;


{ TInpEditPager }

constructor TInpEditPager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTabCloseBtnVisible := False;
  fCloseTabIndex := -1;
  fWatch := TWatchNotifyer.Create;
  fWatch.OnFileStateChange := @WatchFileChange;
  fHighliter := TInpHighlighter.Create(self);
  fCompletion := TSynCompletion.Create(self);
  fCompletion.AutoUseSingleIdent := True;
  fCompletion.CaseSensitive := False;
  fCompletion.LinesInWindow := 10;
  fCompletion.EndOfTokenChr := ', ';
  fCompletion.ShowSizeDrag := True;
  {$IF LCL_FULLVERSION>=1080000}
  fCompletion.ToggleReplaceWhole := False;
  {$ENDIF}
  fCompletion.ShortCut := 16416;
  fCompletion.OnCodeCompletion := @CompletionCodeCompletion;
  fCompletion.OnExecute := @CompletionExecute;
  fCompletion.OnMeasureItem := @CompletionMeasureItem;
  fCompletion.OnPaintItem := @CompletionPaintItem;
  fCompletion.OnSearchPosition := @CompletionSearchPosition;
  TabDragMode := dmAutomatic;
  TabDragAcceptMode := dmAutomatic;
  BorderSpacing.Top := 1;
  BorderSpacing.Left := 1;
  {$IF LCL_FULLVERSION>=1080000}
  Options := Options + [nboDoChangeOnSetIndex];
  {$ENDIF}
  TabCloseBtnVisible := True; // show close button on tab
end;

destructor TInpEditPager.Destroy;
begin
  FreeAndNil(fWatch);
  FreeAndNil(fHighliter);
  FreeAndNil(fCompletion);
  inherited Destroy;
end;

function TInpEditPager.CreateFile(const aFileName: TFileName): boolean;
var
  s: TFileStreamEx;
  r: boolean;
begin
  repeat
    r := False;
    try
      s := TFileStreamEx.Create(aFileName, fmCreate);
      s.Free;
      Result := True;
    except
      Result := False;
    end;
    if not Result then
    begin
      case MessageDlg(Format(sCannotCreate, [aFileName]), mtError, [mbAbort, mbRetry], 0) of
        mrAbort: exit;
        mrRetry: r := True;
      end;
    end;
  until not r;
end;

procedure TInpEditPager.SetTabCloseBtnVisible(AValue: boolean);
{$IfDef Windows}
var
  i: integer;
{$EndIf}
begin
  if fTabCloseBtnVisible = AValue then
    Exit;
  fTabCloseBtnVisible := AValue;
  if fTabCloseBtnVisible then
    Options := Options + [nboShowCloseButtons]
  else
    Options := Options - [nboShowCloseButtons];
  {$IfDef Windows}
  for i := 0 to PageCount - 1 do
    if fTabCloseBtnVisible then
      Pages[i].Caption := TrimRight(Pages[i].Caption) + BUTTON_SPACES
    else
      Pages[i].Caption := TrimRight(Pages[i].Caption);
  {$EndIf}
end;

function TInpEditPager.GetActiveEditor: TInpEditor;
begin
  Result := nil;
  if (PageCount > 0) and (ActivePageIndex >= 0) then
    Result := TInpEditTabSheet(ActivePage).Editor;
end;

function TInpEditPager.EditorFromIndex(const Index: integer): TInpEditor;
begin
  if InRange(Index, 0, PageCount - 1) then
    Result := TInpEditTabSheet(Pages[Index]).Editor
  else
    raise EInpEditor.CreateFmt(sOutTabIndex, [Index]);
end;

function TInpEditPager.IndexFromFilename(const aFileName: TFileName): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to PageCount - 1 do
    if LazFileUtils.CompareFilenames(EditorFromIndex(i).FileName, aFileName) = 0 then
      Exit(i);
end;

function TInpEditPager.IndexOfSpareSheet: integer;
var
  i: integer;
  e: TInpEditor;
begin
  Result := -1;
  for i := 0 to PageCount - 1 do
  begin
    e := EditorFromIndex(i);
    if e.Unnamed and not e.Modified then
      Exit(i);
  end;
end;

function TInpEditPager.Open(const aFileName: TFilename;
  const FromImport: boolean): TInpEditor;
var
  i: integer;
  s: TInpEditTabSheet;
begin
  if aFileName <> '' then
  begin
    i := IndexFromFilename(aFileName);
    if (i >= 0) and (not FromImport) then
    begin
      ActivePageIndex := i;
      Exit;
    end;
    if (not FromImport) and (not ZFileExists(aFileName)) and (aFileName <> '') then
    begin
      case MessageDlg(Format(sAskFileCreation, [aFileName]), mtConfirmation, mbYesNo, 0) of
        mrNo: Exit;
        mrYes: if not CreateFile(aFileName) then
            Exit;
      end;
    end;
    i := IndexOfSpareSheet;
    if i >= 0 then
    begin
      ActivePageIndex := i;
      Result := EditorFromIndex(i);
      if (not FromImport) then
      begin
        Result.Open(aFileName);
        fWatch.AddFile(aFileName, Result);
      end;
      DoChange;
      Exit;
    end;
  end;

  s := TInpEditTabSheet.Create(self);
  s.PageControl := self;
  //s.BorderSpacing.Around:=2;
  Result := TInpEditor.Create(s);
  Result.Align := alClient;
  Result.Sheet := s;
  Result.Watch := fWatch;
  Result.Highlighter := fHighliter;
  Result.OnStatusChange := OnStatusChange;
  Result.Parent := s;
  Result.Unnamed := (aFileName = '') or FromImport;
  UpdateEditorOptions(Result);
  if Result.Unnamed then
  begin
    if FromImport then
      s.Title := '' // title set on future
    else
    begin
      Inc(UnnamedCount);
      s.Title := Format('New%.2d', [UnnamedCount]);
    end;
  end
  else
  begin
    Result.Open(aFileName);
    fWatch.AddFile(aFileName, Result);
  end;
  fCompletion.AddEditor(Result);
  if ActivePage = s then
    DoChange
  else
    ActivePage := s;
end;

function TInpEditPager.Close(Editor: TInpEditor; Force: boolean): boolean;
var
  s: TInpEditTabSheet;
  c: boolean;
begin
  Result := True;
  // if fist tab in unused
  if (PageCount = 1) and Editor.Unnamed and (not Editor.Modified) then
    exit;

  c := False;
  if Assigned(fOnBeforeClose) and not Force then
    fOnBeforeClose(Editor, c);

  if (not c) or Force then
  begin
    s := Editor.fSheet;
    fWatch.RemoveFile(Editor.FileName);
    fCompletion.RemoveEditor(Editor);
    Application.ReleaseComponent(Editor);
    Application.ReleaseComponent(s);
    Application.ProcessMessages;
    if (PageCount = 0) then
    begin
      Open;
      DoChange;
    end;
  end
  else
    Result := False;
end;

function TInpEditPager.CloseAll: boolean;
var
  i: integer;
begin
  Result := True;
  for i := PageCount - 1 downto 0 do
    if not Close(EditorFromIndex(i)) then
      Exit(False);
end;

function TInpEditPager.SaveAll: boolean;
var
  i: integer;
begin
  Result := True;
  for i := PageCount - 1 downto 0 do
    if not EditorFromIndex(i).Save then
      Exit(False);
end;

procedure TInpEditPager.CheckFileChanges;
begin
  fWatch.CheckFiles;
end;

procedure TInpEditPager.SaveHighlighterToIni(Ini: TIniFileEx; const aSection: string);
begin
  fHighliter.SaveToIni(ini, aSection);
end;

procedure TInpEditPager.LoadHighlighterFromIni(Ini: TIniFileEx; const aSection: string);
begin
  fHighliter.LoadFromIni(ini, aSection);
end;

procedure TInpEditPager.UpdateEditorOptions(Editor: TInpEditor);
begin
  FrmMain.EditorOptions.AssignTo(Editor);
  if Highliter.Enabled then
    Editor.Highlighter := Highliter
  else
    Editor.Highlighter := nil;
end;

procedure TInpEditPager.UpdateOptions;
var
  i: integer;
begin
  for i := PageCount - 1 downto 0 do
    UpdateEditorOptions(EditorFromIndex(i));
end;

procedure TInpEditPager.WatchFileChange(Sender: TObject; FileName: TFileName;
  Data: Pointer; State: TWatchStateChange);
var
  e: TInpEditor;
  s: string;
begin
  e := TInpEditor(Data);
  case State of
    wscModified:
    begin
      if e.Modified then
        s := sReloadModified
      else
        s := sReloadSimple;
      if MessageDlg(sReload, Format(s, [FileName]), mtConfirmation, mbYesNo, 0) = mrYes then
      begin
        e.StoreCaretPos;
        e.Open(FileName);
        e.Modified := False;
        e.RestoreCaretPos;
      end
      else
        e.Modified := True;
    end;
    wscDeleted:
    begin
      if MessageDlg(sReload, Format(sKeepDeleted, [FileName]), mtConfirmation,
        mbYesNo, 0) = mrYes then
      begin
        e.Modified := True;
        fWatch.Update(FileName);
      end
      else
        Close(e, True);
    end;
  end;
  fWatch.Update(FileName);
  Widgetset.SetForegroundWindow(Widgetset.AppHandle);
end;

//  Code Completion

procedure TInpEditPager.CompletionCodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin
  if KeyChar = '*' then
  begin
    // ignore comment
    Value := '*';
    SourceEnd := SourceStart;
    fCompletion.Deactivate;
    Exit;
  end;
  if Assigned(ActiveEditor) and (SourceStart.x > 1) then
  begin
    if ActiveEditor.Lines[SourceStart.y - 1][SourceStart.x - 1] = '*' then
    begin
      SourceStart.x -= 1;
    end;
  end;
end;

procedure TInpEditPager.CompletionExecute(Sender: TObject);
begin
  fCompletion.ItemList.Assign(KeywordsList);
  if Assigned(ActiveEditor) then
    ActiveEditor.SetFocus;
end;

procedure TInpEditPager.CompletionSearchPosition(var APosition: integer);

  procedure Add(const s: string);
  var
    x: string;
  begin
    x := fCompletion.CurrentString;
    if Pos(UpperCase(x), UpperCase(s)) = 1 then
      fCompletion.ItemList.Add(s);
  end;

var
  i: integer;
begin
  fCompletion.ItemList.Clear;
  if fCompletion.CurrentString = '' then
  begin
    fCompletion.ItemList.Assign(KeywordsList);
    APosition := -1;
    Exit;
  end
  else
    for i := 0 to KeywordsList.Count - 1 do
      Add(KeywordsList[i]);
  APosition := IfThen(fCompletion.ItemList.Count > 0, 0, -1);
end;

function TInpEditPager.CompletionMeasureItem(const AKey: string;
  ACanvas: TCanvas; Selected: boolean; Index: integer): TPoint;
begin
  ACanvas.Font.Style := [fsBold];
  Result.x := ACanvas.TextWidth(AKey) + 2;
  Result.y := ACanvas.TextHeight('Xy');
end;

function TInpEditPager.CompletionPaintItem(const AKey: string;
  ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean;
begin
  Inc(X, 2);
  Inc(Y);
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := IfThen(Selected, clHighlightText, clWindowText);
  ACanvas.TextOut(X, Y, AKey);
  Result := True;
end;

procedure TInpEditPager.DoCloseTabClicked(APage: TCustomPage);
begin
  inherited DoCloseTabClicked(APage);
  if Assigned(APage) then
    Close(EditorFromIndex(APage.PageIndex));
end;

{$IfDef Windows}
procedure TInpEditPager.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  r: TRect;
  i, h: integer;
  Editor: TInpEditor;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if fTabCloseBtnVisible and (Button = mbLeft) then
  begin
    i := IndexOfTabAt(X, Y);
    if not InRange(i, 0, PageCount - 1) then
      Exit;
    Editor := EditorFromIndex(i);
    if (PageCount = 1) and Editor.Unnamed and (not Editor.Modified) then
    begin
      if Editor.CanFocus then
        Editor.SetFocus;
      Exit;
    end;
    r := TabRect(i);
    h := (r.Bottom - r.Top);
    if (X > r.right - h) and (Y > r.bottom - h) then
    begin
      fCloseTabIndex := i;
      Invalidate;
      Exit;
    end;
  end;
  fCloseTabIndex := -1;
end;

procedure TInpEditPager.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if InRange(fCloseTabIndex, 0, PageCount - 1) then
  begin
    Close(EditorFromIndex(fCloseTabIndex));
    fCloseTabIndex := -1;
  end;
end;

procedure TInpEditPager.PaintWindow(DC: HDC);
const
  bs = 13;
var
  r: TRect;
  i, h, n: integer;
  d: TThemedWindow;
  e: TThemedElementDetails;
  Editor: TInpEditor;
begin
  inherited PaintWindow(DC);
  if not fTabCloseBtnVisible then
    Exit;
  n := SaveDC(DC);
  try
    for i := 0 to PageCount - 1 do
    begin
      r := TabRect(i);
      h := (r.Bottom - r.Top - bs) div 2;
      r.Left := r.Right - bs - h;
      r.Right := r.Left + bs;
      r.Top := r.Top + h;
      r.Bottom := r.Top + bs;
      if ActivePageIndex <> i then
        OffSetRect(r, 0, 2);
      d := twSmallCloseButtonNormal;
      Editor := EditorFromIndex(i);
      if (PageCount = 1) and Editor.Unnamed and (not Editor.Modified) then
        d := twSmallCloseButtonDisabled
      else
      if i = fCloseTabIndex then
        d := twSmallCloseButtonPushed;
      e := ThemeServices.GetElementDetails(d);
      ThemeServices.DrawElement(DC, e, r, nil);
    end;
  finally
    RestoreDC(DC, n);
  end;
end;

procedure TInpEditPager.DoChange;
begin
  inherited DoChange;
  Invalidate;
end;

{$EndIf}

end.
