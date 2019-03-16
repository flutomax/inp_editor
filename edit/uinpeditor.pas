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
  TInpEditZoomMode = (zmReset,zmIn,zmOut);

  TTextLevel = (tlSelection,tlLines,tlFullText);
  TTextLevels = set of TTextLevel;

const
  DefaultLevels = [tlSelection, tlLines];

type

  TTextOperation = function(const text: string): string;

  { TInpEditor }

  TInpEditor = class(TSynEdit)
  private
    fUnnamed: Boolean;
    fFileName: TFilename;
    fSheet: TInpEditTabSheet;
    fCaretPos: TPoint;
    fSyncEdit: TSynPluginSyncroEdit;
    fMultiCaret: TSynPluginMultiCaret;
    fWatch: TWatchNotifyer;
    fCompletionOnAsterick: Boolean;
    fHighlightActiveLine: Boolean;
    fHighlightActiveLineColor: TColor;
    fHighlightMatches: Boolean;
    fHighlightMatchesColor: TColor;
    fEncoding: TStreamEncoding;
    function GetLastPt: TPoint;
    function GetCharCursor: char;
    function GetLineBreakStyle: TTextLineBreakStyle;
    function GetWordAtCursor: string;
    procedure SetFileName(const aValue: TFileName);
    procedure SetHighlightActiveLine(AValue: Boolean);
    procedure SetHighlightActiveLineColor(AValue: TColor);
    procedure SetHighlightMatches(AValue: Boolean);
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
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseLink(Sender: TObject; X, Y: Integer;
      var AllowMouseLink: Boolean);
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
    procedure InsertLine(const aText: string; const aLine: Integer;
      const aCol: Integer = 0);
    procedure RemoveEmptyLines;
    procedure ClearBookmarks;
    procedure CaretToEOF;
    function CanZoomIn: Boolean;
    function CanZoomOut: Boolean;
    function CanOpenFileAtCursor: Boolean;
    function Save: Boolean;
    function SaveAs(const aFileName: TFileName): Boolean;
    property LastPt: TPoint read GetLastPt;
    property WordAtCursor: string read GetWordAtCursor;
    property Encoding: TStreamEncoding read fEncoding;
  published
    property FileName: TFileName read fFileName write SetFileName;
    property Unnamed: boolean read fUnnamed write SetUnnamed;
    property Sheet: TInpEditTabSheet read fSheet write SetSheet;
    property CompletionOnAsterick: Boolean read fCompletionOnAsterick
      write fCompletionOnAsterick;
    property TextLineBreakStyle: TTextLineBreakStyle read GetLineBreakStyle
      write SetTextLineBreakStyle;
    property HighlightActiveLine: Boolean read fHighlightActiveLine
      write SetHighlightActiveLine;
    property HighlightActiveLineColor: TColor read fHighlightActiveLineColor
      write SetHighlightActiveLineColor;
    property HighlightMatches: Boolean read fHighlightMatches
      write SetHighlightMatches;
    property HighlightMatchesColor: TColor read fHighlightMatchesColor
      write SetHighlightMatchesColor;
  end;

  { TInpEditTabSheet }

  TInpEditTabSheet = class(TTabSheet)
  private
    fEditor: TInpEditor;
  public
    property Editor: TInpEditor read fEditor;
  end;

  TOnBeforeClose = procedure(Editor: TInpEditor; var Cancel: Boolean) of object;

  { TInpEditPager }

  TInpEditPager = class(TExtendedNotebook)
  private
    fWatch: TWatchNotifyer;
    fHighliter: TInpHighlighter;
    fCompletion: TSynCompletion;
    fOnBeforeClose: TOnBeforeClose;
    fOnStatusChange: TStatusChangeEvent;
    function GetActiveEditor: TInpEditor;
    function EditorFromIndex(const Index: Integer): TInpEditor;
    function IndexFromFilename(const aFileName: TFileName): Integer;
    function IndexOfSpareSheet: Integer;
    function CreateFile(const aFileName: TFileName): Boolean;
    procedure WatchFileChange(Sender: TObject; FileName: TFileName;
      Data: Pointer; State: TWatchStateChange);
    procedure UpdateEditorOptions(Editor: TInpEditor);
    procedure CompletionCodeCompletion(var Value: string;
      SourceValue: string; var SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    procedure CompletionExecute(Sender: TObject);
    procedure CompletionSearchPosition(var APosition: integer);
    function CompletionMeasureItem(const AKey: string; ACanvas: TCanvas;
      Selected: boolean; Index: integer): TPoint;
    function CompletionPaintItem(const AKey: string; ACanvas: TCanvas; X,
      Y: integer; Selected: boolean; Index: integer): boolean;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open(const aFileName: TFilename = '';
      const FromImport: Boolean = false): TInpEditor;
    function Close(Editor: TInpEditor; Force: Boolean = False): Boolean;
    function CloseAll: Boolean;
    function SaveAll: Boolean;
    procedure CheckFileChanges;
    procedure SaveHighlighterToIni(Ini: TIniFileEx; const aSection: string);
    procedure LoadHighlighterFromIni(Ini: TIniFileEx; const aSection: string);
    procedure UpdateOptions;
    property Highliter: TInpHighlighter read fHighliter;
  published
    property ActiveEditor: TInpEditor read GetActiveEditor;
    property OnBeforeClose: TOnBeforeClose read fOnBeforeClose write fOnBeforeClose;
    property OnStatusChange: TStatusChangeEvent read fOnStatusChange write fOnStatusChange;
  end;

implementation

uses
  Math, StrUtils, LazFileUtils, SynEditKeyCmds, SynEditMouseCmds, SynExportRTF,
  SynEditHighlighterFoldBase, SynEditMarkupWordGroup, InterfaceBase,
  uConsts, uFrmMain, uEditorMisc, uFileUtils;

var
  UnnamedCount: Integer = 0;


{ TInpEditor }

constructor TInpEditor.Create(AOwner: TComponent);
var
  b: TBitmap;
  i: Integer;
  ma: TSynEditMouseAction;
begin
  inherited Create(AOwner);
  Options:=Options+[eoAltSetsColumnMode];
  fHighlightMatches:=true;
  fHighlightActiveLine:=true;
  fCompletionOnAsterick:=true;

  fMultiCaret:=TSynPluginMultiCaret.Create(Self);
  fMultiCaret.EnableWithColumnSelection:=true;
  fMultiCaret.DefaultMode:=mcmMoveAllCarets;
  fMultiCaret.DefaultColumnSelectMode:=mcmCancelOnCaretMove;
  MouseOptions:=[emShowCtrlMouseLinks,emCtrlWheelZoom];
  MouseLinkColor.Style:=[fsUnderline];
  ma:=MouseActions.Add;
  ma.Command:=emcMouseLink;
  ma.Shift:=[ssCtrl];
  ma.ShiftMask:=[ssCtrl];
  OnMouseLink:=@DoMouseLink;
  OnClickLink:=@DoClickLink;

  b:=TBitmap.Create;
  try
    FrmMain.IlMain.GetBitmap(30,b);
    fSyncEdit:=TSynPluginSyncroEdit.Create(Self);
    fSyncEdit.Editor:=Self;
    fSyncEdit.GutterGlyph.Assign(b);
    fSyncEdit.CaseSensitive:=false;
  finally
    b.Free;
  end;

  BookMarkOptions.BookmarkImages:=FrmMain.IlBookmark;
  PopupMenu:=FrmMain.PmEditor;

  // !!! bugfix dwawing red frame in MarkupWordGroup
  for i:=0 to MarkupManager.Count-1 do
    if MarkupManager.Markup[i] is TSynEditMarkupWordGroup then
      MarkupManager.Markup[i].MarkupInfo.FrameColor:=clNone;

  fHighlightMatchesColor:=$009BFF9B;
  HighlightAllColor.Background:=fHighlightMatchesColor;
  HighlightAllColor.Foreground:=clBlack;

  fHighlightActiveLineColor:=$00FFE8E8;
  LineHighlightColor.Background:=fHighlightActiveLineColor;
end;

destructor TInpEditor.Destroy;
begin
  FreeAndNil(fSyncEdit);
  FreeAndNil(fMultiCaret);
  inherited Destroy;
end;

procedure TInpEditor.StoreCaretPos;
begin
  fCaretPos:=CaretXY;
end;

procedure TInpEditor.RestoreCaretPos;
begin
  CaretXY:=fCaretPos;
end;

procedure TInpEditor.SetFileName(const aValue: TFileName);
begin
  if fFileName=AValue then
    Exit;
  fFileName:=AValue;
  fUnnamed:=fFileName='';
  if not fUnnamed then
    fSheet.Caption:=ExtractFileName(fFileName);
end;

procedure TInpEditor.UpdateHighlightActiveLine;
begin
  if fHighlightActiveLine then
    LineHighlightColor.Background:=fHighlightActiveLineColor
  else
    LineHighlightColor.Background:=clNone;
end;

procedure TInpEditor.DoClickLink(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  s: string;
begin
  s:=GetWordAtRowCol(PixelsToRowColumn(Point(X,Y)));
  s:=CreateAbsolutePath(s,ExtractFileDir(FileName));
  if ZFileExists(s) then
    FrmMain.Open(s)
  else
    MessageDlg(Format(sFileNotFound,[s]),mtWarning,[mbOK],0);
end;

procedure TInpEditor.DoMouseLink(Sender: TObject; X, Y: Integer;
  var AllowMouseLink: Boolean);
var
  s: string;
  Attr: TSynHighlighterAttributes;
begin
  GetHighlighterAttriAtRowCol(Point(X,Y),s,Attr);
  AllowMouseLink:=Assigned(Attr) and (Attr.StoredName=SYNS_AttrIncludeFName);
end;

procedure TInpEditor.SetHighlightActiveLine(AValue: Boolean);
begin
  if fHighlightActiveLine=AValue then
    Exit;
  fHighlightActiveLine:=AValue;
  UpdateHighlightActiveLine;
end;

procedure TInpEditor.SetHighlightActiveLineColor(AValue: TColor);
begin
  if fHighlightActiveLineColor=AValue then
    Exit;
  fHighlightActiveLineColor:=AValue;
  UpdateHighlightActiveLine;
end;

procedure TInpEditor.UpdateHighlightMatches;
begin
  if fHighlightMatches then begin
    HighlightAllColor.Background:=fHighlightMatchesColor;
    HighlightAllColor.Foreground:=clBlack;
  end else begin
    HighlightAllColor.Background:=clNone;
    HighlightAllColor.Foreground:=clNone;
  end;
end;

procedure TInpEditor.SetHighlightMatches(AValue: Boolean);
begin
  if fHighlightMatches=AValue then
    Exit;
  fHighlightMatches:=AValue;
  UpdateHighlightMatches;
end;

procedure TInpEditor.SetHighlightMatchesColor(AValue: TColor);
begin
  if fHighlightMatchesColor=AValue then
    Exit;
  fHighlightMatchesColor:=AValue;
  UpdateHighlightMatches;
end;

procedure TInpEditor.SetTextLineBreakStyle(AValue: TTextLineBreakStyle);
begin
  if Lines.TextLineBreakStyle=AValue then
    Exit;
  Lines.TextLineBreakStyle:=AValue;
  Modified:=true;
end;

function TInpEditor.GetLineBreakStyle: TTextLineBreakStyle;
begin
  result:=Lines.TextLineBreakStyle;
end;

function TInpEditor.GetCharCursor: char;
var
  Line: string;
  x: integer;
begin
  Line:=MarkupManager.Lines[CaretXY.Y-1];
  x:=CaretXY.x-1;
  if (Length(Line)>=x) and (x>0) then
    result:=Line[x]
  else
    result:=#0;
end;

function TInpEditor.GetWordAtCursor: string;
begin
  Result:=GetWordAtRowCol(CaretXY);
end;

function TInpEditor.GetLastPt: TPoint;
begin
  Result.Y:=MarkupManager.Lines.Count;
  Result.X:=Length(MarkupManager.Lines[Result.Y-1])+1;
end;

procedure TInpEditor.SetSheet(const aValue: TInpEditTabSheet);
begin
  if fSheet=aValue then
    Exit;
  fSheet:=aValue;
  if Assigned(fSheet) then
    fSheet.fEditor:=self;
end;

procedure TInpEditor.SetUnnamed(const aValue: boolean);
begin
  if fUnnamed=aValue then
    Exit;
  fUnnamed:=aValue;
  if fUnnamed then
    fFileName:='';
end;

procedure TInpEditor.DoCompletionOnAsterick(Data: PtrInt);
var
  key: Word;
begin
  key:=32;
  KeyDown(key,[ssCtrl]);
end;

procedure TInpEditor.UTF8KeyPress(var Key: TUTF8Char);
begin
  if fCompletionOnAsterick and (Key='*') then
    Application.QueueAsyncCall(@DoCompletionOnAsterick,0);
  if (GetCharCursor='*') then begin
    Application.ProcessMessages;
  end;

  inherited UTF8KeyPress(Key);
end;

procedure TInpEditor.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  if fHighlightMatches and (scSelection in Changes) then
    SetHighlightSearch(SelText,[ssoWholeWord]);

  inherited DoOnStatusChange(Changes);
end;

procedure TInpEditor.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  inherited SetHighlighter(Value);
  if Value is TInpHighlighter then begin
    MouseLinkColor.Foreground:=TInpHighlighter(Value).IncludeFNAttri.Foreground;
    MouseLinkColor.Background:=TInpHighlighter(Value).IncludeFNAttri.Background;
  end;
end;

procedure TInpEditor.Open(const aFileName: TFileName);
var
  fs: TFileStreamEx;
begin
  fs:=TFileStreamEx.Create(aFileName,fmOpenRead or fmShareDenyNone);
  try
    // correct encoding if needed
    fEncoding:=AutoDetectEncoding(fs);
    StreamToUTF8Lines(fs,fEncoding,Lines);
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
  GetHighlighterAttriAtRowCol(CaretXY,s,Attr);
  if (Attr=nil) or (Attr.StoredName<>SYNS_AttrIncludeFName) then
    Exit;
  s:=GetWordAtRowCol(CaretXY);
  s:=CreateAbsolutePath(s,ExtractFileDir(FileName));
  if ZFileExists(s) then
    TInpEditPager(fSheet.PageControl).Open(s)
  else
    MessageDlg(Format(sFileNotFound,[s]),mtWarning,[mbOK],0);
end;

function TInpEditor.CanOpenFileAtCursor: Boolean;
var
  s: string;
  Attr: TSynHighlighterAttributes;
begin
  GetHighlighterAttriAtRowCol(CaretXY,s,Attr);
  result:=Assigned(Attr) and (Attr.StoredName=SYNS_AttrIncludeFName);
end;

procedure TInpEditor.Reload;
begin
  StoreCaretPos;
  Open(FileName);
  Modified:=false;
  RestoreCaretPos;
end;

function TInpEditor.Save: Boolean;
begin
  Result:=SaveAs(fFileName);
  Watch.Update(fFileName);
end;

function TInpEditor.SaveAs(const aFileName: TFileName): Boolean;
var
  r: Boolean;
  fs: TFileStreamEx;
begin
  repeat
    r:=false;
    try
      if fFileName<>'' then
        Watch.RemoveFile(fFileName);
      fs:=TFileStreamEx.Create(aFileName,fmCreate);
      try
        Lines.SaveToStream(fs);
      finally
        fs.Free;
      end;
      SetFileName(aFileName);
      Watch.AddFile(aFileName,Self);
      Modified:=false;
      Unnamed:=false;
      Result:=true;
    except
      Result:=false;
    end;
    if not Result then
    begin
      case MessageDlg(Format(sCannotSave,[aFileName]),mtError,mbAbortRetryIgnore,0) of
        mrAbort: Exit;
        mrIgnore: Exit(true);
        mrRetry: r:=true;
      end;
    end;
  until not r;
end;

procedure TInpEditor.FoldCurrent;
begin
  ExecuteCommand(EcFoldCurrent,#0,nil);
end;

procedure TInpEditor.UnfoldCurrent;
begin
  ExecuteCommand(EcUnFoldCurrent,#0,nil);
end;

procedure TInpEditor.Zoom(const Mode: TInpEditZoomMode);
begin
  case Mode of
    zmIn: ExecuteCommand(ecZoomIn,#0,nil);
    zmOut: ExecuteCommand(ecZoomOut,#0,nil);
    zmReset: ExecuteCommand(ecZoomNorm,#0,nil);
  end;
end;

function TInpEditor.CanZoomIn: Boolean;
begin
  result:=abs(Font.Height)<50;
end;

function TInpEditor.CanZoomOut: Boolean;
begin
  result:=abs(Font.Height)>3;
end;

procedure TInpEditor.SetLineText(Index: integer; NewText: string);
begin
  TextBetweenPoints[Point(1,Index+1),
    PhysicalToLogicalPos(Point(Length(Lines[Index])+1,Index+1))]:=NewText;
end;

procedure TInpEditor.SetText(NewText: string);
begin
  TextBetweenPoints[Point(1,1),
    PhysicalToLogicalPos(Point(Length(Lines[Lines.Count-1])+1,
    Lines.Count))]:=NewText;
end;

procedure TInpEditor.TextOperation(Operation: TTextOperation;
  const Level: TTextLevels);
var
  i: integer;
  t: TStringList;
begin
  if (tlSelection in Level) and SelAvail then begin
    t:=TStringList.Create;
    t.Text:=SelText;
    for i:=0 to t.Count-1 do
      t[i]:=Operation(t[i]);
    SelText:=Copy(t.Text,1,Length(t.Text)-Length(LineEnding));
  end else
  if (tlLines in Level) then begin
    BeginUpdate(True);
    try
      for i:=0 to Lines.Count-1 do
        SetLineText(i,Operation(Lines[i]));
    finally
      EndUpdate;
    end;
  end else begin
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
  CaretX:=0;
  SelText:=aText;
end;

procedure TInpEditor.SplitLines;
begin
  if not SelAvail then
    SelectLine;
  SelText:=WrapToList(SelText,RightEdge);
  CaretX:=1;
end;

procedure TInpEditor.JoinLines;
begin
  SelText:=JoinText(SelText);
end;

procedure TInpEditor.DuplicateLine;
var
  ss: Integer;
  s: string;
begin
  if not SelAvail then
    SelectLine;
  ss:=self.SelStart;
  s:=SelText+LineEnding;
  SelStart:=ss;
  SelEnd:=ss;
  TextToNewLine(s);
end;

procedure TInpEditor.InsertLine(const aText: string; const aLine: Integer;
  const aCol: Integer);
begin
  CaretX:=aCol;
  CaretY:=aLine;
  SelText:=aText+Lines.LineBreak;
end;

procedure TInpEditor.RemoveEmptyLines;
var
  i: Integer;
  s: TStringList;
begin
  s:=TStringList.Create;
  try
    if SelAvail then
      s.Text:=SelText
    else
      s.Assign(Lines);
      for i:=s.Count-1 downto 0 do begin
        if Trim(s[i])='' then
          s.Delete(i);
      end;
    if SelAvail then
      SelText:=s.Text
    else
      SetText(s.Text);
  finally
    s.Free;
  end;
end;

procedure TInpEditor.ClearBookmarks;
var
  i: Integer;
begin
  for i:=0 to 9 do
    ClearBookMark(i);
end;

procedure TInpEditor.CaretToEOF;
begin
  ExecuteCommand(ecEditorBottom,#0,nil);
end;


{ TInpEditPager }

constructor TInpEditPager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWatch:=TWatchNotifyer.Create;
  fWatch.OnFileStateChange:=@WatchFileChange;
  fHighliter:=TInpHighlighter.Create(self);
  fCompletion:=TSynCompletion.Create(self);
  fCompletion.AutoUseSingleIdent:=true;
  fCompletion.CaseSensitive:=false;
  fCompletion.LinesInWindow:=10;
  fCompletion.EndOfTokenChr:=', ';
  fCompletion.ShowSizeDrag:=true;
  {$IF LCL_FULLVERSION>=1080000}
  fCompletion.ToggleReplaceWhole:=false;
  {$ENDIF}
  fCompletion.ShortCut:=16416;
  fCompletion.OnCodeCompletion:=@CompletionCodeCompletion;
  fCompletion.OnExecute:=@CompletionExecute;
  fCompletion.OnMeasureItem:=@CompletionMeasureItem;
  fCompletion.OnPaintItem:=@CompletionPaintItem;
  fCompletion.OnSearchPosition:=@CompletionSearchPosition;
  TabDragMode:=dmAutomatic;
  TabDragAcceptMode:=dmAutomatic;
  BorderSpacing.Top:=1;
  BorderSpacing.Left:=1;
  {$IF LCL_FULLVERSION>=1080000}
  Options:=Options+[nboDoChangeOnSetIndex];
  {$ENDIF}
end;

destructor TInpEditPager.Destroy;
begin
  FreeAndNil(fWatch);
  FreeAndNil(fHighliter);
  FreeAndNil(fCompletion);
  inherited Destroy;
end;

function TInpEditPager.CreateFile(const aFileName: TFileName): Boolean;
var
  s: TFileStreamEx;
  r: boolean;
begin
  repeat
    r:=false;
    try
      s:=TFileStreamEx.Create(aFileName,fmCreate);
      s.Free;
      Result:=true;
    except
      Result:=false;
    end;
    if not Result then begin
      case MessageDlg(Format(sCannotCreate,[aFileName]),mtError,[mbAbort,mbRetry],0) of
        mrAbort: exit;
        mrRetry: r:=true;
      end;
    end;
  until not r;
end;

function TInpEditPager.GetActiveEditor: TInpEditor;
begin
  result:=nil;
  if (PageCount>0) and (ActivePageIndex>=0) then
    result:=TInpEditTabSheet(ActivePage).Editor;
end;

function TInpEditPager.EditorFromIndex(const Index: Integer): TInpEditor;
begin
  if InRange(Index,0,PageCount-1) then
    result:=TInpEditTabSheet(Pages[Index]).Editor
  else
    raise EInpEditor.CreateFmt(sOutTabIndex,[Index]);
end;

function TInpEditPager.IndexFromFilename(const aFileName: TFileName): Integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to PageCount-1 do
    if LazFileUtils.CompareFilenames(EditorFromIndex(i).FileName,aFileName)=0 then
      Exit(i);
end;

function TInpEditPager.IndexOfSpareSheet: Integer;
var
  i: integer;
  e: TInpEditor;
begin
  result:=-1;
  for i:=0 to PageCount-1 do begin
    e:=EditorFromIndex(i);
    if e.Unnamed and not e.Modified then
      Exit(i);
  end;
end;

function TInpEditPager.Open(const aFileName: TFilename;
  const FromImport: Boolean): TInpEditor;
var
  i: integer;
  s: TInpEditTabSheet;
begin
  if aFileName<>'' then begin
    i:=IndexFromFilename(aFileName);
    if (i>=0) and (not FromImport) then begin
      ActivePageIndex:=i;
      Exit;
    end;
    if (not FromImport) and (not ZFileExists(aFileName))
    and (aFileName<>'') then begin
      case MessageDlg(Format(sAskFileCreation,[aFileName]),mtConfirmation,mbYesNo,0) of
        mrNo: Exit;
        mrYes: if not CreateFile(aFileName) then Exit;
      end;
    end;
    i:=IndexOfSpareSheet;
    if i>=0 then begin
      ActivePageIndex:=i;
      Result:=EditorFromIndex(i);
      if (not FromImport) then begin
        Result.Open(aFileName);
        fWatch.AddFile(aFileName,Result);
      end;
      DoChange;
      Exit;
    end;
  end;

  s:=TInpEditTabSheet.Create(self);
  s.PageControl:=self;
  //s.BorderSpacing.Around:=2;
  Result:=TInpEditor.Create(s);
  Result.Align:=alClient;
  Result.Sheet:=s;
  Result.Watch:=fWatch;
  Result.Highlighter:=fHighliter;
  Result.OnStatusChange:=OnStatusChange;
  Result.Parent:=s;
  Result.Unnamed:=(aFileName='') or FromImport;
  UpdateEditorOptions(Result);
  if Result.Unnamed then begin
    if FromImport then
      s.Caption:='' // caption set on future
    else begin
      Inc(UnnamedCount);
      s.Caption:=Format('New%.2d',[UnnamedCount]);
    end;
  end else begin
    Result.Open(aFileName);
    fWatch.AddFile(aFileName,Result);
  end;
  fCompletion.AddEditor(Result);
  if ActivePage=s then
    DoChange
  else
    ActivePage:=s;
end;

function TInpEditPager.Close(Editor: TInpEditor; Force: Boolean): Boolean;
var
  s: TInpEditTabSheet;
  c: boolean;
begin
  result:=true;
  // if last tab in unused
  if (PageCount=1) and Editor.Unnamed and (not Editor.Modified) then
    exit;

  c:=false;
  if Assigned(fOnBeforeClose) and not Force then
    fOnBeforeClose(Editor,c);

  if (not c) or Force then begin
    s:=Editor.fSheet;
    fWatch.RemoveFile(Editor.FileName);
    fCompletion.RemoveEditor(Editor);
    Application.ReleaseComponent(Editor);
    Application.ReleaseComponent(s);
    Application.ProcessMessages;
    if (PageCount=0) then begin
      Open;
      DoChange;
    end;
  end
  else
    result:=false;
end;

function TInpEditPager.CloseAll: Boolean;
var
  i: integer;
begin
  Result:=true;
  for i:=PageCount-1 downto 0 do
    if not Close(EditorFromIndex(i)) then
      Exit(false);
end;

function TInpEditPager.SaveAll: Boolean;
var
  i: integer;
begin
  Result:=true;
  for i:=PageCount-1 downto 0 do
    if not EditorFromIndex(i).Save then
      Exit(false);
end;

procedure TInpEditPager.CheckFileChanges;
begin
  fWatch.CheckFiles;
end;

procedure TInpEditPager.SaveHighlighterToIni(Ini: TIniFileEx;
  const aSection: string);
begin
  fHighliter.SaveToIni(ini,aSection);
end;

procedure TInpEditPager.LoadHighlighterFromIni(Ini: TIniFileEx;
  const aSection: string);
begin
  fHighliter.LoadFromIni(ini,aSection);
end;

procedure TInpEditPager.UpdateEditorOptions(Editor: TInpEditor);
begin
  FrmMain.EditorOptions.AssignTo(Editor);
  if Highliter.Enabled then
    Editor.Highlighter:=Highliter
  else
    Editor.Highlighter:=nil;
end;

procedure TInpEditPager.UpdateOptions;
var
  i: integer;
begin
  for i:=PageCount-1 downto 0 do
    UpdateEditorOptions(EditorFromIndex(i));
end;

procedure TInpEditPager.WatchFileChange(Sender: TObject; FileName: TFileName;
  Data: Pointer; State: TWatchStateChange);
var
  e: TInpEditor;
  s: string;
begin
  e:=TInpEditor(Data);
  case State of
    wscModified: begin
      if e.Modified then
        s:=sReloadModified
      else
        s:=sReloadSimple;
      if MessageDlg(sReload,Format(s,[FileName]),mtConfirmation,mbYesNo,0)=mrYes then begin
        e.StoreCaretPos;
        e.Open(FileName);
        e.Modified:=false;
        e.RestoreCaretPos;
      end
      else
        e.Modified:=true;
    end;
    wscDeleted: begin
      if MessageDlg(sReload,Format(sKeepDeleted,[FileName]),mtConfirmation,mbYesNo,0)=mrYes then begin
        e.Modified:=true;
        fWatch.Update(FileName);
      end
      else
        Close(e,true);
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
  if KeyChar='*' then begin
    // ignore comment
    Value:='*';
    SourceEnd:=SourceStart;
    fCompletion.Deactivate;
    Exit;
  end;
  if Assigned(ActiveEditor) and (SourceStart.x>1) then begin
    if ActiveEditor.Lines[SourceStart.y-1][SourceStart.x-1]='*' then begin
      SourceStart.x-=1;
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
    x:=fCompletion.CurrentString;
    if Pos(UpperCase(x),UpperCase(s))=1 then
      fCompletion.ItemList.Add(s);
  end;

var
  i: Integer;
begin
  fCompletion.ItemList.Clear;
  if fCompletion.CurrentString='' then begin
    fCompletion.ItemList.Assign(KeywordsList);
    APosition:=-1;
    Exit;
  end
  else
    for i:=0 to KeywordsList.Count-1 do
      Add(KeywordsList[i]);
  APosition:=IfThen(fCompletion.ItemList.Count>0,0,-1);
end;

function TInpEditPager.CompletionMeasureItem(const AKey: string;
  ACanvas: TCanvas; Selected: boolean; Index: integer): TPoint;
begin
  ACanvas.Font.Style:=[fsBold];
  result.x:=ACanvas.TextWidth(AKey)+2;
  result.y:=ACanvas.TextHeight('Xy');
end;

function TInpEditPager.CompletionPaintItem(const AKey: string;
  ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean;
begin
  Inc(X,2);
  Inc(Y);
  ACanvas.Font.Style:=[fsBold];
  ACanvas.Font.Color:=IfThen(Selected,clHighlightText,clWindowText);
  ACanvas.TextOut(X,Y,AKey);
  result:=true;
end;


end.

