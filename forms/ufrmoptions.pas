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

unit uFrmOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Spin, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, EditBtn, CheckLst, ButtonPanel,
  Buttons, uHighliter;

type

  { TFrmOptions }

  TFrmOptions = class(TForm)
    btBrowsePathCCX: TSpeedButton;
    btBrowseCGXPath: TSpeedButton;
    btBrowseTerminalPath: TSpeedButton;
    btFont: TButton;
    btReset: TButton;
    btClearMRU: TButton;
    ckScrollToEnd: TCheckBox;
    edPathCGX: TComboBox;
    edPathTerminal: TComboBox;
    edPathTemplates: TComboBox;
    edPathCCX: TComboBox;
    lbPathTerminal: TLabel;
    lbPathTemplates: TLabel;
    lbPathCCX: TLabel;
    lbPathCGX: TLabel;
    dlgOpenProgram: TOpenDialog;
    pnBottom: TButtonPanel;
    ckHighlightActiveLine: TCheckBox;
    ckHighlightMatches: TCheckBox;
    ckWantTabs: TCheckBox;
    ckHideSelection: TCheckBox;
    ckGutterOptions: TCheckGroup;
    ckShowGutter: TCheckBox;
    ckCompletionOnAsterick: TCheckBox;
    ckEditMouseOptionsCtrlWheelZoom: TCheckBox;
    ckEditMouseOptionsDoubleClickSelectsLine: TCheckBox;
    ckEditMouseOptionsAltSetsColumnMode: TCheckBox;
    ckEditOptions2ColorSelectionTillEol: TCheckBox;
    ckEditOptions2AutoHideCursor: TCheckBox;
    ckEditOptions2OverwriteBlock: TCheckBox;
    ckEditOptions2PersistentBlock: TCheckBox;
    ckEditOptions2FoldedCopyPaste: TCheckBox;
    ckEditOptions2EnhanceEndKey: TCheckBox;
    ckEditOptions2AlwaysVisibleCaret: TCheckBox;
    ckEditOptions2CaretSkipTab: TCheckBox;
    ckEditOptions2CaretSkipsSelection: TCheckBox;
    ckUseHighLighter: TCheckBox;
    ckHighlighterBold: TCheckBox;
    ckHighlighterItalic: TCheckBox;
    ckHighlighterUnderline: TCheckBox;
    ckEditOptionsTrimTrailingSpaces: TCheckBox;
    ckEditOptionsTabsToSpaces: TCheckBox;
    ckEditOptionsShowScrollHint: TCheckBox;
    ckEditOptionsScrollPastEOL: TCheckBox;
    ckEditOptionsScrollPastEOF: TCheckBox;
    ckEditOptionsScrollByOneLess: TCheckBox;
    ckEditOptionsHalfPageScroll: TCheckBox;
    ckEditOptionsHideShowScrollbars: TCheckBox;
    ckEditOptionsEnhanceHomeKey: TCheckBox;
    ckEditOptionsRightMouseMovesCursor: TCheckBox;
    ckEditOptionsSmartTabDelete: TCheckBox;
    ckEditOptionsSmartTabs: TCheckBox;
    ckEditOptionsKeepCaretX: TCheckBox;
    ckEditOptionsAltSetsColumnMode: TCheckBox;
    ckEditOptionsDragDropEditing: TCheckBox;
    ckEditOptionsAutoIndent: TCheckBox;
    ckEditOptionsGroupUndo: TCheckBox;
    ckFileAssociation: TCheckBox;
    ckUniqueInstance: TCheckBox;
    ckStoreFormPlacement: TCheckBox;
    ckShowHints: TCheckBox;
    clbGutterColor: TColorButton;
    clbModifiedColor: TColorButton;
    clbSavedColor: TColorButton;
    clbSelectedColorBackground: TColorButton;
    clbRightEdgeColor: TColorButton;
    clbHighlightActiveLineBackground: TColorButton;
    clbHighlightMatchesBackground: TColorButton;
    clbSelectedColorForeground: TColorButton;
    clBtnHighlighterForeground: TColorButton;
    clBtnHighlighterBackground: TColorButton;
    cbOverwriteCaret: TComboBox;
    cbInsertCaret: TComboBox;
    edExtraCharSpacing: TSpinEdit;
    Editor: TSynEdit;
    edExtraLineSpacing: TSpinEdit;
    dlgFont: TFontDialog;
    edTabWidth: TSpinEdit;
    gbPaths: TGroupBox;
    gbElement: TGroupBox;
    gbAttr: TGroupBox;
    gbCaret: TGroupBox;
    GroupBox1: TGroupBox;
    gbFont: TGroupBox;
    gbSelection: TGroupBox;
    gbGutter: TGroupBox;
    gbRightEdge: TGroupBox;
    gbGutterColors: TGroupBox;
    gbRecentFiles: TGroupBox;
    gbActiveLine: TGroupBox;
    gbHighlightMatches: TGroupBox;
    lbMaxMHU: TLabel;
    lbRightEdge: TLabel;
    lbHighlightActiveLineBackground: TLabel;
    lbHighlightMatchesBackground: TLabel;
    lbShowOnlyLineNumbersMultiplesOf: TLabel;
    lbGutterColor: TLabel;
    lbModifiedColor: TLabel;
    lbSavedColor: TLabel;
    lbInsertCaret: TLabel;
    lbOverwriteCaret: TLabel;
    lbBtnHighlighterForeground: TLabel;
    lbBtnHighlighterBackground: TLabel;
    lbEditAttr: TListBox;
    lbSelectedColorBackground: TLabel;
    lbRightEdgeColor: TLabel;
    lbSelectedColorForeground: TLabel;
    lbTabWidth: TLabel;
    lbExtraLineSpacing: TLabel;
    lbExtraCharSpacing: TLabel;
    lbUndoLimit: TLabel;
    pcMain: TPageControl;
    pnFont: TPanel;
    edUndoLimit: TSpinEdit;
    edRightEdge: TSpinEdit;
    edShowOnlyLineNumbersMultiplesOf: TSpinEdit;
    edMaxMHU: TSpinEdit;
    btBrowseTemplatesPath: TSpeedButton;
    dlgPathTemplates: TSelectDirectoryDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    tsGeneral: TTabSheet;
    procedure btBrowsePathCCXClick(Sender: TObject);
    procedure btBrowseTemplatesPathClick(Sender: TObject);
    procedure btClearMRUClick(Sender: TObject);
    procedure btFontClick(Sender: TObject);
    procedure btResetClick(Sender: TObject);
    procedure ckHighlightActiveLineClick(Sender: TObject);
    procedure ckHighlightMatchesClick(Sender: TObject);
    procedure ckShowGutterClick(Sender: TObject);
    procedure ckUseHighLighterClick(Sender: TObject);
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbEditAttrClick(Sender: TObject);
    procedure HighlighterAttrChange(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
  private
    fHighlighter: TInpHighlighter;
    fUpdateAtr: Boolean;
    fDoReset: Boolean;
    procedure CheckAttrEnabled;
    procedure UpdateHighlighter;
    procedure UpdateGuter;
    procedure UpdateEditorFont(aFont: TFont);
    procedure UpdateHighlightActiveLine;
    procedure UpdateHighlightMatches;
    procedure ResetHighlighterAttributes;
  public
    procedure SaveOptions;
  end;

var
  FrmOptions: TFrmOptions;

  function ShowOptions(const NumPage: Integer = 0): Boolean;

implementation

{$R *.lfm}

uses
  Math, LazFileUtils, SynEditHighlighter, SynEditMouseCmds, SynEditPointClasses,
  SynEditStrConst, uFrmMain, uDerivedClasses, uConsts, uEditorMisc;

function ShowOptions(const NumPage: Integer): Boolean;
begin
  FrmOptions:=TFrmOptions.Create(Application);
  try
    FrmOptions.pcMain.ActivePageIndex:=NumPage;
    Result:=FrmOptions.ShowModal=mrOK;
    if Result then
      FrmOptions.SaveOptions;
  finally
    FrmOptions.Release;
  end;
end;

{ TFrmOptions }

procedure TFrmOptions.FormCreate(Sender: TObject);
begin
  pnBottom.Color:=clForm; // correct button panel color
  // correct buttons glyph
  btBrowseTemplatesPath.Glyph.TransparentColor:=clFuchsia;
  btBrowsePathCCX.Glyph.TransparentColor:=clFuchsia;
  btBrowseCGXPath.Glyph.TransparentColor:=clFuchsia;
  btBrowseTerminalPath.Glyph.TransparentColor:=clFuchsia;
  fHighlighter:=TInpHighlighter.Create(self);
  Editor.Highlighter:=fHighlighter;
  fUpdateAtr:=false;
  fDoReset:=false;
  cbInsertCaret.AddItem('Vertical Line',nil);
  cbInsertCaret.AddItem('Horizontal Line',nil);
  cbInsertCaret.AddItem('Half Block',nil);
  cbInsertCaret.AddItem('Block',nil);
  cbOverwriteCaret.Items.Assign(cbInsertCaret.Items);
  ckScrollToEnd.Checked:=true;
  CentredLabels(self);
  dlgOpenProgram.Filter:=sFilterProgram;
  dlgOpenProgram.DefaultExt:=sDefExtProgram;
  {$IfDef Windows}
    edPathTerminal.Enabled:=false;
    lbPathTerminal.Enabled:=false;
    btBrowseTerminalPath.Enabled:=false;
  {$EndIf}
end;

procedure TFrmOptions.FormDestroy(Sender: TObject);
begin
  fHighlighter.Free;
end;

procedure TFrmOptions.FormShow(Sender: TObject);
var
  i: Integer;

var
  ini: TIniFileEx;
begin
  {$IfDef Windows}
    ckFileAssociation.Checked:=FrmMain.Config.FileAssociation;
  {$Else}
    ckFileAssociation.Enabled:=false;
  {$EndIf}
  ckShowHints.Checked:=FrmMain.Config.ShowHints;
  ckStoreFormPlacement.Checked:=FrmMain.Config.StoreFormPlacement;
  ckUniqueInstance.Checked:=FrmMain.Config.UniqueInstance;
  edPathTemplates.Items.Assign(FrmMain.Config.TemplatesPaths);
  edPathTemplates.Text:=FrmMain.Config.TemplatesPath;
  edPathCCX.Text:=FrmMain.Config.CCXPath;
  edPathCCX.Items.Assign(FrmMain.Config.CCXPaths);
  edPathCGX.Text:=FrmMain.Config.CGXPath;
  edPathCGX.Items.Assign(FrmMain.Config.CGXPaths);
  edPathTerminal.Text:=FrmMain.Config.TerminalPath;
  edPathTerminal.Items.Assign(FrmMain.Config.TerminalPaths);
  edMaxMHU.Value:=FrmMain.MRUList.MaxRecent;

  fHighlighter.Assign(FrmMain.Pager.Highliter);

  ckUseHighLighter.Checked:=fHighlighter.Enabled;
  for i:=0 to fHighlighter.AttrCount-1 do
    lbEditAttr.Items.AddObject(fHighlighter.Attribute[i].Name,fHighlighter.Attribute[i]);
  UpdateHighlighter;

  with FrmMain.EditorOptions do begin
    ckEditOptionsGroupUndo.Checked:=eoGroupUndo in Options;
    ckEditOptionsAutoIndent.Checked:=eoAutoIndent in Options;
    ckEditOptionsDragDropEditing.Checked:=eoDragDropEditing in Options;
    ckEditOptionsAltSetsColumnMode.Checked:=eoAltSetsColumnMode in Options;
    ckEditOptionsKeepCaretX.Checked:=eoKeepCaretX in Options;
    ckEditOptionsSmartTabs.Checked:=eoSmartTabs in Options;
    ckEditOptionsSmartTabDelete.Checked:=eoSmartTabDelete in Options;
    ckEditOptionsRightMouseMovesCursor.Checked:=eoRightMouseMovesCursor in Options;
    ckEditOptionsEnhanceHomeKey.Checked:=eoEnhanceHomeKey in Options;
    ckEditOptions2EnhanceEndKey.Checked:=eoEnhanceEndKey in Options2;
    ckEditOptionsHideShowScrollbars.Checked:=eoHideShowScrollbars in Options;
    ckEditOptionsHalfPageScroll.Checked:=eoHalfPageScroll in Options;
    ckEditOptionsScrollByOneLess.Checked:=eoScrollByOneLess in Options;
    ckEditOptionsScrollPastEOF.Checked:=eoScrollPastEOF in Options;
    ckEditOptionsScrollPastEOL.Checked:=eoScrollPastEOL in Options;
    ckEditOptionsShowScrollHint.Checked:=eoShowScrollHint in Options;
    ckEditOptionsTabsToSpaces.Checked:=eoTabsToSpaces in Options;
    ckEditOptionsTrimTrailingSpaces.Checked:=eoTrimTrailingSpaces in Options;
    ckEditOptions2CaretSkipsSelection.Checked:=eoCaretSkipsSelection in Options2;
    ckEditOptions2CaretSkipTab.Checked:=eoCaretSkipTab in Options2;
    ckEditOptions2AlwaysVisibleCaret.Checked:=eoAlwaysVisibleCaret in Options2;
    ckEditOptions2FoldedCopyPaste.Checked:=eoFoldedCopyPaste in Options2;
    ckEditOptions2PersistentBlock.Checked:=eoPersistentBlock in Options2;
    ckEditOptions2OverwriteBlock.Checked:=eoOverwriteBlock in Options2;
    ckEditOptions2AutoHideCursor.Checked:=eoAutoHideCursor in Options2;
    ckEditOptions2ColorSelectionTillEol.Checked:=eoColorSelectionTillEol in Options2;
    ckEditMouseOptionsAltSetsColumnMode.Checked:=emAltSetsColumnMode in MouseOptions;
    ckEditMouseOptionsDoubleClickSelectsLine.Checked:=emDoubleClickSelectsLine in MouseOptions;
    ckEditMouseOptionsCtrlWheelZoom.Checked:=emCtrlWheelZoom in MouseOptions;
    ckCompletionOnAsterick.Checked:=CompletionOnAsterick;
    ckHideSelection.Checked:=HideSelection;
    ckWantTabs.Checked:=WantTabs;

    cbInsertCaret.ItemIndex:=Ord(InsertCaret);
    cbOverwriteCaret.ItemIndex:=Ord(OverwriteCaret);
    ckScrollToEnd.Checked:=ScrollToEnd;
    edUndoLimit.Value:=MaxUndo;
    edTabWidth.Value:=TabWidth;
    edExtraLineSpacing.Value:=ExtraLineSpacing;
    edExtraCharSpacing.Value:=ExtraCharSpacing;

    UpdateEditorFont(Font);

    clbSelectedColorForeground.ButtonColor:=SelectedColor.Foreground;
    clbSelectedColorBackground.ButtonColor:=SelectedColor.Background;

    ckHighlightActiveLine.Checked:=HighlightActiveLine;
    clbHighlightActiveLineBackground.ButtonColor:=HighlightActiveLineColor;
    UpdateHighlightActiveLine;

    ckHighlightMatches.Checked:=HighlightMatches;
    clbHighlightMatchesBackground.ButtonColor:=HighlightMatchesColor;
    UpdateHighlightMatches;

    edRightEdge.Value:=RightEdge;
    clbRightEdgeColor.ButtonColor:=RightEdgeColor;

    ckShowGutter.Checked:=ShowGutter;
    ckGutterOptions.Checked[0]:=GutterAutoSize;
    ckGutterOptions.Checked[1]:=ShowLineNumbers;
    ckGutterOptions.Checked[2]:=LeadingZeros;
    ckGutterOptions.Checked[3]:=ZeroStart;
    ckGutterOptions.Checked[4]:=ShowChanges;
    ckGutterOptions.Checked[5]:=ShowCodeFolding;
    clbGutterColor.ButtonColor:=GutterColor;
    clbModifiedColor.ButtonColor:=ModifiedColor;
    clbSavedColor.ButtonColor:=SavedColor;
    edShowOnlyLineNumbersMultiplesOf.Value:=ShowOnlyLineNumbersMultiplesOf;
    UpdateGuter;
  end;

end;

procedure TFrmOptions.SaveOptions;
var
  op: TSynEditorOptions;
  op2: TSynEditorOptions2;
  mo: TSynEditorMouseOptions;

  procedure SetEditOpton(const ck: TCheckBox; const opt: TSynEditorOption);
  begin
    if ck.Checked then
      Include(op,opt)
    else
      Exclude(op,opt);
  end;

  procedure SetEditOpton2(const ck: TCheckBox; const opt: TSynEditorOption2);
  begin
    if ck.Checked then
      Include(op2,opt)
    else
      Exclude(op2,opt);
  end;

  procedure SetMouseOpton(const ck: TCheckBox; const opt: TSynEditorMouseOption);
  begin
    if ck.Checked then
      Include(mo,opt)
    else
      Exclude(mo,opt);
  end;

begin
  FrmMain.Config.ShowHints:=ckShowHints.Checked;
  FrmMain.Config.StoreFormPlacement:=ckStoreFormPlacement.Checked;
  FrmMain.Config.UniqueInstance:=ckUniqueInstance.Checked;
  FrmMain.Config.FileAssociation:=ckFileAssociation.Checked;
  FrmMain.Config.TemplatesPath:=edPathTemplates.Text;
  FrmMain.Config.CCXPath:=edPathCCX.Text;
  FrmMain.Config.CGXPath:=edPathCGX.Text;
  {$IfNDef Windows}
    FrmMain.Config.TerminalPath:=edPathTerminal.Text;
  {$EndIf}
  FrmMain.Pager.Highliter.Assign(fHighlighter);
  FrmMain.MRUList.MaxRecent:=edMaxMHU.Value;

  op:=FrmMain.EditorOptions.DefOptions;
  SetEditOpton(ckEditOptionsGroupUndo,eoGroupUndo);
  SetEditOpton(ckEditOptionsAutoIndent,eoAutoIndent);
  SetEditOpton(ckEditOptionsDragDropEditing,eoDragDropEditing);
  SetEditOpton(ckEditOptionsAltSetsColumnMode,eoAltSetsColumnMode);
  SetEditOpton(ckEditOptionsKeepCaretX,eoKeepCaretX);
  SetEditOpton(ckEditOptionsSmartTabs,eoSmartTabs);
  SetEditOpton(ckEditOptionsSmartTabDelete,eoSmartTabDelete);
  SetEditOpton(ckEditOptionsRightMouseMovesCursor,eoRightMouseMovesCursor);
  SetEditOpton(ckEditOptionsEnhanceHomeKey,eoEnhanceHomeKey);
  SetEditOpton(ckEditOptionsHideShowScrollbars,eoHideShowScrollbars);
  SetEditOpton(ckEditOptionsHalfPageScroll,eoHalfPageScroll);
  SetEditOpton(ckEditOptionsScrollByOneLess,eoScrollByOneLess);
  SetEditOpton(ckEditOptionsScrollPastEOF,eoScrollPastEOF);
  SetEditOpton(ckEditOptionsScrollPastEOL,eoScrollPastEOL);
  SetEditOpton(ckEditOptionsShowScrollHint,eoShowScrollHint);
  SetEditOpton(ckEditOptionsTabsToSpaces,eoTabsToSpaces);
  SetEditOpton(ckEditOptionsTrimTrailingSpaces,eoTrimTrailingSpaces);
  FrmMain.EditorOptions.Options:=op;

  op2:=FrmMain.EditorOptions.DefOptions2;
  SetEditOpton2(ckEditOptions2EnhanceEndKey,eoEnhanceEndKey);
  SetEditOpton2(ckEditOptions2CaretSkipsSelection,eoCaretSkipsSelection);
  SetEditOpton2(ckEditOptions2CaretSkipTab,eoCaretSkipTab);
  SetEditOpton2(ckEditOptions2AlwaysVisibleCaret,eoAlwaysVisibleCaret);
  SetEditOpton2(ckEditOptions2FoldedCopyPaste,eoFoldedCopyPaste);
  SetEditOpton2(ckEditOptions2PersistentBlock,eoPersistentBlock);
  SetEditOpton2(ckEditOptions2OverwriteBlock,eoOverwriteBlock);
  SetEditOpton2(ckEditOptions2AutoHideCursor,eoAutoHideCursor);
  SetEditOpton2(ckEditOptions2ColorSelectionTillEol,eoColorSelectionTillEol);
  FrmMain.EditorOptions.Options2:=op2;

  mo:=FrmMain.EditorOptions.DefMouseOptions;
  SetMouseOpton(ckEditMouseOptionsAltSetsColumnMode,emAltSetsColumnMode);
  SetMouseOpton(ckEditMouseOptionsDoubleClickSelectsLine,emDoubleClickSelectsLine);
  SetMouseOpton(ckEditMouseOptionsCtrlWheelZoom,emCtrlWheelZoom);
  FrmMain.EditorOptions.MouseOptions:=mo;

  with FrmMain.EditorOptions do begin
    CompletionOnAsterick:=ckCompletionOnAsterick.Checked;
    HideSelection:=ckHideSelection.Checked;
    WantTabs:=ckWantTabs.Checked;
    InsertCaret:=TSynEditCaretType(cbInsertCaret.ItemIndex);
    OverwriteCaret:=TSynEditCaretType(cbOverwriteCaret.ItemIndex);
    ScrollToEnd:=ckScrollToEnd.Checked;
    MaxUndo:=edUndoLimit.Value;
    TabWidth:=edTabWidth.Value;
    ExtraLineSpacing:=edExtraLineSpacing.Value;
    ExtraCharSpacing:=edExtraCharSpacing.Value;
    Font:=pnFont.Font;
    SelectedColor.Foreground:=clbSelectedColorForeground.ButtonColor;
    SelectedColor.Background:=clbSelectedColorBackground.ButtonColor;

    HighlightActiveLine:=ckHighlightActiveLine.Checked;
    HighlightActiveLineColor:=clbHighlightActiveLineBackground.ButtonColor;
    HighlightMatches:=ckHighlightMatches.Checked;
    HighlightMatchesColor:=clbHighlightMatchesBackground.ButtonColor;

    RightEdge:=edRightEdge.Value;
    RightEdgeColor:=clbRightEdgeColor.ButtonColor;
    ShowGutter:=ckShowGutter.Checked ;
    GutterAutoSize:=ckGutterOptions.Checked[0];
    ShowLineNumbers:=ckGutterOptions.Checked[1];
    LeadingZeros:=ckGutterOptions.Checked[2];
    ZeroStart:=ckGutterOptions.Checked[3];
    ShowChanges:=ckGutterOptions.Checked[4];
    ShowCodeFolding:=ckGutterOptions.Checked[5];
    GutterColor:=clbGutterColor.ButtonColor;
    ModifiedColor:=clbModifiedColor.ButtonColor;
    SavedColor:=clbSavedColor.ButtonColor;
    ShowOnlyLineNumbersMultiplesOf:=edShowOnlyLineNumbersMultiplesOf.Value;
  end;

  FrmMain.Pager.UpdateOptions;

  // Reset toolbars
  if fDoReset then begin
    FrmMain.cmdViewTbReset.Execute;
    if not FrmMain.sbEditor.Visible then
      FrmMain.cmdViewStatusbar.Execute;
    fDoReset:=false;
  end;
end;


procedure TFrmOptions.EditorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Attr: TSynHighlighterAttributes;
  Pt: TPoint;
  i: Integer;
  s: string;
begin
  Pt:=Editor.PixelsToRowColumn(Point(X,Y));
  Editor.GetHighlighterAttriAtRowCol(Pt,s,Attr);
  if Attr=nil then
    Exit;
  i:=lbEditAttr.Items.IndexOf(Attr.Name);
  if i<0 then
    Exit;
  lbEditAttr.ItemIndex:=i;
  lbEditAttrClick(LbEditAttr);
end;

procedure TFrmOptions.lbEditAttrClick(Sender: TObject);
var
  Attr: TSynHighlighterAttributes;
begin
  fUpdateAtr:=true;
  try
    if InRange(LbEditAttr.ItemIndex,0,LbEditAttr.Items.Count-1) then begin
      Attr:=TSynHighlighterAttributes(LbEditAttr.Items.Objects[LbEditAttr.ItemIndex]);
      clBtnHighlighterForeground.ButtonColor:=Attr.Foreground; // ColorToRGB()
      clBtnHighlighterBackground.ButtonColor:=Attr.Background;
      ckHighlighterBold.Checked:=fsBold in Attr.Style;
      ckHighlighterItalic.Checked:=fsItalic in Attr.Style;
      ckHighlighterUnderline.Checked:=fsUnderline in Attr.Style;
      CheckAttrEnabled;
    end else begin
      clBtnHighlighterForeground.ButtonColor:=clNone;
      clBtnHighlighterBackground.ButtonColor:=clNone;
      ckHighlighterBold.Checked:=false;
      ckHighlighterItalic.Checked:=false;
      ckHighlighterUnderline.Checked:=false;
      gbAttr.Enabled:=false;
      ckHighlighterBold.Enabled:=false;
      ckHighlighterItalic.Enabled:=false;
      ckHighlighterUnderline.Enabled:=false;
      clBtnHighlighterForeground.Enabled:=false;
      lbBtnHighlighterForeground.Enabled:=false;
      clBtnHighlighterBackground.Enabled:=false;
      lbBtnHighlighterBackground.Enabled:=false;
    end;
  finally
    fUpdateAtr:=false;
  end;
end;

procedure TFrmOptions.HighlighterAttrChange(Sender: TObject);
var
  Attr: TSynHighlighterAttributes;
  FontStyle: TFontStyles;
begin
  if fUpdateAtr then
    Exit;
  Attr:=TSynHighlighterAttributes(LbEditAttr.Items.Objects[LbEditAttr.ItemIndex]);
  Attr.Foreground:=ClBtnHighlighterForeground.ButtonColor;
  Attr.Background:=ClBtnHighlighterBackground.ButtonColor;
  FontStyle:=[];
  if ckHighlighterBold.Checked then
    Include(FontStyle,fsBold);
  if ckHighlighterItalic.Checked then
    Include(FontStyle,fsItalic);
  if ckHighlighterUnderline.Checked then
    Include(FontStyle,fsUnderline);
  Attr.Style:=FontStyle;
end;

procedure TFrmOptions.TabSheet2Show(Sender: TObject);
begin
  lbEditAttr.ItemIndex:=-1;
  CheckAttrEnabled;
end;

procedure TFrmOptions.CheckAttrEnabled;
var
  e: Boolean;
begin
  e:=ckUseHighLighter.Checked and (LbEditAttr.ItemIndex>=0);
  gbAttr.Enabled:=e;
  ckHighlighterBold.Enabled:=e;
  ckHighlighterItalic.Enabled:=e;
  ckHighlighterUnderline.Enabled:=e;
  clBtnHighlighterForeground.Enabled:=e;
  lbBtnHighlighterForeground.Enabled:=e;
  clBtnHighlighterBackground.Enabled:=e;
  lbBtnHighlighterBackground.Enabled:=e;
end;

procedure TFrmOptions.UpdateHighlighter;
var
  i: integer;
begin
  fHighlighter.Enabled:=ckUseHighLighter.Checked;
  gbElement.Enabled:=fHighlighter.Enabled;
  gbAttr.Enabled:=fHighlighter.Enabled;
  LbEditAttr.Enabled:=fHighlighter.Enabled;
  for i:=0 to gbAttr.ControlCount-1 do
    gbAttr.Controls[i].Enabled:=fHighlighter.Enabled;

  if fHighlighter.Enabled then
    Editor.Highlighter:=fHighlighter
  else
    Editor.Highlighter:=nil;
end;

procedure TFrmOptions.UpdateEditorFont(aFont: TFont);
begin
  pnFont.Font:=aFont;
  pnFont.Caption:=Format('%s %d pt',[aFont.Name,aFont.Size]);
end;

procedure TFrmOptions.UpdateHighlightActiveLine;
begin
  lbHighlightActiveLineBackground.Enabled:=ckHighlightActiveLine.Checked;
  clbHighlightActiveLineBackground.Enabled:=ckHighlightActiveLine.Checked;
end;

procedure TFrmOptions.UpdateHighlightMatches;
begin
  lbHighlightMatchesBackground.Enabled:=ckHighlightMatches.Checked;
  clbHighlightMatchesBackground.Enabled:=ckHighlightMatches.Checked;
end;


procedure TFrmOptions.ckUseHighLighterClick(Sender: TObject);
begin
  UpdateHighlighter;
end;

procedure TFrmOptions.btFontClick(Sender: TObject);
begin
  dlgFont.Font:=pnFont.Font;
  if not dlgFont.Execute then
    Exit;
  UpdateEditorFont(dlgFont.Font);
end;

procedure TFrmOptions.btClearMRUClick(Sender: TObject);
begin
  if MessageDlg(sClearMRU,mtWarning,mbYesNo,0)=mrYes then
    FrmMain.MRUList.Clear;
end;

procedure TFrmOptions.btBrowseTemplatesPathClick(Sender: TObject);
begin
  dlgPathTemplates.FileName:=ExcludeTrailingBackslash(edPathTemplates.Text);
  dlgPathTemplates.InitialDir:=ExtractFileDir(edPathTemplates.Text);
  Application.ProcessMessages;
  if not dlgPathTemplates.Execute then
    Exit;
  edPathTemplates.Text:=AppendPathDelim(TrimFilename(dlgPathTemplates.FileName));
end;

procedure TFrmOptions.btBrowsePathCCXClick(Sender: TObject);
var
  fn: string;
begin
  case TComponent(Sender).Tag of
    0: fn:=edPathCCX.Text;
    1: fn:=edPathCGX.Text;
    2: fn:=edPathTerminal.Text;
  end;
  dlgOpenProgram.InitialDir:=ExtractFilePath(fn);
  dlgOpenProgram.FileName:=ExtractFileName(fn);
  Application.ProcessMessages;
  if not dlgOpenProgram.Execute then
    Exit;
  case TComponent(Sender).Tag of
    0: edPathCCX.Text:=dlgOpenProgram.FileName;
    1: edPathCGX.Text:=dlgOpenProgram.FileName;
    2: edPathTerminal.Text:=dlgOpenProgram.FileName;
  end;
end;

procedure TFrmOptions.UpdateGuter;
var
  i: integer;
begin
  lbShowOnlyLineNumbersMultiplesOf.Enabled:=ckShowGutter.Checked;
  edShowOnlyLineNumbersMultiplesOf.Enabled:=ckShowGutter.Checked;
  ckGutterOptions.Enabled:=ckShowGutter.Checked;
  for i:=0 to gbGutterColors.ControlCount-1 do
    gbGutterColors.Controls[i].Enabled:=ckShowGutter.Checked;
end;

procedure TFrmOptions.ckShowGutterClick(Sender: TObject);
begin
  UpdateGuter;
end;


procedure TFrmOptions.btResetClick(Sender: TObject);
begin
  // General
  ckShowHints.Checked:=true;
  ckStoreFormPlacement.Checked:=true;
  ckUniqueInstance.Checked:=true;
  ckFileAssociation.Checked:=false;
  edPathTemplates.Text:=FrmMain.Config.DefTemplatesPath;
  edMaxMHU.Value:=10;

  // Display
  pnFont.Font.Name:=SynDefaultFontName;
  pnFont.Font.Size:=SynDefaultFontSize;
  pnFont.Font.Height:=SynDefaultFontHeight;
  pnFont.Font.Pitch:=SynDefaultFontPitch;
  pnFont.Font.Quality:=fqDraft;
  clbSelectedColorForeground.ButtonColor:=clHighlightText;
  clbSelectedColorBackground.ButtonColor:=clHighlight;
  ckHighlightActiveLine.Checked:=true;
  clbHighlightActiveLineBackground.ButtonColor:=$00FFE8E8;
  ckHighlightMatches.Checked:=true;
  clbHighlightMatchesBackground.ButtonColor:=$009BFF9B;
  edRightEdge.Value:=80;
  clbRightEdgeColor.ButtonColor:=clSilver;
  ckShowGutter.Checked:=true;
  edShowOnlyLineNumbersMultiplesOf.Value:=5;
  ckGutterOptions.Checked[0]:=true;
  ckGutterOptions.Checked[1]:=true;
  ckGutterOptions.Checked[2]:=false;
  ckGutterOptions.Checked[3]:=false;
  ckGutterOptions.Checked[4]:=true;
  ckGutterOptions.Checked[5]:=true;
  clbGutterColor.ButtonColor:=clBtnFace;
  clbModifiedColor.ButtonColor:=$0000E9FC;
  clbSavedColor.ButtonColor:=clGreen;

  // Editor options
  edUndoLimit.Value:=32768;
  edTabWidth.Value:=4;
  edExtraLineSpacing.Value:=0;
  edExtraCharSpacing.Value:=0;
  cbInsertCaret.ItemIndex:=Ord(ctVerticalLine);
  cbOverwriteCaret.ItemIndex:=Ord(ctBlock);
  ckScrollToEnd.Checked:=true;

  ckEditOptionsGroupUndo.Checked:=false;
  ckEditOptionsAutoIndent.Checked:=true;
  ckEditOptionsDragDropEditing.Checked:=true;
  ckEditOptionsAltSetsColumnMode.Checked:=true;
  ckEditOptionsKeepCaretX.Checked:=true;
  ckEditOptionsSmartTabs.Checked:=true;
  ckEditOptionsSmartTabDelete.Checked:=false;
  ckEditOptionsRightMouseMovesCursor.Checked:=false;
  ckEditOptionsEnhanceHomeKey.Checked:=false;
  ckEditOptions2EnhanceEndKey.Checked:=false;
  ckEditOptionsHideShowScrollbars.Checked:=false;
  ckHideSelection.Checked:=false;
  ckEditOptionsHalfPageScroll.Checked:=false;
  ckEditOptionsScrollByOneLess.Checked:=false;
  ckEditOptionsScrollPastEOF.Checked:=false;
  ckEditOptionsScrollPastEOL.Checked:=true;
  ckEditOptionsShowScrollHint.Checked:=true;
  ckWantTabs.Checked:=true;
  ckEditOptionsTabsToSpaces.Checked:=true;
  ckEditOptionsTrimTrailingSpaces.Checked:=true;
  ckEditOptions2CaretSkipsSelection.Checked:=false;
  ckEditOptions2CaretSkipTab.Checked:=false;
  ckEditOptions2AlwaysVisibleCaret.Checked:=false;
  ckEditOptions2FoldedCopyPaste.Checked:=true;
  ckEditOptions2PersistentBlock.Checked:=false;
  ckEditOptions2OverwriteBlock.Checked:=true;
  ckEditOptions2AutoHideCursor.Checked:=false;
  ckEditOptions2ColorSelectionTillEol.Checked:=false;
  ckEditMouseOptionsAltSetsColumnMode.Checked:=false;
  ckEditMouseOptionsDoubleClickSelectsLine.Checked:=false;
  ckEditMouseOptionsCtrlWheelZoom.Checked:=true;
  ckCompletionOnAsterick.Checked:=true;

  // Syntax Highligthing
  ckUseHighLighter.Checked:=true;
  ResetHighlighterAttributes;

  // update controls
  UpdateGuter;
  UpdateHighlighter;
  UpdateHighlightActiveLine;
  UpdateHighlightMatches;
  // Set Reset Flag
  fDoReset:=true;
end;

procedure TFrmOptions.ckHighlightActiveLineClick(Sender: TObject);
begin
  UpdateHighlightActiveLine;
end;

procedure TFrmOptions.ckHighlightMatchesClick(Sender: TObject);
begin
  UpdateHighlightMatches;
end;

procedure TFrmOptions.ResetHighlighterAttributes;
var
  i: Integer;
  a: TSynHighlighterAttributes;
begin
  for i:=0 to lbEditAttr.Items.Count-1 do begin
    if Assigned(lbEditAttr.Items.Objects[i])
    and (lbEditAttr.Items.Objects[i] is TSynHighlighterAttributes) then begin
      a:=TSynHighlighterAttributes(lbEditAttr.Items.Objects[i]);
      a.Clear;
      if a.StoredName=SYNS_AttrComment then begin
        a.Foreground:=clGreen;
        a.Style:=[fsItalic];
      end else
      if a.StoredName=SYNS_AttrInclude then begin
        a.Foreground:=clGreen;
        a.Style:=[fsBold];
      end else
      if a.StoredName=SYNS_AttrIncludeFName then begin
        a.Foreground:=clBlue;
      end else
      if a.StoredName=SYNS_AttrIdentifier then begin
        a.Foreground:=clTeal;
      end else
      if a.StoredName=SYNS_AttrNonReservedKeyword then begin
        a.Foreground:=clNavy;
        a.Style:=[fsBold];
      end else
      if a.StoredName=SYNS_AttrNumber then begin
        a.Foreground:=$00A00000;
      end else
      if a.StoredName=SYNS_AttrReservedWord then begin
        a.Foreground:=clBlack;
        a.Style:=[fsBold];
      end else
      if a.StoredName=SYNS_AttrSymbol then begin
        a.Foreground:=clRed;
      end else
      if a.StoredName=SYNS_AttrText then begin
        a.Foreground:=clBlack;
      end else
      if a.StoredName=SYNS_AttrIllegalChar then begin
        a.Foreground:=clRed;
      end;
    end;
  end;
end;

end.

