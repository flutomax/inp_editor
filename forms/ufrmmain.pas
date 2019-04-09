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

{%RunCommand $MakeExe($(EdFile)) -m H:\Work\calculix\CL32-win32\hlp\examples\nl\nl-buckl\_nlb.inp}
unit uFrmMain;

{$mode objfpc}{$H+}
{$I general.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLType,
  Types, Menus, ActnList, ComCtrls, StdActns, IniPropStorage, ExtCtrls, Buttons,
  AsyncProcess, PopupNotifier, LazFileUtils, SynEdit, SynEditTypes,
  PrintersDlgs, SynEditPrint, uConsts, uUiComp, uInpEditor, uEditorMisc,
  uSynEditorOptions, uConfig, uMRUList, uDerivedClasses, uFrmModelViewer;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    cmdViewTabCloseBtnVisible: TAction;
    cmdToolsMonitor: TAction;
    cmdFileShowLocation: TAction;
    cmdAddBCPressure: TAction;
    cmdEditCut: TAction;
    cmdAddBCFaces: TAction;
    cmdToolsGroupView: TAction;
    apRuner: TAsyncProcess;
    cmdViewTbCalculix: TAction;
    cmdCalculixPreFlagStl: TAction;
    cmdCalculixPreFlagStep: TAction;
    cmdCalculixPreFlagNg: TAction;
    cmdCalculixPreFlagFoam: TAction;
    cmdCalculixPreFlagIsaac3d: TAction;
    cmdCalculixPreFlagIsaac2d: TAction;
    cmdCalculixPreFlagDuns3d: TAction;
    cmdCalculixPreFlagDuns2d: TAction;
    cmdCalculixPreFlagB: TAction;
    cmdCalculixPreFlagC: TAction;
    cmdCalculixPostCGX: TAction;
    cmdCalculixPreCGX: TAction;
    cmdCalculixSolver: TAction;
    cmdToolsModelViewer: TAction;
    cmdToolsNodesTransform: TAction;
    cmdFileOpenFileAtCursor: TAction;
    cmdEditEolMacintosh: TAction;
    cmdEditEolUnix: TAction;
    cmdEditEolWindows: TAction;
    cmdFindClearBookmarks: TAction;
    cmdViewStatusbar: TAction;
    cmdViewTbReset: TAction;
    cmdViewTbTools: TAction;
    cmdHelpEditorKeystrokes: TAction;
    cmdHelpHomepage: TAction;
    cmdToolsExportHTML: TAction;
    cmdToolsExportRTF: TAction;
    cmdFileReload: TAction;
    cmdEditRemoveEmptyLines: TAction;
    cmdEditJoinLines: TAction;
    cmdEditSplitLines: TAction;
    cmdEditSelectPara: TAction;
    cmdEditSelectLine: TAction;
    cmdEditDuplicateLine: TAction;
    cmdEditToggleCase: TAction;
    cmdEditSentenceCase: TAction;
    cmdEditTitleCase: TAction;
    cmdEditLowerCase: TAction;
    cmdEditUpperCase: TAction;
    cmdEditReduceMultipleSpaces: TAction;
    cmdEditTabToSpaces: TAction;
    cmdEditTrimAll: TAction;
    cmdEditTrimTrailing: TAction;
    cmdEditTrimLeading: TAction;
    cmdViewTbView: TAction;
    cmdViewTbSearch: TAction;
    cmdViewTbEdit: TAction;
    cmdViewTbFile: TAction;
    btFileClose: TSpeedButton;
    cbMain: TCoolBar;
    cmdViewZoomDefault: TAction;
    cmdViewZoomOut: TAction;
    cmdViewZoomIn: TAction;
    cmdViewSpecialChars: TAction;
    cmdViewUnfoldCurrent: TAction;
    cmdViewFoldCurrent: TAction;
    cmdToolsOptions: TAction;
    AlMain: TActionList;
    AppProperties: TApplicationProperties;
    cmdEditCommentSelected: TAction;
    cmdEditRedo: TAction;
    cmdEditUncommentSelected: TAction;
    cmdFileClose: TAction;
    cmdFileCloseAll: TAction;
    cmdFileNew: TAction;
    cmdFilePrint: TAction;
    cmdFilePrintSetup: TAction;
    cmdFileSave: TAction;
    cmdFileSaveAll: TAction;
    cmdFindGotoBookmark0: TAction;
    cmdFindGotoBookmark1: TAction;
    cmdFindGotoBookmark2: TAction;
    cmdFindGotoBookmark3: TAction;
    cmdFindGotoBookmark4: TAction;
    cmdFindGotoBookmark5: TAction;
    cmdFindGotoBookmark6: TAction;
    cmdFindGotoBookmark7: TAction;
    cmdFindGotoBookmark8: TAction;
    cmdFindGotoBookmark9: TAction;
    cmdFindGotoLine: TAction;
    cmdFindNext: TAction;
    cmdFindPrevious: TAction;
    cmdFindReplace: TAction;
    cmdFindSearch: TAction;
    cmdFindToggleBookmark0: TAction;
    cmdFindToggleBookmark1: TAction;
    cmdFindToggleBookmark2: TAction;
    cmdFindToggleBookmark3: TAction;
    cmdFindToggleBookmark4: TAction;
    cmdFindToggleBookmark5: TAction;
    cmdFindToggleBookmark6: TAction;
    cmdFindToggleBookmark7: TAction;
    cmdFindToggleBookmark8: TAction;
    cmdFindToggleBookmark9: TAction;
    cmdViewFoldAll: TAction;
    cmdViewUnfoldAll: TAction;
    cmdHelpAbout: TAction;
    cmdOptionsRemoveComents: TAction;
    DlgSave: TSaveDialog;
    cmdFileOpen: TFileOpen;
    cmdFileSaveAs: TFileSaveAs;
    cmdFileExit: TFileExit;
    cmdEditUndo: TEditUndo;
    cmdEditCopy: TAction;
    cmdEditPaste: TEditPaste;
    cmdEditDelete: TEditDelete;
    cmdEditSelectAll: TEditSelectAll;
    cmdEditIncludeFile: TFileOpen;
    cmdFileImport: TFileOpen;
    Find: TMenuItem;
    IlBookmark: TImageList;
    IlDMain: TImageList;
    IlMain: TImageList;
    IlStatus: TImageList;
    IlTabs: TImageList;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem100: TMenuItem;
    MenuItem101: TMenuItem;
    MenuItem102: TMenuItem;
    MenuItem103: TMenuItem;
    MenuItem104: TMenuItem;
    MenuItem105: TMenuItem;
    MenuItem106: TMenuItem;
    MenuItem107: TMenuItem;
    MenuItem108: TMenuItem;
    MenuItem109: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem110: TMenuItem;
    MenuItem111: TMenuItem;
    MenuItem112: TMenuItem;
    MenuItem113: TMenuItem;
    MenuItem114: TMenuItem;
    MenuItem115: TMenuItem;
    MenuItem116: TMenuItem;
    MenuItem117: TMenuItem;
    MenuItem118: TMenuItem;
    MenuItem119: TMenuItem;
    MenuItem120: TMenuItem;
    MenuItem121: TMenuItem;
    MenuItem122: TMenuItem;
    MenuItem123: TMenuItem;
    MenuItem124: TMenuItem;
    MenuItem125: TMenuItem;
    MenuItem126: TMenuItem;
    MenuItem127: TMenuItem;
    MenuItem128: TMenuItem;
    MenuItem129: TMenuItem;
    MenuItem130: TMenuItem;
    MenuItem131: TMenuItem;
    MenuItem132: TMenuItem;
    MenuItem133: TMenuItem;
    MenuItem134: TMenuItem;
    MenuItem135: TMenuItem;
    MenuItem136: TMenuItem;
    MenuItem137: TMenuItem;
    MenuItem138: TMenuItem;
    MenuItem139: TMenuItem;
    MenuItem140: TMenuItem;
    MenuItem141: TMenuItem;
    MenuItem142: TMenuItem;
    MenuItem143: TMenuItem;
    MenuItem144: TMenuItem;
    MenuItem145: TMenuItem;
    MenuItem146: TMenuItem;
    MenuItem147: TMenuItem;
    MenuItem148: TMenuItem;
    MenuItem149: TMenuItem;
    MenuItem150: TMenuItem;
    MenuItem151: TMenuItem;
    MenuItem152: TMenuItem;
    MenuItem153: TMenuItem;
    MenuItem154: TMenuItem;
    MenuItem155: TMenuItem;
    MenuItem156: TMenuItem;
    MenuItem157: TMenuItem;
    MenuItem158: TMenuItem;
    MenuItem159: TMenuItem;
    MenuItem160: TMenuItem;
    MenuItem161: TMenuItem;
    MenuItem162: TMenuItem;
    MenuItem163: TMenuItem;
    MenuItem164: TMenuItem;
    MenuItem165: TMenuItem;
    MenuItem166: TMenuItem;
    MenuItem167: TMenuItem;
    MenuItem168: TMenuItem;
    MenuItem169: TMenuItem;
    MenuItem173: TMenuItem;
    MenuItem174: TMenuItem;
    MenuItem175: TMenuItem;
    MenuItem176: TMenuItem;
    MenuItem177: TMenuItem;
    MenuItem178: TMenuItem;
    MenuItem179: TMenuItem;
    MenuItem180: TMenuItem;
    MenuItem181: TMenuItem;
    MenuItem182: TMenuItem;
    MenuItem183: TMenuItem;
    MenuItem184: TMenuItem;
    MenuItem185: TMenuItem;
    MenuItem186: TMenuItem;
    MenuItem187: TMenuItem;
    MenuItem188: TMenuItem;
    MenuItem189: TMenuItem;
    MenuItem190: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem81: TMenuItem;
    msOpenFileAtCursor: TMenuItem;
    miOpenFileAtCursor: TMenuItem;
    miEndOfLine: TMenuItem;
    MenuItem170: TMenuItem;
    MenuItem171: TMenuItem;
    MenuItem172: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    miTabs: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    MenuItem99: TMenuItem;
    MiDelete: TMenuItem;
    miEdit: TMenuItem;
    miFile: TMenuItem;
    miFind: TMenuItem;
    miHelp: TMenuItem;
    miTools: TMenuItem;
    miRecentFile1: TMenuItem;
    miRecentFile2: TMenuItem;
    miRecentFile3: TMenuItem;
    miRecentFile4: TMenuItem;
    miRecentFile5: TMenuItem;
    miTemplates: TMenuItem;
    MnMain: TMainMenu;
    msRfBot: TMenuItem;
    msRfTop: TMenuItem;
    pnTbConteiner: TPanel;
    pnBtnFileClose: TPanel;
    pnToolbar: TPanel;
    pnClose: TPanel;
    PmEditor: TPopupMenu;
    PmPager: TPopupMenu;
    pmPreFlags: TPopupMenu;
    sbEditor: TStatusBar;
    tbEdit: TToolBar;
    tbFile: TToolBar;
    tbSearch: TToolBar;
    tbTools: TToolBar;
    tbView: TToolBar;
    tbCalculix: TToolBar;
    ToolButton1: TToolButton;
    ToolButton100: TToolButton;
    ToolButton101: TToolButton;
    ToolButton103: TToolButton;
    ToolButton105: TToolButton;
    ToolButton106: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton62: TToolButton;
    ToolButton63: TToolButton;
    ToolButton64: TToolButton;
    ToolButton65: TToolButton;
    ToolButton66: TToolButton;
    ToolButton67: TToolButton;
    ToolButton68: TToolButton;
    ToolButton69: TToolButton;
    ToolButton70: TToolButton;
    ToolButton73: TToolButton;
    ToolButton74: TToolButton;
    ToolButton75: TToolButton;
    ToolButton76: TToolButton;
    ToolButton77: TToolButton;
    ToolButton78: TToolButton;
    ToolButton79: TToolButton;
    ToolButton80: TToolButton;
    ToolButton83: TToolButton;
    ToolButton84: TToolButton;
    ToolButton92: TToolButton;
    ToolButton93: TToolButton;
    ToolButton94: TToolButton;
    ToolButton95: TToolButton;
    ToolButton96: TToolButton;
    ToolButton97: TToolButton;
    ToolButton98: TToolButton;
    ToolButton99: TToolButton;
    procedure AlMainUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure AppPropertiesActivate(Sender: TObject);
    procedure AppPropertiesDropFiles(Sender: TObject;
      const FileNames: array of String);
    procedure AppPropertiesException(Sender: TObject; E: Exception);
    procedure AppPropertiesHint(Sender: TObject);
    procedure AppPropertiesShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure cmdAddBCFacesExecute(Sender: TObject);
    procedure cmdCalculixPreFlagCExecute(Sender: TObject);
    procedure cmdCalculixSolverExecute(Sender: TObject);
    procedure cmdEditCommentSelectedExecute(Sender: TObject);
    procedure cmdEditCopyExecute(Sender: TObject);
    procedure cmdEditDuplicateLineExecute(Sender: TObject);
    procedure cmdEditEolWindowsExecute(Sender: TObject);
    procedure cmdEditIncludeFileAccept(Sender: TObject);
    procedure cmdEditIncludeFileBeforeExecute(Sender: TObject);
    procedure cmdEditJoinLinesExecute(Sender: TObject);
    procedure cmdEditRedoExecute(Sender: TObject);
    procedure cmdEditRemoveEmptyLinesExecute(Sender: TObject);
    procedure cmdEditSelectLineExecute(Sender: TObject);
    procedure cmdEditSelectParaExecute(Sender: TObject);
    procedure cmdEditSplitLinesExecute(Sender: TObject);
    procedure cmdEditTrimLeadingExecute(Sender: TObject);
    procedure cmdEditUncommentSelectedExecute(Sender: TObject);
    procedure cmdFileCloseAllExecute(Sender: TObject);
    procedure cmdFileCloseExecute(Sender: TObject);
    procedure cmdFileImportAccept(Sender: TObject);
    procedure cmdFileNewExecute(Sender: TObject);
    procedure cmdFileOpenAccept(Sender: TObject);
    procedure cmdFileOpenBeforeExecute(Sender: TObject);
    procedure cmdFileOpenFileAtCursorExecute(Sender: TObject);
    procedure cmdFilePrintExecute(Sender: TObject);
    procedure cmdFilePrintSetupExecute(Sender: TObject);
    procedure cmdFileReloadExecute(Sender: TObject);
    procedure cmdFileSaveAsAccept(Sender: TObject);
    procedure cmdFileSaveAsBeforeExecute(Sender: TObject);
    procedure cmdFileSaveExecute(Sender: TObject);
    procedure cmdFileShowLocationExecute(Sender: TObject);
    procedure cmdFindClearBookmarksExecute(Sender: TObject);
    procedure cmdFindGotoBookmark0Execute(Sender: TObject);
    procedure cmdFindGotoLineExecute(Sender: TObject);
    procedure cmdFindNextExecute(Sender: TObject);
    procedure cmdFindReplaceExecute(Sender: TObject);
    procedure cmdFindSearchExecute(Sender: TObject);
    procedure cmdFindToggleBookmark0Execute(Sender: TObject);
    procedure cmdHelpAboutExecute(Sender: TObject);
    procedure cmdHelpEditorKeystrokesExecute(Sender: TObject);
    procedure cmdHelpHomepageExecute(Sender: TObject);
    procedure cmdOptionsRemoveComentsExecute(Sender: TObject);
    procedure cmdToolsExportRTFExecute(Sender: TObject);
    procedure cmdToolsGroupViewExecute(Sender: TObject);
    procedure cmdToolsModelViewerExecute(Sender: TObject);
    procedure cmdToolsMonitorExecute(Sender: TObject);
    procedure cmdToolsNodesTransformExecute(Sender: TObject);
    procedure cmdToolsOptionsExecute(Sender: TObject);
    procedure cmdViewSpecialCharsExecute(Sender: TObject);
    procedure cmdViewStatusbarExecute(Sender: TObject);
    procedure cmdViewTabCloseBtnVisibleExecute(Sender: TObject);
    procedure cmdViewTbFileExecute(Sender: TObject);
    procedure cmdViewTbResetExecute(Sender: TObject);
    procedure cmdViewUnfoldCurrentExecute(Sender: TObject);
    procedure cmdViewFoldAllExecute(Sender: TObject);
    procedure cmdViewFoldCurrentExecute(Sender: TObject);
    procedure cmdViewUnfoldAllExecute(Sender: TObject);
    procedure cmdViewZoomInExecute(Sender: TObject);
    procedure cmdEditCutExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miEndOfLineClick(Sender: TObject);
    procedure miTabsClick(Sender: TObject);
    procedure PmEditorPopup(Sender: TObject);
    procedure pmPreFlagsPopup(Sender: TObject);
    procedure pnBtnFileClosePaint(Sender: TObject);
    procedure pnToolbarPaint(Sender: TObject);
    procedure sbEditorDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  private
    fMRUList: TMRUList;
    fPrint: TSynEditPrint;
    fUI: TUniqueInstance;
    fEditorOptions: TSynEditorOptionsStorage;
    fConfig: TConfig;
    fPager: TInpEditPager;
    function GetIsActiveEditor: Boolean;
    function GetActiveEditor: TInpEditor;
    function GetFileName(Editor: TInpEditor): Boolean;
    function GetReadOnly(Editor: TInpEditor): Boolean;
    procedure LoadIni;
    procedure SaveIni;
    procedure PagerChange(Sender: TObject);
    procedure SetupBookmark;
    procedure SetupToolbar;
    procedure OtherInstanceRun(Sender: TObject; aParamCount: Integer;
      const aParameters: array of string);
    procedure RecentFileClick(Sender: TObject; const aFileName: string);
    procedure PagerBeforeClose(Editor: TInpEditor; var Cancel: boolean);
    procedure PagerStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure UpdateActiveTab(Data: PtrInt);
    procedure UpdateStatusHint(const aHint: string);
    procedure UpdateCloseBtn;
    procedure PasteFromFile(const aFileName: TFileName);
    procedure MakeTemplatesMenu;
    procedure TemplatesSearch(const Path: TFileName; MenuItem: TMenuItem);
    procedure TemplateMenuClick(Sender: TObject);
    procedure PagerShowTabs(Sender: TObject);
    procedure cbMainPaint(Sender: TObject);
    procedure cbMainPostPaint(Data: PtrInt);
    procedure ThemeServicesThemeChange(Sender: TObject);
  public
    procedure Open(const aFileName: string = '');
    property Pager: TInpEditPager read fPager;
    property ActiveEditor: TInpEditor read GetActiveEditor;
    property IsActiveEditor: Boolean read GetIsActiveEditor;
    property EditorOptions: TSynEditorOptionsStorage read fEditorOptions;
    property Config: TConfig read fConfig;
    property MRUList: TMRUList read fMRUList;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses
  InterfaceBase, LazUTF8, Math, StrUtils, LCLIntf, Themes, SynEditKeyCmds,
  uFrmGotoLine, uFrmOptions, uFrmTextSearch, uFrmTextReplace, uFrmExportText,
  uFrmAbout, uFrmEditorKeystrokes, uFrmNodesTransform, uFrmViewGroup,
  uFrmGroupSelector, ufrmFileImport, uFrmMonitor, uHighliter, uFileUtils,
  uCalculix, uDialogs, uAddBCFunctions, SynEditMouseCmds;

{ TFrmMain }

// coomon form procedure

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  fEditorOptions:=TSynEditorOptionsStorage.Create(self);
  fConfig:=TConfig.Create(self);
  fConfig.MainForm:=self;
  fMRUList:=TMRUList.Create(self);
  fMRUList.MIRecent:=miFile;
  fMRUList.TopSepar:=msRfTop;
  fMRUList.BotSepar:=msRfBot;
  fMRUList.OnRecent:=@RecentFileClick;
  fPager:=TInpEditPager.Create(self);
  fPager.Align:=alClient;
  fPager.Images:=IlTabs;
  fPager.PopupMenu:=PmPager;
  fPager.Parent:=Self;
  fPager.OnChange:=@PagerChange;
  fPager.OnBeforeClose:=@PagerBeforeClose;
  fPager.OnStatusChange:=@PagerStatusChange;
  LoadIni;
  fUI:=TUniqueInstance.Create(self);
  fUI.Identifier:=sAppTitle;
  fUI.Enabled:=fConfig.UniqueInstance;
  fUI.OnOtherInstance:=@OtherInstanceRun;
  fPrint:=TSynEditPrint.Create(self);
  fPrint.Colors:=true;
  cmdViewStatusbar.Checked:=fConfig.StatusbarVisible;
  sbEditor.Visible:=cmdViewStatusbar.Checked;
  cmdViewTabCloseBtnVisible.Checked:=fConfig.TabCloseBtnVisible;
  UpdateCloseBtn;
  DrawDisabledImagelist(IlMain,IlDMain);
  cbMain.OnPaint:=@cbMainPaint;
  ThemeServices.OnThemeChange:=@ThemeServicesThemeChange;
  SetupBookmark;
  MakeTemplatesMenu;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  fUI.Enabled:=false;
  SaveIni;
end;

procedure TFrmMain.FormResize(Sender: TObject);
begin
  cbMain.AutosizeBands;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Assigned(fPager) and (fPager.PageCount>0) then
    CanClose:=fPager.CloseAll
  else
    CanClose:=true;
end;

procedure TFrmMain.FormShow(Sender: TObject);
var
  i: Integer;
begin
  SetupToolbar;
  fMRUList.UpdateRecentFiles;
  fUI.Loaded;
  if ParamCount>0 then
    for i:=1 to ParamCount do
      Open(ParamStr(i))
  else
    Open;
  ThemeServicesThemeChange(nil);
end;


// Actions

procedure TFrmMain.AlMainUpdate(AAction: TBasicAction; var Handled: Boolean);
var
  a,b: Boolean;
  e: TInpEditor;
  i: Integer;
  c: TContainedAction;
begin
  a:=IsActiveEditor;
  e:=ActiveEditor;
  b:=false;
  cmdFileReload.Enabled:=a and (not e.Unnamed) and ZFileExists(e.FileName);
  cmdFileShowLocation.Enabled:=cmdFileReload.Enabled;
  cmdFileSave.Enabled:=a and e.Modified;
  cmdFileSaveAs.Enabled:=a;
  cmdFilePrint.Enabled:=a;
  cmdEditUndo.Enabled:=a and e.CanUndo;
  cmdEditRedo.Enabled:=a and e.CanRedo;
  cmdEditCopy.Enabled:=a and e.SelAvail;
  cmdEditCut.Enabled:=cmdEditCopy.Enabled;
  // in Linux CanPaste creates a bug
  {$IfDef Windows}
  cmdEditPaste.Enabled:=a and e.CanPaste;
  {$EndIf}
  cmdEditSelectAll.Enabled:=a and (e.Lines.Count>0);
  cmdEditSelectLine.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditSelectPara.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditIncludeFile.Enabled:=a;
  cmdEditEolWindows.Enabled:=a;
  cmdEditEolMacintosh.Enabled:=a;
  cmdEditEolUnix.Enabled:=a;
  cmdFindSearch.Enabled:=cmdEditSelectAll.Enabled;
  cmdFindReplace.Enabled:=cmdEditSelectAll.Enabled;
  cmdFindNext.Enabled:=cmdEditSelectAll.Enabled;
  cmdFindPrevious.Enabled:=cmdEditSelectAll.Enabled;
  cmdFindGotoLine.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditCommentSelected.Enabled:=cmdEditCopy.Enabled;
  cmdEditUncommentSelected.Enabled:=cmdEditCopy.Enabled and (Pos('**',e.SelText)<>0);
  cmdEditTrimLeading.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditTrimTrailing.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditTrimAll.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditTabToSpaces.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditReduceMultipleSpaces.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditUpperCase.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditLowerCase.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditTitleCase.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditSentenceCase.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditToggleCase.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditDuplicateLine.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditSplitLines.Enabled:=cmdEditSelectAll.Enabled;
  cmdEditJoinLines.Enabled:=cmdEditCopy.Enabled;
  cmdEditRemoveEmptyLines.Enabled:=cmdEditSelectAll.Enabled;
  cmdToolsExportRTF.Enabled:=cmdEditSelectAll.Enabled;
  cmdToolsExportHTML.Enabled:=cmdEditSelectAll.Enabled;
  cmdToolsModelViewer.Enabled:=cmdEditSelectAll.Enabled;
  cmdToolsMonitor.Enabled:=cmdFileReload.Enabled;
  cmdToolsNodesTransform.Enabled:=cmdEditSelectAll.Enabled;
  cmdToolsGroupView.Enabled:=cmdEditSelectAll.Enabled;
  cmdAddBCFaces.Enabled:=cmdEditSelectAll.Enabled;
  cmdAddBCPressure.Enabled:=cmdEditSelectAll.Enabled;
  cmdCalculixSolver.Enabled:=cmdEditSelectAll.Enabled and Config.TerminalExists;
  cmdCalculixPreCGX.Enabled:=cmdEditSelectAll.Enabled and Config.TerminalExists;
  cmdCalculixPostCGX.Enabled:=cmdEditSelectAll.Enabled and Config.TerminalExists;

  // update bookmarks
  for i:=0 to 9 do begin
    c:=AlMain.ActionByName(Format('cmdFindToggleBookmark%d',[i]));
    b:=b or e.IsBookmark(i);
    if Assigned(c) and (c is TCustomAction) then begin
      TCustomAction(c).Enabled:=a;
      TCustomAction(c).Checked:=a and e.IsBookmark(i);
    end;
    c:=AlMain.ActionByName(Format('cmdFindGotoBookmark%d',[i]));
    if Assigned(c) and (c is TCustomAction) then
      TCustomAction(c).Enabled:=a and e.IsBookmark(i);
  end;
  cmdFindClearBookmarks.Enabled:=a and b;

  cmdViewFoldAll.Enabled:=a;
  cmdViewUnfoldAll.Enabled:=a;
  cmdViewFoldCurrent.Enabled:=a;
  cmdViewUnfoldCurrent.Enabled:=a;
  cmdViewSpecialChars.Enabled:=a;
  cmdViewSpecialChars.Checked:=a and (eoShowSpecialChars in e.Options);
  cmdViewZoomIn.Enabled:=a and e.CanZoomIn;
  Handled:=true;
end;


procedure TFrmMain.cmdEditCopyExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.CopyToClipboard;
end;

procedure TFrmMain.cmdEditCutExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.CutToClipboard;
end;

// Statusbar

procedure TFrmMain.sbEditorDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  t: integer;
  ts: TTextStyle;
  r: TRect;
begin
  r:=Rect;
  if Panel.Index=3 then begin
    t:=Rect.Top+(Rect.Bottom-Rect.Top-IlStatus.Height) div 2;
    IlStatus.Draw(StatusBar.Canvas,Rect.Left,t,StatusBar.Tag);
    r.Left:=Rect.Left+IlStatus.Width+4;
    Dec(r.Right,StatusBar.Height);
  end;
  FillChar(ts,SizeOf(ts),0);
  ts.Layout:=tlCenter;
  ts.SingleLine:=true;
  ts.Clipping:=true;
  ts.EndEllipsis:=true;
  StatusBar.Canvas.TextRect(r,r.Left,0,Panel.Text,ts);
end;

// Ini file

procedure TFrmMain.LoadIni;
begin
  fConfig.LoadFromIni('Application Options');
  fConfig.LoadCoolBar('Toolbars',cbMain);
  fMRUList.LoadFromIni(fConfig.Ini,'Recent Files');
  fEditorOptions.LoadFromIni(fConfig.Ini,'Editor Options');
  fPager.LoadHighlighterFromIni(fConfig.Ini,'Highlighter');
end;

procedure TFrmMain.SaveIni;
begin
  fConfig.SaveToIni('Application Options');
  fConfig.SaveCoolBar('Toolbars',cbMain);
  fMRUList.SaveToIni(fConfig.Ini,'Recent Files');
  fEditorOptions.SaveToIni(fConfig.Ini,'Editor Options');
  fPager.SaveHighlighterToIni(fConfig.Ini,'Highlighter');
end;

// Bookmark;

procedure TFrmMain.SetupBookmark;
var
  i: Integer;
  b: TBitmap;
begin
  b:=TBitmap.Create;
  try
    for i:=20 to 29 do begin
      IlMain.GetBitmap(i,b);
      IlBookmark.Add(b,nil);
    end;
  finally
    b.Free;
  end;
end;

// Editor

function TFrmMain.GetActiveEditor: TInpEditor;
begin
  result:=fPager.ActiveEditor;
end;

function TFrmMain.GetIsActiveEditor: Boolean;
begin
  result:=Assigned(fPager.ActiveEditor);
end;

procedure TFrmMain.PagerChange(Sender: TObject);
begin
  Application.QueueAsyncCall(@UpdateActiveTab,0);
end;

procedure TFrmMain.UpdateActiveTab(Data: PtrInt);
var
  Editor: TInpEditor;
begin
  if Application.Terminated then
    Exit;  // without this - AV!!!
  if IsActiveEditor then begin
    Editor:=ActiveEditor;
    if Editor.CanFocus then
      Editor.SetFocus;
    Caption:=Format('%s - [%s]',[sAppTitle,Editor.Sheet.Title]);
    Application.Title:=Caption;
    PagerStatusChange(Editor,[scCaretX,scCaretY,scModified,scInsertMode]);
  end;
end;

procedure TFrmMain.PagerStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  Editor: TInpEditor;
begin
  if IsActiveEditor then begin
    Editor:=Sender as TInpEditor;
    if Changes*[scCaretX,scCaretY]<>[] then
      sbEditor.Panels[0].Text:=Format('%6d:%3d',[Editor.CaretY,Editor.CaretX]);
    if Changes*[scModified,scInsertMode]<>[] then begin
      if GetReadOnly(Editor) then begin
        Editor.Sheet.ImageIndex:=2;
        sbEditor.Panels[2].Text:=sStatusBarReadonly;
      end else begin
        sbEditor.Panels[2].Text:=IfThen(Editor.InsertMode,sStatusBarInsMode,sStatusBarOvrMode);
        Editor.Sheet.ImageIndex:=Ord(Editor.Modified);
      end;
      sbEditor.Panels[1].Text:=IfThen(Editor.Modified,sStatusBarModified);
    end;
    sbEditor.Panels[3].Text:=Editor.FileName;
    sbEditor.Tag:=0;
  end else begin
    sbEditor.Panels[0].Text:='';
    sbEditor.Panels[1].Text:='';
    sbEditor.Panels[2].Text:='';
    sbEditor.Panels[3].Text:='';
    sbEditor.Tag:=1;
  end;
end;

procedure TFrmMain.OtherInstanceRun(Sender: TObject; aParamCount: Integer;
  const aParameters: array of string);
var
  AppWnd: HWND;
begin
  if aParamCount>0 then
    Open(aParameters[0]);
  AppWnd:=Widgetset.AppHandle;
  if Widgetset.IsIconic(AppWnd) then
    Application.Restore;
  Widgetset.SetForegroundWindow(AppWnd);
end;

procedure TFrmMain.RecentFileClick(Sender: TObject; const aFileName: string);
begin
  Application.ProcessMessages;
  Open(aFileName);
end;

procedure TFrmMain.Open(const aFileName: string);
var
  b: Boolean;
begin
  b:=Screen.Cursor<>crHourGlass;
  if b then
    Screen.Cursor:=crHourGlass;
  try
    fPager.Open(aFileName);
    if ZFileExists(aFileName) then
      fMRUList.AddToRecent(aFileName);
  finally
    if b then
      Screen.Cursor:=crDefault;
  end;
end;

procedure TFrmMain.cmdFileOpenAccept(Sender: TObject);
var
  i: Integer;
begin
  Screen.Cursor:=crHourGlass;
  try
    for i:=0 to cmdFileOpen.Dialog.Files.Count-1 do
      Open(cmdFileOpen.Dialog.Files[i]);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TFrmMain.cmdFileOpenBeforeExecute(Sender: TObject);
begin
  if IsActiveEditor then
    TFileOpen(Sender).Dialog.InitialDir:=ExtractFilePath(ActiveEditor.FileName);
end;

procedure TFrmMain.cmdFileOpenFileAtCursorExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.OpenFileAtCursor;
end;

procedure TFrmMain.cmdFileImportAccept(Sender: TObject);
begin
  Application.CreateForm(TFrmPopupNotifier, FrmPopupNotifier);
  try
    FrmPopupNotifier.JobList:=cmdFileImport.Dialog.Files;
    FrmPopupNotifier.ShowModal;
  finally
    FrmPopupNotifier.Release;
  end;
end;

procedure TFrmMain.cmdFileNewExecute(Sender: TObject);
begin
  Open;
end;

procedure TFrmMain.cmdFileReloadExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.Reload;
end;

procedure TFrmMain.AppPropertiesDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: Integer;
begin
  Screen.Cursor:=crHourGlass;
  try
    for i:=0 to Length(FileNames)-1 do
      Open(FileNames[i]);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TFrmMain.PagerBeforeClose(Editor: TInpEditor; var Cancel: boolean);
var
  s: string;
begin
  if Editor.Modified then begin
    s:=IfThen(Editor.Unnamed,Editor.Sheet.Title,Editor.FileName);
    case MessageDlg(Format(sSaveChanges,[s]),mtWarning,mbYesNoCancel,0) of
      mrYes: begin
        if Editor.Unnamed or GetReadOnly(Editor) then
          if not GetFileName(Editor) then begin
            Cancel:=true;
            Exit;
          end;
        Cancel:=not Editor.Save;
      end;
      mrNo: Cancel:=false;
      mrCancel: Cancel:=true;
    end
  end
  else
    Cancel:=false
end;

procedure TFrmMain.cmdFileCloseExecute(Sender: TObject);
begin
  fPager.Close(ActiveEditor);
end;

procedure TFrmMain.cmdFileCloseAllExecute(Sender: TObject);
begin
  fPager.CloseAll;
end;

procedure TFrmMain.AppPropertiesActivate(Sender: TObject);
begin
  fPager.CheckFileChanges;
end;

procedure TFrmMain.UpdateStatusHint(const aHint: string);
begin
  if aHint='' then
    Exit;
  sbEditor.Panels[3].Text:=aHint;
  sbEditor.Tag:=1;
end;

function TFrmMain.GetFileName(Editor: TInpEditor): Boolean;
begin
  DlgSave.FileName:=IfThen(Editor.Unnamed,Editor.Sheet.Title,
    ExtractFileName(Editor.FileName));
  result:=DlgSave.Execute;
  if result then
    Editor.FileName:=DlgSave.FileName;
  Application.ProcessMessages;
end;

function TFrmMain.GetReadOnly(Editor: TInpEditor): Boolean;
begin
  Result:=(Editor.FileName<>'') and ZFileExists(Editor.FileName) and
    ZFileReadOnly(Editor.FileName);
end;

procedure TFrmMain.cmdFileSaveExecute(Sender: TObject);
var
  Editor: TInpEditor;
begin
  if not IsActiveEditor then
    Exit;
  Editor:=ActiveEditor;
  if Editor.Unnamed or GetReadOnly(Editor) then begin
    if not GetFileName(Editor) then
      Exit;
    fMRUList.AddToRecent(Editor.FileName);
  end;
  Editor.Save;
end;

procedure TFrmMain.cmdFileSaveAsAccept(Sender: TObject);
var
  Editor: TInpEditor;
begin
  if not IsActiveEditor then
    Exit;
  Editor:=ActiveEditor;
  Editor.SaveAs(cmdFileSaveAs.Dialog.FileName);
  fMRUList.AddToRecent(cmdFileSaveAs.Dialog.FileName);
end;

procedure TFrmMain.cmdFileSaveAsBeforeExecute(Sender: TObject);
var
  Editor: TInpEditor;
begin
  if not IsActiveEditor then
    Exit;
  Editor:=ActiveEditor;
  if ZFileExists(Editor.FileName) then begin
    cmdFileSaveAs.Dialog.InitialDir:=ExtractFilePath(Editor.FileName);
    cmdFileSaveAs.Dialog.FileName:=ExtractFileName(Editor.FileName);
    if LowerCase(ExtractFileExt(Editor.FileName))<>cmdFileSaveAs.Dialog.DefaultExt then
      cmdFileSaveAs.Dialog.FilterIndex:=2;
  end else
    cmdFileSaveAs.Dialog.FileName:=Editor.Sheet.Title;
end;


procedure TFrmMain.cmdFileShowLocationExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ShowFileLocation(ActiveEditor.FileName);
end;

// Print

procedure TFrmMain.cmdFilePrintExecute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  fPrint.SynEdit:=ActiveEditor;
  fPrint.Print;
end;

procedure TFrmMain.cmdFilePrintSetupExecute(Sender: TObject);
begin
  with TPrinterSetupDialog.Create(Self) do
  try
    Execute;
  finally
    Free;
  end;
end;


// Hints

procedure TFrmMain.AppPropertiesHint(Sender: TObject);
begin
  // forced hint event
  UpdateStatusHint(GetControlHint(Application.MouseControl));
end;

procedure TFrmMain.AppPropertiesShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  // delayed hint event
  CanShow:=fConfig.ShowHints;
  UpdateStatusHint(HintInfo.HintStr);
end;

// Exceptions

procedure TFrmMain.AppPropertiesException(Sender: TObject; E: Exception);
var
  x: Integer;
begin
  if Assigned(E) then
    x:=MessageDlg(Format(sAppError1,[E.Message,sAppError3]),mtError,[mbAbort,mbIgnore],0)
  else
    x:=MessageDlg(Format(sAppError2,[sAppError3]),mtError,[mbAbort,mbIgnore],0);
  if x=mrAbort then
    Halt(-1);   // Application.Terminate method is not close immediately
end;


// Commands


procedure TFrmMain.cmdEditRedoExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.Redo;
end;

procedure TFrmMain.cmdEditSelectLineExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.SelectLine;
end;

procedure TFrmMain.cmdEditSelectParaExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.SelectParagraph;
end;

procedure TFrmMain.cmdEditIncludeFileAccept(Sender: TObject);
var
  Editor: TInpEditor;
  s: string;
begin
  Application.ProcessMessages;
  if not IsActiveEditor then
    Exit;
  Editor:=ActiveEditor;
  if Editor.FileName='' then
    case MessageDlg(sReload,Format(sSaveWanted,[Editor.Sheet.Title]),
      mtWarning,mbYesNo,0) of
      mrYes: if GetFileName(Editor) then
                Editor.Save
              else
                Exit;
      mrNo: Exit;
    end;
  s:=ExtractFilePath(Editor.FileName);
  s:=ExtractRelativePath(s,cmdEditIncludeFile.Dialog.FileName);
  s:=Format(sInclude,[s]);
  ActiveEditor.TextToNewLine(s);
end;

procedure TFrmMain.cmdEditIncludeFileBeforeExecute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  cmdEditIncludeFile.Dialog.InitialDir:=ExtractFilePath(ActiveEditor.FileName);
end;

procedure TFrmMain.PasteFromFile(const aFileName: TFileName);
var
  List: TStringList;
  i: integer;
  s: string;
begin
  if not IsActiveEditor then
    Exit;
  List:=TStringList.Create;
  try
    List.LoadFromFile(aFileName);
    if cmdOptionsRemoveComents.Checked then // remove comments
      for i:=List.Count-1 downto 0 do begin
        s:=Trim(List[i]);
        if (Length(s)>1) and (s[1]='*') and (s[2]='*') then // detect comment
          List.Delete(i); // remove comment string
      end;
    ActiveEditor.TextToNewLine(List.Text);
  finally
    List.Free;
  end;
end;

procedure TFrmMain.cmdEditCommentSelectedExecute(Sender: TObject);
var
  lst: TStringList;
  i: Integer;
  s: string;
  Editor: TInpEditor;
begin
  if not IsActiveEditor then
    Exit;
  Editor:=ActiveEditor;
  lst:=TStringList.Create;
  try
    lst.Text:=Editor.SelText;
    for i:=0 to lst.Count-1 do begin
      s:=lst[i];
      if Copy(Trim(s),1,2)<>'**' then
        lst[i]:='**'+s;
    end;
    Editor.SelText:=TrimRight(lst.Text);
  finally
    lst.Free;
  end;
end;


procedure TFrmMain.cmdEditUncommentSelectedExecute(Sender: TObject);
var
  lst: TStringList;
  i: Integer;
  s: string;
  Editor: TInpEditor;
begin
  if not IsActiveEditor then
    Exit;
  Editor:=ActiveEditor;
  lst:=TStringList.Create;
  try
    lst.Text:=Editor.SelText;
    for i:=0 to lst.Count-1 do begin
      s:=lst[i];
      if Copy(Trim(s),1,2)='**' then
        Delete(s,Pos('**',s),2);
      lst[i]:=s;
    end;
    Editor.SelText:=TrimRight(lst.Text);
  finally
    lst.Free;
  end;
end;

procedure TFrmMain.cmdEditDuplicateLineExecute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  ActiveEditor.DuplicateLine;
end;

procedure TFrmMain.cmdEditSplitLinesExecute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  ActiveEditor.SplitLines;
end;


procedure TFrmMain.cmdEditJoinLinesExecute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  ActiveEditor.JoinLines;
end;


procedure TFrmMain.cmdEditRemoveEmptyLinesExecute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  ActiveEditor.RemoveEmptyLines;
end;

procedure TFrmMain.cmdEditTrimLeadingExecute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  case TAction(Sender).Tag of
    0: ActiveEditor.TextOperation(@TrimLeft);
    1: ActiveEditor.TextOperation(@TrimRight);
    2: ActiveEditor.TextOperation(@Trim);
    3: ActiveEditor.TextOperation(@TabsToSpace);
    4: ActiveEditor.TextOperation(@RemoveMultipleSpaces);
    5: ActiveEditor.TextOperation(@UTF8UpperString);
    6: ActiveEditor.TextOperation(@UTF8LowerString);
    7: ActiveEditor.TextOperation(@TitleCase);
    8: ActiveEditor.TextOperation(@SentenceCase);
    9: ActiveEditor.TextOperation(@ToggleCase)
  end;
end;

procedure TFrmMain.cmdEditEolWindowsExecute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  ActiveEditor.TextLineBreakStyle:=TTextLineBreakStyle(TAction(Sender).Tag);
end;

procedure TFrmMain.miEndOfLineClick(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  case ActiveEditor.TextLineBreakStyle of
    tlbsCRLF: cmdEditEolWindows.Checked:=true;
    tlbsCR: cmdEditEolMacintosh.Checked:=true;
    tlbsLF: cmdEditEolUnix.Checked:=true;
  end;
end;

// Search

procedure TFrmMain.cmdFindSearchExecute(Sender: TObject);
var
  Editor: TInpEditor;
begin
  if not IsActiveEditor then
    Exit;
  Editor:=ActiveEditor;
  with TFrmTextSearch.Create(Application) do
  try
    Setup(Editor);
    if ShowModal=mrOK then begin
      if Editor.SearchReplace(edSearchText.Text,'',GetOptions)=0 then
        MessageDlg(sSearchNotFound,Format(sSearchNotFoundMsg,[edSearchText.Text]),
        mtInformation,[mbOK],0);
    end;
  finally
    Release;
  end;
end;

procedure TFrmMain.cmdFindNextExecute(Sender: TObject);
label
  again;
var
  opt: TSynSearchOptions;
  Editor: TInpEditor;
  s: string;
  backward: Boolean;
  StartPos: TPoint;
begin
  if not IsActiveEditor then
    Exit;
  if LastFoundText='' then begin
    cmdFindSearch.Execute;
    Exit;
  end;
  Editor:=ActiveEditor;
  backward:=TAction(Sender).Tag=1;
  opt:=LastSearchOptions-[ssoEntireScope,ssoSelectedOnly];

  if backward then begin
    Include(opt,ssoBackwards);
    StartPos:=Editor.BlockBegin;
  end else begin
    Exclude(opt,ssoBackwards);
    StartPos:=Editor.BlockEnd;
  end;
again:
  if Editor.SearchReplaceEx(LastFoundText,'',opt,StartPos)=0 then begin
    s:=IfThen(backward,sSearchNotFoundContEnd,sSearchNotFoundContBegin);
    if MessageDlg(sSearchNotFound,Format(s,[LastFoundText]),
      mtConfirmation,mbYesNo,0)=mrNo then
        Exit;
    if backward then
      StartPos:=Editor.LastPt
    else
      StartPos:=Point(1,1);
    goto again;
  end;
end;

procedure TFrmMain.cmdFindReplaceExecute(Sender: TObject);
var
  Editor: TInpEditor;
  opt: TSynSearchOptions;
begin
  if not IsActiveEditor then
    Exit;
  Editor:=ActiveEditor;
  with TFrmTextReplace.Create(Application) do
  try
    Setup(Editor);
    case ShowModal of
      mrOk: opt:=GetOptions+[ssoReplace];
      mrAll: opt:=GetOptions+[ssoReplace,ssoReplaceAll];
    else
      Exit;
    end;
    if Editor.SearchReplace(edSearchText.Text,edReplaceText.Text,opt)=0 then
      MessageDlg(sSearchNotFound,Format(sSearchNotFoundMsg,[edSearchText.Text]),
        mtInformation,[mbOK],0);
  finally
    Release;
  end;
end;

procedure TFrmMain.cmdFindGotoLineExecute(Sender: TObject);
begin
  if IsActiveEditor then
    GoToLineNumber(ActiveEditor);
end;

procedure TFrmMain.cmdFindToggleBookmark0Execute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  with ActiveEditor do
    if TAction(Sender).Checked then
      ClearBookMark(TAction(Sender).Tag)
    else
      SetBookMark(TAction(Sender).Tag,CaretX,CaretY);
end;

procedure TFrmMain.cmdFindGotoBookmark0Execute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  ActiveEditor.GotoBookMark(TAction(Sender).Tag);
end;


procedure TFrmMain.cmdFindClearBookmarksExecute(Sender: TObject);
begin
  if not IsActiveEditor then
    Exit;
  ActiveEditor.ClearBookmarks;
end;

// Templates

procedure TFrmMain.MakeTemplatesMenu;
var
  mi: TMenuItem;
begin
  if not DirectoryExists(fConfig.TemplatesPath) then
    Exit;
  miTemplates.Clear;
  TemplatesSearch(fConfig.TemplatesPath,miTemplates);
  SortMenu(miTemplates);
  if miTemplates.Count>0 then begin
    miTemplates.Enabled:=true;
    mi:=TMenuItem.Create(self);
    mi.Caption:='-';
    miTemplates.Add(mi);
    mi:=TMenuItem.Create(self);
    mi.Action:=cmdOptionsRemoveComents;
    miTemplates.Add(mi);
  end;
end;

procedure TFrmMain.TemplatesSearch(const Path: TFileName; MenuItem: TMenuItem);
var
  si: TSearchRec;
  mi: TMenuItem;
begin
  if FindFirstUTF8(AppendPathDelim(Path)+'*',faAnyFile,si)=0 then begin
    repeat
      if (si.Name='.') or (si.Name='..') or (si.Name='') then continue;
      if (si.Attr and faDirectory)=faDirectory then begin
        mi:=TMenuItem.Create(self);
        mi.Caption:=si.Name;
        MenuItem.Add(mi);
        TemplatesSearch(AppendPathDelim(Path)+si.Name,mi);
      end else begin
        mi:=TMenuItem.Create(self);
        mi.Caption:=si.Name;
        mi.Hint:=AppendPathDelim(Path)+si.Name;
        mi.OnClick:=@TemplateMenuClick;
        MenuItem.Add(mi);
      end;
    until FindNextUTF8(si)<>0;
  FindCloseUTF8(si);
  end;
end;

procedure TFrmMain.TemplateMenuClick(Sender: TObject);
begin
  PasteFromFile(TMenuItem(Sender).Hint);
end;
         
procedure TFrmMain.cmdOptionsRemoveComentsExecute(Sender: TObject);
begin
  Application.ProcessMessages; // dummy
end;


// View commands

procedure TFrmMain.miTabsClick(Sender: TObject);
var
  i: integer;
  mi: TMenuItem;
begin
  miTabs.Clear;
  for i:=0 to fPager.PageCount-1 do begin
    mi:=TMenuItem.Create(miTabs);
    mi.Caption:=TInpEditTabSheet(fPager.Pages[i]).Title;
    mi.Tag:=i;
    mi.RadioItem:=true;
    mi.AutoCheck:=true;
    mi.GroupIndex:=2;
    mi.Checked:=fPager.ActivePageIndex=i;
    mi.OnClick:=@PagerShowTabs;
    miTabs.Add(mi);
  end;
end;

procedure TFrmMain.PmEditorPopup(Sender: TObject);
var
  v: Boolean = false;
begin
  if IsActiveEditor then
    v:=ActiveEditor.CanOpenFileAtCursor;
  msOpenFileAtCursor.Visible:=v;
  miOpenFileAtCursor.Visible:=v;
end;

procedure TFrmMain.PagerShowTabs(Sender: TObject);
begin
  fPager.ActivePageIndex:=TMenuItem(Sender).Tag;
end;

procedure TFrmMain.cmdViewFoldAllExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.FoldAll;
end;

procedure TFrmMain.cmdViewUnfoldAllExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.UnfoldAll;
end;

procedure TFrmMain.cmdViewFoldCurrentExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.FoldCurrent;
end;

procedure TFrmMain.cmdViewUnfoldCurrentExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.UnfoldCurrent;
end;

procedure TFrmMain.cmdViewSpecialCharsExecute(Sender: TObject);
begin
  if IsActiveEditor then
    with ActiveEditor do begin
      if eoShowSpecialChars in Options then
        Options:=Options-[eoShowSpecialChars]
      else
        Options:=Options+[eoShowSpecialChars];
    end;
end;

procedure TFrmMain.cmdViewZoomInExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ActiveEditor.Zoom(TInpEditZoomMode(TAction(Sender).Tag));
end;


// Toolbar

procedure TFrmMain.SetupToolbar;

  procedure FixTollbarSize(tb: TToolBar);
  var
    i,x: Integer;
  begin
    x:=0;
    for i:=0 to tb.ButtonCount-1 do
      Inc(x,tb.Buttons[i].Width);
    Inc(x);
    tb.Width:=x;
  end;

var
  i: Integer;
  tb: TToolBar;
begin
  {$IFDEF FIXTOOLBARS}
  for i:=0 to cbMain.Bands.Count-1 do
    if cbMain.Bands[i].Control is TToolBar then begin
      tb:=TToolBar(cbMain.Bands[i].Control);
      FixTollbarSize(tb);
      cbMain.Bands[i].MinWidth:=tb.Width;
    end;
  cbMain.AutosizeBands;
  {$ENDIF}
  for i:=0 to cbMain.Bands.Count-1 do
    case cbMain.Bands[i].ID of
      0: cmdViewTbFile.Checked:=cbMain.Bands[i].Visible;
      1: cmdViewTbEdit.Checked:=cbMain.Bands[i].Visible;
      2: cmdViewTbSearch.Checked:=cbMain.Bands[i].Visible;
      3: cmdViewTbView.Checked:=cbMain.Bands[i].Visible;
      4: cmdViewTbCalculix.Checked:=cbMain.Bands[i].Visible;
      5: cmdViewTbTools.Checked:=cbMain.Bands[i].Visible;
    end;
end;

procedure TFrmMain.pnToolbarPaint(Sender: TObject);
begin
  DrawMainToolbar(TPanel(Sender));
end;

procedure TFrmMain.pnBtnFileClosePaint(Sender: TObject);
begin
  if ThemeServices.ThemesEnabled then
    DrawThemedControl(TPanel(Sender));
end;

procedure TFrmMain.cmdViewTbFileExecute(Sender: TObject);
begin
  TCoolBand(cbMain.Bands.FindItemID(TAction(Sender).Tag)).Visible:=TAction(Sender).Checked;
end;

procedure TFrmMain.cmdViewTbResetExecute(Sender: TObject);
var
  i,j: Integer;
begin
  cbMain.DisableAutoSizing;
  try
    for i:=0 to cbMain.Bands.Count-1 do begin
      if i<>cbMain.Bands[i].ID then
        for j:=i+1 to cbMain.Bands.Count-1 do
          if cbMain.Bands[j].ID=i then begin
            cbMain.Bands[j].Index:=i;
            break;
          end;
    end;
    for i:=0 to cbMain.Bands.Count-1 do begin
      cbMain.Bands[i].Index:=i;
      cbMain.Bands[i].Visible:=true;
    end;
  finally
    cbMain.EnableAutoSizing;
  end;
  SetupToolbar;
end;

procedure TFrmMain.cmdViewStatusbarExecute(Sender: TObject);
begin
  fConfig.StatusbarVisible:=cmdViewStatusbar.Checked;
  sbEditor.Visible:=cmdViewStatusbar.Checked;
end;


procedure TFrmMain.UpdateCloseBtn;
begin
  fPager.TabCloseBtnVisible:=fConfig.TabCloseBtnVisible;
  pnBtnFileClose.Visible:=not fConfig.TabCloseBtnVisible;
  cbMain.AutosizeBands;
end;

procedure TFrmMain.cmdViewTabCloseBtnVisibleExecute(Sender: TObject);
begin
  fConfig.TabCloseBtnVisible:=cmdViewTabCloseBtnVisible.Checked;
  UpdateCloseBtn;
end;

procedure TFrmMain.cbMainPostPaint(Data: PtrInt);
var
  x: Integer;
  r1,r2: TRect;
begin
  r1:=cbMain.ClientRect;
  r1.Left:=r1.Right-1;
  r2:=pnBtnFileClose.ClientRect;
  r2.Right:=r2.Left+1;
  cbMain.Canvas.CopyRect(r1,pnBtnFileClose.Canvas,r2);
end;

procedure TFrmMain.cbMainPaint(Sender: TObject);
begin
  // hack method for paint toolbar
  // remove white border line in Windows Themes
  if ThemeServices.ThemesEnabled then
    Application.QueueAsyncCall(@cbMainPostPaint,0);
end;

procedure TFrmMain.ThemeServicesThemeChange(Sender: TObject);
begin
  // draw horisontal edges in panel if not enabled ThemeServices
  if ThemeServices.ThemesEnabled then begin
    pnToolbar.BevelInner:=bvNone;
    pnToolbar.BevelOuter:=bvNone;
  end else begin
    pnToolbar.BevelInner:=bvSpace;
    pnToolbar.BevelOuter:=bvSpace;
  end;
end;


// Options

procedure TFrmMain.cmdToolsOptionsExecute(Sender: TObject);
begin
  if ShowOptions then begin
    MakeTemplatesMenu;
    Config.UpdateConfig;
  end;
end;


// Tools

procedure TFrmMain.cmdToolsExportRTFExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ShowExportDlg(ActiveEditor,TExportFormat(TAction(Sender).Tag));
end;

procedure TFrmMain.cmdToolsNodesTransformExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ApplyNodesTransform(ActiveEditor);
end;

procedure TFrmMain.cmdToolsModelViewerExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ShowModelViewer(ActiveEditor);
end;

procedure TFrmMain.cmdToolsMonitorExecute(Sender: TObject);
begin
  if IsActiveEditor then
  {$IFDEF MONITOR_DETACH}
  begin
    apRuner.Executable:=Application.ExeName;
    apRuner.Parameters.Clear;
    apRuner.Parameters.Add('-m');
    apRuner.Parameters.Add(AnsiQuotedStr(ActiveEditor.FileName,'"'));
    apRuner.Execute;
  end;
  {$ELSE}
    ShowMonitor(ActiveEditor.FileName);
  {$ENDIF}
end;

procedure TFrmMain.cmdToolsGroupViewExecute(Sender: TObject);
begin
  if IsActiveEditor then
    ShowGroupViewer(ActiveEditor);
end;

// Add BC

procedure TFrmMain.cmdAddBCFacesExecute(Sender: TObject);
var
  DotnShow: Boolean;
  Editor: TInpEditor;
begin
  if not IsActiveEditor then
    Exit;
  Editor:=ActiveEditor;
  // show warnind on not saved working file
  if Editor.Modified then begin
    if Config.ShowWarning['SaveBeforeRunBC'] then begin
      if WarningOnce(sWarnBeforeRunAddBC,DotnShow)=mrCancel then
        Exit;
      Config.ShowWarning['SaveBeforeRunBC']:=not DotnShow;
    end;
    cmdFileSave.Execute;
  end;

  ShowGroupSelector(Editor,TAddBCCmd(TAction(Sender).Tag));
end;

// Calculix

procedure TFrmMain.cmdCalculixSolverExecute(Sender: TObject);
var
  Editor: TInpEditor;
  DotnShow: Boolean;
begin
  if not IsActiveEditor then
    Exit;
  Editor:=ActiveEditor;
  if Editor.FileName='' then
    case MessageDlg(sReload,Format(sSaveWanted,[Editor.Sheet.Title]),
      mtWarning,mbYesNo,0) of
      mrYes: if GetFileName(Editor) then
                Editor.Save
              else
                Exit;
      mrNo: Exit;
    end;

  if Editor.Modified then begin
    if Config.ShowWarning['SaveBeforeRunCalculix'] then begin
      if WarningOnce(sWarnBeforeRunCalculix,DotnShow)=mrCancel then
        Exit;
      Config.ShowWarning['SaveBeforeRunCalculix']:=not DotnShow;
    end;
    cmdFileSave.Execute;
  end;

  RunCalculix(TCalculixOper(TAction(Sender).Tag),
    StrToCGXPreFlag(Config.CGXPreFlag),Editor.FileName);
end;

procedure TFrmMain.cmdCalculixPreFlagCExecute(Sender: TObject);
var
  x: TCGXPreFlags;
begin
  x:=TCGXPreFlags(TAction(Sender).Tag);
  Config.CGXPreFlag:=CGXPreFlagToStr(x);
end;

procedure TFrmMain.pmPreFlagsPopup(Sender: TObject);
var
  x: TCGXPreFlags;
begin
  x:=StrToCGXPreFlag(Config.CGXPreFlag);
  cmdCalculixPreFlagC.Checked:=x=pfC;
  cmdCalculixPreFlagB.Checked:=x=pfB;
  cmdCalculixPreFlagDuns2d.Checked:=x=pfDuns2d;
  cmdCalculixPreFlagDuns3d.Checked:=x=pfDuns3d;
  cmdCalculixPreFlagIsaac2d.Checked:=x=pfIsaac2d;
  cmdCalculixPreFlagIsaac3d.Checked:=x=pfIsaac3d;
  cmdCalculixPreFlagFoam.Checked:=x=pfFoam;
  cmdCalculixPreFlagNg.Checked:=x=pfNg;
  cmdCalculixPreFlagStep.Checked:=x=pfStep;
  cmdCalculixPreFlagStl.Checked:=x=pfStl;
end;

// Help

procedure TFrmMain.cmdHelpEditorKeystrokesExecute(Sender: TObject);
begin
  ShowEditorKeystrokes;
end;

procedure TFrmMain.cmdHelpAboutExecute(Sender: TObject);
begin
  ShowAbout;
end;


procedure TFrmMain.cmdHelpHomepageExecute(Sender: TObject);
begin
  OpenURL('http://stone-voices.ru/?lang=en');
end;


end.

