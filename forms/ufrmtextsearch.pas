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

unit uFrmTextSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SynEditTypes, uDerivedClasses, uInpEditor;

type

  { TFrmTextSearch }

  TFrmTextSearch = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    ckCaseSensitive: TCheckBox;
    ckSearchFromCursor: TCheckBox;
    ckSearchSelectedOnly: TCheckBox;
    ckWholeWord: TCheckBox;
    edSearchText: TComboBox;
    gbSearchText: TGroupBox;
    rgDirection: TGroupBox;
    rgDirection1: TRadioButton;
    rgDirection2: TRadioButton;
    gbSearchOptions: TGroupBox;
    procedure ckSearchFromCursorClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    fSearchHistory: TStrings;
    procedure LoadIni;
    procedure SaveIni;
  protected
    procedure LoadOptions(ini: TIniFileEx); virtual;
    procedure SaveOptions(ini: TIniFileEx); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function GetOptions: TSynSearchOptions;
    procedure SetOptions(const Value: TSynSearchOptions);
    procedure Setup(Editor: TInpEditor);
  end;

var
  FrmTextSearch: TFrmTextSearch;
  LastFoundText: string = '';
  LastSearchOptions: TSynSearchOptions = [];

implementation

{$R *.lfm}

uses
  uFrmMain, uConsts;

{ TFrmTextSearch }

constructor TFrmTextSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSearchHistory:=TStringList.Create;
end;

destructor TFrmTextSearch.Destroy;
begin
  fSearchHistory.Free;
  inherited Destroy;
end;

procedure TFrmTextSearch.AfterConstruction;
begin
  inherited AfterConstruction;
  LoadIni;
  edSearchText.Items.Assign(fSearchHistory);
  SetOptions(LastSearchOptions);
end;

procedure TFrmTextSearch.BeforeDestruction;
var
  i: Integer;
begin
  if ModalResult in [mrOK,mrAll] then begin
    LastFoundText:=edSearchText.Text;
    LastSearchOptions:=GetOptions;
    if LastFoundText<>'' then begin
      i:=fSearchHistory.IndexOf(LastFoundText);
      if i<0 then
        fSearchHistory.Insert(0,LastFoundText)
      else
        fSearchHistory.Move(i,0);
      while fSearchHistory.Count>20 do
        fSearchHistory.Delete(fSearchHistory.Count-1);
    end;
  end;
  SaveIni;
  inherited BeforeDestruction;
end;

procedure TFrmTextSearch.LoadIni;
begin
  LoadOptions(FrmMain.Config.Ini);
end;

procedure TFrmTextSearch.SaveIni;
begin
  SaveOptions(FrmMain.Config.Ini);
end;

procedure TFrmTextSearch.LoadOptions(ini: TIniFileEx);
begin
  ini.ReadStringList('Search','SearchHistory',fSearchHistory);
  if ini.ReadBool('Search','WholeWord',false) then
    Include(LastSearchOptions,ssoWholeWord);
  if ini.ReadBool('Search','MatchCase',false) then
    Include(LastSearchOptions,ssoMatchCase);
  if not ini.ReadBool('Search','EntireScope',true) then
    Include(LastSearchOptions,ssoEntireScope);
  if ini.ReadBool('Search','SelectedOnly',false) then
    Include(LastSearchOptions,ssoSelectedOnly);
  if ini.ReadBool('Search','Backwards',false) then
    Include(LastSearchOptions,ssoBackwards);
end;

procedure TFrmTextSearch.SaveOptions(ini: TIniFileEx);
begin
  ini.WriteBool('Search','WholeWord',ssoWholeWord in LastSearchOptions);
  ini.WriteBool('Search','MatchCase',ssoMatchCase in LastSearchOptions);
  ini.WriteBool('Search','EntireScope',not (ssoEntireScope in LastSearchOptions));
  ini.WriteBool('Search','SelectedOnly',ssoSelectedOnly in LastSearchOptions);
  ini.WriteBool('Search','Backwards',ssoBackwards in LastSearchOptions);
  ini.WriteStringList('Search','SearchHistory',fSearchHistory);
end;

procedure TFrmTextSearch.Setup(Editor: TInpEditor);
begin
  ckSearchSelectedOnly.Enabled:=Editor.SelAvail;
  if Editor.SelAvail then
    edSearchText.Text:=Editor.SelText
  else
    edSearchText.Text:=Editor.WordAtCursor;
end;

function TFrmTextSearch.GetOptions: TSynSearchOptions;
begin
  Result:=[];
  if ckWholeWord.Checked then
    Include(Result,ssoWholeWord);
  if ckCaseSensitive.Checked then
    Include(Result,ssoMatchCase);
  if not ckSearchFromCursor.Checked then
    Include(Result,ssoEntireScope);
  if ckSearchSelectedOnly.Checked then
    Include(Result,ssoSelectedOnly);
  if rgDirection2.Checked then
    Include(Result,ssoBackwards);
end;

procedure TFrmTextSearch.SetOptions(const Value: TSynSearchOptions);
begin
  ckWholeWord.Checked:=ssoWholeWord in Value;
  ckCaseSensitive.Checked:=ssoMatchCase in Value;
  ckSearchFromCursor.Checked:=not (ssoEntireScope in Value);
  ckSearchSelectedOnly.Checked:=ssoSelectedOnly in Value;
  ckSearchFromCursorClick(nil);
  if ssoBackwards in Value then
    rgDirection2.Checked:=true;
end;

procedure TFrmTextSearch.ckSearchFromCursorClick(Sender: TObject);
begin
  rgDirection1.Enabled:=ckSearchFromCursor.Checked;
  rgDirection2.Enabled:=ckSearchFromCursor.Checked;
end;

procedure TFrmTextSearch.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=true;
  if (ModalResult<>mrCancel) and (edSearchText.Text='') then begin
    MessageDlg(sSearchEmpty,mtWarning,[mbOK],0);
    CanClose:=false;
    edSearchText.SetFocus;
  end;
end;

end.

