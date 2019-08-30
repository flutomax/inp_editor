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

unit uFrmGroupSelector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ExtCtrls, Spin, uInpEditor, uAddBCFunctions;

type

  { TFrmGroupSelector }

  TFrmGroupSelector = class(TForm)
    cbGroups: TComboBox;
    edPressure: TEdit;
    edFilmCoefficient: TEdit;
    edSinkTemperature: TEdit;
    lbGroups: TLabel;
    lbPressure: TLabel;
    lbFilmCoefficient: TLabel;
    lbSinkTemperature: TLabel;
    nbPager: TNotebook;
    pgAddConvection: TPage;
    pgAddFaces: TPage;
    pgAddPressureToBody: TPage;
    pnGroups: TPanel;
    pnBottom: TButtonPanel;
    procedure edPressureKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    fAddBCProc: TAddBCProc;
    fCmd: TAddBCCmd;
    function CheckEdit(Edit: TEdit): boolean;
  public
    procedure Work(Data: PtrInt);
  end;

var
  FrmGroupSelector: TFrmGroupSelector;

procedure ShowGroupSelector(aEditor: TInpEditor; const aCmd: TAddBCCmd);

implementation

{$R *.lfm}

uses
  uEditorMisc, uFrmMain, uConsts;

const
  TITLES: array[TAddBCCmd] of string = (
    'Add Faces', 'Add Pressure To Body', 'Add Convection');

procedure ShowGroupSelector(aEditor: TInpEditor; const aCmd: TAddBCCmd);
begin
  with TFrmGroupSelector.Create(Application) do
    try
      fAddBCProc.Editor := aEditor;
      fCmd := aCmd;
      Caption := TITLES[fCmd];
      ShowModal;
    finally
      Release;
    end;
end;


{ TFrmGroupSelector }

procedure TFrmGroupSelector.FormCreate(Sender: TObject);
begin
  pnBottom.Color := clForm; // correct button panel color
  nbPager.Color := clForm;
  fAddBCProc := TAddBCProc.Create;
end;

procedure TFrmGroupSelector.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fAddBCProc);
end;

procedure TFrmGroupSelector.FormShow(Sender: TObject);
const
  NFIELD: array[TAddBCCmd] of integer = (0, 1, 2);
var
  a, h, n: integer;
begin
  nbPager.PageIndex := Ord(fCmd);
  a := pnGroups.Height;
  n := NFIELD[fCmd];
  h := n * (edPressure.Height + 10) + 4;
  Inc(a, h);
  Inc(a, pnBottom.Height);
  Inc(a, 16);
  ClientHeight := a;
  CentredLabels(self);
  Application.QueueAsyncCall(@Work, 0);
end;

procedure TFrmGroupSelector.Work(Data: PtrInt);
begin
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    fAddBCProc.Load(cbGroups.Items, fCmd);
    if cbGroups.Items.Count > 0 then
    begin
      cbGroups.ItemIndex := 0;
      pnBottom.OKButton.Enabled := True;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TFrmGroupSelector.CheckEdit(Edit: TEdit): boolean;
begin
  Result := True;
  if Trim(Edit.Text) = '' then
  begin
    MessageDlg(sWarnEmptyValue, mtWarning, [mbOK], 0);
    if Edit.CanFocus then
      Edit.SetFocus;
    ModalResult := mrNone;
    Result := False;
  end;
end;

procedure TFrmGroupSelector.OKButtonClick(Sender: TObject);
var
  a1, a2: double;
  sn: integer;
begin
  a1 := 0;
  a2 := 0;
  case fCmd of
    bcAddPressure:
    begin
      // check PressureValue
      if not CheckEdit(edPressure) then
        Exit;
      a1 := ParseValue(edPressure.Text);
    end;
    bcAddConvection:
    begin
      // check Film Coefficient & Sink Temperature
      if not CheckEdit(edFilmCoefficient) then
        Exit;
      if not CheckEdit(edSinkTemperature) then
        Exit;
      a1 := ParseValue(edPressure.Text);
      a2 := ParseValue(edSinkTemperature.Text);
    end;
  end;

  Close;
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    with cbGroups do
      sn := integer(PtrUint(Items.Objects[ItemIndex]));
    case fCmd of
      bcAddFaces: fAddBCProc.Work(sn, []);
      bcAddPressure: fAddBCProc.Work(sn, [a1]);
      bcAddConvection: fAddBCProc.Work(sn, [a1, a2]);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TFrmGroupSelector.edPressureKeyPress(Sender: TObject; var Key: char);
begin
  if not AllowValueChar(Key) then
  begin
    Key := #0;
    Beep;
  end;
end;



end.
