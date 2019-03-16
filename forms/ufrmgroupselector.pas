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
    edPressureValue: TFloatSpinEdit;
    lbGroups: TLabel;
    lbPressureValue: TLabel;
    pnBottom: TButtonPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    fAddBCProc: TAddBCProc;
    fCmd: TAddBCCmd;
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
    'Add Faces','Add Pressure To Body');

procedure ShowGroupSelector(aEditor: TInpEditor; const aCmd: TAddBCCmd);
begin
  with TFrmGroupSelector.Create(Application) do
    try
      fAddBCProc.Editor:=aEditor;
      fCmd:=aCmd;
      Caption:=TITLES[fCmd];
      ShowModal;
    finally
      Release;
    end;
end;


{ TFrmGroupSelector }

procedure TFrmGroupSelector.FormCreate(Sender: TObject);
begin
  pnBottom.Color:=clForm; // correct button panel color
  fAddBCProc:=TAddBCProc.Create;
end;

procedure TFrmGroupSelector.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fAddBCProc);
end;

procedure TFrmGroupSelector.FormShow(Sender: TObject);
var
  h: Integer;
begin
  h:=cbGroups.Height;
  case fCmd of
    bcAddFaces: begin
      edPressureValue.Visible:=false;
      lbPressureValue.Visible:=false;
    end;
    bcAddPressure: begin
      edPressureValue.Visible:=true;
      lbPressureValue.Visible:=true;
      Inc(h,edPressureValue.Height+8);
    end;
  end;
  Inc(h,40);
  Height:=pnBottom.Height+h;
  CentredLabels(self);
  Application.QueueAsyncCall(@Work,0);
end;

procedure TFrmGroupSelector.Work(Data: PtrInt);
begin
  Screen.Cursor:=crHourGlass;
  try
    Application.ProcessMessages;
    fAddBCProc.Load(cbGroups.Items,fCmd);
    if cbGroups.Items.Count>0 then begin
      cbGroups.ItemIndex:=0;
      pnBottom.OKButton.Enabled:=true;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;


procedure TFrmGroupSelector.OKButtonClick(Sender: TObject);
begin

  if fCmd=bcAddPressure then begin
    {  // check PressureValue
    if Trim(edPressureValue.Text)='' then begin
      MessageDlg(sWarnEmptyValue,mtWarning,[mbOk],0);
      edPressureValue.SetFocus;
      Exit;
    end;  }
    fAddBCProc.PressureValue:=edPressureValue.Value;
  end;
  Close;
  Screen.Cursor:=crHourGlass;
  try
    Application.ProcessMessages;
    with cbGroups do
      fAddBCProc.Work(Integer(PtrUint(Items.Objects[ItemIndex])));
  finally
    Screen.Cursor:=crDefault;
  end;
end;



end.

