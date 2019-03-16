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

unit uFrmGoToLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ButtonPanel, uInpEditor;

type

  { TFrmGoToLine }

  TFrmGoToLine = class(TForm)
    btGoTo: TPanelBitBtn;
    pnBottom: TButtonPanel;
    Label2: TLabel;
    seLine: TSpinEdit;
    procedure btGoToClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fEditor: TInpEditor;
  public
    property Editor: TInpEditor read fEditor write fEditor;
  end;

procedure GoToLineNumber(aEditor: TInpEditor);

var
  FrmGoToLine: TFrmGoToLine;

implementation

{$R *.lfm}

uses
  uEditorMisc;

procedure GoToLineNumber(aEditor: TInpEditor);
begin
  with TFrmGoToLine.Create(Application) do begin
    Editor:=aEditor;
    seLine.MaxValue:=Editor.Lines.Count;
    seLine.Value:=Editor.CaretY;
    ShowModal;
    Release;
  end;
end;

{ TFrmGoToLine }

procedure TFrmGoToLine.FormCreate(Sender: TObject);
begin
  CentredLabels(self);
  pnBottom.Color:=clForm; // correct button panel color
end;

procedure TFrmGoToLine.btGoToClick(Sender: TObject);
begin
  with Editor do begin
    CaretXY:=Point(1,seLine.Value);
    EnsureCursorPosVisible;
  end;
  ModalResult:=mrOK;
end;




end.

