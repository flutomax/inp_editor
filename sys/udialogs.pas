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

unit uDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

function WarningOnce(const aText: string; var DotnShow: Boolean;
    const Btns: TMsgDlgButtons = mbOKCancel): Integer;
function InputQueryEx(const ACaption, APrompt: string;
  var Value: string; const CheckEmpty: Boolean = true): Boolean;

implementation

uses
  Types, LCLType, Controls, FileCtrl, Graphics, Forms, StdCtrls, ExtCtrls,
  Buttons, uConsts;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
  sz: TSize;
begin
  for I := 0 to 25 do Buffer[I] := Char(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Char(I + Ord('a'));
  sz:=Canvas.TextExtent(Buffer);
  Result.X := (sz.cx div 26 + 1) div 2;
  Result.Y := sz.cy;
end;

function WarningOnce(const aText: string; var DotnShow: Boolean;
  const Btns: TMsgDlgButtons): Integer;
var
  dlg: TForm;
  cb: TCheckBox;
  btn: TBitBtn;
  i,y: Integer;
begin
  y:=90;
  dlg:=CreateMessageDialog(aText,mtWarning,Btns);
  try
    for i:=0 to dlg.ComponentCount-1 do
      if (dlg.Components[i] is TBitBtn) then begin
        btn:=TBitBtn(dlg.Components[i]);
        y:=btn.Top+btn.Height+10;
        break;
      end;
    dlg.Height:=y+30;
    cb:=TCheckBox.Create(dlg);
    cb.Parent:=dlg;
    cb.Caption:='Don''t show me again.';
    cb.Top:=y;
    cb.Left:=8;
    result:=dlg.ShowModal;
    DotnShow:=cb.Checked;
  finally
    dlg.Release;
  end;
end;

type

  TInputQueryExForm = class(TForm)
    Prompt: TLabel;
    Edit: TEdit;
    procedure InputCloseQueryEx(Sender: TObject; var CanClose: boolean);
  end;

procedure TInputQueryExForm.InputCloseQueryEx(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose:=false;
  if (ModalResult=mrOk) and (Trim(Edit.Text)='') then begin
    MessageDlg(sWarnEmptyValue,mtWarning,[mbOK],0);
    Edit.SetFocus;
    Exit;
  end;
  CanClose:=true;
end;

function InputQueryEx(const ACaption, APrompt: string;
  var Value: string; const CheckEmpty: Boolean): Boolean;
var
  Form: TInputQueryExForm;
  DialogUnits: TPoint;
  ButtonLeft, ButtonTop1, ButtonTop2,
  EditTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result:=false;
  Form:=TInputQueryExForm.CreateNew(Application);
  with Form do
    try
      BorderStyle:=bsSingle;
      BorderIcons:=[biSystemMenu];
      Canvas.Font:=Font;
      DialogUnits:=GetAveCharSize(Canvas);
      Caption:=ACaption;
      ClientWidth:=MulDiv(240, DialogUnits.X, 4);
      Position:=poOwnerFormCenter;
      if CheckEmpty then
        OnCloseQuery:=@InputCloseQueryEx;
      ButtonWidth:=MulDiv(50, DialogUnits.X, 4);
      ButtonHeight:=MulDiv(14, DialogUnits.Y, 8);
      ButtonTop1:=12;
      ButtonTop2:=ButtonTop1+ButtonHeight+4;
      ButtonLeft:=ClientWidth-ButtonWidth-12;
      EditTop:=ButtonTop2+2*ButtonHeight+4;
      Prompt:=TLabel.Create(Form);
      with Prompt do begin
        Parent:=Form;
        Caption:=APrompt;
        Left:=MulDiv(8,DialogUnits.X,4);
        Top:=MulDiv(8,DialogUnits.Y,8);
        Constraints.MaxWidth:=MulDiv(164,DialogUnits.X,4);
        WordWrap:=true;
      end;
      Edit:=TEdit.Create(Form);
      with Edit do begin
        Parent:=Form;
        Left:=Prompt.Left;
        Top:=EditTop;
        Width:=Form.ClientWidth-Left*2;
        Name:='Edit';
        MaxLength:=255;
        Text:=Value;
        SelectAll;
        Form.ClientHeight:=Top+Height+12;
      end;
      with TButton.Create(Form) do begin
        Parent:=Form;
        Caption:='OK';
        ModalResult:=mrOk;
        Default:=true;
        SetBounds(ButtonLeft,ButtonTop1,ButtonWidth,ButtonHeight);
      end;
      with TButton.Create(Form) do begin
        Parent:=Form;
        Caption:='Cancel';
        ModalResult:=mrCancel;
        Cancel:=true;
        SetBounds(ButtonLeft,ButtonTop2,ButtonWidth,ButtonHeight);
      end;
      if ShowModal=mrOk then begin
        Value:=Edit.Text;
        Result:=true;
      end;
    finally
      Form.Free;
    end;
end;

end.

