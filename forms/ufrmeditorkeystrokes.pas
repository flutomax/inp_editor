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

unit uFrmEditorKeystrokes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ValEdit, ButtonPanel;

type

  { TFrmEditorKeystrokes }

  TFrmEditorKeystrokes = class(TForm)
    pnBottom: TButtonPanel;
    KeyCmdList: TValueListEditor;
    seKeystrokes: TSynEdit;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FrmEditorKeystrokes: TFrmEditorKeystrokes;

  procedure ShowEditorKeystrokes;

implementation

{$R *.lfm}

uses
  LCLProc, SynEditKeyCmds, SynEditStrConst;

function ConvertCodeString(aString: string): string;
var
  i: Integer;
  WorkStr: string;
begin
  if Pos('ec',LowerCase(aString))=1 then begin
    Delete(aString,1,2);
    WorkStr:='';
    for i:=Length(aString) downto 1 do
      if CharInSet(AString[i],['A'..'Z','0'..'9']) and (i>1) and
         not CharInSet(AString[i-1],['A'..'Z','0'..'9']) then
      begin
        WorkStr:=' '+aString[i]+WorkStr
      end
      else
        WorkStr:=aString[i]+WorkStr;
    Trim(WorkStr);
    i:=Pos('Sel ',WorkStr);
    while i<>0 do begin
      Delete(WorkStr,i,Length('Sel '));
      Insert('Select ',WorkStr,i);
      i:=Pos('Sel ',WorkStr);
    end;
    i:=Pos('Marker ',WorkStr);
    while i <> 0 do begin
      Delete(WorkStr,i,Length('Marker '));
      Insert('Bookmark ',WorkStr,i);
      i:=Pos('Marker ',WorkStr);
    end;
    Result:=Trim(WorkStr);
  end
  else
    Result:=aString;
end;

procedure ShowEditorKeystrokes;
begin
  with TFrmEditorKeystrokes.Create(Application) do
    try
      ShowModal;
    finally
      Release;
    end;
end;

{ TFrmEditorKeystrokes }

procedure TFrmEditorKeystrokes.FormCreate(Sender: TObject);
var
  x: Integer;
  s: string;
begin
  pnBottom.Color:=clForm; // correct button panel color
  for x:=0 to seKeystrokes.Keystrokes.Count-1 do begin
    s:=ConvertCodeString(EditorCommandToCodeString(seKeystrokes.Keystrokes[x].Command));
    KeyCmdList.Cells[0,x+1]:=s;
    if seKeystrokes.Keystrokes[x].ShortCut=0 then
      KeyCmdList.Cells[1,x+1]:=SYNS_ShortCutNone
    else
      KeyCmdList.Cells[1,x+1]:=ShortCutToText(seKeystrokes.Keystrokes[x].ShortCut);
  end;
end;

end.

