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

unit uFrmViewGroup;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, ComCtrls, ActnList, Menus, uInpTypes, uInpTranslator, uInpEditor;

type

  { TFrmViewGroup }

  TFrmViewGroup = class(TForm)
    lvStat: TListView;
    pnBottom: TButtonPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fInpFile: TInpFile;
    fEditor: TInpEditor;
  public
    procedure Parse(Data: PtrInt);
    procedure RefreshGroupList;
  end;

var
  FrmViewGroup: TFrmViewGroup;

  procedure ShowGroupViewer(aEditor: TInpEditor);

implementation

{$R *.lfm}

uses
  uFrmMain;

procedure ShowGroupViewer(aEditor: TInpEditor);
begin
  with TFrmViewGroup.Create(Application) do
    try
      fEditor:=aEditor;
      RefreshGroupList;
      ShowModal;
    finally
      Release;
    end;
end;



{ TFrmViewGroup }

procedure TFrmViewGroup.FormCreate(Sender: TObject);
begin
  pnBottom.Color:=clForm; // correct button panel color
  fInpFile:=TInpFile.Create;
end;

procedure TFrmViewGroup.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fInpFile);
end;


procedure TFrmViewGroup.RefreshGroupList;
begin
  Application.QueueAsyncCall(@Parse,0);
end;

procedure TFrmViewGroup.Parse(Data: PtrInt);
var
  i: integer;
  t: TSet;
  m: TListItem;
begin
  Screen.Cursor:=crHourGlass;
  try
    Application.ProcessMessages;
    fInpFile.Parse(fEditor.Lines,fEditor.FileName);
    lvStat.Items.BeginUpdate;
    try
      lvStat.Items.Clear;
      m:=lvStat.Items.Add;
      m.Caption:='1';
      m.SubItems.Add('All');
      m.SubItems.Add('%d',[fInpFile.Summen.n]);
      m.SubItems.Add('%d',[fInpFile.Summen.e]);
      m.SubItems.Add('%d',[fInpFile.Summen.f]);
      for i:=0 to fInpFile.Summen.sets-1 do begin
        t:=fInpFile.Sets[i];
        if AnsiSameText(t.Name,'all') then // ignore all set
          continue;
        m:=lvStat.Items.Add;
        m.Caption:=IntToStr(m.Index+1);
        m.SubItems.Add(t.Name);
        m.SubItems.Add('%d',[t.NumNodes]);
        m.SubItems.Add('%d',[t.NumElements]);
        m.SubItems.Add('%d',[t.NumFaces]);
      end;
    finally
      lvStat.Items.EndUpdate;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

end.



