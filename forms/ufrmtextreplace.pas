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

unit uFrmTextReplace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, uDerivedClasses, uFrmTextSearch;

type

  { TFrmTextReplace }

  TFrmTextReplace = class(TFrmTextSearch)
    btReplaceAll: TButton;
    gbReplaceText: TGroupBox;
    edReplaceText: TComboBox;
  private
    fReplaceHistory: TStrings;
  protected
    procedure LoadOptions(ini: TIniFileEx); override;
    procedure SaveOptions(ini: TIniFileEx); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  FrmTextReplace: TFrmTextReplace;
  LastReplaceText: string = '';

implementation

{$R *.lfm}

{ TFrmTextReplace }

constructor TFrmTextReplace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fReplaceHistory:=TStringList.Create;
end;

destructor TFrmTextReplace.Destroy;
begin
  fReplaceHistory.Free;
  inherited Destroy;
end;

procedure TFrmTextReplace.AfterConstruction;
begin
  inherited AfterConstruction;
  edReplaceText.Items.Assign(fReplaceHistory);
end;

procedure TFrmTextReplace.BeforeDestruction;
var
  i: Integer;
begin
  if ModalResult in [mrOK,mrAll] then begin
    LastReplaceText:=edReplaceText.Text;
    if LastReplaceText<>'' then begin
      i:=fReplaceHistory.IndexOf(LastReplaceText);
      if i<0 then
        fReplaceHistory.Insert(0,LastReplaceText)
      else
        fReplaceHistory.Move(i,0);
      while fReplaceHistory.Count>20 do
        fReplaceHistory.Delete(fReplaceHistory.Count-1);
    end;
  end;
  inherited BeforeDestruction;
end;

procedure TFrmTextReplace.LoadOptions(ini: TIniFileEx);
begin
  ini.ReadStringList('Search','ReplaceHistory',fReplaceHistory);
  inherited LoadOptions(ini);
end;

procedure TFrmTextReplace.SaveOptions(ini: TIniFileEx);
begin
  inherited SaveOptions(ini);
  ini.WriteStringList('Search','ReplaceHistory',fReplaceHistory);
end;

end.

