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

unit uUnicalConv;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, Forms, uDerivedClasses;

type

  TImportStartEvent = procedure(Sender: TObject; const aFileName: string) of object;
  TImportSuccessEvent = procedure(Sender: TObject; const aFileName: string;
    aText: TStringStreamEx) of object;
  TImportMessageEvent = procedure(Sender: TObject; const aMsg: string) of object;

  { TImportThread }

  TImportThread = class(TThread)
  protected
    fSuccessfully: Boolean;
    fText: TStringStreamEx;
    fFileName: string;
    fMessage: string;
    fException: Exception;
    fOnMessage: TImportMessageEvent;
    fOnStart: TImportStartEvent;
    procedure DoHandleException;
    procedure DoMessage;
    procedure DoStart;
    procedure HandleException; virtual;
    procedure Message(const aMsg: string);
    procedure Process; virtual;
    procedure Execute; override;
  public
    constructor Create(const aFileName: string); virtual;
    destructor Destroy; override;
    property FileName: string read fFileName;
    property Text: TStringStreamEx read fText;
    property Successfully: Boolean read fSuccessfully;
    property OnStart: TImportStartEvent read fOnStart write fOnStart;
    property OnMessage: TImportMessageEvent read fOnMessage write fOnMessage;
  end;

  { TImporter }

  TImporter = class(TComponent)
  private
    fThread: TImportThread;
    fFileList: TStrings;
    fStarted: Boolean;
    fOnSuccessfully: TImportSuccessEvent;
    fOnMessage: TImportMessageEvent;
    fOnStart: TImportStartEvent;
    fOnFinish: TNotifyEvent;
    function GetBusy: Boolean;
    procedure DoStart(Data: PtrInt);
    procedure ThreadTerminate(Sender: TObject);
    procedure ThreadMessage(Sender: TObject; const aMsg: string);
    procedure ThreadStart(Sender: TObject; const aFileName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddJob(aFileList: TStrings);
    procedure Start;
    procedure Stop;
    property Busy: Boolean read GetBusy;
    property Started: Boolean read fStarted;
    property OnStart: TImportStartEvent read fOnStart write fOnStart;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property OnMessage: TImportMessageEvent read fOnMessage write fOnMessage;
    property OnSuccessfully: TImportSuccessEvent read fOnSuccessfully write fOnSuccessfully;
  end;


implementation

uses
  uInpTypes;

{$I uunicalconv.inc}

type

  EUniReader = class(Exception);

  { TUniReader }

  TUniReader = class(TFileReader)
  public
    procedure Error(const ErrMsg: string = '');
    procedure Skip;
  end;

  TElements = array[TBuffIndex] of TIntegerDynArray;
  TIntDynMatrix = array of TIntegerDynArray;

  TUnvEntity = record
    Member: Integer;               // stores group member (elem/node numbers).
    Type_: Integer;                // type of group member (1=elem, 2=node).
    Pntr: Integer;                 // pointer to connectivity of elements in group.
    ElType: Integer;               // type of element in group.
  end;
  TUnvEntities = array of TUnvEntity;

  TUnvGroup = record
    Ent: Integer;                  // lengths of group.
    Number: Integer;               // stores group numbers.
    Name: string;                  // stores group names.
    Entities: TUnvEntities;        // stores entities in group
  end;
  TUnvGroups = array of TUnvGroup;

  { TUnv }

  TUnv = class(TImportThread)
  private
    fReader: TUniReader;              // virtual text file reader
    fMaxNode: Integer;                // total number of nodes.
    fMaxEle: TIntBuffer;              // total number of elements for each type.
    fSumEle: Integer;                 // total number of all elements.
    fNumNode: Integer;                // current number of node.
    fNumVol: Integer;                 // number of elements with dim=maxdimen.
    fNumEle: Integer;                 // current number of element.
    fGroupNumber: Integer;            // group number.
    fGroupName: string;               // group name.
    fNumEnt: Integer;                 // number of entities in current group.
    fMaxGroup: Integer;               // total number of groups on dataset.
    fNumGroup: Integer;               // current group number.
    fElType: Integer;                 // type of current element (internal).
    fElNode: array[0..49] of Integer; // numbers of nodes at current element.
    fBType: Integer;                  // beam or non-beam element
    fMaxDimen: Integer;               // max dimensions found in model.
    fMinDimen: Integer;               // min dimensions found in model.
    fNodeNumbers: TIntegerDynArray;   // node point numbers; sorted.
    fCoords: TPoint3DDynArray;        // node point coordinates.
    fGroups: TUnvGroups;              // groups info
    fElNumber: TIntBuffer;            // number of elements for each type.
    fElNumbers: TElements;            // element numbers.
    fElem: TElements;                 // element connectivity.
    fSurfNum: TIntegerDynArray;       // surface element number.
    fVolElNum: TIntegerDynArray;      // volume element number.
    fFaceNum: TIntegerDynArray;       // face number of volume element.
    fGroup: TIntegerDynArray;         // group number of surface element.
    fFull: Boolean;
    procedure Allocate;
    procedure CountUni;
    procedure ReadNodes;
    procedure ReadElements;
    procedure ReadGroups;
    procedure WriteMesh;
    procedure WriteSurface;
    procedure ClearElements(var elm: TElements);
    procedure ClearGroups;
  protected
    procedure Process; override;
  public
    constructor Create(const aFileName: string); override;
    destructor Destroy; override;
  end;

function ConvertExponent(const s: string): string;
// convert exponent letter D+00 to E+00
begin
  ConvertExponent:=StringReplace(s,'D','E',[rfReplaceAll,rfIgnoreCase]);
end;

{ TImporter }

constructor TImporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFileList:=TStringList.Create;
  fThread:=nil;
  fStarted:=false;
end;

destructor TImporter.Destroy;
begin
  fThread:=nil;
  FreeAndNil(fFileList);
  inherited Destroy;
end;

procedure TImporter.AddJob(aFileList: TStrings);
begin
  fFileList.Assign(aFileList);
end;

procedure TImporter.Start;
begin
  if fFileList=nil then
    Exit;
  if fFileList.Count>0 then begin
    fThread:=TUnv.Create(fFileList[fFileList.Count-1]);
    fThread.OnTerminate:=@ThreadTerminate;
    fThread.OnMessage:=@ThreadMessage;
    fThread.OnStart:=@ThreadStart;
  end
  else
    if Assigned(fOnFinish) then
      fOnFinish(self);
end;

procedure TImporter.Stop;
var
  i: Integer;
begin
  if Assigned(fThread) then
    fThread.Terminate;
end;

procedure TImporter.DoStart(Data: PtrInt);
begin
  Start;
end;

procedure TImporter.ThreadTerminate(Sender: TObject);
begin
  if fThread.Successfully and Assigned(fOnSuccessfully) then
    fOnSuccessfully(self,fThread.FileName,fThread.Text);
  fFileList.Delete(fFileList.IndexOf(fThread.FileName));
  fThread:=nil;
  Application.QueueAsyncCall(@DoStart,0);
end;

function TImporter.GetBusy: Boolean;
begin
  GetBusy:=Assigned(fThread);
end;

procedure TImporter.ThreadMessage(Sender: TObject; const aMsg: string);
begin
  if Assigned(fOnMessage) then
    fOnMessage(self,aMsg);
end;

procedure TImporter.ThreadStart(Sender: TObject; const aFileName: string);
begin
  fStarted:=true;
  if Assigned(fOnStart) then
    fOnStart(self,aFileName);
end;

{ TImportThread }

constructor TImportThread.Create(const aFileName: string);
begin
  fText:=TStringStreamEx.Create('');
  fSuccessfully:=false;
  fFileName:=aFileName;
  FreeOnTerminate:=true;
  inherited Create(false);
end;

destructor TImportThread.Destroy;
begin
  FreeAndNil(fText);
  inherited Destroy;
end;

procedure TImportThread.HandleException;
begin
  fException:=Exception(ExceptObject);
  try
    // Don't show EAbort messages
    if not (fException is EAbort) then
      Synchronize(@DoHandleException);
  finally
    fException:=nil;
  end;
end;

procedure TImportThread.DoHandleException;
begin
  if fException is Exception then
    Application.ShowException(fException)
  else
    SysUtils.ShowException(fException,nil);
end;

procedure TImportThread.DoMessage;
begin
  if Assigned(fOnMessage) then
    fOnMessage(self,fMessage);
end;

procedure TImportThread.DoStart;
begin
  if Assigned(fOnStart) then
    fOnStart(self,fFileName);
end;

procedure TImportThread.Message(const aMsg: string);
begin
  fMessage:=aMsg;
  Synchronize(@DoMessage);
end;

procedure TImportThread.Process;
begin
  Synchronize(@DoStart);
end;

procedure TImportThread.Execute;
begin
  try
    Process;
  except
    HandleException;
  end;
end;



{ TUniReader }

procedure TUniReader.Error(const ErrMsg: string);
begin
  if ErrMsg='' then
    raise EUniReader.CreateFmt(
      'Unexpected error while reading file "%s" line %d.'+
      LineEnding+'Cannot proceed.',
      [FileName,CurrentLine])
  else
    raise EUniReader.CreateFmt('Error "%s" while reading file "%s" line %d.'+
      LineEnding+'Cannot proceed.',
      [ErrMsg,FileName,CurrentLine])
end;

procedure TUniReader.Skip;
begin
  while not Eof do
    if ReadLine='-1' then
      break;
end;

{ TUnv }

constructor TUnv.Create(const aFileName: string);
begin
  fReader:=TUniReader.Create;
  fFull:=false;
  inherited Create(aFileName);
end;

destructor TUnv.Destroy;
begin
  FreeAndNil(fReader);
  ClearElements(fElNumbers);
  ClearElements(fElem);
  ClearGroups;
  SetLength(fNodeNumbers,0);
  SetLength(fCoords,0);
  SetLength(fSurfNum,0);
  SetLength(fFaceNum,0);
  SetLength(fVolElNum,0);
  SetLength(fGroup,0);
  inherited Destroy;
end;

procedure TUnv.ClearElements(var elm: TElements);
var
  i: integer;
begin
  for i:=Low(elm) to High(elm) do
    SetLength(elm[i],0);
end;

procedure TUnv.ClearGroups;
var
  i: integer;
begin
  for i:=0 to Length(fGroups)-1 do
    SetLength(fGroups[i].Entities,0);
  SetLength(fGroups,0);
end;

procedure TUnv.CountUni;
var
  ret,key: Integer;
  line: string;

  procedure Read2411;
  begin
    Message('countunv: processing dataset 2411, nodes.');
    while not fReader.Eof do begin
      if Terminated then
        Abort;
      line:=fReader.ReadLine;
      ret:=SScanf(line,'%d',[@key]);
      if ret=0 then
        fReader.Error('bad arguments in set 2411');
      if key=-1 then
        break;
      Inc(fMaxNode);
      fReader.ReadLine;
    end;
  end;

  procedure Read2412;
  var
    i,j,ncard,utype: Integer;
  begin
    Message('countunv: processing dataset 2412, elements.');
    while not fReader.Eof do begin
      if Terminated then
        Abort;
      line:=fReader.ReadLine;
      ret:=SScanf(line,'%d %d ',[@key,@utype]);
      if ret=0 then
        fReader.Error('bad arguments in set 2412');
      if key=-1 then
        break;
      if (utype<1) or (utype>232) then
        fReader.Error('wrong type of element');
      for i:=0 to 86 do
        if utype=ELTYPES[i,1] then begin
          fElType:=ELTYPES[i,0];
          fBType:=ELTYPES[i,2];
          break;
        end;
      Inc(fSumEle);
      Inc(fMaxEle[fElType]);
      if fBType=1 then
        fReader.ReadLine;
      ncard:=NUMNODES[fElType] div 8;
      if (NUMNODES[fElType] mod 8)>0 then
        Inc(ncard);
      for j:=0 to ncard-1 do
        fReader.ReadLine;
    end;
  end;

  procedure Read2467;
  var
    i,n,dummy: Integer;
  begin
    Message('countunv: processing dataset 2467, groups.');
    while not fReader.Eof do begin
      if Terminated then
        Abort;
      line:=fReader.ReadLine;
      ret:=SScanf(line,'%d %d %d %d %d %d %d %d %d',
        [@fGroupNumber,@dummy,@dummy,@dummy,@dummy,@dummy,@dummy,@fNumEnt]);
      if ret=0 then
        fReader.Error('bad arguments in set 2467');
      if fGroupNumber=-1 then
        break;
      Inc(fMaxGroup);
      if fNumEnt<=0 then
        fReader.Error('wrong nument <=0');
      fGroupName:=fReader.ReadLine;
      n:=(fNumEnt div 2)+(fNumEnt mod 2);
      for i:=1 to n do
        fReader.ReadLine;
    end;
  end;

var
  i: Integer;
begin
  FillChar(fMaxEle,SizeOf(fMaxEle),0);
  fSumEle:=0;
  fMaxNode:=0;
  fMaxGroup:=0;
  fNumEnt:=0;
  fMinDimen:=10;
  fMaxDimen:=0;
  fNumVol:=0;
  while not fReader.Eof do begin
    if Terminated then
      Abort;
    fReader.Skip;
    line:=fReader.ReadLine;
    ret:=SScanf(line,'%d',[@key]);
    if ret=0 then
      fReader.Error;
    case key of
      -1: continue;
      2411: Read2411;
      2412: Read2412;
      2467: Read2467;
      else fReader.Skip;
    end;
  end;

  if fMaxNode<=0 then
    fReader.Error('no nodes found');
  if fSumEle<=0 then
    fReader.Error('no elements found');
  Message(Format('countunv: %d nodes found on dataset.',[fMaxNode]));

  for i:=1 to 15 do
    if fMaxEle[i]>0 then begin
      if fMinDimen>DIMENSIONS[i] then
        fMinDimen:=DIMENSIONS[i];
      if fMaxDimen<DIMENSIONS[i] then
        fMaxDimen:=DIMENSIONS[i];
      Message(Format('countunv: %d elements of type %d: %s.',
        [fMaxEle[i],i,NOMABQ[i]]));
    end;

  Message(Format('countunv: %d groups found on .unv dataset.',[fMaxGroup]));
  Message(Format('countunv: mindimen=%d, maxdimen=%d',[fMinDimen,fMaxDimen]));

  for i:=0 to 15 do
    if fMaxDimen=DIMENSIONS[i] then
      Inc(fNumVol,fMaxEle[i]);
  Message(Format('countunv: %d elements of max dimension found.',[fNumVol]));
  Message(Format('countunv: maxnode=%d, sumele=%d',[fMaxNode,fSumEle]));
end;

procedure TUnv.Allocate;
var
  i,n: Integer;
begin
  // allocate memory: Mesh.
  if Terminated then
    Abort;
  Message('allocate: allocating large data arrays.');
  SetLength(fNodeNumbers,fMaxNode);
  if Terminated then
    Abort;
  SetLength(fCoords,fMaxNode);
  if Terminated then
    Abort;
  SetLength(fGroups,fMaxGroup);
  if Terminated then
    Abort;
  ClearElements(fElNumbers);
  ClearElements(fElem);

  for i:=1 to 12 do
    if fMaxEle[i]>=0 then begin
      SetLength(fElem[i],NUMNODES[i]*fMaxEle[i]);
      SetLength(fElNumbers[i],fMaxEle[i]);
    end;

  n:=fNumVol*6;
  SetLength(fSurfNum,n);
  if Terminated then
    Abort;
  SetLength(fVolElNum,n);
  if Terminated then
    Abort;
  SetLength(fFaceNum,n);
  if Terminated then
    Abort;
  SetLength(fGroup,n);
  if Terminated then
    Abort;
end;

procedure TUnv.ReadNodes;
var
  key,ret,n,i: Integer;
  x,y,z: Extended;
  line: string;
begin
  fReader.Reset;
  fNumNode:=0;
  while not fReader.Eof do begin
    if Terminated then
      Abort;
    if fReader.ReadLine='-1' then begin
      line:=fReader.ReadLine;
      ret:=SScanf(line,'%d',[@key]);
      if ret=0 then
        fReader.Error;
      if key=2411 then begin
        while not fReader.Eof do begin
          if Terminated then
            Abort;
          line:=fReader.ReadLine;
          ret:=SScanf(line,'%d',[@key]);
          if ret=0 then
            fReader.Error('bad arguments in set 2411');
          if key=-1 then
            break;
          Inc(fNumNode);
          if fNumNode>fMaxNode then
            fReader.Error('too many nodes found');
          fNodeNumbers[fNumNode-1]:=key;
          line:=ConvertExponent(fReader.ReadLine);
          ret:=SScanf(line,'%f %f %f',[@x,@y,@z]);
          if ret=0 then
            fReader.Error('bad arguments in set 2411');
          n:=(fNumNode-1);
          fCoords[n].X:=x;
          fCoords[n].Y:=y;
          fCoords[n].Z:=z;
        end;
      end else
        fReader.Skip;
    end;
  end;
  Message('readnodes: done.');
end;

procedure TUnv.ReadElements;
var
  key,ret,i,j,k,n,
  utype,ncard: Integer;
  line: string;
begin
  fReader.Reset;
  fNumEle:=0;
  FillChar(fElNumber,SizeOf(fElNumber),0);
  while not fReader.Eof do begin
    if Terminated then
      Abort;
    if fReader.ReadLine='-1' then begin
      line:=fReader.ReadLine;
      ret:=SScanf(line,'%d',[@key]);
      if ret=0 then
        fReader.Error;
      if key=2412 then begin
        while not fReader.Eof do begin
          line:=fReader.ReadLine;
          ret:=SScanf(line,'%d %d ',[@key,@utype]);
          if ret=0 then
            fReader.Error('bad arguments in set 2412');
          if key=-1 then
            break;
          if (utype<1) or (utype>232) then
            fReader.Error('wrong type of element');
          for i:=0 to 86 do
            if utype=ELTYPES[i,1] then begin
              fElType:=ELTYPES[i,0];
              fBType:=ELTYPES[i,2];
              break;
            end;
          Inc(fNumEle);
          Inc(fElNumber[fElType]);
          if fElNumber[fElType]>fMaxEle[fElType] then
            fReader.Error('too many elements');
          fElNumbers[fElType,fElNumber[fElType]-1]:=key;
          if fBType=1 then
            fReader.ReadLine;
          ncard:=NUMNODES[fElType] div 8;
          if (NUMNODES[fElType] mod 8)>0 then
            Inc(ncard);
          for j:=0 to ncard-1 do begin
            line:=fReader.ReadLine;
            n:=j*8;
            ret:=SScanf(line,' %d %d %d %d %d %d %d %d ',
            [@fElNode[n],@fElNode[n+1],@fElNode[n+2],@fElNode[n+3],
            @fElNode[n+4],@fElNode[n+5],@fElNode[n+6],@fElNode[n+7]]);
            if ret=0 then
              fReader.Error('bad arguments in set 2412');
          end;
          n:=NUMNODES[fElType];
          for i:=1 to n do begin
            k:=MAPUNV[fElType,i];
            fElem[fElType,n*(fElNumber[fElType]-1)+i-1]:=fElNode[k-1];
          end;
        end;

      end else
        fReader.Skip;
    end;
  end;
  Message('readelements: done.');
end;

procedure TUnv.ReadGroups;
var
  key,ret,dummy,
  i,k,l,n: Integer;
  line: string;
label
  exitloop;
begin
  fReader.Reset;
  fNumGroup:=-1;
  while not fReader.Eof do begin
    if Terminated then
      Abort;
    if fReader.ReadLine='-1' then begin
      line:=fReader.ReadLine;
      ret:=SScanf(line,'%d',[@key]);
      if ret=0 then
        fReader.Error;
      if key=2467 then begin
        while not fReader.Eof do begin
          line:=fReader.ReadLine;
          ret:=SScanf(line,'%d %d %d %d %d %d %d %d',
            [@fGroupNumber,@dummy,@dummy,@dummy,@dummy,@dummy,@dummy,@fNumEnt]);
          if ret=0 then
            fReader.Error('bad arguments in set 2467');
          if fGroupNumber=-1 then
            break;
          Inc(fNumGroup);
          if fNumGroup>fMaxGroup then
            fReader.Error('too many groups');
          if fNumEnt<=0 then
            fReader.Error('wrong nument <=0');
          fGroups[fNumGroup].Number:=fGroupNumber;
          fGroups[fNumGroup].Ent:=fNumEnt;

          // allocate space for group entities.
          SetLength(fGroups[fNumGroup].Entities,fNumEnt);

          // read group data.
          fGroups[fNumGroup].Name:=fReader.ReadLine;
          n:=fNumEnt div 2;
          for i:=0 to n-1 do begin
            line:=fReader.ReadLine;
            FillChar(fElNode,SizeOf(fElNode),0);
            ret:=SScanf(line,'%d %d %d %d %d %d %d %d',
            [@fElNode[0],@fElNode[1],@fElNode[2],@fElNode[3],
            @fElNode[4],@fElNode[5],@fElNode[6],@fElNode[7]]);
            if ret=0 then
              fReader.Error('bad arguments in set 2467');
            if (fElNode[0]<=0) or (fElNode[1]<=0)
            or (fElNode[4]<=0) or (fElNode[5]<=0) then
              fReader.Error('wrong group ID numbers in set 2467');
            fGroups[fNumGroup].Entities[2*i].Type_:=fElNode[0];
            fGroups[fNumGroup].Entities[2*i].Member:=fElNode[1];
            fGroups[fNumGroup].Entities[2*i+1].Type_:=fElNode[4];
            fGroups[fNumGroup].Entities[2*i+1].Member:=fElNode[5];
          end;
          if fNumEnt mod 2=1 then begin
            line:=fReader.ReadLine;
            FillChar(fElNode,SizeOf(fElNode),0);
            ret:=SScanf(line,'%d %d %d %d %d %d %d %d',
            [@fElNode[0],@fElNode[1],@fElNode[2],@fElNode[3],
            @fElNode[4],@fElNode[5],@fElNode[6],@fElNode[7]]);
            if ret=0 then
              fReader.Error('bad arguments in set 2467');
            if (fElNode[0]<=0) or (fElNode[1]<=0) then
              fReader.Error('wrong group ID numbers in set 2467');
            fGroups[fNumGroup].Entities[fNumEnt-1].Type_:=fElNode[0];
            fGroups[fNumGroup].Entities[fNumEnt-1].Member:=fElNode[1];
          end;
          // find pointers to element data of elements of this group
          Message(Format('readgroups: indexing entities of group %s',
            [fGroups[fNumGroup].Name]));
          for i:=0 to fNumEnt-1 do begin
            if Terminated then
              Abort;
            if (i mod 10000=0) and (i>0) then
              Message(Format('readgroups: processing entity %d',[i]));
            fGroups[fNumGroup].Entities[i].ElType:=0;
            fGroups[fNumGroup].Entities[i].Pntr:=0;
            for k:=1 to 12 do
              for l:=0 to fElNumber[k]-1 do begin
                if Terminated then
                  Abort;
                if (fGroups[fNumGroup].Entities[i].Member=fElNumbers[k,l])
                and (fGroups[fNumGroup].Entities[i].Type_=8) then begin
                  fGroups[fNumGroup].Entities[i].ElType:=k;
                  fGroups[fNumGroup].Entities[i].Pntr:=l;
                  goto exitloop;
                end;
              end;
            exitloop:
          end;
        end;
      end else
        fReader.Skip;
    end;
  end;
  Message('readgroups: done.');
end;

procedure TUnv.WriteMesh;
var
  i,j,k,l,n: Integer;
begin
  Message('writemesh: writing node point data.');
  fText.WriteLn('** Imported from SDRL file format. Source file "%s"',
    [ExtractFileName(fFileName)]);
  // writing node point data.
  fText.Write('*node, nset=Nall');
  for i:=0 to fMaxNode-1 do begin
    if Terminated then
      Abort;
    if (i mod 10000=0) and (i>0) then
      Message(Format('writemesh: writing node %d',[i]));
    fText.WriteLn;
    with fCoords[i] do
      fText.Write('%d, %.7e, %.7e, %.7e',[fNodeNumbers[i],X,Y,Z]);
  end;

  // writing groups of nodes.
  Message('writemesh: writing groups of nodes: *NSETs.');
  for i:=0 to fMaxGroup-1 do begin
    if Terminated then
      Abort;
    fNumEnt:=fGroups[i].Ent;
    j:=0;
    for k:=0 to fNumEnt-1 do begin
      if fGroups[i].Entities[k].Type_=7 then begin
        if j=0 then begin
          fText.WriteLn;
          fText.WriteLn('*nset, nset=%s',[fGroups[i].Name]);
        end;
        Inc(j);
        if (j mod 8)=0 then
          fText.WriteLn;
        fText.Write('%d, ',[fGroups[i].Entities[k].Member]);
      end;
    end;
  end;
  fText.WriteLn;

  // writing element connectivity data.
  Message('writemesh: writing element connectivity data.');
  k:=0;
  for i:=1 to 12 do begin
    if Terminated then
      Abort;
    if (fElNumber[i]>0) and ((DIMENSIONS[i]=fMaxDimen) or fFull) then begin
      Message(Format('writemesh: processing elset %s',[NOMABQ[i]]));
      fText.WriteLn('*element, elset=%s, type=%s',[NOMABQ[i],NOMABQ[i]]);
      Message(Format('writemesh: elnumber %d',[fElNumber[i]]));
      for l:=0 to fElNumber[i]-1 do begin
        Inc(k);
        if (k mod 10000=0) then
          Message(Format('writemesh: writing element %d',[k]));
        fText.Write('%d, ',[fElNumbers[i,l]]);
        for n:=0 to NUMNODES[i]-1 do begin
          fText.Write('%d, ',[fElem[i,NUMNODES[i]*l+n]]);
          if n=12 then
            fText.WriteLn;
        end;
        fText.WriteLn;
      end;
    end;
  end;

  // writing groups of elements: *ELSETs.
  Message('writemesh: writing groups of elements: *ELSETs.');
  for i:=0 to fMaxGroup-1 do begin
    if Terminated then
      Abort;
    Message(Format('writemesh: processing group %s',[fGroups[i].Name]));
    fNumEnt:=fGroups[i].Ent;
    j:=0;
    for k:=0 to fNumEnt-1 do begin
      if (DIMENSIONS[fGroups[i].Entities[k].ElType]=fMaxDimen) or fFull then begin
        if j=0 then begin
          fText.WriteLn;
          fText.WriteLn('*elset, elset=%s',[fGroups[i].Name]);
        end;
        Inc(j);
        if (j mod 8)=0 then
          fText.WriteLn;
        fText.Write('%d, ',[fGroups[i].Entities[k].Member]);
      end;
    end;
  end;
  Message('writemesh: done.');
end;

procedure TUnv.WriteSurface;
var
  i,j,k,l,m,n,dim,
  o,p,q,r,s,facepoint,
  facnod,nodcount,
  maxface,surfcounter: Integer;
label
  endloopk;
begin
  Message('surface: generating surfaces.');
  // generating surfaces.
  j:=0;
  surfcounter:=-1;
  // all element types:
  for i:=1 to 15 do begin
    // surfaces are checked for maxdimen elements only.
    if (DIMENSIONS[i]=fMaxDimen) and (fElNumber[i]>0) then begin
      // facepoint points to faces table.
      facepoint:=ELPOINTER[i];
      // determine number of faces at this (volume or plane) element type.
      maxface:=FACES[facepoint,0,0];
      // start loop on all surface element groups.
      for n:=0 to fMaxGroup-1 do begin
        // loop on all elements within this group.
        fNumEnt:=fGroups[n].Ent;
        for k:=0 to fNumEnt-1 do begin
          // elements only (no nodes).
          if Terminated then
            Abort;
          if fGroups[n].Entities[k].Type_=8 then begin
            // check dimension of and pointers (l,m) to element of this group.
            l:=fGroups[n].Entities[k].ElType;
            dim:=DIMENSIONS[l];
            m:=fGroups[n].Entities[k].Pntr;
            // if dimension fits, e.g. .eq. maxdimen-1, search for matching surface.
            if (dim=fMaxDimen-1) and ((l=SMATCH[0,i]) or (l=SMATCH[1,i])) then begin
              Inc(j);
              if (j mod 1000=0) then
                Message(Format('surface: converting surface element %d',[k+1]));
              // loop on volume elements
              for o:=0 to fElNumber[i]-1 do begin
                // start loop on all surfaces of vol element.
                for p:=1 to maxface do begin
                  // number of nodes at this surface.
                  facnod:=FACES[facepoint,p,0];
                  // loop on all edge nodes on surface of volume element.
                  nodcount:=0;
                  for q:=1 to facnod do begin
                    s:=FACES[facepoint,p,q];
                    // loop on all edge nodes of surface element.
                    for r:=1 to facnod do
                      if fElem[i,NUMNODES[i]*o+s-1]=fElem[l,NUMNODES[l]*m+r-1] then
                        Inc(nodcount);
                    if nodcount<q then
                      break;
                  end;
                  if facnod=nodcount then begin
                    Inc(surfcounter);
                    if surfcounter>6*fNumVol then
                      fReader.Error('too many surfaces found');
                    fSurfNum[surfcounter]:=fElNumbers[l,m];
                    fVolElNum[surfcounter]:=fElNumbers[i,o];
                    fFaceNum[surfcounter]:=p;
                    fGroup[surfcounter]:=n;
                    goto endloopk;
                  end;
                end;
              end;
            end;
          end;
          endloopk:
        end;
      end;
    end;
  end;

  if surfcounter<=0 then begin
    // no surfaces found.
    Message('surface: no surfaces found.');
    Exit;
  end;

  Message(Format('surface: surfcounter=%d surfaces found',[surfcounter]));
  for m:=0 to fMaxGroup-1 do begin
    Message(Format('surface: processing group set %d, %s',[m,fGroups[m].Name]));
    for p:=1 to 6 do begin
      n:=0;
      for q:=0 to surfcounter do begin
        if Terminated then
          Abort;
        if (fGroup[q]=m) and (fFaceNum[q]=p) then begin
          Inc(n);
          if (n mod 8)=0 then
            fText.WriteLn;
          if n=1 then begin
            fText.WriteLn;
            fText.WriteLn('*elset, elset=%sF%d',[fGroups[m].Name,p]);
          end;
          fText.Write('%d, ',[fVolElNum[q]]);
        end;
      end;
    end;
  end;
  fText.WriteLn;

  // generate ABAQUS input lines for element surfaces.
  for m:=0 to fMaxGroup-1 do begin
    i:=0;
    for p:=1 to 6 do begin
      if Terminated then
        Abort;
      j:=0;
      for q:=0 to surfcounter do begin
        if (fGroup[q]=m) and (fFaceNum[q]=p) then begin
          Inc(i);
          Inc(j);
          if i=1 then
            fText.WriteLn('*surface, type=element, name=s%s',[fGroups[m].Name]);
          if j=1 then
            fText.WriteLn('%sF%d, S%d',[fGroups[m].Name,p,p]);
        end;
      end;
    end;
  end;

  Message('surface: done.');
end;

procedure TUnv.Process;
begin
  inherited Process;
  try
    Message('Reading file...');
    fReader.Parse(fFileName);
    CountUni;            // determine number of elements, nodes, etc.
    Allocate;            // allocate large arrays.
    ReadNodes;           // read nodes: coordinates.
    ReadElements;        // read element connectivities
    ReadGroups;          // read group information.
    WriteMesh;           // write node point and element data to abaqus/calculix input file.
    WriteSurface;        // process groups for boundary conditions.
    fText.WriteLn;       // add empty last line
    fText.Seek(0,0);
    fSuccessfully:=true; // process ended successfully
  except
    fSuccessfully:=false;
    HandleException;
  end;
end;

initialization
  FormatSettings.DecimalSeparator:='.';

end.

