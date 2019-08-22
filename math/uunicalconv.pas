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
    fSuccessfully: boolean;
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
    procedure Message(const aMsg: string); overload;
    procedure Message(const aMsg: string; const Args: array of const); overload;
    procedure Process; virtual;
    procedure Execute; override;
  public
    constructor Create(const aFileName: string); virtual;
    destructor Destroy; override;
    property FileName: string read fFileName;
    property Text: TStringStreamEx read fText;
    property Successfully: boolean read fSuccessfully;
    property OnStart: TImportStartEvent read fOnStart write fOnStart;
    property OnMessage: TImportMessageEvent read fOnMessage write fOnMessage;
  end;

  { TImporter }

  TImporter = class(TComponent)
  private
    fThread: TImportThread;
    fFileList: TStrings;
    fStarted: boolean;
    fOnSuccessfully: TImportSuccessEvent;
    fOnMessage: TImportMessageEvent;
    fOnStart: TImportStartEvent;
    fOnFinish: TNotifyEvent;
    function GetBusy: boolean;
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
    property Busy: boolean read GetBusy;
    property Started: boolean read fStarted;
    property OnStart: TImportStartEvent read fOnStart write fOnStart;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property OnMessage: TImportMessageEvent read fOnMessage write fOnMessage;
    property OnSuccessfully: TImportSuccessEvent
      read fOnSuccessfully write fOnSuccessfully;
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
    procedure Error(const ErrMsg: string = ''); overload;
    procedure Error(const ErrMsg: string; const Args: array of const); overload;
    procedure Skip;
  end;

  TElements = array[TBuffIndex] of TIntegerDynArray;
  TIntDynMatrix = array of TIntegerDynArray;

  TUnvEntity = record
    Member: integer;               // stores group member (elem/node numbers).
    Type_: integer;                // type of group member (1=elem, 2=node).
    Pntr: integer;                 // pointer to connectivity of elements in group.
    ElType: integer;               // type of element in group.
  end;
  TUnvEntities = array of TUnvEntity;

  TUnvGroup = record
    Id: integer;                   // stores group dataset ID.
    Ent: integer;                  // lengths of group.
    Number: integer;               // stores group numbers.
    Name: string;                  // stores group names.
    Entities: TUnvEntities;        // stores entities in group
  end;
  TUnvGroups = array of TUnvGroup;

  { TUnv }

  TUnv = class(TImportThread)
  private
    fReader: TUniReader;              // virtual text file reader
    fMaxNode: integer;                // total number of nodes.
    fMaxEle: TIntBuffer;              // total number of elements for each type.
    fSumEle: integer;                 // total number of all elements.
    fNumNode: integer;                // current number of node.
    fNumVol: integer;                 // number of elements with dim=maxdimen.
    fNumEle: integer;                 // current number of element.
    fGroupNumber: integer;            // group number.
    fGroupName: string;               // group name.
    fNumEnt: integer;                 // number of entities in current group.
    fMaxNumEnt: integer;              // max number of entities in current group (for 2477).
    fMaxNumEntGroup: integer;         // group number with max number of entities (for 2477).
    fMaxGroup: integer;               // total number of groups on dataset.
    fNumGroup: integer;               // current group number.
    fElType: integer;                 // type of current element (internal).
    fElNode: array[0..49] of integer; // numbers of nodes at current element.
    fBType: integer;                  // beam or non-beam element
    fMaxDimen: integer;               // max dimensions found in model.
    fMinDimen: integer;               // min dimensions found in model.
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
    fFull: boolean;
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
  ConvertExponent := StringReplace(s, 'D', 'E', [rfReplaceAll, rfIgnoreCase]);
end;

{ TImporter }

constructor TImporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFileList := TStringList.Create;
  fThread := nil;
  fStarted := False;
end;

destructor TImporter.Destroy;
begin
  fThread := nil;
  FreeAndNil(fFileList);
  inherited Destroy;
end;

procedure TImporter.AddJob(aFileList: TStrings);
begin
  fFileList.Assign(aFileList);
end;

procedure TImporter.Start;
begin
  if fFileList = nil then
    Exit;
  if fFileList.Count > 0 then
  begin
    fThread := TUnv.Create(fFileList[fFileList.Count - 1]);
    fThread.OnTerminate := @ThreadTerminate;
    fThread.OnMessage := @ThreadMessage;
    fThread.OnStart := @ThreadStart;
  end
  else
  if Assigned(fOnFinish) then
    fOnFinish(self);
end;

procedure TImporter.Stop;
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
    fOnSuccessfully(self, fThread.FileName, fThread.Text);
  fFileList.Delete(fFileList.IndexOf(fThread.FileName));
  fThread := nil;
  Application.QueueAsyncCall(@DoStart, 0);
end;

function TImporter.GetBusy: boolean;
begin
  GetBusy := Assigned(fThread);
end;

procedure TImporter.ThreadMessage(Sender: TObject; const aMsg: string);
begin
  if Assigned(fOnMessage) then
    fOnMessage(self, aMsg);
end;

procedure TImporter.ThreadStart(Sender: TObject; const aFileName: string);
begin
  fStarted := True;
  if Assigned(fOnStart) then
    fOnStart(self, aFileName);
end;

{ TImportThread }

constructor TImportThread.Create(const aFileName: string);
begin
  fText := TStringStreamEx.Create('');
  fSuccessfully := False;
  fFileName := aFileName;
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TImportThread.Destroy;
begin
  FreeAndNil(fText);
  inherited Destroy;
end;

procedure TImportThread.HandleException;
begin
  fException := Exception(ExceptObject);
  try
    // Don't show EAbort messages
    if not (fException is EAbort) then
      Synchronize(@DoHandleException);
  finally
    fException := nil;
  end;
end;

procedure TImportThread.DoHandleException;
begin
  if fException is Exception then
    Application.ShowException(fException)
  else
    SysUtils.ShowException(fException, nil);
end;

procedure TImportThread.DoMessage;
begin
  if Assigned(fOnMessage) then
    fOnMessage(self, fMessage);
end;

procedure TImportThread.DoStart;
begin
  if Assigned(fOnStart) then
    fOnStart(self, fFileName);
end;

procedure TImportThread.Message(const aMsg: string);
begin
  fMessage := aMsg;
  Synchronize(@DoMessage);
end;

procedure TImportThread.Message(const aMsg: string; const Args: array of const);
begin
  Message(Format(aMsg, Args));
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
  if ErrMsg = '' then
    raise EUniReader.CreateFmt(
      'Unexpected error while reading file "%s" at line %d.' +
      LineEnding + 'Cannot proceed.', [FileName, CurrentLine])
  else
    raise EUniReader.CreateFmt('Error "%s" while reading file "%s" at line %d.' +
      LineEnding + 'Cannot proceed.', [ErrMsg, FileName, CurrentLine]);
end;

procedure TUniReader.Error(const ErrMsg: string; const Args: array of const);
begin
  Error(Format(ErrMsg, Args));
end;

procedure TUniReader.Skip;
begin
  while not EOF do
    if ReadLine = '-1' then
      break;
end;

{ TUnv }

constructor TUnv.Create(const aFileName: string);
begin
  fReader := TUniReader.Create;
  fFull := False;
  inherited Create(aFileName);
end;

destructor TUnv.Destroy;
begin
  FreeAndNil(fReader);
  ClearElements(fElNumbers);
  ClearElements(fElem);
  ClearGroups;
  SetLength(fNodeNumbers, 0);
  SetLength(fCoords, 0);
  SetLength(fSurfNum, 0);
  SetLength(fFaceNum, 0);
  SetLength(fVolElNum, 0);
  SetLength(fGroup, 0);
  inherited Destroy;
end;

procedure TUnv.ClearElements(var elm: TElements);
var
  i: integer;
begin
  for i := Low(elm) to High(elm) do
    SetLength(elm[i], 0);
end;

procedure TUnv.ClearGroups;
var
  i: integer;
begin
  for i := 0 to Length(fGroups) - 1 do
    SetLength(fGroups[i].Entities, 0);
  SetLength(fGroups, 0);
end;

procedure TUnv.CountUni;
var
  ret, key: integer;
  line: string;

  procedure Read2411;
  begin
    Message('countunv: processing dataset 2411, nodes.');
    while not fReader.EOF do
    begin
      if Terminated then
        Abort;
      line := fReader.ReadLine;
      ret := SScanf(line, '%d', [@key]);
      if ret = 0 then
        fReader.Error('bad arguments in set 2411');
      if key = -1 then
        break;
      Inc(fMaxNode);
      fReader.ReadLine;
    end;
  end;

  procedure Read2412;
  var
    i, j, ncard, utype: integer;
  begin
    Message('countunv: processing dataset 2412, elements.');
    while not fReader.EOF do
    begin
      if Terminated then
        Abort;
      line := fReader.ReadLine;
      ret := SScanf(line, '%d %d ', [@key, @utype]);
      if ret = 0 then
        fReader.Error('bad arguments in set 2412');
      if key = -1 then
        break;
      if (utype < 1) or (utype > 232) then
        fReader.Error('wrong type of element');
      for i := 0 to 86 do
        if utype = ELTYPES[i, 1] then
        begin
          fElType := ELTYPES[i, 0];
          fBType := ELTYPES[i, 2];
          break;
        end;
      Inc(fSumEle);
      Inc(fMaxEle[fElType]);
      if fBType = 1 then
        fReader.ReadLine;
      ncard := NUMNODES[fElType] div 8;
      if (NUMNODES[fElType] mod 8) > 0 then
        Inc(ncard);
      for j := 0 to ncard - 1 do
        fReader.ReadLine;
    end;
  end;

  procedure ReadGroup(const dataset: integer);
  var
    i, n, dummy: integer;
  begin
    Message('countunv: processing dataset %d, groups.', [dataset]);
    while not fReader.EOF do
    begin
      if Terminated then
        Abort;
      line := fReader.ReadLine;
      ret := SScanf(line, '%d %d %d %d %d %d %d %d %d',
        [@fGroupNumber, @dummy, @dummy, @dummy, @dummy, @dummy, @dummy, @fNumEnt]);
      if ret = 0 then
        fReader.Error('bad arguments in set %d', [dataset]);
      if fGroupNumber = -1 then
        break;
      Inc(fMaxGroup);
      if fNumEnt <= 0 then
        fReader.Error('wrong nument <=0');
      fGroupName := fReader.ReadLine;
      if (fNumEnt > fMaxNumEnt) and (dataset = 2477) then
      begin
        // find group number with max number of entities for 2477
        fMaxNumEnt := fNumEnt;
        fMaxNumEntGroup := fGroupNumber;
      end;
      // jump to next group definition, but check nested entities before
      n := 0;
      while n < fNumEnt do
      begin
        line := fReader.ReadLine;
        ret := SScanf(line, '%d %d %d %d %d %d %d %d %d',
          [@dummy, @dummy, @dummy, @dummy, @dummy, @dummy, @dummy, @dummy]);
        case ret of
          4: Inc(n);
          8: Inc(n, 2);
          else
            fReader.Error('bad entities definition in set %d', [dataset]);
        end;
      end;
    end;
  end;

var
  i: integer;
begin
  FillChar(fMaxEle, SizeOf(fMaxEle), 0);
  fSumEle := 0;
  fMaxNode := 0;
  fMaxGroup := 0;
  fNumEnt := 0;
  fMaxNumEnt := 0;
  fMaxNumEntGroup := 0;
  fMinDimen := 10;
  fMaxDimen := 0;
  fNumVol := 0;
  while not fReader.EOF do
  begin
    if Terminated then
      Abort;
    fReader.Skip;
    line := fReader.ReadLine;
    ret := SScanf(line, '%d', [@key]);
    if ret = 0 then
      fReader.Error;
    case key of
      -1: continue;
      2411: Read2411;
      2412: Read2412;
      2467, 2477: ReadGroup(key);
      else
        fReader.Skip;
    end;
  end;

  if fMaxNode <= 0 then
    fReader.Error('no nodes found');
  if fSumEle <= 0 then
    fReader.Error('no elements found');
  Message('countunv: %d nodes found on dataset.', [fMaxNode]);

  for i := 1 to 15 do
    if fMaxEle[i] > 0 then
    begin
      if fMinDimen > DIMENSIONS[i] then
        fMinDimen := DIMENSIONS[i];
      if fMaxDimen < DIMENSIONS[i] then
        fMaxDimen := DIMENSIONS[i];
      Message('countunv: %d elements of type %d: %s.', [fMaxEle[i], i, NOMABQ[i]]);
    end;

  Message('countunv: %d groups found on .unv dataset.', [fMaxGroup]);
  Message('countunv: mindimen=%d, maxdimen=%d', [fMinDimen, fMaxDimen]);

  for i := 0 to 15 do
    if fMaxDimen = DIMENSIONS[i] then
      Inc(fNumVol, fMaxEle[i]);
  Message('countunv: %d elements of max dimension found.', [fNumVol]);
  Message('countunv: maxnode=%d, sumele=%d', [fMaxNode, fSumEle]);
end;

procedure TUnv.Allocate;
var
  i, n: integer;
begin
  // allocate memory: Mesh.
  if Terminated then
    Abort;
  Message('allocate: allocating large data arrays.');
  SetLength(fNodeNumbers, fMaxNode);
  if Terminated then
    Abort;
  SetLength(fCoords, fMaxNode);
  if Terminated then
    Abort;
  SetLength(fGroups, fMaxGroup);
  if Terminated then
    Abort;
  ClearElements(fElNumbers);
  ClearElements(fElem);

  for i := 1 to 12 do
    if fMaxEle[i] >= 0 then
    begin
      SetLength(fElem[i], NUMNODES[i] * fMaxEle[i]);
      SetLength(fElNumbers[i], fMaxEle[i]);
    end;

  n := fNumVol * 6;
  SetLength(fSurfNum, n);
  if Terminated then
    Abort;
  SetLength(fVolElNum, n);
  if Terminated then
    Abort;
  SetLength(fFaceNum, n);
  if Terminated then
    Abort;
  SetLength(fGroup, n);
  if Terminated then
    Abort;
end;

procedure TUnv.ReadNodes;
var
  key, ret, n, i: integer;
  x, y, z: extended;
  line: string;
begin
  fReader.Reset;
  fNumNode := 0;
  while not fReader.EOF do
  begin
    if Terminated then
      Abort;
    if fReader.ReadLine = '-1' then
    begin
      line := fReader.ReadLine;
      ret := SScanf(line, '%d', [@key]);
      if ret = 0 then
        fReader.Error;
      if key = 2411 then
      begin
        while not fReader.EOF do
        begin
          if Terminated then
            Abort;
          line := fReader.ReadLine;
          ret := SScanf(line, '%d', [@key]);
          if ret = 0 then
            fReader.Error('bad arguments in set 2411');
          if key = -1 then
            break;
          Inc(fNumNode);
          if fNumNode > fMaxNode then
            fReader.Error('too many nodes found');
          fNodeNumbers[fNumNode - 1] := key;
          line := ConvertExponent(fReader.ReadLine);
          ret := SScanf(line, '%f %f %f', [@x, @y, @z]);
          if ret = 0 then
            fReader.Error('bad arguments in set 2411');
          n := (fNumNode - 1);
          fCoords[n].X := x;
          fCoords[n].Y := y;
          fCoords[n].Z := z;
        end;
      end
      else
        fReader.Skip;
    end;
  end;
  Message('readnodes: done.');
end;

procedure TUnv.ReadElements;
var
  key, ret, i, j, k, n, utype, ncard: integer;
  line: string;
begin
  fReader.Reset;
  fNumEle := 0;
  FillChar(fElNumber, SizeOf(fElNumber), 0);
  while not fReader.EOF do
  begin
    if Terminated then
      Abort;
    if fReader.ReadLine = '-1' then
    begin
      line := fReader.ReadLine;
      ret := SScanf(line, '%d', [@key]);
      if ret = 0 then
        fReader.Error;
      if key = 2412 then
      begin
        while not fReader.EOF do
        begin
          line := fReader.ReadLine;
          ret := SScanf(line, '%d %d ', [@key, @utype]);
          if ret = 0 then
            fReader.Error('bad arguments in set 2412');
          if key = -1 then
            break;
          if (utype < 1) or (utype > 232) then
            fReader.Error('wrong type of element');
          for i := 0 to 86 do
            if utype = ELTYPES[i, 1] then
            begin
              fElType := ELTYPES[i, 0];
              fBType := ELTYPES[i, 2];
              break;
            end;
          Inc(fNumEle);
          Inc(fElNumber[fElType]);
          if fElNumber[fElType] > fMaxEle[fElType] then
            fReader.Error('too many elements');
          fElNumbers[fElType, fElNumber[fElType] - 1] := key;
          if fBType = 1 then
            fReader.ReadLine;
          ncard := NUMNODES[fElType] div 8;
          if (NUMNODES[fElType] mod 8) > 0 then
            Inc(ncard);
          for j := 0 to ncard - 1 do
          begin
            line := fReader.ReadLine;
            n := j * 8;
            ret := SScanf(line, ' %d %d %d %d %d %d %d %d ',
              [@fElNode[n], @fElNode[n + 1], @fElNode[n + 2], @fElNode[n + 3],
              @fElNode[n + 4], @fElNode[n + 5], @fElNode[n + 6], @fElNode[n + 7]]);
            if ret = 0 then
              fReader.Error('bad arguments in set 2412');
          end;
          n := NUMNODES[fElType];
          for i := 1 to n do
          begin
            k := MAPUNV[fElType, i];
            fElem[fElType, n * (fElNumber[fElType] - 1) + i - 1] := fElNode[k - 1];
          end;
        end;

      end
      else
        fReader.Skip;
    end;
  end;
  Message('readelements: done.');
end;

procedure TUnv.ReadGroups;
var
  key, ret, dummy, i, k, l, n: integer;
  line: string;
label
  exitloop;
begin
  fReader.Reset;
  fNumGroup := -1;
  while not fReader.EOF do
  begin
    if Terminated then
      Abort;
    if fReader.ReadLine = '-1' then
    begin
      line := fReader.ReadLine;
      ret := SScanf(line, '%d', [@key]);
      if ret = 0 then
        fReader.Error;
      if (key = 2467) or (key = 2477) then
      begin
        while not fReader.EOF do
        begin
          line := fReader.ReadLine;
          ret := SScanf(line, '%d %d %d %d %d %d %d %d',
            [@fGroupNumber, @dummy, @dummy, @dummy, @dummy, @dummy, @dummy, @fNumEnt]);
          if ret = 0 then
            fReader.Error('bad arguments in set %d', [key]);
          if fGroupNumber = -1 then
            break;
          Inc(fNumGroup);
          if fNumGroup > fMaxGroup then
            fReader.Error('too many groups');
          if fNumEnt <= 0 then
            fReader.Error('wrong nument <=0');
          fGroups[fNumGroup].Id := key;
          fGroups[fNumGroup].Number := fGroupNumber;
          fGroups[fNumGroup].Ent := fNumEnt;

          // allocate space for group entities.
          SetLength(fGroups[fNumGroup].Entities, fNumEnt);

          // read group data.
          fGroups[fNumGroup].Name := fReader.ReadLine;
          n := 0;
          while n < fNumEnt do
          begin
            line := fReader.ReadLine;
            FillChar(fElNode, SizeOf(fElNode), 0);
            ret := SScanf(line, '%d %d %d %d %d %d %d %d',
              [@fElNode[0], @fElNode[1], @fElNode[2], @fElNode[3],
              @fElNode[4], @fElNode[5], @fElNode[6], @fElNode[7]]);
            case ret of
              4: i:=1;
              8: i:=2;
              else
                fReader.Error('bad arguments in set %d', [key]);
            end;
            l:=0;
            for k:=0 to i-1 do begin
              if (fElNode[l] <= 0) or (fElNode[l+1] <= 0)
              then
                fReader.Error('wrong group ID numbers in set %d', [key]);
              fGroups[fNumGroup].Entities[n+k].Type_ := fElNode[l];
              fGroups[fNumGroup].Entities[n+k].Member := fElNode[l+1];
              inc(l,4);
            end;
            inc(n,i);
          end;

          // find pointers to element data of elements of this group
          Message('readgroups: indexing entities of group %s', [fGroups[fNumGroup].Name]);
          for i := 0 to fNumEnt - 1 do
          begin
            if Terminated then
              Abort;
            if (i mod 10000 = 0) and (i > 0) then
              Message('readgroups: processing entity %d', [i]);
            fGroups[fNumGroup].Entities[i].ElType := 0;
            fGroups[fNumGroup].Entities[i].Pntr := 0;
            for k := 1 to 12 do
              for l := 0 to fElNumber[k] - 1 do
              begin
                if Terminated then
                  Abort;
                if (fGroups[fNumGroup].Entities[i].Member = fElNumbers[k, l])
                  and (fGroups[fNumGroup].Entities[i].Type_ = 8) then
                begin
                  fGroups[fNumGroup].Entities[i].ElType := k;
                  fGroups[fNumGroup].Entities[i].Pntr := l;
                  goto exitloop;
                end;
              end;
            exitloop: ;
          end;
        end;
      end
      else
        fReader.Skip;
    end;
  end;
  Message('readgroups: done.');
end;


procedure TUnv.WriteMesh;
var
  i, j, k, l, n: integer;
begin
  Message('writemesh: writing node point data.');
  fText.WriteLn('** Imported from SDRL file format. Source file "%s"',
    [ExtractFileName(fFileName)]);
  // writing node point data.
  fText.Write('*node, nset=Nall');
  for i := 0 to fMaxNode - 1 do
  begin
    if Terminated then
      Abort;
    if (i mod 10000 = 0) and (i > 0) then
      Message('writemesh: writing node %d', [i]);
    fText.WriteLn;
    with fCoords[i] do
      fText.Write('%d, %.7e, %.7e, %.7e', [fNodeNumbers[i], X, Y, Z]);
  end;

  // writing groups of nodes.
  Message('writemesh: writing groups of nodes: *NSETs.');
  for i := 0 to fMaxGroup - 1 do
  begin
    if Terminated then
      Abort;
    fNumEnt := fGroups[i].Ent;
    j := 0;
    for k := 0 to fNumEnt - 1 do
    begin
      if fGroups[i].Entities[k].Type_ = 7 then
      begin
        if j = 0 then
        begin
          fText.WriteLn;
          fText.WriteLn('*nset, nset=%s', [fGroups[i].Name]);
        end;
        Inc(j);
        if (j mod 8) = 0 then
          fText.WriteLn;
        fText.Write('%d, ', [fGroups[i].Entities[k].Member]);
      end;
    end;
  end;
  fText.WriteLn;

  // writing element connectivity data.
  Message('writemesh: writing element connectivity data.');
  k := 0;
  for i := 1 to 12 do
  begin
    if Terminated then
      Abort;
    if (fElNumber[i] > 0) and ((DIMENSIONS[i] = fMaxDimen) or fFull) then
    begin
      Message('writemesh: processing elset %s', [NOMABQ[i]]);
      fText.WriteLn('*element, elset=%s, type=%s', [NOMABQ[i], NOMABQ[i]]);
      Message('writemesh: elnumber %d', [fElNumber[i]]);
      for l := 0 to fElNumber[i] - 1 do
      begin
        Inc(k);
        if (k mod 10000 = 0) then
          Message('writemesh: writing element %d', [k]);
        fText.Write('%d, ', [fElNumbers[i, l]]);
        for n := 0 to NUMNODES[i] - 1 do
        begin
          fText.Write('%d, ', [fElem[i, NUMNODES[i] * l + n]]);
          if n = 12 then
            fText.WriteLn;
        end;
        fText.WriteLn;
      end;
    end;
  end;

  // writing groups of elements: *ELSETs.

  Message('writemesh: writing groups of elements: *ELSETs.');
  for i := 0 to fMaxGroup - 1 do
  begin
    if Terminated then
      Abort;
    Message('writemesh: processing group %s', [fGroups[i].Name]);
    fNumEnt := fGroups[i].Ent;
    j := 0;
    for k := 0 to fNumEnt - 1 do
    begin
      case fGroups[i].Id of
        2467: // used rule for 2467 dataset
        begin
          if (DIMENSIONS[fGroups[i].Entities[k].ElType] = fMaxDimen) or fFull then
          begin
            if j = 0 then
            begin
              fText.WriteLn;
              fText.WriteLn('*elset, elset=%s', [fGroups[i].Name]);
            end;
            Inc(j);
            if (j mod 8) = 0 then
              fText.WriteLn;
            fText.Write('%d, ', [fGroups[i].Entities[k].Member]);
          end;
        end;
        2477: // used rule for 2477 dataset
        begin
          if (fGroups[i].Entities[k].Type_ = 8) and
            ((fGroups[i].Number = fMaxNumEntGroup) or
            (Pos('elem', LowerCase(fGroups[i].Name)) <> 0)) then
          begin
            if j = 0 then
            begin
              fText.WriteLn;
              fText.WriteLn('*elset, elset=%s', [fGroups[i].Name]);
            end;
            Inc(j);
            if (j mod 8) = 0 then
              fText.WriteLn;
            fText.Write('%d, ', [fGroups[i].Entities[k].Member]);
          end;
        end;
      end;
    end;

  end;
  Message('writemesh: done.');
end;

procedure TUnv.WriteSurface;
var
  i, j, k, l, m, n, dim, o, p, q, r, s,
  facepoint, facnod, nodcount, maxface,
  surfcounter: integer;
label
  endloopk;
begin
  Message('surface: generating surfaces.');
  // generating surfaces.
  j := 0;
  surfcounter := -1;
  // all element types:
  for i := 1 to 15 do
  begin
    // surfaces are checked for maxdimen elements only.
    if (DIMENSIONS[i] = fMaxDimen) and (fElNumber[i] > 0) then
    begin
      // facepoint points to faces table.
      facepoint := ELPOINTER[i];
      // determine number of faces at this (volume or plane) element type.
      maxface := FACES[facepoint, 0, 0];
      // start loop on all surface element groups.
      for n := 0 to fMaxGroup - 1 do
      begin
        // loop on all elements within this group.
        fNumEnt := fGroups[n].Ent;
        for k := 0 to fNumEnt - 1 do
        begin
          // elements only (no nodes).
          if Terminated then
            Abort;
          if fGroups[n].Entities[k].Type_ = 8 then
          begin
            // check dimension of and pointers (l,m) to element of this group.
            l := fGroups[n].Entities[k].ElType;
            dim := DIMENSIONS[l];
            m := fGroups[n].Entities[k].Pntr;
            // if dimension fits, e.g. .eq. maxdimen-1, search for matching surface.
            if (dim = fMaxDimen - 1) and ((l = SMATCH[0, i]) or (l = SMATCH[1, i])) then
            begin
              Inc(j);
              if (j mod 1000 = 0) then
                Message('surface: converting surface element %d', [k + 1]);
              // loop on volume elements
              for o := 0 to fElNumber[i] - 1 do
              begin
                // start loop on all surfaces of vol element.
                for p := 1 to maxface do
                begin
                  // number of nodes at this surface.
                  facnod := FACES[facepoint, p, 0];
                  // loop on all edge nodes on surface of volume element.
                  nodcount := 0;
                  for q := 1 to facnod do
                  begin
                    s := FACES[facepoint, p, q];
                    // loop on all edge nodes of surface element.
                    for r := 1 to facnod do
                      if fElem[i, NUMNODES[i] * o + s - 1] = fElem[l, NUMNODES[l] * m + r - 1] then
                        Inc(nodcount);
                    if nodcount < q then
                      break;
                  end;
                  if facnod = nodcount then
                  begin
                    Inc(surfcounter);
                    if surfcounter > 6 * fNumVol then
                      fReader.Error('too many surfaces found');
                    fSurfNum[surfcounter] := fElNumbers[l, m];
                    fVolElNum[surfcounter] := fElNumbers[i, o];
                    fFaceNum[surfcounter] := p;
                    fGroup[surfcounter] := n;
                    goto endloopk;
                  end;
                end;
              end;
            end;
          end;
          endloopk: ;
        end;
      end;
    end;
  end;

  if surfcounter <= 0 then
  begin
    // no surfaces found.
    Message('surface: no surfaces found.');
    Exit;
  end;

  Message('surface: surfcounter=%d surfaces found', [surfcounter]);
  for m := 0 to fMaxGroup - 1 do
  begin
    Message('surface: processing group set %d, %s', [m, fGroups[m].Name]);
    for p := 1 to 6 do
    begin
      n := 0;
      for q := 0 to surfcounter do
      begin
        if Terminated then
          Abort;
        if (fGroup[q] = m) and (fFaceNum[q] = p) then
        begin
          Inc(n);
          if (n mod 8) = 0 then
            fText.WriteLn;
          if n = 1 then
          begin
            fText.WriteLn;
            fText.WriteLn('*elset, elset=%sF%d', [fGroups[m].Name, p]);
          end;
          fText.Write('%d, ', [fVolElNum[q]]);
        end;
      end;
    end;
  end;
  fText.WriteLn;

  // generate ABAQUS input lines for element surfaces.
  for m := 0 to fMaxGroup - 1 do
  begin
    i := 0;
    for p := 1 to 6 do
    begin
      if Terminated then
        Abort;
      j := 0;
      for q := 0 to surfcounter do
      begin
        if (fGroup[q] = m) and (fFaceNum[q] = p) then
        begin
          Inc(i);
          Inc(j);
          if i = 1 then
            fText.WriteLn('*surface, type=element, name=s%s', [fGroups[m].Name]);
          if j = 1 then
            fText.WriteLn('%sF%d, S%d', [fGroups[m].Name, p, p]);
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
    fText.Seek(0, 0);
    fSuccessfully := True; // process ended successfully
  except
    fSuccessfully := False;
    HandleException;
  end;
end;

initialization
  FormatSettings.DecimalSeparator := '.';

end.
