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

unit uEncodingFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TStreamEncoding = (seAnsi, seUtf8, seUtf8Bom, seUtf16Le, seUtf16Be);

  function AutoDetectEncoding(Stream: TStream): TStreamEncoding;
  function StreamToUTF8Lines(Stream: TStream; const FromEncoding: TStreamEncoding;
    Lines: TStrings): Boolean;

implementation

uses LazUTF8, LConvEncoding;

const
  ENCODINGS: array[TStreamEncoding] of string =
    (EncodingAnsi, EncodingUTF8, EncodingUTF8BOM, EncodingUCS2LE, EncodingUCS2BE);


function GetLineBreakStyle(const s: string): TTextLineBreakStyle;
var
  Start, Finish, Current: PAnsiChar;
begin
  Start:=PAnsiChar(s);
  Finish:=Start+Length(s);
  Current:=Start;
  while Current<Finish do begin
    case Current[0] of
      #10,#13:
        begin
          if (Current[0]=#13) then begin
            if (Current[1]=#10) then
              Result:=tlbsCRLF
            else
              Result:=tlbsCR;
          end
          else
            Result:=tlbsLF;
          Exit;
        end;
    end;
    Inc(Current);
  end;
  Result:=DefaultTextLineBreakStyle;
end;

function Compare(p1, p2: PAnsiChar; Count: Integer): Boolean;
var
  i: Integer;
  Chr1: Byte;
  Chr2: Byte;
begin
  for i:=1 to Count do begin
    Chr1:=Byte(p1^);
    Chr2:=Byte(p2^);
    if Chr1<>Chr2 then
      Exit(false);
    Inc(p1);
    Inc(p2);
  end;
  Result:=true;
end;

function AutoDetectEncoding(Stream: TStream): TStreamEncoding;
var
  bf: AnsiString;
  br,i: Integer;
  p: PAnsiChar;
begin
  result:=seAnsi;
  SetLength(bf,4096);
  br:=Stream.Read(Pointer(bf)^,4096);
  if br=0 then
    Exit;
  Stream.Seek(-br,soFromCurrent);

  p:=StrPLCopy(PAnsiChar(bf),bf,br);
  if Compare(p,UTF8BOM,3) then
    Exit(seUtf8Bom);
  if Compare(p,UTF16LEBOM,2) then
    Exit(seUtf16Le);
  if Compare(p,UTF16BEBOM,2) then
    Exit(seUtf16Be);

  i:=0;
  while p^<>#0 do begin
    if Byte(p^)<128 then
      Inc(p)
    else begin
      i:=UTF8CharacterStrictLength(p);
      if i=0 then
        Exit;
      Inc(p,i);
    end;
  end;
  if i>0 then
    Result:=seUtf8;
end;

function StreamToUTF8Lines(Stream: TStream; const FromEncoding: TStreamEncoding;
  Lines: TStrings): Boolean;
var
  s: string;
  br,n: Integer;
begin
  result:=false;
  case FromEncoding of
    seUtf8Bom: n:=3;
    seUtf16Le, seUtf16Be: n:=2;
    else n:=0;
  end;
  // Ignore any BOM
  SetLength(s,Stream.Size-n);
  Stream.Seek(n,0);
  br:=Stream.Read(Pointer(s)^,Length(s));
  if br=0 then
    Exit;
  if not (FromEncoding in [seUtf8,seUtf8Bom]) then
    s:=ConvertEncodingToUTF8(s,ENCODINGS[FromEncoding],result);
  Lines.Text:=s;
  // Set original LB
  Lines.TextLineBreakStyle:=GetLineBreakStyle(s);
end;


end.

