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

unit uInpTypes;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, GL;

const
  SET_COLS = 9;

type

  TBuffIndex = 0..15;
  TIntBuffer = array[TBuffIndex] of Integer;

  TElementType = (
    etS3, etDS3, etM3D3, etSFM3D3, etSTRI3, etCPS3, etCPE3, etCAX3,
    etS6, etDS6, etM3D6, etSC6, etSFM3D6, etSTRI65, etCPS6, etCPE6, etCAX6,
    etS4, etS4R, etCPS4, etCPS4R, etCPE4, etCPE4R, etCAX4, etCAX4R,
    etS8, etS8R, etCPS8, etCPS8R, etCPE8, etCPE8R, etCAX8, etCAX8R,
    etC3D4, etC3D10, etC3D8, etC3D8R, etC3D8I,
    etC3D20, etC3D20R, etC3D20RI, etC3D6, etC3D15, etB21, etB31, etB31R,
    etT3D2, etB22, etB32, etB32R, etT3D3, etUnknown);

  TElementCategory = (
    ecTria3, ecTria6, ecQuad4, ecQuad8, ecTetra4, ecTetra10, ecHexa8, ecHexa20,
    ecPenta6, ecPenta15, ecSeg2, ecSeg3, ecUnknown);

  TQuat = array[0..3] of Double;
  TQuatMatrix = array[0..3] of TQuat;

  TScale = record
    X: Double;
    Y: Double;
    Z: Double;
    W: Double;
    SMin: Double;
    SMax: Double;
    XMin: Double;
    XMax: Double;
    YMin: Double;
    YMax: Double;
    ZMin: Double;
    ZMax: Double;
  end;

  TPoint3D = record
    X: Double;
    Y: Double;
    Z: Double;
  end;
  TPoint3DDynArray = array of TPoint3D;

  TNode = record
    Number: Integer;
    Index: Integer;
    Flag: Integer;
    X: Double;
    Y: Double;
    Z: Double;
  end;
  TNodeDynArray = array of TNode;

  TNodeBuffer = array[0..26] of Integer;

  TElement = record
    Number: Integer;
    Category: TElementCategory;
    Group: Integer;
    Mat: Integer;
    Attr: Integer;
    Node: TNodeBuffer;
    Side: TPoint3DDynArray;
  end;
  TElementDynArray = array of TElement;

  TElementBuffer = array[0..39] of Integer;

  TFace = record
    Number: Integer;
    Index: array[0..5] of Integer;
    ElemNumber: Integer;
    Category: TElementCategory;
    Group: Integer;
    Mat: Integer;
    Node: array[0..9] of Integer;
    Side: TPoint3DDynArray;
  end;
  TFaceDynArray = array of TFace;

  TEdge = record
    p1: Integer;
    p2: Integer;
  end;
  TEdgeDynArray = array of TEdge;

  TElface = record
    e: Integer;
    f: Integer;
  end;
  TElfaceDynArray = array of TElface;

  TSet = record
    Name: string;
    NumNodes: Integer;
    NumElements: Integer;
    NumFaces: Integer;
    NumSets: Integer;
    NumShapes: Integer;
    NumElfaces: Integer;
    Nodes: TIntegerDynArray;
    Elements: TIntegerDynArray;
    Faces: TIntegerDynArray;
    Sets: TIntegerDynArray;
    Shapes: TIntegerDynArray;
    Elfaces: TElfaceDynArray;
  end;
  TSetDynArray = array of TSet;

  TSummen = record
    Model: string;
    UHeader: string;
    PHeader: string;
    u: Integer;
    p: Integer;
    n: Integer;
    e: Integer;
    f: Integer;
    g: Integer;
    t: Integer;
    sets: Integer;
    mats: Integer;
    amps: Integer;
    l: Integer;
    b: Integer;
    c: Integer;
    etype: array[TElementCategory] of Integer;
    nmax: Integer;
    nmin: Integer;
    emax: Integer;
    emin: Integer;
    orignmax: Integer;
    orign: Integer;
    olc: Integer;
    noffs: Integer;
    eoffs: Integer;
  end;

  TColorEntity = record
    k: AnsiChar;
    r: GLfloat;
    g: GLfloat;
    b: GLfloat;
  end;

const

  COLOR_ENTITY: array[0..SET_COLS-1] of TColorEntity = (
    (k: 'k'; r: 0.0; g: 0.0; b: 0.0),
    (k: 'w'; r: 1.0; g: 1.0; b: 1.0),
    (k: 'n'; r: 0.6; g: 0.6; b: 0.6),
    (k: 'r'; r: 1.0; g: 0.0; b: 0.0),
    (k: 'g'; r: 0.0; g: 1.0; b: 0.0),
    (k: 'b'; r: 0.0; g: 0.0; b: 1.0),
    (k: 'y'; r: 1.0; g: 1.0; b: 0.0),
    (k: 'm'; r: 1.0; g: 0.0; b: 1.0),
    (k: 't'; r: 0.0; g: 1.0; b: 1.0));

implementation


end.

