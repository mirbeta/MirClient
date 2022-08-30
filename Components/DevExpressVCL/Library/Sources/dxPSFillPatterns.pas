{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxPSFillPatterns;

interface

{$I cxVer.inc}

uses
  Classes, Windows, Types, Graphics,
  dxCore, dxBase, dxPSSngltn;

type
  TdxPSPatternBitRow = Word;
  TdxPSPatternBitRows = array[0..7] of TdxPSPatternBitRow;

  TdxPSFillPatternClass = class of TdxPSFillPattern;

  TdxPSFillPattern = class(TPersistent)
  public
    class procedure Register; virtual;
    class procedure Unregister; virtual;

    class procedure Bits(var APattern: TdxPSPatternBitRows); virtual;
    class function Dimensions: TSize; virtual;
    class function Name: string; virtual;
    class function RequiredBrushOrigin: Boolean; virtual;
    class function RequiredScaling: Boolean; virtual;
    class function Solid: Boolean; virtual;
  end;

  TdxPSSolidFillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
    class function RequiredBrushOrigin: Boolean; override;
    class function RequiredScaling: Boolean; override;
    class function Solid: Boolean; override;
  end;

  TdxPSGray50FillPattern = class(TdxPSFillPattern)
  public
    class procedure Bits(var APattern: TdxPSPatternBitRows); override;
    class function Name: string; override;
  end;

  { TdxPSFillPatternItem }

  TdxPSFillPatternItem = class(TObject)
  private
    FBitmap: TBitmap;
    FBrush: TBrush;
    FPattern: TdxPSFillPatternClass;
  protected
    procedure CreatePatternBitmap(ABitmap: TBitmap);
    procedure Initialize(AIsPrinting: Boolean);
  public
    constructor Create(APattern: TdxPSFillPatternClass; AIsPrinting: Boolean);
    destructor Destroy; override;

    property Bitmap: TBitmap read FBitmap;
    property Brush: TBrush read FBrush;
    property Pattern: TdxPSFillPatternClass read FPattern;
  end;

  TdxPSFillPatternFactory = class(TBasedxPSSingleton)
  private
    FItems: TList;
    FPatterns: TdxClassList;
    FPrintItems: TList;

    function GetCount: Integer;
    function GetInternalItem(Index: Integer; IsPrinting: Boolean): TdxPSFillPatternItem;
    function GetItem(Pattern: TdxPSFillPatternClass; IsPrinting: Boolean): TdxPSFillPatternItem;
    function GetPattern(Index: Integer): TdxPSFillPatternClass;
    procedure SetInternalItem(Index: Integer; IsPrinting: Boolean; Value: TdxPSFillPatternItem);

    procedure AddPattern(APattern: TdxPSFillPatternClass);
    procedure RemovePattern(APattern: TdxPSFillPatternClass);

    function HasItem(APattern: TdxPSFillPatternClass): Boolean;
    function HasPrintItem(APattern: TdxPSFillPatternClass): Boolean;

    procedure RemovePatternItem(APattern: TdxPSFillPatternClass);
    procedure RemovePatternPrintItem(APattern: TdxPSFillPatternClass);
  protected
    procedure FinalizeInstance; override;
    procedure InitializeInstance; override;
    function IndexOf(APattern: TdxPSFillPatternClass): Integer;
    property InternalItems[Index: Integer; IsPrinting: Boolean]: TdxPSFillPatternItem read GetInternalItem write SetInternalItem;
  public
    class function Instance: TdxPSFillPatternFactory; reintroduce; overload;
    procedure Register(APattern: TdxPSFillPatternClass);
    procedure Unregister(APattern: TdxPSFillPatternClass);

    procedure ResetPrintItems;
    property Count: Integer read GetCount;
    property Items[Pattern: TdxPSFillPatternClass; IsPrinting: Boolean]: TdxPSFillPatternItem read GetItem; default;
    property Patterns[Index: Integer]: TdxPSFillPatternClass read GetPattern;
  end;

function dxPSFillPatternFactory: TdxPSFillPatternFactory;

implementation

uses
  SysUtils, Forms, Math, cxClasses, dxPSRes, dxPrnDev, dxPSUtl, dxPSCore;

function dxPSFillPatternFactory: TdxPSFillPatternFactory;
begin
  Result := TdxPSFillPatternFactory.Instance;
end;

{ TdxPSFillPattern }

class procedure TdxPSFillPattern.Register;
begin
  dxPSFillPatternFactory.Register(Self);
end;

class procedure TdxPSFillPattern.Unregister;
begin
  dxPSFillPatternFactory.Unregister(Self);
end;

class procedure TdxPSFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000);
begin
  APattern := Bits;
end;

class function TdxPSFillPattern.Dimensions: TSize;
begin
  Result.cX := 8;
  Result.cY := 8;
end;

class function TdxPSFillPattern.Name: string;
begin
  Result := '';
end;

class function TdxPSFillPattern.RequiredBrushOrigin: Boolean;
begin
  Result := True;
end;

class function TdxPSFillPattern.RequiredScaling: Boolean;
begin
  Result := True;
end;

class function TdxPSFillPattern.Solid: Boolean;
begin
  Result := False;
end;

{ TdxPSSolidFillPattern }

class procedure TdxPSSolidFillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000);
begin
  APattern := Bits;
end;

class function TdxPSSolidFillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxSolidFillPattern);
end;

class function TdxPSSolidFillPattern.RequiredBrushOrigin: Boolean;
begin
  Result := False;
end;

class function TdxPSSolidFillPattern.RequiredScaling: Boolean;
begin
  Result := False;
end;

class function TdxPSSolidFillPattern.Solid: Boolean;
begin
  Result := True;
end;

{ TdxPSGray50FillPattern }

class procedure TdxPSGray50FillPattern.Bits(var APattern: TdxPSPatternBitRows);
const
  Bits: TdxPSPatternBitRows = ($5555, $AAAA, $5555, $AAAA, $5555, $AAAA, $5555, $AAAA);
begin
  APattern := Bits;
end;

class function TdxPSGray50FillPattern.Name: string;
begin
  Result := cxGetResourceString(@sdxGray50FillPattern);
end;

{ TdxPSFillPatternItem }

constructor TdxPSFillPatternItem.Create(APattern: TdxPSFillPatternClass; AIsPrinting: Boolean);
begin
  inherited Create;
  FPattern := APattern;
  FBrush := TBrush.Create;
  FBitmap := TBitmap.Create;
  Initialize(AIsPrinting);
end;

destructor TdxPSFillPatternItem.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TdxPSFillPatternItem.CreatePatternBitmap(ABitmap: TBitmap);
const
  Colors: array[Boolean] of TColor = (clWhite, clBlack);
var
  APatternBits: TdxPSPatternBitRows;
  APatternRow: TdxPSPatternBitRow;
  I, J: Integer;
begin
  ABitmap.Width := Max(8, Pattern.Dimensions.cx);
  ABitmap.Height := Max(8, Pattern.Dimensions.cy);
  ABitmap.Monochrome := True;
  Pattern.Bits(APatternBits);

  for I := 0 to ABitmap.Width - 1 do
  begin
    APatternRow := APatternBits[I];
    for J := 0 to ABitmap.Height - 1 do
      ABitmap.Canvas.Pixels[J, I] := Colors[APatternRow and (1 shl J) = 0];
  end;
end;

procedure TdxPSFillPatternItem.Initialize(AIsPrinting: Boolean);
begin
  CreatePatternBitmap(Bitmap);
  Brush.Bitmap := Bitmap;
end;

{ TdxPSFillPatternFactory }

class function TdxPSFillPatternFactory.Instance: TdxPSFillPatternFactory;
begin
  Result := inherited Instance as TdxPSFillPatternFactory;
end;

procedure TdxPSFillPatternFactory.Register(APattern: TdxPSFillPatternClass);
begin
  if IndexOf(APattern) = -1 then
  begin
    AddPattern(APattern);
    Classes.RegisterClass(APattern);
  end;
end;

procedure TdxPSFillPatternFactory.Unregister(APattern: TdxPSFillPatternClass);
begin
  RemovePattern(APattern);
  //Classes.UnregisterClass(APattern);
end;

procedure TdxPSFillPatternFactory.ResetPrintItems;
var
  I: Integer;
begin
  for I := 0 to FPrintItems.Count - 1 do
  begin
    TObject(FPrintItems[I]).Free;
    FPrintItems[I] := nil;
  end;
end;

procedure TdxPSFillPatternFactory.FinalizeInstance;

  procedure FreeObjectList(AList: TList);
  var
    I: Integer;
  begin
    for I := 0 to AList.Count - 1 do
      TObject(AList[I]).Free;
    AList.Free;
  end;

begin
  FreeObjectList(FItems);
  FreeObjectList(FPrintItems);
  FreeAndNil(FPatterns);
  inherited;
end;

procedure TdxPSFillPatternFactory.InitializeInstance;
begin
  inherited;
  FItems := TList.Create;
  FPatterns := TdxClassList.Create;
  FPrintItems := TList.Create;
end;

function TdxPSFillPatternFactory.IndexOf(APattern: TdxPSFillPatternClass): Integer;
begin
  Result := FPatterns.IndexOf(APattern);
end;

function TdxPSFillPatternFactory.GetCount: Integer;
begin
  Result := FPatterns.Count;
end;

function TdxPSFillPatternFactory.GetInternalItem(Index: Integer; IsPrinting: Boolean): TdxPSFillPatternItem;
begin
  if IsPrinting then
    Result := TdxPSFillPatternItem(FPrintItems[Index])
  else
    Result := TdxPSFillPatternItem(FItems[Index]);
end;

function TdxPSFillPatternFactory.GetItem(Pattern: TdxPSFillPatternClass; IsPrinting: Boolean): TdxPSFillPatternItem;
var
  Index: Integer;
begin
  if Pattern <> nil then
  begin
    Index := IndexOf(Pattern);
    if Index = -1 then
      raise EdxException.CreateFmt(cxGetResourceString(@sdxPatternIsNotRegistered), [Pattern.ClassName]);
    if InternalItems[Index, IsPrinting] = nil then
      InternalItems[Index, IsPrinting] := TdxPSFillPatternItem.Create(Pattern, IsPrinting);
    Result := InternalItems[Index, IsPrinting];
  end
  else
    Result := nil;
end;

function TdxPSFillPatternFactory.GetPattern(Index: Integer): TdxPSFillPatternClass;
begin
  Result := TdxPSFillPatternClass(FPatterns[Index]);
end;

procedure TdxPSFillPatternFactory.SetInternalItem(Index: Integer; IsPrinting: Boolean;
  Value: TdxPSFillPatternItem);
begin
  if IsPrinting then
    FPrintItems[Index] := Value
  else
    FItems[Index] := Value;
end;

procedure TdxPSFillPatternFactory.AddPattern(APattern: TdxPSFillPatternClass);
begin
  FPatterns.Add(APattern);
  FItems.Add(nil);
  FPrintItems.Add(nil);
end;

procedure TdxPSFillPatternFactory.RemovePattern(APattern: TdxPSFillPatternClass);
begin
  if HasItem(APattern) then RemovePatternItem(APattern);
  if HasPrintItem(APattern) then RemovePatternPrintItem(APattern);
  FPatterns.Remove(APattern);
end;

function TdxPSFillPatternFactory.HasItem(APattern: TdxPSFillPatternClass): Boolean;
begin
  Result := InternalItems[IndexOf(APattern), False] <> nil;
end;

function TdxPSFillPatternFactory.HasPrintItem(APattern: TdxPSFillPatternClass): Boolean;
begin
  Result := InternalItems[IndexOf(APattern), True] <> nil;
end;

procedure TdxPSFillPatternFactory.RemovePatternItem(APattern: TdxPSFillPatternClass);
var
  Item: TdxPSFillPatternItem;
begin
  Item := Items[APattern, False];
  FItems.Remove(Item);
  Item.Free;
end;

procedure TdxPSFillPatternFactory.RemovePatternPrintItem(APattern: TdxPSFillPatternClass);
var
  Item: TdxPSFillPatternItem;
begin
  Item := Items[APattern, True];
  FPrintItems.Remove(Item);
  Item.Free;
end;

initialization
  TdxPSSolidFillPattern.Register;
  TdxPSGray50FillPattern.Register;

end.
