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

unit dxPSEdgePatterns;

interface

{$I cxVer.inc}

uses
  Types, Classes, Windows, Graphics, dxPSSngltn, dxBase, dxPSCore, dxCore;

type
  TdxPSCellEdgePatternOrientation = (cepoHorizontal, cepoVertical);

  TdxPSEdgePattern = class(TdxPSCustomCellBorder)
  public
    class procedure Register; override;
    class procedure Unregister; override;

    class function Bits(Index: Integer): DWORD; virtual;
    class function Name: string; virtual;
    class function RequiredBrushOrigin: Boolean; virtual;
    class function RequiredScaling: Boolean; virtual;
    class function Size: TSize; virtual;
    class function Solid: Boolean; override;
    class function Thickness: Integer; override;
  end;

  TdxPSEdgePatternClass = class of TdxPSEdgePattern;

  TdxPSSolidEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function RequiredBrushOrigin: Boolean; override;
    class function RequiredScaling: Boolean; override;
    class function Size: TSize; override;
    class function Solid: Boolean; override;
    class function Thickness: Integer; override;
  end;

  TdxPSEdgePatternItem = class
  private
    FBitmaps: array[TdxPSCellEdgePatternOrientation] of TBitmap;
    FBrushes: array[TdxPSCellEdgePatternOrientation] of TBrush;
    function GetBitmap(Orientation: TdxPSCellEdgePatternOrientation): TBitmap;
    function GetBrush(Orientation: TdxPSCellEdgePatternOrientation): TBrush;
  protected
    procedure Initialize(APattern: TdxPSEdgePatternClass; AIsPrinting: Boolean); virtual;
  public
    constructor Create(APattern: TdxPSEdgePatternClass; AIsPrinting: Boolean);
    destructor Destroy; override;
    property Bitmaps[Orientation: TdxPSCellEdgePatternOrientation]: TBitmap read GetBitmap;
    property Brushes[Orientation: TdxPSCellEdgePatternOrientation]: TBrush read GetBrush;
  end;

  TdxPSEdgePatternFactory = class(TBasedxPSSingleton)
  private
    FItems: TList;
    FPatterns: TdxClassList;
    FPrintItems: TList;
    function GetCount: Integer;
    function GetInternalItem(Index: Integer; IsPrinting: Boolean): TdxPSEdgePatternItem;
    function GetItem(Pattern: TdxPSEdgePatternClass; IsPrinting: Boolean): TdxPSEdgePatternItem;
    function GetPattern(Index: Integer): TdxPSEdgePatternClass;
    procedure SetInternalItem(Index: Integer; IsPrinting: Boolean; Value: TdxPSEdgePatternItem);

    procedure AddPattern(APattern: TdxPSEdgePatternClass);
    procedure RemovePattern(APattern: TdxPSEdgePatternClass);

    function HasItem(APattern: TdxPSEdgePatternClass): Boolean;
    function HasPrintItem(APattern: TdxPSEdgePatternClass): Boolean;

    procedure RemovePatternItem(APattern: TdxPSEdgePatternClass);
    procedure RemovePatternPrintItem(APattern: TdxPSEdgePatternClass);
  protected
    procedure FinalizeInstance; override;
    procedure InitializeInstance; override;
    function IndexOf(APattern: TdxPSEdgePatternClass): Integer;
    property InternalItems[Index: Integer; IsPrinting: Boolean]: TdxPSEdgePatternItem read GetInternalItem write SetInternalItem;
  public
    class function Instance: TdxPSEdgePatternFactory; reintroduce; overload;
    procedure Register(APattern: TdxPSEdgePatternClass);
    procedure Unregister(APattern: TdxPSEdgePatternClass);

    procedure ResetPrintItems;

    property Count: Integer read GetCount;
    property Items[Pattern: TdxPSEdgePatternClass; IsPrinting: Boolean]: TdxPSEdgePatternItem read GetItem; default;
    property Patterns[Index: Integer]: TdxPSEdgePatternClass read GetPattern;
  end;

function dxPSEdgePatternFactory: TdxPSEdgePatternFactory;

implementation

uses
  SysUtils, Forms, cxClasses, dxPSRes, dxPrnDev, dxPSUtl, cxGeometry;

function dxPSEdgePatternFactory: TdxPSEdgePatternFactory;
begin
  Result := TdxPSEdgePatternFactory.Instance;
end;

{ TcxPSEdgePattern }

class procedure TdxPSEdgePattern.Register;
begin
  inherited;
  dxPSEdgePatternFactory.Register(Self);
end;

class procedure TdxPSEdgePattern.Unregister;
begin
  inherited;
  dxPSEdgePatternFactory.Unregister(Self);
end;

class function TdxPSEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := 0;
end;

class function TdxPSEdgePattern.Name: string;
begin
  Result := '';
end;

class function TdxPSEdgePattern.RequiredBrushOrigin: Boolean;
begin
  Result := True;
end;

class function TdxPSEdgePattern.RequiredScaling: Boolean;
begin
  Result := True;
end;

class function TdxPSEdgePattern.Size: TSize;
begin
  Result := cxNullSize;
end;

class function TdxPSEdgePattern.Solid: Boolean;
begin
  Result := False;
end;

class function TdxPSEdgePattern.Thickness: Integer;
begin
  Result := 0;
end;

{ TdxPSSolidEdgePattern }

class function TdxPSSolidEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $FFFFFFFF;
end;

class function TdxPSSolidEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxSolidEdgePattern);
end;

class function TdxPSSolidEdgePattern.RequiredBrushOrigin: Boolean;
begin
  Result := False;
end;

class function TdxPSSolidEdgePattern.RequiredScaling: Boolean;
begin
  Result := False;
end;

class function TdxPSSolidEdgePattern.Size: TSize;
begin
  Result := cxSize(1, 1);
end;

class function TdxPSSolidEdgePattern.Solid: Boolean;
begin
  Result := True;
end;

class function TdxPSSolidEdgePattern.Thickness: Integer;
begin
  Result := 1;
end;

{ TdxPSEdgePatternItem }

constructor TdxPSEdgePatternItem.Create(APattern: TdxPSEdgePatternClass; AIsPrinting: Boolean);
begin
  inherited Create;
  FBitmaps[cepoHorizontal] := TBitmap.Create;
  FBitmaps[cepoVertical] := TBitmap.Create;
  FBrushes[cepoHorizontal] := TBrush.Create;
  FBrushes[cepoVertical] := TBrush.Create;
  Initialize(APattern, AIsPrinting);
end;

destructor TdxPSEdgePatternItem.Destroy;
begin
  FBrushes[cepoVertical].Free;
  FBrushes[cepoHorizontal].Free;
  FBitmaps[cepoVertical].Free;
  FBitmaps[cepoHorizontal].Free;
  inherited Destroy;
end;

procedure TdxPSEdgePatternItem.Initialize(APattern: TdxPSEdgePatternClass; AIsPrinting: Boolean);

  procedure ScaleBitmap(ABitmap: TBitmap);
  var
    H, W, Numerator, Denominator: Integer;
    TempBitmap: TBitmap;
  begin
    H := ABitmap.Height;
    W := ABitmap.Width;
    Numerator := GetDeviceCaps(dxPrintDevice.Handle, LOGPIXELSY);
    Denominator := Screen.PixelsPerInch;
    TempBitmap := TBitmap.Create;
    try
      TempBitmap.HandleType := bmDIB;
      TempBitmap.Assign(ABitmap);
      ABitmap.Height := H * Numerator div Denominator;
      ABitmap.Height := ABitmap.Height - ABitmap.Height mod H;
      ABitmap.Width := W * Numerator div Denominator;
      ABitmap.Width := ABitmap.Width - ABitmap.Width mod W;
      ABitmap.Canvas.StretchDraw(Rect(0, 0, ABitmap.Width, ABitmap.Height), TempBitmap);
    finally
      TempBitmap.Free;
    end;
  end;

  procedure InitializeOrientation(AOrientation: TdxPSCellEdgePatternOrientation);
  const
    Colors: array[Boolean] of TColor = (clWhite, clBlack);
  var
    ABitmap: TBitmap;
    AColor: TColor;
    ASize: TSize;
    I, J: Integer;
  begin
    ASize := APattern.Size;
    ABitmap := FBitmaps[AOrientation];
    if AOrientation = cepoVertical then
      ABitmap.SetSize(ASize.cy, ASize.cx)
    else
      ABitmap.SetSize(ASize.cx, ASize.cy);

    ABitmap.Monochrome := True;
    ABitmap.HandleType := bmDIB;
    for I := 0 to ASize.cx - 1 do
      for J := 0 to ASize.cy - 1 do
      begin
        AColor := Colors[APattern.Bits(J) and (1 shl I) <> 0];
        if AOrientation = cepoVertical then
          ABitmap.Canvas.Pixels[J, I] := AColor
        else
          ABitmap.Canvas.Pixels[I, J] := AColor;
      end;

   if AIsPrinting and APattern.RequiredScaling then
     ScaleBitmap(ABitmap);
    FBrushes[AOrientation].Bitmap := ABitmap;
  end;

begin
  InitializeOrientation(cepoHorizontal);
  InitializeOrientation(cepoVertical);
end;

function TdxPSEdgePatternItem.GetBitmap(Orientation: TdxPSCellEdgePatternOrientation): TBitmap;
begin
  Result := FBitmaps[Orientation];
end;

function TdxPSEdgePatternItem.GetBrush(Orientation: TdxPSCellEdgePatternOrientation): TBrush;
begin
  Result := FBrushes[Orientation];
end;

{ TdxPSEdgePatternFactory }

class function TdxPSEdgePatternFactory.Instance: TdxPSEdgePatternFactory;
begin
  Result := inherited Instance as TdxPSEdgePatternFactory;
end;

procedure TdxPSEdgePatternFactory.Register(APattern: TdxPSEdgePatternClass);
begin
  if IndexOf(APattern) = -1 then AddPattern(APattern);
end;

procedure TdxPSEdgePatternFactory.Unregister(APattern: TdxPSEdgePatternClass);
begin
  RemovePattern(APattern);
end;

procedure TdxPSEdgePatternFactory.ResetPrintItems;
var
  I: Integer;
begin
  for I := 0 to FPrintItems.Count - 1 do
  begin
    TObject(FPrintItems[I]).Free;
    FPrintItems[I] := nil;
  end;
end;

procedure TdxPSEdgePatternFactory.FinalizeInstance;

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

procedure TdxPSEdgePatternFactory.InitializeInstance;
begin
  inherited;
  FItems := TList.Create;
  FPatterns := TdxClassList.Create;
  FPrintItems := TList.Create;
end;

function TdxPSEdgePatternFactory.IndexOf(APattern: TdxPSEdgePatternClass): Integer;
begin
  Result := FPatterns.IndexOf(APattern);
end;

function TdxPSEdgePatternFactory.GetCount: Integer;
begin
  Result := FPatterns.Count;
end;

function TdxPSEdgePatternFactory.GetInternalItem(Index: Integer;
  IsPrinting: Boolean): TdxPSEdgePatternItem;
begin
  if IsPrinting then
    Result := TdxPSEdgePatternItem(FPrintItems[Index])
  else
    Result := TdxPSEdgePatternItem(FItems[Index]);
end;

function TdxPSEdgePatternFactory.GetItem(Pattern: TdxPSEdgePatternClass;
  IsPrinting: Boolean): TdxPSEdgePatternItem;
var
  Index: Integer;
begin
  if Pattern <> nil then
  begin
    Index := IndexOf(Pattern);
    if Index = -1 then
      raise EdxException.CreateFmt(cxGetResourceString(@sdxPatternIsNotRegistered), [Pattern.ClassName]);
    if InternalItems[Index, IsPrinting] = nil then
      InternalItems[Index, IsPrinting] := TdxPSEdgePatternItem.Create(Pattern, IsPrinting);
    Result := InternalItems[Index, IsPrinting];
  end
  else
    Result := nil;
end;

function TdxPSEdgePatternFactory.GetPattern(Index: Integer): TdxPSEdgePatternClass;
begin
  Result := TdxPSEdgePatternClass(FPatterns[Index]);
end;

procedure TdxPSEdgePatternFactory.SetInternalItem(Index: Integer; IsPrinting: Boolean;
  Value: TdxPSEdgePatternItem);
begin
  if IsPrinting then
    FPrintItems[Index] := Value
  else
    FItems[Index] := Value;
end;

procedure TdxPSEdgePatternFactory.AddPattern(APattern: TdxPSEdgePatternClass);
begin
  FPatterns.Add(APattern);
  FItems.Add(nil);
  FPrintItems.Add(nil);
end;

procedure TdxPSEdgePatternFactory.RemovePattern(APattern: TdxPSEdgePatternClass);
begin
  if HasItem(APattern) then RemovePatternItem(APattern);
  if HasPrintItem(APattern) then RemovePatternPrintItem(APattern);
  FPatterns.Remove(APattern)
end;

function TdxPSEdgePatternFactory.HasItem(APattern: TdxPSEdgePatternClass): Boolean;
begin
  Result := InternalItems[IndexOf(APattern), False] <> nil;
end;

function TdxPSEdgePatternFactory.HasPrintItem(APattern: TdxPSEdgePatternClass): Boolean;
begin
  Result := InternalItems[IndexOf(APattern), True] <> nil;
end;

procedure TdxPSEdgePatternFactory.RemovePatternItem(APattern: TdxPSEdgePatternClass);
var
  Item: TdxPSEdgePatternItem;
begin
  Item := Items[APattern, False];
  FItems.Remove(Item);
  Item.Free;
end;

procedure TdxPSEdgePatternFactory.RemovePatternPrintItem(APattern: TdxPSEdgePatternClass);
var
  Item: TdxPSEdgePatternItem;
begin
  Item := Items[APattern, True];
  FPrintItems.Remove(Item);
  Item.Free;
end;

initialization
  TdxPSSolidEdgePattern.Register;

end.
