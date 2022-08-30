{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetConditionalFormattingIconSet;

{$I cxVer.Inc}
{$R dxSpreadSheetConditionalFormattingIconSet.res}

interface

uses
  Types, Generics.Defaults, Generics.Collections, cxGraphics, Graphics;

type

  { TdxSpreadSheetConditionalFormattingIconSetPreset }

  TdxSpreadSheetConditionalFormattingIconSetPreset = class
  strict private
    function GetIconIndex(Index: Integer): Integer;
    function GetIconIndexCount: Integer;
  protected
    FDescription: string;
    FIconIndexes: TList<Integer>;
    FName: string;
  public
    constructor Create(const AName: string); overload;
    constructor Create(const AName: string; AIndexes: array of Integer); overload;
    destructor Destroy; override;
    function IndexOf(IconIndex: Integer): Integer;
    //
    property Description: string read FDescription;
    property IconIndexCount: Integer read GetIconIndexCount;
    property IconIndexes[Index: Integer]: Integer read GetIconIndex; default;
    property Name: string read FName;
  end;

  { TdxSpreadSheetConditionalFormattingIconSetPresets }

  TdxSpreadSheetConditionalFormattingIconSetPresets = class(TObjectList<TdxSpreadSheetConditionalFormattingIconSetPreset>)
  public
    function Add(const AName, ADescription: string; AIndexes: array of Integer): Integer;
    function FindByCount(const ACount: Integer; out APreset: TdxSpreadSheetConditionalFormattingIconSetPreset): Boolean;
    function FindByIconIndex(const AIconIndex: Integer;
      out APreset: TdxSpreadSheetConditionalFormattingIconSetPreset; out AIndexInPreset: Integer): Boolean;
    function FindByName(const AName: string; out APreset: TdxSpreadSheetConditionalFormattingIconSetPreset): Boolean;
  end;

  { TdxSpreadSheetConditionalFormattingIconSet }

  TdxSpreadSheetConditionalFormattingIconSet = class
  strict private
    FIcons: TcxImageList;
    FPresets: TdxSpreadSheetConditionalFormattingIconSetPresets;
    FPresetPreviews: TcxImageList;

    function GetPresetPreviews: TcxImageList;
  protected
    procedure PopulateIcons;
    procedure PopulatePresets;
    procedure PopulatePresetPreviews;
  public
    constructor Create;
    destructor Destroy; override;
    //
    property Icons: TcxImageList read FIcons;
    property Presets: TdxSpreadSheetConditionalFormattingIconSetPresets read FPresets;
    property PresetPreviews: TcxImageList read GetPresetPreviews;
  end;

function ConditionalFormattingIconSet: TdxSpreadSheetConditionalFormattingIconSet;
implementation

uses
  SysUtils, dxGDIPlusClasses, cxGeometry, Math;

const
  sdxIconSet3ArrowsDescription = '3 Arrows';
  sdxIconSet3ArrowsGrayDescription = '3 Arrows (Gray)';
  sdxIconSet3FlagsDescription = '3 Flags';
  sdxIconSet3SignsDescription = '3 Signs';
  sdxIconSet3SymbolsDescription = '3 Symbols (Circled)';
  sdxIconSet3Symbols2Description = '3 Symbols';
  sdxIconSet3TrafficLights1Description = '3 Traffic Lights';
  sdxIconSet3TrafficLights2Description = '3 Traffic Lights (Rimmed)';
  sdxIconSet3TrianglesDescription = '3 Triangles';
  sdxIconSet4ArrowsDescription = '4 Arrows';
  sdxIconSet4ArrowsGrayDescription = '4 Arrows (Gray)';
  sdxIconSet4RatingDescription = '4 Ratings';
  sdxIconSet4RedToBlackDescription = 'Red to Black';
  sdxIconSet4TrafficLightsDescription = '4 Traffic Lights';
  sdxIconSet5ArrowsDescription = '5 Arrows';
  sdxIconSet5ArrowsGrayDescription = '5 Arrows (Gray)';
  sdxIconSet5BoxesDescription = '5 Boxes';
  sdxIconSet5QuartersDescription = '5 Quarters';
  sdxIconSet5RatingDescription = '5 Ratings';

var
  FIconSet: TdxSpreadSheetConditionalFormattingIconSet;

function ConditionalFormattingIconSet: TdxSpreadSheetConditionalFormattingIconSet;
begin
  if FIconSet = nil then
    FIconSet := TdxSpreadSheetConditionalFormattingIconSet.Create;
  Result := FIconSet;
end;

{ TdxSpreadSheetConditionalFormattingIconSetPreset }

constructor TdxSpreadSheetConditionalFormattingIconSetPreset.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FIconIndexes := TList<Integer>.Create;
end;

constructor TdxSpreadSheetConditionalFormattingIconSetPreset.Create(const AName: string; AIndexes: array of Integer);
var
  I: Integer;
begin
  Create(AName);
  FIconIndexes.Capacity := Length(AIndexes);
  for I := 0 to Length(AIndexes) - 1 do
    FIconIndexes.Add(AIndexes[I]);
end;

destructor TdxSpreadSheetConditionalFormattingIconSetPreset.Destroy;
begin
  FreeAndNil(FIconIndexes);
  inherited Destroy;
end;

function TdxSpreadSheetConditionalFormattingIconSetPreset.IndexOf(IconIndex: Integer): Integer;
begin
  Result := FIconIndexes.IndexOf(IconIndex);
end;

function TdxSpreadSheetConditionalFormattingIconSetPreset.GetIconIndex(Index: Integer): Integer;
begin
  Result := FIconIndexes[Index];
end;

function TdxSpreadSheetConditionalFormattingIconSetPreset.GetIconIndexCount: Integer;
begin
  Result := FIconIndexes.Count;
end;

{ TdxSpreadSheetConditionalFormattingIconSetPresets }

function TdxSpreadSheetConditionalFormattingIconSetPresets.Add(
  const AName, ADescription: string; AIndexes: array of Integer): Integer;
var
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
begin
  APreset := TdxSpreadSheetConditionalFormattingIconSetPreset.Create(AName, AIndexes);
  APreset.FDescription := ADescription;
  Result := inherited Add(APreset);
end;

function TdxSpreadSheetConditionalFormattingIconSetPresets.FindByCount(
  const ACount: Integer; out APreset: TdxSpreadSheetConditionalFormattingIconSetPreset): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    APreset := Items[I];
    if APreset.IconIndexCount = ACount then
      Exit(True);
  end;
  Result := False;
end;

function TdxSpreadSheetConditionalFormattingIconSetPresets.FindByIconIndex(const AIconIndex: Integer;
  out APreset: TdxSpreadSheetConditionalFormattingIconSetPreset; out AIndexInPreset: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    APreset := Items[I];
    AIndexInPreset := APreset.IndexOf(AIconIndex);
    if AIndexInPreset >= 0 then
      Exit(True);
  end;
  Result := False;
end;

function TdxSpreadSheetConditionalFormattingIconSetPresets.FindByName(
  const AName: string; out APreset: TdxSpreadSheetConditionalFormattingIconSetPreset): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    APreset := Items[I];
    if APreset.Name = AName then
      Exit(True);
  end;
  Result := False;
end;

{ TdxSpreadSheetConditionalFormattingIconSet }

constructor TdxSpreadSheetConditionalFormattingIconSet.Create;
begin
  inherited Create;
  FIcons := TcxImageList.CreateSize(16, 16);
  FPresets := TdxSpreadSheetConditionalFormattingIconSetPresets.Create;
  PopulateIcons;
  PopulatePresets;
end;

destructor TdxSpreadSheetConditionalFormattingIconSet.Destroy;
begin
  FreeAndNil(FPresetPreviews);
  FreeAndNil(FPresets);
  FreeAndNil(FIcons);
  inherited Destroy;
end;

procedure TdxSpreadSheetConditionalFormattingIconSet.PopulateIcons;
var
  ASmartImage: TdxPNGImage;
begin
  ASmartImage := TdxPNGImage.Create;
  try
    ASmartImage.LoadFromResource(HInstance, 'DXSPREADSHEET_CONDITIONAL_FORMATTING_ICONSET', 'PNG');
    FIcons.Add(ASmartImage);
  finally
    ASmartImage.Free;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingIconSet.PopulatePresets;
begin
  Presets.Add('3Arrows', sdxIconSet3ArrowsDescription, [2, 1, 0]);
  Presets.Add('3ArrowsGray', sdxIconSet3ArrowsGrayDescription, [5, 4, 3]);
  Presets.Add('3Flags', sdxIconSet3FlagsDescription, [8, 7, 6]);
  Presets.Add('3Signs', sdxIconSet3SignsDescription, [17, 16, 9]);
  Presets.Add('3Symbols', sdxIconSet3SymbolsDescription, [20, 19, 18]);
  Presets.Add('3Symbols2', sdxIconSet3Symbols2Description, [23, 22, 21]);
  Presets.Add('3TrafficLights1', sdxIconSet3TrafficLights1Description, [11, 10, 9]);
  Presets.Add('3TrafficLights2', sdxIconSet3TrafficLights2Description, [15, 14, 13]);
  Presets.Add('3Triangles', sdxIconSet3TrianglesDescription, [46, 45, 44]);
  Presets.Add('4Arrows', sdxIconSet4ArrowsDescription, [2, 25, 24, 0]);
  Presets.Add('4ArrowsGray', sdxIconSet4ArrowsGrayDescription, [5, 27, 26, 3]);
  Presets.Add('4Rating', sdxIconSet4RatingDescription, [37, 38, 39, 40]);
  Presets.Add('4RedToBlack', sdxIconSet4RedToBlackDescription, [31, 30, 29, 28]);
  Presets.Add('4TrafficLights', sdxIconSet4TrafficLightsDescription, [12, 11, 10, 9]);
  Presets.Add('5Arrows', sdxIconSet5ArrowsDescription, [2, 25, 1, 24, 0]);
  Presets.Add('5ArrowsGray', sdxIconSet5ArrowsGrayDescription, [5, 27, 4, 26, 3]);
  Presets.Add('5Boxes', sdxIconSet5BoxesDescription, [51, 50, 49, 48, 47]);
  Presets.Add('5Quarters', sdxIconSet5QuartersDescription, [35, 34, 33, 32, 31]);
  Presets.Add('5Rating', sdxIconSet5RatingDescription, [36, 37, 38, 39, 40]);

  Presets.Add('NoIcons', '', [-1]);
end;

procedure TdxSpreadSheetConditionalFormattingIconSet.PopulatePresetPreviews;
const
  SpaceBetweenIcons = 5;

  function MeasureWidth: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Presets.Count - 1 do
      Result := Max(Result, Presets[I].IconIndexCount);
    Result := (Icons.Width + SpaceBetweenIcons) * Result - SpaceBetweenIcons;
  end;

  procedure AddPreview(APreset: TdxSpreadSheetConditionalFormattingIconSetPreset);
  var
    ABitmap: TcxBitmap32;
    I: Integer;
    R: TRect;
  begin
    ABitmap := TcxBitmap32.CreateSize(FPresetPreviews.Width, FPresetPreviews.Height, True);
    try
      R := cxRectSetSize(ABitmap.ClientRect, Icons.Width, Icons.Height);
      for I := 0 to APreset.IconIndexCount - 1 do
      begin
        Icons.Draw(ABitmap.Canvas, R, APreset.IconIndexes[I]);
        R := cxRectOffset(R, cxRectWidth(R) + SpaceBetweenIcons, 0);
      end;
      FPresetPreviews.Add(ABitmap, nil);
    finally
      ABitmap.Free;
    end;
  end;

var
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
  I: Integer;
begin
  FPresetPreviews.Width := MeasureWidth;
  FPresetPreviews.Height := Icons.Height;
  for I := 0 to Presets.Count - 1 do
  begin
    APreset := Presets[I];
    if APreset.IndexOf(-1) < 0 then
      AddPreview(APreset);
  end;
end;

function TdxSpreadSheetConditionalFormattingIconSet.GetPresetPreviews: TcxImageList;
begin
  if FPresetPreviews = nil then
  begin
    FPresetPreviews := TcxImageList.Create(nil);
    PopulatePresetPreviews;
  end;
  Result := FPresetPreviews;
end;

initialization

finalization
  FreeAndNil(FIconSet);
end.
