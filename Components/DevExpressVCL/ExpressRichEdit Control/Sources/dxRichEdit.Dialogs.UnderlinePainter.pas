{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Dialogs.UnderlinePainter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Variants, Windows, Classes, Controls, Dialogs, Forms, Graphics, Messages, Types,
  Printers, SysUtils, Generics.Defaults, Generics.Collections, dxCoreClasses, dxCoreGraphics, cxGeometry,
  cxDropDownEdit, cxGraphics, dxCore, dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.PatternLine,
  dxRichEdit.Platform.PatternLinePainter,
  dxRichEdit.DocumentLayout.UnitDocumentConverter;

type
  TdxNullableUnderlineType = TdxNullableValue<TdxUnderlineType>;

  { TdxEmptyDocumentLayoutUnitConverter }

  TdxEmptyDocumentLayoutUnitConverter = TdxDocumentLayoutUnitDocumentConverter;

  { TdxScreenCharacterLinePainterParameters }

  TdxScreenCharacterLinePainterParameters = class(TdxPatternLinePainterParameters)
  protected
    function PixelsToUnits(AValue: Single; ADpi: Single): Single; override;
  end;

  { TdxScreenCharacterLinePainter }

  TdxScreenCharacterLinePainter = class(TdxRichEditPatternLinePainter)
  strict private
    class var
      FParametersTable: TdxPatternLinePainterParametersTable;
  strict private
    class constructor Initialize;
    class destructor Finalize;
  protected
    function CreateParameters: TdxPatternLinePainterParameters; override;
    function GetParametersTable: TdxPatternLinePainterParametersTable; override;
    procedure InitializeParameters(AParameters: TdxPatternLinePainterParameters); override;
    function PixelsToUnits(AVal, ADpi: Single): Single; override;
    function UnitsToPixels(AVal, ADpi: Single): Single; override;
  end;

  { TdxUnderlinePainterHelper }

  TdxUnderlinePainterHelper = class
  public const
    HorizontalPadding = 2;
    VerticalPadding = 2;
    UnderlineThickness = 1;
    UnderlineBoxHeight = 3;
  private
    FItems: TStrings;
    FControl: TcxCustomComboBox;
    FUnderlineRepository: TdxUnderlineRepository;
    function GetItems: TStrings;
    function GetUnderlineType: TdxNullableUnderlineType;
    procedure SetUnderlineRepository(const Value: TdxUnderlineRepository);
    procedure SetUnderlineType(const Value: TdxNullableUnderlineType);
  protected
    procedure AddItem(AUnderlineType: TdxUnderlineType; const ADescription: string = '');
    class function CalculateCenteredUnderlineBounds(AUnderline: TdxUnderline; const ABounds: TRect;
      AThickness: Integer): TdxRectF; static;
    procedure DrawUnderlineItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer;
      const ARect: TRect; AState: TOwnerDrawState);
    class function GetUnderlineTypeName(AUnderlineType: TdxUnderlineType): string; static;
    class function GetUnderlineThickness(AUnderline: TdxUnderline): Integer; static;
    procedure InitItems;
    procedure PopulateItems;
  public
    constructor Create;
    destructor Destroy; override;
    class function CalcClipBounds(const ABounds: TRect): TdxRectF; static;
    class procedure DrawUnderlineItemCore(AUnderline: TdxUnderline; ACanvas: TcxCanvas; const ABounds: TRect;
      AForeColor: TdxAlphaColor; AThickness, AUnderlineBoxHeight: Integer); static;
    procedure Subscribe(AControl: TcxCustomComboBox);
    property UnderlineRepository: TdxUnderlineRepository read FUnderlineRepository write SetUnderlineRepository;
    property UnderlineType: TdxNullableUnderlineType read GetUnderlineType write SetUnderlineType;
    property Items: TStrings read GetItems;
  end;

implementation

uses
  TypInfo,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentLayout.UnitPixelsConverter,
  dxTypeHelpers;

{ TdxScreenCharacterLinePainterParameters }

function TdxScreenCharacterLinePainterParameters.PixelsToUnits(AValue: Single; ADpi: Single): Single;
begin
  Result := AValue;
end;

{ TdxScreenCharacterLinePainter }

class constructor TdxScreenCharacterLinePainter.Initialize;
begin
  FParametersTable := TdxPatternLinePainterParametersTable.Create([doOwnsValues]);
end;

function TdxScreenCharacterLinePainter.CreateParameters: TdxPatternLinePainterParameters;
begin
  Result := TdxScreenCharacterLinePainterParameters.Create;
end;

class destructor TdxScreenCharacterLinePainter.Finalize;
begin
  FParametersTable.Free;
end;

function TdxScreenCharacterLinePainter.GetParametersTable: TdxPatternLinePainterParametersTable;
begin
  Result := FParametersTable;
end;

procedure TdxScreenCharacterLinePainter.InitializeParameters(AParameters: TdxPatternLinePainterParameters);
begin
  AParameters.Initialize(PixelGraphics.DpiX);
end;

function TdxScreenCharacterLinePainter.PixelsToUnits(AVal, ADpi: Single): Single;
begin
  Result := AVal;
end;

function TdxScreenCharacterLinePainter.UnitsToPixels(AVal, ADpi: Single): Single;
begin
  Result := AVal;
end;

{ TdxUnderlineStyleComboBoxHelper }

function TdxUnderlinePainterHelper.GetItems: TStrings;
begin
  Result := FItems;
end;

function TdxUnderlinePainterHelper.GetUnderlineType: TdxNullableUnderlineType;
begin
  if FControl.ItemIndex >= 0 then
    Result := TdxUnderline(FControl.Properties.Items.Objects[FControl.ItemIndex]).Id
  else
    Result := TdxNullableUnderlineType.Null;
end;

class function TdxUnderlinePainterHelper.GetUnderlineTypeName(AUnderlineType: TdxUnderlineType): string;
var
  AInfo: PTypeInfo;
begin
  AInfo := TypeInfo(TdxUnderlineType);
  Result := GetEnumName(AInfo, Ord(AUnderlineType));
end;

class function TdxUnderlinePainterHelper.GetUnderlineThickness(AUnderline: TdxUnderline): Integer;
begin
  if AUnderline.InheritsFrom(TdxUnderlineThickSize) then
    Result := 2
  else
    Result := 1;
end;

procedure TdxUnderlinePainterHelper.AddItem(AUnderlineType: TdxUnderlineType; const ADescription: string);
begin
  if ADescription = '' then
    Items.AddObject(GetUnderlineTypeName(AUnderlineType), UnderlineRepository.GetPatternLineByType(AUnderlineType))
  else
    Items.AddObject(ADescription, UnderlineRepository.GetPatternLineByType(AUnderlineType));
end;

class function TdxUnderlinePainterHelper.CalcClipBounds(const ABounds: TRect): TdxRectF;
var
  ARect: TRect;
begin
  ARect := ABounds;
  ARect.Inflate(-HorizontalPadding, -VerticalPadding);
  Result := ARect.ToRectF;
end;

class function TdxUnderlinePainterHelper.CalculateCenteredUnderlineBounds(AUnderline: TdxUnderline;
  const ABounds: TRect; AThickness: Integer): TdxRectF;
var
  ARect: TRect;
  Ady: Single;
begin
  ARect := ABounds;
  ARect.Height := AThickness;
  Result := AUnderline.CalcLineBounds(ARect, UnderlineThickness).ToRectF;
  Ady := (ABounds.Top - Result.Top) + (ABounds.Height - Result.Height) / 2;
  Result.Offset(0, Ady);
  if Result.Height = 0 then
    Result.Height := AThickness;
end;

procedure TdxUnderlinePainterHelper.DrawUnderlineItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas;
  AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
var
  AItemStr: string;
  AItemUnderlineObj: TdxUnderline;
  ATextRect: TRect;
begin
  ACanvas.FillRect(ARect, clWindow);

  AItemUnderlineObj := AControl.Properties.Items.Objects[AIndex] as TdxUnderline;
  if AItemUnderlineObj.Id <> TdxUnderlineType.None then
    DrawUnderlineItemCore(AItemUnderlineObj, ACanvas, ARect,
      TdxAlphaColors.FromColor(clWindowText), GetUnderlineThickness(AItemUnderlineObj), UnderlineBoxHeight)
  else
  begin
    AItemStr := AControl.Properties.Items[AIndex];
    ATextRect := ARect;
    ATextRect.Inflate(-HorizontalPadding, 0);
    ACanvas.Font.Color := clWindowText;
    ACanvas.DrawText(AItemStr, ATextRect, 0);
  end;
end;

class procedure TdxUnderlinePainterHelper.DrawUnderlineItemCore(AUnderline: TdxUnderline; ACanvas: TcxCanvas;
  const ABounds: TRect; AForeColor: TdxAlphaColor; AThickness, AUnderlineBoxHeight: Integer);
var
  AGraphics: TdxGraphics;
  APainter: TdxGdiPlusPainter;
  AOldClip, AClipRect, ActualUnderlineBounds: TdxRectF;
  ALinePainter: TdxRichEditPatternLinePainter;
  AEmptyDocumentLayoutUnitConverter: TdxEmptyDocumentLayoutUnitConverter;
begin
  AGraphics := TdxGraphics.CreateFromHdc(ACanvas.Handle);
  try
    APainter := TdxGdiPlusPainter.Create(AGraphics);
    try
      AOldClip := APainter.ClipBounds;
      AClipRect := CalcClipBounds(ABounds);
      APainter.Graphics.SetClip(AClipRect);
      ActualUnderlineBounds := CalculateCenteredUnderlineBounds(AUnderline, ABounds, AThickness);
      AEmptyDocumentLayoutUnitConverter := TdxEmptyDocumentLayoutUnitConverter.Create;
      ALinePainter := TdxScreenCharacterLinePainter.Create(APainter, AEmptyDocumentLayoutUnitConverter);
      try
        AUnderline.Draw(ALinePainter, ActualUnderlineBounds, AForeColor);
      finally
        AEmptyDocumentLayoutUnitConverter.Free;
        ALinePainter.Free;
      end;
    finally
      APainter.ClipBounds := AOldClip;
      APainter.Free;
    end;
  finally
    AGraphics.Free;
  end;
end;

procedure TdxUnderlinePainterHelper.InitItems;
begin
  Items.Clear;
  Items.BeginUpdate;
  try
    PopulateItems;
  finally
    Items.EndUpdate;
  end;
end;

procedure TdxUnderlinePainterHelper.PopulateItems;
begin
  AddItem(TdxUnderlineType.None, cxGetResourceString(@sdxRichEditFontDialogUnderlineStyleNone));
  AddItem(TdxUnderlineType.Single);
  AddItem(TdxUnderlineType.ThickSingle);
  AddItem(TdxUnderlineType.Double);
  AddItem(TdxUnderlineType.Dotted);
  AddItem(TdxUnderlineType.ThickDotted);
  AddItem(TdxUnderlineType.Dashed);
  AddItem(TdxUnderlineType.ThickDashed);
  AddItem(TdxUnderlineType.LongDashed);
  AddItem(TdxUnderlineType.ThickLongDashed);
  AddItem(TdxUnderlineType.DashDotted);
  AddItem(TdxUnderlineType.ThickDashDotted);
  AddItem(TdxUnderlineType.DashDotDotted);
  AddItem(TdxUnderlineType.ThickDashDotDotted);
  AddItem(TdxUnderlineType.Wave);
  AddItem(TdxUnderlineType.HeavyWave);
  AddItem(TdxUnderlineType.DoubleWave);
end;

procedure TdxUnderlinePainterHelper.SetUnderlineRepository(const Value: TdxUnderlineRepository);
begin
  FUnderlineRepository := Value;
  if Assigned(Value) then
    InitItems;
end;

procedure TdxUnderlinePainterHelper.SetUnderlineType(const Value: TdxNullableUnderlineType);
var
  AIndex: Integer;
begin
  if Value.IsNull then
    AIndex := -1
  else
  begin
    AIndex := FControl.Properties.Items.Count - 1;
    while AIndex >= 0 do
    begin
      if TdxUnderline(FControl.Properties.Items.Objects[AIndex]).Id = Value.Value then
        Break;
      Dec(AIndex);
    end;
  end;
  FControl.ItemIndex := AIndex;
end;

constructor TdxUnderlinePainterHelper.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TdxUnderlinePainterHelper.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TdxUnderlinePainterHelper.Subscribe(AControl: TcxCustomComboBox);
begin
  FControl := AControl;
  FControl.Properties.Items.BeginUpdate;
  FControl.Properties.Items.Assign(Items);
  FControl.Properties.Items.EndUpdate;
  FControl.Properties.OnDrawItem := DrawUnderlineItem;
end;

end.
