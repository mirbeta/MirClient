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

unit  dxRichEditBorderLineWeightComboBox;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Variants, Windows, Classes, Controls, Dialogs, Forms, Graphics, Messages,
  Printers, SysUtils,
  dxCore, dxCoreClasses, cxClasses, cxContainer, cxControls, cxGraphics, cxEdit, cxImageComboBox, dxCoreGraphics,
  cxGeometry,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.PatternLine,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Platform.Win.Painter;

type

  TdxCustomBorderLineWeightComboBox = class;
  TdxCustomBorderLineWeightComboBoxProperties = class;
  TdxBorderLineWeightComboBoxItems = TcxImageComboBoxItems;

  { TdxBorderLineWeightPainter }

  TdxBorderLineWeightPainter = class
  protected
    class procedure BorderTablePainter(ABorderInfo: TdxBorderInfo; APainter: TdxPainter;
      AUnitConverter: TdxDocumentModelUnitConverter; AScaleFactor: Integer; const AProc: TProc<TdxTableBorderPainter>); static;
    class function GetTableBorderLine(ABorderLineStyle: TdxBorderLineStyle): TdxUnderline; static;
  public
    class procedure DrawBorderLineItem(ABorderInfo: TdxBorderInfo; ACanvas: TcxCanvas; R: TRect;
      AUnitConverter: TdxDocumentModelUnitConverter; AIsSelected: Boolean); overload; static;
    class procedure DrawBorderLineItem(ABorderInfo: TdxBorderInfo; ACanvas: TcxCanvas; const R: TRect;
      ABackgroundColor: TdxAlphaColor; AForegroundColor: TdxAlphaColor;
      AUnitConverter: TdxDocumentModelUnitConverter); overload; static;
    class procedure PopulateBorderLineWeight(AProperties: TdxCustomBorderLineWeightComboBoxProperties;
      ABorderStyle: TdxBorderLineStyle; ABorderColor: TdxAlphaColor; AUnitConverter: TdxDocumentModelUnitConverter;
      ABordersLineWeights: array of Single); static;
  end;

  { TdxBorderLineWeightComboBoxProperties }

  TdxCustomBorderLineWeightComboBoxProperties = class(TcxCustomImageComboBoxProperties)
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  end;

  { TdxCustomBorderLineWeightComboBox }

  TdxCustomBorderLineWeightComboBox = class(TcxCustomImageComboBox)
  private
    function GetProperties: TdxCustomBorderLineWeightComboBoxProperties;
    procedure SetProperties(const Value: TdxCustomBorderLineWeightComboBoxProperties);
    function GetActiveProperties: TdxCustomBorderLineWeightComboBoxProperties;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TdxCustomBorderLineWeightComboBoxProperties read GetActiveProperties;
    property Properties: TdxCustomBorderLineWeightComboBoxProperties read GetProperties write SetProperties;
  end;

  { TdxBorderLineWeightComboBoxProperties }

  TdxBorderLineWeightComboBoxProperties = class(TdxCustomBorderLineWeightComboBoxProperties);

  { TdxBorderLineWeightComboBox }

  TdxBorderLineWeightComboBox = class(TdxCustomBorderLineWeightComboBox)
  private
    function GetActiveProperties: TdxBorderLineWeightComboBoxProperties;
    function GetProperties: TdxBorderLineWeightComboBoxProperties;
    procedure SetProperties(Value: TdxBorderLineWeightComboBoxProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TdxBorderLineWeightComboBoxProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Properties: TdxBorderLineWeightComboBoxProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  Math,
  dxTypeHelpers,
  dxStringHelper,
  dxRichEdit.Dialogs.UnderlinePainter,
  dxRichEdit.Platform.PatternLinePainter,
  dxRichEdit.DocumentLayout.UnitPixelsConverter,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentLayout,
  dxRichEdit.Strs,
  dxRichEdit.Dialogs.Strs,
  ImgList;

{ TdxBorderLineWeightPainter }

class procedure TdxBorderLineWeightPainter.BorderTablePainter(ABorderInfo: TdxBorderInfo; APainter: TdxPainter;
  AUnitConverter: TdxDocumentModelUnitConverter; AScaleFactor: Integer; const AProc: TProc<TdxTableBorderPainter>);
var
  ABorderCalculator: TdxTableBorderCalculator;
  ACompoundArray: TArray<Single>;
  AThickness, AWidth: Integer;
  AHorizontalLinePainter, AVerticalLinePainter: TdxRichEditPatternLinePainter;
  ALayoutUnitConverter: TdxDocumentLayoutUnitPixelsConverter;
  APainterWrapper: TdxGraphicsPainterWrapper;
  ABorderPainter: TdxTableBorderPainter;
begin
  ABorderCalculator := TdxTableBorderCalculator.Create;
  try
    ACompoundArray := ABorderCalculator.GetDrawingCompoundArray(ABorderInfo.Style);
    if not Assigned(ACompoundArray) then
      ACompoundArray := ABorderCalculator.GetDrawingCompoundArray(TdxBorderLineStyle.Single);
    if not Assigned(ACompoundArray)  then
      Exit;
    AThickness := Max(1, AUnitConverter.ModelUnitsToPixels(ABorderInfo.Width * AScaleFactor, TdxCustomDocumentModel.Dpi));
    AWidth := ABorderCalculator.GetActualWidth(ABorderInfo.Style,  AThickness);
    ALayoutUnitConverter := TdxDocumentLayoutUnitPixelsConverter.Create(TdxCustomDocumentModel.Dpi);
    AHorizontalLinePainter := TdxRichEditHorizontalPatternLinePainter.Create(APainter, ALayoutUnitConverter);
    AVerticalLinePainter := TdxRichEditVerticalPatternLinePainter.Create(APainter, ALayoutUnitConverter);
    APainterWrapper := TdxGraphicsPainterWrapper.Create(APainter, AHorizontalLinePainter, AVerticalLinePainter);
    try
      if Length(ACompoundArray) = 4 then
        ABorderPainter := TdxDoubleBorderPainter.Create(APainterWrapper, ACompoundArray, AWidth)
      else
        if Length(ACompoundArray) = 6 then
          ABorderPainter := TdxTripleBorderPainter.Create(APainterWrapper, ACompoundArray, AWidth)
        else
          ABorderPainter := TdxSingleBorderPainter.Create(APainterWrapper, AWidth, GetTableBorderLine(ABorderInfo.Style));
      try
        AProc(ABorderPainter);
      finally
        ABorderPainter.Free;
      end;
    finally
      APainterWrapper.Free;
      AVerticalLinePainter.Free;
      AHorizontalLinePainter.Free;
      ALayoutUnitConverter.Free;
    end;
  finally
    ABorderCalculator.Free;
  end;
end;

class function TdxBorderLineWeightPainter.GetTableBorderLine(
  ABorderLineStyle: TdxBorderLineStyle): TdxUnderline;
begin
  if ABorderLineStyle = TdxBorderLineStyle.Single then
    Result := nil
  else
    Result := TdxBorderInfoRepository.GetUnderlineByBorderLineStyle(ABorderLineStyle);
end;

class procedure TdxBorderLineWeightPainter.DrawBorderLineItem(ABorderInfo: TdxBorderInfo; ACanvas: TcxCanvas;
  const R: TRect; ABackgroundColor: TdxAlphaColor; AForegroundColor: TdxAlphaColor;
  AUnitConverter: TdxDocumentModelUnitConverter);
var
  ABkColor: Cardinal;
  AFgColor: Cardinal;
  AOrigin: TPoint;
  APrevOrigin: TPoint;
begin
  ABkColor := SetBkColor(ACanvas.Handle, ColorToRGB(TdxAlphaColors.ToColor(ABackgroundColor)));
  AFgColor := SetTextColor(ACanvas.Handle, ColorToRGB(TdxAlphaColors.ToColor(AForegroundColor)));

  if ACanvas is TcxControlCanvas then
    AOrigin := TcxControlCanvas(ACanvas).Origin
  else
    AOrigin := TPoint.Null;

  SetBrushOrgEx(ACanvas.Handle, -AOrigin.X, -AOrigin.Y, @APrevOrigin);
  ACanvas.FillRect(R);
  DrawBorderLineItem(ABorderInfo, ACanvas, R, AUnitConverter, False);
  SetBrushOrgEx(ACanvas.Handle, APrevOrigin.X, APrevOrigin.Y, nil);

  SetBkColor(ACanvas.Handle, ABkColor);
  SetTextColor(ACanvas.Handle, AFgColor);
end;

class procedure TdxBorderLineWeightPainter.DrawBorderLineItem(ABorderInfo: TdxBorderInfo; ACanvas: TcxCanvas;
  R: TRect; AUnitConverter: TdxDocumentModelUnitConverter; AIsSelected: Boolean);
var
  AColor: TdxAlphaColor;
  AUnderline: TdxUnderline;
  AGraphics: TdxGraphics;
  APainter: TdxGdiPlusPainter;
  AThickness, AScaleFactor: Integer;
  AClipRect: TdxRectF;
  AOldClip: TdxRectF;
  AOldTransform: TdxTransformMatrix;
begin
  AColor := ABorderInfo.Color;
  if ABorderInfo.Style in [TdxBorderLineStyle.DoubleWave, TdxBorderLineStyle.Wave] then
  begin
    AUnderline := TdxBorderInfoRepository.GetUnderlineByBorderLineStyle(ABorderInfo.Style);
    AThickness := Max(1, AUnitConverter.ModelUnitsToPixels(ABorderInfo.Width, TdxCustomDocumentModel.Dpi));
    TdxUnderlinePainterHelper.DrawUnderlineItemCore(AUnderline, Acanvas, R, AColor, AThickness, AThickness);
    Exit;
  end;

  AGraphics := TdxGraphics.CreateFromHdc(ACanvas.Handle);
  try
    AOldClip := AGraphics.ClipBounds;
    AOldTransform := AGraphics.Transform;
    AScaleFactor := 12;
    APainter := TdxGdiPlusPainter.Create(AGraphics);
    try
      AClipRect := TdxUnderlinePainterHelper.CalcClipBounds(R);
      APainter.Graphics.SetClip(AClipRect);

      BorderTablePainter(ABorderInfo, APainter, AUnitConverter, AScaleFactor,
        procedure (ABorderPainter: TdxTableBorderPainter)
        begin
          AGraphics.TranslateWorldTransform(0, (R.Top + R.Bottom) / 2.0);
          AGraphics.ScaleWorldTransform(1, 1 / AScaleFactor);
          ABorderPainter.DrawHorizontalBorder(AColor, TdxPointF.Create(R.Left, -ABorderPainter.Width / 2.0), R.Width);
        end);
    finally
      APainter.Free;
    end;
    AGraphics.Transform := AOldTransform;
    AGraphics.SetClip(AOldClip);
  finally
    AGraphics.Free;
  end;
end;

class procedure TdxBorderLineWeightPainter.PopulateBorderLineWeight(
  AProperties: TdxCustomBorderLineWeightComboBoxProperties; ABorderStyle: TdxBorderLineStyle;
  ABorderColor: TdxAlphaColor; AUnitConverter: TdxDocumentModelUnitConverter; ABordersLineWeights: array of Single);
var
  AWeight: Single;
  I, AValue: Integer;
  ABitmap: TcxBitmap;
  AMeasurement: string;
  AItem: TcxImageComboBoxItem;
  ABorderInfo: TdxBorderInfo;
  AImageList: TCustomImageList;
  AItems: TdxBorderLineWeightComboBoxItems;
begin
  AItems := AProperties.Items;
  AImageList := AProperties.Images;
  ABitmap := TcxBitmap.CreateSize(AImageList.Width, AImageList.Height);
  try
    AImageList.Clear;
    AMeasurement := cxGetResourceString(@sdxRichEditUnitsPoints);
    ABorderInfo := TdxBorderInfo.Create;
    try
      ABorderInfo.Style := ABorderStyle;//Border.Style;
      ABorderInfo.Color := ABorderColor; //Border.Color;
      AItems.BeginUpdate;
      try
        AItems.Clear;
        for I := Low(ABordersLineWeights) to High(ABordersLineWeights) do
        begin
          AWeight := AUnitConverter.PointsToModelUnitsF(ABordersLineWeights[I]);
          AValue := Max(1, Round(AWeight));
          ABorderInfo.Width := AValue;
          cxPaintCanvas.BeginPaint(ABitmap.Canvas);
          DrawBorderLineItem(ABorderInfo, cxPaintCanvas, ABitmap.ClientRect, clWhite, clBlack, AUnitConverter);
          cxPaintCanvas.EndPaint;
          AItem := AItems.Add;
          AItem.ImageIndex := AImageList.Add(ABitmap, nil);
          AItem.Value := AValue;
          AItem.Description := TdxStringHelper.Format('%g %s', [ABordersLineWeights[I], AMeasurement]);
        end;
      finally
        AItems.EndUpdate;
      end;
    finally
      ABorderInfo.Free;
    end;
  finally
    ABitmap.Free;
  end;
end;

{ TdxCustomBorderLineWeightComboBoxProperties }

constructor TdxCustomBorderLineWeightComboBoxProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  Images := TcxImageList.Create(nil);
  Images.Width := 48;
  ImageAlign := iaRight;
end;

destructor TdxCustomBorderLineWeightComboBoxProperties.Destroy;
begin
  Images.Free;
  inherited Destroy;
end;

procedure TdxCustomBorderLineWeightComboBoxProperties.DoAssign(AProperties: TcxCustomEditProperties);
var
  AImages: TCustomImageList;
  ACustomImageComboBoxProperties: TcxCustomImageComboBoxProperties;
begin
  AImages := nil;
  if AProperties is TcxCustomImageComboBoxProperties then
  begin
    ACustomImageComboBoxProperties := TcxCustomImageComboBoxProperties(AProperties);
    AImages := ACustomImageComboBoxProperties.Images;
  end
  else
    ACustomImageComboBoxProperties := nil;

  inherited DoAssign(AProperties);

  if ACustomImageComboBoxProperties <> nil then
    if AProperties is TdxCustomBorderLineWeightComboBoxProperties then
      ACustomImageComboBoxProperties.Images.Assign(AImages)
    else
      ACustomImageComboBoxProperties.Images := AImages;
end;

{ TdxCustomBorderLineWeightComboBox }

class function TdxCustomBorderLineWeightComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomBorderLineWeightComboBoxProperties;
end;

function TdxCustomBorderLineWeightComboBox.GetActiveProperties: TdxCustomBorderLineWeightComboBoxProperties;
begin
  Result := TdxCustomBorderLineWeightComboBoxProperties(InternalGetActiveProperties);
end;

function TdxCustomBorderLineWeightComboBox.GetProperties: TdxCustomBorderLineWeightComboBoxProperties;
begin
  Result := TdxCustomBorderLineWeightComboBoxProperties(inherited Properties);
end;

procedure TdxCustomBorderLineWeightComboBox.SetProperties(const Value: TdxCustomBorderLineWeightComboBoxProperties);
begin
  Properties.Assign(Value);
end;

{ TdxBorderLineWeightComboBox }

function TdxBorderLineWeightComboBox.GetActiveProperties: TdxBorderLineWeightComboBoxProperties;
begin
  Result := TdxBorderLineWeightComboBoxProperties(InternalGetActiveProperties);
end;

function TdxBorderLineWeightComboBox.GetProperties: TdxBorderLineWeightComboBoxProperties;
begin
  Result := TdxBorderLineWeightComboBoxProperties(inherited Properties);
end;

class function TdxBorderLineWeightComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxBorderLineWeightComboBoxProperties;
end;

procedure TdxBorderLineWeightComboBox.SetProperties(Value: TdxBorderLineWeightComboBoxProperties);
begin
  Properties.Assign(Value);
end;


end.
