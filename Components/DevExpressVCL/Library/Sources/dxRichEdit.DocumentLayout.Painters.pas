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

unit dxRichEdit.DocumentLayout.Painters;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Windows, Messages, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections, Controls, StdCtrls, Forms,
  dxCore, dxCoreClasses, dxCoreGraphics, cxGeometry, cxLookAndFeels, cxGraphics, cxControls, cxClasses,
  dxMessages, dxGDIPlusClasses,

  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Control.HotZones,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentLayout.CommentPadding;

type
  TdxCustomMarkVisualInfo = class;

  TdxRichEditHoverPainters = class(TObject);

  { TdxCustomMarkVisualInfo }

  TdxCustomMarkVisualInfo = class
  private
    FBounds: TRect;
    FCustomMark: TdxCustomMark;
    function GetUserData: TObject;
  protected
    property CustomMark: TdxCustomMark read FCustomMark;
  public
    constructor Create(ACustomMark: TdxCustomMark; const ABounds: TRect);

    property UserData: TObject read GetUserData;
    property Bounds: TRect read FBounds write FBounds;
  end;
  TdxCustomMarkVisualInfoCollection = class(TdxList<TdxCustomMarkVisualInfo>);

  IdxCustomMarkExporter = interface
  ['{49117034-859E-435A-B1AA-60B5DDBF60D8}']
    function GetCustomMarkVisualInfoCollection: TdxCustomMarkVisualInfoCollection;
    procedure ExportCustomMarkBox(ACustomMark: TdxCustomMark; const ABounds: TRect);
    property CustomMarkVisualInfoCollection: TdxCustomMarkVisualInfoCollection read GetCustomMarkVisualInfoCollection;
  end;

  { TdxCustomMarkExporter }

  TdxCustomMarkExporter = class(TcxIUnknownObject, IdxCustomMarkExporter)
  private
    FVisualInfoCollection: TdxCustomMarkVisualInfoCollection;
    function GetCustomMarkVisualInfoCollection: TdxCustomMarkVisualInfoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExportCustomMarkBox(ACustomMark: TdxCustomMark; const ABounds: TRect); virtual;

    property CustomMarkVisualInfoCollection: TdxCustomMarkVisualInfoCollection read GetCustomMarkVisualInfoCollection;
  end;

  { TdxOfficeSelectionPainter }

  TdxOfficeSelectionPainter = class abstract (TcxIUnknownObject)
  strict private
    class var
      FHotZoneGradientColor: TdxAlphaColor;
      FHotZoneRotationGradientColor: TdxAlphaColor;
      FHotZoneBorderColor: TdxAlphaColor;
      FShapeBorderColor: TdxAlphaColor;
      FShapeBorderWidth: TdxAlphaColor;
  strict private
    FGraphics: TdxGraphics;
    FTransformsStack: TdxObjectStack<TdxGpMatrix>;
    class constructor Initialize;
  public
    constructor Create(AGraphics: TdxGraphics {const ACache: IdxGraphicsCache});
    destructor Destroy; override;
    function TryPushRotationTransform(const ACenter: TPoint; AAngleInDegrees: Single): Boolean;
    procedure PushRotationTransform(const ACenter: TPoint; AAngleInDegrees: Single);
    procedure PopTransform;

    procedure DrawRectangularHotZone(const ABounds: TRect; AColor: TdxAlphaColor); virtual;
    procedure DrawEllipticHotZone(const ABounds: TRect; AColor: TdxAlphaColor); virtual;
    function CreateFillBrush(const ABounds: TRect; AColor: TdxAlphaColor): TdxGPCustomBrush; virtual;
    procedure DrawInHighQuality(const ADraw: TdxAction<TdxGraphics>);
    procedure FillInHighQuality(const AFill: TdxAction<TdxGraphics>);
    procedure FillRectangle(const ABounds: TRect); virtual; abstract;
    procedure DrawRectangle(const ABounds: TRect); virtual; abstract;
    procedure DrawLine(const AFrom: TPoint; const ATo: TPoint); virtual; abstract;

    property Graphics: TdxGraphics read FGraphics;
    class property HotZoneGradientColor: TdxAlphaColor read FHotZoneGradientColor;
    class property HotZoneRotationGradientColor: TdxAlphaColor read FHotZoneRotationGradientColor;
    class property HotZoneBorderColor: TdxAlphaColor read FHotZoneBorderColor;
    class property ShapeBorderColor: TdxAlphaColor read FShapeBorderColor;
    class property ShapeBorderWidth: TdxAlphaColor read FShapeBorderWidth;
  end;

  { TdxRichEditSelectionPainter }

  TdxRichEditSelectionPainter = class abstract(TdxOfficeSelectionPainter,
    IdxSelectionPainter,
    IdxHotZoneVisitor,
    IdxRectangularObjectHotZoneVisitor)
  strict private
    class var
      FSelectionColor: TdxAlphaColor;
      FAnchorImage: TdxSmartImage;
    function GetAnchorImage: TdxSmartImage;
  strict private
    class function LoadAnchorImage: TdxSmartImage; static;
    class constructor Initialize;
  protected
    class procedure FinalizeAnchorImage; static;
  protected
    procedure DrawHotZone(const AHotZone: IdxHotZone); virtual;
    procedure Visit(AHotZone: TdxRectangularObjectBottomRightHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectBottomMiddleHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectBottomLeftHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectMiddleRightHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectMiddleLeftHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectTopRightHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectTopMiddleHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectTopLeftHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectRotationHotZone); overload;

    property AnchorImage: TdxSmartImage read GetAnchorImage;
  public
    procedure Draw(AViewInfo: TdxRowSelectionLayoutBase); overload; virtual;
    procedure Draw(AViewInfo: TdxRectangularObjectSelectionLayout); overload; virtual;
    procedure Draw(AViewInfo: TdxFloatingObjectAnchorSelectionLayout); overload; virtual;

    class property SelectionColor: TdxAlphaColor read FSelectionColor;
  end;

  { TdxSemitransparentSelectionPainter }

  TdxSemitransparentSelectionPainter = class(TdxRichEditSelectionPainter)
  public
    procedure FillRectangle(const ABounds: TRect); override;
    procedure DrawRectangle(const ABounds: TRect); override;
    procedure DrawLine(const AFrom: TPoint; const ATo: TPoint); override;
  end;

  { TdxRectangularObjectSelectionPainter }

  TdxRectangularObjectSelectionPainter = class(TdxRichEditSelectionPainter)
  public
    procedure DrawLine(const AFrom: TPoint; const ATo: TPoint); override;
    procedure FillRectangle(const ABounds: TRect); override;
    procedure DrawRectangle(const ABounds: TRect); override;
  end;

implementation

{$R 'dxRichEdit.DocumentModel.Images.RES'}

uses
  Contnrs, Math, dxTypeHelpers, dxGDIPlusAPI,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentLayout.UnitConverter;

{ TdxCustomMarkVisualInfo }

constructor TdxCustomMarkVisualInfo.Create(ACustomMark: TdxCustomMark; const ABounds: TRect);
begin
  inherited Create;
  Assert(ACustomMark is TdxCustomMark);
  FBounds := ABounds;
  FCustomMark := ACustomMark;
end;

function TdxCustomMarkVisualInfo.GetUserData: TObject;
begin
  Result := CustomMark.UserData;
end;

{ TdxCustomMarkExporter }

constructor TdxCustomMarkExporter.Create;
begin
  inherited Create;
  FVisualInfoCollection := TdxCustomMarkVisualInfoCollection.Create;
end;

destructor TdxCustomMarkExporter.Destroy;
begin
  FreeAndNil(FVisualInfoCollection);
  inherited Destroy;
end;

procedure TdxCustomMarkExporter.ExportCustomMarkBox(ACustomMark: TdxCustomMark; const ABounds: TRect);
begin
  FVisualInfoCollection.Add(TdxCustomMarkVisualInfo.Create(ACustomMark, ABounds));
end;

function TdxCustomMarkExporter.GetCustomMarkVisualInfoCollection: TdxCustomMarkVisualInfoCollection;
begin
  Result := FVisualInfoCollection;
end;

{ TdxOfficeSelectionPainter }

constructor TdxOfficeSelectionPainter.Create(AGraphics: TdxGraphics {const ACache: IdxGraphicsCache});
begin
  inherited Create;
  FTransformsStack := TdxObjectStack<TdxGpMatrix>.Create(True);
  Assert(AGraphics is TdxGraphics);
  FGraphics := AGraphics;
end;

destructor TdxOfficeSelectionPainter.Destroy;
begin
  FTransformsStack.Free;
  inherited Destroy;
end;

class constructor TdxOfficeSelectionPainter.Initialize;
begin
  FHotZoneGradientColor := dxMakeAlphaColor(255, 202, 234, 237);
  FHotZoneRotationGradientColor := dxMakeAlphaColor(255, 136, 228, 58);
  FHotZoneBorderColor := dxMakeAlphaColor(clGray);
  FShapeBorderColor := dxMakeAlphaColor(255, 90, 147, 211);
  FShapeBorderWidth := 1;
end;

function TdxOfficeSelectionPainter.TryPushRotationTransform(const ACenter: TPoint; AAngleInDegrees: Single): Boolean;
begin
  if IsZero(Single(dxFMod(AAngleInDegrees, 360))) then
    Exit(False);
  PushRotationTransform(ACenter, AAngleInDegrees);
  Result := True;
end;

procedure TdxOfficeSelectionPainter.PushRotationTransform(const ACenter: TPoint; AAngleInDegrees: Single);
var
  ATransform: TdxGpMatrix;
begin
  Graphics.SaveWorldTransform;
  ATransform := Graphics.Transform;
  try
    ATransform.Rotate(AAngleInDegrees, ACenter.ToPointF);
    Graphics.Transform := ATransform;
  finally
    ATransform.Free;
  end;
end;

procedure TdxOfficeSelectionPainter.PopTransform;
begin
  Graphics.RestoreWorldTransform;
end;

procedure TdxOfficeSelectionPainter.DrawRectangularHotZone(const ABounds: TRect; AColor: TdxAlphaColor);
var
  R: TRect;
begin
  R := ABounds;
  FillInHighQuality(
    procedure (const AGraphics: TdxGraphics)
    var
      ABrush: TdxGPCustomBrush;
    begin
      ABrush := CreateFillBrush(R, AColor);
      try
        AGraphics.FillRectangle(R, ABrush);
        AGraphics.Rectangle(R, HotZoneBorderColor, dxacNone);
      finally
        ABrush.Free;
      end;
    end);
end;

procedure TdxOfficeSelectionPainter.DrawEllipticHotZone(const ABounds: TRect; AColor: TdxAlphaColor);
var
  R: TRect;
begin
  R := ABounds;
  FillInHighQuality(
    procedure(const AGraphics: TdxGraphics)
    var
      ABrush: TdxGPCustomBrush;
    begin
      ABrush := CreateFillBrush(R, AColor);
      try
        AGraphics.Ellipse(R, nil, ABrush);
        AGraphics.Ellipse(R, HotZoneBorderColor, dxacNone);
      finally
        ABrush.Free;
      end;
    end);
end;

function TdxOfficeSelectionPainter.CreateFillBrush(const ABounds: TRect; AColor: TdxAlphaColor): TdxGPCustomBrush;
var
  ABrush: TdxLinearGradientBrush;
begin
  ABrush := TdxLinearGradientBrush.Create(ABounds, $FFFFFFFF, AColor, LinearGradientModeVertical);
  ABrush.SetSigmaBellShape(0.5, 1.0);
  Result := ABrush;
end;

procedure TdxOfficeSelectionPainter.DrawInHighQuality(const ADraw: TdxAction<TdxGraphics>);
var
  AOldSmoothingMode: TdxGPSmoothingMode;
begin
  AOldSmoothingMode := Graphics.SmoothingMode;
  try
    Graphics.SmoothingMode := TdxGPSmoothingMode.smHighQuality;
    ADraw(Graphics);
  finally
    Graphics.SmoothingMode := AOldSmoothingMode;
  end;
end;

procedure TdxOfficeSelectionPainter.FillInHighQuality(const AFill: TdxAction<TdxGraphics>);
var
  AOldSmoothingMode: TdxGPSmoothingMode;
  AOldPixelOffsetMode: TdxGpPixelOffsetMode;
begin
  AOldSmoothingMode := Graphics.SmoothingMode;
  AOldPixelOffsetMode := Graphics.PixelOffsetMode;
  try
    Graphics.SmoothingMode := smHighQuality;
    Graphics.PixelOffsetMode := PixelOffsetModeHighQuality;
    AFill(Graphics);
  finally
    Graphics.SmoothingMode := AOldSmoothingMode;
    Graphics.PixelOffsetMode := AOldPixelOffsetMode;
  end;
end;

{ TdxRichEditSelectionPainter }

class constructor TdxRichEditSelectionPainter.Initialize;
begin
  FSelectionColor := TdxAlphaColors.FromArgb(127, 80, 151, 227);
end;

class procedure TdxRichEditSelectionPainter.FinalizeAnchorImage;
begin
  FAnchorImage.Free;
end;

function TdxRichEditSelectionPainter.GetAnchorImage: TdxSmartImage;
begin
  if FAnchorImage = nil then
    FAnchorImage := LoadAnchorImage;
  Result := FAnchorImage;
end;

class function TdxRichEditSelectionPainter.LoadAnchorImage: TdxSmartImage;
var
  AStream: TResourceStream;
begin
  AStream := TResourceStream.Create(HInstance, 'DXRECANCHOR', RT_RCDATA);
  try
    Result := TdxSmartImage.CreateFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxRichEditSelectionPainter.Draw(AViewInfo: TdxRowSelectionLayoutBase);
begin
  FillRectangle(AViewInfo.Bounds);
end;

procedure TdxRichEditSelectionPainter.Draw(AViewInfo: TdxRectangularObjectSelectionLayout);
var
  ABounds: TRect;
  ACenter: TPoint;
  ATransformApplied: Boolean;
  AView: TdxRichEditView;
  AHotZones: TdxHotZoneCollection;
  AHotZone: TdxHotZone;
  I: Integer;
begin
  ABounds := AViewInfo.Box.ActualSizeBounds;
  ACenter := ABounds.CenterPoint;
  AView := TdxRichEditView(AViewInfo.View);
  ATransformApplied := TryPushRotationTransform(ACenter, AView.DocumentModel.GetBoxEffectiveRotationAngleInDegrees(AViewInfo.Box));
  try
    if AViewInfo.Resizeable then
      DrawRectangle(ABounds)
    else
      FillRectangle(ABounds);
    AHotZones := AView.SelectionLayout.LastDocumentSelectionLayout.HotZones;
    for I := 0 to AHotZones.Count - 1 do
    begin
      AHotZone := AHotZones[I];
      DrawHotZone(AHotZone);
    end;
  finally
    if ATransformApplied then
      PopTransform;
  end;
end;

procedure TdxRichEditSelectionPainter.Draw(AViewInfo: TdxFloatingObjectAnchorSelectionLayout);
var
  AHDC: HDC;
  ALocation, APixelPoint: TPoint;
  AUnitConverter: TdxDocumentLayoutUnitConverter;
  ABounds: TRect;
begin
  ALocation := AViewInfo.AnchorBounds.Location;
  AUnitConverter := TdxRichEditView(AViewInfo.View).DocumentLayout.UnitConverter;
  APixelPoint := AUnitConverter.LayoutUnitsToPixels(ALocation, TdxDocumentModel.DpiX, TdxDocumentModel.DpiY);
  ABounds.InitSize(
    AUnitConverter.PixelsToLayoutUnits(APixelPoint.X, TdxDocumentModel.DpiX),
    AUnitConverter.PixelsToLayoutUnits(APixelPoint.Y, TdxDocumentModel.DpiY),
    AUnitConverter.PixelsToLayoutUnits(AnchorImage.Width, TdxDocumentModel.DpiX),
    AUnitConverter.PixelsToLayoutUnits(AnchorImage.Height, TdxDocumentModel.DpiY));
  ABounds.X := ABounds.X - ABounds.Width;

  AHDC := Graphics.GetHDC;
  try
    AnchorImage.StretchDraw(AHDC, ABounds);
  finally
    Graphics.ReleaseHDC(AHDC);
  end;
end;

procedure TdxRichEditSelectionPainter.DrawHotZone(const AHotZone: IdxHotZone);
begin
  AHotZone.Accept(Self);
end;

procedure TdxRichEditSelectionPainter.Visit(AHotZone: TdxRectangularObjectBottomRightHotZone);
begin
  DrawEllipticHotZone(AHotZone.Bounds, HotZoneGradientColor);
end;

procedure TdxRichEditSelectionPainter.Visit(AHotZone: TdxRectangularObjectBottomMiddleHotZone);
begin
  DrawRectangularHotZone(AHotZone.Bounds, HotZoneGradientColor);
end;

procedure TdxRichEditSelectionPainter.Visit(AHotZone: TdxRectangularObjectBottomLeftHotZone);
begin
  DrawEllipticHotZone(AHotZone.Bounds, HotZoneGradientColor);
end;

procedure TdxRichEditSelectionPainter.Visit(AHotZone: TdxRectangularObjectMiddleRightHotZone);
begin
  DrawRectangularHotZone(AHotZone.Bounds, HotZoneGradientColor);
end;

procedure TdxRichEditSelectionPainter.Visit(AHotZone: TdxRectangularObjectMiddleLeftHotZone);
begin
  DrawRectangularHotZone(AHotZone.Bounds, HotZoneGradientColor);
end;

procedure TdxRichEditSelectionPainter.Visit(AHotZone: TdxRectangularObjectTopRightHotZone);
begin
  DrawEllipticHotZone(AHotZone.Bounds, HotZoneGradientColor);
end;

procedure TdxRichEditSelectionPainter.Visit(AHotZone: TdxRectangularObjectTopMiddleHotZone);
begin
  DrawRectangularHotZone(AHotZone.Bounds, HotZoneGradientColor);
end;

procedure TdxRichEditSelectionPainter.Visit(AHotZone: TdxRectangularObjectTopLeftHotZone);
begin
  DrawEllipticHotZone(AHotZone.Bounds, HotZoneGradientColor);
end;

procedure TdxRichEditSelectionPainter.Visit(AHotZone: TdxRectangularObjectRotationHotZone);
begin
  DrawLine(AHotZone.Bounds.CenterPoint, AHotZone.LineEnd);
  DrawEllipticHotZone(AHotZone.Bounds, HotZoneRotationGradientColor);
end;

{ TdxSemitransparentSelectionPainter }

procedure TdxSemitransparentSelectionPainter.FillRectangle(const ABounds: TRect);
begin
  Graphics.FillRectangle(ABounds, SelectionColor);
end;

procedure TdxSemitransparentSelectionPainter.DrawRectangle(const ABounds: TRect);
begin
  Graphics.Rectangle(ABounds, SelectionColor, dxacNone);
end;

procedure TdxSemitransparentSelectionPainter.DrawLine(const AFrom: TPoint; const ATo: TPoint);
begin
  Graphics.Line(AFrom.X, AFrom.Y, ATo.X, ATo.Y, SelectionColor);
end;

{ TdxRectangularObjectSelectionPainter }

procedure TdxRectangularObjectSelectionPainter.DrawLine(const AFrom: TPoint; const ATo: TPoint);
begin
  Graphics.Line(AFrom.X, AFrom.Y, ATo.X, ATo.Y, ShapeBorderColor, ShapeBorderWidth);
end;

procedure TdxRectangularObjectSelectionPainter.FillRectangle(const ABounds: TRect);
begin
  Graphics.FillRectangle(ABounds, SelectionColor);
end;

procedure TdxRectangularObjectSelectionPainter.DrawRectangle(const ABounds: TRect);
begin
  Graphics.Rectangle(ABounds, ShapeBorderColor, dxacNone, ShapeBorderWidth);
end;

procedure Finalize;
begin
  TdxRichEditSelectionPainter.FinalizeAnchorImage;
end;

initialization
  dxUnitsLoader.AddUnit(nil, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.
