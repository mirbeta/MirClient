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

unit dxRichEdit.Platform.Win.Strategies;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Controls, Graphics, Generics.Defaults, Generics.Collections, dxCoreGraphics, dxGDIPlusClasses,

  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Commands.Selection,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.Keyboard,
  dxRichEdit.Control.HotZones,
  dxRichEdit.Control.Mouse,
  dxRichEdit.Control.Mouse.DragAndDrop,
  dxRichEdit.Control.Mouse.Resize,
  dxRichEdit.Control.Core,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.Core;

type
  { TdxVCLResizeTableRowMouseHandlerStateStrategy }

  TdxVCLResizeTableRowMouseHandlerStateStrategy = class(TdxResizeTableRowMouseHandlerStateStrategy)
  private
    function GetControl: TdxRichEditControlBase;
  protected
    procedure DrawReversibleLineCore(Y: Integer); override;
    procedure BeginVisualFeedback; override;
    procedure ShowVisualFeedback; override;
    procedure EndVisualFeedback; override;

    property Control: TdxRichEditControlBase read GetControl;
  end;

  { TdxVCLResizeTableVirtualColumnMouseHandlerStateStrategy }

  TdxVCLResizeTableVirtualColumnMouseHandlerStateStrategy = class(TdxResizeTableVirtualColumnMouseHandlerStateStrategy)
  strict private
    function GetControl: TdxRichEditControlBase;
  protected
    procedure DrawReversibleLineCore(X: Integer); override;
    procedure BeginVisualFeedback; override;
    procedure ShowVisualFeedback; override;
    procedure HideVisualFeedback; override;
    procedure EndVisualFeedback; override;

    property Control: TdxRichEditControlBase read GetControl;
  end;

  { TdxShadowedFloatingObjectPainter }

  TdxShadowedFloatingObjectPainter = class
  strict private
    FUnitConverter: TdxDocumentLayoutUnitConverter;
    FImage: TdxOfficeImage;
    FTransform: TdxTransformMatrix;
    FBounds: TRect;
    FInitialShapeBounds: TRect;
    FInitialContentBounds: TRect;
    FFillColor: TdxAlphaColor;
    FOutlineColor: TdxAlphaColor;
    FAlpha: Single;
    FSizeMode: TdxImageSizeMode;
    FShadowDisplayMode: TdxResizingShadowDisplayMode;
  public
    constructor Create(AUnitConverter: TdxDocumentLayoutUnitConverter);
    procedure Paint(AGraphics: TdxGraphics);
    procedure PaintCore(AGraphics: TdxGraphics);
    function ShouldDrawImage: Boolean;
    procedure DrawImage(AGraphics: TdxGraphics; const ABounds: TRect);
    procedure DrawFeedbackShape(AGraphics: TdxGraphics; const AShapeBounds: TRect;
      const AContentBounds: TRect; AFillColor: TdxAlphaColor; AOutlineColor: TdxAlphaColor;
      AUnitConverter: TdxDocumentLayoutUnitConverter);

    property Image: TdxOfficeImage read FImage write FImage;
    property SizeMode: TdxImageSizeMode read FSizeMode write FSizeMode;
    property ShadowDisplayMode: TdxResizingShadowDisplayMode read FShadowDisplayMode write FShadowDisplayMode;
    property Transform: TdxTransformMatrix read FTransform write FTransform;
    property Bounds: TRect read FBounds write FBounds;
    property InitialShapeBounds: TRect read FInitialShapeBounds write FInitialShapeBounds;
    property InitialContentBounds: TRect read FInitialContentBounds write FInitialContentBounds;
    property FillColor: TdxAlphaColor read FFillColor write FFillColor;
    property OutlineColor: TdxAlphaColor read FOutlineColor write FOutlineColor;
    property Alpha: Single read FAlpha write FAlpha;
  end;

  { TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy }

  TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy = class(TdxDragFloatingObjectManuallyMouseHandlerStateStrategy)
  public const
    Alpha = 0.6;
  strict private
    FPainter: TdxShadowedFloatingObjectPainter;
    function GetWinControl: TdxRichEditControlBase;
  protected
    procedure ShowVisualFeedbackCore(const ABounds: TRect; APageViewInfo: TdxPageViewInfo; AImage: TdxOfficeImage); override;
    procedure HideVisualFeedbackCore(const ABounds: TRect; APageViewInfo: TdxPageViewInfo); override;
    function CreateFeedbackImage(AOriginalImage: TdxOfficeImage): TdxOfficeImage; override;
    function CreateSemitransparentFeedbackImage(ABitmap: TdxSmartImage; AAlpha: Single): TdxOfficeImage; virtual;
    procedure BeginVisualFeedback; override;
    procedure EndVisualFeedback; override;
  public
    property WinControl: TdxRichEditControlBase read GetWinControl;
  end;

  { TdxVCLRectangularObjectResizeMouseHandlerStateStrategy }

  TdxVCLRectangularObjectResizeMouseHandlerStateStrategy = class(TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy)
  private const
    Alpha = 0.6;
  strict private
    FPainter: TdxShadowedFloatingObjectPainter;
    FUseReversibleFrame: Boolean;
    function GetWinControl: TdxRichEditControlBase;
  protected
    procedure BeginVisualFeedback; override;
    function TryFloatingObjectVisualFeedback: Boolean;
    function TryInlinePictureVisualFeedback: Boolean;
    procedure ShowVisualFeedback; override;
    procedure HideVisualFeedback; override;
    procedure EndVisualFeedback; override;
    procedure DrawReversibleFrameCore;
    procedure DrawVisualFeedback;
  public
    destructor Destroy; override;
    property WinControl: TdxRichEditControlBase read GetWinControl;
  end;

implementation

uses
  Windows, Math, cxGraphics,
  dxGDIPlusAPI, cxGeometry, dxTypeHelpers,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Boxes;

{ TdxVCLResizeTableRowMouseHandlerStateStrategy }

procedure TdxVCLResizeTableRowMouseHandlerStateStrategy.BeginVisualFeedback;
begin
  State.DrawReversibleLineCore;
end;

procedure TdxVCLResizeTableRowMouseHandlerStateStrategy.DrawReversibleLineCore(
  Y: Integer);
begin
  Control.Painter.DrawReversibleHorizontalLine(y, PageViewInfo);
end;

procedure TdxVCLResizeTableRowMouseHandlerStateStrategy.EndVisualFeedback;
begin
end;

function TdxVCLResizeTableRowMouseHandlerStateStrategy.GetControl: TdxRichEditControlBase;
begin
  Result := TdxRichEditControlBase(Controller.Control.Control);
end;

procedure TdxVCLResizeTableRowMouseHandlerStateStrategy.ShowVisualFeedback;
begin
  State.DrawReversibleLineCore;
end;

{ TdxVCLResizeTableVirtualColumnMouseHandlerStateStrategy }

procedure TdxVCLResizeTableVirtualColumnMouseHandlerStateStrategy.BeginVisualFeedback;
begin
  State.DrawReversibleLineCore;
end;

procedure TdxVCLResizeTableVirtualColumnMouseHandlerStateStrategy.DrawReversibleLineCore(X: Integer);
begin
  Control.Painter.DrawReversibleVerticalLine(X, PageViewInfo);
end;

procedure TdxVCLResizeTableVirtualColumnMouseHandlerStateStrategy.EndVisualFeedback;
begin
end;

function TdxVCLResizeTableVirtualColumnMouseHandlerStateStrategy.GetControl: TdxRichEditControlBase;
begin
  Result := TdxRichEditControlBase(Controller.Control.Control);
end;

procedure TdxVCLResizeTableVirtualColumnMouseHandlerStateStrategy.HideVisualFeedback;
begin
  State.DrawReversibleLineCore;
end;

procedure TdxVCLResizeTableVirtualColumnMouseHandlerStateStrategy.ShowVisualFeedback;
begin
  State.DrawReversibleLineCore;
end;

{ TdxShadowedFloatingObjectPainter }

constructor TdxShadowedFloatingObjectPainter.Create(AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  inherited Create;
  FAlpha := 1.0;
  FSizeMode := TdxImageSizeMode.StretchImage;
  FShadowDisplayMode := TdxResizingShadowDisplayMode.Content;
  FUnitConverter := AUnitConverter;
end;

procedure TdxShadowedFloatingObjectPainter.Paint(AGraphics: TdxGraphics);
var
  AOriginalTransform, ANewTransform: TdxTransformMatrix;
  AOriginalSmoothingMode: TdxGPSmoothingMode;
begin
  AOriginalTransform := nil;
  AOriginalSmoothingMode := smDefault;
  if FTransform <> nil then
  begin
    AOriginalTransform := AGraphics.Transform;
    ANewTransform := AGraphics.Transform;
    ANewTransform.Multiply(FTransform);
    AGraphics.Transform := ANewTransform;
    AOriginalSmoothingMode := AGraphics.SmoothingMode;
    AGraphics.SmoothingMode := smHighQuality;
  end;
  try
    PaintCore(AGraphics);
  finally
    if FTransform <> nil then
    begin
      AGraphics.Transform := AOriginalTransform;
      AGraphics.SmoothingMode := AOriginalSmoothingMode;
    end;
  end;
end;

procedure TdxShadowedFloatingObjectPainter.PaintCore(AGraphics: TdxGraphics);
var
  AContentBounds: TRect;
  P: TPoint;
begin
  AContentBounds := FInitialContentBounds;
  P.X := AContentBounds.Left + FBounds.Left - FInitialShapeBounds.Left;
  P.Y := AContentBounds.Top + FBounds.Top - FInitialShapeBounds.Top;
  AContentBounds.Location := P;
  AContentBounds.Width := AContentBounds.Width + FBounds.Width - FInitialShapeBounds.Width;
  AContentBounds.Height := AContentBounds.Height + FBounds.Height - FInitialShapeBounds.Height;
  DrawFeedbackShape(AGraphics, FBounds, AContentBounds, FillColor, OutlineColor, FUnitConverter);

  if (FImage <> nil) and ShouldDrawImage then
    DrawImage(AGraphics, AContentBounds);

  AGraphics.Rectangle(FBounds, TdxAlphaColors.FromArgb($80, 0, 0, 0), TdxAlphaColors.Empty, 1);
end;

function TdxShadowedFloatingObjectPainter.ShouldDrawImage: Boolean;
begin
  Result := ShadowDisplayMode = TdxResizingShadowDisplayMode.Content;
end;

procedure TdxShadowedFloatingObjectPainter.DrawImage(AGraphics: TdxGraphics; const ABounds: TRect);
var
  AMatrix: TdxGPColorMatrix;
  AAttributes: TdxGPImageAttributes;
  AImgActualSize: TSize;
  AImgRect: TRect;
begin
  if FAlpha = 1.0 then
  begin
    AGraphics.Draw(FImage, ABounds);
    Exit;
  end;
  AMatrix := dxGpDefaultColorMatrix;
  AMatrix[3, 3] := FAlpha;
  AAttributes := TdxGPImageAttributes.Create;
  try
    AAttributes.SetColorMatrix(@AMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);

    AImgActualSize := FImage.Size;
    AImgRect := cxRect(TdxImageTool.CalculateImageRectCore(ABounds, AImgActualSize, SizeMode));
    AGraphics.SaveClipRegion;
    try
      AGraphics.SetClipRect(ABounds, gmReplace);

      AGraphics.Draw(FImage, AImgRect, AAttributes);
    finally
      AGraphics.RestoreClipRegion;
    end;
  finally
    AAttributes.Free;
  end;
end;

procedure TdxShadowedFloatingObjectPainter.DrawFeedbackShape(AGraphics: TdxGraphics;
  const AShapeBounds: TRect; const AContentBounds: TRect;
  AFillColor: TdxAlphaColor; AOutlineColor: TdxAlphaColor; AUnitConverter: TdxDocumentLayoutUnitConverter);
var
  ABounds: TRect;
begin
  if not TdxAlphaColors.IsTransparentOrEmpty(AFillColor) then
    AGraphics.FillRectangle(AContentBounds, AFillColor);

  if not EqualRect(FInitialShapeBounds, FInitialContentBounds) then
  begin
    ABounds.InitSize(AShapeBounds.X, AShapeBounds.Y, AContentBounds.X - AShapeBounds.X, AShapeBounds.Height);
    AGraphics.FillRectangle(ABounds, AOutlineColor);
    ABounds.InitSize(AContentBounds.X, AShapeBounds.Y, AContentBounds.Width, AContentBounds.Y - AShapeBounds.Y);
    AGraphics.FillRectangle(ABounds, AOutlineColor);
    ABounds.InitSize(AContentBounds.Right, AShapeBounds.Y, AShapeBounds.Right - AContentBounds.Right, AShapeBounds.Height);
    AGraphics.FillRectangle(ABounds, AOutlineColor);
    ABounds.InitSize(AContentBounds.X, AContentBounds.Bottom, AContentBounds.Width, AShapeBounds.Bottom - AContentBounds.Bottom);
    AGraphics.FillRectangle(ABounds, AOutlineColor);
  end;
end;

{ TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy }

function TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy.GetWinControl: TdxRichEditControlBase;
begin
  Result := TdxRichEditControlBase(Control);
end;

procedure TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy.ShowVisualFeedbackCore(const ABounds: TRect;
  APageViewInfo: TdxPageViewInfo; AImage: TdxOfficeImage);
begin
  if AImage = nil then
    Exit;

  FPainter.Image := AImage;
  FPainter.Bounds := ABounds;
  FPainter.Transform := State.CreateVisualFeedbackTransform;
  WinControl.Painter.DeferredDraw(APageViewInfo, FPainter.Paint);
  WinControl.Refresh;
end;

procedure TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy.HideVisualFeedbackCore(const ABounds: TRect;
  APageViewInfo: TdxPageViewInfo);
begin
  WinControl.Invalidate;
end;

function TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy.CreateFeedbackImage(
  AOriginalImage: TdxOfficeImage): TdxOfficeImage;
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
  ASize: TSize;
  ABmp: TdxOfficeImage;
  ACanvas: TdxGPCanvas;
begin
  AUnitConverter := DocumentModel.LayoutUnitConverter;
  ASize := AUnitConverter.LayoutUnitsToPixels(InitialContentBounds.Size);
  ABmp := TdxOfficeImage.CreateSize(Max(ASize.Width, 1), Max(ASize.Height, 1));
  try
    ACanvas := ABmp.CreateCanvas;
    try
      ACanvas.Clear(TdxAlphaColors.Transparent);
      if AOriginalImage <> nil then
        ACanvas.Draw(AOriginalImage, TRect.CreateSize(ASize));
    finally
      ACanvas.Free;
    end;
    Result := CreateSemitransparentFeedbackImage(ABmp, Alpha);
  finally
    ABmp.Free;
  end;
end;

function TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy.CreateSemitransparentFeedbackImage(
  ABitmap: TdxSmartImage; AAlpha: Single): TdxOfficeImage;
var
  ACanvas: TdxGPCanvas;
  AMatrix: TdxGPColorMatrix;
  AAttributes: TdxGPImageAttributes;
begin
  Result := TdxOfficeImage.CreateSize(ABitmap.Width, ABitmap.Height);
  ACanvas := Result.CreateCanvas;
  try
    ACanvas.Clear(TdxAlphaColors.Transparent);
    AMatrix := dxGpDefaultColorMatrix;
    AMatrix[3, 3] := AAlpha;

    AAttributes := TdxGPImageAttributes.Create;
    try
      AAttributes.SetColorMatrix(@AMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
      ACanvas.Draw(ABitmap, ABitmap.ClientRect, AAttributes);
    finally
      AAttributes.Free;
    end;
  finally
    ACanvas.Free;
  end;
end;

procedure TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy.BeginVisualFeedback;
var
  AFillColor, AOutlineColor: TdxAlphaColor;
begin
  FPainter := TdxShadowedFloatingObjectPainter.Create(DocumentModel.LayoutUnitConverter);
  FPainter.InitialShapeBounds := InitialShapeBounds;
  FPainter.InitialContentBounds := InitialContentBounds;
  AFillColor := Run.Shape.FillColor;
  AOutlineColor := Run.Shape.OutlineColor;

  FPainter.FillColor := TdxAlphaColors.FromArgb(Round(Alpha * TdxAlphaColors.Alpha(AFillColor)), AFillColor);
  FPainter.OutlineColor := TdxAlphaColors.FromArgb(Round(Alpha * TdxAlphaColors.Alpha(AOutlineColor)), AOutlineColor);
end;

procedure TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy.EndVisualFeedback;
begin
  if (FPainter <> nil) and (FPainter.Image <> nil) then
  begin
    FPainter.Image.Free;
    FPainter.Image := nil;
  end;
  FreeAndNil(FPainter);
end;

{ TdxVCLRectangularObjectResizeMouseHandlerStateStrategy }

destructor TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.Destroy;
begin
  FreeAndNil(FPainter);
  inherited Destroy;
end;

function TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.GetWinControl: TdxRichEditControlBase;
begin
  Result := TdxRichEditControlBase(Control);
end;

procedure TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.BeginVisualFeedback;
begin
  FUseReversibleFrame := not TryFloatingObjectVisualFeedback;
  if FUseReversibleFrame then
    FUseReversibleFrame := not TryInlinePictureVisualFeedback;

  if FUseReversibleFrame then
    DrawReversibleFrameCore;
end;

function TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.TryFloatingObjectVisualFeedback: Boolean;
var
  ABox: TdxFloatingObjectBox;
  ARun: TdxFloatingObjectAnchorRun;
  AFillColor, AOutlineColor: TdxAlphaColor;
  AContent: TdxPictureFloatingObjectContent;
begin
  if not (State.HotZone.Box is TdxFloatingObjectBox) then
    Exit(False);
  ABox := TdxFloatingObjectBox(State.HotZone.Box);

  ARun := ABox.GetFloatingObjectRun;
  FPainter := TdxShadowedFloatingObjectPainter.Create(State.DocumentModel.LayoutUnitConverter);
  FPainter.Alpha := Alpha;
  FPainter.InitialShapeBounds := ABox.Bounds;
  FPainter.InitialContentBounds := ABox.ContentBounds;
  AFillColor := ARun.Shape.FillColor;
  AOutlineColor := ARun.Shape.OutlineColor;
  FPainter.FillColor := TdxAlphaColors.FromArgb(Byte(Trunc(Alpha * dxGetAlpha(AFillColor))), AFillColor);
  FPainter.OutlineColor := TdxAlphaColors.FromArgb(Byte(Trunc(Alpha * dxGetAlpha(AOutlineColor))), AOutlineColor);
  if ARun.Content is TdxPictureFloatingObjectContent then
  begin
    AContent := TdxPictureFloatingObjectContent(ARun.Content);
    if AContent <> nil then
      FPainter.Image := AContent.Image.Image;
    FPainter.Transform := State.CreateVisualFeedbackTransform;
  end;
  Result := True;
end;

function TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.TryInlinePictureVisualFeedback: Boolean;
var
  ABox: TdxInlinePictureBox;
  APieceTable: TdxPieceTable;
begin
  if not (State.HotZone.Box is TdxInlinePictureBox) then
    Exit(False);
  ABox := TdxInlinePictureBox(State.HotZone.Box);
  FPainter := TdxShadowedFloatingObjectPainter.Create(State.DocumentModel.LayoutUnitConverter);
  FPainter.Alpha := Alpha;
  FPainter.InitialShapeBounds := ABox.Bounds;
  FPainter.InitialContentBounds := ABox.Bounds;

  APieceTable := State.DocumentModel.ActivePieceTable;
  FPainter.Image := ABox.GetImage(APieceTable, Control.ReadOnly);
  FPainter.SizeMode := ABox.GetSizing(APieceTable);
  FPainter.ShadowDisplayMode := ABox.GetResizingShadowDisplayMode(APieceTable);
  Result := True;
end;

procedure TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.ShowVisualFeedback;
begin
  if FUseReversibleFrame then
    DrawReversibleFrameCore
  else
    DrawVisualFeedback;
end;

procedure TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.HideVisualFeedback;
begin
  if not FUseReversibleFrame then
    WinControl.Invalidate;
end;

procedure TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.EndVisualFeedback;
begin
  WinControl.Invalidate;
  FreeAndNil(FPainter);
end;

procedure TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.DrawReversibleFrameCore;
begin
  WinControl.Painter.DeferredDrawReversibleFrame(ObjectActualBounds, PageViewInfo);
  WinControl.Refresh;
end;

procedure TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.DrawVisualFeedback;
begin
  FPainter.Bounds := State.CalculateBoxBounds;
  FPainter.Transform := State.CreateVisualFeedbackTransform;
  WinControl.Painter.DeferredDraw(PageViewInfo, FPainter.Paint);
  WinControl.Refresh;
end;

end.
