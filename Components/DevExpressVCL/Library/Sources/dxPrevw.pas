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

unit dxPreVw;

interface

{$I cxVer.inc}

uses
  Classes, Controls, Windows, Graphics, Forms, SysUtils, dxCore, dxBkgnd, cxGraphics, cxGeometry, dxCustomPreview,
  dxPSReportRenderCanvas;

type
  TCustomdxPreview = class;

  { TdxPreviewPageBackground }

  TdxPreviewPageBackground = class(TdxBackground)
  strict private
    FBitmap: TBitmap;
    FPreview: TCustomdxPreview;
  protected
    procedure DoApply; override;
    procedure DoChange(AChangeWhats: TdxBackgroundParams); override;
    function HasPreview: Boolean;
  public
    constructor Create(APreview: TCustomdxPreview); reintroduce;
    destructor Destroy; override;
    procedure Paint(ACanvas: TCanvas; const R: TRect); override;
    //
    property Preview: TCustomdxPreview read FPreview;
    property OnApply;
  end;

  { TCustomdxPreview }

  TCustomdxPreview = class(TdxCustomPreview)
  strict private
    FPageBackground: TdxBackground;
    FPageBackgroundApplyToEntirePage: Boolean;

    procedure SetPageBackground(Value: TdxBackground);
    procedure SetPageBackgroundApplyToEntirePage(AValue: Boolean);
  protected
    function CanCalculate: Boolean; override;
    function CreateBackgroundRegion(APage: TdxPreviewPage): TcxRegion; override;
    procedure DrawPageBackground(ACanvas: TcxCanvas; APage: TdxPreviewPage; ASelected: Boolean); override;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    //
    property PageBackgroundApplyToEntirePage: Boolean read FPageBackgroundApplyToEntirePage write SetPageBackgroundApplyToEntirePage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PageBackground: TdxBackground read FPageBackground write SetPageBackground;
  end;

  { TdxPreview }

  TdxPreview = class(TCustomdxPreview)
  strict private
    function GetOptionsBehavior: TdxPreviewOptionsBehavior;
    function GetOptionsView: TdxPreviewOptionsView;
    function GetOptionsZoom: TdxPreviewOptionsZoom;
    procedure SetOptionsBehavior(const AValue: TdxPreviewOptionsBehavior);
    procedure SetOptionsView(const AValue: TdxPreviewOptionsView);
    procedure SetOptionsZoom(const AValue: TdxPreviewOptionsZoom);
  public
    property SelPage;
    property SelPageIndex;
  published
    property OptionsBehavior: TdxPreviewOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior default dxPreviewDefaultBehaviorOptions;
    property OptionsView: TdxPreviewOptionsView read GetOptionsView write SetOptionsView default dxPreviewDefaultOptionsView;
    property OptionsZoom: TdxPreviewOptionsZoom read GetOptionsZoom write SetOptionsZoom default dxPreviewDefaultOptionsZoom;

    property Align;
    property Anchors;
    property BorderStyle;
    property Color default clBtnShadow;
    property Constraints;
    property Ctl3D;
    property DragMode;
    property Enabled;
    property LookAndFeel;
    property MarginColor;
    property Margins;
    property MaxZoomFactor;
    property MeasurementUnits;
    property MinZoomFactor;
    property OptionsHint;
    property OptionsStore;
    property PageBackground;
    property PageXCount;
    property PageYCount;
    property ParentBackground;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property ZoomFactor;
    property ZoomMode;
    property ZoomStep;
    property OnAfterDragMargin;
    property OnBeforeDragMargin;
    property OnCalcPageCount;
    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragMargin;
    property OnDragOver;
    property OnDrawPageBackground;
    property OnDrawPageContent;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetPageNumberHint;
    property OnCanShowMarginHint;
    property OnMarginChanged;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectedPageChanged;
    property OnSelectedPageChanging;
    property OnSelectingPage;
    property OnStartDrag;
    property OnZoomFactorChanged;
    property OnZoomModeChanged;
  end;

implementation

uses
  dxPSRes, dxPSImgs;

{ TdxPreviewPageBackground }

constructor TdxPreviewPageBackground.Create(APreview: TCustomdxPreview);
begin
  inherited Create;
  FPreview := APreview;
  FBitmap := TBitmap.Create;
end;

destructor TdxPreviewPageBackground.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TdxPreviewPageBackground.Paint(ACanvas: TCanvas; const R: TRect);
begin
  if (Mode = bmPicture) and (PictureMode = ppmCenter) and (Preview.ZoomFactor <> 100) then
  begin
    FBitmap.Width := MulDiv(Picture.Width, Preview.ZoomFactor, 100);
    FBitmap.Height := MulDiv(Picture.Height, Preview.ZoomFactor, 100);
    FBitmap.Canvas.StretchDraw(Rect(0, 0, FBitmap.Width, FBitmap.Height), Picture);
    cxBkgndDrawPicture(FBitmap, ACanvas, R, PictureMode, 1, 1);
  end
  else
    inherited Paint(ACanvas, R);
end;

procedure TdxPreviewPageBackground.DoApply;
begin
  inherited DoApply;
  if HasPreview and not Preview.IsUpdateLocked then
  begin
    Preview.InvalidatePages;
    Preview.DesignerModified;
  end;
end;

procedure TdxPreviewPageBackground.DoChange(AChangeWhats: TdxBackgroundParams);
begin
  inherited DoChange(AChangeWhats);
  if (UpdateCount = 0) and IsRepaintNeeded and HasPreview and not Preview.IsUpdateLocked then
    FPreview.InvalidatePages;
end;

function TdxPreviewPageBackground.HasPreview: Boolean;
begin
  Result := Preview <> nil;
end;

{ TCustomdxPreview }

constructor TCustomdxPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageBackground := TdxPreviewPageBackground.Create(Self);
end;

destructor TCustomdxPreview.Destroy;
begin
  FreeAndnil(FPageBackground);
  inherited Destroy;
end;

function TCustomdxPreview.CanCalculate: Boolean;
begin
  Result := True;
end;

function TCustomdxPreview.CreateBackgroundRegion(APage: TdxPreviewPage): TcxRegion;
begin
  if PageBackgroundApplyToEntirePage then
    Result := TcxRegion.Create(APage.Bounds)
  else
    Result := inherited CreateBackgroundRegion(APage);
end;

procedure TCustomdxPreview.DrawPageBackground(ACanvas: TcxCanvas; APage: TdxPreviewPage; ASelected: Boolean);
begin
  inherited DrawPageBackground(ACanvas, APage, ASelected);

  if CanDrawBackground and (PageBackground.Mode <> bmNone) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.SetClipRegion(CreateBackgroundRegion(APage), roIntersect);
      PageBackground.Paint(ACanvas.Canvas, APage.Bounds);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

function TCustomdxPreview.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

procedure TCustomdxPreview.SetPageBackground(Value: TdxBackground);
begin
  PageBackground.Assign(Value);
end;

procedure TCustomdxPreview.SetPageBackgroundApplyToEntirePage(AValue: Boolean);
begin
  if FPageBackgroundApplyToEntirePage <> AValue then
  begin
    FPageBackgroundApplyToEntirePage := AValue;
    InvalidatePages;
  end;
end;

{ TdxPreview }

function TdxPreview.GetOptionsBehavior: TdxPreviewOptionsBehavior;
begin
  Result:= InternalOptionsBehavior;
end;

function TdxPreview.GetOptionsView: TdxPreviewOptionsView;
begin
  Result:= InternalOptionsView;
end;

function TdxPreview.GetOptionsZoom: TdxPreviewOptionsZoom;
begin
  Result:= InternalOptionsZoom;
end;

procedure TdxPreview.SetOptionsBehavior(const AValue: TdxPreviewOptionsBehavior);
begin
  InternalOptionsBehavior := AValue;
end;

procedure TdxPreview.SetOptionsView(const AValue: TdxPreviewOptionsView);
begin
  InternalOptionsView := AValue;
end;

procedure TdxPreview.SetOptionsZoom(const AValue: TdxPreviewOptionsZoom);
begin
  InternalOptionsZoom := AValue;
end;

end.
