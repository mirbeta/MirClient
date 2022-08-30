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

unit dxRichEdit.Control.HotZones;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Generics.Defaults, Generics.Collections, Controls,

  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.InnerControl.Mouse,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Control.HitTest;

type
  TdxRectangularObjectTopLeftHotZone = class;
  TdxRectangularObjectTopMiddleHotZone = class;
  TdxRectangularObjectTopRightHotZone = class;
  TdxRectangularObjectMiddleLeftHotZone = class;
  TdxRectangularObjectMiddleRightHotZone = class;
  TdxRectangularObjectBottomLeftHotZone = class;
  TdxRectangularObjectBottomMiddleHotZone = class;
  TdxRectangularObjectBottomRightHotZone = class;
  TdxRectangularObjectRotationHotZone = class;

  { IdxRectangularObjectHotZoneVisitor }

  IdxRectangularObjectHotZoneVisitor = interface(IdxHotZoneVisitor)
    procedure Visit(AHotZone: TdxRectangularObjectTopLeftHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectTopMiddleHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectTopRightHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectMiddleLeftHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectMiddleRightHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectBottomLeftHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectBottomMiddleHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectBottomRightHotZone); overload;
    procedure Visit(AHotZone: TdxRectangularObjectRotationHotZone); overload;
  end;

  { TdxHotZone }

  TdxHotZone = class abstract(TdxReferencedObject, IdxHotZone)
  public const
    HotZoneSize = 10;
  private
    FBounds: TRect;
    FGestureStateIndicator: IdxGestureStateIndicator;
    FExtendedBounds: TRect;
    FHitTestTransform: TdxTransformMatrix;
    function GetUseExtendedBounds: Boolean;
  protected
    procedure AcceptCore(const AVisitor: IdxHotZoneVisitor); virtual; abstract;
    function GetCursor: TCursor; virtual; abstract;
  public
    function CalculateActualBounds(ADpiX, AScaleFactor: Single): TRect;

    function BeforeActivate(AController: TdxRichEditCustomMouseController; AResult: TdxRichEditHitTestResult): Boolean; virtual; abstract;
    procedure Activate(AController: TdxRichEditCustomMouseController; AResult: TdxRichEditHitTestResult); virtual; abstract;

    //IdxHotZone
    function HitTest(const P: TPoint; ADpi, AScaleFactor: Single): Boolean;
    procedure Accept(const AVisitor: IdxHotZoneVisitor);

    property Bounds: TRect read FBounds write FBounds;
    property Cursor: TCursor read GetCursor;
    property GestureStateIndicator: IdxGestureStateIndicator read FGestureStateIndicator write FGestureStateIndicator;
    property HitTestTransform: TdxTransformMatrix read FHitTestTransform write FHitTestTransform;
    property ExtendedBounds: TRect read FExtendedBounds write FExtendedBounds;
    property UseExtendedBounds: Boolean read GetUseExtendedBounds;
  end;

  TdxHotZoneCollection = class(TdxReferencedObjectList<TdxHotZone>);

  { TdxRectangularObjectHotZone }

  TdxRectangularObjectHotZone = class abstract(TdxHotZone)
  strict private
    FBox: TdxBox;
    FPieceTable: TdxPieceTable;
    function GetEnlargedHotZones: Boolean;
  protected
    function GetCanKeepAspectRatio: Boolean; virtual;
    function ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect; overload; virtual;
    function CreateValidBoxBoundsCore(const APoint: TPoint): TRect; virtual; abstract;
    function CalculateOffsetCore(const APoint: TPoint): TPoint; virtual; abstract;
    function CalculateCursor(ADefaultCursor: TCursor): TCursor;
    function CalculateCursorAngle(ADefaultCursor: TCursor): Single;
    function CalculateCursorByAngle(AAngle: Single): TCursor;
    procedure AcceptCore(const AVisitor: IdxHotZoneVisitor); override;
  public
    constructor Create(ABox: TdxBox; APieceTable: TdxPieceTable;
      const AGestureStateIndicator: IdxGestureStateIndicator);
    function BeforeActivate(AController: TdxRichEditCustomMouseController; AResult: TdxRichEditHitTestResult): Boolean; override;
    procedure Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor); virtual; abstract;
    procedure Activate(AController: TdxRichEditCustomMouseController; AResult: TdxRichEditHitTestResult); override;
    function CreateValidBoxBounds(APoint: TPoint): TRect;
    function CalculateOffset(APoint: TPoint): TPoint;
    function ForceKeepOriginalAspectRatio(const ABounds: TRect; const AOriginalSize: TSize): TRect; overload; virtual;

    property Box: TdxBox read FBox;
    property CanKeepAspectRatio: Boolean read GetCanKeepAspectRatio;
    property PieceTable: TdxPieceTable read FPieceTable;
    property EnlargedHotZones: Boolean read GetEnlargedHotZones;
  end;

  { TdxRectangularObjectTopLeftHotZone }

  TdxRectangularObjectTopLeftHotZone = class(TdxRectangularObjectHotZone)
  protected
    function GetCursor: TCursor; override;
    function CreateValidBoxBoundsCore(const APoint: TPoint): TRect; override;
    function CalculateOffsetCore(const APoint: TPoint): TPoint; override;
    function ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect; override;
  public
    procedure Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor); override;
  end;

  { TdxRectangularObjectBottomRightHotZone }

  TdxRectangularObjectBottomRightHotZone = class(TdxRectangularObjectHotZone)
  protected
    function GetCursor: TCursor; override;
    function CreateValidBoxBoundsCore(const APoint: TPoint): TRect; override;
    function CalculateOffsetCore(const APoint: TPoint): TPoint; override;
    function ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect; override;
  public
    procedure Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor); override;
  end;

  { TdxRectangularObjectTopRightHotZone }

  TdxRectangularObjectTopRightHotZone = class(TdxRectangularObjectHotZone)
  protected
    function GetCursor: TCursor; override;
    function CreateValidBoxBoundsCore(const APoint: TPoint): TRect; override;
    function CalculateOffsetCore(const APoint: TPoint): TPoint; override;
    function ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect; override;
  public
    procedure Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor); override;
  end;

  { TdxRectangularObjectBottomLeftHotZone }

  TdxRectangularObjectBottomLeftHotZone = class(TdxRectangularObjectHotZone)
  protected
    function GetCursor: TCursor; override;
    function CreateValidBoxBoundsCore(const APoint: TPoint): TRect; override;
    function CalculateOffsetCore(const APoint: TPoint): TPoint; override;
    function ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect; override;
  public
    procedure Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor); override;
  end;

  { TdxRectangularObjectTopMiddleHotZone }

  TdxRectangularObjectTopMiddleHotZone = class(TdxRectangularObjectHotZone)
  protected
    function GetCursor: TCursor; override;
    function GetCanKeepAspectRatio: Boolean; override;
    function CreateValidBoxBoundsCore(const APoint: TPoint): TRect; override;
    function CalculateOffsetCore(const APoint: TPoint): TPoint; override;
  public
    procedure Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor); override;
  end;

  { TdxRectangularObjectBottomMiddleHotZone }

  TdxRectangularObjectBottomMiddleHotZone = class(TdxRectangularObjectHotZone)
  protected
    function GetCursor: TCursor; override;
    function GetCanKeepAspectRatio: Boolean; override;
    function CreateValidBoxBoundsCore(const APoint: TPoint): TRect; override;
    function CalculateOffsetCore(const APoint: TPoint): TPoint; override;
  public
    procedure Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor); override;
  end;

  { TdxRectangularObjectMiddleLeftHotZone }

  TdxRectangularObjectMiddleLeftHotZone = class(TdxRectangularObjectHotZone)
  protected
    function GetCursor: TCursor; override;
    function GetCanKeepAspectRatio: Boolean; override;
    function CreateValidBoxBoundsCore(const APoint: TPoint): TRect; override;
    function CalculateOffsetCore(const APoint: TPoint): TPoint; override;
  public
    procedure Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor); override;
  end;

  { TdxRectangularObjectMiddleRightHotZone }

  TdxRectangularObjectMiddleRightHotZone = class(TdxRectangularObjectHotZone)
  protected
    function GetCursor: TCursor; override;
    function GetCanKeepAspectRatio: Boolean; override;
    function CreateValidBoxBoundsCore(const APoint: TPoint): TRect; override;
    function CalculateOffsetCore(const APoint: TPoint): TPoint; override;
  public
    procedure Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor); override;
  end;

  { TdxRectangularObjectRotationHotZone }

  TdxRectangularObjectRotationHotZone = class(TdxRectangularObjectHotZone)
  strict private
    FLineEnd: TPoint;
  protected
    function GetCursor: TCursor; override;
    function CreateValidBoxBoundsCore(const APoint: TPoint): TRect; override;
    function CalculateOffsetCore(const APoint: TPoint): TPoint; override;
    function ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect; override;
  public
    procedure Activate(AController: TdxRichEditCustomMouseController; AResult: TdxRichEditHitTestResult); override;
    procedure Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor); override;

    property LineEnd: TPoint read FLineEnd write FLineEnd;
  end;

implementation

uses
  Math,

  cxGeometry, dxTypeHelpers,
  dxMeasurementUnits,
  dxRichEdit.Utils.Cursors,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.InlineObjectRange;

{ TdxHotZone }

function TdxHotZone.CalculateActualBounds(ADpiX, AScaleFactor: Single): TRect;
var
  AMinHotZoneSize: Integer;
begin
  AMinHotZoneSize := PixelsToDocuments(HotZoneSize, ADpiX * AScaleFactor);
  if UseExtendedBounds then
    Result := ExtendedBounds
  else
    Result := Bounds;
  if Result.Width < AMinHotZoneSize then
  begin
    Result.Left := Result.Left + (Result.Width - AMinHotZoneSize) div 2;
    Result.Width := AMinHotZoneSize;
  end;
  if Result.Height < AMinHotZoneSize then
  begin
    Result.Top := Result.Top + (Result.Height - AMinHotZoneSize) div 2;
    Result.Height := AMinHotZoneSize;
  end;
end;

function TdxHotZone.HitTest(const P: TPoint; ADpi, AScaleFactor: Single): Boolean;
var
  R: TRect;
  APoint: TPoint;
begin
  R := CalculateActualBounds(ADpi, AScaleFactor);
  APoint := P;
  if FHitTestTransform <> nil then
    APoint := FHitTestTransform.TransformPoint(APoint);
  Result := R.Contains(APoint);
end;

procedure TdxHotZone.Accept(const AVisitor: IdxHotZoneVisitor);
begin
  AcceptCore(AVisitor);
end;

function TdxHotZone.GetUseExtendedBounds: Boolean;
var
  AIndicator: IdxGestureStateIndicator;
begin
  AIndicator := GestureStateIndicator;
  Result := (AIndicator <> nil) and AIndicator.GestureActivated;
end;

{ TdxRectangularObjectHotZone }

constructor TdxRectangularObjectHotZone.Create(ABox: TdxBox; APieceTable: TdxPieceTable; const AGestureStateIndicator: IdxGestureStateIndicator);
begin
  inherited Create;
  FBox := ABox;
  FPieceTable := APieceTable;
end;

function TdxRectangularObjectHotZone.GetCanKeepAspectRatio: Boolean;
var
  ALockAspectRatio: Boolean;
  AFloatingObjectBox: TdxFloatingObjectBox;
  AInlinePictureBox: TdxInlinePictureBox;
begin
  ALockAspectRatio := True;
  if FBox is TdxFloatingObjectBox then
  begin
    AFloatingObjectBox := TdxFloatingObjectBox(FBox);
    ALockAspectRatio := AFloatingObjectBox.GetFloatingObjectRun.FloatingObjectProperties.LockAspectRatio
  end
  else
  begin
    if FBox is TdxInlinePictureBox then
    begin
      AInlinePictureBox := TdxInlinePictureBox(FBox);
      ALockAspectRatio := (TdxInlinePictureRun(AInlinePictureBox.GetRun(PieceTable))).PictureProperties.LockAspectRatio;
    end;
  end;
  Result := ALockAspectRatio xor TdxKeyboardHelper.IsShiftPressed;
end;

function TdxRectangularObjectHotZone.GetEnlargedHotZones: Boolean;
begin
  Result := GestureStateIndicator.GestureActivated;
end;

function TdxRectangularObjectHotZone.BeforeActivate(AController: TdxRichEditCustomMouseController; AResult: TdxRichEditHitTestResult): Boolean;
begin
  Result := not AController.DeactivateTextBoxPieceTableIfNeed(PieceTable, AResult);
end;

procedure TdxRectangularObjectHotZone.Activate(AController: TdxRichEditCustomMouseController; AResult: TdxRichEditHitTestResult);
var
  AState: IdxCustomMouseState;
begin
  AState := AController.CreateRectangularObjectResizeState(Self, AResult);
  AController.SwitchStateCore(AState, cxNullPoint);
end;

function TdxRectangularObjectHotZone.ForceKeepOriginalAspectRatio(const ABounds: TRect; const AOriginalSize: TSize): TRect;
var
  AAspectX, AAspectY: Single;
begin
  if not CanKeepAspectRatio then
    Exit(ABounds);
  AAspectX := Max(1.0, 100.0 * ABounds.Width / AOriginalSize.Width);
  AAspectY := Max(1.0, 100.0 * ABounds.Height / AOriginalSize.Height);
  if AAspectX > AAspectY then
    Result := ForceKeepOriginalAspectRatio(ABounds, ABounds.Width, Round(AOriginalSize.Height * AAspectX / 100.0))
  else
    Result := ForceKeepOriginalAspectRatio(ABounds, Round(AOriginalSize.Width * AAspectY / 100.0), ABounds.Height);
end;

function TdxRectangularObjectHotZone.ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect;
begin
  Result := ABounds;
end;

function TdxRectangularObjectHotZone.CreateValidBoxBounds(APoint: TPoint): TRect;
begin
  if HitTestTransform <> nil then
    APoint := HitTestTransform.TransformPoint(APoint);
  Result := CreateValidBoxBoundsCore(APoint);
end;

function TdxRectangularObjectHotZone.CalculateOffset(APoint: TPoint): TPoint;
begin
  if HitTestTransform <> nil then
    APoint := HitTestTransform.TransformPoint(APoint);
  Result := CalculateOffsetCore(APoint);
end;

function TdxRectangularObjectHotZone.CalculateCursor(ADefaultCursor: TCursor): TCursor;
var
  ACursorAngle, ARotationAngle: Single;
begin
  if HitTestTransform = nil then
    Exit(ADefaultCursor);
  ACursorAngle := CalculateCursorAngle(ADefaultCursor);
  if ACursorAngle < 0 then
    Exit(ADefaultCursor);
  ARotationAngle := -PieceTable.DocumentModel.GetBoxEffectiveRotationAngleInDegrees(Box);
  Result := CalculateCursorByAngle(ARotationAngle + ACursorAngle);
end;

function TdxRectangularObjectHotZone.CalculateCursorAngle(ADefaultCursor: TCursor): Single;
begin
  if ADefaultCursor = TdxRichEditCursors.SizeWE then
    Exit(0);
  if ADefaultCursor = TdxRichEditCursors.SizeNS then
    Exit(90);
  if ADefaultCursor = TdxRichEditCursors.SizeNESW then
    Exit(45);
  if ADefaultCursor = TdxRichEditCursors.SizeNWSE then
    Exit(135);
  Result := -1;
end;

function TdxRectangularObjectHotZone.CalculateCursorByAngle(AAngle: Single): TCursor;
begin
  AAngle := AAngle - ((Trunc(AAngle) div 360) * 360.0);
  if AAngle >= 180.0 then
    AAngle := AAngle - 180;
  if AAngle <= -180.0 then
    AAngle := AAngle + 180.0;
  if AAngle < 0 then
    AAngle := AAngle + 180.0;

  Assert((AAngle >= 0.0) and (AAngle <= 180.0));

  if (AAngle < 22.5) or (AAngle >= 157.5) then
    Exit(TdxRichEditCursors.SizeWE);
  if (AAngle >= 22.5) and (AAngle <= 67.5) then
    Exit(TdxRichEditCursors.SizeNESW);
  if (AAngle > 67.5) and (AAngle < 112.5) then
    Exit(TdxRichEditCursors.SizeNS);
  Result := TdxRichEditCursors.SizeNWSE;
end;

procedure TdxRectangularObjectHotZone.AcceptCore(const AVisitor: IdxHotZoneVisitor);
begin
  Accept(IdxRectangularObjectHotZoneVisitor(AVisitor));
end;

{ TdxRectangularObjectTopLeftHotZone }

function TdxRectangularObjectTopLeftHotZone.GetCursor: TCursor;
begin
  Result := CalculateCursor(TdxRichEditCursors.SizeNWSE);
end;

function TdxRectangularObjectTopLeftHotZone.CreateValidBoxBoundsCore(const APoint: TPoint): TRect;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(Min(APoint.X, ABoxBounds.Right - 1), Min(APoint.Y, ABoxBounds.Bottom - 1), ABoxBounds.Right, ABoxBounds.Bottom);
end;

function TdxRectangularObjectTopLeftHotZone.CalculateOffsetCore(const APoint: TPoint): TPoint;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Left - APoint.X, ABoxBounds.Top - APoint.Y);
end;

function TdxRectangularObjectTopLeftHotZone.ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect;
begin
  Result.InitSize(ABounds.Right - ADesiredWidth, ABounds.Bottom - ADesiredHeight, ADesiredWidth, ADesiredHeight);
end;

procedure TdxRectangularObjectTopLeftHotZone.Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor);
begin
  AVisitor.Visit(Self);
end;

{ TdxRectangularObjectBottomRightHotZone }

function TdxRectangularObjectBottomRightHotZone.GetCursor: TCursor;
begin
  Result := CalculateCursor(TdxRichEditCursors.SizeNWSE);
end;

function TdxRectangularObjectBottomRightHotZone.CreateValidBoxBoundsCore(const APoint: TPoint): TRect;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Left, ABoxBounds.Top, Math.Max(APoint.X, ABoxBounds.Left + 1), Math.Max(APoint.Y, ABoxBounds.Top + 1));
end;

function TdxRectangularObjectBottomRightHotZone.CalculateOffsetCore(const APoint: TPoint): TPoint;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Right - APoint.X, ABoxBounds.Bottom - APoint.Y);
end;

function TdxRectangularObjectBottomRightHotZone.ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect;
begin
  Result.InitSize(ABounds.TopLeft, ADesiredWidth, ADesiredHeight);
end;

procedure TdxRectangularObjectBottomRightHotZone.Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor);
begin
  AVisitor.Visit(Self);
end;

{ TdxRectangularObjectTopRightHotZone }

function TdxRectangularObjectTopRightHotZone.GetCursor: TCursor;
begin
  Result := CalculateCursor(TdxRichEditCursors.SizeNESW);
end;

function TdxRectangularObjectTopRightHotZone.CreateValidBoxBoundsCore(const APoint: TPoint): TRect;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Left, Math.Min(APoint.Y, ABoxBounds.Bottom - 1), Math.Max(APoint.X, ABoxBounds.Left + 1), ABoxBounds.Bottom);
end;

function TdxRectangularObjectTopRightHotZone.CalculateOffsetCore(const APoint: TPoint): TPoint;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Right - APoint.X, ABoxBounds.Top - APoint.Y);
end;

function TdxRectangularObjectTopRightHotZone.ForceKeepOriginalAspectRatio(const ABounds: TRect;
  ADesiredWidth: Integer; ADesiredHeight: Integer): TRect;
begin
  Result.InitSize(ABounds.Left, ABounds.Bottom - ADesiredHeight, ADesiredWidth, ADesiredHeight);
end;

procedure TdxRectangularObjectTopRightHotZone.Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor);
begin
  AVisitor.Visit(Self);
end;

{ TdxRectangularObjectBottomLeftHotZone }

function TdxRectangularObjectBottomLeftHotZone.GetCursor: TCursor;
begin
  Result := CalculateCursor(TdxRichEditCursors.SizeNESW);
end;

function TdxRectangularObjectBottomLeftHotZone.CreateValidBoxBoundsCore(const APoint: TPoint): TRect;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(Min(APoint.X, ABoxBounds.Right - 1), ABoxBounds.Top, ABoxBounds.Right, Max(APoint.Y, ABoxBounds.Top + 1));
end;

function TdxRectangularObjectBottomLeftHotZone.CalculateOffsetCore(const APoint: TPoint): TPoint;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Left - APoint.X, ABoxBounds.Bottom - APoint.Y);
end;

function TdxRectangularObjectBottomLeftHotZone.ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect;
begin
  Result.InitSize(ABounds.Right - ADesiredWidth, ABounds.Top, ADesiredWidth, ADesiredHeight);
end;

procedure TdxRectangularObjectBottomLeftHotZone.Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor);
begin
  AVisitor.Visit(Self);
end;

{ TdxRectangularObjectTopMiddleHotZone }

function TdxRectangularObjectTopMiddleHotZone.GetCursor: TCursor;
begin
  Result := CalculateCursor(TdxRichEditCursors.SizeNS);
end;

function TdxRectangularObjectTopMiddleHotZone.GetCanKeepAspectRatio: Boolean;
begin
  Result := False;
end;

function TdxRectangularObjectTopMiddleHotZone.CreateValidBoxBoundsCore(const APoint: TPoint): TRect;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Left, Min(APoint.Y, ABoxBounds.Bottom - 1), ABoxBounds.Right, ABoxBounds.Bottom);
end;

function TdxRectangularObjectTopMiddleHotZone.CalculateOffsetCore(const APoint: TPoint): TPoint;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(0, ABoxBounds.Top - APoint.Y);
end;

procedure TdxRectangularObjectTopMiddleHotZone.Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor);
begin
  AVisitor.Visit(Self);
end;

{ TdxRectangularObjectBottomMiddleHotZone }

function TdxRectangularObjectBottomMiddleHotZone.GetCursor: TCursor;
begin
  Result := CalculateCursor(TdxRichEditCursors.SizeNS);
end;

function TdxRectangularObjectBottomMiddleHotZone.GetCanKeepAspectRatio: Boolean;
begin
  Result := False;
end;

function TdxRectangularObjectBottomMiddleHotZone.CreateValidBoxBoundsCore(const APoint: TPoint): TRect;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Left, ABoxBounds.Top, ABoxBounds.Right, Max(APoint.Y, ABoxBounds.Top + 1));
end;

function TdxRectangularObjectBottomMiddleHotZone.CalculateOffsetCore(const APoint: TPoint): TPoint;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(0, ABoxBounds.Bottom - APoint.Y);
end;

procedure TdxRectangularObjectBottomMiddleHotZone.Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor);
begin
  AVisitor.Visit(Self);
end;

{ TdxRectangularObjectMiddleLeftHotZone }

function TdxRectangularObjectMiddleLeftHotZone.GetCursor: TCursor;
begin
  Result := CalculateCursor(TdxRichEditCursors.SizeWE);
end;

function TdxRectangularObjectMiddleLeftHotZone.GetCanKeepAspectRatio: Boolean;
begin
  Result := False;
end;

function TdxRectangularObjectMiddleLeftHotZone.CreateValidBoxBoundsCore(const APoint: TPoint): TRect;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(Min(APoint.X, ABoxBounds.Right - 1), ABoxBounds.Top, ABoxBounds.Right, ABoxBounds.Bottom);
end;

function TdxRectangularObjectMiddleLeftHotZone.CalculateOffsetCore(const APoint: TPoint): TPoint;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Left - APoint.X, 0);
end;

procedure TdxRectangularObjectMiddleLeftHotZone.Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor);
begin
  AVisitor.Visit(Self);
end;

{ TdxRectangularObjectMiddleRightHotZone }

function TdxRectangularObjectMiddleRightHotZone.GetCursor: TCursor;
begin
  Result := CalculateCursor(TdxRichEditCursors.SizeWE);
end;

function TdxRectangularObjectMiddleRightHotZone.GetCanKeepAspectRatio: Boolean;
begin
  Result := False;
end;

function TdxRectangularObjectMiddleRightHotZone.CreateValidBoxBoundsCore(const APoint: TPoint): TRect;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Left, ABoxBounds.Top, Math.Max(APoint.X, ABoxBounds.Left + 1), ABoxBounds.Bottom);
end;

function TdxRectangularObjectMiddleRightHotZone.CalculateOffsetCore(const APoint: TPoint): TPoint;
var
  ABoxBounds: TRect;
begin
  ABoxBounds := Box.ActualSizeBounds;
  Result.Init(ABoxBounds.Right - APoint.X, 0);
end;

procedure TdxRectangularObjectMiddleRightHotZone.Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor);
begin
  AVisitor.Visit(Self);
end;

{ TdxRectangularObjectRotationHotZone }

function TdxRectangularObjectRotationHotZone.GetCursor: TCursor;
begin
  Result := CalculateCursor(TdxRichEditCursors.BeginRotate);
end;

procedure TdxRectangularObjectRotationHotZone.Activate(AController: TdxRichEditCustomMouseController; AResult: TdxRichEditHitTestResult);
var
  AState: IdxCustomMouseState;
begin
  AState := AController.CreateRectangularObjectRotateState(Self, AResult);
  AController.SwitchStateCore(AState, cxNullPoint);
end;

function TdxRectangularObjectRotationHotZone.CreateValidBoxBoundsCore(const APoint: TPoint): TRect;
begin
  Result := Box.ActualSizeBounds;
end;

function TdxRectangularObjectRotationHotZone.CalculateOffsetCore(const APoint: TPoint): TPoint;
var
  ARotationPoint: TPoint;
begin
  ARotationPoint := Bounds.CenterPoint;
  Result.Init(ARotationPoint.X - APoint.X, ARotationPoint.Y - APoint.Y);
end;

function TdxRectangularObjectRotationHotZone.ForceKeepOriginalAspectRatio(const ABounds: TRect; ADesiredWidth: Integer; ADesiredHeight: Integer): TRect;
begin
  Result := ABounds;
end;

procedure TdxRectangularObjectRotationHotZone.Accept(const AVisitor: IdxRectangularObjectHotZoneVisitor);
begin
  AVisitor.Visit(Self);
end;

end.
