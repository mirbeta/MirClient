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

unit dxRichEdit.Control.Mouse.Resize;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, Controls, Generics.Defaults, Generics.Collections,

  dxRichEdit.Utils.Graphics,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.Commands.Selection,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.Keyboard,
  dxRichEdit.Control.HotZones,
  dxRichEdit.Control.Mouse,
  dxRichEdit.Control.Mouse.DragAndDrop,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.Core;

type
  TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy = class;
  TdxResizeTableRowMouseHandlerStateStrategy = class;

  { TdxRichEditRectangularObjectResizeMouseHandlerState }

  TdxRichEditRectangularObjectResizeMouseHandlerState = class(TdxRichEditMouseCustomState)
  strict private
    FHotZone: TdxRectangularObjectHotZone;
    FInitialHitTestResult: TdxRichEditHitTestResult;
    FPlatformStrategy: TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy;
    FObjectActualBounds: TRect;
    FPageViewInfo: TdxPageViewInfo;
    FInitialOffset: TPoint;
    FOldKeyboardHandler: IdxKeyboardHandlerService;
    FLastLogicalPoint: TPoint;
    FInitialBounds: TRect;
    FInitialActualSizeBounds: TRect;
    FRotationAngle: Single;
    FMinAffectedRunIndex: TdxRunIndex;
  protected
    function CreatePlatformStrategy: TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy; virtual;
    procedure SetActionCursor; virtual;
    procedure InstallKeyboardHandler; virtual;
    procedure RestoreKeyboardHandler; virtual;
    function CreateKeyboardHandlerService(const AOldKeyboardHandler: IdxKeyboardHandlerService): TdxKeyboardHandlerServiceWrapper; virtual;
    procedure UpdateLastLogicalPoint(const APhysicalPoint: TPoint); virtual;
    procedure CommitChanges;
    procedure CommitFloatingObjectLocationAndSizeChanges(const AOffset: TPoint);
    procedure CommitFloatingObjectChanges(AAnchorRun: TdxFloatingObjectAnchorRun); virtual;
    procedure CommitFloatingObjectSizeChanges(AAnchorRun: TdxFloatingObjectAnchorRun); virtual;
    procedure CommitFloatingObjectSizeChangesCore(AAnchorRun: TdxFloatingObjectAnchorRun); virtual;
    procedure ApplySizeChanges(const ARectangularObject: IdxRectangularObject); virtual;
    procedure ApplySizeChangesCore(const ARectangularObject: IdxRectangularObject); virtual;
    procedure ApplyRunChanges(AAnchorRun: TdxFloatingObjectAnchorRun); virtual;
    function CalculateOffset: TPoint;
    function CalculateInitialOffset(const APoint: TPoint): TPoint; virtual;
    procedure UpdateObjectProperties(const APoint: TPoint); virtual;
    procedure BeginVisualFeedback; virtual;
    procedure ShowVisualFeedback; virtual;
    procedure HideVisualFeedback; virtual;
    procedure EndVisualFeedback; virtual;
    function ForceKeepOriginalAspectRatio(const ABounds: TRect): TRect; virtual;
    function GetOriginalSize(const ABounds: TRect): TSize; virtual;

    property InitialOffset: TPoint read FInitialOffset;
  public
    constructor Create(AController: TdxRichEditMouseController; AHotZone: TdxRectangularObjectHotZone;
      AInitialHitTestResult: TdxRichEditHitTestResult); reintroduce;
    destructor Destroy; override;

    function CalculateBoxBounds: TRect; virtual;
    function CreateVisualFeedbackTransform: TdxTransformMatrix; virtual;
    procedure HandleMouseMove(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseUp(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseWheel(const Args: TdxMouseEventArgs); override;
    function SuppressDefaultMouseWheelProcessing: Boolean; override;
    procedure Start; override;
    procedure Finish; override;
    procedure Update;

    property HotZone: TdxRectangularObjectHotZone read FHotZone;
    property ObjectActualBounds: TRect read FObjectActualBounds write FObjectActualBounds;
    property InitialActualSizeBounds: TRect read FInitialActualSizeBounds write FInitialActualSizeBounds;
    property PageViewInfo: TdxPageViewInfo read FPageViewInfo;
    property RotationAngle: Single read FRotationAngle write FRotationAngle;
  end;

  { TdxRichEditRectangularObjectResizeKeyboardHandlerService }

  TdxRichEditRectangularObjectResizeKeyboardHandlerService = class(TdxKeyboardHandlerServiceWrapper)
  strict private
    FOwner: TdxRichEditRectangularObjectResizeMouseHandlerState;
    FShiftPressed: Boolean;
  public
    constructor Create(AOwner: TdxRichEditRectangularObjectResizeMouseHandlerState;
      const AService: IdxKeyboardHandlerService); reintroduce;
    function HandleKeyDown(const Args: TdxKeyEventArgs): Boolean; override;
    function HandleKeyUp(const Args: TdxKeyEventArgs): Boolean; override;
    function HandleKeyPress(const Args: TdxKeyPressEventArgs): Boolean; override;
  end;

  { TdxObjectRotationAngleCalculator }

  TdxObjectRotationAngleCalculator = class
  public
    class function CalculateAngle(const APoint: TPoint; const AObjectBounds: TRect; AInitialRotationAngle: Single): Single; overload; static;
    class function CalculateAngle(APoint: TPoint; const AObjectBounds: TRect; AInitialRotationAngle: Single;
      ATransformMatrix: TdxTransformMatrix): Single; overload; static;
    class function SnapAngle(AAngle: Single): Single; overload; static;
    class function SnapAngle(AAngle: Single; AStep: Integer; ADelta: Single): Single; overload; static;
    class function NormalizeAngle(AAngle: Single): Single; static;
    class function CalculateSnap(AAngle: Single; ABaseAngle: Single; ADelta: Single): Single; static;
  end;

  { TdxRichEditRectangularObjectRotateMouseHandlerState }

  TdxRichEditRectangularObjectRotateMouseHandlerState = class(TdxRichEditRectangularObjectResizeMouseHandlerState)
  strict private
    FInitialRotationAngle: Single;
  protected
    procedure SetActionCursor; override;
    procedure UpdateObjectProperties(const APoint: TPoint); override;
    procedure ApplySizeChanges(const ARectangularObject: IdxRectangularObject); override;
    procedure ApplyRunChanges(AAnchorRun: TdxFloatingObjectAnchorRun); override;
    function SnapAngle(AAngle: Single): Single; overload;
    function SnapAngle(AAngle: Single; AStep: Integer; ADelta: Single): Single; overload;

    property InitialRotationAngle: Single read FInitialRotationAngle write FInitialRotationAngle;
  public
    constructor Create(AController: TdxRichEditMouseController; AHotZone: TdxRectangularObjectHotZone;
      AInitialHitTestResult: TdxRichEditHitTestResult); reintroduce;
    class function NormalizeAngle(AAngle: Single): Single; static;
    function CalculateSnap(AAngle: Single; ABaseAngle: Single; ADelta: Single): Single;
  end;

  { TdxFloatingObjectResizeLayoutModifier }

  TdxFloatingObjectResizeLayoutModifier = class(TdxFloatingObjectLayoutModifier)
  strict private
    FState: TdxRichEditRectangularObjectResizeMouseHandlerState;
  protected
    procedure CommitCore(const APhysicalPoint: TPoint); override;
  public
    constructor Create(AState: TdxRichEditRectangularObjectResizeMouseHandlerState; const AControl: IdxRichEditControl; ARun: TdxFloatingObjectAnchorRun; AFloatingObjectAnchorRunIndex: TdxRunIndex);
  end;

  { TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy }

  TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy = class abstract(TdxRichEditMouseHandlerStateStrategyBase)
  strict private
    function GetObjectActualBounds: TRect;
    function GetPageViewInfo: TdxPageViewInfo;
    function GetState: TdxRichEditRectangularObjectResizeMouseHandlerState;
  protected
    procedure BeginVisualFeedback; virtual; abstract;
    procedure ShowVisualFeedback; virtual; abstract;
    procedure HideVisualFeedback; virtual; abstract;
    procedure EndVisualFeedback; virtual; abstract;

    property ObjectActualBounds: TRect read GetObjectActualBounds;
    property PageViewInfo: TdxPageViewInfo read GetPageViewInfo;
    property State: TdxRichEditRectangularObjectResizeMouseHandlerState read GetState;
  end;


  { TdxResizeTableRowMouseHandlerState }

  TdxResizeTableRowMouseHandlerState = class(TdxCancellableDragMouseHandlerStateBase)
  strict private
    FInitialHitTestResult: TdxRichEditHitTestResult;
    FTableRow: TdxTableRowViewInfoBase;
    FPageViewInfo: TdxPageViewInfo;
    FReferenceTop: Integer;
    FPlatformStrategy: TdxResizeTableRowMouseHandlerStateStrategy;
    FInitialTableRowBottom: Integer;
    FTableRowBottom: Integer;
    FMouseCursorOffset: Integer;
  protected
    function CreatePlatformStrategy: TdxResizeTableRowMouseHandlerStateStrategy; virtual;
    function GetHitTestDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function GetAutoScrollEnabled: Boolean; override;
    function CalculateMouseCursor(AShiftState: TShiftState): TCursor; override;
    function CreateDataObject: IdxDataObject; override;
    function ContinueDrag(const APoint: TPoint; const AAllowedEffects: TdxDragDropEffects; AShiftState: TShiftState;
      const ADataObject: IdxDataObject): TdxDragDropEffects; override;
    function CommitDrag(const APoint: TPoint; AShiftState: TShiftState; const ADataObject: IdxDataObject): Boolean; override;
    function SnapVertically(AValue: Integer; AShiftState: TShiftState): Integer; virtual;
    function ShouldSnapVertically(AValue: Integer; const AAnchor: TdxTableCellVerticalAnchor): Boolean; virtual;
    procedure BeginVisualFeedback; override;
    procedure ShowVisualFeedback; override;
    procedure HideVisualFeedback; override;
    procedure EndVisualFeedback; override;
    procedure DrawReversibleLineCore(Y: Integer); overload; virtual;
  public
    constructor Create(AController: TdxRichEditMouseController; AInitialHitTestResult: TdxRichEditHitTestResult;
      ATableRow: TdxTableRowViewInfoBase); reintroduce;
    destructor Destroy; override;
    procedure DrawReversibleLineCore; overload;

    property PageViewInfo: TdxPageViewInfo read FPageViewInfo;
  end;

  { TdxResizeTableRowMouseHandlerStateStrategy }

  TdxResizeTableRowMouseHandlerStateStrategy = class abstract(TdxRichEditMouseHandlerStateStrategyBase)
  strict private
    function GetPageViewInfo: TdxPageViewInfo;
    function GetState: TdxResizeTableRowMouseHandlerState;
  protected
    procedure DrawReversibleLineCore(Y: Integer); virtual; abstract;
    procedure BeginVisualFeedback; virtual; abstract;
    procedure ShowVisualFeedback; virtual; abstract;
    procedure EndVisualFeedback; virtual; abstract;
    procedure HideVisualFeedback; virtual;

    property PageViewInfo: TdxPageViewInfo read GetPageViewInfo;
    property State: TdxResizeTableRowMouseHandlerState read GetState;
  end;

  { TdxResizeTableVirtualColumnMouseHandlerState }

  TdxResizeTableVirtualColumnMouseHandlerStateStrategy = class;

  TdxResizeTableVirtualColumnMouseHandlerState = class(TdxCancellableDragMouseHandlerStateBase)
  strict private
    FInitialHitTestResult: TdxRichEditHitTestResult;
    FColumn: TdxVirtualTableColumn;
    FPageViewInfo: TdxPageViewInfo;
    FReferenceLeft: TdxLayoutUnit;
    FReferenceRight: TdxLayoutUnit;
    FPlatformStrategy: TdxResizeTableVirtualColumnMouseHandlerStateStrategy;
    FInitialTableColumnRight: Integer;
    FTableColumnRight: Integer;
    FMouseCursorOffset: Integer;
  protected
    function GetHitTestDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function GetAutoScrollEnabled: Boolean; override;
    function CreatePlatformStrategy: TdxResizeTableVirtualColumnMouseHandlerStateStrategy; virtual;
    function CalculateMouseCursor(AShiftState: TShiftState): TCursor; override;
    function CreateDataObject: IdxDataObject; override;
    function ContinueDrag(const APoint: TPoint; const AAllowedEffects: TdxDragDropEffects; AShiftState: TShiftState; const ADataObject: IdxDataObject): TdxDragDropEffects; override;
    function CommitDrag(const APoint: TPoint; AShiftState: TShiftState; const ADataObject: IdxDataObject): Boolean; override;
    function SnapHorizontally(AValue: Integer; AShiftState: TShiftState): Integer; virtual;
    function ShouldSnapHorizontally(AValue: Integer; ASnapPositionIndex: Integer; ATableViewInfo: TdxTableViewInfo): Boolean; virtual;
    procedure BeginVisualFeedback; override;
    procedure ShowVisualFeedback; override;
    procedure HideVisualFeedback; override;
    procedure EndVisualFeedback; override;
    procedure DrawReversibleLineCore(X: Integer); overload; virtual;
  public
    constructor Create(AController: TdxRichEditMouseController; AInitialHitTestResult: TdxRichEditHitTestResult; AColumn: TdxVirtualTableColumn); reintroduce;
    destructor Destroy; override;
    procedure DrawReversibleLineCore; overload;

    property PageViewInfo: TdxPageViewInfo read FPageViewInfo;
  end;

  { TdxResizeTableVirtualColumnMouseHandlerStateStrategy }

  TdxResizeTableVirtualColumnMouseHandlerStateStrategy = class abstract(TdxRichEditMouseHandlerStateStrategyBase)
  strict private
    function GetPageViewInfo: TdxPageViewInfo;
    function GetState: TdxResizeTableVirtualColumnMouseHandlerState;
  protected
    procedure DrawReversibleLineCore(X: Integer); virtual; abstract;
    procedure BeginVisualFeedback; virtual; abstract;
    procedure ShowVisualFeedback; virtual; abstract;
    procedure HideVisualFeedback; virtual; abstract;
    procedure EndVisualFeedback; virtual; abstract;

    property PageViewInfo: TdxPageViewInfo read GetPageViewInfo;
    property State: TdxResizeTableVirtualColumnMouseHandlerState read GetState;
  end;

implementation

uses
  Windows, SysUtils, Math,
  dxCore, dxTypeHelpers,
  dxRichEdit.Utils.Cursors,
  dxRichEdit.Commands.Tables,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.Platform.Win.Strategies;

{ TdxRichEditRectangularObjectResizeMouseHandlerState }

constructor TdxRichEditRectangularObjectResizeMouseHandlerState.Create(
  AController: TdxRichEditMouseController;
  AHotZone: TdxRectangularObjectHotZone;
  AInitialHitTestResult: TdxRichEditHitTestResult);
var
  ABox: TdxBox;
begin
  inherited Create(AController);
  FPlatformStrategy := CreatePlatformStrategy;
  FHotZone := AHotZone;
  FInitialHitTestResult := AInitialHitTestResult;
  ABox := AHotZone.Box;
  FInitialBounds := ABox.Bounds;
  FInitialActualSizeBounds := ABox.ActualSizeBounds;
  FRotationAngle := DocumentModel.GetBoxEffectiveRotationAngleInDegrees(ABox);
  if AInitialHitTestResult.Page <> nil then
    FMinAffectedRunIndex := AInitialHitTestResult.Page.GetFirstPosition(ActivePieceTable).RunIndex
  else
    FMinAffectedRunIndex := 0;
end;

destructor TdxRichEditRectangularObjectResizeMouseHandlerState.Destroy;
begin
  FreeAndNil(FPlatformStrategy);
  inherited Destroy;
end;

function TdxRichEditRectangularObjectResizeMouseHandlerState.SuppressDefaultMouseWheelProcessing: Boolean;
begin
  Result := True;
end;

function TdxRichEditRectangularObjectResizeMouseHandlerState.CalculateBoxBounds: TRect;
var
  P: TPoint;
begin
  Result := FInitialBounds;
  P.X := Result.Left + ObjectActualBounds.Left - FInitialActualSizeBounds.Left;
  P.Y := Result.Top + ObjectActualBounds.Top - FInitialActualSizeBounds.Top;
  Result.Location := P;
  Result.Width := Result.Width + ObjectActualBounds.Width - FInitialActualSizeBounds.Width;
  Result.Height := Result.Height + ObjectActualBounds.Height - FInitialActualSizeBounds.Height;
end;

function TdxRichEditRectangularObjectResizeMouseHandlerState.CreatePlatformStrategy: TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy;
begin
  Result := TdxVCLRectangularObjectResizeMouseHandlerStateStrategy.Create(Self);
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.Start;
var
  ATopLevelPage: TdxPage;
begin
  inherited Start;
  SetActionCursor;
  FInitialOffset := CalculateInitialOffset(FInitialHitTestResult.LogicalPoint);
  UpdateObjectProperties(FInitialHitTestResult.LogicalPoint);
  ATopLevelPage := GetTopLevelPage(FInitialHitTestResult);
  FPageViewInfo := Control.InnerControl.ActiveView.LookupPageViewInfoByPage(ATopLevelPage);
  BeginVisualFeedback;
  InstallKeyboardHandler;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.Finish;
begin
  EndVisualFeedback;
  RestoreKeyboardHandler;
  inherited Finish;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.SetActionCursor;
begin
  SetMouseCursor(TdxRichEditCursors.Cross);
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.InstallKeyboardHandler;
begin
  FOldKeyboardHandler := Control.KeyboardHandler;
  if FOldKeyboardHandler <> nil then
  begin
    Control.RemoveKeyboardService(FOldKeyboardHandler);
    Control.AddKeyboardService(CreateKeyboardHandlerService(FOldKeyboardHandler));
  end;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.RestoreKeyboardHandler;
begin
  if FOldKeyboardHandler <> nil then
  begin
    Control.RemoveKeyboardService(Control.KeyboardHandler);
    Control.AddKeyboardService(FOldKeyboardHandler);
  end;
end;

function TdxRichEditRectangularObjectResizeMouseHandlerState.CreateKeyboardHandlerService(const AOldKeyboardHandler: IdxKeyboardHandlerService): TdxKeyboardHandlerServiceWrapper;
begin
  Result := TdxRichEditRectangularObjectResizeKeyboardHandlerService.Create(Self, AOldKeyboardHandler);
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.HandleMouseMove(const Args: TdxMouseEventArgs);
begin
  SetActionCursor;
  UpdateLastLogicalPoint(Args.MousePos);
  UpdateObjectProperties(FLastLogicalPoint);
  ShowVisualFeedback;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.UpdateLastLogicalPoint(
  const APhysicalPoint: TPoint);
var
  AViewInfo: TdxPageViewInfo;
begin
  AViewInfo := Control.InnerControl.ActiveView.GetPageViewInfoFromPoint(APhysicalPoint, True);
  if (AViewInfo <> nil) and (FPageViewInfo <> nil) and
      (AViewInfo.Page.PageIndex = FPageViewInfo.Page.PageIndex) then
    FPageViewInfo := AViewInfo;
  FLastLogicalPoint := Control.InnerControl.ActiveView.CreateLogicalPoint(FPageViewInfo.ClientBounds, APhysicalPoint);
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.Update;
begin
  UpdateObjectProperties(FLastLogicalPoint);
  ShowVisualFeedback;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.HandleMouseWheel(const Args: TdxMouseEventArgs);
begin
//do nothing
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.HandleMouseUp(const Args: TdxMouseEventArgs);
begin
  UpdateLastLogicalPoint(Args.MousePos);
  UpdateObjectProperties(FLastLogicalPoint);
  Controller.SwitchToDefaultState;
  CommitChanges;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.CommitChanges;
var
  ARun: TdxTextRunBase;
  ARectangularObject: IdxRectangularObject;
  AAnchorRun: TdxFloatingObjectAnchorRun;
begin
  ARun := TdxTextRunBase(HotZone.Box.GetRun(ActivePieceTable));
  if not TdxPieceTable(ARun.PieceTable).CanEditSelection then
    Exit;
  ARectangularObject := ARun.GetRectangularObject;
  if ARectangularObject = nil then
    Exit;
  if ARun is TdxFloatingObjectAnchorRun then
  begin
    AAnchorRun := TdxFloatingObjectAnchorRun(ARun);
    CommitFloatingObjectChanges(AAnchorRun);
  end
  else
    ApplySizeChanges(ARectangularObject);
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.CommitFloatingObjectLocationAndSizeChanges(const AOffset: TPoint);
var
  ARun: TdxTextRunBase;
  ACurrentTopLeftCorner, ATopLeftPhysicalPoint: TPoint;
  AAnchorRun: TdxFloatingObjectAnchorRun;
  ASelection: TdxSelection;
  ARunInfo: TdxRunInfo;
  AFloatingObjectAnchorRunIndex: TdxRunIndex;
  AModifier: TdxFloatingObjectResizeLayoutModifier;
begin
  ARun := TdxTextRunBase(HotZone.Box.GetRun(ActivePieceTable));
  if not (ARun is TdxFloatingObjectAnchorRun) then
    Exit;
  AAnchorRun := TdxFloatingObjectAnchorRun(ARun);
  ASelection := DocumentModel.Selection;
  ARunInfo := ASelection.PieceTable.FindRunInfo(ASelection.NormalizedStart, ASelection.Length);
  try
    AFloatingObjectAnchorRunIndex := ARunInfo.Start.RunIndex;
  finally
    ARunInfo.Free;
  end;
  Control.BeginUpdate;
  try
    AModifier := TdxFloatingObjectResizeLayoutModifier.Create(Self, Control, AAnchorRun, AFloatingObjectAnchorRunIndex);
    try
      AModifier.OldTopLeftCorner := FInitialActualSizeBounds.Location;
      ACurrentTopLeftCorner := AModifier.OldTopLeftCorner;
      ACurrentTopLeftCorner.X := ACurrentTopLeftCorner.X + AOffset.X;
      ACurrentTopLeftCorner.Y := ACurrentTopLeftCorner.Y + AOffset.Y;
      AModifier.CurrentTopLeftCorner := ACurrentTopLeftCorner;
      AModifier.MinAffectedRunIndex := FMinAffectedRunIndex;
      ATopLeftPhysicalPoint := Control.InnerControl.ActiveView.CreatePhysicalPoint(FPageViewInfo, AModifier.CurrentTopLeftCorner);
      AModifier.Commit(ATopLeftPhysicalPoint);
      AAnchorRun.Select;
    finally
      AModifier.Free;
    end;
  finally
    Control.EndUpdate;
  end;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.CommitFloatingObjectChanges(AAnchorRun: TdxFloatingObjectAnchorRun);
var
  AOffset: TPoint;
begin
  AOffset := CalculateOffset;
  if AAnchorRun.FloatingObjectProperties.HorizontalPositionAlignment <> TdxFloatingObjectHorizontalPositionAlignment.None then
    AOffset.X := 0;
  if AAnchorRun.FloatingObjectProperties.VerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.None then
    AOffset.Y := 0;
  if (AOffset.X <> 0) or (AOffset.Y <> 0) then
    CommitFloatingObjectLocationAndSizeChanges(AOffset)
  else
    CommitFloatingObjectSizeChanges(AAnchorRun);
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.CommitFloatingObjectSizeChanges(AAnchorRun: TdxFloatingObjectAnchorRun);
begin
  DocumentModel.BeginUpdate;
  try
    CommitFloatingObjectSizeChangesCore(AAnchorRun);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.CommitFloatingObjectSizeChangesCore(AAnchorRun: TdxFloatingObjectAnchorRun);
begin
  if AAnchorRun.Content is TdxTextBoxFloatingObjectContent then
    TdxTextBoxFloatingObjectContent(AAnchorRun.Content).TextBoxProperties.ResizeShapeToFitText := False;
  ApplySizeChanges(AAnchorRun as IdxRectangularObject);
  ApplyRunChanges(AAnchorRun);
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.ApplySizeChanges(const ARectangularObject: IdxRectangularObject);
begin
  DocumentModel.BeginUpdate;
  try
    ApplySizeChangesCore(ARectangularObject);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.ApplySizeChangesCore(const ARectangularObject: IdxRectangularObject);
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  ARectangularObject.ActualSize := TSize.Create(AUnitConverter.ToModelUnits(FObjectActualBounds.Width), AUnitConverter.ToModelUnits(FObjectActualBounds.Height));
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.ApplyRunChanges(AAnchorRun: TdxFloatingObjectAnchorRun);
begin
end;

function TdxRichEditRectangularObjectResizeMouseHandlerState.CalculateOffset: TPoint;
var
  ATransform: TdxTransformMatrix;
  ATopLeft, ACenter, ANewTopLeft: TPoint;
begin
  ATransform := TdxTransformMatrix.Create;
  try
    ATransform.Rotate(RotationAngle, FInitialActualSizeBounds.CenterPoint.ToPointF);
    ATopLeft := ATransform.TransformPoint(FObjectActualBounds.Location);
    ACenter := ATransform.TransformPoint(FObjectActualBounds.CenterPoint);
  finally
    ATransform.Free;
  end;

  ATransform := TdxTransformMatrix.Create;
  try
    ATransform := TdxTransformMatrix.Create;
    ATransform.Rotate(-RotationAngle, ACenter.ToPointF);
    ANewTopLeft := ATransform.TransformPoint(ATopLeft);
  finally
    ATransform.Free;
  end;
  Result := TPoint.Create(ANewTopLeft.X - FInitialActualSizeBounds.X, ANewTopLeft.Y - FInitialActualSizeBounds.Y);
end;

function TdxRichEditRectangularObjectResizeMouseHandlerState.CalculateInitialOffset(const APoint: TPoint): TPoint;
begin
  Result := FHotZone.CalculateOffset(APoint);
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.UpdateObjectProperties(const APoint: TPoint);
var
  AResult: TRect;
begin
  APoint.Offset(FInitialOffset);
  AResult := FHotZone.CreateValidBoxBounds(APoint);
  if HotZone.CanKeepAspectRatio then
    AResult := ForceKeepOriginalAspectRatio(AResult);

  FObjectActualBounds := AResult;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.BeginVisualFeedback;
begin
  FPlatformStrategy.BeginVisualFeedback;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.ShowVisualFeedback;
begin
  FPlatformStrategy.ShowVisualFeedback;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.HideVisualFeedback;
begin
  FPlatformStrategy.HideVisualFeedback;
end;

procedure TdxRichEditRectangularObjectResizeMouseHandlerState.EndVisualFeedback;
begin
  FPlatformStrategy.EndVisualFeedback;
end;

function TdxRichEditRectangularObjectResizeMouseHandlerState.CreateVisualFeedbackTransform: TdxTransformMatrix;
begin
  Result := TdxFloatingObjectBox.CreateTransformUnsafe(FRotationAngle, FInitialBounds);
end;

function TdxRichEditRectangularObjectResizeMouseHandlerState.ForceKeepOriginalAspectRatio(const ABounds: TRect): TRect;
var
  AActualSize: TSize;
begin
  AActualSize := InitialActualSizeBounds.Size;
  if AActualSize.IsEqual(ABounds.Size) then
    Exit(ABounds);

  Result := HotZone.ForceKeepOriginalAspectRatio(ABounds, AActualSize);
end;

function TdxRichEditRectangularObjectResizeMouseHandlerState.GetOriginalSize(const ABounds: TRect): TSize;
var
  ARectangularObject: IdxRectangularObject;
  ARectangularScalingObject: IdxRectangularScalableObject;
begin
  ARectangularObject := TdxTextRunBase(HotZone.Box.GetRun(ActivePieceTable)).GetRectangularObject;
  if ARectangularObject = nil then
    Exit(ABounds.Size);

  if Supports(ARectangularObject, IdxRectangularScalableObject, ARectangularScalingObject) then
    Result := ARectangularScalingObject.OriginalSize
  else
    Result := ARectangularObject.ActualSize;
end;

{ TdxRichEditRectangularObjectResizeKeyboardHandlerService }

constructor TdxRichEditRectangularObjectResizeKeyboardHandlerService.Create(AOwner: TdxRichEditRectangularObjectResizeMouseHandlerState; const AService: IdxKeyboardHandlerService);
begin
  inherited Create(AService);
  FOwner := AOwner;
end;

function TdxRichEditRectangularObjectResizeKeyboardHandlerService.HandleKeyDown(const Args: TdxKeyEventArgs): Boolean;
begin
  Result := False;
  if Args.KeyData = VK_ESCAPE then
  begin
    FOwner.Controller.SwitchToDefaultState;
    Result := True;
  end
  else
    if (Args.KeyData = VK_SHIFT) and not FShiftPressed then
    begin
      FOwner.Update;
      FShiftPressed := True;
    end;
end;

function TdxRichEditRectangularObjectResizeKeyboardHandlerService.HandleKeyUp(const Args: TdxKeyEventArgs): Boolean;
begin
  if Args.KeyData = VK_SHIFT then
  begin
    FOwner.Update;
    FShiftPressed := False;
  end;
  Result := False;
end;

function TdxRichEditRectangularObjectResizeKeyboardHandlerService.HandleKeyPress(const Args: TdxKeyPressEventArgs): Boolean;
begin
  Result := False;
end;

{ TdxObjectRotationAngleCalculator }

class function TdxObjectRotationAngleCalculator.CalculateAngle(const APoint: TPoint; const AObjectBounds: TRect;
  AInitialRotationAngle: Single): Single;
begin
  Result := CalculateAngle(APoint, AObjectBounds, AInitialRotationAngle, nil);
end;

class function TdxObjectRotationAngleCalculator.CalculateAngle(APoint: TPoint; const AObjectBounds: TRect;
  AInitialRotationAngle: Single; ATransformMatrix: TdxTransformMatrix): Single;
var
  ACenter: TPoint;
  ADeltaAngle: Single;
begin
  if ATransformMatrix <> nil then
    APoint := ATransformMatrix.TransformPoint(APoint);

  ACenter := AObjectBounds.CenterPoint;
  Dec(APoint.X, ACenter.X);
  Dec(APoint.Y, ACenter.Y);
  if (APoint.X = 0) and (APoint.Y = 0) then
    Exit(AInitialRotationAngle);

  if APoint.Y = 0 then
  begin
    if APoint.X < 0 then
      ADeltaAngle := -90
    else
      ADeltaAngle := 90;
  end
  else
    ADeltaAngle := RadToDeg(ArcTan(APoint.X / APoint.Y));
  if APoint.Y > 0 then
    ADeltaAngle := ADeltaAngle + 180;
  if Abs(ADeltaAngle) = 90 then
    ADeltaAngle := -ADeltaAngle;

  Result := SnapAngle(AInitialRotationAngle - ADeltaAngle);
end;

class function TdxObjectRotationAngleCalculator.SnapAngle(AAngle: Single): Single;
begin
  if TdxKeyboardHelper.IsShiftPressed then
    Result := SnapAngle(AAngle, 15, 7.5)
  else
    Result := SnapAngle(AAngle, 90, 3);
end;

class function TdxObjectRotationAngleCalculator.SnapAngle(AAngle: Single; AStep: Integer; ADelta: Single): Single;
var
  I: Integer;
begin
  AAngle := NormalizeAngle(AAngle);
  I := 0;
  repeat
    AAngle := CalculateSnap(AAngle, I, ADelta);
    Inc(I, AStep);
  until I > 360;
  Result := NormalizeAngle(AAngle);
end;

class function TdxObjectRotationAngleCalculator.NormalizeAngle(AAngle: Single): Single;
begin
  AAngle := dxFMod(AAngle, 360);
  if AAngle < 0 then
    AAngle := AAngle + 360;
  Result := AAngle;
end;

class function TdxObjectRotationAngleCalculator.CalculateSnap(AAngle: Single; ABaseAngle: Single; ADelta: Single): Single;
begin
  if (ABaseAngle - ADelta <= AAngle) and (AAngle < ABaseAngle + ADelta) then
    Result := ABaseAngle
  else
    Result := AAngle;
end;

{ TdxRichEditRectangularObjectRotateMouseHandlerState }

constructor TdxRichEditRectangularObjectRotateMouseHandlerState.Create(AController: TdxRichEditMouseController;
  AHotZone: TdxRectangularObjectHotZone; AInitialHitTestResult: TdxRichEditHitTestResult);
begin
  inherited Create(AController, AHotZone, AInitialHitTestResult);
  FInitialRotationAngle := RotationAngle;
end;

procedure TdxRichEditRectangularObjectRotateMouseHandlerState.SetActionCursor;
begin
  SetMouseCursor(TdxRichEditCursors.Rotate);
end;

procedure TdxRichEditRectangularObjectRotateMouseHandlerState.UpdateObjectProperties(const APoint: TPoint);
var
  P: TPoint;
begin
  P := APoint;
  P.Offset(InitialOffset);
  ObjectActualBounds := HotZone.CreateValidBoxBounds(P);
  RotationAngle := TdxObjectRotationAngleCalculator.CalculateAngle(APoint, ObjectActualBounds, InitialRotationAngle, HotZone.HitTestTransform);
end;

procedure TdxRichEditRectangularObjectRotateMouseHandlerState.ApplySizeChanges(const ARectangularObject: IdxRectangularObject);
begin
end;

procedure TdxRichEditRectangularObjectRotateMouseHandlerState.ApplyRunChanges(AAnchorRun: TdxFloatingObjectAnchorRun);
begin
  AAnchorRun.Shape.Rotation := DocumentModel.UnitConverter.DegreeToModelUnits(RotationAngle);
end;

function TdxRichEditRectangularObjectRotateMouseHandlerState.SnapAngle(AAngle: Single): Single;
begin
  if TdxKeyboardHelper.IsShiftPressed then
    Result := SnapAngle(AAngle, 15, 7.5)
  else
    Result := SnapAngle(AAngle, 90, 9);
end;

function TdxRichEditRectangularObjectRotateMouseHandlerState.SnapAngle(AAngle: Single; AStep: Integer; ADelta: Single): Single;
var
  I: Integer;
begin
  AAngle := NormalizeAngle(AAngle);
  I := 0;
  repeat
    AAngle := CalculateSnap(AAngle, I, ADelta);
    Inc(I, AStep);
  until I > 360;
  Result := NormalizeAngle(AAngle);
end;

class function TdxRichEditRectangularObjectRotateMouseHandlerState.NormalizeAngle(AAngle: Single): Single;
begin
  AAngle := dxFMod(AAngle, 360);
  if AAngle < 0 then
    AAngle := AAngle + 360;
  Result := AAngle;
end;

function TdxRichEditRectangularObjectRotateMouseHandlerState.CalculateSnap(AAngle: Single; ABaseAngle: Single; ADelta: Single): Single;
begin
  if (ABaseAngle - ADelta <= AAngle) and (AAngle < ABaseAngle + ADelta) then
    Result := ABaseAngle
  else
    Result := AAngle;
end;

{ TdxFloatingObjectResizeLayoutModifier }

constructor TdxFloatingObjectResizeLayoutModifier.Create(AState: TdxRichEditRectangularObjectResizeMouseHandlerState; const AControl: IdxRichEditControl; ARun: TdxFloatingObjectAnchorRun; AFloatingObjectAnchorRunIndex: TdxRunIndex);
begin
  inherited Create(AControl, ARun, AFloatingObjectAnchorRunIndex);
  Assert(AState <> nil);
  FState := AState;
end;

procedure TdxFloatingObjectResizeLayoutModifier.CommitCore(const APhysicalPoint: TPoint);
begin
  inherited CommitCore(APhysicalPoint);
  FState.CommitFloatingObjectSizeChangesCore(AnchorRun);
end;

{ TdxResizeTableRowMouseHandlerState }

constructor TdxResizeTableRowMouseHandlerState.Create(AController: TdxRichEditMouseController;
  AInitialHitTestResult: TdxRichEditHitTestResult; ATableRow: TdxTableRowViewInfoBase);
var
  ATopLevelPage: TdxPage;
begin
  inherited Create(AController);
  Assert(AInitialHitTestResult <> nil);
  Assert(ATableRow <> nil);
  FPlatformStrategy := CreatePlatformStrategy;
  FInitialHitTestResult := AInitialHitTestResult;
  FTableRow := ATableRow;
  FMouseCursorOffset := AInitialHitTestResult.LogicalPoint.Y - ATableRow.BottomAnchor.VerticalPosition;
  FReferenceTop := ATableRow.TopAnchor.VerticalPosition;
  FTableRowBottom := ATableRow.BottomAnchor.VerticalPosition;
  FInitialTableRowBottom := FTableRowBottom;
  ATopLevelPage := GetTopLevelPage(AInitialHitTestResult);
  FPageViewInfo := Control.InnerControl.ActiveView.LookupPageViewInfoByPage(ATopLevelPage);
end;

destructor TdxResizeTableRowMouseHandlerState.Destroy;
begin
  FreeAndNil(FPlatformStrategy);
  inherited Destroy;
end;

function TdxResizeTableRowMouseHandlerState.CreatePlatformStrategy: TdxResizeTableRowMouseHandlerStateStrategy;
begin
  Result := TdxVCLResizeTableRowMouseHandlerStateStrategy.Create(Self);
end;

function TdxResizeTableRowMouseHandlerState.GetHitTestDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Page;
end;

function TdxResizeTableRowMouseHandlerState.GetAutoScrollEnabled: Boolean;
begin
  Result := False;
end;

function TdxResizeTableRowMouseHandlerState.CalculateMouseCursor(AShiftState: TShiftState): TCursor;
begin
  Result := TdxRichEditCursors.ResizeTableRow;
end;

function TdxResizeTableRowMouseHandlerState.CreateDataObject: IdxDataObject;
begin
  Result := nil;
end;

function TdxResizeTableRowMouseHandlerState.ContinueDrag(const APoint: TPoint;
  const AAllowedEffects: TdxDragDropEffects; AShiftState: TShiftState; const ADataObject: IdxDataObject): TdxDragDropEffects;
var
  AHitTestResult: TdxRichEditHitTestResult;
  ABottom: Integer;
begin
  SetMouseCursor(TdxRichEditCursors.ResizeTableRow);
  AHitTestResult := CalculateHitTest(APoint);
  try
    if AHitTestResult = nil then
      Exit(AAllowedEffects);

    if GetTopLevelPage(FInitialHitTestResult) <> GetTopLevelPage(AHitTestResult) then
      Exit(AAllowedEffects);

    HideVisualFeedback;
    ABottom := SnapVertically(AHitTestResult.LogicalPoint.Y - FMouseCursorOffset, AShiftState);
    FTableRowBottom := Max(FReferenceTop, ABottom);
    ShowVisualFeedback;

    Result := AAllowedEffects;
  finally
    AHitTestResult.Free;
  end;
end;

function TdxResizeTableRowMouseHandlerState.CommitDrag(const APoint: TPoint;
  AShiftState: TShiftState; const ADataObject: IdxDataObject): Boolean;
var
  ABottom, AHeight: Integer;
  ACommand: TdxChangeTableRowHeightCommand;
begin
  if FTableRowBottom <> FInitialTableRowBottom then
  begin
    ABottom := SnapVertically(FTableRowBottom, AShiftState);
    AHeight := DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(ABottom - FReferenceTop);
    ACommand := TdxChangeTableRowHeightCommand.Create(Control, FTableRow.Row, AHeight);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
  Result := True;
end;

function TdxResizeTableRowMouseHandlerState.SnapVertically(AValue: Integer; AShiftState: TShiftState): Integer;
var
  AAnchors: TdxTableCellVerticalAnchorCollection;
  AItems: TdxTableCellVerticalAnchorList;
  AIndex: Integer;
  AComparable: TdxTableCellVerticalAnchorYComparable;
begin
  if ssAlt in AShiftState then
    Exit(AValue);

  AAnchors := FTableRow.TableViewInfo.Anchors;

  AItems := AAnchors.Items;
  AComparable := TdxTableCellVerticalAnchorYComparable.Create(AValue);
  try
    if TdxAlgorithms1<TdxTableCellVerticalAnchor>.BinarySearch(AItems, AComparable, AIndex) then
      Exit(AValue);
  finally
    AComparable.Free;
  end;

  if AIndex >= AItems.Count then
  begin
    if ShouldSnapVertically(AValue, AAnchors.Last) then
      Exit(AAnchors.Last.VerticalPosition);
    Exit(AValue);
  end;

  if ShouldSnapVertically(AValue, AAnchors[AIndex]) then
    Exit(AAnchors[AIndex].VerticalPosition);

  if AIndex > 0 then
  begin
    if ShouldSnapVertically(AValue, AAnchors[AIndex - 1]) then
      Exit(AAnchors[AIndex - 1].VerticalPosition);
  end;
  Result := AValue;
end;

function TdxResizeTableRowMouseHandlerState.ShouldSnapVertically(AValue: Integer; const AAnchor: TdxTableCellVerticalAnchor): Boolean;
var
  ASnapValue, Y: Integer;
begin
  ASnapValue := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(2, DocumentModel.DpiY);
  Y := AAnchor.VerticalPosition;
  if AValue < Y then
    Exit(Y - AValue <= ASnapValue);

  Y := AAnchor.VerticalPosition + AAnchor.BottomTextIndent;
  if AValue > Y then
    Exit(AValue - Y <= ASnapValue);

  Result := True;
end;

procedure TdxResizeTableRowMouseHandlerState.BeginVisualFeedback;
begin
  FPlatformStrategy.BeginVisualFeedback;
end;

procedure TdxResizeTableRowMouseHandlerState.ShowVisualFeedback;
begin
  FPlatformStrategy.ShowVisualFeedback;
end;

procedure TdxResizeTableRowMouseHandlerState.HideVisualFeedback;
begin
  FPlatformStrategy.HideVisualFeedback;
end;

procedure TdxResizeTableRowMouseHandlerState.EndVisualFeedback;
begin
  FPlatformStrategy.EndVisualFeedback;
end;

procedure TdxResizeTableRowMouseHandlerState.DrawReversibleLineCore;
begin
  DrawReversibleLineCore(FTableRowBottom);
end;

procedure TdxResizeTableRowMouseHandlerState.DrawReversibleLineCore(Y: Integer);
begin
  FPlatformStrategy.DrawReversibleLineCore(Y);
end;

{ TdxResizeTableRowMouseHandlerStateStrategy }

function TdxResizeTableRowMouseHandlerStateStrategy.GetPageViewInfo: TdxPageViewInfo;
begin
  Result := State.PageViewInfo;
end;

function TdxResizeTableRowMouseHandlerStateStrategy.GetState: TdxResizeTableRowMouseHandlerState;
begin
  Result := TdxResizeTableRowMouseHandlerState(inherited State);
end;

procedure TdxResizeTableRowMouseHandlerStateStrategy.HideVisualFeedback;
begin
  State.DrawReversibleLineCore;
end;

{ TdxResizeTableVirtualColumnMouseHandlerState }

constructor TdxResizeTableVirtualColumnMouseHandlerState.Create(AController: TdxRichEditMouseController;
  AInitialHitTestResult: TdxRichEditHitTestResult; AColumn: TdxVirtualTableColumn);
var
  ATopLevelPage: TdxPage;
begin
  inherited Create(AController);
  Assert(AInitialHitTestResult is TdxRichEditHitTestResult);
  Assert(AColumn is TdxVirtualTableColumn);
  FPlatformStrategy := CreatePlatformStrategy;
  FInitialHitTestResult := AInitialHitTestResult;
  FColumn := AColumn;
  FMouseCursorOffset := 0;
  FReferenceLeft := AColumn.MaxLeftBorder;
  FReferenceRight := AColumn.MaxRightBorder;
  FTableColumnRight := AColumn.Position;
  FInitialTableColumnRight := FTableColumnRight;
  ATopLevelPage := GetTopLevelPage(AInitialHitTestResult);
  FPageViewInfo := Control.InnerControl.ActiveView.LookupPageViewInfoByPage(ATopLevelPage);
end;

destructor TdxResizeTableVirtualColumnMouseHandlerState.Destroy;
begin
  FreeAndNil(FColumn);
  FreeAndNil(FPlatformStrategy);
  inherited Destroy;
end;

function TdxResizeTableVirtualColumnMouseHandlerState.GetHitTestDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Page;
end;

function TdxResizeTableVirtualColumnMouseHandlerState.GetAutoScrollEnabled: Boolean;
begin
  Result := False;
end;

function TdxResizeTableVirtualColumnMouseHandlerState.CreatePlatformStrategy: TdxResizeTableVirtualColumnMouseHandlerStateStrategy;
begin
  Result := TdxVCLResizeTableVirtualColumnMouseHandlerStateStrategy.Create(Self);
end;

function TdxResizeTableVirtualColumnMouseHandlerState.CalculateMouseCursor(AShiftState: TShiftState): TCursor;
begin
  Result := TdxRichEditCursors.ResizeTableColumn;
end;

function TdxResizeTableVirtualColumnMouseHandlerState.CreateDataObject: IdxDataObject;
begin
  Result := nil;
end;

function TdxResizeTableVirtualColumnMouseHandlerState.ContinueDrag(const APoint: TPoint; const AAllowedEffects: TdxDragDropEffects;
  AShiftState: TShiftState; const ADataObject: IdxDataObject): TdxDragDropEffects;
var
  AHitTestResult: TdxRichEditHitTestResult;
  ARight: Integer;
begin
  SetMouseCursor(TdxRichEditCursors.ResizeTableColumn);

  AHitTestResult := CalculateHitTest(APoint);
  try
    if AHitTestResult = nil then
      Exit(AAllowedEffects);

    if GetTopLevelPage(FInitialHitTestResult) <> GetTopLevelPage(AHitTestResult) then
      Exit(AAllowedEffects);

    HideVisualFeedback;
    ARight := SnapHorizontally(AHitTestResult.LogicalPoint.X - FMouseCursorOffset, AShiftState);
    FTableColumnRight := Min(Max(FReferenceLeft, ARight), FReferenceRight);
    ShowVisualFeedback;
    Result := AAllowedEffects;
  finally
    AHitTestResult.Free;
  end;
end;

function TdxResizeTableVirtualColumnMouseHandlerState.CommitDrag(const APoint: TPoint; AShiftState: TShiftState; const ADataObject: IdxDataObject): Boolean;
var
  ARight: Integer;
  ACommand: TdxChangeTableVirtualColumnRightCommand;
begin
  if FTableColumnRight <> FInitialTableColumnRight then
  begin
    ARight := SnapHorizontally(FTableColumnRight, AShiftState);
    FTableColumnRight := Min(Max(FReferenceLeft, ARight), FReferenceRight);
    ACommand := TdxChangeTableVirtualColumnRightCommand.Create(Control, FColumn, FTableColumnRight);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
  Result := True;
end;

function TdxResizeTableVirtualColumnMouseHandlerState.SnapHorizontally(AValue: Integer; AShiftState: TShiftState): Integer;
var
  ATableViewInfo: TdxTableViewInfo;
  AHorizontalOffset, AIndex: Integer;
  APositions: TdxLayoutUnitSortedList;
begin
  if ssAlt in AShiftState then
    Exit(AValue);

  ATableViewInfo := FColumn.TableViewInfo;
  AHorizontalOffset := ATableViewInfo.Column.Bounds.Left;
  APositions := ATableViewInfo.VerticalBorderPositions.AlignedPosition;

  AIndex := APositions.BinarySearch(AValue - AHorizontalOffset);
  if AIndex >= 0 then
    Exit(AValue);

  AIndex := not AIndex;
  if AIndex >= APositions.Count then
  begin
    if ShouldSnapHorizontally(AValue, APositions.Count - 1, ATableViewInfo) then
      Exit(APositions.Last + AHorizontalOffset)
    else
      Exit(AValue);
  end;
  if ShouldSnapHorizontally(AValue, AIndex, ATableViewInfo) then
    Exit(APositions[AIndex] + AHorizontalOffset);
  if AIndex > 0 then
  begin
    if ShouldSnapHorizontally(AValue, AIndex - 1, ATableViewInfo) then
      Exit(APositions[AIndex - 1] + AHorizontalOffset);
  end;
  Result := AValue;
end;

function TdxResizeTableVirtualColumnMouseHandlerState.ShouldSnapHorizontally(AValue: Integer;
  ASnapPositionIndex: Integer; ATableViewInfo: TdxTableViewInfo): Boolean;
var
  ASnapValue, APosition: Integer;
begin
  ASnapValue := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(2, DocumentModel.DpiX);
  APosition := ATableViewInfo.GetAlignedPosition(ASnapPositionIndex);
  Result := Abs(APosition - AValue) <= ASnapValue;
end;

procedure TdxResizeTableVirtualColumnMouseHandlerState.BeginVisualFeedback;
begin
  FPlatformStrategy.BeginVisualFeedback;
end;

procedure TdxResizeTableVirtualColumnMouseHandlerState.ShowVisualFeedback;
begin
  FPlatformStrategy.ShowVisualFeedback;
end;

procedure TdxResizeTableVirtualColumnMouseHandlerState.HideVisualFeedback;
begin
  FPlatformStrategy.HideVisualFeedback;
end;

procedure TdxResizeTableVirtualColumnMouseHandlerState.EndVisualFeedback;
begin
  FPlatformStrategy.EndVisualFeedback;
end;

procedure TdxResizeTableVirtualColumnMouseHandlerState.DrawReversibleLineCore;
begin
  DrawReversibleLineCore(FTableColumnRight);
end;

procedure TdxResizeTableVirtualColumnMouseHandlerState.DrawReversibleLineCore(X: Integer);
begin
  FPlatformStrategy.DrawReversibleLineCore(X);
end;

{ TdxResizeTableVirtualColumnMouseHandlerStateStrategy }

function TdxResizeTableVirtualColumnMouseHandlerStateStrategy.GetPageViewInfo: TdxPageViewInfo;
begin
  Result := State.PageViewInfo;
end;

function TdxResizeTableVirtualColumnMouseHandlerStateStrategy.GetState: TdxResizeTableVirtualColumnMouseHandlerState;
begin
  Result := TdxResizeTableVirtualColumnMouseHandlerState(inherited State);
end;

{ TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy }

function TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy.GetObjectActualBounds: TRect;
begin
  Result := State.ObjectActualBounds;
end;

function TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy.GetPageViewInfo: TdxPageViewInfo;
begin
  Result := State.PageViewInfo;
end;

function TdxRichEditRectangularObjectResizeMouseHandlerStateStrategy.GetState: TdxRichEditRectangularObjectResizeMouseHandlerState;
begin
  Result := TdxRichEditRectangularObjectResizeMouseHandlerState(inherited State);
end;

end.

