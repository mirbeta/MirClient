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

unit dxRichEdit.Control.Mouse.DragAndDrop;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, Controls, SysUtils, ActiveX, Generics.Defaults, Generics.Collections, dxCore,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.History.FloatingObject,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.Mouse;

type
  TdxDragContentMouseHandlerStateBaseStrategy = class;

  { TdxRichEditDragCaretCustomVisualizer }

  TdxRichEditDragCaretCustomVisualizer = class abstract
  public
    procedure Finish; virtual;
    procedure Start; virtual;
    procedure ShowCaret(ACaretLogPosition: TdxDocumentLogPosition); virtual; abstract;
    procedure HideCaret(ACaretLogPosition: TdxDocumentLogPosition); virtual; abstract;
  end;

  { TdxRichEditDragCaretVisualizer }

  TdxRichEditDragCaretVisualizer = class(TdxRichEditDragCaretCustomVisualizer)
  strict private
    FControl: IdxRichEditControl;
  private
    procedure DrawCaret(ACaretLogPosition: TdxDocumentLogPosition);
    function GetCaret: TdxDragCaret;
  protected
    property Control: IdxRichEditControl read FControl;
    property Caret: TdxDragCaret read GetCaret;
  public
    constructor Create(const AControl: IdxRichEditControl);
    procedure Finish; override;
    procedure Start; override;
    procedure ShowCaret(ACaretLogPosition: Integer); override;
    procedure HideCaret(ACaretLogPosition: Integer); override;
  end;

  { TdxBeginMouseDragHelperState }

  TdxBeginMouseDragHelperState = class(TdxRichEditMouseCustomState)
  private
    FCancelOnRightMouseUp: Boolean;
    FCancelOnPopupMenu: Boolean;
    FDragState: IdxRichEditMouseState;
    FInitialPoint: TPoint;
    function IsDragStarted(const Args: TdxMouseEventArgs): Boolean;
  protected
    function GetStopClickTimerOnStart: Boolean; override;
    property InitialPoint: TPoint read FInitialPoint;
  public
    constructor Create(AController: TdxRichEditMouseController; ADragState: TdxRichEditMouseCustomState;
      const P: TPoint); reintroduce; virtual;
    destructor Destroy; override;

    procedure HandleMouseMove(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseUp(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseWheel(const Args: TdxMouseEventArgs); override;
    function HandlePopupMenu(const Args: TdxMouseEventArgs): Boolean; override;

    property CancelOnRightMouseUp: Boolean read FCancelOnRightMouseUp write FCancelOnRightMouseUp;
    property CancelOnPopupMenu: Boolean read FCancelOnPopupMenu write FCancelOnPopupMenu;
    property DragState: IdxRichEditMouseState read FDragState;
  end;

  { TdxRichEditBeginMouseDragHelperState }

  TdxRichEditBeginMouseDragHelperState = class(TdxBeginMouseDragHelperState)
  private
    procedure HandleEndDocumentUpdate(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs);
  public
    procedure Start; override;
    procedure Finish; override;
    procedure HandleMouseWheel(const Args: TdxMouseEventArgs); override;
  end;

  { TdxBeginContentDragHelperState }

  TdxBeginContentDragHelperState = class(TdxRichEditBeginMouseDragHelperState)
  private
    FResetSelectionOnMouseUp: Boolean;
  public
    constructor Create(AController: TdxRichEditMouseController;
      ADragState: TdxRichEditMouseCustomState; const P: TPoint); override;
    procedure HandleMouseUp(const Args: TdxMouseEventArgs); override;

    property ResetSelectionOnMouseUp: Boolean read FResetSelectionOnMouseUp write FResetSelectionOnMouseUp;
  end;

  { TdxBeginMouseDragHyperlinkClickHandleHelperState }

  TdxBeginMouseDragHyperlinkClickHandleHelperState = class(TdxRichEditBeginMouseDragHelperState)
  private
    procedure InternalHandleMouseUp(const Args: TdxMouseEventArgs);
    procedure ValidateCursorPosition(const AControl: IdxRichEditControl);
  public
    procedure HandleMouseUp(const Args: TdxMouseEventArgs); override;
  end;

  { TdxCancellableDragMouseHandlerStateBase }

  TdxCancellableDragMouseHandlerStateBase = class abstract(TdxRichEditMouseCustomState,
    IdxKeyboardHandlerService)
  strict private
    FDataObject: IdxDataObject;
  protected
    function GetAutoScrollEnabled: Boolean; override;
    function CanShowToolTip: Boolean; override;
    function GetStopClickTimerOnStart: Boolean; override;
    function CalculateHitTest(const P: TPoint): TdxRichEditHitTestResult; override;

    function GetDataObject(const AData: IDataObject): IdxDataObject; virtual;
    function GetHitTestDetailsLevel: TdxDocumentLayoutDetailsLevel; virtual; abstract;
    function HandleDragDropManually: Boolean; virtual;

    function CanKeyDownModifyEdit(const Args: TdxKeyEventArgs): Boolean;
    function HandleKeyDown(const Args: TdxKeyEventArgs): Boolean;
    function HandleKeyUp(const Args: TdxKeyEventArgs): Boolean;
    function HandleKeyPress(const Args: TdxKeyPressEventArgs): Boolean;

    procedure BeginVisualFeedback; virtual;
    procedure EndVisualFeedback; virtual;
    procedure ShowVisualFeedback; virtual; abstract;
    procedure HideVisualFeedback; virtual; abstract;

    function CalculateMouseCursor(AShiftState: TShiftState): TCursor; virtual; abstract;
    function CreateDataObject: IdxDataObject; virtual; abstract;
    function CommitDrag(const P: TPoint; AShiftState: TShiftState; const ADataObject: IdxDataObject): Boolean; virtual; abstract;
    function ContinueDrag(const Args: TdxDragEventArgs): TdxDragDropEffects; overload;
    function ContinueDrag(const P: TPoint; const AllowedEffects: TdxDragDropEffects; AShiftState: TShiftState; const ADataObject: IdxDataObject): TdxDragDropEffects; overload; virtual; abstract;

    property DataObject: IdxDataObject read FDataObject;
    property HitTestDetailsLevel: TdxDocumentLayoutDetailsLevel read GetHitTestDetailsLevel;
  public
    procedure Start; override;
    procedure Finish; override;
    procedure HandleMouseMove(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseUp(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseWheel(const Args: TdxMouseEventArgs); override;
  end;

  { TdxDragContentMouseHandlerStateBase }

  TdxDragContentMouseHandlerStateBase = class(TdxCancellableDragMouseHandlerStateBase)
  public const
    RichEditDataFormatSelection: string = 'dxRichEdit.DocumentModel.Selection';
  private
    FCalculator: TdxDragContentMouseHandlerStateCalculator;
    FCaretLogPosition: TdxDocumentLogPosition;
    FCaretVisualizer: TdxRichEditDragCaretVisualizer;
  protected
    function GetHitTestDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure BeginVisualFeedback; override;
    procedure EndVisualFeedback; override;
    procedure ShowVisualFeedback; override;
    procedure HideVisualFeedback; override;

    function CalculateDragDropEffects(ASiftState: TShiftState): TdxDragDropEffects; virtual;
    function CanDropData(const ADataObject: IdxDataObject): Boolean; overload; virtual;
    function CanDropContentTo(AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    function ContinueDrag(const P: TPoint;
      const AllowedEffects: TdxDragDropEffects; AShiftState: TShiftState; const ADataObject: IdxDataObject): TdxDragDropEffects; override;
    function CreateCaretVisualizer: TdxRichEditDragCaretVisualizer; virtual;
    function CreateDropCommand(APos: PdxDocumentModelPosition;
      const ADataObject: IdxDataObject; AIsControlPressed: Boolean): TdxCommand; virtual;
    function GetHitTestDocumentModelPosition(AHitTestResult: TdxRichEditHitTestResult): TdxDocumentModelPosition;
    function ShouldShowVisualFeedback(const P: TPoint): Boolean; virtual;
    function UpdateModelPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    procedure UpdateVisualState; virtual;
    function CalculateMouseCursor(AShiftState: TShiftState): TCursor; override;
    function CommitDrag(const APoint: TPoint; AShiftState: TShiftState; const ADataObject: IdxDataObject): Boolean; override;
    function CreateDataObject: IdxDataObject; override;

    property Calculator: TdxDragContentMouseHandlerStateCalculator read FCalculator;
    property CaretVisualizer: TdxRichEditDragCaretVisualizer read FCaretVisualizer;
  public
    constructor Create(AController: TdxRichEditMouseController); override;
    destructor Destroy; override;

    function CanDropData(var Args: TdxDragEventArgs): Boolean; overload;

    procedure Start; override;
    procedure Finish; override;

    property CaretLogPosition: TdxDocumentLogPosition read FCaretLogPosition write FCaretLogPosition;
  end;

  { TdxDragContentStandardMouseHandlerStateBase }

  TdxDragContentStandardMouseHandlerStateBase = class abstract(TdxDragContentMouseHandlerStateBase)
  protected
    function HandleDragDropManually: Boolean; override;
    procedure SetMouseCursor(ACursor: TCursor); override;

    procedure DoDragOver(var Args: TdxDragEventArgs); override;
    procedure DoDragDrop(var Args: TdxDragEventArgs); override;
    procedure QueryContinueDrag(Args: PdxQueryContinueDragEventArgs); override;

    procedure DoDragDropCore(var Args: TdxDragEventArgs); virtual;
  end;

  { TdxDragContentStandardMouseHandlerState }

  TdxDragContentStandardMouseHandlerState = class(TdxDragContentStandardMouseHandlerStateBase)
  private
    FDragToExternalTarget: Boolean;
  public
    constructor Create(AController: TdxRichEditMouseController); override;
    procedure Start; override;

    procedure DoDragEnter(var Args: TdxDragEventArgs); override;
    procedure DoDragLeave; override;
    procedure QueryContinueDrag(Args: PdxQueryContinueDragEventArgs); override;
  end;

  { TdxDragExternalContentMouseHandlerState }

  TdxDragExternalContentMouseHandlerState = class(TdxDragContentStandardMouseHandlerStateBase)
  protected
    function CalculateDragDropEffects(ASiftState: TShiftState): TdxDragDropEffects; override;
    function CreateDataObject: IdxDataObject; override;
    function ContinueDrag(const P: TPoint;
      const AllowedEffects: TdxDragDropEffects; AShiftState: TShiftState; const ADataObject: IdxDataObject): TdxDragDropEffects; override;
    function CommitDrag(const P: TPoint; AShiftState: TShiftState; const ADataObject: IdxDataObject): Boolean; override;
    function CreateDropCommand(APos: PdxDocumentModelPosition;
      const ADataObject: IdxDataObject; AIsControlPressed: Boolean): TdxCommand; override;
    procedure UpdateTableViewInfoController(const APhysicalPoint: TPoint; AHitTestResult: TdxRichEditHitTestResult); virtual;
  public
    procedure DoDragOver(var Args: TdxDragEventArgs); override;
    procedure DoDragLeave; override;
  end;

  { TdxDragContentManuallyMouseHandlerState }

  TdxDragContentManuallyMouseHandlerState = class(TdxDragContentMouseHandlerStateBase)
  public
    procedure Start; override;
  end;

  { TdxDragContentMouseHandlerStateBaseStrategy }

  TdxDragContentMouseHandlerStateBaseStrategy = class abstract(TdxRichEditMouseHandlerStateStrategyBase)
  private
    function GetState: TdxDragContentMouseHandlerStateBase;
  protected
    function CreateCaretVisualizer: TdxRichEditDragCaretVisualizer; virtual; abstract;
  public
    procedure Finish; virtual; abstract;
    property State: TdxDragContentMouseHandlerStateBase read GetState;
  end;

  { TdxDragFloatingObjectManuallyMouseHandlerStateStrategy }

  TdxDragFloatingObjectManuallyMouseHandlerState = class;

  TdxDragFloatingObjectManuallyMouseHandlerStateStrategy = class abstract(TdxRichEditMouseHandlerStateStrategyBase)
  strict private
    function GetDocumentModel: TdxDocumentModel;
    function GetInitialShapeBounds: TRect;
    function GetInitialContentBounds: TRect;
    function GetRun: TdxFloatingObjectAnchorRun;
  private
    function GetState: TdxDragFloatingObjectManuallyMouseHandlerState;
  protected
    procedure ShowVisualFeedbackCore(const ABounds: TRect; APageViewInfo: TdxPageViewInfo; AImage: TdxOfficeImage); virtual; abstract;
    procedure HideVisualFeedbackCore(const ABounds: TRect; APageViewInfo: TdxPageViewInfo); virtual; abstract;
    function CreateFeedbackImage(AOriginalImage: TdxOfficeImage): TdxOfficeImage; virtual; abstract;
    procedure BeginVisualFeedback; virtual; abstract;
    procedure EndVisualFeedback; virtual; abstract;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property InitialShapeBounds: TRect read GetInitialShapeBounds;
    property InitialContentBounds: TRect read GetInitialContentBounds;
    property Run: TdxFloatingObjectAnchorRun read GetRun;

    property State: TdxDragFloatingObjectManuallyMouseHandlerState read GetState;

  end;

  { TdxDragFloatingObjectManuallyMouseHandlerState }

  TdxDragFloatingObjectManuallyMouseHandlerState = class(TdxCancellableDragMouseHandlerStateBase)
  public const
    Unassigned: TPoint = (X: MinInt; Y: MinInt);
  strict private
    FCurrentTopLeftCorner: TPoint;
    FPageViewInfo: TdxPageViewInfo;
    FInitialLogicalClickPoint: TPoint;
    FImage: TdxOfficeImage;
    FClickPointLogicalOffset: TPoint;
    FCurrentHitTestResult: TdxRichEditHitTestResult;
    FRun: TdxFloatingObjectAnchorRun;
    FFloatingObjectAnchorRunIndex: TdxRunIndex;
    FOldTopLeftCorner: TPoint;
    FInitialShapeBounds: TRect;
    FInitialContentBounds: TRect;
    FMinAffectedRunIndex: TdxRunIndex;
    FPlatformStrategy: TdxDragFloatingObjectManuallyMouseHandlerStateStrategy;
    FRotationAngle: Single;
    FCalculator: TdxDragFloatingObjectMouseHandlerStateCalculator;
  protected
    function GetHitTestDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function CreatePlatformStrategy: TdxDragFloatingObjectManuallyMouseHandlerStateStrategy; virtual;
    function CreateFeedbackImage(AOriginalImage: TdxOfficeImage): TdxOfficeImage; virtual;
    function CalculateMouseCursor(AShiftState: TShiftState): TCursor; override;
    function CreateDataObject: IdxDataObject; override;
    function CalculateHitTestAndCurrentPoint(const APoint: TPoint): Boolean;
    function CalculateMinAffectedRunIndex(APage: TdxPage): TdxRunIndex;
    function CommitDrag(const APoint: TPoint; AShiftState: TShiftState; const ADataObject: IdxDataObject): Boolean; override;
    function ContinueDrag(const P: TPoint;
      const AllowedEffects: TdxDragDropEffects; AShiftState: TShiftState; const ADataObject: IdxDataObject): TdxDragDropEffects; override;
    procedure ShowVisualFeedback; override;
    procedure HideVisualFeedback; override;
    procedure ShowVisualFeedbackCore(const ABounds: TRect; APageViewInfo: TdxPageViewInfo; AImage: TdxOfficeImage); virtual;
    procedure HideVisualFeedbackCore(const ABounds: TRect; APageViewInfo: TdxPageViewInfo); virtual;
    procedure BeginVisualFeedback; override;
    procedure EndVisualFeedback; override;
  public
    constructor Create(AController: TdxRichEditMouseController; AHitTestResult: TdxRichEditHitTestResult); reintroduce;
    destructor Destroy; override;
    function CreateVisualFeedbackTransform: TdxTransformMatrix; virtual;
    procedure Start; override;

    property FeedbackImage: TdxOfficeImage read FImage;
    property InitialShapeBounds: TRect read FInitialShapeBounds;
    property InitialContentBounds: TRect read FInitialContentBounds;
    property Run: TdxFloatingObjectAnchorRun read FRun;
    property RotationAngle: Single read FRotationAngle;
  end;

implementation

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Math,
  cxGeometry, cxControls, Windows, dxTypeHelpers, dxCoreClasses,
  dxRichEdit.Options,
  dxRichEdit.Commands,
  dxRichEdit.Commands.DragAndDrop,
  dxRichEdit.Commands.CopyAndPaste,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.Utils.Cursors,
  dxEncoding,
  dxRichEdit.Platform.Win.Strategies,
  dxRichEdit.DocumentModel.ParagraphFormatting;

type
  { TdxRichEditCursorsHelper }

  TdxRichEditCursorsHelper = class helper for TdxRichEditCursors
  public
    class function GetCursor(AEffect: TdxDragDropEffects): TCursor; static;
  end;

{ TdxRichEditCursorsHelper }

class function TdxRichEditCursorsHelper.GetCursor(AEffect: TdxDragDropEffects): TCursor;
begin
  if TdxDragDropEffect.Scroll in AEffect then
    Result := TdxRichEditCursors.Hand
  else
  if TdxDragDropEffect.Link in AEffect then
    Result := TdxRichEditCursors.Hand
  else
  if TdxDragDropEffect.Copy in AEffect then
    Result := TdxRichEditCursors.Hand
  else
  if TdxDragDropEffect.Move in AEffect then
    Result := TdxRichEditCursors.Hand
  else
    Result := TdxRichEditCursors.Default;
end;

{ TdxRichEditDragCaretCustomVisualizer }

procedure TdxRichEditDragCaretCustomVisualizer.Finish;
begin
end;

procedure TdxRichEditDragCaretCustomVisualizer.Start;
begin
end;

{ TdxRichEditDragCaretVisualizer }

constructor TdxRichEditDragCaretVisualizer.Create(const AControl: IdxRichEditControl);
begin
  inherited Create;
  FControl := AControl;
end;

procedure TdxRichEditDragCaretVisualizer.DrawCaret(
  ACaretLogPosition: TdxDocumentLogPosition);
begin
  Caret.SetLogPosition(ACaretLogPosition);
  Control.DrawDragCaret;
end;

procedure TdxRichEditDragCaretVisualizer.Finish;
begin
  Control.DestroyDragCaret;
  inherited Finish;
end;

function TdxRichEditDragCaretVisualizer.GetCaret: TdxDragCaret;
begin
  Result := Control.DragCaret;
end;

procedure TdxRichEditDragCaretVisualizer.HideCaret(ACaretLogPosition: Integer);
begin
  if not Caret.IsHidden then
  begin
    DrawCaret(ACaretLogPosition);
    Caret.IsHidden := True;
  end;
end;

procedure TdxRichEditDragCaretVisualizer.ShowCaret(ACaretLogPosition: Integer);
begin
  if Caret.IsHidden then
  begin
    DrawCaret(ACaretLogPosition);
    Caret.IsHidden := False;
  end;
end;

procedure TdxRichEditDragCaretVisualizer.Start;
begin
  Control.CreateDragCaret;
  inherited Start;
end;

{ TdxBeginMouseDragHelperState }

constructor TdxBeginMouseDragHelperState.Create(
  AController: TdxRichEditMouseController;
  ADragState: TdxRichEditMouseCustomState; const P: TPoint);
begin
  inherited Create(AController);
  FDragState := ADragState;
  FInitialPoint := P;
end;

destructor TdxBeginMouseDragHelperState.Destroy;
begin
  FDragState := nil;
  inherited Destroy;
end;

procedure TdxBeginMouseDragHelperState.HandleMouseMove(
  const Args: TdxMouseEventArgs);
begin
  if IsDragStarted(Args) then
  begin
    LockRelease;
    try
      Controller.SwitchStateCore(DragState.State, Args.MousePos);
      DragState.HandleMouseMove(Args);
    finally
      UnlockRelease;
    end;
  end;
end;

procedure TdxBeginMouseDragHelperState.HandleMouseUp(const Args: TdxMouseEventArgs);
begin
  if (mbLeft in Args.Buttons) or ((mbRight in Args.Buttons) and CancelOnRightMouseUp) then
    Controller.SwitchToDefaultState;
end;

procedure TdxBeginMouseDragHelperState.HandleMouseWheel(const Args: TdxMouseEventArgs);
begin
  Controller.SwitchStateCore(DragState.State, cxNullPoint);
  DragState.HandleMouseWheel(Args);
end;

function TdxBeginMouseDragHelperState.HandlePopupMenu(const Args: TdxMouseEventArgs): Boolean;
begin
  if CancelOnPopupMenu then
  begin
    Controller.SwitchToDefaultState;
    Result := False;
  end
  else
    Result := inherited HandlePopupMenu(Args);
end;

function TdxBeginMouseDragHelperState.IsDragStarted(const Args: TdxMouseEventArgs): Boolean;
var
  ADragSize: TSize;
begin
  ADragSize := Controller.DragSize;
  Result := (Abs(InitialPoint.X - Args.MousePos.X) > ADragSize.cx) or (Abs(InitialPoint.Y - Args.MousePos.Y) > ADragSize.cy);
end;

function TdxBeginMouseDragHelperState.GetStopClickTimerOnStart: Boolean;
begin
  Result := False;
end;

{ TdxRichEditBeginMouseDragHelperState }

procedure TdxRichEditBeginMouseDragHelperState.Start;
begin
  inherited Start;
  DocumentModel.EndDocumentUpdate.Add(HandleEndDocumentUpdate);
end;

procedure TdxRichEditBeginMouseDragHelperState.Finish;
begin
  inherited Finish;
  DocumentModel.EndDocumentUpdate.Remove(HandleEndDocumentUpdate);
end;

procedure TdxRichEditBeginMouseDragHelperState.HandleMouseWheel(const Args: TdxMouseEventArgs);
begin
  Controller.SwitchStateCore(DragState.State, InitialPoint);
  DragState.HandleMouseMove(Args);
end;

procedure TdxRichEditBeginMouseDragHelperState.HandleEndDocumentUpdate(ASender: TObject;
  E: TdxDocumentUpdateCompleteEventArgs);
var
  AHandler: IdxEndDocumentUpdateHandler;
begin
  if Supports(DragState, IdxEndDocumentUpdateHandler, AHandler) then
    AHandler.HandleEndDocumentUpdate(E);
end;

{ TdxBeginContentDragHelperState }

constructor TdxBeginContentDragHelperState.Create(AController: TdxRichEditMouseController;
  ADragState: TdxRichEditMouseCustomState; const P: TPoint);
begin
  inherited Create(AController, ADragState, P);
  FResetSelectionOnMouseUp := True;
end;

procedure TdxBeginContentDragHelperState.HandleMouseUp(
  const Args: TdxMouseEventArgs);
var
  ACommand: TdxPlaceCaretToPhysicalPointCommand;
begin
  inherited HandleMouseUp(Args);
  if ResetSelectionOnMouseUp then
  begin
    ACommand := TdxPlaceCaretToPhysicalPointCommand2.Create(Control);
    try
      ACommand.PhysicalPoint := Args.MousePos;
      Control.InnerControl.DocumentModel.Selection.ClearMultiSelection;
      ACommand.ExecuteCore;
    finally
      ACommand.Free;
    end;
  end;
end;

{ TdxBeginMouseDragHyperlinkClickHandleHelperState }

procedure TdxBeginMouseDragHyperlinkClickHandleHelperState.HandleMouseUp(
  const Args: TdxMouseEventArgs);
begin
  LockRelease;
  try
    inherited HandleMouseUp(Args);
    InternalHandleMouseUp(Args);
  finally
    UnlockRelease;
  end;
end;

procedure TdxBeginMouseDragHyperlinkClickHandleHelperState.InternalHandleMouseUp(
  const Args: TdxMouseEventArgs);
var
  AHandler: TdxHyperlinkMouseClickHandler;
begin
  ValidateCursorPosition(Control);
  AHandler := TdxHyperlinkMouseClickHandler.Create(Control);
  try
    AHandler.HandleMouseUp(Args);
  finally
    AHandler.Free;
  end;
end;

procedure TdxBeginMouseDragHyperlinkClickHandleHelperState.ValidateCursorPosition(
  const AControl: IdxRichEditControl);
var
  ASelection: TdxSelection;
  ACursorPos, ARunStartPosition: TdxDocumentLogPosition;
  ARunIndex: TdxRunIndex;
  AParagraphIndex: TdxParagraphIndex;
  ARun: TdxTextRunBase;
  ATextFilter: IdxVisibleTextFilter;
begin
  ASelection := DocumentModel.Selection;
  if ASelection.Length <> 0 then
    Exit;
  ACursorPos := ASelection.Start;
  if ACursorPos <= ActivePieceTable.DocumentStartLogPosition then
    Exit;
  AParagraphIndex := ActivePieceTable.FindParagraphIndex(ACursorPos);
  ARunStartPosition := ActivePieceTable.FindRunStartLogPosition(
    ActivePieceTable.Paragraphs[AParagraphIndex], ACursorPos, ARunIndex);
  if (ARunStartPosition <> ACursorPos) or (ARunIndex = 0) then
    Exit;
  ARun := ActivePieceTable.Runs[ARunIndex - 1];
  if not (ARun is TdxFieldCodeRunBase) then
    Exit;
  ATextFilter := ActivePieceTable.VisibleTextFilter;
  ACursorPos := ATextFilter.GetPrevVisibleLogPosition(ACursorPos + 1, True);
  if (ACursorPos < ActivePieceTable.DocumentStartLogPosition) or (ACursorPos = ASelection.Start) then
    Exit;
  AControl.BeginUpdate;
  try
    ASelection.Start := ACursorPos;
    ASelection.&End := ACursorPos;
  finally
    AControl.EndUpdate;
  end;
end;

{ TdxCancellableDragMouseHandlerStateBase }

function TdxCancellableDragMouseHandlerStateBase.GetAutoScrollEnabled: Boolean;
begin
  Result := True;
end;

procedure TdxCancellableDragMouseHandlerStateBase.BeginVisualFeedback;
begin
end;

function TdxCancellableDragMouseHandlerStateBase.CalculateHitTest(
  const P: TPoint): TdxRichEditHitTestResult;
var
  ARequest: TdxRichEditHitTestRequest;
begin
  ARequest := TdxRichEditHitTestRequest.Create(DocumentModel.ActivePieceTable);
  ARequest.PhysicalPoint := P;
  ARequest.DetailsLevel := HitTestDetailsLevel;
  ARequest.Accuracy := Controller.ActiveView.DefaultHitTestPageAccuracy or NearestPageArea or
    NearestColumn or NearestTableRow or NearestTableCell or NearestRow or NearestBox or NearestCharacter;

  Result := Control.InnerControl.ActiveView.HitTestCore(ARequest, True);
  if not Result.IsValid(HitTestDetailsLevel) then
    FreeAndNil(Result);
end;

function TdxCancellableDragMouseHandlerStateBase.GetDataObject(const AData: IDataObject): IdxDataObject;
begin
  if AData = nil then
    Result := nil
  else
    if not Supports(AData, IdxDataObject, Result) then
      Result := TdxDragAndDropExternalDataObject.Create(AData);
end;

function TdxCancellableDragMouseHandlerStateBase.CanShowToolTip: Boolean;
begin
  Result := True;
end;

procedure TdxCancellableDragMouseHandlerStateBase.EndVisualFeedback;
begin
end;

function TdxCancellableDragMouseHandlerStateBase.ContinueDrag(const Args: TdxDragEventArgs): TdxDragDropEffects;
var
  ADataObject: IdxDataObject;
begin
  ADataObject := GetDataObject(Args.Data);
  try
    Result := ContinueDrag(Args.P, Args.AllowedEffect, Args.KeyState, ADataObject);
  finally
    ADataObject := nil;
  end;
end;

procedure TdxCancellableDragMouseHandlerStateBase.Finish;
begin
  EndVisualFeedback;
  Controller.Control.RemoveKeyboardService(Self);
  inherited Finish;
end;

function TdxCancellableDragMouseHandlerStateBase.HandleDragDropManually: Boolean;
begin
  Result := True;
end;

function TdxCancellableDragMouseHandlerStateBase.CanKeyDownModifyEdit(
  const Args: TdxKeyEventArgs): Boolean;
begin
  Result := False;
end;

function TdxCancellableDragMouseHandlerStateBase.HandleKeyDown(const Args: TdxKeyEventArgs): Boolean;
begin
  Result := False;
  if Args.KeyData = VK_ESCAPE then
  begin
    Result := True;
    HideVisualFeedback;
    Controller.SwitchToDefaultState;
    Controller.State.HandleMouseMove(Controller.CreateFakeMouseMoveEventArgs);
  end;
  if Args.Control then
    SetMouseCursor(CalculateMouseCursor(Args.ShiftState));
end;

function TdxCancellableDragMouseHandlerStateBase.HandleKeyUp(const Args: TdxKeyEventArgs): Boolean;
begin
  SetMouseCursor(CalculateMouseCursor(Args.ShiftState));
  Result := False;
end;

function TdxCancellableDragMouseHandlerStateBase.HandleKeyPress(const Args: TdxKeyPressEventArgs): Boolean;
begin
  Result := False;
end;

procedure TdxCancellableDragMouseHandlerStateBase.HandleMouseMove(
  const Args: TdxMouseEventArgs);
begin
  if HandleDragDropManually then
    ContinueDrag(Args.MousePos, [TdxDragDropEffect.Move, TdxDragDropEffect.Copy], Args.Shift, DataObject);
end;

procedure TdxCancellableDragMouseHandlerStateBase.HandleMouseUp(
  const Args: TdxMouseEventArgs);
begin
  if not HandleDragDropManually then
    Exit;
  HideVisualFeedback;
  CommitDrag(Args.MousePos, Args.Shift, nil);
  Controller.SwitchToDefaultState;
end;

procedure TdxCancellableDragMouseHandlerStateBase.HandleMouseWheel(
  const Args: TdxMouseEventArgs);
begin
  inherited HandleMouseWheel(Args);
  ContinueDrag(Args.MousePos, [TdxDragDropEffect.Move, TdxDragDropEffect.Copy], Args.Shift, nil);
end;

procedure TdxCancellableDragMouseHandlerStateBase.Start;
begin
  inherited Start;
  Controller.Control.AddKeyboardService(Self);
  FDataObject := CreateDataObject;
  BeginVisualFeedback;
end;

function TdxCancellableDragMouseHandlerStateBase.GetStopClickTimerOnStart: Boolean;
begin
  Result := False;
end;

{ TdxDragContentMouseHandlerStateBase }

constructor TdxDragContentMouseHandlerStateBase.Create(
  AController: TdxRichEditMouseController);
begin
  inherited Create(AController);
  FCaretVisualizer := CreateCaretVisualizer;
  FCalculator := Controller.CreateDragContentMouseHandlerStateCalculator;
end;

destructor TdxDragContentMouseHandlerStateBase.Destroy;
begin
  FreeAndNil(FCalculator);
  FreeAndNil(FCaretVisualizer);
  inherited Destroy;
end;

function TdxDragContentMouseHandlerStateBase.CanDropData(var Args: TdxDragEventArgs): Boolean;
var
  AData: IdxDataObject;
begin
  AData := GetDataObject(Args.Data);
  try
    Result := CanDropData(AData);
  finally
    AData := nil;
  end;
end;

function TdxDragContentMouseHandlerStateBase.CreateCaretVisualizer: TdxRichEditDragCaretVisualizer;
begin
  Result := TdxRichEditDragCaretVisualizer.Create(Control);
end;

function TdxDragContentMouseHandlerStateBase.CreateDropCommand(APos: PdxDocumentModelPosition;
  const ADataObject: IdxDataObject; AIsControlPressed: Boolean): TdxCommand;
begin
  if AIsControlPressed then
    Result := TdxDragCopyContentCommand.Create(Control, APos)
  else
    Result := TdxDragMoveContentCommand.Create(Control, APos);
end;

function TdxDragContentMouseHandlerStateBase.CalculateDragDropEffects(ASiftState: TShiftState): TdxDragDropEffects;
begin
  if ssCtrl in ASiftState then
    Result := [TdxDragDropEffect.Copy]
  else
    Result := [TdxDragDropEffect.Move];
end;

function TdxDragContentMouseHandlerStateBase.CanDropContentTo(
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
begin
  Result := FCalculator.CanDropContentTo(AHitTestResult, DocumentModel.ActivePieceTable);
end;

function TdxDragContentMouseHandlerStateBase.CanDropData(
  const ADataObject: IdxDataObject): Boolean;
var
  ACommand: TdxRichEditCommand;
begin
  if ADataObject = nil then
    Result := False
  else
  begin
    ACommand := TdxPasteDataObjectCoreCommand.Create(Control, ADataObject);
    try
      Result := ACommand.CanExecute;
    finally
      ACommand.Free;
    end;
  end;
end;

function TdxDragContentMouseHandlerStateBase.ContinueDrag(const P: TPoint;
  const AllowedEffects: TdxDragDropEffects; AShiftState: TShiftState;
  const ADataObject: IdxDataObject): TdxDragDropEffects;
var
  AHitTestResult: TdxRichEditHitTestResult;
  APos: TdxDocumentModelPosition;
  AShouldShowVisualFeedback: Boolean;
begin
  AHitTestResult := CalculateHitTest(P);
  try
    if not CanDropData(ADataObject) or (AHitTestResult = nil) or not CanDropContentTo(AHitTestResult) then
    begin
      HideVisualFeedback;
      SetMouseCursor(TdxRichEditCursors.GetCursor([]));
      Exit([TdxDragDropEffect.None]);
    end;
    Result := CalculateDragDropEffects(AShiftState) * AllowedEffects;
    APos := GetHitTestDocumentModelPosition(AHitTestResult);
    AShouldShowVisualFeedback := ShouldShowVisualFeedback(AHitTestResult.LogicalPoint);
    if APos.LogPosition = CaretLogPosition then
    begin
      if AShouldShowVisualFeedback then
        ShowVisualFeedback
      else
        HideVisualFeedback;
      Exit;
    end;
    HideVisualFeedback;
    FCaretLogPosition := APos.LogPosition;
    UpdateVisualState;
    if AShouldShowVisualFeedback then
      ShowVisualFeedback;
    SetMouseCursor(TdxRichEditCursors.GetCursor(Result));
  finally
    AHitTestResult.Free;
  end;
end;

procedure TdxDragContentMouseHandlerStateBase.BeginVisualFeedback;
begin
  CaretVisualizer.Start;
end;

procedure TdxDragContentMouseHandlerStateBase.EndVisualFeedback;
begin
  CaretVisualizer.Finish;
end;

procedure TdxDragContentMouseHandlerStateBase.Finish;
begin
  inherited Finish;
  Control.Control.Invalidate;
end;

function TdxDragContentMouseHandlerStateBase.CalculateMouseCursor(AShiftState: TShiftState): TCursor;
begin
  Result := TdxRichEditCursors.GetCursor(CalculateDragDropEffects(AShiftState));
end;

function TdxDragContentMouseHandlerStateBase.CommitDrag(const APoint: TPoint;
  AShiftState: TShiftState; const ADataObject: IdxDataObject): Boolean;
var
  AHitTestResult: TdxRichEditHitTestResult;
  APos: TdxDocumentModelPosition;
  ACommand: TdxCommand;
begin
  AHitTestResult := CalculateHitTest(APoint);
  try
    Result := (AHitTestResult <> nil) and CanDropContentTo(AHitTestResult);
    if Result then
    begin
      HideVisualFeedback;
      APos := GetHitTestDocumentModelPosition(AHitTestResult);
      ACommand := CreateDropCommand(@APos, ADataObject, ssCtrl in AShiftState);
      try
        ACommand.Execute;
      finally
        ACommand.Free;
      end;
    end;
  finally
    AHitTestResult.Free;
  end;
end;

function TdxDragContentMouseHandlerStateBase.CreateDataObject: IdxDataObject;
var
  AManager: TdxCopySelectionManager;
  ASelection: TdxSelection;
  AOptions: TdxRtfDocumentExporterOptions;
  ARtf: TArray<Byte>;
  AData: string;
  ASelections: TdxSelectionRangeCollection;
begin
  AManager := TdxCopySelectionManager.Create(Control.InnerControl);
  try
    ASelection := DocumentModel.Selection;
    Result := TdxDragAndDropDataObject.Create;
    Result.Open;
    try
      ASelections := ASelection.GetSortedSelectionCollection;
      try
        AOptions := TdxRtfDocumentExporterOptions.Create;
        try
          AOptions.ExportFinalParagraphMark := TdxExportFinalParagraphMark.Never;
          AData := AManager.GetRtfText(ASelection.PieceTable, ASelections, AOptions, True, True);
          ARtf := TdxEncoding.ANSI.GetBytes(AData);
          Result.SetData(TdxOfficeDataFormats.Rtf, ARtf);
          AData := AManager.GetPlainText(ASelection.PieceTable, ASelections);
          Result.SetData(TdxOfficeDataFormats.UnicodeText, AData);
          AData := IntToStr(Control.Control.GetHashCode);
          Result.SetData(RichEditDataFormatSelection, AData);
          AData := AManager.GetSuppressStoreImageSizeCollection(ASelection.PieceTable, ASelections);
          Result.SetData(TdxOfficeDataFormats.SuppressStoreImageSize, AData);
        finally
          AOptions.Free;
        end;
      finally
        ASelections.Free;
      end;
    finally
      Result.Close;
    end;
  finally
    AManager.Free;
  end;
end;

function TdxDragContentMouseHandlerStateBase.GetHitTestDocumentModelPosition(
  AHitTestResult: TdxRichEditHitTestResult): TdxDocumentModelPosition;
begin
  if AHitTestResult.Character <> nil then
    Result := AHitTestResult.Character.GetFirstPosition(AHitTestResult.PieceTable)
  else
    Result := AHitTestResult.Box.GetFirstPosition(AHitTestResult.PieceTable);
  Result := UpdateModelPosition(Result);
end;

procedure TdxDragContentMouseHandlerStateBase.HideVisualFeedback;
begin
  CaretVisualizer.HideCaret(CaretLogPosition);
end;

function TdxDragContentMouseHandlerStateBase.GetHitTestDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Character;
end;

function TdxDragContentMouseHandlerStateBase.ShouldShowVisualFeedback(
  const P: TPoint): Boolean;
begin
  Result := True;
end;

procedure TdxDragContentMouseHandlerStateBase.ShowVisualFeedback;
begin
  CaretVisualizer.ShowCaret(CaretLogPosition);
end;

procedure TdxDragContentMouseHandlerStateBase.Start;
begin
  inherited Start;
  FCaretLogPosition := -1;
end;

function TdxDragContentMouseHandlerStateBase.UpdateModelPosition(
  const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := FCalculator.UpdateDocumentModelPosition(APos);
end;

procedure TdxDragContentMouseHandlerStateBase.UpdateVisualState;
begin
  FCalculator.UpdateVisualState;
end;

{ TdxDragContentStandardMouseHandlerStateBase }

function TdxDragContentStandardMouseHandlerStateBase.HandleDragDropManually: Boolean;
begin
  Result := False;
end;

procedure TdxDragContentStandardMouseHandlerStateBase.SetMouseCursor(
  ACursor: TCursor);
begin
end;

procedure TdxDragContentStandardMouseHandlerStateBase.DoDragOver(var Args: TdxDragEventArgs);
begin
  Args.Effect := ContinueDrag(Args);
end;

procedure TdxDragContentStandardMouseHandlerStateBase.DoDragDrop(var Args: TdxDragEventArgs);
begin
  inherited DoDragDrop(Args);
  DoDragDropCore(Args);
  Controller.SwitchToDefaultState;
end;

procedure TdxDragContentStandardMouseHandlerStateBase.QueryContinueDrag(Args: PdxQueryContinueDragEventArgs);
var
  AIsCommandDisabled: Boolean;
begin
  AIsCommandDisabled := not InnerControl.Options.Behavior.DragAllowed;
  if Args.EscapePressed or AIsCommandDisabled then
  begin
    Args.Action := TdxDragAction.Cancel;
    Controller.SwitchToDefaultState;
  end;
end;

procedure TdxDragContentStandardMouseHandlerStateBase.DoDragDropCore(var Args: TdxDragEventArgs);
var
  ADataObject: IdxDataObject;
begin
  if Control.InnerControl.Options.Behavior.DropAllowed then
  begin
    ADataObject := GetDataObject(Args.Data);
    try
      CommitDrag(Args.P, Args.KeyState, ADataObject);
    finally
      ADataObject := nil;
    end;
  end;
end;

{ TdxDragContentStandardMouseHandlerState }

constructor TdxDragContentStandardMouseHandlerState.Create(
  AController: TdxRichEditMouseController);
begin
  inherited Create(AController);
  FDragToExternalTarget := True;
end;

procedure TdxDragContentStandardMouseHandlerState.Start;
begin
  inherited Start;
  Calculator.OnInternalDragStart;
  Control.DoDragDrop(DataObject, [TdxDragDropEffect.Move, TdxDragDropEffect.Copy]);
end;

procedure TdxDragContentStandardMouseHandlerState.DoDragEnter(var Args: TdxDragEventArgs);
begin
  FDragToExternalTarget := False;
  Args.Effect := ContinueDrag(Args);
end;

procedure TdxDragContentStandardMouseHandlerState.DoDragLeave;
begin
  FDragToExternalTarget := True;
  inherited DoDragLeave;
  Controller.SwitchToDefaultState;
end;

procedure TdxDragContentStandardMouseHandlerState.QueryContinueDrag(Args: PdxQueryContinueDragEventArgs);
begin
  inherited QueryContinueDrag(Args);
  if (Args.Action = TdxDragAction.Drop) and FDragToExternalTarget then
    Controller.SwitchToDefaultState;
end;

{ TdxDragExternalContentMouseHandlerState }

procedure TdxDragExternalContentMouseHandlerState.DoDragOver(var Args: TdxDragEventArgs);
var
  APhysicalPoint: TPoint;
  AHitTestResult: TdxRichEditHitTestResult;
begin
  inherited DoDragOver(Args);
  APhysicalPoint := Args.P;
  AHitTestResult := CalculateHitTest(APhysicalPoint);
  try
    if (AHitTestResult <> nil) and (AHitTestResult.TableCell <> nil) then
      Controller.TableViewInfo := AHitTestResult.TableCell.TableViewInfo;
    if UseHover then
    begin
      UpdateHover(AHitTestResult);
      UpdateTableViewInfoController(APhysicalPoint, AHitTestResult);
    end;
  finally
    AHitTestResult.Free;
  end;
end;

procedure TdxDragExternalContentMouseHandlerState.DoDragLeave;
begin
  Controller.SwitchToDefaultState;
  Controller.TableViewInfo := nil;
end;

function TdxDragExternalContentMouseHandlerState.CalculateDragDropEffects(ASiftState: TShiftState): TdxDragDropEffects;
begin
  Result := [TdxDragDropEffect.Copy];
end;

function TdxDragExternalContentMouseHandlerState.CreateDataObject: IdxDataObject;
begin
  Result := nil;
end;

function TdxDragExternalContentMouseHandlerState.ContinueDrag(const P: TPoint;
  const AllowedEffects: TdxDragDropEffects; AShiftState: TShiftState;
  const ADataObject: IdxDataObject): TdxDragDropEffects;
var
  ACommand: TdxPasteLoadDocumentFromFileCommand;
begin
  Result := inherited ContinueDrag(P, AllowedEffects, AShiftState, ADataObject);
  if Result = [] then
  begin
    ACommand := TdxPasteLoadDocumentFromFileCommand.Create(Control);
    try
      ACommand.PasteSource := TdxDataObjectPasteSource.Create(ADataObject);
      if ACommand.CanExecute then
        Result := CalculateDragDropEffects(AShiftState);
    finally
      ACommand.Free;
    end;
  end;
end;

function TdxDragExternalContentMouseHandlerState.CommitDrag(const P: TPoint; AShiftState: TShiftState;
  const ADataObject: IdxDataObject): Boolean;
var
  ACommand: TdxPasteLoadDocumentFromFileCommand;
begin
  Result := inherited CommitDrag(P, AShiftState, ADataObject);
  if not Result then
  begin
    ACommand := TdxPasteLoadDocumentFromFileCommand.Create(Control);
    try
      ACommand.PasteSource := TdxDataObjectPasteSource.Create(ADataObject);
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
end;

function TdxDragExternalContentMouseHandlerState.CreateDropCommand(APos: PdxDocumentModelPosition;
  const ADataObject: IdxDataObject; AIsControlPressed: Boolean): TdxCommand;
begin
  Result := TdxDragCopyExternalContentCommand.Create(Control, APos, ADataObject);
end;

procedure TdxDragExternalContentMouseHandlerState.UpdateTableViewInfoController(const APhysicalPoint: TPoint;
  AHitTestResult: TdxRichEditHitTestResult);
var
  AController: TdxTableViewInfoController;
begin
  AController := Control.InnerControl.ActiveView.TableController;
  if AController <> nil then
    AController.Update(APhysicalPoint, AHitTestResult);
end;

{ TdxDragContentManuallyMouseHandlerState }

procedure TdxDragContentManuallyMouseHandlerState.Start;
begin
  inherited Start;
  Calculator.OnInternalDragStart;
end;

{ TdxDragContentMouseHandlerStateBaseStrategy }

function TdxDragContentMouseHandlerStateBaseStrategy.GetState: TdxDragContentMouseHandlerStateBase;
begin
  Result := TdxDragContentMouseHandlerStateBase(inherited State);
end;

{ TdxDragFloatingObjectManuallyMouseHandlerStateStrategy }

function TdxDragFloatingObjectManuallyMouseHandlerStateStrategy.GetDocumentModel: TdxDocumentModel;
begin
  Result := State.DocumentModel;
end;

function TdxDragFloatingObjectManuallyMouseHandlerStateStrategy.GetInitialShapeBounds: TRect;
begin
  Result := State.InitialShapeBounds;
end;

function TdxDragFloatingObjectManuallyMouseHandlerStateStrategy.GetInitialContentBounds: TRect;
begin
  Result := State.InitialContentBounds;
end;

function TdxDragFloatingObjectManuallyMouseHandlerStateStrategy.GetRun: TdxFloatingObjectAnchorRun;
begin
  Result := State.Run;
end;

function TdxDragFloatingObjectManuallyMouseHandlerStateStrategy.GetState: TdxDragFloatingObjectManuallyMouseHandlerState;
begin
  Result := TdxDragFloatingObjectManuallyMouseHandlerState(inherited State);
end;

{ TdxDragFloatingObjectManuallyMouseHandlerState }

constructor TdxDragFloatingObjectManuallyMouseHandlerState.Create(AController: TdxRichEditMouseController;
  AHitTestResult: TdxRichEditHitTestResult);
var
  ABox: TdxFloatingObjectBox;
begin
  inherited Create(AController);
  ABox := AHitTestResult.FloatingObjectBox;
  Assert(ABox <> nil);
  FPlatformStrategy := CreatePlatformStrategy;
  FInitialLogicalClickPoint := AHitTestResult.LogicalPoint;
  FOldTopLeftCorner := ABox.Bounds.Location;
  FClickPointLogicalOffset.Init(FOldTopLeftCorner.X - FInitialLogicalClickPoint.X, FOldTopLeftCorner.Y - FInitialLogicalClickPoint.Y);
  FInitialShapeBounds := ABox.Bounds;
  FInitialContentBounds := ABox.ContentBounds;
  FMinAffectedRunIndex := CalculateMinAffectedRunIndex(AHitTestResult.Page);
  FCalculator := AController.CreateDragFloatingObjectMouseHandlerStateCalculator;
  FRotationAngle := DocumentModel.GetBoxEffectiveRotationAngleInDegrees(ABox);
end;

destructor TdxDragFloatingObjectManuallyMouseHandlerState.Destroy;
begin
  FreeAndNil(FCurrentHitTestResult);
  FreeAndNil(FCalculator);
  FreeAndNil(FPlatformStrategy);
  inherited Destroy;
end;

function TdxDragFloatingObjectManuallyMouseHandlerState.GetHitTestDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxDragFloatingObjectManuallyMouseHandlerState.CreatePlatformStrategy: TdxDragFloatingObjectManuallyMouseHandlerStateStrategy;
begin
  Result := TdxWinFormsDragFloatingObjectManuallyMouseHandlerStateStrategy.Create(Self);
end;

procedure TdxDragFloatingObjectManuallyMouseHandlerState.Start;
var
  ASelection: TdxSelection;
  ARunInfo: TdxRunInfo;
  ASelectionRun: TdxTextRunBase;
  AContent: TdxPictureFloatingObjectContent;
begin
  if Control.InnerControl.IsEditable then
  begin
    FCurrentTopLeftCorner := Unassigned;
    ASelection := DocumentModel.Selection;
    Assert(ASelection.Length = 1);

    ARunInfo := ASelection.PieceTable.FindRunInfo(ASelection.NormalizedStart, ASelection.Length);
    try
      FFloatingObjectAnchorRunIndex := ARunInfo.Start.RunIndex;
    finally
      ARunInfo.Free;
    end;

    ASelectionRun := ASelection.PieceTable.Runs[FFloatingObjectAnchorRunIndex];
    FRun := TdxFloatingObjectAnchorRun(ASelectionRun);

    AContent := Safe<TdxPictureFloatingObjectContent>.Cast(FRun.Content);
    if AContent <> nil then
      FImage := CreateFeedbackImage(AContent.Image.Image)
    else
      FImage := CreateFeedbackImage(nil);
    FCalculator.Init(FRun, FClickPointLogicalOffset);
  end;
  inherited Start;
end;

function TdxDragFloatingObjectManuallyMouseHandlerState.CreateFeedbackImage(AOriginalImage: TdxOfficeImage): TdxOfficeImage;
begin
  Result := FPlatformStrategy.CreateFeedbackImage(AOriginalImage);
end;

function TdxDragFloatingObjectManuallyMouseHandlerState.CalculateMouseCursor(AShiftState: TShiftState): TCursor;
begin
  Result := TdxRichEditCursors.Default;
end;

function TdxDragFloatingObjectManuallyMouseHandlerState.CreateDataObject: IdxDataObject;
begin
  Result := nil;
end;

function TdxDragFloatingObjectManuallyMouseHandlerState.CalculateHitTestAndCurrentPoint(const APoint: TPoint): Boolean;
var
  ARequest: TdxRichEditHitTestRequest;
begin
  ARequest := TdxRichEditHitTestRequest.Create(DocumentModel.ActivePieceTable);
  ARequest.PhysicalPoint := APoint;
  ARequest.Accuracy := NearestPage or NearestPageArea or NearestColumn or NearestRow or NearestBox or NearestTableRow or
    NearestTableCell or NearestCharacter;
  ARequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Column;

  FPageViewInfo := Control.InnerControl.ActiveView.GetPageViewInfoFromPoint(APoint, False);

  FreeAndNil(FCurrentHitTestResult);
  FCurrentHitTestResult := Control.InnerControl.ActiveView.HitTestCore(ARequest, False);
  if FCurrentHitTestResult = nil then
    Exit(False);

  FCurrentTopLeftCorner := FCurrentHitTestResult.LogicalPoint;
  FCurrentTopLeftCorner.X := FCurrentTopLeftCorner.X + FClickPointLogicalOffset.X;
  FCurrentTopLeftCorner.Y := FCurrentTopLeftCorner.Y + FClickPointLogicalOffset.Y;
  Result := True;
end;

function TdxDragFloatingObjectManuallyMouseHandlerState.CalculateMinAffectedRunIndex(APage: TdxPage): TdxRunIndex;
var
  ASelection: TdxSelection;
  ARunInfo: TdxRunInfo;
  AParagraph: TdxSimpleParagraph;
  APieceTable: TdxPieceTable;
  AHitTestRequest: TdxRichEditHitTestRequest;
  APageController: TdxPageController;
  AHitTestResult: TdxRichEditHitTestResult;
  ACalculator: TdxBoxHitTestCalculator;
begin
  ASelection := DocumentModel.Selection;
  Assert(ASelection.Length = 1);

  ARunInfo := ASelection.PieceTable.FindRunInfo(ASelection.NormalizedStart, ASelection.Length);
  try
    FRun := Safe<TdxFloatingObjectAnchorRun>.Cast(ASelection.PieceTable.Runs[ARunInfo.Start.RunIndex]);
    AParagraph := ASelection.PieceTable.Runs[ARunInfo.Start.RunIndex].Paragraph;
  finally
    ARunInfo.Free;
  end;

  APieceTable := TdxPieceTable(FRun.PieceTable);

  if (FRun = nil) and AParagraph.HasParagraphFrame and not AParagraph.IsInCell then
    APieceTable := ASelection.PieceTable;

  AHitTestRequest := TdxRichEditHitTestRequest.Create(APieceTable);
  AHitTestRequest.LogicalPoint := FOldTopLeftCorner;
  AHitTestRequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Row;
  AHitTestRequest.Accuracy := NearestPageArea or NearestColumn or NearestRow;

  APageController := Control.InnerControl.Formatter.DocumentFormatter.Controller.PageController;
  AHitTestResult := TdxRichEditHitTestResult.Create(APageController.DocumentLayout, APieceTable);
  try
    AHitTestResult.Page := APage;
    AHitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Page);

    ACalculator := APageController.CreateHitTestCalculator(AHitTestRequest, AHitTestResult);
    try
      ACalculator.CalcHitTest(APage);
    finally
      ACalculator.Free;
    end;

    if AHitTestResult.Row <> nil then
      Result := AHitTestResult.Row.GetFirstPosition(APieceTable).RunIndex
    else
      Result := 0;
  finally
    AHitTestResult.Free;
  end;
end;

function TdxDragFloatingObjectManuallyMouseHandlerState.CommitDrag(const APoint: TPoint; AShiftState: TShiftState;
  const ADataObject: IdxDataObject): Boolean;
var
  AScaleFactor: Single;
  ATopLeftPhysicalPoint: TPoint;
  AModifier: TdxFloatingObjectLayoutModifier;
begin
  if not TdxPieceTable(FRun.PieceTable).CanEditSelection or not Control.InnerControl.IsEditable then
    Exit(False);

  if not CalculateHitTestAndCurrentPoint(APoint) then
    Exit(False);

  if not FCalculator.CanDropTo(FCurrentHitTestResult) then
    Exit(False);

  HideVisualFeedback;
  Control.BeginUpdate;
  try
    AScaleFactor := Control.InnerControl.ActiveView.ScaleFactor;
    ATopLeftPhysicalPoint := APoint;
    ATopLeftPhysicalPoint.X := ATopLeftPhysicalPoint.X + Round(FClickPointLogicalOffset.X * AScaleFactor);
    ATopLeftPhysicalPoint.Y := ATopLeftPhysicalPoint.Y + Round(FClickPointLogicalOffset.Y * AScaleFactor);

    AModifier := TdxFloatingObjectLayoutModifier.Create(Control, FRun, FFloatingObjectAnchorRunIndex);
    try
      AModifier.OldTopLeftCorner := FOldTopLeftCorner;
      AModifier.CurrentTopLeftCorner := FCurrentTopLeftCorner;
      AModifier.MinAffectedRunIndex := FMinAffectedRunIndex;
      AModifier.Commit(ATopLeftPhysicalPoint);
    finally
      AModifier.Free;
    end;
    FRun.Select;
  finally
    Control.EndUpdate;
  end;
  Result := True;
end;

function TdxDragFloatingObjectManuallyMouseHandlerState.ContinueDrag(const P: TPoint; const AllowedEffects: TdxDragDropEffects;
  AShiftState: TShiftState; const ADataObject: IdxDataObject): TdxDragDropEffects;
begin
  if not CalculateHitTestAndCurrentPoint(P) then
    Exit([TdxDragDropEffect.None]);

  if not FCalculator.CanDropTo(FCurrentHitTestResult) then
    Exit([TdxDragDropEffect.None]);

  HideVisualFeedback;
  ShowVisualFeedback;
  Result := AllowedEffects * [TdxDragDropEffect.Move];
end;

function TdxDragFloatingObjectManuallyMouseHandlerState.CreateVisualFeedbackTransform: TdxTransformMatrix;
begin
  if FCurrentTopLeftCorner.IsEqual(Unassigned) or (FPageViewInfo = nil) or (FCurrentHitTestResult = nil) then
    Exit(nil);

  Result := TdxFloatingObjectBox.CreateTransformUnsafe(FRotationAngle, TRect.CreateSize(FCurrentTopLeftCorner, FInitialShapeBounds.Size));
end;

procedure TdxDragFloatingObjectManuallyMouseHandlerState.ShowVisualFeedback;
begin
  if FCurrentTopLeftCorner.IsEqual(Unassigned) or (FPageViewInfo = nil) or (FCurrentHitTestResult = nil) then
    Exit;
  ShowVisualFeedbackCore(TRect.CreateSize(FCurrentTopLeftCorner, FInitialShapeBounds.Size), FPageViewInfo, FImage);
end;

procedure TdxDragFloatingObjectManuallyMouseHandlerState.HideVisualFeedback;
begin
  if FCurrentTopLeftCorner.IsEqual(Unassigned) or (FPageViewInfo = nil) or (FCurrentHitTestResult = nil) then
    Exit;

  HideVisualFeedbackCore(TRect.Null, FPageViewInfo);
end;

procedure TdxDragFloatingObjectManuallyMouseHandlerState.ShowVisualFeedbackCore(const ABounds: TRect; APageViewInfo: TdxPageViewInfo; AImage: TdxOfficeImage);
begin
  FPlatformStrategy.ShowVisualFeedbackCore(ABounds, APageViewInfo, AImage);
end;

procedure TdxDragFloatingObjectManuallyMouseHandlerState.HideVisualFeedbackCore(const ABounds: TRect; APageViewInfo: TdxPageViewInfo);
begin
  FPlatformStrategy.HideVisualFeedbackCore(ABounds, APageViewInfo);
end;

procedure TdxDragFloatingObjectManuallyMouseHandlerState.BeginVisualFeedback;
begin
  FPlatformStrategy.BeginVisualFeedback;
end;

procedure TdxDragFloatingObjectManuallyMouseHandlerState.EndVisualFeedback;
begin
  FPlatformStrategy.EndVisualFeedback;
end;

end.
