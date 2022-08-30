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

unit dxRichEdit.Control.Mouse;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

{.$DEFINE DXLOGGING}

interface

uses
  Types, Classes, Controls, SysUtils, Generics.Defaults, Generics.Collections, ActiveX,
  dxCore, dxCoreClasses, cxClasses,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Types,
  dxRichEdit.InnerControl.Mouse,
  dxRichEdit.Control.Core,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.Commands,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.HotZones,
  dxRichEdit.Control.Keyboard,
  dxRichEdit.Control.Mouse.AutoScroller;

type
  TdxRichEditMouseCustomState = class;
  TdxRichEditMouseCustomVisualFeedbackState = class;
  TdxRichEditMouseController = class;

  { IdxVisualFeedbackValueProvider }

  IdxVisualFeedbackValueProvider<T> = interface
    function GetVisualFeedbackValue: T;

    property VisualFeedbackValue: T read GetVisualFeedbackValue;
  end;

  { IdxVisualFeedback }

  IdxVisualFeedback = interface
    procedure &Begin;
    procedure &End;
    procedure Show;
    procedure Hide;
  end;

  { IdxEndDocumentUpdateHandler }

  IdxEndDocumentUpdateHandler = interface
  ['{690CD121-1D0A-4822-89B9-482A48CB7C35}']
    procedure HandleEndDocumentUpdate(const Args: TdxDocumentUpdateCompleteEventArgs);
  end;

  { IdxRichEditMouseState }

  IdxRichEditMouseState = interface(IdxCustomMouseState)
  ['{119E3746-78CD-4F20-9502-7B6B3FD6E405}']
    function GetState: TdxRichEditMouseCustomState;

    property State: TdxRichEditMouseCustomState read GetState;
  end;

  { TdxIOfficeScroller }

  IdxOfficeScroller = interface
    procedure Start(AControl: TControl); overload;
    procedure Start(AControl: TControl; const AScreenPoint: TPoint); overload;
  end;

  { TdxVisualFeedback }

  TdxVisualFeedback<T> = class abstract(TInterfacedObject, IdxVisualFeedback)
  strict private
    FValueProvider: TdxRichEditMouseCustomVisualFeedbackState;
  public
    constructor Create(const AValueProvider: TdxRichEditMouseCustomVisualFeedbackState);
    procedure &Begin; virtual; abstract;
    procedure &End; virtual; abstract;
    procedure Show; virtual; abstract;
    procedure Hide; virtual; abstract;

    property ValueProvider: TdxRichEditMouseCustomVisualFeedbackState read FValueProvider;
  end;

  { TdxRectangleVisualFeedbackValue }

  TdxRectangleVisualFeedbackValue = class
  strict private
    FPageViewInfo: TdxPageViewInfo;
    FBounds: TRect;
  public
    constructor Create(APageViewInfo: TdxPageViewInfo; const ABounds: TRect);

    property PageViewInfo: TdxPageViewInfo read FPageViewInfo;
    property Bounds: TRect read FBounds;
  end;

  { TdxRichEditMouseCustomState }

  TdxRichEditMouseCustomState = class abstract(TInterfacedObject,
    IdxRichEditMouseState, IdxCustomMouseState)
  private
    FController: TdxRichEditMouseController;
    FIsFinished: Boolean;
    function GetActivePieceTable: TdxPieceTable;
    function GetControl: IdxRichEditControl;
    function GetDocumentModel: TdxDocumentModel;
    function GetHighDetailLevelBox(AHitTestResult: TdxRichEditHitTestResult): TdxBox;
    function GetInnerControl: IdxInnerControl;
  protected
    function CalculateActiveObject(AHitTestResult: TdxRichEditHitTestResult): TObject; virtual;
    function CalculateActiveObjectCore(ARunIndex: Integer): TObject; virtual;
    function CalculateHitTest(const P: TPoint): TdxRichEditHitTestResult; virtual;

    procedure HandleMouseMoveCore(const Args: TdxMouseEventArgs; const P: TPoint; AHitTestResult: TdxRichEditHitTestResult); virtual;

    function CanShowToolTip: Boolean; virtual;
    function GetAutoScrollEnabled: Boolean; virtual;
    function GetStopClickTimerOnStart: Boolean; virtual;

    function GetTopLevelPage(AInitialHitTestResult: TdxRichEditHitTestResult): TdxPage;

    procedure SetMouseCursor(ACursor: TCursor); virtual;

    procedure UpdateHover(AHitTestResult: TdxRichEditHitTestResult); virtual;
    procedure UpdateHoverLayout(AHitTestResult: TdxRichEditHitTestResult;
      AActiveView: TdxRichEditView; ABoxChanged: Boolean); virtual;
    function UseHover: Boolean; virtual;

    procedure LockRelease;
    procedure UnlockRelease;

    procedure DoCancelMode; virtual;
    procedure DoDragOver(var Args: TdxDragEventArgs); virtual;
    procedure DoDragDrop(var Args: TdxDragEventArgs); virtual;
    procedure DoDragEnter(var Args: TdxDragEventArgs); virtual;
    procedure DoDragLeave; virtual;
    procedure GiveFeedback(Args: PdxGiveFeedbackEventArgs); virtual;
    procedure QueryContinueDrag(Args: PdxQueryContinueDragEventArgs); virtual;

    function GetState: TdxRichEditMouseCustomState;

    property ActivePieceTable: TdxPieceTable read GetActivePieceTable;
    property AutoScrollEnabled: Boolean read GetAutoScrollEnabled;
    property InnerControl: IdxInnerControl read GetInnerControl;
  public
    constructor Create(AController: TdxRichEditMouseController); virtual;
    destructor Destroy; override;

    procedure Finish; virtual;
    procedure Start; virtual;

    function CanHandleMouseUp(const Args: TdxMouseEventArgs): Boolean; virtual;
    procedure ContinueSelection(const Args: TdxMouseEventArgs); virtual;
    function SuppressDefaultMouseWheelProcessing: Boolean; virtual;

    procedure HandleLongMouseDown; virtual;
    procedure HandleMouseDown(const Args: TdxMouseEventArgs); virtual;
    procedure HandleMouseDoubleClick(const Args: TdxMouseEventArgs); virtual;
    procedure HandleMouseTripleClick(const Args: TdxMouseEventArgs); virtual;
    procedure HandleMouseMove(const Args: TdxMouseEventArgs); virtual;
    procedure HandleMouseUp(const Args: TdxMouseEventArgs); virtual;
    procedure HandleMouseWheel(const Args: TdxMouseEventArgs); virtual;

    function HandlePopupMenu(const Args: TdxMouseEventArgs): Boolean; virtual;


    property Control: IdxRichEditControl read GetControl;
    property Controller: TdxRichEditMouseController read FController;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property StopClickTimerOnStart: Boolean read GetStopClickTimerOnStart;
  end;
  TdxRichEditMouseCustomStateClass = class of TdxRichEditMouseCustomState;

  TdxRichEditMouseStates = class(TdxObjectList<TdxRichEditMouseCustomState>);

  { TdxRichEditMouseCustomVisualFeedbackState }

  TdxRichEditMouseCustomVisualFeedbackState = class(TdxRichEditMouseCustomState)
  protected
    function GetVisualFeedbackValue: TdxRectangleVisualFeedbackValue; virtual; abstract;
  public
    property VisualFeedbackValue: TdxRectangleVisualFeedbackValue read GetVisualFeedbackValue;
  end;

  { TdxRichEditMouseHandlerStrategyBase }

  TdxRichEditMouseHandlerStrategyBase = class abstract
  strict private
    FController: TdxRichEditMouseController;
    function GetControl: IdxRichEditControl;
  public
    constructor Create(AController: TdxRichEditMouseController);

    property Controller: TdxRichEditMouseController read FController;
    property Control: IdxRichEditControl read GetControl;
  end;

  { TdxRichEditMouseHandlerStateStrategyBase }

  TdxRichEditMouseHandlerStateStrategyBase = class abstract(TdxRichEditMouseHandlerStrategyBase)
  strict private
    FState: TdxRichEditMouseCustomState;
  protected
    property State: TdxRichEditMouseCustomState read FState;
  public
    constructor Create(AState: TdxRichEditMouseCustomState); reintroduce;
  end;

  { TdxRichEditMouseHandlerStrategy }

  TdxRichEditMouseHandlerStrategy = class abstract(TdxRichEditMouseHandlerStrategyBase)
  protected
    function CreateFakeMouseMoveEventArgs: TdxMouseEventArgs; virtual; abstract;
    function ConvertMouseEventArgs(var Args: TdxMouseEventArgs): TdxMouseEventArgs; virtual; abstract;
    function ConvertDragEventArgs(var AScreenDragEventArgs: TdxDragEventArgs): TdxDragEventArgs; virtual; abstract;
    procedure AutoScrollerOnDragOver(const APt: TPoint); virtual; abstract;
    procedure OnMouseUp(E: TdxMouseEventArgs); virtual; abstract;
    function CreateInternalDragState: TdxRichEditMouseCustomState; virtual; abstract;
  end;

  { TdxDragContentMouseHandlerStateCalculator }

  TdxDragContentMouseHandlerStateCalculator = class
  strict private
    FControl: IdxRichEditControl;
  public
    constructor Create(const AControl: IdxRichEditControl);
  public
    function CanDropContentTo(AHitTestResult: TdxRichEditHitTestResult; APieceTable: TdxPieceTable): Boolean; virtual;
    function UpdateDocumentModelPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    procedure UpdateVisualState; virtual;
    procedure OnInternalDragStart; virtual;
  end;

  { TdxDragFloatingObjectMouseHandlerStateCalculator }

  TdxDragFloatingObjectMouseHandlerStateCalculator = class
  private
    FAnchor: TdxFloatingObjectAnchorRun;
    FLogicalPointOffset: TPoint;
  protected
    property Anchor: TdxFloatingObjectAnchorRun read FAnchor;
    property LogicalPointOffset: TPoint read FLogicalPointOffset;
  public
    procedure Init(AAnchor: TdxFloatingObjectAnchorRun; const AOffset: TPoint); virtual;
    function CanDropTo(APoint: TdxRichEditHitTestResult): Boolean; virtual;
  end;

  { TdxRichEditMouseController }

  TdxRichEditMouseController = class(TdxRichEditCustomMouseController)
  private
    FActiveObject: TObject;
    FAutoScroller: TdxAutoScroller;
    FClickCount: Integer;
    FClickScreenPoint: TPoint;
    FCreating: Boolean;
    FDragSize: TSize;
    FSuspended: Boolean;
    FTimer: TcxTimer;
    FHitInfos: TdxMouseEventArgsArray;
    FControl: IdxRichEditControl;
    FState: IdxRichEditMouseState;
    FLastDragPoint: TPoint;
    FLastDragDropEffects: TdxDragDropEffects;

    function GetActiveView: TdxRichEditView;
    function GetButtonsFromShiftState(Shift: TShiftState): TdxMouseButtons;
    function GetDocumentModel: TdxDocumentModel;
    function GetHitInfo: TdxMouseEventArgs;
    function GetInnerControl: IdxInnerControl;
    function GetState: TdxRichEditMouseCustomState;
  protected
    procedure ClearOutdatedSelectionItems;
    function GetDefaultStateClass: TdxRichEditMouseCustomStateClass; virtual;
    procedure SetActiveObject(Value: TObject); virtual;

    function TryFindPageArea(AHitTestResult: TdxRichEditHitTestResult; APieceTable: TdxPieceTable): TdxPageArea;

    procedure ClickTimerHandler(Sender: TObject);
    function IsClickTimerActive: Boolean;
    procedure HandleClickTimer; virtual;
    procedure StartClickTimer;
    procedure StopClickTimer;

    procedure ControlBeginUpdate; override;
    procedure ControlEndUpdate; override;

    procedure ClearHitInfo;
    procedure SaveHitInfo(const Args: TdxMouseEventArgs);

    procedure CalculateAndSaveHitInfo(const Args: TdxMouseEventArgs); virtual;
    function CreateAutoScroller: TdxAutoScroller; virtual;
    function CreateInternalDragState: TdxRichEditMouseCustomState; virtual;
    function ConvertMouseEventArgs(const Args: TdxMouseEventArgs): TdxMouseEventArgs; virtual;
    procedure ConvertMouseDragEventArgs(var Args: TdxDragEventArgs); virtual;
    function GetMouseEventArgs(Buttons: TdxMouseButtons; Shift: TShiftState;
      const AMousePos: TPoint; AWheelDelta: Integer): TdxMouseEventArgs; overload;
    function GetMouseEventArgs(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer): TdxMouseEventArgs; overload;
    function GetMouseEventArgs(Shift: TShiftState; X, Y: Integer): TdxMouseEventArgs; overload;

    function GetDoubleClickSize: TSize;
    function GetDoubleClickTime: Integer;

    function CreateZoomCommand(const Args: TdxMouseEventArgs): TdxCommand; virtual;

    function GetPhysicalPoint(const P: TPoint): TPoint;
    function IsDoubleClick(const Args: TdxMouseEventArgs): Boolean;
    function IsMultipleClickCore(const Args: TdxMouseEventArgs): Boolean;
    function IsTripleClick(const Args: TdxMouseEventArgs): Boolean;
    function SupportsTripleClick: Boolean; virtual;

    function CalculateMouseWheelScrollRate(AWheelDelta: Integer): Single;
    procedure PerformWheelScroll(const Args: TdxMouseEventArgs); virtual;
    procedure PerformWheelZoom(const Args: TdxMouseEventArgs);
    procedure SmallScrollDown(AWheelDelta: Integer); virtual;
    procedure SmallScrollUp(AWheelDelta: Integer); virtual;
    procedure SmallScrollLeft(AWheelDelta: Integer); virtual;
    procedure SmallScrollRight(AWheelDelta: Integer); virtual;
    procedure SmallScrollVerticallyCore(AScrollRate: Single); virtual;
    procedure SmallScrollHorizontallyCore(AScrollRate: Single); virtual;

    procedure HandleMouseDoubleClick(const Args: TdxMouseEventArgs); virtual;
    procedure HandleMouseDown(const Args: TdxMouseEventArgs); virtual;
    procedure HandleMouseTripleClick(const Args: TdxMouseEventArgs); virtual;
    procedure HandleMouseMove(const Args: TdxMouseEventArgs); virtual;
    procedure HandleMouseUp(const Args: TdxMouseEventArgs); virtual;
    procedure HandleMouseWheel(const Args: TdxMouseEventArgs); virtual;

    property ClickCount: Integer read FClickCount;
    property ClickScreenPoint: TPoint read FClickScreenPoint;
    property Timer: TcxTimer read FTimer;

    property AutoScroller: TdxAutoScroller read FAutoScroller;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property HitInfo: TdxMouseEventArgs read GetHitInfo;
    property InnerControl: IdxInnerControl read GetInnerControl;
    property Suspended: Boolean read FSuspended;
  public
    constructor Create(const ARichControl: IdxRichEditControl); reintroduce; virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure ChangeActivePieceTable(APieceTable: TdxPieceTable; AHitTestResult: TdxRichEditHitTestResult = nil); override;
    function CreateDragFloatingObjectMouseHandlerStateCalculator: TdxDragFloatingObjectMouseHandlerStateCalculator; virtual;
    function CreateDragContentMouseHandlerStateCalculator: TdxDragContentMouseHandlerStateCalculator; virtual;

    function CreateFakeMouseMoveEventArgs: TdxMouseEventArgs;

    //for internal use
    function CreateRectangularObjectResizeState(const AHotZone: IdxHotZone; AResult: TdxRichEditHitTestResult): IdxCustomMouseState; override;
    function CreateRectangularObjectRotateState(const AHotZone: IdxHotZone; AResult: TdxRichEditHitTestResult): IdxCustomMouseState; override;
    procedure SwitchStateCore(const ANewState: IdxCustomMouseState; const AMousePosition: TPoint); override;
    procedure SwitchToDefaultState; virtual;

    function MouseActivate(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; HitTest: Integer): TMouseActivate; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;

    procedure DoCancelMode; override;
    function DoContextPopup(var Args: TdxMouseEventArgs): Boolean; override;
    procedure DoDragEnter(var Args: TdxDragEventArgs); override;
    procedure DoDragOver(var Args: TdxDragEventArgs); override;
    procedure DoDragDrop(var Args: TdxDragEventArgs); override;
    procedure DoDragLeave; override;
    procedure GiveFeedback(Args: PdxGiveFeedbackEventArgs); override;
    procedure QueryContinueDrag(Args: PdxQueryContinueDragEventArgs); override;

    property ActiveObject: TObject read FActiveObject;
    property ActiveView: TdxRichEditView read GetActiveView;
    property Control: IdxRichEditControl read FControl;
    property DragSize: TSize read FDragSize;
    property State: TdxRichEditMouseCustomState read GetState;
  end;

  { TdxRichEditMouseDefaultState }

  TdxRichEditMouseDefaultState = class(TdxRichEditMouseCustomState)
  private
    procedure BeginCharacterSelection(AHitTestResult: TdxRichEditHitTestResult);
    procedure BeginCharacterSelectionCore(AHitTestResult: TdxRichEditHitTestResult);
    procedure BeginDragExistingSelection(AHitTestResult: TdxRichEditHitTestResult; AResetSelectionOnMouseUp: Boolean);
    procedure BeginLineSelection(AHitTestResult: TdxRichEditHitTestResult);
    procedure BeginMultiSelection(AHitTestResult: TdxRichEditHitTestResult);
    procedure BeginParagraphsSelection(AHitTestResult: TdxRichEditHitTestResult);
    procedure BeginTableRowsSelectionState(AHitTestResult: TdxRichEditHitTestResult);
    procedure BeginWordsSelection(AHitTestResult: TdxRichEditHitTestResult);
    function CalculateExactPageAreaHitTest(const P: TPoint): TdxRichEditHitTestResult;
    procedure ClearMultiSelection;
    procedure DeactivateFloatingObject(AHitTestResult: TdxRichEditHitTestResult);
    procedure ExtendSelectionToCursor(AHitTestResult: TdxRichEditHitTestResult);
    procedure ExtendMultiSelection(AIterator: TdxPieceTableIterator; const ACurrentModelPosition: TdxDocumentModelPosition); virtual;
    procedure PerformEnhancedSelection(AHitTestResult: TdxRichEditHitTestResult);
    procedure SelectFloatingObject(AHitTestResult: TdxRichEditHitTestResult; AAllowDrag: Boolean; AAllowActivate: Boolean = True);
    procedure SelectPicture(AHitTestResult: TdxRichEditHitTestResult);
    procedure SetSelection(AStart, AEnd: TdxDocumentLogPosition; AClearMultiSelection: Boolean = True);
    function ShouldBeginDragExistingSelection(AHitTestResult: TdxRichEditHitTestResult): Boolean;
    function ShouldDeactivateFloatingObject(AHitTestResult: TdxRichEditHitTestResult): Boolean;
    function ShouldSelectFloatingObject(AHitTestResult: TdxRichEditHitTestResult): Boolean;
    function ShouldSelectPicture(AHitTestResult: TdxRichEditHitTestResult): Boolean;
    function ShouldStartMultiSelection: Boolean;
    function ShouldSwitchActivePieceTable(AHitTestResult: TdxRichEditHitTestResult): Boolean;
    function ShouldExtendSelectionToCursor: Boolean;
    function TryProcessHyperlinkClick(AHitTestResult: TdxRichEditHitTestResult): Boolean;
    procedure ValidateFieldSelection(ADocumentModel: TdxDocumentModel; ASelection: TdxSelection);
  protected
    procedure HandleMouseMoveCore(const Args: TdxMouseEventArgs; const P: TPoint;
      AHitTestResult: TdxRichEditHitTestResult); override;

    function CanShowToolTip: Boolean; override;
    function GetAutoScrollEnabled: Boolean; override;
    function IsHyperlinkActive: Boolean; virtual;
    function GetStopClickTimerOnStart: Boolean; override;

    function CreateDragState(const ADataObject: IDataObject): TdxRichEditMouseCustomState; virtual;
    function CreateDragContentState(AHitTestResult: TdxRichEditHitTestResult): TdxRichEditMouseCustomState; virtual;
    function CreateExternalDragState: TdxRichEditMouseCustomState; virtual;
    function CreateInternalDragState: TdxRichEditMouseCustomState; virtual;
    function IsExternalDrag(const ADataObject: IDataObject): Boolean; virtual;

    procedure ExtendSelection(AIterator: TdxPieceTableIterator); virtual;
    procedure SwitchStateToDragState(AHitTestResult: TdxRichEditHitTestResult; ADragState: TdxRichEditMouseCustomState); virtual;

    function HandleHotZoneMouseDown(AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    function HandleHotZoneMouseDownCore(AHitTestResult: TdxRichEditHitTestResult; AHotZone: TdxHotZone): Boolean; virtual;
  public
    procedure HandleMouseDown(const P: TPoint); reintroduce; overload;
    procedure HandleMouseDown(const AHitTestResult: TdxRichEditHitTestResult); reintroduce; overload;
    procedure HandleMouseDown(const Args: TdxMouseEventArgs); overload; override;
    procedure HandleMouseDoubleClick(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseTripleClick(const Args: TdxMouseEventArgs); override;

    procedure DoDragEnter(var Args: TdxDragEventArgs); override;

    procedure BeginResizeTableRowState(AHitTestResult: TdxRichEditHitTestResult; ATableRow: TdxTableRowViewInfoBase); virtual;
    procedure BeginResizeTableCellsHorizontallyState(AHitTestResult: TdxRichEditHitTestResult; AColumn: TdxVirtualTableColumn); virtual;
    procedure BeginTableColumnsSelectionState(AHitTestResult: TdxRichEditHitTestResult); virtual;
    procedure BeginTableCellsSelectionState(AHitTestResult: TdxRichEditHitTestResult); virtual;

    function CreatePlaceCaretToPhysicalPointCommand(AShift: Boolean): TdxPlaceCaretToPhysicalPointCommand; virtual;

    function HandlePopupMenu(const Args: TdxMouseEventArgs): Boolean; override;
    function IsFloatingObjectSelected(AFloatingObjectBox: TdxFloatingObjectBox): Boolean;
    function IsMouseDownInSelection(ALogPosition: TdxDocumentLogPosition): Boolean; virtual;
  protected
    function ShouldActivateFloatingObjectTextBox(AFloatingObjectBox: TdxFloatingObjectBox;
      ARun: TdxFloatingObjectAnchorRun; const ALogicalPoint: TPoint): Boolean; virtual;
    function ActivateFloatingObjectTextBox(ARun: TdxFloatingObjectAnchorRun; AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    procedure PlaceCaretToPhysicalPoint(const APhysicalPoint: TPoint);
  end;

  TdxBoxAccessorKind = (Box, Row, Character);

  { TdxBoxAccessor }

  TdxBoxAccessor = class
  public
    class function GetBox(AHitTestResult: TdxRichEditHitTestResult; AKind: TdxBoxAccessorKind): TdxBox; static;
  end;

  { TdxContinueSelectionByRangesMouseHandlerState }

  TdxContinueSelectionByRangesMouseHandlerState = class abstract (TdxRichEditMouseCustomState, IdxEndDocumentUpdateHandler)
  private
    FInitialHitTestResult: TdxRichEditHitTestResult;
    FInitialBoxAccessor: TdxBoxAccessorKind;
    FIsInvalidInitialHitTestResult: Boolean;
    FStartCell: TdxTableCell;
    function CalculateStartCell: TdxTableCell;
    procedure EndDocumentUpdateHandler(ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs);
    procedure EnforceFormattingComplete;
    function GetInitialBox: TdxBox;
    function GetInitialHitTestResult: TdxRichEditHitTestResult;
  protected
    function GetAutoScrollEnabled: Boolean; override;
    function GetStopClickTimerOnStart: Boolean; override;

    function CalculateCursor(const Args: TdxMouseEventArgs): TCursor; virtual;
    function CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase; virtual; abstract;
    function GetCurrentCell(const P: TPoint): TdxTableCell;
    function GetHitTestResult(const APhysicalPoint: TPoint): TdxRichEditHitTestResult;
    function GetParagraph(const P: TPoint): TdxParagraph;
    function NeedSwitchStateToTableCellsMouseHandlerState(const P: TPoint): Boolean;
    function NeedSwitchStateToTableCellsMouseHandlerStateCore(ACurrentParagraph: TdxParagraph): Boolean;

    procedure HandleEndDocumentUpdate(const Args: TdxDocumentUpdateCompleteEventArgs);

    property InitialBox: TdxBox read GetInitialBox;
    property InitialBoxAccessor: TdxBoxAccessorKind read FInitialBoxAccessor;
    property InitialHitTestResult: TdxRichEditHitTestResult read GetInitialHitTestResult;
    property StartCell: TdxTableCell read FStartCell write FStartCell;
  public
    constructor Create(AController: TdxRichEditMouseController;
      AInitialBoxAccessor: TdxBoxAccessorKind;
      AInitialHitTestResult: TdxRichEditHitTestResult); reintroduce;
    destructor Destroy; override;

    procedure Finish; override;
    procedure Start; override;

    procedure ContinueSelection(const Args: TdxMouseEventArgs); override;
    function CanShowToolTip: Boolean; override;

    procedure HandleMouseMove(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseUp(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseWheel(const Args: TdxMouseEventArgs); override;
  end;

  { TdxContinueSelectionByCharactersMouseHandlerState }

  TdxContinueSelectionByCharactersMouseHandlerState = class(TdxContinueSelectionByRangesMouseHandlerState)
  protected
    function CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase; override;
  public
    procedure ContinueSelection(const Args: TdxMouseEventArgs); override;
  end;

  { TdxContinueSelectionByTableRowsMouseHandlerStateBase }

  TdxContinueSelectionByTableRowsMouseHandlerStateBase = class abstract(TdxContinueSelectionByRangesMouseHandlerState)
  strict private
    FNestingLevel: Integer;
  protected
    function CalculateCursor(const E: TdxMouseEventArgs): TCursor; override;
    function CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase; override;
    function NeedSwitchState(const AMousePosition: TPoint): Boolean; virtual;
    procedure SwitchState(const AMousePosition: TPoint); virtual; abstract;
  public
    constructor Create(AController: TdxRichEditMouseController; AInitialBoxAccessor: TdxBoxAccessorKind;
      AInitialHitTestResult: TdxRichEditHitTestResult; ANestingLevel: Integer);
    procedure ContinueSelection(const E: TdxMouseEventArgs); override;

    property NestingLevel: Integer read FNestingLevel;
  end;

  { TdxContinueSelectionByStartTableRowsMouseHandlerState }

  TdxContinueSelectionByStartTableRowsMouseHandlerState = class(TdxContinueSelectionByTableRowsMouseHandlerStateBase)
  strict private
    FStartRowIndex: Integer;
  protected
    function CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase; override;
    function NeedSwitchState(const AMousePosition: TPoint): Boolean; override;
    procedure SwitchState(const AMousePosition: TPoint); override;
  public
    constructor Create(AController: TdxRichEditMouseController; AInitialBoxAccessor: TdxBoxAccessorKind; AInitialHitTestResult: TdxRichEditHitTestResult);

    property StartRowIndex: Integer read FStartRowIndex write FStartRowIndex;
  end;

  { TdxContinueSelectionByTableRowsAfterCharactersMouseHandlerState }

  TdxContinueSelectionByTableRowsAfterCharactersMouseHandlerState = class(TdxContinueSelectionByTableRowsMouseHandlerStateBase)
  protected
    procedure SwitchState(const AMousePosition: TPoint); override;
  end;

  { TdxContinueSelectionByTableRowsAfterRowMouseHandlerState }

  TdxContinueSelectionByTableRowsAfterRowMouseHandlerState = class(TdxContinueSelectionByTableRowsMouseHandlerStateBase)
  protected
    procedure SwitchState(const AMousePosition: TPoint); override;
  end;

  { TdxContinueSelectionByTableColumnsMouseHandlerState }

  TdxContinueSelectionByTableColumnsMouseHandlerState = class(TdxContinueSelectionByRangesMouseHandlerState)
  strict private
    FStartColumnIndex: Integer;
    FNestedLevel: Integer;
  protected
    function CalculateCursor(const E: TdxMouseEventArgs): TCursor; override;
    function CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase; override;
  public
    procedure ContinueSelection(const E: TdxMouseEventArgs); override;

    property StartColumnIndex: Integer read FStartColumnIndex write FStartColumnIndex;
    property NestedLevel: Integer read FNestedLevel write FNestedLevel;
  end;

  { TdxContinueSelectionByWordsAndParagraphsMouseHandlerStateBase }

  TdxContinueSelectionByWordsAndParagraphsMouseHandlerStateBase = class abstract(TdxContinueSelectionByRangesMouseHandlerState)
  protected
    function GetNewMouseHandlerState(AController: TdxRichEditMouseController; AInitialBoxAccessor: TdxBoxAccessorKind;
      AInitialHitTestResult: TdxRichEditHitTestResult; ANestingLevel: Integer): TdxContinueSelectionByTableRowsMouseHandlerStateBase; virtual; abstract;
  public
    procedure ContinueSelection(const Args: TdxMouseEventArgs); override;
  end;

  { TdxContinueSelectionByTableCellsMouseHandlerStateBase }

  TdxContinueSelectionByTableCellsMouseHandlerStateBase = class abstract (TdxContinueSelectionByRangesMouseHandlerState)
  protected
    function CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase; override;
    function NeedSwitchState(const APhysicalPoint: TPoint): Boolean; virtual;
    procedure ChangeSelection; virtual; abstract;
    procedure ValidateSelection;
  public
    procedure ContinueSelection(const E: TdxMouseEventArgs); override;
  end;

  { TdxContinueSelectionByStartTableCellsMouseHandlerState }

  TdxContinueSelectionByStartTableCellsMouseHandlerState = class(TdxContinueSelectionByTableCellsMouseHandlerStateBase)
  protected
    function CalculateCursor(const E: TdxMouseEventArgs): TCursor; override;
    procedure ChangeSelection; override;
  end;

  { TdxContinueSelectionByTableCellsMouseHandlerState }

  TdxContinueSelectionByTableCellsMouseHandlerState = class(TdxContinueSelectionByTableCellsMouseHandlerStateBase)
  protected
    function NeedSwitchState(const APhysicalPoint: TPoint): Boolean; override;
    procedure ChangeSelection; override;
  end;

  { TdxContinueSelectionByTableRowsAfterWordsMouseHandlerState }

  TdxContinueSelectionByTableRowsAfterWordsMouseHandlerState = class(TdxContinueSelectionByTableRowsMouseHandlerStateBase)
  protected
    function CalculateCursor(const E: TdxMouseEventArgs): TCursor; override;
    procedure SwitchState(const AMousePosition: TPoint); override;
  end;

  { TdxContinueSelectionByWordsMouseHandlerState }

  TdxContinueSelectionByWordsMouseHandlerState = class(TdxContinueSelectionByWordsAndParagraphsMouseHandlerStateBase)
  protected
    function CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase; override;
    function GetNewMouseHandlerState(AController: TdxRichEditMouseController; AInitialBoxAccessor: TdxBoxAccessorKind;
      AInitialHitTestResult: TdxRichEditHitTestResult; ANestingLevel: Integer): TdxContinueSelectionByTableRowsMouseHandlerStateBase; override;
  end;

  { TdxContinueSelectionByParagraphsMouseHandlerState }

  TdxContinueSelectionByParagraphsMouseHandlerState = class(tdxContinueSelectionByWordsAndParagraphsMouseHandlerStateBase)
  protected
    function CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase; override;
    function GetNewMouseHandlerState(AController: TdxRichEditMouseController; AInitialBoxAccessor: TdxBoxAccessorKind;
      AInitialHitTestResult: TdxRichEditHitTestResult; ANestingLevel: Integer): TdxContinueSelectionByTableRowsMouseHandlerStateBase; override;
  end;

  { TdxContinueSelectionByLinesMouseHandlerState }

  TdxContinueSelectionByLinesMouseHandlerState = class(TdxContinueSelectionByRangesMouseHandlerState)
  protected
    function CalculateCursor(const Args: TdxMouseEventArgs): TCursor; override;
    function CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase; override;
  public
    procedure ContinueSelection(const Args: TdxMouseEventArgs); override;
  end;

  { TdxContinueSelectionByTableRowsAfterParagraphsMouseHandlerState }

  TdxContinueSelectionByTableRowsAfterParagraphsMouseHandlerState = class(TdxContinueSelectionByTableRowsMouseHandlerStateBase)
  protected
    procedure SwitchState(const AMousePosition: TPoint); override;
  end;

  { TdxHyperlinkMouseClickHandler }

  TdxHyperlinkMouseClickHandler = class
  private
    FControl: IdxRichEditControl;
  protected
    function TryProcessHyperlinkClick(AHitTestResult: TdxRichEditHitTestResult): Boolean;
  public
    constructor Create(const AControl: IdxRichEditControl);
    procedure HandleMouseUp(const Args: TdxMouseEventArgs); virtual;
    property Control: IdxRichEditControl read FControl;
  end;

implementation

uses
  Windows, Math, cxGeometry, cxControls, dxTypeHelpers, cxLibraryConsts,

  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.Utils.Cursors,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.Commands.Keyboard,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.Commands.Tables,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.Control.Mouse.Resize,
  dxRichEdit.Control.Mouse.DragAndDrop,
  dxRichEdit.DocumentModel.TableCellsManager,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.ParagraphFormatting;

{ TdxVisualFeedback }

constructor TdxVisualFeedback<T>.Create(const AValueProvider: TdxRichEditMouseCustomVisualFeedbackState);
begin
  inherited Create;
  FValueProvider := AValueProvider;
end;

{ TdxRectangleVisualFeedbackValue }

constructor TdxRectangleVisualFeedbackValue.Create(APageViewInfo: TdxPageViewInfo; const ABounds: TRect);
begin
  inherited Create;
  FPageViewInfo := APageViewInfo;
  FBounds := ABounds;
end;

{ TdxRichEditMouseCustomStateController }

constructor TdxRichEditMouseCustomState.Create(AController: TdxRichEditMouseController);
begin
  inherited Create;
  FController := AController;
end;

destructor TdxRichEditMouseCustomState.Destroy;
begin
  UnlockRelease;
  inherited Destroy;
end;

procedure TdxRichEditMouseCustomState.Finish;
begin
  FIsFinished := True;
end;

procedure TdxRichEditMouseCustomState.SetMouseCursor(ACursor: TCursor);
begin
  Control.Cursor := ACursor;
end;

procedure TdxRichEditMouseCustomState.Start;
begin
  if StopClickTimerOnStart then
    Controller.StopClickTimer;
end;

procedure TdxRichEditMouseCustomState.HandleLongMouseDown;
begin
end;

procedure TdxRichEditMouseCustomState.HandleMouseDown(const Args: TdxMouseEventArgs);
begin
end;

procedure TdxRichEditMouseCustomState.HandleMouseDoubleClick(const Args: TdxMouseEventArgs);
begin
end;

procedure TdxRichEditMouseCustomState.HandleMouseTripleClick(const Args: TdxMouseEventArgs);
begin
end;

procedure TdxRichEditMouseCustomState.HandleMouseMove(const Args: TdxMouseEventArgs);
var
  P: TPoint;
  AHitTestResult: TdxRichEditHitTestResult;
begin
  P := Args.MousePos;
  AHitTestResult := CalculateHitTest(P);
  try
    HandleMouseMoveCore(Args, P, AHitTestResult);
  finally
    AHitTestResult.Free;
  end;
end;

procedure TdxRichEditMouseCustomState.HandleMouseUp(const Args: TdxMouseEventArgs);
begin
end;

procedure TdxRichEditMouseCustomState.HandleMouseWheel(const Args: TdxMouseEventArgs);
begin
end;

function TdxRichEditMouseCustomState.HandlePopupMenu(
  const Args: TdxMouseEventArgs): Boolean;
begin
  Result := False;
end;

procedure TdxRichEditMouseCustomState.LockRelease;
begin
  if Self <> nil then
    _AddRef;
end;

function TdxRichEditMouseCustomState.CalculateActiveObject(AHitTestResult: TdxRichEditHitTestResult): TObject;
begin
  Result := nil;
  if (AHitTestResult <> nil) and (AHitTestResult.DetailsLevel >= TdxDocumentLayoutDetailsLevel.Box) and
    ((AHitTestResult.Accuracy and ExactBox) <> 0) then
  begin
    Result := CalculateActiveObjectCore(GetHighDetailLevelBox(AHitTestResult).StartPos.RunIndex);
  end;
end;

function TdxRichEditMouseCustomState.CalculateActiveObjectCore(ARunIndex: Integer): TObject;
begin
  Result := ActivePieceTable.FindFieldByRunIndex(ARunIndex);
end;

function TdxRichEditMouseCustomState.CalculateHitTest(const P: TPoint): TdxRichEditHitTestResult;
begin
  Result := Controller.ActiveView.CalculateNearestCharacterHitTest(P, ActivePieceTable);
end;

procedure TdxRichEditMouseCustomState.HandleMouseMoveCore(const Args: TdxMouseEventArgs; const P: TPoint;
  AHitTestResult: TdxRichEditHitTestResult);
begin
  if UseHover then
    UpdateHover(AHitTestResult);
  Controller.SetActiveObject(CalculateActiveObject(AHitTestResult));
end;

function TdxRichEditMouseCustomState.GetAutoScrollEnabled: Boolean;
begin
  Result := True;
end;

function TdxRichEditMouseCustomState.CanShowToolTip: Boolean;
begin
  Result := False;
end;

function TdxRichEditMouseCustomState.GetStopClickTimerOnStart: Boolean;
begin
  Result := True;
end;

function TdxRichEditMouseCustomState.GetTopLevelPage(AInitialHitTestResult: TdxRichEditHitTestResult): TdxPage;
begin
  if (AInitialHitTestResult.FloatingObjectBox <> nil) and (AInitialHitTestResult.FloatingObjectBoxPage <> nil) then
    Result := AInitialHitTestResult.FloatingObjectBoxPage
  else
    Result := AInitialHitTestResult.Page;
end;

function TdxRichEditMouseCustomState.CanHandleMouseUp(const Args: TdxMouseEventArgs): Boolean;
begin
  Result := Args.Buttons = [mbLeft];
end;

procedure TdxRichEditMouseCustomState.ContinueSelection(const Args: TdxMouseEventArgs);
begin
end;

function TdxRichEditMouseCustomState.SuppressDefaultMouseWheelProcessing: Boolean;
begin
  Result := False;
end;

procedure TdxRichEditMouseCustomState.UnlockRelease;
begin
  if Self <> nil then
    _Release;
end;

procedure TdxRichEditMouseCustomState.DoCancelMode;
begin
end;

procedure TdxRichEditMouseCustomState.DoDragOver(var Args: TdxDragEventArgs);
begin
end;

procedure TdxRichEditMouseCustomState.DoDragDrop(var Args: TdxDragEventArgs);
begin
end;

procedure TdxRichEditMouseCustomState.DoDragEnter(var Args: TdxDragEventArgs);
begin
end;

procedure TdxRichEditMouseCustomState.DoDragLeave;
begin
end;

procedure TdxRichEditMouseCustomState.GiveFeedback(
  Args: PdxGiveFeedbackEventArgs);
begin
end;

procedure TdxRichEditMouseCustomState.QueryContinueDrag(Args: PdxQueryContinueDragEventArgs);
begin
end;

procedure TdxRichEditMouseCustomState.UpdateHover(AHitTestResult: TdxRichEditHitTestResult);
begin
Assert(False, 'not implemented');
end;

procedure TdxRichEditMouseCustomState.UpdateHoverLayout(
  AHitTestResult: TdxRichEditHitTestResult; AActiveView: TdxRichEditView;
  ABoxChanged: Boolean);
begin
Assert(False, 'not implemented');
end;

function TdxRichEditMouseCustomState.UseHover: Boolean;
begin
  Result := False;
end;

function TdxRichEditMouseCustomState.GetActivePieceTable: TdxPieceTable;
begin
  Result := DocumentModel.ActivePieceTable;
end;

function TdxRichEditMouseCustomState.GetControl: IdxRichEditControl;
begin
  Result := Controller.Control;
end;

function TdxRichEditMouseCustomState.GetDocumentModel: TdxDocumentModel;
begin
  Result := InnerControl.DocumentModel;
end;

function TdxRichEditMouseCustomState.GetHighDetailLevelBox(AHitTestResult: TdxRichEditHitTestResult): TdxBox;
begin
  if AHitTestResult.Accuracy and ExactCharacter <> 0 then
    Result := AHitTestResult.Character
  else
    Result := AHitTestResult.Box;
end;

function TdxRichEditMouseCustomState.GetInnerControl: IdxInnerControl;
begin
  Result := Controller.InnerControl;
end;

function TdxRichEditMouseCustomState.GetState: TdxRichEditMouseCustomState;
begin
  Result := Self;
end;

{ TdxRichEditMouseHandlerStrategyBase }

constructor TdxRichEditMouseHandlerStrategyBase.Create(AController: TdxRichEditMouseController);
begin
  inherited Create;
  FController := AController;
end;

function TdxRichEditMouseHandlerStrategyBase.GetControl: IdxRichEditControl;
begin
  Result := Controller.Control;
end;

{ TdxRichEditMouseHandlerStateStrategyBase }

constructor TdxRichEditMouseHandlerStateStrategyBase.Create(AState: TdxRichEditMouseCustomState);
begin
  inherited Create(AState.Controller);
  Assert(AState <> nil);
  FState := AState;
end;

{ TdxDragContentMouseHandlerStateCalculator }

constructor TdxDragContentMouseHandlerStateCalculator.Create(const AControl: IdxRichEditControl);
begin
  inherited Create;
  FControl := AControl;
end;

function TdxDragContentMouseHandlerStateCalculator.CanDropContentTo(AHitTestResult: TdxRichEditHitTestResult; APieceTable: TdxPieceTable): Boolean;
var
  ARun: TdxFloatingObjectAnchorRun;
  AContent: TdxTextBoxFloatingObjectContent;
begin
  if AHitTestResult.DetailsLevel < TdxDocumentLayoutDetailsLevel.Box then
    Exit(False);

  if AHitTestResult.PageArea.PieceTable = APieceTable then
    Exit(True);

  if not APieceTable.ContentType.IsTextBox or (AHitTestResult.FloatingObjectBox = nil) then
    Exit(False);

  ARun := Safe<TdxFloatingObjectAnchorRun>.Cast(AHitTestResult.FloatingObjectBox.GetRun(AHitTestResult.FloatingObjectBox.PieceTable));
  if ARun = nil then
    Exit(False);

  AContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  if AContent = nil then
    Exit(False);

  Result := AContent.TextBox.PieceTable = APieceTable;
end;

function TdxDragContentMouseHandlerStateCalculator.UpdateDocumentModelPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
end;

procedure TdxDragContentMouseHandlerStateCalculator.UpdateVisualState;
begin
end;

procedure TdxDragContentMouseHandlerStateCalculator.OnInternalDragStart;
begin
end;

{ TdxDragFloatingObjectMouseHandlerStateCalculator }

function TdxDragFloatingObjectMouseHandlerStateCalculator.CanDropTo(APoint: TdxRichEditHitTestResult): Boolean;
begin
  Result := True;
end;

procedure TdxDragFloatingObjectMouseHandlerStateCalculator.Init(AAnchor: TdxFloatingObjectAnchorRun;
  const AOffset: TPoint);
begin
  FAnchor := AAnchor;
  FLogicalPointOffset := AOffset;
end;

{ TdxRichEditMouseController }

constructor TdxRichEditMouseController.Create(const ARichControl: IdxRichEditControl);
begin
  inherited Create;
  FCreating := True;
  FControl := ARichControl;
  FDragSize.Init(GetSystemMetrics(SM_CXDRAG), GetSystemMetrics(SM_CYDRAG));
  FTimer := TcxTimer.Create(nil);
  FLastDragPoint := cxNullPoint;
  FAutoScroller := CreateAutoScroller;
  SwitchToDefaultState;
end;

procedure TdxRichEditMouseController.AfterConstruction;
begin
  inherited AfterConstruction;
  FCreating := False;
end;

function TdxRichEditMouseController.CreateDragContentMouseHandlerStateCalculator: TdxDragContentMouseHandlerStateCalculator;
begin
  Result := TdxDragContentMouseHandlerStateCalculator.Create(Control);
end;

function TdxRichEditMouseController.CreateInternalDragState: TdxRichEditMouseCustomState;
begin
  Result := TdxDragContentStandardMouseHandlerState.Create(Self)
end;

function TdxRichEditMouseController.CreateZoomCommand(
  const Args: TdxMouseEventArgs): TdxCommand;
var
  AZoomCommand: TdxZoomCommandBase;
  AMouseWheelScrollDelta: Single;
  ADelta: Single;
begin
  if Args.WheelDelta > 0 then
    AZoomCommand := TdxZoomInCommand.Create(Control)
  else
    AZoomCommand := TdxZoomOutCommand.Create(Control);
  AMouseWheelScrollDelta := 120;
  ADelta := TdxZoomCommandBase.DefaultZoomFactorDelta * (Abs(Args.WheelDelta) / AMouseWheelScrollDelta);
  AZoomCommand.Delta := Round(ADelta * 100) / 100;
  Result := AZoomCommand;
end;

function TdxRichEditMouseController.CreateDragFloatingObjectMouseHandlerStateCalculator: TdxDragFloatingObjectMouseHandlerStateCalculator;
begin
  Result := TdxDragFloatingObjectMouseHandlerStateCalculator.Create;
end;

destructor TdxRichEditMouseController.Destroy;
begin
  TableViewInfo := nil;
  FreeAndNil(FAutoScroller);
  FreeAndNil(FTimer);
  FState := nil;
  inherited Destroy;
end;

function TdxRichEditMouseController.CreateFakeMouseMoveEventArgs: TdxMouseEventArgs;
var
  P: TPoint;
begin
  P := GetMouseCursorPos;
  P := Control.Control.ScreenToClient(P);
  Result := TdxMouseEventArgs.Create([], [], P);
end;

function TdxRichEditMouseController.GetDefaultStateClass: TdxRichEditMouseCustomStateClass;
begin
  Result := TdxRichEditMouseDefaultState;
end;

procedure TdxRichEditMouseController.SetActiveObject(Value: TObject);
begin
  FActiveObject := Value;
end;

procedure TdxRichEditMouseController.ClickTimerHandler(Sender: TObject);
begin
  FClickCount := 0;
  if IsClickTimerActive then
    HandleClickTimer;
  StopClickTimer;
end;

procedure TdxRichEditMouseController.HandleClickTimer;
begin
  StopClickTimer;
  if not Suspended then
    State.HandleLongMouseDown;
end;

function TdxRichEditMouseController.IsClickTimerActive: Boolean;
begin
  Result := Timer.Enabled;
end;

procedure TdxRichEditMouseController.StartClickTimer;
begin
  Assert(not IsClickTimerActive);
  FTimer.Interval := GetDoubleClickTime;
  FTimer.OnTimer := ClickTimerHandler;
  FTimer.Enabled := True;
end;

procedure TdxRichEditMouseController.StopClickTimer;
begin
  FTimer.Enabled := False;
end;

procedure TdxRichEditMouseController.ControlBeginUpdate;
begin
  Control.BeginUpdate;
end;

procedure TdxRichEditMouseController.ControlEndUpdate;
begin
  Control.EndUpdate;
end;

procedure TdxRichEditMouseController.SaveHitInfo(const Args: TdxMouseEventArgs);
var
  ACount: Integer;
begin
  ACount := Length(FHitInfos);
  SetLength(FHitInfos, ACount + 1);
  FHitInfos[ACount] := Args;
end;

procedure TdxRichEditMouseController.SwitchToDefaultState;
begin
  if not FCreating then
    ClearOutdatedSelectionItems;
  SwitchStateCore(GetDefaultStateClass.Create(Self), cxNullPoint);
end;

function TdxRichEditMouseController.CreateRectangularObjectResizeState(const AHotZone: IdxHotZone; AResult: TdxRichEditHitTestResult): IdxCustomMouseState;
begin
  Result := TdxRichEditRectangularObjectResizeMouseHandlerState.Create(Self, TdxRectangularObjectHotZone(AHotZone), AResult);
end;

function TdxRichEditMouseController.CreateRectangularObjectRotateState(const AHotZone: IdxHotZone; AResult: TdxRichEditHitTestResult): IdxCustomMouseState;
begin
  Result := TdxRichEditRectangularObjectRotateMouseHandlerState.Create(Self, TdxRectangularObjectHotZone(AHotZone), AResult);
end;

procedure TdxRichEditMouseController.SwitchStateCore(const ANewState: IdxCustomMouseState; const AMousePosition: TPoint);
var
  AState: IdxRichEditMouseState;
begin
  Assert(not Suspended);
  AutoScroller.Deactivate;
  AState := ANewState as IdxRichEditMouseState;
  if State <> nil then
    State.Finish;
  FState := AState;
  if State.AutoScrollEnabled then
    AutoScroller.Activate(AMousePosition);
  State.Start;
end;

procedure TdxRichEditMouseController.ClearHitInfo;
begin
  SetLength(FHitInfos, Length(FHitInfos) - 1);
end;

procedure TdxRichEditMouseController.CalculateAndSaveHitInfo(const Args: TdxMouseEventArgs);
begin
end;

function TdxRichEditMouseController.CreateAutoScroller: TdxAutoScroller;
begin
  Result := TdxAutoScroller.Create(Self);
end;

function TdxRichEditMouseController.ConvertMouseEventArgs(const Args: TdxMouseEventArgs): TdxMouseEventArgs;
var
  APoint: TPoint;
begin
  APoint := GetPhysicalPoint(Args.MousePos);
  Result := Args;
  Result.MousePos := APoint;
end;

procedure TdxRichEditMouseController.ConvertMouseDragEventArgs(var Args: TdxDragEventArgs);
var
  AScreenMousePos: TPoint;
  AClientMousePoint: TPoint;
begin
  AScreenMousePos := Args.P;
  AClientMousePoint := TControl(Control).ScreenToClient(AScreenMousePos);
  AClientMousePoint := GetPhysicalPoint(AClientMousePoint);
  Args.P := AClientMousePoint;
end;

function TdxRichEditMouseController.GetMouseEventArgs(Buttons: TdxMouseButtons; Shift: TShiftState;
  const AMousePos: TPoint; AWheelDelta: Integer): TdxMouseEventArgs;
begin
  Result := TdxMouseEventArgs.Create(Shift, AMousePos, AWheelDelta);
end;

function TdxRichEditMouseController.GetMouseEventArgs(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): TdxMouseEventArgs;
var
  AButtons: TdxMouseButtons;
begin
  AButtons := GetButtonsFromShiftState(Shift);
  Include(AButtons, Button);
  Result := TdxMouseEventArgs.Create(AButtons, Shift, Point(X, Y));
end;

function TdxRichEditMouseController.GetMouseEventArgs(Shift: TShiftState; X, Y: Integer): TdxMouseEventArgs;
begin
  Result := GetMouseEventArgs(GetButtonsFromShiftState(Shift), Shift, Point(X, Y), 0);
end;

function TdxRichEditMouseController.GetDoubleClickSize: TSize;
begin
  Result := cxSize(0, 0);
end;

function TdxRichEditMouseController.GetDoubleClickTime: Integer;
begin
  Result := Windows.GetDoubleClickTime;
end;

function TdxRichEditMouseController.GetPhysicalPoint(const P: TPoint): TPoint;
var
  R: TRect;
begin
  R := Control.ViewBounds;
  Result.X := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(P.X, DocumentModel.DpiX) -
    R.Left;
  Result.Y := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(P.Y, DocumentModel.DpiY) -
    R.Top;
end;


function TdxRichEditMouseController.IsDoubleClick(const Args: TdxMouseEventArgs): Boolean;
begin
  Result := IsMultipleClickCore(Args) and (ClickCount = 2);
end;

function TdxRichEditMouseController.TryFindPageArea(AHitTestResult: TdxRichEditHitTestResult;
  APieceTable: TdxPieceTable): TdxPageArea;
begin
  if (AHitTestResult = nil) or (AHitTestResult.Page = nil) then
    Exit(nil);
  Result := AHitTestResult.Page.GetActiveFirstArea(APieceTable);
end;

function TdxRichEditMouseController.IsMultipleClickCore(const Args: TdxMouseEventArgs): Boolean;
begin
  Result := IsClickTimerActive and (mbLeft in Args.Buttons) and
    (Abs(Args.MousePos.X - ClickScreenPoint.X) <= GetDoubleClickSize.cx) and
    (Abs(Args.MousePos.Y - ClickScreenPoint.Y) <= GetDoubleClickSize.cy);
end;

function TdxRichEditMouseController.IsTripleClick(const Args: TdxMouseEventArgs): Boolean;
begin
  Result := IsMultipleClickCore(Args) and SupportsTripleClick and (ClickCount = 3);
end;

function TdxRichEditMouseController.SupportsTripleClick: Boolean;
begin
  Result := True;
end;

function TdxRichEditMouseController.CalculateMouseWheelScrollRate(AWheelDelta: Integer): Single;
begin
  Result := Mouse.WheelScrollLines * Abs(AWheelDelta) / WHEEL_DELTA / 3.0;
end;

procedure TdxRichEditMouseController.ChangeActivePieceTable(APieceTable: TdxPieceTable;
  AHitTestResult: TdxRichEditHitTestResult = nil);
var
  AArea: TdxPageArea;
  ASection: TdxSection;
  ACommand: TdxChangeActivePieceTableCommand;
begin
  ASection := nil;
  if APieceTable.IsHeaderFooter then
  begin
    if (AHitTestResult = nil) or (AHitTestResult.PageArea = nil) then
    begin
      AArea := TryFindPageArea(AHitTestResult, APieceTable);
      if AArea = nil then
        APieceTable := APieceTable.DocumentModel.MainPieceTable
      else
        ASection := AArea.Section;
    end
    else
      ASection := AHitTestResult.PageArea.Section;
  end
  else
    if APieceTable.IsTextBox and (AHitTestResult.PageArea <> nil) then
      ASection := AHitTestResult.PageArea.Section;
  ACommand := TdxChangeActivePieceTableCommand.Create(Control, APieceTable, ASection, 0);
  try
    ACommand.ActivatePieceTable(APieceTable, ASection);
  finally
    ACommand.Free;
  end;
end;

procedure TdxRichEditMouseController.ClearOutdatedSelectionItems;
var
  ASelection: TdxSelection;
begin
  Control.HideCaret;
  ASelection := Control.InnerControl.DocumentModel.Selection;
  ASelection.BeginUpdate;
  ASelection.ClearOutdatedItems;
  ASelection.EndUpdate;
end;

procedure TdxRichEditMouseController.PerformWheelScroll(const Args: TdxMouseEventArgs);
begin
  if Args.Horizontal then
  begin
    if Args.WheelDelta > 0 then
      SmallScrollRight(Args.WheelDelta)
    else
      SmallScrollLeft(Args.WheelDelta);
  end
  else
  begin
    if Args.WheelDelta > 0 then
      SmallScrollUp(Args.WheelDelta)
    else
      SmallScrollDown(Args.WheelDelta);
  end;
end;

procedure TdxRichEditMouseController.PerformWheelZoom(const Args: TdxMouseEventArgs);
var
  ACommand: TdxCommand;
begin
  ACommand := CreateZoomCommand(Args);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxRichEditMouseController.SmallScrollDown(AWheelDelta: Integer);
var
  AScrollRate: Single;
begin
  AScrollRate := CalculateMouseWheelScrollRate(AWheelDelta);
  SmallScrollVerticallyCore(AScrollRate);
end;

procedure TdxRichEditMouseController.SmallScrollUp(AWheelDelta: Integer);
var
  AScrollRate: Single;
begin
  AScrollRate := CalculateMouseWheelScrollRate(AWheelDelta);
  SmallScrollVerticallyCore(-AScrollRate);
end;

procedure TdxRichEditMouseController.SmallScrollLeft(AWheelDelta: Integer);
var
  AScrollRate: Single;
begin
  AScrollRate := CalculateMouseWheelScrollRate(AWheelDelta);
  SmallScrollHorizontallyCore(-AScrollRate);
end;

procedure TdxRichEditMouseController.SmallScrollRight(AWheelDelta: Integer);
var
  AScrollRate: Single;
begin
  AScrollRate := CalculateMouseWheelScrollRate(AWheelDelta);
  SmallScrollHorizontallyCore(AScrollRate);
end;

procedure TdxRichEditMouseController.SmallScrollVerticallyCore(AScrollRate: Single);
var
  AGenerator: TdxPageViewInfoGenerator;
  ALogicalVisibleHeight, ALogicalOffsetInModelUnits: Integer;
  ACommand: TdxScrollVerticallyByLogicalOffsetCommand;
begin
  AGenerator := Control.InnerControl.ActiveView.PageViewInfoGenerator;
  ALogicalVisibleHeight := Round(AGenerator.ViewPortBounds.Height / AGenerator.ScaleFactor);

  ACommand := TdxScrollVerticallyByLogicalOffsetCommand.Create(Control);
  try
    ALogicalOffsetInModelUnits := Trunc(Max(1, Control.InnerControl.DocumentModel.UnitConverter.DocumentsToModelUnits(150) * Abs(AScrollRate)));
    ACommand.LogicalOffset := Sign(AScrollRate) * Min(ALogicalVisibleHeight, Control.InnerControl.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ALogicalOffsetInModelUnits));
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxRichEditMouseController.SmallScrollHorizontallyCore(AScrollRate: Single);
var
  AGenerator: TdxPageViewInfoGenerator;
  AVisibleWidth, APhysicalOffsetInModelUnits: Integer;
  ACommand: TdxScrollHorizontallyByPhysicalOffsetCommand;
begin
  AGenerator := Control.InnerControl.ActiveView.PageViewInfoGenerator;
  AVisibleWidth := Round(AGenerator.ViewPortBounds.Width / AGenerator.ScaleFactor);

  ACommand := TdxScrollHorizontallyByPhysicalOffsetCommand.Create(Control);
  try
    APhysicalOffsetInModelUnits := Trunc(Max(1, Control.InnerControl.DocumentModel.UnitConverter.DocumentsToModelUnits(150) * Abs(AScrollRate)));
    ACommand.PhysicalOffset := Sign(AScrollRate) * Min(AVisibleWidth, Control.InnerControl.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(APhysicalOffsetInModelUnits));
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxRichEditMouseController.HandleMouseDoubleClick(const Args: TdxMouseEventArgs);
begin
  SaveHitInfo(Args);
  try
    (State as IdxRichEditMouseState).HandleMouseDoubleClick(Args);
  finally
    ClearHitInfo;
  end;
end;

procedure TdxRichEditMouseController.HandleMouseDown(const Args: TdxMouseEventArgs);
begin
  SaveHitInfo(Args);
  try
    (State as IdxRichEditMouseState).HandleMouseDown(Args);
  finally
    ClearHitInfo;
  end;
end;

procedure TdxRichEditMouseController.HandleMouseTripleClick(const Args: TdxMouseEventArgs);
begin
  SaveHitInfo(Args);
  try
    (State as IdxRichEditMouseState).HandleMouseTripleClick(Args);
  finally
    ClearHitInfo;
  end;
end;

procedure TdxRichEditMouseController.HandleMouseMove(const Args: TdxMouseEventArgs);
var
  P: TPoint;
begin
  P := Args.MousePos;
  CalculateAndSaveHitInfo(Args);
  SaveHitInfo(Args);
  try
    (State as IdxRichEditMouseState).HandleMouseMove(Args);
    AutoScroller.HandleMouseMove(P);
  finally
    ClearHitInfo;
  end;
end;

procedure TdxRichEditMouseController.HandleMouseUp(const Args: TdxMouseEventArgs);
begin
  if not State.CanHandleMouseUp(Args) then
    Exit;
  CalculateAndSaveHitInfo(Args);
  SaveHitInfo(Args);
  try
    (State as IdxRichEditMouseState).HandleMouseUp(Args);
  finally
    ClearHitInfo;
  end;
end;

procedure TdxRichEditMouseController.HandleMouseWheel(const Args: TdxMouseEventArgs);
var
  ASuppressWheelScroll: Boolean;
  ADragState: TdxBeginMouseDragHelperState;
begin
  if not State.SuppressDefaultMouseWheelProcessing then
  begin
    if TdxKeyboardHelper.IsControlPressed then
      PerformWheelZoom(Args)
    else
    begin
      if State is TdxBeginMouseDragHelperState then
        ADragState := TdxBeginMouseDragHelperState(State)
      else
        ADragState := nil;
      ASuppressWheelScroll := (ADragState <> nil) and (ADragState.DragState is TdxDragContentStandardMouseHandlerState);
      if not ASuppressWheelScroll then
        PerformWheelScroll(Args);
    end;
  end;
  (State as IdxRichEditMouseState).HandleMouseWheel(Args);
end;

function TdxRichEditMouseController.MouseActivate(Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer): TMouseActivate;
begin
  Assert(False, 'need return value of function!');
  Result := maDefault;
end;

procedure TdxRichEditMouseController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Args: TdxMouseEventArgs;
begin
  if Button = mbMiddle then
  begin
    Exit;
  end;
  Inc(FClickCount);
  Args := GetMouseEventArgs(Button, Shift, X, Y);
  CalculateAndSaveHitInfo(Args);
  if IsTripleClick(Args) then
  begin
    StopClickTimer;
    HandleMouseTripleClick(ConvertMouseEventArgs(Args));
  end
  else
    if IsDoubleClick(Args) then
    begin
      StopClickTimer;
      if SupportsTripleClick then
      begin
        StartClickTimer;
        FClickCount := 2;
      end;
      HandleMouseDoubleClick(ConvertMouseEventArgs(Args));
    end
    else
    begin
      StopClickTimer;
      StartClickTimer;
      FClickCount := 1;
      FClickScreenPoint := Args.MousePos;
      HandleMouseDown(ConvertMouseEventArgs(Args));
    end;
end;

procedure TdxRichEditMouseController.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  HandleMouseMove(ConvertMouseEventArgs(GetMouseEventArgs(Shift, X, Y)));
end;

procedure TdxRichEditMouseController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  HandleMouseUp(ConvertMouseEventArgs(GetMouseEventArgs(Button, Shift, X, Y)));
end;

function TdxRichEditMouseController.MouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := True;
  HandleMouseWheel(ConvertMouseEventArgs(GetMouseEventArgs(GetButtonsFromShiftState(Shift), Shift, MousePos, WheelDelta)));
end;

procedure TdxRichEditMouseController.DoDragEnter(var Args: TdxDragEventArgs);
begin
  FLastDragPoint := cxNullPoint;
  ConvertMouseDragEventArgs(Args);
  FLastDragDropEffects := [];
  State.DoDragEnter(Args);
end;

procedure TdxRichEditMouseController.DoDragOver(var Args: TdxDragEventArgs);
var
  P: TPoint;
begin
  P := Args.P;
  ConvertMouseDragEventArgs(Args);
  if not P.IsEqual(FLastDragPoint) then
  begin
    State.DoDragOver(Args);
    FLastDragDropEffects := Args.Effect;
    AutoScroller.DoMouseMove(TControl(Control).ScreenToClient(P));
    FLastDragPoint := P;
  end
  else
    Args.Effect := FLastDragDropEffects;
end;

procedure TdxRichEditMouseController.DoCancelMode;
begin
  (State as IdxRichEditMouseState).DoCancelMode;
end;

function TdxRichEditMouseController.DoContextPopup(var Args: TdxMouseEventArgs): Boolean;
begin
  Result := State.HandlePopupMenu(ConvertMouseEventArgs(Args));
end;

procedure TdxRichEditMouseController.DoDragDrop(var Args: TdxDragEventArgs);
begin
  ConvertMouseDragEventArgs(Args);
  State.DoDragDrop(Args);
end;

procedure TdxRichEditMouseController.DoDragLeave;
begin
  (State as IdxRichEditMouseState).DoDragLeave;
end;

procedure TdxRichEditMouseController.GiveFeedback(Args: PdxGiveFeedbackEventArgs);
begin
  State.GiveFeedback(Args);
end;

procedure TdxRichEditMouseController.QueryContinueDrag(Args: PdxQueryContinueDragEventArgs);
begin
  State.QueryContinueDrag(Args);
end;

function TdxRichEditMouseController.GetActiveView: TdxRichEditView;
begin
  Result := InnerControl.ActiveView;
end;

function TdxRichEditMouseController.GetButtonsFromShiftState(Shift: TShiftState): TdxMouseButtons;
begin
  Result := [];
  if ssLeft in Shift then
    Include(Result, mbLeft);
  if ssRight in Shift then
    Include(Result, mbRight);
  if ssMiddle in Shift then
    Include(Result, mbMiddle);
end;

function TdxRichEditMouseController.GetDocumentModel: TdxDocumentModel;
begin
  Result := InnerControl.DocumentModel;
end;

function TdxRichEditMouseController.GetHitInfo: TdxMouseEventArgs;
var
  ACount: Integer;
begin
  ACount := Length(FHitInfos);
  if ACount > 0 then
    Result := FHitInfos[ACount - 1]
  else
  begin
    FillChar(Result, SizeOf(TdxMouseEventArgs), 0);
    Result.MousePos := cxInvalidPoint;
  end;
end;

function TdxRichEditMouseController.GetInnerControl: IdxInnerControl;
begin
  Result := Control.InnerControl;
end;

function TdxRichEditMouseController.GetState: TdxRichEditMouseCustomState;
begin
  if FState = nil then
    Result := nil
  else
    Result := FState.State;
end;

{ TdxRichEditMouseDefaultStateController }

procedure TdxRichEditMouseDefaultState.HandleMouseDown(const P: TPoint);
var
  AHitTestResult: TdxRichEditHitTestResult;
begin
  AHitTestResult := CalculateHitTest(P);
  try
    if AHitTestResult = nil then
    begin
      AHitTestResult := Controller.ActiveView.CalculateNearestPageHitTest(P, False);
      if AHitTestResult <> nil then
      begin
        if not HandleHotZoneMouseDown(AHitTestResult) then
        begin
          if ShouldSelectFloatingObject(AHitTestResult) then
            SelectFloatingObject(AHitTestResult, True)
          else
            if ShouldDeactivateFloatingObject(AHitTestResult) then
              DeactivateFloatingObject(AHitTestResult);
        end;
      end;
    end
    else
      HandleMouseDown(AHitTestResult);
  finally
    AHitTestResult.Free;
  end;
end;

procedure TdxRichEditMouseDefaultState.HandleMouseDown(const AHitTestResult: TdxRichEditHitTestResult);
var
  ASelectionManager: TdxEnhancedSelectionManager;
  ATableRow: TdxTableRowViewInfoBase;
  AVirtualColumn: TdxVirtualTableColumn;
begin
  if HandleHotZoneMouseDown(AHitTestResult) then
    Exit;

  ASelectionManager := TdxEnhancedSelectionManager.Create(ActivePieceTable);
  try
    ATableRow := ASelectionManager.CalculateTableRowToResize(AHitTestResult);
    if ASelectionManager.ShouldResizeTableRow(Control, AHitTestResult, ATableRow) then
    begin
      BeginResizeTableRowState(AHitTestResult, ATableRow);
      Exit;
    end;

    AVirtualColumn := ASelectionManager.CalculateTableCellsToResizeHorizontally(AHitTestResult);
    if ASelectionManager.ShouldResizeTableCellsHorizontally(Control, AHitTestResult, AVirtualColumn) then
    begin
      BeginResizeTableCellsHorizontallyState(AHitTestResult, AVirtualColumn);
      Exit;
    end
    else
      AVirtualColumn.Free;

    if ASelectionManager.ShouldSelectEntireTableColumn(AHitTestResult) then
      BeginTableColumnsSelectionState(AHitTestResult)
    else if ASelectionManager.ShouldSelectEntireTableRow(AHitTestResult) then
      BeginTableRowsSelectionState(AHitTestResult)
    else if ASelectionManager.ShouldSelectEntireTableCell(AHitTestResult) then
      BeginTableCellsSelectionState(AHitTestResult)
    else if ShouldSelectFloatingObject(AHitTestResult) then
      SelectFloatingObject(AHitTestResult, True)
    else if ASelectionManager.ShouldSelectEntireRow(AHitTestResult) then
      BeginLineSelection(AHitTestResult)
    else if ShouldStartMultiSelection then
      BeginMultiSelection(AHitTestResult)
    else
      if ShouldExtendSelectionToCursor then
      begin
        if TryProcessHyperlinkClick(AHitTestResult) then
          Exit;
        ExtendSelectionToCursor(AHitTestResult);
      end
      else
        if ShouldSelectPicture(AHitTestResult) then
        begin
          if TryProcessHyperlinkClick(AHitTestResult) then
            Exit;
          SelectPicture(AHitTestResult);
        end
        else
        begin
          if AHitTestResult.PieceTable = ActivePieceTable then
            BeginCharacterSelection(AHitTestResult)
          else
          begin
            if ShouldDeactivateFloatingObject(AHitTestResult) then
              DeactivateFloatingObject(AHitTestResult);
          end;
        end;
  finally
    ASelectionManager.Free;
  end;
end;

procedure TdxRichEditMouseDefaultState.HandleMouseDown(
  const Args: TdxMouseEventArgs);
begin
  if [mbLeft] = Args.Buttons then
    HandleMouseDown(Args.MousePos);
end;

procedure TdxRichEditMouseDefaultState.HandleMouseDoubleClick(const Args: TdxMouseEventArgs);
var
  APhysicalPoint: TPoint;
  AHitTestResult: TdxRichEditHitTestResult;
  ACommand: TdxChangeActivePieceTableCommand;
begin
  APhysicalPoint := Args.MousePos;
  AHitTestResult := CalculateExactPageAreaHitTest(APhysicalPoint);
  try
    if (AHitTestResult <> nil) and ShouldSwitchActivePieceTable(AHitTestResult) then
    begin
      ACommand := TdxChangeActivePieceTableCommand.Create(Control, TdxPieceTable(AHitTestResult.PageArea.PieceTable),
        AHitTestResult.PageArea.Section, AHitTestResult.Page.PageIndex);
      try
        ACommand.Execute;
      finally
        ACommand.Free;
      end;
      Exit;
    end;
  finally
    AHitTestResult.Free;
  end;
  AHitTestResult := CalculateHitTest(APhysicalPoint);
  try
    if AHitTestResult = nil then
      Exit;
    if AHitTestResult.PieceTable <> ActivePieceTable then
      Exit;
    PerformEnhancedSelection(AHitTestResult);
  finally
    AHitTestResult.Free;
  end;
end;

procedure TdxRichEditMouseDefaultState.HandleMouseTripleClick(const Args: TdxMouseEventArgs);
var
  P: TPoint;
  AHitTestResult: TdxRichEditHitTestResult;
  ASelectionManager: TdxEnhancedSelectionManager;
  ACommand: TdxSelectAllCommand;
begin
  P := Args.MousePos;
  AHitTestResult := CalculateHitTest(P);
  try
    if AHitTestResult = nil then
      Exit;
    if AHitTestResult.PieceTable <> ActivePieceTable then
      Exit;
    ASelectionManager := TdxEnhancedSelectionManager.Create(ActivePieceTable);
    try
      if ASelectionManager.ShouldSelectEntireRow(AHitTestResult) then
      begin
        ACommand := TdxSelectAllCommand.Create(Control);
        try
          ACommand.Execute;
        finally
          ACommand.Free;
        end;
      end
      else
        BeginParagraphsSelection(AHitTestResult);
    finally
      ASelectionManager.Free;
    end;
  finally
    AHitTestResult.Free;
  end;
end;

procedure TdxRichEditMouseDefaultState.HandleMouseMoveCore(const Args: TdxMouseEventArgs;
  const P: TPoint; AHitTestResult: TdxRichEditHitTestResult);
var
  ACalculator: TdxMouseCursorCalculator;
begin
  inherited HandleMouseMoveCore(Args, P, AHitTestResult);
  if IsHyperlinkActive and InnerControl.IsHyperlinkModifierKeysPress then
    SetMouseCursor(TdxRichEditCursors.Hand)
  else
  begin
    ACalculator := Control.InnerControl.CreateMouseCursorCalculator;
    try
      SetMouseCursor(ACalculator.Calculate(AHitTestResult, P));
    finally
      ACalculator.Free;
    end;
  end;
end;

function TdxRichEditMouseDefaultState.IsExternalDrag(
  const ADataObject: IDataObject): Boolean;
begin
  Result := not Supports(ADataObject, IdxDataObject);
end;

function TdxRichEditMouseDefaultState.IsHyperlinkActive: Boolean;
begin
  Result := (Controller.ActiveObject is TdxField) and
    DocumentModel.ActivePieceTable.IsHyperlinkField(TdxField(Controller.ActiveObject));
end;

function TdxRichEditMouseDefaultState.ActivateFloatingObjectTextBox(ARun: TdxFloatingObjectAnchorRun;
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  APhysicalPoint: TPoint;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
begin
  if ARun = nil then
    Exit(False);
  APhysicalPoint := AHitTestResult.PhysicalPoint;
  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  if ATextBoxContent <> nil then
  begin
    Control.BeginUpdate;
    try
      Controller.ChangeActivePieceTable(TdxPieceTable(ATextBoxContent.TextBox.PieceTable), AHitTestResult);
      ClearMultiSelection;
      PlaceCaretToPhysicalPoint(APhysicalPoint);
    finally
      Control.EndUpdate;
    end;
    Exit(True);
  end;
  Result := False;
end;

function TdxRichEditMouseDefaultState.GetAutoScrollEnabled: Boolean;
begin
  Result := False;
end;

function TdxRichEditMouseDefaultState.CanShowToolTip: Boolean;
begin
  Result := True;
end;

procedure TdxRichEditMouseDefaultState.BeginCharacterSelection(AHitTestResult: TdxRichEditHitTestResult);
begin
  if ShouldBeginDragExistingSelection(AHitTestResult) then
    BeginDragExistingSelection(AHitTestResult, True)
  else
  begin
    ClearMultiSelection;
    BeginCharacterSelectionCore(AHitTestResult);
  end;
end;

procedure TdxRichEditMouseDefaultState.BeginCharacterSelectionCore(
  AHitTestResult: TdxRichEditHitTestResult);
var
  ADragState: TdxRichEditMouseCustomState;
  ANewState: TdxBeginMouseDragHyperlinkClickHandleHelperState;
  ACommand: TdxPlaceCaretToPhysicalPointCommand;
begin
  ADragState := TdxContinueSelectionByCharactersMouseHandlerState.Create(Controller, TdxBoxAccessorKind.Character,
    AHitTestResult);
  ANewState := TdxBeginMouseDragHyperlinkClickHandleHelperState.Create(Controller, ADragState,
    AHitTestResult.PhysicalPoint);
  LockRelease;
  try
    Controller.SwitchStateCore(ANewState, AHitTestResult.PhysicalPoint);
    ACommand := TdxPlaceCaretToPhysicalPointCommand2.Create(Control);
    try
      ACommand.SuppressClearOutdatedSelectionItems := True;
      ACommand.PhysicalPoint := AHitTestResult.PhysicalPoint;
      ACommand.ExecuteCore;
    finally
      ACommand.Free;
    end;
  finally
    UnlockRelease;
  end;
end;

procedure TdxRichEditMouseDefaultState.BeginDragExistingSelection(
  AHitTestResult: TdxRichEditHitTestResult; AResetSelectionOnMouseUp: Boolean);
var
  ADragState: TdxRichEditMouseCustomState;
  ANewState: TdxBeginContentDragHelperState;
begin
  if Controller.DeactivateTextBoxPieceTableIfNeed(AHitTestResult.PieceTable, AHitTestResult) then
  begin
    HandleMouseDown(AHitTestResult.PhysicalPoint);
    Exit;
  end;
  if not Control.UseStandardDragDropMode and
    (not Control.InnerControl.Options.Behavior.DragAllowed or not Control.InnerControl.Options.Behavior.DropAllowed) then
    Exit;
  ADragState := CreateDragContentState(AHitTestResult);
  ANewState := TdxBeginContentDragHelperState.Create(Controller, ADragState, AHitTestResult.PhysicalPoint);
  ANewState.ResetSelectionOnMouseUp := AResetSelectionOnMouseUp;
  Controller.SwitchStateCore(ANewState, AHitTestResult.PhysicalPoint);
end;

procedure TdxRichEditMouseDefaultState.BeginLineSelection(AHitTestResult: TdxRichEditHitTestResult);
var
  ACommand: TdxSelectLineCommand;
  ADragState: TdxRichEditMouseCustomState;
begin
  ACommand := TdxSelectLineCommand.Create(Control);
  try
    ACommand.PhysicalPoint := AHitTestResult.PhysicalPoint;
    ACommand.ExecuteCore;
    ClearMultiSelection;
    ADragState := TdxContinueSelectionByLinesMouseHandlerState.Create(Controller, TdxBoxAccessorKind.Row, AHitTestResult);
    SwitchStateToDragState(AHitTestResult, ADragState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxRichEditMouseDefaultState.BeginMultiSelection(AHitTestResult: TdxRichEditHitTestResult);
begin
  if AHitTestResult.PieceTable <> ActivePieceTable then
    Exit;
  DocumentModel.Selection.BeginMultiSelection(ActivePieceTable);
  BeginCharacterSelectionCore(AHitTestResult);
end;

procedure TdxRichEditMouseDefaultState.BeginParagraphsSelection(AHitTestResult: TdxRichEditHitTestResult);
var
  ADragState: TdxRichEditMouseCustomState;
  ANewState: TdxBeginMouseDragHelperState;
  AIterator: TdxParagraphsDocumentModelIterator;
begin
  ADragState := TdxContinueSelectionByParagraphsMouseHandlerState.Create(Controller, TdxBoxAccessorKind.Character, AHitTestResult);
  ANewState := TdxRichEditBeginMouseDragHelperState.Create(Controller, ADragState, AHitTestResult.PhysicalPoint);
  Controller.SwitchStateCore(ANewState, AHitTestResult.PhysicalPoint);
  AIterator := TdxParagraphsDocumentModelIterator.Create(ActivePieceTable);
  try
    ExtendSelection(AIterator);
  finally
    AIterator.Free;
  end;
end;

procedure TdxRichEditMouseDefaultState.BeginTableRowsSelectionState(AHitTestResult: TdxRichEditHitTestResult);
var
  ACommand: TdxSelectTableRowCommand;
  ARow: TdxTableRow;
  ARestartCell: TdxTableCell;
  ARowIndex: Integer;
  ADragState: TdxContinueSelectionByStartTableRowsMouseHandlerState;
begin
  if ShouldStartMultiSelection then
    DocumentModel.Selection.BeginMultiSelection(ActivePieceTable);
  ACommand := TdxSelectTableRowCommand.Create(Control);
  try
    ACommand.CanCalculateExecutionParameters := False;
    ACommand.ShouldEnsureCaretVisibleVerticallyAfterUpdate := False;
    ARow := AHitTestResult.TableRow.Row;
    ACommand.Rows := ARow.Table.Rows;
    ARestartCell := ARow.Table.GetFirstCellInVerticalMergingGroup(ARow.FirstCell);
    ARowIndex := ARestartCell.Row.IndexInTable;
    ACommand.StartRowIndex := ARowIndex;
    ACommand.EndRowIndex := ARowIndex;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
  ADragState := TdxContinueSelectionByStartTableRowsMouseHandlerState.Create(Controller,
    TdxBoxAccessorKind.Row, AHitTestResult);
  ADragState.StartRowIndex := ARowIndex;
  SwitchStateToDragState(AHitTestResult, ADragState);
end;

procedure TdxRichEditMouseDefaultState.BeginWordsSelection(AHitTestResult: TdxRichEditHitTestResult);
var
  ADragState: TdxRichEditMouseCustomState;
  ANewState: TdxBeginMouseDragHelperState;
  AIterator: TdxWordsDocumentModelIterator;
  APosition: TdxDocumentModelPosition;
begin
  ADragState := TdxContinueSelectionByWordsMouseHandlerState.Create(Controller, TdxBoxAccessorKind.Character, AHitTestResult);
  ANewState := TdxRichEditBeginMouseDragHelperState.Create(Controller, ADragState, AHitTestResult.PhysicalPoint);
  Controller.SwitchStateCore(ANewState, AHitTestResult.PhysicalPoint);
  AIterator := TdxWordsDocumentModelIterator.Create(ActivePieceTable);
  try
    if ShouldStartMultiSelection then
    begin
      if AHitTestResult.PieceTable <> ActivePieceTable then
        Exit;
      APosition := TdxBoxAccessor.GetBox(AHitTestResult, TdxBoxAccessorKind.Character).GetFirstPosition(ActivePieceTable);
      ExtendMultiSelection(AIterator, APosition);
    end
    else
      ExtendSelection(AIterator);
  finally
    AIterator.Free;
  end;
end;

function TdxRichEditMouseDefaultState.CalculateExactPageAreaHitTest(const P: TPoint): TdxRichEditHitTestResult;
var
  ARequest: TdxRichEditHitTestRequest;
begin
  ARequest := TdxRichEditHitTestRequest.Create(ActivePieceTable);
  ARequest.PhysicalPoint := P;
  ARequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Character;
  ARequest.Accuracy := ExactPage or ExactPageArea or NearestColumn or NearestTableRow or
    NearestTableCell or NearestRow or NearestBox or NearestCharacter;
  ARequest.SearchAnyPieceTable := True;

  Result := Controller.ActiveView.HitTestCore(ARequest, True);
  if not Result.IsValid(TdxDocumentLayoutDetailsLevel.PageArea) then
    FreeAndNil(Result);
end;

procedure TdxRichEditMouseDefaultState.ClearMultiSelection;
begin
  DocumentModel.Selection.ClearMultiSelection;
end;

function TdxRichEditMouseDefaultState.CreateDragContentState(
  AHitTestResult: TdxRichEditHitTestResult): TdxRichEditMouseCustomState;
begin
  if (AHitTestResult.FloatingObjectBox <> nil) and not Controller.IsInlinePictureBoxHit(AHitTestResult) then
  begin
    Result := TdxDragFloatingObjectManuallyMouseHandlerState.Create(Controller, AHitTestResult);
    Exit;
  end;
  if Control.UseStandardDragDropMode then
    Result := TdxDragContentStandardMouseHandlerState.Create(Controller)
  else
    Result := TdxDragContentManuallyMouseHandlerState.Create(Controller);
end;

function TdxRichEditMouseDefaultState.CreateDragState(
  const ADataObject: IDataObject): TdxRichEditMouseCustomState;
begin
  if IsExternalDrag(ADataObject) then
    Result :=  CreateExternalDragState
  else
    Result := CreateInternalDragState;
end;

function TdxRichEditMouseDefaultState.CreateExternalDragState: TdxRichEditMouseCustomState;
begin
  Result := TdxDragExternalContentMouseHandlerState.Create(Controller);
end;

function TdxRichEditMouseDefaultState.CreateInternalDragState: TdxRichEditMouseCustomState;
begin
  Result := Controller.CreateInternalDragState;
end;

procedure TdxRichEditMouseDefaultState.DeactivateFloatingObject(AHitTestResult: TdxRichEditHitTestResult);
var
  AFloatingObjectBox: TdxFloatingObjectBox;
  ATextBox: TdxTextBoxContentType;
  ARun: TdxFloatingObjectAnchorRun;
begin
  AFloatingObjectBox := AHitTestResult.FloatingObjectBox;
  if AFloatingObjectBox = nil then
  begin
    Control.BeginUpdate;
    try
      if ActivePieceTable.IsTextBox then
      begin
        ATextBox := TdxTextBoxContentType(ActivePieceTable.ContentType);
        Controller.ChangeActivePieceTable(TdxPieceTable(ATextBox.AnchorRun.PieceTable), AHitTestResult);
      end
      else
        Controller.ChangeActivePieceTable(DocumentModel.MainPieceTable);
      ClearMultiSelection;
      PlaceCaretToPhysicalPoint(AHitTestResult.PhysicalPoint);

      HandleMouseDown(AHitTestResult.PhysicalPoint);
    finally
      Control.EndUpdate;
    end;
  end
  else
  begin
    ARun := AFloatingObjectBox.GetFloatingObjectRun;
    if ARun.PieceTable <> ActivePieceTable then
      SelectFloatingObject(AHitTestResult, False);
  end;
end;

procedure TdxRichEditMouseDefaultState.DoDragEnter(var Args: TdxDragEventArgs);
var
  ADataObject: IDataObject;
  AState: TdxDragContentMouseHandlerStateBase;
  AIsCommandEnabled: Boolean;
begin
  inherited DoDragEnter(Args);
  ADataObject := Args.Data;
  if ADataObject = nil then
    Exit;
  AState := CreateDragState(ADataObject) as TdxDragContentMouseHandlerStateBase;
  AIsCommandEnabled := Control.InnerControl.Options.Behavior.DropAllowed;
  if AIsCommandEnabled and AState.CanDropData(Args) then
    Controller.SwitchStateCore(AState, Point(Args.X, Args.Y))
  else
    AState.Free;
end;

procedure TdxRichEditMouseDefaultState.BeginResizeTableRowState(AHitTestResult: TdxRichEditHitTestResult; ATableRow: TdxTableRowViewInfoBase);
var
  AResizeState: TdxResizeTableRowMouseHandlerState;
begin
  AResizeState := TdxResizeTableRowMouseHandlerState.Create(Controller, AHitTestResult, ATableRow);
  SwitchStateToDragState(AHitTestResult, AResizeState);
end;

procedure TdxRichEditMouseDefaultState.BeginResizeTableCellsHorizontallyState(AHitTestResult: TdxRichEditHitTestResult; AColumn: TdxVirtualTableColumn);
var
  AResizeState: TdxResizeTableVirtualColumnMouseHandlerState;
begin
  AResizeState := TdxResizeTableVirtualColumnMouseHandlerState.Create(Controller, AHitTestResult, AColumn);
  SwitchStateToDragState(AHitTestResult, AResizeState);
end;

procedure TdxRichEditMouseDefaultState.BeginTableColumnsSelectionState(AHitTestResult: TdxRichEditHitTestResult);
var
  ACommand: TdxSelectTableColumnsCommand;
  ATableCell: TdxTableCell;
  AStartColumnIndex, AEndColumnIndex: Integer;
  ADragState: TdxContinueSelectionByTableColumnsMouseHandlerState;
begin
  if ShouldStartMultiSelection then
    DocumentModel.Selection.BeginMultiSelection(ActivePieceTable);

  ACommand := TdxSelectTableColumnsCommand.Create(Control);
  try
    ACommand.Rows := AHitTestResult.TableRow.Row.Table.Rows;
    ATableCell := AHitTestResult.TableCell.Cell;
    AStartColumnIndex := ATableCell.GetStartColumnIndexConsiderRowGrid;
    AEndColumnIndex := ATableCell.GetEndColumnIndexConsiderRowGrid(AStartColumnIndex);
    ACommand.StartColumnIndex := AStartColumnIndex;
    ACommand.EndColumnIndex := AEndColumnIndex;
    ACommand.CanCalculateExecutionParameters := False;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;

  ADragState := TdxContinueSelectionByTableColumnsMouseHandlerState.Create(Controller, TdxBoxAccessorKind.Box, AHitTestResult);
  ADragState.StartColumnIndex := AStartColumnIndex;
  ADragState.NestedLevel := ATableCell.Table.NestedLevel;
  SwitchStateToDragState(AHitTestResult, ADragState);
end;

procedure TdxRichEditMouseDefaultState.BeginTableCellsSelectionState(AHitTestResult: TdxRichEditHitTestResult);
var
  ACommand: TdxSelectTableCellCommand;
  ADragState: TdxRichEditMouseCustomState;
begin
  if ShouldStartMultiSelection then
    DocumentModel.Selection.BeginMultiSelection(ActivePieceTable);

  ACommand := TdxSelectTableCellCommand.Create(Control);
  try
    ACommand.Cell := AHitTestResult.TableCell.Cell;
    ACommand.ShouldEnsureCaretVisibleVerticallyBeforeUpdate := False;
    ACommand.ShouldEnsureCaretVisibleVerticallyAfterUpdate := False;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;

  ADragState := TdxContinueSelectionByStartTableCellsMouseHandlerState.Create(Controller, TdxBoxAccessorKind.Box, AHitTestResult);
  SwitchStateToDragState(AHitTestResult, ADragState);
end;




function TdxRichEditMouseDefaultState.CreatePlaceCaretToPhysicalPointCommand(AShift: Boolean): TdxPlaceCaretToPhysicalPointCommand;
begin
  if AShift then
    Exit(TdxExtendSelectionToPhysicalPointCommand.Create(Control))
  else
    Exit(TdxPlaceCaretToPhysicalPointCommand2.Create(Control));
end;

function TdxRichEditMouseDefaultState.HandlePopupMenu(const Args: TdxMouseEventArgs): Boolean;
var
  AHitTestResult: TdxRichEditHitTestResult;
  AFloatingObjectBox: TdxFloatingObjectBox;
  ALogPosition: TdxDocumentLogPosition;
begin
  if not (mbRight in Args.Buttons) then
    Exit(False);
  AHitTestResult := CalculateHitTest(Args.MousePos);
  try
    if AHitTestResult = nil then
       Exit(True);

    AFloatingObjectBox := AHitTestResult.FloatingObjectBox;
    if ShouldSelectFloatingObject(AHitTestResult) or ((AFloatingObjectBox <> nil) and not ActivePieceTable.IsTextBox) then
    begin
      if not IsFloatingObjectSelected(AFloatingObjectBox) then
        SelectFloatingObject(AHitTestResult, False);
      Exit(True);
    end;
    if AHitTestResult.PieceTable <> ActivePieceTable then
      Exit(False);
    ALogPosition := AHitTestResult.Character.GetFirstPosition(ActivePieceTable).LogPosition;
    if IsMouseDownInSelection(ALogPosition) then
      Exit(True);
    ClearMultiSelection;
    PlaceCaretToPhysicalPoint(AHitTestResult.PhysicalPoint);
    Result := True;
  finally
    AHitTestResult.Free;
  end;
end;

function TdxRichEditMouseDefaultState.IsFloatingObjectSelected(AFloatingObjectBox: TdxFloatingObjectBox): Boolean;
var
  ASelection: TdxSelection;
  ARun: TdxFloatingObjectAnchorRun;
  ALogPosition: TdxDocumentLogPosition;
begin
  ASelection := DocumentModel.Selection;
  ARun := AFloatingObjectBox.GetFloatingObjectRun;
  if ASelection.PieceTable <> ARun.PieceTable then
    Exit(False);
  ALogPosition := ARun.PieceTable.GetRunLogPosition(AFloatingObjectBox.StartPos.RunIndex);
  Result := (ASelection.Start = ALogPosition) and (ASelection.&End = (ALogPosition + 1));
end;

function TdxRichEditMouseDefaultState.IsMouseDownInSelection(ALogPosition: TdxDocumentLogPosition): Boolean;
var
  ASelection: TdxSelection;
  ASelectionsCount, I: Integer;
  ASelectionItem: TdxSelectionItem;
begin
  ASelection := DocumentModel.Selection;
  ASelectionsCount := ASelection.Items.Count;
  for I := 0 to ASelectionsCount - 1 do
  begin
    ASelectionItem := ASelection.Items[I];
    if (ASelectionItem.NormalizedStart <= ALogPosition) and (ALogPosition <= ASelectionItem.NormalizedEnd) then
      Exit(True);
  end;
  Result := False;
end;

procedure TdxRichEditMouseDefaultState.ExtendSelection(
  AIterator: TdxPieceTableIterator);
var
  ADocumentModel: TdxDocumentModel;
  ACurrentModelPosition, AStart, AEnd: TdxDocumentModelPosition;
begin
  ADocumentModel := DocumentModel;
  ACurrentModelPosition := ADocumentModel.Selection.Interval.Start;
  if AIterator.IsNewElement(ACurrentModelPosition) then
    AStart := ACurrentModelPosition
  else
    AStart := AIterator.MoveBack(ACurrentModelPosition);
  AEnd := AIterator.MoveForward(ACurrentModelPosition);
  SetSelection(AStart.LogPosition, AEnd.LogPosition);
end;

procedure TdxRichEditMouseDefaultState.ExtendSelectionToCursor(AHitTestResult: TdxRichEditHitTestResult);
var
  ACommand: TdxPlaceCaretToPhysicalPointCommand;
begin
  if AHitTestResult.PieceTable <> ActivePieceTable then
    Exit;
  ACommand := TdxExtendSelectionToPhysicalPointCommand.Create(Control);
  try
    ACommand.PhysicalPoint := AHitTestResult.PhysicalPoint;
    ACommand.ExecuteCore;
  finally
    ACommand.Free;
  end;
end;

procedure TdxRichEditMouseDefaultState.ExtendMultiSelection(AIterator: TdxPieceTableIterator;
  const ACurrentModelPosition: TdxDocumentModelPosition);
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  if AIterator.IsNewElement(ACurrentModelPosition) then
    AStart := ACurrentModelPosition
  else
    AStart := AIterator.MoveBack(ACurrentModelPosition);
  AEnd := AIterator.MoveForward(ACurrentModelPosition);
  DocumentModel.Selection.BeginMultiSelection(ActivePieceTable);
  SetSelection(AStart.LogPosition, AEnd.LogPosition, False);
end;

procedure TdxRichEditMouseDefaultState.PerformEnhancedSelection(AHitTestResult: TdxRichEditHitTestResult);
var
  ASelectionManager: TdxEnhancedSelectionManager;
begin
  ASelectionManager := TdxEnhancedSelectionManager.Create(ActivePieceTable);
  try
    if ASelectionManager.ShouldSelectEntireTableCell(AHitTestResult) then
      BeginTableRowsSelectionState(AHitTestResult)
    else
      if ASelectionManager.ShouldSelectEntireRow(AHitTestResult) then
        BeginParagraphsSelection(AHitTestResult)
      else
        BeginWordsSelection(AHitTestResult);
  finally
    ASelectionManager.Free;
  end;
end;

procedure TdxRichEditMouseDefaultState.PlaceCaretToPhysicalPoint(const APhysicalPoint: TPoint);
var
  APlaceCaretCommand: TdxPlaceCaretToPhysicalPointCommand;
begin
  APlaceCaretCommand := TdxPlaceCaretToPhysicalPointCommand2.Create(Control);
  try
    APlaceCaretCommand.PhysicalPoint := APhysicalPoint;
    APlaceCaretCommand.ExecuteCore;
  finally
    APlaceCaretCommand.Free;
  end;
end;

procedure TdxRichEditMouseDefaultState.SelectFloatingObject(AHitTestResult: TdxRichEditHitTestResult; AAllowDrag: Boolean; AAllowActivate: Boolean = True);
var
  AFloatingObjectBox: TdxFloatingObjectBox;
  ARun: TdxFloatingObjectAnchorRun;
  ALogPosition, AEndLogPosition: TdxDocumentLogPosition;
  AParagraph: TdxSimpleParagraph;
  ACurrentFrameProperties, ANextFrameProperties: TdxMergedFrameProperties;
  ANextParagraphIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
  AIsParagraphFrame: Boolean;
begin
  Assert(AHitTestResult.FloatingObjectBox <> nil);
  AFloatingObjectBox := AHitTestResult.FloatingObjectBox;
  ARun := AFloatingObjectBox.GetFloatingObjectRun;

  ALogPosition := ARun.PieceTable.GetRunLogPosition(AFloatingObjectBox.StartPos.RunIndex);
  AEndLogPosition := ALogPosition + 1;
  if ActivePieceTable <> ARun.PieceTable then
    Controller.ChangeActivePieceTable(TdxPieceTable(ARun.PieceTable), AHitTestResult);
  AParagraph := ARun.PieceTable.Runs[AFloatingObjectBox.StartPos.RunIndex].Paragraph;
  ACurrentFrameProperties := AParagraph.GetMergedFrameProperties;
  try
    AIsParagraphFrame := (ACurrentFrameProperties <> nil) and ACurrentFrameProperties.IsParagraphFrame;
    if AIsParagraphFrame and not AParagraph.IsInCell then
    begin
      ALogPosition := AParagraph.LogPosition;
      AEndLogPosition := ALogPosition + AParagraph.Length;
      if not AParagraph.IsLast then
      begin
        ANextParagraphIndex := AParagraph.Index + 1;
        AParagraphs := TdxPieceTable(ARun.PieceTable).Paragraphs;
        ANextFrameProperties := AParagraphs[ANextParagraphIndex].GetMergedFrameProperties;
        try
          while ACurrentFrameProperties.CanMerge(ANextFrameProperties) do
          begin
            Inc(AEndLogPosition, AParagraphs[ANextParagraphIndex].Length);
            if AParagraphs.Last.Index <= ANextParagraphIndex + 1 then
            begin
              Inc(ANextParagraphIndex);
              ANextFrameProperties.Free;
              ANextFrameProperties := AParagraphs[ANextParagraphIndex].GetMergedFrameProperties;
            end
            else
              Break;
          end;
        finally
          ANextFrameProperties.Free;
        end;
      end;
    end;
  finally
    ACurrentFrameProperties.Free;
  end;
  SetSelection(ALogPosition, AEndLogPosition);

  if AAllowActivate and ShouldActivateFloatingObjectTextBox(AFloatingObjectBox, ARun, AHitTestResult.LogicalPoint) and
    ActivateFloatingObjectTextBox(ARun, AHitTestResult) then
    AAllowDrag := False;

  if AIsParagraphFrame then
    AAllowDrag := False;

  if AAllowDrag and ShouldBeginDragExistingSelection(AHitTestResult) then
    BeginDragExistingSelection(AHitTestResult, False);
end;

procedure TdxRichEditMouseDefaultState.SelectPicture(AHitTestResult: TdxRichEditHitTestResult);
var
  AStart: TdxDocumentLogPosition;
begin
  AStart := AHitTestResult.Box.GetFirstPosition(ActivePieceTable).LogPosition;
  SetSelection(AStart, AStart + 1);
  if ShouldBeginDragExistingSelection(AHitTestResult) then
    BeginDragExistingSelection(AHitTestResult, False);
end;

procedure TdxRichEditMouseDefaultState.SetSelection(AStart, AEnd: TdxDocumentLogPosition;
  AClearMultiSelection: Boolean = True);
var
  ASelection: TdxSelection;
begin
  DocumentModel.BeginUpdate;
  try
    ASelection := DocumentModel.Selection;
    ASelection.BeginUpdate;
    try
      if AClearMultiSelection then
      begin
        ASelection.ClearMultiSelection;
        ASelection.Start := AStart;
        ASelection.&End := AEnd;
      end
      else
        ASelection.SetInterval(AStart, AEnd);
      ASelection.SetStartCell(AStart);
      ValidateFieldSelection(DocumentModel, ASelection);
    finally
      ASelection.EndUpdate;
    end;
    DocumentModel.ActivePieceTable.ApplyChangesCore([TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
      TdxDocumentModelChangeAction.ResetRuler], -1, -1);
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxRichEditMouseDefaultState.ShouldActivateFloatingObjectTextBox(AFloatingObjectBox: TdxFloatingObjectBox;
  ARun: TdxFloatingObjectAnchorRun; const ALogicalPoint: TPoint): Boolean;
var
  APoint: TPoint;
begin
  if (ARun = nil) or (AFloatingObjectBox.DocumentLayout = nil) then
    Exit(False);
  if not (ARun.Content is TdxTextBoxFloatingObjectContent) then
    Exit(False);
  APoint := AFloatingObjectBox.TransformPointBackward(ALogicalPoint);
  Result := AFloatingObjectBox.GetTextBoxContentBounds.Contains(APoint);
end;

function TdxRichEditMouseDefaultState.ShouldBeginDragExistingSelection(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  APos: TdxDocumentModelPosition;
  ASelectionLayout: TdxSelectionLayout;
begin
  if AHitTestResult = nil then
    Exit(False);

  if (AHitTestResult.FloatingObjectBox <> nil) and not ActivePieceTable.ContentType.IsTextBox then
    APos := AHitTestResult.FloatingObjectBox.GetFirstPosition(ActivePieceTable)
  else
    if AHitTestResult.Character <> nil then
    begin
      if AHitTestResult.Box is TdxInlinePictureBox then
        APos := AHitTestResult.Box.GetFirstPosition(ActivePieceTable)
      else
        APos := AHitTestResult.Character.GetFirstPosition(ActivePieceTable);
    end
    else
      APos := AHitTestResult.Box.GetFirstPosition(ActivePieceTable);

  if not APos.IsValid then
    Exit(False);

  ASelectionLayout := Controller.ActiveView.SelectionLayout;
  Result := ASelectionLayout.HitTest(APos.LogPosition, AHitTestResult.LogicalPoint);
  if Result and (ASelectionLayout is TdxHeaderFooterSelectionLayout) then
    Result := TdxHeaderFooterSelectionLayout(ASelectionLayout).PreferredPageIndex = AHitTestResult.Page.PageIndex;
end;

function TdxRichEditMouseDefaultState.ShouldDeactivateFloatingObject(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  AFloatingObjectBox: TdxFloatingObjectBox;
begin
  if AHitTestResult.DetailsLevel < TdxDocumentLayoutDetailsLevel.Page then
    Exit(False);
  if not ActivePieceTable.IsTextBox then
    Exit(False);
  AFloatingObjectBox := AHitTestResult.FloatingObjectBox;
  Result := (AFloatingObjectBox = nil) or (AFloatingObjectBox.GetFloatingObjectRun.PieceTable <> ActivePieceTable);
end;

function TdxRichEditMouseDefaultState.ShouldSelectFloatingObject(
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  AFloatingObjectBox: TdxFloatingObjectBox;
  ASelectionManager: TdxEnhancedSelectionManager;
begin
  if AHitTestResult.DetailsLevel < TdxDocumentLayoutDetailsLevel.Page then
    Exit(False);

  AFloatingObjectBox := AHitTestResult.FloatingObjectBox;
  if (AFloatingObjectBox = nil) or Controller.IsInlinePictureBoxHit(AHitTestResult) then
    Exit(False);

  if AFloatingObjectBox.PieceTable = ActivePieceTable then
    Exit(True);

  ASelectionManager := TdxEnhancedSelectionManager.Create(ActivePieceTable);
  try
    Result := ASelectionManager.ShouldSelectFloatingObject(AFloatingObjectBox, AHitTestResult.LogicalPoint);
  finally
    ASelectionManager.Free;
  end;
end;

function TdxRichEditMouseDefaultState.ShouldSelectPicture(AHitTestResult: TdxRichEditHitTestResult): Boolean;
begin
  Result := False;
  if AHitTestResult.PieceTable <> ActivePieceTable then
    Exit;
  if AHitTestResult.DetailsLevel < TdxDocumentLayoutDetailsLevel.Box then
    Exit;
  if ((AHitTestResult.Accuracy and ExactBox) = 0) and ((AHitTestResult.Accuracy and ExactCharacter) = 0) then
    Exit;

  Result := AHitTestResult.Box is TdxInlinePictureBox;
end;

function TdxRichEditMouseDefaultState.ShouldStartMultiSelection: Boolean;
begin
  Result := TdxKeyboardHelper.IsControlPressed and (DocumentModel.Selection.Length > 0);
end;

function TdxRichEditMouseDefaultState.ShouldSwitchActivePieceTable(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  ARun: TdxFloatingObjectAnchorRun;
  AContent: TdxTextBoxFloatingObjectContent;
begin
  if (AHitTestResult.PageArea <> nil) and (ActivePieceTable = AHitTestResult.PageArea.PieceTable) then
    Result := False
  else
  begin
    if AHitTestResult.FloatingObjectBox <> nil then
    begin
      ARun := AHitTestResult.FloatingObjectBox.GetFloatingObjectRun;
      AContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
      Result := (AContent <> nil) and (AContent.TextBox.PieceTable <> ActivePieceTable);
    end
    else
      Result := Control.InnerControl.ActiveViewType = TdxRichEditViewType.PrintLayout;
  end;
end;

function TdxRichEditMouseDefaultState.GetStopClickTimerOnStart: Boolean;
begin
  Result := False;
end;

procedure TdxRichEditMouseDefaultState.SwitchStateToDragState(
  AHitTestResult: TdxRichEditHitTestResult;
  ADragState: TdxRichEditMouseCustomState);
var
  ANewState: TdxBeginMouseDragHelperState;
begin
  ANewState := TdxBeginMouseDragHelperState.Create(Controller, ADragState, AHitTestResult.PhysicalPoint);
  ANewState.CancelOnPopupMenu := True;
  ANewState.CancelOnRightMouseUp := True;
  Controller.SwitchStateCore(ANewState, AHitTestResult.PhysicalPoint);
end;

function TdxRichEditMouseDefaultState.HandleHotZoneMouseDown(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  AHotZone: TdxHotZone;
begin
  AHotZone := Controller.ActiveView.SelectionLayout.CalculateHotZone(AHitTestResult, Controller.ActiveView);
  Result := HandleHotZoneMouseDownCore(AHitTestResult, AHotZone);
end;

function TdxRichEditMouseDefaultState.HandleHotZoneMouseDownCore(AHitTestResult: TdxRichEditHitTestResult;
  AHotZone: TdxHotZone): Boolean;
begin
  Result := False;
  if (AHotZone = nil) or not Control.InnerControl.IsEditable then
    Exit;
  Result := True;
  if not AHotZone.BeforeActivate(Controller, AHitTestResult) then
    HandleMouseDown(AHitTestResult.PhysicalPoint)
  else
    AHotZone.Activate(Controller, AHitTestResult);
end;

function TdxRichEditMouseDefaultState.ShouldExtendSelectionToCursor: Boolean;
begin
  Result := TdxKeyboardHelper.IsShiftPressed;
end;

function TdxRichEditMouseDefaultState.TryProcessHyperlinkClick(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  AHandler: TdxHyperlinkMouseClickHandler;
begin
  Result := False;
  if AHitTestResult.PieceTable <> ActivePieceTable then
    Exit;
  AHandler := TdxHyperlinkMouseClickHandler.Create(Control);
  try
    Result := AHandler.TryProcessHyperlinkClick(AHitTestResult);
  finally
    AHandler.Free;
  end;
end;

procedure TdxRichEditMouseDefaultState.ValidateFieldSelection(ADocumentModel: TdxDocumentModel; ASelection: TdxSelection);
var
  AValidator: TdxFieldIsSelectValidator;
begin
  AValidator := TdxFieldIsSelectValidator.Create(ADocumentModel.ActivePieceTable);
  try
    AValidator.ValidateSelection(ASelection);
  finally
    AValidator.Free;
  end;
end;

{ TdxBoxAccessor }

class function TdxBoxAccessor.GetBox(AHitTestResult: TdxRichEditHitTestResult; AKind: TdxBoxAccessorKind): TdxBox;
begin
  case AKind of
    TdxBoxAccessorKind.Box:
      Result := AHitTestResult.Box;
    TdxBoxAccessorKind.Row:
      Result := AHitTestResult.Row;
    else
      Result := AHitTestResult.Character;
  end;
end;

{ TdxContinueSelectionByRangesMouseHandlerState }

constructor TdxContinueSelectionByRangesMouseHandlerState.Create(
  AController: TdxRichEditMouseController;
  AInitialBoxAccessor: TdxBoxAccessorKind;
  AInitialHitTestResult: TdxRichEditHitTestResult);
begin
  inherited Create(AController);
  FInitialHitTestResult := TdxRichEditHitTestResult(AInitialHitTestResult.Clone);
  FInitialBoxAccessor := AInitialBoxAccessor;
  FStartCell := CalculateStartCell;
end;

destructor TdxContinueSelectionByRangesMouseHandlerState.Destroy;
begin
  FreeAndNil(FInitialHitTestResult);
  inherited Destroy;
end;

procedure TdxContinueSelectionByRangesMouseHandlerState.Start;
begin
  inherited Start;
  DocumentModel.EndDocumentUpdate.Add(EndDocumentUpdateHandler);
end;

function TdxContinueSelectionByRangesMouseHandlerState.CanShowToolTip: Boolean;
begin
  Result := True;
end;

procedure TdxContinueSelectionByRangesMouseHandlerState.HandleEndDocumentUpdate(
  const Args: TdxDocumentUpdateCompleteEventArgs);
begin
  EndDocumentUpdateHandler(Self, Args);
end;

procedure TdxContinueSelectionByRangesMouseHandlerState.HandleMouseMove(const Args: TdxMouseEventArgs);
begin
  ContinueSelection(Args);
  SetMouseCursor(CalculateCursor(Args));
end;

procedure TdxContinueSelectionByRangesMouseHandlerState.HandleMouseUp(const Args: TdxMouseEventArgs);
begin
  Controller.SwitchToDefaultState;
end;

procedure TdxContinueSelectionByRangesMouseHandlerState.HandleMouseWheel(const Args: TdxMouseEventArgs);
begin
  inherited HandleMouseWheel(Args);
  ContinueSelection(Args);
end;

procedure TdxContinueSelectionByRangesMouseHandlerState.Finish;
var
  ACommand: TdxChangeFontBackColorByMouseCommand;
begin
  inherited Finish;
  DocumentModel.EndDocumentUpdate.Remove(EndDocumentUpdateHandler);
  if TdxChangeFontBackColorByMouseCommand.IsChangeByMouse then
  begin
    ACommand := TdxChangeFontBackColorByMouseCommand.Create(Control);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
end;

function TdxContinueSelectionByRangesMouseHandlerState.GetAutoScrollEnabled: Boolean;
begin
  Result := True;
end;

function TdxContinueSelectionByRangesMouseHandlerState.CalculateCursor(
  const Args: TdxMouseEventArgs): TCursor;
var
  ACalculator: TdxMouseCursorCalculator;
  AHitTestResult: TdxRichEditHitTestResult;
begin
  ACalculator := Control.InnerControl.CreateMouseCursorCalculator;
  try
    AHitTestResult := CalculateHitTest(Args.MousePos);
    try
      Result := ACalculator.Calculate(AHitTestResult, Args.MousePos);
    finally
      AHitTestResult.Free;
    end;
  finally
    ACalculator.Free;
  end;
end;

function TdxContinueSelectionByRangesMouseHandlerState.CalculateStartCell: TdxTableCell;
var
  ALogPosition: TdxDocumentLogPosition;
begin
  ALogPosition := InitialBox.GetFirstPosition(ActivePieceTable).LogPosition;
  Result := ActivePieceTable.FindParagraph(ALogPosition).GetCell;
end;

procedure TdxContinueSelectionByRangesMouseHandlerState.EndDocumentUpdateHandler(
  ASender: TObject; E: TdxDocumentUpdateCompleteEventArgs);
var
  AChangeActions: TdxDocumentModelChangeActions;
begin
  AChangeActions := E.DeferredChanges.ChangeActions;
  if (AChangeActions * [TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetAllPrimaryLayout]) <> [] then
    FIsInvalidInitialHitTestResult := True;
end;

procedure TdxContinueSelectionByRangesMouseHandlerState.EnforceFormattingComplete;
begin
  Control.InnerControl.ActiveView.EnforceFormattingCompleteForVisibleArea;
end;

function TdxContinueSelectionByRangesMouseHandlerState.GetInitialBox: TdxBox;
begin
  Result := TdxBoxAccessor.GetBox(FInitialHitTestResult, FInitialBoxAccessor);
end;

function TdxContinueSelectionByRangesMouseHandlerState.GetInitialHitTestResult: TdxRichEditHitTestResult;
var
  APhysicalPoint: TPoint;
begin
  if FIsInvalidInitialHitTestResult then
  begin
    EnforceFormattingComplete;
    APhysicalPoint := FInitialHitTestResult.PhysicalPoint;
    FInitialHitTestResult.Free;
    FInitialHitTestResult := GetHitTestResult(APhysicalPoint);
    FIsInvalidInitialHitTestResult := False;
  end;
  Result := FInitialHitTestResult;
end;

function TdxContinueSelectionByRangesMouseHandlerState.GetStopClickTimerOnStart: Boolean;
begin
  Result := True;
end;

procedure TdxContinueSelectionByRangesMouseHandlerState.ContinueSelection(const Args: TdxMouseEventArgs);
var
  ACommand: TdxExtendSelectionByRangesCommandBase;
begin
  ACommand := CreateExtendSelectionCommand;
  try
    ACommand.SuppressClearOutdatedSelectionItems := True;
    ACommand.InitialBox := InitialBox;
    ACommand.PhysicalPoint := Args.MousePos;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxContinueSelectionByRangesMouseHandlerState.GetCurrentCell(const P: TPoint): TdxTableCell;
var
  AParagraph: TdxParagraph;
begin
  AParagraph := GetParagraph(P);
  if AParagraph = nil then
    Result := nil
  else
    Result := AParagraph.GetCell;
end;

function TdxContinueSelectionByRangesMouseHandlerState.GetHitTestResult(const APhysicalPoint: TPoint): TdxRichEditHitTestResult;
var
  ARequest: TdxRichEditHitTestRequest;
begin
  ARequest := TdxRichEditHitTestRequest.Create(ActivePieceTable);
  ARequest.PhysicalPoint := APhysicalPoint;
  ARequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Character;
  ARequest.Accuracy := NearestCharacter;

  Result := Controller.ActiveView.HitTestCore(ARequest, False);
end;

function TdxContinueSelectionByRangesMouseHandlerState.GetParagraph(const P: TPoint): TdxParagraph;
var
  ARequest: TdxRichEditHitTestRequest;
  AHitTestResult: TdxRichEditHitTestResult;
  ALogPosition: TdxDocumentLogPosition;
begin
  ARequest := TdxRichEditHitTestRequest.Create(ActivePieceTable);
  ARequest.PhysicalPoint := P;
  ARequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Character;
  ARequest.Accuracy := NearestCharacter;

  AHitTestResult := InnerControl.ActiveView.HitTestCore(ARequest, False);
  try
    if not AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Character) then
      Result := nil
    else
    begin
      ALogPosition := AHitTestResult.Character.GetFirstPosition(AHitTestResult.PieceTable).LogPosition;
      Result := AHitTestResult.PieceTable.FindParagraph(ALogPosition);
    end;
  finally
    AHitTestResult.Free;
  end;
end;

function TdxContinueSelectionByRangesMouseHandlerState.NeedSwitchStateToTableCellsMouseHandlerState(const P: TPoint): Boolean;
var
  ACurrentParagraph: TdxParagraph;
begin
  ACurrentParagraph := GetParagraph(P);
  if (StartCell = nil) or (ACurrentParagraph = nil) or (ACurrentParagraph.GetCell = nil) then
    Result := False
  else
    Result := NeedSwitchStateToTableCellsMouseHandlerStateCore(ACurrentParagraph);
end;

function TdxContinueSelectionByRangesMouseHandlerState.NeedSwitchStateToTableCellsMouseHandlerStateCore(ACurrentParagraph: TdxParagraph): Boolean;
var
  ACellManager: TdxTableCellsManager;
  ActualCurrentCell, ACellConsiderNestedLevel: TdxTableCell;
begin
  ACellManager := ActivePieceTable.TableCellsManager;
  ActualCurrentCell := ACellManager.GetCellByNestingLevel(ACurrentParagraph.Index, StartCell.Table.NestedLevel);
  ACellConsiderNestedLevel := StartCell;
  if ActualCurrentCell.Table.NestedLevel <> StartCell.Table.NestedLevel then
    ACellConsiderNestedLevel := ACellManager.GetCellByNestingLevel(StartCell.StartParagraphIndex, ActualCurrentCell.Table.NestedLevel);
  Result := (ACellConsiderNestedLevel <> ActualCurrentCell) and (ACellConsiderNestedLevel.Table = ActualCurrentCell.Table);
end;

{ TdxContinueSelectionByCharactersMouseHandlerState }

procedure TdxContinueSelectionByCharactersMouseHandlerState.ContinueSelection(const Args: TdxMouseEventArgs);
var
  APhysicalPoint: TPoint;
  ACurrentCell: TdxTableCell;
  ACellsMouseHandlerState: TdxContinueSelectionByTableCellsMouseHandlerState;
  ANewHitTestResult: TdxRichEditHitTestResult;
  ANestingLevel: Integer;
  ANewState: TdxRichEditMouseCustomState;
begin
  APhysicalPoint := Args.MousePos;
  ACurrentCell := GetCurrentCell(APhysicalPoint);
  if NeedSwitchStateToTableCellsMouseHandlerState(APhysicalPoint) then
  begin
    ACellsMouseHandlerState := TdxContinueSelectionByTableCellsMouseHandlerState.Create(Controller, InitialBoxAccessor, InitialHitTestResult);
    ACellsMouseHandlerState.StartCell := ActivePieceTable.TableCellsManager.GetCellByNestingLevel(StartCell.StartParagraphIndex, ACurrentCell.Table.NestedLevel);
    Controller.SwitchStateCore(ACellsMouseHandlerState, APhysicalPoint);
    Exit;
  end;
  if (((ACurrentCell <> nil) and (StartCell = nil))) or (StartCell <> nil) and (ACurrentCell <> nil) and
    (StartCell.Table.NestedLevel < ACurrentCell.Table.NestedLevel) then
  begin
    ANewHitTestResult := GetHitTestResult(APhysicalPoint);
    try
      if (ANewHitTestResult.Accuracy and ExactTableRow) = 0 then
        Exit;

      if StartCell = nil then
        ANestingLevel := 0
      else
        ANestingLevel := ANewHitTestResult.TableRow.Row.Table.NestedLevel;

      ANewState := TdxContinueSelectionByTableRowsAfterCharactersMouseHandlerState.Create(Controller, InitialBoxAccessor, InitialHitTestResult, ANestingLevel);
      Controller.SwitchStateCore(ANewState, APhysicalPoint);
      Exit;
    finally
      ANewHitTestResult.Free;
    end;
  end;
  inherited ContinueSelection(Args);
end;

function TdxContinueSelectionByCharactersMouseHandlerState.CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase;
begin
  Result := TdxExtendSelectionByCharactersCommand.Create(Control);
end;

{ TdxContinueSelectionByTableRowsMouseHandlerStateBase }

constructor TdxContinueSelectionByTableRowsMouseHandlerStateBase.Create(AController: TdxRichEditMouseController;
  AInitialBoxAccessor: TdxBoxAccessorKind; AInitialHitTestResult: TdxRichEditHitTestResult; ANestingLevel: Integer);
begin
  inherited Create(AController, AInitialBoxAccessor, AInitialHitTestResult);
  FNestingLevel := ANestingLevel;
end;

function TdxContinueSelectionByTableRowsMouseHandlerStateBase.CalculateCursor(const E: TdxMouseEventArgs): TCursor;
begin
  Result := TdxRichEditCursors.SelectRow;
end;

function TdxContinueSelectionByTableRowsMouseHandlerStateBase.CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase;
var
  ACommand: TdxExtendSelectionByTableRowsCommand;
  AInitialBoxLogPosition: TdxDocumentLogPosition;
begin
  ACommand := TdxExtendSelectionByTableRowsCommand.Create(Control, NestingLevel);
  AInitialBoxLogPosition := InitialBox.GetFirstPosition(ActivePieceTable).LogPosition;
  if StartCell = nil then
    ACommand.StartLogPosition := AInitialBoxLogPosition
  else
    ACommand.StartLogPosition := DocumentModel.Selection.SelectedCells.OriginalStartLogPosition;
  ACommand.EndLogPosition := DocumentModel.Selection.NormalizedEnd;
  Result := ACommand;
end;

procedure TdxContinueSelectionByTableRowsMouseHandlerStateBase.ContinueSelection(const E: TdxMouseEventArgs);
var
  AMousePosition: TPoint;
begin
  AMousePosition := E.MousePos;
  if NeedSwitchState(AMousePosition) then
    SwitchState(AMousePosition)
  else
    inherited ContinueSelection(E);
end;

function TdxContinueSelectionByTableRowsMouseHandlerStateBase.NeedSwitchState(const AMousePosition: TPoint): Boolean;
var
  ACurrentCell: TdxTableCell;
begin
  ACurrentCell := GetCurrentCell(AMousePosition);
  Result := (ACurrentCell = nil) or ((StartCell <> nil) and (StartCell.Table = ACurrentCell.Table));
end;

{ TdxContinueSelectionByStartTableRowsMouseHandlerState }

constructor TdxContinueSelectionByStartTableRowsMouseHandlerState.Create(
  AController: TdxRichEditMouseController; AInitialBoxAccessor: TdxBoxAccessorKind;
  AInitialHitTestResult: TdxRichEditHitTestResult);
begin
  inherited Create(AController, AInitialBoxAccessor, AInitialHitTestResult,
    AInitialHitTestResult.TableRow.Row.Table.NestedLevel);
end;

function TdxContinueSelectionByStartTableRowsMouseHandlerState.CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase;
var
  ACommand: TdxExtendSelectionByStartTableRowsCommand;
begin
  ACommand := TdxExtendSelectionByStartTableRowsCommand.Create(Control, NestingLevel);
  ACommand.StartRowIndex := StartRowIndex;
  Result := ACommand;
end;

function TdxContinueSelectionByStartTableRowsMouseHandlerState.NeedSwitchState(const AMousePosition: TPoint): Boolean;
var
  ACurrentCell: TdxTableCell;
begin
  ACurrentCell := GetCurrentCell(AMousePosition);
  Result := (ACurrentCell = nil) or (((StartCell <> nil) and (ACurrentCell.Table <> StartCell.Table)));
end;

procedure TdxContinueSelectionByStartTableRowsMouseHandlerState.SwitchState(const AMousePosition: TPoint);
var
  AByCharactersState: TdxContinueSelectionByCharactersMouseHandlerState;
begin
  AByCharactersState := TdxContinueSelectionByCharactersMouseHandlerState.Create(Controller, InitialBoxAccessor, InitialHitTestResult);
  Controller.SwitchStateCore(AByCharactersState, AMousePosition);
end;

{ TdxContinueSelectionByTableRowsAfterCharactersMouseHandlerState }

procedure TdxContinueSelectionByTableRowsAfterCharactersMouseHandlerState.SwitchState(const AMousePosition: TPoint);
var
  AByLinesState: TdxContinueSelectionByCharactersMouseHandlerState;
begin
  AByLinesState := TdxContinueSelectionByCharactersMouseHandlerState.Create(Controller, InitialBoxAccessor, InitialHitTestResult);
  Controller.SwitchStateCore(AByLinesState, AMousePosition);
end;

{ TdxContinueSelectionByTableRowsAfterRowMouseHandlerState }

procedure TdxContinueSelectionByTableRowsAfterRowMouseHandlerState.SwitchState(const AMousePosition: TPoint);
var
  AByLinesState: TdxContinueSelectionByLinesMouseHandlerState;
begin
  AByLinesState := TdxContinueSelectionByLinesMouseHandlerState.Create(Controller, InitialBoxAccessor, InitialHitTestResult);
  Controller.SwitchStateCore(AByLinesState, AMousePosition);
end;

{ TdxContinueSelectionByTableColumnsMouseHandlerState }

function TdxContinueSelectionByTableColumnsMouseHandlerState.CalculateCursor(const E: TdxMouseEventArgs): TCursor;
begin
  Result := TdxRichEditCursors.SelectTableColumn;
end;

function TdxContinueSelectionByTableColumnsMouseHandlerState.CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase;
begin
  Result := TdxExtendSelectionByTableColumnsCommand.Create(Control, StartColumnIndex, NestedLevel);
end;

procedure TdxContinueSelectionByTableColumnsMouseHandlerState.ContinueSelection(const E: TdxMouseEventArgs);
var
  AMousePosition: TPoint;
  ACurrentCell: TdxTableCell;
  AByCharacterState: TdxContinueSelectionByCharactersMouseHandlerState;
begin
  AMousePosition := E.MousePos;
  ACurrentCell := GetCurrentCell(AMousePosition);
  if ACurrentCell = nil then
  begin
    AByCharacterState := TdxContinueSelectionByCharactersMouseHandlerState.Create(Controller, InitialBoxAccessor, InitialHitTestResult);
    Controller.SwitchStateCore(AByCharacterState, AMousePosition);
  end
  else
    inherited ContinueSelection(E);
end;

{ TdxContinueSelectionByWordsAndParagraphsMouseHandlerStateBase }

procedure TdxContinueSelectionByWordsAndParagraphsMouseHandlerStateBase.ContinueSelection(const Args: TdxMouseEventArgs);
var
  ACell: TdxTableCell;
  APhysicalPoint: TPoint;
  ANewHitTestResult: TdxRichEditHitTestResult;
  ANestingLevel: Integer;
begin
  APhysicalPoint := Args.MousePos;
  ACell := GetCurrentCell(APhysicalPoint);
  if ACell <> nil then
  begin
    if (ACell <> nil) and (StartCell = nil) or (StartCell <> nil) and (ACell <> nil) and (StartCell.Table.NestedLevel <> ACell.Table.NestedLevel) then
    begin
      ANewHitTestResult := GetHitTestResult(APhysicalPoint);
      try
        ANestingLevel := ANewHitTestResult.TableRow.Row.Table.NestedLevel;
        Controller.SwitchStateCore(GetNewMouseHandlerState(Controller, InitialBoxAccessor, InitialHitTestResult, ANestingLevel), APhysicalPoint);
        Exit;
      finally
        ANewHitTestResult.Free;
      end;
    end;
    if (ACell <> nil) and (StartCell <> nil) and (ACell <> StartCell) then
    begin
      Controller.SwitchStateCore(TdxContinueSelectionByTableCellsMouseHandlerState.Create(Controller, InitialBoxAccessor, InitialHitTestResult), APhysicalPoint);
      Exit;
    end;
  end;
  inherited ContinueSelection(Args);
end;

{ TdxContinueSelectionByTableCellsMouseHandlerStateBase }

function TdxContinueSelectionByTableCellsMouseHandlerStateBase.CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase;
begin
  Result := TdxExtendSelectionByCellsCommand.Create(Control, StartCell);
end;

procedure TdxContinueSelectionByTableCellsMouseHandlerStateBase.ContinueSelection(const E: TdxMouseEventArgs);
var
  AMousePosition: TPoint;
  ANewState: TdxContinueSelectionByCharactersMouseHandlerState;
  AState: IdxRichEditMouseState;
begin
  AMousePosition := E.MousePos;
  if NeedSwitchState(AMousePosition) then
  begin
    ChangeSelection;
    ANewState := TdxContinueSelectionByCharactersMouseHandlerState.Create(Controller, InitialBoxAccessor, InitialHitTestResult);
    AState := Controller.State;
    try
      Controller.SwitchStateCore(ANewState, AMousePosition);
      ValidateSelection;
      ANewState.ContinueSelection(E);
    finally
      AState := nil;
    end;
  end
  else
    inherited ContinueSelection(E);
end;

function TdxContinueSelectionByTableCellsMouseHandlerStateBase.NeedSwitchState(const APhysicalPoint: TPoint): Boolean;
var
  ACurrentCell: TdxTableCell;
begin
  ACurrentCell := GetCurrentCell(APhysicalPoint);
  Result := (ACurrentCell = nil) or (StartCell.Table.NestedLevel > ACurrentCell.Table.NestedLevel);
end;

procedure TdxContinueSelectionByTableCellsMouseHandlerStateBase.ValidateSelection;
var
  ACommand: TdxExtendSelectionByRangesCommandBase;
begin
  ACommand := CreateExtendSelectionCommand;
  try
    ACommand.ValidateSelection;
  finally
    ACommand.Free;
  end;
end;

{ TdxContinueSelectionByStartTableCellsMouseHandlerState }

function TdxContinueSelectionByStartTableCellsMouseHandlerState.CalculateCursor(const E: TdxMouseEventArgs): TCursor;
begin
  Result := TdxRichEditCursors.SelectTableCell;
end;

procedure TdxContinueSelectionByStartTableCellsMouseHandlerState.ChangeSelection;
begin
end;

{ TdxContinueSelectionByTableCellsMouseHandlerState }

function TdxContinueSelectionByTableCellsMouseHandlerState.NeedSwitchState(const APhysicalPoint: TPoint): Boolean;
begin
  Result := (inherited NeedSwitchState(APhysicalPoint)) or (StartCell = GetCurrentCell(APhysicalPoint));
end;

procedure TdxContinueSelectionByTableCellsMouseHandlerState.ChangeSelection;
var
  ASelection: TdxSelection;
  AOldStartLogPosition: TdxDocumentLogPosition;
begin
  ASelection := DocumentModel.Selection;
  ASelection.ClearSelectionInTable;
  AOldStartLogPosition := ASelection.SelectedCells.OriginalStartLogPosition;
  ASelection.SetStartCell(AOldStartLogPosition);
  ASelection.Start := AOldStartLogPosition;
end;

{ TdxContinueSelectionByTableRowsAfterWordsMouseHandlerState }

function TdxContinueSelectionByTableRowsAfterWordsMouseHandlerState.CalculateCursor(const E: TdxMouseEventArgs): TCursor;
begin
  Result := TdxRichEditCursors.IBeam;
end;

procedure TdxContinueSelectionByTableRowsAfterWordsMouseHandlerState.SwitchState(const AMousePosition: TPoint);
var
  AByWordsState: TdxContinueSelectionByWordsMouseHandlerState;
begin
  AByWordsState := TdxContinueSelectionByWordsMouseHandlerState.Create(Controller, InitialBoxAccessor, InitialHitTestResult);
  Controller.SwitchStateCore(AByWordsState, AMousePosition);
end;

{ TdxHyperlinkMouseClickHandler }

constructor TdxHyperlinkMouseClickHandler.Create(const AControl: IdxRichEditControl);
begin
  inherited Create;
  FControl := AControl;
end;

procedure TdxHyperlinkMouseClickHandler.HandleMouseUp(
  const Args: TdxMouseEventArgs);
var
  AHitTestResult: TdxRichEditHitTestResult;
begin
  AHitTestResult := Control.InnerControl.ActiveView.CalculateHitTest(Args.MousePos, TdxDocumentLayoutDetailsLevel.Box);
  try
    if (AHitTestResult = nil) or not AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Box) then
      Exit;
    if Args.Buttons = [mbLeft] then
      TryProcessHyperlinkClick(AHitTestResult);
  finally
    AHitTestResult.Free;
  end;
end;

function TdxHyperlinkMouseClickHandler.TryProcessHyperlinkClick(
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  AField: TdxField;
begin
  AField := Control.InnerControl.ActiveView.GetHyperlinkField(AHitTestResult);

  if AField = nil then
  begin
    Result := False;
    Exit;
  end;
  if (AField = nil) or AField.IsCodeView then
    Result := False
  else
    Result := Control.InnerControl.OnHyperlinkClick(AField, True);
end;

{ TdxContinueSelectionByWordsMouseHandlerState }

function TdxContinueSelectionByWordsMouseHandlerState.CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase;
begin
  Result := TdxExtendSelectionByWordsCommand.Create(Control);
end;

function TdxContinueSelectionByWordsMouseHandlerState.GetNewMouseHandlerState(AController: TdxRichEditMouseController;
  AInitialBoxAccessor: TdxBoxAccessorKind; AInitialHitTestResult: TdxRichEditHitTestResult; ANestingLevel: Integer): TdxContinueSelectionByTableRowsMouseHandlerStateBase;
begin
  Result := TdxContinueSelectionByTableRowsAfterWordsMouseHandlerState.Create(Controller, InitialBoxAccessor,
    InitialHitTestResult, ANestingLevel);
end;

{ TdxContinueSelectionByParagraphsMouseHandlerState }

function TdxContinueSelectionByParagraphsMouseHandlerState.CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase;
begin
  Result := TdxExtendSelectionByParagraphsCommand.Create(Control);
end;

function TdxContinueSelectionByParagraphsMouseHandlerState.GetNewMouseHandlerState(AController: TdxRichEditMouseController;
  AInitialBoxAccessor: TdxBoxAccessorKind; AInitialHitTestResult: TdxRichEditHitTestResult; ANestingLevel: Integer): TdxContinueSelectionByTableRowsMouseHandlerStateBase;
begin
  Result := TdxContinueSelectionByTableRowsAfterParagraphsMouseHandlerState.Create(Controller, InitialBoxAccessor,
    InitialHitTestResult, ANestingLevel);
end;

{ TdxContinueSelectionByLinesMouseHandlerState }

function TdxContinueSelectionByLinesMouseHandlerState.CalculateCursor(
  const Args: TdxMouseEventArgs): TCursor;
begin
  Result := TdxRichEditCursors.SelectRow;
end;

procedure TdxContinueSelectionByLinesMouseHandlerState.ContinueSelection(
  const Args: TdxMouseEventArgs);
var
  ANestingLevel: Integer;
  APhysicalPoint: TPoint;
  ANewState: TdxRichEditMouseCustomState;
  ANewHitTestResult: TdxRichEditHitTestResult;
begin
  if StartCell <> nil then
    inherited ContinueSelection(Args);
  APhysicalPoint := Args.MousePos;
  ANewHitTestResult := GetHitTestResult(APhysicalPoint);
  try
    if ANewHitTestResult.TableRow <> nil then
    begin
      ANestingLevel := ANewHitTestResult.TableRow.Row.Table.NestedLevel;
      ANewState := TdxContinueSelectionByTableRowsAfterRowMouseHandlerState.Create(Controller,
        InitialBoxAccessor, InitialHitTestResult, ANestingLevel);
      Controller.SwitchStateCore(ANewState, APhysicalPoint);
    end
    else
      inherited ContinueSelection(Args);
  finally
    ANewHitTestResult.Free;
  end;
end;

function TdxContinueSelectionByLinesMouseHandlerState.CreateExtendSelectionCommand: TdxExtendSelectionByRangesCommandBase;
begin
  Result := TdxExtendSelectionByLinesCommand.Create(Control);
end;

{ TdxContinueSelectionByTableRowsAfterParagraphsMouseHandlerState }

procedure TdxContinueSelectionByTableRowsAfterParagraphsMouseHandlerState.SwitchState(const AMousePosition: TPoint);
var
  AByParagraphs: TdxContinueSelectionByParagraphsMouseHandlerState;
begin
  AByParagraphs := TdxContinueSelectionByParagraphsMouseHandlerState.Create(Controller, InitialBoxAccessor, InitialHitTestResult);
  Controller.SwitchStateCore(AByParagraphs, AMousePosition);
end;


end.

