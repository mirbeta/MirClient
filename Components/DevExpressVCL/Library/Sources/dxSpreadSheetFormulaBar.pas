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

unit dxSpreadSheetFormulaBar;

{$I cxVer.inc}
{$R dxSpreadSheetFormulaBar.res}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Types, Classes, Messages, Controls, StdCtrls, Contnrs, Variants,
  Generics.Collections, Generics.Defaults, Graphics, Forms,
  dxCore, cxControls, cxContainer, cxEdit, cxRichEdit, dxSpreadSheetCore, cxDropDownEdit, cxLookAndFeelPainters,
  cxGeometry, dxCoreClasses, dxGenerics, cxGraphics, cxLookAndFeels, cxButtonEdit, cxHeader, cxScrollBar, dxMessages,
  cxClasses, dxHashUtils, dxSpreadSheetInplaceEdit;

type
  TdxSpreadSheetFormulaBarInplaceEdit = class;
  TdxSpreadSheetFormulaBarOptionsBehavior = class;
  TdxSpreadSheetFormulaBarOptionsView = class;
  TdxSpreadSheetFormulaBarViewInfo = class;

  { TdxSpreadSheetAbstractFormulaBar }

  TdxSpreadSheetFormulaBarChange = (ssfbcSize, ssfbcLayout, ssfbcState);
  TdxSpreadSheetFormulaBarChanges = set of TdxSpreadSheetFormulaBarChange;

  TdxSpreadSheetAbstractFormulaBar = class(TcxControl,
    IdxSpreadSheetInplaceEditController)
  protected const
    DefaultSeparatorPosition = 110;
  strict private
    FButtonBox: TcxCustomButtonEdit;
    FFormulaEdit: TdxSpreadSheetFormulaBarInplaceEdit;
    FFocused: Boolean;
    FNameBox: TcxCustomComboBox;
    FOptionsBehavior: TdxSpreadSheetFormulaBarOptionsBehavior;
    FOptionsView: TdxSpreadSheetFormulaBarOptionsView;
    FTransparent: Boolean;
    FViewInfo: TdxSpreadSheetFormulaBarViewInfo;

    FCustomHeight: Integer;
    FSeparatorPosition: Integer;

    function GetExpanded: Boolean;
    procedure SetExpanded(AValue: Boolean);
    procedure SetOptionsBehavior(AValue: TdxSpreadSheetFormulaBarOptionsBehavior);
    procedure SetOptionsView(AValue: TdxSpreadSheetFormulaBarOptionsView);
    procedure SetSeparatorPosition(AValue: Integer);
    procedure SetTransparent(AValue: Boolean);
    //
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure DXMContainerSetFocus(var AMessage: TMessage); message DXM_CONTAINERSETFOCUS;
  protected
    function CreateButtonBox: TcxCustomButtonEdit; virtual;
    function CreateFormulaEdit: TdxSpreadSheetFormulaBarInplaceEdit; virtual;
    function CreateNameBox: TcxCustomComboBox; virtual;
    function CreateOptionsBehavior: TdxSpreadSheetFormulaBarOptionsBehavior; virtual;
    function CreateOptionsView: TdxSpreadSheetFormulaBarOptionsView; virtual;
    function CreateViewInfo: TdxSpreadSheetFormulaBarViewInfo; virtual;

    procedure BoundsChanged; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function CanExecuteInsertFunctionDialog: Boolean; virtual;
    procedure Changed(AChanges: TdxSpreadSheetFormulaBarChanges);
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CreateHandle; override;
    procedure CreateInnerControls; virtual;
    procedure DestroyHandle; override;
    procedure DoPaint; override;
    procedure EraseBackground(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure FocusChanged; override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    function HasBackground: Boolean; override;
    function IsEditing: Boolean; virtual;
    procedure PlaceInnerControls; virtual;
    procedure PopulateButtonBox(AButtons: TcxEditButtons); overload; virtual;
    procedure PopulateButtonBox; overload;
    procedure ProcessChanges(AChanges: TdxSpreadSheetFormulaBarChanges); virtual;
    procedure UpdateInnerControlsHints; virtual;
    procedure UpdateInnerControlsState; virtual;
    // Keyboard
    procedure KeyDown(var Key: Word; ShiftState: TShiftState); override;
    // Mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    // Drag-and-Drop
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure UpdateDragAndDropObjectClass(const P: TPoint); virtual;
    // IdxSpreadSheetInplaceEditController
    procedure IdxSpreadSheetInplaceEditController.KeyDown = ControllerKeyDown;
    procedure ControllerKeyDown(var Key: Word; ShiftState: TShiftState); virtual;
    function IsAutoCompleteAllowed: Boolean; virtual;
    function IsAutoCompleteSuggestionsHintsAllowed: Boolean; virtual;
    procedure Post(AIsArrayFormula: Boolean); virtual;
    // Handlers
    procedure HandlerAfterFocusChanged(Sender: TObject); virtual;
    procedure HandlerButtonClick(Sender: TObject; AButtonIndex: Integer); virtual;
    procedure HandlerFormulaEditChanged(Sender: TObject); virtual;
    procedure HandlerFormulaEditExit(Sender: TObject); virtual;
    procedure HandlerFormulaEditSelectionChanged(Sender: TObject); virtual;
    procedure HandlerFormulaEditKeyDown(Sender: TObject; var Key: Word; ShiftState: TShiftState); virtual;
    procedure HandlerFormulaEditKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure HandlerFormulaEditKeyUp(Sender: TObject; var Key: Word; ShiftState: TShiftState); virtual;
    procedure HandlerExpandButtonClick(Sender: TObject); virtual;
    procedure HandlerFocusChanged(Sender: TObject); virtual;
    procedure HandlerNameSelected(Sender: TObject); virtual;
    // Events
    procedure DoExecuteInsertFunctionDialog; virtual; abstract;
    procedure DoHideEdit(AAccept: Boolean; AIsArrayFormula: Boolean = False; ACanChangeFocus: Boolean = True); virtual;
    procedure DoSelectNamedObject(const AText: string; AObject: TObject); virtual; abstract;

    property ButtonBox: TcxCustomButtonEdit read FButtonBox;
    property FormulaEdit: TdxSpreadSheetFormulaBarInplaceEdit read FFormulaEdit;
    property NameBox: TcxCustomComboBox read FNameBox;
    property ViewInfo: TdxSpreadSheetFormulaBarViewInfo read FViewInfo;

    property CustomHeight: Integer read FCustomHeight write FCustomHeight;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property OptionsBehavior: TdxSpreadSheetFormulaBarOptionsBehavior read FOptionsBehavior write SetOptionsBehavior;
    property OptionsView: TdxSpreadSheetFormulaBarOptionsView read FOptionsView write SetOptionsView;
    property SeparatorPosition: Integer read FSeparatorPosition write SetSeparatorPosition default DefaultSeparatorPosition;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure CancelEditing;
    function Focused: Boolean; override;
    procedure FullRefresh;
    procedure TranslationChanged; override;
  end;

  { TdxSpreadSheetFormulaBarCustomOptions }

  TdxSpreadSheetFormulaBarCustomOptions = class(TPersistent)
  strict private
    FOwner: TdxSpreadSheetAbstractFormulaBar;
  protected
    procedure Changed; virtual;
    //
    property Owner: TdxSpreadSheetAbstractFormulaBar read FOwner;
  public
    constructor Create(AOwner: TdxSpreadSheetAbstractFormulaBar); virtual;
  end;

  { TdxSpreadSheetFormulaBarOptionsBehavior }

  TdxSpreadSheetFormulaBarOptionsBehavior = class(TdxSpreadSheetFormulaBarCustomOptions)
  strict private
    FFormulaAutoComplete: TdxDefaultBoolean;
    FFormulaAutoCompleteShowHint: TdxDefaultBoolean;
  public
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
  published
    property FormulaAutoComplete: TdxDefaultBoolean read FFormulaAutoComplete write FFormulaAutoComplete default bDefault;
    property FormulaAutoCompleteShowHint: TdxDefaultBoolean read FFormulaAutoCompleteShowHint write FFormulaAutoCompleteShowHint default bDefault;
  end;

  { TdxSpreadSheetFormulaBarOptionsView }

  TdxSpreadSheetFormulaBarOptionsView = class(TdxSpreadSheetFormulaBarCustomOptions)
  strict private
    FShowButtons: Boolean;
    FShowNameBox: Boolean;

    procedure SetShowButtons(AValue: Boolean);
    procedure SetShowNameBox(AValue: Boolean);
  public
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ShowButtons: Boolean read FShowButtons write SetShowButtons default True;
    property ShowNameBox: Boolean read FShowNameBox write SetShowNameBox default True;
  end;

  { TdxSpreadSheetFormulaBarInplaceEdit }

  TdxSpreadSheetFormulaBarInplaceEdit = class(TdxSpreadSheetCustomInplaceEdit)
  strict private
    FBar: TdxSpreadSheetAbstractFormulaBar;
    FLineHeight: Integer;

    procedure SetLineHeight(AValue: Integer);
  protected
    function AllowTouchScrollUIMode: Boolean; override;
    function CanFocusOnClick: Boolean; override;
    procedure DoExpandButtonClick; virtual;
    function GetController: IdxSpreadSheetInplaceEditController; override;
    function GetScrollBarClass(AKind: TScrollBarKind): TcxControlScrollBarClass; override;
  {$IFNDEF DXSPREADSHEETFORMULABAR_RTF}
    function IsFormattingSupported: Boolean; override;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;

    property LineHeight: Integer read FLineHeight write SetLineHeight;
  end;

  { TdxSpreadSheetFormulaBarInplaceEditScrollBar }

  TdxSpreadSheetFormulaBarInplaceEditScrollBar = class(TcxControlScrollBar)
  protected
    function GetHelperClass: TcxScrollBarHelperClass; override;
    procedure SetEnabled(Value: Boolean); override;
  end;

  { TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper }

  TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo = class;

  TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper = class(TcxScrollBarHelper)
  strict private
    function GetInplaceEdit: TdxSpreadSheetFormulaBarInplaceEdit;
  protected
    procedure DoExpandButtonClick; virtual;
    function GetControllerClass: TcxScrollBarControllerClass; override;
    function GetExpandButtonHeight: Integer; virtual;
    function GetPainterClass: TcxScrollBarPainterClass; override;
    function GetViewInfoClass: TcxScrollBarViewInfoClass; override;
  public
    property InplaceEdit: TdxSpreadSheetFormulaBarInplaceEdit read GetInplaceEdit;
  end;

  { TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo }

  TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo = class(TcxScrollBarViewInfo)
  strict private
    function GetOriginalBounds: TRect;
    function GetScrollBar: TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper;
  protected
    FExpandButtonRect: TRect;
    FExpandButtonState: TcxButtonState;

    function GetBounds: TRect; override;
  public
    constructor Create(AScrollBar: TcxScrollBarHelper); override;
    procedure Calculate; override;

    property Bounds: TRect read GetOriginalBounds;
    property ExpandButtonRect: TRect read FExpandButtonRect;
    property ExpandButtonState: TcxButtonState read FExpandButtonState;
    property ScrollBar: TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper read GetScrollBar;
    property ScrollBarBounds: TRect read GetBounds;
  end;

  { TdxSpreadSheetFormulaBarInplaceEditScrollBarController }

  TdxSpreadSheetFormulaBarInplaceEditScrollBarController = class(TcxScrollBarController)
  strict private
    function GetExpandButtonState: TcxButtonState;
    function GetScrollBar: TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper;
    function GetViewInfo: TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo;
    procedure SetExpandButtonState(AValue: TcxButtonState);
  protected
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseLeave(AControl: TControl); override;
    procedure DoMouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function IsMouseOverExpandButton(X, Y: Integer): Boolean;
  public
    property ExpandButtonState: TcxButtonState read GetExpandButtonState write SetExpandButtonState;
    property ScrollBar: TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper read GetScrollBar;
    property ViewInfo: TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo read GetViewInfo;
  end;

  { TdxSpreadSheetFormulaBarInplaceEditScrollBarPainter }

  TdxSpreadSheetFormulaBarInplaceEditScrollBarPainter = class(TcxScrollBarPainter)
  strict private
    function GetViewInfo: TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo;
  protected
    procedure DrawExpandButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AExpanded: Boolean); virtual;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
    //
    property ViewInfo: TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo read GetViewInfo;
  end;

  { TdxSpreadSheetFormulaBarSplitterDragHandler }

  TdxSpreadSheetFormulaBarSplitterDragHandler = class(TcxDragAndDropObject)
  strict private
    FCapturePoint: TPoint;
    FSeparatorPosition: Integer;
    FSeparatorPositionMax: Integer;

    function GetControl: TdxSpreadSheetAbstractFormulaBar;
  protected
    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetImmediateStart: Boolean; override;
  public
    property Control: TdxSpreadSheetAbstractFormulaBar read GetControl;
  end;

  { TdxSpreadSheetFormulaBarViewInfo }

  TdxSpreadSheetFormulaBarViewInfo = class
  strict private
    FBar: TdxSpreadSheetAbstractFormulaBar;

    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetScaleFactor: TdxScaleFactor;
  protected
    FButtonBoxRect: TRect;
    FFormulaEditRect: TRect;
    FLineHeight: Integer;
    FNameBoxRect: TRect;
    FSeparatorRect: TRect;

    function GetIndentBetweenElements: Integer; virtual;
    function MeasureLineHeight: Integer; virtual;
  public
    constructor Create(ABar: TdxSpreadSheetAbstractFormulaBar); virtual;
    procedure Calculate(ABounds: TRect); virtual;
    function CalculateMaxSeparatorPosition: Integer;
    procedure Draw(ACanvas: TcxCanvas); virtual;

    property ButtonBoxRect: TRect read FButtonBoxRect;
    property FormulaEditRect: TRect read FFormulaEditRect;
    property IndentBetweenElements: Integer read GetIndentBetweenElements;
    property LineHeight: Integer read FLineHeight;
    property NameBoxRect: TRect read FNameBoxRect;
    property SeparatorRect: TRect read FSeparatorRect;

    property Bar: TdxSpreadSheetAbstractFormulaBar read FBar;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  { TdxSpreadSheetCustomFormulaBar }

  TdxSpreadSheetCustomFormulaBar = class;

  TdxSpreadSheetEditingController = class(TdxSpreadSheetTableViewEditingController);
  TdxSpreadSheetFormulaBarExecuteInsertFunctionDialogEvent = procedure (Sender: TdxSpreadSheetCustomFormulaBar; var AHandled: Boolean) of object;

  TdxSpreadSheetCustomFormulaBar = class(TdxSpreadSheetAbstractFormulaBar,
    IdxSpreadSheet,
    IdxSpreadSheetListener,
    IdxSpreadSheetListener2,
    IdxSpreadSheetEditControllerListener,
    IdxSpreadSheetKeyboardListener,
    IdxSpreadSheetSelectionListener,
    IdxSpreadSheetTableViewSelectionModeListener)
  strict private
    FPreparingEdit: Boolean;
    FSpreadSheet: TdxCustomSpreadSheet;
    FValidationErrorOccurs: Boolean;

    FOnExecuteInsertFunctionDialog: TdxSpreadSheetFormulaBarExecuteInsertFunctionDialogEvent;

    function GetActiveSheet: TdxSpreadSheetTableView;
    function GetEditingController: TdxSpreadSheetEditingController;
    function GetFocusedCellPosition: TPoint;
    procedure SetSpreadSheet(AValue: TdxCustomSpreadSheet);
  protected
    procedure ControllerKeyDown(var Key: Word; ShiftState: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function IsAutoCompleteAllowed: Boolean; override;
    function IsAutoCompleteSuggestionsHintsAllowed: Boolean; override;
    function IsEditing: Boolean; override;
    procedure PopulateNameBox(AItems: TStrings); overload; virtual;
    procedure PopulateNameBox; overload; virtual;
    procedure SetFocusToSpreadSheet;
    procedure SynchronizeEditingValues(ATarget, ASource: TcxCustomEdit);
    procedure UpdateEditValue; virtual;
    procedure UpdateInnerControlsState; override;
    procedure UpdateNameBoxValue; virtual;
    procedure UpdateReferencesHighlighting;
    // Handlers
    procedure HandlerAfterFocusChanged(Sender: TObject); override;
    procedure HandlerFocusChanged(Sender: TObject); override;
    procedure HandlerFormulaEditChanged(Sender: TObject); override;
    procedure HandlerFormulaEditSelectionChanged(Sender: TObject); override;
    // Events
    function CanExecuteInsertFunctionDialog: Boolean; override;
    procedure DoCreateDefinedName(const AArea: TRect; const AName, ASheetName: string); virtual;
    procedure DoExecuteInsertFunctionDialog; override;
    procedure DoHideEdit(AAccept: Boolean; AIsArrayFormula: Boolean = False; ACanChangeFocus: Boolean = False); override;
    procedure DoSelectArea(const AView: TdxSpreadSheetTableView; const AArea: TRect); virtual;
    procedure DoSelectNamedObject(const AText: string; AObject: TObject); override;
    // IdxSpreadSheet
    function GetControl: TdxCustomSpreadSheet;
    // IdxSpreadSheetListener
    procedure DataChanged(Sender: TdxCustomSpreadSheet);
    // IdxSpreadSheetListener2
    procedure ActiveSheetChanged(Sender: TdxCustomSpreadSheet);
    procedure DefinedNamesChanged(Sender: TdxCustomSpreadSheet);
    // IdxSpreadSheetEditControllerListener
    procedure Edited(Sender: TdxSpreadSheetCustomView);
    procedure Editing(Sender: TdxSpreadSheetCustomView);
    procedure EditingValueChanged(Sender: TdxSpreadSheetCustomView);
    // IdxSpreadSheetKeyboardListener
    procedure IdxSpreadSheetKeyboardListener.KeyDown = SpreadSheetKeyDown;
    procedure SpreadSheetKeyDown(Sender: TdxCustomSpreadSheet; var Key: Word; Shift: TShiftState);
    // IdxSpreadSheetSelectionListener
    procedure SelectionChanged(Sender: TdxSpreadSheetCustomView);
    // IdxSpreadSheetTableViewSelectionModeListener
    procedure SelectionModeChanged(Sender: TdxSpreadSheetTableView; AMode: TdxSpreadSheetTableViewSelectionMode);
    //
    property ActiveSheet: TdxSpreadSheetTableView read GetActiveSheet;
    property EditingController: TdxSpreadSheetEditingController read GetEditingController;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure TranslationChanged; override;
    //
    property CustomHeight;
    property SeparatorPosition;
    property Expanded;
    property OptionsBehavior;
    property OptionsView;
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet write SetSpreadSheet;
    property Transparent default False;
    //
    property OnExecuteInsertFunctionDialog: TdxSpreadSheetFormulaBarExecuteInsertFunctionDialogEvent read FOnExecuteInsertFunctionDialog write FOnExecuteInsertFunctionDialog;
  end;

  { TdxSpreadSheetFormulaBar }

  TdxSpreadSheetFormulaBar = class(TdxSpreadSheetCustomFormulaBar)
  published
    property Align;
    property Anchors;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property LookAndFeel;
    property OptionsBehavior;
    property OptionsView;
    property ParentDoubleBuffered;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property SpreadSheet;
    property TabOrder;
    property Transparent;
    property Visible;
    //
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    //
    property OnExecuteInsertFunctionDialog;
  end;

implementation

uses
  SysUtils, Math, dxTypeHelpers, cxEditUtils, dxSpreadSheetStrs, dxDPIAwareUtils, dxSpreadSheetUtils,
  dxSpreadSheetTypes, dxSpreadSheetCoreHelpers, cxTextEdit, dxSpreadSheetInsertFunctionDialog, dxSpreadSheetFunctions,
  dxSpreadSheetCoreStrs, dxSpreadSheetFormulasHelpers, dxSpreadSheetCoreHistory, dxSpreadSheetCoreFormulasParser;

type
  TcxCustomEditAccess = class(TcxCustomEdit);
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

function FindSpreadSheetByOwner(AOwner: TComponent): TdxCustomSpreadSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    if AOwner.Components[I] is TdxCustomSpreadSheet then
      Exit(TdxCustomSpreadSheet(AOwner.Components[I]));
  end;
end;

{ TdxSpreadSheetAbstractFormulaBar }

constructor TdxSpreadSheetAbstractFormulaBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FocusOnClick := False;
  FSeparatorPosition := DefaultSeparatorPosition;
  FOptionsBehavior := CreateOptionsBehavior;
  FOptionsView := CreateOptionsView;
  FViewInfo := CreateViewInfo;
  CreateInnerControls;
  PopulateButtonBox;
  SetBounds(0, 0, 300, 10);
end;

destructor TdxSpreadSheetAbstractFormulaBar.Destroy;
begin
  FreeAndNil(FOptionsBehavior);
  FreeAndNil(FOptionsView);
  FreeAndNil(FButtonBox);
  FreeAndNil(FNameBox);
  FreeAndNil(FFormulaEdit);
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TdxSpreadSheetAbstractFormulaBar.AfterConstruction;
begin
  inherited;
  FullRefresh;
  AutoSize := True;
  UpdateInnerControlsHints;
end;

procedure TdxSpreadSheetAbstractFormulaBar.CancelEditing;
begin
  DoHideEdit(False);
end;

function TdxSpreadSheetAbstractFormulaBar.Focused: Boolean;
begin
  Result := inherited or NameBox.Focused or ButtonBox.Focused or FormulaEdit.Focused;
end;

procedure TdxSpreadSheetAbstractFormulaBar.FullRefresh;
begin
  Changed([Low(TdxSpreadSheetFormulaBarChange)..High(TdxSpreadSheetFormulaBarChange)]);
end;

procedure TdxSpreadSheetAbstractFormulaBar.TranslationChanged;
begin
  inherited;
  PopulateButtonBox;
  UpdateInnerControlsHints;
end;

procedure TdxSpreadSheetAbstractFormulaBar.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited;
  FullRefresh;
end;

function TdxSpreadSheetAbstractFormulaBar.CreateButtonBox: TcxCustomButtonEdit;
begin
  Result := TcxButtonEdit.Create(Self);
end;

function TdxSpreadSheetAbstractFormulaBar.CreateFormulaEdit: TdxSpreadSheetFormulaBarInplaceEdit;
begin
  Result := TdxSpreadSheetFormulaBarInplaceEdit.Create(Self);
end;

function TdxSpreadSheetAbstractFormulaBar.CreateNameBox: TcxCustomComboBox;
begin
  Result := TcxComboBox.Create(Self);
end;

function TdxSpreadSheetAbstractFormulaBar.CreateOptionsBehavior: TdxSpreadSheetFormulaBarOptionsBehavior;
begin
  Result := TdxSpreadSheetFormulaBarOptionsBehavior.Create(Self);
end;

function TdxSpreadSheetAbstractFormulaBar.CreateOptionsView: TdxSpreadSheetFormulaBarOptionsView;
begin
  Result := TdxSpreadSheetFormulaBarOptionsView.Create(Self);
end;

function TdxSpreadSheetAbstractFormulaBar.CreateViewInfo: TdxSpreadSheetFormulaBarViewInfo;
begin
  Result := TdxSpreadSheetFormulaBarViewInfo.Create(Self);
end;

procedure TdxSpreadSheetAbstractFormulaBar.BoundsChanged;
begin
  inherited BoundsChanged;
  if Expanded then
    FCustomHeight := Height;
  Changed([ssfbcLayout]);
end;

function TdxSpreadSheetAbstractFormulaBar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if ViewInfo.LineHeight > 0 then
    NewHeight := ViewInfo.LineHeight * Max(1, Round(NewHeight / ViewInfo.LineHeight));
end;

function TdxSpreadSheetAbstractFormulaBar.CanExecuteInsertFunctionDialog: Boolean;
begin
  Result := not IsEditing or dxSpreadSheetIsFormula(FormulaEdit.Text);
end;

procedure TdxSpreadSheetAbstractFormulaBar.Changed(AChanges: TdxSpreadSheetFormulaBarChanges);
begin
  if not IsDestroying then
    ProcessChanges(AChanges);
end;

procedure TdxSpreadSheetAbstractFormulaBar.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  FSeparatorPosition := MulDiv(FSeparatorPosition, M, D);
  inherited;
end;

procedure TdxSpreadSheetAbstractFormulaBar.CreateHandle;
begin
  inherited;
  Changed([ssfbcSize, ssfbcLayout]);
end;

procedure TdxSpreadSheetAbstractFormulaBar.CreateInnerControls;
begin
  FNameBox := CreateNameBox;
  FNameBox.Parent := Self;
  FNameBox.Style.TransparentBorder := False;
  FNameBox.Style.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  FNameBox.OnFocusChanged := HandlerFocusChanged;
  FNameBox.Properties.ImmediatePost := True;
  FNameBox.Properties.OnEditValueChanged := HandlerNameSelected;
  TcxCustomEditAccess(FNameBox).AutoHeight := False;

  FButtonBox := CreateButtonBox;
  FButtonBox.Parent := Self;
  FButtonBox.Properties.ReadOnly := True;
  FButtonBox.Properties.OnButtonClick := HandlerButtonClick;
  FButtonBox.Style.TransparentBorder := False;
  FButtonBox.Style.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  FButtonBox.TabStop := False;
  FButtonBox.OnFocusChanged := HandlerFocusChanged;
  TcxCustomEditAccess(FButtonBox).AutoHeight := False;
  TcxCustomEditAccess(FButtonBox).NeedFocusOnClick := False;

  FFormulaEdit := CreateFormulaEdit;
  FFormulaEdit.Parent := Self;
  FFormulaEdit.Style.TransparentBorder := False;
  FFormulaEdit.Style.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  FFormulaEdit.OnExit := HandlerFormulaEditExit;
  FFormulaEdit.OnKeyDown := HandlerFormulaEditKeyDown;
  FFormulaEdit.OnKeyUp := HandlerFormulaEditKeyUp;
  FFormulaEdit.OnKeyPress := HandlerFormulaEditKeyPress;
  FFormulaEdit.OnFocusChanged := HandlerFocusChanged;
  FFormulaEdit.Properties.OnChange := HandlerFormulaEditChanged;
  FFormulaEdit.Properties.OnSelectionChange := HandlerFormulaEditSelectionChanged;
end;

procedure TdxSpreadSheetAbstractFormulaBar.DestroyHandle;
begin
  CancelEditing;
  inherited;
end;

procedure TdxSpreadSheetAbstractFormulaBar.DoPaint;
begin
  inherited;
  ViewInfo.Draw(Canvas);
end;

procedure TdxSpreadSheetAbstractFormulaBar.FocusChanged;
begin
  inherited;
  Changed([ssfbcState]);
end;

procedure TdxSpreadSheetAbstractFormulaBar.Loaded;
begin
  inherited;
  FullRefresh;
end;

function TdxSpreadSheetAbstractFormulaBar.IsEditing: Boolean;
begin
  Result := FormulaEdit.Focused;
end;

procedure TdxSpreadSheetAbstractFormulaBar.PlaceInnerControls;
var
  AHandle: THandle;
  ARect: TRect;
begin
  if HandleAllocated then
  begin
    if IsDesigning then
    begin
      NameBox.BoundsRect := ViewInfo.NameBoxRect;
      ButtonBox.BoundsRect := ViewInfo.ButtonBoxRect;
      FormulaEdit.BoundsRect := ViewInfo.FormulaEditRect;
      FormulaEdit.LineHeight := ViewInfo.LineHeight;
    end
    else
    begin
      AHandle := BeginDeferWindowPos(3);
      try
        ARect := ViewInfo.NameBoxRect;
        DeferWindowPos(AHandle, NameBox.Handle, 0, ARect.Left, ARect.Top, ARect.Width, ARect.Height, SWP_NOZORDER);

        ARect := ViewInfo.ButtonBoxRect;
        DeferWindowPos(AHandle, ButtonBox.Handle, 0, ARect.Left, ARect.Top, ARect.Width, ARect.Height, SWP_NOZORDER);

        ARect := ViewInfo.FormulaEditRect;
        DeferWindowPos(AHandle, FormulaEdit.Handle, 0, ARect.Left, ARect.Top, ARect.Width, ARect.Height, SWP_NOZORDER);
        FormulaEdit.LineHeight := ViewInfo.LineHeight;
      finally
        EndDeferWindowPos(AHandle);
      end;
    end;
  end;
end;
procedure TdxSpreadSheetAbstractFormulaBar.PopulateButtonBox;
var
  AButtons: TcxEditButtons;
begin
  AButtons := FButtonBox.Properties.Buttons;
  AButtons.BeginUpdate;
  try
    AButtons.Clear;
    PopulateButtonBox(AButtons);
  finally
    AButtons.EndUpdate;
  end;
end;

procedure TdxSpreadSheetAbstractFormulaBar.PopulateButtonBox(AButtons: TcxEditButtons);

  procedure AddButton(const AGlyphResId: string; AHintResStr: Pointer);
  var
    AButton: TcxEditButton;
  begin
    AButton := AButtons.Add;
    AButton.Glyph.LoadFromResource(HInstance, AGlyphResId, 'SVG');
    AButton.Glyph.SourceHeight := 16;
    AButton.Glyph.SourceWidth := 16;
    AButton.Hint := cxGetResourceString(AHintResStr);
    AButton.Kind := bkGlyph;
  end;

begin
  AddButton('DXSPREADSHEET_FORMULABAR_CANCEL', @sdxFormulaBarCancelHint);
  AddButton('DXSPREADSHEET_FORMULABAR_ENTER', @sdxFormulaBarEnterHint);
  AddButton('DXSPREADSHEET_FORMULABAR_INSERT', @sdxFormulaBarInsertFunctionHint);
end;

procedure TdxSpreadSheetAbstractFormulaBar.ProcessChanges(AChanges: TdxSpreadSheetFormulaBarChanges);
begin
  if ssfbcLayout in AChanges then
  begin
    ViewInfo.Calculate(ClientBounds);
    PlaceInnerControls;
  end;
  if ssfbcState in AChanges then
    UpdateInnerControlsState;
  if ssfbcSize in AChanges then
    AdjustSize;
  Invalidate;
end;

procedure TdxSpreadSheetAbstractFormulaBar.UpdateInnerControlsHints;
begin
  FormulaEdit.Hint := cxGetResourceString(@sdxFormulaBarFormulaBarHint);
  NameBox.Hint := cxGetResourceString(@sdxFormulaBarNameBoxHint);
end;

procedure TdxSpreadSheetAbstractFormulaBar.UpdateInnerControlsState;
begin
  NameBox.Enabled := FormulaEdit.Enabled and not IsEditing;
  NameBox.Visible := OptionsView.ShowNameBox;
  ButtonBox.Enabled := FormulaEdit.Enabled;
  ButtonBox.Properties.Buttons[0].Enabled := ButtonBox.Enabled and IsEditing;
  ButtonBox.Properties.Buttons[1].Enabled := ButtonBox.Enabled and IsEditing;
  ButtonBox.Properties.Buttons[2].Enabled := ButtonBox.Enabled and CanExecuteInsertFunctionDialog;
  ButtonBox.Visible := OptionsView.ShowButtons;
end;

procedure TdxSpreadSheetAbstractFormulaBar.KeyDown(var Key: Word; ShiftState: TShiftState);
begin
  inherited;

  ShiftState := ShiftState * [ssCtrl, ssAlt, ssShift];
  case Key of
    Ord('U'):
      if ShiftState = [ssShift, ssCtrl] then
      begin
        Expanded := not Expanded;
        Key := 0;
      end;

    VK_F3:
      if ShiftState = [ssShift] then
      begin
        if CanExecuteInsertFunctionDialog then
          DoExecuteInsertFunctionDialog;
        Key := 0;
      end;
  end;
end;

procedure TdxSpreadSheetAbstractFormulaBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if [ssLeft, ssDouble] * Shift = [ssLeft, ssDouble] then
  begin
    if PtInRect(ViewInfo.SeparatorRect, Point(X, Y)) then
      SeparatorPosition := DefaultSeparatorPosition;
  end;
end;

procedure TdxSpreadSheetAbstractFormulaBar.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  CursorMap: array[Boolean] of TCursor = (crDefault, crHSplit);
begin
  inherited;
  if not (ssLeft in Shift) then
    Cursor := CursorMap[PtInRect(ViewInfo.SeparatorRect, Point(X, Y))];
end;

function TdxSpreadSheetAbstractFormulaBar.StartDragAndDrop(const P: TPoint): Boolean;
begin
  UpdateDragAndDropObjectClass(P);
  Result := DragAndDropObjectClass <> TcxDragAndDropObject;
end;

procedure TdxSpreadSheetAbstractFormulaBar.UpdateDragAndDropObjectClass(const P: TPoint);
begin
  if PtInRect(ViewInfo.SeparatorRect, P) then
    DragAndDropObjectClass := TdxSpreadSheetFormulaBarSplitterDragHandler
  else
    DragAndDropObjectClass := nil;
end;

procedure TdxSpreadSheetAbstractFormulaBar.ControllerKeyDown(var Key: Word; ShiftState: TShiftState);
begin
  // do nothing
end;

function TdxSpreadSheetAbstractFormulaBar.IsAutoCompleteAllowed: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(OptionsBehavior.FormulaAutoComplete, True);
end;

function TdxSpreadSheetAbstractFormulaBar.IsAutoCompleteSuggestionsHintsAllowed: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(OptionsBehavior.FormulaAutoCompleteShowHint, True);
end;

procedure TdxSpreadSheetAbstractFormulaBar.Post(AIsArrayFormula: Boolean);
begin
  DoHideEdit(True, AIsArrayFormula);
end;

function TdxSpreadSheetAbstractFormulaBar.GetExpanded: Boolean;
begin
  Result := Height > ViewInfo.LineHeight;
end;

procedure TdxSpreadSheetAbstractFormulaBar.SetExpanded(AValue: Boolean);
var
  APrevCustomHeight: Integer;
begin
  if Expanded <> AValue then
  begin
    APrevCustomHeight := FCustomHeight;
    try
      if AValue then
        Height := Max(FCustomHeight, 2 * ViewInfo.LineHeight)
      else
        Height := ViewInfo.LineHeight;
    finally
      FCustomHeight := APrevCustomHeight;
    end;
  end;
end;

procedure TdxSpreadSheetAbstractFormulaBar.SetOptionsBehavior(AValue: TdxSpreadSheetFormulaBarOptionsBehavior);
begin
  FOptionsBehavior.Assign(AValue);
end;

procedure TdxSpreadSheetAbstractFormulaBar.SetOptionsView(AValue: TdxSpreadSheetFormulaBarOptionsView);
begin
  FOptionsView.Assign(AValue);
end;

procedure TdxSpreadSheetAbstractFormulaBar.SetSeparatorPosition(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if FSeparatorPosition <> AValue then
  begin
    FSeparatorPosition := AValue;
    Changed([ssfbcLayout]);
  end;
end;

procedure TdxSpreadSheetAbstractFormulaBar.SetTransparent(AValue: Boolean);
begin
  if FTransparent <> AValue then
  begin
    FTransparent := AValue;
    Invalidate;
  end;
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerAfterFocusChanged(Sender: TObject);
begin
  Changed([ssfbcState]);
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  case AButtonIndex of
    0: CancelEditing;
    1: DoHideEdit(True);
    2: DoExecuteInsertFunctionDialog;
  end;
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerFormulaEditChanged(Sender: TObject);
begin
  Changed([ssfbcState]);
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerFormulaEditExit(Sender: TObject);
begin
  if TdxSpreadSheetTableViewEditingController.CanFinishEditingOnExit(FormulaEdit) then
    DoHideEdit(True, False, False);
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerFormulaEditKeyDown(Sender: TObject; var Key: Word; ShiftState: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    CancelEditing;
    Key := 0;
  end
  else
    KeyDown(Key, ShiftState);
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerFormulaEditKeyPress(Sender: TObject; var Key: Char);
begin
  KeyPress(Key);
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerFormulaEditKeyUp(Sender: TObject; var Key: Word; ShiftState: TShiftState);
begin
  KeyUp(Key, ShiftState);
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerFormulaEditSelectionChanged(Sender: TObject);
begin
  // do nothing
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerFocusChanged(Sender: TObject);
var
  AFocused: Boolean;
begin
  AFocused := Focused;
  if FFocused <> AFocused then
  begin
    FFocused := AFocused;
    if FFocused then
      DoEnter
    else
      DoExit;
  end;
  PostMessage(Handle, DXM_CONTAINERSETFOCUS, 0, LPARAM(Sender));
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerExpandButtonClick(Sender: TObject);
begin
  Expanded := not Expanded;
end;

procedure TdxSpreadSheetAbstractFormulaBar.HandlerNameSelected(Sender: TObject);
begin
  if NameBox.Focused then
    DoSelectNamedObject(NameBox.Text, NameBox.ItemObject);
end;

function TdxSpreadSheetAbstractFormulaBar.HasBackground: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetAbstractFormulaBar.DoHideEdit(AAccept, AIsArrayFormula, ACanChangeFocus: Boolean);
begin
  Changed([ssfbcState]);
end;

procedure TdxSpreadSheetAbstractFormulaBar.CMEnter(var Message: TCMEnter);
begin
  HandlerFocusChanged(Self);
end;

procedure TdxSpreadSheetAbstractFormulaBar.CMExit(var Message: TCMExit);
begin
  HandlerFocusChanged(Self);
end;

procedure TdxSpreadSheetAbstractFormulaBar.DXMContainerSetFocus(var AMessage: TMessage);
begin
  HandlerAfterFocusChanged(TObject(AMessage.LParam));
end;

procedure TdxSpreadSheetAbstractFormulaBar.EraseBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if Transparent then
    cxDrawTransparentControlBackground(Self, ACanvas, ARect)
  else
    LookAndFeelPainter.DrawPanelContent(ACanvas, ARect, False);
end;

{ TdxSpreadSheetFormulaBarCustomOptions }

constructor TdxSpreadSheetFormulaBarCustomOptions.Create(AOwner: TdxSpreadSheetAbstractFormulaBar);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxSpreadSheetFormulaBarCustomOptions.Changed;
begin
  if FOwner <> nil then
    FOwner.FullRefresh;
end;

{ TdxSpreadSheetFormulaBarOptionsBehavior }

procedure TdxSpreadSheetFormulaBarOptionsBehavior.AfterConstruction;
begin
  inherited;
  FFormulaAutoComplete := bDefault;
  FFormulaAutoCompleteShowHint := bDefault;
end;

procedure TdxSpreadSheetFormulaBarOptionsBehavior.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetFormulaBarOptionsBehavior then
  begin
    FormulaAutoComplete := TdxSpreadSheetFormulaBarOptionsBehavior(Source).FormulaAutoComplete;
    FormulaAutoCompleteShowHint := TdxSpreadSheetFormulaBarOptionsBehavior(Source).FormulaAutoCompleteShowHint;
  end;
end;

{ TdxSpreadSheetFormulaBarOptionsView }

procedure TdxSpreadSheetFormulaBarOptionsView.AfterConstruction;
begin
  inherited;
  FShowButtons := True;
  FShowNameBox := True;
end;

procedure TdxSpreadSheetFormulaBarOptionsView.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetFormulaBarOptionsView then
  begin
    ShowButtons := TdxSpreadSheetFormulaBarOptionsView(Source).ShowButtons;
    ShowNameBox := TdxSpreadSheetFormulaBarOptionsView(Source).ShowNameBox;
  end;
end;

procedure TdxSpreadSheetFormulaBarOptionsView.SetShowButtons(AValue: Boolean);
begin
  if FShowButtons <> AValue then
  begin
    FShowButtons := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetFormulaBarOptionsView.SetShowNameBox(AValue: Boolean);
begin
  if FShowNameBox <> AValue then
  begin
    FShowNameBox := AValue;
    Changed;
  end;
end;

{ TdxSpreadSheetFormulaBarInplaceEdit }

constructor TdxSpreadSheetFormulaBarInplaceEdit.Create(AOwner: TComponent);
begin
  inherited;
  FBar := AOwner as TdxSpreadSheetAbstractFormulaBar;
  Properties.RichEditClass := recRichEdit20;
  Properties.HideScrollBars := False;
  Properties.ScrollBars := ssVertical;
end;

function TdxSpreadSheetFormulaBarInplaceEdit.AllowTouchScrollUIMode: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetFormulaBarInplaceEdit.CanFocusOnClick: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetFormulaBarInplaceEdit.DoExpandButtonClick;
begin
  dxCallNotify(FBar.HandlerExpandButtonClick, Self);
end;

function TdxSpreadSheetFormulaBarInplaceEdit.GetController: IdxSpreadSheetInplaceEditController;
begin
  Result := FBar;
end;

function TdxSpreadSheetFormulaBarInplaceEdit.GetScrollBarClass(AKind: TScrollBarKind): TcxControlScrollBarClass;
begin
  if AKind = sbVertical then
    Result := TdxSpreadSheetFormulaBarInplaceEditScrollBar
  else
    Result := inherited;
end;

{$IFNDEF DXSPREADSHEETFORMULABAR_RTF}
function TdxSpreadSheetFormulaBarInplaceEdit.IsFormattingSupported: Boolean;
begin
  Result := False;
end;
{$ENDIF}

procedure TdxSpreadSheetFormulaBarInplaceEdit.SetLineHeight(AValue: Integer);
begin
  if FLineHeight <> AValue then
  begin
    FLineHeight := AValue;
    AdjustScrollBarPosition(VScrollBar);
  end;
end;

{ TdxSpreadSheetFormulaBarInplaceEditScrollBar }

function TdxSpreadSheetFormulaBarInplaceEditScrollBar.GetHelperClass: TcxScrollBarHelperClass;
begin
  Result := TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper;
end;

procedure TdxSpreadSheetFormulaBarInplaceEditScrollBar.SetEnabled(Value: Boolean);
begin
  Helper.Enabled := Value;
  Helper.Calculate;
  Helper.Repaint;
end;

{ TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper }

procedure TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper.DoExpandButtonClick;
begin
  InplaceEdit.DoExpandButtonClick;
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper.GetControllerClass: TcxScrollBarControllerClass;
begin
  Result := TdxSpreadSheetFormulaBarInplaceEditScrollBarController;
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper.GetInplaceEdit: TdxSpreadSheetFormulaBarInplaceEdit;
begin
  Result := ((Owner.GetControl as TdxSpreadSheetFormulaBarInplaceEditScrollBar).OwnerControl as TdxSpreadSheetFormulaBarInplaceEdit);
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper.GetExpandButtonHeight: Integer;
begin
  Result := InplaceEdit.LineHeight - 2 * LookAndFeelPainter.BorderSize;
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper.GetPainterClass: TcxScrollBarPainterClass;
begin
  Result := TdxSpreadSheetFormulaBarInplaceEditScrollBarPainter;
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper.GetViewInfoClass: TcxScrollBarViewInfoClass;
begin
  Result := TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo;
end;

{ TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo }

constructor TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo.Create(AScrollBar: TcxScrollBarHelper);
begin
  inherited;
  FExpandButtonState := cxbsNormal;
end;

procedure TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo.Calculate;
const
  StateMap: array[Boolean] of TcxButtonState = (cxbsDisabled, cxbsNormal);
begin
  FExpandButtonRect := cxRectSetHeight(Bounds, ScrollBar.GetExpandButtonHeight);
  FExpandButtonState := StateMap[ScrollBar.InplaceEdit.Enabled];
  inherited;
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo.GetBounds: TRect;
begin
  Result := inherited;
  Inc(Result.Top, ScrollBar.GetExpandButtonHeight);
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo.GetOriginalBounds: TRect;
begin
  Result := inherited GetBounds;
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo.GetScrollBar: TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper;
begin
  Result := inherited ScrollBar as TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper;
end;

{ TdxSpreadSheetFormulaBarInplaceEditScrollBarController }

procedure TdxSpreadSheetFormulaBarInplaceEditScrollBarController.DoMouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and IsMouseOverExpandButton(X, Y) then
  begin
    if ExpandButtonState <> cxbsDisabled then
      ExpandButtonState := cxbsPressed;
  end;
end;

procedure TdxSpreadSheetFormulaBarInplaceEditScrollBarController.DoMouseLeave(AControl: TControl);
begin
  if ExpandButtonState <> cxbsDisabled then
    ExpandButtonState := cxbsNormal;
  inherited;
end;

procedure TdxSpreadSheetFormulaBarInplaceEditScrollBarController.DoMouseMove(Shift: TShiftState; X, Y: Integer);
const
  StateMap: array[Boolean] of TcxButtonState = (cxbsNormal, cxbsHot);
begin
  if not (ExpandButtonState in [cxbsPressed, cxbsDisabled]) then
    ExpandButtonState := StateMap[IsMouseOverExpandButton(X, Y)];
  inherited;
end;

procedure TdxSpreadSheetFormulaBarInplaceEditScrollBarController.DoMouseUp(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ExpandButtonState = cxbsPressed) and IsMouseOverExpandButton(X, Y) then
    ScrollBar.DoExpandButtonClick;
  DoMouseMove(Shift, X, Y);
  inherited;
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarController.IsMouseOverExpandButton(X, Y: Integer): Boolean;
begin
  Result := PtInRect(ViewInfo.ExpandButtonRect, cxPoint(X, Y));
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarController.GetExpandButtonState: TcxButtonState;
begin
  Result := ViewInfo.FExpandButtonState;
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarController.GetScrollBar: TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper;
begin
  Result := TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper(inherited ScrollBar);
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarController.GetViewInfo: TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo;
begin
  Result := TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo(inherited ViewInfo);
end;

procedure TdxSpreadSheetFormulaBarInplaceEditScrollBarController.SetExpandButtonState(AValue: TcxButtonState);
begin
  if ExpandButtonState <> AValue then
  begin
    ViewInfo.FExpandButtonState := AValue;
    ScrollBar.Repaint;
  end;
end;

{ TdxSpreadSheetFormulaBarInplaceEditScrollBarPainter }

procedure TdxSpreadSheetFormulaBarInplaceEditScrollBarPainter.Paint(ACanvas: TcxCanvas);
begin
  inherited;
  DrawExpandButton(ACanvas, ViewInfo.ExpandButtonRect, ViewInfo.ExpandButtonState, not cxRectIsEmpty(ViewInfo.ScrollBarBounds));
end;

procedure TdxSpreadSheetFormulaBarInplaceEditScrollBarPainter.DrawExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AExpanded: Boolean);
begin
  LookAndFeelPainter.DrawSpreadSheetFormulaBarScaledExpandButton(ACanvas, R, AState, AExpanded, ScaleFactor);
end;

function TdxSpreadSheetFormulaBarInplaceEditScrollBarPainter.GetViewInfo: TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo;
begin
  Result := TdxSpreadSheetFormulaBarInplaceEditScrollBarViewInfo(
    TdxSpreadSheetFormulaBarInplaceEditScrollBarHelper(ScrollBar).ViewInfo);
end;

{ TdxSpreadSheetFormulaBarSplitterDragHandler }

procedure TdxSpreadSheetFormulaBarSplitterDragHandler.BeginDragAndDrop;
begin
  inherited;
  FCapturePoint := GetClientCursorPos;
  FSeparatorPosition := Control.SeparatorPosition;
  FSeparatorPositionMax := Control.ViewInfo.CalculateMaxSeparatorPosition;
end;

procedure TdxSpreadSheetFormulaBarSplitterDragHandler.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  Control.SeparatorPosition := Max(0, Min(FSeparatorPosition + P.X - FCapturePoint.X, FSeparatorPositionMax));
  Control.Update;
end;

function TdxSpreadSheetFormulaBarSplitterDragHandler.GetControl: TdxSpreadSheetAbstractFormulaBar;
begin
  Result := inherited Control as TdxSpreadSheetAbstractFormulaBar;
end;

function TdxSpreadSheetFormulaBarSplitterDragHandler.GetImmediateStart: Boolean;
begin
  Result := True;
end;

{ TdxSpreadSheetFormulaBarViewInfo }

constructor TdxSpreadSheetFormulaBarViewInfo.Create(ABar: TdxSpreadSheetAbstractFormulaBar);
begin
  inherited Create;
  FBar := ABar;
end;

procedure TdxSpreadSheetFormulaBarViewInfo.Calculate(ABounds: TRect);
var
  AButtonBoxWidth: Integer;
  AButtons: TcxEditButtons;
  I: Integer;
begin
  FLineHeight := MeasureLineHeight;

  if Bar.OptionsView.ShowNameBox then
  begin
    FNameBoxRect := cxRectSetSize(ABounds, Bar.SeparatorPosition, LineHeight);
    ABounds.Left := NameBoxRect.Right + IndentBetweenElements;

    FSeparatorRect := cxRectSetWidth(ABounds, Painter.SpreadSheetFormulaBarGetScaledSeparatorSize(ScaleFactor));
    FSeparatorRect := cxRectSetHeight(SeparatorRect, LineHeight);
    ABounds.Left := SeparatorRect.Right + IndentBetweenElements;
    FSeparatorRect := cxRectInflate(SeparatorRect, IndentBetweenElements, 0); // TODO
  end
  else
  begin
    FNameBoxRect := cxRectSetSize(ABounds, 0, LineHeight);
    FSeparatorRect := cxRectSetWidth(ABounds, 0);
  end;

  if Bar.OptionsView.ShowButtons then
  begin
    AButtons := Bar.ButtonBox.Properties.Buttons;
    AButtons.BeginUpdate;
    try
      for I := 0 to AButtons.Count - 1 do
        AButtons[I].Width := LineHeight;
    finally
      AButtons.EndUpdate;
    end;
    AButtonBoxWidth := 2 * Painter.BorderSize + AButtons.Count * LineHeight;
    FButtonBoxRect := cxRectSetSize(ABounds, AButtonBoxWidth, LineHeight);
    ABounds.Left := ButtonBoxRect.Right + IndentBetweenElements;
  end;

  FFormulaEditRect := ABounds;
end;

function TdxSpreadSheetFormulaBarViewInfo.CalculateMaxSeparatorPosition: Integer;
begin
  Result := Bar.ClientWidth - cxRectWidth(ButtonBoxRect) - cxRectWidth(SeparatorRect);
end;

procedure TdxSpreadSheetFormulaBarViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Painter.DrawSpreadSheetFormulaBarScaledSeparator(ACanvas, SeparatorRect, ScaleFactor);
end;

function TdxSpreadSheetFormulaBarViewInfo.MeasureLineHeight: Integer;

  procedure AdjustLineHeight(AEdit: TcxCustomEditAccess; var AHeight: Integer);
  var
    ASizeProperties: TcxEditSizeProperties;
    AViewData: TcxCustomEditViewData;
  begin
    AEdit.PopulateSizeProperties(ASizeProperties);
    AViewData := TcxCustomEditViewData(AEdit.CreateViewData);
    try
      AHeight := Max(AHeight, AViewData.GetEditSize(cxScreenCanvas, AEdit.EditValue, ASizeProperties, AEdit.ViewInfo).cy);
    finally
      AViewData.Free;
    end;
  end;

begin
  Result := 0;
  AdjustLineHeight(TcxCustomEditAccess(Bar.NameBox), Result);
  AdjustLineHeight(TcxCustomEditAccess(Bar.ButtonBox), Result);
  cxScreenCanvas.Dormant;
end;

function TdxSpreadSheetFormulaBarViewInfo.GetIndentBetweenElements: Integer;
begin
  Result := ScaleFactor.Apply(3 * cxTextOffset);
end;

function TdxSpreadSheetFormulaBarViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Bar.LookAndFeelPainter;
end;

function TdxSpreadSheetFormulaBarViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Bar.ScaleFactor;
end;

{ TdxSpreadSheetCustomFormulaBar }

procedure TdxSpreadSheetCustomFormulaBar.AfterConstruction;
begin
  inherited;
  if IsDesigning then
    SpreadSheet := FindSpreadSheetByOwner(Owner);
end;

procedure TdxSpreadSheetCustomFormulaBar.BeforeDestruction;
begin
  inherited BeforeDestruction;
  SpreadSheet := nil;
end;

procedure TdxSpreadSheetCustomFormulaBar.TranslationChanged;
begin
  inherited;
  UpdateNameBoxValue;
end;

procedure TdxSpreadSheetCustomFormulaBar.HandlerAfterFocusChanged(Sender: TObject);
begin
  if (Sender = NameBox) and NameBox.Focused then
    NameBox.SelectAll;
  inherited;
end;

procedure TdxSpreadSheetCustomFormulaBar.HandlerFormulaEditChanged(Sender: TObject);
begin
  inherited;
  if FormulaEdit.Focused and (EditingController <> nil) then
  begin
    SynchronizeEditingValues(EditingController.Edit, FormulaEdit);
    if not FormulaEdit.IsInImeComposition then
    begin
      EditingController.UpdateReferencesHighlighting;
      UpdateReferencesHighlighting;
    end;
  end;
end;

procedure TdxSpreadSheetCustomFormulaBar.HandlerFormulaEditSelectionChanged(Sender: TObject);
begin
  inherited;

  if FormulaEdit.Focused and (EditingController <> nil) then
  begin
    if (EditingController.Edit is TcxCustomTextEdit) and EditingController.Edit.HandleAllocated then
      TcxCustomTextEdit(EditingController.Edit).SelStart := FormulaEdit.SelStart;
  end;
end;

procedure TdxSpreadSheetCustomFormulaBar.HandlerFocusChanged(Sender: TObject);
begin
  inherited;

  if Sender = FormulaEdit then
  begin
    if FormulaEdit.Focused then
    begin
      if not EditingController.IsEditing then
      begin
        FPreparingEdit := True;
        try
          if not FValidationErrorOccurs then
          try
            EditingController.ShowEdit;
          except
            SetFocusToSpreadSheet;
            FValidationErrorOccurs := True;
            raise;
          end
          else
            SetFocusToSpreadSheet;
        finally
          FPreparingEdit := False;
        end;
      end;
    end
    else
      FValidationErrorOccurs := False;
  end;
end;

function TdxSpreadSheetCustomFormulaBar.CanExecuteInsertFunctionDialog: Boolean;
begin
  Result := (SpreadSheet <> nil) and inherited CanExecuteInsertFunctionDialog;
end;

procedure TdxSpreadSheetCustomFormulaBar.DoCreateDefinedName(const AArea: TRect; const AName, ASheetName: string);
begin
  SpreadSheet.History.AddCommand(TdxSpreadSheetHistoryCreateDefinedNameCommand.Create(
    SpreadSheet.DefinedNames.Add(AName, dxReferenceToString(AArea, False, [croSheetName], ASheetName))));
end;

procedure TdxSpreadSheetCustomFormulaBar.DoExecuteInsertFunctionDialog;
var
  AHandled: Boolean;
  AInfo: TdxSpreadSheetFunctionInfo;
begin
  AHandled := False;
  if Assigned(OnExecuteInsertFunctionDialog) then
    OnExecuteInsertFunctionDialog(Self, AHandled);

  if not AHandled then
  begin
    FormulaEdit.SetFocus;
    if ShowInsertFunctionDialog(SpreadSheet, AInfo) then
      TdxModifyFormulasHelper.Insert(AInfo.Name, FormulaEdit);
  end;
end;

procedure TdxSpreadSheetCustomFormulaBar.DoHideEdit(AAccept, AIsArrayFormula, ACanChangeFocus: Boolean);
begin
  if (EditingController <> nil) and EditingController.IsEditing then
  begin
    EditingController.HideEdit(AAccept);
    UpdateEditValue;
    inherited;
    if ACanChangeFocus then
      SetFocusToSpreadSheet;
  end;
end;

procedure TdxSpreadSheetCustomFormulaBar.DoSelectArea(const AView: TdxSpreadSheetTableView; const AArea: TRect);
begin
  if (AView <> nil) and dxSpreadSheetIsValidArea(AArea) then
  begin
    AView.Active := True;
    AView.Selection.Add(AArea);
    AView.MakeVisible(AArea);
  end;
end;

procedure TdxSpreadSheetCustomFormulaBar.DoSelectNamedObject(const AText: string; AObject: TObject);
var
  AArea: TRect;
begin
  if AObject is TdxSpreadSheetDefinedName then
  try
    TdxSpreadSheetDefinedName(AObject).EnumReferences(
      procedure (const Area: TRect; View: TObject)
      begin
        DoSelectArea(View as TdxSpreadSheetTableView, Area);
        Abort;
      end);
  except
    // do nothing
  end
  else
    if dxTryStringToReferenceArea(AText, AArea) then
      DoSelectArea(ActiveSheet, AArea)
    else
      DoCreateDefinedName(ActiveSheet.Selection.Area, AText, ActiveSheet.Caption);

  SetFocusToSpreadSheet;
  UpdateNameBoxValue;
end;

procedure TdxSpreadSheetCustomFormulaBar.ControllerKeyDown(var Key: Word; ShiftState: TShiftState);
var
  AController: IdxSpreadSheetInplaceEditController;
begin
  if Supports(EditingController, IdxSpreadSheetInplaceEditController, AController) then
    AController.KeyDown(Key, ShiftState);
end;

procedure TdxSpreadSheetCustomFormulaBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = SpreadSheet) then
    SpreadSheet := nil;
end;

function TdxSpreadSheetCustomFormulaBar.IsAutoCompleteAllowed: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(OptionsBehavior.FormulaAutoComplete,
    (SpreadSheet <> nil) and SpreadSheet.OptionsBehavior.FormulaAutoComplete);
end;

function TdxSpreadSheetCustomFormulaBar.IsAutoCompleteSuggestionsHintsAllowed: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(OptionsBehavior.FormulaAutoCompleteShowHint,
    (SpreadSheet <> nil) and SpreadSheet.OptionsBehavior.FormulaAutoCompleteShowHint);
end;

function TdxSpreadSheetCustomFormulaBar.IsEditing: Boolean;
begin
  Result := inherited or (EditingController <> nil) and EditingController.IsEditing;
end;

procedure TdxSpreadSheetCustomFormulaBar.PopulateNameBox;
begin
  PopulateNameBox(NameBox.Properties.Items);
end;

procedure TdxSpreadSheetCustomFormulaBar.PopulateNameBox(AItems: TStrings);
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    TdxSpreadSheetDefinedNameHelper.Enum(ActiveSheet,
      procedure (ADefinedName: TdxSpreadSheetDefinedName)
      begin
        AItems.AddObject(ADefinedName.Caption, ADefinedName);
      end);
  finally
    AItems.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCustomFormulaBar.SetFocusToSpreadSheet;
begin
  if (SpreadSheet <> nil) and SpreadSheet.CanFocusEx then
    SpreadSheet.SetFocus;
end;

procedure TdxSpreadSheetCustomFormulaBar.SynchronizeEditingValues(ATarget, ASource: TcxCustomEdit);

  function IsFormattingSupported(ATarget: TcxCustomEdit): Boolean;
  begin
  {$IFDEF DXSPREADSHEETFORMULABAR_RTF}
    Result := dxSpreadSheetTextService.IsRTFSupported and
      (ATarget is TcxCustomRichEdit) and not TcxCustomRichEdit(ATarget).Properties.PlainText;
  {$ELSE}
    Result := False;
  {$ENDIF}
  end;

  function IsFormattedValue(const AValue: TcxEditValue; const APlainValue: string): Boolean;
  begin
    Result := not dxSpreadSheetIsFormula(APlainValue) and
      dxSpreadSheetTextService.IsFormattedEditValue(VarToStr(AValue));
  end;

var
  ASourceValue: string;
begin
  if (ATarget <> nil) and (ASource <> nil) then
  begin
    ASourceValue := TdxSpreadSheetTableViewEditingController.GetEditingText(ASource);
    if IsFormattingSupported(ATarget) and IsFormattedValue(ASource.EditValue, ASourceValue) then
      ASourceValue := VarToStr(ASource.EditValue);

    ATarget.LockChangeEvents(True, False);
    try
      ATarget.EditValue := ASourceValue;
      if ATarget.HandleAllocated then
      begin
        if ATarget is TcxCustomTextEdit then
          TcxCustomTextEdit(ATarget).SelStart := Length(ASourceValue);
      end;
    finally
      ATarget.LockChangeEvents(False, False);
    end;
  end;
end;

procedure TdxSpreadSheetCustomFormulaBar.UpdateEditValue;
var
  ACellPosition: TPoint;
begin
  FValidationErrorOccurs := False;
  if EditingController <> nil then
  begin
    ACellPosition := GetFocusedCellPosition;
    FormulaEdit.EditValue := EditingController.GetValueFromCell(
      ActiveSheet.Cells[ACellPosition.Y, ACellPosition.X], FormulaEdit.IsFormattingSupported);
  end
  else
    FormulaEdit.EditValue := Null;
end;

procedure TdxSpreadSheetCustomFormulaBar.UpdateInnerControlsState;
begin
  FormulaEdit.Enabled := ActiveSheet <> nil;
  inherited UpdateInnerControlsState;
end;

procedure TdxSpreadSheetCustomFormulaBar.UpdateNameBoxValue;
var
  AArea: TRect;
  ACellPosition: TPoint;
  ADefinedName: TdxSpreadSheetDefinedName;
begin
  if ActiveSheet <> nil then
  begin
    AArea := ActiveSheet.Selection.Area;
    if TdxSpreadSheetDefinedNameHelper.FindByArea(SpreadSheet.DefinedNames, ActiveSheet, AArea, ADefinedName) then
      NameBox.ItemIndex := NameBox.Properties.Items.IndexOfObject(ADefinedName)
    else
      if (ActiveSheet.Controller.SelectionMode <> smNone) and not dxSpreadSheetIsSingleCellArea(AArea) then
      begin
        NameBox.Text := Format(cxGetResourceString(@sdxFormulaBarSelectionInfo),
          [dxSpreadSheetAreaHeight(AArea), dxSpreadSheetAreaWidth(AArea)]);
      end
      else
      begin
        ACellPosition := GetFocusedCellPosition;
        NameBox.Text := dxReferenceToString(ACellPosition.Y, ACellPosition.X);
      end;
  end;
end;

procedure TdxSpreadSheetCustomFormulaBar.UpdateReferencesHighlighting;
var
  AEditingController: TdxSpreadSheetEditingController;
begin
  AEditingController := EditingController;
  if (AEditingController <> nil) and AEditingController.IsEditing then
  begin
    AEditingController.ReferenceHighlighter.HighlightReferences(FormulaEdit);
    AEditingController.SpreadSheet.Invalidate;
  end;
end;

function TdxSpreadSheetCustomFormulaBar.GetControl: TdxCustomSpreadSheet;
begin
  Result := SpreadSheet;
end;

procedure TdxSpreadSheetCustomFormulaBar.DataChanged(Sender: TdxCustomSpreadSheet);
begin
  UpdateEditValue;
end;

procedure TdxSpreadSheetCustomFormulaBar.ActiveSheetChanged(Sender: TdxCustomSpreadSheet);
begin
  DefinedNamesChanged(Sender);
  SelectionChanged(ActiveSheet);
  Changed([ssfbcState]);
end;

procedure TdxSpreadSheetCustomFormulaBar.DefinedNamesChanged(Sender: TdxCustomSpreadSheet);
begin
  PopulateNameBox;
end;

procedure TdxSpreadSheetCustomFormulaBar.Edited(Sender: TdxSpreadSheetCustomView);
begin
  Changed([ssfbcState]);
  UpdateEditValue;
end;

procedure TdxSpreadSheetCustomFormulaBar.Editing(Sender: TdxSpreadSheetCustomView);
begin
  Changed([ssfbcState]);
  UpdateReferencesHighlighting;
end;

procedure TdxSpreadSheetCustomFormulaBar.EditingValueChanged(Sender: TdxSpreadSheetCustomView);
begin
  if (Sender = ActiveSheet) and not FPreparingEdit then
  begin
    SynchronizeEditingValues(FormulaEdit, EditingController.Edit);
    UpdateReferencesHighlighting;
  end;
end;

procedure TdxSpreadSheetCustomFormulaBar.SpreadSheetKeyDown(
  Sender: TdxCustomSpreadSheet; var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

procedure TdxSpreadSheetCustomFormulaBar.SelectionChanged(Sender: TdxSpreadSheetCustomView);
begin
  if not NameBox.Focused then
    UpdateNameBoxValue;
  UpdateEditValue;
end;

procedure TdxSpreadSheetCustomFormulaBar.SelectionModeChanged(
  Sender: TdxSpreadSheetTableView; AMode: TdxSpreadSheetTableViewSelectionMode);
begin
  UpdateNameBoxValue;
end;

function TdxSpreadSheetCustomFormulaBar.GetActiveSheet: TdxSpreadSheetTableView;
begin
  if (SpreadSheet <> nil) and not SpreadSheet.IsDestroying then
    Result := SpreadSheet.ActiveSheetAsTable
  else
    Result := nil;
end;

function TdxSpreadSheetCustomFormulaBar.GetFocusedCellPosition: TPoint;
var
  ASheet: TdxSpreadSheetTableView;
begin
  ASheet := ActiveSheet;
  if ASheet <> nil then
    Result := ASheet.MergedCells.ExpandArea(ASheet.Selection.FocusedColumn, ASheet.Selection.FocusedRow).TopLeft
  else
    Result := cxNullPoint;
end;

function TdxSpreadSheetCustomFormulaBar.GetEditingController: TdxSpreadSheetEditingController;
var
  AView: TdxSpreadSheetTableView;
begin
  AView := ActiveSheet;
  if AView <> nil then
    Result := TdxSpreadSheetEditingController(TdxSpreadSheetTableViewAccess(AView).EditingController)
  else
    Result := nil;
end;

procedure TdxSpreadSheetCustomFormulaBar.SetSpreadSheet(AValue: TdxCustomSpreadSheet);
begin
  if SpreadSheet <> AValue then
  begin
    CancelEditing;
    if SpreadSheet <> nil then
    begin
      TdxSpreadSheetAccess(FSpreadSheet).Listeners.Remove(Self);
      FSpreadSheet.RemoveFreeNotification(Self);
      FSpreadSheet := nil;
    end;
    if AValue <> nil then
    begin
      FSpreadSheet := AValue;
      FSpreadSheet.FreeNotification(Self);
      TdxSpreadSheetAccess(FSpreadSheet).Listeners.Add(Self);
    end;
    ActiveSheetChanged(SpreadSheet);
  end;
end;

end.
