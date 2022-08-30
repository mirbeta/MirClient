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

unit dxSpreadSheetCore;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, Classes, Types, SysUtils, Graphics, Controls, Variants, Forms, Math, Clipbrd, StdCtrls,
  Generics.Defaults, Generics.Collections,
  // CX
  cxClasses, dxCore, cxGraphics, dxCoreClasses, cxFormats, cxControls, cxGeometry, cxVariants, cxLookAndFeels,
  cxLookAndFeelPainters, dxCoreGraphics, cxInplaceContainer, dxGDIPlusClasses, cxNavigator, cxLibraryConsts,
  dxHashUtils, cxDateUtils, cxStyles, cxEdit, cxMemo, cxTextEdit, cxRichEdit, cxRichEditUtils, dxCustomHint,
  cxDrawTextUtils, dxMessages, dxProtectionUtils, cxDropDownEdit, cxContainer,
  // SpreadSheetCore
  dxSpreadSheetConditionalFormatting,
  dxSpreadSheetStyles,
  // SpreadSheet
  dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetUtils, dxSpreadSheetStrs, dxSpreadSheetGraphics,
  dxSpreadSheetNumberFormat, dxSpreadSheetPrinting, dxSpreadSheetHyperlinks, dxSpreadSheetProtection,
  dxSpreadSheetCoreStyles, dxSpreadSheetCoreFormulas, dxSpreadSheetInplaceEdit;

const
  dxSpreadSheetMovingContainerAlpha: Integer = MaxByte;
  dxSpreadSheetMovingClonedContainerAlpha: Integer = 150;
  dxSpreadSheetResizingContainerAlpha: Integer = 150;
  dxSpreadSheetRotationContainerAlpha: Integer = 150;

  dxSpreadSheetContainerRotateMarkerColor1: TdxAlphaColor = $FFE1F8CE;
  dxSpreadSheetContainerRotateMarkerColor2: TdxAlphaColor = $FF88E43A;
  dxSpreadSheetContainerRotateMarkerSize: Integer = 11;
  dxSpreadSheetContainerSelectionFrameColor: TdxAlphaColor = $FF808080;
  dxSpreadSheetContainerSizingMarkerColor1: TdxAlphaColor = $FFFFFFFF;
  dxSpreadSheetContainerSizingMarkerColor2: TdxAlphaColor = $FFCAEAED;
  dxSpreadSheetContainerSizingMarkerSize: Integer = 9;

  dxSpreadSheetContainerVersion = 1;

  // table view HitTests bits
  hcBackground          = 1;
  hcColumnHeader        = 2;
  hcRowHeader           = 4;
  hcResizeArea          = 8;
  hcCell                = 16;
  hcFrozenPaneSeparator = 32;
  hcContainer           = 64;
  hcContainerSelection  = 128;
  hcExpandButton        = 256;
  hcGroup               = 512;
  hcGroupingArea        = 1024;
  hcHyperlink           = 2048;
  hcSelectionFrame      = 4096;

  // PageControl HitTest
  hcButton      = 2;
  hcSplitter    = 4;
  hcPageTab     = 8;

  // styles
  s_Background = 0;
  s_Content    = 1;
  s_Header     = 2;
  s_PageControl= 3;
  s_Selection  = 4;

type
  EdxSpreadSheetCannotChangePartOfArrayError = class(EdxSpreadSheetError);
  EdxSpreadSheetFormatError = class(EdxSpreadSheetError);
  EdxSpreadSheetProtectionError = class(EdxSpreadSheetError);
  EdxSpreadSheetReaderError = class(EdxSpreadSheetFormatError);

  TdxCustomSpreadSheet = class;
  TdxSpreadSheetOptionsBehavior = class;
  TdxSpreadSheetOptionsView = class;

  TdxSpreadSheetHistory = class;
  TdxSpreadSheetHistoryAction = class;
  TdxSpreadSheetHistoryActionClass = class of TdxSpreadSheetHistoryAction;

  TdxSpreadSheetStyles = class;

  TdxSpreadSheetObjectListItem = class;
  TdxSpreadSheetObjectList = class;

  TdxSpreadSheetCell = class;

  TdxSpreadSheetCustomHitTest = class;

  TdxSpreadSheetFormula = class;
  TdxSpreadSheetFormulaClass = class of TdxSpreadSheetFormula;

  TdxSpreadSheetFormulaAsTextInfoList = class;

  TdxSpreadSheetFormulaController = class;

  TdxSpreadSheetContainer = class;
  TdxSpreadSheetContainerCalculator = class;
  TdxSpreadSheetContainerSelectionCellViewInfo = class;
  TdxSpreadSheetContainerViewInfo = class;

  TdxSpreadSheetCustomController = class;

  TdxSpreadSheetCustomView = class;
  TdxSpreadSheetCustomViewClass = class of TdxSpreadSheetCustomView;

  TdxSpreadSheetCustomViewViewInfo = class;

  TdxSpreadSheetCellViewInfo = class;
  TdxSpreadSheetCellViewInfoList = class;
  TdxSpreadSheetCustomCellViewInfo = class;
  TdxSpreadSheetViewInfo = class;

  TdxSpreadSheetCustomFiler = class;
  TdxSpreadSheetCustomFilerSubTask = class;
  TdxSpreadSheetCustomReferenceHighlighter = class;

  TdxSpreadSheetMergedCellList = class;

  TdxSpreadSheetTableView = class;
  TdxSpreadSheetTableViewCellViewInfo = class;
  TdxSpreadSheetTableViewCellViewInfoList = class;
  TdxSpreadSheetTableViewController = class;
  TdxSpreadSheetTableViewCustomCellViewInfo = class;
  TdxSpreadSheetTableViewCustomGroupingAreaViewInfo = class;
  TdxSpreadSheetTableViewHeaderCellViewInfo = class;
  TdxSpreadSheetTableViewInfo = class;
  TdxSpreadSheetTableViewMergedCellViewInfo = class;
  TdxSpreadSheetTableViewMergedCellViewInfoList = class;
  TdxSpreadSheetTableViewSelectionViewInfo = class;

  TdxSpreadSheetTableItems = class;
  TdxSpreadSheetTableItemClass = class of TdxSpreadSheetTableItem;
  TdxSpreadSheetTableItem = class;
  TdxSpreadSheetTableItemGroup = class;
  TdxSpreadSheetTableItemGroupList = class;
  TdxSpreadSheetTableColumn = class;
  TdxSpreadSheetTableRow = class;

  TdxSpreadSheetDefinedName = class;
  TdxSpreadSheetDefinedNames = class;

  TdxSpreadSheetExternalLink = class;
  TdxSpreadSheetExternalLinks = class;

  TdxSpreadSheetPageControl = class;

  TdxSpreadSheetMergedCell = class;

  TdxSpreadSheetProgressEvent = TcxProgressEvent;

  TdxSpreadSheetPageControlButton = (sspcbFirst, sspcbPrev, sspcbNext, sspcbLast, sspcbNew);
  TdxSpreadSheetPageControlButtons = set of TdxSpreadSheetPageControlButton;

  TdxSpreadSheetChange = (sscData, sscLayout, sscZoom, sscStyle, sscDimension, sscOptionsPrint, sscModified);
  TdxSpreadSheetChanges = set of TdxSpreadSheetChange;

  TdxSpreadSheetState = (sssReading, sssReadingCells, sssWriting, sssExporting);
  TdxSpreadSheetStates = set of TdxSpreadSheetState;

  TdxSpreadSheetTableViewForEachCellProc = reference to procedure (ACell: TdxSpreadSheetCell);

  TdxSpreadSheetTableViewSelectionMode = (smNone, smRows, smColumns, smCells);

  { Aliases }

  TdxSpreadSheetFormulaToken = dxSpreadSheetCoreFormulas.TdxSpreadSheetFormulaToken;
  TdxSpreadSheetFormulaResult = dxSpreadSheetCoreFormulas.TdxSpreadSheetFormulaResult;
  TdxSpreadSheetCellStyle = dxSpreadSheetStyles.TdxSpreadSheetCellStyle;
  TdxSpreadSheetCustomConditionalFormattingRule = dxSpreadSheetConditionalFormatting.TdxSpreadSheetCustomConditionalFormattingRule;

  { IdxSpreadSheetListener }

  IdxSpreadSheetListener = interface
  ['{F9D4DD62-FB94-45FA-8934-5A59B03DA598}']
    procedure DataChanged(Sender: TdxCustomSpreadSheet);
  end;

  { IdxSpreadSheetListener2 }

  IdxSpreadSheetListener2 = interface
  ['{B10199D7-0493-40E6-A686-89F00019D617}']
    procedure ActiveSheetChanged(Sender: TdxCustomSpreadSheet);
    procedure DefinedNamesChanged(Sender: TdxCustomSpreadSheet);
  end;

  { IdxSpreadSheetKeyboardListener }

  IdxSpreadSheetKeyboardListener = interface
  ['{953AB6CB-9E7F-4CA9-A720-AA1AD4BCC94C}']
    procedure KeyDown(Sender: TdxCustomSpreadSheet; var Key: Word; Shift: TShiftState);
  end;

  { IdxSpreadSheetEditControllerListener }

  IdxSpreadSheetEditControllerListener = interface
  ['{00C1AAE2-488B-4963-8518-9ED72A64B56A}']
    procedure Edited(Sender: TdxSpreadSheetCustomView);
    procedure Editing(Sender: TdxSpreadSheetCustomView);
    procedure EditingValueChanged(Sender: TdxSpreadSheetCustomView);
  end;

  { IdxSpreadSheetOptionsPrintListener }

  IdxSpreadSheetOptionsPrintListener = interface(IdxSpreadSheetListener)
  ['{1BF81B3F-206B-4049-9461-2424A72FEB97}']
    procedure OptionsChanged(Sender: TdxSpreadSheetCustomView);
  end;

  { IdxSpreadSheetSelectionListener }

  IdxSpreadSheetSelectionListener = interface(IdxSpreadSheetListener)
  ['{444A6B98-BF0A-474D-B3B5-AB88F5141A47}']
    procedure SelectionChanged(Sender: TdxSpreadSheetCustomView);
  end;

  { IdxSpreadSheetTableViewSelectionModeListener }

  IdxSpreadSheetTableViewSelectionModeListener = interface(IdxSpreadSheetSelectionListener)
  ['{52B30115-93EC-438C-ACC4-E4E1390B6974}']
    procedure SelectionModeChanged(Sender: TdxSpreadSheetTableView; AMode: TdxSpreadSheetTableViewSelectionMode);
  end;

  { IdxSpreadSheet }

  IdxSpreadSheet = interface
  ['{AB7B6216-4144-4EBB-BE18-BFF82971D905}']
    function GetControl: TdxCustomSpreadSheet;
  end;

  { IdxSpreadSheetTableView }

  IdxSpreadSheetTableView = interface
  ['{5A6CD395-25BF-48FE-8FC8-B320466367BD}']
    function GetAbsoluteCellBounds(const ARowIndex, AColumnIndex: Integer; ACheckMergedCells: Boolean = True): TRect;
    function GetCell(const ARowIndex, AColumnIndex: Integer): TdxSpreadSheetCell;
    function GetCellArea(const ARowIndex, AColumnIndex: Integer): TRect;
    function GetCellAtAbsolutePoint(const P: TPoint; out ARowIndex, AColumnIndex: Integer): Boolean;
  end;

  { TdxSpreadSheetListeners }

  TdxSpreadSheetListeners = class(TInterfaceList)
  public
    procedure NotifyActiveSheetChanged(Sender: TdxCustomSpreadSheet);
    procedure NotifyDataChanged(Sender: TdxCustomSpreadSheet);
    procedure NotifyDefinedNamesChanged(Sender: TdxCustomSpreadSheet);
    procedure NotifyEdited(Sender: TdxSpreadSheetCustomView);
    procedure NotifyEditing(Sender: TdxSpreadSheetCustomView);
    procedure NotifyEditingValueChanged(Sender: TdxSpreadSheetCustomView);
    procedure NotifyKeyDown(Sender: TdxCustomSpreadSheet; var Key: Word; ShiftState: TShiftState);
    procedure NotifyOptionsPrintChanged(Sender: TdxSpreadSheetCustomView);
    procedure NotifySelectionChanged(Sender: TdxSpreadSheetCustomView);
    procedure NotifySelectionModeChanged(Sender: TdxSpreadSheetTableView; AMode: TdxSpreadSheetTableViewSelectionMode);
  end;

  { TdxSpreadSheetPersistentObject }

  TdxSpreadSheetPersistentObject = class(TInterfacedPersistent)
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); virtual;

    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  end;

  { TdxSpreadSheetViewPersistentObject }

  TdxSpreadSheetViewPersistentObject = class
  strict private
    FView: TdxSpreadSheetCustomView;

    function GetScaleFactor: TdxScaleFactor;
    function GetSpreadSheet: TdxCustomSpreadSheet;
  public
    constructor Create(AView: TdxSpreadSheetCustomView); virtual;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
    property View: TdxSpreadSheetCustomView read FView;
  end;

  { TdxSpreadSheetObjectList }

  TdxSpreadSheetObjectList = class(TdxSpreadSheetPersistentObject)
  strict private
    FItems: TcxObjectList;

    function GetCount: Integer;
    function GetItem(AIndex: Integer): TdxSpreadSheetObjectListItem;
  protected
    function CreateItem: TdxSpreadSheetObjectListItem; virtual;
    procedure Changed; virtual;

    property ItemList: TcxObjectList read FItems;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Remove(AItem: TdxSpreadSheetObjectListItem);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxSpreadSheetObjectListItem read GetItem;
  end;

  { TdxSpreadSheetObjectListItem }

  TdxSpreadSheetObjectListItem = class
  strict private
    FOwner: TdxSpreadSheetObjectList;
    function GetIndex: Integer;
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetObjectList); virtual;
    destructor Destroy; override;

    property Index: Integer read GetIndex;
    property Owner: TdxSpreadSheetObjectList read FOwner;
  end;

  { TdxSpreadSheetDefaultCellStyle }

  TdxSpreadSheetDefaultCellStyle = class(TdxSpreadSheetCellStyle)
  protected
    procedure CloneHandle; override;
    procedure ReleaseHandle; override;
    procedure ReplaceHandle; override;
    procedure SetHandle(const AHandle: TdxSpreadSheetCellStyleHandle); override;
  public
    constructor Create(const AOwner: IdxSpreadSheetCellStyleOwner); override;
    procedure Reset;
  end;

  { TdxSpreadSheetCellDisplayValue }

  TdxSpreadSheetCellDisplayValue = class
  strict private
    FFormatCode: string;
    FFormattedValue: TdxSpreadSheetNumberFormatResult;
    FValue: Variant;

    function CheckNeedUpdateValue(const AValue: Variant; const AFormatCode: string): Boolean; inline;
    procedure SetAsText(const AText: string);
  public
    IsDirty: Boolean;
    ShrinkToFitCacheContentWidth: Integer;
    ShrinkToFitCacheFontSize: Integer;

    constructor Create;
    function Update(const ACell: TdxSpreadSheetCell): TdxSpreadSheetNumberFormatResult;
  end;

  { TdxSpreadSheetCell }

  TdxSpreadSheetCell = class(TdxDynamicListItem,
    IdxSpreadSheetCellData,
    IdxSpreadSheetCellStyleOwner)
  strict private
    FShowFormula: TdxDefaultBoolean;

    function GetActualDataType: TdxSpreadSheetCellDataType;
    function GetActualShowFormula: Boolean;
    function GetColumn: TdxSpreadSheetTableColumn; inline;
    function GetColumnIndex: Integer; inline;
    function GetDisplayText: string;
    function GetDisplayValue: TdxSpreadSheetNumberFormatResult;
    function GetHasValue: Boolean;
    function GetHistory: TdxSpreadSheetHistory; inline;
    function GetIsEmpty: Boolean;
    function GetIsFormula: Boolean;
    function GetIsMerged: Boolean;
    function GetRow: TdxSpreadSheetTableRow; inline;
    function GetRowIndex: Integer; inline;
    function GetSpreadSheet: TdxCustomSpreadSheet; inline;
    function GetStyle: TdxSpreadSheetCellStyle;
    function GetStyleHandle: TdxSpreadSheetCellStyleHandle;
    function GetView: TdxSpreadSheetTableView; inline;
    procedure SetColumn(AValue: TdxSpreadSheetTableColumn);
    procedure SetColumnIndex(AValue: Integer);
    procedure SetIsEmpty(AValue: Boolean);
    procedure SetRow(AValue: TdxSpreadSheetTableRow);
    procedure SetRowIndex(AValue: Integer);
    procedure SetShowFormula(AValue: TdxDefaultBoolean);
    procedure SetStyleHandle(const AValue: TdxSpreadSheetCellStyleHandle);

    // IdxSpreadSheetCellData
    function GetAsFormulaObject: TObject;
    function GetDataType: TdxSpreadSheetCellDataType;
    function IdxSpreadSheetCellData.GetAsFormula = GetAsFormulaObject;
  protected
    FData: array [0..9] of Byte;
    FDataType: TdxSpreadSheetCellDataType;
    FDisplayValue: TdxSpreadSheetCellDisplayValue;
    FStyleObject: TObject;

    procedure AssignData(ASource: TdxSpreadSheetCell);
    procedure Changed(AChanges: TdxSpreadSheetChanges); virtual;
    procedure CheckAndDeleteObjectData; inline;
    procedure CheckAreaReferenceTokens(var AParentToken: TdxSpreadSheetFormulaToken);
    procedure CheckDefaultCellFormat; virtual;
    procedure Move(ARow: TdxSpreadSheetTableRow; AColumn: TdxSpreadSheetTableColumn); overload;
    procedure Move(ARowIndex, AColumnIndex: Integer); overload;
    procedure ReleaseDisplayValue; inline;
    procedure ReleaseWrappers;
    procedure SetDisplayValueDirty; inline;
    procedure StyleChanged; virtual;

    function IsMultiline: Boolean; virtual;
    function IsPartOfArrayFormula(AFormula: PdxSpreadSheetCustomFormula = nil): TdxSpreadSheetArrayFormulaPart;
    function IsValueFormattedString: Boolean; inline;

    procedure LoadFromStream(AReader: TcxReader; AFormulasRef: TdxSpreadSheetFormulaAsTextInfoList); virtual;
    procedure SaveToStream(AWriter: TcxWriter); virtual;

    class function GetContentOffsets(ABottomBorderStyle: TdxSpreadSheetCellBorderStyle): TRect; inline;

    function GetAsBoolean: Boolean; virtual;
    function GetAsCurrency: Currency; virtual;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsError: TdxSpreadSheetFormulaErrorCode; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsFormula: TdxSpreadSheetFormula; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsSharedString: TdxSpreadSheetSharedString; virtual;
    function GetAsString: string; virtual;
    function GetAsVariant: Variant; virtual;

    procedure SetAsBoolean(const AValue: Boolean); virtual;
    procedure SetAsCurrency(const AValue: Currency); virtual;
    procedure SetAsDateTime(const AValue: TDateTime); virtual;
    procedure SetAsError(const AValue: TdxSpreadSheetFormulaErrorCode); virtual;
    procedure SetAsFloat(const AValue: Double); virtual;
    procedure SetAsFormula(const AValue: TdxSpreadSheetFormula); virtual;
    procedure SetAsInteger(const AValue: Integer); virtual;
    procedure SetAsSharedString(AValue: TdxSpreadSheetSharedString); virtual;
    procedure SetAsString(const AValue: string); virtual;
    procedure SetAsVariant(const AValue: Variant); virtual;
    procedure SetTextCore(const AText: string; const AFormulaChecking: Boolean); virtual;

    // IdxSpreadSheetCellStyleOwner
    procedure IdxSpreadSheetCellStyleOwner.CellStyleChanged = StyleChanged;
    procedure CellStyleChanging;
    function GetCellStyles: TdxSpreadSheetCellStyles;
    function GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
    procedure ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);

    property ActualDataType: TdxSpreadSheetCellDataType read GetActualDataType;
    property ActualShowFormula: Boolean read GetActualShowFormula;
    property History: TdxSpreadSheetHistory read GetHistory;
  public
    constructor Create(AOwner: TdxDynamicItemList; AIndex: Integer); override;
    destructor Destroy; override;
    procedure Assign(ASource: TdxDynamicListItem); override;
    procedure BeforeDestruction; override;
    procedure Clear;
    function GetAbsoluteBounds: TRect;
    function GetReference(AOptions: TdxSpreadSheetCellReferenceOptions = []): string; overload;
    function GetReference(AR1C1ReferenceStyle: Boolean; AOptions: TdxSpreadSheetCellReferenceOptions = []): string; overload;
    function IsNumericValue: Boolean;

    function GetAsRTF(out AText: string): Boolean;
    function SetAsRTF(const AText: string): Boolean;
    procedure SetText(const AText: string; const AFormulaChecking: Boolean = False); virtual;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsError: TdxSpreadSheetFormulaErrorCode read GetAsError write SetAsError;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsFormula: TdxSpreadSheetFormula read GetAsFormula write SetAsFormula;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsSharedString: TdxSpreadSheetSharedString read GetAsSharedString write SetAsSharedString;
    property AsString: string read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property Column: TdxSpreadSheetTableColumn read GetColumn write SetColumn;
    property ColumnIndex: Integer read GetColumnIndex write SetColumnIndex;
    property DataType: TdxSpreadSheetCellDataType read FDataType;
    property DisplayText: string read GetDisplayText;
    property DisplayValue: TdxSpreadSheetNumberFormatResult read GetDisplayValue;
    property HasValue: Boolean read GetHasValue;
    property IsEmpty: Boolean read GetIsEmpty write SetIsEmpty;
    property IsFormula: Boolean read GetIsFormula;
    property IsMerged: Boolean read GetIsMerged;
    property Row: TdxSpreadSheetTableRow read GetRow write SetRow;
    property RowIndex: Integer read GetRowIndex write SetRowIndex;
    property ShowFormula: TdxDefaultBoolean read FShowFormula write SetShowFormula;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
    property Style: TdxSpreadSheetCellStyle read GetStyle;
    property StyleHandle: TdxSpreadSheetCellStyleHandle read GetStyleHandle write SetStyleHandle;
    property View: TdxSpreadSheetTableView read GetView;
  end;

  { TdxSpreadSheetCellHelper }

  TdxSpreadSheetCellHelper = class
  public
    class procedure DecodeRef(const ARef: Int64; out ARow, AColumn: Integer);

    class function EncodeRef(ACell: TdxSpreadSheetCell): Int64; overload;
    class function EncodeRef(ARow, AColumn: Integer): Int64; overload;
    class function EncodeRefAsString(ACell: TdxSpreadSheetCell): string;

    class function GetCell(ARow, AColumn: Integer; AView: TdxSpreadSheetCustomView): TdxSpreadSheetCell;
    class function ReadRef(AReader: TcxReader; AView: TdxSpreadSheetCustomView;
      AAnchorRow: Integer = 0; AAnchorColumn: Integer = 0): TdxSpreadSheetCell;
    class procedure WriteRef(AWriter: TcxWriter; ACell: TdxSpreadSheetCell;
      AAnchorRow: Integer = 0; AAnchorColumn: Integer = 0);
  end;

  { TdxSpreadSheetCustomDrawingObject }

  TdxSpreadSheetCustomDrawingObject = class(TdxSpreadSheetPersistentObject)
  strict private
    FIsDestroying: Boolean;

    FOnChange: TNotifyEvent;
  protected
    procedure Changed; virtual;
    //
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure Assign(Source: TPersistent); override;
    procedure BeforeDestruction; override;
  end;

  { TdxSpreadSheetContainerAnchorPoint }

  TdxSpreadSheetContainerAnchorPoint = class(TcxOwnedPersistent)
  strict private
    FCell: TdxSpreadSheetCell;
    FFixedToCell: Boolean;
    FOffset: TPoint;

    function GetContainer: TdxSpreadSheetContainer; inline;
    procedure SetCell(const AValue: TdxSpreadSheetCell);
    procedure SetOffset(const AValue: TPoint);
  protected
    procedure Changed; virtual;
    procedure DoAssign(Source: TPersistent); override;

    procedure LoadFromStream(AReader: TcxReader); virtual;
    procedure SaveToStream(AWriter: TcxWriter); virtual;
    //
    property Container: TdxSpreadSheetContainer read GetContainer;
  public
    constructor Create(AOwner: TPersistent); override;
    //
    property Cell: TdxSpreadSheetCell read FCell write SetCell;
    property FixedToCell: Boolean read FFixedToCell write FFixedToCell;
    property Offset: TPoint read FOffset write SetOffset;
  end;

  { TdxSpreadSheetContainerTransform }

  TdxSpreadSheetContainerTransform = class(TcxOwnedPersistent)
  strict private
    FFlipHorizontally: Boolean;
    FFlipVertically: Boolean;
    FRotationAngle: Double;

    function GetContainer: TdxSpreadSheetContainer; inline;
    procedure SetFlipHorizontally(AValue: Boolean);
    procedure SetFlipVertically(AValue: Boolean);
    procedure SetRotationAngle(const AValue: Double);
  protected
    procedure Changed; virtual;
    procedure DoAssign(Source: TPersistent); override;

    procedure LoadFromStream(AReader: TcxReader); virtual;
    procedure SaveToStream(AWriter: TcxWriter); virtual;
    //
    property Container: TdxSpreadSheetContainer read GetContainer;
  public
    property FlipHorizontally: Boolean read FFlipHorizontally write SetFlipHorizontally;
    property FlipVertically: Boolean read FFlipVertically write SetFlipVertically;
    property RotationAngle: Double read FRotationAngle write SetRotationAngle;
  end;

  { TdxSpreadSheetContainer }

  TdxSpreadSheetContainerAnchorType = (catAbsolute, catOneCell, catTwoCell);

  TdxSpreadSheetContainerChange = (ccContent, ccAnchors, ccPosition, ccTransform, ccVisibility);
  TdxSpreadSheetContainerChanges = set of TdxSpreadSheetContainerChange;

  TdxSpreadSheetContainerRestriction = (crNoCrop, crNoMove, crNoResize, crNoRotation, crNoChangeAspectUsingCornerHandles);
  TdxSpreadSheetContainerRestrictions = set of TdxSpreadSheetContainerRestriction;

  TdxSpreadSheetContainerClass = class of TdxSpreadSheetContainer;
  TdxSpreadSheetContainer = class(TdxSpreadSheetPersistentObject)
  strict private
    FAnchorPoint1: TdxSpreadSheetContainerAnchorPoint;
    FAnchorPoint2: TdxSpreadSheetContainerAnchorPoint;
    FAnchorType: TdxSpreadSheetContainerAnchorType;
    FCalculator: TdxSpreadSheetContainerCalculator;
    FChanges: TdxSpreadSheetContainerChanges;
    FDescription: string;
    FHyperlink: TdxSpreadSheetHyperlink;
    FName: string;
    FParent: TdxSpreadSheetCustomView;
    FRestrictions: TdxSpreadSheetContainerRestrictions;
    FTitle: string;
    FTransform: TdxSpreadSheetContainerTransform;
    FUpdateCount: Integer;
    FVisible: Boolean;

    function GetFocused: Boolean;
    function GetHistory: TdxSpreadSheetHistory;
    function GetIndex: Integer;
    function GetIsUpdating: Boolean;
    procedure SetAnchorPoint1(const AValue: TdxSpreadSheetContainerAnchorPoint);
    procedure SetAnchorPoint2(const AValue: TdxSpreadSheetContainerAnchorPoint);
    procedure SetAnchorType(const AValue: TdxSpreadSheetContainerAnchorType);
    procedure SetFocused(const AValue: Boolean);
    procedure SetHyperlink(const AValue: TdxSpreadSheetHyperlink);
    procedure SetIndex(AValue: Integer);
    procedure SetParent(const AValue: TdxSpreadSheetCustomView);
    procedure SetTransform(AValue: TdxSpreadSheetContainerTransform);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure Changed(AChanges: TdxSpreadSheetContainerChanges); virtual;
    function CreateAnchorPoint: TdxSpreadSheetContainerAnchorPoint; virtual;
    function CreateCalculator: TdxSpreadSheetContainerCalculator; virtual;
    function CreateTransform: TdxSpreadSheetContainerTransform; virtual;
    function CreateViewInfo: TdxSpreadSheetContainerViewInfo; virtual;
    procedure CreateSubClasses; virtual;
    procedure DoAssign(Source: TPersistent); virtual;
    procedure DoChanged(AChanges: TdxSpreadSheetContainerChanges); virtual;
    procedure DoSetParent(AValue: TdxSpreadSheetCustomView); virtual;

    procedure CellRemoving(ACell: TdxSpreadSheetCell); virtual;
    function IsCellUsed(ACell: TdxSpreadSheetCell): Boolean; virtual;

    function CanFocusViaKeyboard: Boolean; virtual;
    function CanClone: Boolean; virtual;
    function CanMove: Boolean; virtual;
    function CanResize: Boolean; virtual;
    function CanRotate: Boolean; virtual;
    function IsTransformsSupported: Boolean; virtual;
    function KeepAspectUsingCornerHandles: Boolean; virtual;
    procedure ReleaseHyperlink;

    procedure LoadFromStream(AReader: TcxReader; ACanReadContent: Boolean = True); overload;
    procedure LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True); overload; virtual;
    procedure SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True); virtual;

    property Calculator: TdxSpreadSheetContainerCalculator read FCalculator;
    property History: TdxSpreadSheetHistory read GetHistory;
    property IsUpdating: Boolean read GetIsUpdating;
  public
    constructor Create(AView: TdxSpreadSheetCustomView); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeforeDestruction; override;
    procedure BeginChanging;
    procedure EndChanging;
    procedure BeginUpdate;
    procedure EndUpdate;
    //
    procedure BringToFront;
    procedure SendToBack;
    //
    property AnchorPoint1: TdxSpreadSheetContainerAnchorPoint read FAnchorPoint1 write SetAnchorPoint1;
    property AnchorPoint2: TdxSpreadSheetContainerAnchorPoint read FAnchorPoint2 write SetAnchorPoint2;
    property AnchorType: TdxSpreadSheetContainerAnchorType read FAnchorType write SetAnchorType;
    property Description: string read FDescription write FDescription;
    property Focused: Boolean read GetFocused write SetFocused;
    property Hyperlink: TdxSpreadSheetHyperlink read FHyperlink write SetHyperlink;
    property Index: Integer read GetIndex write SetIndex;
    property Name: string read FName write FName;
    property Parent: TdxSpreadSheetCustomView read FParent write SetParent;
    property Restrictions: TdxSpreadSheetContainerRestrictions read FRestrictions write FRestrictions;
    property Title: string read FTitle write FTitle;
    property Transform: TdxSpreadSheetContainerTransform read FTransform write SetTransform;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  { TdxSpreadSheetContainerCalculator }

  TdxSpreadSheetContainerCalculator = class
  strict private
    FOwner: TdxSpreadSheetContainer;

    function GetAnchorPoint1: TdxSpreadSheetContainerAnchorPoint; inline;
    function GetAnchorPoint2: TdxSpreadSheetContainerAnchorPoint; inline;
    function GetAnchorType: TdxSpreadSheetContainerAnchorType; inline;
    function GetTransform: TdxSpreadSheetContainerTransform; inline;
    procedure SetAnchorType(const Value: TdxSpreadSheetContainerAnchorType); inline;
  protected
    function CheckForContentArea(const R: TRect): TRect;
    function GetAbsoluteCellBounds(ACell: TdxSpreadSheetCell): TRect;
    function GetContentRect: TRect;
    function GetNearCells(const R: TRect; out ACell1, ACell2: TdxSpreadSheetCell): Boolean; virtual;
    procedure ModifyBounds(ALeftModifier, ATopModifier, ARightModifier, ABottomModifier: Integer);
    procedure Resize(ADeltaX, ADeltaY: Integer);
    function TransformCoords(const R: TRect; ABackwardDirection: Boolean): TRect; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetContainer); virtual;
    function CalculateBounds: TRect; virtual;
    procedure UpdateAnchors(const ABounds: TRect); virtual;
    procedure UpdateAnchorsAfterResize(const APrevBounds: TRect; AForceUpdate: Boolean = False); virtual;
    //
    property AnchorPoint1: TdxSpreadSheetContainerAnchorPoint read GetAnchorPoint1;
    property AnchorPoint2: TdxSpreadSheetContainerAnchorPoint read GetAnchorPoint2;
    property AnchorType: TdxSpreadSheetContainerAnchorType read GetAnchorType write SetAnchorType;
    property Transform: TdxSpreadSheetContainerTransform read GetTransform;
    property Owner: TdxSpreadSheetContainer read FOwner;
  end;

  { TdxSpreadSheetContainers }

  TdxSpreadSheetContainerEnumProc = reference to function (AContainer: TdxSpreadSheetContainer): Boolean;

  TdxSpreadSheetContainers = class(TObjectList<TdxSpreadSheetContainer>)
  strict private
    FBoundsStorage: TDictionary<TdxSpreadSheetContainer, TRect>;
    FBoundsStorageLockCount: Integer;
    FOwner: TdxSpreadSheetCustomView;

    function FindNearestVisibleContainer(AStartFromIndex, ADirection: Integer): TdxSpreadSheetContainer;
  protected
    FComments: TDictionary<Int64, TdxSpreadSheetContainer>;

    function FindCommentContainerCore(ARow, AColumn: Integer; out AContainer): Boolean;

    procedure CellRemoving(ACell: TdxSpreadSheetCell);
    procedure InternalAdd(AContainer: TdxSpreadSheetContainer);
    procedure InternalRemove(AContainer: TdxSpreadSheetContainer);

    procedure RegisterCommentContainer(AContainer: TdxSpreadSheetContainer);
    procedure UnregisterCommentContainer(AContainer: TdxSpreadSheetContainer);

    procedure AfterResize;
    procedure BeforeResize;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomView); virtual;
    destructor Destroy; override;

    function Add(AClass: TdxSpreadSheetContainerClass): TdxSpreadSheetContainer; overload;
    procedure Add(AClass: TdxSpreadSheetContainerClass; out AContainer); overload;
    function AddCommentContainer(ACell: TdxSpreadSheetCell): TdxSpreadSheetContainer; overload;
    procedure AddCommentContainer(ACell: TdxSpreadSheetCell; out AContainer); overload;

    function GetFirstVisibleContainer: TdxSpreadSheetContainer;
    function GetLastVisibleContainer: TdxSpreadSheetContainer;
    function GetNextVisibleContainer(ACurrentContainer: TdxSpreadSheetContainer): TdxSpreadSheetContainer;
    function GetPrevVisibleContainer(ACurrentContainer: TdxSpreadSheetContainer): TdxSpreadSheetContainer;
    function IsCellUsed(ACell: TdxSpreadSheetCell): Boolean;

    // Comments
    procedure EnumCommentContainers(AProc: TdxSpreadSheetContainerEnumProc); overload;
    procedure EnumCommentContainers(const AArea: TRect; AProc: TdxSpreadSheetContainerEnumProc); overload;
    function FindCommentContainer(ACell: TdxSpreadSheetCell): TdxSpreadSheetContainer; overload;
    function FindCommentContainer(ACell: TdxSpreadSheetCell; out AContainer): Boolean; overload;
    function FindCommentContainer(ARow, AColumn: Integer): TdxSpreadSheetContainer; overload;
    function FindCommentContainer(ARow, AColumn: Integer; out AContainer): Boolean; overload;
    function FindNextCommentContainer(ACell: TdxSpreadSheetCell; out AContainer; AGoForward: Boolean = True): Boolean; overload;
    function FindNextCommentContainer(ARow, AColumn: Integer; out AContainer; AGoForward: Boolean = True): Boolean; overload;

    property Owner: TdxSpreadSheetCustomView read FOwner;
  end;

  { TdxSpreadSheetFormula }

  TdxSpreadSheetFormula = class(TdxSpreadSheetCustomFormula)
  strict private
    FOwner: TdxSpreadSheetCell;
  protected
    procedure CalculateAnchors; override;
    procedure ClearResult; override;
    function GetController: TdxSpreadSheetCustomFormulaController; override;
    function GetMaxIterationCount: Integer; override;
    function GetView: TObject; override;
    procedure Initialize(ACell: TdxSpreadSheetCell);
    procedure InternalSetOwner(ACell: TdxSpreadSheetCell);
    function IsLinkedToCell: Boolean; override;
  public
    constructor Create(AOwner: TdxSpreadSheetCell); virtual;
    destructor Destroy; override;
    function Clone: TdxSpreadSheetFormula;

    property Cell: TdxSpreadSheetCell read FOwner;
  end;

  { TdxSpreadSheetFormulaAsTextInfo }

  TdxSpreadSheetFormulaAsTextInfo = class
  protected
    procedure ResolveReferences(Parser: TObject); virtual;
  public
    Anchor: TPoint;
    Cell: TdxSpreadSheetCell;
    Formula: string;
    FormulaResult: Variant;
    IsArray: Boolean;
    IsShared: Boolean;
    Reference: TRect;

    constructor Create(AFormula: TdxSpreadSheetFormula = nil);
    procedure LoadFromStream(AReader: TcxReader);
    procedure SaveToStream(AWriter: TcxWriter);
    class procedure SaveFormulaToStream(AFormula: TdxSpreadSheetFormula; AWriter: TcxWriter);
  end;

  { TdxSpreadSheetFormulaAsTextInfoList }

  TdxSpreadSheetFormulaAsTextInfoList = class(TObjectList<TdxSpreadSheetFormulaAsTextInfo>)
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;
  protected
    function CreateItem: TdxSpreadSheetFormulaAsTextInfo; virtual;
    function CreateParser: TObject; virtual; // TdxSpreadSheetFormulaParser
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); virtual;
    function Add(ACell: TdxSpreadSheetCell; const AFormula: string;
      AIsArray, AIsShared: Boolean; const AReference: TRect): TdxSpreadSheetFormulaAsTextInfo; overload;
    function Add(ACell: TdxSpreadSheetCell; const AAnchor: TPoint; const AFormula: string;
      AIsArray, AIsShared: Boolean; const AReference: TRect): TdxSpreadSheetFormulaAsTextInfo; overload;
    function AddFromStream(ACell: TdxSpreadSheetCell; AReader: TcxReader): TdxSpreadSheetFormulaAsTextInfo;
    function HasCell(ACell: TdxSpreadSheetCell): Boolean;
    procedure ResolveReferences; virtual;
    //
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  end;

  { TdxSpreadSheetDefinedNameFormula }

  TdxSpreadSheetDefinedNameFormula = class(TdxSpreadSheetFormula)
  strict private
    FOwner: TdxSpreadSheetDefinedName;
  protected
    function GetController: TdxSpreadSheetCustomFormulaController; override;
    function GetView: TObject; override;
  public
    constructor Create(AOwner: TdxSpreadSheetDefinedName); reintroduce; overload;

    property Owner: TdxSpreadSheetDefinedName read FOwner;
  end;

  { TdxSpreadSheetControlFormatSettings }

  TdxSpreadSheetControlFormatSettings = class(TdxSpreadSheetFormatSettings)
  strict private
    FOwner: TdxCustomSpreadSheet;
  protected
    function GetLocaleID: Integer; override;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet); virtual;
    function ExpandExternalLinks: Boolean; override;
    function GetFunctionName(const AName: Pointer): string; override;
    procedure UpdateSettings; override;

    property Owner: TdxCustomSpreadSheet read FOwner;
  end;

  { TdxSpreadSheetFormulaController }

  TdxSpreadSheetFormulaController = class(TdxSpreadSheetCustomFormulaController)
  strict private
    FOwner: TdxCustomSpreadSheet;

    function GetItem(Index: Integer): TdxSpreadSheetFormula;
  protected
    procedure ClearFormulasResults; override;
    procedure DoNameChanged(AName: TdxSpreadSheetDefinedName; const ANewName: string);
    function CreateParser: TObject; override;
    function GetCellFromPath(APath: TdxSpreadSheetReferencePath): TdxSpreadSheetCell;
    function GetFormatSettings: TdxSpreadSheetFormatSettings; override;
    function NeedAddFeatureFunctionPrefixToFunctionName: Boolean; override;
    function NeedRecalculate: Boolean;
    procedure Recalculate;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet); virtual;
    procedure Calculate; override;
    procedure UpdateReferences(AFormula: TdxSpreadSheetCustomFormula; AView: TdxSpreadSheetTableView; const AArea: TRect;
      const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode); overload;
    procedure UpdateReferences(AView: TdxSpreadSheetTableView; const AArea: TRect;
      const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode); overload;

    property Items[Index: Integer]: TdxSpreadSheetFormula read GetItem; default;
    property SpreadSheet: TdxCustomSpreadSheet read FOwner;
  end;

  { TdxSpreadSheetCustomController }

  TdxSpreadSheetCustomController = class
  strict private
    FOwner: TObject;
    FPopupMenu: TComponent;
    FPressedObject: TObject;
  protected
    function ContextPopup(const P: TPoint): Boolean; virtual;

    function CanFocusOnClick: Boolean; virtual;
    procedure DblClick; virtual;
    function GetCursor(const ACursorPos: TPoint): TCursor; virtual;
    function GetDragAndDropObjectClass(const ACursorPos: TPoint): TcxDragAndDropObjectClass; virtual;
    function GetHitTest: TdxSpreadSheetCustomHitTest; virtual;
    function GetPopupMenuClass(const ACursorPos: TPoint): TComponentClass; virtual;
    function GetSpreadSheet: TdxCustomSpreadSheet; virtual; abstract;
    function GetZoomFactor: Integer; virtual;

    function IMEComposition(var AMessage: TMessage): Boolean; virtual;
    function IMEStartComposition: Boolean; virtual;
    procedure StopEditing; virtual;
    procedure UpdateStates; virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;

    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; const P: TPoint): Boolean; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer; const P: TPoint): Boolean;

    property PopupMenu: TComponent read FPopupMenu;
    property PressedObject: TObject read FPressedObject;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;

    property HitTest: TdxSpreadSheetCustomHitTest read GetHitTest;
    property Owner: TObject read FOwner;
    property ZoomFactor: Integer read GetZoomFactor;
  end;

  { TdxSpreadSheetCustomViewController }

  TdxSpreadSheetCustomViewController = class(TdxSpreadSheetCustomController)
  strict private
    FFocusedContainer: TdxSpreadSheetContainer;
    FFocusedContainerWasHidden: Boolean;

    function GetContainers: TdxSpreadSheetContainers;
    function GetView: TdxSpreadSheetCustomView;
    procedure SetFocusedContainer(AValue: TdxSpreadSheetContainer);
  protected
    procedure CheckScrollArea(const P: TPoint); overload; virtual;
    procedure CheckScrollArea(X, Y: Integer); overload; inline;

    function ContainerCanDelete: Boolean; virtual;
    function ContainerProcessKeyDown(Key: Word; Shift: TShiftState): Boolean; virtual;
    procedure FocusContainer(AContainer: TdxSpreadSheetContainer; AMakeVisible: Boolean); virtual;
    procedure MoveContainer(AContainer: TdxSpreadSheetContainer; ADeltaX, ADeltaY: Integer);
    procedure ResizeContainer(AContainer: TdxSpreadSheetContainer; ADeltaX, ADeltaY: Integer);
    procedure RotateContainer(AContainer: TdxSpreadSheetContainer; ADelta: Single);

    function GetHitTest: TdxSpreadSheetCustomHitTest; override;
    function GetSpreadSheet: TdxCustomSpreadSheet; override;
    function GetZoomFactor: Integer; override;
    procedure GetRedoActionCount(var ARedoCount: Integer); virtual;
    procedure GetUndoActionCount(var AUndoCount: Integer); virtual;
    function Redo(const ARedoCount: Integer = 1): Boolean; virtual;
    function Undo(const AUndoCount: Integer = 1): Boolean; virtual;
  public
    property Containers: TdxSpreadSheetContainers read GetContainers;
    property FocusedContainer: TdxSpreadSheetContainer read FFocusedContainer write SetFocusedContainer;
    property View: TdxSpreadSheetCustomView read GetView;
  end;

  { TdxSpreadSheetCustomHitTest }

  TdxSpreadSheetCustomHitTest = class
  strict private
    FHitCode: Integer;
    FHitObject: TdxSpreadSheetCustomCellViewInfo;
    FHitObjectData: TdxNativeInt;
    FHitPoint: TPoint;
    FOwner: TObject;
  protected
    FActualHitPoint: TPoint;

    function CanDrag(const P: TPoint): Boolean; inline;
    function GetActualHitPoint: TPoint; virtual;
    function GetCursor(const P: TPoint): TCursor; inline;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; overload; inline;
    function GetDragAndDropObjectClass(const P: TPoint): TcxDragAndDropObjectClass; overload; inline;
    function GetHitCode(ACode: Integer): Boolean; inline;
    procedure SetHitCode(ACode: Integer; AValue: Boolean); inline;

    property HitCodes[ACode: Integer]: Boolean read GetHitCode write SetHitCode;
    property Owner: TObject read FOwner;
  public
    constructor Create(AOwner: TObject); virtual;
    procedure Calculate(const AHitPoint: TPoint); virtual;
    procedure Clear;
    function GetPopupMenuClass(const P: TPoint): TComponentClass;
    procedure Recalculate;

    property ActualHitPoint: TPoint read FActualHitPoint;
    property HitObject: TdxSpreadSheetCustomCellViewInfo read FHitObject write FHitObject;
    property HitObjectData: TdxNativeInt read FHitObjectData write FHitObjectData;
    property HitPoint: TPoint read FHitPoint;
  end;

  { TdxSpreadSheetCustomViewOptions }

  TdxSpreadSheetCustomViewOptions = class(TcxLockablePersistent)
  strict private
    function GetView: TdxSpreadSheetCustomView;
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure DoChanged; override;
    //
    property View: TdxSpreadSheetCustomView read GetView;
  end;

  { TdxSpreadSheetCustomView }

  TdxSpreadSheetCustomView = class(TdxSpreadSheetPersistentObject, IdxSpreadSheetViewCaption)
  strict private
    FCaption: string;
    FChanges: TdxSpreadSheetChanges;
    FContainers: TdxSpreadSheetContainers;
    FController: TdxSpreadSheetCustomViewController;
    FHitTest: TdxSpreadSheetCustomHitTest;
    FIsCaptionTextDelimited: Boolean;
    FIsDestroying: Boolean;
    FLockCount: Integer;
    FOptions: TdxSpreadSheetCustomViewOptions;
    FScaleFactor: TdxOwnedScaleFactor;
    FViewInfo: TdxSpreadSheetCustomViewViewInfo;
    FVisible: Boolean;

    function GetActive: Boolean;
    function GetBounds: TRect;
    function GetCaption: string;
    function GetCellStyles: TdxSpreadSheetCellStyles;
    function GetFormatSettings: TdxSpreadSheetFormatSettings;
    function GetIndex: Integer;
    function GetIsDestroying: Boolean;
    function GetIsLocked: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetCaption(AValue: string);
    procedure SetIndex(AValue: Integer);
    procedure SetOptions(AValue: TdxSpreadSheetCustomViewOptions);
    procedure SetVisible(AValue: Boolean);
    //
    procedure ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
  protected
    FMergeCellStylesLockCount: Integer;

    procedure AddChanges(AChanges: TdxSpreadSheetChanges); inline;
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CheckChanges;
    procedure DoCheckChanges; virtual;
    procedure DoDataChanged; virtual;
    procedure InitScrollBarsParameters; virtual;
    function IsCaptionTextDelimited: Boolean;
    procedure Loaded; virtual;
    procedure RecalculateBestFit; virtual;
    procedure SelectionChanged; virtual;
    procedure ValidateCaption; virtual;

    function CreateContainers: TdxSpreadSheetContainers; virtual;
    function CreateController: TdxSpreadSheetCustomViewController; virtual;
    function CreateHitTest: TdxSpreadSheetCustomHitTest; virtual;
    function CreateOptions: TdxSpreadSheetCustomViewOptions; virtual;
    function CreateViewInfo: TdxSpreadSheetCustomViewViewInfo; virtual;
    //
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    //
    function GetContentOrigin: TPoint; virtual;
    function GetPartOffsetByPoint(const P: TPoint): TPoint; virtual;
    function GetZoomFactor: Integer; virtual;
    //
    procedure Pack; virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;

    property Changes: TdxSpreadSheetChanges read FChanges write FChanges;
    property Controller: TdxSpreadSheetCustomViewController read FController;
    property FormatSettings: TdxSpreadSheetFormatSettings read GetFormatSettings;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsLocked: Boolean read GetIsLocked;
    property LockCount: Integer read FLockCount;
    property Options: TdxSpreadSheetCustomViewOptions read FOptions write SetOptions;
    property ScaleFactor: TdxOwnedScaleFactor read FScaleFactor;
    property ViewInfo: TdxSpreadSheetCustomViewViewInfo read FViewInfo;
    property ZoomFactor: Integer read GetZoomFactor;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeforeDestruction; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Invalidate;
    procedure InvalidateRect(const R: TRect); virtual;

    function CanEditContainers: Boolean; virtual;

    property Active: Boolean read GetActive write SetActive stored False;
    property Bounds: TRect read GetBounds;
    property Caption: string read GetCaption write SetCaption;
    property CellStyles: TdxSpreadSheetCellStyles read GetCellStyles;
    property Containers: TdxSpreadSheetContainers read FContainers;
    property HitTest: TdxSpreadSheetCustomHitTest read FHitTest;
    property Index: Integer read GetIndex write SetIndex stored False;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TdxSpreadSheetCustomViewPainter }

  TdxSpreadSheetCustomViewPainter = class(TdxSpreadSheetViewPersistentObject)
  strict private
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
  public
    procedure FlushCache; virtual;
    procedure PrepareCanvasFont(ACanvas: TcxCanvas; AFont: TFont);
    //
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  end;

  { TdxSpreadSheetCustomViewViewInfo }

  TdxSpreadSheetCustomViewViewInfo = class(TdxSpreadSheetViewPersistentObject)
  strict private
    FContainers: TdxSpreadSheetCellViewInfoList;
    FIsDirty: Boolean;
    FPainter: TdxSpreadSheetCustomViewPainter;

    procedure SetIsDirty(AValue: Boolean);
  protected
    FBounds: TRect;

    // Containers
    procedure CalculateContainerDependedCells(AContainerViewInfo: TdxSpreadSheetContainerViewInfo); virtual;
    procedure CalculateContainers; virtual;
    function CalculateContainersHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; virtual;

    function CreatePainter: TdxSpreadSheetCustomViewPainter; virtual;
    procedure InitScrollBarsParameters; virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure SelectionChanged; virtual;
  public
    constructor Create(AView: TdxSpreadSheetCustomView); override;
    destructor Destroy; override;
    procedure Calculate; virtual;
    procedure Clear; virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    procedure Recalculate;
    procedure Validate;

    property Bounds: TRect read FBounds write FBounds;
    property Containers: TdxSpreadSheetCellViewInfoList read FContainers;
    property IsDirty: Boolean read FIsDirty write SetIsDirty;
    property Painter: TdxSpreadSheetCustomViewPainter read FPainter;
  end;

  { TdxSpreadSheetViewList }

  TdxSpreadSheetViewList = class(TcxObjectList)
  strict private
    function GetItem(Index: Integer): TdxSpreadSheetCustomView; inline;
  public
    procedure AddChanges(AChanges: TdxSpreadSheetChanges);
    procedure CheckChanges;
    procedure DataChanged;
    procedure Loaded;

    property Items[Index: Integer]: TdxSpreadSheetCustomView read GetItem; default;
  end;

  { TdxSpreadSheetMergedCellsRowItem }

  TdxSpreadSheetMergedCellsRowItem = class(TcxDoublyLinkedObject)
  public
    Cell: TdxSpreadSheetMergedCell;
  end;

  { TdxSpreadSheetMergedCellsRow }

  TdxSpreadSheetMergedCellListEnumCellsProc = reference to procedure (ACell: TdxSpreadSheetMergedCell);

  TdxSpreadSheetMergedCellsRow = class(TcxDoublyLinkedObjectList)
  protected
    function CreateLinkedObject: TcxDoublyLinkedObject; override;
  public
    procedure AddCell(ACell: TdxSpreadSheetMergedCell);
    function Contains(AColumn: Integer): TdxSpreadSheetMergedCell; inline;
    procedure Enum(AColumn1, AColumn2: Integer; AProc: TdxSpreadSheetMergedCellListEnumCellsProc); inline;
    procedure RemoveCell(ACell: TdxSpreadSheetMergedCell);
    procedure Union(var AArea: TRect);
  end;

  { TdxSpreadSheetMergedCell }

  TdxSpreadSheetMergedCell = class(TcxDoublyLinkedObject)
  strict private
    FArea: TRect;
    FOwner: TdxSpreadSheetMergedCellList;

    function GetActiveCell: TdxSpreadSheetCell;
    function GetHistory: TdxSpreadSheetHistory; inline;
    function GetNext: TdxSpreadSheetMergedCell; inline;
    function GetPrev: TdxSpreadSheetMergedCell; inline;
    function GetView: TdxSpreadSheetTableView;
    procedure SetNext(const Value: TdxSpreadSheetMergedCell); inline;
    procedure SetPrev(const Value: TdxSpreadSheetMergedCell); inline;
  protected
    procedure Initialize(AOwner: TdxSpreadSheetMergedCellList; const AArea: TRect);

    property History: TdxSpreadSheetHistory read GetHistory;
  public
    destructor Destroy; override;
    function Contains(const ARow, AColumn: Integer): Boolean;
    function Intersects(const AArea: TRect): Boolean;

    property ActiveCell: TdxSpreadSheetCell read GetActiveCell;
    property Area: TRect read FArea;
    property Next: TdxSpreadSheetMergedCell read GetNext write SetNext;
    property Owner: TdxSpreadSheetMergedCellList read FOwner;
    property Prev: TdxSpreadSheetMergedCell read GetPrev write SetPrev;
    property View: TdxSpreadSheetTableView read GetView;
  end;

  { TdxSpreadSheetClipboardArea }

  TdxSpreadSheetClipboardArea = class
  public
    Area: TRect;
    View: TdxSpreadSheetCustomView;

    procedure Initialize(AView: TdxSpreadSheetCustomView; const AArea: TRect);
    function IsEmpty: Boolean;
    procedure Reset;
  end;

  { TdxSpreadSheetMergedCellList }

  TdxSpreadSheetMergedCellListEnumRowsProc = reference to procedure (ARow: TdxSpreadSheetMergedCellsRow);

  TdxSpreadSheetMergedCellList = class(TcxDoublyLinkedObjectList)
  strict private
    FCount: Integer;
    FIsDeleting: Boolean;
    FView: TdxSpreadSheetTableView;

    function GetFirst: TdxSpreadSheetMergedCell; inline;
    function GetHistory: TdxSpreadSheetHistory; inline;
    function GetItem(AIndex: Integer): TdxSpreadSheetMergedCell;
    function GetLast: TdxSpreadSheetMergedCell; inline;
  protected
    CachedCell: TdxSpreadSheetMergedCell;
    CachedExpandedArea: TRect;
    CachedPoint: TPoint;
    CachedSourceArea: TRect;
    Rows: TObjectDictionary<Integer, TdxSpreadSheetMergedCellsRow>;

    procedure Changed; inline;
    procedure ClearCacheInformation; inline;
    function CreateLinkedObject: TcxDoublyLinkedObject; override;
    procedure DoRemoving(ACell: TdxSpreadSheetMergedCell);
    function Find(ACell: TdxSpreadSheetMergedCell): Integer;
    procedure InitializeCell(ACell: TdxSpreadSheetMergedCell; const AArea: TRect);
    //
    procedure AddRef(ACell: TdxSpreadSheetMergedCell);
    procedure RemoveRef(ACell: TdxSpreadSheetMergedCell);

    property History: TdxSpreadSheetHistory read GetHistory;
  public
    constructor Create(AView: TdxSpreadSheetTableView); virtual;
    destructor Destroy; override;
    procedure Add(const AArea: TRect); reintroduce;
    function CheckCell(ARow, AColumn: Integer): TRect;
    procedure Clear; override;
    procedure Delete(ALinkedObject: TcxDoublyLinkedObject); override;
    procedure DeleteItemsInArea(const AArea: TRect; AEnclosedItemsOnly: Boolean = True);
    procedure EnumCells(AProc: TdxSpreadSheetMergedCellListEnumCellsProc); overload;
    procedure EnumCells(const AArea: TRect; AProc: TdxSpreadSheetMergedCellListEnumCellsProc); overload;
    procedure EnumRows(AStartIndex, AFinishIndex: Integer; AProc: TdxSpreadSheetMergedCellListEnumRowsProc);
    function ExpandArea(const AArea: TRect): TRect; overload;
    function ExpandArea(const AColumn, ARow: Integer): TRect; overload;
    procedure Extract(const AArea: TRect; ADest: TList<TdxSpreadSheetMergedCell>); reintroduce; overload;
    function FindCell(ARow, AColumn: Integer): TdxSpreadSheetMergedCell;
    function HasItemsInArea(const AArea: TRect): Boolean;
    //
    property Count: Integer read FCount;
    property First: TdxSpreadSheetMergedCell read GetFirst;
    property Items[Index: Integer]: TdxSpreadSheetMergedCell read GetItem; default;
    property Last: TdxSpreadSheetMergedCell read GetLast;
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetTableItem }

  TdxSpreadSheetTableItemEnumProc = reference to procedure (ACell: TdxSpreadSheetCell);

  TdxSpreadSheetTableItem = class(TdxDynamicListItem, IdxSpreadSheetCellStyleOwner)
  strict private
    FStyle: TdxSpreadSheetCellStyle;
    FVisible: Boolean;

    function GetHistory: TdxSpreadSheetHistory;
    function GetNext: TdxSpreadSheetTableItem;
    function GetOwner: TdxSpreadSheetTableItems;
    function GetPrev: TdxSpreadSheetTableItem;
    function GetSize: Integer;
    function GetView: TdxSpreadSheetTableView; inline;
    procedure SetDefaultSize(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
  protected
    FDefaultSize: Boolean;
    FIsCustomSize: Boolean;
    FSize: Integer;

    procedure AfterResize;
    procedure BeforeResize;
    function CalculateBestFit: Integer; virtual;
    procedure Changed;
    function GetCell(AIndex: Integer): TdxSpreadSheetCell; virtual; abstract;
    function GetCellCount: Integer; virtual; abstract;
    function GetDisplayText: string; virtual;
    function IsValueDefined(AIndex: Integer): Boolean; inline;
    procedure MarkBestFitDirty; virtual;
    function MeasureCellSize(ACanvas: TcxCanvas; ACell: TdxSpreadSheetCell): Integer; virtual; abstract;
    procedure SetSize(const AValue: Integer); virtual;
    procedure SetSizeEx(const ASize: TSize); virtual;

    // IdxSpreadSheetCellStyleOwner
    procedure CellStyleChanged;
    procedure CellStyleChanging;
    function GetCellStyles: TdxSpreadSheetCellStyles;
    function GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
    procedure ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);

    property History: TdxSpreadSheetHistory read GetHistory;
    property IsCustomSize: Boolean read FIsCustomSize write FIsCustomSize;
  public
    constructor Create(AOwner: TdxDynamicItemList; AIndex: Integer); override;
    destructor Destroy; override;
    procedure Assign(ASource: TdxDynamicListItem); override;
    procedure ApplyBestFit; virtual;
    procedure BeforeDestruction; override;
    function CreateCell(AIndex: Integer): TdxSpreadSheetCell; virtual; abstract;
    procedure EnumCells(AEnumProc: TdxSpreadSheetTableItemEnumProc); virtual;

    property CellCount: Integer read GetCellCount;
    property Cells[Index: Integer]: TdxSpreadSheetCell read GetCell;
    property CustomSize: Integer read FSize;
    property DefaultSize: Boolean read FDefaultSize write SetDefaultSize;
    property DisplayText: string read GetDisplayText;
    property Next: TdxSpreadSheetTableItem read GetNext;
    property Owner: TdxSpreadSheetTableItems read GetOwner;
    property Prev: TdxSpreadSheetTableItem read GetPrev;
    property Size: Integer read GetSize write SetSize;
    property Style: TdxSpreadSheetCellStyle read FStyle;
    property View: TdxSpreadSheetTableView read GetView;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  { TdxSpreadSheetTableItemGroup }

  TdxSpreadSheetTableItemGroupExpandButtonPosition = (gebpGroupStart, gebpGroupFinish);

  TdxSpreadSheetTableItemGroup = class
  strict private
    FFinishIndex: Integer;
    FHasChanges: Boolean;
    FLockCount: Integer;
    FOwner: TdxSpreadSheetTableItems;
    FParent: TdxSpreadSheetTableItemGroup;
    FStartIndex: Integer;

    function GetCount: Integer;
    function GetExpandButtonPosition: TdxSpreadSheetTableItemGroupExpandButtonPosition;
    function GetExpanded: Boolean;
    function GetHistory: TdxSpreadSheetHistory;
    function GetIndex: Integer;
    function GetItem(Index: Integer): TdxSpreadSheetTableItemGroup;
    function GetLevel: Integer;
    function GetMaxNestingLevel: Integer;
    procedure SetExpanded(AValue: Boolean);
    procedure SetFinishIndex(AValue: Integer);
    procedure SetStartIndex(AValue: Integer);
    procedure ValidateIndex(var AIndex: Integer);
  protected
    FChildren: TdxSpreadSheetTableItemGroupList;
    FCollapsedByUser: Boolean;

    procedure AddChild(AStartIndex, AFinishIndex: Integer);
    procedure Changed;
    procedure CheckRange;
    procedure DoChanged;
    procedure Exclude(AIndex: Integer);
    function HasItemsOnThisLevel: Boolean; virtual;
    function IsCollapsedByUser: Boolean;
    function IsParentExpanded: Boolean;
    function IsRoot: Boolean;
    procedure SetParent(AParent: TdxSpreadSheetTableItemGroup);
    //
    property ExpandButtonPosition: TdxSpreadSheetTableItemGroupExpandButtonPosition read GetExpandButtonPosition;
    property History: TdxSpreadSheetHistory read GetHistory;
    property MaxNestingLevel: Integer read GetMaxNestingLevel;
  public
    constructor Create(AOwner: TdxSpreadSheetTableItems; AParent: TdxSpreadSheetTableItemGroup; AStartIndex, AFinishIndex: Integer);
    destructor Destroy; override;
    //
    procedure BeginUpdate;
    procedure EndUpdate;
    //
    procedure DeleteAll;
    function Find(AIndex: Integer; ARecursive: Boolean = True): TdxSpreadSheetTableItemGroup;
    procedure ToggleExpanded;
    //
    property Count: Integer read GetCount;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property FinishIndex: Integer read FFinishIndex write SetFinishIndex;
    property Index: Integer read GetIndex;
    property Items[Index: Integer]: TdxSpreadSheetTableItemGroup read GetItem; default;
    property Level: Integer read GetLevel;
    property Owner: TdxSpreadSheetTableItems read FOwner;
    property Parent: TdxSpreadSheetTableItemGroup read FParent;
    property StartIndex: Integer read FStartIndex write SetStartIndex;
  end;

  { TdxSpreadSheetTableItemGroupComparer }

  TdxSpreadSheetTableItemGroupComparer = class(TInterfacedObject, IComparer<TdxSpreadSheetTableItemGroup>)
  public
    // IComparer<TdxSpreadSheetTableItemGroup>
    function Compare(const Left, Right: TdxSpreadSheetTableItemGroup): Integer;
  end;

  { TdxSpreadSheetTableItemGroupList }

  TdxSpreadSheetTableItemGroupList = class(TList<TdxSpreadSheetTableItemGroup>)
  strict private
    FOwner: TdxSpreadSheetTableItemGroup;
    FLockCount: Integer;
  protected
    procedure Notify(const Item: TdxSpreadSheetTableItemGroup; Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TdxSpreadSheetTableItemGroup);
    procedure BeginUpdate;
    procedure EndUpdate;

    function Find(AIndex: Integer; ARecursive: Boolean = True): TdxSpreadSheetTableItemGroup;
    function GetRange(out AStartIndex, AFinishIndex: Integer): Boolean;
    procedure Validate(ARecursive: Boolean = False);
    procedure ValidateRanges(AStartIndex, AFinishIndex: Integer);
  end;

  { TdxSpreadSheetTableItemGroups }

  TdxSpreadSheetTableItemGroups = class
  strict private
    FExpandButtonPosition: TdxSpreadSheetTableItemGroupExpandButtonPosition;
    FRoot: TdxSpreadSheetTableItemGroup;

    function GetCount: Integer;
    function GetItem(Index: Integer): TdxSpreadSheetTableItemGroup;
    procedure SetExpandButtonPosition(AValue: TdxSpreadSheetTableItemGroupExpandButtonPosition);
  protected
    procedure Validate;
    //
    property Root: TdxSpreadSheetTableItemGroup read FRoot;
  public
    constructor Create(AOwner: TdxSpreadSheetTableItems);
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Add(AIndex: Integer); overload;
    procedure Add(AStartIndex, AFinishIndex: Integer); overload;
    procedure Delete(AStartIndex, AFinishIndex: Integer);
    procedure DeleteAll;
    procedure ExpandToLevel(ALevel: Integer);
    function Find(AIndex: Integer; ARecursive: Boolean = True): TdxSpreadSheetTableItemGroup;

    property Count: Integer read GetCount;
    property ExpandButtonPosition: TdxSpreadSheetTableItemGroupExpandButtonPosition read FExpandButtonPosition write SetExpandButtonPosition default gebpGroupFinish;
    property Items[Index: Integer]: TdxSpreadSheetTableItemGroup read GetItem; default;
  end;

  { TdxSpreadSheetTableItems }

  TdxSpreadSheetTableItems = class(TdxDynamicItemList)
  strict private
    FGroups: TdxSpreadSheetTableItemGroups;
    FPrevIndex: Integer;
    FPrevSize: Integer;
    FView: TdxSpreadSheetTableView;

    function GetFirst: TdxSpreadSheetTableItem;
    function GetItem(AIndex: Integer): TdxSpreadSheetTableItem; inline;
    function GetLast: TdxSpreadSheetTableItem;
    function GetSpreadSheet: TdxCustomSpreadSheet;
    procedure SetItem(AIndex: Integer; const AValue: TdxSpreadSheetTableItem); {$IFNDEF VER220} inline; {$ENDIF}
  protected
    procedure AfterResize; virtual;
    procedure BeforeResize; virtual;
    procedure Changed; virtual;
    function GetDefaultSize: Integer; virtual; abstract;
    function GetFixedItemIndex: Integer; virtual; abstract;
    function GetItemDisplayText(const AItem: TdxSpreadSheetTableItem): string; virtual;
    function GetItemSize(AIndex: Integer): Integer; inline;
    function GetItemState(AIndex: Integer): TcxButtonState; virtual;
    function GetItemStyle(AIndex: Integer): TdxSpreadSheetCellStyle; inline;
    function GetItemStyleHandle(AIndex: Integer): TdxSpreadSheetCellStyleHandle; inline;
    function GetItemText(AIndex: Integer): string; virtual;
    function GetItemVisible(AIndex: Integer): Boolean;
    function GetMaxItemIndex: Integer; virtual;
    function GetNextVisibleItemIndex(AIndex: Integer; AGoForward: Boolean; out AResult: Integer): Boolean; overload;
    function GetOppositeItems: TdxSpreadSheetTableItems; virtual; abstract;
    function GetRealItemSize(AIndex: Integer): Integer; inline;
    procedure ResizeItem(AIndex, ADelta: Integer); virtual;
    procedure SetDefaultSize(const AValue: Integer); virtual; abstract;

    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  public
    constructor Create(AOwner: TdxSpreadSheetTableView); virtual;
    destructor Destroy; override;

    function CreateItem(const AIndex: Integer): TdxSpreadSheetTableItem;
    function GetDistance(AStartIndex, AFinishIndex: Integer): Integer;
    function GetItemIndexFromDistance(AStartIndex: Integer; ADistance: Integer): Integer;
    function GetNextVisibleItemIndex(AIndex: Integer; AGoForward: Boolean): Integer; overload;
    function GetPosition(AIndex: Integer): Integer;

    property DefaultSize: Integer read GetDefaultSize write SetDefaultSize;
    property First: TdxSpreadSheetTableItem read GetFirst;
    property Groups: TdxSpreadSheetTableItemGroups read FGroups;
    property Items[Index: Integer]: TdxSpreadSheetTableItem read GetItem write SetItem; default;
    property Last: TdxSpreadSheetTableItem read GetLast;
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetTableItemStyle }

  TdxSpreadSheetTableItemStyle = class(TdxSpreadSheetCellStyle);

  { TdxSpreadSheetTableColumn }

  TdxSpreadSheetTableColumn = class(TdxSpreadSheetTableItem)
  protected
    function GetCell(ARow: Integer): TdxSpreadSheetCell; override;
    function GetCellCount: Integer; override;
    function MeasureCellSize(ACanvas: TcxCanvas; ACell: TdxSpreadSheetCell): Integer; override;
    procedure ShiftIndex(ADelta: Integer); override;
  public
    function CreateCell(ARow: Integer): TdxSpreadSheetCell; override;
  end;

  { TdxSpreadSheetTableColumns }

  TdxSpreadSheetTableColumns = class(TdxSpreadSheetTableItems)
  strict private
    function GetItem(AIndex: Integer): TdxSpreadSheetTableColumn;
  protected
    function GetDefaultSize: Integer; override;
    function GetFixedItemIndex: Integer; override;
    function GetItemClass: TdxDynamicListItemClass; override;
    function GetItemText(AIndex: Integer): string; override;
    function GetItemState(AIndex: Integer): TcxButtonState; override;
    function GetMaxItemIndex: Integer; override;
    function GetOppositeItems: TdxSpreadSheetTableItems; override;
    procedure SetDefaultSize(const AValue: Integer); override;
  public
    function CreateItem(const AIndex: Integer): TdxSpreadSheetTableColumn; overload;
    function CreateItem(const AName: string): TdxSpreadSheetTableColumn; overload;
    //
    property Items[Index: Integer]: TdxSpreadSheetTableColumn read GetItem; default;
  end;

  { TdxSpreadSheetTableRowCells }

  TdxSpreadSheetTableRowCells = class(TdxDynamicItemList)
  strict private
    FRow: TdxSpreadSheetTableRow;
  protected
    procedure DoItemCreated(AItem: TdxDynamicListItem); override;
    function GetItemClass: TdxDynamicListItemClass; override;
  public
    constructor Create(ARow: TdxSpreadSheetTableRow);

    property First: TdxDynamicListItem read FFirst;
    property Last: TdxDynamicListItem read FLast;
    property Row: TdxSpreadSheetTableRow read FRow write FRow;
  end;

  { TdxSpreadSheetTableRow }

  TdxSpreadSheetTableRow = class(TdxSpreadSheetTableItem)
  strict private
    FCells: TdxSpreadSheetTableRowCells;
  protected
    FBestFitIsDirty: Boolean;

    function CalculateBestFit: Integer; override;
    function CreateRowCells: TdxSpreadSheetTableRowCells; virtual;
    procedure DoCellCreated(ACell: TdxSpreadSheetCell); virtual;
    function GetCell(AColumn: Integer): TdxSpreadSheetCell; override;
    function GetCellCount: Integer; override;
    procedure MarkBestFitDirty; override;
    function MeasureCellSize(ACanvas: TcxCanvas; ACell: TdxSpreadSheetCell): Integer; override;
    procedure SetSizeEx(const ASize: TSize); override;

    property RowCells: TdxSpreadSheetTableRowCells read FCells;
  public
    constructor Create(AOwner: TdxDynamicItemList; AIndex: Integer); override;
    destructor Destroy; override;
    procedure ApplyBestFit; override;
    function CreateCell(AColumn: Integer): TdxSpreadSheetCell; override;
    procedure EnumCells(AEnumProc: TdxSpreadSheetTableItemEnumProc); override;
  end;

  { TdxSpreadSheetTableRows }

  TdxSpreadSheetTableRows = class(TdxSpreadSheetTableItems)
  strict private
    function GetItem(AIndex: Integer): TdxSpreadSheetTableRow; inline;
  protected
    procedure CheckBestFit;
    procedure SetBestFitDirty;

    procedure ForEachCell(AArea: TRect; AProc: TdxDynamicItemListForEachProcRef; AGoForward: Boolean = True);
    function GetDefaultSize: Integer; override;
    function GetFixedItemIndex: Integer; override;
    function GetItemClass: TdxDynamicListItemClass; override;
    function GetItemState(AIndex: Integer): TcxButtonState; override;
    function GetMaxItemIndex: Integer; override;
    function GetOppositeItems: TdxSpreadSheetTableItems; override;
    procedure SetDefaultSize(const AValue: Integer); override;
  public
    function CreateItem(const AIndex: Integer): TdxSpreadSheetTableRow;
    //
    property Items[Index: Integer]: TdxSpreadSheetTableRow read GetItem; default;
  end;

  { TdxSpreadSheetTableViewOptionsPrint }

  TdxSpreadSheetTableViewOptionsPrint = class(TdxSpreadSheetCustomViewOptions)
  strict private
    FHeaderFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooter;
    FPage: TdxSpreadSheetTableViewOptionsPrintPage;
    FPagination: TdxSpreadSheetTableViewOptionsPrintPagination;
    FPrinting: TdxSpreadSheetTableViewOptionsPrintPrinting;
    FSource: TdxSpreadSheetTableViewOptionsPrintSource;

    procedure ChangeHandler(Sender: TObject);
    procedure SetHeaderFooter(AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooter);
    procedure SetPage(AValue: TdxSpreadSheetTableViewOptionsPrintPage);
    procedure SetPagination(AValue: TdxSpreadSheetTableViewOptionsPrintPagination);
    procedure SetPrinting(AValue: TdxSpreadSheetTableViewOptionsPrintPrinting);
    procedure SetSource(AValue: TdxSpreadSheetTableViewOptionsPrintSource);
  protected
    procedure DoAssign(ASource: TPersistent); override;
    procedure DoChanged; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Reset;
  published
    property HeaderFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooter read FHeaderFooter write SetHeaderFooter;
    property Page: TdxSpreadSheetTableViewOptionsPrintPage read FPage write SetPage;
    property Pagination: TdxSpreadSheetTableViewOptionsPrintPagination read FPagination write SetPagination;
    property Printing: TdxSpreadSheetTableViewOptionsPrintPrinting read FPrinting write SetPrinting;
    property Source: TdxSpreadSheetTableViewOptionsPrintSource read FSource write SetSource;
  end;

  { TdxSpreadSheetTableViewOptions }

  TdxSpreadSheetTableViewOptions = class(TdxSpreadSheetCustomViewOptions)
  strict private
    FDefaultColumnWidth: Integer;
    FDefaultRowHeight: Integer;
    FGridLines: TdxDefaultBoolean;
    FHeaders: TdxDefaultBoolean;
    FHorizontalScrollBar: TdxDefaultBoolean;
    FShowFormulas: TdxDefaultBoolean;
    FVerticalScrollBar: TdxDefaultBoolean;
    FZeroValues: TdxDefaultBoolean;
    FZoomFactor: Integer;

    function GetActualGridLines: Boolean;
    function GetActualHeaders: Boolean;
    function GetActualHorizontalScrollBar: Boolean;
    function GetActualShowFormulas: Boolean;
    function GetActualVerticalScrollBar: Boolean;
    function GetActualZeroValues: Boolean;
    function GetOptionsView: TdxSpreadSheetOptionsView; inline;
    function GetProtected: Boolean;
    function GetView: TdxSpreadSheetTableView; inline;
    procedure SetDefaultColumnWidth(AValue: Integer);
    procedure SetDefaultRowHeight(AValue: Integer);
    procedure SetGridLines(AValue: TdxDefaultBoolean);
    procedure SetHeaders(AValue: TdxDefaultBoolean);
    procedure SetHorizontalScrollBar(AValue: TdxDefaultBoolean);
    procedure SetProtected(AValue: Boolean);
    procedure SetShowFormulas(AValue: TdxDefaultBoolean);
    procedure SetVerticalScrollBar(AValue: TdxDefaultBoolean);
    procedure SetZeroValues(AValue: TdxDefaultBoolean);
    procedure SetZoomFactor(AValue: Integer);
  protected
    procedure ChangeScale(M, D: Integer); override;
    //
    property OptionsView: TdxSpreadSheetOptionsView read GetOptionsView;
    property View: TdxSpreadSheetTableView read GetView;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property ActualGridLines: Boolean read GetActualGridLines;
    property ActualHeaders: Boolean read GetActualHeaders;
    property ActualHorizontalScrollBar: Boolean read GetActualHorizontalScrollBar;
    property ActualShowFormulas: Boolean read GetActualShowFormulas;
    property ActualVerticalScrollBar: Boolean read GetActualVerticalScrollBar;
    property ActualZeroValues: Boolean read GetActualZeroValues;
  published
    property DefaultColumnWidth: Integer read FDefaultColumnWidth write SetDefaultColumnWidth;
    property DefaultRowHeight: Integer read FDefaultRowHeight write SetDefaultRowHeight;
    property GridLines: TdxDefaultBoolean read FGridLines write SetGridLines default bDefault;
    property Headers: TdxDefaultBoolean read FHeaders write SetHeaders default bDefault;
    property HorizontalScrollBar: TdxDefaultBoolean read FHorizontalScrollBar write SetHorizontalScrollBar default bDefault;
    property Protected: Boolean read GetProtected write SetProtected stored False; // obsolete
    property ShowFormulas: TdxDefaultBoolean read FShowFormulas write SetShowFormulas default bDefault;
    property VerticalScrollBar: TdxDefaultBoolean read FVerticalScrollBar write SetVerticalScrollBar default bDefault;
    property ZeroValues: TdxDefaultBoolean read FZeroValues write SetZeroValues default bDefault;
    property ZoomFactor: Integer read FZoomFactor write SetZoomFactor default dxSpreadSheetDefaultZoomFactor;
  end;

  { TdxSpreadSheetTableViewHitTest }

  TdxSpreadSheetTableViewHitTest = class(TdxSpreadSheetCustomHitTest)
  strict private
    function GetContainer: TdxSpreadSheetContainer;
    function GetView: TdxSpreadSheetTableView; inline;
  protected
    function GetActualHitPoint: TPoint; override;
  public
    procedure Calculate(const AHitPoint: TPoint); override;

    property Container: TdxSpreadSheetContainer read GetContainer;
    property HitAtBackground: Boolean index hcBackground read GetHitCode;
    property HitAtCell: Boolean index hcCell read GetHitCode;
    property HitAtColumnHeader: Boolean index hcColumnHeader read GetHitCode;
    property HitAtContainer: Boolean index hcContainer read GetHitCode;
    property HitAtContainerSelection: Boolean index hcContainerSelection read GetHitCode;
    property HitAtExpandButton: Boolean index hcExpandButton read GetHitCode;
    property HitAtFrozenPaneSeparator: Boolean index hcFrozenPaneSeparator read GetHitCode;
    property HitAtGroup: Boolean index hcGroup read GetHitCode;
    property HitAtGroupingArea: Boolean index hcGroupingArea read GetHitCode;
    property HitAtHyperlink: Boolean index hcHyperlink read GetHitCode;
    property HitAtResizeArea: Boolean index hcResizeArea read GetHitCode;
    property HitAtRowHeader: Boolean index hcRowHeader read GetHitCode;
    property HitAtSelectionFrame: Boolean index hcSelectionFrame read GetHitCode;
    property View: TdxSpreadSheetTableView read GetView;
  end;

  { TdxSpreadSheetTableViewSelection }

  TdxSpreadSheetTableViewSelection = class
  strict private
    FFocusedColumn: Integer;
    FFocusedRow: Integer;
    FItems: TObjectList<TcxRect>;
    FLockCount: Integer;
    FView: TdxSpreadSheetTableView;

    function GetArea: TRect;
    function GetCount: Integer;
    function GetFocusedCell: TdxSpreadSheetCell;
    function GetFocusedContainer: TdxSpreadSheetContainer;
    function GetItem(AIndex: Integer): TcxRect;
    function GetMergedCells: TdxSpreadSheetMergedCellList;
    function GetSelectionByColumns: Boolean;
    function GetSelectionByRows: Boolean;
    procedure SetFocusedCell(AValue: TdxSpreadSheetCell);
    procedure SetFocusedColumn(AValue: Integer);
    procedure SetFocusedContainer(const Value: TdxSpreadSheetContainer);
    procedure SetFocusedRow(AValue: Integer);
  protected
    function CanSelectArea(const AArea: TRect): Boolean;
    procedure Changed; virtual;
    procedure CheckClear(const AShift: TShiftState);
    function GetActiveSelection: TRect;
    procedure GetState(ARow, AColumn: Integer; var AFocused, ASelected: Boolean);
    function IsAreaMoreThanOneCell: Boolean;
    function IsMergedCells(const AArea: TRect): Boolean;
    procedure ItemChanged(Sender: TObject); virtual;
    procedure LoadFromStream(AReader: TcxReader); virtual;
    procedure SaveToStream(AWriter: TcxWriter); virtual;
    procedure Validate;

    property MergedCells: TdxSpreadSheetMergedCellList read GetMergedCells;
    property SelectionByColumns: Boolean read GetSelectionByColumns;
    property SelectionByRows: Boolean read GetSelectionByRows;
  public
    constructor Create(AOwner: TdxSpreadSheetTableView); virtual;
    destructor Destroy; override;
    procedure Add(AArea: TRect; AShift: TShiftState = []; AFocusedRow: Integer = -1; AFocusedColumn: Integer = -1);
    procedure Clear;

    procedure BeginUpdate;
    procedure EndUpdate;

    function HasArea(const AArea: TRect): Boolean;
    function IsCellSelected(ARow, AColumn: Integer): Boolean;
    function IsColumnSelected(AColumn: Integer): Boolean;
    function IsRowSelected(ARow: Integer): Boolean;
    function IsEntireColumnSelected(AColumn: Integer): Boolean;
    function IsEntireRowSelected(ARow: Integer): Boolean;
    procedure SelectAll;
    procedure SelectCell(ARow, AColumn: Integer; AShift: TShiftState = []);
    procedure SelectColumns(AStartColumn, AFinishColumn: Integer; AShift: TShiftState = []);
    procedure SelectRows(AStartRow, AFinishRow: Integer; AShift: TShiftState = []);
    procedure SetFocused(ARow, AColumn: Integer; AShift: TShiftState);

    property Area: TRect read GetArea;
    property Count: Integer read GetCount;
    property FocusedCell: TdxSpreadSheetCell read GetFocusedCell write SetFocusedCell;
    property FocusedColumn: Integer read FFocusedColumn write SetFocusedColumn;
    property FocusedContainer: TdxSpreadSheetContainer read GetFocusedContainer write SetFocusedContainer;
    property FocusedRow: Integer read FFocusedRow write SetFocusedRow;
    property Items[Index: Integer]: TcxRect read GetItem; default;
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetInplaceEdit }

  TdxSpreadSheetInplaceEdit = class(TdxSpreadSheetCustomInplaceEdit)
  protected
    function GetController: IdxSpreadSheetInplaceEditController; override;
  end;

  { TdxSpreadSheetTableViewEditingController }

  TdxSpreadSheetTableViewEditingController = class(TcxCustomEditingController,
    IdxSpreadSheetInplaceEditController)
  strict private
    FAlignment: TAlignment;
    FCanUseCurrentValue: Boolean;
    FDefaultEditProperties: TcxCustomMemoProperties;
    FEditData: TcxCustomEditData;
    FEditing: Boolean;
    FEditStyle: TcxEditStyle;
    FEditStyleFont: TFont;
    FEditValue: string;
    FIsArrayFormula: Boolean;
    FIsEditHiding: Boolean;
    FOwner: TdxSpreadSheetTableViewController;
    FPrevIsEditValueFormula: Boolean;
    FPropertiesValue: TcxCustomEditProperties;
    FRedoValue: Variant;
    FReferenceHighlighter: TdxSpreadSheetCustomReferenceHighlighter;
    FReplacementMode: Boolean;

    function GetCell: TdxSpreadSheetCell;
    function GetCellPosition: TPoint;
    function GetCellViewInfo: TdxSpreadSheetTableViewCellViewInfo;
    function GetDefaultEdit: TcxCustomMemo;
    function GetEditSize(const AText: string; ASizeProperties: TcxEditSizeProperties): TSize;
    function GetEditText: string;
    function GetSpreadSheet: TdxCustomSpreadSheet;
    function GetView: TdxSpreadSheetTableView; inline;
  protected
    procedure AssignEditStyle; virtual;

    function CanInitEditing: Boolean; override;
    function CanInsertNewAreaViaMouse: Boolean; virtual;
    function CanRedo: Boolean; virtual;
    function CanUndo: Boolean; virtual;
    function CanUpdateEditValue: Boolean; override;
    function CanUpdateMultilineEditHeight: Boolean;

    function CreateDefaultEdit: TcxCustomMemo; virtual;
    function CreateDefaultEditProperties: TcxCustomMemoProperties; virtual;
    function CreateEditStyle: TcxEditStyle; virtual;
    function CreateReferenceHighlighter: TdxSpreadSheetCustomReferenceHighlighter; virtual;

    function IsEditValueFormula: Boolean;

    procedure ClearEditingItem; override;
    procedure DoHideEdit(Accept: Boolean); override;
    procedure DoUpdateEdit; override;
    procedure EditActivated;
    procedure EditChanged(Sender: TObject); override;
    procedure EditExit(Sender: TObject); override;
    procedure EditFocusChanged(Sender: TObject); override;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure EditKeyPress(Sender: TObject; var Key: Char); override;
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    function GetAdjustedEditBoundsHorizontally(const AArea, ATextRect: TRect): TRect;
    function GetAdjustedEditBoundsVertically(const AArea, ATextRect: TRect): TRect;
    function GetAdjustedMultilineEditBounds: TRect;
    function GetAreaBounds(const AArea: TRect): TRect;
    function GetCancelEditingOnExit: Boolean; override;
    function GetCaretPosByMousePos: TPoint;
    function GetEditClipArea: TRect;
    function GetEditParent: TWinControl; override;
    function GetFocusedCellBounds: TRect; override;
    function GetHideEditOnFocusedRecordChange: Boolean; override;
    function GetIsEditing: Boolean; override;
    function GetValue: TcxEditValue; override;
    function GetValueFromCell(ACell: TdxSpreadSheetCell; AAllowRTF: Boolean = True): TcxEditValue;
    procedure HistoryChanged;
    procedure MultilineEditTextChanged; override;
    procedure SetValue(const AValue: TcxEditValue); override;
    procedure ShowEditWithTopCellValue(const ACurrentRow, ACurrentColumn: Integer; const AUseFormula: Boolean);
    procedure StartEditingByTimer; override;
    procedure UninitEdit; override;
    procedure UpdateEditPosition;
    procedure UpdateInplaceParamsPosition; override;
    procedure UpdateReferencesHighlighting;
    //
    function PrepareEdit(AIsMouseEvent: Boolean): Boolean;
    procedure ReplaceEditValue(const AValue: Variant);
    //
    procedure Redo; virtual;
    procedure Undo; virtual;
    // IdxSpreadSheetInplaceEditController
    function IsAutoCompleteAllowed: Boolean;
    function IsAutoCompleteSuggestionsHintsAllowed: Boolean;
    procedure KeyDown(var Key: Word; ShiftState: TShiftState); overload;
    procedure Post(AIsArrayFormula: Boolean = False);

    property DefaultEdit: TcxCustomMemo read GetDefaultEdit;
    property DefaultEditProperties: TcxCustomMemoProperties read FDefaultEditProperties;
    property EditStyle: TcxEditStyle read FEditStyle;
    property EditStyleFont: TFont read FEditStyleFont;
    property EditText: string read GetEditText;
    property EditValue: string read FEditValue write FEditValue;
    property IsArrayFormula: Boolean read FIsArrayFormula write FIsArrayFormula;
    property PropertiesValue: TcxCustomEditProperties read FPropertiesValue;
    property RedoValue: Variant read FRedoValue write FRedoValue;
    property ReferenceHighlighter: TdxSpreadSheetCustomReferenceHighlighter read FReferenceHighlighter;
    property ReplacementMode: Boolean read FReplacementMode write FReplacementMode;
  public
    constructor Create(AOwner: TdxSpreadSheetTableViewController); reintroduce; virtual;
    destructor Destroy; override;
    class function CanFinishEditingOnExit(AEdit: TcxCustomEdit): Boolean;
    class function GetActualInplaceEdit(ASheet: TdxSpreadSheetTableView; out AEdit: TdxSpreadSheetCustomInplaceEdit): Boolean;
    class function GetEditingText(AEdit: TcxCustomEdit): string;

    procedure HideEdit(Accept: Boolean); override;
    procedure ShowEdit; overload; override;
    procedure ShowEdit(const AValue: string); reintroduce; overload;
    procedure ShowEditByKey(AKey: Char);
    procedure ShowEditByMouse(X, Y: Integer; AShift: TShiftState);

    property Cell: TdxSpreadSheetCell read GetCell;
    property CellViewInfo: TdxSpreadSheetTableViewCellViewInfo read GetCellViewInfo;
    property Controller: TdxSpreadSheetTableViewController read FOwner;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
    property View: TdxSpreadSheetTableView read GetView;
  end;

  { TdxSpreadSheetTableViewCellHintController }

  TdxSpreadSheetTableViewCellHintController = class(TdxSpreadSheetViewPersistentObject)
  strict private
    FContainerToDisplayPrevBounds: TRect;
    FContainerToDisplayPrevZOrder: Integer;
    FDelayTimer: TcxTimer;
    FHintText: string;
    FHintToDisplay: TcxObjectLink;
    FHintVisible: Boolean;
    FLockHintCount: Integer;

    function GetHintHelper: TcxControlHintHelper;
    function GetView: TdxSpreadSheetTableView;
    procedure DelayTimerHandler(Sender: TObject);
    procedure ReleaseCellHint(ACell: TdxSpreadSheetCellViewInfo);
    procedure ReleaseContainer(AContainer: TdxSpreadSheetContainer);
    procedure ShowCellHint(ACell: TdxSpreadSheetCellViewInfo);
    procedure ShowContainer(AContainer: TdxSpreadSheetContainer);
  protected
    function GetComment(ACellViewInfo: TdxSpreadSheetCellViewInfo): TdxSpreadSheetContainer;
    function GetHyperlink(ACellViewInfo: TdxSpreadSheetCellViewInfo): TdxSpreadSheetHyperlink;
    function NeedShowHint(ACellViewInfo: TdxSpreadSheetCellViewInfo): Boolean;
    procedure DoReleaseHint;

    property HintHelper: TcxControlHintHelper read GetHintHelper;
    property HintText: string read FHintText write FHintText;
    property HintToDisplay: TcxObjectLink read FHintToDisplay;
    property HintVisible: Boolean read FHintVisible write FHintVisible;
    property View: TdxSpreadSheetTableView read GetView;
  public
    procedure BeforeDestruction; override;

    procedure LockHint;
    procedure UnlockHint;

    procedure Hide;
    procedure Show(ACellViewInfo: TdxSpreadSheetCellViewInfo);
  end;

  { TdxSpreadSheetHintHelper }

  TdxSpreadSheetHintHelper = class(TcxControlHintHelper)
  strict private
    FOwner: TdxSpreadSheetTableViewController;
  protected
    function GetOwnerControl: TcxControl; override;
  public
    constructor Create(AOwner: TdxSpreadSheetTableViewController); virtual;
  end;

  { TdxSpreadSheetTableViewController }

  TdxSpreadSheetTableViewController = class(TdxSpreadSheetCustomViewController)
  strict private
    FAnchorColumn: Integer;
    FAnchorRow: Integer;
    FCanClickOnTheCell: Boolean;
    FCellHintController: TdxSpreadSheetTableViewCellHintController;
    FClickTimer: TcxTimer;
    FEditingController: TdxSpreadSheetTableViewEditingController;
    FForcedSelectionMode: TdxSpreadSheetTableViewSelectionMode;
    FHintHelper: TdxSpreadSheetHintHelper;
    FScrollingTimer: TcxTimer;
    FSelectionMode: TdxSpreadSheetTableViewSelectionMode;

    function ExpandDataRegion(const AArea: TRect): TRect;
    function GetDataRegionAroundActiveCell: TRect;
    function IsColumnPartHasContent(const AColumn, AStartRow, AEndRow: Integer): Boolean;
    function IsRowPartHasContent(const ARow, AStartColumn, AEndColumn: Integer): Boolean;
    procedure SelectDataRegionAroundActiveCell;
    procedure TrySelectAll;

    function GetColumns: TdxSpreadSheetTableColumns;
    function GetFocusedColumn: Integer; inline;
    function GetFocusedHyperlink: TdxSpreadSheetHyperlink;
    function GetFocusedRow: Integer; inline;
    function GetFocusedRowOffsetFromTopRow(ARow: Integer): Integer;
    function GetMergedCells: TdxSpreadSheetMergedCellList;
    function GetOptionsBehavior: TdxSpreadSheetOptionsBehavior; inline;
    function GetRows: TdxSpreadSheetTableRows;
    function GetSelection: TdxSpreadSheetTableViewSelection; inline;
    function GetTableItemsForNavigation(ADirection: TcxArrowDirection): TdxSpreadSheetTableItems; inline;
    function GetView: TdxSpreadSheetTableView; inline;
    function GetViewHitTest: TdxSpreadSheetTableViewHitTest; inline;
    function GetViewInfo: TdxSpreadSheetTableViewInfo; inline;
    procedure SetSelectionModeField(AMode: TdxSpreadSheetTableViewSelectionMode);

    procedure SetForcedSelectionMode(const Value: TdxSpreadSheetTableViewSelectionMode);
  protected
    procedure ApplyBestFitForSelectedHeaderCells(ACells: TdxSpreadSheetCellViewInfoList);
    function CalculateNewFocusedRowIndex(AOffsetFromFirstRow: Integer): Integer;
    function CanExecuteHyperlink: Boolean; virtual;
    function CanFocusOnClick: Boolean; override;
    procedure CheckCtrlCommand(const AKey: Char); virtual;
    procedure CheckCtrlShiftCommand(const AKey: Word); virtual;
    procedure CheckScrollArea(const P: TPoint); override;
    procedure ClickTimerHandler(Sender: TObject); virtual;
    function ContainerCanDelete: Boolean; override;
    function ContextPopup(const P: TPoint): Boolean; override;
    function CreateEditingController: TdxSpreadSheetTableViewEditingController; virtual;
    procedure DblClick; override;

    function CanFocusMergedCellsPart(const AArea: TRect; ACell: TdxSpreadSheetCell; AGoForward: Boolean): Boolean;
    procedure FocusContainer(AContainer: TdxSpreadSheetContainer; AMakeVisible: Boolean); override;
    procedure FocusNextPossibleCell(ADirection: TcxArrowDirection; AUnlockedCellsOnly: Boolean);
    function GetNextColumn(AColumn: TdxSpreadSheetTableColumn; AGoForward: Boolean): TdxSpreadSheetTableColumn;
    function GetNextNonEmptyCell(AItems: TdxSpreadSheetTableItems; AFocusedItemIndex, AFocusedCellIndex: Integer; AGoForward: Boolean): Integer;
    function GetNextRow(ARow: TdxSpreadSheetTableRow; AGoForward: Boolean): TdxSpreadSheetTableRow;
    function GetNextUnlockedCellByColumn(const AArea: TRect; AStartRow, AStartColumn: Integer; AGoForward: Boolean): TdxSpreadSheetCell;
    function GetNextUnlockedCellByRow(const AArea: TRect; AStartRow, AStartColumn: Integer; AGoForward: Boolean): TdxSpreadSheetCell;
    function GetNextVisibleCell(AIndex: Integer; ADirection: TcxArrowDirection): Integer; overload;
    function GetNextVisibleCell(AIndex: Integer; ADirection: TcxArrowDirection; out AResult: Integer): Boolean; overload;
    function GetSearchAreaFirstColumn(const AArea: TRect): TdxSpreadSheetTableColumn;
    function GetSearchAreaFirstRow(const AArea: TRect): TdxSpreadSheetTableRow;
    function GetSearchAreaLastColumn(const AArea: TRect): TdxSpreadSheetTableColumn;
    function GetSearchAreaLastRow(const AArea: TRect): TdxSpreadSheetTableRow;
    procedure GetNextSelectedAreaCell(AIndex: Integer; AGoForward: Boolean; var ARow, AColumn: Integer);
    function GetNextSelectedAreaIndex(AIndex: Integer; AGoForward: Boolean): Integer;
    function GetSelectionAreaByFocusedCell(out ARect: TRect; out AIndex: Integer): Boolean;
    function GetSelectionItem(ASide: TcxBorder; AShift: TShiftState): Integer; inline;
    function FindUnlockedCellByColumn(const AStartArea: TRect; const AStartIndex, AStartRow, AStartColumn: Integer; AGoForward: Boolean): TdxSpreadSheetCell;
    function FindUnlockedCellByRow(const AStartArea: TRect; const AStartIndex, AStartRow, AStartColumn: Integer; AGoForward: Boolean): TdxSpreadSheetCell;

    procedure GetCellFromMouseCursor(var ARow, AColumn: Integer);
    function GetHitTest: TdxSpreadSheetCustomHitTest; override;
    procedure GetRedoActionCount(var ARedoCount: Integer); override;
    procedure GetUndoActionCount(var AUndoCount: Integer); override;

    procedure HideSelectedColumns;
    procedure HideSelectedRows;

    function DoActiveCellChanging(AColumn, ARow: Integer): Boolean; virtual;

    function ContainerProcessKeyDown(Key: Word; Shift: TShiftState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseDownAtCellArea(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUpAtGroupingArea(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; const P: TPoint): Boolean; override;

    function Redo(const ARedoCount: Integer = 1): Boolean; override;
    function Undo(const AUndoCount: Integer = 1): Boolean; override;

    function IMEComposition(var AMessage: TMessage): Boolean; override;
    function IMEStartComposition: Boolean; override;
    procedure StopEditing; override;
    procedure UpdateStates; override;

    procedure ScrollingTimerHandler(Sender: TObject); virtual;
    procedure SelectCellsWithComments;
    procedure SelectColumn(AColumn: Integer; AShift: TShiftState);
    procedure SelectRow(ARow: Integer; AShift: TShiftState);
    procedure SetFocused(ARow, AColumn: Integer; AShift: TShiftState);
    procedure SetSelectionMode(ARow, AColumn: Integer; AShift: TShiftState; AMode: TdxSpreadSheetTableViewSelectionMode);

    property CanClickOnTheCell: Boolean read FCanClickOnTheCell write FCanClickOnTheCell;
    property ClickTimer: TcxTimer read FClickTimer;
    property ScrollingTimer: TcxTimer read FScrollingTimer;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    property AnchorColumn: Integer read FAnchorColumn write FAnchorColumn;
    property AnchorRow: Integer read FAnchorRow write FAnchorRow;
    property CellHintController: TdxSpreadSheetTableViewCellHintController read FCellHintController;
    property Columns: TdxSpreadSheetTableColumns read GetColumns;
    property EditingController: TdxSpreadSheetTableViewEditingController read FEditingController;
    property FocusedColumn: Integer read GetFocusedColumn;
    property FocusedHyperlink: TdxSpreadSheetHyperlink read GetFocusedHyperlink;
    property FocusedRow: Integer read GetFocusedRow;
    property ForcedSelectionMode: TdxSpreadSheetTableViewSelectionMode read FForcedSelectionMode write SetForcedSelectionMode; // for internal use
    property HintHelper: TdxSpreadSheetHintHelper read FHintHelper;
    property HitTest: TdxSpreadSheetTableViewHitTest read GetViewHitTest;
    property MergedCells: TdxSpreadSheetMergedCellList read GetMergedCells;
    property OptionsBehavior: TdxSpreadSheetOptionsBehavior read GetOptionsBehavior;
    property Rows: TdxSpreadSheetTableRows read GetRows;
    property Selection: TdxSpreadSheetTableViewSelection read GetSelection;
    property SelectionMode: TdxSpreadSheetTableViewSelectionMode read FSelectionMode write SetSelectionModeField;
    property View: TdxSpreadSheetTableView read GetView;
    property ViewInfo: TdxSpreadSheetTableViewInfo read GetViewInfo;
  end;

  { TdxSpreadSheetConditionalFormatting }

  TdxSpreadSheetConditionalFormatting = class(TdxSpreadSheetCustomConditionalFormatting)
  strict private
    FView: TdxSpreadSheetTableView;
    function GetHistory: TdxSpreadSheetHistory; inline;
  protected
    procedure DoChanged; override;
    procedure DoRuleAdded(ARule: TdxSpreadSheetCustomConditionalFormattingRule); override;
    procedure DoRuleChanging(ARule: TdxSpreadSheetCustomConditionalFormattingRule); override;
    procedure DoRuleIndexChanged(AOldIndex, ANewIndex: Integer); override;
    procedure DoRuleDeleted(ARule: TdxSpreadSheetCustomConditionalFormattingRule); override;

    property History: TdxSpreadSheetHistory read GetHistory;
  public
    constructor Create(AView: TdxSpreadSheetTableView); reintroduce;
    procedure Add(const AArea: string; ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule); overload;
    procedure BeginEditing; override;
    procedure EndEditing(ACancel: Boolean); override;
  end;

  { TdxSpreadSheetTableView }

  TdxSpreadSheetCellCommentHideEvent = procedure (Sender: TdxSpreadSheetCell;
    ACommentContainer: TdxSpreadSheetContainer) of object;
  TdxSpreadSheetCellCommentShowEvent = procedure (Sender: TdxSpreadSheetCell;
    ACommentContainer: TdxSpreadSheetContainer; var AHandled: Boolean) of object;

  TdxSpreadSheetTableViewClearCellsOption = (ccoValues, ccoHyperlinks, ccoComments, ccoStyles, ccoFreeCellInstances);
  TdxSpreadSheetTableViewClearCellsOptions = set of TdxSpreadSheetTableViewClearCellsOption;

  TdxSpreadSheetTableViewMergedCellsConflictResolution = (
    mccrShowConfirmation, mccrUnmergeAllAffectedCells, mccrRaiseException);

  TdxSpreadSheetTableView = class(TdxSpreadSheetCustomView,
    IdxSpreadSheetCellStyleOwner,
    IdxSpreadSheetConditionalFormatting,
    IdxSpreadSheetConditionalFormattingOwner,
    IdxSpreadSheetViewData,
    IdxSpreadSheetTableView)
  strict private
    FColumns: TdxSpreadSheetTableColumns;
    FConditionalFormatting: TdxSpreadSheetConditionalFormatting;
    FDimensions: TRect;
    FFrozenColumn: Integer;
    FFrozenRow: Integer;
    FGUID: string;
    FHyperlinks: TdxSpreadSheetHyperlinks;
    FMergedCells: TdxSpreadSheetMergedCellList;
    FOptionsPrint: TdxSpreadSheetTableViewOptionsPrint;
    FOptionsProtection: TdxSpreadSheetSheetProtectionOptions;
    FRows: TdxSpreadSheetTableRows;
    FSelection: TdxSpreadSheetTableViewSelection;

    procedure AddArrayFormula(AFormula: TdxSpreadSheetFormula; const AText: string; const AArea: TRect); overload;
    function GetBottomRow: Integer;
    function GetCell(ARow, AColumn: Integer): TdxSpreadSheetCell; inline;
    function GetController: TdxSpreadSheetTableViewController; inline;
    function GetDimensions: TRect;
    function GetEditingController: TdxSpreadSheetTableViewEditingController; inline;
    function GetHistory: TdxSpreadSheetHistory;
    function GetHitTest: TdxSpreadSheetTableViewHitTest; inline;
    function GetIsEditing: Boolean;
    function GetLeftColumn: Integer;
    function GetOptions: TdxSpreadSheetTableViewOptions;
    function GetPrintArea: TRect;
    function GetRightColumn: Integer;
    function GetState: TdxSpreadSheetStates; inline;
    function GetStringTable: TdxSpreadSheetSharedStringTable; inline;
    function GetTopRow: Integer;
    function GetViewInfo: TdxSpreadSheetTableViewInfo; inline;
    procedure SetBottomRow(AValue: Integer);
    procedure SetFrozenColumn(AValue: Integer);
    procedure SetFrozenRow(AValue: Integer);
    procedure SetLeftColumn(AValue: Integer);
    procedure SetOptions(AValue: TdxSpreadSheetTableViewOptions);
    procedure SetOptionsPrint(AValue: TdxSpreadSheetTableViewOptionsPrint);
    procedure SetOptionsProtection(AValue: TdxSpreadSheetSheetProtectionOptions);
    procedure SetRightColumn(AValue: Integer);
    procedure SetTopRow(AValue: Integer);

    function CheckPasteSelectionSizes(const AClipboardArea: TRect): Boolean;

    procedure OptionsProtectionChangeHandler(Sender: TObject);
  protected
    function StringToAreaReference(const S: string): TRect;
    function StringToColumnIndex(const S: string): Integer;
    procedure StringToCellReference(const S: string; out ARow, AColumn: Integer);

    procedure AddArrayFormula(AFormula: TdxSpreadSheetFormula; const AArea: TRect); overload;
    function CanClearSelectedCells: Boolean;
    function CanDeleteCore: Boolean; virtual;
    function CanInsertCellsCore: Boolean; virtual;
    function CanModifyDataInArrayFormulaArea(AArea: TRect; AMode: TdxSpreadSheetCellsModificationMode): Boolean;
    procedure ClearCells(const AArea, AExcludedArea: TRect; AOptions: TdxSpreadSheetTableViewClearCellsOptions); overload;

    procedure CheckCopySelection;
    procedure CheckCutToClipboard;
    procedure CheckPasteSelection(const AClipboardArea: TRect; AMode: TdxSpreadSheetClipboardCopyMode);
    procedure CheckProtection(AMode: TdxSpreadSheetCellsModificationMode); overload;
    procedure CheckProtection(AMode: TdxSpreadSheetCellsModificationMode; const AArea: TRect); overload;

    procedure FillCells(ATopDown: Boolean);

    function GetActiveCell(const ARow, AColumn: Integer): TdxSpreadSheetCell;
    function GetArrayFormulaArea(const ASenderRowIndex, ASenderColumnIndex: Integer): TRect; inline;
    function GetTopCell(const ACurrentRow, ACurrentColumn: Integer): TdxSpreadSheetCell;
    procedure PopulateAreaByValue(const AValue: Variant; const AArea: TRect);

    function GetCellsModification(const AArea: TRect; AIsDeletingMode: Boolean; out AModification: TdxSpreadSheetCellsModification): Boolean;
    function GetCellStyle(ARow, AColumn: Integer; ACell: TdxSpreadSheetCell): TdxSpreadSheetCellStyle; inline;
    function GetCellStyleHandle(AMergedCell: TdxSpreadSheetMergedCell): TdxSpreadSheetCellStyleHandle; overload; inline;
    function GetCellStyleHandle(ARow, AColumn: Integer): TdxSpreadSheetCellStyleHandle; overload; inline;
    function GetCellStyleHandle(ARow, AColumn: Integer; ACell: TdxSpreadSheetCell): TdxSpreadSheetCellStyleHandle; overload; inline;
    function GetFactuallyModifiableArea(const AArea: TRect; AModification: TdxSpreadSheetCellsModification): TRect;
    function GetFocusedCellStyle: TdxSpreadSheetCellStyle;
    function GetLockedStateOfCellsInArea(const AArea: TRect): TCheckBoxState;
    function GetNextVisibleView(AOnCycle: Boolean): TdxSpreadSheetTableView;

    // IdxSpreadSheetConditionalFormatting
    function GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;

    // IdxSpreadSheetConditionalFormattingOwner
    function GetFormulaController: TdxSpreadSheetCustomFormulaController;
    function IdxSpreadSheetConditionalFormattingOwner.GetSelectionArea = GetConditionalFormattingSelectionArea;
    function GetConditionalFormattingSelectionArea: TRect;
    function IsRightToLeft: Boolean;

    // IdxSpreadSheetViewData
    procedure ForEachCell(const AArea: TRect; AProc: TdxSpreadSheetViewForEachCellProc; AGoForward: Boolean = True); overload;
    function GetCellData(const ARow, AColumn: Integer): IdxSpreadSheetCellData; overload;
    function GetMaxColumnIndex: Integer;
    function GetMaxRowIndex: Integer;
    function GetNextColumnWithNonEmptyCell(const ARow, AColumn: Integer; const AGoForward: Boolean = True): Integer;
    function GetNextRowWithNonEmptyCell(const ARow, AColumn: Integer; const AGoForward: Boolean = True): Integer;
    function IsRowVisible(const ARow: Integer): Boolean;
    procedure SetCellData(const ARow, AColumn: Integer; const AValue: Variant; const AErrorCode: TdxSpreadSheetFormulaErrorCode);

    // IdxSpreadSheetTableView
    function IdxSpreadSheetTableView.GetCell = CreateCell;
    function GetAbsoluteCellBounds(const ARowIndex, AColumnIndex: Integer; ACheckMergedCells: Boolean = True): TRect;
    function GetCellAtAbsolutePoint(const P: TPoint; out ARowIndex, AColumnIndex: Integer): Boolean;
    function GetCellArea(const ARowIndex, AColumnIndex: Integer): TRect;

    // IdxSpreadSheetCellStyleOwner
    procedure CellStyleChanged;
    procedure CellStyleChanging;
    function GetCellStyles: TdxSpreadSheetCellStyles;
    function GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
    procedure ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);

    function CreateColumns: TdxSpreadSheetTableColumns; virtual;
    function CreateConditionalFormatting: TdxSpreadSheetConditionalFormatting; virtual;
    function CreateController: TdxSpreadSheetCustomViewController; override;
    function CreateHitTest: TdxSpreadSheetCustomHitTest; override;
    function CreateHyperlinks: TdxSpreadSheetHyperlinks; virtual;
    function CreateMergedCells: TdxSpreadSheetMergedCellList; virtual;
    function CreateOptions: TdxSpreadSheetCustomViewOptions; override;
    function CreateOptionsPrint: TdxSpreadSheetTableViewOptionsPrint; virtual;
    function CreateOptionsProtection: TdxSpreadSheetSheetProtectionOptions; virtual;
    function CreateRows: TdxSpreadSheetTableRows; virtual;
    function CreateSelection: TdxSpreadSheetTableViewSelection; virtual;
    function CreateViewInfo: TdxSpreadSheetCustomViewViewInfo; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    procedure DimensionChanged; inline;
    function DoActiveCellChanging(AColumn, ARow: Integer): Boolean; virtual;
    procedure DoCheckChanges; override;
    procedure DoCompare(const AData1, AData2: TdxSpreadSheetCellData; var Compare: Integer); virtual;
    procedure DoDataChanged; override;
    procedure DoEditChanged; virtual;
    procedure DoEdited; virtual;
    procedure DoEditing(var AProperties: TcxCustomEditProperties; var ACanEdit: Boolean); virtual;
    procedure DoEditValueChanged; virtual;
    procedure DoInitEdit(AEdit: TcxCustomEdit); virtual;
    procedure DoInitEditValue(AEdit: TcxCustomEdit; var AValue: Variant); virtual;
    procedure DoOptionsProtectionChanged; virtual;
    procedure DoRemoveCell(ACell: TdxSpreadSheetCell); virtual;

    procedure ExchangeValues(ACell1, ACell2: TdxSpreadSheetCell); overload; virtual;
    procedure ExchangeValues(ARow1, AColumn1, ARow2, AColumn2: Integer); overload; virtual;
    procedure Pack; override;
    procedure PasteFromClipboardCore(AOptions: TdxSpreadSheetClipboardPasteOptions;
      const AViewGUID: string; const AClipboardArea: TRect; AClipboardCopyMode: TdxSpreadSheetClipboardCopyMode); virtual;
    procedure RecalculateBestFit; override;
    procedure SelectionChanged; override;
    procedure SetDisplayValuesToDirty;
    procedure ToggleShowFormulas;
    procedure ValidateDimension;

    function GetContentOrigin: TPoint; override;
    function GetPartOffsetByPoint(const P: TPoint): TPoint; override;
    function GetZoomFactor: Integer; override;

    property GUID: string read FGUID;
    property State: TdxSpreadSheetStates read GetState;
    property EditingController: TdxSpreadSheetTableViewEditingController read GetEditingController;
    property FormulaController: TdxSpreadSheetCustomFormulaController read GetFormulaController;
    property StringTable: TdxSpreadSheetSharedStringTable read GetStringTable;
    property ViewInfo: TdxSpreadSheetTableViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InvalidateRect(const R: TRect); override;

    procedure CellAtPoint(const P: TPoint; out ARow, AColumn: Integer;
      AReturnNearestCell: Boolean = False; AVisibleAreaOnly: Boolean = True); overload;
    procedure CellAtPoint(const P: TPoint; out ACell: TdxSpreadSheetCell;
      AReturnNearestCell: Boolean = False; AVisibleAreaOnly: Boolean = True); overload;
    function CreateCell(const ACellReference: string): TdxSpreadSheetCell; overload;
    function CreateCell(const ARow, AColumn: Integer): TdxSpreadSheetCell; overload; virtual;
    procedure GetCellValue(ARow, AColumn: Integer; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);

    function CanClearCells: Boolean; overload; virtual;
    function CanClearCells(const AArea: TRect): Boolean; overload; virtual;
    function CanClearCells(const AArea: string): Boolean; overload;
    procedure ClearCells(const AArea: TRect; AClearValues: Boolean = True; AClearFormats: Boolean = True); overload; virtual;
    procedure ClearCells(const AArea: TRect; AOptions: TdxSpreadSheetTableViewClearCellsOptions); overload;
    procedure ClearCells(const AArea: string; AClearValues: Boolean = True; AClearFormats: Boolean = True); overload;
    procedure ClearCells(const AArea: string; AOptions: TdxSpreadSheetTableViewClearCellsOptions); overload;
    procedure ClearCellValues;

    function CanDelete: Boolean; overload; virtual;
    function CanDelete(AModification: TdxSpreadSheetCellsModification): Boolean; overload; virtual;
    procedure DeleteAllCells;
    procedure DeleteCells; overload; virtual;
    procedure DeleteCells(const AArea: TRect; AModification: TdxSpreadSheetCellsModification;
      AMergedCellsConflictResolution: TdxSpreadSheetTableViewMergedCellsConflictResolution = mccrShowConfirmation); overload; virtual;
    procedure DeleteCells(const AArea: string; AModification: TdxSpreadSheetCellsModification;
      AMergedCellsConflictResolution: TdxSpreadSheetTableViewMergedCellsConflictResolution = mccrShowConfirmation); overload;
    procedure DeleteColumns(AStartColumn: Integer; ACount: Integer);
    procedure DeleteRows(AStartRow: Integer; ACount: Integer);

    function CanFillData: Boolean; virtual;
    procedure FillData(ADirection: TcxDirection; ACount: Integer); overload;
    procedure FillData(const ASourceArea, ATargetArea: string); overload;
    procedure FillData(const ASourceArea, ATargetArea: TRect); overload;
    procedure FillData(const ASourceArea: string; ADirection: TcxDirection; ACount: Integer); overload;
    procedure FillData(const ASourceArea: TRect; ADirection: TcxDirection; ACount: Integer); overload; virtual;

    procedure AddArrayFormula(const AText: string; const AArea: TRect); overload;
    procedure AddArrayFormula(const AText: string; const AArea: string); overload;
    function CanModifyDataInArea(const AArea: TRect; const AMode: TdxSpreadSheetCellsModificationMode): Boolean; overload;
    function CanModifyDataInArea(const AArea: string; const AMode: TdxSpreadSheetCellsModificationMode): Boolean; overload;

    procedure ForEachCell(AProc: TdxSpreadSheetTableViewForEachCellProc); overload;
    procedure ForEachCell(const AArea: TRect; AProc: TdxSpreadSheetTableViewForEachCellProc; AGoForward: Boolean = True); overload;
    procedure ForEachCell(const AArea: string; AProc: TdxSpreadSheetTableViewForEachCellProc; AGoForward: Boolean = True); overload;

    procedure FreezeColumns(const AColumn: Integer); overload;
    procedure FreezeColumns(const AColumn: string); overload;
    procedure FreezePanes(const ACellReference: string); overload;
    procedure FreezePanes(const ARow, AColumn: Integer); overload;
    procedure FreezeRows(const ARow: Integer);
    procedure UnfreezePanes;

    function CanInsert: Boolean; overload; virtual;
    function CanInsert(AModification: TdxSpreadSheetCellsModification): Boolean; overload; virtual;
    function CanInsertCells(AArea: TRect; const AModification: TdxSpreadSheetCellsModification): Boolean; overload;
    function CanInsertCells(const AArea: string; const AModification: TdxSpreadSheetCellsModification): Boolean; overload;
    procedure InsertCells; overload; virtual;
    procedure InsertCells(const AArea: TRect; AModification: TdxSpreadSheetCellsModification;
      AMergedCellsConflictResolution: TdxSpreadSheetTableViewMergedCellsConflictResolution = mccrShowConfirmation); overload; virtual;
    procedure InsertCells(const AArea: string; AModification: TdxSpreadSheetCellsModification;
      AMergedCellsConflictResolution: TdxSpreadSheetTableViewMergedCellsConflictResolution = mccrShowConfirmation); overload;
    procedure InsertColumns(AColumn: Integer; ACount: Integer); overload;
    procedure InsertColumns(const AColumn: string; ACount: Integer); overload;
    procedure InsertRows(ARow: Integer; ACount: Integer);

    procedure MakeFocusedCellVisible;
    procedure MakeVisible(AContainer: TdxSpreadSheetContainer); overload;
    procedure MakeVisible(AArea: TRect); overload;
    procedure MakeVisible(ARow, AColumn: Integer); overload;
    procedure MakeVisible(const AArea: string); overload;
    procedure MakeVisibleColumn(const AColumn: Integer); overload;
    procedure MakeVisibleColumn(const AColumn: string); overload;
    procedure MakeVisibleRow(const ARow: Integer);

    function CanCopyToClipboard: Boolean; virtual;
    function CanCutToClipboard: Boolean; virtual;
    function CanPasteFromClipboard: Boolean; overload; virtual;
    function CanPasteFromClipboard(AOptions: TdxSpreadSheetClipboardPasteOptions): Boolean; overload; virtual;
    procedure CopyToClipboard; virtual;
    procedure CutToClipboard; virtual;
    procedure PasteFromClipboard; overload; virtual;
    procedure PasteFromClipboard(AOptions: TdxSpreadSheetClipboardPasteOptions); overload; virtual;

    procedure CopyCellDataToStream(const AArea: TRect; AStream: TStream); overload;
    procedure CopyCellDataToStream(const AArea: string; AStream: TStream); overload;
    procedure PasteCellDataFromStream(const ADestination: TPoint; AStream: TStream);

    function CanMergeSelected: Boolean; virtual;
    function CanSplitSelected: Boolean; virtual;
    procedure MergeSelected; virtual;
    procedure SplitSelected; virtual;

    // Containers
    function CanEditContainers: Boolean; override;

    // Hyperlinks
    function CanDeleteHyperlink: Boolean; virtual;
    function CanEditHyperlinks: Boolean; virtual;
    procedure DeleteHyperlink; virtual;
    procedure EditHyperlink; virtual;

    // Protection
    procedure Protect;
    procedure Unprotect;

    // Comments
    function CanEditComment: Boolean; virtual;
    function CanDeleteComments: Boolean; overload;
    function CanDeleteComments(const AArea: string): Boolean; overload;
    function CanDeleteComments(const AArea: TRect): Boolean; overload; virtual;
    procedure EditComment; virtual;
    procedure DeleteComments; overload;
    procedure DeleteComments(const AArea: string); overload;
    procedure DeleteComments(const AArea: TRect); overload; virtual;

    procedure HideEdit(Accept: Boolean);
    procedure ShowEdit;
    procedure ShowEditByKey(AKey: Char);
    procedure ShowEditByMouse(X, Y: Integer; AShift: TShiftState);

    function CanSort: Boolean; virtual;
    procedure SortByColumnValues(const AArea: string; const ASortOrders: array of TdxSortOrder; const AColumns: array of Integer); overload;
    procedure SortByColumnValues(const AArea: TRect; const ASortOrders: array of TdxSortOrder; const AColumns: array of Integer); overload;
    procedure SortByRowValues(const AArea: string; const ASortOrders: array of TdxSortOrder; const ARows: array of Integer); overload;
    procedure SortByRowValues(const AArea: TRect; const ASortOrders: array of TdxSortOrder; const ARows: array of Integer); overload;

    property BottomRow: Integer read GetBottomRow write SetBottomRow;
    property LeftColumn: Integer read GetLeftColumn write SetLeftColumn;
    property RightColumn: Integer read GetRightColumn write SetRightColumn;
    property TopRow: Integer read GetTopRow write SetTopRow;

    property FrozenColumn: Integer read FFrozenColumn write SetFrozenColumn default -1;
    property FrozenRow: Integer read FFrozenRow write SetFrozenRow default -1;

    property Cells[ARow, AColumn: Integer]: TdxSpreadSheetCell read GetCell;
    property Columns: TdxSpreadSheetTableColumns read FColumns;
    property ConditionalFormatting: TdxSpreadSheetConditionalFormatting read FConditionalFormatting;
    property Controller: TdxSpreadSheetTableViewController read GetController;
    property History: TdxSpreadSheetHistory read GetHistory;
    property Hyperlinks: TdxSpreadSheetHyperlinks read FHyperlinks;
    property MergedCells: TdxSpreadSheetMergedCellList read FMergedCells;
    property PrintArea: TRect read GetPrintArea;
    property Rows: TdxSpreadSheetTableRows read FRows;
    property Selection: TdxSpreadSheetTableViewSelection read FSelection;

    property Caption;
    property Dimensions: TRect read GetDimensions;
    property HitTest: TdxSpreadSheetTableViewHitTest read GetHitTest;
    property IsEditing: Boolean read GetIsEditing;
    property Options: TdxSpreadSheetTableViewOptions read GetOptions write SetOptions;
    property OptionsPrint: TdxSpreadSheetTableViewOptionsPrint read FOptionsPrint write SetOptionsPrint;
    property OptionsProtection: TdxSpreadSheetSheetProtectionOptions read FOptionsProtection write SetOptionsProtection;
  end;

  { TdxSpreadSheetTableViewPainter }

  TdxSpreadSheetTableViewPainter = class(TdxSpreadSheetCustomViewPainter)
  strict private
    FGroupExpandButtonMinSize: TSize;
  protected type
    TDrawProc = reference to procedure (ACanvas: TcxCanvas; const R: TRect);
  protected
    procedure BufferedDraw(ACanvas: TcxCanvas; const R: TRect; AProc: TDrawProc);
  public
    procedure FlushCache; override;
    // Grouping
    procedure DrawGroupExpandButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AVertical, AExpanded: Boolean); virtual;
    procedure DrawGroupLevelExpandButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; ALevel: Integer); virtual;
    procedure DrawGroupLevelMark(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawGroupLine(ACanvas: TcxCanvas; const R: TRect; AVertical: Boolean;
      AExpandButtonPosition: TdxSpreadSheetTableItemGroupExpandButtonPosition); virtual;
    function GetGroupExpandButtonMinSize: TSize; virtual;
    function GetGroupLevelMarkSize: TSize; virtual;
    function GetGroupLineSize: Integer; virtual;
  end;

  { TdxSpreadSheetTableViewInfo }

  TdxSpreadSheetTableViewInfo = class(TdxSpreadSheetCustomViewViewInfo)
  strict private
    FBackgroundCells: TdxSpreadSheetCellViewInfoList;
    FCells: TdxSpreadSheetTableViewCellViewInfoList;
    FCellsArea: TRect;
    FCellsOverlay: TdxSpreadSheetCellViewInfoList;
    FColumnsHeader: TdxSpreadSheetCellViewInfoList;
    FContentParams: TcxViewParams;
    FDefaultCellStyleHandle: TdxSpreadSheetCellStyleHandle;
    FFirstColumnOrigin: Integer;
    FFirstRowOrigin: Integer;
    FFirstScrollableColumn: Integer;
    FFirstScrollableColumnOffset: Integer;
    FFirstScrollableRow: Integer;
    FFirstScrollableRowOffset: Integer;
    FFocusedCell: TdxSpreadSheetTableViewCellViewInfo;
    FFrozenColumnSeparatorPosition: Integer;
    FFrozenRowSeparatorPosition: Integer;
    FGridLineColor: TColor;
    FGroupingAreaLeft: TdxSpreadSheetTableViewCustomGroupingAreaViewInfo;
    FGroupingAreas: TdxSpreadSheetCellViewInfoList;
    FGroupingAreaTop: TdxSpreadSheetTableViewCustomGroupingAreaViewInfo;
    FHasGridLines: Boolean;
    FHeaderParams: TcxViewParams;
    FHeaderSize: TSize;
    FLastVisibleColumn: Integer;
    FLastVisibleRow: Integer;
    FMergedCells: TdxSpreadSheetTableViewMergedCellViewInfoList;
    FRowsHeader: TdxSpreadSheetCellViewInfoList;
    FScrollableArea: TRect;
    FSelectionCell: TdxSpreadSheetTableViewSelectionViewInfo;
    FSelectionParams: TcxViewParams;
    FVisibleCells: TRect;
    FVisibleColumnCount: Integer;
    FVisibleRowCount: Integer;

    procedure AddOutOfBoundsNonEmptyCell(ARow: TdxSpreadSheetTableRow; AIndex: Integer; const ABounds: TRect; AGoForward: Boolean);
    function CalculatePrevPageFirstIndex(APosition: Integer; AIsHorizontal: Boolean): Integer; overload;
    function CalculatePrevPageFirstIndex(APosition, ASize, AFixedIndex: Integer; AItems: TdxSpreadSheetTableItems): Integer; overload;
    function GetColumns: TdxSpreadSheetTableColumns; inline;
    function GetFirstScrollableColumn: Integer;
    function GetFirstScrollableRow: Integer;
    function GetFocusedCell: TdxSpreadSheetTableViewCellViewInfo;
    function GetFrozenColumn: Integer; inline;
    function GetFrozenRow: Integer; inline;
    function GetHScrollBarPos: Integer;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetOptions: TdxSpreadSheetTableViewOptions; inline;
    function GetOptionsView: TdxSpreadSheetOptionsView; inline;
    function GetOrigin: TPoint; inline;
    function GetPainter: TdxSpreadSheetTableViewPainter; inline;
    function GetReferenceHighlighter: TdxSpreadSheetCustomReferenceHighlighter;
    function GetRows: TdxSpreadSheetTableRows;  inline;
    function GetStyles: TdxSpreadSheetStyles; inline;
    function GetTotalColumnCount: Integer;
    function GetTotalRowCount: Integer;
    function GetView: TdxSpreadSheetTableView; inline;
    function GetVScrollBarPos: Integer;
    function RemoveNeighborCellBorder(ASide: TcxBorder;
      AIndex, AStartIndex, AFinishIndex: Integer; var AEndPos: Integer): Boolean; inline;
    procedure SetFirstScrollableColumn(AValue: Integer);
    procedure SetFirstScrollableRow(AValue: Integer);
  protected
    procedure AddCell(ARow: TdxSpreadSheetTableRow; ARowIndex, AColumnIndex: Integer; const ABounds: TRect); virtual;
    procedure AddFrozenPaneSeparator(const ABounds: TRect);
    procedure AddGroupingAreaCells; virtual;
    procedure AddPageBreak(const ABounds: TRect); virtual;
    procedure AddPageBreakHorz(const ARowIndex: Integer; const APrintArea: TRect); virtual;
    procedure AddPageBreakVert(const AColumnIndex: Integer; const APrintArea: TRect); virtual;
    procedure AddPrintArea(const AArea: TRect); virtual;
    procedure PrepareCanvas(ACanvas: TcxCanvas; const AParams: TcxViewParams);

    procedure CalculateAreas(ABounds: TRect); virtual;
    procedure CalculateCells; virtual;
    procedure CalculateColumnHeaders; virtual;
    procedure CalculateContainerDependedCells(AContainerViewInfo: TdxSpreadSheetContainerViewInfo); override;
    procedure CalculateGroupingAreas; virtual;
    procedure CalculateHeaders; virtual;
    procedure CalculateHitTest(AHitTest: TdxSpreadSheetCustomHitTest); virtual;
    procedure CalculateLastItemIndex(ASize, AStartIndex, AFixedIndex, AFirstScrollableIndex, AMaxIndex: Integer;
      AItems: TdxSpreadSheetTableItems; var ASeparatorPosition, ALastIndex: Integer; ASeparatorPositionOffset: Integer);
    procedure CalculatePrintAreas; virtual;
    procedure CalculateRowHeaders; virtual;
    procedure CalculateSelection; virtual;
    procedure CalculateVisibleArea; virtual;

    procedure ChangeVisibleArea(ASide: TcxBorder; AValue: Integer);
    procedure ChangeVisibleAreaBounds(ASide: TcxBorder; AValue: Integer);
    procedure CheckRowsBestFit; virtual;
    function CreateCellViewInfo(ACell: TdxSpreadSheetCell): TdxSpreadSheetTableViewCellViewInfo; virtual;
    function CreateMergedCellViewInfo(AMergedCell: TdxSpreadSheetMergedCell): TdxSpreadSheetTableViewMergedCellViewInfo; virtual;
    function CreatePainter: TdxSpreadSheetCustomViewPainter; override;
    function CreatePageBreakViewInfo: TdxSpreadSheetCellViewInfo; virtual;
    function CreatePrintAreaViewInfo: TdxSpreadSheetCellViewInfo; virtual;
    procedure DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;

    procedure DrawCellsArea(ACanvas: TcxCanvas); virtual;
    procedure DrawPart(ACanvas: TcxCanvas; const AOffset: TPoint; const AClipRect: TRect); virtual;
    procedure DrawSelectedAreaFrame(ACanvas: TcxCanvas; const AArea: TRect);
    procedure DrawSelection(ACanvas: TcxCanvas); virtual;

    function ContentRectToScreenRect(const R: TRect): TRect;
    function GetCellClipArea(ARow, AColumn: Integer): TRect; inline;
    procedure GetCellAtPoint(const P: TPoint; out ARow, AColumn: Integer;
      AReturnNearestCell: Boolean = False; AVisibleAreaOnly: Boolean = True);
    function GetMergedCell(ARow, AColumn: Integer): TdxSpreadSheetMergedCell;
    function GetNeighborCellStyle(ARow, AColumn: Integer; ASide: TcxBorder;
      ADisplayStyle, ACheckVisibleNeighborsOnly: Boolean): TdxSpreadSheetCellStyleHandle;

    function GetPartBounds(APart: Integer): TRect;
    function GetPartCount: Integer;
    function GetPartOffset(APart: Integer): TPoint;
    function GetPartOffsetByPoint(const P: TPoint): TPoint;

    procedure InitScrollBarsParameters; override;
    procedure MergeCellsBorders(AStartIndex, AFinishIndex: Integer);
    procedure PostProcessRowCells(ARow: TdxSpreadSheetTableRow; AStartIndex, AFinishIndex: Integer);
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure SelectionChanged; override;
    procedure SetScrollBarInfo(AScrollBarKind: TScrollBarKind; AMin, AMax, AStep, APage, APos: Integer; AAllowShow, AAllowHide: Boolean);
    procedure ValidateBounds(AStartIndex, AFinishIndex, AFixedIndex, ASeparatorPosition: Integer; var AStart, AFinish: Integer);
  public
    constructor Create(AView: TdxSpreadSheetCustomView); override;
    destructor Destroy; override;
    function CanDrawCellSelection: Boolean; virtual;
    procedure Calculate; override;
    procedure Clear; override;
    procedure Draw(ACanvas: TcxCanvas); override;
    procedure InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest); virtual;
    procedure Validate;

    function GetAreaAbsoluteBounds(const AArea: TRect): TRect;
    function GetAreaBounds(ARow, AColumn: Integer; const AArea, ACellBounds: TRect): TRect; overload;
    function GetAreaBounds(const AArea: TRect): TRect; overload;

    property BackgroundCells: TdxSpreadSheetCellViewInfoList read FBackgroundCells;
    property Cells: TdxSpreadSheetTableViewCellViewInfoList read FCells;
    property CellsArea: TRect read FCellsArea;
    property CellsOverlay: TdxSpreadSheetCellViewInfoList read FCellsOverlay;
    property Columns: TdxSpreadSheetTableColumns read GetColumns;
    property ColumnsHeader: TdxSpreadSheetCellViewInfoList read FColumnsHeader;
    property ContentParams: TcxViewParams read FContentParams;
    property DefaultCellStyleHandle: TdxSpreadSheetCellStyleHandle read FDefaultCellStyleHandle;
    property FirstColumnOrigin: Integer read FFirstColumnOrigin;
    property FirstRowOrigin: Integer read FFirstRowOrigin;
    property FirstScrollableColumn: Integer read GetFirstScrollableColumn write SetFirstScrollableColumn;
    property FirstScrollableColumnOffset: Integer read FFirstScrollableColumnOffset;
    property FirstScrollableRow: Integer read GetFirstScrollableRow write SetFirstScrollableRow;
    property FirstScrollableRowOffset: Integer read FFirstScrollableRowOffset;
    property FocusedCell: TdxSpreadSheetTableViewCellViewInfo read GetFocusedCell;
    property FrozenColumn: Integer read GetFrozenColumn;
    property FrozenColumnSeparatorPosition: Integer read FFrozenColumnSeparatorPosition;
    property FrozenRow: Integer read GetFrozenRow;
    property FrozenRowSeparatorPosition: Integer read FFrozenRowSeparatorPosition;
    property GridLineColor: TColor read FGridLineColor;
    property GroupingAreaLeft: TdxSpreadSheetTableViewCustomGroupingAreaViewInfo read FGroupingAreaLeft;
    property GroupingAreas: TdxSpreadSheetCellViewInfoList read FGroupingAreas;
    property GroupingAreaTop: TdxSpreadSheetTableViewCustomGroupingAreaViewInfo read FGroupingAreaTop;
    property HasGridLines: Boolean read FHasGridLines;
    property HeaderParams: TcxViewParams read FHeaderParams;
    property HeaderSize: TSize read FHeaderSize;
    property HScrollBarPos: Integer read GetHScrollBarPos;
    property LastVisibleColumn: Integer read FLastVisibleColumn;
    property LastVisibleRow: Integer read FLastVisibleRow;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property MergedCells: TdxSpreadSheetTableViewMergedCellViewInfoList read FMergedCells;
    property Options: TdxSpreadSheetTableViewOptions read GetOptions;
    property OptionsView: TdxSpreadSheetOptionsView read GetOptionsView;
    property Origin: TPoint read GetOrigin;
    property Painter: TdxSpreadSheetTableViewPainter read GetPainter;
    property ReferenceHighlighter: TdxSpreadSheetCustomReferenceHighlighter read GetReferenceHighlighter;
    property Rows: TdxSpreadSheetTableRows read GetRows;
    property RowsHeader: TdxSpreadSheetCellViewInfoList read FRowsHeader;
    property ScrollableArea: TRect read FScrollableArea;
    property SelectionCell: TdxSpreadSheetTableViewSelectionViewInfo read FSelectionCell;
    property SelectionParams: TcxViewParams read FSelectionParams;
    property Styles: TdxSpreadSheetStyles read GetStyles;
    property TotalColumnCount: Integer read GetTotalColumnCount;
    property TotalRowCount: Integer read GetTotalRowCount;
    property View: TdxSpreadSheetTableView read GetView;
    property VisibleCells: TRect read FVisibleCells;
    property VisibleColumnCount: Integer read FVisibleColumnCount;
    property VisibleRowCount: Integer read FVisibleRowCount;
    property VScrollBarPos: Integer read GetVScrollBarPos;
  end;

  { TdxSpreadSheetDefinedName }

  TdxSpreadSheetDefinedName = class(TdxSpreadSheetObjectListItem)
  strict private
    FCaption: string;
    FScope: TdxSpreadSheetCustomView;
    FFormula: TdxSpreadSheetDefinedNameFormula;

    function GetIndex: Integer;
    function GetReference: string;
    function GetSpreadSheet: TdxCustomSpreadSheet;
    function GetValueAsText: string;
    procedure SetCaption(const AValue: string);
    procedure SetFormula(const AValue: TdxSpreadSheetDefinedNameFormula);
    procedure SetReference(const AValue: string);
    procedure SetScope(const AValue: TdxSpreadSheetCustomView);
  protected
    procedure ClearResult;
    function Compare(ACandidate: TdxSpreadSheetDefinedName): Integer;
    function CompareEx(const ACaption: string; AScope: TdxSpreadSheetCustomView; AScopeMaybeEmpty: Boolean = False): Integer;
    procedure Initialize(const ACaption, AReference: string; AScope: TdxSpreadSheetCustomView);
    procedure LoadFromStream(AReader: TcxReader);
    procedure SaveToStream(AWriter: TcxWriter);
    procedure UpdateReference;

    property Formula: TdxSpreadSheetDefinedNameFormula read FFormula write SetFormula;
  public
    destructor Destroy; override;
    procedure EnumReferences(AProc: TdxSpreadSheetFormulaEnumReferencesProc);
    function IsCellReference: Boolean; overload; virtual;
    function IsCellReference(out AArea: TRect; out AView: TdxSpreadSheetCustomView): Boolean; overload; virtual;
    function ToString: string; override;

    property Caption: string read FCaption write SetCaption;
    property Index: Integer read GetIndex;
    property Reference: string read GetReference write SetReference;
    property Scope: TdxSpreadSheetCustomView read FScope write SetScope;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
    property ValueAsText: string read GetValueAsText;
  end;

  { TdxSpreadSheetDefinedNames }

  TdxSpreadSheetDefinedNames = class(TdxSpreadSheetObjectList)
  strict private
    FSorted: Boolean;

    function GetName(AIndex: Integer): TdxSpreadSheetDefinedName;
    class function CompareNames(AName1, AName2: TdxSpreadSheetDefinedName): Integer; static;
  protected
    procedure AfterLoad; virtual;
    procedure Changed; override;
    procedure CheckSorted;
    function CreateItem: TdxSpreadSheetObjectListItem; override;
    function Find(const ACaption: string; AScope: TdxSpreadSheetCustomView; AScopeMaybeEmpty: Boolean): Integer; inline;
    procedure OnRemoveSheet(AScope: TdxSpreadSheetCustomView);

    property Sorted: Boolean read FSorted write FSorted;
  public
    function Add(const ACaption, AReference: string; AScope: TdxSpreadSheetCustomView = nil): TdxSpreadSheetDefinedName;
    function AddFromStream(AReader: TcxReader): TdxSpreadSheetDefinedName;
    function AddOrSet(const ACaption, AReference: string; AScope: TdxSpreadSheetCustomView = nil): TdxSpreadSheetDefinedName;
    function Contains(const ACaption: string; AScope: TdxSpreadSheetCustomView): Boolean;
    function GetItemByName(const ACaption: string; AScope: TdxSpreadSheetCustomView): TdxSpreadSheetDefinedName;
    function IndexOf(const ACaption: string): Integer; overload;
    function IndexOf(const ACaption: string; AScope: TdxSpreadSheetCustomView): Integer; overload;

    property Items[Index: Integer]: TdxSpreadSheetDefinedName read GetName; default;
  end;

  { TdxSpreadSheetExternalLink }

  TdxSpreadSheetExternalLink = class(TdxSpreadSheetObjectListItem)
  strict private
    FOwner: TdxSpreadSheetExternalLinks;

    function GetActualTarget: string;
  protected
    FTarget: string;
  public
    destructor Destroy; override;
    //
    property ActualTarget: string read GetActualTarget;
    property Target: string read FTarget;
    property Owner: TdxSpreadSheetExternalLinks read FOwner;
  end;

  { TdxSpreadSheetExternalLinks }

  TdxSpreadSheetExternalLinks = class(TdxSpreadSheetObjectList)
  strict private
    function GetItem(AIndex: Integer): TdxSpreadSheetExternalLink;
  protected
    function CreateItem: TdxSpreadSheetObjectListItem; override;
  public
    function Add(const ATarget: string): TdxSpreadSheetExternalLink;
    function GetLinkByTarget(const ATarget: string): TdxSpreadSheetExternalLink;

    property Items[Index: Integer]: TdxSpreadSheetExternalLink read GetItem; default;
  end;

  { TdxSpreadSheetCustomFilerProgressHelper }

  TdxSpreadSheetCustomFilerProgressHelper = class(TcxCustomProgressCalculationHelper)
  strict private
    FOwner: TdxSpreadSheetCustomFiler;
  protected
    procedure ProgressChanged; override;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFiler; AStageCount: Integer);
  end;

  { TdxSpreadSheetCustomFiler }

  TdxSpreadSheetCustomFiler = class(TdxSpreadSheetPersistentObject)
  strict private
    FIgnoreMessages: TdxSpreadSheetMessageTypes;
    FInvariantFormatSettings: TFormatSettings;
    FProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
    FStream: TStream;
    FStreamAutoFree: Boolean;

    FOnProgress: TdxSpreadSheetProgressEvent;
  protected
    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; virtual; abstract;
    procedure DoError(const AMessage: string; AType: TdxSpreadSheetMessageType); overload;
    procedure DoError(const AFormatString: string; const AArguments: array of const; AType: TdxSpreadSheetMessageType); overload;
    procedure DoProgress(AProgress: Integer);
    procedure ExecuteSubTask(ASubTask: TdxSpreadSheetCustomFilerSubTask);
    //
    property ProgressHelper: TdxSpreadSheetCustomFilerProgressHelper read FProgressHelper;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); reintroduce; virtual;
    destructor Destroy; override;
    //
    property IgnoreMessages: TdxSpreadSheetMessageTypes read FIgnoreMessages write FIgnoreMessages;
    property InvariantFormatSettings: TFormatSettings read FInvariantFormatSettings;
    property Stream: TStream read FStream;
    property StreamAutoFree: Boolean read FStreamAutoFree write FStreamAutoFree;
    //
    property OnProgress: TdxSpreadSheetProgressEvent read FOnProgress write FOnProgress;
  end;

  { TdxSpreadSheetCustomFilerSubTask }

  TdxSpreadSheetCustomFilerSubTask = class(TObject)
  strict private
    FOwner: TdxSpreadSheetCustomFiler;
    function GetSpreadSheet: TdxCustomSpreadSheet; inline;
  protected
    procedure DoError(const AFormatString: string; const AArguments: array of const; AType: TdxSpreadSheetMessageType); overload;
    procedure DoError(const AMessage: string; AType: TdxSpreadSheetMessageType); overload; inline;
    procedure ExecuteSubTask(ASubTask: TdxSpreadSheetCustomFilerSubTask); inline;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFiler);
    procedure Execute; virtual; abstract;
    //
    property Owner: TdxSpreadSheetCustomFiler read FOwner;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  end;

  { TdxSpreadSheetCustomReader }

  TdxSpreadSheetCustomReaderClass = class of TdxSpreadSheetCustomReader;
  TdxSpreadSheetCustomReader = class(TdxSpreadSheetCustomFiler)
  strict private
    function GetCellStyles: TdxSpreadSheetCellStyles; {$IFNDEF VER220} inline; {$ENDIF}
    function GetStringTable: TdxSpreadSheetSharedStringTable; inline;
  protected
    function AddBorders(ABordersHandle: TdxSpreadSheetBordersHandle): TdxSpreadSheetBordersHandle;
    function AddBrush(ABrushHandle: TdxSpreadSheetBrushHandle): TdxSpreadSheetBrushHandle;
    function AddCellStyle(AStyleHandle: TdxSpreadSheetCellStyleHandle): TdxSpreadSheetCellStyleHandle;
    function AddFont(AFontHandle: TdxSpreadSheetFontHandle): TdxSpreadSheetFontHandle;
    function AddFormattedSharedString(S: TdxSpreadSheetFormattedSharedString): TdxSpreadSheetFormattedSharedString;
    function AddImage(const AStream: TStream): TdxSpreadSheetSharedImageHandle;
    function AddNumberFormat(const AFormatCode: string; ID: Integer = -1): TdxSpreadSheetFormatHandle;
    function AddSharedString(const S: string): TdxSpreadSheetSharedString;
    function AddTableView(const ACaption: string): TdxSpreadSheetTableView;

    procedure Check(AValue: Boolean; const AMessage: string; AMessageType: TdxSpreadSheetMessageType = ssmtError);

    function CreateTempCellStyle(AFont: TdxSpreadSheetFontHandle; AFormat: TdxSpreadSheetFormatHandle;
      AFill: TdxSpreadSheetBrushHandle; ABorders: TdxSpreadSheetBordersHandle): TdxSpreadSheetCellStyleHandle;
    function CreateTempBordersHandle: TdxSpreadSheetBordersHandle;
    function CreateTempBrushHandle: TdxSpreadSheetBrushHandle;
    function CreateTempFontHandle: TdxSpreadSheetFontHandle;
    function CreateTempFormattedSharedString(const S: string): TdxSpreadSheetFormattedSharedString;

    property CellStyles: TdxSpreadSheetCellStyles read GetCellStyles;
    property StringTable: TdxSpreadSheetSharedStringTable read GetStringTable;
  public
    procedure ReadData; virtual; abstract;
  end;

  { TdxSpreadSheetCustomWriter }

  TdxSpreadSheetCustomWriterClass = class of TdxSpreadSheetCustomWriter;
  TdxSpreadSheetCustomWriter = class(TdxSpreadSheetCustomFiler)
  public
    procedure WriteData; virtual; abstract;
  end;

  { TdxSpreadSheetCustomFormat }

  TdxSpreadSheetCustomFormatClass = class of TdxSpreadSheetCustomFormat;
  TdxSpreadSheetCustomFormat = class
  public
    class function CanCheckByContent: Boolean; virtual;
    class function CanReadFromStream(AStream: TStream): Boolean; virtual;
    class function CreateFormatSettings: TdxSpreadSheetFormatSettings; virtual;
    class function CreateReader(ASpreadSheet: TdxCustomSpreadSheet; AStream: TStream): TdxSpreadSheetCustomReader; virtual;
    class function CreateWriter(ASpreadSheet: TdxCustomSpreadSheet; AStream: TStream): TdxSpreadSheetCustomWriter; virtual;
    class function GetDescription: string; virtual;
    class function GetExt: string; virtual;
    class function GetReader: TdxSpreadSheetCustomReaderClass; virtual;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; virtual;
    class procedure Register;
    class procedure Unregister;
  end;

  { TdxSpreadSheetFormatsRepository }

  TdxSpreadSheetFormatsRepository = class(TObject)
  strict private
    FList: TList;

    function GetCount: Integer;
    function GetDescriptions(Index: Integer): string;
    function GetExt(Index: Integer): string;
    function GetItem(Index: Integer): TdxSpreadSheetCustomFormatClass;
  public
    constructor Create;
    destructor Destroy; override;
    function Find(const AFileName: string; out AFormat: TdxSpreadSheetCustomFormatClass): Boolean;
    function GetOpenDialogFilter: string;
    function GetSaveDialogFilter: string;

    procedure Register(AFormat: TdxSpreadSheetCustomFormatClass);
    procedure Unregister(AFormat: TdxSpreadSheetCustomFormatClass);

    property Count: Integer read GetCount;
    property Descriptions[Index: Integer]: string read GetDescriptions;
    property Exts[Index: Integer]: string read GetExt;
    property Items[Index: Integer]: TdxSpreadSheetCustomFormatClass read GetItem; default;
  end;

  { TdxSpreadSheetCustomDragAndDropObject }

  TdxSpreadSheetCustomDragAndDropObject = class(TcxDragAndDropObject)
  strict private
    function GetControl: TdxCustomSpreadSheet;
    function GetHitTest: TdxSpreadSheetCustomHitTest;
    function GetView: TdxSpreadSheetCustomView;
  protected
    procedure CheckScrollArea(const P: TPoint); virtual;
    function GetZoomFactor: Integer; override;
    function TranslateCoords(const P: TPoint): TPoint; override;
  public
    property Control: TdxCustomSpreadSheet read GetControl;
    property HitTest: TdxSpreadSheetCustomHitTest read GetHitTest;
    property View: TdxSpreadSheetCustomView read GetView;
  end;

  { TdxSpreadSheetCustomCellViewInfo }

  TdxSpreadSheetCustomCellViewInfo = class(TcxIUnknownObject)
  strict private
    FOwner: TObject;

    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; inline;
    function GetScaleFactor: TdxScaleFactor; inline;
  protected
    FDrawingStage: TdxSpreadSheetDrawingStage;

    function CanDraw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage): Boolean; virtual;
    procedure DoDraw(ACanvas: TcxCanvas); virtual;
    function GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass; virtual;
    function GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass; virtual;
    function GetSpreadSheet: TdxCustomSpreadSheet; virtual; abstract;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; virtual;
    //
    property Owner: TObject read FOwner;
  public
    constructor Create(AOwner: TObject); virtual;
    procedure Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage); virtual;
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; virtual;
    //
    property DrawingStage: TdxSpreadSheetDrawingStage read FDrawingStage;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  end;

  { TdxSpreadSheetCellViewInfo }

  TdxSpreadSheetCellViewInfo = class(TdxSpreadSheetCustomCellViewInfo, IcxHintableObject)
  strict private
    function GetBounds: TRect; inline;
  protected
    FBounds: TRect;
    FCalculated: Boolean;
    FClipRect: TRect;
    FScreenClipRect: TRect;
    FSupportedDrawingStages: TdxSpreadSheetDrawingStages;
    FVisible: Boolean;

    procedure CalculateBounds; virtual;
    function CanDraw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage): Boolean; override;
    procedure DoCalculate; virtual;
    function DoCustomDraw(ACanvas: TcxCanvas): Boolean; virtual;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
    procedure InvalidateRect(const R: TRect); virtual;
    function IsPermanent: Boolean; virtual;
    procedure UpdateState; virtual;
    // IcxHintableObject
    function HasHintPoint(const P: TPoint): Boolean;
    function IsHintAtMousePos: Boolean;
    function UseHintHidePause: Boolean;

    property Calculated: Boolean read FCalculated write FCalculated;
  public
    constructor Create(AOwner: TObject); override;
    procedure Calculate; inline;
    procedure Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage); override;
    procedure Invalidate; virtual;
    procedure Recalculate;
    procedure SetBounds(const ABounds: TRect); overload;
    procedure SetBounds(const ABounds, AClipBounds: TRect); overload; virtual;

    property Bounds: TRect read GetBounds;
    property ClipRect: TRect read FClipRect;
    property ScreenClipRect: TRect read FScreenClipRect;
    property Visible: Boolean read FVisible;
  end;

  { TdxSpreadSheetViewAbstractCellViewInfo }

  TdxSpreadSheetViewAbstractCellViewInfo = class(TdxSpreadSheetCellViewInfo)
  protected
    function GetSpreadSheet: TdxCustomSpreadSheet; override;
    function GetView: TdxSpreadSheetCustomView; virtual; abstract;
    procedure InvalidateRect(const R: TRect); override;
  public
    property View: TdxSpreadSheetCustomView read GetView;
  end;

  { TdxSpreadSheetTableViewAbstractCellViewInfo }

  TdxSpreadSheetTableViewAbstractCellViewInfo = class(TdxSpreadSheetViewAbstractCellViewInfo)
  strict private
    function GetTableView: TdxSpreadSheetTableView; inline;
  public
    property View: TdxSpreadSheetTableView read GetTableView;
  end;

  { TdxSpreadSheetTableViewSelectionViewInfo }

  TdxSpreadSheetTableViewSelectionViewInfo = class
  strict private
    FFillAreas: TdxRectList;
    FFillBrush: TdxGPBrush;
    FFocusedCellBounds: TRect;
    FFrameViewInfo: TdxSpreadSheetCellViewInfo;
    FOwnerViewInfo: TdxSpreadSheetTableViewInfo;

    function GetFillAreaBounds(const AArea: TRect): TRect;
    function GetBoundsRect: TRect;
  protected
    procedure CalculateCore(ASelection: TdxSpreadSheetTableViewSelection); virtual;
  public
    constructor Create(AOwnerViewInfo: TdxSpreadSheetTableViewInfo);
    destructor Destroy; override;
    procedure Calculate; virtual;
    procedure Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage); virtual;
    procedure Invalidate;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; virtual;
    //
    property BoundsRect: TRect read GetBoundsRect;
    property FillAreas: TdxRectList read FFillAreas;
    property FillBrush: TdxGPBrush read FFillBrush;
    property FocusedCellBounds: TRect read FFocusedCellBounds;
    property FrameViewInfo: TdxSpreadSheetCellViewInfo read FFrameViewInfo;
    property OwnerViewInfo: TdxSpreadSheetTableViewInfo read FOwnerViewInfo;
  end;

  { TdxSpreadSheetCustomReferenceHighlighter }

  TdxSpreadSheetCustomReferenceHighlighter = class(TdxSpreadSheetCustomCellViewInfo)
  public
    procedure Calculate(const AEdit: TcxCustomEdit); overload; virtual; abstract;
    procedure Calculate(const AFormulaText: string); overload; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure HighlightReferences(AEdit: TcxCustomRichEdit); virtual; abstract;
    procedure PrepareColorPalette; virtual; abstract;
  end;

  { TdxSpreadSheetCellViewInfoList }

  TdxSpreadSheetCellViewInfoList = class(TdxFastObjectList)
  strict private
    function GetItem(AIndex: Integer): TdxSpreadSheetCellViewInfo;  inline;
  public
    procedure Add(AViewInfo: TdxSpreadSheetCellViewInfo); virtual;
    function CalculateHitTest(AHitTest: TdxSpreadSheetCustomHitTest; AReverse: Boolean = False): Boolean;
    procedure Clear; override;
    procedure Draw(ACanvas: TcxCanvas); overload;
    procedure Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage); overload;
    function FindItem(AItemOwner: TObject): Integer;
    procedure UpdateState;

    property Items[Index: Integer]: TdxSpreadSheetCellViewInfo read GetItem; default;
  end;

  { TdxSpreadSheetTableViewCustomCellViewInfo }

  TdxSpreadSheetTableViewCustomCellViewInfo = class(TdxSpreadSheetTableViewAbstractCellViewInfo)
  strict private
    function GetPainter: TdxSpreadSheetTableViewPainter; inline;
    function GetViewInfo: TdxSpreadSheetTableViewInfo; inline;
  protected
    function DoCustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass; override;
    function GetView: TdxSpreadSheetCustomView; override;
  public
    property Painter: TdxSpreadSheetTableViewPainter read GetPainter;
    property ViewInfo: TdxSpreadSheetTableViewInfo read GetViewInfo;
  end;

  { TdxSpreadSheetTableViewGroupCustomExpandButtonViewInfo }

  TdxSpreadSheetTableViewGroupCustomExpandButtonViewInfo = class(TdxSpreadSheetTableViewAbstractCellViewInfo)
  strict private
    FState: TcxButtonState;

    function GetPainter: TdxSpreadSheetTableViewPainter; inline;
    procedure SetState(AValue: TcxButtonState);
  protected
    procedure UpdateState; override;
  public
    constructor Create(AOwner: TObject); override;
    //
    property Painter: TdxSpreadSheetTableViewPainter read GetPainter;
    property State: TcxButtonState read FState write SetState;
  end;

  { TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo }

  TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo = class(TdxSpreadSheetTableViewGroupCustomExpandButtonViewInfo)
  strict private
    FLevel: Integer;

    function GetItems: TdxSpreadSheetTableItems; inline;
  protected
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetView: TdxSpreadSheetCustomView; override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
  public
    constructor Create(AOwner: TObject; ALevel: Integer); reintroduce;
    //
    property Items: TdxSpreadSheetTableItems read GetItems;
  end;

  { TdxSpreadSheetTableViewGroupViewInfo }

  TdxSpreadSheetTableViewGroupViewInfo = class(TdxSpreadSheetTableViewGroupCustomExpandButtonViewInfo)
  strict private
    FExpandButtonRect: TRect;
    FExpanded: Boolean;
    FLineRect: TRect;
    FVertical: Boolean;

    function GetGroup: TdxSpreadSheetTableItemGroup; inline;
  protected
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetView: TdxSpreadSheetCustomView; override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
  public
    constructor Create(AOwner: TObject; AVertical: Boolean); reintroduce;

    property ExpandButtonRect: TRect read FExpandButtonRect;
    property Expanded: Boolean read FExpanded;
    property Group: TdxSpreadSheetTableItemGroup read GetGroup;
    property LineRect: TRect read FLineRect;
    property Vertical: Boolean read FVertical;
  end;

  { TdxSpreadSheetTableViewCustomGroupingAreaViewInfo }

  TdxSpreadSheetTableViewCustomGroupingAreaViewInfoClass = class of TdxSpreadSheetTableViewCustomGroupingAreaViewInfo;
  TdxSpreadSheetTableViewCustomGroupingAreaViewInfo = class(TdxSpreadSheetTableViewCustomCellViewInfo)
  strict private
    FCells: TdxSpreadSheetCellViewInfoList;
    FMarks: TdxRectList;
  protected const
    ContentIndent = 2;
  protected
    FBorders: TcxBorders;

    procedure AddGroupCell(ACell: TdxSpreadSheetTableViewGroupViewInfo; const ABounds: TRect);
    procedure AddGroupLevelExpandButtonCell(ALevel: Integer; const ABounds: TRect);

    procedure CalculateGroup(AGroup: TdxSpreadSheetTableItemGroup;
      const AAreaBounds: TRect; AAreaStartIndex, AAreaFinishIndex: Integer); virtual; abstract;
    procedure CalculateGroupChildren(AGroup: TdxSpreadSheetTableItemGroup;
      const AAreaBounds: TRect; AAreaStartIndex, AAreaFinishIndex: Integer);
    function CalculateGroupLevelMarkBounds(const AAreaBounds: TRect; AStartIndex, AIndex: Integer): TRect; virtual; abstract;
    procedure CalculateGroupLevelMarks(AGroup: TdxSpreadSheetTableItemGroup; const AAreaBounds: TRect); virtual;
    procedure CalculateGroupLevelsExpandButtons; virtual; abstract;
    procedure CalculateGroups; virtual; abstract;

    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;

    function GetGroupCellsArea: TRect; virtual;
    function GetGroupLevelSize: Integer; virtual; abstract;
    function GetItems: TdxSpreadSheetTableItems; virtual; abstract;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
    procedure UpdateState; override;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    function MeasureSize: Integer; virtual;
    //
    property Cells: TdxSpreadSheetCellViewInfoList read FCells;
    property GroupCellsArea: TRect read GetGroupCellsArea;
    property GroupLevelSize: Integer read GetGroupLevelSize;
    property Items: TdxSpreadSheetTableItems read GetItems;
    property Marks: TdxRectList read FMarks;
  end;

  { TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo }

  TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo = class(TdxSpreadSheetTableViewCustomGroupingAreaViewInfo)
  protected
    procedure CalculateGroup(AGroup: TdxSpreadSheetTableItemGroup;
      const AAreaBounds: TRect; AAreaStartIndex, AAreaFinishIndex: Integer); override;
    function CalculateGroupLevelMarkBounds(const AAreaBounds: TRect; AStartIndex, AIndex: Integer): TRect; override;
    procedure CalculateGroupLevelsExpandButtons; override;
    procedure CalculateGroups; override;
    function GetGroupCellsArea: TRect; override;
    function GetGroupLevelSize: Integer; override;
    function GetItems: TdxSpreadSheetTableItems; override;
  public
    procedure AfterConstruction; override;
  end;

  { TdxSpreadSheetTableViewRowsGroupingAreaViewInfo }

  TdxSpreadSheetTableViewRowsGroupingAreaViewInfo = class(TdxSpreadSheetTableViewCustomGroupingAreaViewInfo)
  protected
    procedure CalculateGroup(AGroup: TdxSpreadSheetTableItemGroup;
      const AAreaBounds: TRect; AAreaStartIndex: Integer; AAreaFinishIndex: Integer); override;
    function CalculateGroupLevelMarkBounds(const AAreaBounds: TRect; AStartIndex, AIndex: Integer): TRect; override;
    procedure CalculateGroupLevelsExpandButtons; override;
    procedure CalculateGroups; override;
    function GetGroupCellsArea: TRect; override;
    function GetGroupLevelSize: Integer; override;
    function GetItems: TdxSpreadSheetTableItems; override;
  public
    procedure AfterConstruction; override;
  end;

  { TdxSpreadSheetContainerViewInfo }

  TdxSpreadSheetContainerViewInfo = class(TdxSpreadSheetCellViewInfo)
  strict private
    FAlpha: Byte;
    FCanDrawSelection: Boolean;
    FContentBounds: TdxRectF;
    FIsDragging: Boolean;
    FRotationAngle: Double;
    FSelection: TdxSpreadSheetContainerSelectionCellViewInfo;
    FTransformMatrix: TdxGPMatrix;
    FTransformMatrixInv: TdxGPMatrix;

    function GetOptionsView: TdxSpreadSheetOptionsView; inline;
    function GetOwner: TdxSpreadSheetContainer; inline;
    function GetSelected: Boolean;
    function GetTransform: TdxSpreadSheetContainerTransform; inline;
    procedure SetAlpha(const AValue: Byte);
    procedure SetContentBounds(const AValue: TdxRectF);
    procedure SetIsDragging(const Value: Boolean);
    procedure SetRotationAngle(const AValue: Double);
    procedure SetSelected(const AValue: Boolean);
  protected
    FFlipHorz: Boolean;
    FFlipVert: Boolean;

    procedure CalculateBounds; override;
    procedure CalculateDisplayClipRect; virtual;
    procedure CalculateSelection; virtual;
    procedure CalculateTransformMatrix; virtual;
    procedure ContentBoundsChanged; virtual;
    function CreateSelectionViewInfo: TdxSpreadSheetContainerSelectionCellViewInfo; virtual;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function DoInitHitTest(const P: TPoint; AHitTest: TdxSpreadSheetCustomHitTest): Boolean; virtual;
    procedure DrawBackground(ACanvas: TdxGPCanvas); virtual;
    procedure DrawBorder(ACanvas: TdxGPCanvas); virtual;
    procedure DrawContent(ACanvas: TdxGPCanvas); virtual;
    function GetBorderWidth: Single; virtual;
    function GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass; override;
    function GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass; override;
    function GetRealBounds: TRect;
    function GetRealDrawingBounds: TRect; virtual;
    function GetSpreadSheet: TdxCustomSpreadSheet; override;
    function HasSelectionFrame: Boolean; virtual;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
    procedure InvalidateRect(const R: TRect); override;
    function IsPermanent: Boolean; override;
    procedure UpdateState; override;
  public
    constructor Create(AContainer: TdxSpreadSheetContainer); reintroduce; virtual;
    destructor Destroy; override;
    procedure InternalDraw(ACanvas: TdxGPCanvas);
    procedure Invalidate; override;
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; override;
    //
    property Alpha: Byte read FAlpha write SetAlpha;
    property BorderWidth: Single read GetBorderWidth;
    property CanDrawSelection: Boolean read FCanDrawSelection write FCanDrawSelection;
    property ContentBounds: TdxRectF read FContentBounds write SetContentBounds;
    property FlipHorz: Boolean read FFlipHorz write FFlipHorz;
    property FlipVert: Boolean read FFlipVert write FFlipVert;
    property IsDragging: Boolean read FIsDragging write SetIsDragging;
    property OptionsView: TdxSpreadSheetOptionsView read GetOptionsView;
    property Owner: TdxSpreadSheetContainer read GetOwner;
    property RealBounds: TRect read GetRealBounds;
    property RealDrawingBounds: TRect read GetRealDrawingBounds;
    property RotationAngle: Double read FRotationAngle write SetRotationAngle;
    property Selected: Boolean read GetSelected write SetSelected;
    property Selection: TdxSpreadSheetContainerSelectionCellViewInfo read FSelection;
    property Transform: TdxSpreadSheetContainerTransform read GetTransform;
    property TransformMatrix: TdxGPMatrix read FTransformMatrix;
    property TransformMatrixInv: TdxGPMatrix read FTransformMatrixInv;
  end;

  { TdxSpreadSheetContainerCustomDragAndDropObject }

  TdxSpreadSheetContainerCustomDragAndDropObject = class(TdxSpreadSheetCustomDragAndDropObject)
  strict private
    FBounds: TRect;
    FContentRect: TRect;
    FTableViewIntf: IdxSpreadSheetTableView;
    FViewInfo: TdxSpreadSheetContainerViewInfo;

    function CheckIsKeyPressed(const Index: Integer): Boolean;
    function GetContainer: TdxSpreadSheetContainer; inline;
    function GetContentOrigin: TPoint;
    function GetHistory: TdxSpreadSheetHistory; inline;
    function GetPivotPoint: TPoint;
  protected
    FCapturePoint: TPoint;

    procedure AlignToCell(var ADeltaX, ADeltaY: Integer; const P: TPoint); virtual;
    procedure AlignToCells(var R: TRect);
    procedure BeforeBeginDragAndDrop; override;
    procedure BeginDragAndDrop; override;
    function CheckForContentArea(const R: TRect): TRect; inline;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetContainerViewInfo: TdxSpreadSheetContainerViewInfo; virtual;
    function GetContentRect: TRect; virtual;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetImmediateStart: Boolean; override;
    procedure InvalidateRects(const R1, R2: TRect);
    function ProcessKeyDown(AKey: Word; AShiftState: TShiftState): Boolean; override;
    function ProcessKeyUp(AKey: Word; AShiftState: TShiftState): Boolean; override;
    procedure RecalculateAnchors; virtual;
    procedure RecalculateDragAndDropInfo;
    function TranslateCoords(const P: TPoint): TPoint; override;

    function ToAbsoluteRect(const R: TRect): TRect;
    function ToContentRect(const R: TRect): TRect;

    property IsAltPressed: Boolean index VK_MENU read CheckIsKeyPressed;
    property IsControlPressed: Boolean index VK_CONTROL read CheckIsKeyPressed;
    property IsShiftPressed: Boolean index VK_SHIFT read CheckIsKeyPressed;
  public
    property Bounds: TRect read FBounds;
    property CapturePoint: TPoint read FCapturePoint;
    property Container: TdxSpreadSheetContainer read GetContainer;
    property ContentOrigin: TPoint read GetContentOrigin;
    property ContentRect: TRect read FContentRect;
    property History: TdxSpreadSheetHistory read GetHistory;
    property PivotPoint: TPoint read GetPivotPoint;
    property TableViewIntf: IdxSpreadSheetTableView read FTableViewIntf;
    property ViewInfo: TdxSpreadSheetContainerViewInfo read FViewInfo;
  end;

  { TdxSpreadSheetContainerSelectionCellViewInfo }

  TdxSpreadSheetContainerSelectionCellViewInfo = class(TdxSpreadSheetCellViewInfo)
  strict private
    FContainerViewInfo: TdxSpreadSheetContainerViewInfo;
  protected
    FRotateMarkerSize: Integer;
    FSizeMarkersArea: TRect;
    FSizeMarkerSize: Integer;

    procedure CalculateBounds; override;
    function CanDraw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage): Boolean; override;
    function CreatePen: TdxGpPen; virtual;
    function CreateRotateMarkerBrush: TdxGpBrush; virtual;
    function CreateSizingMarkerBrush: TdxGpBrush; virtual;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DrawContent(ACanvas: TdxGPCanvas); virtual;
    procedure DrawRotateMarker(ACanvas: TdxGPCanvas; APen: TdxGPPen); virtual;
    procedure DrawSizingMarkers(ACanvas: TdxGPCanvas; APen: TdxGPPen); virtual;
    function GetContentOffsets: TRect; virtual;
    function GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass; override;
    function GetFrameRect: TRect; virtual;
    function GetRealDrawingBounds: TRect;
    function GetRotateMarker: TRect; virtual;
    function GetSizeMarkerAtLocalPoint(const ALocalPoint: TPoint; out AMarker: TdxSpreadSheetSizingMarker): Boolean;
    function GetSizeMarkerRect(AMarker: TdxSpreadSheetSizingMarker): TRect; virtual;
    function GetSpreadSheet: TdxCustomSpreadSheet; override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
    procedure InvalidateRect(const R: TRect); override;
    //
    property RotateMarkerSize: Integer read FRotateMarkerSize;
    property SizingPointSize: Integer read FSizeMarkerSize;
  public
    constructor Create(AContainerViewInfo: TdxSpreadSheetContainerViewInfo); reintroduce; virtual;
    function HasResizeMarkers: Boolean;
    function HasRotateMarker: Boolean;
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; override;
    procedure Invalidate; override;
    //
    property ContainerViewInfo: TdxSpreadSheetContainerViewInfo read FContainerViewInfo;
    property ContentOffsets: TRect read GetContentOffsets;
    property FrameRect: TRect read GetFrameRect;
    property RealDrawingBounds: TRect read GetRealDrawingBounds;
    property RotateMarker: TRect read GetRotateMarker;
    property SizeMarkerRect[Marker: TdxSpreadSheetSizingMarker]: TRect read GetSizeMarkerRect;
    property SizeMarkersArea: TRect read FSizeMarkersArea;
  end;

  { TdxSpreadSheetResizeTableItemDragAndDropObject }

  TdxSpreadSheetResizeTableItemDragAndDropObject = class(TdxSpreadSheetCustomDragAndDropObject)
  strict private
    function GetHitTest: TdxSpreadSheetTableViewHitTest;
    function GetOrientation: TdxOrientation;
    function GetView: TdxSpreadSheetTableView;
  protected
    ItemIndex: Integer;
    Items: TdxSpreadSheetTableItems;
    ItemStartPos: TPoint;
    StartPos: TPoint;

    procedure BeginDragAndDrop; override;
    procedure ChangePosition;
    function CheckPoint(const P: TPoint): TPoint;
    procedure DrawSizingMark(const ARect: TRect);
    function GetDistance(const P1, P2: TPoint): Integer;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetImmediateStart: Boolean; override;
    function GetSizingMarkBounds(const P: TPoint): TRect;
    function IsEntireSheetArea(const R: TRect): Boolean;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function TranslateCoords(const P: TPoint): TPoint; override;
  public
    constructor Create(AControl: TcxControl); override;

    property HitTest: TdxSpreadSheetTableViewHitTest read GetHitTest;
    property Orientation: TdxOrientation read GetOrientation;
    property View: TdxSpreadSheetTableView read GetView;
  end;

  { TdxSpreadSheetTableViewHeaderCellViewInfo }

  TdxSpreadSheetTableViewHeaderCellViewInfo = class(TdxSpreadSheetTableViewCustomCellViewInfo)
  strict private
    FAlignHorz: TAlignment;
    FAlignVert: TcxAlignmentVert;
    FDisplayText: string;
    FIndex: Integer;
    FIsFirst: Boolean;
    FIsLast: Boolean;
    FItem: TdxSpreadSheetTableItem;
    FNeighbors: TcxNeighbors;
    FState: TcxButtonState;

    procedure SetState(AValue: TcxButtonState);
  protected
    FNextNeighbor: TdxSpreadSheetTableViewHeaderCellViewInfo;
    FViewParams: TcxViewParams;

    procedure ApplyBestFit; inline;
    function AllowResize: Boolean; virtual; abstract;
    function DoCustomDraw(ACanvas: TcxCanvas): Boolean; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DoPrepareCanvas(ACanvas: TcxCanvas);
    function GetBorders: TcxBorders; virtual;
    function GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass; override;
    function GetItems: TdxSpreadSheetTableItems; virtual; abstract;
    function GetResizeArea(const AResizeAreaSize: Integer): TRect; virtual; abstract;
    function GetTextColor: TColor; virtual;
    procedure Initialize(AItem: TdxSpreadSheetTableItem; AIndex: Integer; ANeighbors: TcxNeighbors; const ADisplayText: string);
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
    procedure UpdateState; override;

    property Item: TdxSpreadSheetTableItem read FItem;
    property Items: TdxSpreadSheetTableItems read GetItems;
  public
    property AlignHorz: TAlignment read FAlignHorz write FAlignHorz;
    property AlignVert: TcxAlignmentVert read FAlignVert write FAlignVert;
    property Borders: TcxBorders read GetBorders;
    property DisplayText: string read FDisplayText write FDisplayText;
    property Index: Integer read FIndex;
    property IsFirst: Boolean read FIsFirst;
    property IsLast: Boolean read FIsLast;
    property Neighbors: TcxNeighbors read FNeighbors;
    property State: TcxButtonState read FState write SetState;
    property ViewParams: TcxViewParams read FViewParams write FViewParams;
  end;

  { TdxSpreadSheetTableViewColumnHeaderCellViewInfo }

  TdxSpreadSheetTableViewColumnHeaderCellViewInfo = class(TdxSpreadSheetTableViewHeaderCellViewInfo)
  protected
    function AllowResize: Boolean; override;
    function GetItems: TdxSpreadSheetTableItems; override;
    function GetResizeArea(const AResizeAreaSize: Integer): TRect; override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
  public
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; override;
  end;

  { TdxSpreadSheetTableViewHeaderCornerCellViewInfo }

  TdxSpreadSheetTableViewHeaderCornerCellViewInfo = class(TdxSpreadSheetTableViewColumnHeaderCellViewInfo)
  public
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; override;
  end;

  { TdxSpreadSheetTableViewRowHeaderCellViewInfo }

  TdxSpreadSheetTableViewRowHeaderCellViewInfo = class(TdxSpreadSheetTableViewHeaderCellViewInfo)
  protected
    FFirstCellIndex: Integer;
    FLastCellIndex: Integer;

    function AllowResize: Boolean; override;
    function GetItems: TdxSpreadSheetTableItems; override;
    function GetResizeArea(const AResizeAreaSize: Integer): TRect; override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
  public
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; override;
  end;

  { TdxSpreadSheetTableViewCellDataBar }

  TdxSpreadSheetTableViewCellDataBar = class(TdxSpreadSheetCellDataBar);

  { TdxSpreadSheetTableViewCellDisplayStyle }

  TdxSpreadSheetTableViewCellDisplayStyle = class(TdxSpreadSheetCellDisplayStyle)
  strict private
    function GetDataBar: TdxSpreadSheetTableViewCellDataBar;
  protected
    function CreateDataBar: TdxSpreadSheetCellDataBar; override;
  public
    property DataBar: TdxSpreadSheetTableViewCellDataBar read GetDataBar;
  end;

  { TdxSpreadSheetTableViewCustomDataCellViewInfo }

  TdxSpreadSheetTableViewCustomDataCellViewInfo = class(TdxSpreadSheetTableViewCustomCellViewInfo)
  protected
    FAlignHorz: TcxTextAlignX;
    FAlignVert: TcxTextAlignY;
    FBackgroundColor: TColor;
    FCanvasFontPrepared: Boolean;
    FCell: TdxSpreadSheetCell;
    FContentBounds: TRect;
    FDisplayText: string;
    FFontSize: Integer;
    FForegroundColor: TColor;
    FIsNumeric: Boolean;
    FMultiline: Boolean;
    FStyle: TdxSpreadSheetTableViewCellDisplayStyle;
    FStyleViewInfo: TdxSpreadSheetConditionalFormattingStyleViewInfo;
    FTextBounds: TRect;
    FTextColor: TColor;

    procedure CalculateBounds; override;
    procedure CalculateDisplayTextParameters(const R: TRect; ACell: TdxSpreadSheetCell); virtual;
    function CreateStyleViewInfo: TdxSpreadSheetConditionalFormattingStyleViewInfo; virtual;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DrawValue(ACanvas: TcxCanvas); virtual;
    function GetIsEditing: Boolean; virtual;
    function GetTextOutFormat: Integer; inline;
    procedure InitDrawValue; virtual; abstract;
    procedure InitDrawValueCore(ACell: TdxSpreadSheetCell); virtual;
    procedure InitStyle(AStyleHandle: TdxSpreadSheetCellStyleHandle); virtual;
    procedure PrepareCanvasFont(ACanvas: TcxCanvas); virtual;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    procedure Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage); override;
    //
    property AlignHorz: TcxTextAlignX read FAlignHorz write FAlignHorz;
    property AlignVert: TcxTextAlignY read FAlignVert write FAlignVert;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property Cell: TdxSpreadSheetCell read FCell;
    property ContentBounds: TRect read FContentBounds;
    property DisplayText: string read FDisplayText write FDisplayText;
    property FontSize: Integer read FFontSize;
    property ForegroundColor: TColor read FForegroundColor write FForegroundColor;
    property IsEditing: Boolean read GetIsEditing;
    property IsNumeric: Boolean read FIsNumeric;
    property Multiline: Boolean read FMultiline write FMultiline;
    property Style: TdxSpreadSheetTableViewCellDisplayStyle read FStyle;
    property StyleViewInfo: TdxSpreadSheetConditionalFormattingStyleViewInfo read FStyleViewInfo;
    property TextBounds: TRect read FTextBounds;
    property TextColor: TColor read FTextColor write FTextColor;
  end;

  { TdxSpreadSheetTableViewCellViewInfo }

  TdxSpreadSheetTableViewCellViewInfo = class(TdxSpreadSheetTableViewCustomDataCellViewInfo)
  strict private
    FIsLeftmost: Boolean;
    FIsRightmost: Boolean;

    function GetActualCell: TdxSpreadSheetCell;
    function GetActualCellPosition: TPoint;
    function GetBorderColor(ASide: TcxBorder): TColor;
    function GetBorderStyle(ASide: TcxBorder): TdxSpreadSheetCellBorderStyle;
    function GetCommentMarkColor: TColor;
    function GetIsMerged: Boolean; inline;
  protected
    FBorderColor: array[TcxBorder] of TColor;
    FBorderStyle: TdxSpreadSheetCellBordersStyles;
    FCellBounds: TRect;
    FColumn: Integer;
    FComment: TdxSpreadSheetContainer;
    FMergedCell: TdxSpreadSheetTableViewMergedCellViewInfo;
    FRow: Integer;

    procedure CalculateDisplayStyle; virtual;
    procedure CalculateDisplayTextParameters(const R: TRect; ACell: TdxSpreadSheetCell); override;
    function ContentRect(const R: TRect): TRect; inline;
    procedure DoCalculate; override;
    function DoCustomDraw(ACanvas: TcxCanvas): Boolean; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawBorders(ACanvas: TcxCanvas); virtual;
    procedure DrawCommentMark(ACanvas: TcxCanvas); virtual;
    function GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass; override;
    function GetIsEditing: Boolean; override;
    function HasBorder(ASide: TcxBorder): Boolean; inline;
    function HasValue: Boolean; inline;

    procedure MergeBorder(ASide: TcxBorder; ANeighborStyle: TdxSpreadSheetCellStyleHandle);
    procedure RemoveBorder(ASide: TcxBorder); inline;
    procedure SetBorderStyle(ASide: TcxBorder; AStyle: TdxSpreadSheetCellBorderStyle; AColor: TColor);

    procedure InitDrawValue; override;
    procedure InitDrawValueCore(ACell: TdxSpreadSheetCell); override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
    function IsInternalBorder(ASide: TcxBorder): Boolean; inline;
    function IsPermanent: Boolean; override;
    function IsTextOutOfBounds: Boolean; inline;
    procedure UpdateSupportedDrawingStages; virtual;

    property ActualCell: TdxSpreadSheetCell read GetActualCell;
    property ActualCellPosition: TPoint read GetActualCellPosition;
  public
    constructor Create(AOwner: TObject); override;
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; override;
    procedure SetBounds(const AAbsoluteBounds, AScreenClipRect: TRect); override;

    property BorderColor[ASide: TcxBorder]: TColor read GetBorderColor;
    property BorderStyle[ASide: TcxBorder]: TdxSpreadSheetCellBorderStyle read GetBorderStyle;
    property CellBounds: TRect read FCellBounds;
    property Column: Integer read FColumn;
    property Comment: TdxSpreadSheetContainer read FComment;
    property CommentMarkColor: TColor read GetCommentMarkColor;
    property IsLeftmost: Boolean read FIsLeftmost;
    property IsMerged: Boolean read GetIsMerged;
    property IsRightmost: Boolean read FIsRightmost;
    property MergedCell: TdxSpreadSheetTableViewMergedCellViewInfo read FMergedCell;
    property Row: Integer read FRow;
  end;

  { TdxSpreadSheetTableViewCellViewInfoList }

  TdxSpreadSheetTableViewCellViewInfoList = class(TdxSpreadSheetCellViewInfoList)
  strict private
    FIndex: TDictionary<Int64, TdxSpreadSheetTableViewCellViewInfo>;

    function GetItem(Index: Integer): TdxSpreadSheetTableViewCellViewInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AViewInfo: TdxSpreadSheetCellViewInfo); override;
    procedure Clear; override;
    function FindItemForCell(ACell: TdxSpreadSheetCell; out AViewInfo: TdxSpreadSheetTableViewCellViewInfo): Boolean; overload;
    function FindItemForCell(ARow, AColumn: Integer; out AViewInfo: TdxSpreadSheetTableViewCellViewInfo): Boolean; overload;
    //
    property Items[Index: Integer]: TdxSpreadSheetTableViewCellViewInfo read GetItem; default;
  end;

  { TdxSpreadSheetTableViewMergedCellViewInfo }

  TdxSpreadSheetTableViewMergedCellViewInfo = class(TdxSpreadSheetTableViewCustomDataCellViewInfo)
  strict private
    FMergedCell: TdxSpreadSheetMergedCell;

    function GetArea: TRect; inline;
  protected
    procedure CalculateDisplayTextParameters(const R: TRect; ACell: TdxSpreadSheetCell); override;
    procedure InitDrawValue; override;
  public
    constructor Create(AViewInfo: TdxSpreadSheetTableViewInfo; ACell: TdxSpreadSheetMergedCell); reintroduce; virtual;

    property Area: TRect read GetArea;
    property MergedCell: TdxSpreadSheetMergedCell read FMergedCell;
  end;

  { TdxSpreadSheetTableViewMergedCellViewInfoList }

  TdxSpreadSheetTableViewMergedCellViewInfoList = class(TdxSpreadSheetCellViewInfoList)
  strict private
    FIndex: TDictionary<TdxSpreadSheetMergedCell, TdxSpreadSheetTableViewMergedCellViewInfo>;

    function GetItem(Index: Integer): TdxSpreadSheetTableViewMergedCellViewInfo; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AViewInfo: TdxSpreadSheetCellViewInfo); override;
    procedure Clear; override;
    function Contains(ACell: TdxSpreadSheetMergedCell): Boolean; inline;
    function TryGetValue(ACell: TdxSpreadSheetMergedCell; out AViewInfo: TdxSpreadSheetTableViewMergedCellViewInfo): Boolean; inline;
    //
    property Items[Index: Integer]: TdxSpreadSheetTableViewMergedCellViewInfo read GetItem; default;
  end;

  { TdxSpreadSheetTableViewFrozenPaneSeparatorCellViewInfo }

  TdxSpreadSheetTableViewFrozenPaneSeparatorCellViewInfo = class(TdxSpreadSheetTableViewCustomCellViewInfo)
  strict private
    FColor: TColor;
  protected
    function DoCustomDraw(ACanvas: TcxCanvas): Boolean; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
  public
    property Color: TColor read FColor write FColor;
  end;

  { TdxSpreadSheetPageControlHitTest }

  TdxSpreadSheetPageControlHitTest = class(TdxSpreadSheetCustomHitTest)
  strict private
    function GetPageControl: TdxSpreadSheetPageControl;
  public
    procedure Calculate(const AHitPoint: TPoint); override;

    property HitAtBackground: Boolean index hcBackground read GetHitCode;
    property HitAtButton: Boolean index hcButton read GetHitCode;
    property HitAtPageTab: Boolean index hcPageTab read GetHitCode;
    property HitAtSplitter: Boolean index hcSplitter read GetHitCode;
    property PageControl: TdxSpreadSheetPageControl read GetPageControl;
  end;

  { TdxSpreadSheetPageControlController }

  TdxSpreadSheetPageControlController = class(TdxSpreadSheetCustomController)
  strict private
    FPressedObject: TdxSpreadSheetCellViewInfo;

    function GetPageControlHitTest: TdxSpreadSheetPageControlHitTest; inline;
    function GetPageControl: TdxSpreadSheetPageControl;
    procedure SetPressedObject(AValue: TdxSpreadSheetCellViewInfo);
  protected
    function GetHitTest: TdxSpreadSheetCustomHitTest; override;
    function GetSpreadSheet: TdxCustomSpreadSheet; override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UpdateStates; override;
  public
    property HitTest: TdxSpreadSheetPageControlHitTest read GetPageControlHitTest;
    property PageControl: TdxSpreadSheetPageControl read GetPageControl;
    property PressedObject: TdxSpreadSheetCellViewInfo read FPressedObject write SetPressedObject;
  end;

  { TdxSpreadSheetPageControlBackgroundCellViewInfo }

  TdxSpreadSheetPageControlBackgroundCellViewInfo = class(TdxSpreadSheetCellViewInfo)
  strict private
    FState: TcxButtonState;
    FViewParams: TcxViewParams;

    function GetPageControl: TdxSpreadSheetPageControl; inline;
    procedure SetState(AValue: TcxButtonState);
  protected
    function DoCustomDraw(ACanvas: TcxCanvas): Boolean; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetHitCode: Integer; virtual;
    function GetSpreadSheet: TdxCustomSpreadSheet; override;
    function GetState: TcxButtonState; virtual;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
    procedure InitViewParams; virtual;
    procedure UpdateState; override;

    property State: TcxButtonState read GetState write SetState;
  public
    constructor Create(AOwner: TObject); override;

    property PageControl: TdxSpreadSheetPageControl read GetPageControl;
    property ViewParams: TcxViewParams read FViewParams write FViewParams;
  end;

  { TdxSpreadSheetPageControlButtonCellViewInfo }

  TdxSpreadSheetPageControlButtonCellViewInfo = class(TdxSpreadSheetPageControlBackgroundCellViewInfo)
  strict private
    FButtonType: TdxSpreadSheetPageControlButton;
  protected
    procedure DoClick; virtual;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetHitCode: Integer; override;
    procedure Initialize(const ABounds, AClipRect: TRect; AType: TdxSpreadSheetPageControlButton);
    procedure UpdateState; override;
  public
    property State;
    property ButtonType: TdxSpreadSheetPageControlButton read FButtonType;
  end;

  { TdxSpreadSheetPageControlTabDragAndDropObject }

  TdxSpreadSheetPageControlTabDragAndDropObject = class(TcxDragAndDropObject)
  strict private
    function GetLeftScrollArea: TRect;
    function GetHitTest: TdxSpreadSheetPageControlHitTest;
    function GetHitTestView: TdxSpreadSheetCustomView;
    function GetPageControl: TdxSpreadSheetPageControl;
    function GetRightScrollArea: TRect;
  protected
    Arrows: TcxPlaceArrows;
    DropIndex: Integer;
    ScrollDelta: Integer;
    ScrollTimer: TcxTimer;
    View: TdxSpreadSheetCustomView;

    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    procedure InitScrollTimer(ADelta: Integer);
    procedure ScrollTimerHandler(ASender: TObject);
  public
    constructor Create(AControl: TcxControl); override;
    destructor Destroy; override;

    property HitTest: TdxSpreadSheetPageControlHitTest read GetHitTest;
    property HitTestView: TdxSpreadSheetCustomView read GetHitTestView;
    property LeftScrollArea: TRect read GetLeftScrollArea;
    property PageControl: TdxSpreadSheetPageControl read GetPageControl;
    property RightScrollArea: TRect read GetRightScrollArea;
  end;

  { TdxSpreadSheetPageControlTabCellViewInfo }

  TdxSpreadSheetPageControlTabCellViewInfo = class(TdxSpreadSheetPageControlBackgroundCellViewInfo)
  strict private
    FView: TdxSpreadSheetCustomView;
  protected
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass; override;
    function GetHitCode: Integer; override;
    function GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass; override;
    function GetState: TcxButtonState; override;
    class function GetTextMargins: TRect; virtual;
    procedure Initialize(const ABounds, AClipRect: TRect; AView: TdxSpreadSheetCustomView);
  public
    constructor Create(AOwner: TObject); override;

    property State;
    property View: TdxSpreadSheetCustomView read FView;
  end;

  { TdxSpreadSheetPageControlSplitterCellViewInfo }

  TdxSpreadSheetPageControlSplitterCellViewInfo = class(TdxSpreadSheetPageControlBackgroundCellViewInfo)
  protected
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass; override;
    function GetHitCode: Integer; override;
    function GetState: TcxButtonState; override;
    function MeasureWidth: Integer; virtual;
  public
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; override;
    //
    property State;
  end;

  { TdxSpreadSheetPageControlSplitterDragAndDropObject }

  TdxSpreadSheetPageControlSplitterDragAndDropObject = class(TcxDragAndDropObject)
  strict private
    StartPos: TPoint;
    StartWidth: Integer;
    function GetPageControl: TdxSpreadSheetPageControl;
  protected
    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetImmediateStart: Boolean; override;
  public
    property PageControl: TdxSpreadSheetPageControl read GetPageControl;
  end;

  { TdxSpreadSheetPageControlViewInfo }

  TdxSpreadSheetPageControlViewInfo = class
  strict private
    FCells: TdxSpreadSheetCellViewInfoList;
    FClipRect: TRect;
    FLastVisiblePageIndex: Integer;
    FLeftScrollArea: TRect;
    FPageControl: TdxSpreadSheetPageControl;
    FRightScrollArea: TRect;
    FViewParams: TcxViewParams;

    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; inline;
    function GetScaleFactor: TdxScaleFactor; inline;
    function GetSpreadSheet: TdxCustomSpreadSheet; inline;
  protected
    Bounds: TRect;
    Pages: TList<TdxSpreadSheetCustomView>;

    procedure AddButton(var R: TRect; AType: TdxSpreadSheetPageControlButton); virtual;
    procedure AddPageTab(var R: TRect; APage: TdxSpreadSheetCustomView); virtual;
    procedure AddCells; virtual;
    function CreateSplitterCellViewInfo:  TdxSpreadSheetPageControlSplitterCellViewInfo; virtual;
    function CreateTabCellViewInfo: TdxSpreadSheetPageControlTabCellViewInfo; virtual;
    procedure InitializeViewParams; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetPageControl); virtual;
    destructor Destroy; override;
    procedure Calculate; virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    procedure InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest); virtual;
    function MeasureHeight: Integer; virtual;
    function MeasureWidth: Integer; virtual;

    property Cells: TdxSpreadSheetCellViewInfoList read FCells;
    property LastVisiblePageIndex: Integer read FLastVisiblePageIndex;
    property LeftScrollArea: TRect read FLeftScrollArea;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property PageControl: TdxSpreadSheetPageControl read FPageControl;
    property RightScrollArea: TRect read FRightScrollArea;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
    property ViewParams: TcxViewParams read FViewParams write FViewParams;
  end;

  { TdxSpreadSheetPageControl }

  TdxSpreadSheetPageControl = class(TdxSpreadSheetPersistentObject)
  strict private
    FButtons: TdxSpreadSheetPageControlButtons;
    FController: TdxSpreadSheetPageControlController;
    FDropArrowColor: TColor;
    FFirstVisiblePageIndex: Integer;
    FHitTest: TdxSpreadSheetPageControlHitTest;
    FVisible: Boolean;
    FViewInfo: TdxSpreadSheetPageControlViewInfo;
    FWidth: Integer;

    function GetBounds: TRect;
    function GetFirstVisiblePageIndex: Integer;
    function GetVisiblePage(AIndex: Integer): TdxSpreadSheetCustomView;
    function GetVisiblePageCount: Integer;
    function IsButtonsStored: Boolean;
    procedure SetButtons(AValue: TdxSpreadSheetPageControlButtons);
    procedure SetFirstVisiblePageIndex(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure Changed; virtual;
    function CreateController: TdxSpreadSheetPageControlController; virtual;
    function CreateHitTest: TdxSpreadSheetPageControlHitTest; virtual;
    function CreateViewInfo: TdxSpreadSheetPageControlViewInfo; virtual;
    procedure Invalidate;
    procedure Recalculate;
    //
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;

    property Controller: TdxSpreadSheetPageControlController read FController;
    property ViewInfo: TdxSpreadSheetPageControlViewInfo read FViewInfo;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure MakeVisible(APage: TdxSpreadSheetCustomView);

    property Bounds: TRect read GetBounds;
    property HitTest: TdxSpreadSheetPageControlHitTest read FHitTest;
    property VisiblePageCount: Integer read GetVisiblePageCount;
    property VisiblePages[Index: Integer]: TdxSpreadSheetCustomView read GetVisiblePage;
  published
    property Buttons: TdxSpreadSheetPageControlButtons read FButtons write SetButtons stored IsButtonsStored;
    property DropArrowColor: TColor read FDropArrowColor write FDropArrowColor default clGreen;
    property FirstVisiblePageIndex: Integer read GetFirstVisiblePageIndex write SetFirstVisiblePageIndex default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth default 0;
  end;

  { TdxSpreadSheetViewInfo }

  TdxSpreadSheetViewInfo = class(TdxSpreadSheetPersistentObject)
  strict private
    function GetBackgroundParams: TcxViewParams;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
  public
    property BackgroundParams: TcxViewParams read GetBackgroundParams;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  end;

  { TdxSpreadSheetStyles }

  TdxSpreadSheetStyles = class(TcxStyles)
  strict private
    function GetSpreadSheet: TdxCustomSpreadSheet;
  protected
    procedure Changed(AIndex: Integer); override;
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetBackgroundStyle: TcxViewParams;
    function GetContentStyle(AView: TdxSpreadSheetCustomView): TcxViewParams;
    function GetHeaderStyle(AView: TdxSpreadSheetCustomView): TcxViewParams;
    function GetPageControlStyle: TcxViewParams;
    function GetSelectionStyle: TcxViewParams;

    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  published
    property Background: TcxStyle index s_Background read GetValue write SetValue;
    property Content: TcxStyle index s_Content read GetValue write SetValue;
    property Header: TcxStyle  index s_Header read GetValue write SetValue;
    property PageControl: TcxStyle index s_PageControl read GetValue write SetValue;
    property Selection: TcxStyle index s_Selection read GetValue write SetValue;
  end;

  { TdxSpreadSheetOptionsBehavior }

  TdxSpreadSheetOptionsBehavior = class(TdxSpreadSheetPersistentObject)
  strict private
    FAutomaticCalculation: Boolean;
    FDeleting: Boolean;
    FDragFilling: Boolean;
    FDragMoving: Boolean;
    FEditing: Boolean;
    FEnterKeyNavigation: TdxSpreadSheetEnterKeyNavigation;
    FFormatting: Boolean;
    FFormulaAutoComplete: Boolean;
    FFormulaAutoCompleteShowHint: Boolean;
    FHistory: Boolean;
    FInserting: Boolean;
    FIterativeCalculation: Boolean;
    FIterativeCalculationMaxCount: Integer;

    function GetProtected: Boolean;
    procedure SetHistory(AValue: Boolean);
    procedure SetIterativeCalculationMaxCount(AValue: Integer);
    procedure SetProtected(AValue: Boolean);
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutomaticCalculation: Boolean read FAutomaticCalculation write FAutomaticCalculation default True;
    property Deleting: Boolean read FDeleting write FDeleting default True;
    property DragFilling: Boolean read FDragFilling write FDragFilling default True;
    property DragMoving: Boolean read FDragMoving write FDragMoving default True;
    property Editing: Boolean read FEditing write FEditing default True;
    property EnterKeyNavigation: TdxSpreadSheetEnterKeyNavigation read FEnterKeyNavigation write FEnterKeyNavigation default eknDefault;
    property FormulaAutoComplete: Boolean read FFormulaAutoComplete write FFormulaAutoComplete default True;
    property FormulaAutoCompleteShowHint: Boolean read FFormulaAutoCompleteShowHint write FFormulaAutoCompleteShowHint default True;
    property Formatting: Boolean read FFormatting write FFormatting default True;
    property History: Boolean read FHistory write SetHistory default False;
    property Inserting: Boolean read FInserting write FInserting default True;
    property IterativeCalculation: Boolean read FIterativeCalculation write FIterativeCalculation default False;
    property IterativeCalculationMaxCount: Integer read FIterativeCalculationMaxCount write SetIterativeCalculationMaxCount default 0;
    property Protected: Boolean read GetProtected write SetProtected stored False; // obsolete
  end;

  { TdxSpreadSheetOptionsView }

  TdxSpreadSheetOptionsView = class(TdxSpreadSheetPersistentObject)
  strict private
    FAntialiasing: Boolean;
    FCellAutoHeight: Boolean;
    FCommentMarkColor: TColor;
    FDateTimeSystem: TdxSpreadSheetDateTimeSystem;
    FFrozenPaneSeparatorColor: TColor;
    FFrozenPaneSeparatorWidth: Integer;
    FGridLineColor: TColor;
    FGridLines: Boolean;
    FHeaders: Boolean;
    FHorizontalScrollBar: Boolean;
    FPrintAreaColor: TColor;
    FPrintAreas: Boolean;
    FR1C1Reference: Boolean;
    FShowFormulas: Boolean;
    FVerticalScrollBar: Boolean;
    FZeroValues: Boolean;

    function GetActualDateTimeSystem: TdxSpreadSheetDateTimeSystem;
    procedure SetAntialiasing(const AValue: Boolean);
    procedure SetCellAutoHeight(const Value: Boolean);
    procedure SetCommentMarkColor(AValue: TColor);
    procedure SetDateTimeSystem(AValue: TdxSpreadSheetDateTimeSystem);
    procedure SetFrozenPaneSeparatorColor(AValue: TColor);
    procedure SetFrozenPaneSeparatorWidth(AValue: Integer);
    procedure SetGridLineColor(AValue: TColor);
    procedure SetGridLines(AValue: Boolean);
    procedure SetHeaders(AValue: Boolean);
    procedure SetHorizontalScrollBar(AValue: Boolean);
    procedure SetPrintAreaColor(const Value: TColor);
    procedure SetPrintAreas(AValue: Boolean);
    procedure SetR1C1Reference(AValue: Boolean);
    procedure SetShowFormulas(AValue: Boolean);
    procedure SetVerticalScrollBar(AValue: Boolean);
    procedure SetZeroValues(AValue: Boolean);
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); override;
    procedure Assign(Source: TPersistent); override;

    property ActualDateTimeSystem: TdxSpreadSheetDateTimeSystem read GetActualDateTimeSystem;
  published
    property Antialiasing: Boolean read FAntialiasing write SetAntialiasing default True;
    property CellAutoHeight: Boolean read FCellAutoHeight write SetCellAutoHeight default True;
    property CommentMarkColor: TColor read FCommentMarkColor write SetCommentMarkColor default clDefault;
    property DateTimeSystem: TdxSpreadSheetDateTimeSystem read FDateTimeSystem write SetDateTimeSystem default dts1900;
    property FrozenPaneSeparatorColor: TColor read FFrozenPaneSeparatorColor write SetFrozenPaneSeparatorColor default clDefault;
    property FrozenPaneSeparatorWidth: Integer read FFrozenPaneSeparatorWidth write SetFrozenPaneSeparatorWidth default 1;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clDefault;
    property GridLines: Boolean read FGridLines write SetGridLines default True;
    property Headers: Boolean read FHeaders write SetHeaders default True;
    property HorizontalScrollBar: Boolean read FHorizontalScrollBar write SetHorizontalScrollBar default True;
    property PrintAreaColor: TColor read FPrintAreaColor write SetPrintAreaColor default clDefault;
    property PrintAreas: Boolean read FPrintAreas write SetPrintAreas default False;
    property R1C1Reference: Boolean read FR1C1Reference write SetR1C1Reference default False;
    property ShowFormulas: Boolean read FShowFormulas write SetShowFormulas default False;
    property VerticalScrollBar: Boolean read FVerticalScrollBar write SetVerticalScrollBar default True;
    property ZeroValues: Boolean read FZeroValues write SetZeroValues default True;
  end;

  { TdxSpreadSheetHistoryCustomCommand }

  TdxSpreadSheetHistoryCustomCommand = class
  private
    FOwner: TdxSpreadSheetHistoryAction;

    function GetView: TdxSpreadSheetCustomView;
  protected
    procedure Initialize; virtual;
  public
    class function ActionClass: TdxSpreadSheetHistoryActionClass; virtual;
    function CompatibleWith(ACommand: TdxSpreadSheetHistoryCustomCommand): Boolean; virtual;
    procedure Redo; virtual;
    procedure Undo; virtual;

    property Owner: TdxSpreadSheetHistoryAction read FOwner;
    property View: TdxSpreadSheetCustomView read GetView;
  end;

  { TdxSpreadSheetHistoryAction }

  TdxSpreadSheetHistoryAction = class
  strict private
    FCommands: TcxObjectList;
    FData: TStream;
    FHistory: TdxSpreadSheetHistory;
    FModified: Boolean;
    FView: TdxSpreadSheetCustomView;

    function GetCount: Integer;
    function GetCommand(AIndex: Integer): TdxSpreadSheetHistoryCustomCommand;
    function GetSpreadSheet: TdxCustomSpreadSheet;
  protected
    function AcceptCommand(ACommand: TdxSpreadSheetHistoryCustomCommand): Boolean; virtual;
    procedure AddCommand(ACommand: TdxSpreadSheetHistoryCustomCommand);
    procedure DoRedo; virtual;
    procedure DoUndo; virtual;
    class function GetDescription: string; virtual;
    procedure RestoreSelection(ANewData: TStream);
    function StoreSelection: TStream;

    property Commands[Index: Integer]: TdxSpreadSheetHistoryCustomCommand read GetCommand;
    property Count: Integer read GetCount;
    property History: TdxSpreadSheetHistory read FHistory;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
    property View: TdxSpreadSheetCustomView read FView;
  public
    constructor Create(AOwner: TdxSpreadSheetHistory); virtual;
    destructor Destroy; override;
    procedure Redo;
    procedure Undo;

    property Description: string read GetDescription;
  end;

  { TdxSpreadSheetHistoryEditCellAction }

  TdxSpreadSheetHistoryEditCellAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryEditCommentAction }

  TdxSpreadSheetHistoryEditCommentAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryExpandGroupAction }

  TdxSpreadSheetHistoryExpandGroupAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryMergeCellsAction }

  TdxSpreadSheetHistoryMergeCellsAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryClearCellsAction }

  TdxSpreadSheetHistoryClearCellsAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryDeleteCellsAction }

  TdxSpreadSheetHistoryDeleteCellsAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryDeleteCommentsAction }

  TdxSpreadSheetHistoryDeleteCommentsAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryFillCellsAction }

  TdxSpreadSheetHistoryFillCellsAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryMoveCellsAction }

  TdxSpreadSheetHistoryMoveCellsAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryInsertCellsAction }

  TdxSpreadSheetHistoryInsertCellsAction = class(TdxSpreadSheetHistoryAction)
  protected
    function AcceptCommand(ACommand: TdxSpreadSheetHistoryCustomCommand): Boolean; override;
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryFormatCellAction }

  TdxSpreadSheetHistoryFormatCellAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryReplaceAction }

  TdxSpreadSheetHistoryReplaceAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistorySortingAction }

  TdxSpreadSheetHistorySortingAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryCutToClipboardAction }

  TdxSpreadSheetHistoryCutToClipboardAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryPasteFromClipboardAction }

  TdxSpreadSheetHistoryPasteFromClipboardAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryAddGroupAction }

  TdxSpreadSheetHistoryAddGroupAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryChangeGroupAction }

  TdxSpreadSheetHistoryChangeGroupAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryDeleteGroupAction }

  TdxSpreadSheetHistoryDeleteGroupAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryDragAndDropAction }

  TdxSpreadSheetHistoryDragAndDropAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryChangeRowColumnItemAction }

  TdxSpreadSheetHistoryChangeRowColumnItemAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryChangeContainerAction }

  TdxSpreadSheetHistoryChangeContainerAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryChangeConditionalFormattingAction }

  TdxSpreadSheetHistoryChangeConditionalFormattingAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryChangePrintingOptionsAction }

  TdxSpreadSheetHistoryChangePrintingOptionsAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryCreateDefinedNameAction }

  TdxSpreadSheetHistoryCreateDefinedNameAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistory }

  TdxSpreadSheetHistory = class(TdxSpreadSheetPersistentObject)
  strict private
    FCurrentAction: TdxSpreadSheetHistoryAction;
    FInProcessRefCount: Integer;
    FLockCount: Integer;
    FRedoActions: TcxObjectList;
    FUndoActions: TcxObjectList;

    function GetInProcess: Boolean;
    function GetIsLocked: Boolean;
    function GetRedoAction(AIndex: Integer): TdxSpreadSheetHistoryAction;
    function GetRedoActionCount: Integer;
    function GetUndoAction(AIndex: Integer): TdxSpreadSheetHistoryAction;
    function GetUndoActionCount: Integer;
    procedure SetInProcess(AValue: Boolean);
  protected
    FActionLockCount: Integer;

    property CurrentAction: TdxSpreadSheetHistoryAction read FCurrentAction write FCurrentAction;
    property InProcess: Boolean read GetInProcess write SetInProcess;
    property IsLocked: Boolean read GetIsLocked;
    property LockCount: Integer read FLockCount write FLockCount;
    property RedoActionList: TcxObjectList read FRedoActions;
    property UndoActionList: TcxObjectList read FUndoActions;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); override;
    destructor Destroy; override;
    procedure AddCommand(ACommand: TdxSpreadSheetHistoryCustomCommand); inline;
    procedure BeginAction(AActionClass: TdxSpreadSheetHistoryActionClass);
    function CanAddCommand: Boolean;
    procedure EndAction(ACanceled: Boolean = False);
    //
    procedure Clear;
    procedure Lock;
    procedure Unlock;
    procedure Redo(const ARedoCount: Integer = 1);
    procedure Undo(const AUndoCount: Integer = 1);

    property RedoActionCount: Integer read GetRedoActionCount;
    property RedoActions[Index: Integer]: TdxSpreadSheetHistoryAction read GetRedoAction;
    property UndoActionCount: Integer read GetUndoActionCount;
    property UndoActions[Index: Integer]: TdxSpreadSheetHistoryAction read GetUndoAction;
  end;

  { TdxSpreadSheetLockedStatePaintHelper }

  TdxSpreadSheetLockedStatePaintHelper = class(TcxLockedStatePaintHelper)
  strict private
    function GetSpreadSheet: TdxCustomSpreadSheet;
  protected
    function CanCreateLockedImage: Boolean; override;
    function DoPrepareImage: Boolean; override;
    function GetControl: TcxControl; override;
    function GetOptions: TcxLockedStateImageOptions; override;

    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  end;

  { TdxSpreadSheetLockedStateImageOptions }

  TdxSpreadSheetLockedStateImageOptions = class(TcxLockedStateImageOptions)
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;
  protected
    function GetFont: TFont; override;

    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AssignedValues;
    property Color;
    property Effect;
    property Font;
    property ShowText;
    property Text;
  end;

  { TdxCustomSpreadSheet }

  TdxSpreadSheetViewCompareValuesEvent = procedure (Sender: TdxSpreadSheetCustomView;
    const AData1, AData2: TdxSpreadSheetCellData; var Compare: Integer) of object;

  TdxSpreadSheetHyperlinkExecuteEvent = procedure (Sender: TdxCustomSpreadSheet; AHyperlink: TdxSpreadSheetHyperlink; var AHandled: Boolean) of object;

  TdxSpreadSheetPrepareLockedStateImageEvent = procedure(Sender: TdxCustomSpreadSheet; AImage: TcxBitmap32; var ADone: Boolean) of object;
  TdxSpreadSheetScrollEvent = procedure (Sender: TdxCustomSpreadSheet; AScrollBarKind: TScrollBarKind; AFirstScrollablePosition: Integer) of object;
  TdxSpreadSheetViewInitInplaceEditEvent = procedure (Sender: TdxSpreadSheetCustomView; AEdit: TcxCustomEdit) of object;
  TdxSpreadSheetViewInitInplaceEditValueEvent = procedure (Sender: TdxSpreadSheetCustomView; AEdit: TcxCustomEdit; var AValue: TcxEditValue) of object;
  TdxSpreadSheetViewInplaceEditingEvent = procedure (Sender: TdxSpreadSheetCustomView; var AProperties: TcxCustomEditProperties; var AAllow: Boolean) of object;
  TdxSpreadSheetViewNotifyEvent = procedure (Sender: TdxSpreadSheetCustomView) of object;

  TdxSpreadSheetCustomDrawTableViewCellEvent = procedure(Sender: TdxSpreadSheetTableView; ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewCellViewInfo; var AHandled: Boolean) of object;
  TdxSpreadSheetCustomDrawTableViewCommonCellEvent = procedure(Sender: TdxSpreadSheetTableView; ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewCustomCellViewInfo; var AHandled: Boolean) of object;
  TdxSpreadSheetCustomDrawTableViewHeaderCellEvent = procedure(Sender: TdxSpreadSheetTableView; ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewHeaderCellViewInfo; var AHandled: Boolean) of object;

  TdxSpreadSheetTableViewActiveCellChangingEvent = procedure (Sender: TdxSpreadSheetTableView;
    const ANewActiveCell: TPoint; var ACanSelect: Boolean) of object;

  TdxCustomSpreadSheet = class(TcxControl,
    IcxLockedStateFontChanged,
    IcxLockedStatePaint,
    IdxDialogOwner,
    IdxSpreadSheet,
    IdxSpreadSheetCellStyleOwner,
    IdxSpreadSheetConditionalFormatting,
    IdxSkinSupport)
  strict private
    FActiveController: TdxSpreadSheetCustomController;
    FActiveSheetIndex: Integer;
    FCellStyles: TdxSpreadSheetCellStyles;
    FDefaultCellStyle: TdxSpreadSheetDefaultCellStyle;
    FDefinedNames: TdxSpreadSheetDefinedNames;
    FDialogsLookAndFeel: TcxLookAndFeel;
    FExternalLinks: TdxSpreadSheetExternalLinks;
    FFormatSettings: TdxSpreadSheetFormatSettings;
    FFormattedSharedStringCache: TdxSpreadSheetFormattedSharedStringCache;
    FFormulaController:  TdxSpreadSheetFormulaController;
    FHistory: TdxSpreadSheetHistory;
    FListeners: TdxSpreadSheetListeners;
    FLockedStatePaintHelper: TdxSpreadSheetLockedStatePaintHelper;
    FModified: Boolean;
    FOptionsBehavior: TdxSpreadSheetOptionsBehavior;
    FOptionsLockedStateImage: TdxSpreadSheetLockedStateImageOptions;
    FOptionsProtection: TdxSpreadSheetWorkbookProtectionOptions;
    FOptionsView: TdxSpreadSheetOptionsView;
    FPageControl: TdxSpreadSheetPageControl;
    FPassword: string;
    FSharedImages: TdxSpreadSheetSharedImages;
    FStringTable: TdxSpreadSheetSharedStringTable;
    FStyles: TdxSpreadSheetStyles;
    FViewInfo: TdxSpreadSheetViewInfo;

    FClipboardArea: TdxSpreadSheetClipboardArea;
    FIsClipboardListener: Boolean;
    FIsClipboardListenerLocked: Boolean;
    FNextClipboardListener: HWND;

    FChanges: TdxSpreadSheetChanges;
    FLockCount: Integer;
    FProcessingChanges: Boolean;
    FScrollbarsLocked: Boolean;

    FOnActiveCellChanging: TdxSpreadSheetTableViewActiveCellChangingEvent;
    FOnActiveSheetChanged: TNotifyEvent;
    FOnCommentHide: TdxSpreadSheetCellCommentHideEvent;
    FOnCommentShow: TdxSpreadSheetCellCommentShowEvent;
    FOnCompare: TdxSpreadSheetViewCompareValuesEvent;
    FOnCustomDrawTableViewCell: TdxSpreadSheetCustomDrawTableViewCellEvent;
    FOnCustomDrawTableViewCommonCell: TdxSpreadSheetCustomDrawTableViewCommonCellEvent;
    FOnCustomDrawTableViewHeaderCell: TdxSpreadSheetCustomDrawTableViewHeaderCellEvent;
    FOnDataChanged: TNotifyEvent;
    FOnEditChanged: TdxSpreadSheetViewNotifyEvent;
    FOnEdited: TdxSpreadSheetViewNotifyEvent;
    FOnEditing: TdxSpreadSheetViewInplaceEditingEvent;
    FOnEditValueChanged: TdxSpreadSheetViewNotifyEvent;
    FOnGetPassword: TdxGetPasswordEvent;
    FOnHistoryChanged: TNotifyEvent;
    FOnHyperlinkExecute: TdxSpreadSheetHyperlinkExecuteEvent;
    FOnInitEdit: TdxSpreadSheetViewInitInplaceEditEvent;
    FOnInitEditValue: TdxSpreadSheetViewInitInplaceEditValueEvent;
    FOnLayoutChanged: TNotifyEvent;
    FOnModifiedChanged: TNotifyEvent;
    FOnPrepareLockedStateImage: TdxSpreadSheetPrepareLockedStateImageEvent;
    FOnProgress: TdxSpreadSheetProgressEvent;
    FOnScroll: TdxSpreadSheetScrollEvent;
    FOnSelectionChanged: TNotifyEvent;

    function GetActiveSheet: TdxSpreadSheetCustomView;
    function GetActiveSheetAsTable: TdxSpreadSheetTableView;
    function GetIsLocked: Boolean;
    function GetSheet(AIndex: Integer): TdxSpreadSheetCustomView;
    function GetSheetCount: Integer;
    function GetVisibleSheet(Index: Integer): TdxSpreadSheetCustomView;
    function GetVisibleSheetCount: Integer;
    procedure SetActiveSheet(AValue: TdxSpreadSheetCustomView);
    procedure SetActiveSheetIndex(AValue: Integer);
    procedure SetDialogsLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetIsClipboardListener(AValue: Boolean);
    procedure SetModified(AValue: Boolean);
    procedure SetOptionsBehavior(AValue: TdxSpreadSheetOptionsBehavior);
    procedure SetOptionsLockedStateImage(AValue: TdxSpreadSheetLockedStateImageOptions);
    procedure SetOptionsProtection(AValue: TdxSpreadSheetWorkbookProtectionOptions);
    procedure SetOptionsView(AValue: TdxSpreadSheetOptionsView);
    procedure SetPageControl(AValue: TdxSpreadSheetPageControl);
    procedure SetStyle(AValue: TdxSpreadSheetStyles);
    //
    procedure ReadBinaryData(AStream: TStream);
    procedure WriteBinaryData(AStream: TStream);

    procedure ForwardMessage(var AMessage: TMessage);
    procedure WMCancelHint(var Message: TMessage); message DXM_CANCELHINT;
    procedure WMChangeCBChain(var AMessage: TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMDrawClipboard(var AMessage: TWMDrawClipboard); message WM_DRAWCLIPBOARD;
    procedure WMIMEComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WMIMEStartComposition(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMPostHideEdit(var Message: TMessage); message DXM_POSTHIDEEDIT;
  protected
    FDefaultEdit: TcxCustomMemo;
    FSheets: TdxSpreadSheetViewList;
    FState: TdxSpreadSheetStates;
    FVisibleSheets: TList<TdxSpreadSheetCustomView>;

    procedure AddChanges(AChanges: TdxSpreadSheetChanges); virtual;
    procedure AfterLoad; virtual;
    procedure BoundsChanged; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CheckChanges; virtual;
    procedure CheckDestroyedView(AView: TdxSpreadSheetCustomView); virtual;
    procedure ClearClipboard;
    function ControllerFromPoint(const P: TPoint; var AController: TdxSpreadSheetCustomController): Boolean; overload; virtual;
    function ControllerFromPoint(X, Y: Integer; var AController: TdxSpreadSheetCustomController): Boolean; overload;

    function CreateCellStyles: TdxSpreadSheetCellStyles; virtual;
    function CreateDefaultCellStyle: TdxSpreadSheetDefaultCellStyle; virtual;
    function CreateDefinedNames: TdxSpreadSheetDefinedNames; virtual;
    function CreateExternalLinks: TdxSpreadSheetExternalLinks; virtual;
    function CreateFormattedSharedStringCache: TdxSpreadSheetFormattedSharedStringCache; virtual;
    function CreateFormatSettings: TdxSpreadSheetFormatSettings; virtual;
    function CreateFormulaController:  TdxSpreadSheetFormulaController; virtual;
    function CreateHistory:  TdxSpreadSheetHistory; virtual;
    function CreateLockedStatePaintHelper: TdxSpreadSheetLockedStatePaintHelper; virtual;
    function CreateOptionsBehavior: TdxSpreadSheetOptionsBehavior; virtual;
    function CreateOptionsLockedStateImage: TdxSpreadSheetLockedStateImageOptions; virtual;
    function CreateOptionsProtection: TdxSpreadSheetWorkbookProtectionOptions; virtual;
    function CreateOptionsView: TdxSpreadSheetOptionsView; virtual;
    function CreatePageControl: TdxSpreadSheetPageControl; virtual;
    function CreateSharedImages: TdxSpreadSheetSharedImages; virtual;
    function CreateSharedStringTable: TdxSpreadSheetSharedStringTable; virtual;
    function CreateStyles: TdxSpreadSheetStyles; virtual;
    function CreateViewInfo: TdxSpreadSheetViewInfo; virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;

    procedure DefineProperties(Filer: TFiler); override;
    procedure FontChanged; override;

    procedure DefinedNamesChanged; virtual;
    function DoActiveCellChanging(AView: TdxSpreadSheetTableView; const ANewActiveCell: TPoint): Boolean; virtual;
    procedure DoActiveSheetChanged; virtual;
    procedure DoCompare(AView: TdxSpreadSheetCustomView; const AData1, AData2: TdxSpreadSheetCellData; var Compare: Integer); virtual;
    procedure DoCommentHide(AContainer: TdxSpreadSheetContainer); virtual;
    function DoCommentShow(AContainer: TdxSpreadSheetContainer): Boolean; virtual;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoDataChanged; virtual;
    procedure DoModifiedChanged; virtual;
    procedure DoEditChanged(AView: TdxSpreadSheetCustomView); virtual;
    procedure DoEdited(AView: TdxSpreadSheetCustomView); virtual;
    procedure DoEditing(AView: TdxSpreadSheetCustomView; var AProperties: TcxCustomEditProperties; var ACanEdit: Boolean); virtual;
    procedure DoEditValueChanged(AView: TdxSpreadSheetCustomView); virtual;
    procedure DoHistoryChanged;
    function DoHyperlinkExecute(ALink: TdxSpreadSheetHyperlink): Boolean; virtual;
    procedure DoInitEdit(AView: TdxSpreadSheetCustomView; AEdit: TcxCustomEdit); virtual;
    procedure DoInitEditValue(AView: TdxSpreadSheetCustomView; AEdit: TcxCustomEdit; var AValue: Variant); virtual;
    function DoGetPassword(var APassword: string): Boolean; virtual;
    procedure DoLayoutChanged; virtual;
    procedure DoOptionsProtectionChanged(Sender: TObject); virtual;
    procedure DoScroll(AScrollBarKind: TScrollBarKind; AFirstScrollablePosition: Integer);
    procedure DoSelectionChanged(AView: TdxSpreadSheetCustomView); virtual;

    procedure DoAddSheet(ASheet: TdxSpreadSheetCustomView);
    procedure DoChangeSheetVisibility(ASheet: TdxSpreadSheetCustomView);
    procedure DoRemoveSheet(ASheet: TdxSpreadSheetCustomView);

    function DoCreateSheet(var ASheet: TdxSpreadSheetCustomView; const ACaption: string = ''; AViewClass: TdxSpreadSheetCustomViewClass = nil): Boolean; virtual;
    function InsertSheet(AIndex: Integer = -1; const ACaption: string = ''; AViewClass: TdxSpreadSheetCustomViewClass = nil): TdxSpreadSheetCustomView;
    function GetSheetByGUID(const AGUID: string): TdxSpreadSheetTableView;
    function AllowTouchScrollUIMode: Boolean; override;
    function CanFocusOnClick: Boolean; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoPaint; override;
    function GetClientBounds: TRect; override;
    function GetCurrentCursor(X, Y: Integer): TCursor; override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    function GetHScrollBarBounds: TRect; override;
    function GetScrollContentForegroundColor: TColor; override;
    function GetSizeGripBounds: TRect; override;
    function GetVScrollBarBounds: TRect; override;
    procedure InitScrollBarsParameters; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function IsSizeGripVisible: Boolean; override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;

    // Custom Draw
    procedure DoCustomDrawTableViewCell(Sender: TdxSpreadSheetTableView; ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewCellViewInfo; var AHandled: Boolean); virtual;
    procedure DoCustomDrawTableViewCommonCell(Sender: TdxSpreadSheetTableView; ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewCustomCellViewInfo; var AHandled: Boolean); virtual;
    procedure DoCustomDrawTableViewHeaderCell(Sender: TdxSpreadSheetTableView; ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewHeaderCellViewInfo; var AHandled: Boolean); virtual;
    function IsTableViewCellCustomDrawn: Boolean; virtual;

    // Keyboard
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure DoKeyPress(var Key: Char); virtual;
    procedure DoKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    // Mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Pack;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure SetPaintRegion; override;
    procedure SetSheetIndex(AView: TdxSpreadSheetCustomView; AIndex: Integer); virtual;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure StopEditing;
    procedure StyleChanged(Sender: TObject);
    procedure UnprotectCore(AOptions: TdxSpreadSheetCustomProtectionOptions);
    procedure UpdateCursor;
    procedure UpdateHitTests(const P: TPoint); overload;
    procedure UpdateHitTests(X, Y: Integer); overload; inline;
    procedure UpdateStates;
    procedure UpdateVisibleSheetList;
    function ValidateSheetCaption(ACaption: string): string;

    procedure LockClipboardListener;
    procedure UnlockClipboardListener;

    function DoPrepareLockedStateImage: Boolean; virtual;
    // IdxDialogOwner
    function IdxDialogOwner.GetOwner = DialogsGetOwner;
    function IdxDialogOwner.GetParentForm = DialogsGetParentForm;
    function IdxDialogOwner.GetLookAndFeel = DialogsGetLookAndFeel;
    function DialogsGetLookAndFeel: TcxLookAndFeel;
    function DialogsGetOwner: TComponent;
    function DialogsGetParentForm: TCustomForm;
    // IcxLockedStatePaint
    function IcxLockedStatePaint.GetImage = GetLockedStateImage;
    function IcxLockedStatePaint.GetTopmostControl = GetLockedStateTopmostControl;
    function GetLockedStateImage: TcxBitmap32;
    function GetLockedStateTopmostControl: TcxControl;
    // IcxLockedStateFontChanged
    procedure IcxLockedStateFontChanged.FontChanged = UpdateLockedStateFont;
    procedure UpdateLockedStateFont(AFont: TFont);
    // IdxSpreadSheet
    function GetControl: TdxCustomSpreadSheet;
    // IdxSpreadSheetCellStyleOwner
    procedure CellStyleChanged;
    procedure CellStyleChanging;
    function GetCellStyles: TdxSpreadSheetCellStyles;
    function GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
    procedure ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);
    // IdxSpreadSheetConditionalFormatting
    function GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;

    procedure InternalLoadFromStream(AStream: TStream; AFormat: TdxSpreadSheetCustomFormatClass;
      AProgressEvent: TdxSpreadSheetProgressEvent); virtual;
    procedure InternalSaveToStream(AStream: TStream; AFormat: TdxSpreadSheetCustomFormatClass;
      AProgressEvent: TdxSpreadSheetProgressEvent); virtual;
    //
    property ActiveController: TdxSpreadSheetCustomController read FActiveController write FActiveController;
    property Changes: TdxSpreadSheetChanges read FChanges write FChanges;
    property ClipboardArea: TdxSpreadSheetClipboardArea read FClipboardArea;
    property FormattedSharedStringCache: TdxSpreadSheetFormattedSharedStringCache read FFormattedSharedStringCache;
    property FormatSettings: TdxSpreadSheetFormatSettings read FFormatSettings write FFormatSettings;
    property IsClipboardListener: Boolean read FIsClipboardListener write SetIsClipboardListener;
    property IsLocked: Boolean read GetIsLocked;
    property Listeners: TdxSpreadSheetListeners read FListeners;
    property LockCount: Integer read FLockCount write FLockCount;
    property LockedStatePaintHelper: TdxSpreadSheetLockedStatePaintHelper read FLockedStatePaintHelper;
    property ProcessingChanges: Boolean read FProcessingChanges write FProcessingChanges;
    property SharedImages: TdxSpreadSheetSharedImages read FSharedImages;
    property State: TdxSpreadSheetStates read FState;
    property StringTable: TdxSpreadSheetSharedStringTable read FStringTable;
    property ViewInfo: TdxSpreadSheetViewInfo read FViewInfo;
    //
    property OnActiveCellChanging: TdxSpreadSheetTableViewActiveCellChangingEvent read FOnActiveCellChanging write FOnActiveCellChanging;
    property OnActiveSheetChanged: TNotifyEvent read FOnActiveSheetChanged write FOnActiveSheetChanged;
    property OnCommentHide: TdxSpreadSheetCellCommentHideEvent read FOnCommentHide write FOnCommentHide;
    property OnCommentShow: TdxSpreadSheetCellCommentShowEvent read FOnCommentShow write FOnCommentShow;
    property OnCompare: TdxSpreadSheetViewCompareValuesEvent read FOnCompare write FOnCompare;
    property OnCustomDrawTableViewCell: TdxSpreadSheetCustomDrawTableViewCellEvent read FOnCustomDrawTableViewCell write FOnCustomDrawTableViewCell;
    property OnCustomDrawTableViewCommonCell: TdxSpreadSheetCustomDrawTableViewCommonCellEvent read FOnCustomDrawTableViewCommonCell write FOnCustomDrawTableViewCommonCell;
    property OnCustomDrawTableViewHeaderCell: TdxSpreadSheetCustomDrawTableViewHeaderCellEvent read FOnCustomDrawTableViewHeaderCell write FOnCustomDrawTableViewHeaderCell;
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
    property OnEditChanged: TdxSpreadSheetViewNotifyEvent read FOnEditChanged write FOnEditChanged;
    property OnEdited: TdxSpreadSheetViewNotifyEvent read FOnEdited write FOnEdited;
    property OnEditing: TdxSpreadSheetViewInplaceEditingEvent read FOnEditing write FOnEditing;
    property OnEditValueChanged: TdxSpreadSheetViewNotifyEvent read FOnEditValueChanged write FOnEditValueChanged;
    property OnGetPassword: TdxGetPasswordEvent read FOnGetPassword write FOnGetPassword;
    property OnHistoryChanged: TNotifyEvent read FOnHistoryChanged write FOnHistoryChanged;
    property OnHyperlinkExecute: TdxSpreadSheetHyperlinkExecuteEvent read FOnHyperlinkExecute write FOnHyperlinkExecute;
    property OnInitEdit: TdxSpreadSheetViewInitInplaceEditEvent read FOnInitEdit write FOnInitEdit;
    property OnInitEditValue: TdxSpreadSheetViewInitInplaceEditValueEvent read FOnInitEditValue write FOnInitEditValue;
    property OnLayoutChanged: TNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;
    property OnModifiedChanged: TNotifyEvent read FOnModifiedChanged write FOnModifiedChanged;
    property OnPrepareLockedStateImage: TdxSpreadSheetPrepareLockedStateImageEvent read FOnPrepareLockedStateImage write FOnPrepareLockedStateImage;
    property OnProgress: TdxSpreadSheetProgressEvent read FOnProgress write FOnProgress;
    property OnScroll: TdxSpreadSheetScrollEvent read FOnScroll write FOnScroll;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function AddSheet(const ACaption: string = ''; AViewClass: TdxSpreadSheetCustomViewClass = nil): TdxSpreadSheetCustomView;
    procedure BeginDragAndDrop; override;
    procedure ClearAll;
    function EvaluateExpression(const AExpression: string; AView: TdxSpreadSheetTableView = nil): Variant;
    function GetSheetByName(const ACaption: string): TdxSpreadSheetCustomView;
    procedure LayoutChanged;

    procedure BeginUpdate(AMode: TcxLockedStateImageShowingMode = lsimNever);
    procedure EndUpdate;

    // Protection
    procedure Protect;
    procedure Unprotect;

    procedure LoadFromFile(const AFileName: string); virtual;
    procedure LoadFromStream(AStream: TStream); overload;
    procedure LoadFromStream(AStream: TStream; AFormat: TdxSpreadSheetCustomFormatClass); overload; virtual;
    procedure SaveToFile(const AFileName: string); virtual;
    procedure SaveToStream(AStream: TStream; AFormat: TdxSpreadSheetCustomFormatClass = nil); virtual;
    //
    procedure AddListener(AListener: IdxSpreadSheetListener);
    procedure RemoveListener(AListener: IdxSpreadSheetListener);

    property ActiveSheet: TdxSpreadSheetCustomView read GetActiveSheet write SetActiveSheet;
    property ActiveSheetAsTable: TdxSpreadSheetTableView read GetActiveSheetAsTable;
    property ActiveSheetIndex: Integer read FActiveSheetIndex write SetActiveSheetIndex;
    property CellStyles: TdxSpreadSheetCellStyles read FCellStyles;
    property DefaultCellStyle: TdxSpreadSheetDefaultCellStyle read FDefaultCellStyle;
    property DefinedNames: TdxSpreadSheetDefinedNames read FDefinedNames;
    property DialogsLookAndFeel: TcxLookAndFeel read FDialogsLookAndFeel write SetDialogsLookAndFeel;
    property ExternalLinks: TdxSpreadSheetExternalLinks read FExternalLinks;
    property FormulaController: TdxSpreadSheetFormulaController read FFormulaController;
    property History: TdxSpreadSheetHistory read FHistory;
    property LookAndFeel;
  {$WARNINGS OFF}
    property Modified: Boolean read FModified write SetModified default False;
  {$WARNINGS ON}
    property Password: string read FPassword write FPassword;
    property SheetCount: Integer read GetSheetCount;
    property Sheets[Index: Integer]: TdxSpreadSheetCustomView read GetSheet;
    property VisibleSheetCount: Integer read GetVisibleSheetCount;
    property VisibleSheets[Index: Integer]: TdxSpreadSheetCustomView read GetVisibleSheet;
    //
    property OptionsBehavior: TdxSpreadSheetOptionsBehavior read FOptionsBehavior write SetOptionsBehavior;
    property OptionsLockedStateImage: TdxSpreadSheetLockedStateImageOptions read FOptionsLockedStateImage write SetOptionsLockedStateImage;
    property OptionsProtection: TdxSpreadSheetWorkbookProtectionOptions read FOptionsProtection write SetOptionsProtection;
    property OptionsView: TdxSpreadSheetOptionsView read FOptionsView write SetOptionsView;
    property PageControl: TdxSpreadSheetPageControl read FPageControl write SetPageControl;
    property Styles: TdxSpreadSheetStyles read FStyles write SetStyle;
  end;

  { TdxSpreadSheetTextService }

  TdxSpreadSheetTextServiceClass = class of TdxSpreadSheetTextService;
  TdxSpreadSheetTextService = class
  strict private const
    ExcelLineSpacing = 1.0;
  protected
    class procedure ApplyDefaultStyle(ACell: TdxSpreadSheetCell; const AEditValue: string); virtual;
    class procedure CalculateTextBounds(ACanvas: TcxCanvas; ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo; var ATextBounds: TRect); virtual;
    class procedure DrawValue(ACanvas: TcxCanvas; ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo; ABounds: TRect); virtual;
    //
    class procedure Register;
    class procedure Unregister;
  public
    class procedure CalculateSize(ACell: TdxSpreadSheetCell; ACanvas: TcxCanvas;
      const ABounds: TRect; AIsMerged: Boolean; AWidth, AHeight: PInteger); virtual;
    class function IsFormattedEditValue(const AEditValue: string): Boolean; virtual;
    class function IsFormattedTextValue(ACell: TdxSpreadSheetCell): Boolean; virtual;
    class function IsRTFSupported: Boolean; virtual;
    class procedure MeasureSize(ACanvas: TcxCanvas; ACell: TdxSpreadSheetCell; AWidth, AHeight: PInteger); virtual;
    class function ForceSetAsRTF(ACell: TdxSpreadSheetCell; const AEditValue: string): Boolean; virtual;
    class function GetAsRTF(ACell: TdxSpreadSheetCell; var AValue: string): Boolean; virtual;
    class function SetAsRTF(ACell: TdxSpreadSheetCell; const AEditValue: string): Boolean; virtual;
  end;

  { TdxSpreadSheetRTFService }

  TdxSpreadSheetRTFService = class
  public
    class function CurrentService: TdxSpreadSheetTextServiceClass;
  end deprecated 'use dxSpreadSheetTextService instead';

var
  dxSpreadSheetTextService: TdxSpreadSheetTextServiceClass = TdxSpreadSheetTextService;

function dxSpreadSheetFormatsRepository: TdxSpreadSheetFormatsRepository;
implementation

{$R dxSpreadSheet.res}

uses
  Consts, RTLConsts, dxTypeHelpers, dxOLECryptoContainerStrs, dxDPIAwareUtils,
  // SpreadSheet - Formats
  dxSpreadSheetFormatXLSX, dxSpreadSheetFormatXLS, dxSpreadSheetFormatBinary, dxSpreadSheetFormatCSV,
  dxSpreadSheetFormatODS, dxSpreadSheetFormatTXT, dxSpreadSheetFormatHTML, dxSpreadSheetFormatXML,
  //
  dxSpreadSheetFormulas, dxSpreadSheetFunctions, dxSpreadSheetCoreHelpers, dxSpreadSheetPopupMenu,
  dxSpreadSheetCellsModificationDialog, dxGDIPlusAPI, dxSpreadSheetFormatCellsDialog, dxSpreadSheetCoreHistory,
  dxSpreadSheetFindAndReplaceDialog,
  dxSpreadSheetConditionalFormattingIconSet, dxSpreadSheetContainers, dxSpreadSheetContainerCustomizationDialog,
  dxSpreadSheetClipboard, dxSpreadSheetEditHyperlinkDialog, dxSpreadSheetClipboardFormats, dxSpreadSheetSelection,
  dxSpreadSheetAutoFilling, dxSpreadSheetPasswordDialog, dxSpreadSheetProtectSheetDialog,
  dxSpreadSheetProtectWorkbookDialog, dxSpreadSheetActions, dxSpreadSheetCoreFormulasParser,
  dxSpreadSheetCoreFormulasTokens, dxSpreadSheetCoreStrs, dxSpreadSheetFormulasHelpers;

const
  SFSpreadSheetVersion = $53534346;

type
  TcxCustomEditStyleAccess = class(TcxEditStyle);
  TcxCustomRichEditAccess = class(TcxCustomRichEdit);
  TDropDownEditAccess = class(TcxCustomDropDownInnerEdit);
  TdxFormulaAccess = class(TdxSpreadSheetCustomFormula);
  TdxFunctionTokenAccess = class(TdxSpreadSheetFormulaFunctionToken);
  TdxPredefinedFormatsAccess = class(TdxSpreadSheetPredefinedFormats);
  TdxSpreadSheetCustomFormulaControllerAccess = class(TdxSpreadSheetCustomFormulaController);
  TdxSpreadSheetFormattedSharedStringAccess = class(TdxSpreadSheetFormattedSharedString);
  TdxSpreadSheetHyperlinkAccess = class(TdxSpreadSheetHyperlink);
  TdxSpreadSheetHyperlinksAccess = class(TdxSpreadSheetHyperlinks);
  TdxTokenAccess = class(TdxSpreadSheetFormulaToken);
  TdxVariantTokenAccess = class(TdxSpreadSheetFormulaVariantToken);
  TdxSpreadSheetCellStyleAccess = class(TdxSpreadSheetCellStyleHandle);

var
  FSpreadSheetFormats: TdxSpreadSheetFormatsRepository = nil;

function dxSpreadSheetFormatsRepository: TdxSpreadSheetFormatsRepository;
begin
  if FSpreadSheetFormats = nil then
    FSpreadSheetFormats := TdxSpreadSheetFormatsRepository.Create;
  Result := FSpreadSheetFormats;
end;

function dxSpreadSheetContains(const AArea: TRect; ACell: TdxSpreadSheetCell): Boolean; overload;
begin
  Result := (ACell <> nil) and dxSpreadSheetContains(AArea, ACell.RowIndex, ACell.ColumnIndex);
end;

{ TdxSpreadSheetPersistentObject }

constructor TdxSpreadSheetPersistentObject.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  FSpreadSheet := ASpreadSheet;
end;

function TdxSpreadSheetPersistentObject.GetOwner: TPersistent;
begin
  Result := FSpreadSheet;
end;

{ TdxSpreadSheetListeners }

procedure TdxSpreadSheetListeners.NotifyActiveSheetChanged(Sender: TdxCustomSpreadSheet);
var
  AListener: IdxSpreadSheetListener2;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Supports(Items[I], IdxSpreadSheetListener2, AListener) then
      AListener.ActiveSheetChanged(Sender);
end;

procedure TdxSpreadSheetListeners.NotifyDataChanged(Sender: TdxCustomSpreadSheet);
var
  AListener: IdxSpreadSheetListener;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Supports(Items[I], IdxSpreadSheetListener, AListener) then
      AListener.DataChanged(Sender);
end;

procedure TdxSpreadSheetListeners.NotifyDefinedNamesChanged(Sender: TdxCustomSpreadSheet);
var
  AListener: IdxSpreadSheetListener2;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Supports(Items[I], IdxSpreadSheetListener2, AListener) then
      AListener.DefinedNamesChanged(Sender);
end;

procedure TdxSpreadSheetListeners.NotifyEdited(Sender: TdxSpreadSheetCustomView);
var
  AListener: IdxSpreadSheetEditControllerListener;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Supports(Items[I], IdxSpreadSheetEditControllerListener, AListener) then
      AListener.Edited(Sender);
  end;
end;

procedure TdxSpreadSheetListeners.NotifyEditing(Sender: TdxSpreadSheetCustomView);
var
  AListener: IdxSpreadSheetEditControllerListener;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Supports(Items[I], IdxSpreadSheetEditControllerListener, AListener) then
      AListener.Editing(Sender);
  end;
end;

procedure TdxSpreadSheetListeners.NotifyEditingValueChanged(Sender: TdxSpreadSheetCustomView);
var
  AListener: IdxSpreadSheetEditControllerListener;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Supports(Items[I], IdxSpreadSheetEditControllerListener, AListener) then
      AListener.EditingValueChanged(Sender);
  end;
end;

procedure TdxSpreadSheetListeners.NotifyKeyDown(Sender: TdxCustomSpreadSheet; var Key: Word; ShiftState: TShiftState);
var
  AListener: IdxSpreadSheetKeyboardListener;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Supports(Items[I], IdxSpreadSheetKeyboardListener, AListener) then
      AListener.KeyDown(Sender, Key, ShiftState);
  end;
end;

procedure TdxSpreadSheetListeners.NotifyOptionsPrintChanged(Sender: TdxSpreadSheetCustomView);
var
  AListener: IdxSpreadSheetOptionsPrintListener;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Supports(Items[I], IdxSpreadSheetOptionsPrintListener, AListener) then
      AListener.OptionsChanged(Sender);
end;

procedure TdxSpreadSheetListeners.NotifySelectionChanged(Sender: TdxSpreadSheetCustomView);
var
  AListener: IdxSpreadSheetSelectionListener;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Supports(Items[I], IdxSpreadSheetSelectionListener, AListener) then
      AListener.SelectionChanged(Sender);
end;

procedure TdxSpreadSheetListeners.NotifySelectionModeChanged(
  Sender: TdxSpreadSheetTableView; AMode: TdxSpreadSheetTableViewSelectionMode);
var
  AListener: IdxSpreadSheetTableViewSelectionModeListener;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Supports(Items[I], IdxSpreadSheetTableViewSelectionModeListener, AListener) then
      AListener.SelectionModeChanged(Sender, AMode);
end;

{ TdxSpreadSheetViewPersistentObject }

constructor TdxSpreadSheetViewPersistentObject.Create(AView: TdxSpreadSheetCustomView);
begin
  FView := AView;
end;

function TdxSpreadSheetViewPersistentObject.GetScaleFactor: TdxScaleFactor;
begin
  Result := SpreadSheet.ScaleFactor;
end;

function TdxSpreadSheetViewPersistentObject.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := FView.SpreadSheet;
end;

{ TdxSpreadSheetObjectListItem }

constructor TdxSpreadSheetObjectListItem.Create(AOwner: TdxSpreadSheetObjectList);
begin
  FOwner := AOwner;
  Owner.ItemList.Add(Self);
end;

destructor TdxSpreadSheetObjectListItem.Destroy;
begin
  Owner.Remove(Self);
  inherited Destroy;
end;

procedure TdxSpreadSheetObjectListItem.Changed;
begin
  Owner.Changed;
end;

function TdxSpreadSheetObjectListItem.GetIndex: Integer;
begin
  Result := Owner.ItemList.IndexOf(Self);
end;

{ TdxSpreadSheetObjectList }

constructor TdxSpreadSheetObjectList.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(ASpreadSheet);
  FItems := TcxObjectList.Create;
end;

destructor TdxSpreadSheetObjectList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxSpreadSheetObjectList.Clear;
var
  I: Integer;
begin
  SpreadSheet.BeginUpdate;
  try
    for I := Count - 1 downto 0 do
      FItems[I].Free;
  finally
    SpreadSheet.EndUpdate;
  end;
end;

procedure TdxSpreadSheetObjectList.Delete(AIndex: Integer);
begin
  Items[AIndex].Free;
end;

procedure TdxSpreadSheetObjectList.Changed;
begin
  SpreadSheet.AddChanges([sscData, sscModified]);
end;

function TdxSpreadSheetObjectList.CreateItem: TdxSpreadSheetObjectListItem;
begin
  Result := nil;
end;

procedure TdxSpreadSheetObjectList.Remove(AItem: TdxSpreadSheetObjectListItem);
begin
  if FItems.Remove(AItem) >= 0 then
    Changed;
end;

function TdxSpreadSheetObjectList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxSpreadSheetObjectList.GetItem(AIndex: Integer): TdxSpreadSheetObjectListItem;
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    TdxSpreadSheetInvalidObject.AssignTo(Result)
  else
    Result := TdxSpreadSheetObjectListItem(FItems.List[AIndex])
end;

{ TdxSpreadSheetCellDisplayValue }

constructor TdxSpreadSheetCellDisplayValue.Create;
begin
  IsDirty := True;
end;

function TdxSpreadSheetCellDisplayValue.CheckNeedUpdateValue(const AValue: Variant; const AFormatCode: string): Boolean;
begin
  Result := (AFormatCode <> FFormatCode) or (VarType(AValue) <> VarType(FValue)) or not VarEquals(AValue, FValue);
  if Result then
  begin
    FFormatCode := AFormatCode;
    FValue := AValue;
  end;
end;

procedure TdxSpreadSheetCellDisplayValue.SetAsText(const AText: string);
begin
  FValue := Null;
  FFormatCode := '';
  FFormattedValue.Text := AText;
  FFormattedValue.IsText := bTrue;
end;

function TdxSpreadSheetCellDisplayValue.Update(const ACell: TdxSpreadSheetCell): TdxSpreadSheetNumberFormatResult;
var
  ANeedFormatting: Boolean;
  AValue: Variant;
begin
  if IsDirty then
  begin
    IsDirty := False;
    if (ACell.DataType = cdtFormula) and ACell.ActualShowFormula then
      SetAsText(ACell.AsFormula.AsText)
    else
    begin
      AValue := ACell.AsVariant;
      ANeedFormatting := CheckNeedUpdateValue(AValue, ACell.StyleHandle.DataFormat.FormatCode);
      if dxIsNumericOrDateTime(AValue) and (AValue = 0) and (ACell.DataType <> cdtBoolean) and (VarType(AValue) <> varBoolean) then
      begin
        ANeedFormatting := ACell.View.Options.ActualZeroValues;
        if not ANeedFormatting then
          SetAsText('');
      end;
      if ANeedFormatting then
        ACell.StyleHandle.DataFormat.Format(AValue, ACell.DataType, ACell.SpreadSheet.FormatSettings, FFormattedValue);
    end;
  end;
  Result := FFormattedValue;
end;

{ TdxSpreadSheetCell }

constructor TdxSpreadSheetCell.Create(AOwner: TdxDynamicItemList; AIndex: Integer);
begin
  inherited Create(AOwner, AIndex);
  FShowFormula := bDefault;
  StyleHandle := GetCellStyles.DefaultStyle;
  View.DimensionChanged;
end;

destructor TdxSpreadSheetCell.Destroy;
begin
  CheckAndDeleteObjectData;
  if History.CanAddCommand then
    History.AddCommand(TdxSpreadSheetHistoryDeleteCellCommand.Create(Self));
  if FStyleObject.ClassType = TdxSpreadSheetCellStyle then
    FreeAndNil(FStyleObject)
  else
    StyleHandle := nil;

  ReleaseDisplayValue;
  inherited Destroy;
end;

procedure TdxSpreadSheetCell.Assign(ASource: TdxDynamicListItem);
begin
  if ASource is TdxSpreadSheetCell then
  begin
    AssignData(TdxSpreadSheetCell(ASource));
    StyleHandle := TdxSpreadSheetCell(ASource).StyleHandle;
  end;
end;

procedure TdxSpreadSheetCell.BeforeDestruction;
begin
  inherited BeforeDestruction;
  View.DoRemoveCell(Self);
end;

procedure TdxSpreadSheetCell.Clear;
var
  AChanges: TdxSpreadSheetChanges;
begin
  AChanges := [sscData];
  if FDataType <> cdtBlank then
    Include(AChanges, sscModified);
  CheckAndDeleteObjectData;
  FDataType := cdtBlank;
  Changed(AChanges);
end;

function TdxSpreadSheetCell.GetAbsoluteBounds: TRect;
begin
  Result := View.GetAbsoluteCellBounds(RowIndex, ColumnIndex);
end;

function TdxSpreadSheetCell.GetReference(AOptions: TdxSpreadSheetCellReferenceOptions = []): string;
begin
  Result := GetReference(SpreadSheet.OptionsView.R1C1Reference, AOptions);
end;

function TdxSpreadSheetCell.GetReference(AR1C1ReferenceStyle: Boolean; AOptions: TdxSpreadSheetCellReferenceOptions = []): string;
begin
  Result := dxReferenceToString(RowIndex, ColumnIndex, AR1C1ReferenceStyle, AOptions, View.Caption);
end;

function TdxSpreadSheetCell.GetAsRTF(out AText: string): Boolean;
begin
  Result := dxSpreadSheetTextService.GetAsRTF(Self, AText);
end;

function TdxSpreadSheetCell.SetAsRTF(const AText: string): Boolean;
begin
  Result := dxSpreadSheetTextService.SetAsRTF(Self, AText);
end;

procedure TdxSpreadSheetCell.SetText(const AText: string; const AFormulaChecking: Boolean = False);
begin
  if not SpreadSheet.FormulaController.UpdatingSlaveCells and (IsPartOfArrayFormula <> afpNone) then
    raise EdxSpreadSheetCannotChangePartOfArrayError.Create(cxGetResourceString(@sdxErrorCannotChangePartOfArray));
  SetTextCore(AText, AFormulaChecking);
end;

procedure TdxSpreadSheetCell.AssignData(ASource: TdxSpreadSheetCell);
begin
  case ASource.DataType of
    cdtString:
      AsSharedString := ASource.AsSharedString;
    cdtFormula:
      AsFormula := ASource.AsFormula.Clone;
  else
    AsVariant := ASource.AsVariant;
  end;
end;

procedure TdxSpreadSheetCell.Changed(AChanges: TdxSpreadSheetChanges);
var
  ARow: TdxSpreadSheetTableRow;
begin
  ARow := Row;
  if not ARow.IsCustomSize then
    ARow.FBestFitIsDirty := True;
  if IsPartOfArrayFormula <> afpSlaveCell then
    ARow.View.AddChanges(AChanges);
  ReleaseDisplayValue;
end;

procedure TdxSpreadSheetCell.CheckAndDeleteObjectData;
begin
  if History.CanAddCommand then
    History.AddCommand(TdxSpreadSheetHistoryChangeCellCommand.CreateEx(Self));
  if DataType = cdtString then
    TdxSpreadSheetSharedString(PObject(@FData)^).Release
  else
    if IsFormula then
      PObject(@FData)^.Free;
end;

procedure TdxSpreadSheetCell.CheckAreaReferenceTokens(var AParentToken: TdxSpreadSheetFormulaToken);
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  if AParentToken = nil then
    Exit;
  if AParentToken is TdxSpreadSheetFormulaAreaReference then
    TdxSpreadSheetFormulaAreaReference(AParentToken).Check
  else
  begin
    AToken := AParentToken.FirstChild;
    while AToken <> nil do
    begin
      CheckAreaReferenceTokens(AToken);
      AToken := AToken.Next;
    end;
  end;
  AToken := AParentToken.Next;
  CheckAreaReferenceTokens(AToken);
end;

procedure TdxSpreadSheetCell.CheckDefaultCellFormat;
var
  AValue: Variant;
begin
  case FDataType of
    cdtCurrency:
      Style.DataFormat.FormatCodeID := 7;
    cdtDateTime:
      if not StyleHandle.DataFormat.IsDateTime then
      begin
        AValue := AsDateTime;
        if not IsZero(AValue) and IsZero(Frac(AValue)) then
          Style.DataFormat.FormatCodeID := $E
        else
          Style.DataFormat.FormatCode := TdxSpreadSheetDateTimeFormatHelper.GetDefaultFormat(AValue);
      end;
  end;
end;

procedure TdxSpreadSheetCell.Move(ARow: TdxSpreadSheetTableRow; AColumn: TdxSpreadSheetTableColumn);
var
  AItem: TdxDynamicListItem;
begin
  if (ARow <> nil) and (AColumn <> nil) and ((ARow <> Row) or (AColumn <> Column)) then
  begin
    AItem := ARow.RowCells.FindItem(AColumn.Index);
    if (AItem <> nil) and (AItem.Index = AColumn.Index) then
      raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorCellAlreadyExists), [AColumn.Index]);

    if AItem = Self then
    begin
      AItem := FNext;
      if AItem = nil then
        AItem := FPrev;
    end;

    Row.RowCells.DeleteItem(Self);
    FOwner := ARow.RowCells;
    FIndex := AColumn.Index;
    ARow.RowCells.InsertItem(Self, AItem);
    Changed([sscLayout, sscData, sscModified, sscDimension]);
  end;
end;

procedure TdxSpreadSheetCell.Move(ARowIndex, AColumnIndex: Integer);
begin
  dxSpreadSheetValidate(AColumnIndex, 0, dxSpreadSheetMaxColumnIndex);
  dxSpreadSheetValidate(ARowIndex, 0, dxSpreadSheetMaxRowIndex);
  if (ARowIndex <> RowIndex) or (AColumnIndex <> ColumnIndex) then
    Move(View.Rows.CreateItem(ARowIndex), View.Columns.CreateItem(AColumnIndex));
end;

procedure TdxSpreadSheetCell.CellStyleChanging;
begin
  if History.CanAddCommand then
  begin
    if IsValueFormattedString then
      History.AddCommand(TdxSpreadSheetHistoryChangeCellCommand.CreateEx(Self));
    History.AddCommand(TdxSpreadSheetHistoryChangeCellStyleCommand.Create(Self));
  end;
end;

function TdxSpreadSheetCell.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := SpreadSheet.CellStyles;
end;

function TdxSpreadSheetCell.GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
begin
  Result := SpreadSheet.FormatSettings;
end;

procedure TdxSpreadSheetCell.ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);

  function CompareBorders(ABorder: TcxBorder): Boolean;
  begin
    Result :=
      (APrevStyle.Borders.BorderColor[ABorder] = ANewStyle.Borders.BorderColor[ABorder]) and
      (APrevStyle.Borders.BorderStyle[ABorder] = ANewStyle.Borders.BorderStyle[ABorder]);
  end;

  procedure CheckBorder(ABorder: TcxBorder);
  const
    OppositeBordersMap: array[TcxBorder] of TcxBorder = (bRight, bBottom, bLeft, bTop);
    HorzOffsetMap: array[TcxBorder] of Integer = (-1, 0, 1, 0);
    VertOffsetMap: array[TcxBorder] of Integer = (0, -1, 0, 1);
  var
    ACell: TdxSpreadSheetCell;
  begin
    if not CompareBorders(ABorder) then
    begin
      ACell := View.Cells[RowIndex + VertOffsetMap[ABorder], ColumnIndex + HorzOffsetMap[ABorder]];
      if ACell <> nil then
        ACell.Style.Borders[OppositeBordersMap[ABorder]].Assign(Style.Borders[ABorder]);
    end;
  end;

var
  ABorder: TcxBorder;
begin
  for ABorder := Low(ABorder) to High(ABorder) do
    CheckBorder(ABorder);
  if (APrevStyle.Font <> ANewStyle.Font) and IsValueFormattedString then
    TdxSpreadSheetFormattedSharedStringAccess(AsSharedString).FontChanged(APrevStyle.Font, ANewStyle.Font);
end;

procedure TdxSpreadSheetCell.ReleaseDisplayValue;
begin
  FreeAndNil(FDisplayValue);
end;

procedure TdxSpreadSheetCell.ReleaseWrappers;
var
  AStyle: TdxSpreadSheetCellStyle;
begin
  if FStyleObject.ClassType = TdxSpreadSheetCellStyle then
  begin
    AStyle := TdxSpreadSheetCellStyle(FStyleObject);
    try
      FStyleObject := nil;
      StyleHandle := AStyle.Handle;
    finally
      AStyle.Free;
    end;
  end;
end;

procedure TdxSpreadSheetCell.SetDisplayValueDirty;
begin
  if FDisplayValue <> nil then
    FDisplayValue.IsDirty := True;
end;

procedure TdxSpreadSheetCell.StyleChanged;
begin
  Changed([sscLayout, sscStyle]);
end;

function TdxSpreadSheetCell.GetAsBoolean: Boolean;
begin
  if DataType = cdtBoolean then
    Result := PBoolean(@FData)^
  else
    Result := GetAsFloat <> 0;
end;

function TdxSpreadSheetCell.GetAsCurrency: Currency;
begin
  if DataType = cdtCurrency then
    Result := PCurrency(@FData)^
  else
    Result := GetAsFloat;
end;

function TdxSpreadSheetCell.GetAsDateTime: TDateTime;
begin
  Result := GetAsFloat;
end;

function TdxSpreadSheetCell.GetAsError: TdxSpreadSheetFormulaErrorCode;
begin
  if DataType = cdtError then
    Result := TdxSpreadSheetFormulaErrorCode(FData[0])
  else
    if (DataType = cdtFormula) and (AsFormula.ActualErrorCode <> ecNone) then
      Result := AsFormula.ActualErrorCode
    else
      Result := TdxSpreadSheetFormulaErrorCode(GetAsInteger);
end;

function TdxSpreadSheetCell.GetAsFloat: Double;
begin
  case DataType of
    cdtBoolean:
      Result := Integer(PBoolean(@FData)^);
    cdtCurrency:
      Result := PCurrency(@FData)^;
    cdtFloat:
      Result := PFloat(@FData)^;
    cdtDateTime:
      Result := PDateTime(@FData)^;
    cdtInteger:
      Result := PInteger(@FData)^;
    cdtFormula:
      Result := AsVariant;
  else
    Result := 0;
  end;
end;

function TdxSpreadSheetCell.GetAsFormula: TdxSpreadSheetFormula;
begin
  if IsFormula then
    Result := TdxSpreadSheetFormula(PObject(@FData)^)
  else
    Result := nil;
end;

function TdxSpreadSheetCell.GetAsInteger: Integer;
begin
  if DataType = cdtInteger then
    Result := PInteger(@FData)^
  else
    Result := Trunc(GetAsFloat);
end;

function TdxSpreadSheetCell.GetAsSharedString: TdxSpreadSheetSharedString;
begin
  if DataType = cdtString then
    Result := TdxSpreadSheetSharedString(PObject(@FData)^)
  else
    Result := nil;
end;

function TdxSpreadSheetCell.GetAsString: string;

  function GetAsFormatedDateTime(AValue: TDateTime): string;
  begin
    AValue := dxDateTimeToRealDateTime(AValue, SpreadSheet.OptionsView.ActualDateTimeSystem);
    if StyleHandle.DataFormat.IsTime then
      Result := TimeToStr(AValue)
    else
      Result := DateTimeToStr(AValue);
  end;

begin
  Result := '';
  case DataType of
    cdtString:
      Result := GetAsSharedString.Value;
    cdtBoolean:
      Result := dxBoolToString[PBoolean(@FData)^];
    cdtCurrency:
      Result := CurrToStr(PCurrency(@FData)^);
    cdtInteger:
      Result := IntToStr(PInteger(@FData)^);
    cdtDateTime:
      Result := GetAsFormatedDateTime(PDateTime(@FData)^);

    cdtFloat:
      if StyleHandle.DataFormat.IsDateTime then
        Result := GetAsFormatedDateTime(PFloat(@FData)^)
      else
        Result := FloatToStr(PFloat(@FData)^);

    cdtFormula:
      if ActualShowFormula then
        Result := AsFormula.AsText
      else
        Result := VarToStr(AsVariant);
  end;
end;

function TdxSpreadSheetCell.GetAsVariant: Variant;
begin
  case DataType of
    cdtBoolean:
      Result := AsBoolean;
    cdtCurrency:
      Result := AsCurrency;
    cdtError:
      Result := AsError;
    cdtFloat:
      Result := AsFloat;
    cdtDateTime:
      Result := AsDateTime;
    cdtInteger:
      Result := AsInteger;
    cdtString:
      Result := AsString;
    cdtFormula:
      Result := AsFormula.ActualValue;
  else
    Result := Null;
  end;
end;

function TdxSpreadSheetCell.GetActualDataType: TdxSpreadSheetCellDataType;
begin
  Result := FDataType;
  if Result = cdtFormula then
  begin
    if (AsFormula.ErrorCode <> ecNone) or (AsFormula.ResultValue <> nil) and (AsFormula.ResultValue.ErrorCode <> ecNone) then
      Result := cdtError
    else
      Result := dxGetDataTypeByVariantValue(AsFormula.ActualValue);
  end;
  if DisplayValue.IsText = bTrue then
    Result := cdtString;
end;

function TdxSpreadSheetCell.GetActualShowFormula: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(ShowFormula, View.Options.ActualShowFormulas);
end;

function TdxSpreadSheetCell.GetColumn: TdxSpreadSheetTableColumn;
begin
  Result := View.Columns[Index];
end;

function TdxSpreadSheetCell.GetColumnIndex: Integer;
begin
  Result := Index;
end;

function TdxSpreadSheetCell.GetDisplayText: string;
begin
  Result := DisplayValue.Text;
end;

function TdxSpreadSheetCell.GetDisplayValue: TdxSpreadSheetNumberFormatResult;
begin
  if FDisplayValue = nil then
    FDisplayValue := TdxSpreadSheetCellDisplayValue.Create;
  Result := FDisplayValue.Update(Self)
end;

function TdxSpreadSheetCell.GetHasValue: Boolean;
begin
  Result := FDataType in [cdtBoolean, cdtCurrency, cdtFloat, cdtDateTime, cdtInteger, cdtFormula];
  if FDataType = cdtString then
    Result := AsString <> '';
end;

function TdxSpreadSheetCell.GetHistory: TdxSpreadSheetHistory;
begin
  Result := SpreadSheet.History;
end;

function TdxSpreadSheetCell.GetIsEmpty: Boolean;
begin
  Result := (Self = nil) or (DataType = cdtBlank);
end;

function TdxSpreadSheetCell.GetIsFormula: Boolean;
begin
  Result := (Self <> nil) and (DataType = cdtFormula);
end;

function TdxSpreadSheetCell.GetIsMerged: Boolean;
begin
  Result := not dxSpreadSheetIsSingleCellArea(View.MergedCells.ExpandArea(ColumnIndex, RowIndex));
end;

function TdxSpreadSheetCell.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := View.SpreadSheet;
end;

function TdxSpreadSheetCell.GetStyle: TdxSpreadSheetCellStyle;
var
  AHandle: TdxSpreadSheetCellStyleHandle;
begin
  if FStyleObject.ClassType <> TdxSpreadSheetCellStyle then
  begin
    AHandle := TdxSpreadSheetCellStyleHandle(FStyleObject);
    FStyleObject := TdxSpreadSheetCellStyle.Create(Self, AHandle);
    AHandle.Release;
  end;
  Result := TdxSpreadSheetCellStyle(FStyleObject);
end;

function TdxSpreadSheetCell.GetStyleHandle: TdxSpreadSheetCellStyleHandle;
begin
  if (FStyleObject <> nil) and (FStyleObject.ClassType = TdxSpreadSheetCellStyle) then
    Result := TdxSpreadSheetCellStyle(FStyleObject).Handle
  else
    Result := TdxSpreadSheetCellStyleHandle(FStyleObject);
end;

function TdxSpreadSheetCell.GetRow: TdxSpreadSheetTableRow;
begin
  Result := TdxSpreadSheetTableRowCells(FOwner).Row;
end;

function TdxSpreadSheetCell.GetRowIndex: Integer;
begin
  Result := Row.Index;
end;

function TdxSpreadSheetCell.GetView: TdxSpreadSheetTableView;
begin
  Result := Row.View;
end;

procedure TdxSpreadSheetCell.SetColumn(AValue: TdxSpreadSheetTableColumn);
begin
  Move(Row, AValue);
end;

procedure TdxSpreadSheetCell.SetColumnIndex(AValue: Integer);
begin
  Move(RowIndex, AValue);
end;

procedure TdxSpreadSheetCell.SetIsEmpty(AValue: Boolean);
begin
  if AValue then
    Clear;
end;

procedure TdxSpreadSheetCell.SetRow(AValue: TdxSpreadSheetTableRow);
begin
  Move(AValue, Column);
end;

procedure TdxSpreadSheetCell.SetRowIndex(AValue: Integer);
begin
  Move(AValue, ColumnIndex);
end;

procedure TdxSpreadSheetCell.SetShowFormula(AValue: TdxDefaultBoolean);
begin
  if FShowFormula <> AValue then
  begin
    FShowFormula := AValue;
    Changed([sscData]);
  end;
end;

procedure TdxSpreadSheetCell.SetStyleHandle(const AValue: TdxSpreadSheetCellStyleHandle);
begin
  if (FStyleObject <> nil) and (FStyleObject.ClassType = TdxSpreadSheetCellStyle) then
    TdxSpreadSheetCellStyle(FStyleObject).Handle := AValue
  else
    if FStyleObject <> AValue then
    begin
      if (FStyleObject <> nil) and (AValue <> nil) then
        CellStyleChanging;
      if dxChangeHandle(TdxHashTableItem(FStyleObject), AValue) then
        StyleChanged;
    end;
end;

function TdxSpreadSheetCell.IsMultiline: Boolean;
begin
  Result := ((csWordWrap in StyleHandle.States) or (StyleHandle.AlignVert in [ssavJustify, ssavDistributed])) and not IsNumericValue;
end;

function TdxSpreadSheetCell.IsPartOfArrayFormula(AFormula: PdxSpreadSheetCustomFormula = nil): TdxSpreadSheetArrayFormulaPart;
begin
  Result := SpreadSheet.FormulaController.IsPartOfArrayFormula(View, RowIndex, ColumnIndex, AFormula);
end;

function TdxSpreadSheetCell.IsValueFormattedString: Boolean;
begin
  Result := (DataType = cdtString) and (AsSharedString is TdxSpreadSheetFormattedSharedString);
end;

function TdxSpreadSheetCell.IsNumericValue: Boolean;
begin
  Result := ActualDataType in [cdtCurrency, cdtFloat, cdtInteger, cdtDateTime];
end;

procedure TdxSpreadSheetCell.LoadFromStream(AReader: TcxReader; AFormulasRef: TdxSpreadSheetFormulaAsTextInfoList);
var
  AType: TdxSpreadSheetCellDataType;
begin
  Clear;
  AType := TdxSpreadSheetCellDataType(AReader.ReadByte);
  ZeroMemory(@FData[0], SizeOf(FData));
  case AType of
    cdtBlank:
      if AReader.Version <= 8 then {do nothing, for new streams only}
        AReader.Stream.ReadBuffer(FData, SizeOf(FData));
    cdtString:
      AsSharedString := SpreadSheet.StringTable.LoadItemFromStream(AReader);

    cdtFormula:
      if AReader.Version >= 4 then
        AFormulasRef.AddFromStream(Self, AReader)
      else
        raise EdxSpreadSheetFormatError.Create('');
  else
    AReader.Stream.ReadBuffer(FData, SizeOf(FData));
  end;

  FDataType := AType;
  Changed([sscData]);
end;

procedure TdxSpreadSheetCell.SaveToStream(AWriter: TcxWriter);
begin
  AWriter.WriteByte(Byte(FDataType));
  case FDataType of
    cdtBlank:
      {do nothing};
    cdtString:
      AsSharedString.SaveToStream(AWriter);
    cdtFormula:
      TdxSpreadSheetFormulaAsTextInfo.SaveFormulaToStream(AsFormula, AWriter);
  else
    AWriter.Stream.WriteBuffer(FData, SizeOf(FData));
  end;
end;

class function TdxSpreadSheetCell.GetContentOffsets(ABottomBorderStyle: TdxSpreadSheetCellBorderStyle): TRect;
begin
  Result := cxRect(3, 1, 2, dxSpreadSheetBorderStyleThickness[ABottomBorderStyle] div 2);
end;

procedure TdxSpreadSheetCell.SetAsBoolean(const AValue: Boolean);
begin
  if (DataType <> cdtBoolean) or (AsBoolean <> AValue) then
  begin
    CheckAndDeleteObjectData;
    FDataType := cdtBoolean;
    PBoolean(@FData)^ := AValue;
    Changed([sscData, sscModified]);
  end;
end;

procedure TdxSpreadSheetCell.SetAsCurrency(const AValue: Currency);
begin
  if (DataType <> cdtCurrency) or (AsCurrency <> AValue) then
  begin
    CheckAndDeleteObjectData;
    FDataType := cdtCurrency;
    PCurrency(@FData)^ := AValue;
    CheckDefaultCellFormat;
    Changed([sscData, sscModified]);
  end;
end;

procedure TdxSpreadSheetCell.SetAsDateTime(const AValue: TDateTime);
begin
  if (DataType <> cdtDateTime) or (AsDateTime <> AValue) then
  begin
    CheckAndDeleteObjectData;
    FDataType := cdtDateTime;
    if InRange(Double(AValue), 1, 60) then
      PDateTime(@FData)^ := AValue - 1
    else
      PDateTime(@FData)^ := AValue;

    CheckDefaultCellFormat;
    Changed([sscData, sscModified]);
  end;
end;

procedure TdxSpreadSheetCell.SetAsError(const AValue: TdxSpreadSheetFormulaErrorCode);
begin
  if (DataType <> cdtError) or (AsError <> AValue) then
  begin
    CheckAndDeleteObjectData;
    FDataType := cdtError;
    FData[0] := Byte(AValue);
    Changed([sscData, sscModified]);
  end;
end;

procedure TdxSpreadSheetCell.SetAsFloat(const AValue: Double);
begin
  if (DataType <> cdtFloat) or (AsFloat <> AValue) then
  begin
    CheckAndDeleteObjectData;
    FDataType := cdtFloat;
    PFloat(@FData)^ := AValue;
    Changed([sscData, sscModified]);
  end;
end;

procedure TdxSpreadSheetCell.SetAsFormula(const AValue: TdxSpreadSheetFormula);
begin
  if (DataType <> cdtFormula) or (PObject(@FData)^ <> AValue) then
  begin
    CheckAndDeleteObjectData;
    FDataType := cdtFormula;
    PObject(@FData)^ := AValue;
    AValue.Initialize(Self);
    Changed([sscData, sscModified]);
  end;
end;

procedure TdxSpreadSheetCell.SetAsInteger(const AValue: Integer);
begin
  if (DataType <> cdtInteger) or (AsInteger <> AValue) then
  begin
    CheckAndDeleteObjectData;
    FDataType := cdtInteger;
    PInteger(@FData)^ := AValue;
    Changed([sscData, sscModified]);
  end;
end;

procedure TdxSpreadSheetCell.SetAsSharedString(AValue: TdxSpreadSheetSharedString);
begin
  if (DataType <> cdtString) or (AsSharedString <> AValue) then
  begin
    CheckAndDeleteObjectData;
    FDataType := cdtString;
    AValue.AddRef;
    PObject(@FData)^ := AValue;
    Changed([sscData, sscModified]);
  end;
end;

procedure TdxSpreadSheetCell.SetAsString(const AValue: string);
begin
  SetAsSharedString(View.StringTable.Add(AValue));
end;

procedure TdxSpreadSheetCell.SetAsVariant(const AValue: Variant);
begin
  case VarType(AValue) of
    varEmpty, varNull:
      Clear;
    varSmallint, varInteger, varShortInt, varByte, varWord:
      AsInteger := AValue;
    varDate:
      AsDateTime := AValue;
    varCurrency:
      AsCurrency := AValue;
    varDouble, varSingle, varLongWord, varInt64:
      AsFloat := AValue;
    varBoolean:
      AsBoolean := AValue;
  else
    SetText(VarToWideStr(AValue), True);
  end;
end;

procedure TdxSpreadSheetCell.SetTextCore(const AText: string; const AFormulaChecking: Boolean);

  procedure CheckFormula(const AText: string);
  var
    AParser: TdxSpreadSheetFormulaParser;
  begin
    if History.InProcess or (AsFormula = nil) or not SameText(View.FormatSettings.Operations[opEQ] + AsFormula.SourceText, AText) then
    begin
      AParser := TdxSpreadSheetFormulaParser.Create(View.SpreadSheet);
      try
        if AParser.ParseFormula(AText, Self) then
        begin
          CheckAreaReferenceTokens(AsFormula.FTokens);
          AsFormula.Validate;
        end
        else
          AsString := AText;
      finally
        AParser.Free;
      end;
    end;
  end;

var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AValue: Variant;
begin
  View.BeginUpdate;
  try
    if dxSpreadSheetIsErrorString(AText, AErrorCode) and (AErrorCode <> ecNone) then
      AsError := AErrorCode
    else if StyleHandle.DataFormat.IsText then
      AsString := AText
    else if dxTryStrToOrdinal(AText, AValue, SpreadSheet.FormatSettings) then
      AsVariant := AValue
    else if AFormulaChecking then
      CheckFormula(AText)
    else
      AsString := AText;
  finally
    View.EndUpdate;
  end;
end;

function TdxSpreadSheetCell.GetAsFormulaObject: TObject;
begin
  Result := AsFormula;
end;

function TdxSpreadSheetCell.GetDataType: TdxSpreadSheetCellDataType;
begin
  Result := FDataType;
end;

{ TdxSpreadSheetContainerAnchorPoint }

constructor TdxSpreadSheetContainerAnchorPoint.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFixedToCell := True;
end;

procedure TdxSpreadSheetContainerAnchorPoint.Changed;
begin
  Container.Changed([ccAnchors, ccPosition]);
end;

procedure TdxSpreadSheetContainerAnchorPoint.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetContainerAnchorPoint then
  begin
    Cell := TdxSpreadSheetContainerAnchorPoint(Source).Cell;
    Offset := TdxSpreadSheetContainerAnchorPoint(Source).Offset;
    FixedToCell := TdxSpreadSheetContainerAnchorPoint(Source).FixedToCell;
  end;
end;

procedure TdxSpreadSheetContainerAnchorPoint.LoadFromStream(AReader: TcxReader);
begin
  Cell := TdxSpreadSheetCellHelper.ReadRef(AReader, Container.Parent);
  FixedToCell := AReader.ReadBoolean;
  Offset := AReader.ReadPoint;
end;

procedure TdxSpreadSheetContainerAnchorPoint.SaveToStream(AWriter: TcxWriter);
begin
  TdxSpreadSheetCellHelper.WriteRef(AWriter, Cell);
  AWriter.WriteBoolean(FixedToCell);
  AWriter.WritePoint(Offset);
end;

function TdxSpreadSheetContainerAnchorPoint.GetContainer: TdxSpreadSheetContainer;
begin
  Result := Owner as TdxSpreadSheetContainer;
end;

procedure TdxSpreadSheetContainerAnchorPoint.SetCell(const AValue: TdxSpreadSheetCell);
begin
  if (AValue <> nil) and (AValue.View <> Container.Parent) then
  begin
    raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInvalidAnchorCell),
      [TdxSpreadSheetCellHelper.EncodeRefAsString(AValue)]);
  end;

  if Cell <> AValue then
  begin
    FCell := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetContainerAnchorPoint.SetOffset(const AValue: TPoint);
begin
  if not cxPointIsEqual(FOffset, AValue) then
  begin
    FOffset := AValue;
    Changed;
  end;
end;

{ TdxSpreadSheetContainerTransform }

procedure TdxSpreadSheetContainerTransform.Changed;
begin
  Container.Changed([ccTransform]);
end;

procedure TdxSpreadSheetContainerTransform.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetContainerTransform then
  begin
    FlipHorizontally := TdxSpreadSheetContainerTransform(Source).FlipHorizontally;
    FlipVertically := TdxSpreadSheetContainerTransform(Source).FlipVertically;
    RotationAngle := TdxSpreadSheetContainerTransform(Source).RotationAngle;
  end;
end;

procedure TdxSpreadSheetContainerTransform.LoadFromStream(AReader: TcxReader);
begin
  FlipHorizontally := AReader.ReadBoolean;
  FlipVertically := AReader.ReadBoolean;
  RotationAngle := AReader.ReadFloat;
end;

procedure TdxSpreadSheetContainerTransform.SaveToStream(AWriter: TcxWriter);
begin
  AWriter.WriteBoolean(FlipHorizontally);
  AWriter.WriteBoolean(FlipVertically);
  AWriter.WriteFloat(RotationAngle);
end;

function TdxSpreadSheetContainerTransform.GetContainer: TdxSpreadSheetContainer;
begin
  Result := inherited Owner as TdxSpreadSheetContainer;
end;

procedure TdxSpreadSheetContainerTransform.SetFlipHorizontally(AValue: Boolean);
begin
  if FFlipHorizontally <> AValue then
  begin
    FFlipHorizontally := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetContainerTransform.SetFlipVertically(AValue: Boolean);
begin
  if FFlipVertically <> AValue then
  begin
    FFlipVertically := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetContainerTransform.SetRotationAngle(const AValue: Double);
var
  ABounds: TRect;
begin
  if FRotationAngle <> AValue then
  begin
    if not Container.IsUpdating then
    begin
      ABounds := Container.Calculator.CalculateBounds;
      FRotationAngle := AValue;
      Container.Calculator.UpdateAnchors(ABounds);
    end
    else
      FRotationAngle := AValue;

    Changed;
  end;
end;

{ TdxSpreadSheetContainer }

constructor TdxSpreadSheetContainer.Create(AView: TdxSpreadSheetCustomView);
begin
  inherited Create(AView.SpreadSheet);
  CreateSubClasses;
  Parent := AView;

  if History.CanAddCommand then
  begin
    History.BeginAction(TdxSpreadSheetHistoryChangeContainerAction);
    History.AddCommand(TdxSpreadSheetHistoryCreateContainerCommand.Create(Self));
    History.EndAction;
  end;
end;

destructor TdxSpreadSheetContainer.Destroy;
begin
  cxClearObjectLinks(Self);
  FreeAndNil(FAnchorPoint2);
  FreeAndNil(FAnchorPoint1);
  FreeAndNil(FCalculator);
  FreeAndNil(FTransform);
  inherited Destroy;
end;

procedure TdxSpreadSheetContainer.Assign(Source: TPersistent);
begin
  BeginChanging;
  try
    DoAssign(Source);
  finally
    EndChanging;
  end;
end;

procedure TdxSpreadSheetContainer.BeforeDestruction;
begin
  inherited BeforeDestruction;

  if not SpreadSheet.IsDestroying then
  begin
    if History.CanAddCommand then
    begin
      History.BeginAction(TdxSpreadSheetHistoryChangeContainerAction);
      History.AddCommand(TdxSpreadSheetHistoryDeleteContainerCommand.Create(Self));
      History.EndAction;
    end;
    Parent := nil;
  end;
end;

procedure TdxSpreadSheetContainer.BeginChanging;
begin
  BeginUpdate;
  if (FUpdateCount = 1) and History.CanAddCommand and (Parent <> nil) then
  begin
    History.BeginAction(TdxSpreadSheetHistoryChangeContainerAction);
    History.AddCommand(TdxSpreadSheetHistoryChangeContainerCommand.Create(Self, True));
  end;
end;

procedure TdxSpreadSheetContainer.EndChanging;
begin
  if FUpdateCount = 1 then
  begin
    if History.CanAddCommand and (Parent <> nil) then
      History.EndAction;
  end;
  EndUpdate;
end;

procedure TdxSpreadSheetContainer.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxSpreadSheetContainer.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed([]);
end;

procedure TdxSpreadSheetContainer.BringToFront;
begin
  Index := MaxInt;
end;

procedure TdxSpreadSheetContainer.SendToBack;
begin
  Index := 0;
end;

procedure TdxSpreadSheetContainer.Changed(AChanges: TdxSpreadSheetContainerChanges);
begin
  FChanges := FChanges + AChanges;
  if not IsUpdating and (FChanges <> []) then
  begin
    AChanges := FChanges;
    FChanges := [];
    DoChanged(AChanges);
  end;
end;

function TdxSpreadSheetContainer.CreateAnchorPoint: TdxSpreadSheetContainerAnchorPoint;
begin
  Result := TdxSpreadSheetContainerAnchorPoint.Create(Self);
end;

function TdxSpreadSheetContainer.CreateCalculator: TdxSpreadSheetContainerCalculator;
begin
  Result := TdxSpreadSheetContainerCalculator.Create(Self);
end;

function TdxSpreadSheetContainer.CreateTransform: TdxSpreadSheetContainerTransform;
begin
  Result := TdxSpreadSheetContainerTransform.Create(Self);
end;

function TdxSpreadSheetContainer.CreateViewInfo: TdxSpreadSheetContainerViewInfo;
begin
  Result := TdxSpreadSheetContainerViewInfo.Create(Self);
end;

procedure TdxSpreadSheetContainer.CreateSubClasses;
begin
  FCalculator := CreateCalculator;
  FAnchorPoint1 := CreateAnchorPoint;
  FAnchorPoint2 := CreateAnchorPoint;
  FTransform := CreateTransform;
  FVisible := True;
end;

procedure TdxSpreadSheetContainer.DoAssign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetContainer then
  begin
    AnchorPoint1 := TdxSpreadSheetContainer(Source).AnchorPoint1;
    AnchorPoint2 := TdxSpreadSheetContainer(Source).AnchorPoint2;
    AnchorType := TdxSpreadSheetContainer(Source).AnchorType;
    Description := TdxSpreadSheetContainer(Source).Description;
    Name := TdxSpreadSheetContainer(Source).Name;
    Restrictions := TdxSpreadSheetContainer(Source).Restrictions;
    Title := TdxSpreadSheetContainer(Source).Title;
    Transform := TdxSpreadSheetContainer(Source).Transform;
    Visible := TdxSpreadSheetContainer(Source).Visible;
  end;
end;

procedure TdxSpreadSheetContainer.DoChanged(AChanges: TdxSpreadSheetContainerChanges);
begin
  SpreadSheet.AddChanges([sscLayout, sscModified]);
end;

procedure TdxSpreadSheetContainer.DoSetParent(AValue: TdxSpreadSheetCustomView);
var
  ABounds: TRect;
begin
  Focused := False;
  ABounds := Calculator.CalculateBounds;
  if FParent <> nil then
    FParent.Containers.InternalRemove(Self);
  if AValue <> nil then
    AValue.Containers.InternalAdd(Self);
  FParent := AValue;
  AnchorPoint1.Cell := nil;
  AnchorPoint2.Cell := nil;
  Calculator.UpdateAnchors(ABounds);
end;

procedure TdxSpreadSheetContainer.CellRemoving(ACell: TdxSpreadSheetCell);
var
  R: TRect;
begin
  if (ACell = AnchorPoint1.Cell) or (ACell = AnchorPoint2.Cell) then
  begin
    if AnchorType <> catAbsolute then
    begin
      R := Calculator.CalculateBounds;
      AnchorType := catAbsolute;
      Calculator.UpdateAnchors(R);
    end;
    AnchorPoint1.Cell := nil;
    AnchorPoint2.Cell := nil;
  end;
end;

function TdxSpreadSheetContainer.IsCellUsed(ACell: TdxSpreadSheetCell): Boolean;
begin
  Result := (AnchorPoint1.Cell = ACell) or (AnchorPoint2.Cell = ACell);
end;

function TdxSpreadSheetContainer.CanFocusViaKeyboard: Boolean;
begin
  Result := Visible;
end;

function TdxSpreadSheetContainer.CanClone: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetContainer.CanMove: Boolean;
begin
  Result := Parent.CanEditContainers and not (crNoMove in Restrictions);
end;

function TdxSpreadSheetContainer.CanResize: Boolean;
begin
  Result := Parent.CanEditContainers and not (crNoResize in Restrictions);
end;

function TdxSpreadSheetContainer.CanRotate: Boolean;
begin
  Result := IsTransformsSupported and Parent.CanEditContainers and not (crNoRotation in Restrictions);
end;

function TdxSpreadSheetContainer.IsTransformsSupported: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetContainer.KeepAspectUsingCornerHandles: Boolean;
begin
  Result := crNoChangeAspectUsingCornerHandles in Restrictions;
end;

procedure TdxSpreadSheetContainer.ReleaseHyperlink;
begin
  FHyperlink := nil;
end;

procedure TdxSpreadSheetContainer.LoadFromStream(AReader: TcxReader; ACanReadContent: Boolean = True);
begin
  BeginChanging;
  try
    AnchorType := TdxSpreadSheetContainerAnchorType(AReader.ReadWord);
    LoadFromStream(AReader, AReader.ReadWord, ACanReadContent);
  finally
    EndChanging;
  end;
end;

procedure TdxSpreadSheetContainer.LoadFromStream(AReader: TcxReader; AVersion: Word; ACanReadContent: Boolean = True);
var
  AIndex: TdxSpreadSheetContainerRestriction;
begin
  AnchorPoint1.LoadFromStream(AReader);
  AnchorPoint2.LoadFromStream(AReader);
  Description := AReader.ReadWideString;
  Name := AReader.ReadWideString;
  Title := AReader.ReadWideString;
  Visible := AReader.ReadBoolean;
  Transform.LoadFromStream(AReader);

  Restrictions := [];
  for AIndex := Low(AIndex) to High(AIndex) do
  begin
    if AReader.ReadBoolean then
      Restrictions := Restrictions + [AIndex];
  end;

  if (AReader.Version >= 7) and AReader.ReadBoolean then
  begin
    HyperLink := TdxSpreadSheetTableView(Parent).Hyperlinks.Add(cxInvalidRect);
    TdxSpreadSheetHyperlinkAccess(Hyperlink).LoadFromStream(AReader);
  end;
end;

procedure TdxSpreadSheetContainer.SaveToStream(AWriter: TcxWriter; ACanSaveContent: Boolean = True);
var
  AIndex: TdxSpreadSheetContainerRestriction;
begin
  AWriter.WriteWord(Ord(AnchorType));
  AWriter.WriteWord(dxSpreadSheetContainerVersion);
  AnchorPoint1.SaveToStream(AWriter);
  AnchorPoint2.SaveToStream(AWriter);
  AWriter.WriteWideString(Description);
  AWriter.WriteWideString(Name);
  AWriter.WriteWideString(Title);
  AWriter.WriteBoolean(Visible);
  Transform.SaveToStream(AWriter);
  for AIndex := Low(AIndex) to High(AIndex) do
    AWriter.WriteBoolean(AIndex in Restrictions);
  AWriter.WriteBoolean(Hyperlink <> nil);
  if Hyperlink <> nil then
    TdxSpreadSheetHyperlinkAccess(Hyperlink).SaveToStream(AWriter);
end;

function TdxSpreadSheetContainer.GetFocused: Boolean;
begin
  Result := (Parent <> nil) and (Parent.Controller.FocusedContainer = Self);
end;

function TdxSpreadSheetContainer.GetHistory: TdxSpreadSheetHistory;
begin
  Result := SpreadSheet.History;
end;

function TdxSpreadSheetContainer.GetIndex: Integer;
begin
  if Parent <> nil then
    Result := Parent.Containers.IndexOf(Self)
  else
    Result := -1;
end;

function TdxSpreadSheetContainer.GetIsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TdxSpreadSheetContainer.SetAnchorPoint1(const AValue: TdxSpreadSheetContainerAnchorPoint);
begin
  FAnchorPoint1.Assign(AValue);
end;

procedure TdxSpreadSheetContainer.SetAnchorPoint2(const AValue: TdxSpreadSheetContainerAnchorPoint);
begin
  FAnchorPoint2.Assign(AValue);
end;

procedure TdxSpreadSheetContainer.SetAnchorType(const AValue: TdxSpreadSheetContainerAnchorType);
begin
  if FAnchorType <> AValue then
  begin
    FAnchorType := AValue;
    Changed([ccAnchors, ccPosition]);
  end;
end;

procedure TdxSpreadSheetContainer.SetFocused(const AValue: Boolean);
begin
  if Focused <> AValue then
  begin
    if AValue then
      Parent.Controller.FocusContainer(Self, True)
    else
      Parent.Controller.FocusedContainer := nil;
  end;
end;

procedure TdxSpreadSheetContainer.SetHyperlink(const AValue: TdxSpreadSheetHyperlink);
begin
  if Hyperlink <> AValue then
  begin
    FreeAndNil(FHyperlink);
    FHyperlink := AValue;
    Changed([ccContent]);
  end;
end;

procedure TdxSpreadSheetContainer.SetIndex(AValue: Integer);
begin
  if Parent <> nil then
  begin
    AValue := Max(Min(AValue, Parent.Containers.Count - 1), 0);
    if Index <> AValue then
    begin
      History.BeginAction(TdxSpreadSheetHistoryChangeContainerAction);
      try
        History.AddCommand(TdxSpreadSheetHistoryChangeContainerIndexCommand.Create(Self, AValue));
        Parent.Containers.Extract(Self);
        Parent.Containers.Insert(AValue, Self);
        Changed([ccPosition]);
      finally
        History.EndAction;
      end;
    end;
  end;
end;

procedure TdxSpreadSheetContainer.SetParent(const AValue: TdxSpreadSheetCustomView);
begin
  if FParent <> AValue then
  begin
    BeginUpdate;
    try
      DoSetParent(AValue);
      Changed([ccPosition]);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetContainer.SetTransform(AValue: TdxSpreadSheetContainerTransform);
begin
  FTransform.Assign(AValue);
end;

procedure TdxSpreadSheetContainer.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    if not AValue then
      Focused := False;
    FVisible := AValue;
    Changed([ccVisibility]);
  end;
end;

{ TdxSpreadSheetCellHelper }

class procedure TdxSpreadSheetCellHelper.DecodeRef(const ARef: Int64; out ARow, AColumn: Integer);
begin
  AColumn := Integer(ARef);
  ARow := Integer(ARef shr 32);
end;

class function TdxSpreadSheetCellHelper.EncodeRef(ACell: TdxSpreadSheetCell): Int64;
begin
  if ACell <> nil then
    Result := EncodeRef(ACell.RowIndex, ACell.ColumnIndex)
  else
    Result := -1;
end;

class function TdxSpreadSheetCellHelper.EncodeRef(ARow, AColumn: Integer): Int64;
begin
  Result := dxMakeInt64(AColumn, ARow);
end;

class function TdxSpreadSheetCellHelper.EncodeRefAsString(ACell: TdxSpreadSheetCell): string;
begin
  Result := ACell.View.Caption + '.' + TdxSpreadSheetColumnHelper.NameByIndex(ACell.ColumnIndex) + IntToStr(ACell.RowIndex + 1);
end;

class function TdxSpreadSheetCellHelper.GetCell(ARow, AColumn: Integer; AView: TdxSpreadSheetCustomView): TdxSpreadSheetCell;
var
  ATableView: IdxSpreadSheetTableView;
begin
  if (AColumn >= 0) and (ARow >= 0) and Supports(AView, IdxSpreadSheetTableView, ATableView) then
    Result := ATableView.GetCell(ARow, AColumn)
  else
    Result := nil;
end;

class function TdxSpreadSheetCellHelper.ReadRef(AReader: TcxReader;
  AView: TdxSpreadSheetCustomView; AAnchorRow: Integer = 0; AAnchorColumn: Integer = 0): TdxSpreadSheetCell;
var
  AColumnIndex: Integer;
  ARowIndex: Integer;
begin
  AColumnIndex := AReader.ReadInteger;
  if AColumnIndex >= 0 then
    Inc(AColumnIndex, AAnchorColumn);

  ARowIndex := AReader.ReadInteger;
  if ARowIndex >= 0 then
    Inc(ARowIndex, AAnchorRow);

  Result := GetCell(ARowIndex, AColumnIndex, AView);
end;

class procedure TdxSpreadSheetCellHelper.WriteRef(AWriter: TcxWriter;
  ACell: TdxSpreadSheetCell; AAnchorRow: Integer = 0; AAnchorColumn: Integer = 0);
begin
  if ACell <> nil then
  begin
    AWriter.WriteInteger(ACell.ColumnIndex - AAnchorColumn);
    AWriter.WriteInteger(ACell.RowIndex - AAnchorRow);
  end
  else
  begin
    AWriter.WriteInteger(-1);
    AWriter.WriteInteger(-1);
  end;
end;

{ TdxSpreadSheetCustomDrawingObject }

procedure TdxSpreadSheetCustomDrawingObject.Assign(Source: TPersistent);
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomDrawingObject.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FIsDestroying := True;
end;

procedure TdxSpreadSheetCustomDrawingObject.Changed;
begin
  if not FIsDestroying then
    dxCallNotify(OnChange, Self);
end;

{ TdxSpreadSheetContainerCalculator }

constructor TdxSpreadSheetContainerCalculator.Create(AOwner: TdxSpreadSheetContainer);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TdxSpreadSheetContainerCalculator.CalculateBounds: TRect;

  function CalculateOffset(const ACellBounds: TRect; const AOffset: TPoint): TPoint;
  begin
    Result := ACellBounds.TopLeft;
    if (AOffset.X < 0) or (ACellBounds.Right > ACellBounds.Left) then
      Inc(Result.X, AOffset.X);
    if (AOffset.Y < 0) or (ACellBounds.Bottom > ACellBounds.Top) then
      Inc(Result.Y, AOffset.Y);
  end;

begin
  Result := cxNullRect;
  case AnchorType of
    catOneCell, catTwoCell:
      Result.TopLeft := CalculateOffset(GetAbsoluteCellBounds(AnchorPoint1.Cell), AnchorPoint1.Offset);
    catAbsolute:
      Result.TopLeft := AnchorPoint1.Offset;
  end;

  case AnchorType of
    catAbsolute, catOneCell:
      Result := cxRectSetSize(Result, AnchorPoint2.Offset.X, AnchorPoint2.Offset.Y);
    catTwoCell:
      Result.BottomRight := CalculateOffset(GetAbsoluteCellBounds(AnchorPoint2.Cell), AnchorPoint2.Offset);
  end;

  Result.Bottom := Max(Result.Bottom, Result.Top);
  Result.Right := Max(Result.Right, Result.Left);
  Result := cxRectAdjust(TransformCoords(Result, True));
end;

procedure TdxSpreadSheetContainerCalculator.UpdateAnchors(const ABounds: TRect);
var
  ACell1: TdxSpreadSheetCell;
  ACell2: TdxSpreadSheetCell;
  ARect: TRect;
begin
  Owner.BeginUpdate;
  try
    ARect := TransformCoords(ABounds, False);
    case AnchorType of
      catOneCell:
        if GetNearCells(ARect, ACell1, ACell2) then
        begin
          AnchorPoint1.Cell := ACell1;
          AnchorPoint1.Offset := cxPointOffset(ARect.TopLeft, GetAbsoluteCellBounds(ACell1).TopLeft, False);
          AnchorPoint2.Offset := cxPointOffset(ARect.BottomRight, ARect.TopLeft, False);
        end
        else
          AnchorType := catAbsolute;

      catTwoCell:
        if GetNearCells(ARect, ACell1, ACell2) then
        begin
          AnchorPoint1.Cell := ACell1;
          AnchorPoint1.Offset := cxPointOffset(ARect.TopLeft, GetAbsoluteCellBounds(ACell1).TopLeft, False);

          AnchorPoint2.Cell := ACell2;
          AnchorPoint2.Offset := cxPointOffset(ARect.BottomRight, GetAbsoluteCellBounds(ACell2).TopLeft, False);
        end
        else
          AnchorType := catAbsolute;
    end;

    if AnchorType = catAbsolute then
    begin
      AnchorPoint1.Offset := ARect.TopLeft;
      AnchorPoint2.Offset := cxPointOffset(ARect.BottomRight, ARect.TopLeft, False);
    end;
  finally
    Owner.EndUpdate;
  end;
end;

procedure TdxSpreadSheetContainerCalculator.UpdateAnchorsAfterResize(const APrevBounds: TRect; AForceUpdate: Boolean = False);
var
  ACell: TdxSpreadSheetCell;
  ACellOffset: TPoint;
begin
  if AnchorPoint1.FixedToCell then
  begin
    if (AnchorType = catTwoCell) and not AnchorPoint2.FixedToCell then
    begin
      Owner.BeginUpdate;
      try
        ACell := AnchorPoint1.Cell;
        ACellOffset := AnchorPoint1.Offset;
        UpdateAnchors(cxRectSetSize(CalculateBounds, cxSize(APrevBounds)));
        AnchorPoint1.Offset := ACellOffset;
        AnchorPoint1.Cell := ACell;
      finally
        Owner.EndUpdate;
      end;
    end
    else
      if AForceUpdate then
        UpdateAnchors(APrevBounds);
  end
  else
    if AForceUpdate or (AnchorType in [catOneCell, catTwoCell]) then
      UpdateAnchors(APrevBounds);
end;

function TdxSpreadSheetContainerCalculator.CheckForContentArea(const R: TRect): TRect;
var
  AContentRect: TRect;
  ADeltaX: Integer;
  ADeltaY: Integer;
  AIndex: Integer;
  ARect: TRect;
  AView: TdxSpreadSheetCustomView;
  AViewInfo: TdxSpreadSheetContainerViewInfo;
begin
  Result := R;
  AView := Owner.Parent;
  if AView <> nil then
  begin
    AIndex := AView.ViewInfo.Containers.FindItem(Owner);
    if AIndex >= 0 then
    begin
      AViewInfo := AView.ViewInfo.Containers.Items[AIndex] as TdxSpreadSheetContainerViewInfo;
      ARect := cxRectCenter(R, cxSize(AViewInfo.RealBounds));

      AContentRect := GetContentRect;
      ADeltaX := Max(AContentRect.Left - ARect.Left, 0);
      if ADeltaX = 0 then
        ADeltaX := Min(AContentRect.Right - ARect.Right, 0);

      ADeltaY := Max(AContentRect.Top - ARect.Top, 0);
      if ADeltaY = 0 then
        ADeltaY := Min(AContentRect.Bottom - ARect.Bottom, 0);

      Result := cxRectOffset(R, ADeltaX, ADeltaY);
    end;
  end;
end;

function TdxSpreadSheetContainerCalculator.GetAbsoluteCellBounds(ACell: TdxSpreadSheetCell): TRect;
begin
  if ACell <> nil then
    Result := ACell.View.GetAbsoluteCellBounds(ACell.RowIndex, ACell.ColumnIndex, False)
  else
    Result := cxNullRect;
end;

function TdxSpreadSheetContainerCalculator.GetContentRect: TRect;
var
  ATableView: IdxSpreadSheetTableView;
begin
  Result := cxNullRect;
  if Supports(Owner.Parent, IdxSpreadSheetTableView, ATableView) then
    Result.BottomRight := ATableView.GetAbsoluteCellBounds(dxSpreadSheetMaxRowIndex, dxSpreadSheetMaxColumnIndex, False).BottomRight;
end;

function TdxSpreadSheetContainerCalculator.GetNearCells(const R: TRect; out ACell1, ACell2: TdxSpreadSheetCell): Boolean;
var
  AColumnIndex: Integer;
  ARowIndex: Integer;
  ATableView: IdxSpreadSheetTableView;
begin
  ACell1 := nil;
  ACell2 := nil;
  if Supports(Owner.Parent, IdxSpreadSheetTableView, ATableView) then
  begin
    if ATableView.GetCellAtAbsolutePoint(R.TopLeft, ARowIndex, AColumnIndex) then
      ACell1 := ATableView.GetCell(ARowIndex, AColumnIndex);
    if ATableView.GetCellAtAbsolutePoint(R.BottomRight, ARowIndex, AColumnIndex) then
      ACell2 := ATableView.GetCell(ARowIndex, AColumnIndex);
  end;
  Result := (ACell1 <> nil) and (ACell2 <> nil);
end;

procedure TdxSpreadSheetContainerCalculator.ModifyBounds(
  ALeftModifier, ATopModifier, ARightModifier, ABottomModifier: Integer);
var
  ARect: TRect;
begin
  ARect := CalculateBounds;
  ARect.TopLeft := cxPointOffset(ARect.TopLeft, ALeftModifier, ATopModifier);
  ARect.BottomRight := cxPointOffset(ARect.BottomRight, ARightModifier, ABottomModifier);
  UpdateAnchors(CheckForContentArea(ARect));
end;

procedure TdxSpreadSheetContainerCalculator.Resize(ADeltaX, ADeltaY: Integer);
var
  AFactorX: Integer;
  AFactorY: Integer;
  ARect: TRect;
begin
  ARect := CalculateBounds;
  AFactorX := MulDiv(ARect.Width, 10, 100);
  AFactorY := MulDiv(ARect.Height, 10, 100);

  if ADeltaX > 0 then
    AFactorX := Max(AFactorX, 1);
  if ADeltaY > 0 then
    AFactorY := Max(AFactorY, 1);

  if Owner.KeepAspectUsingCornerHandles then
  begin
    if ADeltaX = 0 then
      ADeltaX := ADeltaY
    else
      ADeltaY := ADeltaX;
  end;

  ModifyBounds(-ADeltaX * AFactorX, -ADeltaY * AFactorY, ADeltaX * AFactorX, ADeltaY * AFactorY);
end;

function TdxSpreadSheetContainerCalculator.TransformCoords(const R: TRect; ABackwardDirection: Boolean): TRect;
var
  AMatrix: TdxGPMatrix;
begin
  Result := R;
  if Abs(Sin(Transform.RotationAngle * Pi / 180)) > Sin(Pi / 4) then
  begin
    AMatrix := TdxGPMatrix.Create;
    try
      AMatrix.Rotate(90, cxRectCenter(dxRectF(Result)));
      if ABackwardDirection then
        AMatrix.Invert;
      Result := cxRectAdjust(AMatrix.TransformRect(Result));
    finally
      AMatrix.Free;
    end;
  end;
end;

function TdxSpreadSheetContainerCalculator.GetAnchorPoint1: TdxSpreadSheetContainerAnchorPoint;
begin
  Result := Owner.AnchorPoint1;
end;

function TdxSpreadSheetContainerCalculator.GetAnchorPoint2: TdxSpreadSheetContainerAnchorPoint;
begin
  Result := Owner.AnchorPoint2;
end;

function TdxSpreadSheetContainerCalculator.GetAnchorType: TdxSpreadSheetContainerAnchorType;
begin
  Result := Owner.AnchorType;
end;

function TdxSpreadSheetContainerCalculator.GetTransform: TdxSpreadSheetContainerTransform;
begin
  Result := Owner.Transform;
end;

procedure TdxSpreadSheetContainerCalculator.SetAnchorType(const Value: TdxSpreadSheetContainerAnchorType);
begin
  Owner.AnchorType := Value;
end;

{ TdxSpreadSheetContainers }

constructor TdxSpreadSheetContainers.Create(AOwner: TdxSpreadSheetCustomView);
begin
  inherited Create;
  FOwner := AOwner;
  FComments := TDictionary<Int64, TdxSpreadSheetContainer>.Create;
end;

destructor TdxSpreadSheetContainers.Destroy;
begin
  FreeAndNil(FComments);
  inherited Destroy;
end;

function TdxSpreadSheetContainers.Add(AClass: TdxSpreadSheetContainerClass): TdxSpreadSheetContainer;
begin
  Result := AClass.Create(Owner);
end;

procedure TdxSpreadSheetContainers.Add(AClass: TdxSpreadSheetContainerClass; out AContainer);
begin
  TdxSpreadSheetContainer(AContainer) := Add(AClass);
end;

function TdxSpreadSheetContainers.AddCommentContainer(ACell: TdxSpreadSheetCell): TdxSpreadSheetContainer;
begin
  Result := Add(TdxSpreadSheetCommentContainer);
  try
    if ACell <> nil then
    begin
      Result.BeginChanging;
      try
        TdxSpreadSheetCommentContainer(Result).Cell := ACell;
        Result.AnchorType := catAbsolute;
        Result.Calculator.UpdateAnchors(TdxSpreadSheetCommentContainerHelper.GetDefaultPosition(ACell));
      finally
        Result.EndChanging;
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TdxSpreadSheetContainers.AddCommentContainer(ACell: TdxSpreadSheetCell; out AContainer);
begin
  TdxSpreadSheetContainer(AContainer) := AddCommentContainer(ACell);
end;

function TdxSpreadSheetContainers.GetFirstVisibleContainer: TdxSpreadSheetContainer;
begin
  Result := FindNearestVisibleContainer(0, 1);
end;

function TdxSpreadSheetContainers.GetLastVisibleContainer: TdxSpreadSheetContainer;
begin
  Result := FindNearestVisibleContainer(Count - 1, -1);
end;

function TdxSpreadSheetContainers.GetNextVisibleContainer(
  ACurrentContainer: TdxSpreadSheetContainer): TdxSpreadSheetContainer;
begin
  Result := FindNearestVisibleContainer(ACurrentContainer.Index + 1, 1);
end;

function TdxSpreadSheetContainers.GetPrevVisibleContainer(
  ACurrentContainer: TdxSpreadSheetContainer): TdxSpreadSheetContainer;
begin
  Result := FindNearestVisibleContainer(ACurrentContainer.Index - 1, -1);
end;

function TdxSpreadSheetContainers.IsCellUsed(ACell: TdxSpreadSheetCell): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].IsCellUsed(ACell) then
      Exit(True);
  end;
  Result := False;
end;

procedure TdxSpreadSheetContainers.EnumCommentContainers(AProc: TdxSpreadSheetContainerEnumProc);
var
  AComment: TdxSpreadSheetContainer;
begin
  for AComment in FComments.Values do
  begin
    if not AProc(AComment) then
      Break;
  end;
end;

procedure TdxSpreadSheetContainers.EnumCommentContainers(const AArea: TRect; AProc: TdxSpreadSheetContainerEnumProc);
var
  AComment: TdxSpreadSheetContainer;
begin
  for AComment in FComments.Values do
    if dxSpreadSheetContains(AArea, TdxSpreadSheetCommentContainer(AComment).Cell) then
    begin
      if not AProc(AComment) then
        Break;
    end;
end;

function TdxSpreadSheetContainers.FindCommentContainer(ACell: TdxSpreadSheetCell): TdxSpreadSheetContainer;
begin
  if not FindCommentContainer(ACell, Result) then
    Result := nil;
end;

function TdxSpreadSheetContainers.FindCommentContainer(ACell: TdxSpreadSheetCell; out AContainer): Boolean;
begin
  Result := (ACell <> nil) and FindCommentContainer(ACell.RowIndex, ACell.ColumnIndex, AContainer);
end;

function TdxSpreadSheetContainers.FindCommentContainer(ARow, AColumn: Integer): TdxSpreadSheetContainer;
begin
  if not FindCommentContainer(ARow, AColumn, Result) then
    Result := nil;
end;

function TdxSpreadSheetContainers.FindCommentContainer(ARow, AColumn: Integer; out AContainer): Boolean;
var
  AArea: TRect;
  AIntf: IdxSpreadSheetTableView;
begin
  if not Owner.SpreadSheet.History.InProcess and Supports(Owner, IdxSpreadSheetTableView, AIntf) then
  begin
    AArea := AIntf.GetCellArea(ARow, AColumn);
    AColumn := AArea.Left;
    ARow := AArea.Top;
  end;
  Result := FindCommentContainerCore(ARow, AColumn, AContainer);
end;

function TdxSpreadSheetContainers.FindNextCommentContainer(
  ACell: TdxSpreadSheetCell; out AContainer; AGoForward: Boolean = True): Boolean;
begin
  if ACell <> nil then
    Result := FindNextCommentContainer(ACell.RowIndex, ACell.ColumnIndex, AContainer, AGoForward)
  else
    Result := FindNextCommentContainer(-1, -1, AContainer, AGoForward);
end;

function TdxSpreadSheetContainers.FindNextCommentContainer(
  ARow, AColumn: Integer; out AContainer; AGoForward: Boolean = True): Boolean;
var
  ACurrentContainerKey: Int64;
  ADelta: Int64;
  ADirection: Integer;
  AKey: Int64;
  AMinDelta: Int64;
begin
  Result := False;
  AMinDelta := MaxInt64;
  ADirection := IfThen(AGoForward, 1, -1);
  ACurrentContainerKey := TdxSpreadSheetCellHelper.EncodeRef(ARow, AColumn);
  for AKey in FComments.Keys do
  begin
    ADelta := AKey - ACurrentContainerKey;
    if (Sign(ADelta) = ADirection) and (Abs(ADelta) < AMinDelta) then
    begin
      TdxSpreadSheetContainer(AContainer) := FComments.Items[AKey];
      AMinDelta := Abs(ADelta);
      Result := True;
    end;
  end;
end;

function TdxSpreadSheetContainers.FindCommentContainerCore(ARow, AColumn: Integer; out AContainer): Boolean;
begin
  Result := FComments.TryGetValue(TdxSpreadSheetCellHelper.EncodeRef(ARow, AColumn), TdxSpreadSheetContainer(AContainer));
end;

procedure TdxSpreadSheetContainers.CellRemoving(ACell: TdxSpreadSheetCell);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].CellRemoving(ACell);
end;

procedure TdxSpreadSheetContainers.InternalAdd(AContainer: TdxSpreadSheetContainer);
begin
  inherited Add(AContainer);
end;

procedure TdxSpreadSheetContainers.InternalRemove(AContainer: TdxSpreadSheetContainer);
begin
  Extract(AContainer);
end;

procedure TdxSpreadSheetContainers.RegisterCommentContainer(AContainer: TdxSpreadSheetContainer);
begin
  if (AContainer is TdxSpreadSheetCommentContainer) and (TdxSpreadSheetCommentContainer(AContainer).Cell <> nil) then
    FComments.AddOrSetValue(TdxSpreadSheetCellHelper.EncodeRef(TdxSpreadSheetCommentContainer(AContainer).Cell), AContainer);
end;

procedure TdxSpreadSheetContainers.UnregisterCommentContainer(AContainer: TdxSpreadSheetContainer);
var
  AKey: Int64;
begin
  for AKey in FComments.Keys do
    if FComments.Items[AKey] = AContainer then
    begin
      FComments.Remove(AKey);
      Break;
    end;
end;

procedure TdxSpreadSheetContainers.AfterResize;
var
  AContainer: TdxSpreadSheetContainer;
begin
  Dec(FBoundsStorageLockCount);
  if (FBoundsStorage <> nil) and (FBoundsStorageLockCount = 0) then
  begin
    for AContainer in FBoundsStorage.Keys do
      AContainer.Calculator.UpdateAnchorsAfterResize(FBoundsStorage.Items[AContainer]);
    FreeAndNil(FBoundsStorage);
  end;
end;

procedure TdxSpreadSheetContainers.BeforeResize;
var
  I: Integer;
begin
  Inc(FBoundsStorageLockCount);
  if (Count > 0) and (FBoundsStorageLockCount = 1) then
  begin
    FBoundsStorage := TDictionary<TdxSpreadSheetContainer, TRect>.Create;
    for I := 0 to Count - 1 do
      FBoundsStorage.Add(Items[I], Items[I].Calculator.CalculateBounds);
  end;
end;

function TdxSpreadSheetContainers.FindNearestVisibleContainer(AStartFromIndex, ADirection: Integer): TdxSpreadSheetContainer;
begin
  Result := nil;
  while InRange(AStartFromIndex, 0, Count - 1) do
  begin
    if Items[AStartFromIndex].Visible then
      Exit(Items[AStartFromIndex]);
    Inc(AStartFromIndex, ADirection);
  end;
end;

{ TdxSpreadSheetFormulaAsTextInfo }

constructor TdxSpreadSheetFormulaAsTextInfo.Create(AFormula: TdxSpreadSheetFormula);
begin
  inherited Create;
  if AFormula <> nil then
  begin
    Formula := AFormula.AsText;
    FormulaResult := AFormula.ActualValue;
    Anchor := Point(AFormula.Cell.ColumnIndex, AFormula.Cell.RowIndex);
    IsArray := AFormula.IsArrayFormula;
  end;
end;

procedure TdxSpreadSheetFormulaAsTextInfo.LoadFromStream(AReader: TcxReader);
begin
  if AReader.Version >= 8 then
    FormulaResult := AReader.ReadVariant
  else
    FormulaResult := Null;

  Anchor := AReader.ReadPoint;
  Formula := AReader.ReadWideString;
  IsArray := AReader.ReadBoolean;
  IsShared := AReader.ReadBoolean;
  Reference := AReader.ReadRect;
end;

procedure TdxSpreadSheetFormulaAsTextInfo.SaveToStream(AWriter: TcxWriter);
begin
  if AWriter.Version >= 8 then
    AWriter.WriteVariant(FormulaResult);
  AWriter.WritePoint(Anchor);
  AWriter.WriteWideString(Formula);
  AWriter.WriteBoolean(IsArray);
  AWriter.WriteBoolean(IsShared);
  AWriter.WriteRect(Reference);
end;

class procedure TdxSpreadSheetFormulaAsTextInfo.SaveFormulaToStream(AFormula: TdxSpreadSheetFormula; AWriter: TcxWriter);
begin
  with Create(AFormula) do
  try
    SaveToStream(AWriter);
  finally
    Free;
  end;
end;

procedure TdxSpreadSheetFormulaAsTextInfo.ResolveReferences(Parser: TObject);
var
  AColumn: Integer;
  AFormula: TdxSpreadSheetFormula;
  AParser: TdxSpreadSheetFormulaParser absolute Parser;
  ARow: Integer;
begin
  if not AParser.ParseFormula(Formula, Cell) then
    raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInvalidFormula), [Formula]);

  AFormula := Cell.AsFormula;
  if not AParser.R1C1Reference then
    AFormula.Offset(Cell.RowIndex - Anchor.Y, Cell.ColumnIndex - Anchor.X);
  AFormula.IsArrayFormula := IsArray;
  AFormula.SourceText := AFormula.AsText;
  if IsArray then
    Cell.View.AddArrayFormula(AFormula, Reference);

  if IsShared then
  begin
    for ARow := Reference.Top to Reference.Bottom do
      for AColumn := Reference.Left to Reference.Right do
      begin
        if (ARow <> Cell.RowIndex) and (AColumn <> Cell.ColumnIndex) then
          Cell.View.CreateCell(ARow, AColumn).AsFormula := AFormula.Clone;
      end;
  end;
end;

{ TdxSpreadSheetFormulaAsTextInfoList }

constructor TdxSpreadSheetFormulaAsTextInfoList.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(True);
  FSpreadSheet := ASpreadSheet;
end;

function TdxSpreadSheetFormulaAsTextInfoList.Add(ACell: TdxSpreadSheetCell;
  const AFormula: string; AIsArray, AIsShared: Boolean; const AReference: TRect): TdxSpreadSheetFormulaAsTextInfo;
begin
  Result := Add(ACell, cxPoint(ACell.ColumnIndex, ACell.RowIndex), AFormula, AIsArray, AIsShared, AReference);
end;

function TdxSpreadSheetFormulaAsTextInfoList.Add(ACell: TdxSpreadSheetCell; const AAnchor: TPoint;
  const AFormula: string; AIsArray, AIsShared: Boolean; const AReference: TRect): TdxSpreadSheetFormulaAsTextInfo;
begin
  Result := CreateItem;
  Result.Cell := ACell;
  Result.Formula := AFormula;
  Result.IsArray := AIsArray;
  Result.IsShared := AIsShared;
  Result.Reference := AReference;
  Result.Anchor := AAnchor;
  Add(Result);
end;

function TdxSpreadSheetFormulaAsTextInfoList.AddFromStream(
  ACell: TdxSpreadSheetCell; AReader: TcxReader): TdxSpreadSheetFormulaAsTextInfo;
begin
  Result := CreateItem;
  Result.Cell := ACell;
  Result.LoadFromStream(AReader);
  Add(Result);
end;

function TdxSpreadSheetFormulaAsTextInfoList.HasCell(ACell: TdxSpreadSheetCell): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].Cell = ACell then
      Exit(True);
  end;
  Result := False;
end;

procedure TdxSpreadSheetFormulaAsTextInfoList.ResolveReferences;
var
  AInfo: TdxSpreadSheetFormulaAsTextInfo;
  AParser: TObject;
  I: Integer;
begin
  AParser := CreateParser;
  try
    for I := 0 to Count - 1 do
    begin
      AInfo := Items[I];
      if (AInfo.Formula <> '') and (AInfo.Cell <> nil) then
        AInfo.ResolveReferences(AParser);
    end;
  finally
    AParser.Free;
  end;
end;

function TdxSpreadSheetFormulaAsTextInfoList.CreateItem: TdxSpreadSheetFormulaAsTextInfo;
begin
  Result := TdxSpreadSheetFormulaAsTextInfo.Create;
end;

function TdxSpreadSheetFormulaAsTextInfoList.CreateParser: TObject;
begin
  Result := TdxSpreadSheetFormulaParser.Create(SpreadSheet);
end;

{ TdxSpreadSheetDefinedNameFormula }

constructor TdxSpreadSheetDefinedNameFormula.Create(AOwner: TdxSpreadSheetDefinedName);
begin
  inherited Create(nil);
  FOwner := AOwner;
end;

function TdxSpreadSheetDefinedNameFormula.GetController: TdxSpreadSheetCustomFormulaController;
begin
  Result := Owner.SpreadSheet.FormulaController;
end;

function TdxSpreadSheetDefinedNameFormula.GetView: TObject;
begin
  Result := nil;
end;

{ TdxSpreadSheetFormula }

constructor TdxSpreadSheetFormula.Create(AOwner: TdxSpreadSheetCell);
begin
  FOwner := AOwner;
  if Cell <> nil then
    Controller.Add(Self);
end;

destructor TdxSpreadSheetFormula.Destroy;
begin
  if (Cell <> nil) and (Cell.AsFormula = Self) then
    Cell.FDataType := cdtBlank;
  inherited Destroy;
end;

function TdxSpreadSheetFormula.Clone: TdxSpreadSheetFormula;
var
  AParser: TdxSpreadSheetFormulaParser;
begin
  AParser := TdxSpreadSheetFormulaParser.Create(Cell.SpreadSheet);
  try
    Result := TdxSpreadSheetFormula.Create(nil);
    Result.InternalSetOwner(Cell);
    Result.CalculateAnchors;
    AParser.ParseFormula(AsText, Result);
    Result.InternalSetOwner(nil); // # T138686: relative references incorrect resolves after clone because anchor is lost
    Result.SetError(ErrorCode, ErrorIndex);
  finally
    AParser.Free;
  end;
end;

function TdxSpreadSheetFormula.GetController: TdxSpreadSheetCustomFormulaController;
begin
  if Cell <> nil then
    Result := Cell.SpreadSheet.FormulaController
  else
    Result := nil;
end;

function TdxSpreadSheetFormula.GetMaxIterationCount: Integer;
begin
  if (Cell <> nil) and Cell.SpreadSheet.OptionsBehavior.IterativeCalculation then
    Result := Cell.SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount + 1
  else
    Result := inherited GetMaxIterationCount;
end;

function TdxSpreadSheetFormula.GetView: TObject;
begin
  if Cell <> nil then
    Result := Cell.View
  else
    Result := nil;
end;

function TdxSpreadSheetFormula.IsLinkedToCell: Boolean;
begin
  Result := FOwner <> nil;
end;

procedure TdxSpreadSheetFormula.Initialize(ACell: TdxSpreadSheetCell);
begin
  if FOwner = nil then
  begin
    InternalSetOwner(ACell);
    Controller.Add(Self);
    SourceText := AsText;
  end;
end;

procedure TdxSpreadSheetFormula.InternalSetOwner(ACell: TdxSpreadSheetCell);
begin
  if ACell <> FOwner then
  begin
    FOwner := ACell;
    CalculateAnchors;
  end;
end;

procedure TdxSpreadSheetFormula.CalculateAnchors;
begin
  if Cell <> nil then
  begin
    FAnchorColumn := Cell.ColumnIndex;
    FAnchorRow := Cell.RowIndex;
  end
  else
  begin
    FAnchorColumn := 0;
    FAnchorRow := 0;
  end;
  inherited CalculateAnchors;
end;

procedure TdxSpreadSheetFormula.ClearResult;
begin
  inherited ClearResult;
  if Cell <> nil then
    Cell.SetDisplayValueDirty;
end;

{ TdxSpreadSheetControlFormatSettings }

constructor TdxSpreadSheetControlFormatSettings.Create(AOwner: TdxCustomSpreadSheet);
begin
  FOwner := AOwner;
  inherited Create;
end;

function TdxSpreadSheetControlFormatSettings.ExpandExternalLinks: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetControlFormatSettings.GetFunctionName(const AName: Pointer): string;
begin
  Result := dxSpreadSheetUpperCase(cxGetResourceString(AName));
end;

procedure TdxSpreadSheetControlFormatSettings.UpdateSettings;
begin
  inherited UpdateSettings;
  CurrencyFormat := dxFormatSettings.CurrencyString;
  Data.ListSeparator := dxFormatSettings.ListSeparator;
  Data.DecimalSeparator := dxFormatSettings.DecimalSeparator;

  if ListSeparator = DecimalSeparator then
  begin
    if DecimalSeparator = ',' then
      ListSeparator := ';'
    else
      ListSeparator := ',';
  end;

  if ListSeparator = ';' then
    ArraySeparator := ':'
  else
    ArraySeparator := ';';

  R1C1Reference := Owner.OptionsView.R1C1Reference;
  DateTimeSystem := Owner.OptionsView.ActualDateTimeSystem;
  UpdateOperations;
end;

function TdxSpreadSheetControlFormatSettings.GetLocaleID: Integer;
begin
  Result := GetThreadLocale;
end;

{ TdxSpreadSheetFormulaController }

procedure TdxSpreadSheetFormulaController.Calculate;
begin
  inherited;
  if Count > 0 then
    SpreadSheet.AddChanges([sscData]);
end;

procedure TdxSpreadSheetFormulaController.ClearFormulasResults;
var
  I: Integer;
begin
  for I := 0 to SpreadSheet.DefinedNames.Count - 1 do
    SpreadSheet.DefinedNames[I].ClearResult;
  inherited;
end;

constructor TdxSpreadSheetFormulaController.Create(AOwner: TdxCustomSpreadSheet);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxSpreadSheetFormulaController.UpdateReferences(AFormula: TdxSpreadSheetCustomFormula;
  AView: TdxSpreadSheetTableView; const AArea: TRect; const ATargetOrigin: TPoint;
  AMode: TdxSpreadSheetFormulaUpdateReferencesMode);
var
  AExpression: string;
begin
  if AFormula <> nil then
  begin
    if SpreadSheet.History.CanAddCommand and (AMode in [urmDelete, urmMove]) then
    begin
      AExpression := AFormula.AsText;
      if TdxFormulaAccess(AFormula).UpdateReferences(AView, AArea, ATargetOrigin, AMode) then
        SpreadSheet.History.AddCommand(TdxSpreadSheetHistoryFormulaChangedCommand.Create(AFormula, AExpression));
    end
    else
      TdxFormulaAccess(AFormula).UpdateReferences(AView, AArea, ATargetOrigin, AMode);
  end;
end;

procedure TdxSpreadSheetFormulaController.UpdateReferences(AView: TdxSpreadSheetTableView;
  const AArea: TRect; const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    UpdateReferences(Items[I], AView, AArea, ATargetOrigin, AMode);
  for I := 0 to SpreadSheet.DefinedNames.Count - 1 do
    UpdateReferences(SpreadSheet.DefinedNames[I].Formula, AView, AArea, ATargetOrigin, AMode);
end;

procedure TdxSpreadSheetFormulaController.DoNameChanged(AName: TdxSpreadSheetDefinedName; const ANewName: string);

  function ProcessTokens(AToken: TdxSpreadSheetFormulaToken): Boolean;
  begin
    if AToken is TdxSpreadSheetDefinedNameToken then
      Result := TdxSpreadSheetDefinedNameToken(AToken).UpdateValue(AName, ANewName)
    else
      Result := False;

    while AToken <> nil do
    begin
      Result := ProcessTokens(AToken.FirstChild) or Result;
      AToken := AToken.Next;
    end;
  end;

  procedure ProcessFormula(AFormula: TdxSpreadSheetFormula);
  begin
    if ProcessTokens(AFormula.Tokens) then
    begin
      if AFormula.Cell <> nil then
        AFormula.Cell.SetDisplayValueDirty;
    end;
  end;

var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ProcessFormula(Items[I]);
end;

function TdxSpreadSheetFormulaController.CreateParser: TObject;
begin
  Result := TdxSpreadSheetFormulaParser.Create(SpreadSheet);
end;

function TdxSpreadSheetFormulaController.GetCellFromPath(APath: TdxSpreadSheetReferencePath): TdxSpreadSheetCell;
begin
  Result := TdxSpreadSheetTableView(APath.Sheet).Cells[APath.Row, APath.Column];
end;

function TdxSpreadSheetFormulaController.NeedAddFeatureFunctionPrefixToFunctionName: Boolean;
begin
  Result := sssWriting in SpreadSheet.State;
end;

function TdxSpreadSheetFormulaController.NeedRecalculate: Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.AutomaticCalculation;
end;

procedure TdxSpreadSheetFormulaController.Recalculate;
begin
  if NeedRecalculate then
  begin
    SpreadSheet.BeginUpdate;
    try
      Calculate;
    finally
      SpreadSheet.EndUpdate;
    end;
  end;
end;

function TdxSpreadSheetFormulaController.GetFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := SpreadSheet.FormatSettings;
end;

function TdxSpreadSheetFormulaController.GetItem(Index: Integer): TdxSpreadSheetFormula;
begin
  Result := inherited Items[Index] as TdxSpreadSheetFormula;
end;

{ TdxSpreadSheetCustomController }

constructor TdxSpreadSheetCustomController.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner:= AOwner;
end;

destructor TdxSpreadSheetCustomController.Destroy;
begin
  if (SpreadSheet <> nil) and (SpreadSheet.ActiveController = Self) then
    SpreadSheet.ActiveController := nil;
  FreeAndNil(FPopupMenu);
  inherited Destroy;
end;

function TdxSpreadSheetCustomController.ContextPopup(const P: TPoint): Boolean;
var
  APopupMenuClass: TComponentClass;
begin
  APopupMenuClass := GetPopupMenuClass(P);
  if (FPopupMenu = nil) or (FPopupMenu.ClassType <> APopupMenuClass) then
  begin
    FreeAndNil(FPopupMenu);
    if APopupMenuClass <> nil then
      FPopupMenu := APopupMenuClass.Create(SpreadSheet);
  end;
  Result := (FPopupMenu <> nil) and (FPopupMenu as TdxSpreadSheetCustomPopupMenu).Popup(P);
end;

function TdxSpreadSheetCustomController.CanFocusOnClick: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetCustomController.DblClick;
begin
end;

function TdxSpreadSheetCustomController.GetCursor(const ACursorPos: TPoint): TCursor;
begin
  Result := HitTest.GetCursor(ACursorPos);
end;

function TdxSpreadSheetCustomController.GetDragAndDropObjectClass(const ACursorPos: TPoint): TcxDragAndDropObjectClass;
begin
  Result := HitTest.GetDragAndDropObjectClass(ACursorPos);
end;

function TdxSpreadSheetCustomController.GetHitTest: TdxSpreadSheetCustomHitTest;
begin
  Result := nil;
end;

function TdxSpreadSheetCustomController.GetPopupMenuClass(const ACursorPos: TPoint): TComponentClass;
begin
  Result := HitTest.GetPopupMenuClass(ACursorPos);
end;

function TdxSpreadSheetCustomController.GetZoomFactor: Integer;
begin
  Result := 100;
end;

function TdxSpreadSheetCustomController.IMEComposition(var AMessage: TMessage): Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCustomController.IMEStartComposition: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetCustomController.StopEditing;
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomController.UpdateStates;
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomController.KeyDown(var Key: Word; Shift: TShiftState);
begin
end;

procedure TdxSpreadSheetCustomController.KeyUp(var Key: Word; Shift: TShiftState);
begin
end;

procedure TdxSpreadSheetCustomController.KeyPress(var Key: Char);
begin
end;

procedure TdxSpreadSheetCustomController.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateStates;
end;

procedure TdxSpreadSheetCustomController.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  UpdateStates;
end;

procedure TdxSpreadSheetCustomController.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateStates;
end;

function TdxSpreadSheetCustomController.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; const P: TPoint): Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetCustomController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPressedObject := HitTest.HitObject;
  DoMouseDown(Button, Shift, MulDiv(X, 100, ZoomFactor), MulDiv(Y, 100, ZoomFactor));
end;

procedure TdxSpreadSheetCustomController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  DoMouseMove(Shift, MulDiv(X, 100, ZoomFactor), MulDiv(Y, 100, ZoomFactor));
end;

procedure TdxSpreadSheetCustomController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoMouseUp(Button, Shift, MulDiv(X, 100, ZoomFactor), MulDiv(Y, 100, ZoomFactor));
  FPressedObject := nil;
end;

function TdxSpreadSheetCustomController.MouseWheel(Shift: TShiftState; WheelDelta: Integer; const P: TPoint): Boolean;
begin
  Result := DoMouseWheel(Shift, WheelDelta, cxPointScale(P, 100, ZoomFactor));
end;

{ TdxSpreadSheetCustomViewOptions }

procedure TdxSpreadSheetCustomViewOptions.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomViewOptions.DoChanged;
begin
  if View <> nil then
    View.AddChanges([sscLayout, sscModified]);
end;

function TdxSpreadSheetCustomViewOptions.GetView: TdxSpreadSheetCustomView;
begin
  Result := inherited Owner as TdxSpreadSheetCustomView;
end;

{ TdxSpreadSheetCustomView }

constructor TdxSpreadSheetCustomView.Create(AOwner: TdxCustomSpreadSheet);
begin
  inherited Create(AOwner);
  FVisible := True;
  CreateSubClasses;
  ScaleFactor.Owner := AOwner.ScaleFactor;
  SpreadSheet.DoAddSheet(Self);
end;

destructor TdxSpreadSheetCustomView.Destroy;
begin
  DestroySubClasses;
  SpreadSheet.DoRemoveSheet(Self);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomView.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetCustomView then
  begin
    Caption := TdxSpreadSheetCustomView(Source).Caption;
    Visible := TdxSpreadSheetCustomView(Source).Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TdxSpreadSheetCustomView.BeforeDestruction;
begin
  SpreadSheet.CheckDestroyedView(Self);
  inherited BeforeDestruction;
  FIsDestroying := True;
  cxClearObjectLinks(Self);
end;

procedure TdxSpreadSheetCustomView.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxSpreadSheetCustomView.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    CheckChanges;
end;

procedure TdxSpreadSheetCustomView.Invalidate;
begin
  InvalidateRect(ViewInfo.Bounds);
end;

procedure TdxSpreadSheetCustomView.InvalidateRect(const R: TRect);
var
  ARect: TRect;
begin
  if Visible then
  begin
    if ZoomFactor <> 100 then
      ARect := cxRectInflate(cxRectScale(R, ZoomFactor, 100), 1)
    else
      ARect := R;

    SpreadSheet.InvalidateRect(ARect, False);
  end;
end;

function TdxSpreadSheetCustomView.CanEditContainers: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetCustomView.AddChanges(AChanges: TdxSpreadSheetChanges);
begin
  FChanges := FChanges + AChanges;
  CheckChanges;
end;

procedure TdxSpreadSheetCustomView.Changed;
begin
  AddChanges([sscLayout]);
end;

procedure TdxSpreadSheetCustomView.ChangeScale(M, D: Integer);
begin
  Options.ChangeScale(M, D);
end;

procedure TdxSpreadSheetCustomView.CheckChanges;
begin
  if not (IsDestroying or IsLocked or (Changes = []) or SpreadSheet.FormulaController.CalculationInProcess) then
    DoCheckChanges;
end;

procedure TdxSpreadSheetCustomView.DoCheckChanges;
var
  ADelta: TdxSpreadSheetChanges;
begin
  ADelta := [sscData, sscStyle, sscModified] * Changes;
  if ADelta <> [] then
  begin
    SpreadSheet.AddChanges(ADelta);
    Changes := Changes - ADelta;
  end;

  if not SpreadSheet.ProcessingChanges then
  begin
    if sscOptionsPrint in Changes then
    begin
      Changes := Changes - [sscOptionsPrint] + [sscLayout];
      SpreadSheet.Listeners.NotifyOptionsPrintChanged(Self);
    end;
    if sscLayout in Changes then
    begin
      Changes := Changes - [sscLayout];
      if Active then
        ViewInfo.Recalculate
      else
        ViewInfo.IsDirty := True;

      SpreadSheet.DoLayoutChanged;
    end;
  end;
end;

procedure TdxSpreadSheetCustomView.DoDataChanged;
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomView.InitScrollBarsParameters;
begin
  ViewInfo.InitScrollBarsParameters;
end;

function TdxSpreadSheetCustomView.IsCaptionTextDelimited: Boolean;
begin
  Result := FIsCaptionTextDelimited;
end;

procedure TdxSpreadSheetCustomView.Loaded;
begin
  Exclude(FChanges, sscModified);
end;

procedure TdxSpreadSheetCustomView.RecalculateBestFit;
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomView.SelectionChanged;
begin
  if not IsLocked then
  begin
    Controller.StopEditing;
    ViewInfo.SelectionChanged;
    SpreadSheet.DoSelectionChanged(Self);
  end;
end;

procedure TdxSpreadSheetCustomView.ValidateCaption;
var
  I: Integer;
  J: TdxSpreadSheetFormulaOperation;
begin
  for I := 0 to SpreadSheet.SheetCount - 1 do
  begin
    if (SpreadSheet.Sheets[I] <> Self) and dxSpreadSheetTextIsEqual(SpreadSheet.Sheets[I].Caption, FCaption) then
      Abort;
  end;
  FIsCaptionTextDelimited := False;
  for I := 1 to Length(FCaption) do
  begin
    FIsCaptionTextDelimited := FIsCaptionTextDelimited or
      dxSpreadSheetCharIsEqual(FCaption[I], FormatSettings.Data.DecimalSeparator);
    if not FIsCaptionTextDelimited then
    begin
      for J := Low(FormatSettings.Operations) to High(FormatSettings.Operations) do
        FIsCaptionTextDelimited := FIsCaptionTextDelimited or ((Length(FormatSettings.Operations[J]) > 0) and
          dxSpreadSheetCharIsEqual(FCaption[I], FormatSettings.Operations[J][1]));
    end;
    if FIsCaptionTextDelimited then Break;
  end;
end;

function TdxSpreadSheetCustomView.CreateController: TdxSpreadSheetCustomViewController;
begin
  Result := TdxSpreadSheetCustomViewController.Create(Self);
end;

function TdxSpreadSheetCustomView.CreateContainers: TdxSpreadSheetContainers;
begin
  Result := TdxSpreadSheetContainers.Create(Self);
end;

function TdxSpreadSheetCustomView.CreateHitTest: TdxSpreadSheetCustomHitTest;
begin
  Result := TdxSpreadSheetCustomHitTest.Create(Self);
end;

function TdxSpreadSheetCustomView.CreateOptions: TdxSpreadSheetCustomViewOptions;
begin
  Result := TdxSpreadSheetCustomViewOptions.Create(Self);
end;

function TdxSpreadSheetCustomView.CreateViewInfo: TdxSpreadSheetCustomViewViewInfo;
begin
  Result := TdxSpreadSheetCustomViewViewInfo.Create(Self);
end;

procedure TdxSpreadSheetCustomView.CreateSubClasses;
begin
  FOptions := CreateOptions;
  FViewInfo := CreateViewInfo;
  FContainers := CreateContainers;
  FHitTest := CreateHitTest;
  FController := CreateController;
  FScaleFactor := TdxOwnedScaleFactor.Create;
  FScaleFactor.ListenerAdd(ScaleFactorChangeHandler);
end;

procedure TdxSpreadSheetCustomView.DestroySubClasses;
begin
  Containers.Clear;
  ScaleFactor.ListenerRemove(ScaleFactorChangeHandler);
  FreeAndNil(FScaleFactor);
  FreeAndNil(FViewInfo);
  FreeAndNil(FHitTest);
  FreeAndNil(FController);
  FreeAndNil(FContainers);
  FreeAndNil(FOptions);
end;

function TdxSpreadSheetCustomView.GetContentOrigin: TPoint;
begin
  Result := cxNullPoint;
end;

function TdxSpreadSheetCustomView.GetPartOffsetByPoint(const P: TPoint): TPoint;
begin
  Result := cxNullPoint;
end;

function TdxSpreadSheetCustomView.GetZoomFactor: Integer;
begin
  Result := ScaleFactor.Apply(100);
end;

procedure TdxSpreadSheetCustomView.Pack;
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomView.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  ViewInfo.Scroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

function TdxSpreadSheetCustomView.GetActive: Boolean;
begin
  Result := SpreadSheet.ActiveSheet = self;
end;

function TdxSpreadSheetCustomView.GetBounds: TRect;
begin
  if Visible then
    Result := SpreadSheet.ClientBounds
  else
    Result := cxInvalidRect;
end;

function TdxSpreadSheetCustomView.GetCaption: string;
begin
  Result := FCaption;
end;

function TdxSpreadSheetCustomView.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := SpreadSheet.CellStyles;
end;

function TdxSpreadSheetCustomView.GetIndex: Integer;
begin
  Result := SpreadSheet.FSheets.IndexOf(Self);
end;

function TdxSpreadSheetCustomView.GetIsDestroying: Boolean;
begin
  Result := FIsDestroying or SpreadSheet.IsDestroying;
end;

function TdxSpreadSheetCustomView.GetIsLocked: Boolean;
begin
  Result := (FLockCount > 0) or IsDestroying or SpreadSheet.IsLocked;
end;

function TdxSpreadSheetCustomView.GetFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := SpreadSheet.FormatSettings;
end;

procedure TdxSpreadSheetCustomView.SetActive(AValue: Boolean);
begin
  SpreadSheet.ActiveSheet := Self;
end;

procedure TdxSpreadSheetCustomView.SetCaption(AValue: string);
const
  QuoteChar: Char = '''';
  InvalidChars: array[0..8] of Char = (#0, #3, ':', '\', '/', '*', '?', '[', ']');
var
  AOldCaption: string;
  I, J: Integer;
begin
  I := 1;
  while I <= Length(AValue) do
  begin
    for J := 0 to High(InvalidChars) do
      if dxSpreadSheetCharIsEqual(AValue[I], InvalidChars[J]) then
      begin
        Delete(AValue, I, 1);
        Dec(I);
        Break;
      end;
    Inc(I);
  end;
  while (Length(AValue) > 0) and dxSpreadSheetCharIsEqual(AValue[1], QuoteChar) do
    Delete(AValue, 1, 1);
  while (Length(AValue) > 0) and dxSpreadSheetCharIsEqual(AValue[Length(AValue)], QuoteChar) do
    Delete(AValue, Length(AValue), 1);
  if Length(AValue) > dxSpreadSheetMaxCaptionLength then
    AValue := Copy(AValue, 1, dxSpreadSheetMaxCaptionLength);

  if dxSpreadSheetCompareText(FCaption, AValue) <> 0 then
  begin
    AOldCaption := FCaption;
    try
      FCaption := AValue;
      ValidateCaption;
      SpreadSheet.AddChanges([sscLayout, sscModified]);
    except
      on EAbort do
      begin
        FCaption := AOldCaption;
        raise;
      end;
    end;
  end;
end;

procedure TdxSpreadSheetCustomView.SetIndex(AValue: Integer);
begin
  SpreadSheet.SetSheetIndex(Self, AValue);
end;

procedure TdxSpreadSheetCustomView.SetOptions(AValue: TdxSpreadSheetCustomViewOptions);
begin
  FOptions.Assign(AValue);
end;

procedure TdxSpreadSheetCustomView.SetVisible(AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    SpreadSheet.DoChangeSheetVisibility(Self);
  end;
end;

procedure TdxSpreadSheetCustomView.ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
begin
  ChangeScale(M, D);
end;

{ TdxSpreadSheetCustomViewPainter }

procedure TdxSpreadSheetCustomViewPainter.FlushCache;
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomViewPainter.PrepareCanvasFont(ACanvas: TcxCanvas; AFont: TFont);
begin
  ACanvas.Font.Assign(AFont);
  ACanvas.Font.Height := ScaleFactor.Revert(AFont.Height);
end;

function TdxSpreadSheetCustomViewPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := SpreadSheet.LookAndFeelPainter;
end;

{ TdxSpreadSheetCustomViewViewInfo }

constructor TdxSpreadSheetCustomViewViewInfo.Create(AView: TdxSpreadSheetCustomView);
begin
  inherited Create(AView);
  FContainers := TdxSpreadSheetCellViewInfoList.Create;
  FPainter := CreatePainter;
end;

destructor TdxSpreadSheetCustomViewViewInfo.Destroy;
begin
  FreeAndNil(FContainers);
  FreeAndNil(FPainter);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomViewViewInfo.Calculate;
begin
  FBounds := View.SpreadSheet.ClientBounds;
  FIsDirty := False;
  Painter.FlushCache;
end;

procedure TdxSpreadSheetCustomViewViewInfo.CalculateContainerDependedCells(
  AContainerViewInfo: TdxSpreadSheetContainerViewInfo);
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomViewViewInfo.CalculateContainers;
var
  ACell: TdxSpreadSheetContainerViewInfo;
  AComments: Boolean;
  AContainer: TdxSpreadSheetContainer;
  AIndex: Integer;
  AOrigin: TPoint;
  AVisibleIndex: Integer;
  I: Integer;
begin
  AVisibleIndex := 0;
  AOrigin := View.GetContentOrigin;
  for AComments := False to True do
    for I := 0 to View.Containers.Count - 1 do
    begin
      AContainer := View.Containers[I];
      if AContainer.Visible and (AContainer is TdxSpreadSheetCommentContainer = AComments) then
      begin
        AIndex := Containers.FindItem(AContainer);
        if AIndex >= 0 then
        begin
          Containers.Move(AIndex, AVisibleIndex);
          ACell := TdxSpreadSheetContainerViewInfo(Containers[AVisibleIndex]);
        end
        else
        begin
          ACell := AContainer.CreateViewInfo;
          ACell.SetBounds(cxRectOffset(AContainer.Calculator.CalculateBounds, AOrigin, False));
          Containers.Add(ACell);
        end;
        CalculateContainerDependedCells(ACell);
        Inc(AVisibleIndex);
      end;
    end;
end;

function TdxSpreadSheetCustomViewViewInfo.CalculateContainersHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
var
  AViewInfo: TdxSpreadSheetContainerViewInfo;
  I: Integer;
begin
  for I := Containers.Count - 1 downto 0 do
  begin
    AViewInfo := TdxSpreadSheetContainerViewInfo(Containers[I]);
    if AViewInfo.Selected and AViewInfo.Selection.InitHitTest(AHitTest) then
      Exit(True);
  end;
  Result := Containers.CalculateHitTest(AHitTest, True);
end;

function TdxSpreadSheetCustomViewViewInfo.CreatePainter: TdxSpreadSheetCustomViewPainter;
begin
  Result := TdxSpreadSheetCustomViewPainter.Create(View);
end;

procedure TdxSpreadSheetCustomViewViewInfo.Clear;
begin
  FContainers.Clear;
end;

procedure TdxSpreadSheetCustomViewViewInfo.Draw(ACanvas: TcxCanvas);
begin
end;

procedure TdxSpreadSheetCustomViewViewInfo.Recalculate;
begin
  Calculate;
  if View.Active then
    View.Invalidate;
end;

procedure TdxSpreadSheetCustomViewViewInfo.Validate;
begin
  if IsDirty then
    Calculate;
end;

procedure TdxSpreadSheetCustomViewViewInfo.InitScrollBarsParameters;
begin
end;

procedure TdxSpreadSheetCustomViewViewInfo.Scroll(
  AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
end;

procedure TdxSpreadSheetCustomViewViewInfo.SelectionChanged;
begin
end;

procedure TdxSpreadSheetCustomViewViewInfo.SetIsDirty(AValue: Boolean);
begin
  if AValue <> FIsDirty then
  begin
    if not AValue then
    begin
      Calculate;
      View.Invalidate;
    end
    else
      FIsDirty := AValue;
  end;
end;

{ TdxSpreadSheetViewList }

procedure TdxSpreadSheetViewList.AddChanges(AChanges: TdxSpreadSheetChanges);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].AddChanges(AChanges);
end;

procedure TdxSpreadSheetViewList.CheckChanges;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].CheckChanges;
end;

procedure TdxSpreadSheetViewList.DataChanged;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DoDataChanged;
end;

procedure TdxSpreadSheetViewList.Loaded;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Loaded;
end;

function TdxSpreadSheetViewList.GetItem(Index: Integer): TdxSpreadSheetCustomView;
begin
  Result := TdxSpreadSheetCustomView(List[Index]);
end;

{ TdxSpreadSheetMergedCellsRow }

procedure TdxSpreadSheetMergedCellsRow.AddCell(ACell: TdxSpreadSheetMergedCell);
begin
  TdxSpreadSheetMergedCellsRowItem(inherited Add).Cell := ACell;
end;

function TdxSpreadSheetMergedCellsRow.Contains(AColumn: Integer): TdxSpreadSheetMergedCell;
var
  AItem: TdxSpreadSheetMergedCellsRowItem;
begin
  Result := nil;
  AItem := TdxSpreadSheetMergedCellsRowItem(First);
  while AItem <> nil do
  begin
    if InRange(AColumn, AItem.Cell.Area.Left, AItem.Cell.Area.Right) then
    begin
      Result := AItem.Cell;
      Break;
    end
    else
      AItem := TdxSpreadSheetMergedCellsRowItem(AItem.Next);
  end;
end;

procedure TdxSpreadSheetMergedCellsRow.Enum(AColumn1, AColumn2: Integer; AProc: TdxSpreadSheetMergedCellListEnumCellsProc);
var
  AItem: TdxSpreadSheetMergedCellsRowItem;
begin
  AItem := TdxSpreadSheetMergedCellsRowItem(First);
  while AItem <> nil do
  begin
    if (AItem.Cell.Area.Right >= AColumn1) and (AItem.Cell.Area.Left <= AColumn2) then
      AProc(AItem.Cell);
    AItem := TdxSpreadSheetMergedCellsRowItem(AItem.Next);
  end;
end;

procedure TdxSpreadSheetMergedCellsRow.RemoveCell(ACell: TdxSpreadSheetMergedCell);
var
  AItem: TdxSpreadSheetMergedCellsRowItem;
begin
  AItem := TdxSpreadSheetMergedCellsRowItem(First);
  while AItem <> nil do
  begin
    if AItem.Cell = ACell then
    begin
      Delete(AItem);
      Break;
    end
    else
      AItem := TdxSpreadSheetMergedCellsRowItem(AItem.Next)
  end;
end;

procedure TdxSpreadSheetMergedCellsRow.Union(var AArea: TRect);
var
  R: TRect;
  AItem: TdxSpreadSheetMergedCellsRowItem;
begin
  AItem := TdxSpreadSheetMergedCellsRowItem(First);
  while AItem <> nil do
  begin
    R := AItem.Cell.Area;
    if InRange(AArea.Left, R.Left, R.Right) or InRange(AArea.Right, R.Left, R.Right) then
      AArea := dxSpreadSheetCellsUnion(AArea, R);
    AItem := TdxSpreadSheetMergedCellsRowItem(AItem.Next);
  end;
end;

function TdxSpreadSheetMergedCellsRow.CreateLinkedObject: TcxDoublyLinkedObject;
begin
  Result := TdxSpreadSheetMergedCellsRowItem.Create;
end;

{ TdxSpreadSheetMergedCell }

destructor TdxSpreadSheetMergedCell.Destroy;
begin
  if Owner <> nil then
  begin
    Owner.DoRemoving(Self);
    if Owner.History.CanAddCommand then
      Owner.History.AddCommand(TdxSpreadSheetHistoryMergeCellsCommand.Create(nil, Area));
  end;
  inherited Destroy;
end;

function TdxSpreadSheetMergedCell.Contains(const ARow, AColumn: Integer): Boolean;
begin
  Result := dxSpreadSheetContains(FArea, ARow, AColumn)
end;

function TdxSpreadSheetMergedCell.Intersects(const AArea: TRect): Boolean;
begin
  Result := dxSpreadSheetIntersects(FArea, AArea);
end;

procedure TdxSpreadSheetMergedCell.Initialize(AOwner: TdxSpreadSheetMergedCellList; const AArea: TRect);
begin
  if FOwner <> nil then
    FOwner.RemoveRef(Self);
  FOwner := AOwner;
  FArea := dxSpreadSheetGetRealArea(AArea);
  if FOwner <> nil then
    FOwner.AddRef(Self);
end;

function TdxSpreadSheetMergedCell.GetActiveCell: TdxSpreadSheetCell;
begin
  Result := View.Cells[FArea.Top, FArea.Left];
end;

function TdxSpreadSheetMergedCell.GetHistory: TdxSpreadSheetHistory;
begin
  Result := Owner.History;
end;

function TdxSpreadSheetMergedCell.GetNext: TdxSpreadSheetMergedCell;
begin
  Result := TdxSpreadSheetMergedCell(inherited Next);
end;

function TdxSpreadSheetMergedCell.GetPrev: TdxSpreadSheetMergedCell;
begin
  Result := TdxSpreadSheetMergedCell(inherited Prev);
end;

function TdxSpreadSheetMergedCell.GetView: TdxSpreadSheetTableView;
begin
  Result := FOwner.View;
end;

procedure TdxSpreadSheetMergedCell.SetNext(const Value: TdxSpreadSheetMergedCell);
begin
  inherited Next := Value;
end;

procedure TdxSpreadSheetMergedCell.SetPrev(const Value: TdxSpreadSheetMergedCell);
begin
  inherited Prev := Value;
end;

{ TdxSpreadSheetClipboardArea }

procedure TdxSpreadSheetClipboardArea.Initialize(AView: TdxSpreadSheetCustomView; const AArea: TRect);
begin
  View := AView;
  Area := AArea;
  if View <> nil then
    View.Changed;
end;

function TdxSpreadSheetClipboardArea.IsEmpty: Boolean;
begin
  Result := View = nil;
end;

procedure TdxSpreadSheetClipboardArea.Reset;
begin
  Area := cxInvalidRect;
  if View <> nil then
    View.Changed;
  View := nil;
end;

{ TdxSpreadSheetMergedCellList }

constructor TdxSpreadSheetMergedCellList.Create(AView: TdxSpreadSheetTableView);
begin
  inherited Create;
  Rows := TObjectDictionary<Integer, TdxSpreadSheetMergedCellsRow>.Create([doOwnsValues], 1024);
  FView := AView;
end;

destructor TdxSpreadSheetMergedCellList.Destroy;
begin
  FreeAndNil(Rows);
  inherited Destroy;
end;

procedure TdxSpreadSheetMergedCellList.Add(const AArea: TRect);
var
  ACell: TdxSpreadSheetMergedCell;
begin
  if dxSpreadSheetIsValidArea(AArea) and not dxSpreadSheetIsSingleCellArea(AArea) then
  begin
    View.BeginUpdate;
    try
      if not (sssReading in View.State) and not View.History.InProcess then
        DeleteItemsInArea(AArea, False);
      ACell := TdxSpreadSheetMergedCell(inherited Add);
      Inc(FCount);
      if History.CanAddCommand then
      begin
        History.BeginAction(TdxSpreadSheetHistoryMergeCellsAction);
        try
          History.AddCommand(TdxSpreadSheetHistoryMergeCellsCommand.Create(ACell, AArea));
          InitializeCell(ACell, AArea);
        finally
          View.History.EndAction;
        end;
      end
      else
        InitializeCell(ACell, AArea);
    finally
      View.EndUpdate;
    end;
  end;
end;

function TdxSpreadSheetMergedCellList.CheckCell(ARow, AColumn: Integer): TRect;
begin
  Result := ExpandArea(AColumn, ARow);
end;

procedure TdxSpreadSheetMergedCellList.Clear;
begin
  FIsDeleting := True;
  try
    inherited Clear;
    if Rows <> nil then
      Rows.Clear;
  finally
    FCount := 0;
    FIsDeleting := False;
    Changed;
  end;
end;

procedure TdxSpreadSheetMergedCellList.Delete(ALinkedObject: TcxDoublyLinkedObject);
begin
  ALinkedObject.Free;
  ClearCacheInformation;
end;

procedure TdxSpreadSheetMergedCellList.DeleteItemsInArea(const AArea: TRect; AEnclosedItemsOnly: Boolean = True);
var
  ACellsToRemove: TList<TdxSpreadSheetMergedCell>;
  ADeletingArea: TRect;
  I: Integer;
begin
  View.BeginUpdate;
  try
    ACellsToRemove := TList<TdxSpreadSheetMergedCell>.Create;
    try
      ADeletingArea := AArea;
      EnumCells(ADeletingArea,
        procedure (ACell: TdxSpreadSheetMergedCell)
        var
          R: TRect;
        begin
          if dxSpreadSheetIntersects(ACell.Area, ADeletingArea, R) and (not AEnclosedItemsOnly or cxRectIsEqual(R, ACell.Area)) then
          begin
            if ACellsToRemove.IndexOf(ACell) < 0 then
              ACellsToRemove.Add(ACell);
          end;
        end);

      for I := ACellsToRemove.Count - 1 downto 0 do
        ACellsToRemove[I].Free;
    finally
      ACellsToRemove.Free;
    end;
  finally
    View.EndUpdate;
  end;
end;

procedure TdxSpreadSheetMergedCellList.DoRemoving(ACell: TdxSpreadSheetMergedCell);
begin
  Dec(FCount);
  if not FIsDeleting then
  begin
    RemoveRef(ACell);
    inherited Extract(ACell);
    Changed;
  end;
end;

function TdxSpreadSheetMergedCellList.Find(ACell: TdxSpreadSheetMergedCell): Integer;
var
  AItem: TdxSpreadSheetMergedCell;
begin
  Result := 0;
  AItem := First;
  while (AItem <> nil) and (AItem <> ACell) do
  begin
    Inc(Result);
    AItem := AItem.Next;
  end;
  if AItem = nil then
    Result := -1;
end;

procedure TdxSpreadSheetMergedCellList.InitializeCell(ACell: TdxSpreadSheetMergedCell; const AArea: TRect);
begin
  ACell.Initialize(Self, AArea);
  if not (sssReading in View.State) and not History.InProcess then
    TdxSpreadSheetTableViewMergeCellStyleHelper.Calculate(ACell);
  Changed;
end;

procedure TdxSpreadSheetMergedCellList.AddRef(ACell: TdxSpreadSheetMergedCell);
var
  ARowIndex: Integer;
  ARowCells: TdxSpreadSheetMergedCellsRow;
begin
  for ARowIndex := Max(0, ACell.Area.Top) to ACell.Area.Bottom do
  begin
    if not Rows.TryGetValue(ARowIndex, ARowCells) then
    begin
      ARowCells := TdxSpreadSheetMergedCellsRow.Create;
      Rows.Add(ARowIndex, ARowCells);
    end;
    ARowCells.AddCell(ACell);
  end;
end;

procedure TdxSpreadSheetMergedCellList.RemoveRef(ACell: TdxSpreadSheetMergedCell);
begin
  EnumRows(ACell.Area.Top, ACell.Area.Bottom,
    procedure (ARow: TdxSpreadSheetMergedCellsRow)
    begin
      ARow.RemoveCell(ACell)
    end);
end;

procedure TdxSpreadSheetMergedCellList.EnumCells(AProc: TdxSpreadSheetMergedCellListEnumCellsProc);
begin
  EnumCells(cxRect(0, 0, dxSpreadSheetMaxColumnIndex, dxSpreadSheetMaxRowIndex), AProc);
end;

procedure TdxSpreadSheetMergedCellList.EnumCells(const AArea: TRect; AProc: TdxSpreadSheetMergedCellListEnumCellsProc);
var
  ARowIndex: Integer;
begin
  for ARowIndex in Rows.Keys do
    if (ARowIndex >= AArea.Top) and (ARowIndex <= AArea.Bottom) then
      Rows.Items[ARowIndex].Enum(AArea.Left, AArea.Right, AProc);
end;

procedure TdxSpreadSheetMergedCellList.EnumRows(
  AStartIndex, AFinishIndex: Integer; AProc: TdxSpreadSheetMergedCellListEnumRowsProc);
var
  ARowIndex: Integer;
begin
  AStartIndex := Max(0, AStartIndex);
  for ARowIndex in Rows.Keys do
  begin
    if (ARowIndex >= AStartIndex) and (ARowIndex <= AFinishIndex) then
      AProc(Rows.Items[ARowIndex]);
  end;
end;

function TdxSpreadSheetMergedCellList.ExpandArea(const AArea: TRect): TRect;
var
  AResult: TRect;
  R: TRect;
begin
  if (AArea.Left < 0) or (AArea.Top < 0) or (Count = 0) then
    Exit(AArea);
  if cxRectIsEqual(AArea, CachedSourceArea) then
    Exit(CachedExpandedArea);

  AResult := AArea;
  repeat
    R := AResult;
    EnumRows(AResult.Top, AResult.Bottom,
      procedure (ARow: TdxSpreadSheetMergedCellsRow)
      begin
        ARow.Union(AResult);
      end);
  until ((AResult.Top = R.Top) and (AResult.Bottom = R.Bottom)) or (cxRectWidth(AResult) * cxRectHeight(AResult) = 0);
  CachedSourceArea := AArea;
  CachedExpandedArea := AResult;
  Result := AResult;
end;

function TdxSpreadSheetMergedCellList.ExpandArea(const AColumn, ARow: Integer): TRect;
var
  ACell: TdxSpreadSheetMergedCell;
begin
  ACell := FindCell(ARow, AColumn);
  if ACell <> nil then
    Result := ACell.Area
  else
    Result := cxRect(AColumn, ARow, AColumn, ARow);
end;

procedure TdxSpreadSheetMergedCellList.Extract(const AArea: TRect; ADest: TList<TdxSpreadSheetMergedCell>);
begin
  ADest.Clear;
  ADest.Capacity := Count;
  EnumCells(AArea,
    procedure (ACell: TdxSpreadSheetMergedCell)
    begin
      if ADest.IndexOf(ACell) < 0 then
        ADest.Add(ACell);
    end);
end;

function TdxSpreadSheetMergedCellList.FindCell(ARow, AColumn: Integer): TdxSpreadSheetMergedCell;
var
  APoint: TPoint;
  ARowCells: TdxSpreadSheetMergedCellsRow;
begin
  Result := nil;
  if (ARow < 0) or (AColumn < 0) or (Count = 0) then
    Exit;
  APoint := Point(AColumn, ARow);
  if cxPointIsEqual(CachedPoint, APoint) or ((CachedCell <> nil) and CachedCell.Contains(ARow, AColumn)) then
    Result := CachedCell
  else
  begin
    if Rows.TryGetValue(ARow, ARowCells) then
      Result := ARowCells.Contains(AColumn);
    CachedPoint := APoint;
    CachedCell := Result;
  end;
end;

function TdxSpreadSheetMergedCellList.HasItemsInArea(const AArea: TRect): Boolean;
var
  ACell: TdxSpreadSheetMergedCell;
begin
  Result := False;
  ACell := Last;
  while ACell <> nil do
  begin
    if ACell.Intersects(AArea) then
      Exit(True);
    ACell := ACell.Prev;
  end;
end;

procedure TdxSpreadSheetMergedCellList.Changed;
begin
  View.AddChanges([sscLayout, sscDimension]);
  ClearCacheInformation;
end;

procedure TdxSpreadSheetMergedCellList.ClearCacheInformation;
begin
  CachedCell := nil;
  CachedExpandedArea := cxInvalidRect;
  CachedPoint := cxInvalidPoint;
  CachedSourceArea := cxInvalidRect;
end;

function TdxSpreadSheetMergedCellList.CreateLinkedObject: TcxDoublyLinkedObject;
begin
  Result := TdxSpreadSheetMergedCell.Create;
end;

function TdxSpreadSheetMergedCellList.GetFirst: TdxSpreadSheetMergedCell;
begin
  Result := TdxSpreadSheetMergedCell(inherited First);
end;

function TdxSpreadSheetMergedCellList.GetHistory: TdxSpreadSheetHistory;
begin
  Result := View.History;
end;

function TdxSpreadSheetMergedCellList.GetItem(AIndex: Integer): TdxSpreadSheetMergedCell;
begin
  Result := First;
  while (Result <> nil) and (AIndex > 0) do
  begin
    Result := Result.Next;
    Dec(AIndex)
  end;
end;

function TdxSpreadSheetMergedCellList.GetLast: TdxSpreadSheetMergedCell;
begin
  Result := TdxSpreadSheetMergedCell(inherited Last);
end;

{ TdxSpreadSheetTableItem }

constructor TdxSpreadSheetTableItem.Create(AOwner: TdxDynamicItemList; AIndex: Integer);
begin
  inherited Create(AOwner, AIndex);
  FStyle := TdxSpreadSheetTableItemStyle.Create(Self);
  FDefaultSize := True;
  FVisible := True;
end;

destructor TdxSpreadSheetTableItem.Destroy;
begin
  FreeAndNil(FStyle);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableItem.Assign(ASource: TdxDynamicListItem);
begin
  if ASource is TdxSpreadSheetTableItem then
  begin
    FDefaultSize := TdxSpreadSheetTableItem(ASource).FDefaultSize;
    FIsCustomSize := TdxSpreadSheetTableItem(ASource).FIsCustomSize;
    FSize := TdxSpreadSheetTableItem(ASource).FSize;
    FVisible := TdxSpreadSheetTableItem(ASource).FVisible;
    FStyle.Handle := View.CellStyles.AddClone(TdxSpreadSheetTableItem(ASource).Style.Handle);
  end;
end;

procedure TdxSpreadSheetTableItem.ApplyBestFit;
var
  ASize: Integer;
begin
  if Visible then
  begin
    ASize := CalculateBestFit;
    if ASize = 0 then
      ASize := Owner.DefaultSize;
    if ASize <> Size then
      SetSize(ASize);
    FIsCustomSize := False;
  end;
end;

procedure TdxSpreadSheetTableItem.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if not Owner.FIsDeletion then
    History.AddCommand(TdxSpreadSheetHistoryDeleteItemCommand.Create(Self));
end;

procedure TdxSpreadSheetTableItem.EnumCells(AEnumProc: TdxSpreadSheetTableItemEnumProc);
var
  ACell: TdxSpreadSheetCell;
  I: Integer;
begin
  for I := 0 to CellCount - 1 do
  begin
    ACell := Cells[I];
    if ACell <> nil then
      AEnumProc(ACell);
  end;
end;

procedure TdxSpreadSheetTableItem.AfterResize;
begin
  Owner.AfterResize;
end;

procedure TdxSpreadSheetTableItem.BeforeResize;
begin
  Owner.BeforeResize;
end;

function TdxSpreadSheetTableItem.CalculateBestFit: Integer;
var
  ACanvas: TcxCanvas;
  APrevDPI: Integer;
  ASize: Integer;
begin
  ACanvas := cxScreenCanvas;
  APrevDPI := dxSpreadSheetPrepareCanvas(ACanvas, View.SpreadSheet.Font, dxDefaultDPI);
  try
    ASize := 0;
    EnumCells(
      procedure (ACell: TdxSpreadSheetCell)
      begin
        ASize := Max(ASize, MeasureCellSize(ACanvas, ACell));
      end);
  finally
    dxSpreadSheetUnprepareCanvas(ACanvas, APrevDPI);
  end;
  Result := ASize;
end;

procedure TdxSpreadSheetTableItem.Changed;
begin
  Owner.Changed;
end;

function TdxSpreadSheetTableItem.GetDisplayText: string;
begin
  Result := Owner.GetItemDisplayText(Self);
end;

function TdxSpreadSheetTableItem.GetHistory: TdxSpreadSheetHistory;
begin
  Result := Owner.SpreadSheet.History;
end;

function TdxSpreadSheetTableItem.GetNext: TdxSpreadSheetTableItem;
begin
  Result := TdxSpreadSheetTableItem(FNext);
end;

function TdxSpreadSheetTableItem.GetOwner: TdxSpreadSheetTableItems;
begin
  Result := TdxSpreadSheetTableItems(FOwner);
end;

function TdxSpreadSheetTableItem.GetPrev: TdxSpreadSheetTableItem;
begin
  Result := TdxSpreadSheetTableItem(FPrev);
end;

function TdxSpreadSheetTableItem.GetSize: Integer;
begin
  if not Visible then
    Result := 0
  else
    if DefaultSize then
      Result := Owner.DefaultSize
    else
      Result := FSize;
end;

function TdxSpreadSheetTableItem.GetView: TdxSpreadSheetTableView;
begin
  Result := Owner.View;
end;

procedure TdxSpreadSheetTableItem.SetVisible(AValue: Boolean);
begin
  BeforeResize;
  try
    History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
    try
      if History.CanAddCommand then
        History.AddCommand(TdxSpreadSheetHistoryChangeItemCommand.Create(Self));
      if (AValue <> FVisible) or AValue and (FSize = 0) then
      begin
        if AValue and (FSize = 0) then
          DefaultSize := True;
        FVisible := AValue;
        Changed;
      end;
    finally
      History.EndAction;
    end;
  finally
    AfterResize;
  end;
end;

procedure TdxSpreadSheetTableItem.CellStyleChanged;
begin
  // do nothing
end;

procedure TdxSpreadSheetTableItem.CellStyleChanging;
begin
  if History.CanAddCommand then
    History.AddCommand(TdxSpreadSheetHistoryChangeCellStyleCommand.Create(Self));
end;

function TdxSpreadSheetTableItem.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := View.SpreadSheet.CellStyles;
end;

function TdxSpreadSheetTableItem.GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
begin
  Result := View.FormatSettings;
end;

procedure TdxSpreadSheetTableItem.ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);
var
  ACell: TdxSpreadSheetCell;
  AList: TdxFastList;
  APrevStyleIsDefault: Boolean;
  AStyleMergeHelper: TdxSpreadSheetCellStyleMergeHelper;
  I: Integer;
begin
  AStyleMergeHelper := TdxSpreadSheetCellStyleMergeHelper.Create(APrevStyle, ANewStyle);
  try
    View.BeginUpdate;
    try
      if View.FMergeCellStylesLockCount = 0 then
      begin
        APrevStyleIsDefault := APrevStyle = View.CellStyles.DefaultStyle;
        AList := TdxFastList.Create(CellCount);
        try
          Owner.GetOppositeItems.ForEach(
            procedure (AItem: TdxDynamicListItem)
            var
              ATableItem: TdxSpreadSheetTableItem;
            begin
              ATableItem := TdxSpreadSheetTableItem(AItem);
              ACell := ATableItem.GetCell(Index);
              if (ACell = nil) and not ATableItem.Style.IsDefault then
              begin
                ACell := ATableItem.CreateCell(Index);
                if APrevStyleIsDefault then
                  ACell.StyleHandle := ATableItem.Style.Handle;
              end;
              if ACell <> nil then
                AList.Add(ACell);
            end);

          for I := 0 to AList.Count - 1 do
            AStyleMergeHelper.ProcessCellStyle(TdxSpreadSheetCell(AList.List[I]).Style);
        finally
          AList.Free;
        end;
      end;
      if not IsCustomSize and AStyleMergeHelper.IsUpdateBestFitNeeded then
        MarkBestFitDirty;
    finally
      View.EndUpdate;
    end;
  finally
    AStyleMergeHelper.Free;
  end;
end;

function TdxSpreadSheetTableItem.IsValueDefined(AIndex: Integer): Boolean;
var
  ACell: TdxSpreadSheetCell;
begin
  ACell := GetCell(AIndex);
  Result := (ACell <> nil) and ACell.HasValue;
end;

procedure TdxSpreadSheetTableItem.MarkBestFitDirty;
begin
  // do nothing
end;

procedure TdxSpreadSheetTableItem.SetDefaultSize(AValue: Boolean);
begin
  if FDefaultSize <> AValue then
  begin
    BeforeResize;
    try
      FDefaultSize := AValue;
      FIsCustomSize := not DefaultSize;
    finally
      AfterResize;
    end;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableItem.SetSize(const AValue: Integer);
var
  AIsReading: Boolean;
begin
  if DefaultSize or (Size <> AValue) then
  begin
    AIsReading := sssReading in View.State;
    if not AIsReading then
      BeforeResize;
    History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
    try
      if History.CanAddCommand then
        History.AddCommand(TdxSpreadSheetHistoryChangeItemCommand.Create(Self));
      FSize := AValue;
      FDefaultSize := False;
      FIsCustomSize := True;
      FVisible := True;
    finally
      History.EndAction;
      if not AIsReading then
      begin
        AfterResize;
        Changed;
      end;
    end;
  end;
end;

procedure TdxSpreadSheetTableItem.SetSizeEx(const ASize: TSize);
begin
  SetSize(ASize.cx);
end;

{ TdxSpreadSheetTableItemGroup }

constructor TdxSpreadSheetTableItemGroup.Create(AOwner: TdxSpreadSheetTableItems;
  AParent: TdxSpreadSheetTableItemGroup; AStartIndex, AFinishIndex: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FChildren := TdxSpreadSheetTableItemGroupList.Create(Self);

  if AParent <> nil then
  begin
    History.BeginAction(TdxSpreadSheetHistoryAddGroupAction);
    try
      AParent.BeginUpdate;
      try
        ValidateIndex(AStartIndex);
        ValidateIndex(AFinishIndex);
        FFinishIndex := Max(AFinishIndex, AStartIndex);
        FStartIndex := Min(AStartIndex, AFinishIndex);
        SetParent(AParent);
      finally
        AParent.EndUpdate;
      end;
    finally
      History.EndAction;
    end;
  end;
end;

destructor TdxSpreadSheetTableItemGroup.Destroy;
begin
  History.BeginAction(TdxSpreadSheetHistoryDeleteGroupAction);
  try
    DeleteAll;
    SetParent(nil);
    FreeAndNil(FChildren);
  finally
    History.EndAction;
  end;
  cxClearObjectLinks(Self);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableItemGroup.BeginUpdate;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
  begin
    Owner.View.BeginUpdate;
    FChildren.BeginUpdate;
  end;
end;

procedure TdxSpreadSheetTableItemGroup.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
  begin
    FChildren.EndUpdate;
    Owner.View.EndUpdate;
    if FHasChanges then
      Changed;
  end;
end;

procedure TdxSpreadSheetTableItemGroup.DeleteAll;
begin
  while FChildren.Count > 0 do
    FChildren.Last.Free;
end;

function TdxSpreadSheetTableItemGroup.Find(AIndex: Integer; ARecursive: Boolean = True): TdxSpreadSheetTableItemGroup;
begin
  Result := nil;
  if InRange(AIndex, StartIndex, FinishIndex) then
  begin
    Result := FChildren.Find(AIndex, ARecursive);
    if (Result = nil) and not IsRoot then
      Result := Self;
  end;
end;

procedure TdxSpreadSheetTableItemGroup.ToggleExpanded;
begin
  Expanded := not Expanded;
end;

procedure TdxSpreadSheetTableItemGroup.AddChild(AStartIndex, AFinishIndex: Integer);
begin
  BeginUpdate;
  try
    TdxSpreadSheetTableItemGroup.Create(Owner, Self, AStartIndex, AFinishIndex);
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableItemGroup.Changed;
begin
  FHasChanges := True;
  if FLockCount = 0 then
  begin
    FHasChanges := False;
    DoChanged;
  end;
end;

procedure TdxSpreadSheetTableItemGroup.CheckRange;
var
  AStartIndex, AFinishIndex: Integer;
begin
  FChildren.Validate;
  if FChildren.GetRange(AStartIndex, AFinishIndex) then
  begin
    BeginUpdate;
    try
      StartIndex := Min(StartIndex, AStartIndex);
      FinishIndex := Max(FinishIndex, AFinishIndex);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetTableItemGroup.DoChanged;
var
  ALink: TcxObjectLink;
begin
  FChildren.ValidateRanges(StartIndex, FinishIndex);
  if Parent <> nil then
  begin
    ALink := cxAddObjectLink(Self);
    try
      Parent.CheckRange;
      if ALink.Ref <> nil then
        Parent.Changed;
    finally
      cxRemoveObjectLink(ALink);
    end;
  end
  else
    Owner.Changed;
end;

procedure TdxSpreadSheetTableItemGroup.Exclude(AIndex: Integer);
var
  ASecondPart: TdxSpreadSheetTableItemGroup;
  I: Integer;
begin
  if AIndex = StartIndex then
  begin
    if StartIndex = FinishIndex then
      Free
    else
      StartIndex := AIndex + 1;
  end
  else
    if AIndex = FinishIndex then
      FinishIndex := AIndex - 1
    else
    begin
      Parent.BeginUpdate;
      try
        ASecondPart := TdxSpreadSheetTableItemGroup.Create(Owner, Parent, AIndex + 1, FinishIndex);
        for I := Count - 1 downto 0 do
        begin
          if Items[I].StartIndex > AIndex then
            Items[I].SetParent(ASecondPart);
        end;
        FinishIndex := AIndex - 1;
      finally
        Parent.EndUpdate;
      end;
    end;
end;

function TdxSpreadSheetTableItemGroup.HasItemsOnThisLevel: Boolean;
var
  AValue: Integer;
  I: Integer;
begin
  AValue := FinishIndex - StartIndex + 1;
  for I := 0 to Count - 1 do
    Dec(AValue, Items[I].FinishIndex - Items[I].StartIndex + 1);
  Result := AValue > 0;
end;

function TdxSpreadSheetTableItemGroup.IsCollapsedByUser: Boolean;
begin
  if FCollapsedByUser then
    FCollapsedByUser := not Expanded;
  Result := FCollapsedByUser;
end;

function TdxSpreadSheetTableItemGroup.IsParentExpanded: Boolean;
begin
  Result := IsRoot or not Parent.IsCollapsedByUser and Parent.IsParentExpanded;
end;

function TdxSpreadSheetTableItemGroup.IsRoot: Boolean;
begin
  Result := Parent = nil;
end;

procedure TdxSpreadSheetTableItemGroup.SetParent(AParent: TdxSpreadSheetTableItemGroup);
var
  ACommand: TdxSpreadSheetHistoryChangeGroupParentCommand;
begin
  if AParent <> nil then
  begin
    if AParent.Level + MaxNestingLevel + 2 > dxSpreadSheetMaxOutlineLevel then
      raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorMaxOutlineLevel));
  end;
  if AParent <> Parent then
  begin
    History.BeginAction(TdxSpreadSheetHistoryChangeGroupAction);
    try
      ACommand := TdxSpreadSheetHistoryChangeGroupParentCommand.Create(Self);
      ACommand.Changing(Self);
      try
        if Parent <> nil then
        begin
          FParent.FChildren.Extract(Self);
          FParent.Changed;
          FParent := nil;
        end;
        if AParent <> nil then
        begin
          FParent := AParent;
          FParent.FChildren.Add(Self);
          FParent.FChildren.Sort;
        end;
      finally
        ACommand.Changed(Self);
      end;
      History.AddCommand(ACommand);
    finally
      History.EndAction;
    end;
    Changed;
  end;
end;

function TdxSpreadSheetTableItemGroup.GetCount: Integer;
begin
  Result := FChildren.Count;
end;

function TdxSpreadSheetTableItemGroup.GetExpandButtonPosition: TdxSpreadSheetTableItemGroupExpandButtonPosition;
begin
  Result := Owner.Groups.ExpandButtonPosition;
end;

function TdxSpreadSheetTableItemGroup.GetExpanded: Boolean;
var
  AItem: TdxSpreadSheetTableItem;
begin
  AItem := Owner.First;
  while (AItem <> nil) and (AItem.Index < StartIndex) do
    AItem := AItem.Next;
  while (AItem <> nil) and (AItem.Index <= FinishIndex) do
  begin
    if AItem.Visible then
      Exit(True);
    if AItem.Index = FinishIndex then
      Exit(False);
    AItem := AItem.Next;
  end;
  Result := (AItem = nil) or (AItem.Index > FinishIndex);
end;

function TdxSpreadSheetTableItemGroup.GetHistory: TdxSpreadSheetHistory;
begin
  Result := Owner.View.History;
end;

function TdxSpreadSheetTableItemGroup.GetIndex: Integer;
begin
  if Parent <> nil then
    Result := Parent.FChildren.IndexOf(Self)
  else
    Result := -1;
end;

function TdxSpreadSheetTableItemGroup.GetItem(Index: Integer): TdxSpreadSheetTableItemGroup;
begin
  Result := FChildren.Items[Index];
end;

function TdxSpreadSheetTableItemGroup.GetLevel: Integer;
begin
  if Parent <> nil then
    Result := Parent.Level + 1
  else
    Result := -1;
end;

function TdxSpreadSheetTableItemGroup.GetMaxNestingLevel: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Max(Result, Items[I].MaxNestingLevel);
  Inc(Result);
end;

procedure TdxSpreadSheetTableItemGroup.SetExpanded(AValue: Boolean);

  procedure ChangeVisibility(AStartIndex, AFinishIndex: Integer; AValue: Boolean);
  var
    I: Integer;
  begin
    if AFinishIndex >= AStartIndex then
    begin
      if AValue then
      begin
        Owner.ForEach(
          procedure (AItem: TdxDynamicListItem)
          begin
            TdxSpreadSheetTableItem(AItem).Visible := AValue;
          end,
          AStartIndex, AFinishIndex);
      end
      else
        for I := AStartIndex to AFinishIndex do
          Owner.CreateItem(I).Visible := AValue;
    end;
  end;

var
  AGroup: TdxSpreadSheetTableItemGroup;
  AIndex: Integer;
  I: Integer;
begin
  FCollapsedByUser := not AValue;
  if (Expanded <> AValue) and IsParentExpanded then
  begin
    Owner.View.BeginUpdate;
    Owner.BeforeResize;
    try
      History.BeginAction(TdxSpreadSheetHistoryExpandGroupAction);
      try
        if AValue then
        begin
          AIndex := StartIndex;
          for I := 0 to Count - 1 do
          begin
            AGroup := Items[I];
            ChangeVisibility(AIndex, AGroup.StartIndex - 1, AValue);
            if not AGroup.IsCollapsedByUser then
              AGroup.Expanded := True;
            AIndex := AGroup.FinishIndex + 1;
          end;
          ChangeVisibility(AIndex, FinishIndex, AValue);
        end
        else
          ChangeVisibility(StartIndex, FinishIndex, AValue);
      finally
        History.EndAction;
      end;
    finally
      Owner.AfterResize;
      Owner.View.EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetTableItemGroup.SetFinishIndex(AValue: Integer);
begin
  ValidateIndex(AValue);
  if AValue <> FFinishIndex then
  begin
    History.BeginAction(TdxSpreadSheetHistoryChangeGroupAction);
    try
      BeginUpdate;
      try
        if History.CanAddCommand then
          History.AddCommand(TdxSpreadSheetHistoryChangeGroupRangeCommand.Create(Self));
        FFinishIndex := AValue;
        FStartIndex := Min(StartIndex, FinishIndex);
        Changed;
      finally
        EndUpdate;
      end;
    finally
      History.EndAction;
    end;
  end;
end;

procedure TdxSpreadSheetTableItemGroup.SetStartIndex(AValue: Integer);
begin
  ValidateIndex(AValue);
  if AValue <> FStartIndex then
  begin
    History.BeginAction(TdxSpreadSheetHistoryChangeGroupAction);
    try
      BeginUpdate;
      try
        if History.CanAddCommand then
          History.AddCommand(TdxSpreadSheetHistoryChangeGroupRangeCommand.Create(Self));
        FStartIndex := AValue;
        FFinishIndex := Max(StartIndex, FinishIndex);
        Changed;
      finally
        EndUpdate;
      end;
    finally
      History.EndAction;
    end;
  end;
end;

procedure TdxSpreadSheetTableItemGroup.ValidateIndex(var AIndex: Integer);
begin
  AIndex := Min(Max(AIndex, 0), Owner.GetMaxItemIndex);
end;

{ TdxSpreadSheetTableItemGroupComparer }

function TdxSpreadSheetTableItemGroupComparer.Compare(const Left, Right: TdxSpreadSheetTableItemGroup): Integer;
begin
  Result := Left.StartIndex - Right.StartIndex;
end;

{ TdxSpreadSheetTableItemGroupList }

constructor TdxSpreadSheetTableItemGroupList.Create(AOwner: TdxSpreadSheetTableItemGroup);
begin
  inherited Create(TdxSpreadSheetTableItemGroupComparer.Create);
  FOwner := AOwner;
end;

procedure TdxSpreadSheetTableItemGroupList.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxSpreadSheetTableItemGroupList.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Validate;
end;

function TdxSpreadSheetTableItemGroupList.Find(AIndex: Integer; ARecursive: Boolean = True): TdxSpreadSheetTableItemGroup;
var
  AItem: TdxSpreadSheetTableItemGroup;
  I: Integer;
begin
  Result := nil;
  if ARecursive then
  begin
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].Find(AIndex, ARecursive);
      if Result <> nil then
        Break;
    end;
  end
  else
    for I := 0 to Count - 1 do
    begin
      AItem := Items[I];
      if InRange(AIndex, AItem.StartIndex, AItem.FinishIndex) then
        Exit(AItem);
    end;
end;

function TdxSpreadSheetTableItemGroupList.GetRange(out AStartIndex, AFinishIndex: Integer): Boolean;
var
  AItem: TdxSpreadSheetTableItemGroup;
  I: Integer;
begin
  Result := Count > 0;
  if Result then
  begin
    AStartIndex := First.StartIndex;
    AFinishIndex := First.FinishIndex;
    for I := 1 to Count - 1 do
    begin
      AItem := Items[I];
      AStartIndex := Min(AStartIndex, AItem.StartIndex);
      AFinishIndex := Max(AFinishIndex, AItem.FinishIndex);
    end;
  end;
end;

procedure TdxSpreadSheetTableItemGroupList.Validate(ARecursive: Boolean = False);
var
  AItem1: TdxSpreadSheetTableItemGroup;
  AItem2: TdxSpreadSheetTableItemGroup;
  I, J: Integer;
begin
  if (FLockCount = 0) and (Count > 1) and not FOwner.History.InProcess then
  begin
    Inc(FLockCount);
    try
      Sort;
      for I := Count - 1 downto 1 do
      begin
        AItem1 := Items[I];
        AItem2 := Items[I - 1];
        if AItem1.StartIndex <= AItem2.FinishIndex + 1 then
        begin
          AItem2.BeginUpdate;
          try
            AItem2.FCollapsedByUser := AItem2.FCollapsedByUser or AItem1.FCollapsedByUser;
            AItem2.FinishIndex := AItem1.FinishIndex;
            for J := AItem1.Count - 1 downto 0 do
              AItem1[J].SetParent(AItem2);
            AItem1.Free;
          finally
            AItem2.EndUpdate;
          end;
        end;
      end;

      if ARecursive then
      begin
        for I := 0 to Count - 1 do
          Items[I].FChildren.Validate(ARecursive);
      end;
    finally
      Dec(FLockCount);
    end;
  end;
end;

procedure TdxSpreadSheetTableItemGroupList.ValidateRanges(AStartIndex, AFinishIndex: Integer);
var
  AItem: TdxSpreadSheetTableItemGroup;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    AItem := Items[I];
    AItem.BeginUpdate;
    try
      AItem.StartIndex := Max(AItem.StartIndex, AStartIndex);
      AItem.FinishIndex := Min(AItem.FinishIndex, AFinishIndex);
    finally
      AItem.EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetTableItemGroupList.Notify(const Item: TdxSpreadSheetTableItemGroup; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if Action = cnAdded then
    Validate;
end;

{ TdxSpreadSheetTableItemGroups }

constructor TdxSpreadSheetTableItemGroups.Create(AOwner: TdxSpreadSheetTableItems);
begin
  inherited Create;
  FRoot := TdxSpreadSheetTableItemGroup.Create(AOwner, nil, 0, 0);
  FExpandButtonPosition := gebpGroupFinish;
end;

destructor TdxSpreadSheetTableItemGroups.Destroy;
begin
  FreeAndNil(FRoot);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableItemGroups.BeginUpdate;
begin
  Root.BeginUpdate;
end;

procedure TdxSpreadSheetTableItemGroups.EndUpdate;
begin
  Root.EndUpdate;
end;

procedure TdxSpreadSheetTableItemGroups.Add(AIndex: Integer);
var
  AGroup: TdxSpreadSheetTableItemGroup;
begin
  AGroup := Find(AIndex);
  if AGroup = nil then
    AGroup := Root;
  AGroup.AddChild(AIndex, AIndex);
end;

procedure TdxSpreadSheetTableItemGroups.Add(AStartIndex, AFinishIndex: Integer);
var
  AGroup: TdxSpreadSheetTableItemGroup;
  AIndex: Integer;
  I: Integer;
begin
  Root.History.BeginAction(TdxSpreadSheetHistoryAddGroupAction);
  try
    BeginUpdate;
    try
      while AStartIndex <= AFinishIndex do
      begin
        AGroup := Find(AStartIndex);
        if AGroup = nil then
          AGroup := Root;

        AIndex := AFinishIndex;
        if AGroup <> Root then
          AIndex := Min(AGroup.FinishIndex, AIndex);
        for I := 0 to AGroup.Count - 1 do
          if AGroup[I].StartIndex >= AStartIndex then
          begin
            AIndex := Min(AIndex, AGroup[I].StartIndex - 1);
            Break;
          end;

        AGroup.AddChild(AStartIndex, AIndex);
        AStartIndex := AIndex + 1;
      end;
    finally
      EndUpdate;
    end;
  finally
    Root.History.EndAction;
  end;
end;

procedure TdxSpreadSheetTableItemGroups.Delete(AStartIndex, AFinishIndex: Integer);
var
  AGroup: TdxSpreadSheetTableItemGroup;
begin
  dxSpreadSheetValidate(AStartIndex, 0, Root.Owner.GetMaxItemIndex);
  dxSpreadSheetValidate(AFinishIndex, 0, Root.Owner.GetMaxItemIndex);

  Root.History.BeginAction(TdxSpreadSheetHistoryDeleteGroupAction);
  try
    BeginUpdate;
    try
      while AStartIndex <= AFinishIndex do
      begin
        AGroup := Find(AStartIndex);
        if AGroup <> nil then
          AGroup.Exclude(AStartIndex);
        Inc(AStartIndex);
      end;
    finally
      EndUpdate;
    end;
  finally
    Root.History.EndAction;
  end;
end;

procedure TdxSpreadSheetTableItemGroups.DeleteAll;
begin
  Root.DeleteAll;
end;

procedure TdxSpreadSheetTableItemGroups.ExpandToLevel(ALevel: Integer);

  procedure CollapseSubGroups(AGroup: TdxSpreadSheetTableItemGroup);
  var
    I: Integer;
  begin
    for I := 0 to AGroup.Count - 1 do
      AGroup[I].Expanded := False;
  end;

  procedure ExpandTo(AGroup: TdxSpreadSheetTableItemGroup);
  begin
    while not AGroup.IsRoot do
    begin
      AGroup.Expanded := True;
      AGroup := AGroup.Parent;
    end;
  end;

  procedure CheckExpandLevel(AGroup: TdxSpreadSheetTableItemGroup; ALevel: Integer);
  var
    ASubGroup: TdxSpreadSheetTableItemGroup;
    I: Integer;
  begin
    for I := 0 to AGroup.Count - 1 do
    begin
      ASubGroup := AGroup[I];
      if (ASubGroup.Count = 0) or (ALevel = 0) then
      begin
        CollapseSubGroups(ASubGroup);
        if ASubGroup.HasItemsOnThisLevel then
          ExpandTo(ASubGroup);
      end;
      if ALevel > 0 then
        CheckExpandLevel(ASubGroup, ALevel - 1);
    end;
  end;

begin
  BeginUpdate;
  try
    Root.History.BeginAction(TdxSpreadSheetHistoryExpandGroupAction);
    try
      if ALevel >= 0 then
        CheckExpandLevel(Root, ALevel)
      else
        CollapseSubGroups(Root);
    finally
      Root.History.EndAction;
    end;
  finally
    EndUpdate;
  end;
end;

function TdxSpreadSheetTableItemGroups.Find(AIndex: Integer; ARecursive: Boolean = True): TdxSpreadSheetTableItemGroup;
begin
  Result := Root.Find(AIndex, ARecursive)
end;

procedure TdxSpreadSheetTableItemGroups.Validate;
begin
  Root.FChildren.Validate(True);
end;

function TdxSpreadSheetTableItemGroups.GetCount: Integer;
begin
  Result := Root.Count;
end;

function TdxSpreadSheetTableItemGroups.GetItem(Index: Integer): TdxSpreadSheetTableItemGroup;
begin
  Result := Root.Items[Index];
end;

procedure TdxSpreadSheetTableItemGroups.SetExpandButtonPosition(AValue: TdxSpreadSheetTableItemGroupExpandButtonPosition);
begin
  if FExpandButtonPosition <> AValue then
  begin
    FExpandButtonPosition := AValue;
    Root.Owner.Changed;
  end;
end;

{ TdxSpreadSheetTableItems }

constructor TdxSpreadSheetTableItems.Create(AOwner: TdxSpreadSheetTableView);
begin
  inherited Create;
  FView := AOwner;
  FGroups := TdxSpreadSheetTableItemGroups.Create(Self);
end;

destructor TdxSpreadSheetTableItems.Destroy;
begin
  FreeAndNil(FGroups);
  inherited Destroy;
end;

function TdxSpreadSheetTableItems.CreateItem(const AIndex: Integer): TdxSpreadSheetTableItem;
begin
  Result := TdxSpreadSheetTableItem(inherited CreateItem(AIndex));
end;

function TdxSpreadSheetTableItems.GetDistance(AStartIndex, AFinishIndex: Integer): Integer;
var
  ADefaultSize: Integer;
  APrevItemIndex: Integer;
  ASize: Integer;
begin
  AStartIndex := Min(AStartIndex, GetMaxItemIndex + 1);
  AFinishIndex := Min(AFinishIndex, GetMaxItemIndex + 1);

  if AFinishIndex >= AStartIndex then
  begin
    ASize := 0;
    APrevItemIndex := AStartIndex;
    ADefaultSize := DefaultSize;

    ForEach(
      procedure (AItem: TdxDynamicListItem)
      begin
        Inc(ASize, (AItem.Index - APrevItemIndex) * ADefaultSize);
        Inc(ASize, TdxSpreadSheetTableItem(AItem).Size);
        APrevItemIndex := AItem.Index + 1;
      end,
      AStartIndex, AFinishIndex);

    Result := ASize + (AFinishIndex - APrevItemIndex + 1) * ADefaultSize;
  end
  else
    Result := 0;
end;

function TdxSpreadSheetTableItems.GetItemIndexFromDistance(AStartIndex: Integer; ADistance: Integer): Integer;
var
  AInc: Integer;
begin
  AInc := Sign(ADistance);
  Result := AStartIndex + AInc;
  if AInc = 0 then Exit;
  ADistance := Abs(ADistance);
  while InRange(Result, 1, GetMaxItemIndex - 1) and (ADistance > 0) do
  begin
    Dec(ADistance, GetItemSize(Result));
    if ADistance > 0 then
      Result := Result + AInc;
  end;
end;

function TdxSpreadSheetTableItems.GetNextVisibleItemIndex(AIndex: Integer; AGoForward: Boolean): Integer;
begin
  if not GetNextVisibleItemIndex(AIndex, AGoForward, Result) then
    Result := AIndex;
end;

function TdxSpreadSheetTableItems.GetNextVisibleItemIndex(AIndex: Integer; AGoForward: Boolean; out AResult: Integer): Boolean;
begin
  AResult := AIndex + ValueIncr[AGoForward];
  while InRange(AResult, 0, GetMaxItemIndex) and not GetItemVisible(AResult) do
    Inc(AResult, ValueIncr[AGoForward]);
  Result := InRange(AResult, 0, GetMaxItemIndex) and GetItemVisible(AResult);
end;

function TdxSpreadSheetTableItems.GetPosition(AIndex: Integer): Integer;
var
  ADefaultSize: Integer;
  APrevItemIndex: Integer;
  ASize: Integer;
begin
  Result := 0;
  if AIndex >= 0 then
  begin
    if AIndex < FPrevIndex then
    begin
      FPrevSize := 0;
      FPrevIndex := 0;
    end;
    ASize := FPrevSize;
    APrevItemIndex := FPrevIndex;
    ADefaultSize := DefaultSize;
    if FPrevIndex <> AIndex then
    begin
      ForEach(
        procedure (AItem: TdxDynamicListItem)
        begin
          Inc(ASize, (AItem.Index - APrevItemIndex) * ADefaultSize);
          Inc(ASize, TdxSpreadSheetTableItem(AItem).Size);
          APrevItemIndex := AItem.Index + 1;
        end,
        APrevItemIndex, AIndex - 1);
    end;
    Result := ASize + (AIndex - APrevItemIndex) * ADefaultSize;
    FPrevIndex := AIndex;
    FPrevSize := Result;
    if (GetFixedItemIndex >= 0) and (AIndex > GetFixedItemIndex) then
      Inc(Result, View.SpreadSheet.OptionsView.FrozenPaneSeparatorWidth - 1);
  end;
end;

procedure TdxSpreadSheetTableItems.AfterResize;
begin
  if not View.History.InProcess then
    View.Containers.AfterResize;
end;

procedure TdxSpreadSheetTableItems.BeforeResize;
begin
  if not View.History.InProcess then
    View.Containers.BeforeResize;
end;

procedure TdxSpreadSheetTableItems.Changed;
begin
  FPrevIndex := 0;
  FPrevSize := 0;
  View.AddChanges([sscLayout, sscData, sscModified]);
end;

function TdxSpreadSheetTableItems.GetItemDisplayText(const AItem: TdxSpreadSheetTableItem): string;
begin
  Result := GetItemText(AItem.Index);
end;

function TdxSpreadSheetTableItems.GetItemSize(AIndex: Integer): Integer;
var
  AItem: TdxSpreadSheetTableItem;
begin
  AItem := Items[AIndex];
  if AItem <> nil then
    Result := AItem.Size
  else
    Result := DefaultSize;
end;

function TdxSpreadSheetTableItems.GetItemState(AIndex: Integer): TcxButtonState;
begin
  Result := cxbsDefault;
end;

function TdxSpreadSheetTableItems.GetItemStyle(AIndex: Integer): TdxSpreadSheetCellStyle;
var
  AItem: TdxSpreadSheetTableItem;
begin
  AItem := GetItem(AIndex);
  if AItem <> nil then
    Result := AItem.Style
  else
    Result := nil;
end;

function TdxSpreadSheetTableItems.GetItemStyleHandle(AIndex: Integer): TdxSpreadSheetCellStyleHandle;
var
  AItem: TdxSpreadSheetTableItem;
begin
  AItem := GetItem(AIndex);
  if AItem <> nil then
    Result := AItem.Style.Handle
  else
    Result := nil;
end;

function TdxSpreadSheetTableItems.GetItemText(AIndex: Integer): string;
begin
  Result := IntToStr(AIndex + 1);
end;

function TdxSpreadSheetTableItems.GetItemVisible(AIndex: Integer): Boolean;
var
  AItem: TdxSpreadSheetTableItem;
begin
  AItem := GetItem(AIndex);
  Result := (AItem = nil) or (AItem.Size > 0);
end;

function TdxSpreadSheetTableItems.GetMaxItemIndex: Integer;
begin
  Result := MaxInt;
end;

function TdxSpreadSheetTableItems.GetRealItemSize(AIndex: Integer): Integer;
var
  AItem: TdxSpreadSheetTableItem;
begin
  Result := DefaultSize;
  AItem := Items[AIndex];
  if AItem <> nil then
  begin
    if AItem.Visible then
      Result := AItem.Size
    else
      if not AItem.DefaultSize then
        Result := AItem.CustomSize;
  end;
end;

procedure TdxSpreadSheetTableItems.ResizeItem(AIndex, ADelta: Integer);
var
  AItem: TdxSpreadSheetTableItem;
begin
  if ADelta > 0 then
  begin
    if not GetItemVisible(AIndex + 1) then
    begin
      Inc(AIndex);
      while not GetItemVisible(AIndex + 1) do
        Inc(AIndex);
      Items[AIndex].Size := ADelta;
    end
    else
    begin
      AItem := TdxSpreadSheetTableItem(ItemNeeded(AIndex));
      AItem.Size := AItem.Size + ADelta;
    end;
  end
  else
  begin
    while (AIndex >= 0) and (ADelta < 0) do
    begin
      Inc(ADelta, GetItemSize(AIndex));
      AItem := TdxSpreadSheetTableItem(ItemNeeded(AIndex));
      AItem.Size := Max(0, ADelta);
      Dec(AIndex)
    end;
  end;
end;

function TdxSpreadSheetTableItems.GetFirst: TdxSpreadSheetTableItem;
begin
  Result := TdxSpreadSheetTableItem(FFirst);
end;

function TdxSpreadSheetTableItems.GetItem(AIndex: Integer): TdxSpreadSheetTableItem;
begin
  Result := TdxSpreadSheetTableItem(inherited Items[AIndex]);
end;

function TdxSpreadSheetTableItems.GetLast: TdxSpreadSheetTableItem;
begin
  Result := TdxSpreadSheetTableItem(FLast);
end;

function TdxSpreadSheetTableItems.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := View.SpreadSheet;
end;

procedure TdxSpreadSheetTableItems.SetItem(AIndex: Integer; const AValue: TdxSpreadSheetTableItem);
begin
  inherited Items[AIndex] := AValue;
end;

{ TdxSpreadSheetTableColumn }

function TdxSpreadSheetTableColumn.CreateCell(ARow: Integer): TdxSpreadSheetCell;
begin
  Result := View.Rows.CreateItem(ARow).CreateCell(Index)
end;

function TdxSpreadSheetTableColumn.GetCell(ARow: Integer): TdxSpreadSheetCell;
var
  ARowItem: TdxSpreadSheetTableItem;
begin
  ARowItem := View.Rows[ARow];
  if ARowItem <> nil then
    Result := ARowItem.Cells[Index]
  else
    Result := nil;
end;

function TdxSpreadSheetTableColumn.GetCellCount: Integer;
begin
  Result := View.Dimensions.Bottom + 1;
end;

function TdxSpreadSheetTableColumn.MeasureCellSize(ACanvas: TcxCanvas; ACell: TdxSpreadSheetCell): Integer;
begin
  dxSpreadSheetTextService.MeasureSize(ACanvas, ACell, @Result, nil);
end;

procedure TdxSpreadSheetTableColumn.ShiftIndex(ADelta: Integer);
begin
  if CellCount > 0 then
  begin
    View.Rows.ForEach(
      procedure (AItem: TdxDynamicListItem)
      var
        ACell: TdxSpreadSheetCell;
      begin
        ACell := TdxSpreadSheetTableRow(AItem).Cells[Index];
        if ACell <> nil then
          ACell.ShiftIndex(ADelta);
      end);
  end;
  inherited ShiftIndex(ADelta);
end;

{ TdxSpreadSheetTableColumns }

function TdxSpreadSheetTableColumns.CreateItem(const AIndex: Integer): TdxSpreadSheetTableColumn;
begin
  Result := TdxSpreadSheetTableColumn(inherited CreateItem(AIndex));
end;

function TdxSpreadSheetTableColumns.CreateItem(const AName: string): TdxSpreadSheetTableColumn;
begin
  Result := CreateItem(View.StringToColumnIndex(AName));
end;

function TdxSpreadSheetTableColumns.GetDefaultSize: Integer;
begin
  Result := View.ScaleFactor.Revert(View.Options.DefaultColumnWidth);
end;

function TdxSpreadSheetTableColumns.GetFixedItemIndex: Integer;
begin
  Result := View.FrozenColumn;
end;

function TdxSpreadSheetTableColumns.GetItemClass: TdxDynamicListItemClass;
begin
  Result := TdxSpreadSheetTableColumn;
end;

function TdxSpreadSheetTableColumns.GetItemText(AIndex: Integer): string;
begin
  Result := TdxSpreadSheetColumnHelper.NameByIndex(AIndex, View.SpreadSheet.OptionsView.R1C1Reference);
end;

function TdxSpreadSheetTableColumns.GetItemState(AIndex: Integer): TcxButtonState;
begin
  if View.Selection.IsColumnSelected(AIndex) then
    Result := cxbsHot
  else
    Result := inherited GetItemState(AIndex);
end;

function TdxSpreadSheetTableColumns.GetMaxItemIndex: Integer;
begin
  Result := dxSpreadSheetMaxColumnIndex;
end;

function TdxSpreadSheetTableColumns.GetOppositeItems: TdxSpreadSheetTableItems;
begin
  Result := View.Rows;
end;

procedure TdxSpreadSheetTableColumns.SetDefaultSize(const AValue: Integer);
begin
  View.Options.DefaultColumnWidth := View.ScaleFactor.Apply(AValue);
end;

function TdxSpreadSheetTableColumns.GetItem(AIndex: Integer): TdxSpreadSheetTableColumn;
begin
  Result := TdxSpreadSheetTableColumn(inherited Items[AIndex])
end;

{ TdxSpreadSheetTableRowCells }

constructor TdxSpreadSheetTableRowCells.Create(ARow: TdxSpreadSheetTableRow);
begin
  FRow := ARow;
end;

procedure TdxSpreadSheetTableRowCells.DoItemCreated(AItem: TdxDynamicListItem);
begin
  Row.Owner.GetOppositeItems.CreateItem(AItem.Index);
  Row.DoCellCreated(TdxSpreadSheetCell(AItem));
end;

function TdxSpreadSheetTableRowCells.GetItemClass: TdxDynamicListItemClass;
begin
  Result := TdxSpreadSheetCell;
end;

{ TdxSpreadSheetTableRow }

constructor TdxSpreadSheetTableRow.Create(AOwner: TdxDynamicItemList; AIndex: Integer);
begin
  inherited Create(AOwner, AIndex);
  FCells := CreateRowCells;
end;

destructor TdxSpreadSheetTableRow.Destroy;
begin
  FreeAndNil(FCells);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableRow.ApplyBestFit;
begin
  inherited ApplyBestFit;
  FBestFitIsDirty := False;
end;

function TdxSpreadSheetTableRow.CreateCell(AColumn: Integer): TdxSpreadSheetCell;
begin
  Result := TdxSpreadSheetCell(FCells.CreateItem(AColumn));
end;

procedure TdxSpreadSheetTableRow.EnumCells(AEnumProc: TdxSpreadSheetTableItemEnumProc);
begin
  RowCells.ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      AEnumProc(TdxSpreadSheetCell(AItem));
    end);
end;

function TdxSpreadSheetTableRow.CalculateBestFit: Integer;
begin
  Result := Max(inherited CalculateBestFit, Owner.DefaultSize);
end;

function TdxSpreadSheetTableRow.CreateRowCells: TdxSpreadSheetTableRowCells;
begin
  Result := TdxSpreadSheetTableRowCells.Create(Self);
end;

procedure TdxSpreadSheetTableRow.DoCellCreated(ACell: TdxSpreadSheetCell);
begin
  if not (sssReadingCells in View.State) then
    ACell.Style.Merge(ACell.Column.Style, Style);
  Changed;
end;

function TdxSpreadSheetTableRow.GetCell(AColumn: Integer): TdxSpreadSheetCell;
begin
  Result := TdxSpreadSheetCell(FCells[AColumn]);
end;

function TdxSpreadSheetTableRow.GetCellCount: Integer;
begin
  if FCells.FLast <> nil then
    Result := FCells.FLast.Index + 1
  else
    Result := 0;
end;

procedure TdxSpreadSheetTableRow.MarkBestFitDirty;
begin
  FBestFitIsDirty := True;
  Changed;
end;

function TdxSpreadSheetTableRow.MeasureCellSize(ACanvas: TcxCanvas; ACell: TdxSpreadSheetCell): Integer;
begin
  dxSpreadSheetTextService.MeasureSize(ACanvas, ACell, nil, @Result);
end;

procedure TdxSpreadSheetTableRow.SetSizeEx(const ASize: TSize);
begin
  SetSize(ASize.cy);
end;

{ TdxSpreadSheetTableRows }

function TdxSpreadSheetTableRows.CreateItem(const AIndex: Integer): TdxSpreadSheetTableRow;
begin
  Result := TdxSpreadSheetTableRow(inherited CreateItem(AIndex));
end;

procedure TdxSpreadSheetTableRows.CheckBestFit;
var
  ACellAutoHeight: Boolean;
begin
  if not SpreadSheet.OptionsView.CellAutoHeight or View.History.InProcess then
    Exit;

  ACellAutoHeight := SpreadSheet.OptionsView.CellAutoHeight;
  ForEach(
    procedure(AItem: TdxDynamicListItem)
    var
      ARow: TdxSpreadSheetTableRow;
    begin
      ARow := TdxSpreadSheetTableRow(AItem);
      if ARow.FBestFitIsDirty and ACellAutoHeight then
      begin
        ARow.FBestFitIsDirty := False;
        if not ARow.IsCustomSize then
          ARow.ApplyBestFit;
      end;
    end);
end;

procedure TdxSpreadSheetTableRows.SetBestFitDirty;
begin
  ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      TdxSpreadSheetTableRow(AItem).FBestFitIsDirty := True;
    end);
end;

procedure TdxSpreadSheetTableRows.ForEachCell(AArea: TRect; AProc: TdxDynamicItemListForEachProcRef; AGoForward: Boolean = True);
begin
  ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      TdxSpreadSheetTableRow(AItem).RowCells.ForEach(AProc, AArea.Left, AArea.Right, AGoForward);
    end,
    AArea.Top, AArea.Bottom, AGoForward);
end;

function TdxSpreadSheetTableRows.GetDefaultSize: Integer;
begin
  Result := View.ScaleFactor.Revert(View.Options.DefaultRowHeight);
end;

function TdxSpreadSheetTableRows.GetFixedItemIndex: Integer;
begin
  Result := View.FrozenRow;
end;

function TdxSpreadSheetTableRows.GetItemClass: TdxDynamicListItemClass;
begin
  Result := TdxSpreadSheetTableRow;
end;

function TdxSpreadSheetTableRows.GetItemState(AIndex: Integer): TcxButtonState;
begin
  if View.Selection.IsRowSelected(AIndex) then
    Result := cxbsHot
  else
    Result := inherited GetItemState(AIndex);
end;

function TdxSpreadSheetTableRows.GetMaxItemIndex: Integer;
begin
  Result := dxSpreadSheetMaxRowIndex;
end;

function TdxSpreadSheetTableRows.GetOppositeItems: TdxSpreadSheetTableItems;
begin
  Result := View.Columns;
end;

procedure TdxSpreadSheetTableRows.SetDefaultSize(const AValue: Integer);
begin
  View.Options.DefaultRowHeight := View.ScaleFactor.Apply(AValue);
end;

function TdxSpreadSheetTableRows.GetItem(AIndex: Integer): TdxSpreadSheetTableRow;
begin
  Result := TdxSpreadSheetTableRow(inherited Items[AIndex]);
end;

{ TdxSpreadSheetTableViewOptionsPrint }

constructor TdxSpreadSheetTableViewOptionsPrint.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FHeaderFooter := TdxSpreadSheetTableViewOptionsPrintHeaderFooter.Create(ChangeHandler);
  FPage := TdxSpreadSheetTableViewOptionsPrintPage.Create(ChangeHandler);
  FPagination := TdxSpreadSheetTableViewOptionsPrintPagination.Create(ChangeHandler);
  FPrinting := TdxSpreadSheetTableViewOptionsPrintPrinting.Create(ChangeHandler);
  FSource := TdxSpreadSheetTableViewOptionsPrintSource.Create(ChangeHandler);
end;

destructor TdxSpreadSheetTableViewOptionsPrint.Destroy;
begin
  FreeAndNil(FHeaderFooter);
  FreeAndNil(FPage);
  FreeAndNil(FPagination);
  FreeAndNil(FPrinting);
  FreeAndNil(FSource);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewOptionsPrint.Reset;
begin
  HeaderFooter.Reset;
  Page.Reset;
  Pagination.Reset;
  Printing.Reset;
  Source.Reset;
end;

procedure TdxSpreadSheetTableViewOptionsPrint.DoAssign(ASource: TPersistent);
begin
  inherited DoAssign(ASource);
  if ASource is TdxSpreadSheetTableViewOptionsPrint then
  begin
    HeaderFooter := TdxSpreadSheetTableViewOptionsPrint(ASource).HeaderFooter;
    Page := TdxSpreadSheetTableViewOptionsPrint(ASource).Page;
    Pagination := TdxSpreadSheetTableViewOptionsPrint(ASource).Pagination;
    Printing := TdxSpreadSheetTableViewOptionsPrint(ASource).Printing;
    Source := TdxSpreadSheetTableViewOptionsPrint(ASource).Source;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrint.DoChanged;
begin
  if View <> nil then
    View.AddChanges([sscOptionsPrint, sscModified]);
end;

procedure TdxSpreadSheetTableViewOptionsPrint.ChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TdxSpreadSheetTableViewOptionsPrint.SetHeaderFooter(AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooter);
begin
  FHeaderFooter.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrint.SetPage(AValue: TdxSpreadSheetTableViewOptionsPrintPage);
begin
  FPage.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrint.SetPagination(AValue: TdxSpreadSheetTableViewOptionsPrintPagination);
begin
  FPagination.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrint.SetPrinting(AValue: TdxSpreadSheetTableViewOptionsPrintPrinting);
begin
  FPrinting.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrint.SetSource(AValue: TdxSpreadSheetTableViewOptionsPrintSource);
begin
  FSource.Assign(AValue);
end;

{ TdxSpreadSheetTableViewOptions }

constructor TdxSpreadSheetTableViewOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDefaultColumnWidth := dxSpreadSheetDefaultColumnWidth;
  FDefaultRowHeight := dxSpreadSheetDefaultRowHeight;
  FHeaders := bDefault;
  FGridLines := bDefault;
  FHorizontalScrollBar := bDefault;
  FVerticalScrollBar := bDefault;
  FZoomFactor := dxSpreadSheetDefaultZoomFactor;
  FShowFormulas := bDefault;
  FZeroValues := bDefault;
end;

procedure TdxSpreadSheetTableViewOptions.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxSpreadSheetTableViewOptions then
  begin
    FGridLines := TdxSpreadSheetTableViewOptions(Source).GridLines;
    FHeaders := TdxSpreadSheetTableViewOptions(Source).Headers;
    FHorizontalScrollBar := TdxSpreadSheetTableViewOptions(Source).HorizontalScrollBar;
    FShowFormulas := TdxSpreadSheetTableViewOptions(Source).ShowFormulas;
    FVerticalScrollBar := TdxSpreadSheetTableViewOptions(Source).VerticalScrollBar;
    FZeroValues := TdxSpreadSheetTableViewOptions(Source).ZeroValues;
    FZoomFactor := TdxSpreadSheetTableViewOptions(Source).ZoomFactor;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptions.ChangeScale(M, D: Integer);
begin
  inherited;
  DefaultRowHeight := MulDiv(DefaultRowHeight, M, D);
  DefaultColumnWidth := MulDiv(DefaultColumnWidth, M, D);
end;

function TdxSpreadSheetTableViewOptions.GetActualGridLines: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(GridLines, OptionsView.GridLines);
end;

function TdxSpreadSheetTableViewOptions.GetActualHeaders: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(Headers, OptionsView.Headers);
end;

function TdxSpreadSheetTableViewOptions.GetActualHorizontalScrollBar: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(HorizontalScrollBar, OptionsView.HorizontalScrollBar);
end;

function TdxSpreadSheetTableViewOptions.GetActualShowFormulas: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(ShowFormulas, OptionsView.ShowFormulas);
end;

function TdxSpreadSheetTableViewOptions.GetActualVerticalScrollBar: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(VerticalScrollBar, OptionsView.VerticalScrollBar);
end;

function TdxSpreadSheetTableViewOptions.GetActualZeroValues: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(ZeroValues, OptionsView.ZeroValues);
end;

function TdxSpreadSheetTableViewOptions.GetOptionsView: TdxSpreadSheetOptionsView;
begin
  Result := View.SpreadSheet.OptionsView;
end;

function TdxSpreadSheetTableViewOptions.GetProtected: Boolean;
begin
  Result := View.OptionsProtection.&Protected;
end;

function TdxSpreadSheetTableViewOptions.GetView: TdxSpreadSheetTableView;
begin
  Result := TdxSpreadSheetTableView(inherited View);
end;

procedure TdxSpreadSheetTableViewOptions.SetDefaultColumnWidth(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue <> DefaultColumnWidth then
  begin
    FDefaultColumnWidth := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptions.SetDefaultRowHeight(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue <> DefaultRowHeight then
  begin
    FDefaultRowHeight := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptions.SetGridLines(AValue: TdxDefaultBoolean);
begin
  if AValue <> FGridLines then
  begin
    FGridLines := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptions.SetHeaders(AValue: TdxDefaultBoolean);
begin
  if FHeaders <> AValue then
  begin
    FHeaders := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptions.SetShowFormulas(AValue: TdxDefaultBoolean);
begin
  if AValue <> FShowFormulas then
  begin
    FShowFormulas := AValue;
    View.AddChanges([sscData, sscModified]);
  end;
end;

procedure TdxSpreadSheetTableViewOptions.SetHorizontalScrollBar(AValue: TdxDefaultBoolean);
begin
  if FHorizontalScrollBar <> AValue then
  begin
    FHorizontalScrollBar := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptions.SetProtected(AValue: Boolean);
begin
  View.OptionsProtection.&Protected := AValue;
end;

procedure TdxSpreadSheetTableViewOptions.SetVerticalScrollBar(AValue: TdxDefaultBoolean);
begin
  if FVerticalScrollBar <> AValue then
  begin
    FVerticalScrollBar := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptions.SetZeroValues(AValue: TdxDefaultBoolean);
begin
  if FZeroValues <> AValue then
  begin
    FZeroValues := AValue;
    View.AddChanges([sscData, sscModified]);
  end;
end;

procedure TdxSpreadSheetTableViewOptions.SetZoomFactor(AValue: Integer);
begin
  AValue := Max(dxSpreadSheetMinimumZoomFactor, Min(dxSpreadSheetMaximumZoomFactor, AValue));
  if AValue <> FZoomFactor then
  begin
    FZoomFactor := AValue;
    View.AddChanges([sscZoom, sscLayout, sscModified]);
  end;
end;

{ TdxSpreadSheetTableViewHitTest }

procedure TdxSpreadSheetTableViewHitTest.Calculate(const AHitPoint: TPoint);
begin
  inherited Calculate(AHitPoint);
  View.ViewInfo.InitHitTest(Self);
end;

function TdxSpreadSheetTableViewHitTest.GetActualHitPoint: TPoint;
begin
  Result := cxPointScale(HitPoint, 100, View.ZoomFactor);
end;

function TdxSpreadSheetTableViewHitTest.GetContainer: TdxSpreadSheetContainer;
begin
  Result := TdxSpreadSheetContainerViewInfo(HitObject).Owner;
end;

function TdxSpreadSheetTableViewHitTest.GetView: TdxSpreadSheetTableView;
begin
  Result := TdxSpreadSheetTableView(Owner);
end;

{ TdxSpreadSheetTableViewSelection }

constructor TdxSpreadSheetTableViewSelection.Create(AOwner: TdxSpreadSheetTableView);
begin
  inherited Create;
  FView := AOwner;
  FItems := TObjectList<TcxRect>.Create;
  FItems.Add(TcxRect.Create(nil));
end;

destructor TdxSpreadSheetTableViewSelection.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewSelection.Add(AArea: TRect;
  AShift: TShiftState = []; AFocusedRow: Integer = -1; AFocusedColumn: Integer = -1);
var
  ARect: TcxRect;
  APrevArea: TRect;
  APrevRow, APrevColumn: Integer;
begin
  APrevArea := Area;
  APrevRow := FFocusedRow;
  APrevColumn := FFocusedColumn;
  if (FItems.Count > 1) and not dxSpreadSheetContains(FItems.Last.Rect, APrevRow, APrevColumn) then
  begin
    APrevArea := FItems.Last.Rect;
    APrevRow := APrevArea.Top;
    APrevColumn := APrevArea.Left;
  end;

  if not dxSpreadSheetIsEntireRowOrColumn(AArea) then
    AArea := MergedCells.ExpandArea(AArea);

  if CanSelectArea(AArea) then
  begin
    BeginUpdate;
    try
      CheckClear(AShift);

      if (AFocusedColumn < AArea.Left) or (AFocusedColumn > AArea.Right) then
        AFocusedColumn := AArea.Left;
      if (AFocusedRow < AArea.Top) or (AFocusedRow > AArea.Bottom) then
        AFocusedRow := AArea.Top;

      if (FFocusedColumn <> AFocusedColumn) or (FFocusedRow <> AFocusedRow) then
      begin
        if View.Controller.DoActiveCellChanging(AFocusedColumn, AFocusedRow) then
        begin
          FFocusedColumn := AFocusedColumn;
          FFocusedRow := AFocusedRow;
        end
        else
        begin
          Add(APrevArea, [], APrevRow, APrevColumn);
          Exit;
        end;
      end;

      ARect := TcxRect.Create(nil);
      ARect.OnChange := ItemChanged;
      ARect.Rect := AArea;
      FItems.Add(ARect);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetTableViewSelection.Clear;
begin
  FItems.Clear;
  FFocusedColumn := -1;
  FFocusedRow := -1;
  Changed;
end;

procedure TdxSpreadSheetTableViewSelection.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxSpreadSheetTableViewSelection.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Changed;
end;

function TdxSpreadSheetTableViewSelection.HasArea(const AArea: TRect): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FItems.Count - 1 do
    if cxRectIsEqual(AArea, FItems[I].Rect) then
    begin
      Result := True;
      Break;
    end;
end;

function TdxSpreadSheetTableViewSelection.IsCellSelected(ARow, AColumn: Integer): Boolean;
var
  I: Integer;
begin
  Result := (ARow = FocusedRow) and (AColumn = FocusedColumn);
  I := 0;
  while not Result and (I < Count) do
  begin
    Result := dxSpreadSheetContains(Items[I].Rect, ARow, AColumn);
    Inc(I);
  end;
end;

function TdxSpreadSheetTableViewSelection.IsColumnSelected(AColumn: Integer): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := AColumn = FocusedColumn;
  while not Result and (I < Count) do
  begin
    Result := InRange(AColumn, Items[I].Left, Items[I].Right);
    Inc(I);
  end;
end;

function TdxSpreadSheetTableViewSelection.IsRowSelected(ARow: Integer): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := ARow = FocusedRow;
  while not Result and (I < Count) do
  begin
    Result := InRange(ARow, Items[I].Top, Items[I].Bottom);
    Inc(I);
  end;
end;

function TdxSpreadSheetTableViewSelection.IsEntireColumnSelected(AColumn: Integer): Boolean;
var
  AItem: TcxRect;
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if dxSpreadSheetIsEntireColumn(AItem.Rect) then
    begin
      if InRange(AColumn, AItem.Left, AItem.Right) then
        Exit(True);
    end;
  end;
end;

function TdxSpreadSheetTableViewSelection.IsEntireRowSelected(ARow: Integer): Boolean;
var
  AItem: TcxRect;
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if dxSpreadSheetIsEntireRow(AItem.Rect) then
    begin
      if InRange(ARow, AItem.Top, AItem.Bottom) then
        Exit(True);
    end;
  end;
end;

procedure TdxSpreadSheetTableViewSelection.SelectAll;
begin
  Add(dxSpreadSheetEntireSheetArea, [], Max(0, FocusedRow), Max(0, FocusedColumn));
end;

procedure TdxSpreadSheetTableViewSelection.SelectCell(ARow, AColumn: Integer; AShift: TShiftState = []);
begin
  Add(Rect(AColumn, ARow, AColumn, ARow), AShift);
end;

procedure TdxSpreadSheetTableViewSelection.SelectColumns(AStartColumn, AFinishColumn: Integer; AShift: TShiftState = []);
var
  ARow: Integer;
begin
  ARow := FocusedRow;
  if not (ssShift in AShift) or not cxInRange(ARow, View.TopRow, View.BottomRow) or not SelectionByColumns then
    ARow := View.TopRow;
  Add(Rect(Min(AStartColumn, AFinishColumn), 0,
    Max(AStartColumn, AFinishColumn), dxSpreadSheetMaxRowIndex), AShift, ARow);
end;

procedure TdxSpreadSheetTableViewSelection.SelectRows(AStartRow, AFinishRow: Integer; AShift: TShiftState = []);
var
  AColumn: Integer;
begin
  AColumn := FocusedColumn;
  if not (ssShift in AShift) and not cxInRange(AColumn, View.LeftColumn, View.RightColumn) or not SelectionByRows then
    AColumn := View.LeftColumn;
  Add(Rect(0, Min(AStartRow, AFinishRow), dxSpreadSheetMaxColumnIndex, Max(AStartRow, AFinishRow)), AShift, -1, AColumn);
end;

procedure TdxSpreadSheetTableViewSelection.SetFocused(ARow, AColumn: Integer; AShift: TShiftState);

  function CheckMoveFocus: Boolean;
  begin
    Result := False;
    if IsCellSelected(ARow, AColumn) and View.Controller.DoActiveCellChanging(AColumn, ARow) then
    begin
      CheckClear(AShift);
      Result := Count > 0;
      if Result then
      begin
        FFocusedRow := ARow;
        FFocusedColumn := AColumn;
        Changed;
      end;
    end;
  end;

begin
  dxSpreadSheetValidate(ARow, 0, dxSpreadSheetMaxRowIndex);
  dxSpreadSheetValidate(AColumn, 0, dxSpreadSheetMaxColumnIndex);
  if (ARow <> FFocusedRow) or (AColumn <> FFocusedColumn) then
  begin
    BeginUpdate;
    try
      if not CheckMoveFocus then
        Add(MergedCells.ExpandArea(AColumn, ARow), AShift, ARow, AColumn);
      if (FocusedRow = ARow) and (FocusedColumn = AColumn) then
        View.MakeVisible(FocusedRow, FocusedColumn);
    finally
      EndUpdate;
    end;
  end
  else
  begin
    CheckClear(AShift);
    if Count = 0 then
      SelectCell(ARow, AColumn);
  end;
end;

function TdxSpreadSheetTableViewSelection.CanSelectArea(const AArea: TRect): Boolean;
begin
  Result := View.OptionsProtection.ActualAllowSelectLockedCells and View.OptionsProtection.ActualAllowSelectUnlockedCells;
  if not Result then
    case View.GetLockedStateOfCellsInArea(AArea) of
      cbChecked:
        Result := View.OptionsProtection.ActualAllowSelectLockedCells;
      cbUnchecked:
        Result := View.OptionsProtection.ActualAllowSelectUnlockedCells;
    end;
end;

procedure TdxSpreadSheetTableViewSelection.Changed;
begin
  if FLockCount = 0 then
    View.SelectionChanged;
end;

procedure TdxSpreadSheetTableViewSelection.CheckClear(const AShift: TShiftState);
begin
  if [ssCtrl, ssShift] * AShift = [] then
    Clear;
end;

function TdxSpreadSheetTableViewSelection.GetActiveSelection: TRect;
var
  I: Integer;
begin
  Result := Rect(FocusedColumn, FocusedRow, FocusedColumn, FocusedRow);
  for I := 0 to Count - 1 do
    if dxSpreadSheetContains(Items[I].Rect, FocusedRow, FocusedColumn) then
    begin
      Result := Items[I].Rect;
      Break;
    end;
end;

procedure TdxSpreadSheetTableViewSelection.GetState(ARow, AColumn: Integer; var AFocused, ASelected: Boolean);
var
  I: Integer;
begin
  ASelected := False;
  AFocused := dxSpreadSheetContains(MergedCells.ExpandArea(FocusedColumn, FocusedRow), ARow, AColumn);
  for I := 0 to Count - 1 do
  begin
    ASelected := dxSpreadSheetContains(Items[I].Rect, ARow, AColumn);
    if ASelected then Break;
  end
end;

function TdxSpreadSheetTableViewSelection.IsAreaMoreThanOneCell: Boolean;
var
  AArea: TRect;
begin
  AArea := Area;
  Result := not (dxSpreadSheetIsSingleCellArea(AArea) or IsMergedCells(AArea));
end;

function TdxSpreadSheetTableViewSelection.IsMergedCells(const AArea: TRect): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to View.MergedCells.Count - 1 do
  begin
    Result := cxRectIsEqual(AArea, View.MergedCells[I].Area);
    if Result then
      Break;
  end;
end;

procedure TdxSpreadSheetTableViewSelection.ItemChanged(Sender: TObject);
begin
  BeginUpdate;
  try
    TcxRect(Sender).Rect := cxRectAdjust(TcxRect(Sender).Rect);
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableViewSelection.LoadFromStream(AReader: TcxReader);
var
  ARect: TcxRect;
  I, ACount, AIndex: Integer;
begin
  BeginUpdate;
  try
    FItems.Clear;
    FFocusedRow := AReader.ReadInteger;
    FFocusedColumn := AReader.ReadInteger;

    AIndex := AReader.ReadInteger;
    if AIndex > 0 then
      FocusedContainer := View.Containers[AIndex]
    else
      FocusedContainer := nil;

    ACount := AReader.ReadInteger;
    for I := 0 to ACount - 1 do
    begin
      ARect := TcxRect.Create(nil);
      ARect.Rect := AReader.ReadRect;
      ARect.OnChange := ItemChanged;
      FItems.Add(ARect);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableViewSelection.SaveToStream(AWriter: TcxWriter);
var
  I: Integer;
begin
  AWriter.WriteInteger(FFocusedRow);
  AWriter.WriteInteger(FFocusedColumn);

  if FocusedContainer <> nil then
    AWriter.WriteInteger(FocusedContainer.Index)
  else
    AWriter.WriteInteger(-1);

  AWriter.WriteInteger(Count);
  for I := 0 to Count - 1 do
    AWriter.WriteRect(Items[I].Rect);
end;

procedure TdxSpreadSheetTableViewSelection.Validate;
var
  I: Integer;
begin
  BeginUpdate;
  try
    if not (View.OptionsProtection.ActualAllowEditContainers) then
      FocusedContainer := nil;

    for I := Count - 1 downto 0 do
      if not CanSelectArea(Items[I].Rect) then
      begin
        FItems.Delete(I);
        Changed;
      end;

    if not CanSelectArea(cxRectBounds(FocusedColumn, FocusedRow, 0, 0)) then
    begin
      FFocusedColumn := -1;
      FFocusedRow := -1;
      Changed;
    end;
  finally
    EndUpdate;
  end;
end;

function TdxSpreadSheetTableViewSelection.GetArea: TRect;
var
  I: Integer;
begin
  Result := Rect(FocusedColumn, FocusedRow, FocusedColumn, FocusedRow);
  I := 0;
  if Count = 1 then
    Result := Items[0].Rect
  else
    while I < Count do
    begin
      Result := dxSpreadSheetCellsUnion(Result, Items[I].Rect);
      Inc(I);
    end;
end;

function TdxSpreadSheetTableViewSelection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxSpreadSheetTableViewSelection.GetFocusedCell: TdxSpreadSheetCell;
begin
  Result := View.Cells[FocusedRow, FocusedColumn];
end;

function TdxSpreadSheetTableViewSelection.GetFocusedContainer: TdxSpreadSheetContainer;
begin
  Result := View.Controller.FocusedContainer;
end;

function TdxSpreadSheetTableViewSelection.GetItem(AIndex: Integer): TcxRect;
begin
  Result := FItems[AIndex];
end;

function TdxSpreadSheetTableViewSelection.GetMergedCells: TdxSpreadSheetMergedCellList;
begin
  Result := View.MergedCells;
end;

function TdxSpreadSheetTableViewSelection.GetSelectionByColumns: Boolean;
begin
  Result := (Count > 0) and (Items[Count - 1].Bottom >= dxSpreadSheetMaxRowIndex);
end;

function TdxSpreadSheetTableViewSelection.GetSelectionByRows: Boolean;
begin
  Result := (Count > 0) and (Items[Count - 1].Right >= dxSpreadSheetMaxColumnIndex);
end;

procedure TdxSpreadSheetTableViewSelection.SetFocusedCell(AValue: TdxSpreadSheetCell);
begin
  if AValue <> nil then
    SetFocused(AValue.RowIndex, AValue.ColumnIndex, []);
end;

procedure TdxSpreadSheetTableViewSelection.SetFocusedColumn(AValue: Integer);
begin
  SetFocused(FocusedRow, AValue, []);
end;

procedure TdxSpreadSheetTableViewSelection.SetFocusedContainer(const Value: TdxSpreadSheetContainer);
begin
  View.Controller.FocusedContainer := Value;
end;

procedure TdxSpreadSheetTableViewSelection.SetFocusedRow(AValue: Integer);
begin
  SetFocused(AValue, FocusedColumn, []);
end;

{ TdxSpreadSheetTableViewEditingController }

constructor TdxSpreadSheetTableViewEditingController.Create(AOwner: TdxSpreadSheetTableViewController);
begin
  inherited Create(AOwner.View.SpreadSheet);
  FOwner := AOwner;
  FEditStyleFont := TFont.Create;
  FReferenceHighlighter := CreateReferenceHighlighter;
  FDefaultEditProperties := CreateDefaultEditProperties;
  FDefaultEditProperties.HideSelection := False;
  FDefaultEditProperties.WantReturns := True;
  FEditStyle := CreateEditStyle;
  FCanUseCurrentValue := True;
end;

destructor TdxSpreadSheetTableViewEditingController.Destroy;
begin
  FreeAndNil(FReferenceHighlighter);
  FreeAndNil(FDefaultEditProperties);
  FreeAndNil(FEditData);
  FreeAndNil(FEditStyleFont);
  FreeAndNil(FEditStyle);
  inherited Destroy;
end;

class function TdxSpreadSheetTableViewEditingController.CanFinishEditingOnExit(AEdit: TcxCustomEdit): Boolean;
var
  AFocusedControl: TWinControl;
  AIntf1: IdxSpreadSheet;
  AIntf2: IdxSpreadSheet;
begin
  Result := True;
  if AEdit <> nil then
  begin
    AFocusedControl := GetInnerControlContainer(FindControl(GetFocus));
    if AFocusedControl <> nil then
    begin
      if Supports(AEdit.Owner, IdxSpreadSheet, AIntf2) and Supports(AFocusedControl.Owner, IdxSpreadSheet, AIntf1) then
        Result := AIntf1.GetControl <> AIntf2.GetControl;
    end;
  end;
end;

class function TdxSpreadSheetTableViewEditingController.GetActualInplaceEdit(
  ASheet: TdxSpreadSheetTableView; out AEdit: TdxSpreadSheetCustomInplaceEdit): Boolean;
var
  AFocusedControl: TWinControl;
  AIntf: IdxSpreadSheet;
begin
  Result := False;
  if ASheet.EditingController.IsEditing then
  begin
    AFocusedControl := GetInnerControlContainer(FindControl(GetFocus));
    if AFocusedControl is TdxSpreadSheetCustomInplaceEdit then
    begin
      Result := Supports(AFocusedControl.Owner, IdxSpreadSheet, AIntf) and (AIntf.GetControl = ASheet.SpreadSheet);
      if Result then
        AEdit := TdxSpreadSheetCustomInplaceEdit(AFocusedControl);
    end
    else
      if ASheet.EditingController.Edit is TdxSpreadSheetCustomInplaceEdit then
      begin
        AEdit := TdxSpreadSheetCustomInplaceEdit(ASheet.EditingController.Edit);
        Result := True;
      end;
  end;
end;

class function TdxSpreadSheetTableViewEditingController.GetEditingText(AEdit: TcxCustomEdit): string;
begin
  if AEdit is TcxCustomRichEdit then
    Result := TcxCustomRichEdit(AEdit).Text
  else
    Result := VarToStr(AEdit.EditValue);
end;

procedure TdxSpreadSheetTableViewEditingController.HideEdit(Accept: Boolean);
begin
  try
    inherited HideEdit(Accept);
  except
    on EdxSpreadSheetCannotChangePartOfArrayError do
    begin
      FEdit.RepositoryItem := nil;
      FEdit := nil;
      raise;
    end;
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.ShowEdit;
begin
  ShowEdit('');
end;

procedure TdxSpreadSheetTableViewEditingController.ShowEdit(const AValue: string);
begin
  FEditValue := AValue;
  ReplacementMode := False;
  if PrepareEdit(True) then
  begin
    FEdit.Activate(FEditData, SpreadSheet.Focused);
    EditActivated;
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.ShowEditByKey(AKey: Char);
begin
  FCanUseCurrentValue := False;
  try
    FEditValue := '';
    ReplacementMode := True;
    if PrepareEdit(False) then
    begin
      FEdit.ActivateByKey(AKey, FEditData);
      EditActivated;
    end;
  finally
    FCanUseCurrentValue := True;
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.ShowEditByMouse(X, Y: Integer; AShift: TShiftState);
begin
  FEditValue := '';
  View.HitTest.Calculate(cxPoint(X, Y));
  if not View.HitTest.HitAtCell then
    Exit;

  ReplacementMode := (Cell = nil) or Cell.IsEmpty;
  if PrepareEdit(True) then
  begin
    FEdit.ActivateByMouse(AShift, X, Y, FEditData);
    EditActivated;
    if FEdit = DefaultEdit then
      DefaultEdit.CaretPos := GetCaretPosByMousePos;
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.ShowEditWithTopCellValue(
  const ACurrentRow, ACurrentColumn: Integer; const AUseFormula: Boolean);
var
  ACell: TdxSpreadSheetCell;
  AValue: string;
begin
  ACell := View.GetTopCell(ACurrentRow, ACurrentColumn);
  AValue := '';
  if ACell <> nil then
    if not AUseFormula then
      AValue := ACell.DisplayText
    else
      if ACell.AsFormula <> nil then
        AValue := ACell.AsFormula.AsText
      else
        AValue := VarToStr(ACell.AsVariant);

  FCanUseCurrentValue := False;
  try
    ShowEdit(AValue);
  finally
    FCanUseCurrentValue := True;
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.AssignEditStyle;
var
  AStyle: TcxCustomEditStyleAccess;
begin
  AStyle := TcxCustomEditStyleAccess(EditStyle);
  AStyle.FAssignedValues := AStyle.FAssignedValues + [svFont, svColor, svButtonTransparency];

  CellViewInfo.Style.Font.AssignToFont(EditStyleFont);
  if Edit.ActiveProperties is TcxCustomRichEditProperties then
    TcxCustomRichEditProperties(Edit.ActiveProperties).ZoomFactor := View.ZoomFactor / 100
  else
    EditStyleFont.Height := MulDiv(dxSystemScaleFactor.Revert(EditStyleFont.Height), View.ZoomFactor, 100);

  AStyle.StyleData.Font := EditStyleFont;
  AStyle.StyleData.Color := CellViewInfo.BackgroundColor;
  AStyle.StyleData.FontColor := CellViewInfo.TextColor;
  AStyle.ButtonTransparency := ebtHideInactive;
  AStyle.Changed;
  AStyle.BorderStyle := ebsNone;
  AStyle.TransparentBorder := False;

  Edit.Style.Assign(EditStyle);
end;

function TdxSpreadSheetTableViewEditingController.CanInitEditing: Boolean;
begin
  Result := False;
  if View.Active then
  begin
    if SpreadSheet.DragAndDropState = ddsNone then
      View.MakeFocusedCellVisible;
    if CellViewInfo <> nil then
    begin
      View.CheckProtection(cmmReplace, cxRectBounds(CellViewInfo.Column, CellViewInfo.Row, 0, 0));
      FPropertiesValue := DefaultEditProperties;
      if FPropertiesValue is TcxRichEditProperties then
      begin
        TcxRichEditProperties(FPropertiesValue).PlainText := IsEditValueFormula;
        TcxRichEditProperties(FPropertiesValue).RichEditClass := recRichEdit20;
      end;
      Result := True;
      View.DoEditing(FPropertiesValue, Result);
    end
  end;
end;

function TdxSpreadSheetTableViewEditingController.CanInsertNewAreaViaMouse: Boolean;
begin
  Result := IsEditing and IsEditValueFormula and (Edit is TcxCustomTextEdit) and
    dxIsOperandSeparator(EditText[TcxCustomTextEdit(Edit).SelStart]);
end;

function TdxSpreadSheetTableViewEditingController.CanRedo: Boolean;
begin
  Result := IsEditing and not VarIsNull(RedoValue) and VarEquals(Edit.EditValue, EditValue);
end;

function TdxSpreadSheetTableViewEditingController.CanUndo: Boolean;
begin
  Result := IsEditing and Edit.EditModified;
end;

function TdxSpreadSheetTableViewEditingController.CanUpdateEditValue: Boolean;
begin
  Result := inherited CanUpdateEditValue and (CellViewInfo <> nil);
end;

function TdxSpreadSheetTableViewEditingController.CanUpdateMultilineEditHeight: Boolean;
begin
  Result := CanUpdateEditValue and not Edit.IsHiding and Edit.Focused;
end;

function TdxSpreadSheetTableViewEditingController.CreateDefaultEdit: TcxCustomMemo;
begin
  Result := TdxSpreadSheetInplaceEdit.Create(SpreadSheet, True);
  TdxSpreadSheetInplaceEdit(Result).ScaleFactor.Assign(View.ScaleFactor);
end;

function TdxSpreadSheetTableViewEditingController.CreateDefaultEditProperties: TcxCustomMemoProperties;
begin
  Result := TcxRichEditProperties.Create(View);
  TcxRichEditProperties(Result).AutoSelect := False;
  TcxRichEditProperties(Result).WantReturns := False;
  TcxRichEditProperties(Result).WantTabs := False;
end;

function TdxSpreadSheetTableViewEditingController.CreateEditStyle: TcxEditStyle;
begin
  Result := TcxEditStyle.Create(nil, True);
end;

function TdxSpreadSheetTableViewEditingController.CreateReferenceHighlighter: TdxSpreadSheetCustomReferenceHighlighter;
begin
  Result := TdxSpreadSheetTableViewEditingCellReferenceHighlighter.Create(View);
end;

procedure TdxSpreadSheetTableViewEditingController.ClearEditingItem;
begin
end;

function TdxSpreadSheetTableViewEditingController.IsEditValueFormula: Boolean;
begin
  Result := dxSpreadSheetIsFormula(EditText);
end;

procedure TdxSpreadSheetTableViewEditingController.DoHideEdit(Accept: Boolean);
var
  ACell: TdxSpreadSheetCell;
  AEditValue, APrevValue: Variant;
begin
  if Edit = nil then
    Exit;

  ACell := Cell;
  if Accept then
  begin
    Edit.Deactivate;
    AEditValue := Edit.EditValue;
  end;

  if Edit <> nil then
  begin
    UninitEdit;
    Edit.EditModified := False;
    if SpreadSheet.CanFocusEx and (Edit <> nil) and Edit.IsFocused then
      SpreadSheet.SetFocus;
  end;

  try
    if Accept then
    begin
      APrevValue := GetValueFromCell(ACell);
      SetValue(AEditValue);
      if not VarEquals(APrevValue, GetValueFromCell(ACell)) then
        View.DoEditValueChanged;
    end;
  finally
    HideInplaceEditor;
    ReferenceHighlighter.Clear;
    FEditing := False;
    FEditValue := '';
    View.Pack;
    View.Invalidate;
    if not SpreadSheet.IsDestroying then
    begin
      View.DoEdited;
      HistoryChanged;
    end;
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.DoUpdateEdit;
begin
  if not IsEditing or (Edit = nil) then
    Exit;

  if EditPreparing then
  begin
    AssignEditStyle;
    View.DoInitEdit(Edit);
    UpdateEditPosition;
    Edit.Visible := True;
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.EditActivated;
begin
  SpreadSheet.ClearClipboard;
  if Edit = DefaultEdit then
  begin
    DefaultEdit.SelectAll;
    DefaultEdit.SelStart := MaxInt;
  end;
  if IsEditValueFormula then
    UpdateReferencesHighlighting;
  View.SpreadSheet.Listeners.NotifyEditing(View);
  RedoValue := Null;
  HistoryChanged;
end;

procedure TdxSpreadSheetTableViewEditingController.EditChanged(Sender: TObject);
var
  AIsFormula: Boolean;
begin
  MultilineEditTextChanged;
  inherited EditChanged(Sender);

  AIsFormula := IsEditValueFormula;
  if AIsFormula or (FPrevIsEditValueFormula <> AIsFormula) then
  begin
    FPrevIsEditValueFormula := AIsFormula;
    UpdateReferencesHighlighting;
    View.Invalidate;
  end;

  View.DoEditChanged;
  HistoryChanged;
end;

procedure TdxSpreadSheetTableViewEditingController.EditExit(Sender: TObject);

  function IsOurAction(AControl: TControl): Boolean;
  begin
    Result := (AControl <> nil) and
      ((AControl.Action is TdxSpreadSheetChangeFontSize) or
      (AControl.Action is TdxSpreadSheetChangeFontName))
  end;

begin
  if IsOurAction(FindControl(GetFocus)) or IsOurAction(Screen.ActiveControl) or not CanFinishEditingOnExit(Edit) then
    Exit;
  if not (EditPreparing or FIsEditHiding) then
  begin
    FIsEditHiding := True;
    PostMessage(SpreadSheet.Handle, DXM_POSTHIDEEDIT, TdxNativeUInt(Self), 0);
  end
  else
  begin
    FIsEditHiding := False;
    inherited EditExit(Sender);
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.EditFocusChanged(Sender: TObject);
begin
  inherited EditFocusChanged(Sender);
  SpreadSheet.FocusChanged;
end;

procedure TdxSpreadSheetTableViewEditingController.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited EditKeyDown(Sender, Key, Shift);
  SpreadSheet.DoKeyDown(Key, Shift);

  case Key of
    VK_ESCAPE:
      HideEdit(False);
    VK_TAB:
      if [ssCtrl] * Shift = [] then
        HideEdit(True);
    VK_RETURN:
      if Edit <> DefaultEdit then
        HideEdit(True);
    Word('Z'):
      if ssCtrl in Shift then
      begin
        Undo;
        Key := 0;
      end;
    Word('Y'):
      if ssCtrl in Shift then
      begin
        Redo;
        Key := 0;
      end;
  end;

  if ReplacementMode then
    case Key of
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_NEXT, VK_PRIOR, VK_HOME, VK_END:
        HideEdit(True);
    end;

  if Key <> 0 then
    Controller.KeyDown(Key, Shift);
  if Edit = nil then
    Key := 0;
  HistoryChanged;
end;

procedure TdxSpreadSheetTableViewEditingController.EditKeyPress(Sender: TObject; var Key: Char);
begin
  SpreadSheet.DoKeyPress(Key);
  if Key = #27 then
    Key := #0;
end;

procedure TdxSpreadSheetTableViewEditingController.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  SpreadSheet.DoKeyUp(Key, Shift);
end;

function TdxSpreadSheetTableViewEditingController.GetAdjustedMultilineEditBounds: TRect;
var
  AText: string;
  ASizeProperties: TcxEditSizeProperties;
  ATextBounds, AMergedArea: TRect;
const
  AHorzAlignToTextAlign: array[TcxTextAlignX] of TAlignment = (taLeftJustify, taCenter, taRightJustify, taLeftJustify, taLeftJustify);
begin
  FAlignment := taLeftJustify;
  AMergedArea := View.MergedCells.ExpandArea(View.Selection.FocusedColumn, View.Selection.FocusedRow);
  AText := DefaultEdit.Text;
  if (AText <> '') and dxCharInSet(AText[Length(AText)], [#13, #10]) then
    AText := AText + ' ';
  ATextBounds := cxRectSetSize(cxNullRect, GetEditSize(AText, cxDefaultEditSizeProperties));
  if CellViewInfo <> nil then
  begin
    FAlignment := AHorzAlignToTextAlign[CellViewInfo.AlignHorz];
    if (Cell <> nil) and Cell.IsFormula then
      FAlignment := taLeftJustify;
  end;
  Result := GetAdjustedEditBoundsHorizontally(AMergedArea, ATextBounds);
  Result := cxRectSetHeight(Result, View.Rows.GetDistance(AMergedArea.Top, AMergedArea.Bottom));
  ATextBounds := Result;
  if AText <> '' then
  begin
    ASizeProperties := cxDefaultEditSizeProperties;
    ASizeProperties.Width := cxRectWidth(Result);
    ATextBounds.Bottom :=  ATextBounds.Top + Max(cxRectHeight(Result), GetEditSize(AText, ASizeProperties).cy);
  end;
  Result := GetAdjustedEditBoundsVertically(AMergedArea, ATextBounds);
  Result := cxRectInflate(Result, -1, 0, -cxTextOffset, -cxTextOffset);
  if cxRectIntersect(Result, GetEditClipArea, Result) then
    Result := cxRectScale(Result, View.ZoomFactor, 100)
  else
    Result := cxRectSetOrigin(Result, cxInvisiblePoint);
end;

function TdxSpreadSheetTableViewEditingController.GetAdjustedEditBoundsHorizontally(const AArea, ATextRect: TRect): TRect;
var
  AClip, R: TRect;
  AWidth: Integer;
begin
  R := AArea;
  AWidth := 0;
  Result := ATextRect;
  if (CellViewInfo <> nil) and not CellViewInfo.Multiline and not CellViewInfo.IsMerged then
  begin
    AWidth := View.Columns.GetDistance(R.Left, R.Right) - cxTextOffset * 2;
    if cxRectWidth(Result) > AWidth then
    begin
      if FAlignment = taRightJustify then
        Result := cxRectSetRight(Result, 0, cxRectWidth(ATextRect) - AWidth)
      else
        if FAlignment = taCenter then
        begin
          Result.Right := ((cxRectWidth(ATextRect) - AWidth) div 2) + 1;
          Result.Left := -Result.Right;
        end
        else
          Result := cxRectSetLeft(Result, 0, cxRectWidth(ATextRect) - AWidth);
    end
    else
      AWidth := 0;
  end;
  AClip := GetEditClipArea;
  if (Cell = nil) or not (csWordWrap in Cell.StyleHandle.States) and (AWidth > 0) then
  begin
    while (R.Right < dxSpreadSheetMaxColumnCount - 1) and (Result.Right > 0) do
    begin
      Inc(R.Right);
      Dec(Result.Right, View.Columns.GetItemSize(R.Right));
    end;
    while (R.Left > 0) and (Result.Left < 0) do
    begin
      Dec(R.Left);
      Inc(Result.Left, View.Columns.GetItemSize(R.Left));
    end;
  end;
  Result := GetAreaBounds(R);
  if (FAlignment = taCenter) and not cxRectIsEqual(AArea, R) then
  begin
    if Result.Left < AClip.Left then
    begin
      Result.Left := AClip.Left;
      FAlignment := taLeftJustify
    end
    else
      if Result.Right > AClip.Right then
      begin
        Result.Right := AClip.Right;
        FAlignment := taRightJustify;
      end;
    R := GetAreaBounds(AArea);
    AWidth := Min(R.Left - Result.Left, Result.Right - R.Right);
    if AWidth > 0 then
      Result := cxRectInflate(R, AWidth, 0);
  end;
  if not cxRectIntersect(Result, AClip, Result) then
    Result := cxRectSetOrigin(Result, cxInvisiblePoint);
end;

function TdxSpreadSheetTableViewEditingController.GetAdjustedEditBoundsVertically(const AArea, ATextRect: TRect): TRect;
var
  ARow: Integer;
  ABounds: TRect;
begin
  Result := ATextRect;
  ARow := AArea.Bottom;
  ABounds := GetAreaBounds(AArea);
  while (ARow < dxSpreadSheetMaxRowCount - 1) and (ATextRect.Bottom > ABounds.Bottom) do
  begin
    Inc(ARow);
    Inc(ABounds.Bottom, View.Rows.GetItemSize(ARow));
  end;
  Result.Bottom := ABounds.Bottom;
  if not cxRectIntersect(Result, GetEditClipArea, Result) then
    Result := cxRectSetOrigin(Result, cxInvisiblePoint);
end;

function TdxSpreadSheetTableViewEditingController.GetAreaBounds(const AArea: TRect): TRect;
begin
  Result := View.ViewInfo.ContentRectToScreenRect(View.ViewInfo.GetAreaBounds(AArea));
end;

function TdxSpreadSheetTableViewEditingController.GetCancelEditingOnExit: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetTableViewEditingController.GetCaretPosByMousePos: TPoint;
var
  P: TPoint;
  S: string;
begin
  Result := cxNullPoint;
  P := DefaultEdit.ScreenToClient(GetMouseCursorPos);
  if P.X <= 0 then Exit;
  Dec(P.X, 2);
  Result.X := 1;
  Result.Y := Min(DefaultEdit.Lines.Count, P.Y div cxTextHeight(DefaultEdit.Style.Font));
  S := DefaultEdit.Lines[Result.Y];
  while (Result.X <= Length(S)) and (cxTextWidth(DefaultEdit.Style.Font, Copy(S, 1, Result.X)) < P.X) do
    Inc(Result.X);
end;

function TdxSpreadSheetTableViewEditingController.GetEditClipArea: TRect;
begin
  Result := View.ViewInfo.GetCellClipArea(CellViewInfo.Row, CellViewInfo.Column);
end;

function TdxSpreadSheetTableViewEditingController.GetEditParent: TWinControl;
begin
  Result := SpreadSheet;
end;

function TdxSpreadSheetTableViewEditingController.GetFocusedCellBounds: TRect;
var
  ABounds: TRect;
begin
  ABounds := cxRectInflate(CellViewInfo.Bounds, -1, 0);
  if CellViewInfo.IsMerged then
    ABounds := View.ViewInfo.GetAreaBounds(CellViewInfo.Row, CellViewInfo.Column, CellViewInfo.MergedCell.Area, ABounds);

  Result := View.ViewInfo.ContentRectToScreenRect(CellViewInfo.ContentRect(ABounds));
  if cxRectIntersect(Result, Result, View.ViewInfo.CellsArea) then
    Result := cxRectScale(cxRectInflate(Result, 2, 0), View.ZoomFactor, 100);
end;

function TdxSpreadSheetTableViewEditingController.GetHideEditOnFocusedRecordChange: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetTableViewEditingController.GetIsEditing: Boolean;
begin
  Result := FEditing;
end;

function TdxSpreadSheetTableViewEditingController.GetValue: TcxEditValue;
begin
  if (EditValue = '') and FCanUseCurrentValue and (Cell <> nil) then
    EditValue := GetValueFromCell(Cell);
  Result := EditValue;
  if FCanUseCurrentValue then
    View.DoInitEditValue(Edit, Result);
end;

function TdxSpreadSheetTableViewEditingController.GetValueFromCell(
  ACell: TdxSpreadSheetCell; AAllowRTF: Boolean = True): TcxEditValue;
var
  AArrayFormula: TdxSpreadSheetCustomFormula;
  ARTFText: string;
begin
  if ACell = nil then
    Result := Null
  else
    if ACell.IsPartOfArrayFormula(@AArrayFormula) = afpSlaveCell then
      Result := AArrayFormula.AsText
    else
      if ACell.IsFormula then
        Result := ACell.AsFormula.AsText
      else
        if AAllowRTF and dxSpreadSheetTextService.GetAsRTF(ACell, ARTFText) then
          Result := ARTFText
        else
          Result := ACell.AsString;
end;

procedure TdxSpreadSheetTableViewEditingController.MultilineEditTextChanged;
var
  R: TRect;
begin
  if CanUpdateMultilineEditHeight then
  begin
    R := GetAdjustedMultilineEditBounds;
    if not cxRectIsEqual(Edit.BoundsRect, R) then
    begin
      Edit.BoundsRect := R;
      Edit.ScrollContent(dirUp);
    end;
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.UpdateReferencesHighlighting;
begin
  if IsEditing and (SpreadSheet.DragAndDropState = ddsNone) then
    ReferenceHighlighter.Calculate(Edit);
end;

procedure TdxSpreadSheetTableViewEditingController.HistoryChanged;
begin
  View.SpreadSheet.DoHistoryChanged;
end;

procedure TdxSpreadSheetTableViewEditingController.SetValue(const AValue: TcxEditValue);
var
  ACell: TdxSpreadSheetCell;
  ACellPosition: TPoint;
  ASelectedArea: TRect;
  ATextValue: Variant;
begin
  ACell := Cell;
  ATextValue := GetEditingText(Edit);
  if (ACell <> nil) and (ACell.IsPartOfArrayFormula <> afpNone) then
  begin
    ASelectedArea := View.GetArrayFormulaArea(ACell.RowIndex, ACell.ColumnIndex);
    if not (IsArrayFormula and View.CanModifyDataInArrayFormulaArea(ASelectedArea, cmmReplace)) then
      raise EdxSpreadSheetCannotChangePartOfArrayError.Create(cxGetResourceString(@sdxErrorCannotChangePartOfArray));
    View.AddArrayFormula(ATextValue, ASelectedArea);
  end
  else
    if VarIsNull(AValue) and (ACell <> nil) then
      ACell.SetTextCore(ATextValue, True)
    else
      if not VarIsNull(AValue) then
      begin
        ACellPosition := GetCellPosition;
        View.BeginUpdate;
        try
          if IsArrayFormula then
            View.AddArrayFormula(ATextValue, View.GetArrayFormulaArea(ACellPosition.Y, ACellPosition.X))
          else
            if IsEditValueFormula or not dxSpreadSheetTextService.SetAsRTF(ACell, AValue) then
            begin
              dxSpreadSheetTextService.ApplyDefaultStyle(ACell, AValue);
              View.CreateCell(ACellPosition.Y, ACellPosition.X).SetTextCore(ATextValue, True);
            end;
        finally
          View.EndUpdate;
        end;
      end;
end;

procedure TdxSpreadSheetTableViewEditingController.StartEditingByTimer;
begin
end;

procedure TdxSpreadSheetTableViewEditingController.UpdateEditPosition;
const
  AlignmentMap: array[TAlignment] of TcxParaFormat2Alignment = (pfaLeft, pfaRight, pfaCenter);
begin
  if Edit <> nil then
  begin
    Edit.BoundsRect := GetAdjustedMultilineEditBounds;
    if (Edit is TcxCustomRichEdit) and (not IsEditValueFormula and ((Cell = nil) or not Cell.IsFormula)) then
      TcxCustomRichEdit(Edit).Paragraph2.Alignment := AlignmentMap[FAlignment];
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.UpdateInplaceParamsPosition;
begin
  // do nothing
end;

function TdxSpreadSheetTableViewEditingController.PrepareEdit(AIsMouseEvent: Boolean): Boolean;
begin
  Result := IsEditing;
  if EditPreparing or EditHiding or not CanInitEditing then
    Exit;

  FEditPreparing := True;
  try
    Result := CellViewInfo <> nil;
    if Result then
    begin
      if Cell = nil then
      begin
        with GetCellPosition do
          View.CreateCell(Y, X);
      end;

      FreeAndNil(FEditData);
      FreeAndNil(SpreadSheet.FDefaultEdit);
      if PropertiesValue = DefaultEditProperties then
      begin
        FEdit := DefaultEdit;
        DefaultEdit.Properties.Assign(DefaultEditProperties);
        DefaultEdit.Properties.HideSelection := False;
      end
      else
        FEdit := EditList.GetEdit(PropertiesValue);

      AssignEditStyle;
      Edit.Visible := False;
      Edit.Parent := nil;
    end;

    FEditing := Result;
    if Edit <> nil then
    begin
      CellViewInfo.Invalidate;
      InitEdit;
      UpdateEditPosition;
    end
    else
      Result := False;
  finally
    FEditPreparing := False;
  end;
  if Result then
    View.Invalidate;
end;

procedure TdxSpreadSheetTableViewEditingController.ReplaceEditValue(const AValue: Variant);
begin
  if Edit is TcxCustomTextEdit then
  begin
    SendMessage(Edit.Handle, WM_SETREDRAW, 0, 0);
    try
      Edit.LockChangeEvents(False, False);
      try
        TcxCustomTextEdit(Edit).EditValue := AValue;
      finally
        TcxCustomTextEdit(Edit).SelStart := MaxInt;
        TcxCustomTextEdit(Edit).SelLength := 0;
        Edit.LockChangeEvents(True, False);
      end;
    finally
      SendMessage(Edit.Handle, WM_SETREDRAW, 1, 1);
    end;
  end
  else
    Edit.EditValue := AValue;
end;

procedure TdxSpreadSheetTableViewEditingController.Redo;
begin
  if CanRedo then
  begin
    ReplaceEditValue(RedoValue);
    HistoryChanged;
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.Undo;
begin
  if CanUndo then
  begin
    RedoValue := Edit.EditValue;
    ReplaceEditValue(EditValue);
    Edit.EditModified := False;
    HistoryChanged;
  end;
end;

procedure TdxSpreadSheetTableViewEditingController.UninitEdit;
begin
  inherited;
  if FPropertiesValue is TcxRichEditProperties then
    TcxRichEditProperties(FPropertiesValue).PlainText := False;
end;

function TdxSpreadSheetTableViewEditingController.IsAutoCompleteAllowed: Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.FormulaAutoComplete;
end;

function TdxSpreadSheetTableViewEditingController.IsAutoCompleteSuggestionsHintsAllowed: Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.FormulaAutoCompleteShowHint;
end;

procedure TdxSpreadSheetTableViewEditingController.KeyDown(var Key: Word; ShiftState: TShiftState);
begin
  Controller.KeyDown(Key, ShiftState);
end;

procedure TdxSpreadSheetTableViewEditingController.Post(AIsArrayFormula: Boolean = False);
begin
  IsArrayFormula := AIsArrayFormula;
  HideEdit(True);
  IsArrayFormula := False;
end;

function TdxSpreadSheetTableViewEditingController.GetCell: TdxSpreadSheetCell;
begin
  if CellViewInfo <> nil then
    Result := CellViewInfo.ActualCell
  else
    Result := nil;
end;

function TdxSpreadSheetTableViewEditingController.GetCellPosition: TPoint;
begin
  Result := CellViewInfo.ActualCellPosition;
end;

function TdxSpreadSheetTableViewEditingController.GetCellViewInfo: TdxSpreadSheetTableViewCellViewInfo;
begin
  Result := View.ViewInfo.FocusedCell;
end;

function TdxSpreadSheetTableViewEditingController.GetDefaultEdit: TcxCustomMemo;
begin
  if (SpreadSheet.FDefaultEdit = nil) or (SpreadSheet.FDefaultEdit.Properties.ClassType <> DefaultEditProperties.ClassType) then
  begin
    FreeAndNil(SpreadSheet.FDefaultEdit);
    SpreadSheet.FDefaultEdit := CreateDefaultEdit;
  end;
  Result := SpreadSheet.FDefaultEdit;
end;

function TdxSpreadSheetTableViewEditingController.GetEditSize(
  const AText: string; ASizeProperties: TcxEditSizeProperties): TSize;
begin
  cxScreenCanvas.Font := DefaultEdit.Style.Font;
  Result := DefaultEditProperties.GetEditContentSize(cxScreenCanvas, DefaultEdit.Style, True, AText, ASizeProperties);
  cxScreenCanvas.Dormant;
end;

function TdxSpreadSheetTableViewEditingController.GetEditText: string;
begin
  if IsEditing then
    Result := GetEditingText(Edit)
  else
    Result := VarToStr(EditValue);
end;

function TdxSpreadSheetTableViewEditingController.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := View.SpreadSheet;
end;

function TdxSpreadSheetTableViewEditingController.GetView: TdxSpreadSheetTableView;
begin
  Result := Controller.View;
end;

{ TdxSpreadSheetInplaceEdit }

function TdxSpreadSheetInplaceEdit.GetController: IdxSpreadSheetInplaceEditController;
begin
  if Owner is TdxCustomSpreadSheet then
    Result := TdxCustomSpreadSheet(Owner).ActiveSheetAsTable.EditingController
  else
    Result := nil;
end;

{ TdxSpreadSheetTableViewCellHintController }

procedure TdxSpreadSheetTableViewCellHintController.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Hide;
end;

procedure TdxSpreadSheetTableViewCellHintController.LockHint;
begin
  Hide;
  Inc(FLockHintCount);
end;

procedure TdxSpreadSheetTableViewCellHintController.UnlockHint;
begin
  Dec(FLockHintCount);
end;

procedure TdxSpreadSheetTableViewCellHintController.Hide;
begin
  FreeAndNil(FDelayTimer);
  if FHintToDisplay <> nil then
  try
    if HintVisible and (FHintToDisplay.Ref <> nil) then
      DoReleaseHint;
  finally
    cxRemoveObjectLink(FHintToDisplay);
    if View.Controller <> nil then
      HintHelper.CancelHint;
    FHintToDisplay := nil;
    HintVisible := False;
  end
end;

procedure TdxSpreadSheetTableViewCellHintController.Show(ACellViewInfo: TdxSpreadSheetCellViewInfo);

  function GetFormatStringForHint: string;
  begin
    if ACellViewInfo is TdxSpreadSheetContainerViewInfo then
      Result := cxGetResourceString(@sdxDefaultHyperlinkShortScreenTip)
    else
      Result := cxGetResourceString(@sdxDefaultHyperlinkScreenTip);
  end;

var
  AComment: TdxSpreadSheetContainer;
  AHintObject: TObject;
  AHyperLink: TdxSpreadSheetHyperlink;
begin
  if (FLockHintCount = 0) and ((FHintToDisplay = nil) or NeedShowHint(ACellViewInfo)) then
  begin
    Hide;

    AHintObject := nil;
    AComment := GetComment(ACellViewInfo);
    if (AComment <> nil) and not AComment.Visible then
      AHintObject := AComment
    else
    begin
      AHyperLink := GetHyperlink(ACellViewInfo);
      if AHyperLink <> nil then
      begin
        AHintObject := ACellViewInfo;
        if havScreenTip in AHyperLink.AssignedValues then
          FHintText := AHyperLink.ScreenTip
        else
          FHintText := Format(GetFormatStringForHint, [dxSpreadSheetFormulaExcludeEqualSymbol(AHyperLink.Value)]);
      end;
    end;

    if AHintObject <> nil then
    begin
      FHintToDisplay := cxAddObjectLink(AHintObject);
      FDelayTimer := cxCreateTimer(DelayTimerHandler, Application.HintPause);
    end;
  end;
end;

function  TdxSpreadSheetTableViewCellHintController.GetComment(
  ACellViewInfo: TdxSpreadSheetCellViewInfo): TdxSpreadSheetContainer;
begin
  if ACellViewInfo is TdxSpreadSheetTableViewCellViewInfo then
    Result := TdxSpreadSheetTableViewCellViewInfo(ACellViewInfo).Comment
  else
    if ACellViewInfo is TdxSpreadSheetCommentContainerViewInfo then
      Result := TdxSpreadSheetCommentContainerViewInfo(ACellViewInfo).Owner
    else
      Result := nil;
end;

function  TdxSpreadSheetTableViewCellHintController.GetHyperlink(
  ACellViewInfo: TdxSpreadSheetCellViewInfo): TdxSpreadSheetHyperlink;
begin
  if ACellViewInfo is TdxSpreadSheetTableViewCellViewInfo then
    Result := View.Hyperlinks.FindItem(
      TdxSpreadSheetTableViewCellViewInfo(ACellViewInfo).Row,
      TdxSpreadSheetTableViewCellViewInfo(ACellViewInfo).Column)
  else
    if ACellViewInfo is TdxSpreadSheetContainerViewInfo then
      Result := TdxSpreadSheetContainerViewInfo(ACellViewInfo).Owner.Hyperlink
    else
      Result := nil;
end;

function TdxSpreadSheetTableViewCellHintController.NeedShowHint(ACellViewInfo: TdxSpreadSheetCellViewInfo): Boolean;
begin
  Result := (GetComment(ACellViewInfo) <> HintToDisplay.Ref) and (HintToDisplay.Ref <> ACellViewInfo);
end;

procedure TdxSpreadSheetTableViewCellHintController.DoReleaseHint;
begin
  if FHintToDisplay.Ref is TdxSpreadSheetContainer then
    ReleaseContainer(TdxSpreadSheetContainer(FHintToDisplay.Ref))
  else
    if FHintToDisplay.Ref is TdxSpreadSheetTableViewCellViewInfo then
      ReleaseCellHint(TdxSpreadSheetCellViewInfo(FHintToDisplay.Ref));
end;

function TdxSpreadSheetTableViewCellHintController.GetHintHelper: TcxControlHintHelper;
begin
  Result := View.Controller.HintHelper;
end;

function TdxSpreadSheetTableViewCellHintController.GetView: TdxSpreadSheetTableView;
begin
  Result := TdxSpreadSheetTableView(inherited View);
end;

procedure TdxSpreadSheetTableViewCellHintController.DelayTimerHandler(Sender: TObject);
begin
  FreeAndNil(FDelayTimer);
  FHintVisible := True;
  if FHintToDisplay.Ref is TdxSpreadSheetContainer then
    ShowContainer(TdxSpreadSheetContainer(FHintToDisplay.Ref))
  else
    if FHintToDisplay.Ref is TdxSpreadSheetCellViewInfo then
      ShowCellHint(TdxSpreadSheetTableViewCellViewInfo(FHintToDisplay.Ref))
    else
      Hide;
end;

procedure TdxSpreadSheetTableViewCellHintController.ReleaseCellHint(ACell: TdxSpreadSheetCellViewInfo);
begin
  if View.Controller <> nil then
    HintHelper.HideHint;
end;

procedure TdxSpreadSheetTableViewCellHintController.ReleaseContainer(AContainer: TdxSpreadSheetContainer);
begin
  AContainer.Visible := False;
  AContainer.Calculator.UpdateAnchors(FContainerToDisplayPrevBounds);
  AContainer.Index := FContainerToDisplayPrevZOrder;
  SpreadSheet.DoCommentHide(AContainer);
end;

procedure TdxSpreadSheetTableViewCellHintController.ShowCellHint(ACell: TdxSpreadSheetCellViewInfo);
var
  AHintBounds: TRect;
  AClientMousePos: TPoint;
begin
  AClientMousePos := SpreadSheet.ScreenToClient(GetMouseCursorPos);
  AHintBounds := cxRectOffset(cxRect(AClientMousePos, Point(SpreadSheet.Width, SpreadSheet.Height)), 0, cxGetCursorSize.cy);
  if FHintText <> '' then
    HintHelper.ShowHint(AHintBounds, cxTextRect(AHintBounds), HintText, True, ACell)
  else
    Hide;
end;

procedure TdxSpreadSheetTableViewCellHintController.ShowContainer(AContainer: TdxSpreadSheetContainer);
var
  ARect: TRect;
begin
  if not SpreadSheet.DoCommentShow(AContainer) then
  begin
    FContainerToDisplayPrevBounds := AContainer.Calculator.CalculateBounds;
    FContainerToDisplayPrevZOrder := AContainer.Index;
    if AContainer is TdxSpreadSheetCommentContainer then
    begin
      ARect := TdxSpreadSheetCommentContainerHelper.GetDefaultPosition(TdxSpreadSheetCommentContainer(AContainer).Cell);
      ARect := cxRectSetOrigin(FContainerToDisplayPrevBounds, ARect.TopLeft);
      AContainer.Calculator.UpdateAnchors(ARect);
      AContainer.BringToFront;
    end;
    AContainer.Visible := True;
  end;
end;

{ TdxSpreadSheetHintHelper }

constructor TdxSpreadSheetHintHelper.Create(AOwner: TdxSpreadSheetTableViewController);
begin
  FOwner := AOwner;
end;

function TdxSpreadSheetHintHelper.GetOwnerControl: TcxControl;
begin
  Result := FOwner.SpreadSheet;
end;


{ TdxSpreadSheetTableViewController }

constructor TdxSpreadSheetTableViewController.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FEditingController := CreateEditingController;
  FScrollingTimer := cxCreateTimer(ScrollingTimerHandler, 25, False);
  FCellHintController := TdxSpreadSheetTableViewCellHintController.Create(View);
  FHintHelper := TdxSpreadSheetHintHelper.Create(Self);
  FClickTimer := cxCreateTimer(ClickTimerHandler, GetDblClickInterval * 2, False);
  CanClickOnTheCell := True;
end;

destructor TdxSpreadSheetTableViewController.Destroy;
begin
  FreeAndNil(FClickTimer);
  FHintHelper.HideHint;
  FreeAndNil(FHintHelper);
  FreeAndNil(FCellHintController);
  FreeAndNil(FEditingController);
  FreeAndNil(FScrollingTimer);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewController.ApplyBestFitForSelectedHeaderCells(ACells: TdxSpreadSheetCellViewInfoList);
var
  ACell: TdxSpreadSheetTableViewHeaderCellViewInfo;
  I: Integer;
begin
  View.BeginUpdate;
  try
    for I := 0 to ACells.Count - 1 do
    begin
      ACell := TdxSpreadSheetTableViewHeaderCellViewInfo(ACells.Items[I]);
      if ACell.State = cxbsHot then
        ACell.ApplyBestFit;
    end;
  finally
    View.EndUpdate;
  end;
end;

function TdxSpreadSheetTableViewController.CalculateNewFocusedRowIndex(AOffsetFromFirstRow: Integer): Integer;
begin
  Result := ViewInfo.FirstScrollableRow;
  while (AOffsetFromFirstRow > 0) and (Result <= View.BottomRow) do
  begin
    if Rows.GetItemVisible(Result) then
      Dec(AOffsetFromFirstRow);
    Inc(Result);
  end;
  dxSpreadSheetValidate(Result, ViewInfo.FirstScrollableRow, View.BottomRow);
end;

function TdxSpreadSheetTableViewController.CanExecuteHyperlink: Boolean;
begin
  Result := CanClickOnTheCell and not IsCtrlPressed and SpreadSheet.Focused;
end;

function TdxSpreadSheetTableViewController.CanFocusOnClick: Boolean;
begin
  Result := not EditingController.CanInsertNewAreaViaMouse and
    (HitTest.HitObject <> EditingController.ReferenceHighlighter);
end;

procedure TdxSpreadSheetTableViewController.CheckCtrlCommand(const AKey: Char);
begin
  if View.OptionsProtection.ActualAllowFormatCells then
    case AKey of
      '1':
        ShowFormatCellsDialog(View);
      '2', 'B':
        TdxSpreadSheetCellStylesHelper.SaveFontStyle(View, fsBold);
      '3', 'I':
        TdxSpreadSheetCellStylesHelper.SaveFontStyle(View, fsItalic);
      '4', 'U':
        TdxSpreadSheetCellStylesHelper.SaveFontStyle(View, fsUnderline);
      '5':
        TdxSpreadSheetCellStylesHelper.SaveFontStyle(View, fsStrikeOut);
    end;

  case AKey of
    '-':
      View.DeleteCells;
    '+':
      View.InsertCells;
    '0':
      HideSelectedColumns;
    '9':
      HideSelectedRows;
    'A':
      TrySelectAll;
    'C':
      View.CopyToClipboard;
    'D':
      View.FillCells(True);
    'H':
      ShowFindAndReplaceDialog(SpreadSheet, 1);
    'F':
      ShowFindAndReplaceDialog(SpreadSheet);
    'K':
      View.EditHyperlink;
    'R':
      View.FillCells(False);
    'X':
      View.CutToClipboard;
    'V':
      View.PasteFromClipboard;
    'Z':
      SpreadSheet.History.Undo;
    'Y':
      SpreadSheet.History.Redo;
    Chr(186):
      EditingController.ShowEdit(dxGetFormattedResult(Date, View.FormatSettings.Data.ShortDateFormat).Text);
    Chr(192):
      View.ToggleShowFormulas;
    Chr(222):
      EditingController.ShowEditWithTopCellValue(FocusedRow, FocusedColumn, True);
  end;
end;

procedure TdxSpreadSheetTableViewController.CheckCtrlShiftCommand(const AKey: Word);
begin
  if View.OptionsProtection.ActualAllowFormatCells then
    case AKey of
      49:
        TdxSpreadSheetCellStylesHelper.SaveNumberFormat(View, sknfNumber00);
      50:
        TdxSpreadSheetCellStylesHelper.SaveNumberFormat(View, sknfTime);
      51:
        TdxSpreadSheetCellStylesHelper.SaveNumberFormat(View, sknfDate);
      52:
        TdxSpreadSheetCellStylesHelper.SaveNumberFormat(View, sknfCurrency);
      53:
        TdxSpreadSheetCellStylesHelper.SaveNumberFormat(View, sknfPercent);
      54:
        TdxSpreadSheetCellStylesHelper.SaveNumberFormat(View, sknfScientific);
      55:
        TdxSpreadSheetCellStylesHelper.SaveBorder(View, True);
      80:
        ShowFormatCellsDialog(View, 2);
      189:
        TdxSpreadSheetCellStylesHelper.SaveBorder(View, False);
    end;

  case AKey of
    192:
      TdxSpreadSheetCellStylesHelper.SaveNumberFormat(View, sknfGeneral);
    56:
      SelectDataRegionAroundActiveCell;
    79:
      SelectCellsWithComments;
    186:
      EditingController.ShowEdit(dxGetFormattedResult(Time, 'hh:mm AM/PM').Text);
    187:
      View.InsertCells;
    222:
      EditingController.ShowEditWithTopCellValue(FocusedRow, FocusedColumn, False);
  end;
end;

procedure TdxSpreadSheetTableViewController.CheckScrollArea(const P: TPoint);

  function AllowAutoScroll: Boolean;
  begin
    Result := (SelectionMode <> smNone) or (SpreadSheet.DragAndDropState = ddsInProcess);
  end;

  function IsPtInScrollableArea: Boolean;
  begin
    Result := (PtInRect(ViewInfo.ScrollableArea, P) or
      not PtInRect(cxRectOffset(ViewInfo.CellsArea, View.GetPartOffsetByPoint(P), False), P)) and
      not PtInRect(cxRectInflate(ViewInfo.ScrollableArea, -dxSpreadSheetScrollAreaWidth), P);
  end;

begin
  FScrollingTimer.Enabled := AllowAutoScroll and IsPtInScrollableArea;
end;

procedure TdxSpreadSheetTableViewController.ClickTimerHandler(Sender: TObject);
begin
  ClickTimer.Enabled := False;
  CanClickOnTheCell := False;
  SpreadSheet.UpdateCursor;
end;

function TdxSpreadSheetTableViewController.ContainerCanDelete: Boolean;
begin
  Result := View.CanDelete;
end;

function TdxSpreadSheetTableViewController.ContextPopup(const P: TPoint): Boolean;
begin
  CellHintController.LockHint;
  try
    Result := inherited ContextPopup(P);
  finally
    CellHintController.UnlockHint;
  end;
end;

function TdxSpreadSheetTableViewController.CreateEditingController: TdxSpreadSheetTableViewEditingController;
begin
  Result := TdxSpreadSheetTableViewEditingController.Create(Self);
end;

procedure TdxSpreadSheetTableViewController.DblClick;
var
  ACell: TdxSpreadSheetTableViewHeaderCellViewInfo;
  APos: TPoint;
begin
  CellHintController.Hide;
  APos := View.SpreadSheet.GetMouseCursorClientPos;
  View.HitTest.Calculate(APos);

  if View.HitTest.HitAtResizeArea then
  begin
    if HitTest.HitAtColumnHeader or HitTest.HitAtRowHeader then
    begin
      ACell := TdxSpreadSheetTableViewHeaderCellViewInfo(HitTest.HitObject);
      if ACell.State = cxbsHot then
      begin
        if HitTest.HitAtColumnHeader then
          ApplyBestFitForSelectedHeaderCells(ViewInfo.ColumnsHeader)
        else
          ApplyBestFitForSelectedHeaderCells(ViewInfo.RowsHeader);
      end
      else
        ACell.ApplyBestFit;
    end;
  end
  else

  if View.HitTest.HitAtContainer then
  begin
    if (FocusedContainer <> nil) and View.CanEditContainers then
      ShowContainerCustomizationDialog(FocusedContainer, -1);
  end
  else
    EditingController.ShowEditByMouse(APos.X, APos.Y, KeyboardStateToShiftState);
end;

function TdxSpreadSheetTableViewController.ExpandDataRegion(const AArea: TRect): TRect;
begin
  Result := AArea;
  if IsRowPartHasContent(AArea.Top - 1, AArea.Left - 1, AArea.Right + 1) then
    Result.Top := AArea.Top - 1;
  if IsRowPartHasContent(AArea.Bottom + 1, AArea.Left - 1, AArea.Right + 1) then
    Result.Bottom := AArea.Bottom + 1;
  if IsColumnPartHasContent(AArea.Left - 1, AArea.Top - 1, AArea.Bottom + 1) then
    Result.Left := AArea.Left - 1;
  if IsColumnPartHasContent(AArea.Right + 1, AArea.Top - 1, AArea.Bottom + 1) then
    Result.Right := AArea.Right + 1;
  if not cxRectIsEqual(Result, AArea) then
    Result := ExpandDataRegion(Result);
end;

function TdxSpreadSheetTableViewController.GetDataRegionAroundActiveCell: TRect;
var
  ARow, AColumn: Integer;
begin
  ARow := FocusedRow;
  AColumn := FocusedColumn;
  Result := ExpandDataRegion(cxRect(AColumn, ARow, AColumn, ARow));
end;

function TdxSpreadSheetTableViewController.IMEComposition(var AMessage: TMessage): Boolean;
begin
  Result := EditingController.IMEComposition(AMessage);
end;

function TdxSpreadSheetTableViewController.IMEStartComposition: Boolean;
begin
  Result := EditingController.IMEStartComposition;
end;

function TdxSpreadSheetTableViewController.IsColumnPartHasContent(const AColumn, AStartRow, AEndRow: Integer): Boolean;
var
  ARow: Integer;
begin
  Result := False;
  if (AColumn < 0) or (AColumn > dxSpreadSheetMaxColumnIndex) then
    Exit;
  for ARow := AStartRow to AEndRow do
  begin
    Result := not View.Cells[ARow, AColumn].IsEmpty;
    if Result  then
      Break;
  end;
end;

function TdxSpreadSheetTableViewController.IsRowPartHasContent(const ARow, AStartColumn, AEndColumn: Integer): Boolean;
var
  AColumn: Integer;
begin
  Result := False;
  if (ARow < 0) or (ARow > dxSpreadSheetMaxRowIndex) then
    Exit;
  for AColumn := AStartColumn to AEndColumn do
  begin
    Result := not View.Cells[ARow, AColumn].IsEmpty;
    if Result  then
      Break;
  end;
end;

procedure TdxSpreadSheetTableViewController.SelectDataRegionAroundActiveCell;
var
  AArea: TRect;
begin
  AArea := GetDataRegionAroundActiveCell;
  Selection.Add(AArea, [], AArea.Top, AArea.Left);
end;

procedure TdxSpreadSheetTableViewController.TrySelectAll;
var
  AArea: TRect;
begin
  AArea := Selection.Area;
  if (dxSpreadSheetAreaWidth(AArea) > 1) or (dxSpreadSheetAreaHeight(AArea) > 1) then
    Selection.SelectAll
  else
  begin
    AArea := GetDataRegionAroundActiveCell;
    if (dxSpreadSheetAreaWidth(AArea) = 1) and (dxSpreadSheetAreaHeight(AArea) = 1) then
      Selection.SelectAll
    else
      Selection.Add(AArea, [], FocusedRow, FocusedColumn);
  end;
end;

function TdxSpreadSheetTableViewController.CanFocusMergedCellsPart(
  const AArea: TRect; ACell: TdxSpreadSheetCell; AGoForward: Boolean): Boolean;
var
  R, ACorner: TRect;
  ARow, AColumn, I, J: Integer;
begin
  Result := True;
  R := MergedCells.ExpandArea(ACell.ColumnIndex, ACell.RowIndex);
  if cxRectIsEqual(R, cxRect(ACell.ColumnIndex, ACell.RowIndex, ACell.ColumnIndex, ACell.RowIndex)) then
    Exit;

  if AGoForward then
  begin
    ACorner.TopLeft := R.TopLeft;
    ACorner.Right := ACell.ColumnIndex;
    ACorner.Bottom := ACell.RowIndex;
  end
  else
  begin
    ACorner.Left := ACell.ColumnIndex;
    ACorner.Top := ACell.RowIndex;
    ACorner.BottomRight := R.BottomRight;
  end;
  I := ACorner.Top;
  while (I <= ACorner.Bottom) and Result do
  begin
    if AGoForward then
      ARow := I
    else
      ARow := ACorner.Bottom - (I - ACorner.Top);
    J := ACorner.Left;
    while (J <= ACorner.Right) and Result do
    begin
      if AGoForward then
        AColumn := J
      else
        AColumn := ACorner.Right - (J - ACorner.Left);
      if not((ARow = ACell.RowIndex) and (AColumn = ACell.ColumnIndex)) then
        Result := not dxSpreadSheetContains(AArea, ARow, AColumn) or (not Rows[ARow].Visible) or (not Columns[AColumn].Visible);
      Inc(J);
    end;
    Inc(I);
  end;
end;

procedure TdxSpreadSheetTableViewController.FocusContainer(AContainer: TdxSpreadSheetContainer; AMakeVisible: Boolean);
begin
  inherited FocusContainer(AContainer, AMakeVisible);
  if AMakeVisible then
    View.MakeVisible(FocusedContainer);
end;

procedure TdxSpreadSheetTableViewController.FocusNextPossibleCell(ADirection: TcxArrowDirection; AUnlockedCellsOnly: Boolean);

  function FindNextCell(var X, Y: Integer; const AArea: TRect;
    ASelectionAreaIndex: Integer; ANextX, ANextY, APrevX, APrevY: TcxArrowDirection): Boolean;
  var
    ATempValue: Integer;
  begin
    Result := GetNextVisibleCell(X, ADirection, ATempValue);
    if Result then
      X := ATempValue;
    if ASelectionAreaIndex >= 0 then
    begin
      if (X > AArea.Right) or not Result and (X >= AArea.Right) then
        Result := GetNextVisibleCell(AArea.Left - 1, ANextX, X) and GetNextVisibleCell(Y, ANextY, Y)
      else
        if (X < AArea.Left) or not Result and (X <= AArea.Left) then
          Result := GetNextVisibleCell(AArea.Right + 1, APrevX, X) and GetNextVisibleCell(Y, APrevY, Y);

      Result := Result and InRange(Y, AArea.Top, AArea.Bottom);
    end;
  end;

const
  ShiftStateForFocusing: array[Boolean] of TShiftState = ([], [ssCtrl]);
var
  ACell: TdxSpreadSheetCell;
  AColumn: Integer;
  AGoForward: Boolean;
  ARect: TRect;
  AResult: Boolean;
  ARow: Integer;
  ASelectionAreaIndex: Integer;
begin
  AGoForward := ADirection in [adRight, adDown];
  if not (Selection.IsAreaMoreThanOneCell and GetSelectionAreaByFocusedCell(ARect, ASelectionAreaIndex)) then
  begin
    ARect := dxSpreadSheetEntireSheetArea;
    ASelectionAreaIndex := -1;
  end;

  if AUnlockedCellsOnly then
  begin
    if ADirection in [adLeft, adRight] then
      ACell := FindUnlockedCellByColumn(ARect, ASelectionAreaIndex, FocusedRow, FocusedColumn, AGoForward)
    else
      ACell := FindUnlockedCellByRow(ARect, ASelectionAreaIndex, FocusedRow, FocusedColumn, AGoForward);

    if ACell <> nil then
      Selection.SetFocused(ACell.RowIndex, ACell.ColumnIndex, ShiftStateForFocusing[ASelectionAreaIndex >= 0]);
  end
  else
  begin
    ARow := FocusedRow;
    AColumn := FocusedColumn;
    if ADirection in [adLeft, adRight] then
      AResult := FindNextCell(AColumn, ARow, ARect, ASelectionAreaIndex, adRight, adDown, adLeft, adUp)
    else
      AResult := FindNextCell(ARow, AColumn, cxRectRotate(ARect), ASelectionAreaIndex, adDown, adRight, adUp, adLeft);

    if not AResult then
      GetNextSelectedAreaCell(ASelectionAreaIndex, AGoForward, ARow, AColumn);
    Selection.SetFocused(ARow, AColumn, ShiftStateForFocusing[ASelectionAreaIndex >= 0]);
  end;
end;

function TdxSpreadSheetTableViewController.GetNextColumn(AColumn: TdxSpreadSheetTableColumn; AGoForward: Boolean): TdxSpreadSheetTableColumn;
begin
  if AGoForward then
    Result := TdxSpreadSheetTableColumn(AColumn.Next)
  else
    Result := TdxSpreadSheetTableColumn(AColumn.Prev)
end;

function TdxSpreadSheetTableViewController.GetNextNonEmptyCell(
  AItems: TdxSpreadSheetTableItems; AFocusedItemIndex, AFocusedCellIndex: Integer; AGoForward: Boolean): Integer;
var
  AIndex: Integer;
  AItem: TdxSpreadSheetTableItem;
begin
  AItem := AItems.Items[AFocusedItemIndex];
  if AItem <> nil then
  begin
    AIndex := Min(AFocusedCellIndex, AItem.CellCount) + ValueIncr[AGoForward];
    while InRange(AIndex, 0, AItem.CellCount - 1) do
    begin
      if AItem.Cells[AIndex] <> nil then
        Exit(AIndex);
      Inc(AIndex, ValueIncr[AGoForward]);
    end;
  end;
  if AGoForward then
    Result := AItems.GetOppositeItems.GetMaxItemIndex
  else
    Result := 0;
end;

function TdxSpreadSheetTableViewController.GetNextRow(ARow: TdxSpreadSheetTableRow; AGoForward: Boolean): TdxSpreadSheetTableRow;
begin
  if AGoForward then
    Result := TdxSpreadSheetTableRow(ARow.Next)
  else
    Result := TdxSpreadSheetTableRow(ARow.Prev);
end;

function TdxSpreadSheetTableViewController.GetNextUnlockedCellByColumn(
  const AArea: TRect; AStartRow, AStartColumn: Integer; AGoForward: Boolean): TdxSpreadSheetCell;

  function IsCellReallyNext(ACell: TdxSpreadSheetCell): Boolean;
  begin
    if AGoForward then
      Result := ((ACell.RowIndex > AStartRow) or ((ACell.RowIndex = AStartRow) and (ACell.ColumnIndex > AStartColumn))) and
        CanFocusMergedCellsPart(AArea, ACell, AGoForward)
    else
      Result := ((ACell.RowIndex < AStartRow) or ((ACell.RowIndex = AStartRow) and (ACell.ColumnIndex < AStartColumn))) and
        CanFocusMergedCellsPart(AArea, ACell, AGoForward);
  end;

var
  ARow: TdxSpreadSheetTableRow;
  AColumn, AFirstColumn: TdxSpreadSheetTableColumn;
  ACell: TdxSpreadSheetCell;
begin
  Result := nil;
  if AGoForward then
  begin
    AStartColumn := MergedCells.ExpandArea(AStartColumn, AStartRow).Right;
    ARow := GetSearchAreaFirstRow(AArea);
    AFirstColumn := GetSearchAreaFirstColumn(AArea);
  end
  else
  begin
    AStartColumn := MergedCells.ExpandArea(AStartColumn, AStartRow).Left;
    ARow := GetSearchAreaLastRow(AArea);
    AFirstColumn := GetSearchAreaLastColumn(AArea);
  end;
  if AFirstColumn <> nil then
    while (ARow <> nil) and (Result = nil) do
    begin
      if ARow.Visible and dxSpreadSheetContainsRow(AArea, ARow.Index) then
      begin
        AColumn := AFirstColumn;
        while (AColumn <> nil) and (Result = nil) and dxSpreadSheetContainsColumn(AArea, AColumn.Index) do
        begin
          if AColumn.Visible then
          begin
            ACell := ARow.Cells[AColumn.Index];
            if (ACell <> nil) and not (csLocked in ACell.StyleHandle.States) and IsCellReallyNext(ACell) then
              Result := ACell;
          end;
          AColumn := GetNextColumn(AColumn, AGoForward);
        end;
      end;
      ARow := GetNextRow(ARow, AGoForward);
    end;
end;

function TdxSpreadSheetTableViewController.GetNextUnlockedCellByRow(
  const AArea: TRect; AStartRow, AStartColumn: Integer; AGoForward: Boolean): TdxSpreadSheetCell;

  function IsCellReallyNext(ACell: TdxSpreadSheetCell): Boolean;
  begin
    if AGoForward then
      Result := ((ACell.ColumnIndex > AStartColumn) or ((ACell.ColumnIndex = AStartColumn) and (ACell.RowIndex > AStartRow))) and
        CanFocusMergedCellsPart(AArea, ACell, AGoForward)
    else
      Result := ((ACell.ColumnIndex < AStartColumn) or ((ACell.ColumnIndex = AStartColumn) and (ACell.RowIndex < AStartRow))) and
        CanFocusMergedCellsPart(AArea, ACell, AGoForward);
  end;

var
  AColumn: TdxSpreadSheetTableColumn;
  ARow, AFirstRow: TdxSpreadSheetTableRow;
  ACell: TdxSpreadSheetCell;
begin
  Result := nil;
  if AGoForward then
  begin
    AStartRow := MergedCells.ExpandArea(AStartColumn, AStartRow).Bottom;
    AColumn := GetSearchAreaFirstColumn(AArea);
    AFirstRow := GetSearchAreaFirstRow(AArea);
  end
  else
  begin
    AStartRow := MergedCells.ExpandArea(AStartColumn, AStartRow).Top;
    AColumn := GetSearchAreaLastColumn(AArea);
    AFirstRow := GetSearchAreaLastRow(AArea);
  end;
  if AFirstRow <> nil then
    while (AColumn <> nil) and (Result = nil) do
    begin
      if AColumn.Visible and dxSpreadSheetContainsColumn(AArea, AColumn.Index) then
      begin
        ARow := AFirstRow;
        while (ARow <> nil) and (Result = nil) and dxSpreadSheetContainsRow(AArea, ARow.Index) do
        begin
          if ARow.Visible then
          begin
            ACell := AColumn.Cells[ARow.Index];
            if (ACell <> nil) and not (csLocked in ACell.StyleHandle.States) and IsCellReallyNext(ACell) then
              Result := ACell;
          end;
          ARow := GetNextRow(ARow, AGoForward);
        end;
      end;
      AColumn := GetNextColumn(AColumn, AGoForward);
    end;
end;

function TdxSpreadSheetTableViewController.GetNextVisibleCell(AIndex: Integer; ADirection: TcxArrowDirection): Integer;
begin
  if not GetNextVisibleCell(AIndex, ADirection, Result) then
    Result := AIndex;
end;

function TdxSpreadSheetTableViewController.GetNextVisibleCell(
  AIndex: Integer; ADirection: TcxArrowDirection; out AResult: Integer): Boolean;

  function ExpandAreaByMergedCells(AIndex: Integer; out AIndex1, AIndex2: Integer): Boolean;
  var
    AArea: TRect;
  begin
    if ADirection in [adLeft, adRight] then
    begin
      AArea := MergedCells.ExpandArea(AIndex, FocusedRow);
      AIndex1 := AArea.Left;
      AIndex2 := AArea.Right;
    end
    else
    begin
      AArea := MergedCells.ExpandArea(FocusedColumn, AIndex);
      AIndex1 := AArea.Top;
      AIndex2 := AArea.Bottom;
    end;
    Result := AIndex1 < AIndex2;
  end;

var
  AIndex1, AIndex2: Integer;
begin
  Result := GetTableItemsForNavigation(ADirection).GetNextVisibleItemIndex(AIndex, ADirection in [adRight, adDown], AResult);
  if Result and ExpandAreaByMergedCells(AResult, AIndex1, AIndex2) then
  begin
    if ADirection in [adRight, adDown] then
    begin
      if (AResult > AIndex1) and (AIndex + 1 > AIndex1) then
        Result := GetTableItemsForNavigation(ADirection).GetNextVisibleItemIndex(AIndex2, True, AResult)
    end
    else
      if (AResult < AIndex2) and (AIndex - 1 < AIndex2) then
        Result := GetTableItemsForNavigation(ADirection).GetNextVisibleItemIndex(AIndex1, False, AResult)
  end;
end;

function TdxSpreadSheetTableViewController.GetSearchAreaFirstColumn(const AArea: TRect): TdxSpreadSheetTableColumn;
var
  AColumn: TdxSpreadSheetTableColumn;
begin
  Result := nil;
  AColumn := TdxSpreadSheetTableColumn(View.Columns.First);
  while (AColumn <> nil) and (Result = nil) do
  begin
    if (AColumn.Index >= AArea.Left) and AColumn.Visible then
      Result := AColumn;
    AColumn := GetNextColumn(AColumn, True);
  end;
end;

function TdxSpreadSheetTableViewController.GetSearchAreaFirstRow(const AArea: TRect): TdxSpreadSheetTableRow;
var
  ARow: TdxSpreadSheetTableRow;
begin
  Result := nil;
  ARow := TdxSpreadSheetTableRow(View.Rows.First);
  while (ARow <> nil) and (Result = nil) do
  begin
    if (ARow.Index >= AArea.Top) and ARow.Visible then
      Result := ARow;
    ARow := GetNextRow(ARow, True);
  end;
end;

function TdxSpreadSheetTableViewController.GetSearchAreaLastColumn(const AArea: TRect): TdxSpreadSheetTableColumn;
var
  AColumn: TdxSpreadSheetTableColumn;
begin
  Result := nil;
  AColumn := TdxSpreadSheetTableColumn(View.Columns.Last);
  while (AColumn <> nil) and (Result = nil) do
  begin
    if (AColumn.Index <= AArea.Right) and AColumn.Visible then
      Result := AColumn;
    AColumn := GetNextColumn(AColumn, False);
  end;
end;

function TdxSpreadSheetTableViewController.GetSearchAreaLastRow(const AArea: TRect): TdxSpreadSheetTableRow;
var
  ARow: TdxSpreadSheetTableRow;
begin
  Result := nil;
  ARow := TdxSpreadSheetTableRow(View.Rows.Last);
  while (ARow <> nil) and (Result = nil) do
  begin
    if (ARow.Index <= AArea.Bottom) and ARow.Visible then
      Result := ARow;
    ARow := GetNextRow(ARow, False);
  end;
end;

procedure TdxSpreadSheetTableViewController.GetNextSelectedAreaCell(AIndex: Integer; AGoForward: Boolean; var ARow, AColumn: Integer);
begin
  AIndex := GetNextSelectedAreaIndex(AIndex, AGoForward);
  if AGoForward then
  begin
    AColumn := GetNextVisibleCell(Selection[AIndex].Left - 1, adRight);
    ARow := GetNextVisibleCell(Selection[AIndex].Top - 1, adDown);
  end
  else
  begin
    AColumn := GetNextVisibleCell(Selection[AIndex].Right + 1, adLeft);
    ARow := GetNextVisibleCell(Selection[AIndex].Bottom + 1, adUp);
  end;
end;

function TdxSpreadSheetTableViewController.GetNextSelectedAreaIndex(AIndex: Integer; AGoForward: Boolean): Integer;
begin
  Result := AIndex + ValueIncr[AGoForward];
  if Result >= Selection.Count then
    Result := 0
  else
    if Result < 0 then
      Result := Selection.Count - 1;
end;

function TdxSpreadSheetTableViewController.GetSelectionAreaByFocusedCell(out ARect: TRect; out AIndex: Integer): Boolean;
begin
  Result := False;
  AIndex := Selection.Count - 1;
  while AIndex >= 0 do
  begin
    ARect := Selection[AIndex].Rect;
    Result := dxSpreadSheetContains(ARect, FocusedRow, FocusedColumn);
    if Result then
      Break;
    Dec(AIndex);
  end;
end;

function TdxSpreadSheetTableViewController.GetSelectionItem(ASide: TcxBorder; AShift: TShiftState): Integer;
var
  R: TRect;
begin
  if (ssShift in AShift) and (Selection.Count > 0) then
  begin
    R := Selection[Selection.Count - 1].Rect;
    case ASide of
      bLeft:
        Result := IfThen(R.Left < FocusedColumn, R.Left, IfThen(FocusedColumn < R.Right, R.Right, FocusedColumn));
      bTop:
        Result := IfThen(R.Top < FocusedRow, R.Top, IfThen(FocusedRow < R.Bottom, R.Bottom, FocusedRow));
      bRight:
        Result := IfThen(R.Right > FocusedColumn, R.Right, IfThen(FocusedColumn > R.Left, R.Left, FocusedColumn));
    else
      Result := IfThen(R.Bottom > FocusedRow, R.Bottom, IfThen(FocusedRow > R.Top, R.Top, FocusedRow));
    end;
  end
  else
    if ASide in [bLeft, bRight] then
      Result := FocusedColumn
    else
      Result := FocusedRow;
end;

function TdxSpreadSheetTableViewController.FindUnlockedCellByColumn(
  const AStartArea: TRect; const AStartIndex, AStartRow, AStartColumn: Integer; AGoForward: Boolean): TdxSpreadSheetCell;

  function GetAreaStartColumn(const AArea: TRect): Integer;
  begin
    if AGoForward then
      Result := AArea.Left - 1
    else
      Result := AArea.Right + 1;
  end;

  function GetAreaStartRow(const AArea: TRect): Integer;
  begin
    if AGoForward then
      Result := AArea.Top
    else
      Result := AArea.Bottom;
  end;

var
  ANextIndex: Integer;
  ANextArea: TRect;
begin
  Result := GetNextUnlockedCellByColumn(AStartArea, AStartRow, AStartColumn, AGoForward);
  if Result = nil then
    if AStartIndex < 0 then
      Result := GetNextUnlockedCellByColumn(AStartArea, GetAreaStartRow(AStartArea), GetAreaStartColumn(AStartArea), AGoForward)
    else
    begin
      ANextIndex := AStartIndex;
      repeat
        ANextIndex := GetNextSelectedAreaIndex(ANextIndex, AGoForward);
        ANextArea := Selection[ANextIndex].Rect;
        Result := GetNextUnlockedCellByColumn(ANextArea, GetAreaStartRow(ANextArea), GetAreaStartColumn(ANextArea), AGoForward)
      until (AStartIndex = ANextIndex) or (Result <> nil);
    end;
end;

function TdxSpreadSheetTableViewController.FindUnlockedCellByRow(
  const AStartArea: TRect; const AStartIndex, AStartRow, AStartColumn: Integer; AGoForward: Boolean): TdxSpreadSheetCell;

  function GetAreaStartColumn(const AArea: TRect): Integer;
  begin
    if AGoForward then
      Result := AArea.Left
    else
      Result := AArea.Right;
  end;

  function GetAreaStartRow(const AArea: TRect): Integer;
  begin
    if AGoForward then
      Result := AArea.Top - 1
    else
      Result := AArea.Bottom + 1;
  end;

var
  ANextIndex: Integer;
  ANextArea: TRect;
begin
  Result := GetNextUnlockedCellByRow(AStartArea, AStartRow, AStartColumn, AGoForward);
  if Result = nil then
    if AStartIndex < 0 then
      Result := GetNextUnlockedCellByRow(AStartArea, GetAreaStartRow(AStartArea), GetAreaStartColumn(AStartArea), AGoForward)
    else
    begin
      ANextIndex := AStartIndex;
      repeat
        ANextIndex := GetNextSelectedAreaIndex(ANextIndex, AGoForward);
        ANextArea := Selection[ANextIndex].Rect;
        Result := GetNextUnlockedCellByRow(ANextArea, GetAreaStartRow(ANextArea), GetAreaStartColumn(ANextArea), AGoForward)
      until (AStartIndex = ANextIndex) or (Result <> nil);
    end;
end;

procedure TdxSpreadSheetTableViewController.StopEditing;
begin
  if EditingController.IsEditing then
    EditingController.HideEdit(True);
end;

procedure TdxSpreadSheetTableViewController.UpdateStates;
begin
  ViewInfo.GroupingAreas.UpdateState;
end;

procedure TdxSpreadSheetTableViewController.GetCellFromMouseCursor(var ARow, AColumn: Integer);
begin
  if HitTest.HitAtColumnHeader then
  begin
    AColumn := TdxSpreadSheetTableViewHeaderCellViewInfo(HitTest.HitObject).Index;
    ARow := -1;
  end
  else

  if HitTest.HitAtRowHeader then
  begin
    ARow := TdxSpreadSheetTableViewHeaderCellViewInfo(HitTest.HitObject).Index;
    AColumn := -1;
  end
  else
    View.CellAtPoint(HitTest.ActualHitPoint, ARow, AColumn, True);
end;

function TdxSpreadSheetTableViewController.GetHitTest: TdxSpreadSheetCustomHitTest;
begin
  Result := HitTest;
end;

procedure TdxSpreadSheetTableViewController.GetRedoActionCount(var ARedoCount: Integer);
begin
  if EditingController.IsEditing then
  begin
    if EditingController.CanRedo then
      ARedoCount := 1
    else
      ARedoCount := 0;
  end;
end;

procedure TdxSpreadSheetTableViewController.GetUndoActionCount(var AUndoCount: Integer);
begin
  if EditingController.IsEditing then
  begin
    if EditingController.CanUndo then
      AUndoCount := 1
    else
      AUndoCount := 0;
  end;
end;

procedure TdxSpreadSheetTableViewController.HideSelectedColumns;
var
  ASelection: TdxSpreadSheetTableViewSelection;
  AAreaIndex, AColumn: Integer;
  ARect: TRect;
begin
  View.BeginUpdate;
  try
    View.History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
    ASelection := View.Selection;
    for AAreaIndex := 0 to ASelection.Count - 1 do
    begin
      ARect := ASelection[AAreaIndex].Rect;
      for AColumn := ARect.Left to ARect.Right do
        View.Columns.CreateItem(AColumn).Visible := False;
    end;
  finally
    View.History.EndAction;
    View.EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableViewController.HideSelectedRows;
var
  ASelection: TdxSpreadSheetTableViewSelection;
  AAreaIndex, ARow: Integer;
  ARect: TRect;
begin
  View.BeginUpdate;
  try
    View.History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
    ASelection := View.Selection;
    for AAreaIndex := 0 to ASelection.Count - 1 do
    begin
      ARect := ASelection[AAreaIndex].Rect;
      for ARow := ARect.Top to ARect.Bottom do
        View.Rows.CreateItem(ARow).Visible := False;
    end;
  finally
    View.History.EndAction;
    View.EndUpdate;
  end;
end;

function TdxSpreadSheetTableViewController.DoActiveCellChanging(AColumn, ARow: Integer): Boolean;
begin
  Result := (ForcedSelectionMode <> smNone) or View.DoActiveCellChanging(AColumn, ARow);
end;

function TdxSpreadSheetTableViewController.ContainerProcessKeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := (Key = VK_DELETE) and ContainerCanDelete and (ssShift in Shift);
  if Result then
    TdxSpreadSheetTableView(View).CutToClipboard
  else
    Result := inherited ContainerProcessKeyDown(Key, Shift);
end;

procedure TdxSpreadSheetTableViewController.KeyDown(var Key: Word; Shift: TShiftState);
var
  AColumn: Integer;
  ADimensions: TRect;
  AFocusedRowOffset: Integer;
  APosition: Integer;
  ARow: Integer;
  ASelFinish: Integer;
  ASelStart: Integer;
  AShift: TShiftState;
begin
  if (FocusedContainer <> nil) and ContainerProcessKeyDown(Key, Shift) then
    Exit;
  if (ForcedSelectionMode <> smNone) then
    Exit;
  if EditingController.IsEditing then
    Exit;

  case Key of
    VK_ESCAPE:
      SpreadSheet.ClearClipboard;

    VK_ADD:
      if ssCtrl in Shift then
        CheckCtrlCommand('+');

    VK_SUBTRACT:
      if ssCtrl in Shift then
      begin
        SpreadSheet.ClearClipboard;
        CheckCtrlCommand('-');
      end;

    VK_F2:
      if ssShift in Shift then
        View.EditComment
      else
        EditingController.ShowEdit;

    VK_LEFT:
      if ssCtrl in Shift then
        SelectColumn(GetNextVisibleCell(GetNextNonEmptyCell(Rows, FocusedRow, FocusedColumn, False) + 1, adLeft), Shift - [ssCtrl])
      else
        SelectColumn(GetNextVisibleCell(GetSelectionItem(bLeft, Shift), adLeft), Shift);

    VK_UP:
      if ssCtrl in Shift then
        SelectRow(GetNextVisibleCell(GetNextNonEmptyCell(Columns, FocusedColumn, FocusedRow, False) + 1, adUp), Shift - [ssCtrl])
      else
        SelectRow(GetNextVisibleCell(GetSelectionItem(bTop, Shift), adUp), Shift);

    VK_DOWN:
      if ssCtrl in Shift then
        SelectRow(GetNextVisibleCell(GetNextNonEmptyCell(Columns, FocusedColumn, FocusedRow, True) - 1, adDown), Shift - [ssCtrl])
      else
        SelectRow(GetNextVisibleCell(GetSelectionItem(bBottom, Shift), adDown), Shift);

    VK_RIGHT:
      if ssCtrl in Shift then
        SelectColumn(GetNextVisibleCell(GetNextNonEmptyCell(Rows, FocusedRow, FocusedColumn, True) - 1, adRight), Shift - [ssCtrl])
      else
        SelectColumn(GetNextVisibleCell(GetSelectionItem(bRight, Shift), adRight), Shift);

    VK_PRIOR:
      if ssCtrl in Shift then
        SpreadSheet.ActiveSheetIndex := SpreadSheet.ActiveSheetIndex - 1
      else
      begin
        AFocusedRowOffset := GetFocusedRowOffsetFromTopRow(GetSelectionItem(bTop, Shift));
        ViewInfo.Scroll(sbVertical, scPageUp, APosition);
        SelectRow(CalculateNewFocusedRowIndex(AFocusedRowOffset), Shift);
      end;

    VK_NEXT:
      if ssCtrl in Shift then
        SpreadSheet.ActiveSheetIndex := SpreadSheet.ActiveSheetIndex + 1
      else
      begin
        AFocusedRowOffset := GetFocusedRowOffsetFromTopRow(GetSelectionItem(bBottom, Shift));
        ViewInfo.Scroll(sbVertical, scPageDown, APosition);
        SelectRow(CalculateNewFocusedRowIndex(AFocusedRowOffset), Shift);
      end;

    VK_HOME:
      begin
        ARow := View.FrozenRow + 1;
        while (View.Rows[ARow] <> nil) and not View.Rows[ARow].Visible do
          Inc(ARow);
        AColumn := View.FrozenColumn + 1;
        while (View.Columns[AColumn] <> nil) and not View.Columns[AColumn].Visible do
          Inc(AColumn);
        if ssCtrl in Shift then
          SetFocused(ARow, AColumn, Shift - [ssCtrl])
        else
          SelectColumn(AColumn, Shift);
      end;

    VK_END:
      if ssCtrl in Shift then
      begin
        ADimensions := View.Dimensions;
        SetFocused(GetNextVisibleCell(ADimensions.Bottom - 1, adDown), GetNextVisibleCell(ADimensions.Right - 1, adRight), Shift - [ssCtrl])
      end
      else
        if Rows[FocusedRow] = nil then
          SelectColumn(0, Shift)
        else
          SelectColumn(GetNextVisibleCell(Rows[FocusedRow].CellCount - 1, adRight), Shift);

    VK_TAB:
      if ssShift in Shift then
        FocusNextPossibleCell(adLeft, View.Options.Protected)
      else
        FocusNextPossibleCell(adRight, View.Options.Protected);

    VK_INSERT:
      if Shift = [ssCtrl]  then
        CheckCtrlCommand('C')
      else
        if Shift = [ssShift]  then
          CheckCtrlCommand('V');

    VK_BACK:
      if ssAlt in Shift then
      begin
        SpreadSheet.ClearClipboard;
        CheckCtrlCommand('Z');
        Key := 0;
      end;

    VK_RETURN:
      if ssShift in Shift then
        FocusNextPossibleCell(adUp,   View.Options.Protected and (OptionsBehavior.EnterKeyNavigation = eknSkipLockedCells))
      else
        FocusNextPossibleCell(adDown, View.Options.Protected and (OptionsBehavior.EnterKeyNavigation = eknSkipLockedCells));

    VK_DELETE:
      if [ssAlt, ssCtrl, ssShift] * Shift = [ssShift] then
        CheckCtrlCommand('X')
      else
        if [ssAlt, ssShift] * Shift = [] then
        begin
          SpreadSheet.ClearClipboard;
          View.ClearCellValues;
        end;

    VK_F9:
      begin
        SpreadSheet.ClearClipboard;
        SpreadSheet.FormulaController.Calculate;
      end;

    VK_F11:
      if Shift = [ssShift] then
        SpreadSheet.InsertSheet;

    VK_SPACE:
      if [ssShift, ssCtrl] * Shift <> [] then
      begin
        ARow := FocusedRow;
        AColumn := FocusedColumn;
        if ssCtrl in Shift then
        begin
          AShift := [ssShift];
          if (SelectionMode <> smColumns) and (Selection.Count <= 1) then
            AShift := [];
          ASelStart := Selection.GetActiveSelection.Left;
          ASelFinish := Selection.GetActiveSelection.Right;
          if Selection.IsEntireRowSelected(FocusedRow) then
            Selection.SelectAll
          else
            Selection.SelectColumns(ASelStart, ASelFinish, AShift);
        end;
        if ssShift in Shift then
        begin
          AShift := [ssShift];
          if (SelectionMode <> smRows) and (Selection.Count <= 1) then
            AShift := [];
          ASelStart := Selection.GetActiveSelection.Top;
          ASelFinish := Selection.GetActiveSelection.Bottom;
          if Selection.IsEntireColumnSelected(FocusedColumn) then
            Selection.SelectAll
          else
            Selection.SelectRows(ASelStart, ASelFinish, AShift);
        end;
        Selection.SetFocused(ARow, AColumn, [ssShift]);
      end;

  else
    if (Key <> 16) and (Key <> 17) then
    begin
      if [ssCtrl, ssShift, ssAlt] * Shift = [ssCtrl, ssShift] then
        CheckCtrlShiftCommand(Key);
      if [ssCtrl, ssShift, ssAlt] * Shift = [ssCtrl] then
        CheckCtrlCommand(WideChar(Key));
    end;
  end;
  if Key = VK_CONTROL then
    CellHintController.Hide;
end;

procedure TdxSpreadSheetTableViewController.KeyPress(var Key: Char);
begin
  if (Key < #$21) or (ForcedSelectionMode <> smNone) then
    Key := #0
  else
    EditingController.ShowEditByKey(Key);
end;

procedure TdxSpreadSheetTableViewController.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_CONTROL) and (HitTest.HitObject is TdxSpreadSheetCellViewInfo)  then
    CellHintController.Show(TdxSpreadSheetCellViewInfo(HitTest.HitObject));
end;

procedure TdxSpreadSheetTableViewController.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseDown(Button, Shift, X, Y);

  SelectionMode := smNone;

  if HitTest.HitAtGroupingArea or HitTest.HitAtResizeArea or not PtInRect(ViewInfo.Bounds, Point(X, Y)) or
     EditingController.IsEditing and (SpreadSheet.DragAndDropState <> ddsNone) then
  begin
    CanClickOnTheCell := False;
    CellHintController.Hide;
    Exit;
  end;

  if HitTest.HitAtContainer then
    FocusedContainer := HitTest.Container
  else
    if not HitTest.HitAtContainerSelection then
      FocusedContainer := nil;

  if (FocusedContainer <> nil) or HitTest.HitAtSelectionFrame then
    CellHintController.Hide
  else
    DoMouseDownAtCellArea(Button, Shift, X, Y);

  ClickTimer.Enabled := True;
  CanClickOnTheCell := True;
end;

procedure TdxSpreadSheetTableViewController.DoMouseDownAtCellArea(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ARow, AColumn: Integer;
begin
  GetCellFromMouseCursor(ARow, AColumn);

  if (ARow = -1) and (AColumn = -1) then
  begin
    if HitTest.HitAtColumnHeader and (ForcedSelectionMode in [smNone, smCells]) then
      Selection.Add(dxSpreadSheetEntireSheetArea, [], 0, 0);
  end
  else

  if (ARow = -1) or (ForcedSelectionMode = smColumns) then
  begin
    if (Button <> mbRight) or not Selection.IsEntireColumnSelected(AColumn) then
    begin
      SetSelectionMode(FocusedRow, AColumn, Shift, smColumns);
      Selection.SelectColumns(AnchorColumn, AColumn, Shift);
    end;
  end
  else

  if (AColumn = -1) or (ForcedSelectionMode = smRows) then
  begin
    if (Button <> mbRight) or not Selection.IsEntireRowSelected(ARow) then
    begin
      SetSelectionMode(ARow, FocusedColumn, Shift, smRows);
      Selection.SelectRows(AnchorRow, ARow, Shift);
    end;
  end
  else

  if (Button <> mbRight) or not Selection.IsCellSelected(ARow, AColumn) then
    SetSelectionMode(ARow, AColumn, Shift, smCells);
end;

procedure TdxSpreadSheetTableViewController.DoMouseMove(Shift: TShiftState; X, Y: Integer);
var
  AArea: TRect;
  AColumn: Integer;
  ARow: Integer;
begin
  inherited DoMouseMove(Shift, X, Y);

  if (ssLeft in Shift) and (SelectionMode <> smNone) and (Selection.Count > 0) then
  begin
    CellHintController.Hide;

    CheckScrollArea(X, Y);
    GetCellFromMouseCursor(ARow, AColumn);
    dxSpreadSheetValidate(ARow, 0, dxSpreadSheetMaxRowIndex);
    dxSpreadSheetValidate(AColumn, 0, dxSpreadSheetMaxColumnIndex);

    case SelectionMode of
      smRows:
        AArea := cxRectAdjust(Rect(0, ARow, dxSpreadSheetMaxColumnIndex, FocusedRow));
      smColumns:
        AArea := cxRectAdjust(Rect(AColumn, 0, FocusedColumn, dxSpreadSheetMaxRowIndex));
      smCells:
        AArea := MergedCells.ExpandArea(cxRectAdjust(Rect(AColumn, ARow, FocusedColumn, FocusedRow)));
    else
      Exit;
    end;

    if Selection.CanSelectArea(AArea) and DoActiveCellChanging(AColumn, ARow) then
      Selection.Items[Selection.Count - 1].Rect := AArea;
  end
  else
    if HitTest.HitAtCell or HitTest.HitAtContainer then
      CellHintController.Show(HitTest.HitObject as TdxSpreadSheetCellViewInfo)
    else
      CellHintController.Hide;
end;

procedure TdxSpreadSheetTableViewController.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ARow, AColumn: Integer;
  AHyperlink: TdxSpreadSheetHyperlink;
begin
  if HitTest.HitAtContainer then
    AHyperlink := HitTest.Container.Hyperlink
  else
  begin
    GetCellFromMouseCursor(ARow, AColumn);
    AHyperlink := View.Hyperlinks.FindItem(ARow, AColumn);
  end;
  if (Button = mbLeft) and (AHyperlink <> nil) and CanExecuteHyperlink then
  begin
    AHyperlink.Execute;
    PostMessage(SpreadSheet.Handle, DXM_CANCELHINT, TdxNativeUInt(View), TdxNativeInt(AHyperlink));
  end;

  inherited DoMouseUp(Button, Shift, X, Y);

  if HitTest.HitAtGroupingArea then
  begin
    if HitTest.HitObject = PressedObject then
      DoMouseUpAtGroupingArea(Button, Shift, X, Y);
  end;

  SelectionMode := smNone;
  ScrollingTimer.Enabled := False;
  CanClickOnTheCell := True;
  ClickTimer.Enabled := False;
end;

procedure TdxSpreadSheetTableViewController.DoMouseUpAtGroupingArea(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if HitTest.HitAtGroup then
    TdxSpreadSheetTableViewGroupViewInfo(HitTest.HitObject).Group.ToggleExpanded
  else
    if HitTest.HitAtExpandButton then
      TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo(HitTest.HitObject).Items.Groups.ExpandToLevel(HitTest.HitObjectData - 1);
end;

function TdxSpreadSheetTableViewController.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; const P: TPoint): Boolean;
begin
  Result := True;
  if ssCtrl in Shift then
  begin
    View.HideEdit(True);
    View.Options.ZoomFactor := View.Options.ZoomFactor + Sign(WheelDelta) * 15
  end
  else
  begin
    SpreadSheet.ShowTouchScrollUI(SpreadSheet, True);
    try
      WheelDelta := Max(Abs(WheelDelta) div 2, 1) * Sign(WheelDelta);
      ViewInfo.FirstScrollableRow := Rows.GetItemIndexFromDistance(View.ViewInfo.FirstScrollableRow, -WheelDelta);
      ViewInfo.Validate;
    finally
      SpreadSheet.HideTouchScrollUI(SpreadSheet);
    end;
  end;
  EditingController.UpdateEditPosition;
end;

function TdxSpreadSheetTableViewController.Redo(const ARedoCount: Integer = 1): Boolean;
begin
  Result := EditingController.IsEditing;
  if Result then
    EditingController.Redo
  else
    Result := inherited Redo(ARedoCount);
end;

function TdxSpreadSheetTableViewController.Undo(const AUndoCount: Integer = 1): Boolean;
begin
  Result := EditingController.IsEditing;
  if Result then
    EditingController.Undo
  else
    Result := inherited Undo(AUndoCount);
end;

procedure TdxSpreadSheetTableViewController.ScrollingTimerHandler(Sender: TObject);
var
  AAccepted: Boolean;
  AScrollPos: Integer;
  P: TPoint;
  R: TRect;
begin
  AScrollPos := 0;
  P := SpreadSheet.GetMouseCursorClientPos;
  R := cxRectInflate(ViewInfo.ScrollableArea, -dxSpreadSheetScrollAreaWidth);
  if (P.Y < R.Top) and not InRange(FocusedRow, 0, View.FrozenRow) and (SelectionMode <> smColumns) then
    ViewInfo.DoScroll(sbVertical, scLineUp, AScrollPos);
  if (P.X < R.Left) and not InRange(FocusedColumn, 0, View.FrozenColumn) and (SelectionMode <> smRows) then
    ViewInfo.DoScroll(sbHorizontal, scLineUp, AScrollPos);
  if (P.Y > R.Bottom) and (SelectionMode <> smColumns) then
    ViewInfo.DoScroll(sbVertical, scLineDown, AScrollPos);
  if (P.X > R.Right) and  (SelectionMode <> smRows) then
    ViewInfo.DoScroll(sbHorizontal, scLineDown, AScrollPos);
  if ViewInfo.IsDirty then
  begin
    ViewInfo.Validate;
    MouseMove(KeyboardStateToShiftState, P.X, P.Y);
  end;
  if SpreadSheet.DragAndDropState = ddsInProcess then
  begin
    AAccepted := False;
    SpreadSheet.DragAndDrop(P, AAccepted);
  end;
end;

procedure TdxSpreadSheetTableViewController.SelectCellsWithComments;
var
  AColumn: Integer;
  AKey: Int64;
  AMinKey: Int64;
  ARow: Integer;
begin
  if View.Containers.FComments.Count > 0 then
  begin
    View.BeginUpdate;
    try
      Selection.Clear;
      FocusedContainer := nil;
      AMinKey := MaxInt64;
      for AKey in View.Containers.FComments.Keys do
      begin
        TdxSpreadSheetCellHelper.DecodeRef(AKey, ARow, AColumn);
        Selection.SelectCell(ARow, AColumn, [ssCtrl]);
        AMinKey := Min(AMinKey, AKey);
      end;
      TdxSpreadSheetCellHelper.DecodeRef(AMinKey, ARow, AColumn);
      SetFocused(ARow, AColumn, [ssCtrl]);
    finally
      View.EndUpdate;
      View.SelectionChanged;
    end;
  end;
end;

procedure TdxSpreadSheetTableViewController.SelectColumn(AColumn: Integer; AShift: TShiftState);
var
  R: TcxRect;
  R1: TRect;
begin
  dxSpreadSheetValidate(AColumn, 0, dxSpreadSheetMaxColumnIndex);
  if (Selection.Count = 0) or not (ssShift in AShift)  then
    SetFocused(FocusedRow, AColumn, AShift)
  else
  begin
    R := Selection[Selection.Count - 1];
    R1 := cxRectAdjust(Rect(FocusedColumn, R.Top, AColumn, R.Bottom));

    if dxSpreadSheetIsEntireRowOrColumn(R1) then
      R.Rect := R1
    else
      if DoActiveCellChanging(AColumn, FocusedRow) then
        R.Rect := MergedCells.ExpandArea(R1);

    View.MakeVisibleColumn(IfThen(AColumn > FocusedColumn, R.Right, R.Left));
  end;
end;

procedure TdxSpreadSheetTableViewController.SelectRow(ARow: Integer; AShift: TShiftState);
var
  R: TcxRect;
  R1: TRect;
begin
  dxSpreadSheetValidate(ARow, 0, dxSpreadSheetMaxRowIndex);
  if (Selection.Count = 0) or not (ssShift in AShift) then
    SetFocused(ARow, FocusedColumn, AShift)
  else
  begin
    R := Selection[Selection.Count - 1];
    R1 := cxRectAdjust(Rect(R.Left, FocusedRow, R.Right, ARow));

    if dxSpreadSheetIsEntireRowOrColumn(R1) then
      R.Rect := R1
    else
      if DoActiveCellChanging(FocusedColumn, ARow) then
        R.Rect := MergedCells.ExpandArea(R1);

    View.MakeVisibleRow(IfThen(ARow > FocusedRow, R.Bottom, R.Top));
  end;
end;

procedure TdxSpreadSheetTableViewController.SetFocused(ARow, AColumn: Integer; AShift: TShiftState);
var
  R: TcxRect;
begin
  if DoActiveCellChanging(AColumn, ARow) then
  begin
    if Selection.Count > 0 then
      R := Selection[Selection.Count - 1]
    else
      R := nil;

    if (R = nil) or not (ssShift in AShift) then
      Selection.SetFocused(ARow, AColumn, AShift)
    else
    begin
      if R.Right >= dxSpreadSheetMaxColumnIndex then
        R.Rect := cxRectAdjust(Rect(0, ARow, dxSpreadSheetMaxColumnIndex, FocusedRow))
      else
        if R.Bottom >= dxSpreadSheetMaxRowIndex then
          R.Rect := cxRectAdjust(Rect(AColumn, 0, FocusedColumn, dxSpreadSheetMaxRowIndex))
        else
          R.Rect := MergedCells.ExpandArea(cxRectAdjust(Rect(AColumn, ARow, FocusedColumn, FocusedRow)));

      View.MakeVisible(ARow, AColumn);
    end;
  end;
end;

procedure TdxSpreadSheetTableViewController.SetForcedSelectionMode(const Value: TdxSpreadSheetTableViewSelectionMode);
begin
  FForcedSelectionMode := Value;
end;

procedure TdxSpreadSheetTableViewController.SetSelectionMode(
  ARow, AColumn: Integer; AShift: TShiftState; AMode: TdxSpreadSheetTableViewSelectionMode);
begin
  SelectionMode := AMode;
  case AMode of
    smRows:
      if ssShift in AShift then
        AnchorRow := FocusedRow
      else
        AnchorRow := ARow;

    smColumns:
      if ssShift in AShift then
        AnchorColumn := FocusedColumn
      else
        AnchorColumn := AColumn;
  else
    SetFocused(ARow, AColumn, AShift);
  end;
end;

function TdxSpreadSheetTableViewController.GetColumns: TdxSpreadSheetTableColumns;
begin
  Result := View.Columns;
end;

function TdxSpreadSheetTableViewController.GetFocusedColumn: Integer;
begin
  Result := Selection.FocusedColumn;
end;

function TdxSpreadSheetTableViewController.GetFocusedHyperlink: TdxSpreadSheetHyperlink;
begin
  if FocusedContainer <> nil then
    Result := FocusedContainer.Hyperlink
  else
    Result := View.Hyperlinks.FindItem(FocusedRow, FocusedColumn);
end;

function TdxSpreadSheetTableViewController.GetFocusedRow: Integer;
begin
  Result := Selection.FocusedRow;
end;

function TdxSpreadSheetTableViewController.GetFocusedRowOffsetFromTopRow(ARow: Integer): Integer;
var
  ACount: Integer;
begin
  Result := ViewInfo.FirstScrollableRow;
  dxSpreadSheetValidate(ARow, ViewInfo.FirstScrollableRow, View.BottomRow);
  ACount := 0;
  while Result < ARow do
  begin
    if Rows.GetItemVisible(Result) then
      Inc(ACount);
    Inc(Result);
  end;
  Result := ACount;
end;

function TdxSpreadSheetTableViewController.GetMergedCells: TdxSpreadSheetMergedCellList;
begin
  Result := View.MergedCells;
end;

function TdxSpreadSheetTableViewController.GetOptionsBehavior: TdxSpreadSheetOptionsBehavior;
begin
  Result := View.SpreadSheet.OptionsBehavior;
end;

function TdxSpreadSheetTableViewController.GetRows: TdxSpreadSheetTableRows;
begin
  Result := View.Rows;
end;

function TdxSpreadSheetTableViewController.GetSelection: TdxSpreadSheetTableViewSelection;
begin
  Result := View.Selection;
end;

function TdxSpreadSheetTableViewController.GetTableItemsForNavigation(ADirection: TcxArrowDirection): TdxSpreadSheetTableItems;
begin
  if ADirection in [adLeft, adRight] then
    Result := Columns
  else
    Result := Rows;
end;

function TdxSpreadSheetTableViewController.GetView: TdxSpreadSheetTableView;
begin
  Result := TdxSpreadSheetTableView(Owner);
end;

function TdxSpreadSheetTableViewController.GetViewHitTest: TdxSpreadSheetTableViewHitTest;
begin
  Result := View.HitTest;
end;

function TdxSpreadSheetTableViewController.GetViewInfo: TdxSpreadSheetTableViewInfo;
begin
  Result := View.ViewInfo;
end;

procedure TdxSpreadSheetTableViewController.SetSelectionModeField(AMode: TdxSpreadSheetTableViewSelectionMode);
begin
  if FSelectionMode <> AMode then
  begin
    FSelectionMode := AMode;
    SpreadSheet.Listeners.NotifySelectionModeChanged(View, AMode);
  end;
end;

{ TdxSpreadSheetConditionalFormatting }

constructor TdxSpreadSheetConditionalFormatting.Create(AView: TdxSpreadSheetTableView);
begin
  inherited Create(AView);
  FView := AView;
end;

procedure TdxSpreadSheetConditionalFormatting.Add(
  const AArea: string; ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule);
var
  AAreaAsRect: TRect;
begin
  AAreaAsRect := dxStringToReferenceArea(AArea);
  if not dxSpreadSheetIsValidArea(AAreaAsRect) then
    raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInvalidCellsReference), [AArea]);
  Add(AAreaAsRect, ARuleClass, ARule);
end;

procedure TdxSpreadSheetConditionalFormatting.BeginEditing;
begin
  History.BeginAction(TdxSpreadSheetHistoryChangeConditionalFormattingAction);
  inherited BeginEditing;
end;

procedure TdxSpreadSheetConditionalFormatting.EndEditing(ACancel: Boolean);
begin
  inherited EndEditing(ACancel);
  History.EndAction(ACancel);
end;

procedure TdxSpreadSheetConditionalFormatting.DoChanged;
begin
  inherited DoChanged;
  FView.AddChanges([sscLayout, sscStyle]);
end;

procedure TdxSpreadSheetConditionalFormatting.DoRuleAdded(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited DoRuleAdded(ARule);
  if History.CanAddCommand then
    History.AddCommand(TdxSpreadSheetHistoryCreateConditionalFormattingRuleCommand.Create(ARule));
end;

procedure TdxSpreadSheetConditionalFormatting.DoRuleChanging(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited DoRuleChanging(ARule);
  if History.CanAddCommand and (ARule.Index >= 0) then
    History.AddCommand(TdxSpreadSheetHistoryChangeConditionalFormattingRuleCommand.Create(ARule));
end;

procedure TdxSpreadSheetConditionalFormatting.DoRuleIndexChanged(AOldIndex, ANewIndex: Integer);
begin
  inherited DoRuleIndexChanged(AOldIndex, ANewIndex);
  if History.CanAddCommand then
    History.AddCommand(TdxSpreadSheetHistoryChangeConditionalFormattingRuleIndexCommand.Create(AOldIndex, ANewIndex));
end;

procedure TdxSpreadSheetConditionalFormatting.DoRuleDeleted(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  if History.CanAddCommand then
    History.AddCommand(TdxSpreadSheetHistoryDeleteConditionalFormattingRuleCommand.Create(ARule));
  inherited DoRuleDeleted(ARule);
end;

function TdxSpreadSheetConditionalFormatting.GetHistory: TdxSpreadSheetHistory;
begin
  Result := FView.History;
end;

{ TdxSpreadSheetTableView }

constructor TdxSpreadSheetTableView.Create(AOwner: TdxCustomSpreadSheet);
begin
  inherited Create(AOwner);
  FFrozenColumn := -1;
  FFrozenRow := -1;
  FGUID := dxGenerateGUID;
end;

destructor TdxSpreadSheetTableView.Destroy;
begin
  HideEdit(False);
  History.Lock;
  try
    inherited Destroy;
  finally
    History.Unlock;
  end;
end;

procedure TdxSpreadSheetTableView.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetTableView then
  begin
    ViewInfo.FirstScrollableColumn := 0;
    ViewInfo.FirstScrollableRow := 0;
    FFrozenColumn := TdxSpreadSheetTableView(Source).FFrozenColumn;
    FFrozenRow := TdxSpreadSheetTableView(Source).FFrozenRow;
    LeftColumn := TdxSpreadSheetTableView(Source).LeftColumn;
    TopRow := TdxSpreadSheetTableView(Source).TopRow;
    Options := TdxSpreadSheetTableView(Source).Options;
    OptionsPrint := TdxSpreadSheetTableView(Source).OptionsPrint;
  end;
  inherited Assign(Source);
end;

procedure TdxSpreadSheetTableView.CellAtPoint(const P: TPoint; out ARow, AColumn: Integer;
  AReturnNearestCell: Boolean = False; AVisibleAreaOnly: Boolean = True);
begin
  ViewInfo.GetCellAtPoint(P, ARow, AColumn, AReturnNearestCell, AVisibleAreaOnly);
end;

procedure TdxSpreadSheetTableView.CellAtPoint(const P: TPoint; out ACell: TdxSpreadSheetCell;
  AReturnNearestCell: Boolean = False; AVisibleAreaOnly: Boolean = True);
var
  ARow, AColumn: Integer;
begin
  CellAtPoint(P, ARow, AColumn, AReturnNearestCell, AVisibleAreaOnly);
  if dxSpreadSheetIsValidCellReference(ARow, AColumn) then
    ACell := Cells[ARow, AColumn];
end;

function TdxSpreadSheetTableView.CreateCell(const ACellReference: string): TdxSpreadSheetCell;
var
  ARow, AColumn: Integer;
begin
  StringToCellReference(ACellReference, ARow, AColumn);
  Result := CreateCell(ARow, AColumn);
end;

function TdxSpreadSheetTableView.CreateCell(const ARow, AColumn: Integer): TdxSpreadSheetCell;
begin
  if History.CanAddCommand and (Cells[ARow, AColumn] = nil) then
    History.AddCommand(TdxSpreadSheetHistoryCreateCellCommand.Create(Self, ARow, AColumn));
  Result := TdxSpreadSheetTableRow(Rows.ItemNeeded(ARow)).CreateCell(AColumn);
end;

function TdxSpreadSheetTableView.CanClearCells: Boolean;
begin
  Result := CanClearCells(Selection.Area);
end;

procedure TdxSpreadSheetTableView.GetCellValue(ARow, AColumn: Integer;
  var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  TdxSpreadSheetCustomFormulaControllerAccess(FormulaController).GetCellValue(Self, ARow, AColumn, AValue, AErrorCode);
end;

function TdxSpreadSheetTableView.CanClearCells(const AArea: TRect): Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.Editing and
    (not Options.&Protected or (GetLockedStateOfCellsInArea(AArea) <> cbChecked));
end;

function TdxSpreadSheetTableView.CanClearCells(const AArea: string): Boolean;
begin
  Result := CanClearCells(StringToAreaReference(AArea));
end;

procedure TdxSpreadSheetTableView.ClearCells(const AArea: TRect; AClearValues: Boolean = True; AClearFormats: Boolean = True);
var
  AOptions: TdxSpreadSheetTableViewClearCellsOptions;
begin
  AOptions := [];
  if AClearFormats then
    Include(AOptions, ccoStyles);
  if AClearValues then
  begin
    Include(AOptions, ccoValues);
    Include(AOptions, ccoHyperlinks);
  end;
  ClearCells(AArea, AOptions);
end;

procedure TdxSpreadSheetTableView.ClearCells(const AArea: TRect; AOptions: TdxSpreadSheetTableViewClearCellsOptions);
begin
  if not (CanClearCells(AArea) and (AOptions <> [])) then
    Exit;

  BeginUpdate;
  try
    if not CanModifyDataInArrayFormulaArea(AArea, cmmClear) then
      raise EdxSpreadSheetCannotChangePartOfArrayError.Create(cxGetResourceString(@sdxErrorCannotChangePartOfArray));

    if ccoHyperlinks in AOptions then
      TdxSpreadSheetHyperlinksAccess(Hyperlinks).DeleteCells(AArea);

    if ccoComments in AOptions then
      DeleteComments(AArea);

    if [ccoFreeCellInstances, ccoValues, ccoStyles] * AOptions <> [] then
    begin
      Rows.ForEachCell(AArea,
        procedure (ACell: TdxDynamicListItem)
        begin
          if not (Options.Protected and (csLocked in TdxSpreadSheetCell(ACell).StyleHandle.States)) then
          begin
            if ccoStyles in AOptions then
            begin
              TdxSpreadSheetCell(ACell).StyleHandle := CellStyles.DefaultStyle;
              if Options.Protected then
                TdxSpreadSheetCell(ACell).Style.Locked := False;
            end;
            if ccoValues in AOptions then
              TdxSpreadSheetCell(ACell).IsEmpty := True;
            if ccoFreeCellInstances in AOptions then
              ACell.Free;
          end;
        end);
    end;
    Pack;
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableView.ClearCells(const AArea: string; AClearValues, AClearFormats: Boolean);
begin
  ClearCells(StringToAreaReference(AArea), AClearValues, AClearFormats);
end;

procedure TdxSpreadSheetTableView.ClearCells(const AArea: string; AOptions: TdxSpreadSheetTableViewClearCellsOptions);
begin
  ClearCells(StringToAreaReference(AArea), AOptions);
end;

function TdxSpreadSheetTableView.CanClearSelectedCells : Boolean;
var
  I: Integer;
begin
  for I := 0 to Selection.Count - 1 do
  begin
    if not CanModifyDataInArrayFormulaArea(Selection.Items[I].Rect, cmmClear) then
      Exit(False);
  end;
  Result := True;
end;

function TdxSpreadSheetTableView.CanDeleteCore: Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.Editing and SpreadSheet.OptionsBehavior.Deleting and
    (OptionsProtection.ActualAllowDeleteColumns or OptionsProtection.ActualAllowDeleteRows);
end;

function TdxSpreadSheetTableView.CanInsertCellsCore: Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.Editing and SpreadSheet.OptionsBehavior.Inserting and
    (OptionsProtection.ActualAllowInsertColumns or OptionsProtection.ActualAllowInsertRows);
end;

function TdxSpreadSheetTableView.CanModifyDataInArrayFormulaArea(AArea: TRect; AMode: TdxSpreadSheetCellsModificationMode): Boolean;
var
  AInnerResult: Boolean;
begin
  if (AArea.Top = AArea.Bottom) and (AArea.Left = AArea.Right) and (AMode = cmmReplace) then
    Exit(True);

  AInnerResult := True;
  FormulaController.EnumArrayFormulas(Self,
    function (AFormula: TdxSpreadSheetCustomFormula): Boolean
    var
      AArrayFormulaArea: TRect;
      AIntersection: TRect;
    begin
      AArrayFormulaArea := AFormula.ArrayFormulaArea;
      AInnerResult := not dxSpreadSheetIntersects(AArea, AArrayFormulaArea, AIntersection) or cxRectIsEqual(AIntersection, AArrayFormulaArea);
      Result := AInnerResult;
    end);
  Result := AInnerResult;
end;

procedure TdxSpreadSheetTableView.ClearCells(const AArea, AExcludedArea: TRect; AOptions: TdxSpreadSheetTableViewClearCellsOptions);

  procedure InternalClear(const R: TRect);
  begin
    if dxSpreadSheetIsValidArea(R) then
      ClearCells(R, AOptions);
  end;

var
  R: TRect;
begin
  if dxSpreadSheetIntersects(AArea, AExcludedArea, R) then
  begin
    BeginUpdate;
    try
      InternalClear(cxRect(AArea.Left, AArea.Top, R.Left - 1, R.Top - 1));
      InternalClear(cxRect(R.Left, AArea.Top, R.Right, R.Top - 1));
      InternalClear(cxRect(R.Right + 1, AArea.Top, AArea.Right, R.Top - 1));

      InternalClear(cxRect(AArea.Left, R.Top, R.Left - 1, R.Bottom));
      InternalClear(cxRect(R.Right + 1, R.Top, AArea.Right, R.Bottom));

      InternalClear(cxRect(AArea.Left, R.Bottom + 1, R.Left - 1, AArea.Bottom));
      InternalClear(cxRect(R.Left, R.Bottom + 1, R.Right, AArea.Bottom));
      InternalClear(cxRect(R.Right + 1, R.Bottom + 1, AArea.Right, AArea.Bottom));
    finally
      EndUpdate;
    end;
  end
  else
    ClearCells(AArea, AOptions);
end;

procedure TdxSpreadSheetTableView.ClearCellValues;
var
  I: Integer;
begin
  if not CanClearSelectedCells then
    raise EdxSpreadSheetCannotChangePartOfArrayError.Create(cxGetResourceString(@sdxErrorCannotChangePartOfArray));

  BeginUpdate;
  try
    History.BeginAction(TdxSpreadSheetHistoryClearCellsAction);
    try
      for I := 0 to Selection.Count - 1 do
        ClearCells(Selection.Items[I].Rect, True, False);
    finally
      History.EndAction;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableView.DeleteAllCells;
begin
  if (Rows.First = nil) or not CanDelete then
    Exit;

  History.BeginAction(TdxSpreadSheetHistoryDeleteCellsAction);
  try
    BeginUpdate;
    try
      Rows.ForEach(
        procedure(AItem: TdxDynamicListItem)
        begin
          TdxSpreadSheetTableRow(AItem).RowCells.Clear;
        end);

      MergedCells.Clear;
      ConditionalFormatting.Clear;
      Columns.Groups.DeleteAll;
      Rows.Groups.DeleteAll;
    finally
      EndUpdate;
    end;
  finally
    History.EndAction;
  end;
end;

procedure TdxSpreadSheetTableView.DeleteCells;
var
  AArea: TRect;
  AModification: TdxSpreadSheetCellsModification;
begin
  if CanDelete then
  begin
    if Selection.Count <> 1 then
      raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidSelection));

    AArea := Selection.Area;
    if GetCellsModification(AArea, True, AModification) then
      DeleteCells(AArea, AModification);
  end;
end;

procedure TdxSpreadSheetTableView.DeleteCells(const AArea: TRect; AModification: TdxSpreadSheetCellsModification;
  AMergedCellsConflictResolution: TdxSpreadSheetTableViewMergedCellsConflictResolution = mccrShowConfirmation);
var
  AHelper: TdxSpreadSheetTableViewDeleteCellsHelper;
begin
  if CanDelete(AModification) then
  begin
    if not CanModifyDataInArrayFormulaArea(GetFactuallyModifiableArea(AArea, AModification), cmmDelete) then
      raise EdxSpreadSheetCannotChangePartOfArrayError.Create(cxGetResourceString(@sdxErrorCannotChangePartOfArray));

    AHelper := TdxSpreadSheetTableViewDeleteCellsHelper.Create(Self);
    try
      AHelper.MergedCellsConflictResolution := AMergedCellsConflictResolution;
      AHelper.Modification := AModification;
      AHelper.Process(AArea);
    finally
      AHelper.Free;
    end;
  end;
end;

procedure TdxSpreadSheetTableView.DeleteCells(const AArea: string; AModification: TdxSpreadSheetCellsModification;
  AMergedCellsConflictResolution: TdxSpreadSheetTableViewMergedCellsConflictResolution = mccrShowConfirmation);
begin
  DeleteCells(StringToAreaReference(AArea), AModification, AMergedCellsConflictResolution);
end;

procedure TdxSpreadSheetTableView.DeleteColumns(AStartColumn: Integer; ACount: Integer);
begin
  DeleteCells(cxRect(AStartColumn, 0, AStartColumn + ACount - 1, 0), cmShiftColumns, mccrRaiseException);
end;

procedure TdxSpreadSheetTableView.DeleteRows(AStartRow: Integer; ACount: Integer);
begin
  DeleteCells(cxRect(0, AStartRow, 0, AStartRow + ACount - 1), cmShiftRows, mccrRaiseException);
end;

function TdxSpreadSheetTableView.CanFillData: Boolean;
begin
  Result := (Selection.Count = 1) and SpreadSheet.OptionsBehavior.Editing;
end;

procedure TdxSpreadSheetTableView.FillData(ADirection: TcxDirection; ACount: Integer);
begin
  if Selection.Count <> 1 then
    raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidSelection));
  FillData(Selection.Area, ADirection, ACount);
end;

procedure TdxSpreadSheetTableView.FillData(const ASourceArea, ATargetArea: string);
begin
  FillData(StringToAreaReference(ASourceArea), StringToAreaReference(ATargetArea));
end;

procedure TdxSpreadSheetTableView.FillData(const ASourceArea, ATargetArea: TRect);

  function GetCount(ADirection: TcxDirection): Integer;
  begin
    Result := 0;
    case ADirection of
      dirUp:
        Result := ASourceArea.Top - ATargetArea.Top;
      dirLeft:
        Result := ASourceArea.Left - ATargetArea.Left;
      dirRight:
        Result := ATargetArea.Right - ASourceArea.Right;
      dirDown:
        Result := ATargetArea.Bottom - ASourceArea.Bottom;
    end;
  end;

var
  ADirection: TcxDirection;
begin
  BeginUpdate;
  try
    ADirection := TdxSpreadSheetTableViewAutoFillDragAndDropObject.CalculateDirection(ASourceArea, ATargetArea);
    if ADirection = dirNone then
    begin
      History.BeginAction(TdxSpreadSheetHistoryClearCellsAction);
      try
        ClearCells(ASourceArea, ATargetArea, [ccoValues])
      finally
        History.EndAction;
      end;
    end
    else
      FillData(ASourceArea, ADirection, GetCount(ADirection));
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableView.FillData(const ASourceArea: string; ADirection: TcxDirection; ACount: Integer);
begin
  FillData(StringToAreaReference(ASourceArea), ADirection, ACount);
end;

procedure TdxSpreadSheetTableView.FillData(const ASourceArea: TRect; ADirection: TcxDirection; ACount: Integer);
var
  AHelper: TdxSpreadSheetTableViewAutoFillingHelper;
begin
  if CanFillData then
  begin
    AHelper := TdxSpreadSheetTableViewAutoFillingHelper.Create(Self);
    try
      AHelper.Fill(ASourceArea, ADirection, ACount);
    finally
      AHelper.Free;
    end;
  end;
end;

procedure TdxSpreadSheetTableView.AddArrayFormula(AFormula: TdxSpreadSheetFormula; const AText: string; const AArea: TRect);
var
  ACell: TdxSpreadSheetCell;
  ADestArea: TRect;
  ADestSize: TSize;
  AExistingArea: TRect;
  AExistingFormula: TdxSpreadSheetCustomFormula;
begin
  if not CanModifyDataInArrayFormulaArea(AArea, cmmReplace) then
    raise EdxSpreadSheetCannotChangePartOfArrayError.Create(cxGetResourceString(@sdxErrorCannotChangePartOfArray));

  BeginUpdate;
  try
    ADestArea := AArea;
    if SpreadSheet.FormulaController.IsPartOfArrayFormula(Self, AArea.Top, AArea.Left, @AExistingFormula) <> afpNone then
    begin
      AExistingArea := AExistingFormula.ArrayFormulaArea;
      if (AExistingArea.Right > ADestArea.Right) or (AExistingArea.Bottom > ADestArea.Bottom) then
        ADestArea := AExistingArea;
    end;
    ADestSize := dxSpreadSheetAreaSize(ADestArea);

    if AFormula <> nil then
      AFormula.SetArrayFormulaSize(ADestSize)
    else
    begin
      ACell := CreateCell(ADestArea.Top, ADestArea.Left);
      if ACell.IsFormula then
        ACell.AsFormula.Free;
      ACell.SetTextCore(AText, True);
      if ACell.IsFormula then
        ACell.AsFormula.SetArrayFormulaSize(ADestSize)
      else
        PopulateAreaByValue(AText, ADestArea);
      AFormula := ACell.AsFormula;
    end;

    if AFormula <> nil then
      FormulaController.AddArrayFormula(AFormula);
  finally
    EndUpdate;
  end;
end;

function TdxSpreadSheetTableView.StringToAreaReference(const S: string): TRect;
begin
  Result := dxStringToReferenceArea(S);
  if not dxSpreadSheetIsValidArea(Result) then
    raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInvalidCellsReference), [S]);
end;

function TdxSpreadSheetTableView.StringToColumnIndex(const S: string): Integer;
begin
  Result := TdxSpreadSheetColumnHelper.IndexByName(S);
  if not InRange(Result, 0, dxSpreadSheetMaxColumnIndex) then
    raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInvalidColumnIndex), [S]);
end;

procedure TdxSpreadSheetTableView.StringToCellReference(const S: string; out ARow, AColumn: Integer);
begin
  dxStringToReference(S, AColumn, ARow);
  if not dxSpreadSheetIsValidCellReference(ARow, AColumn) then
    raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInvalidCellReference), [S]);
end;

procedure TdxSpreadSheetTableView.AddArrayFormula(AFormula: TdxSpreadSheetFormula; const AArea: TRect);
begin
  AddArrayFormula(AFormula, '', AArea);
end;

procedure TdxSpreadSheetTableView.AddArrayFormula(const AText: string; const AArea: TRect);
begin
  AddArrayFormula(nil, AText, AArea);
end;

procedure TdxSpreadSheetTableView.AddArrayFormula(const AText: string; const AArea: string);
begin
  AddArrayFormula(AText, StringToAreaReference(AArea));
end;

function TdxSpreadSheetTableView.CanModifyDataInArea(const AArea: TRect; const AMode: TdxSpreadSheetCellsModificationMode): Boolean;
begin
  try
    CheckProtection(AMode, AArea);
    Result := True;
  except
    Result := False;
  end;
end;

function TdxSpreadSheetTableView.CanModifyDataInArea(const AArea: string; const AMode: TdxSpreadSheetCellsModificationMode): Boolean;
begin
  Result := CanModifyDataInArea(StringToAreaReference(AArea), AMode);
end;

procedure TdxSpreadSheetTableView.FillCells(ATopDown: Boolean);
var
  AHelper: TdxSpreadSheetTableViewFillCellsHelper;
begin
  AHelper := TdxSpreadSheetTableViewFillCellsHelper.Create(Self);
  try
    AHelper.FillCells(ATopDown);
  finally
    AHelper.Free;
  end;
end;

procedure TdxSpreadSheetTableView.ForEachCell(AProc: TdxSpreadSheetTableViewForEachCellProc);
begin
  ForEachCell(Dimensions, AProc);
end;

procedure TdxSpreadSheetTableView.ForEachCell(const AArea: TRect;
  AProc: TdxSpreadSheetTableViewForEachCellProc; AGoForward: Boolean = True);
begin
  Rows.ForEachCell(AArea,
    procedure(AItem: TdxDynamicListItem)
    begin
      AProc(TdxSpreadSheetCell(AItem));
    end, AGoForward);
end;

procedure TdxSpreadSheetTableView.ForEachCell(const AArea: string;
  AProc: TdxSpreadSheetTableViewForEachCellProc; AGoForward: Boolean = True);
begin
  ForEachCell(StringToAreaReference(AArea), AProc, AGoForward);
end;

procedure TdxSpreadSheetTableView.FreezeColumns(const AColumn: Integer);
begin
  FreezePanes(-1, AColumn);
end;

procedure TdxSpreadSheetTableView.FreezeColumns(const AColumn: string);
begin
  FreezeColumns(StringToColumnIndex(AColumn));
end;

procedure TdxSpreadSheetTableView.FreezePanes(const ACellReference: string);
var
  ARow, AColumn: Integer;
begin
  StringToCellReference(ACellReference, ARow, AColumn);
  FreezePanes(ARow, AColumn);
end;

procedure TdxSpreadSheetTableView.FreezePanes(const ARow, AColumn: Integer);
begin
  BeginUpdate;
  try
    FrozenRow := ARow;
    FrozenColumn := AColumn;
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableView.FreezeRows(const ARow: Integer);
begin
  FreezePanes(ARow, -1);
end;

procedure TdxSpreadSheetTableView.UnfreezePanes;
begin
  FreezePanes(-1, -1);
end;

function TdxSpreadSheetTableView.CanInsert: Boolean;
begin
  Result := CanInsertCellsCore;
  if not OptionsProtection.ActualAllowInsertColumns then
    Result := Result and dxSpreadSheetIsEntireRow(Selection.Area);
  if not OptionsProtection.ActualAllowInsertRows then
    Result := Result and dxSpreadSheetIsEntireColumn(Selection.Area);
end;

function TdxSpreadSheetTableView.CanInsert(AModification: TdxSpreadSheetCellsModification): Boolean;
begin
  Result := CanInsertCellsCore;
  if not OptionsProtection.ActualAllowInsertColumns then
    Result := Result and (AModification = cmShiftRows);
  if not OptionsProtection.ActualAllowInsertRows then
    Result := Result and (AModification = cmShiftColumns);
end;

function TdxSpreadSheetTableView.CanInsertCells(AArea: TRect; const AModification: TdxSpreadSheetCellsModification): Boolean;
var
  AInnerResult: Boolean;
begin
  AInnerResult := True;
  FormulaController.EnumArrayFormulas(Self,
    function (AFormula: TdxSpreadSheetCustomFormula): Boolean
    var
      AArrayFormulaArea: TRect;
      AIntersecting: TRect;
    begin
      AArrayFormulaArea := AFormula.ArrayFormulaArea;
      case AModification of
        cmShiftRows:
          AInnerResult := (AArea.Top <= AArrayFormulaArea.Top) or (AArea.Top > AArrayFormulaArea.Bottom);
        cmShiftColumns:
          AInnerResult := (AArea.Left <= AArrayFormulaArea.Left) or (AArea.Left > AArrayFormulaArea.Right);
      else
        AInnerResult := not dxSpreadSheetIntersects(AArea, AArrayFormulaArea, AIntersecting) or cxRectIsEqual(AIntersecting, AArrayFormulaArea);
      end;
      Result := AInnerResult;
    end);
  Result := AInnerResult;
end;

function TdxSpreadSheetTableView.CanInsertCells(const AArea: string; const AModification: TdxSpreadSheetCellsModification): Boolean;
begin
  Result := CanInsertCells(StringToAreaReference(AArea), AModification);
end;

procedure TdxSpreadSheetTableView.InsertCells;
var
  AArea: TRect;
  AModification: TdxSpreadSheetCellsModification;
begin
  if CanInsert then
  begin
    if Selection.Count <> 1 then
      raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidSelection));
    AArea := Selection.Area;
    if GetCellsModification(AArea, False, AModification) then
      InsertCells(AArea, AModification);
  end;
end;

procedure TdxSpreadSheetTableView.InsertCells(const AArea: TRect; AModification: TdxSpreadSheetCellsModification;
  AMergedCellsConflictResolution: TdxSpreadSheetTableViewMergedCellsConflictResolution = mccrShowConfirmation);
var
  AHelper: TdxSpreadSheetTableViewInsertCellsHelper;
begin
  if CanInsert(AModification) then
  begin
    if not CanInsertCells(GetFactuallyModifiableArea(AArea, AModification), AModification) then
      raise EdxSpreadSheetCannotChangePartOfArrayError.Create(cxGetResourceString(@sdxErrorCannotChangePartOfArray));

    AHelper := TdxSpreadSheetTableViewInsertCellsHelper.Create(Self);
    try
      AHelper.MergedCellsConflictResolution := AMergedCellsConflictResolution;
      AHelper.Modification := AModification;
      AHelper.Process(AArea);
    finally
      AHelper.Free;
    end;
  end;
end;

procedure TdxSpreadSheetTableView.InsertCells(const AArea: string; AModification: TdxSpreadSheetCellsModification;
  AMergedCellsConflictResolution: TdxSpreadSheetTableViewMergedCellsConflictResolution = mccrShowConfirmation);
begin
  InsertCells(StringToAreaReference(AArea), AModification, AMergedCellsConflictResolution);
end;

procedure TdxSpreadSheetTableView.InsertColumns(AColumn: Integer; ACount: Integer);
begin
  InsertCells(cxRect(AColumn, 0, AColumn + ACount - 1, 0), cmShiftColumns, mccrRaiseException);
end;

procedure TdxSpreadSheetTableView.InsertColumns(const AColumn: string; ACount: Integer);
begin
  InsertColumns(StringToColumnIndex(AColumn), ACount);
end;

procedure TdxSpreadSheetTableView.InsertRows(ARow: Integer; ACount: Integer);
begin
  InsertCells(cxRect(0, ARow, 0, ARow + ACount - 1), cmShiftRows, mccrRaiseException);
end;

procedure TdxSpreadSheetTableView.InvalidateRect(const R: TRect);
begin
  inherited InvalidateRect(ViewInfo.ContentRectToScreenRect(R));
end;

procedure TdxSpreadSheetTableView.MakeFocusedCellVisible;
begin
  MakeVisible(Selection.FocusedRow, Selection.FocusedColumn);
end;

procedure TdxSpreadSheetTableView.MakeVisible(AContainer: TdxSpreadSheetContainer);
var
  ACellRef: TPoint;
  ARect: TRect;
begin
  if (AContainer <> nil) and (AContainer.Parent = Self) and AContainer.Visible then
  begin
    ARect := AContainer.Calculator.CalculateBounds;
    if GetCellAtAbsolutePoint(ARect.BottomRight, ACellRef.Y, ACellRef.X) then
      MakeVisible(ACellRef.Y, ACellRef.X);
    if GetCellAtAbsolutePoint(ARect.TopLeft, ACellRef.Y, ACellRef.X) then
      MakeVisible(ACellRef.Y, ACellRef.X);
  end;
end;

procedure TdxSpreadSheetTableView.MakeVisible(ARow, AColumn: Integer);
begin
  MakeVisible(cxRectBounds(AColumn, ARow, 0, 0));
end;

procedure TdxSpreadSheetTableView.MakeVisible(const AArea: string);
begin
  MakeVisible(StringToAreaReference(AArea));
end;

procedure TdxSpreadSheetTableView.MakeVisibleColumn(const AColumn: string);
begin
  MakeVisibleColumn(StringToColumnIndex(AColumn));
end;

procedure TdxSpreadSheetTableView.MakeVisible(AArea: TRect);
begin
  if Controller.SelectionMode <> smNone then
    Exit;

  dxSpreadSheetValidate(AArea.Left, 0, dxSpreadSheetMaxColumnIndex);
  dxSpreadSheetValidate(AArea.Bottom, 0, dxSpreadSheetMaxRowIndex);
  dxSpreadSheetValidate(AArea.Right, 0, dxSpreadSheetMaxColumnIndex);
  dxSpreadSheetValidate(AArea.Top, 0, dxSpreadSheetMaxRowIndex);

  if InRange(AArea.Top, TopRow, BottomRow) and InRange(AArea.Bottom, TopRow, BottomRow) then
  begin
    if AArea.Top > FrozenRow then
      ViewInfo.FirstScrollableRow := Min(ViewInfo.FirstScrollableRow, AArea.Top);
  end
  else
  begin
    SpreadSheet.ShowTouchScrollUI(SpreadSheet, True);
    if AArea.Bottom > BottomRow then
      ViewInfo.ChangeVisibleAreaBounds(bBottom, AArea.Bottom);
    if AArea.Top < TopRow then
      ViewInfo.ChangeVisibleAreaBounds(bTop, AArea.Top);
  end;

  if InRange(AArea.Left, LeftColumn, RightColumn) and InRange(AArea.Right, LeftColumn, RightColumn) then
  begin
    if AArea.Left > FrozenColumn then
      ViewInfo.FirstScrollableColumn := Min(ViewInfo.FirstScrollableColumn, AArea.Left);
  end
  else
  begin
    SpreadSheet.ShowTouchScrollUI(SpreadSheet, True);
    if AArea.Right > RightColumn then
      ViewInfo.ChangeVisibleAreaBounds(bRight, AArea.Right);
    if AArea.Left < LeftColumn then
      ViewInfo.ChangeVisibleAreaBounds(bLeft, AArea.Left);
  end;
  ViewInfo.Validate;
end;

procedure TdxSpreadSheetTableView.MakeVisibleColumn(const AColumn: Integer);
begin
  MakeVisible(TopRow, AColumn);
end;

procedure TdxSpreadSheetTableView.MakeVisibleRow(const ARow: Integer);
begin
  MakeVisible(ARow, LeftColumn);
end;

function TdxSpreadSheetTableView.CanCopyToClipboard: Boolean;
begin
  Result := not IsEditing;
end;

function TdxSpreadSheetTableView.CanCutToClipboard: Boolean;
begin
  Result := not IsEditing;
  if Result then
  try
    CheckCutToClipboard;
  except
    on EdxSpreadSheetError do
       Result := False
    else
      raise;
  end;
end;

function TdxSpreadSheetTableView.CanPasteFromClipboard: Boolean;
begin
  Result := CanPasteFromClipboard(dxSpreadSheetDefaultPasteOptions);
end;

function TdxSpreadSheetTableView.CanPasteFromClipboard(AOptions: TdxSpreadSheetClipboardPasteOptions): Boolean;
begin
  Result := not IsEditing and Controller.OptionsBehavior.Editing and
    TdxSpreadSheetTableViewClipboardHelper.CanPaste(AOptions);
end;

procedure TdxSpreadSheetTableView.CheckCopySelection;
begin
  if (Controller.FocusedContainer = nil) and (Selection.Count <> 1) then
    raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidSelection));
end;

procedure TdxSpreadSheetTableView.CheckCutToClipboard;
begin
  CheckCopySelection;
  CheckProtection(cmmClear, Selection.Area);
end;

procedure TdxSpreadSheetTableView.CheckPasteSelection(const AClipboardArea: TRect; AMode: TdxSpreadSheetClipboardCopyMode);
var
  I: Integer;
begin
  if AMode = ccmCut then
    CheckCopySelection
  else
    if Selection.Count < 1 then
      raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidSelection));

  for I := 0 to Selection.Count - 1 do
    CheckProtection(cmmReplace, cxRectSetSize(Selection[I].Rect, cxSize(AClipboardArea)));
  if not CheckPasteSelectionSizes(AClipboardArea) then
    raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidPasteArea));
end;

function TdxSpreadSheetTableView.CheckPasteSelectionSizes(const AClipboardArea: TRect): Boolean;

  function CheckBounds(const AArea: TRect; AClipboardAreaWidth, AClipboardAreaHeight: Integer): Boolean;
  begin
    Result :=
      (AArea.Left + AClipboardAreaWidth - 1 < dxSpreadSheetMaxColumnCount) and
      (AArea.Top + AClipboardAreaHeight - 1 < dxSpreadSheetMaxRowCount);
  end;

  function CheckAreaSize(const AArea: TRect; AClipboardAreaWidth, AClipboardAreaHeight: Integer): Boolean;
  var
    AAreaHeight: Integer;
    AAreaWidth: Integer;
  begin
    AAreaWidth := dxSpreadSheetAreaWidth(AArea);
    AAreaHeight := dxSpreadSheetAreaHeight(AArea);
    Result :=
      (AAreaWidth = 1) and (AAreaHeight = 1) or
      (AAreaWidth = 1) and (AAreaHeight mod AClipboardAreaHeight = 0) or
      (AAreaHeight = 1) and (AAreaWidth mod AClipboardAreaWidth = 0) or
      (AAreaWidth mod AClipboardAreaWidth = 0) and (AAreaHeight mod AClipboardAreaHeight = 0);
  end;

var
  AArea: TRect;
  AHeight: Integer;
  AWidth: Integer;
  I: Integer;
begin
  AHeight := dxSpreadSheetAreaHeight(AClipboardArea);
  AWidth := dxSpreadSheetAreaWidth(AClipboardArea);

  if Selection.Count = 1 then
    Exit(CheckBounds(Selection[0].Rect, AWidth, AHeight));

  Result := False;
  for I := 0 to Selection.Count - 1 do
  begin
    AArea := Selection[I].Rect;
    Result := CheckBounds(AArea, AWidth, AHeight) and CheckAreaSize(AArea, AWidth, AHeight);
    if not Result then
      Break;
  end;
end;

procedure TdxSpreadSheetTableView.OptionsProtectionChangeHandler(Sender: TObject);
begin
  if not (sssReading in State) then
    DoOptionsProtectionChanged;
end;

procedure TdxSpreadSheetTableView.CheckProtection(AMode: TdxSpreadSheetCellsModificationMode);
var
  I: Integer;
begin
  for I := 0 to Selection.Count - 1 do
    CheckProtection(AMode, Selection[I].Rect);
end;

procedure TdxSpreadSheetTableView.CheckProtection(AMode: TdxSpreadSheetCellsModificationMode; const AArea: TRect);
begin
  if not SpreadSheet.OptionsBehavior.Editing or Options.&Protected and (GetLockedStateOfCellsInArea(AArea) <> cbUnchecked) then
    raise EdxSpreadSheetProtectionError.Create(cxGetResourceString(@sdxErrorCellProtected));
  if not CanModifyDataInArrayFormulaArea(AArea, AMode) then
    raise EdxSpreadSheetCannotChangePartOfArrayError.Create(cxGetResourceString(@sdxErrorCannotChangePartOfArray));
end;

procedure TdxSpreadSheetTableView.CopyToClipboard;
begin
  if CanCopyToClipboard then
  begin
    BeginUpdate;
    try
      CheckCopySelection;
      SpreadSheet.LockClipboardListener;
      try
        SpreadSheet.IsClipboardListener := True;
        TdxSpreadSheetTableViewClipboardHelper.Copy(Self);
      finally
        SpreadSheet.UnlockClipboardListener;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetTableView.CutToClipboard;
begin
  CheckCutToClipboard;
  BeginUpdate;
  try
    SpreadSheet.LockClipboardListener;
    try
      SpreadSheet.IsClipboardListener := True;
      History.BeginAction(TdxSpreadSheetHistoryCutToClipboardAction);
      try
        TdxSpreadSheetTableViewClipboardHelper.Cut(Self);
      finally
        History.EndAction;
      end;
    finally
      SpreadSheet.UnlockClipboardListener;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableView.PasteFromClipboard;
var
  AClipboardArea: TRect;
  AClipboardCopyMode: TdxSpreadSheetClipboardCopyMode;
  AOptions: TdxSpreadSheetClipboardPasteOptions;
  AViewGUID: string;
begin
  if CanPasteFromClipboard then
  begin
    AOptions := dxSpreadSheetDefaultPasteOptions;
    TdxSpreadSheetTableViewClipboardHelper.GetDataInfoFromClipboard(AViewGUID, AClipboardCopyMode, AClipboardArea);
    if dxSpreadSheetIsEntireColumn(AClipboardArea) then
      Include(AOptions, cpoColumnWidths);
    PasteFromClipboardCore(AOptions, AViewGUID, AClipboardArea, AClipboardCopyMode);
  end;
end;

procedure TdxSpreadSheetTableView.PasteFromClipboard(AOptions: TdxSpreadSheetClipboardPasteOptions);
var
  AClipboardArea: TRect;
  AClipboardCopyMode: TdxSpreadSheetClipboardCopyMode;
  AViewGUID: string;
begin
  if CanPasteFromClipboard then
  begin
    TdxSpreadSheetTableViewClipboardHelper.GetDataInfoFromClipboard(AViewGUID, AClipboardCopyMode, AClipboardArea);
    PasteFromClipboardCore(AOptions, AViewGUID, AClipboardArea, AClipboardCopyMode);
  end;
end;

procedure TdxSpreadSheetTableView.CopyCellDataToStream(const AArea: TRect; AStream: TStream);
begin
  WriteIntegerProc(AStream, SFSpreadSheetVersion);
  TdxSpreadSheetBinaryClipboardFormat.SaveToStream(AStream,
    TdxSpreadSheetBinaryClipboardFormat.Build(AArea, nil, ccmCopy, Self));
end;

procedure TdxSpreadSheetTableView.CopyCellDataToStream(const AArea: string; AStream: TStream);
begin
  CopyCellDataToStream(StringToAreaReference(AArea), AStream);
end;

procedure TdxSpreadSheetTableView.PasteCellDataFromStream(const ADestination: TPoint; AStream: TStream);
const
  SFSpreadSheetOldVersion = $53500001;
var
  AData: IdxSpreadSheetClipboardData;
  AFocusedCell, AScrollPos: TPoint;
  ASelection: TdxRectList;
  AVersion: Integer;
begin
  AVersion := ReadIntegerFunc(AStream);
  if (AVersion = SFSpreadSheetVersion) or (AVersion = SFSpreadSheetOldVersion) then
  begin
    ASelection := TdxRectList.Create;
    dxSpreadSheetStoreSelectionAndScrollPosition(Self, ASelection, AFocusedCell, AScrollPos);
    try
      Selection.Add(cxRect(ADestination, ADestination));
      if AVersion = SFSpreadSheetOldVersion then
        AData := TdxSpreadSheetPasteCellDataFromStreamHelper.Create(AStream)
      else
        AData := TdxSpreadSheetBinaryClipboardFormat.LoadFromStream(AStream);

      if AData <> nil then
        AData.Paste(Self, ADestination, dxSpreadSheetDefaultPasteOptions);
    finally
      dxSpreadSheetRestoreSelectionAndScrollPosition(Self, ASelection, AFocusedCell, AScrollPos);
      FreeAndNil(ASelection);
    end;
  end;
end;

function TdxSpreadSheetTableView.CanMergeSelected: Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.Editing and not Options.Protected;
end;

function TdxSpreadSheetTableView.CanSplitSelected: Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.Editing and not Options.Protected;
end;

procedure TdxSpreadSheetTableView.MergeSelected;
var
  I: Integer;
begin
  if CanMergeSelected then
  begin
    History.BeginAction(TdxSpreadSheetHistoryMergeCellsAction);
    try
      BeginUpdate;
      try
        SplitSelected;
        for I := 0 to Selection.Count - 1 do
          MergedCells.Add(Selection.Items[I].Rect);
      finally
        EndUpdate;
      end;
    finally
      History.EndAction;
    end;
  end;
end;

procedure TdxSpreadSheetTableView.SplitSelected;
var
  I: Integer;
begin
  if CanSplitSelected then
  begin
    BeginUpdate;
    try
      for I := 0 to Selection.Count - 1 do
        MergedCells.DeleteItemsInArea(Selection.Items[I].Rect, False);
    finally
      EndUpdate;
    end;
  end;
end;

function TdxSpreadSheetTableView.CanEditContainers: Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.Editing and OptionsProtection.ActualAllowEditContainers;
end;

function TdxSpreadSheetTableView.CanDeleteHyperlink: Boolean;
begin
  Result := OptionsProtection.ActualAllowEditHyperlinks and (Controller.FocusedHyperlink <> nil);
end;

function TdxSpreadSheetTableView.CanEditHyperlinks: Boolean;

  function IsLocked(ACell: TdxSpreadSheetCell): Boolean;
  begin
    Result := (ACell = nil) or (csLocked in ACell.StyleHandle.States);
  end;

begin
  Result := OptionsProtection.ActualAllowEditHyperlinks and
    (CanEditContainers or (Controller.FocusedContainer = nil)) and not
    (OptionsProtection.&Protected and IsLocked(Cells[Selection.FocusedRow, Selection.FocusedColumn]));
end;

procedure TdxSpreadSheetTableView.DeleteHyperlink;
begin
  if CanDeleteHyperlink then
    Controller.FocusedHyperlink.Free;
end;

procedure TdxSpreadSheetTableView.EditHyperlink;
begin
  if CanEditHyperlinks then
    ShowHyperlinkEditorDialog(Self, Controller.FocusedHyperlink);
end;

procedure TdxSpreadSheetTableView.Protect;
begin
  if not OptionsProtection.Protected then
    ShowProtectSheetDialog(Self);
end;

procedure TdxSpreadSheetTableView.Unprotect;
begin
  SpreadSheet.UnprotectCore(OptionsProtection);
end;

function TdxSpreadSheetTableView.CanEditComment: Boolean;
begin
  Result := CanEditContainers and (
    (Selection.FocusedContainer is TdxSpreadSheetCommentContainer) or
    (Selection.FocusedRow >= 0) and (Selection.FocusedColumn >= 0));
end;

function TdxSpreadSheetTableView.CanDelete: Boolean;
begin
  Result := CanDeleteCore;
  if not OptionsProtection.ActualAllowDeleteColumns then
    Result := Result and dxSpreadSheetIsEntireRow(Selection.Area);
  if not OptionsProtection.ActualAllowDeleteRows then
    Result := Result and dxSpreadSheetIsEntireColumn(Selection.Area);
end;

function TdxSpreadSheetTableView.CanDelete(AModification: TdxSpreadSheetCellsModification): Boolean;
begin
  Result := CanDeleteCore;
  if not OptionsProtection.ActualAllowDeleteColumns then
    Result := Result and (AModification = cmShiftRows);
  if not OptionsProtection.ActualAllowDeleteRows then
    Result := Result and (AModification = cmShiftColumns);
end;

function TdxSpreadSheetTableView.CanDeleteComments: Boolean;
begin
  Result := CanEditContainers and
    ((Selection.FocusedContainer is TdxSpreadSheetCommentContainer) or CanDeleteComments(Selection.Area));
end;

function TdxSpreadSheetTableView.CanDeleteComments(const AArea: string): Boolean;
begin
  Result := CanDeleteComments(StringToAreaReference(AArea));
end;

function TdxSpreadSheetTableView.CanDeleteComments(const AArea: TRect): Boolean;
var
  AValue: TdxSpreadSheetContainer;
begin
  if CanEditContainers then
    for AValue in Containers.FComments.Values do
    begin
      if dxSpreadSheetContains(AArea, TdxSpreadSheetCommentContainer(AValue).Cell) then
        Exit(True);
    end;
  Result := False;
end;

procedure TdxSpreadSheetTableView.EditComment;
var
  ACell: TdxSpreadSheetCell;
  ACellPos: TPoint;
  AContainer: TdxSpreadSheetContainer;
begin
  if CanEditComment then
  begin
    History.BeginAction(TdxSpreadSheetHistoryEditCommentAction);
    try
      BeginUpdate;
      try
        if Selection.FocusedContainer is TdxSpreadSheetCommentContainer then
          ShowContainerCustomizationDialog(Selection.FocusedContainer, -1)
        else
        begin
          ACellPos := MergedCells.CheckCell(Selection.FocusedRow, Selection.FocusedColumn).TopLeft;
          ACell := CreateCell(ACellPos.Y, ACellPos.X);
          if Containers.FindCommentContainer(ACell, AContainer) then
            ShowContainerCustomizationDialog(AContainer, -1)
          else
          begin
            AContainer := Containers.AddCommentContainer(ACell);
            AContainer.Visible := False;
            if not ShowContainerCustomizationDialog(AContainer, -1) then
              AContainer.Free;
          end;
        end;
      finally
        EndUpdate;
      end;
    finally
      History.EndAction;
    end;
  end;
end;

procedure TdxSpreadSheetTableView.DeleteComments;
begin
  if CanEditContainers then
  begin
    if Selection.FocusedContainer is TdxSpreadSheetCommentContainer then
      Selection.FocusedContainer.Free
    else
      DeleteComments(Selection.Area);
  end;
end;

procedure TdxSpreadSheetTableView.DeleteComments(const AArea: string);
begin
  DeleteComments(StringToAreaReference(AArea));
end;

procedure TdxSpreadSheetTableView.DeleteComments(const AArea: TRect);
var
  AContainer: TdxSpreadSheetContainer;
  I: Integer;
begin
  if CanDeleteComments(AArea) then
  begin
    History.BeginAction(TdxSpreadSheetHistoryDeleteCommentsAction);
    try
      BeginUpdate;
      try
        for I := Containers.Count - 1 downto 0 do
        begin
          AContainer := Containers[I];
          if AContainer is TdxSpreadSheetCommentContainer then
          begin
            if dxSpreadSheetContains(AArea, TdxSpreadSheetCommentContainer(AContainer).Cell) then
              AContainer.Free;
          end;
        end;
      finally
        EndUpdate;
      end;
    finally
      History.EndAction;
    end;
  end;
end;

procedure TdxSpreadSheetTableView.HideEdit(Accept: Boolean);
begin
  EditingController.HideEdit(Accept);
end;

procedure TdxSpreadSheetTableView.ShowEdit;
begin
  EditingController.ShowEdit;
end;

procedure TdxSpreadSheetTableView.ShowEditByKey(AKey: Char);
begin
  EditingController.ShowEditByKey(AKey);
end;

procedure TdxSpreadSheetTableView.ShowEditByMouse(X, Y: Integer; AShift: TShiftState);
begin
  EditingController.ShowEditByMouse(X, Y, AShift);
end;

function TdxSpreadSheetTableView.CanSort: Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.Editing and OptionsProtection.ActualAllowSort;
end;

procedure TdxSpreadSheetTableView.SortByColumnValues(const AArea: TRect;
  const ASortOrders: array of TdxSortOrder; const AColumns: array of Integer);
var
  AHelper: TdxSpreadSheetTableViewSortingHelper;
begin
  if CanSort then
  begin
    AHelper := TdxSpreadSheetTableViewSortingHelper.Create(Self);
    try
      AHelper.SortByColumnValues(AArea, ASortOrders, AColumns);
    finally
      AHelper.Free;
    end;
  end;
end;

procedure TdxSpreadSheetTableView.SortByColumnValues(const AArea: string;
  const ASortOrders: array of TdxSortOrder; const AColumns: array of Integer);
begin
  SortByColumnValues(StringToAreaReference(AArea), ASortOrders, AColumns);
end;

procedure TdxSpreadSheetTableView.SortByRowValues(const AArea: string;
  const ASortOrders: array of TdxSortOrder; const ARows: array of Integer);
begin
  SortByRowValues(StringToAreaReference(AArea), ASortOrders, ARows);
end;

procedure TdxSpreadSheetTableView.SortByRowValues(const AArea: TRect;
  const ASortOrders: array of TdxSortOrder; const ARows: array of Integer);
var
  AHelper: TdxSpreadSheetTableViewSortingHelper;
begin
  if CanSort then
  begin
    AHelper := TdxSpreadSheetTableViewSortingHelper.Create(Self);
    try
      AHelper.SortByRowValues(AArea, ASortOrders, ARows);
    finally
      AHelper.Free;
    end;
  end;
end;

function TdxSpreadSheetTableView.GetActiveCell(const ARow, AColumn: Integer): TdxSpreadSheetCell;
var
  AMergedCell: TdxSpreadSheetMergedCell;
begin
  Result := Cells[ARow, AColumn];
  if Result <> nil then
  begin
    AMergedCell := Controller.MergedCells.FindCell(ARow, AColumn);
    if AMergedCell <> nil then
      Result := AMergedCell.ActiveCell;
  end;
end;

function TdxSpreadSheetTableView.GetArrayFormulaArea(const ASenderRowIndex, ASenderColumnIndex: Integer): TRect;
begin
  Result := cxRect(ASenderColumnIndex, ASenderRowIndex, ASenderColumnIndex, ASenderRowIndex);
  if Selection.Count > 0 then
  begin
    if dxSpreadSheetContains(Selection[Selection.Count - 1].Rect, ASenderRowIndex, ASenderColumnIndex) then
      Result := Selection[Selection.Count - 1].Rect;
  end;
end;

procedure TdxSpreadSheetTableView.PopulateAreaByValue(const AValue: Variant; const AArea: TRect);
var
  ARow, AColumn: Integer;
begin
  BeginUpdate;
  try
    for ARow := AArea.Top to AArea.Bottom do
      for AColumn := AArea.Left to AArea.Right do
        CreateCell(ARow, AColumn).AsVariant := AValue;
  finally
    EndUpdate;
  end;
end;

function TdxSpreadSheetTableView.GetCellsModification(const AArea: TRect;
  AIsDeletingMode: Boolean; out AModification: TdxSpreadSheetCellsModification): Boolean;
begin
  Result := True;
  if dxSpreadSheetIsEntireColumn(AArea) then
    AModification := cmShiftColumns
  else
    if dxSpreadSheetIsEntireRow(AArea) then
      AModification := cmShiftRows
    else
      Result := ShowCellsModificationDialog(AModification, AIsDeletingMode, SpreadSheet);
end;

function TdxSpreadSheetTableView.GetCellStyle(ARow, AColumn: Integer; ACell: TdxSpreadSheetCell): TdxSpreadSheetCellStyle;
begin
  if ACell <> nil then
    Exit(ACell.Style);

  Result := Rows.GetItemStyle(ARow);
  if (Result = nil) or Result.IsDefault then
    Result := Columns.GetItemStyle(AColumn);
  if Result = nil then
    Result := SpreadSheet.DefaultCellStyle;
end;

function TdxSpreadSheetTableView.GetCellStyleHandle(ARow, AColumn: Integer; ACell: TdxSpreadSheetCell): TdxSpreadSheetCellStyleHandle;
begin
  if ACell <> nil then
    Exit(ACell.StyleHandle);

  Result := Rows.GetItemStyleHandle(ARow);
  if (Result = nil) or Result.IsDefault then
    Result := Columns.GetItemStyleHandle(AColumn);
  if Result = nil then
    Result := SpreadSheet.CellStyles.DefaultStyle;
end;

function TdxSpreadSheetTableView.GetCellStyleHandle(AMergedCell: TdxSpreadSheetMergedCell): TdxSpreadSheetCellStyleHandle;
begin
  Result := GetCellStyleHandle(AMergedCell.Area.Top, AMergedCell.Area.Left, AMergedCell.ActiveCell);
end;

function TdxSpreadSheetTableView.GetCellStyleHandle(ARow, AColumn: Integer): TdxSpreadSheetCellStyleHandle;
var
  ACell: TdxSpreadSheetCell;
begin
  if InRange(ARow, 0, dxSpreadSheetMaxRowIndex) and InRange(AColumn, 0, dxSpreadSheetMaxColumnIndex) then
    ACell := Cells[ARow, AColumn]
  else
    ACell := nil;

  Result := GetCellStyleHandle(ARow, AColumn, ACell);
end;

function TdxSpreadSheetTableView.GetFactuallyModifiableArea(
  const AArea: TRect; AModification: TdxSpreadSheetCellsModification): TRect;
begin
  case AModification of
    cmShiftCellsHorizontally:
      Result := cxRect(AArea.Left, AArea.Top, dxSpreadSheetMaxColumnIndex, AArea.Bottom);
    cmShiftCellsVertically:
      Result := cxRect(AArea.Left, AArea.Top, AArea.Right, dxSpreadSheetMaxRowIndex);
    cmShiftColumns:
      Result := cxRect(AArea.Left, 0, AArea.Right, dxSpreadSheetMaxRowIndex);
    cmShiftRows:
      Result := cxRect(0, AArea.Top, dxSpreadSheetMaxColumnIndex, AArea.Bottom);
  end;
end;

function TdxSpreadSheetTableView.GetFocusedCellStyle: TdxSpreadSheetCellStyle;
begin
  Result := GetCellStyle(Selection.FocusedRow, Selection.FocusedColumn, Selection.FocusedCell);
end;

function TdxSpreadSheetTableView.GetLockedStateOfCellsInArea(const AArea: TRect): TCheckBoxState;
const
  StateMap: array[Boolean] of TCheckBoxState = (cbUnchecked, cbChecked);
var
  AChangedCount: Integer;
  ADefaultLocked: Boolean;
begin
  AChangedCount := 0;
  ADefaultLocked := SpreadSheet.DefaultCellStyle.Locked;

  Rows.ForEachCell(AArea,
    procedure(AItem: TdxDynamicListItem)
    begin
      if (csLocked in TdxSpreadSheetCell(AItem).StyleHandle.States) <> ADefaultLocked then
        Inc(AChangedCount);
    end);

  if AChangedCount = 0 then
    Result := StateMap[ADefaultLocked]
  else
    if AChangedCount <> (AArea.Width + 1) * (AArea.Height + 1) then
      Result := cbGrayed
    else
      Result := StateMap[not ADefaultLocked];
end;

function TdxSpreadSheetTableView.GetNextVisibleView(AOnCycle: Boolean): TdxSpreadSheetTableView;
var
  AIndex: Integer;
  ASheet: TdxSpreadSheetCustomView;
begin
  Result := Self;
  repeat
    AIndex := Index + 1;
    if AIndex > SpreadSheet.SheetCount - 1 then
      if AOnCycle then
        AIndex := 0
      else
        AIndex := SpreadSheet.SheetCount - 1;
    ASheet := SpreadSheet.Sheets[AIndex];
  until ASheet.Visible or (ASheet = Self) or (AOnCycle and (AIndex = SpreadSheet.SheetCount - 1));
  if ASheet.Visible then
    Result := ASheet as TdxSpreadSheetTableView;
end;

function TdxSpreadSheetTableView.IsRightToLeft: Boolean;
begin
  Result := SpreadSheet.IsRightToLeft;
end;

function TdxSpreadSheetTableView.GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
begin
  Result := FConditionalFormatting;
end;

function TdxSpreadSheetTableView.GetFormulaController: TdxSpreadSheetCustomFormulaController;
begin
  Result := SpreadSheet.FormulaController;
end;

function TdxSpreadSheetTableView.GetConditionalFormattingSelectionArea: TRect;
begin
  if Selection.Count > 1 then
    Result := Selection.Items[0].Rect
  else
    Result := Selection.Area;
end;

procedure TdxSpreadSheetTableView.ForEachCell(const AArea: TRect;
  AProc: TdxSpreadSheetViewForEachCellProc; AGoForward: Boolean = True);
begin
  Rows.ForEachCell(AArea,
    procedure(AItem: TdxDynamicListItem)
    begin
      AProc(TdxSpreadSheetCell(AItem));
    end, AGoForward);
end;

function TdxSpreadSheetTableView.GetCellData(const ARow, AColumn: Integer): IdxSpreadSheetCellData;
begin
  Result := Cells[ARow, AColumn];
end;

function TdxSpreadSheetTableView.GetMaxColumnIndex: Integer;
begin
  Result := Columns.GetMaxItemIndex;
end;

function TdxSpreadSheetTableView.GetMaxRowIndex: Integer;
begin
  Result := Rows.GetMaxItemIndex;
end;

function TdxSpreadSheetTableView.GetNextColumnWithNonEmptyCell(const ARow, AColumn: Integer;
  const AGoForward: Boolean = True): Integer;
const
  AResultBounds: array[Boolean] of Integer = (0, dxSpreadSheetMaxColumnIndex);
var
  AColumnObject: TdxSpreadSheetTableItem;
begin
  AColumnObject := Columns[AColumn];
  if AColumnObject <> nil then
  begin
    AColumnObject := Controller.GetNextColumn(TdxSpreadSheetTableColumn(AColumnObject), AGoForward);
    if AColumnObject <> nil then
      Result := AColumnObject.Index
    else
      Result := AResultBounds[AGoForward];
  end
  else
    Result := Controller.GetNextNonEmptyCell(Rows, ARow, AColumn, AGoForward);
end;

function TdxSpreadSheetTableView.GetNextRowWithNonEmptyCell(const ARow, AColumn: Integer;
  const AGoForward: Boolean = True): Integer;
const
  AResultBounds: array[Boolean] of Integer = (0, dxSpreadSheetMaxRowIndex);
var
  ARowObject: TdxSpreadSheetTableItem;
begin
  ARowObject := Rows[ARow];
  if ARowObject <> nil then
  begin
    ARowObject := Controller.GetNextRow(TdxSpreadSheetTableRow(ARowObject), AGoForward);
    if ARowObject <> nil then
      Result := ARowObject.Index
    else
      Result := AResultBounds[AGoForward];
  end
  else
    Result := Controller.GetNextNonEmptyCell(Columns, AColumn, ARow, AGoForward);
end;

function TdxSpreadSheetTableView.IsRowVisible(const ARow: Integer): Boolean;
var
  ARowItem: TdxSpreadSheetTableItem;
begin
  ARowItem := Rows.Items[ARow];
  Result := (ARowItem = nil) or ARowItem.Visible;
end;

procedure TdxSpreadSheetTableView.SetCellData(const ARow, AColumn: Integer;
  const AValue: Variant; const AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  ACell: TdxSpreadSheetCell;
begin
  ACell := CreateCell(ARow, AColumn);
  if AErrorCode <> ecNone then
    ACell.AsError := AErrorCode
  else
    ACell.AsVariant := AValue;
end;

function TdxSpreadSheetTableView.GetAbsoluteCellBounds(const ARowIndex, AColumnIndex: Integer; ACheckMergedCells: Boolean = True): TRect;
begin
  if ACheckMergedCells then
    Result := MergedCells.ExpandArea(AColumnIndex, ARowIndex)
  else
    Result := cxRectBounds(AColumnIndex, ARowIndex, 0, 0);

  Result := ViewInfo.GetAreaAbsoluteBounds(Result);
end;

function TdxSpreadSheetTableView.GetCellAtAbsolutePoint(const P: TPoint; out ARowIndex, AColumnIndex: Integer): Boolean;

  function FindItem(X, AMaxIndex: Integer; AItems: TdxSpreadSheetTableItems): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to AMaxIndex do
    begin
      if X >= AItems.GetPosition(I) then
        Result := I
      else
        Break;
    end;
  end;

begin
  ViewInfo.Validate;
  AColumnIndex := FindItem(P.X, dxSpreadSheetMaxColumnIndex, Columns);
  ARowIndex := FindItem(P.Y, dxSpreadSheetMaxRowIndex, Rows);
  Result := (AColumnIndex >= 0) and (ARowIndex >= 0);
end;

function TdxSpreadSheetTableView.GetCellArea(const ARowIndex, AColumnIndex: Integer): TRect;
begin
  Result := MergedCells.CheckCell(ARowIndex, AColumnIndex);
end;

procedure TdxSpreadSheetTableView.CellStyleChanged;
begin
  // do nothing
end;

procedure TdxSpreadSheetTableView.CellStyleChanging;
begin
  // do nothing
end;

procedure TdxSpreadSheetTableView.ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);
begin
  // do nothing
end;

function TdxSpreadSheetTableView.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := SpreadSheet.CellStyles;
end;

function TdxSpreadSheetTableView.GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
begin
  Result := SpreadSheet.FormatSettings;
end;

function TdxSpreadSheetTableView.CreateColumns: TdxSpreadSheetTableColumns;
begin
  Result := TdxSpreadSheetTableColumns.Create(Self);
end;

function TdxSpreadSheetTableView.CreateConditionalFormatting: TdxSpreadSheetConditionalFormatting;
begin
  Result := TdxSpreadSheetConditionalFormatting.Create(Self);
end;

function TdxSpreadSheetTableView.CreateController: TdxSpreadSheetCustomViewController;
begin
  Result := TdxSpreadSheetTableViewController.Create(Self);
end;

function TdxSpreadSheetTableView.CreateHitTest: TdxSpreadSheetCustomHitTest;
begin
  Result := TdxSpreadSheetTableViewHitTest.Create(Self);
end;

function TdxSpreadSheetTableView.CreateHyperlinks: TdxSpreadSheetHyperlinks;
begin
  Result := TdxSpreadSheetHyperlinks.Create(Self);
end;

function TdxSpreadSheetTableView.CreateMergedCells: TdxSpreadSheetMergedCellList;
begin
  Result := TdxSpreadSheetMergedCellList.Create(Self);
end;

function TdxSpreadSheetTableView.CreateOptions: TdxSpreadSheetCustomViewOptions;
begin
  Result := TdxSpreadSheetTableViewOptions.Create(Self);
end;

function TdxSpreadSheetTableView.CreateOptionsPrint: TdxSpreadSheetTableViewOptionsPrint;
begin
  Result := TdxSpreadSheetTableViewOptionsPrint.Create(Self);
end;

function TdxSpreadSheetTableView.CreateOptionsProtection: TdxSpreadSheetSheetProtectionOptions;
begin
  Result := TdxSpreadSheetSheetProtectionOptions.Create(OptionsProtectionChangeHandler);
end;

function TdxSpreadSheetTableView.CreateRows: TdxSpreadSheetTableRows;
begin
  Result := TdxSpreadSheetTableRows.Create(Self);
end;

function TdxSpreadSheetTableView.CreateSelection: TdxSpreadSheetTableViewSelection;
begin
  Result := TdxSpreadSheetTableViewSelection.Create(Self);
end;

function TdxSpreadSheetTableView.CreateViewInfo: TdxSpreadSheetCustomViewViewInfo;
begin
  Result := TdxSpreadSheetTableViewInfo.Create(Self);
end;

procedure TdxSpreadSheetTableView.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FOptionsPrint := CreateOptionsPrint;
  FOptionsProtection := CreateOptionsProtection;
  FSelection := CreateSelection;
  FRows := CreateRows;
  FColumns := CreateColumns;
  FMergedCells := CreateMergedCells;
  FConditionalFormatting := CreateConditionalFormatting;
  FHyperlinks := CreateHyperlinks;
end;

procedure TdxSpreadSheetTableView.DestroySubClasses;
begin
  inherited DestroySubClasses;
  FreeAndNil(FMergedCells);
  FreeAndNil(FColumns);
  FreeAndNil(FRows);
  FreeAndNil(FSelection);
  FreeAndNil(FConditionalFormatting);
  FreeAndNil(FHyperlinks);
  FreeAndNil(FOptionsPrint);
  FreeAndNil(FOptionsProtection);
end;

procedure TdxSpreadSheetTableView.DimensionChanged;
begin
  Changes := Changes + [sscDimension];
end;

function TdxSpreadSheetTableView.DoActiveCellChanging(AColumn, ARow: Integer): Boolean;
begin
  Result := SpreadSheet.DoActiveCellChanging(Self, cxPoint(AColumn, ARow));
end;

procedure TdxSpreadSheetTableView.DoCheckChanges;
var
  AZoomChanged: Boolean;
begin
  ValidateDimension;
  if Changes = [] then
    Exit;
  AZoomChanged := sscZoom in Changes;
  inherited DoCheckChanges;
  if AZoomChanged then
  begin
    Changes := Changes - [sscZoom];
    MakeFocusedCellVisible;
  end;
end;

procedure TdxSpreadSheetTableView.DoCompare(const AData1, AData2: TdxSpreadSheetCellData; var Compare: Integer);
begin
  SpreadSheet.DoCompare(Self, AData1, AData2, Compare);
end;

procedure TdxSpreadSheetTableView.DoDataChanged;
begin
  ConditionalFormatting.FlushCache;
  SetDisplayValuesToDirty;
end;

procedure TdxSpreadSheetTableView.DoEditChanged;
begin
  SpreadSheet.DoEditChanged(Self);
end;

procedure TdxSpreadSheetTableView.DoEdited;
begin
  SpreadSheet.DoEdited(Self);
end;

procedure TdxSpreadSheetTableView.DoEditing(var AProperties: TcxCustomEditProperties; var ACanEdit: Boolean);
begin
  SpreadSheet.DoEditing(Self, AProperties, ACanEdit);
end;

procedure TdxSpreadSheetTableView.DoEditValueChanged;
begin
  SpreadSheet.DoEditValueChanged(Self);
end;

procedure TdxSpreadSheetTableView.DoInitEdit(AEdit: TcxCustomEdit);
begin
  SpreadSheet.DoInitEdit(Self, AEdit);
end;

procedure TdxSpreadSheetTableView.DoInitEditValue(AEdit: TcxCustomEdit; var AValue: Variant);
begin
  SpreadSheet.DoInitEditValue(Self, AEdit, AValue);
end;

procedure TdxSpreadSheetTableView.DoOptionsProtectionChanged;
begin
  AddChanges([sscModified]);
  if OptionsProtection.&Protected then
    History.Clear;
  Selection.Validate;
end;

procedure TdxSpreadSheetTableView.DoRemoveCell(ACell: TdxSpreadSheetCell);
begin
  if (Containers <> nil) and (Containers.Count > 0) then
    Containers.CellRemoving(ACell);
  DimensionChanged;
end;

procedure TdxSpreadSheetTableView.ExchangeValues(ACell1, ACell2: TdxSpreadSheetCell);

  procedure CheckFormula(ASource, ADest: TdxSpreadSheetCell);
  begin
    if not ASource.IsFormula then
      Exit;
    ASource.AsFormula.InternalSetOwner(ADest);
  end;

  function GetCellStyle(AStyle1, AStyle2: TdxSpreadSheetCellStyle): TdxSpreadSheetCellStyleHandle;
  begin
    Result := CellStyles.CreateStyle(CellStyles.Fonts.AddClone(AStyle2.Handle.Font),
      CellStyles.Formats.AddClone(AStyle2.Handle.DataFormat),
      CellStyles.Brushes.AddClone(AStyle2.Handle.Brush),
      CellStyles.Borders.AddClone(AStyle1.Handle.Borders));
    TdxSpreadSheetCellStyleAccess(Result).AssignFields(AStyle2.Handle);
    Result := CellStyles.AddStyle(Result);
    Result.AddRef;
  end;

var
  ADataType: TdxSpreadSheetCellDataType;
  AData: array[0..9] of Byte;
  AStyleHandle1, AStyleHandle2: TdxSpreadSheetCellStyleHandle;
begin
  if History.CanAddCommand then
  begin
    History.AddCommand(TdxSpreadSheetHistoryChangeCellCommand.CreateEx(ACell1));
    History.AddCommand(TdxSpreadSheetHistoryChangeCellCommand.CreateEx(ACell2));
  end;
  CheckFormula(ACell1, ACell2);
  CheckFormula(ACell2, ACell1);
  ADataType := ACell1.FDataType;
  ACell1.FDataType := ACell2.FDataType;
  ACell2.FDataType := ADataType;
  cxCopyData(@ACell1.FData, @AData, SizeOf(AData));
  cxCopyData(@ACell2.FData, @ACell1.FData, SizeOf(AData));
  cxCopyData(@AData, @ACell2.FData, SizeOf(AData));

  AStyleHandle1 := GetCellStyle(ACell1.Style, ACell2.Style);
  AStyleHandle2 := GetCellStyle(ACell2.Style, ACell1.Style);

  ACell1.StyleHandle := AStyleHandle1;
  ACell2.StyleHandle := AStyleHandle2;

  ACell1.Changed([sscData, sscModified, sscStyle]);
  ACell2.Changed([sscData, sscModified, sscStyle]);
end;

procedure TdxSpreadSheetTableView.ExchangeValues(ARow1, AColumn1, ARow2, AColumn2: Integer);
begin
  if Cells[ARow1, AColumn1] <> Cells[ARow2, AColumn2] then
    ExchangeValues(CreateCell(ARow1, AColumn1),  CreateCell(ARow2, AColumn2));
end;

procedure TdxSpreadSheetTableView.Pack;
begin
  if History.CurrentAction <> nil then
    Inc(History.FActionLockCount);
  try
    DimensionChanged;
    with TdxSpreadSheetTableViewPackHelper.Create(Self) do
    try
      Pack;
    finally
      Free;
    end;
  finally
    if History.CurrentAction <> nil then
      Dec(History.FActionLockCount);
  end;
end;

procedure TdxSpreadSheetTableView.PasteFromClipboardCore(AOptions: TdxSpreadSheetClipboardPasteOptions;
  const AViewGUID: string; const AClipboardArea: TRect; AClipboardCopyMode: TdxSpreadSheetClipboardCopyMode);
begin
  CheckPasteSelection(AClipboardArea, AClipboardCopyMode);

  ShowHourglassCursor;
  BeginUpdate;
  try
    History.BeginAction(TdxSpreadSheetHistoryPasteFromClipboardAction);
    try
      TdxSpreadSheetTableViewClipboardHelper.Paste(Self, AClipboardArea, AClipboardCopyMode, AViewGUID, AOptions);
    finally
      History.EndAction;
    end;
  finally
    EndUpdate;
    HideHourglassCursor;
  end;
end;

procedure TdxSpreadSheetTableView.RecalculateBestFit;
begin
  BeginUpdate;
  try
    Rows.SetBestFitDirty;
    Rows.CheckBestFit;
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableView.SelectionChanged;
begin
  inherited SelectionChanged;
  if Controller.ClickTimer.Enabled then
    Controller.CanClickOnTheCell := False;
end;

procedure TdxSpreadSheetTableView.SetDisplayValuesToDirty;
begin
  ForEachCell(
    procedure (ACell: TdxSpreadSheetCell)
    begin
      ACell.SetDisplayValueDirty;
    end);
end;

procedure TdxSpreadSheetTableView.ToggleShowFormulas;
begin
  Options.ShowFormulas := dxBooleanToDefaultBoolean(not dxDefaultBooleanToBoolean(Options.ShowFormulas, Options.ShowFormulas = bTrue));
end;

procedure TdxSpreadSheetTableView.ValidateDimension;
var
  ARow: TdxSpreadSheetTableRow;
begin
  if sscDimension in Changes then
  begin
    FDimensions := cxNullRect;
    ARow := TdxSpreadSheetTableRow(Rows.First);
    while ARow <> nil do
    begin
      if ARow.RowCells <> nil then
      begin
        FDimensions.Right := Max(FDimensions.Right, ARow.RowCells.LastIndex);
        FDimensions.Bottom := ARow.Index;
      end;
      ARow := TdxSpreadSheetTableRow(ARow.Next);
    end;

    MergedCells.EnumCells(
      procedure (ACell: TdxSpreadSheetMergedCell)
      begin
        FDimensions := dxSpreadSheetCellsUnion(FDimensions, ACell.Area);
      end);
    Changes := Changes - [sscDimension];
  end;
end;

function TdxSpreadSheetTableView.GetContentOrigin: TPoint;
begin
  Result.X := -ViewInfo.CellsArea.X + ViewInfo.FirstColumnOrigin;
  Result.Y := -ViewInfo.CellsArea.Y + ViewInfo.FirstRowOrigin;
end;

function TdxSpreadSheetTableView.GetPartOffsetByPoint(const P: TPoint): TPoint;
begin
  Result := ViewInfo.GetPartOffsetByPoint(P);
end;

function TdxSpreadSheetTableView.GetPrintArea: TRect;
begin
  if OptionsPrint.Source.Area.Assigned then
    Result := OptionsPrint.Source.Area.Rect
  else
    Result := cxRect(0, 0, dxSpreadSheetMaxColumnIndex, dxSpreadSheetMaxRowIndex);
end;

function TdxSpreadSheetTableView.GetZoomFactor: Integer;
begin
  Result := ScaleFactor.Apply(Options.ZoomFactor);
end;

function TdxSpreadSheetTableView.GetBottomRow: Integer;
begin
  ViewInfo.Validate;
  Result := Max(TopRow, ViewInfo.LastVisibleRow);
end;

function TdxSpreadSheetTableView.GetCell(ARow, AColumn: Integer): TdxSpreadSheetCell;
var
  ARowItem: TdxSpreadSheetTableItem;
begin
  ARowItem := Rows[ARow];
  if ARowItem <> nil then
    Result := ARowItem.Cells[AColumn]
  else
    Result := nil;
end;

function TdxSpreadSheetTableView.GetController: TdxSpreadSheetTableViewController;
begin
  Result := TdxSpreadSheetTableViewController(inherited Controller);
end;

function TdxSpreadSheetTableView.GetDimensions: TRect;
begin
  ValidateDimension;
  Result := FDimensions;
end;

function TdxSpreadSheetTableView.GetEditingController: TdxSpreadSheetTableViewEditingController;
begin
  Result := Controller.EditingController;
end;

function TdxSpreadSheetTableView.GetHistory: TdxSpreadSheetHistory;
begin
  Result := SpreadSheet.History;
end;

function TdxSpreadSheetTableView.GetHitTest: TdxSpreadSheetTableViewHitTest;
begin
  Result := TdxSpreadSheetTableViewHitTest(inherited HitTest);
end;

function TdxSpreadSheetTableView.GetIsEditing: Boolean;
begin
  Result := EditingController.IsEditing;
end;

function TdxSpreadSheetTableView.GetLeftColumn: Integer;
begin
  ViewInfo.Validate;
  Result := ViewInfo.VisibleCells.Left;
end;

function TdxSpreadSheetTableView.GetOptions: TdxSpreadSheetTableViewOptions;
begin
  Result := TdxSpreadSheetTableViewOptions(inherited Options);
end;

function TdxSpreadSheetTableView.GetRightColumn: Integer;
begin
  ViewInfo.Validate;
  Result := Max(LeftColumn, ViewInfo.LastVisibleColumn);
end;

function TdxSpreadSheetTableView.GetState: TdxSpreadSheetStates;
begin
  Result := SpreadSheet.State;
end;

function TdxSpreadSheetTableView.GetStringTable: TdxSpreadSheetSharedStringTable;
begin
  Result := SpreadSheet.StringTable;
end;

function TdxSpreadSheetTableView.GetTopCell(const ACurrentRow, ACurrentColumn: Integer): TdxSpreadSheetCell;
var
  ACell: TDxSpreadSheetCell;
begin
  ACell := GetActiveCell(ACurrentRow, ACurrentColumn);
  if ACell <> nil then
    Result := Cells[ACell.RowIndex - 1, ACell.ColumnIndex]
  else
    Result := Cells[ACurrentRow - 1, ACurrentColumn]
end;

function TdxSpreadSheetTableView.GetTopRow: Integer;
begin
  ViewInfo.Validate;
  Result := ViewInfo.VisibleCells.Top;
end;

function TdxSpreadSheetTableView.GetViewInfo: TdxSpreadSheetTableViewInfo;
begin
  Result := TdxSpreadSheetTableViewInfo(inherited ViewInfo);
end;

procedure TdxSpreadSheetTableView.SetBottomRow(AValue: Integer);
begin
  ViewInfo.ChangeVisibleArea(bBottom, AValue);
end;

procedure TdxSpreadSheetTableView.SetFrozenColumn(AValue: Integer);
begin
  dxSpreadSheetValidate(AValue, -1, dxSpreadSheetMaxColumnIndex);
  if AValue <> FFrozenColumn then
  begin
    FFrozenColumn := AValue;
    ViewInfo.IsDirty := True;
    ViewInfo.FirstScrollableColumn := 0;
    AddChanges([sscLayout, sscModified]);
  end;
end;

procedure TdxSpreadSheetTableView.SetFrozenRow(AValue: Integer);
begin
  dxSpreadSheetValidate(AValue, -1, dxSpreadSheetMaxRowIndex);
  if AValue <> FFrozenRow then
  begin
    FFrozenRow := AValue;
    ViewInfo.IsDirty := True;
    ViewInfo.FirstScrollableRow := 0;
    AddChanges([sscLayout, sscModified]);
  end;
end;

procedure TdxSpreadSheetTableView.SetLeftColumn(AValue: Integer);
begin
  ViewInfo.ChangeVisibleArea(bLeft, AValue);
end;

procedure TdxSpreadSheetTableView.SetOptions(AValue: TdxSpreadSheetTableViewOptions);
begin
  inherited Options := AValue;
end;

procedure TdxSpreadSheetTableView.SetOptionsPrint(AValue: TdxSpreadSheetTableViewOptionsPrint);
begin
  FOptionsPrint.Assign(AValue);
end;

procedure TdxSpreadSheetTableView.SetOptionsProtection(AValue: TdxSpreadSheetSheetProtectionOptions);
begin
  FOptionsProtection.Assign(AValue);
end;

procedure TdxSpreadSheetTableView.SetRightColumn(AValue: Integer);
begin
  ViewInfo.ChangeVisibleArea(bRight, AValue);
end;

procedure TdxSpreadSheetTableView.SetTopRow(AValue: Integer);
begin
  ViewInfo.ChangeVisibleArea(bTop, AValue);
end;

{ TdxSpreadSheetTableViewPainter }

procedure TdxSpreadSheetTableViewPainter.FlushCache;
begin
  inherited FlushCache;
  FGroupExpandButtonMinSize := cxNullSize;
end;

procedure TdxSpreadSheetTableViewPainter.DrawGroupExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AVertical, AExpanded: Boolean);
begin
  BufferedDraw(ACanvas, R,
    procedure (ACanvas: TcxCanvas; const R: TRect)
    begin
      LookAndFeelPainter.DrawSpreadSheetScaledGroupExpandButton(ACanvas, R, AState, dxDefaultScaleFactor);
      LookAndFeelPainter.DrawSpreadSheetScaledGroupExpandButtonGlyph(
        ACanvas, R, AState, AExpanded, dxDefaultScaleFactor, NavigatorImages);
    end);
end;

procedure TdxSpreadSheetTableViewPainter.DrawGroupLevelExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; ALevel: Integer);
begin
  BufferedDraw(ACanvas, R,
    procedure (ACanvas: TcxCanvas; const R: TRect)
    begin
      LookAndFeelPainter.DrawSpreadSheetScaledGroupExpandButton(ACanvas, R, AState, dxDefaultScaleFactor);
    end);

  PrepareCanvasFont(ACanvas, SpreadSheet.Font);
  ACanvas.Font.Color := LookAndFeelPainter.SpreadSheetGroupExpandButtonTextColor(AState);
  ACanvas.Brush.Style := bsClear;
  ACanvas.DrawTexT(IntToStr(ALevel + 1),
    cxRectContent(R, LookAndFeelPainter.SpreadSheetScaledGroupExpandButtonContentOffsets(dxDefaultScaleFactor)),
    taCenter, vaCenter, False, False);
end;

procedure TdxSpreadSheetTableViewPainter.DrawGroupLevelMark(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.FillRect(R, LookAndFeelPainter.SpreadSheetGroupLineColor);
end;

procedure TdxSpreadSheetTableViewPainter.DrawGroupLine(ACanvas: TcxCanvas; const R: TRect;
  AVertical: Boolean; AExpandButtonPosition: TdxSpreadSheetTableItemGroupExpandButtonPosition);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.ExcludeClipRect(cxRectOffset(R,
      2 * ValueIncr[(AExpandButtonPosition = gebpGroupFinish) or AVertical],
      2 * ValueIncr[(AExpandButtonPosition = gebpGroupFinish) or not AVertical]));
    ACanvas.FillRect(R, LookAndFeelPainter.SpreadSheetGroupLineColor);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TdxSpreadSheetTableViewPainter.GetGroupLevelMarkSize: TSize;
begin
  Result := LookAndFeelPainter.SpreadSheetScaledGroupLevelMarkSize(dxDefaultScaleFactor);
end;

function TdxSpreadSheetTableViewPainter.GetGroupExpandButtonMinSize: TSize;
var
  ATextHeight: Integer;
begin
  if cxSizeIsEqual(FGroupExpandButtonMinSize, cxNullSize) then
  begin
    PrepareCanvasFont(cxScreenCanvas, SpreadSheet.Font);
    ATextHeight := cxTextHeight(cxScreenCanvas.Handle);
    cxScreenCanvas.Dormant;

    FGroupExpandButtonMinSize := LookAndFeelPainter.SpreadSheetScaledGroupExpandButtonGlyphSize(dxDefaultScaleFactor);
    FGroupExpandButtonMinSize.cx := Max(FGroupExpandButtonMinSize.cx, ATextHeight) +
      cxMarginsWidth(LookAndFeelPainter.SpreadSheetScaledGroupExpandButtonContentOffsets(dxDefaultScaleFactor));
    FGroupExpandButtonMinSize.cy := Max(FGroupExpandButtonMinSize.cy, ATextHeight) +
      cxMarginsHeight(LookAndFeelPainter.SpreadSheetScaledGroupExpandButtonContentOffsets(dxDefaultScaleFactor));

    FGroupExpandButtonMinSize.cx := Max(FGroupExpandButtonMinSize.cx, FGroupExpandButtonMinSize.cy);
    FGroupExpandButtonMinSize.cy := FGroupExpandButtonMinSize.cx;
  end;
  Result := FGroupExpandButtonMinSize;
end;

function TdxSpreadSheetTableViewPainter.GetGroupLineSize: Integer;
begin
  Result := 6;
end;

procedure TdxSpreadSheetTableViewPainter.BufferedDraw(ACanvas: TcxCanvas; const R: TRect; AProc: TDrawProc);
var
  ABitmap: TcxBitmap;
begin
  if View.ZoomFactor <> 100 then
  begin
    ABitmap := TcxBitmap.CreateSize(R);
    try
      AProc(ABitmap.cxCanvas, ABitmap.ClientRect);
      ACanvas.StretchDraw(R, ABitmap);
    finally
      ABitmap.Free;
    end;
  end
  else
    AProc(ACanvas, R);
end;

{ TdxSpreadSheetTableViewInfo }

constructor TdxSpreadSheetTableViewInfo.Create(AView: TdxSpreadSheetCustomView);
begin
  inherited Create(AView);
  FCells := TdxSpreadSheetTableViewCellViewInfoList.Create;
  FCellsOverlay := TdxSpreadSheetCellViewInfoList.Create;
  FColumnsHeader := TdxSpreadSheetCellViewInfoList.Create;
  FBackgroundCells := TdxSpreadSheetCellViewInfoList.Create;
  FRowsHeader := TdxSpreadSheetCellViewInfoList.Create;
  FGroupingAreas := TdxSpreadSheetCellViewInfoList.Create;
  FSelectionCell := TdxSpreadSheetTableViewSelectionViewInfo.Create(Self);
  FMergedCells := TdxSpreadSheetTableViewMergedCellViewInfoList.Create;
  IsDirty := True;
end;

destructor TdxSpreadSheetTableViewInfo.Destroy;
begin
  Clear;
  FreeAndNil(FMergedCells);
  FreeAndNil(FSelectionCell);
  FreeAndNil(FBackgroundCells);
  FreeAndNil(FGroupingAreas);
  FreeAndNil(FCellsOverlay);
  FreeAndNil(FColumnsHeader);
  FreeAndNil(FRowsHeader);
  FreeAndNil(FCells);
  inherited Destroy;
end;

function TdxSpreadSheetTableViewInfo.CanDrawCellSelection: Boolean;
begin
  Result := (View.Controller.FocusedContainer = nil) and (View.Controller.ForcedSelectionMode = smNone) or
   (SpreadSheet.DragAndDropState <> ddsNone) and (SpreadSheet.DragAndDropObject is TdxSpreadSheetTableViewCustomSelectionDragAndDropObject);
end;

procedure TdxSpreadSheetTableViewInfo.Calculate;
var
  AZoomFactor: Single;
begin
  inherited Calculate;
  Columns.Groups.Validate;
  Rows.Groups.Validate;

  Clear;
  CheckRowsBestFit;
  if View.ZoomFactor <> 100 then
  begin
    AZoomFactor := 100 / View.ZoomFactor;
    FBounds.Bottom := Trunc(Ceil(FBounds.Bottom * AZoomFactor));
    FBounds.Right := Trunc(Ceil(FBounds.Right * AZoomFactor));
  end;

  FContentParams := Styles.GetContentStyle(View);
  FHeaderParams := Styles.GetHeaderStyle(View);
  FSelectionParams := Styles.GetSelectionStyle;

  FHasGridLines := Options.ActualGridLines;
  FGridLineColor := cxGetActualColor(OptionsView.GridLineColor, LookAndFeelPainter.DefaultGridLineColor);
  FDefaultCellStyleHandle := View.CellStyles.DefaultStyle;

  CalculateAreas(Bounds);
  CalculateGroupingAreas;
  CalculateHeaders;
  CalculateCells;
  CalculateContainers;
  CalculatePrintAreas;

  SelectionChanged;
  if View.IsEditing then
  begin
    View.EditingController.UpdateEditPosition;
    View.EditingController.UpdateReferencesHighlighting;
  end;
  SpreadSheet.UpdateScrollBars;
end;

procedure TdxSpreadSheetTableViewInfo.Clear;
begin
  inherited Clear;

  if FFocusedCell <> nil then
  begin
    if FFocusedCell.IsPermanent then
      FFocusedCell.FMergedCell := nil
    else
      FFocusedCell := nil;
  end;

  FGroupingAreaLeft := nil;
  FGroupingAreaTop := nil;

  BackgroundCells.Clear;
  Cells.Clear;
  CellsOverlay.Clear;
  ColumnsHeader.Clear;
  GroupingAreas.Clear;
  RowsHeader.Clear;
  MergedCells.Clear;
end;

procedure TdxSpreadSheetTableViewInfo.Draw(ACanvas: TcxCanvas);
var
  APrevPixelsPerInch: Integer;
  APrevXForm: TXForm;
  AZoomFactor: Integer;
  I: Integer;
begin
  APrevPixelsPerInch := dxSpreadSheetPrepareCanvas(ACanvas, SpreadSheet.Font, dxDefaultDPI);
  try
    AZoomFactor := View.ZoomFactor;
    if AZoomFactor <> 100 then
      dxSetZoomFactor(ACanvas, AZoomFactor, APrevXForm);
    try
      for I := 0 to GetPartCount - 1 do
        DrawPart(ACanvas, cxPointScale(GetPartOffset(I), AZoomFactor, 100), GetPartBounds(I));
      ACanvas.ExcludeClipRect(cxRect(Bounds.TopLeft, CellsArea.BottomRight));
    finally
      if AZoomFactor <> 100 then
        SetWorldTransform(ACanvas.Handle, APrevXForm);
    end;
  finally
    dxSpreadSheetUnprepareCanvas(ACanvas, APrevPixelsPerInch);
  end;
end;

procedure TdxSpreadSheetTableViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest);
var
  AOffset: TPoint;
begin
  if not IsDirty and (View.Changes = []) then
  begin
    if PtInRect(Bounds, AHitTest.ActualHitPoint) then
    begin
      AOffset := GetPartOffsetByPoint(AHitTest.ActualHitPoint);
      if not cxPointIsNull(AOffset) then
      begin
        AHitTest.FActualHitPoint := cxPointOffset(AHitTest.FActualHitPoint, AOffset);
        CalculateHitTest(AHitTest);
        AHitTest.FActualHitPoint := cxPointOffset(AHitTest.FActualHitPoint, AOffset, False);
      end
      else
        CalculateHitTest(AHitTest);
    end;
  end;
end;

procedure TdxSpreadSheetTableViewInfo.Validate;
begin
  if IsDirty then
  begin
    Calculate;
    View.Invalidate;
  end;
end;

function TdxSpreadSheetTableViewInfo.GetAreaAbsoluteBounds(const AArea: TRect): TRect;
begin
  Result.Top := Rows.GetPosition(AArea.Top);
  Result.Left := Columns.GetPosition(AArea.Left);
  Result.Right := Result.Left + Columns.GetDistance(AArea.Left, AArea.Right);
  Result.Bottom := Result.Top + Rows.GetDistance(AArea.Top, AArea.Bottom);
end;

function TdxSpreadSheetTableViewInfo.GetAreaBounds(ARow, AColumn: Integer; const AArea, ACellBounds: TRect): TRect;
begin
  Result := cxRectInflate(ACellBounds,
    Columns.GetDistance(AArea.Left, AColumn - 1), Rows.GetDistance(AArea.Top, ARow - 1),
    Columns.GetDistance(AColumn + 1, AArea.Right), Rows.GetDistance(ARow + 1, AArea.Bottom));

  if FrozenRow >= 0 then
  begin
    if (ARow > FrozenRow) and (Result.Top <> ACellBounds.Top) then
      Dec(Result.Top, OptionsView.FrozenPaneSeparatorWidth - 1)
    else
      if (ARow <= FrozenRow) and (Result.Bottom <> ACellBounds.Bottom) then
        Inc(Result.Bottom, OptionsView.FrozenPaneSeparatorWidth - 1);
  end;

  if FrozenColumn >= 0 then
  begin
    if (AColumn > FrozenColumn) and (Result.Left <> ACellBounds.Left) then
      Dec(Result.Left, OptionsView.FrozenPaneSeparatorWidth - 1)
    else
      if (ARow <= FrozenColumn) and (Result.Bottom <> ACellBounds.Right) then
        Inc(Result.Right, OptionsView.FrozenPaneSeparatorWidth - 1);
  end;
end;

function TdxSpreadSheetTableViewInfo.GetAreaBounds(const AArea: TRect): TRect;
begin
  Result := GetAreaAbsoluteBounds(AArea);
  Result := cxRectOffset(Result, -FirstColumnOrigin, -FirstRowOrigin);
  Result := cxRectOffset(Result, CellsArea.TopLeft);
end;

procedure TdxSpreadSheetTableViewInfo.AddCell(ARow: TdxSpreadSheetTableRow; ARowIndex, AColumnIndex: Integer; const ABounds: TRect);
var
  ABorder: TcxBorder;
  ACell: TdxSpreadSheetCell;
  ACellStyleHandle: TdxSpreadSheetCellStyleHandle;
  ACellViewInfo: TdxSpreadSheetTableViewCellViewInfo;
  AMergedCell: TdxSpreadSheetMergedCell;
  AMergedCellViewInfo: TdxSpreadSheetTableViewMergedCellViewInfo;
begin
  if ARow <> nil then
    ACell := ARow.GetCell(AColumnIndex)
  else
    ACell := nil;

  if not Cells.FindItemForCell(ARowIndex, AColumnIndex, ACellViewInfo) then
  begin
    ACellViewInfo := CreateCellViewInfo(ACell);
    ACellViewInfo.FColumn := AColumnIndex;
    ACellViewInfo.FRow := ARowIndex;
    Cells.Add(ACellViewInfo);
  end;

  AMergedCellViewInfo := nil;
  AMergedCell := GetMergedCell(ARowIndex, AColumnIndex);
  if AMergedCell <> nil then
  begin
    if not MergedCells.TryGetValue(AMergedCell, AMergedCellViewInfo) then
    begin
      AMergedCellViewInfo := CreateMergedCellViewInfo(AMergedCell);
      AMergedCellViewInfo.SetBounds(GetAreaBounds(ARowIndex, AColumnIndex, AMergedCell.Area, ABounds), CellsArea);
      AMergedCellViewInfo.InitStyle(View.GetCellStyleHandle(AMergedCell));
      AMergedCellViewInfo.InitDrawValue;
      MergedCells.Add(AMergedCellViewInfo);
    end;
  end;

  ACellStyleHandle := View.GetCellStyleHandle(ARowIndex, AColumnIndex, ACell);
  ACellViewInfo.FMergedCell := AMergedCellViewInfo;
  ACellViewInfo.FCell := ACell;
  ACellViewInfo.InitStyle(ACellStyleHandle);

  if (AMergedCellViewInfo <> nil) and (AMergedCellViewInfo.Style.Handle <> ACellStyleHandle) then
  begin
    ACellViewInfo.Style.BeginUpdate;
    try
      ACellViewInfo.Style.Assign(AMergedCellViewInfo.Style);
      for ABorder := Low(ABorder) to High(ABorder) do
        ACellViewInfo.Style.Borders[ABorder].Assign(ACellStyleHandle.Borders);
    finally
      ACellViewInfo.Style.EndUpdate;
    end;
  end;

  ACellViewInfo.SetBounds(ABounds, CellsArea);
  ACellViewInfo.InitDrawValue;
end;

procedure TdxSpreadSheetTableViewInfo.AddFrozenPaneSeparator(const ABounds: TRect);
var
  ACell: TdxSpreadSheetTableViewFrozenPaneSeparatorCellViewInfo;
begin
  ACell := TdxSpreadSheetTableViewFrozenPaneSeparatorCellViewInfo.Create(Self);
  ACell.Color := cxGetActualColor(OptionsView.FrozenPaneSeparatorColor, LookAndFeelPainter.SpreadSheetFrozenPaneSeparatorColor);
  ACell.SetBounds(ABounds, Bounds);
  BackgroundCells.Add(ACell);
end;

procedure TdxSpreadSheetTableViewInfo.AddGroupingAreaCells;

  function AddGroupingArea(AItems: TdxSpreadSheetTableItems;
    AClass: TdxSpreadSheetTableViewCustomGroupingAreaViewInfoClass): TdxSpreadSheetTableViewCustomGroupingAreaViewInfo;
  begin
    if AItems.Groups.Count > 0 then
    begin
      Result := AClass.Create(Self);
      GroupingAreas.Add(Result);
    end
    else
      Result := nil;
  end;

begin
  FGroupingAreaTop := AddGroupingArea(Columns, TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo);
  FGroupingAreaLeft := AddGroupingArea(Rows, TdxSpreadSheetTableViewRowsGroupingAreaViewInfo);
end;

procedure TdxSpreadSheetTableViewInfo.AddPageBreak(const ABounds: TRect);
var
  ACellViewInfo: TdxSpreadSheetCellViewInfo;
begin
  ACellViewInfo := CreatePageBreakViewInfo;
  ACellViewInfo.SetBounds(ABounds, Bounds);
  CellsOverlay.Add(ACellViewInfo);
end;

procedure TdxSpreadSheetTableViewInfo.AddPageBreakHorz(const ARowIndex: Integer; const APrintArea: TRect);
var
  ARect: TRect;
begin
  if (ARowIndex > APrintArea.Top) and (ARowIndex < APrintArea.Bottom) then
  begin
    ARect := GetAreaBounds(cxRect(APrintArea.Left, ARowIndex, APrintArea.Right, ARowIndex));
    ARect := cxRectOffset(ARect, 0, -1);
    ARect.Bottom := ARect.Top + 1;
    AddPageBreak(ARect);
  end;
end;

procedure TdxSpreadSheetTableViewInfo.AddPageBreakVert(const AColumnIndex: Integer; const APrintArea: TRect);
var
  ARect: TRect;
begin
  if (AColumnIndex > APrintArea.Left) and (AColumnIndex < APrintArea.Right) then
  begin
    ARect := GetAreaBounds(cxRect(AColumnIndex, APrintArea.Top, AColumnIndex, APrintArea.Bottom));
    ARect := cxRectOffset(ARect, -1, 0);
    ARect.Right := ARect.Left + 1;
    AddPageBreak(ARect);
  end;
end;

procedure TdxSpreadSheetTableViewInfo.AddPrintArea(const AArea: TRect);
var
  AAreaViewInfo: TdxSpreadSheetCellViewInfo;
begin
  AAreaViewInfo := CreatePrintAreaViewInfo;
  AAreaViewInfo.SetBounds(GetAreaBounds(AArea), Bounds);
  CellsOverlay.Add(AAreaViewInfo);
end;

procedure TdxSpreadSheetTableViewInfo.PrepareCanvas(ACanvas: TcxCanvas; const AParams: TcxViewParams);
begin
  Painter.PrepareCanvasFont(ACanvas, AParams.Font);
end;

procedure TdxSpreadSheetTableViewInfo.CalculateAreas(ABounds: TRect);

  function CalculateHeaderSize: TSize;
  begin
    Result.cx := cxTextOffset * 2 + LookAndFeelPainter.ScaledHeaderWidth(cxScreenCanvas,
      cxBordersAll, IntToStr(Max(100, VisibleCells.Bottom)), cxScreenCanvas.Font, dxDefaultScaleFactor);
    Result.cy := cxTextOffset + LookAndFeelPainter.ScaledHeaderHeight(cxTextHeight(cxScreenCanvas.Font), dxDefaultScaleFactor);
  end;

var
  AAdjustCells: Boolean;
begin
  AddGroupingAreaCells;
  if GroupingAreaTop <> nil then
    Inc(ABounds.Top, GroupingAreaTop.MeasureSize);
  if GroupingAreaLeft <> nil then
    Inc(ABounds.Left, GroupingAreaLeft.MeasureSize);

  FCellsArea := ABounds;
  if Options.ActualHeaders then
  begin
    PrepareCanvas(cxScreenCanvas, HeaderParams);
    try
      for AAdjustCells := False to True do
      begin
        FHeaderSize := CalculateHeaderSize;
        FCellsArea.Left := ABounds.Left + HeaderSize.cx;
        FCellsArea.Top := ABounds.Top + HeaderSize.cy;
        CalculateVisibleArea;
      end;
    finally
      cxScreenCanvas.Dormant;
    end;
  end
  else
  begin
    FHeaderSize := cxNullSize;
    CalculateVisibleArea;
  end;
end;

procedure TdxSpreadSheetTableViewInfo.CalculateCells;
var
  ABounds: TRect;
  AColumn: TdxSpreadSheetTableViewHeaderCellViewInfo;
  ARow: TdxSpreadSheetTableViewRowHeaderCellViewInfo;
  I, J: Integer;
begin
  if FocusedCell <> nil then
    FocusedCell.SetBounds(cxRectSetOrigin(FocusedCell.Bounds, cxInvisiblePoint), Bounds);

  for I := 0 to RowsHeader.Count - 1 do
  begin
    ARow := TdxSpreadSheetTableViewRowHeaderCellViewInfo(RowsHeader.List[I]);
    ARow.FFirstCellIndex := Cells.Count;
    for J := 1 to ColumnsHeader.Count - 1 do
    begin
      AColumn := TdxSpreadSheetTableViewHeaderCellViewInfo(ColumnsHeader.List[J]);
      ABounds := cxRect(AColumn.Bounds.Left - 1, ARow.Bounds.Top - 1, AColumn.Bounds.Right - 1, ARow.Bounds.Bottom - 1);
      if (ABounds.Right > ABounds.Left) and (ABounds.Bottom > ABounds.Top) then
      begin
        if J = 1 then
          AddOutOfBoundsNonEmptyCell(TdxSpreadSheetTableRow(ARow.Item), AColumn.Index, ABounds, False);
        AddCell(TdxSpreadSheetTableRow(ARow.Item), ARow.Index, AColumn.Index, ABounds);
        if J = ColumnsHeader.Count - 1 then
          AddOutOfBoundsNonEmptyCell(TdxSpreadSheetTableRow(ARow.Item), AColumn.Index, ABounds, True);
      end;
    end;
    ARow.FLastCellIndex := Cells.Count - 1;
  end;

  for I := 0 to RowsHeader.Count - 1 do
  begin
    ARow := TdxSpreadSheetTableViewRowHeaderCellViewInfo(RowsHeader.List[I]);
    MergeCellsBorders(ARow.FFirstCellIndex, ARow.FLastCellIndex);
  end;

  for I := 0 to RowsHeader.Count - 1 do
  begin
    ARow := TdxSpreadSheetTableViewRowHeaderCellViewInfo(RowsHeader.List[I]);
    PostProcessRowCells(TdxSpreadSheetTableRow(ARow.Item), ARow.FFirstCellIndex, ARow.FLastCellIndex);
  end;
end;

procedure TdxSpreadSheetTableViewInfo.CalculateColumnHeaders;
var
  ACell: TdxSpreadSheetTableViewHeaderCellViewInfo;
  ACellBounds: TRect;
  ACellSize: Integer;
  AClipRect: TRect;
  AColumn: TdxSpreadSheetTableItem;
  AIndex: Integer;
  APrevCell: TdxSpreadSheetTableViewHeaderCellViewInfo;
begin
  AClipRect := Bounds;
  ACellBounds := cxRectSetBottom(CellsArea, CellsArea.Top, HeaderSize.cy);
  ACellBounds := cxRectSetRight(ACellBounds, ACellBounds.Left, HeaderSize.cx);

  ACell := TdxSpreadSheetTableViewHeaderCornerCellViewInfo.Create(Self);
  ACell.Initialize(nil, -1, [nRight, nBottom], '');
  ACell.SetBounds(ACellBounds, AClipRect);
  ColumnsHeader.Add(ACell);

  APrevCell := nil;
  AIndex := VisibleCells.Left;
  FLastVisibleColumn := AIndex;
  while AIndex <= VisibleCells.Right do
  begin
    if (ACellBounds.Right = FrozenColumnSeparatorPosition) and (AIndex <= FirstScrollableColumn) then
    begin
      if OptionsView.FrozenPaneSeparatorWidth > 0 then
      begin
        AddFrozenPaneSeparator(cxRectSetLeft(Bounds, FrozenColumnSeparatorPosition - 1, OptionsView.FrozenPaneSeparatorWidth));
        Inc(ACellBounds.Right, OptionsView.FrozenPaneSeparatorWidth - 1);
      end;
      AIndex := FirstScrollableColumn;
      Inc(ACellBounds.Right, FirstScrollableColumnOffset);
      Inc(AClipRect.Right, FirstScrollableColumnOffset);
    end;

    AColumn := Columns[AIndex];
    if AColumn <> nil then
      ACellSize := AColumn.Size
    else
      ACellSize := Columns.DefaultSize;

    if (AIndex = VisibleCells.Left) or (ACellSize > 0) then
    begin
      ACell := TdxSpreadSheetTableViewColumnHeaderCellViewInfo.Create(Self);
      ACellBounds := cxRectSetLeft(ACellBounds, ACellBounds.Right, ACellSize);
      if ACellBounds.Right <= AClipRect.Right then
      begin
        Inc(FVisibleColumnCount);
        FLastVisibleColumn := AIndex;
      end;
      ACell.Initialize(AColumn, AIndex, [nLeft, nRight], Columns.GetItemText(AIndex));
      ACell.SetBounds(ACellBounds, AClipRect);
      ColumnsHeader.Add(ACell);
      if APrevCell <> nil then
        APrevCell.FNextNeighbor := ACell;
      APrevCell := ACell;
    end;
    Inc(AIndex);
  end;
  FCellsArea.Right := Max(ACellBounds.Right, CellsArea.Right);
end;

procedure TdxSpreadSheetTableViewInfo.CalculateContainerDependedCells(AContainerViewInfo: TdxSpreadSheetContainerViewInfo);
var
  AConnectionViewInfo: TdxSpreadSheetCommentContainerConnectionViewInfo;
begin
  if AContainerViewInfo is TdxSpreadSheetCommentContainerViewInfo then
  begin
    AConnectionViewInfo := TdxSpreadSheetCommentContainerConnectionViewInfo.Create(AContainerViewInfo);
    AConnectionViewInfo.SetBounds(cxNullRect, CellsArea);
    TdxSpreadSheetCommentContainerViewInfo(AContainerViewInfo).ConnectionViewInfo := AConnectionViewInfo;
    CellsOverlay.Add(AConnectionViewInfo);
  end;
end;

procedure TdxSpreadSheetTableViewInfo.CalculateGroupingAreas;
var
  AWorkArea: TRect;
  ARect: TRect;
begin
  AWorkArea := cxRectSetSize(Bounds,
    cxRectWidth(Bounds) + FirstScrollableColumnOffset,
    cxRectHeight(Bounds) + FirstScrollableRowOffset);

  if GroupingAreaTop <> nil then
  begin
    ARect := cxRectSetHeight(AWorkArea, GroupingAreaTop.MeasureSize);
    if GroupingAreaLeft <> nil then
      Inc(ARect.Left, GroupingAreaLeft.MeasureSize);
    GroupingAreaTop.SetBounds(ARect, AWorkArea);
  end;

  if GroupingAreaLeft <> nil then
  begin
    ARect := cxRectSetWidth(AWorkArea, GroupingAreaLeft.MeasureSize);
    if GroupingAreaTop <> nil then
      Inc(ARect.Top, GroupingAreaTop.MeasureSize);
    GroupingAreaLeft.SetBounds(ARect, AWorkArea);
  end;
end;

procedure TdxSpreadSheetTableViewInfo.CalculateHeaders;
begin
  FVisibleColumnCount := 0;
  FVisibleRowCount := 0;
  FScrollableArea := CellsArea;

  CalculateColumnHeaders;
  CalculateRowHeaders;

  if FrozenColumn >= 0 then
    FScrollableArea.Left := FrozenColumnSeparatorPosition - 1 + OptionsView.FrozenPaneSeparatorWidth;
  if FrozenRow >= 0 then
    FScrollableArea.Top := FrozenRowSeparatorPosition - 1 + OptionsView.FrozenPaneSeparatorWidth;
  FVisibleColumnCount := Max(FVisibleColumnCount, 1);
  FVisibleRowCount := Max(FVisibleRowCount, 1);
end;

procedure TdxSpreadSheetTableViewInfo.CalculateHitTest(AHitTest: TdxSpreadSheetCustomHitTest);
begin
  if not (
    GroupingAreas.CalculateHitTest(AHitTest) or
    ReferenceHighlighter.InitHitTest(AHitTest) or
    SelectionCell.InitHitTest(AHitTest) or
    ColumnsHeader.CalculateHitTest(AHitTest) or
    RowsHeader.CalculateHitTest(AHitTest) or
    CalculateContainersHitTest(AHitTest) or
    CellsOverlay.CalculateHitTest(AHitTest) or
    Cells.CalculateHitTest(AHitTest) or
    BackgroundCells.CalculateHitTest(AHitTest))
  then
    AHitTest.SetHitCode(hcBackground, True);
end;

procedure TdxSpreadSheetTableViewInfo.CalculatePrintAreas;
var
  AOptionsPrint: TdxSpreadSheetTableViewOptionsPrint;
  APrintArea: TRect;
  I: Integer;
begin
  if OptionsView.PrintAreas then
  begin
    APrintArea := View.PrintArea;
    AOptionsPrint := View.OptionsPrint;
    if AOptionsPrint.Source.Area.Assigned then
      AddPrintArea(AOptionsPrint.Source.Area.Rect);
    for I := 0 to AOptionsPrint.Pagination.RowPageBreaks.Count - 1 do
      AddPageBreakHorz(AOptionsPrint.Pagination.RowPageBreaks.Items[I], APrintArea);
    for I := 0 to AOptionsPrint.Pagination.ColumnPageBreaks.Count - 1 do
      AddPageBreakVert(AOptionsPrint.Pagination.ColumnPageBreaks.Items[I], APrintArea);
  end;
end;

procedure TdxSpreadSheetTableViewInfo.CalculateRowHeaders;
var
  ACell: TdxSpreadSheetTableViewHeaderCellViewInfo;
  ACellBounds: TRect;
  AClipRect: TRect;
  AIndex: Integer;
  APrevCell: TdxSpreadSheetTableViewHeaderCellViewInfo;
  ARow: TdxSpreadSheetTableItem;
  ARowSize: Integer;
begin
  AClipRect := Bounds;
  ACellBounds := cxRectSetHeight(cxRectSetRight(CellsArea, CellsArea.Left, HeaderSize.cx), 0);

  APrevCell := nil;
  AIndex := VisibleCells.Top;
  FLastVisibleRow := AIndex;
  while AIndex <= VisibleCells.Bottom do
  begin
    if (ACellBounds.Bottom = FrozenRowSeparatorPosition) and (AIndex <= FirstScrollableRow) then
    begin
      if OptionsView.FrozenPaneSeparatorWidth > 0 then
      begin
        AddFrozenPaneSeparator(cxRectSetTop(Bounds, FrozenRowSeparatorPosition - 1, OptionsView.FrozenPaneSeparatorWidth));
        Inc(ACellBounds.Bottom, OptionsView.FrozenPaneSeparatorWidth - 1);
      end;
      AIndex := FirstScrollableRow;
      Inc(ACellBounds.Bottom, FirstScrollableRowOffset);
      Inc(AClipRect.Bottom, FirstScrollableRowOffset);
    end;

    ARow := Rows[AIndex];
    if ARow <> nil then
      ARowSize := ARow.Size
    else
      ARowSize := Rows.DefaultSize;

    if (AIndex = VisibleCells.Top) or (ARowSize > 0) then
    begin
      ACell := TdxSpreadSheetTableViewRowHeaderCellViewInfo.Create(Self);
      ACellBounds := cxRectSetTop(ACellBounds, ACellBounds.Bottom, ARowSize);
      if ACellBounds.Bottom <= AClipRect.Bottom then
      begin
        Inc(FVisibleRowCount);
        FLastVisibleRow := AIndex;
      end;
      ACell.Initialize(ARow, AIndex, [nTop, nBottom], Rows.GetItemText(AIndex));
      ACell.SetBounds(ACellBounds, AClipRect);
      RowsHeader.Add(ACell);

      if APrevCell <> nil then
        APrevCell.FNextNeighbor := ACell;
      APrevCell := ACell;
    end;
    Inc(AIndex);
  end;
  FCellsArea.Bottom := Max(CellsArea.Bottom, ACellBounds.Bottom);
end;

procedure TdxSpreadSheetTableViewInfo.CalculateSelection;
begin
  if not Cells.FindItemForCell(View.Selection.FocusedRow, View.Selection.FocusedColumn, FFocusedCell) then
    FFocusedCell := nil;
  SelectionCell.Calculate;
end;

procedure TdxSpreadSheetTableViewInfo.CalculateVisibleArea;
begin
  if FrozenColumn >= 0 then
    FVisibleCells.Left := 0;
  if FrozenRow >= 0 then
    FVisibleCells.Top := 0;

  CalculateLastItemIndex(cxRectWidth(CellsArea), VisibleCells.Left, FrozenColumn, FFirstScrollableColumn,
    dxSpreadSheetMaxColumnIndex, Columns, FFrozenColumnSeparatorPosition, FVisibleCells.Right, FCellsArea.Left);
  CalculateLastItemIndex(cxRectHeight(CellsArea), VisibleCells.Top, FrozenRow, FFirstScrollableRow,
    dxSpreadSheetMaxRowIndex, Rows, FFrozenRowSeparatorPosition, FVisibleCells.Bottom, FCellsArea.Top);

  FFirstColumnOrigin := Columns.GetDistance(0, VisibleCells.Left - 1);
  FFirstRowOrigin := Rows.GetDistance(0, VisibleCells.Top - 1);
  FFirstScrollableColumnOffset := Columns.GetDistance(FrozenColumn + 1, FirstScrollableColumn - 1);
  FFirstScrollableRowOffset := Rows.GetDistance(FrozenRow + 1, FirstScrollableRow - 1);
end;

procedure TdxSpreadSheetTableViewInfo.ChangeVisibleArea(ASide: TcxBorder; AValue: Integer);
begin
  ChangeVisibleAreaBounds(ASide, AValue);
  Calculate;
  View.Invalidate;
end;

procedure TdxSpreadSheetTableViewInfo.ChangeVisibleAreaBounds(ASide: TcxBorder; AValue: Integer);
var
  AIndex: Integer;
  AScrolled: Boolean;
begin
  case ASide of
    bLeft, bRight:
      dxSpreadSheetValidate(AValue, 0, dxSpreadSheetMaxColumnIndex);
    bTop, bBottom:
      dxSpreadSheetValidate(AValue, 0, dxSpreadSheetMaxRowIndex);
  end;

  AScrolled := False;
  case ASide of
    bLeft:
      FVisibleCells.Left := AValue;
    bTop:
      FVisibleCells.Top := AValue;
    bRight:
      begin
        AIndex := CalculatePrevPageFirstIndex(AValue, True);
        if FrozenColumn < 0 then
          FVisibleCells.Left := AIndex
        else
        begin
          FirstScrollableColumn := AIndex;
          AScrolled := IsDirty;
        end;
      end;
    bBottom:
      begin
        AIndex := CalculatePrevPageFirstIndex(AValue, False);
        if FrozenRow < 0 then
          FVisibleCells.Top := AIndex
        else
        begin
          FirstScrollableRow := AIndex;
          AScrolled := IsDirty;
        end;
      end;
  end;
  IsDirty := True;
  if not AScrolled then
    case ASide of
      bLeft, bRight:
        SpreadSheet.DoScroll(sbHorizontal, FirstScrollableColumn);
      bTop, bBottom:
        SpreadSheet.DoScroll(sbVertical, FirstScrollableRow);
    end;
end;

procedure TdxSpreadSheetTableViewInfo.CheckRowsBestFit;
begin
  View.BeginUpdate;
  try
    Rows.CheckBestFit;
  finally
    View.EndUpdate;
  end;
end;

function TdxSpreadSheetTableViewInfo.CreateCellViewInfo(ACell: TdxSpreadSheetCell): TdxSpreadSheetTableViewCellViewInfo;
begin
  Result := TdxSpreadSheetTableViewCellViewInfo.Create(Self);
end;

function TdxSpreadSheetTableViewInfo.CreateMergedCellViewInfo(
  AMergedCell: TdxSpreadSheetMergedCell): TdxSpreadSheetTableViewMergedCellViewInfo;
begin
  Result := TdxSpreadSheetTableViewMergedCellViewInfo.Create(Self, AMergedCell);
end;

function TdxSpreadSheetTableViewInfo.CreatePainter: TdxSpreadSheetCustomViewPainter;
begin
  Result := TdxSpreadSheetTableViewPainter.Create(View);
end;

function TdxSpreadSheetTableViewInfo.CreatePageBreakViewInfo: TdxSpreadSheetCellViewInfo;
begin
  Result := TdxSpreadSheetTableViewPageBreakViewInfo.Create(View);
end;

function TdxSpreadSheetTableViewInfo.CreatePrintAreaViewInfo: TdxSpreadSheetCellViewInfo;
begin
  Result := TdxSpreadSheetTableViewPrintAreaViewInfo.Create(View);
end;

procedure TdxSpreadSheetTableViewInfo.DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  AItems: TdxSpreadSheetTableItems;
  APosition: Integer;
begin
  if AScrollCode = scEndScroll then
    Exit;
  SpreadSheet.ShowTouchScrollUI(SpreadSheet, True);
  APosition := IfThen(AScrollBarKind = sbHorizontal, FirstScrollableColumn, FirstScrollableRow);
  case AScrollCode of
    scTrack:
      APosition := AScrollPos + (IfThen(AScrollBarKind = sbHorizontal, FrozenColumn, FrozenRow) + 1);

    scPageUp:
      if AScrollBarKind = sbHorizontal then
        APosition := CalculatePrevPageFirstIndex(View.Controller.GetNextVisibleCell(APosition, adLeft), True)
      else
        APosition := CalculatePrevPageFirstIndex(View.Controller.GetNextVisibleCell(APosition, adUp), False);

    scPageDown:
      if AScrollBarKind = sbHorizontal then
        Inc(APosition, VisibleColumnCount - (FrozenColumn + 1))
      else
        Inc(APosition, VisibleRowCount - (FrozenRow + 1));

    scLineUp, scLineDown:
      begin
        if AScrollBarKind = sbHorizontal then
          AItems := View.Columns
        else
          AItems := View.Rows;

        Inc(APosition, ValueIncr[AScrollCode = scLineDown]);
        while not AItems.GetItemVisible(APosition) do
          Inc(APosition, ValueIncr[AScrollCode = scLineDown]);
      end;
  end;

  if AScrollBarKind = sbHorizontal then
    FirstScrollableColumn := APosition
  else
    FirstScrollableRow := APosition;

  AScrollPos := IfThen(AScrollBarKind = sbHorizontal, HScrollBarPos, VScrollBarPos);
end;

procedure TdxSpreadSheetTableViewInfo.DrawCellsArea(ACanvas: TcxCanvas);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(CellsArea);

    if CanDrawCellSelection then
    begin
      ACanvas.SaveClipRegion;
      try
        Cells.Draw(ACanvas, dsFirst);
        SelectionCell.Draw(ACanvas, dsFirst);
        Cells.Draw(ACanvas, dsSecond);
        MergedCells.Draw(ACanvas);
      finally
        ACanvas.RestoreClipRegion;
      end;
      SelectionCell.Draw(ACanvas, dsSecond);
    end
    else
    begin
      ACanvas.SaveClipRegion;
      try
        Cells.Draw(ACanvas);
        MergedCells.Draw(ACanvas);
      finally
        ACanvas.RestoreClipRegion;
      end;
    end;

    ReferenceHighlighter.Draw(ACanvas, dsSecond);
    DrawSelection(ACanvas);
    CellsOverlay.Draw(ACanvas);
    Containers.Draw(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxSpreadSheetTableViewInfo.DrawPart(ACanvas: TcxCanvas; const AOffset: TPoint; const AClipRect: TRect);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(AClipRect);
    BackgroundCells.Draw(ACanvas);

    MoveWindowOrg(ACanvas.Handle, -AOffset.X, -AOffset.Y);
    try
      DrawCellsArea(ACanvas);
      GroupingAreas.Draw(ACanvas);
      ColumnsHeader.Draw(ACanvas);
      RowsHeader.Draw(ACanvas);
    finally
      MoveWindowOrg(ACanvas.Handle, AOffset.X, AOffset.Y);
    end;
  finally
    ACanvas.RestoreClipRegion;
    ACanvas.ExcludeClipRect(AClipRect);
  end;
end;

procedure TdxSpreadSheetTableViewInfo.DrawSelectedAreaFrame(ACanvas: TcxCanvas; const AArea: TRect);
begin
  dxSpreadSheetDrawBorders(ACanvas, GetAreaBounds(AArea), SelectionParams.Color, ContentParams.Color, sscbsMediumDashed);
end;

procedure TdxSpreadSheetTableViewInfo.DrawSelection(ACanvas: TcxCanvas);
begin
  if View.Controller.ForcedSelectionMode <> smNone then
    DrawSelectedAreaFrame(ACanvas, View.Selection.Area);
  if not SpreadSheet.ClipboardArea.IsEmpty and (View = SpreadSheet.ClipboardArea.View) then
    DrawSelectedAreaFrame(ACanvas, SpreadSheet.ClipboardArea.Area);
end;

function TdxSpreadSheetTableViewInfo.ContentRectToScreenRect(const R: TRect): TRect;
begin
  Result := R;
  if (FrozenColumn >= 0) and (Result.Left >= FrozenColumnSeparatorPosition) then
    OffsetRect(Result, -FirstScrollableColumnOffset, 0);
  if (FrozenRow >= 0) and (Result.Top >= FrozenRowSeparatorPosition) then
    OffsetRect(Result, 0, -FirstScrollableRowOffset);
end;

function TdxSpreadSheetTableViewInfo.GetCellClipArea(ARow, AColumn: Integer): TRect;
begin
  Result := CellsArea;
  if AColumn <= FrozenColumn then
    Result.Right := FrozenColumnSeparatorPosition + 1
  else
    if AColumn > FrozenColumn then
      Result.Left := FrozenColumnSeparatorPosition + OptionsView.FrozenPaneSeparatorWidth - 1;

  if ARow <= FrozenRow then
    Result.Bottom := FrozenRowSeparatorPosition + 1
  else
    if ARow > FrozenRow then
      Result.Top := FrozenRowSeparatorPosition + OptionsView.FrozenPaneSeparatorWidth - 1;
end;

procedure TdxSpreadSheetTableViewInfo.GetCellAtPoint(const P: TPoint;
  out ARow, AColumn: Integer; AReturnNearestCell: Boolean = False; AVisibleAreaOnly: Boolean = True);

  function FindItem(APos, AMinPos, AMaxPos, AFirstIndex, ALastIndex, AFixedPosition, AFixedOffset: Integer;
    ACells: TdxSpreadSheetCellViewInfoList; ACheckHorizontal: Boolean): Integer;
  var
    ACell: TdxSpreadSheetTableViewHeaderCellViewInfo;
    ACellPosition: Integer;
    ACellVisible: Boolean;
    I: Integer;
  begin
    Result := -1;
    if AReturnNearestCell then
      Result := AFirstIndex;
    if APos >= AMaxPos then
      Exit(ALastIndex);
    if APos > AMinPos then
    begin
      Result := TdxSpreadSheetTableViewHeaderCellViewInfo(ACells[ACells.Count - 1]).Index;
      if (AFixedPosition <> -1) and (APos > AFixedPosition + OptionsView.FrozenPaneSeparatorWidth - 1) then
        Inc(APos, AFixedOffset);
      for I := 0 to ACells.Count - 1 do
      begin
        ACell := TdxSpreadSheetTableViewHeaderCellViewInfo(ACells[I]);
        if ACheckHorizontal then
          ACellVisible := cxRectWidth(ACell.Bounds) > 0
        else
          ACellVisible := cxRectHeight(ACell.Bounds) > 0;

        if ACellVisible then
        begin
          if ACheckHorizontal then
            ACellPosition := ACell.Bounds.Left
          else
            ACellPosition := ACell.Bounds.Top;

          if APos < ACellPosition then
            Exit(TdxSpreadSheetTableViewHeaderCellViewInfo(ACells[I - 1]).Index);
        end;
      end;
    end;
  end;

begin
  ARow := -1;
  AColumn := -1;
  Validate;
  if AReturnNearestCell or not AVisibleAreaOnly or PtInRect(CellsArea, P) then
  begin
    AColumn := FindItem(P.X, CellsArea.Left, CellsArea.Right, View.LeftColumn,
      VisibleCells.Right, FrozenColumnSeparatorPosition, FirstScrollableColumnOffset, ColumnsHeader, True);
    ARow := FindItem(P.Y, CellsArea.Top, CellsArea.Bottom, View.TopRow,
      VisibleCells.Bottom, FrozenRowSeparatorPosition, FirstScrollableRowOffset, RowsHeader, False);
  end;
end;

function TdxSpreadSheetTableViewInfo.GetMergedCell(ARow, AColumn: Integer): TdxSpreadSheetMergedCell;
begin
  Result := View.MergedCells.FindCell(ARow, AColumn);
end;

function TdxSpreadSheetTableViewInfo.GetNeighborCellStyle(ARow, AColumn: Integer;
  ASide: TcxBorder; ADisplayStyle, ACheckVisibleNeighborsOnly: Boolean): TdxSpreadSheetCellStyleHandle;

  function IsNeighborVisible(const ARow, AColumn: Integer): Boolean;
  begin
    if ASide in [bLeft, bRight] then
      Result := View.Columns.GetItemVisible(AColumn)
    else
      Result := View.Rows.GetItemVisible(ARow);
  end;

  function FindNeighbor(var ARow, AColumn: Integer): Boolean;
  begin
    case ASide of
      bLeft:
        Dec(AColumn);
      bTop:
        Dec(ARow);
      bRight:
        Inc(AColumn);
    else
      Inc(ARow);
    end;
    Result := InRange(ARow, 0, dxSpreadSheetMaxRowIndex) and InRange(AColumn, 0, dxSpreadSheetMaxColumnIndex);
  end;

var
  ACellViewInfo: TdxSpreadSheetTableViewCellViewInfo;
begin
  while FindNeighbor(ARow, AColumn) do
  begin
    if not ACheckVisibleNeighborsOnly or IsNeighborVisible(ARow, AColumn) then
      Break;
  end;

  if ADisplayStyle and Cells.FindItemForCell(ARow, AColumn, ACellViewInfo) then
    Result := ACellViewInfo.Style.Handle
  else
    Result := View.GetCellStyleHandle(ARow, AColumn);
end;

function TdxSpreadSheetTableViewInfo.GetPartBounds(APart: Integer): TRect;
var
  AWidth: Integer;
begin
  Result := Bounds;
  AWidth := Max(0, OptionsView.FrozenPaneSeparatorWidth - 1);
  case APart of
    0:
      begin
        if FrozenColumn >= 0 then
          Result.Right := FrozenColumnSeparatorPosition + AWidth;
        if FrozenRow >= 0 then
          Result.Bottom := FrozenRowSeparatorPosition + AWidth;
      end;
    1:
      if FrozenRow >= 0 then
      begin
        Result.Top := FrozenRowSeparatorPosition + AWidth;
        if FrozenColumn >= 0 then
          Result.Right := FrozenColumnSeparatorPosition + AWidth
      end
      else
        Result := GetPartBounds(2);
    2:
      if FrozenColumn >= 0 then
      begin
        Result.Left := FrozenColumnSeparatorPosition + AWidth;
        if FrozenRow >= 0 then
          Result.Bottom := FrozenRowSeparatorPosition + AWidth;
      end;
    3:
      begin
        Result.Left := FrozenColumnSeparatorPosition + AWidth;
        Result.Top := FrozenRowSeparatorPosition + AWidth;
      end;
  end;
end;

function TdxSpreadSheetTableViewInfo.GetPartCount: Integer;
begin
  Result := 1 + Byte(FrozenColumn >= 0) + Byte(FrozenRow >= 0) + Byte((FrozenColumn >= 0) and (FrozenRow >= 0));
end;

function TdxSpreadSheetTableViewInfo.GetPartOffset(APart: Integer): TPoint;
begin
  Result := cxNullPoint;
  case APart of
    1:
     if FrozenRow >= 0 then
       Inc(Result.Y, FirstScrollableRowOffset)
     else
       Result := GetPartOffset(2);
    2:
     if FrozenColumn >= 0 then
       Inc(Result.X, FirstScrollableColumnOffset);
    3:
      Result := cxPointOffset(Result, FirstScrollableColumnOffset, FirstScrollableRowOffset);
  end;
end;

function TdxSpreadSheetTableViewInfo.GetPartOffsetByPoint(const P: TPoint): TPoint;

  function GetPartIndex(const P: TPoint): Integer;
  begin
    case GetPartCount of
      2:
        if FrozenColumn >= 0 then
          Result := Ord(P.X > GetPartBounds(0).Right)
        else
          Result := Ord(P.Y > GetPartBounds(0).Bottom);
      4:
        Result := Ord(P.Y > GetPartBounds(0).Bottom) + 2 * Ord(P.X > GetPartBounds(0).Right);
    else
      Result := 0;
    end;
  end;

begin
  Result := GetPartOffset(GetPartIndex(P));
end;

procedure TdxSpreadSheetTableViewInfo.InitScrollBarsParameters;
begin
  SetScrollBarInfo(sbVertical, 0, TotalRowCount, 1, VisibleRowCount, VScrollBarPos,
    Options.ActualVerticalScrollBar, Options.ActualVerticalScrollBar);
  SetScrollBarInfo(sbHorizontal, 0, TotalColumnCount, 1, VisibleColumnCount, HScrollBarPos,
    Options.ActualHorizontalScrollBar, Options.ActualHorizontalScrollBar);
end;

procedure TdxSpreadSheetTableViewInfo.MergeCellsBorders(AStartIndex, AFinishIndex: Integer);
var
  ACell: TdxSpreadSheetTableViewCellViewInfo;
  AIndex: Integer;
  ASide: TcxBorder;
begin
  for AIndex := AStartIndex to AFinishIndex do
  begin
    ACell := TdxSpreadSheetTableViewCellViewInfo(Cells.List[AIndex]);
    for ASide := Low(ASide) to High(ASide) do
      ACell.MergeBorder(ASide, GetNeighborCellStyle(ACell.Row, ACell.Column, ASide, True, True));
  end;
end;

procedure TdxSpreadSheetTableViewInfo.PostProcessRowCells(ARow: TdxSpreadSheetTableRow; AStartIndex, AFinishIndex: Integer);
var
  AIndex, I, APos: Integer;
  ACell: TdxSpreadSheetTableViewCellViewInfo;
begin
  if ARow <> nil then
    for AIndex := AStartIndex to AFinishIndex do
    begin
      ACell := TdxSpreadSheetTableViewCellViewInfo(Cells.List[AIndex]);
      if ACell.IsTextOutOfBounds then
      begin
        if ACell.TextBounds.Right > ACell.Bounds.Right - cxTextOffset then
        begin
          I := AIndex;
          APos := ACell.ContentBounds.Right;
          while RemoveNeighborCellBorder(bRight, I, AStartIndex, AFinishIndex, APos) do
            Inc(I);
          ACell.FContentBounds.Right := Min(ACell.ContentBounds.Right, APos);
        end;
        if ACell.TextBounds.Left < ACell.Bounds.Left + cxTextOffset + 1 then
        begin
          I := AIndex;
          APos := ACell.ContentBounds.Left;
          while RemoveNeighborCellBorder(bLeft, I, AStartIndex, AFinishIndex, APos) do
            Dec(I);
          ACell.FContentBounds.Left := Max(ACell.ContentBounds.Left, APos);
        end;
      end;
    end;
end;

procedure TdxSpreadSheetTableViewInfo.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if AScrollCode <> scEndScroll then
  begin
    DoScroll(AScrollBarKind, AScrollCode, AScrollPos);
    Validate;
  end;
end;

procedure TdxSpreadSheetTableViewInfo.SelectionChanged;
begin
  inherited SelectionChanged;

  RowsHeader.UpdateState;
  ColumnsHeader.UpdateState;
  Containers.UpdateState;
  CalculateSelection;
end;

procedure TdxSpreadSheetTableViewInfo.SetScrollBarInfo(AScrollBarKind: TScrollBarKind;
  AMin, AMax, AStep, APage, APos: Integer; AAllowShow, AAllowHide: Boolean);
begin
  SpreadSheet.SetScrollBarInfo(AScrollBarKind, AMin, AMax, AStep, APage, APos, AAllowShow, AAllowHide);
end;

procedure TdxSpreadSheetTableViewInfo.ValidateBounds(
  AStartIndex, AFinishIndex, AFixedIndex, ASeparatorPosition: Integer; var AStart, AFinish: Integer);
begin
  if AFixedIndex < 0 then Exit;
  if AFinishIndex > AFixedIndex then
    AFinish := Max(AFinish, ASeparatorPosition);
  if AStartIndex > AFixedIndex  then
    AStart := Max(AStart, ASeparatorPosition);
end;

procedure TdxSpreadSheetTableViewInfo.AddOutOfBoundsNonEmptyCell(
  ARow: TdxSpreadSheetTableRow; AIndex: Integer; const ABounds: TRect; AGoForward: Boolean);
var
  R: TRect;
  I: Integer;
begin
  if ARow = nil then
    Exit;
  if AGoForward then
  begin
    for I := AIndex + 1 to ARow.RowCells.LastIndex do
      if Columns.GetItemVisible(I) and ARow.IsValueDefined(I) then
      begin
        if not (ARow.Cells[I].IsMerged or ARow.IsValueDefined(I - 1)) then
          AddCell(ARow, ARow.Index, I, cxRectSetLeft(ABounds, ABounds.Right +
            Columns.GetDistance(AIndex + 1, I - 1), Columns.GetItemSize(I)));
        Break;
      end;
  end
  else
    for I := AIndex - 1 downto ARow.RowCells.FirstIndex do
      if Columns.GetItemVisible(I) and ARow.IsValueDefined(I) then
      begin
        if not (ARow.Cells[I].IsMerged or ARow.IsValueDefined(I + 1)) then
        begin
          R := cxRectOffset(ABounds, -Columns.GetDistance(I, AIndex - 1), 0);
          R := cxRectSetWidth(R, Columns.GetItemSize(I));
          AddCell(ARow, ARow.Index, I, R);
        end;
        Break;
      end;
end;

procedure TdxSpreadSheetTableViewInfo.CalculateLastItemIndex(
  ASize, AStartIndex, AFixedIndex, AFirstScrollableIndex, AMaxIndex: Integer;
  AItems: TdxSpreadSheetTableItems; var ASeparatorPosition, ALastIndex: Integer; ASeparatorPositionOffset: Integer);
var
  AItemSize: Integer;
begin
  ALastIndex := AStartIndex;
  ASeparatorPosition := 0;
  while (ASize > 0) and (ALastIndex < AMaxIndex) do
  begin
    AItemSize := AItems.GetItemSize(ALastIndex);
    if ALastIndex <= AFixedIndex then
      Inc(ASeparatorPosition, AItemSize);
    Dec(ASize, AItemSize);
    if AFixedIndex = ALastIndex then
    begin
      ALastIndex := AFirstScrollableIndex;
      Dec(ASize, OptionsView.FrozenPaneSeparatorWidth - 1);
    end
    else
      if ASize > 0 then
        Inc(ALastIndex);
  end;
  if AStartIndex > AFixedIndex then
    ASeparatorPosition := -1;
  if AFixedIndex >= 0 then
    Inc(ASeparatorPosition, ASeparatorPositionOffset);
end;

function TdxSpreadSheetTableViewInfo.CalculatePrevPageFirstIndex(APosition: Integer; AIsHorizontal: Boolean): Integer;
begin
  if AIsHorizontal then
    Result := CalculatePrevPageFirstIndex(APosition, cxRectWidth(ScrollableArea), FrozenColumn, Columns)
  else
    Result := CalculatePrevPageFirstIndex(APosition, cxRectHeight(ScrollableArea), FrozenRow, Rows)
end;

function TdxSpreadSheetTableViewInfo.CalculatePrevPageFirstIndex(
  APosition, ASize, AFixedIndex: Integer; AItems: TdxSpreadSheetTableItems): Integer;
begin
  Result := Max(AFixedIndex + 1, APosition);
  if Result = AFixedIndex + 1 then Exit;
  while (Result > AFixedIndex) and (ASize > 0) do
  begin
    Dec(ASize, AItems.GetItemSize(Result));
    if ASize > 0 then
      Dec(Result)
    else
      if ASize < 0 then
      begin
        Inc(Result);
        while AItems.GetItemSize(Result) <= 0 do
          Inc(Result);
      end;
  end;
  Result := Max(AFixedIndex + 1, Result);
end;

function TdxSpreadSheetTableViewInfo.GetColumns: TdxSpreadSheetTableColumns;
begin
  Result := View.Columns;
end;

function TdxSpreadSheetTableViewInfo.GetFirstScrollableColumn: Integer;
begin
  Result := VisibleCells.Left;
  if FrozenColumn >= 0 then
    Result := FFirstScrollableColumn;
end;

function TdxSpreadSheetTableViewInfo.GetFirstScrollableRow: Integer;
begin
  Result := VisibleCells.Top;
  if FrozenRow >= 0 then
    Result := FFirstScrollableRow;
end;

function TdxSpreadSheetTableViewInfo.GetFrozenColumn: Integer;
begin
  Result := View.FrozenColumn;
end;

function TdxSpreadSheetTableViewInfo.GetFrozenRow: Integer;
begin
  Result := View.FrozenRow;
end;

function TdxSpreadSheetTableViewInfo.GetFocusedCell: TdxSpreadSheetTableViewCellViewInfo;
begin
  Validate;
  Result := FFocusedCell;
end;

function TdxSpreadSheetTableViewInfo.GetHScrollBarPos: Integer;
begin
  Result := FirstScrollableColumn - (FrozenColumn + 1);
end;

function TdxSpreadSheetTableViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := View.SpreadSheet.LookAndFeelPainter;
end;

function TdxSpreadSheetTableViewInfo.GetOrigin: TPoint;
begin
  Result := cxPoint(FirstColumnOrigin, FirstRowOrigin);
end;

function TdxSpreadSheetTableViewInfo.GetOptions: TdxSpreadSheetTableViewOptions;
begin
  Result := View.Options;
end;

function TdxSpreadSheetTableViewInfo.GetOptionsView: TdxSpreadSheetOptionsView;
begin
  Result := SpreadSheet.OptionsView;
end;

function TdxSpreadSheetTableViewInfo.GetPainter: TdxSpreadSheetTableViewPainter;
begin
  Result := TdxSpreadSheetTableViewPainter(inherited Painter);
end;

function TdxSpreadSheetTableViewInfo.GetReferenceHighlighter: TdxSpreadSheetCustomReferenceHighlighter;
begin
  Result := View.EditingController.ReferenceHighlighter;
end;

function TdxSpreadSheetTableViewInfo.GetRows: TdxSpreadSheetTableRows;
begin
  Result := View.Rows;
end;

function TdxSpreadSheetTableViewInfo.GetStyles: TdxSpreadSheetStyles;
begin
  Result := View.SpreadSheet.Styles;
end;

function TdxSpreadSheetTableViewInfo.GetTotalColumnCount: Integer;
begin
  Result := HScrollBarPos + VisibleColumnCount;
  if HScrollBarPos > 0 then
    Dec(Result);
  Result := Max(Result, Columns.LastIndex);
end;

function TdxSpreadSheetTableViewInfo.GetTotalRowCount: Integer;
begin
  Result := VScrollBarPos + VisibleRowCount;
  if VScrollBarPos > 0 then
    Dec(Result);
  Result := Max(Result, Rows.LastIndex);
end;

function TdxSpreadSheetTableViewInfo.GetView: TdxSpreadSheetTableView;
begin
  Result := TdxSpreadSheetTableView(inherited View);
end;

function TdxSpreadSheetTableViewInfo.GetVScrollBarPos: Integer;
begin
  Result := FirstScrollableRow - (FrozenRow + 1)
end;

function TdxSpreadSheetTableViewInfo.RemoveNeighborCellBorder(ASide: TcxBorder;
  AIndex, AStartIndex, AFinishIndex: Integer; var AEndPos: Integer): Boolean;
var
  AHasValue: Boolean;
  ACell, ANeighborCell: TdxSpreadSheetTableViewCellViewInfo;
begin
  ACell := TdxSpreadSheetTableViewCellViewInfo(Cells[AIndex]);
  Result := False;
  AHasValue := False;
  if (ASide = bRight) and (AIndex < AFinishIndex) then
  begin
    ANeighborCell := TdxSpreadSheetTableViewCellViewInfo(Cells[AIndex + 1]);
    AHasValue := ANeighborCell.HasValue;
    Result := not AHasValue;
    if Result then
    begin
      ANeighborCell.RemoveBorder(bLeft);
      Result := AEndPos > ANeighborCell.ContentBounds.Right;
    end
    else
      AEndPos := ACell.Bounds.Right - cxTextOffset;
  end
  else
    if (ASide = bLeft) and (AIndex > AStartIndex) then
    begin
      ANeighborCell := TdxSpreadSheetTableViewCellViewInfo(Cells[AIndex - 1]);
      AHasValue := ANeighborCell.HasValue;
      Result := not AHasValue;
      if Result then
      begin
        ANeighborCell.RemoveBorder(bRight);
        Result := AEndPos < ANeighborCell.ContentBounds.Left;
      end
      else
        AEndPos := ACell.Bounds.Left + cxTextOffset + 1;
    end
    else
      AEndPos := cxRectGetItem(TdxSpreadSheetTableViewCellViewInfo(Cells[AIndex]).ContentBounds, Integer(ASide));

  if not AHasValue then
    ACell.RemoveBorder(ASide);
end;

procedure TdxSpreadSheetTableViewInfo.SetFirstScrollableColumn(AValue: Integer);
var
  APrevValue: Integer;
begin
  APrevValue := FirstScrollableColumn;
  dxSpreadSheetValidate(AValue, FrozenColumn + 1, dxSpreadSheetMaxColumnIndex - (FrozenColumn + 1));
  if FrozenColumn < 0 then
    FVisibleCells.Left := AValue
  else
    FFirstScrollableColumn := Max(AValue, FrozenColumn + 1);

  IsDirty := IsDirty or (APrevValue <> FirstScrollableColumn);
  if IsDirty then
    SpreadSheet.DoScroll(sbHorizontal, FirstScrollableColumn);
end;

procedure TdxSpreadSheetTableViewInfo.SetFirstScrollableRow(AValue: Integer);
var
  APrevValue: Integer;
begin
  APrevValue := FirstScrollableRow;
  dxSpreadSheetValidate(AValue, FrozenRow + 1, dxSpreadSheetMaxRowIndex - (FrozenRow + 1));
  if FrozenRow < 0 then
    FVisibleCells.Top := AValue
  else
    FFirstScrollableRow := Max(AValue, FrozenRow + 1);

  IsDirty := IsDirty or (APrevValue <> FirstScrollableRow);
  if IsDirty then
    SpreadSheet.DoScroll(sbVertical, FirstScrollableRow);
end;

{ TdxSpreadSheetDefinedName }

destructor TdxSpreadSheetDefinedName.Destroy;
begin
  SpreadSheet.FormulaController.ClearFormulasResults;
  Formula := nil;
  inherited Destroy;
end;

procedure TdxSpreadSheetDefinedName.EnumReferences(AProc: TdxSpreadSheetFormulaEnumReferencesProc);
begin
  if Formula <> nil then
    Formula.EnumReferences(AProc);
end;

function TdxSpreadSheetDefinedName.ToString: string;
begin
  Result := Caption;
  if Scope <> nil then
    Result := Scope.Caption + '!' + Result;
  if Pos(' ', Result) > 0 then
    Result := '''' + Result + '''';
end;

procedure TdxSpreadSheetDefinedName.LoadFromStream(AReader: TcxReader);
begin
  if not AReader.ReadBoolean then
    Formula := nil
  else
    if AReader.Version >= 4 then
      Reference := AReader.ReadWideString
    else
      raise EdxSpreadSheetFormatError.Create(ClassName);
end;

procedure TdxSpreadSheetDefinedName.SaveToStream(AWriter: TcxWriter);
begin
  AWriter.WriteWideString(Caption);
  if Scope <> nil then
    AWriter.WriteInteger(Scope.Index)
  else
    AWriter.WriteInteger(-1);

  AWriter.WriteBoolean(Formula <> nil);
  if Formula <> nil then
    AWriter.WriteWideString(Reference);
end;

procedure TdxSpreadSheetDefinedName.UpdateReference;
var
  AReference: string;
begin
  AReference := Reference;
  FreeAndNil(FFormula);
  Reference := AReference;
end;

procedure TdxSpreadSheetDefinedName.ClearResult;
begin
  if Formula <> nil then
    Formula.ClearResult;
end;

function TdxSpreadSheetDefinedName.Compare(ACandidate: TdxSpreadSheetDefinedName): Integer;
begin
  if Self = ACandidate then
    Result := 0
  else
    Result := CompareEx(ACandidate.Caption, ACandidate.Scope);
end;

function TdxSpreadSheetDefinedName.CompareEx(const ACaption: string;
  AScope: TdxSpreadSheetCustomView; AScopeMaybeEmpty: Boolean = False): Integer;
begin
  Result := dxSpreadSheetCompareText(Caption, ACaption);
  if Result = 0 then
  begin
    if (Scope <> nil) and (AScope <> nil) then
      Result := Scope.Index - AScope.Index
    else
      Result := dxCompareValues(Scope, AScope);

    if (Result <> 0) and AScopeMaybeEmpty and (AScope = nil) then
      Result := 0;
  end;
end;

procedure TdxSpreadSheetDefinedName.Initialize(const ACaption, AReference: string;
  AScope: TdxSpreadSheetCustomView);
begin
  FCaption := ACaption;
  FScope := AScope;
  Reference := AReference;
end;

function TdxSpreadSheetDefinedName.IsCellReference: Boolean;

  procedure CheckToken(AToken: TdxSpreadSheetFormulaToken);
  begin
    while Result and (AToken <> nil) do
    begin
      Result :=
        (AToken.ClassType = TdxSpreadSheetFormulaToken) or
        (AToken is TdxSpreadSheetFormulaParenthesesToken) or
        (AToken is TdxSpreadSheetFormulaReference);

      CheckToken(AToken.FirstChild);
      if Result then
        AToken := AToken.Next;
    end;
  end;

begin
  Result := Formula <> nil;
  if Result then
    CheckToken(Formula.Tokens);
end;

function TdxSpreadSheetDefinedName.IsCellReference(out AArea: TRect; out AView: TdxSpreadSheetCustomView): Boolean;
var
  ATempArea: TRect;
  ATempView: TdxSpreadSheetCustomView;
begin
  Result := IsCellReference;
  if Result then
  begin
    ATempView := nil;
    ATempArea := cxInvalidRect;
    EnumReferences(
      procedure (const AArea: TRect; AView: TObject)
      begin
        ATempArea := AArea;
        ATempView := AView as TdxSpreadSheetCustomView;
      end);
    Result := (ATempView <> nil) and dxSpreadSheetIsValidArea(ATempArea);
  end;
end;

function TdxSpreadSheetDefinedName.GetIndex: Integer;
begin
  Result := Owner.ItemList.IndexOf(Self);
end;

function TdxSpreadSheetDefinedName.GetReference: string;
begin
  if Formula = nil then
    Result := ''
  else
    Result := Formula.AsText;
end;

function TdxSpreadSheetDefinedName.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := Owner.SpreadSheet;
end;

function TdxSpreadSheetDefinedName.GetValueAsText: string;
begin
  Result := VarToStr(Formula.Value);
end;

procedure TdxSpreadSheetDefinedName.SetCaption(const AValue: string);
begin
  if not dxSpreadSheetTextIsEqual(AValue, FCaption) then
  begin
    SpreadSheet.FormulaController.DoNameChanged(Self, AValue);
    FCaption := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetDefinedName.SetFormula(const AValue: TdxSpreadSheetDefinedNameFormula);
begin
  if FFormula <> AValue then
  begin
    FreeAndNil(FFormula);
    FFormula := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetDefinedName.SetReference(const AValue: string);
var
  AFormula: TdxSpreadSheetDefinedNameFormula;
begin
  if not dxSpreadSheetTextIsEqual(AValue, Reference) then
  begin
    AFormula := TdxSpreadSheetDefinedNameFormula.Create(Self);
    dxSpreadSheetParseReference(AValue, AFormula);
    Formula := AFormula;
  end;
end;

procedure TdxSpreadSheetDefinedName.SetScope(const AValue: TdxSpreadSheetCustomView);
begin
  FScope := AValue;
end;

{ TdxSpreadSheetDefinedNames }

function TdxSpreadSheetDefinedNames.Add(const ACaption, AReference: string;
  AScope: TdxSpreadSheetCustomView = nil): TdxSpreadSheetDefinedName;
begin
  if Find(ACaption, AScope, False) <> -1 then
    raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorDefinedNameAlreadyExists), [ACaption]);
  Result := CreateItem as TdxSpreadSheetDefinedName;
  Result.Initialize(ACaption, AReference, AScope);
end;

function TdxSpreadSheetDefinedNames.AddFromStream(AReader: TcxReader): TdxSpreadSheetDefinedName;
var
  ACaption: string;
  AScope: TdxSpreadSheetCustomView;
  AScopeIndex: Integer;
begin
  ACaption := AReader.ReadWideString;
  AScopeIndex := AReader.ReadInteger;
  if AScopeIndex >= 0 then
    AScope := SpreadSheet.Sheets[AScopeIndex]
  else
    AScope := nil;

  Result := Add(ACaption, '', AScope);
  Result.LoadFromStream(AReader);
end;

function TdxSpreadSheetDefinedNames.AddOrSet(const ACaption, AReference: string;
  AScope: TdxSpreadSheetCustomView = nil): TdxSpreadSheetDefinedName;
begin
  Result := GetItemByName(ACaption, AScope);
  if Result = nil then
    Result := CreateItem as TdxSpreadSheetDefinedName;
  Result.Initialize(ACaption, AReference, AScope);
end;

function TdxSpreadSheetDefinedNames.Contains(const ACaption: string; AScope: TdxSpreadSheetCustomView): Boolean;
begin
  Result := IndexOf(ACaption, AScope) >= 0;
end;

function TdxSpreadSheetDefinedNames.GetItemByName(const ACaption: string; AScope: TdxSpreadSheetCustomView): TdxSpreadSheetDefinedName;
var
  AIndex: Integer;
begin
  AIndex := IndexOf(ACaption, AScope);
  if AIndex >= 0 then
    Result := Items[AIndex]
  else
    Result := nil;
end;

function TdxSpreadSheetDefinedNames.IndexOf(const ACaption: string): Integer;
begin
  Result := Find(ACaption, nil, False);
  if Result < 0 then
    Result := Find(ACaption, nil, True);
end;

function TdxSpreadSheetDefinedNames.IndexOf(const ACaption: string; AScope: TdxSpreadSheetCustomView): Integer;
begin
  Result := Find(ACaption, AScope, False);
end;

procedure TdxSpreadSheetDefinedNames.AfterLoad;
var
  I: Integer;
begin
  SpreadSheet.BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].UpdateReference;
  finally
    SpreadSheet.EndUpdate;
  end;
end;

procedure TdxSpreadSheetDefinedNames.Changed;
begin
  SpreadSheet.DefinedNamesChanged;
end;

procedure TdxSpreadSheetDefinedNames.CheckSorted;
begin
  if Sorted then Exit;
  ItemList.Sort(@CompareNames);
  Sorted := True;
end;

function TdxSpreadSheetDefinedNames.CreateItem: TdxSpreadSheetObjectListItem;
begin
  Result := TdxSpreadSheetDefinedName.Create(Self);
  Sorted := False;
end;

function TdxSpreadSheetDefinedNames.Find(const ACaption: string;
  AScope: TdxSpreadSheetCustomView; AScopeMaybeEmpty: Boolean): Integer;
var
  L, H, I, C: Integer;
  AItem: TdxSpreadSheetDefinedName;
begin
  Result := -1;
  CheckSorted;
  if Count = 0 then Exit;
  L := 0;
  H := ItemList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    AItem := TdxSpreadSheetDefinedName(ItemList.List[I]);
    C := AItem.CompareEx(ACaption, AScope, AScopeMaybeEmpty);
    if C = 0 then
    begin
      Result := I;
      Exit;
    end
    else
      if C < 0 then
        L := I + 1
      else
        H := I - 1;
  end;
end;

procedure TdxSpreadSheetDefinedNames.OnRemoveSheet(AScope: TdxSpreadSheetCustomView);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Scope = AScope then
      Items[I].Scope := nil;
end;

function TdxSpreadSheetDefinedNames.GetName(AIndex: Integer): TdxSpreadSheetDefinedName;
begin
  Result := TdxSpreadSheetDefinedName(inherited Items[AIndex]);
end;

class function TdxSpreadSheetDefinedNames.CompareNames(AName1, AName2: TdxSpreadSheetDefinedName): Integer;
begin
  Result := AName1.Compare(AName2);
end;

{ TdxSpreadSheetExternalLink }

destructor TdxSpreadSheetExternalLink.Destroy;
begin
  cxClearObjectLinks(Self);
  inherited;
end;

function TdxSpreadSheetExternalLink.GetActualTarget: string;
begin
  Result := Target;
end;

{ TdxSpreadSheetExternalLinks }

function TdxSpreadSheetExternalLinks.Add(const ATarget: string): TdxSpreadSheetExternalLink;
begin
  Result := TdxSpreadSheetExternalLink(CreateItem);
  Result.FTarget := ATarget;
end;

function TdxSpreadSheetExternalLinks.GetLinkByTarget(const ATarget: string): TdxSpreadSheetExternalLink;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if dxSpreadSheetTextIsEqual(Result.Target, ATarget) then Exit;
  end;
  Result := nil;
end;

function TdxSpreadSheetExternalLinks.CreateItem: TdxSpreadSheetObjectListItem;
begin
  Result := TdxSpreadSheetExternalLink.Create(Self);
end;

function TdxSpreadSheetExternalLinks.GetItem(AIndex: Integer): TdxSpreadSheetExternalLink;
begin
  Result := TdxSpreadSheetExternalLink(inherited Items[AIndex]);
end;

{ TdxSpreadSheetCustomFilerProgressHelper }

constructor TdxSpreadSheetCustomFilerProgressHelper.Create(AOwner: TdxSpreadSheetCustomFiler; AStageCount: Integer);
begin
  inherited Create(AStageCount);
  FOwner := AOwner;
end;

procedure TdxSpreadSheetCustomFilerProgressHelper.ProgressChanged;
begin
  FOwner.DoProgress(Progress);
end;

{ TdxSpreadSheetCustomFiler }

constructor TdxSpreadSheetCustomFiler.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner);
  FStream := AStream;
  FIgnoreMessages := [ssmtWarning];
  FProgressHelper := CreateProgressHelper;
  dxGetLocaleFormatSettings(dxGetInvariantLocaleID, FInvariantFormatSettings);
end;

destructor TdxSpreadSheetCustomFiler.Destroy;
begin
  FreeAndNil(FProgressHelper);
  if StreamAutoFree then
    FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomFiler.DoError(const AMessage: string; AType: TdxSpreadSheetMessageType);
begin
  if not (AType in IgnoreMessages) then
    raise EdxSpreadSheetReaderError.Create(AMessage);
end;

procedure TdxSpreadSheetCustomFiler.DoError(
  const AFormatString: string; const AArguments: array of const; AType: TdxSpreadSheetMessageType);
begin
  DoError(Format(AFormatString, AArguments), AType);
end;

procedure TdxSpreadSheetCustomFiler.DoProgress(AProgress: Integer);
begin
  if Assigned(OnProgress) then
    OnProgress(SpreadSheet, AProgress);
end;

procedure TdxSpreadSheetCustomFiler.ExecuteSubTask(ASubTask: TdxSpreadSheetCustomFilerSubTask);
begin
  try
    ASubTask.Execute;
  finally
    ASubTask.Free;
  end;
end;

{ TdxSpreadSheetCustomFilerSubTask }

constructor TdxSpreadSheetCustomFilerSubTask.Create(AOwner: TdxSpreadSheetCustomFiler);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxSpreadSheetCustomFilerSubTask.DoError(
  const AFormatString: string; const AArguments: array of const; AType: TdxSpreadSheetMessageType);
begin
  Owner.DoError(AFormatString, AArguments, AType);
end;

procedure TdxSpreadSheetCustomFilerSubTask.DoError(const AMessage: string; AType: TdxSpreadSheetMessageType);
begin
  Owner.DoError(AMessage, AType);
end;

procedure TdxSpreadSheetCustomFilerSubTask.ExecuteSubTask(ASubTask: TdxSpreadSheetCustomFilerSubTask);
begin
  Owner.ExecuteSubTask(ASubTask);
end;

function TdxSpreadSheetCustomFilerSubTask.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := Owner.SpreadSheet;
end;

{ TdxSpreadSheetCustomReader }

function TdxSpreadSheetCustomReader.AddBorders(ABordersHandle: TdxSpreadSheetBordersHandle): TdxSpreadSheetBordersHandle;
begin
  Result := CellStyles.Borders.AddBorders(ABordersHandle);
end;

function TdxSpreadSheetCustomReader.AddCellStyle(AStyleHandle: TdxSpreadSheetCellStyleHandle): TdxSpreadSheetCellStyleHandle;
begin
  Result := CellStyles.AddStyle(AStyleHandle);
end;

function TdxSpreadSheetCustomReader.AddBrush(ABrushHandle: TdxSpreadSheetBrushHandle): TdxSpreadSheetBrushHandle;
begin
  Result := CellStyles.Brushes.AddBrush(ABrushHandle);
end;

function TdxSpreadSheetCustomReader.AddFont(AFontHandle: TdxSpreadSheetFontHandle): TdxSpreadSheetFontHandle;
begin
  Result := CellStyles.Fonts.AddFont(AFontHandle);
end;

function TdxSpreadSheetCustomReader.AddFormattedSharedString(S: TdxSpreadSheetFormattedSharedString): TdxSpreadSheetFormattedSharedString;
begin
  Result := TdxSpreadSheetFormattedSharedString(SpreadSheet.StringTable.Add(S));
end;

function TdxSpreadSheetCustomReader.AddImage(const AStream: TStream): TdxSpreadSheetSharedImageHandle;
begin
  Result := SpreadSheet.SharedImages.Add(AStream);
end;

function TdxSpreadSheetCustomReader.AddNumberFormat(
  const AFormatCode: string; ID: Integer = -1): TdxSpreadSheetFormatHandle;
begin
  if (ID >= 0) and (AFormatCode = '') then
    Result := CellStyles.Formats.PredefinedFormats.GetFormatHandleByID(ID)
  else
    Result := nil;

  if Result = nil then
  begin
    if ID < 0 then
      ID := CellStyles.Formats.PredefinedFormats.GetIDByFormatCode(AFormatCode);
    Result := CellStyles.Formats.AddFormat(AFormatCode, ID);
  end;
end;

function TdxSpreadSheetCustomReader.AddSharedString(const S: string): TdxSpreadSheetSharedString;
begin
  Result := SpreadSheet.StringTable.Add(S);
end;

function TdxSpreadSheetCustomReader.AddTableView(const ACaption: string): TdxSpreadSheetTableView;
begin
  Result := TdxSpreadSheetTableView(SpreadSheet.AddSheet(ACaption, TdxSpreadSheetTableView));
end;

procedure TdxSpreadSheetCustomReader.Check(AValue: Boolean;
  const AMessage: string; AMessageType: TdxSpreadSheetMessageType = ssmtError);
begin
  if not AValue then
    DoError(AMessage, AMessageType);
end;

function TdxSpreadSheetCustomReader.CreateTempBordersHandle: TdxSpreadSheetBordersHandle;
begin
  Result := CellStyles.Borders.CreateBorders;
end;

function TdxSpreadSheetCustomReader.CreateTempCellStyle(
  AFont: TdxSpreadSheetFontHandle; AFormat: TdxSpreadSheetFormatHandle;
  AFill: TdxSpreadSheetBrushHandle; ABorders: TdxSpreadSheetBordersHandle): TdxSpreadSheetCellStyleHandle;
begin
  Result := CellStyles.CreateStyle(AFont, AFormat, AFill, ABorders);
end;

function TdxSpreadSheetCustomReader.CreateTempBrushHandle: TdxSpreadSheetBrushHandle;
begin
  Result := CellStyles.Brushes.CreateBrush;
end;

function TdxSpreadSheetCustomReader.CreateTempFontHandle: TdxSpreadSheetFontHandle;
begin
  Result := CellStyles.Fonts.CreateFont;
end;

function TdxSpreadSheetCustomReader.CreateTempFormattedSharedString(const S: string): TdxSpreadSheetFormattedSharedString;
begin
  Result := TdxSpreadSheetFormattedSharedString.CreateObject(S);
end;

function TdxSpreadSheetCustomReader.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := SpreadSheet.CellStyles;
end;

function TdxSpreadSheetCustomReader.GetStringTable: TdxSpreadSheetSharedStringTable;
begin
  Result := SpreadSheet.StringTable;
end;

{ TdxSpreadSheetCustomFormat }

class function TdxSpreadSheetCustomFormat.CanCheckByContent: Boolean;
begin
  Result := True;
end;

class function TdxSpreadSheetCustomFormat.CanReadFromStream(AStream: TStream): Boolean;
begin
  Result := False;
end;

class function TdxSpreadSheetCustomFormat.CreateFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := nil;
end;

class function TdxSpreadSheetCustomFormat.CreateReader(
  ASpreadSheet: TdxCustomSpreadSheet; AStream: TStream): TdxSpreadSheetCustomReader;
begin
  Result := GetReader.Create(ASpreadSheet, AStream);
end;

class function TdxSpreadSheetCustomFormat.CreateWriter(
  ASpreadSheet: TdxCustomSpreadSheet; AStream: TStream): TdxSpreadSheetCustomWriter;
begin
  Result := GetWriter.Create(ASpreadSheet, AStream);
end;

class function TdxSpreadSheetCustomFormat.GetDescription: string;
begin
  Result := '';
end;

class function TdxSpreadSheetCustomFormat.GetExt: string;
begin
  Result := '';
end;

class function TdxSpreadSheetCustomFormat.GetReader: TdxSpreadSheetCustomReaderClass;
begin
  Result := nil;
end;

class function TdxSpreadSheetCustomFormat.GetWriter: TdxSpreadSheetCustomWriterClass;
begin
  Result := nil;
end;

class procedure TdxSpreadSheetCustomFormat.Register;
begin
  dxSpreadSheetFormatsRepository.Register(Self);
end;

class procedure TdxSpreadSheetCustomFormat.Unregister;
begin
  if FSpreadSheetFormats <> nil then
    dxSpreadSheetFormatsRepository.Unregister(Self);
end;

{ TdxSpreadSheetFormatsRepository }

constructor TdxSpreadSheetFormatsRepository.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TdxSpreadSheetFormatsRepository.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxSpreadSheetFormatsRepository.Find(const AFileName: string; out AFormat: TdxSpreadSheetCustomFormatClass): Boolean;
var
  AExt: string;
  I: Integer;
begin
  Result := False;
  AExt := ExtractFileExt(AFileName);
  for I := 0 to Count - 1 do
    if SameText(AExt, Items[I].GetExt) then
    begin
      AFormat := Items[I];
      Result := True;
      Break;
    end;
end;

function TdxSpreadSheetFormatsRepository.GetOpenDialogFilter: string;
var
  AExts: string;
  AItem: TdxSpreadSheetCustomFormatClass;
  I: Integer;
begin
  Result := '';
  AExts := '';
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if (AItem.GetReader <> nil) and (AItem.GetDescription <> '') then
    begin
      Result := Result + Format('%0:s (*%1:s)|*%1:s|', [AItem.GetDescription, AItem.GetExt]);
      AExts := AExts + '*' + AItem.GetExt + ';';
    end;
  end;
  Result := cxGetResourceString(@sdxFileDialogAllSupported) + '|' + AExts + '|' + Result;
end;

function TdxSpreadSheetFormatsRepository.GetSaveDialogFilter: string;
var
  AItem: TdxSpreadSheetCustomFormatClass;
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if (AItem.GetWriter <> nil) and (AItem.GetDescription <> '') then
      Result := Result + Format('%0:s (*%1:s)|*%1:s|', [AItem.GetDescription, AItem.GetExt]);
  end;
end;

procedure TdxSpreadSheetFormatsRepository.Register(AFormat: TdxSpreadSheetCustomFormatClass);
begin
  FList.Add(AFormat);
end;

procedure TdxSpreadSheetFormatsRepository.Unregister(AFormat: TdxSpreadSheetCustomFormatClass);
begin
  FList.Remove(AFormat)
end;

function TdxSpreadSheetFormatsRepository.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxSpreadSheetFormatsRepository.GetDescriptions(Index: Integer): string;
begin
  Result := Items[Index].GetDescription;
end;

function TdxSpreadSheetFormatsRepository.GetExt(Index: Integer): string;
begin
  Result := Items[Index].GetExt;
end;

function TdxSpreadSheetFormatsRepository.GetItem(Index: Integer): TdxSpreadSheetCustomFormatClass;
begin
  Result := TdxSpreadSheetCustomFormatClass(FList[Index]);
end;

{ TdxSpreadSheetCustomViewController }

procedure TdxSpreadSheetCustomViewController.CheckScrollArea(const P: TPoint);
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomViewController.CheckScrollArea(X, Y: Integer);
begin
  CheckScrollArea(cxPoint(X, Y));
end;

function TdxSpreadSheetCustomViewController.ContainerCanDelete: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetCustomViewController.ContainerProcessKeyDown(Key: Word; Shift: TShiftState): Boolean;

  function FindNextContainerForFocusing(AShift: TShiftState; AContainer: TdxSpreadSheetContainer): TdxSpreadSheetContainer;
  begin
    Result := AContainer;
    repeat
      if ssShift in AShift then
        Result := Containers.GetPrevVisibleContainer(Result)
      else
        Result := Containers.GetNextVisibleContainer(Result);

    until (Result = nil) or Result.CanFocusViaKeyboard;
  end;

var
  AContainer: TdxSpreadSheetContainer;
begin
  Result := True;
  case Key of
    VK_ESCAPE:
      FocusedContainer := nil;

    VK_DELETE:
      if ContainerCanDelete then
        FocusedContainer.Free;

    VK_TAB:
      begin
        AContainer := FindNextContainerForFocusing(Shift, FocusedContainer);
        if AContainer = nil then
        begin
          if ssShift in Shift then
            AContainer := Containers.GetLastVisibleContainer
          else
            AContainer := Containers.GetFirstVisibleContainer;

          if (AContainer <> nil) and not AContainer.CanFocusViaKeyboard then
            AContainer := FindNextContainerForFocusing(Shift, AContainer);
        end;
        if FocusedContainer <> AContainer then
          FocusContainer(AContainer, True);
      end;

    VK_LEFT, VK_RIGHT:
      if [ssAlt, ssShift] * Shift = [ssAlt] then
        RotateContainer(FocusedContainer, ValueIncr[Key = VK_RIGHT] * 15)
      else
        if [ssAlt, ssShift] * Shift = [ssShift] then
          ResizeContainer(FocusedContainer, ValueIncr[Key = VK_RIGHT], 0)
        else
          if [ssAlt, ssShift] * Shift = [] then
            MoveContainer(FocusedContainer, ValueIncr[Key = VK_RIGHT], 0);

    VK_UP, VK_DOWN:
      if [ssAlt, ssShift] * Shift = [ssShift] then
        ResizeContainer(FocusedContainer, 0, ValueIncr[Key = VK_DOWN])
      else
        if [ssAlt, ssShift] * Shift = [] then
          MoveContainer(FocusedContainer, 0, ValueIncr[Key = VK_DOWN]);
  else
    Result := False;
  end;
end;

procedure TdxSpreadSheetCustomViewController.FocusContainer(AContainer: TdxSpreadSheetContainer; AMakeVisible: Boolean);
begin
  FocusedContainer := AContainer;
end;

procedure TdxSpreadSheetCustomViewController.MoveContainer(AContainer: TdxSpreadSheetContainer; ADeltaX, ADeltaY: Integer);
begin
  if AContainer.CanMove then
  begin
    AContainer.BeginChanging;
    try
      AContainer.Calculator.ModifyBounds(ADeltaX, ADeltaY, ADeltaX, ADeltaY);
    finally
      AContainer.EndChanging;
    end;
  end;
end;

procedure TdxSpreadSheetCustomViewController.ResizeContainer(AContainer: TdxSpreadSheetContainer; ADeltaX, ADeltaY: Integer);
begin
  if AContainer.CanResize then
  begin
    AContainer.BeginChanging;
    try
      AContainer.Calculator.Resize(ADeltaX, ADeltaY);
    finally
      AContainer.EndChanging;
    end;
  end;
end;

procedure TdxSpreadSheetCustomViewController.RotateContainer(AContainer: TdxSpreadSheetContainer; ADelta: Single);
begin
  if AContainer.CanRotate then
  begin
    AContainer.BeginChanging;
    try
      AContainer.Transform.RotationAngle := AContainer.Transform.RotationAngle + ADelta;
    finally
      AContainer.EndChanging;
    end;
  end;
end;

function TdxSpreadSheetCustomViewController.GetContainers: TdxSpreadSheetContainers;
begin
  Result := View.Containers;
end;

function TdxSpreadSheetCustomViewController.GetHitTest: TdxSpreadSheetCustomHitTest;
begin
  Result := View.HitTest;
end;

function TdxSpreadSheetCustomViewController.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := View.SpreadSheet;
end;

function TdxSpreadSheetCustomViewController.GetView: TdxSpreadSheetCustomView;
begin
  Result := TdxSpreadSheetCustomView(Owner);
end;

procedure TdxSpreadSheetCustomViewController.GetRedoActionCount(var ARedoCount: Integer);
begin
end;

procedure TdxSpreadSheetCustomViewController.GetUndoActionCount(var AUndoCount: Integer);
begin
end;

function TdxSpreadSheetCustomViewController.GetZoomFactor: Integer;
begin
  Result := View.ZoomFactor;
end;

function TdxSpreadSheetCustomViewController.Redo(const ARedoCount: Integer = 1): Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCustomViewController.Undo(const AUndoCount: Integer = 1): Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetCustomViewController.SetFocusedContainer(AValue: TdxSpreadSheetContainer);
var
  ATempValue: TdxSpreadSheetContainer;
begin
  if (AValue <> nil) and (AValue.Parent <> View) then
    Exit;
  if FFocusedContainer <> AValue then
  begin
    if FFocusedContainer <> nil then
    begin
      ATempValue := FFocusedContainer;
      FFocusedContainer := nil;
      if FFocusedContainerWasHidden then
        ATempValue.Visible := False;
    end;
    if AValue <> nil then
    begin
      FFocusedContainer := AValue;
      FFocusedContainerWasHidden := not FFocusedContainer.Visible;
      FFocusedContainer.Visible := True;
    end;
    if not View.IsLocked then
      View.SelectionChanged;
  end;
end;

{ TdxSpreadSheetCustomHitTest }

constructor TdxSpreadSheetCustomHitTest.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;

procedure TdxSpreadSheetCustomHitTest.Calculate(const AHitPoint: TPoint);
begin
  Clear;
  FHitPoint := AHitPoint;
  FActualHitPoint := GetActualHitPoint;
end;

procedure TdxSpreadSheetCustomHitTest.Clear;
begin
  FHitCode := 0;
  FHitObjectData := 0;
  FHitObject := nil;
end;

function TdxSpreadSheetCustomHitTest.GetPopupMenuClass(const P: TPoint): TComponentClass;
begin
  Calculate(P);
  if HitObject <> nil then
    Result := HitObject.GetPopupMenuClass(Self)
  else
    Result := nil;
end;

procedure TdxSpreadSheetCustomHitTest.Recalculate;
begin
  Calculate(HitPoint);
end;

function TdxSpreadSheetCustomHitTest.GetActualHitPoint: TPoint;
begin
  Result := FHitPoint;
end;

function TdxSpreadSheetCustomHitTest.GetCursor(const P: TPoint): TCursor;
begin
  Calculate(P);
  if HitObject <> nil then
    Result := HitObject.GetCursor(Self)
  else
    Result := crDefault;
end;

function TdxSpreadSheetCustomHitTest.CanDrag(const P: TPoint): Boolean;
begin
  Result := GetDragAndDropObjectClass(P) <> nil;
end;

function TdxSpreadSheetCustomHitTest.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  if HitObject <> nil then
    Result := HitObject.GetDragAndDropObjectClass(Self)
  else
    Result := nil;
end;

function TdxSpreadSheetCustomHitTest.GetDragAndDropObjectClass(const P: TPoint): TcxDragAndDropObjectClass;
begin
  Calculate(P);
  Result := GetDragAndDropObjectClass;
end;

function TdxSpreadSheetCustomHitTest.GetHitCode(ACode: Integer): Boolean;
begin
  Result := FHitCode and ACode <> 0
end;

procedure TdxSpreadSheetCustomHitTest.SetHitCode(ACode: Integer; AValue: Boolean);
begin
  if AValue then
    FHitCode := FHitCode or ACode
  else
    FHitCode := FHitCode and not ACode;
end;

{ TdxSpreadSheetCustomDragAndDropObject }

procedure TdxSpreadSheetCustomDragAndDropObject.CheckScrollArea(const P: TPoint);
begin
  View.Controller.CheckScrollArea(cxPointScale(P, 100, ZoomFactor));
end;

function TdxSpreadSheetCustomDragAndDropObject.GetZoomFactor: Integer;
begin
  Result := View.ZoomFactor;
end;

function TdxSpreadSheetCustomDragAndDropObject.TranslateCoords(const P: TPoint): TPoint;
begin
  Result := cxPointOffset(inherited TranslateCoords(P), View.GetPartOffsetByPoint(P));
end;

function TdxSpreadSheetCustomDragAndDropObject.GetControl: TdxCustomSpreadSheet;
begin
  Result := TdxCustomSpreadSheet(inherited Control);
end;

function TdxSpreadSheetCustomDragAndDropObject.GetHitTest: TdxSpreadSheetCustomHitTest;
begin
  Result := Control.ActiveController.HitTest;
end;

function TdxSpreadSheetCustomDragAndDropObject.GetView: TdxSpreadSheetCustomView;
begin
  Result := Control.ActiveSheet;
end;

{ TdxSpreadSheetCustomCellViewInfo }

constructor TdxSpreadSheetCustomCellViewInfo.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxSpreadSheetCustomCellViewInfo.Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage);
begin
  if CanDraw(ACanvas, AStage) then
  begin
    FDrawingStage := AStage;
    DoDraw(ACanvas);
  end;
end;

function TdxSpreadSheetCustomCellViewInfo.CanDraw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage): Boolean;
begin
  Result := AStage = dsFirst;
end;

procedure TdxSpreadSheetCustomCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  // do nothing
end;

function TdxSpreadSheetCustomCellViewInfo.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
begin
  Result := crDefault;
end;

function TdxSpreadSheetCustomCellViewInfo.GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass;
begin
  Result := nil;
end;

function TdxSpreadSheetCustomCellViewInfo.GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass;
begin
  Result := nil;
end;

function TdxSpreadSheetCustomCellViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCustomCellViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := SpreadSheet.LookAndFeelPainter;
end;

function TdxSpreadSheetCustomCellViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := SpreadSheet.ScaleFactor;
end;

{ TdxSpreadSheetCellViewInfo }

constructor TdxSpreadSheetCellViewInfo.Create(AOwner: TObject);
begin
  inherited;
  FSupportedDrawingStages := [dsFirst];
  FScreenClipRect := cxRect(MinInt, MinInt, MaxInt, MaxInt);
end;

procedure TdxSpreadSheetCellViewInfo.Calculate;
begin
  if not Calculated then
    DoCalculate;
end;

procedure TdxSpreadSheetCellViewInfo.Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage);
begin
  if CanDraw(ACanvas, AStage) then
  begin
    FDrawingStage := AStage;
    if not DoCustomDraw(ACanvas) then
      DoDraw(ACanvas);
  end;
end;

procedure TdxSpreadSheetCellViewInfo.Invalidate;
begin
  Calculate;
  if Visible then
    InvalidateRect(Bounds);
end;

procedure TdxSpreadSheetCellViewInfo.Recalculate;
begin
  Calculated := False;
  Calculate;
end;

procedure TdxSpreadSheetCellViewInfo.SetBounds(const ABounds: TRect);
begin
  SetBounds(ABounds, FScreenClipRect);
end;

procedure TdxSpreadSheetCellViewInfo.SetBounds(const ABounds, AClipBounds: TRect);
begin
  FBounds := ABounds;
  FScreenClipRect := AClipBounds;
  Calculated := False;
end;

procedure TdxSpreadSheetCellViewInfo.UpdateState;
begin
  // do nothing
end;

procedure TdxSpreadSheetCellViewInfo.CalculateBounds;
begin
  FVisible := cxRectIntersect(FClipRect, FBounds, FScreenClipRect);
end;

function TdxSpreadSheetCellViewInfo.CanDraw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage): Boolean;
begin
  Result := Visible and (AStage in FSupportedDrawingStages) and ACanvas.RectVisible(ClipRect);
end;

procedure TdxSpreadSheetCellViewInfo.DoCalculate;
begin
  CalculateBounds;
  FCalculated := True;
end;

function TdxSpreadSheetCellViewInfo.DoCustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCellViewInfo.GetBounds: TRect;
begin
  Calculate;
  Result := FBounds;
end;

function TdxSpreadSheetCellViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := PtInRect(ClipRect, AHitTest.ActualHitPoint);
  if Result then
    AHitTest.HitObject := Self;
end;

procedure TdxSpreadSheetCellViewInfo.InvalidateRect(const R: TRect);
begin
  SpreadSheet.InvalidateRect(R, False);
end;

function TdxSpreadSheetCellViewInfo.IsPermanent: Boolean;
begin
  Result := False;
end;

 // IcxHintableObject
function TdxSpreadSheetCellViewInfo.HasHintPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(ScreenClipRect, P);
end;

function TdxSpreadSheetCellViewInfo.IsHintAtMousePos: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetCellViewInfo.UseHintHidePause: Boolean;
begin
  Result := True;
end;

{ TdxSpreadSheetViewAbstractCellViewInfo }

function TdxSpreadSheetViewAbstractCellViewInfo.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := View.SpreadSheet;
end;

procedure TdxSpreadSheetViewAbstractCellViewInfo.InvalidateRect(const R: TRect);
begin
  View.InvalidateRect(R);
end;

{ TdxSpreadSheetTableViewAbstractCellViewInfo }

function TdxSpreadSheetTableViewAbstractCellViewInfo.GetTableView: TdxSpreadSheetTableView;
begin
  Result := TdxSpreadSheetTableView(inherited View);
end;

{ TdxSpreadSheetTableViewSelectionViewInfo }

constructor TdxSpreadSheetTableViewSelectionViewInfo.Create(AOwnerViewInfo: TdxSpreadSheetTableViewInfo);
begin
  FOwnerViewInfo := AOwnerViewInfo;
  FFillAreas := TdxRectList.Create;
  FFillBrush := TdxGPBrush.Create;
  FFrameViewInfo := TdxSpreadSheetTableViewSelectionFrameViewInfo.Create(AOwnerViewInfo.View);
end;

destructor TdxSpreadSheetTableViewSelectionViewInfo.Destroy;
begin
  FreeAndNil(FFrameViewInfo);
  FreeAndNil(FFillBrush);
  FreeAndNil(FFillAreas);
  inherited;
end;

procedure TdxSpreadSheetTableViewSelectionViewInfo.Calculate;
begin
  Invalidate;
  CalculateCore(OwnerViewInfo.View.Selection);
  Invalidate;
end;

procedure TdxSpreadSheetTableViewSelectionViewInfo.Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage);
var
  AClipRect: TRect;
  AGpCanvas: TdxGPPaintCanvas;
  ARect: TRect;
  I: Integer;
begin
  if (AStage = dsFirst) and (FillAreas.Count > 0) and (GetClipBox(ACanvas.Handle, AClipRect) > NULLREGION) then
  begin
    if cxRectIntersect(AClipRect, AClipRect, BoundsRect) then
    begin
      AGpCanvas := dxGPPaintCanvas;
      AGpCanvas.BeginPaint(ACanvas.Handle, AClipRect);
      try
        AGpCanvas.SaveClipRegion;
        try
          AGpCanvas.SetClipRect(FocusedCellBounds, gmExclude);
          for I := 0 to FillAreas.Count - 1 do
          begin
            ARect := FillAreas[I];
            if cxRectIntersect(ARect, ARect, AClipRect) then
            begin
              AGpCanvas.FillRectangle(ARect, FillBrush);
              AGpCanvas.SetClipRect(ARect, gmExclude);
            end;
          end;
        finally
          AGpCanvas.RestoreClipRegion;
        end;
      finally
        AGpCanvas.EndPaint;
      end;
    end;
  end;
  FrameViewInfo.Draw(ACanvas, AStage);
end;

procedure TdxSpreadSheetTableViewSelectionViewInfo.Invalidate;
begin
  OwnerViewInfo.View.InvalidateRect(BoundsRect);
end;

function TdxSpreadSheetTableViewSelectionViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := FrameViewInfo.InitHitTest(AHitTest);
end;

procedure TdxSpreadSheetTableViewSelectionViewInfo.CalculateCore(ASelection: TdxSpreadSheetTableViewSelection);
var
  AFocusedCellArea: TRect;
  AFrameArea: TRect;
  I: Integer;
begin
  FillBrush.Color := dxColorToAlphaColor(OwnerViewInfo.SelectionParams.Color, 127);

  AFocusedCellArea := cxRectBounds(ASelection.FocusedColumn, ASelection.FocusedRow, 0, 0);
  if not dxSpreadSheetIsEntireRowOrColumn(AFocusedCellArea) then
    AFocusedCellArea := OwnerViewInfo.View.MergedCells.ExpandArea(AFocusedCellArea);
  if ASelection.Count > 1 then
    AFrameArea := AFocusedCellArea
  else
    AFrameArea := ASelection.Area;

  FFocusedCellBounds := GetFillAreaBounds(AFocusedCellArea);

  FrameViewInfo.SetBounds(OwnerViewInfo.GetAreaBounds(AFrameArea), OwnerViewInfo.Bounds);
  FrameViewInfo.Recalculate;

  FillAreas.Count := 0;
  FillAreas.Capacity := Max(FillAreas.Capacity, ASelection.Count);
  for I := 0 to ASelection.Count - 1 do
    FillAreas.Add(GetFillAreaBounds(ASelection[I].Rect));
end;

function TdxSpreadSheetTableViewSelectionViewInfo.GetFillAreaBounds(const AArea: TRect): TRect;
begin
  Result := cxRectOffset(OwnerViewInfo.GetAreaBounds(AArea), -1, -1);
end;

function TdxSpreadSheetTableViewSelectionViewInfo.GetBoundsRect: TRect;
var
  I: Integer;
begin
  Result := cxRectInflate(FrameViewInfo.Bounds, dxSpreadSheetSelectionThickness);
  for I := 0 to FillAreas.Count - 1 do
    Result := cxRectUnion(Result, FillAreas[I]);
end;

{ TdxSpreadSheetCellViewInfoList }

procedure TdxSpreadSheetCellViewInfoList.Add(AViewInfo: TdxSpreadSheetCellViewInfo);
begin
  inherited Add(AViewInfo);
end;

function TdxSpreadSheetCellViewInfoList.CalculateHitTest(
  AHitTest: TdxSpreadSheetCustomHitTest; AReverse: Boolean = False): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AReverse then
    for I := Count - 1 downto 0 do
    begin
      Result := Items[I].InitHitTest(AHitTest);
      if Result then
        Break;
    end
  else
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].InitHitTest(AHitTest);
      if Result then
        Break;
    end;
end;

procedure TdxSpreadSheetCellViewInfoList.Clear;
var
  AItem: TdxSpreadSheetCellViewInfo;
  AList: TList;
  I: Integer;
begin
  AList := nil;
  for I := 0 to Count - 1 do
  begin
    AItem := TdxSpreadSheetCellViewInfo(List[I]);
    if AItem.IsPermanent then
    begin
      if AList = nil then
        AList := TList.Create;
      AList.Add(AItem);
      List[I] := nil;
    end;
  end;

  Count := 0;

  if AList <> nil then
  try
    for I := 0 to AList.Count - 1 do
      Add(AList[I]);
  finally
    AList.Free;
  end;
end;

procedure TdxSpreadSheetCellViewInfoList.Draw(ACanvas: TcxCanvas);
begin
  Draw(ACanvas, dsFirst);
  Draw(ACanvas, dsSecond);
end;

procedure TdxSpreadSheetCellViewInfoList.Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Draw(ACanvas, AStage);
end;

function TdxSpreadSheetCellViewInfoList.FindItem(AItemOwner: TObject): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].Owner = AItemOwner then
      Exit(I);
  end;
  Result := -1;
end;

procedure TdxSpreadSheetCellViewInfoList.UpdateState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].UpdateState;
end;

function TdxSpreadSheetCellViewInfoList.GetItem(AIndex: Integer): TdxSpreadSheetCellViewInfo;
begin
  Result := TdxSpreadSheetCellViewInfo(List[AIndex]);
  Result.Calculate;
end;

{ TdxSpreadSheetTableViewCellViewInfoList }

constructor TdxSpreadSheetTableViewCellViewInfoList.Create;
begin
  inherited Create;
  FIndex := TDictionary<Int64, TdxSpreadSheetTableViewCellViewInfo>.Create;
end;

destructor TdxSpreadSheetTableViewCellViewInfoList.Destroy;
begin
  inherited;
  FreeAndNil(FIndex);
end;

procedure TdxSpreadSheetTableViewCellViewInfoList.Add(AViewInfo: TdxSpreadSheetCellViewInfo);
begin
  inherited Add(AViewInfo);
  FIndex.AddOrSetValue(dxMakeInt64(
    TdxSpreadSheetTableViewCellViewInfo(AViewInfo).Column,
    TdxSpreadSheetTableViewCellViewInfo(AViewInfo).Row),
    TdxSpreadSheetTableViewCellViewInfo(AViewInfo));
end;

procedure TdxSpreadSheetTableViewCellViewInfoList.Clear;
begin
  FIndex.Clear;
  inherited;
end;

function TdxSpreadSheetTableViewCellViewInfoList.FindItemForCell(
  ACell: TdxSpreadSheetCell; out AViewInfo: TdxSpreadSheetTableViewCellViewInfo): Boolean;
begin
  Result := (ACell <> nil) and FindItemForCell(ACell.RowIndex, ACell.ColumnIndex, AViewInfo)
end;

function TdxSpreadSheetTableViewCellViewInfoList.FindItemForCell(
  ARow, AColumn: Integer; out AViewInfo: TdxSpreadSheetTableViewCellViewInfo): Boolean;
begin
  Result := FIndex.TryGetValue(dxMakeInt64(AColumn, ARow), AViewInfo);
end;

function TdxSpreadSheetTableViewCellViewInfoList.GetItem(Index: Integer): TdxSpreadSheetTableViewCellViewInfo;
begin
  Result := TdxSpreadSheetTableViewCellViewInfo(inherited Items[Index]);
end;

{ TdxSpreadSheetTableViewMergedCellViewInfo }

constructor TdxSpreadSheetTableViewMergedCellViewInfo.Create(
  AViewInfo: TdxSpreadSheetTableViewInfo; ACell: TdxSpreadSheetMergedCell);
begin
  inherited Create(AViewInfo);
  FMergedCell := ACell;
  FCell := ACell.ActiveCell;
end;

function TdxSpreadSheetTableViewMergedCellViewInfo.GetArea: TRect;
begin
  Result := MergedCell.Area;
end;

procedure TdxSpreadSheetTableViewMergedCellViewInfo.CalculateDisplayTextParameters(const R: TRect; ACell: TdxSpreadSheetCell);
begin
  if dxSpreadSheetTextService.IsFormattedTextValue(ACell) then
  begin
    inherited;
    Exit;
  end;

  if Style.ShowCellValue then
    FDisplayText := ACell.DisplayText
  else
    FDisplayText := '';

  if FDisplayText <> '' then
    FTextBounds := R
  else
    FTextBounds := cxRectSetWidth(R, 0);
end;

procedure TdxSpreadSheetTableViewMergedCellViewInfo.InitDrawValue;
begin
  Calculate;
  FContentBounds := cxRectContent(Bounds, TdxSpreadSheetCell.GetContentOffsets(sscbsDefault));
  InitDrawValueCore(Cell);
end;

{ TdxSpreadSheetTableViewGroupCustomExpandButtonViewInfo }

constructor TdxSpreadSheetTableViewGroupCustomExpandButtonViewInfo.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FState := cxbsNormal;
end;

procedure TdxSpreadSheetTableViewGroupCustomExpandButtonViewInfo.UpdateState;
begin
  if View.Controller.HitTest.HitObject <> Self then
    State := cxbsNormal
  else if View.Controller.PressedObject = Self then
    State := cxbsPressed
  else if View.Controller.PressedObject <> nil then
    State := cxbsNormal
  else
    State := cxbsHot;
end;

procedure TdxSpreadSheetTableViewGroupCustomExpandButtonViewInfo.SetState(AValue: TcxButtonState);
begin
  if FState <> AValue then
  begin
    FState := AValue;
    Invalidate;
  end;
end;

function TdxSpreadSheetTableViewGroupCustomExpandButtonViewInfo.GetPainter: TdxSpreadSheetTableViewPainter;
begin
  Result := View.ViewInfo.Painter;
end;

{ TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo }

constructor TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo.Create(AOwner: TObject; ALevel: Integer);
begin
  inherited Create(AOwner);
  FLevel := ALevel;
end;

procedure TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  Painter.DrawGroupLevelExpandButton(ACanvas, Bounds, State, FLevel);
end;

function TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo.GetView: TdxSpreadSheetCustomView;
begin
  Result := Items.View;
end;

function TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := inherited;
  if Result then
  begin
    AHitTest.HitObjectData := FLevel;
    AHitTest.SetHitCode(hcExpandButton, True);
  end;
end;

function TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo.GetItems: TdxSpreadSheetTableItems;
begin
  Result := TdxSpreadSheetTableItems(Owner);
end;

{ TdxSpreadSheetTableViewGroupViewInfo }

constructor TdxSpreadSheetTableViewGroupViewInfo.Create(AOwner: TObject; AVertical: Boolean);
begin
  inherited Create(AOwner);
  FVertical := AVertical;
end;

procedure TdxSpreadSheetTableViewGroupViewInfo.DoCalculate;
var
  AMinSize: TSize;
begin
  inherited DoCalculate;
  FExpanded := Group.Expanded;
  AMinSize := Painter.GetGroupExpandButtonMinSize;

  if Vertical then
  begin
    FLineRect := cxRectCenterHorizontally(Bounds, Painter.GetGroupLineSize);
    if Group.ExpandButtonPosition = gebpGroupFinish then
    begin
      FExpandButtonRect := cxRectSetBottom(Bounds, Bounds.Bottom, AMinSize.cy);
      FLineRect.Bottom := FExpandButtonRect.Top;
    end
    else
    begin
      FExpandButtonRect := cxRectSetHeight(Bounds, AMinSize.cy);
      FLineRect.Top := FExpandButtonRect.Bottom;
    end;
  end
  else
  begin
    FLineRect := cxRectCenterVertically(Bounds, Painter.GetGroupLineSize);
    if Group.ExpandButtonPosition = gebpGroupFinish then
    begin
      FExpandButtonRect := cxRectSetRight(Bounds, Bounds.Right, AMinSize.cx);
      FLineRect.Right := FExpandButtonRect.Left;
    end
    else
    begin
      FExpandButtonRect := cxRectSetWidth(Bounds, AMinSize.cx);
      FLineRect.Left := FExpandButtonRect.Right;
    end;
  end;
end;

procedure TdxSpreadSheetTableViewGroupViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(ClipRect);
    if Expanded then
      Painter.DrawGroupLine(ACanvas, LineRect, Vertical, Group.ExpandButtonPosition);
    Painter.DrawGroupExpandButton(ACanvas, ExpandButtonRect, State, Vertical, Expanded);
  finally
    ACanvas.RestoreClipRegion;
  end;
  if cxRectIntersect(R, ClipRect, ExpandButtonRect) then
    ACanvas.ExcludeClipRect(R);
end;

function TdxSpreadSheetTableViewGroupViewInfo.GetView: TdxSpreadSheetCustomView;
begin
  Result := Group.Owner.View;
end;

function TdxSpreadSheetTableViewGroupViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := inherited and (Expanded and PtInRect(LineRect, AHitTest.ActualHitPoint) or
    PtInRect(ExpandButtonRect, AHitTest.ActualHitPoint));

  if Result then
  begin
    AHitTest.SetHitCode(hcGroup, True);
    AHitTest.SetHitCode(hcExpandButton, PtInRect(ExpandButtonRect, AHitTest.ActualHitPoint));
  end;
end;

function TdxSpreadSheetTableViewGroupViewInfo.GetGroup: TdxSpreadSheetTableItemGroup;
begin
  Result := TdxSpreadSheetTableItemGroup(Owner);
end;

{ TdxSpreadSheetTableViewCustomGroupingAreaViewInfo }

constructor TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FMarks := TdxRectList.Create;
  FCells := TdxSpreadSheetCellViewInfoList.Create;
end;

destructor TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.Destroy;
begin
  FreeAndNil(FCells);
  FreeAndNil(FMarks);
  inherited Destroy;
end;

function TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.MeasureSize: Integer;
begin
  Result := GetGroupLevelSize * Items.Groups.Root.MaxNestingLevel + 2 * ContentIndent;
end;

procedure TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.AddGroupCell(
  ACell: TdxSpreadSheetTableViewGroupViewInfo; const ABounds: TRect);
begin
  ACell.SetBounds(ABounds, GroupCellsArea);
  Cells.Add(ACell);
end;

procedure TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.AddGroupLevelExpandButtonCell(ALevel: Integer; const ABounds: TRect);
var
  ACell: TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo;
begin
  ACell := TdxSpreadSheetTableViewGroupLevelExpandButtonViewInfo.Create(Items, ALevel);
  ACell.SetBounds(ABounds, Bounds);
  Cells.Add(ACell);
end;

procedure TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.CalculateGroupChildren(
  AGroup: TdxSpreadSheetTableItemGroup; const AAreaBounds: TRect; AAreaStartIndex, AAreaFinishIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to AGroup.Count - 1 do
    CalculateGroup(AGroup[I], AAreaBounds, AAreaStartIndex, AAreaFinishIndex);
end;

procedure TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.CalculateGroupLevelMarks(
  AGroup: TdxSpreadSheetTableItemGroup; const AAreaBounds: TRect);
var
  AIndex: Integer;
  ASubGroup: TdxSpreadSheetTableItemGroup;
begin
  AIndex := AGroup.StartIndex;
  while AIndex <= AGroup.FinishIndex do
  begin
    ASubGroup := AGroup.Find(AIndex, False);
    if ASubGroup = AGroup then
      Marks.Add(CalculateGroupLevelMarkBounds(AAreaBounds, AGroup.StartIndex, AIndex))
    else
      AIndex := ASubGroup.FinishIndex + 1;

    Inc(AIndex);
  end;
end;

procedure TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.DoCalculate;
begin
  Cells.Clear;
  Marks.Clear;
  inherited DoCalculate;
  CalculateGroups;
  if ViewInfo.Options.ActualHeaders then
    CalculateGroupLevelsExpandButtons;
end;

procedure TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  if not ViewInfo.Options.ActualHeaders then
    ACanvas.FrameRect(Bounds, ViewInfo.GridLineColor, 1, FBorders);

  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(Bounds);
    Cells.Draw(ACanvas);

    ACanvas.IntersectClipRect(GroupCellsArea);
    for I := 0 to Marks.Count - 1 do
      Painter.DrawGroupLevelMark(ACanvas, Marks[I]);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.GetGroupCellsArea: TRect;
begin
  Result := cxRectInflate(Bounds, -ContentIndent);
end;

function TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := inherited InitHitTest(AHitTest);
  if Result then
  begin
    AHitTest.SetHitCode(hcGroupingArea, True);
    Cells.CalculateHitTest(AHitTest);
  end;
end;

procedure TdxSpreadSheetTableViewCustomGroupingAreaViewInfo.UpdateState;
begin
  inherited;
  Cells.UpdateState;
end;

{ TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo }

procedure TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  FBorders := [bBottom];
end;

procedure TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo.CalculateGroup(
  AGroup: TdxSpreadSheetTableItemGroup; const AAreaBounds: TRect; AAreaStartIndex, AAreaFinishIndex: Integer);
var
  AGroupArea: TRect;
  ARect: TRect;
begin
  if not (
    InRange(ViewInfo.VisibleCells.Left, AGroup.StartIndex, AGroup.FinishIndex) or
    InRange(ViewInfo.VisibleCells.Right, AGroup.StartIndex, AGroup.FinishIndex) or
    InRange(AGroup.StartIndex, ViewInfo.VisibleCells.Left, ViewInfo.VisibleCells.Right) or
    InRange(AGroup.FinishIndex, ViewInfo.VisibleCells.Left, ViewInfo.VisibleCells.Right))
  then
    Exit;

  AGroupArea := cxRectSetSize(AAreaBounds, Items.GetDistance(AGroup.StartIndex, AGroup.FinishIndex), GroupLevelSize);
  AGroupArea := cxRectOffset(AGroupArea, cxPoint(Items.GetDistance(AAreaStartIndex, AGroup.StartIndex - 1), 0));

  if AGroup.FinishIndex <> AAreaFinishIndex then
  begin
    ARect := AGroupArea;
    if Items.Groups.ExpandButtonPosition = gebpGroupStart then
      Dec(ARect.Left, (Items.GetItemSize(AGroup.StartIndex - 1) + Painter.GetGroupExpandButtonMinSize.cx) div 2)
    else
      Inc(ARect.Right, (Items.GetItemSize(AGroup.FinishIndex + 1) + Painter.GetGroupExpandButtonMinSize.cx) div 2);

    ARect.Left := Min(ARect.Left, ARect.Right - Painter.GetGroupExpandButtonMinSize.cx);
    AddGroupCell(TdxSpreadSheetTableViewGroupViewInfo.Create(AGroup, False), ARect);
  end;

  if not cxRectIsEmpty(AGroupArea) then
  begin
    AGroupArea.Top := AGroupArea.Bottom;
    AGroupArea.Bottom := AAreaBounds.Bottom;
    CalculateGroupChildren(AGroup, AGroupArea, AGroup.StartIndex, AGroup.FinishIndex);
    CalculateGroupLevelMarks(AGroup, AGroupArea);
  end;
end;

function TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo.CalculateGroupLevelMarkBounds(
  const AAreaBounds: TRect; AStartIndex, AIndex: Integer): TRect;
begin
  Result := AAreaBounds;
  Inc(Result.Left, Items.GetDistance(AStartIndex, AIndex - 1));
  Result := cxRectSetSize(Result, Items.GetItemSize(AIndex), GroupLevelSize);
  Result := cxRectCenter(Result, Painter.GetGroupLevelMarkSize);
end;

procedure TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo.CalculateGroupLevelsExpandButtons;
var
  I: Integer;
  R: TRect;
begin
  R := cxRectOffset(cxRectSetHeight(Bounds, GroupLevelSize), 0, ContentIndent);
  R.Right := GroupCellsArea.Left;
  R := cxRectCenterHorizontally(R, Painter.GetGroupExpandButtonMinSize.cx);
  for I := 0 to Items.Groups.Root.MaxNestingLevel - 1 do
  begin
    AddGroupLevelExpandButtonCell(I, R);
    R := cxRectOffset(R, 0, cxRectHeight(R));
  end;
end;

procedure TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo.CalculateGroups;
var
  ARect: TRect;
begin
  ARect := GroupCellsArea;
  Dec(ARect.Left, ViewInfo.FirstColumnOrigin);
  CalculateGroupChildren(Items.Groups.Root, ARect, 0, Items.GetMaxItemIndex);
end;

function TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo.GetGroupCellsArea: TRect;
begin
  Result := inherited GetGroupCellsArea;
  Result.Left := ViewInfo.CellsArea.Left;
end;

function TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo.GetGroupLevelSize: Integer;
begin
  Result := Painter.GetGroupExpandButtonMinSize.cy;
end;

function TdxSpreadSheetTableViewColumnsGroupingAreaViewInfo.GetItems: TdxSpreadSheetTableItems;
begin
  Result := ViewInfo.Columns;
end;

{ TdxSpreadSheetTableViewRowsGroupingAreaViewInfo }

procedure TdxSpreadSheetTableViewRowsGroupingAreaViewInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  FBorders := [bRight];
end;

procedure TdxSpreadSheetTableViewRowsGroupingAreaViewInfo.CalculateGroup(
  AGroup: TdxSpreadSheetTableItemGroup; const AAreaBounds: TRect; AAreaStartIndex, AAreaFinishIndex: Integer);
var
  AGroupArea: TRect;
  ARect: TRect;
begin
  if not (
    InRange(ViewInfo.VisibleCells.Top, AGroup.StartIndex, AGroup.FinishIndex) or
    InRange(ViewInfo.VisibleCells.Bottom, AGroup.StartIndex, AGroup.FinishIndex) or
    InRange(AGroup.StartIndex, ViewInfo.VisibleCells.Top, ViewInfo.VisibleCells.Bottom) or
    InRange(AGroup.FinishIndex, ViewInfo.VisibleCells.Top, ViewInfo.VisibleCells.Bottom))
  then
    Exit;

  AGroupArea := cxRectSetSize(cxRectOffset(AAreaBounds, 0, Items.GetDistance(AAreaStartIndex, AGroup.StartIndex - 1)),
    GroupLevelSize, Items.GetDistance(AGroup.StartIndex, AGroup.FinishIndex));

  if AGroup.FinishIndex <> AAreaFinishIndex then
  begin
    ARect := AGroupArea;
    if Items.Groups.ExpandButtonPosition = gebpGroupStart then
      Dec(ARect.Top, (Items.GetItemSize(AGroup.StartIndex - 1) + Painter.GetGroupExpandButtonMinSize.cy) div 2)
    else
      Inc(ARect.Bottom, (Items.GetItemSize(AGroup.FinishIndex + 1) + Painter.GetGroupExpandButtonMinSize.cy) div 2);

    ARect.Top := Min(ARect.Top, ARect.Bottom - Painter.GetGroupExpandButtonMinSize.cy);
    AddGroupCell(TdxSpreadSheetTableViewGroupViewInfo.Create(AGroup, True), ARect);
  end;

  if not cxRectIsEmpty(AGroupArea) then
  begin
    AGroupArea.Left := AGroupArea.Right;
    AGroupArea.Right := AAreaBounds.Right;
    CalculateGroupChildren(AGroup, AGroupArea, AGroup.StartIndex, AGroup.FinishIndex);
    CalculateGroupLevelMarks(AGroup, AGroupArea);
  end;
end;

function TdxSpreadSheetTableViewRowsGroupingAreaViewInfo.CalculateGroupLevelMarkBounds(
  const AAreaBounds: TRect; AStartIndex, AIndex: Integer): TRect;
begin
  Result := AAreaBounds;
  Inc(Result.Top, Items.GetDistance(AStartIndex, AIndex - 1));
  Result := cxRectSetSize(Result, GroupLevelSize, Items.GetItemSize(AIndex));
  Result := cxRectCenter(Result, Painter.GetGroupLevelMarkSize);
end;

procedure TdxSpreadSheetTableViewRowsGroupingAreaViewInfo.CalculateGroupLevelsExpandButtons;
var
  I: Integer;
  R: TRect;
begin
  R := cxRectOffset(cxRectSetWidth(Bounds, GroupLevelSize), ContentIndent, 0);
  R.Bottom := GroupCellsArea.Top;
  R := cxRectCenterVertically(R, Painter.GetGroupExpandButtonMinSize.cy);
  for I := 0 to Items.Groups.Root.MaxNestingLevel - 1 do
  begin
    AddGroupLevelExpandButtonCell(I, R);
    R := cxRectOffset(R, cxRectWidth(R), 0);
  end;
end;

procedure TdxSpreadSheetTableViewRowsGroupingAreaViewInfo.CalculateGroups;
var
  ARect: TRect;
begin
  ARect := GroupCellsArea;
  Dec(ARect.Top, ViewInfo.FirstRowOrigin);
  CalculateGroupChildren(Items.Groups.Root, ARect, 0, Items.GetMaxItemIndex);
end;

function TdxSpreadSheetTableViewRowsGroupingAreaViewInfo.GetGroupCellsArea: TRect;
begin
  Result := inherited GetGroupCellsArea;
  Result.Top := ViewInfo.CellsArea.Top;
end;

function TdxSpreadSheetTableViewRowsGroupingAreaViewInfo.GetGroupLevelSize: Integer;
begin
  Result := Painter.GetGroupExpandButtonMinSize.cx;
end;

function TdxSpreadSheetTableViewRowsGroupingAreaViewInfo.GetItems: TdxSpreadSheetTableItems;
begin
  Result := ViewInfo.Rows;
end;

{ TdxSpreadSheetContainerViewInfo }

constructor TdxSpreadSheetContainerViewInfo.Create(AContainer: TdxSpreadSheetContainer);
begin
  inherited Create(AContainer);
  FAlpha := MaxByte;
  FTransformMatrix := TdxGPMatrix.Create;
  FTransformMatrixInv := TdxGPMatrix.Create;
  FSupportedDrawingStages := [dsFirst, dsSecond];
  FCanDrawSelection := True;
end;

destructor TdxSpreadSheetContainerViewInfo.Destroy;
begin
  FreeAndNil(FTransformMatrixInv);
  FreeAndNil(FTransformMatrix);
  FreeAndNil(FSelection);
  inherited Destroy;
end;

function TdxSpreadSheetContainerViewInfo.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
begin
  if Owner.Hyperlink <> nil then
    Result := crHandPoint
  else
    Result := inherited GetCursor(AHitTest);
end;

procedure TdxSpreadSheetContainerViewInfo.Invalidate;
begin
  if Selected then
    Selection.Invalidate;
  InvalidateRect(RealDrawingBounds);
end;

procedure TdxSpreadSheetContainerViewInfo.CalculateBounds;
var
  AMargins: TdxRectF;
begin
  if IsDragging and Calculated then
  begin
    AMargins := dxRectF(ContentBounds.Left - Bounds.Left, ContentBounds.Top - Bounds.Top,
      Bounds.Right - ContentBounds.Right, Bounds.Bottom - ContentBounds.Bottom);
  end
  else
    AMargins := dxNullRectF;

  FCalculated := True;
  ContentBounds := cxRectContent(dxRectF(FBounds), AMargins);
  CalculateTransformMatrix;
  CalculateSelection;
  CalculateDisplayClipRect;
end;

procedure TdxSpreadSheetContainerViewInfo.CalculateDisplayClipRect;
var
  R: TRect;
begin
  R := RealDrawingBounds;
  if Selected then
    R := cxRectUnion(R, Selection.RealDrawingBounds);
  FVisible := cxRectIntersect(FClipRect, R, FScreenClipRect);
end;

procedure TdxSpreadSheetContainerViewInfo.CalculateSelection;
begin
  if Selected then
  begin
    Selection.SetBounds(cxRectInflate(Bounds, Selection.ContentOffsets), ScreenClipRect);
    Selection.Calculate;
  end;
end;

procedure TdxSpreadSheetContainerViewInfo.CalculateTransformMatrix;
var
  APivotPoint: TdxPointF;
begin
  APivotPoint := dxPointF(cxRectCenter(Bounds));

  TransformMatrix.Reset;
  TransformMatrix.Rotate(RotationAngle, APivotPoint);

  TransformMatrixInv.Assign(TransformMatrix);
  TransformMatrixInv.Invert;
end;

procedure TdxSpreadSheetContainerViewInfo.ContentBoundsChanged;
begin
  CalculateDisplayClipRect;
end;

function TdxSpreadSheetContainerViewInfo.CreateSelectionViewInfo: TdxSpreadSheetContainerSelectionCellViewInfo;
begin
  Result := TdxSpreadSheetContainerSelectionCellViewInfo.Create(Self);
end;

procedure TdxSpreadSheetContainerViewInfo.DoCalculate;
begin
  if Owner.IsTransformsSupported then
  begin
    FRotationAngle := Transform.RotationAngle;
    FFlipHorz := Transform.FlipHorizontally;
    FFlipVert := Transform.FlipVertically;
  end
  else
  begin
    FRotationAngle := 0;
    FFlipHorz := False;
    FFlipVert := False;
  end;
  CalculateBounds;
end;

procedure TdxSpreadSheetContainerViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  ABitmap: TdxGPImage;
  ABitmapCanvas: TdxGPCanvas;
  ARect: TRect;
begin
  if (DrawingStage = dsFirst) and (Alpha = MaxByte) then
  begin
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, RealDrawingBounds);
    try
      InternalDraw(dxGPPaintCanvas);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;

  if (DrawingStage = dsSecond) and (Alpha > 0) and (Alpha < MaxByte) then
  begin
    ARect := RealDrawingBounds;
    ABitmap := TdxGPImage.CreateSize(ARect);
    try
      ABitmapCanvas := ABitmap.CreateCanvas;
      try
        ABitmapCanvas.TranslateWorldTransform(-ARect.Left, -ARect.Top);
        InternalDraw(ABitmapCanvas);
        ABitmapCanvas.ResetWorldTransform;
      finally
        ABitmapCanvas.Free;
      end;
      ABitmap.StretchDraw(ACanvas.Handle, ARect, Alpha);
    finally
      ABitmap.Free;
    end;
  end;

  if HasSelectionFrame then
    Selection.Draw(ACanvas, DrawingStage);
end;

function TdxSpreadSheetContainerViewInfo.DoInitHitTest(const P: TPoint; AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := PtInRect(cxRect(ContentBounds), P);
end;

procedure TdxSpreadSheetContainerViewInfo.DrawBackground(ACanvas: TdxGPCanvas);
begin
  // do nothing
end;

procedure TdxSpreadSheetContainerViewInfo.DrawBorder(ACanvas: TdxGPCanvas);
begin
  // do nothing
end;

procedure TdxSpreadSheetContainerViewInfo.DrawContent(ACanvas: TdxGPCanvas);
begin
  // do nothing
end;

function TdxSpreadSheetContainerViewInfo.GetBorderWidth: Single;
begin
  Result := 0;
end;

function TdxSpreadSheetContainerViewInfo.GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass;
begin
  if Owner.CanMove then
    Result := TdxSpreadSheetContainerMoveDragAndDropObject
  else
    Result := nil;
end;

function TdxSpreadSheetContainerViewInfo.GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass;
begin
  Result := TdxSpreadSheetBuiltInTableViewPopupMenu;
end;

function TdxSpreadSheetContainerViewInfo.GetRealBounds: TRect;
begin
  Result := cxRect(TransformMatrix.GetBoundingRectangle(ContentBounds));
end;

function TdxSpreadSheetContainerViewInfo.GetRealDrawingBounds: TRect;
begin
  Result := cxRect(TransformMatrix.GetBoundingRectangle(cxRectInflate(ContentBounds, Ceil(BorderWidth))), False);
end;

function TdxSpreadSheetContainerViewInfo.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := Owner.SpreadSheet;
end;

function TdxSpreadSheetContainerViewInfo.GetTransform: TdxSpreadSheetContainerTransform;
begin
  Result := Owner.Transform;
end;

function TdxSpreadSheetContainerViewInfo.HasSelectionFrame: Boolean;
begin
  Result := Selected and not cxRectIsEmpty(Bounds);
end;

function TdxSpreadSheetContainerViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := HasSelectionFrame and Selection.InitHitTest(AHitTest);
  if not Result then
  begin
    Result := DoInitHitTest(TransformMatrixInv.TransformPoint(AHitTest.ActualHitPoint), AHitTest);
    if Result then
    begin
      AHitTest.HitObject := Self;
      AHitTest.HitCodes[hcContainer] := True;
    end;
  end;
end;

procedure TdxSpreadSheetContainerViewInfo.InvalidateRect(const R: TRect);
begin
  if Owner.Parent <> nil then
    Owner.Parent.InvalidateRect(R)
  else
    inherited;
end;

function TdxSpreadSheetContainerViewInfo.IsPermanent: Boolean;
begin
  Result := FIsDragging;
end;

procedure TdxSpreadSheetContainerViewInfo.UpdateState;
begin
  Selected := Owner.Focused;

  if HasSelectionFrame then
    FSupportedDrawingStages := Selection.FSupportedDrawingStages
  else
    FSupportedDrawingStages := [];

  if Alpha = MaxByte then
    Include(FSupportedDrawingStages, dsFirst)
  else
    Include(FSupportedDrawingStages, dsSecond);
end;

function TdxSpreadSheetContainerViewInfo.GetOptionsView: TdxSpreadSheetOptionsView;
begin
  Result := SpreadSheet.OptionsView;
end;

function TdxSpreadSheetContainerViewInfo.GetOwner: TdxSpreadSheetContainer;
begin
  Result := TdxSpreadSheetContainer(inherited Owner);
end;

function TdxSpreadSheetContainerViewInfo.GetSelected: Boolean;
begin
  Result := Selection <> nil;
end;

procedure TdxSpreadSheetContainerViewInfo.InternalDraw(ACanvas: TdxGPCanvas);
begin
  dxSpreadSheetSetupGpCanvas(ACanvas, OptionsView.Antialiasing);

  ACanvas.ModifyWorldTransform(TransformMatrix);
  ACanvas.FlipWorldTransform(FFlipHorz, FFlipVert, cxRectCenter(ContentBounds));

  DrawBackground(ACanvas);
  DrawContent(ACanvas);
  DrawBorder(ACanvas);
end;

procedure TdxSpreadSheetContainerViewInfo.SetAlpha(const AValue: Byte);
begin
  if Alpha <> AValue then
  begin
    FAlpha := AValue;
    UpdateState;
    Invalidate;
  end;
end;

procedure TdxSpreadSheetContainerViewInfo.SetContentBounds(const AValue: TdxRectF);
begin
  if FContentBounds <> AValue then
  begin
    FContentBounds := AValue;
    ContentBoundsChanged;
  end;
end;

procedure TdxSpreadSheetContainerViewInfo.SetIsDragging(const Value: Boolean);
begin
  if IsDragging <> Value then
  begin
    CanDrawSelection := not Value;
    FIsDragging := Value;
    Invalidate;
  end;
end;

procedure TdxSpreadSheetContainerViewInfo.SetRotationAngle(const AValue: Double);
begin
  if RotationAngle <> AValue then
  begin
    FRotationAngle := AValue;
    CalculateTransformMatrix;
  end;
end;

procedure TdxSpreadSheetContainerViewInfo.SetSelected(const AValue: Boolean);
begin
  if Selected <> AValue then
  begin
    if AValue then
    begin
      FSelection := CreateSelectionViewInfo;
      CalculateSelection;
    end
    else
    begin
      Selection.Invalidate;
      FreeAndNil(FSelection);
    end;
    CalculateDisplayClipRect;
  end;
end;

{ TdxSpreadSheetContainerCustomDragAndDropObject }

procedure TdxSpreadSheetContainerCustomDragAndDropObject.AlignToCell(var ADeltaX, ADeltaY: Integer; const P: TPoint);

  procedure SetDelta(var ADelta: Integer; ANewValue: Integer);
  begin
    if (ADelta = MaxInt) or (Abs(ANewValue) < Abs(ADelta)) then
      ADelta := ANewValue;
  end;

var
  ACellBounds: TRect;
  AColumnIndex, ARowIndex: Integer;
begin
  if (TableViewIntf <> nil) and TableViewIntf.GetCellAtAbsolutePoint(P, ARowIndex, AColumnIndex) then
  begin
    ACellBounds := TableViewIntf.GetAbsoluteCellBounds(ARowIndex, AColumnIndex, False);
    if P.X > cxRectCenter(ACellBounds).X then
      SetDelta(ADeltaX, ACellBounds.Right - P.X)
    else
      SetDelta(ADeltaX, ACellBounds.Left - P.X);

    if P.Y > cxRectCenter(ACellBounds).Y then
      SetDelta(ADeltaY, ACellBounds.Bottom - P.Y)
    else
      SetDelta(ADeltaY, ACellBounds.Top - P.Y);
  end;
end;

procedure TdxSpreadSheetContainerCustomDragAndDropObject.AlignToCells(var R: TRect);
var
  ADeltaX, ADeltaY: Integer;
begin
  ADeltaX := MaxInt;
  ADeltaY := MaxInt;
  AlignToCell(ADeltaX, ADeltaY, R.TopLeft);
  AlignToCell(ADeltaX, ADeltaY, R.BottomRight);
  R := cxRectOffset(R, IfThen(ADeltaX <> MaxInt, ADeltaX), IfThen(ADeltaY <> MaxInt, ADeltaY));
end;

procedure TdxSpreadSheetContainerCustomDragAndDropObject.BeforeBeginDragAndDrop;
begin
  inherited BeforeBeginDragAndDrop;
  FViewInfo := GetContainerViewInfo;
  FViewInfo.IsDragging := True;
  Supports(View, IdxSpreadSheetTableView, FTableViewIntf);
end;

procedure TdxSpreadSheetContainerCustomDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  FCapturePoint := CurMousePos;
  FBounds := ToAbsoluteRect(ViewInfo.Bounds);
  FContentRect := GetContentRect;
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.CheckForContentArea(const R: TRect): TRect;
begin
  Result := Container.Calculator.CheckForContentArea(R);
end;

procedure TdxSpreadSheetContainerCustomDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  inherited DragAndDrop(P, Accepted);
  CheckScrollArea(GetClientCursorPos);
end;

procedure TdxSpreadSheetContainerCustomDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  inherited EndDragAndDrop(Accepted);
  if Accepted and not cxRectIsEqual(Bounds, ToAbsoluteRect(ViewInfo.Bounds)) then
  begin
    Container.BeginChanging;
    try
      RecalculateAnchors;
    finally
      Container.EndChanging;
    end;
  end;
  ViewInfo.Alpha := MaxByte;
  ViewInfo.IsDragging := False;
  Control.AddChanges([sscLayout, sscModified]);
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.GetContainerViewInfo: TdxSpreadSheetContainerViewInfo;
begin
  Result := HitTest.HitObject as TdxSpreadSheetContainerViewInfo;
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.GetContentOrigin: TPoint;
begin
  Result := View.GetContentOrigin;
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.GetContentRect: TRect;
begin
  Result := cxNullRect;
  if TableViewIntf <> nil then
    Result.BottomRight := TableViewIntf.GetAbsoluteCellBounds(dxSpreadSheetMaxRowIndex, dxSpreadSheetMaxColumnIndex, False).BottomRight;
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := ViewInfo.GetCursor(Control.ActiveController.HitTest)
  else
    Result := crNoDrop
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.GetHistory: TdxSpreadSheetHistory;
begin
  Result := View.SpreadSheet.History;
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetContainerCustomDragAndDropObject.InvalidateRects(const R1, R2: TRect);
begin
  View.InvalidateRect(R1);
  View.InvalidateRect(R2);
  Control.Update;
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.ProcessKeyDown(AKey: Word; AShiftState: TShiftState): Boolean;
begin
  case AKey of
    VK_CONTROL, VK_LCONTROL, VK_RCONTROL, VK_SHIFT, VK_LSHIFT, VK_RSHIFT, VK_MENU, VK_LMENU, VK_RMENU:
      Result := True;
  else
    Result := inherited ProcessKeyDown(AKey, AShiftState);
  end;
  RecalculateDragAndDropInfo;
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.ProcessKeyUp(AKey: Word; AShiftState: TShiftState): Boolean;
begin
  Result := inherited ProcessKeyUp(AKey, AShiftState);
  RecalculateDragAndDropInfo;
end;

procedure TdxSpreadSheetContainerCustomDragAndDropObject.RecalculateAnchors;
begin
  Container.Calculator.UpdateAnchors(ToAbsoluteRect(ViewInfo.Bounds));
end;

procedure TdxSpreadSheetContainerCustomDragAndDropObject.RecalculateDragAndDropInfo;
var
  AAccepted: Boolean;
begin
  DragAndDrop(CurMousePos, AAccepted);
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.TranslateCoords(const P: TPoint): TPoint;
begin
  Result := cxPointOffset(inherited TranslateCoords(P), ContentOrigin);
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.ToAbsoluteRect(const R: TRect): TRect;
begin
  Result := cxRectOffset(R, ContentOrigin);
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.ToContentRect(const R: TRect): TRect;
begin
  Result := cxRectOffset(R, ContentOrigin, False);
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.CheckIsKeyPressed(const Index: Integer): Boolean;
begin
  Result := GetAsyncKeyState(Index) < 0;
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.GetContainer: TdxSpreadSheetContainer;
begin
  Result := ViewInfo.Owner;
end;

function TdxSpreadSheetContainerCustomDragAndDropObject.GetPivotPoint: TPoint;
begin
  Result := cxRectCenter(Bounds);
end;

{ TdxSpreadSheetContainerSelectionCellViewInfo }

constructor TdxSpreadSheetContainerSelectionCellViewInfo.Create(AContainerViewInfo: TdxSpreadSheetContainerViewInfo);
begin
  inherited Create(AContainerViewInfo.Owner);
  FContainerViewInfo := AContainerViewInfo;
  FRotateMarkerSize := dxSpreadSheetContainerRotateMarkerSize;
  FSizeMarkerSize := dxSpreadSheetContainerSizingMarkerSize;
  FSupportedDrawingStages := [dsSecond];
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.HasResizeMarkers: Boolean;
begin
  Result := ContainerViewInfo.Owner.CanResize;
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.HasRotateMarker: Boolean;
begin
  Result := ContainerViewInfo.Owner.CanRotate;
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
const
  CursorMap: array[0..7, TdxSpreadSheetSizingMarker] of TCursor = (
    (crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW),
    (crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE),
    (crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE),
    (crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS),

    (crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW),
    (crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE),
    (crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE),
    (crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS)
  );
var
  AAngleIndex: Integer;
begin
  if AHitTest.HitObjectData < 0 then
    Result := crdxSpreadSheetRotate
  else
  begin
    AAngleIndex := Round(ContainerViewInfo.Transform.RotationAngle / 45) mod 8;
    if AAngleIndex < 0 then
      Inc(AAngleIndex, 8);
    Result := CursorMap[AAngleIndex, TdxSpreadSheetSizingMarker(AHitTest.HitObjectData)];
  end;
end;

procedure TdxSpreadSheetContainerSelectionCellViewInfo.Invalidate;
begin
  InvalidateRect(RealDrawingBounds);
end;

procedure TdxSpreadSheetContainerSelectionCellViewInfo.CalculateBounds;
begin
  FCalculated := True;
  FVisible := cxRectIntersect(FClipRect, RealDrawingBounds, FScreenClipRect);
  FSizeMarkersArea := Bounds;
  if HasRotateMarker then
    Inc(FSizeMarkersArea.Top, SizingPointSize + RotateMarkerSize);
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.CanDraw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage): Boolean;
begin
  Result := (AStage in FSupportedDrawingStages) and ContainerViewInfo.CanDrawSelection;
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.CreatePen: TdxGpPen;
begin
  Result := TdxGPPen.Create;
  Result.Brush.Color := dxSpreadSheetContainerSelectionFrameColor;
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.CreateSizingMarkerBrush: TdxGpBrush;
begin
  Result := TdxGPBrush.Create;
  Result.Style := gpbsGradient;
  Result.GradientMode := gpbgmVertical;
  Result.GradientPoints.Add(0, dxSpreadSheetContainerSizingMarkerColor1);
  Result.GradientPoints.Add(0.5, dxSpreadSheetContainerSizingMarkerColor2);
  Result.GradientPoints.Add(1, dxSpreadSheetContainerSizingMarkerColor1);
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.CreateRotateMarkerBrush: TdxGpBrush;
begin
  Result := TdxGPBrush.Create;
  Result.Style := gpbsGradient;
  Result.GradientMode := gpbgmVertical;
  Result.GradientPoints.Add(0, dxSpreadSheetContainerRotateMarkerColor1);
  Result.GradientPoints.Add(0.5, dxSpreadSheetContainerRotateMarkerColor2);
  Result.GradientPoints.Add(1, dxSpreadSheetContainerRotateMarkerColor1);
end;

procedure TdxSpreadSheetContainerSelectionCellViewInfo.DoDraw(ACanvas: TcxCanvas);
const
  Map: array[Boolean] of TdxGPSmoothingMode = (smHighSpeed, smAntiAlias);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, RealDrawingBounds);
  try
    dxGPPaintCanvas.SmoothingMode := Map[SpreadSheet.OptionsView.Antialiasing];
    dxGPPaintCanvas.SetWorldTransform(ContainerViewInfo.TransformMatrix);
    DrawContent(dxGPPaintCanvas);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxSpreadSheetContainerSelectionCellViewInfo.DrawContent(ACanvas: TdxGPCanvas);
var
  APen: TdxGPPen;
begin
  ACanvas.SaveClipRegion;
  try
    APen := CreatePen;
    try
      if HasResizeMarkers then
        DrawSizingMarkers(ACanvas, APen);
      if HasRotateMarker then
        DrawRotateMarker(ACanvas, APen);
      ACanvas.Rectangle(FrameRect, APen, nil);
    finally
      APen.Free;
    end;
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxSpreadSheetContainerSelectionCellViewInfo.DrawRotateMarker(ACanvas: TdxGPCanvas; APen: TdxGPPen);
var
  ABrush: TdxGPBrush;
  ARect: TRect;
begin
  ABrush := CreateRotateMarkerBrush;
  try
    ARect := RotateMarker;
    ACanvas.Ellipse(ARect, APen, ABrush);
    ACanvas.SetClipRect(ARect, gmExclude);
    ACanvas.Line(cxRectCenter(ARect).X, ARect.Bottom, cxRectCenter(ARect).X, FrameRect.Top, APen);
  finally
    ABrush.Free;
  end;
end;

procedure TdxSpreadSheetContainerSelectionCellViewInfo.DrawSizingMarkers(ACanvas: TdxGPCanvas; APen: TdxGPPen);
var
  ABrush: TdxGPBrush;
  AMarker: TdxSpreadSheetSizingMarker;
  ARect: TRect;
begin
  ABrush := CreateSizingMarkerBrush;
  try
    for AMarker := Low(AMarker) to High(AMarker) do
    begin
      ARect := SizeMarkerRect[AMarker];
      ACanvas.Rectangle(ARect, APen, ABrush);
      ACanvas.SetClipRect(ARect, gmExclude);
    end;
  finally
    ABrush.Free;
  end;
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.GetContentOffsets: TRect;
var
  AHalfSize: Integer;
begin
  AHalfSize := SizingPointSize div 2;
  Result := cxRect(AHalfSize, AHalfSize, AHalfSize, AHalfSize);
  if HasRotateMarker then
    Inc(Result.Top, RotateMarkerSize + SizingPointSize);
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.GetDragAndDropObjectClass(
  AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass;
begin
  if AHitTest.HitObjectData < 0 then
    Result := TdxSpreadSheetContainerRotateDragAndDropObject
  else
    Result := TdxSpreadSheetContainerResizeDragAndDropObject;
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.GetFrameRect: TRect;
begin
  Result := cxRectInflate(SizeMarkersArea, -SizingPointSize div 2);
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.GetRealDrawingBounds: TRect;
begin
  Result := cxRectInflate(cxRect(ContainerViewInfo.TransformMatrix.GetBoundingRectangle(dxRectF(Bounds))), 1);
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.GetRotateMarker: TRect;
begin
  if HasRotateMarker then
    Result := cxRectCenterHorizontally(cxRectSetHeight(Bounds, RotateMarkerSize), RotateMarkerSize)
  else
    Result := cxNullRect;
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.GetSizeMarkerAtLocalPoint(
  const ALocalPoint: TPoint; out AMarker: TdxSpreadSheetSizingMarker): Boolean;
var
  AIndex: TdxSpreadSheetSizingMarker;
begin
  Result := False;
  for AIndex := Low(TdxSpreadSheetSizingMarker) to High(TdxSpreadSheetSizingMarker) do
    if PtInRect(SizeMarkerRect[AIndex], ALocalPoint) then
    begin
      AMarker := AIndex;
      Result := True;
      Break;
    end;
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.GetSizeMarkerRect(AMarker: TdxSpreadSheetSizingMarker): TRect;
begin
  if HasResizeMarkers then
  begin
    Result := cxRectSetSize(SizeMarkersArea, SizingPointSize, SizingPointSize);
    if AMarker in [smTopRight, smBottomRight] then
      Result := cxRectSetRight(Result, SizeMarkersArea.Right);
    if AMarker in [smBottomLeft, smBottomRight] then
      Result := cxRectSetBottom(Result, SizeMarkersArea.Bottom);

    if AMarker in [smLeft, smRight] then
      Result := cxRectOffset(Result, 0, (cxRectHeight(SizeMarkersArea) - SizingPointSize) div 2)
    else
      if AMarker = smBottom then
        Result := cxRectOffset(Result, 0, cxRectHeight(SizeMarkersArea) - SizingPointSize);

    if AMarker in [smTop, smBottom] then
      Result := cxRectOffset(Result, (cxRectWidth(SizeMarkersArea) - SizingPointSize) div 2, 0)
    else
      if AMarker = smRight then
        Result := cxRectOffset(Result, cxRectWidth(SizeMarkersArea) - SizingPointSize, 0);
  end
  else
    Result := cxNullRect;
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := ContainerViewInfo.SpreadSheet;
end;

function TdxSpreadSheetContainerSelectionCellViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
var
  ALocalPoint: TPoint;
  AMarker: TdxSpreadSheetSizingMarker;
begin
  ALocalPoint := ContainerViewInfo.TransformMatrixInv.TransformPoint(AHitTest.ActualHitPoint);

  Result := PtInRect(RotateMarker, ALocalPoint);
  if Result then
  begin
    AHitTest.HitObject := Self;
    AHitTest.HitObjectData := -1;
    AHitTest.HitCodes[hcContainerSelection] := True;
  end
  else
    if GetSizeMarkerAtLocalPoint(ALocalPoint, AMarker) then
    begin
      AHitTest.HitObject := Self;
      AHitTest.HitObjectData := Ord(AMarker);
      AHitTest.HitCodes[hcContainerSelection] := True;
      Result := True;
    end;
end;

procedure TdxSpreadSheetContainerSelectionCellViewInfo.InvalidateRect(const R: TRect);
begin
  ContainerViewInfo.InvalidateRect(R);
end;

{ TdxSpreadSheetResizeTableItemDragAndDropObject }

constructor TdxSpreadSheetResizeTableItemDragAndDropObject.Create(AControl: TcxControl);
var
  AHeaderViewInfo: TdxSpreadSheetTableViewHeaderCellViewInfo;
begin
  inherited Create(AControl);
  StartPos := Control.GetMouseCursorClientPos;

  if HitTest.HitAtColumnHeader then
    Items := View.Columns
  else
    if HitTest.HitAtRowHeader then
      Items := View.Rows
    else
    begin
      AControl.FinishDragAndDrop(False);
      Exit;
    end;

  AHeaderViewInfo := TdxSpreadSheetTableViewHeaderCellViewInfo(HitTest.HitObject);
  ItemIndex := AHeaderViewInfo.Index;
  if Items.GetItemVisible(ItemIndex + 1) then
    ItemStartPos := AHeaderViewInfo.Bounds.TopLeft
  else
    ItemStartPos := cxPoint(AHeaderViewInfo.Bounds.Left, AHeaderViewInfo.Bounds.Bottom);

  ItemStartPos := cxPointOffset(ItemStartPos, cxSimplePoint, False);
  ItemStartPos := cxPointOffset(ItemStartPos, View.GetPartOffsetByPoint(StartPos), False);
  ItemStartPos := cxPointScale(ItemStartPos, View.ZoomFactor, 100);
end;

procedure TdxSpreadSheetResizeTableItemDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  PrevMousePos := StartPos;
  DrawSizingMark(GetSizingMarkBounds(StartPos));
  DrawSizingMark(GetSizingMarkBounds(ItemStartPos));
end;

procedure TdxSpreadSheetResizeTableItemDragAndDropObject.ChangePosition;
begin
  PrevMousePos := CheckPoint(PrevMousePos);
  CurMousePos := CheckPoint(CurMousePos);
  if GetDistance(PrevMousePos, CurMousePos) <> 0 then
  begin
    DrawSizingMark(GetSizingMarkBounds(PrevMousePos));
    ChangeMousePos(CurMousePos);
    DrawSizingMark(GetSizingMarkBounds(CurMousePos));
  end;
end;

function TdxSpreadSheetResizeTableItemDragAndDropObject.CheckPoint(const P: TPoint): TPoint;
var
  R: TRect;
begin
  R := View.ViewInfo.CellsArea;
  R.Right := Max(R.Left, View.ViewInfo.Bounds.Right);
  R.Bottom := Max(R.Bottom, View.ViewInfo.Bounds.Bottom);
  R := cxRectScale(R, View.ZoomFactor, 100);
  Result.X := Max(R.Left, Min(P.X, R.Right));
  Result.Y := Max(R.Top, Min(P.Y, R.Bottom));
end;

procedure TdxSpreadSheetResizeTableItemDragAndDropObject.DrawSizingMark(const ARect: TRect);
begin
  Canvas.SaveState;
  try
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Mode := pmXor;
    if Orientation = orHorizontal then
    begin
      Canvas.MoveTo(ARect.Left, ARect.Top);
      Canvas.LineTo(ARect.Left, ARect.Bottom);
    end
    else
    begin
      Canvas.MoveTo(ARect.Left, ARect.Top);
      Canvas.LineTo(ARect.Right, ARect.Top);
    end;
  finally
    Canvas.RestoreState;
  end;
end;

function TdxSpreadSheetResizeTableItemDragAndDropObject.GetDistance(const P1, P2: TPoint): Integer;
begin
  Result := P1.X - P2.X;
  if Orientation <> orHorizontal then
    Result := P1.Y - P2.Y;
end;

function TdxSpreadSheetResizeTableItemDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
const
  Cursors: array[TdxOrientation] of TCursor = (crcxHorzSize, crcxVertSize);
begin
  Result := Cursors[Orientation];
end;

function TdxSpreadSheetResizeTableItemDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetResizeTableItemDragAndDropObject.GetSizingMarkBounds(const P: TPoint): TRect;
begin
  if Orientation = orHorizontal then
    Result := cxRectSetLeft(Control.ClientBounds, P.X, 1)
  else
    Result := cxRectSetTop(Control.ClientBounds, P.Y, 1);
end;

function TdxSpreadSheetResizeTableItemDragAndDropObject.IsEntireSheetArea(const R: TRect): Boolean;
begin
  Result := cxPointIsEqual(R.TopLeft, cxNullPoint) and
    (R.Right >= dxSpreadSheetMaxColumnIndex) and (R.Bottom >= dxSpreadSheetMaxRowIndex);
end;

procedure TdxSpreadSheetResizeTableItemDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  inherited DragAndDrop(P, Accepted);
  ChangePosition;
end;

procedure TdxSpreadSheetResizeTableItemDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
var
  I: Integer;
  ARect, AEntireArea: TRect;
  ADelta: Integer;
  AItemSize: Integer;
  AFullItemSelected, AEntireAreaSelected: Boolean;
begin
  ADelta := MulDiv(GetDistance(CurMousePos, StartPos), 100, View.ZoomFactor);
  Accepted := Accepted and (ADelta <> 0);
  if Accepted then
  begin
    View.BeginUpdate;
    View.History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
    try
      Items.ResizeItem(ItemIndex, ADelta);
      if Items.GetItemState(ItemIndex) = cxbsHot then
      begin
        AItemSize := Items.GetItemSize(ItemIndex);
        if Orientation = orHorizontal then
          AFullItemSelected := View.Selection.IsEntireColumnSelected(ItemIndex)
        else
          AFullItemSelected := View.Selection.IsEntireRowSelected(ItemIndex);

        if AFullItemSelected then
        begin
          AEntireArea := dxSpreadSheetEntireSheetArea;
          ARect := View.Selection.Items[0].Rect;
          AEntireAreaSelected := IsEntireSheetArea(ARect);
          if not AEntireAreaSelected then
            for I := 1 to View.Selection.Count - 1 do
            begin
              ARect := dxSpreadSheetCellsUnion(View.Selection.Items[I].Rect, ARect);
              AEntireAreaSelected := AEntireAreaSelected or IsEntireSheetArea(View.Selection.Items[I].Rect);
            end;

          if AEntireAreaSelected then
          begin
            Items.ForEach(
              procedure(AItem: TdxDynamicListItem)
              begin
                TdxSpreadSheetTableItem(AItem).Size := AItemSize;
              end);
            View.History.AddCommand(TdxSpreadSheetHistoryChangeDefaultSizeCommand.Create(Items));
            Items.DefaultSize := AItemSize;
          end
          else
          begin
            cxRectIntersect(ARect, AEntireArea, ARect);
            if Orientation = orVertical then
              ARect := cxRectRotate(ARect);
            for I := ARect.Left to ARect.Right do
              if ((Orientation = orHorizontal) and View.Selection.IsEntireColumnSelected(I)) or
                 ((Orientation = orVertical) and View.Selection.IsEntireRowSelected(I)) then
                   TdxSpreadSheetTableItem(Items.ItemNeeded(I)).Size := AItemSize;
          end;
        end;
      end;
    finally
      View.History.EndAction;
      View.EndUpdate;
    end;
  end;
  View.Invalidate;
end;

function TdxSpreadSheetResizeTableItemDragAndDropObject.TranslateCoords(const P: TPoint): TPoint;
begin
  Result := P;
end;

function TdxSpreadSheetResizeTableItemDragAndDropObject.GetHitTest: TdxSpreadSheetTableViewHitTest;
begin
  Result := View.HitTest;
end;

function TdxSpreadSheetResizeTableItemDragAndDropObject.GetOrientation: TdxOrientation;
begin
  if Items = View.Rows then
    Result := orVertical
  else
    Result := orHorizontal;
end;

function TdxSpreadSheetResizeTableItemDragAndDropObject.GetView: TdxSpreadSheetTableView;
begin
  Result := Control.ActiveSheetAsTable;
end;

{ TdxSpreadSheetTableViewMergedCellViewInfoList }

constructor TdxSpreadSheetTableViewMergedCellViewInfoList.Create;
begin
  inherited Create;
  FIndex := TDictionary<TdxSpreadSheetMergedCell, TdxSpreadSheetTableViewMergedCellViewInfo>.Create;
end;

destructor TdxSpreadSheetTableViewMergedCellViewInfoList.Destroy;
begin
  inherited;
  FreeAndNil(FIndex);
end;

procedure TdxSpreadSheetTableViewMergedCellViewInfoList.Add(AViewInfo: TdxSpreadSheetCellViewInfo);
begin
  FIndex.Add(
    TdxSpreadSheetTableViewMergedCellViewInfo(AViewInfo).MergedCell,
    TdxSpreadSheetTableViewMergedCellViewInfo(AViewInfo));
  inherited Add(AViewInfo);
end;

procedure TdxSpreadSheetTableViewMergedCellViewInfoList.Clear;
begin
  FIndex.Clear;
  inherited;
end;

function TdxSpreadSheetTableViewMergedCellViewInfoList.Contains(ACell: TdxSpreadSheetMergedCell): Boolean;
begin
  Result := FIndex.ContainsKey(ACell);
end;

function TdxSpreadSheetTableViewMergedCellViewInfoList.TryGetValue(
  ACell: TdxSpreadSheetMergedCell; out AViewInfo: TdxSpreadSheetTableViewMergedCellViewInfo): Boolean;
begin
  Result := FIndex.TryGetValue(ACell, AViewInfo);
end;

function TdxSpreadSheetTableViewMergedCellViewInfoList.GetItem(Index: Integer): TdxSpreadSheetTableViewMergedCellViewInfo;
begin
  Result := TdxSpreadSheetTableViewMergedCellViewInfo(inherited Items[Index]);
end;

{ TdxSpreadSheetTableViewCustomCellViewInfo }

function TdxSpreadSheetTableViewCustomCellViewInfo.DoCustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  SpreadSheet.DoCustomDrawTableViewCommonCell(View, ACanvas, Self, Result);
end;

function TdxSpreadSheetTableViewCustomCellViewInfo.GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass;
begin
  Result := TdxSpreadSheetBuiltInTableViewPopupMenu;
end;

function TdxSpreadSheetTableViewCustomCellViewInfo.GetPainter: TdxSpreadSheetTableViewPainter;
begin
  Result := ViewInfo.Painter;
end;

function TdxSpreadSheetTableViewCustomCellViewInfo.GetViewInfo: TdxSpreadSheetTableViewInfo;
begin
  Result := TdxSpreadSheetTableViewInfo(inherited Owner);
end;

function TdxSpreadSheetTableViewCustomCellViewInfo.GetView: TdxSpreadSheetCustomView;
begin
  Result := ViewInfo.View;
end;

{ TdxSpreadSheetTableViewHeaderCellViewInfo }

procedure TdxSpreadSheetTableViewHeaderCellViewInfo.ApplyBestFit;
begin
  if Item <> nil then
    Item.ApplyBestFit;
end;

function TdxSpreadSheetTableViewHeaderCellViewInfo.DoCustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  DoPrepareCanvas(ACanvas);
  SpreadSheet.DoCustomDrawTableViewHeaderCell(View, ACanvas, Self, Result);
end;

procedure TdxSpreadSheetTableViewHeaderCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  FViewParams.TextColor := GetTextColor;
  DoPrepareCanvas(ACanvas);
  LookAndFeelPainter.DrawSpreadSheetScaledHeader(ACanvas, Bounds, cxTextRect(Bounds), Neighbors,
    Borders, State, AlignHorz, AlignVert, False, False, DisplayText, ACanvas.Font, ViewParams.TextColor,
    ViewParams.Color, ScaleFactor, TcxDrawBackgroundEvent(nil), IsLast);
end;

procedure TdxSpreadSheetTableViewHeaderCellViewInfo.DoPrepareCanvas(ACanvas: TcxCanvas);
begin
  Painter.PrepareCanvasFont(ACanvas, ViewParams.Font);
  ACanvas.Font.Color := ViewParams.TextColor;
  ACanvas.Brush.Color := ViewParams.Color;
end;

function TdxSpreadSheetTableViewHeaderCellViewInfo.GetBorders: TcxBorders;
begin
  Result := LookAndFeelPainter.HeaderBorders(Neighbors);
  if not Items.GetItemVisible(Index - 1) then
  begin
    if Neighbors * [nLeft, nRight] <> [] then
      Include(Result, bLeft)
    else
      Include(Result, bTop);
  end;
end;

function TdxSpreadSheetTableViewHeaderCellViewInfo.GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass;
begin
  if AHitTest.GetHitCode(hcRowHeader or hcColumnHeader) and AHitTest.GetHitCode(hcResizeArea) then
    Result := TdxSpreadSheetResizeTableItemDragAndDropObject
  else
    Result := nil;
end;

function TdxSpreadSheetTableViewHeaderCellViewInfo.GetTextColor: TColor;
const
  ColorMap: array[Boolean] of TColor = (clBlack, clRed);
begin
  Result := cxGetActualColor(ViewParams.TextColor, ColorMap[State = cxbsHot]);
end;

procedure TdxSpreadSheetTableViewHeaderCellViewInfo.Initialize(
  AItem: TdxSpreadSheetTableItem; AIndex: Integer; ANeighbors: TcxNeighbors; const ADisplayText: string);
begin
  FDisplayText := ADisplayText;
  FViewParams := TdxSpreadSheetTableViewInfo(Owner).HeaderParams;
  FAlignHorz := taCenter;
  FAlignVert := cxClasses.vaCenter;
  FIndex := AIndex;
  FNeighbors := ANeighbors;
  FItem := AItem;
  FIsFirst := Index = -1;
  FIsLast := [nRight, nBottom] * FNeighbors = [];
end;

function TdxSpreadSheetTableViewHeaderCellViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;

  function IsResizeArea: Boolean;
  var
    ADelta: Integer;
  begin
    ADelta := 2 * dxSpreadSheetResizeDelta;
    if (FNextNeighbor <> nil) and (FNextNeighbor.Item <> nil) and (FNextNeighbor.Item.Size < ADelta) then
      ADelta := dxSpreadSheetResizeDelta;
    Result := cxRectPtIn(GetResizeArea(ADelta), AHitTest.ActualHitPoint);
  end;

begin
  Result := inherited InitHitTest(AHitTest);
  if Index >= 0 then
  begin
    AHitTest.SetHitCode(hcResizeArea, AllowResize and IsResizeArea);
    Result := Result or AHitTest.GetHitCode(hcResizeArea);
    if Result then
      AHitTest.HitObject := Self;
  end;
end;

procedure TdxSpreadSheetTableViewHeaderCellViewInfo.UpdateState;
begin
  State := Items.GetItemState(Index);
end;

procedure TdxSpreadSheetTableViewHeaderCellViewInfo.SetState(AValue: TcxButtonState);
begin
  if AValue <> FState then
  begin
    FState := AValue;
    Invalidate;
  end;
end;

{ TdxSpreadSheetTableViewColumnHeaderCellViewInfo }

function TdxSpreadSheetTableViewColumnHeaderCellViewInfo.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
begin
  if AHitTest.GetHitCode(hcResizeArea) then
    Result := crcxHorzSize
  else
    Result := crdxSpreadSheetDownArrow;
end;

function TdxSpreadSheetTableViewColumnHeaderCellViewInfo.AllowResize: Boolean;
begin
  Result := View.OptionsProtection.ActualAllowResizeColumns;
end;

function TdxSpreadSheetTableViewColumnHeaderCellViewInfo.GetItems: TdxSpreadSheetTableItems;
begin
  Result := View.Columns;
end;

function TdxSpreadSheetTableViewColumnHeaderCellViewInfo.GetResizeArea(const AResizeAreaSize: Integer): TRect;
begin
  Result := cxRectSetLeft(Bounds, Bounds.Right - dxSpreadSheetResizeDelta, AResizeAreaSize);
end;

function TdxSpreadSheetTableViewColumnHeaderCellViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := inherited InitHitTest(AHitTest);
  if Result then
    AHitTest.SetHitCode(hcColumnHeader, True);
end;

{ TdxSpreadSheetTableViewHeaderCornerCellViewInfo }

function TdxSpreadSheetTableViewHeaderCornerCellViewInfo.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
begin
  Result := crdxSpreadSheetCross;
end;

{ TdxSpreadSheetTableViewRowHeaderCellViewInfo }

function TdxSpreadSheetTableViewRowHeaderCellViewInfo.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
begin
  if AHitTest.GetHitCode(hcResizeArea) then
    Result := crcxVertSize
  else
    Result := crdxSpreadSheetLeftArrow;
end;

function TdxSpreadSheetTableViewRowHeaderCellViewInfo.AllowResize: Boolean;
begin
  Result := View.OptionsProtection.ActualAllowResizeRows;
end;

function TdxSpreadSheetTableViewRowHeaderCellViewInfo.GetItems: TdxSpreadSheetTableItems;
begin
  Result := View.Rows;
end;

function TdxSpreadSheetTableViewRowHeaderCellViewInfo.GetResizeArea(const AResizeAreaSize: Integer): TRect;
begin
  Result := cxRectSetTop(Bounds, Bounds.Bottom - dxSpreadSheetResizeDelta, AResizeAreaSize);
end;

function TdxSpreadSheetTableViewRowHeaderCellViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := inherited InitHitTest(AHitTest);
  if Result then
    AHitTest.SetHitCode(hcRowHeader, True);
end;

{ TdxSpreadSheetTableViewCellDisplayStyle }

function TdxSpreadSheetTableViewCellDisplayStyle.CreateDataBar: TdxSpreadSheetCellDataBar;
begin
  Result := TdxSpreadSheetTableViewCellDataBar.Create;
end;

function TdxSpreadSheetTableViewCellDisplayStyle.GetDataBar: TdxSpreadSheetTableViewCellDataBar;
begin
  Result := TdxSpreadSheetTableViewCellDataBar(inherited DataBar);
end;

{ TdxSpreadSheetTableViewCustomDataCellViewInfo }

constructor TdxSpreadSheetTableViewCustomDataCellViewInfo.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FSupportedDrawingStages := [dsSecond];
end;

destructor TdxSpreadSheetTableViewCustomDataCellViewInfo.Destroy;
begin
  FreeAndNil(FStyleViewInfo);
  FreeAndNil(FStyle);
  inherited;
end;

procedure TdxSpreadSheetTableViewCustomDataCellViewInfo.Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage);
begin
  FCanvasFontPrepared := False;
  inherited;
end;

procedure TdxSpreadSheetTableViewCustomDataCellViewInfo.CalculateBounds;
var
  ARect: TRect;
begin
  ARect := cxRectInflate(FBounds, dxSpreadSheetMaxBorderSize);
  if not cxRectIsEmpty(ContentBounds) then
    ARect := cxRectUnion(ARect, ContentBounds);
  FVisible := cxRectIntersect(FClipRect, ARect, FScreenClipRect);
end;

procedure TdxSpreadSheetTableViewCustomDataCellViewInfo.CalculateDisplayTextParameters(const R: TRect; ACell: TdxSpreadSheetCell);
var
  ADisplayValue: TdxSpreadSheetCellDisplayValue;
  AFont: TFont;
  AHelper: TdxAdjustFontSizeHelper;
  APrevPixelsPerInch: Integer;
  APrevTransform: TXForm;
begin
  if Style.ShowCellValue then
    FDisplayText := ACell.DisplayText
  else
    FDisplayText := '';

  if FDisplayText = '' then
  begin
    FTextBounds := cxRectSetWidth(R, 0);
    Exit;
  end;

  AFont := Style.Handle.Font.GraphicObject;
  if Style.ShrinkToFit and not Style.WordWrap then
  begin
    if ACell.FDisplayValue = nil then
      ACell.FDisplayValue := TdxSpreadSheetCellDisplayValue.Create;

    ADisplayValue := ACell.FDisplayValue;
    if ADisplayValue.ShrinkToFitCacheContentWidth <> R.Width then
    begin
      if (ADisplayValue.ShrinkToFitCacheFontSize <> AFont.Size) or (R.Width < ADisplayValue.ShrinkToFitCacheContentWidth) then
      begin
        AHelper := TdxAdjustFontSizeHelper.Create;
        try
          AHelper.ZoomFactor := View.ZoomFactor;
          AHelper.Font.Assign(AFont);
          AHelper.Calculate(R.Width, FDisplayText);
          ADisplayValue.ShrinkToFitCacheFontSize := AHelper.Font.Size;
        finally
          AHelper.Free;
        end;
      end;
      ADisplayValue.ShrinkToFitCacheContentWidth := R.Width;
    end;
    FFontSize := ADisplayValue.ShrinkToFitCacheFontSize;
  end;

  FTextBounds := R;
  APrevPixelsPerInch := dxSpreadSheetPrepareCanvas(cxScreenCanvas, AFont, dxDefaultDPI);
  try
    cxScreenCanvas.Font.Size := FontSize;
    if View.ZoomFactor <> 100 then
    begin
      dxSetZoomFactor(cxScreenCanvas, View.ZoomFactor, APrevTransform);
      dxSpreadSheetTextService.CalculateTextBounds(cxScreenCanvas, Self, FTextBounds);
      SetWorldTransform(cxScreenCanvas.Handle, APrevTransform);
    end
    else
      dxSpreadSheetTextService.CalculateTextBounds(cxScreenCanvas, Self, FTextBounds);
  finally
    dxSpreadSheetUnprepareCanvas(cxScreenCanvas, APrevPixelsPerInch);
  end;

  case AlignHorz of
    taLeft:
      FTextBounds := cxRectSetLeft(FTextBounds, R.Left);
    taRight:
      FTextBounds := cxRectSetRight(FTextBounds, R.Right);
    taCenterX:
      FTextBounds := cxRectSetLeft(FTextBounds, cxRectCenter(R).X - FTextBounds.Width div 2, FTextBounds.Width);
  else
    FTextBounds := cxRectSetXPos(FTextBounds, R.Left, R.Right);
  end;

  if AlignVert = taCenterY then
    FTextBounds := cxRectSetTop(FTextBounds, cxRectCenter(R).Y - FTextBounds.Height div 2, FTextBounds.Height);

  if FIsNumeric then
    FTextBounds.Left := Max(FTextBounds.Left, R.Left);
end;

function TdxSpreadSheetTableViewCustomDataCellViewInfo.CreateStyleViewInfo: TdxSpreadSheetConditionalFormattingStyleViewInfo;
begin
  Result := TdxSpreadSheetConditionalFormattingStyleViewInfo.Create(Style, dxDefaultScaleFactor);
end;

procedure TdxSpreadSheetTableViewCustomDataCellViewInfo.DoCalculate;
var
  ABrush: TdxSpreadSheetBrushHandle;
begin
  ABrush := Style.Handle.Brush;
  FBackgroundColor := dxSpreadSheetGetColorDefault(ABrush.BackgroundColor, ViewInfo.ContentParams.Color);
  FForegroundColor := dxSpreadSheetGetColorDefault(ABrush.ForegroundColor, ViewInfo.ContentParams.TextColor);
  inherited DoCalculate;
end;

procedure TdxSpreadSheetTableViewCustomDataCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if DrawingStage = dsSecond then
    DrawValue(ACanvas);
end;

procedure TdxSpreadSheetTableViewCustomDataCellViewInfo.DrawValue(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  if View.IsEditing and IsEditing then
    Exit;
  if cxRectIsEmpty(FTextBounds) and cxRectIsEmpty(StyleViewInfo.IconBounds) then
    Exit;
  if not cxRectIntersect(R, ContentBounds, FScreenClipRect) then
    Exit;
  if ACanvas.RectVisible(R) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(R);
      StyleViewInfo.Draw(ACanvas, dsSecond);
      if not cxRectIsEmpty(FTextBounds) then
      begin
        PrepareCanvasFont(ACanvas);
        dxSpreadSheetTextService.DrawValue(ACanvas, Self, FTextBounds);
      end;
      Dec(R.Bottom, 2);
    finally
      ACanvas.RestoreClipRegion;
      ACanvas.ExcludeClipRect(R);
    end;
  end;
end;

function TdxSpreadSheetTableViewCustomDataCellViewInfo.GetIsEditing: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetTableViewCustomDataCellViewInfo.GetTextOutFormat: Integer;
begin
  Result := cxMakeFormat(AlignHorz, AlignVert);
  if Multiline then
    Result := Result or CXTO_WORDBREAK or CXTO_PREVENT_TOP_EXCEED;
  if IsNumeric then
    Result := Result or CXTO_PREVENT_LEFT_EXCEED;
end;

procedure TdxSpreadSheetTableViewCustomDataCellViewInfo.InitDrawValueCore(ACell: TdxSpreadSheetCell);
const
  GeneralAlignToTextAlign: array[TdxSpreadSheetCellDataType] of TcxTextAlignX =
    (taLeft, taCenterX, taCenterX, taRight, taRight, taRight, taRight, taLeft, taLeft);
  HorzAlignToTextAlignH: array[TdxSpreadSheetDataAlignHorz] of TcxTextAlignX =
    (taLeft, taLeft, taCenterX, taRight, taLeft, taJustifyX, taDistributeX);
  VertAlignToTextAlignY: array[TdxSpreadSheetDataAlignVert] of TcxTextAlignY =
    (taTop, taCenterY, taBottom, taDistributeY, taDistributeY);
var
  AFont: TdxSpreadSheetFontHandle;
  R: TRect;
begin
  AFont := Style.Handle.Font;
  if ACell = nil then
  begin
    FTextColor := cxGetActualColor(AFont.Color, ViewInfo.ContentParams.TextColor);
    FTextBounds := cxNullRect;
    FIsNumeric := False;
  end
  else
  begin
    FFontSize := AFont.Size;
    FTextColor := cxGetActualColor(ACell.DisplayValue.Color, AFont.Color);
    FTextColor := cxGetActualColor(FTextColor, ViewInfo.ContentParams.TextColor);
    FAlignHorz := HorzAlignToTextAlignH[Style.AlignHorz];
    FAlignVert := VertAlignToTextAlignY[Style.AlignVert];
    FIsNumeric := ACell.IsNumericValue;
    FMultiline := ACell.IsMultiline;

    R := FContentBounds;
    if Style.AlignHorz in [ssahLeft, ssahDistributed] then
      Inc(R.Left, Style.AlignHorzIndent);
    if Style.AlignHorz in [ssahRight, ssahDistributed] then
      Dec(R.Right, Style.AlignHorzIndent);

    if Style.AlignHorz = ssahGeneral then
      FAlignHorz := GeneralAlignToTextAlign[ACell.ActualDataType]
    else
      if Style.AlignHorz = ssahFill then
      begin
        FAlignVert := taBottom;
        FMultiline := False;
      end;

    StyleViewInfo.Calculate(FContentBounds, R);
    StyleViewInfo.BackgroundColor := BackgroundColor;
    CalculateDisplayTextParameters(StyleViewInfo.TextRect, ACell);
  end;
end;

procedure TdxSpreadSheetTableViewCustomDataCellViewInfo.InitStyle(AStyleHandle: TdxSpreadSheetCellStyleHandle);
begin
  if FStyle <> nil then
    FStyle.Handle := AStyleHandle
  else
  begin
    FStyle := TdxSpreadSheetTableViewCellDisplayStyle.Create(View, AStyleHandle);
    FStyleViewInfo := CreateStyleViewInfo;
  end;
end;

procedure TdxSpreadSheetTableViewCustomDataCellViewInfo.PrepareCanvasFont(ACanvas: TcxCanvas);
var
  AFont: TFont;
begin
  if not FCanvasFontPrepared then
  begin
    FCanvasFontPrepared := True;

    AFont := Style.Handle.Font.GraphicObject;
    ACanvas.Font.PixelsPerInch := AFont.PixelsPerInch;
    ACanvas.Font.Assign(AFont);

    if ACanvas.Font.Size <> FontSize then
      ACanvas.Font.Size := FontSize;
    ACanvas.Font.Color := TextColor;
    ACanvas.Brush.Color := BackgroundColor;
  end;
end;

{ TdxSpreadSheetTableViewCellViewInfo }

constructor TdxSpreadSheetTableViewCellViewInfo.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FSupportedDrawingStages := [dsFirst, dsSecond];
end;

function TdxSpreadSheetTableViewCellViewInfo.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
begin
  case View.Controller.ForcedSelectionMode of
    smRows:
      Result := crdxSpreadSheetLeftArrow;
    smColumns:
      Result := crdxSpreadSheetDownArrow;
  else
    if AHitTest.GetHitCode(hcHyperlink) and View.Controller.CanExecuteHyperlink then
      Result := crHandPoint
    else
      Result := inherited;
  end;
end;

procedure TdxSpreadSheetTableViewCellViewInfo.SetBounds(const AAbsoluteBounds, AScreenClipRect: TRect);
begin
  inherited SetBounds(AAbsoluteBounds, AScreenClipRect);
  Calculate;
end;

function TdxSpreadSheetTableViewCellViewInfo.GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass;
begin
  if View.Controller.EditingController.CanInsertNewAreaViaMouse then
    Result := TdxSpreadSheetTableViewReferenceHighlighterAddAreaDragAndDropObject
  else
    Result := inherited GetDragAndDropObjectClass(AHitTest);
end;

function TdxSpreadSheetTableViewCellViewInfo.GetIsEditing: Boolean;
begin
  Result := View.IsEditing and (Self = View.EditingController.CellViewInfo);
end;

function TdxSpreadSheetTableViewCellViewInfo.HasBorder(ASide: TcxBorder): Boolean;
begin
  Result := FBorderStyle[ASide] <> sscbsNone;
end;

function TdxSpreadSheetTableViewCellViewInfo.HasValue: Boolean;
begin
  Result := (DisplayText <> '') or IsMerged;
end;

procedure TdxSpreadSheetTableViewCellViewInfo.MergeBorder(ASide: TcxBorder; ANeighborStyle: TdxSpreadSheetCellStyleHandle);
var
  ABorderColor: TColor;
  ABorderStyle: TdxSpreadSheetCellBorderStyle;
begin
  if IsInternalBorder(ASide) then
  begin
    ABorderColor := clNone;
    ABorderStyle := sscbsNone;
  end
  else
  begin
    dxSpreadSheetMergeBorderStyle(ASide, Style.Handle, ANeighborStyle, ABorderColor, ABorderStyle);
    if dxSpreadSheetIsColorDefault(ABorderColor) then
    begin
      if ABorderStyle <> sscbsDefault then
        ABorderColor := clBlack
      else
        ABorderColor := ViewInfo.GridLineColor;
    end;
  end;

  if (ABorderStyle = sscbsDefault) and not ViewInfo.HasGridLines then
    ABorderStyle := sscbsNone;

  SetBorderStyle(ASide, ABorderStyle, ABorderColor);
end;

procedure TdxSpreadSheetTableViewCellViewInfo.RemoveBorder(ASide: TcxBorder);
begin
  SetBorderStyle(ASide, sscbsNone, clNone);
end;

procedure TdxSpreadSheetTableViewCellViewInfo.SetBorderStyle(
  ASide: TcxBorder; AStyle: TdxSpreadSheetCellBorderStyle; AColor: TColor);
begin
  FBorderStyle[ASide] := AStyle;
  FBorderColor[ASide] := AColor;
  Calculated := False;
end;

procedure TdxSpreadSheetTableViewCellViewInfo.InitDrawValue;
begin
  Calculate;
  if IsMerged then
    FCellBounds := ViewInfo.GetAreaBounds(Row, Column, MergedCell.Area, Bounds)
  else
    FCellBounds := Bounds;

  FContentBounds := ContentRect(CellBounds);
  InitDrawValueCore(ActualCell);
  UpdateSupportedDrawingStages;
end;

procedure TdxSpreadSheetTableViewCellViewInfo.InitDrawValueCore(ACell: TdxSpreadSheetCell);
begin
  if ACell <> nil then
    View.Containers.FindCommentContainerCore(ACell.RowIndex, ACell.ColumnIndex, FComment);
  inherited;
end;

function TdxSpreadSheetTableViewCellViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := inherited InitHitTest(AHitTest);
  AHitTest.SetHitCode(hcCell, Result);
  if Result then
    AHitTest.SetHitCode(hcHyperlink, View.Hyperlinks.FindItem(Row, Column) <> nil);
end;

function TdxSpreadSheetTableViewCellViewInfo.IsTextOutOfBounds: Boolean;
var
  R: TRect;
begin
  if IsMerged then
    Exit(False);
  R := ContentRect(Bounds);
  Result := (ContentBounds.Left < R.Left) or (ContentBounds.Right > R.Right)
end;

function TdxSpreadSheetTableViewCellViewInfo.IsInternalBorder(ASide: TcxBorder): Boolean;
begin
  Result := IsMerged;
  if Result then
    case ASide of
      bLeft:
        Result := Column <> MergedCell.Area.Left;
      bTop:
        Result := Row <> MergedCell.Area.Top;
      bRight:
        Result := Column <> MergedCell.Area.Right;
      bBottom:
        Result := Row <> MergedCell.Area.Bottom;
    end;
end;

function TdxSpreadSheetTableViewCellViewInfo.IsPermanent: Boolean;
begin
  Result := not View.IsDestroying and IsEditing;
end;

procedure TdxSpreadSheetTableViewCellViewInfo.UpdateSupportedDrawingStages;
var
  ASide: TcxBorder;
begin
  FSupportedDrawingStages := [dsFirst];
  if not cxRectIsEmpty(StyleViewInfo.IconBounds) then
    Include(FSupportedDrawingStages, dsSecond);
  if not cxRectIsEmpty(TextBounds) and HasValue then
    Include(FSupportedDrawingStages, dsSecond);
  for ASide := Low(TcxBorder) to High(TcxBorder) do
  begin
    if HasBorder(ASide) and (FBorderStyle[ASide] <> sscbsDefault) then
      Include(FSupportedDrawingStages, dsSecond);
  end;
end;

procedure TdxSpreadSheetTableViewCellViewInfo.CalculateDisplayStyle;
var
  AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
  APosition: TPoint;
begin
  AConditionalFormatting := View.ConditionalFormatting;
  if AConditionalFormatting.RuleCount > 0 then
  begin
    APosition := ActualCellPosition;
    AConditionalFormatting.CalculateStyle(Style, APosition.Y, APosition.X, ActualCell);
  end;
end;

procedure TdxSpreadSheetTableViewCellViewInfo.CalculateDisplayTextParameters(const R: TRect; ACell: TdxSpreadSheetCell);
begin
  if IsMerged then
    FTextBounds := cxRectSetWidth(R, 0)
  else
  begin
    inherited;
    FContentBounds.Left := Min(FTextBounds.Left, ContentBounds.Left);
    FContentBounds.Right := Max(FTextBounds.Right, ContentBounds.Right);
  end;
end;

function TdxSpreadSheetTableViewCellViewInfo.ContentRect(const R: TRect): TRect;
begin
  Result := cxRectContent(R, TdxSpreadSheetCell.GetContentOffsets(FBorderStyle[bBottom]));
end;

procedure TdxSpreadSheetTableViewCellViewInfo.DoCalculate;
begin
  CalculateDisplayStyle;
  inherited DoCalculate;
  UpdateSupportedDrawingStages;
end;

function TdxSpreadSheetTableViewCellViewInfo.DoCustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := SpreadSheet.IsTableViewCellCustomDrawn;
  if Result then
  begin
    PrepareCanvasFont(ACanvas);
    SpreadSheet.DoCustomDrawTableViewCell(View, ACanvas, Self, Result);
  end;
end;

procedure TdxSpreadSheetTableViewCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if DrawingStage = dsFirst then
  begin
    DrawBackground(ACanvas);
    if ActualCell = Cell then
      StyleViewInfo.Draw(ACanvas, dsFirst);
  end;
  DrawBorders(ACanvas);
  inherited;
end;

procedure TdxSpreadSheetTableViewCellViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  dxSpreadSheetDrawBackground(ACanvas, Bounds, BackgroundColor, ForegroundColor, Style.Brush.Style);
  if Comment <> nil then
    DrawCommentMark(ACanvas);
end;

procedure TdxSpreadSheetTableViewCellViewInfo.DrawBorders(ACanvas: TcxCanvas);
var
  APrevForm: TXForm;
  ASide: TcxBorder;
  ABounds: TRect;
begin
  ABounds := Bounds;
  dxSpreadSheetResetZoomFactor(ACanvas, ABounds, APrevForm);
  try
    for ASide := Low(TcxBorder) to High(TcxBorder) do
      if HasBorder(ASide) and ((DrawingStage = dsFirst) = (BorderStyle[ASide] = sscbsDefault)) then
      begin
        dxSpreadSheetDrawBorder(ACanvas, dxSpreadSheetGetBorderBounds(ABounds, ASide, FBorderStyle),
          BorderColor[ASide], BackgroundColor, BorderStyle[ASide], ASide in [bTop, bBottom]);
      end;
  finally
    dxSpreadSheetRestoreZoomFactor(ACanvas, APrevForm);
  end;
end;

procedure TdxSpreadSheetTableViewCellViewInfo.DrawCommentMark(ACanvas: TcxCanvas);
const
  Size = 4;
var
  APoints: array [0..2] of TPoint;
begin
  APoints[0] := cxPoint(CellBounds.Right - 1, CellBounds.Top + 1);
  APoints[1] := cxPointOffset(APoints[0], -Size, 0);
  APoints[2] := cxPointOffset(APoints[0], 0, Size);

  ACanvas.Pen.Color := CommentMarkColor;
  ACanvas.Brush.Color := CommentMarkColor;
  ACanvas.Polygon(APoints);
end;

function TdxSpreadSheetTableViewCellViewInfo.GetActualCell: TdxSpreadSheetCell;
begin
  if IsMerged then
    Result := MergedCell.Cell
  else
    Result := Cell;
end;

function TdxSpreadSheetTableViewCellViewInfo.GetActualCellPosition: TPoint;
begin
  if IsMerged then
    Result := MergedCell.Area.TopLeft
  else
    Result := cxPoint(Column, Row);
end;

function TdxSpreadSheetTableViewCellViewInfo.GetBorderColor(ASide: TcxBorder): TColor;
begin
  Result := FBorderColor[ASide];
end;

function TdxSpreadSheetTableViewCellViewInfo.GetBorderStyle(ASide: TcxBorder): TdxSpreadSheetCellBorderStyle;
begin
  Result := FBorderStyle[ASide];
end;

function TdxSpreadSheetTableViewCellViewInfo.GetCommentMarkColor: TColor;
begin
  Result := cxGetActualColor(SpreadSheet.OptionsView.CommentMarkColor, clRed);
end;

function TdxSpreadSheetTableViewCellViewInfo.GetIsMerged: Boolean;
begin
  Result := FMergedCell <> nil;
end;

{ TdxSpreadSheetTableViewFrozenPaneSeparatorCellViewInfo }

function TdxSpreadSheetTableViewFrozenPaneSeparatorCellViewInfo.DoCustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  ACanvas.SetBrushColor(Color);
  Result := inherited DoCustomDraw(ACanvas);
end;

procedure TdxSpreadSheetTableViewFrozenPaneSeparatorCellViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  APrevForm: TXForm;
  ARect: TRect;
begin
  ARect := Bounds;
  dxSpreadSheetResetZoomFactor(ACanvas, ARect, APrevForm);
  try
    ACanvas.FillRect(ARect);
    ACanvas.ExcludeClipRect(ARect);
  finally
    dxSpreadSheetRestoreZoomFactor(ACanvas, APrevForm);
  end;
end;

function TdxSpreadSheetTableViewFrozenPaneSeparatorCellViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := inherited InitHitTest(AHitTest);
  AHitTest.SetHitCode(hcFrozenPaneSeparator, Result);
end;

{ TdxSpreadSheetPageControlHitTest }

procedure TdxSpreadSheetPageControlHitTest.Calculate(const AHitPoint: TPoint);
begin
  inherited Calculate(AHitPoint);
  PageControl.ViewInfo.InitHitTest(Self);
end;

function TdxSpreadSheetPageControlHitTest.GetPageControl: TdxSpreadSheetPageControl;
begin
  Result := TdxSpreadSheetPageControl(Owner);
end;

{ TdxSpreadSheetPageControlController }

function TdxSpreadSheetPageControlController.GetHitTest: TdxSpreadSheetCustomHitTest;
begin
  Result := PageControl.HitTest;
end;

procedure TdxSpreadSheetPageControlController.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if HitTest.HitAtPageTab then
    TdxSpreadSheetPageControlTabCellViewInfo(HitTest.HitObject).View.Active := True
  else
    if HitTest.HitAtButton then
    begin
      if Button = mbLeft then
        TdxSpreadSheetPageControlButtonCellViewInfo(HitTest.HitObject).DoClick;
    end;
end;

procedure TdxSpreadSheetPageControlController.UpdateStates;
begin
  PageControl.ViewInfo.Cells.UpdateState;
end;

function TdxSpreadSheetPageControlController.GetPageControlHitTest: TdxSpreadSheetPageControlHitTest;
begin
  Result := TdxSpreadSheetPageControlHitTest(inherited HitTest);
end;

function TdxSpreadSheetPageControlController.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := PageControl.SpreadSheet;
end;

function TdxSpreadSheetPageControlController.GetPageControl: TdxSpreadSheetPageControl;
begin
  Result := TdxSpreadSheetPageControl(Owner);
end;

procedure TdxSpreadSheetPageControlController.SetPressedObject(AValue: TdxSpreadSheetCellViewInfo);
begin
  if AValue <> FPressedObject then
  begin
    FPressedObject := AValue;
    UpdateStates;
  end;
end;

{ TdxSpreadSheetPageControlBackgroundCellViewInfo }

constructor TdxSpreadSheetPageControlBackgroundCellViewInfo.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  State := cxbsNormal;
  InitViewParams;
end;

function TdxSpreadSheetPageControlBackgroundCellViewInfo.DoCustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := False;
  ACanvas.Font.Assign(ViewParams.Font);
  ACanvas.Font.Color := ViewParams.TextColor;
  ACanvas.Brush.Color := ViewParams.Color;
  if State = cxbsPressed then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsBold]
  else
    ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
end;

procedure TdxSpreadSheetPageControlBackgroundCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  LookAndFeelPainter.DrawScaledScrollBarBackground(ACanvas, Bounds, True, ScaleFactor);
end;

function TdxSpreadSheetPageControlBackgroundCellViewInfo.GetHitCode: Integer;
begin
  Result := hcBackground;
end;

function TdxSpreadSheetPageControlBackgroundCellViewInfo.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := PageControl.SpreadSheet;
end;

function TdxSpreadSheetPageControlBackgroundCellViewInfo.GetState: TcxButtonState;
begin
  Result := FState;
end;

function TdxSpreadSheetPageControlBackgroundCellViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
begin
  Result := inherited InitHitTest(AHitTest);
  AHitTest.SetHitCode(GetHitCode, Result);
  if Result then
  begin
    if GetHitCode = hcBackground then
      Result := False
    else
      AHitTest.SetHitCode(hcBackground, False);
  end;
end;

procedure TdxSpreadSheetPageControlBackgroundCellViewInfo.InitViewParams;
begin
  FViewParams := PageControl.ViewInfo.ViewParams;
end;

procedure TdxSpreadSheetPageControlBackgroundCellViewInfo.UpdateState;
begin
  if (PageControl.Controller.PressedObject = Self) and (PageControl.HitTest.HitObject = Self) then
    State := cxbsPressed
  else if (PageControl.Controller.PressedObject = nil) and (PageControl.HitTest.HitObject = Self) then
    State := cxbsHot
  else
    State := cxbsNormal;
end;

function TdxSpreadSheetPageControlBackgroundCellViewInfo.GetPageControl: TdxSpreadSheetPageControl;
begin
  Result := TdxSpreadSheetPageControlViewInfo(Owner).PageControl;
end;

procedure TdxSpreadSheetPageControlBackgroundCellViewInfo.SetState(AValue: TcxButtonState);
begin
  if (AValue <> State) and (State <> cxbsDisabled) then
  begin
    FState := AValue;
    Invalidate;
  end;
end;

{ TdxSpreadSheetPageControlButtonCellViewInfo }

procedure TdxSpreadSheetPageControlButtonCellViewInfo.DoClick;
begin
  if State = cxbsDisabled then Exit;
  case ButtonType of
    sspcbFirst:
      PageControl.FirstVisiblePageIndex := 0;
    sspcbPrev:
      PageControl.FirstVisiblePageIndex := PageControl.FirstVisiblePageIndex - 1;
    sspcbNext:
      PageControl.FirstVisiblePageIndex := PageControl.FirstVisiblePageIndex + 1;
    sspcbLast:
      PageControl.FirstVisiblePageIndex := PageControl.ViewInfo.Pages.Count - 1;
    sspcbNew:
      SpreadSheet.AddSheet('').Active := True;
  end;
end;

procedure TdxSpreadSheetPageControlButtonCellViewInfo.DoDraw(ACanvas: TcxCanvas);
const
  GlyphIndexMap: array[TdxSpreadSheetPageControlButton] of Integer = (0, 2, 3, 5, 7);
var
  AGlyphRect: TRect;
begin
  LookAndFeelPainter.DrawNavigatorScaledButton(ACanvas, Bounds, State, ViewParams.Color, ScaleFactor);
  LookAndFeelPainter.DrawNavigatorBorder(ACanvas, Bounds, False);

  ACanvas.SaveState;
  try
    AGlyphRect := cxRectCenter(Bounds, LookAndFeelPainter.NavigatorScaledButtonGlyphSize(ScaleFactor));
    if State = cxbsPressed then
      AGlyphRect := cxRectOffset(AGlyphRect, LookAndFeelPainter.NavigatorButtonPressedGlyphOffset);

    LookAndFeelPainter.DrawNavigatorScaledButtonGlyph(ACanvas, NavigatorImages,
      GlyphIndexMap[ButtonType], AGlyphRect, State <> cxbsDisabled, False, ScaleFactor);
  finally
    ACanvas.RestoreState;
  end;
end;

function TdxSpreadSheetPageControlButtonCellViewInfo.GetHitCode: Integer;
begin
  Result := hcButton;
end;

procedure TdxSpreadSheetPageControlButtonCellViewInfo.Initialize(
  const ABounds, AClipRect: TRect; AType: TdxSpreadSheetPageControlButton);
begin
  SetBounds(ABounds, AClipRect);
  FButtonType := AType;
  State := cxbsNormal;
  Calculate;
end;

procedure TdxSpreadSheetPageControlButtonCellViewInfo.UpdateState;
begin
  inherited UpdateState;
  case ButtonType of
    sspcbNew:
      if not SpreadSheet.OptionsProtection.ActualAllowChangeStructure then
        State := cxbsDisabled;

    sspcbFirst, sspcbPrev:
      if PageControl.FirstVisiblePageIndex = 0 then
        State := cxbsDisabled;

    sspcbNext, sspcbLast:
      if PageControl.ViewInfo.LastVisiblePageIndex >= PageControl.VisiblePageCount - 1 then
        State := cxbsDisabled;
  end;
end;

{ TdxSpreadSheetPageControlTabDragAndDropObject }

constructor TdxSpreadSheetPageControlTabDragAndDropObject.Create(AControl: TcxControl);
begin
  inherited Create(AControl);
  Arrows := TcxPlaceArrows.CreateArrows(PageControl.DropArrowColor, clBtnText);
  ScrollTimer := TcxTimer.Create(AControl);
  ScrollTimer.Interval := cxScrollWidthDragInterval;
  ScrollTimer.Enabled := False;
  ScrollTimer.OnTimer := ScrollTimerHandler;
  View := HitTestView;
end;

destructor TdxSpreadSheetPageControlTabDragAndDropObject.Destroy;
begin
  FreeAndNil(Arrows);
  FreeAndNil(ScrollTimer);
  inherited Destroy;
end;

procedure TdxSpreadSheetPageControlTabDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
const
  BordersMap: array[Boolean] of TcxBorder = (bLeft, bRight);
var
  ASide: TcxBorder;
  R: TRect;
begin
  DropIndex := -1;
  ASide := bLeft;
  ChangeMousePos(P);

  if PtInRect(LeftScrollArea, P) then
    InitScrollTimer(-1)
  else
    if PtInRect(RightScrollArea, P) then
      InitScrollTimer(1)
    else
      ScrollTimer.Enabled := False;

  R := cxInvalidRect;
  if HitTest.HitAtPageTab then
    R := TdxSpreadSheetCellViewInfo(HitTest.HitObject).ClipRect;

  if PtInRect(R, P) then
  begin
    ASide := BordersMap[cxRectCenter(TdxSpreadSheetCellViewInfo(HitTest.HitObject).ClipRect).X < P.X];
    R := cxRectOffset(R, PageControl.SpreadSheet.ClientToScreen(cxNullPoint));
    R.Right := R.Right - Byte(ASide = bRight);
    DropIndex := HitTestView.Index + Byte(ASide = bRight);
    if DropIndex > View.Index then
      Dec(DropIndex);
  end;

  Accepted := (DropIndex >= 0) and (DropIndex <> View.Index);
  if Accepted then
    Arrows.MoveTo(R, ASide)
  else
    Arrows.Hide;

  inherited DragAndDrop(P, Accepted);
end;

procedure TdxSpreadSheetPageControlTabDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  inherited EndDragAndDrop(Accepted);
  if Accepted and (DropIndex >= 0) then
    View.Index := DropIndex;
end;

function TdxSpreadSheetPageControlTabDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
const
  Cursors: array[Boolean] of TCursor = (crNoDrop, crDrag);
begin
  Result := Cursors[Accepted];
end;

procedure TdxSpreadSheetPageControlTabDragAndDropObject.InitScrollTimer(ADelta: Integer);
begin
  ScrollTimer.Enabled := True;
  ScrollDelta := ADelta;
end;

procedure TdxSpreadSheetPageControlTabDragAndDropObject.ScrollTimerHandler(ASender: TObject);
var
  AAccepted: Boolean;
begin
  PageControl.FirstVisiblePageIndex := PageControl.FirstVisiblePageIndex  + ScrollDelta;
  DragAndDrop(CurMousePos, AAccepted);
end;

function TdxSpreadSheetPageControlTabDragAndDropObject.GetLeftScrollArea: TRect;
begin
  Result := PageControl.ViewInfo.LeftScrollArea;
end;

function TdxSpreadSheetPageControlTabDragAndDropObject.GetHitTest: TdxSpreadSheetPageControlHitTest;
begin
  Result := PageControl.HitTest;
end;

function TdxSpreadSheetPageControlTabDragAndDropObject.GetHitTestView: TdxSpreadSheetCustomView;
begin
   if HitTest.HitAtPageTab then
     Result := TdxSpreadSheetPageControlTabCellViewInfo(HitTest.HitObject).View
   else
     Result := nil;
end;

function TdxSpreadSheetPageControlTabDragAndDropObject.GetPageControl: TdxSpreadSheetPageControl;
begin
  Result := TdxCustomSpreadSheet(Control).PageControl;
end;

function TdxSpreadSheetPageControlTabDragAndDropObject.GetRightScrollArea: TRect;
begin
  Result := PageControl.ViewInfo.RightScrollArea;
end;

{ TdxSpreadSheetPageControlTabCellViewInfo }

constructor TdxSpreadSheetPageControlTabCellViewInfo.Create(AOwner: TObject);
begin
  inherited;
  FSupportedDrawingStages := [dsFirst, dsSecond];
end;

procedure TdxSpreadSheetPageControlTabCellViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  ATextRect: TRect;
begin
  if (DrawingStage = dsSecond) = (State = cxbsPressed) then
  begin
    LookAndFeelPainter.DrawNavigatorScaledButton(ACanvas, Bounds, State, ViewParams.Color, ScaleFactor);
    LookAndFeelPainter.DrawNavigatorBorder(ACanvas, Bounds, False);

    ATextRect := cxRectContent(Bounds, GetTextMargins);
    cxTextOut(ACanvas.Handle, View.Caption, ATextRect,
      cxMakeFormat(taLeft, taBottom), nil, 0, 0, 0, ColorToRGB(ViewParams.TextColor));
  end;
end;

function TdxSpreadSheetPageControlTabCellViewInfo.GetDragAndDropObjectClass(
  AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass;
begin
  if SpreadSheet.OptionsProtection.ActualAllowChangeStructure then
    Result := TdxSpreadSheetPageControlTabDragAndDropObject
  else
    Result := nil;
end;

function TdxSpreadSheetPageControlTabCellViewInfo.GetHitCode: Integer;
begin
  Result := hcPageTab;
end;

function TdxSpreadSheetPageControlTabCellViewInfo.GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass;
begin
  Result := TdxSpreadSheetBuiltInPageControlTabPopupMenu;
end;

function TdxSpreadSheetPageControlTabCellViewInfo.GetState: TcxButtonState;
begin
  Result := inherited GetState;
  if View = SpreadSheet.ActiveSheet then
    Result := cxbsPressed
  else
    if (Result = cxbsHot) and (SpreadSheet.DragAndDropState <> ddsNone) then
      Result := cxbsNormal;
end;

class function TdxSpreadSheetPageControlTabCellViewInfo.GetTextMargins: TRect;
begin
  Result := cxRectBounds(cxTextOffset * 5, cxTextOffset * 2, 0, 0);
end;

procedure TdxSpreadSheetPageControlTabCellViewInfo.Initialize(const ABounds, AClipRect: TRect; AView: TdxSpreadSheetCustomView);
begin
  FView := AView;
  cxScreenCanvas.Font := ViewParams.Font;
  if View.Active then
    cxScreenCanvas.Font.Style := cxScreenCanvas.Font.Style + [fsBold];
  SetBounds(cxRectSetWidth(ABounds, cxScreenCanvas.TextWidth(View.Caption) + cxMarginsWidth(GetTextMargins)), AClipRect);
  cxScreenCanvas.Dormant;
  UpdateState;
end;

{ TdxSpreadSheetPageControlSplitterCellViewInfo }

function TdxSpreadSheetPageControlSplitterCellViewInfo.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
begin
  if AHitTest.GetHitCode(hcSplitter) then
    Result := crcxHorzSize
  else
    Result := crDefault;
end;

procedure TdxSpreadSheetPageControlSplitterCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  LookAndFeelPainter.DrawScaledScrollBarSplitter(ACanvas, Bounds, State, ScaleFactor);
  ACanvas.ExcludeClipRect(Bounds);
end;

function TdxSpreadSheetPageControlSplitterCellViewInfo.GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass;
begin
  Result := TdxSpreadSheetPageControlSplitterDragAndDropObject;
end;

function TdxSpreadSheetPageControlSplitterCellViewInfo.GetHitCode: Integer;
begin
  Result := hcSplitter;
end;

function TdxSpreadSheetPageControlSplitterCellViewInfo.GetState: TcxButtonState;
begin
  Result := inherited GetState;
  if (SpreadSheet.DragAndDropState = ddsInProcess) and
    SpreadSheet.DragAndDropObject.InheritsFrom(TdxSpreadSheetPageControlSplitterDragAndDropObject)
  then
    Result := cxbsHot;
end;

function TdxSpreadSheetPageControlSplitterCellViewInfo.MeasureWidth: Integer;
begin
  Result := SpreadSheet.LookAndFeelPainter.GetScaledSplitterSize(True, ScaleFactor).cx;
  if Result = 0 then
    Result := 17;
  Result := MulDiv(Result, 2, 3);
end;

{ TdxSpreadSheetPageControlSplitterDragAndDropObject }

procedure TdxSpreadSheetPageControlSplitterDragAndDropObject.BeginDragAndDrop;
begin
  StartPos := GetMouseCursorPos;
  StartWidth := cxRectWidth(PageControl.ViewInfo.Bounds);
end;

procedure TdxSpreadSheetPageControlSplitterDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  PageControl.Width := StartWidth + (GetMouseCursorPos.X - StartPos.X);
end;

procedure TdxSpreadSheetPageControlSplitterDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  PageControl.Invalidate;
end;

function TdxSpreadSheetPageControlSplitterDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetPageControlSplitterDragAndDropObject.GetPageControl: TdxSpreadSheetPageControl;
begin
  Result := TdxCustomSpreadSheet(Control).PageControl;
end;

{ TdxSpreadSheetPageControlViewInfo }

constructor TdxSpreadSheetPageControlViewInfo.Create(AOwner: TdxSpreadSheetPageControl);
begin
  FPageControl := AOwner;
  FCells := TdxSpreadSheetCellViewInfoList.Create;
  Pages := TList<TdxSpreadSheetCustomView>.Create;
end;

destructor TdxSpreadSheetPageControlViewInfo.Destroy;
begin
  FreeAndNil(FCells);
  FreeAndNil(Pages);
  inherited Destroy;
end;

procedure TdxSpreadSheetPageControlViewInfo.Calculate;
var
  I: Integer;
begin
  Cells.Clear;
  Pages.Clear;
  PageControl.HitTest.Clear;
  InitializeViewParams;
  for I := 0 to SpreadSheet.VisibleSheetCount - 1 do
    Pages.Add(SpreadSheet.VisibleSheets[I]);
  Bounds := SpreadSheet.ClientBounds;
  Bounds := cxRectBounds(Bounds.Left, Bounds.Bottom, MeasureWidth, MeasureHeight);
  FLeftScrollArea := cxInvalidRect;
  FRightScrollArea := cxInvalidRect;
  AddCells;
  PageControl.HitTest.Recalculate;
end;

procedure TdxSpreadSheetPageControlViewInfo.Draw(ACanvas: TcxCanvas);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(Bounds);
    Cells.Draw(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
    ACanvas.ExcludeClipRect(Bounds);
  end;
end;

procedure TdxSpreadSheetPageControlViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest);
begin
  if PtInRect(Bounds, AHitTest.ActualHitPoint) then
    Cells.CalculateHitTest(AHitTest);
end;

function TdxSpreadSheetPageControlViewInfo.MeasureHeight: Integer;
begin
  Result := LookAndFeelPainter.ScaledSizeGripSize(ScaleFactor).cy;
  if PageControl.Visible then
  begin
    Result := Max(Result, LookAndFeelPainter.NavigatorScaledButtonMinSize(ScaleFactor).cy);

    InitializeViewParams;
    cxScreenCanvas.Font := ViewParams.Font;
    cxScreenCanvas.Font.Style := cxScreenCanvas.Font.Style + [fsBold];
    Result := Max(Result, cxScreenCanvas.TextHeight(dxMeasurePattern) +
      cxMarginsHeight(TdxSpreadSheetPageControlTabCellViewInfo.GetTextMargins));
    cxScreenCanvas.Dormant;
  end;
end;

function TdxSpreadSheetPageControlViewInfo.MeasureWidth: Integer;
begin
  if not PageControl.Visible then
    Exit(0);

  Result := cxRectWidth(SpreadSheet.ClientBounds);
  if SpreadSheet.HScrollBarVisible and not SpreadSheet.IsPopupScrollBars then
  begin
    if PageControl.Width = 0 then
      Result := MulDiv(Result, 2, 3)
    else
      Result := Min(PageControl.Width, Result - LookAndFeelPainter.ScaledSizeGripSize(ScaleFactor).cx * 4);
  end;
end;

procedure TdxSpreadSheetPageControlViewInfo.AddCells;
var
  R: TRect;
  I: Integer;
  AButton: TdxSpreadSheetPageControlButton;
  ABackground: TdxSpreadSheetPageControlBackgroundCellViewInfo;
  ASplitter: TdxSpreadSheetPageControlSplitterCellViewInfo;
begin
  ABackground := TdxSpreadSheetPageControlBackgroundCellViewInfo.Create(Self);
  ABackground.SetBounds(Bounds, Bounds);
  FClipRect := Bounds;
  Cells.Add(ABackground);

  if not SpreadSheet.IsPopupScrollBars and SpreadSheet.HScrollBarVisible then
  begin
    ASplitter := CreateSplitterCellViewInfo;
    ASplitter.SetBounds(cxRectSetRight(Bounds, Bounds.Right, ASplitter.MeasureWidth), Bounds);
    Cells.Add(ASplitter);
    FClipRect.Right := ASplitter.Bounds.Left;
  end;

  R := cxRectSetWidth(Bounds, 0);
  for AButton := sspcbFirst to sspcbLast do
    AddButton(R, AButton);

  if PageControl.FirstVisiblePageIndex > 0 then
    FLeftScrollArea := cxRectSetXPos(R, Bounds.Left, R.Right + dxSpreadSheetResizeDelta * 3);

  FLastVisiblePageIndex := PageControl.FirstVisiblePageIndex;
  for I := PageControl.FirstVisiblePageIndex to Pages.Count - 1 do
  begin
    AddPageTab(R, Pages[I]);
    if R.Right <= FClipRect.Right then
      FLastVisiblePageIndex := I;
  end;

  if FLastVisiblePageIndex < Pages.Count - 1 then
    FRightScrollArea := cxRectSetXPos(R, FClipRect.Right - dxSpreadSheetResizeDelta * 3, SpreadSheet.ClientBounds.Right)
  else
    FRightScrollArea.Left := R.Right;

  AddButton(R, sspcbNew);
  PageControl.Controller.UpdateStates;
end;

procedure TdxSpreadSheetPageControlViewInfo.InitializeViewParams;
begin
  FViewParams := SpreadSheet.Styles.GetPageControlStyle;
end;

procedure TdxSpreadSheetPageControlViewInfo.AddButton(var R: TRect; AType: TdxSpreadSheetPageControlButton);
var
  ACell: TdxSpreadSheetPageControlButtonCellViewInfo;
begin
  if AType in PageControl.Buttons then
  begin
    R := cxRectSetLeft(R, R.Right, LookAndFeelPainter.NavigatorScaledButtonMinSize(ScaleFactor).cy);
    if LookAndFeelPainter.NavigatorBorderOverlap and (R.Left > Bounds.Left) then
      OffsetRect(R, -LookAndFeelPainter.NavigatorBorderSize, 0);
    ACell := TdxSpreadSheetPageControlButtonCellViewInfo.Create(Self);
    ACell.Initialize(R, FClipRect, AType);
    Cells.Add(ACell);
  end;
end;

procedure TdxSpreadSheetPageControlViewInfo.AddPageTab(var R: TRect; APage: TdxSpreadSheetCustomView);
var
  ACell: TdxSpreadSheetPageControlTabCellViewInfo;
begin
  ACell := CreateTabCellViewInfo;
  R.Left := R.Right;
  if LookAndFeelPainter.NavigatorBorderOverlap and (R.Left > Bounds.Left) then
    OffsetRect(R, -LookAndFeelPainter.NavigatorBorderSize, 0);
  ACell.Initialize(R, FClipRect, APage);
  R.Right := ACell.Bounds.Right;
  Cells.Add(ACell);
end;

function TdxSpreadSheetPageControlViewInfo.CreateSplitterCellViewInfo: TdxSpreadSheetPageControlSplitterCellViewInfo;
begin
  Result := TdxSpreadSheetPageControlSplitterCellViewInfo.Create(Self);
end;

function TdxSpreadSheetPageControlViewInfo.CreateTabCellViewInfo: TdxSpreadSheetPageControlTabCellViewInfo;
begin
  Result := TdxSpreadSheetPageControlTabCellViewInfo.Create(Self);
end;

function TdxSpreadSheetPageControlViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := SpreadSheet.LookAndFeelPainter;
end;

function TdxSpreadSheetPageControlViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := SpreadSheet.ScaleFactor;
end;

function TdxSpreadSheetPageControlViewInfo.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := PageControl.SpreadSheet;
end;

{ TdxSpreadSheetPageControl }

constructor TdxSpreadSheetPageControl.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(ASpreadSheet);
  CreateSubClasses;
  FButtons := [sspcbFirst..sspcbNew];
  FVisible := True;
  FDropArrowColor := clGreen;
end;

destructor TdxSpreadSheetPageControl.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxSpreadSheetPageControl.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetPageControl then
  begin
    FVisible := False;
    Buttons := TdxSpreadSheetPageControl(Source).Buttons;
    DropArrowColor := TdxSpreadSheetPageControl(Source).DropArrowColor;
    FirstVisiblePageIndex := TdxSpreadSheetPageControl(Source).FirstVisiblePageIndex;
    Width := TdxSpreadSheetPageControl(Source).Width;
    Visible := TdxSpreadSheetPageControl(Source).Visible;
  end;
end;

procedure TdxSpreadSheetPageControl.MakeVisible(APage: TdxSpreadSheetCustomView);
var
  AIndex: Integer;
begin
  if APage <> nil then
  begin
    AIndex := ViewInfo.Pages.IndexOf(APage);
    if (AIndex >= 0) and (AIndex < FirstVisiblePageIndex) then
      FirstVisiblePageIndex := AIndex
    else
      while AIndex > ViewInfo.LastVisiblePageIndex do
        FirstVisiblePageIndex := FirstVisiblePageIndex + 1;
  end;
end;

procedure TdxSpreadSheetPageControl.Changed;
begin
  ViewInfo.Calculate;
  if Visible then
    Invalidate;
end;

function TdxSpreadSheetPageControl.CreateController: TdxSpreadSheetPageControlController;
begin
  Result := TdxSpreadSheetPageControlController.Create(Self);
end;

function TdxSpreadSheetPageControl.CreateHitTest: TdxSpreadSheetPageControlHitTest;
begin
  Result := TdxSpreadSheetPageControlHitTest.Create(Self);
end;

function TdxSpreadSheetPageControl.CreateViewInfo: TdxSpreadSheetPageControlViewInfo;
begin
  Result := TdxSpreadSheetPageControlViewInfo.Create(Self);
end;

procedure TdxSpreadSheetPageControl.Invalidate;
begin
  if Visible then
    SpreadSheet.InvalidateRect(cxRectSetWidth(ViewInfo.Bounds, SpreadSheet.Width), False);
end;

procedure TdxSpreadSheetPageControl.Recalculate;
begin
  ViewInfo.Calculate;
  Invalidate;
end;

procedure TdxSpreadSheetPageControl.CreateSubClasses;
begin
  FHitTest := CreateHitTest;
  FViewInfo := CreateViewInfo;
  FController := CreateController;
end;

procedure TdxSpreadSheetPageControl.DestroySubClasses;
begin
  FreeAndNil(FViewInfo);
  FreeAndNil(FHitTest);
  FreeAndNil(FController);
end;

function TdxSpreadSheetPageControl.GetBounds: TRect;
begin
  if Visible then
    Result := ViewInfo.Bounds
  else
    Result := cxInvalidRect;
end;

function TdxSpreadSheetPageControl.GetFirstVisiblePageIndex: Integer;
begin
  Result := FFirstVisiblePageIndex;
end;

function TdxSpreadSheetPageControl.GetVisiblePage(AIndex: Integer): TdxSpreadSheetCustomView;
begin
  Result := ViewInfo.Pages[AIndex];
end;

function TdxSpreadSheetPageControl.GetVisiblePageCount: Integer;
begin
  Result := ViewInfo.Pages.Count;
end;

function TdxSpreadSheetPageControl.IsButtonsStored: Boolean;
begin
  Result := Buttons <> [sspcbFirst..sspcbNew];
end;

procedure TdxSpreadSheetPageControl.SetButtons(AValue: TdxSpreadSheetPageControlButtons);
begin
  if AValue <> FButtons then
  begin
    FButtons := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetPageControl.SetFirstVisiblePageIndex(AValue: Integer);
begin
  AValue := Max(0, Min(AValue, VisiblePageCount));
  if AValue <> FFirstVisiblePageIndex then
  begin
    FFirstVisiblePageIndex := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetPageControl.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    SpreadSheet.SetInternalControlsBounds;
    Changed;
  end;
end;

procedure TdxSpreadSheetPageControl.SetVisible(AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    SpreadSheet.BoundsChanged;
  end;
end;

{ TdxSpreadSheetViewInfo }

function TdxSpreadSheetViewInfo.GetBackgroundParams: TcxViewParams;
begin
  Result := SpreadSheet.Styles.GetBackgroundStyle;
end;

function TdxSpreadSheetViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := SpreadSheet.LookAndFeelPainter;
end;

{ TdxSpreadSheetStyles }

procedure TdxSpreadSheetStyles.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetStyles then
  begin
    Background := TdxSpreadSheetStyles(Source).Background;
    Content := TdxSpreadSheetStyles(Source).Content;
    Header := TdxSpreadSheetStyles(Source).Header;
end;
end;

procedure TdxSpreadSheetStyles.Changed(AIndex: Integer);
begin
  SpreadSheet.AddChanges([sscLayout, sscModified]);
end;

procedure TdxSpreadSheetStyles.GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams);
begin
  inherited GetDefaultViewParams(Index, AData, AParams);
  AParams.Font := SpreadSheet.Font;
  AParams.TextColor := AParams.Font.Color;
  case Index of
    s_Selection:
      AParams.Color := SpreadSheet.LookAndFeelPainter.SpreadSheetSelectionColor;

    s_Background:
      begin
        AParams.Color := SpreadSheet.LookAndFeelPainter.DefaultContentColor;
        AParams.TextColor := SpreadSheet.LookAndFeelPainter.DefaultContentTextColor;
      end;

    s_Content:
      begin
        AParams.Color := SpreadSheet.LookAndFeelPainter.SpreadSheetContentColor;
        AParams.TextColor := SpreadSheet.LookAndFeelPainter.SpreadSheetContentTextColor;
      end;

    s_Header:
      begin
        AParams.Color := SpreadSheet.LookAndFeelPainter.DefaultHeaderColor;
        AParams.TextColor := SpreadSheet.LookAndFeelPainter.DefaultHeaderTextColor;
      end;

    s_PageControl:
      begin
        AParams.Color := SpreadSheet.LookAndFeelPainter.DefaultTabColor;
        AParams.TextColor := SpreadSheet.LookAndFeelPainter.NavigatorButtonTextColor(cxbsNormal);
      end;
   end;
end;

function TdxSpreadSheetStyles.GetBackgroundStyle: TcxViewParams;
begin
  GetViewParams(s_Background, nil, nil, Result);
end;

function TdxSpreadSheetStyles.GetContentStyle(AView: TdxSpreadSheetCustomView): TcxViewParams;
begin
  GetViewParams(s_Content, AView, nil, Result);
end;

function TdxSpreadSheetStyles.GetHeaderStyle(AView: TdxSpreadSheetCustomView): TcxViewParams;
begin
  GetViewParams(s_Header, AView, nil, Result);
end;

function TdxSpreadSheetStyles.GetPageControlStyle: TcxViewParams;
begin
  GetViewParams(s_PageControl, nil, nil, Result);
end;

function TdxSpreadSheetStyles.GetSelectionStyle: TcxViewParams;
begin
  GetViewParams(s_Selection, nil, nil, Result);
end;

function TdxSpreadSheetStyles.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := Owner as TdxCustomSpreadSheet;
end;

{ TdxSpreadSheetOptionsBehavior }

constructor TdxSpreadSheetOptionsBehavior.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(ASpreadSheet);
  FFormulaAutoComplete := True;
  FFormulaAutoCompleteShowHint := True;
  FAutomaticCalculation := True;
  FDeleting := True;
  FEnterKeyNavigation := eknDefault;
  FEditing := True;
  FFormatting := True;
  FInserting := True;
  FDragMoving := True;
  FDragFilling := True;
end;

procedure TdxSpreadSheetOptionsBehavior.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetOptionsBehavior then
  begin
    AutomaticCalculation := TdxSpreadSheetOptionsBehavior(Source).AutomaticCalculation;
    Deleting := TdxSpreadSheetOptionsBehavior(Source).Deleting;
    DragFilling := TdxSpreadSheetOptionsBehavior(Source).DragFilling;
    DragMoving := TdxSpreadSheetOptionsBehavior(Source).DragMoving;
    Editing := TdxSpreadSheetOptionsBehavior(Source).Editing;
    FormulaAutoComplete := TdxSpreadSheetOptionsBehavior(Source).FormulaAutoComplete;
    EnterKeyNavigation := TdxSpreadSheetOptionsBehavior(Source).EnterKeyNavigation;
    Formatting := TdxSpreadSheetOptionsBehavior(Source).Formatting;
    FormulaAutoComplete := TdxSpreadSheetOptionsBehavior(Source).FormulaAutoComplete;
    FormulaAutoCompleteShowHint := TdxSpreadSheetOptionsBehavior(Source).FormulaAutoCompleteShowHint;
    History := TdxSpreadSheetOptionsBehavior(Source).History;
    Inserting := TdxSpreadSheetOptionsBehavior(Source).Inserting;
    IterativeCalculation := TdxSpreadSheetOptionsBehavior(Source).IterativeCalculation;
    IterativeCalculationMaxCount := TdxSpreadSheetOptionsBehavior(Source).IterativeCalculationMaxCount;
  end;
end;

function TdxSpreadSheetOptionsBehavior.GetProtected: Boolean;
begin
  Result := SpreadSheet.OptionsProtection.&Protected;
end;

procedure TdxSpreadSheetOptionsBehavior.SetHistory(AValue: Boolean);
begin
  if FHistory <> AValue then
  begin
    FHistory := AValue;
    SpreadSheet.History.Clear;
  end;
end;

procedure TdxSpreadSheetOptionsBehavior.SetIterativeCalculationMaxCount(AValue: Integer);
begin
  FIterativeCalculationMaxCount := Max(AValue, 0);
end;

procedure TdxSpreadSheetOptionsBehavior.SetProtected(AValue: Boolean);
begin
  SpreadSheet.OptionsProtection.AllowChangeStructure := not AValue;
  SpreadSheet.OptionsProtection.Protected := AValue;
end;

{ TdxSpreadSheetOptionsView }

constructor TdxSpreadSheetOptionsView.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(ASpreadSheet);
  FAntialiasing := True;
  FCellAutoHeight := True;
  FDateTimeSystem := dts1900;
  FFrozenPaneSeparatorWidth := 1;
  FFrozenPaneSeparatorColor := clDefault;
  FCommentMarkColor := clDefault;
  FGridLineColor := clDefault;
  FGridLines := True;
  FHeaders := True;
  FHorizontalScrollBar := True;
  FPrintAreaColor := clDefault;
  FR1C1Reference := False;
  FShowFormulas := False;
  FVerticalScrollBar := True;
  FZeroValues := True;
end;

procedure TdxSpreadSheetOptionsView.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetOptionsView then
  begin
    FAntialiasing := TdxSpreadSheetOptionsView(Source).Antialiasing;
    FCellAutoHeight := TdxSpreadSheetOptionsView(Source).CellAutoHeight;
    FDateTimeSystem := TdxSpreadSheetOptionsView(Source).DateTimeSystem;
    FCommentMarkColor := TdxSpreadSheetOptionsView(Source).CommentMarkColor;
    FFrozenPaneSeparatorColor := TdxSpreadSheetOptionsView(Source).FrozenPaneSeparatorColor;
    FFrozenPaneSeparatorWidth := TdxSpreadSheetOptionsView(Source).FrozenPaneSeparatorWidth;
    FGridLineColor := TdxSpreadSheetOptionsView(Source).FGridLineColor;
    FGridLines := TdxSpreadSheetOptionsView(Source).GridLines;
    FHeaders := TdxSpreadSheetOptionsView(Source).Headers;
    FHorizontalScrollBar := TdxSpreadSheetOptionsView(Source).FHorizontalScrollBar;
    FPrintAreas := TdxSpreadSheetOptionsView(Source).PrintAreas;
    FPrintAreaColor := TdxSpreadSheetOptionsView(Source).PrintAreaColor;
    FR1C1Reference := TdxSpreadSheetOptionsView(Source).R1C1Reference;
    FShowFormulas :=  TdxSpreadSheetOptionsView(Source).FShowFormulas;
    FVerticalScrollBar := TdxSpreadSheetOptionsView(Source).FVerticalScrollBar;
    FZeroValues := TdxSpreadSheetOptionsView(Source).ZeroValues;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.Changed;
begin
  SpreadSheet.FormatSettings.UpdateSettings;
  SpreadSheet.AddChanges([sscData, sscModified, sscLayout]);
end;

procedure TdxSpreadSheetOptionsView.ChangeScale(M, D: Integer);
begin
  FrozenPaneSeparatorWidth := MulDiv(FrozenPaneSeparatorWidth, M, D);
end;

function TdxSpreadSheetOptionsView.GetActualDateTimeSystem: TdxSpreadSheetDateTimeSystem;
begin
  Result := DateTimeSystem;
  if Result = dtsDefault then
    Result := dts1900;
end;

procedure TdxSpreadSheetOptionsView.SetAntialiasing(const AValue: Boolean);
begin
  if Antialiasing <> AValue then
  begin
    FAntialiasing := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetCellAutoHeight(const Value: Boolean);
begin
  if FCellAutoHeight <> Value then
  begin
    FCellAutoHeight := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetCommentMarkColor(AValue: TColor);
begin
  if CommentMarkColor <> AValue then
  begin
    FCommentMarkColor := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetDateTimeSystem(AValue: TdxSpreadSheetDateTimeSystem);
begin
  if AValue <> FDateTimeSystem then
  begin
    FDateTimeSystem := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetFrozenPaneSeparatorColor(AValue: TColor);
begin
  if AValue <> FFrozenPaneSeparatorColor then
  begin
    FFrozenPaneSeparatorColor := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetFrozenPaneSeparatorWidth(AValue: Integer);
begin
  dxSpreadSheetValidate(AValue, 0, dxSpreadSheetMaxSeparatorWidth);
  if AValue <> FFrozenPaneSeparatorWidth then
  begin
    FFrozenPaneSeparatorWidth := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetGridLineColor(AValue: TColor);
begin
  if AValue <> FGridLineColor then
  begin
    FGridLineColor := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetGridLines(AValue: Boolean);
begin
  if AValue <> FGridLines then
  begin
    FGridLines := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetHeaders(AValue: Boolean);
begin
  if AValue <> FHeaders then
  begin
    FHeaders := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetHorizontalScrollBar(AValue: Boolean);
begin
  if AValue <> FHorizontalScrollBar then
  begin
    FHorizontalScrollBar := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetPrintAreaColor(const Value: TColor);
begin
  if FPrintAreaColor <> Value then
  begin
    FPrintAreaColor := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetPrintAreas(AValue: Boolean);
begin
  if FPrintAreas <> AValue then
  begin
    FPrintAreas := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetR1C1Reference(AValue: Boolean);
begin
  if AValue <> FR1C1Reference then
  begin
    FR1C1Reference := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetShowFormulas(AValue: Boolean);
begin
  if AValue <> FShowFormulas then
  begin
    FShowFormulas := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetVerticalScrollBar(AValue: Boolean);
begin
  if AValue <> FVerticalScrollBar then
  begin
    FVerticalScrollBar := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetOptionsView.SetZeroValues(AValue: Boolean);
begin
  if FZeroValues <> AValue then
  begin
    FZeroValues := AValue;
    Changed;
  end;
end;

{ TdxSpreadSheetHistoryCustomCommand }

procedure TdxSpreadSheetHistoryCustomCommand.Initialize;
begin
end;

class function TdxSpreadSheetHistoryCustomCommand.ActionClass: TdxSpreadSheetHistoryActionClass;
begin
  Result := TdxSpreadSheetHistoryAction;
end;

function TdxSpreadSheetHistoryCustomCommand.CompatibleWith(ACommand: TdxSpreadSheetHistoryCustomCommand): Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetHistoryCustomCommand.Redo;
begin
end;

procedure TdxSpreadSheetHistoryCustomCommand.Undo;
begin
end;

function TdxSpreadSheetHistoryCustomCommand.GetView: TdxSpreadSheetCustomView;
begin
  Result := Owner.View;
end;

{ TdxSpreadSheetHistoryAction }

constructor TdxSpreadSheetHistoryAction.Create(AOwner: TdxSpreadSheetHistory);
begin
  FHistory := AOwner;
  FCommands := TcxObjectList.Create;
  FView := SpreadSheet.ActiveSheet;
  FModified := SpreadSheet.Modified;
  FData := StoreSelection;
end;

destructor TdxSpreadSheetHistoryAction.Destroy;
begin
  FreeAndNil(FData);
  if FHistory.CurrentAction = Self then
    FHistory.CurrentAction := nil;
  FreeAndNil(FCommands);
  inherited Destroy;
end;

procedure TdxSpreadSheetHistoryAction.Redo;
var
  AData: TStream;
begin
  History.CurrentAction := nil;
  if History.RedoActions[History.RedoActionCount - 1] = Self then
  begin
    SpreadSheet.BeginUpdate;
    History.InProcess := True;
    try
      AData := StoreSelection;
      try
        DoRedo;
        History.RedoActionList.Remove(Self);
        History.UndoActionList.Add(Self);
      finally
        RestoreSelection(AData);
      end;
    finally
      History.InProcess := False;
      SpreadSheet.EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetHistoryAction.Undo;
var
  AData: TStream;
begin
  History.CurrentAction := nil;
  if History.UndoActions[History.UndoActionCount - 1] = Self then
  begin
    SpreadSheet.BeginUpdate;
    History.InProcess := True;
    try
      AData := StoreSelection;
      try
        DoUndo;
        History.UndoActionList.Remove(Self);
        History.RedoActionList.Add(Self);
      finally
        RestoreSelection(AData);
      end;
    finally
      History.InProcess := False;
      SpreadSheet.Modified := FModified;
      SpreadSheet.EndUpdate;
    end;
  end;
end;

function TdxSpreadSheetHistoryAction.AcceptCommand(ACommand: TdxSpreadSheetHistoryCustomCommand): Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetHistoryAction.AddCommand(ACommand: TdxSpreadSheetHistoryCustomCommand);
begin
  FCommands.Add(ACommand);
  ACommand.FOwner := Self;
  ACommand.Initialize;
end;

procedure TdxSpreadSheetHistoryAction.DoRedo;
var
  I: Integer;
begin
  SpreadSheet.ActiveSheet := View;
  for I := 0 to Count - 1 do
    Commands[I].Redo;
end;

procedure TdxSpreadSheetHistoryAction.DoUndo;
var
  I: Integer;
begin
  SpreadSheet.ActiveSheet := View;
  for I := Count - 1 downto 0 do
    Commands[I].Undo;
end;

class function TdxSpreadSheetHistoryAction.GetDescription: string;
begin
  Result := '';
end;

function TdxSpreadSheetHistoryAction.StoreSelection: TStream;
var
  AWriter: TcxWriter;
begin
  Result := TMemoryStream.Create;
  AWriter := TcxWriter.Create(Result, dxSpreadSheetBinaryFormatVersion);
  try
    TdxSpreadSheetTableView(View).Selection.SaveToStream(AWriter);
  finally
    AWriter.Free;
  end;
end;

procedure TdxSpreadSheetHistoryAction.RestoreSelection(ANewData: TStream);
var
  AReader: TcxReader;
  AView: TdxSpreadSheetTableView;
begin
  FData.Position := 0;
  try
    AReader := TcxReader.Create(FData, dxSpreadSheetBinaryFormatVersion);
    try
      AView := TdxSpreadSheetTableView(View);
      AView.Selection.LoadFromStream(AReader);
      AView.MakeVisible(AView.Selection.FocusedRow, AView.Selection.FocusedColumn);
    finally
      AReader.Free;
    end;
  finally
    FData.Free;
    FData := ANewData;
  end;
end;

function TdxSpreadSheetHistoryAction.GetCount: Integer;
begin
  Result := FCommands.Count;
end;

function TdxSpreadSheetHistoryAction.GetCommand(AIndex: Integer): TdxSpreadSheetHistoryCustomCommand;
begin
  Result := FCommands[AIndex] as TdxSpreadSheetHistoryCustomCommand;
end;

function TdxSpreadSheetHistoryAction.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := History.SpreadSheet;
end;

{ TdxSpreadSheetHistoryEditCellAction }

class function TdxSpreadSheetHistoryEditCellAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionCellEditing);
end;

{ TdxSpreadSheetHistoryEditCommentAction }

class function TdxSpreadSheetHistoryEditCommentAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionEditComment);
end;

{ TdxSpreadSheetHistoryExpandGroupAction }

class function TdxSpreadSheetHistoryExpandGroupAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionExpandCollapseGroup);
end;

{ TdxSpreadSheetHistoryMergeCellsAction }

class function TdxSpreadSheetHistoryMergeCellsAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionCellsMerge);
end;

{ TdxSpreadSheetHistoryClearCellsAction }

class function TdxSpreadSheetHistoryClearCellsAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionClearCells);
end;

{ TdxSpreadSheetHistoryDeleteCellsAction }

class function TdxSpreadSheetHistoryDeleteCellsAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionDeleteCells);
end;

{ TdxSpreadSheetHistoryDeleteCommentsAction }

class function TdxSpreadSheetHistoryDeleteCommentsAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionDeleteComment);
end;

{ TdxSpreadSheetHistoryFillCellsAction }

class function TdxSpreadSheetHistoryFillCellsAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionFillCells);
end;

{ TdxSpreadSheetHistoryMoveCellsAction }

class function TdxSpreadSheetHistoryMoveCellsAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionMoveCells);
end;

{ TdxSpreadSheetHistoryInsertCellsAction }

function TdxSpreadSheetHistoryInsertCellsAction.AcceptCommand(ACommand: TdxSpreadSheetHistoryCustomCommand): Boolean;
begin
  Result := not (ACommand is TdxSpreadSheetHistoryChangeCellStyleCommand);
end;

class function TdxSpreadSheetHistoryInsertCellsAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionInsertCells);
end;

{ TdxSpreadSheetHistoryFormatCellAction }

class function TdxSpreadSheetHistoryFormatCellAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionFormatCells);
end;

{ TdxSpreadSheetHistoryReplaceAction }

class function TdxSpreadSheetHistoryReplaceAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionReplace);
end;

{ TdxSpreadSheetHistorySortingAction }

class function TdxSpreadSheetHistorySortingAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionSortCells);
end;

{ TdxSpreadSheetHistoryCutToClipboardAction }

class function TdxSpreadSheetHistoryCutToClipboardAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionCutCells);
end;

{ TdxSpreadSheetHistoryPasteFromClipboardAction }

class function TdxSpreadSheetHistoryPasteFromClipboardAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionPasteCells);
end;

{ TdxSpreadSheetHistoryAddGroupAction }

class function TdxSpreadSheetHistoryAddGroupAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionAddGroup);
end;

{ TdxSpreadSheetHistoryChangeGroupAction }

class function TdxSpreadSheetHistoryChangeGroupAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionChangeGroup);
end;

{ TdxSpreadSheetHistoryDeleteGroupAction }

class function TdxSpreadSheetHistoryDeleteGroupAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionDeleteGroup);
end;

{ TdxSpreadSheetHistoryDragAndDropAction }

class function TdxSpreadSheetHistoryDragAndDropAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionDragAndDrop);
end;

{ TdxSpreadSheetHistoryChangeRowColumnItemAction }

class function TdxSpreadSheetHistoryChangeRowColumnItemAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionChangeRowColumn);
end;

{ TdxSpreadSheetHistoryChangeContainerAction }

class function TdxSpreadSheetHistoryChangeContainerAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionChangeContainer);
end;

{ TdxSpreadSheetHistoryChangeConditionalFormattingAction }

class function TdxSpreadSheetHistoryChangeConditionalFormattingAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionChangeConditionalFormatting);
end;

{ TdxSpreadSheetHistoryChangePrintingOptionsAction }

class function TdxSpreadSheetHistoryChangePrintingOptionsAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionChangePrintingOptions);
end;

{ TdxSpreadSheetHistoryCreateDefinedNameAction }

class function TdxSpreadSheetHistoryCreateDefinedNameAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionCreateDefinedName);
end;

{ TdxSpreadSheetHistory }

constructor TdxSpreadSheetHistory.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(ASpreadSheet);
  FRedoActions := TcxObjectList.Create;
  FUndoActions  := TcxObjectList.Create;
end;

destructor TdxSpreadSheetHistory.Destroy;
begin
  Clear;
  FreeAndNil(FRedoActions);
  FreeAndNil(FUndoActions);
  inherited Destroy;
end;

function TdxSpreadSheetHistory.CanAddCommand: Boolean;
begin
  Result := not (IsLocked or InProcess);
end;

procedure TdxSpreadSheetHistory.AddCommand(ACommand: TdxSpreadSheetHistoryCustomCommand);
begin
  if not CanAddCommand or ((FCurrentAction <> nil) and not FCurrentAction.AcceptCommand(ACommand)) then
    ACommand.Free
  else
  begin
    if (FCurrentAction = nil) or (FActionLockCount = 0) then
    begin
      if FUndoActions.Count > 0 then
      begin
        FCurrentAction := UndoActions[FUndoActions.Count - 1];
        if (FCurrentAction.Count = 0) or not FCurrentAction.Commands[0].CompatibleWith(ACommand) then
          FCurrentAction := nil;
      end;
      if FCurrentAction = nil then
      begin
        FCurrentAction := ACommand.ActionClass.Create(Self);
        FUndoActions.Add(FCurrentAction);
        SpreadSheet.DoHistoryChanged;
      end;
      FRedoActions.Clear;
    end;
    FCurrentAction.AddCommand(ACommand);
  end;
end;

procedure TdxSpreadSheetHistory.BeginAction(AActionClass: TdxSpreadSheetHistoryActionClass);
begin
  Inc(FActionLockCount);
  if InProcess then
    Exit;

  if not IsLocked then
  begin
    if FActionLockCount = 1 then
      FCurrentAction := AActionClass.Create(Self);
  end
  else
    FCurrentAction := nil;

  if not IsLocked then
    FRedoActions.Clear;
end;

procedure TdxSpreadSheetHistory.EndAction(ACanceled: Boolean = False);
begin
  Dec(FActionLockCount);
  if (FActionLockCount = 0) and (FCurrentAction <> nil) then
  begin
    if (FCurrentAction.Count > 0) and not ACanceled then
    begin
      FUndoActions.Add(FCurrentAction);
      FCurrentAction := nil;
      SpreadSheet.DoHistoryChanged;
    end
    else
      FreeAndNil(FCurrentAction);
  end;
end;

procedure TdxSpreadSheetHistory.Clear;
var
  ACount: Integer;
begin
  FCurrentAction := nil;
  ACount := FUndoActions.Count + RedoActionCount;
  FUndoActions.Clear;
  FRedoActions.Clear;
  if ACount > 0 then
    SpreadSheet.DoHistoryChanged;
end;

procedure TdxSpreadSheetHistory.Lock;
begin
  Inc(FLockCount);
end;

procedure TdxSpreadSheetHistory.Unlock;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Clear;
end;

procedure TdxSpreadSheetHistory.Redo(const ARedoCount: Integer = 1);
var
  I: Integer;
begin
  if (ARedoCount <= 0) or (RedoActionCount = 0) then
    Exit;
  if (SpreadSheet.ActiveSheet <> nil) and SpreadSheet.ActiveSheet.Controller.Redo(ARedoCount) then
    Exit;
  FCurrentAction := nil;
  Inc(FLockCount);
  InProcess := True;
  SpreadSheet.BeginUpdate;
  try
    if RedoActionCount > 0 then
      SpreadSheet.AddChanges([sscData, sscLayout]);
    for I := RedoActionCount - 1 downto Max(0, RedoActionCount - ARedoCount) do
      RedoActions[I].Redo;
  finally
    Dec(FLockCount);
    try
      SpreadSheet.EndUpdate;
    finally
      FInProcessRefCount := 0;
    end;
  end;
  SpreadSheet.DoHistoryChanged;
end;

procedure TdxSpreadSheetHistory.Undo(const AUndoCount: Integer = 1);
var
  AModified: Boolean;
  I: Integer;
begin
  if (AUndoCount <= 0) or (UndoActionCount = 0) then
    Exit;
  if (SpreadSheet.ActiveSheet <> nil) and SpreadSheet.ActiveSheet.Controller.Undo(AUndoCount) then
    Exit;

  Inc(FLockCount);
  FCurrentAction := nil;
  AModified := SpreadSheet.Modified;
  InProcess := True;
  SpreadSheet.BeginUpdate;
  try
    if UndoActionCount > 0 then
      SpreadSheet.AddChanges([sscData, sscLayout]);
    for I := UndoActionCount - 1 downto Max(0, UndoActionCount - AUndoCount) do
      UndoActions[I].Undo;
    AModified := SpreadSheet.Modified;
  finally
    Dec(FLockCount);
    SpreadSheet.EndUpdate;
    SpreadSheet.Modified := AModified;
    FInProcessRefCount := 0;
  end;
  SpreadSheet.DoHistoryChanged;
end;

function TdxSpreadSheetHistory.GetInProcess: Boolean;
begin
  Result := FInProcessRefCount > 0;
end;

function TdxSpreadSheetHistory.GetIsLocked: Boolean;
begin
  Result := (FLockCount > 0) or SpreadSheet.IsDestroying or SpreadSheet.IsLoading or not SpreadSheet.OptionsBehavior.History;
end;

function TdxSpreadSheetHistory.GetRedoAction(AIndex: Integer): TdxSpreadSheetHistoryAction;
begin
  if AIndex >= FRedoActions.Count then
    Result := nil
  else
    Result := TdxSpreadSheetHistoryAction(FRedoActions[AIndex]);
end;

function TdxSpreadSheetHistory.GetRedoActionCount: Integer;
begin
  Result := FRedoActions.Count;
  if not SpreadSheet.IsDestroying and (SpreadSheet.ActiveSheet <> nil) then
    SpreadSheet.ActiveSheet.Controller.GetRedoActionCount(Result);
end;

function TdxSpreadSheetHistory.GetUndoAction(AIndex: Integer): TdxSpreadSheetHistoryAction;
begin
  if AIndex < FUndoActions.Count then
    Result := TdxSpreadSheetHistoryAction(FUndoActions[AIndex])
  else
    Result := nil;
end;

function TdxSpreadSheetHistory.GetUndoActionCount: Integer;
begin
  Result := FUndoActions.Count;
  if not SpreadSheet.IsDestroying and (SpreadSheet.ActiveSheet <> nil) then
    SpreadSheet.ActiveSheet.Controller.GetUndoActionCount(Result);
end;

procedure TdxSpreadSheetHistory.SetInProcess(AValue: Boolean);
begin
  if AValue then
    Inc(FInProcessRefCount)
  else
    Dec(FInProcessRefCount);
end;

{ TdxSpreadSheetLockedStatePaintHelper }

function TdxSpreadSheetLockedStatePaintHelper.CanCreateLockedImage: Boolean;
begin
  Result := inherited CanCreateLockedImage and not SpreadSheet.IsLocked;
end;

function TdxSpreadSheetLockedStatePaintHelper.DoPrepareImage: Boolean;
begin
  Result := SpreadSheet.DoPrepareLockedStateImage;
end;

function TdxSpreadSheetLockedStatePaintHelper.GetControl: TcxControl;
begin
  Result := SpreadSheet;
end;

function TdxSpreadSheetLockedStatePaintHelper.GetOptions: TcxLockedStateImageOptions;
begin
  Result := SpreadSheet.OptionsLockedStateImage;
end;

function TdxSpreadSheetLockedStatePaintHelper.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := TdxCustomSpreadSheet(Owner);
end;

{ TdxSpreadSheetLockedStateImageOptions }

constructor TdxSpreadSheetLockedStateImageOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner) ;
  FSpreadSheet := AOwner as TdxCustomSpreadSheet;
  Enabled := True;
end;

function TdxSpreadSheetLockedStateImageOptions.GetFont: TFont;
begin
  Result := SpreadSheet.Font;
end;

{ TdxCustomSpreadSheet }

constructor TdxCustomSpreadSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateSubClasses;
  Keys := [kAll..kTab];
  Width := 460;
  Height := 240;
  AddSheet;
  FClipboardArea := TdxSpreadSheetClipboardArea.Create;
end;

destructor TdxCustomSpreadSheet.Destroy;
begin
  StopEditing;
  DestroySubClasses;
  FreeAndNil(FClipboardArea);
  inherited Destroy;
end;

procedure TdxCustomSpreadSheet.BeforeDestruction;
begin
  inherited;
  cxBroadcastRemoveNotifications(Self);
end;

function TdxCustomSpreadSheet.AddSheet(const ACaption: string = '';
  AViewClass: TdxSpreadSheetCustomViewClass = nil): TdxSpreadSheetCustomView;
begin
  if DoCreateSheet(Result, ACaption, AViewClass) then
  begin
    if ActiveSheetIndex = -1 then
      ActiveSheetIndex := 0;
    AddChanges([sscModified, sscLayout]);
  end;
end;

procedure TdxCustomSpreadSheet.BeginDragAndDrop;
begin
  DragAndDropState := ddsInProcess;
  inherited;
end;

procedure TdxCustomSpreadSheet.ClearAll;
begin
  History.Lock;
  try
    StopEditing;
    FormattedSharedStringCache.Clear;
    while SheetCount > 0 do
      Sheets[SheetCount - 1].Free;
    DefinedNames.Clear;
    DefaultCellStyle.Reset;
    ExternalLinks.Clear;
    FActiveSheetIndex := -1;
  finally
    History.Unlock;
    History.Clear;
  end;
end;

function TdxCustomSpreadSheet.DoCreateSheet(var ASheet: TdxSpreadSheetCustomView;
  const ACaption: string = ''; AViewClass: TdxSpreadSheetCustomViewClass = nil): Boolean;
begin
  ASheet := GetSheetByName(ACaption);
  Result := ASheet = nil;
  if Result then
  begin
    if AViewClass = nil then
      AViewClass := TdxSpreadSheetTableView;
    ASheet := AViewClass.Create(Self);
    ASheet.Caption := ValidateSheetCaption(ACaption);
  end;
end;

function TdxCustomSpreadSheet.InsertSheet(AIndex: Integer = -1; const ACaption: string = ''; AViewClass: TdxSpreadSheetCustomViewClass = nil): TdxSpreadSheetCustomView;
begin
  if OptionsBehavior.Protected then
    Exit(nil);
  if AIndex = -1 then
    AIndex := ActiveSheetIndex;
  if not DoCreateSheet(Result, ACaption, AViewClass) then
    Exit;
  Result.Index := Min(Max(0, AIndex), SheetCount - 1);
  ActiveSheetIndex := Result.Index;
  LayoutChanged;
end;

function TdxCustomSpreadSheet.EvaluateExpression(const AExpression: string; AView: TdxSpreadSheetTableView = nil): Variant;
var
  AExpr: TdxSpreadSheetExpression;
begin
  if AView = nil then
    AView := ActiveSheetAsTable;
  if AView <> nil then
  begin
    AExpr := TdxSpreadSheetExpression.Create(AView);
    try
      AExpr.Calculate(AExpression);
      if AExpr.ErrorCode = ecNone then
        Result := AExpr.Value
      else
        Result := Null;
    finally
      AExpr.Free;
    end;
  end
  else
    Result := Null;
end;

function TdxCustomSpreadSheet.GetSheetByName(const ACaption: string): TdxSpreadSheetCustomView;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to SheetCount - 1 do
  begin
    if (Result = nil) and (dxSpreadSheetCompareText(ACaption, Sheets[I].Caption) = 0) then
      Result := Sheets[I];
  end;
end;

procedure TdxCustomSpreadSheet.LayoutChanged;
begin
  AddChanges([sscLayout])
end;

procedure TdxCustomSpreadSheet.BeginUpdate(AMode: TcxLockedStateImageShowingMode = lsimNever);
begin
  LockedStatePaintHelper.BeginLockedPaint(AMode);
  Inc(FLockCount);
end;

procedure TdxCustomSpreadSheet.EndUpdate;
begin
  Dec(FLockCount);
  try
    if FLockCount = 0 then
      CheckChanges;
  finally
    LockedStatePaintHelper.EndLockedPaint;
  end;
end;

procedure TdxCustomSpreadSheet.Protect;
begin
  if not OptionsProtection.Protected then
    ShowProtectWorkbookDialog(Self);
end;

procedure TdxCustomSpreadSheet.Unprotect;
begin
  UnprotectCore(OptionsProtection);
end;

procedure TdxCustomSpreadSheet.LoadFromFile(const AFileName: string);
var
  AFileStream: TFileStream;
  AFormat: TdxSpreadSheetCustomFormatClass;
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      AStream.LoadFromStream(AFileStream);
    finally
      AFileStream.Free;
    end;

    if dxSpreadSheetFormatsRepository.Find(AFileName, AFormat) and
      (not AFormat.CanCheckByContent or AFormat.CanReadFromStream(AStream))
    then
      LoadFromStream(AStream, AFormat)
    else
      LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxCustomSpreadSheet.LoadFromStream(AStream: TStream);
var
  AFormat: TdxSpreadSheetCustomFormatClass;
  I: Integer;
begin
  for I := 0 to dxSpreadSheetFormatsRepository.Count - 1 do
  begin
    AFormat := dxSpreadSheetFormatsRepository[I];
    if AFormat.CanCheckByContent and AFormat.CanReadFromStream(AStream) then
    begin
      LoadFromStream(AStream, AFormat);
      Exit;
    end;
  end;
  raise EdxSpreadSheetFormatError.Create(cxGetResourceString(@sdxErrorUnsupportedDocumentFormat));
end;

procedure TdxCustomSpreadSheet.LoadFromStream(AStream: TStream; AFormat: TdxSpreadSheetCustomFormatClass);
begin
  InternalLoadFromStream(AStream, AFormat, OnProgress);
end;

procedure TdxCustomSpreadSheet.SaveToFile(const AFileName: string);
var
  AFormat: TdxSpreadSheetCustomFormatClass;
  AStream: TFileStream;
begin
  if not dxSpreadSheetFormatsRepository.Find(AFileName, AFormat) then
    raise EdxSpreadSheetFormatError.Create(cxGetResourceString(@sdxErrorUnsupportedDocumentFormat));

  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream, AFormat);
  finally
    AStream.Free;
  end;
end;

procedure TdxCustomSpreadSheet.SaveToStream(AStream: TStream; AFormat: TdxSpreadSheetCustomFormatClass = nil);
begin
  InternalSaveToStream(AStream, AFormat, OnProgress);
end;

procedure TdxCustomSpreadSheet.AddListener(AListener: IdxSpreadSheetListener);
begin
  if Listeners.IndexOf(AListener) < 0 then
    Listeners.Add(AListener);
end;

procedure TdxCustomSpreadSheet.RemoveListener(AListener: IdxSpreadSheetListener);
begin
  if Listeners <> nil then
    Listeners.Remove(AListener);
end;

procedure TdxCustomSpreadSheet.AddChanges(AChanges: TdxSpreadSheetChanges);
begin
  FChanges := FChanges + AChanges;
  CheckChanges;
end;

procedure TdxCustomSpreadSheet.AfterLoad;
var
  I: Integer;
begin
  for I := 0 to SheetCount - 1 do
    Sheets[I].RecalculateBestFit;
  DefinedNames.AfterLoad;
end;

procedure TdxCustomSpreadSheet.BoundsChanged;
begin
  inherited BoundsChanged;
  LayoutChanged;
  Invalidate;
end;

procedure TdxCustomSpreadSheet.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  OptionsView.ChangeScale(M, D);
  OptionsLockedStateImage.ChangeScale(M, D);
end;

procedure TdxCustomSpreadSheet.CheckChanges;
var
  AShouldNotifyListeners: Boolean;
begin
  if IsLocked or ProcessingChanges then
    Exit;

  AShouldNotifyListeners := False;
  ProcessingChanges := True;
  try
    FSheets.CheckChanges;
    while Changes <> [] do
    begin
      AShouldNotifyListeners := AShouldNotifyListeners or ([sscData, sscStyle] * Changes <> []);

      if sscData in Changes then
      begin
        FSheets.DataChanged;
        FormulaController.Recalculate;
        Changes := Changes + [sscLayout] - [sscData];
        DoDataChanged;
      end;

      if sscLayout in Changes then
      begin
        Changes := Changes - [sscLayout];
        FSheets.AddChanges([sscLayout]);
        if ActiveSheet = nil then
        begin
          UpdateScrollBars;
          Invalidate;
        end;
        PageControl.Recalculate;
        DoLayoutChanged;
      end;

      if sscModified in Changes then
      begin
        Changes := Changes - [sscModified];
        if not IsLoading then
          Modified := True;
      end;

      Changes := Changes - [sscStyle];
      FSheets.CheckChanges;
    end;
  finally
    ProcessingChanges := False;
    FSheets.CheckChanges;
  end;

  FormulaController.CheckCircularReferences;
  if AShouldNotifyListeners then
    Listeners.NotifyDataChanged(Self);
end;

procedure TdxCustomSpreadSheet.CheckDestroyedView(AView: TdxSpreadSheetCustomView);
begin
  if not IsDestroying and (ClipboardArea.View = AView) then
    Clipboard.Clear;
end;

procedure TdxCustomSpreadSheet.ClearClipboard;
begin
  if IsClipboardListener then
    Clipboard.Clear;
end;

function TdxCustomSpreadSheet.ControllerFromPoint(const P: TPoint; var AController: TdxSpreadSheetCustomController): Boolean;
begin
  AController := ActiveController;
  if AController = nil then
  begin
    if (ActiveSheet <> nil) and PtInRect(ActiveSheet.Bounds, P) then
      AController := ActiveSheet.Controller
    else
      if PtInRect(PageControl.Bounds, P) then
        AController := PageControl.Controller;
  end;
  Result := AController <> nil;
end;

function TdxCustomSpreadSheet.ControllerFromPoint(
  X, Y: Integer; var AController: TdxSpreadSheetCustomController): Boolean;
begin
  Result := ControllerFromPoint(Point(X, Y), AController);
end;

function TdxCustomSpreadSheet.CreateCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := TdxSpreadSheetCellStyles.Create;
  Result.OnChange := StyleChanged;
end;

function TdxCustomSpreadSheet.CreateDefaultCellStyle: TdxSpreadSheetDefaultCellStyle;
begin
  Result := TdxSpreadSheetDefaultCellStyle.Create(Self);
end;

function TdxCustomSpreadSheet.CreateDefinedNames: TdxSpreadSheetDefinedNames;
begin
  Result := TdxSpreadSheetDefinedNames.Create(Self);
end;

function TdxCustomSpreadSheet.CreateExternalLinks: TdxSpreadSheetExternalLinks;
begin
  Result := TdxSpreadSheetExternalLinks.Create(Self);
end;

function TdxCustomSpreadSheet.CreateFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := TdxSpreadSheetControlFormatSettings.Create(Self);
end;

function TdxCustomSpreadSheet.CreateFormulaController:  TdxSpreadSheetFormulaController;
begin
  Result := TdxSpreadSheetFormulaController.Create(Self);
end;

function TdxCustomSpreadSheet.CreateHistory: TdxSpreadSheetHistory;
begin
  Result := TdxSpreadSheetHistory.Create(Self);
end;

function TdxCustomSpreadSheet.CreateLockedStatePaintHelper: TdxSpreadSheetLockedStatePaintHelper;
begin
  Result := TdxSpreadSheetLockedStatePaintHelper.Create(Self);
end;

function TdxCustomSpreadSheet.CreateOptionsBehavior: TdxSpreadSheetOptionsBehavior;
begin
  Result := TdxSpreadSheetOptionsBehavior.Create(Self);
end;

function TdxCustomSpreadSheet.CreateOptionsLockedStateImage: TdxSpreadSheetLockedStateImageOptions;
begin
  Result := TdxSpreadSheetLockedStateImageOptions.Create(Self);
end;

function TdxCustomSpreadSheet.CreateOptionsProtection: TdxSpreadSheetWorkbookProtectionOptions;
begin
  Result := TdxSpreadSheetWorkbookProtectionOptions.Create(DoOptionsProtectionChanged);
end;

function TdxCustomSpreadSheet.CreateOptionsView: TdxSpreadSheetOptionsView;
begin
  Result := TdxSpreadSheetOptionsView.Create(Self);
end;

function TdxCustomSpreadSheet.CreatePageControl: TdxSpreadSheetPageControl;
begin
  Result := TdxSpreadSheetPageControl.Create(Self);
end;

function TdxCustomSpreadSheet.CreateSharedImages: TdxSpreadSheetSharedImages;
begin
  Result := TdxSpreadSheetSharedImages.Create;
end;

function TdxCustomSpreadSheet.CreateFormattedSharedStringCache: TdxSpreadSheetFormattedSharedStringCache;
begin
  Result := TdxSpreadSheetFormattedSharedStringCache.Create;
end;

function TdxCustomSpreadSheet.CreateSharedStringTable: TdxSpreadSheetSharedStringTable;
begin
  Result := TdxSpreadSheetSharedStringTable.Create(CellStyles.Fonts);
end;

function TdxCustomSpreadSheet.CreateStyles: TdxSpreadSheetStyles;
begin
  Result := TdxSpreadSheetStyles.Create(Self);
end;

function TdxCustomSpreadSheet.CreateViewInfo: TdxSpreadSheetViewInfo;
begin
  Result := TdxSpreadSheetViewInfo.Create(Self);
end;

procedure TdxCustomSpreadSheet.CreateSubClasses;
begin
  FListeners := TdxSpreadSheetListeners.Create;
  FStyles := CreateStyles;
  FFormulaController := CreateFormulaController;
  FLockedStatePaintHelper := CreateLockedStatePaintHelper;
  FOptionsLockedStateImage := CreateOptionsLockedStateImage;
  FOptionsBehavior := CreateOptionsBehavior;
  FOptionsProtection := CreateOptionsProtection;
  FOptionsView := CreateOptionsView;
  FCellStyles := CreateCellStyles;
  FDefaultCellStyle := CreateDefaultCellStyle;
  FStringTable := CreateSharedStringTable;
  FSharedImages := CreateSharedImages;
  FFormattedSharedStringCache:= CreateFormattedSharedStringCache;
  FSheets := TdxSpreadSheetViewList.Create;
  FVisibleSheets := TList<TdxSpreadSheetCustomView>.Create;
  FFormatSettings := CreateFormatSettings;
  FDefinedNames := CreateDefinedNames;
  FExternalLinks := CreateExternalLinks;
  FPageControl := CreatePageControl;
  FHistory := CreateHistory;
  FViewInfo := CreateViewInfo;
  FDialogsLookAndFeel := TcxLookAndFeel.Create(Self);
  FDialogsLookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

procedure TdxCustomSpreadSheet.DestroySubClasses;
begin
  History.Lock;
  History.Clear;
  FreeAndNil(FDefaultEdit);
  DefinedNames.Clear;
  FreeAndNil(FSheets);
  FreeAndNil(FVisibleSheets);
  FreeAndNil(FDefinedNames);
  FreeAndNil(FFormulaController);
  FreeAndNil(FStringTable);
  FreeAndNil(FSharedImages);
  FreeAndNil(FFormattedSharedStringCache);
  FreeAndNil(FDefaultCellStyle);
  FreeAndNil(FOptionsLockedStateImage);
  FreeAndNil(FCellStyles);
  FreeAndNil(FOptionsView);
  FreeAndNil(FOptionsBehavior);
  FreeAndNil(FFormatSettings);
  FreeAndNil(FDefinedNames);
  FreeAndNil(FExternalLinks);
  FreeAndNil(FViewInfo);
  FreeAndNil(FStyles);
  FreeAndNil(FPageControl);
  FreeAndNil(FHistory);
  FreeAndNil(FDialogsLookAndFeel);
  FreeAndNil(FOptionsProtection);
  FreeAndNil(FLockedStatePaintHelper);
  FreeAndNil(FListeners);
end;

procedure TdxCustomSpreadSheet.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadBinaryData, WriteBinaryData, True);
end;

procedure TdxCustomSpreadSheet.FontChanged;
begin
  inherited FontChanged;
  AddChanges([sscLayout, sscModified]);
end;

procedure TdxCustomSpreadSheet.DefinedNamesChanged;
begin
  Listeners.NotifyDefinedNamesChanged(Self);
  AddChanges([sscData, sscModified]);
end;

function TdxCustomSpreadSheet.DoActiveCellChanging(AView: TdxSpreadSheetTableView; const ANewActiveCell: TPoint): Boolean;
begin
  Result := True;
  if not (csReading in ComponentState) then
  begin
    if Assigned(OnActiveCellChanging) then
      OnActiveCellChanging(AView, ANewActiveCell, Result);
  end;
end;

procedure TdxCustomSpreadSheet.DoActiveSheetChanged;
begin
  Listeners.NotifyActiveSheetChanged(Self);
  LayoutChanged;
  BoundsChanged;
  dxCallNotify(OnActiveSheetChanged, Self);
end;

procedure TdxCustomSpreadSheet.DoCompare(AView: TdxSpreadSheetCustomView;
  const AData1, AData2: TdxSpreadSheetCellData; var Compare: Integer);
begin
  if Assigned(OnCompare) then
    OnCompare(AView, AData1, AData2, Compare);
end;

procedure TdxCustomSpreadSheet.DoCommentHide(AContainer: TdxSpreadSheetContainer);
begin
  if Assigned(OnCommentHide) then
    OnCommentHide(TdxSpreadSheetCommentContainer(AContainer).Cell, AContainer);
end;

function TdxCustomSpreadSheet.DoCommentShow(AContainer: TdxSpreadSheetContainer): Boolean;
begin
  Result := False;
  if Assigned(OnCommentShow) then
    OnCommentShow(TdxSpreadSheetCommentContainer(AContainer).Cell, AContainer, Result);
end;

procedure TdxCustomSpreadSheet.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  inherited DoContextPopup(MousePos, Handled);

  if not Handled then
  begin
    if cxPointIsEqual(MousePos, cxInvalidPoint) or (ActiveController = nil) then
      Handled := (ActiveSheet <> nil) and ActiveSheet.Controller.ContextPopup(GetMouseCursorClientPos)
    else
      Handled := ActiveController.ContextPopup(MousePos);
  end;
end;

procedure TdxCustomSpreadSheet.DoCustomDrawTableViewCell(Sender: TdxSpreadSheetTableView;
  ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewCellViewInfo; var AHandled: Boolean);
begin
  AHandled := False;
  if Assigned(FOnCustomDrawTableViewCell) then
    FOnCustomDrawTableViewCell(Sender, ACanvas, AViewInfo, AHandled);
end;

procedure TdxCustomSpreadSheet.DoCustomDrawTableViewCommonCell(Sender: TdxSpreadSheetTableView;
  ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewCustomCellViewInfo; var AHandled: Boolean);
begin
  AHandled := False;
  if Assigned(FOnCustomDrawTableViewCommonCell) then
    FOnCustomDrawTableViewCommonCell(Sender, ACanvas, AViewInfo, AHandled);
end;

procedure TdxCustomSpreadSheet.DoCustomDrawTableViewHeaderCell(Sender: TdxSpreadSheetTableView;
  ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewHeaderCellViewInfo; var AHandled: Boolean);
begin
  AHandled := False;
  if Assigned(FOnCustomDrawTableViewHeaderCell) then
    FOnCustomDrawTableViewHeaderCell(Sender, ACanvas, AViewInfo, AHandled);
end;

function TdxCustomSpreadSheet.IsTableViewCellCustomDrawn: Boolean;
begin
  Result := Assigned(OnCustomDrawTableViewCell);
end;

procedure TdxCustomSpreadSheet.DoDataChanged;
begin
  dxCallNotify(OnDataChanged, Self);
end;

procedure TdxCustomSpreadSheet.DoModifiedChanged;
begin
  if not IsLoading then
    dxCallNotify(OnModifiedChanged, Self);
end;

procedure TdxCustomSpreadSheet.DoEditChanged(AView: TdxSpreadSheetCustomView);
begin
  Listeners.NotifyEditingValueChanged(AView);
  if Assigned(OnEditChanged) then
    OnEditChanged(AView);
end;

procedure TdxCustomSpreadSheet.DoEdited(AView: TdxSpreadSheetCustomView);
begin
  Listeners.NotifyEdited(AView);
  if Assigned(OnEdited) then
    OnEdited(AView);
end;

procedure TdxCustomSpreadSheet.DoEditing(AView: TdxSpreadSheetCustomView;
  var AProperties: TcxCustomEditProperties; var ACanEdit: Boolean);
begin
  if Assigned(OnEditing) then
    OnEditing(AView, AProperties, ACanEdit);
end;

procedure TdxCustomSpreadSheet.DoEditValueChanged(AView: TdxSpreadSheetCustomView);
begin
  if Assigned(OnEditValueChanged) then
    OnEditValueChanged(AView);
end;

procedure TdxCustomSpreadSheet.DoHistoryChanged;
begin
  if Assigned(OnHistoryChanged) and not (IsLoading or IsDestroying) then
    OnHistoryChanged(Self);
end;

function TdxCustomSpreadSheet.DoHyperlinkExecute(ALink: TdxSpreadSheetHyperlink): Boolean;
begin
  Result := False;
  if Assigned(OnHyperlinkExecute) then
    OnHyperlinkExecute(Self, ALink, Result);
end;

procedure TdxCustomSpreadSheet.DoInitEdit(AView: TdxSpreadSheetCustomView; AEdit: TcxCustomEdit);
begin
  if Assigned(OnInitEdit) then
    OnInitEdit(AView, AEdit);
end;

procedure TdxCustomSpreadSheet.DoInitEditValue(AView: TdxSpreadSheetCustomView; AEdit: TcxCustomEdit; var AValue: Variant);
begin
  if Assigned(OnInitEditValue) then
    OnInitEditValue(AView, AEdit, AValue);
end;

function TdxCustomSpreadSheet.DoGetPassword(var APassword: string): Boolean;
begin
  if Assigned(OnGetPassword) then
    Result := OnGetPassword(Self, APassword)
  else
    Result := ShowPasswordDialog(Self, pdmQuery, APassword);
end;

procedure TdxCustomSpreadSheet.DoLayoutChanged;
begin
  if not IsLoading then
    CallNotify(OnLayoutChanged, Self);
end;

procedure TdxCustomSpreadSheet.DoOptionsProtectionChanged(Sender: TObject);
begin
  if OptionsProtection.&Protected then
    History.Clear;
  AddChanges([sscLayout, sscModified]);
end;

procedure TdxCustomSpreadSheet.DoScroll(AScrollBarKind: TScrollBarKind; AFirstScrollablePosition: Integer);
begin
  if Assigned(OnScroll) then
    OnScroll(Self, AScrollBarKind, AFirstScrollablePosition);
end;

procedure TdxCustomSpreadSheet.DoSelectionChanged(AView: TdxSpreadSheetCustomView);
begin
  Listeners.NotifySelectionChanged(AView);
  if not IsLoading then
    CallNotify(OnSelectionChanged, AView);
end;

procedure TdxCustomSpreadSheet.DoAddSheet(ASheet: TdxSpreadSheetCustomView);
begin
  StopEditing;
  History.Lock;
  try
    FSheets.Add(ASheet);
    if ASheet.Visible then
      FVisibleSheets.Add(ASheet);
  finally
    History.Unlock;
  end;
end;

procedure TdxCustomSpreadSheet.DoChangeSheetVisibility(ASheet: TdxSpreadSheetCustomView);
begin
  StopEditing;
  BeginUpdate;
  try
    UpdateVisibleSheetList;
    SetActiveSheetIndex(ActiveSheetIndex);
    AddChanges([sscModified, sscLayout]);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomSpreadSheet.DoRemoveSheet(ASheet: TdxSpreadSheetCustomView);
begin
  if not IsDestroying then
  begin
    History.Lock;
    try
      BeginUpdate;
      try
        FVisibleSheets.Remove(ASheet);
        FSheets.Remove(ASheet);
        FDefinedNames.OnRemoveSheet(ASheet);
        SetActiveSheetIndex(ActiveSheetIndex);
        AddChanges([sscData, sscModified, sscLayout]);
      finally
        EndUpdate;
      end;
    finally
      History.Unlock;
    end;
  end;
end;

function TdxCustomSpreadSheet.GetSheetByGUID(const AGUID: string): TdxSpreadSheetTableView;
var
  I: Integer;
  ASheet: TdxSpreadSheetTableView;
begin
  for I := 0 to SheetCount - 1 do
  begin
    ASheet := Sheets[I] as TdxSpreadSheetTableView;
    if SameText(AGUID, ASheet.GUID) then
      Exit(ASheet);
  end;
  Result := nil;
end;

function TdxCustomSpreadSheet.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

function TdxCustomSpreadSheet.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick and ((ActiveController = nil) or ActiveController.CanFocusOnClick);
end;

function TdxCustomSpreadSheet.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  AController: TdxSpreadSheetCustomController;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    AController := ActiveController;
    if (AController <> nil) or ControllerFromPoint(GetMouseCursorClientPos, AController) then
      Result := AController.MouseWheel(Shift, WheelDelta, ScreenToClient(MousePos));
  end;
end;

procedure TdxCustomSpreadSheet.DoPaint;
begin
  inherited DoPaint;
  if LockCount = 0 then
  begin
    if PageControl.Visible then
      PageControl.ViewInfo.Draw(Canvas);
    Canvas.FillRect(ClientBounds, ViewInfo.BackgroundParams);
    if ActiveSheet <> nil then
      ActiveSheet.ViewInfo.Draw(Canvas);
  end;
  Canvas.FillRect(ClientBounds, ViewInfo.BackgroundParams);
end;

function TdxCustomSpreadSheet.GetClientBounds: TRect;
begin
  Result := inherited GetClientBounds;
  if (IsPopupScrollBars or not HScrollBarVisible) and PageControl.Visible then
    Dec(Result.Bottom, PageControl.ViewInfo.MeasureHeight);
end;

function TdxCustomSpreadSheet.GetCurrentCursor(X, Y: Integer): TCursor;
var
  AController: TdxSpreadSheetCustomController;
begin
  Result := inherited GetCurrentCursor(X, Y);
  if not IsDesigning then
  begin
    if ControllerFromPoint(Point(X, Y), AController) then
      Result := AController.GetCursor(Point(X, Y));
  end;
end;

function TdxCustomSpreadSheet.GetHScrollBarBounds: TRect;
begin
  Result := inherited GetHScrollBarBounds;
  if IsPopupScrollBars then
  begin
    if not UseRightToLeftAlignment then
    begin
      if ActiveSheet <> nil then
        Inc(Result.Left, MulDiv(ActiveSheetAsTable.ViewInfo.CellsArea.X, ActiveSheet.ZoomFactor, 100));
    end;
    Result.Top := Result.Bottom - GetScrollBarSize.cy;
  end
  else
  begin
    Inc(Result.Left, PageControl.ViewInfo.MeasureWidth);
    Result.Top := Result.Bottom - PageControl.ViewInfo.MeasureHeight;
  end;
end;

function TdxCustomSpreadSheet.GetSizeGripBounds: TRect;
begin
  if IsPopupScrollBars then
    Result := inherited GetSizeGripBounds
  else
  begin
    Result := cxRect(ClientBounds.BottomRight, ClientBounds.BottomRight);
    if HScrollBarVisible or PageControl.Visible then
      Result.Bottom := Result.Top + PageControl.ViewInfo.MeasureHeight;
    if VScrollBarVisible then
      Result.Right := Result.Left + VScrollBar.Width;
  end;
end;

function TdxCustomSpreadSheet.GetScrollContentForegroundColor: TColor;
begin
  Result := LookAndFeelPainter.SpreadSheetContentTextColor;
end;

function TdxCustomSpreadSheet.GetVScrollBarBounds: TRect;
begin
  if IsPopupScrollBars then
  begin
    Result := ClientBounds;
    if UseRightToLeftScrollBar then
      Result.Right := Result.Left + VScrollBar.Width
    else
      Result.Left := Result.Right - VScrollBar.Width;
    if ActiveSheet <> nil then
      Inc(Result.Top, MulDiv(ActiveSheetAsTable.ViewInfo.CellsArea.Y, ActiveSheet.ZoomFactor, 100));
    if HScrollBarVisible then
      Result.Bottom := Result.Bottom - HScrollBar.Height;
  end
  else
    Result := inherited GetVScrollBarBounds
end;

function TdxCustomSpreadSheet.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := nil;
  if (ActiveController <> nil) and (ActiveController.HitTest.HitObject <> nil) then
    Result := ActiveController.HitTest.GetDragAndDropObjectClass;
  if Result = nil then
    Result := inherited GetDragAndDropObjectClass;
end;

procedure TdxCustomSpreadSheet.InitScrollBarsParameters;
begin
  if ActiveSheet <> nil then
    ActiveSheet.InitScrollBarsParameters
  else
  begin
    SetScrollBarInfo(sbHorizontal, 0, 1, 1, 1, 0, False, True);
    SetScrollBarInfo(sbVertical, 0, 1, 1, 1, 0, False, True);
  end;
end;

function TdxCustomSpreadSheet.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

function TdxCustomSpreadSheet.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
end;

function TdxCustomSpreadSheet.IsSizeGripVisible: Boolean;
begin
  Result := inherited IsSizeGripVisible or (PageControl.Visible and VScrollBarVisible);
end;

procedure TdxCustomSpreadSheet.Loaded;
begin
  inherited Loaded;
  FSheets.Loaded;
  Exclude(FChanges, sscModified);
  CheckChanges;
  if ActiveSheet <> nil then
    DoSelectionChanged(ActiveSheet);
end;

procedure TdxCustomSpreadSheet.LockClipboardListener;
begin
  FIsClipboardListenerLocked := True;
end;

procedure TdxCustomSpreadSheet.UnlockClipboardListener;
begin
  FIsClipboardListenerLocked := False;
end;

procedure TdxCustomSpreadSheet.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  FFormattedSharedStringCache.Clear;
  LayoutChanged;
end;

procedure TdxCustomSpreadSheet.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  Listeners.NotifyKeyDown(Self, Key, Shift);
end;

procedure TdxCustomSpreadSheet.DoKeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
end;

procedure TdxCustomSpreadSheet.DoKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure TdxCustomSpreadSheet.KeyDown(var Key: Word; Shift: TShiftState);
begin
  DoKeyDown(Key, Shift);
  if ActiveSheet <> nil then
    ActiveSheet.Controller.KeyDown(Key, Shift);
  UpdateCursor;
end;

procedure TdxCustomSpreadSheet.KeyUp(var Key: Word; Shift: TShiftState);
begin
  DoKeyUp(Key, Shift);
  if ActiveSheet <> nil then
    ActiveSheet.Controller.KeyUp(Key, Shift);
  if Key = VK_BACK then
    Key := 0;
  UpdateCursor;
end;

procedure TdxCustomSpreadSheet.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if ActiveSheet <> nil then
    ActiveSheet.Controller.KeyPress(Key)
end;

procedure TdxCustomSpreadSheet.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AController: TdxSpreadSheetCustomController;
begin
  UpdateHitTests(X, Y);
  if ControllerFromPoint(X, Y, AController) then
    ActiveController := AController
  else
    ActiveController := nil;

  inherited MouseDown(Button, Shift, X, Y);

  if ActiveController <> nil then
  begin
    ActiveController.MouseDown(Button, Shift, X, Y);
    if ssDouble in Shift then
      ActiveController.DblClick;
  end;
  UpdateHitTests(X, Y);
end;

procedure TdxCustomSpreadSheet.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  UpdateHitTests(cxInvisiblePoint);
  UpdateStates;
end;

procedure TdxCustomSpreadSheet.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AController: TdxSpreadSheetCustomController;
begin
  UpdateHitTests(X, Y);
  inherited MouseMove(Shift, X, Y);
  if ControllerFromPoint(X, Y, AController) then
    AController.MouseMove(Shift, X, Y);
  UpdateHitTests(X, Y);
  UpdateStates;
end;

procedure TdxCustomSpreadSheet.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AController: TdxSpreadSheetCustomController;
begin
  UpdateHitTests(X, Y);
  inherited MouseUp(Button, Shift, X, Y);
  UpdateHitTests(X, Y); // # Need update HitTest because after mouse up drag drop can be finished
  if ControllerFromPoint(X, Y, AController) then
    AController.MouseUp(Button, Shift, X, Y);
  ActiveController := nil;
  UpdateHitTests(X, Y);
end;

procedure TdxCustomSpreadSheet.Pack;
var
  I: Integer;
begin
  for I := 0 to SheetCount - 1 do
    Sheets[I].Pack;
end;

procedure TdxCustomSpreadSheet.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if ActiveSheet <> nil then
  begin
    if IsGestureScrolling then
    begin
      ActiveSheet.Scroll(AScrollBarKind, AScrollCode, AScrollPos);
      Exit;
    end;

    if (AScrollCode = scTrack) and not FScrollbarsLocked then
    begin
      FScrollbarsLocked := True;
      LockScrollBars;
    end;

    ActiveSheet.Scroll(AScrollBarKind, AScrollCode, AScrollPos);

    if (AScrollCode = scEndScroll) and FScrollbarsLocked then
    begin
      FScrollbarsLocked := False;
      UnlockScrollBars;
      UpdateScrollBars;
    end;
  end;
end;

procedure TdxCustomSpreadSheet.SetPaintRegion;
begin
  inherited SetPaintRegion;
  if PageControl.Visible then
    Canvas.SetClipRegion(TcxRegion.Create(PageControl.ViewInfo.Bounds), roAdd);
end;

procedure TdxCustomSpreadSheet.SetSheetIndex(AView: TdxSpreadSheetCustomView; AIndex: Integer);
var
  AActive: Boolean;
begin
  AActive := AView.Active;
  FSheets.Remove(AView);
  FSheets.Insert(AIndex, AView);
  UpdateVisibleSheetList;
  if AActive then
    FActiveSheetIndex := FSheets.IndexOf(AView);
  AddChanges([sscLayout, sscModified]);
end;

function TdxCustomSpreadSheet.StartDragAndDrop(const P: TPoint): Boolean;
var
  AController: TdxSpreadSheetCustomController;
begin
  Result := ControllerFromPoint(P, AController) and AController.HitTest.CanDrag(P);
end;

procedure TdxCustomSpreadSheet.StopEditing;
var
  I: Integer;
begin
  if ActiveController <> nil then
    ActiveController.StopEditing;
  for I := 0 to SheetCount - 1 do
    Sheets[I].Controller.StopEditing;
end;

procedure TdxCustomSpreadSheet.StyleChanged(Sender: TObject);
begin
end;

procedure TdxCustomSpreadSheet.UpdateHitTests(X, Y: Integer);
begin
  UpdateHitTests(Point(X, Y));
end;

procedure TdxCustomSpreadSheet.UpdateStates;
begin
  if ActiveSheet <> nil then
    ActiveSheet.Controller.UpdateStates;
  if PageControl <> nil then
    PageControl.Controller.UpdateStates;
end;

procedure TdxCustomSpreadSheet.UpdateVisibleSheetList;
var
  ASheet: TdxSpreadSheetCustomView;
  I: Integer;
begin
  FVisibleSheets.Clear;
  FVisibleSheets.Capacity := SheetCount;
  for I := 0 to SheetCount - 1 do
  begin
    ASheet := Sheets[I];
    if ASheet.Visible then
      FVisibleSheets.Add(ASheet);
  end;
end;

procedure TdxCustomSpreadSheet.UnprotectCore(AOptions: TdxSpreadSheetCustomProtectionOptions);
var
  APassword: string;
begin
  if AOptions.ProtectionInfo <> nil then
  begin
    if ShowPasswordDialog(Self, pdmQuery, APassword) then
    begin
      if not AOptions.ProtectionInfo.CheckPassword(APassword) then
        raise Exception.Create(cxGetResourceString(@sdxOleCryptoContainerInvalidPassword));
      AOptions.ProtectionInfo := nil;
    end;
  end;
  if AOptions.ProtectionInfo = nil then
    AOptions.Protected := False;
end;

procedure TdxCustomSpreadSheet.UpdateCursor;
begin
  if HandleAllocated then
    Perform(WM_SETCURSOR, Handle, HTCLIENT);
end;

procedure TdxCustomSpreadSheet.UpdateHitTests(const P: TPoint);
begin
  if ActiveSheet <> nil then
    ActiveSheet.HitTest.Calculate(P);
  PageControl.HitTest.Calculate(P)
end;

function TdxCustomSpreadSheet.ValidateSheetCaption(ACaption: string): string;

  function DoCheckSheetCaption(const AFormatLine: string): string;
  var
    AIndex: Integer;
  begin
    AIndex := 1;
    while GetSheetByName(Format(AFormatLine, [AIndex])) <> nil do
      Inc(AIndex);
    Result := Format(AFormatLine, [AIndex]);
  end;

  function ValidateSheetCaptionLength(const ACaption: string): string;
  var
    AIndex: Integer;
  begin
    Result := ACaption;
    if Length(Result) > dxSpreadSheetMaxCaptionLength then
    begin
      Result := Copy(Result, 1, dxSpreadSheetMaxCaptionLength);
      if GetSheetByName(Result) <> nil then
      begin
        AIndex := 1;
        repeat
          Result := DoCheckSheetCaption(Copy(Result, 1, dxSpreadSheetMaxCaptionLength - AIndex) + '%d');
          Inc(AIndex);
        until Length(Result) <= dxSpreadSheetMaxCaptionLength;
      end;
    end;
  end;

begin
  if ACaption = '' then
    ACaption := DoCheckSheetCaption(cxGetResourceString(@sdxDefaultSheetCaption));
  Result := ValidateSheetCaptionLength(ACaption);
end;

function TdxCustomSpreadSheet.DoPrepareLockedStateImage: Boolean;
begin
  Result := False;
  if Assigned(OnPrepareLockedStateImage) then
    OnPrepareLockedStateImage(Self, LockedStatePaintHelper.Bitmap, Result);
end;

function TdxCustomSpreadSheet.DialogsGetLookAndFeel: TcxLookAndFeel;
begin
  Result := DialogsLookAndFeel;
end;

function TdxCustomSpreadSheet.DialogsGetOwner: TComponent;
begin
  Result := Self;
end;

function TdxCustomSpreadSheet.DialogsGetParentForm: TCustomForm;
begin
  Result := GetParentForm(Self);
end;

function TdxCustomSpreadSheet.GetLockedStateImage: TcxBitmap32;
begin
  Result := LockedStatePaintHelper.GetImage;
end;

function TdxCustomSpreadSheet.GetLockedStateTopmostControl: TcxControl;
begin
  Result := Self;
end;

procedure TdxCustomSpreadSheet.UpdateLockedStateFont(AFont: TFont);
begin
  OptionsLockedStateImage.UpdateFont(AFont);
end;

function TdxCustomSpreadSheet.GetControl: TdxCustomSpreadSheet;
begin
  Result := Self;
end;

procedure TdxCustomSpreadSheet.CellStyleChanged;
begin
  BeginUpdate;
  try
    if ActiveSheet <> nil then
      ActiveSheet.RecalculateBestFit;
    AddChanges([sscLayout, sscModified]);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomSpreadSheet.CellStyleChanging;
begin
  // do nothing
end;

function TdxCustomSpreadSheet.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := FCellStyles;
end;

function TdxCustomSpreadSheet.GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
begin
  Result := FFormatSettings;
end;

procedure TdxCustomSpreadSheet.ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);
begin
  // do nothing
end;

function TdxCustomSpreadSheet.GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
begin
  Result := ActiveSheetAsTable.ConditionalFormatting;
end;

procedure TdxCustomSpreadSheet.InternalLoadFromStream(AStream: TStream;
  AFormat: TdxSpreadSheetCustomFormatClass; AProgressEvent: TdxSpreadSheetProgressEvent);

  procedure ExchangeFormatSettings(var AFormatSettings: TdxSpreadSheetFormatSettings);
  begin
    if AFormatSettings <> nil then
      ExchangePointers(AFormatSettings, FFormatSettings);
    FormulaController.TranslationChanged;
  end;

var
  AFormatSettings: TdxSpreadSheetFormatSettings;
  AMemStream: TMemoryStream;
  AReader: TdxSpreadSheetCustomReader;
begin
  StopEditing;
  if AFormat.GetReader = nil then
    raise EdxSpreadSheetFormatError.Create(cxGetResourceString(@sdxErrorUnsupportedDocumentFormat));

  BeginUpdate(lsimImmediate);
  ShowHourglassCursor;
  try
    History.Lock;
    Include(FState, sssReading);
    try
      History.Clear;
      AFormatSettings := AFormat.CreateFormatSettings;
      try
        ExchangeFormatSettings(AFormatSettings);
        AMemStream := TMemoryStream.Create;
        try
          InternalSaveToStream(AMemStream, TdxSpreadSheetBinaryFormat, nil);
          try
            ClearAll;
            AReader := AFormat.CreateReader(Self, AStream);
            try
              AReader.OnProgress := AProgressEvent;
              AReader.ReadData;
            finally
              AReader.Free;
            end;
            if SheetCount = 0 then
              AddSheet;
            Pack;
          except
            AMemStream.Position := 0;
            InternalLoadFromStream(AMemStream, TdxSpreadSheetBinaryFormat, nil);
            raise;
          end;
        finally
          AMemStream.Free;
        end;
      finally
        ExchangeFormatSettings(AFormatSettings);
        AFormatSettings.Free;
      end;
      AfterLoad;
    finally
      Exclude(FState, sssReading);
      History.Unlock;
    end;
  finally
    try
      EndUpdate;
    finally
      HideHourglassCursor;
      Modified := False;
    end;
  end;
end;

procedure TdxCustomSpreadSheet.InternalSaveToStream(AStream: TStream;
  AFormat: TdxSpreadSheetCustomFormatClass; AProgressEvent: TdxSpreadSheetProgressEvent);
var
  AFormatSettings: TdxSpreadSheetFormatSettings;
  AWriter: TdxSpreadSheetCustomWriter;
begin
  StopEditing;
  if AFormat = nil then
    AFormat := TdxSpreadSheetBinaryFormat;

  if AFormat.GetWriter = nil then
    raise EdxSpreadSheetFormatError.Create(cxGetResourceString(@sdxErrorUnsupportedDocumentFormat));

  BeginUpdate(lsimImmediate);
  ShowHourglassCursor;
  try
    Include(FState, sssWriting);
    try
      Pack;
      AFormatSettings := AFormat.CreateFormatSettings;
      try
        if AFormatSettings <> nil then
          ExchangePointers(AFormatSettings, FFormatSettings);
        AWriter := AFormat.CreateWriter(Self, AStream);
        try
          AWriter.OnProgress := AProgressEvent;
          AWriter.WriteData;
        finally
          AWriter.Free;
        end;
      finally
        if AFormatSettings <> nil then
          ExchangePointers(AFormatSettings, FFormatSettings);
        AFormatSettings.Free;
      end;
    finally
      Exclude(FState, sssWriting);
    end;
  finally
    HideHourglassCursor;
    EndUpdate;
  end;
end;

function TdxCustomSpreadSheet.GetActiveSheet: TdxSpreadSheetCustomView;
begin
  if (ActiveSheetIndex >= 0) and (ActiveSheetIndex < SheetCount) then
    Result := Sheets[ActiveSheetIndex]
  else
    Result := nil;
end;

function TdxCustomSpreadSheet.GetActiveSheetAsTable: TdxSpreadSheetTableView;
begin
  Result := ActiveSheet as TdxSpreadSheetTableView;
end;

function TdxCustomSpreadSheet.GetIsLocked: Boolean;
begin
  Result := (LockCount > 0) or IsLoading or IsDestroying;
end;

function TdxCustomSpreadSheet.GetSheet(AIndex: Integer): TdxSpreadSheetCustomView;
begin
  Result := TdxSpreadSheetCustomView(FSheets[AIndex]);
end;

function TdxCustomSpreadSheet.GetSheetCount: Integer;
begin
  Result := FSheets.Count;
end;

function TdxCustomSpreadSheet.GetVisibleSheet(Index: Integer): TdxSpreadSheetCustomView;
begin
  Result := TdxSpreadSheetCustomView(FVisibleSheets[Index]);
end;

function TdxCustomSpreadSheet.GetVisibleSheetCount: Integer;
begin
  Result := FVisibleSheets.Count;
end;

procedure TdxCustomSpreadSheet.SetActiveSheet(AValue: TdxSpreadSheetCustomView);
begin
  if AValue <> nil then
    ActiveSheetIndex := AValue.Index;
end;

procedure TdxCustomSpreadSheet.SetActiveSheetIndex(AValue: Integer);

  function GetNearestAvailableIndex(AIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to VisibleSheetCount - 1 do
    begin
      if AIndex >= VisibleSheets[I].Index then
        Result := VisibleSheets[I].Index;
    end;
    if (Result < 0) and (VisibleSheetCount > 0) then
    begin
      if AIndex > FVisibleSheets.Last.Index then
        Result := FVisibleSheets.Last.Index
      else
        Result := FVisibleSheets.First.Index;
    end;
  end;

begin
  AValue := GetNearestAvailableIndex(AValue);
  if AValue <> ActiveSheetIndex then
  begin
    if ActiveSheet is TdxSpreadSheetTableView then
      ActiveSheetAsTable.Controller.CellHintController.Hide;
    FActiveSheetIndex := AValue;
    PageControl.MakeVisible(ActiveSheet);
    DoActiveSheetChanged;
  end;
end;

procedure TdxCustomSpreadSheet.SetDialogsLookAndFeel(AValue: TcxLookAndFeel);
begin
  FDialogsLookAndFeel.Assign(AValue);
end;

procedure TdxCustomSpreadSheet.SetIsClipboardListener(AValue: Boolean);
begin
  if (FIsClipboardListener <> AValue) and (Parent <> nil) then
  begin
    FIsClipboardListener := AValue;
    if IsClipboardListener then
      FNextClipboardListener := SetClipboardViewer(Handle)
    else
      ChangeClipboardChain(Handle, FNextClipboardListener);
  end;
end;

procedure TdxCustomSpreadSheet.SetModified(AValue: Boolean);
begin
  if FModified <> AValue then
  begin
    FModified := AValue;
    DoModifiedChanged;
  end;
end;

procedure TdxCustomSpreadSheet.SetOptionsBehavior(AValue: TdxSpreadSheetOptionsBehavior);
begin
  FOptionsBehavior.Assign(AValue);
end;

procedure TdxCustomSpreadSheet.SetOptionsLockedStateImage(AValue: TdxSpreadSheetLockedStateImageOptions);
begin
  FOptionsLockedStateImage.Assign(AValue);
end;

procedure TdxCustomSpreadSheet.SetOptionsProtection(AValue: TdxSpreadSheetWorkbookProtectionOptions);
begin
  FOptionsProtection.Assign(AValue);
end;

procedure TdxCustomSpreadSheet.SetOptionsView(AValue: TdxSpreadSheetOptionsView);
begin
  FOptionsView.Assign(AValue);
end;

procedure TdxCustomSpreadSheet.SetPageControl(AValue: TdxSpreadSheetPageControl);
begin
  FPageControl.Assign(AValue);
end;

procedure TdxCustomSpreadSheet.SetStyle(AValue: TdxSpreadSheetStyles);
begin
  FStyles.Assign(AValue);
end;

procedure TdxCustomSpreadSheet.ReadBinaryData(AStream: TStream);
var
  AMemStream: TMemoryStream;
  ASize: Integer;
begin
  AMemStream := TMemoryStream.Create;
  try
    AStream.ReadBuffer(ASize, SizeOf(ASize));
    AMemStream.Size := ASize;
    AStream.ReadBuffer(AMemStream.Memory^, AMemStream.Size);
    AMemStream.Position := 0;
    InternalLoadFromStream(AMemStream, TdxSpreadSheetBinaryFormat, nil);
  finally
    AMemStream.Free;
  end;
end;

procedure TdxCustomSpreadSheet.ForwardMessage(var AMessage: TMessage);
begin
  if FNextClipboardListener <> 0 then
    SendMessage(FNextClipboardListener, AMessage.Msg, AMessage.WParam, AMessage.LParam);
end;

procedure TdxCustomSpreadSheet.WMCancelHint(var Message: TMessage);
var
  ASheet: TdxSpreadSheetTableView;
begin
  ASheet := TdxSpreadSheetTableView(Message.WParam);
  if FSheets.IndexOf(ASheet) >= 0 then
  begin
    ASheet.Controller.CellHintController.Hide;
    ASheet.Controller.FocusedContainer := nil;
  end;
end;

procedure TdxCustomSpreadSheet.WMChangeCBChain(var AMessage: TWMChangeCBChain);
begin
  if AMessage.Remove = FNextClipboardListener then
    FNextClipboardListener := AMessage.Next
  else
    ForwardMessage(TMessage(AMessage));
end;

procedure TdxCustomSpreadSheet.WMDrawClipboard(var AMessage: TWMDrawClipboard);
var
  AArea: TRect;
  ACopyMode: TdxSpreadSheetClipboardCopyMode;
  AView: TdxSpreadSheetTableView;
  AViewGUID: string;
begin
  if IsClipboardListener then
  begin
    if Application.Handle = GetClipboardOwner then
    begin
      TdxSpreadSheetTableViewClipboardHelper.GetDataInfoFromClipboard(AViewGUID, ACopyMode, AArea);
      AView := GetSheetByGUID(AViewGUID);
      if ClipboardArea.View <> AView then
        ClipboardArea.Reset;
      if AView <> nil then
        ClipboardArea.Initialize(AView, AArea)
    end
    else
      ClipboardArea.Reset;

    IsClipboardListener := FIsClipboardListenerLocked;
  end;
  ForwardMessage(TMessage(AMessage));
end;

procedure TdxCustomSpreadSheet.WMIMEComposition(var Message: TMessage);
begin
  if (ActiveSheet = nil) or not ActiveSheet.Controller.IMEComposition(Message) then
    inherited;
end;

procedure TdxCustomSpreadSheet.WMIMEStartComposition(var Message: TMessage);
begin
  if (ActiveSheet = nil) or not ActiveSheet.Controller.IMEStartComposition then
    inherited;
end;

procedure TdxCustomSpreadSheet.WMCopy(var Message: TMessage);
begin
  if ActiveSheet is TdxSpreadSheetTableView then
    TdxSpreadSheetTableView(ActiveSheet).CopyToClipboard;
end;

procedure TdxCustomSpreadSheet.WMCut(var Message: TMessage);
begin
  if ActiveSheet is TdxSpreadSheetTableView then
    TdxSpreadSheetTableView(ActiveSheet).CutToClipboard;
end;

procedure TdxCustomSpreadSheet.WMPaste(var Message: TMessage);
begin
  if ActiveSheet is TdxSpreadSheetTableView then
    TdxSpreadSheetTableView(ActiveSheet).PasteFromClipboard;
end;

procedure TdxCustomSpreadSheet.WMPostHideEdit(var Message: TMessage);
var
  AController: TdxSpreadSheetTableViewEditingController;
  I: Integer;
begin
  for I := 0 to SheetCount - 1 do
  begin
    AController := TdxSpreadSheetTableView(Sheets[I]).EditingController;
    if WPARAM(AController) = Message.WParam then
      AController.EditExit(Self);
  end;
end;

procedure TdxCustomSpreadSheet.WriteBinaryData(AStream: TStream);
var
  AMemStream: TMemoryStream;
  ASize: Integer;
begin
  AMemStream := TMemoryStream.Create;
  try
    InternalSaveToStream(AMemStream, TdxSpreadSheetBinaryFormat, nil);
    ASize := AMemStream.Size;
    AStream.WriteBuffer(ASize, SizeOf(ASize));
    AStream.WriteBuffer(AMemStream.Memory^, ASize);
  finally
    AMemStream.Free;
  end;
end;

{ TdxSpreadSheetDefaultCellStyle }

constructor TdxSpreadSheetDefaultCellStyle.Create(const AOwner: IdxSpreadSheetCellStyleOwner);
begin
  inherited Create(AOwner);
  FHandle := CellStyles.DefaultStyle;
end;

procedure TdxSpreadSheetDefaultCellStyle.Reset;
begin
  Handle := CellStyles.AddStyle(CellStyles.CreateStyle);
end;

procedure TdxSpreadSheetDefaultCellStyle.CloneHandle;
begin
  // do nothing
end;

procedure TdxSpreadSheetDefaultCellStyle.SetHandle(const AHandle: TdxSpreadSheetCellStyleHandle);
begin
  if CellStyles.DefaultStyle <> AHandle then
    CellStyles.DefaultStyle.Assign(AHandle);
end;

procedure TdxSpreadSheetDefaultCellStyle.ReleaseHandle;
begin
  // do nothing
end;

procedure TdxSpreadSheetDefaultCellStyle.ReplaceHandle;
begin
  DoChanged;
end;

{ TdxSpreadSheetTextService }

class function TdxSpreadSheetTextService.GetAsRTF(ACell: TdxSpreadSheetCell; var AValue: string): Boolean;
begin
  Result := False;
end;

class function TdxSpreadSheetTextService.IsRTFSupported: Boolean;
begin
  Result := False;
end;

class function TdxSpreadSheetTextService.SetAsRTF(ACell: TdxSpreadSheetCell; const AEditValue: string): Boolean;
begin
  Result := False;
end;

class procedure TdxSpreadSheetTextService.ApplyDefaultStyle(ACell: TdxSpreadSheetCell; const AEditValue: string);
begin
  // do nothing
end;

class procedure TdxSpreadSheetTextService.CalculateSize(ACell: TdxSpreadSheetCell;
  ACanvas: TcxCanvas; const ABounds: TRect; AIsMerged: Boolean; AWidth, AHeight: PInteger);
var
  AFlags: Integer;
  ALineCount: Integer;
  ALinesInRow: Integer;
  AText: string;
  ATextParams: TcxTextParams;
  ATextRows: TcxTextRows;
begin
  AFlags := CXTO_CALCROWCOUNT or CXTO_CHARBREAK or CXTO_AUTOINDENTS or cxMakeFormat(taLeft, taTop);
  if not (AIsMerged and (AHeight <> nil) and (AWidth = nil)) and ACell.IsMultiline then
    AFlags := AFlags or CXTO_WORDBREAK;

  ACell.StyleHandle.Font.AssignToFont(ACanvas.Font);
  ATextParams := cxCalcTextParams(ACanvas.Handle, AFlags, ExcelLineSpacing);
  ALinesInRow := Max(1, cxRectHeight(ABounds) div ATextParams.FullRowHeight);

  AText := ACell.DisplayValue.Text;
  cxMakeTextRows(ACanvas.Handle, PWideChar(AText), Length(AText), ABounds, ATextParams, ATextRows, ALineCount);
  try
    if AHeight <> nil then
      AHeight^ := ALineCount * ATextParams.FullRowHeight;
    if AWidth <> nil then
    begin
      if ALineCount > ALinesInRow then
        AWidth^ := cxRectWidth(cxGetTextRect(ACanvas.Handle, AText, ALinesInRow)) + cxTextOffset
      else
        AWidth^ := cxGetLongestTextRowWidth(ATextRows, ALineCount) + cxTextOffset;
    end;
  finally
    cxResetTextRows(ATextRows);
  end;
end;

class procedure TdxSpreadSheetTextService.CalculateTextBounds(
  ACanvas: TcxCanvas; ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo; var ATextBounds: TRect);
begin
  cxTextOut(ACanvas.Handle, ACell.FDisplayText, ATextBounds,
    ACell.GetTextOutFormat or CXTO_CALCRECT, nil, 0, 0, 0, clDefault, ExcelLineSpacing);
end;

class procedure TdxSpreadSheetTextService.DrawValue(
  ACanvas: TcxCanvas; ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo; ABounds: TRect);
begin
  cxTextOut(ACanvas.Handle, ACell.DisplayText, ABounds,
    ACell.GetTextOutFormat, nil, 0, 0, 0, ACanvas.Font.Color, ExcelLineSpacing);
end;

class function TdxSpreadSheetTextService.ForceSetAsRTF(ACell: TdxSpreadSheetCell; const AEditValue: string): Boolean;
begin
  Result := False;
end;

class procedure TdxSpreadSheetTextService.MeasureSize(ACanvas: TcxCanvas; ACell: TdxSpreadSheetCell; AWidth, AHeight: PInteger);
var
  ABounds: TRect;
  AHasText: Boolean;
  AIndents: TRect;
  AMergedCell: TdxSpreadSheetMergedCell;
  AView: TdxSpreadSheetTableView;
begin
  if AHeight <> nil then
    AHeight^ := 0;
  if AWidth <> nil then
    AWidth^ := 0;

  AHasText := not ACell.IsEmpty and (ACell.DisplayValue.Text <> '');
  if not AHasText and (AHeight = nil) then
    Exit;

  AView := ACell.View;
  ABounds := Bounds(ACell.ColumnIndex, ACell.RowIndex, 0, 0);
  AMergedCell := AView.MergedCells.FindCell(ABounds.Top, ABounds.Left);
  if AMergedCell <> nil then
  begin
    if Assigned(AHeight) and (AMergedCell.Area.Top <> AMergedCell.Area.Bottom) or
       Assigned(AWidth) and (AMergedCell.Area.Left <> AMergedCell.Area.Right) or
      (AMergedCell.Area.Left <> ABounds.Left) or (AMergedCell.Area.Top <> ABounds.Top)
    then
      Exit;

    ABounds := cxRectBounds(0, 0,
      AView.Columns.GetDistance(AMergedCell.Area.Left, AMergedCell.Area.Right),
      AView.Rows.GetDistance(AMergedCell.Area.Top, AMergedCell.Area.Bottom));
  end
  else
    ABounds := cxRectBounds(0, 0, AView.Columns.GetRealItemSize(ABounds.Left), AView.Rows.GetRealItemSize(ABounds.Bottom));

  AIndents := ACell.GetContentOffsets(ACell.StyleHandle.Borders.BorderStyle[bBottom]);
  ABounds := cxRectContent(ABounds, AIndents);
  ABounds.Right := Max(ABounds.Left + 1, ABounds.Right - ACell.StyleHandle.AlignHorzIndent);

  CalculateSize(ACell, ACanvas, ABounds, AMergedCell <> nil, AWidth, AHeight);
  if AHeight <> nil then
    Inc(AHeight^, cxMarginsHeight(AIndents));
  if AWidth <> nil then
  begin
    if AHasText then
      Inc(AWidth^, ACell.StyleHandle.AlignHorzIndent + cxMarginsWidth(AIndents))
    else
      AWidth^ := 0;
  end;
end;

class function TdxSpreadSheetTextService.IsFormattedEditValue(const AEditValue: string): Boolean;
begin
  Result := False;
end;

class function TdxSpreadSheetTextService.IsFormattedTextValue(ACell: TdxSpreadSheetCell): Boolean;
begin
  Result := False;
end;

class procedure TdxSpreadSheetTextService.Register;
begin
  dxSpreadSheetTextService := Self;
end;

class procedure TdxSpreadSheetTextService.Unregister;
begin
  dxSpreadSheetTextService := TdxSpreadSheetTextService;
end;

{ TdxSpreadSheetRTFService }

class function TdxSpreadSheetRTFService.CurrentService: TdxSpreadSheetTextServiceClass;
begin
  Result := dxSpreadSheetTextService;
end;

initialization
  Screen.Cursors[crdxSpreadSheetRotate] := LoadCursor(HInstance, 'DXSPREADSHEET_ROTATECURSOR');
  Screen.Cursors[crdxSpreadSheetRotation] := LoadCursor(HInstance, 'DXSPREADSHEET_ROTATIONCURSOR');
  Screen.Cursors[crdxSpreadSheetDownArrow] := LoadCursor(HInstance, 'DXSPREADSHEET_DOWNARROW');
  Screen.Cursors[crdxSpreadSheetLeftArrow] := LoadCursor(HInstance, 'DXSPREADSHEET_LEFTARROW');
  Screen.Cursors[crdxSpreadSheetCross] := LoadCursor(HInstance, 'DXSPREADSHEET_CROSS');

  RegisterClasses([TdxSpreadSheetTableView, TdxSpreadSheetContainer]);

finalization
  FreeAndNil(FSpreadSheetFormats);
end.

