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

unit dxRichEdit.LayoutEngine.Formatter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Classes, SysUtils, Contnrs, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxGenerics,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxCharacters,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.Control.HitTest,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Control.Hyphenations,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable;

type
  TdxLineSpacingCalculatorBase = class;
  TdxRowSpacingParametersCalculatorBase = class;
  TdxUnderlineCalculator = class;
  TdxStrikeoutCalculator = class;

  TdxCharacterFormatterState = (
    Start,
    Spaces,
    Tab,
    FirstDash,
    Dash,
    Separator,
    LineBreak,
    PageBreak,
    ColumnBreak,
    Text,
    InlineObject,
    LayoutDependentText,
    FloatingObject,
    Final
  );

  TdxParagraphBoxFormatterState = (
    ParagraphStart,
    ParagraphStartAfterBreak,
    ParagraphStartFromTheMiddle,
    ParagraphStartAfterFloatingObject,
    ParagraphStartFromTheMiddleAfterFloatingObject,
    RowEmpty,
    RowEmptyAfterFloatingObject,
    RowWithSpacesOnly,
    RowWithTextOnly,
    RowWithInlineObjectOnly,
    RowWithTextOnlyAfterFloatingObject,
    RowWithDashOnly,
    RowWithDashOnlyAfterFloatingObject,
    RowWithTextOnlyAfterFirstLeadingTab,
    RowWithTextOnlyAfterFirstLeadingTabAfterFloatingObject,
    RowSpaces,
    RowText,
    RowTextAfterFloatingObject,
    RowDashAfterText,
    RowDash,
    RowDashAfterFloatingObject,
    RowWithDashAfterTextOnly,
    RowTextSplit,
    RowTextSplitAfterFloatingObject,
    RowTextSplitFloatingObject,
    RowTextSplitAfterFirstLeadingTab,
    RowTextSplitAfterFirstLeadingTabAfterFloatingObject,

    RowDashSplit,
    RowDashSplitAfterFloatingObject,

    RowEmptyHyphenation,
    RowEmptyHyphenationAfterFloatingObject,
    RowTextHyphenationFirstSyllable,
    RowTextHyphenationFirstSyllableAfterFloatingObject,
    RowTextHyphenationFirstSyllableAfterFirstLeadingTab,
    RowTextHyphenationFirstSyllableAfterFirstLeadingTabAfterFloatingObject,
    RowTextHyphenation,
    RowTextHyphenationAfterFloatingObject,
    RowFirstLeadingTab,
    RowLeadingTab,
    RowLeadingTabAfterFloatingObject,
    RowTab,
    RowTabAfterFloatingObject,
    RowLineBreak,
    RowPageBreak,
    RowPageBreakAtParagraphStart,
    RowColumnBreak,
    RowColumnBreakAtParagraphStart,

    SectionBreakAfterParagraphMark,
    FloatingObject,
    ParagraphFrame,

    ContinueFromParagraph,

    Final
  );

  TdxFinalizeEvent = procedure (ABoxInfo: TdxBoxInfo) of object;

  TdxFormattingProcess = (
    ContinueFromParagraph,
    Continue,
    RestartFromTheStartOfRow,
    Finish);

  TdxAddBoxResult = (
    Success,
    HorizontalIntersect,
    LeaveColumnPlain,
    LeaveColumnFirstCellRow,
    IntersectWithFloatObject,
    RestartDueFloatingObject
  );

  TdxSplitBoxResult = (
    Success,
    SuccessSuppressedHorizontalOverfull,
    FailedHorizontalOverfull
  );

  TdxAdjustEndPositionResult = (
    Success,
    Failure
  );

  TdxStateContinueFormatResult = (
    Success,
    RestartDueFloatingObject,
    RestartDueOrphanedObjects
  );

  TdxClearInvalidatedContentResult = (
    Restart,
    RestartFromTheStartOfSection,
    NoRestart,
    ClearOnlyTableCellRows
  );

  TdxCanFitFloatingObjectResult = (
    Always,
    Never,
    DependsOnTable
  );

  TdxParagraphBoxFormatter = class;
  TdxParagraphBoxIterator = class;
  TdxRowsController = class;
  TdxPageAreaController = class;
  TdxTablesController = class;
  TdxTablesControllerStateBase = class;
  TdxFloatingObjectSizeAndPositionController = class;
  TdxFloatingObjectsLayout = class;

  { TdxCustomColumnController }

  TdxCustomColumnController = class abstract (TdxReferencedObject)
  strict private
    FChildren: TdxReferencedObjectList<TdxReferencedObject>;
  protected
    function GetCurrentPageBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect; virtual; abstract;
    function GetCurrentPageClientBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect; virtual; abstract;
    function GetMeasurer: TdxBoxMeasurer; virtual; abstract;
    function GetPageAreaController: TdxPageAreaController; virtual; abstract;
    function GetPageLastRunIndex: TdxRunIndex; virtual; abstract;
    function GetShouldZeroSpacingBeforeWhenMoveRowToNextColumn: Boolean; virtual; abstract;
    function GetTopLevelColumnsCount: Integer; virtual; abstract;

    procedure AddChild(AChild: TdxReferencedObject);
    procedure RemoveChild(AChild: TdxReferencedObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddInnerTable(ATableViewInfo: TdxTableViewInfo); virtual; abstract;
    function CompleteCurrentColumnFormatting(AColumn: TdxColumn): TdxCompleteFormattingResult; virtual; abstract;
    function CreateRow: TdxRow; virtual; abstract;
    function GetNextColumn(AColumn: TdxColumn; AKeepFloatingObjects: Boolean): TdxColumn; virtual; abstract;
    function GetPreviousColumn(AColumn: TdxColumn): TdxColumn; virtual; abstract;
    procedure Reset(ASection: TdxSection); virtual;
    procedure ResetToFirstColumn; virtual; abstract;

    property Measurer: TdxBoxMeasurer read GetMeasurer;
    property PageAreaController: TdxPageAreaController read GetPageAreaController;
    property PageLastRunIndex: TdxRunIndex read GetPageLastRunIndex;
    property ShouldZeroSpacingBeforeWhenMoveRowToNextColumn: Boolean read GetShouldZeroSpacingBeforeWhenMoveRowToNextColumn;
    property TopLevelColumnsCount: Integer read GetTopLevelColumnsCount;
  end;

  { IdxPageAreaController }

  IdxPageAreaController = interface
    procedure Reset(ASection: TdxSection);
    procedure BeginSectionFormatting(ASection: TdxSection; ACurrentColumnsCount: Integer);
    procedure RestartFormattingFromTheStartOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer);
    procedure RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer);
    procedure ClearInvalidatedContent(const APos: TdxFormatterPosition);
    function CompleteCurrentAreaFormatting: TdxCompleteFormattingResult;
    function GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea;
  end;

  { TdxTextArea }

  TdxTextArea = record
  public
    type
      TdxTextAreaStartComparer = class(TdxComparer<TdxTextArea>)
      public
        function Compare(const X, Y: TdxTextArea): Integer; override;
      end;
  strict private
    class var
      FStartComparer: TdxTextAreaStartComparer;
    class constructor Initialize;
    class destructor Finalize;
  public
    Start: Integer;
    &End: Integer;
    constructor Create(AStart, AEnd: Integer);
    function GetEmpty: TdxTextArea;
    function GetWidth: Integer;

    function IntersectsWith(const AInterval: TdxTextArea): Boolean;
    function IntersectsWithExcludingBounds(const AInterval: TdxTextArea): Boolean;
    class function Union(const AInterval1, AInterval2: TdxTextArea): TdxTextArea; static;
    function Subtract(const AInterval: TdxTextArea): TList<TdxTextArea>;
    function Contains(const AInterval: TdxTextArea): Boolean;
    function ToString: string;

    property Empty: TdxTextArea read GetEmpty;
    property Width: Integer read GetWidth;
    class property StartComparer: TdxTextAreaStartComparer read FStartComparer;
  end;

  { TdxColumnList }

  TdxColumnList = class(TdxReferencedObjectList<TdxColumn>);

  { TdxTableCellColumn }

  TdxTableCellColumn = class(TdxColumn)
  private
    FParent: TdxColumn;
    FCell: TdxTableCell;
  protected
    function GetRows: TdxRowCollection; override;
    function GetTopLevelColumn: TdxColumn; override;
  public
    constructor Create(AParent: TdxColumn; ACell: TdxTableCell);
    destructor Destroy; override;

    function GetOwnRows: TdxRowCollection; override;
    procedure AddParagraphFrame(AParagraph: TdxSimpleParagraph); override;

    property Parent: TdxColumn read FParent;
    property Cell: TdxTableCell read FCell;
  end;

  { TdxTableCellColumnController }

  TdxTableCellColumnController = class(TdxCustomColumnController)
  private
    FColumns: TdxBoxList;
    FCurrentParentColumn: TdxColumn;
    FCurrentCellViewInfo: TdxTableCellViewInfo;
    FParent: TdxCustomColumnController;
    FTop: Integer;
    FLeft: Integer;
    FWidth: Integer;
    FGeneratedColumns: TdxColumnList;
    FGeneratedTableViewInfo: TdxReferencedObjectList<TdxTableViewInfo>;
    FCurrentColumnIndex: Integer;
    FLastCreatedRow: TdxTableCellRow;
    FCurrentCell: TdxTableCell;
    function GetParentColumn: TdxColumn;
    function GetLastParentColumn: TdxColumn;
    function GetFirstParentColumn: TdxColumn;
    function GetCurrentTopLevelColumn: TdxColumn;
    function GetViewInfo: TdxTableViewInfo;
    procedure InternalSetCurrentParentColumn(const Value: TdxColumn);
    procedure SetParent(AValue: TdxCustomColumnController);
  protected
    procedure AddTableViewInfo(ACurrentTableViewInfo: TdxTableViewInfo);
    procedure RemoveTableViewInfo(ATableViewInfo: TdxTableViewInfo);
    function GetCurrentPageBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect; override;
    function GetCurrentPageClientBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect; override;
    function GetMeasurer: TdxBoxMeasurer; override;
    function GetPageAreaController: TdxPageAreaController; override;
    function GetPageLastRunIndex: TdxRunIndex; override;
    function GetShouldZeroSpacingBeforeWhenMoveRowToNextColumn: Boolean; override;
    function GetTopLevelColumnsCount: Integer; override;
    procedure SetCurrentTableCellViewInfo(AValue: TdxTableCellViewInfo);

    property ParentColumn: TdxColumn read GetParentColumn;
    property LastParentColumn: TdxColumn read GetLastParentColumn;
    property FirstParentColumn: TdxColumn read GetFirstParentColumn;
    property CurrentTopLevelColumn: TdxColumn read GetCurrentTopLevelColumn;
    property ViewInfo: TdxTableViewInfo read GetViewInfo;
    property CurrentParentColumn: TdxColumn read FCurrentParentColumn write InternalSetCurrentParentColumn;
  public
    constructor Create(AParent: TdxCustomColumnController; ACurrentParentColumn: TdxColumn; ALeft, ATop, AWidth: Integer;
      ATableViewInfo: TdxTableViewInfo; ACurrentCell: TdxTableCell);
    destructor Destroy; override;

    function CompleteCurrentColumnFormatting(AColumn: TdxColumn): TdxCompleteFormattingResult; override;
    procedure ResetToFirstColumn; override;
    procedure MoveToNextColumn;
    function GetStartColumn: TdxColumn;
    procedure StartNewCell(ACurrentParentColumn: TdxColumn; ALeft, ATop, AWidth: Integer; ACurrentCell: TdxTableCell);
    function GetMaxAnchor(AAnchor1, AAnchor2: TdxTableCellVerticalAnchor): TdxTableCellVerticalAnchor;
    function GetNextColumn(AColumn: TdxColumn; AKeepFloatingObjects: Boolean): TdxColumn; override;
    procedure SetCurrentParentColumn(AColumn: TdxColumn);
    function CreateRow: TdxRow; override;
    procedure AddInnerTable(ATableViewInfo: TdxTableViewInfo); override;
    procedure RemoveGeneratedColumn(AColumn: TdxColumn);
    function GetPreviousColumn(AColumn: TdxColumn): TdxColumn; override;

    property Parent: TdxCustomColumnController read FParent write SetParent;
    property CurrentCell: TdxTableCellViewInfo read FCurrentCellViewInfo;
  end;

  { TdxFormattingProcessResult }

  TdxFormattingProcessResult = record
    RestartPosition: TdxDocumentModelPosition;
    FormattingProcess: TdxFormattingProcess;
    ParagraphIndex: TdxParagraphIndex;
    ForceRestart: Boolean;
    procedure Init(AFormattingProcess: TdxFormattingProcess); overload;
    procedure Init(AParagraphIndex: TdxParagraphIndex); overload;
    procedure Init(const ARestartPosition: TdxDocumentModelPosition; AForceRestart: Boolean = False); overload;
  end;

  { TdxParagraphBoxFormatterTableStateBase }

  TdxParagraphBoxFormatterTableStateBase = class abstract
  private
    FFormatter: TdxParagraphBoxFormatter;
  protected
    procedure OnColumnOverfull; virtual; abstract;

    property Formatter: TdxParagraphBoxFormatter read FFormatter;
  public
    constructor Create(AFormatter: TdxParagraphBoxFormatter);
  end;

  { TdxParagraphBoxFormatterTextState }

  TdxParagraphBoxFormatterTextState = class(TdxParagraphBoxFormatterTableStateBase)
  protected
    procedure OnColumnOverfull; override;
  end;

  { TdxParagraphBoxFormatterTableState }

  TdxParagraphBoxFormatterTableState = class(TdxParagraphBoxFormatterTableStateBase)
  protected
    procedure OnColumnOverfull; override;
  end;

  { TdxBoxInfoList }

  TdxBoxInfoList = class(TdxFastObjectList)
  private
    function GetItem(Index: Integer): TdxBoxInfo;
  public
    property Items[Index: Integer]: TdxBoxInfo read GetItem; default;
  end;

  { TdxParagraphIteratorBase }

  TdxParagraphIteratorBase = class
  private
    FPieceTable: TdxPieceTable;
    FParagraph: TdxParagraph;
    FVisibleTextFilter: TdxVisibleTextFilterBase;
    FMaxOffset: Integer;
    FRunStartIndex: TdxRunIndex;
    FEndRunIndex: TdxRunIndex;
    FPosition: TdxFormatterPosition;
  protected
    function GetIsEnd: Boolean; virtual;
    function GetCurrentChar: Char; virtual;

    property Table: TdxPieceTable read FPieceTable;
    property PieceTable: TdxPieceTable read FPieceTable;
  public
    constructor Create(AParagraph: TdxParagraph; APieceTable: TdxPieceTable; AVisibleTextFilter: TdxVisibleTextFilterBase);
    function CreatePosition: TdxFormatterPosition; virtual; abstract;
    function IsInlinePictureRun: Boolean;
    function IsParagraphMarkRun: Boolean; virtual;
    function IsParagraphFrame: Boolean; virtual;
    function IsFloatingObjectAnchorRun: Boolean;
    procedure SetRunIndexCore(ARunIndex: TdxRunIndex);
    procedure SetPosition(const APos: TdxFormatterPosition); overload; virtual;
    procedure SetPositionCore(const APos: TdxFormatterPosition); virtual;
    procedure SetPosition(ARunIndex: TdxRunIndex; AOffset: Integer); overload; virtual;
    function Next: TdxParagraphIteratorResult; virtual;
    procedure NextOffset; virtual;
    procedure NextRun; virtual;
    function GetCurrentPosition: TdxFormatterPosition; virtual;
    function GetPreviousPosition: TdxFormatterPosition; virtual;
    function GetPreviousVisibleRunPosition: TdxFormatterPosition; virtual;
    function GetPreviousOffsetPosition: TdxFormatterPosition; virtual;

    property CurrentChar: Char read GetCurrentChar;
    property IsEnd: Boolean read GetIsEnd;
    property Offset: Integer read FPosition.Offset;
    property Paragraph: TdxParagraph read FParagraph;
    property RunIndex: TdxRunIndex read FPosition.RunIndex;
    property VisibleTextFilter: TdxVisibleTextFilterBase read FVisibleTextFilter;
  end;

  { TdxParagraphCharacterIterator }

  TdxParagraphCharacterIterator = class(TdxParagraphIteratorBase)
  public
    function CreatePosition: TdxFormatterPosition; override;
  end;

  TdxIteratorClass = class of TdxParagraphIteratorBase;

  TdxParagraphCharacterFormatter = class;

  { TdxFormatterStateBase }

  TdxFormatterStateBase = class(TcxIUnknownObject)
  protected
    const MaxTextLength = 3000;
  protected
    function GetIteratorClass: TdxIteratorClass; virtual; abstract;

    function GetMeasurer: TdxBoxMeasurer; virtual; abstract;
    function GetFormattingComplete: Boolean; virtual; abstract;
    function GetIterator: TdxParagraphIteratorBase; virtual; abstract;

    function IsTerminatorChar(ACh: Char): Boolean; virtual; abstract;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; virtual; abstract;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); virtual; abstract;
    function FinishParagraph: TdxCompleteFormattingResult; virtual; abstract;
    function FinishSection: TdxCompleteFormattingResult; virtual; abstract;

    procedure FinishParagraphCore(AIterator: TdxParagraphIteratorBase); virtual; abstract;

    function ContinueFormatByCharacter(var ABoxInfo: TdxBoxInfo; ALayoutDependentTextBox: TdxLayoutDependentTextBox): TdxStateContinueFormatResult; virtual;
    function MaxTextLengthExceeded(ABox: TdxBoxInfo): Boolean; virtual;
  public
    property Measurer: TdxBoxMeasurer read GetMeasurer;
    property FormattingComplete: Boolean read GetFormattingComplete;
    property Iterator: TdxParagraphIteratorBase read GetIterator;
  end;


  { TdxCharacterFormatterStateBase }

  TdxCharacterFormatterStateBase = class abstract (TdxFormatterStateBase)
  private
    FFormatter: TdxParagraphCharacterFormatter;
  protected
    function GetDashState: TdxCharacterFormatterState; virtual;
    function GetIteratorClass: TdxIteratorClass; override;
    function GetIterator: TdxParagraphIteratorBase; override;
    function GetFormattingComplete: Boolean; override;
    function GetMeasurer: TdxBoxMeasurer; override;
    function GetType: TdxCharacterFormatterState; virtual; abstract;
    function ContinueFormat: Boolean; virtual;
    procedure FinishParagraphCore(AIterator: TdxParagraphIteratorBase); override;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
    procedure ChangeState(AStateType: TdxCharacterFormatterState); virtual;

    function GetParagraph: TdxParagraph; virtual;
    function GetCharacterFormatterState: TdxCharacterFormatterState;
  public
    constructor Create(AFormatter: TdxParagraphCharacterFormatter);

    procedure AppendBoxCore(AType: TdxBoxClass; var ABoxInfo: TdxBoxInfo);
    procedure SwitchToNextState;
    procedure AddBox(AType: TdxBoxClass; var ABoxInfo: TdxBoxInfo; AMeasured: Boolean);
    function CreateParagraphMarkBoxInfo: TdxBoxInfo;
    function CreateSectionMarkBoxInfo: TdxBoxInfo;
    function GetNextState: TdxCharacterFormatterState; virtual;
    function IsParagraphMarkRun(ARun: TdxTextRunBase): Boolean;
    function IsInlineObjectRun(ARun: TdxTextRunBase): Boolean;
    function IsSeparatorRun(ARun: TdxTextRunBase): Boolean;
    function IsFloatingObjectRun(ARun: TdxTextRunBase): Boolean;
    function IsLayoutDependentTextRun(ARun: TdxTextRunBase): Boolean;

    property DashState: TdxCharacterFormatterState read GetDashState;
    property Paragraph: TdxParagraph read GetParagraph;
    property State: TdxCharacterFormatterState read GetCharacterFormatterState;
    property &Type: TdxCharacterFormatterState read GetType;
    property Formatter: TdxParagraphCharacterFormatter read FFormatter;
  end;
  TdxCharacterFormatterStateBaseClass = class of TdxCharacterFormatterStateBase;

  { TdxCharacterFormatterStartState }

  TdxCharacterFormatterStartState = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function ContinueFormat: Boolean; override;
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
  end;

  { TdxCharacterFormatterSpacesState }

  TdxCharacterFormatterSpacesState = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
  end;

  { TdxCharacterFormatterTextState }

  TdxCharacterFormatterTextState = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
    function MaxTextLengthExceeded(ABox: TdxBoxInfo): Boolean; override;
  end;

  { TdxCharacterFormatterDashState }

  TdxCharacterFormatterDashState = class(TdxCharacterFormatterStateBase)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
  end;

  { TdxCharacterFormatterFirstDashState }

  TdxCharacterFormatterFirstDashState = class  sealed  (TdxCharacterFormatterDashState)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function GetDashState: TdxCharacterFormatterState; override;
  end;

  { TdxCharacterFormatterLayoutDependentTextState }

  TdxCharacterFormatterLayoutDependentTextState = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
  end;

  { TdxCharacterFormatterInlineObjectState }

  TdxCharacterFormatterInlineObjectState = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
  end;

  { TdxCharacterFormatterSeparatorState }

  TdxCharacterFormatterSeparatorState = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
  end;

  { TdxCharacterFormatterFloatingObjectState }

  TdxCharacterFormatterFloatingObjectState = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
  end;

  { TdxCharacterFormatterTabState }

  TdxCharacterFormatterTabState = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
  end;

  { TdxCharacterFormatterLineBreak }

  TdxCharacterFormatterLineBreak = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
  end;

  { TdxCharacterFormatterPageBreak }

  TdxCharacterFormatterPageBreak = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
  end;

  { TdxCharacterFormatterColumnBreak }

  TdxCharacterFormatterColumnBreak = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function GetType: TdxCharacterFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
  end;

  { TdxCharacterFormatterFinalState }

  TdxCharacterFormatterFinalState = class  sealed  (TdxCharacterFormatterStateBase)
  protected
    function GetFormattingComplete: Boolean; override;
    function GetType: TdxCharacterFormatterState; override;
    function ContinueFormat: Boolean; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
  end;


  { TdxBoxFormatterStateBase }

  TdxBoxFormatterStateBase = class abstract (TdxFormatterStateBase)
  private
    FFormatter: TdxParagraphBoxFormatter;
    FPreviousState: TdxBoxFormatterStateBase;
    FStateAfterFinalizePage: TdxParagraphBoxFormatterState;
    FStateAfterFinalizeColumn: TdxParagraphBoxFormatterState;
    function GetCurrentRow: TdxRow;
    function GetForceFormattingComplete: Boolean;
    procedure SetForceFormattingComplete(const Value: Boolean);
    function GetRowsController: TdxRowsController;
  protected
    function CanUseBox: Boolean; virtual;
    function GetIteratorClass: TdxIteratorClass; override;

    function FinishParagraphOrSection: TdxCompleteFormattingResult; virtual;
    procedure FinishParagraphCore(AIterator: TdxParagraphIteratorBase); override;
    function GetIterator: TdxParagraphIteratorBase; override;
    function GetIterator_: TdxParagraphBoxIterator;
    function GetFormattingComplete: Boolean; override;
    function GetMeasurer: TdxBoxMeasurer; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; virtual;
    function GetType: TdxParagraphBoxFormatterState; virtual; abstract;

    procedure AddSuccess(ABoxInfo: TdxBoxInfo); virtual; abstract;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; virtual; abstract;
    function ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; virtual; abstract;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; virtual; abstract;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
    function FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; virtual;
    function FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; virtual;
    function FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; virtual;
    function CanAddBoxWithoutHyphen(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; virtual;
    procedure ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo); virtual;
    procedure ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo); virtual;
    procedure ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo); virtual;
    procedure ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo); virtual;
    procedure FinalizeParagraph(ABoxInfo: TdxBoxInfo);
    procedure FinalizeSection(ABoxInfo: TdxBoxInfo);
    procedure FinalizeLine(ABoxInfo: TdxBoxInfo);
    function FinalizePage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
    function FinalizeColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
    procedure AddNumberingListBox; virtual;
    function CanInsertNumberingListBox: Boolean; virtual;
    function CreateParagraphMarkBoxInfo: TdxBoxInfo;
    function CreateSectionMarkBoxInfo: TdxBoxInfo; virtual;
    function CreateLineBreakBoxInfo: TdxBoxInfo;
    function CreatePageBreakBoxInfo: TdxBoxInfo;
    function CreateColumnBreakBoxInfo: TdxBoxInfo;
    function BinarySearchFittedBox(ABoxInfo: TdxBoxInfo): Integer; overload; virtual;
    function BinarySearchFittedBox(ABoxInfo: TdxBoxInfo; ALow, AHi: Integer): Integer; overload; virtual;
    procedure SetCurrentRowHeightToLastBoxHeight;
    procedure EndRow;
    procedure CalculateAndMeasureLayoutDependentTextBox(ALayoutDependentTextBox: TdxLayoutDependentTextBox; ABoxInfo: TdxBoxInfo); virtual;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
    procedure MeasureBoxContentCore(ABoxInfo: TdxBoxInfo);
    function MaxTextLengthExceeded(ABox: TdxBoxInfo): Boolean; override;
    procedure ChangeState(AStateType: TdxParagraphBoxFormatterState); virtual;
    procedure AddTextBox(ABoxInfo: TdxBoxInfo);
    function GetAddBoxResultColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult): TdxAddBoxResult; virtual;
    function CanAddBoxCore(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; virtual;
    function CanAddBoxWithHyphenCore(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
    function GetContinueFormatResult(AResult: TdxCompleteFormattingResult): TdxStateContinueFormatResult;
    function SplitBox(ABoxInfo: TdxBoxInfo): TdxSplitBoxResult;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
  public
    constructor Create(AFormatter: TdxParagraphBoxFormatter);

    function ContinueFormat: TdxStateContinueFormatResult; virtual;
    function AddExistedBox(ABoxInfo: TdxBoxInfo; AIterator: TdxParagraphBoxIterator): TdxStateContinueFormatResult;
    function ObtainAddBoxResult(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
    function DispatchAddBoxResult(ABoxInfo: TdxBoxInfo; AResult: TdxAddBoxResult): TdxStateContinueFormatResult;
    function GetCanFitCurrentRowToColumn(AAddBoxResult: TdxAddBoxResult): TdxCanFitCurrentRowToColumnResult;
    function CalcBoxSizeWithHyphen(ABoxInfo: TdxBoxInfo): TSize;
    function IsCurrentRowEmpty: Boolean;
    function AdjustEndPositionToFit(ABoxInfo: TdxBoxInfo): TdxAdjustEndPositionResult;

    property Formatter: TdxParagraphBoxFormatter read FFormatter;
    property CurrentRow: TdxRow read GetCurrentRow;
    property ForceFormattingComplete: Boolean read GetForceFormattingComplete write SetForceFormattingComplete;
    property RowsController: TdxRowsController read GetRowsController;
    property StateAfterFloatingObject: TdxParagraphBoxFormatterState read GetStateAfterFloatingObject;
    property &Type: TdxParagraphBoxFormatterState read GetType;
    property Iterator: TdxParagraphBoxIterator read GetIterator_;
    property PreviousState: TdxBoxFormatterStateBase read FPreviousState write FPreviousState;
    property StateAfterFinalizePage: TdxParagraphBoxFormatterState read FStateAfterFinalizePage write FStateAfterFinalizePage;
    property StateAfterFinalizeColumn: TdxParagraphBoxFormatterState read FStateAfterFinalizeColumn write FStateAfterFinalizeColumn;
  end;
  TdxBoxFormatterStateBaseClass = class of TdxBoxFormatterStateBase;

  { ISupportsChangeStateManually }

  IdxSupportsChangeStateManually = interface
  ['{F5605619-439A-4000-9907-D3F8D025E241}']
    procedure ChangeStateManuallyIfNeeded(AIteratorResult: TdxParagraphIteratorResult);
  end;

  { TdxStateRowTextSplit }

  TdxStateRowTextSplit = class(TdxBoxFormatterStateBase, IdxSupportsChangeStateManually)
  protected
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function HandleUnsuccessfulSplitting: TdxCompleteFormattingResult; virtual;
    function ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    procedure AddSuccess(ABoxInfo: TdxBoxInfo); override;
    function ShouldChangeStateManually(AIteratorResult: TdxParagraphIteratorResult; ACurrentCharacter: Char): Boolean;
    function CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState; virtual;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
    function FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
  public
    procedure ChangeStateManuallyIfNeeded(AIteratorResult: TdxParagraphIteratorResult);
    function NextSplitState: TdxParagraphBoxFormatterState; virtual;
  end;

  { TdxStateRowTextSplitAfterFloatingObject }

  TdxStateRowTextSplitAfterFloatingObject = class  sealed  (TdxStateRowTextSplit)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  end;

  { TdxStateRowDashSplit }

  TdxStateRowDashSplit = class(TdxStateRowTextSplit)
  protected
    function CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function HandleUnsuccessfulSplitting: TdxCompleteFormattingResult; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
  public
    function NextSplitState: TdxParagraphBoxFormatterState; override;
  end;

  { TdxStateRowDashSplitAfterFloatingObject }

  TdxStateRowDashSplitAfterFloatingObject = class  sealed  (TdxStateRowDashSplit)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  end;

  { TdxStateRowEmptyHyphenation }

  TdxStateRowEmptyHyphenation = class(TdxBoxFormatterStateBase)
  private
    FSwitchToRowEmpty: Boolean;
  protected
    procedure AddSuccess(ABoxInfo: TdxBoxInfo); override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function IsHyphenationOfWordComplete: Boolean;
    function ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    function CanAddBoxWithoutHyphen(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    function GetIterator: TdxParagraphIteratorBase; override;
    function GetNextState: TdxParagraphBoxFormatterState; virtual;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
  public
    function CalcFinalState(ACurrentChar: Char): TdxParagraphBoxFormatterState;

    property NextState: TdxParagraphBoxFormatterState read GetNextState;
  end;

  { TdxStateRowEmptyHyphenationAfterFloatingObject }

  TdxStateRowEmptyHyphenationAfterFloatingObject = class  sealed  (TdxStateRowEmptyHyphenation)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  end;

  { TdxStateRowTextHyphenation }

  TdxStateRowTextHyphenation = class  sealed  (TdxStateRowEmptyHyphenation)
  protected
    function GetNextState: TdxParagraphBoxFormatterState; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    procedure AddSuccess(ABoxInfo: TdxBoxInfo); override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    function CanAddBoxWithoutHyphen(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    procedure InsertHyphenBox;
    function CreateHyphenBoxInfo: TdxBoxInfo;
  end;

  { TdxStateRowWithTextOnlyBase }

  TdxStateRowWithTextOnlyBase = class(TdxBoxFormatterStateBase, IdxSupportsChangeStateManually)
  protected
    function CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState; virtual;
    function GetHyphenationState: TdxParagraphBoxFormatterState; virtual; abstract;
    function GetNoHyphenationNextState: TdxParagraphBoxFormatterState; virtual; abstract;
    function CanUseBox: Boolean; override;
    function GetDashAfterTextState: TdxParagraphBoxFormatterState; virtual;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
  public
    procedure AddSuccess(ABoxInfo: TdxBoxInfo); override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
    function FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function ShouldChangeStateManually(ACurrentCharacter: Char): Boolean;
    procedure ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ChangeStateManuallyIfNeeded(AIteratorResult: TdxParagraphIteratorResult);

    property DashAfterTextState: TdxParagraphBoxFormatterState read GetDashAfterTextState;
    property HyphenationState: TdxParagraphBoxFormatterState read GetHyphenationState;
    property NoHyphenationNextState: TdxParagraphBoxFormatterState read GetNoHyphenationNextState;
  end;

  { TdxStateRowWithTextOnly }

  TdxStateRowWithTextOnly = class(TdxStateRowWithTextOnlyBase)
  protected
    function GetHyphenationState: TdxParagraphBoxFormatterState; override;
    function GetNoHyphenationNextState: TdxParagraphBoxFormatterState; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    function GetDashAfterTextState: TdxParagraphBoxFormatterState; override;
  end;

  { TdxStateRowWithInlineObjectOnly }

  TdxStateRowWithInlineObjectOnly = class(TdxStateRowWithTextOnly)
  protected
    function CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState; override;
    function CanUseBox: Boolean; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
  public
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
  end;

  { TdxStateRowText }

  TdxStateRowText = class(TdxStateRowWithTextOnly)
  protected
    function GetDashAfterTextState: TdxParagraphBoxFormatterState; override;
    function GetHyphenationState: TdxParagraphBoxFormatterState; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
  public
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function WrapLine: TdxCompleteFormattingResult;
  end;

  { TdxStateRowDashAfterText }

  TdxStateRowDashAfterText = class  sealed  (TdxStateRowText)
  protected
    function CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
  end;

  { TdxStateRowWithDashAfterTextOnly }

  TdxStateRowWithDashAfterTextOnly = class  sealed  (TdxStateRowWithTextOnly)
  protected
    function CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
  public
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
  end;

  { TdxStateRowDash }

  TdxStateRowDash = class  sealed  (TdxStateRowText)
  protected
    function CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
  public
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
  end;

  { TdxStateRowWithDashOnly }

  TdxStateRowWithDashOnly = class  sealed  (TdxStateRowWithTextOnly)
  protected
    function CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
  public
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
  end;

  { TdxStateRowSpaces }

  TdxStateRowSpaces = class(TdxBoxFormatterStateBase, IdxSupportsChangeStateManually)
  protected
    function CanUseBox: Boolean; override;
    function GetType: TdxParagraphBoxFormatterState; override;
  public
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    procedure AddSuccess(ABoxInfo: TdxBoxInfo); override;
    function CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState; virtual;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    procedure ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    function ShouldChangeStateManually(ACurrentCharacter: Char): Boolean;
    procedure ChangeStateManuallyIfNeeded(AIteratorResult: TdxParagraphIteratorResult);
  end;

  { TdxStateRowWithSpacesOnly }

  TdxStateRowWithSpacesOnly = class  sealed  (TdxStateRowSpaces)
  strict private type
    TFinalizeEvent = procedure (ABoxInfo: TdxBoxInfo) of object;
    function InternalFinishParagraphCore(ABoxInfo: TdxBoxInfo; AFinalizeHandler: TFinalizeEvent): TdxCompleteFormattingResult;
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  public
    procedure ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo); override;
    function CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState; override;
    function FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
  end;

  { TdxStateRowEmptyBase }

  TdxStateRowEmptyBase = class(TdxBoxFormatterStateBase)
  strict private type
    TFinalizeEvent = procedure (ABoxInfo: TdxBoxInfo) of object;
    function InternalFinishParagraphCore(ABoxInfo: TdxBoxInfo; AFinalizeHandler: TFinalizeEvent): TdxCompleteFormattingResult;
  protected
    function CanFitCurrentRowToColumn(ABoxInfo: TdxBoxInfo): TdxCanFitCurrentRowToColumnResult; virtual;
    procedure ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    function FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
  public
    function ContinueFormat: TdxStateContinueFormatResult; override;
  end;

  { TdxStateRowEmpty }

  TdxStateRowEmpty = class(TdxStateRowEmptyBase)
  protected
    procedure AddSuccess(ABoxInfo: TdxBoxInfo); override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
  end;

  { TdxStateRowEmptyAfterFloatingObject }

  TdxStateRowEmptyAfterFloatingObject = class  sealed  (TdxStateRowEmpty)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  end;

  { TdxStateParagraphStart }

  TdxStateParagraphStart = class(TdxStateRowEmpty)
  protected
    function CanFitCurrentRowToColumn(ABoxInfo: TdxBoxInfo): TdxCanFitCurrentRowToColumnResult; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    procedure InsertNumberingListBox; virtual;
  public
    function ContinueFormat: TdxStateContinueFormatResult; override;
  end;

 { TdxStateParagraphStartAfterBreak }

  TdxStateParagraphStartAfterBreak = class(TdxStateParagraphStart)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  public
    function ContinueFormat: TdxStateContinueFormatResult; override;
  end;

  { TdxStateParagraphStartAfterFloatingObject }

  TdxStateParagraphStartAfterFloatingObject = class  sealed  (TdxStateParagraphStart)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  end;

  { TdxStateParagraphStartFromTheMiddle }

  TdxStateParagraphStartFromTheMiddle = class(TdxStateParagraphStart)
  protected
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    procedure InsertNumberingListBox; override;
  end;

  { TdxStateParagraphStartFromTheMiddleAfterFloatingObject }

  TdxStateParagraphStartFromTheMiddleAfterFloatingObject = class  sealed  (TdxStateParagraphStartFromTheMiddle)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
  end;

  { TdxStateRowWithTextOnlyAfterFirstLeadingTab}

  TdxStateRowWithTextOnlyAfterFirstLeadingTab = class  sealed  (TdxStateRowWithTextOnly)
  protected
    function GetHyphenationState: TdxParagraphBoxFormatterState; override;
    function GetNoHyphenationNextState: TdxParagraphBoxFormatterState; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
  end;

  { TdxStateRowTabBase }

  TdxStateRowTabBase = class(TdxBoxFormatterStateBase, IdxSupportsChangeStateManually)
  private
    FCurrentTab: TdxTabInfo;
  protected
    procedure AddSuccess(ABoxInfo: TdxBoxInfo); override;
    function ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    procedure SwitchToNextState; virtual; abstract;
    procedure MeasureBoxContent(ABoxInfo: TdxBoxInfo); override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function CanUseBox: Boolean; override;
    function CurrentTabPosition: Integer;

    property CurrentTab: TdxTabInfo read FCurrentTab write FCurrentTab;
  public
    procedure ChangeStateManuallyIfNeeded(AIteratorResult: TdxParagraphIteratorResult);
  end;

  { TdxStateRowTab }

  TdxStateRowTab = class  sealed  (TdxStateRowTabBase)
  protected
    procedure SwitchToNextState; override;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
    function FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
  end;

  { TdxStateRowLeadingTab }

  TdxStateRowLeadingTab = class(TdxStateRowTabBase)
  strict private type
    TFinalizeEvent = procedure (ABoxInfo: TdxBoxInfo) of object;
    function InternalFinishParagraphCore(ABoxInfo: TdxBoxInfo; AFinalizeHandler: TFinalizeEvent): TdxCompleteFormattingResult;
  protected
    procedure SwitchToNextState; override;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
    procedure ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo); override;
    function FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    procedure ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    procedure ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo); override;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
  end;

  { TdxStateRowFirstLeadingTab }

  TdxStateRowFirstLeadingTab = class  sealed  (TdxStateRowLeadingTab)
  protected
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    procedure SwitchToNextState; override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
  end;

  { TdxStateRowTextSplitAfterFirstLeadingTab }

  TdxStateRowTextSplitAfterFirstLeadingTab = class  sealed  (TdxStateRowTextSplit)
  private
    function CompleteRowProcessing: TdxCompleteFormattingResult;
  protected
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function HandleUnsuccessfulSplitting: TdxCompleteFormattingResult; override;
  end;

  { TdxStateRowTextHyphenationFirstSyllable }

  TdxStateRowTextHyphenationFirstSyllable = class(TdxStateRowEmptyHyphenation)
  protected
    function GetNextState: TdxParagraphBoxFormatterState; override;
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
  end;

  { TdxStateRowTextHyphenationFirstSyllableAfterFirstLeadingTab }

  TdxStateRowTextHyphenationFirstSyllableAfterFirstLeadingTab = class  sealed  (TdxStateRowTextHyphenationFirstSyllable)
  private
    function CompleteRowProcessing: TdxCompleteFormattingResult;
  protected
    function GetStateAfterFloatingObject: TdxParagraphBoxFormatterState; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
  end;

  { TdxStateFloatingObject }

  TdxStateFloatingObject = class  sealed  (TdxBoxFormatterStateBase)
  private
    FPreviousState: TdxBoxFormatterStateBase;
  protected
    function CanUseBox: Boolean; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    procedure SwitchToNextState; virtual;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    procedure AddSuccess(ABoxInfo: TdxBoxInfo); override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishParagraphOrSection: TdxCompleteFormattingResult; override;
  public
    constructor Create(AFormatter: TdxParagraphBoxFormatter; APreviousState: TdxBoxFormatterStateBase);

    function ContinueFormat: TdxStateContinueFormatResult; override;
  end;

  { TdxStateParagraphFrame }

  TdxStateParagraphFrame = class  sealed  (TdxBoxFormatterStateBase)
  strict private
    FPreviousState: TdxBoxFormatterStateBase;
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
    function CanUseBox: Boolean; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    procedure SwitchToNextState; virtual;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
    procedure AddSuccess(ABoxInfo: TdxBoxInfo); override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishParagraphOrSection: TdxCompleteFormattingResult; override;
    function CanAddBoxCore(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
  public
    constructor Create(AFormatter: TdxParagraphBoxFormatter; APreviousState: TdxBoxFormatterStateBase);
    function ContinueFormat: TdxStateContinueFormatResult; override;
    function FormatParagraphFrame: TdxStateContinueFormatResult;
  end;

  { TdxFormatterStartEndState }

  TdxFormatterStartEndState = class(TdxBoxFormatterStateBase)
  public
    procedure AddSuccess(ABoxInfo: TdxBoxInfo); override;
    function HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
    function AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult; override;
    function CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult; override;
  end;

  { TdxStateContinueFormattingFromParagraph }

  TdxStateContinueFormattingFromParagraph = class  sealed  (TdxFormatterStartEndState)
  private
    FParagraphIndex: TdxParagraphIndex;
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  public
    constructor Create(AFormatter: TdxParagraphBoxFormatter; AParagraphIndex: TdxParagraphIndex);

    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
  end;

  { TdxStateSectionBreakAfterParagraphMark }

  TdxStateSectionBreakAfterParagraphMark = class  sealed  (TdxFormatterStartEndState)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
    function FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function CreateSectionMarkBoxInfo: TdxBoxInfo; override;
  public
    function ContinueFormat: TdxStateContinueFormatResult; override;
  end;

  { TdxStateRowBreakBase }

  TdxStateRowBreakBase = class(TdxFormatterStartEndState)
  public
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
    function FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function IsTerminatorChar(ACh: Char): Boolean; override;
  end;

  { TdxStateRowPageBreak }

  TdxStateRowPageBreak = class(TdxStateRowBreakBase)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  public
    function ContinueFormat: TdxStateContinueFormatResult; override;
  end;

  { TdxStateRowPageBreakAtParagraphStart }

  TdxStateRowPageBreakAtParagraphStart = class(TdxStateRowPageBreak)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  public
    function ContinueFormat: TdxStateContinueFormatResult; override;
  end;

  { TdxStateRowColumnBreak }

  TdxStateRowColumnBreak = class (TdxStateRowBreakBase)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  public
    function ContinueFormat: TdxStateContinueFormatResult; override;
  end;

  { TdxStateRowColumnBreakAtParagraphStart }

  TdxStateRowColumnBreakAtParagraphStart = class(TdxStateRowColumnBreak)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  public
    function ContinueFormat: TdxStateContinueFormatResult; override;
  end;

  { TdxStateRowLineBreak }

  TdxStateRowLineBreak = class  sealed  (TdxStateRowBreakBase)
  protected
    function GetType: TdxParagraphBoxFormatterState; override;
  public
    function ContinueFormat: TdxStateContinueFormatResult; override;
  end;

  { TdxStateFinal }

  TdxStateFinal = class  sealed  (TdxFormatterStartEndState)
  protected
    function GetFormattingComplete: Boolean; override;
    function GetType: TdxParagraphBoxFormatterState; override;
    function FinishParagraph: TdxCompleteFormattingResult; override;
    function FinishSection: TdxCompleteFormattingResult; override;
    function FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
    function FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult; override;
  end;

  { TdxParagraphFormatterBase }

  TdxParagraphFormatterBase<TState: TdxFormatterStateBase> = class
  private
    FPieceTable: TdxPieceTable;
    FMeasurer: TdxBoxMeasurer;
    FIterator: TdxParagraphIteratorBase;
    FState: TState;
    function GetDocumentModel: TdxDocumentModel;
    procedure SetIterator(const Value: TdxParagraphIteratorBase);
  protected
    procedure CreateStates; virtual;
    procedure DestroyStates; virtual;
    function GetIteratorClass: TdxIteratorClass; virtual; abstract;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Measurer: TdxBoxMeasurer read FMeasurer;
  public
    constructor Create(APieceTable: TdxPieceTable; AMeasurer: TdxBoxMeasurer);
    destructor Destroy; override;

    procedure BeginParagraph(ABeginFromParagraphStart, AParagraphFrameStarted: Boolean); virtual; abstract;
    class function GetSpaceBoxTemplate(ABoxInfo: TdxBoxInfo): TdxBoxClass;
    procedure OnNewMeasurementAndDrawingStrategyChanged(AMeasurer: TdxBoxMeasurer); virtual;

    property Iterator: TdxParagraphIteratorBase read FIterator write SetIterator;
    property PieceTable: TdxPieceTable read FPieceTable;
    property State: TState read FState write FState;
  end;

  { TdxParagraphBoxIterator }

  TdxParagraphBoxIterator = class(TdxParagraphIteratorBase)
  private
    FBoxIndex: Integer;
    function GetCurrentBox: TdxBox;
  public
    function CreatePosition: TdxFormatterPosition; override;

    procedure NextBox;
    function Next: TdxParagraphIteratorResult; override;
    procedure SetPosition(const APos: TdxFormatterPosition); override;
    function GetPreviousVisibleRunPosition: TdxFormatterPosition; override;
    function GetPreviousOffsetPosition: TdxFormatterPosition; override;
    function GetCurrentPosition: TdxFormatterPosition; override;
    procedure NextBoxCore;
    procedure SetNextPosition(const APrevPosition: TdxFormatterPosition);

    property CurrentBox: TdxBox read GetCurrentBox;
    property BoxIndex: Integer read FBoxIndex write FBoxIndex;
  end;

  { TdxSyllableBoxIterator }

  TdxFormatterPositionCollection = class(TList<TdxFormatterPosition>);

  TdxSyllableBoxIterator = class(TdxParagraphBoxIterator)
  private
    FIterator: TdxParagraphBoxIterator;
    FHyphenationService: IdxHyphenationService;
    FHyphenPositions: TdxFormatterPositionCollection;
    FHyphenPos: TdxFormatterPosition;
    FHyphenPosIndex: Integer;
    FHyphenAtCurrentPosition: Boolean;
    FEndPos: TdxFormatterPosition;
    function GetHyphenChar: Char;
  protected
    function GetCurrentChar: Char; override;
    function GetIsEnd: Boolean; override;
  public
    constructor Create(AIterator: TdxParagraphBoxIterator; const AHyphenationService: IdxHyphenationService );
    destructor Destroy; override;

    procedure SetInvalidHyphenPos;
    procedure SetPosition(ARunIndex: TdxRunIndex; AOffset: Integer); override;
    procedure SetPosition(const APos: TdxFormatterPosition); override;
    function GetCurrentWord: string;
    procedure HyphenateCurrentWord; virtual;

    class function IsEndOfWord(ACh: Char): Boolean; static;
    class function IsBreak(ACh: Char): Boolean; static;
    class function IsWhiteSpace(ACh: Char): Boolean; static;

    function Next: TdxParagraphIteratorResult; override;

    property Hyphenator: IdxHyphenationService read FHyphenationService;
    property Iterator: TdxParagraphBoxIterator read FIterator;
    property HyphenPositions: TdxFormatterPositionCollection read FHyphenPositions;
    property HyphenChar: Char read GetHyphenChar;
  end;

  { TdxParagraphBoxFormatter }

  TdxParagraphBoxFormatter = class  sealed  (TdxParagraphFormatterBase<TdxBoxFormatterStateBase>)
  public const
    InvalidPosition: TdxFormatterPosition = (BoxIndex: -1; Offset: -1; RunIndex: -1);
  private
    FPreviousSyllableIterator: TdxParagraphBoxIterator;
    FSyllableIterator: TdxParagraphBoxIterator;
    FParagraphStartPos: TdxFormatterPosition;
    FLastRestartDueToFloatingObjectParagraphStartPos: TdxFormatterPosition;
    FRowsController: TdxRowsController;
    FWordStartPos: TdxFormatterPosition;
    FRowStartPos: TdxFormatterPosition;
    FSuppressHyphenation: Boolean;
    FMaxHeight: Integer;
    FForceFormattingComplete: Boolean;
    FPageNumberSource: TdxPage;
    FStates: array[TdxParagraphBoxFormatterState] of TdxBoxFormatterStateBase;
    FHasDeferredNumberingListBox: Boolean;
    FHyphenationService: IdxHyphenationService;
    FParagraphStartRowCount: Integer;
    FLastTabStartPos: TdxFormatterPosition;
    FUnapprovedFloatingObjectsStartPos: TdxFormatterPosition;
    FHasUnapprovedFloatingObjects: Boolean;
    FTableState: TdxParagraphBoxFormatterTableStateBase;
    function GetActualParagraphFramePropertiesProperty: TdxMergedFrameProperties;
    function GetCurrentRow: TdxRow;
    function GetNewState(AStateType: TdxParagraphBoxFormatterState): TdxBoxFormatterStateBase; inline;
  protected
    procedure CreateStates; override;
    procedure DestroyStates; override;
    function GetIteratorClass: TdxIteratorClass; override;

    procedure ApproveFloatingObjects;
    procedure RollbackUnapprovedFloatingObjects;
    procedure StartFloatingObjects;
  public
    constructor Create(APieceTable: TdxPieceTable; AMeasurer: TdxBoxMeasurer; ARowsController: TdxRowsController);
    destructor Destroy; override;

    procedure ChangeStateContinueFromParagraph(AParagraphIndex: TdxParagraphIndex);
    procedure ChangeState(AStateType: TdxParagraphBoxFormatterState); virtual;
    function HasActualParagraphFrameProperties(AParagraph: TdxSimpleParagraph): Boolean; overload;
    function HasActualParagraphFrameProperties: Boolean; overload;
    function GetActualParagraphFrameProperties(AParagraph: TdxSimpleParagraph): TdxMergedFrameProperties; overload;
    function IsParagraphFrame(AProperties: TdxMergedFrameProperties): Boolean;
    function GetInitialStateType(ABeginFromParagraphStart: Boolean): TdxParagraphBoxFormatterState;
    procedure Initialize(ARowsController: TdxRowsController); virtual;
    procedure SubscribeRowsControllerEvents; virtual;
    procedure UnsubscribeRowsControllerEvents; virtual;
    procedure OnTableStart(ASender: TObject; AE: TdxEventArgs);
    function GetActiveIterator: TdxParagraphBoxIterator;
    procedure UpdateSyllableIterator;
    procedure ClearSyllableIterator;
    procedure BeginParagraphFormatting(AIterator: TdxParagraphBoxIterator; ABeginFromParagraphStart, AParagraphFrameStarted: Boolean);
    function FormatNextRow: TdxFormattingProcessResult;
    procedure EndParagraphFormatting;
    procedure BeginParagraph(ABeginFromParagraphStart, AParagraphFrameStarted: Boolean); override;
    function CanBreakPageBefore(AParagraph: TdxParagraph): Boolean;
    function ShouldPageBreakBeforeParagraph(AParagraph: TdxParagraph): Boolean; virtual;
    procedure EndParagraph;
    procedure StartNewRow;
    procedure StartNewTab;
    procedure StartNewWord;
    procedure RollbackToPositionAndClearLastRow(const APos: TdxFormatterPosition);
    function RollbackToParagraphStart(const APos: TdxFormatterPosition; AInitialRowCount: Integer): TdxCompleteFormattingResult;
    function RollbackToStartOfRowCore(ACanFit: TdxCanFitCurrentRowToColumnResult; ANextState: TdxParagraphBoxFormatterState): TdxCompleteFormattingResult;
    function RollbackToStartOfRowTable(ACanFit: TdxCanFitCurrentRowToColumnResult): TdxParagraphIndex;
    function RollbackToStartOfRow(ACanFit: TdxCanFitCurrentRowToColumnResult): TdxCompleteFormattingResult;
    procedure ResetLastTabPosition;
    procedure RollbackToLastTab; overload;
    procedure RollbackToLastTab(AIter: TdxParagraphBoxIterator); overload;
    procedure RollbackToStartOfWord; overload;
    procedure RollbackToStartOfWord(AIter: TdxParagraphBoxIterator); overload;

    property ActualParagraphFrameProperties: TdxMergedFrameProperties read GetActualParagraphFramePropertiesProperty;
    property SyllableIterator: TdxParagraphBoxIterator read FSyllableIterator;
    property CurrentRow: TdxRow read GetCurrentRow;
    property ParagraphStartPos: TdxFormatterPosition read FParagraphStartPos;
    property RowStartPos: TdxFormatterPosition read FRowStartPos;
    property WordStartPos: TdxFormatterPosition read FWordStartPos;
    property RowsController: TdxRowsController read FRowsController;
    property SuppressHyphenation: Boolean read FSuppressHyphenation;
    property MaxHeight: Integer read FMaxHeight write FMaxHeight;
    property ForceFormattingComplete: Boolean read FForceFormattingComplete write FForceFormattingComplete;
    property PageNumberSource: TdxPage read FPageNumberSource write FPageNumberSource;
    property HasDeferredNumberingListBox: Boolean read FHasDeferredNumberingListBox write FHasDeferredNumberingListBox;
  end;

  { TdxParagraphCharacterFormatter }

  TdxParagraphCharacterFormatter = class  sealed  (TdxParagraphFormatterBase<TdxCharacterFormatterStateBase>)
  private
    FStates: array[TdxCharacterFormatterState] of TdxCharacterFormatterStateBase;
    FTextBoxInfos: TdxBoxInfoList;
    FTextBoxes: TdxBoxList;
    function GetIterator: TdxParagraphCharacterIterator; inline;
  protected
    procedure CreateStates; override;
    procedure DestroyStates; override;
    procedure FormatParagraph(AIterator: TdxParagraphIteratorBase); virtual;
    function GetInitialStateType(ABeginFromParagraphStart: Boolean): TdxCharacterFormatterState;
    function GetIteratorClass: TdxIteratorClass; override;

    property Iterator: TdxParagraphCharacterIterator read GetIterator;
  public
    constructor Create(APieceTable: TdxPieceTable; AMeasurer: TdxBoxMeasurer);
    destructor Destroy; override;

    procedure ChangeState(AStateType: TdxCharacterFormatterState); virtual;
    procedure BeginParagraph(ABeginFromParagraphStart, AParagraphFrameStarted: Boolean); override;
    procedure FormatNumberingListBoxes; virtual;
    function CreateNumberingListBox(ASeparatorChar: Char; const APosition: TdxFormatterPosition): TdxNumberingListBox;
    function CreateSeparatorBox(ASeparator: Char; const APosition: TdxFormatterPosition): TdxBox;
    procedure Format(AIterator: TdxParagraphCharacterIterator); virtual;
    procedure AddTextBoxToQueue(ABox: TdxBox; ABoxInfo: TdxBoxInfo);
    procedure MeasureTextBoxes;
  end;


  { TdxCurrentHorizontalPositionController }

  TdxCurrentHorizontalPositionController = class
  private
    FRowsController: TdxRowsController;
    FCurrentHorizontalPosition: Integer;
  protected
    procedure IncrementCurrentHorizontalPosition(ADelta: Integer); virtual;
    procedure RollbackCurrentHorizontalPositionTo(AValue: Integer); virtual;
    procedure SetCurrentRowInitialHorizontalPosition; virtual;
    procedure OnCurrentRowHeightChanged(AKeepTextAreas: Boolean); virtual;
    function CalculateBoxBounds(ABoxInfo: TdxBoxInfo): TRect; virtual;
    function CanFitCurrentRowToColumn(ANewRowHeight: Integer): TdxCanFitCurrentRowToColumnResult; virtual;
    function CanFitBoxToCurrentRow(const ABoxSize: TSize): Boolean; virtual;
    function CanFitNumberingListBoxToCurrentRow(const ABoxSize: TSize): Boolean; virtual;
    function GetMaxBoxWidth: Integer; virtual;
    procedure SetCurrentHorizontalPosition(AValue: Integer); virtual;
    procedure OnCurrentRowFinished; virtual;
    function MoveCurrentRowDownToFitTable(ATableWidth, ATableTop: Integer): Integer; virtual;
    function CalculateFloatingObjectColumnBounds(ACurrentColumn: TdxColumn): TRect; virtual;

    property InnerCurrentHorizontalPosition: Integer read FCurrentHorizontalPosition write FCurrentHorizontalPosition;
  public
    constructor Create(ARowsController: TdxRowsController); overload;
    constructor Create(ARowsController: TdxRowsController; APosition: Integer); overload;
    function GetTextAreaForTable: TdxTextArea; virtual;

    property RowsController: TdxRowsController read FRowsController;
    property CurrentHorizontalPosition: Integer read FCurrentHorizontalPosition;
  end;

  { TdxFloatingObjectsCurrentHorizontalPositionController }

  TdxFloatingObjectsCurrentHorizontalPositionController = class(TdxCurrentHorizontalPositionController)
  public type
    TPositionInfo = record
    public
      Position: Integer;
      TextAreaIndex: Integer;
      constructor Create(APosition, ATextAreaIndex: Integer);
    end;
  private
    FTextAreas: TList<TdxTextArea>;
    FCurrentTextAreaIndex: Integer;
    FMaxRowHeight: Integer;
    FMinLeftArea: Integer;
    FMinArea: Integer;
    function AreTextAreasChanged(APrevTextAreas, ATextAreas: TList<TdxTextArea>): Boolean;
    function CanFitBoxToTextArea(ATextAreaIndex: Integer; const ABoxSize: TSize): Boolean;
    function DoCurrentRowHeightChanged(AHeight: Integer; const ACurrentRowBounds: TRect; AKeepTextAreas,
      AIgnoreFloatingObjects: Boolean): Boolean;
    function GetAvailableTextAreaInitialPosition(APreferedTextAreaIndex: Integer): TPositionInfo;
    function GetTextOffset(ATextAreaIndex: Integer): Integer;
    function GetInitialPosition(ATextAreaIndex: Integer): Integer; overload;
    function GetInitialPosition(ATextAreaIndex, ATextOffset: Integer): Integer; overload;
    function IsCurrentRowEmpty: Boolean;
    procedure RemoveShortAreas(AAreas: TList<TdxTextArea>; ALeft: Integer; ARight: Integer);
    procedure DoSetCurrentHorizontalPosition(const APosition: TPositionInfo);
    function UpdateCurrentTextAreaIndex: Boolean;
  protected
    procedure OnCurrentRowHeightChanged(AKeepTextAreas: Boolean); overload; override;
    procedure SetCurrentRowInitialHorizontalPosition; override;
    procedure IncrementCurrentHorizontalPosition(ADelta: Integer); override;
    procedure RollbackCurrentHorizontalPositionTo(AValue: Integer); override;
    procedure SetCurrentHorizontalPosition(AValue: Integer); override;
    function MoveCurrentRowDownToFitTable(ATableWidth, ATableTop: Integer): Integer; override;
    function CanFitCurrentRowToColumn(ANewRowHeight: Integer): TdxCanFitCurrentRowToColumnResult; override;
    function CanFitNumberingListBoxToCurrentRow(const ABoxSize: TSize): Boolean; override;
    function CanFitBoxToCurrentRow(const ABoxSize: TSize): Boolean; override;
    function CalculateBoxBounds(ABoxInfo: TdxBoxInfo): TRect; override;
    function GetMaxBoxWidth: Integer; override;
    procedure AppendRowBoxRange(ABoxRanges: TdxRowBoxRangeCollection; AFirstBoxIndex, ALastBoxIndex, ATextAreaIndex: Integer);
    procedure OnCurrentRowFinished; override;
    procedure TryCreateDefaultBoxRange(ACurrentRow: TdxRow);
    function ShouldBeMovedToNextTextArea(ABox: TdxBox): Boolean; virtual;
  public
    constructor Create(ARowsController: TdxRowsController); overload;
    constructor Create(ARowsController: TdxRowsController; APosition: Integer); overload;
    destructor Destroy; override;
    function CalculateTextAreas(AParagraphFrameItems: TdxParagraphFrameBoxList;
      AFloatingObjectItems: TdxFloatingObjectBoxList; const ABounds: TRect): TList<TdxTextArea>;
    function GetTextAreaForTable: TdxTextArea; override;
    function AdvanceHorizontalPositionToNextTextArea: Boolean;
  end;

  { TdxTabsController }

  TdxTabsController = class
  public const
    DefaultDecimalChars: array[0..1] of Char = ('.', ',');
  private
    FColumnLeft: Integer;
    FParagraphRight: Integer;
    FDocumentModel: TdxDocumentModel;
    FTabs: TdxTabFormattingInfo;
    FTabStack: TdxIntegerStack;
    FBoxes: TdxBoxCollection;
    FPieceTable: TdxPieceTable;
    FParagraph: TdxParagraph;
    FSingleDecimalTabInTable: Boolean;
    function IsDecimalSeparator(const ASource: string; AIndex: Integer): Boolean;
    function IsGroupSeparator(const ASource: string; AIndex: Integer): Boolean;
    function IsSubstringStarts(const ASource: string; AIndex: Integer; const ASubstring: string): Boolean;
    function TryFindDecimalSeparator(ABoxes: TdxBoxCollection; AFrom, ATo: Integer;
      out ABoxIndex: Integer; out AOffset: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearLastTabPosition;
    procedure UpdateLastTabPosition(ABoxesCount: Integer);
    procedure SaveLastTabPosition(APos: Integer);
    function CalcLastTabWidth(ARow: TdxRow; AFormatter: TdxParagraphBoxFormatter): Integer;

    class function CalculateLeaderCount(ABox: TdxTabSpaceBox; APieceTable: TdxPieceTable): Integer; static;
    class function GetTabLeaderCharacterWidth(ABox: TdxTabSpaceBox; APieceTable: TdxPieceTable): Integer; static;
    class procedure CacheLeader(ABox: TdxTabSpaceBox; AFormatter: TdxParagraphBoxFormatter); static;
    class function GetTabLeaderCharacter(ALeaderType: TdxTabLeaderType): Char; static;

    procedure BeginParagraph(AParagraph: TdxParagraph);
    procedure ObtainTabs;
    function GetNextTab(APos: Integer): TdxTabInfo;

    function CalcLastRightTabWidth(const ATab: TdxTabInfo; AStartIndex: Integer): Integer;
    function CalcLastCenterTabWidth(const ATab: TdxTabInfo; AStartIndex: Integer): Integer;
    function CalcLastDecimalTabWidth(const ATab: TdxTabInfo; AStartIndex: Integer; AFormatter: TdxParagraphBoxFormatter; ARowLeft: Integer): Integer;
    function AdjustAlignedTabWidth(const ATab: TdxTabInfo; ATabWidth, ALastNonSpaceBoxRight: Integer): Integer;
    function IsTabPositionBehindParagraphRight(const ATab: TdxTabInfo): Boolean;
    function FindDecimalPointPosition(ABoxes: TdxBoxCollection; AFrom, ATo: Integer; AFormatter: TdxParagraphBoxFormatter; ARowLeft: Integer): Integer;

    property ColumnLeft: Integer read FColumnLeft write FColumnLeft;
    property ParagraphRight: Integer read FParagraphRight write FParagraphRight;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property Tabs: TdxTabFormattingInfo read FTabs;
    property SingleDecimalTabInTable: Boolean read FSingleDecimalTabInTable write FSingleDecimalTabInTable;
  end;

  { TdxTextAreaCollectionEx }

  TdxTextAreaCollectionEx = class
  strict private
    FInnerList: TList<TdxTextArea>;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxTextArea;
  protected
    function AddCore(AInterval: TdxTextArea): Integer; overload;
    procedure RemoveCore(AToRemove: TList<TdxTextArea>);
    procedure AddCore(AToAdd: TList<TdxTextArea>); overload;
    procedure DontOwnInnerList;

    property InnerList: TList<TdxTextArea> read FInnerList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const AInterval: TdxTextArea);
    function Remove(const AInterval: TdxTextArea): Boolean;
    function Contains(const AInterval: TdxTextArea): Boolean;
    procedure Sort;

    property Items[Index: Integer]: TdxTextArea read GetItem;
    property Count: Integer read GetCount;
  end;

  { TdxParagraphFramesLayout }

  TdxParagraphFramesLayout = class
  strict private class var
    FHorizontalObjectComparer: TdxBoxBaseComparer;
    FVerticalObjectComparer: TdxBoxBaseComparer;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FItems: TdxParagraphFrameBoxList;
  public
    constructor Create;
    destructor Destroy; override;

    function ContainsParagraph(AParagraph: TdxParagraphBase): Boolean;
    function GetFloatingObject(AParagraph: TdxParagraphBase): TdxParagraphFrameBox;
    procedure AddParagraphFrameBox(AParagraphFrameBox: TdxParagraphFrameBox);
    function GetObjectsInRectangle(const ABounds: TRect): TdxParagraphFrameBoxList; overload;
    procedure GetObjectsInRectangle(AWhere: TdxParagraphFrameBoxList; ATo: TdxParagraphFrameBoxList; const ABounds: TRect); overload;
    procedure ProcessParagraphFrame(AParagraphFrame: TdxParagraphFrameBox; AProcessedObjects: TdxParagraphFrameBoxList; AResult: TdxTextAreaCollectionEx; const AInitialBounds: TRect);
    function FindLeftMostX(AProcessedObjects: TdxParagraphFrameBoxList; AInitialX: Integer; const ABounds: TRect): Integer;
    function FindRightMostX(AProcessedObjects: TdxParagraphFrameBoxList; AInitialX: Integer; const ABounds: TRect): Integer;
    procedure ClearParagraphFrames(ARunIndex: TdxRunIndex);
    procedure Clear;

    property Items: TdxParagraphFrameBoxList read FItems;
  end;

  { TdxRowsController }

  TdxRowsController = class
  strict private
    FSavedCurrentColumnBounds: TRect;
    FBoxes: TdxFastObjectList;
    FColumnController: TdxCustomColumnController;
    FColumns: TdxBoxList;
    FCurrentRow: TdxRow;
    FCurrentColumn: TdxColumn;
    FDefaultRowHeight: Integer;
    FCurrentRowIndent: Integer;
    FCurrentRowIndentAfterFloatingObject: Integer;
    FParagraphLeft: Integer;
    FSuppressHorizontalOverfull: Boolean;
    FTablesController: TdxTablesController;
    FFloatingObjectsLayout: TdxFloatingObjectsLayout;
    FParagraph: TdxParagraph;
    FInitialLineNumber: Integer;
    FRestartModelPosition: TdxDocumentModelPosition;
    FLineNumber: Integer;
    FLineNumberStep: Integer;
    FPieceTable: TdxPieceTable;
    FHorizontalPositionController: TdxCurrentHorizontalPositionController;
    FSupportsColumnAndPageBreaks: Boolean;
    FParagraphFramesLayout: TdxParagraphFramesLayout;
    FParagraphRight: Integer;
    FRegularRowIndent: Integer;
    FStartWordBoxIndex: Integer;
    FHeightBeforeWord: Integer;
    FLastRestartDueToFloatingObjectModelPosition: TdxDocumentModelPosition;
    FLineSpacingCalculator: TdxLineSpacingCalculatorBase;
    FMaxPictureHeight: Integer;
    FMaxAscentAndFree: Integer;
    FMaxAscentBeforeWord: Integer;
    FMaxDescent: Integer;
    FMaxDescentBeforeWord: Integer;
    FMaxPictureHeightBeforeWord: Integer;
    FRunIndexBeforeWord: TdxRunIndex;
    FCurrentRunIndex: TdxRunIndex;
    FRowProcessingFlagsBeforeWord: TdxRowProcessingFlags;
    FTabsController: TdxTabsController;
    FCurrentParagraphNumberingListBounds: TRect;
    FSpacingAfterPrevParagraph: Integer;
    FIsSpacingAfterPrevParagraphValid: Boolean;
    FInvisibleEmptyParagraphInCellAfterNestedTable: Boolean;
    FParagraphFirstRowOnPage: TdxRow;
    FParagraphFirstColumnOnPage: TdxColumn;

    FSuppressLineNumberingRecalculationForLastPage: Boolean;
    FMatchHorizontalTableIndentsToTextEdge: Boolean;
    FFrameParagraphIndex: TdxParagraphIndex;
    FForceFormattingComplete: Boolean;

    FOnBeginNextSectionFormatting: TdxEventHandler;
    FOnTableStarted: TdxEventHandler;

    function FindVisibleParagraph(AStart: TdxParagraphIndex): TdxParagraphIndex;
    function GetColumnSize(AAnchorBox: TdxFloatingObjectAnchorBox; AContent: TdxTextBoxFloatingObjectContent;
      const ATextBoxContent: TRect; AGetFinalSize: Boolean): TSize;
    function GetTableCell(AHitTestResult: TdxRichEditHitTestResult): TdxTableCell;
    function GetDocumentModel: TdxDocumentModel;
    function GetCurrentHorizontalPosition: Integer;
    function GetParagraphFrameColumnSize(AParagraphFrameBox: TdxParagraphFrameBox): TSize;
    procedure SetInitialLineNumber(const Value: Integer);
    procedure SetLineNumberStep(const Value: Integer);
    procedure SetCurrentRow(const Value: TdxRow);
    procedure SetCurrentColumn(const Value: TdxColumn);
    procedure ObtainParagraphFirstRowPosition(AParagraph: TdxParagraph);
    function CanFitParagraphFrame(ANewBoxInfo: TdxBoxInfo; AEmptyColumn: Boolean): Boolean;
    procedure InnerSetColumnController(const AValue: TdxCustomColumnController);
  protected
    function CalcRowHeightWithBox(ABoxInfo: TdxBoxInfo): Integer;
    function CalculateSpacingBefore: Integer; virtual;
    function CanAddSectionBreakToPrevRow: Boolean; virtual;
    function CanFitFloatingObject(AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox;
      ANewBoxInfo: TdxBoxInfo; AEmptyColumn: Boolean): TdxCanFitFloatingObjectResult;
    function CompleteCurrentColumnFormatting: TdxCompleteFormattingResult; virtual;
    function CreateCurrentHorizontalPosition: TdxCurrentHorizontalPositionController; overload; virtual;
    function CreateCurrentHorizontalPosition(APosition: Integer): TdxCurrentHorizontalPositionController; overload; virtual;
    procedure EnsureFloatingObjectBoxActualBounds(AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox; AObjectAnchorRun: TdxFloatingObjectAnchorRun; AFloatingObjectBox: TdxFloatingObjectBox);
    function GetContextualSpacingAfter(AParagraph: TdxParagraph): Integer; virtual;
    function ShouldApplySpacingBefore(AParagraph: TdxParagraph): Boolean;

    class function GetBounds(const ALocation: TPoint; AWidth: Integer; AHeight: Integer): TRect; static;
    class procedure SetActualBoundsForEmptyHeightFrameBox(AParagraphFrameBox: TdxParagraphFrameBox; AElementBoundsHeight: Integer); static;
    class function GetBoundsLocation(const ABounds: TRect; APositionOffset: Integer): TPoint; static;
    class procedure SetActualBoundsForEmptyWidthFrameBox(AParagraphFrameBox: TdxParagraphFrameBox;
      AAlignment: TdxParagraphFrameHorizontalPositionAlignment; AElementBoundsWidth: Integer); static;
    function GetMaxBoxWidth: Integer;
    function SnapsToModelUnits(AValue: Integer): Integer;
    procedure CorrectFloatingObjectTextBoxVerticalPosition(AFloatingObjectBox: TdxFloatingObjectBox; AAnchorBox: TdxFloatingObjectAnchorBox);

    property CurrentHorizontalPosition: Integer read  GetCurrentHorizontalPosition;
    property CurrentRowIndent: Integer read FCurrentRowIndent write FCurrentRowIndent;
    property RegularRowIndent: Integer read FRegularRowIndent write FRegularRowIndent;
    property DefaultRowHeight: Integer read FDefaultRowHeight;
    property ParagraphLeft: Integer read FParagraphLeft;
    property SuppressHorizontalOverfull: Boolean read FSuppressHorizontalOverfull write FSuppressHorizontalOverfull;
    property Paragraph: TdxParagraph read FParagraph;
    property InitialLineNumber: Integer read FInitialLineNumber write SetInitialLineNumber;
    property LastRestartDueToFloatingObjectModelPosition: TdxDocumentModelPosition read FLastRestartDueToFloatingObjectModelPosition write FLastRestartDueToFloatingObjectModelPosition;
    property LineNumber: Integer read FLineNumber write FLineNumber;
    property LineNumberStep: Integer read FLineNumberStep write SetLineNumberStep;
    property PieceTable: TdxPieceTable read FPieceTable;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property SupportsColumnAndPageBreaks: Boolean read FSupportsColumnAndPageBreaks write FSupportsColumnAndPageBreaks;
    property HorizontalPositionController: TdxCurrentHorizontalPositionController read FHorizontalPositionController write FHorizontalPositionController;

    property ForceFormattingComplete: Boolean read FForceFormattingComplete;
    property ParagraphFirstRowOnPage: TdxRow read FParagraphFirstRowOnPage write FParagraphFirstRowOnPage;
    property ParagraphFirstColumnOnPage: TdxColumn read FParagraphFirstColumnOnPage write FParagraphFirstColumnOnPage;
  public
    constructor Create(APieceTable: TdxPieceTable; const AColumnController: TdxCustomColumnController; AMatchHorizontalTableIndentsToTextEdge: Boolean);
    destructor Destroy; override;

    procedure RaiseBeginNextSectionFormatting(ASectionIndex: TdxSectionIndex); virtual;
    procedure RaiseTableStarted; virtual;
    procedure Reset(ASection: TdxSection; AKeepFloatingObjects: Boolean); virtual;
    procedure SetColumnController(ANewColumnController: TdxCustomColumnController);
    procedure ClearInvalidatedContentCore(AColumn: TdxColumn; const APos: TdxFormatterPosition; AParagraphIndex: TdxParagraphIndex);
    procedure ClearInvalidatedContentFromTheStartOfRowAtCurrentPage(AColumn: TdxColumn; const APos: TdxFormatterPosition; AParagraphIndex: TdxParagraphIndex); virtual;
    procedure ClearInvalidatedContent(AColumn: TdxColumn; const APos: TdxFormatterPosition; AParagraphIndex: TdxParagraphIndex); virtual;
    property FloatingObjectsLayout: TdxFloatingObjectsLayout read FFloatingObjectsLayout;
    procedure RecreateHorizontalPositionController; virtual;
    function GetTopLevelColumnController: TdxCustomColumnController;
    procedure BeginSectionFormatting(ASection: TdxSection); virtual;
    procedure RestartFormattingFromTheStartOfSection(ASection: TdxSection; AColumn: TdxColumn); virtual;
    procedure RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; AColumn: TdxColumn); virtual;
    procedure RestartFormattingFromTheStartOfRowAtCurrentPage(ASection: TdxSection; AColumn: TdxColumn); virtual;
    procedure CreateNewCurrentRowAfterRestart(ASection: TdxSection; AColumn: TdxColumn); virtual;
    procedure AssignCurrentRowLineNumber;
    function CreateTablesController: TdxTablesController; virtual;
    procedure StartNewWord; virtual;
    procedure AddSingleDecimalTabInTable; virtual;
    procedure AddTabBox(const ATab: TdxTabInfo; ABoxInfo: TdxBoxInfo); virtual;
    procedure ExpandLastTab(AFormatter: TdxParagraphBoxFormatter);
    function AddBox(ABoxType: TdxBoxClass; ABoxInfo: TdxBoxInfo): TdxBox; virtual;
    procedure AddNumberingListBox(ABox: TdxNumberingListBox; AProperties: TdxListLevelProperties;
      AFormatter: TdxParagraphBoxFormatter); virtual;
    function CalculateNumberingListBoxBounds(ABox: TdxNumberingListBox; AProperties: TdxListLevelProperties;
      AFormatter: TdxParagraphBoxFormatter): TRect;
    function CalculateNumberingListBoxBoundsCore(ABox: TdxNumberingListBox; AProperties: TdxListLevelProperties;
      AFormatter: TdxParagraphBoxFormatter): TRect;
    function GetNumberingListBoxSeparatorWidth(ABox: TdxNumberingListBoxWithSeparator; const ABounds: TRect;
      AProperties: TdxListLevelProperties; AFormatter: TdxParagraphBoxFormatter): Integer;
    function GetLegacyNumberingListBoxSeparatorWidth(ABox: TdxNumberingListBoxWithSeparator; const ABounds: TRect;
      AProperties: TdxListLevelProperties; AFormatter: TdxParagraphBoxFormatter): Integer;
    function GetActualTabInfo(const ATabInfo: TdxTabInfo; AProperties: TdxListLevelProperties): TdxTabInfo;
    function CalculateNumberingListBoxLeft(ABoxWidth: Integer; AAlignment: TdxListNumberAlignment): Integer;
    procedure AddSectionBreakBoxToPrevRow(ABoxType: TdxBoxClass; ABoxInfo: TdxBoxInfo); virtual;
    function GetBox(ABoxType: TdxBoxClass; ABoxInfo: TdxBoxInfo): TdxBox; virtual;
    procedure UpdateCurrentRowHeight(ABoxInfo: TdxBoxInfo);
    procedure UpdateCurrentRowHeightFromInlineObjectRun(ARun: TdxTextRunBase; ABox: TdxBox; const AMeasurer: IdxObjectMeasurer);
    procedure UpdateCurrentRowHeightFromFloatingObject(ARun: TdxFloatingObjectAnchorRun; const AMeasurer: IdxObjectMeasurer);
    function CanFitCurrentRowToColumn: TdxCanFitCurrentRowToColumnResult; overload;
    function MoveCurrentRowDownToFitTable(ATableWidth, ATableTop: Integer): Integer;
    function GetTextAreaForTable: TdxTextArea;
    function CanFitCurrentRowToColumn(ANewBoxInfo: TdxBoxInfo): TdxCanFitCurrentRowToColumnResult; overload;
    function CreateFloatingObjectSizeAndPositionController: TdxFloatingObjectSizeAndPositionController; virtual;
    function CanRestartDueFloatingObject: Boolean; virtual;
    procedure ApplyFloatingObjectBoxProperties(AFloatingObjectBox: TdxFloatingObjectBox; AFloatingObjectProperties: TdxFloatingObjectProperties); virtual;
    procedure ApplyFloatingObjectBoxBounds(AFloatingObjectBox: TdxFloatingObjectBox; AAnchorBox: TdxFloatingObjectAnchorBox); virtual;
    procedure UpdateCurrentTableCellBottom(AFloatingObjectBox: TdxFloatingObjectBox); virtual;
    procedure ApplyParagraphFrameBoxBounds(AParagraphFrameBox: TdxParagraphFrameBox; ANewParagraphFrameBox: TdxParagraphFrameBox); virtual;
    procedure FormatParagraphFrameTextBox(AParagraphFrameBox: TdxParagraphFrameBox); virtual;
    procedure AddParagraphFrameToLayoutCore(AParagraphFrameBox: TdxParagraphFrameBox; ABoxInfo: TdxBoxInfo);
    function AddParagraphFrameToLayout(AParagrahFrameBox: TdxParagraphFrameBox; ABoxInfo: TdxBoxInfo): Boolean;
    function ShouldChangeExistingFloatingObjectBounds(AFloatingObjectBox: TdxFloatingObjectBox; AAnchorBox: TdxFloatingObjectAnchorBox): Boolean;
    function AddFloatingObjectToLayout(AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox; ABoxInfo: TdxBoxInfo): Boolean;
    procedure FormatFloatingObjectTextBox(AFloatingObjectBox: TdxFloatingObjectBox; AContent: TdxTextBoxFloatingObjectContent;
      AAnchorBox: TdxFloatingObjectAnchorBox); virtual;
    procedure ResizeFloatingObjectBoxToFitText(AFloatingObjectBox: TdxFloatingObjectBox;
      AContent: TdxTextBoxFloatingObjectContent; AAnchorBox: TdxFloatingObjectAnchorBox; const ATextBoxContent: TRect; AActualSize: Integer);
    function CalculateRestartPositionDueFloatingObject(AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox): TdxDocumentModelPosition; overload; virtual;
    function CalculateRestartPositionDueParagraphFrame(AParagraphFrameBox: TdxParagraphFrameBox): TdxDocumentModelPosition; virtual;
    function CalculateRestartPositionDueFloatingObject(AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox;
      const ACurrentPosition: TPoint): TdxDocumentModelPosition; overload; virtual;
    function CalculateRestartPositionDueFloatingObject(ABox: TdxSinglePositionBox; const ABounds: TRect; AIsContains: Boolean;
      const ACurrentPosition: TPoint): TdxDocumentModelPosition; overload; virtual;
    function CanFitCurrentRowToColumnCore(ANewRowHeight: Integer): TdxCanFitCurrentRowToColumnResult;
    function CanFitNumberingListBoxToCurrentRow(const ABoxSize: TSize): Boolean;
    function CanFitBoxToCurrentRow(const ABoxSize: TSize): Boolean;
    function CreateFloatingObjectBox(AAnchorBox: TdxFloatingObjectAnchorBox; AObjectAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectBox;
    function IsPositionOutsideRightParagraphBound(APos: Integer): Boolean;
    function CalcNewCurrentRowHeight(ANewBoxInfo: TdxBoxInfo): Integer;
    procedure OnCellStart; virtual;
    procedure UpdateMaxAscentAndDescent(ABoxInfo: TdxBoxInfo);
    function GetFontAscentAndFree(ANewBoxInfo: TdxBoxInfo): Integer; overload; virtual;
    function GetFontDescent(ANewBoxInfo: TdxBoxInfo): Integer; overload; virtual;
    function GetFontAscentAndFree(ARun: TdxTextRunBase): Integer; overload; virtual;
    function GetFontDescent(ARun: TdxInlinePictureRun): Integer; overload; virtual;
    procedure RemoveLastTextBoxes;
    procedure TryToRemoveLastTabBox;
    function CreateLineSpacingCalculator: TdxLineSpacingCalculatorBase; virtual;
    procedure PrepareCurrentRowBounds(AParagraph: TdxParagraph; ABeginFromParagraphStart: Boolean);
    procedure BeginParagraph(AParagraph: TdxParagraph; ABeginFromParagraphStart, AParagraphFrameStarted: Boolean);
    procedure EnsureSpacingAfterPrevParagraphValid(ACurrentParagraph: TdxParagraph);
    function GetActualSpacingAfter(AParagraphIndex: TdxParagraphIndex): Integer;
    function ShouldIgnoreParagraphHeight(AParagraph: TdxParagraph): Boolean;
    procedure EndParagraph;
    procedure EndSection;
    procedure ClearRow(AKeepTextAreas: Boolean);
    procedure InitCurrentRow(AKeepTextAreas: Boolean);
    procedure MoveRowToNextColumn;
    procedure MoveRowToNextPage;
    procedure OnFloatingObjectsLayoutChanged;
    procedure OnParagraphFramesLayoutChanged;
    procedure OnPageBreakBeforeParagraph;
    procedure MoveCurrentRowToNextColumnCore; virtual;
    function GetEffectiveLineNumberingRestartType(ASection: TdxSection): TdxLineNumberingRestart; virtual;
    procedure OnPageFormattingComplete(ASection: TdxSection; APage: TdxPage); virtual;
    procedure CreateLineNumberBoxes(ASection: TdxSection; APage: TdxPage); overload; virtual;
    procedure CreateLineNumberBoxes(ASection: TdxSection; APageArea: TdxPageArea); overload; virtual;
    procedure CreateLineNumberBoxes(ASection: TdxSection; AColumn: TdxColumn; ALineNumbers: TdxLineNumberBoxCollection); overload; virtual;
    procedure EndRow(AFormatter: TdxParagraphBoxFormatter);
    procedure EndRowCore(AParagraph: TdxParagraph; AFormatter: TdxParagraphBoxFormatter);
    function ShouldAddParagraphBackgroundFrameBox(AParagraph: TdxParagraph): Boolean;
    function ShouldUseMaxDescent: Boolean; virtual;
    procedure CreateNewCurrentRow(APrevCurrentRow: TdxRow; AAfterRestart: Boolean); virtual;
    function CalcPrevBottom(APrevRow: TdxRow; AAfterRestart: Boolean; ANewRow: TdxRow): Integer;
    procedure ApplyCurrentColumnBounds(const ABounds: TRect);
    procedure OnDeferredBeginParagraph; virtual;
    procedure RecalcRowBounds; virtual;
    procedure ObtainRowIndents; virtual;
    function GetNextTab(AFormatter: TdxParagraphBoxFormatter): TdxTabInfo;
    function CalcDefaultRowBounds(Y: Integer): TRect;
    procedure OffsetCurrentRow(AOffset: Integer);
    function IncreaseLastRowHeight(ADelta: Integer): TdxRow;
    procedure EnforceNextRowToStartFromNewPage; virtual;
    procedure ApplySectionStart(ASection: TdxSection; ACurrentColumnsCount: Integer); virtual;
    procedure NewTableStarted;
    function CreateTableGridCalculator(ADocumentModel: TdxDocumentModel; AWidthsCalculator: TdxTableWidthsCalculator;
      AMaxTableWidth: Integer): TdxTableGridCalculator; virtual;
    procedure AddPageBreakBeforeRow;
    procedure SaveCurrentInfo;
    function CanFitCurrentRowWithFrameToColumn(ANewBoxInfo: TdxBoxInfo): TdxCanFitCurrentRowToColumnResult;
    procedure ResetSpacingBefore;

    property CurrentRowIndentAfterFloatingObject: Integer read FCurrentRowIndentAfterFloatingObject;
    property CurrentColumn: TdxColumn read FCurrentColumn write SetCurrentColumn;
    property CurrentRow: TdxRow read FCurrentRow write SetCurrentRow;
    property ParagraphFramesLayout: TdxParagraphFramesLayout read FParagraphFramesLayout;
    property TablesController: TdxTablesController read FTablesController;
    property BeginNextSectionFormatting: TdxEventHandler read FOnBeginNextSectionFormatting;
    property FrameParagraphIndex: TdxParagraphIndex read FFrameParagraphIndex write FFrameParagraphIndex;
    property TableStarted: TdxEventHandler read FOnTableStarted;

    // for internal use
    property ColumnController: TdxCustomColumnController read FColumnController;
    property MatchHorizontalTableIndentsToTextEdge: Boolean read FMatchHorizontalTableIndentsToTextEdge write FMatchHorizontalTableIndentsToTextEdge;
    property RestartModelPosition: TdxDocumentModelPosition read FRestartModelPosition write FRestartModelPosition;
  end;

  TdxCalculatePageOrdinalResult = record
    PageOrdinal: Integer;
    SkippedPageCount: Integer;

    constructor Create(APageOrdinal, ASkippedPageCount: Integer);
  end;

  { TdxPageFormattingCompleteEventArgs }

  TdxPageFormattingCompleteEventArgs = class(TdxEventArgs)
  private
    FDocumentFormattingComplete: Boolean;
    FPage: TdxPage;
  public
    constructor Create(APage: TdxPage; ADocumentFormattingComplete: Boolean);

    property Page: TdxPage read FPage;
    property DocumentFormattingComplete: Boolean read FDocumentFormattingComplete;
  end;
  TdxPageFormattingCompleteEvent = procedure (ASender: TObject; E: TdxPageFormattingCompleteEventArgs) of object;
  TdxPageFormattingCompleteEventHandler = TdxMulticastMethod<TdxPageFormattingCompleteEvent>;

  { TdxResetSecondaryFormattingForPageArgs }

  TdxResetSecondaryFormattingForPageArgs = class(TdxEventArgs)
  private
    FPage: TdxPage;
    FPageIndex: Integer;
  public
    constructor Create(APage: TdxPage; APageIndex: Integer);

    property Page: TdxPage read FPage;
    property PageIndex: Integer read FPageIndex;
  end;

  { TdxResetSecondaryFormattingForPageHandler }

  TdxResetSecondaryFormattingForPageEvent = procedure (ASender: TObject; AArgs: TdxResetSecondaryFormattingForPageArgs) of object;

  TdxResetSecondaryFormattingForPageEventHandler = TdxMulticastMethod<TdxResetSecondaryFormattingForPageEvent>;

  { TdxColumnsBoundsCalculator }

  TdxColumnsBoundsCalculator = class
  private
    FUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  protected
    procedure PopulateEqualWidthColumnsBoundsCore(const ARects: TdxRectList; const AColumnRects: TArray<TRect>;
      ASpaceBetweenColumns: Integer); virtual;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
    function Calculate(ASection: TdxSection; const ABounds: TRect): TdxRectList; virtual;
    procedure PopulateColumnsBounds(const ARects: TdxRectList; const ABounds: TRect; const AColumnInfoCollection: TdxColumnInfoCollection); virtual;
    procedure PopulateEqualWidthColumnsBounds(const AColumnBounds: TdxRectList; const ABounds: TRect; AColumnCount, ASpaceBetweenColumns: Integer); virtual;

    property UnitConverter: TdxDocumentModelUnitToLayoutUnitConverter read FUnitConverter;
  end;

  { TdxColumnController }

  TdxColumnController = class(TdxCustomColumnController)
  private
    FPageAreaController: TdxPageAreaController;
    FColumnsBounds: TdxRectList;
    FNextColumnIndex: Integer;
    FOnGenerateNewColumn: TdxEventHandler;
  protected
    function GetCurrentPageBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect; override;
    function GetCurrentPageClientBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect; override;
    function GetMeasurer: TdxBoxMeasurer; override;
    function GetColumns: TdxColumnCollection; virtual;
    function GetPageAreaController: TdxPageAreaController; override;
    function GetPageLastRunIndex: TdxRunIndex; override;
    function GetShouldZeroSpacingBeforeWhenMoveRowToNextColumn: Boolean; override;
    function GetTopLevelColumnsCount: Integer; override;
  public
    constructor Create(APageAreaController: TdxPageAreaController);
    destructor Destroy; override;
    function CompleteCurrentColumnFormatting(AColumn: TdxColumn): TdxCompleteFormattingResult; override;
    procedure RaiseGenerateNewColumn; virtual;
    procedure Reset(ASection: TdxSection); override;
    procedure ClearInvalidatedContent(const APos: TdxFormatterPosition); virtual;
    procedure ClearInvalidatedContentCore(const APos: TdxFormatterPosition; AColumns: TdxColumnCollection);
    procedure CleanupEmptyBoxes(ALastColumn: TdxColumn); virtual;
    procedure ResetToFirstColumn; override;
    procedure BeginSectionFormatting(ASection: TdxSection); virtual;
    procedure RestartFormattingFromTheStartOfSection(ASection: TdxSection; ACurrentColumnIndex: Integer); virtual;
    procedure RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; ACurrentColumnIndex: Integer); virtual;
    procedure RestartFormattingFromTheStartOfRowAtCurrentPage; virtual;
    function GetNextColumn(AColumn: TdxColumn; AKeepFloatingObjects: Boolean): TdxColumn; override;
    procedure RemoveGeneratedColumn(AColumn: TdxColumn);
    procedure AddColumn(AColumn: TdxColumn); virtual;
    function GetNextColumnCore(AColumn: TdxColumn): TdxColumn; virtual;
    function CalculateColumnBounds(AColumnIndex: Integer): TRect;
    function CalculateColumnBoundsCore(AColumnIndex: Integer): TRect; virtual; abstract;
    procedure ApplySectionStart(ASection: TdxSection); virtual;
    procedure CreateColumnBounds; virtual;
    function CreateColumnBoundsCalculator: TdxColumnsBoundsCalculator; virtual;
    function CreateRow: TdxRow; override;
    procedure AddInnerTable(ATableViewInfo: TdxTableViewInfo); override;
    function GetPreviousColumn(AColumn: TdxColumn): TdxColumn; override;
    property Columns: TdxColumnCollection read GetColumns;
    property ColumnsBounds: TdxRectList read FColumnsBounds;
    property GenerateNewColumn: TdxEventHandler read FOnGenerateNewColumn;
  end;

  { TdxPageBoundsCalculator }

  TdxPageBoundsCalculator = class
  private
    FUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  protected
    function CalculatePageBounds(ASection: TdxSection): TRect; virtual;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
    function CalculatePageClientBounds(ASection: TdxSection): TRect; virtual;
    function CalculatePageClientBoundsCore(APageWidth, APageHeight, AMarginLeft, AMarginTop, AMarginRight,
      AMarginBottom: Integer): TRect; virtual;

    property UnitConverter: TdxDocumentModelUnitToLayoutUnitConverter read FUnitConverter;
  end;

  { TdxPageController }

  TdxPageController = class abstract
  strict private
    FCurrentSection: TdxSection;
    FDocumentLayout: TdxDocumentLayout;
    FFloatingObjectsLayout: TdxFloatingObjectsLayout;
    FPageClientBounds: TRect;
    FNextPageOrdinalType: TdxSectionStartType;
    FFirstPageOfSection: Boolean;
    FTableStarted: Boolean;
    FCurrentSectionIndex: TdxSectionIndex;
    FPageBoundsCalculator: TdxPageBoundsCalculator;
    FParagraphFramesLayout: TdxParagraphFramesLayout;
    FCurrentPageClientBounds: TRect;
    FPageBounds: TRect;
    FOnPageFormattingStarted: TdxPageFormattingCompleteEventHandler;
    FOnPageFormattingComplete: TdxPageFormattingCompleteEventHandler;
    FOnFloatingObjectsLayoutChanged: TdxEventHandler;
    FOnPageCountChanged: TdxEventHandler;
    FOnParagraphFramesLayoutChanged: TdxEventHandler;
    FPageLastRunIndex: TdxRunIndex;
    FPagesBeforeTable: Integer;
    FNextSection: Boolean;
    FNeedDisposeFloatingObjectsLayout: Boolean;
    FNeedDisposeParagraphFramesLayout: Boolean;
    function ContainsOrphanedItems(AItems: TdxFloatingObjectBoxList; AMaxRunIndex: TdxRunIndex): Boolean; overload;
    function ContainsOrphanedItems(AItems: TdxParagraphFrameBoxList): Boolean; overload;
    function GetPageCount: Integer;
  protected
    procedure FormatHeader(APage: TdxPage; AFirstPageOfSection: Boolean); virtual;
    procedure FormatFooter(APage: TdxPage; AFirstPageOfSection: Boolean); virtual;
    function GetActualFormattingSectionIndex(ASectionIndex: TdxSectionIndex): TdxSectionIndex;
    function GetCurrentSection: TdxSection; virtual;
    function GetCurrentSectionIndex: TdxSectionIndex; virtual;
    function GetFirstPageOfSection: Boolean; virtual;
    function GetPages: TdxPageCollection; virtual;
    function GetPieceTable: TdxPieceTable; virtual;
    function GetDocumentModel: TdxDocumentModel; virtual;
    procedure SetCurrentSectionIndex(const Value: TdxSectionIndex); virtual;
    function IsCurrentPageEven: Boolean; virtual;
    procedure AppendFloatingObjects(AFrom, ATo: TdxFloatingObjectBoxList; AMaxRunIndex: TdxRunIndex); virtual;
    procedure AppendParagraphFrames(AFrom, ATo: TdxParagraphFrameBoxList); virtual;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; AFloatingObjectsLayout: TdxFloatingObjectsLayout; AParagraphFramesLayout: TdxParagraphFramesLayout);
    destructor Destroy; override;

    procedure PageContentFormattingComplete(APage: TdxPage); virtual;
    procedure RaisePageFormattingStarted(APage: TdxPage); virtual;
    procedure RaisePageFormattingComplete(APage: TdxPage); virtual;
    procedure RaisePageCountChanged; virtual;
    procedure RaiseParagraphFramesLayoutChanged; virtual;
    procedure RaiseFloatingObjectsLayoutChanged; virtual;

    procedure Reset(AKeepFloatingObjects: Boolean); virtual;
    function ClearInvalidatedContentFromTheStartOfRowAtCurrentPage(const APos: TdxFormatterPosition;
      AParagraphIndex: TdxParagraphIndex; AKeepFloatingObjects: Boolean; ATablesController: TdxTablesController;
      AEmptyCurrentRow: Boolean; out ALeaveTable: Boolean): TdxClearInvalidatedContentResult; virtual;

    function ClearInvalidatedContent(const APos: TdxFormatterPosition; AParagraphIndex: TdxParagraphIndex;
      AKeepFloatingObjects, AEmptyCurrentRow: Boolean): TdxClearInvalidatedContentResult; virtual;
    function CompleteCurrentPageFormatting: TdxCompleteFormattingResult; virtual;
    procedure CorrectLastPageOrdinal(AParagraphIndex: TdxParagraphIndex; APageIndex: Integer); virtual;
    procedure BeginNextSectionFormatting(ASectionIndex: TdxSectionIndex); virtual;
    procedure RestartFormattingFromTheStartOfSection; virtual;
    function CalculatePageBounds(ASection: TdxSection): TRect; virtual;
    procedure RestartFormattingFromTheMiddleOfSectionCore(ASection: TdxSection); virtual;
    procedure PopulateFloatingObjectsLayout(ALastPage: TdxPage);
    procedure PopulateFloatingObjects(AFloatingObjects: TdxFloatingObjectBoxList);
    procedure PopulateParagraphFramesLayout(ALastPage: TdxPage);
    procedure PopulateParagraphFrames(AParagraphFrames: TdxParagraphFrameBoxList);
    procedure ResetPageLastRunIndex;
    procedure RestartFormattingFromTheMiddleOfSection(ASection: TdxSection); virtual;
    procedure RestartFormattingFromTheStartOfRowAtCurrentPage; virtual;
    function GetNextPage(AKeepFloatingObjects: Boolean): TdxPage; virtual;
    function CalculatePageOrdinal(AFirstPageOfSection: Boolean): TdxCalculatePageOrdinalResult; virtual;
    function CalculatePageOrdinalCore(APreviousPageOrdinal, ANumSkippedPages: Integer; AFirstPageOfSection: Boolean): TdxCalculatePageOrdinalResult; virtual;
    function GetNextPageCore: TdxPage; virtual;
    procedure ApplySectionStart(ASection: TdxSection); virtual;
    function CalculateNextPageOrdinalType(ASection: TdxSection): TdxSectionStartType; virtual;
    procedure FinalizePagePrimaryFormatting(APage: TdxPage; ADocumentEnded: Boolean); virtual;
    function AppendFloatingObjectsToPage(APage: TdxPage): Boolean; virtual;
    procedure ClearFloatingObjectsLayout; virtual;
    procedure ClearParagraphFramesLayout; virtual;
    procedure ApplyExistingHeaderAreaBounds(APage: TdxPage); virtual;
    procedure ApplyExistingFooterAreaBounds(APage: TdxPage); virtual;
    function CreatePageBoundsCalculator: TdxPageBoundsCalculator; virtual; abstract;
    procedure RemoveLastPage;
    procedure SetPageLastRunIndex(ARunIndex: TdxRunIndex);
    procedure BeginTableFormatting;
    procedure EndTableFormatting;
    function CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest;
      AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator; virtual; abstract;
    procedure SetFloatingObjectsLayout(ALayout: TdxFloatingObjectsLayout);

    property PageFormattingStarted: TdxPageFormattingCompleteEventHandler read FOnPageFormattingStarted;
    property PageFormattingComplete: TdxPageFormattingCompleteEventHandler read FOnPageFormattingComplete;
    property ParagraphFramesLayoutChanged: TdxEventHandler read FOnParagraphFramesLayoutChanged;
    property FloatingObjectsLayoutChanged: TdxEventHandler read FOnFloatingObjectsLayoutChanged;
    property PageCountChanged: TdxEventHandler read FOnPageCountChanged;
    property ParagraphFramesLayout: TdxParagraphFramesLayout read FParagraphFramesLayout;
    property PageBounds: TRect read FPageBounds;
    property PageClientBounds: TRect read FCurrentPageClientBounds;
    property CurrentSectionIndex: TdxSectionIndex read GetCurrentSectionIndex write SetCurrentSectionIndex;
    property CurrentSection: TdxSection read GetCurrentSection;
    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
    property PageCount: Integer read GetPageCount;
    property Pages: TdxPageCollection read GetPages;
    property NextPageOrdinalType: TdxSectionStartType read FNextPageOrdinalType;
    property PageBoundsCalculator: TdxPageBoundsCalculator read FPageBoundsCalculator;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property FirstPageOfSection: Boolean read GetFirstPageOfSection;
    property FloatingObjectsLayout: TdxFloatingObjectsLayout read FFloatingObjectsLayout;
    property NextSection: Boolean read FNextSection write FNextSection;
    property PageLastRunIndex: TdxRunIndex read FPageLastRunIndex;
  end;

  { TdxPageAreaControllerState }

  TdxPageAreaControllerState = class(TcxIUnknownObject, IdxPageAreaController)
  private
    FOwner: TdxPageAreaController;
    FCurrentAreaBounds: TRect;
    function GetPageController: TdxPageController;
  protected
    function GetAreas: TdxPageAreaCollection; virtual;
    procedure RestoreCurrentAreaBounds(const AOldBounds: TRect); virtual;
  public
    constructor Create(AOwner: TdxPageAreaController);
    function CompleteCurrentAreaFormatting: TdxCompleteFormattingResult; virtual; abstract;
    function GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea; virtual; abstract;
    procedure Reset(ASection: TdxSection); virtual;
    procedure BeginSectionFormatting(ASection: TdxSection; ACurrentColumnsCount: Integer); virtual;
    procedure RestartFormattingFromTheStartOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer); virtual;
    procedure RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer); virtual;
    procedure RestartFormattingFromTheStartOfRowAtCurrentPage; virtual;
    procedure ClearInvalidatedContent(const APos: TdxFormatterPosition);
    function GetNextPageAreaCore: TdxPageArea;
    procedure CreateCurrentAreaBounds; virtual;
    function CreateCurrentAreaBoundsCore: TRect; virtual; abstract;
    procedure ApplySectionStart(ASection: TdxSection; ACurrentColumnsCount: Integer); virtual; abstract;

    property Owner: TdxPageAreaController read FOwner;
    property CurrentAreaBounds: TRect read FCurrentAreaBounds;
    property PageController: TdxPageController read GetPageController;
    property Areas: TdxPageAreaCollection read GetAreas;
  end;

  { TdxPageAreaController }

  TdxPageAreaController = class(TcxIUnknownObject, IdxPageAreaController)
  private
    FPageController: TdxPageController;
    FState: TdxPageAreaControllerState;
  protected
    function GetAreas: TdxPageAreaCollection; virtual;
    function GetCurrentAreaBounds: TRect; virtual;
  public
    constructor Create(APageController: TdxPageController);
    destructor Destroy; override;

    function CreateDefaultState(ACurrentAreaIndex: Integer): TdxPageAreaControllerState; virtual;
    function GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea; virtual;
    procedure Reset(ASection: TdxSection); virtual;
    procedure BeginSectionFormatting(ASection: TdxSection; ACurrentColumnsCount: Integer); virtual;
    procedure RestartFormattingFromTheStartOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer); virtual;
    procedure RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer); virtual;
    procedure RestartFormattingFromTheStartOfRowAtCurrentPage; virtual;
    procedure ClearInvalidatedContent(const APos: TdxFormatterPosition); virtual;
    function CompleteCurrentAreaFormatting: TdxCompleteFormattingResult; virtual;
    procedure SwitchToState(AState: TdxPageAreaControllerState); virtual;
    procedure RemoveLastPageArea; virtual;

    property PageController: TdxPageController read FPageController;
    property Areas: TdxPageAreaCollection read GetAreas;
    property State: TdxPageAreaControllerState read FState;
    property CurrentAreaBounds: TRect read GetCurrentAreaBounds;
  end;

  { TdxDefaultPageAreaControllerState }

  TdxDefaultPageAreaControllerState = class(TdxPageAreaControllerState)
  private
    FNextAreaIndex: Integer;
  protected
    procedure ApplyContinuousSectionStart(ASection: TdxSection); virtual;
  public
    constructor Create(AOwner: TdxPageAreaController; ANextAreaIndex: Integer);
    function CompleteCurrentAreaFormatting: TdxCompleteFormattingResult; override;
    function GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea; override;
    procedure RestartFormattingFromTheStartOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer); override;
    procedure RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer); override;
    function CreateCurrentAreaBoundsCore: TRect; override;
    procedure ApplySectionStart(ASection: TdxSection; ACurrentColumnsCount: Integer); override;
  end;

  { TdxTablesControllerStateBaseStack }

  TdxTablesControllerStateBaseStack = class(TStack)
  private
    function GetItem(Index: Integer): TdxTablesControllerStateBase;
  public
    function Push(AItem: TdxTablesControllerStateBase): TdxTablesControllerStateBase;
    function Pop: TdxTablesControllerStateBase;
    function Peek: TdxTablesControllerStateBase;

    property Items[Index: Integer]: TdxTablesControllerStateBase read GetItem; default;
  end;

  TdxTableBreakType = (
    NoBreak,
    NextPage,
    InfinityHeight
  );

  TdxTableViewInfoManager = class;

  { TdxTablesController }

  TdxTablesController = class
  private
    FRowsController: TdxRowsController;
    FPageController: TdxPageController;
    FStates: TdxTablesControllerStateBaseStack;
    FTableBreaks: TObjectDictionary<TdxTable, TdxSortedList<TPair<Integer, Integer>>>;
    FInfinityHeights: TdxIntegersDictionary;
    function GetState: TdxTablesControllerStateBase;
    function GetIsInsideTable: Boolean;
  protected
    function CreateTableViewInfoManager(AParentTableViewInfoManager: TdxTableViewInfoManager;
      APageController: TdxPageController; ARowsController: TdxRowsController): TdxTableViewInfoManager; virtual;
    function GetSimpleView: Boolean; virtual;
  public
    constructor Create(APageController: TdxPageController; ARowsController: TdxRowsController);
    destructor Destroy; override;

    function GetTableBreak(ATable: TdxTable; ATableViewInfoIndex: Integer; out ABottomBounds: Integer): TdxTableBreakType;
    function IsInfinityHeight(ATableViewInfoIndex: Integer; out ABottomAnchorIndex: Integer): Boolean;
    procedure AddTableBreak(ATable: TdxTable; ATableViewInfoIndex, ABottomBounds: Integer);
    procedure AddInfinityTableBreak(ATableViewInfoIndex, ABottomAnchorIndex: Integer);
    procedure RemoveAllTableBreaks;
    procedure BeginParagraph(AParagraph: TdxParagraph);
    procedure StartNewTable(ANewCell: TdxTableCell);
    procedure StartTopLevelTable(ANewCell: TdxTableCell);
    procedure EndParagraph(ALastRow: TdxRow);
    function CanFitRowToColumn(ALastTextRowBottom: TdxLayoutUnit; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult; virtual;
    function CanFitFloatingObjectInTableRow(ABottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult;

    procedure BeforeMoveRowToNextColumn;
    procedure AfterMoveRowToNextColumn;
    procedure AfterMoveRowToNextPage;
    procedure BeforeMoveRowToNextPage;
    procedure StartInnerTable(ACell: TdxTableCell);
    procedure UpdateCurrentCellBottom(ABottom: TdxLayoutUnit);
    procedure UpdateCurrentCellHeight;
    procedure ReturnToPrevState;
    procedure LeaveCurrentTableCore;
    procedure LeaveCurrentTable(ANextCell: TdxTableCell);
    procedure ClearInvalidatedContent;
    procedure Reset;
    procedure OnCurrentRowFinished;
    function RollbackToStartOfRowTableOnFirstCellRowColumnOverfull: TdxParagraphIndex;
    function GetCurrentCell: TdxTableCell;

    property RowsController: TdxRowsController read FRowsController;
    property PageController: TdxPageController read FPageController;
    property SimpleView: Boolean read GetSimpleView;
    property State: TdxTablesControllerStateBase read GetState;
    property States: TdxTablesControllerStateBaseStack read FStates;
    property IsInsideTable: Boolean read GetIsInsideTable;
  end;

  { TdxLayoutGridRectangle }

  TdxLayoutGridRectangle = record
    Bounds: TRect;
    ColumnIndex: Integer;
    ColumnSpan: Integer;
    RowSpan: Integer;
    RowIndex: Integer;
    constructor Create(const ABounds: TRect; ARowIndex, AColumnIndex, AColumnSpan: Integer);
  end;

  { TdxTableViewInfos }

  TdxTableViewInfos = class(TdxReferencedObjectList<TdxTableViewInfo>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TdxTableViewInfoManager }

  TdxTableViewInfoManager = class
  public type
    TCellAction = (None, Split, SetBottomIndex);
  strict private
    FRowsController: TdxRowsController;
    FPageController: TdxPageController;
    FParentTableViewInfoManager: TdxTableViewInfoManager;
    FTableViewInfos: TdxTableViewInfos;
    FCurrentCellBottoms: TdxIntegerList;
    FColumnController: TdxTableCellColumnController;
    FTableGrid: TdxTableGrid;
    FCurrentTableViewInfoIndex: Integer;
    FCurrentTableCellViewInfo: TdxTableCellViewInfo;
    FTable: TdxTable;
    procedure SetColumnController(Value: TdxTableCellColumnController);
    function GetCurrentTableViewInfo: TdxTableViewInfo;
    function GetCurrentCellBottom: Integer;
    procedure SetCurrentCellBottom(Value: Integer);
    procedure InternalSetCurrentTableCellViewInfo(Value: TdxTableCellViewInfo);
    function CreateTableViewInfo(ATable: TdxTable; ATopRowIndex: Integer; AFirstContentInParentCell: Boolean;
      AVerticalBorderPositions: TdxVerticalBorderPositions): TdxTableViewInfo;
    function AddTableViewInfo(ATableViewInfo: TdxTableViewInfo): Integer;
    function GetStartTableViewInfoByRowSeparatorIndex(ARowSeparatorIndex: Integer): TdxTableViewInfo;
    function GetStartTableViewInfoIndexByRowSeparatorIndex(ARowSeparatorIndex: Integer): Integer;
    function GetEndTableViewInfoIndexByRowSeparatorIndex(ARowSeparatorIndex: Integer): Integer;
    procedure SetRowSeparatorCore(ATableViewInfo: TdxTableViewInfo; ARowSeparatorIndex: Integer; AAnchor: TdxTableCellVerticalAnchor);
    function CreateTableRowViewInfo(ATableViewInfo: TdxTableViewInfo; ARow: TdxTableRow; ARowIndex: Integer): TdxTableRowViewInfoBase;
    function GetRowStartAnchorIndex(ATableViewInfo: TdxTableViewInfo; ARowIndex: Integer): Integer;
    function GetRowStartAnchor(ATableViewInfo: TdxTableViewInfo; ARowIndex: Integer): TdxTableCellVerticalAnchor; overload;
    function AddFirstCellViewInfo(ATableViewInfo: TdxTableViewInfo; ACell: TdxTableCell;
      const ABounds: TRect; const AGridBounds: TdxLayoutGridRectangle; const ATextBounds: TRect): TdxTableCellViewInfo;
    procedure AddMiddleCellViewInfo(ATableViewInfo: TdxTableViewInfo; ACell: TdxTableCell;
      const ABounds: TRect; const AGridBounds: TdxLayoutGridRectangle; const ATextBounds: TRect);
    procedure AddLastCellViewInfo(ATableViewInfo: TdxTableViewInfo; ACell: TdxTableCell; const ABounds: TRect;
      const AGridBounds: TdxLayoutGridRectangle; const ATextBounds: TRect);
    function GetLastTableViewInfo: TdxTableViewInfo;
    procedure AddTableCellViewInfo(ATableViewInfo: TdxTableViewInfo; ACellViewInfo: TdxTableCellViewInfo; AStartRowViewInfoIndex, AEndRowViewInfoIndex: Integer);
    procedure MoveRowAndAnchors_(ASource, ATarget: TdxTableViewInfo; AStartAnchorIndex: Integer);
    function ShouldSplitCell(ASource, ATarget: TdxTableViewInfo; ACellViewInfo: TdxTableCellViewInfo;
      ASplitAnchorIndex, ASourceAnchorsCount, AInitialSourceBottomRowIndex, AInitialTargetTopRowIndex: Integer): TCellAction;
    procedure MoveRowAndAnchor(ASource, ATarget: TdxTableViewInfo; AStartAnchorIndex: Integer);
    procedure SplitCellsByAnchor(ACurrentTableViewInfo, ANextTableViewInfo: TdxTableViewInfo; AAnchorIndex: Integer);
    procedure SplitCellByAnchor(ACurrentTableViewInfo, ANextTableViewInfo: TdxTableViewInfo; AAnchorIndex: Integer; ATableCellViewInfo: TdxTableCellViewInfo);
    function ShouldRemoveColumn(ATableViewInfoIndex: Integer; ATableViewInfo: TdxTableViewInfo): Boolean;
    function GetTopLevelTableViewInfoManager: TdxTableViewInfoManager;
  protected
    procedure EnsureFloatingObjectsLayoutValid(ATableViewInfo: TdxTableViewInfo); virtual;
    function GetParentTableCellViewInfo: TdxTableCellViewInfo; virtual;
    procedure SetCurrentTableCellViewInfo(ATableCellViewInfo: TdxTableCellViewInfo); virtual;
    procedure ValidateTopLevelColumn; virtual;
    procedure SetCurrentParentColumn(ATableViewInfo: TdxTableViewInfo); virtual;
    function FindSuitableTableCellViewInfo(ACells: TdxTableCellViewInfoCollection; ACell: TdxTableCell): TdxTableCellViewInfo;
    function GetSplitAnchorHorizontalCellBorders(ASplitAnchor: TdxTableCellVerticalAnchor): TdxHorizontalCellBordersInfoList; virtual;
  public
    constructor Create(AParentTableViewInfoManager: TdxTableViewInfoManager; APageController: TdxPageController; ARowsController: TdxRowsController);
    destructor Destroy; override;

    procedure BeforeStartNextCell(ACell: TdxTableCell; const AGridBounds: TdxLayoutGridRectangle);
    function StartNewTable(ATable: TdxTable; ACellBounds: TDictionary<TdxTableCell, TdxLayoutGridRectangle>;
      ALeftOffset, ARightCellMargin: TdxLayoutUnit; AFirstContentInParentCell: Boolean;
      const ATableTextArea: TdxTextArea; out AMaxRight: TdxLayoutUnit): TdxTableViewInfo;
    function GetTableGrid: TdxTableGrid;
    function GetTableViewInfos: TdxList<TdxTableViewInfo>;
    procedure SetRowSeparator(ARowSeparatorIndex: Integer; AAnchor: TdxTableCellVerticalAnchor);
    procedure SetRowSeparatorForCurrentTableViewInfo(ARowSeparatorIndex: Integer; AAnchor: TdxTableCellVerticalAnchor);
    procedure EnsureTableRowViewInfo(ATableViewInfo: TdxTableViewInfo; ARow: TdxTableRow; ARowIndex: Integer);
    function GetRowStartAnchor(ARowIndex: Integer): TdxTableCellVerticalAnchor; overload;
    function GetRowStartColumn(ARowIndex: Integer): TdxColumn;
    function GetRowStartTableViewInfo(ATableRow: TdxTableRow): TdxTableViewInfo;
    procedure StartNextCell(ACell: TdxTableCell; const ABounds: TRect; const AGridBounds: TdxLayoutGridRectangle; const ATextBounds: TRect);
    function GetLastTableBottom: Integer;
    function GetBottomCellAnchor(ACurrentCell: TdxTableCell): TdxTableCellVerticalAnchor;
    function GetBottomCellRowSeparatorIndex(ACurrentCell: TdxTableCell; ARowSpan: Integer): Integer;
    procedure BeforeMoveRowToNextColumn(ACurrentCellBottom: TdxLayoutUnit);
    procedure AfterMoveRowToNextColumn;
    function GetCurrentCellTop: Integer;
    function GetCurrentCellTopAnchorIndex: Integer;
    function GetCurrentCellBottomAnchorIndex: Integer;
    procedure LeaveCurrentTable(ABeforeRestart: Boolean); virtual;
    procedure RemoveAllInvalidRowsOnColumnOverfull(AFirstInvalidRowIndex: Integer);
    procedure FixColumnOverflow; virtual;
    function IsCurrentCellViewInfoFirst: Boolean;
    function IsFirstTableRowViewInfoInColumn: Boolean;
    procedure SetCurrentCellHasContent;
    function GetCurrentTableViewInfoIndex: Integer;
    function GetCurrentTopLevelTableViewInfoIndex: Integer;
    function IsFirstContentInParentCell: Boolean;
    function GetParentTableViewInfoManager: TdxTableViewInfoManager;

    property ColumnController: TdxTableCellColumnController read FColumnController write SetColumnController;
    property CurrentTableViewInfo: TdxTableViewInfo read GetCurrentTableViewInfo;
    property CurrentCellBottom: Integer read GetCurrentCellBottom write SetCurrentCellBottom;
    property CurrentTableCellViewInfo: TdxTableCellViewInfo read FCurrentTableCellViewInfo write InternalSetCurrentTableCellViewInfo;
    property RowsController: TdxRowsController read FRowsController;
    property TopLevelTableViewInfoManager: TdxTableViewInfoManager read GetTopLevelTableViewInfoManager;
  end;

  { TdxTablesControllerStateBase }

  TdxTablesControllerStateBase = class abstract
  private
    FTablesController: TdxTablesController;
  public
    constructor Create(ATablesController: TdxTablesController);
    destructor Destroy; override;

    procedure EnsureCurrentCell(ACell: TdxTableCell); virtual; abstract;
    procedure UpdateCurrentCellBottom(ABottom: TdxLayoutUnit); virtual; abstract;
    procedure EndParagraph(ALastRow: TdxRow); virtual; abstract;
    procedure BeforeMoveRowToNextColumn; virtual; abstract;
    procedure AfterMoveRowToNextColumn; virtual; abstract;
    procedure UpdateCurrentCellHeight(ARow: TdxRow); virtual; abstract;
    function CanFitRowToColumn(ALastTextRowBottom: TdxLayoutUnit; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult; virtual; abstract;
    function CanFitFloatingObjectInTableRow(ABottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult; virtual; abstract;
    procedure OnCurrentRowFinished; virtual; abstract;
    function RollbackToStartOfRowTableOnFirstCellRowColumnOverfull(AFirstTableRowViewInfoInColumn, AInnerMostTable: Boolean): TdxParagraphIndex; virtual; abstract;

    property TablesController: TdxTablesController read FTablesController;
  end;

  { TdxTablesControllerNoTableState }

  TdxTablesControllerNoTableState = class(TdxTablesControllerStateBase)
  public
    procedure EnsureCurrentCell(ACell: TdxTableCell); override;
    procedure EndParagraph(ALastRow: TdxRow); override;
    procedure BeforeMoveRowToNextColumn; override;
    procedure AfterMoveRowToNextColumn; override;
    procedure UpdateCurrentCellHeight(ARow: TdxRow); override;
    procedure UpdateCurrentCellBottom(ABottom: TdxLayoutUnit); override;
    function CanFitRowToColumn(ALastTextRowBottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult; override;
    procedure OnCurrentRowFinished; override;
    function RollbackToStartOfRowTableOnFirstCellRowColumnOverfull(AFirstTableRowViewInfoInColumn, AInnerMostTable: Boolean): TdxParagraphIndex; override;
    function CanFitFloatingObjectInTableRow(ABottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult; override;
  end;

  { TdxTableCellIteratorBase }

  TdxTableCellIteratorBase = class
  protected
    function GetCurrentStartColumnIndex: Integer; virtual; abstract;
    function GetCurrentEndColumnIndex: Integer; virtual; abstract;
    function GetCurrentCell: TdxTableCell; virtual; abstract;
    function GetEndOfRow: Boolean; virtual; abstract;
  public
    procedure SetStartColumnIndex(ANewStartColumnIndex: Integer); virtual; abstract;
    function MoveNextCell: Boolean; virtual; abstract;

    property CurrentStartColumnIndex: Integer read GetCurrentStartColumnIndex;
    property CurrentEndColumnIndex: Integer read GetCurrentEndColumnIndex;
    property CurrentCell: TdxTableCell read GetCurrentCell;
    property EndOfRow: Boolean read GetEndOfRow;
  end;

  { TdxTableCellIterator }

  TdxTableCellIterator = class(TdxTableCellIteratorBase)
  private
    FRow: TdxTableRow;
    FCurrentStartColumnIndex: Integer;
    FCurrentEndColumnIndex: Integer;
    FCurrentCellIndex: Integer;
    function GetCellCount: Integer;
  protected
    function GetCurrentStartColumnIndex: Integer; override;
    function GetCurrentEndColumnIndex: Integer; override;
    function GetCurrentCell: TdxTableCell; override;
    function GetEndOfRow: Boolean; override;

    property Row: TdxTableRow read FRow;
    property CellCount: Integer read GetCellCount;
  public
    constructor Create(ARow: TdxTableRow);
    procedure SetStartColumnIndex(ANewStartColumnIndex: Integer); override;
    function MoveNextCell: Boolean; override;
  end;

  { TdxTableCellEmptyIterator }

  TdxTableCellEmptyIterator = class(TdxTableCellIteratorBase)
  protected
    function GetCurrentStartColumnIndex: Integer; override;
    function GetCurrentEndColumnIndex: Integer; override;
    function GetCurrentCell: TdxTableCell; override;
    function GetEndOfRow: Boolean; override;
  public
    function MoveNextCell: Boolean; override;
    procedure SetStartColumnIndex(ANewStartColumnIndex: Integer); override;
  end;

  { TdxTableCellBorderIterator }

  TdxTableCellBorderIterator = class
  private
    FCurrentCellAbove: TdxTableCell;
    FCurrentCellBelow: TdxTableCell;
    FAboveRowIterator: TdxTableCellIteratorBase;
    FBelowRowIterator: TdxTableCellIteratorBase;
    FCurrentStartColumnIndex: Integer;
    FCurrentEndColumnIndex: Integer;
    function GetCurrentAboveInfo: TdxNullableHorizontalCellBordersInfo;
    function GetCurrentMergedInfo: TdxHorizontalCellBordersInfo;
    function GetCurrentBelowInfo: TdxNullableHorizontalCellBordersInfo;
  public
    constructor Create(AAboveRow, ABelowRow: TdxTableRow);
    destructor Destroy; override;
    function MoveNext: Boolean;
    function IsVerticallyMerged: Boolean;

    property CurrentAboveInfo: TdxNullableHorizontalCellBordersInfo read GetCurrentAboveInfo;
    property CurrentMergedInfo: TdxHorizontalCellBordersInfo read GetCurrentMergedInfo;
    property CurrentBelowInfo: TdxNullableHorizontalCellBordersInfo read GetCurrentBelowInfo;
  end;

  { TdxTableCellHorizontalBorderCalculator }

  TdxTableCellHorizontalBorderCalculator = class
  private
    FTable: TdxTable;
    function GetAnchorBorders(AAnchorIndex: Integer): TdxHorizontalCellBordersInfoList;
    function ResolveBorder(ATable: TdxTable; ABorder1, ABorder2: TdxNullableHorizontalCellBordersInfo): TdxHorizontalCellBordersInfo;
  public
    constructor Create(ATable: TdxTable);
    function GetBottomBorders(ARow: TdxTableRow): TdxHorizontalCellBordersInfoList;
    function GetTopBorders(ARow: TdxTableRow): TdxHorizontalCellBordersInfoList;
  end;

  TdxRestartFrom = (
    NoRestart,
    CellStart,
    TableStart
  );

  { TdxTablesControllerTableState }

  TdxTablesControllerTableState = class(TdxTablesControllerStateBase)
  private
    FTableViewInfoManager: TdxTableViewInfoManager;
    FCellsBounds: TDictionary<TdxTableCell, TdxLayoutGridRectangle>;
    FUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    FVerticalBordersCalculator: TdxTableCellVerticalBorderCalculator;
    FCurrentRow: TdxTableRow;
    FCurrentCell: TdxTableCell;
    FCurrentCellRowCount: Integer;
    FRestartFrom: TdxRestartFrom;
    FRowBoundsBeforeTableStart: TRect;
    FMaxTableWidth: TdxLayoutUnit;
    FTableRight: TdxLayoutUnit;

    function GetRowSpan(const AGridPosition: TdxLayoutGridRectangle): Integer;
    function GetCellBounds(ANewCell: TdxTableCell): TRect;
    function CalcFirstInvalidRowIndex: Integer;

    function GetCurrentTable: TdxTable;
    function GetRowsController: TdxRowsController;
    function GetCurrentCellViewInfoEmpty: Boolean;
  protected
    FHorizontalBordersCalculator: TdxTableCellHorizontalBorderCalculator;
    FBorderCalculator: TdxTableBorderCalculator;
    procedure StartNewTable(ATable: TdxTable; AFirstContentInParentCell, ASimpleView: Boolean);
    procedure EnsureCurrentTableRow(ARow: TdxTableRow);
    function CalculateTableActualLeftRelativeOffset(ATable: TdxTable): TdxModelUnit;
    function GetTextBounds(ACell: TdxTableCell; const ACellBounds: TRect): TRect;
    function GetActualCellBottomMargin(ACell: TdxTableCell): TdxModelUnit;
    function GetActualCellLeftMargin(ACell: TdxTableCell): TdxModelUnit;
    function GetActualCellRightMargin(ACell: TdxTableCell): TdxModelUnit;
    function CalculateBottomTextIndent(ARowSeparatorIndex: Integer; out ACellBordersInfo: TdxHorizontalCellBordersInfoList): TdxLayoutUnit;
    procedure ChangeCurrentCellInTheSameTable(ANewCell: TdxTableCell);
    procedure PrepareGridCellBounds;
    function CalculateShiftedPositions(ASimpleView: Boolean): TdxLayoutUnitSortedList;
    function CalculateInitialPositions(ALeftOffset: TdxLayoutUnit): TdxLayoutUnitSortedList;
    function PrepareCellsBounds(ALeftOffset: TdxLayoutUnit; ASimpleView: Boolean): TdxVerticalBorderPositions;
    function GetWidth(AGrid: TdxTableGrid; AFirstColumn, ACount: Integer): TdxLayoutUnit;
    procedure FinishCurrentTable;
    procedure ProcessPendingCells(ATableViewInfo: TdxTableViewInfo; APendingCells: TdxList<TdxTableCellViewInfo>);
    procedure FinishCurrentCell;
    function CalcFirstInvalidRowIndexCore(AStartRowIndex: Integer): Integer;

    procedure ShiftRow(APositions: TdxLayoutUnitSortedList; ARow: TdxTableRow; AOffset: Integer); virtual;
    procedure AddTopAnchor; virtual;
    function GetRowOffset(ARow: TdxTableRow; ASimpleView: Boolean): Integer; virtual;
  public
    constructor Create(ATablesController: TdxTablesController; AStartCell: TdxTableCell; AFirstContentInParentCell, ASimpleView: Boolean);
    destructor Destroy; override;

    class function GetActualCellSpacing(ARow: TdxTableRow): TdxModelUnit; static;
    function GetActualCellTopMargin(ACell: TdxTableCell): TdxModelUnit;

    function CanFitRowToColumn(ALastTextRowBottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult; override;
    function CanFitFloatingObjectInTableRow(ABottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult; override;
    function IsFirstTableRowViewInfoInColumn: Boolean;
    procedure EnsureCurrentCell(ACell: TdxTableCell); override;
    procedure OnCurrentRowFinished; override;
    procedure LeaveCurrentTable(ABeforeRestart, ARoolbackParent: Boolean);
    procedure EndParagraph(ALastRow: TdxRow); override;
    procedure UpdateCurrentCellBottom(ABottom: TdxLayoutUnit); override;
    procedure UpdateCurrentCellHeight(ARow: TdxRow); override;
    procedure BeforeMoveRowToNextColumn; override;
    procedure AfterMoveRowToNextColumn; override;
    function RollbackToStartOfRowTableOnFirstCellRowColumnOverfull(AFirstTableRowViewInfoInColumn,
      AInnerMostTable: Boolean): TdxParagraphIndex; override;
    function GetCurrentCell: TdxTableCell;

    property CurrentCell: TdxTableCell read FCurrentCell;
    property CurrentRow: TdxTableRow read FCurrentRow;
    property CurrentTable: TdxTable read GetCurrentTable;
    property RowsController: TdxRowsController read GetRowsController;
    property UnitConverter: TdxDocumentModelUnitToLayoutUnitConverter read FUnitConverter;
    property VerticalBordersCalculator: TdxTableCellVerticalBorderCalculator read FVerticalBordersCalculator;
    property TableViewInfoManager: TdxTableViewInfoManager read FTableViewInfoManager;
    property CurrentCellViewInfoEmpty: Boolean read GetCurrentCellViewInfoEmpty;
  end;

  { TdxFloatingObjectSizeController }

  TdxFloatingObjectSizeController = class
  strict private
    FPieceTable: TdxPieceTable;
    function GetDocumentModel: TdxDocumentModel;
  protected
    function ValidateRotatedShapeHorizontalPosition(const ARect: TRect; AProperties: TdxFloatingObjectProperties): TRect; virtual;
    function ValidateRotatedShapeVerticalPosition(const ARect: TRect; AProperties: TdxFloatingObjectProperties): TRect; virtual;
    function CreateFloatingObjectLocation(AProperties: TdxFloatingObjectProperties; AAutoHeight: Boolean): IdxFloatingObjectLocation;
    function CreateRotatedFloatingObjectLocation(const AProperties: IdxFloatingObjectLocation;
      AAngle: Integer; AAutoHeight: Boolean): IdxFloatingObjectLocation;
    function CalculateAbsoluteFloatingObjectShapeBounds(const ALocation: IdxFloatingObjectLocation; AShape: TdxShape; AIsTextBox: Boolean): TRect; virtual;
    function CalculateAbsoluteParagraphFrameShapeBounds(const ALocation: IdxParagraphFrameLocation): TRect; virtual;
    function CenterRectangleHorizontally(const ARectangle: TRect; const AWhere: TRect): TRect;
    function CenterRectangleVertically(const ARectangle: TRect; const AWhere: TRect): TRect;
    function CalculateAbsoluteFloatingObjectContentBounds(const AFloatingObjectProperties: IdxFloatingObjectLocation;
      AShape: TdxShape; const AShapeBounds: TRect): TRect; virtual;
    function CalculateActualAbsoluteFloatingObjectBounds(AFloatingObjectProperties: TdxFloatingObjectProperties; const ABounds: TRect): TRect; virtual;
    function CalculateAbsoluteFloatingObjectBounds(const ALocation: IdxFloatingObjectLocation): TRect; virtual;
    function CalculateAbsoluteParagraphFrameBounds(const ALocation: IdxParagraphFrameLocation): TRect; virtual;
    class function GetActualBorderWidth(AUseBorder: Boolean; ABorder: TdxBorderInfo; AConverter: TdxDocumentModelUnitToLayoutUnitConverter): Integer; static;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read FPieceTable;
  public
    constructor Create(APieceTable: TdxPieceTable);
    procedure UpdateFloatingObjectBox(AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox);
    procedure UpdateParagraphFrameBox(AParagraphFrameBox: TdxParagraphFrameBox);
  end;

  { TdxFloatingObjectTargetPlacementInfo }

  TdxFloatingObjectTargetPlacementInfo = record
  strict private
    FColumnBounds: TRect;
    FOriginalColumnBounds: TRect;
    FOriginX: Integer;
    FOriginY: Integer;
    FPageBounds: TRect;
    FPageClientBounds: TRect;
    procedure SetColumnBounds(const Value: TRect);
  public
    procedure Init;

    property ColumnBounds: TRect read FColumnBounds write SetColumnBounds;
    property OriginalColumnBounds: TRect read FOriginalColumnBounds write FOriginalColumnBounds;
    property OriginX: Integer read FOriginX write FOriginX;
    property OriginY: Integer read FOriginY write FOriginY;
    property PageBounds: TRect read FPageBounds write FPageBounds;
    property PageClientBounds: TRect read FPageClientBounds write FPageClientBounds;
  end;

  { TdxParagraphFrameHorizontalPositionCalculator }

  TdxParagraphFrameHorizontalPositionCalculator = class
  strict private
    FUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  protected
    function CalculateAlignBounds(const ALocation: IdxParagraphFrameLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): TRect;
    function ValidateFloatingObjectX(AActualWidth: Integer; const APageBounds: TRect; AValue: Integer): Integer;
    function CalculateAbsoluteFloatingObjectXCore(AHorizontalPositionType: TdxParagraphFrameHorizontalPositionType;
      AOffsetX: Integer; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
    function CalculateAbsoluteFloatingObjectHorizontalAlignmentPosition(AAlignment: TdxParagraphFrameHorizontalPositionAlignment;
      const AAlignBounds: TRect; AActualWidth: Integer): Integer; virtual;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
    function CalculateAbsoluteFloatingObjectX(const ALocation: IdxParagraphFrameLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualWidth: Integer): Integer; virtual;
    function CalculateFloatingObjectOffsetX(AHorizontalPositionType: TdxParagraphFrameHorizontalPositionType; X: Integer;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer; virtual;
  end;

  { TdxParagraphFrameVerticalPositionCalculator }

  TdxParagraphFrameVerticalPositionCalculator = class
  strict private
    FUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  protected
    function ValidateY(Y: Integer; AActualHeight: Integer; const ATargetBounds: TRect): Integer; virtual;
    function CalculateAlignBounds(const ALocation: IdxParagraphFrameLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): TRect;
    function CalculateAbsoluteParagraphFrameVerticalAlignmentPosition(AAlignment: TdxParagraphFrameVerticalPositionAlignment;
      const AAlignBounds: TRect; AActualHeight: Integer): Integer; virtual;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
    function CalculateAbsoluteFloatingObjectY(const ALocation: IdxParagraphFrameLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualHeight: Integer): Integer; virtual;
    function CalculateFloatingObjectOffsetY(AVerticalPositionType: TdxParagraphFrameVerticalPositionType; Y: Integer;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer; virtual;
    function CalculateAbsoluteFloatingObjectYCore(AType: TdxParagraphFrameVerticalPositionType; AOffsetY: Integer;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
  end;

  { TdxFloatingObjectSizeAndPositionController }

  TdxFloatingObjectSizeAndPositionController = class(TdxFloatingObjectSizeController)
  private
    FRowsController: TdxRowsController;
    function GetCurrentColumn: TdxColumn;
    function GetColumnController: TdxCustomColumnController;
    function GetHorizontalPositionController: TdxCurrentHorizontalPositionController;
    function GetCurrentRow: TdxRow;
    function GetCurrentHorizontalPosition: Integer;
  protected
    function CalculateAbsoluteFloatingObjectBounds(const ALocation: IdxFloatingObjectLocation): TRect; override;
    function CalculateAbsoluteParagraphFrameBounds(const ALocation: IdxParagraphFrameLocation): TRect; override;
    function CalculatePlacementInfo(const ALocation: IdxFloatingObjectLocation): TdxFloatingObjectTargetPlacementInfo;
    function CalculateAbsoluteFloatingObjectX(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualWidth: Integer): Integer; virtual;
    function CalculateAbsoluteFloatingObjectY(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualHeight: Integer): Integer; virtual;
    function CalculateAbsoluteFloatingObjectWidth(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer; virtual;
    function CalculateAbsoluteFloatingObjectHeight(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer; virtual;

    function CalculateAbsoluteParagraphFrameX(const ALocation: IdxParagraphFrameLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualWidth: Integer): Integer; virtual;
    function CalculateAbsoluteParagraphFrameY(const ALocation: IdxParagraphFrameLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualHeight: Integer): Integer; virtual;

    function ValidateRotatedShapeHorizontalPosition(const AShapeBounds: TRect; AProperties: TdxFloatingObjectProperties): TRect; override;
    function ValidateRotatedShapeVerticalPosition(const AShapeBounds: TRect; AProperties: TdxFloatingObjectProperties): TRect; override;

    property RowsController: TdxRowsController read FRowsController;
    property CurrentColumn: TdxColumn read GetCurrentColumn;
    property ColumnController: TdxCustomColumnController read GetColumnController;
    property HorizontalPositionController: TdxCurrentHorizontalPositionController read GetHorizontalPositionController;
    property CurrentRow: TdxRow read GetCurrentRow;
    property CurrentHorizontalPosition: Integer read GetCurrentHorizontalPosition;
  public
    constructor Create(ARowsController: TdxRowsController);
  end;

  { TdxFloatingObjectHorizontalPositionCalculator }

  TdxFloatingObjectHorizontalPositionCalculator = class
  private
    FUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    function CalculateAbsoluteFloatingObjectWidthCore(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
    function GetPercentBaseWidth(AFrom: TdxFloatingObjectRelativeFromHorizontal;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
    function CalculateFloatingObjectOffsetPercentBase(AType: TdxFloatingObjectHorizontalPositionType;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
    function CalculateAbsoluteFloatingObjectXCore(AHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
      AOffsetX: Integer; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
  protected
    function CalculateAlignBounds(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): TRect;
    function CalculateAbsoluteFloatingObjectHorizontalAlignmentPosition(
      AAlignment: TdxFloatingObjectHorizontalPositionAlignment; AAlignBounds: TRect; AActualWidth: Integer): Integer; virtual;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
    function CalculateAbsoluteFloatingObjectWidth(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer; virtual;
    function CalculateAbsoluteFloatingObjectX(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualWidth: Integer): Integer; virtual;
    function CalculateFloatingObjectOffsetX(AHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; X: Integer;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer; virtual;
  end;

  { TdxFloatingObjectVerticalPositionCalculator }

  TdxFloatingObjectVerticalPositionCalculator = class
  private
    FUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    function CalculateAbsoluteFloatingObjectHeightCore(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
    function GetPercentBaseHeight(AFrom: TdxFloatingObjectRelativeFromVertical;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
    function CalculateFloatingObjectOffsetPercentBase(AType: TdxFloatingObjectVerticalPositionType;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
    function CalculateAbsoluteFloatingObjectYCore(AType: TdxFloatingObjectVerticalPositionType; AOffsetY: Integer;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
  protected
    function ValidateY(Y, AActualHeight: Integer; const ATargetBounds: TRect): Integer; virtual;
    function CalculateAlignBounds(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): TRect;
    function CalculateAbsoluteFloatingObjectVerticalAlignmentPosition(
      AAlignment: TdxFloatingObjectVerticalPositionAlignment; const AAlignBounds: TRect; AActualHeight: Integer): Integer; virtual;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);

    function CalculateAbsoluteFloatingObjectHeight(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer; virtual;
    function CalculateAbsoluteFloatingObjectY(const ALocation: IdxFloatingObjectLocation;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualHeight: Integer): Integer; virtual;
    function CalculateFloatingObjectOffsetY(AVerticalPositionType: TdxFloatingObjectVerticalPositionType; Y: Integer;
      const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer; virtual;
  end;

  { TdxFloatingObjectsLayout }

  TdxFloatingObjectsLayout = class
  strict private
    FItems: TdxFloatingObjectBoxList;
    FForegroundItems: TdxFloatingObjectBoxList;
    FBackgroundItems: TdxFloatingObjectBoxList;
    FRuns: TdxList<TdxFloatingObjectAnchorRun>;
    FObjectsTable: TDictionary<TdxFloatingObjectAnchorRun, TdxFloatingObjectBox>;
    FObjectToRunMapTable: TDictionary<TdxFloatingObjectBox, TdxFloatingObjectAnchorRun>;
    function FindLeftMostX(AProcessedObjects: TdxFloatingObjectBoxList; AInitialX: Integer; const ABounds: TRect): Integer;
    function FindRightMostX(AProcessedObjects: TdxFloatingObjectBoxList; AInitialX: Integer; const ABounds: TRect): Integer;
  strict private class var
    FHorizontalObjectComparer: TdxBoxBaseComparer;
    FVerticalObjectComparer: TdxBoxBaseComparer;
    class constructor Initialize;
    class destructor Finalize;
  protected
    procedure Add(AFloatingObject: TdxFloatingObjectBox); overload;
    procedure ClearFloatingObjects(ARunIndex: TdxRunIndex; AObjects: TdxFloatingObjectBoxList); overload;

    property Runs: TdxList<TdxFloatingObjectAnchorRun> read FRuns;
  public
    constructor Create;
    destructor Destroy; override;
    function ContainsRun(AObjectAnchorRun: TdxFloatingObjectAnchorRun): Boolean;
    function GetFloatingObject(AObjectAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectBox;
    procedure Add(AObjectAnchorRun: TdxFloatingObjectAnchorRun; AFloatingObject: TdxFloatingObjectBox); overload;
    function GetObjectsInRectangle(const ABounds: TRect): TdxFloatingObjectBoxList; overload;
    function GetAllObjectsInRectangle(const ABounds: TRect): TdxFloatingObjectBoxList;
    procedure GetObjectsInRectangle(AWhere: TdxFloatingObjectBoxList; ATo: TdxFloatingObjectBoxList; const ABounds: TRect); overload;
    procedure ProcessFloatingObject(AFloatingObject: TdxFloatingObjectBox; AProcessedObjects: TdxFloatingObjectBoxList;
      AResult: TdxTextAreaCollectionEx; const AInitialBounds: TRect);
    procedure Clear;
    procedure ClearFloatingObjects(ARunIndex: TdxRunIndex); overload;
    function GetFloatingObjects(APieceTable: TdxPieceTable): TdxFloatingObjectBoxList; overload;
    procedure GetFloatingObjects(AWhere: TdxFloatingObjectBoxList; APieceTable: TdxPieceTable;
      AObjects: TdxFloatingObjectBoxList); overload;
    procedure MoveFloatingObjectsVertically(ADeltaY: Integer; APieceTable: TdxPieceTable);

    property Items: TdxFloatingObjectBoxList read FItems;
    property ForegroundItems: TdxFloatingObjectBoxList read FForegroundItems;
    property BackgroundItems: TdxFloatingObjectBoxList read FBackgroundItems;
  end;

  { TdxMaxWidthCalculator }

  TdxMaxWidthCalculator = class
  public
    function GetMaxWidth(APage: TdxPage): Integer; overload;
    function GetMaxWidth(AAreas: TdxPageAreaCollection): Integer; overload;
    function GetMaxWidth(ARows: TdxRowCollection): Integer; overload;

    function GetMaxRight(ARows: TdxRowCollection): Integer;
    function GetLastNonLineBreakBox(ARow: TdxRow): TdxBox;
    function GetLastNonLineBreakOrSpaceBox(ARow: TdxRow): TdxBox;
  end;

  { TdxParagraphFinalFormatter }

  TdxParagraphFinalFormatter = class
  strict private
    FPieceTable: TdxPieceTable;
    FCurrentParagraph: TdxSimpleParagraph;
    FCurrentColumn: TdxColumn;
    FLineSpacingCalculator: TdxLineSpacingCalculatorBase;
    FAlignmentCalculator: TdxParagraphAlignmentCalculatorBase;
    FUnderlineCalculator: TdxUnderlineCalculator;
    FStrikeoutCalculator: TdxStrikeoutCalculator;
    FBookmarkCalculator: TdxBookmarkBoxCalculator;
    FRangePermissionCalculator: TdxRangePermissionBoxCalculator;
    FCustomMarkCalculator: TdxCustomMarkBoxCalculator;
    FDocumentLayout: TdxDocumentLayout;
    FRangePermissionColorer: TdxRangePermissionColorer;
    function GetDocumentModel: TdxDocumentModel; inline;
    procedure SetPieceTable(const Value: TdxPieceTable);
  protected
    procedure FormateTextBoxes(AList: TdxFloatingObjectBoxList);
    procedure FormatTextBox(ABox: TdxFloatingObjectBox);
    procedure FormatCore(ARow: TdxRow);
    procedure SubstituteFont(ABox: TdxLayoutDependentTextBox);

  protected
    function CreateCommentHighlightAreaCore(ARow: TdxRow; AStart: TdxRunIndex; AEnd: TdxRunIndex;
      const ATightRowBounds: TRect; AComment: TdxComment): TdxHighlightArea;
    procedure CalculateCommentHighlight(ARow: TdxRow; const ATightRowBounds: TRect);
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout);
    destructor Destroy; override;

    procedure FormatPages(APages: TdxPageCollection);
    procedure FormatPage(APage: TdxPage);
    procedure FormatPageArea(APageArea: TdxPageArea);
    procedure FormatColumn(AColumn: TdxColumn);
    procedure Format(ARows: TdxRowCollection); overload;
    procedure Format(ARow: TdxRow); overload;
    procedure AlignLineNumberToBaseLine(ABox: TdxLineNumberBox);
    procedure CalculateCustomMarkBounds(ARow: TdxRow);
    procedure UpdateLayoutDependentBox(ARow: TdxRow);
    function CalcBoxDescent(ABox: TdxBox): Integer;
    function CalcBoxAscentAndFree(ABox: TdxBox): Integer;
    function CalcBaseBoxDescent(ABox: TdxBox): Integer;
    function CalcBaseBoxAscentAndFree(ABox: TdxBox): Integer;
    function CalculateSubscriptOrSuperScriptOffset(ABox: TdxBox): Integer;
    procedure UpdateCurrentParagraph(AParagraph: TdxSimpleParagraph);
    function CalculateTightRowBounds(ARow: TdxRow): TRect;
    procedure CalculateRangePermissionHighlight(ARow: TdxRow; const ATightRowBounds: TRect);
    procedure CalculateFieldsHighlight(ARow: TdxRow; const ATightRowBounds: TRect);
    function LookupFirstFieldIndexByRunIndex(ARow: TdxRow; ARunIndex: TdxRunIndex): Integer;
    function CreateFieldHighlightAreaCore(ARow: TdxRow; AStart, AEnd: TdxRunIndex; const ATightRowBounds: TRect; AColor: TdxAlphaColor): TdxHighlightArea;
    function CreateRangePermissionHighlightAreaCore(ARow: TdxRow; AStart, AEnd: TdxRunIndex; const ATightRowBounds: TRect; ARangePermission: TdxRangePermission): TdxHighlightArea;
    function FindBoxIndex(ABoxes: TdxBoxCollection; AFrom: Integer; AIndexToSearch: TdxRunIndex): Integer;
    procedure CalculateBackgroundHighlight(ARow: TdxRow; const ATightRowBounds: TRect);
    procedure CalculateHiddenTextBoxes(ARow: TdxRow);
    procedure CalculateSpecialTextBoxes(ARow: TdxRow);
    procedure FormatParagraphFrames(AParagraphFrames: TdxParagraphFrameBoxCollection);
    procedure FormatParagraphFrame(AParagraphFrameBox: TdxParagraphFrameBox; ANextParagraphFrameBox: TdxParagraphFrameBox);
    function CalculateBottomRow(ARows: TdxRowCollection; AParagraphIndex: TdxParagraphIndex; ACellRow: TdxTableCellRow): TdxRow;
    function CalculateBottomPosition(AFrameBox, ANextFrameBox: TdxParagraphFrameBox; ARow: TdxRow; ACellRow: TdxTableCellRow): Integer;
    function ShouldMergeParagraphFrameBoxes(AParagraphFrameBox: TdxParagraphFrameBox; ANextParagraphFrameBox: TdxParagraphFrameBox; ACellRow: TdxTableCellRow): Boolean;
    function CalculateRowBoxesTop(ARow: TdxRow): Integer;
    function CalculateRowBoxesBottom(ARow: TdxRow): Integer;
    function FindRowByParagraphIndex(ARows: TdxRowCollection; AParagraphIndex: TdxParagraphIndex): Integer;
    procedure ApplyTableCellsVerticalContentAlignment(ATables: TdxTableViewInfoCollection); overload;
    procedure ApplyTableCellsVerticalContentAlignment(ATable: TdxTableViewInfo); overload;
    procedure ApplyTableCellVerticalContentAlignment(ACell: TdxTableCellViewInfo);
    procedure ApplyTableCellsVerticalContentAlignment(AInnerTables: TdxTableViewInfoCollection; AFirstRowIndex, ALastRowIndex, AOffset: Integer); overload;
    function CalculateContentOffset(ACell: TdxTableCellViewInfo; AContentHeight: Integer): Integer; overload;
    function CalculateContentOffset(AVertialAlignment: TdxVerticalAlignment; AMaxContentHeight, AContentHeight: Integer): Integer; overload;
    function CalculateCellContentVerticalBounds(ACell: TdxTableCellViewInfo; AFirstRow, ALastRow: TdxRow): TRect;
    function CalculateCellRowsVerticalBounds(ACell: TdxTableCellViewInfo; AFirstRow, ALastRow: TdxRow): TRect;
    function CalculateCellInnerTablesVerticalBounds(ATableViewInfoCollection: TdxTableViewInfoCollection): TRect;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read FPieceTable write SetPieceTable;
    property BookmarkCalculator: TdxBookmarkBoxCalculator read FBookmarkCalculator;
    property CustomMarkCalculator: TdxCustomMarkBoxCalculator read FCustomMarkCalculator;
    property RangePermissionCalculator: TdxRangePermissionBoxCalculator read FRangePermissionCalculator;
    property RangePermissionColorer: TdxRangePermissionColorer read FRangePermissionColorer;
  end;

  { TdxLineSpacingCalculatorBase }

  TdxLineSpacingCalculatorBase = class abstract
  protected
    procedure AlignRowBoxesToBaseLine(AFormatter: TdxParagraphFinalFormatter; ARow: TdxRow; ACount: Integer; const AAscents: TArray<Integer>); virtual;
    function CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight, ARowTextSpacing, AMaxPictureHeight: Integer): Integer; virtual; abstract;
    function CreateParametersCalculator(AFormatter: TdxParagraphFinalFormatter; AParameterCount: Integer): TdxRowSpacingParametersCalculatorBase; virtual;
    function GetDefaultRowHeight: Integer; virtual;
    function GetRowContentBoxes(ARow: TdxRow): TdxBoxList; virtual;
  public
    class function GetLineSpacingCalculator(AParagraph: TdxSimpleParagraph): TdxLineSpacingCalculatorBase; static;
    function CalculateSpacing(ARowHeight, AMaxAscentAndFree, AMaxDescent, AMaxPictureHeight: Integer): Integer; virtual;
    function CalculateSpacingCore(ARowHeight: Integer): Integer; virtual; abstract;
    function CalcRowHeight(AOldRowHeight, ANewBoxHeight: Integer): Integer; virtual;
    function CalcRowHeightFromInlineObject(AOldRowHeight, AMaxPictureHeight, AMaxDescent: Integer): Integer; virtual;
    procedure FormatRow(AFormatter: TdxParagraphFinalFormatter; ARow: TdxRow); virtual;

    property DefaultRowHeight: Integer read GetDefaultRowHeight;
  end;
  TdxLineSpacingCalculatorClass = class of TdxLineSpacingCalculatorBase;

  { TdxSingleSpacingCalculator }

  TdxSingleSpacingCalculator = class(TdxLineSpacingCalculatorBase)
  public
    function CalculateSpacingCore(ARowHeight: Integer): Integer; override;
    function CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight, ARowTextSpacing, AMaxPictureHeight: Integer): Integer; override;
  end;

  { TdxMultipleSpacingCalculator }

  TdxMultipleSpacingCalculator = class(TdxLineSpacingCalculatorBase)
  private
    FMultiplier: Single;
  protected
    function CreateParametersCalculator(AFormatter: TdxParagraphFinalFormatter; AParameterCount: Integer): TdxRowSpacingParametersCalculatorBase; override;
  public
    constructor Create(AMultiplier: Single);
    function CalculateSpacingCore(ARowHeight: Integer): Integer; override;
    function CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight, ARowTextSpacing, AMaxPictureHeight: Integer): Integer; override;
  end;

  { TdxSesquialteralSpacingCalculator }

  TdxSesquialteralSpacingCalculator = class(TdxLineSpacingCalculatorBase)
  protected
    function CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight, ARowTextSpacing, AMaxPictureHeight: Integer): Integer; override;
  public
    function CalculateSpacingCore(ARowHeight: Integer): Integer; override;
  end;

  { TdxDoubleSpacingCalculator }

  TdxDoubleSpacingCalculator = class(TdxLineSpacingCalculatorBase)
  protected
    function CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight, ARowTextSpacing, AMaxPictureHeight: Integer): Integer; override;
  public
    function CalculateSpacingCore(ARowHeight: Integer): Integer; override;
  end;

  { TdxExactlySpacingCalculator }

  TdxExactlySpacingCalculator = class(TdxLineSpacingCalculatorBase)
  private
		FSpacing: Integer;
		FBaseLineOffset: Integer;
    function CalcBaseLineOffset: Integer;
  protected
    function CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight, ARowTextSpacing, AMaxPictureHeight: Integer): Integer; override;
    function CreateParametersCalculator(AFormatter: TdxParagraphFinalFormatter; AParameterCount: Integer): TdxRowSpacingParametersCalculatorBase; override;
  public
    constructor Create(ASpacing: Integer);
    function CalculateSpacing(ARowHeight, AMaxAscent, AMaxDescent, AMaxPictureHeight: Integer): Integer; override;
    function CalculateSpacingCore(ARowHeight: Integer): Integer; override;
    function CalcRowHeight(AOldRowHeight, ANewBoxHeight: Integer): Integer; override;
    function CalcRowHeightFromInlineObject(AOldRowHeight, AMaxPictureHeight, AMaxDescent: Integer): Integer; override;

    property DefaultRowHeight read FSpacing;
  end;

  { TdxAtLeastSpacingCalculator }

  TdxAtLeastSpacingCalculator = class(TdxLineSpacingCalculatorBase)
  private
    FSpacing: Integer;
  protected
    function GetDefaultRowHeight: Integer; override;
  protected
    constructor Create(ASpacing: Integer);
    function CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight, ARowTextSpacing, AMaxPictureHeight: Integer): Integer; override;
    function CreateParametersCalculator(AFormatter: TdxParagraphFinalFormatter; AParameterCount: Integer): TdxRowSpacingParametersCalculatorBase; override;
  public
    function CalculateSpacing(ARowHeight, AMaxAscentAndFree, AMaxDescent, AMaxPictureHeight: Integer): Integer; override;
    function CalculateSpacingCore(ARowHeight: Integer): Integer; override;
  end;

  { TdxRowSpacingParametersCalculatorBase }

  TdxRowSpacingParametersCalculatorBase = class abstract
  private
    FMaxParameterValue: Integer;
    FFormatter: TdxParagraphFinalFormatter;
    FMaxMarkParameterValue: Integer;
    FMaxSpaceParameterValue: Integer;
    FParameterValues: TArray<Integer>;
  protected
    function IsSpaceBox(ABox: TdxBox): Boolean;
    function IsMarkBox(ABox: TdxBox): Boolean;
    procedure ProcessParameterValue(ABox: TdxBox; AValue: Integer); virtual;
    function CalculateActualMaxParameterValue: Integer; virtual;
  public
    constructor Create(AFormatter: TdxParagraphFinalFormatter; AParameterCount: Integer);

    procedure ProcessBox(ABox: TdxBox; ABoxIndex: Integer); virtual; abstract;
    function CalculateRowBaseLineOffset(ARow: TdxRow): Integer; virtual; abstract;

    property ParameterValues: TArray<Integer> read FParameterValues;
    property MaxParameterValue: Integer read FMaxParameterValue write FMaxParameterValue;
    property MaxSpaceParameterValue: Integer read FMaxSpaceParameterValue write FMaxSpaceParameterValue;
    property MaxMarkParameterValue: Integer read FMaxMarkParameterValue write FMaxMarkParameterValue;
    property Formatter: TdxParagraphFinalFormatter read FFormatter;
  end;

  { TdxRowSpacingParametersCalculator }

  TdxRowSpacingParametersCalculator = class(TdxRowSpacingParametersCalculatorBase)
  public
    procedure ProcessBox(ABox: TdxBox; ABoxIndex: Integer); override;
    function CalculateRowBaseLineOffset(ARow: TdxRow): Integer; override;
  end;

  { TdxDescentParametersCalculator }

  TdxDescentParametersCalculator = class(TdxRowSpacingParametersCalculatorBase)
  public
    procedure ProcessBox(ABox: TdxBox; ABoxIndex: Integer); override;
    function CalculateRowBaseLineOffset(ARow: TdxRow): Integer; override;
  end;

  { TdxMultipleSpacingParametersCalculator }

  TdxMultipleSpacingParametersCalculator = class(TdxRowSpacingParametersCalculator)
  strict private
    FDescentParametersCalculator: TdxDescentParametersCalculator;
    FMultiplier: Single;
  public
    constructor Create(AFormatter: TdxParagraphFinalFormatter; AParameterCount: Integer; AMultiplier: Single);
    destructor Destroy; override;
    procedure ProcessBox(ABox: TdxBox; ABoxIndex: Integer); override;
    function CalculateRowBaseLineOffset(ARow: TdxRow): Integer; override;
  end;

  { TdxExactlyRowSpacingParametersCalculator }

  TdxExactlyRowSpacingParametersCalculator = class(TdxRowSpacingParametersCalculatorBase)
  private
    FBaseLineOffset: Integer;
  public
    constructor Create(AFormatter: TdxParagraphFinalFormatter; AParameterCount, ABaseLineOffset: Integer);
    procedure ProcessBox(ABox: TdxBox; ABoxIndex: Integer); override;
    function CalculateRowBaseLineOffset(ARow: TdxRow): Integer; override;
  end;

  { TdxAtLeastRowSpacingParametersCalculator }

  TdxAtLeastRowSpacingParametersCalculator = class(TdxRowSpacingParametersCalculatorBase)
  private
    FMaxPictureHeight: Integer;
    function CalculateRowBaseLineOffsetCore(ARow: TdxRow): Integer;
  public
    procedure ProcessBox(ABox: TdxBox; ABoxIndex: Integer); override;
    function CalculateRowBaseLineOffset(ARow: TdxRow): Integer; override;
  end;

  { TdxCharacterLineCalculator }

  TdxCharacterLineCalculator<T: record> = class abstract
  public const
    UnknownScript = TdxCharacterFormattingScript(-1);
  strict private
    FBoxCount: Integer;
    FBoxes: TdxBoxCollection;
    FComparer: IEqualityComparer<T>;
    FCurrentRunCharacterLineType: T;
    FCurrentScript: TdxCharacterFormattingScript;
    FNumberingListBox: TdxNumberingListBox;
    FNumberingListBoxesCount: Integer;
    FOwnsBoxes: Boolean;
    FPieceTable: TdxPieceTable;
    FRow: TdxRow;
    FUseForWordsOnly: Boolean;
    function GetDocumentModel: TdxDocumentModel;
    procedure SetPieceTable(const AValue: TdxPieceTable);
    procedure SetBoxes(const Value: TdxBoxCollection);
  protected
    function GetCharacterLineTypeNone: T; virtual; abstract;
    procedure StartCharacterLine(AStartAnchorIndex: Integer); virtual; abstract;
    procedure EndCharacterLine(AEndAnchorIndex: Integer; ACurrentFontInfo: TdxFontInfo); virtual; abstract;
    function IsCharacterLineBreak(ACharacterLineTypeChanged: Boolean; ARunChanged: Boolean;
      ABoxScript: TdxCharacterFormattingScript): Boolean; virtual; abstract;
    procedure AfterCalculate; virtual;
    procedure BeforeCalculate(ARow: TdxRow); virtual;
    procedure Initialize(ARow: TdxRow); virtual;
    procedure AddBoxToCharacterLine(ABox: TdxBox); virtual;
    procedure CalculateCharacterLineBoxesByType(ALastNonSpaceBoxIndex: Integer); virtual;
    function SetCurrentRunCharacterLineTypeFromNumberingListBox: Boolean; virtual;
    function SetCurrentRunCharacterLineTypeFromBox(ABox: TdxBox; APrevBoxIndex: TdxRunIndex): Boolean; virtual;
    function GetRunCharacterLineType(ARun: TdxTextRun; ARunIndex: TdxRunIndex): T; virtual; abstract;
    function GetRunCharacterLineUseForWordsOnly(ARun: TdxTextRun; ARunIndex: TdxRunIndex): Boolean; virtual; abstract;
    function GetNumberingListBoxCharacterLineType(ANumberingListBox: TdxNumberingListBox): T; virtual; abstract;
    function GetNumberingListBoxCharacterLineUseForWordsOnly(ANumberingListBox: TdxNumberingListBox): Boolean; virtual; abstract;
    function GetBoxScript(ABoxIndex: Integer): TdxCharacterFormattingScript; virtual;
    function GetBoxFontInfo(ABoxIndex: Integer): TdxFontInfo; virtual;
    function CalculateActualCharacterLineType(ABox: TdxBox): T; virtual;
    function CalculateActualScript(AScript: TdxCharacterFormattingScript): TdxCharacterFormattingScript; virtual;
    function CalculateScriptOffset(AFontInfo: TdxFontInfo; AScript: TdxCharacterFormattingScript): Integer;

    property BoxCount: Integer read FBoxCount;
    property Boxes: TdxBoxCollection read FBoxes write SetBoxes;
    property CharacterLineTypeNone: T read GetCharacterLineTypeNone;
    property Comparer: IEqualityComparer<T> read FComparer;
    property CurrentRunCharacterLineType: T read FCurrentRunCharacterLineType write FCurrentRunCharacterLineType;
    property CurrentScript: TdxCharacterFormattingScript read FCurrentScript write FCurrentScript;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read FPieceTable write SetPieceTable;
    property Row: TdxRow read FRow write FRow;
    property UseForWordsOnly: Boolean read FUseForWordsOnly write FUseForWordsOnly;
  public
    constructor Create(APieceTable: TdxPieceTable);
    destructor Destroy; override;
    procedure Calculate(ARow: TdxRow); virtual;
    function FindLastNonSpaceBoxIndex(AStartIndex: Integer = 0): Integer;
  end;

  { TdxUnderlineInfo }

  TdxUnderlineInfo = record
    TotalUnderlineWidth: Integer;
    WeightedUnderlinePositionSum: Integer;
    WeightedUnderlineThicknessSum: Integer;
    TopOffset: Integer;
    procedure Init;
  end;

  { TdxUnderlineCalculator }

  TdxUnderlineCalculator = class(TdxCharacterLineCalculator<TdxUnderlineType>)
  strict private
    FStartAnchorIndex: Integer;
    FUnderlineBoxesByType: TdxUnderlineBoxCollection;
    FActualScript: TdxCharacterFormattingScript;
  protected
    function GetCharacterLineTypeNone: TdxUnderlineType; override;
    procedure BeforeCalculate(ARow: TdxRow); override;
    procedure AfterCalculate; override;
    function CalculateActualScript(AScript: TdxCharacterFormattingScript): TdxCharacterFormattingScript; override;
    function ShouldBreakCharacterLineByScript(AScript: TdxCharacterFormattingScript): Boolean;
    function IsCharacterLineBreak(ACharacterLineTypeChanged: Boolean; ARunChanged: Boolean; ABoxScript: TdxCharacterFormattingScript): Boolean; override;
    procedure StartCharacterLine(AStartAnchorIndex: Integer); override;
    procedure EndCharacterLine(AEndAnchorIndex: Integer; ACurrentFontInfo: TdxFontInfo); override;
    procedure AddBoxToCharacterLine(ABox: TdxBox); override;
    function CalculateUnderlineInfo(AEndAnchorIndex: Integer): TdxUnderlineInfo;
    function CalculateTabLeaderUnderlineInfo(ABox: TdxTabSpaceBox; const ABounds: TRect): TdxUnderlineInfo;
    function GetRunCharacterLineType(ARun: TdxTextRun; ARunIndex: TdxRunIndex): TdxUnderlineType; override;
    function GetRunCharacterLineUseForWordsOnly(ARun: TdxTextRun; ARunIndex: TdxRunIndex): Boolean; override;
    function GetNumberingListBoxCharacterLineType(ANumberingListBox: TdxNumberingListBox): TdxUnderlineType; override;
    function GetNumberingListBoxCharacterLineUseForWordsOnly(ANumberingListBox: TdxNumberingListBox): Boolean; override;
    procedure SplitUnderlinesByColor(AUnderlines: TdxUnderlineBoxCollection);
    procedure SplitUnderlineBoxByColor(AUnderlineBox: TdxUnderlineBox);
    procedure AddNewUnderlineBoxToRow(ASourceUnderlineBox: TdxUnderlineBox; AStartIndex: Integer; AEndIndex: Integer);
    procedure SetUnderlinesBounds;
    procedure SetUnderlineBoxBounds(AUnderlineBox: TdxUnderlineBox);
    function GetUnderlineLeft(AUnderlineBox: TdxUnderlineBox): Integer;
    function GetUnderlineRight(AUnderlineBox: TdxUnderlineBox): Integer;

    property StartAnchorIndex: Integer read FStartAnchorIndex write FStartAnchorIndex;
    property UnderlineBoxesByType: TdxUnderlineBoxCollection read FUnderlineBoxesByType write FUnderlineBoxesByType;
    property ActualScript: TdxCharacterFormattingScript read FActualScript;
  public
    constructor Create(APieceTable: TdxPieceTable);
    destructor Destroy; override;
    class function CalculateUnderlineThickness(AWeightedUnderlineThicknessSum: Integer; ATotalUnderlineWidth: Integer): Integer; static;
    class function CalculateUnderlinePosition(AWeightedUnderlinePositionSum: Integer; ATotalUnderlineWidth: Integer): Integer; static;
    function CreateTabLeaderUnderlineBox(ABox: TdxTabSpaceBox; const ABounds: TRect; ARow: TdxRow): TdxUnderlineBox;
    procedure CenterTabLeaderUnderlineBoxVertically(AUnderlineBox: TdxUnderlineBox; const ABounds: TRect);
  end;

  { TdxStrikeoutCalculator }

  TdxStrikeoutCalculator = class(TdxCharacterLineCalculator<TdxStrikeoutType>)
  strict private
    FStartAnchorIndex: Integer;
  protected
    function GetCharacterLineTypeNone: TdxStrikeoutType; override;
    procedure BeforeCalculate(ARow: TdxRow); override;
    function IsCharacterLineBreak(ACharacterLineTypeChanged: Boolean; ARunChanged: Boolean; ABoxScript: TdxCharacterFormattingScript): Boolean; override;
    procedure StartCharacterLine(AStartAnchorIndex: Integer); override;
    function CalculateStrikeoutBoxTop(AFontInfo: TdxFontInfo): Integer; virtual;
    procedure EndCharacterLine(AEndAnchorIndex: Integer; AFontInfo: TdxFontInfo); override;
    function GetRunCharacterLineType(ARun: TdxTextRun; ARunIndex: TdxRunIndex): TdxStrikeoutType; override;
    function GetRunCharacterLineUseForWordsOnly(ARun: TdxTextRun; ARunIndex: TdxRunIndex): Boolean; override;
    function GetNumberingListBoxCharacterLineType(ANumberingListBox: TdxNumberingListBox): TdxStrikeoutType; override;
    function GetNumberingListBoxCharacterLineUseForWordsOnly(ANumberingListBox: TdxNumberingListBox): Boolean; override;

    property StartAnchorIndex: Integer read FStartAnchorIndex write FStartAnchorIndex;
  end;


implementation

uses
  RTLConsts, Math,
  dxTypeHelpers,
  cxGeometry,

  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.FastComparer,
  dxRichEdit.Utils.TextColors,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.DocumentLayout.BottomTextIndentCalculator,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.Printing;

type
  TRectangularFrameXComparer = class(TdxBoxBaseComparer)
  public
    function Compare(const X, Y: TdxBoxBase): Integer; override;
  end;
  TRectangularFrameYComparer = class(TdxBoxBaseComparer)
  public
    function Compare(const X, Y: TdxBoxBase): Integer; override;
  end;

  THorizontalObjectComparer = class(TdxBoxBaseComparer)
  public
    function Compare(const ALeft, ARight: TdxBoxBase): Integer; override;
  end;

  TVerticalObjectComparer = class(TdxBoxBaseComparer)
  public
    function Compare(const ALeft, ARight: TdxBoxBase): Integer; override;
  end;

  { TdxTextAreaAndXComparable }

  TdxTextAreaAndXComparable = class(TcxIUnknownObject, IdxComparable<TdxTextArea>)
  private
    FX: Integer;
  public
    constructor Create(X: Integer);
    function CompareTo(const AOther: TdxTextArea): Integer;
  end;

  { TdxTableCellLeftComparable }

  TdxTableCellLeftComparable = class(TcxIUnknownObject, IdxComparable<TdxTableCellViewInfo>)
  private
    FPos: Integer;
  public
    constructor Create(APos: Integer);
    function CompareTo(const ACell: TdxTableCellViewInfo): Integer;
  end;

  TdxTableViewInfoHelper = class helper for TdxTableViewInfo
  private
    function GetAssociatedFloatingObjectsLayout: TdxFloatingObjectsLayout;
    procedure SetAssociatedFloatingObjectsLayout(const Value: TdxFloatingObjectsLayout);
  public
    property AssociatedFloatingObjectsLayout: TdxFloatingObjectsLayout read GetAssociatedFloatingObjectsLayout write SetAssociatedFloatingObjectsLayout;
  end;

  { TdxRowParagraphComparer }

  TdxRowParagraphComparer = class(TcxIUnknownObject, IdxComparable<TdxBoxBase>)
  strict private
    FIndex: TdxParagraphIndex;
  protected
    //IdxComparable<TdxRow>
    function CompareTo(const Value: TdxBoxBase): Integer;
  public
    constructor Create(AIndex: TdxParagraphIndex);
  end;

  { TTableViewInfoAndStartRowSeparatorIndexComparer }

  TTableViewInfoAndStartRowSeparatorIndexComparer = class(TcxIUnknownObject, IdxComparable<TdxTableViewInfo>)
  private
    FRowSeparatorIndex: Integer;
  public
    constructor Create(ARowSeparatorIndex: Integer);
    function CompareTo(const Value: TdxTableViewInfo): Integer;
  end;

  { TTableViewInfoAndEndRowSeparatorIndexComparer }

  TTableViewInfoAndEndRowSeparatorIndexComparer = class(TcxIUnknownObject, IdxComparable<TdxTableViewInfo>)
  private
    FRowSeparatorIndex: Integer;
  public
    constructor Create(ARowSeparatorIndex: Integer);
    function CompareTo(const Value: TdxTableViewInfo): Integer;
  end;

{ TdxTableViewInfoHelper }

function TdxTableViewInfoHelper.GetAssociatedFloatingObjectsLayout: TdxFloatingObjectsLayout;
begin
  Result := TdxFloatingObjectsLayout(FAssociatedFloatingObjectsLayout);
end;

procedure TdxTableViewInfoHelper.SetAssociatedFloatingObjectsLayout(const Value: TdxFloatingObjectsLayout);
begin
  FAssociatedFloatingObjectsLayout := Value;
end;

{ TdxRowParagraphComparer }

constructor TdxRowParagraphComparer.Create(AIndex: TdxParagraphIndex);
begin
  inherited Create;
  FIndex := AIndex;
end;

function TdxRowParagraphComparer.CompareTo(const Value: TdxBoxBase): Integer;
begin
  Result := CompareInteger(TdxRow(Value).Paragraph.Index, FIndex);
end;


{ TdxTablesController }

constructor TdxTablesController.Create(APageController: TdxPageController; ARowsController: TdxRowsController);
begin
  inherited Create;
  FRowsController := ARowsController;
  FPageController := APageController;
  FStates := TdxTablesControllerStateBaseStack.Create;
  FStates.Push(TdxTablesControllerNoTableState.Create(Self));
  FTableBreaks := TObjectDictionary<TdxTable, TdxSortedList<TPair<Integer, Integer>>>.Create([doOwnsValues]);
  FInfinityHeights := TdxIntegersDictionary.Create;
end;

destructor TdxTablesController.Destroy;
begin
  while FStates.Count > 0 do
    FStates.Pop.Free;
  FreeAndNil(FStates);
  FreeAndNil(FInfinityHeights);
  FreeAndNil(FTableBreaks);
  inherited Destroy;
end;

type
  { TdxBreakComparer }

  TdxBreakComparer = class(TcxIUnknownObject, IdxComparable<TPair<Integer, Integer>>)
  private
    FKey: Integer;
  public
    constructor Create(AKey: Integer);
    function CompareTo(const AOther: TPair<Integer, Integer>): Integer;
  end;

  { TdxBreakComparer }

  constructor TdxBreakComparer.Create(AKey: Integer);
  begin
    inherited Create;
    FKey := AKey;
  end;

  function TdxBreakComparer.CompareTo(const AOther: TPair<Integer, Integer>): Integer;
  begin
    Result := AOther.Key - FKey;
  end;

function TdxTablesController.GetTableBreak(ATable: TdxTable; ATableViewInfoIndex: Integer;
  out ABottomBounds: Integer): TdxTableBreakType;
var
  AIndex: Integer;
  AComparer: TdxBreakComparer;
  ABreaks: TdxSortedList<TPair<Integer, Integer>>;
begin
  ABottomBounds := 0;
  if not FTableBreaks.TryGetValue(ATable, ABreaks) then
    Exit(TdxTableBreakType.NoBreak);
  AComparer := TdxBreakComparer.Create(ATableViewInfoIndex);
  try
    AIndex := ABreaks.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;
  if AIndex < 0 then
    Result := TdxTableBreakType.NoBreak
  else
  begin
    ABottomBounds := ABreaks[AIndex].Value;
    Result := TdxTableBreakType.NextPage;
  end;
end;

function TdxTablesController.IsInfinityHeight(ATableViewInfoIndex: Integer; out ABottomAnchorIndex: Integer): Boolean;
begin
  Result := FInfinityHeights.TryGetValue(ATableViewInfoIndex, ABottomAnchorIndex);
end;

type
  { TdxBreakPairComparer }

  TdxBreakPairComparer = class(TComparer<TPair<Integer, Integer>>)
  public
    function Compare(const X, Y: TPair<Integer, Integer>): Integer; override;
  end;

  function TdxBreakPairComparer.Compare(const X, Y: TPair<Integer, Integer>): Integer;
  begin
    Result := X.Key - Y.Key;
  end;

procedure TdxTablesController.AddTableBreak(ATable: TdxTable; ATableViewInfoIndex, ABottomBounds: Integer);
var
  ABreaks: TdxSortedList<TPair<Integer, Integer>>;
  AKeyIndex: Integer;
begin
  if not FTableBreaks.TryGetValue(ATable, ABreaks) then
  begin
    ABreaks := TdxSortedList<TPair<Integer, Integer>>.Create(TdxBreakPairComparer.Create);
    FTableBreaks.Add(ATable, ABreaks);
  end
  else
  begin
    AKeyIndex := ABreaks.Count - 1;
    while (AKeyIndex >= 0) and (ABreaks[AKeyIndex].Key > ATableViewInfoIndex) do
    begin
      ABreaks.Delete(AKeyIndex);
      Dec(AKeyIndex);
    end;
  end;
  ABreaks.Add(TPair<Integer, Integer>.Create(ATableViewInfoIndex, ABottomBounds));
end;

procedure TdxTablesController.AddInfinityTableBreak(ATableViewInfoIndex, ABottomAnchorIndex: Integer);
begin
  FInfinityHeights.Add(ATableViewInfoIndex, ABottomAnchorIndex);
end;

procedure TdxTablesController.RemoveAllTableBreaks;
begin
  FTableBreaks.Clear;
  FInfinityHeights.Clear;
end;

procedure TdxTablesController.BeginParagraph(AParagraph: TdxParagraph);
var
  AParagraphCell: TdxTableCell;
begin
  AParagraphCell := AParagraph.GetCell;
  State.EnsureCurrentCell(AParagraphCell);
end;

procedure TdxTablesController.StartNewTable(ANewCell: TdxTableCell);
var
  ACurrentNestedLevel: Integer;
begin
  ACurrentNestedLevel := States.Count - 1;
  if ANewCell.Table.NestedLevel > ACurrentNestedLevel then
    StartNewTable(ANewCell.Table.ParentCell);

  if ANewCell.Table.NestedLevel > 0 then
    StartInnerTable(ANewCell)
  else
    StartTopLevelTable(ANewCell);
end;

procedure TdxTablesController.StartTopLevelTable(ANewCell: TdxTableCell);
begin
  RowsController.NewTableStarted;
  States.Push(TdxTablesControllerTableState.Create(Self, ANewCell, RowsController.CurrentColumn.Rows.Count = 0, SimpleView));
end;

function TdxTablesController.CanFitRowToColumn(ALastTextRowBottom: TdxLayoutUnit; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult;
begin
  Result := State.CanFitRowToColumn(ALastTextRowBottom, AColumn);
end;

function TdxTablesController.CanFitFloatingObjectInTableRow(ABottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult;
begin
  Result := State.CanFitFloatingObjectInTableRow(ABottom, AColumn);
end;

procedure TdxTablesController.BeforeMoveRowToNextColumn;
var
  I, ACount: Integer;
begin
  ACount := States.Count;
  for I := 0 to ACount - 1 do
    States.Items[I].BeforeMoveRowToNextColumn;
end;

procedure TdxTablesController.AfterMoveRowToNextColumn;
var
  I, ACount: Integer;
begin
  ACount := States.Count;
  for I := 0 to ACount - 1 do
    States.Items[I].AfterMoveRowToNextColumn;
end;

procedure TdxTablesController.AfterMoveRowToNextPage;
begin
end;

procedure TdxTablesController.BeforeMoveRowToNextPage;
begin
end;

procedure TdxTablesController.StartInnerTable(ACell: TdxTableCell);
begin
  FRowsController.NewTableStarted();
  FStates.Push(TdxTablesControllerTableState.Create(Self, ACell, TdxTablesControllerTableState(State).CurrentCellViewInfoEmpty, SimpleView));
end;

procedure TdxTablesController.UpdateCurrentCellBottom(ABottom: TdxLayoutUnit);
var
  I, ACount: Integer;
begin
  ACount := States.Count;
  for I := 0 to ACount - 1 do
    States.Items[I].UpdateCurrentCellBottom(ABottom);
end;

procedure TdxTablesController.UpdateCurrentCellHeight;
var
  I, ACount: Integer;
begin
  ACount := States.Count;
  for I := 0 to ACount - 1 do
    States.Items[I].UpdateCurrentCellHeight(RowsController.CurrentRow);
end;

procedure TdxTablesController.ReturnToPrevState;
begin
  States.Pop.Free;
end;

procedure TdxTablesController.LeaveCurrentTableCore;
var
  ATableState: TdxTablesControllerTableState;
begin
  ATableState := TdxTablesControllerTableState(State);
  ATableState.LeaveCurrentTable(False, False);
  ReturnToPrevState;
  if States.Count = 1 then
    RowsController.ColumnController.PageAreaController.PageController.EndTableFormatting;
end;

procedure TdxTablesController.LeaveCurrentTable(ANextCell: TdxTableCell);
begin
  LeaveCurrentTableCore;
  State.EnsureCurrentCell(ANextCell);
end;

procedure TdxTablesController.ClearInvalidatedContent;
begin
  while States.Count > 0 do
    States.Pop.Free;
  States.Push(TdxTablesControllerNoTableState.Create(Self));
end;

procedure TdxTablesController.Reset;
begin
  ClearInvalidatedContent;
end;

procedure TdxTablesController.EndParagraph(ALastRow: TdxRow);
var
  I: Integer;
begin
  for I := States.Count - 1 downto 0 do
    States[I].EndParagraph(ALastRow);
end;

function TdxTablesController.GetIsInsideTable: Boolean;
begin
  Result := States.Count > 1;
end;

function TdxTablesController.GetState: TdxTablesControllerStateBase;
begin
  Result := States.Peek;
end;

procedure TdxTablesController.OnCurrentRowFinished;
var
  I: Integer;
begin
  for I := 0 to States.Count - 1 do
    States[I].OnCurrentRowFinished;
end;

function TdxTablesController.RollbackToStartOfRowTableOnFirstCellRowColumnOverfull: TdxParagraphIndex;
var
  I, ACount: Integer;
  AFirstTableRowViewInfoInColumn: Boolean;
  AParagraphIndex: TdxParagraphIndex;
begin
  ACount := FStates.Count;
  AFirstTableRowViewInfoInColumn := False;
  if ACount > 1 then
    AFirstTableRowViewInfoInColumn := TdxTablesControllerTableState(State).IsFirstTableRowViewInfoInColumn;
  for I := ACount - 1 downto 0 do
  begin
    AParagraphIndex := States.Items[I].RollbackToStartOfRowTableOnFirstCellRowColumnOverfull(
      AFirstTableRowViewInfoInColumn, I = ACount - 1);
    if AParagraphIndex >= 0 then
      Exit(AParagraphIndex);
  end;
  Assert(False);
  Result := 0;
end;

function TdxTablesController.GetCurrentCell: TdxTableCell;
begin
  if FStates.Count <= 1 then
    Result := nil
  else
    Result := TdxTablesControllerTableState(FStates.Peek).GetCurrentCell;
end;

function TdxTablesController.CreateTableViewInfoManager(AParentTableViewInfoManager: TdxTableViewInfoManager;
  APageController: TdxPageController; ARowsController: TdxRowsController): TdxTableViewInfoManager;
begin
  Result := TdxTableViewInfoManager.Create(AParentTableViewInfoManager, APageController, ARowsController);
end;

function TdxTablesController.GetSimpleView: Boolean;
begin
  Result := False;
end;

{ TdxTablesControllerStateBaseStack }

function TdxTablesControllerStateBaseStack.GetItem(Index: Integer): TdxTablesControllerStateBase;
begin
  Result := List[Index]
end;

function TdxTablesControllerStateBaseStack.Peek: TdxTablesControllerStateBase;
begin
  Result := inherited Peek;
end;

function TdxTablesControllerStateBaseStack.Pop: TdxTablesControllerStateBase;
begin
  Result := inherited Pop;
end;

function TdxTablesControllerStateBaseStack.Push(AItem: TdxTablesControllerStateBase): TdxTablesControllerStateBase;
begin
  Result := inherited Push(AItem);
end;

{ TdxTablesControllerStateBase }

constructor TdxTablesControllerStateBase.Create(ATablesController: TdxTablesController);
begin
  inherited Create;
  Assert(ATablesController <> nil);
  FTablesController := ATablesController;
end;

destructor TdxTablesControllerStateBase.Destroy;
begin
  inherited Destroy;
end;

{ TdxTablesControllerNoTableState }

procedure TdxTablesControllerNoTableState.AfterMoveRowToNextColumn;
begin
end;

procedure TdxTablesControllerNoTableState.BeforeMoveRowToNextColumn;
begin
end;

function TdxTablesControllerNoTableState.CanFitRowToColumn(ALastTextRowBottom: Integer;
  AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult;
var
  AColumnBottom: Integer;
begin
  if TablesController.RowsController.CurrentColumn.Rows.Count <= 0 then
  begin
    Result := TdxCanFitCurrentRowToColumnResult.RowFitted;
    Exit;
  end;
  AColumnBottom := AColumn.Bounds.Bottom;
  if ALastTextRowBottom <= AColumnBottom then
    Result := TdxCanFitCurrentRowToColumnResult.RowFitted
  else
    Result := TdxCanFitCurrentRowToColumnResult.PlainRowNotFitted;
end;

procedure TdxTablesControllerNoTableState.EndParagraph(ALastRow: TdxRow);
begin
end;

procedure TdxTablesControllerNoTableState.EnsureCurrentCell(ACell: TdxTableCell);
begin
  if ACell <> nil then
  begin
    TablesController.RowsController.ColumnController.PageAreaController.PageController.BeginTableFormatting;
    TablesController.StartNewTable(ACell);
  end;
end;

procedure TdxTablesControllerNoTableState.OnCurrentRowFinished;
begin
end;

function TdxTablesControllerNoTableState.RollbackToStartOfRowTableOnFirstCellRowColumnOverfull(
  AFirstTableRowViewInfoInColumn, AInnerMostTable: Boolean): TdxParagraphIndex;
begin
  Result := 0;
end;

function TdxTablesControllerNoTableState.CanFitFloatingObjectInTableRow(ABottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult;
begin
  Result := TdxCanFitCurrentRowToColumnResult.RowFitted;
end;

procedure TdxTablesControllerNoTableState.UpdateCurrentCellBottom(ABottom: TdxLayoutUnit);
begin
end;

procedure TdxTablesControllerNoTableState.UpdateCurrentCellHeight(ARow: TdxRow);
begin
end;

{ TdxTabsController }

constructor TdxTabsController.Create;
begin
  inherited Create;
  FTabStack := TdxIntegerStack.Create;
  FTabs := TdxTabFormattingInfo.Create;
end;

destructor TdxTabsController.Destroy;
begin
  FreeAndNil(FTabs);
  FreeAndNil(FTabStack);
  inherited Destroy;
end;

function TdxTabsController.AdjustAlignedTabWidth(const ATab: TdxTabInfo; ATabWidth, ALastNonSpaceBoxRight: Integer): Integer;
begin
  if IsTabPositionBehindParagraphRight(ATab) and (ALastNonSpaceBoxRight + ATabWidth > FParagraphRight) then
    Result := FParagraphRight - ALastNonSpaceBoxRight
  else
    Result := ATabWidth;
end;

procedure TdxTabsController.BeginParagraph(AParagraph: TdxParagraph);
begin
  FParagraph := AParagraph;
  FPieceTable := AParagraph.PieceTable;
  FDocumentModel := AParagraph.DocumentModel;
  ClearLastTabPosition;
  ObtainTabs;
end;

function TdxTabsController.CalcLastCenterTabWidth(const ATab: TdxTabInfo; AStartIndex: Integer): Integer;
var
  ALastNonSpaceBoxIndex, ATabbedContentWidth, ALastNonSpaceBoxRight, ATabPosition, ATabWidth: Integer;
begin
  ALastNonSpaceBoxIndex := TdxRow.FindLastNonSpaceBoxIndex(FBoxes, AStartIndex);
  ATabbedContentWidth := FBoxes[ALastNonSpaceBoxIndex].Bounds.Right - FBoxes[AStartIndex].Bounds.Left;
  ALastNonSpaceBoxRight := FBoxes[ALastNonSpaceBoxIndex].Bounds.Right;
  ATabPosition := ATab.GetLayoutPosition(documentModel.ToDocumentLayoutUnitConverter);
  ATabWidth := Trunc(ATabbedContentWidth / 2 + (FColumnLeft + ATabPosition) - ALastNonSpaceBoxRight);
  ATabWidth := AdjustAlignedTabWidth(ATab, ATabWidth, ALastNonSpaceBoxRight);
  Result := Math.Max(0, ATabWidth);
end;

function TdxTabsController.CalcLastDecimalTabWidth(const ATab: TdxTabInfo; AStartIndex: Integer;
  AFormatter: TdxParagraphBoxFormatter; ARowLeft: Integer): Integer;
var
  ALastNonSpaceBoxIndex, ADecimalPointPosition, ALastNonSpaceBoxRight, ATabPosition, ATabWidth: Integer;
begin
  ALastNonSpaceBoxIndex := TdxRow.FindLastNonSpaceBoxIndex(FBoxes, AStartIndex);
  ADecimalPointPosition := FindDecimalPointPosition(FBoxes, AStartIndex + 1, ALastNonSpaceBoxIndex, AFormatter, ARowLeft);
  if ALastNonSpaceBoxIndex >= 0 then
    ALastNonSpaceBoxRight := FBoxes[ALastNonSpaceBoxIndex].Bounds.Right
  else
    ALastNonSpaceBoxRight := ARowLeft;
  ATabPosition := ATab.GetLayoutPosition(FDocumentModel.ToDocumentLayoutUnitConverter);
  ATabWidth := (FColumnLeft + ATabPosition) - ADecimalPointPosition;
  ATabWidth := AdjustAlignedTabWidth(ATab, ATabWidth, ALastNonSpaceBoxRight);
  Result := Max(0, ATabWidth);
end;

function TdxTabsController.CalcLastRightTabWidth(const ATab: TdxTabInfo; AStartIndex: Integer): Integer;
var
  ATabPosition, ALastNonSpaceBoxIndex: Integer;
begin
  ALastNonSpaceBoxIndex := TdxRow.FindLastNonSpaceBoxIndex(FBoxes, AStartIndex);
  ATabPosition := ATab.GetLayoutPosition(FDocumentModel.ToDocumentLayoutUnitConverter);
  Result := Max(0, (FColumnLeft + ATabPosition) - FBoxes[ALastNonSpaceBoxIndex].Bounds.Right);
end;

function TdxTabsController.CalcLastTabWidth(ARow: TdxRow; AFormatter: TdxParagraphBoxFormatter): Integer;
var
  AIndex, ATabWidth: Integer;
  ABox: TdxTabSpaceBox;
  ATab: TdxTabInfo;
  R: TRect;
begin
  if FTabStack.Count <= 0 then
    Exit(0);
  FBoxes := ARow.Boxes;

  AIndex := FTabStack.Pop;
  if AIndex >= 0 then
    ABox := TdxTabSpaceBox(FBoxes[AIndex])
  else
    ABox := nil;
  if ABox <> nil then
    ATab := ABox.TabInfo
  else
    ATab := FTabs[0];
  case ATab.Alignment of
    TdxTabAlignmentType.Right:
      ATabWidth := CalcLastRightTabWidth(ATab, AIndex);
    TdxTabAlignmentType.Center:
      ATabWidth := CalcLastCenterTabWidth(ATab, AIndex);
    TdxTabAlignmentType.Decimal:
      ATabWidth := CalcLastDecimalTabWidth(ATab, AIndex, AFormatter, ARow.Bounds.Left);
  else
    Exit(0);
  end;
  if ABox <> nil then
  begin
    R := ABox.Bounds;
    R.Width := ATabWidth;
    ABox.Bounds := R;
    ABox.LeaderCount := CalculateLeaderCount(ABox, FPieceTable);
    CacheLeader(ABox, AFormatter);
  end
  else
    ARow.TextOffset := ARow.TextOffset + ATabWidth;
  Result := ATabWidth;
end;

class function TdxTabsController.CalculateLeaderCount(ABox: TdxTabSpaceBox; APieceTable: TdxPieceTable): Integer;
begin
  if ABox.TabInfo.Leader = TdxTabLeaderType.None then
    Result := -1
  else
    Result := ABox.Bounds.Width div Max(1, GetTabLeaderCharacterWidth(ABox, APieceTable));
end;

class function TdxTabsController.GetTabLeaderCharacterWidth(ABox: TdxTabSpaceBox; APieceTable: TdxPieceTable): Integer;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := ABox.GetFontInfo(APieceTable);
  case ABox.TabInfo.Leader of
    TdxTabLeaderType.MiddleDots:
      Result := AFontInfo.MiddleDotWidth;
    TdxTabLeaderType.Hyphens:
      Result := AFontInfo.DashWidth;
    TdxTabLeaderType.EqualSign:
      Result := AFontInfo.EqualSignWidth;
    TdxTabLeaderType.ThickLine,
    TdxTabLeaderType.Underline:
      Result := AFontInfo.UnderscoreWidth;
    else
      Result := AFontInfo.DotWidth;
  end;
end;

class procedure TdxTabsController.CacheLeader(ABox: TdxTabSpaceBox; AFormatter: TdxParagraphBoxFormatter);
var
  AText: string;
  AFontInfo: TdxFontInfo;
  AMeasurer: TdxBoxMeasurer;
  ATextInfo: TdxTextViewInfo;
begin
  try
    if ABox.LeaderCount > 0 then
    begin
      AText := StringOfChar(GetTabLeaderCharacter(ABox.TabInfo.Leader), ABox.LeaderCount);
      AFontInfo := ABox.GetFontInfo(AFormatter.PieceTable);
      AMeasurer := AFormatter.Measurer;
      if AMeasurer.TextViewInfoCache.TryGetTextViewInfo(AText, AFontInfo) = nil then
      begin
        ATextInfo := AMeasurer.CreateTextViewInfo(nil, AText, AFontInfo);
        AMeasurer.TextViewInfoCache.AddTextViewInfo(AText, AFontInfo, ATextInfo);
      end;
    end;
  except
  end;
end;

class function TdxTabsController.GetTabLeaderCharacter(ALeaderType: TdxTabLeaderType): Char;
begin
  case ALeaderType of
    TdxTabLeaderType.MiddleDots:
      Result := TdxCharacters.MiddleDot;
    TdxTabLeaderType.Hyphens:
      Result := TdxCharacters.Dash;
    TdxTabLeaderType.EqualSign:
      Result := TdxCharacters.EqualSign;
    TdxTabLeaderType.ThickLine,
    TdxTabLeaderType.Underline:
      Result := TdxCharacters.Underscore;
    else
      Result := TdxCharacters.Dot;
  end;
end;

procedure TdxTabsController.ClearLastTabPosition;
begin
  FTabStack.Clear;
end;

procedure TdxTabsController.UpdateLastTabPosition(ABoxesCount: Integer);
begin
  if (FTabStack.Count > 0) and (FTabStack.Peek >= ABoxesCount) then
    FTabStack.Pop;
end;

function TdxTabsController.FindDecimalPointPosition(ABoxes: TdxBoxCollection; AFrom, ATo: Integer;
  AFormatter: TdxParagraphBoxFormatter; ARowLeft: Integer): Integer;
var
  ABoxIndex, AOffset: Integer;
  ABox: TdxBox;
  ABoxInfo: TdxBoxInfo;
  AEndPos: TdxFormatterPosition;
begin
  if not TryFindDecimalSeparator(ABoxes, AFrom, ATo, ABoxIndex, AOffset) then
    if ATo >= 0 then
      Exit(ABoxes[ATo].Bounds.Right)
    else
      Exit(ARowLeft);

  ABox := ABoxes[ABoxIndex];

  if AOffset = 0 then
    Exit(ABox.Bounds.Left);

  ABoxInfo := TdxBoxInfo.Create;
  try
    ABoxInfo.StartPos := ABox.StartPos;
    AEndPos := TdxFormatterPosition.Create(ABox.EndPos.RunIndex, ABox.StartPos.Offset + AOffset - 1, 0);
    ABoxInfo.EndPos := AEndPos;
    AFormatter.Measurer.MeasureText(ABoxInfo);
    Result := ABox.Bounds.Left + ABoxInfo.Size.Width;
  finally
    ABoxInfo.Free;
  end;
end;

function TdxTabsController.GetNextTab(APos: Integer): TdxTabInfo;
var
  ANextTab: TdxNullableValue<TdxTabInfo>;
  ADefaultTabWidth, ATabNumber: Integer;
begin
  ANextTab := FTabs.FindNextTab(APos);
  if not ANextTab.IsNull then
    Exit(ANextTab.Value);

  ADefaultTabWidth := FDocumentModel.DocumentProperties.DefaultTabWidth;
  if ADefaultTabWidth <= 0 then
    Result := TdxTabInfo.CreateDefault(APos)
  else
  begin
    ATabNumber := (APos div ADefaultTabWidth) + 1;
    Result := TdxTabInfo.CreateDefault(ATabNumber * ADefaultTabWidth)
  end;
end;

function TdxTabsController.IsTabPositionBehindParagraphRight(const ATab: TdxTabInfo): Boolean;
var
  ATabPosition: Integer;
begin
  ATabPosition := ATab.GetLayoutPosition(FDocumentModel.ToDocumentLayoutUnitConverter);
  Result := ATabPosition + FColumnLeft <= FParagraphRight;
end;

procedure TdxTabsController.ObtainTabs;
var
  ATempTabs, AAutomaticTabAtHangingIndent: TdxTabFormattingInfo;
begin
  FreeAndNil(FTabs);
  if (FParagraph.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging) and not FParagraph.IsInList then
  begin
    AAutomaticTabAtHangingIndent := TdxTabFormattingInfo.Create;
    try
      AAutomaticTabAtHangingIndent.Add(TdxTabInfo.CreateDefault(FParagraph.LeftIndent));
      ATempTabs := FParagraph.GetTabs;
      try
        FTabs := TdxTabFormattingInfo.Merge(ATempTabs, AAutomaticTabAtHangingIndent);
      finally
        FreeAndNil(ATempTabs);
      end;
    finally
      FreeAndNil(AAutomaticTabAtHangingIndent);
    end;
  end
  else
    FTabs := FParagraph.GetTabs;

  SingleDecimalTabInTable := (FTabs.Count = 1) and (FTabs[0].Alignment = TdxTabAlignmentType.Decimal) and FParagraph.IsInCell;
end;

procedure TdxTabsController.SaveLastTabPosition(APos: Integer);
begin
  FTabStack.Push(APos);
end;

function TdxTabsController.IsDecimalSeparator(const ASource: string; AIndex: Integer): Boolean;
begin
  Result := IsSubstringStarts(ASource, AIndex, DocumentModel.NumberDecimalSeparator);
end;

function TdxTabsController.IsGroupSeparator(const ASource: string; AIndex: Integer): Boolean;
begin
  Result := IsSubstringStarts(ASource, AIndex, DocumentModel.NumberGroupSeparator);
end;

function TdxTabsController.IsSubstringStarts(const ASource: string; AIndex: Integer; const ASubstring: string): Boolean;
var
  S: string;
begin
  S := Copy(ASource, AIndex, Length(ASubstring));
  Result := CompareText(S, ASubstring) = 0;
end;

function TdxTabsController.TryFindDecimalSeparator(ABoxes: TdxBoxCollection; AFrom, ATo: Integer;
  out ABoxIndex: Integer; out AOffset: Integer): Boolean;
var
  ANumberStarted: Boolean;
  AText: string;
  ALength: Integer;
begin
  ABoxIndex := AFrom;
  AOffset := 0;
  ANumberStarted := False;
  while ABoxIndex <= ATo do
  begin
    AText := ABoxes[ABoxIndex].GetText(FPieceTable);
    ALength := Length(AText);
    AOffset := 0;
    while AOffset < ALength do
    begin
      if IsDecimalSeparator(AText, AOffset + 1) then
        Exit(True);

      if dxWideIsNumeric(AText[AOffset + 1]) then
        ANumberStarted := True
      else
        if ANumberStarted and not IsGroupSeparator(AText, AOffset + 1) then
          Exit(True);
      Inc(AOffset);
    end;
    Inc(ABoxIndex);
  end;
  Result := False;
end;

{ TdxTableCellColumnController }

constructor TdxTableCellColumnController.Create(AParent: TdxCustomColumnController; ACurrentParentColumn: TdxColumn; ALeft,
  ATop, AWidth: Integer; ATableViewInfo: TdxTableViewInfo; ACurrentCell: TdxTableCell);
begin
  inherited Create;
  Assert(AParent <> nil);
  Assert(ACurrentParentColumn <> nil);
  Parent := AParent;
  FColumns := TdxBoxList.Create;
  CurrentParentColumn := ACurrentParentColumn;
  FCurrentCell := ACurrentCell;
  FGeneratedColumns := TdxColumnList.Create;
  FGeneratedColumns.Add(ACurrentParentColumn);
  StartNewCell(ACurrentParentColumn, ALeft, ATop, AWidth, ACurrentCell);
  FGeneratedTableViewInfo := TdxReferencedObjectList<TdxTableViewInfo>.Create;
  FGeneratedTableViewInfo.Add(ATableViewInfo);
end;

destructor TdxTableCellColumnController.Destroy;
begin
  CurrentParentColumn := nil;
  FreeAndNil(FColumns);
  TdxTableCellRow.Release(FLastCreatedRow);
  FreeAndNil(FGeneratedTableViewInfo);
  FreeAndNil(FGeneratedColumns);
  inherited Destroy;
end;

function TdxTableCellColumnController.CreateRow: TdxRow;
begin
  TdxTableCellRow.Release(FLastCreatedRow);
  FLastCreatedRow := TdxTableCellRow.Create(FCurrentCellViewInfo);
  TdxTableCellRow.AddReference(FLastCreatedRow);
  Result := FLastCreatedRow;
end;

procedure TdxTableCellColumnController.AddInnerTable(ATableViewInfo: TdxTableViewInfo);
begin
  CurrentCell.AddInnerTable(ATableViewInfo);
end;

procedure TdxTableCellColumnController.RemoveGeneratedColumn(AColumn: TdxColumn);
var
  AInitialColumnCount: Integer;
begin
  AInitialColumnCount := FGeneratedColumns.Count;
  if AInitialColumnCount > 1 then
  begin
    FGeneratedColumns.Delete(AInitialColumnCount - 1);
    if FCurrentColumnIndex >= FGeneratedColumns.Count then
    begin
      FCurrentColumnIndex := FGeneratedColumns.Count - 1;
      CurrentParentColumn := FGeneratedColumns[FCurrentColumnIndex];
    end;
    if FParent is TdxColumnController then
      TdxColumnController(FParent).RemoveGeneratedColumn(AColumn);
  end;
end;

function TdxTableCellColumnController.GetPreviousColumn(AColumn: TdxColumn): TdxColumn;
begin
  Result := nil;
end;

function TdxTableCellColumnController.CompleteCurrentColumnFormatting(AColumn: TdxColumn): TdxCompleteFormattingResult;
begin
  if FCurrentColumnIndex + 1 >= FGeneratedColumns.Count then
    Result := FParent.CompleteCurrentColumnFormatting(AColumn)
  else
    Result := TdxCompleteFormattingResult.Success;
end;

procedure TdxTableCellColumnController.ResetToFirstColumn;
begin
  FParent.ResetToFirstColumn;
end;

function TdxTableCellColumnController.GetNextColumn(AColumn: TdxColumn; AKeepFloatingObjects: Boolean): TdxColumn;
var
  ANewColumn: TdxColumn;
  ABounds: TRect;
begin
  Inc(FCurrentColumnIndex);
  if (FCurrentColumnIndex >= FGeneratedColumns.Count) then
  begin
    ANewColumn := FParent.GetNextColumn(FCurrentParentColumn, AKeepFloatingObjects);
    FGeneratedColumns.Add(ANewColumn);
  end
  else
    if FParent is TdxTableCellColumnController then
      TdxTableCellColumnController(FParent).MoveToNextColumn;
  CurrentParentColumn := FGeneratedColumns[FCurrentColumnIndex];
  ABounds := FCurrentParentColumn.Bounds;
  Result := TdxTableCellColumn.Create(FCurrentParentColumn, FCurrentCell);

  Result.Bounds := TRect.CreateSize(ABounds.Left + FLeft, ABounds.Top, FWidth, ABounds.Height);
end;

function TdxTableCellColumnController.GetPageAreaController: TdxPageAreaController;
begin
  Result := Parent.PageAreaController;
end;

procedure TdxTableCellColumnController.MoveToNextColumn;
begin
  Inc(FCurrentColumnIndex);
  if FParent is TdxTableCellColumnController then
    TdxTableCellColumnController(FParent).MoveToNextColumn();
  CurrentParentColumn := FGeneratedColumns[FCurrentColumnIndex];
end;

function TdxTableCellColumnController.GetStartColumn: TdxColumn;
var
  ABounds: TRect;
begin
  ABounds := FGeneratedColumns[FCurrentColumnIndex].Bounds;
  Result := TdxTableCellColumn.Create(FGeneratedColumns[FCurrentColumnIndex], FCurrentCell);
  Result.Bounds := TRect.CreateSize(ABounds.Left + FLeft, FTop, FWidth, ABounds.Bottom - FTop);
end;

procedure TdxTableCellColumnController.SetCurrentParentColumn(AColumn: TdxColumn);
begin
  FCurrentColumnIndex := FGeneratedColumns.IndexOf(AColumn);
  CurrentParentColumn := AColumn;
  if Parent is TdxTableCellColumnController then
    TdxTableCellColumnController(Parent).SetCurrentParentColumn(TdxTableCellColumn(AColumn).Parent);
end;

procedure TdxTableCellColumnController.StartNewCell(ACurrentParentColumn: TdxColumn; ALeft, ATop, AWidth: Integer;
  ACurrentCell: TdxTableCell);
begin
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FCurrentCell := ACurrentCell;
  SetCurrentParentColumn(ACurrentParentColumn);
end;

function TdxTableCellColumnController.GetMaxAnchor(AAnchor1, AAnchor2: TdxTableCellVerticalAnchor): TdxTableCellVerticalAnchor;
var
  ACellBordersInfo: TdxHorizontalCellBordersInfoList;
begin
  ACellBordersInfo := AAnchor1.CellBorders.Clone;
  if AAnchor1.VerticalPosition > AAnchor2.VerticalPosition then
    Result := TdxTableCellVerticalAnchor.Create(AAnchor1.VerticalPosition, AAnchor1.BottomTextIndent, ACellBordersInfo)
  else
    Result := TdxTableCellVerticalAnchor.Create(AAnchor2.VerticalPosition, AAnchor2.BottomTextIndent, ACellBordersInfo);
end;

procedure TdxTableCellColumnController.AddTableViewInfo(ACurrentTableViewInfo: TdxTableViewInfo);
begin
  FGeneratedTableViewInfo.Add(ACurrentTableViewInfo);
end;

procedure TdxTableCellColumnController.RemoveTableViewInfo(ATableViewInfo: TdxTableViewInfo);
begin
  FGeneratedTableViewInfo.Remove(ATableViewInfo);
end;

function TdxTableCellColumnController.GetCurrentPageBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect;
begin
  Result := ACurrentColumn.Bounds;
end;

function TdxTableCellColumnController.GetCurrentPageClientBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect;
begin
  Result := ACurrentColumn.Bounds;
end;

function TdxTableCellColumnController.GetPageLastRunIndex: TdxRunIndex;
begin
  Result := PageAreaController.PageController.PageLastRunIndex;
end;

function TdxTableCellColumnController.GetShouldZeroSpacingBeforeWhenMoveRowToNextColumn: Boolean;
begin
  Result := False;
end;

function TdxTableCellColumnController.GetTopLevelColumnsCount: Integer;
begin
  Result := Parent.TopLevelColumnsCount;
end;

procedure TdxTableCellColumnController.SetCurrentTableCellViewInfo(AValue: TdxTableCellViewInfo);
begin
  if FLastCreatedRow <> nil then
    FLastCreatedRow.CellViewInfo := AValue;
  FCurrentCellViewInfo := AValue;
end;

function TdxTableCellColumnController.GetMeasurer: TdxBoxMeasurer;
begin
  Result := FParent.Measurer;
end;

function TdxTableCellColumnController.GetParentColumn: TdxColumn;
begin
  Result := FGeneratedColumns[FCurrentColumnIndex];
end;

function TdxTableCellColumnController.GetLastParentColumn: TdxColumn;
begin
  Result := FGeneratedColumns[FGeneratedColumns.Count - 1];
end;

function TdxTableCellColumnController.GetFirstParentColumn: TdxColumn;
begin
  Result := FGeneratedColumns[0];
end;

function TdxTableCellColumnController.GetCurrentTopLevelColumn: TdxColumn;
begin
  if FParent is TdxTableCellColumnController then
    Result := TdxTableCellColumnController(FParent).CurrentTopLevelColumn
  else
    Result := ParentColumn;
end;

function TdxTableCellColumnController.GetViewInfo: TdxTableViewInfo;
begin
  if FCurrentColumnIndex < FGeneratedTableViewInfo.Count then
    Result := FGeneratedTableViewInfo[FCurrentColumnIndex]
  else
    Result := nil;
end;

procedure TdxTableCellColumnController.InternalSetCurrentParentColumn(const Value: TdxColumn);
begin
  if Value <> FCurrentParentColumn then
  begin
    FCurrentParentColumn := Value;
    if FCurrentParentColumn <> nil then
      FColumns.Add(FCurrentParentColumn);
  end;
end;

procedure TdxTableCellColumnController.SetParent(AValue: TdxCustomColumnController);
begin
  if AValue <> FParent then
  begin
    if FParent <> nil then
      FParent.RemoveChild(Self);
    FParent := AValue;
    if FParent <> nil then
      FParent.AddChild(Self);
  end;
end;

{ TdxLayoutGridRectangle }

constructor TdxLayoutGridRectangle.Create(const ABounds: TRect; ARowIndex, AColumnIndex, AColumnSpan: Integer);
begin
  Assert(AColumnIndex >= 0);
  Assert(AColumnSpan > 0);
  Bounds := ABounds;
  RowIndex := ARowIndex;
  ColumnIndex := AColumnIndex;
  ColumnSpan := AColumnSpan;
  RowSpan := 0;
end;

{ TdxTableViewInfoManager }

constructor TTableViewInfoAndStartRowSeparatorIndexComparer.Create(ARowSeparatorIndex: Integer);
begin
  inherited Create;
  FRowSeparatorIndex := ARowSeparatorIndex;
end;

function TTableViewInfoAndStartRowSeparatorIndexComparer.CompareTo(const Value: TdxTableViewInfo): Integer;
var
  ADiff: Integer;
begin
  ADiff := Value.TopRowIndex - FRowSeparatorIndex;
  if ADiff <> 0 then
    Exit(ADiff);
  if Value.PrevTableViewInfo = nil then
    Result := 0
  else
    Result := Ord(Value.PrevTableViewInfo.BottomRowIndex = FRowSeparatorIndex);
end;

constructor TTableViewInfoAndEndRowSeparatorIndexComparer.Create(ARowSeparatorIndex: Integer);
begin
  inherited Create;
  FRowSeparatorIndex := ARowSeparatorIndex;
end;

function TTableViewInfoAndEndRowSeparatorIndexComparer.CompareTo(const Value: TdxTableViewInfo): Integer;
var
  ABottomSeparatorIndex, ATopSeparatorIndex: Integer;
begin
  ABottomSeparatorIndex := Value.BottomRowIndex + 1;
  if ABottomSeparatorIndex < FRowSeparatorIndex then
    Exit(-1);
  ATopSeparatorIndex := Value.TopRowIndex + 1;
  if ATopSeparatorIndex > FRowSeparatorIndex then
    Exit(1);
  if ABottomSeparatorIndex = FRowSeparatorIndex then
  begin
    if (Value.NextTableViewInfo <> nil) and (Value.NextTableViewInfo.TopRowIndex = Value.BottomRowIndex) then
      Result := -1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

{ TdxTableCellLeftComparable }

constructor TdxTableCellLeftComparable.Create(APos: Integer);
begin
  inherited Create;
  FPos := APos;
end;

function TdxTableCellLeftComparable.CompareTo(const ACell: TdxTableCellViewInfo): Integer;
begin
  Result := ACell.Left - FPos;
end;

{ TdxTableViewInfos }

constructor TdxTableViewInfos.Create;
begin
  inherited Create;
end;

destructor TdxTableViewInfos.Destroy;
begin
  inherited Destroy;
end;

{ TdxTableViewInfoManager }

constructor TdxTableViewInfoManager.Create(AParentTableViewInfoManager: TdxTableViewInfoManager;
  APageController: TdxPageController; ARowsController: TdxRowsController);
begin
  inherited Create;
  FTableViewInfos := TdxTableViewInfos.Create;
  FCurrentCellBottoms := TdxIntegerList.Create;
  FRowsController := ARowsController;
  FPageController := APageController;
  FParentTableViewInfoManager := AParentTableViewInfoManager;
end;

destructor TdxTableViewInfoManager.Destroy;
begin
  FreeAndNil(FTableViewInfos);
  FreeAndNil(FCurrentCellBottoms);
  TdxTableCellColumnController.Release(FColumnController);
  inherited Destroy;
end;

procedure TdxTableViewInfoManager.BeforeStartNextCell(ACell: TdxTableCell; const AGridBounds: TdxLayoutGridRectangle);
var
  AStartTableViewInfoIndex: Integer;
begin
  AStartTableViewInfoIndex := GetStartTableViewInfoIndexByRowSeparatorIndex(AGridBounds.RowIndex);
  if AStartTableViewInfoIndex <> FCurrentTableViewInfoIndex then
  begin
    FCurrentTableViewInfoIndex := AStartTableViewInfoIndex;
    if FParentTableViewInfoManager <> nil then
    begin
      Assert(CurrentTableViewInfo.ParentTableCellViewInfo <> nil);
      FParentTableViewInfoManager.SetCurrentTableCellViewInfo(CurrentTableViewInfo.ParentTableCellViewInfo);
    end;
    SetCurrentParentColumn(CurrentTableViewInfo);
  end;
end;

function TdxTableViewInfoManager.StartNewTable(ATable: TdxTable; ACellBounds: TDictionary<TdxTableCell, TdxLayoutGridRectangle>;
  ALeftOffset, ARightCellMargin: TdxLayoutUnit; AFirstContentInParentCell: Boolean;
  const ATableTextArea: TdxTextArea; out AMaxRight: TdxLayoutUnit): TdxTableViewInfo;
var
  AColumnWidth, AMaxTableWidth, APercentBaseWidth: Integer;
  ATableViewInfo: TdxTableViewInfo;
  AWidthsCalculator: TdxTableWidthsCalculator;
  ACalculator: TdxTableGridCalculator;
begin
  AColumnWidth := ATableTextArea.Width;
  AMaxTableWidth := AColumnWidth - ALeftOffset + ARightCellMargin;
  APercentBaseWidth := AColumnWidth;
  AMaxRight := APercentBaseWidth;
  if (ATable.NestedLevel = 0) and not RowsController.MatchHorizontalTableIndentsToTextEdge then
  begin
    Inc(APercentBaseWidth, ARightCellMargin - ALeftOffset);
    if ATable.TableAlignment = TdxTableRowAlignment.Center then
      AMaxRight := APercentBaseWidth;
  end;
  AMaxRight := APercentBaseWidth;
  AWidthsCalculator := TdxTableWidthsCalculator.Create(TdxPieceTable(ATable.PieceTable), RowsController.ColumnController.Measurer, AMaxTableWidth);
  try
    ACalculator := RowsController.CreateTableGridCalculator(TdxDocumentModel(ATable.DocumentModel), AWidthsCalculator, AMaxTableWidth);
    try
      FTableGrid := ACalculator.CalculateTableGrid(ATable, APercentBaseWidth);
    finally
      ACalculator.Free;
    end;
  finally
    AWidthsCalculator.Free;
  end;
  ATableViewInfo := CreateTableViewInfo(ATable, 0, AFirstContentInParentCell, nil);
  FCurrentTableViewInfoIndex := AddTableViewInfo(ATableViewInfo);
  FTable := ATable;
  Result := CurrentTableViewInfo;
end;

function TdxTableViewInfoManager.GetTableGrid: TdxTableGrid;
begin
  Result := FTableGrid;
end;

function TdxTableViewInfoManager.GetTableViewInfos: TdxList<TdxTableViewInfo>;
begin
  Result := FTableViewInfos;
end;

procedure TdxTableViewInfoManager.SetRowSeparator(ARowSeparatorIndex: Integer; AAnchor: TdxTableCellVerticalAnchor);
var
  ATableViewInfo: TdxTableViewInfo;
begin
  ATableViewInfo := GetStartTableViewInfoByRowSeparatorIndex(ARowSeparatorIndex);
  SetRowSeparatorCore(ATableViewInfo, ARowSeparatorIndex, AAnchor);
end;

procedure TdxTableViewInfoManager.SetRowSeparatorForCurrentTableViewInfo(ARowSeparatorIndex: Integer; AAnchor: TdxTableCellVerticalAnchor);
var
  AActualRowSeparatorIndex: Integer;
begin
  AActualRowSeparatorIndex := Min(CurrentTableViewInfo.TopRowIndex + CurrentTableViewInfo.Anchors.Count, ARowSeparatorIndex);
  if AActualRowSeparatorIndex - CurrentTableViewInfo.TopRowIndex > CurrentTableViewInfo.Rows.Count then
  begin
    AAnchor.Free;
    Exit;
  end;
  SetRowSeparatorCore(CurrentTableViewInfo, AActualRowSeparatorIndex, AAnchor);
end;

procedure TdxTableViewInfoManager.EnsureTableRowViewInfo(ATableViewInfo: TdxTableViewInfo; ARow: TdxTableRow; ARowIndex: Integer);
begin
  if ARowIndex < ATableViewInfo.TopRowIndex + ATableViewInfo.Rows.Count then
    Exit;
  Assert(ARowIndex = ATableViewInfo.TopRowIndex + ATableViewInfo.Rows.Count);
  ATableViewInfo.Rows.Add(CreateTableRowViewInfo(ATableViewInfo, ARow, ARowIndex));
end;

procedure TdxTableViewInfoManager.LeaveCurrentTable(ABeforeRestart: Boolean);
var
  ATableViewInfosCount: Integer;
  ALastTableViewInfo: TdxTableViewInfo;
begin
  if not ABeforeRestart then
  begin
    ATableViewInfosCount := FTableViewInfos.Count;
    Assert(ATableViewInfosCount > 0);
    ALastTableViewInfo := FTableViewInfos[ATableViewInfosCount - 1];
    if FParentTableViewInfoManager <> nil then
    begin
      FParentTableViewInfoManager.SetCurrentTableCellViewInfo(ALastTableViewInfo.ParentTableCellViewInfo);
      FParentTableViewInfoManager.SetCurrentParentColumn(ALastTableViewInfo.ParentTableCellViewInfo.TableViewInfo);
      FParentTableViewInfoManager.ValidateTopLevelColumn;
    end;
  end;
end;

procedure TdxTableViewInfoManager.RemoveAllInvalidRowsOnColumnOverfull(AFirstInvalidRowIndex: Integer);
var
  I, ALastValidTableViewInfoIndex, AFirstInvalidRowViewInfoIndex: Integer;
  ALastValidTableViewInfo, ATableViewInfo: TdxTableViewInfo;
  AShouldRemoveColumn: Boolean;
begin
  ALastValidTableViewInfoIndex := GetStartTableViewInfoIndexByRowSeparatorIndex(AFirstInvalidRowIndex);
  ALastValidTableViewInfo := FTableViewInfos[ALastValidTableViewInfoIndex];
  AFirstInvalidRowViewInfoIndex := AFirstInvalidRowIndex - ALastValidTableViewInfo.TopRowIndex;
  if AFirstInvalidRowViewInfoIndex = 0 then
    Dec(ALastValidTableViewInfoIndex);
  for I := FTableViewInfos.Count - 1 downto ALastValidTableViewInfoIndex + 1 do
  begin
    ATableViewInfo := FTableViewInfos[I];
    TdxTableViewInfo.AddReference(ATableViewInfo);
    ATableViewInfo.Column.TopLevelColumn.RemoveTableViewInfoWithContent(ATableViewInfo);
    FTableViewInfos.Delete(I);
    ColumnController.RemoveTableViewInfo(ATableViewInfo);
    if ATableViewInfo.ParentTableCellViewInfo <> nil then
      ATableViewInfo.ParentTableCellViewInfo.InnerTables.Remove(ATableViewInfo);
    AShouldRemoveColumn := ShouldRemoveColumn(I, ATableViewInfo);
    if AShouldRemoveColumn then
      ColumnController.RemoveGeneratedColumn(ATableViewInfo.Column);
    TdxTableViewInfo.Release(ATableViewInfo);
  end;
  if AFirstInvalidRowViewInfoIndex > 0 then
  begin
    ALastValidTableViewInfo.RemoveRowsFromIndex(AFirstInvalidRowViewInfoIndex);
    ALastValidTableViewInfo.NextTableViewInfo := nil;
  end;
end;

procedure TdxTableViewInfoManager.FixColumnOverflow;
var
  I, AAnchorCount: Integer;
  ALastAnchor: TdxTableCellVerticalAnchor;
  ATableViewInfo: TdxTableViewInfo;
  AVerticalAnchors: TdxTableCellVerticalAnchorCollection;
begin
  if FParentTableViewInfoManager <> nil then
    Exit;
  for I := 0 to FTableViewInfos.Count - 1 do
  begin
    ATableViewInfo := FTableViewInfos[I];
    AVerticalAnchors := ATableViewInfo.Anchors;
    AAnchorCount := ATableViewInfo.Anchors.Count;
    if AAnchorCount = 0 then
        Continue;
    ALastAnchor := AVerticalAnchors[AAnchorCount - 1];
    if ALastAnchor.VerticalPosition > ATableViewInfo.Column.Bounds.Bottom then
      ALastAnchor.VerticalPosition := ATableViewInfo.Column.Bounds.Bottom;
  end;
end;

function TdxTableViewInfoManager.IsCurrentCellViewInfoFirst: Boolean;
begin
  Result := not CurrentTableCellViewInfo.IsStartOnPreviousTableViewInfo;
end;

function TdxTableViewInfoManager.IsFirstTableRowViewInfoInColumn: Boolean;
begin
  if CurrentTableCellViewInfo.TopAnchorIndex > 0 then
    Exit(False);
  if not CurrentTableViewInfo.FirstContentInParentCell then
    Exit(False);
  if FParentTableViewInfoManager = nil then
    Result := True
  else
    Result := FParentTableViewInfoManager.IsFirstTableRowViewInfoInColumn;
end;

procedure TdxTableViewInfoManager.SetCurrentCellHasContent;
begin
  CurrentTableCellViewInfo.EmptyCell := True;
end;

function TdxTableViewInfoManager.GetCurrentTableViewInfoIndex: Integer;
begin
  Result := FCurrentTableViewInfoIndex;
end;

function TdxTableViewInfoManager.GetCurrentTopLevelTableViewInfoIndex: Integer;
begin
  if FParentTableViewInfoManager <> nil then
    Result := FParentTableViewInfoManager.GetCurrentTopLevelTableViewInfoIndex
  else
    Result := GetCurrentTableViewInfoIndex;
end;

function TdxTableViewInfoManager.IsFirstContentInParentCell: Boolean;
begin
  Result := CurrentTableViewInfo.FirstContentInParentCell;
end;

function TdxTableViewInfoManager.GetParentTableViewInfoManager: TdxTableViewInfoManager;
begin
  Result := FParentTableViewInfoManager;
end;

function TdxTableViewInfoManager.GetRowStartAnchor(ARowIndex: Integer): TdxTableCellVerticalAnchor;
var
  ATableViewInfo: TdxTableViewInfo;
begin
  ATableViewInfo := GetStartTableViewInfoByRowSeparatorIndex(ARowIndex);
  Result := GetRowStartAnchor(ATableViewInfo, ARowIndex);
end;

function TdxTableViewInfoManager.GetRowStartColumn(ARowIndex: Integer): TdxColumn;
var
  ATableViewInfo: TdxTableViewInfo;
begin
  ATableViewInfo := GetStartTableViewInfoByRowSeparatorIndex(ARowIndex);
  Result := ATableViewInfo.Column;
end;

function TdxTableViewInfoManager.GetRowStartTableViewInfo(ATableRow: TdxTableRow): TdxTableViewInfo;
var
  ARowIndex: Integer;
begin
  ARowIndex := ATableRow.Table.Rows.IndexOf(ATableRow);
  Result := GetStartTableViewInfoByRowSeparatorIndex(ARowIndex);
end;

procedure TdxTableViewInfoManager.StartNextCell(ACell: TdxTableCell; const ABounds: TRect; const AGridBounds: TdxLayoutGridRectangle; const ATextBounds: TRect);
var
  AStartTableViewInfoIndex, ALastTableViewInfoIndex, ATableViewInfoIndex: Integer;
begin
  AStartTableViewInfoIndex := GetStartTableViewInfoIndexByRowSeparatorIndex(AGridBounds.RowIndex);
  ALastTableViewInfoIndex := GetEndTableViewInfoIndexByRowSeparatorIndex(AGridBounds.RowIndex + AGridBounds.RowSpan);
  CurrentTableCellViewInfo := AddFirstCellViewInfo(FTableViewInfos[AStartTableViewInfoIndex], ACell, ABounds,
    AGridBounds, ATextBounds);
  for ATableViewInfoIndex := AStartTableViewInfoIndex + 1 to ALastTableViewInfoIndex - 1 do
    AddMiddleCellViewInfo(FTableViewInfos[ATableViewInfoIndex], ACell, ABounds, AGridBounds, ATextBounds);
  if ALastTableViewInfoIndex > AStartTableViewInfoIndex then
    AddLastCellViewInfo(FTableViewInfos[ALastTableViewInfoIndex], ACell, ABounds, AGridBounds, ATextBounds);

  ValidateTopLevelColumn;
end;

function TdxTableViewInfoManager.GetLastTableBottom: Integer;
var
  ALastTableViewInfo: TdxTableViewInfo;
begin
  ALastTableViewInfo := GetLastTableViewInfo;
  Result := ALastTableViewInfo.GetTableBottom;
end;

function TdxTableViewInfoManager.GetBottomCellAnchor(ACurrentCell: TdxTableCell): TdxTableCellVerticalAnchor;
var
  ABottomAnchorIndex: Integer;
begin
  ABottomAnchorIndex := CurrentTableCellViewInfo.BottomAnchorIndex;
  Assert(ACurrentCell = CurrentTableCellViewInfo.Cell);
  Result := CurrentTableViewInfo.Anchors[ABottomAnchorIndex];
end;

function TdxTableViewInfoManager.GetBottomCellRowSeparatorIndex(ACurrentCell: TdxTableCell; ARowSpan: Integer): Integer;
begin
  Result := ACurrentCell.Table.Rows.IndexOf(ACurrentCell.Row) + ARowSpan;
end;

procedure TdxTableViewInfoManager.BeforeMoveRowToNextColumn(ACurrentCellBottom: TdxLayoutUnit);
begin
end;

procedure TdxTableViewInfoManager.AfterMoveRowToNextColumn;
var
  AVerticalBorderPositions: TdxVerticalBorderPositions;
  ACurrentCellBottom, ABottomTextIndent: TdxLayoutUnit;
  AColumnController: TdxTableCellColumnController;
  APrevTableViewInfo, ANewTableViewInfo: TdxTableViewInfo;
  ASuitableTableCellViewInfo: TdxTableCellViewInfo;
  ACount, ABottomAnchorIndex, ACurrentCellViewInfoIndex, ARowViewInfoIndex, ANewTableViewInfoIndex, ASplitAnchorIndex: Integer;
  ACells: TdxTableCellViewInfoCollection;
  ASplitAnchor: TdxTableCellVerticalAnchor;
  ABorders: TdxHorizontalCellBordersInfoList;
  ACurrentRow: TdxRow;
  ARowBounds: TRect;
  AComparable: TdxTableCellLeftComparable;
  AParentColumn: TdxColumn;
  AParentTableCellViewInfo: TdxTableCellViewInfo;
  ACurrentTopLevelColumn: TdxColumn;
begin
  ACurrentCellBottom := CurrentCellBottom;
  AColumnController := ColumnController;
  if AColumnController.ViewInfo <> nil then
  begin
    APrevTableViewInfo := CurrentTableViewInfo;
    FCurrentTableViewInfoIndex := FTableViewInfos.IndexOf(AColumnController.ViewInfo);
    ASuitableTableCellViewInfo := FindSuitableTableCellViewInfo(CurrentTableViewInfo.Cells, CurrentTableCellViewInfo.Cell);
    if FParentTableViewInfoManager = nil then
      EnsureFloatingObjectsLayoutValid(FTableViewInfos[FCurrentTableViewInfoIndex]);

    if ASuitableTableCellViewInfo <> nil then
    begin
      CurrentTableCellViewInfo := ASuitableTableCellViewInfo;
      ACount := APrevTableViewInfo.Anchors.Count;
      APrevTableViewInfo.Anchors[ACount - 1].VerticalPosition := (Max(APrevTableViewInfo.BottomAnchor.VerticalPosition, ACurrentCellBottom));
    end
    else
    begin
      ABottomAnchorIndex := CurrentTableCellViewInfo.BottomAnchorIndex;

      MoveRowAndAnchors_(APrevTableViewInfo, CurrentTableViewInfo, ABottomAnchorIndex);
      ACells := APrevTableViewInfo.Rows.Last.Cells;
      AComparable := TdxTableCellLeftComparable.Create(CurrentTableCellViewInfo.Left);
      try
        TdxAlgorithms1<TdxTableCellViewInfo>.BinarySearch(ACells, AComparable, ACurrentCellViewInfoIndex);
        Assert(ACurrentCellViewInfoIndex >= 0);
      finally
        AComparable.Free;
      end;

      CurrentTableCellViewInfo := CurrentTableViewInfo.Rows.First.Cells[ACurrentCellViewInfoIndex];
    end;
  end
  else
  begin
    ARowViewInfoIndex := CurrentTableCellViewInfo.BottomAnchorIndex - 1;
    ACells := CurrentTableViewInfo.Rows[CurrentTableCellViewInfo.BottomAnchorIndex - 1].Cells;
    AComparable := TdxTableCellLeftComparable.Create(CurrentTableCellViewInfo.Left);
    try
      TdxAlgorithms1<TdxTableCellViewInfo>.BinarySearch(ACells, AComparable, ACurrentCellViewInfoIndex);
    finally
      AComparable.Free;
    end;
    Assert(ACurrentCellViewInfoIndex >= 0);
    AVerticalBorderPositions := TdxVerticalBorderPositions.Create(
        TdxLayoutUnitSortedList(CurrentTableViewInfo.VerticalBorderPositions.InitialPositions.Clone),
        TdxLayoutUnitSortedList(CurrentTableViewInfo.VerticalBorderPositions.AlignedPosition.Clone));
    AParentColumn := AColumnController.ParentColumn;
    AParentTableCellViewInfo := GetParentTableCellViewInfo;
    ACurrentTopLevelColumn := AColumnController.CurrentTopLevelColumn;
    ANewTableViewInfo := TdxTableViewInfo.Create(FTable, ACurrentTopLevelColumn, AParentColumn,
      AVerticalBorderPositions,
      CurrentTableViewInfo.TopRowIndex + CurrentTableCellViewInfo.BottomAnchorIndex - 1,
      AParentTableCellViewInfo, True);
    if FParentTableViewInfoManager = nil then
      ANewTableViewInfo.AssociatedFloatingObjectsLayout := FPageController.FloatingObjectsLayout;
    AColumnController.Parent.AddInnerTable(ANewTableViewInfo);
    ANewTableViewInfo.LeftOffset := CurrentTableViewInfo.LeftOffset;
    ANewTableViewInfo.TextAreaOffset := CurrentTableViewInfo.LeftOffset;
    ANewTableViewInfo.ModelRelativeIndent := CurrentTableViewInfo.ModelRelativeIndent;
    ANewTableViewInfoIndex := AddTableViewInfo(ANewTableViewInfo);
    AColumnController.CurrentTopLevelColumn.Tables.Add(ANewTableViewInfo);
    AColumnController.AddTableViewInfo(ANewTableViewInfo);
    MoveRowAndAnchor(CurrentTableViewInfo, ANewTableViewInfo, ARowViewInfoIndex + 1);
    ASplitAnchorIndex := CurrentTableCellViewInfo.BottomAnchorIndex;
    ASplitAnchor := CurrentTableViewInfo.Anchors[ASplitAnchorIndex - 1];
    ABorders := GetSplitAnchorHorizontalCellBorders(ASplitAnchor);
    CurrentTableViewInfo.Anchors[ASplitAnchorIndex] := TdxTableCellVerticalAnchor.Create(ACurrentCellBottom, 0, ABorders);
    if ASplitAnchor <> nil then
      ABottomTextIndent := ASplitAnchor.BottomTextIndent
    else
      ABottomTextIndent := 0;
    ANewTableViewInfo.Anchors[0] := TdxTableCellVerticalAnchor.Create(ANewTableViewInfo.Column.Bounds.Top, ABottomTextIndent, ABorders.Clone);
    SplitCellsByAnchor(CurrentTableViewInfo, ANewTableViewInfo, ASplitAnchorIndex);
    Assert(CurrentTableCellViewInfo.BottomAnchorIndex - ASplitAnchorIndex + 1 = 1);

    Assert(ANewTableViewInfo.Rows.First.Cells[ACurrentCellViewInfoIndex].Cell = CurrentTableCellViewInfo.Cell);
    CurrentTableCellViewInfo := ANewTableViewInfo.Rows.First.Cells[ACurrentCellViewInfoIndex];
    FCurrentTableViewInfoIndex := ANewTableViewInfoIndex;
  end;
  ACurrentRow := RowsController.CurrentRow;
  ARowBounds := ACurrentRow.Bounds;
  ARowBounds.MoveToTop(CurrentTableViewInfo.TopAnchor.VerticalPosition + CurrentTableViewInfo.TopAnchor.BottomTextIndent + ACurrentRow.SpacingBefore);
  RowsController.CurrentRow.Bounds := ARowBounds;
  RowsController.HorizontalPositionController.SetCurrentHorizontalPosition(ARowBounds.Left);

  ValidateTopLevelColumn;
end;

function TdxTableViewInfoManager.GetCurrentCellTop: Integer;
begin
  Result := CurrentTableViewInfo.Anchors[CurrentTableCellViewInfo.TopAnchorIndex].VerticalPosition;
end;

function TdxTableViewInfoManager.GetCurrentCellTopAnchorIndex: Integer;
begin
  Result := CurrentTableCellViewInfo.TopAnchorIndex;
end;

function TdxTableViewInfoManager.GetCurrentCellBottomAnchorIndex: Integer;
begin
  Result := CurrentTableCellViewInfo.BottomAnchorIndex;
end;

procedure TdxTableViewInfoManager.SetColumnController(Value: TdxTableCellColumnController);
begin
  if FColumnController <> nil then
    raise Exception.Create('Can''t change ColumnController');
  if FColumnController <> Value then
  begin
    TdxTableCellColumnController.Release(FColumnController);
    FColumnController := Value;
    TdxTableCellColumnController.AddReference(FColumnController);
  end;
end;

function TdxTableViewInfoManager.GetCurrentTableViewInfo: TdxTableViewInfo;
begin
  Result := FTableViewInfos[FCurrentTableViewInfoIndex];
end;

function TdxTableViewInfoManager.GetCurrentCellBottom: Integer;
begin
  Result := FCurrentCellBottoms[FCurrentTableViewInfoIndex];
end;

procedure TdxTableViewInfoManager.SetCurrentCellBottom(Value: Integer);
begin
  FCurrentCellBottoms[FCurrentTableViewInfoIndex] := Value;
end;

procedure TdxTableViewInfoManager.InternalSetCurrentTableCellViewInfo(Value: TdxTableCellViewInfo);
begin
  FCurrentTableCellViewInfo := Value;
  ColumnController.SetCurrentTableCellViewInfo(Value);
end;

function TdxTableViewInfoManager.CreateTableViewInfo(ATable: TdxTable; ATopRowIndex: Integer; AFirstContentInParentCell: Boolean; AVerticalBorderPositions: TdxVerticalBorderPositions): TdxTableViewInfo;
var
  ACurrentController: TdxTableCellColumnController;
  ACurrentTopLevelColumn: TdxColumn;
begin
  ACurrentController := Safe<TdxTableCellColumnController>.Cast(RowsController.ColumnController);
  if ACurrentController <> nil then
    ACurrentTopLevelColumn := ACurrentController.CurrentTopLevelColumn
  else
    ACurrentTopLevelColumn := RowsController.CurrentColumn;
  Result := TdxTableViewInfo.Create(ATable, ACurrentTopLevelColumn, RowsController.CurrentColumn,
    AVerticalBorderPositions, ATopRowIndex, GetParentTableCellViewInfo, AFirstContentInParentCell);
  if FParentTableViewInfoManager = nil then
    Result.AssociatedFloatingObjectsLayout := FPageController.FloatingObjectsLayout;
  ACurrentTopLevelColumn.Tables.Add(Result);
  if ACurrentController <> nil then
    ACurrentController.AddInnerTable(Result);
end;

function TdxTableViewInfoManager.AddTableViewInfo(ATableViewInfo: TdxTableViewInfo): Integer;
var
  ALastTableViewInfo: TdxTableViewInfo;
begin
  Result := FTableViewInfos.Count;
  if Result > 0 then
  begin
    ALastTableViewInfo := FTableViewInfos[Result - 1];
    ALastTableViewInfo.NextTableViewInfo := ATableViewInfo;
    ATableViewInfo.PrevTableViewInfo := ALastTableViewInfo;
  end;
  FTableViewInfos.Add(ATableViewInfo);
  FCurrentCellBottoms.Add(0);
end;

function TdxTableViewInfoManager.GetStartTableViewInfoByRowSeparatorIndex(ARowSeparatorIndex: Integer): TdxTableViewInfo;
var
  ATableViewInfoIndex: Integer;
begin
  ATableViewInfoIndex := GetStartTableViewInfoIndexByRowSeparatorIndex(ARowSeparatorIndex);
  Result := FTableViewInfos[ATableViewInfoIndex];
end;

function TdxTableViewInfoManager.GetStartTableViewInfoIndexByRowSeparatorIndex(ARowSeparatorIndex: Integer): Integer;
var
  ATableViewInfoIndex: Integer;
  AComparer: TTableViewInfoAndStartRowSeparatorIndexComparer;
begin
  AComparer := TTableViewInfoAndStartRowSeparatorIndexComparer.Create(ARowSeparatorIndex);
  try
    if not TdxAlgorithms1<TdxTableViewInfo>.BinarySearch(FTableViewInfos, AComparer, ATableViewInfoIndex) then
      Dec(ATableViewInfoIndex);
  finally
    AComparer.Free;
  end;


  Result := ATableViewInfoIndex;
end;

function TdxTableViewInfoManager.GetEndTableViewInfoIndexByRowSeparatorIndex(ARowSeparatorIndex: Integer): Integer;
var
  ATableViewInfoIndex: Integer;
  AComparer: TTableViewInfoAndEndRowSeparatorIndexComparer;
begin
  Assert(ARowSeparatorIndex > 0);
  AComparer := TTableViewInfoAndEndRowSeparatorIndexComparer.Create(ARowSeparatorIndex);
  try
    if not TdxAlgorithms1<TdxTableViewInfo>.BinarySearch(FTableViewInfos, AComparer, ATableViewInfoIndex) then
    begin
      Assert(ATableViewInfoIndex = FTableViewInfos.Count);
      ATableViewInfoIndex := FTableViewInfos.Count - 1;
    end;
  finally
    AComparer.Free;
  end;
  Result := ATableViewInfoIndex;
end;

procedure TdxTableViewInfoManager.SetRowSeparatorCore(ATableViewInfo: TdxTableViewInfo; ARowSeparatorIndex: Integer; AAnchor: TdxTableCellVerticalAnchor);
var
  AAnchorIndex: Integer;
begin
  AAnchorIndex := ARowSeparatorIndex - ATableViewInfo.TopRowIndex;
  Assert(AAnchorIndex <= ATableViewInfo.Rows.Count);
  ATableViewInfo.Anchors[AAnchorIndex] := AAnchor;
end;

function TdxTableViewInfoManager.CreateTableRowViewInfo(ATableViewInfo: TdxTableViewInfo; ARow: TdxTableRow; ARowIndex: Integer): TdxTableRowViewInfoBase;
var
  ACellSpacing: TdxModelUnit;
begin
  ACellSpacing := TdxTablesControllerTableState.GetActualCellSpacing(ARow);
  if ACellSpacing <> 0 then
    Result := TdxTableRowViewInfoWithCellSpacing.Create(ATableViewInfo, ARowIndex, ACellSpacing)
  else
    Result := TdxTableRowViewInfoNoCellSpacing.Create(ATableViewInfo, ARowIndex);
end;

function TdxTableViewInfoManager.GetRowStartAnchorIndex(ATableViewInfo: TdxTableViewInfo; ARowIndex: Integer): Integer;
begin
  Result := ARowIndex - ATableViewInfo.TopRowIndex;
end;

function TdxTableViewInfoManager.GetRowStartAnchor(ATableViewInfo: TdxTableViewInfo; ARowIndex: Integer): TdxTableCellVerticalAnchor;
var
  AAnchorIndex: Integer;
begin
  AAnchorIndex := GetRowStartAnchorIndex(ATableViewInfo, ARowIndex);
  Result := ATableViewInfo.Anchors[AAnchorIndex] as TdxTableCellVerticalAnchor;
end;

function TdxTableViewInfoManager.AddFirstCellViewInfo(ATableViewInfo: TdxTableViewInfo; ACell: TdxTableCell;
  const ABounds: TRect; const AGridBounds: TdxLayoutGridRectangle; const ATextBounds: TRect): TdxTableCellViewInfo;
var
  ATopAnchorIndex, ABottomAnchorIndex: Integer;
  ACellViewInfo: TdxTableCellViewInfo;
  ATextOffset: TdxLayoutUnit;
begin
  ATopAnchorIndex := GetRowStartAnchorIndex(ATableViewInfo, AGridBounds.RowIndex);
  ABottomAnchorIndex := ATopAnchorIndex + AGridBounds.RowSpan;
  if (ABottomAnchorIndex >= ATableViewInfo.Anchors.Count) and (ATableViewInfo.NextTableViewInfo <> nil) then
    ABottomAnchorIndex := ATableViewInfo.Anchors.Count - 1;
  ATextOffset := ABounds.Left - AGridBounds.Bounds.Left;
  ACellViewInfo := TdxTableCellViewInfo.Create(ATableViewInfo, ACell, ABounds.Left, ABounds.Width, ATextBounds.Left + ATextOffset,
    ATextBounds.Width, ATextOffset, ATopAnchorIndex, ABottomAnchorIndex, AGridBounds.RowIndex, AGridBounds.RowSpan);
  ATableViewInfo.Cells.Add(ACellViewInfo);
  AddTableCellViewInfo(ATableViewInfo, ACellViewInfo, ATopAnchorIndex, ABottomAnchorIndex - 1);
  Result := ACellViewInfo;
end;

procedure TdxTableViewInfoManager.AddMiddleCellViewInfo(ATableViewInfo: TdxTableViewInfo; ACell: TdxTableCell;
  const ABounds: TRect; const AGridBounds: TdxLayoutGridRectangle; const ATextBounds: TRect);
var
  ATopAnchorIndex, ABottomAnchorIndex: Integer;
  ACellViewInfo: TdxTableCellViewInfo;
  ATextOffset: TdxLayoutUnit;
begin
  ATopAnchorIndex := 0;
  ABottomAnchorIndex := ATableViewInfo.Anchors.Count - 1;
  ATextOffset := ABounds.Left - AGridBounds.Bounds.Left;
  ACellViewInfo := TdxTableCellViewInfo.Create(ATableViewInfo, ACell, ABounds.Left, ABounds.Width, ATextBounds.Left + ATextOffset,
    ATextBounds.Width, ATextOffset, ATopAnchorIndex, ABottomAnchorIndex, AGridBounds.RowIndex, AGridBounds.RowSpan);
  ATableViewInfo.Cells.Add(ACellViewInfo);
  AddTableCellViewInfo(ATableViewInfo, ACellViewInfo, ATopAnchorIndex, ABottomAnchorIndex - 1);
end;

procedure TdxTableViewInfoManager.AddLastCellViewInfo(ATableViewInfo: TdxTableViewInfo; ACell: TdxTableCell;
  const ABounds: TRect; const AGridBounds: TdxLayoutGridRectangle; const ATextBounds: TRect);
var
  ATopAnchorIndex, ABottomAnchorIndex: Integer;
  ACellViewInfo: TdxTableCellViewInfo;
  ATextOffset: TdxLayoutUnit;
begin
  ATopAnchorIndex := 0;
  ABottomAnchorIndex := GetRowStartAnchorIndex(ATableViewInfo, AGridBounds.RowIndex + AGridBounds.RowSpan);
  ATextOffset := ABounds.Left - AGridBounds.Bounds.Left;
  ACellViewInfo := TdxTableCellViewInfo.Create(ATableViewInfo, ACell, ABounds.Left, ABounds.Width, ATextBounds.Left + ATextOffset,
    ATextBounds.Width, ATextOffset, ATopAnchorIndex, ABottomAnchorIndex, AGridBounds.RowIndex, AGridBounds.RowSpan);
  ATableViewInfo.Cells.Add(ACellViewInfo);
  AddTableCellViewInfo(ATableViewInfo, ACellViewInfo, ATopAnchorIndex, ABottomAnchorIndex - 1);
end;

function TdxTableViewInfoManager.GetLastTableViewInfo: TdxTableViewInfo;
begin
  Result := FTableViewInfos[FTableViewInfos.Count - 1];
end;

procedure TdxTableViewInfoManager.AddTableCellViewInfo(ATableViewInfo: TdxTableViewInfo; ACellViewInfo: TdxTableCellViewInfo;
  AStartRowViewInfoIndex, AEndRowViewInfoIndex: Integer);
var
  AComparable: TdxTableCellLeftComparable;
  ARowViewInfoIndex, ARowIndex, ACellIndex: Integer;
begin
  for ARowViewInfoIndex := AStartRowViewInfoIndex to AEndRowViewInfoIndex do
  begin
    ARowIndex := ATableViewInfo.TopRowIndex + ARowViewInfoIndex;
    EnsureTableRowViewInfo(ATableViewInfo, ATableViewInfo.Table.Rows[ARowIndex], ARowIndex);
    AComparable := TdxTableCellLeftComparable.Create(ACellViewInfo.Left);
    try
      if TdxAlgorithms1<TdxTableCellViewInfo>.BinarySearch(ATableViewInfo.Rows[ARowViewInfoIndex].Cells, AComparable, ACellIndex) then
        Assert(False);
    finally
      AComparable.Free;
    end;
    ATableViewInfo.Rows[ARowViewInfoIndex].Cells.Insert(ACellIndex, ACellViewInfo);
  end;
end;

procedure TdxTableViewInfoManager.MoveRowAndAnchors_(ASource, ATarget: TdxTableViewInfo; AStartAnchorIndex: Integer);
var
  I, AInitialTargetTopRowIndex, AInitialSourceBottomRowIndex, ASourceRowCount, AMovedRowCount, ARowViewInfoIndex,
  ARowIndex, ASourceAnchorsCount, ALastMovedAnchorIndex, AMovedAnchorCount, AAnchorIndex, ASourceCellCount, ATargetCellCount: Integer;
  ARow: TdxTableRow;
  AKeepFirstAnchorInTarget: Boolean;
  ASourceAnchor: TdxTableCellVerticalAnchor;
  ATopTargetRowViewInfo, ASourceRowViewInfo: TdxTableRowViewInfoBase;
  ATargetCells, ASourceCells: TdxTableCellViewInfoCollection;
  ACellViewInfo: TdxTableCellViewInfo;
  AAction: TCellAction;
begin
  Assert(AStartAnchorIndex > 0);
  AInitialTargetTopRowIndex := ATarget.TopRowIndex;
  AInitialSourceBottomRowIndex := ASource.BottomRowIndex;
  ASourceRowCount := ASource.Rows.Count;
  AMovedRowCount := ASourceRowCount - AStartAnchorIndex;
  if ATarget.TopRowIndex = ASource.BottomRowIndex then
    Dec(AMovedRowCount);
  ATarget.Rows.ShiftForward(AMovedRowCount + 1);
  for I := 0 to AMovedRowCount - 1 do
  begin
    ARowViewInfoIndex := AStartAnchorIndex + I;
    ARowIndex := ASource.TopRowIndex + ARowViewInfoIndex;
    ARow := ASource.Rows[ARowViewInfoIndex].Row;
    ATarget.Rows[I + 1] := CreateTableRowViewInfo(ATarget, ARow, ARowIndex);
  end;
  ATarget.Rows[0] := CreateTableRowViewInfo(ATarget, ASource.Rows[AStartAnchorIndex - 1].Row, ASource.TopRowIndex + AStartAnchorIndex - 1);
  ASource.Rows.RemoveRows(AStartAnchorIndex, ASourceRowCount - AStartAnchorIndex);

  ASourceAnchorsCount := ASource.Anchors.Count;
  if AInitialSourceBottomRowIndex = AInitialTargetTopRowIndex then
    ALastMovedAnchorIndex := ASourceAnchorsCount - 2
  else
    ALastMovedAnchorIndex := ASourceAnchorsCount - 1;

  AMovedAnchorCount := ALastMovedAnchorIndex - AStartAnchorIndex + 1;
  AKeepFirstAnchorInTarget := AInitialSourceBottomRowIndex = AInitialTargetTopRowIndex;
  if AKeepFirstAnchorInTarget then
    ATarget.Anchors.ShiftForward(1, AMovedAnchorCount)
  else
    ATarget.Anchors.ShiftForward(0, AMovedAnchorCount);

  if not AKeepFirstAnchorInTarget then
    TdxRichEditExceptions.ThrowInternalException;
  for AAnchorIndex := AStartAnchorIndex to ALastMovedAnchorIndex do
  begin
    if ASource.Anchors[AAnchorIndex] <> nil then
    begin
      ASourceAnchor := ASource.Anchors[AAnchorIndex];
      ATarget.Anchors[AAnchorIndex - AStartAnchorIndex + 1] := ASourceAnchor.CloneWithNewVerticalPosition(0); ;
    end;
  end;
  ASource.Anchors.RemoveAnchors(AStartAnchorIndex, AMovedAnchorCount);

  ATarget.SetTopRowIndex(ASource.BottomRowIndex);

  ATopTargetRowViewInfo := ATarget.Rows[AMovedRowCount + 1];
  Assert(ATopTargetRowViewInfo <> nil);
  ATargetCells := ATopTargetRowViewInfo.Cells;
  ATargetCellCount := ATargetCells.Count;
  for I := 0 to ATargetCellCount - 1 do
  begin
    AddTableCellViewInfo(ATarget, ATargetCells[I], 0, AMovedRowCount);
    ATargetCells[I].ShiftBottom(AMovedAnchorCount);
    ATargetCells[I].SetTopAnchorIndexToLastAnchor();
  end;
  ASourceRowViewInfo := ASource.Rows[AStartAnchorIndex - 1];
  ASourceCells := ASourceRowViewInfo.Cells;
  ASourceCellCount := ASourceCells.Count;
  for I := 0 to ASourceCellCount - 1 do
  begin
    ACellViewInfo := ASourceCells[I];
    AAction := ShouldSplitCell(ASource, ATarget, ACellViewInfo, AStartAnchorIndex, ASourceAnchorsCount, AInitialSourceBottomRowIndex, AInitialTargetTopRowIndex);
    case AAction of
      TCellAction.Split:
        SplitCellByAnchor(ASource, ATarget, AStartAnchorIndex, ACellViewInfo);
      TCellAction.SetBottomIndex:
        ACellViewInfo.SetBottomAnchorIndexToLastAnchor;
    end;
  end;
end;

function TdxTableViewInfoManager.ShouldSplitCell(ASource, ATarget: TdxTableViewInfo; ACellViewInfo: TdxTableCellViewInfo; ASplitAnchorIndex, ASourceAnchorsCount, AInitialSourceBottomRowIndex, AInitialTargetTopRowIndex: Integer): TCellAction;
var
  ABottomAnchorIndex: Integer;
begin
  ABottomAnchorIndex := ACellViewInfo.BottomAnchorIndex;
  if ABottomAnchorIndex < ASplitAnchorIndex then
    Exit(TCellAction.None);
  if ABottomAnchorIndex < ASourceAnchorsCount - 1 then
    Exit(TCellAction.Split);
  Assert(ABottomAnchorIndex = ASourceAnchorsCount - 1);
  if AInitialTargetTopRowIndex = AInitialSourceBottomRowIndex then
    Exit(TCellAction.SetBottomIndex);
  TdxRichEditExceptions.ThrowInternalException;
  Exit(TCellAction.None);
end;

procedure TdxTableViewInfoManager.MoveRowAndAnchor(ASource, ATarget: TdxTableViewInfo; AStartAnchorIndex: Integer);
var
  ARowCount, ARowIndex, AAnchorCount, AAnchorIndex: Integer;
  ARow: TdxTableRow;
  ASourceAnchor: TdxTableCellVerticalAnchor;
begin
  ARowCount := ASource.Rows.Count;
  for ARowIndex := AStartAnchorIndex - 1 to ARowCount - 1 do
  begin
    ARow := ASource.Rows[ARowIndex].Row;
    EnsureTableRowViewInfo(ATarget, ARow, ASource.TopRowIndex + ARowIndex);
  end;

  AAnchorCount := ASource.Anchors.Count;
  for AAnchorIndex := AStartAnchorIndex to AAnchorCount - 1 do
  begin
    if ASource.Anchors[AAnchorIndex] <> nil then
    begin
      ASourceAnchor := ASource.Anchors[AAnchorIndex];
      ATarget.Anchors[AAnchorIndex - AStartAnchorIndex + 1] := ASourceAnchor.CloneWithNewVerticalPosition(0);
    end;
  end;
  ASource.Rows.RemoveRows(AStartAnchorIndex, ARowCount - AStartAnchorIndex);
  ASource.Anchors.RemoveAnchors(AStartAnchorIndex, AAnchorCount - AStartAnchorIndex);
end;

procedure TdxTableViewInfoManager.SplitCellsByAnchor(ACurrentTableViewInfo, ANextTableViewInfo: TdxTableViewInfo; AAnchorIndex: Integer);
var
  I, ARowViewInfoIndex, ACellCount: Integer;
  ARowViewInfo: TdxTableRowViewInfoBase;
begin
  Assert(AAnchorIndex > 0);
  ARowViewInfoIndex := AAnchorIndex - 1;
  ARowViewInfo := ACurrentTableViewInfo.Rows[ARowViewInfoIndex];
  ACellCount := ARowViewInfo.Cells.Count;
  for I := 0 to ACellCount - 1 do
    SplitCellByAnchor(ACurrentTableViewInfo, ANextTableViewInfo, AAnchorIndex, ARowViewInfo.Cells[I]);
end;

procedure TdxTableViewInfoManager.SplitCellByAnchor(ACurrentTableViewInfo, ANextTableViewInfo: TdxTableViewInfo;
  AAnchorIndex: Integer; ATableCellViewInfo: TdxTableCellViewInfo);
var
  ACell: TdxTableCell;
  ANextCellViewInfo: TdxTableCellViewInfo;
  ANewBottomAnchorIndex, ARelativeLeft: Integer;
begin
  ANewBottomAnchorIndex := ATableCellViewInfo.BottomAnchorIndex - AAnchorIndex + 1;
  ARelativeLeft := ATableCellViewInfo.Left - ACurrentTableViewInfo.Column.Bounds.Left;
  ACell := ATableCellViewInfo.Cell;
  ANextCellViewInfo := TdxTableCellViewInfo.Create(ANextTableViewInfo, ACell,
    ARelativeLeft + ANextTableViewInfo.Column.Bounds.Left, ATableCellViewInfo.Width, ATableCellViewInfo.TextLeft,
    ATableCellViewInfo.TextWidth, ATableCellViewInfo.TextOffset, 0, ANewBottomAnchorIndex, ATableCellViewInfo.StartRowIndex,
    ATableCellViewInfo.RowSpan);
  ANextTableViewInfo.Cells.Add(ANextCellViewInfo);
  ATableCellViewInfo.SetBottomAnchorIndexToLastAnchor;
  AddTableCellViewInfo(ANextTableViewInfo, ANextCellViewInfo, 0, ANewBottomAnchorIndex - 1);
end;

function TdxTableViewInfoManager.ShouldRemoveColumn(ATableViewInfoIndex: Integer; ATableViewInfo: TdxTableViewInfo): Boolean;
begin
  if ATableViewInfoIndex > 0 then
    Result := True
  else
    Result := ATableViewInfo.FirstContentInParentCell;
end;

function TdxTableViewInfoManager.GetTopLevelTableViewInfoManager: TdxTableViewInfoManager;
begin
  if FParentTableViewInfoManager <> nil then
    Result := FParentTableViewInfoManager.GetTopLevelTableViewInfoManager
  else
    Result := Self;
end;

function TdxTableViewInfoManager.GetParentTableCellViewInfo: TdxTableCellViewInfo;
begin
  if FParentTableViewInfoManager = nil then
    Result := nil
  else
    Result := FParentTableViewInfoManager.CurrentTableCellViewInfo;
end;

procedure TdxTableViewInfoManager.SetCurrentTableCellViewInfo(ATableCellViewInfo: TdxTableCellViewInfo);
begin
  Assert(ATableCellViewInfo <> nil);
  FCurrentTableCellViewInfo := ATableCellViewInfo;
  FCurrentTableViewInfoIndex := FTableViewInfos.IndexOf(FCurrentTableCellViewInfo.TableViewInfo);
  if FParentTableViewInfoManager <> nil then
    FParentTableViewInfoManager.SetCurrentTableCellViewInfo(ATableCellViewInfo.TableViewInfo.ParentTableCellViewInfo)
  else
    EnsureFloatingObjectsLayoutValid(ATableCellViewInfo.TableViewInfo);
  Assert(FCurrentTableViewInfoIndex >= 0);
end;

procedure TdxTableViewInfoManager.ValidateTopLevelColumn;
begin
end;

procedure TdxTableViewInfoManager.SetCurrentParentColumn(ATableViewInfo: TdxTableViewInfo);
begin
  ColumnController.SetCurrentParentColumn(ATableViewInfo.Column);
  if FParentTableViewInfoManager <> nil then
    FParentTableViewInfoManager.SetCurrentParentColumn(ATableViewInfo.ParentTableCellViewInfo.TableViewInfo)
  else
    EnsureFloatingObjectsLayoutValid(ATableViewInfo);
end;

procedure TdxTableViewInfoManager.EnsureFloatingObjectsLayoutValid(ATableViewInfo: TdxTableViewInfo);
begin
  Assert(FParentTableViewInfoManager = nil);
  if ATableViewInfo.AssociatedFloatingObjectsLayout <> nil then
    FPageController.SetFloatingObjectsLayout(ATableViewInfo.AssociatedFloatingObjectsLayout);
end;

function TdxTableViewInfoManager.FindSuitableTableCellViewInfo(ACells: TdxTableCellViewInfoCollection; ACell: TdxTableCell): TdxTableCellViewInfo;
var
  I: Integer;
  ACellViewInfo: TdxTableCellViewInfo;
begin
  for I := ACells.Count - 1 downto 0 do
  begin
    ACellViewInfo := ACells[I];
    if ACellViewInfo.Cell = ACell then
      Exit(ACellViewInfo);
  end;
  Result := nil;
end;

function TdxTableViewInfoManager.GetSplitAnchorHorizontalCellBorders(ASplitAnchor: TdxTableCellVerticalAnchor): TdxHorizontalCellBordersInfoList;
begin
  if ASplitAnchor <> nil then
    Result := ASplitAnchor.CellBorders.Clone
  else
    Result := nil;
end;



{ TdxStateRowTextSplit }

procedure TdxStateRowTextSplit.AddSuccess(ABoxInfo: TdxBoxInfo);
begin
  Formatter.ApproveFloatingObjects;
  AddTextBox(ABoxInfo);
  ChangeStateManuallyIfNeeded(ABoxInfo.IteratorResult);
end;

function TdxStateRowTextSplit.CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState;
begin
  if TdxCharacters.IsCharSpace(ACurrentCharacter) then
    Exit(TdxParagraphBoxFormatterState.RowSpaces);
  if ACurrentCharacter = TdxCharacters.TabMark then
    Exit(TdxParagraphBoxFormatterState.RowTab);
  if TdxCharacters.IsCharDash(ACurrentCharacter) then
    Exit(TdxParagraphBoxFormatterState.RowDash);
  if ACurrentCharacter = TdxCharacters.LineBreak then
    Exit(TdxParagraphBoxFormatterState.RowLineBreak);
  if ACurrentCharacter = TdxCharacters.PageBreak then
  begin
    if RowsController.TablesController.IsInsideTable then
      Exit(TdxParagraphBoxFormatterState.RowSpaces)
    else
      if not RowsController.SupportsColumnAndPageBreaks then
        Exit(TdxParagraphBoxFormatterState.RowLineBreak)
      else
        Exit(TdxParagraphBoxFormatterState.RowPageBreak);
  end;
  if ACurrentCharacter = TdxCharacters.ColumnBreak then
  begin
    if RowsController.TablesController.IsInsideTable then
      Exit(TdxParagraphBoxFormatterState.RowSpaces)
    else
      if not RowsController.SupportsColumnAndPageBreaks then
        Exit(TdxParagraphBoxFormatterState.RowLineBreak)
      else
        Exit(TdxParagraphBoxFormatterState.RowColumnBreak);
  end;
  if ACurrentCharacter = TdxCharacters.FloatingObjectMark then
    if Iterator.IsFloatingObjectAnchorRun then
      Exit(TdxParagraphBoxFormatterState.FloatingObject)
    else
      Exit(TdxParagraphBoxFormatterState.RowWithTextOnly);
  TdxRichEditExceptions.ThrowInternalException;
  Result := TdxParagraphBoxFormatterState.Final;
end;

function TdxStateRowTextSplit.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := CanAddBoxCore(ABoxInfo);
end;

procedure TdxStateRowTextSplit.ChangeStateManuallyIfNeeded(AIteratorResult: TdxParagraphIteratorResult);
var
  ACurrentChar: Char;
  ANextState: TdxParagraphBoxFormatterState;
begin
  ACurrentChar := Iterator.CurrentChar;
  if ShouldChangeStateManually(AIteratorResult, ACurrentChar) then
  begin
    ANextState := CalcNextState(ACurrentChar);
    ChangeState(ANextState);
  end;
end;

function TdxStateRowTextSplit.ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowTextSplit.FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(CurrentRow.Height > 0);

  Result := RowsController.CompleteCurrentColumnFormatting;
  if Result <> TdxCompleteFormattingResult.Success then
    Exit;
  Formatter.RowsController.AddBox(TdxColumnBreakBox, ABoxInfo);
  Formatter.ApproveFloatingObjects;
  EndRow;
  RowsController.MoveRowToNextColumn;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
end;

function TdxStateRowTextSplit.FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(CurrentRow.Height > 0);
  Formatter.RowsController.AddBox(TdxLineBreakBox, ABoxInfo);
  Formatter.ApproveFloatingObjects;
  EndRow;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowTextSplit.FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(CurrentRow.Height > 0);

  Result := RowsController.CompleteCurrentColumnFormatting;
  if Result <> TdxCompleteFormattingResult.Success then
    Exit;
  Formatter.RowsController.AddBox(TdxPageBreakBox, ABoxInfo);
  Formatter.ApproveFloatingObjects;
  EndRow;
  RowsController.MoveRowToNextPage;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
end;

function TdxStateRowTextSplit.FinishParagraph: TdxCompleteFormattingResult;
begin
  Assert(CurrentRow.Height > 0);
  Result := inherited FinishParagraph;
end;

function TdxStateRowTextSplit.FinishSection: TdxCompleteFormattingResult;
begin
  Assert(CurrentRow.Height > 0);
  Result := inherited FinishSection;
end;

function TdxStateRowTextSplit.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextSplit;
end;

function TdxStateRowTextSplit.NextSplitState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextSplit;
end;

function TdxStateRowTextSplit.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextSplitAfterFloatingObject;
end;

function TdxStateRowTextSplit.HandleUnsuccessfulSplitting: TdxCompleteFormattingResult;
begin
  EndRow;
  ChangeState(TdxParagraphBoxFormatterState.RowTextSplit);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowTextSplit.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ASplitBoxResult: TdxSplitBoxResult;
begin
  Result := TdxCompleteFormattingResult.Success;
  ASplitBoxResult := SplitBox(ABoxInfo);
  if ASplitBoxResult <> TdxSplitBoxResult.FailedHorizontalOverfull then
  begin
    AddTextBox(ABoxInfo);
    Formatter.ApproveFloatingObjects;
    if not Iterator.IsEnd then
    begin
      if (Iterator.CurrentChar = TdxCharacters.PageBreak) and not RowsController.TablesController.IsInsideTable then
      begin
        if not RowsController.SupportsColumnAndPageBreaks then
          ChangeState(TdxParagraphBoxFormatterState.RowLineBreak)
        else
          ChangeState(TdxParagraphBoxFormatterState.RowPageBreak);
        Exit;
      end;
      if (Iterator.CurrentChar = TdxCharacters.ColumnBreak) and not RowsController.TablesController.IsInsideTable then
      begin
        if not RowsController.SupportsColumnAndPageBreaks then
          ChangeState(TdxParagraphBoxFormatterState.RowLineBreak)
        else
          ChangeState(TdxParagraphBoxFormatterState.RowColumnBreak);
        Exit;
      end;
      if (Iterator.CurrentChar = TdxCharacters.LineBreak) and (ASplitBoxResult <> TdxSplitBoxResult.SuccessSuppressedHorizontalOverfull) then
        ChangeState(TdxParagraphBoxFormatterState.RowLineBreak)
      else
      begin
        if not (Iterator.CurrentBox is TdxFloatingObjectAnchorBox) then
        begin
          EndRow;
          if IsTerminatorChar(Iterator.CurrentChar) then
            ChangeState(TdxParagraphBoxFormatterState.RowEmpty)
          else
            ChangeState(NextSplitState);
        end;
      end;
    end;
  end
  else
    Result := HandleUnsuccessfulSplitting;
end;

function TdxStateRowTextSplit.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := TdxCharacters.IsCharSpace(ACh) or (ACh = TdxCharacters.TabMark) or (ACh = TdxCharacters.LineBreak) or
    (ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak) or (ACh = TdxCharacters.FloatingObjectMark) or
    TdxCharacters.IsCharDash(ACh);
end;

function TdxStateRowTextSplit.ShouldChangeStateManually(AIteratorResult: TdxParagraphIteratorResult;
  ACurrentCharacter: Char): Boolean;
begin
  Result := (AIteratorResult = TdxParagraphIteratorResult.Success) or ((not Iterator.IsEnd) and IsTerminatorChar(ACurrentCharacter));
end;

{ TdxStateRowTextSplitAfterFloatingObject }

function TdxStateRowTextSplitAfterFloatingObject.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextSplitAfterFloatingObject;
end;

{ TdxStateRowDashSplit }

function TdxStateRowDashSplit.CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState;
begin
  if not inherited IsTerminatorChar(ACurrentCharacter) then
    Result := TdxParagraphBoxFormatterState.RowText
  else
    Result := inherited CalcNextState(ACurrentCharacter);
end;

function TdxStateRowDashSplit.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowDashSplit;
end;

function TdxStateRowDashSplit.HandleUnsuccessfulSplitting: TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  if CurrentRow.Height <= 0 then
  begin
    SetCurrentRowHeightToLastBoxHeight;
    ACanFit := RowsController.CanFitCurrentRowToColumn;
    if ACanFit <> TdxCanFitCurrentRowToColumnResult.RowFitted then
      Exit(Formatter.RollbackToStartOfRow(ACanFit));
  end;
  EndRow;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowDashSplit.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := not TdxCharacters.IsCharDash(ACh);
end;

function TdxStateRowDashSplit.NextSplitState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowDashSplit;
end;

function TdxStateRowDashSplit.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowDashSplitAfterFloatingObject;
end;

{ TdxStateRowDashSplitAfterFloatingObject }

function TdxStateRowDashSplitAfterFloatingObject.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowDashSplitAfterFloatingObject;
end;

{ TdxStateRowSpaces }

procedure TdxStateRowSpaces.AddSuccess(ABoxInfo: TdxBoxInfo);
begin
  Formatter.ApproveFloatingObjects;
  RowsController.AddBox(TdxParagraphBoxFormatter.GetSpaceBoxTemplate(ABoxInfo), ABoxInfo);
  ChangeStateManuallyIfNeeded(ABoxInfo.IteratorResult);
end;

procedure TdxStateRowSpaces.ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert(CurrentRow.Height <> 0);
end;

procedure TdxStateRowSpaces.ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert(CurrentRow.Height <> 0);
end;

procedure TdxStateRowSpaces.ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert(CurrentRow.Height <> 0);
end;

procedure TdxStateRowSpaces.ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert(CurrentRow.Height <> 0);
end;

function TdxStateRowSpaces.CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState;
begin
  if ACurrentCharacter = TdxCharacters.TabMark then
    Exit( TdxParagraphBoxFormatterState.RowTab );
  if ACurrentCharacter = TdxCharacters.LineBreak then
    Exit( TdxParagraphBoxFormatterState.RowLineBreak );
  if ACurrentCharacter = TdxCharacters.PageBreak then
  begin
    Assert(not RowsController.TablesController.IsInsideTable);
    if not RowsController.SupportsColumnAndPageBreaks then
      Result := TdxParagraphBoxFormatterState.RowLineBreak
    else
      Result := TdxParagraphBoxFormatterState.RowPageBreak;
    Exit;
  end;
  if ACurrentCharacter = TdxCharacters.ColumnBreak then
  begin
    Assert(not RowsController.TablesController.IsInsideTable);
    if not RowsController.SupportsColumnAndPageBreaks then
      Result := TdxParagraphBoxFormatterState.RowLineBreak
    else
      Result := TdxParagraphBoxFormatterState.RowColumnBreak;
  end
  else
    if (ACurrentCharacter = TdxCharacters.FloatingObjectMark) and Iterator.IsFloatingObjectAnchorRun then
      Result := TdxParagraphBoxFormatterState.FloatingObject
    else
      if TdxCharacters.IsCharDash(ACurrentCharacter) then
        Result := TdxParagraphBoxFormatterState.RowDash
      else
        Result := TdxParagraphBoxFormatterState.RowText;
end;

function TdxStateRowSpaces.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := TdxAddBoxResult.Success;
end;

procedure TdxStateRowSpaces.ChangeStateManuallyIfNeeded(AIteratorResult: TdxParagraphIteratorResult);
var
  ACurrentChar: Char;
  ANextState: TdxParagraphBoxFormatterState;
begin
  ACurrentChar := Iterator.CurrentChar;
  if ShouldChangeStateManually(ACurrentChar) then
  begin
    ANextState := CalcNextState(ACurrentChar);
    ChangeState(ANextState);
  end;
end;

function TdxStateRowSpaces.ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowSpaces.CanUseBox: Boolean;
begin
  Result := True;
end;

function TdxStateRowSpaces.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowSpaces;
end;

function TdxStateRowSpaces.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowSpaces.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := not TdxCharacters.IsCharSpace(ACh) or
    (((ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak)) and RowsController.TablesController.IsInsideTable);
end;

procedure TdxStateRowSpaces.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Formatter.Measurer.MeasureSpaces(ABoxInfo);
end;

function TdxStateRowSpaces.ShouldChangeStateManually(ACurrentCharacter: Char): Boolean;
begin
  Result := (not Iterator.IsEnd) and IsTerminatorChar(ACurrentCharacter);
end;

{ TdxStateRowWithSpacesOnly }

procedure TdxStateRowWithSpacesOnly.ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

procedure TdxStateRowWithSpacesOnly.ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

procedure TdxStateRowWithSpacesOnly.ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

procedure TdxStateRowWithSpacesOnly.ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo);
begin
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

function TdxStateRowWithSpacesOnly.CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState;
begin
  if ACurrentCharacter = TdxCharacters.TabMark then
    Exit(TdxParagraphBoxFormatterState.RowLeadingTab);
  if ACurrentCharacter = TdxCharacters.LineBreak then
    Exit(TdxParagraphBoxFormatterState.RowLineBreak);
  if ACurrentCharacter = TdxCharacters.PageBreak then
  begin
    if RowsController.TablesController.IsInsideTable then
      Exit(TdxParagraphBoxFormatterState.RowText);
    if not RowsController.SupportsColumnAndPageBreaks then
      Exit(TdxParagraphBoxFormatterState.RowLineBreak)
    else
      Exit(TdxParagraphBoxFormatterState.RowPageBreak);
  end;
  if ACurrentCharacter = TdxCharacters.ColumnBreak then
  begin
    Assert(not RowsController.TablesController.IsInsideTable);
    if not RowsController.SupportsColumnAndPageBreaks then
      Exit(TdxParagraphBoxFormatterState.RowLineBreak)
    else
      Exit(TdxParagraphBoxFormatterState.RowColumnBreak);
  end;
  if TdxCharacters.IsCharDash(ACurrentCharacter) then
    Exit(TdxParagraphBoxFormatterState.RowDash);
  if (ACurrentCharacter = TdxCharacters.FloatingObjectMark) and Iterator.IsFloatingObjectAnchorRun then
    Exit(TdxParagraphBoxFormatterState.FloatingObject)
  else
    Exit(TdxParagraphBoxFormatterState.RowText);
end;

function TdxStateRowWithSpacesOnly.FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
    Result := FinalizeColumn(ABoxInfo)
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowWithSpacesOnly.FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
  begin
    FinalizeLine(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  end
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowWithSpacesOnly.FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
    Result := FinalizePage(ABoxInfo)
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowWithSpacesOnly.FinishParagraph: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateParagraphMarkBoxInfo;
  try
    Result := InternalFinishParagraphCore(ABoxInfo, FinalizeParagraph);
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxStateRowWithSpacesOnly.FinishSection: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateSectionMarkBoxInfo;
  try
    Result := InternalFinishParagraphCore(ABoxInfo, FinalizeSection);
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxStateRowWithSpacesOnly.InternalFinishParagraphCore(ABoxInfo: TdxBoxInfo;
  AFinalizeHandler: TFinalizeEvent): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ApplyParagraphMarkSize(ABoxInfo);
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
  begin
    AFinalizeHandler(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  end
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowWithSpacesOnly.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithSpacesOnly;
end;

{ TdxTableCellColumn }

constructor TdxTableCellColumn.Create(AParent: TdxColumn; ACell: TdxTableCell);
begin
  inherited Create;
  Assert(AParent <> nil);
  FParent := AParent;
  FCell := ACell;
end;

destructor TdxTableCellColumn.Destroy;
begin
  inherited Destroy;
end;

procedure TdxTableCellColumn.AddParagraphFrame(AParagraph: TdxSimpleParagraph);
begin
  TopLevelColumn.AddParagraphFrame(AParagraph);
end;

function TdxTableCellColumn.GetOwnRows: TdxRowCollection;
begin
  Result := TdxTableCellViewInfo.GetRows(Rows, Cell);
end;

function TdxTableCellColumn.GetRows: TdxRowCollection;
begin
  Result := FParent.Rows;
end;

function TdxTableCellColumn.GetTopLevelColumn: TdxColumn;
begin
  Result := Parent.TopLevelColumn;
end;

{ TdxSyllableBoxIterator }

constructor TdxSyllableBoxIterator.Create(AIterator: TdxParagraphBoxIterator; const AHyphenationService: IdxHyphenationService);
begin
  inherited Create(AIterator.Paragraph, AIterator.PieceTable, AIterator.VisibleTextFilter);
  Assert(AHyphenationService <> nil);
  inherited SetPosition(AIterator.GetCurrentPosition);
  FHyphenationService := AHyphenationService;
  FIterator := AIterator;
  HyphenateCurrentWord;
end;

destructor TdxSyllableBoxIterator.Destroy;
begin
  FreeAndNil(FHyphenPositions);
  inherited Destroy;
end;

function TdxSyllableBoxIterator.GetCurrentChar: Char;
begin
  if FHyphenAtCurrentPosition then
    Result := HyphenChar
  else
    Result := inherited GetCurrentChar;
end;

function TdxSyllableBoxIterator.GetCurrentWord: string;
var
  ABuilder: TdxChunkedStringBuilder;
  C: Char;
begin
  ABuilder := TdxChunkedStringBuilder.Create;
  try
    while not FIterator.IsEnd do
    begin
      C := FIterator.CurrentChar;
      if IsEndOfWord(C) then
        break;
      ABuilder.Append(C);
      FIterator.Next;
    end;
    FEndPos := FIterator.GetCurrentPosition;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxSyllableBoxIterator.GetHyphenChar: Char;
begin
  Result := TdxCharacters.Hyphen;
end;

function TdxSyllableBoxIterator.GetIsEnd: Boolean;
begin
  Result := ((RunIndex = FEndPos.RunIndex) and (Offset >= FEndPos.Offset)) or (RunIndex > FEndPos.RunIndex);
end;

procedure TdxSyllableBoxIterator.HyphenateCurrentWord;
var
  AWord: string;
  AHyphenIndices: TdxIntegerList;
  I, AHyphenIndex, AHyphenIndicesCount: Integer;
  AStartWordPosition: TdxFormatterPosition;
begin
  AWord := GetCurrentWord;
  AHyphenIndices := FHyphenationService.Hyphenate(AWord);
  try
    FreeAndNil(FHyphenPositions);
    FHyphenPositions := TdxFormatterPositionCollection.Create;

    AHyphenIndicesCount := AHyphenIndices.Count;
    if AHyphenIndicesCount <= 0 then
    begin
      SetInvalidHyphenPos;
      Exit;
    end;
    AStartWordPosition := GetCurrentPosition;
    I := 1;
    AHyphenIndex := 0;
    while not IsEnd and not IsWhiteSpace(inherited GetCurrentChar) do
    begin
      if I = AHyphenIndices[AHyphenIndex] then
      begin
        FHyphenPositions.Add(GetCurrentPosition);
        Inc(AHyphenIndex);
        if AHyphenIndex >= AHyphenIndicesCount then
          break;
      end;
      Inc(I);
      inherited Next;
    end;

    Assert(FHyphenPositions.Count = AHyphenIndicesCount);

    FHyphenPosIndex := 0;
    FHyphenPos := FHyphenPositions[FHyphenPosIndex];

    SetPosition(AStartWordPosition);
  finally
    AHyphenIndices.Free;
  end;
end;

class function TdxSyllableBoxIterator.IsBreak(ACh: Char): Boolean;
begin
  Result := (ACh = TdxCharacters.LineBreak) or (ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak);
end;

class function TdxSyllableBoxIterator.IsEndOfWord(ACh: Char): Boolean;
begin
  Result := False;
  if IsWhiteSpace(ACh) then
    Result := True
  else
    if IsBreak(ACh) then
      Result := True;
end;

class function TdxSyllableBoxIterator.IsWhiteSpace(ACh: Char): Boolean;
begin
  Result := (ACh = TdxCharacters.Space) or (ACh = TdxCharacters.TabMark);
end;

function TdxSyllableBoxIterator.Next: TdxParagraphIteratorResult;
begin
  if FHyphenAtCurrentPosition then
  begin
    FHyphenAtCurrentPosition := False;
    Inc(FHyphenPosIndex);
    if FHyphenPosIndex >= FHyphenPositions.Count then
      SetInvalidHyphenPos
    else
      FHyphenPos := FHyphenPositions[FHyphenPosIndex];

    Exit(TdxParagraphIteratorResult.Success);
  end;
  Result := inherited Next;
  if Result <> TdxParagraphIteratorResult.Finished then
  begin
    if (RunIndex = FHyphenPos.RunIndex) and (Offset = FHyphenPos.Offset) then
      FHyphenAtCurrentPosition := True;
    if IsEnd then
      Result := TdxParagraphIteratorResult.Finished;
  end;
end;

procedure TdxSyllableBoxIterator.SetInvalidHyphenPos;
begin
  FHyphenPos.Init(-1, -1, -1);
  FHyphenPosIndex := -1;
end;

procedure TdxSyllableBoxIterator.SetPosition(const APos: TdxFormatterPosition);
begin
  SetPosition(APos.RunIndex, APos.Offset);
  BoxIndex := APos.BoxIndex;
end;

procedure TdxSyllableBoxIterator.SetPosition(ARunIndex: TdxRunIndex; AOffset: Integer);
var
  I, ACount: Integer;
  APos: TdxFormatterPosition;
begin
  inherited SetPosition(ARunIndex, AOffset);
  FHyphenAtCurrentPosition := False;
  ACount := FHyphenPositions.Count;
  for I := 0 to ACount - 1 do
  begin
    APos := FHyphenPositions[I];
    if (APos.RunIndex > ARunIndex) or ((APos.RunIndex = ARunIndex) and (APos.Offset > AOffset)) then
    begin
      FHyphenPosIndex := I;
      FHyphenPos := FHyphenPositions[FHyphenPosIndex];
      Break;
    end;
  end;
end;

{ TdxStateRowBreakBase }

function TdxStateRowBreakBase.FinishParagraph: TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowBreakBase.FinishSection: TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowBreakBase.FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowBreakBase.FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowBreakBase.FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowBreakBase.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := True;
end;

{ TdxStateRowPageBreak }

function TdxStateRowPageBreak.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowPageBreak;
end;

function TdxStateRowPageBreak.ContinueFormat: TdxStateContinueFormatResult;
var
  ABoxInfo: TdxBoxInfo;
  ACurrentBox: TdxBox;
begin
  if not RowsController.TablesController.IsInsideTable then
  begin
    ABoxInfo := TdxBoxInfo.Create;
    try
      ACurrentBox := Iterator.CurrentBox;
      ABoxInfo.Box := ACurrentBox;
      ABoxInfo.StartPos := ACurrentBox.StartPos;
      ABoxInfo.EndPos := ACurrentBox.EndPos;
      ABoxInfo.Size := ABoxInfo.Box.Bounds.Size;
      PreviousState.ApplyPageBreakMarkSize(ABoxInfo);
      Iterator.Next;
      Result := GetContinueFormatResult(PreviousState.FinishPage(ABoxInfo));
    finally
      FreeAndNil(ABoxInfo);
    end;
  end
  else
  begin
    Iterator.Next;
    ChangeState(PreviousState.&Type);
    Result := TdxStateContinueFormatResult.Success;
  end;
end;

{ TdxStateRowPageBreakAtParagraphStart }

function TdxStateRowPageBreakAtParagraphStart.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowPageBreakAtParagraphStart;
end;

function TdxStateRowPageBreakAtParagraphStart.ContinueFormat: TdxStateContinueFormatResult;
begin
  PreviousState.StateAfterFinalizePage := TdxParagraphBoxFormatterState.ParagraphStartAfterBreak;
  Result := inherited ContinueFormat;
end;

{ TdxStateRowColumnBreak }

function TdxStateRowColumnBreak.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowColumnBreak;
end;

function TdxStateRowColumnBreak.ContinueFormat: TdxStateContinueFormatResult;
var
  ABoxInfo: TdxBoxInfo;
  ACurrentBox: TdxBox;
begin
  ABoxInfo := TdxBoxInfo.Create;
  try
    ACurrentBox := Iterator.CurrentBox;
    ABoxInfo.Box := ACurrentBox;
    ABoxInfo.StartPos := ACurrentBox.StartPos;
    ABoxInfo.EndPos := ACurrentBox.EndPos;
    ABoxInfo.Size := ABoxInfo.Box.Bounds.Size;
    PreviousState.ApplyColumnBreakMarkSize(ABoxInfo);
    Iterator.Next;
    Result := GetContinueFormatResult(PreviousState.FinishColumn(ABoxInfo));
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

{ TdxStateRowColumnBreakAtParagraphStart }

function TdxStateRowColumnBreakAtParagraphStart.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowColumnBreakAtParagraphStart;
end;

function TdxStateRowColumnBreakAtParagraphStart.ContinueFormat: TdxStateContinueFormatResult;
begin
  PreviousState.StateAfterFinalizeColumn := TdxParagraphBoxFormatterState.ParagraphStartAfterBreak;
  Result := inherited ContinueFormat;
end;

{ TdxStateRowLineBreak }

function TdxStateRowLineBreak.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowLineBreak;
end;

function TdxStateRowLineBreak.ContinueFormat: TdxStateContinueFormatResult;
var
  ABoxInfo: TdxBoxInfo;
  ACurrentBox: TdxBox;
begin
  ABoxInfo := TdxBoxInfo.Create;
  try
    ACurrentBox := Iterator.CurrentBox;
    ABoxInfo.Box := ACurrentBox;
    ABoxInfo.StartPos := ACurrentBox.StartPos;
    ABoxInfo.EndPos := ACurrentBox.EndPos;
    ABoxInfo.Size := ABoxInfo.Box.Bounds.Size;
    PreviousState.ApplyLineBreakMarkSize(ABoxInfo);
    Iterator.Next;
    Result := GetContinueFormatResult(PreviousState.FinishLine(ABoxInfo));
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

{ TdxStateFinal }

function TdxStateFinal.FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateFinal.FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateFinal.FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateFinal.FinishParagraph: TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateFinal.FinishSection: TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateFinal.GetFormattingComplete: Boolean;
begin
  Result := True;
end;

function TdxStateFinal.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.Final;
end;

{ TdxBoxInfoList }

function TdxBoxInfoList.GetItem(Index: Integer): TdxBoxInfo;
begin
  Result := TdxBoxInfo(inherited Items[Index]);
end;

{ TdxStateRowText }

function TdxStateRowText.GetDashAfterTextState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowDashAfterText;
end;

function TdxStateRowText.GetHyphenationState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllable;
end;

function TdxStateRowText.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowText;
end;

function TdxStateRowText.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  if Formatter.SuppressHyphenation then
    Result := WrapLine
  else
    Result := inherited HorizontalOverfull(ABoxInfo);
end;

function TdxStateRowText.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextAfterFloatingObject;
end;

function TdxStateRowText.WrapLine: TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  Formatter.RollbackToStartOfWord;
  Formatter.RollbackToLastTab;
  RowsController.TryToRemoveLastTabBox;
  if CurrentRow.Height <= 0 then
  begin
    SetCurrentRowHeightToLastBoxHeight;
    ACanFit := RowsController.CanFitCurrentRowToColumn;
    if ACanFit <> TdxCanFitCurrentRowToColumnResult.RowFitted then
    begin
      Formatter.ClearSyllableIterator;
      Exit(Formatter.RollbackToStartOfRow(ACanFit));
    end;
  end;
  EndRow;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
  Result := TdxCompleteFormattingResult.Success;
end;

{ TdxStateRowDashAfterText }

function TdxStateRowDashAfterText.CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState;
begin
  if TdxCharacters.IsCharDash(ACurrentCharacter) then
    Exit(TdxParagraphBoxFormatterState.RowDash);
  if inherited IsTerminatorChar(ACurrentCharacter) then
    Result := inherited CalcNextState(ACurrentCharacter)
  else
    Result := TdxParagraphBoxFormatterState.RowText;
end;

function TdxStateRowDashAfterText.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowText;
end;

function TdxStateRowDashAfterText.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := True;
end;

{ TdxStateRowWithDashAfterTextOnly }

function TdxStateRowWithDashAfterTextOnly.CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState;
begin
  if TdxCharacters.IsCharDash(ACurrentCharacter) then
    Exit(TdxParagraphBoxFormatterState.RowDash);
  if inherited IsTerminatorChar(ACurrentCharacter) then
    Result := inherited CalcNextState(ACurrentCharacter)
  else
    Result := TdxParagraphBoxFormatterState.RowText;
end;

function TdxStateRowWithDashAfterTextOnly.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithDashAfterTextOnly;
end;

function TdxStateRowWithDashAfterTextOnly.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Formatter.RollbackToStartOfWord;
  ChangeState(TdxParagraphBoxFormatterState.RowDashSplit);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowWithDashAfterTextOnly.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := True;
end;

{ TdxStateRowDash }

function TdxStateRowDash.CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState;
begin
  if inherited IsTerminatorChar(ACurrentCharacter) then
    Result := inherited CalcNextState(ACurrentCharacter)
  else
    Result := TdxParagraphBoxFormatterState.RowText;
end;

function TdxStateRowDash.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowDash;
end;

function TdxStateRowDash.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Formatter.RollbackToStartOfWord;
  ChangeState(TdxParagraphBoxFormatterState.RowDashSplit);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowDash.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := not TdxCharacters.IsCharDash(ACh) or
    (((ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak)) and RowsController.TablesController.IsInsideTable);
end;

function TdxStateRowDash.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowDashAfterFloatingObject;
end;

{ TdxStateRowWithDashOnly }

function TdxStateRowWithDashOnly.CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState;
begin
  if inherited IsTerminatorChar(ACurrentCharacter) then
    Result := inherited CalcNextState(ACurrentCharacter)
  else
    Result := TdxParagraphBoxFormatterState.RowText;
end;

function TdxStateRowWithDashOnly.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithDashOnly;
end;

function TdxStateRowWithDashOnly.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Formatter.RollbackToStartOfWord;
  ChangeState(TdxParagraphBoxFormatterState.RowDashSplit);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowWithDashOnly.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := not TdxCharacters.IsCharDash(ACh) or
   (((ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak)) and RowsController.TablesController.IsInsideTable);
end;

function TdxStateRowWithDashOnly.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithDashOnlyAfterFloatingObject;
end;

{ TdxFormatterStartEndState }

procedure TdxFormatterStartEndState.AddSuccess(ABoxInfo: TdxBoxInfo);
begin
  Assert(False);
end;

function TdxFormatterStartEndState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  Result := TdxStateContinueFormatResult.Success;
end;

function TdxFormatterStartEndState.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := TdxAddBoxResult.Success;
end;

function TdxFormatterStartEndState.ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxFormatterStartEndState.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxFormatterStartEndState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := False;
end;

{ TdxStateContinueFormattingFromParagraph }

constructor TdxStateContinueFormattingFromParagraph.Create(AFormatter: TdxParagraphBoxFormatter; AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create(AFormatter);
  FParagraphIndex := AParagraphIndex;
end;

function TdxStateContinueFormattingFromParagraph.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.ContinueFromParagraph;
end;

{ TdxStateSectionBreakAfterParagraphMark }

function TdxStateSectionBreakAfterParagraphMark.ContinueFormat: TdxStateContinueFormatResult;
begin
  Assert(Iterator.CurrentChar = TdxCharacters.SectionMark);
  Result := GetContinueFormatResult(FinishSection);
end;

function TdxStateSectionBreakAfterParagraphMark.CreateSectionMarkBoxInfo: TdxBoxInfo;
begin
  Result := TdxBoxInfo.Create;
  Result.Box := Iterator.CurrentBox;
  Result.StartPos := Iterator.GetCurrentPosition;
  Result.EndPos := Result.StartPos;
  Formatter.Measurer.MeasureSectionMark(Result);
end;

function TdxStateSectionBreakAfterParagraphMark.FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateSectionBreakAfterParagraphMark.FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateSectionBreakAfterParagraphMark.FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateSectionBreakAfterParagraphMark.FinishParagraph: TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateSectionBreakAfterParagraphMark.FinishSection: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateSectionMarkBoxInfo;
  try
    if RowsController.CanAddSectionBreakToPrevRow then
    begin
      RowsController.AddSectionBreakBoxToPrevRow(TdxSectionMarkBox, ABoxInfo);
      ChangeState(TdxParagraphBoxFormatterState.Final);
    end
    else
    begin
      if CurrentRow.Height = 0 then
        RowsController.UpdateCurrentRowHeight(ABoxInfo);
      ApplyParagraphMarkSize(ABoxInfo);
      FinalizeSection(ABoxInfo);
    end;
    Result := TdxCompleteFormattingResult.Success;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxStateSectionBreakAfterParagraphMark.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.SectionBreakAfterParagraphMark
end;

{ TdxDefaultPageAreaControllerState }

constructor TdxDefaultPageAreaControllerState.Create(AOwner: TdxPageAreaController; ANextAreaIndex: Integer);
begin
  inherited Create(AOwner);
  FNextAreaIndex := ANextAreaIndex;
end;

procedure TdxDefaultPageAreaControllerState.ApplyContinuousSectionStart(ASection: TdxSection);
begin
  FNextAreaIndex := 0;
end;

procedure TdxDefaultPageAreaControllerState.ApplySectionStart(ASection: TdxSection; ACurrentColumnsCount: Integer);
begin
  case ASection.GeneralSettings.StartType of
    TdxSectionStartType.Continuous:
      ApplyContinuousSectionStart(ASection);
    TdxSectionStartType.EvenPage, TdxSectionStartType.OddPage, TdxSectionStartType.NextPage:
      FNextAreaIndex := 0;
    TdxSectionStartType.Column:
      if ASection.GetActualColumnsCount <> ACurrentColumnsCount then
        FNextAreaIndex := 0;
  end;
end;

function TdxDefaultPageAreaControllerState.CompleteCurrentAreaFormatting: TdxCompleteFormattingResult;
var
  ABounds: TRect;
  ANewBoundsHeight: Integer;
begin
  if FNextAreaIndex = 0 then
    Exit(PageController.CompleteCurrentPageFormatting)
  else
  begin
    ABounds := CurrentAreaBounds;
    CreateCurrentAreaBounds;
    ANewBoundsHeight := CurrentAreaBounds.Height;
    RestoreCurrentAreaBounds(ABounds);
    if ANewBoundsHeight <= 0 then
    begin
      Result := PageController.CompleteCurrentPageFormatting;
      if Result <> TdxCompleteFormattingResult.Success then
        Exit(Result);
    end;
  end;
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxDefaultPageAreaControllerState.CreateCurrentAreaBoundsCore: TRect;
var
  ALastAreaBottom: Integer;
begin
  if FNextAreaIndex = 0 then
    Result := PageController.PageClientBounds
  else
  begin
    Assert(Areas.Count > 0);
    Result := PageController.PageClientBounds;
    ALastAreaBottom := Areas[FNextAreaIndex].Bounds.Bottom;
    Result.Height := Result.Bottom - ALastAreaBottom;
    Result := cxRectSetTop(Result, ALastAreaBottom);
  end;
end;

function TdxDefaultPageAreaControllerState.GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea;
begin
  if FNextAreaIndex = 0 then
  begin
    PageController.GetNextPage(AKeepFloatingObjects);
    CreateCurrentAreaBounds;
  end
  else
  begin
    CreateCurrentAreaBounds;
    if CurrentAreaBounds.Bottom - CurrentAreaBounds.Top  <= 0 then
    begin
      FNextAreaIndex := 0;
      PageController.GetNextPage(AKeepFloatingObjects);
      CreateCurrentAreaBounds;
    end;
  end;

  Result := GetNextPageAreaCore;
  Areas.Add(Result);
end;

procedure TdxDefaultPageAreaControllerState.RestartFormattingFromTheMiddleOfSection(ASection: TdxSection;
  ACurrentAreaIndex: Integer);
begin
  Assert(ACurrentAreaIndex >= 0);
  FNextAreaIndex := ACurrentAreaIndex;
  inherited RestartFormattingFromTheMiddleOfSection(ASection, ACurrentAreaIndex);
end;

procedure TdxDefaultPageAreaControllerState.RestartFormattingFromTheStartOfSection(ASection: TdxSection;
  ACurrentAreaIndex: Integer);
begin
  Assert(ACurrentAreaIndex >= 0);
  FNextAreaIndex := ACurrentAreaIndex;
  inherited RestartFormattingFromTheStartOfSection(ASection, ACurrentAreaIndex);
end;

{ TdxResetSecondaryFormattingForPageArgs }

constructor TdxResetSecondaryFormattingForPageArgs.Create(APage: TdxPage; APageIndex: Integer);
begin
  inherited Create;
  FPage := APage;
  FPageIndex := APageIndex;
end;

{ TdxPageFormattingCompleteEventArgs }

constructor TdxPageFormattingCompleteEventArgs.Create(APage: TdxPage; ADocumentFormattingComplete: Boolean);
begin
  inherited Create;
  Assert(APage <> nil, 'page = nil');
  FPage := APage;
  FDocumentFormattingComplete := ADocumentFormattingComplete;
end;

{ TdxColumnsBoundsCalculator }

constructor TdxColumnsBoundsCalculator.Create(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
begin
  inherited Create;
  Assert(AUnitConverter <> nil);
  FUnitConverter := AUnitConverter;
end;

function TdxColumnsBoundsCalculator.Calculate(ASection: TdxSection; const ABounds: TRect): TdxRectList;
var
  AColumnsSettings: TdxSectionColumns;
  AColumnInfoCollection: TdxColumnInfoCollection;
begin
  Result := TdxRectList.Create;
  AColumnsSettings := ASection.Columns;
  if AColumnsSettings.EqualWidthColumns then
    PopulateEqualWidthColumnsBounds(Result, ABounds, AColumnsSettings.ColumnCount, FUnitConverter.ToLayoutUnits(AColumnsSettings.Space))
  else
  begin
    AColumnInfoCollection := AColumnsSettings.GetColumns;
    try
      PopulateColumnsBounds(Result, ABounds, AColumnInfoCollection);
    finally
      AColumnInfoCollection.Free;
    end;
  end;
end;

procedure TdxColumnsBoundsCalculator.PopulateColumnsBounds(const ARects: TdxRectList; const ABounds: TRect; const AColumnInfoCollection: TdxColumnInfoCollection);
var
  I, X, ACount: Integer;
  AColumnBounds: TRect;
begin
  X := ABounds.Left;
  ACount := AColumnInfoCollection.Count;
  for I := 0 to ACount - 1 do
  begin
    AColumnBounds := cxRectSetLeft(ABounds, X);
    AColumnBounds.Width := UnitConverter.ToLayoutUnits(AColumnInfoCollection[I].Width);
    Inc(X, AColumnBounds.Width + UnitConverter.ToLayoutUnits(AColumnInfoCollection[I].Space));
    ARects.Add(AColumnBounds);
  end;
end;

procedure TdxColumnsBoundsCalculator.PopulateEqualWidthColumnsBounds(const AColumnBounds: TdxRectList;
  const ABounds: TRect; AColumnCount, ASpaceBetweenColumns: Integer);
var
  R: TRect;
  ARects: TArray<TRect>;
begin
  Assert(AColumnCount > 0);
  R := ABounds;
  Dec(R.Right, ASpaceBetweenColumns * (AColumnCount - 1));
  ARects := TdxRectangleUtils.SplitHorizontally(R, AColumnCount);
  PopulateEqualWidthColumnsBoundsCore(AColumnBounds, ARects, ASpaceBetweenColumns);
end;

procedure TdxColumnsBoundsCalculator.PopulateEqualWidthColumnsBoundsCore(const ARects: TdxRectList; const AColumnRects: TArray<TRect>; ASpaceBetweenColumns: Integer);
var
  I: Integer;
  AColumnBounds: TRect;
begin
  Assert(Length(AColumnRects) > 0);
  ARects.Add(AColumnRects[0]);
  for I := 1 to Length(AColumnRects) - 1 do
  begin
    AColumnBounds := AColumnRects[I];
    AColumnBounds.Offset(I * ASpaceBetweenColumns, 0);
    ARects.Add(AColumnBounds);
  end;
end;

{ TdxPageBoundsCalculator }

constructor TdxPageBoundsCalculator.Create(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
begin
  inherited Create;
  Assert(AUnitConverter <> nil);
  FUnitConverter := AUnitConverter;
end;

function TdxPageBoundsCalculator.CalculatePageBounds(ASection: TdxSection): TRect;
var
  APageSettings: TdxSectionPage;
  AWidth, AHeight: Integer;
begin
  APageSettings := ASection.Page;
  AWidth := FUnitConverter.ToLayoutUnits(APageSettings.Width);
  AHeight := FUnitConverter.ToLayoutUnits(APageSettings.Height);
  Result.InitSize(0, 0, AWidth, AHeight);
end;

function TdxPageBoundsCalculator.CalculatePageClientBounds(ASection: TdxSection): TRect;
var
  APage: TdxSectionPage;
  AMargins: TdxSectionMargins;
begin
  APage := ASection.Page;
  AMargins := ASection.Margins;
  Result := CalculatePageClientBoundsCore(APage.Width, APage.Height, AMargins.Left, Abs(AMargins.Top), AMargins.Right, Abs(AMargins.Bottom));
end;

function TdxPageBoundsCalculator.CalculatePageClientBoundsCore(APageWidth, APageHeight, AMarginLeft, AMarginTop,
  AMarginRight, AMarginBottom: Integer): TRect;
var
  AWidth, AHeight, ALeft, ATop, ARight, ABottom: Integer;
begin
  AWidth := UnitConverter.ToLayoutUnits(APageWidth);
  AHeight := UnitConverter.ToLayoutUnits(APageHeight);
  ALeft := UnitConverter.ToLayoutUnits(AMarginLeft);
  ATop := UnitConverter.ToLayoutUnits(AMarginTop);
  ARight := UnitConverter.ToLayoutUnits(AMarginRight);
  ABottom := UnitConverter.ToLayoutUnits(AMarginBottom);
  Result.Init(ALeft, ATop, AWidth - ARight, AHeight - ABottom);
end;

{ TdxCalculatePageOrdinalResult }

constructor TdxCalculatePageOrdinalResult.Create(APageOrdinal, ASkippedPageCount: Integer);
begin
  PageOrdinal := APageOrdinal;
  SkippedPageCount := ASkippedPageCount;
end;

{ TdxColumnController }

constructor TdxColumnController.Create(APageAreaController: TdxPageAreaController);
begin
  Assert(APageAreaController <> nil, 'APageAreaController = nil');
  inherited Create;
  FPageAreaController := APageAreaController;
  FColumnsBounds := TdxRectList.Create;
end;

destructor TdxColumnController.Destroy;
begin
  FreeAndNil(FColumnsBounds);
  inherited Destroy;
end;

procedure TdxColumnController.AddColumn(AColumn: TdxColumn);
begin
  Columns.Add(AColumn);
end;

procedure TdxColumnController.ApplySectionStart(ASection: TdxSection);
begin
  case ASection.GeneralSettings.StartType of
    TdxSectionStartType.Continuous,
    TdxSectionStartType.EvenPage,
    TdxSectionStartType.OddPage,
    TdxSectionStartType.NextPage:
      FNextColumnIndex := 0;
    TdxSectionStartType.Column:
      if ASection.GetActualColumnsCount <> TopLevelColumnsCount then
        FNextColumnIndex := 0;
  end;
end;

procedure TdxColumnController.BeginSectionFormatting(ASection: TdxSection);
begin
  ApplySectionStart(ASection);
  CreateColumnBounds;
end;

function TdxColumnController.CalculateColumnBounds(AColumnIndex: Integer): TRect;
var
  AAreaBounds: TRect;
begin
  Result := CalculateColumnBoundsCore(AColumnIndex);
  AAreaBounds := FPageAreaController.CurrentAreaBounds;
  Result.Top := AAreaBounds.Top;
  Result.Height := AAreaBounds.Height;
end;

procedure TdxColumnController.CleanupEmptyBoxes(ALastColumn: TdxColumn);
begin
end;

procedure TdxColumnController.ClearInvalidatedContent(const APos: TdxFormatterPosition);
begin
  ClearInvalidatedContentCore(APos, Columns);
end;

procedure TdxColumnController.ClearInvalidatedContentCore(const APos: TdxFormatterPosition; AColumns: TdxColumnCollection);
var
  AColumn: TdxColumn;
  I, AColumnIndex, ACount, AParagraphIndex: Integer;
  ATables: TdxTableViewInfoCollection;
  ATableViewInfo: TdxTableViewInfo;
  ATable: TdxTable;
  AParagraph: TdxParagraphBase;
begin
  AColumnIndex := AColumns.BinarySearchBoxIndex(APos);
  if AColumnIndex < 0 then
  begin
    AColumnIndex := not AColumnIndex;
  end;
  if AColumnIndex < AColumns.Count then
  begin
    AColumn := AColumns[AColumnIndex];
    ATables := AColumn.InnerTables;
    if ATables <> nil then
    begin
      ACount := ATables.Count;
      for I := ACount - 1 downto 0 do
      begin
        ATableViewInfo := ATables[I];
        ATable := ATableViewInfo.Table;
        if ATable.Rows.Count = 0 then
          ATables.Delete(I)
        else
        begin
          AParagraphIndex := ATable.Rows.First.Cells.First.StartParagraphIndex;
          if AParagraphIndex >= ATable.PieceTable.Paragraphs.Count then
            ATables.Delete(I)
          else
          begin
            AParagraph := ATable.PieceTable.Paragraphs[AParagraphIndex];
            if AParagraph.FirstRunIndex >= APos.RunIndex then
              ATables.Delete(I);
          end;
        end;
      end;
    end;
  end;

  if AColumnIndex + 1 < AColumns.Count then
    AColumns.DeleteRange(AColumnIndex + 1, AColumns.Count - AColumnIndex - 1);
end;

procedure TdxColumnController.CreateColumnBounds;
var
  ACalculator: TdxColumnsBoundsCalculator;
begin
  ACalculator := CreateColumnBoundsCalculator;
  try
    FreeAndNil(FColumnsBounds);
    FColumnsBounds := ACalculator.Calculate(FPageAreaController.PageController.CurrentSection, FPageAreaController.CurrentAreaBounds);
  finally
    ACalculator.Free;
  end;
end;

function TdxColumnController.CreateColumnBoundsCalculator: TdxColumnsBoundsCalculator;
begin
  Result := TdxColumnsBoundsCalculator.Create(FPageAreaController.PageController.DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter);
end;

function TdxColumnController.CreateRow: TdxRow;
begin
  Result := TdxRow.Create;
end;

procedure TdxColumnController.AddInnerTable(ATableViewInfo: TdxTableViewInfo);
begin
  Assert(ATableViewInfo.Table.ParentCell = nil);
end;

function TdxColumnController.CompleteCurrentColumnFormatting(AColumn: TdxColumn): TdxCompleteFormattingResult;
begin
  if (FNextColumnIndex > 0) or (AColumn = nil) then
    Result := TdxCompleteFormattingResult.Success
  else
    Result := PageAreaController.CompleteCurrentAreaFormatting;
end;

function TdxColumnController.GetCurrentPageBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect;
begin
  Result := ACurrentPage.Bounds;
end;

function TdxColumnController.GetCurrentPageClientBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect;
begin
  Result := ACurrentPage.ClientBounds;
end;

function TdxColumnController.GetColumns: TdxColumnCollection;
begin
  Result := FPageAreaController.Areas.Last.Columns;
end;

function TdxColumnController.GetPageAreaController: TdxPageAreaController;
begin
  Result := FPageAreaController;
end;

function TdxColumnController.GetMeasurer: TdxBoxMeasurer;
begin
  Result := PageAreaController.PageController.DocumentLayout.Measurer;
end;

function TdxColumnController.GetPageLastRunIndex: TdxRunIndex;
begin
  Result := PageAreaController.PageController.PageLastRunIndex;
end;

function TdxColumnController.GetNextColumn(AColumn: TdxColumn; AKeepFloatingObjects: Boolean): TdxColumn;
begin
  if FNextColumnIndex = 0 then
    PageAreaController.GetNextPageArea(AKeepFloatingObjects);

  if ColumnsBounds.Count = 0 then
    CreateColumnBounds;

  Result := GetNextColumnCore(AColumn);
  AddColumn(Result);
  RaiseGenerateNewColumn;
end;

procedure TdxColumnController.RemoveGeneratedColumn(AColumn: TdxColumn);
begin
  Columns.Delete(Columns.Count - 1);
  if Columns.Count = 0 then
    PageAreaController.RemoveLastPageArea;
end;

function TdxColumnController.GetNextColumnCore(AColumn: TdxColumn): TdxColumn;
var
  ABounds: TRect;
begin
  Result := TdxColumn.Create;
  ABounds := CalculateColumnBounds(FNextColumnIndex);
  Result.Bounds := ABounds;
  FNextColumnIndex := (FNextColumnIndex + 1) mod ColumnsBounds.Count;
end;

function TdxColumnController.GetPreviousColumn(AColumn: TdxColumn): TdxColumn;
var
  AIndex: Integer;
begin
  AIndex := Columns.IndexOf(AColumn);
  if AIndex > 0 then
    Result := Columns[AIndex - 1]
  else
    Result := nil;
end;

function TdxColumnController.GetShouldZeroSpacingBeforeWhenMoveRowToNextColumn: Boolean;
begin
  Result := True;
end;

function TdxColumnController.GetTopLevelColumnsCount: Integer;
begin
  Result := ColumnsBounds.Count;
end;

procedure TdxColumnController.RaiseGenerateNewColumn;
begin
  if not FOnGenerateNewColumn.Empty then
    FOnGenerateNewColumn.Invoke(Self, nil);
end;

procedure TdxColumnController.Reset(ASection: TdxSection);
begin
  inherited Reset(ASection);
  ColumnsBounds.Clear;
  BeginSectionFormatting(ASection);
end;

procedure TdxColumnController.ResetToFirstColumn;
begin
  FNextColumnIndex := 0;
end;

procedure TdxColumnController.RestartFormattingFromTheMiddleOfSection(ASection: TdxSection;
  ACurrentColumnIndex: Integer);
begin
  CreateColumnBounds;
  FNextColumnIndex := (ACurrentColumnIndex + 1) mod ColumnsBounds.Count;
end;

procedure TdxColumnController.RestartFormattingFromTheStartOfRowAtCurrentPage;
begin
end;

procedure TdxColumnController.RestartFormattingFromTheStartOfSection(ASection: TdxSection;
  ACurrentColumnIndex: Integer);
begin
  CreateColumnBounds;
  FNextColumnIndex := (ACurrentColumnIndex + 1) mod ColumnsBounds.Count;
  ApplySectionStart(ASection);
end;

{ TdxParagraphFrameHorizontalPositionCalculator }

constructor TdxParagraphFrameHorizontalPositionCalculator.Create(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
end;

function TdxParagraphFrameHorizontalPositionCalculator.CalculateAbsoluteFloatingObjectX(
  const ALocation: IdxParagraphFrameLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
  AActualWidth: Integer): Integer;
var
  AAlignBounds: TRect;
begin
  if ALocation.X <> 0 then
    Result := CalculateAbsoluteFloatingObjectXCore(ALocation.HorizontalPositionType, ALocation.X, APlacementInfo)
  else
  begin
    AAlignBounds := CalculateAlignBounds(ALocation, APlacementInfo);
    Result := CalculateAbsoluteFloatingObjectHorizontalAlignmentPosition(ALocation.HorizontalPositionAlignment, AAlignBounds, AActualWidth);
  end;
end;

function TdxParagraphFrameHorizontalPositionCalculator.CalculateFloatingObjectOffsetX(
  AHorizontalPositionType: TdxParagraphFrameHorizontalPositionType; X: Integer;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  case AHorizontalPositionType of
    TdxParagraphFrameHorizontalPositionType.Page:
      Result := FUnitConverter.ToModelUnits(X - APlacementInfo.PageBounds.Left);
    TdxParagraphFrameHorizontalPositionType.Column:
      Result := FUnitConverter.ToModelUnits(X - APlacementInfo.ColumnBounds.Left);
    TdxParagraphFrameHorizontalPositionType.Margin:
      Result := FUnitConverter.ToModelUnits(X - APlacementInfo.PageClientBounds.Left);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Result := 0;
  end;
end;

function TdxParagraphFrameHorizontalPositionCalculator.CalculateAlignBounds(const ALocation: IdxParagraphFrameLocation;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): TRect;
begin
  case ALocation.HorizontalPositionType of
    TdxParagraphFrameHorizontalPositionType.Page:
      Result := APlacementInfo.PageBounds;
    TdxParagraphFrameHorizontalPositionType.Column:
      Result := APlacementInfo.OriginalColumnBounds;
    TdxParagraphFrameHorizontalPositionType.Margin:
      Result := APlacementInfo.PageClientBounds;
    else
      TdxRichEditExceptions.ThrowInternalException;
      Result.Empty;
  end;
end;

function TdxParagraphFrameHorizontalPositionCalculator.ValidateFloatingObjectX(AActualWidth: Integer;
  const APageBounds: TRect; AValue: Integer): Integer;
var
  AOverflow: Integer;
begin
  AOverflow := AValue + FUnitConverter.ToLayoutUnits(AActualWidth) - APageBounds.Right;
  if AOverflow > 0 then
    Dec(AValue, AOverflow);

  Result := Max(APageBounds.Left, AValue);
end;

function TdxParagraphFrameHorizontalPositionCalculator.CalculateAbsoluteFloatingObjectXCore(
  AHorizontalPositionType: TdxParagraphFrameHorizontalPositionType; AOffsetX: Integer;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  case AHorizontalPositionType of
    TdxParagraphFrameHorizontalPositionType.Page:
      Result := FUnitConverter.ToLayoutUnits(AOffsetX) + APlacementInfo.PageBounds.Left;
    TdxParagraphFrameHorizontalPositionType.Column:
      Result := FUnitConverter.ToLayoutUnits(AOffsetX) + APlacementInfo.ColumnBounds.Left;
    TdxParagraphFrameHorizontalPositionType.Margin:
      Result := FUnitConverter.ToLayoutUnits(AOffsetX) + APlacementInfo.PageClientBounds.Left;
    else
      TdxRichEditExceptions.ThrowInternalException;
      Result := 0;
  end;
end;

function TdxParagraphFrameHorizontalPositionCalculator.CalculateAbsoluteFloatingObjectHorizontalAlignmentPosition(
  AAlignment: TdxParagraphFrameHorizontalPositionAlignment; const AAlignBounds: TRect; AActualWidth: Integer): Integer;
begin
  case AAlignment of
    TdxParagraphFrameHorizontalPositionAlignment.None,
    TdxParagraphFrameHorizontalPositionAlignment.Outside,
    TdxParagraphFrameHorizontalPositionAlignment.Left:
      Result := AAlignBounds.Left;
    TdxParagraphFrameHorizontalPositionAlignment.Inside,
    TdxParagraphFrameHorizontalPositionAlignment.Right:
      Result := AAlignBounds.Right - FUnitConverter.ToLayoutUnits(AActualWidth);
    TdxParagraphFrameHorizontalPositionAlignment.Center:
      Result := (AAlignBounds.Right + AAlignBounds.Left) div 2 - FUnitConverter.ToLayoutUnits(AActualWidth) div 2;
    else
      TdxRichEditExceptions.ThrowInternalException;
      Result := 0;
  end;
end;

{ TdxParagraphFrameVerticalPositionCalculator }

constructor TdxParagraphFrameVerticalPositionCalculator.Create(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
end;

function TdxParagraphFrameVerticalPositionCalculator.CalculateAbsoluteFloatingObjectY(
  const ALocation: IdxParagraphFrameLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualHeight: Integer): Integer;
var
  AAlignBounds: TRect;
begin
  if ALocation.Y <> 0 then
    Result := CalculateAbsoluteFloatingObjectYCore(ALocation.VerticalPositionType, ALocation.Y, APlacementInfo)
  else
  begin
    AAlignBounds := CalculateAlignBounds(ALocation, APlacementInfo);
    Result := CalculateAbsoluteParagraphFrameVerticalAlignmentPosition(ALocation.VerticalPositionAlignment, AAlignBounds, AActualHeight);
  end;
end;

function TdxParagraphFrameVerticalPositionCalculator.ValidateY(Y: Integer; AActualHeight: Integer; const ATargetBounds: TRect): Integer;
var
  AHeight, ABottom: Integer;
begin
  AHeight := FUnitConverter.ToLayoutUnits(AActualHeight);
  ABottom := Y + AHeight;
  if ABottom >= ATargetBounds.Bottom then
    Dec(Y, ABottom - ATargetBounds.Bottom);
  if Y < ATargetBounds.Top then
    Y := ATargetBounds.Top;
  Result := Y;
end;

function TdxParagraphFrameVerticalPositionCalculator.CalculateFloatingObjectOffsetY(
  AVerticalPositionType: TdxParagraphFrameVerticalPositionType; Y: Integer;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  case AVerticalPositionType of
    TdxParagraphFrameVerticalPositionType.Paragraph:
      Result := FUnitConverter.ToModelUnits(Y - APlacementInfo.OriginY);
    TdxParagraphFrameVerticalPositionType.Page:
      Result := FUnitConverter.ToModelUnits(Y - APlacementInfo.PageBounds.Y);
    TdxParagraphFrameVerticalPositionType.Margin:
      Result := FUnitConverter.ToModelUnits(Y - APlacementInfo.ColumnBounds.Y);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Result := 0;
  end;
end;

function TdxParagraphFrameVerticalPositionCalculator.CalculateAbsoluteFloatingObjectYCore(
  AType: TdxParagraphFrameVerticalPositionType; AOffsetY: Integer;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  case AType of
    TdxParagraphFrameVerticalPositionType.Paragraph:
      Result := APlacementInfo.OriginY + FUnitConverter.ToLayoutUnits(AOffsetY);
    TdxParagraphFrameVerticalPositionType.Page:
      Result := APlacementInfo.PageBounds.Y + FUnitConverter.ToLayoutUnits(AOffsetY);
    TdxParagraphFrameVerticalPositionType.Margin:
      Result := APlacementInfo.PageClientBounds.Y + FUnitConverter.ToLayoutUnits(AOffsetY);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Result := 0;
  end;
end;

function TdxParagraphFrameVerticalPositionCalculator.CalculateAlignBounds(const ALocation: IdxParagraphFrameLocation;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): TRect;
begin
  case ALocation.VerticalPositionType of
    TdxParagraphFrameVerticalPositionType.Page:
      Result := APlacementInfo.PageBounds;
    TdxParagraphFrameVerticalPositionType.Margin:
      Result := APlacementInfo.PageClientBounds;
    TdxParagraphFrameVerticalPositionType.Paragraph:
      Result := TRect.CreateSize(0, APlacementInfo.OriginY, 0, 0);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Result.Empty;
  end;
end;

function TdxParagraphFrameVerticalPositionCalculator.CalculateAbsoluteParagraphFrameVerticalAlignmentPosition(
  AAlignment: TdxParagraphFrameVerticalPositionAlignment; const AAlignBounds: TRect; AActualHeight: Integer): Integer;
begin
  case AAlignment of
    TdxParagraphFrameVerticalPositionAlignment.None,
    TdxParagraphFrameVerticalPositionAlignment.Inline,
    TdxParagraphFrameVerticalPositionAlignment.Inside,
    TdxParagraphFrameVerticalPositionAlignment.Top:
      Result := AAlignBounds.Top;
    TdxParagraphFrameVerticalPositionAlignment.Outside,
    TdxParagraphFrameVerticalPositionAlignment.Bottom:
      Result := AAlignBounds.Bottom - FUnitConverter.ToLayoutUnits(AActualHeight);
    TdxParagraphFrameVerticalPositionAlignment.Center:
      Result := (AAlignBounds.Bottom + AAlignBounds.Top) div 2 - FUnitConverter.ToLayoutUnits(AActualHeight) div 2;
    else
      TdxRichEditExceptions.ThrowInternalException;
      Result := 0;
  end;
end;

{ TdxFloatingObjectHorizontalPositionCalculator }

function TdxFloatingObjectHorizontalPositionCalculator.CalculateAbsoluteFloatingObjectHorizontalAlignmentPosition(
  AAlignment: TdxFloatingObjectHorizontalPositionAlignment; AAlignBounds: TRect; AActualWidth: Integer): Integer;
begin
  case AAlignment of
    TdxFloatingObjectHorizontalPositionAlignment.Inside,
    TdxFloatingObjectHorizontalPositionAlignment.Left:
      Result := AAlignBounds.Left;
    TdxFloatingObjectHorizontalPositionAlignment.Outside,
    TdxFloatingObjectHorizontalPositionAlignment.Right:
      Result := AAlignBounds.Right - FUnitConverter.ToLayoutUnits(AActualWidth);
    TdxFloatingObjectHorizontalPositionAlignment.Center:
      Result := Trunc((AAlignBounds.Right + AAlignBounds.Left) / 2 - FUnitConverter.ToLayoutUnits(AActualWidth) / 2);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Exit(0);
  end;
end;

function TdxFloatingObjectHorizontalPositionCalculator.CalculateAbsoluteFloatingObjectWidth(
  const ALocation: IdxFloatingObjectLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  if not ALocation.UseRelativeWidth or (ALocation.RelativeWidth.Width = 0) then
    Result := ALocation.ActualWidth
  else
    Result := CalculateAbsoluteFloatingObjectWidthCore(ALocation, APlacementInfo);
end;

function TdxFloatingObjectHorizontalPositionCalculator.CalculateAbsoluteFloatingObjectWidthCore(
  const ALocation: IdxFloatingObjectLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
var
  ARelativeWidth: TdxFloatingObjectRelativeWidth;
begin
  ARelativeWidth := ALocation.RelativeWidth;
  Result := Trunc(ARelativeWidth.Width / 100000 * GetPercentBaseWidth(ARelativeWidth.From, APlacementInfo));
end;

function TdxFloatingObjectHorizontalPositionCalculator.CalculateAbsoluteFloatingObjectX(
  const ALocation: IdxFloatingObjectLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
  AActualWidth: Integer): Integer;
var
  AAlignBounds: TRect;
  AOffsetX, APercentBaseWidth: Integer;
begin
  if ALocation.HorizontalPositionAlignment = TdxFloatingObjectHorizontalPositionAlignment.None then
  begin
    AOffsetX := ALocation.OffsetX;
    if ALocation.PercentOffsetX <> 0 then
    begin
      APercentBaseWidth := CalculateFloatingObjectOffsetPercentBase(ALocation.HorizontalPositionType, APlacementInfo);
      AOffsetX := Trunc(ALocation.PercentOffsetX / 100000 * APercentBaseWidth);
    end;
    Result := CalculateAbsoluteFloatingObjectXCore(ALocation.HorizontalPositionType, AOffsetX, APlacementInfo);
  end
  else
  begin
    AAlignBounds := CalculateAlignBounds(ALocation, APlacementInfo);
    Result := CalculateAbsoluteFloatingObjectHorizontalAlignmentPosition(ALocation.HorizontalPositionAlignment,
      AAlignBounds, AActualWidth);
  end;
end;

function TdxFloatingObjectHorizontalPositionCalculator.CalculateAbsoluteFloatingObjectXCore(
  AHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; AOffsetX: Integer;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  case AHorizontalPositionType of
    TdxFloatingObjectHorizontalPositionType.LeftMargin,
    TdxFloatingObjectHorizontalPositionType.InsideMargin,
    TdxFloatingObjectHorizontalPositionType.Page:
      Result := FUnitConverter.ToLayoutUnits(AOffsetX) + APlacementInfo.PageBounds.Left;
    TdxFloatingObjectHorizontalPositionType.Column:
      Result := FUnitConverter.ToLayoutUnits(AOffsetX) + APlacementInfo.ColumnBounds.Left;
    TdxFloatingObjectHorizontalPositionType.Margin:
      Result := FUnitConverter.ToLayoutUnits(AOffsetX) + APlacementInfo.PageClientBounds.Left;
    TdxFloatingObjectHorizontalPositionType.OutsideMargin,
    TdxFloatingObjectHorizontalPositionType.RightMargin:
      Result := FUnitConverter.ToLayoutUnits(AOffsetX) + APlacementInfo.PageClientBounds.Right;
    TdxFloatingObjectHorizontalPositionType.Character:
      Result := FUnitConverter.ToLayoutUnits(AOffsetX) + APlacementInfo.OriginX;
    else
      TdxRichEditExceptions.ThrowInternalException;
      Exit(0);
  end;
end;

function TdxFloatingObjectHorizontalPositionCalculator.CalculateAlignBounds(const ALocation: IdxFloatingObjectLocation;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): TRect;
var
  APageBounds, APageClientBounds: TRect;
begin
  case ALocation.HorizontalPositionType of
    TdxFloatingObjectHorizontalPositionType.LeftMargin,
    TdxFloatingObjectHorizontalPositionType.InsideMargin:
      begin
        APageBounds := APlacementInfo.PageBounds;
        Result.InitSize(APageBounds.Left, APageBounds.Top, APlacementInfo.PageClientBounds.Left - APageBounds.Left, APageBounds.Height);
      end;
    TdxFloatingObjectHorizontalPositionType.Page:
        Result := APlacementInfo.PageBounds;
    TdxFloatingObjectHorizontalPositionType.Column:
        Result := APlacementInfo.OriginalColumnBounds;
    TdxFloatingObjectHorizontalPositionType.OutsideMargin,
    TdxFloatingObjectHorizontalPositionType.RightMargin:
      begin
        APageBounds := APlacementInfo.PageBounds;
        APageClientBounds := APlacementInfo.PageClientBounds;
        Result.InitSize(APageClientBounds.Right, APageBounds.Top, APageBounds.Right - APageClientBounds.Right, APageBounds.Height);
      end;
    TdxFloatingObjectHorizontalPositionType.Margin:
        Result := APlacementInfo.PageClientBounds;
    TdxFloatingObjectHorizontalPositionType.Character:
        Result.InitSize(APlacementInfo.OriginX, 0, 0, 0);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

function TdxFloatingObjectHorizontalPositionCalculator.CalculateFloatingObjectOffsetPercentBase(
  AType: TdxFloatingObjectHorizontalPositionType; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  case AType of
    TdxFloatingObjectHorizontalPositionType.Page:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageBounds.Width);
    TdxFloatingObjectHorizontalPositionType.Margin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Width);
    TdxFloatingObjectHorizontalPositionType.OutsideMargin, TdxFloatingObjectHorizontalPositionType.LeftMargin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Left - APlacementInfo.PageBounds.Left);
    TdxFloatingObjectHorizontalPositionType.InsideMargin, TdxFloatingObjectHorizontalPositionType.RightMargin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Right - APlacementInfo.PageBounds.Right);
    else
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Width);
  end;
end;

function TdxFloatingObjectHorizontalPositionCalculator.CalculateFloatingObjectOffsetX(
  AHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; X: Integer;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  case AHorizontalPositionType of
    TdxFloatingObjectHorizontalPositionType.LeftMargin,
    TdxFloatingObjectHorizontalPositionType.InsideMargin,
    TdxFloatingObjectHorizontalPositionType.Page:
      Result := FUnitConverter.ToModelUnits(X - APlacementInfo.PageBounds.Left);
    TdxFloatingObjectHorizontalPositionType.Column:
      Result := FUnitConverter.ToModelUnits(X - APlacementInfo.ColumnBounds.Left);
    TdxFloatingObjectHorizontalPositionType.Margin:
      Result := FUnitConverter.ToModelUnits(X - APlacementInfo.PageClientBounds.Left);
    TdxFloatingObjectHorizontalPositionType.OutsideMargin,
    TdxFloatingObjectHorizontalPositionType.RightMargin:
      Result := FUnitConverter.ToModelUnits(X - APlacementInfo.PageClientBounds.Right);
    else
      Result := FUnitConverter.ToModelUnits(X - APlacementInfo.OriginX);
  end;
end;

constructor TdxFloatingObjectHorizontalPositionCalculator.Create(
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
end;

function TdxFloatingObjectHorizontalPositionCalculator.GetPercentBaseWidth(
  AFrom: TdxFloatingObjectRelativeFromHorizontal; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  Result := 0;
  case AFrom of
    TdxFloatingObjectRelativeFromHorizontal.Page:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageBounds.Width);
    TdxFloatingObjectRelativeFromHorizontal.Margin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Width);
    TdxFloatingObjectRelativeFromHorizontal.OutsideMargin, TdxFloatingObjectRelativeFromHorizontal.LeftMargin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Left - APlacementInfo.PageBounds.Left);
    TdxFloatingObjectRelativeFromHorizontal.InsideMargin, TdxFloatingObjectRelativeFromHorizontal.RightMargin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageBounds.Right - APlacementInfo.PageClientBounds.Right);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;


{ TdxFloatingObjectVerticalPositionCalculator }

function TdxFloatingObjectVerticalPositionCalculator.CalculateAbsoluteFloatingObjectHeight(
  const ALocation: IdxFloatingObjectLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  if not ALocation.UseRelativeHeight or (ALocation.RelativeHeight.Height = 0) then
    Result := ALocation.ActualHeight
  else
    Result := CalculateAbsoluteFloatingObjectHeightCore(ALocation, APlacementInfo);
end;

function TdxFloatingObjectVerticalPositionCalculator.CalculateAbsoluteFloatingObjectHeightCore(
  const ALocation: IdxFloatingObjectLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
var
  ARelativeHeight: TdxFloatingObjectRelativeHeight;
begin
  ARelativeHeight := ALocation.RelativeHeight;
  Result :=  Trunc(ARelativeHeight.Height / 100000.0 * GetPercentBaseHeight(ARelativeHeight.From, APlacementInfo));
end;

function TdxFloatingObjectVerticalPositionCalculator.CalculateAbsoluteFloatingObjectVerticalAlignmentPosition(
  AAlignment: TdxFloatingObjectVerticalPositionAlignment; const AAlignBounds: TRect; AActualHeight: Integer): Integer;
begin
  case AAlignment of
    TdxFloatingObjectVerticalPositionAlignment.Inside,
    TdxFloatingObjectVerticalPositionAlignment.Top:
      Result := AAlignBounds.Top;
    TdxFloatingObjectVerticalPositionAlignment.Outside,
    TdxFloatingObjectVerticalPositionAlignment.Bottom:
      Result := AAlignBounds.Bottom - FUnitConverter.ToLayoutUnits(AActualHeight);
    TdxFloatingObjectVerticalPositionAlignment.Center:
      Result := Trunc((AAlignBounds.Bottom + AAlignBounds.Top) / 2 - FUnitConverter.ToLayoutUnits(AActualHeight) / 2);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Exit(0);
  end;
end;

function TdxFloatingObjectVerticalPositionCalculator.CalculateAbsoluteFloatingObjectY(
  const ALocation: IdxFloatingObjectLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
  AActualHeight: Integer): Integer;
var
  AAlignBounds: TRect;
  AOffsetY, APercentBaseWidth: Integer;
begin
  if ALocation.VerticalPositionAlignment = TdxFloatingObjectVerticalPositionAlignment.None then
  begin
    AOffsetY := ALocation.OffsetY;
    if ALocation.PercentOffsetY <> 0 then
    begin
      APercentBaseWidth := CalculateFloatingObjectOffsetPercentBase(ALocation.VerticalPositionType, APlacementInfo);
      AOffsetY := Trunc(ALocation.PercentOffsetY / 100000 * APercentBaseWidth);
    end;
    Result := CalculateAbsoluteFloatingObjectYCore(ALocation.VerticalPositionType, AOffsetY, APlacementInfo);
  end
  else
  begin
    AAlignBounds := CalculateAlignBounds(ALocation, APlacementInfo);
    Result := CalculateAbsoluteFloatingObjectVerticalAlignmentPosition(ALocation.VerticalPositionAlignment,
      AAlignBounds, AActualHeight);
  end;
end;

function TdxFloatingObjectVerticalPositionCalculator.CalculateAbsoluteFloatingObjectYCore(
  AType: TdxFloatingObjectVerticalPositionType; AOffsetY: Integer;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  case AType of
    TdxFloatingObjectVerticalPositionType.Paragraph,
    TdxFloatingObjectVerticalPositionType.Line:
      Result := APlacementInfo.OriginY + FUnitConverter.ToLayoutUnits(AOffsetY);
    TdxFloatingObjectVerticalPositionType.Page,
    TdxFloatingObjectVerticalPositionType.InsideMargin,
    TdxFloatingObjectVerticalPositionType.TopMargin:
      Result := APlacementInfo.PageBounds.Top + FUnitConverter.ToLayoutUnits(AOffsetY);
    TdxFloatingObjectVerticalPositionType.OutsideMargin,
    TdxFloatingObjectVerticalPositionType.BottomMargin:
      Result := APlacementInfo.PageBounds.Bottom + FUnitConverter.ToLayoutUnits(AOffsetY);
    TdxFloatingObjectVerticalPositionType.Margin:
      Result := APlacementInfo.ColumnBounds.Top + FUnitConverter.ToLayoutUnits(AOffsetY);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Exit(0);
  end;
end;

function TdxFloatingObjectVerticalPositionCalculator.CalculateAlignBounds(const ALocation: IdxFloatingObjectLocation;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): TRect;
var
  APageBounds, APageClientBounds: TRect;
begin
  case ALocation.VerticalPositionType of
    TdxFloatingObjectVerticalPositionType.TopMargin,
    TdxFloatingObjectVerticalPositionType.InsideMargin:
      begin
        APageBounds := APlacementInfo.PageBounds;
        APageClientBounds := APlacementInfo.PageClientBounds;
        Result := TRect.CreateSize(APageBounds.Left, APageBounds.Top, APageBounds.Width,
          APageClientBounds.Top - APageBounds.Top);
      end;
    TdxFloatingObjectVerticalPositionType.Page:
      Result := APlacementInfo.PageBounds;
    TdxFloatingObjectVerticalPositionType.OutsideMargin,
    TdxFloatingObjectVerticalPositionType.BottomMargin:
      begin
        APageBounds := APlacementInfo.PageBounds;
        APageClientBounds := APlacementInfo.PageClientBounds;
        Result := TRect.CreateSize(APageBounds.Left, APageClientBounds.Bottom, APageBounds.Width,
          APageBounds.Bottom - APageClientBounds.Bottom);
      end;
    TdxFloatingObjectVerticalPositionType.Margin:
      Result := APlacementInfo.PageClientBounds;
    TdxFloatingObjectVerticalPositionType.Line,
    TdxFloatingObjectVerticalPositionType.Paragraph:
      Result := TRect.CreateSize(0, APlacementInfo.OriginY, 0, 0);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Exit(TRect.Null);
  end;
end;

function TdxFloatingObjectVerticalPositionCalculator.CalculateFloatingObjectOffsetPercentBase(
  AType: TdxFloatingObjectVerticalPositionType; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  case AType of
    TdxFloatingObjectVerticalPositionType.Page:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageBounds.Height);
    TdxFloatingObjectVerticalPositionType.Margin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Height);
    TdxFloatingObjectVerticalPositionType.OutsideMargin,
      TdxFloatingObjectVerticalPositionType.InsideMargin,
      TdxFloatingObjectVerticalPositionType.TopMargin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Top - APlacementInfo.PageBounds.Top);
    TdxFloatingObjectVerticalPositionType.BottomMargin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Bottom - APlacementInfo.PageBounds.Bottom);
    else
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Height);
  end;
end;

function TdxFloatingObjectVerticalPositionCalculator.CalculateFloatingObjectOffsetY(
  AVerticalPositionType: TdxFloatingObjectVerticalPositionType; Y: Integer;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  case AVerticalPositionType of
    TdxFloatingObjectVerticalPositionType.Paragraph,
    TdxFloatingObjectVerticalPositionType.Line:
      Result := FUnitConverter.ToModelUnits(Y - APlacementInfo.OriginY);
    TdxFloatingObjectVerticalPositionType.Page,
    TdxFloatingObjectVerticalPositionType.OutsideMargin,
    TdxFloatingObjectVerticalPositionType.InsideMargin,
    TdxFloatingObjectVerticalPositionType.TopMargin:
      Result := FUnitConverter.ToModelUnits(Y - APlacementInfo.PageBounds.Top);
    TdxFloatingObjectVerticalPositionType.BottomMargin:
      Result := FUnitConverter.ToModelUnits(Y - APlacementInfo.PageBounds.Bottom);
    TdxFloatingObjectVerticalPositionType.Margin:
      Result := FUnitConverter.ToModelUnits(Y - APlacementInfo.ColumnBounds.Top);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Exit(0);
  end;
end;

constructor TdxFloatingObjectVerticalPositionCalculator.Create(
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
end;

function TdxFloatingObjectVerticalPositionCalculator.GetPercentBaseHeight(AFrom: TdxFloatingObjectRelativeFromVertical;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
begin
  Result := 0;
  case AFrom of
    TdxFloatingObjectRelativeFromVertical.Page:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageBounds.Height);
    TdxFloatingObjectRelativeFromVertical.Margin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Height);
    TdxFloatingObjectRelativeFromVertical.OutsideMargin,
      TdxFloatingObjectRelativeFromVertical.InsideMargin,
      TdxFloatingObjectRelativeFromVertical.TopMargin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageClientBounds.Top - APlacementInfo.PageBounds.Top);
    TdxFloatingObjectRelativeFromVertical.BottomMargin:
      Result := FUnitConverter.ToModelUnits(APlacementInfo.PageBounds.Bottom - APlacementInfo.PageClientBounds.Bottom);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

function TdxFloatingObjectVerticalPositionCalculator.ValidateY(Y, AActualHeight: Integer;
  const ATargetBounds: TRect): Integer;
var
  AHeight, ABottom: Integer;
begin
  AHeight := FUnitConverter.ToLayoutUnits(AActualHeight);
  ABottom := Y + AHeight;
  if ABottom >= ATargetBounds.Bottom then
    Y := Y - ABottom - ATargetBounds.Bottom;
  if Y < ATargetBounds.Top then
    Y := ATargetBounds.Top;
  Result := Y;
end;

{ TdxFloatingObjectsCurrentHorizontalPositionController.TPositionInfo }

constructor TdxFloatingObjectsCurrentHorizontalPositionController.TPositionInfo.Create(APosition, ATextAreaIndex: Integer);
begin
  Position := APosition;
  TextAreaIndex := ATextAreaIndex;
end;

{ TdxFloatingObjectsCurrentHorizontalPositionController }

constructor TdxFloatingObjectsCurrentHorizontalPositionController.Create(ARowsController: TdxRowsController; APosition: Integer);
begin
  inherited Create(ARowsController, APosition);
  FMaxRowHeight := -1;
  FMinLeftArea := ARowsController.DocumentModel.LayoutUnitConverter.TwipsToLayoutUnits(180);
  FMinArea := ARowsController.DocumentModel.LayoutUnitConverter.TwipsToLayoutUnits(144);
end;

constructor TdxFloatingObjectsCurrentHorizontalPositionController.Create(ARowsController: TdxRowsController);
begin
  inherited Create(ARowsController);
  FMaxRowHeight := -1;
  FMinLeftArea := ARowsController.DocumentModel.LayoutUnitConverter.TwipsToLayoutUnits(180);
  FMinArea := ARowsController.DocumentModel.LayoutUnitConverter.TwipsToLayoutUnits(144);
end;

destructor TdxFloatingObjectsCurrentHorizontalPositionController.Destroy;
begin
  FreeAndNil(FTextAreas);
  inherited Destroy;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.AdvanceHorizontalPositionToNextTextArea: Boolean;
begin
  if FCurrentTextAreaIndex + 1 >= FTextAreas.Count then
    Exit(False);
  Inc(FCurrentTextAreaIndex);
  InnerCurrentHorizontalPosition := FTextAreas[FCurrentTextAreaIndex].Start;
  Result := True;
end;

procedure TdxFloatingObjectsCurrentHorizontalPositionController.AppendRowBoxRange(ABoxRanges: TdxRowBoxRangeCollection;
  AFirstBoxIndex, ALastBoxIndex, ATextAreaIndex: Integer);
var
  ACurrentRow: TdxRow;
  ALeft, ARight: Integer;
  ACurrentRowBounds, ABounds: TRect;
begin
  ACurrentRow := RowsController.CurrentRow;
  ATextAreaIndex := Min(FTextAreas.Count - 1, ATextAreaIndex);
  ALeft := Max(FTextAreas[ATextAreaIndex].Start, ACurrentRow.Boxes[AFirstBoxIndex].Bounds.Left);
  ARight := FTextAreas[ATextAreaIndex].&End;
  ACurrentRowBounds := ACurrentRow.Bounds;
  ABounds := TRect.CreateSize(ALeft, ACurrentRowBounds.Top, Max(ARight - ALeft, 0), ACurrentRowBounds.Height);
  ABoxRanges.Add(TdxRowBoxRange.Create(AFirstBoxIndex, ALastBoxIndex, ABounds));
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.AreTextAreasChanged(APrevTextAreas,
  ATextAreas: TList<TdxTextArea>): Boolean;
var
  I, ACount: Integer;
begin
  if APrevTextAreas = nil then
    Exit(True);

  ACount := ATextAreas.Count;
  if ACount <> APrevTextAreas.Count then
    Exit(True);
  for I := 0 to ACount - 1 do
    if (APrevTextAreas[I].Start <> ATextAreas[I].Start) or (APrevTextAreas[I].&End <> ATextAreas[I].&End) then
      Exit(True);

  Result := False;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.CalculateBoxBounds(ABoxInfo: TdxBoxInfo): TRect;
var
  I, ATextOffset: Integer;
begin
  if FTextAreas.Count <= 0 then
  begin
    CanFitCurrentRowToColumn(ABoxInfo.Size.cy);
    if not (ABoxInfo.Box is TdxSpaceBoxa) and not (ABoxInfo.Box is TdxSingleSpaceBox) then
      RowsController.UpdateCurrentRowHeight(ABoxInfo);
    if FTextAreas.Count <= 0 then
      Exit(inherited CalculateBoxBounds(ABoxInfo));
    Assert(FTextAreas.Count > 0);
  end;
  if (CurrentHorizontalPosition + ABoxInfo.Size.Width > FTextAreas[FCurrentTextAreaIndex].&End) and
    ShouldBeMovedToNextTextArea(TdxBox(ABoxInfo.Box)) then
    for I := FCurrentTextAreaIndex + 1 to FTextAreas.Count - 1 do
    begin
      ATextOffset := GetTextOffset(I);
      InnerCurrentHorizontalPosition := GetInitialPosition(I, ATextOffset);
      FCurrentTextAreaIndex := I;
      if CanFitBoxToTextArea(I, ABoxInfo.Size) then
        Break;
    end;
  Result := inherited CalculateBoxBounds(ABoxInfo);
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.GetMaxBoxWidth: Integer;
var
  ACount, I: Integer;
begin
  if RowsController.SuppressHorizontalOverfull then
    Exit(MaxInt div 2);

  if FTextAreas.Count <= 0 then
    Exit(0);

  Result := FTextAreas[FCurrentTextAreaIndex].&End - CurrentHorizontalPosition;
  ACount := FTextAreas.Count;
  for I := FCurrentTextAreaIndex + 1 to ACount - 1 do
    Result := Max(Result, FTextAreas[I].Width);
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.CalculateTextAreas(
  AParagraphFrameItems: TdxParagraphFrameBoxList; AFloatingObjectItems: TdxFloatingObjectBoxList;
  const ABounds: TRect): TList<TdxTextArea>;
var
  I, ALeft, ARight: Integer;
  AProcessedParagraphFrames: TdxParagraphFrameBoxList;
  AProcessedFloatingObjects: TdxFloatingObjectBoxList;
  ATextAreas: TdxTextAreaCollectionEx;
  AKeepInitialArea: Boolean;
begin
  ATextAreas := TdxTextAreaCollectionEx.Create;
  try
    Result := ATextAreas.InnerList;
    AProcessedParagraphFrames := TdxParagraphFrameBoxList.Create;
    try
      AProcessedFloatingObjects := TdxFloatingObjectBoxList.Create;
      try
        ALeft := Min(ABounds.Left, ABounds.Right);
        ARight := Max(ALeft + 1, ABounds.Right);
        ATextAreas.Add(TdxTextArea.Create(ALeft, ARight));
        for I := 0 to AParagraphFrameItems.Count - 1 do
          RowsController.ParagraphFramesLayout.ProcessParagraphFrame(AParagraphFrameItems[I], AProcessedParagraphFrames,
            ATextAreas, ABounds);
        for I := 0 to AFloatingObjectItems.Count - 1 do
          RowsController.FloatingObjectsLayout.ProcessFloatingObject(AFloatingObjectItems[I], AProcessedFloatingObjects, ATextAreas, ABounds);
      finally
        AProcessedFloatingObjects.Free;
      end;
    finally
      AProcessedParagraphFrames.Free;
    end;
    ATextAreas.Sort;
    AKeepInitialArea := ((Result.Count = 1) and (Result[0].Start = ALeft)) and (Result[0].&End = ARight);
    if not AKeepInitialArea then
      RemoveShortAreas(Result, ALeft, ARight);
  finally
    ATextAreas.DontOwnInnerList;
    ATextAreas.Free;
  end;
end;

procedure TdxFloatingObjectsCurrentHorizontalPositionController.RemoveShortAreas(AAreas: TList<TdxTextArea>; ALeft: Integer; ARight: Integer);
var
  ACount, I, ALength: Integer;
  AArea: TdxTextArea;
begin
  if ARight - ALeft <= Min(FMinArea, FMinLeftArea) then
    Exit;
  ACount := AAreas.Count;
  for I := ACount - 1 downto 0 do
  begin
    AArea := AAreas[I];
    ALength := AArea.&End - AArea.Start;
    if (((AArea.Start = ALeft) and (ALength < FMinLeftArea))) or (((AArea.Start <> ALeft) and (ALength < FMinArea))) then
      AAreas.Delete(I);
  end;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.CanFitBoxToCurrentRow(const ABoxSize: TSize): Boolean;
var
  I: Integer;
begin
  if RowsController.SuppressHorizontalOverfull then
    Exit(True);

  if FTextAreas.Count <= 0 then
    Exit(False);

  if CurrentHorizontalPosition + ABoxSize.Width <= FTextAreas[FCurrentTextAreaIndex].&End then
    Exit(True);

  for I := FCurrentTextAreaIndex + 1 to FTextAreas.Count - 1 do
    if CanFitBoxToTextArea(I, ABoxSize) then
      Exit(True);

  Result := False;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.CanFitBoxToTextArea(ATextAreaIndex: Integer; const ABoxSize: TSize): Boolean;
begin
  Result := GetInitialPosition(ATextAreaIndex) + ABoxSize.Width <= FTextAreas[ATextAreaIndex].&End;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.CanFitCurrentRowToColumn(
  ANewRowHeight: Integer): TdxCanFitCurrentRowToColumnResult;
var
  ACurrentRowBounds: TRect;
  ATextAreasRecreated, ARecreated: Boolean;
begin
  Result := inherited CanFitCurrentRowToColumn(ANewRowHeight);
  if (ANewRowHeight <= FMaxRowHeight) and (FTextAreas.Count > 0) then
    Exit;

  ACurrentRowBounds := RowsController.CurrentRow.Bounds;
  ATextAreasRecreated := DoCurrentRowHeightChanged(ANewRowHeight, ACurrentRowBounds, False, False);

  while True do
  begin
    if FTextAreas.Count > 0 then
    begin
      RowsController.CurrentRow.Bounds := ACurrentRowBounds;
      if ATextAreasRecreated then
        Result := TdxCanFitCurrentRowToColumnResult.TextAreasRecreated;
      Exit;
    end;

    ACurrentRowBounds.Y := ACurrentRowBounds.Y + 1;
    ACurrentRowBounds.Height := RowsController.DefaultRowHeight;
    if ACurrentRowBounds.Bottom > RowsController.CurrentColumn.Bounds.Bottom then
    begin
      if RowsController.CurrentColumn.Rows.Count = 0 then
        ATextAreasRecreated := DoCurrentRowHeightChanged(ANewRowHeight, ACurrentRowBounds, False, True);
      RowsController.CurrentRow.Bounds := ACurrentRowBounds;
      if ATextAreasRecreated then
        Result := TdxCanFitCurrentRowToColumnResult.TextAreasRecreated;
      Exit;
    end;

    ARecreated := DoCurrentRowHeightChanged(ANewRowHeight, ACurrentRowBounds, False, False);
    ATextAreasRecreated := ATextAreasRecreated or ARecreated;
  end;

end;

function TdxFloatingObjectsCurrentHorizontalPositionController.CanFitNumberingListBoxToCurrentRow(
  const ABoxSize: TSize): Boolean;
begin
  if RowsController.SuppressHorizontalOverfull then
    Exit(True);

  if FTextAreas.Count <= 0 then
    Exit(False);

  Result := CurrentHorizontalPosition + ABoxSize.Width <= FTextAreas[FCurrentTextAreaIndex].&End;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.GetTextAreaForTable: TdxTextArea;
begin
  if (FTextAreas = nil) or (FTextAreas.Count < 1) then
  begin
    Assert(False);
    Result := inherited GetTextAreaForTable;
  end
  else
    Result := FTextAreas[0];
end;

procedure TdxFloatingObjectsCurrentHorizontalPositionController.IncrementCurrentHorizontalPosition(ADelta: Integer);
begin
  inherited IncrementCurrentHorizontalPosition(ADelta);
  if UpdateCurrentTextAreaIndex then
  begin
    Assert(not IsCurrentRowEmpty);
    InnerCurrentHorizontalPosition := GetInitialPosition(FCurrentTextAreaIndex, 0);
  end;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.MoveCurrentRowDownToFitTable(ATableWidth,
  ATableTop: Integer): Integer;
var
  Y, AMaxY: Integer;
  ATopTableRowBounds: TRect;
  ATextAreasRecreated, ATableFit: Boolean;
begin
  AMaxY := RowsController.CurrentColumn.Bounds.Bottom;
  ATopTableRowBounds := RowsController.CurrentColumn.Bounds;
  ATopTableRowBounds.Height := 0;
  ATopTableRowBounds.Y := ATableTop;
  for Y := ATableTop to AMaxY do
  begin
    ATextAreasRecreated := DoCurrentRowHeightChanged(0, ATopTableRowBounds, False, False);
    if ATextAreasRecreated or (Y = ATableTop) then
    begin
      ATableFit := (FTextAreas.Count = 1) and (FTextAreas[0].Width >= ATableWidth);
      if ATableFit then
        Exit(ATopTableRowBounds.Top);
    end;
    ATopTableRowBounds.Y := ATopTableRowBounds.Top + 1;
  end;
  ATopTableRowBounds.Y := ATableTop;
  DoCurrentRowHeightChanged(0, ATopTableRowBounds, False, True);
  Result := ATableTop;
end;

procedure TdxFloatingObjectsCurrentHorizontalPositionController.OnCurrentRowFinished;
var
  ACurrentRow: TdxRow;
  ABoxes: TdxBoxCollection;
  AComparable: TdxTextAreaAndXComparable;
  I, ACount, AFirstBoxIndex, ATextAreaIndex: Integer;
begin
  FMaxRowHeight := -1;
  if FTextAreas.Count <= 0 then
    Exit;

  ACurrentRow := RowsController.CurrentRow;
  if FTextAreas.Count = 1 then
  begin
    TryCreateDefaultBoxRange(ACurrentRow);
    Exit;
  end;

  ABoxes := ACurrentRow.Boxes;
  ACount := ABoxes.Count;
  if ACount <= 0 then
    Exit;

  AFirstBoxIndex := 0;
  AComparable := TdxTextAreaAndXComparable.Create(ABoxes[AFirstBoxIndex].Bounds.Left);
  try
    TdxAlgorithms<TdxTextArea>.BinarySearch(FTextAreas, AComparable, ATextAreaIndex);
    Assert(ATextAreaIndex >= 0);
  finally
    AComparable.Free;
  end;
  for I := 1 to ACount - 1 do
    if ABoxes[I - 1].Bounds.Right <> ABoxes[I].Bounds.Left then
    begin
      AppendRowBoxRange(ACurrentRow.BoxRanges, AFirstBoxIndex, I - 1, ATextAreaIndex);
      AFirstBoxIndex := I;
      AComparable := TdxTextAreaAndXComparable.Create(ABoxes[I].Bounds.Left);
      try
        TdxAlgorithms<TdxTextArea>.BinarySearch(FTextAreas, AComparable, ATextAreaIndex);
        Assert(ATextAreaIndex >= 0);
      finally
        AComparable.Free;
      end;
    end;
  AppendRowBoxRange(ACurrentRow.BoxRanges, AFirstBoxIndex, ACount - 1, ATextAreaIndex);
end;

procedure TdxFloatingObjectsCurrentHorizontalPositionController.OnCurrentRowHeightChanged(AKeepTextAreas: Boolean);
var
  ACurrentRowBounds: TRect;
begin
  ACurrentRowBounds := RowsController.CurrentRow.Bounds;
  DoCurrentRowHeightChanged(ACurrentRowBounds.Height, ACurrentRowBounds, AKeepTextAreas, False);
end;

procedure TdxFloatingObjectsCurrentHorizontalPositionController.RollbackCurrentHorizontalPositionTo(AValue: Integer);
begin
  inherited RollbackCurrentHorizontalPositionTo(AValue);
  UpdateCurrentTextAreaIndex;
end;

procedure TdxFloatingObjectsCurrentHorizontalPositionController.SetCurrentHorizontalPosition(AValue: Integer);
begin
  inherited SetCurrentHorizontalPosition(AValue);
  UpdateCurrentTextAreaIndex;
end;

procedure TdxFloatingObjectsCurrentHorizontalPositionController.SetCurrentRowInitialHorizontalPosition;
begin
  if FTextAreas.Count > 0 then
    DoSetCurrentHorizontalPosition(GetAvailableTextAreaInitialPosition(0))
  else
    inherited SetCurrentRowInitialHorizontalPosition;
end;

procedure TdxFloatingObjectsCurrentHorizontalPositionController.DoSetCurrentHorizontalPosition(const APosition: TPositionInfo);
begin
  FCurrentTextAreaIndex := APosition.TextAreaIndex;
  InnerCurrentHorizontalPosition := APosition.Position;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.GetAvailableTextAreaInitialPosition(APreferedTextAreaIndex: Integer): TPositionInfo;
var
  ACount, ATextAreaIndex, ATextOffset, AIndex, AStartPosition: Integer;
begin
  ACount := FTextAreas.Count;
  AIndex := APreferedTextAreaIndex;
  repeat
    ATextAreaIndex := AIndex;
    ATextOffset := GetTextOffset(ATextAreaIndex);
    if ATextOffset < FTextAreas[ATextAreaIndex].Width then
      Break;
    Inc(AIndex);
  until AIndex >= ACount;
  AStartPosition := GetInitialPosition(ATextAreaIndex, ATextOffset);
  Result := TPositionInfo.Create(AStartPosition, ATextAreaIndex);
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.ShouldBeMovedToNextTextArea(ABox: TdxBox): Boolean;
begin
  Result := not (
    Supports(ABox, IdxSpaceBox) or
    (ABox is TdxParagraphMarkBox) or
    (ABox is TdxLineBreakBox) or
    (ABox is TdxSectionMarkBox) or
    (ABox is TdxPageBreakBox) or
    (ABox is TdxColumnBreakBox));
end;

procedure TdxFloatingObjectsCurrentHorizontalPositionController.TryCreateDefaultBoxRange(ACurrentRow: TdxRow);
begin
  if (FTextAreas.Count = 1) and (FTextAreas[0].Start = ACurrentRow.Bounds.Left) and (FTextAreas[0].&End = ACurrentRow.Bounds.Right) then
    Exit;

  AppendRowBoxRange(ACurrentRow.BoxRanges, 0, ACurrentRow.Boxes.Count - 1, 0);
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.UpdateCurrentTextAreaIndex: Boolean;
var
  ANewTextAreaIndex: Integer;
  AComparable: TdxTextAreaAndXComparable;
begin
  AComparable := TdxTextAreaAndXComparable.Create(InnerCurrentHorizontalPosition);
  try
    if not TdxAlgorithms<TdxTextArea>.BinarySearch(FTextAreas, AComparable, ANewTextAreaIndex) then
    begin
      if ANewTextAreaIndex >= FTextAreas.Count then
        Exit(False);
    end;
  finally
    AComparable.Free;
  end;
  if FCurrentTextAreaIndex <> ANewTextAreaIndex then
  begin
    FCurrentTextAreaIndex := ANewTextAreaIndex;
    Result := True;
  end
  else
    Result := False;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.IsCurrentRowEmpty: Boolean;
begin
  Result := RowsController.CurrentRow.Boxes.Count = 0;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.GetTextOffset(ATextAreaIndex: Integer): Integer;
begin
  if IsCurrentRowEmpty then
  begin
    if FTextAreas[ATextAreaIndex].Start = RowsController.CurrentRow.Bounds.Left then
      Result := 0
    else
      Result := RowsController.CurrentRowIndentAfterFloatingObject;
  end
  else
    Result := 0;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.GetInitialPosition(ATextAreaIndex: Integer): Integer;
begin
  Result := GetInitialPosition(ATextAreaIndex, GetTextOffset(ATextAreaIndex));
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.GetInitialPosition(ATextAreaIndex, ATextOffset: Integer): Integer;
begin
  Result := FTextAreas[ATextAreaIndex].Start + ATextOffset;
end;

function TdxFloatingObjectsCurrentHorizontalPositionController.DoCurrentRowHeightChanged(AHeight: Integer;
  const ACurrentRowBounds: TRect; AKeepTextAreas, AIgnoreFloatingObjects: Boolean): Boolean;
var
  ALayout: TdxFloatingObjectsLayout;
  AFloatingObjects: TdxFloatingObjectBoxList;
  AParagraphFramesLayout: TdxParagraphFramesLayout;
  AParagraphFrames: TdxParagraphFrameBoxList;
  APrevTextAreas: TList<TdxTextArea>;
  ABounds: TRect;
  AInitialPosition: TPositionInfo;
begin
  ABounds := ACurrentRowBounds;
  ALayout := RowsController.FloatingObjectsLayout;
  ABounds.Height := AHeight;
  if AIgnoreFloatingObjects then
    AFloatingObjects := TdxFloatingObjectBoxList.Create
  else
    AFloatingObjects := ALayout.GetObjectsInRectangle(ABounds);
  try
    AParagraphFramesLayout := RowsController.ParagraphFramesLayout;
    if AIgnoreFloatingObjects then
      AParagraphFrames := TdxParagraphFrameBoxList.Create
    else
      AParagraphFrames := AParagraphFramesLayout.GetObjectsInRectangle(ABounds);
    try
      APrevTextAreas := FTextAreas;
      try
        if (APrevTextAreas = nil) or not AKeepTextAreas then
          FTextAreas := CalculateTextAreas(AParagraphFrames, AFloatingObjects, ABounds);

        if FTextAreas.Count > 0 then
        begin
          AInitialPosition := GetAvailableTextAreaInitialPosition(0);
          InnerCurrentHorizontalPosition := Max(InnerCurrentHorizontalPosition, AInitialPosition.Position);
          UpdateCurrentTextAreaIndex;
        end;
        Result := AreTextAreasChanged(APrevTextAreas, FTextAreas);
        if Result then
          FMaxRowHeight := Max(AHeight, FMaxRowHeight);
      finally
        if APrevTextAreas <> FTextAreas then
          APrevTextAreas.Free;
      end;
    finally
      AParagraphFrames.Free;
    end;
  finally
    AFloatingObjects.Free;
  end;
end;

{ TdxFloatingObjectSizeController }

constructor TdxFloatingObjectSizeController.Create(APieceTable: TdxPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

function TdxFloatingObjectSizeController.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

procedure TdxFloatingObjectSizeController.UpdateFloatingObjectBox(AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox);
var
  AObjectAnchorRun: TdxFloatingObjectAnchorRun;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
  AIsTextBox, AAutoHeight: Boolean;
  AFloatingObjectProperties: TdxFloatingObjectProperties;
  ARotatedLocation, ALocation: IdxFloatingObjectLocation;
  ARotatedShapeBounds, AShapeBounds, ARect {INDENTIFIER REDECLARED}, AActualSizeBounds: TRect;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AOutlineWidth: Integer;
begin
  AObjectAnchorRun := AFloatingObjectAnchorBox.GetFloatingObjectRun(PieceTable);

  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(AObjectAnchorRun.Content);
  AIsTextBox := ATextBoxContent <> nil;
  if AIsTextBox then
    AAutoHeight := ATextBoxContent.TextBoxProperties.ResizeShapeToFitText
  else
    AAutoHeight := False;
  AFloatingObjectProperties := AObjectAnchorRun.FloatingObjectProperties;
  ARotatedLocation := CreateRotatedFloatingObjectLocation(AFloatingObjectProperties, AObjectAnchorRun.Shape.Rotation, AAutoHeight);
  ARotatedShapeBounds := CalculateAbsoluteFloatingObjectShapeBounds(ARotatedLocation, AObjectAnchorRun.Shape, AIsTextBox);
  ALocation := CreateFloatingObjectLocation(AFloatingObjectProperties, AAutoHeight);
  AShapeBounds := CalculateAbsoluteFloatingObjectShapeBounds(ALocation, AObjectAnchorRun.Shape, AIsTextBox);

  if AFloatingObjectProperties.HorizontalPositionAlignment <> TdxFloatingObjectHorizontalPositionAlignment.None then
    AShapeBounds := CenterRectangleHorizontally(AShapeBounds, ARotatedShapeBounds)
  else
  begin
    ARect := CenterRectangleHorizontally(ARotatedShapeBounds, AShapeBounds);
    ARotatedShapeBounds := ValidateRotatedShapeHorizontalPosition(ARect, AFloatingObjectProperties);
    AShapeBounds := CenterRectangleHorizontally(AShapeBounds, ARotatedShapeBounds);
  end;

  if AFloatingObjectProperties.VerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.None then
    AShapeBounds := CenterRectangleVertically(AShapeBounds, ARotatedShapeBounds)
  else
  begin
    ARect := CenterRectangleVertically(ARotatedShapeBounds, AShapeBounds);
    ARotatedShapeBounds := ValidateRotatedShapeVerticalPosition(ARect, AFloatingObjectProperties);
    AShapeBounds := CenterRectangleVertically(AShapeBounds, ARotatedShapeBounds);
  end;

  AFloatingObjectAnchorBox.ShapeBounds := AShapeBounds;
  AFloatingObjectAnchorBox.ContentBounds := CalculateAbsoluteFloatingObjectContentBounds(AFloatingObjectProperties, AObjectAnchorRun.Shape, AFloatingObjectAnchorBox.ShapeBounds);
  AFloatingObjectAnchorBox.ActualBounds := CalculateActualAbsoluteFloatingObjectBounds(AFloatingObjectProperties, ARotatedShapeBounds);

  if AIsTextBox then
  begin
    AConverter := DocumentModel.ToDocumentLayoutUnitConverter;
    AActualSizeBounds := AFloatingObjectAnchorBox.ShapeBounds;
    AOutlineWidth := AConverter.ToLayoutUnits(AObjectAnchorRun.Shape.OutlineWidth);
    AActualSizeBounds.Y := AActualSizeBounds.Y + AOutlineWidth div 2;
    AActualSizeBounds.Height := AActualSizeBounds.Height - AOutlineWidth;
    AFloatingObjectAnchorBox.SetActualSizeBounds(AActualSizeBounds);
  end
  else
    AFloatingObjectAnchorBox.SetActualSizeBounds(AFloatingObjectAnchorBox.ContentBounds);
end;

procedure TdxFloatingObjectSizeController.UpdateParagraphFrameBox(AParagraphFrameBox: TdxParagraphFrameBox);
var
  AParagraphProperties: TdxParagraphProperties;
  AFrameProperties: TdxMergedFrameProperties;
  AShapeBounds, ANewShapeBounds: TRect;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ALeftBorderWidthWithOffset, ARightBorderWidthWithOffset, ATopBorderWidthWithOffset, ABottomBorderWidthWithOffset, APadding, AHorizontalPadding, ABorderOffset, ALeftPosition, ARightPosition: Integer;
begin
  AParagraphProperties := AParagraphFrameBox.GetParagraph.ParagraphProperties;
  AFrameProperties := AParagraphFrameBox.GetFrameProperties;
  try
    AShapeBounds := CalculateAbsoluteParagraphFrameShapeBounds(AFrameProperties);

    AConverter := DocumentModel.ToDocumentLayoutUnitConverter;

    AParagraphFrameBox.ShapeBounds := AShapeBounds;
    AParagraphFrameBox.ContentBounds := AParagraphFrameBox.ShapeBounds;

    ALeftBorderWidthWithOffset := GetActualBorderWidth(AParagraphProperties.UseLeftBorder, AParagraphProperties.LeftBorder, AConverter);
    ARightBorderWidthWithOffset := GetActualBorderWidth(AParagraphProperties.UseRightBorder, AParagraphProperties.RightBorder, AConverter);
    ATopBorderWidthWithOffset := GetActualBorderWidth(AParagraphProperties.UseTopBorder, AParagraphProperties.TopBorder, AConverter);
    ABottomBorderWidthWithOffset := GetActualBorderWidth(AParagraphProperties.UseBottomBorder, AParagraphProperties.BottomBorder, AConverter);

    APadding := 30;

    if AFrameProperties.Info.HorizontalPadding <> 0 then
      AHorizontalPadding := AConverter.ToLayoutUnits(APadding)
    else
      AHorizontalPadding := 0;

    ABorderOffset := 15;

    if ALeftBorderWidthWithOffset <> 0 then
      ALeftPosition := AHorizontalPadding + ALeftBorderWidthWithOffset + AConverter.ToLayoutUnits(ABorderOffset)
    else
      ALeftPosition := 0;
    if ARightBorderWidthWithOffset <> 0 then
      ARightPosition := AHorizontalPadding + ARightBorderWidthWithOffset + AConverter.ToLayoutUnits(ABorderOffset)
    else
      ARightPosition := 0;

    ANewShapeBounds := AParagraphFrameBox.ShapeBounds;
    AParagraphFrameBox.ShapeBounds := TRect.CreateSize(ANewShapeBounds.X - ALeftPosition, ANewShapeBounds.Y,
      ANewShapeBounds.Width + ALeftPosition + ARightPosition, ANewShapeBounds.Height);

    AParagraphFrameBox.ContentBounds := TRect.CreateSize(ANewShapeBounds.X, ANewShapeBounds.Y + ATopBorderWidthWithOffset,
      ANewShapeBounds.Width, ANewShapeBounds.Height - ATopBorderWidthWithOffset - ABottomBorderWidthWithOffset);
    AParagraphFrameBox.ActualBounds := AParagraphFrameBox.ShapeBounds;
    AParagraphFrameBox.SetActualSizeBounds(AParagraphFrameBox.ShapeBounds);
  finally
    AFrameProperties.Free;
  end;
end;

function TdxFloatingObjectSizeController.ValidateRotatedShapeHorizontalPosition(const ARect: TRect; AProperties: TdxFloatingObjectProperties): TRect;
begin
  Result := ARect;
end;

function TdxFloatingObjectSizeController.ValidateRotatedShapeVerticalPosition(const ARect: TRect; AProperties: TdxFloatingObjectProperties): TRect;
begin
  Result := ARect;
end;

function TdxFloatingObjectSizeController.CreateRotatedFloatingObjectLocation(const AProperties: IdxFloatingObjectLocation;
  AAngle: Integer; AAutoHeight: Boolean): IdxFloatingObjectLocation;
var
  AActualSize: TSize;
  AMatrix: TdxTransformMatrix;
  AObjectLocation: TdxExplicitFloatingObjectLocation;
  ABoundingRectangle, ARelativeBoundingRectangle: TRect;
  AFloatingObjectProperties: TdxFloatingObjectProperties;
  AFrameProperties: TdxFrameProperties;
begin
  AMatrix := TdxTransformMatrix.Create;
  try
    AMatrix.Rotate(DocumentModel.UnitConverter.ModelUnitsToDegreeF(-AAngle));
    if AProperties is TdxFloatingObjectProperties then
    begin
      AFloatingObjectProperties := TdxFloatingObjectProperties(AProperties);
      AActualSize := AFloatingObjectProperties.ActualSize;
    end
    else
      AFloatingObjectProperties := nil;
    if AProperties is TdxFrameProperties then
    begin
      AFrameProperties := TdxFrameProperties(AProperties);
      AActualSize := TSize.Create(AFrameProperties.Width, AFrameProperties.Height);
    end;
    ABoundingRectangle := TdxRectangleUtils.BoundingRectangle(TRect.CreateSize(AActualSize), AMatrix);
    ARelativeBoundingRectangle := TdxRectangleUtils.BoundingRectangle(TRect.CreateSize(
      cxSize(AProperties.RelativeWidth.Width, AProperties.RelativeHeight.Height)), AMatrix);
  finally
    AMatrix.Free;
  end;
  AObjectLocation := TdxExplicitFloatingObjectLocation.Create;
  AObjectLocation.ActualWidth := ABoundingRectangle.Width;
  AObjectLocation.ActualHeight := ABoundingRectangle.Height;
  AObjectLocation.OffsetX := AProperties.OffsetX;
  AObjectLocation.OffsetY := AProperties.OffsetY;
  AObjectLocation.HorizontalPositionAlignment := AProperties.HorizontalPositionAlignment;
  AObjectLocation.VerticalPositionAlignment := AProperties.VerticalPositionAlignment;
  AObjectLocation.HorizontalPositionType := AProperties.HorizontalPositionType;
  AObjectLocation.VerticalPositionType := AProperties.VerticalPositionType;
  AObjectLocation.TextWrapType := AProperties.TextWrapType;
  if AProperties.UseRelativeWidth then
  begin
    AObjectLocation.UseRelativeWidth := True;
    AObjectLocation.RelativeWidth := TdxFloatingObjectRelativeWidth.Create(AProperties.RelativeWidth.From,
      ARelativeBoundingRectangle.Width);
  end;
  if AProperties.UseRelativeHeight and not AAutoHeight then
  begin
    AObjectLocation.UseRelativeHeight := True;
    AObjectLocation.RelativeHeight := TdxFloatingObjectRelativeHeight.Create(AProperties.RelativeHeight.From,
      ARelativeBoundingRectangle.Height);
  end;
  if (AFloatingObjectProperties <> nil) and AFloatingObjectProperties.UsePercentOffset then
  begin
    AObjectLocation.PercentOffsetX := AProperties.PercentOffsetX;
    AObjectLocation.PercentOffsetY := AProperties.PercentOffsetY;
  end;
  AObjectLocation.LayoutInTableCell := AProperties.LayoutInTableCell;
  Result := AObjectLocation;
end;

function TdxFloatingObjectSizeController.CreateFloatingObjectLocation(AProperties: TdxFloatingObjectProperties; AAutoHeight: Boolean): IdxFloatingObjectLocation;
var
  ALocation: TdxExplicitFloatingObjectLocation;
begin
  if not AAutoHeight then
    Exit(AProperties);

  ALocation := TdxExplicitFloatingObjectLocation.Create;
  ALocation.ActualWidth := AProperties.ActualWidth;
  ALocation.ActualHeight := AProperties.ActualHeight;
  ALocation.OffsetX := AProperties.OffsetX;
  ALocation.OffsetY := AProperties.OffsetY;
  ALocation.HorizontalPositionAlignment := AProperties.HorizontalPositionAlignment;
  ALocation.VerticalPositionAlignment := AProperties.VerticalPositionAlignment;
  ALocation.HorizontalPositionType := AProperties.HorizontalPositionType;
  ALocation.VerticalPositionType := AProperties.VerticalPositionType;
  ALocation.TextWrapType := AProperties.TextWrapType;
  if AProperties.UseRelativeWidth then
  begin
    ALocation.UseRelativeWidth := True;
    ALocation.RelativeWidth := AProperties.RelativeWidth;
  end;
  if AProperties.UsePercentOffset then
  begin
    ALocation.PercentOffsetX := AProperties.PercentOffsetX;
    ALocation.PercentOffsetY := AProperties.PercentOffsetY;
  end;
  ALocation.LayoutInTableCell := AProperties.LayoutInTableCell;
  Result := ALocation;
end;

function TdxFloatingObjectSizeController.CalculateAbsoluteFloatingObjectShapeBounds(const ALocation: IdxFloatingObjectLocation;
  AShape: TdxShape; AIsTextBox: Boolean): TRect;
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ABounds: TRect;
  APosition: TPoint;
  ASize: TSize;
  AOutlineWidth, AWidth, AHeight: Integer;
begin
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  ABounds := CalculateAbsoluteFloatingObjectBounds(ALocation);
  APosition := ABounds.Location;
  ASize := ABounds.Size;
  if not TdxAlphaColors.IsTransparentOrEmpty(AShape.OutlineColor) then
  begin
    if AIsTextBox then
    begin
      ASize.Height := ASize.Height + AShape.OutlineWidth;
    end
    else
    begin
      AOutlineWidth := AUnitConverter.ToLayoutUnits(AShape.OutlineWidth);
      APosition.X := APosition.X - AOutlineWidth;
      APosition.Y := APosition.Y - AOutlineWidth;
      ASize.Width := ASize.Width + 2 * AShape.OutlineWidth;
      ASize.Height := ASize.Height + 2 * AShape.OutlineWidth;
    end;
  end;
  AWidth := AUnitConverter.ToLayoutUnits(ASize.Width);
  AHeight := AUnitConverter.ToLayoutUnits(ASize.Height);
  Result.InitSize(APosition, AWidth, AHeight);
end;

function TdxFloatingObjectSizeController.CalculateAbsoluteParagraphFrameShapeBounds(const ALocation: IdxParagraphFrameLocation): TRect;
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ABounds: TRect;
  AWidth, AHeight: Integer;
begin
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  ABounds := CalculateAbsoluteParagraphFrameBounds(ALocation);
  AWidth := AUnitConverter.ToLayoutUnits(ABounds.Size.Width);
  AHeight := AUnitConverter.ToLayoutUnits(ABounds.Size.Height);
  Result.InitSize(ABounds.Location, AWidth, AHeight);
end;

function TdxFloatingObjectSizeController.CenterRectangleHorizontally(const ARectangle: TRect; const AWhere: TRect): TRect;
begin
  Result := ARectangle;
  Result.X := AWhere.Left + (AWhere.Width - ARectangle.Width) div 2;
end;

function TdxFloatingObjectSizeController.CenterRectangleVertically(const ARectangle: TRect; const AWhere: TRect): TRect;
begin
  Result := ARectangle;
  Result.Y := AWhere.Top + (AWhere.Height - ARectangle.Height) div 2;
end;

function TdxFloatingObjectSizeController.CalculateAbsoluteFloatingObjectContentBounds(const AFloatingObjectProperties: IdxFloatingObjectLocation; AShape: TdxShape; const AShapeBounds: TRect): TRect;
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AWidth: Integer;
begin
  if TdxAlphaColors.IsTransparentOrEmpty(AShape.OutlineColor) then
    Exit(AShapeBounds);
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  AWidth := AUnitConverter.ToLayoutUnits(AShape.OutlineWidth);
  Result := AShapeBounds;
  Result.X := Result.Left + AWidth;
  Result.Width := Result.Width - 2 * AWidth;
  Result.Y := Result.Top + AWidth;
  Result.Height := Result.Height - 2 * AWidth;
end;

function TdxFloatingObjectSizeController.CalculateActualAbsoluteFloatingObjectBounds(AFloatingObjectProperties: TdxFloatingObjectProperties; const ABounds: TRect): TRect;
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  Result := ABounds;
  Result.X := Result.Left - AUnitConverter.ToLayoutUnits(AFloatingObjectProperties.LeftDistance);
  Result.Y := Result.Top - AUnitConverter.ToLayoutUnits(AFloatingObjectProperties.TopDistance);
  Result.Width := Result.Width + AUnitConverter.ToLayoutUnits(AFloatingObjectProperties.LeftDistance + AFloatingObjectProperties.RightDistance);
  Result.Height := Result.Height + AUnitConverter.ToLayoutUnits(AFloatingObjectProperties.TopDistance + AFloatingObjectProperties.BottomDistance);
end;

function TdxFloatingObjectSizeController.CalculateAbsoluteFloatingObjectBounds(const ALocation: IdxFloatingObjectLocation): TRect;
begin
  Result.InitSize(TSize.Create(ALocation.ActualWidth, ALocation.ActualHeight));
end;

function TdxFloatingObjectSizeController.CalculateAbsoluteParagraphFrameBounds(const ALocation: IdxParagraphFrameLocation): TRect;
begin
  Result.InitSize(0, 0, ALocation.Width, ALocation.Height);
end;

class function TdxFloatingObjectSizeController.GetActualBorderWidth(AUseBorder: Boolean; ABorder: TdxBorderInfo;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter): Integer;
begin
  if AUseBorder then
    Result := AConverter.ToLayoutUnits(ABorder.Width + ABorder.Offset)
  else
    Result := 0;
end;

{ TdxFloatingObjectSizeAndPositionController }

constructor TdxFloatingObjectSizeAndPositionController.Create(ARowsController: TdxRowsController);
begin
  inherited Create(ARowsController.PieceTable);
  FRowsController := ARowsController;
end;

function TdxFloatingObjectSizeAndPositionController.GetColumnController: TdxCustomColumnController;
begin
  Result := RowsController.ColumnController;
end;

function TdxFloatingObjectSizeAndPositionController.GetCurrentColumn: TdxColumn;
begin
  Result := RowsController.CurrentColumn;
end;

function TdxFloatingObjectSizeAndPositionController.GetCurrentHorizontalPosition: Integer;
begin
  Result := RowsController.CurrentHorizontalPosition;
end;

function TdxFloatingObjectSizeAndPositionController.GetCurrentRow: TdxRow;
begin
  Result := RowsController.CurrentRow;
end;

function TdxFloatingObjectSizeAndPositionController.GetHorizontalPositionController: TdxCurrentHorizontalPositionController;
begin
  Result := RowsController.HorizontalPositionController;
end;

function TdxFloatingObjectSizeAndPositionController.CalculateAbsoluteFloatingObjectBounds(const ALocation: IdxFloatingObjectLocation): TRect;
var
  ASize: TSize;
  ALeft, ATop: Integer;
  APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
begin
  APlacementInfo := CalculatePlacementInfo(ALocation);
  APlacementInfo.OriginY := APlacementInfo.OriginY - CurrentRow.SpacingBefore;
  ASize := cxSize(CalculateAbsoluteFloatingObjectWidth(ALocation, APlacementInfo),
    CalculateAbsoluteFloatingObjectHeight(ALocation, APlacementInfo));
  ALeft := CalculateAbsoluteFloatingObjectX(ALocation, APlacementInfo, ASize.cx);
  ATop := CalculateAbsoluteFloatingObjectY(ALocation, APlacementInfo, ASize.cy);
  Result.InitSize(ALeft, ATop, ASize);
end;

function TdxFloatingObjectSizeAndPositionController.CalculateAbsoluteParagraphFrameBounds(const ALocation: IdxParagraphFrameLocation): TRect;
var
  APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
  ASize: TSize;
  APoint: TPoint;
begin
  APlacementInfo := CalculatePlacementInfo(nil);
  ASize := TSize.Create(ALocation.Width, ALocation.Height);
  APoint.X := CalculateAbsoluteParagraphFrameX(ALocation, APlacementInfo, ASize.cx);
  APoint.Y := CalculateAbsoluteParagraphFrameY(ALocation, APlacementInfo, ASize.cy);
  Result.InitSize(APoint, ASize.cx, ASize.cy);
end;

function TdxFloatingObjectSizeAndPositionController.CalculateAbsoluteFloatingObjectWidth(
  const ALocation: IdxFloatingObjectLocation; const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
var
  ACalculator: TdxFloatingObjectHorizontalPositionCalculator;
begin
  ACalculator := TdxFloatingObjectHorizontalPositionCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
  try
    Result := ACalculator.CalculateAbsoluteFloatingObjectWidth(ALocation, APlacementInfo);
  finally
    ACalculator.Free;
  end;
end;

function TdxFloatingObjectSizeAndPositionController.CalculateAbsoluteFloatingObjectHeight(const ALocation: IdxFloatingObjectLocation;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo): Integer;
var
  ACalculator: TdxFloatingObjectVerticalPositionCalculator;
begin
  ACalculator := TdxFloatingObjectVerticalPositionCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
  try
    Result := ACalculator.CalculateAbsoluteFloatingObjectHeight(ALocation, APlacementInfo);
  finally
    ACalculator.Free;
  end;
end;

function TdxFloatingObjectSizeAndPositionController.CalculateAbsoluteParagraphFrameX(const ALocation: IdxParagraphFrameLocation;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualWidth: Integer): Integer;
var
  ACalculator: TdxParagraphFrameHorizontalPositionCalculator;
begin
  ACalculator := TdxParagraphFrameHorizontalPositionCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
  try
    Result := ACalculator.CalculateAbsoluteFloatingObjectX(ALocation, APlacementInfo, AActualWidth);
  finally
    ACalculator.Free;
  end;
end;

function TdxFloatingObjectSizeAndPositionController.CalculateAbsoluteParagraphFrameY(const ALocation: IdxParagraphFrameLocation;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualHeight: Integer): Integer;
var
  ACalculator: TdxParagraphFrameVerticalPositionCalculator;
begin
  ACalculator := TdxParagraphFrameVerticalPositionCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
  try
    Result := ACalculator.CalculateAbsoluteFloatingObjectY(ALocation, APlacementInfo, AActualHeight);
  finally
    ACalculator.Free;
  end;
end;

function TdxFloatingObjectSizeAndPositionController.ValidateRotatedShapeHorizontalPosition(const AShapeBounds: TRect;
  AProperties: TdxFloatingObjectProperties): TRect;
var
  AOverflow: Integer;
  APageBounds: TRect;
  APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
begin
  if AProperties.TextWrapType = TdxFloatingObjectTextWrapType.None then
    Exit(AShapeBounds);

  APlacementInfo := CalculatePlacementInfo(AProperties);
  APageBounds := APlacementInfo.PageBounds;
  AOverflow := AShapeBounds.Left + AShapeBounds.Width - APageBounds.Right;
  if AOverflow > 0 then
    AShapeBounds.SetLocation(AShapeBounds.Left - AOverflow, AShapeBounds.Top);

  AShapeBounds.SetLocation(Math.Max(APageBounds.Left, AShapeBounds.Left), AShapeBounds.Top);
  Result := AShapeBounds;
end;

function TdxFloatingObjectSizeAndPositionController.ValidateRotatedShapeVerticalPosition(const AShapeBounds: TRect;
  AProperties: TdxFloatingObjectProperties): TRect;
var
  APageBounds: TRect;
  ABottom: Integer;
  APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
begin
  if AProperties.TextWrapType = TdxFloatingObjectTextWrapType.None then
    Exit(AShapeBounds);

  ABottom := AShapeBounds.Bottom;
  APlacementInfo := CalculatePlacementInfo(AProperties);
  APageBounds := APlacementInfo.PageBounds;
  if ABottom >= APageBounds.Bottom then
    AShapeBounds.SetLocation(AShapeBounds.Left, AShapeBounds.Top - ABottom + APageBounds.Bottom);
  if AShapeBounds.Top < APageBounds.Top then
    AShapeBounds.SetLocation(AShapeBounds.Left, APageBounds.Top);
  Result := AShapeBounds;
end;

function TdxFloatingObjectSizeAndPositionController.CalculatePlacementInfo(const ALocation: IdxFloatingObjectLocation): TdxFloatingObjectTargetPlacementInfo;
var
 APage: TdxPage;
begin
  Result.Init;
  APage := ColumnController.PageAreaController.PageController.Pages.Last;
  if (ALocation <> nil) and ALocation.LayoutInTableCell then
  begin
    Result.PageBounds := ColumnController.GetCurrentPageBounds(APage, CurrentColumn);
    Result.PageClientBounds := ColumnController.GetCurrentPageClientBounds(APage, CurrentColumn);
    Result.ColumnBounds := HorizontalPositionController.CalculateFloatingObjectColumnBounds(CurrentColumn);
  end
  else
  begin
    Result.PageBounds := APage.Bounds;
    Result.PageClientBounds := APage.ClientBounds;
    Result.ColumnBounds := HorizontalPositionController.CalculateFloatingObjectColumnBounds(CurrentColumn.TopLevelColumn);
  end;

  Result.OriginalColumnBounds := CurrentColumn.Bounds;
  Result.OriginX := CurrentHorizontalPosition;
  Result.OriginY := CurrentRow.Bounds.Y;
end;

function TdxFloatingObjectSizeAndPositionController.CalculateAbsoluteFloatingObjectX(const ALocation: IdxFloatingObjectLocation;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualWidth: Integer): Integer;
var
  ACalculator: TdxFloatingObjectHorizontalPositionCalculator;
begin
  ACalculator := TdxFloatingObjectHorizontalPositionCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
  try
    Result := ACalculator.CalculateAbsoluteFloatingObjectX(ALocation, APlacementInfo, AActualWidth);
  finally
    ACalculator.Free;
  end;
end;

function TdxFloatingObjectSizeAndPositionController.CalculateAbsoluteFloatingObjectY(const ALocation: IdxFloatingObjectLocation;
  const APlacementInfo: TdxFloatingObjectTargetPlacementInfo; AActualHeight: Integer): Integer;
var
  ACalculator: TdxFloatingObjectVerticalPositionCalculator;
begin
  ACalculator := TdxFloatingObjectVerticalPositionCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
  try
    Result := ACalculator.CalculateAbsoluteFloatingObjectY(ALocation, APlacementInfo, AActualHeight);
  finally
    ACalculator.Free;
  end;
end;

{ TdxFloatingObjectTargetPlacementInfo }

procedure TdxFloatingObjectTargetPlacementInfo.Init;
begin
  FColumnBounds.Empty;
  FOriginalColumnBounds.Empty;
  FOriginX := 0;
  FOriginY := 0;
  FPageBounds.Empty;
  FPageClientBounds.Empty;
end;

procedure TdxFloatingObjectTargetPlacementInfo.SetColumnBounds(const Value: TRect);
begin
  FColumnBounds := Value;
  FOriginalColumnBounds := Value;
end;

{ TdxTableCellIterator }

constructor TdxTableCellIterator.Create(ARow: TdxTableRow);
begin
  inherited Create;
  FRow := ARow;
  FCurrentStartColumnIndex := -1;
  FCurrentEndColumnIndex := -1;
  FCurrentCellIndex := -1;
end;

function TdxTableCellIterator.GetCellCount: Integer;
begin
  Result := Row.Cells.Count;
end;

function TdxTableCellIterator.GetCurrentCell: TdxTableCell;
begin
  if (FCurrentCellIndex >= 0) and (FCurrentCellIndex < CellCount) then
    Result := FRow.Cells[FCurrentCellIndex]
  else
    Result := nil;
end;

function TdxTableCellIterator.GetCurrentEndColumnIndex: Integer;
begin
  Result := FCurrentEndColumnIndex;
end;

function TdxTableCellIterator.GetCurrentStartColumnIndex: Integer;
begin
  Result := FCurrentStartColumnIndex;
end;

function TdxTableCellIterator.GetEndOfRow: Boolean;
begin
  Result := FCurrentCellIndex >= CellCount;
end;

function TdxTableCellIterator.MoveNextCell: Boolean;
begin
  if EndOfRow then
    Exit(False);
  Inc(FCurrentCellIndex);
  if FCurrentCellIndex = 0 then
    FCurrentStartColumnIndex := Row.LayoutProperties.GridBefore
  else
    FCurrentStartColumnIndex := CurrentEndColumnIndex + 1;
  if FCurrentCellIndex < Row.Cells.Count then
    FCurrentEndColumnIndex := CurrentStartColumnIndex + CurrentCell.LayoutProperties.ColumnSpan - 1
  else
    FCurrentEndColumnIndex := FCurrentStartColumnIndex;
  Result := True;
end;

procedure TdxTableCellIterator.SetStartColumnIndex(ANewStartColumnIndex: Integer);
begin
  if ANewStartColumnIndex < CurrentStartColumnIndex then
    TdxRichEditExceptions.ThrowInternalException;
  while ANewStartColumnIndex > CurrentEndColumnIndex do
    if not MoveNextCell then
      Exit;
  FCurrentStartColumnIndex := ANewStartColumnIndex;
end;

{ TdxTableCellEmptyIterator }

function TdxTableCellEmptyIterator.GetCurrentCell: TdxTableCell;
begin
  Result := nil;
end;

function TdxTableCellEmptyIterator.GetCurrentEndColumnIndex: Integer;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Exit(0);
end;

function TdxTableCellEmptyIterator.GetCurrentStartColumnIndex: Integer;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Exit(0);
end;

function TdxTableCellEmptyIterator.GetEndOfRow: Boolean;
begin
  Result := True;
end;

function TdxTableCellEmptyIterator.MoveNextCell: Boolean;
begin
  Result := False;
end;

procedure TdxTableCellEmptyIterator.SetStartColumnIndex(ANewStartColumnIndex: Integer);
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

{ TdxTablesControllerTableState }

constructor TdxTablesControllerTableState.Create(ATablesController: TdxTablesController; AStartCell: TdxTableCell;
  AFirstContentInParentCell, ASimpleView: Boolean);
var
  ATable: TdxTable;
  ATableControllerParentState: TdxTablesControllerTableState;
begin
  inherited Create(ATablesController);
  Assert(AStartCell <> nil);
  FCurrentCell := AStartCell;
  FBorderCalculator := TdxTableBorderCalculator.Create;
  FUnitConverter := AStartCell.DocumentModel.ToDocumentLayoutUnitConverter;
  ATableControllerParentState := Safe<TdxTablesControllerTableState>.Cast(ATablesController.State);
  if ATableControllerParentState is TdxTablesControllerTableState then
    FTableViewInfoManager := ATablesController.CreateTableViewInfoManager(ATableControllerParentState.TableViewInfoManager,
      TablesController.PageController, TablesController.RowsController)
  else
    FTableViewInfoManager := ATablesController.CreateTableViewInfoManager(nil,
      TablesController.PageController, TablesController.RowsController);
  FCellsBounds := TDictionary<TdxTableCell, TdxLayoutGridRectangle>.Create;
  ATable := CurrentCell.Table;
  FVerticalBordersCalculator := TdxTableCellVerticalBorderCalculator.Create(ATable);
  FHorizontalBordersCalculator := TdxTableCellHorizontalBorderCalculator.Create(ATable);
  PrepareGridCellBounds;
  StartNewTable(ATable, AFirstContentInParentCell, ASimpleView);
end;

destructor TdxTablesControllerTableState.Destroy;
begin
  FHorizontalBordersCalculator.Free;
  FVerticalBordersCalculator.Free;
  FCellsBounds.Free;
  FTableViewInfoManager.Free;
  FBorderCalculator.Free;
  inherited Destroy;
end;

procedure TdxTablesControllerTableState.AddTopAnchor;
var
  ARowSeparatorIndex: Integer;
  ABottomTextIndent: TdxLayoutUnit;
  ATopAnchor: TdxTableCellVerticalAnchor;
  ACellBordersInfo: TdxHorizontalCellBordersInfoList;
begin
  ARowSeparatorIndex := CurrentCell.RowIndex;
  ABottomTextIndent := CalculateBottomTextIndent(ARowSeparatorIndex, ACellBordersInfo);
  ATopAnchor := TdxTableCellVerticalAnchor.Create(TableViewInfoManager.CurrentCellBottom, ABottomTextIndent,
    ACellBordersInfo);
  ATopAnchor.TopTextIndent := UnitConverter.ToLayoutUnits(GetActualCellTopMargin(CurrentCell));

  TableViewInfoManager.SetRowSeparator(ARowSeparatorIndex, ATopAnchor);
  TableViewInfoManager.SetRowSeparatorForCurrentTableViewInfo(ARowSeparatorIndex, ATopAnchor);
end;

procedure TdxTablesControllerTableState.AfterMoveRowToNextColumn;
begin
  TableViewInfoManager.AfterMoveRowToNextColumn;
  TableViewInfoManager.CurrentCellBottom := 0;
  FCurrentCellRowCount := 0;
  FTableViewInfoManager.ValidateTopLevelColumn;
end;

procedure TdxTablesControllerTableState.BeforeMoveRowToNextColumn;
begin
end;

function TdxTablesControllerTableState.CalcFirstInvalidRowIndex: Integer;
var
  ANewResult: Integer;
begin
  Result := CurrentTable.Rows.IndexOf(CurrentCell.Row);
  while Result > 0 do
  begin
    ANewResult := CalcFirstInvalidRowIndexCore(Result);
    if ANewResult = Result then
      Break
    else
      Result := ANewResult;
  end;
end;

function TdxTablesControllerTableState.CalcFirstInvalidRowIndexCore(AStartRowIndex: Integer): Integer;
var
  I: Integer;
  ACell, ATopCell: TdxTableCell;
  ACells: TdxTableCellCollection;
begin
  Result := AStartRowIndex;
  ACells := CurrentTable.Rows[AStartRowIndex].Cells;
  for I := 0 to ACells.Count - 1 do
  begin
    ACell := ACells[I];
    if ACell = CurrentCell then
      Continue;
    if ACell.VerticalMerging <> TdxMergingState.Continue then
      Continue;
    ATopCell := CurrentTable.GetFirstCellInVerticalMergingGroup(ACell);
    Result := Math.Min(Result, CurrentTable.Rows.IndexOf(ATopCell.Row));
  end;
end;

function TdxTablesControllerTableState.CalculateBottomTextIndent(ARowSeparatorIndex: Integer;
  out ACellBordersInfo: TdxHorizontalCellBordersInfoList): TdxLayoutUnit;
var
  AIndentCalculator: TdxBottomTextIndentCalculatorBase;
begin
  AIndentCalculator := TdxBottomTextIndentCalculatorBase.CreateCalculator(Self, CurrentTable, ARowSeparatorIndex);
  try
    Result := AIndentCalculator.CalculateBottomTextIndent(ACellBordersInfo);
  finally
    AIndentCalculator.Free;
  end;
end;

function TdxTablesControllerTableState.CalculateTableActualLeftRelativeOffset(ATable: TdxTable): TdxModelUnit;
var
  AFirstCell: TdxTableCell;
  ALeftMargin: TdxMarginUnitBase;
  ALeftBorderWidth, ALeftMarginValue: TdxModelUnit;
begin
  AFirstCell := ATable.Rows.First.Cells.First;
  ALeftBorderWidth := VerticalBordersCalculator.GetLeftBorderWidth(FBorderCalculator, AFirstCell, False);
  if (ATable.NestedLevel > 0) or  RowsController.MatchHorizontalTableIndentsToTextEdge then
    Result := ALeftBorderWidth div 2
  else
  begin
    ALeftMargin := AFirstCell.GetActualLeftMargin();
    if ALeftMargin.&Type = TdxWidthUnitType.ModelUnits then
      ALeftMarginValue := ALeftMargin.Value
    else
      ALeftMarginValue := 0;
    Result := -Math.Max(ALeftMarginValue, ALeftBorderWidth div 2);
  end;
end;

function TdxTablesControllerTableState.CanFitRowToColumn(ALastTextRowBottom: Integer;
  AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult;
var
  AInfinityHeight: Boolean;
  ASpecialBottomAnchorIndex, ABottomBounds: Integer;
  ABreakType: TdxTableBreakType;
  ACells: TdxTableCellList;
  ACell: TdxTableCell;
  ACurrentTableViewInfoManager: TdxTableViewInfoManager;
  ATable: TdxTable;
begin
  AInfinityHeight := TablesController.IsInfinityHeight(TableViewInfoManager.GetCurrentTopLevelTableViewInfoIndex, ASpecialBottomAnchorIndex);
  if AInfinityHeight then
  begin
    if CurrentTable.NestedLevel > 0 then
      Exit(TdxCanFitCurrentRowToColumnResult.RowFitted);
    if TableViewInfoManager.GetCurrentCellTopAnchorIndex >= ASpecialBottomAnchorIndex then
    begin
      TableViewInfoManager.SetCurrentCellHasContent;
      Exit(TdxCanFitCurrentRowToColumnResult.PlainRowNotFitted);
    end
    else
      Exit(TdxCanFitCurrentRowToColumnResult.RowFitted);
  end;
  if FCurrentCellRowCount = 0 then
  begin
    ABreakType := TdxTableBreakType.NoBreak;
    ABottomBounds := AColumn.Bounds.Bottom;
    if not IsFirstTableRowViewInfoInColumn then
    begin
      ACell := CurrentCell;
      ACurrentTableViewInfoManager := FTableViewInfoManager;
      while ACell <> nil do
      begin
        ATable := ACell.Table;
        ABreakType := TablesController.GetTableBreak(ATable, FTableViewInfoManager.GetCurrentTableViewInfoIndex,
          ABottomBounds);

        if ACell.VerticalMerging = TdxMergingState.Restart then
        begin
          ACells := ACell.GetVerticalSpanCells;
          try
            if ACells.Count > 1 then
              if (ABreakType = TdxTableBreakType.NoBreak) or (FTableViewInfoManager.GetCurrentCellTop <> ABottomBounds) then
                Exit(TdxCanFitCurrentRowToColumnResult.RowFitted);
          finally
            ACells.Free;
          end;
        end;
        if ABreakType <> TdxTableBreakType.NoBreak then
          Break;
        if ACurrentTableViewInfoManager.GetCurrentCellTopAnchorIndex > 0 then
          Break;
        if not ACurrentTableViewInfoManager.IsCurrentCellViewInfoFirst then
          Break;
        if not ACurrentTableViewInfoManager.IsFirstContentInParentCell then
          Break;
        ACurrentTableViewInfoManager := FTableViewInfoManager.GetParentTableViewInfoManager;
        ACell := ACell.Table.ParentCell;
      end;
    end
    else
      ABreakType := TdxTableBreakType.NoBreak;
    if ABreakType = TdxTableBreakType.NextPage then
    begin
      if ALastTextRowBottom > ABottomBounds then
      begin
        FTableViewInfoManager.SetCurrentCellHasContent;
        Exit(TdxCanFitCurrentRowToColumnResult.PlainRowNotFitted);
      end
      else
        Exit(TdxCanFitCurrentRowToColumnResult.RowFitted);
    end;
    if ALastTextRowBottom > AColumn.Bounds.Bottom then
      Result := TdxCanFitCurrentRowToColumnResult.FirstCellRowNotFitted
    else
      Result := TdxCanFitCurrentRowToColumnResult.RowFitted;
  end
  else
  begin
    ABottomBounds := AColumn.Bounds.Bottom;
    ABreakType := TablesController.GetTableBreak(CurrentTable, FTableViewInfoManager.GetCurrentTableViewInfoIndex, ABottomBounds);
    if ABreakType = TdxTableBreakType.NoBreak then
      ABottomBounds := AColumn.Bounds.Bottom;
    if ALastTextRowBottom > ABottomBounds then
      Result := TdxCanFitCurrentRowToColumnResult.PlainRowNotFitted
    else
      Result := TdxCanFitCurrentRowToColumnResult.RowFitted;
  end;
end;

function TdxTablesControllerTableState.CanFitFloatingObjectInTableRow(ABottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult;
begin
  Result := CanFitRowToColumn(ABottom, AColumn);
end;

procedure TdxTablesControllerTableState.ChangeCurrentCellInTheSameTable(ANewCell: TdxTableCell);
var
  ARowIndex: Integer;
  ACellBounds: TRect;
  ATopAnchor: TdxTableCellVerticalAnchor;
  AColumnController: TdxTableCellColumnController;
  ABounds, ATextBounds, ANewRowBounds: TRect;
begin
  if FRestartFrom = TdxRestartFrom.NoRestart then
  begin
    TableViewInfoManager.ValidateTopLevelColumn;
    FinishCurrentCell;
  end;
  FCurrentCell := ANewCell;
  EnsureCurrentTableRow(CurrentCell.Row);
  ARowIndex := ANewCell.Table.Rows.IndexOf(ANewCell.Row);
  ATopAnchor := TableViewInfoManager.GetRowStartAnchor(ARowIndex);
  if ATopAnchor = nil then
  begin
    AddTopAnchor;
    ATopAnchor := TableViewInfoManager.GetRowStartAnchor(ARowIndex);
  end;
  ABounds := GetCellBounds(CurrentCell);

  TableViewInfoManager.BeforeStartNextCell(CurrentCell, FCellsBounds[CurrentCell]);
  AColumnController := TdxTableCellColumnController(RowsController.ColumnController);

  ACellBounds := ABounds;
  ACellBounds.Offset(AColumnController.ParentColumn.Bounds.Left, 0);
  ATextBounds := GetTextBounds(CurrentCell, ABounds);

  TableViewInfoManager.StartNextCell(CurrentCell, ACellBounds, FCellsBounds[CurrentCell], ATextBounds);

  AColumnController.StartNewCell(TableViewInfoManager.GetRowStartColumn(ARowIndex), ATextBounds.Left, ATextBounds.Top, ATextBounds.Width, CurrentCell);
  RowsController.CurrentColumn := AColumnController.GetStartColumn;
  RowsController.OnCellStart;
  ANewRowBounds := RowsController.CurrentRow.Bounds;
  ANewRowBounds.MoveToTop(ATextBounds.Top);
  RowsController.CurrentRow.Bounds := ANewRowBounds;
  TableViewInfoManager.CurrentCellBottom := ATopAnchor.VerticalPosition + ATopAnchor.BottomTextIndent;
  FTableViewInfoManager.ValidateTopLevelColumn;
end;

procedure TdxTablesControllerTableState.EndParagraph(ALastRow: TdxRow);
begin
  UpdateCurrentCellHeight(ALastRow);
end;

procedure TdxTablesControllerTableState.EnsureCurrentCell(ACell: TdxTableCell);
var
  ASameLevelCell: TdxTableCell;
begin
  if FRestartFrom = TdxRestartFrom.CellStart then
  begin
    ASameLevelCell := ACell;
    while ASameLevelCell.Table.NestedLevel > CurrentTable.NestedLevel do
      ASameLevelCell := ASameLevelCell.Table.ParentCell;
    ChangeCurrentCellInTheSameTable(ASameLevelCell);
    FRestartFrom := TdxRestartFrom.NoRestart;
    if ACell.Table.NestedLevel > CurrentTable.NestedLevel then
      TablesController.StartNewTable(ACell);
    Exit;
  end;
  if ACell = CurrentCell then
    Exit;
  if (ACell = nil) or (CurrentCell.Table.ParentCell = ACell) then
  begin
    TablesController.LeaveCurrentTable(ACell);
    Exit;
  end;
  if ACell.Table = CurrentTable then
  begin
    ChangeCurrentCellInTheSameTable(ACell);
    Exit;
  end;
  if ACell.Table.NestedLevel = CurrentTable.NestedLevel then
  begin
    TablesController.LeaveCurrentTable(ACell);
    Exit;
  end;
  if ACell.Table.NestedLevel > CurrentTable.NestedLevel then
  begin
    ASameLevelCell := ACell;
    while ASameLevelCell.Table.NestedLevel > CurrentTable.NestedLevel do
      ASameLevelCell := ASameLevelCell.Table.ParentCell;
    if ASameLevelCell <> CurrentCell then
    begin
      if ASameLevelCell.Table = CurrentCell.Table then
        ChangeCurrentCellInTheSameTable(ASameLevelCell)
      else
      begin
        TablesController.LeaveCurrentTable(ACell);
        Exit;
      end;
    end;
  end;
  if ACell.Table.NestedLevel < CurrentTable.NestedLevel then
  begin
    TablesController.LeaveCurrentTable(ACell);
    Exit;
  end;
  TablesController.StartNewTable(ACell);
end;

procedure TdxTablesControllerTableState.EnsureCurrentTableRow(ARow: TdxTableRow);
begin
  if CurrentRow <> ARow then
    FCurrentRow := ARow;
end;

procedure TdxTablesControllerTableState.FinishCurrentCell;
var
  ABottomMargin, ARowTop, ACurrentRowHeight, ARowHeight, ABottomTextIndent: TdxLayoutUnit;
  ACurrentCellBottom, ANewBottom: Integer;
  AFixedRowHeight: Boolean;
  ARowSeparatorIndex: Integer;
  AColumnController: TdxTableCellColumnController;
  ACellBordersInfo: TdxHorizontalCellBordersInfoList;
  ABottomAnchor, AAnchor: TdxTableCellVerticalAnchor;
begin
  ABottomMargin := UnitConverter.ToLayoutUnits(GetActualCellBottomMargin(CurrentCell));
  ACurrentCellBottom := TableViewInfoManager.CurrentCellBottom;
  Inc(ACurrentCellBottom, ABottomMargin);
  AFixedRowHeight := False;
  if (FCellsBounds[CurrentCell].RowSpan = 1) and (CurrentCell.Row.Height.&Type <> TdxHeightUnitType.Auto) then
  begin
    ARowTop := TableViewInfoManager.GetCurrentCellTop;
    ACurrentRowHeight := ACurrentCellBottom - TableViewInfoManager.GetCurrentCellTop;
    ARowHeight := CurrentCell.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(FCurrentCell.Row.Height.Value);
    if (FCurrentCell.Row.Height.&Type = TdxHeightUnitType.Minimum) and (ACurrentRowHeight < ARowHeight) then
      ACurrentCellBottom := ACurrentCellBottom + (ARowHeight - ACurrentRowHeight)
    else
      if FCurrentCell.Row.Height.&Type = TdxHeightUnitType.Exact then
      begin
        ACurrentCellBottom := ARowTop + ARowHeight;
        AFixedRowHeight := True;
      end;
  end;

  ABottomAnchor := TableViewInfoManager.GetBottomCellAnchor(CurrentCell);
  ARowSeparatorIndex := FTableViewInfoManager.GetBottomCellRowSeparatorIndex(CurrentCell, FCellsBounds[CurrentCell].RowSpan);
  AColumnController := TdxTableCellColumnController(RowsController.ColumnController);
  if ABottomAnchor = nil then
  begin
    ABottomTextIndent := CalculateBottomTextIndent(ARowSeparatorIndex, ACellBordersInfo);
    ABottomAnchor := TdxTableCellVerticalAnchor.Create(ACurrentCellBottom, ABottomTextIndent, ACellBordersInfo);
    ABottomAnchor.TopTextIndent := ABottomMargin;
    TableViewInfoManager.SetRowSeparator(ARowSeparatorIndex, ABottomAnchor);
  end
  else
  begin
    if ABottomMargin > ABottomAnchor.TopTextIndent then
    begin
      ANewBottom := ABottomAnchor.VerticalPosition - ABottomAnchor.TopTextIndent + ABottomMargin;
      if not AFixedRowHeight then
        ACurrentCellBottom := Math.Max(ACurrentCellBottom, ANewBottom);
    end;
    ACellBordersInfo := ABottomAnchor.CellBorders.Clone;
    AAnchor := TdxTableCellVerticalAnchor.Create(ACurrentCellBottom, ABottomAnchor.BottomTextIndent, ACellBordersInfo);
    try
      ABottomAnchor := AColumnController.GetMaxAnchor(ABottomAnchor, AAnchor);
      ABottomAnchor.TopTextIndent := Math.Max(ABottomMargin, ABottomAnchor.TopTextIndent);
    finally
      AAnchor.Free;
    end;
  end;
  TableViewInfoManager.SetRowSeparatorForCurrentTableViewInfo(ARowSeparatorIndex, ABottomAnchor);
  FCurrentCellRowCount := 0;
  TableViewInfoManager.CurrentCellBottom := ACurrentCellBottom;
end;

procedure TdxTablesControllerTableState.FinishCurrentTable;
var
  I: Integer;
  ATableViewInfo: TdxTableViewInfo;
  ATableViewInfos: TdxList<TdxTableViewInfo>;
  APendingCells: TdxList<TdxTableCellViewInfo>;
begin
  ATableViewInfos := TableViewInfoManager.GetTableViewInfos;
  APendingCells := TdxList<TdxTableCellViewInfo>.Create;
  try
    for I := 0 to ATableViewInfos.Count - 1 do
    begin
      ATableViewInfo := ATableViewInfos[I];
      FTableViewInfoManager.FixColumnOverflow;
      ATableViewInfo.Complete(0, 0);
      ProcessPendingCells(ATableViewInfo, APendingCells);
    end;
  finally
    APendingCells.Free;
  end;
  Assert(ATableViewInfos.Count > 0);
  if ATableViewInfos[0].Cells.Count = 0 then
  begin
    Assert(ATableViewInfos.Count > 1);
    ATableViewInfos[1].PrevTableViewInfo := nil;
    ATableViewInfos[0].TopLevelColumn.RemoveTableViewInfoWithContent(ATableViewInfos[0]);
  end;
  if CurrentTable.NestedLevel = 0 then
    TablesController.RemoveAllTableBreaks;
end;

function TdxTablesControllerTableState.GetActualCellBottomMargin(ACell: TdxTableCell): TdxModelUnit;
var
  AMargin: TdxWidthUnit;
begin
  AMargin := ACell.GetActualBottomMargin;
  if AMargin.&Type = TdxWidthUnitType.ModelUnits then
    Result := AMargin.Value
  else
    Result := 0;
end;

function TdxTablesControllerTableState.GetActualCellLeftMargin(ACell: TdxTableCell): TdxModelUnit;
var
  AMargin: TdxWidthUnit;
begin
  AMargin := ACell.GetActualLeftMargin();
  if AMargin.&Type = TdxWidthUnitType.ModelUnits then
    Result := AMargin.Value
  else
    Result := 0;
end;

function TdxTablesControllerTableState.GetActualCellRightMargin(ACell: TdxTableCell): TdxModelUnit;
var
  AMargin: TdxWidthUnit;
begin
  AMargin := ACell.GetActualRightMargin;
  if AMargin.&Type = TdxWidthUnitType.ModelUnits then
    Result := AMargin.Value
  else
    Result := 0;
end;

class function TdxTablesControllerTableState.GetActualCellSpacing(ARow: TdxTableRow): TdxModelUnit;
var
  ASpacing: TdxWidthUnit;
begin
  ASpacing := ARow.CellSpacing;
  if ASpacing.&Type = TdxWidthUnitType.ModelUnits then
    Result := ASpacing.Value * 2
  else
    Result := 0;
end;

function TdxTablesControllerTableState.GetActualCellTopMargin(ACell: TdxTableCell): TdxModelUnit;
var
  AMargin: TdxWidthUnit;
begin
  if ACell = nil then
    Exit(0);
  AMargin := ACell.GetActualTopMargin;
  if AMargin.&Type = TdxWidthUnitType.ModelUnits then
    Result := AMargin.Value
  else
    Result := 0;
end;

function TdxTablesControllerTableState.GetCellBounds(ANewCell: TdxTableCell): TRect;
var
  ACellSpacing: TdxWidthUnit;
  ATopMargin: TdxMarginUnitBase;
  AGridPosition: TdxLayoutGridRectangle;
  ATopAnchor: TdxTableCellVerticalAnchor;
  AIsFirstCellInRow, AIsLastCellInRow: Boolean;
  ALeftBorderWidth, ARightBorderWidth: TdxModelUnit;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ACellTopMargin, ATopBorderHeight, ACellSpacingValue, AGridColumnCount, ALeftShift, ARightShift: Integer;
begin
  AGridPosition := FCellsBounds[ANewCell];
  Result := AGridPosition.Bounds;
  ATopMargin := ANewCell.GetActualTopMargin;
  if ATopMargin.&Type = TdxWidthUnitType.ModelUnits then
    ACellTopMargin := ATopMargin.Value
  else
    ACellTopMargin := 0;
  ACellTopMargin := ANewCell.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ACellTopMargin);
  ATopBorderHeight := FBorderCalculator.GetActualWidth(ANewCell.GetActualTopCellBorder);
  ATopBorderHeight := ANewCell.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ATopBorderHeight);
  ATopAnchor := TableViewInfoManager.GetRowStartAnchor(AGridPosition.RowIndex);
  Result.MoveToTop(ATopAnchor.VerticalPosition + ATopAnchor.BottomTextIndent - ACellTopMargin - ATopBorderHeight);

  ACellSpacing := ANewCell.Row.CellSpacing;
  ACellSpacingValue := ACellSpacing.Value * 2;
  if (ACellSpacing.&Type = TdxWidthUnitType.ModelUnits) and (ACellSpacingValue > 0) then
  begin
    AConverter := ANewCell.DocumentModel.ToDocumentLayoutUnitConverter;
    AIsFirstCellInRow := AGridPosition.ColumnIndex = 0;
    AGridColumnCount := ANewCell.Row.Cells.Last.GetEndColumnIndexConsiderRowGrid + 1;
    AIsLastCellInRow := (AGridPosition.ColumnIndex + AGridPosition.ColumnSpan) = AGridColumnCount;

    ALeftShift := ACellSpacingValue;
    ARightShift := ACellSpacingValue;

    ALeftBorderWidth := VerticalBordersCalculator.GetLeftBorderWidth(FBorderCalculator, ANewCell, True);
    ARightBorderWidth := VerticalBordersCalculator.GetRightBorderWidth(FBorderCalculator, ANewCell);
    Inc(ALeftShift, ALeftBorderWidth div 2);
    Inc(ARightShift, ARightBorderWidth div 2);
    ALeftShift := AConverter.ToLayoutUnits(ALeftShift);
    ARightShift := AConverter.ToLayoutUnits(ARightShift);

    if not AIsFirstCellInRow then
      ALeftShift := ALeftShift div 2;

    if not AIsLastCellInRow then
      ARightShift := ARightShift div 2;

    Result.MoveToLeft(Result.Left + ALeftShift);
    Result.Width := Result.Width - (ALeftShift + ARightShift);
    if Result.Width < 0 then
      Result.Width := 1;
  end;
end;

function TdxTablesControllerTableState.GetCurrentCell: TdxTableCell;
begin
  Result := CurrentCell;
end;

function TdxTablesControllerTableState.GetCurrentCellViewInfoEmpty: Boolean;
begin
  Result := FCurrentCellRowCount = 0;
end;

function TdxTablesControllerTableState.GetCurrentTable: TdxTable;
begin
  if CurrentCell <> nil then
    Result := CurrentCell.Table
  else
    Result := nil;
end;

function TdxTablesControllerTableState.GetRowOffset(ARow: TdxTableRow; ASimpleView: Boolean): Integer;
var
  AAlignment: TdxTableRowAlignment;
  AMaxRight: Integer;
  ATable: TdxTable;
  ALeftCellMargin: TdxModelUnit;
  ALayoutLeftCellMargin: TdxLayoutUnit;
begin
  AAlignment := ARow.TableRowAlignment;
  if (AAlignment <> TdxTableRowAlignment.Center) and (AAlignment <> TdxTableRowAlignment.Right) then
    Exit(0);
  case AAlignment of
    TdxTableRowAlignment.Center:
      begin
        Result := (FMaxTableWidth - FTableRight) div 2;
        if (Result < 0) and ASimpleView then
          Result := 0;
      end;
    TdxTableRowAlignment.Right:
      begin
        AMaxRight := FMaxTableWidth;
        ATable := ARow.Table;
        if ATable.NestedLevel = 0 then
        begin
          ALeftCellMargin := GetActualCellLeftMargin(ATable.Rows.First.Cells.First);
          ALayoutLeftCellMargin := UnitConverter.ToLayoutUnits(ALeftCellMargin);
          Inc(AMaxRight, ALayoutLeftCellMargin);
        end;
        Exit(AMaxRight - FTableRight);
      end;
  else
    Result := 0;
  end;
end;

function TdxTablesControllerTableState.GetRowsController: TdxRowsController;
begin
  Result := TablesController.RowsController;
end;

function TdxTablesControllerTableState.GetRowSpan(const AGridPosition: TdxLayoutGridRectangle): Integer;
var
  I, J: Integer;
  ARow: TdxTableRow;
  ACellFound: Boolean;
  ACell: TdxTableCell;
  ARows: TdxTableRowCollection;
  ACells: TdxTableCellCollection;
  ANextGridPosition: TdxLayoutGridRectangle;
begin
  ARows := CurrentTable.Rows;
  Result := 1;
  for I := AGridPosition.RowIndex + 1 to ARows.Count - 1 do
  begin
    ARow := ARows[I];
    ACells := ARow.Cells;
    ACellFound := False;
    for J := 0 to ACells.Count - 1 do
    begin
      ACell := ACells[J];
      if ACell.VerticalMerging = TdxMergingState.Continue then
      begin
        ANextGridPosition := FCellsBounds[ACell];
        if (ANextGridPosition.ColumnIndex = AGridPosition.ColumnIndex) and (ANextGridPosition.ColumnSpan = AGridPosition.ColumnSpan) then
        begin
          ACellFound := True;
          Break;
        end;
      end;
    end;
    if not ACellFound then
      Break;
    Inc(Result);
  end;
end;

function TdxTablesControllerTableState.GetTextBounds(ACell: TdxTableCell;
  const ACellBounds: TRect): TRect;
var
  ATopBorderHeight, ATopMargin, ALeftBorderWidth, ARightBorderWidth, ALeftMargin, ARightMargin,
    ALeftOffsetInModelUnit, ARightOffsetInModelUnit: TdxModelUnit;
  ALeftOffset, AMarginsWidth, ATopOffset: TdxLayoutUnit;
begin
  ATopBorderHeight := FBorderCalculator.GetActualWidth(ACell.GetActualTopCellBorder);
  ATopMargin := GetActualCellTopMargin(ACell);

  ALeftBorderWidth := VerticalBordersCalculator.GetLeftBorderWidth(FBorderCalculator, ACell, True);
  ARightBorderWidth := VerticalBordersCalculator.GetRightBorderWidth(FBorderCalculator, ACell);
  ALeftMargin := GetActualCellLeftMargin(ACell);
  ARightMargin := GetActualCellRightMargin(ACell);

  ALeftOffsetInModelUnit := Math.Max(ALeftMargin, ALeftBorderWidth div 2);
  ARightOffsetInModelUnit := Math.Max(ARightMargin, ARightBorderWidth div 2);
  ALeftOffset := UnitConverter.ToLayoutUnits(ALeftOffsetInModelUnit);
  AMarginsWidth := UnitConverter.ToLayoutUnits(ALeftOffsetInModelUnit + ARightOffsetInModelUnit);
  ATopOffset := UnitConverter.ToLayoutUnits(ATopBorderHeight + ATopMargin);
  Result := TRect.CreateSize(ACellBounds.Left + ALeftOffset, ACellBounds.Top + ATopOffset,
    Math.Max(0, ACellBounds.Width - AMarginsWidth), ACellBounds.Height);
end;

function TdxTablesControllerTableState.GetWidth(AGrid: TdxTableGrid; AFirstColumn, ACount: Integer): TdxLayoutUnit;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ACount - 1 do
    Inc(Result, Math.Max(AGrid.Columns[AFirstColumn + I].Width, 1));
end;

function TdxTablesControllerTableState.IsFirstTableRowViewInfoInColumn: Boolean;
begin
  Result := FTableViewInfoManager.IsFirstTableRowViewInfoInColumn;
end;

procedure TdxTablesControllerTableState.LeaveCurrentTable(ABeforeRestart, ARoolbackParent: Boolean);
var
  AColumnController: TdxTableCellColumnController;
  ANewRowBounds: TRect;
begin
  if not ABeforeRestart then
  begin
    FinishCurrentCell;
    FinishCurrentTable;
  end;
  TableViewInfoManager.LeaveCurrentTable(ABeforeRestart);
  AColumnController := TdxTableCellColumnController(RowsController.ColumnController);
  RowsController.SetColumnController(AColumnController.Parent);
  FCurrentCell := nil;
  if ABeforeRestart then
  begin
    if not ARoolbackParent then
      RowsController.CurrentColumn := AColumnController.FirstParentColumn
    else
      RowsController.CurrentColumn := AColumnController.ParentColumn;
  end
  else
    RowsController.CurrentColumn := AColumnController.LastParentColumn;

  if not ABeforeRestart then
  begin
    ANewRowBounds := RowsController.CurrentRow.Bounds;
    ANewRowBounds.MoveToTop(TableViewInfoManager.GetLastTableBottom);
    ANewRowBounds.Width := RowsController.CurrentColumn.Bounds.Width;
    RowsController.CurrentRow.Bounds := ANewRowBounds;
  end
  else
    RowsController.CurrentRow.Bounds := FRowBoundsBeforeTableStart;
end;

procedure TdxTablesControllerTableState.OnCurrentRowFinished;
begin
  Inc(FCurrentCellRowCount);
end;

function TdxTablesControllerTableState.CalculateShiftedPositions(ASimpleView: Boolean): TdxLayoutUnitSortedList;
var
  ARows: TdxTableRowCollection;
  I, J, ARowCount, AOffset, ACellCount, AColumnIndex, AColumnSpan: Integer;
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ACell: TdxTableCell;
  AGridBounds: TdxLayoutGridRectangle;
  ABounds: TRect;
begin
  Result := TdxLayoutUnitSortedList.Create;
  ARows := CurrentTable.Rows;
  ARowCount := ARows.Count;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := ARows[I];
    AOffset := GetRowOffset(ARow, ASimpleView);
    ShiftRow(Result, ARow, AOffset);
  end;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := ARows[I];
    ARow.LayoutProperties.GridBefore := Result.BinarySearch(FCellsBounds[ARow.Cells.First].Bounds.Left);
    ARow.LayoutProperties.GridAfter := Result.Count - Result.BinarySearch(FCellsBounds[ARow.Cells.Last].Bounds.Right) - 1;
    ACells := ARow.Cells;
    ACellCount := ACells.Count;
    AColumnIndex := ARow.LayoutProperties.GridBefore;
    for J := 0 to ACellCount - 1 do
    begin
      ACell := ACells[J];
      AGridBounds := FCellsBounds[ACell];
      ABounds := AGridBounds.Bounds;
      AColumnSpan := Result.BinarySearch(ABounds.Right) - Result.BinarySearch(ABounds.Left);
      ACell.LayoutProperties.ColumnSpan := AColumnSpan;
      AGridBounds.ColumnIndex := AColumnIndex;
      AGridBounds.ColumnSpan := AColumnSpan;
      Inc(AColumnIndex, AColumnSpan);
    end;
  end;
end;

function TdxTablesControllerTableState.CalculateInitialPositions(ALeftOffset: TdxLayoutUnit): TdxLayoutUnitSortedList;
var
  AGrid: TdxTableGrid;
  ARows: TdxTableRowCollection;
  I, J, ARowCount, ACellCount, AColumnIndex: Integer;
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ACellBounds: TdxLayoutGridRectangle;
  ALeft, AWidth: TdxLayoutUnit;
  ACell: TdxTableCell;
begin
  AGrid := TableViewInfoManager.GetTableGrid;
  ARows := CurrentTable.Rows;
  Result := TdxLayoutUnitSortedList.Create;
  ARowCount := ARows.Count;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := ARows[I];
    ACells := ARow.Cells;
    ACellCount := ACells.Count;
    ALeft := GetWidth(AGrid, 0, ARow.GridBefore) + ALeftOffset;
    AColumnIndex := ARow.GridBefore;
    Result.Add(ALeft);
    for J := 0 to ACellCount - 1 do
    begin
      ACell := ACells[J];
      AWidth := GetWidth(AGrid, AColumnIndex, ACell.ColumnSpan);

      ACellBounds := FCellsBounds[ACell];
      ACellBounds.Bounds := TRect.CreateSize(ALeft, 0, AWidth, 0);
      FCellsBounds[ACell] := ACellBounds;

      Inc(ALeft, AWidth);
      Inc(AColumnIndex, ACell.ColumnSpan);
      Result.Add(ALeft);
    end;
    if (I = 0) or (ALeft > FTableRight) then
      FTableRight := ALeft;
  end;
end;

function TdxTablesControllerTableState.PrepareCellsBounds(ALeftOffset: TdxLayoutUnit; ASimpleView: Boolean): TdxVerticalBorderPositions;
var
  AInitialPositions, AShiftedPositions: TdxLayoutUnitSortedList;
begin
  AInitialPositions := CalculateInitialPositions(ALeftOffset);
  AShiftedPositions := CalculateShiftedPositions(ASimpleView);
  Result := TdxVerticalBorderPositions.Create(AInitialPositions, AShiftedPositions);
end;

procedure TdxTablesControllerTableState.PrepareGridCellBounds;
var
  ARow: TdxTableRow;
  ACell: TdxTableCell;
  ACellBounds: TdxLayoutGridRectangle;
  ARows: TdxTableRowCollection;
  ACells: TdxTableCellCollection;
  I, J, ARowCount, AColumnIndex: Integer;
begin
  ARows := CurrentTable.Rows;
  ARowCount := ARows.Count;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := ARows[I];
    ACells := ARow.Cells;
    AColumnIndex := ARow.GridBefore;
    for J := 0 to ACells.Count - 1 do
    begin
      ACell := ACells[J];
      FCellsBounds.Add(ACell, TdxLayoutGridRectangle.Create(cxNullRect, I, AColumnIndex, ACell.ColumnSpan));
      Inc(AColumnIndex, ACell.ColumnSpan);
    end;
  end;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := ARows[I];
    ACells := ARow.Cells;
    for J := 0 to ACells.Count - 1 do
    begin
      ACell := ACells[J];
      ACellBounds := FCellsBounds[ACell];
      case ACell.VerticalMerging of
        TdxMergingState.None:
          ACellBounds.RowSpan := 1;
        TdxMergingState.Continue:
          ACellBounds.RowSpan := 0;
        TdxMergingState.Restart:
          ACellBounds.RowSpan := GetRowSpan(FCellsBounds[ACell]);
      end;
      FCellsBounds[ACell] := ACellBounds;
    end;
  end;
end;

procedure TdxTablesControllerTableState.ProcessPendingCells(ATableViewInfo: TdxTableViewInfo;
  APendingCells: TdxList<TdxTableCellViewInfo>);
var
  I: Integer;
begin
  for I := APendingCells.Count - 1 downto 0 do
    Assert(False);
end;

function TdxTablesControllerTableState.RollbackToStartOfRowTableOnFirstCellRowColumnOverfull(
  AFirstTableRowViewInfoInColumn, AInnerMostTable: Boolean): TdxParagraphIndex;
var
  ACells: TdxTableCellCollection;
  AParagraphIndex: TdxParagraphIndex;
  I, ANewColumnBottom, ABottomAnchorIndex, AFirstInvalidRowIndex, ANestedLevel: Integer;
begin
  if AFirstTableRowViewInfoInColumn then
  begin
    ABottomAnchorIndex := FTableViewInfoManager.GetCurrentCellBottomAnchorIndex;
    if CurrentCell.Table.NestedLevel = 0 then
    begin
      TablesController.AddInfinityTableBreak(FTableViewInfoManager.GetCurrentTableViewInfoIndex, ABottomAnchorIndex);
      AFirstInvalidRowIndex := CalcFirstInvalidRowIndex;
    end
    else
      AFirstInvalidRowIndex := 0;
  end
  else
  begin
    ANewColumnBottom := FTableViewInfoManager.GetCurrentCellTop;
    if AInnerMostTable then
      TablesController.AddTableBreak(CurrentTable, FTableViewInfoManager.GetCurrentTableViewInfoIndex, ANewColumnBottom);
    AFirstInvalidRowIndex := CalcFirstInvalidRowIndex;
  end;

  TableViewInfoManager.RemoveAllInvalidRowsOnColumnOverfull(AFirstInvalidRowIndex);
  if AFirstInvalidRowIndex = 0 then
  begin
    FRestartFrom := TdxRestartFrom.TableStart;
    AParagraphIndex := CurrentTable.Rows.First.Cells.First.StartParagraphIndex;
    ANestedLevel := CurrentCell.Table.NestedLevel;
    LeaveCurrentTable(True, CurrentTable.NestedLevel > 0);
    TablesController.ReturnToPrevState;
    if ANestedLevel > 0 then
      Exit(-1)
    else
      Exit(AParagraphIndex);
  end
  else
  begin
    ACells := CurrentTable.Rows[AFirstInvalidRowIndex].Cells;
    for I := 0 to ACells.Count - 1 do
      if ACells[I].VerticalMerging <> TdxMergingState.Continue then
      begin
        FRestartFrom := TdxRestartFrom.CellStart;
        Exit(ACells[I].StartParagraphIndex);
      end;
    TdxRichEditExceptions.ThrowInternalException;
    Exit(0);
  end;
end;

procedure TdxTablesControllerTableState.ShiftRow(APositions: TdxLayoutUnitSortedList; ARow: TdxTableRow;
  AOffset: Integer);
var
  I: Integer;
  ABounds: TRect;
  ACell: TdxTableCell;
  ACells: TdxTableCellCollection;
  ACellBounds: TdxLayoutGridRectangle;
begin
  ACells := ARow.Cells;
  for I := 0 to ACells.Count - 1 do
  begin
    ACell := ACells[I];
    ABounds := FCellsBounds[ACell].Bounds;
    ABounds.Offset(AOffset, 0);
    APositions.Add(ABounds.Left);
    ACellBounds := FCellsBounds[ACell];
    ACellBounds.Bounds := ABounds;
    FCellsBounds[ACell] := ACellBounds;
  end;
  APositions.Add(FCellsBounds[ACells.Last].Bounds.Right);
end;

procedure TdxTablesControllerTableState.StartNewTable(ATable: TdxTable; AFirstContentInParentCell, ASimpleView: Boolean);
var
  ATableTop: Integer;
  AColumnBounds: TRect;
  ATableTextArea: TdxTextArea;
  ATableViewInfo: TdxTableViewInfo;
  ATableCellController: TdxTableCellColumnController;
  ACellBordersInfo: TdxHorizontalCellBordersInfoList;
  ABounds, ACellBounds, ATextBounds, ANewRowBounds: TRect;
  AModelRelativeIndent, AActualTableIndent, AModelLeftOffset, ARightCellMargin: TdxModelUnit;
  ALayoutLeftOffset, ALayoutRightTableBorder, ARightTableBorder, ALayoutRightCellMargin, AMinTableWidth, ABottomTextIndent: TdxLayoutUnit;
  APreferredWidth: TdxWidthUnitInfo;
begin
  AModelRelativeIndent := CalculateTableActualLeftRelativeOffset(ATable);
  if (ATable.TableIndent.&Type = TdxWidthUnitType.ModelUnits) and (ATable.TableAlignment = TdxTableRowAlignment.Left) then
    AActualTableIndent := ATable.TableIndent.Value
  else
    AActualTableIndent := 0;
  AModelLeftOffset := AActualTableIndent + AModelRelativeIndent;
  ALayoutLeftOffset := UnitConverter.ToLayoutUnits(AModelLeftOffset);
  ARightCellMargin := GetActualCellRightMargin(ATable.Rows.First.Cells.First);
  ALayoutRightTableBorder := 0;
  if RowsController.MatchHorizontalTableIndentsToTextEdge then
  begin
    ARightTableBorder := VerticalBordersCalculator.GetRightBorderWidth(FBorderCalculator, ATable.FirstRow.LastCell);
    ALayoutRightTableBorder := UnitConverter.ToLayoutUnits(ARightTableBorder);
  end;
  ALayoutRightCellMargin := UnitConverter.ToLayoutUnits(ARightCellMargin);
  ATableTop := RowsController.CurrentRow.Bounds.Top;
  APreferredWidth := ATable.GetActualPreferredWidth;
  if (ATable.TableLayout = TdxTableLayoutType.Autofit) and (APreferredWidth.&Type in [TdxWidthUnitType.Auto, TdxWidthUnitType.&Nil]) then
  begin
    AMinTableWidth := 1;
    ATableTop := RowsController.MoveCurrentRowDownToFitTable(AMinTableWidth, ATableTop);
    ATableTextArea := RowsController.GetTextAreaForTable;
    ATableViewInfo := TableViewInfoManager.StartNewTable(ATable, FCellsBounds, ALayoutLeftOffset, ALayoutRightCellMargin,
      AFirstContentInParentCell, ATableTextArea, FMaxTableWidth);
    ATableViewInfo.TextAreaOffset := ATableTextArea.Start - RowsController.CurrentColumn.Bounds.Left;
    Inc(ALayoutLeftOffset, ATableViewInfo.TextAreaOffset);
  end
  else
  begin
    AColumnBounds := RowsController.CurrentColumn.Bounds;
    ATableTextArea := TdxTextArea.Create(AColumnBounds.Left, AColumnBounds.Right - ALayoutRightTableBorder);
    ATableViewInfo := TableViewInfoManager.StartNewTable(ATable, FCellsBounds, ALayoutLeftOffset, ALayoutRightCellMargin,
      AFirstContentInParentCell, ATableTextArea, FMaxTableWidth);
    ATableTop := RowsController.MoveCurrentRowDownToFitTable(AColumnBounds.Width, ATableTop);
    ATableTextArea := RowsController.GetTextAreaForTable;
    ATableViewInfo.TextAreaOffset := ATableTextArea.Start - RowsController.CurrentColumn.Bounds.Left;
    Inc(ALayoutLeftOffset, ATableViewInfo.TextAreaOffset);
  end;

  ATableViewInfo.VerticalBorderPositions := PrepareCellsBounds(ALayoutLeftOffset, ASimpleView);
  ATableViewInfo.LeftOffset := ALayoutLeftOffset;
  ATableViewInfo.ModelRelativeIndent := AModelRelativeIndent;
  EnsureCurrentTableRow(CurrentCell.Row);

  ABottomTextIndent := CalculateBottomTextIndent(0, ACellBordersInfo);
  FRowBoundsBeforeTableStart := RowsController.CurrentRow.Bounds;
  TableViewInfoManager.SetRowSeparator(0, TdxTableCellVerticalAnchor.Create(ATableTop, ABottomTextIndent, ACellBordersInfo));

  ABounds := GetCellBounds(CurrentCell);

  ACellBounds := ABounds;

  ACellBounds.Offset(RowsController.CurrentColumn.Bounds.Left, 0);
  ATextBounds := GetTextBounds(CurrentCell, ABounds);

  ATableCellController := TdxTableCellColumnController.Create(RowsController.ColumnController,
    RowsController.CurrentColumn, ATextBounds.Left, ATextBounds.Top, ATextBounds.Width, ATableViewInfo, CurrentCell);
  RowsController.SetColumnController(ATableCellController);
  TableViewInfoManager.ColumnController := ATableCellController;

  TableViewInfoManager.StartNextCell(CurrentCell, ACellBounds, FCellsBounds[CurrentCell], ATextBounds);

  ANewRowBounds := RowsController.CurrentRow.Bounds;
  ANewRowBounds.Y := ATextBounds.Top;
  RowsController.CurrentRow.Bounds := ANewRowBounds;

  TableViewInfoManager.CurrentCellBottom := ATableViewInfo.TopAnchor.VerticalPosition +
    ATableViewInfo.TopAnchor.BottomTextIndent;

  RowsController.CurrentColumn := ATableCellController.GetStartColumn;
  RowsController.OnCellStart;
  FCurrentCellRowCount := 0;
end;

procedure TdxTablesControllerTableState.UpdateCurrentCellBottom(ABottom: TdxLayoutUnit);
begin
  if CurrentCell = nil then
    Exit;
  TableViewInfoManager.CurrentCellBottom := Math.Max(ABottom, TableViewInfoManager.CurrentCellBottom);
end;

procedure TdxTablesControllerTableState.UpdateCurrentCellHeight(ARow: TdxRow);
begin
  if CurrentCell = nil then
    Exit;
  TableViewInfoManager.CurrentCellBottom := Math.Max(ARow.Bounds.Bottom, TableViewInfoManager.CurrentCellBottom);
end;

{ TdxTableCellBorderIterator }

constructor TdxTableCellBorderIterator.Create(AAboveRow, ABelowRow: TdxTableRow);
begin
  inherited Create;
  if (AAboveRow = nil) and (ABelowRow = nil) then
    TdxRichEditExceptions.ThrowInternalException;
  if AAboveRow <> nil then
    FAboveRowIterator := TdxTableCellIterator.Create(AAboveRow)
  else
    FAboveRowIterator := TdxTableCellEmptyIterator.Create;
  if ABelowRow <> nil then
    FBelowRowIterator := TdxTableCellIterator.Create(ABelowRow)
  else
    FBelowRowIterator := TdxTableCellEmptyIterator.Create;
  FCurrentCellAbove := nil;
  FCurrentCellBelow := nil;
  FAboveRowIterator.MoveNextCell;
  FBelowRowIterator.MoveNextCell;
end;

destructor TdxTableCellBorderIterator.Destroy;
begin
  FAboveRowIterator.Free;
  FBelowRowIterator.Free;
  inherited Destroy;
end;

function TdxTableCellBorderIterator.GetCurrentAboveInfo: TdxNullableHorizontalCellBordersInfo;
var
  ABorder: TdxBorderBase;
begin
  Result.Reset;
  if FCurrentCellAbove = nil then
    Exit;
  ABorder := FCurrentCellAbove.GetActualBottomCellBorder;
  Result := TdxHorizontalCellBordersInfo.Create(FCurrentCellAbove, FCurrentCellBelow, ABorder, FCurrentStartColumnIndex, FCurrentEndColumnIndex);
end;

function TdxTableCellBorderIterator.GetCurrentBelowInfo: TdxNullableHorizontalCellBordersInfo;
var
  ABorder: TdxBorderBase;
begin
  Result.Reset;
  if FCurrentCellBelow = nil then
    Exit;
  ABorder := FCurrentCellBelow.GetActualTopCellBorder;
  Result := TdxHorizontalCellBordersInfo.Create(FCurrentCellAbove, FCurrentCellBelow, ABorder, FCurrentStartColumnIndex, FCurrentEndColumnIndex);
end;

function TdxTableCellBorderIterator.GetCurrentMergedInfo: TdxHorizontalCellBordersInfo;
begin
  Assert((FCurrentCellAbove <> nil) and (FCurrentCellBelow <> nil));
  Result := TdxHorizontalCellBordersInfo.Create(FCurrentCellAbove, FCurrentCellBelow, nil, FCurrentStartColumnIndex, FCurrentEndColumnIndex);
end;

function TdxTableCellBorderIterator.IsVerticallyMerged: Boolean;
begin
  if (FCurrentCellAbove = nil) or (FCurrentCellBelow = nil) then
    Exit(False);
  Result := ((FCurrentCellAbove.VerticalMerging = TdxMergingState.Restart) or (FCurrentCellAbove.VerticalMerging = TdxMergingState.Continue)) and
    (FCurrentCellBelow.VerticalMerging = TdxMergingState.Continue);
end;

function TdxTableCellBorderIterator.MoveNext: Boolean;
var
  AAboveStartColumnIndex, ABelowStartColumnIndex: Integer;
begin
  if FAboveRowIterator.EndOfRow and FBelowRowIterator.EndOfRow then
    Exit(False);
  if FAboveRowIterator.EndOfRow then
  begin
    FCurrentStartColumnIndex := FBelowRowIterator.CurrentStartColumnIndex;
    FCurrentEndColumnIndex := FBelowRowIterator.CurrentEndColumnIndex;
    FCurrentCellAbove := nil;
    FCurrentCellBelow := FBelowRowIterator.CurrentCell;
    FBelowRowIterator.SetStartColumnIndex(FCurrentEndColumnIndex + 1);
    Exit(True);
  end;
  if FBelowRowIterator.EndOfRow then
  begin
    FCurrentStartColumnIndex := FAboveRowIterator.CurrentStartColumnIndex;
    FCurrentEndColumnIndex := FAboveRowIterator.CurrentEndColumnIndex;
    FCurrentCellAbove := FAboveRowIterator.CurrentCell;
    FCurrentCellBelow := nil;
    FAboveRowIterator.SetStartColumnIndex(FCurrentEndColumnIndex + 1);
    Exit(True);
  end;
  AAboveStartColumnIndex := FAboveRowIterator.CurrentStartColumnIndex;
  ABelowStartColumnIndex := FBelowRowIterator.CurrentStartColumnIndex;
  if AAboveStartColumnIndex < ABelowStartColumnIndex then
  begin
    FCurrentStartColumnIndex := AAboveStartColumnIndex;
    FCurrentEndColumnIndex := Math.Min(ABelowStartColumnIndex - 1, FAboveRowIterator.CurrentEndColumnIndex);
    FCurrentCellAbove := FAboveRowIterator.CurrentCell;
    FAboveRowIterator.SetStartColumnIndex(FCurrentEndColumnIndex + 1);
    Result := True;
  end
  else
    if ABelowStartColumnIndex < AAboveStartColumnIndex then
    begin
      FCurrentStartColumnIndex := ABelowStartColumnIndex;
      FCurrentEndColumnIndex := Math.Min(AAboveStartColumnIndex - 1, FBelowRowIterator.CurrentEndColumnIndex);
      FCurrentCellBelow := FBelowRowIterator.CurrentCell;
      FBelowRowIterator.SetStartColumnIndex(FCurrentEndColumnIndex + 1);
      Result := True;
    end
    else
    begin
      FCurrentStartColumnIndex := AAboveStartColumnIndex;
      FCurrentEndColumnIndex := Math.Min(FAboveRowIterator.CurrentEndColumnIndex, FBelowRowIterator.CurrentEndColumnIndex);
      FCurrentCellAbove := FAboveRowIterator.CurrentCell;
      FCurrentCellBelow := FBelowRowIterator.CurrentCell;
      FAboveRowIterator.SetStartColumnIndex(FCurrentEndColumnIndex + 1);
      FBelowRowIterator.SetStartColumnIndex(FCurrentEndColumnIndex + 1);
      Result := True;
    end;
end;

{ TdxTableCellHorizontalBorderCalculator }

constructor TdxTableCellHorizontalBorderCalculator.Create(ATable: TdxTable);
begin
  inherited Create;
  FTable := ATable;
end;

function TdxTableCellHorizontalBorderCalculator.GetAnchorBorders(
  AAnchorIndex: Integer): TdxHorizontalCellBordersInfoList;
var
  ARowAboveIndex, ARowBelowIndex: Integer;
  ARowAbove, ARowBelow: TdxTableRow;
  AIterator: TdxTableCellBorderIterator;
  ABorder: TdxHorizontalCellBordersInfo;
  AAboveBorder, ABelowBorder: TdxNullableHorizontalCellBordersInfo;
begin
  ARowAboveIndex := AAnchorIndex - 1;
  ARowBelowIndex := AAnchorIndex;
  if ARowAboveIndex >= 0 then
    ARowAbove := FTable.Rows[ARowAboveIndex]
  else
    ARowAbove := nil;
  if ARowBelowIndex < FTable.Rows.Count then
    ARowBelow := FTable.Rows[ARowBelowIndex]
  else
    ARowBelow := nil;
  AIterator := TdxTableCellBorderIterator.Create(ARowAbove, ARowBelow);
  try
    Result := TdxHorizontalCellBordersInfoList.Create;
    while AIterator.MoveNext do
    begin
      if not AIterator.IsVerticallyMerged then
      begin
        AAboveBorder := AIterator.CurrentAboveInfo;
        ABelowBorder := AIterator.CurrentBelowInfo;
        ABorder := ResolveBorder(FTable, AAboveBorder, ABelowBorder);
      end
      else
        ABorder := AIterator.CurrentMergedInfo;
      Result.Add(ABorder);
    end;
  finally
    AIterator.Free;
  end;
end;

function TdxTableCellHorizontalBorderCalculator.GetBottomBorders(
  ARow: TdxTableRow): TdxHorizontalCellBordersInfoList;
var
  ARowIndex, AAnchorIndex: Integer;
begin
  ARowIndex := FTable.Rows.IndexOf(ARow);
  AAnchorIndex := ARowIndex + 1;
  Result := GetAnchorBorders(AAnchorIndex);
end;

function TdxTableCellHorizontalBorderCalculator.GetTopBorders(ARow: TdxTableRow): TdxHorizontalCellBordersInfoList;
var
  ARowIndex, AAnchorIndex: Integer;
begin
  ARowIndex := FTable.Rows.IndexOf(ARow);
  AAnchorIndex := ARowIndex;
  Result := GetAnchorBorders(AAnchorIndex);
end;

function TdxTableCellHorizontalBorderCalculator.ResolveBorder(ATable: TdxTable; ABorder1,
  ABorder2: TdxNullableHorizontalCellBordersInfo): TdxHorizontalCellBordersInfo;
var
  ABorderCalculator: TdxTableBorderCalculator;
  ABorderInfo: TdxBorderInfo;
begin
  Assert((not ABorder1.IsNull) or (not ABorder2.IsNull));
  if ABorder1.IsNull then
    Exit(ABorder2.Value);
  if ABorder2.IsNull then
    Exit(ABorder1.Value);
  ABorderCalculator := TdxTableBorderCalculator.Create;
  try
    ABorderInfo := ABorderCalculator.GetVerticalBorderSource(
      ATable, ABorder1.Value.Border.Info, ABorder2.Value.Border.Info);
  finally
    ABorderCalculator.Free;
  end;
  if ABorderInfo = ABorder1.Value.Border.Info then
    Result := ABorder1.Value
  else
    Result := ABorder2.Value;
end;

{ TdxTextAreaAndXComparable }

function TdxTextAreaAndXComparable.CompareTo(const AOther: TdxTextArea): Integer;
begin
  if FX < AOther.Start then
    Result := 1
  else
    if FX > AOther.Start then
    begin
      if FX <= AOther.&End then
        Result := 0
      else
        Result := -1;
    end
    else
      Result := 0;
end;

constructor TdxTextAreaAndXComparable.Create(X: Integer);
begin
  inherited Create;
  FX := X;
end;

{ THorizontalObjectComparer }

function THorizontalObjectComparer.Compare(const ALeft, ARight: TdxBoxBase): Integer;
begin
  Result := CompareInteger(TdxFloatingObjectBox(ALeft).X, TdxFloatingObjectBox(ARight).X);
end;

{ TdxFloatingObjectsLayout.TVerticalObjectComparer }

function TVerticalObjectComparer.Compare(const ALeft, ARight: TdxBoxBase): Integer;
begin
  Result := CompareInteger(TdxFloatingObjectBox(ALeft).Y, TdxFloatingObjectBox(ARight).Y);
end;

{ TdxFloatingObjectsLayout }

constructor TdxFloatingObjectsLayout.Create;
begin
  inherited Create;
  FItems := TdxFloatingObjectBoxList.Create;
  FForegroundItems := TdxFloatingObjectBoxList.Create;
  FBackgroundItems := TdxFloatingObjectBoxList.Create;
  FRuns := TdxList<TdxFloatingObjectAnchorRun>.Create;
  FObjectsTable := TDictionary<TdxFloatingObjectAnchorRun, TdxFloatingObjectBox>.Create;
  FObjectToRunMapTable := TDictionary<TdxFloatingObjectBox, TdxFloatingObjectAnchorRun>.Create;
end;

destructor TdxFloatingObjectsLayout.Destroy;
begin
  FreeAndNil(FObjectToRunMapTable);
  FreeAndNil(FObjectsTable);
  FreeAndNil(FRuns);
  FreeAndNil(FBackgroundItems);
  FreeAndNil(FForegroundItems);
  FreeAndNil(FItems);
  inherited Destroy;
end;

class constructor TdxFloatingObjectsLayout.Initialize;
begin
  FHorizontalObjectComparer := THorizontalObjectComparer.Create;
  FVerticalObjectComparer := TVerticalObjectComparer.Create;
end;

class destructor TdxFloatingObjectsLayout.Finalize;
begin
  FreeAndNil(FHorizontalObjectComparer);
  FreeAndNil(FVerticalObjectComparer);
end;

function TdxFloatingObjectsLayout.FindLeftMostX(AProcessedObjects: TdxFloatingObjectBoxList; AInitialX: Integer;
  const ABounds: TRect): Integer;
var
  I, AObjectBoundsRight: Integer;
begin
  Result := ABounds.Left;
  for I := 0 to AProcessedObjects.Count - 1 do
  begin
    AObjectBoundsRight := AProcessedObjects[I].ExtendedBounds.Right;
    if (AObjectBoundsRight < AInitialX) and (AObjectBoundsRight > Result) then
      Result := AObjectBoundsRight;
  end;
end;

function TdxFloatingObjectsLayout.FindRightMostX(AProcessedObjects: TdxFloatingObjectBoxList; AInitialX: Integer;
  const ABounds: TRect): Integer;
var
  I, AObjectBoundsLeft: Integer;
begin
  Result := ABounds.Right;
  for I := 0 to AProcessedObjects.Count - 1 do
  begin
    AObjectBoundsLeft := AProcessedObjects[i].ExtendedBounds.Left;
    if (AObjectBoundsLeft > AInitialX) and (AObjectBoundsLeft < Result) then
      Result := AObjectBoundsLeft;
  end;
end;

function TdxFloatingObjectsLayout.GetAllObjectsInRectangle(const ABounds: TRect): TdxFloatingObjectBoxList;
begin
  ABounds.Height := Max(1, ABounds.Height);
  Result := TdxFloatingObjectBoxList.Create;
  GetObjectsInRectangle(Items, Result, ABounds);
  GetObjectsInRectangle(ForegroundItems, Result, ABounds);
  GetObjectsInRectangle(BackgroundItems, Result, ABounds);
end;

function TdxFloatingObjectsLayout.GetFloatingObject(AObjectAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectBox;
begin
  if not FObjectsTable.TryGetValue(AObjectAnchorRun, Result) then
    Result := nil;
end;

procedure TdxFloatingObjectsLayout.GetFloatingObjects(AWhere: TdxFloatingObjectBoxList; APieceTable: TdxPieceTable;
  AObjects: TdxFloatingObjectBoxList);
var
  I: Integer;
begin
  if AObjects = nil then
    Exit;

  for I := 0 to AObjects.Count - 1 do
    if AObjects[I].PieceTable = APieceTable then
        AWhere.Add(AObjects[I]);
end;

function TdxFloatingObjectsLayout.GetFloatingObjects(APieceTable: TdxPieceTable): TdxFloatingObjectBoxList;
begin
  Result := TdxFloatingObjectBoxList.Create;
  GetFloatingObjects(Result, APieceTable, Items);
  GetFloatingObjects(Result, APieceTable, ForegroundItems);
  GetFloatingObjects(Result, APieceTable, BackgroundItems);
end;

procedure TdxFloatingObjectsLayout.GetObjectsInRectangle(AWhere, ATo: TdxFloatingObjectBoxList;
  const ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to AWhere.Count - 1 do
    if AWhere[I].ExtendedBounds.IntersectsWith(ABounds) then
      ATo.Add(AWhere[I]);
end;

procedure TdxFloatingObjectsLayout.MoveFloatingObjectsVertically(ADeltaY: Integer; APieceTable: TdxPieceTable);
var
  I: Integer;
  AObjects: TdxFloatingObjectBoxList;
begin
  AObjects := GetFloatingObjects(APieceTable);
  try
    for I := 0 to AObjects.Count - 1 do
      AObjects[I].MoveVertically(ADeltaY);
  finally
    AObjects.Free;
  end;
end;

procedure TdxFloatingObjectsLayout.ProcessFloatingObject(AFloatingObject: TdxFloatingObjectBox;
  AProcessedObjects: TdxFloatingObjectBoxList; AResult: TdxTextAreaCollectionEx; const AInitialBounds: TRect);
var
  ABounds: TRect;
  ALeftMostX, ARightMostX, ALeftSideWidth, ARightSideWidth: Integer;
begin
  ABounds := AFloatingObject.ExtendedBounds;
  ALeftMostX := FindLeftMostX(AProcessedObjects, ABounds.Left, AInitialBounds);
  ARightMostX := FindRightMostX(AProcessedObjects, ABounds.Right, AInitialBounds);

  AProcessedObjects.Add(AFloatingObject);

  AResult.Remove(TdxTextArea.Create(ABounds.Left, ABounds.Right));
  ALeftSideWidth := ABounds.Left - ALeftMostX;
  ARightSideWidth := ARightMostX - ABounds.Right;
  if not AFloatingObject.CanPutTextAtLeft or (AFloatingObject.PutTextAtLargestSide and (ALeftSideWidth <= ARightSideWidth)) then
    AResult.Remove(TdxTextArea.Create(ALeftMostX, ABounds.Left));

  if not AFloatingObject.CanPutTextAtRight or (AFloatingObject.PutTextAtLargestSide and (ARightSideWidth < ALeftSideWidth)) then
    AResult.Remove(TdxTextArea.Create(ABounds.Right, ARightMostX));
end;

function TdxFloatingObjectsLayout.GetObjectsInRectangle(const ABounds: TRect): TdxFloatingObjectBoxList;
var
  R: TRect;
begin
  R := ABounds;
  R.Height := Math.Max(1, ABounds.Height);
  Result := TdxFloatingObjectBoxList.Create;
  GetObjectsInRectangle(Items, Result, R);
  Result.Sort(FHorizontalObjectComparer);
end;

procedure TdxFloatingObjectsLayout.Add(AObjectAnchorRun: TdxFloatingObjectAnchorRun;
  AFloatingObject: TdxFloatingObjectBox);
begin
  if Runs.Contains(AObjectAnchorRun) then
    Exit;

  Runs.Add(AObjectAnchorRun);
  FObjectsTable.Add(AObjectAnchorRun, AFloatingObject);
  FObjectToRunMapTable.Add(AFloatingObject, AObjectAnchorRun);

  if AObjectAnchorRun.FloatingObjectProperties.TextWrapType = TdxFloatingObjectTextWrapType.None then
  begin
    if AObjectAnchorRun.FloatingObjectProperties.IsBehindDoc then
      FBackgroundItems.Add(AFloatingObject)
    else
      FForegroundItems.Add(AFloatingObject);
  end
  else
    Add(AFloatingObject);
end;

procedure TdxFloatingObjectsLayout.Add(AFloatingObject: TdxFloatingObjectBox);
var
  AIndex: Integer;
begin
  if FItems.BinarySearch(AFloatingObject, AIndex, FVerticalObjectComparer) then
  begin
    FItems.Insert(AIndex, AFloatingObject);
    Exit;
  end;

  if AIndex >= FItems.Count then
    FItems.Add(AFloatingObject)
  else
    FItems.Insert(AIndex, AFloatingObject);
end;

procedure TdxFloatingObjectsLayout.ClearFloatingObjects(ARunIndex: TdxRunIndex; AObjects: TdxFloatingObjectBoxList);
var
  I, ACount: Integer;
  AFloatingObject: TdxFloatingObjectBox;
  ARun: TdxFloatingObjectAnchorRun;
begin
  ACount := AObjects.Count;
  for I := ACount - 1 downto 0 do
  begin
    AFloatingObject := AObjects[I];
    if ARunIndex <= AFloatingObject.StartPos.RunIndex then
    begin
      AFloatingObject.LockPosition := False;
      ARun := FObjectToRunMapTable[AFloatingObject];
      FObjectToRunMapTable.Remove(AFloatingObject);
      FObjectsTable.Remove(ARun);
      Runs.Remove(ARun);
      AObjects.Remove(AFloatingObject);
    end;
  end;
end;


procedure TdxFloatingObjectsLayout.Clear;
begin
  Items.Clear;
  BackgroundItems.Clear;
  ForegroundItems.Clear;
  Runs.Clear;
  FObjectsTable.Clear;
  FObjectToRunMapTable.Clear;
end;

procedure TdxFloatingObjectsLayout.ClearFloatingObjects(ARunIndex: TdxRunIndex);
begin
  ClearFloatingObjects(ARunIndex, Items);
  ClearFloatingObjects(ARunIndex, BackgroundItems);
  ClearFloatingObjects(ARunIndex, ForegroundItems);
end;

function TdxFloatingObjectsLayout.ContainsRun(AObjectAnchorRun: TdxFloatingObjectAnchorRun): Boolean;
begin
  Result := FRuns.Contains(AObjectAnchorRun);
end;

{ TdxPageAreaController }

procedure TdxPageAreaController.BeginSectionFormatting(ASection: TdxSection; ACurrentColumnsCount: Integer);
begin
  State.BeginSectionFormatting(ASection, ACurrentColumnsCount);
end;

procedure TdxPageAreaController.ClearInvalidatedContent(const APos: TdxFormatterPosition);
begin
  State.ClearInvalidatedContent(APos);
end;

function TdxPageAreaController.CompleteCurrentAreaFormatting: TdxCompleteFormattingResult;
begin
  Result := State.CompleteCurrentAreaFormatting;
end;

constructor TdxPageAreaController.Create(APageController: TdxPageController);
begin
  inherited Create;
  FPageController := APageController;
  SwitchToState(CreateDefaultState(0));
end;

destructor TdxPageAreaController.Destroy;
begin
  FreeAndNil(FState);
  inherited Destroy;
end;

function TdxPageAreaController.CreateDefaultState(ACurrentAreaIndex: Integer): TdxPageAreaControllerState;
begin
  Result := TdxDefaultPageAreaControllerState.Create(Self, ACurrentAreaIndex);
end;

function TdxPageAreaController.GetAreas: TdxPageAreaCollection;
begin
  Result := FPageController.Pages.Last.Areas;
end;

function TdxPageAreaController.GetCurrentAreaBounds: TRect;
begin
  Result := State.CurrentAreaBounds;
end;

function TdxPageAreaController.GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea;
begin
  Result := State.GetNextPageArea(AKeepFloatingObjects);
end;

procedure TdxPageAreaController.Reset(ASection: TdxSection);
begin
  State.Reset(ASection);
end;

procedure TdxPageAreaController.RestartFormattingFromTheMiddleOfSection(ASection: TdxSection;
  ACurrentAreaIndex: Integer);
begin
  State.RestartFormattingFromTheMiddleOfSection(ASection, ACurrentAreaIndex);
end;

procedure TdxPageAreaController.RestartFormattingFromTheStartOfRowAtCurrentPage;
begin
  State.RestartFormattingFromTheStartOfRowAtCurrentPage;
end;

procedure TdxPageAreaController.RestartFormattingFromTheStartOfSection(ASection: TdxSection;
  ACurrentAreaIndex: Integer);
begin
  State.RestartFormattingFromTheStartOfSection(ASection, ACurrentAreaIndex);
end;

procedure TdxPageAreaController.SwitchToState(AState: TdxPageAreaControllerState);
begin
  FreeAndNil(FState);
  FState := AState;
end;

procedure TdxPageAreaController.RemoveLastPageArea;
begin
  Areas.Delete(Areas.Count - 1);
  if Areas.Count = 0 then
    PageController.RemoveLastPage;
end;

{ TdxPageAreaControllerState }

procedure TdxPageAreaControllerState.BeginSectionFormatting(ASection: TdxSection; ACurrentColumnsCount: Integer);
begin
  ApplySectionStart(ASection, ACurrentColumnsCount);
  CreateCurrentAreaBounds;
end;

procedure TdxPageAreaControllerState.ClearInvalidatedContent(const APos: TdxFormatterPosition);
var
  AAreas: TdxPageAreaCollection;
  AAreaIndex: Integer;
begin
  AAreas := Areas;
  AAreaIndex := AAreas.BinarySearchBoxIndex(APos);
  if AAreaIndex < 0 then
  begin
    AAreaIndex := not AAreaIndex;
  end;
  if AAreaIndex + 1 < AAreas.Count then
    AAreas.DeleteRange(AAreaIndex + 1, AAreas.Count - AAreaIndex - 1);
end;

constructor TdxPageAreaControllerState.Create(AOwner: TdxPageAreaController);
begin
  inherited Create;
  Assert(AOwner <> nil);
  FOwner := AOwner;
end;

procedure TdxPageAreaControllerState.CreateCurrentAreaBounds;
begin
  FCurrentAreaBounds := CreateCurrentAreaBoundsCore;
end;

function TdxPageAreaControllerState.GetAreas: TdxPageAreaCollection;
begin
  Result := Owner.Areas;
end;

procedure TdxPageAreaControllerState.RestoreCurrentAreaBounds(const AOldBounds: TRect);
begin
  FCurrentAreaBounds := AOldBounds;
end;

function TdxPageAreaControllerState.GetNextPageAreaCore: TdxPageArea;
begin
  Result := TdxPageArea.Create(PageController.PieceTable.ContentType, PageController.CurrentSection);
  Result.Bounds := CurrentAreaBounds;
end;

function TdxPageAreaControllerState.GetPageController: TdxPageController;
begin
  Result := Owner.PageController;
end;

procedure TdxPageAreaControllerState.Reset(ASection: TdxSection);
begin
  BeginSectionFormatting(ASection, 0);
end;

procedure TdxPageAreaControllerState.RestartFormattingFromTheMiddleOfSection(ASection: TdxSection;
  ACurrentAreaIndex: Integer);
begin
  Assert(ACurrentAreaIndex >= 0);
  CreateCurrentAreaBounds;
end;

procedure TdxPageAreaControllerState.RestartFormattingFromTheStartOfRowAtCurrentPage;
begin
end;

procedure TdxPageAreaControllerState.RestartFormattingFromTheStartOfSection(ASection: TdxSection;
  ACurrentAreaIndex: Integer);
begin
  Assert(ACurrentAreaIndex >= 0);
  CreateCurrentAreaBounds;
end;

{ TdxPageController }

constructor TdxPageController.Create(ADocumentLayout: TdxDocumentLayout;
  AFloatingObjectsLayout: TdxFloatingObjectsLayout; AParagraphFramesLayout: TdxParagraphFramesLayout);
begin
  inherited Create;
  Assert(ADocumentLayout <> nil);
  FDocumentLayout := ADocumentLayout;
  FNextPageOrdinalType := TdxSectionStartType.Continuous;
  FPageBoundsCalculator := CreatePageBoundsCalculator;
  if AFloatingObjectsLayout = nil then
  begin
    FFloatingObjectsLayout := TdxFloatingObjectsLayout.Create;
    FNeedDisposeFloatingObjectsLayout := True;
  end
  else
    FFloatingObjectsLayout := AFloatingObjectsLayout;
  if AParagraphFramesLayout = nil then
  begin
    FParagraphFramesLayout := TdxParagraphFramesLayout.Create;
    FNeedDisposeParagraphFramesLayout := True;
  end
  else
    FParagraphFramesLayout := AParagraphFramesLayout;
end;

destructor TdxPageController.Destroy;
begin
  FreeAndNil(FPageBoundsCalculator);
  if FNeedDisposeFloatingObjectsLayout then
    FreeAndNil(FFloatingObjectsLayout);
  if FNeedDisposeParagraphFramesLayout then
    FreeAndNil(FParagraphFramesLayout);
  inherited Destroy;
end;

procedure TdxPageController.EndTableFormatting;
var
  I, ANewPageCount: Integer;
  ANewPages: array of TdxPage;
begin
  FTableStarted := False;
  ANewPageCount := Pages.Count - FPagesBeforeTable;
  if ANewPageCount <= 0 then
    Exit;
  SetLength(ANewPages, ANewPageCount);
  for I := 0 to ANewPageCount - 1 do
  begin
    ANewPages[I] := Pages[FPagesBeforeTable];
    Pages.Extract(ANewPages[I]);
  end;

  for I := 0 to ANewPageCount - 1 do
  begin
    PageContentFormattingComplete(Pages.Last);
    Pages.Add(ANewPages[I]);
  end;
end;

procedure TdxPageController.SetFloatingObjectsLayout(ALayout: TdxFloatingObjectsLayout);
begin
  FFloatingObjectsLayout := ALayout;
  RaiseFloatingObjectsLayoutChanged;
end;

procedure TdxPageController.FinalizePagePrimaryFormatting(APage: TdxPage; ADocumentEnded: Boolean);
begin
  if ADocumentEnded then
  begin
      AppendFloatingObjectsToPage(APage);
  end;
  if APage.Areas.Last.Section = CurrentSection then
    FFirstPageOfSection := False;
end;

function TdxPageController.AppendFloatingObjectsToPage(APage: TdxPage): Boolean;
var
  APageContainsFloatingObjects: Boolean;
  AMaxRunIndex: TdxRunIndex;
begin
  APageContainsFloatingObjects := (FloatingObjectsLayout.Items.Count > 0) or
    (FloatingObjectsLayout.ForegroundItems.Count > 0) or
    (FloatingObjectsLayout.BackgroundItems.Count > 0);
  if not APageContainsFloatingObjects and not (ParagraphFramesLayout.Items.Count > 0) then
  begin
    ClearFloatingObjectsLayout;
    ClearParagraphFramesLayout;
    Exit(True);
  end;

  if APage.IsEmpty then
    AMaxRunIndex := MaxInt
  else
    AMaxRunIndex := APage.GetLastPosition(PieceTable).RunIndex;
  if ContainsOrphanedItems(FloatingObjectsLayout.Items, AMaxRunIndex) then
    Exit(False);
  if ContainsOrphanedItems(FloatingObjectsLayout.ForegroundItems, AMaxRunIndex) then
    Exit(False);
  if ContainsOrphanedItems(FloatingObjectsLayout.BackgroundItems, AMaxRunIndex) then
    Exit(False);
  if ContainsOrphanedItems(ParagraphFramesLayout.Items) then
    Exit(False);

  AppendFloatingObjects(FloatingObjectsLayout.Items, APage.FloatingObjects, AMaxRunIndex);
  AppendFloatingObjects(FloatingObjectsLayout.ForegroundItems, APage.ForegroundFloatingObjects, AMaxRunIndex);
  AppendFloatingObjects(FloatingObjectsLayout.BackgroundItems, APage.BackgroundFloatingObjects, AMaxRunIndex);
  ClearFloatingObjectsLayout;
  AppendParagraphFrames(ParagraphFramesLayout.Items, APage.ParagraphFrames);
  ParagraphFramesLayout.Clear;
  Result := True;
end;

procedure TdxPageController.ApplyExistingFooterAreaBounds(APage: TdxPage);
begin
end;

procedure TdxPageController.ApplyExistingHeaderAreaBounds(APage: TdxPage);
begin
end;

procedure TdxPageController.ApplySectionStart(ASection: TdxSection);
begin
  FNextPageOrdinalType := CalculateNextPageOrdinalType(ASection);
  if ASection.FootNote.NumberingRestartType = TdxLineNumberingRestart.NewSection then
    DocumentLayout.Counters.GetCounter(TdxFootNote.FootNoteCounterId).LastValue := ASection.FootNote.StartingNumber;
  if ASection.EndNote.NumberingRestartType = TdxLineNumberingRestart.NewSection then
    DocumentLayout.Counters.GetCounter(TdxEndNote.EndNoteCounterId).LastValue := ASection.EndNote.StartingNumber;
end;

procedure TdxPageController.BeginNextSectionFormatting(ASectionIndex: TdxSectionIndex);
var
  ASection: TdxSection;
begin
  CurrentSectionIndex := ASectionIndex;
  ASection := TdxDocumentModel(DocumentLayout.DocumentModel).Sections[CurrentSectionIndex];
  ApplySectionStart(ASection);

  RestartFormattingFromTheMiddleOfSectionCore(ASection);
  FFirstPageOfSection := True;
end;

procedure TdxPageController.BeginTableFormatting;
begin
  if not FTableStarted then
    FPagesBeforeTable := Pages.Count;
  FTableStarted := True;
end;

function TdxPageController.CalculateNextPageOrdinalType(ASection: TdxSection): TdxSectionStartType;
begin
  Result := TdxSectionStartType.Continuous;
  case ASection.GeneralSettings.StartType of
    TdxSectionStartType.OddPage:
      Result := TdxSectionStartType.OddPage;
    TdxSectionStartType.EvenPage:
      Result := TdxSectionStartType.EvenPage;
  end;
end;

function TdxPageController.CalculatePageBounds(ASection: TdxSection): TRect;
begin
  Result := PageBoundsCalculator.CalculatePageBounds(ASection);
end;

function TdxPageController.CalculatePageOrdinal(AFirstPageOfSection: Boolean): TdxCalculatePageOrdinalResult;
var
  APageOrdinal, ANumSkippedPages: Integer;
begin
  if PageCount <= 0 then
  begin
    APageOrdinal := 0;
    ANumSkippedPages := 0;
  end
  else
  begin
    APageOrdinal := Pages.Last.PageOrdinal;
    ANumSkippedPages := Pages.Last.NumSkippedPages;
  end;

  Result := CalculatePageOrdinalCore(APageOrdinal, ANumSkippedPages, AFirstPageOfSection);
end;

function TdxPageController.CalculatePageOrdinalCore(APreviousPageOrdinal, ANumSkippedPages: Integer;
  AFirstPageOfSection: Boolean): TdxCalculatePageOrdinalResult;
var
  APageOrdinal: Integer;
  AIsPageOrdinalOdd: Boolean;
begin
  if AFirstPageOfSection and (not CurrentSection.PageNumbering.ContinueNumbering) then
    APageOrdinal := CurrentSection.PageNumbering.FirstPageNumber
  else
    APageOrdinal := APreviousPageOrdinal + 1;

  AIsPageOrdinalOdd := ((APageOrdinal) mod 2) <> 0;
  case FNextPageOrdinalType of
    TdxSectionStartType.OddPage:
      begin
        if AIsPageOrdinalOdd then
          Result := TdxCalculatePageOrdinalResult.Create(APageOrdinal, ANumSkippedPages)
        else
          Result := TdxCalculatePageOrdinalResult.Create(APageOrdinal + 1, ANumSkippedPages + 1);
      end;
    TdxSectionStartType.EvenPage:
      begin
        if AIsPageOrdinalOdd then
          Result := TdxCalculatePageOrdinalResult.Create(APageOrdinal, ANumSkippedPages)
        else
          Result := TdxCalculatePageOrdinalResult.Create(APageOrdinal + 1, ANumSkippedPages + 1);
      end;
    else
      Result := TdxCalculatePageOrdinalResult.Create(APageOrdinal, ANumSkippedPages);
  end;
end;

procedure TdxPageController.ClearFloatingObjectsLayout;
begin
  FloatingObjectsLayout.Clear;
  RaiseFloatingObjectsLayoutChanged;
end;

procedure TdxPageController.ClearParagraphFramesLayout;
begin
  ParagraphFramesLayout.Clear;
  RaiseParagraphFramesLayoutChanged;
end;

procedure TdxPageController.FormatHeader(APage: TdxPage; AFirstPageOfSection: Boolean);
begin
end;

procedure TdxPageController.FormatFooter(APage: TdxPage; AFirstPageOfSection: Boolean);
begin
end;

function TdxPageController.ClearInvalidatedContent(const APos: TdxFormatterPosition; AParagraphIndex: TdxParagraphIndex;
  AKeepFloatingObjects, AEmptyCurrentRow: Boolean): TdxClearInvalidatedContentResult;
var
  ALastColumn: TdxColumn;
  APieceTable: TdxPieceTable;
  APageIndex, APageCount: Integer;
  ANewSectionIndex: TdxSectionIndex;
  ASection: TdxSection;
begin
  if not AKeepFloatingObjects then
  begin
    FloatingObjectsLayout.ClearFloatingObjects(APos.RunIndex);
    ParagraphFramesLayout.ClearParagraphFrames(APos.RunIndex);
  end;

  FTableStarted := False;
  APageIndex := Pages.BinarySearchBoxIndex(APos);
  if APageIndex < 0 then
    APageIndex := not APageIndex;
  if APageIndex > 0 then
  begin
    ALastColumn := Pages[APageIndex - 1].GetLastColumn;
    ALastColumn.RemoveEmptyTableViewInfos;
  end;
  if (APageIndex >= Pages.Count) and AEmptyCurrentRow then
  begin
    if not Pages.Last.IsEmpty then
      Exit(TdxClearInvalidatedContentResult.NoRestart);
    CorrectLastPageOrdinal(AParagraphIndex, APageIndex);
    Exit(TdxClearInvalidatedContentResult.NoRestart);
  end;

  Assert(APageIndex >= 0);

  if APageIndex + 1 < Pages.Count then
  begin
    APageCount := Pages.Count - APageIndex - 1;
    if APageCount > 0 then
    begin
      Pages.DeleteRange(APageIndex + 1, APageCount);
      RaisePageCountChanged;
    end;
  end;

  APieceTable := PieceTable;
  if APageIndex < Pages.Count then
    Pages[APageIndex].ClearInvalidatedContent(APos.RunIndex, APieceTable);

  ANewSectionIndex := APieceTable.LookupSectionIndexByParagraphIndex(AParagraphIndex);
  if (ANewSectionIndex > CurrentSectionIndex) and (GetActualFormattingSectionIndex(ANewSectionIndex) > CurrentSectionIndex) then
    Exit(TdxClearInvalidatedContentResult.NoRestart);
  CurrentSectionIndex := ANewSectionIndex;

  ASection := DocumentModel.Sections[CurrentSectionIndex];
  if (ASection.FirstParagraphIndex = AParagraphIndex) and (APos.Offset = 0) and (APos.RunIndex = APieceTable.Paragraphs[AParagraphIndex].FirstRunIndex) then
  begin
    CurrentSectionIndex := CurrentSectionIndex - 1;
    Pages[APageIndex].ClearInvalidatedContent(APos.RunIndex, nil);
    Exit(TdxClearInvalidatedContentResult.RestartFromTheStartOfSection);
  end
  else
    Result := TdxClearInvalidatedContentResult.Restart;
end;

function TdxPageController.GetActualFormattingSectionIndex(ASectionIndex: TdxSectionIndex): TdxSectionIndex;
var
  APrevSectionIndex: TdxSectionIndex;
  APrevSectionLastParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex;
begin
  while ASectionIndex > dxSectionIndexMinValue do
  begin
    APrevSectionIndex := ASectionIndex - 1;
    APrevSectionLastParagraphIndex := DocumentModel.Sections[APrevSectionIndex].LastParagraphIndex;
    ARunIndex := PieceTable.Paragraphs[APrevSectionLastParagraphIndex].LastRunIndex;
    if PieceTable.VisibleTextFilter.IsRunVisible(ARunIndex) then
      Exit(ASectionIndex);
    ASectionIndex := APrevSectionIndex;
  end;
  Result := ASectionIndex;
end;

function TdxPageController.CompleteCurrentPageFormatting: TdxCompleteFormattingResult;
begin
  if (PageCount > 0) and not AppendFloatingObjectsToPage(Pages.Last) then
    Result := TdxCompleteFormattingResult.OrphanedFloatingObjects
  else
    Result := TdxCompleteFormattingResult.Success;
end;

function TdxPageController.ClearInvalidatedContentFromTheStartOfRowAtCurrentPage(const APos: TdxFormatterPosition;
  AParagraphIndex: TdxParagraphIndex; AKeepFloatingObjects: Boolean; ATablesController: TdxTablesController;
  AEmptyCurrentRow: Boolean; out ALeaveTable: Boolean): TdxClearInvalidatedContentResult;
var
  ACurrentCell: TdxTableCell;
begin
  ALeaveTable := False;
  if FTableStarted then
  begin
    ACurrentCell := ATablesController.GetCurrentCell;
    if (ACurrentCell <> nil) and (AParagraphIndex >= ACurrentCell.Table.StartParagraphIndex) or
      (ACurrentCell.Table.NestedLevel > 0) then
      Exit(TdxClearInvalidatedContentResult.ClearOnlyTableCellRows);
    ATablesController.RemoveAllTableBreaks;
    ALeaveTable := True;
  end;
  Result := ClearInvalidatedContent(APos, AParagraphIndex, AKeepFloatingObjects, AEmptyCurrentRow);
end;

procedure TdxPageController.CorrectLastPageOrdinal(AParagraphIndex: TdxParagraphIndex; APageIndex: Integer);
var
  APieceTable: TdxPieceTable;
  ALastNonEmptyPage: TdxPage;
  AEmptyPageStartLogPosition: TdxDocumentLogPosition;
  ASectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  APageOrdinal: TdxCalculatePageOrdinalResult;
begin
  APageIndex := Pages.Count - 2;
  if APageIndex < 0 then
    Exit;

  APieceTable := PieceTable;

  ALastNonEmptyPage := Pages[APageIndex];
  AEmptyPageStartLogPosition := ALastNonEmptyPage.GetLastPosition(APieceTable).LogPosition + 1;
  ASectionIndex := APieceTable.LookupSectionIndexByParagraphIndex(AParagraphIndex);
  ASection := DocumentModel.Sections[ASectionIndex];
  if (APieceTable.Paragraphs[ASection.FirstParagraphIndex].LogPosition = AEmptyPageStartLogPosition) then
  begin
    FNextPageOrdinalType := CalculateNextPageOrdinalType(ASection);
    APageOrdinal := CalculatePageOrdinal(FirstPageOfSection);
    Pages.Last.PageOrdinal := APageOrdinal.PageOrdinal;
    Pages.Last.NumSkippedPages := APageOrdinal.SkippedPageCount;
    FNextPageOrdinalType := TdxSectionStartType.Continuous;
  end;
end;

procedure TdxPageController.RemoveLastPage;
begin
  Pages.Delete(Pages.Count - 1);
  ClearFloatingObjectsLayout;
  ClearParagraphFramesLayout;
end;

procedure TdxPageController.SetPageLastRunIndex(ARunIndex: TdxRunIndex);
begin
  FPageLastRunIndex := ARunIndex;
  FloatingObjectsLayout.ClearFloatingObjects(ARunIndex + 1);
  ParagraphFramesLayout.ClearParagraphFrames(ARunIndex + 1);
end;

function TdxPageController.GetCurrentSection: TdxSection;
begin
  Result := FCurrentSection;
end;

function TdxPageController.GetCurrentSectionIndex: TdxSectionIndex;
begin
  Result := FCurrentSectionIndex;
end;

function TdxPageController.GetFirstPageOfSection: Boolean;
begin
  Result := FFirstPageOfSection;
end;

function TdxPageController.GetNextPage(AKeepFloatingObjects: Boolean): TdxPage;
var
  AIsFirstPageOfSection: Boolean;
  APageOrdinal: TdxCalculatePageOrdinalResult;
begin
  if (PageCount > 0) and not FTableStarted then
    PageContentFormattingComplete(Pages.Last);

  if not AKeepFloatingObjects then
    ClearFloatingObjectsLayout;
  ClearParagraphFramesLayout;
  Result := GetNextPageCore;
  Result.PageIndex := PageCount;
  APageOrdinal := CalculatePageOrdinal(FFirstPageOfSection);
  Result.PageOrdinal := APageOrdinal.PageOrdinal;
  Result.NumSkippedPages := APageOrdinal.SkippedPageCount;
  Result.NumPages := DocumentModel.ExtendedDocumentProperties.Pages;
  FNextPageOrdinalType := TdxSectionStartType.Continuous;
  Pages.Add(Result);
  RaisePageFormattingStarted(Result);
  AIsFirstPageOfSection := NextSection or FFirstPageOfSection;
  FormatHeader(Result, AIsFirstPageOfSection);
  FormatFooter(Result, AIsFirstPageOfSection);
  FCurrentPageClientBounds := Result.ClientBounds;
  FFirstPageOfSection := False;
  FNextSection := False;

  RaisePageCountChanged;

  ResetPageLastRunIndex;
end;

function TdxPageController.GetNextPageCore: TdxPage;
begin
  Result := TdxPage.Create;
  Result.Bounds := FPageBounds;
  Result.ClientBounds := FPageClientBounds;
  if CurrentSection.FootNote.NumberingRestartType = TdxLineNumberingRestart.NewPage then
    DocumentLayout.Counters.GetCounter(TdxFootNote.FootNoteCounterId).LastValue := CurrentSection.FootNote.StartingNumber;
  if CurrentSection.EndNote.NumberingRestartType = TdxLineNumberingRestart.NewPage then
    DocumentLayout.Counters.GetCounter(TdxEndNote.EndNoteCounterId).LastValue := CurrentSection.EndNote.StartingNumber;
end;

function TdxPageController.GetPages: TdxPageCollection;
begin
  Result := DocumentLayout.Pages;
end;

function TdxPageController.GetPieceTable: TdxPieceTable;
begin
  Result := DocumentModel.MainPieceTable;
end;

procedure TdxPageController.PopulateFloatingObjects(AFloatingObjects: TdxFloatingObjectBoxList);
var
  I: Integer;
  AFloatingObject: TdxFloatingObjectBox;
  ARun: TdxFloatingObjectAnchorRun;
begin
  if AFloatingObjects = nil then
    Exit;

  for I := 0 to AFloatingObjects.Count - 1 do
  begin
    AFloatingObject := AFloatingObjects[I];
    ARun := TdxFloatingObjectAnchorRun(AFloatingObject.PieceTable.Runs[AFloatingObject.StartPos.RunIndex]);
    FloatingObjectsLayout.Add(ARun, AFloatingObject);
  end;
end;

procedure TdxPageController.PopulateParagraphFramesLayout(ALastPage: TdxPage);
begin
  PopulateParagraphFrames(ALastPage.InnerParagraphFrames);
end;

procedure TdxPageController.PopulateParagraphFrames(AParagraphFrames: TdxParagraphFrameBoxList);
var
  I: Integer;
begin
  if AParagraphFrames = nil then
    Exit;

  for I := 0 to AParagraphFrames.Count - 1 do
    ParagraphFramesLayout.AddParagraphFrameBox(AParagraphFrames[I]);
end;

procedure TdxPageController.PopulateFloatingObjectsLayout(ALastPage: TdxPage);
begin
  PopulateFloatingObjects(ALastPage.InnerFloatingObjects);
  PopulateFloatingObjects(ALastPage.InnerForegroundFloatingObjects);
  PopulateFloatingObjects(ALastPage.InnerBackgroundFloatingObjects);
end;

function TdxPageController.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(FDocumentLayout.DocumentModel);
end;

procedure TdxPageController.SetCurrentSectionIndex(const Value: TdxSectionIndex);
begin
  FCurrentSectionIndex := Value;
end;

function TdxPageController.IsCurrentPageEven: Boolean;
begin
  if PageCount = 0 then
      Exit(False);
  Result := Pages.Last.IsEven;
end;

procedure TdxPageController.AppendFloatingObjects(AFrom, ATo: TdxFloatingObjectBoxList; AMaxRunIndex: TdxRunIndex);
var
  I: Integer;
  AFloatingObject: TdxFloatingObjectBox;
begin
  ATo.Clear;
  if AFrom.Count = 0 then
    Exit;
  for I := 0 to AFrom.Count - 1 do
  begin
    AFloatingObject := AFrom[I];
    if (AFloatingObject.PieceTable <> PieceTable) or (AFloatingObject.StartPos.RunIndex <= AMaxRunIndex) then
      ATo.Add(AFloatingObject);
  end;
  ATo.Sort(TdxFloatingObjectBox.ZOrderComparer);
end;

procedure TdxPageController.AppendParagraphFrames(AFrom, ATo: TdxParagraphFrameBoxList);
var
  ACount, I: Integer;
  ABox: TdxParagraphFrameBox;
begin
  ATo.Clear;
  if AFrom.Count = 0 then
    Exit;
  ACount := AFrom.Count;
  for I := 0 to ACount - 1 do
  begin
    ABox := AFrom[I];
    if (ABox.PieceTable <> PieceTable) or (ABox.StartPos.RunIndex <= PageLastRunIndex) then
      ATo.Add(ABox);
  end;
  ATo.Sort(TdxParagraphFrameBox.IndexComparer);
end;

procedure TdxPageController.RaisePageCountChanged;
begin
  if not FOnPageCountChanged.Empty then
    FOnPageCountChanged.Invoke(Self, nil);
end;

procedure TdxPageController.RaiseParagraphFramesLayoutChanged;
begin
  if not FOnParagraphFramesLayoutChanged.Empty then
    FOnParagraphFramesLayoutChanged.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxPageController.RaisePageFormattingComplete(APage: TdxPage);
var
  AArgs: TdxPageFormattingCompleteEventArgs;
begin
  if FOnPageFormattingComplete.Empty then
    Exit;
  AArgs := TdxPageFormattingCompleteEventArgs.Create(APage, False);
  try
    FOnPageFormattingComplete.Invoke(Self, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxPageController.RaisePageFormattingStarted(APage: TdxPage);
var
  AArgs: TdxPageFormattingCompleteEventArgs;
begin
  if FOnPageFormattingStarted.Empty then
    Exit;
  AArgs := TdxPageFormattingCompleteEventArgs.Create(APage, False);
  try
    FOnPageFormattingStarted.Invoke(Self, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxPageController.RaiseFloatingObjectsLayoutChanged;
begin
  if not FOnFloatingObjectsLayoutChanged.Empty then
    FOnFloatingObjectsLayoutChanged.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxPageController.PageContentFormattingComplete(APage: TdxPage);
begin
  RaisePageFormattingComplete(APage);
end;

procedure TdxPageController.Reset(AKeepFloatingObjects: Boolean);
begin
  Pages.Clear;
  FTableStarted := False;
  CurrentSectionIndex := -1;
  FreeAndNil(FPageBoundsCalculator);
  FPageBoundsCalculator := CreatePageBoundsCalculator;
  BeginNextSectionFormatting(CurrentSectionIndex + 1);
  if not AKeepFloatingObjects then
    ClearFloatingObjectsLayout;
  ClearParagraphFramesLayout;
  RaisePageCountChanged;
end;

procedure TdxPageController.ResetPageLastRunIndex;
begin
  FPageLastRunIndex := MaxInt;
end;

procedure TdxPageController.RestartFormattingFromTheMiddleOfSection(ASection: TdxSection);
var
  ALastPage: TdxPage;
begin
  RestartFormattingFromTheMiddleOfSectionCore(ASection);
  ALastPage := Pages.Last;
  if ALastPage <> nil then
  begin
    ApplyExistingHeaderAreaBounds(ALastPage);
    ApplyExistingFooterAreaBounds(ALastPage);
    FCurrentPageClientBounds := ALastPage.ClientBounds;
  end;

  FNextPageOrdinalType := TdxSectionStartType.Continuous;
end;

procedure TdxPageController.RestartFormattingFromTheMiddleOfSectionCore(ASection: TdxSection);
var
  ALastPage: TdxPage;
begin
  FCurrentSection := ASection;
  FPageBounds := CalculatePageBounds(CurrentSection);
  FPageClientBounds := PageBoundsCalculator.CalculatePageClientBounds(CurrentSection);
  FCurrentPageClientBounds := FPageClientBounds;

  if Pages.Count > 0 then
  begin
    ALastPage := Pages.Last;
    if not ALastPage.IsEmpty then
    begin
      PopulateFloatingObjectsLayout(ALastPage);
      ALastPage.ClearFloatingObjects;
      PopulateParagraphFramesLayout(ALastPage);
      ALastPage.ClearParagraphFrames;
    end;
  end;
end;

procedure TdxPageController.RestartFormattingFromTheStartOfRowAtCurrentPage;
begin
end;

procedure TdxPageController.RestartFormattingFromTheStartOfSection;
var
  F9DB0D08252841399B9BC9932A1C4800: TdxSection;
begin
  CurrentSectionIndex := CurrentSectionIndex + 1;
  F9DB0D08252841399B9BC9932A1C4800 := TdxDocumentModel(DocumentLayout.DocumentModel).Sections[CurrentSectionIndex];

  ApplySectionStart(F9DB0D08252841399B9BC9932A1C4800);
  RestartFormattingFromTheMiddleOfSectionCore(F9DB0D08252841399B9BC9932A1C4800);
  FFirstPageOfSection := True;
end;

function TdxPageController.ContainsOrphanedItems(AItems: TdxFloatingObjectBoxList; AMaxRunIndex: TdxRunIndex): Boolean;
var
  I: Integer;
  AFloatingObject: TdxFloatingObjectBox;
begin
  Result := False;
  if AItems.Count = 0 then
    Exit;
  for I := 0 to AItems.Count - 1 do
  begin
    AFloatingObject := AItems[i];
    if (AFloatingObject.PieceTable = PieceTable) and (AFloatingObject.StartPos.RunIndex > AMaxRunIndex) and
      AFloatingObject.WasRestart then
        Exit(True);
  end;
end;

function TdxPageController.ContainsOrphanedItems(AItems: TdxParagraphFrameBoxList): Boolean;
var
  ACount, I: Integer;
  AParagraphFrame: TdxParagraphFrameBox;
begin
  if AItems.Count = 0 then
    Exit(False);
  ACount := AItems.Count;
  for I := 0 to ACount - 1 do
  begin
    AParagraphFrame := AItems[I];
    if (AParagraphFrame.PieceTable = PieceTable) and AParagraphFrame.WasRestart and (AParagraphFrame.StartPos.RunIndex > PageLastRunIndex) and
      AParagraphFrame.WasRestart then
      Exit(True);
  end;
  Result := False;
end;

function TdxPageController.GetPageCount: Integer;
begin
  Result := Pages.Count;
end;

{ TdxMaxWidthCalculator }

function TdxMaxWidthCalculator.GetMaxWidth(APage: TdxPage): Integer;
var
  I: Integer;
begin
  Result := GetMaxWidth(APage.Areas);
  if APage.FloatingObjects <> nil then
    for I := 0 to APage.FloatingObjects.Count - 1 do
      Result := Max(APage.FloatingObjects[I].Bounds.Right, Result);
end;

function TdxMaxWidthCalculator.GetMaxWidth(AAreas: TdxPageAreaCollection): Integer;
var
  ARows: TdxRowCollection;
  I: Integer;
begin
  Result := MinInt;
  for I := 0 to AAreas.Count - 1 do
  begin
    ARows := AAreas[I].Columns.First.Rows;
    Result := Max(Result, GetMaxWidth(ARows));
  end;
end;

function TdxMaxWidthCalculator.GetMaxWidth(ARows: TdxRowCollection): Integer;
var
  ALastBox: TdxBox;
  I: Integer;
begin
  Result := 0;
  for I := 0 to ARows.Count - 1 do
  begin
    ALastBox := GetLastNonLineBreakBox(ARows[I]);
    if ALastBox <> nil then
      Result := Max(Result, ALastBox.Bounds.Right);
  end;
end;

function TdxMaxWidthCalculator.GetMaxRight(ARows: TdxRowCollection): Integer;
var
  I: Integer;
  ALastBox: TdxBox;
begin
  Result := 0;
  for I := 0 to ARows.Count - 1 do
  begin
    ALastBox := GetLastNonLineBreakOrSpaceBox(ARows[I]);
    if ALastBox <> nil then
      Result := Max(Result, ALastBox.Bounds.Right);
  end;
end;

function TdxMaxWidthCalculator.GetLastNonLineBreakBox(ARow: TdxRow): TdxBox;
var
  I: Integer;
begin
  for I := ARow.Boxes.Count - 1 downto 0 do
    if not ARow.Boxes[I].IsLineBreak then
      Exit(ARow.Boxes[I]);

  Result := nil;
end;

function TdxMaxWidthCalculator.GetLastNonLineBreakOrSpaceBox(ARow: TdxRow): TdxBox;
var
  I: Integer;
begin
  for I := ARow.Boxes.Count - 1 downto 0 do
    if not ARow.Boxes[I].IsLineBreak and ARow.Boxes[I].IsNotWhiteSpaceBox then
      Exit(ARow.Boxes[I]);

  Result := nil;
end;

{ TdxParagraphFinalFormatter }

constructor TdxParagraphFinalFormatter.Create(
  ADocumentLayout: TdxDocumentLayout);
begin
  inherited Create;
  Assert(ADocumentLayout <> nil);
  FPieceTable := TdxDocumentModel(ADocumentLayout.DocumentModel).MainPieceTable;
  FUnderlineCalculator := TdxUnderlineCalculator.Create(PieceTable);
  FStrikeoutCalculator := TdxStrikeoutCalculator.Create(PieceTable);
  FDocumentLayout := ADocumentLayout;
  FBookmarkCalculator := TdxBookmarkBoxCalculator.Create(PieceTable, ADocumentLayout.MeasurerProvider);
  FRangePermissionCalculator := TdxRangePermissionBoxCalculator.Create(PieceTable, ADocumentLayout.MeasurerProvider);
  FCustomMarkCalculator := TdxCustomMarkBoxCalculator.Create();
  FRangePermissionColorer := TdxRangePermissionColorer.Create;
end;

destructor TdxParagraphFinalFormatter.Destroy;
begin
  FreeAndNil(FLineSpacingCalculator);
  FreeAndNil(FRangePermissionColorer);
  FreeAndNil(FCustomMarkCalculator);
  FreeAndNil(FRangePermissionCalculator);
  FreeAndNil(FBookmarkCalculator);
  FreeAndNil(FStrikeoutCalculator);
  FreeAndNil(FUnderlineCalculator);
  inherited Destroy;
end;


function TdxParagraphFinalFormatter.GetDocumentModel: TdxDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

procedure TdxParagraphFinalFormatter.AlignLineNumberToBaseLine(ABox: TdxLineNumberBox);
var
  ARow: TdxRow;
  ARect: TRect;
begin
  ARow := ABox.Row;
  ARect := ABox.Bounds;
  ARect.Y := ARow.Bounds.Top + ARow.BaseLineOffset - ABox.CalcAscentAndFree(PieceTable);
  ABox.Bounds := ARect;
end;

procedure TdxParagraphFinalFormatter.ApplyTableCellsVerticalContentAlignment(ATables: TdxTableViewInfoCollection);
var
  I: Integer;
  ATableViewInfo: TdxTableViewInfo;
begin
  for I := 0 to ATables.Count - 1 do
  begin
    ATableViewInfo := ATables[I];
    ApplyTableCellsVerticalContentAlignment(ATableViewInfo);
  end;
end;

procedure TdxParagraphFinalFormatter.ApplyTableCellsVerticalContentAlignment(ATable: TdxTableViewInfo);
var
  I: Integer;
  ATableCellViewInfo: TdxTableCellViewInfo;
begin
  for I := 0 to ATable.Cells.Count - 1 do
  begin
    ATableCellViewInfo := ATable.Cells[I];
    ApplyTableCellVerticalContentAlignment(ATableCellViewInfo);
  end;
end;

procedure TdxParagraphFinalFormatter.ApplyTableCellsVerticalContentAlignment(AInnerTables: TdxTableViewInfoCollection;
  AFirstRowIndex, ALastRowIndex, AOffset: Integer);
var
  I: Integer;
begin
  for I := AFirstRowIndex to ALastRowIndex do
    FCurrentColumn.Rows[I].MoveVertically(AOffset);
  for I := 0 to AInnerTables.Count - 1 do
    AInnerTables[I].MoveVerticallyRecursive(AOffset);
end;

procedure TdxParagraphFinalFormatter.ApplyTableCellVerticalContentAlignment(ACell: TdxTableCellViewInfo);
var
  AActualContentVerticalBounds: TRect;
  AFirstRowIndex, ALastRowIndex, AOffset, AInitialOffset: Integer;
begin
  if ACell.IsStartOnPreviousTableViewInfo then
    Exit;
  AFirstRowIndex := ACell.GetFirstRowIndex(FCurrentColumn);
  if AFirstRowIndex < 0 then
    Exit;
  ALastRowIndex := ACell.GetLastRowIndex(FCurrentColumn);
  if ALastRowIndex < 0 then
    Exit;
  AActualContentVerticalBounds := CalculateCellContentVerticalBounds(ACell, FCurrentColumn.Rows[AFirstRowIndex],
    FCurrentColumn.Rows[ALastRowIndex]);
  if AActualContentVerticalBounds.IsEqual(TRect.Null) then
    Exit;

  if ACell.InitialContentTop = MinInt then
    ACell.InitialContentTop := AActualContentVerticalBounds.Top;

  AOffset := CalculateContentOffset(ACell, AActualContentVerticalBounds.Bottom - AActualContentVerticalBounds.Top);
  AInitialOffset := AActualContentVerticalBounds.Top - ACell.InitialContentTop;
  Dec(AOffset, AInitialOffset);
  ApplyTableCellsVerticalContentAlignment(ACell.InnerTables, AFirstRowIndex, ALastRowIndex, AOffset);
end;

function TdxParagraphFinalFormatter.CalcBaseBoxAscentAndFree(ABox: TdxBox): Integer;
begin
  Result := ABox.CalcBaseAscentAndFree(PieceTable);
end;

function TdxParagraphFinalFormatter.CalcBaseBoxDescent(ABox: TdxBox): Integer;
begin
  Result := ABox.CalcBaseDescent(PieceTable);
end;

function TdxParagraphFinalFormatter.CalcBoxAscentAndFree(ABox: TdxBox): Integer;
begin
  Result := ABox.CalcAscentAndFree(PieceTable);
end;

function TdxParagraphFinalFormatter.CalcBoxDescent(ABox: TdxBox): Integer;
begin
  Result := ABox.CalcDescent(PieceTable)
end;

procedure TdxParagraphFinalFormatter.CalculateBackgroundHighlight(ARow: TdxRow; const ATightRowBounds: TRect);
var
  ABoxes: TdxBoxCollection;
  ACount: Integer;
  APrevStartRunIndex, AStartRunIndex, ACurrentRunIndex, ALastBoxRunIndex: TdxRunIndex;
  APrevStartBox, AStartBox, ACurrentBox, APrevBox, ALastProcessedBox, ALastBox: TdxBox;
  I: Integer;
  AIsSolidBoxInterval: Boolean;
  ARun: IdxHighlightableTextRun;
  ABackColor: TdxAlphaColor;
  ALeft, ARight: Integer;
  R, ALastBoxBounds: TRect;
begin
  ARow.ClearHighlightAreas;
  Assert(ARow.Boxes.Count > 0);
  ABoxes := ARow.Boxes;
  ACount := ABoxes.Count;
  AStartBox := ABoxes[0];
  AStartRunIndex := AStartBox.StartPos.RunIndex;
  ALastProcessedBox := nil;
  for I := 1 to ACount - 1 do
  begin
    ACurrentBox := ABoxes[I];
    ACurrentRunIndex := ACurrentBox.StartPos.RunIndex;
    AIsSolidBoxInterval := AStartRunIndex = ACurrentRunIndex;
    if AIsSolidBoxInterval and (I < ACount -1) then
      Continue;

    APrevStartRunIndex := AStartRunIndex;
    AStartRunIndex := ACurrentRunIndex;
    APrevStartBox := AStartBox;
    AStartBox := ACurrentBox;
    if not Supports(PieceTable.Runs[APrevStartRunIndex], IdxHighlightableTextRun, ARun) then
      Continue;

    ABackColor := ARun.BackColor;
    if TdxAlphaColors.IsTransparentOrEmpty(ABackColor) then
      Continue;

    APrevBox := ABoxes[I - 1];
    if AIsSolidBoxInterval and not (ACurrentBox is TdxPageBreakBox) then
      APrevBox := ACurrentBox;
    ALastProcessedBox := APrevBox;
    ALeft := APrevStartBox.Bounds.Left;
    ARight := APrevBox.Bounds.Right;
    R.Init(ALeft, ATightRowBounds.Top, ARight, ATightRowBounds.Bottom);
    ARow.HighlightAreas.Add(TdxHighlightArea.Create(ABackColor, R));
  end;
  ALastBox := ABoxes.Last;
  if (ALastProcessedBox <> ALastBox) and not (ALastBox is TdxPageBreakBox) then
  begin
    ALastBoxRunIndex := ALastBox.StartPos.RunIndex;

    if not Supports(PieceTable.Runs[ALastBoxRunIndex], IdxHighlightableTextRun, ARun) then
      ARun := nil;

    if (ARun = nil) or TdxAlphaColors.IsTransparentOrEmpty(ARun.BackColor) then
      Exit;

    ALastBoxBounds := ALastBox.Bounds;
    ALeft := ALastBoxBounds.Left;
    ARight := ALastBoxBounds.Right;
    R.Init(ALeft, ATightRowBounds.Top, ARight, ATightRowBounds.Bottom);
    ARow.HighlightAreas.Add(TdxHighlightArea.Create(ARun.BackColor, R));
  end;
end;

function TdxParagraphFinalFormatter.CalculateBottomPosition(AFrameBox, ANextFrameBox: TdxParagraphFrameBox;
  ARow: TdxRow; ACellRow: TdxTableCellRow): Integer;
begin
  if ShouldMergeParagraphFrameBoxes(AFrameBox, ANextFrameBox, ACellRow) then
    Result := ANextFrameBox.Bounds.Top
  else
    Result := CalculateRowBoxesBottom(ARow);
end;

function TdxParagraphFinalFormatter.CalculateBottomRow(ARows: TdxRowCollection; AParagraphIndex: TdxParagraphIndex;
  ACellRow: TdxTableCellRow): TdxRow;
var
  ALastRowIndex: Integer;
  ACellViewInfo: TdxTableCellViewInfo;
begin
  ALastRowIndex := FindRowByParagraphIndex(ARows, AParagraphIndex);
  if ALastRowIndex >= 0 then
    Exit(ARows[ALastRowIndex - 1]);
  if (ALastRowIndex < 0) and (ACellRow <> nil) then
  begin
    ACellViewInfo := ACellRow.CellViewInfo;
    Exit(ACellViewInfo.GetLastRow(ACellViewInfo.TableViewInfo.Column));
  end;
  Result := ARows[ARows.Count - 1];
end;

function TdxParagraphFinalFormatter.CalculateContentOffset(ACell: TdxTableCellViewInfo; AContentHeight: Integer): Integer;
var
  ATopAnchor, ABottomAnchor: TdxTableCellVerticalAnchor;
  ATop, ABottom, ACellMaxContentHeight: Integer;
begin
  ATopAnchor := ACell.TableViewInfo.Anchors[ACell.TopAnchorIndex];
  ABottomAnchor := ACell.TableViewInfo.Anchors[ACell.BottomAnchorIndex];
  ATop := ATopAnchor.VerticalPosition + ATopAnchor.BottomTextIndent;
  ABottom := ABottomAnchor.VerticalPosition - ABottomAnchor.TopTextIndent;
  ACellMaxContentHeight := ABottom - ATop;

  Result := CalculateContentOffset(ACell.Cell.VerticalAlignment, ACellMaxContentHeight, AContentHeight);
end;

function TdxParagraphFinalFormatter.CalculateCellContentVerticalBounds(ACell: TdxTableCellViewInfo; AFirstRow,
  ALastRow: TdxRow): TRect;
var
  ARowsBounds, AInnerTablesBounds: TRect;
begin
  ARowsBounds := CalculateCellRowsVerticalBounds(ACell, AFirstRow, ALastRow);
  AInnerTablesBounds := CalculateCellInnerTablesVerticalBounds(ACell.InnerTables);
  if ARowsBounds.IsEqual(TRect.Null) then
    Exit(AInnerTablesBounds);
  if AInnerTablesBounds.IsEqual(TRect.Null) then
    Exit(ARowsBounds);
  Result.Init(0, Math.Min(ARowsBounds.Top, AInnerTablesBounds.Top),
    0, Math.Max(ARowsBounds.Bottom, AInnerTablesBounds.Bottom));
end;

function TdxParagraphFinalFormatter.CalculateCellInnerTablesVerticalBounds(
  ATableViewInfoCollection: TdxTableViewInfoCollection): TRect;
var
  ACount: Integer;
  ATopAnchor, ABottomAnchor: TdxTableCellVerticalAnchor;
begin
  ACount := ATableViewInfoCollection.Count;
  if ACount <= 0 then
    Exit(cxNullRect);
  ATopAnchor := ATableViewInfoCollection[0].TopAnchor;
  ABottomAnchor := ATableViewInfoCollection[ACount - 1].BottomAnchor;
  Result.Init(0, ATopAnchor.VerticalPosition, 0, ABottomAnchor.VerticalPosition + ABottomAnchor.BottomTextIndent);
end;

function TdxParagraphFinalFormatter.CalculateCellRowsVerticalBounds(ACell: TdxTableCellViewInfo; AFirstRow,
  ALastRow: TdxRow): TRect;
begin
  Result.Init(0, AFirstRow.Bounds.Top, 0, ALastRow.Bounds.Bottom);
end;

function TdxParagraphFinalFormatter.CalculateContentOffset(AVertialAlignment: TdxVerticalAlignment; AMaxContentHeight,
  AContentHeight: Integer): Integer;
begin
  case AVertialAlignment of
    TdxVerticalAlignment.Center:
      Result := Math.Max(0, (AMaxContentHeight - AContentHeight) div 2);
    TdxVerticalAlignment.Bottom:
      Result := Math.Max(0, AMaxContentHeight - AContentHeight);
  else
    Result := 0;
  end;
end;

procedure TdxParagraphFinalFormatter.CalculateCustomMarkBounds(ARow: TdxRow);
begin
  NotImplemented;
end;

procedure TdxParagraphFinalFormatter.CalculateFieldsHighlight(ARow: TdxRow; const ATightRowBounds: TRect);
var
  AIndex, ACount: Integer;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
  AFields: TdxFieldCollection;
  AField: TdxField;
  AStart, AEnd: TdxRunIndex;
  AColor: TdxAlphaColor;
  AArea: TdxHighlightArea;
begin
  ARow.ClearFieldHighlightAreas;
  if DocumentModel.FieldOptions.HighlightMode <> TdxFieldsHighlightMode.Always then
    Exit;
  AIndex := LookupFirstFieldIndexByRunIndex(ARow, ARow.Boxes.First.StartPos.RunIndex);
  if AIndex < 0 then
    Exit;
  AStartRunIndex := ARow.Boxes.First.StartPos.RunIndex;
  AEndRunIndex := ARow.Boxes.Last.EndPos.RunIndex;
  AFields := PieceTable.Fields;
  ACount := AFields.Count;
  while AIndex < ACount do
  begin
    AIndex := AFields.LookupParentFieldIndex(AIndex);
    AField := AFields[AIndex];
    if AField.FirstRunIndex > AEndRunIndex then
      Break;
    if (AField.LastRunIndex >= AStartRunIndex) and (AField.FirstRunIndex <= AEndRunIndex) then
    begin
      AStart := Max(AStartRunIndex, AField.FirstRunIndex);
      AEnd := Min(AEndRunIndex, AField.LastRunIndex);
      AColor := DocumentModel.FieldOptions.HighlightColor;
      AArea := CreateFieldHighlightAreaCore(ARow, AStart, AEnd, ATightRowBounds, AColor);
      if not AArea.IsNull then
        ARow.FieldHighlightAreas.Add(AArea);
    end;
    Inc(AIndex);
  end;
end;

procedure TdxParagraphFinalFormatter.CalculateHiddenTextBoxes(ARow: TdxRow);
var
  AStartBox: TdxBox;
  AFontInfo: TdxFontInfo;
  ARunIndex: TdxRunIndex;
  ABoxes: TdxBoxCollection;
  ABox: TdxHiddenTextUnderlineBox;
  I, ACount, AHiddenBoxesCount, AStart, ABottomOffset, AEnd: Integer;
begin
  ARow.ClearHiddenTextBoxes;
  ABoxes := ARow.Boxes;
  ACount := ABoxes.Count;
  AHiddenBoxesCount := 0;
  for I := 0 to ACount - 1 do
  begin
    ARunIndex := ABoxes[I].StartPos.RunIndex;
    if PieceTable.Runs[ARunIndex].Hidden and (I < ACount - 1) then
    begin
      Inc(AHiddenBoxesCount);
      Continue;
    end;
    if AHiddenBoxesCount = 0 then
      Continue;
    AStartBox := ABoxes[I - AHiddenBoxesCount];
    AStart := AStartBox.Bounds.Left;
    AFontInfo := AStartBox.GetFontInfo(PieceTable);
    ABottomOffset := AFontInfo.UnderlinePosition;
    AEnd := ABoxes[I].Bounds.Left;
    ABox := TdxHiddenTextUnderlineBox.Create(AStart, AEnd, ABottomOffset);
    ARow.HiddenTextBoxes.Add(ABox);
    AHiddenBoxesCount := 0;
  end;
end;

procedure TdxParagraphFinalFormatter.CalculateRangePermissionHighlight(ARow: TdxRow; const ATightRowBounds: TRect);
var
  AStartRunIndex, AEndRunIndex, AStart, AEnd: TdxRunIndex;
  AIndex, ACount: Integer;
  ARangePermissions: TdxRangePermissionCollection;
  ARangePermission: TdxRangePermission;
  AStartPosition, AEndPosition: TdxFormatterPosition;
begin
  ARow.ClearRangePermissionHighlightAreas;
  if DocumentModel.RangePermissionOptions.Visibility = TdxRichEditRangePermissionVisibility.Hidden then
    Exit;

  AStartRunIndex := ARow.Boxes.First.StartPos.RunIndex;
  AEndRunIndex := ARow.Boxes.Last.EndPos.RunIndex;
  ARangePermissions := PieceTable.RangePermissions;
  ACount := ARangePermissions.Count;
  for AIndex := 0 to ACount - 1 do
  begin
    ARangePermission := ARangePermissions[AIndex];
    if DocumentModel.ProtectionProperties.EnforceProtection and not PieceTable.IsPermissionGranted(ARangePermission) then
      Continue;

    AStartPosition := TdxFormatterPosition.Create(ARangePermission.Interval.Start.RunIndex, ARangePermission.Interval.Start.RunOffset, 0);
    AEndPosition := TdxFormatterPosition.Create(ARangePermission.Interval.&End.RunIndex, ARangePermission.Interval.&End.RunOffset, 0);
    if (AEndPosition >= ARow.Boxes.First.StartPos) and (AStartPosition <= ARow.Boxes.Last.EndPos) then
    begin
      AStart := Max(AStartRunIndex, ARangePermission.Interval.NormalizedStart.RunIndex);
      AEnd := Min(AEndRunIndex, ARangePermission.Interval.NormalizedEnd.RunIndex);
      ARow.RangePermissionHighlightAreas.Add(CreateRangePermissionHighlightAreaCore(ARow, AStart, AEnd, ATightRowBounds, ARangePermission));
    end;
  end;
end;

function TdxParagraphFinalFormatter.CalculateRowBoxesBottom(ARow: TdxRow): Integer;
var
  I: Integer;
  ABoxes: TdxBoxCollection;
begin
  ABoxes := ARow.Boxes;
  if ABoxes.Count <= 0 then
  begin
    Result := ARow.Bounds.Bottom;
    Exit;
  end;
  Result := ARow.Bounds.Top;
  for I := 0 to ABoxes.Count - 1 do
    Result := Max(Result, ABoxes[I].Bounds.Bottom);
end;

function TdxParagraphFinalFormatter.CalculateRowBoxesTop(ARow: TdxRow): Integer;
var
  I: Integer;
  ABoxes: TdxBoxCollection;
begin
  ABoxes := ARow.Boxes;
  if ABoxes.Count <= 0 then
  begin
    Result := ARow.Bounds.Top;
    Exit;
  end;
  Result := ARow.Bounds.Bottom;
  for I := 0 to ABoxes.Count - 1 do
    Result := Min(Result, ABoxes[I].Bounds.Top);
end;

procedure TdxParagraphFinalFormatter.CalculateSpecialTextBoxes(ARow: TdxRow);
var
  ABoxes: TdxBoxCollection;
  I, ACount: Integer;
  ASpecialTextBox: TdxSpecialTextBox;
begin
  ARow.ClearSpecialTextBoxes;
  ABoxes := ARow.Boxes;
  ACount := ABoxes.Count;
  for I := 0 to ACount - 1 do
  begin
    if ABoxes[I] is TdxSpecialTextBox then
    begin
      ASpecialTextBox := TdxSpecialTextBox(ABoxes[I]);
      ARow.SpecialTextBoxes.Add(ASpecialTextBox);
    end;
  end;
end;

function TdxParagraphFinalFormatter.CalculateSubscriptOrSuperScriptOffset(ABox: TdxBox): Integer;
begin
  case TdxTextRunBase(ABox.GetRun(PieceTable)).Script of
    TdxCharacterFormattingScript.Normal:
      Result := 0;
    TdxCharacterFormattingScript.Subscript:
      Result := ABox.GetFontInfo(PieceTable).SubscriptOffset.Y;
    TdxCharacterFormattingScript.Superscript:
      Result := ABox.GetFontInfo(PieceTable).SuperscriptOffset.Y;
  else
    Result := 0;
  end;
end;

function TdxParagraphFinalFormatter.CalculateTightRowBounds(ARow: TdxRow): TRect;
var
  ABox: TdxBox;
  ABoxes: TdxBoxCollection;
  I, ACount, AMinY, AMaxY, ABaselinePosition, AAscentAndFree, ADescent: Integer;
  AFontInfo: TdxFontInfo;
begin
  ABoxes := ARow.Boxes;
  ACount := ABoxes.Count;
  if ACount <= 0 then
    Exit;
  AMinY := MaxInt;
  AMaxY := MinInt;
  ABaselinePosition := ARow.Bounds.Top + ARow.BaseLineOffset;
  for I := 0 to ACount - 1 do
  begin
    ABox := ABoxes[I];
    if ABox.IsNotWhiteSpaceBox and not ABox.IsLineBreak then
    begin
      if ABox.UseAscentAndDescentFromFontInfo then
      begin
        AFontInfo := ABox.GetFontInfo(PieceTable);
        AAscentAndFree := AFontInfo.AscentAndFree;
        ADescent := AFontInfo.Descent;
      end
      else
      begin
        AAscentAndFree := ABox.CalcAscentAndFree(PieceTable);
        ADescent := ABox.CalcDescent(PieceTable);
      end;

      AMinY := Min(AMinY, ABaselinePosition - AAscentAndFree);
      AMaxY := Max(AMaxY, ABaselinePosition + ADescent);
    end;
  end;
  if (AMinY = MaxInt) or (AMaxY = MinInt) then
    Result := ARow.Bounds
  else
    Result.InitSize(ARow.Bounds.Left, AMinY, ARow.Bounds.Width, AMaxY - AMinY);
end;

function TdxParagraphFinalFormatter.CreateFieldHighlightAreaCore(ARow: TdxRow; AStart, AEnd: TdxRunIndex;
  const ATightRowBounds: TRect; AColor: TdxAlphaColor): TdxHighlightArea;
var
  ABoxes: TdxBoxCollection;
  AFirstBoxIndex, ALastBoxIndex: Integer;
  AFirstBox: TdxBox;
  ALeft, ARight: Integer;
begin
  ABoxes := ARow.Boxes;
  AFirstBoxIndex := FindBoxIndex(ABoxes, 0, AStart);
  AFirstBox := ABoxes[AFirstBoxIndex];
  if AFirstBox.StartPos.RunIndex > AEnd then
    Exit(TdxHighlightArea.Null);
  ALastBoxIndex := FindBoxIndex(ABoxes, AFirstBoxIndex + 1, AEnd + 1) - 1;
  ALeft := AFirstBox.Bounds.Left;
  ARight := ABoxes[ALastBoxIndex].Bounds.Right;
  Result := TdxHighlightArea.Create(AColor, TRect.CreateSize(ALeft, ATightRowBounds.Top, ARight - ALeft, ATightRowBounds.Height));
end;

function TdxParagraphFinalFormatter.CreateRangePermissionHighlightAreaCore(ARow: TdxRow; AStart, AEnd: TdxRunIndex;
  const ATightRowBounds: TRect; ARangePermission: TdxRangePermission): TdxHighlightArea;
var
  ARangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection;
  ABoxes: TdxBoxCollection;
  AFirstBoxIndex, ALastBoxIndex, ALeft, ARight, ARangePermissionBoxesCount, I: Integer;
  AStartBox, AEndBox, AItemBox: TdxVisitableDocumentIntervalBox;
  AColor: TdxAlphaColor;
begin
  ARangePermissionBoxes := ARow.InnerRangePermissionBoxes;
  ABoxes := ARow.Boxes;

  AFirstBoxIndex := FindBoxIndex(ABoxes, 0, AStart);
  ALastBoxIndex := FindBoxIndex(ABoxes, AFirstBoxIndex + 1, AEnd + 1) - 1;
  ALeft := ABoxes[AFirstBoxIndex].Bounds.Left;
  ARight := ABoxes[ALastBoxIndex].Bounds.Right;

  if ARangePermissionBoxes = nil then
    ARangePermissionBoxesCount := 0
  else
    ARangePermissionBoxesCount := ARangePermissionBoxes.Count;
  if ARangePermissionBoxesCount > 0 then
  begin
    AStartBox := nil;
    AEndBox := nil;
    for I := 0 to ARangePermissionBoxesCount - 1 do
    begin
      AItemBox := ARangePermissionBoxes[I];
      if AItemBox.Interval = ARangePermission then
      begin
        if AItemBox is TdxBookmarkStartBox then
          AStartBox := AItemBox
        else
          AEndBox := AItemBox;
      end;
    end;
    if AStartBox <> nil then
      ALeft := AStartBox.HorizontalPosition;
    if AEndBox <> nil then
      ARight := AEndBox.HorizontalPosition;
  end;
  AColor := DocumentModel.RangePermissionOptions.HighlightColor;
  if not DocumentModel.ProtectionProperties.EnforceProtection then
    AColor := RangePermissionColorer.GetColor(ARangePermission);
  Result := TdxHighlightArea.Create(AColor, TRect.CreateSize(ALeft, ATightRowBounds.Top, ARight - ALeft, ATightRowBounds.Height));
end;

procedure TdxParagraphFinalFormatter.Format(ARows: TdxRowCollection);
var
  I: Integer;
begin
  for I := 0 to ARows.Count - 1 do
    Format(ARows[I]);
end;

function TdxParagraphFinalFormatter.FindBoxIndex(ABoxes: TdxBoxCollection; AFrom: Integer;
  AIndexToSearch: TdxRunIndex): Integer;
var
  I, ACount: Integer;
begin
  ACount := ABoxes.Count;
  for I := AFrom to ABoxes.Count - 1 do
    if ABoxes[I].StartPos.RunIndex >= AIndexToSearch then
    begin
      Result := I;
      Exit;
    end;
  Result := ACount;
end;

function TdxParagraphFinalFormatter.FindRowByParagraphIndex(ARows: TdxRowCollection;
  AParagraphIndex: TdxParagraphIndex): Integer;
var
  I: Integer;
  AComparer: TdxRowParagraphComparer;
begin
  AComparer := TdxRowParagraphComparer.Create(AParagraphIndex);
  try
    if not TdxAlgorithms1<TdxBoxBase>.BinarySearch(FCurrentColumn.Rows, AComparer, Result) then
      Exit(-Result);
  finally
    AComparer.Free;
  end;

  for I := Result - 1 downto 0 do
  begin
    if ARows[I].Paragraph.Index <> AParagraphIndex then
      Break;
    Result := I;
  end;
end;

procedure TdxParagraphFinalFormatter.Format(ARow: TdxRow);
begin
  if ARow.Paragraph <> FCurrentParagraph then
    UpdateCurrentParagraph(ARow.Paragraph);
  if ARow.ShouldProcessLayoutDependentText then
    UpdateLayoutDependentBox(ARow);
  FormatCore(ARow);
end;

procedure TdxParagraphFinalFormatter.FormatColumn(AColumn: TdxColumn);
begin
  FCurrentParagraph := nil;
  FCurrentColumn := AColumn;
  Format(AColumn.Rows);
  if AColumn.InnerTables <> nil then
    ApplyTableCellsVerticalContentAlignment(AColumn.InnerTables);
  if AColumn.ShouldProcessParagraphFrames then
    FormatParagraphFrames(AColumn.InnerParagraphFrames);
  FCurrentColumn := nil;
end;

procedure TdxParagraphFinalFormatter.FormatPage(APage: TdxPage);
var
  I, ACount: Integer;
  AMainPieceTable: TdxPieceTable;
begin
  AMainPieceTable := PieceTable;
  if APage.Header <> nil then
  begin
    PieceTable := TdxPieceTable(APage.Header.Header.PieceTable);
    FormatPageArea(APage.Header);
  end;
  if APage.Footer <> nil then
  begin
    PieceTable := TdxPieceTable(APage.Footer.Footer.PieceTable);
    FormatPageArea(APage.Footer);
  end;

  PieceTable := AMainPieceTable;

  ACount := APage.Areas.Count;
  for I := 0 to ACount - 1 do
    FormatPageArea(APage.Areas[I]);
  FormateTextBoxes(APage.InnerBackgroundFloatingObjects);
  FormateTextBoxes(APage.InnerFloatingObjects);
  FormateTextBoxes(APage.InnerForegroundFloatingObjects);

end;

procedure TdxParagraphFinalFormatter.FormatPageArea(APageArea: TdxPageArea);
var
  I: Integer;
begin
  for I := 0 to APageArea.Columns.Count - 1 do
    FormatColumn(APageArea.Columns[I]);
  for I := 0 to APageArea.LineNumbers.Count - 1 do
    AlignLineNumberToBaseLine(TdxLineNumberBox(APageArea.LineNumbers[I]));
end;

procedure TdxParagraphFinalFormatter.FormatPages(APages: TdxPageCollection);
var
  I: Integer;
begin
  for I := 0 to APages.Count - 1 do
    FormatPage(APages[I]);
end;

procedure TdxParagraphFinalFormatter.FormatParagraphFrame(AParagraphFrameBox,
  ANextParagraphFrameBox: TdxParagraphFrameBox);
var
  ARows: TdxRowCollection;
  AParagraphIndex: TdxParagraphIndex;
  AFirstRowIndex: Integer;
  ATop, ABottom: Integer;
  ATableCellRow: TdxTableCellRow;
  ARow: TdxRow;
  ACurrentColumnBounds: TRect;
  AParagraph: TdxSimpleParagraph;
  ALeftIndent: Integer;
  P: TPoint;
begin
  ARows := FCurrentColumn.Rows;
  AParagraphIndex := AParagraphFrameBox.ParagraphIndex;
  AFirstRowIndex := FindRowByParagraphIndex(ARows, AParagraphIndex);
  if AFirstRowIndex < 0 then
    Exit;

  AParagraphFrameBox.FirstRow := ARows[AFirstRowIndex];
  ATop := CalculateRowBoxesTop(AParagraphFrameBox.FirstRow);

  ATableCellRow := Safe<TdxTableCellRow>.Cast(AParagraphFrameBox.FirstRow);
  ARow := CalculateBottomRow(ARows, AParagraphIndex + 1, ATableCellRow);
  ABottom := CalculateBottomPosition(AParagraphFrameBox, ANextParagraphFrameBox, ARow, ATableCellRow);

  if ATableCellRow = nil then
    ACurrentColumnBounds := FCurrentColumn.Bounds
  else
    ACurrentColumnBounds := ATableCellRow.CellViewInfo.GetBounds;

  AParagraph := AParagraphFrameBox.GetParagraph;
  ALeftIndent := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(AParagraph.LeftIndent);
  if (AParagraph.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging) and (AParagraph.FirstLineIndent > 0) then
    ALeftIndent := ALeftIndent - DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(AParagraph.FirstLineIndent);
  P := ACurrentColumnBounds.Location;
  P.X := P.X + ALeftIndent;
  ACurrentColumnBounds.Location := P;
  ACurrentColumnBounds.Width := ACurrentColumnBounds.Width - ALeftIndent;
  ACurrentColumnBounds.Width := ACurrentColumnBounds.Width - DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(AParagraph.RightIndent);
  ACurrentColumnBounds.Width := Max(0, ACurrentColumnBounds.Width);
  AParagraphFrameBox.Bounds := TRect.CreateSize(ACurrentColumnBounds.Left, ATop,
    ACurrentColumnBounds.Width, ABottom - ATop);
  if AParagraphFrameBox.ContentBounds.IsEmpty then
      AParagraphFrameBox.ContentBounds := AParagraphFrameBox.Bounds;
end;

procedure TdxParagraphFinalFormatter.FormatParagraphFrames(AParagraphFrames: TdxParagraphFrameBoxCollection);
var
  ANextBox: TdxParagraphFrameBox;
  I, ACount: Integer;
begin
  ANextBox := nil;
  ACount := AParagraphFrames.Count;
  for I := ACount - 1 downto 0 do
  begin
    FormatParagraphFrame(AParagraphFrames[I], ANextBox);
    ANextBox := AParagraphFrames[I];
  end;
end;

function TdxParagraphFinalFormatter.LookupFirstFieldIndexByRunIndex(ARow: TdxRow; ARunIndex: TdxRunIndex): Integer;
var
  AFields: TdxFieldCollection;
  AComparator: TdxFieldRunIndexComparable;
begin
  AFields := PieceTable.Fields;
  AComparator := TdxFieldRunIndexComparable.Create(ARunIndex);
  try
    TdxAlgorithms1<TdxField>.BinarySearch(AFields.InnerList, AComparator, Result);
    if Result >= AFields.Count then
      Result := -1;
  finally
    AComparator.Free;
  end;
end;

function TdxParagraphFinalFormatter.CreateCommentHighlightAreaCore(ARow: TdxRow; AStart: TdxRunIndex; AEnd: TdxRunIndex; const ATightRowBounds: TRect; AComment: TdxComment): TdxHighlightArea;
begin
  NotImplemented;
  Result := TdxHighlightArea.Null;
end;

procedure TdxParagraphFinalFormatter.CalculateCommentHighlight(ARow: TdxRow; const ATightRowBounds: TRect);
begin
  NotImplemented;
end;

procedure TdxParagraphFinalFormatter.FormateTextBoxes(AList: TdxFloatingObjectBoxList);
var
  I: Integer;
  ABox: TdxFloatingObjectBox;
begin
  if (AList = nil) or (AList.Count = 0) then
    Exit;
  for I := 0 to AList.Count - 1 do
  begin
    ABox := AList[I];
    FormatTextBox(ABox);
  end;
end;

procedure TdxParagraphFinalFormatter.FormatTextBox(ABox: TdxFloatingObjectBox);
var
  ADocumentLayout: TdxDocumentLayout;
  APages: TdxPageCollection;
  AAreas: TdxPageAreaCollection;
  AColumns: TdxColumnCollection;
  ARows: TdxRowCollection;
  ACount, I: Integer;
  ARow: TdxRow;
  ARun: TdxFloatingObjectAnchorRun;
  AContentType: TdxTextBoxFloatingObjectContent;
  AOldPieceTable: TdxPieceTable;
begin
  ADocumentLayout := ABox.DocumentLayout;
  if ADocumentLayout = nil then
    Exit;
  APages := ADocumentLayout.Pages;
  if APages.Count = 0 then
    Exit;
  AAreas := APages[0].Areas;
  if AAreas.Count = 0 then
    Exit;
  AColumns := AAreas[0].Columns;
  if AColumns.Count = 0 then
    Exit;
  ARows := AColumns[0].Rows;
  ACount := ARows.Count;
  for I := 0 to ACount - 1 do
  begin
    ARow := ARows[I];
    if ARow.ShouldProcessLayoutDependentText then
    begin
      if ARow.Paragraph <> FCurrentParagraph then
        UpdateCurrentParagraph(ARow.Paragraph);
      ARun := ABox.GetFloatingObjectRun;
      AContentType := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
      if AContentType <> nil then
      begin
        AOldPieceTable := FPieceTable;
        FPieceTable := TdxPieceTable(AContentType.TextBox.PieceTable);
        try
          UpdateLayoutDependentBox(ARow);
          FormatCore(ARow);
        finally
          FPieceTable := AOldPieceTable;
        end;
      end;
    end;
  end;
end;

{ TdxParagraphFinalFormatter }

procedure TdxParagraphFinalFormatter.FormatCore(ARow: TdxRow);
var
  ATightRowBounds: TRect;
begin
  FLineSpacingCalculator.FormatRow(Self, ARow);
  FAlignmentCalculator.AlignRow(ARow);
  if ARow.ShouldProcessCharacterLines then
  begin
    FUnderlineCalculator.Calculate(ARow);
    FStrikeoutCalculator.Calculate(ARow);
  end;
  ATightRowBounds := CalculateTightRowBounds(ARow);
  if ARow.ShouldProcessTextHighlight then
    CalculateBackgroundHighlight(ARow, ATightRowBounds);
  CalculateFieldsHighlight(ARow, ATightRowBounds);
  if ARow.ShouldProcessHiddenText then
    CalculateHiddenTextBoxes(ARow);
  if ARow.ShouldProcessSpecialTextBoxes then
    CalculateSpecialTextBoxes(ARow);
  FBookmarkCalculator.Calculate(ARow);
  FRangePermissionCalculator.Calculate(ARow);
  CalculateRangePermissionHighlight(ARow, ATightRowBounds);
end;




procedure TdxParagraphFinalFormatter.SubstituteFont(ABox: TdxLayoutDependentTextBox);
var
  ACh: Char;
  AFontCache: TdxFontCache;
  ASourceFontInfo: TdxFontInfo;
  AFontCharacterSet: TdxFontCharacterSet;
  ASourceFontName, ASubstFontName: string;
  ASourceFontCharacterSet: TdxFontCharacterSet;
begin
  AFontCache := DocumentModel.FontCache;
  ASourceFontInfo := ABox.GetFontInfo(PieceTable);
  ASourceFontName := ASourceFontInfo.Name;
  ASourceFontCharacterSet := AFontCache.GetFontCharacterSet(ASourceFontName);
  if ASourceFontCharacterSet = nil then
    Exit;
  ACh := ABox.CalculatedText[1];
  if not ASourceFontCharacterSet.ContainsChar(ACh) then
  begin
    ASubstFontName := AFontCache.FindSubstituteFont(ASourceFontName, ACh, AFontCharacterSet);
    TdxTextRunBase(ABox.GetRun(PieceTable)).FontName := ASubstFontName;
  end;
end;

procedure TdxParagraphFinalFormatter.SetPieceTable(const Value: TdxPieceTable);
begin
  Assert(FPieceTable <> nil);
  if FPieceTable = Value then
    Exit;
  FPieceTable := Value;
  FUnderlineCalculator.PieceTable := Value;
  FStrikeoutCalculator.PieceTable := FPieceTable;
  FBookmarkCalculator.PieceTable := FPieceTable;
  FRangePermissionCalculator.PieceTable := FPieceTable;
end;

function TdxParagraphFinalFormatter.ShouldMergeParagraphFrameBoxes(AParagraphFrameBox,
  ANextParagraphFrameBox: TdxParagraphFrameBox; ACellRow: TdxTableCellRow): Boolean;
var
  AParagraphIndex: TdxParagraphIndex;
  AParagraph, ANextParagraph: TdxSimpleParagraph;
begin
  Result := False;
  if ANextParagraphFrameBox = nil then
    Exit;
  AParagraphIndex := AParagraphFrameBox.ParagraphIndex;
  if ANextParagraphFrameBox.ParagraphIndex - 1 <> AParagraphIndex then
    Exit;
  if (ACellRow <> nil) and (ACellRow.CellViewInfo.Cell.EndParagraphIndex = AParagraphIndex) then
    Exit;
  ANextParagraph := ANextParagraphFrameBox.GetParagraph;
  AParagraph := AParagraphFrameBox.GetParagraph;
  Result := (ANextParagraph.LeftIndent = AParagraph.LeftIndent) and
    (ANextParagraph.RightIndent = AParagraph.RightIndent);
end;

procedure TdxParagraphFinalFormatter.UpdateCurrentParagraph(AParagraph: TdxSimpleParagraph);
begin
  FreeAndNil(FLineSpacingCalculator);
  FCurrentParagraph := AParagraph;
  FLineSpacingCalculator := TdxLineSpacingCalculatorBase.GetLineSpacingCalculator(FCurrentParagraph);
  FAlignmentCalculator := TdxParagraphAlignmentCalculatorBase.GetAlignmentCalculator(AParagraph.Alignment);
end;

procedure TdxParagraphFinalFormatter.UpdateLayoutDependentBox(ARow: TdxRow);
var
  ABoxes: TdxBoxCollection;
  I, ACount: Integer;
  AMeasurer: TdxBoxMeasurer;
  ABox: TdxLayoutDependentTextBox;
  ARun: TdxLayoutDependentTextRun;
  AOldPieceTable: TdxCustomPieceTable;
  ABoxInfo: TdxBoxInfo;
  ABounds: TRect;
begin
  ABoxes := ARow.Boxes;
  ACount := ABoxes.Count;
  AMeasurer := FDocumentLayout.Measurer;
  for I := 0 to ACount - 1 do
  begin
    ABox := Safe<TdxLayoutDependentTextBox>.Cast(ABoxes[I]);
    if ABox = nil then
      Continue;

    ABox := TdxLayoutDependentTextBox(ABoxes[I]);
    ARun := TdxLayoutDependentTextRun(PieceTable.Runs[ABox.StartPos.RunIndex]);
    if not ARun.FieldResultFormatting.RecalculateOnSecondaryFormatting then
      Continue;
    AOldPieceTable := AMeasurer.PieceTable;
    try
      AMeasurer.PieceTable := PieceTable;
      ABox.CalculatedText := ARun.FieldResultFormatting.GetValue(nil, DocumentModel);
      SubstituteFont(ABox);
      ABoxInfo := TdxBoxInfo.Create;
      try
        ABoxInfo.Box := ABox;
        ABoxInfo.StartPos := ABox.StartPos;
        ABoxInfo.EndPos := ABox.EndPos;

        AMeasurer.MeasureText(ABoxInfo);
        ABounds := ABox.Bounds;
        ABounds.Size := ABoxInfo.Size;
        ABox.Bounds := ABounds;
      finally
        ABoxInfo.Free;
      end;
    finally
      AMeasurer.PieceTable := AOldPieceTable;
    end;
  end;
end;

{ TdxStateRowEmptyHyphenation }

procedure TdxStateRowEmptyHyphenation.AddSuccess(ABoxInfo: TdxBoxInfo);
begin
  Formatter.ApproveFloatingObjects;
  AddTextBox(ABoxInfo);
  if Iterator.CurrentChar = TdxCharacters.Hyphen then
  begin
    Iterator.Next;
    ChangeState(NextState);
  end;
end;

function TdxStateRowEmptyHyphenation.CalcFinalState(ACurrentChar: Char): TdxParagraphBoxFormatterState;
begin
  if FSwitchToRowEmpty then
    Exit(TdxParagraphBoxFormatterState.RowEmpty);
  if TdxCharacters.IsCharSpace(ACurrentChar) then
    Exit(TdxParagraphBoxFormatterState.RowSpaces);
  if ACurrentChar = TdxCharacters.TabMark then
    Exit(TdxParagraphBoxFormatterState.RowTab);
  if ACurrentChar = TdxCharacters.LineBreak then
    Exit(TdxParagraphBoxFormatterState.RowLineBreak);
  if ACurrentChar = TdxCharacters.PageBreak then
    if RowsController.TablesController.IsInsideTable then
      Exit(TdxParagraphBoxFormatterState.RowSpaces)
    else
      if not RowsController.SupportsColumnAndPageBreaks then
        Exit(TdxParagraphBoxFormatterState.RowLineBreak)
      else
        Exit(TdxParagraphBoxFormatterState.RowPageBreak);
  if ACurrentChar = TdxCharacters.ColumnBreak then
  begin
    if RowsController.TablesController.IsInsideTable then
      Exit(TdxParagraphBoxFormatterState.RowSpaces)
    else
      if not RowsController.SupportsColumnAndPageBreaks then
        Exit(TdxParagraphBoxFormatterState.RowLineBreak)
      else
        Exit(TdxParagraphBoxFormatterState.RowColumnBreak);
  end;
  TdxRichEditExceptions.ThrowInternalException;
  Result := TdxParagraphBoxFormatterState.Final;
end;

function TdxStateRowEmptyHyphenation.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowEmptyHyphenationAfterFloatingObject;
end;

function TdxStateRowEmptyHyphenation.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := CanAddBoxWithHyphenCore(ABoxInfo);
end;

function TdxStateRowEmptyHyphenation.CanAddBoxWithoutHyphen(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := CanAddBoxCore(ABoxInfo);
end;

function TdxStateRowEmptyHyphenation.ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := Formatter.RollbackToStartOfRowCore(ACanFit, TdxParagraphBoxFormatterState.RowEmptyHyphenation);
end;

function TdxStateRowEmptyHyphenation.FinishParagraph: TdxCompleteFormattingResult;
var
  ACurrentChar: Char;
begin
  try
    if Formatter.Iterator.IsEnd then
    begin
      Assert(CurrentRow.Height > 0);
      if Formatter.Iterator.CurrentChar = TdxCharacters.ParagraphMark then
        Exit(inherited FinishParagraph)
      else
        Exit(inherited FinishSection);
    end
    else
    begin
      ACurrentChar := Formatter.Iterator.CurrentChar;
      ChangeState(CalcFinalState(ACurrentChar));
    end;
  finally
    Formatter.ClearSyllableIterator;
  end;
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowEmptyHyphenation.GetIterator: TdxParagraphIteratorBase;
begin
  Result := Formatter.SyllableIterator;
end;

function TdxStateRowEmptyHyphenation.GetNextState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextHyphenation;
end;

function TdxStateRowEmptyHyphenation.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowEmptyHyphenation;
end;

function TdxStateRowEmptyHyphenation.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ASplitBoxResult: TdxSplitBoxResult;
begin
  Result := TdxCompleteFormattingResult.Success;
  ASplitBoxResult := SplitBox(ABoxInfo);
  if (ASplitBoxResult <> TdxSplitBoxResult.FailedHorizontalOverfull) then
  begin
    Formatter.ApproveFloatingObjects;
    AddTextBox(ABoxInfo);
  end;

  if not IsHyphenationOfWordComplete then
  begin
    EndRow;
    ChangeState(TdxParagraphBoxFormatterState.RowEmptyHyphenation);
    Exit;
  end;

  if Formatter.Iterator.IsEnd then
    Exit;
  if Iterator.CurrentChar = TdxCharacters.PageBreak then
    Exit;
  if Iterator.CurrentChar = TdxCharacters.ColumnBreak then
    Exit;
  if (Iterator.CurrentChar <> TdxCharacters.LineBreak) or (ASplitBoxResult = TdxSplitBoxResult.SuccessSuppressedHorizontalOverfull) then
  begin
    EndRow;
    FSwitchToRowEmpty := True;
  end;
end;

function TdxStateRowEmptyHyphenation.IsHyphenationOfWordComplete: Boolean;
begin
  Result := Iterator.IsEnd;
end;

function TdxStateRowEmptyHyphenation.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := ACh = TdxCharacters.Hyphen;
end;

{ TdxStateRowEmptyHyphenationAfterFloatingObject }

function TdxStateRowEmptyHyphenationAfterFloatingObject.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowEmptyHyphenationAfterFloatingObject;
end;

{ TdxStateRowEmptyAfterFloatingObject }

function TdxStateRowEmptyAfterFloatingObject.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowEmptyAfterFloatingObject;
end;

{ TdxStateRowTextHyphenation }

function TdxStateRowTextHyphenation.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextHyphenation;
end;

function TdxStateRowTextHyphenation.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextHyphenationAfterFloatingObject;
end;

function TdxStateRowTextHyphenation.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Formatter.RollbackToStartOfWord;
  InsertHyphenBox;
  EndRow;
  ChangeState(NextState);
  Result := TdxCompleteFormattingResult.Success;
end;

procedure TdxStateRowTextHyphenation.InsertHyphenBox;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateHyphenBoxInfo;
  try
    RowsController.AddBox(TdxHyphenBox, ABoxInfo);
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

procedure TdxStateRowTextHyphenation.AddSuccess(ABoxInfo: TdxBoxInfo);
begin
  Formatter.ApproveFloatingObjects;
  AddTextBox(ABoxInfo);
  if Iterator.CurrentChar = TdxCharacters.Hyphen then
  begin
    Iterator.Next;
    Formatter.StartNewWord;
  end;
end;

function TdxStateRowTextHyphenation.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := CanAddBoxWithHyphenCore(ABoxInfo);
end;

function TdxStateRowTextHyphenation.CanAddBoxWithoutHyphen(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := CanAddBoxCore(ABoxInfo);
end;

function TdxStateRowTextHyphenation.CreateHyphenBoxInfo: TdxBoxInfo;
begin
  Result := TdxBoxInfo.Create;
  Result.StartPos := Iterator.GetPreviousPosition;
  Result.EndPos := Result.StartPos;
  Result.Size := Formatter.Measurer.MeasureHyphen(Result.StartPos, Result);
end;

function TdxStateRowTextHyphenation.GetNextState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowEmptyHyphenation;
end;

{ TdxStateRowWithTextOnlyBase }

procedure TdxStateRowWithTextOnlyBase.AddSuccess(ABoxInfo: TdxBoxInfo);
begin
  Formatter.ApproveFloatingObjects;
  AddTextBox(ABoxInfo);
  ChangeStateManuallyIfNeeded(ABoxInfo.IteratorResult);
end;

procedure TdxStateRowWithTextOnlyBase.ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  if CurrentRow.Boxes.Count <= 0 then
    RowsController.UpdateCurrentRowHeight(ABoxInfo)
  else
    inherited ApplyColumnBreakMarkSize(ABoxInfo);
end;

procedure TdxStateRowWithTextOnlyBase.ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  if CurrentRow.Boxes.Count <= 0 then
    RowsController.UpdateCurrentRowHeight(ABoxInfo)
  else
    inherited ApplyLineBreakMarkSize(ABoxInfo);
end;

procedure TdxStateRowWithTextOnlyBase.ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  if CurrentRow.Boxes.Count <= 0 then
    RowsController.UpdateCurrentRowHeight(ABoxInfo)
  else
    inherited ApplyPageBreakMarkSize(ABoxInfo);
end;

procedure TdxStateRowWithTextOnlyBase.ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo);
begin
  if CurrentRow.Boxes.Count <= 0 then
    RowsController.UpdateCurrentRowHeight(ABoxInfo)
  else
    inherited ApplyParagraphMarkSize(ABoxInfo);
end;

procedure TdxStateRowWithTextOnlyBase.ChangeStateManuallyIfNeeded(AIteratorResult: TdxParagraphIteratorResult);
var
  ACurrentChar: Char;
  ANextState: TdxParagraphBoxFormatterState;
begin
  ACurrentChar := Iterator.CurrentChar;
  if ShouldChangeStateManually(ACurrentChar) then
  begin
    ANextState := CalcNextState(ACurrentChar);
    ChangeState(ANextState);
  end;
end;

function TdxStateRowWithTextOnlyBase.FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert((CurrentRow.Boxes.Count <= 0) or (CurrentRow.Height > 0));
  Result := RowsController.CompleteCurrentColumnFormatting;
  if Result <> TdxCompleteFormattingResult.Success then
    Exit;
  if CurrentRow.Boxes.Count <= 0 then
    RowsController.UpdateCurrentRowHeight(ABoxInfo);
  Formatter.RowsController.AddBox(TdxColumnBreakBox, ABoxInfo);
  Formatter.ApproveFloatingObjects;
  EndRow;
  RowsController.MoveRowToNextColumn;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
end;

function TdxStateRowWithTextOnlyBase.FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert((CurrentRow.Boxes.Count <= 0) or (CurrentRow.Height > 0));

  if CurrentRow.Boxes.Count <= 0 then
    RowsController.UpdateCurrentRowHeight(ABoxInfo);
  Formatter.RowsController.AddBox(TdxLineBreakBox, ABoxInfo);
  Formatter.ApproveFloatingObjects;
  EndRow;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowWithTextOnlyBase.FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert((CurrentRow.Boxes.Count <= 0) or (CurrentRow.Height > 0));

  Result := RowsController.CompleteCurrentColumnFormatting;
  if Result <> TdxCompleteFormattingResult.Success then
    Exit;
  if CurrentRow.Boxes.Count <= 0 then
    RowsController.UpdateCurrentRowHeight(ABoxInfo);
  Formatter.RowsController.AddBox(TdxPageBreakBox, ABoxInfo);
  Formatter.ApproveFloatingObjects;
  EndRow;
  RowsController.MoveRowToNextPage;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
end;

function TdxStateRowWithTextOnlyBase.FinishParagraph: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  Assert((CurrentRow.Boxes.Count <= 0) or (CurrentRow.Height > 0));
  ABoxInfo := CreateParagraphMarkBoxInfo;
  try
    ApplyParagraphMarkSize(ABoxInfo);
    FinalizeParagraph(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxStateRowWithTextOnlyBase.FinishSection: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  Assert((CurrentRow.Boxes.Count <= 0) or (CurrentRow.Height > 0));
  ABoxInfo := CreateSectionMarkBoxInfo;
  try
    ApplyParagraphMarkSize(ABoxInfo);
    FinalizeSection(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxStateRowWithTextOnlyBase.CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState;
begin
  if TdxCharacters.IsCharSpace(ACurrentCharacter) then
    Exit(TdxParagraphBoxFormatterState.RowSpaces);
  if ACurrentCharacter = TdxCharacters.TabMark then
    Exit(TdxParagraphBoxFormatterState.RowTab);
  if TdxCharacters.IsCharDash(ACurrentCharacter) then
    Exit(DashAfterTextState);
  if ACurrentCharacter = TdxCharacters.LineBreak then
    Exit(TdxParagraphBoxFormatterState.RowLineBreak);
  if ACurrentCharacter = TdxCharacters.PageBreak then
  begin
    if RowsController.TablesController.IsInsideTable then
      Exit(TdxParagraphBoxFormatterState.RowSpaces)
    else
      if not RowsController.SupportsColumnAndPageBreaks then
        Exit(TdxParagraphBoxFormatterState.RowLineBreak)
      else
        Exit(TdxParagraphBoxFormatterState.RowPageBreak);
  end;
  if ACurrentCharacter = TdxCharacters.ColumnBreak then
  begin
    if RowsController.TablesController.IsInsideTable then
      Exit(TdxParagraphBoxFormatterState.RowSpaces)
    else
      if not RowsController.SupportsColumnAndPageBreaks then
        Exit(TdxParagraphBoxFormatterState.RowLineBreak)
      else
        Exit(TdxParagraphBoxFormatterState.RowColumnBreak);
  end;
  if ACurrentCharacter = TdxCharacters.FloatingObjectMark then
  begin
    if Iterator.IsFloatingObjectAnchorRun() then
      Exit(TdxParagraphBoxFormatterState.FloatingObject)
    else
      Exit(TdxParagraphBoxFormatterState.RowSpaces);
  end;

  TdxRichEditExceptions.ThrowInternalException;
  Result := TdxParagraphBoxFormatterState.Final;
end;

function TdxStateRowWithTextOnlyBase.CanUseBox: Boolean;
begin
  Result := True;
end;

function TdxStateRowWithTextOnlyBase.GetDashAfterTextState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowDashAfterText;
end;

function TdxStateRowWithTextOnlyBase.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := TdxCharacters.IsCharSpace(ACh) or (ACh = TdxCharacters.TabMark) or (ACh = TdxCharacters.LineBreak) or
    (ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak) or (ACh = TdxCharacters.FloatingObjectMark) or
    TdxCharacters.IsCharDash(ACh);
end;

function TdxStateRowWithTextOnlyBase.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFloatingObject;
end;

function TdxStateRowWithTextOnlyBase.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Formatter.RollbackToStartOfWord;
  if Formatter.SuppressHyphenation then
    ChangeState(NoHyphenationNextState)
  else
    ChangeState(HyphenationState);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowWithTextOnlyBase.ShouldChangeStateManually(ACurrentCharacter: Char): Boolean;
begin
  Result := not Iterator.IsEnd and IsTerminatorChar(ACurrentCharacter);
end;

{ TdxStateRowWithTextOnly }

function TdxStateRowWithTextOnly.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := CanAddBoxCore(ABoxInfo);
end;

function TdxStateRowWithTextOnly.GetDashAfterTextState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithDashAfterTextOnly;
end;

function TdxStateRowWithTextOnly.ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowWithTextOnly.GetHyphenationState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowEmptyHyphenation;
end;

function TdxStateRowWithTextOnly.GetNoHyphenationNextState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextSplit;
end;

function TdxStateRowWithTextOnly.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithTextOnly;
end;

function TdxStateRowWithTextOnly.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFloatingObject;
end;

{ TdxStateRowWithInlineObjectOnly }

function TdxStateRowWithInlineObjectOnly.CalcNextState(ACurrentCharacter: Char): TdxParagraphBoxFormatterState;
begin
  if inherited IsTerminatorChar(ACurrentCharacter) then
    Result := inherited CalcNextState(ACurrentCharacter)
  else
    Result := TdxParagraphBoxFormatterState.RowText;
end;

function TdxStateRowWithInlineObjectOnly.CanUseBox: Boolean;
begin
  Result := True;
end;

function TdxStateRowWithInlineObjectOnly.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithInlineObjectOnly;
end;

function TdxStateRowWithInlineObjectOnly.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  AddTextBox(ABoxInfo);
  Formatter.ApproveFloatingObjects;
  if not Iterator.IsEnd then
  begin
    EndRow;
    ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
  end;
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowWithInlineObjectOnly.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := True;
end;

{ TdxCharacterLineCalculator }

constructor TdxCharacterLineCalculator<T>.Create(APieceTable: TdxPieceTable);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  FPieceTable := APieceTable;
  FComparer := TEqualityComparer<T>.Default;
end;

destructor TdxCharacterLineCalculator<T>.Destroy;
begin
  Boxes := nil;
  inherited Destroy;
end;

procedure TdxCharacterLineCalculator<T>.Calculate(ARow: TdxRow);
var
  ALastNonSpaceBoxIndex: Integer;
begin
  BeforeCalculate(ARow);
  if BoxCount = 0 then
    Exit;
  ALastNonSpaceBoxIndex := FindLastNonSpaceBoxIndex;
  if ALastNonSpaceBoxIndex < 0 then
    Exit;
  CalculateCharacterLineBoxesByType(ALastNonSpaceBoxIndex);
  AfterCalculate;
end;

function TdxCharacterLineCalculator<T>.FindLastNonSpaceBoxIndex(AStartIndex: Integer = 0): Integer;
begin
  Result := Row.FindLastNonSpaceBoxIndex(Boxes, AStartIndex);
end;

procedure TdxCharacterLineCalculator<T>.AddBoxToCharacterLine(ABox: TdxBox);
begin
end;

procedure TdxCharacterLineCalculator<T>.AfterCalculate;
begin
end;

procedure TdxCharacterLineCalculator<T>.BeforeCalculate(ARow: TdxRow);
begin
  Initialize(ARow);
end;

function TdxCharacterLineCalculator<T>.CalculateActualCharacterLineType(ABox: TdxBox): T;
begin
  if FUseForWordsOnly then
  begin
    if Supports(ABox, IdxSpaceBox) then
      Result := CharacterLineTypeNone
    else
      Result := FCurrentRunCharacterLineType
  end
  else
    Result := FCurrentRunCharacterLineType;
end;

function TdxCharacterLineCalculator<T>.CalculateActualScript(AScript: TdxCharacterFormattingScript): TdxCharacterFormattingScript;
begin
  Result := AScript;
end;

procedure TdxCharacterLineCalculator<T>.CalculateCharacterLineBoxesByType(ALastNonSpaceBoxIndex: Integer);
var
  ACurrentFontInfo: TdxFontInfo;
  APrevBoxIndex: TdxRunIndex;
  ACurrentCharacterLineType, ABoxCharacterLineType: T;
  ABoxScript, AActualScript: TdxCharacterFormattingScript;
  I: Integer;
  ABox: TdxBox;
  ARunChanged, ACharacterLineTypeChanged: Boolean;
  ACurrentTextRun: TdxTextRun;
begin
  ACurrentFontInfo := nil;
  APrevBoxIndex := -1;
  FCurrentRunCharacterLineType := CharacterLineTypeNone;
  FCurrentScript := UnknownScript;
  ACurrentCharacterLineType := CharacterLineTypeNone;
  ABoxScript := TdxCharacterFormattingScript.Normal;

  for I := 0 to ALastNonSpaceBoxIndex do
  begin
    ABox := FBoxes[I];
    if I >= FNumberingListBoxesCount then
    begin
      ARunChanged := SetCurrentRunCharacterLineTypeFromBox(ABox, APrevBoxIndex);
      APrevBoxIndex := ABox.StartPos.RunIndex;
    end
    else
      ARunChanged := SetCurrentRunCharacterLineTypeFromNumberingListBox;

    ABoxCharacterLineType := CalculateActualCharacterLineType(ABox);
    ACharacterLineTypeChanged := not Comparer.Equals(ACurrentCharacterLineType, ABoxCharacterLineType);
    if ARunChanged then
      ABoxScript := GetBoxScript(I);

    AActualScript := CalculateActualScript(ABoxScript);
    if IsCharacterLineBreak(ACharacterLineTypeChanged, ARunChanged, ABoxScript) then
    begin
      if not Comparer.Equals(ACurrentCharacterLineType, CharacterLineTypeNone) then
        EndCharacterLine(I, ACurrentFontInfo);
      FCurrentScript := AActualScript;
      ACurrentCharacterLineType := ABoxCharacterLineType;
      if not Comparer.Equals(ACurrentCharacterLineType, CharacterLineTypeNone) then
        StartCharacterLine(I)
      else
        FCurrentScript := UnknownScript;
    end;
    if not Comparer.Equals(ACurrentCharacterLineType, CharacterLineTypeNone) then
    begin
      if ARunChanged then
      begin
        if I >= FNumberingListBoxesCount then
        begin
          ACurrentTextRun := TdxTextRun(FPieceTable.Runs[APrevBoxIndex]);
          ACurrentFontInfo := DocumentModel.FontCache[ACurrentTextRun.FontCacheIndex];
        end
        else
          ACurrentFontInfo := FNumberingListBox.GetFontInfo(PieceTable);
      end;
      FCurrentScript := AActualScript;
      AddBoxToCharacterLine(ABox);
    end;
  end;
  if not Comparer.Equals(ACurrentCharacterLineType, CharacterLineTypeNone) then
    EndCharacterLine(ALastNonSpaceBoxIndex + 1, ACurrentFontInfo);
end;

function TdxCharacterLineCalculator<T>.SetCurrentRunCharacterLineTypeFromNumberingListBox: Boolean;
begin
  FCurrentRunCharacterLineType := GetNumberingListBoxCharacterLineType(FNumberingListBox);
  FUseForWordsOnly := GetNumberingListBoxCharacterLineUseForWordsOnly(FNumberingListBox);
  Result := True;
end;

procedure TdxCharacterLineCalculator<T>.SetBoxes(const Value: TdxBoxCollection);
begin
  if FBoxes <> Value then
  begin
    if FOwnsBoxes then
      FreeAndNil(FBoxes);
    FBoxes := Value;
  end;
end;

function TdxCharacterLineCalculator<T>.SetCurrentRunCharacterLineTypeFromBox(ABox: TdxBox; APrevBoxIndex: TdxRunIndex): Boolean;
var
  ACurrentRunIndex: TdxRunIndex;
  ACurrentTextRun: TdxTextRun;
begin
  if ABox.StartPos.RunIndex = APrevBoxIndex then
    Exit(False);

  ACurrentRunIndex := ABox.StartPos.RunIndex;
  ACurrentTextRun := Safe<TdxTextRun>.Cast(FPieceTable.Runs[ACurrentRunIndex]);
  if ACurrentTextRun <> nil then
  begin
    FCurrentRunCharacterLineType := GetRunCharacterLineType(ACurrentTextRun, ACurrentRunIndex);
    FUseForWordsOnly := GetRunCharacterLineUseForWordsOnly(ACurrentTextRun, ACurrentRunIndex);
  end
  else
  begin
    FCurrentRunCharacterLineType := CharacterLineTypeNone;
    FUseForWordsOnly := False;
  end;
  Result := True;
end;

function TdxCharacterLineCalculator<T>.CalculateScriptOffset(AFontInfo: TdxFontInfo; AScript: TdxCharacterFormattingScript): Integer;
begin
  case AScript of
    TdxCharacterFormattingScript.Subscript:
      Result := AFontInfo.SubscriptOffset.Y;
    TdxCharacterFormattingScript.Superscript:
      Result := AFontInfo.SuperscriptOffset.Y;
  else
    Result := 0;
  end;
end;

function TdxCharacterLineCalculator<T>.GetBoxFontInfo(ABoxIndex: Integer): TdxFontInfo;
var
  ANumberingListBox: TdxNumberingListBox;
  AIndex: Integer;
begin
  if ABoxIndex < FNumberingListBoxesCount then
  begin
    ANumberingListBox := TdxNumberingListBox(Boxes[0]);
    Result := ANumberingListBox.GetFontInfo(PieceTable);
  end
  else
  begin
    AIndex := Boxes[ABoxIndex].GetRun(PieceTable).FontCacheIndex;
    Result := DocumentModel.FontCache[AIndex];
  end;
end;

function TdxCharacterLineCalculator<T>.GetBoxScript(ABoxIndex: Integer): TdxCharacterFormattingScript;
var
  ANumberingListBox: TdxNumberingListBox;
  ANumerationCharacterProperties: TdxMergedCharacterProperties;
begin
  if ABoxIndex < FNumberingListBoxesCount then
  begin
    ANumberingListBox := TdxNumberingListBox(Boxes[0]);
    ANumerationCharacterProperties := ANumberingListBox.GetNumerationCharacterProperties(PieceTable);
    try
      Result := ANumerationCharacterProperties.Info.Script;
    finally
      ANumerationCharacterProperties.Free;
    end;
  end
  else
    Result := TdxTextRunBase(Boxes[ABoxIndex].GetRun(PieceTable)).Script;
end;

procedure TdxCharacterLineCalculator<T>.Initialize(ARow: TdxRow);
begin
  if ARow.NumberingListBox <> nil then
  begin
    FNumberingListBox := ARow.NumberingListBox;
    FBoxCount := ARow.Boxes.Count + 1;
    Boxes := TdxBoxCollection.Create;
    FOwnsBoxes := True;
    FBoxes.Capacity := FBoxCount;
    FBoxes.Add(FNumberingListBox);
    FNumberingListBoxesCount := 1;
    FBoxes.AddRange(ARow.Boxes);
  end
  else
  begin
    Boxes := ARow.Boxes;
    FOwnsBoxes := False;
    FBoxCount := FBoxes.Count;
    FNumberingListBoxesCount := 0;
  end;
  FRow := ARow;
end;

procedure TdxCharacterLineCalculator<T>.SetPieceTable(const AValue: TdxPieceTable);
begin
  Assert(AValue <> nil);
  FPieceTable := AValue;
end;

function TdxCharacterLineCalculator<T>.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

{ TdxUnderlineInfo }

procedure TdxUnderlineInfo.Init;
begin
  TotalUnderlineWidth := 0;
  WeightedUnderlinePositionSum := 0;
  WeightedUnderlineThicknessSum := 0;
  TopOffset := 0;
end;

{ TdxLineSpacingCalculatorBase }

procedure TdxLineSpacingCalculatorBase.AlignRowBoxesToBaseLine(AFormatter: TdxParagraphFinalFormatter; ARow: TdxRow;
  ACount: Integer; const AAscents: TArray<Integer>);
var
  I, ABaseLine, ARowTop: Integer;
  ABoxes: TdxBoxList;
  ABox: TdxBox;
  R: TRect;
begin
  ABaseLine := ARow.BaseLineOffset;
  ABoxes := GetRowContentBoxes(ARow);
  try
    ARowTop := ARow.Bounds.Top;
    for I := 0 to ACount - 1 do
    begin
      ABox := TdxBox(ABoxes[I]);
      R := ABox.Bounds;
      R.Y := ARowTop + ABaseLine - AAscents[I] + AFormatter.CalculateSubscriptOrSuperScriptOffset(ABox);
      ABox.Bounds := R;
    end;
  finally
    FreeAndNil(ABoxes);
  end;
end;

function TdxLineSpacingCalculatorBase.CalcRowHeight(AOldRowHeight, ANewBoxHeight: Integer): Integer;
begin
  Result := Max(AOldRowHeight, ANewBoxHeight);
end;

function TdxLineSpacingCalculatorBase.CalcRowHeightFromInlineObject(AOldRowHeight, AMaxPictureHeight,
  AMaxDescent: Integer): Integer;
begin
  Result := Math.Max(AOldRowHeight, AMaxPictureHeight + AMaxDescent);
end;

function TdxLineSpacingCalculatorBase.CalculateSpacing(ARowHeight, AMaxAscentAndFree, AMaxDescent,
  AMaxPictureHeight: Integer): Integer;
var
  ARowTextHeight, ARowTextSpacing: Integer;
begin
  if (AMaxAscentAndFree = 0) and (AMaxDescent = 0) then
  begin
    Result := ARowHeight;
    Exit;
  end;
  ARowTextHeight := AMaxAscentAndFree + AMaxDescent;
  ARowTextSpacing := CalculateSpacingCore(ARowTextHeight);
  if AMaxAscentAndFree >= AMaxPictureHeight then
    Result := ARowTextSpacing
  else
    Result := CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight, ARowTextSpacing, AMaxPictureHeight);
end;

function TdxLineSpacingCalculatorBase.CreateParametersCalculator(AFormatter: TdxParagraphFinalFormatter;
  AParameterCount: Integer): TdxRowSpacingParametersCalculatorBase;
begin
  Result := TdxRowSpacingParametersCalculator.Create(AFormatter, AParameterCount);
end;

procedure TdxLineSpacingCalculatorBase.FormatRow(AFormatter: TdxParagraphFinalFormatter; ARow: TdxRow);
var
  I, ACount: Integer;
  ABoxes: TdxBoxList;
  ACalculator: TdxRowSpacingParametersCalculatorBase;
begin
  ABoxes := GetRowContentBoxes(ARow);
  try
    ACount := ABoxes.Count;
    if ACount <= 0 then
      Exit;
    ACalculator := CreateParametersCalculator(AFormatter, ACount);
    try
      for I := 0 to ACount - 1 do
        ACalculator.ProcessBox(TdxBox(ABoxes[I]), I);
      ARow.BaseLineOffset := ACalculator.CalculateRowBaseLineOffset(ARow) + ARow.SpacingBefore;
      AlignRowBoxesToBaseLine(AFormatter, ARow, ACount, ACalculator.ParameterValues);
    finally
      FreeAndNil(ACalculator);
    end;
  finally
    FreeAndNil(ABoxes);
  end;
end;

class function TdxLineSpacingCalculatorBase.GetLineSpacingCalculator(AParagraph: TdxSimpleParagraph): TdxLineSpacingCalculatorBase;
var
  ALineSpacing: Integer;
  AMultiplier: Single;
begin
  case AParagraph.LineSpacingType of
    TdxParagraphLineSpacing.Sesquialteral:
      Result := TdxSesquialteralSpacingCalculator.Create;
    TdxParagraphLineSpacing.Double:
      Result := TdxDoubleSpacingCalculator.Create;
    TdxParagraphLineSpacing.Multiple:
      begin
        AMultiplier := AParagraph.LineSpacing;
        if AMultiplier = 0 then
          AMultiplier := 1;
        Result := TdxMultipleSpacingCalculator.Create(AMultiplier);
      end;
    TdxParagraphLineSpacing.AtLeast:
      begin
        ALineSpacing := Trunc(AParagraph.LineSpacing);
        Result := TdxAtLeastSpacingCalculator.Create(Math.Max(1, AParagraph.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ALineSpacing)));
      end;
    TdxParagraphLineSpacing.Exactly:
      begin
        ALineSpacing := Trunc(AParagraph.LineSpacing);
        Result := TdxExactlySpacingCalculator.Create(Math.Max(1, AParagraph.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ALineSpacing)));
      end;
      else
        Result := TdxSingleSpacingCalculator.Create;
  end;
end;

function TdxLineSpacingCalculatorBase.GetDefaultRowHeight: Integer;
begin
  Result := 0;
end;

function TdxLineSpacingCalculatorBase.GetRowContentBoxes(ARow: TdxRow): TdxBoxList;
var
  I: Integer;
begin
  Result := TdxBoxList.Create;
  if ARow.NumberingListBox <> nil then
    Result.Add(ARow.NumberingListBox);
  for I := 0 to ARow.Boxes.Count - 1 do
    Result.Add(ARow.Boxes[I]);
end;

{ TdxSingleSpacingCalculator }

function TdxSingleSpacingCalculator.CalculateSpacingCore(ARowHeight: Integer): Integer;
begin
  Result := ARowHeight;
end;

function TdxSingleSpacingCalculator.CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight,
  ARowTextSpacing, AMaxPictureHeight: Integer): Integer;
begin
  Result := AMaxPictureHeight + AMaxDescent;
end;

{ TdxMultipleSpacingCalculator }

constructor TdxMultipleSpacingCalculator.Create(AMultiplier: Single);
begin
  inherited Create;
  if AMultiplier <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('multiplier', FloatToStr(AMultiplier));
  FMultiplier := AMultiplier;
end;

function TdxMultipleSpacingCalculator.CalculateSpacingCore(ARowHeight: Integer): Integer;
begin
  Result := Trunc(ARowHeight * FMultiplier);
end;

function TdxMultipleSpacingCalculator.CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight,
  ARowTextSpacing, AMaxPictureHeight: Integer): Integer;
begin
  Result := ARowHeight + (ARowTextSpacing - ARowTextHeight);
end;

function TdxMultipleSpacingCalculator.CreateParametersCalculator(AFormatter: TdxParagraphFinalFormatter;
  AParameterCount: Integer): TdxRowSpacingParametersCalculatorBase;
begin
  Result := TdxMultipleSpacingParametersCalculator.Create(AFormatter, AParameterCount, FMultiplier);
end;

{ TdxSesquialteralSpacingCalculator }

function TdxSesquialteralSpacingCalculator.CalculateSpacingCore(ARowHeight: Integer): Integer;
begin
  Result := Trunc(3 * ARowHeight / 2);
end;

function TdxSesquialteralSpacingCalculator.CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight,
  ARowTextSpacing, AMaxPictureHeight: Integer): Integer;
begin
  Result := ARowHeight + (ARowTextSpacing - ARowTextHeight);
end;

{ TdxDoubleSpacingCalculator }

function TdxDoubleSpacingCalculator.CalculateSpacingCore(ARowHeight: Integer): Integer;
begin
  Result := 2 * ARowHeight;
end;

function TdxDoubleSpacingCalculator.CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight,
  ARowTextSpacing, AMaxPictureHeight: Integer): Integer;
begin
  Result := ARowHeight + (ARowTextSpacing - ARowTextHeight);
end;

{ TdxExactlySpacingCalculator }

constructor TdxExactlySpacingCalculator.Create(ASpacing: Integer);
begin
  inherited Create;
  FSpacing := ASpacing;
  FBaseLineOffset := CalcBaseLineOffset;
end;

function TdxExactlySpacingCalculator.CalcBaseLineOffset: Integer;
begin
  Result := Trunc(1825 * FSpacing / (1825 + 443));
end;

function TdxExactlySpacingCalculator.CalcRowHeight(AOldRowHeight, ANewBoxHeight: Integer): Integer;
begin
  Result := FSpacing;
end;

function TdxExactlySpacingCalculator.CalcRowHeightFromInlineObject(AOldRowHeight, AMaxPictureHeight,
  AMaxDescent: Integer): Integer;
begin
  Result := FSpacing;
end;

function TdxExactlySpacingCalculator.CalculateSpacing(ARowHeight, AMaxAscent, AMaxDescent,
  AMaxPictureHeight: Integer): Integer;
begin
  Result := FSpacing;
end;

function TdxExactlySpacingCalculator.CalculateSpacingCore(ARowHeight: Integer): Integer;
begin
  Result := FSpacing;
end;

function TdxExactlySpacingCalculator.CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight,
  ARowTextSpacing, AMaxPictureHeight: Integer): Integer;
begin
  Result := 0;
end;

function TdxExactlySpacingCalculator.CreateParametersCalculator(AFormatter: TdxParagraphFinalFormatter;
  AParameterCount: Integer): TdxRowSpacingParametersCalculatorBase;
begin
  Result := TdxExactlyRowSpacingParametersCalculator.Create(AFormatter, AParameterCount, FBaseLineOffset);
end;

{ TdxAtLeastSpacingCalculator }

constructor TdxAtLeastSpacingCalculator.Create(ASpacing: Integer);
begin
  inherited Create;
  FSpacing := ASpacing;
end;

function TdxAtLeastSpacingCalculator.CalculateSpacing(ARowHeight, AMaxAscentAndFree, AMaxDescent,
  AMaxPictureHeight: Integer): Integer;
begin
  Result := Math.Max(FSpacing, ARowHeight);
end;

function TdxAtLeastSpacingCalculator.CalculateSpacingCore(ARowHeight: Integer): Integer;
begin
  Result := Math.Max(FSpacing, ARowHeight);
end;

function TdxAtLeastSpacingCalculator.CalculateSpacingInlineObjectCase(ARowHeight, AMaxDescent, ARowTextHeight,
  ARowTextSpacing, AMaxPictureHeight: Integer): Integer;
begin
  Result := 0;
end;

function TdxAtLeastSpacingCalculator.CreateParametersCalculator(AFormatter: TdxParagraphFinalFormatter;
  AParameterCount: Integer): TdxRowSpacingParametersCalculatorBase;
begin
  Result := TdxAtLeastRowSpacingParametersCalculator.Create(AFormatter, AParameterCount);
end;

function TdxAtLeastSpacingCalculator.GetDefaultRowHeight: Integer;
begin
  Result := FSpacing;
end;

{ TdxRowSpacingParametersCalculatorBase }

constructor TdxRowSpacingParametersCalculatorBase.Create(AFormatter: TdxParagraphFinalFormatter;
  AParameterCount: Integer);
begin
  inherited Create;
  FFormatter := AFormatter;
  SetLength(FParameterValues, AParameterCount);
end;

function TdxRowSpacingParametersCalculatorBase.CalculateActualMaxParameterValue: Integer;
begin
  if MaxParameterValue > 0 then
    Result := MaxParameterValue
  else
    if MaxMarkParameterValue > 0 then
      Result := MaxMarkParameterValue
    else
      Result := MaxSpaceParameterValue;
end;

function TdxRowSpacingParametersCalculatorBase.IsMarkBox(ABox: TdxBox): Boolean;
begin
  Result := ABox.IsLineBreak;
end;

function TdxRowSpacingParametersCalculatorBase.IsSpaceBox(ABox: TdxBox): Boolean;
begin
  Result := not ABox.IsNotWhiteSpaceBox;
end;

procedure TdxRowSpacingParametersCalculatorBase.ProcessParameterValue(ABox: TdxBox; AValue: Integer);
begin
  if IsMarkBox(ABox) then
    MaxMarkParameterValue := Max(MaxMarkParameterValue, AValue)
  else
    if IsSpaceBox(ABox) then
      MaxSpaceParameterValue := Max(MaxSpaceParameterValue, AValue)
    else
      MaxParameterValue := Max(MaxParameterValue, AValue);
end;

{ TdxRowSpacingParametersCalculator }

function TdxRowSpacingParametersCalculator.CalculateRowBaseLineOffset(ARow: TdxRow): Integer;
begin
  Result := CalculateActualMaxParameterValue;
end;

procedure TdxRowSpacingParametersCalculator.ProcessBox(ABox: TdxBox; ABoxIndex: Integer);
var
  AAscent: Integer;
begin
  AAscent := Formatter.CalcBaseBoxAscentAndFree(ABox);
  ProcessParameterValue(ABox, AAscent);
  ParameterValues[ABoxIndex] := Formatter.CalcBoxAscentAndFree(ABox);
end;

{ TdxDescentParametersCalculator }

procedure TdxDescentParametersCalculator.ProcessBox(ABox: TdxBox; ABoxIndex: Integer);
var
  ADescent: Integer;
begin
  ADescent := Formatter.CalcBoxDescent(ABox);
  ProcessParameterValue(ABox, ADescent);
  ParameterValues[ABoxIndex] := ADescent;
end;

function TdxDescentParametersCalculator.CalculateRowBaseLineOffset(ARow: TdxRow): Integer;
begin
  raise EInvalidOpException.Create('');
end;

{ TdxMultipleSpacingParametersCalculator }

constructor TdxMultipleSpacingParametersCalculator.Create(AFormatter: TdxParagraphFinalFormatter; AParameterCount: Integer; AMultiplier: Single);
begin
  inherited Create(AFormatter, AParameterCount);
  FMultiplier := AMultiplier;
  if AMultiplier < 1 then
    FDescentParametersCalculator := TdxDescentParametersCalculator.Create(AFormatter, AParameterCount);
end;

destructor TdxMultipleSpacingParametersCalculator.Destroy;
begin
  FDescentParametersCalculator.Free;
  inherited Destroy;
end;

procedure TdxMultipleSpacingParametersCalculator.ProcessBox(ABox: TdxBox; ABoxIndex: Integer);
begin
  inherited ProcessBox(ABox, ABoxIndex);
  if FDescentParametersCalculator <> nil then
    FDescentParametersCalculator.ProcessBox(ABox, ABoxIndex);
end;

function TdxMultipleSpacingParametersCalculator.CalculateRowBaseLineOffset(ARow: TdxRow): Integer;
var
  AMaxDescent, AMaxAscentAndFree, ATextHeight, ARowHeightDelta: Integer;
begin
  if FDescentParametersCalculator <> nil then
  begin
    AMaxDescent := FDescentParametersCalculator.CalculateActualMaxParameterValue;
    AMaxAscentAndFree := CalculateActualMaxParameterValue;
    ATextHeight := AMaxDescent + AMaxAscentAndFree;
    ARowHeightDelta := ATextHeight - ARow.Height;
    Result := CalculateActualMaxParameterValue - Max(0, ARowHeightDelta);
  end
  else
    Result := CalculateActualMaxParameterValue;
end;

{ TdxExactlyRowSpacingParametersCalculator }

function TdxExactlyRowSpacingParametersCalculator.CalculateRowBaseLineOffset(ARow: TdxRow): Integer;
begin
  Result := FBaseLineOffset;
end;

constructor TdxExactlyRowSpacingParametersCalculator.Create(AFormatter: TdxParagraphFinalFormatter; AParameterCount,
  ABaseLineOffset: Integer);
begin
  inherited Create(AFormatter, AParameterCount);
  FBaseLineOffset := ABaseLineOffset;
end;

procedure TdxExactlyRowSpacingParametersCalculator.ProcessBox(ABox: TdxBox; ABoxIndex: Integer);
begin
  ParameterValues[ABoxIndex] := Formatter.CalcBoxAscentAndFree(ABox);
end;

{ TdxAtLeastRowSpacingParametersCalculator }

function TdxAtLeastRowSpacingParametersCalculator.CalculateRowBaseLineOffset(ARow: TdxRow): Integer;
begin
  Result := Math.Max(FMaxPictureHeight, CalculateRowBaseLineOffsetCore(ARow));
end;

function TdxAtLeastRowSpacingParametersCalculator.CalculateRowBaseLineOffsetCore(ARow: TdxRow): Integer;
begin
  if ARow.LastParagraphRow then
    Result := ARow.LastParagraphRowOriginalHeight - CalculateActualMaxParameterValue - ARow.SpacingBefore
  else
    Result := ARow.Height - CalculateActualMaxParameterValue - ARow.SpacingBefore;
end;

procedure TdxAtLeastRowSpacingParametersCalculator.ProcessBox(ABox: TdxBox; ABoxIndex: Integer);
var
  ADescent: Integer;
begin
  if ABox.IsInlinePicture then
    FMaxPictureHeight := Math.Max(FMaxPictureHeight, ABox.Bounds.Height);
  ADescent := Formatter.CalcBaseBoxDescent(ABox);
  ProcessParameterValue(ABox, ADescent);
  ParameterValues[ABoxIndex] := Formatter.CalcBoxAscentAndFree(ABox);
end;

{ TdxStateRowEmpty }

procedure TdxStateRowEmpty.AddSuccess(ABoxInfo: TdxBoxInfo);
begin
  Assert(False);
end;

function TdxStateRowEmpty.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  Result := TdxStateContinueFormatResult.Success;
end;

function TdxStateRowEmpty.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := TdxAddBoxResult.Success;
end;

function TdxStateRowEmpty.ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowEmpty.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowEmpty;
end;

function TdxStateRowEmpty.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowEmptyAfterFloatingObject;
end;

function TdxStateRowEmpty.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowEmpty.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := False;
end;

{ TdxStateRowEmptyBase }

procedure TdxStateRowEmptyBase.ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert((CurrentRow.NumberingListBox <> nil) or (CurrentRow.Height = RowsController.DefaultRowHeight));
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

procedure TdxStateRowEmptyBase.ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert((CurrentRow.NumberingListBox <> nil) or (CurrentRow.Height = RowsController.DefaultRowHeight));
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

procedure TdxStateRowEmptyBase.ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert((CurrentRow.NumberingListBox <> nil) or (CurrentRow.Height = RowsController.DefaultRowHeight));
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

procedure TdxStateRowEmptyBase.ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo);
begin
  if not (TdxRowProcessingFlag.LastInvisibleEmptyCellRowAfterNestedTable in CurrentRow.ProcessingFlags) then
    RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

function TdxStateRowEmptyBase.CanFitCurrentRowToColumn(ABoxInfo: TdxBoxInfo): TdxCanFitCurrentRowToColumnResult;
begin
  Result := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
end;

function TdxStateRowEmptyBase.ContinueFormat: TdxStateContinueFormatResult;
var
  ACh: Char;
begin
  ACh := Iterator.CurrentChar;
  if ACh = TdxCharacters.ParagraphMark then
  begin
    if Iterator.IsParagraphMarkRun then
      Exit(GetContinueFormatResult(FinishParagraph))
    else
      ChangeState(TdxParagraphBoxFormatterState.RowWithTextOnly);
  end
  else
    if ACh = TdxCharacters.SectionMark then
    begin
      if Iterator.IsParagraphMarkRun then
        Exit(GetContinueFormatResult(FinishSection))
      else
        ChangeState(TdxParagraphBoxFormatterState.RowWithTextOnly);
    end
    else
      if TdxCharacters.IsCharSpace(ACh) then
        ChangeState(TdxParagraphBoxFormatterState.RowWithSpacesOnly)
      else
        if TdxCharacters.IsCharDash(ACh) then
          ChangeState(TdxParagraphBoxFormatterState.RowWithDashOnly)
        else
          if ACh = TdxCharacters.TabMark then
            ChangeState(TdxParagraphBoxFormatterState.RowFirstLeadingTab)
          else
            if ACh = TdxCharacters.LineBreak then
              ChangeState(TdxParagraphBoxFormatterState.RowLineBreak)
            else
              if ACh = TdxCharacters.PageBreak then
              begin
                if RowsController.TablesController.IsInsideTable then
                  ChangeState(TdxParagraphBoxFormatterState.RowWithSpacesOnly)
                else
                  if not RowsController.SupportsColumnAndPageBreaks then
                    ChangeState(TdxParagraphBoxFormatterState.RowLineBreak)
                  else
                    ChangeState(TdxParagraphBoxFormatterState.RowPageBreak);
              end
              else
                if ACh = TdxCharacters.ColumnBreak then
                begin
                  if RowsController.TablesController.IsInsideTable then
                      ChangeState(TdxParagraphBoxFormatterState.RowWithSpacesOnly)
                  else
                    if not RowsController.SupportsColumnAndPageBreaks then
                      ChangeState(TdxParagraphBoxFormatterState.RowLineBreak)
                    else
                      ChangeState(TdxParagraphBoxFormatterState.RowColumnBreak);
                end
                else
                  if ACh = TdxCharacters.FloatingObjectMark then
                  begin
                    if Iterator.IsFloatingObjectAnchorRun then
                      ChangeState(TdxParagraphBoxFormatterState.FloatingObject)
                    else
                      ChangeState(TdxParagraphBoxFormatterState.RowWithTextOnly);
                  end
                  else
                  if ACh = TdxCharacters.ObjectMark then
                  begin
                    if Iterator.IsInlinePictureRun then
                      ChangeState(TdxParagraphBoxFormatterState.RowWithInlineObjectOnly)
                    else
                      ChangeState(TdxParagraphBoxFormatterState.RowWithTextOnly);
                  end
                  else
                    ChangeState(TdxParagraphBoxFormatterState.RowWithTextOnly);
  Result := TdxStateContinueFormatResult.Success;
end;

function TdxStateRowEmptyBase.FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
    Result := FinalizeColumn(ABoxInfo)
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowEmptyBase.FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
  begin
    FinalizeLine(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  end
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowEmptyBase.FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
    Result := FinalizePage(ABoxInfo)
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowEmptyBase.FinishParagraph: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateParagraphMarkBoxInfo;
  try
    Result := InternalFinishParagraphCore(ABoxInfo, FinalizeParagraph);
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxStateRowEmptyBase.InternalFinishParagraphCore(ABoxInfo: TdxBoxInfo; AFinalizeHandler: TFinalizeEvent): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
  begin
    ApplyParagraphMarkSize(ABoxInfo);
    AFinalizeHandler(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  end
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowEmptyBase.FinishSection: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateSectionMarkBoxInfo;
  try
    Result := InternalFinishParagraphCore(ABoxInfo, FinalizeSection);
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

{ TdxStateParagraphFrame }

constructor TdxStateParagraphFrame.Create(AFormatter: TdxParagraphBoxFormatter; APreviousState: TdxBoxFormatterStateBase);
begin
  inherited Create(AFormatter);
  Assert(APreviousState <> nil);
  FPreviousState := APreviousState;
end;

function TdxStateParagraphFrame.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.ParagraphFrame;
end;

function TdxStateParagraphFrame.CanUseBox: Boolean;
begin
  Result := True;
end;

function TdxStateParagraphFrame.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := ACh <> TdxCharacters.FloatingObjectMark;
end;

function TdxStateParagraphFrame.ContinueFormat: TdxStateContinueFormatResult;
begin
  if Iterator.IsParagraphFrame then
    Exit(FormatParagraphFrame)
  else
  begin
    SwitchToNextState;
    Exit(TdxStateContinueFormatResult.Success);
  end;
end;

procedure TdxStateParagraphFrame.SwitchToNextState;
var
  ANewState: IdxSupportsChangeStateManually;
begin
  ChangeState(FPreviousState.StateAfterFloatingObject);
  if Supports(Formatter.State, IdxSupportsChangeStateManually, ANewState) then
    ANewState.ChangeStateManuallyIfNeeded(TdxParagraphIteratorResult.RunFinished);
end;

function TdxStateParagraphFrame.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := CanAddBoxCore(ABoxInfo);
end;

procedure TdxStateParagraphFrame.AddSuccess(ABoxInfo: TdxBoxInfo);
var
  ARun: TdxTextRunBase;
begin
  ARun := Formatter.PieceTable.Runs[ABoxInfo.StartPos.RunIndex];
  if Formatter.HasActualParagraphFrameProperties(ARun.Paragraph) then
  begin
    RowsController.FrameParagraphIndex := ARun.Paragraph.Index;
    RowsController.AddParagraphFrameToLayout(TdxParagraphFrameBox.Create(ARun.Paragraph), ABoxInfo);
  end;
end;

function TdxStateParagraphFrame.FormatParagraphFrame: TdxStateContinueFormatResult;
var
  ABoxInfo: TdxBoxInfo;
  AFrameBox: TdxParagraphFrameBox;
  AAddExistingBoxResult: TdxStateContinueFormatResult;
begin
  ABoxInfo := TdxBoxInfo.Create;
  try
    ABoxInfo.StartPos := Iterator.GetCurrentPosition;
    ABoxInfo.EndPos := ABoxInfo.StartPos;
    AFrameBox := TdxParagraphFrameBox.Create(Formatter.Iterator.Paragraph);
    try
      TdxBox.AddReference(AFrameBox);
      ABoxInfo.Box := AFrameBox;
      if CanUseBox and (ABoxInfo.StartPos = Iterator.CurrentBox.StartPos) then
      begin
        AAddExistingBoxResult := AppendBox(ABoxInfo);
        if AAddExistingBoxResult <> TdxStateContinueFormatResult.Success then
          Exit(AAddExistingBoxResult);
        if Iterator.IsEnd then
        begin
          if Iterator.IsParagraphFrame and (Self is TdxStateParagraphFrame) then
            Exit(TdxStateContinueFormatResult.Success);
          if (Formatter.SyllableIterator = nil) or (Formatter.SyllableIterator.IsEnd) then
          begin
            Exit(GetContinueFormatResult(FinishParagraphOrSection));
          end;
        end;
        Exit(TdxStateContinueFormatResult.Success);
      end;
      Result := ContinueFormatByCharacter(ABoxInfo, Safe<TdxLayoutDependentTextBox>.Cast(Iterator.CurrentBox));
    finally
      TdxBox.Release(AFrameBox);
    end;
  finally
    ABoxInfo.Free;
  end;
end;

function TdxStateParagraphFrame.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateParagraphFrame.ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateParagraphFrame.FinishParagraphOrSection: TdxCompleteFormattingResult;
begin
  Result := FPreviousState.FinishParagraphOrSection;
end;

function TdxStateParagraphFrame.CanAddBoxCore(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
  AActualParagraphFrameProperties: TdxMergedFrameProperties;
begin
  if ABoxInfo.Box = nil then
    ABoxInfo.Box := TdxParagraphFrameBox.Create(Formatter.Iterator.Paragraph);

  ACanFit := RowsController.CanFitCurrentRowWithFrameToColumn(ABoxInfo);
  if ABoxInfo.Box is TdxParagraphFrameBox then
  begin
    AActualParagraphFrameProperties := Formatter.ActualParagraphFrameProperties;
    try
      if AActualParagraphFrameProperties <> nil then
      begin
        if ACanFit = TdxCanFitCurrentRowToColumnResult.RestartDueFloatingObject then
          Exit(TdxAddBoxResult.RestartDueFloatingObject);
        if not AActualParagraphFrameProperties.IsParagraphFrame and not RowsController.CanFitBoxToCurrentRow(ABoxInfo.Size) then
          Exit(TdxAddBoxResult.HorizontalIntersect);
        Exit(TdxAddBoxResult.Success);
      end;
    finally
      AActualParagraphFrameProperties.Free;
    end;
  end;

  if ACanFit <> TdxCanFitCurrentRowToColumnResult.RowFitted then
  begin
    if ACanFit = TdxCanFitCurrentRowToColumnResult.TextAreasRecreated then
      Exit(TdxAddBoxResult.IntersectWithFloatObject)
    else
      if ACanFit = TdxCanFitCurrentRowToColumnResult.RestartDueFloatingObject then
        Exit(TdxAddBoxResult.RestartDueFloatingObject);
    Exit(GetAddBoxResultColumnOverfull(ACanFit));
  end;

  if not RowsController.CanFitBoxToCurrentRow(ABoxInfo.Size) then
    Result := TdxAddBoxResult.HorizontalIntersect
  else
    Result := TdxAddBoxResult.Success;
end;

{ TdxStateRowTextSplitAfterFirstLeadingTab }

function TdxStateRowTextSplitAfterFirstLeadingTab.CompleteRowProcessing: TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn;
  if ACanFit <> TdxCanFitCurrentRowToColumnResult.RowFitted then
    Result := Formatter.RollbackToStartOfRow(ACanFit)
  else
  begin
    EndRow;
    ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
    Result := TdxCompleteFormattingResult.Success;
  end;
end;

function TdxStateRowTextSplitAfterFirstLeadingTab.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextSplitAfterFirstLeadingTab;
end;

function TdxStateRowTextSplitAfterFirstLeadingTab.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextSplitAfterFirstLeadingTabAfterFloatingObject
end;

function TdxStateRowTextSplitAfterFirstLeadingTab.HandleUnsuccessfulSplitting: TdxCompleteFormattingResult;
begin
  Formatter.RollbackToStartOfWord;
  SetCurrentRowHeightToLastBoxHeight;
  Result := CompleteRowProcessing;
end;

{ TdxStateRowTextHyphenationFirstSyllable }

function TdxStateRowTextHyphenationFirstSyllable.GetNextState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextHyphenation;
end;

function TdxStateRowTextHyphenationFirstSyllable.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllable;
end;

function TdxStateRowTextHyphenationFirstSyllable.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllableAfterFloatingObject;
end;

function TdxStateRowTextHyphenationFirstSyllable.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  Formatter.RollbackToStartOfWord(TdxParagraphBoxIterator(Formatter.Iterator));
  Formatter.RollbackToStartOfWord;
  Formatter.RollbackToLastTab(TdxParagraphBoxIterator(Formatter.Iterator));
  Formatter.RollbackToLastTab;
  RowsController.TryToRemoveLastTabBox;
  if CurrentRow.Height <= 0 then
  begin
    SetCurrentRowHeightToLastBoxHeight;
    ACanFit := RowsController.CanFitCurrentRowToColumn;
    if ACanFit <> TdxCanFitCurrentRowToColumnResult.RowFitted then
    begin
      Formatter.ClearSyllableIterator;
      Exit(Formatter.RollbackToStartOfRow(ACanFit));
    end;
  end;
  EndRow;
  Formatter.ClearSyllableIterator;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
  Result := TdxCompleteFormattingResult.Success;
end;

{ TdxStateRowTextHyphenationFirstSyllableAfterFirstLeadingTab }

function TdxStateRowTextHyphenationFirstSyllableAfterFirstLeadingTab.CompleteRowProcessing: TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn;
  if ACanFit <> TdxCanFitCurrentRowToColumnResult.RowFitted then
    Exit(Formatter.RollbackToStartOfRow(ACanFit))
  else
  begin
    EndRow;
    ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
  end;
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowTextHyphenationFirstSyllableAfterFirstLeadingTab.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllableAfterFirstLeadingTab;
end;

function TdxStateRowTextHyphenationFirstSyllableAfterFirstLeadingTab.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllableAfterFirstLeadingTabAfterFloatingObject;
end;

function TdxStateRowTextHyphenationFirstSyllableAfterFirstLeadingTab.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ASplitBoxResult: TdxSplitBoxResult;
begin
  ASplitBoxResult := SplitBox(ABoxInfo);
  if ASplitBoxResult <> TdxSplitBoxResult.FailedHorizontalOverfull then
  begin
    AddTextBox(ABoxInfo);
    EndRow;
    ChangeState(TdxParagraphBoxFormatterState.RowEmptyHyphenation);
    Result := TdxCompleteFormattingResult.Success;
  end
  else
  begin
    Formatter.ClearSyllableIterator;
    Formatter.RollbackToStartOfWord;
    SetCurrentRowHeightToLastBoxHeight;
    Result := CompleteRowProcessing;
  end;
end;

{ TdxStateFloatingObject }

constructor TdxStateFloatingObject.Create(AFormatter: TdxParagraphBoxFormatter; APreviousState: TdxBoxFormatterStateBase);
begin
  inherited Create(AFormatter);
  Assert(APreviousState <> nil);
  FPreviousState := APreviousState;
end;

procedure TdxStateFloatingObject.AddSuccess(ABoxInfo: TdxBoxInfo);
var
  ARun: TdxTextRunBase;
  AFrameProperties: TdxFrameProperties;
  AObjectAnchorRun: TdxFloatingObjectAnchorRun;
  AAnchorBox: TdxFloatingObjectAnchorBox;
begin
  ARun := Formatter.PieceTable.Runs[ABoxInfo.StartPos.RunIndex];
  AObjectAnchorRun := Safe<TdxFloatingObjectAnchorRun>.Cast(ARun);
  if AObjectAnchorRun <> nil then
  begin
    if not AObjectAnchorRun.ExcludeFromLayout then
    begin
      AAnchorBox := Safe<TdxFloatingObjectAnchorBox>.Cast(ABoxInfo.Box);
      if AAnchorBox <> nil then
        RowsController.AddFloatingObjectToLayout(AAnchorBox, ABoxInfo);
    end;
  end
  else
  begin
    AFrameProperties := ARun.Paragraph.FrameProperties;
    if AFrameProperties <> nil then
    begin
      RowsController.FrameParagraphIndex := ARun.Paragraph.Index;
      RowsController.AddParagraphFrameToLayout(TdxParagraphFrameBox.Create(ARun.Paragraph), ABoxInfo);
    end;
  end;
end;

function TdxStateFloatingObject.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := CanAddBoxCore(ABoxInfo);
end;

function TdxStateFloatingObject.CanUseBox: Boolean;
begin
  Result := True;
end;

function TdxStateFloatingObject.ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateFloatingObject.ContinueFormat: TdxStateContinueFormatResult;
begin
  if Iterator.IsFloatingObjectAnchorRun then
    Result := inherited ContinueFormat
  else
  begin
    SwitchToNextState;
    Result := TdxStateContinueFormatResult.Success;
  end;
end;

function TdxStateFloatingObject.FinishParagraphOrSection;
begin
  Result := PreviousState.FinishParagraphOrSection;
end;

function TdxStateFloatingObject.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.FloatingObject;
end;

function TdxStateFloatingObject.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateFloatingObject.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := ACh <> TdxCharacters.FloatingObjectMark;
end;

procedure TdxStateFloatingObject.SwitchToNextState;
var
  ANewState: IdxSupportsChangeStateManually;
begin
  ChangeState(PreviousState.StateAfterFloatingObject);
  if Supports(Formatter.State, IdxSupportsChangeStateManually, ANewState) then
    ANewState.ChangeStateManuallyIfNeeded(TdxParagraphIteratorResult.RunFinished);
end;

{ TdxCustomColumnController }

constructor TdxCustomColumnController.Create;
begin
  inherited Create;
  FChildren := TdxReferencedObjectList<TdxReferencedObject>.Create;
end;

destructor TdxCustomColumnController.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TdxCustomColumnController.Reset(ASection: TdxSection);
begin
  ResetToFirstColumn;
  FChildren.Clear;
end;

procedure TdxCustomColumnController.AddChild(AChild: TdxReferencedObject);
begin
  FChildren.Add(AChild);
end;

procedure TdxCustomColumnController.RemoveChild(AChild: TdxReferencedObject);
begin
  FChildren.Remove(AChild);
end;

{ TdxTextArea.TdxTextAreaStartComparer }

function TdxTextArea.TdxTextAreaStartComparer.Compare(const X, Y: TdxTextArea): Integer;
begin
  Result := CompareInteger(X.Start, Y.Start);
end;

{ TdxTextArea }

constructor TdxTextArea.Create(AStart, AEnd: Integer);
begin
  Start := AStart;
  &End  := AEnd;
end;

class constructor TdxTextArea.Initialize;
begin
  FStartComparer := TdxTextAreaStartComparer.Create;
end;

class destructor TdxTextArea.Finalize;
begin
  FStartComparer.Free;
end;

function TdxTextArea.GetEmpty: TdxTextArea;
begin
  Result.Start := 0;
  Result.&End := 0;
end;

function TdxTextArea.GetWidth: Integer;
begin
  Result := &End - Start;
end;

function TdxTextArea.IntersectsWith(const AInterval: TdxTextArea): Boolean;
begin
  Result := (AInterval.&End >= Start) and (AInterval.Start <= &End);
end;

function TdxTextArea.IntersectsWithExcludingBounds(const AInterval: TdxTextArea): Boolean;
begin
  Result := (AInterval.&End > Start) and (AInterval.Start < &End);
end;

class function TdxTextArea.Union(const AInterval1, AInterval2: TdxTextArea): TdxTextArea;
begin
  Result := TdxTextArea.Create(Min(AInterval1.Start, AInterval2.Start), Max(AInterval1.&End, AInterval2.&End));
end;

function TdxTextArea.Subtract(const AInterval: TdxTextArea): TList<TdxTextArea>;
var
  ATextAreas: TdxTextAreaCollectionEx;
begin
  ATextAreas := TdxTextAreaCollectionEx.Create;
  try
    Result := ATextAreas.InnerList;
    if not IntersectsWithExcludingBounds(AInterval) then
    begin
      ATextAreas.Add(Self);
      Exit;
    end;

    if AInterval.Contains(Self) then
      Exit;

    if Self.Contains(AInterval) then
    begin
      if (Self.Start <> AInterval.Start) then
        ATextAreas.Add(TdxTextArea.Create(Self.Start, AInterval.Start));
      if (Self.&End <> AInterval.&End) then
        ATextAreas.Add(TdxTextArea.Create(AInterval.&End, Self.&End));
      Exit;
    end;

    if (Self.Start >= AInterval.Start) then
    begin
      if (Self.&End <> AInterval.&End) then
        ATextAreas.Add(TdxTextArea.Create(AInterval.&End, Self.&End));
    end
    else
    begin
      if (Self.Start <> AInterval.Start) then
        ATextAreas.Add(TdxTextArea.Create(Self.Start, AInterval.Start));
    end;
  finally
    ATextAreas.DontOwnInnerList;
    ATextAreas.Free;
  end;
end;

function TdxTextArea.Contains(const AInterval: TdxTextArea): Boolean;
begin
  Result := (AInterval.Start >= Start) and (AInterval.&End <= &End);
end;

function TdxTextArea.ToString: string;
begin
  Result := Format('[%d - %d]', [Start, &End]);
end;

{ TdxFormattingProcessResult }

procedure TdxFormattingProcessResult.Init(AFormattingProcess: TdxFormattingProcess);
begin
  FormattingProcess := AFormattingProcess;
  ParagraphIndex := 0;
  RestartPosition := TdxDocumentModelPosition.Null;
  ForceRestart := False;
end;

procedure TdxFormattingProcessResult.Init(AParagraphIndex: TdxParagraphIndex);
begin
  FormattingProcess := TdxFormattingProcess.ContinueFromParagraph;
  ParagraphIndex := AParagraphIndex;
  RestartPosition := TdxDocumentModelPosition.Null;
  ForceRestart := False;
end;

procedure TdxFormattingProcessResult.Init(const ARestartPosition: TdxDocumentModelPosition; AForceRestart: Boolean = False);
begin
  FormattingProcess := TdxFormattingProcess.RestartFromTheStartOfRow;
  ParagraphIndex := 0;
  RestartPosition := ARestartPosition;
  ForceRestart := AForceRestart;
end;

{ TdxParagraphBoxFormatter }

constructor TdxParagraphBoxFormatter.Create(APieceTable: TdxPieceTable; AMeasurer: TdxBoxMeasurer;
  ARowsController: TdxRowsController);
begin
  inherited Create(APieceTable, AMeasurer);
  FMaxHeight := MaxInt;
  if PieceTable.DocumentModel.DocumentProperties.HyphenateDocument then
    FHyphenationService := DocumentModel.GetService<IdxHyphenationService>;
  Initialize(ARowsController);
end;

destructor TdxParagraphBoxFormatter.Destroy;
begin
  FreeAndNil(FPreviousSyllableIterator);
  FreeAndNil(FSyllableIterator);
  FreeAndNil(FTableState);
  inherited Destroy;
end;

procedure TdxParagraphBoxFormatter.BeginParagraph(ABeginFromParagraphStart, AParagraphFrameStarted: Boolean);
var
  AParagraph: TdxParagraph;
  AShouldAddPageBreak: Boolean;
begin
  AParagraph := Iterator.Paragraph;
  if ABeginFromParagraphStart then
    FParagraphStartPos := TdxFormatterPosition.Create(Iterator.RunIndex, 0, 0)
  else
    FParagraphStartPos := TdxFormatterPosition.Create(PieceTable.VisibleTextFilter.FindVisibleRunForward(AParagraph.FirstRunIndex), 0, 0);
  FParagraphStartRowCount := RowsController.CurrentColumn.Rows.Count;
  AShouldAddPageBreak := ABeginFromParagraphStart and ShouldPageBreakBeforeParagraph(AParagraph) and
    (FParagraphStartRowCount > 0) and PieceTable.IsMain and CanBreakPageBefore(AParagraph);
  if AShouldAddPageBreak then
    RowsController.AddPageBreakBeforeRow;
  RowsController.BeginParagraph(AParagraph, ABeginFromParagraphStart, AParagraphFrameStarted);
  FSuppressHyphenation := (FHyphenationService = nil) or AParagraph.SuppressHyphenation or
    not DocumentModel.DocumentProperties.HyphenateDocument;
  if AShouldAddPageBreak then
    RowsController.OnPageBreakBeforeParagraph;
end;

procedure TdxParagraphBoxFormatter.BeginParagraphFormatting(AIterator: TdxParagraphBoxIterator;
  ABeginFromParagraphStart, AParagraphFrameStarted: Boolean);
begin
  Iterator := AIterator;
  HasDeferredNumberingListBox := False;
  BeginParagraph(ABeginFromParagraphStart, AParagraphFrameStarted);
  ChangeState(GetInitialStateType(ABeginFromParagraphStart));
end;

function TdxParagraphBoxFormatter.CanBreakPageBefore(AParagraph: TdxParagraph): Boolean;
var
  ACell: TdxTableCell;
begin
  if not RowsController.TablesController.IsInsideTable then
    Exit(True);
  ACell := AParagraph.GetCell;
  if ACell = nil then
    Exit(True);

  while ACell.Table.ParentCell <> nil do
    ACell := ACell.Table.ParentCell;
  Result := ACell.IsFirstCellInRow and (ACell.StartParagraphIndex = AParagraph.Index);
end;

function TdxParagraphBoxFormatter.GetNewState(AStateType: TdxParagraphBoxFormatterState): TdxBoxFormatterStateBase;
var
  APreviousState: TdxBoxFormatterStateBase;
begin
  APreviousState := State;
  Result := FStates[AStateType];
  Result.PreviousState := APreviousState;
end;

procedure TdxParagraphBoxFormatter.ChangeState(AStateType: TdxParagraphBoxFormatterState);
begin
  case AStateType of
    TdxParagraphBoxFormatterState.RowEmpty:
      begin
        StartNewRow;
        State := GetNewState(TdxParagraphBoxFormatterState.RowEmpty);
      end;
    TdxParagraphBoxFormatterState.RowEmptyAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.RowEmptyAfterFloatingObject);
    TdxParagraphBoxFormatterState.ParagraphStart:
      begin
        StartNewRow;
        State := GetNewState(TdxParagraphBoxFormatterState.ParagraphStart);
      end;
    TdxParagraphBoxFormatterState.ParagraphStartAfterBreak:
      begin
        StartNewRow;
        State := GetNewState(TdxParagraphBoxFormatterState.ParagraphStartAfterBreak);
      end;
    TdxParagraphBoxFormatterState.ParagraphStartAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.ParagraphStartAfterFloatingObject);
    TdxParagraphBoxFormatterState.ParagraphStartFromTheMiddle:
      begin
        StartNewRow;
        State := GetNewState(TdxParagraphBoxFormatterState.ParagraphStartFromTheMiddle);
      end;
    TdxParagraphBoxFormatterState.ParagraphStartFromTheMiddleAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.ParagraphStartFromTheMiddleAfterFloatingObject);
    TdxParagraphBoxFormatterState.RowWithSpacesOnly:
      State := GetNewState(TdxParagraphBoxFormatterState.RowWithSpacesOnly);
    TdxParagraphBoxFormatterState.RowWithTextOnly:
      begin
        State := GetNewState(TdxParagraphBoxFormatterState.RowWithTextOnly);
        StartNewWord;
      end;
    TdxParagraphBoxFormatterState.RowWithInlineObjectOnly:
      begin
        State := GetNewState(TdxParagraphBoxFormatterState.RowWithInlineObjectOnly);
        StartNewWord;
      end;
    TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.RowWithTextOnly);
    TdxParagraphBoxFormatterState.RowWithDashOnly:
      begin
        StartNewWord;
        State := GetNewState(TdxParagraphBoxFormatterState.RowWithDashOnly);
      end;
    TdxParagraphBoxFormatterState.RowWithDashAfterTextOnly:
      begin
        StartNewWord;
        State := GetNewState(TdxParagraphBoxFormatterState.RowWithDashAfterTextOnly);
      end;
    TdxParagraphBoxFormatterState.RowWithDashOnlyAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.RowWithDashOnly);
    TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFirstLeadingTab:
      begin
        State := GetNewState(TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFirstLeadingTab);
        StartNewWord;
      end;
    TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFirstLeadingTabAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFirstLeadingTab);
    TdxParagraphBoxFormatterState.RowFirstLeadingTab:
      State := GetNewState(TdxParagraphBoxFormatterState.RowFirstLeadingTab);
    TdxParagraphBoxFormatterState.RowLeadingTab:
      begin
        StartNewTab;
        State := GetNewState(TdxParagraphBoxFormatterState.RowLeadingTab);
      end;
    TdxParagraphBoxFormatterState.RowLeadingTabAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.RowLeadingTab);
    TdxParagraphBoxFormatterState.RowTab:
      begin
        StartNewTab;
        State := GetNewState(TdxParagraphBoxFormatterState.RowTab);
      end;
    TdxParagraphBoxFormatterState.RowTabAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.RowTab);
    TdxParagraphBoxFormatterState.RowSpaces:
      begin
        ResetLastTabPosition;
        State := GetNewState(TdxParagraphBoxFormatterState.RowSpaces);
      end;
    TdxParagraphBoxFormatterState.RowText:
      begin
        StartNewWord;
        State := GetNewState(TdxParagraphBoxFormatterState.RowText);
      end;
    TdxParagraphBoxFormatterState.RowTextAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.RowText);
    TdxParagraphBoxFormatterState.RowDashAfterText:
      State := GetNewState(TdxParagraphBoxFormatterState.RowDashAfterText);
    TdxParagraphBoxFormatterState.RowDash:
      begin
        StartNewWord;
        State := GetNewState(TdxParagraphBoxFormatterState.RowDash);
      end;
    TdxParagraphBoxFormatterState.RowDashAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.RowDash);
    TdxParagraphBoxFormatterState.RowTextSplit:
      begin
        StartNewRow;
        State := GetNewState(TdxParagraphBoxFormatterState.RowTextSplit);
      end;
    TdxParagraphBoxFormatterState.RowTextSplitAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.RowTextSplitAfterFloatingObject);
    TdxParagraphBoxFormatterState.RowTextSplitAfterFirstLeadingTab:
      begin
        State := GetNewState(TdxParagraphBoxFormatterState.RowTextSplitAfterFirstLeadingTab);
        StartNewWord;
      end;
    TdxParagraphBoxFormatterState.RowTextSplitAfterFirstLeadingTabAfterFloatingObject:
      State := GetNewState(TdxParagraphBoxFormatterState.RowTextSplitAfterFirstLeadingTab);
    TdxParagraphBoxFormatterState.RowDashSplit:
      begin
        StartNewRow;
        State := GetNewState(TdxParagraphBoxFormatterState.RowDashSplit);
      end;
    TdxParagraphBoxFormatterState.RowDashSplitAfterFloatingObject:
        State := GetNewState(TdxParagraphBoxFormatterState.RowDashSplitAfterFloatingObject);
    TdxParagraphBoxFormatterState.RowEmptyHyphenation:
      begin
        UpdateSyllableIterator;
        StartNewRow;
        State := GetNewState(TdxParagraphBoxFormatterState.RowEmptyHyphenation);
      end;
    TdxParagraphBoxFormatterState.RowEmptyHyphenationAfterFloatingObject:
      begin
        UpdateSyllableIterator;
        State := GetNewState(TdxParagraphBoxFormatterState.RowEmptyHyphenationAfterFloatingObject);
      end;
    TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllable:
      begin
        UpdateSyllableIterator;
        State := GetNewState(TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllable);
        StartNewWord;
      end;
    TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllableAfterFloatingObject:
      begin
        UpdateSyllableIterator;
        State := GetNewState(TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllable);
      end;
    TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllableAfterFirstLeadingTab:
      begin
        UpdateSyllableIterator;
        State := GetNewState(TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllableAfterFirstLeadingTab);
        StartNewWord;
      end;
    TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllableAfterFirstLeadingTabAfterFloatingObject:
      begin
        UpdateSyllableIterator;
        State := GetNewState(TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllableAfterFirstLeadingTab);
      end;
    TdxParagraphBoxFormatterState.RowTextHyphenation:
      begin
        UpdateSyllableIterator;
        State := GetNewState(TdxParagraphBoxFormatterState.RowTextHyphenation);
        StartNewWord;
      end;
    TdxParagraphBoxFormatterState.RowTextHyphenationAfterFloatingObject:
      begin
        UpdateSyllableIterator;
        State := GetNewState(TdxParagraphBoxFormatterState.RowTextHyphenation);
      end;
    TdxParagraphBoxFormatterState.RowLineBreak:
      State := GetNewState(TdxParagraphBoxFormatterState.RowLineBreak);
    TdxParagraphBoxFormatterState.RowPageBreak:
      State := GetNewState(TdxParagraphBoxFormatterState.RowPageBreak);
    TdxParagraphBoxFormatterState.RowPageBreakAtParagraphStart:
      State := GetNewState(TdxParagraphBoxFormatterState.RowPageBreakAtParagraphStart);
    TdxParagraphBoxFormatterState.RowColumnBreak:
      State := GetNewState(TdxParagraphBoxFormatterState.RowColumnBreak);
    TdxParagraphBoxFormatterState.RowColumnBreakAtParagraphStart:
      State := GetNewState(TdxParagraphBoxFormatterState.RowColumnBreakAtParagraphStart);
    TdxParagraphBoxFormatterState.FloatingObject:
      begin
        StartFloatingObjects;
        State := GetNewState(TdxParagraphBoxFormatterState.FloatingObject);
      end;
    TdxParagraphBoxFormatterState.SectionBreakAfterParagraphMark:
      State := GetNewState(TdxParagraphBoxFormatterState.SectionBreakAfterParagraphMark);
    TdxParagraphBoxFormatterState.Final:
      State := GetNewState(TdxParagraphBoxFormatterState.Final);
  else
    TdxRichEditExceptions.ThrowInternalException;
  end;
end;

function TdxParagraphBoxFormatter.GetActualParagraphFrameProperties(AParagraph: TdxSimpleParagraph): TdxMergedFrameProperties;
begin
  Result := AParagraph.GetActualFrameProperties(False);
  if (Result = nil) or not Result.IsParagraphFrame then
    FreeAndNil(Result);
end;

function TdxParagraphBoxFormatter.IsParagraphFrame(AProperties: TdxMergedFrameProperties): Boolean;
begin
  Result := (AProperties.Width <> 0) or
    ((AProperties.Height <> 0) and (AProperties.Info.HorizontalRule <> TdxParagraphFrameHorizontalRule.Auto)) or
    (AProperties.X <> 0) or (AProperties.Y <> 0) or
    (AProperties.HorizontalPositionType <> TdxParagraphFrameHorizontalPositionType.Column) or
    (AProperties.VerticalPositionType = TdxParagraphFrameVerticalPositionType.Paragraph) or
    (AProperties.Options.UseHorizontalPositionAlignment and (AProperties.HorizontalPositionAlignment <> TdxParagraphFrameHorizontalPositionAlignment.Left)) or
    (AProperties.Options.UseVerticalPositionAlignment and (AProperties.VerticalPositionAlignment <> TdxParagraphFrameVerticalPositionAlignment.Inline)) or
    (AProperties.Info.TextWrapType <> TdxParagraphFrameTextWrapType.Auto);
end;

procedure TdxParagraphBoxFormatter.ChangeStateContinueFromParagraph(AParagraphIndex: TdxParagraphIndex);
begin
  TdxStateContinueFormattingFromParagraph(FStates[TdxParagraphBoxFormatterState.ContinueFromParagraph]).FParagraphIndex := AParagraphIndex;
  State := FStates[TdxParagraphBoxFormatterState.ContinueFromParagraph];
end;

procedure TdxParagraphBoxFormatter.ClearSyllableIterator;
begin
  FreeAndNil(FPreviousSyllableIterator);
  FPreviousSyllableIterator := FSyllableIterator;
  FSyllableIterator := nil;
end;

procedure TdxParagraphBoxFormatter.EndParagraph;
begin
  if Iterator.CurrentChar = TdxCharacters.ParagraphMark then
    RowsController.EndParagraph
  else
    RowsController.EndSection;
end;

procedure TdxParagraphBoxFormatter.EndParagraphFormatting;
begin
  EndParagraph;
end;

function TdxParagraphBoxFormatter.FormatNextRow: TdxFormattingProcessResult;
var
  ARestartPosition: TdxDocumentModelPosition;
  AContinueFormatResult: TdxStateContinueFormatResult;
begin
  repeat
    AContinueFormatResult := State.ContinueFormat;
    if AContinueFormatResult <> TdxStateContinueFormatResult.Success then
    begin
      if AContinueFormatResult = TdxStateContinueFormatResult.RestartDueFloatingObject then
      begin
        FLastRestartDueToFloatingObjectParagraphStartPos := FParagraphStartPos;
        ARestartPosition := RowsController.RestartModelPosition;
        Assert(ARestartPosition.IsValid);
        RowsController.RestartModelPosition.Invalidate;
        FHasUnapprovedFloatingObjects := False;
        Result.Init(ARestartPosition);
        Exit;
      end
      else
      begin
        RowsController.ColumnController.PageAreaController.PageController.SetPageLastRunIndex(FLastRestartDueToFloatingObjectParagraphStartPos.RunIndex - 1);
        Assert(RowsController.LastRestartDueToFloatingObjectModelPosition.IsValid);
        RowsController.RestartModelPosition.Invalidate;
        Result.Init(RowsController.LastRestartDueToFloatingObjectModelPosition, True);
        Exit;
      end;
    end;
    case State.&Type of
      TdxParagraphBoxFormatterState.RowEmpty, TdxParagraphBoxFormatterState.RowEmptyHyphenation:
        begin
          Result.Init(TdxFormattingProcess.Continue);
          Break;
        end;
      TdxParagraphBoxFormatterState.Final:
        begin
          Result.Init(TdxFormattingProcess.Finish);
          Break;
        end;
      TdxParagraphBoxFormatterState.ContinueFromParagraph:
        begin
          Result.Init(TdxStateContinueFormattingFromParagraph(State).ParagraphIndex);
          Break;
        end;
    end;
  until False;
end;

function TdxParagraphBoxFormatter.GetActiveIterator: TdxParagraphBoxIterator;
var
  AIterator: TdxParagraphIteratorBase;
begin
  Result := TdxParagraphBoxIterator(Iterator);
  if State <> nil then
  begin
    AIterator := State.Iterator;
    if AIterator <> nil then
      Result := TdxParagraphBoxIterator(AIterator);
  end;
end;

function TdxParagraphBoxFormatter.HasActualParagraphFrameProperties: Boolean;
begin
  Result := HasActualParagraphFrameProperties(Iterator.Paragraph);
end;

function TdxParagraphBoxFormatter.HasActualParagraphFrameProperties(AParagraph: TdxSimpleParagraph): Boolean;
var
  AFrameProperties: TdxMergedFrameProperties;
begin
  AFrameProperties := GetActualParagraphFrameProperties(AParagraph);
  try
    Result := AFrameProperties <> nil;
  finally
    AFrameProperties.Free;
  end;
end;

function TdxParagraphBoxFormatter.GetActualParagraphFramePropertiesProperty: TdxMergedFrameProperties;
begin
  Result := GetActualParagraphFrameProperties(Iterator.Paragraph);
end;

function TdxParagraphBoxFormatter.GetCurrentRow: TdxRow;
begin
  Result := RowsController.CurrentRow;
end;

function TdxParagraphBoxFormatter.GetInitialStateType(ABeginFromParagraphStart: Boolean): TdxParagraphBoxFormatterState;
var
  ARuns: TdxTextRunCollection;
  APrevParagraph: TdxSimpleParagraph;
begin
  if ABeginFromParagraphStart then
    Result := TdxParagraphBoxFormatterState.ParagraphStart
  else
    Result := TdxParagraphBoxFormatterState.ParagraphStartFromTheMiddle;

  ARuns := PieceTable.Runs;
  if not (ARuns[Iterator.RunIndex] is TdxSectionRun) then
    Exit;

  if ARuns[Iterator.RunIndex] <> ARuns.First then
    if ARuns[Iterator.RunIndex - 1].ClassType = TdxParagraphRun then
    begin
      APrevParagraph := ARuns[Iterator.RunIndex - 1].Paragraph;
      if not APrevParagraph.IsInCell and not HasActualParagraphFrameProperties(APrevParagraph) then
        Result := TdxParagraphBoxFormatterState.SectionBreakAfterParagraphMark;
    end;
end;

procedure TdxParagraphBoxFormatter.Initialize(ARowsController: TdxRowsController);
begin
  if ARowsController = nil then
    TdxRichEditExceptions.ThrowArgumentException('rowsController', ARowsController);

  FRowsController := ARowsController;
  FTableState := TdxParagraphBoxFormatterTextState.Create(Self);
  SubscribeRowsControllerEvents;
end;

procedure TdxParagraphBoxFormatter.OnTableStart(ASender: TObject; AE: TdxEventArgs);
begin
  FTableState.Free;
  FTableState := TdxParagraphBoxFormatterTableState.Create(Self);
end;

procedure TdxParagraphBoxFormatter.ResetLastTabPosition;
begin
  FLastTabStartPos := InvalidPosition;
end;

procedure TdxParagraphBoxFormatter.RollbackToLastTab;
var
  AIterator: TdxParagraphBoxIterator;
begin
  AIterator := GetActiveIterator;
  RollbackToLastTab(AIterator);
end;

procedure TdxParagraphBoxFormatter.RollbackToLastTab(AIter: TdxParagraphBoxIterator);
begin
  if FLastTabStartPos <> InvalidPosition then
    AIter.SetPosition(FLastTabStartPos);
end;

function TdxParagraphBoxFormatter.RollbackToParagraphStart(const APos: TdxFormatterPosition; AInitialRowCount: Integer): TdxCompleteFormattingResult;
var
  ARows: TdxRowCollection;
begin
  Result := RowsController.CompleteCurrentColumnFormatting;
  if Result <> TdxCompleteFormattingResult.Success then
    Exit;

  RollbackToPositionAndClearLastRow(APos);
  ARows := RowsController.CurrentColumn.Rows;
  ARows.DeleteRange(AInitialRowCount, ARows.Count - AInitialRowCount);
  RowsController.MoveRowToNextColumn;
  ChangeState(TdxParagraphBoxFormatterState.ParagraphStart);
end;

procedure TdxParagraphBoxFormatter.RollbackToPositionAndClearLastRow(const APos: TdxFormatterPosition);
var
  AIterator: TdxParagraphBoxIterator;
begin
  AIterator := GetActiveIterator;
  AIterator.SetPosition(APos);
  RowsController.ClearRow(True);
  ResetLastTabPosition;
end;

function TdxParagraphBoxFormatter.RollbackToStartOfRow(ACanFit: TdxCanFitCurrentRowToColumnResult): TdxCompleteFormattingResult;
var
  ARollbackToParagraphStart: Boolean;
  ANextState: TdxParagraphBoxFormatterState;
  AFirstVisibleRunIndex: TdxRunIndex;
begin
  AFirstVisibleRunIndex := Iterator.VisibleTextFilter.FindVisibleRunForward(Iterator.Paragraph.FirstRunIndex);
  ARollbackToParagraphStart := (RowStartPos.RunIndex = AFirstVisibleRunIndex) and (RowStartPos.Offset = 0);
  if ARollbackToParagraphStart then
    ANextState := TdxParagraphBoxFormatterState.ParagraphStart
  else
    ANextState := TdxParagraphBoxFormatterState.RowEmpty;
  Result := RollbackToStartOfRowCore(ACanFit, ANextState);
end;

function TdxParagraphBoxFormatter.RollbackToStartOfRowCore(ACanFit: TdxCanFitCurrentRowToColumnResult; ANextState: TdxParagraphBoxFormatterState): TdxCompleteFormattingResult;
var
  AStartParagraphIndex: TdxParagraphIndex;
begin
  FHasUnapprovedFloatingObjects := False;
  if ACanFit = TdxCanFitCurrentRowToColumnResult.FirstCellRowNotFitted then
  begin
    RollbackToPositionAndClearLastRow(RowStartPos);
    AStartParagraphIndex := RollbackToStartOfRowTable(ACanFit);
    ChangeStateContinueFromParagraph(AStartParagraphIndex);
  end
  else
    if ACanFit = TdxCanFitCurrentRowToColumnResult.TextAreasRecreated then
    begin
      RollbackToPositionAndClearLastRow(RowStartPos);
      ChangeState(ANextState);
    end
    else
    begin
      if (FParagraphStartRowCount > 0) and RowsController.Paragraph.KeepLinesTogether and PieceTable.IsMain and not RowsController.TablesController.IsInsideTable then
      begin
        Result := RollbackToParagraphStart(ParagraphStartPos, FParagraphStartRowCount);
        if Result <> TdxCompleteFormattingResult.Success then
          Exit;
        FParagraphStartRowCount := -1;
      end
      else
      begin
        Result := RowsController.CompleteCurrentColumnFormatting;
        if Result <> TdxCompleteFormattingResult.Success then
          Exit;
        RollbackToPositionAndClearLastRow(RowStartPos);
        RowsController.MoveRowToNextColumn;
        ChangeState(ANextState);
      end;
    end;
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxParagraphBoxFormatter.RollbackToStartOfRowTable(
  ACanFit: TdxCanFitCurrentRowToColumnResult): TdxParagraphIndex;
begin
  Result := RowsController.TablesController.RollbackToStartOfRowTableOnFirstCellRowColumnOverfull;
end;

procedure TdxParagraphBoxFormatter.RollbackToStartOfWord(AIter: TdxParagraphBoxIterator);
begin
  AIter.SetPosition(WordStartPos);
  RowsController.RemoveLastTextBoxes;
end;

procedure TdxParagraphBoxFormatter.CreateStates;
const
  StateClasses: array[TdxParagraphBoxFormatterState] of TdxBoxFormatterStateBaseClass = (
    TdxStateParagraphStart,
    TdxStateParagraphStartAfterBreak,
    TdxStateParagraphStartFromTheMiddle,
    TdxStateParagraphStartAfterFloatingObject,
    TdxStateParagraphStartFromTheMiddleAfterFloatingObject,
    TdxStateRowEmpty,
    TdxStateRowEmptyAfterFloatingObject,

    TdxStateRowWithSpacesOnly,
    TdxStateRowWithTextOnly,
    TdxStateRowWithInlineObjectOnly,
    nil,
    TdxStateRowWithDashOnly,
    nil,
    TdxStateRowWithTextOnlyAfterFirstLeadingTab,
    nil,

    TdxStateRowSpaces,
    TdxStateRowText,
    nil,
    TdxStateRowDashAfterText,
    TdxStateRowDash,
    nil,
    TdxStateRowWithDashAfterTextOnly,
    TdxStateRowTextSplit,
    TdxStateRowTextSplitAfterFloatingObject,
    nil,
    TdxStateRowTextSplitAfterFirstLeadingTab,
    nil,

    TdxStateRowDashSplit,
    TdxStateRowDashSplitAfterFloatingObject,

    TdxStateRowEmptyHyphenation,
    TdxStateRowEmptyHyphenationAfterFloatingObject,
    TdxStateRowTextHyphenationFirstSyllable,
    nil,
    TdxStateRowTextHyphenationFirstSyllableAfterFirstLeadingTab,
    nil,
    TdxStateRowTextHyphenation,
    nil,
    TdxStateRowFirstLeadingTab,
    TdxStateRowLeadingTab,
    nil,
    TdxStateRowTab,
    nil,
    TdxStateRowLineBreak,
    TdxStateRowPageBreak,
    TdxStateRowPageBreakAtParagraphStart,
    TdxStateRowColumnBreak,
    TdxStateRowColumnBreakAtParagraphStart,

    TdxStateSectionBreakAfterParagraphMark,
    TdxStateFloatingObject,
    TdxStateParagraphFrame,

    TdxStateContinueFormattingFromParagraph,

    TdxStateFinal
  );
var
  I: TdxParagraphBoxFormatterState;
begin
  for I := Low(TdxParagraphBoxFormatterState) to High(TdxParagraphBoxFormatterState) do
    if StateClasses[I] <> nil then
      FStates[I] := StateClasses[I].Create(Self);
end;

procedure TdxParagraphBoxFormatter.DestroyStates;
var
  I: TdxParagraphBoxFormatterState;
begin
  for I := Low(TdxParagraphBoxFormatterState) to High(TdxParagraphBoxFormatterState) do
    FStates[I].Free;
end;

function TdxParagraphBoxFormatter.GetIteratorClass: TdxIteratorClass;
begin
  Result := TdxParagraphBoxIterator;
end;

procedure TdxParagraphBoxFormatter.ApproveFloatingObjects;
begin
  FHasUnapprovedFloatingObjects := False;
end;

procedure TdxParagraphBoxFormatter.RollbackUnapprovedFloatingObjects;
begin
  if FHasUnapprovedFloatingObjects then
  begin
    GetActiveIterator.SetPosition(FUnapprovedFloatingObjectsStartPos);
    FHasUnapprovedFloatingObjects := False;
  end;
end;

procedure TdxParagraphBoxFormatter.StartFloatingObjects;
begin
  if not FHasUnapprovedFloatingObjects then
  begin
    FUnapprovedFloatingObjectsStartPos := GetActiveIterator.GetCurrentPosition;
    FHasUnapprovedFloatingObjects := True;
  end;
end;

procedure TdxParagraphBoxFormatter.RollbackToStartOfWord;
begin
  RollbackToStartOfWord(GetActiveIterator);
end;

function TdxParagraphBoxFormatter.ShouldPageBreakBeforeParagraph(AParagraph: TdxParagraph): Boolean;
var
  APageBreakBefore: Boolean;
begin
  APageBreakBefore := AParagraph.PageBreakBefore;
  if (not APageBreakBefore) or (AParagraph.Length > 1) then
    Result := APageBreakBefore
  else
    Result := not (PieceTable.Runs[AParagraph.FirstRunIndex] is TdxSectionRun);
end;

procedure TdxParagraphBoxFormatter.StartNewRow;
begin
  FRowStartPos := GetActiveIterator.GetCurrentPosition;
  ResetLastTabPosition;
  StartNewWord;
end;

procedure TdxParagraphBoxFormatter.StartNewTab;
begin
  StartNewWord;
  FLastTabStartPos := GetActiveIterator.GetCurrentPosition;
end;

procedure TdxParagraphBoxFormatter.StartNewWord;
begin
  FWordStartPos := GetActiveIterator.GetCurrentPosition;
  RowsController.StartNewWord;
end;

procedure TdxParagraphBoxFormatter.SubscribeRowsControllerEvents;
begin
  FRowsController.TableStarted.Add(OnTableStart);
end;

procedure TdxParagraphBoxFormatter.UnsubscribeRowsControllerEvents;
begin
  FRowsController.TableStarted.Remove(OnTableStart);
end;

procedure TdxParagraphBoxFormatter.UpdateSyllableIterator;
begin
  if FSyllableIterator = nil then
    FSyllableIterator := TdxSyllableBoxIterator.Create(Iterator as TdxParagraphBoxIterator, FHyphenationService);
end;

{ TdxRichEditCharacterFormatterStateBase }

constructor TdxCharacterFormatterStateBase.Create(
  AFormatter: TdxParagraphCharacterFormatter);
begin
  inherited Create;
  Assert(AFormatter <> nil);
  FFormatter := AFormatter;
end;

procedure TdxCharacterFormatterStateBase.AddBox(AType: TdxBoxClass; var ABoxInfo: TdxBoxInfo; AMeasured: Boolean);
var
  ABox: TdxBox;
begin
  ABox := AType.CreateBox;
  ABox.StartPos := ABoxInfo.StartPos;
  ABox.EndPos := ABoxInfo.EndPos;
  if AMeasured then
    ABox.Bounds := TRect.Create(0, 0, ABoxInfo.Size.cx, ABoxInfo.Size.cy)
  else
  begin
    Formatter.AddTextBoxToQueue(ABox, ABoxInfo);
    ABoxInfo := nil;
  end;
  Paragraph.BoxCollection.Add(ABox);
end;

procedure TdxCharacterFormatterStateBase.AppendBoxCore(AType: TdxBoxClass; var ABoxInfo: TdxBoxInfo);
var
  AMeasured: Boolean;
begin
  AMeasured := False;
  if AType <> TdxTextBox then
  begin
    if AType <> TdxLayoutDependentTextBox then
      MeasureBoxContent(ABoxInfo);
    AMeasured := True;
  end;
  AddBox(AType, ABoxInfo, AMeasured);
  SwitchToNextState;
end;

procedure TdxCharacterFormatterStateBase.ChangeState(AStateType: TdxCharacterFormatterState);
begin
  Formatter.ChangeState(AStateType);
end;

function TdxCharacterFormatterStateBase.ContinueFormat: Boolean;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := TdxBoxInfo.Create;
  try
    ABoxInfo.StartPos := Iterator.GetCurrentPosition;
    Result := ContinueFormatByCharacter(ABoxInfo, nil) = TdxStateContinueFormatResult.Success;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxCharacterFormatterStateBase.CreateParagraphMarkBoxInfo: TdxBoxInfo;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := TdxBoxInfo.Create;
  ABoxInfo.StartPos := Iterator.GetCurrentPosition;
  ABoxInfo.EndPos := ABoxInfo.StartPos;
  Measurer.MeasureParagraphMark(ABoxInfo);
  Result := ABoxInfo;
end;

function TdxCharacterFormatterStateBase.CreateSectionMarkBoxInfo: TdxBoxInfo;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := TdxBoxInfo.Create;
  ABoxInfo.StartPos := Iterator.GetCurrentPosition;
  ABoxInfo.EndPos := ABoxInfo.StartPos;
  Measurer.MeasureSectionMark(ABoxInfo);
  Result := ABoxInfo;
end;

function TdxCharacterFormatterStateBase.FinishParagraph: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateParagraphMarkBoxInfo;
  try
    AddBox(TdxParagraphMarkBox, ABoxInfo, True);
    Result := TdxCompleteFormattingResult.Success;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

procedure TdxCharacterFormatterStateBase.FinishParagraphCore(AIterator: TdxParagraphIteratorBase);
begin
end;

function TdxCharacterFormatterStateBase.FinishSection: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateSectionMarkBoxInfo;
  try
    AddBox(TdxSectionMarkBox, ABoxInfo, True);
    Result := TdxCompleteFormattingResult.Success;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxCharacterFormatterStateBase.GetCharacterFormatterState: TdxCharacterFormatterState;
begin
  Result := &Type;
end;

function TdxCharacterFormatterStateBase.GetFormattingComplete: Boolean;
begin
  Result := False;
end;

function TdxCharacterFormatterStateBase.GetIterator: TdxParagraphIteratorBase;
begin
  Result := Formatter.Iterator;
end;

function TdxCharacterFormatterStateBase.GetDashState: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.FirstDash;
end;

function TdxCharacterFormatterStateBase.GetIteratorClass: TdxIteratorClass;
begin
  Result := TdxParagraphCharacterIterator;
end;

function TdxCharacterFormatterStateBase.GetMeasurer: TdxBoxMeasurer;
begin
  Result := Formatter.Measurer;
end;

function TdxCharacterFormatterStateBase.GetNextState: TdxCharacterFormatterState;
var
  ACurrentRun: TdxTextRunBase;
begin
  ACurrentRun := Formatter.PieceTable.Runs[Iterator.RunIndex];
  if IsInlineObjectRun(ACurrentRun) then
    Result := TdxCharacterFormatterState.InlineObject
  else
    if IsSeparatorRun(ACurrentRun) then
      Result := TdxCharacterFormatterState.Separator
    else
      if IsFloatingObjectRun(ACurrentRun) then
        Result := TdxCharacterFormatterState.FloatingObject
      else
        if IsLayoutDependentTextRun(ACurrentRun) then
          Result := TdxCharacterFormatterState.LayoutDependentText
        else
          case Iterator.CurrentChar of
            TdxCharacters.ParagraphMark:
              begin
                if IsParagraphMarkRun(ACurrentRun) then
                begin
                  FinishParagraph;
                  Result := TdxCharacterFormatterState.Final;
                end
                else
                  Result := TdxCharacterFormatterState.Text;
              end;
            TdxCharacters.SectionMark:
              begin
                if IsParagraphMarkRun(ACurrentRun) then
                begin
                  FinishSection;
                  Result := TdxCharacterFormatterState.Final;
                end
                else
                  Result := TdxCharacterFormatterState.Text;
              end;
            TdxCharacters.Space,
            TdxCharacters.EmSpace,
            TdxCharacters.EnSpace:
    					Result := TdxCharacterFormatterState.Spaces;
            TdxCharacters.Dash,
            TdxCharacters.EmDash,
            TdxCharacters.EnDash:
              Result := DashState;
            TdxCharacters.TabMark:
              Result := TdxCharacterFormatterState.Tab;
            TdxCharacters.LineBreak:
              Result := TdxCharacterFormatterState.LineBreak;
            TdxCharacters.PageBreak:
              begin
              if Paragraph.IsInCell then
                Result := TdxCharacterFormatterState.Spaces
              else
                Result := TdxCharacterFormatterState.PageBreak;
              end;
            TdxCharacters.ColumnBreak:
              begin
              if Paragraph.IsInCell then
                Result := TdxCharacterFormatterState.Spaces
              else
                Result := TdxCharacterFormatterState.ColumnBreak;
              end;
            else
              Result := TdxCharacterFormatterState.Text;
          end;
end;

function TdxCharacterFormatterStateBase.GetParagraph: TdxParagraph;
begin
  Result := TdxParagraphCharacterIterator(Iterator).Paragraph;
end;

function TdxCharacterFormatterStateBase.IsFloatingObjectRun(ARun: TdxTextRunBase): Boolean;
begin
  Result := ARun is TdxFloatingObjectAnchorRun;
end;

function TdxCharacterFormatterStateBase.IsInlineObjectRun(ARun: TdxTextRunBase): Boolean;
begin
  Result := Supports(ARun, IdxInlineObjectRun);
end;

function TdxCharacterFormatterStateBase.IsLayoutDependentTextRun(ARun: TdxTextRunBase): Boolean;
begin
  Result := ARun is TdxLayoutDependentTextRun;
end;

function TdxCharacterFormatterStateBase.IsParagraphMarkRun(ARun: TdxTextRunBase): Boolean;
begin
  Result := ARun is TdxParagraphRun;
end;

function TdxCharacterFormatterStateBase.IsSeparatorRun(ARun: TdxTextRunBase): Boolean;
begin
  Result := ARun is TdxSeparatorTextRun;
end;

procedure TdxCharacterFormatterStateBase.SwitchToNextState;
var
  ANextState: TdxCharacterFormatterState;
begin
  ANextState := GetNextState;
  ChangeState(ANextState);
end;

{ TdxParagraphFormatterBase }

constructor TdxParagraphFormatterBase<TState>.Create(APieceTable: TdxPieceTable; AMeasurer: TdxBoxMeasurer);
begin
  inherited Create;
  Assert(APieceTable <> nil, 'pieceTable = nil');
  Assert(AMeasurer <> nil, 'measurer = nil');
  FPieceTable := APieceTable;
  FMeasurer := AMeasurer;
  CreateStates;
end;

destructor TdxParagraphFormatterBase<TState>.Destroy;
begin
  DestroyStates;
  FreeAndNil(FIterator);
  inherited Destroy;
end;

procedure TdxParagraphFormatterBase<TState>.CreateStates;
begin
end;

procedure TdxParagraphFormatterBase<TState>.DestroyStates;
begin
  FreeAndNil(FState);
end;

function TdxParagraphFormatterBase<TState>.GetDocumentModel: TdxDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

class function TdxParagraphFormatterBase<TState>.GetSpaceBoxTemplate(ABoxInfo: TdxBoxInfo): TdxBoxClass;
begin
  if IsFormatterPositionEquals(ABoxInfo.StartPos, ABoxInfo.EndPos) then
    Result := TdxSingleSpaceBox
  else
    Result := TdxSpaceBoxa;
end;

procedure TdxParagraphFormatterBase<TState>.OnNewMeasurementAndDrawingStrategyChanged(AMeasurer: TdxBoxMeasurer);
begin
  Assert(AMeasurer <> nil);
  FMeasurer := AMeasurer;
end;

procedure TdxParagraphFormatterBase<TState>.SetIterator(const Value: TdxParagraphIteratorBase);
begin
  FreeAndNil(FIterator);
  FIterator := Value;
end;

{ TdxCharacterFormatterStartState }

function TdxCharacterFormatterStartState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  Assert(False);
  Result := TdxStateContinueFormatResult.Success;
end;

function TdxCharacterFormatterStartState.ContinueFormat: Boolean;
begin
  SwitchToNextState;
  Result := True;
end;

function TdxCharacterFormatterStartState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.Start;
end;

function TdxCharacterFormatterStartState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Assert(False);
  Result := True;
end;

procedure TdxCharacterFormatterStartState.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Assert(False);
end;

{ TdxCharacterFormatterSpacesState }

function TdxCharacterFormatterSpacesState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  AppendBoxCore(Formatter.GetSpaceBoxTemplate(ABoxInfo), ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

function TdxCharacterFormatterSpacesState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.Spaces;
end;

{ TdxCharacterFormatterTextState }

function TdxCharacterFormatterTextState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  if Paragraph.PieceTable.Runs[ABoxInfo.StartPos.RunIndex] is TdxSpecialTextRun then
    AppendBoxCore(TdxSpecialTextBox, ABoxInfo)
  else
    AppendBoxCore(TdxTextBox, ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

function TdxCharacterFormatterTextState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.Text;
end;

function TdxCharacterFormatterTextState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := TdxCharacters.IsCharSpace(ACh) or (ACh = TdxCharacters.TabMark) or (ACh = TdxCharacters.LineBreak) or
    (ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak) or TdxCharacters.IsCharDash(ACh);
end;

procedure TdxCharacterFormatterTextState.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Measurer.MeasureText(ABoxInfo);
end;

function TdxCharacterFormatterTextState.MaxTextLengthExceeded(ABox: TdxBoxInfo): Boolean;
begin
  Result := Iterator.Offset - ABox.StartPos.Offset > MaxTextLength;
end;

{ TdxCharacterFormatterDashState }

function TdxCharacterFormatterDashState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  AppendBoxCore(TdxTextBox, ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

function TdxCharacterFormatterDashState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.Dash;
end;

function TdxCharacterFormatterDashState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := not TdxCharacters.IsCharDash(ACh);
end;

procedure TdxCharacterFormatterDashState.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Measurer.MeasureText(ABoxInfo);
end;

{ TdxCharacterFormatterLayoutDependentTextState }

function TdxCharacterFormatterLayoutDependentTextState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.LayoutDependentText;
end;

function TdxCharacterFormatterLayoutDependentTextState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := TdxCharacters.IsCharSpace(ACh) or (ACh = TdxCharacters.TabMark) or (ACh = TdxCharacters.LineBreak) or
     (ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak);
end;

function TdxCharacterFormatterLayoutDependentTextState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  AppendBoxCore(TdxLayoutDependentTextBox, ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

procedure TdxCharacterFormatterLayoutDependentTextState.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Measurer.MeasureText(ABoxInfo);
end;

{ TdxCharacterFormatterInlineObjectState }

function TdxCharacterFormatterInlineObjectState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.InlineObject;
end;

function TdxCharacterFormatterInlineObjectState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := TdxCharacters.IsCharSpace(ACh) or (ACh = TdxCharacters.TabMark) or (ACh = TdxCharacters.LineBreak) or
     (ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak) or TdxCharacters.IsCharDash(ACh);
end;

function TdxCharacterFormatterInlineObjectState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
var
  AInlineObjectRun: IdxInlineObjectRun;
begin
  if not Supports(Formatter.PieceTable.Runs[ABoxInfo.StartPos.RunIndex], IdxInlineObjectRun, AInlineObjectRun) then
    Assert(False);
  AppendBoxCore(AInlineObjectRun.GetBoxClassType, ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

procedure TdxCharacterFormatterInlineObjectState.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
var
  ARun: TdxTextRunBase;
begin
  ARun := Formatter.PieceTable.Runs[ABoxInfo.StartPos.RunIndex];
  ARun.Measure(ABoxInfo, Measurer);
end;

{ TdxCharacterFormatterFloatingObjectState }

function TdxCharacterFormatterFloatingObjectState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.InlineObject;
end;

function TdxCharacterFormatterFloatingObjectState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := TdxCharacters.IsCharSpace(ACh) or (ACh = TdxCharacters.TabMark) or (ACh = TdxCharacters.LineBreak) or
    (ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak) or TdxCharacters.IsCharDash(ACh);
end;

function TdxCharacterFormatterFloatingObjectState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  AppendBoxCore(TdxFloatingObjectAnchorBox, ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

procedure TdxCharacterFormatterFloatingObjectState.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  ABoxInfo.Size := cxNullSize;
end;

{ TdxCharacterFormatterSeparatorState }

function TdxCharacterFormatterSeparatorState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.Separator;
end;

function TdxCharacterFormatterSeparatorState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := TdxCharacters.IsCharSpace(ACh) or (ACh = TdxCharacters.TabMark) or (ACh = TdxCharacters.LineBreak) or
    (ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak) or TdxCharacters.IsCharDash(ACh);
end;

function TdxCharacterFormatterSeparatorState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  AppendBoxCore(TdxSeparatorBox, ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

procedure TdxCharacterFormatterSeparatorState.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
var
  ARun: TdxTextRunBase;
begin
  ARun := Formatter.PieceTable.Runs[ABoxInfo.StartPos.RunIndex];
  Assert(ARun is TdxSeparatorTextRun);
  ARun.Measure(ABoxInfo, Measurer);
end;

{ TdxCharacterFormatterTabState }

function TdxCharacterFormatterTabState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.Tab;
end;

function TdxCharacterFormatterTabState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := True;
end;

function TdxCharacterFormatterTabState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  AppendBoxCore(TdxTabSpaceBox, ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

procedure TdxCharacterFormatterTabState.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Measurer.MeasureTab(ABoxInfo);
end;

{ TdxCharacterFormatterLineBreak }

function TdxCharacterFormatterLineBreak.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.LineBreak;
end;

function TdxCharacterFormatterLineBreak.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := True;
end;

function TdxCharacterFormatterLineBreak.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  AppendBoxCore(TdxLineBreakBox, ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

procedure TdxCharacterFormatterLineBreak.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Measurer.MeasureLineBreakMark(ABoxInfo);
end;

{ TdxCharacterFormatterColumnBreak }

function TdxCharacterFormatterColumnBreak.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.ColumnBreak;
end;

function TdxCharacterFormatterColumnBreak.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := True;
end;

function TdxCharacterFormatterColumnBreak.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  AppendBoxCore(TdxColumnBreakBox, ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

procedure TdxCharacterFormatterColumnBreak.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Measurer.MeasureColumnBreakMark(ABoxInfo);
end;

{ TdxCharacterFormatterFinalState }

function TdxCharacterFormatterFinalState.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  Assert(False);
  Result := TdxStateContinueFormatResult.Success;
end;

function TdxCharacterFormatterFinalState.ContinueFormat: Boolean;
begin
  Assert(False);
  Result := True;
end;

function TdxCharacterFormatterFinalState.GetFormattingComplete: Boolean;
begin
  Result := True;
end;

function TdxCharacterFormatterFinalState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.Final;
end;

function TdxCharacterFormatterFinalState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Assert(False);
  Result := True;
end;

procedure TdxCharacterFormatterFinalState.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Assert(False);
end;

{ TdxCharacterFormatterPageBreak }

function TdxCharacterFormatterPageBreak.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.PageBreak;
end;

function TdxCharacterFormatterPageBreak.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := True;
end;

function TdxCharacterFormatterPageBreak.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
begin
  AppendBoxCore(TdxPageBreakBox, ABoxInfo);
  Result := TdxStateContinueFormatResult.Success;
end;

procedure TdxCharacterFormatterPageBreak.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Measurer.MeasurePageBreakMark(ABoxInfo);
end;

{ TdxParagraphCharacterFormatter }

constructor TdxParagraphCharacterFormatter.Create(APieceTable: TdxPieceTable; AMeasurer: TdxBoxMeasurer);
begin
  inherited Create(APieceTable, AMeasurer);
  FTextBoxes := TdxBoxList.Create;
  FTextBoxInfos := TdxBoxInfoList.Create;
end;

destructor TdxParagraphCharacterFormatter.Destroy;
begin
  FreeAndNil(FTextBoxInfos);
  FreeAndNil(FTextBoxes);
  inherited Destroy;
end;

procedure TdxParagraphCharacterFormatter.CreateStates;
const
  StateClasses: array[TdxCharacterFormatterState] of TdxCharacterFormatterStateBaseClass = (
    TdxCharacterFormatterStartState,
    TdxCharacterFormatterSpacesState,
    TdxCharacterFormatterTabState,
    TdxCharacterFormatterFirstDashState,
    TdxCharacterFormatterDashState,
    TdxCharacterFormatterSeparatorState,
    TdxCharacterFormatterLineBreak,
    TdxCharacterFormatterPageBreak,
    TdxCharacterFormatterColumnBreak,
    TdxCharacterFormatterTextState,
    TdxCharacterFormatterInlineObjectState,
    TdxCharacterFormatterLayoutDependentTextState,
    TdxCharacterFormatterFloatingObjectState,
    TdxCharacterFormatterFinalState
  );
var
  I: TdxCharacterFormatterState;
begin
  for I := Low(TdxCharacterFormatterState) to High(TdxCharacterFormatterState) do
    FStates[I] := StateClasses[I].Create(Self);
end;

procedure TdxParagraphCharacterFormatter.DestroyStates;
var
  I: TdxCharacterFormatterState;
begin
  for I := Low(TdxCharacterFormatterState) to High(TdxCharacterFormatterState) do
    FStates[I].Free;
end;

procedure TdxParagraphCharacterFormatter.FormatParagraph(AIterator: TdxParagraphIteratorBase);
begin
  FIterator := AIterator;
  BeginParagraph(True, False);
  ChangeState(GetInitialStateType(True));
  repeat
    State.ContinueFormat;
  until State.FormattingComplete;
end;

procedure TdxParagraphCharacterFormatter.AddTextBoxToQueue(ABox: TdxBox; ABoxInfo: TdxBoxInfo);
begin
  FTextBoxInfos.Add(ABoxInfo);
  FTextBoxes.Add(ABox);
end;

procedure TdxParagraphCharacterFormatter.BeginParagraph(ABeginFromParagraphStart, AParagraphFrameStarted: Boolean);
var
  AParagraph: TdxParagraph;
begin
  AParagraph := Iterator.Paragraph;
  AParagraph.BoxCollection.Clear;
  if AParagraph.IsInList then
    FormatNumberingListBoxes;
end;

procedure TdxParagraphCharacterFormatter.ChangeState(AStateType: TdxCharacterFormatterState);
begin
//  Assert(State <> FStates[AStateType]);
//  if AStateType = TdxCharacterFormatterState. then

  State := FStates[AStateType];
end;

function TdxParagraphCharacterFormatter.CreateNumberingListBox(ASeparatorChar: Char;
  const APosition: TdxFormatterPosition): TdxNumberingListBox;
begin
  if ASeparatorChar = #0000 then
    Result := TdxNumberingListBox.Create
  else
  begin
    Result := TdxNumberingListBoxWithSeparator.Create;
    Result.StartPos := APosition;
    Result.EndPos := APosition;
    TdxNumberingListBoxWithSeparator(Result).SeparatorBox := CreateSeparatorBox(ASeparatorChar, APosition);
  end;
end;

function TdxParagraphCharacterFormatter.CreateSeparatorBox(ASeparator: Char; const APosition: TdxFormatterPosition): TdxBox;
var
  ABoxInfo: TdxBoxInfo;
begin
  if ASeparator = TdxCharacters.TabMark then
  begin
    Result := TdxTabSpaceBox.Create;
    Result.Bounds.Empty;
  end
  else
  begin
    Assert(TdxCharacters.IsCharSpace(ASeparator));
    ABoxInfo := TdxBoxInfo.Create;
    try
      ABoxInfo.StartPos := APosition;
      ABoxInfo.EndPos := APosition;
      Measurer.MeasureSingleSpace(ABoxInfo);
      Result := TdxSingleSpaceBox.Create;
      Result.Bounds := TRect.CreateSize(ABoxInfo.Size);
    finally
      ABoxInfo.Free;
    end;
  end;
end;

procedure TdxParagraphCharacterFormatter.Format(AIterator: TdxParagraphCharacterIterator);
begin
  FormatParagraph(AIterator);
  MeasureTextBoxes;
end;

procedure TdxParagraphCharacterFormatter.FormatNumberingListBoxes;
var
  AParagraph: TdxParagraph;
  ABoxInfo: TdxBoxInfo;
  ANumberingList: TdxNumberingList;
  APosition: TdxFormatterPosition;
  AListLevelIndex: Integer;
  AListLevel: TdxAbstractListLevel;
  ANumberingListBox: TdxNumberingListBox;
  ANumberingListText: string;
begin
  AParagraph := Iterator.Paragraph;
  ABoxInfo := TdxNumberingListBoxInfo.Create;
  try
    ANumberingList := DocumentModel.NumberingLists[AParagraph.GetNumberingListIndex];
    APosition.Init(AParagraph.FirstRunIndex, 0, 0);
    ABoxInfo.StartPos := APosition;
    ABoxInfo.EndPos := APosition;
    ANumberingListText := AParagraph.GetNumberingListText;
    Measurer.MeasureText(ABoxInfo, ANumberingListText, AParagraph.GetNumerationFontInfo);

    AListLevelIndex := AParagraph.GetListLevelIndex;
    AListLevel := ANumberingList.Levels[AListLevelIndex];

    ANumberingListBox := CreateNumberingListBox(AListLevel.ListLevelProperties.Separator, APosition);
    ANumberingListBox.NumberingListText := ANumberingListText;
    ANumberingListBox.StartPos := APosition;
    ANumberingListBox.EndPos := APosition;
    ANumberingListBox.InitialBounds := TRect.CreateSize(ABoxInfo.Size);
    TdxParagraphBoxCollection(AParagraph.BoxCollection).NumberingListBox := ANumberingListBox;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxParagraphCharacterFormatter.GetInitialStateType(ABeginFromParagraphStart: Boolean): TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.Start;
end;

function TdxParagraphCharacterFormatter.GetIterator: TdxParagraphCharacterIterator;
begin
  Result := TdxParagraphCharacterIterator(inherited Iterator);
end;

function TdxParagraphCharacterFormatter.GetIteratorClass: TdxIteratorClass;
begin
  Result := TdxParagraphCharacterIterator;
end;

procedure TdxParagraphCharacterFormatter.MeasureTextBoxes;
var
  I, ACount: Integer;
  ARuns: TdxTextRunCollection;
  ABoxInfo: TdxBoxInfo;
  ARun: TdxTextRunBase;
begin
  ACount := FTextBoxInfos.Count;
  Assert(ACount = FTextBoxes.Count);
  ARuns := PieceTable.Runs;
  Measurer.BeginTextMeasure;
  try
    for I := 0 to ACount - 1 do
    begin
      ABoxInfo := FTextBoxInfos[I];
      ARun := ARuns[ABoxInfo.StartPos.RunIndex];
      ARun.Measure(ABoxInfo, Measurer);
    end;
  finally
    Measurer.EndTextMeasure;
  end;
  for I := 0 to ACount - 1 do
  begin
    ABoxInfo := FTextBoxInfos[I];
    FTextBoxes[I].Bounds := TRect.CreateSize(ABoxInfo.Size);
  end;
end;

{ TdxFormatterStateBase }

function TdxFormatterStateBase.ContinueFormatByCharacter(var ABoxInfo: TdxBoxInfo;
  ALayoutDependentTextBox: TdxLayoutDependentTextBox): TdxStateContinueFormatResult;
var
  AIterator: TdxParagraphIteratorBase;
  AAppendBoxResult: TdxStateContinueFormatResult;
  ATerminatorChar: Boolean;
begin
  AIterator := Iterator;
  repeat
    ABoxInfo.IteratorResult := AIterator.Next;
    Assert((ALayoutDependentTextBox = nil) or (ABoxInfo.IteratorResult <> TdxParagraphIteratorResult.Success));
    ABoxInfo.Box := ALayoutDependentTextBox;
    if ABoxInfo.IteratorResult <> TdxParagraphIteratorResult.Success then
    begin
      ABoxInfo.EndPos := AIterator.GetPreviousPosition;
      AAppendBoxResult := AppendBox(ABoxInfo);
      if AAppendBoxResult <> TdxStateContinueFormatResult.Success then
        Exit(AAppendBoxResult);
      FinishParagraphCore(AIterator);
      Exit(TdxStateContinueFormatResult.Success);
    end
    else
    begin
      ATerminatorChar := IsTerminatorChar(AIterator.CurrentChar);
      if ATerminatorChar or MaxTextLengthExceeded(ABoxInfo) then
      begin
        if not ATerminatorChar then
          ABoxInfo.IteratorResult := TdxParagraphIteratorResult.RunFinished;

        ABoxInfo.EndPos := AIterator.GetPreviousPosition;
        Exit(AppendBox(ABoxInfo));
      end;
    end;
  until False;
end;

function TdxFormatterStateBase.MaxTextLengthExceeded(ABox: TdxBoxInfo): Boolean;
begin
  Result := False;
end;

{ TdxParagraphIteratorBase }

constructor TdxParagraphIteratorBase.Create(AParagraph: TdxParagraph; APieceTable: TdxPieceTable;
  AVisibleTextFilter: TdxVisibleTextFilterBase);
begin
  inherited Create;
  Assert(AParagraph <> nil, 'AParagraph = nil');
  Assert(APieceTable <> nil, 'APieceTable = nil');
  Assert(AVisibleTextFilter <> nil);
  FParagraph := AParagraph;
  FPieceTable := APieceTable;
  FVisibleTextFilter := AVisibleTextFilter;
  FEndRunIndex := AVisibleTextFilter.FindVisibleParagraphRunForward(AParagraph.LastRunIndex);
  FPosition := CreatePosition;
  SetRunIndexCore(AVisibleTextFilter.FindVisibleRunForward(AParagraph.FirstRunIndex));
end;

function TdxParagraphIteratorBase.GetCurrentChar: Char;
begin
  Result := FPieceTable.TextBuffer[FRunStartIndex + Offset];
end;

function TdxParagraphIteratorBase.GetCurrentPosition: TdxFormatterPosition;
begin
  Result.Init(RunIndex, Offset, 0);
end;

function TdxParagraphIteratorBase.GetIsEnd: Boolean;
begin
  Result := RunIndex >= FEndRunIndex;
end;

function TdxParagraphIteratorBase.GetPreviousOffsetPosition: TdxFormatterPosition;
begin
  Result.Init(RunIndex, Offset - 1, 0);
end;

function TdxParagraphIteratorBase.GetPreviousPosition: TdxFormatterPosition;
begin
  if Offset = 0 then
    Result := GetPreviousVisibleRunPosition
  else
    Result := GetPreviousOffsetPosition;
end;

function TdxParagraphIteratorBase.GetPreviousVisibleRunPosition: TdxFormatterPosition;
var
  ARunIndex: TdxRunIndex;
begin
  ARunIndex := VisibleTextFilter.GetPrevVisibleRunIndex(RunIndex);
  Result.Init(ARunIndex, FPieceTable.Runs[ARunIndex].Length - 1, 0);
end;

function TdxParagraphIteratorBase.IsFloatingObjectAnchorRun: Boolean;
begin
  Result := PieceTable.Runs[RunIndex] is TdxFloatingObjectAnchorRun;
end;

function TdxParagraphIteratorBase.IsInlinePictureRun: Boolean;
begin
  Result :=  PieceTable.Runs[RunIndex] is TdxInlinePictureRun;
end;

function TdxParagraphIteratorBase.IsParagraphMarkRun: Boolean;
begin
  Result := PieceTable.Runs[RunIndex] is TdxParagraphRun;
end;

function TdxParagraphIteratorBase.IsParagraphFrame: Boolean;
var
  F9DB0D08252841399B9BC9932A1C4800: TdxMergedFrameProperties;
begin
  F9DB0D08252841399B9BC9932A1C4800 := PieceTable.Runs[RunIndex].Paragraph.GetMergedFrameProperties;
  try
    Result := (F9DB0D08252841399B9BC9932A1C4800 <> nil) and F9DB0D08252841399B9BC9932A1C4800.IsParagraphFrame;
  finally
    F9DB0D08252841399B9BC9932A1C4800.Free;
  end;
end;

function TdxParagraphIteratorBase.Next: TdxParagraphIteratorResult;
begin
  if Offset < FMaxOffset then
  begin
    NextOffset;
    Exit(TdxParagraphIteratorResult.Success);
  end;
  while not IsEnd do
  begin
    NextRun;
    if VisibleTextFilter.IsRunVisible(RunIndex) then
      if IsEnd then
        Exit(TdxParagraphIteratorResult.Finished)
      else
        Exit(TdxParagraphIteratorResult.RunFinished);
  end;
  Result := TdxParagraphIteratorResult.Finished;
end;

procedure TdxParagraphIteratorBase.NextOffset;
begin
  Inc(FPosition.Offset)
end;

procedure TdxParagraphIteratorBase.NextRun;
begin
  SetRunIndexCore(RunIndex + 1);
  FPosition.Offset := 0;
end;

procedure TdxParagraphIteratorBase.SetPosition(const APos: TdxFormatterPosition);
begin
  SetRunIndexCore(APos.RunIndex);
  FPosition.Offset := APos.Offset;
end;

procedure TdxParagraphIteratorBase.SetPosition(ARunIndex: TdxRunIndex; AOffset: Integer);
begin
  SetRunIndexCore(ARunIndex);
  FPosition.Offset := AOffset;
end;

procedure TdxParagraphIteratorBase.SetPositionCore(const APos: TdxFormatterPosition);
begin
  SetRunIndexCore(APos.RunIndex);
  FPosition.Offset := APos.Offset;
end;

procedure TdxParagraphIteratorBase.SetRunIndexCore(ARunIndex: TdxRunIndex);
var
  ARun: TdxTextRunBase;
begin
  FPosition.RunIndex := ARunIndex;
  ARun := FPieceTable.Runs[RunIndex];
  FMaxOffset := ARun.Length - 1;
  FRunStartIndex := ARun.StartIndex;
end;

{ TdxParagraphCharacterIterator }

function TdxParagraphCharacterIterator.CreatePosition: TdxFormatterPosition;
begin
  Result.Init(0, 0, 0);
end;

function TdxCharacterFormatterSpacesState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := not TdxCharacters.IsCharSpace(ACh);
end;

procedure TdxCharacterFormatterSpacesState.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Measurer.MeasureSpaces(ABoxInfo);
end;

{ TdxParagraphBoxFormatterTableStateBase }

constructor TdxParagraphBoxFormatterTableStateBase.Create(AFormatter: TdxParagraphBoxFormatter);
begin
  inherited Create;
  Assert(AFormatter <> nil);
  FFormatter := AFormatter;
end;

{ TdxParagraphBoxFormatterTextState }

procedure TdxParagraphBoxFormatterTextState.OnColumnOverfull;
begin
end;

{ TdxParagraphBoxFormatterTableState }

procedure TdxParagraphBoxFormatterTableState.OnColumnOverfull;
begin
end;

{ TdxBoxFormatterStateBase }

constructor TdxBoxFormatterStateBase.Create(AFormatter: TdxParagraphBoxFormatter);
begin
  inherited Create;
  Assert(AFormatter <> nil);
  FFormatter := AFormatter;
  FStateAfterFinalizePage := TdxParagraphBoxFormatterState.RowEmpty;
  FStateAfterFinalizeColumn := TdxParagraphBoxFormatterState.RowEmpty;
end;

function TdxBoxFormatterStateBase.AddExistedBox(ABoxInfo: TdxBoxInfo; AIterator: TdxParagraphBoxIterator): TdxStateContinueFormatResult;
begin
  ABoxInfo.Box := AIterator.CurrentBox;
  AIterator.NextBox;
  ABoxInfo.EndPos := AIterator.GetPreviousPosition;
  Result := AppendBox(ABoxInfo);
end;

procedure TdxBoxFormatterStateBase.AddNumberingListBox;
var
  ANumberingListBox: TdxNumberingListBox;
  AIndex: TdxNumberingListIndex;
  AListLevelIndex: Integer;
  ABoxInfo: TdxBoxInfo;
  AListLevel: TdxAbstractListLevel;
begin
  if CurrentRow.NumberingListBox <> nil then
    Exit;

  ANumberingListBox := TdxParagraphBoxCollection(Iterator.Paragraph.BoxCollection).NumberingListBox;
  AIndex := Iterator.Paragraph.GetNumberingListIndex;
  AListLevelIndex := Iterator.Paragraph.GetListLevelIndex;
  AListLevel := Iterator.Paragraph.DocumentModel.NumberingLists[AIndex].Levels[AListLevelIndex];

  ABoxInfo := TdxNumberingListBoxInfo.Create;
  try
    ABoxInfo.Box := ANumberingListBox;
    ABoxInfo.StartPos := ANumberingListBox.StartPos;
    ABoxInfo.EndPos := ANumberingListBox.EndPos;
    ABoxInfo.Size := cxSize(ANumberingListBox.InitialBounds);
    ANumberingListBox.Bounds := ANumberingListBox.InitialBounds;

    RowsController.UpdateCurrentRowHeight(ABoxInfo);
    RowsController.AddNumberingListBox(ANumberingListBox, AListLevel.ListLevelProperties, Formatter);
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

procedure TdxBoxFormatterStateBase.AddTextBox(ABoxInfo: TdxBoxInfo);
var
  ARun: TdxTextRunBase;
  AInlineObjectRun: IdxInlineObjectRun;
  ABox: TdxBox;
  ALayoutDependentTextRun: TdxLayoutDependentTextRun;
begin
  ARun := Formatter.PieceTable.Runs[ABoxInfo.StartPos.RunIndex];

  if Supports(ARun, IdxInlineObjectRun, AInlineObjectRun) then
  begin
    ABox := RowsController.AddBox(AInlineObjectRun.GetBoxClassType, ABoxInfo);
    RowsController.UpdateCurrentRowHeightFromInlineObjectRun(ARun, ABox, Formatter.Measurer);
  end
  else
  begin
    ALayoutDependentTextRun := Safe<TdxLayoutDependentTextRun>.Cast(ARun);
    if ALayoutDependentTextRun = nil then
      RowsController.AddBox(TdxTextBox, ABoxInfo)
    else
    begin
        RowsController.AddBox(TdxLayoutDependentTextBox, ABoxInfo);
        if ALayoutDependentTextRun is TdxFootNoteRun then
          RowsController.CurrentRow.ProcessingFlags := RowsController.CurrentRow.ProcessingFlags + [TdxRowProcessingFlag.ContainsFootNotes];
        if ALayoutDependentTextRun is TdxEndNoteRun then
          RowsController.CurrentRow.ProcessingFlags := RowsController.CurrentRow.ProcessingFlags + [TdxRowProcessingFlag.ContainsEndNotes];
    end;
    RowsController.UpdateCurrentRowHeight(ABoxInfo);
  end;
end;

function TdxBoxFormatterStateBase.AdjustEndPositionToFit(ABoxInfo: TdxBoxInfo): TdxAdjustEndPositionResult;
const
  SplitTextBinarySearchBounds = 300;
var
  AMaxWidth, AOffset, ALow, AHi: Integer;
  AOriginalEndPos: TdxFormatterPosition;
begin
  if ABoxInfo.StartPos = ABoxInfo.EndPos then
    Exit(TdxAdjustEndPositionResult.Failure);

  ABoxInfo.Box := nil;
  AMaxWidth := RowsController.GetMaxBoxWidth;
  AOriginalEndPos := ABoxInfo.EndPos;
  if Formatter.Measurer.TryAdjustEndPositionToFit(ABoxInfo, AMaxWidth) then
  begin
    Formatter.Measurer.MeasureText(ABoxInfo);
    if CanAddBoxWithoutHyphen(ABoxInfo) = TdxAddBoxResult.Success then
      Exit(TdxAdjustEndPositionResult.Success);

    ABoxInfo.EndPos := AOriginalEndPos;
  end;

  AOffset := 0;
  if ABoxInfo.EndPos.Offset - ABoxInfo.StartPos.Offset >= SplitTextBinarySearchBounds then
  begin
    ALow := ABoxInfo.StartPos.Offset;
    AHi := ABoxInfo.StartPos.Offset + SplitTextBinarySearchBounds;
    AOffset := BinarySearchFittedBox(ABoxInfo, ALow, AHi);
    if (not AOffset) > AHi then
    begin
      AOffset := 0;
      ABoxInfo.EndPos := AOriginalEndPos;
    end;
  end;
  if AOffset = 0 then
    AOffset := BinarySearchFittedBox(ABoxInfo);
  Assert(AOffset < 0);
  AOffset := (not AOffset) - 1;
  if AOffset < ABoxInfo.StartPos.Offset then
  begin
    ABoxInfo.EndPos := TdxFormatterPosition.Create(ABoxInfo.EndPos.RunIndex, ABoxInfo.StartPos.Offset, ABoxInfo.EndPos.BoxIndex);

    Exit(TdxAdjustEndPositionResult.Failure);
  end
  else
  begin
    ABoxInfo.EndPos := TdxFormatterPosition.Create(ABoxInfo.EndPos.RunIndex, AOffset, ABoxInfo.EndPos.BoxIndex);

    Formatter.Measurer.MeasureText(ABoxInfo);
    Exit(TdxAdjustEndPositionResult.Success);
  end;
end;

function TdxBoxFormatterStateBase.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := &Type;
end;

function TdxBoxFormatterStateBase.AppendBox(var ABoxInfo: TdxBoxInfo): TdxStateContinueFormatResult;
var
  ABoxResult: TdxAddBoxResult;
begin
  MeasureBoxContentCore(ABoxInfo);
  ABoxResult := ObtainAddBoxResult(ABoxInfo);
  Result := DispatchAddBoxResult(ABoxInfo, ABoxResult);
end;

procedure TdxBoxFormatterStateBase.ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert(CurrentRow.Height <> 0);
end;

procedure TdxBoxFormatterStateBase.ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert(CurrentRow.Height <> 0);
end;

procedure TdxBoxFormatterStateBase.ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert(CurrentRow.Height <> 0);
end;

procedure TdxBoxFormatterStateBase.ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert(CurrentRow.Height <> 0);
end;

function TdxBoxFormatterStateBase.BinarySearchFittedBox(ABoxInfo: TdxBoxInfo): Integer;
begin
  Result := BinarySearchFittedBox(ABoxInfo, ABoxInfo.StartPos.Offset, ABoxInfo.EndPos.Offset);
end;

function TdxBoxFormatterStateBase.BinarySearchFittedBox(ABoxInfo: TdxBoxInfo; ALow, AHi: Integer): Integer;
var
  AEndPos: TdxFormatterPosition;
  AMedian: Integer;
begin
  AEndPos := ABoxInfo.EndPos;
  ALow := ABoxInfo.StartPos.Offset;
  AHi  := ABoxInfo.EndPos.Offset;
  while ALow <= AHi do
  begin
    AMedian := ALow + ((AHi - ALow) shr 1);
    ABoxInfo.EndPos.Init(AEndPos.RunIndex, AMedian, AEndPos.BoxIndex);
    Formatter.Measurer.MeasureText(ABoxInfo);
    if CanAddBoxWithoutHyphen(ABoxInfo) = TdxAddBoxResult.Success then
      ALow := AMedian + 1
    else
      AHi := AMedian - 1;
  end;
  Result := not ALow;
end;

function TdxBoxFormatterStateBase.CalcBoxSizeWithHyphen(ABoxInfo: TdxBoxInfo): TSize;
begin
  Result := ABoxInfo.Size;
  Result.cx := Result.cx + Formatter.Measurer.MeasureHyphen(ABoxInfo.EndPos, nil).cx;
end;

procedure TdxBoxFormatterStateBase.CalculateAndMeasureLayoutDependentTextBox(
  ALayoutDependentTextBox: TdxLayoutDependentTextBox; ABoxInfo: TdxBoxInfo);
var
  ARun: TdxLayoutDependentTextRun;
begin
  ARun := TdxLayoutDependentTextRun(Formatter.PieceTable.Runs[ALayoutDependentTextBox.StartPos.RunIndex]);
  ALayoutDependentTextBox.CalculatedText := ARun.FieldResultFormatting.GetValue(Formatter, Formatter.DocumentModel);
  MeasureBoxContent(ABoxInfo);
end;

function TdxBoxFormatterStateBase.CanAddBoxCore(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);

  if ACanFit <> TdxCanFitCurrentRowToColumnResult.RowFitted then
  begin
    if ACanFit = TdxCanFitCurrentRowToColumnResult.TextAreasRecreated then
      Exit(TdxAddBoxResult.IntersectWithFloatObject)
    else
      if ACanFit = TdxCanFitCurrentRowToColumnResult.RestartDueFloatingObject then
        Exit(TdxAddBoxResult.RestartDueFloatingObject);
    Exit(GetAddBoxResultColumnOverfull(ACanFit));
  end;

  if not RowsController.CanFitBoxToCurrentRow(ABoxInfo.Size) then
    Exit(TdxAddBoxResult.HorizontalIntersect);
  Result := TdxAddBoxResult.Success;
end;

function TdxBoxFormatterStateBase.CanAddBoxWithHyphenCore(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
  ABoxSize: TSize;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit <> TdxCanFitCurrentRowToColumnResult.RowFitted then
  begin
    if ACanFit = TdxCanFitCurrentRowToColumnResult.TextAreasRecreated then
      Exit(TdxAddBoxResult.IntersectWithFloatObject)
    else
      if ACanFit = TdxCanFitCurrentRowToColumnResult.RestartDueFloatingObject then
        Exit(TdxAddBoxResult.RestartDueFloatingObject);
    Exit(GetAddBoxResultColumnOverfull(ACanFit));
  end;

  ABoxSize := CalcBoxSizeWithHyphen(ABoxInfo);
  if not RowsController.CanFitBoxToCurrentRow(ABoxSize) then
    Result := TdxAddBoxResult.HorizontalIntersect
  else
    Result := TdxAddBoxResult.Success;
end;

function TdxBoxFormatterStateBase.GetContinueFormatResult(AResult: TdxCompleteFormattingResult): TdxStateContinueFormatResult;
begin
  if AResult = TdxCompleteFormattingResult.Success then
    Result := TdxStateContinueFormatResult.Success
  else
    Result := TdxStateContinueFormatResult.RestartDueOrphanedObjects;
end;

function TdxBoxFormatterStateBase.CanAddBoxWithoutHyphen(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  Result := CanAddBox(ABoxInfo);
end;

function TdxBoxFormatterStateBase.CanInsertNumberingListBox: Boolean;
var
  ACh: Char;
begin
  ACh := Iterator.CurrentChar;
  Result := (ACh <> TdxCharacters.PageBreak) and (ACh <> TdxCharacters.ColumnBreak);
end;

procedure TdxBoxFormatterStateBase.ChangeState(AStateType: TdxParagraphBoxFormatterState);
begin
  Formatter.ChangeState(AStateType);
end;

function TdxBoxFormatterStateBase.ContinueFormat: TdxStateContinueFormatResult;
var
  ABoxInfo: TdxBoxInfo;
  AAddExistingBoxResult: TdxStateContinueFormatResult;
begin
  ABoxInfo := TdxBoxInfo.Create;
  try
    ABoxInfo.StartPos := Iterator.GetCurrentPosition;
    if CanUseBox and (ABoxInfo.StartPos = Iterator.CurrentBox.StartPos) then
    begin
      AAddExistingBoxResult := AddExistedBox(ABoxInfo, Iterator);
      if AAddExistingBoxResult <> TdxStateContinueFormatResult.Success then
        Exit(AAddExistingBoxResult);
      if Iterator.IsEnd then
      begin
        if Iterator.IsParagraphFrame and (Self is TdxStateFloatingObject) then
          Exit(TdxStateContinueFormatResult.Success);
        if (FFormatter.SyllableIterator = nil) or FFormatter.SyllableIterator.IsEnd then
          Exit(GetContinueFormatResult(FinishParagraphOrSection));
      end;
      Exit(TdxStateContinueFormatResult.Success);
    end;
    Result := ContinueFormatByCharacter(ABoxInfo, Safe<TdxLayoutDependentTextBox>.Cast(Iterator.CurrentBox))
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxBoxFormatterStateBase.CreateColumnBreakBoxInfo: TdxBoxInfo;
begin
  Result := TdxBoxInfo.Create;
  Result.StartPos := Iterator.GetCurrentPosition;
  Result.EndPos := Result.StartPos;
  Formatter.Measurer.MeasureColumnBreakMark(Result);
end;

function TdxBoxFormatterStateBase.CreateLineBreakBoxInfo: TdxBoxInfo;
begin
  Result := TdxBoxInfo.Create;
  Result.StartPos := Iterator.GetCurrentPosition;
  Result.EndPos := Result.StartPos;
  Formatter.Measurer.MeasureLineBreakMark(Result);
end;

function TdxBoxFormatterStateBase.CreatePageBreakBoxInfo: TdxBoxInfo;
begin
  Result := TdxBoxInfo.Create;
  Result.StartPos := Iterator.GetCurrentPosition;
  Result.EndPos := Result.StartPos;
  Formatter.Measurer.MeasurePageBreakMark(Result);
end;

function TdxBoxFormatterStateBase.CreateParagraphMarkBoxInfo: TdxBoxInfo;
begin
  Result := TdxBoxInfo.Create;
  Result.Box := Iterator.CurrentBox;
  Result.StartPos := Iterator.GetCurrentPosition();
  Result.EndPos := Result.StartPos;
  Formatter.Measurer.MeasureParagraphMark(Result);
end;

function TdxBoxFormatterStateBase.CreateSectionMarkBoxInfo: TdxBoxInfo;
begin
  Result := TdxBoxInfo.Create;
  Result.Box := Iterator.CurrentBox;
  Result.StartPos := Iterator.GetCurrentPosition();
  Result.EndPos := Result.StartPos;
  Formatter.Measurer.MeasureSectionMark(Result);
end;

function TdxBoxFormatterStateBase.DispatchAddBoxResult(ABoxInfo: TdxBoxInfo; AResult: TdxAddBoxResult): TdxStateContinueFormatResult;
begin
  case AResult of
    TdxAddBoxResult.Success:
      AddSuccess(ABoxInfo);
    TdxAddBoxResult.HorizontalIntersect:
      Exit(GetContinueFormatResult(HorizontalOverfull(ABoxInfo)));
    TdxAddBoxResult.LeaveColumnFirstCellRow,
    TdxAddBoxResult.LeaveColumnPlain:
      Exit(GetContinueFormatResult(ColumnOverfull(GetCanFitCurrentRowToColumn(AResult), ABoxInfo)));
    TdxAddBoxResult.IntersectWithFloatObject:
      Exit(GetContinueFormatResult(Formatter.RollbackToStartOfRow(TdxCanFitCurrentRowToColumnResult.TextAreasRecreated)));
    TdxAddBoxResult.RestartDueFloatingObject:
      Exit(TdxStateContinueFormatResult.RestartDueFloatingObject);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
  Result := TdxStateContinueFormatResult.Success;
end;

procedure TdxBoxFormatterStateBase.EndRow;
var
  ACurrentBottom: Integer;
begin
  Formatter.RollbackUnapprovedFloatingObjects;
  Formatter.RowsController.EndRow(Formatter);
  ACurrentBottom := Formatter.RowsController.CurrentRow.Bounds.Bottom;
  if (ACurrentBottom >= Formatter.MaxHeight) and not Iterator.Paragraph.IsInCell and
    (Iterator.Paragraph.FrameProperties = nil) then
    ForceFormattingComplete := True;
end;

function TdxBoxFormatterStateBase.FinalizeColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  if not Formatter.Iterator.Paragraph.IsInCell and RowsController.SupportsColumnAndPageBreaks then
  begin
    Result := RowsController.CompleteCurrentColumnFormatting;
    if Result <> TdxCompleteFormattingResult.Success then
      Exit;
    RowsController.AddBox(TdxColumnBreakBox, ABoxInfo);
    Formatter.ApproveFloatingObjects;
    EndRow;
    RowsController.MoveRowToNextColumn;
    ChangeState(StateAfterFinalizeColumn);
  end
  else
  begin
    if CurrentRow.Boxes.Count > 0 then
      FinalizeLine(ABoxInfo)
    else
      ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
  end;
  Result := TdxCompleteFormattingResult.Success;
end;

procedure TdxBoxFormatterStateBase.FinalizeLine(ABoxInfo: TdxBoxInfo);
begin
  RowsController.AddBox(TdxLineBreakBox, ABoxInfo);
  Formatter.ApproveFloatingObjects;
  EndRow;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
end;

function TdxBoxFormatterStateBase.FinalizePage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  RowsController.AddBox(TdxPageBreakBox, ABoxInfo);
  Formatter.ApproveFloatingObjects;

  EndRow;
  Result := RowsController.CompleteCurrentColumnFormatting;
  if Result <> TdxCompleteFormattingResult.Success then
    Exit;
  RowsController.MoveRowToNextPage;
  ChangeState(StateAfterFinalizePage);
  Result := TdxCompleteFormattingResult.Success;
end;

procedure TdxBoxFormatterStateBase.FinalizeParagraph(ABoxInfo: TdxBoxInfo);
begin
  RowsController.AddBox(TdxParagraphMarkBox, ABoxInfo);
  Formatter.ApproveFloatingObjects;
  EndRow;
  ChangeState(TdxParagraphBoxFormatterState.Final);
end;

procedure TdxBoxFormatterStateBase.FinalizeSection(ABoxInfo: TdxBoxInfo);
begin
  RowsController.AddBox(TdxSectionMarkBox, ABoxInfo);
  Formatter.ApproveFloatingObjects;
  EndRow;
  ChangeState(TdxParagraphBoxFormatterState.Final);
end;

function TdxBoxFormatterStateBase.FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := FinalizeColumn(ABoxInfo);
end;

function TdxBoxFormatterStateBase.FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  FinalizeLine(ABoxInfo);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxBoxFormatterStateBase.FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := FinalizePage(ABoxInfo);
end;

function TdxBoxFormatterStateBase.FinishParagraph: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateParagraphMarkBoxInfo;
  try
    ApplyParagraphMarkSize(ABoxInfo);
    FinalizeParagraph(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

procedure TdxBoxFormatterStateBase.FinishParagraphCore(AIterator: TdxParagraphIteratorBase);
begin
  if AIterator.IsEnd then
    if (FFormatter.SyllableIterator = nil) or FFormatter.SyllableIterator.IsEnd then
      FinishParagraph;
end;

function TdxBoxFormatterStateBase.FinishParagraphOrSection: TdxCompleteFormattingResult;
begin
  if Iterator.CurrentChar = TdxCharacters.ParagraphMark then
    Result := FinishParagraph
  else
    Result := FinishSection;
end;

function TdxBoxFormatterStateBase.FinishSection: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateSectionMarkBoxInfo;
  try
    ApplyParagraphMarkSize(ABoxInfo);
    FinalizeSection(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxBoxFormatterStateBase.GetAddBoxResultColumnOverfull(
  ACanFit: TdxCanFitCurrentRowToColumnResult): TdxAddBoxResult;
begin
  case ACanFit of
    TdxCanFitCurrentRowToColumnResult.PlainRowNotFitted:
      Result := TdxAddBoxResult.LeaveColumnPlain;
    TdxCanFitCurrentRowToColumnResult.FirstCellRowNotFitted:
      Result := TdxAddBoxResult.LeaveColumnFirstCellRow;
  else
    TdxRichEditExceptions.ThrowInternalException;
    Result := TdxAddBoxResult.LeaveColumnPlain;
  end;
end;

function TdxBoxFormatterStateBase.GetCanFitCurrentRowToColumn(
  AAddBoxResult: TdxAddBoxResult): TdxCanFitCurrentRowToColumnResult;
begin
  case AAddBoxResult of
    TdxAddBoxResult.LeaveColumnPlain:
      Result := TdxCanFitCurrentRowToColumnResult.PlainRowNotFitted;
    TdxAddBoxResult.LeaveColumnFirstCellRow:
      Result := TdxCanFitCurrentRowToColumnResult.FirstCellRowNotFitted;
    else
    begin
      Assert(False);
      Result := TdxCanFitCurrentRowToColumnResult.RowFitted;
    end;
  end;
end;

function TdxBoxFormatterStateBase.CanUseBox: Boolean;
begin
  Result := False;
end;

function TdxBoxFormatterStateBase.GetIteratorClass: TdxIteratorClass;
begin
  Result := TdxParagraphBoxIterator;
end;

function TdxBoxFormatterStateBase.GetCurrentRow: TdxRow;
begin
  Result := Formatter.CurrentRow;
end;

function TdxBoxFormatterStateBase.GetForceFormattingComplete: Boolean;
begin
  Result := Formatter.ForceFormattingComplete;
end;

function TdxBoxFormatterStateBase.GetFormattingComplete: Boolean;
begin
  Result := ForceFormattingComplete;
end;

function TdxBoxFormatterStateBase.GetIterator: TdxParagraphIteratorBase;
begin
  Result := Formatter.Iterator;
end;

function TdxBoxFormatterStateBase.GetIterator_: TdxParagraphBoxIterator;
begin
  Result := TdxParagraphBoxIterator(GetIterator);
end;

function TdxBoxFormatterStateBase.GetMeasurer: TdxBoxMeasurer;
begin
  Result := Formatter.Measurer;
end;

function TdxBoxFormatterStateBase.GetRowsController: TdxRowsController;
begin
  Result := FFormatter.RowsController;
end;

function TdxBoxFormatterStateBase.IsCurrentRowEmpty: Boolean;
begin
  Result := RowsController.CurrentRow.Boxes.Count <= 0;
end;

procedure TdxBoxFormatterStateBase.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Formatter.Measurer.MeasureText(ABoxInfo);
end;

procedure TdxBoxFormatterStateBase.MeasureBoxContentCore(ABoxInfo: TdxBoxInfo);
var
  ABox: TdxBox;
  ALayoutDependentTextBox: TdxLayoutDependentTextBox;
begin
  ABox := TdxBox(ABoxInfo.Box);
  ALayoutDependentTextBox := nil;
  if ABox is TdxLayoutDependentTextBox then
    ALayoutDependentTextBox := TdxLayoutDependentTextBox(ABox);
  if (ABox <> nil) and (ALayoutDependentTextBox = nil) then
    ABoxInfo.Size := ABox.Bounds.Size
  else
    if ALayoutDependentTextBox <> nil then
      CalculateAndMeasureLayoutDependentTextBox(ALayoutDependentTextBox, ABoxInfo)
    else
      MeasureBoxContent(ABoxInfo);
end;

function TdxBoxFormatterStateBase.MaxTextLengthExceeded(ABox: TdxBoxInfo): Boolean;
begin
  Result := Iterator.Offset - ABox.StartPos.Offset > MaxTextLength;
end;

function TdxBoxFormatterStateBase.ObtainAddBoxResult(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  if Iterator.CurrentChar = TdxCharacters.Hyphen then
    Result := CanAddBox(ABoxInfo)
  else
    Result := CanAddBoxWithoutHyphen(ABoxInfo);
end;

procedure TdxBoxFormatterStateBase.SetCurrentRowHeightToLastBoxHeight;
var
  ACurrentRow: TdxRow;
  ALastBox: TdxBox;
  ABoxInfo: TdxBoxInfo;
begin
  ACurrentRow := CurrentRow;
  Assert(ACurrentRow.Height <= RowsController.DefaultRowHeight);
  Assert(ACurrentRow.Boxes.Count > 0);
  ALastBox := ACurrentRow.Boxes.Last;
  ABoxInfo := TdxBoxInfo.Create;
  try
    ABoxInfo.StartPos := ALastBox.StartPos;
    ABoxInfo.EndPos := ALastBox.EndPos;
    ABoxInfo.Size := ALastBox.Bounds.Size;
    ABoxInfo.Box := ALastBox;
    RowsController.UpdateCurrentRowHeight(ABoxInfo);
  finally
    ABoxInfo.Free;
  end;
end;

procedure TdxBoxFormatterStateBase.SetForceFormattingComplete(const Value: Boolean);
begin
  Formatter.ForceFormattingComplete := Value;
end;

function TdxBoxFormatterStateBase.SplitBox(ABoxInfo: TdxBoxInfo): TdxSplitBoxResult;
var
  AStartPos: TdxFormatterPosition;
  AIterator: TdxParagraphBoxIterator;
begin
  AStartPos := ABoxInfo.StartPos;

  Assert(AStartPos.RunIndex = ABoxInfo.EndPos.RunIndex);

  if AdjustEndPositionToFit(ABoxInfo) = TdxAdjustEndPositionResult.Success then
  begin
    Iterator.SetNextPosition(ABoxInfo.EndPos);
    Result := TdxSplitBoxResult.Success;
    Exit;
  end;

  AIterator := Iterator;
  AIterator.SetPosition(AStartPos);
  if IsCurrentRowEmpty then
  begin
    AIterator.Next;
    Result := TdxSplitBoxResult.SuccessSuppressedHorizontalOverfull;
  end
  else
    Result := TdxSplitBoxResult.FailedHorizontalOverfull;
end;

{ TdxCurrentHorizontalPositionController }

constructor TdxCurrentHorizontalPositionController.Create(ARowsController: TdxRowsController; APosition: Integer);
begin
  inherited Create;
  Assert(ARowsController <> nil);
  FRowsController := ARowsController;
  FCurrentHorizontalPosition := APosition;
end;

constructor TdxCurrentHorizontalPositionController.Create(ARowsController: TdxRowsController);
begin
  Create(ARowsController, 0);
end;

function TdxCurrentHorizontalPositionController.CalculateBoxBounds(ABoxInfo: TdxBoxInfo): TRect;
begin
  Result.InitSize(CurrentHorizontalPosition, 0, ABoxInfo.Size);
end;

function TdxCurrentHorizontalPositionController.CalculateFloatingObjectColumnBounds(
  ACurrentColumn: TdxColumn): TRect;
begin
  Result := ACurrentColumn.Bounds;
end;

function TdxCurrentHorizontalPositionController.CanFitBoxToCurrentRow(const ABoxSize: TSize): Boolean;
begin
  Result := (CurrentHorizontalPosition + ABoxSize.cx <= FRowsController.CurrentRow.Bounds.Right) or
    FRowsController.SuppressHorizontalOverfull;
end;

function TdxCurrentHorizontalPositionController.CanFitCurrentRowToColumn(
  ANewRowHeight: Integer): TdxCanFitCurrentRowToColumnResult;
begin
  Result := RowsController.TablesController.CanFitRowToColumn(RowsController.CurrentRow.Bounds.Top + ANewRowHeight,
    RowsController.CurrentColumn);
end;

function TdxCurrentHorizontalPositionController.CanFitNumberingListBoxToCurrentRow(const ABoxSize: TSize): Boolean;
begin
  Result := True;
end;

function TdxCurrentHorizontalPositionController.GetMaxBoxWidth: Integer;
begin
  if FRowsController.SuppressHorizontalOverfull then
    Result := MaxInt div 2
  else
    Result := Max(1, FRowsController.CurrentRow.Bounds.Right - CurrentHorizontalPosition);
end;

function TdxCurrentHorizontalPositionController.GetTextAreaForTable: TdxTextArea;
var
  ABounds: TRect;
begin
  ABounds := RowsController.CurrentColumn.Bounds;
  Result := TdxTextArea.Create(ABounds.Left, ABounds.Right);
end;

procedure TdxCurrentHorizontalPositionController.IncrementCurrentHorizontalPosition(ADelta: Integer);
begin
  FCurrentHorizontalPosition := FCurrentHorizontalPosition + ADelta;
end;

function TdxCurrentHorizontalPositionController.MoveCurrentRowDownToFitTable(ATableWidth, ATableTop: Integer): Integer;
begin
  Result := ATableTop;
end;

procedure TdxCurrentHorizontalPositionController.OnCurrentRowFinished;
begin
end;

procedure TdxCurrentHorizontalPositionController.OnCurrentRowHeightChanged(AKeepTextAreas: Boolean);
begin
end;

procedure TdxCurrentHorizontalPositionController.RollbackCurrentHorizontalPositionTo(AValue: Integer);
begin
  Assert(AValue <= FCurrentHorizontalPosition);
  FCurrentHorizontalPosition := AValue;
end;

procedure TdxCurrentHorizontalPositionController.SetCurrentHorizontalPosition(AValue: Integer);
begin
  FCurrentHorizontalPosition := AValue;
end;

procedure TdxCurrentHorizontalPositionController.SetCurrentRowInitialHorizontalPosition;
begin
  FCurrentHorizontalPosition := FRowsController.ParagraphLeft + FRowsController.CurrentRowIndent;
end;

{ TdxTextAreaCollectionEx }

constructor TdxTextAreaCollectionEx.Create;
begin
  inherited Create;
  FInnerList := TList<TdxTextArea>.Create;
end;

destructor TdxTextAreaCollectionEx.Destroy;
begin
  FreeAndNil(FInnerList);
  inherited Destroy;
end;

function TdxTextAreaCollectionEx.GetCount: Integer;
begin
  Result := InnerList.Count;
end;

function TdxTextAreaCollectionEx.GetItem(Index: Integer): TdxTextArea;
begin
  Result := InnerList[Index];
end;

procedure TdxTextAreaCollectionEx.Add(const AInterval: TdxTextArea);
begin
  AddCore(AInterval);
end;

function TdxTextAreaCollectionEx.AddCore(AInterval: TdxTextArea): Integer;
var
  AToRemove: TList<TdxTextArea>;
  ACount, I: Integer;
begin
  if Contains(AInterval) then
    Exit(0);
  AToRemove := TList<TdxTextArea>.Create;
  try
    ACount := Count;
    for I := 0 to ACount - 1 do
    begin
      if AInterval.IntersectsWith(Items[I]) then
      begin
        AInterval := TdxTextArea.Union(Items[I], AInterval);
        AToRemove.Add(InnerList[I]);
      end;
    end;
    RemoveCore(AToRemove);
  finally
    AToRemove.Free;
  end;
  if AInterval.Width > 0 then
    InnerList.Add(AInterval);
  Result := Count - 1;
end;

procedure TdxTextAreaCollectionEx.RemoveCore(AToRemove: TList<TdxTextArea>);
var
  I: Integer;
begin
  for I := 0 to AToRemove.Count - 1 do
    InnerList.Remove(AToRemove[I]);
end;

procedure TdxTextAreaCollectionEx.AddCore(AToAdd: TList<TdxTextArea>);
var
  I: Integer;
begin
  for I := 0 to AToAdd.Count - 1 do
    InnerList.Add(AToAdd[I]);
end;

procedure TdxTextAreaCollectionEx.DontOwnInnerList;
begin
  FInnerList := nil;
end;

function TdxTextAreaCollectionEx.Remove(const AInterval: TdxTextArea): Boolean;
var
  AIndex, ACount, I, ASubtractResultCount, ASubtractResultIndex: Integer;
  ASubtractResult, AToRemove, AToAdd: TList<TdxTextArea>;
begin
  AIndex := InnerList.IndexOf(AInterval);
  if AIndex >= 0 then
  begin
    InnerList.Delete(AIndex);
    Exit(True);
  end;

  AToRemove := TList<TdxTextArea>.Create;
  AToAdd := TList<TdxTextArea>.Create;
  try
    ACount := Count;
    for I := 0 to ACount - 1 do
    begin
      if AInterval.IntersectsWithExcludingBounds(Items[I]) then
      begin
        AToRemove.Add(Items[I]);
        ASubtractResult := Items[I].Subtract(AInterval);
        try
          ASubtractResultCount := ASubtractResult.Count;
          for ASubtractResultIndex := 0 to ASubtractResultCount - 1 do
            AToAdd.Add(ASubtractResult[ASubtractResultIndex]);
        finally
          ASubtractResult.Free;
        end;
      end;
    end;
    RemoveCore(AToRemove);
    AddCore(AToAdd);
  finally
    AToRemove.Free;
    AToAdd.Free;
  end;

  Result := True;
end;

function TdxTextAreaCollectionEx.Contains(const AInterval: TdxTextArea): Boolean;
var
  ACount, I: Integer;
begin
  if InnerList.Contains(AInterval) then
    Exit(True);

  ACount := Count;
  for I := 0 to ACount - 1 do
    if Items[I].Contains(AInterval) then
      Exit(True);

  Result := False;
end;

procedure TdxTextAreaCollectionEx.Sort;
begin
  InnerList.Sort(TdxTextArea.StartComparer);
end;

{ TdxCharacterFormatterFirstDashState }

function TdxCharacterFormatterFirstDashState.GetDashState: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.Dash;
end;

function TdxCharacterFormatterFirstDashState.GetType: TdxCharacterFormatterState;
begin
  Result := TdxCharacterFormatterState.FirstDash;
end;

function TdxCharacterFormatterFirstDashState.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := True;
end;

{ TRectangularFrameXComparer }

function TRectangularFrameXComparer.Compare(const X, Y: TdxBoxBase): Integer;
begin
  Result := CompareInteger(TdxParagraphFrameBox(X).X, TdxParagraphFrameBox(Y).X);
end;

{ TRectangularFrameYComparer }

function TRectangularFrameYComparer.Compare(const X, Y: TdxBoxBase): Integer;
begin
  Result := CompareInteger(TdxParagraphFrameBox(X).Y, TdxParagraphFrameBox(Y).Y);
end;

{ TdxParagraphFramesLayout }

constructor TdxParagraphFramesLayout.Create;
begin
  inherited Create;
  FItems := TdxParagraphFrameBoxList.Create;
end;

destructor TdxParagraphFramesLayout.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

class constructor TdxParagraphFramesLayout.Initialize;
begin
  FHorizontalObjectComparer := TRectangularFrameXComparer.Create;
  FVerticalObjectComparer := TRectangularFrameYComparer.Create;
end;

class destructor TdxParagraphFramesLayout.Finalize;
begin
  FreeAndNil(FHorizontalObjectComparer);
  FreeAndNil(FVerticalObjectComparer);
end;

function TdxParagraphFramesLayout.ContainsParagraph(AParagraph: TdxParagraphBase): Boolean;
var
  I: Integer;
begin
  Result := False;
  if FItems.Count = 0 then
    Exit;
  for I := 0 to FItems.Count - 1 do
    if (Items[I].PieceTable = AParagraph.PieceTable) and
        (AParagraph.Index >= Items[I].ParagraphIndex) and
        (AParagraph.Index <= Items[I].LastParagraphIndex) then
      Exit(True);
end;

function TdxParagraphFramesLayout.GetFloatingObject(AParagraph: TdxParagraphBase): TdxParagraphFrameBox;
var
  AIndex: TdxParagraphIndex;
  I: Integer;
begin
  if FItems.Count = 0 then
    Exit(nil);
  AIndex := AParagraph.Index;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I].ParagraphIndex = AIndex then
      Exit(Items[I]);
  end;
  Result := nil;
end;

procedure TdxParagraphFramesLayout.AddParagraphFrameBox(AParagraphFrameBox: TdxParagraphFrameBox);
begin
  if not FItems.Contains(AParagraphFrameBox) then
    Items.Add(AParagraphFrameBox);
end;

function TdxParagraphFramesLayout.GetObjectsInRectangle(const ABounds: TRect): TdxParagraphFrameBoxList;
var
  R: TRect;
begin
  R := ABounds;
  R.Height := Max(1, ABounds.Height);
  Result := TdxParagraphFrameBoxList.Create;
  GetObjectsInRectangle(Items, Result, R);
  Result.Sort(FHorizontalObjectComparer);
end;

procedure TdxParagraphFramesLayout.GetObjectsInRectangle(AWhere: TdxParagraphFrameBoxList; ATo: TdxParagraphFrameBoxList; const ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to AWhere.Count - 1 do
    if AWhere[I].ExtendedBounds.IntersectsWith(ABounds) then
      ATo.Add(AWhere[I]);
end;


procedure TdxParagraphFramesLayout.ProcessParagraphFrame(AParagraphFrame: TdxParagraphFrameBox;
  AProcessedObjects: TdxParagraphFrameBoxList; AResult: TdxTextAreaCollectionEx; const AInitialBounds: TRect);
var
  ABounds: TRect;
  ALeftMostX, ARightMostX: Integer;
  AFrameProperties: TdxMergedFrameProperties;
begin
  ABounds := AParagraphFrame.ExtendedBounds;
  ALeftMostX := FindLeftMostX(AProcessedObjects, ABounds.Left, AInitialBounds);
  ARightMostX := FindRightMostX(AProcessedObjects, ABounds.Right, AInitialBounds);

  AProcessedObjects.Add(AParagraphFrame);

  AFrameProperties := AParagraphFrame.GetFrameProperties;
  try
    if AFrameProperties.Info.TextWrapType <> TdxParagraphFrameTextWrapType.None then
      AResult.Remove(TdxTextArea.Create(ABounds.Left, ABounds.Right));
    if AFrameProperties.Info.TextWrapType = TdxParagraphFrameTextWrapType.NotBeside then
      AResult.Remove(TdxTextArea.Create(ALeftMostX, ARightMostX));
  finally
    AFrameProperties.Free;
  end;
end;


function TdxParagraphFramesLayout.FindLeftMostX(AProcessedObjects: TdxParagraphFrameBoxList; AInitialX: Integer; const ABounds: TRect): Integer;
var
  I, AObjectBoundsRight: Integer;
begin
  Result := ABounds.Left;
  for I := 0 to AProcessedObjects.Count - 1 do
  begin
    AObjectBoundsRight := AProcessedObjects[I].ExtendedBounds.Right;
    if (AObjectBoundsRight < AInitialX) and (AObjectBoundsRight > Result) then
      Result := AObjectBoundsRight;
  end;
end;

function TdxParagraphFramesLayout.FindRightMostX(AProcessedObjects: TdxParagraphFrameBoxList; AInitialX: Integer; const ABounds: TRect): Integer;
var
  I, AObjectBoundsLeft: Integer;
begin
  Result := ABounds.Right;
  for I := 0 to AProcessedObjects.Count - 1 do
  begin
    AObjectBoundsLeft := AProcessedObjects[I].ExtendedBounds.Left;
    if (AObjectBoundsLeft > AInitialX) and (AObjectBoundsLeft < Result) then
      Result := AObjectBoundsLeft;
  end;
end;

procedure TdxParagraphFramesLayout.ClearParagraphFrames(ARunIndex: TdxRunIndex);
var
  I: Integer;
  AParagraphFrame: TdxParagraphFrameBox;
begin
  for I := Items.Count - 1 downto 0 do
  begin
    AParagraphFrame := Items[I];
    if ARunIndex <= AParagraphFrame.StartPos.RunIndex then
    begin
      Items.Delete(I);
      AParagraphFrame.LockPosition := False;
    end;
  end;
end;

procedure TdxParagraphFramesLayout.Clear;
begin
  Items.Clear;
end;

{ TdxRowsController }

constructor TdxRowsController.Create(APieceTable: TdxPieceTable; const AColumnController: TdxCustomColumnController; AMatchHorizontalTableIndentsToTextEdge: Boolean);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  Assert(AColumnController <> nil);
  InnerSetColumnController(AColumnController);
  FTabsController := TdxTabsController.Create;
  FTablesController := CreateTablesController;
  FColumns := TdxBoxList.Create;
  FPieceTable := APieceTable;
  FCurrentRunIndex := -1;
  FSupportsColumnAndPageBreaks := True;
  FFloatingObjectsLayout := AColumnController.PageAreaController.PageController.FloatingObjectsLayout;
  Assert(FFloatingObjectsLayout <> nil);
  FParagraphFramesLayout := AColumnController.PageAreaController.PageController.ParagraphFramesLayout;
  Assert(FParagraphFramesLayout <> nil);
  FHorizontalPositionController := CreateCurrentHorizontalPosition;
  FLineNumberStep := 1;
  FBoxes := TdxFastObjectList.Create(False);
  FMatchHorizontalTableIndentsToTextEdge := AMatchHorizontalTableIndentsToTextEdge;
  FFrameParagraphIndex := -1;
  FLastRestartDueToFloatingObjectModelPosition := TdxDocumentModelPosition.Create(APieceTable);
end;

destructor TdxRowsController.Destroy;
begin
  InnerSetColumnController(nil);
  FreeAndNil(FBoxes);
  FreeAndNil(FLineSpacingCalculator);
  FreeAndNil(FHorizontalPositionController);
  FreeAndNil(FTablesController);
  FreeAndNil(FTabsController);
  CurrentRow := nil;
  CurrentColumn := nil;
  FreeAndNil(FColumns);
  inherited Destroy;
end;

function TdxRowsController.AddBox(ABoxType: TdxBoxClass; ABoxInfo: TdxBoxInfo): TdxBox;
var
  ASpaceBox: IdxSpaceBox;
  ARunIndex: TdxRunIndex;
  ARun: TdxTextRunBase;
begin
  Result := GetBox(ABoxType, ABoxInfo);
  Result.Bounds := HorizontalPositionController.CalculateBoxBounds(ABoxInfo);
  if Supports(Result, IdxSpaceBox, ASpaceBox) then
    ASpaceBox.MinWidth := ABoxInfo.Size.cx;
  CurrentRow.Boxes.Add(Result);
  HorizontalPositionController.IncrementCurrentHorizontalPosition(ABoxInfo.Size.cx);
  ARunIndex := ABoxInfo.StartPos.RunIndex;
  ARun := PieceTable.Runs[ARunIndex];
  CurrentRow.ProcessingFlags := CurrentRow.ProcessingFlags + ARun.RowProcessingFlags;
end;

function TdxRowsController.AddFloatingObjectToLayout(AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox;
  ABoxInfo: TdxBoxInfo): Boolean;
var
  ACurrentRun: TdxTextRunBase;
  AObjectAnchorRun: TdxFloatingObjectAnchorRun;
  AFloatingObjectBox: TdxFloatingObjectBox;
begin
  ACurrentRun := PieceTable.Runs[ABoxInfo.StartPos.RunIndex];
  Assert(ACurrentRun is TdxFloatingObjectAnchorRun);
  AObjectAnchorRun := TdxFloatingObjectAnchorRun(ACurrentRun);
  if AObjectAnchorRun.ExcludeFromLayout then
    Exit(False);
  Assert((ABoxInfo.Box is TdxFloatingObjectAnchorBox) and (ABoxInfo.Box = AFloatingObjectAnchorBox));

  if FloatingObjectsLayout.ContainsRun(AObjectAnchorRun) then
  begin
    AFloatingObjectBox := FloatingObjectsLayout.GetFloatingObject(AObjectAnchorRun);
    if AObjectAnchorRun.FloatingObjectProperties.TextWrapType <> TdxFloatingObjectTextWrapType.None then
      AFloatingObjectBox.LockPosition := True;

    EnsureFloatingObjectBoxActualBounds(AFloatingObjectAnchorBox, AObjectAnchorRun, AFloatingObjectBox);
    Result := False;
  end
  else
  begin
    AFloatingObjectBox := AFloatingObjectAnchorBox.FloatingObjectBox;
    if AFloatingObjectBox = nil then
    begin
      Assert(False);
      AFloatingObjectBox := CreateFloatingObjectBox(AFloatingObjectAnchorBox, AObjectAnchorRun);
    end
    else
      EnsureFloatingObjectBoxActualBounds(AFloatingObjectAnchorBox, AObjectAnchorRun, AFloatingObjectBox);
    FloatingObjectsLayout.Add(AObjectAnchorRun, AFloatingObjectBox);
    UpdateCurrentTableCellBottom(AFloatingObjectBox);
    RecreateHorizontalPositionController;
    if AObjectAnchorRun.FloatingObjectProperties.TextWrapType <> TdxFloatingObjectTextWrapType.None then
      AFloatingObjectBox.LockPosition := True;
    Result := True;
  end;
end;

procedure TdxRowsController.EnsureFloatingObjectBoxActualBounds(AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox;
  AObjectAnchorRun: TdxFloatingObjectAnchorRun; AFloatingObjectBox: TdxFloatingObjectBox);
begin
  if ShouldChangeExistingFloatingObjectBounds(AFloatingObjectBox, AFloatingObjectAnchorBox) then
  begin
    ApplyFloatingObjectBoxBounds(AFloatingObjectBox, AFloatingObjectAnchorBox);
    if AObjectAnchorRun.Content is TdxTextBoxFloatingObjectContent then
      FormatFloatingObjectTextBox(AFloatingObjectBox, Safe<TdxTextBoxFloatingObjectContent>.Cast(AObjectAnchorRun.Content), AFloatingObjectAnchorBox);
  end;
end;

function TdxRowsController.CreateFloatingObjectBox(AAnchorBox: TdxFloatingObjectAnchorBox; AObjectAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectBox;
begin
  Result := TdxFloatingObjectBox.Create;
  Result.StartPos := AAnchorBox.StartPos;
  ApplyFloatingObjectBoxProperties(Result, AObjectAnchorRun.FloatingObjectProperties);

  AAnchorBox.SetFloatingObjectRun(AObjectAnchorRun);
  ApplyFloatingObjectBoxBounds(Result, AAnchorBox);

  if AObjectAnchorRun.Content is TdxTextBoxFloatingObjectContent then
    FormatFloatingObjectTextBox(Result, Safe<TdxTextBoxFloatingObjectContent>.Cast(AObjectAnchorRun.Content), AAnchorBox);
end;

procedure TdxRowsController.AddNumberingListBox(ABox: TdxNumberingListBox; AProperties: TdxListLevelProperties;
  AFormatter: TdxParagraphBoxFormatter);
var
  ABounds: TRect;
begin
  FCurrentParagraphNumberingListBounds := ABox.Bounds;
  ABounds := CalculateNumberingListBoxBounds(ABox, AProperties, AFormatter);
  ABox.Bounds := ABounds;
  HorizontalPositionController.SetCurrentHorizontalPosition(ABounds.Right);
end;

procedure TdxRowsController.AddPageBreakBeforeRow;
var
  ATopLevelColumn: TdxColumn;
  R: TRect;
begin
  ATopLevelColumn := CurrentColumn.TopLevelColumn;
  R := ATopLevelColumn.Bounds;
  R.Height := Max(CurrentRow.Bounds.Top - R.Top, 0);
  ATopLevelColumn.Bounds := R;
end;

procedure TdxRowsController.SaveCurrentInfo;
begin
  FSavedCurrentColumnBounds := CurrentColumn.Bounds;
end;

function TdxRowsController.CanFitCurrentRowWithFrameToColumn(ANewBoxInfo: TdxBoxInfo): TdxCanFitCurrentRowToColumnResult;
var
  AEmptyColumn: Boolean;
  AParagraphFrameBox: TdxParagraphFrameBox;
  ARowHeight: Integer;
begin
  AEmptyColumn := CurrentColumn.Rows.Count <= 0;
  if (ANewBoxInfo.StartPos.RunIndex > ColumnController.PageLastRunIndex) and not AEmptyColumn then
    Exit(TdxCanFitCurrentRowToColumnResult.PlainRowNotFitted);

  AParagraphFrameBox := Safe<TdxParagraphFrameBox>.Cast(ANewBoxInfo.Box);
  if Paragraph.HasMergedFrameProperties and (AParagraphFrameBox <> nil) then
  begin
    if not CanFitParagraphFrame(ANewBoxInfo, AEmptyColumn) then
      Exit(TdxCanFitCurrentRowToColumnResult.RestartDueFloatingObject);
  end;
  ARowHeight := CalcNewCurrentRowHeight(ANewBoxInfo);
  Result := CanFitCurrentRowToColumnCore(ARowHeight);
end;

function TdxRowsController.CalcRowHeightWithBox(ABoxInfo: TdxBoxInfo): Integer;
var
  AAscentAndFree: Integer;
begin
  AAscentAndFree := GetFontAscentAndFree(ABoxInfo);
  if (ABoxInfo.Box <> nil) and ABoxInfo.Box.IsInlinePicture then
    AAscentAndFree := Max(AAscentAndFree, ABoxInfo.Size.cy);
  Result := Max(FMaxAscentAndFree, AAscentAndFree) + Max(FMaxDescent, GetFontDescent(ABoxInfo));
end;

function TdxRowsController.CalculateSpacingBefore: Integer;
var
  ASpacingBefore: Integer;
begin
  ASpacingBefore := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(FParagraph.ContextualSpacingBefore);
  if ASpacingBefore < FSpacingAfterPrevParagraph then
    Result := 0
  else
    Result := ASpacingBefore - FSpacingAfterPrevParagraph;
end;

function TdxRowsController.CanAddSectionBreakToPrevRow: Boolean;
begin
  Result := CurrentColumn.Rows.Last <> nil;
end;

function TdxRowsController.CompleteCurrentColumnFormatting: TdxCompleteFormattingResult;
begin
  Result := ColumnController.CompleteCurrentColumnFormatting(CurrentColumn);
end;

function TdxRowsController.CreateCurrentHorizontalPosition: TdxCurrentHorizontalPositionController;
begin
  Result := TdxCurrentHorizontalPositionController.Create(Self);
end;

function TdxRowsController.CreateCurrentHorizontalPosition(APosition: Integer): TdxCurrentHorizontalPositionController;
begin
  Result := TdxCurrentHorizontalPositionController.Create(Self, APosition);
end;

function TdxRowsController.GetContextualSpacingAfter(AParagraph: TdxParagraph): Integer;
begin
  Result := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(AParagraph.ContextualSpacingAfter);
end;

class function TdxRowsController.GetBounds(const ALocation: TPoint; AWidth: Integer; AHeight: Integer): TRect;
begin
  Result.InitSize(ALocation, AWidth, AHeight);
end;

class procedure TdxRowsController.SetActualBoundsForEmptyHeightFrameBox(AParagraphFrameBox: TdxParagraphFrameBox; AElementBoundsHeight: Integer);
begin
  AParagraphFrameBox.Bounds := GetBounds(AParagraphFrameBox.Bounds.Location, AParagraphFrameBox.Bounds.Width, AElementBoundsHeight);
  AParagraphFrameBox.ShapeBounds := GetBounds(AParagraphFrameBox.ShapeBounds.Location, AParagraphFrameBox.ShapeBounds.Width, AElementBoundsHeight);
  AParagraphFrameBox.ContentBounds := GetBounds(AParagraphFrameBox.ContentBounds.Location, AParagraphFrameBox.ContentBounds.Width, AElementBoundsHeight);
  AParagraphFrameBox.ExtendedBounds := GetBounds(AParagraphFrameBox.ExtendedBounds.Location, AParagraphFrameBox.ExtendedBounds.Width, AElementBoundsHeight);
  AParagraphFrameBox.ActualBounds := GetBounds(AParagraphFrameBox.ActualBounds.Location, AParagraphFrameBox.ActualBounds.Width, AElementBoundsHeight);
  AParagraphFrameBox.SetActualSizeBounds(GetBounds(AParagraphFrameBox.ActualSizeBounds.Location, AParagraphFrameBox.ActualSizeBounds.Width, AElementBoundsHeight));
end;

class function TdxRowsController.GetBoundsLocation(const ABounds: TRect; APositionOffset: Integer): TPoint;
begin
  Result.Init(ABounds.Location.X - APositionOffset, ABounds.Location.Y);
end;

class procedure TdxRowsController.SetActualBoundsForEmptyWidthFrameBox(AParagraphFrameBox: TdxParagraphFrameBox;
  AAlignment: TdxParagraphFrameHorizontalPositionAlignment; AElementBoundsWidth: Integer);
var
  APositionOffset, I: Integer;
  ABoundsLocation, AShapeLocation, AContentLocation, AExtendedLocation, AActualLocation, AActualSizeLocation, ACurrentBoxLocation: TPoint;
  ACurrentBox: TdxBox;
  ABoxCollection: TdxSimpleParagraphBoxCollection;
begin
  if AAlignment in [TdxParagraphFrameHorizontalPositionAlignment.Right, TdxParagraphFrameHorizontalPositionAlignment.Outside] then
    APositionOffset := AElementBoundsWidth
  else
    if AAlignment = TdxParagraphFrameHorizontalPositionAlignment.Center then
      APositionOffset := AElementBoundsWidth div 2
    else
      APositionOffset := 0;

  ABoundsLocation := GetBoundsLocation(AParagraphFrameBox.Bounds, APositionOffset);
  AShapeLocation := GetBoundsLocation(AParagraphFrameBox.ShapeBounds, APositionOffset);
  AContentLocation := GetBoundsLocation(AParagraphFrameBox.ContentBounds, APositionOffset);
  AExtendedLocation := GetBoundsLocation(AParagraphFrameBox.ExtendedBounds, APositionOffset);
  AActualLocation := GetBoundsLocation(AParagraphFrameBox.ActualBounds, APositionOffset);
  AActualSizeLocation := GetBoundsLocation(AParagraphFrameBox.ActualSizeBounds, APositionOffset);

  AParagraphFrameBox.Bounds := GetBounds(ABoundsLocation, AElementBoundsWidth, AParagraphFrameBox.Bounds.Height);
  AParagraphFrameBox.ShapeBounds := GetBounds(AShapeLocation, AElementBoundsWidth, AParagraphFrameBox.ShapeBounds.Height);
  AParagraphFrameBox.ContentBounds := GetBounds(AContentLocation, AElementBoundsWidth, AParagraphFrameBox.ContentBounds.Height);
  AParagraphFrameBox.ExtendedBounds := GetBounds(AExtendedLocation, AElementBoundsWidth, AParagraphFrameBox.ExtendedBounds.Height);
  AParagraphFrameBox.ActualBounds := GetBounds(AActualLocation, AElementBoundsWidth, AParagraphFrameBox.ActualBounds.Height);
  AParagraphFrameBox.SetActualSizeBounds(GetBounds(AActualSizeLocation, AElementBoundsWidth, AParagraphFrameBox.ActualSizeBounds.Height));

  ABoxCollection := AParagraphFrameBox.GetParagraph.BoxCollection;
  for I := 0 to ABoxCollection.Count - 1 do
  begin
    ACurrentBox := ABoxCollection[I];
    ACurrentBoxLocation := GetBoundsLocation(ACurrentBox.Bounds, APositionOffset);
    ACurrentBox.Bounds := TRect.CreateSize(ACurrentBoxLocation, ACurrentBox.Bounds.Size);
  end;
end;

procedure TdxRowsController.AddSectionBreakBoxToPrevRow(ABoxType: TdxBoxClass; ABoxInfo: TdxBoxInfo);
var
  ABox: TdxBox;
  ALastRow: TdxRow;
begin
  ALastRow := CurrentColumn.Rows.Last;
  ABox := GetBox(ABoxType, ABoxInfo);
  ABox.Bounds := TRect.CreateSize(TPoint.Create(ALastRow.Boxes.Last.Bounds.Right, 0), ABoxInfo.Size);
  ALastRow.Boxes.Add(ABox);
end;

procedure TdxRowsController.AddSingleDecimalTabInTable;
begin
  if CurrentHorizontalPosition > FParagraphRight then
    SuppressHorizontalOverfull := True;
  FTabsController.SaveLastTabPosition(-1);
end;

procedure TdxRowsController.AddTabBox(const ATab: TdxTabInfo; ABoxInfo: TdxBoxInfo);
var
  ABox: TdxTabSpaceBox;
  ATabPosition, ANewPosition: Integer;
begin
  if ATab.Alignment = TdxTabAlignmentType.Left then
  begin
    ATabPosition := ATab.GetLayoutPosition(DocumentModel.ToDocumentLayoutUnitConverter);
    ANewPosition := ATabPosition + CurrentColumn.Bounds.Left;
    ABoxInfo.Size := cxSize(ANewPosition - CurrentHorizontalPosition, ABoxInfo.Size.cy);
  end
  else
    ANewPosition := CurrentHorizontalPosition;
  if (ANewPosition > FParagraphRight) and not ATab.IsDefault then
    SuppressHorizontalOverfull := True;
  FTabsController.SaveLastTabPosition(CurrentRow.Boxes.Count);
  ABox := TdxTabSpaceBox(AddBox(TdxTabSpaceBox, ABoxInfo));
  ABox.TabInfo := ATab;
  ABox.LeaderCount := TdxTabsController.CalculateLeaderCount(ABox, PieceTable);
end;

procedure TdxRowsController.ApplyCurrentColumnBounds(const ABounds: TRect);
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  FParagraphLeft := ABounds.Left + AUnitConverter.ToLayoutUnits(FParagraph.LeftIndent);
  FParagraphRight := ABounds.Right - AUnitConverter.ToLayoutUnits(FParagraph.RightIndent);
  HorizontalPositionController.SetCurrentRowInitialHorizontalPosition;
  FTabsController.ColumnLeft := ABounds.Left;
  FTabsController.FParagraphRight := FParagraphRight;
end;

procedure TdxRowsController.ApplyFloatingObjectBoxBounds(AFloatingObjectBox: TdxFloatingObjectBox;
  AAnchorBox: TdxFloatingObjectAnchorBox);
begin
  AFloatingObjectBox.ExtendedBounds := AAnchorBox.ActualBounds;
  AFloatingObjectBox.Bounds := AAnchorBox.ShapeBounds;
  AFloatingObjectBox.ContentBounds := AAnchorBox.ContentBounds;
  AFloatingObjectBox.SetActualSizeBounds(AAnchorBox.ActualSizeBounds);
end;

procedure TdxRowsController.ApplyFloatingObjectBoxProperties(AFloatingObjectBox: TdxFloatingObjectBox;
  AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  AFloatingObjectBox.CanPutTextAtLeft := AFloatingObjectProperties.CanPutTextAtLeft;
  AFloatingObjectBox.CanPutTextAtRight := AFloatingObjectProperties.CanPutTextAtRight;
  AFloatingObjectBox.PutTextAtLargestSide := AFloatingObjectProperties.PutTextAtLargestSide;
  AFloatingObjectBox.PieceTable := TdxPieceTable(AFloatingObjectProperties.PieceTable);
end;

procedure TdxRowsController.ApplySectionStart(ASection: TdxSection; ACurrentColumnsCount: Integer);
var
  ARestart: TdxLineNumberingRestart;
begin
  case ASection.GeneralSettings.StartType of
    TdxSectionStartType.Column:
      if ASection.GetActualColumnsCount <> ACurrentColumnsCount then
        EnforceNextRowToStartFromNewPage;
    TdxSectionStartType.EvenPage,
    TdxSectionStartType.OddPage,
    TdxSectionStartType.NextPage:
      EnforceNextRowToStartFromNewPage;
  end;

  ARestart := GetEffectiveLineNumberingRestartType(ASection);
  if ARestart = TdxLineNumberingRestart.NewSection then
  begin
    InitialLineNumber := ASection.LineNumbering.StartingLineNumber;
    LineNumberStep := ASection.LineNumbering.Step;
  end
  else
    if ARestart = TdxLineNumberingRestart.NewPage then
    begin
      InitialLineNumber := ASection.LineNumbering.StartingLineNumber;
      LineNumberStep := ASection.LineNumbering.Step;
    end;
end;

procedure TdxRowsController.AssignCurrentRowLineNumber;
begin
  if not FParagraph.SuppressLineNumbers and not TablesController.IsInsideTable and ((LineNumber mod LineNumberStep) = 0) then
    CurrentRow.LineNumber := LineNumber
  else
    CurrentRow.LineNumber := -LineNumber;
end;

procedure TdxRowsController.BeginParagraph(AParagraph: TdxParagraph; ABeginFromParagraphStart, AParagraphFrameStarted: Boolean);
var
  ASpacingBefore: Integer;
begin
  Assert(AParagraph.PieceTable = PieceTable);
  EnsureSpacingAfterPrevParagraphValid(AParagraph);
  FParagraph := AParagraph;
  PrepareCurrentRowBounds(AParagraph, ABeginFromParagraphStart);

  if not AParagraphFrameStarted then
    TablesController.BeginParagraph(AParagraph)
  else if TablesController.IsInsideTable then
    TablesController.LeaveCurrentTableCore;

  PrepareCurrentRowBounds(AParagraph, ABeginFromParagraphStart); // call before FloatingObjectsCurrentHorizontalPositionController. MoveCurrentRowDownToFitTable

  ASpacingBefore := CalculateSpacingBefore;
  FInvisibleEmptyParagraphInCellAfterNestedTable := ShouldIgnoreParagraphHeight(AParagraph);
  if FInvisibleEmptyParagraphInCellAfterNestedTable then
  begin
    CurrentRow.ProcessingFlags := CurrentRow.ProcessingFlags + [TdxRowProcessingFlag.LastInvisibleEmptyCellRowAfterNestedTable];
    CurrentRow.Height := 0;
    ASpacingBefore := 0;
  end;

  CurrentRow.ProcessingFlags := CurrentRow.ProcessingFlags + [TdxRowProcessingFlag.FirstParagraphRow];

  if ShouldApplySpacingBefore(AParagraph) then
  begin
    CurrentRow.SpacingBefore := ASpacingBefore;
    OffsetCurrentRow(ASpacingBefore);
  end;
  HorizontalPositionController.OnCurrentRowHeightChanged(False);
  HorizontalPositionController.SetCurrentRowInitialHorizontalPosition;
  AssignCurrentRowLineNumber;
  if FTabsController.SingleDecimalTabInTable then
    AddSingleDecimalTabInTable;
  if ABeginFromParagraphStart then
  begin
    ParagraphFirstRowOnPage := CurrentRow;
    ParagraphFirstColumnOnPage := CurrentColumn;
  end
  else
    ObtainParagraphFirstRowPosition(AParagraph);
end;

procedure TdxRowsController.ResetSpacingBefore;
var
  ASpacingBefore: Integer;
  ABounds: TRect;
begin
  ASpacingBefore := CurrentRow.SpacingBefore;
  if ASpacingBefore = 0 then
    Exit;
  ABounds := CurrentRow.Bounds;
  ABounds.Offset(0, -ASpacingBefore);
  CurrentRow.Bounds := ABounds;
  CurrentRow.SpacingBefore := 0;
  HorizontalPositionController.OnCurrentRowHeightChanged(True);
end;

function TdxRowsController.ShouldApplySpacingBefore(AParagraph: TdxParagraph): Boolean;
begin
  Result := (FrameParagraphIndex <> -1) or not AParagraph.HasParagraphFrame;
end;

procedure TdxRowsController.BeginSectionFormatting(ASection: TdxSection);
begin
  ApplySectionStart(ASection, ColumnController.TopLevelColumnsCount);
  FSpacingAfterPrevParagraph := 0;
  FIsSpacingAfterPrevParagraphValid := True;
end;

function TdxRowsController.CalcDefaultRowBounds(Y: Integer): TRect;
begin
  Result.Init(ParagraphLeft + FCurrentRowIndent, Y, FParagraphRight, Y + DefaultRowHeight);
end;

function TdxRowsController.CalcNewCurrentRowHeight(ANewBoxInfo: TdxBoxInfo): Integer;
begin
  Result := FLineSpacingCalculator.CalcRowHeight(CurrentRow.Height, CalcRowHeightWithBox(ANewBoxInfo));
end;

function TdxRowsController.CalcPrevBottom(APrevRow: TdxRow; AAfterRestart: Boolean; ANewRow: TdxRow): Integer;
var
  ATableCellRow: TdxTableCellRow;
begin
  if APrevRow = nil then
    Result := CurrentColumn.Bounds.Top
  else
  begin
    Result := APrevRow.Bounds.Bottom;
    if not AAfterRestart then
      Exit;
    if not (APrevRow is TdxTableCellRow) then
      Exit;
    ATableCellRow := TdxTableCellRow(APrevRow);
    if not (ANewRow is TdxTableCellRow) or (TdxTableCellRow(ANewRow).CellViewInfo.Cell.Table <> ATableCellRow.CellViewInfo.Cell.Table) then
      Result := ATableCellRow.CellViewInfo.TableViewInfo.GetTableBottom;
  end;
end;

function TdxRowsController.CalculateNumberingListBoxBounds(ABox: TdxNumberingListBox;
  AProperties: TdxListLevelProperties; AFormatter: TdxParagraphBoxFormatter): TRect;
var
  ALeft, AInitialControllerLeft, ADelta: Integer;
  AController: TdxFloatingObjectsCurrentHorizontalPositionController;
begin
  ALeft := CurrentRow.Bounds.Left;
  AInitialControllerLeft := HorizontalPositionController.CurrentHorizontalPosition;
  AController := Safe<TdxFloatingObjectsCurrentHorizontalPositionController>.Cast(HorizontalPositionController);
  if (AController <> nil) and (ParagraphLeft + FCurrentRowIndent < AInitialControllerLeft) then
  begin
    ADelta := ParagraphLeft + FCurrentRowIndent - CurrentColumn.Bounds.Left;
    HorizontalPositionController.SetCurrentHorizontalPosition(AInitialControllerLeft + ADelta);
  end;
  Result := CalculateNumberingListBoxBoundsCore(ABox, AProperties, AFormatter);
  CurrentRow.TextOffset := Result.Right - ALeft;
  if AController = nil then
    Exit;
  ALeft := AInitialControllerLeft;
  CurrentRow.TextOffset := Result.Right - ALeft;
  if CanFitNumberingListBoxToCurrentRow(Result.Size) then
    Exit;

  while AController.AdvanceHorizontalPositionToNextTextArea do
  begin
    ALeft := AController.CurrentHorizontalPosition;
    Result := CalculateNumberingListBoxBoundsCore(ABox, AProperties, AFormatter);
    if CanFitNumberingListBoxToCurrentRow(Result.Size) then
      Break;
  end;
  CurrentRow.TextOffset := Result.Right - ALeft;
end;

function TdxRowsController.CalculateNumberingListBoxBoundsCore(ABox: TdxNumberingListBox;
  AProperties: TdxListLevelProperties; AFormatter: TdxParagraphBoxFormatter): TRect;
var
  ASeparatorBounds: TRect;
  AAlignment: TdxListNumberAlignment;
  ANumberingListBoxLeft, ASeparatorWidth: Integer;
  ABoxWithSeparator: TdxNumberingListBoxWithSeparator;
begin
  Result := ABox.Bounds;
  AAlignment := AProperties.Alignment;
  ANumberingListBoxLeft := CalculateNumberingListBoxLeft(Result.Width, AAlignment);

  Result.InitSize(ANumberingListBoxLeft, 0, Result.Width, Result.Height);
  CurrentRow.NumberingListBox := ABox;

  if ABox is TdxNumberingListBoxWithSeparator then
  begin
    ABoxWithSeparator := TdxNumberingListBoxWithSeparator(ABox);
    if Supports(ABoxWithSeparator.SeparatorBox, IdxSpaceBox) then
      Result.Width := Result.Width + ABoxWithSeparator.SeparatorBox.Bounds.Width
    else
    begin
      Assert(ABoxWithSeparator.SeparatorBox is TdxTabSpaceBox);
      if AProperties.Legacy then
        ASeparatorWidth := GetLegacyNumberingListBoxSeparatorWidth(ABoxWithSeparator, Result, AProperties, AFormatter)
      else
        ASeparatorWidth := GetNumberingListBoxSeparatorWidth(ABoxWithSeparator, Result, AProperties, AFormatter);
      Result.Width := Result.Width + ASeparatorWidth;
      ASeparatorBounds := ABoxWithSeparator.SeparatorBox.Bounds;
      ASeparatorBounds.Width := ASeparatorWidth;
      ABoxWithSeparator.SeparatorBox.Bounds := ASeparatorBounds;
    end;
  end;
end;

function TdxRowsController.CalculateNumberingListBoxLeft(ABoxWidth: Integer; AAlignment: TdxListNumberAlignment): Integer;
var
  ABoundsLeft: Integer;
begin
  ABoundsLeft := CurrentHorizontalPosition;
  case AAlignment of
    TdxListNumberAlignment.Left:
      Result := ABoundsLeft;
    TdxListNumberAlignment.Right:
      Result := ABoundsLeft - ABoxWidth;
    else
      Result := Trunc(ABoundsLeft - ABoxWidth / 2);
  end;
end;

function TdxRowsController.CalculateRestartPositionDueFloatingObject(
  AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox; const ACurrentPosition: TPoint): TdxDocumentModelPosition;
var
  ABounds, ARowBounds: TRect;
  APageController: TdxPageController;
  APages: TdxPageCollection;
  ACurrentPage: TdxPage;
  AHitTestRequest: TdxRichEditHitTestRequest;
  AHitTestResult: TdxRichEditHitTestResult;
  ACalculator: TdxBoxHitTestCalculator;
  ATableCellColumnController: TdxTableCellColumnController;
  ARows: TdxRowCollection;
  ATable: TdxTable;
  ACell, ALastTableCell: TdxTableCell;
begin
  ABounds := AFloatingObjectAnchorBox.ActualBounds;
  if (ABounds.Left >= ACurrentPosition.X) and (ABounds.Top >= ACurrentPosition.Y) then
    Exit(TdxDocumentModelPosition.Null);

  APageController := ColumnController.PageAreaController.PageController;
  APages := APageController.Pages;
  ACurrentPage := APages.Last;
  if ACurrentPage.IsEmpty then
  begin
    if FloatingObjectsLayout.ContainsRun(AFloatingObjectAnchorBox.GetFloatingObjectRun(PieceTable)) then
      Exit(TdxDocumentModelPosition.Null);
    if CurrentRow.IsFirstParagraphRow then
      Exit(TdxDocumentModelPosition.FromParagraphStart(PieceTable, Paragraph.Index));
    if CurrentRow.Boxes.Count > 0 then
      Exit(CurrentRow.GetFirstPosition(PieceTable))
    else
      Exit(AFloatingObjectAnchorBox.GetFirstPosition(PieceTable));
  end;

  AHitTestRequest := TdxRichEditHitTestRequest.Create(PieceTable);
  AHitTestRequest.LogicalPoint := ABounds.Location;
  AHitTestRequest.IgnoreInvalidAreas := True;
  AHitTestRequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Row;
  AHitTestRequest.Accuracy := NearestPageArea or NearestColumn or NearestRow;

  AHitTestResult := TdxRichEditHitTestResult.Create(APageController.DocumentLayout, PieceTable);
  try
    AHitTestResult.Page := ACurrentPage;
    AHitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Page);
    ACalculator := APageController.CreateHitTestCalculator(AHitTestRequest, AHitTestResult);
    try
      if ColumnController is TdxTableCellColumnController then
        ATableCellColumnController := TdxTableCellColumnController(ColumnController)
      else
        ATableCellColumnController := nil;
      if ATableCellColumnController <> nil then
      begin
        AHitTestResult.PageArea := ColumnController.PageAreaController.Areas.Last;
        AHitTestResult.Column := ATableCellColumnController.CurrentTopLevelColumn;
        AHitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Column);
        ACalculator.CalcHitTest(CurrentColumn);
        if not AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Row) then
        begin
          if FloatingObjectsLayout.ContainsRun(AFloatingObjectAnchorBox.GetFloatingObjectRun(PieceTable)) then
            Exit(TdxDocumentModelPosition.Null)
          else
            Exit(AFloatingObjectAnchorBox.GetFirstPosition(PieceTable));
        end;
      end
      else
        ACalculator.CalcHitTest(ACurrentPage);
    finally
      ACalculator.Free;
    end;
    if not AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Row) then
    begin
      Assert(AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.PageArea));
      Assert(AHitTestResult.PageArea.Bounds.Bottom <= ABounds.Y);
      Exit(TdxDocumentModelPosition.Null);
    end;
    Assert(AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Row));
    Assert(AHitTestResult.Row <> nil);

    ARows := AHitTestResult.Column.Rows;
    if ARows.Last = AHitTestResult.Row then
    begin
      ARowBounds := AHitTestResult.Row.Bounds;
      if (ABounds.Top > ARowBounds.Bottom) and not ARowBounds.IntersectsWith(ABounds) then
        Exit(TdxDocumentModelPosition.Null)
    end;
    ACell := GetTableCell(AHitTestResult);
    if ACell <> nil then
    begin
      if ATableCellColumnController = nil then
      begin
        ATable := ACell.Table;
        while ATable.ParentCell <> nil do
          ATable := ATable.ParentCell.Table;
        ALastTableCell := ATable.Rows.Last.Cells.Last;
        Exit(TdxDocumentModelPosition.FromParagraphStart(PieceTable, ALastTableCell.EndParagraphIndex + 1));
      end;
    end;
    Result := AHitTestResult.Row.GetFirstPosition(PieceTable);
  finally
    AHitTestResult.Free;
  end;
end;

function TdxRowsController.CalculateRestartPositionDueFloatingObject(ABox: TdxSinglePositionBox; const ABounds: TRect;
  AIsContains: Boolean; const ACurrentPosition: TPoint): TdxDocumentModelPosition;
var
  APageController: TdxPageController;
  APages: TdxPageCollection;
  ACurrentPage: TdxPage;
  AHitTestRequest: TdxRichEditHitTestRequest;
  AHitTestResult: TdxRichEditHitTestResult;
  ACalculator: TdxBoxHitTestCalculator;
  ATableCellColumnController: TdxTableCellColumnController;
  ARows: TdxRowCollection;
  ARowBounds: TRect;
  ACell, ALastTableCell: TdxTableCell;
  AParagraphIndex: TdxParagraphIndex;
  ATable, ACurrentTable: TdxTable;
begin
  if (ABounds.Left >= ACurrentPosition.X) and (ABounds.Top >= ACurrentPosition.Y) then
    Exit(TdxDocumentModelPosition.Null);

  APageController := ColumnController.PageAreaController.PageController;
  APages := APageController.Pages;
  ACurrentPage := APages.Last;
  if ACurrentPage.IsEmpty then
  begin
    if AIsContains then
      Exit(TdxDocumentModelPosition.Null);
    if CurrentRow.IsFirstParagraphRow then
      Exit(TdxDocumentModelPosition.FromParagraphStart(PieceTable, Paragraph.Index));
    if CurrentRow.Boxes.Count > 0 then
      Exit(CurrentRow.GetFirstPosition(PieceTable))
    else
      Exit(ABox.GetFirstPosition(PieceTable));
  end;

  AHitTestRequest := TdxRichEditHitTestRequest.Create(PieceTable);
  AHitTestRequest.LogicalPoint := TPoint.Create(Max(ABounds.Top, ParagraphLeft), ABounds.Top);
  AHitTestRequest.IgnoreInvalidAreas := True;
  AHitTestRequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Row;
  AHitTestRequest.Accuracy := NearestPageArea or NearestColumn or NearestRow or ExactTableRow or ExactTableCell;

  AHitTestResult := TdxRichEditHitTestResult.Create(APageController.DocumentLayout, PieceTable);
  try
    AHitTestResult.Page := ACurrentPage;
    AHitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Page);
    ACalculator := APageController.CreateHitTestCalculator(AHitTestRequest, AHitTestResult);
    try
      ATableCellColumnController := Safe<TdxTableCellColumnController>.Cast(ColumnController);
      if ATableCellColumnController <> nil then
      begin
        AHitTestResult.PageArea := ColumnController.PageAreaController.Areas.Last;
        AHitTestResult.Column := ATableCellColumnController.CurrentTopLevelColumn;
        AHitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Column);
        ACalculator.CalcHitTest(CurrentColumn);
        if not AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Row) then
        begin
          if AIsContains then
            Exit(TdxDocumentModelPosition.Null);
          Exit(ABox.GetFirstPosition(PieceTable));
        end;
      end
      else
        ACalculator.CalcHitTest(ACurrentPage);
    finally
      ACalculator.Free;
    end;
    if not AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Row) then
    begin
      Assert(AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.PageArea));
      Exit(TdxDocumentModelPosition.Null);
    end;
    Assert(AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Row));
    Assert(AHitTestResult.Row <> nil);

    ARows := AHitTestResult.Column.Rows;
    if ARows.Last = AHitTestResult.Row then
    begin
      ARowBounds := AHitTestResult.Row.Bounds;
      if (ABounds.Top > ARowBounds.Bottom) and not ARowBounds.IntersectsWith(ABounds) then
        Exit(TdxDocumentModelPosition.Null);
    end;
    ACell := GetTableCell(AHitTestResult);
    if ACell <> nil then
    begin
      if ATableCellColumnController <> nil then
        ACurrentTable := ATableCellColumnController.CurrentCell.Cell.Table
      else
        ACurrentTable := nil;
      if ACurrentTable <> ACell.Table then
      begin
        ATable := ACell.Table;
        while (ATable.ParentCell <> nil)  and (ATable.ParentCell.Table <> ACurrentTable) do
          ATable := ATable.ParentCell.Table;
        ALastTableCell := ATable.Rows.Last.Cells.Last;
        AParagraphIndex := FindVisibleParagraph(ALastTableCell.EndParagraphIndex + 1);

        Exit(TdxDocumentModelPosition.FromParagraphStart(PieceTable, AParagraphIndex));
      end;
    end;
    Result := AHitTestResult.Row.GetFirstPosition(PieceTable);
  finally
    AHitTestResult.Free;
  end;
end;

function TdxRowsController.FindVisibleParagraph(AStart: TdxParagraphIndex): TdxParagraphIndex;
var
  AParagraph: TdxParagraph;
  ALastRunIndex, ARunIndex: TdxRunIndex;
begin
  Result := AStart;
  while Result < PieceTable.Paragraphs.Count - 1 do
  begin
    AParagraph := PieceTable.Paragraphs[Result];
    ALastRunIndex := AParagraph.LastRunIndex;
    if PieceTable.VisibleTextFilter.IsRunVisible(AParagraph.LastRunIndex) then
      Exit(Result);

    for ARunIndex := AParagraph.FirstRunIndex to ALastRunIndex - 1 do
      if PieceTable.VisibleTextFilter.IsRunVisible(ARunIndex) then
        Exit(Result);
    Inc(Result);
  end;
end;

function TdxRowsController.CalculateRestartPositionDueFloatingObject(
  AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox): TdxDocumentModelPosition;
var
  APoint: TPoint;
begin
  APoint.Init(CurrentHorizontalPosition, CurrentRow.Bounds.Top);
  Result := CalculateRestartPositionDueFloatingObject(AFloatingObjectAnchorBox, AFloatingObjectAnchorBox.ActualBounds,
    FloatingObjectsLayout.ContainsRun(AFloatingObjectAnchorBox.GetFloatingObjectRun(PieceTable)), APoint);
end;

function TdxRowsController.CalculateRestartPositionDueParagraphFrame(AParagraphFrameBox: TdxParagraphFrameBox): TdxDocumentModelPosition;
var
  X, Y: Integer;
begin
  X := CurrentHorizontalPosition;
  Y := CurrentRow.Bounds.Top;
  Result := CalculateRestartPositionDueFloatingObject(AParagraphFrameBox, AParagraphFrameBox.ActualBounds,
    ParagraphFramesLayout.ContainsParagraph(AParagraphFrameBox.GetParagraph), TPoint.Create(X, Y));
end;

function TdxRowsController.CanFitBoxToCurrentRow(const ABoxSize: TSize): Boolean;
begin
  Result := HorizontalPositionController.CanFitBoxToCurrentRow(ABoxSize);
end;

function TdxRowsController.CanFitCurrentRowToColumn(ANewBoxInfo: TdxBoxInfo): TdxCanFitCurrentRowToColumnResult;
var
  ABox: TdxBox;
  AEmptyPage: Boolean;
  ABoxIndex, ARowHeight: Integer;
  AResult: TdxCanFitFloatingObjectResult;
  AParagraphFrameBox: TdxParagraphFrameBox;
  AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox;
begin
  AEmptyPage := ColumnController.PageAreaController.PageController.Pages.Last.IsEmpty;
  if (ANewBoxInfo.StartPos.RunIndex > ColumnController.PageLastRunIndex) and not AEmptyPage then
    Exit(TdxCanFitCurrentRowToColumnResult.PlainRowNotFitted);

  if ANewBoxInfo.Box = nil then
  begin
    ABoxIndex := ANewBoxInfo.StartPos.BoxIndex;
    if ABoxIndex < Paragraph.BoxCollection.Count then
    begin
      ABox := Paragraph.BoxCollection[ABoxIndex];
      if ABox is TdxFloatingObjectAnchorBox then
        ANewBoxInfo.Box := ABox;
    end;
  end;
  AFloatingObjectAnchorBox := Safe<TdxFloatingObjectAnchorBox>.Cast(ANewBoxInfo.Box);
  if AFloatingObjectAnchorBox <> nil then
  begin
    AResult := CanFitFloatingObject(AFloatingObjectAnchorBox, ANewBoxInfo, AEmptyPage);
    if TdxCanFitFloatingObjectResult.Always = AResult then
      Exit(TdxCanFitCurrentRowToColumnResult.RowFitted)
    else
      if TdxCanFitFloatingObjectResult.DependsOnTable = AResult then
        Exit(TablesController.CanFitFloatingObjectInTableRow(AFloatingObjectAnchorBox.ActualBounds.Bottom, CurrentColumn))
      else
        Exit(TdxCanFitCurrentRowToColumnResult.RestartDueFloatingObject);
  end;
  AParagraphFrameBox := Safe<TdxParagraphFrameBox>.Cast(ANewBoxInfo.Box);
  if (Paragraph.FrameProperties <> nil) and (AParagraphFrameBox <> nil) then
  begin
    if not CanFitParagraphFrame(ANewBoxInfo, AEmptyPage) then
      Exit(TdxCanFitCurrentRowToColumnResult.RestartDueFloatingObject)
    else
      Exit(TdxCanFitCurrentRowToColumnResult.RowFitted);
  end;

  if (CurrentRow.Boxes.Count = 0) and (ANewBoxInfo.Box is TdxSectionMarkBox) then
    Exit(TdxCanFitCurrentRowToColumnResult.RowFitted);

  ARowHeight := CalcNewCurrentRowHeight(ANewBoxInfo);
  Result := CanFitCurrentRowToColumnCore(ARowHeight);
end;

function TdxRowsController.CanFitCurrentRowToColumnCore(ANewRowHeight: Integer): TdxCanFitCurrentRowToColumnResult;
begin
  Result := HorizontalPositionController.CanFitCurrentRowToColumn(ANewRowHeight);
end;

function TdxRowsController.CanFitNumberingListBoxToCurrentRow(const ABoxSize: TSize): Boolean;
begin
  Result := HorizontalPositionController.CanFitNumberingListBoxToCurrentRow(ABoxSize);
end;

function TdxRowsController.CanRestartDueFloatingObject: Boolean;
begin
  Result := True;
end;

function TdxRowsController.CanFitCurrentRowToColumn: TdxCanFitCurrentRowToColumnResult;
var
  ARowHeight: Integer;
begin
  ARowHeight := FLineSpacingCalculator.CalcRowHeight(CurrentRow.Height, CurrentRow.Height);
  Result := CanFitCurrentRowToColumnCore(ARowHeight);
end;

procedure TdxRowsController.ClearInvalidatedContent(AColumn: TdxColumn; const APos: TdxFormatterPosition;
  AParagraphIndex: TdxParagraphIndex);
begin
  ClearInvalidatedContentCore(AColumn, APos, AParagraphIndex);
  TablesController.ClearInvalidatedContent;
  InnerSetColumnController(GetTopLevelColumnController);
  RecreateHorizontalPositionController;
end;

procedure TdxRowsController.ClearInvalidatedContentCore(AColumn: TdxColumn; const APos: TdxFormatterPosition;
  AParagraphIndex: TdxParagraphIndex);
var
  ARows: TdxRowCollection;
  ARowIndex: Integer;
begin
  AColumn.ClearInvalidatedParagraphFrames(AParagraphIndex);
  ARows := AColumn.Rows;
  ARowIndex := ARows.BinarySearchBoxIndex(APos);
  if ARowIndex < 0 then
    ARowIndex := not ARowIndex;
  if ARowIndex < ARows.Count then
    ARows.DeleteRange(ARowIndex, ARows.Count - ARowIndex);
  FParagraph := PieceTable.Paragraphs[AParagraphIndex];
  ApplyCurrentColumnBounds(FSavedCurrentColumnBounds);
end;

procedure TdxRowsController.ClearInvalidatedContentFromTheStartOfRowAtCurrentPage(AColumn: TdxColumn;
  const APos: TdxFormatterPosition; AParagraphIndex: TdxParagraphIndex);
begin
  ClearInvalidatedContentCore(AColumn, APos, AParagraphIndex);
  RecreateHorizontalPositionController;
end;

procedure TdxRowsController.ClearRow(AKeepTextAreas: Boolean);
begin
  CurrentRow.Boxes.Clear;
  CurrentRow.ClearBoxRanges;
  InitCurrentRow(AKeepTextAreas);
end;

function TdxRowsController.CreateFloatingObjectSizeAndPositionController: TdxFloatingObjectSizeAndPositionController;
begin
  Result := TdxFloatingObjectSizeAndPositionController.Create(Self);
end;

procedure TdxRowsController.CreateLineNumberBoxes(ASection: TdxSection; APage: TdxPage);
var
  I: Integer;
  AAreas: TdxPageAreaCollection;
begin
  if ASection.LineNumbering.Step <= 0 then
    Exit;
  AAreas := APage.Areas;
  for I := 0 to AAreas.Count - 1 do
    CreateLineNumberBoxes(ASection, AAreas[i]);
end;

procedure TdxRowsController.CreateLineNumberBoxes(ASection: TdxSection; APageArea: TdxPageArea);
var
  I: Integer;
  AColumns: TdxColumnCollection;
begin
  APageArea.LineNumbers.Clear;
  AColumns := APageArea.Columns;
  for I := 0 to AColumns.Count - 1 do
    CreateLineNumberBoxes(ASection, AColumns[I], APageArea.LineNumbers);
end;

procedure TdxRowsController.CreateLineNumberBoxes(ASection: TdxSection; AColumn: TdxColumn;
  ALineNumbers: TdxLineNumberBoxCollection);
var
  AMeasurer: TdxBoxMeasurer;
  I, ADistance: Integer;
  ABoxInfo: TdxBoxInfo;
  AFontInfo: TdxFontInfo;
  ARows: TdxRowCollection;
  ARow: TdxRow;
  ABox: TdxLineNumberBox;
  AText: string;
  ABounds: TRect;
  ASize: TSize;
begin
  AMeasurer := ColumnController.Measurer;
  if AMeasurer = nil then
    Exit;
  ADistance := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ASection.LineNumbering.Distance);
  if ADistance <= 0 then
    ADistance := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(DocumentModel.UnitConverter.DocumentsToModelUnits(75));
  ABoxInfo := TdxBoxInfo.Create;
  try
    ABoxInfo.StartPos := TdxFormatterPosition.Create(0, 0, 0);
    ABoxInfo.EndPos := TdxFormatterPosition.Create(0, 0, 0);
    AFontInfo := DocumentModel.FontCache[DocumentModel.LineNumberRun.FontCacheIndex];
    ARows := AColumn.Rows;
    for I := 0 to ARows.Count - 1 do
    begin
      ARow := ARows[I];
      if ARow.LineNumber > 0 then
      begin
        AText := IntToStr(ARow.LineNumber);
        ABox := TdxLineNumberBox.Create(DocumentModel.LineNumberRun, ARow, AText);
        ABoxInfo.Box := ABox;
        AMeasurer.MeasureText(ABoxInfo, AText, AFontInfo);
        ABounds := ARow.Bounds;
        ASize := ABoxInfo.Size;
        ABounds.Left := AColumn.Bounds.Left - ADistance - ASize.cx;
        ABounds.Width := ASize.cx;
        ABox.Bounds := ABounds;
        ALineNumbers.Add(ABox);
      end;
    end;
  finally
    ABoxInfo.Free;
  end;
end;

function TdxRowsController.CreateLineSpacingCalculator: TdxLineSpacingCalculatorBase;
begin
  Result := TdxLineSpacingCalculatorBase.GetLineSpacingCalculator(FParagraph);
end;

procedure TdxRowsController.CreateNewCurrentRow(APrevCurrentRow: TdxRow; AAfterRestart: Boolean);
var
  ANewRow: TdxRow;
  APrevBottom: Integer;
begin
  ANewRow := ColumnController.CreateRow;
  APrevBottom := CalcPrevBottom(APrevCurrentRow, AAfterRestart, ANewRow);
  ANewRow.Bounds := CalcDefaultRowBounds(APrevBottom);
  CurrentRow := ANewRow;
  InitCurrentRow(False);
end;

procedure TdxRowsController.CreateNewCurrentRowAfterRestart(ASection: TdxSection; AColumn: TdxColumn);
var
  ALastRow: TdxRow;
  APrevColumn: TdxColumn;
  APrevParagraph: TdxParagraph;
  APrevParagraphIndex: TdxParagraphIndex;
  ARows: TdxRowCollection;
begin
  ARows := AColumn.GetOwnRows;
  try
    ALastRow := ARows.Last;
    InitialLineNumber := ASection.LineNumbering.StartingLineNumber;
    if ALastRow = nil then
    begin
      APrevColumn := ColumnController.GetPreviousColumn(AColumn);
      if (APrevColumn = nil) or APrevColumn.IsEmpty then
        LineNumber := InitialLineNumber
      else
        LineNumber := Abs(APrevColumn.Rows.Last.LineNumber);
    end
    else
      LineNumber := Abs(ALastRow.LineNumber);
    APrevParagraphIndex := FParagraph.Index - 1;
    if APrevParagraphIndex >= 0 then
    begin
      APrevParagraph := PieceTable.Paragraphs[APrevParagraphIndex];
      if not APrevParagraph.SuppressLineNumbers and not APrevParagraph.IsInCell then
        LineNumber := LineNumber + 1;
    end;
    LineNumberStep := ASection.LineNumbering.Step;
    CreateNewCurrentRow(ALastRow, True);
  finally
    if AColumn.Rows <> ARows then
      ARows.Free;
  end;
end;

function TdxRowsController.CreateTableGridCalculator(ADocumentModel: TdxDocumentModel;
  AWidthsCalculator: TdxTableWidthsCalculator; AMaxTableWidth: Integer): TdxTableGridCalculator;
begin
  Result := TdxTableGridCalculator.Create(AWidthsCalculator, AMaxTableWidth,
    DocumentModel.LayoutOptions.PrintLayoutView.AllowTablesToExtendIntoMargins, False);
end;

function TdxRowsController.CreateTablesController: TdxTablesController;
begin
  Result := TdxTablesController.Create(ColumnController.PageAreaController.PageController, Self);
end;

procedure TdxRowsController.EndParagraph;
var
  ASpacingAfter: Integer;
  ALastRow: TdxRow;
begin
  ASpacingAfter := GetContextualSpacingAfter(FParagraph);
  if FInvisibleEmptyParagraphInCellAfterNestedTable then
    ASpacingAfter := 0;
  ALastRow := IncreaseLastRowHeight(ASpacingAfter);
  ALastRow.LastParagraphRowOriginalHeight := ALastRow.Height - ASpacingAfter;
  ALastRow.ProcessingFlags := ALastRow.ProcessingFlags + [TdxRowProcessingFlag.LastParagraphRow];
  TablesController.EndParagraph(ALastRow);
  OffsetCurrentRow(ASpacingAfter);
  FSpacingAfterPrevParagraph := ASpacingAfter;
  FIsSpacingAfterPrevParagraphValid := True;
end;

procedure TdxRowsController.EndRow(AFormatter: TdxParagraphBoxFormatter);
begin
  EndRowCore(FParagraph, AFormatter);
end;

procedure TdxRowsController.EndRowCore(AParagraph: TdxParagraph; AFormatter: TdxParagraphBoxFormatter);
var
  ADescent: Integer;
  ALineSpacing: Integer;
  R: TRect;
  ASpaceBefore: Integer;
  AHeaderOffset, AMaxHeaderBottom, AFooterOffset, AMaxFooterBottom: Integer;
  ANewRow: TdxRow;
begin
  HorizontalPositionController.IncrementCurrentHorizontalPosition(FTabsController.CalcLastTabWidth(CurrentRow, AFormatter));
  CurrentRow.Paragraph := AParagraph;
  if ShouldAddParagraphBackgroundFrameBox(AParagraph) then
    CurrentColumn.AddParagraphFrame(AParagraph);
  FForceFormattingComplete := False;

  if (CurrentColumn.Rows.Count > 0) and not AParagraph.HasParagraphFrame and not AParagraph.IsInCell then
  begin
    if PieceTable.IsHeader then
    begin
      AHeaderOffset := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ColumnController.PageAreaController.PageController.CurrentSection.Margins.HeaderOffset);
      AMaxHeaderBottom := (ColumnController.PageAreaController.PageController.PageBounds.Height div 2) + AHeaderOffset;
      FForceFormattingComplete := CurrentRow.Bounds.Bottom > AMaxHeaderBottom;
    end;
    if PieceTable.IsFooter then
    begin
      AFooterOffset := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ColumnController.PageAreaController.PageController.CurrentSection.Margins.FooterOffset);
      AMaxFooterBottom := ColumnController.PageAreaController.PageController.PageBounds.Bottom - AFooterOffset;
      FForceFormattingComplete := CurrentRow.Bounds.Bottom > AMaxFooterBottom;
    end;
  end;

  if ForceFormattingComplete then
  begin
    FCurrentRowIndent := FRegularRowIndent;
    FCurrentRowIndentAfterFloatingObject := FCurrentRowIndent;
    TablesController.OnCurrentRowFinished;
    HorizontalPositionController.OnCurrentRowFinished;
    ANewRow := TdxRow.Create;
    ANewRow.Bounds := CalcDefaultRowBounds(CurrentRow.Bounds.Top);
    CurrentRow := ANewRow;
    InitCurrentRow(False);
  end
  else
  begin
    CurrentColumn.Rows.Add(CurrentRow);
    if CurrentRow.ContainsFootNotes then
    begin
    end;
    if not AParagraph.SuppressLineNumbers and not TablesController.IsInsideTable then
      Inc(FLineNumber);

    if ShouldUseMaxDescent then
      ADescent := FMaxDescent
    else
      ADescent := 0;

    ALineSpacing := FLineSpacingCalculator.CalculateSpacing(CurrentRow.Bounds.Height, FMaxAscentAndFree, ADescent, FMaxPictureHeight);
    R := CurrentRow.Bounds;
    ASpaceBefore := CurrentRow.SpacingBefore;
    R.Y := R.Y - ASpaceBefore;
    R.Height := ALineSpacing + ASpaceBefore;
    CurrentRow.Bounds := R;

    FCurrentRowIndent := FRegularRowIndent;
    FCurrentRowIndentAfterFloatingObject := FCurrentRowIndent;
    TablesController.OnCurrentRowFinished;
    HorizontalPositionController.OnCurrentRowFinished;
    CreateNewCurrentRow(CurrentRow, False);
  end;
end;

procedure TdxRowsController.EndSection;
var
  ANextParagraphIndex: TdxParagraphIndex;
begin
  EndParagraph;
  ANextParagraphIndex := FParagraph.Index + 1;
  if ANextParagraphIndex < PieceTable.Paragraphs.Count then
    RaiseBeginNextSectionFormatting(FPieceTable.LookupSectionIndexByParagraphIndex(ANextParagraphIndex));
  ColumnController.PageAreaController.PageController.NextSection := True;
  CompleteCurrentColumnFormatting;
  MoveRowToNextColumn;
end;

procedure TdxRowsController.EnforceNextRowToStartFromNewPage;
begin
  if CurrentColumn <> nil then
  begin
    CurrentRow.Bounds := CalcDefaultRowBounds(CurrentColumn.Bounds.Bottom);
    HorizontalPositionController.OnCurrentRowHeightChanged(False);
  end;
end;

procedure TdxRowsController.EnsureSpacingAfterPrevParagraphValid(ACurrentParagraph: TdxParagraph);
var
  APrevParagraphIndex: TdxParagraphIndex;
begin
  if FIsSpacingAfterPrevParagraphValid then
    Exit;
  APrevParagraphIndex := ACurrentParagraph.Index - 1;
  FSpacingAfterPrevParagraph := GetActualSpacingAfter(APrevParagraphIndex);
  FIsSpacingAfterPrevParagraphValid := True;
end;

procedure TdxRowsController.ExpandLastTab(AFormatter: TdxParagraphBoxFormatter);
var
  AOffset: Integer;
begin
  AOffset := FTabsController.CalcLastTabWidth(CurrentRow, AFormatter);
  HorizontalPositionController.IncrementCurrentHorizontalPosition(AOffset);
end;

procedure TdxRowsController.FormatFloatingObjectTextBox(AFloatingObjectBox: TdxFloatingObjectBox;
  AContent: TdxTextBoxFloatingObjectContent; AAnchorBox: TdxFloatingObjectAnchorBox);
var
  AActualSize: Integer;
  ATextBoxArea: TdxPageArea;
  ATextBoxContent, AActualPageAreaBounds, AFinalColumnBounds: TRect;
  APrinter: TdxTextBoxFloatingObjectContentPrinter;
begin
  APrinter := TdxTextBoxFloatingObjectContentPrinter.Create(AContent.TextBox,
    ColumnController.PageAreaController.PageController.Pages.Last.PageNumberSource, ColumnController.Measurer);
  try
    ATextBoxContent := AFloatingObjectBox.GetTextBoxContentBounds;
    APrinter.ColumnLocation := ATextBoxContent.Location;
    APrinter.ColumnSize := GetColumnSize(AAnchorBox, AContent, ATextBoxContent, False);
    AActualSize := APrinter.Format(APrinter.ColumnLocation.Y + APrinter.ColumnSize.Height);
    if APrinter.FrameParagraphIndex <> - 1 then
      FrameParagraphIndex := APrinter.FrameParagraphIndex;
    Dec(AActualSize, APrinter.ColumnLocation.Y);
    AFloatingObjectBox.DocumentLayout := APrinter.DocumentLayout;
    if AContent.TextBoxProperties.ResizeShapeToFitText then
      ResizeFloatingObjectBoxToFitText(AFloatingObjectBox, AContent, AAnchorBox, ATextBoxContent, AActualSize);

    ATextBoxArea := AFloatingObjectBox.DocumentLayout.Pages.First.Areas.First;
    AActualPageAreaBounds := ATextBoxArea.Columns.First.Bounds;
    AActualPageAreaBounds.Height := AActualSize;
    ATextBoxArea.Bounds := AActualPageAreaBounds;

    AFinalColumnBounds := ATextBoxArea.Columns.First.Bounds;
    AFinalColumnBounds.Size := GetColumnSize(AAnchorBox, AContent, ATextBoxContent, True);
    ATextBoxArea.Columns.First.Bounds := AFinalColumnBounds;
  finally
    APrinter.Free;
  end;
end;

function TdxRowsController.GetActualSpacingAfter(AParagraphIndex: TdxParagraphIndex): Integer;
var
  AParagraph: TdxParagraph;
begin
  Result := 0;
  if AParagraphIndex < 0 then
    Exit;
  AParagraph := PieceTable.Paragraphs[AParagraphIndex];
  if not ShouldIgnoreParagraphHeight(AParagraph) then
    Result := GetContextualSpacingAfter(AParagraph);
end;

function TdxRowsController.GetActualTabInfo(const ATabInfo: TdxTabInfo; AProperties: TdxListLevelProperties): TdxTabInfo;
var
  AIndentTabPosition, ATabPosition: Integer;
begin
  if Paragraph.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging then
  begin
    Result := TdxTabInfo.Create(Paragraph.LeftIndent);
    AIndentTabPosition := Result.GetLayoutPosition(DocumentModel.ToDocumentLayoutUnitConverter);
    ATabPosition := ATabInfo.GetLayoutPosition(DocumentModel.ToDocumentLayoutUnitConverter);
    if not ATabInfo.IsDefault and (ATabPosition <= AIndentTabPosition) then
      Result := ATabInfo;
  end
  else
    Result := ATabInfo;
end;

function TdxRowsController.GetBox(ABoxType: TdxBoxClass; ABoxInfo: TdxBoxInfo): TdxBox;
begin
  if (ABoxInfo.Box <> nil) and not (ABoxInfo.Box is TdxFloatingObjectAnchorBox) and not (ABoxInfo.Box is TdxParagraphFrameBox) then
  begin
    Result := TdxBox(ABoxInfo.Box);
    Assert(Result.StartPos = ABoxInfo.StartPos);
    Assert(Result.EndPos = ABoxInfo.EndPos);
  end
  else
  begin
    Result := ABoxType.CreateBox;
    Result.StartPos := ABoxInfo.StartPos;
    Result.EndPos := ABoxInfo.EndPos;
    FBoxes.Add(Result);
  end;
end;

function TdxRowsController.GetCurrentHorizontalPosition: Integer;
begin
  Result := FHorizontalPositionController.CurrentHorizontalPosition;
end;

function TdxRowsController.GetMaxBoxWidth: Integer;
begin
  Result := HorizontalPositionController.GetMaxBoxWidth;
end;

function TdxRowsController.GetParagraphFrameColumnSize(AParagraphFrameBox: TdxParagraphFrameBox): TSize;
var
  ABounds: TRect;
  AMergedFrameProperties: TdxMergedFrameProperties;
begin
  ABounds := AParagraphFrameBox.ContentBounds;
  Result.Init(0, MaxInt div 4);
  AMergedFrameProperties := AParagraphFrameBox.GetParagraph.GetMergedFrameProperties;
  try
    if AMergedFrameProperties.Info.Width = 0 then
      Result.cx := CurrentColumn.ActualSizeBounds.Width
    else
      Result.cx := ABounds.Size.Width;
  finally
    AMergedFrameProperties.Free;
  end;
end;

procedure TdxRowsController.ObtainParagraphFirstRowPosition(AParagraph: TdxParagraph);
begin
end;

function TdxRowsController.CanFitFloatingObject(AFloatingObjectAnchorBox: TdxFloatingObjectAnchorBox;
  ANewBoxInfo: TdxBoxInfo; AEmptyColumn: Boolean): TdxCanFitFloatingObjectResult;
var
  ACurrentRun: TdxTextRunBase;
  AObjectAnchorRun, ARun: TdxFloatingObjectAnchorRun;
  AFloatingObjectBox: TdxFloatingObjectBox;
  AController: TdxFloatingObjectSizeAndPositionController;
  ARestartPosition: TdxDocumentModelPosition;
  AObjectAdded: Boolean;
begin
  ACurrentRun := PieceTable.Runs[ANewBoxInfo.StartPos.RunIndex];
  AObjectAnchorRun := TdxFloatingObjectAnchorRun(ACurrentRun);
  AFloatingObjectBox := nil;
  if FloatingObjectsLayout.ContainsRun(AObjectAnchorRun) then
  begin
    AFloatingObjectBox := FloatingObjectsLayout.GetFloatingObject(AObjectAnchorRun);
    if AFloatingObjectBox.LockPosition then
      Exit(TdxCanFitFloatingObjectResult.Always);
    AFloatingObjectBox.WasRestart := False;
  end;
  AController := CreateFloatingObjectSizeAndPositionController;
  try
    AController.UpdateFloatingObjectBox(AFloatingObjectAnchorBox);
  finally
    AController.Free;
  end;

  ACurrentRun := PieceTable.Runs[AFloatingObjectAnchorBox.StartPos.RunIndex];
  ARun := TdxFloatingObjectAnchorRun(ACurrentRun);
  if ARun.ExcludeFromLayout then
    Exit(TdxCanFitFloatingObjectResult.Always);

  if AFloatingObjectBox = nil then
  begin
    AFloatingObjectBox := CreateFloatingObjectBox(AFloatingObjectAnchorBox, AObjectAnchorRun);
    AFloatingObjectAnchorBox.FloatingObjectBox := AFloatingObjectBox;
  end;

  if ARun.FloatingObjectProperties.TextWrapType = TdxFloatingObjectTextWrapType.None then
    Exit(TdxCanFitFloatingObjectResult.Always);

  ARestartPosition := CalculateRestartPositionDueFloatingObject(AFloatingObjectAnchorBox);
  if (ARestartPosition = TdxDocumentModelPosition.Null) or AEmptyColumn then
    Exit(TdxCanFitFloatingObjectResult.DependsOnTable);
  if (ARestartPosition.RunIndex = AFloatingObjectAnchorBox.StartPos.RunIndex) and (ARestartPosition.RunOffset = AFloatingObjectAnchorBox.StartPos.Offset) then
  begin
    LastRestartDueToFloatingObjectModelPosition := ARestartPosition;
    Exit(TdxCanFitFloatingObjectResult.DependsOnTable);
  end;
  AObjectAdded := AddFloatingObjectToLayout(AFloatingObjectAnchorBox, ANewBoxInfo);
  if AObjectAdded and CanRestartDueFloatingObject then
  begin
    AFloatingObjectBox.WasRestart := True;
    RestartModelPosition := ARestartPosition;
    LastRestartDueToFloatingObjectModelPosition := ARestartPosition;
    Exit(TdxCanFitFloatingObjectResult.Never);
  end;
  Result := TdxCanFitFloatingObjectResult.DependsOnTable;
end;

function TdxRowsController.CanFitParagraphFrame(ANewBoxInfo: TdxBoxInfo; AEmptyColumn: Boolean): Boolean;
var
  ACurrentRun: TdxTextRunBase;
  AParagraphFrameBox: TdxParagraphFrameBox;
  ACurrentParagraph: TdxSimpleParagraph;
  AController: TdxFloatingObjectSizeAndPositionController;
  ARestartPosition: TdxDocumentModelPosition;
  AObjectAdded: Boolean;
  AFrameProperties: TdxMergedFrameProperties;
begin
  Result := True;
  ACurrentRun := PieceTable.Runs[ANewBoxInfo.StartPos.RunIndex];
  AParagraphFrameBox := Safe<TdxParagraphFrameBox>.Cast(ANewBoxInfo.Box);
  ACurrentParagraph := ACurrentRun.Paragraph;
  if ParagraphFramesLayout.ContainsParagraph(ACurrentParagraph) then
  begin
    AParagraphFrameBox := ParagraphFramesLayout.GetFloatingObject(ACurrentParagraph);
    if AParagraphFrameBox <> nil then
    begin
      if AParagraphFrameBox.LockPosition then
        Exit;
      AParagraphFrameBox.WasRestart := False;
    end;
  end;

  AController := CreateFloatingObjectSizeAndPositionController;
  try
    AController.UpdateParagraphFrameBox(AParagraphFrameBox);
  finally
    AController.Free;
  end;

  if AEmptyColumn then
    Exit;

  AFrameProperties := AParagraphFrameBox.GetFrameProperties;
  try
   if AFrameProperties.Info.TextWrapType = TdxParagraphFrameTextWrapType.None then
     Exit;
  finally
    AFrameProperties.Free;
  end;

  ARestartPosition := CalculateRestartPositionDueParagraphFrame(AParagraphFrameBox);
  if ARestartPosition = TdxDocumentModelPosition.Null then
    Exit;

  if ARestartPosition.RunIndex = PieceTable.Paragraphs[AParagraphFrameBox.ParagraphIndex].FirstRunIndex then
  begin
    LastRestartDueToFloatingObjectModelPosition := ARestartPosition;
    Exit;
  end;

  AObjectAdded := AddParagraphFrameToLayout(AParagraphFrameBox, ANewBoxInfo);
  if AObjectAdded and CanRestartDueFloatingObject then
  begin
    AParagraphFrameBox.WasRestart := True;
    RestartModelPosition := ARestartPosition;
    LastRestartDueToFloatingObjectModelPosition := ARestartPosition;
    Exit(False);
  end;
end;

procedure TdxRowsController.InnerSetColumnController(const AValue: TdxCustomColumnController);
begin
  if FColumnController <> AValue then
  begin
    TdxCustomColumnController.Release(FColumnController);
    FColumnController := AValue;
    TdxCustomColumnController.AddReference(FColumnController);
  end;
end;

function TdxRowsController.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxRowsController.GetEffectiveLineNumberingRestartType(ASection: TdxSection): TdxLineNumberingRestart;
begin
  Result := ASection.LineNumbering.NumberingRestartType;
end;

function TdxRowsController.GetFontAscentAndFree(ANewBoxInfo: TdxBoxInfo): Integer;
begin
  Result := ANewBoxInfo.GetFontInfo(PieceTable).GetBaseAscentAndFree(DocumentModel);
end;

function TdxRowsController.GetFontAscentAndFree(ARun: TdxTextRunBase): Integer;
begin
  Result := DocumentModel.FontCache[ARun.FontCacheIndex].GetBaseAscentAndFree(DocumentModel);
end;

function TdxRowsController.GetFontDescent(ARun: TdxInlinePictureRun): Integer;
begin
  Result := DocumentModel.FontCache[ARun.FontCacheIndex].GetBaseDescent(DocumentModel);
end;

function TdxRowsController.GetFontDescent(ANewBoxInfo: TdxBoxInfo): Integer;
begin
  Result := ANewBoxInfo.GetFontInfo(PieceTable).GetBaseDescent(DocumentModel);
end;

function TdxRowsController.GetLegacyNumberingListBoxSeparatorWidth(ABox: TdxNumberingListBoxWithSeparator;
  const ABounds: TRect; AProperties: TdxListLevelProperties; AFormatter: TdxParagraphBoxFormatter): Integer;
var
  AStartHorizontalPosition, ALegacySpace, ALegacyIndent, ATextRight: Integer;
begin
  AStartHorizontalPosition := CurrentHorizontalPosition;
  HorizontalPositionController.SetCurrentHorizontalPosition(ABounds.Right);
  ALegacySpace := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(AProperties.LegacySpace);
  ALegacyIndent := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(AProperties.LegacyIndent);
  ATextRight := Max(AStartHorizontalPosition + ALegacyIndent, ABounds.Right + ALegacySpace);
  if FParagraph.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging then
    ATextRight := Max(ATextRight, DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(FParagraph.FirstLineIndent) + AStartHorizontalPosition);
  Result := Math.Max(ATextRight - CurrentHorizontalPosition, 0);
end;

function TdxRowsController.GetNextTab(AFormatter: TdxParagraphBoxFormatter): TdxTabInfo;
var
  APosition, ARoundingFix, ALayoutTabPosition: Integer;
  ATabInfo: TdxTabInfo;
begin
  ExpandLastTab(AFormatter);
  APosition := DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(CurrentHorizontalPosition - CurrentColumn.Bounds.Left);
  ARoundingFix := 0;
  repeat
    ATabInfo := FTabsController.GetNextTab(ARoundingFix + APosition);
    ALayoutTabPosition := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ATabInfo.Position);
    if ALayoutTabPosition + CurrentColumn.Bounds.Left > CurrentHorizontalPosition then
      Exit(ATabInfo)
    else
      Inc(ARoundingFix);
  until False;
end;

function TdxRowsController.GetNumberingListBoxSeparatorWidth(ABox: TdxNumberingListBoxWithSeparator;
  const ABounds: TRect; AProperties: TdxListLevelProperties; AFormatter: TdxParagraphBoxFormatter): Integer;
var
  ATabInfo, AActualTabInfo: TdxTabInfo;
  ATabRight, ATabLeft: Integer;
begin
  HorizontalPositionController.SetCurrentHorizontalPosition(ABounds.Right);
  ATabInfo := GetNextTab(AFormatter);
  AActualTabInfo := GetActualTabInfo(ATabInfo, AProperties);
  ATabRight := AActualTabInfo.GetLayoutPosition(DocumentModel.ToDocumentLayoutUnitConverter);
  ATabLeft := ABounds.Right - CurrentColumn.Bounds.Left;
  Result := ATabRight - ATabLeft;
  if Result < 0 then
  begin
    ATabRight := ATabInfo.GetLayoutPosition(DocumentModel.ToDocumentLayoutUnitConverter);
    Result := ATabRight - ATabLeft;
  end;
end;

function TdxRowsController.GetTextAreaForTable: TdxTextArea;
begin
  Result := HorizontalPositionController.GetTextAreaForTable;
end;

function TdxRowsController.GetTopLevelColumnController: TdxCustomColumnController;
var
  ACurrentController: TdxCustomColumnController;
begin
  ACurrentController := ColumnController;
  repeat
    if ACurrentController is TdxTableCellColumnController then
      ACurrentController := TdxTableCellColumnController(ACurrentController).Parent
    else
    begin
      Result := ACurrentController;
      Exit;
    end;
  until False;
  //Result := nil;
end;

function TdxRowsController.IncreaseLastRowHeight(ADelta: Integer): TdxRow;
var
  ABounds: TRect;
begin
  Assert(ADelta >= 0);
  Result := CurrentColumn.Rows.Last;
  ABounds := Result.Bounds;
  ABounds.Bottom := ABounds.Bottom + ADelta;
  Result.Bounds := ABounds;
end;

procedure TdxRowsController.InitCurrentRow(AKeepTextAreas: Boolean);
begin
  SuppressHorizontalOverfull := False;
  HorizontalPositionController.OnCurrentRowHeightChanged(AKeepTextAreas);
  HorizontalPositionController.SetCurrentRowInitialHorizontalPosition;
  FMaxAscentAndFree := 0;
  FMaxPictureHeight := 0;
  FMaxDescent := 0;
  FCurrentRunIndex := -1;
  CurrentRow.ProcessingFlags := CurrentRow.ProcessingFlags - [TdxRowProcessingFlag.ProcessCharacterLines,
    TdxRowProcessingFlag.ProcessHiddenText,
    TdxRowProcessingFlag.ProcessTextHighlight,
    TdxRowProcessingFlag.ProcessSpecialTextBoxes,
    TdxRowProcessingFlag.ProcessLayoutDependentText];
  FTabsController.ClearLastTabPosition;
  if FTabsController.SingleDecimalTabInTable then
    AddSingleDecimalTabInTable;
  AssignCurrentRowLineNumber;
end;

function TdxRowsController.IsPositionOutsideRightParagraphBound(APos: Integer): Boolean;
begin
  Result := APos > FParagraphRight;
end;

function TdxRowsController.MoveCurrentRowDownToFitTable(ATableWidth, ATableTop: Integer): Integer;
begin
  Result := HorizontalPositionController.MoveCurrentRowDownToFitTable(ATableWidth, ATableTop);
end;

procedure TdxRowsController.MoveCurrentRowToNextColumnCore;
begin
  ClearRow(False);
  CurrentColumn := ColumnController.GetNextColumn(CurrentColumn, False);
  ApplyCurrentColumnBounds(CurrentColumn.Bounds);
  CurrentRow.Bounds := CalcDefaultRowBounds(CurrentColumn.Bounds.Top);
  if ColumnController.ShouldZeroSpacingBeforeWhenMoveRowToNextColumn then
    CurrentRow.SpacingBefore := 0
  else
    OffsetCurrentRow(CurrentRow.SpacingBefore);
  HorizontalPositionController.OnCurrentRowHeightChanged(false);
  if CurrentRow.NumberingListBox <> nil then
  begin
    CurrentRow.NumberingListBox.Bounds := FCurrentParagraphNumberingListBounds;
    CurrentRow.NumberingListBox := nil;
  end;
end;

procedure TdxRowsController.MoveRowToNextColumn;
begin
  TablesController.BeforeMoveRowToNextColumn;
  MoveCurrentRowToNextColumnCore;
  TablesController.AfterMoveRowToNextColumn;
end;

procedure TdxRowsController.MoveRowToNextPage;
begin
  TablesController.BeforeMoveRowToNextPage;
  ColumnController.ResetToFirstColumn;
  MoveCurrentRowToNextColumnCore;
  TablesController.AfterMoveRowToNextPage;
end;

procedure TdxRowsController.NewTableStarted;
begin
  RaiseTableStarted;
end;

procedure TdxRowsController.ObtainRowIndents;
begin
  case Paragraph.FirstLineIndentType of
    TdxParagraphFirstLineIndent.None:
      begin
        FCurrentRowIndent := 0;
        FRegularRowIndent := 0;
        FCurrentRowIndentAfterFloatingObject := 0;
      end;
    TdxParagraphFirstLineIndent.Indented:
      begin
        FCurrentRowIndent := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(Paragraph.FirstLineIndent);
        FRegularRowIndent := 0;
        FCurrentRowIndentAfterFloatingObject := FCurrentRowIndent;
      end;
    TdxParagraphFirstLineIndent.Hanging:
      begin
        FCurrentRowIndent := -DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(Paragraph.FirstLineIndent);
        FRegularRowIndent := 0;
        FCurrentRowIndentAfterFloatingObject := Max(0, DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(Paragraph.LeftIndent) + FCurrentRowIndent);;
      end;
  end;
end;

procedure TdxRowsController.OffsetCurrentRow(AOffset: Integer);
var
  ABounds: TRect;
begin
  Assert(AOffset >= 0);
  ABounds := CurrentRow.Bounds;
  OffsetRect(ABounds, 0, AOffset);
  CurrentRow.Bounds := ABounds;
end;

procedure TdxRowsController.OnCellStart;
begin
  FSpacingAfterPrevParagraph := 0;
  FIsSpacingAfterPrevParagraphValid := True;
end;

procedure TdxRowsController.OnDeferredBeginParagraph;
begin
  ObtainRowIndents;
  RecalcRowBounds;
  HorizontalPositionController.OnCurrentRowHeightChanged(False);
  HorizontalPositionController.SetCurrentRowInitialHorizontalPosition;
end;

procedure TdxRowsController.OnFloatingObjectsLayoutChanged;
begin
  FFloatingObjectsLayout := FColumnController.PageAreaController.PageController.FloatingObjectsLayout;
  Assert(FFloatingObjectsLayout <> nil);
end;

procedure TdxRowsController.OnParagraphFramesLayoutChanged;
begin
  FParagraphFramesLayout := FColumnController.PageAreaController.PageController.ParagraphFramesLayout;
  Assert(FParagraphFramesLayout <> nil);
end;

procedure TdxRowsController.OnPageBreakBeforeParagraph;
begin
  ColumnController.ResetToFirstColumn;
end;

procedure TdxRowsController.OnPageFormattingComplete(ASection: TdxSection; APage: TdxPage);
var
  ASectionIndex: TdxSectionIndex;
begin
  if FSuppressLineNumberingRecalculationForLastPage then
    Exit;
  if ASection.FirstParagraphIndex > FParagraph.Index then
  begin
    ASectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(FParagraph.Index);
    if ASectionIndex >= 0 then
      ASection := DocumentModel.Sections[ASectionIndex];
  end;
  if GetEffectiveLineNumberingRestartType(ASection) = TdxLineNumberingRestart.NewPage then
  begin
    InitialLineNumber := ASection.LineNumbering.StartingLineNumber;
    LineNumberStep := ASection.LineNumbering.Step;
    AssignCurrentRowLineNumber;
  end;
  CreateLineNumberBoxes(ASection, APage);
end;

procedure TdxRowsController.PrepareCurrentRowBounds(AParagraph: TdxParagraph; ABeginFromParagraphStart: Boolean);
begin
  FreeAndNil(FLineSpacingCalculator);
  FLineSpacingCalculator := CreateLineSpacingCalculator;
  FDefaultRowHeight := FLineSpacingCalculator.DefaultRowHeight;
  ObtainRowIndents;
  if not ABeginFromParagraphStart then
    FCurrentRowIndent := FRegularRowIndent;
  FTabsController.BeginParagraph(AParagraph);
  ApplyCurrentColumnBounds(CurrentColumn.Bounds);
  CurrentRow.Bounds := CalcDefaultRowBounds(CurrentRow.Bounds.Top);
end;

procedure TdxRowsController.RaiseBeginNextSectionFormatting(ASectionIndex: TdxSectionIndex);
var
  AArgs: TdxBeginNextSectionFormattingEventArgs;
begin
  if FOnBeginNextSectionFormatting.Empty then
    Exit;

  AArgs := TdxBeginNextSectionFormattingEventArgs.Create(ASectionIndex);
  try
    FOnBeginNextSectionFormatting.Invoke(Self, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxRowsController.RaiseTableStarted;
begin
  if not FOnTableStarted.Empty then
    FOnTableStarted.Invoke(Self, nil);
end;

procedure TdxRowsController.RecalcRowBounds;
var
  ABounds: TRect;
begin
  ABounds := CurrentRow.Bounds;
  CurrentRow.Bounds := TRect.CreateSize(ParagraphLeft + FCurrentRowIndent, ABounds.Top,
    FParagraphRight - ParagraphLeft - FCurrentRowIndent, ABounds.Height);
end;

procedure TdxRowsController.RecreateHorizontalPositionController;
var
  APosition: Integer;
begin
  APosition := FHorizontalPositionController.CurrentHorizontalPosition;
  if (FFloatingObjectsLayout.Items.Count > 0) or (ParagraphFramesLayout.Items.Count > 0) then
  begin
    if FHorizontalPositionController.ClassType <> TdxFloatingObjectsCurrentHorizontalPositionController then
    begin
      FHorizontalPositionController.Free;
      FHorizontalPositionController := TdxFloatingObjectsCurrentHorizontalPositionController.Create(Self, APosition);
    end;
  end
  else
    if FHorizontalPositionController.ClassType <> TdxCurrentHorizontalPositionController then
    begin
      FHorizontalPositionController.Free;
      FHorizontalPositionController := CreateCurrentHorizontalPosition(APosition);
    end;
  FHorizontalPositionController.OnCurrentRowHeightChanged(False);
end;

procedure TdxRowsController.RemoveLastTextBoxes;
var
  ABoxes: TdxBoxCollection;
  ACount: Integer;
begin
  ABoxes := CurrentRow.Boxes;
  ABoxes.DeleteRange(FStartWordBoxIndex, ABoxes.Count - FStartWordBoxIndex);
  CurrentRow.Height := FHeightBeforeWord;
  FMaxAscentAndFree := FMaxAscentBeforeWord;
  FMaxDescent := FMaxDescentBeforeWord;
  FMaxPictureHeight := FMaxPictureHeightBeforeWord;
  FCurrentRunIndex := FRunIndexBeforeWord;
  CurrentRow.ProcessingFlags := FRowProcessingFlagsBeforeWord;
  ACount := ABoxes.Count;
  if ACount > 0 then
    HorizontalPositionController.RollbackCurrentHorizontalPositionTo(ABoxes[ACount - 1].Bounds.Right)
  else
    if CurrentRow.NumberingListBox = nil then
      HorizontalPositionController.SetCurrentRowInitialHorizontalPosition
    else
      HorizontalPositionController.RollbackCurrentHorizontalPositionTo(CurrentRow.NumberingListBox.Bounds.Right);
  FTabsController.UpdateLastTabPosition(ACount);
end;

procedure TdxRowsController.Reset(ASection: TdxSection; AKeepFloatingObjects: Boolean);
begin
  CurrentColumn := nil;
  FColumns.Clear;
  InnerSetColumnController(GetTopLevelColumnController);
  InitialLineNumber := ASection.LineNumbering.StartingLineNumber;
  LineNumberStep := ASection.LineNumbering.Step;
  BeginSectionFormatting(ASection);
  CurrentColumn := ColumnController.GetNextColumn(nil, AKeepFloatingObjects);
  CurrentRow := ColumnController.CreateRow;
  CurrentRow.Bounds := TRect.CreateSize(0, CurrentColumn.Bounds.Top, 0, 0);
  RecreateHorizontalPositionController;
  HorizontalPositionController.OnCurrentRowHeightChanged(False);
  FParagraph := PieceTable.Paragraphs.First;
  AssignCurrentRowLineNumber;
  TablesController.Reset;
end;

procedure TdxRowsController.ResizeFloatingObjectBoxToFitText(AFloatingObjectBox: TdxFloatingObjectBox;
  AContent: TdxTextBoxFloatingObjectContent; AAnchorBox: TdxFloatingObjectAnchorBox; const ATextBoxContent: TRect;
  AActualSize: Integer);
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AFloatingObjectProperties: TdxFloatingObjectProperties;
  ADeltaHeight, ANewActualHeight: Integer;
  ARectangularObject: IdxRectangularScalableObject;
  AVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  AFloatingObjectProperties := AContent.TextBox.AnchorRun.FloatingObjectProperties;
  ADeltaHeight := AActualSize - ATextBoxContent.Height;
  if ADeltaHeight = 0 then
    Exit;

  AFloatingObjectBox.IncreaseHeight(ADeltaHeight);
  AAnchorBox.IncreaseHeight(ADeltaHeight);
  ANewActualHeight := AFloatingObjectProperties.ActualHeight + AUnitConverter.ToModelUnits(ADeltaHeight);
  if ANewActualHeight <> AContent.OriginalSize.Height then
  begin
    ARectangularObject := AContent.TextBox.AnchorRun;
    AFloatingObjectProperties.DisableHistory := True;
    try
      ARectangularObject.ActualSize := TSize.Create(ARectangularObject.ActualSize.Width, ANewActualHeight);
    finally
      AFloatingObjectProperties.DisableHistory := False;
    end;
  end;
  AVerticalPositionAlignment := AFloatingObjectProperties.VerticalPositionAlignment;
  if AVerticalPositionAlignment in [TdxFloatingObjectVerticalPositionAlignment.Bottom, TdxFloatingObjectVerticalPositionAlignment.Center] then
    CorrectFloatingObjectTextBoxVerticalPosition(AFloatingObjectBox, AAnchorBox);
end;

function TdxRowsController.SnapsToModelUnits(AValue: Integer): Integer;
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  Result := AUnitConverter.ToLayoutUnits(AUnitConverter.ToModelUnits(AValue));
end;

procedure TdxRowsController.CorrectFloatingObjectTextBoxVerticalPosition(AFloatingObjectBox: TdxFloatingObjectBox; AAnchorBox: TdxFloatingObjectAnchorBox);
var
  AController: TdxFloatingObjectSizeAndPositionController;
  ADeltaY: Integer;
begin
  AController := TdxFloatingObjectSizeAndPositionController.Create(Self);
  try
    AController.UpdateFloatingObjectBox(AAnchorBox);
  finally
    AController.Free;
  end;
  ADeltaY := AAnchorBox.ContentBounds.Top - AFloatingObjectBox.ContentBounds.Top;
  if ADeltaY <> 0 then
    AFloatingObjectBox.MoveLayoutVertically(ADeltaY);
  ApplyFloatingObjectBoxBounds(AFloatingObjectBox, AAnchorBox);
end;

procedure TdxRowsController.RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; AColumn: TdxColumn);
begin
  FIsSpacingAfterPrevParagraphValid := False;
  CurrentColumn := AColumn;
  CreateNewCurrentRowAfterRestart(ASection, AColumn);
  RecreateHorizontalPositionController;
end;

procedure TdxRowsController.RestartFormattingFromTheStartOfRowAtCurrentPage(ASection: TdxSection; AColumn: TdxColumn);
begin
  FIsSpacingAfterPrevParagraphValid := False;
  if not (CurrentColumn is TdxTableCellColumn) then
  begin
    if AColumn = nil then
      AColumn := ColumnController.GetNextColumn(AColumn, True);
    CurrentColumn := AColumn;
  end;
  CreateNewCurrentRowAfterRestart(ASection, CurrentColumn);
end;

procedure TdxRowsController.RestartFormattingFromTheStartOfSection(ASection: TdxSection; AColumn: TdxColumn);
begin
  CurrentColumn := AColumn;
  CreateNewCurrentRowAfterRestart(ASection, AColumn);
  ApplySectionStart(ASection, ColumnController.TopLevelColumnsCount);
  FSuppressLineNumberingRecalculationForLastPage := True;
  try
    CompleteCurrentColumnFormatting;
    MoveRowToNextColumn;
  finally
    FSuppressLineNumberingRecalculationForLastPage := False;
  end;
end;

procedure TdxRowsController.SetColumnController(ANewColumnController: TdxCustomColumnController);
var
  ANewRow: TdxRow;
begin
  if CurrentRow.Boxes.Count > 0 then
    TdxRichEditExceptions.ThrowInternalException;
  InnerSetColumnController(ANewColumnController);
  ANewRow := ColumnController.CreateRow;
  ANewRow.Bounds := CurrentRow.Bounds;
  ANewRow.LineNumber := CurrentRow.LineNumber;
  CurrentRow := ANewRow;
end;

procedure TdxRowsController.SetCurrentRow(const Value: TdxRow);
begin
  if FCurrentRow <> Value then
  begin
    TdxRow.Release(FCurrentRow);
    FCurrentRow := Value;
    TdxRow.AddReference(FCurrentRow);
  end;
end;

procedure TdxRowsController.SetCurrentColumn(const Value: TdxColumn);
begin
  if FCurrentColumn <> Value then
  begin
    FCurrentColumn := Value;
    if FCurrentColumn <> nil then
      FColumns.Add(FCurrentColumn);
  end;
end;

function TdxRowsController.GetColumnSize(AAnchorBox: TdxFloatingObjectAnchorBox;
  AContent: TdxTextBoxFloatingObjectContent; const ATextBoxContent: TRect; AGetFinalSize: Boolean): TSize;
var
  AObjectAnchorRun: TdxFloatingObjectAnchorRun;
  AAngle: Single;
begin
  if AContent.TextBoxProperties.ResizeShapeToFitText then
    Result.Init(ATextBoxContent.Width, MaxInt div 4)
  else
  begin
    if not AContent.TextBoxProperties.Upright then
      if AGetFinalSize then
        Result := ATextBoxContent.Size
      else
        Result.Init(ATextBoxContent.Width, MaxInt div 4)
    else
    begin
      AObjectAnchorRun := TdxFloatingObjectAnchorRun(PieceTable.Runs[AAnchorBox.StartPos.RunIndex]);
      AAngle := dxFMod(DocumentModel.UnitConverter.ModelUnitsToDegreeF(AObjectAnchorRun.Shape.Rotation), 360);
      if AAngle < 0 then
        AAngle := AAngle + 360;
      if (AAngle < 45) or (AAngle >= 135) and (AAngle < 225) or (AAngle >= 315) then
        Result := ATextBoxContent.Size
      else
        Result.Init(ATextBoxContent.Size.Height, MaxInt div 4);
    end;
  end;
end;

function TdxRowsController.GetTableCell(AHitTestResult: TdxRichEditHitTestResult): TdxTableCell;
begin
  if AHitTestResult.TableCell <> nil then
    Exit(AHitTestResult.TableCell.Cell);
  if AHitTestResult.Row is TdxTableCellRow then
    Exit(TdxTableCellRow(AHitTestResult.Row).CellViewInfo.Cell);
  Result := nil;
end;

procedure TdxRowsController.SetInitialLineNumber(const Value: Integer);
begin
  FInitialLineNumber := Value;
  LineNumber := Value;
end;

procedure TdxRowsController.SetLineNumberStep(const Value: Integer);
begin
  FLineNumberStep := Max(1, Value);
end;

function TdxRowsController.ShouldAddParagraphBackgroundFrameBox(AParagraph: TdxParagraph): Boolean;
begin
  if TdxAlphaColors.IsTransparentOrEmpty(AParagraph.BackColor) then
    Exit(False);

  Result := AParagraph.IsInCell or not AParagraph.HasParagraphFrame;
end;

procedure TdxRowsController.ApplyParagraphFrameBoxBounds(AParagraphFrameBox: TdxParagraphFrameBox;
  ANewParagraphFrameBox: TdxParagraphFrameBox);
begin
  AParagraphFrameBox.ExtendedBounds := ANewParagraphFrameBox.ActualBounds;
  AParagraphFrameBox.Bounds := ANewParagraphFrameBox.ShapeBounds;
  AParagraphFrameBox.ContentBounds := ANewParagraphFrameBox.ContentBounds;
  AParagraphFrameBox.SetActualSizeBounds(ANewParagraphFrameBox.ActualSizeBounds);
end;

procedure TdxRowsController.FormatParagraphFrameTextBox(AParagraphFrameBox: TdxParagraphFrameBox);
var
  ATextBox: TdxTextBoxContentType;
  APrinter: TdxTextBoxFloatingObjectContentPrinter;
  APageArea: TdxPageArea;
  AColumn: TdxColumn;
  ARows: TdxRowCollection;
  ARowsHeight, AElementBoundsWidth: Integer;
  AFrameProperties: TdxMergedFrameProperties;
  AHRule: TdxParagraphFrameHorizontalRule;
  ACalculator: TdxMaxWidthCalculator;
  AActualPageAreaBounds: TRect;
begin
  ATextBox := TdxTextBoxContentType.Create(AParagraphFrameBox.PieceTable);
  try
    APrinter := TdxTextBoxFloatingObjectContentPrinter.Create(ATextBox,
      ColumnController.PageAreaController.PageController.Pages.Last.PageNumberSource, ColumnController.Measurer);
    try
      APrinter.FrameParagraphIndex := AParagraphFrameBox.ParagraphIndex;
      APrinter.ColumnLocation := AParagraphFrameBox.ContentBounds.Location;
      APrinter.ColumnSize := GetParagraphFrameColumnSize(AParagraphFrameBox);
      APrinter.Format(APrinter.ColumnSize.Height);
      FrameParagraphIndex := APrinter.FrameParagraphIndex;

      AParagraphFrameBox.DocumentLayout := APrinter.DocumentLayout;
      APageArea := AParagraphFrameBox.DocumentLayout.Pages.First.Areas.First;
      AColumn := APageArea.Columns.First;
      ARows := AColumn.Rows;
      ARowsHeight := ARows.Last.Bounds.Bottom - ARows.First.Bounds.Top;

      AFrameProperties := AParagraphFrameBox.GetFrameProperties;
      try
        AHRule := AFrameProperties.Info.HorizontalRule;
        if (AHRule = TdxParagraphFrameHorizontalRule.Auto) or
           ((AHRule = TdxParagraphFrameHorizontalRule.AtLeast) and (AParagraphFrameBox.Bounds.Height < ARowsHeight)) then
          SetActualBoundsForEmptyHeightFrameBox(AParagraphFrameBox, ARowsHeight);

        if AFrameProperties.Info.Width = 0 then
        begin
          ACalculator := TdxMaxWidthCalculator.Create;
          try
            AElementBoundsWidth := Max(ACalculator.GetMaxRight(ARows), AColumn.Bounds.Left) - AColumn.Bounds.Left;
            SetActualBoundsForEmptyWidthFrameBox(AParagraphFrameBox, AFrameProperties.Info.HorizontalPositionAlignment, AElementBoundsWidth);
          finally
            ACalculator.Free;
          end;
        end;
      finally
        AFrameProperties.Free;
      end;

      AActualPageAreaBounds := AColumn.Bounds;
      AActualPageAreaBounds.Height := AParagraphFrameBox.ActualSizeBounds.Height;
      APageArea.Bounds := AActualPageAreaBounds;
      AColumn.Bounds := AActualPageAreaBounds;
    finally
      APrinter.Free;
    end;
  finally
    ATextBox.Free;
  end;
end;

procedure TdxRowsController.AddParagraphFrameToLayoutCore(AParagraphFrameBox: TdxParagraphFrameBox; ABoxInfo: TdxBoxInfo);
begin
  AParagraphFrameBox.StartPos := ABoxInfo.StartPos;
  ApplyParagraphFrameBoxBounds(AParagraphFrameBox, Safe<TdxParagraphFrameBox>.Cast(ABoxInfo.Box));
  FormatParagraphFrameTextBox(AParagraphFrameBox);
  ParagraphFramesLayout.AddParagraphFrameBox(AParagraphFrameBox);
  RecreateHorizontalPositionController;
end;

function TdxRowsController.AddParagraphFrameToLayout(AParagrahFrameBox: TdxParagraphFrameBox; ABoxInfo: TdxBoxInfo): Boolean;
var
  ACurrentRun: TdxTextRunBase;
  AParagraph: TdxSimpleParagraph;
  AMergedFrameProperties: TdxMergedFrameProperties;
begin
  ACurrentRun := PieceTable.Runs[ABoxInfo.StartPos.RunIndex];
  Assert(ABoxInfo.Box <> nil);
  AParagraph := ACurrentRun.Paragraph;
  if ParagraphFramesLayout.ContainsParagraph(AParagraph) then
    Result := False
  else
  begin
    AddParagraphFrameToLayoutCore(AParagrahFrameBox, ABoxInfo);
    AMergedFrameProperties := AParagraph.GetMergedFrameProperties;
    try
      if AMergedFrameProperties.Info.TextWrapType <> TdxParagraphFrameTextWrapType.None then
        AParagrahFrameBox.LockPosition := True;
      Result := True;
    finally
      AMergedFrameProperties.Free;
    end;
  end;
end;

function TdxRowsController.ShouldChangeExistingFloatingObjectBounds(AFloatingObjectBox: TdxFloatingObjectBox;
  AAnchorBox: TdxFloatingObjectAnchorBox): Boolean;
var
  AExistingBounds, ANewBounds: TRect;
begin
  AExistingBounds := AFloatingObjectBox.ExtendedBounds;
  ANewBounds := AAnchorBox.ActualBounds;
  Result := not ANewBounds.IsEqual(AExistingBounds);
end;

function TdxRowsController.ShouldIgnoreParagraphHeight(AParagraph: TdxParagraph): Boolean;
var
  ADocumentModel: TdxDocumentModel;
begin
  ADocumentModel := AParagraph.DocumentModel;
  Result := ADocumentModel.IsSpecialEmptyParagraphAfterInnerTable(AParagraph, TablesController.GetCurrentCell) and
    not ADocumentModel.IsCursorInParagraph(AParagraph);
end;

function TdxRowsController.ShouldUseMaxDescent: Boolean;
var
  ACount: Integer;
begin
  Result := True;
  ACount := CurrentRow.Boxes.Count;
  if ACount <= 0 then
    Exit;
  if not (CurrentRow.Boxes.First.GetRun(PieceTable) is TdxInlineObjectRun) then
    Exit;
  if ACount = 1 then
    Exit(False);
  if (ACount = 2) and (CurrentRow.Boxes.Last is TdxParagraphMarkBox) then
    Exit(False);
end;

procedure TdxRowsController.StartNewWord;
begin
  FStartWordBoxIndex := CurrentRow.Boxes.Count;
  FHeightBeforeWord := CurrentRow.Height;
  FMaxAscentBeforeWord := FMaxAscentAndFree;
  FMaxPictureHeightBeforeWord := FMaxPictureHeight;
  FMaxDescentBeforeWord := FMaxDescent;
  FRunIndexBeforeWord := FCurrentRunIndex;
  FRowProcessingFlagsBeforeWord := CurrentRow.ProcessingFlags;
end;

procedure TdxRowsController.TryToRemoveLastTabBox;
var
  ABoxes: TdxBoxCollection;
  AIndex: Integer;
  ATab: TdxTabSpaceBox;
begin
  ABoxes := CurrentRow.Boxes;
  AIndex := ABoxes.Count - 1;
  if ABoxes[AIndex] is TdxTabSpaceBox then
  begin
    ATab := TdxTabSpaceBox(ABoxes[AIndex]);
    HorizontalPositionController.RollbackCurrentHorizontalPositionTo(ATab.Bounds.Left);
    ABoxes.Delete(AIndex);
    FTabsController.UpdateLastTabPosition(AIndex);
  end;
end;

procedure TdxRowsController.UpdateCurrentRowHeight(ABoxInfo: TdxBoxInfo);
var
  ARunIndex: TdxRunIndex;
  APrevRowHeight: Integer;
begin
  ARunIndex := ABoxInfo.StartPos.RunIndex;
  if (ARunIndex = FCurrentRunIndex) and not ABoxInfo.ForceUpdateCurrentRowHeight then
    Exit;
  if not ABoxInfo.ForceUpdateCurrentRowHeight then
    FCurrentRunIndex := ARunIndex;
  UpdateMaxAscentAndDescent(ABoxInfo);
  APrevRowHeight := CurrentRow.Height;
  CurrentRow.Height := FLineSpacingCalculator.CalcRowHeight(CurrentRow.Height, FMaxAscentAndFree + FMaxDescent);
  if APrevRowHeight <> CurrentRow.Height then
    HorizontalPositionController.OnCurrentRowHeightChanged(False);
  TablesController.UpdateCurrentCellHeight;
end;

procedure TdxRowsController.UpdateCurrentRowHeightFromFloatingObject(ARun: TdxFloatingObjectAnchorRun;
  const AMeasurer: IdxObjectMeasurer);
var
  APictureHeight: Integer;
begin
  FMaxAscentAndFree := Max(FMaxAscentAndFree, GetFontAscentAndFree(ARun));
  APictureHeight := Max(FMaxAscentAndFree, AMeasurer.MeasureRectangularObject(ARun).Height);
  CurrentRow.Height := FLineSpacingCalculator.CalcRowHeight(CurrentRow.Height, APictureHeight);
  TablesController.UpdateCurrentCellHeight;
end;

procedure TdxRowsController.UpdateCurrentRowHeightFromInlineObjectRun(ARun: TdxTextRunBase; ABox: TdxBox;
  const AMeasurer: IdxObjectMeasurer);
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := TdxBoxInfo.Create;
  try
    FMaxAscentAndFree := Max(FMaxAscentAndFree, GetFontAscentAndFree(ARun));
    ARun.Measure(ABoxInfo, AMeasurer);
    FMaxPictureHeight := Math.Max(FMaxPictureHeight, ABoxInfo.Size.cy);
    CurrentRow.Height := FLineSpacingCalculator.CalcRowHeightFromInlineObject(CurrentRow.Height, FMaxPictureHeight, FMaxDescent);
    TablesController.UpdateCurrentCellHeight;
  finally
    ABoxInfo.Free;
  end;
end;

procedure TdxRowsController.UpdateCurrentTableCellBottom(AFloatingObjectBox: TdxFloatingObjectBox);
var
  AFloatingObjectProperties: TdxFloatingObjectProperties;
begin
  AFloatingObjectProperties := AFloatingObjectBox.GetFloatingObjectRun.FloatingObjectProperties;
  if AFloatingObjectProperties.TextWrapType <> TdxFloatingObjectTextWrapType.None then
    TablesController.UpdateCurrentCellBottom(AFloatingObjectBox.ExtendedBounds.Bottom);
end;

procedure TdxRowsController.UpdateMaxAscentAndDescent(ABoxInfo: TdxBoxInfo);
begin
  FMaxAscentAndFree := Max(FMaxAscentAndFree, GetFontAscentAndFree(ABoxInfo));
  FMaxDescent := Max(FMaxDescent, GetFontDescent(ABoxInfo));
end;

{ TdxStateParagraphStart }

function TdxStateParagraphStart.CanFitCurrentRowToColumn(ABoxInfo: TdxBoxInfo): TdxCanFitCurrentRowToColumnResult;
begin
  if TdxRowProcessingFlag.LastInvisibleEmptyCellRowAfterNestedTable in RowsController.CurrentRow.ProcessingFlags then
    Result := TdxCanFitCurrentRowToColumnResult.RowFitted
  else
    Result := inherited CanFitCurrentRowToColumn(ABoxInfo);
end;

function TdxStateParagraphStart.ContinueFormat: TdxStateContinueFormatResult;
var
  ACh: Char;
begin
  ACh := Iterator.CurrentChar;
  if (ACh = TdxCharacters.PageBreak) or (ACh = TdxCharacters.ColumnBreak) then
  begin
    if not RowsController.TablesController.IsInsideTable and RowsController.SupportsColumnAndPageBreaks then
    begin
      RowsController.ResetSpacingBefore;
      if ACh = TdxCharacters.PageBreak then
        ChangeState(TdxParagraphBoxFormatterState.RowPageBreakAtParagraphStart)
      else
        ChangeState(TdxParagraphBoxFormatterState.RowColumnBreakAtParagraphStart);
      Exit(TdxStateContinueFormatResult.Success);
    end;
  end;
  InsertNumberingListBox;
  Result := inherited ContinueFormat;
end;

procedure TdxStateParagraphStart.InsertNumberingListBox;
begin
  if Iterator.Paragraph.IsInList then
    AddNumberingListBox
end;

function TdxStateParagraphStart.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.ParagraphStart;
end;

function TdxStateParagraphStart.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.ParagraphStartAfterFloatingObject;
end;

{ TdxStateParagraphStartAfterBreak }

function TdxStateParagraphStartAfterBreak.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.ParagraphStartAfterBreak;
end;

function TdxStateParagraphStartAfterBreak.ContinueFormat: TdxStateContinueFormatResult;
begin
  RowsController.OnDeferredBeginParagraph;
  Result := inherited ContinueFormat;
end;

{ TdxStateParagraphStartAfterFloatingObject }

function TdxStateParagraphStartAfterFloatingObject.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.ParagraphStartAfterFloatingObject;
end;

{ TdxStateParagraphStartFromTheMiddle }

function TdxStateParagraphStartFromTheMiddle.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.ParagraphStartFromTheMiddle;
end;

procedure TdxStateParagraphStartFromTheMiddle.InsertNumberingListBox;
begin
end;

function TdxStateParagraphStartFromTheMiddle.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := &Type;
end;

{ TdxStateParagraphStartFromTheMiddleAfterFloatingObject }

function TdxStateParagraphStartFromTheMiddleAfterFloatingObject.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.ParagraphStartFromTheMiddleAfterFloatingObject;
end;

function TdxStateParagraphStartFromTheMiddleAfterFloatingObject.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := &Type;
end;

{ TdxStateRowWithTextOnlyAfterFirstLeadingTab }

function TdxStateRowWithTextOnlyAfterFirstLeadingTab.GetHyphenationState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextHyphenationFirstSyllableAfterFirstLeadingTab;
end;

function TdxStateRowWithTextOnlyAfterFirstLeadingTab.GetNoHyphenationNextState: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTextSplitAfterFirstLeadingTab;
end;

function TdxStateRowWithTextOnlyAfterFirstLeadingTab.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFirstLeadingTab;
end;

function TdxStateRowWithTextOnlyAfterFirstLeadingTab.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFirstLeadingTabAfterFloatingObject;
end;

{ TdxParagraphBoxIterator }

function TdxParagraphBoxIterator.CreatePosition: TdxFormatterPosition;
begin
  Result.Init(0, 0, 0);
end;

function TdxParagraphBoxIterator.GetCurrentBox: TdxBox;
begin
  Result := Paragraph.BoxCollection[FBoxIndex];
end;

function TdxParagraphBoxIterator.GetCurrentPosition: TdxFormatterPosition;
begin
  Result := inherited GetCurrentPosition;
  Result.BoxIndex := BoxIndex;
end;

function TdxParagraphBoxIterator.GetPreviousOffsetPosition: TdxFormatterPosition;
var
  APos: TdxFormatterPosition;
begin
  APos := inherited GetPreviousOffsetPosition;
  if (APos.Offset < CurrentBox.StartPos.Offset) then
  begin
    APos.BoxIndex := BoxIndex - 1;
  end
  else
    APos.BoxIndex := BoxIndex;
  Result := APos;
end;

function TdxParagraphBoxIterator.GetPreviousVisibleRunPosition: TdxFormatterPosition;
var
  APos: TdxFormatterPosition;
begin
  APos := inherited GetPreviousVisibleRunPosition;
  APos.BoxIndex := FBoxIndex - 1;
  Result := APos;
end;

function TdxParagraphBoxIterator.Next: TdxParagraphIteratorResult;
begin
  Result := inherited Next;
  if (Offset > CurrentBox.EndPos.Offset) or (Result <> TdxParagraphIteratorResult.Success) then
    NextBoxCore;
end;

procedure TdxParagraphBoxIterator.NextBox;
begin
  NextBoxCore;
  inherited SetPosition(CurrentBox.StartPos);
end;

procedure TdxParagraphBoxIterator.NextBoxCore;
begin
  if BoxIndex < Paragraph.BoxCollection.Count - 1 then
    BoxIndex := BoxIndex + 1
end;

procedure TdxParagraphBoxIterator.SetNextPosition(const APrevPosition: TdxFormatterPosition);
begin
  SetPosition(APrevPosition.RunIndex, APrevPosition.Offset + 1);
  BoxIndex := APrevPosition.BoxIndex;
end;

procedure TdxParagraphBoxIterator.SetPosition(const APos: TdxFormatterPosition);
begin
  inherited SetPosition(APos);
  BoxIndex := APos.BoxIndex;
end;

{ TdxStateRowTabBase }

procedure TdxStateRowTabBase.AddSuccess(ABoxInfo: TdxBoxInfo);
begin
  Formatter.ApproveFloatingObjects;
  ABoxInfo.Size := cxSize(0, ABoxInfo.Size.cy);
  RowsController.AddTabBox(CurrentTab, ABoxInfo);
  TdxTabsController.CacheLeader(TdxTabSpaceBox(ABoxInfo.Box), Formatter);
  SwitchToNextState;
end;

function TdxStateRowTabBase.CanUseBox: Boolean;
begin
  Result := True;
end;

procedure TdxStateRowTabBase.ChangeStateManuallyIfNeeded(AIteratorResult: TdxParagraphIteratorResult);
begin
  SwitchToNextState;
end;

function TdxStateRowTabBase.ColumnOverfull(ACanFit: TdxCanFitCurrentRowToColumnResult; ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowTabBase.CurrentTabPosition: Integer;
begin
  Result := CurrentTab.GetLayoutPosition(Formatter.DocumentModel.ToDocumentLayoutUnitConverter);
end;

function TdxStateRowTabBase.IsTerminatorChar(ACh: Char): Boolean;
begin
  Result := True;
end;

procedure TdxStateRowTabBase.MeasureBoxContent(ABoxInfo: TdxBoxInfo);
begin
  Formatter.Measurer.MeasureTab(ABoxInfo);
end;

{ TdxStateRowTab }

function TdxStateRowTab.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  CurrentTab := RowsController.GetNextTab(Formatter);
  if not CurrentTab.IsDefault then
    Result := TdxAddBoxResult.Success
  else
    if not RowsController.SuppressHorizontalOverfull and
      RowsController.IsPositionOutsideRightParagraphBound(CurrentTabPosition + RowsController.CurrentColumn.Bounds.Left) then
      Result := TdxAddBoxResult.HorizontalIntersect
    else
      Result := TdxAddBoxResult.Success;
end;

function TdxStateRowTab.FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := FinalizeColumn(ABoxInfo);
end;

function TdxStateRowTab.FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  FinalizeLine(ABoxInfo);
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxStateRowTab.FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Result := FinalizePage(ABoxInfo);
end;

function TdxStateRowTab.FinishParagraph: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateParagraphMarkBoxInfo;
  try
    ApplyParagraphMarkSize(ABoxInfo);
    FinalizeParagraph(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxStateRowTab.FinishSection: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateSectionMarkBoxInfo;
  try
    ApplyParagraphMarkSize(ABoxInfo);
    FinalizeSection(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxStateRowTab.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTab;
end;

function TdxStateRowTab.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowTabAfterFloatingObject;
end;

function TdxStateRowTab.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Formatter.RollbackToStartOfWord;
  EndRow;
  ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
  Result := TdxCompleteFormattingResult.Success;
end;

procedure TdxStateRowTab.SwitchToNextState;
var
  ANextState: TdxParagraphBoxFormatterState;
begin
  case Iterator.CurrentChar of
    TdxCharacters.Space,
    TdxCharacters.EmSpace,
    TdxCharacters.EnSpace:
      ANextState := TdxParagraphBoxFormatterState.RowSpaces;
    TdxCharacters.TabMark:
      begin
        Formatter.StartNewTab;
        Exit;
      end;
    TdxCharacters.LineBreak:
      ANextState := TdxParagraphBoxFormatterState.RowLineBreak;
    TdxCharacters.PageBreak:
      if RowsController.TablesController.IsInsideTable then
        ANextState := TdxParagraphBoxFormatterState.RowSpaces
      else
        if not RowsController.SupportsColumnAndPageBreaks then
          ANextState := TdxParagraphBoxFormatterState.RowLineBreak
        else
          ANextState := TdxParagraphBoxFormatterState.RowPageBreak;
    TdxCharacters.ColumnBreak:
      if RowsController.TablesController.IsInsideTable then
        ANextState := TdxParagraphBoxFormatterState.RowSpaces
      else
        if not RowsController.SupportsColumnAndPageBreaks then
          ANextState := TdxParagraphBoxFormatterState.RowLineBreak
        else
          ANextState := TdxParagraphBoxFormatterState.RowColumnBreak;
    TdxCharacters.ParagraphMark:
      if Iterator.IsParagraphMarkRun then
        Exit
      else
        ANextState := TdxParagraphBoxFormatterState.RowText;
    TdxCharacters.SectionMark:
      if Iterator.IsParagraphMarkRun then
        Exit
      else
        ANextState := TdxParagraphBoxFormatterState.RowText;
    TdxCharacters.FloatingObjectMark:
      if Iterator.IsFloatingObjectAnchorRun then
        ANextState := TdxParagraphBoxFormatterState.FloatingObject
      else
        ANextState := TdxParagraphBoxFormatterState.RowText;
  else
    ANextState := TdxParagraphBoxFormatterState.RowText;
  end;
  ChangeState(ANextState);
end;

{ TdxStateRowLeadingTab }

procedure TdxStateRowLeadingTab.ApplyColumnBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert((CurrentRow.NumberingListBox <> nil) or (CurrentRow.NumberingListBox = nil) and (CurrentRow.Height = RowsController.DefaultRowHeight));
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

procedure TdxStateRowLeadingTab.ApplyLineBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert((CurrentRow.NumberingListBox <> nil) or (CurrentRow.NumberingListBox = nil) and (CurrentRow.Height = RowsController.DefaultRowHeight));
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

procedure TdxStateRowLeadingTab.ApplyPageBreakMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert((CurrentRow.NumberingListBox <> nil) or (CurrentRow.NumberingListBox = nil) and (CurrentRow.Height = RowsController.DefaultRowHeight));
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

procedure TdxStateRowLeadingTab.ApplyParagraphMarkSize(ABoxInfo: TdxBoxInfo);
begin
  Assert((CurrentRow.NumberingListBox <> nil) or (CurrentRow.NumberingListBox = nil) and (CurrentRow.Height = RowsController.DefaultRowHeight));
  RowsController.UpdateCurrentRowHeight(ABoxInfo);
end;

function TdxStateRowLeadingTab.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  CurrentTab := RowsController.GetNextTab(Formatter);
  if not RowsController.CanFitBoxToCurrentRow(cxNullSize) then
    Result := TdxAddBoxResult.HorizontalIntersect
  else
    if not CurrentTab.IsDefault then
      Result := TdxAddBoxResult.Success
    else
      if RowsController.IsPositionOutsideRightParagraphBound(CurrentTabPosition + RowsController.CurrentColumn.Bounds.Left) then
        Result := TdxAddBoxResult.HorizontalIntersect
      else
        Result := TdxAddBoxResult.Success;
end;

function TdxStateRowLeadingTab.FinishColumn(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
    Result := FinalizeColumn(ABoxInfo)
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowLeadingTab.FinishLine(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
  begin
    FinalizeLine(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  end
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowLeadingTab.FinishPage(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
    Result := FinalizePage(ABoxInfo)
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowLeadingTab.FinishParagraph: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateParagraphMarkBoxInfo;
  try
    Result := InternalFinishParagraphCore(ABoxInfo, FinalizeParagraph);
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxStateRowLeadingTab.InternalFinishParagraphCore(ABoxInfo: TdxBoxInfo; AFinalizeHandler: TFinalizeEvent): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  ApplyParagraphMarkSize(ABoxInfo);
  ACanFit := RowsController.CanFitCurrentRowToColumn(ABoxInfo);
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
  begin
    AFinalizeHandler(ABoxInfo);
    Result := TdxCompleteFormattingResult.Success;
  end
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

function TdxStateRowLeadingTab.FinishSection: TdxCompleteFormattingResult;
var
  ABoxInfo: TdxBoxInfo;
begin
  ABoxInfo := CreateSectionMarkBoxInfo;
  try
    Result := InternalFinishParagraphCore(ABoxInfo, FinalizeSection);
  finally
    FreeAndNil(ABoxInfo);
  end;
end;

function TdxStateRowLeadingTab.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowLeadingTab;
end;

function TdxStateRowLeadingTab.GetStateAfterFloatingObject: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowLeadingTabAfterFloatingObject;
end;

function TdxStateRowLeadingTab.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
var
  ACanFit: TdxCanFitCurrentRowToColumnResult;
begin
  Formatter.RollbackToStartOfWord;
  SetCurrentRowHeightToLastBoxHeight;
  ACanFit := RowsController.CanFitCurrentRowToColumn;
  if ACanFit = TdxCanFitCurrentRowToColumnResult.RowFitted then
  begin
    EndRow;
    if RowsController.CanFitCurrentRowToColumn <> TdxCanFitCurrentRowToColumnResult.RowFitted then
    begin
      Result := RowsController.CompleteCurrentColumnFormatting;
      if Result <> TdxCompleteFormattingResult.Success then
        Exit;
      RowsController.MoveRowToNextColumn;
    end;
    ChangeState(TdxParagraphBoxFormatterState.RowEmpty);
    Result := TdxCompleteFormattingResult.Success;
  end
  else
    Result := Formatter.RollbackToStartOfRow(ACanFit);
end;

procedure TdxStateRowLeadingTab.SwitchToNextState;
var
  ANextState: TdxParagraphBoxFormatterState;
begin
  case Iterator.CurrentChar of
    TdxCharacters.Space,
    TdxCharacters.EmSpace,
    TdxCharacters.EnSpace:
      ANextState := TdxParagraphBoxFormatterState.RowWithSpacesOnly;
    TdxCharacters.TabMark:
      begin
        Formatter.StartNewTab;
        Exit;
      end;
    TdxCharacters.LineBreak:
      ANextState := TdxParagraphBoxFormatterState.RowLineBreak;
    TdxCharacters.PageBreak:
      if RowsController.TablesController.IsInsideTable then
        ANextState := TdxParagraphBoxFormatterState.RowWithSpacesOnly
      else
        if not RowsController.SupportsColumnAndPageBreaks then
          ANextState := TdxParagraphBoxFormatterState.RowLineBreak
        else
          ANextState := TdxParagraphBoxFormatterState.RowPageBreak;
    TdxCharacters.ColumnBreak:
      if RowsController.TablesController.IsInsideTable then
        ANextState := TdxParagraphBoxFormatterState.RowWithSpacesOnly
      else
        if not RowsController.SupportsColumnAndPageBreaks then
          ANextState := TdxParagraphBoxFormatterState.RowLineBreak
        else
          ANextState := TdxParagraphBoxFormatterState.RowColumnBreak;
    TdxCharacters.ParagraphMark:
      if Iterator.IsParagraphMarkRun then
        Exit
      else
        ANextState := TdxParagraphBoxFormatterState.RowText;
    TdxCharacters.SectionMark:
      if Iterator.IsParagraphMarkRun then
        Exit
      else
        ANextState := TdxParagraphBoxFormatterState.RowText;
    TdxCharacters.FloatingObjectMark:
      if Iterator.IsFloatingObjectAnchorRun then
        ANextState := TdxParagraphBoxFormatterState.FloatingObject
      else
        ANextState := TdxParagraphBoxFormatterState.RowText;
  else
    ANextState := TdxParagraphBoxFormatterState.RowText;
  end;
  ChangeState(ANextState);
end;

{ TdxStateRowFirstLeadingTab }

function TdxStateRowFirstLeadingTab.CanAddBox(ABoxInfo: TdxBoxInfo): TdxAddBoxResult;
begin
  CurrentTab := RowsController.GetNextTab(Formatter);
  Result := TdxAddBoxResult.Success;
end;

function TdxStateRowFirstLeadingTab.GetType: TdxParagraphBoxFormatterState;
begin
  Result := TdxParagraphBoxFormatterState.RowFirstLeadingTab;
end;

function TdxStateRowFirstLeadingTab.HorizontalOverfull(ABoxInfo: TdxBoxInfo): TdxCompleteFormattingResult;
begin
  Assert(False);
  Result := TdxCompleteFormattingResult.Success;
end;

procedure TdxStateRowFirstLeadingTab.SwitchToNextState;
var
  ANextState: TdxParagraphBoxFormatterState;
begin
  case Iterator.CurrentChar of
    TdxCharacters.Space,
    TdxCharacters.EmSpace,
    TdxCharacters.EnSpace:
      ANextState := TdxParagraphBoxFormatterState.RowWithSpacesOnly;
    TdxCharacters.TabMark:
      ANextState := TdxParagraphBoxFormatterState.RowLeadingTab;
    TdxCharacters.LineBreak:
      ANextState := TdxParagraphBoxFormatterState.RowLineBreak;
    TdxCharacters.Dash,
    TdxCharacters.EmDash,
    TdxCharacters.EnDash:
      ANextState := TdxParagraphBoxFormatterState.RowWithDashOnly;
    TdxCharacters.PageBreak:
      if RowsController.TablesController.IsInsideTable then
        ANextState := TdxParagraphBoxFormatterState.RowWithSpacesOnly
      else
        if not RowsController.SupportsColumnAndPageBreaks then
          ANextState := TdxParagraphBoxFormatterState.RowLineBreak
        else
          ANextState := TdxParagraphBoxFormatterState.RowPageBreak;
    TdxCharacters.ColumnBreak:
      if RowsController.TablesController.IsInsideTable then
        ANextState := TdxParagraphBoxFormatterState.RowWithSpacesOnly
      else
        if not RowsController.SupportsColumnAndPageBreaks then
          ANextState := TdxParagraphBoxFormatterState.RowLineBreak
        else
          ANextState := TdxParagraphBoxFormatterState.RowColumnBreak;
    TdxCharacters.ParagraphMark:
      if Iterator.IsParagraphMarkRun then
        Exit
      else
        ANextState := TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFirstLeadingTab;
    TdxCharacters.SectionMark:
      if Iterator.IsParagraphMarkRun then
        Exit
      else
        ANextState := TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFirstLeadingTab;
    TdxCharacters.FloatingObjectMark:
      if Iterator.IsFloatingObjectAnchorRun then
        ANextState := TdxParagraphBoxFormatterState.FloatingObject
      else
        ANextState := TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFirstLeadingTab;
  else
    ANextState := TdxParagraphBoxFormatterState.RowWithTextOnlyAfterFirstLeadingTab;
  end;
  ChangeState(ANextState);
end;

{ TdxUnderlineCalculator }

constructor TdxUnderlineCalculator.Create(APieceTable: TdxPieceTable);
begin
  inherited Create(APieceTable);
  FUnderlineBoxesByType := TdxUnderlineBoxCollection.Create;
end;

destructor TdxUnderlineCalculator.Destroy;
begin
  FreeAndNil(FUnderlineBoxesByType);
  inherited Destroy;
end;

procedure TdxUnderlineCalculator.CenterTabLeaderUnderlineBoxVertically(AUnderlineBox: TdxUnderlineBox; const ABounds: TRect);
var
  AOffset: Integer;
  R: TRect;
begin
  AOffset := (AUnderlineBox.UnderlineBounds.Top - ABounds.Top) div 2 - AUnderlineBox.UnderlineThickness;
  R := AUnderlineBox.UnderlineBounds;
  R.Y := R.Y - AOffset;
  AUnderlineBox.UnderlineBounds := R;

  R := AUnderlineBox.ClipBounds;
  R.Y := R.Y - AOffset;
  AUnderlineBox.ClipBounds := R;
end;

function TdxUnderlineCalculator.CreateTabLeaderUnderlineBox(ABox: TdxTabSpaceBox;
  const ABounds: TRect; ARow: TdxRow): TdxUnderlineBox;
var
  AInfo: TdxUnderlineInfo;
  ABaseLinePosition, AUnderlineTop, AUnderlineBottom: Integer;
begin
  Row := ARow;
  AInfo := CalculateTabLeaderUnderlineInfo(ABox, ABounds);
  Result := TdxUnderlineBox.Create;
  Result.UnderlinePosition := CalculateUnderlinePosition(AInfo.WeightedUnderlinePositionSum, AInfo.TotalUnderlineWidth);
  Result.UnderlineThickness := CalculateUnderlineThickness(AInfo.WeightedUnderlineThicknessSum, AInfo.TotalUnderlineWidth);
  Result.UnderlineBounds := TRect.CreateSize(0, AInfo.TopOffset, 0, 0);
  ABaseLinePosition := ARow.Bounds.Top + ARow.BaseLineOffset + Result.UnderlineBounds.Height;
  AUnderlineTop := ABaseLinePosition - Result.UnderlineThickness;
  AUnderlineBottom := ABaseLinePosition + Result.UnderlineThickness div 2 ;
  Result.UnderlineBounds := TRect.Create(Row.Bounds.Left, AUnderlineTop, Row.Bounds.Right, AUnderlineBottom);
  Result.ClipBounds := TRect.Create(ABounds.Left, AUnderlineTop, ABounds.Right, AUnderlineBottom);
  Result.UnderlinePosition := 0;
end;

procedure TdxUnderlineCalculator.AddBoxToCharacterLine(ABox: TdxBox);
begin
  FActualScript := CurrentScript;
end;

procedure TdxUnderlineCalculator.AfterCalculate;
begin
  inherited AfterCalculate;
  SplitUnderlinesByColor(UnderlineBoxesByType);
  SetUnderlinesBounds;
end;

procedure TdxUnderlineCalculator.BeforeCalculate(ARow: TdxRow);
begin
  inherited BeforeCalculate(ARow);
  ARow.ClearUnderlines;
  UnderlineBoxesByType.Clear;
end;

function TdxUnderlineCalculator.CalculateActualScript(AScript: TdxCharacterFormattingScript): TdxCharacterFormattingScript;
begin
  if AScript in [TdxCharacterFormattingScript.Subscript, TdxCharacterFormattingScript.Normal] then
    Exit(AScript);

  if (CurrentScript = TdxCharacterFormattingScript.Superscript) or (CurrentScript = UnknownScript) then
    Result := TdxCharacterFormattingScript.Superscript
  else
    Result := TdxCharacterFormattingScript.Normal;
end;

procedure TdxUnderlineCalculator.EndCharacterLine(AEndAnchorIndex: Integer; ACurrentFontInfo: TdxFontInfo);
var
  AInfo: TdxUnderlineInfo;
  AUnderlineBox: TdxUnderlineBox;
begin
  AInfo := CalculateUnderlineInfo(AEndAnchorIndex);
  AUnderlineBox := TdxUnderlineBox.Create(FStartAnchorIndex, AEndAnchorIndex - FStartAnchorIndex);
  AUnderlineBox.UnderlinePosition := CalculateUnderlinePosition(AInfo.WeightedUnderlinePositionSum, AInfo.TotalUnderlineWidth);
  AUnderlineBox.UnderlineThickness := CalculateUnderlineThickness(AInfo.WeightedUnderlineThicknessSum, AInfo.TotalUnderlineWidth);
  AUnderlineBox.UnderlineBounds := TRect.CreateSize(0, AInfo.TopOffset, 0, 0);
  FUnderlineBoxesByType.Add(AUnderlineBox);
end;

function TdxUnderlineCalculator.GetCharacterLineTypeNone: TdxUnderlineType;
begin
  Result := TdxUnderlineType.None;
end;

function TdxUnderlineCalculator.GetRunCharacterLineType(ARun: TdxTextRun; ARunIndex: TdxRunIndex): TdxUnderlineType;
begin
  Result := ARun.FontUnderlineType;
end;

function TdxUnderlineCalculator.GetRunCharacterLineUseForWordsOnly(ARun: TdxTextRun; ARunIndex: TdxRunIndex): Boolean;
begin
  Result := ARun.UnderlineWordsOnly;
end;

function TdxUnderlineCalculator.GetNumberingListBoxCharacterLineType(ANumberingListBox: TdxNumberingListBox): TdxUnderlineType;
begin
  Result := ANumberingListBox.GetFontUnderlineType(PieceTable);
end;

function TdxUnderlineCalculator.GetNumberingListBoxCharacterLineUseForWordsOnly(ANumberingListBox: TdxNumberingListBox): Boolean;
var
  ANumerationCharacterProperties: TdxMergedCharacterProperties;
begin
  ANumerationCharacterProperties := ANumberingListBox.GetNumerationCharacterProperties(PieceTable);
  try
    Result := ANumerationCharacterProperties.Info.UnderlineWordsOnly;
  finally
    ANumerationCharacterProperties.Free;
  end;
end;

function TdxUnderlineCalculator.IsCharacterLineBreak(ACharacterLineTypeChanged,
  ARunChanged: Boolean; ABoxScript: TdxCharacterFormattingScript): Boolean;
var
  AShouldBreakCharacterLineByScript: Boolean;
begin
  AShouldBreakCharacterLineByScript := ShouldBreakCharacterLineByScript(ABoxScript);
  Result := ACharacterLineTypeChanged or AShouldBreakCharacterLineByScript or
    ((CurrentScript = TdxCharacterFormattingScript.Subscript) and ARunChanged);
end;

function TdxUnderlineCalculator.ShouldBreakCharacterLineByScript(AScript: TdxCharacterFormattingScript): Boolean;
begin
  if CurrentScript = UnknownScript then
    Exit(True);
  if (CurrentScript = TdxCharacterFormattingScript.Subscript) and (AScript = TdxCharacterFormattingScript.Subscript) then
    Exit(False);
  Result := (CurrentScript = TdxCharacterFormattingScript.Subscript) or (AScript = TdxCharacterFormattingScript.Subscript);
end;

procedure TdxUnderlineCalculator.StartCharacterLine(AStartAnchorIndex: Integer);
begin
  FActualScript := CurrentScript;
  FStartAnchorIndex := AStartAnchorIndex;
end;

procedure TdxUnderlineCalculator.AddNewUnderlineBoxToRow(ASourceUnderlineBox: TdxUnderlineBox; AStartIndex: Integer; AEndIndex: Integer);
var
  ANewUnderlineBox: TdxUnderlineBox;
begin
  ANewUnderlineBox := TdxUnderlineBox.Create(AStartIndex, AEndIndex - AStartIndex);
  ANewUnderlineBox.UnderlinePosition := ASourceUnderlineBox.UnderlinePosition;
  ANewUnderlineBox.UnderlineThickness := ASourceUnderlineBox.UnderlineThickness;
  ANewUnderlineBox.UnderlineBounds := ASourceUnderlineBox.UnderlineBounds;
  Row.Underlines.Add(ANewUnderlineBox);
end;

function TdxUnderlineCalculator.CalculateTabLeaderUnderlineInfo(ABox: TdxTabSpaceBox; const ABounds: TRect): TdxUnderlineInfo;
var
  AFontInfo: TdxFontInfo;
  ABoxWidth: Integer;
begin
  Result.Init;
  AFontInfo := ABox.GetFontInfo(PieceTable);
  ABoxWidth := ABounds.Width;
  Result.TotalUnderlineWidth := Result.TotalUnderlineWidth + ABoxWidth;
  Result.WeightedUnderlinePositionSum := Result.WeightedUnderlinePositionSum + AFontInfo.UnderlinePosition * ABoxWidth;
  Result.WeightedUnderlineThicknessSum := Result.WeightedUnderlineThicknessSum + AFontInfo.UnderlineThickness * ABoxWidth;
end;

function TdxUnderlineCalculator.CalculateUnderlineInfo(AEndAnchorIndex: Integer): TdxUnderlineInfo;
var
  I, ABoxWidth, AMaxTopOffset, AMinTopOffset, AUnderlinePositionForMaxTopOffset, ATopOffset: Integer;
  AFontInfo: TdxFontInfo;
begin
  Result.Init;
  AMaxTopOffset := MinInt;
  AMinTopOffset := MaxInt;
  AUnderlinePositionForMaxTopOffset := 0;
  for I := FStartAnchorIndex to AEndAnchorIndex - 1 do
  begin
    if (FActualScript = TdxCharacterFormattingScript.Normal) and (GetBoxScript(I) = TdxCharacterFormattingScript.Superscript) then
      Continue;
    AFontInfo := GetBoxFontInfo(I);
    ATopOffset := CalculateScriptOffset(AFontInfo, FActualScript);
    AMinTopOffset := Min(AMaxTopOffset, ATopOffset);
    if ATopOffset > AMaxTopOffset then
    begin
      AMaxTopOffset := ATopOffset;
      AUnderlinePositionForMaxTopOffset := AFontInfo.UnderlinePosition;
    end;
    ABoxWidth := Boxes[I].Bounds.Width;
    Result.TotalUnderlineWidth := Result.TotalUnderlineWidth + ABoxWidth;
    Result.WeightedUnderlinePositionSum := Result.WeightedUnderlinePositionSum +
      AFontInfo.UnderlinePosition * ABoxWidth;
    Result.WeightedUnderlineThicknessSum := Result.WeightedUnderlineThicknessSum +
      AFontInfo.UnderlineThickness * ABoxWidth;
  end;
  if Result.TopOffset = MinInt then
    Result.TopOffset := 0
  else
  begin
    Result.TopOffset := AMaxTopOffset;
    if AMinTopOffset <> AMaxTopOffset then
      Result.WeightedUnderlinePositionSum := AUnderlinePositionForMaxTopOffset * Result.TotalUnderlineWidth;
  end;
end;

class function TdxUnderlineCalculator.CalculateUnderlinePosition(AWeightedUnderlinePositionSum, ATotalUnderlineWidth: Integer): Integer;
begin
  if ATotalUnderlineWidth <> 0 then
    Result := Round(AWeightedUnderlinePositionSum / ATotalUnderlineWidth)
  else
    Result := 0;
end;

class function TdxUnderlineCalculator.CalculateUnderlineThickness(AWeightedUnderlineThicknessSum, ATotalUnderlineWidth: Integer): Integer;
begin
  if ATotalUnderlineWidth <> 0 then
    Result := Max(Round(AWeightedUnderlineThicknessSum / ATotalUnderlineWidth), 1)
  else
    Result := 1;
end;

function TdxUnderlineCalculator.GetUnderlineLeft(AUnderlineBox: TdxUnderlineBox): Integer;
begin
  Result := Boxes[AUnderlineBox.StartAnchorIndex].Bounds.Left;
end;

function TdxUnderlineCalculator.GetUnderlineRight(AUnderlineBox: TdxUnderlineBox): Integer;
var
  AEndAnchorIndex, ABoxesCount: Integer;
begin
  AEndAnchorIndex := AUnderlineBox.EndAnchorIndex;
  ABoxesCount := Boxes.Count;
  if AEndAnchorIndex < ABoxesCount then
    Result := Boxes[AEndAnchorIndex].Bounds.Left
  else
    Result := Boxes[ABoxesCount - 1].Bounds.Right;
end;

procedure TdxUnderlineCalculator.SetUnderlineBoxBounds(AUnderlineBox: TdxUnderlineBox);
var
  AUnderlineLeft, AUnderlineRight, ABaseLinePosition, AUnderlineTop, AUnderlineBottom, ABottomDistance: Integer;
begin
  AUnderlineLeft := GetUnderlineLeft(AUnderlineBox);
  AUnderlineRight := GetUnderlineRight(AUnderlineBox);

  ABaseLinePosition := Row.Bounds.Top + Row.BaseLineOffset + AUnderlineBox.UnderlineBounds.Top;
  AUnderlineTop := ABaseLinePosition + AUnderlineBox.UnderlinePosition div 2;
  AUnderlineBottom := ABaseLinePosition + AUnderlineBox.UnderlinePosition + AUnderlineBox.UnderlineThickness;

  ABottomDistance := Row.Bounds.Bottom - AUnderlineBottom;
  if ABottomDistance < 0 then
  begin
    Inc(AUnderlineTop, ABottomDistance);
    Inc(AUnderlineBottom, ABottomDistance);
  end;

  AUnderlineBox.UnderlineBounds := TRect.Create(Row.Bounds.Left, AUnderlineTop, Row.Bounds.Right, AUnderlineBottom);
  AUnderlineBox.ClipBounds := TRect.Create(AUnderlineLeft, AUnderlineTop, AUnderlineRight, AUnderlineBottom);
  AUnderlineBox.UnderlinePosition := AUnderlineBox.UnderlinePosition - AUnderlineBox.UnderlinePosition div 2;
end;

procedure TdxUnderlineCalculator.SetUnderlinesBounds;
var
  AUnderlines: TdxUnderlineBoxCollection;
  I: Integer;
begin
  AUnderlines := Row.InnerUnderlines;
  if AUnderlines <> nil then
    for I := 0 to AUnderlines.Count - 1 do
      SetUnderlineBoxBounds(AUnderlines[I]);
end;

procedure TdxUnderlineCalculator.SplitUnderlineBoxByColor(AUnderlineBox: TdxUnderlineBox);
var
  AStartAnchorIndex, AEndAnchorIndex, AStartIndex, I: Integer;
  ABox: TdxBox;
  ACurrentRunIndex, ABoxRunIndex: TdxRunIndex;
  ACurrentUnderlineColor, ABoxUnderlineColor: TdxAlphaColor;
begin
  AStartAnchorIndex := AUnderlineBox.StartAnchorIndex;
  AEndAnchorIndex := AUnderlineBox.EndAnchorIndex;

  AStartIndex := AStartAnchorIndex;
  ABox := Boxes[AStartIndex];

  ACurrentRunIndex := ABox.StartPos.RunIndex;
  ACurrentUnderlineColor := ABox.GetActualUnderlineColor(PieceTable, TdxTextColors.Defaults, TdxAlphaColors.Empty);
  for I := AStartAnchorIndex + 1 to AEndAnchorIndex - 1 do
  begin
    ABox := Boxes[I];
    ABoxRunIndex := ABox.StartPos.RunIndex;
    if ABoxRunIndex = ACurrentRunIndex then
      Continue;
    ACurrentRunIndex := ABoxRunIndex;
    ABoxUnderlineColor := ABox.GetActualUnderlineColor(PieceTable, TdxTextColors.Defaults, TdxAlphaColors.Empty);
    if ABoxUnderlineColor <> ACurrentUnderlineColor then
    begin
      AddNewUnderlineBoxToRow(AUnderlineBox, AStartIndex, I);
      AStartIndex := I;
      ACurrentUnderlineColor := ABoxUnderlineColor;
    end;
  end;
  AddNewUnderlineBoxToRow(AUnderlineBox, AStartIndex, AEndAnchorIndex);
end;

procedure TdxUnderlineCalculator.SplitUnderlinesByColor(AUnderlines: TdxUnderlineBoxCollection);
var
  I: Integer;
begin
  for I := 0 to AUnderlines.Count - 1 do
    SplitUnderlineBoxByColor(AUnderlines[I]);
end;


procedure TdxStrikeoutCalculator.BeforeCalculate(ARow: TdxRow);
begin
  inherited BeforeCalculate(ARow);
  ARow.ClearStrikeouts;
end;

function TdxStrikeoutCalculator.GetCharacterLineTypeNone: TdxStrikeoutType;
begin
  Result := TdxStrikeoutType.None;
end;

function TdxStrikeoutCalculator.GetRunCharacterLineType(ARun: TdxTextRun; ARunIndex: TdxRunIndex): TdxStrikeoutType;
begin
  Result := ARun.FontStrikeoutType;
end;

function TdxStrikeoutCalculator.GetRunCharacterLineUseForWordsOnly(ARun: TdxTextRun; ARunIndex: TdxRunIndex): Boolean;
begin
  Result := ARun.StrikeoutWordsOnly;
end;

procedure TdxStrikeoutCalculator.EndCharacterLine(AEndAnchorIndex: Integer; AFontInfo: TdxFontInfo);
var
  AStrikeoutBox: TdxUnderlineBox;
  ATop, ARight: Integer;
  AStrikeoutBounds, AClipBounds: TRect;
begin
  AStrikeoutBox := TdxUnderlineBox.Create(FStartAnchorIndex, AEndAnchorIndex - FStartAnchorIndex);
  Row.Strikeouts.Add(AStrikeoutBox);
  ATop := CalculateStrikeoutBoxTop(AFontInfo);
  AStrikeoutBounds := Row.Bounds;
  AStrikeoutBounds.Top := ATop;
  AStrikeoutBounds.Bottom := ATop;
  if AEndAnchorIndex < BoxCount then
    ARight := Boxes[AEndAnchorIndex].Bounds.Left
  else
    ARight := Boxes[BoxCount - 1].Bounds.Right;
  AClipBounds := TRect.Create(Boxes[FStartAnchorIndex].Bounds.Left, ATop, ARight, ATop);

  AStrikeoutBox.UnderlineBounds := AStrikeoutBounds;
  AStrikeoutBox.ClipBounds := AClipBounds;
  AStrikeoutBox.UnderlinePosition := AFontInfo.StrikeoutPosition;
  AStrikeoutBox.UnderlineThickness := AFontInfo.StrikeoutThickness;
end;

function TdxStrikeoutCalculator.IsCharacterLineBreak(ACharacterLineTypeChanged,
  ARunChanged: Boolean; ABoxScript: TdxCharacterFormattingScript): Boolean;
begin
  Result := ACharacterLineTypeChanged or ARunChanged;
end;

procedure TdxStrikeoutCalculator.StartCharacterLine(AStartAnchorIndex: Integer);
begin
  FStartAnchorIndex := AStartAnchorIndex;
end;

function TdxStrikeoutCalculator.CalculateStrikeoutBoxTop(AFontInfo: TdxFontInfo): Integer;
begin
  Result := Row.Bounds.Top + Row.BaseLineOffset - AFontInfo.StrikeoutPosition;
  Inc(Result, CalculateScriptOffset(AFontInfo, GetBoxScript(FStartAnchorIndex)));
end;

function TdxStrikeoutCalculator.GetNumberingListBoxCharacterLineType(ANumberingListBox: TdxNumberingListBox): TdxStrikeoutType;
var
  ANumerationCharacterProperties: TdxMergedCharacterProperties;
begin
  ANumerationCharacterProperties := ANumberingListBox.GetNumerationCharacterProperties(PieceTable);
  try
    Result := ANumerationCharacterProperties.Info.FontStrikeoutType;
  finally
    ANumerationCharacterProperties.Free;
  end;
end;

function TdxStrikeoutCalculator.GetNumberingListBoxCharacterLineUseForWordsOnly(ANumberingListBox: TdxNumberingListBox): Boolean;
var
  ANumerationCharacterProperties: TdxMergedCharacterProperties;
begin
  ANumerationCharacterProperties := ANumberingListBox.GetNumerationCharacterProperties(PieceTable);
  try
    Result := ANumerationCharacterProperties.Info.StrikeoutWordsOnly;
  finally
    ANumerationCharacterProperties.Free;
  end;
end;

end.
