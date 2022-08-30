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

unit dxRichEdit.InnerControl.SpellCheckerController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Controls,
  dxCoreClasses, cxControls, dxCultureInfo, dxSpellCheckerCore,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.Platform.Font,
  dxRichEdit.View.Core;

type
  TdxSpellCheckerErrorBoxCalculator = class;

  { IdxSyntaxCheckService }

  IdxSyntaxCheckService = interface
  ['{20F147E8-3BD7-4172-892E-3042B451C5F3}']
    function Check(const AStart, AEnd: TdxDocumentModelPosition): TArray<TdxRunInfo>;
  end;

  { TdxSpellCheckerCustomController }

  TdxSpellCheckerCustomController = class abstract(TdxSpellCheckerControllerBase)
  strict private
    FControl: IdxInnerControl;
    FPieceTable: TdxPieceTable;
    FDocumentLayout: TdxDocumentLayout;
    function GetBoxMeasurer: TdxBoxMeasurer;
    function GetPages: TdxPageCollection;
    function GetDocumentModel: TdxDocumentModel;
  protected
    function GetPieceTable: TdxPieceTable; virtual;
    procedure SetPieceTable(const AValue: TdxPieceTable); virtual;
    function CreateErrorBoxCalculator: TdxSpellCheckerErrorBoxCalculator; virtual;
    procedure OnPieceTableChanged; virtual;
    procedure CheckPage(APage: TdxPage); virtual;
    procedure CheckPieceTable;
    procedure CheckFloatingObjects(APage: TdxPage); overload;
    procedure CheckFloatingObjects(const AFloatingObjects: TdxFloatingObjectBoxList); overload;
    procedure CheckTextBoxContent(AFloatingObject: TdxFloatingObjectBox);
    procedure CheckPageContent(APage: TdxPage);
    function CanCheckDocument: Boolean; virtual;
    procedure CheckHeaderFooter(AArea: TdxPageArea; APageIndex: Integer); virtual;
    procedure CheckContent(const AStart, AEnd: TdxDocumentModelPosition; APageIndex: Integer); virtual; abstract;

    property BoxMeasurer: TdxBoxMeasurer read GetBoxMeasurer;
    property DocumentLayout: TdxDocumentLayout read FDocumentLayout write FDocumentLayout;
    property Pages: TdxPageCollection read GetPages;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    constructor Create(const AControl: IdxInnerControl); overload; virtual;
    destructor Destroy; override;

    procedure SubscribeToEvents; virtual;
    procedure UnsubscribeToEvents; virtual;
    procedure ResetCore; override;
    procedure Reset; override;
    procedure CheckPages(AFrom: Integer); override;

    property Control: IdxInnerControl read FControl;
    property PieceTable: TdxPieceTable read GetPieceTable write SetPieceTable;
  end;

  { TdxDocumentPosition }

  TdxDocumentPosition = class(TdxSpellCheckerPosition<TdxDocumentModelPosition>)
  strict private
    class var
      FZero: IdxSpellCheckerPosition;
  strict private
    FPosition: TdxDocumentModelPosition;
    FIsValid: Boolean;

    class constructor Initialize;
    class destructor Finalize;
    function GetPosition: TdxDocumentModelPosition;
    function GetPieceTable: TdxPieceTable;
  protected
    function Compare(APosition: IdxSpellCheckerPosition): Integer; override;
    function GetActualPosition: TdxDocumentModelPosition; override;
    function GetActualPositionHashCode: Integer; override;
    function GetZero: IdxSpellCheckerPosition; override;
    procedure SetActualPosition(const AValue: TdxDocumentModelPosition); override;

    function GetIsValid: Boolean; virtual;
    function GetLogPosition: TdxDocumentLogPosition; virtual;
    procedure InvalidatePosition;

    function MoveForward: IdxSpellCheckerPosition; override;
    function MoveBackward: IdxSpellCheckerPosition; override;

    property IsValid: Boolean read GetIsValid;
    property LogPosition: TdxDocumentLogPosition read GetLogPosition;
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(APieceTable: TdxPieceTable); overload;
    constructor Create(const AActualPosition: TdxDocumentModelPosition); overload; override;

    function Clone: IdxSpellCheckerPosition; override;
    function Add(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;
    function Subtract(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;

    function ToInteger: Integer; override;
    function ToString: string; override;

    function UpdatePosition: Boolean; virtual;

    property Position: TdxDocumentModelPosition read GetPosition;
  end;

  { TdxPositionOffset }

  TdxPositionOffset = class(TdxSpellCheckerIntegerPosition)
  public
    property Offset: Integer read FValue;
  end;

  { TdxExpandedDocumentLayoutPosition }

  TdxExpandedDocumentLayoutPosition = class(TdxDocumentLayoutPosition)
  strict private
    FPageIndex: Integer;
    FPageAreaIndex: Integer;
    FColumnIndex: Integer;
    FRowIndex: Integer;
    FBoxIndex: Integer;
    FCharacterIndex: Integer;
  protected
    function CreateEmptyClone: TdxDocumentLayoutPosition; override;
    function GetPageIndex(APages: TdxPageCollection; AStartIndex: Integer; AEndIndex: Integer): Integer; override;
    function GetPageAreaIndex(AAreas: TdxPageAreaCollection; AStartIndex: Integer; AEndIndex: Integer): Integer; override;
    function GetColumnIndex(AColumns: TdxColumnCollection; AStartIndex: Integer; AEndIndex: Integer): Integer; override;
    function GetRowIndex(ARows: TdxRowCollection; AStartIndex: Integer; AEndIndex: Integer): Integer; override;
    function GetBoxIndex(ABoxes: TdxBoxCollection; AStartIndex: Integer; AEndIndex: Integer): Integer; override;
    function GetCharIndex(ACharacters: TdxCharacterBoxCollection; AStartIndex: Integer; AEndIndex: Integer): Integer; override;
    function Update(APages: TdxPageCollection; AIndex: Integer; ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; overload; virtual;
    function Update(APages: TdxPageCollection; AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; overload;
    function UpdateCore(APages: TdxPageCollection; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload; override;
    function UpdateCore(APages: TdxPageCollection; AIndex: Integer; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload; virtual;
    function UpdateCore(APages: TdxPageCollection; AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload; virtual;
    function UpdateBoxRecursive(AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload;
    function UpdateRowRecursive(AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload;
    function UpdateColumnRecursive(AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload;
    function UpdatePageAreaRecursive(AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel; overload;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition); override;

    procedure CopyFrom(AValue: TdxDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel); override;
    procedure Invalidate; override;

    property PageIndex: Integer read FPageIndex write FPageIndex;
    property PageAreaIndex: Integer read FPageAreaIndex write FPageAreaIndex;
    property ColumnIndex: Integer read FColumnIndex write FColumnIndex;
    property RowIndex: Integer read FRowIndex write FRowIndex;
    property BoxIndex: Integer read FBoxIndex write FBoxIndex;
    property CharacterIndex: Integer read FCharacterIndex write FCharacterIndex;
  end;

  { TdxBoxLayoutInfo }

  TdxBoxLayoutInfo = class
  strict private
    FStart: TdxExpandedDocumentLayoutPosition;
    FEnd: TdxExpandedDocumentLayoutPosition;
    function GetStartLogPosition: TdxDocumentLogPosition;
    function GetEndLogPosition: TdxDocumentLogPosition;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition);
    destructor Destroy; override;

    property Start: TdxExpandedDocumentLayoutPosition read FStart;
    property &End: TdxExpandedDocumentLayoutPosition read FEnd;
    property StartLogPosition: TdxDocumentLogPosition read GetStartLogPosition;
    property EndLogPosition: TdxDocumentLogPosition read GetEndLogPosition;
  end;

  { TdxDocumentLayoutIterator }

  TdxDocumentLayoutIterator = class
  strict private
    FDocumentLayout: TdxDocumentLayout;
    FBoxInfo: TdxBoxLayoutInfo;
    FTableCell: TdxTableCellViewInfo;
    FCurrentBoxIndex: Integer;
    FCurrentRowIndex: Integer;
    FCurrentColumnIndex: Integer;
    FCurrentPageAreaIndex: Integer;
    FCurrentPageIndex: Integer;
    FHeader: TdxHeaderPageArea;
    FFooter: TdxFooterPageArea;
    FStopInHeaderFooter: Boolean;
    function GetStart: TdxDocumentLayoutPosition;
    function GetEnd: TdxDocumentLayoutPosition;
    function GetCurrentPage: TdxPage;
    function GetCurrentPageArea: TdxPageArea;
    function GetCurrentColumn: TdxColumn;
    function GetRows: TdxRowCollection;
    function GetCurrentRow: TdxRow;
    function GetCurrentBox: TdxBox;
  protected
    procedure Initialize; virtual;
    procedure SetHeaderFooter; virtual;
    function MoveBoxToNext: Boolean; virtual;
    function MoveRowToNext: Boolean; virtual;
    function MoveTableCellRowToNext: Boolean;
    function IsRowLastInCurrentColumn(ARow: TdxRow): Boolean;
    function MoveColumnToNext: Boolean; virtual;
    function MovePageAreaToNext: Boolean; virtual;
    function MovePageToNext: Boolean; virtual;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; ABoxInfo: TdxBoxLayoutInfo; AStopInHeaderFooter: Boolean = False); overload;
    function MoveNext(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; virtual;

    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
    property BoxInfo: TdxBoxLayoutInfo read FBoxInfo;
    property Start: TdxDocumentLayoutPosition read GetStart;
    property &End: TdxDocumentLayoutPosition read GetEnd;
    property CurrentPage: TdxPage read GetCurrentPage;
    property CurrentPageArea: TdxPageArea read GetCurrentPageArea;
    property CurrentColumn: TdxColumn read GetCurrentColumn;
    property Header: TdxHeaderPageArea read FHeader;
    property Footer: TdxFooterPageArea read FFooter;
    property TableCell: TdxTableCellViewInfo read FTableCell;
    property Rows: TdxRowCollection read GetRows;
    property CurrentRow: TdxRow read GetCurrentRow;
    property CurrentBox: TdxBox read GetCurrentBox;
    property StopInHeaderFooter: Boolean read FStopInHeaderFooter;
  end;

  { TdxSpellCheckerErrorBoxCalculator }

  TdxSpellCheckerErrorBoxCalculator = class
  strict private
    const
      UnderlineThicknessDefault = 8;
  strict private
    FDocumentLayout: TdxDocumentLayout;
    FBoxMeasurer: TdxBoxMeasurer;
    FPieceTable: TdxPieceTable;
    FLastProcessedRow: TdxRow;
    FLastCalcLayoutPos: TdxExpandedDocumentLayoutPosition;
  protected
    function Update(ABoxInfo: TdxBoxLayoutInfo; AIndex: Integer; ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; overload;
    function Update(ABoxInfo: TdxBoxLayoutInfo; AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean; overload;
    procedure CalculateCore(ABoxInfo: TdxBoxLayoutInfo; AStart: TdxFormatterPosition; AEnd: TdxFormatterPosition; AErrorType: TdxSpellingError); virtual;
    procedure CalculateErrorBoxInSomeRows(AIterator: TdxDocumentLayoutIterator; AStart: TdxFormatterPosition; AEnd: TdxFormatterPosition; AErrorType: TdxSpellingError); virtual;
    procedure CalculateErrorBoxInCurrentRow(AIterator: TdxDocumentLayoutIterator; AStart: TdxFormatterPosition; AEnd: TdxFormatterPosition; AErrorType: TdxSpellingError); virtual;
    function FirstBox(ABox1: TdxBox; ABox2: TdxBox): TdxBox; virtual;
    function LastBox(ABox1: TdxBox; ABox2: TdxBox): TdxBox; virtual;
    function MaxPosition(APosition1: TdxFormatterPosition; APosition2: TdxFormatterPosition): TdxFormatterPosition; virtual;
    function MinPosition(APosition1: TdxFormatterPosition; APosition2: TdxFormatterPosition): TdxFormatterPosition; virtual;
    procedure CalculateErrorBox(ARow: TdxRow; AErrorBox: TdxErrorBox; AFirst: TdxBox; ALast: TdxBox); virtual;
    procedure CalculateErrorBoxBounds(ARow: TdxRow; AErrorBox: TdxErrorBox; AFirst: TdxBox; ALast: TdxBox); virtual;
    function GetFontInfo(ABox: TdxBox): TdxFontInfo; virtual;
    function GetUnderlineLeft(AFirst: TdxBox; AErrorBox: TdxErrorBox): Integer; virtual;
    function GetUnderlineRight(ALast: TdxBox; AErrorBox: TdxErrorBox): Integer; virtual;
    function GetFirstBoxOffset(ABox: TdxBox; AOffset: Integer): Integer; virtual;
    function GetLastBoxOffset(ABox: TdxBox; AOffset: Integer): Integer; virtual;
    function GetCharactersBounds(ABox: TdxBox; const ABoxText: string): TArray<TRect>;
    function GetBoxText(ABox: TdxBox): string;

    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
    property BoxMeasurer: TdxBoxMeasurer read FBoxMeasurer;
    property PieceTable: TdxPieceTable read FPieceTable;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; ABoxMeasurer: TdxBoxMeasurer; APieceTable: TdxPieceTable);
    destructor Destroy; override;
    procedure Calculate(APageIndex: Integer; const AStartPos, AEndPos: TdxDocumentModelPosition; AErrorType: TdxSpellingError); virtual;
    procedure ClearErrorBoxes(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition); virtual;
  end;

  { TdxSyntaxCheckController }

  TdxSyntaxCheckController = class(TdxSpellCheckerCustomController)
  strict private
    FSyntaxChecker: IdxSyntaxCheckService;
  protected
    procedure CheckContent(const AStart, AEnd: TdxDocumentModelPosition; APageIndex: Integer); override;
  public
    constructor Create(const AControl: IdxInnerControl; const ASyntaxChecker: IdxSyntaxCheckService);
  end;

  { TdxIgnoreListManager }

  TdxIgnoreListManager = class(TcxIUnknownObject, IdxSpellCheckerIgnoreList)
  strict private
    FDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
    function GetIgnoredList: TdxIgnoredList;
    function GetMisspelledIntervals: TdxMisspelledIntervalCollection;
  protected
    procedure AddCore(const AStartPosition, AEndPosition: TdxDocumentPosition); virtual;
    procedure RemoveCore(const AStartPosition, AEndPosition: TdxDocumentPosition); virtual;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    procedure Add(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string); overload; virtual;
    procedure Add(const AWord: string); overload; virtual;
    function Contains(const AWord: string): Boolean; overload; virtual;
    function Contains(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string): Boolean; overload; virtual;
    procedure Remove(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string); overload; virtual;
    procedure Remove(const AWord: string); overload; virtual;
    procedure Clear;

    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property IgnoredList: TdxIgnoredList read GetIgnoredList;
    property MisspelledIntervals: TdxMisspelledIntervalCollection read GetMisspelledIntervals;
  end;

  { TdxVisibleCharactersIterator }

  TdxVisibleCharactersIterator = class(TdxPieceTableIterator)
  strict private
    FCachedRunIndex: TdxRunIndex;
    FIsCachedRunVisible: Boolean;
    function GetVisibleTextFilter: IdxVisibleTextFilter;
  protected
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition); override;
    procedure MoveBackCore(var APos: TdxDocumentModelPosition); override;
    function IsRunVisible(ARunIndex: TdxRunIndex): Boolean; virtual;

    property VisibleTextFilter: IdxVisibleTextFilter read GetVisibleTextFilter;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;
  end;

  { TdxSentencePositionCalculator }

  TdxSentencePositionCalculator = class
  strict private
    class var
      FSimpleSentenceSeparators: TdxCharList;
      FSpecialSentenceSeparators: TdxCharList;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FWordIterator: TdxSpellCheckerWordIterator;
  public
    constructor Create(AWordIterator: TdxSpellCheckerWordIterator);

    function GetSentenceEndPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function GetSentenceEndPositionCore(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function GetSentenceStartPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function GetSentenceStartPositionCore(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function FindSentenceStart(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function ProcessPunctuation(const AStart, AFinish: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function FindSentenceSeparatorAfterThreeDotsWithSpace(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function FindPunctuationAfterThreeDotsWithSpace(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function FindAbbreviation(const AStart, AFinish: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function PrevCharIsLetter(const APos: TdxDocumentModelPosition): Boolean;
    function NextCharIsDot(const APos: TdxDocumentModelPosition): Boolean;
    function FindLetterOrDigitPosition(const AStartPosition: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function FindPrevLetterOrDigitPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function FindSentenceSeparatorWithSpace(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function FindThreeDotsWithSpace(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function SkipThreeDots(const APos: TdxDocumentModelPosition): Boolean;
    function GetCharacter(const APos: TdxDocumentModelPosition): Char;
    function IsNotWordSymbolAndNotSentenceSeparator(const ACh: Char): Boolean;
    function IsDot(const ACh: Char): Boolean;
    function IsSpace(const ACh: Char): Boolean;
    function IsSpecialSentenceSeparator(const ACh: Char): Boolean;
    function IsSimpleSentenceSeparator(const ACh: Char): Boolean;
    function IsSentenceSeparator(const ACh: Char): Boolean;
    function IsStartOfDocument(const APos: TdxDocumentModelPosition; AWordIterator: TdxSpellCheckerWordIterator): Boolean;
    function IsEndOfDocument(const APos: TdxDocumentModelPosition; AWordIterator: TdxSpellCheckerWordIterator): Boolean;
  end;

  { TdxRichEditTextController }

  TdxRichEditTextController = class abstract(TcxIUnknownObject, IdxSpellCheckTextController{, IdxSupportMultiCulture})
  strict private
    FControl: IdxInnerControl;
    FGuid: string;
    FSpellingFinish: IdxSpellCheckerPosition;
    FSpellingStart: IdxSpellCheckerPosition;

    function GetDocumentModel: TdxDocumentModel;
    function GetText: string;
    procedure SetText(const AValue: string);
  protected
    class function GetCharacter(const APos: TdxDocumentModelPosition): Char; static;

    function CanCheckDocument: Boolean; virtual;
    function CanEditRange(APieceTable: TdxPieceTable; AStart, AFinish: TdxDocumentLogPosition): Boolean;
    function CreatePosition(ALogPosition: Integer): IdxSpellCheckerPosition;
    function CreateDocumentPosition(const APos: TdxDocumentModelPosition): TdxDocumentPosition;
    function DeleteWordCore(APieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition; AFinish: TdxDocumentLogPosition): Boolean;
    function GetModelPosition(APosition: TdxDocumentPosition): TdxDocumentModelPosition;
    function GetPieceTable: TdxPieceTable; virtual;
    function GetRunText(APieceTable: TdxPieceTable; AIndex: TdxRunIndex): string; overload;
    function GetRunText(APieceTable: TdxPieceTable; AIndex: TdxRunIndex; AFrom: Integer; ATo: Integer): string; overload;
    function GetSentencePositionCalculator(APieceTable: TdxCustomPieceTable): TdxSentencePositionCalculator;
    function GetVisibleText(APieceTable: TdxPieceTable; const AStart, AEnd: TdxDocumentModelPosition): string;
    function GetWordIterator(APieceTable: TdxCustomPieceTable): TdxSpellCheckerWordIterator; virtual;
    function ReplaceWordCore(APieceTable: TdxPieceTable; const AWord: string; AStart: TdxDocumentLogPosition; AFinish: TdxDocumentLogPosition): Boolean;
    function TryShiftEndPosition(const APos: TdxDocumentModelPosition; AIterator: TdxVisibleCharactersIterator): Boolean;
    function TryShiftStartPosition(const APos: TdxDocumentModelPosition; AIterator: TdxVisibleCharactersIterator): Boolean;

    function IsPositionVisible(const APos: IdxSpellCheckerPosition): Boolean;
    function IsRangeEditable(const AStart, AFinish: IdxSpellCheckerPosition): Boolean;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(const AControl: IdxInnerControl); virtual;

    function CanDoNextStep(const APosition: IdxSpellCheckerPosition): Boolean; overload;
    function DeleteWord(const AStart, AFinish: IdxSpellCheckerPosition): Boolean; overload;
    function GetNextPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; overload;
    function GetNextWord(var AStart, AFinish: IdxSpellCheckerPosition): Boolean;
    function GetPreviousWord(const APos: IdxSpellCheckerPosition): string; overload;
    function GetPrevPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; overload;
    function GetSentenceFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; virtual;
    function GetSentenceStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; virtual;
    function GetTextLength(const AText: string): IdxSpellCheckerPosition;
    function GetWord(const AStart, AFinish: IdxSpellCheckerPosition): string; overload;
    function GetWordFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
    function GetWordStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; overload;
    function HasLetters(const AStart, AFinish: IdxSpellCheckerPosition): Boolean; overload;
    function IsFinishPositionRequest(const AText: string): Boolean;
    procedure PreparePositionsForDelete(const AStart, AFinish: TdxDocumentModelPosition);
    function ReplaceWord(const AStart, AFinish: IdxSpellCheckerPosition; const AWord: string): Boolean; overload;
    procedure SetSpellingArea(const AStart, AFinish: IdxSpellCheckerPosition);
    function ShouldCheckWord(const AStart, AEnd: IdxSpellCheckerPosition): Boolean; overload;

    property Control: IdxInnerControl read FControl;
    property Text: string read GetText write SetText;
  end;

  { TdxCheckAsYouTypeTextController }

  TdxCheckAsYouTypeTextController = class(TdxRichEditTextController)
  strict private
    FWordIterator: TdxSpellCheckerWordIterator;
    FPieceTable: TdxPieceTable;
  protected
    function CanCheckDocument: Boolean; override;
    function GetPieceTable: TdxPieceTable; override;
    procedure SetPieceTable(APieceTable: TdxPieceTable);
    function GetWordIterator(APieceTable: TdxCustomPieceTable): TdxSpellCheckerWordIterator; override;
  public
    constructor Create(const AControl: IdxInnerControl); override;
    destructor Destroy; override;
    procedure Reset;
    procedure ResetSpellCheck(AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex);
  end;

  { TdxSpellCheckerController }

  TdxSpellCheckerController = class(TdxSpellCheckerCustomController)
  strict protected
    const
      LowerLimit = 1000;
      UpperLimit = 2000;
  strict private
    FIgnoreListManager: TdxIgnoreListManager;
    FSpellChecker: IdxSpellChecker3;
    FTextController: TdxCheckAsYouTypeTextController;

    function GetAsControl: TWinControl;
  protected
    procedure SubscribeSpellCheckerEvents; virtual;
    procedure UnsubscribeSpellCheckerEvents; virtual;
    procedure OnSpellCheckModeChanged(ASender: TObject); virtual;
    procedure OnSpellCheckerCultureChanged(ASender: TObject); virtual;
    procedure OnOptionsChanged(ASender: TObject); virtual;
    procedure OnCustomDictionaryChanged(ASender: TObject); virtual;
    procedure ResetSpellCheck(AStartRunIndex: TdxRunIndex); overload;
    procedure ResetSpellCheck(AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex); overload;
    procedure CheckContent(const AStart, AEnd: TdxDocumentModelPosition; APageIndex: Integer); override;
    function CheckContentCore(const AStart, AEnd: TdxDocumentModelPosition): Boolean;
    procedure ClearErrorBoxes(AFrom, ATo: TdxDocumentLogPosition); virtual;
    procedure CalculateErrorBoxes(const AStart, AEnd: TdxDocumentModelPosition; APageIndex: Integer); virtual;
    function CheckInterval(AInterval: TdxSpellCheckerInterval): Boolean; virtual;
    function IsUpperLimitExceeded: Boolean; virtual;
    procedure CalculateMisspelledInterval(const AStart, AEnd: TdxDocumentModelPosition;
      const AWord: string; AError: TdxSpellingError); virtual;
    function CanCheckDocument: Boolean; override;
    procedure OnPieceTableChanged; override;

    property AsControl: TWinControl read GetAsControl;
    property IgnoreListManager: TdxIgnoreListManager read FIgnoreListManager;
    property TextController: TdxCheckAsYouTypeTextController read FTextController;
  public
    constructor Create(const AControl: IdxInnerControl); override;
    destructor Destroy; override;

    procedure SubscribeToEvents; override;
    procedure UnsubscribeToEvents; override;

    procedure ResetCore; override;
    procedure Reset; override;

    property SpellChecker: IdxSpellChecker3 read FSpellChecker;
  end;

  { TdxEmptySpellCheckerController }

  TdxEmptySpellCheckerController = class(TdxSpellCheckerCustomController)
  protected
    procedure SetPieceTable(const AValue: TdxPieceTable); override;
    procedure CheckContent(const AStart, AEnd: TdxDocumentModelPosition; APageIndex: Integer); override;
  public
    constructor Create; reintroduce;
    procedure Reset; override;
    procedure CheckPages(AFrom: Integer); override;
  end;

implementation

uses
  Contnrs, Math, Character,
  dxTypeHelpers, dxCore,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.TextRange,
  dxCharacters,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.InnerControl;

type

  { TdxEmptyDocumentPosition }

  TdxEmptyDocumentPosition = class(TdxDocumentPosition)
  protected
    function GetLogPosition: TdxDocumentLogPosition; override;
    function GetActualPosition: TdxDocumentModelPosition; override;
    procedure SetActualPosition(const AValue: TdxDocumentModelPosition); override;
    function GetIsValid: Boolean; override;
    function MoveBackward: IdxSpellCheckerPosition; override;
    function MoveForward: IdxSpellCheckerPosition; override;
  public
    constructor Create; reintroduce;

    function Clone: IdxSpellCheckerPosition; override;
    function Add(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;
    function Subtract(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;
    function UpdatePosition: Boolean; override;
  end;

{ TdxEmptyDocumentPosition }

constructor TdxEmptyDocumentPosition.Create;
begin
  inherited Create;
end;

function TdxEmptyDocumentPosition.GetLogPosition: TdxDocumentLogPosition;
begin
  Result := 0;
end;

function TdxEmptyDocumentPosition.GetActualPosition: TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.Null;
end;

procedure TdxEmptyDocumentPosition.SetActualPosition(const AValue: TdxDocumentModelPosition);
begin
end;

function TdxEmptyDocumentPosition.GetIsValid: Boolean;
begin
  Result := True;
end;

function TdxEmptyDocumentPosition.Add(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxEmptyDocumentPosition.Subtract(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxEmptyDocumentPosition.MoveBackward: IdxSpellCheckerPosition;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxEmptyDocumentPosition.MoveForward: IdxSpellCheckerPosition;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxEmptyDocumentPosition.UpdatePosition: Boolean;
begin
  Result := True;
end;

function TdxEmptyDocumentPosition.Clone: IdxSpellCheckerPosition;
begin
  Result := TdxEmptyDocumentPosition.Create;
end;

{ TdxDocumentPosition }

class constructor TdxDocumentPosition.Initialize;
begin
  FZero := TdxEmptyDocumentPosition.Create;
end;

class destructor TdxDocumentPosition.Finalize;
begin
  FZero := nil;
end;

constructor TdxDocumentPosition.Create(const AActualPosition: TdxDocumentModelPosition);
begin
  inherited Create(AActualPosition);
  FPosition := AActualPosition;
  FIsValid := True;
end;

constructor TdxDocumentPosition.Create(APieceTable: TdxPieceTable);
begin
  Create(TdxDocumentModelPosition.Create(APieceTable));
end;

function TdxDocumentPosition.GetIsValid: Boolean;
begin
  Result := FIsValid;
end;

function TdxDocumentPosition.GetPosition: TdxDocumentModelPosition;
begin
  if not IsValid then
    UpdatePosition;
  Result := FPosition;
end;

function TdxDocumentPosition.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(Position.PieceTable);
end;

function TdxDocumentPosition.GetLogPosition: TdxDocumentLogPosition;
begin
  Result := Position.LogPosition;
end;

function TdxDocumentPosition.GetActualPosition: TdxDocumentModelPosition;
begin
  Result := Position;
end;

function TdxDocumentPosition.GetActualPositionHashCode: Integer;
begin
  Result := ActualPosition.GetHashCode;
end;

procedure TdxDocumentPosition.SetActualPosition(const AValue: TdxDocumentModelPosition);
begin
  FPosition := AValue;
end;

function TdxDocumentPosition.GetZero: IdxSpellCheckerPosition;
begin
  Result := FZero;
end;

function TdxDocumentPosition.ToInteger: Integer;
begin
  Result := LogPosition;
end;

procedure TdxDocumentPosition.InvalidatePosition;
begin
  FIsValid := False;
end;

function TdxDocumentPosition.Add(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
var
  APositionOffset: TdxPositionOffset;
  ALogPosition: TdxDocumentLogPosition;
begin
  if APosition = nil then
    Exit(Clone);
  APositionOffset := TdxPositionOffset(APosition);
  if APositionOffset = nil then
    TdxRichEditExceptions.ThrowArgumentException('position', TObject(APosition));
  ALogPosition := Position.LogPosition + APositionOffset.Offset;
  if ALogPosition > PieceTable.DocumentEndLogPosition then
    TdxRichEditExceptions.ThrowArgumentException('position', APositionOffset);
  Result := TdxDocumentPosition.Create(TdxPositionConverter.ToDocumentModelPosition(PieceTable, ALogPosition));
end;

function TdxDocumentPosition.Subtract(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
var
  ADocumentPosition: TdxDocumentPosition;
  APositionOffset: TdxPositionOffset;
  ALogPosition: TdxDocumentLogPosition;
begin
  if APosition = nil then
    Exit(Clone);
  ADocumentPosition := TdxDocumentPosition(APosition);
  if ADocumentPosition <> nil then
  begin
    Result := TdxPositionOffset.Create(Position.LogPosition - ADocumentPosition.Position.LogPosition);
    Exit;
  end;
  APositionOffset := TdxPositionOffset(APosition);
  if APositionOffset <> nil then
  begin
    ALogPosition := Position.LogPosition - APositionOffset.Offset;
    if ALogPosition < PieceTable.DocumentStartLogPosition then
      TdxRichEditExceptions.ThrowArgumentException('position', APositionOffset);
    Result := TdxDocumentPosition.Create(TdxPositionConverter.ToDocumentModelPosition(PieceTable, ALogPosition));
    Exit;
  end;
  TdxRichEditExceptions.ThrowArgumentException('position', TObject(APosition));
  Result := nil;
end;

function TdxDocumentPosition.Compare(APosition: IdxSpellCheckerPosition): Integer;
var
  ADocumentPosition: TdxDocumentPosition;
begin
  if APosition = nil then
    Exit(1);
  ADocumentPosition := TdxDocumentPosition(APosition);
  if ADocumentPosition = nil then
    TdxRichEditExceptions.ThrowArgumentException('position', TObject(APosition));
  Result := LogPosition - ADocumentPosition.LogPosition;
end;

function TdxDocumentPosition.MoveForward: IdxSpellCheckerPosition;
begin
  Result := TdxDocumentPosition.Create(TdxDocumentModelPosition.MoveForward(Position));
end;

function TdxDocumentPosition.MoveBackward: IdxSpellCheckerPosition;
begin
  Result := TdxDocumentPosition.Create(TdxDocumentModelPosition.MoveBackward(Position));
end;

function TdxDocumentPosition.ToString: string;
begin
  Result := IntToStr(Position.LogPosition);
end;

function TdxDocumentPosition.Clone: IdxSpellCheckerPosition;
begin
  Result := TdxDocumentPosition.Create(Position);
end;

function TdxDocumentPosition.UpdatePosition: Boolean;
var
  ALogPosition: TdxDocumentLogPosition;
  APieceTable: TdxPieceTable;
begin
  ALogPosition := FPosition.LogPosition;
  APieceTable := TdxPieceTable(FPosition.PieceTable);
  if (ALogPosition < APieceTable.DocumentStartLogPosition) or
      (ALogPosition > APieceTable.DocumentEndLogPosition) then
    FIsValid := False
  else
  begin
    FPosition.Update;
    FIsValid := True;
  end;
  Result := FIsValid;
end;

{ TdxExpandedDocumentLayoutPosition }

constructor TdxExpandedDocumentLayoutPosition.Create(ADocumentLayout: TdxDocumentLayout;
  APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition);
begin
  inherited Create(ADocumentLayout, APieceTable, ALogPosition);
  FPageIndex := -1;
  FPageAreaIndex := -1;
  FColumnIndex := -1;
  FRowIndex := -1;
  FBoxIndex := -1;
  FCharacterIndex := -1;
end;

function TdxExpandedDocumentLayoutPosition.CreateEmptyClone: TdxDocumentLayoutPosition;
begin
  Result := TdxExpandedDocumentLayoutPosition.Create(DocumentLayout, PieceTable, LogPosition);
end;

function TdxExpandedDocumentLayoutPosition.GetPageIndex(APages: TdxPageCollection; AStartIndex: Integer; AEndIndex: Integer): Integer;
begin
  FPageIndex := inherited GetPageIndex(APages, AStartIndex, AEndIndex);
  Result := FPageIndex;
end;

function TdxExpandedDocumentLayoutPosition.GetPageAreaIndex(AAreas: TdxPageAreaCollection; AStartIndex: Integer; AEndIndex: Integer): Integer;
begin
  FPageAreaIndex := inherited GetPageAreaIndex(AAreas, AStartIndex, AEndIndex);
  Result := FPageAreaIndex;
end;

function TdxExpandedDocumentLayoutPosition.GetColumnIndex(AColumns: TdxColumnCollection; AStartIndex: Integer; AEndIndex: Integer): Integer;
begin
  FColumnIndex := inherited GetColumnIndex(AColumns, AStartIndex, AEndIndex);
  Result := FColumnIndex;
end;

function TdxExpandedDocumentLayoutPosition.GetRowIndex(ARows: TdxRowCollection; AStartIndex: Integer; AEndIndex: Integer): Integer;
begin
  FRowIndex := inherited GetRowIndex(ARows, AStartIndex, AEndIndex);
  Result := FRowIndex;
end;

function TdxExpandedDocumentLayoutPosition.GetBoxIndex(ABoxes: TdxBoxCollection; AStartIndex: Integer; AEndIndex: Integer): Integer;
begin
  FBoxIndex := inherited GetBoxIndex(ABoxes, AStartIndex, AEndIndex);
  Result := FBoxIndex;
end;

function TdxExpandedDocumentLayoutPosition.GetCharIndex(ACharacters: TdxCharacterBoxCollection; AStartIndex: Integer; AEndIndex: Integer): Integer;
begin
  FCharacterIndex := inherited GetCharIndex(ACharacters, AStartIndex, AEndIndex);
  Result := FCharacterIndex;
end;

function TdxExpandedDocumentLayoutPosition.Update(APages: TdxPageCollection; AIndex: Integer; ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  if not IsValid(ADetailsLevel) then
  begin
    IncreaseDetailsLevel(UpdateCore(APages, AIndex, ADetailsLevel));
    Exit(IsValid(ADetailsLevel));
  end
  else
    Exit(True);
end;

function TdxExpandedDocumentLayoutPosition.UpdateCore(APages: TdxPageCollection;
  AIndex: Integer; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  APage: TdxPage;
  AResulLevel: TdxDocumentLayoutDetailsLevel;
begin
  if ADetailsLevel < TdxDocumentLayoutDetailsLevel.Page then
    Exit(TdxDocumentLayoutDetailsLevel.None);
  APage := APages[AIndex];
  AResulLevel := UpdatePageAreaRecursive(APage, 0, ADetailsLevel);
  if AResulLevel >= ADetailsLevel then
  begin
    Page := APage;
    FPageIndex := AIndex;
    Exit(AResulLevel);
  end;
  Result := UpdatePageRecursive(APages, AIndex, ADetailsLevel);
end;

function TdxExpandedDocumentLayoutPosition.Update(APages: TdxPageCollection; AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  if not IsValid(ADetailsLevel) then
  begin
    IncreaseDetailsLevel(UpdateCore(APages, AFrom, ADetailsLevel));
    Exit(IsValid(ADetailsLevel));
  end
  else
    Exit(True);
end;

function TdxExpandedDocumentLayoutPosition.UpdateCore(APages: TdxPageCollection; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
begin
  Result := inherited UpdateCore(APages, ADetailsLevel);
end;

function TdxExpandedDocumentLayoutPosition.UpdateCore(APages: TdxPageCollection; AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  AResulLevel: TdxDocumentLayoutDetailsLevel;
begin
  if (AFrom = nil) or (not AFrom.IsValid(ADetailsLevel)) then
    Exit(TdxDocumentLayoutDetailsLevel.None);

  AResulLevel := UpdateBoxRecursive(AFrom, ADetailsLevel);
  if AResulLevel >= ADetailsLevel then
    Exit(AResulLevel);
  Result := UpdatePageRecursive(APages, AFrom.PageIndex, ADetailsLevel);
end;

function TdxExpandedDocumentLayoutPosition.UpdateBoxRecursive(AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  AResultLevel: TdxDocumentLayoutDetailsLevel;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.Box) and (ADetailsLevel >= TdxDocumentLayoutDetailsLevel.Box) then
  begin
    AResultLevel := UpdateBoxRecursive(AFrom.Row, AFrom.BoxIndex, ADetailsLevel);
    if AResultLevel = ADetailsLevel then
    begin
      CopyFrom(AFrom, TdxDocumentLayoutDetailsLevel.Row);
      Exit(AResultLevel);
    end;
  end;
  Result := UpdateRowRecursive(AFrom, ADetailsLevel);
end;

function TdxExpandedDocumentLayoutPosition.UpdateRowRecursive(AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  ARows: TdxRowCollection;
  AResultLevel: TdxDocumentLayoutDetailsLevel;
  ATableRow: TdxTableCellRow;
  ARowIndex: Integer;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.Column) and (ADetailsLevel >= TdxDocumentLayoutDetailsLevel.Row) then
  begin
    if AFrom.TableCell <> nil then
      ARows := AFrom.TableCell.GetRows(AFrom.Column)
    else
      ARows := AFrom.Column.Rows;
    AResultLevel := UpdateRowRecursive(ARows, AFrom.RowIndex, ADetailsLevel);
    if AResultLevel = ADetailsLevel then
    begin
      if AFrom.TableCell = nil then
      begin
        if Row is TdxTableCellRow then
        begin
          ATableRow := Safe<TdxTableCellRow>.Cast(Row);
          TableCell := ATableRow.CellViewInfo;
          ARowIndex := TableCell.GetRows(AFrom.Column).IndexOf(ATableRow);
          if ARowIndex < 0 then
            Exit(TdxDocumentLayoutDetailsLevel.None);
          FRowIndex := ARowIndex;
        end;
        CopyFrom(AFrom, TdxDocumentLayoutDetailsLevel.Column);
      end
      else
        CopyFrom(AFrom, TdxDocumentLayoutDetailsLevel.TableCell);
      Exit(AResultLevel);
    end;
  end;
  Result := UpdateColumnRecursive(AFrom, ADetailsLevel);
end;

function TdxExpandedDocumentLayoutPosition.UpdateColumnRecursive(AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  AResultLevel: TdxDocumentLayoutDetailsLevel;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.PageArea) and (ADetailsLevel >= TdxDocumentLayoutDetailsLevel.Column) then
  begin
    AResultLevel := UpdateColumnRecursive(AFrom.PageArea.Columns, AFrom.ColumnIndex, ADetailsLevel);
    if AResultLevel = ADetailsLevel then
    begin
      CopyFrom(AFrom, TdxDocumentLayoutDetailsLevel.PageArea);
      Exit(AResultLevel);
    end;
  end;
  Result := UpdatePageAreaRecursive(AFrom, ADetailsLevel);
end;

function TdxExpandedDocumentLayoutPosition.UpdatePageAreaRecursive(AFrom: TdxExpandedDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLayoutDetailsLevel;
var
  AResultLevel: TdxDocumentLayoutDetailsLevel;
begin
  if not IsValid(TdxDocumentLayoutDetailsLevel.Page) and (ADetailsLevel >= TdxDocumentLayoutDetailsLevel.PageArea) then
  begin
    AResultLevel := UpdatePageAreaRecursive(AFrom.Page, AFrom.PageAreaIndex, ADetailsLevel);
    if AResultLevel = ADetailsLevel then
    begin
      CopyFrom(AFrom, TdxDocumentLayoutDetailsLevel.Page);
      Exit(AResultLevel);
    end;
  end;
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxExpandedDocumentLayoutPosition.Invalidate;
begin
  inherited Invalidate;
  FPageIndex := -1;
  FPageAreaIndex := -1;
  FColumnIndex := -1;
  FRowIndex := -1;
  FBoxIndex := -1;
  FCharacterIndex := -1;
end;

procedure TdxExpandedDocumentLayoutPosition.CopyFrom(AValue: TdxDocumentLayoutPosition; ADetailsLevel: TdxDocumentLayoutDetailsLevel);
var
  AExpandedPosition: TdxExpandedDocumentLayoutPosition;
begin
  inherited CopyFrom(AValue, ADetailsLevel);
  AExpandedPosition := Safe<TdxExpandedDocumentLayoutPosition>.Cast(AValue);
  if AExpandedPosition = nil then
    Exit;

  if IsValid(TdxDocumentLayoutDetailsLevel.Page) then
    FPageIndex := AExpandedPosition.pageIndex;
  if IsValid(TdxDocumentLayoutDetailsLevel.PageArea) then
    FPageAreaIndex := AExpandedPosition.pageAreaIndex;
  if IsValid(TdxDocumentLayoutDetailsLevel.Column) then
    FColumnIndex := AExpandedPosition.columnIndex;
  if IsValid(TdxDocumentLayoutDetailsLevel.Row) then
    FRowIndex := AExpandedPosition.rowIndex;
  if IsValid(TdxDocumentLayoutDetailsLevel.Box) then
    FBoxIndex := AExpandedPosition.boxIndex;
  if IsValid(TdxDocumentLayoutDetailsLevel.Character) then
    FCharacterIndex := AExpandedPosition.characterIndex;
end;

{ TdxSpellCheckerCustomController }

constructor TdxSpellCheckerCustomController.Create(const AControl: IdxInnerControl);
begin
  Assert(AControl <> nil);
  inherited Create;
  FControl := AControl;
  FPieceTable := AControl.DocumentModel.MainPieceTable;
  FDocumentLayout := AControl.ActiveView.DocumentLayout;
end;

destructor TdxSpellCheckerCustomController.Destroy;
begin
  FControl := nil;
  inherited Destroy;
end;

function TdxSpellCheckerCustomController.GetPieceTable: TdxPieceTable;
begin
  Result := FPieceTable;
end;

procedure TdxSpellCheckerCustomController.SetPieceTable(const AValue: TdxPieceTable);
begin
  Assert(AValue <> nil);
  if FPieceTable = AValue then
    Exit;
  FPieceTable := AValue;
  OnPieceTableChanged;
end;

function TdxSpellCheckerCustomController.GetBoxMeasurer: TdxBoxMeasurer;
begin
  Result := Control.Measurer;
end;

function TdxSpellCheckerCustomController.GetPages: TdxPageCollection;
begin
  Result := DocumentLayout.Pages;
end;

function TdxSpellCheckerCustomController.GetDocumentModel: TdxDocumentModel;
begin
  Result := Control.DocumentModel;
end;

procedure TdxSpellCheckerCustomController.SubscribeToEvents;
begin
end;

procedure TdxSpellCheckerCustomController.UnsubscribeToEvents;
begin
end;

function TdxSpellCheckerCustomController.CreateErrorBoxCalculator: TdxSpellCheckerErrorBoxCalculator;
begin
  Result := TdxSpellCheckerErrorBoxCalculator.Create(DocumentLayout, BoxMeasurer, PieceTable);
end;

procedure TdxSpellCheckerCustomController.OnPieceTableChanged;
begin
end;

procedure TdxSpellCheckerCustomController.ResetCore;
begin
end;

procedure TdxSpellCheckerCustomController.Reset;
begin
end;

procedure TdxSpellCheckerCustomController.CheckPages(AFrom: Integer);
var
  AStartIndex, AEndIndex, I: Integer;
  APage: TdxPage;
begin
  if not CanCheckDocument then
    Exit;

  AStartIndex := DocumentLayout.FirstVisiblePageIndex;
  AEndIndex := DocumentLayout.LastVisiblePageIndex;
  if (AStartIndex > AEndIndex) or (AEndIndex < 0) then
    Exit;

  for I := AStartIndex to AEndIndex do
  begin
    APage := Pages[I];
    if APage.CheckSpellingComplete then
      Continue;

    CheckPage(APage);
    APage.CheckSpellingComplete := True;
  end;
end;

procedure TdxSpellCheckerCustomController.CheckPage(APage: TdxPage);
var
  APageIndex: Integer;
  AHeader: TdxHeaderPageArea;
  AFooter: TdxFooterPageArea;
begin
  APageIndex := APage.PageIndex;
  AHeader := APage.Header;
  if AHeader <> nil then
  begin
    PieceTable := TdxPieceTable(AHeader.Header.PieceTable);
    CheckHeaderFooter(AHeader, APageIndex);
  end;
  AFooter := APage.Footer;
  if AFooter <> nil then
  begin
    PieceTable := TdxPieceTable(AFooter.Footer.PieceTable);
    CheckHeaderFooter(AFooter, APageIndex);
  end;
  CheckFloatingObjects(APage);
  PieceTable := DocumentModel.MainPieceTable;
  CheckPageContent(APage);
end;


procedure TdxSpellCheckerCustomController.CheckPieceTable;
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AStart := TdxPositionConverter.ToDocumentModelPosition(PieceTable, PieceTable.DocumentStartLogPosition);
  AEnd := TdxPositionConverter.ToDocumentModelPosition(PieceTable, PieceTable.DocumentEndLogPosition);
  CheckContent(AStart, AEnd, 0);
end;

procedure TdxSpellCheckerCustomController.CheckFloatingObjects(APage: TdxPage);
var
  AList: TdxFloatingObjectBoxList;
begin
  CheckFloatingObjects(APage.BackgroundFloatingObjects);
  AList := APage.GetSortedNonBackgroundFloatingObjects;
  try
    CheckFloatingObjects(AList);
  finally
    AList.Free;
  end;
end;

procedure TdxSpellCheckerCustomController.CheckFloatingObjects(const AFloatingObjects: TdxFloatingObjectBoxList);
var
  ACount, I: Integer;
  AFloatingObject: TdxFloatingObjectBox;
begin
  if AFloatingObjects = nil then
    Exit;
  ACount := AFloatingObjects.Count;
  for I := 0 to ACount - 1 do
  begin
    AFloatingObject := AFloatingObjects[I];
    if (AFloatingObject.PieceTable = PieceTable) and (AFloatingObject.DocumentLayout <> nil) then
      CheckTextBoxContent(AFloatingObject);
  end;
end;

procedure TdxSpellCheckerCustomController.CheckTextBoxContent(AFloatingObject: TdxFloatingObjectBox);
var
  ARun: TdxFloatingObjectAnchorRun;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
  APrevDocumentLayout: TdxDocumentLayout;
  APrevPieceTable: TdxPieceTable;
begin
  ARun := TdxFloatingObjectAnchorRun(AFloatingObject.PieceTable.Runs[AFloatingObject.StartPos.RunIndex]);
  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  if not ATextBoxContent.TextBox.IsTextBox then
    Exit;

  APrevDocumentLayout := DocumentLayout;
  DocumentLayout := AFloatingObject.DocumentLayout;
  APrevPieceTable := PieceTable;
  PieceTable := TdxPieceTable(ATextBoxContent.TextBox.PieceTable);
  try
    CheckPieceTable;
  finally
    DocumentLayout := APrevDocumentLayout;
    PieceTable := APrevPieceTable;
  end;
end;

procedure TdxSpellCheckerCustomController.CheckPageContent(APage: TdxPage);
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AStart := APage.GetFirstPosition(PieceTable);
  AEnd := APage.GetLastPosition(PieceTable);
  CheckContent(AStart, AEnd, APage.PageIndex);
end;

function TdxSpellCheckerCustomController.CanCheckDocument: Boolean;
begin
  Result := (DocumentLayout <> nil) and Control.IsEditable;
end;

procedure TdxSpellCheckerCustomController.CheckHeaderFooter(AArea: TdxPageArea; APageIndex: Integer);
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AStart := AArea.GetFirstPosition(PieceTable);
  AEnd := AArea.GetLastPosition(PieceTable);
  CheckContent(AStart, AEnd, APageIndex);
end;

{ TdxBoxLayoutInfo }

constructor TdxBoxLayoutInfo.Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition);
begin
  inherited Create;
  FStart := TdxExpandedDocumentLayoutPosition.Create(ADocumentLayout, APieceTable, AStart);
  FEnd := TdxExpandedDocumentLayoutPosition.Create(ADocumentLayout, APieceTable, AEnd);
end;

destructor TdxBoxLayoutInfo.Destroy;
begin
  FreeAndNil(FStart);
  FreeAndNil(FEnd);
  inherited Destroy;
end;

function TdxBoxLayoutInfo.GetStartLogPosition: TdxDocumentLogPosition;
begin
  Result := Start.LogPosition;
end;

function TdxBoxLayoutInfo.GetEndLogPosition: TdxDocumentLogPosition;
begin
  Result := &End.LogPosition;
end;

{ TdxDocumentLayoutIterator }

constructor TdxDocumentLayoutIterator.Create(ADocumentLayout: TdxDocumentLayout;
  ABoxInfo: TdxBoxLayoutInfo; AStopInHeaderFooter: Boolean = False);
begin
  Assert(ADocumentLayout <> nil);
  Assert(ABoxInfo <> nil);
  inherited Create;
  FCurrentBoxIndex := -1;
  FCurrentRowIndex := -1;
  FCurrentColumnIndex := -1;
  FCurrentPageAreaIndex := -1;
  FCurrentPageIndex := -1;
  FDocumentLayout := ADocumentLayout;
  FBoxInfo := ABoxInfo;
  FStopInHeaderFooter := AStopInHeaderFooter;

  Initialize;
end;

function TdxDocumentLayoutIterator.GetStart: TdxDocumentLayoutPosition;
begin
  Result := BoxInfo.Start;
end;

function TdxDocumentLayoutIterator.GetEnd: TdxDocumentLayoutPosition;
begin
  Result := BoxInfo.&End;
end;

function TdxDocumentLayoutIterator.GetCurrentPage: TdxPage;
begin
  if FCurrentPageIndex >= 0 then
    Result := DocumentLayout.Pages[FCurrentPageIndex]
  else
    Result := &End.Page;
end;

function TdxDocumentLayoutIterator.GetCurrentPageArea: TdxPageArea;
begin
  if StopInHeaderFooter and ((Header <> nil) or (Footer <> nil)) then
  begin
    if Header <> nil then
      Exit(Header)
    else
      Exit(Footer);
  end;
  if FCurrentPageAreaIndex >= 0 then
    Result := CurrentPage.Areas[FCurrentPageAreaIndex]
  else
    Result := &End.PageArea;
end;

function TdxDocumentLayoutIterator.GetCurrentColumn: TdxColumn;
begin
  if FCurrentColumnIndex >= 0 then
    Result := CurrentPageArea.Columns[FCurrentColumnIndex]
  else
    Result := &End.Column;
end;

function TdxDocumentLayoutIterator.GetRows: TdxRowCollection;
begin
  if TableCell <> nil then
    Exit(TableCell.GetRows(CurrentColumn));
  Result := CurrentColumn.Rows;
end;

function TdxDocumentLayoutIterator.GetCurrentRow: TdxRow;
begin
  if FCurrentRowIndex >= 0 then
    Result := Rows[FCurrentRowIndex]
  else
    Result := &End.Row;
end;

function TdxDocumentLayoutIterator.GetCurrentBox: TdxBox;
begin
  if FCurrentBoxIndex >= 0 then
    Result := CurrentRow.Boxes[FCurrentBoxIndex]
  else
    Result := &End.Box;
end;

procedure TdxDocumentLayoutIterator.Initialize;
begin
  FTableCell := BoxInfo.Start.TableCell;
  FCurrentPageIndex := BoxInfo.Start.PageIndex;
  FCurrentPageAreaIndex := BoxInfo.Start.PageAreaIndex;
  FCurrentColumnIndex := BoxInfo.Start.ColumnIndex;
  FCurrentRowIndex := BoxInfo.Start.RowIndex;
  FCurrentBoxIndex := BoxInfo.Start.BoxIndex;

  SetHeaderFooter;
end;

procedure TdxDocumentLayoutIterator.SetHeaderFooter;
begin
  if not StopInHeaderFooter then
    Exit;
  if CurrentPage.Header <> nil then
    FHeader := CurrentPage.Header
  else
    FFooter := CurrentPage.Footer;
end;

function TdxDocumentLayoutIterator.MoveNext(ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  if (ADetailsLevel = TdxDocumentLayoutDetailsLevel.Box) and (CurrentBox <> nil) then
    Exit(MoveBoxToNext);
  if (ADetailsLevel = TdxDocumentLayoutDetailsLevel.Row) and (CurrentRow <> nil) then
    Exit(MoveRowToNext);
  if (ADetailsLevel = TdxDocumentLayoutDetailsLevel.Column) and (CurrentColumn <> nil) then
    Exit(MoveColumnToNext);
  if (ADetailsLevel = TdxDocumentLayoutDetailsLevel.PageArea) and (CurrentPageArea <> nil) then
    Exit(MovePageAreaToNext);
  if (ADetailsLevel = TdxDocumentLayoutDetailsLevel.Page) and (CurrentPage <> nil) then
    Exit(MovePageToNext);
  Result := False;
end;

function TdxDocumentLayoutIterator.MoveBoxToNext: Boolean;
begin
  if CurrentBox = &End.Box then
    Exit(False);

  Inc(FCurrentBoxIndex);
  if FCurrentBoxIndex < CurrentRow.Boxes.Count then
    Exit(True);
  FCurrentBoxIndex := 0;
  Result := MoveRowToNext;
end;

function TdxDocumentLayoutIterator.MoveRowToNext: Boolean;
begin
  if CurrentRow = &End.Row then
    Exit(False);

  Inc(FCurrentRowIndex);
  if FCurrentRowIndex < Rows.Count then
    Exit(True);

  if TableCell <> nil then
    Exit(MoveTableCellRowToNext)
  else
  begin
    FCurrentRowIndex := 0;
    Exit(MoveColumnToNext);
  end;
end;

function TdxDocumentLayoutIterator.MoveTableCellRowToNext: Boolean;
var
  ALastCellRow: TdxRow;
  ALastCellRowIndex, ANextRowIndex: Integer;
begin
  ALastCellRow := TableCell.GetLastRow(CurrentColumn);
  if IsRowLastInCurrentColumn(ALastCellRow) then
  begin
    FCurrentRowIndex := 0;
    FTableCell := nil;
    if MoveColumnToNext then
    begin
      if CurrentRow is TdxTableCellRow then
        FTableCell := (TdxTableCellRow(CurrentRow)).CellViewInfo;
      Exit(True);
    end
    else
      Exit(False);
  end;

  ALastCellRowIndex := CurrentColumn.Rows.IndexOf(ALastCellRow);
  if ALastCellRowIndex < 0 then
    TdxRichEditExceptions.ThrowInternalException;

  ANextRowIndex := ALastCellRowIndex + 1;
  if CurrentColumn.Rows[ANextRowIndex] is TdxTableCellRow then
  begin
    FCurrentRowIndex := 0;
    FTableCell := (TdxTableCellRow(CurrentColumn.Rows[ANextRowIndex])).CellViewInfo;
  end
  else
  begin
    FCurrentRowIndex := ANextRowIndex;
    FTableCell := nil;
  end;
  Result := True;
end;

function TdxDocumentLayoutIterator.IsRowLastInCurrentColumn(ARow: TdxRow): Boolean;
begin
  Result := CurrentColumn.Rows.Last = ARow;
end;

function TdxDocumentLayoutIterator.MoveColumnToNext: Boolean;
begin
  if CurrentColumn = &End.Column then
    Exit(False);

  Inc(FCurrentColumnIndex);
  if FCurrentColumnIndex < CurrentPageArea.Columns.Count then
    Exit(True);
  FCurrentColumnIndex := 0;
  Result := MovePageAreaToNext;
end;

function TdxDocumentLayoutIterator.MovePageAreaToNext: Boolean;
begin
  if CurrentPageArea = &End.PageArea then
    Exit(False);

  if Header <> nil then
  begin
    FHeader := nil;
    FFooter := CurrentPage.Footer;
  end
  else
    if Footer <> nil then
      FFooter := nil
    else
    begin
      Inc(FCurrentPageAreaIndex);
      if FCurrentPageAreaIndex < CurrentPage.Areas.Count then
        Exit(True);
      FCurrentPageAreaIndex := 0;
      Exit(MovePageToNext);
    end;
  Result := True;
end;

function TdxDocumentLayoutIterator.MovePageToNext: Boolean;
begin
  if CurrentPage = &End.Page then
    Exit(False);
  Inc(FCurrentPageIndex);
  if not CurrentPage.PrimaryFormattingComplete then
    Exit(False);
  SetHeaderFooter;
  Result := True;
end;

{ TdxSpellCheckerErrorBoxCalculator }

constructor TdxSpellCheckerErrorBoxCalculator.Create(ADocumentLayout: TdxDocumentLayout; ABoxMeasurer: TdxBoxMeasurer;
  APieceTable: TdxPieceTable);
begin
  Assert(ADocumentLayout <> nil);
  Assert(ABoxMeasurer <> nil);
  Assert(APieceTable <> nil);
  inherited Create;
  FDocumentLayout := ADocumentLayout;
  FBoxMeasurer := ABoxMeasurer;
  FPieceTable := APieceTable;
end;

destructor TdxSpellCheckerErrorBoxCalculator.Destroy;
begin
  FreeAndNil(FLastCalcLayoutPos);
  inherited Destroy;
end;

procedure TdxSpellCheckerErrorBoxCalculator.Calculate(APageIndex: Integer; const AStartPos, AEndPos: TdxDocumentModelPosition;
  AErrorType: TdxSpellingError);
var
  ABoxInfo: TdxBoxLayoutInfo;
  AStart, AEnd: TdxFormatterPosition;
begin
  Assert(AEndPos.LogPosition >= AStartPos.LogPosition);

  ABoxInfo := TdxBoxLayoutInfo.Create(DocumentLayout, PieceTable, AStartPos.LogPosition, AEndPos.LogPosition);
  try
    if (FLastCalcLayoutPos <> nil) and (APageIndex = FLastCalcLayoutPos.PageIndex) then
    begin
      if not Update(ABoxInfo, FLastCalcLayoutPos, TdxDocumentLayoutDetailsLevel.Box) then
      begin
        FreeAndNil(FLastCalcLayoutPos);
        Exit;
      end;
    end
    else
    begin
      FreeAndNil(FLastCalcLayoutPos);
      if not Update(ABoxInfo, APageIndex, TdxDocumentLayoutDetailsLevel.Box) then
        Exit;
    end;

    AStart := TdxPositionConverter.ToFormatterPosition(AStartPos);
    AEnd := TdxPositionConverter.ToFormatterPosition(AEndPos);
    CalculateCore(ABoxInfo, AStart, AEnd, AErrorType);
    FLastCalcLayoutPos.Free;
    FLastCalcLayoutPos := TdxExpandedDocumentLayoutPosition(ABoxInfo.&End.Clone);
  finally
    ABoxInfo.Free;
  end;
end;

function TdxSpellCheckerErrorBoxCalculator.Update(ABoxInfo: TdxBoxLayoutInfo; AIndex: Integer;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
var
  APages: TdxPageCollection;
begin
  APages := DocumentLayout.Pages;
  Result := ABoxInfo.Start.Update(APages, AIndex, ADetailsLevel) and ABoxInfo.&End.Update(APages, ABoxInfo.Start, ADetailsLevel);
end;

function TdxSpellCheckerErrorBoxCalculator.Update(ABoxInfo: TdxBoxLayoutInfo; AFrom: TdxExpandedDocumentLayoutPosition;
  ADetailsLevel: TdxDocumentLayoutDetailsLevel): Boolean;
var
  APages: TdxPageCollection;
begin
  APages := DocumentLayout.Pages;
  Result := ABoxInfo.Start.Update(APages, AFrom, ADetailsLevel) and ABoxInfo.&End.Update(APages, ABoxInfo.Start, ADetailsLevel);
end;

procedure TdxSpellCheckerErrorBoxCalculator.CalculateCore(ABoxInfo: TdxBoxLayoutInfo; AStart: TdxFormatterPosition;
  AEnd: TdxFormatterPosition; AErrorType: TdxSpellingError);
var
  AIterator: TdxDocumentLayoutIterator;
  AErrorBox: TdxErrorBox;
  AFirst, ALast: TdxBox;
begin
  AIterator := TdxDocumentLayoutIterator.Create(DocumentLayout, ABoxInfo);
  try
    if AIterator.Start.Row <> AIterator.&End.Row then
      CalculateErrorBoxInSomeRows(AIterator, AStart, AEnd, AErrorType)
    else
    begin
      AErrorBox := TdxErrorBox.Create;
      AErrorBox.ErrorType := AErrorType;
      AErrorBox.StartPos := AStart;
      AErrorBox.EndPos := AEnd;
      AFirst := AIterator.Start.Box;
      ALast := AIterator.&End.Box;
      CalculateErrorBox(AIterator.Start.Row, AErrorBox, AFirst, ALast);
    end;
  finally
    AIterator.Free;
  end;
end;

procedure TdxSpellCheckerErrorBoxCalculator.CalculateErrorBoxInSomeRows(AIterator: TdxDocumentLayoutIterator;
  AStart: TdxFormatterPosition; AEnd: TdxFormatterPosition; AErrorType: TdxSpellingError);
begin
  repeat
    CalculateErrorBoxInCurrentRow(AIterator, AStart, AEnd, AErrorType);
  until not AIterator.MoveRowToNext;
end;

procedure TdxSpellCheckerErrorBoxCalculator.ClearErrorBoxes(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition);
var
  ABoxInfo: TdxBoxLayoutInfo;
  AIterator: TdxDocumentLayoutIterator;
begin
  ABoxInfo := TdxBoxLayoutInfo.Create(DocumentLayout, PieceTable, AStart, AEnd);
  try
    if not ABoxInfo.Start.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row) then
      Exit;
    if not ABoxInfo.&End.Update(DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row) then
      Exit;

    AIterator := TdxDocumentLayoutIterator.Create(DocumentLayout, ABoxInfo, True);
    try
      repeat
        AIterator.CurrentRow.ClearErrors;
      until not AIterator.MoveRowToNext;
    finally
      AIterator.Free;
    end;
  finally
    ABoxInfo.Free;
  end;
end;

procedure TdxSpellCheckerErrorBoxCalculator.CalculateErrorBoxInCurrentRow(AIterator: TdxDocumentLayoutIterator;
  AStart: TdxFormatterPosition; AEnd: TdxFormatterPosition; AErrorType: TdxSpellingError);
var
  ARow: TdxRow;
  AErrorBox: TdxErrorBox;
  AFirst, ALast: TdxBox;
begin
  ARow := AIterator.CurrentRow;
  AErrorBox := TdxErrorBox.Create;
  AErrorBox.ErrorType := AErrorType;
  AFirst := FirstBox(AIterator.Start.Box, AIterator.CurrentRow.Boxes.First);
  ALast := LastBox(AIterator.&End.Box, AIterator.CurrentRow.Boxes.Last);
  AErrorBox.StartPos := MaxPosition(AFirst.StartPos, AStart);
  AErrorBox.EndPos := MinPosition(ALast.EndPos, AEnd);
  CalculateErrorBox(ARow, AErrorBox, AFirst, ALast);
end;

function TdxSpellCheckerErrorBoxCalculator.FirstBox(ABox1: TdxBox; ABox2: TdxBox): TdxBox;
begin
  if ABox1.StartPos >= ABox2.StartPos then
    Exit(ABox1);
  Result := ABox2;
end;

function TdxSpellCheckerErrorBoxCalculator.LastBox(ABox1: TdxBox; ABox2: TdxBox): TdxBox;
begin
  if ABox1.EndPos <= ABox2.EndPos then
    Exit(ABox1);
  Result := ABox2;
end;

function TdxSpellCheckerErrorBoxCalculator.MaxPosition(APosition1: TdxFormatterPosition; APosition2: TdxFormatterPosition): TdxFormatterPosition;
begin
  if APosition1 >= APosition2 then
    Exit(APosition1);
  Result := APosition2;
end;

function TdxSpellCheckerErrorBoxCalculator.MinPosition(APosition1: TdxFormatterPosition; APosition2: TdxFormatterPosition): TdxFormatterPosition;
begin
  if APosition1 <= APosition2 then
    Exit(APosition1);
  Result := APosition2;
end;

procedure TdxSpellCheckerErrorBoxCalculator.CalculateErrorBox(ARow: TdxRow; AErrorBox: TdxErrorBox; AFirst: TdxBox; ALast: TdxBox);
begin
  if FLastProcessedRow <> ARow then
  begin
    ARow.ClearErrors;
    FLastProcessedRow := ARow;
  end;
  ARow.Errors.Add(AErrorBox);
  CalculateErrorBoxBounds(ARow, AErrorBox, AFirst, ALast);
end;

procedure TdxSpellCheckerErrorBoxCalculator.CalculateErrorBoxBounds(ARow: TdxRow; AErrorBox: TdxErrorBox; AFirst: TdxBox; ALast: TdxBox);
var
  AFontInfo: TdxFontInfo;
  AUnderlineLeft, AUnderlineRight, ABaseLinePosition, AUnderlineTop, AUnderlineBottom, ABottomDistance: Integer;
begin
  AFontInfo := GetFontInfo(ARow.Boxes[AErrorBox.StartAnchorIndex]);
  AErrorBox.UnderlinePosition := AFontInfo.UnderlinePosition;
  AErrorBox.UnderlineThickness := AFontInfo.UnderlineThickness;

  AUnderlineLeft := GetUnderlineLeft(AFirst, AErrorBox);
  AUnderlineRight := GetUnderlineRight(ALast, AErrorBox);

  ABaseLinePosition := ARow.Bounds.Top + ARow.BaseLineOffset + AErrorBox.UnderlineBounds.Top;
  AUnderlineTop := ABaseLinePosition + AErrorBox.UnderlinePosition div 2;
  AUnderlineBottom := ABaseLinePosition + AErrorBox.UnderlinePosition + AErrorBox.UnderlineThickness;

  ABottomDistance := ARow.Bounds.Bottom - AUnderlineBottom;
  if ABottomDistance < 0 then
  begin
    Inc(AUnderlineTop, ABottomDistance);
    Inc(AUnderlineBottom, ABottomDistance);
  end;

  AErrorBox.UnderlineBounds := TRect.Create(ARow.Bounds.Left, AUnderlineTop, ARow.Bounds.Right, AUnderlineBottom);
  AErrorBox.ClipBounds := TRect.Create(AUnderlineLeft, AUnderlineTop, AUnderlineRight, AUnderlineBottom);
end;

function TdxSpellCheckerErrorBoxCalculator.GetFontInfo(ABox: TdxBox): TdxFontInfo;
begin
  Result := ABox.GetFontInfo(PieceTable);
end;

function TdxSpellCheckerErrorBoxCalculator.GetUnderlineLeft(AFirst: TdxBox; AErrorBox: TdxErrorBox): Integer;
var
  AStartOffset: Integer;
begin
  if AErrorBox.StartPos.RunIndex <> AFirst.StartPos.RunIndex then
    Exit(AFirst.Bounds.Left);
  AStartOffset := AErrorBox.StartPos.Offset - AFirst.StartPos.Offset;
  Result := AFirst.Bounds.Left + GetFirstBoxOffset(AFirst, AStartOffset);
end;

function TdxSpellCheckerErrorBoxCalculator.GetUnderlineRight(ALast: TdxBox; AErrorBox: TdxErrorBox): Integer;
var
  AEndOffset: Integer;
begin
  if AErrorBox.EndPos.RunIndex <> ALast.EndPos.RunIndex then
    Exit(ALast.Bounds.Left);
  AEndOffset := ALast.EndPos.Offset - AErrorBox.EndPos.Offset;
  Result := ALast.Bounds.Right - GetLastBoxOffset(ALast, AEndOffset);
end;

function TdxSpellCheckerErrorBoxCalculator.GetFirstBoxOffset(ABox: TdxBox; AOffset: Integer): Integer;
var
  ABoxText: string;
  ACharBounds: TArray<TRect>;
  ACount, I: Integer;
begin
  if AOffset = 0 then
    Exit(0);

  ABoxText := GetBoxText(ABox);
  if AOffset >= Length(ABoxText) then
    Exit(ABox.Bounds.Width);
  ACharBounds := GetCharactersBounds(ABox, ABoxText);
  Assert(Length(ACharBounds) = Length(ABoxText));
  if Length(ACharBounds) < AOffset then
    Exit(0);
  ACount := AOffset;
  Result := 0;
  for I := 0 to ACount - 1 do
    Inc(Result, ACharBounds[I].Width);
end;

function TdxSpellCheckerErrorBoxCalculator.GetLastBoxOffset(ABox: TdxBox; AOffset: Integer): Integer;
var
  ABoxText: string;
  ACharBounds: TArray<TRect>;
  ACharCount, AEndIndex, I: Integer;
begin
  if AOffset = 0 then
    Exit(0);

  ABoxText := GetBoxText(ABox);
  if AOffset >= Length(ABoxText) then
    Exit(ABox.Bounds.Width);
  ACharBounds := GetCharactersBounds(ABox, ABoxText);
  Assert(Length(ACharBounds) = Length(ABoxText));
  if Length(ACharBounds) < Length(ABoxText) then
    Exit(0);
  ACharCount := Length(ABoxText);
  AEndIndex := ACharCount - AOffset;
  Result := 0;
  for I := ACharCount - 1 downto AEndIndex do
    Inc(Result, ACharBounds[I].Width);
end;

function TdxSpellCheckerErrorBoxCalculator.GetCharactersBounds(ABox: TdxBox; const ABoxText: string): TArray<TRect>;
begin
  Result := BoxMeasurer.MeasureCharactersBounds(ABoxText, GetFontInfo(ABox), ABox.Bounds);
end;

function TdxSpellCheckerErrorBoxCalculator.GetBoxText(ABox: TdxBox): string;
begin
  Result := PieceTable.GetTextFromSingleRun(ABox.StartPos, ABox.EndPos);
end;

{ TdxSyntaxCheckController }

constructor TdxSyntaxCheckController.Create(const AControl: IdxInnerControl; const ASyntaxChecker: IdxSyntaxCheckService);
begin
  Assert(ASyntaxChecker <> nil);
  inherited Create(AControl);
  FSyntaxChecker := ASyntaxChecker;
end;

procedure TdxSyntaxCheckController.CheckContent(const AStart, AEnd: TdxDocumentModelPosition; APageIndex: Integer);
var
  AIntervals: TArray<TdxRunInfo>;
  ACount, I: Integer;
  ACalculator: TdxSpellCheckerErrorBoxCalculator;
begin
  AIntervals := FSyntaxChecker.Check(AStart, AEnd);
  ACount := Length(AIntervals);
  ACalculator := CreateErrorBoxCalculator;
  try
    for I := 0 to ACount - 1 do
      ACalculator.Calculate(APageIndex, AIntervals[I].Start, AIntervals[I].&End, seSyntax);
  finally
    ACalculator.Free;
  end;
end;

{ TdxIgnoreListManager }

constructor TdxIgnoreListManager.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
end;

function TdxIgnoreListManager.GetPieceTable: TdxPieceTable;
begin
  Result := DocumentModel.ActivePieceTable;
end;

function TdxIgnoreListManager.GetIgnoredList: TdxIgnoredList;
begin
  Result := PieceTable.SpellCheckerManager.IgnoredList;
end;

function TdxIgnoreListManager.GetMisspelledIntervals: TdxMisspelledIntervalCollection;
begin
  Result := PieceTable.SpellCheckerManager.MisspelledIntervals;
end;

procedure TdxIgnoreListManager.Add(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string);
var
  AStartPosition, AEndPosition: TdxDocumentPosition;
begin
  AStartPosition := TdxDocumentPosition(AStart);
  AEndPosition := TdxDocumentPosition(AEnd);
  if (AStartPosition = nil) or (AEndPosition = nil) then
    Exit;
  AddCore(AStartPosition, AEndPosition);
end;

procedure TdxIgnoreListManager.AddCore(const AStartPosition, AEndPosition: TdxDocumentPosition);
var
  AInterval: TdxMisspelledInterval;
begin
  AInterval := MisspelledIntervals.FindInterval(AStartPosition.LogPosition, AEndPosition.LogPosition);
  if AInterval <> nil then
  begin
    MisspelledIntervals.Extract(AInterval);
    IgnoredList.Add(AInterval);
  end
  else
    IgnoredList.Add(AStartPosition.Position, AEndPosition.Position);
end;

function TdxIgnoreListManager.Contains(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string): Boolean;
var
  AStartPosition, AEndPosition: TdxDocumentPosition;
begin
  AStartPosition := TdxDocumentPosition(AStart);
  AEndPosition := TdxDocumentPosition(AEnd);
  if (AStartPosition = nil) or (AEndPosition = nil) then
    Exit(False);
  Result := IgnoredList.Contains(AStartPosition.LogPosition, AEndPosition.LogPosition, AWord);
end;

procedure TdxIgnoreListManager.Remove(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string);
var
  AStartPosition, AEndPosition: TdxDocumentPosition;
begin
  AStartPosition := TdxDocumentPosition(AStart);
  AEndPosition := TdxDocumentPosition(AEnd);
  if (AStartPosition = nil) or (AEndPosition = nil) then
    Exit;
  RemoveCore(AStartPosition, AEndPosition);
end;

procedure TdxIgnoreListManager.RemoveCore(const AStartPosition, AEndPosition: TdxDocumentPosition);
var
  AInterval: TdxMisspelledInterval;
begin
  AInterval := IgnoredList.FindInterval(AStartPosition.LogPosition, AEndPosition.LogPosition);
  if AInterval <> nil then
  begin
    IgnoredList.Remove(AInterval);
    MisspelledIntervals.Add(AInterval);
  end;
end;

procedure TdxIgnoreListManager.Add(const AWord: string);
begin
  if IgnoredList.Contains(AWord) then
    Exit;
  IgnoredList.Add(AWord);
  PieceTable.SpellCheckerManager.InitializeUncheckedInterval;
end;

function TdxIgnoreListManager.Contains(const AWord: string): Boolean;
begin
  Result := IgnoredList.Contains(AWord);
end;

procedure TdxIgnoreListManager.Remove(const AWord: string);
begin
  if not IgnoredList.Remove(AWord) then
    Exit;
  PieceTable.SpellCheckerManager.InitializeUncheckedInterval;
end;

procedure TdxIgnoreListManager.Clear;
begin
  DocumentModel.ActivePieceTable.SpellCheckerManager.IgnoredList.Clear;
end;

{ TdxVisibleCharactersIterator }

constructor TdxVisibleCharactersIterator.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FCachedRunIndex := -1;
end;

function TdxVisibleCharactersIterator.GetVisibleTextFilter: IdxVisibleTextFilter;
begin
  Result := PieceTable.VisibleTextFilter;
end;

procedure TdxVisibleCharactersIterator.MoveForwardCore(var APos: TdxDocumentModelPosition);
begin
  TdxDocumentModelPosition.MoveForwardCore(APos);
  if not IsRunVisible(APos.RunIndex) then
  begin
    APos.LogPosition := VisibleTextFilter.GetNextVisibleLogPosition(APos, False);
    UpdateModelPositionByLogPosition(APos);
  end;
end;

procedure TdxVisibleCharactersIterator.MoveBackCore(var APos: TdxDocumentModelPosition);
begin
  TdxDocumentModelPosition.MoveBackwardCore(APos);
  if not IsRunVisible(APos.RunIndex) then
  begin
    APos.LogPosition := VisibleTextFilter.GetPrevVisibleLogPosition(APos, False);
    UpdateModelPositionByLogPosition(APos);
  end;
end;

function TdxVisibleCharactersIterator.IsRunVisible(ARunIndex: TdxRunIndex): Boolean;
begin
  if FCachedRunIndex <> ARunIndex then
  begin
    FCachedRunIndex := ARunIndex;
    FIsCachedRunVisible := VisibleTextFilter.IsRunVisible(ARunIndex);
  end;
  Result := FIsCachedRunVisible;
end;

{ TdxSentencePositionCalculator }

class constructor TdxSentencePositionCalculator.Initialize;
begin
  FSimpleSentenceSeparators := TdxCharList.Create;
  FSimpleSentenceSeparators.AddRange(['.', '!', '?']);
  FSpecialSentenceSeparators := TdxCharList.Create;
  FSpecialSentenceSeparators.AddRange([TdxCharacters.ParagraphMark, TdxCharacters.ColumnBreak,
    TdxCharacters.PageBreak, TdxCharacters.SectionMark, TdxCharacters.LineBreak,
    TdxCharacters.FloatingObjectMark, TdxCharacters.ObjectMark]);
end;

class destructor TdxSentencePositionCalculator.Finalize;
begin
  FreeAndNil(FSimpleSentenceSeparators);
  FreeAndNil(FSpecialSentenceSeparators);
end;

constructor TdxSentencePositionCalculator.Create(AWordIterator: TdxSpellCheckerWordIterator);
begin
  inherited Create;
  FWordIterator := AWordIterator;
end;

function TdxSentencePositionCalculator.GetSentenceEndPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  AStart, AFinish: TdxDocumentModelPosition;
begin
  Result := APos;
  while not IsEndOfDocument(Result, FWordIterator) do
  begin
    Result := GetSentenceEndPositionCore(Result);
    AStart := FindPrevLetterOrDigitPosition(Result);
    AFinish := FindLetterOrDigitPosition(Result);
    Result := ProcessPunctuation(AStart, AFinish);
    if not {$IFDEF DELPHIXE4}GetCharacter(Result).IsLetter{$ELSE}TCharacter.IsLetter(GetCharacter(Result)){$ENDIF} then
    begin
      FWordIterator.SkipForward(Result, IsSimpleSentenceSeparator);
      Break;
    end;
  end;
end;

function TdxSentencePositionCalculator.GetSentenceEndPositionCore(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  if IsEndOfDocument(APos, FWordIterator) then
    Exit(APos);
  Result := FWordIterator.MoveToWordEnd(APos);
  while not IsEndOfDocument(Result, FWordIterator) and not IsSentenceSeparator(GetCharacter(Result)) do
  begin
    FWordIterator.SkipForward(Result, IsNotWordSymbolAndNotSentenceSeparator);
    Result := FWordIterator.MoveToWordEnd(Result);
  end;
end;

function TdxSentencePositionCalculator.GetSentenceStartPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  AStart, AFinish: TdxDocumentModelPosition;
begin
  Result := APos;
  while not IsStartOfDocument(Result, FWordIterator) do
  begin
    Result := GetSentenceStartPositionCore(Result);
    if IsStartOfDocument(Result, FWordIterator) then
      Exit(Result);
    AStart := FindPrevLetterOrDigitPosition(Result);
    AFinish := FindLetterOrDigitPosition(Result);
    if not {$IFDEF DELPHIXE4}GetCharacter(ProcessPunctuation(AStart, AFinish)).IsLetter{$ELSE}TCharacter.IsLetter(GetCharacter(ProcessPunctuation(AStart, AFinish))){$ENDIF} then
      Break
    else
      Result.CopyFrom(AStart);
  end;
end;

function TdxSentencePositionCalculator.GetSentenceStartPositionCore(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  ASentenceStart: TdxDocumentModelPosition;
begin
  if IsStartOfDocument(APos, FWordIterator) then
    Exit(APos);
  Result := FWordIterator.MoveToWordStart(APos);
  while not IsStartOfDocument(Result, FWordIterator) do
  begin
    ASentenceStart := FindSentenceStart(Result);
    if ASentenceStart.IsValid then
      Exit(ASentenceStart);
    FWordIterator.MoveToPrevChar(Result);
    Result := FWordIterator.MoveToWordStart(Result);
  end;
end;

function TdxSentencePositionCalculator.FindSentenceStart(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  FWordIterator.MoveToPrevChar(Result);
  FWordIterator.SkipBackward(Result, IsNotWordSymbolAndNotSentenceSeparator);
  if not IsSentenceSeparator(GetCharacter(Result)) then
    Exit(TdxDocumentModelPosition.Null);
  FWordIterator.SkipForward(Result, function (const ACh: Char): Boolean
    begin
      Result := IsSentenceSeparator(ACh) or IsSpace(ACh);
    end);
end;

function TdxSentencePositionCalculator.ProcessPunctuation(const AStart, AFinish: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  ACurrentPosition, ASpacePositionAfterThreeDots, ASeparatorPosition, APunctuationPosition,
  ASpacePositionAfterSimpleSentenceSeparator: TdxDocumentModelPosition;
  AIsNextWordStartWithUpperCase, AIsThreeDotsSkipped: Boolean;
  ASentenceSeparatorsCount: Integer;
  ACh: Char;
begin
  ACurrentPosition := FindAbbreviation(AStart, AFinish);
  if ACurrentPosition.IsValid then
    Exit(ACurrentPosition);
  ACurrentPosition := AStart;
  AIsNextWordStartWithUpperCase := {$IFDEF DELPHIXE4}GetCharacter(AFinish).IsUpper{$ELSE}TCharacter.IsUpper(GetCharacter(AFinish)){$ENDIF};
  ASentenceSeparatorsCount := 0;
  while (ACurrentPosition <= AFinish) and not FWordIterator.IsEndOfDocument(ACurrentPosition) do
  begin
    ACh := GetCharacter(ACurrentPosition);
    if IsSpecialSentenceSeparator(ACh) then
      Exit(ACurrentPosition);
    if not IsSimpleSentenceSeparator(ACh) then
    begin
      ASentenceSeparatorsCount := 0;
      FWordIterator.MoveToNextChar(ACurrentPosition);
      Continue;
    end;
    ASpacePositionAfterThreeDots := FindThreeDotsWithSpace(ACurrentPosition);
    if ASpacePositionAfterThreeDots.IsValid then
    begin
      ASeparatorPosition := FindSentenceSeparatorAfterThreeDotsWithSpace(ASpacePositionAfterThreeDots);
      if ASeparatorPosition.IsValid then
        Exit(ASeparatorPosition);
      APunctuationPosition := FindPunctuationAfterThreeDotsWithSpace(ASpacePositionAfterThreeDots);
      if APunctuationPosition.IsValid then
      begin
        ASentenceSeparatorsCount := 0;
        ACurrentPosition.CopyFrom(APunctuationPosition);
        Continue;
      end;
      if AIsNextWordStartWithUpperCase then
        Exit(ASpacePositionAfterThreeDots);
    end;

    ASpacePositionAfterSimpleSentenceSeparator := FindSentenceSeparatorWithSpace(ACurrentPosition);
    if ASpacePositionAfterSimpleSentenceSeparator.IsValid then
      Exit(ASpacePositionAfterSimpleSentenceSeparator);

    AIsThreeDotsSkipped := SkipThreeDots(ACurrentPosition);
    if AIsThreeDotsSkipped then
      ASentenceSeparatorsCount := 0;
    if ASentenceSeparatorsCount > 1 then
      Exit(ACurrentPosition);
    if not AIsThreeDotsSkipped then
      FWordIterator.MoveToNextChar(ACurrentPosition);
  end;
  Result := AFinish;
end;

function TdxSentencePositionCalculator.FindSentenceSeparatorAfterThreeDotsWithSpace(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  FWordIterator.SkipForward(Result, IsSpace);
  if not IsSentenceSeparator(GetCharacter(Result)) then
    Result := TdxDocumentModelPosition.Null;
end;

function TdxSentencePositionCalculator.FindPunctuationAfterThreeDotsWithSpace(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  FWordIterator.SkipForward(Result, IsSpace);
  if not IsNotWordSymbolAndNotSentenceSeparator(GetCharacter(Result)) then
    Result := TdxDocumentModelPosition.Null;
end;

function TdxSentencePositionCalculator.FindAbbreviation(const AStart, AFinish: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  ASentenceSeparatorsCount: Integer;
  ACurrentPosition: TdxDocumentModelPosition;
  ACh: Char;
begin
  if not {$IFDEF DELPHIXE4}GetCharacter(AStart).IsLetter{$ELSE}TCharacter.IsLetter(GetCharacter(AStart)){$ENDIF} then
    Exit(TdxDocumentModelPosition.Null);
  if not IsStartOfDocument(AStart, FWordIterator) and PrevCharIsLetter(AStart) then
    Exit(TdxDocumentModelPosition.Null);
  if not NextCharIsDot(AStart) then
    Exit(TdxDocumentModelPosition.Null);
  ASentenceSeparatorsCount := 0;
  ACurrentPosition := AStart;
  while ACurrentPosition <> AFinish do
  begin
    if ASentenceSeparatorsCount > 1 then
      Exit(TdxDocumentModelPosition.Null);
    FWordIterator.MoveToNextChar(ACurrentPosition);
    ACh := GetCharacter(ACurrentPosition);
    if IsSpecialSentenceSeparator(ACh) then
      Exit(TdxDocumentModelPosition.Null);
    if not IsSimpleSentenceSeparator(ACh) then
      ASentenceSeparatorsCount := 0;
  end;
  Result := AFinish;
end;

function TdxSentencePositionCalculator.PrevCharIsLetter(const APos: TdxDocumentModelPosition): Boolean;
var
  ANewPos: TdxDocumentModelPosition;
begin
  ANewPos := APos;
  FWordIterator.MoveToPrevChar(ANewPos);
  Result := {$IFDEF DELPHIXE4}GetCharacter(ANewPos).IsLetter{$ELSE}TCharacter.IsLetter(GetCharacter(ANewPos)){$ENDIF};
end;

function TdxSentencePositionCalculator.NextCharIsDot(const APos: TdxDocumentModelPosition): Boolean;
var
  ANewPos: TdxDocumentModelPosition;
begin
  ANewPos := APos;
  FWordIterator.MoveToNextChar(ANewPos);
  Result := IsDot(GetCharacter(ANewPos));
end;

function TdxSentencePositionCalculator.FindLetterOrDigitPosition(const AStartPosition: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := AStartPosition;
  FWordIterator.SkipForward(Result, FWordIterator.IsNotLetterOrDigit);
end;

function TdxSentencePositionCalculator.FindPrevLetterOrDigitPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  if {$IFDEF DELPHIXE4}GetCharacter(APos).IsLetter{$ELSE}TCharacter.IsLetter(GetCharacter(APos)){$ENDIF} then
    FWordIterator.MoveToPrevChar(Result);
  FWordIterator.SkipBackward(Result, FWordIterator.IsNotLetterOrDigit);
end;

function TdxSentencePositionCalculator.FindSentenceSeparatorWithSpace(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  if not IsSimpleSentenceSeparator(GetCharacter(Result)) then
    Exit(TdxDocumentModelPosition.Null);
  FWordIterator.MoveToNextChar(Result);
  if not IsSpace(GetCharacter(Result)) then
    Result := TdxDocumentModelPosition.Null;
end;

function TdxSentencePositionCalculator.FindThreeDotsWithSpace(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  if not SkipThreeDots(Result) then
    Exit(TdxDocumentModelPosition.Null);
  if not IsSpace(GetCharacter(Result)) then
    Result := TdxDocumentModelPosition.Null;
end;

function TdxSentencePositionCalculator.SkipThreeDots(const APos: TdxDocumentModelPosition): Boolean;
var
  ANewPos: TdxDocumentModelPosition;
  ARemainingDotsCount: Integer;
begin
  if not IsDot(GetCharacter(APos)) then
    Exit(False);
  ANewPos := APos;
  ARemainingDotsCount := 2;
  while (ARemainingDotsCount > 0) and not IsEndOfDocument(ANewPos, FWordIterator) do
  begin
    FWordIterator.MoveToNextChar(ANewPos);
    if not IsDot(GetCharacter(ANewPos)) then
      Exit(False);
    Dec(ARemainingDotsCount);
  end;
  FWordIterator.MoveToNextChar(ANewPos);
  APos.CopyFrom(ANewPos);
  Result := True;
end;

function TdxSentencePositionCalculator.GetCharacter(const APos: TdxDocumentModelPosition): Char;
begin
  Result := FWordIterator.GetCharacter(APos);
end;

function TdxSentencePositionCalculator.IsNotWordSymbolAndNotSentenceSeparator(const ACh: Char): Boolean;
begin
  Result := FWordIterator.IsNotLetterOrDigit(ACh) and not IsSentenceSeparator(ACh);
end;

function TdxSentencePositionCalculator.IsDot(const ACh: Char): Boolean;
begin
  Result := ACh = TdxCharacters.Dot;
end;

function TdxSentencePositionCalculator.IsSpace(const ACh: Char): Boolean;
begin
  Result := (ACh = TdxCharacters.Space) or (ACh = TdxCharacters.NonBreakingSpace);
end;

function TdxSentencePositionCalculator.IsSpecialSentenceSeparator(const ACh: Char): Boolean;
begin
  Result := FSpecialSentenceSeparators.Contains(ACh);
end;

function TdxSentencePositionCalculator.IsSimpleSentenceSeparator(const ACh: Char): Boolean;
begin
  Result := FSimpleSentenceSeparators.Contains(ACh);
end;

function TdxSentencePositionCalculator.IsSentenceSeparator(const ACh: Char): Boolean;
begin
  Result := (IsSimpleSentenceSeparator(ACh)) or (IsSpecialSentenceSeparator(ACh));
end;

function TdxSentencePositionCalculator.IsStartOfDocument(const APos: TdxDocumentModelPosition; AWordIterator: TdxSpellCheckerWordIterator): Boolean;
begin
  Result := AWordIterator.IsStartOfDocument(APos);
end;

function TdxSentencePositionCalculator.IsEndOfDocument(const APos: TdxDocumentModelPosition; AWordIterator: TdxSpellCheckerWordIterator): Boolean;
begin
  Result := AWordIterator.IsEndOfDocument(APos);
end;

{ TdxRichEditTextController }

constructor TdxRichEditTextController.Create(const AControl: IdxInnerControl);
begin
  Assert(AControl <> nil);
  inherited Create;
  FControl := AControl;
end;

function TdxRichEditTextController.GetDocumentModel: TdxDocumentModel;
begin
  Result := Control.DocumentModel;
end;

function TdxRichEditTextController.GetText: string;
begin
  Result := FGuid;
end;

function TdxRichEditTextController.GetPieceTable: TdxPieceTable;
begin
  Result := DocumentModel.ActivePieceTable;
end;

function TdxRichEditTextController.GetSentencePositionCalculator(APieceTable: TdxCustomPieceTable): TdxSentencePositionCalculator;
begin
  Result := TdxSentencePositionCalculator.Create(GetWordIterator(APieceTable));
end;

function TdxRichEditTextController.DeleteWordCore(APieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition; AFinish: TdxDocumentLogPosition): Boolean;
begin
  if not CanEditRange(APieceTable, AStart, AFinish) then
    Exit(False);
  APieceTable.DeleteContent(AStart, AFinish - AStart, AStart >= APieceTable.DocumentEndLogPosition);
  Result := True;
end;

function TdxRichEditTextController.TryShiftStartPosition(const APos: TdxDocumentModelPosition; AIterator: TdxVisibleCharactersIterator): Boolean;
var
  APrevPos: TdxDocumentModelPosition;
begin
  if APos.LogPosition <= APos.PieceTable.DocumentStartLogPosition then
    Exit(False);

  APrevPos := AIterator.MoveBack(APos);
  if {$IFDEF DELPHIXE4}GetCharacter(APrevPos).IsWhiteSpace{$ELSE}TCharacter.IsWhiteSpace(GetCharacter(APrevPos)){$ENDIF} then
  begin
    APos.CopyFrom(APrevPos);
    Exit(True);
  end;
  Result := False;
end;

function TdxRichEditTextController.IsPositionVisible(const APos: IdxSpellCheckerPosition): Boolean;
var
  APieceTable: TdxPieceTable;
  AModelPos: TdxDocumentModelPosition;
begin
  AModelPos := TdxDocumentPosition(APos).Position;
  APieceTable := TdxPieceTable(AModelPos.PieceTable);
  Result := APieceTable.VisibleTextFilter.IsRunVisible(AModelPos.RunIndex);
end;

function TdxRichEditTextController.IsRangeEditable(const AStart, AFinish: IdxSpellCheckerPosition): Boolean;
var
  APieceTable: TdxPieceTable;
  AModelPos: TdxDocumentModelPosition;
begin
  AModelPos := TdxDocumentPosition(AStart).Position;
  APieceTable := TdxPieceTable(AModelPos.PieceTable);
  Result := APieceTable.CanEditRange(AStart.ToInteger, AFinish.ToInteger);
end;

function TdxRichEditTextController.TryShiftEndPosition(const APos: TdxDocumentModelPosition; AIterator: TdxVisibleCharactersIterator): Boolean;
var
  APrevPos: TdxDocumentModelPosition;
begin
  if APos.LogPosition >= APos.PieceTable.DocumentEndLogPosition then
    Exit(False);

  if {$IFDEF DELPHIXE4}GetCharacter(APos).IsWhiteSpace{$ELSE}TCharacter.IsWhiteSpace(GetCharacter(APos)){$ENDIF} then
  begin
    APrevPos := APos;
    AIterator.MoveForwardCore(APrevPos);
    APos.CopyFrom(APrevPos);
    Exit(True);
  end;
  Result := False;
end;

class function TdxRichEditTextController.GetCharacter(const APos: TdxDocumentModelPosition): Char;
var
  APieceTable: TdxPieceTable;
  ARunText: string;
begin
  APieceTable := TdxPieceTable(APos.PieceTable);
  ARunText := APieceTable.Runs[APos.RunIndex].GetNonEmptyText(APieceTable.TextBuffer);
  Result := ARunText[APos.RunOffset];
end;

function TdxRichEditTextController.GetModelPosition(APosition: TdxDocumentPosition): TdxDocumentModelPosition;
begin
  if APosition <> nil then
    Exit(APosition.Position);
  Result := TdxDocumentModelPosition.Create(PieceTable);
end;

function TdxRichEditTextController.CreateDocumentPosition(const APos: TdxDocumentModelPosition): TdxDocumentPosition;
begin
  Result := TdxDocumentPosition.Create(APos);
end;

function TdxRichEditTextController.CreatePosition(ALogPosition: Integer): IdxSpellCheckerPosition;
begin
  Result := CreateDocumentPosition(TdxPositionConverter.ToDocumentModelPosition(PieceTable, ALogPosition));
end;

function TdxRichEditTextController.GetVisibleText(APieceTable: TdxPieceTable; const AStart, AEnd: TdxDocumentModelPosition): string;
var
  AStartRun: TdxTextRunBase;
  AResult: TStringBuilder;
  I: TdxRunIndex;
begin
  if AEnd.LogPosition - AStart.LogPosition <= 0 then
    Exit('');
  AStartRun := APieceTable.Runs[AStart.RunIndex];
  if AStart.RunIndex = AEnd.RunIndex then
    Exit(GetRunText(APieceTable, AStart.RunIndex, AStart.RunOffset, AEnd.RunOffset - 1));
  AResult := TStringBuilder.Create;
  try
    AResult.Append(GetRunText(APieceTable, AStart.RunIndex, AStart.RunOffset, AStartRun.Length - 1));
    for I := AStart.RunIndex + 1 to AEnd.RunIndex - 1 do
      AResult.Append(GetRunText(APieceTable, I));
    if AEnd.RunOffset > 0 then
      AResult.Append(GetRunText(APieceTable, AEnd.RunIndex, 0, AEnd.RunOffset - 1));
    Result := AResult.ToString;
  finally
    AResult.Free;
  end;
end;

function TdxRichEditTextController.GetRunText(APieceTable: TdxPieceTable; AIndex: TdxRunIndex): string;
begin
  Result := GetRunText(APieceTable, AIndex, 0, APieceTable.Runs[AIndex].Length - 1);
end;

function TdxRichEditTextController.GetRunText(APieceTable: TdxPieceTable; AIndex: TdxRunIndex; AFrom: Integer; ATo: Integer): string;
begin
  if APieceTable.VisibleTextFilter.IsRunVisible(AIndex) then
    Exit(APieceTable.Runs[AIndex].GetText(APieceTable.TextBuffer, AFrom, ATo));
  Result := '';
end;

function TdxRichEditTextController.ReplaceWordCore(APieceTable: TdxPieceTable; const AWord: string; AStart: TdxDocumentLogPosition; AFinish: TdxDocumentLogPosition): Boolean;
begin
  if not CanEditRange(APieceTable, AStart, AFinish) then
    Exit(False);
  APieceTable.ReplaceText(AStart, AFinish - AStart, AWord);
  Result := True;
end;

function TdxRichEditTextController.CanCheckDocument: Boolean;
begin
  Result := True;
end;

function TdxRichEditTextController.CanEditRange(APieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition; AFinish: TdxDocumentLogPosition): Boolean;
begin
  Result := Control.IsEditable and APieceTable.CanEditRange(AStart, AFinish);
end;

procedure TdxRichEditTextController.SetText(const AValue: string);
begin
  FGuid := dxGenerateGUID;
end;

function TdxRichEditTextController.GetSentenceStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
var
  AResult: TdxDocumentModelPosition;
  ACalculator: TdxSentencePositionCalculator;
begin
  AResult := GetModelPosition(TdxDocumentPosition(APos));
  ACalculator := GetSentencePositionCalculator(AResult.PieceTable);
  try
    AResult := ACalculator.GetSentenceStartPosition(AResult);
    Result := CreateDocumentPosition(AResult);
  finally
    ACalculator.Free;
  end;
end;

function TdxRichEditTextController.GetSentenceFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
var
  AResult: TdxDocumentModelPosition;
  ACalculator: TdxSentencePositionCalculator;
begin
  AResult := GetModelPosition(TdxDocumentPosition(APos));
  ACalculator := GetSentencePositionCalculator(AResult.PieceTable);
  try
    AResult := ACalculator.GetSentenceEndPosition(AResult);
    Result := CreateDocumentPosition(AResult);
  finally
    ACalculator.Free;
  end;
end;

function TdxRichEditTextController.CanDoNextStep(const APosition: IdxSpellCheckerPosition): Boolean;
var
  APieceTable: TdxCustomPieceTable;
begin
  if APosition <> nil then
  begin
    APieceTable := TdxDocumentPosition(APosition).Position.PieceTable;
    Exit(TdxDocumentPosition(APosition).Position.LogPosition < APieceTable.DocumentEndLogPosition);
  end
  else
    Exit(True);
end;

function TdxRichEditTextController.DeleteWord(const AStart, AFinish: IdxSpellCheckerPosition): Boolean;
var
  AStartPosition, AFinishPosition: TdxDocumentModelPosition;
  APieceTable: TdxPieceTable;
begin
  AStartPosition := TdxDocumentPosition(AStart).Position;
  AFinishPosition := TdxDocumentPosition(AFinish).Position;
  PreparePositionsForDelete(AStartPosition, AFinishPosition);
  APieceTable := TdxPieceTable(AStartPosition.PieceTable);
  if not DeleteWordCore(APieceTable, AStartPosition.LogPosition, AFinishPosition.LogPosition) then
    Exit(False);
  TdxDocumentPosition(AStart).InvalidatePosition;
  TdxDocumentPosition(AFinish).InvalidatePosition;
  Result := True;
end;

function TdxRichEditTextController.GetNextPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
var
  APosition: TdxDocumentModelPosition;
  AIterator: TdxSpellCheckerWordIterator;
begin
  APosition := GetModelPosition(TdxDocumentPosition(APos));
  AIterator := GetWordIterator(APosition.PieceTable);
  Result := CreateDocumentPosition(AIterator.MoveForward(APosition));
end;

function TdxRichEditTextController.GetNextWord(var AStart, AFinish: IdxSpellCheckerPosition): Boolean;
var
  APosStart, APosFinish: IdxSpellCheckerPosition;
  ASpellingStart, ASpellingFinish: IdxSpellCheckerPosition;
begin
  if not CanCheckDocument then
    Exit(False);

  if (FSpellingStart <> nil) and (AStart.Compare(FSpellingStart) < 0) then
    ASpellingStart := FSpellingStart
  else
    ASpellingStart := AStart;

  if (FSpellingFinish <> nil) and (AFinish.Compare(FSpellingFinish) > 0) then
    ASpellingFinish := FSpellingFinish
  else
    ASpellingFinish := AFinish;

  APosFinish := GetNextPosition(ASpellingStart);
  APosFinish := GetWordFinishPosition(APosFinish);
  if APosFinish.Compare(ASpellingFinish) > 0 then
    Exit(False);
  APosStart := GetWordStartPosition(APosFinish);
  if APosStart.Compare(ASpellingStart) < 0 then
    Exit(False);
  Result := (APosFinish.Compare(APosStart) > 0) and IsPositionVisible(APosFinish) and
    IsPositionVisible(APosStart);
  if Result then
  begin
    AStart := APosStart;
    AFinish := APosFinish;
  end;
end;

function TdxRichEditTextController.GetPrevPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
var
  APosition: TdxDocumentModelPosition;
  AIterator: TdxSpellCheckerWordIterator;
begin
  APosition := GetModelPosition(TdxDocumentPosition(APos));
  AIterator := GetWordIterator(APosition.PieceTable);
  try
    Result := CreateDocumentPosition(AIterator.MoveBack(APosition));
  finally
    AIterator.Free;
  end;
end;

function TdxRichEditTextController.GetPreviousWord(const APos: IdxSpellCheckerPosition): string;
var
  APrevWordEndPosition, APrevWordStartPosition: IdxSpellCheckerPosition;
begin
  APrevWordEndPosition := GetPrevPosition(APos);
  if APrevWordEndPosition = APos then
    Exit('');
  APrevWordStartPosition := GetWordStartPosition(APrevWordEndPosition);
  Result := GetWord(APrevWordStartPosition, APrevWordEndPosition);
end;

function TdxRichEditTextController.GetTextLength(const AText: string): IdxSpellCheckerPosition;
var
  APieceTable: TdxPieceTable;
begin
  if IsFinishPositionRequest(AText) then
  begin
    APieceTable := PieceTable;
    Exit(TdxDocumentPosition.Create(TdxDocumentModelPosition.FromRunStart(APieceTable, APieceTable.Runs.Count - 1)));
  end;
  Result := TdxPositionOffset.Create(Length(AText));
end;

function TdxRichEditTextController.IsFinishPositionRequest(const AText: string): Boolean;
begin
  Result := AText = Text;
end;

function TdxRichEditTextController.GetWord(const AStart, AFinish: IdxSpellCheckerPosition): string;
var
  APieceTable: TdxPieceTable;
begin
  if (not TdxDocumentPosition(AStart).IsValid) or (not TdxDocumentPosition(AFinish).IsValid) then
    Exit('');
  if TdxDocumentPosition(AStart).Compare(AFinish) = 0 then
    Exit('');
  APieceTable := TdxPieceTable(TdxDocumentPosition(AStart).Position.PieceTable);
  Result := GetVisibleText(APieceTable, TdxDocumentPosition(AStart).Position, TdxDocumentPosition(AFinish).Position);
 end;

function TdxRichEditTextController.GetWordFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
var
  APosition: TdxDocumentModelPosition;
  AIterator: TdxSpellCheckerWordIterator;
begin
  APosition := GetModelPosition(TdxDocumentPosition(APos));
  AIterator := GetWordIterator(APosition.PieceTable);
  Result := CreateDocumentPosition(AIterator.MoveToWordEnd(APosition));
end;

function TdxRichEditTextController.GetWordIterator(APieceTable: TdxCustomPieceTable): TdxSpellCheckerWordIterator;
begin
  raise EdxRichEditArgumentException.Create('WordIterator is unavailable for base class');
end;

function TdxRichEditTextController.GetWordStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
var
  APosition: TdxDocumentModelPosition;
  AIterator: TdxSpellCheckerWordIterator;
begin
  APosition := GetModelPosition(TdxDocumentPosition(APos));
  AIterator := GetWordIterator(APosition.PieceTable);
  Result := CreateDocumentPosition(AIterator.MoveToWordStart(APosition));
end;

function TdxRichEditTextController.HasLetters(const AStart, AFinish: IdxSpellCheckerPosition): Boolean;
var
  AWord: string;
  ACharsCount, I: Integer;
begin
  if AStart.Compare(AFinish) >= 0 then
    Exit(False);
  AWord := GetWord(AStart, AFinish);
  if AWord = '' then
    Exit(False);
  ACharsCount := Length(AWord);
  for I := 0 to ACharsCount - 1 do
  begin
    if {$IFDEF DELPHIXE4}AWord[I].IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(AWord[I]){$ENDIF} then
      Exit(True);
  end;
  Result := False;
end;

procedure TdxRichEditTextController.PreparePositionsForDelete(const AStart, AFinish: TdxDocumentModelPosition);
var
  AIterator: TdxVisibleCharactersIterator;
begin
  Assert(AStart.PieceTable = AFinish.PieceTable);
  AIterator := TdxVisibleCharactersIterator.Create(TdxPieceTable(AStart.PieceTable));
  try
    if not TryShiftStartPosition(AStart, AIterator) then
      TryShiftEndPosition(AFinish, AIterator);
  finally
    AIterator.Free;
  end;
end;

function TdxRichEditTextController.ReplaceWord(const AStart, AFinish: IdxSpellCheckerPosition; const AWord: string): Boolean;
var
  AOldWord: string;
  APieceTable: TdxPieceTable;
begin
  AOldWord := GetWord(AStart, AFinish);
  if AOldWord = AWord then
    Exit(False);

  APieceTable := TdxPieceTable(TdxDocumentPosition(AStart).Position.PieceTable);
  if not ReplaceWordCore(APieceTable, AWord, TdxDocumentPosition(AStart).Position.LogPosition, TdxDocumentPosition(AFinish).Position.LogPosition) then
    Exit(False);

  TdxDocumentPosition(AStart).InvalidatePosition;
  TdxDocumentPosition(AFinish).InvalidatePosition;
  Result := True;
end;

procedure TdxRichEditTextController.SetSpellingArea(const AStart, AFinish: IdxSpellCheckerPosition);
begin
  FSpellingStart := AStart;
  FSpellingFinish := AFinish;
end;

function TdxRichEditTextController.ShouldCheckWord(const AStart, AEnd: IdxSpellCheckerPosition): Boolean;
begin
  Result := True;
end;

{ TdxCheckAsYouTypeTextController }

constructor TdxCheckAsYouTypeTextController.Create(const AControl: IdxInnerControl);
begin
  inherited Create(AControl);
  SetPieceTable(AControl.DocumentModel.ActivePieceTable);
end;

destructor TdxCheckAsYouTypeTextController.Destroy;
begin
  FreeAndNil(FWordIterator);
  inherited Destroy;
end;

function TdxCheckAsYouTypeTextController.CanCheckDocument: Boolean;
begin
  Result := Control.IsEditable and (TdxSpellCheckerInstance.ISpellChecker3 <> nil) and
    (TdxSpellCheckerInstance.ISpellChecker3.CheckAsYouTypeOptions.Active);
end;

function TdxCheckAsYouTypeTextController.GetPieceTable: TdxPieceTable;
begin
  Result := FPieceTable;
end;

procedure TdxCheckAsYouTypeTextController.SetPieceTable(APieceTable: TdxPieceTable);
begin
  FPieceTable := APieceTable;
  Reset;
end;

function TdxCheckAsYouTypeTextController.GetWordIterator(APieceTable: TdxCustomPieceTable): TdxSpellCheckerWordIterator;
begin
  if (FWordIterator = nil) or (FWordIterator.PieceTable <> PieceTable) then
  begin
    FWordIterator.Free;
    FWordIterator := TdxSpellCheckerWordIterator.Create(PieceTable);
  end;
  Result := FWordIterator;
end;

procedure TdxCheckAsYouTypeTextController.Reset;
begin
  FreeAndNil(FWordIterator);
end;

procedure TdxCheckAsYouTypeTextController.ResetSpellCheck(AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex);
begin
  if FWordIterator = nil then
    Exit;
  if (FWordIterator.CachedRunIndex >= AStartRunIndex) and (FWordIterator.CachedRunIndex <= AEndRunIndex) then
    Reset;
end;

{ TdxSpellCheckerController }

constructor TdxSpellCheckerController.Create(const AControl: IdxInnerControl);
begin
  inherited Create(AControl);
  FSpellChecker := TdxSpellCheckerInstance.ISpellChecker3;
  FIgnoreListManager := TdxIgnoreListManager.Create(DocumentModel);
  FTextController := TdxCheckAsYouTypeTextController.Create(AControl);

  SpellChecker.UnregisterIgnoreList(AsControl);
  SpellChecker.RegisterIgnoreList(AsControl, FIgnoreListManager);
  SubscribeToEvents;
end;

destructor TdxSpellCheckerController.Destroy;
begin
  if SpellChecker <> nil then
  begin
    UnsubscribeToEvents;
    FSpellChecker.UnregisterIgnoreList(AsControl);
    FSpellChecker := nil;
  end;
  FreeAndNil(FIgnoreListManager);
  FreeAndNil(FTextController);
  inherited Destroy;
end;

function TdxSpellCheckerController.GetAsControl: TWinControl;
begin
  Result := TdxInnerRichEditControl(Control).Owner.Control;
end;


procedure TdxSpellCheckerController.SubscribeToEvents;
begin
  if SpellChecker <> nil then
    SubscribeSpellCheckerEvents;
end;

procedure TdxSpellCheckerController.UnsubscribeToEvents;
begin
  if SpellChecker <> nil then
    UnsubscribeSpellCheckerEvents;
end;

procedure TdxSpellCheckerController.SubscribeSpellCheckerEvents;
begin
  SpellChecker.AddSpellingOptionsChangedHandler(OnOptionsChanged);
  SpellChecker.CheckAsYouTypeOptions.AddChangedHandler(OnOptionsChanged);
  SpellChecker.AddDialogCheckModeChangeHandler(OnSpellCheckModeChanged);
end;

procedure TdxSpellCheckerController.UnsubscribeSpellCheckerEvents;
begin
  SpellChecker.RemoveSpellingOptionsChangedHandler(OnOptionsChanged);
  SpellChecker.CheckAsYouTypeOptions.RemoveChangedHandler(OnOptionsChanged);
  SpellChecker.RemoveDialogCheckModeChangeHandler(OnSpellCheckModeChanged);
end;

procedure TdxSpellCheckerController.OnSpellCheckModeChanged(ASender: TObject);
begin
  DocumentModel.ResetSpellCheck(0, MaxInt, True);
end;

procedure TdxSpellCheckerController.OnSpellCheckerCultureChanged(ASender: TObject);
begin
  ResetSpellCheck(0);
end;

procedure TdxSpellCheckerController.OnOptionsChanged(ASender: TObject);
begin
  ResetSpellCheck(0);
end;

procedure TdxSpellCheckerController.OnCustomDictionaryChanged(ASender: TObject);
begin
  ResetSpellCheck(0);
end;

procedure TdxSpellCheckerController.ResetSpellCheck(AStartRunIndex: TdxRunIndex);
begin
  ResetSpellCheck(AStartRunIndex, MaxInt);
end;

procedure TdxSpellCheckerController.ResetSpellCheck(AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex);
begin
  if (SpellChecker = nil) or not SpellChecker.IsDialogCheckMode then
    DocumentModel.ResetSpellCheck(AStartRunIndex, AEndRunIndex);
  FTextController.ResetSpellCheck(AStartRunIndex, AEndRunIndex);
end;

procedure TdxSpellCheckerController.ResetCore;
begin
  TextController.Reset;
end;

procedure TdxSpellCheckerController.Reset;
begin
  inherited Reset;
  ResetCore;
  ResetSpellCheck(0);
end;

procedure TdxSpellCheckerController.CheckContent(const AStart, AEnd: TdxDocumentModelPosition; APageIndex: Integer);
var
  AWordIterator: TdxSpellCheckerWordIterator;
  AIntervalStart, AIntervalEnd, AWordEnd: TdxDocumentModelPosition;
  APos: TdxDocumentModelPosition;
begin
  if PieceTable.SpellCheckerManager.MisspelledIntervals.Count > LowerLimit then
    Exit;

  AWordIterator := TdxSpellCheckerWordIterator.Create(PieceTable);
  try
    if AStart.LogPosition = PieceTable.DocumentStartLogPosition then
      AIntervalStart := AStart
    else
      AIntervalStart := AWordIterator.MoveToWordStart(AStart, False);
    if AEnd.LogPosition = PieceTable.DocumentEndLogPosition then
      AIntervalEnd := AEnd
    else
    begin
      AWordEnd := AWordIterator.MoveToWordEnd(AEnd);
      APos := TdxDocumentModelPosition.MoveForward(AEnd);
      if AWordEnd > APos then
        AIntervalEnd := AWordEnd
      else
        AIntervalEnd := APos;
    end;
  finally
    AWordIterator.Free;
  end;

  if CheckContentCore(AIntervalStart, AIntervalEnd) then
    CalculateErrorBoxes(AStart, AEnd, APageIndex)
  else
    ClearErrorBoxes(0, AEnd.LogPosition);
end;

function TdxSpellCheckerController.CheckContentCore(const AStart, AEnd: TdxDocumentModelPosition): Boolean;
var
  AIntervals: TArray<TdxSpellCheckerInterval>;
  ACount, I: Integer;
  AInterval: TdxSpellCheckerInterval;
begin
  AIntervals := PieceTable.SpellCheckerManager.PopUncheckedIntervals(AStart, AEnd);
  ACount := Length(AIntervals);
  if ACount = 0 then
    Exit(True);
  try
    FTextController.Reset;
    for I := 0 to ACount - 1 do
    begin
      AInterval := AIntervals[I];
      PieceTable.SpellCheckerManager.RemoveMisspelledIntervals(AInterval.Start, AInterval.&End);
      if not CheckInterval(AInterval) then
        Exit(False);
    end;
    Result := True;
  finally
    for I := 0 to ACount - 1 do
      AIntervals[I].Free;
  end;
end;

procedure TdxSpellCheckerController.ClearErrorBoxes(AFrom: TdxDocumentLogPosition; ATo: TdxDocumentLogPosition);
var
  ACalculator: TdxSpellCheckerErrorBoxCalculator;
begin
  ACalculator := CreateErrorBoxCalculator;
  try
    ACalculator.ClearErrorBoxes(AFrom, ATo);
  finally
    ACalculator.Free;
  end;
end;

procedure TdxSpellCheckerController.CalculateErrorBoxes(const AStart, AEnd: TdxDocumentModelPosition; APageIndex: Integer);
var
  AIntervals: TArray<TdxMisspelledInterval>;
  ACalculator: TdxSpellCheckerErrorBoxCalculator;
  ACount, AIndex: Integer;
  AInterval: TdxRunInfo;
  ABoxStartPos, ABoxEndPos, APos: TdxDocumentModelPosition;
begin
  AIntervals := PieceTable.SpellCheckerManager.MisspelledIntervals.GetIntervals(AStart.LogPosition, AEnd.LogPosition);
  ACount := Length(AIntervals);
  if ACount = 0 then
    Exit;

  ACalculator := CreateErrorBoxCalculator;
  try
    for AIndex := 0 to ACount - 1 do
    begin
      AInterval := AIntervals[AIndex].Interval;
      Assert(AInterval.Start < AInterval.&End);
      if AStart > AInterval.Start then
        ABoxStartPos := AStart
      else
        ABoxStartPos := AInterval.Start;
      APos := TdxDocumentModelPosition.MoveBackward(AInterval.&End);
      if AEnd < APos then
        ABoxEndPos := AEnd
      else
        ABoxEndPos := APos;
      ACalculator.Calculate(APageIndex, ABoxStartPos, ABoxEndPos, AIntervals[AIndex].ErrorType);
    end;
  finally
    ACalculator.Free;
  end;
end;

function TdxSpellCheckerController.CheckInterval(AInterval: TdxSpellCheckerInterval): Boolean;
var
  AStart: IdxSpellCheckerPosition;
  AEnd: IdxSpellCheckerPosition;
begin
  AStart := TdxDocumentPosition.Create(AInterval.Interval.Start);
  AEnd := TdxDocumentPosition.Create(AInterval.Interval.&End);
  Result := SpellChecker.CheckAsync(
    AsControl, TextController,
    AStart, AEnd,
    function (AErrorInfo: IdxSpellingErrorInfo): Boolean
    var
      AStart, AEnd: TdxDocumentModelPosition;
    begin
      AStart := TdxDocumentPosition(AErrorInfo.WordStartPosition).Position;
      AEnd := TdxDocumentPosition(AErrorInfo.WordEndPosition).Position;
      if PieceTable.CanEditRange(AStart.LogPosition, AEnd.LogPosition) then
        CalculateMisspelledInterval(
          AStart, AEnd,
          AErrorInfo.Word, AErrorInfo.Error);
      Result := not IsUpperLimitExceeded;
    end);
end;

function TdxSpellCheckerController.IsUpperLimitExceeded: Boolean;
begin
  Result := PieceTable.SpellCheckerManager.MisspelledIntervals.Count > UpperLimit;
end;

procedure TdxSpellCheckerController.CalculateMisspelledInterval(const AStart, AEnd: TdxDocumentModelPosition; const AWord: string; AError: TdxSpellingError);
begin
  if not IgnoreListManager.IgnoredList.Contains(AStart.LogPosition, AEnd.LogPosition, AWord) then
    PieceTable.SpellCheckerManager.CreateMisspelledInterval(AStart, AEnd, AError);
end;

function TdxSpellCheckerController.CanCheckDocument: Boolean;
begin
  Result := inherited CanCheckDocument and not SpellChecker.IsDialogCheckMode;
end;

procedure TdxSpellCheckerController.OnPieceTableChanged;
begin
  inherited OnPieceTableChanged;
  TextController.SetPieceTable(PieceTable);
end;

{ TdxEmptySpellCheckerController }

constructor TdxEmptySpellCheckerController.Create;
begin
  inherited Create;
end;

procedure TdxEmptySpellCheckerController.SetPieceTable(const AValue: TdxPieceTable);
begin
end;

procedure TdxEmptySpellCheckerController.Reset;
begin
end;

procedure TdxEmptySpellCheckerController.CheckContent(const AStart, AEnd: TdxDocumentModelPosition; APageIndex: Integer);
begin
end;

procedure TdxEmptySpellCheckerController.CheckPages(AFrom: Integer);
begin
end;

end.
