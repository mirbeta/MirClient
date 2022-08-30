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

unit dxRichEdit.DocumentModel.Boxes.Simple;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Contnrs,
  dxCore, dxCoreClasses, dxCoreGraphics, dxSpellCheckerCore,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.TextColors,
  dxGenerics,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Paragraphs,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.InlineObjectFormatting;

type
  TdxBox = class;
  TdxSimpleRow = class;
  TdxTextBox = class;
  TdxSpecialTextBox = class;
  TdxHyphenBox = class;
  TdxInlinePictureBox = class;
  TdxLineBreakBox = class;
  TdxParagraphMarkBox = class;
  TdxUnderlineBox = class;
  TdxErrorBox = class;
  TdxSeparatorBox = class;
  TdxCharacterBox = class;
  TdxBoxCollection = class;
  TdxBoxHitTestCustomManager = class;
  TdxBoxMeasurer = class;
  TdxVisitableDocumentIntervalBoxCollection = class;
  TdxCharacterBoxCollection = class;

  { IdxSimpleDocumentLayoutExporter }

  IdxSimpleDocumentLayoutExporter = interface
    procedure ExportSimpleRow(ARow: TdxSimpleRow);
    procedure ExportTextBox(ABox: TdxTextBox);
    procedure ExportSpecialTextBox(ABox: TdxSpecialTextBox);
    procedure ExportHyphenBox(ABox: TdxHyphenBox);
    procedure ExportInlinePictureBox(ABox: TdxInlinePictureBox);
    procedure ExportSpaceBox(ABox: TdxBox);
    procedure ExportLineBreakBox(ABox: TdxLineBreakBox);
    procedure ExportParagraphMarkBox(ABox: TdxParagraphMarkBox);
    procedure ExportUnderlineBox(ARow: TdxSimpleRow; AUnderlineBox: TdxUnderlineBox);
    procedure ExportStrikeoutBox(ARow: TdxSimpleRow; AStrikeoutBox: TdxUnderlineBox);
    procedure ExportErrorBox(AErrorBox: TdxErrorBox);
    procedure ExportSeparatorBox(ABox: TdxSeparatorBox);
  end;

  { IdxBoxMeasurerProvider }

  IdxBoxMeasurerProvider = interface
  ['{A3A85BFF-3408-453D-BB4E-FDC4A2219859}']
    function GetMeasurer: TdxBoxMeasurer;

    property Measurer: TdxBoxMeasurer read GetMeasurer;
  end;

  { IdxSpaceBox }

  IdxSpaceBox = interface
  ['{B445AC14-0F24-428D-A05E-B60C763617E9}']
    function GetBox: TdxBox;
    function GetMinWidth: Integer;
    procedure SetMinWidth(const Value: Integer);

    property Box: TdxBox read GetBox;
    property MinWidth: Integer read GetMinWidth write SetMinWidth;
  end;

  { IdxBoxHitTestCustomCalculator }

  IdxBoxHitTestCustomCalculator = interface
  ['{8F910E69-A9F9-4106-B398-322ED3398F63}']
    function CreateCharacterBoxHitTestManager(ABox: TdxCharacterBox): TdxBoxHitTestCustomManager;
    function CreateHyphenBoxHitTestManager(ABox: TdxHyphenBox): TdxBoxHitTestCustomManager;
    function CreateRowHitTestManager(ARow: TdxSimpleRow): TdxBoxHitTestCustomManager;
    function CreateTextBoxHitTestManager(ABox: TdxTextBox): TdxBoxHitTestCustomManager;
    function CreateInlinePictureBoxHitTestManager(ABox: TdxInlinePictureBox): TdxBoxHitTestCustomManager;
    function CreateSeparatorMarkBoxHitTestManager(ABox: TdxSeparatorBox): TdxBoxHitTestCustomManager;
    function CreateSpaceBoxHitTestManager(const ABox: IdxSpaceBox): TdxBoxHitTestCustomManager;
    function CreateLineBreakBoxHitTestManager(ABox: TdxLineBreakBox): TdxBoxHitTestCustomManager;
    function CreateParagraphMarkBoxHitTestManager(ABox: TdxParagraphMarkBox): TdxBoxHitTestCustomManager;
//    procedure ProcessRowCollection(ACollection: TdxRowCollection);
    procedure ProcessBoxCollection(ACollection: TdxBoxCollection);
    procedure ProcessCharacterBoxCollection(ACollection: TdxCharacterBoxCollection);
  end;

  { TdxBoxHitTestCustomCalculator }

  TdxBoxHitTestCustomCalculator = class abstract(TcxIUnknownObject, IdxBoxHitTestCustomCalculator)
  protected
    procedure RegisterBoxHitTest(ABox: TdxBox); virtual; abstract;
    procedure RegisterCharacterHitTest(ABox: TdxCharacterBox); virtual; abstract;
  public
  {$REGION 'IdxBoxHitTestCustomCalculator'}
    function CreateCharacterBoxHitTestManager(ABox: TdxCharacterBox): TdxBoxHitTestCustomManager; virtual; abstract;
    function CreateHyphenBoxHitTestManager(ABox: TdxHyphenBox): TdxBoxHitTestCustomManager; virtual; abstract;
    function CreateRowHitTestManager(ARow: TdxSimpleRow): TdxBoxHitTestCustomManager; virtual; abstract;
    function CreateTextBoxHitTestManager(ABox: TdxTextBox): TdxBoxHitTestCustomManager; virtual; abstract;
    function CreateInlinePictureBoxHitTestManager(ABox: TdxInlinePictureBox): TdxBoxHitTestCustomManager; virtual; abstract;
    function CreateSeparatorMarkBoxHitTestManager(ABox: TdxSeparatorBox): TdxBoxHitTestCustomManager; virtual; abstract;
    function CreateSpaceBoxHitTestManager(const ABox: IdxSpaceBox): TdxBoxHitTestCustomManager; virtual; abstract;
    function CreateLineBreakBoxHitTestManager(ABox: TdxLineBreakBox): TdxBoxHitTestCustomManager; virtual; abstract;
    function CreateParagraphMarkBoxHitTestManager(ABox: TdxParagraphMarkBox): TdxBoxHitTestCustomManager; virtual; abstract;
//    procedure ProcessRowCollection(ACollection: TdxRowCollection);
    procedure ProcessBoxCollection(ACollection: TdxBoxCollection); virtual; abstract;
    procedure ProcessCharacterBoxCollection(ACollection: TdxCharacterBoxCollection); virtual; abstract;
  {$ENDREGION}
  end;

  { TdxBoxHitTestCustomManager }

  TdxBoxHitTestCustomManager = class abstract
  strict private
    FCalculator: TdxBoxHitTestCustomCalculator;
    FBox: TdxBoxBase;
  public
    constructor Create(ACalculator: TdxBoxHitTestCustomCalculator; ABox: TdxBoxBase);

    property Calculator: TdxBoxHitTestCustomCalculator read FCalculator;
    property Box: TdxBoxBase read FBox;
  end;

  { TdxBox }

  TdxBox = class(TdxBoxBase)
  protected
    function GetDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function GetHitTestAccuracy: TdxHitTestAccuracy; override;
    function IsColumnBreakBox: Boolean; virtual;
    function IsPageBreakBox: Boolean; virtual;
    function IsSectionMarkBox: Boolean; virtual;
    function IsTabSpaceBox: Boolean; virtual;
  public
    class function CreateBox: TdxBox; virtual;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); virtual; abstract;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; virtual; abstract;
    function GetText(ATable: TdxCustomPieceTable): string; virtual; abstract;

    procedure OffsetRunIndices(ADelta: Integer); virtual; abstract;

    function CalcDescent(APieceTable: TdxCustomPieceTable): Integer; virtual;
    function CalcAscentAndFree(APieceTable: TdxCustomPieceTable): Integer; virtual;
    function CalcBaseDescent(APieceTable: TdxCustomPieceTable): Integer; virtual;
    function CalcBaseAscentAndFree(APieceTable: TdxCustomPieceTable): Integer; virtual;
    function GetDocumentPosition(APieceTable: TdxCustomPieceTable; const APos: TdxFormatterPosition): TdxDocumentModelPosition; virtual;
    function GetUnderlineColorCore(APieceTable: TdxCustomPieceTable): TdxAlphaColor; virtual;
    function GetStrikeoutColorCore(APieceTable: TdxCustomPieceTable): TdxAlphaColor; virtual;
    function GetFontUnderlineType(APieceTable: TdxCustomPieceTable): TdxUnderlineType; virtual;
    function GetFontStrikeoutType(APieceTable: TdxCustomPieceTable): TdxStrikeoutType; virtual;
    function GetActualForeColor(APieceTable: TdxCustomPieceTable; ATextColors: TdxTextColors; ABackColor: TdxAlphaColor): TdxAlphaColor; virtual;
    function GetActualUnderlineColor(APieceTable: TdxCustomPieceTable; ATextColors: TdxTextColors; ABackColor: TdxAlphaColor): TdxAlphaColor; virtual;
    function GetActualStrikeoutColor(APieceTable: TdxCustomPieceTable; ATextColors: TdxTextColors; ABackColor: TdxAlphaColor): TdxAlphaColor; virtual;
    function IsSpaceBox: Boolean; override;
    function UseAscentAndDescentFromFontInfo: Boolean; virtual;
  end;
  TdxBoxClass = class of TdxBox;

  { TdxBoxList }

  TdxBoxList = class(TdxBoxListBase)
  strict private
    function GetItem(Index: Integer): TdxBox;
  public
    property Items[Index: Integer]: TdxBox read GetItem;
  end;

  { TdxCustomBoxCollection }

  TdxCustomBoxCollection = class(TdxBoxList)
  public
    function BinarySearchBoxIndex(const APos: TdxFormatterPosition): Integer; overload;
    function BinarySearchBoxIndex(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition): Integer; overload;
    procedure RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox); virtual; abstract;
    procedure RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator); virtual; abstract;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
    procedure MoveVertically(ADeltaY: Integer);
  end;

  TdxBoxCollectionBase<T: TdxBox> = class(TdxCustomBoxCollection)
  private
    function GetItem(Index: Integer): T;
  public
    function First: T;
    function Last: T;
    property Items[Index: Integer]: T read GetItem; default;
  end;

  { TdxHiddenTextUnderlineBox }

  TdxHiddenTextUnderlineBox = class
  strict private
    FStart: Integer;
    FEnd: Integer;
    FBottomOffset: Integer;
  public
     constructor Create(AStart, AEnd, ABottom: Integer);

    property Start: Integer read FStart;
    property &End: Integer read FEnd;
    property BottomOffset: Integer read FBottomOffset;
  end;
  TdxHiddenTextUnderlineBoxCollection = class(TdxObjectList<TdxHiddenTextUnderlineBox>);

  { TdxSinglePositionBox }

  TdxSinglePositionBox = class(TdxBox)
  strict private
    FPos: TdxFormatterPosition;
  protected
    function GetEndPos: TdxFormatterPosition; override; final;
    function GetStartPos: TdxFormatterPosition; override; final;
    procedure SetEndPos(const Value: TdxFormatterPosition); override; final;
    procedure SetStartPos(const Value: TdxFormatterPosition); override; final;
  public
    function GetFirstFormatterPosition: TdxFormatterPosition; override; final;
    function GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override; final;
    function GetLastFormatterPosition: TdxFormatterPosition; override; final;
    function GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override; final;
    function GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase; override;
    function GetText(ATable: TdxCustomPieceTable): string; override; final;
    procedure OffsetRunIndices(ADelta: Integer); override; final;
  end;

  { TdxInlinePictureBox }

  TdxInlinePictureBox = class(TdxSinglePositionBox)
  protected
    function GetImageCore(ARun: TdxRunBase; AReadOnly: Boolean): TdxOfficeImage; virtual;
  public
    class function CreateBox: TdxBox; override;

    procedure ExportHotZones(APainter: TObject); virtual;
    function IsInlinePicture: Boolean; override;
    function IsLineBreak: Boolean; override;
    function IsVisible: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;

    function CalcDescent(APieceTable: TdxCustomPieceTable): Integer; override;
    function CalcAscentAndFree(APieceTable: TdxCustomPieceTable): Integer; override;
    function CalcBaseAscentAndFree(APieceTable: TdxCustomPieceTable): Integer; override;
    function CalcBaseDescent(APieceTable: TdxCustomPieceTable): Integer; override;

    function GetImage(APieceTable: TdxCustomPieceTable; AReadOnly: Boolean): TdxOfficeImage;
    function GetImageActualSizeInLayoutUnits(APieceTable: TdxCustomPieceTable): TSize;
    function GetSizing(APieceTable: TdxCustomPieceTable): TdxImageSizeMode;
    function GetResizingShadowDisplayMode(APieceTable: TdxCustomPieceTable): TdxResizingShadowDisplayMode;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
  end;

  { TdxNoPositionBox }

  TdxNoPositionBox = class(TdxBox)
  strict private
    class var FPos: TdxFormatterPosition;
  protected
    function GetEndPos: TdxFormatterPosition; override; final;
    function GetStartPos: TdxFormatterPosition; override; final;
    procedure SetEndPos(const Value: TdxFormatterPosition); override; final;
    procedure SetStartPos(const Value: TdxFormatterPosition); override; final;
  public
    procedure OffsetRunIndices(ADelta: Integer); override; final;
  end;

  { TdxMultiPositionBox }

  TdxMultiPositionBox = class(TdxBox)
  protected
    FEndPos: TdxFormatterPosition;
    FStartPos: TdxFormatterPosition;
  protected
    function GetEndPos: TdxFormatterPosition; override; final;
    function GetStartPos: TdxFormatterPosition; override; final;
    procedure SetEndPos(const Value: TdxFormatterPosition); override; final;
    procedure SetStartPos(const Value: TdxFormatterPosition); override; final;
  public
    function GetFirstFormatterPosition: TdxFormatterPosition; override;
    function GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override;
    function GetLastFormatterPosition: TdxFormatterPosition; override;
    function GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override;
    function GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase; override;
    function GetText(ATable: TdxCustomPieceTable): string; override;
    procedure OffsetRunIndices(ADelta: Integer); override;
  end;

  { TdxUnderlineBox }

  TdxUnderlineBox = class(TdxReferencedObject)
  strict private
    FStartAnchorIndex: Integer;
    FBoxCount: Integer;
    FUnderlineThickness: Integer;
    FUnderlinePosition: Integer;
    FUnderlineBounds: TRect;
    FClipBounds: TRect;
    function GetEndAnchorIndex: Integer;
  public
    constructor Create; overload;
    constructor Create(AStartAnchorIndex: Integer); overload;
    constructor Create(AStartAnchorIndex, ABoxCount: Integer); overload;
    procedure MoveVertically(ADeltaY: Integer); virtual;

    property StartAnchorIndex: Integer read FStartAnchorIndex write FStartAnchorIndex;
    property EndAnchorIndex: Integer read GetEndAnchorIndex;
    property BoxCount: Integer read FBoxCount write FBoxCount;
    property UnderlineThickness: Integer read FUnderlineThickness write FUnderlineThickness;
    property UnderlinePosition: Integer read FUnderlinePosition write FUnderlinePosition;
    property UnderlineBounds: TRect read FUnderlineBounds write FUnderlineBounds;
    property ClipBounds: TRect read FClipBounds write FClipBounds;
  end;

  { TdxUnderlineBoxCollectionBase }

  TdxUnderlineBoxCollectionBase = class abstract(TdxReferencedObjectList<TdxUnderlineBox>)
  public
    procedure MoveVertically(ADeltaY: Integer);
  end;

  TdxUnderlineBoxCollection = class sealed(TdxUnderlineBoxCollectionBase);

  { TdxErrorBox }

  TdxErrorBox = class sealed(TdxUnderlineBox)
  strict private
    FFirstBoxOffset: Integer;
    FLastBoxOffset: Integer;
    FStartPos: TdxFormatterPosition;
    FEndPos: TdxFormatterPosition;
    FErrorType: TdxSpellingError;
  public
    constructor Create; overload;
    constructor Create(AStartAnchorIndex, ABoxCount, AFirstBoxOffset, ALastBoxOffset: Integer); overload;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); virtual;

    property ErrorType: TdxSpellingError read FErrorType write FErrorType;
    property FirstBoxOffset: Integer read FFirstBoxOffset write FFirstBoxOffset;
    property LastBoxOffset: Integer read FLastBoxOffset write FLastBoxOffset;
    property StartPos: TdxFormatterPosition read FStartPos write FStartPos;
    property EndPos: TdxFormatterPosition read FEndPos write FEndPos;
  end;

  { TdxErrorBoxCollection }

  TdxErrorBoxCollection = class sealed(TdxUnderlineBoxCollectionBase)
  strict private
    function GetItem(Index: Integer): TdxErrorBox;
  public
    property Items[Index: Integer]: TdxErrorBox read GetItem; default;
  end;

  { TdxTextBox }

  TdxTextBox = class(TdxMultiPositionBox)
  public
    class function CreateBox: TdxBox; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;
  end;

  { TdxSpecialTextBox }

  TdxSpecialTextBox = class(TdxTextBox)
  public
    class function CreateBox: TdxBox; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
  end;
  TdxSpecialTextBoxCollection = class(TdxReferencedObjectList<TdxSpecialTextBox>);

  { TdxSimpleRowExtendedBoxes }

  TdxSimpleRowExtendedBoxes = class
  strict private
    FUnderlines: TdxUnderlineBoxCollection;
    FStrikeouts: TdxUnderlineBoxCollection;
    FErrors: TdxErrorBoxCollection;
    FHighlightAreas: TdxHighlightAreaCollection;
    FHiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection;
    FSpecialTextBoxes: TdxSpecialTextBoxCollection;
    FBoxRanges: TdxRowBoxRangeCollection;
    function GetUnderlines: TdxUnderlineBoxCollection;
    function GetStrikeouts: TdxUnderlineBoxCollection;
    function GetErrors: TdxErrorBoxCollection;
    function GetHighlightAreas: TdxHighlightAreaCollection;
    function GetHiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection;
    function GetSpecialTextBoxes: TdxSpecialTextBoxCollection;
    function GetBoxRanges: TdxRowBoxRangeCollection;
  protected
    property InnerUnderlines: TdxUnderlineBoxCollection read FUnderlines;
    property InnerStrikeouts: TdxUnderlineBoxCollection read FStrikeouts;
    property InnerHighlightAreas: TdxHighlightAreaCollection read FHighlightAreas;
    property InnerHiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection read FHiddenTextBoxes;
    property InnerSpecialTextBoxes: TdxSpecialTextBoxCollection read FSpecialTextBoxes;
    property InnerBoxRanges: TdxRowBoxRangeCollection read FBoxRanges;
  public
    destructor Destroy; override;
    procedure ClearUnderlines;
    procedure ClearStrikeouts;
    procedure ClearErrors;
    procedure ClearHighlightAreas;
    procedure ClearHiddenTextBoxes;
    procedure ClearSpecialTextBoxes;
    procedure ClearBoxRanges;

    property BoxRanges: TdxRowBoxRangeCollection read GetBoxRanges;
    property Underlines: TdxUnderlineBoxCollection read GetUnderlines;
    property Strikeouts: TdxUnderlineBoxCollection read GetStrikeouts;
    property Errors: TdxErrorBoxCollection read GetErrors;
    property SpecialTextBoxes: TdxSpecialTextBoxCollection read GetSpecialTextBoxes;
    property HiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection read GetHiddenTextBoxes;
    property HighlightAreas: TdxHighlightAreaCollection read GetHighlightAreas;
  end;

  { TdxSimpleRow }

  TdxSimpleRow = class(TdxNoPositionBox)
  strict private
    FBoxes: TdxBoxCollection;
    FExtendedBoxes: TdxSimpleRowExtendedBoxes;
    FParagraph: TdxCustomParagraph;
    FSpaceBefore: Integer;
    FBaseLineOffset: Integer;
    FTextOffset: Integer;
    FLineNumber: Integer;
    FLastParagraphRowOriginalHeight: Integer;
    FProcessingFlags: TdxRowProcessingFlags;
    function GetInnerBoxRanges: TdxRowBoxRangeCollection;
    function GetInnerHiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection;
    function GetInnerHighlightAreas: TdxHighlightAreaCollection;
    function GetInnerSpecialTextBoxes: TdxSpecialTextBoxCollection;
    function GetInnerStrikeouts: TdxUnderlineBoxCollection;
    function GetInnerUnderlines: TdxUnderlineBoxCollection;
    function GetExtendedBoxes: TdxSimpleRowExtendedBoxes;
    function GetUnderlines: TdxUnderlineBoxCollection;
    function GetStrikeouts: TdxUnderlineBoxCollection;
    function GetErrors: TdxErrorBoxCollection;
    function GetHighlightAreas: TdxHighlightAreaCollection;
    function GetHiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection;
    function GetSpecialTextBoxes: TdxSpecialTextBoxCollection;
    function GetBoxRanges: TdxRowBoxRangeCollection;
    function GetHeight: Integer;
    function GetLastParagraphRow: Boolean;
    function GetShouldProcessCharacterLines: Boolean;
    function GetShouldProcessHiddenText: Boolean;
    function GetShouldProcessTextHighlight: Boolean;
    function GetShouldProcessLayoutDependentText: Boolean;
    function GetShouldProcessSpecialTextBoxes: Boolean;
    function GetContainsFootNotes: Boolean;
    function GetContainsEndNotes: Boolean;
    function GetIsFirstParagraphRow: Boolean;
    function GetIsLastParagraphRow: Boolean;
    procedure SetHeight(Value: Integer);
    procedure SetParagraph(Value: TdxCustomParagraph);
  protected
    function CreateExtendedBoxes: TdxSimpleRowExtendedBoxes; virtual;
    function GetDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function GetHitTestAccuracy: TdxHitTestAccuracy; override;
    function GetNumberingListBoxCore: TdxBox; virtual;

    property DetailsLevel: TdxDocumentLayoutDetailsLevel read GetDetailsLevel;
    property InnerExtendedBoxes: TdxSimpleRowExtendedBoxes read FExtendedBoxes;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearBoxRanges;
    procedure ClearUnderlines;
    procedure ClearStrikeouts;
    procedure ClearErrors;
    function GetFirstFormatterPosition: TdxFormatterPosition; override;
    function GetLastFormatterPosition: TdxFormatterPosition; override;
    function GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override;
    function GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override;
    class function CreateBox: TdxBox; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    function FindLastNonSpaceBoxIndex: Integer; overload;
    function FindLastNonSpaceBoxIndex(AStartIndex: Integer): Integer; overload;
    class function FindLastNonSpaceBoxIndex(ABoxes: TdxBoxCollection; AStartIndex: Integer): Integer; overload; static;
    procedure MoveVertically(ADeltaY: Integer); override;
    function GetText(ATable: TdxCustomPieceTable): string; override; final;
    function GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase; override;

    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsTabelCellRow: Boolean; virtual;
    function IsVisible: Boolean; override;
    procedure ClearBookmarkBoxes; virtual;
    procedure ClearRangePermissionBoxes; virtual;
    function GetBookmarkBoxesCore: TdxVisitableDocumentIntervalBoxCollection; virtual;
    function GetRangePermissionBoxesCore: TdxVisitableDocumentIntervalBoxCollection; virtual;

    procedure ClearHighlightAreas;
    procedure ClearHiddenTextBoxes;
    procedure ClearSpecialTextBoxes;
    property InnerBoxRanges: TdxRowBoxRangeCollection read GetInnerBoxRanges;

    property Boxes: TdxBoxCollection read FBoxes;
    property TextOffset: Integer read FTextOffset write FTextOffset;
    property SpacingBefore: Integer read FSpaceBefore write FSpaceBefore;
    property Underlines: TdxUnderlineBoxCollection read GetUnderlines;
    property Strikeouts: TdxUnderlineBoxCollection read GetStrikeouts;
    property Errors: TdxErrorBoxCollection read GetErrors;
    property HighlightAreas: TdxHighlightAreaCollection read GetHighlightAreas;
    property HiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection read GetHiddenTextBoxes;
    property SpecialTextBoxes: TdxSpecialTextBoxCollection read GetSpecialTextBoxes;
    property BoxRanges: TdxRowBoxRangeCollection read GetBoxRanges;
    property BaseLineOffset: Integer read FBaseLineOffset write FBaseLineOffset;
    property Paragraph: TdxCustomParagraph read FParagraph write SetParagraph;
    property Height: Integer read GetHeight write SetHeight;
    property LineNumber: Integer read FLineNumber write FLineNumber;
    property LastParagraphRow: Boolean read GetLastParagraphRow;
    property LastParagraphRowOriginalHeight: Integer read FLastParagraphRowOriginalHeight write FLastParagraphRowOriginalHeight;
    property ShouldProcessCharacterLines: Boolean read GetShouldProcessCharacterLines;
    property ShouldProcessHiddenText: Boolean read GetShouldProcessHiddenText;
    property ShouldProcessTextHighlight: Boolean read GetShouldProcessTextHighlight;
    property ShouldProcessLayoutDependentText: Boolean read GetShouldProcessLayoutDependentText;
    property ShouldProcessSpecialTextBoxes: Boolean read GetShouldProcessSpecialTextBoxes;
    property ContainsFootNotes: Boolean read GetContainsFootNotes;
    property ContainsEndNotes: Boolean read GetContainsEndNotes;
    property InnerUnderlines: TdxUnderlineBoxCollection read GetInnerUnderlines;
    property InnerStrikeouts: TdxUnderlineBoxCollection read GetInnerStrikeouts;
    property InnerHighlightAreas: TdxHighlightAreaCollection read GetInnerHighlightAreas;
    property IsFirstParagraphRow: Boolean read GetIsFirstParagraphRow;
    property IsLastParagraphRow: Boolean read GetIsLastParagraphRow;
    property ProcessingFlags: TdxRowProcessingFlags read FProcessingFlags write FProcessingFlags;
    //for internal use
    property ExtendedBoxes: TdxSimpleRowExtendedBoxes read GetExtendedBoxes;
    property InnerHiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection read GetInnerHiddenTextBoxes;
    property InnerSpecialTextBoxes: TdxSpecialTextBoxCollection read GetInnerSpecialTextBoxes;
  end;

  { TdxSingleCharacterMarkBox }

  TdxSingleCharacterMarkBox = class(TdxSinglePositionBox)
  protected
    function GetMarkCharacter: Char; virtual; abstract;
  public
    function GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo; override;

    property MarkCharacter: Char read GetMarkCharacter;
  end;

  { TdxLineBreakBox }

  TdxLineBreakBox = class(TdxSingleCharacterMarkBox)
  protected
    function GetMarkCharacter: Char; override;
  public
    class function CreateBox: TdxBox; override;

    function IsSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsLineBreak: Boolean; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
  end;

  { TdxSeparatorBox }

  TdxSeparatorBox = class(TdxSingleCharacterMarkBox)
  protected
    function GetMarkCharacter: Char; override;
  public
    class function CreateBox: TdxBox; override;
    function IsVisible: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsLineBreak: Boolean; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
  end;

  { TdxParagraphMarkBox }

  TdxParagraphMarkBox = class(TdxSingleCharacterMarkBox)
  protected
    function GetMarkCharacter: Char; override;
  public
    class function CreateBox: TdxBox; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    function IsLineBreak: Boolean; override;
    function IsSpaceBox: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;
  end;

  { TdxSingleSpaceBox }

  TdxSingleSpaceBox = class(TdxSingleCharacterMarkBox, IdxSpaceBox)
  strict private
    FMinWidth: Integer;
    function GetMinWidth: Integer;
    procedure SetMinWidth(const Value: Integer);
    function GetBox: TdxBox;
  protected
    function GetMarkCharacter: Char; override;
  public
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    class function CreateBox: TdxBox; override;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;

    property MinWidth: Integer read GetMinWidth write SetMinWidth;
    property Box: TdxBox read GetBox;
  end;

  { TdxCharacterBox }

  TdxCharacterBox = class(TdxMultiPositionBox)
  strict private
    FTightBounds: TRect;
  protected
    function GetDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function GetHitTestAccuracy: TdxHitTestAccuracy; override;
  public
    class function CreateBox: TdxBox; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    procedure MoveVertically(ADeltaY: Integer); override;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;

    function Clone: TdxCharacterBox;

    property TightBounds: TRect read FTightBounds write FTightBounds;
  end;

  { TdxCharacterBoxCollection }

  TdxCharacterBoxCollection = class(TdxBoxCollectionBase<TdxCharacterBox>)
  public
    procedure RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator); override;
    procedure RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox); override;
  end;

  { TdxCharacterMarkBox }

  TdxCharacterMarkBox = class(TdxMultiPositionBox)
  protected
    FTightBounds: TRect;
    function GetMarkCharacter: Char; virtual; abstract;

    property MarkCharacter: Char read GetMarkCharacter;
  public
    function GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo; override;
    function IsVisible: Boolean; override;
  end;

  { TdxSpaceBoxa }

  TdxSpaceBoxa = class(TdxCharacterMarkBox, IdxSpaceBox)
  strict private
    FMinWidth: Integer;
  protected
    function GetBox: TdxBox;
    function GetMarkCharacter: Char; override;
    function GetMinWidth: Integer;
    procedure SetMinWidth(const Value: Integer);

    property Box: TdxBox read GetBox;
  public
    class function CreateBox: TdxBox; override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;
    procedure MoveVertically(ADeltaY: Integer); override;

    property MinWidth: Integer read GetMinWidth write SetMinWidth;
    property TightBounds: TRect read FTightBounds write FTightBounds;
  end;

  { TdxHyphenBox }

  TdxHyphenBox = class sealed(TdxTextBox)
  public const
    HyphenString = '-';
  public
    class function CreateBox: TdxBox; override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function GetText(ATable: TdxCustomPieceTable): string; override;
  end;

  { TdxBoxCollection }

  TdxBoxCollection = class(TdxBoxCollectionBase<TdxBox>)
  public
    procedure RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox); override;
    procedure RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator); override;
    function BinarySearchBox(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition): TdxBox;
  end;

  { TdxSimpleParagraphBoxCollection }

  TdxSimpleParagraphBoxCollection = class
  strict private
    FBoxCollection: TdxBoxCollection;
    FParagraphStartRunIndex: TdxRunIndex;
    function GetItem(Index: Integer): TdxBox;
    procedure SetParagraphStartRunIndex(const Value: TdxRunIndex);
    function GetCount: Integer;
  protected
    function GetIsValid: Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ABox: TdxBox); virtual;
    procedure Clear; virtual;
    function ContainsFloatingObjectAnchorBox: Boolean; virtual;
    function GetBox(ABoxInfo: TdxBoxInfo): TdxBox;
    procedure InvalidateBoxes; virtual;
    procedure OffsetRunIndices(ADelta: Integer); virtual;

    property Items[Index: Integer]: TdxBox read GetItem; default;
    property InnerCollection: TdxBoxCollection read FBoxCollection;
    property ParagraphStartRunIndex: TdxRunIndex read FParagraphStartRunIndex write SetParagraphStartRunIndex;
    property IsValid: Boolean read GetIsValid;
    property Count: Integer read GetCount;
  end;

  { TdxParagraphAlignmentCalculatorBase }

  TdxParagraphAlignmentCalculatorBase = class
  strict private class var
    FLeftAlignmentCalculator: TdxParagraphAlignmentCalculatorBase;
    FRightAlignmentCalculator: TdxParagraphAlignmentCalculatorBase;
    FCenterAlignmentCalculator: TdxParagraphAlignmentCalculatorBase;
    FJustifyAlignmentCalculator: TdxParagraphAlignmentCalculatorBase;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    function GetRowContentRightBounds(const ARowBounds: TRect; ABoxes: TdxBoxCollection): Integer;
  protected
    procedure AlignBreakBox(const ABounds: TRect; ABoxes: TdxBoxCollection);
    procedure AlignRowCore(const ARowBounds: TRect; ATextOffset: Integer;
      ABoxes: TdxBoxCollection; AFrom, ATo: Integer; AContainsLayoutDependentBox: Boolean); virtual; abstract;
    class function AlignLeft(ABoxes: TdxBoxCollection; ALeft, AFrom, ATo: Integer): Integer; static;
    function GetSectionBoxLeft(const ABounds: TRect): Integer; virtual;
    class function FindLastVisibleBoxIndex(ABoxes: TdxBoxCollection; AFrom, ATo: Integer): Integer; static;
    class function IsBoxOutsideOfRow(const ARowBounds: TRect; AFrom: Integer; ABoxes: TdxBoxCollection; ABoxIndex: Integer): Boolean; static;
  public
    procedure AlignNumberingListBox(ANumberingListBox, AFirstRowBox: TdxBox);
    procedure AlignMultipartRow(const ARowBounds: TRect; ARow: TdxSimpleRow);
    procedure AlignRow(ARow: TdxSimpleRow); overload;
    procedure AlignRow(const ARowBounds: TRect; ARow: TdxSimpleRow); overload;
    class function GetAlignmentCalculator(AAlignment: TdxParagraphAlignment): TdxParagraphAlignmentCalculatorBase; static;

    class property LeftAlignmentCalculator: TdxParagraphAlignmentCalculatorBase read FLeftAlignmentCalculator;
    class property RightAlignmentCalculator: TdxParagraphAlignmentCalculatorBase read FRightAlignmentCalculator;
    class property CenterAlignmentCalculator: TdxParagraphAlignmentCalculatorBase read FCenterAlignmentCalculator;
    class property JustifyAlignmentCalculator: TdxParagraphAlignmentCalculatorBase read FJustifyAlignmentCalculator;
  end;

  { TdxParagraphLeftAlignmentCalculator }

  TdxParagraphLeftAlignmentCalculator = class(TdxParagraphAlignmentCalculatorBase)
  protected
    procedure AlignRowCore(const ARowBounds: TRect; ATextOffset: Integer;
      ABoxes: TdxBoxCollection; AFrom, ATo: Integer; AContainsLayoutDependentBox: Boolean); override;
  end;

  { TdxParagraphRightAlignmentCalculator }

  TdxParagraphRightAlignmentCalculator = class(TdxParagraphAlignmentCalculatorBase)
  protected
    procedure AlignRowCore(const ARowBounds: TRect; ATextOffset: Integer;
      ABoxes: TdxBoxCollection; AFrom, ATo: Integer; AContainsLayoutDependentBox: Boolean); override;
    function GetSectionBoxLeft(const ARowBounds: TRect): Integer; override;
  end;

  { TdxParagraphCenterAlignmentCalculator }

  TdxParagraphCenterAlignmentCalculator = class(TdxParagraphAlignmentCalculatorBase)
  protected
    procedure AlignRowCore(const ARowBounds: TRect; ATextOffset: Integer;
      ABoxes: TdxBoxCollection; AFrom, ATo: Integer; AContainsLayoutDependentBox: Boolean); override;
    function GetSectionBoxLeft(const ARowBounds: TRect): Integer; override;
  end;

  { TdxParagraphJustifyAlignmentCalculator }

  TdxParagraphJustifyAlignmentCalculator = class(TdxParagraphAlignmentCalculatorBase)
  strict private
    function FindFirstBoxIndex(ABoxes: TdxBoxCollection; AFrom, ATo: Integer): Integer;
    procedure JustifyRow(const ARowBounds: TRect; ATextOffset: Integer;
      ABoxes: TdxBoxCollection; AMinFrom, AFrom, ATo, ATotalSpaceWidth, ATotalWidth: Integer);
  protected
    procedure AlignRowCore(const ARowBounds: TRect; ATextOffset: Integer;
      ABoxes: TdxBoxCollection; AFrom, ATo: Integer; AContainsLayoutDependentBox: Boolean); override;
  end;

  { TdxBoxMeasurer }

  TdxBoxMeasurer = class(TcxIUnknownObject, IdxObjectMeasurer)
  strict private
    FTextViewInfoCache: TdxTextViewInfoCache;
    FDocumentModel: TdxCustomDocumentModel;
    FPieceTable: TdxCustomPieceTable;
    procedure SetPieceTable(const Value: TdxCustomPieceTable);
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel);
    destructor Destroy; override;

    function CreateTextViewInfo(ABoxInfo: TdxBoxInfo; const AText: string; AFontInfo: TdxFontInfo): TdxTextViewInfo; virtual; abstract;
    function MeasureText(const AText: string; AFontInfo: TdxFontInfo): TSize; overload; virtual;
    procedure MeasureText(ABoxInfo: TdxBoxInfo; const AText: string; AFontInfo: TdxFontInfo); overload;
    procedure MeasureText(ABoxInfo: TdxBoxInfo); overload;
    procedure BeginTextMeasure;
    procedure EndTextMeasure;
    procedure OnLayoutUnitChanged;
    function GetFontInfo(const APos: TdxFormatterPosition): TdxFontInfo; virtual;
    function MeasureRectangularObject(const AObject: IdxRectangularObject): TSize;
    procedure MeasureSpaces(ABoxInfo: TdxBoxInfo);
    procedure MeasureSingleSpace(ABoxInfo: TdxBoxInfo);
    procedure MeasureTab(ABoxInfo: TdxBoxInfo);
    procedure MeasureParagraphMark(ABoxInfo: TdxBoxInfo);
    procedure MeasureSectionMark(ABoxInfo: TdxBoxInfo);
    procedure MeasureLineBreakMark(ABoxInfo: TdxBoxInfo);
    procedure MeasurePageBreakMark(ABoxInfo: TdxBoxInfo);
    procedure MeasureColumnBreakMark(ABoxInfo: TdxBoxInfo);
    function MeasureHyphen(const APrevCharacterPos: TdxFormatterPosition; AHyphenBoxInfo: TdxBoxInfo): TSize;
    function MeasureCharactersBounds(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect): TArray<TRect>; virtual; abstract;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; const AText: string; AFontInfo: TdxFontInfo; AMaxWidth: Integer): Boolean; overload; virtual;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer): Boolean; overload; virtual;

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property PieceTable: TdxCustomPieceTable read FPieceTable write SetPieceTable;
    property TextViewInfoCache: TdxTextViewInfoCache read FTextViewInfoCache;
  end;

  { TdxVisitableDocumentIntervalBox }

  TdxVisitableDocumentIntervalBox = class abstract
  strict private
    FColor: TdxAlphaColor;
    FHorizontalPosition: Integer;
    FInterval: TdxCustomDocumentInterval;
  public
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); virtual; abstract;

    property Color: TdxAlphaColor read FColor write FColor;
    property HorizontalPosition: Integer read FHorizontalPosition write FHorizontalPosition;
    property Interval: TdxCustomDocumentInterval read FInterval write FInterval;
  end;
  TdxVisitableDocumentIntervalBoxCollection = class(TdxObjectList<TdxVisitableDocumentIntervalBox>);

implementation

uses
  Math, RTLConsts,
  dxTypeHelpers, cxGeometry,
  dxRichEdit.Utils.Exceptions,
  dxCharacters,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentLayout.UnitConverter;

{ TdxBoxHitTestCustomManager }

constructor TdxBoxHitTestCustomManager.Create(ACalculator: TdxBoxHitTestCustomCalculator;
  ABox: TdxBoxBase);
begin
  inherited Create;
  FCalculator := ACalculator;
  FBox := ABox;
end;

{ TdxBox }

class function TdxBox.CreateBox: TdxBox;
begin
  Result := nil;
  dxAbstractError;
end;

function TdxBox.CalcAscentAndFree(APieceTable: TdxCustomPieceTable): Integer;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := GetFontInfo(APieceTable);
  Result := AFontInfo.AscentAndFree;
end;

function TdxBox.CalcBaseAscentAndFree(APieceTable: TdxCustomPieceTable): Integer;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := GetFontInfo(APieceTable);
  Result := AFontInfo.GetBaseAscentAndFree(APieceTable.DocumentModel);
end;

function TdxBox.CalcBaseDescent(APieceTable: TdxCustomPieceTable): Integer;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := GetFontInfo(APieceTable);
  Result := AFontInfo.GetBaseDescent(APieceTable.DocumentModel);
end;

function TdxBox.CalcDescent(APieceTable: TdxCustomPieceTable): Integer;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := GetFontInfo(APieceTable);
  Result := AFontInfo.Descent;
end;

function TdxBox.GetActualForeColor(APieceTable: TdxCustomPieceTable; ATextColors: TdxTextColors; ABackColor: TdxAlphaColor): TdxAlphaColor;
var
  ARun: TdxTextRunBase;
  AColor, ARunBackColor: TdxAlphaColor;
begin
  ARun := TdxTextRunBase(GetRun(APieceTable));
  AColor := ARun.ForeColor;
  ARunBackColor := ARun.BackColor;
  if not TdxAlphaColors.IsTransparentOrEmpty(ARunBackColor) then
    Result := TdxAutoColorUtils.GetActualForeColor(ARunBackColor, AColor, ATextColors)
  else
    Result := TdxAutoColorUtils.GetActualForeColor(ABackColor, AColor, ATextColors);
end;

function TdxBox.GetActualStrikeoutColor(APieceTable: TdxCustomPieceTable; ATextColors: TdxTextColors; ABackColor: TdxAlphaColor): TdxAlphaColor;
begin
  Result := GetStrikeoutColorCore(APieceTable);
  if TdxAlphaColors.IsEmpty(Result) then
    Result := GetActualForeColor(APieceTable, ATextColors, ABackColor);
end;

function TdxBox.IsSpaceBox: Boolean;
begin
  Result := Supports(Self, IdxSpaceBox);
end;

function TdxBox.UseAscentAndDescentFromFontInfo: Boolean;
begin
  Result := True;
end;

function TdxBox.IsTabSpaceBox: Boolean;
begin
  Result := False;
end;

function TdxBox.GetActualUnderlineColor(APieceTable: TdxCustomPieceTable; ATextColors: TdxTextColors; ABackColor: TdxAlphaColor): TdxAlphaColor;
begin
  Result := GetUnderlineColorCore(APieceTable);
  if TdxAlphaColors.IsEmpty(Result) then
    Result := GetActualForeColor(APieceTable, ATextColors, ABackColor);
end;

function TdxBox.GetDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Box;
end;

function TdxBox.GetDocumentPosition(APieceTable: TdxCustomPieceTable;
  const APos: TdxFormatterPosition): TdxDocumentModelPosition;
var
  AParagraph: TdxSimpleParagraph;
  ARuns: TdxTextRunCollection;
  ALastRunIndex: TdxRunIndex;
  AParagraphOffset: Integer;
  I: Integer;
begin
  Result := TdxDocumentModelPosition.Create(APieceTable);
  AParagraph := TdxSimpleParagraph(GetRun(APieceTable).Paragraph);
  ARuns := AParagraph.PieceTable.Runs;
  ALastRunIndex := APos.RunIndex;
  AParagraphOffset := APos.Offset;
  for I := AParagraph.FirstRunIndex to ALastRunIndex - 1 do
    Inc(AParagraphOffset, ARuns[I].Length);
  Result.LogPosition := AParagraph.LogPosition + AParagraphOffset;
  Result.ParagraphIndex := AParagraph.Index;
  Result.RunIndex := APos.RunIndex;
  Result.RunStartLogPosition := Result.LogPosition - APos.Offset;
end;

function TdxBox.GetFontStrikeoutType(APieceTable: TdxCustomPieceTable): TdxStrikeoutType;
begin
  Result := TdxTextRunBase(GetRun(APieceTable)).FontStrikeoutType;
end;

function TdxBox.GetFontUnderlineType(APieceTable: TdxCustomPieceTable): TdxUnderlineType;
begin
  Result := TdxTextRunBase(GetRun(APieceTable)).FontUnderlineType;
end;

function TdxBox.GetHitTestAccuracy: TdxHitTestAccuracy;
begin
  Result := ExactBox;
end;

function TdxBox.IsSectionMarkBox: Boolean;
begin
  Result := False;
end;

function TdxBox.IsPageBreakBox: Boolean;
begin
  Result := False;
end;

function TdxBox.IsColumnBreakBox: Boolean;
begin
  Result := False;
end;

function TdxBox.GetStrikeoutColorCore(APieceTable: TdxCustomPieceTable): TdxAlphaColor;
begin
  Result := TdxTextRunBase(GetRun(APieceTable)).StrikeoutColor;
end;

function TdxBox.GetUnderlineColorCore(APieceTable: TdxCustomPieceTable): TdxAlphaColor;
begin
  Result := TdxTextRunBase(GetRun(APieceTable)).UnderlineColor;
end;

{ TdxBoxList }

function TdxBoxList.GetItem(Index: Integer): TdxBox;
begin
  Result := TdxBox(inherited Items[Index]);
end;

{ TdxCustomBoxCollection }

function TdxCustomBoxCollection.BinarySearchBoxIndex(const APos: TdxFormatterPosition): Integer;
var
  AComparable: TdxBoxAndFormatterPositionComparable;
begin
  AComparable := TdxBoxAndFormatterPositionComparable.Create(APos);
  try
    TdxAlgorithms1<TdxBoxBase>.BinarySearch(Self, AComparable, Result);
  finally
    AComparable.Free;
  end;
end;

function TdxCustomBoxCollection.BinarySearchBoxIndex(APieceTable: TdxCustomPieceTable;
  ALogPosition: TdxDocumentLogPosition): Integer;
var
  AComparable: TdxBoxAndLogPositionComparable;
begin
  AComparable := TdxBoxAndLogPositionComparable.Create(APieceTable, ALogPosition);
  try
    TdxAlgorithms1<TdxBoxBase>.BinarySearch(Self, AComparable, Result);
  finally
    AComparable.Free;
  end;
end;

procedure TdxCustomBoxCollection.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ExportTo(AExporter);
end;

procedure TdxCustomBoxCollection.MoveVertically(ADeltaY: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxBox(Items[I]).MoveVertically(ADeltaY);
end;

{ TdxBoxCollectionBase }

function TdxBoxCollectionBase<T>.First: T;
begin
  if Count > 0 then
    Result := T(inherited First)
  else
    Result := nil;
end;

function TdxBoxCollectionBase<T>.GetItem(Index: Integer): T;
begin
  Result := T(inherited Items[Index]);
end;

function TdxBoxCollectionBase<T>.Last: T;
begin
  if Count > 0 then
    Result := T(inherited Last)
  else
    Result := nil;
end;

{ TdxHiddenTextUnderlineBox }

constructor TdxHiddenTextUnderlineBox.Create(AStart, AEnd, ABottom: Integer);
begin
  inherited Create;
  FStart := AStart;
  FEnd := AEnd;
  FBottomOffset := ABottom;
end;

{ TdxSinglePositionBox }

function TdxSinglePositionBox.GetFirstFormatterPosition: TdxFormatterPosition;
begin
  Result := FPos;
end;

function TdxSinglePositionBox.GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  Result := GetDocumentPosition(APieceTable, FPos);
end;

function TdxSinglePositionBox.GetLastFormatterPosition: TdxFormatterPosition;
begin
 Result := FPos;
end;

function TdxSinglePositionBox.GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  Result := GetDocumentPosition(APieceTable, FPos);
end;

function TdxSinglePositionBox.GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase;
begin
  Result := APieceTable.Runs[FPos.RunIndex];
end;

function TdxSinglePositionBox.GetText(ATable: TdxCustomPieceTable): string;
begin
  Result := TdxSimplePieceTable(ATable).GetTextFromSingleRun(FPos, FPos);
end;

procedure TdxSinglePositionBox.OffsetRunIndices(ADelta: Integer);
begin
  FPos.RunIndex := FPos.RunIndex + ADelta;
end;

function TdxSinglePositionBox.GetEndPos: TdxFormatterPosition;
begin
  Result := FPos;
end;

function TdxSinglePositionBox.GetStartPos: TdxFormatterPosition;
begin
  Result := FPos;
end;

procedure TdxSinglePositionBox.SetEndPos(const Value: TdxFormatterPosition);
begin
  FPos := Value;
end;

procedure TdxSinglePositionBox.SetStartPos(const Value: TdxFormatterPosition);
begin
  FPos := Value;
end;

{ TdxInlinePictureBox }

function TdxInlinePictureBox.CalcAscentAndFree(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := Bounds.Height;
end;

function TdxInlinePictureBox.CalcBaseAscentAndFree(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := CalcAscentAndFree(APieceTable);
end;

function TdxInlinePictureBox.CalcBaseDescent(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := CalcDescent(APieceTable);
end;

function TdxInlinePictureBox.CalcDescent(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := 0;
end;

class function TdxInlinePictureBox.CreateBox: TdxBox;
begin
  Result := TdxInlinePictureBox.Create;
end;

function TdxInlinePictureBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := ACalculator.CreateInlinePictureBoxHitTestManager(Self);
end;

procedure TdxInlinePictureBox.ExportHotZones(APainter: TObject);
begin
end;

procedure TdxInlinePictureBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportInlinePictureBox(Self);
end;

function TdxInlinePictureBox.GetImage(APieceTable: TdxCustomPieceTable; AReadOnly: Boolean): TdxOfficeImage;
var
  ARun: TdxRunBase;
begin
  ARun := GetRun(APieceTable);
  Result := GetImageCore(ARun, AReadOnly);
end;

function TdxInlinePictureBox.GetImageActualSizeInLayoutUnits(APieceTable: TdxCustomPieceTable): TSize;
var
  ATextRun: TdxRunBase;
  ARun: TdxInlinePictureRun;
  AConverter: TdxDocumentLayoutUnitConverter;
  AImgSizeInPixels: TSize;
  AHRes, AVRes: Single;
begin
  ATextRun := GetRun(APieceTable);
  if not (ATextRun is TdxInlinePictureRun) then
    Exit(cxNullSize);
  ARun := TdxInlinePictureRun(ATextRun);
  AConverter := APieceTable.DocumentModel.LayoutUnitConverter;
  AImgSizeInPixels := ARun.Image.Size;
  AHRes := ARun.Image.HorizontalResolution;
  AVRes := ARun.Image.VerticalResolution;
  Result := cxSize(AConverter.PixelsToLayoutUnits(AImgSizeInPixels.Width, AHRes), AConverter.PixelsToLayoutUnits(AImgSizeInPixels.Height, AVRes));
end;

function TdxInlinePictureBox.GetImageCore(ARun: TdxRunBase; AReadOnly: Boolean): TdxOfficeImage;
var
  AContent: TdxPictureFloatingObjectContent;
  AContainer: IdxPictureContainerRun;
begin
  Result := nil;
  if not Supports(ARun, IdxPictureContainerRun, AContainer) then
    Exit;
  AContent := AContainer.PictureContent;
  Result := AContent.Image.Image;
end;

function TdxInlinePictureBox.IsInlinePicture: Boolean;
begin
  Result := True;
end;

function TdxInlinePictureBox.GetResizingShadowDisplayMode(APieceTable: TdxCustomPieceTable): TdxResizingShadowDisplayMode;
var
  ARun: TdxRunBase;
begin
  ARun := GetRun(APieceTable);
  if ARun is TdxInlinePictureRun then
    Result := TdxInlinePictureRun(ARun).ResizingShadowDisplayMode
  else
    Result := TdxResizingShadowDisplayMode.Content;
end;

function TdxInlinePictureBox.GetSizing(APieceTable: TdxCustomPieceTable): TdxImageSizeMode;
var
  ARun: TdxRunBase;
begin
  ARun := GetRun(APieceTable);
  if ARun is TdxInlinePictureRun then
    Result := TdxInlinePictureRun(ARun).Sizing
  else
    Result := TdxImageSizeMode.StretchImage;
end;

function TdxInlinePictureBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxInlinePictureBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxInlinePictureBox.IsVisible: Boolean;
begin
  Result := True;
end;

{ TdxNoPositionBox }

function TdxNoPositionBox.GetStartPos: TdxFormatterPosition;
begin
  Result := FPos;
end;

function TdxNoPositionBox.GetEndPos: TdxFormatterPosition;
begin
  Result := FPos;
end;

procedure TdxNoPositionBox.SetStartPos(const Value: TdxFormatterPosition);
begin
end;

procedure TdxNoPositionBox.SetEndPos(const Value: TdxFormatterPosition);
begin
end;

procedure TdxNoPositionBox.OffsetRunIndices(ADelta: Integer);
begin
end;

{ TdxMultiPositionBox }

function TdxMultiPositionBox.GetFirstFormatterPosition: TdxFormatterPosition;
begin
  Result := StartPos;
end;

function TdxMultiPositionBox.GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  Result := GetDocumentPosition(APieceTable, StartPos);
end;

function TdxMultiPositionBox.GetLastFormatterPosition: TdxFormatterPosition;
begin
  Result := EndPos;
end;

function TdxMultiPositionBox.GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  Result := GetDocumentPosition(APieceTable, EndPos);
end;

function TdxMultiPositionBox.GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase;
begin
  Result := APieceTable.Runs[StartPos.RunIndex];
end;

function TdxMultiPositionBox.GetText(ATable: TdxCustomPieceTable): string;
begin
  Result := TdxSimplePieceTable(ATable).GetTextFromSingleRun(StartPos, EndPos);
end;

procedure TdxMultiPositionBox.OffsetRunIndices(ADelta: Integer);
begin
  FStartPos.RunIndex := FStartPos.RunIndex + ADelta;
  FEndPos.RunIndex := FEndPos.RunIndex + ADelta;
end;

function TdxMultiPositionBox.GetEndPos: TdxFormatterPosition;
begin
  Result := FEndPos;
end;

function TdxMultiPositionBox.GetStartPos: TdxFormatterPosition;
begin
  Result := FStartPos;
end;

procedure TdxMultiPositionBox.SetEndPos(const Value: TdxFormatterPosition);
begin
  FEndPos := Value;
end;

procedure TdxMultiPositionBox.SetStartPos(const Value: TdxFormatterPosition);
begin
  FStartPos := Value;
end;

{ TdxUnderlineBox }

constructor TdxUnderlineBox.Create;
begin
  Create(0, 0);
end;

constructor TdxUnderlineBox.Create(AStartAnchorIndex: Integer);
begin
  Create(AStartAnchorIndex, 0);
end;

constructor TdxUnderlineBox.Create(AStartAnchorIndex, ABoxCount: Integer);
begin
  inherited Create;
  FStartAnchorIndex := AStartAnchorIndex;
  FBoxCount := ABoxCount;
end;

function TdxUnderlineBox.GetEndAnchorIndex: Integer;
begin
  Result := FStartAnchorIndex + FBoxCount;
end;

procedure TdxUnderlineBox.MoveVertically(ADeltaY: Integer);
begin
  OffsetRect(FUnderlineBounds, 0, ADeltaY);
  OffsetRect(FClipBounds, 0, ADeltaY);
end;

{ TdxUnderlineBoxCollectionBase }

procedure TdxUnderlineBoxCollectionBase.MoveVertically(ADeltaY: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[i].MoveVertically(ADeltaY);
end;

{ TdxErrorBox }

constructor TdxErrorBox.Create;
begin
  inherited Create;
  FErrorType := seMisspelling;
end;

constructor TdxErrorBox.Create(AStartAnchorIndex, ABoxCount, AFirstBoxOffset, ALastBoxOffset: Integer);
begin
  inherited Create(startAnchorIndex, boxCount);
  FFirstBoxOffset := AFirstBoxOffset;
  FLastBoxOffset := ALastBoxOffset;
end;

procedure TdxErrorBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportErrorBox(Self);
end;

{ TdxErrorBoxCollection }

function TdxErrorBoxCollection.GetItem(Index: Integer): TdxErrorBox;
begin
  Result := TdxErrorBox(inherited Items[Index]);
end;

{ TdxTextBox }

function TdxTextBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := ACalculator.CreateTextBoxHitTestManager(Self);
end;

function TdxTextBox.IsVisible: Boolean;
begin
  Result := True;
end;

class function TdxTextBox.CreateBox: TdxBox;
begin
  Result := TdxTextBox.Create;
end;

procedure TdxTextBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportTextBox(Self);
end;

function TdxTextBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxTextBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

{ TdxSpecialTextBox }

class function TdxSpecialTextBox.CreateBox: TdxBox;
begin
  Result := TdxSpecialTextBox.Create;
end;

procedure TdxSpecialTextBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportSpecialTextBox(Self);
end;

{ TdxSimpleRowExtendedBoxes }

destructor TdxSimpleRowExtendedBoxes.Destroy;
begin
  FreeAndNil(FUnderlines);
  FreeAndNil(FStrikeouts);
  FreeAndNil(FErrors);
  FreeAndNil(FHighlightAreas);
  FreeAndNil(FHiddenTextBoxes);
  FreeAndNil(FSpecialTextBoxes);
  FreeAndNil(FBoxRanges);
  inherited Destroy;
end;

function TdxSimpleRowExtendedBoxes.GetUnderlines: TdxUnderlineBoxCollection;
begin
  if FUnderlines = nil then
    FUnderlines := TdxUnderlineBoxCollection.Create;
  Result := FUnderlines;
end;

function TdxSimpleRowExtendedBoxes.GetStrikeouts: TdxUnderlineBoxCollection;
begin
  if FStrikeouts = nil then
    FStrikeouts := TdxUnderlineBoxCollection.Create;
  Result := FStrikeouts;
end;

function TdxSimpleRowExtendedBoxes.GetErrors: TdxErrorBoxCollection;
begin
  if FErrors = nil then
    FErrors := TdxErrorBoxCollection.Create;
  Result := FErrors;
end;

function TdxSimpleRowExtendedBoxes.GetHighlightAreas: TdxHighlightAreaCollection;
begin
  if FHighlightAreas = nil then
    FHighlightAreas := TdxHighlightAreaCollection.Create;
  Result := FHighlightAreas;
end;

function TdxSimpleRowExtendedBoxes.GetHiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection;
begin
  if FHiddenTextBoxes = nil then
    FHiddenTextBoxes := TdxHiddenTextUnderlineBoxCollection.Create;
  Result := FHiddenTextBoxes;
end;

function TdxSimpleRowExtendedBoxes.GetSpecialTextBoxes: TdxSpecialTextBoxCollection;
begin
  if FSpecialTextBoxes = nil then
    FSpecialTextBoxes := TdxSpecialTextBoxCollection.Create;
  Result := FSpecialTextBoxes;
end;

function TdxSimpleRowExtendedBoxes.GetBoxRanges: TdxRowBoxRangeCollection;
begin
  if FBoxRanges = nil then
    FBoxRanges := TdxRowBoxRangeCollection.Create;
  Result := FBoxRanges;
end;

procedure TdxSimpleRowExtendedBoxes.ClearUnderlines;
begin
  FreeAndNil(FUnderlines);
end;

procedure TdxSimpleRowExtendedBoxes.ClearStrikeouts;
begin
  FreeAndNil(FStrikeouts);
end;

procedure TdxSimpleRowExtendedBoxes.ClearErrors;
begin
  FreeAndNil(FErrors);
end;

procedure TdxSimpleRowExtendedBoxes.ClearHighlightAreas;
begin
  FreeAndNil(FHighlightAreas);
end;

procedure TdxSimpleRowExtendedBoxes.ClearHiddenTextBoxes;
begin
  FreeAndNil(FHiddenTextBoxes);
end;

procedure TdxSimpleRowExtendedBoxes.ClearSpecialTextBoxes;
begin
  FreeAndNil(FSpecialTextBoxes);
end;

procedure TdxSimpleRowExtendedBoxes.ClearBoxRanges;
begin
  FreeAndNil(FBoxRanges);
end;

{ TdxSimpleRow }

constructor TdxSimpleRow.Create;
begin
  inherited Create;
  FBoxes := TdxBoxCollection.Create;
end;

destructor TdxSimpleRow.Destroy;
begin
  FreeAndNil(FExtendedBoxes);
  FreeAndNil(FBoxes);
  inherited Destroy;
end;

function TdxSimpleRow.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxSimpleRow.IsTabelCellRow: Boolean;
begin
  Result := False;
end;

function TdxSimpleRow.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxSimpleRow.CreateExtendedBoxes: TdxSimpleRowExtendedBoxes;
begin
  Result := TdxSimpleRowExtendedBoxes.Create;
end;

function TdxSimpleRow.GetDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Row;
end;

function TdxSimpleRow.GetHitTestAccuracy: TdxHitTestAccuracy;
begin
  Result := ExactRow;
end;

function TdxSimpleRow.GetNumberingListBoxCore: TdxBox;
begin
  Result := nil;
end;

function TdxSimpleRow.GetExtendedBoxes: TdxSimpleRowExtendedBoxes;
begin
  if FExtendedBoxes = nil then
    FExtendedBoxes := CreateExtendedBoxes;
  Result := FExtendedBoxes;
end;

function TdxSimpleRow.GetInnerBoxRanges: TdxRowBoxRangeCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerBoxRanges
  else
    Result := nil;
end;

function TdxSimpleRow.GetInnerHiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerHiddenTextBoxes
  else
    Result := nil;
end;

function TdxSimpleRow.GetInnerHighlightAreas: TdxHighlightAreaCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerHighlightAreas
  else
    Result := nil;
end;

function TdxSimpleRow.GetInnerSpecialTextBoxes: TdxSpecialTextBoxCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerSpecialTextBoxes
  else
    Result := nil;
end;

function TdxSimpleRow.GetInnerStrikeouts: TdxUnderlineBoxCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerStrikeouts
  else
    Result := nil;
end;

function TdxSimpleRow.GetInnerUnderlines: TdxUnderlineBoxCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerUnderlines
  else
    Result := nil;
end;

function TdxSimpleRow.GetUnderlines: TdxUnderlineBoxCollection;
begin
  Result := ExtendedBoxes.Underlines;
end;

function TdxSimpleRow.GetStrikeouts: TdxUnderlineBoxCollection;
begin
  Result := ExtendedBoxes.Strikeouts;
end;

function TdxSimpleRow.GetErrors: TdxErrorBoxCollection;
begin
  Result := ExtendedBoxes.Errors;
end;

function TdxSimpleRow.GetHighlightAreas: TdxHighlightAreaCollection;
begin
  Result := ExtendedBoxes.HighlightAreas;
end;

function TdxSimpleRow.GetHiddenTextBoxes: TdxHiddenTextUnderlineBoxCollection;
begin
  Result := ExtendedBoxes.HiddenTextBoxes;
end;

function TdxSimpleRow.GetSpecialTextBoxes: TdxSpecialTextBoxCollection;
begin
  Result := ExtendedBoxes.SpecialTextBoxes;
end;

function TdxSimpleRow.GetBookmarkBoxesCore: TdxVisitableDocumentIntervalBoxCollection;
begin
  Result := nil;
end;

function TdxSimpleRow.GetRangePermissionBoxesCore: TdxVisitableDocumentIntervalBoxCollection;
begin
  Result := nil;
end;

function TdxSimpleRow.GetBoxRanges: TdxRowBoxRangeCollection;
begin
  Result := ExtendedBoxes.BoxRanges;
end;

function TdxSimpleRow.GetHeight: Integer;
begin
  Result := Bounds.Bottom - Bounds.Top;
end;

function TdxSimpleRow.GetLastParagraphRow: Boolean;
begin
  Result := FLastParagraphRowOriginalHeight <> 0;
end;

function TdxSimpleRow.GetShouldProcessCharacterLines: Boolean;
begin
  Result := TdxRowProcessingFlag.ProcessCharacterLines in FProcessingFlags;
end;

function TdxSimpleRow.GetShouldProcessHiddenText: Boolean;
begin
  Result := TdxRowProcessingFlag.ProcessHiddenText in FProcessingFlags;
end;

function TdxSimpleRow.GetShouldProcessTextHighlight: Boolean;
begin
  Result := TdxRowProcessingFlag.ProcessTextHighlight in FProcessingFlags;
end;

function TdxSimpleRow.GetShouldProcessLayoutDependentText: Boolean;
begin
  Result := TdxRowProcessingFlag.ProcessLayoutDependentText in FProcessingFlags;
end;

function TdxSimpleRow.GetShouldProcessSpecialTextBoxes: Boolean;
begin
  Result := TdxRowProcessingFlag.ProcessSpecialTextBoxes in FProcessingFlags;
end;

function TdxSimpleRow.GetContainsFootNotes: Boolean;
begin
  Result := TdxRowProcessingFlag.ContainsFootNotes in FProcessingFlags;
end;

function TdxSimpleRow.GetContainsEndNotes: Boolean;
begin
  Result := TdxRowProcessingFlag.ContainsEndNotes in FProcessingFlags;
end;

function TdxSimpleRow.GetIsFirstParagraphRow: Boolean;
begin
  Result := TdxRowProcessingFlag.FirstParagraphRow in FProcessingFlags;
end;

function TdxSimpleRow.GetIsLastParagraphRow: Boolean;
begin
  Result := TdxRowProcessingFlag.LastParagraphRow in FProcessingFlags;
end;

procedure TdxSimpleRow.SetParagraph(Value: TdxCustomParagraph);

  procedure Error;
  begin
    raise Exception.Create('Bad Paragraph');
  end;

begin
  if Value = nil then
    Error;
  FParagraph := Value;
end;

procedure TdxSimpleRow.SetHeight(Value: Integer);

  procedure Error;
  begin
    raise Exception.Create('Height < 0!');
  end;

begin
  if Value < 0 then
    Error;
  FBounds.Height := Value;
end;

procedure TdxSimpleRow.ClearUnderlines;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearUnderlines;
end;

procedure TdxSimpleRow.ClearStrikeouts;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearStrikeouts;
end;

procedure TdxSimpleRow.ClearErrors;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearErrors;
end;

procedure TdxSimpleRow.ClearHighlightAreas;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearHighlightAreas;
end;

procedure TdxSimpleRow.ClearHiddenTextBoxes;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearHiddenTextBoxes;
end;

procedure TdxSimpleRow.ClearSpecialTextBoxes;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearSpecialTextBoxes;
end;

procedure TdxSimpleRow.ClearBoxRanges;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearBoxRanges;
end;

function TdxSimpleRow.GetFirstFormatterPosition: TdxFormatterPosition;
begin
  Result := FBoxes.First.GetFirstFormatterPosition;
end;

function TdxSimpleRow.GetLastFormatterPosition: TdxFormatterPosition;
begin
  Result := FBoxes.Last.GetLastFormatterPosition;
end;

function TdxSimpleRow.GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  Result := FBoxes.First.GetFirstPosition(APieceTable);
end;

function TdxSimpleRow.GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  Result := FBoxes.Last.GetLastPosition(APieceTable);
end;

class function TdxSimpleRow.CreateBox: TdxBox;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Exit(nil);
end;

procedure TdxSimpleRow.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportSimpleRow(Self);
end;

function TdxSimpleRow.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := ACalculator.CreateRowHitTestManager(Self);
end;

function TdxSimpleRow.FindLastNonSpaceBoxIndex: Integer;
begin
  Result := FindLastNonSpaceBoxIndex(0);
end;

function TdxSimpleRow.FindLastNonSpaceBoxIndex(AStartIndex: Integer): Integer;
begin
  Result := FindLastNonSpaceBoxIndex(FBoxes, AStartIndex);
end;

class function TdxSimpleRow.FindLastNonSpaceBoxIndex(ABoxes: TdxBoxCollection; AStartIndex: Integer): Integer;
var
  ABox: TdxBox;
  I, ALastIndex: Integer;
begin
  if AStartIndex < 0 then
    AStartIndex := 0;
  ALastIndex := ABoxes.Count - 1;
  for I := ALastIndex downto AStartIndex do
  begin
    ABox := ABoxes[I];
    if not ABox.IsSpaceBox then
      Exit(I);
  end;
  Result := AStartIndex - 1;
end;

procedure TdxSimpleRow.MoveVertically(ADeltaY: Integer);
begin
  inherited MoveVertically(ADeltaY);
  FBoxes.MoveVertically(ADeltaY);
  if InnerUnderlines <> nil then
    InnerUnderlines.MoveVertically(ADeltaY);
  if InnerStrikeouts <> nil then
    InnerStrikeouts.MoveVertically(ADeltaY);
end;

function TdxSimpleRow.GetText(ATable: TdxCustomPieceTable): string;
begin
  raise Exception.Create('TdxRow.GetText');
  Result := '';
end;

function TdxSimpleRow.GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase;
begin
 // Result := nil;
  raise Exception.Create('TdxRow.GetRun');
end;

function TdxSimpleRow.IsVisible: Boolean;
begin
  Result := Boolean(NotImplemented);
end;

procedure TdxSimpleRow.ClearBookmarkBoxes;
begin
end;

procedure TdxSimpleRow.ClearRangePermissionBoxes;
begin
end;

{ TdxSingleCharacterMarkBox }

function TdxSingleCharacterMarkBox.GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo;
var
  ADocumentModel: TdxCustomDocumentModel;
  ARun: TdxTextRunBase;
  AFontIndex: Integer;
begin
  Result := inherited GetFontInfo(APieceTable);
  ADocumentModel := APieceTable.DocumentModel;
  if not ADocumentModel.FontCache.ShouldUseDefaultFontToDrawInvisibleCharacter(Result, MarkCharacter) then
  begin
    ARun := TdxTextRunBase(GetRun(APieceTable));
    AFontIndex := ARun.CalculateFontIndexCore(ARun.DocumentModel.DefaultCharacterProperties.FontName);
    Result := ADocumentModel.FontCache[AFontIndex];
  end;
end;

{ TdxLineBreakBox }

class function TdxLineBreakBox.CreateBox: TdxBox;
begin
  Result := TdxLineBreakBox.Create;
end;

function TdxLineBreakBox.IsSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxLineBreakBox.IsVisible: Boolean;
begin
  Result := False;
end;

function TdxLineBreakBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxLineBreakBox.IsLineBreak: Boolean;
begin
  Result := True;
end;

procedure TdxLineBreakBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportLineBreakBox(Self);
end;

function TdxLineBreakBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := ACalculator.CreateLineBreakBoxHitTestManager(Self);
end;

function TdxLineBreakBox.GetMarkCharacter: Char;
begin
  Result := #$2190;
end;

{ TdxSeparatorBox }

class function TdxSeparatorBox.CreateBox: TdxBox;
begin
  Result := TdxSeparatorBox.Create;
end;

function TdxSeparatorBox.IsVisible: Boolean;
begin
  Result := False;
end;

function TdxSeparatorBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := False;
end;

function TdxSeparatorBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

procedure TdxSeparatorBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportSeparatorBox(Self);
end;

function TdxSeparatorBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := ACalculator.CreateSeparatorMarkBoxHitTestManager(Self);
end;

function TdxSeparatorBox.GetMarkCharacter: Char;
begin
  Result := TdxCharacters.SeparatorMark;
end;

{ TdxParagraphMarkBox }

class function TdxParagraphMarkBox.CreateBox: TdxBox;
begin
  Result := TdxParagraphMarkBox.Create;
end;

function TdxParagraphMarkBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := ACalculator.CreateParagraphMarkBoxHitTestManager(Self)
end;

function TdxParagraphMarkBox.IsVisible: Boolean;
begin
  Result := False;
end;

procedure TdxParagraphMarkBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportParagraphMarkBox(Self);
end;

function TdxParagraphMarkBox.IsLineBreak: Boolean;
begin
  Result := True;
end;

function TdxParagraphMarkBox.IsSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxParagraphMarkBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxParagraphMarkBox.GetMarkCharacter: Char;
begin
  Result := TdxCharacters.PilcrowSign;
end;

{ TdxSingleSpaceBox }

class function TdxSingleSpaceBox.CreateBox: TdxBox;
begin
  Result := TdxSingleSpaceBox.Create;
end;

function TdxSingleSpaceBox.IsVisible: Boolean;
begin
  Result := False;
end;

function TdxSingleSpaceBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := ACalculator.CreateSpaceBoxHitTestManager(Self);
end;

procedure TdxSingleSpaceBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportSpaceBox(Self);
end;

function TdxSingleSpaceBox.GetBox: TdxBox;
begin
  Result := Self;
end;

function TdxSingleSpaceBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxSingleSpaceBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := False;
end;

function TdxSingleSpaceBox.GetMarkCharacter: Char;
begin
  Result := TdxCharacters.MiddleDot;
end;

function TdxSingleSpaceBox.GetMinWidth: Integer;
begin
  Result := FMinWidth;
end;

procedure TdxSingleSpaceBox.SetMinWidth(const Value: Integer);
begin
  FMinWidth := Value;
end;

{ TdxCharacterBox }

class function TdxCharacterBox.CreateBox: TdxBox;
begin
  Result := TdxCharacterBox.Create;
end;

procedure TdxCharacterBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
end;

function TdxCharacterBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := ACalculator.CreateCharacterBoxHitTestManager(Self);
end;

procedure TdxCharacterBox.MoveVertically(ADeltaY: Integer);
begin
  inherited MoveVertically(ADeltaY);
  FTightBounds.Offset(0, ADeltaY);
end;

function TdxCharacterBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxCharacterBox.IsVisible: Boolean;
begin
  Result := True;
end;

function TdxCharacterBox.Clone: TdxCharacterBox;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxCharacterBox.Create;

  Result.FTightBounds := TightBounds;
  Result.StartPos := StartPos;
  Result.EndPos := EndPos;
  Result.Bounds := Bounds;
end;

function TdxCharacterBox.GetDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Character;
end;

function TdxCharacterBox.GetHitTestAccuracy: TdxHitTestAccuracy;
begin
  Result := ExactCharacter;
end;

function TdxCharacterBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

{ TdxCharacterBoxCollection }

procedure TdxCharacterBoxCollection.RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator);
begin
  ACalculator.RegisterCharacterHitTest(nil);
end;

procedure TdxCharacterBoxCollection.RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox);
begin
  ACalculator.RegisterCharacterHitTest(TdxCharacterBox(AItem));
end;

{ TdxCharacterMarkBox }

function TdxCharacterMarkBox.GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo;
var
  ADocumentModel: TdxCustomDocumentModel;
  ARun: TdxTextRunBase;
  AFontIndex: Integer;
begin
  Result := inherited GetFontInfo(APieceTable);
  ADocumentModel := APieceTable.DocumentModel;
  if not ADocumentModel.FontCache.ShouldUseDefaultFontToDrawInvisibleCharacter(Result, MarkCharacter) then
  begin
    ARun := TdxTextRunBase(GetRun(APieceTable));
    AFontIndex := ARun.CalculateFontIndexCore(ARun.DocumentModel.Cache.CharacterFormattingCache.DefaultItem.FontName);
    Result := ADocumentModel.FontCache[AFontIndex];
  end;
end;

function TdxCharacterMarkBox.IsVisible: Boolean;
begin
  Result := True;
end;

{ TdxSpaceBoxa }

class function TdxSpaceBoxa.CreateBox: TdxBox;
begin
  Result := TdxSpaceBoxa.Create;
end;

function TdxSpaceBoxa.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := ACalculator.CreateSpaceBoxHitTestManager(Self);
end;

procedure TdxSpaceBoxa.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportSpaceBox(Self);
end;

function TdxSpaceBoxa.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxSpaceBoxa.IsNotWhiteSpaceBox: Boolean;
begin
  Result := False;
end;

function TdxSpaceBoxa.IsVisible: Boolean;
begin
  Result := False;
end;

procedure TdxSpaceBoxa.MoveVertically(ADeltaY: Integer);
begin
  inherited MoveVertically(ADeltaY);
  FTightBounds.Offset(0, ADeltaY);
end;

function TdxSpaceBoxa.GetBox: TdxBox;
begin
  Result := Self;
end;

function TdxSpaceBoxa.GetMarkCharacter: Char;
begin
  Result := TdxCharacters.MiddleDot;
end;

function TdxSpaceBoxa.GetMinWidth: Integer;
begin
  Result := FMinWidth;
end;

procedure TdxSpaceBoxa.SetMinWidth(const Value: Integer);
begin
  FMinWidth := Value;
end;

{ TdxHyphenBox }

class function TdxHyphenBox.CreateBox: TdxBox;
begin
  Result := TdxHyphenBox.Create;
end;

function TdxHyphenBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := ACalculator.CreateHyphenBoxHitTestManager(Self);
end;

procedure TdxHyphenBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  AExporter.ExportHyphenBox(Self);
end;

function TdxHyphenBox.GetText(ATable: TdxCustomPieceTable): string;
begin
  Result := HyphenString;
end;

{ TdxBoxCollection }

function TdxBoxCollection.BinarySearchBox(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition): TdxBox;
var
  AIndex: Integer;
begin
  AIndex := BinarySearchBoxIndex(APieceTable, ALogPosition);
  if AIndex < 0 then
    Result := nil
  else
    Result := Self[AIndex];
end;

procedure TdxBoxCollection.RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox);
begin
  ACalculator.RegisterBoxHitTest(AItem);
end;

procedure TdxBoxCollection.RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator);
begin
  ACalculator.RegisterBoxHitTest(nil);
end;

{ TdxSimpleParagraphBoxCollection }

constructor TdxSimpleParagraphBoxCollection.Create;
begin
  inherited Create;
  FBoxCollection := TdxBoxCollection.Create;
  InvalidateBoxes;
end;

destructor TdxSimpleParagraphBoxCollection.Destroy;
begin
  FreeAndNil(FBoxCollection);
  inherited Destroy;
end;

procedure TdxSimpleParagraphBoxCollection.Add(ABox: TdxBox);
begin
  Assert(ABox <> nil);
  FBoxCollection.Add(ABox);
end;

procedure TdxSimpleParagraphBoxCollection.Clear;
begin
  FBoxCollection.Clear;
  FParagraphStartRunIndex := -1;
end;

function TdxSimpleParagraphBoxCollection.ContainsFloatingObjectAnchorBox: Boolean;
begin
  Result := False;
end;

function TdxSimpleParagraphBoxCollection.GetBox(ABoxInfo: TdxBoxInfo): TdxBox;
var
  I: Integer;
begin
  Result := nil;
  Assert(ABoxInfo <> nil);
  for I := 0 to FBoxCollection.Count - 1 do
    if (FBoxCollection[I].StartPos = ABoxInfo.StartPos) and (FBoxCollection[I].EndPos = ABoxInfo.EndPos) then
    begin
      Result := FBoxCollection[I];
      Break;
    end;
end;

function TdxSimpleParagraphBoxCollection.GetCount: Integer;
begin
  Result := FBoxCollection.Count;
end;

function TdxSimpleParagraphBoxCollection.GetIsValid: Boolean;
begin
  Result := FParagraphStartRunIndex >= 0;
end;

function TdxSimpleParagraphBoxCollection.GetItem(Index: Integer): TdxBox;
begin
  Result := FBoxCollection[Index];
end;

procedure TdxSimpleParagraphBoxCollection.InvalidateBoxes;
begin
  FParagraphStartRunIndex := -1;
end;

procedure TdxSimpleParagraphBoxCollection.OffsetRunIndices(ADelta: Integer);
var
  I: Integer;
begin
  for I := 0 to FBoxCollection.Count - 1 do
    FBoxCollection[I].OffsetRunIndices(ADelta);
end;

procedure TdxSimpleParagraphBoxCollection.SetParagraphStartRunIndex(const Value: TdxRunIndex);
begin
  if FParagraphStartRunIndex = Value then
    Exit;
  if IsValid then
    OffsetRunIndices(Value - FParagraphStartRunIndex);
  FParagraphStartRunIndex := Value;
end;

{ TdxParagraphAlignmentCalculatorBase }

class constructor TdxParagraphAlignmentCalculatorBase.Initialize;
begin
  FLeftAlignmentCalculator := TdxParagraphLeftAlignmentCalculator.Create;
  FRightAlignmentCalculator := TdxParagraphRightAlignmentCalculator.Create;
  FCenterAlignmentCalculator := TdxParagraphCenterAlignmentCalculator.Create;
  FJustifyAlignmentCalculator := TdxParagraphJustifyAlignmentCalculator.Create;
end;

class destructor TdxParagraphAlignmentCalculatorBase.Finalize;
begin
  FreeAndNil(FLeftAlignmentCalculator);
  FreeAndNil(FRightAlignmentCalculator);
  FreeAndNil(FCenterAlignmentCalculator);
  FreeAndNil(FJustifyAlignmentCalculator);
end;

procedure TdxParagraphAlignmentCalculatorBase.AlignNumberingListBox(ANumberingListBox, AFirstRowBox: TdxBox);
var
  R: TRect;
begin
  if ANumberingListBox = nil then
    Exit;
  R := ANumberingListBox.Bounds;
  R.Location := Point(AFirstRowBox.Bounds.Left - R.Width, R.Top);
  ANumberingListBox.Bounds := R;
end;

procedure TdxParagraphAlignmentCalculatorBase.AlignMultipartRow(const ARowBounds: TRect; ARow: TdxSimpleRow);
var
  ABoxes: TdxBoxCollection;
  ARowBoxRanges: TdxRowBoxRangeCollection;
  ARange: TdxRowBoxRange;
  AShouldProcessLayoutDependentText: Boolean;
  ALastBox: TdxBox;
  I, ACount: Integer;
begin
  ABoxes := ARow.Boxes;
  ARowBoxRanges := ARow.InnerBoxRanges;
  ACount := ARowBoxRanges.Count;
  AShouldProcessLayoutDependentText := ARow.ShouldProcessLayoutDependentText;
  for I := 0 to ACount - 1 do
  begin
    ARange := ARowBoxRanges[I];
    AlignRowCore(ARange.Bounds, ARow.TextOffset, ABoxes, ARange.FirstBoxIndex, ARange.LastBoxIndex, AShouldProcessLayoutDependentText);
  end;
  ALastBox := ABoxes.Last;
  if ALastBox.IsSectionMarkBox or ALastBox.IsPageBreakBox or ALastBox.IsColumnBreakBox then
    AlignBreakBox(ARowBounds, ABoxes);
  AlignNumberingListBox(ARow.GetNumberingListBoxCore, ARow.Boxes.First);
end;

procedure TdxParagraphAlignmentCalculatorBase.AlignRow(ARow: TdxSimpleRow);
begin
  if ARow.InnerBoxRanges = nil then
    AlignRow(ARow.Bounds, ARow)
  else
    AlignMultipartRow(ARow.Bounds, ARow);
end;

procedure TdxParagraphAlignmentCalculatorBase.AlignRow(const ARowBounds: TRect; ARow: TdxSimpleRow);
var
  ABoxes: TdxBoxCollection;
  ALastBox: TdxBox;
begin
  ABoxes := ARow.Boxes;
  AlignRowCore(ARowBounds, ARow.TextOffset, ABoxes, 0, ABoxes.Count - 1, ARow.ShouldProcessLayoutDependentText);
  ALastBox := ABoxes.Last;
  if ALastBox.IsSectionMarkBox or ALastBox.IsPageBreakBox or ALastBox.IsColumnBreakBox then
    AlignBreakBox(ARowBounds, ABoxes);
  AlignNumberingListBox(ARow.GetNumberingListBoxCore, ARow.Boxes.First);
end;

class function TdxParagraphAlignmentCalculatorBase.GetAlignmentCalculator(
  AAlignment: TdxParagraphAlignment): TdxParagraphAlignmentCalculatorBase;
begin
  case AAlignment of
    TdxParagraphAlignment.Center:
      Result := CenterAlignmentCalculator;
    TdxParagraphAlignment.Right:
      Result := RightAlignmentCalculator;
    TdxParagraphAlignment.Left:
      Result := LeftAlignmentCalculator;
    else
      Result := JustifyAlignmentCalculator;
  end;
end;

procedure TdxParagraphAlignmentCalculatorBase.AlignBreakBox(const ABounds: TRect; ABoxes: TdxBoxCollection);
var
  ASectionBox: TdxBox;
  R: TRect;
  AContentRightBounds: Integer;
begin
  ASectionBox := ABoxes.Last;
  R := ASectionBox.Bounds;
  AContentRightBounds := GetRowContentRightBounds(ABounds, ABoxes);
  R.Width := Max(1, Abs(ABounds.Right - AContentRightBounds));
  R.Location := Point(Min(ABounds.Right, AContentRightBounds), R.Top);
  ASectionBox.Bounds := R;
end;

class function TdxParagraphAlignmentCalculatorBase.AlignLeft(ABoxes: TdxBoxCollection; ALeft, AFrom, ATo: Integer): Integer;
var
  I: Integer;
  ABox: TdxBox;
  R: TRect;
  ASpaceBox: IdxSpaceBox;
begin
  for I := AFrom to ATo do
  begin
    ABox := ABoxes[I];
    R := ABox.Bounds;
    if Supports(ABox, IdxSpaceBox, ASpaceBox) then
      R.Width := ASpaceBox.MinWidth;
    R.MoveToLeft(ALeft);
    Inc(ALeft, R.Width);
    ABox.Bounds := R;
  end;
  Result := ALeft;
end;

function TdxParagraphAlignmentCalculatorBase.GetSectionBoxLeft(const ABounds: TRect): Integer;
begin
  Result := ABounds.Left;
end;

class function TdxParagraphAlignmentCalculatorBase.FindLastVisibleBoxIndex(ABoxes: TdxBoxCollection; AFrom, ATo: Integer): Integer;
var
  I: Integer;
begin
  for I := ATo downto AFrom do
    if ABoxes[I].IsVisible then
      Exit(I);
  Result := AFrom - 1;
end;

class function TdxParagraphAlignmentCalculatorBase.IsBoxOutsideOfRow(const ARowBounds: TRect;
  AFrom: Integer; ABoxes: TdxBoxCollection; ABoxIndex: Integer): Boolean;
begin
  Result := (ABoxIndex > AFrom) and (ABoxes[ABoxIndex].Bounds.Right > ARowBounds.Right);
end;

function TdxParagraphAlignmentCalculatorBase.GetRowContentRightBounds(const ARowBounds: TRect;
  ABoxes: TdxBoxCollection): Integer;
var
  ACount: Integer;
begin
  ACount := ABoxes.Count;
  if ACount > 1 then
    Result := ABoxes[ACount - 2].Bounds.Right
  else
		Result := GetSectionBoxLeft(ARowBounds);
end;

{ TdxParagraphLeftAlignmentCalculator }

procedure TdxParagraphLeftAlignmentCalculator.AlignRowCore(const ARowBounds: TRect; ATextOffset: Integer;
  ABoxes: TdxBoxCollection; AFrom, ATo: Integer; AContainsLayoutDependentBox: Boolean);
begin
  AlignLeft(ABoxes, ARowBounds.Left + ATextOffset, AFrom, ATo);
end;

{ TdxParagraphRightAlignmentCalculator }

procedure TdxParagraphRightAlignmentCalculator.AlignRowCore(const ARowBounds: TRect; ATextOffset: Integer;
  ABoxes: TdxBoxCollection; AFrom, ATo: Integer; AContainsLayoutDependentBox: Boolean);
var
  ABox: TdxBox;
  ALastNotSpaceBoxIndex: Integer;
  ARight: Integer;
  R: TRect;
  I: Integer;
  ASpaceBox: IdxSpaceBox;
begin
  if ATo < AFrom then
    Exit;

  ALastNotSpaceBoxIndex := FindLastVisibleBoxIndex(ABoxes, AFrom, ATo);
  if IsBoxOutsideOfRow(ARowBounds, AFrom, ABoxes, ALastNotSpaceBoxIndex) then
  begin
    AlignLeft(ABoxes, ARowBounds.Left + ATextOffset, AFrom, ATo);
    if not AContainsLayoutDependentBox or IsBoxOutsideOfRow(ARowBounds, AFrom, ABoxes, ALastNotSpaceBoxIndex) then
      Exit;
  end;

  AlignLeft(ABoxes, ARowBounds.Right, ALastNotSpaceBoxIndex + 1, ATo);
  ARight := ARowBounds.Right;
  for I := ALastNotSpaceBoxIndex downto AFrom do
  begin
    ABox := ABoxes[I];
    R := ABox.Bounds;
    if Supports(ABox, IdxSpaceBox, ASpaceBox) then
      R.Width := ASpaceBox.MinWidth;
    ARight := Max(ARowBounds.Left, ARight - R.Width);
    R.X := ARight;
    ABox.Bounds := R;
  end;
end;

function TdxParagraphRightAlignmentCalculator.GetSectionBoxLeft(
  const ARowBounds: TRect): Integer;
begin
  Result := ARowBounds.Right;
end;

{ TdxParagraphCenterAlignmentCalculator }

procedure TdxParagraphCenterAlignmentCalculator.AlignRowCore(const ARowBounds: TRect; ATextOffset: Integer;
  ABoxes: TdxBoxCollection; AFrom, ATo: Integer; AContainsLayoutDependentBox: Boolean);
var
  ALastNotSpaceBoxIndex: Integer;
  ATotalWidth: Integer;
  ABox: TdxBox;
  ASpaceBox: IdxSpaceBox;
  ALeft, I: Integer;
  R: TRect;
begin
  if ATo < AFrom then
    Exit;
  ALastNotSpaceBoxIndex := FindLastVisibleBoxIndex(ABoxes, AFrom, ATo);
  if IsBoxOutsideOfRow(ARowBounds, AFrom, ABoxes, ALastNotSpaceBoxIndex) then
  begin
    AlignLeft(ABoxes, ARowBounds.Left + ATextOffset, AFrom, ATo);
    if not AContainsLayoutDependentBox or IsBoxOutsideOfRow(ARowBounds, AFrom, ABoxes, ALastNotSpaceBoxIndex) then
      Exit;
  end;
  ATotalWidth := 0;
  for I := AFrom to ALastNotSpaceBoxIndex do
  begin
    ABox := ABoxes[I];
    if Supports(ABox, IdxSpaceBox, ASpaceBox) then
      ATotalWidth := ATotalWidth + ASpaceBox.MinWidth
    else
      ATotalWidth := ATotalWidth + ABox.Bounds.Width;
  end;
  ALeft := Max(ARowBounds.Left, (ARowBounds.Left + ARowBounds.Right - ATotalWidth + ATextOffset) div 2);
  for I := AFrom to ALastNotSpaceBoxIndex do
  begin
    ABox := ABoxes[I];
    R := ABox.Bounds;
    if Supports(ABox, IdxSpaceBox, ASpaceBox) then
      R.Width := ASpaceBox.MinWidth;
    R.X := ALeft;
    ALeft := R.Right;
    ABox.Bounds := R;
  end;
  AlignLeft(ABoxes, ALeft, ALastNotSpaceBoxIndex + 1, ATo);
end;

function TdxParagraphCenterAlignmentCalculator.GetSectionBoxLeft(const ARowBounds: TRect): Integer;
begin
  Result := (ARowBounds.Left + ARowBounds.Right) div 2;
end;

{ TdxParagraphJustifyAlignmentCalculator }

procedure TdxParagraphJustifyAlignmentCalculator.AlignRowCore(const ARowBounds: TRect; ATextOffset: Integer;
  ABoxes: TdxBoxCollection; AFrom, ATo: Integer; AContainsLayoutDependentBox: Boolean);
var
  ALastNotSpaceBoxIndex: Integer;
  AFirstBoxIndex, ATotalWidth, ATotalSpaceWidth: Integer;
  I: Integer;
  ABox: TdxBox;
  ASpaceBox: IdxSpaceBox;
begin
  if ATo < AFrom then
    Exit;
  if ABoxes[ATo] is TdxParagraphMarkBox then
    AlignLeft(ABoxes, ARowBounds.Left + ATextOffset, AFrom, ATo)
  else
  begin
    if (ATo - AFrom > 0) and (ABoxes[ATo - 1] is TdxParagraphMarkBox) then
      AlignLeft(ABoxes, ARowBounds.Left + ATextOffset, AFrom, ATo)
    else
    begin
      ALastNotSpaceBoxIndex := FindLastVisibleBoxIndex(ABoxes, AFrom, ATo);
      if IsBoxOutsideOfRow(ARowBounds, AFrom, ABoxes, ALastNotSpaceBoxIndex) then
      begin
        AlignLeft(ABoxes, ARowBounds.Left + ATextOffset, AFrom, ATo);
        if not AContainsLayoutDependentBox or IsBoxOutsideOfRow(ARowBounds, AFrom, ABoxes, ALastNotSpaceBoxIndex) then
          Exit;
      end;
      AlignLeft(ABoxes, ARowBounds.Right, ALastNotSpaceBoxIndex + 1, ATo);
      AFirstBoxIndex := FindFirstBoxIndex(ABoxes, AFrom, ALastNotSpaceBoxIndex);
      ATotalWidth := 0;
      ATotalSpaceWidth := 0;
      for I := ALastNotSpaceBoxIndex downto AFirstBoxIndex + 1 do
      begin
        ABox := ABoxes[I];
        if Supports(ABox, IdxSpaceBox, ASpaceBox) then
        begin
          Inc(ATotalWidth, ASpaceBox.MinWidth);
          Inc(ATotalSpaceWidth, ASpaceBox.MinWidth);
        end
        else
          Inc(ATotalWidth, ABox.Bounds.Width);
      end;
      if ATotalSpaceWidth = 0 then
        TdxParagraphAlignmentCalculatorBase.LeftAlignmentCalculator.AlignRowCore(ARowBounds, ATextOffset, ABoxes, AFrom, ATo, AContainsLayoutDependentBox)
      else
        JustifyRow(ARowBounds, ATextOffset, ABoxes, AFrom, AFirstBoxIndex + 1, ALastNotSpaceBoxIndex, ATotalSpaceWidth, ATotalWidth);
    end;
  end;
end;

function TdxParagraphJustifyAlignmentCalculator.FindFirstBoxIndex(ABoxes: TdxBoxCollection; AFrom, ATo: Integer): Integer;
var
  ALastNonSpace: Integer;
  I: Integer;
begin
  ALastNonSpace := ATo;
  Result := AFrom - 1;
  for I := ATo downto AFrom do
  begin
    if ABoxes[I].IsTabSpaceBox then
    begin
      Result := ALastNonSpace;
      Break;
    end
    else
      if not Supports(ABoxes[I], IdxSpaceBox) then
        ALastNonSpace := I;
  end;
end;

procedure TdxParagraphJustifyAlignmentCalculator.JustifyRow(const ARowBounds: TRect; ATextOffset: Integer;
  ABoxes: TdxBoxCollection; AMinFrom, AFrom, ATo, ATotalSpaceWidth, ATotalWidth: Integer);
var
  ABox: TdxBox;
  ALeft, AFreeSpace, ARemainder: Integer;
  I: Integer;
  R: TRect;
  ASpaceBox: IdxSpaceBox;
  T, ADelta: Integer;
begin
  ALeft := AlignLeft(ABoxes, ARowBounds.Left + ATextOffset, AMinFrom, AFrom - 1);
  AFreeSpace := ARowBounds.Right - ALeft - ATotalWidth;
  ARemainder := 0;
  for I := AFrom to ATo do
  begin
    ABox := ABoxes[I];
    R := cxRectSetLeft(ABox.Bounds, ALeft);
    if Supports(ABox, IdxSpaceBox, ASpaceBox) then
    begin
      T := AFreeSpace * ASpaceBox.MinWidth + ARemainder;
      ADelta := T div ATotalSpaceWidth;
      ARemainder := T mod ATotalSpaceWidth;
      R := cxRectSetWidth(R, ASpaceBox.MinWidth + ADelta);
    end;
    Inc(ALeft, R.Width);
    ABox.Bounds := R;
  end;
end;

{ TdxBoxMeasurer }

constructor TdxBoxMeasurer.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FPieceTable := ADocumentModel.MainPart;
  FTextViewInfoCache := TdxTextViewInfoCache.Create;
end;

destructor TdxBoxMeasurer.Destroy;
begin
  FreeAndNil(FTextViewInfoCache);
  inherited Destroy;
end;

procedure TdxBoxMeasurer.BeginTextMeasure;
begin
end;

function TdxBoxMeasurer.MeasureText(const AText: string; AFontInfo: TdxFontInfo): TSize;
var
  ATextViewInfo: TdxTextViewInfo;
begin
  ATextViewInfo := TextViewInfoCache.TryGetTextViewInfo(AText, AFontInfo);
  if ATextViewInfo = nil then
  begin
    ATextViewInfo := CreateTextViewInfo(nil, AText, AFontInfo);
    TextViewInfoCache.AddTextViewInfo(AText, AFontInfo, ATextViewInfo);
  end;
  Result := ATextViewInfo.Size;
end;

procedure TdxBoxMeasurer.MeasureText(ABoxInfo: TdxBoxInfo; const AText: string; AFontInfo: TdxFontInfo);
begin
  ABoxInfo.Size :=  MeasureText(AText, AFontInfo);
end;

procedure TdxBoxMeasurer.EndTextMeasure;
begin
end;

function TdxBoxMeasurer.GetFontInfo(const APos: TdxFormatterPosition): TdxFontInfo;
var
  ARun: TdxRunBase;
begin
  ARun := PieceTable.Runs[APos.RunIndex];
  Result := DocumentModel.FontCache[ARun.FontCacheIndex];
end;

function TdxBoxMeasurer.MeasureRectangularObject(const AObject: IdxRectangularObject): TSize;
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AActualSize: TSize;
  AWidth, AHeight: Integer;
begin
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  AActualSize := AObject.ActualSize;
  AWidth := Math.Max(1, AUnitConverter.ToLayoutUnits(AActualSize.Width));
  AHeight := Math.Max(1, AUnitConverter.ToLayoutUnits(AActualSize.Height));
  Result := cxSize(AWidth, AHeight);
end;

procedure TdxBoxMeasurer.MeasureColumnBreakMark(ABoxInfo: TdxBoxInfo);
begin
  ABoxInfo.Size := cxSize(1, ABoxInfo.GetFontInfo(PieceTable).LineSpacing);
end;

function TdxBoxMeasurer.MeasureHyphen(const APrevCharacterPos: TdxFormatterPosition; AHyphenBoxInfo: TdxBoxInfo): TSize;
var
  ATextViewInfo: TdxTextViewInfo;
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := GetFontInfo(APrevCharacterPos);
  ATextViewInfo := CreateTextViewInfo(AHyphenBoxInfo, TdxHyphenBox.HyphenString, AFontInfo);
  Result := ATextViewInfo.Size;
end;

procedure TdxBoxMeasurer.MeasureLineBreakMark(ABoxInfo: TdxBoxInfo);
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := ABoxInfo.GetFontInfo(PieceTable);
  ABoxInfo.Size := cxSize(AFontInfo.PilcrowSignWidth, AFontInfo.LineSpacing);
end;

procedure TdxBoxMeasurer.MeasurePageBreakMark(ABoxInfo: TdxBoxInfo);
begin
  ABoxInfo.Size := cxSize(1, ABoxInfo.GetFontInfo(PieceTable).LineSpacing);
end;

procedure TdxBoxMeasurer.MeasureParagraphMark(ABoxInfo: TdxBoxInfo);
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := ABoxInfo.GetFontInfo(PieceTable);
  ABoxInfo.Size := cxSize(AFontInfo.PilcrowSignWidth, AFontInfo.LineSpacing);
end;

procedure TdxBoxMeasurer.MeasureSectionMark(ABoxInfo: TdxBoxInfo);
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := ABoxInfo.GetFontInfo(PieceTable);
  ABoxInfo.Size := cxSize(AFontInfo.PilcrowSignWidth, AFontInfo.LineSpacing);
end;

procedure TdxBoxMeasurer.MeasureSingleSpace(ABoxInfo: TdxBoxInfo);
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := ABoxInfo.GetFontInfo(PieceTable);
  ABoxInfo.Size := cxSize(AFontInfo.SpaceWidth, AFontInfo.LineSpacing);
end;

procedure TdxBoxMeasurer.MeasureSpaces(ABoxInfo: TdxBoxInfo);
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := ABoxInfo.GetFontInfo(PieceTable);
  ABoxInfo.Size := cxSize(AFontInfo.SpaceWidth * (ABoxInfo.EndPos.Offset - ABoxInfo.StartPos.Offset + 1), AFontInfo.LineSpacing);
end;

procedure TdxBoxMeasurer.MeasureTab(ABoxInfo: TdxBoxInfo);
begin
  ABoxInfo.Size := cxSize(0, ABoxInfo.GetFontInfo(PieceTable).LineSpacing);
end;

procedure TdxBoxMeasurer.MeasureText(ABoxInfo: TdxBoxInfo);
var
  ARun: TdxTextRunBase;
begin
  ARun := TdxTextRunBase(PieceTable.Runs[ABoxInfo.StartPos.RunIndex]);
  ARun.Measure(ABoxInfo, Self);
end;

procedure TdxBoxMeasurer.OnLayoutUnitChanged;
begin
  FTextViewInfoCache.Clear;
end;

procedure TdxBoxMeasurer.SetPieceTable(const Value: TdxCustomPieceTable);
begin
  Assert(Value.DocumentModel = FDocumentModel);
  FPieceTable := Value;
end;

function TdxBoxMeasurer.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer): Boolean;
begin
  Result := False;
end;

function TdxBoxMeasurer.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; const AText: string; AFontInfo: TdxFontInfo; AMaxWidth: Integer): Boolean;
begin
  Result := False;
end;

end.
