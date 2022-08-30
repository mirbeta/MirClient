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

unit dxRichEdit.DocumentModel.Boxes;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxGenerics,
  dxRichEdit.Utils.TextColors,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.PieceTableRange,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentLayout.UnitConverter;

type
  TdxRow = class;
  TdxDetailRow = class;
  TdxPage = class;
  TdxPageArea = class;
  TdxColumn = class;
  TdxLayoutDependentTextBox = class;
  TdxTabSpaceBox = class;
  TdxNumberingListBox = class;
  TdxPageBreakBox = class;
  TdxColumnBreakBox = class;
  TdxSectionMarkBox = class;
  TdxLineNumberBox = class;
  TdxTableBorderViewInfoBase = class;
  TdxTableCellViewInfo = class;
  TdxTableViewInfo = class;
  TdxCornerViewInfoBase = class;
  TdxTableRowViewInfoBase = class;
  TdxCustomMarkBox = class;
  TdxFloatingObjectBox = class;
  TdxParagraphFrameBox = class;
  TdxHeaderPageArea = class;
  TdxFooterPageArea = class;
  TdxFootNotePageAreaList = class;
  TdxFootNotePageArea = class;
  TdxPageCollection = class;
  TdxRowCollection = class;
  TdxColumnCollection = class;
  TdxPageAreaCollection = class;
  TdxLineNumberBoxCollection = class;
  TdxTableViewInfoCollection = class;
  TdxTableCellVerticalAnchor = class;
  TdxTableRowViewInfoWithCellSpacing = class;

  TdxExportPageAction = procedure (APage: TdxPage) of object;

  { IdxDocumentLayoutExporter }

  IdxDocumentLayoutExporter = interface(IdxSimpleDocumentLayoutExporter)
  ['{98C913CD-C8DE-46B9-B799-0003D135DAC6}']
    procedure ExportPage(APage: TdxPage);
    procedure ExportPageArea(APageArea: TdxPageArea);
    procedure ExportColumn(AColumn: TdxColumn);
    procedure ExportRow(ARow: TdxRow);
    procedure ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox);
    procedure ExportTabSpaceBox(ABox: TdxTabSpaceBox);
    procedure ExportNumberingListBox(ABox: TdxNumberingListBox);
    procedure ExportPageBreakBox(ABox: TdxPageBreakBox);
    procedure ExportColumnBreakBox(ABox: TdxColumnBreakBox);
    procedure ExportSectionMarkBox(ABox: TdxSectionMarkBox);
    procedure ExportLineNumberBox(ABox: TdxLineNumberBox);
    procedure ExportBookmarkStartBox(ABox: TdxVisitableDocumentIntervalBox);
    procedure ExportBookmarkEndBox(ABox: TdxVisitableDocumentIntervalBox);
    procedure ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect);
    procedure ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X, Y: Integer);
    procedure ExportTableCell(ACell: TdxTableCellViewInfo);
    procedure ExportTableRow(ARow: TdxTableRowViewInfoBase);
    procedure ExportCustomMarkBox(ABox: TdxCustomMarkBox);
    procedure ExportFloatingObjectBox(ABox: TdxFloatingObjectBox);
    procedure ExportParagraphFrameBox(ABox: TdxParagraphFrameBox);
    function IsAnchorVisible(const AAnchor: TdxTableCellVerticalAnchor): Boolean;
    function IsTableRowVisible(ARow: TdxTableRowViewInfoBase): Boolean;
  end;

  { TdxCounterItem }

  TdxCounterItem = class sealed
  strict private
    FLogPosition: TdxDocumentLogPosition;
    FValue: Integer;
  public
    constructor Create(ALogPosition: TdxDocumentLogPosition; AValue: Integer);

    property LogPosition: TdxDocumentLogPosition read FLogPosition;
    property Value: Integer read FValue;
  end;
  TdxCounterItemList = class(TdxList<TdxCounterItem>);

  { TdxCounter }

  TdxCounter = class sealed
  strict private
    FItems: TdxCounterItemList;
    FLastValue: Integer;
    function GetHasItems: Boolean;
    function GetLastItem: TdxCounterItem;
  protected
    property InnerItems: TdxCounterItemList read FItems;
    property HasItems: Boolean read GetHasItems;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure ResetFrom(ALogPosition: TdxDocumentLogPosition);
    function Increment(ALogPosition: TdxDocumentLogPosition): Integer;

    property LastItem: TdxCounterItem read GetLastItem;
    property LastValue: Integer read FLastValue write FLastValue;
  end;

  { TdxCounters }

  TdxCounters = class sealed
  private
    FItems: TdxNamedObjectDictionary<TdxCounter>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Reset;
    procedure ResetFrom(ALogPosition: TdxDocumentLogPosition);
    function RegisterCounter(const AId: string): TdxCounter;
    procedure UnregisterCounter(const AId: string);
    function GetCounter(const AId: string): TdxCounter;
  end;

  { TdxDocumentLayout }

  TdxDocumentLayout = class sealed
  strict private
    FDocumentModel: TdxCustomDocumentModel;
    FMeasurerProvider: IdxBoxMeasurerProvider;
    FPages: TdxPageCollection;
    FCounters: TdxCounters;
    FFirstVisiblePageIndex: Integer;
    FLastVisiblePageIndex: Integer;
    FOnAfterCreateDetailRow: TdxNotifyEventHandler;
    FOnBeforeCreateDetailRow: TdxNotifyEventHandler;
    function GetUnitConverter: TdxDocumentLayoutUnitConverter;
    function GetMeasurer: TdxBoxMeasurer;
  protected
    function CreateDetailRowCore(ARow: TdxRow): TdxDetailRow;
    function CreateDetailRowForBoxCore(ARow: TdxRow; ABox: TdxBox): TdxDetailRow;

    procedure DoAfterCreateDetailRow;
    procedure DoBeforeCreateDetailRow;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AMeasurerProvider: IdxBoxMeasurerProvider);
    destructor Destroy; override;
    function CreateLayoutPosition(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition;
      APreferredPageIndex: Integer): TObject;
    function CreateDetailRow(ARow: TdxRow): TdxDetailRow;
    function CreateDetailRowForBox(ARow: TdxRow; ABox: TdxBox; ASuppressSuspendFormatting: Boolean): TdxDetailRow;

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property UnitConverter: TdxDocumentLayoutUnitConverter read GetUnitConverter;
    property MeasurerProvider: IdxBoxMeasurerProvider read FMeasurerProvider;
    property Measurer: TdxBoxMeasurer read GetMeasurer;
    property Pages: TdxPageCollection read FPages;
    property Counters: TdxCounters read FCounters;
    property FirstVisiblePageIndex: Integer read FFirstVisiblePageIndex write FFirstVisiblePageIndex;
    property LastVisiblePageIndex: Integer read FLastVisiblePageIndex write FLastVisiblePageIndex;
    property OnAfterCreateDetailRow: TdxNotifyEventHandler read FOnAfterCreateDetailRow;
    property OnBeforeCreateDetailRow: TdxNotifyEventHandler read FOnBeforeCreateDetailRow;
  end;

  { TdxTabSpaceBox }

  TdxTabSpaceBox = class sealed(TdxSingleCharacterMarkBox)
  strict private
    FLeaderCount: Integer;
    FTabInfo: TdxTabInfo;
  protected
    function GetMarkCharacter: Char; override;
    function IsTabSpaceBox: Boolean; override;
  public
    constructor Create;
    class function CreateBox: TdxBox; override;
    function IsVisible: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsLineBreak: Boolean; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;

    property LeaderCount: Integer read FLeaderCount write FLeaderCount;
    property TabInfo: TdxTabInfo read FTabInfo write FTabInfo;
  end;

  { TdxFloatingObjectBox }

  TdxFloatingObjectBox = class(TdxSinglePositionBox)
  strict private class var
    FZOrderComparer: TdxBoxBaseComparer;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FPieceTable: TdxCustomPieceTable;
    FLockPosition: Boolean;
    FWasRestart: Boolean;
    FCanPutTextAtLeft: Boolean;
    FCanPutTextAtRight: Boolean;
    FPutTextAtLargestSide: Boolean;
    FExtendedBounds: TRect;
    FContentBounds: TRect;
    FActualSizeBounds: TRect;
    FDocumentLayout: TdxDocumentLayout;
    function GetX: Integer;
    function GetY: Integer;
    procedure SetDocumentLayout(const Value: TdxDocumentLayout);
    procedure SetX(const Value: Integer);
    procedure SetY(const Value: Integer);
  protected
    function GetActualSizeBounds: TRect; override;
  public
    class function CreateBox: TdxBox; override;
    destructor Destroy; override;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;
    procedure IncreaseHeight(ADelta: Integer);
    function CalcDescent(APieceTable: TdxCustomPieceTable): Integer; override;
    function GetFloatingObjectRun: TdxFloatingObjectAnchorRun;
    function GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase; override;
    function CalcAscentAndFree(APieceTable: TdxCustomPieceTable): Integer; override;
    function CalcBaseAscentAndFree(APieceTable: TdxCustomPieceTable): Integer; override;
    function CalcBaseDescent(APieceTable: TdxCustomPieceTable): Integer; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    function GetTextBoxContentBounds: TRect;
    class function CreateTransformUnsafe(const AAngle: Single; const ABounds: TRect): TdxTransformMatrix; overload;
    function CreateBackwardTransformUnsafe: TdxTransformMatrix;
    function CreateTransformUnsafe: TdxTransformMatrix; overload;
    function TransformPointBackward(APoint: TPoint): TPoint;
    procedure MoveLayoutVertically(ADeltaY: Integer);
    procedure MoveVertically(ADeltaY: Integer); override; final;
    procedure MoveVerticallyCore(ADeltaY: Integer);
    procedure SetActualSizeBounds(const ABounds: TRect);

    class property ZOrderComparer: TdxBoxBaseComparer read FZOrderComparer;
    property LockPosition: Boolean read FLockPosition write FLockPosition;
    property WasRestart: Boolean read FWasRestart write FWasRestart;
    property CanPutTextAtLeft: Boolean read FCanPutTextAtLeft write FCanPutTextAtLeft;
    property CanPutTextAtRight: Boolean read FCanPutTextAtRight write FCanPutTextAtRight;
    property PutTextAtLargestSide: Boolean read FPutTextAtLargestSide write FPutTextAtLargestSide;
    property ExtendedBounds: TRect read FExtendedBounds write FExtendedBounds;
    property ContentBounds: TRect read FContentBounds write FContentBounds;
    property PieceTable: TdxCustomPieceTable read FPieceTable write FPieceTable;
    property DocumentLayout: TdxDocumentLayout read FDocumentLayout write SetDocumentLayout;
    property X: Integer read GetX write SetX;
    property Y: Integer read GetY write SetY;
  end;

  TdxFloatingObjectBoxList = class sealed(TdxBoxList)
  strict private
    function GetItem(Index: Integer): TdxFloatingObjectBox;
  public
    property Items[Index: Integer]: TdxFloatingObjectBox read GetItem; default;
  end;

  { TdxParagraphFrameBox }

  TdxParagraphFrameBox = class sealed(TdxSinglePositionBox)
  strict private class var
    FIndexComparer: TdxBoxBaseComparer;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FCanPutTextAtLeft: Boolean;
    FCanPutTextAtRight: Boolean;
    FPutTextAtLargestSide: Boolean;
    FLockPosition: Boolean;
    FExtendedBounds: TRect;
    FContentBounds: TRect;
    FActualSizeBounds: TRect;
    FActualBounds: TRect;
    FShapeBounds: TRect;
    FPieceTable: TdxSimplePieceTable;
    FDocumentLayout: TdxDocumentLayout;
    FFirstRow: TdxRow;
    FRowCollection: TdxRowCollection;
    FParagraphIndex: TdxParagraphIndex;
    FLastParagraphIndex: TdxParagraphIndex;
    FWasRestart: Boolean;
  protected
    function GetActualSizeBounds: TRect; override;
    function GetX: Integer;
    function GetY: Integer;
    procedure SetX(Value: Integer);
    procedure SetY(Value: Integer);
    function ShouldMoveVertically: Boolean;
  public
    constructor Create(AParagraph: TdxSimpleParagraph); overload;
    constructor Create(APieceTable: TdxSimplePieceTable; AParagraphIndex: TdxParagraphIndex); overload;
    destructor Destroy; override;

    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;

    function CreateBox: TdxBox; reintroduce; virtual;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    class function CreateTransformUnsafe(AAngle: Single; const ABounds: TRect): TdxTransformMatrix; overload;
    function CreateBackwardTransformUnsafe: TdxTransformMatrix;
    function CreateTransformUnsafe: TdxTransformMatrix; overload;
    function GetParagraph: TdxSimpleParagraph;
    procedure MoveVertically(ADeltaY: Integer); override; final;
    function TransformPointBackward(const APoint: TPoint): TPoint;
    procedure SetActualSizeBounds(const ABounds: TRect);
    function GetFrameProperties: TdxMergedFrameProperties;

    function HasFrameProperties: Boolean;
    function IsInCell: Boolean;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;

    property ActualBounds: TRect read FActualBounds write FActualBounds;
    property CanPutTextAtLeft: Boolean read FCanPutTextAtLeft write FCanPutTextAtLeft;
    property CanPutTextAtRight: Boolean read FCanPutTextAtRight write FCanPutTextAtRight;
    property ContentBounds: TRect read FContentBounds write FContentBounds;
    property DocumentLayout: TdxDocumentLayout read FDocumentLayout write FDocumentLayout;
    property ExtendedBounds: TRect read FExtendedBounds write FExtendedBounds;
    property FirstRow: TdxRow read FFirstRow write FFirstRow;
    class property IndexComparer: TdxBoxBaseComparer read FIndexComparer;
    property LockPosition: Boolean read FLockPosition write FLockPosition;
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
    property LastParagraphIndex: TdxParagraphIndex read FLastParagraphIndex write FLastParagraphIndex;
    property PieceTable: TdxSimplePieceTable read FPieceTable write FPieceTable;
    property PutTextAtLargestSide: Boolean read FPutTextAtLargestSide write FPutTextAtLargestSide;
    property RowCollection: TdxRowCollection read FRowCollection;
    property ShapeBounds: TRect read FShapeBounds write FShapeBounds;
    property WasRestart: Boolean read FWasRestart write FWasRestart;
    property X: Integer read GetX write SetX;
    property Y: Integer read GetY write SetY;
  end;

  { TdxParagraphFrameBoxList }

  TdxParagraphFrameBoxList = class sealed(TdxBoxList)
  strict private
    function GetItem(Index: Integer): TdxParagraphFrameBox;
  public
    property Items[Index: Integer]: TdxParagraphFrameBox read GetItem; default;
  end;

  { TdxPage }

  TdxPage = class sealed(TdxNoPositionBox)
  strict private
    FClientBounds: TRect;
    FCommentBounds: TRect;
    FAreas: TdxPageAreaCollection;
    FPageIndex: Integer;
    FPrimaryFormattingComplete: Boolean;
    FSecondaryFormattingComplete: Boolean;
    FCheckSpellingComplete: Boolean;
    FHeader: TdxHeaderPageArea;
    FFooter: TdxFooterPageArea;
    FPageOrdinal: Integer;
    FNumPages: Integer;
    FNumSkippedPages: Integer;
    FInnerFloatingObjects: TdxFloatingObjectBoxList;
    FInnerForegroundFloatingObjects: TdxFloatingObjectBoxList;
    FInnerBackgroundFloatingObjects: TdxFloatingObjectBoxList;
    FInnerParagraphFrames: TdxParagraphFrameBoxList;
    FInnerFootNotes: TdxFootNotePageAreaList;
    FInnerComments: TdxCommentViewInfoList;
    FPageNumberSource: TdxPage;

    function GetFootNotes: TdxFootNotePageAreaList;
    function GetFloatingObjects: TdxFloatingObjectBoxList;
    function GetForegroundFloatingObjects: TdxFloatingObjectBoxList;
    function GetBackgroundFloatingObjects: TdxFloatingObjectBoxList;
    function GetParagraphFramesProperty: TdxParagraphFrameBoxList;
  private
    function GetPageNumberSource: TdxPage;
    procedure SetFooter(const Value: TdxFooterPageArea);
    procedure SetHeader(const Value: TdxHeaderPageArea);
    function FindFloatingObject(AObjects: TdxFloatingObjectBoxList; ARun: TdxFloatingObjectAnchorRun): TdxFloatingObjectBox; overload;
  protected
    function GetDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function GetHitTestAccuracy: TdxHitTestAccuracy; override;

    property FootNotes: TdxFootNotePageAreaList read GetFootNotes;
  public
    constructor Create(APageNumberSource: TdxPage = nil);
    destructor Destroy; override;
    class function CreateBox: TdxBox; override;
    function IsVisible: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsLineBreak: Boolean; override;
    function IsEmpty: Boolean;
    function IsEven: Boolean;
    procedure ClearFloatingObjects; overload;
    procedure ClearInvalidatedContent(ARunIndex: TdxRunIndex; APieceTable: TdxCustomPieceTable);
    procedure ClearFloatingObjects(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex; AObjects: TdxFloatingObjectBoxList); overload;
    procedure ClearFootNotes(ARunIndex: TdxRunIndex);
    procedure ClearComments(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex);
    procedure ClearParagraphFrames; overload;
    procedure ClearParagraphFrames(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex; AItems: TdxParagraphFrameBoxList); overload;
    function FindFloatingObject(ARun: TdxFloatingObjectAnchorRun): TdxFloatingObjectBox; overload;
    function GetFirstFormatterPosition: TdxFormatterPosition; override; final;
    function GetLastFormatterPosition: TdxFormatterPosition; override; final;
    function GetActiveFirstArea(APieceTable: TdxCustomPieceTable): TdxPageArea;
    function GetActiveLastArea(APieceTable: TdxCustomPieceTable): TdxPageArea;
    function GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override; final;
    function GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override; final;

    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override; final;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    procedure MoveVertically(ADeltaY: Integer); override; final;
    function GetText(ATable: TdxCustomPieceTable): string; override;
    function GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase; override;
    function GetLastColumn: TdxColumn;
    function GetNonBackgroundFloatingObjects: TdxFloatingObjectBoxList;
    function GetSortedNonBackgroundFloatingObjects: TdxFloatingObjectBoxList;
    function GetParagraphFrames: TdxParagraphFrameBoxList;
    function GetSortedParagraphFrames: TdxParagraphFrameBoxList; overload;

    property PrimaryFormattingComplete: Boolean read FPrimaryFormattingComplete write FPrimaryFormattingComplete;
    property SecondaryFormattingComplete: Boolean read FSecondaryFormattingComplete write FSecondaryFormattingComplete;
    property CheckSpellingComplete: Boolean read FCheckSpellingComplete write FCheckSpellingComplete;
    property PageIndex: Integer read FPageIndex write FPageIndex;
    property NumPages: Integer read FNumPages write FNumPages;
    property NumSkippedPages: Integer read FNumSkippedPages write FNumSkippedPages;
    property PageOrdinal: Integer read FPageOrdinal write FPageOrdinal;
    property Areas: TdxPageAreaCollection read FAreas;
    property PageNumberSource: TdxPage read GetPageNumberSource;
    property ClientBounds: TRect read FClientBounds write FClientBounds;
    property CommentBounds: TRect read FCommentBounds write FCommentBounds;
    property Header: TdxHeaderPageArea read FHeader write SetHeader;
    property Footer: TdxFooterPageArea read FFooter write SetFooter;
    property InnerFloatingObjects: TdxFloatingObjectBoxList read FInnerFloatingObjects;
    property FloatingObjects: TdxFloatingObjectBoxList read GetFloatingObjects;
    property InnerForegroundFloatingObjects: TdxFloatingObjectBoxList read FInnerForegroundFloatingObjects;
    property ForegroundFloatingObjects: TdxFloatingObjectBoxList read GetForegroundFloatingObjects;
    property InnerBackgroundFloatingObjects: TdxFloatingObjectBoxList read FInnerBackgroundFloatingObjects;
    property BackgroundFloatingObjects: TdxFloatingObjectBoxList read GetBackgroundFloatingObjects;
    property InnerParagraphFrames: TdxParagraphFrameBoxList read FInnerParagraphFrames;
    property ParagraphFrames: TdxParagraphFrameBoxList read GetParagraphFramesProperty;
    property InnerFootNotes: TdxFootNotePageAreaList read FInnerFootNotes;
    property InnerComments: TdxCommentViewInfoList read FInnerComments;
  end;

  { TdxPageCollection }

  TdxPageCollection = class sealed(TdxBoxCollectionBase<TdxPage>)
  public
    procedure RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox); override;
    procedure RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator); override;
  end;

  { TdxPageArea }

  TdxPageArea = class(TdxNoPositionBox)
  strict private
    FColumns: TdxColumnCollection;
    FLineNumbers: TdxLineNumberBoxCollection;
    FSection: TdxSection;
    FPieceTable: TdxContentTypeBase;
    function GetIsEmpty: Boolean;
    function GetPieceTable: TdxCustomPieceTable;
  protected
    function GetDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function GetHitTestAccuracy: TdxHitTestAccuracy; override;
  public
    constructor Create(APieceTable: TdxContentTypeBase; ASection: TdxSection);
    destructor Destroy; override;
    function GetFirstFormatterPosition: TdxFormatterPosition; override;
    function GetLastFormatterPosition: TdxFormatterPosition; override;
    function GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override;
    function GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override;
    class function CreateBox: TdxBox; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override; final;
    procedure MoveVertically(ADeltaY: Integer); override;
    function GetText(ATable: TdxCustomPieceTable): string; override;
    function GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase; override;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;

    property Columns: TdxColumnCollection read FColumns;
    property LineNumbers: TdxLineNumberBoxCollection read FLineNumbers;
    property IsEmpty: Boolean read GetIsEmpty;
    property PieceTable: TdxCustomPieceTable read GetPieceTable;
    property Section: TdxSection read FSection;
  end;

  { TdxPageAreaCollection }

  TdxPageAreaCollection = class sealed(TdxBoxCollectionBase<TdxPageArea>)
  public
    procedure RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox); override; final;
    procedure RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator); override; final;
  end;

  { TdxFootNotePageArea }

  TdxFootNotePageArea = class sealed(TdxPageArea)
  strict private
    FReferenceRunIndex: TdxRunIndex;
    function GetNote: TdxFootNote;
  public
    constructor Create(AFootNote: TdxFootNote; ASection: TdxSection; AReferenceRunIndex: TdxRunIndex);

    property Note: TdxFootNote read GetNote;
    property ReferenceRunIndex: TdxRunIndex read FReferenceRunIndex;
  end;

  TdxFootNotePageAreaList = class(TdxBoxList)
  private
    function GetItem(Index: Integer): TdxFootNotePageArea;
  public
    property Items[Index: Integer]: TdxFootNotePageArea read GetItem; default;
  end;

  { TdxHeaderFooterPageAreaBase }

  TdxHeaderFooterPageAreaBase = class abstract(TdxPageArea)
  strict private
    FContentBounds: TRect;
  public
    property ContentBounds: TRect read FContentBounds write FContentBounds;
  end;

  { TdxHeaderPageArea }

  TdxHeaderPageArea = class sealed(TdxHeaderFooterPageAreaBase)
  strict private
    function GetHeader: TdxSectionHeader;
  public
    property Header: TdxSectionHeader read GetHeader;
  end;

  { TdxFooterPageArea }

  TdxFooterPageArea = class sealed(TdxHeaderFooterPageAreaBase)
  strict private
    function GetFooter: TdxSectionFooter;
  public
    property Footer: TdxSectionFooter read GetFooter;
  end;

  { TdxParagraphFrameBoxCollection }

  TdxParagraphFrameBoxCollection = class sealed(TdxObjectList<TdxParagraphFrameBox>)
  public
    procedure ClearInvalidatedParagraphFrames(AFrom, ATo: TdxParagraphIndex);
    procedure ExportTo(const AExporter: IdxDocumentLayoutExporter);
    procedure MoveVertically(ADeltaY: TdxLayoutUnit);
  end;

  { TdxColumnExtendedBoxes }

  TdxColumnExtendedBoxes = class sealed
  strict private
    FTables: TdxTableViewInfoCollection;
    FParagraphFrames: TdxParagraphFrameBoxCollection;
    function GetTables: TdxTableViewInfoCollection;
    function GetParagraphFrames: TdxParagraphFrameBoxCollection;
  public
    destructor Destroy; override;

    property InnerTables: TdxTableViewInfoCollection read FTables;
    property Tables: TdxTableViewInfoCollection read GetTables;
    property InnerParagraphFrames: TdxParagraphFrameBoxCollection read FParagraphFrames;
    property ParagraphFrames: TdxParagraphFrameBoxCollection read GetParagraphFrames;
  end;

  { TdxColumn }

  TdxColumn = class(TdxNoPositionBox)
  strict private
    FChildren: TdxBoxList;
    FRows: TdxRowCollection;
    FExtendedBoxes: TdxColumnExtendedBoxes;
    function GetExtendedBoxes: TdxColumnExtendedBoxes;
    function GetInnerTables: TdxTableViewInfoCollection;
    function GetShouldProcessTables: Boolean;
    function GetParagraphFrames: TdxParagraphFrameBoxCollection;
    function GetShouldProcessParagraphFrames: Boolean;
    function GetInnerParagraphFrames: TdxParagraphFrameBoxCollection;
    function GetIsEmpty: Boolean;
    function GetTables: TdxTableViewInfoCollection;
  protected
    function ContainsParagraphFrame(AParagraph: TdxParagraphBase): Boolean;
    function GetRows: TdxRowCollection; virtual;
    function GetTopLevelColumn: TdxColumn; virtual;
    function GetDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function GetHitTestAccuracy: TdxHitTestAccuracy; override;
    property Children: TdxBoxList read FChildren;
  public
    constructor Create;
    destructor Destroy; override;

    class function CreateBox: TdxBox; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    function GetFirstFormatterPosition: TdxFormatterPosition; override;
    function GetLastFormatterPosition: TdxFormatterPosition; override;
    function GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override;
    function GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override;
    function IsIntersectedWithPrevColumn: Boolean;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;
    procedure MoveVertically(ADeltaY: Integer); override;
    procedure RemoveTableViewInfoWithContent(ATableViewInfo: TdxTableViewInfo);
    procedure RemoveTableCellViewInfoContent(ACell: TdxTableCellViewInfo);
    function GetText(ATable: TdxCustomPieceTable): string; override;
    function GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase; override;
    function GetOwnRows: TdxRowCollection; virtual;
    procedure AddParagraphFrame(AParagraph: TdxSimpleParagraph); virtual;
    procedure ClearInvalidatedParagraphFrames(AFrom: TdxParagraphIndex);
    procedure ClearInvalidatedParagraphFramesCore(AFrom, ATo: TdxParagraphIndex);
    procedure RemoveEmptyTableViewInfos;

    property Rows: TdxRowCollection read GetRows;
    property InnerExtendedBoxes: TdxColumnExtendedBoxes read FExtendedBoxes;
    property ExtendedBoxes: TdxColumnExtendedBoxes read GetExtendedBoxes;
    property ShouldProcessTables: Boolean read GetShouldProcessTables;
    property InnerTables: TdxTableViewInfoCollection read GetInnerTables;
    property ParagraphFrames: TdxParagraphFrameBoxCollection read GetParagraphFrames;
    property ShouldProcessParagraphFrames: Boolean read GetShouldProcessParagraphFrames;
    property InnerParagraphFrames: TdxParagraphFrameBoxCollection read GetInnerParagraphFrames;
    property Tables: TdxTableViewInfoCollection read GetTables;
    property IsEmpty: Boolean read GetIsEmpty;
    property TopLevelColumn: TdxColumn read GetTopLevelColumn;
  end;

  { TdxColumnCollection }

  TdxColumnCollection = class sealed(TdxBoxCollectionBase<TdxColumn>)
  public
    procedure RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator); override;
    procedure RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox); override;
  end;

  { TdxFloatingObjectAnchorBox }

  TdxFloatingObjectAnchorBox = class sealed(TdxSinglePositionBox)
  strict private
    FActualBounds: TRect;
    FShapeBounds: TRect;
    FContentBounds: TRect;
    FActualSizeBounds: TRect;
    FAnchorRun: TdxFloatingObjectAnchorRun;
    FFloatingObjectBox: TdxFloatingObjectBox;
  protected
    function GetActualSizeBounds: TRect; override;
  public
    class function CreateBox: TdxBox; override;

    function IsVisible: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsLineBreak: Boolean; override;

    procedure IncreaseHeight(ADelta: Integer);
    function CalcDescent(APieceTable: TdxCustomPieceTable): Integer; override;
    function GetFloatingObjectRun(APieceTable: TdxCustomPieceTable): TdxFloatingObjectAnchorRun;
    procedure SetFloatingObjectRun(AAnchorRun: TdxFloatingObjectAnchorRun);
    function CalcAscentAndFree(APieceTable: TdxCustomPieceTable): Integer; override;
    function CalcBaseAscentAndFree(APieceTable: TdxCustomPieceTable): Integer; override;
    function CalcBaseDescent(APieceTable: TdxCustomPieceTable): Integer; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    procedure SetActualSizeBounds(const ABounds: TRect);

    property ActualBounds: TRect read FActualBounds write FActualBounds;
    property ContentBounds: TRect read FContentBounds write FContentBounds;
    property FloatingObjectBox: TdxFloatingObjectBox read FFloatingObjectBox write FFloatingObjectBox;
    property ShapeBounds: TRect read FShapeBounds write FShapeBounds;
  end;

  { TdxSectionMarkBox }

  TdxSectionMarkBox = class sealed(TdxMultiPositionBox)
  protected
    function IsSectionMarkBox: Boolean; override;
  public
    class function CreateBox: TdxBox; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;
  end;

  { TdxLayoutDependentTextBox }

  TdxLayoutDependentTextBox = class(TdxTextBox)
  strict private
    FCalculatedText: string;
  public
    class function CreateBox: TdxBox; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    function GetText(ATable: TdxCustomPieceTable): string; override;

    property CalculatedText: string read FCalculatedText write FCalculatedText;
  end;

  { TdxNumberingListBoxInfo }

  TdxNumberingListBoxInfo = class(TdxBoxInfo)
  protected
    function GetForceUpdateCurrentRowHeight: Boolean; override;
  public
    function GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo; override;

    property ForceUpdateCurrentRowHeight: Boolean read GetForceUpdateCurrentRowHeight;
  end;

  { TdxNumberingListBox }

  TdxNumberingListBox = class(TdxMultiPositionBox)
  strict private
    FInitialBounds: TRect;
    FNumberingListText: string;
  public
    class function CreateBox: TdxBox; override;
    function GetNumerationCharacterProperties(APieceTable: TdxCustomPieceTable): TdxMergedCharacterProperties;

    function IsLineBreak: Boolean; override;
    function IsVisible: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;

    function GetDocumentPosition(APieceTable: TdxCustomPieceTable; const APos: TdxFormatterPosition): TdxDocumentModelPosition; override;
    function GetFirstFormatterPosition: TdxFormatterPosition; override;
    function GetLastFormatterPosition: TdxFormatterPosition; override;
    function GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override;
    function GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;

    function GetText(ATable: TdxCustomPieceTable): string; override;
    function GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo; override;
    function GetActualForeColor(APieceTable: TdxCustomPieceTable; ATextColors: TdxTextColors; ABackColor: TdxAlphaColor): TdxAlphaColor; override;
    function GetUnderlineColorCore(APieceTable: TdxCustomPieceTable): TdxAlphaColor; override;
    function GetStrikeoutColorCore(APieceTable: TdxCustomPieceTable): TdxAlphaColor; override;
    function GetFontStrikeoutType(APieceTable: TdxCustomPieceTable): TdxStrikeoutType; override;
    function GetFontUnderlineType(APieceTable: TdxCustomPieceTable): TdxUnderlineType; override;

    function IsHyperlinkSupported: Boolean; override;

    property InitialBounds: TRect read FInitialBounds write FInitialBounds;
    property NumberingListText: string read FNumberingListText write FNumberingListText;
  end;

  { TdxNumberingListBoxWithSeparator }

  TdxNumberingListBoxWithSeparator = class(TdxNumberingListBox)
  strict private
    FSeparatorBox: TdxBox;
    procedure SetSeparatorBox(const Value: TdxBox);
  public
    destructor Destroy; override;
    procedure OffsetRunIndices(ADelta: Integer); override;

    property SeparatorBox: TdxBox read FSeparatorBox write SetSeparatorBox;
  end;

  { TdxPageBreakBox }

  TdxPageBreakBox = class sealed(TdxMultiPositionBox)
  protected
    function IsPageBreakBox: Boolean; override;
  public
    class function CreateBox: TdxBox; override;

    function IsSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;

    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
  end;

  { TdxColumnBreakBox }

  TdxColumnBreakBox = class sealed(TdxMultiPositionBox)
  protected
    function IsColumnBreakBox: Boolean; override; final;
  public
    class function CreateBox: TdxBox; override;
    function IsVisible: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsLineBreak: Boolean; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
  end;

  { TdxExactPageAndLogPositionComparable }

  TdxExactPageAndLogPositionComparable = class(TdxBoxLogPositionComparable)
  public
    function CompareTo(const Value: TdxBoxBase): Integer; override;
  end;

  { TdxExactPageAreaAndLogPositionComparable }

  TdxExactPageAreaAndLogPositionComparable = class(TdxBoxLogPositionComparable)
  public
    function CompareTo(const Value: TdxBoxBase): Integer; override;
  end;

  { TdxExactColumnAndLogPositionComparable }

  TdxExactColumnAndLogPositionComparable = class(TdxBoxLogPositionComparable)
  strict private
    function FindTable(ATables: TdxTableViewInfoCollection; ACell: TdxTableCell;
      out ACellViewInfo: TdxTableCellViewInfo): TdxTableViewInfo;
  public
    function CompareTo(const Value: TdxBoxBase): Integer; override;
  end;

  { TdxParagraphBoxCollection }

  TdxParagraphBoxCollection = class(TdxSimpleParagraphBoxCollection)
  strict private
    FContainsLayoutDependentBox: Boolean;
    FContainsFloatingObjectAnchorBox: Boolean;
    FNumberingListBox: TdxNumberingListBox;
    procedure SetNumberingListBox(const Value: TdxNumberingListBox);
  protected
    function GetIsValid: Boolean; override;
  public
    destructor Destroy; override;

    procedure Add(ABox: TdxBox); override;
    procedure Clear; override;
    function ContainsFloatingObjectAnchorBox: Boolean; override;
    procedure InvalidateBoxes; override;
    procedure OffsetRunIndices(ADelta: Integer); override;

    property NumberingListBox: TdxNumberingListBox read FNumberingListBox write SetNumberingListBox;
  end;

  { TdxCustomMarkBox }

  TdxCustomMarkBox = class(TdxReferencedObject)
  strict private
    FCustomMark: TObject;
    FBounds: TRect;
  public
    procedure MoveVertically(ADeltaY: Integer);
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); virtual;

    property CustomMark: TObject read FCustomMark write FCustomMark;
    property Bounds: TRect read FBounds write FBounds;
  end;

  { TdxCustomMarkBoxCollection }

  TdxCustomMarkBoxCollection = class(TdxReferencedObjectList<TdxCustomMarkBox>)
  public
    procedure MoveVertically(ADeltaY: Integer);
  end;

  { TdxLineNumberBox }

  TdxLineNumberBox = class(TdxMultiPositionBox)
  strict private
    FRow: TdxRow;
    FRun: TdxLineNumberCommonRun;
    FText: string;
  public
    constructor Create(ARun: TdxLineNumberCommonRun; ARow: TdxRow; const AText: string);
    function CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager; override;
    class function CreateBox: TdxBox; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
    function GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase; override;
    function GetText(ATable: TdxCustomPieceTable): string; override;
    function IsLineBreak: Boolean; override;
    function IsNotWhiteSpaceBox: Boolean; override;
    function IsVisible: Boolean; override;

    property Row: TdxRow read FRow;
  end;

  { TdxLineNumberBoxCollection }

  TdxLineNumberBoxCollection = class(TdxBoxCollectionBase<TdxLineNumberBox>)
  public
    procedure RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox); override; final;
    procedure RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator); override; final;
  end;

  { TdxRowExtendedBoxes }

  TdxRowExtendedBoxes = class(TdxSimpleRowExtendedBoxes)
  strict private
    FBookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection;
    FCommentBoxes: TdxVisitableDocumentIntervalBoxCollection;
    FCommentHighlightAreas: TdxHighlightAreaCollection;
    FCustomMarkBoxes: TdxCustomMarkBoxCollection;
    FFieldHighlightAreas: TdxHighlightAreaCollection;
    FRangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection;
    FRangePermissionHighlightAreas: TdxHighlightAreaCollection;
    function GetBookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection;
    function GetCommentBoxes: TdxVisitableDocumentIntervalBoxCollection;
    function GetCommentHighlightAreas: TdxHighlightAreaCollection;
    function GetCustomMarkBoxes: TdxCustomMarkBoxCollection;
    function GetFieldHighlightAreas: TdxHighlightAreaCollection;
    function GetRangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection;
    function GetRangePermissionHighlightAreas: TdxHighlightAreaCollection;
  protected
    property InnerBookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection read FBookmarkBoxes;
    property InnerCommentBoxes: TdxVisitableDocumentIntervalBoxCollection read FCommentBoxes;
    property InnerCommentHighlightAreas: TdxHighlightAreaCollection read FCommentHighlightAreas;
    property InnerCustomMarkBoxes: TdxCustomMarkBoxCollection read FCustomMarkBoxes;
    property InnerFieldHighlightAreas: TdxHighlightAreaCollection read FFieldHighlightAreas;
    property InnerRangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection read FRangePermissionBoxes;
    property InnerRangePermissionHighlightAreas: TdxHighlightAreaCollection read FRangePermissionHighlightAreas;
  public
    destructor Destroy; override;

    procedure ClearBookmarkBoxes;
    procedure ClearCommentBoxes;
    procedure ClearCommentHighlightAreas;
    procedure ClearCustomMarkBoxes;
    procedure ClearFieldHighlightAreas;
    procedure ClearRangePermissionHighlightAreas;
    procedure ClearRangePermissionBoxes;

    property BookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection read GetBookmarkBoxes;
    property CommentBoxes: TdxVisitableDocumentIntervalBoxCollection read GetCommentBoxes;
    property CommentHighlightAreas: TdxHighlightAreaCollection read GetCommentHighlightAreas;
    property CustomMarkBoxes: TdxCustomMarkBoxCollection read GetCustomMarkBoxes;
    property FieldHighlightAreas: TdxHighlightAreaCollection read GetFieldHighlightAreas;
    property RangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection read GetRangePermissionBoxes;
    property RangePermissionHighlightAreas: TdxHighlightAreaCollection read GetRangePermissionHighlightAreas;
  end;

  { TdxRow }

  TdxRow = class(TdxSimpleRow)
  strict private
    FNumberingListBox: TdxNumberingListBox;
    function GetExtendedBoxes: TdxRowExtendedBoxes;
    function GetInnerExtendedBoxes: TdxRowExtendedBoxes;
    function GetInnerBookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection;
    function GetInnerCustomMarkBoxes: TdxCustomMarkBoxCollection;
    function GetInnerFieldHighlightAreas: TdxHighlightAreaCollection;
    function GetInnerRangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection;
    function GetFieldHighlightAreas: TdxHighlightAreaCollection;
    function GetParagraph: TdxSimpleParagraph;
    function GetRangePermissionHighlightAreas: TdxHighlightAreaCollection;
    function GetCommentHighlightAreas: TdxHighlightAreaCollection;
    function GetInnerCommentHighlightAreas: TdxHighlightAreaCollection;
    function GetInnerRangePermissionHighlightAreas: TdxHighlightAreaCollection;
    function GetBookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection;
    function GetRangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection;
    function GetCommentBoxes: TdxVisitableDocumentIntervalBoxCollection;
    function GetCustomMarkBoxes: TdxCustomMarkBoxCollection;
    procedure SetNumberingListBox(const Value: TdxNumberingListBox);
    procedure SetParagraph(const Value: TdxSimpleParagraph);
  protected
    function GetNumberingListBoxCore: TdxBox; override; final;
    function CreateExtendedBoxes: TdxSimpleRowExtendedBoxes; override;
    property InnerExtendedBoxes: TdxRowExtendedBoxes read GetInnerExtendedBoxes;
    property InnerCustomMarkBoxes: TdxCustomMarkBoxCollection read GetInnerCustomMarkBoxes;
  public
    destructor Destroy; override;
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;

    procedure MoveVertically(ADeltaY: Integer); override;
    function GetCellViewInfo: TdxTableCellViewInfo; virtual;

    procedure ClearFieldHighlightAreas;
    procedure ClearRangePermissionHighlightAreas;
    procedure ClearCommentHighlightAreas;
    procedure ClearBookmarkBoxes; override;
    procedure ClearRangePermissionBoxes; override;
    procedure ClearCommentBoxes;
    procedure ClearCustomMarkBoxes;
    function GetBookmarkBoxesCore: TdxVisitableDocumentIntervalBoxCollection; override;
    function GetRangePermissionBoxesCore: TdxVisitableDocumentIntervalBoxCollection; override;

    property BookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection read GetBookmarkBoxes;
    property CommentBoxes: TdxVisitableDocumentIntervalBoxCollection read GetCommentBoxes;
    property CustomMarkBoxes: TdxCustomMarkBoxCollection read GetCustomMarkBoxes;
    property ExtendedBoxes: TdxRowExtendedBoxes read GetExtendedBoxes;
    property InnerBookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection read GetInnerBookmarkBoxes;
    property InnerFieldHighlightAreas: TdxHighlightAreaCollection read GetInnerFieldHighlightAreas;
    property InnerRangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection read GetInnerRangePermissionBoxes;
    property FieldHighlightAreas: TdxHighlightAreaCollection read GetFieldHighlightAreas;
    property RangePermissionHighlightAreas: TdxHighlightAreaCollection read GetRangePermissionHighlightAreas;
    property Paragraph: TdxSimpleParagraph read GetParagraph write SetParagraph;
    property CommentHighlightAreas: TdxHighlightAreaCollection read GetCommentHighlightAreas;
    property InnerCommentHighlightAreas: TdxHighlightAreaCollection read GetInnerCommentHighlightAreas;
    property InnerRangePermissionHighlightAreas: TdxHighlightAreaCollection read GetInnerRangePermissionHighlightAreas;
    property NumberingListBox: TdxNumberingListBox read FNumberingListBox write SetNumberingListBox;
    property RangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection read GetRangePermissionBoxes;
  end;

  { TdxRowCollection }

  TdxRowCollection = class(TdxBoxCollectionBase<TdxRow>)
  public
    procedure RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox); override;
    procedure RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator); override;
  end;

  { TdxTableCellRow }

  TdxTableCellRow = class(TdxRow)
  strict private
    FCellViewInfo: TdxTableCellViewInfo;
  public
    constructor Create(ACellViewInfo: TdxTableCellViewInfo);
    function GetCellViewInfo: TdxTableCellViewInfo; override;
    function IsTabelCellRow: Boolean; override;

    property CellViewInfo: TdxTableCellViewInfo read FCellViewInfo write FCellViewInfo;
  end;

  { TdxDetailRow }

  TdxDetailRow = class
  strict private
    FCharacters: TdxCharacterBoxCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property Characters: TdxCharacterBoxCollection read FCharacters;
  end;

  { TdxTableCellViewInfo }

  TdxTableCellViewInfo = class(TdxReferencedObject)
  strict private
    FTableViewInfo: TdxTableViewInfo;
    FLeft: TdxLayoutUnit;
    FWidth: TdxLayoutUnit;
    FTextWidth: TdxLayoutUnit;
    FTextLeft: TdxLayoutUnit;
    FTextOffset: TdxLayoutUnit;
    FCell: TdxTableCell;
    FTopAnchorIndex: Integer;
    FBottomAnchorIndex: Integer;
    FInitialContentTop: TdxLayoutUnit;
    FInnerTables: TdxTableViewInfoCollection;
    FRowSpan: Integer;
    FStartRowIndex: Integer;
    FHasContent: Boolean;
    function GetSingleColumn: Boolean;
    function GetLeftBorder: TdxBorderBase;
    function GetRightBorder: TdxBorderBase;
    function GetTopBorder: TdxBorderBase;
  public
    constructor Create(ATableViewInfo: TdxTableViewInfo; ACell: TdxTableCell;
      ALeft, AWidth, ATextLeft, ATextWidth, ATextOffset: TdxLayoutUnit;
      ATopAnchorIndex, ABottomAnchorIndex, AStartRowIndex, ARowSpan: Integer);
    destructor Destroy; override;

    procedure SetBottomAnchorIndexToLastAnchor;
    procedure SetTopAnchorIndexToLastAnchor;
    procedure ShiftBottom(ADelta: Integer);

    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);

    function GetActualBottomCellBorder: TdxBorderBase;
    function GetBottomCell: TdxTableCell;
    class function GetRows(ARows: TdxRowCollection; ACell: TdxTableCell): TdxRowCollection; overload; static;
    function GetRows(AColumn: TdxColumn): TdxRowCollection; overload;
    function GetFirstRowIndex(AColumn: TdxColumn): Integer;
    function GetFirstRow(AColumn: TdxColumn): TdxRow;
    function GetLastRowIndex(AColumn: TdxColumn): Integer;
    function GetLastRow(AColumn: TdxColumn): TdxRow;
    function GetBackgroundBounds: TRect; virtual;
    function GetBounds: TRect; virtual;
    function GetTableRow: TdxTableRowViewInfoBase;
    function IsEndOnNextTableViewInfo: Boolean;
    function IsStartOnPreviousTableViewInfo: Boolean;
    procedure AddInnerTable(ATableViewInfo: TdxTableViewInfo);
    procedure RemoveInnerTable(ATableViewInfo: TdxTableViewInfo);

    property TopAnchorIndex: Integer read FTopAnchorIndex;
    property BottomAnchorIndex: Integer read FBottomAnchorIndex;
    property SingleColumn: Boolean read GetSingleColumn;
    property Left: TdxLayoutUnit read FLeft;
    property Width: TdxLayoutUnit read FWidth;
    property TextWidth: TdxLayoutUnit read FTextWidth;
    property TextLeft: TdxLayoutUnit read FTextLeft;
    property TextOffset: TdxLayoutUnit read FTextOffset;
    property Cell: TdxTableCell read FCell;
    property LeftBorder: TdxBorderBase read GetLeftBorder;
    property RightBorder: TdxBorderBase read GetRightBorder;
    property TopBorder: TdxBorderBase read GetTopBorder;
    property BottomBorder: TdxBorderBase read GetActualBottomCellBorder;
    property TableViewInfo: TdxTableViewInfo read FTableViewInfo;
    property InnerTables: TdxTableViewInfoCollection read FInnerTables;
    property EmptyCell: Boolean read FHasContent write FHasContent;
    property InitialContentTop: TdxLayoutUnit read FInitialContentTop write FInitialContentTop;
    property RowSpan: Integer read FRowSpan;
    property StartRowIndex: Integer read FStartRowIndex;
  end;

  { TdxTableCellViewInfoCollection }

  TdxTableCellViewInfoCollection = class(TdxReferencedObjectList<TdxTableCellViewInfo>)
  public
    function Last: TdxTableCellViewInfo;
    function First: TdxTableCellViewInfo;
  end;

  { TdxTableBorderViewInfoBase }
  TdxTableBorderViewInfoBase = class abstract (TcxIUnknownObject)
  protected
    function GetBorder: TdxBorderInfo; virtual; abstract;
    function GetBorderType: TdxBorderTypes; virtual; abstract;
    function GetStartCorner: TdxCornerViewInfoBase; virtual; abstract;
    function GetEndCorner: TdxCornerViewInfoBase; virtual; abstract;
    function GetConverter: TdxDocumentModelUnitToLayoutUnitConverter; virtual; abstract;
  public
    function GetBounds(ATableViewInfo: TdxTableViewInfo): TRect; virtual; abstract;
    function HasStartCorner: Boolean;
    function HasEndCorner: Boolean;

    property Border: TdxBorderInfo read GetBorder;
    property BorderType: TdxBorderTypes read GetBorderType;
    property StartCorner: TdxCornerViewInfoBase read GetStartCorner;
    property EndCorner: TdxCornerViewInfoBase read GetEndCorner;
    property Converter: TdxDocumentModelUnitToLayoutUnitConverter read GetConverter;
    procedure ExportTo(ATableViewInfo: TdxTableViewInfo; const AExporter: IdxDocumentLayoutExporter);
  end;

  { TdxParagraphHorizontalBorderViewInfo }

  TdxParagraphHorizontalBorderViewInfo = class(TdxTableBorderViewInfoBase)
  strict private
    FBorderInfo: TdxBorderInfo;
    FConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    FCornerAtLeft: TdxCornerViewInfoBase;
    FCornerAtRight: TdxCornerViewInfoBase;
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
    function GetConverter: TdxDocumentModelUnitToLayoutUnitConverter; override;
    function GetStartCorner: TdxCornerViewInfoBase; override;
    function GetEndCorner: TdxCornerViewInfoBase; override;
  public
    constructor Create(ABorderInfo: TdxBorderInfo; AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
      ACornerAtLeft: TdxCornerViewInfoBase; ACornerAtRight: TdxCornerViewInfoBase);
    function GetBounds(ATableViewInfo: TdxTableViewInfo): TRect; override;
  end;

  { TdxTableCellHorizontalBorderViewInfo }

  TdxTableCellHorizontalBorderViewInfo = class(TdxTableBorderViewInfoBase)
  strict private
    FAnchor: TdxTableCellVerticalAnchor;
    FBorderInfo: TdxHorizontalCellBordersInfo;
    FConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    FCornerAtLeft: TdxCornerViewInfoBase;
    FCornerAtRight: TdxCornerViewInfoBase;
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
    function GetStartCorner: TdxCornerViewInfoBase; override;
    function GetEndCorner: TdxCornerViewInfoBase; override;
    function GetConverter: TdxDocumentModelUnitToLayoutUnitConverter; override;
  public
    constructor Create(AAnchor: TdxTableCellVerticalAnchor; ABorderInfo: TdxHorizontalCellBordersInfo;
      AConverter: TdxDocumentModelUnitToLayoutUnitConverter; ACornerAtLeft, ACornerAtRight: TdxCornerViewInfoBase);
    function GetBounds(ATableViewInfo: TdxTableViewInfo): TRect; override;
  end;

  { TdxTableCellBorderViewInfoBase }

  TdxTableCellBorderViewInfoBase = class(TdxTableBorderViewInfoBase)
  strict private
    FSourceCell: TdxTableCellViewInfo;
  protected
    function GetConverter: TdxDocumentModelUnitToLayoutUnitConverter; override;
  public
    constructor Create(ASourceCell: TdxTableCellViewInfo);
    function GetBounds(ATableViewInfo: TdxTableViewInfo): TRect; override;
    function GetBoundsCore(ATableViewInfo: TdxTableViewInfo): TRect;

    property SourceCell: TdxTableCellViewInfo read FSourceCell;
  end;

  { TdxTableCellVerticalBorderViewInfoBase }

  TdxTableCellVerticalBorderViewInfoBase = class abstract (TdxTableCellBorderViewInfoBase)
  strict private
    FTopCorner: TdxCornerViewInfoBase;
    FBottomCorner: TdxCornerViewInfoBase;
  protected
    function GetStartCorner: TdxCornerViewInfoBase; override;
    function GetEndCorner: TdxCornerViewInfoBase; override;
  public
    destructor Destroy; override;

    property TopCorner: TdxCornerViewInfoBase read FTopCorner write FTopCorner;
    property BottomCorner: TdxCornerViewInfoBase read FBottomCorner write FBottomCorner;
  end;

  { TdxLeftTableCellBorderViewInfo }

  TdxLeftTableCellBorderViewInfo = class(TdxTableCellVerticalBorderViewInfoBase)
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
  end;

  { TdxRightTableCellBorderViewInfo }

  TdxRightTableCellBorderViewInfo = class(TdxTableCellVerticalBorderViewInfoBase)
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
  end;

  TdxTableCellHorizontalBorderViewInfoBase = class(TdxTableCellBorderViewInfoBase)
  strict private
    FLeftCorner: TdxCornerViewInfoBase;
    FRightCorner: TdxCornerViewInfoBase;
  protected
    function GetStartCorner: TdxCornerViewInfoBase; override;
    function GetEndCorner: TdxCornerViewInfoBase; override;
  public
    property LeftCorner: TdxCornerViewInfoBase read FLeftCorner write FLeftCorner;
    property RightCorner: TdxCornerViewInfoBase read FRightCorner write FRightCorner;
  end;

  { TdxTopTableCellBorderViewInfo }

  TdxTopTableCellBorderViewInfo = class(TdxTableCellHorizontalBorderViewInfoBase)
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
  end;

  { TdxBottomTableCellBorderViewInfo }

  TdxBottomTableCellBorderViewInfo = class(TdxTableCellHorizontalBorderViewInfoBase)
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
  end;

  { TdxCornerViewInfoBase }

  TdxCornerViewInfoBase = class abstract
  strict private
    FWidthF: Single;
    FHeightF: Single;
    FCornerType: TdxCornerViewInfoType;
    function GetWidth: TdxLayoutUnit;
    function GetHeight: TdxLayoutUnit;
  protected
    function GetColor: TdxAlphaColor; virtual; abstract;
  public
    Pattern: TArray<TArray<Boolean>>;
    Widths: TArray<Single>;
    Heights: TArray<Single>;
    constructor Create(ACornerType: TdxCornerViewInfoType);
    class function CreateCorner(AConverter: TdxDocumentModelUnitToLayoutUnitConverter; ABorderAtLeft, ABorderAtTop,
      ABorderAtRight, ABorderAtBottom: TdxBorderInfo; ACellSpacing: TdxModelUnit): TdxCornerViewInfoBase; overload; static;
    class function CreateCorner(ACornerType: TdxCornerViewInfoType;
      AConverter: TdxDocumentModelUnitToLayoutUnitConverter; ABorderAtLeft, ABorderAtTop, ABorderAtRight,
      ABorderAtBottom: TdxBorderInfo; ACellSpacing: TdxModelUnit): TdxCornerViewInfoBase; overload; static;

    procedure Export(const AExporter: IdxDocumentLayoutExporter; X, Y: Integer); virtual; abstract;

    property CornerType: TdxCornerViewInfoType read FCornerType write FCornerType;
    property Width: TdxLayoutUnit read GetWidth;
    property WidthF: Single read FWidthF write FWidthF;
    property Height: TdxLayoutUnit read GetHeight;
    property HeightF: Single read FHeightF write FHeightF;
    property Color: TdxAlphaColor read GetColor;
  end;

  { TdxNoneLineCornerViewInfo }

  TdxNoneLineCornerViewInfo = class(TdxCornerViewInfoBase)
  protected
    function GetColor: TdxAlphaColor; override;
  public
    constructor Create;
    procedure Export(const AExporter: IdxDocumentLayoutExporter; X, Y: Integer); override;
  end;

  { TdxSingleLineCornerViewInfo }

  TdxSingleLineCornerViewInfo = class(TdxCornerViewInfoBase)
  strict private
    FColor: TdxAlphaColor;
  protected
    function GetColor: TdxAlphaColor; override;

    class var FSingleLinePattern: TArray<TArray<Boolean>>;
    class constructor Initialize;
  public
    constructor Create(ALeftBorder, ARightBorder, ATopBorder, ABottomBorder: TdxBorderInfo;
      const AWidth, AHeight: Single; ACornerType: TdxCornerViewInfoType);
    destructor Destroy; override;
    procedure Export(const AExporter: IdxDocumentLayoutExporter; X, Y: Integer); override;
  end;

  { TdxTableCellVerticalAnchor }

  TdxTableCellVerticalAnchor = class(TcxIUnknownObject)
  strict private
    FVerticalPosition: TdxLayoutUnit;
    FTopTextIndent: TdxLayoutUnit;
    FBottomTextIndent: TdxLayoutUnit;
    FCellBorders: TdxHorizontalCellBordersInfoList;
    FCorners: TdxObjectList<TdxCornerViewInfoBase>;
  protected
    function GetBottomTextIndent: TdxLayoutUnit;
    function GetVerticalPosition: TdxLayoutUnit;
  public
    constructor Create(AVerticalPosition, ABottomTextIndent: TdxLayoutUnit; ACellBorders: TdxHorizontalCellBordersInfoList);
    destructor Destroy; override;

    function CloneWithNewVerticalPosition(ANewVerticalPostion: TdxLayoutUnit): TdxTableCellVerticalAnchor;
    procedure MoveVertically(ADeltaY: TdxLayoutUnit); virtual;

    property VerticalPosition: TdxLayoutUnit read FVerticalPosition write FVerticalPosition;
    property BottomTextIndent: TdxLayoutUnit read FBottomTextIndent;
    property CellBorders: TdxHorizontalCellBordersInfoList read FCellBorders;
    property Corners: TdxObjectList<TdxCornerViewInfoBase> read FCorners;
    property TopTextIndent: TdxLayoutUnit read FTopTextIndent write FTopTextIndent;
  end;
  TdxTableCellVerticalAnchorList = class sealed(TdxList<TdxTableCellVerticalAnchor>);

  { TdxTableCellVerticalAnchorCollection }

  TdxTableCellVerticalAnchorCollection = class sealed
  strict private
    FAnchors: TdxTableCellVerticalAnchorList;
    function GetItem(Index: Integer): TdxTableCellVerticalAnchor;
    function GetItems: TdxTableCellVerticalAnchorList;
    procedure SetItem(Index: Integer; const Value: TdxTableCellVerticalAnchor);
    procedure EnsureCapacity(AIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Count: Integer;
    function First: TdxTableCellVerticalAnchor;
    function Last: TdxTableCellVerticalAnchor;
    procedure RemoveAnchors(AStartAnchorIndex, AAnchorCount: Integer);
    procedure ShiftForward(AFrom, ADelta: Integer);
    function GetAnchorIndex(ALogicalVerticalPoint: Integer): Integer;

    property Self[Index: Integer]: TdxTableCellVerticalAnchor read GetItem write SetItem; default;
    property Items: TdxTableCellVerticalAnchorList read GetItems;
  end;

  { TdxTableCellVerticalBorderViewInfo }

  TdxTableCellVerticalBorderViewInfo = class(TdxTableBorderViewInfoBase)
  public
    FBorder: TdxBorderInfo;
    FConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    FRow: TdxTableRowViewInfoBase;
    FLayoutBorderIndex: Integer;
    FModelBorderIndex: Integer;
    FCornerAtTop: TdxCornerViewInfoBase;
    FCornerAtBottom: TdxCornerViewInfoBase;
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
    function GetConverter: TdxDocumentModelUnitToLayoutUnitConverter; override;
    function GetStartCorner: TdxCornerViewInfoBase; override;
    function GetEndCorner: TdxCornerViewInfoBase; override;
  public
    constructor Create(ARow: TdxTableRowViewInfoBase; ABorder: TdxBorderInfo; ALayoutBorderIndex,
      AModelBorderIndex: Integer; AConverter: TdxDocumentModelUnitToLayoutUnitConverter);
    function GetBounds(ATableViewInfo: TdxTableViewInfo): TRect; override;

    property CornerAtTop: TdxCornerViewInfoBase read FCornerAtTop write FCornerAtTop;
    property CornerAtBottom: TdxCornerViewInfoBase read FCornerAtBottom write FCornerAtBottom;
    property LayoutBorderIndex: Integer read FLayoutBorderIndex;
    property ModelBorderIndex: Integer read FModelBorderIndex;
  end;
  TdxTableCellVerticalBorderViewInfoList = class sealed(TdxObjectList<TdxTableCellVerticalBorderViewInfo>);

  { TableRowViewInfoBase }

  TdxTableRowViewInfoBase = class abstract(TdxReferencedObject)
  strict private
    FCells: TdxTableCellViewInfoCollection;
    FTableViewInfo: TdxTableViewInfo;
    FVerticalBorders: TdxTableCellVerticalBorderViewInfoList;
    FRowIndex: Integer;
    function GetTopAnchorIndex: Integer;
    function GetBottomAnchorIndex: Integer;
    function GetTopAnchor: TdxTableCellVerticalAnchor;
    function GetBottomAnchor: TdxTableCellVerticalAnchor;
    function GetRow: TdxTableRow;
    function GetPrevious: TdxTableRowViewInfoBase;
    function GetNext: TdxTableRowViewInfoBase;
  protected
    function CalculateVerticalBorders: TdxTableCellVerticalBorderViewInfoList; virtual; abstract;
    function IsLastRow: Boolean; virtual;
    function IsFirstRow: Boolean; virtual;

    property TopAnchorIndex: Integer read GetTopAnchorIndex;
    property BottomAnchorIndex: Integer read GetBottomAnchorIndex;
  public
    constructor Create(ATableViewInfo: TdxTableViewInfo; ARowIndex: Integer);
    destructor Destroy; override;

    procedure ExportTo(const AExporter: IdxDocumentLayoutExporter); virtual; abstract;
    function ContainsEmptyCell: Boolean; virtual;
    function GetBounds: TRect; virtual; abstract;
    function GetVerticalBorderBounds(ALayoutBorderIndex: Integer): TRect; virtual; abstract;

    property TableViewInfo: TdxTableViewInfo read FTableViewInfo;
    property Cells: TdxTableCellViewInfoCollection read FCells;
    property TopAnchor: TdxTableCellVerticalAnchor read GetTopAnchor;
    property BottomAnchor: TdxTableCellVerticalAnchor read GetBottomAnchor;
    property VerticalBorders: TdxTableCellVerticalBorderViewInfoList read FVerticalBorders;
    property Row: TdxTableRow read GetRow;
    property Previous: TdxTableRowViewInfoBase read GetPrevious;
    property Next: TdxTableRowViewInfoBase read GetNext;
  end;

  { TdxTableBorderWithSpacingViewInfoBase }

  TdxTableBorderWithSpacingViewInfoBase = class abstract (TdxTableBorderViewInfoBase)
  strict private
    FRowViewInfo: TdxTableRowViewInfoWithCellSpacing;
    FLeftCorner: TdxCornerViewInfoBase;
    FRightCorner: TdxCornerViewInfoBase;
    function GetTableViewInfo: TdxTableViewInfo;
  protected
    property TableViewInfo: TdxTableViewInfo read GetTableViewInfo;
    function GetStartCorner: TdxCornerViewInfoBase; override;
    function GetEndCorner: TdxCornerViewInfoBase; override;
    function GetConverter: TdxDocumentModelUnitToLayoutUnitConverter; override;
  public
    constructor Create(ARowViewInfo: TdxTableRowViewInfoWithCellSpacing);

    property RowViewInfo: TdxTableRowViewInfoWithCellSpacing read FRowViewInfo write FRowViewInfo;
    property LeftCorner: TdxCornerViewInfoBase read FLeftCorner write FLeftCorner;
    property RightCorner: TdxCornerViewInfoBase read FRightCorner write FRightCorner;
  end;

  { TdxTableRowViewInfoWithCellSpacing }

  TdxTableRowViewInfoWithCellSpacing = class(TdxTableRowViewInfoBase)
  strict private
    FCellSpacing: TdxModelUnit;
    function GetConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  protected
    function CalculateVerticalBorders: TdxTableCellVerticalBorderViewInfoList; override;
    procedure ExportTableBorders(const AExporter: IdxDocumentLayoutExporter); virtual;

    property Converter: TdxDocumentModelUnitToLayoutUnitConverter read GetConverter;
  public
    constructor Create(ATableViewInfo: TdxTableViewInfo; ARowIndex: Integer; ACellSpacing: TdxModelUnit);
    procedure ExportTo(const AExporter: IdxDocumentLayoutExporter); override;
    function GetBounds: TRect; override;
    function GetVerticalBorderBounds(ALayoutBorderIndex: Integer): TRect; override;

    property CellSpacing: TdxModelUnit read FCellSpacing write FCellSpacing;
  end;

  { TdxLeftRightTableBorderWithSpacingViewInfoBase }

  TdxLeftRightTableBorderWithSpacingViewInfoBase = class(TdxTableBorderWithSpacingViewInfoBase)
  public
    function GetBounds(ATableViewInfo: TdxTableViewInfo): TRect; override;
  end;

  { TdxLeftTableBorderWithSpacingViewInfo }

  TdxLeftTableBorderWithSpacingViewInfo = class(TdxLeftRightTableBorderWithSpacingViewInfoBase)
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
  end;

  { TdxRightTableBorderWithSpacingViewInfo }

  TdxRightTableBorderWithSpacingViewInfo = class(TdxLeftRightTableBorderWithSpacingViewInfoBase)
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
  end;

  { TdxTopTableBorderWithSpacingViewInfo }

  TdxTopTableBorderWithSpacingViewInfo = class(TdxTableBorderWithSpacingViewInfoBase)
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
  public
    function GetBounds(ATableViewInfo: TdxTableViewInfo): TRect; override;
  end;

  { TdxBottomTableBorderWithSpacingViewInfo }

  TdxBottomTableBorderWithSpacingViewInfo = class(TdxTableBorderWithSpacingViewInfoBase)
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
  public
    function GetBounds(ATableViewInfo: TdxTableViewInfo): TRect; override;
  end;

  { TdxTableRowViewInfoCollection }

  TdxTableRowViewInfoCollection = class sealed(TdxReferencedObjectList<TdxTableRowViewInfoBase>)
  strict private
    FTableViewInfo: TdxTableViewInfo;
  public
    constructor Create(ATableViewInfo: TdxTableViewInfo);
    function Last: TdxTableRowViewInfoBase;
    function First: TdxTableRowViewInfoBase;
    procedure RemoveRows(AStartRowIndex, ARowCount: Integer);
    procedure ShiftForward(ADelta: Integer);

    property TableViewInfo: TdxTableViewInfo read FTableViewInfo;
  end;

  { TdxTableRowViewInfoNoCellSpacing }

  TdxTableRowViewInfoNoCellSpacing = class(TdxTableRowViewInfoBase)
  protected
    function CalculateVerticalBorders: TdxTableCellVerticalBorderViewInfoList; override;
    function GetBorderIndexByColumnIndex(ALayoutBorderIndex: Integer): Integer; virtual;
  public
    procedure ExportTo(const AExporter: IdxDocumentLayoutExporter); override;
    function GetBounds: TRect; override;
    function GetVerticalBorderBounds(ALayoutBorderIndex: Integer): TRect; override;
  end;

  { TableViewInfo }

  TdxTableViewInfo = class sealed(TdxReferencedObject)
  strict private
    FTable: TdxTable;
    FRows: TdxTableRowViewInfoCollection;
    FCells: TdxTableCellViewInfoCollection;
    FIsNested: Boolean;
    FMinBottomVerticalOffset: TdxLayoutUnit;
    FTextAreaLeft: TdxLayoutUnit;
    FTopLevelColumn: TdxColumn;
    FColumn: TdxColumn;
    FAnchors: TdxTableCellVerticalAnchorCollection;
    FTopRowIndex: Integer;
    FVerticalBorderPositions: TdxVerticalBorderPositions;
    FLeftOffset: TdxLayoutUnit;
    FModelRelativeIndent: TdxModelUnit;
    FPrevTableViewInfo: TdxTableViewInfo;
    FNextTableViewInfo: TdxTableViewInfo;
    FParentTableCellViewInfo: TdxTableCellViewInfo;
    FFirstContentInParentCell: Boolean;
    procedure CreateAnchorCorner(AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
      AAnchor: TdxTableCellVerticalAnchor; ATopRow: TdxTableRowViewInfoNoCellSpacing; ABottomRow: TdxTableRowViewInfoNoCellSpacing);
    procedure ExportBackgroundCore(const AExporter: IdxDocumentLayoutExporter; const ARowAction: TdxAction<TdxTableRowViewInfoBase>);
    procedure ExportCellsVerticalBorders(ATableRowViewInfoBase: TdxTableRowViewInfoBase; const AExporter: IdxDocumentLayoutExporter);
    procedure ExportRowCells(ARow: TdxTableRowViewInfoBase; const AExporter: IdxDocumentLayoutExporter);
    function GetTopAnchor: TdxTableCellVerticalAnchor;
    function GetBottomAnchor: TdxTableCellVerticalAnchor;
    function GetRowCount: Integer;
    function GetBottomRowIndex: Integer;
    function GetSingleColumn: Boolean;
    function GetVerticalBorderByColumnIndex(ARowViewInfo: TdxTableRowViewInfoNoCellSpacing; ACornerColumnIndex: Integer): TdxTableCellVerticalBorderViewInfo;
    procedure RemoveInnerTablesFromTopLevelColumn(ATableCellViewInfo: TdxTableCellViewInfo);
    procedure RemoveTableViewInfoFromTopLevelColumn(ATableViewInfo: TdxTableViewInfo);
    procedure SetIsNested(const Value: Boolean);
  protected
    FAssociatedFloatingObjectsLayout: TObject;
    procedure ExportAnchorBorders(AAnchor: TdxTableCellVerticalAnchor; const AExporter: IdxDocumentLayoutExporter); virtual;
    procedure NestedChanged; virtual;

    property IsNested: Boolean read FIsNested write SetIsNested;
  public
    constructor Create(ATable: TdxTable; ATopLevelColumn: TdxColumn; AColumn: TdxColumn; AVerticalBorderPositions: TdxVerticalBorderPositions;
      ATopRowIndex: Integer; AParentTableCellViewInfo: TdxTableCellViewInfo; AFirstContentInParentCell: Boolean);
    destructor Destroy; override;

    function GetActualLeftBorder: TdxBorderBase;
    function GetActualRightBorder: TdxBorderBase;
    function GetActualTopBorder: TdxBorderBase;
    function GetActualBottomBorder: TdxBorderBase;
    procedure SetTopRowIndex(ATopRowIndex: Integer);
    function GetColumnLeft(AColumnIndex: Integer): TdxLayoutUnit;
    function GetColumnRight(AColumnIndex: Integer): TdxLayoutUnit;
    function GetVerticalBorderPosition(ABorderIndex: Integer): Integer;
    procedure ExportTo(const AExporter: IdxDocumentLayoutExporter);
    procedure ExportAnchorCorner(const AExporter: IdxDocumentLayoutExporter; AAnchor: TdxTableCellVerticalAnchor);
    procedure ExportBackground(const AExporter: IdxDocumentLayoutExporter);
    function GetRowBounds(ARow: TdxTableRowViewInfoBase): TRect;
    function GetCellBounds(ACellViewInfo: TdxTableCellViewInfo): TRect;
    procedure Complete(ATopBorderHeight: TdxLayoutUnit; ABottomBorderHeight: TdxLayoutUnit);
    procedure RemoveEmptyCells;
    procedure RemoveRowsFromIndex(AFirstRowViewInfoIndex: Integer);
    procedure MoveVertically(ADeltaY: TdxLayoutUnit);
    procedure MoveVerticallyRecursive(ADeltaY: TdxLayoutUnit);
    function GetAlignedPosition(AHorizontalPositionIndex: Integer): Integer; virtual;
    function GetActualBottomPosition: TdxLayoutUnit; virtual;
    function GetTableBottom: Integer; virtual;
    function GetTableTop: Integer; virtual;

    property SingleColumn: Boolean read GetSingleColumn;
    property Cells: TdxTableCellViewInfoCollection read FCells;
    property Rows: TdxTableRowViewInfoCollection read FRows;
    property Anchors: TdxTableCellVerticalAnchorCollection read FAnchors;
    property ParentTableCellViewInfo: TdxTableCellViewInfo read FParentTableCellViewInfo;
    property Table: TdxTable read FTable;
    property MinBottomVerticalOffset: TdxLayoutUnit read FMinBottomVerticalOffset write FMinBottomVerticalOffset;
    property TopAnchor: TdxTableCellVerticalAnchor read GetTopAnchor;
    property BottomAnchor: TdxTableCellVerticalAnchor read GetBottomAnchor;
    property TopLevelColumn: TdxColumn read FTopLevelColumn;
    property Column: TdxColumn read FColumn;
    property LeftOffset: TdxLayoutUnit read FLeftOffset write FLeftOffset;
    property ModelRelativeIndent: TdxModelUnit read FModelRelativeIndent write FModelRelativeIndent;
    property TopRowIndex: Integer read FTopRowIndex;
    property RowCount: Integer read GetRowCount;
    property BottomRowIndex: Integer read GetBottomRowIndex;
    property PrevTableViewInfo: TdxTableViewInfo read FPrevTableViewInfo write FPrevTableViewInfo;
    property NextTableViewInfo: TdxTableViewInfo read FNextTableViewInfo write FNextTableViewInfo;
    property FirstContentInParentCell: Boolean read FFirstContentInParentCell;
    property VerticalBorderPositions: TdxVerticalBorderPositions read FVerticalBorderPositions write FVerticalBorderPositions;
    property TextAreaOffset: TdxLayoutUnit read FTextAreaLeft write FTextAreaLeft;
  end;

  { TdxTableViewInfoCollection }

  TdxTableViewInfoCollection = class sealed(TdxReferencedObjectList<TdxTableViewInfo>)
  public
    procedure ExportTo(const AExporter: IdxDocumentLayoutExporter);
    procedure MoveVertically(ADeltaY: TdxLayoutUnit);
    procedure ExportBackground(const AExporter: IdxDocumentLayoutExporter);
  end;

  { TdxTableBorderInfo }

  TdxTableBorderInfo = record
  strict private
    FWidthDivider: Integer;
    FWidthMultiplier: Integer;
  public
    CompoundArray: TArray<Integer>;
    DrawingCompoundArray: TArray<Single>;
    LineCount: Integer;

    constructor Create(const ACompoundArray: TArray<Integer>; AWidthDivider: Integer);
    function GetActualWidth(ABorderWidth: Integer): Integer;
  end;

  { TdxTableBorderCalculator }

  TdxTableBorderCalculator = class
  strict private class var
{$IFDEF DELPHIXE2}
    LineStyleInfos: array[TdxBorderLineStyle.Single..TdxBorderLineStyle.Inset] of TdxTableBorderInfo;
{$ELSE}
    LineStyleInfos: array[Ord(TdxBorderLineStyle.Single)..Ord(TdxBorderLineStyle.Inset)] of TdxTableBorderInfo;
{$ENDIF}
  strict private
    function CalculateWeight(ABorder: TdxBorderInfo): Integer;
    class function LineStyleInfosContainsKey(AKey: TdxBorderLineStyle): Boolean; static;
    class function LineStyleInfosTryGetValue(AKey: TdxBorderLineStyle; var AInfo: TdxTableBorderInfo): Boolean; static;
  protected
    class procedure AddLineStyleInfo(ALineStyle: TdxBorderLineStyle; const ACompoundArray: TArray<Integer>; AWidthDivider: Integer); static;
  private
    class constructor Initialize;
  protected
    function GetActualBorderLineStyle(ABorderLineStyle: TdxBorderLineStyle): TdxBorderLineStyle;
  public
    function GetActualWidth(ABorder: TdxBorderBase): Integer; overload;
    function GetActualWidth(ABorder: TdxBorderInfo): Integer; overload;
    function GetActualWidth(AStyle: TdxBorderLineStyle; AWidth: Integer): Integer; overload;
    function GetLineCount(ABorder: TdxBorderInfo): Integer;
    function GetDrawingCompoundArray(ABorderLineStyle: TdxBorderLineStyle): TArray<Single>; overload;
    function GetDrawingCompoundArray(ABorder: TdxBorderInfo): TArray<Single>; overload;
    function GetDrawingCompoundArray(ABorder: TdxBorderBase): TArray<Single>; overload;
    function GetVerticalBorderSource(ATable: TdxTable; AFirstCellBorder, ASecondCellBorder: TdxBorderInfo): TdxBorderInfo;
    function IsVisuallyAdjacentBorder(ABorder1, ABorder2: TdxBorderInfo; ASameDirection: Boolean): Boolean;
  end;

  { TdxTableCellVerticalBorderCalculator }

  TdxTableCellVerticalBorderCalculator = class
  private
    FTable: TdxTable;
  protected
    function GetLeftBorder(ABorderCalculator: TdxTableBorderCalculator; ACell: TdxTableCell): TdxBorderInfo;
    function GetRightBorder(ABorderCalculator: TdxTableBorderCalculator; ACell: TdxTableCell): TdxBorderInfo;
  public
    constructor Create(ATable: TdxTable);
    class function GetStartColumnIndex(ACell: TdxTableCell; ALayoutIndex: Boolean): Integer; static;
    class function GetVerticalSpanCells(ACell: TdxTableCell; AStartColumnIndex: Integer; ALayoutIndex: Boolean): TdxTableCellList; overload; static;
    class function GetVerticalSpanCells(ACell: TdxTableCell; ALayoutIndex: Boolean): TdxTableCellList; overload; static;
    class function GetCellByStartColumnIndex(ARow: TdxTableRow; AStartColumnIndex: Integer; ALayoutIndex: Boolean): TdxTableCell; static;
    class function GetCellByColumnIndex(ARow: TdxTableRow; AStartColumnIndex: Integer): TdxTableCell; static;
    class function GetCellByEndColumnIndex(ARow: TdxTableRow; AEndColumnIndex: Integer): TdxTableCell; static;
    class function GetCellsByIntervalColumnIndex(ARow: TdxTableRow; AStartColumnIndex, AEndColumnIndex: Integer): TdxTableCellList; static;
    function GetLeftBorderWidth(ABorderCalculator: TdxTableBorderCalculator; ACell: TdxTableCell; ALayoutIndex: Boolean): TdxModelUnit;
    function GetRightBorderWidth(ABorderCalculator: TdxTableBorderCalculator; ACell: TdxTableCell): TdxModelUnit;
  end;

  { TdxBookmarkStartBox }

  TdxBookmarkStartBox = class(TdxVisitableDocumentIntervalBox)
  public
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
  end;

  { TdxBookmarkEndBox }

  TdxBookmarkEndBox = class(TdxVisitableDocumentIntervalBox)
  public
    procedure ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter); override;
  end;

  { TdxTableCellVerticalAnchorYComparable }

  TdxTableCellVerticalAnchorYComparable = class(TcxIUnknownObject, IdxComparable<TdxTableCellVerticalAnchor>)
  strict private
    FPos: Integer;
  public
    constructor Create(APos: Integer);
    function CompareTo(const AAnchor: TdxTableCellVerticalAnchor): Integer;
  end;

  { TdxTableRowAnchorComparable }

  TdxTableRowAnchorComparable = class(TcxIUnknownObject, IdxComparable<TdxTableRowViewInfoBase>)
  private
    FPos: Integer;
    FLastRow: TdxTableRow;
  public
    constructor Create(APos: Integer; ALastRow: TdxTableRow);
    function CompareTo(const Value: TdxTableRowViewInfoBase): Integer;
  end;

  { TdxTableCellAnchorComparable }

  TdxTableCellAnchorComparable = class(TcxIUnknownObject, IdxComparable<TdxTableCellViewInfo>)
  private
    FPos: Integer;
  public
    constructor Create(APos: Integer);
    function CompareTo(const Value: TdxTableCellViewInfo): Integer;
  end;

implementation

uses
  Contnrs, Math, dxTypeHelpers, cxGeometry,
  dxMeasurementUnits,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.FastComparer,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.Control.HitTest;

type
  TIndexComparer = class(TdxBoxBaseComparer)
  public
    function Compare(const ALeft, ARight: TdxBoxBase): Integer; override;
  end;

  TZOrderComparer = class(TdxBoxBaseComparer)
  public
    function Compare(const ALeft, ARight: TdxBoxBase): Integer; override;
  end;

  { TdxParagraphFrameBoxAndParagraphIndexComparable }

  TdxParagraphFrameBoxAndParagraphIndexComparable = class(TcxIUnknownObject, IdxComparable<TdxParagraphFrameBox>)
  strict private
    FParagraphIndex: TdxParagraphIndex;
  public
    constructor Create(AParagraphIndex: TdxParagraphIndex);
    function CompareTo(const AOther: TdxParagraphFrameBox): Integer;
  end;

  { TdxVerticalBorderAndColumnIndexComparer }

  TdxVerticalBorderAndColumnIndexComparer = class(TcxIUnknownObject, IdxComparable<TdxTableCellVerticalBorderViewInfo>)
  strict private
    FLayoutBorderIndex: Integer;
  public
    constructor Create(ALayoutBorderIndex: Integer);
    function CompareTo(const AOther: TdxTableCellVerticalBorderViewInfo): Integer;
  end;

  { TdxCounterItemAndLogPositionComparable }

  TdxCounterItemAndLogPositionComparable = class(TcxIUnknownObject, IdxComparable<TdxCounterItem>)
  strict private
    FLogPosition: TdxDocumentLogPosition;
  public
    constructor Create(ALogPosition: TdxDocumentLogPosition);
    function CompareTo(const AItem: TdxCounterItem): Integer;

    property LogPosition: TdxDocumentLogPosition read FLogPosition;
  end;

  { TdxMultiLineCornerViewInfoBase }

  TdxMultiLineCornerViewInfoBase = class(TdxCornerViewInfoBase)
  strict private
    FColor: TdxAlphaColor;
  protected
    function GetColor: TdxAlphaColor; override;
    function CreatePattern(ABorderAtLeft, ABorderAtTop, ABorderAtRight, ABorderAtBottom: TdxBorderInfo): TArray<TArray<Boolean>>; virtual;
    function IntersectPattern(const APattern1, APattern2: TArray<TArray<Boolean>>): TArray<TArray<Boolean>>;

    function GetVerticalBordersMask: TArray<TArray<Boolean>>; virtual; abstract;
    function GetHorizontalBordersMask: TArray<TArray<Boolean>>; virtual; abstract;
    function GetCommonBorderMask: TArray<TArray<Boolean>>; virtual; abstract;
    function GetLeftBorderMask: TArray<TArray<Boolean>>; virtual; abstract;
    function GetRightBorderMask: TArray<TArray<Boolean>>; virtual; abstract;
    function GetTopBorderMask: TArray<TArray<Boolean>>; virtual; abstract;
    function GetBottomBorderMask: TArray<TArray<Boolean>>; virtual; abstract;

    property VerticalBordersMask: TArray<TArray<Boolean>> read GetVerticalBordersMask;
    property HorizontalBordersMask: TArray<TArray<Boolean>> read GetHorizontalBordersMask;
    property CommonBorderMask: TArray<TArray<Boolean>> read GetCommonBorderMask;
    property LeftBorderMask: TArray<TArray<Boolean>> read GetLeftBorderMask;
    property RightBorderMask: TArray<TArray<Boolean>> read GetRightBorderMask;
    property TopBorderMask: TArray<TArray<Boolean>> read GetTopBorderMask;
    property BottomBorderMask: TArray<TArray<Boolean>> read GetBottomBorderMask;
  public
    constructor Create(ABorderAtLeft, ABorderAtTop, ABorderAtRight, ABorderAtBottom: TdxBorderInfo;
      const AWidth, AHeight: Single; ACornerType: TdxCornerViewInfoType);
    destructor Destroy; override;

    procedure Export(const AExporter: IdxDocumentLayoutExporter; X, Y: Integer); override;
  end;

  { TdxDoubleLineCornerViewInfo }

  TdxDoubleLineCornerViewInfo = class(TdxMultiLineCornerViewInfoBase)
  protected
    class var FVerticalBordersMask: TArray<TArray<Boolean>>;
    class var FHorizontalBordersMask: TArray<TArray<Boolean>>;
    class var FCommonBorderMask: TArray<TArray<Boolean>>;
    class var FLeftBorderMask: TArray<TArray<Boolean>>;
    class var FRightBorderMask: TArray<TArray<Boolean>>;
    class var FTopBorderMask: TArray<TArray<Boolean>>;
    class var FBottomBorderMask: TArray<TArray<Boolean>>;
    class constructor Initialize;
  protected
    function GetVerticalBordersMask: TArray<TArray<Boolean>>; override;
    function GetHorizontalBordersMask: TArray<TArray<Boolean>>; override;
    function GetCommonBorderMask: TArray<TArray<Boolean>>; override;
    function GetLeftBorderMask: TArray<TArray<Boolean>>; override;
    function GetRightBorderMask: TArray<TArray<Boolean>>; override;
    function GetTopBorderMask: TArray<TArray<Boolean>>; override;
    function GetBottomBorderMask: TArray<TArray<Boolean>>; override;
  end;

  { TdxTripleLineCornerViewInfo }

  TdxTripleLineCornerViewInfo = class(TdxMultiLineCornerViewInfoBase)
  protected
    class var FCommonBorderMask: TArray<TArray<Boolean>>;
    class var FLeftBorderMask: TArray<TArray<Boolean>>;
    class var FNoLeftBorderMask: TArray<TArray<Boolean>>;
    class var FRightBorderMask: TArray<TArray<Boolean>>;
    class var FNoRightBorderMask: TArray<TArray<Boolean>>;
    class var FTopBorderMask: TArray<TArray<Boolean>>;
    class var FNoTopBorderMask: TArray<TArray<Boolean>>;
    class var FBottomBorderMask: TArray<TArray<Boolean>>;
    class var FNoBottomBorderMask: TArray<TArray<Boolean>>;
    class var FVerticalBordersMask: TArray<TArray<Boolean>>;
    class var FHorizontalBordersMask: TArray<TArray<Boolean>>;
    class constructor Initialize;
  protected
    function CreatePattern(ABorderAtLeft, ABorderAtTop, ABorderAtRight, ABorderAtBottom: TdxBorderInfo): TArray<TArray<Boolean>>; override;

    function GetVerticalBordersMask: TArray<TArray<Boolean>>; override;
    function GetHorizontalBordersMask: TArray<TArray<Boolean>>; override;
    function GetCommonBorderMask: TArray<TArray<Boolean>>; override;
    function GetLeftBorderMask: TArray<TArray<Boolean>>; override;
    function GetRightBorderMask: TArray<TArray<Boolean>>; override;
    function GetTopBorderMask: TArray<TArray<Boolean>>; override;
    function GetBottomBorderMask: TArray<TArray<Boolean>>; override;
  end;

  { TdxParagraphIndexComparer }

  TdxParagraphIndexComparer = class(TdxComparer<TdxTableCellViewInfo>)
  public
    function Compare(const X, Y: TdxTableCellViewInfo): Integer; override;
  end;

{ TdxVerticalBorderAndColumnIndexComparer }

constructor TdxVerticalBorderAndColumnIndexComparer.Create(ALayoutBorderIndex: Integer);
begin
  inherited Create;
  FLayoutBorderIndex := ALayoutBorderIndex;
end;

function TdxVerticalBorderAndColumnIndexComparer.CompareTo(const AOther: TdxTableCellVerticalBorderViewInfo): Integer;
begin
  Result :=  AOther.LayoutBorderIndex - FLayoutBorderIndex;
end;

{ TdxCounterItemAndLogPositionComparable }

constructor TdxCounterItemAndLogPositionComparable.Create(ALogPosition: TdxDocumentLogPosition);
begin
  inherited Create;
  FLogPosition := ALogPosition;
end;

function TdxCounterItemAndLogPositionComparable.CompareTo(const AItem: TdxCounterItem): Integer;
var
  AItemLogPosition: TdxDocumentLogPosition;
begin
  AItemLogPosition := AItem.LogPosition;
  if FLogPosition < AItemLogPosition then
    Result := 1
  else
    if FLogPosition > AItemLogPosition then
      Result := -1
    else
      Result := 0;
end;

{ TdxTabSpaceBox }

constructor TdxTabSpaceBox.Create;
begin
  inherited Create;
  FLeaderCount := -1;
end;

class function TdxTabSpaceBox.CreateBox: TdxBox;
begin
  Result := TdxTabSpaceBox.Create;
end;

function TdxTabSpaceBox.IsVisible: Boolean;
begin
  Result := True;
end;

function TdxTabSpaceBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := False;
end;

function TdxTabSpaceBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

procedure TdxTabSpaceBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportTabSpaceBox(Self);
end;

function TdxTabSpaceBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := (ACalculator as IdxBoxHitTestCalculator).CreateTabSpaceBoxHitTestManager(Self);
end;

function TdxTabSpaceBox.GetMarkCharacter: Char;
begin
  Result := #$2192;
end;

function TdxTabSpaceBox.IsTabSpaceBox: Boolean;
begin
  Result := True;
end;

{ TdxRowExtendedBoxes }

destructor TdxRowExtendedBoxes.Destroy;
begin
  FreeAndNil(FCommentHighlightAreas);
  FreeAndNil(FFieldHighlightAreas);
  FreeAndNil(FRangePermissionHighlightAreas);
  FreeAndNil(FBookmarkBoxes);
  FreeAndNil(FRangePermissionBoxes);
  FreeAndNil(FCommentBoxes);
  FreeAndNil(FCustomMarkBoxes);
  inherited Destroy;
end;

function TdxRowExtendedBoxes.GetCommentHighlightAreas: TdxHighlightAreaCollection;
begin
  if FCommentHighlightAreas = nil then
    FCommentHighlightAreas := TdxHighlightAreaCollection.Create;
  Result := FCommentHighlightAreas;
end;

function TdxRowExtendedBoxes.GetFieldHighlightAreas: TdxHighlightAreaCollection;
begin
  if FFieldHighlightAreas = nil then
    FFieldHighlightAreas := TdxHighlightAreaCollection.Create;
  Result := FFieldHighlightAreas;
end;

function TdxRowExtendedBoxes.GetRangePermissionHighlightAreas: TdxHighlightAreaCollection;
begin
  if FRangePermissionHighlightAreas = nil then
    FRangePermissionHighlightAreas := TdxHighlightAreaCollection.Create;
  Result := FRangePermissionHighlightAreas;
end;

function TdxRowExtendedBoxes.GetBookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  if FBookmarkBoxes = nil then
    FBookmarkBoxes := TdxVisitableDocumentIntervalBoxCollection.Create;
  Result := FBookmarkBoxes;
end;

function TdxRowExtendedBoxes.GetRangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  if FRangePermissionBoxes = nil then
    FRangePermissionBoxes := TdxVisitableDocumentIntervalBoxCollection.Create;
  Result := FRangePermissionBoxes;
end;

function TdxRowExtendedBoxes.GetCommentBoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  if FCommentBoxes = nil then
    FCommentBoxes := TdxVisitableDocumentIntervalBoxCollection.Create;
  Result := FCommentBoxes;
end;

function TdxRowExtendedBoxes.GetCustomMarkBoxes: TdxCustomMarkBoxCollection;
begin
  if FCustomMarkBoxes = nil then
    FCustomMarkBoxes := TdxCustomMarkBoxCollection.Create;
  Result := FCustomMarkBoxes;
end;

procedure TdxRowExtendedBoxes.ClearFieldHighlightAreas;
begin
  FreeAndNil(FFieldHighlightAreas);
end;

procedure TdxRowExtendedBoxes.ClearRangePermissionHighlightAreas;
begin
  FreeAndNil(FRangePermissionHighlightAreas);
end;

procedure TdxRowExtendedBoxes.ClearCommentHighlightAreas;
begin
  FreeAndNil(FCommentHighlightAreas);
end;

procedure TdxRowExtendedBoxes.ClearBookmarkBoxes;
begin
  FreeAndNil(FBookmarkBoxes);
end;

procedure TdxRowExtendedBoxes.ClearRangePermissionBoxes;
begin
  FreeAndNil(FRangePermissionBoxes);
end;

procedure TdxRowExtendedBoxes.ClearCommentBoxes;
begin
  FreeAndNil(FCommentBoxes);
end;

procedure TdxRowExtendedBoxes.ClearCustomMarkBoxes;
begin
  FreeAndNil(FCustomMarkBoxes);
end;

{ TdxRow }

destructor TdxRow.Destroy;
begin
  NumberingListBox := nil;
  inherited Destroy;
end;

procedure TdxRow.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportRow(Self);
end;

procedure TdxRow.MoveVertically(ADeltaY: Integer);
begin
  inherited MoveVertically(ADeltaY);
  if InnerHighlightAreas <> nil then
    InnerHighlightAreas.MoveVertically(ADeltaY);
  if InnerFieldHighlightAreas <> nil then
    InnerFieldHighlightAreas.MoveVertically(ADeltaY);
  if InnerRangePermissionHighlightAreas <> nil then
    InnerRangePermissionHighlightAreas.MoveVertically(ADeltaY);
  if NumberingListBox <> nil then
    NumberingListBox.MoveVertically(ADeltaY);
end;

function TdxRow.GetExtendedBoxes: TdxRowExtendedBoxes;
begin
  Result := TdxRowExtendedBoxes(inherited ExtendedBoxes);
end;

function TdxRow.CreateExtendedBoxes: TdxSimpleRowExtendedBoxes;
begin
  Result := TdxRowExtendedBoxes.Create;
end;

function TdxRow.GetInnerExtendedBoxes: TdxRowExtendedBoxes;
begin
  Result := TdxRowExtendedBoxes(inherited InnerExtendedBoxes);
end;

function TdxRow.GetInnerBookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerBookmarkBoxes
  else
    Result := nil;
end;

function TdxRow.GetInnerCustomMarkBoxes: TdxCustomMarkBoxCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerCustomMarkBoxes
  else
    Result := nil;
end;

function TdxRow.GetInnerFieldHighlightAreas: TdxHighlightAreaCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerFieldHighlightAreas
  else
    Result := nil;
end;

function TdxRow.GetInnerRangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerRangePermissionBoxes
  else
    Result := nil;
end;


function TdxRow.GetInnerRangePermissionHighlightAreas: TdxHighlightAreaCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerRangePermissionHighlightAreas
  else
    Result := nil;
end;

function TdxRow.GetNumberingListBoxCore: TdxBox;
begin
  Result := NumberingListBox;
end;

function TdxRow.GetParagraph: TdxSimpleParagraph;
begin
  Result := TdxSimpleParagraph(inherited Paragraph)
end;

function TdxRow.GetFieldHighlightAreas: TdxHighlightAreaCollection;
begin
  Result := ExtendedBoxes.FieldHighlightAreas;
end;

function TdxRow.GetRangePermissionHighlightAreas: TdxHighlightAreaCollection;
begin
  Result := ExtendedBoxes.RangePermissionHighlightAreas;
end;

function TdxRow.GetCommentHighlightAreas: TdxHighlightAreaCollection;
begin
  Result := ExtendedBoxes.CommentHighlightAreas;
end;

function TdxRow.GetInnerCommentHighlightAreas: TdxHighlightAreaCollection;
begin
  if InnerExtendedBoxes <> nil then
    Result := InnerExtendedBoxes.InnerCommentHighlightAreas
  else
    Result := nil;
end;

function TdxRow.GetBookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  Result := ExtendedBoxes.BookmarkBoxes;
end;

function TdxRow.GetRangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  Result := ExtendedBoxes.RangePermissionBoxes;
end;

function TdxRow.GetCommentBoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  Result := ExtendedBoxes.CommentBoxes;
end;

function TdxRow.GetCustomMarkBoxes: TdxCustomMarkBoxCollection;
begin
  Result := ExtendedBoxes.CustomMarkBoxes;
end;

procedure TdxRow.SetNumberingListBox(const Value: TdxNumberingListBox);
begin
  if FNumberingListBox = Value then
    Exit;
  TdxNumberingListBox.Release(FNumberingListBox);
  FNumberingListBox := Value;
  TdxNumberingListBox.AddReference(FNumberingListBox);
end;

procedure TdxRow.SetParagraph(const Value: TdxSimpleParagraph);
begin
  inherited Paragraph := Value;
end;

procedure TdxRow.ClearFieldHighlightAreas;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearFieldHighlightAreas;
end;

procedure TdxRow.ClearRangePermissionHighlightAreas;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearRangePermissionHighlightAreas;
end;

procedure TdxRow.ClearCommentHighlightAreas;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearCommentHighlightAreas;
end;

procedure TdxRow.ClearBookmarkBoxes;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearBookmarkBoxes;
end;

procedure TdxRow.ClearRangePermissionBoxes;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearRangePermissionBoxes;
end;

procedure TdxRow.ClearCommentBoxes;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearCommentBoxes;
end;

procedure TdxRow.ClearCustomMarkBoxes;
begin
  if InnerExtendedBoxes <> nil then
    InnerExtendedBoxes.ClearCustomMarkBoxes;
end;

function TdxRow.GetBookmarkBoxesCore: TdxVisitableDocumentIntervalBoxCollection;
begin
  Result := BookmarkBoxes;
end;

function TdxRow.GetRangePermissionBoxesCore: TdxVisitableDocumentIntervalBoxCollection;
begin
  Result := RangePermissionBoxes;
end;

function TdxRow.GetCellViewInfo: TdxTableCellViewInfo;
begin
  Result := nil;
end;

{ TdxTableCellViewInfo }

constructor TdxTableCellViewInfo.Create(ATableViewInfo: TdxTableViewInfo; ACell: TdxTableCell;
  ALeft, AWidth, ATextLeft, ATextWidth, ATextOffset: TdxLayoutUnit;
  ATopAnchorIndex, ABottomAnchorIndex, AStartRowIndex, ARowSpan: Integer);
begin
  inherited Create;
  Assert(ATextWidth >= 0);
  FTableViewInfo := ATableViewInfo;
  FCell := ACell;
  FWidth := AWidth;
  FLeft := ALeft;
  FTopAnchorIndex := ATopAnchorIndex;
  FBottomAnchorIndex := ABottomAnchorIndex;
  FTextWidth := ATextWidth;
  FTextLeft := ATextLeft;
  FTextOffset := ATextOffset;
  FInnerTables := TdxTableViewInfoCollection.Create;
  FRowSpan := ARowSpan;
  FStartRowIndex := AStartRowIndex;
  FInitialContentTop := MinInt;
end;

destructor TdxTableCellViewInfo.Destroy;
begin
  FreeAndNil(FInnerTables);
  inherited Destroy;
end;

procedure TdxTableCellViewInfo.SetBottomAnchorIndexToLastAnchor;
begin
  FBottomAnchorIndex := TableViewInfo.Anchors.Count - 1;
end;

procedure TdxTableCellViewInfo.SetTopAnchorIndexToLastAnchor;
begin
  FTopAnchorIndex := 0;
end;

procedure TdxTableCellViewInfo.ShiftBottom(ADelta: Integer);
begin
  Inc(FBottomAnchorIndex, ADelta);
end;

procedure TdxTableCellViewInfo.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportTableCell(Self);
end;

function TdxTableCellViewInfo.IsStartOnPreviousTableViewInfo: Boolean;
begin
  if TableViewInfo.PrevTableViewInfo = nil then
    Exit(False);
  if FTopAnchorIndex > 0 then
    Exit(False);
  if StartRowIndex < TableViewInfo.TopRowIndex then
    Exit(True);
  if (StartRowIndex = TableViewInfo.TopRowIndex) and
    (TableViewInfo.TopRowIndex = TableViewInfo.PrevTableViewInfo.BottomRowIndex) then
    Exit(True);
  Result := False;
end;

function TdxTableCellViewInfo.IsEndOnNextTableViewInfo: Boolean;
var
  AEndRowIndex: Integer;
begin
  if TableViewInfo.NextTableViewInfo = nil then
    Exit(False);
  if FBottomAnchorIndex <> TableViewInfo.Anchors.Count - 1 then
    Exit(False);
  AEndRowIndex := StartRowIndex + RowSpan - 1;
  if AEndRowIndex > TableViewInfo.BottomRowIndex then
    Exit(True);
  if (AEndRowIndex = TableViewInfo.BottomRowIndex) and
    (TableViewInfo.BottomRowIndex = TableViewInfo.NextTableViewInfo.TopRowIndex) then
    Exit(True);
  Result := False;
end;

function TdxTableCellViewInfo.GetActualBottomCellBorder: TdxBorderBase;
var
  ACell: TdxTableCell;
begin
  ACell := GetBottomCell;
  Result := ACell.GetActualBottomCellBorder;
end;

function TdxTableCellViewInfo.GetBottomCell: TdxTableCell;
var
  ABottomRowIndex, AColumnIndex, ACellIndex: Integer;
begin
  if FCell.VerticalMerging = TdxMergingState.None then
    Exit(FCell);
  ABottomRowIndex := TableViewInfo.TopRowIndex + BottomAnchorIndex - 1;

  AColumnIndex := FCell.Row.GridBefore;
  ACellIndex := 0;
  while FCell.Row.Cells[ACellIndex] <> FCell do
  begin
    Inc(AColumnIndex, FCell.Row.Cells[ACellIndex].ColumnSpan);
    Inc(ACellIndex);
  end;
  ACellIndex := 0;
  Dec(AColumnIndex, FCell.Table.Rows[ABottomRowIndex].GridBefore);
  while AColumnIndex > 0 do
  begin
    Dec(AColumnIndex, FCell.Table.Rows[ABottomRowIndex].Cells[ACellIndex].ColumnSpan);
    Inc(ACellIndex);
  end;
  Assert(AColumnIndex = 0);
  Result := FCell.Table.Rows[ABottomRowIndex].Cells[ACellIndex];
end;

class function TdxTableCellViewInfo.GetRows(ARows: TdxRowCollection; ACell: TdxTableCell): TdxRowCollection;
var
  APieceTable: TdxPieceTable;
  AComparable: TdxBoxAndLogPositionComparable;
  I, AFirstRowIndex, ALastRowIndex: Integer;
begin
  APieceTable := TdxPieceTable(ACell.PieceTable);

  AComparable := TdxBoxAndLogPositionComparable.Create(APieceTable,
    APieceTable.Paragraphs[ACell.StartParagraphIndex].LogPosition);
  try
    if not TdxAlgorithms1<TdxBoxBase>.BinarySearch(ARows, AComparable, AFirstRowIndex) then
      Assert((AFirstRowIndex >= ARows.Count) or (ARows[AFirstRowIndex].Paragraph.Index >= ACell.StartParagraphIndex));
  finally
    AComparable.Free;
  end;

  AComparable := TdxBoxAndLogPositionComparable.Create(APieceTable,
    APieceTable.Paragraphs[ACell.EndParagraphIndex].EndLogPosition);
  try
    if not TdxAlgorithms1<TdxBoxBase>.BinarySearch(ARows, AComparable, ALastRowIndex) then
    begin
      Dec(ALastRowIndex);
      Assert((AFirstRowIndex >= ARows.Count) or (ARows[AFirstRowIndex].Paragraph.Index >= ACell.StartParagraphIndex));
    end;
  finally
    AComparable.Free;
  end;

  Result := TdxRowCollection.Create;
  for I := AFirstRowIndex to ALastRowIndex do
    Result.Add(ARows[I]);
end;

function TdxTableCellViewInfo.GetRows(AColumn: TdxColumn): TdxRowCollection;
begin
  Result := GetRows(AColumn.Rows, FCell);
end;

function TdxTableCellViewInfo.GetFirstRowIndex(AColumn: TdxColumn): Integer;
var
  APieceTable: TdxPieceTable;
  AParagraphCell: TdxCustomTableCell;
  AComparable: TdxBoxAndLogPositionComparable;
begin
  APieceTable := TdxPieceTable(FCell.PieceTable);
  AComparable := TdxBoxAndLogPositionComparable.Create(APieceTable,
    APieceTable.Paragraphs[Cell.StartParagraphIndex].LogPosition);
  try
    if not TdxAlgorithms1<TdxBoxBase>.BinarySearch(AColumn.Rows, AComparable, Result) then
    begin
      if Result >= AColumn.Rows.Count then
        Exit(-1);
      AParagraphCell := AColumn.Rows[Result].Paragraph.GetCellCore;
      while (AParagraphCell <> nil) and (AParagraphCell <> FCell) do
        AParagraphCell := AParagraphCell.GetTableCore.GetParentCellCore;
      if AParagraphCell <> FCell then
        Exit(-1);
    end;
  finally
    AComparable.Free;
  end;
end;

function TdxTableCellViewInfo.GetFirstRow(AColumn: TdxColumn): TdxRow;
var
  AIndex: Integer;
begin
  AIndex := GetFirstRowIndex(AColumn);
  if AIndex < 0 then
    Result := nil
  else
    Result := AColumn.Rows[AIndex];
end;

function TdxTableCellViewInfo.GetLastRowIndex(AColumn: TdxColumn): Integer;
var
  APieceTable: TdxPieceTable;
  AComparable: TdxBoxAndLogPositionComparable;
begin
  APieceTable := TdxPieceTable(Cell.PieceTable);
  AComparable := TdxBoxAndLogPositionComparable.Create(APieceTable,
    APieceTable.Paragraphs[Cell.EndParagraphIndex].EndLogPosition);
  try
    if not TdxAlgorithms1<TdxBoxBase>.BinarySearch(AColumn.Rows, AComparable, Result) then
    begin
      Dec(Result);
      if Result >= 0 then
        Assert(AColumn.Rows[Result].Paragraph.Index <= Cell.EndParagraphIndex)
      else
        Exit(-1);
    end;
  finally
    AComparable.Free;
  end;
end;

function TdxTableCellViewInfo.GetLastRow(AColumn: TdxColumn): TdxRow;
var
  AIndex: Integer;
begin
  AIndex := GetLastRowIndex(AColumn);
  if AIndex < 0 then
    Result := nil
  else
    Result := AColumn.Rows[AIndex];
end;

function TdxTableCellViewInfo.GetBackgroundBounds: TRect;
begin
  Result := TableViewInfo.GetCellBounds(Self);
end;

function TdxTableCellViewInfo.GetBounds: TRect;
begin
  Result := TableViewInfo.GetCellBounds(Self);
end;

function TdxTableCellViewInfo.GetTableRow: TdxTableRowViewInfoBase;
var
  AModelRowIndex, ALayoutRowIndex: Integer;
begin
  AModelRowIndex := Cell.Table.Rows.IndexOf(Cell.Row);
  ALayoutRowIndex := AModelRowIndex - TableViewInfo.TopRowIndex;
  ALayoutRowIndex := Max(0, Min(ALayoutRowIndex, TableViewInfo.Rows.Count - 1));
  Assert(ALayoutRowIndex >= 0);
  Result := TableViewInfo.Rows[ALayoutRowIndex];
end;

procedure TdxTableCellViewInfo.AddInnerTable(ATableViewInfo: TdxTableViewInfo);
begin
  Assert(ATableViewInfo.Table.ParentCell = Cell);
  InnerTables.Add(ATableViewInfo);
  TableViewInfo.Column.Children.Add(ATableViewInfo.Column);
end;

procedure TdxTableCellViewInfo.RemoveInnerTable(ATableViewInfo: TdxTableViewInfo);
begin
  TableViewInfo.Column.Children.Remove(ATableViewInfo.Column);
  InnerTables.Remove(ATableViewInfo);
end;

function TdxTableCellViewInfo.GetSingleColumn: Boolean;
begin
  Result := (not IsStartOnPreviousTableViewInfo) and (not IsEndOnNextTableViewInfo);
end;

function TdxTableCellViewInfo.GetLeftBorder: TdxBorderBase;
begin
  Result := Cell.GetActualLeftCellBorder;
end;

function TdxTableCellViewInfo.GetRightBorder: TdxBorderBase;
begin
  Result := Cell.GetActualRightCellBorder;
end;

function TdxTableCellViewInfo.GetTopBorder: TdxBorderBase;
begin
  Result := Cell.GetActualTopCellBorder;
end;

{ TdxParagraphBoxCollection }

destructor TdxParagraphBoxCollection.Destroy;
begin
  NumberingListBox := nil;
  inherited Destroy;
end;

procedure TdxParagraphBoxCollection.Add(ABox: TdxBox);
begin
  inherited Add(ABox);
  if ABox is TdxLayoutDependentTextBox then
    FContainsLayoutDependentBox := True;
  if ABox is TdxFloatingObjectAnchorBox then
    FContainsFloatingObjectAnchorBox := True;
end;

procedure TdxParagraphBoxCollection.Clear;
begin
  inherited Clear;
  NumberingListBox := nil;
  FContainsLayoutDependentBox := False;
  FContainsFloatingObjectAnchorBox := False;
end;

function TdxParagraphBoxCollection.ContainsFloatingObjectAnchorBox: Boolean;
begin
  Result := FContainsFloatingObjectAnchorBox;
end;

procedure TdxParagraphBoxCollection.InvalidateBoxes;
begin
  inherited InvalidateBoxes;
  NumberingListBox := nil;
end;

procedure TdxParagraphBoxCollection.OffsetRunIndices(ADelta: Integer);
begin
  inherited OffsetRunIndices(ADelta);
  if FNumberingListBox <> nil then
    FNumberingListBox.OffsetRunIndices(ADelta);
end;

function TdxParagraphBoxCollection.GetIsValid: Boolean;
begin
  Result := inherited GetIsValid and not FContainsLayoutDependentBox;
end;

procedure TdxParagraphBoxCollection.SetNumberingListBox(const Value: TdxNumberingListBox);
begin
  if FNumberingListBox = Value then
    Exit;
  TdxNumberingListBox.Release(FNumberingListBox);
  FNumberingListBox := Value;
  TdxNumberingListBox.AddReference(FNumberingListBox);
end;

{ TdxTableCellVerticalBorderCalculator }

constructor TdxTableCellVerticalBorderCalculator.Create(ATable: TdxTable);
begin
  inherited Create;
  FTable := ATable;
end;

class function TdxTableCellVerticalBorderCalculator.GetCellByColumnIndex(ARow: TdxTableRow;
  AStartColumnIndex: Integer): TdxTableCell;
var
  I, AColumnIndex: Integer;
  ACurrentCell: TdxTableCell;
  ACells: TdxTableCellCollection;
begin
  AColumnIndex := ARow.GridBefore;
  ACells := ARow.Cells;
  for I := 0 to ACells.Count - 1 do
  begin
    ACurrentCell := ACells[I];
    if (AStartColumnIndex >= AColumnIndex) and (AStartColumnIndex < AColumnIndex + ACurrentCell.ColumnSpan) then
      Exit(ACurrentCell);
    Inc(AColumnIndex, ACurrentCell.ColumnSpan);
  end;
  Result := nil;
end;

class function TdxTableCellVerticalBorderCalculator.GetCellByEndColumnIndex(ARow: TdxTableRow;
  AEndColumnIndex: Integer): TdxTableCell;
var
  ACellByColumnIndex: TdxTableCell;
  ACellIndex: Integer;
begin
  ACellByColumnIndex := GetCellByColumnIndex(ARow, AEndColumnIndex);
  if ACellByColumnIndex = nil then
    Exit(nil);

  if GetStartColumnIndex(ACellByColumnIndex, False) + ACellByColumnIndex.ColumnSpan - 1 <= AEndColumnIndex then
    Exit(ACellByColumnIndex);
  ACellIndex := ARow.Cells.IndexOf(ACellByColumnIndex);
  if ACellIndex <> 0 then
    Result := ARow.Cells[ACellIndex - 1]
  else
    Result := nil;
end;

class function TdxTableCellVerticalBorderCalculator.GetCellByStartColumnIndex(ARow: TdxTableRow;
  AStartColumnIndex: Integer; ALayoutIndex: Boolean): TdxTableCell;
var
  ACell: TdxTableCell;
  AColumnIndex, ACellIndex: Integer;
begin
  if ALayoutIndex then
    AColumnIndex := ARow.LayoutProperties.GridBefore
  else
    AColumnIndex := ARow.GridBefore;
  ACellIndex := 0;
  while (AColumnIndex < AStartColumnIndex) and (ACellIndex < ARow.Cells.Count) do
  begin
    ACell := ARow.Cells[ACellIndex];
    if ALayoutIndex then
      Inc(AColumnIndex, ACell.LayoutProperties.ColumnSpan)
    else
      Inc(AColumnIndex, ACell.ColumnSpan);
    Inc(ACellIndex);
  end;
  if ACellIndex < ARow.Cells.Count then
    Result := ARow.Cells[ACellIndex]
  else
    Result := nil;
end;

class function TdxTableCellVerticalBorderCalculator.GetCellsByIntervalColumnIndex(ARow: TdxTableRow; AStartColumnIndex,
  AEndColumnIndex: Integer): TdxTableCellList;
var
  ACell: TdxTableCell;
  ACellStartColumnIndex: Integer;
begin
  Result := TdxTableCellList.Create;
  while AStartColumnIndex <= AEndColumnIndex do
  begin
    ACell := GetCellByColumnIndex(ARow, AStartColumnIndex);
    if ACell = nil then
      Exit;
    Result.Add(ACell);
    ACellStartColumnIndex := GetStartColumnIndex(ACell, False);
    Inc(AStartColumnIndex, AStartColumnIndex - ACellStartColumnIndex + ACell.ColumnSpan);
  end;
end;

function TdxTableCellVerticalBorderCalculator.GetLeftBorder(ABorderCalculator: TdxTableBorderCalculator;
  ACell: TdxTableCell): TdxBorderInfo;
var
  ACellIndex: Integer;
  APrevCellBorder: TdxBorderBase;
begin
  if (ACell.Row.CellSpacing.&Type = TdxWidthUnitType.ModelUnits) and (ACell.Row.CellSpacing.Value > 0) then
    Exit(ACell.GetActualLeftCellBorder.Info);

  ACellIndex := ACell.Row.Cells.IndexOf(ACell);
  if ACellIndex > 0 then
  begin
    APrevCellBorder := ACell.Row.Cells[ACellIndex - 1].GetActualRightCellBorder;
    Result := ABorderCalculator.GetVerticalBorderSource(FTable, APrevCellBorder.Info, ACell.GetActualLeftCellBorder.Info);
  end
  else
    Result := ACell.GetActualLeftCellBorder.Info;
end;

function TdxTableCellVerticalBorderCalculator.GetLeftBorderWidth(ABorderCalculator: TdxTableBorderCalculator;
  ACell: TdxTableCell; ALayoutIndex: Boolean): TdxModelUnit;
var
  I, AStartColumnIndex: Integer;
  AVerticalSpanCells: TdxTableCellList;
begin
  AStartColumnIndex := GetStartColumnIndex(ACell, ALayoutIndex);
  AVerticalSpanCells := GetVerticalSpanCells(ACell, AStartColumnIndex, ALayoutIndex);
  try
    Result := 0;
    for I := 0 to AVerticalSpanCells.Count - 1 do
      Result := Math.Max(ABorderCalculator.GetActualWidth(GetLeftBorder(ABorderCalculator, AVerticalSpanCells[I])), Result);
  finally
    AVerticalSpanCells.Free;
  end;
end;

function TdxTableCellVerticalBorderCalculator.GetRightBorder(ABorderCalculator: TdxTableBorderCalculator;
  ACell: TdxTableCell): TdxBorderInfo;
var
  ACellIndex: Integer;
  ANextCellBorder: TdxBorderInfo;
begin
  if (ACell.Row.CellSpacing.&Type = TdxWidthUnitType.ModelUnits) and (ACell.Row.CellSpacing.Value > 0) then
    Exit(ACell.GetActualRightCellBorder.Info);

  ACellIndex := ACell.Row.Cells.IndexOf(ACell);
  if ACellIndex + 1 < ACell.Row.Cells.Count then
  begin
    ANextCellBorder := ACell.Row.Cells[ACellIndex + 1].GetActualLeftCellBorder.Info;
    Result := ABorderCalculator.GetVerticalBorderSource(FTable, ANextCellBorder, ACell.GetActualRightCellBorder.Info);
  end
  else
    Result := ACell.GetActualRightCellBorder.Info;
end;

function TdxTableCellVerticalBorderCalculator.GetRightBorderWidth(ABorderCalculator: TdxTableBorderCalculator;
  ACell: TdxTableCell): TdxModelUnit;
var
  I, AStartColumnIndex: Integer;
  AVerticalSpanCells: TdxTableCellList;
begin
  AStartColumnIndex := GetStartColumnIndex(ACell, True);
  AVerticalSpanCells := GetVerticalSpanCells(ACell, AStartColumnIndex, True);
  try
    Result := 0;
    for I := 0 to AVerticalSpanCells.Count - 1 do
      Result := Math.Max(ABorderCalculator.GetActualWidth(GetRightBorder(ABorderCalculator, AVerticalSpanCells[I])), Result);
  finally
    AVerticalSpanCells.Free;
  end;
end;

class function TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ACell: TdxTableCell;
  ALayoutIndex: Boolean): Integer;
var
  ARow: TdxTableRow;
  ACurrentCell: TdxTableCell;
  ACells: TdxTableCellCollection;
  AColumnIndex, ACellIndex, ACount: Integer;
begin
  ARow := ACell.Row;
  if ALayoutIndex then
    AColumnIndex := ARow.LayoutProperties.GridBefore
  else
    AColumnIndex := ARow.GridBefore;
  ACellIndex := 0;
  ACells := ARow.Cells;
  ACount := ACells.Count;
  while (ACellIndex < ACount) and (ACells[ACellIndex] <> ACell) do
  begin
    ACurrentCell := ACells[ACellIndex];
    if ALayoutIndex then
      Inc(AColumnIndex, ACurrentCell.LayoutProperties.ColumnSpan)
    else
      Inc(AColumnIndex, ACurrentCell.ColumnSpan);
    Inc(ACellIndex);
  end;
  Result := AColumnIndex;
end;

class function TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(ACell: TdxTableCell;
  ALayoutIndex: Boolean): TdxTableCellList;
var
  AStartColumnIndex: Integer;
begin
  AStartColumnIndex := GetStartColumnIndex(ACell, ALayoutIndex);
  Result := GetVerticalSpanCells(ACell, AStartColumnIndex, ALayoutIndex);
end;

class function TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(ACell: TdxTableCell;
  AStartColumnIndex: Integer; ALayoutIndex: Boolean): TdxTableCellList;
var
  ATable: TdxTable;
  ARowIndex: Integer;
  ARow: TdxTableRow;
  ARowCell: TdxTableCell;
begin
  Result := TdxTableCellList.Create;
  Result.Add(ACell);
  if ACell.VerticalMerging <> TdxMergingState.Restart then
    Exit;
  ATable := ACell.Table;
  for ARowIndex := ATable.Rows.IndexOf(ACell.Row) + 1 to ATable.Rows.Count - 1 do
  begin
    ARow := ATable.Rows[ARowIndex];
    ARowCell := GetCellByStartColumnIndex(ARow, AStartColumnIndex, ALayoutIndex);
    if (ARowCell <> nil) and (ARowCell.VerticalMerging = TdxMergingState.Continue) then
      Result.Add(ARowCell)
    else
      Break;
  end;
end;

{ TdxTableViewInfoCollection }

procedure TdxTableViewInfoCollection.ExportTo(const AExporter: IdxDocumentLayoutExporter);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ExportTo(AExporter);
end;

procedure TdxTableViewInfoCollection.MoveVertically(ADeltaY: TdxLayoutUnit);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].MoveVertically(ADeltaY);
end;

procedure TdxTableViewInfoCollection.ExportBackground(const AExporter: IdxDocumentLayoutExporter);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Table.NestedLevel = 0 then
      Items[I].ExportBackground(AExporter);
end;

{ TdxTableRowViewInfoCollection }

constructor TdxTableRowViewInfoCollection.Create(ATableViewInfo: TdxTableViewInfo);
begin
  inherited Create;
  Assert(ATableViewInfo <> nil);
  FTableViewInfo := ATableViewInfo;
end;

function TdxTableRowViewInfoCollection.Last: TdxTableRowViewInfoBase;
begin
  if Count > 0 then
    Result := Self[Count - 1]
  else
    Result := nil;
end;

function TdxTableRowViewInfoCollection.First: TdxTableRowViewInfoBase;
begin
  if Count > 0 then
    Result := Self[0]
  else
    Result := nil;
end;

procedure TdxTableRowViewInfoCollection.RemoveRows(AStartRowIndex, ARowCount: Integer);
var
  I: Integer;
begin
  for I := AStartRowIndex + ARowCount - 1 downto AStartRowIndex do
    Delete(I);
end;

procedure TdxTableRowViewInfoCollection.ShiftForward(ADelta: Integer);
var
  I, AInitialRowCount: Integer;
begin
  Assert(ADelta >= 0);
  AInitialRowCount := Count;
  for I := 0 to ADelta - 1 do
    Add(nil);
  for I := AInitialRowCount - 1 downto 0 do
    Self[I + ADelta] := Self[I];
  for I := 0 to ADelta - 1 do
    Self[I] := nil;
end;

{ TdxTableBorderViewInfoBase }

procedure TdxTableBorderViewInfoBase.ExportTo(ATableViewInfo: TdxTableViewInfo;
  const AExporter: IdxDocumentLayoutExporter);
var
  ABounds: TRect;
begin
  ABounds := GetBounds(ATableViewInfo);
  AExporter.ExportTableBorder(Self, ABounds);
end;

function TdxTableBorderViewInfoBase.HasEndCorner: Boolean;
begin
  Result := (EndCorner <> nil) and not (EndCorner is TdxNoneLineCornerViewInfo);
end;

function TdxTableBorderViewInfoBase.HasStartCorner: Boolean;
begin
  Result := (StartCorner <> nil) and not (StartCorner is TdxNoneLineCornerViewInfo);
end;

{ TdxParagraphHorizontalBorderViewInfo }

constructor TdxParagraphHorizontalBorderViewInfo.Create(ABorderInfo: TdxBorderInfo;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter; ACornerAtLeft: TdxCornerViewInfoBase;
  ACornerAtRight: TdxCornerViewInfoBase);
begin
  inherited Create;
  FBorderInfo := ABorderInfo;
  FConverter := AConverter;
  FCornerAtLeft := ACornerAtLeft;
  FCornerAtRight := ACornerAtRight;
end;

function TdxParagraphHorizontalBorderViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := FBorderInfo;
end;

function TdxParagraphHorizontalBorderViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Horizontal;
end;

function TdxParagraphHorizontalBorderViewInfo.GetConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := FConverter;
end;

function TdxParagraphHorizontalBorderViewInfo.GetStartCorner: TdxCornerViewInfoBase;
begin
  Result := FCornerAtLeft;
end;

function TdxParagraphHorizontalBorderViewInfo.GetEndCorner: TdxCornerViewInfoBase;
begin
  Result := FCornerAtRight;
end;

function TdxParagraphHorizontalBorderViewInfo.GetBounds(ATableViewInfo: TdxTableViewInfo): TRect;
begin
  Result.Empty;
end;

{ TdxTableCellHorizontalBorderViewInfo }

constructor TdxTableCellHorizontalBorderViewInfo.Create(AAnchor: TdxTableCellVerticalAnchor;
  ABorderInfo: TdxHorizontalCellBordersInfo; AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ACornerAtLeft, ACornerAtRight: TdxCornerViewInfoBase);
begin
  inherited Create;
  FAnchor := AAnchor;
  FBorderInfo := ABorderInfo;
  FConverter := AConverter;
  FCornerAtLeft := ACornerAtLeft;
  FCornerAtRight := ACornerAtRight;
end;

function TdxTableCellHorizontalBorderViewInfo.GetBounds(ATableViewInfo: TdxTableViewInfo): TRect;
var
  Y, ALeft, ARight: Integer;
begin
  Y := FAnchor.VerticalPosition;
  ALeft := ATableViewInfo.GetColumnLeft(FBorderInfo.StartColumnIndex);
  ARight := ATableViewInfo.GetColumnRight(FBorderInfo.EndColumnIndex);

  Result.InitSize(ALeft, Y, ARight - ALeft, 0);
end;

function TdxTableCellHorizontalBorderViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := FBorderInfo.Border.Info;
end;

function TdxTableCellHorizontalBorderViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Horizontal;
end;

function TdxTableCellHorizontalBorderViewInfo.GetStartCorner: TdxCornerViewInfoBase;
begin
  Result := FCornerAtLeft;
end;

function TdxTableCellHorizontalBorderViewInfo.GetEndCorner: TdxCornerViewInfoBase;
begin
  Result := FCornerAtRight;
end;

function TdxTableCellHorizontalBorderViewInfo.GetConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := FConverter;
end;

{ TdxTableBorderWithSpacingViewInfoBase }

constructor TdxTableBorderWithSpacingViewInfoBase.Create(ARowViewInfo: TdxTableRowViewInfoWithCellSpacing);
begin
  inherited Create;
  Assert(ARowViewInfo <> nil);
  FRowViewInfo := ARowViewInfo;
end;

function TdxTableBorderWithSpacingViewInfoBase.GetConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := FRowViewInfo.Row.DocumentModel.ToDocumentLayoutUnitConverter;
end;

function TdxTableBorderWithSpacingViewInfoBase.GetEndCorner: TdxCornerViewInfoBase;
begin
  Result := RightCorner;
end;

function TdxTableBorderWithSpacingViewInfoBase.GetStartCorner: TdxCornerViewInfoBase;
begin
  Result := LeftCorner;
end;

function TdxTableBorderWithSpacingViewInfoBase.GetTableViewInfo: TdxTableViewInfo;
begin
  Result := FRowViewInfo.TableViewInfo;
end;

{ TdxTableCellVerticalBorderViewInfo }

constructor TdxTableCellVerticalBorderViewInfo.Create(ARow: TdxTableRowViewInfoBase; ABorder: TdxBorderInfo;
  ALayoutBorderIndex, AModelBorderIndex: Integer; AConverter: TdxDocumentModelUnitToLayoutUnitConverter);
begin
  inherited Create;
  FConverter := AConverter;
  FBorder := ABorder;
  FRow := ARow;
  FLayoutBorderIndex := ALayoutBorderIndex;
  FModelBorderIndex := AModelBorderIndex;
end;

function TdxTableCellVerticalBorderViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := FBorder;
end;

function TdxTableCellVerticalBorderViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Vertical;
end;

function TdxTableCellVerticalBorderViewInfo.GetBounds(ATableViewInfo: TdxTableViewInfo): TRect;
var
  ATop, ABottom, ALeft: Integer;
begin
  ATop := FRow.TopAnchor.VerticalPosition;
  ABottom := FRow.BottomAnchor.VerticalPosition;
  ALeft := ATableViewInfo.GetVerticalBorderPosition(FLayoutBorderIndex);
  Result := TRect.CreateSize(ALeft, ATop, 0, ABottom - ATop);
end;

function TdxTableCellVerticalBorderViewInfo.GetConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := FConverter;
end;

function TdxTableCellVerticalBorderViewInfo.GetEndCorner: TdxCornerViewInfoBase;
begin
  Result := CornerAtBottom;
end;

function TdxTableCellVerticalBorderViewInfo.GetStartCorner: TdxCornerViewInfoBase;
begin
  Result := CornerAtTop;
end;

{ TdxLeftRightTableBorderWithSpacingViewInfoBase }

function TdxLeftRightTableBorderWithSpacingViewInfoBase.GetBounds(ATableViewInfo: TdxTableViewInfo): TRect;
var
  ATop, ACellSpacing, ALeft, ARight, ABottom: Integer;
  ABottomAnchor: TdxTableCellVerticalAnchor;
  AFirstCellViewInfo, ALastCellViewInfo: TdxTableCellViewInfo;
begin
  ATop := RowViewInfo.TopAnchor.VerticalPosition;

  if RowViewInfo.Previous is TdxTableRowViewInfoWithCellSpacing then
  begin
    ACellSpacing := Converter.ToLayoutUnits(TdxTableRowViewInfoWithCellSpacing(RowViewInfo.Previous).CellSpacing);
    ATop := ATop + ACellSpacing div 2;
  end;

  ACellSpacing := Converter.ToLayoutUnits(RowViewInfo.CellSpacing);

  AFirstCellViewInfo := RowViewInfo.Cells[0];
  ALeft := AFirstCellViewInfo.Left - ACellSpacing;
  ALastCellViewInfo := RowViewInfo.Cells.Last;
  ARight := ALastCellViewInfo.Left + ALastCellViewInfo.Width + ACellSpacing;

  ABottomAnchor := RowViewInfo.BottomAnchor;
  ABottom := ABottomAnchor.VerticalPosition;
  if RowViewInfo.Next <> nil then
    ABottom := ABottom + ACellSpacing - ACellSpacing div 2
  else
    ABottom := ABottom + ACellSpacing;

  Result.Init(ALeft, ATop, ARight, ABottom);
end;

{ TdxLeftTableBorderWithSpacingViewInfo }

function TdxLeftTableBorderWithSpacingViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := TableViewInfo.GetActualLeftBorder.Info;
end;

function TdxLeftTableBorderWithSpacingViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Left;
end;

{ TdxRightTableBorderWithSpacingViewInfo }

function TdxRightTableBorderWithSpacingViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := TableViewInfo.GetActualRightBorder.Info;
end;

function TdxRightTableBorderWithSpacingViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Right;
end;

{ TdxTopTableBorderWithSpacingViewInfo }

function TdxTopTableBorderWithSpacingViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := TableViewInfo.GetActualTopBorder.Info;
end;

function TdxTopTableBorderWithSpacingViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Top;
end;

function TdxTopTableBorderWithSpacingViewInfo.GetBounds(ATableViewInfo: TdxTableViewInfo): TRect;
var
  ATop, ACellSpacing, ALeft, ARight: Integer;
  AFirstCellViewInfo, ALastCellViewInfo: TdxTableCellViewInfo;
begin
  ATop := ATableViewInfo.TopAnchor.VerticalPosition;

  AFirstCellViewInfo := RowViewInfo.Cells[0];
  ACellSpacing := Converter.ToLayoutUnits(RowViewInfo.CellSpacing);
  ALeft := AFirstCellViewInfo.Left - ACellSpacing;

  ALastCellViewInfo := RowViewInfo.Cells.Last;
  ARight := ALastCellViewInfo.Left + ALastCellViewInfo.Width + ACellSpacing;

  Result.Init(ALeft, ATop, ARight, ATop);
end;

{ TdxBottomTableBorderWithSpacingViewInfo }

function TdxBottomTableBorderWithSpacingViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Bottom;
end;

function TdxBottomTableBorderWithSpacingViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := TableViewInfo.GetActualBottomBorder.Info;
end;

function TdxBottomTableBorderWithSpacingViewInfo.GetBounds(ATableViewInfo: TdxTableViewInfo): TRect;
var
  ACellSpacing, ABottom, ALeft, ARight: Integer;
  ABottomAnchor: TdxTableCellVerticalAnchor;
  AFirstCellViewInfo, ALastCellViewInfo: TdxTableCellViewInfo;
begin
  ABottomAnchor := ATableViewInfo.BottomAnchor;
  ACellSpacing := Converter.ToLayoutUnits(RowViewInfo.CellSpacing);
  ABottom := ABottomAnchor.VerticalPosition + ACellSpacing;

  AFirstCellViewInfo := RowViewInfo.Cells[0];
  ALeft := AFirstCellViewInfo.Left - ACellSpacing;

  ALastCellViewInfo := RowViewInfo.Cells.Last;
  ARight := ALastCellViewInfo.Left + ALastCellViewInfo.Width + ACellSpacing;

  Result.Init(ALeft, ABottom, ARight, ABottom);
end;

{ TdxTableCellBorderViewInfoBase }

constructor TdxTableCellBorderViewInfoBase.Create(ASourceCell: TdxTableCellViewInfo);
begin
  inherited Create;
  Assert(ASourceCell <> nil);
  FSourceCell := ASourceCell;
end;

function TdxTableCellBorderViewInfoBase.GetBounds(ATableViewInfo: TdxTableViewInfo): TRect;
begin
  Result := GetBoundsCore(ATableViewInfo);
end;

function TdxTableCellBorderViewInfoBase.GetBoundsCore(ATableViewInfo: TdxTableViewInfo): TRect;
begin
  Result := ATableViewInfo.GetCellBounds(FSourceCell);
end;

function TdxTableCellBorderViewInfoBase.GetConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := FSourceCell.Cell.DocumentModel.ToDocumentLayoutUnitConverter;
end;

{ TdxTableCellVerticalBorderViewInfoBase }

destructor TdxTableCellVerticalBorderViewInfoBase.Destroy;
begin
  FTopCorner.Free;
  FBottomCorner.Free;
  inherited Destroy;
end;

function TdxTableCellVerticalBorderViewInfoBase.GetEndCorner: TdxCornerViewInfoBase;
begin
  Result := BottomCorner;
end;

function TdxTableCellVerticalBorderViewInfoBase.GetStartCorner: TdxCornerViewInfoBase;
begin
  Result := TopCorner;
end;

{ TdxLeftTableCellBorderViewInfo }

function TdxLeftTableCellBorderViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := SourceCell.LeftBorder.Info;
end;

function TdxLeftTableCellBorderViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Left;
end;

{ TdxRightTableCellBorderViewInfo }

function TdxRightTableCellBorderViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := SourceCell.RightBorder.Info;
end;

function TdxRightTableCellBorderViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Right;
end;

{ TdxTableCellHorizontalBorderViewInfoBase }

function TdxTableCellHorizontalBorderViewInfoBase.GetEndCorner: TdxCornerViewInfoBase;
begin
  Result := RightCorner;
end;

function TdxTableCellHorizontalBorderViewInfoBase.GetStartCorner: TdxCornerViewInfoBase;
begin
  Result := LeftCorner;
end;

{ TdxTopTableCellBorderViewInfo }

function TdxTopTableCellBorderViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := SourceCell.TopBorder.Info;
end;

function TdxTopTableCellBorderViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Top;
end;

{ TdxBottomTableCellBorderViewInfo }

function TdxBottomTableCellBorderViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := SourceCell.BottomBorder.Info;
end;

function TdxBottomTableCellBorderViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := TdxBorderTypes.Bottom;
end;

{ TdxTableBorderInfo }

constructor TdxTableBorderInfo.Create(const ACompoundArray: TArray<Integer>; AWidthDivider: Integer);
var
  I, ALastIndex: Integer;
begin
  CompoundArray := ACompoundArray;
  FWidthDivider := AWidthDivider;
  ALastIndex := Length(ACompoundArray) - 1;
  LineCount := Trunc(Length(ACompoundArray) / 2);
  FWidthMultiplier := CompoundArray[ALastIndex];
  SetLength(DrawingCompoundArray, Length(ACompoundArray));
  for I := 0 to ALastIndex do
    DrawingCompoundArray[I] := ACompoundArray[I] / FWidthMultiplier;
end;

function TdxTableBorderInfo.GetActualWidth(ABorderWidth: Integer): Integer;
begin
  Result := MulDiv(ABorderWidth, FWidthMultiplier, FWidthDivider);
end;

{ TdxTableBorderCalculator }

class procedure TdxTableBorderCalculator.AddLineStyleInfo(ALineStyle: TdxBorderLineStyle;
  const ACompoundArray: TArray<Integer>; AWidthDivider: Integer);
begin
{$IFDEF DELPHIXE2}
  LineStyleInfos[ALineStyle] := TdxTableBorderInfo.Create(ACompoundArray, AWidthDivider);
{$ELSE}
  LineStyleInfos[Ord(ALineStyle)] := TdxTableBorderInfo.Create(ACompoundArray, AWidthDivider);
{$ENDIF}
end;

function TdxTableBorderCalculator.CalculateWeight(ABorder: TdxBorderInfo): Integer;
var
  AInfo: TdxTableBorderInfo;
  ABorderStyle: TdxBorderLineStyle;
begin
  ABorderStyle := ABorder.Style;
  if not LineStyleInfosTryGetValue(GetActualBorderLineStyle(ABorderStyle), AInfo) then
  begin
    if ABorderStyle = TdxBorderLineStyle.Disabled then
      Result :=  MaxInt
    else
      Result := 0;
  end
  else
    Result := AInfo.LineCount * Ord(ABorderStyle);
end;

function TdxTableBorderCalculator.GetActualBorderLineStyle(ABorderLineStyle: TdxBorderLineStyle): TdxBorderLineStyle;
begin
  if (ABorderLineStyle = TdxBorderLineStyle.None) or (ABorderLineStyle = TdxBorderLineStyle.Nil) or
    (ABorderLineStyle = TdxBorderLineStyle.Disabled) or LineStyleInfosContainsKey(ABorderLineStyle) then
    Result := ABorderLineStyle
  else
    Result := TdxBorderLineStyle.Single;
end;

class function TdxTableBorderCalculator.LineStyleInfosContainsKey(AKey: TdxBorderLineStyle): Boolean;
begin
  Result := (TdxBorderLineStyle.Single <= AKey) and (AKey <= TdxBorderLineStyle.Inset);
end;

class function TdxTableBorderCalculator.LineStyleInfosTryGetValue(AKey: TdxBorderLineStyle;
  var AInfo: TdxTableBorderInfo): Boolean;
begin
  Result := LineStyleInfosContainsKey(AKey);
  if Result then
{$IFDEF DELPHIXE2}
    AInfo := LineStyleInfos[AKey];
{$ELSE}
    AInfo := LineStyleInfos[Ord(AKey)];
{$ENDIF}
end;

function TdxTableBorderCalculator.GetActualWidth(ABorder: TdxBorderBase): Integer;
begin
  Result := GetActualWidth(ABorder.Info);
end;

function TdxTableBorderCalculator.GetActualWidth(AStyle: TdxBorderLineStyle; AWidth: Integer): Integer;
var
  AInfo: TdxTableBorderInfo;
begin
  if not LineStyleInfosTryGetValue(GetActualBorderLineStyle(AStyle), AInfo) then
    Result := 0
  else
    Result := AInfo.GetActualWidth(AWidth);
end;

class constructor TdxTableBorderCalculator.Initialize;
begin
  AddLineStyleInfo(TdxBorderLineStyle.Single, TArray<Integer>.Create(0, 1), 1);
  AddLineStyleInfo(TdxBorderLineStyle.Thick, TArray<Integer>.Create(0, 1), 1);
  AddLineStyleInfo(TdxBorderLineStyle.Double, TArray<Integer>.Create(0, 1, 2, 3), 1);
  AddLineStyleInfo(TdxBorderLineStyle.Dotted, TArray<Integer>.Create(0, 1), 1);
  AddLineStyleInfo(TdxBorderLineStyle.Dashed, TArray<Integer>.Create(0, 1), 1);
  AddLineStyleInfo(TdxBorderLineStyle.DotDash, TArray<Integer>.Create(0, 1), 1);
  AddLineStyleInfo(TdxBorderLineStyle.DotDotDash, TArray<Integer>.Create(0, 1), 1);
  AddLineStyleInfo(TdxBorderLineStyle.Triple, TArray<Integer>.Create(0, 1, 2, 3, 4, 5), 1);
  AddLineStyleInfo(TdxBorderLineStyle.ThinThickSmallGap, TArray<Integer>.Create(0, 1, 2, 10), 8);
  AddLineStyleInfo(TdxBorderLineStyle.ThickThinSmallGap, TArray<Integer>.Create(0, 8, 9, 10), 8);
  AddLineStyleInfo(TdxBorderLineStyle.ThinThickThinSmallGap, TArray<Integer>.Create(0, 1, 2, 10, 11, 12), 8);
  AddLineStyleInfo(TdxBorderLineStyle.ThinThickMediumGap, TArray<Integer>.Create(0, 1, 2, 4), 2);
  AddLineStyleInfo(TdxBorderLineStyle.ThickThinMediumGap, TArray<Integer>.Create(0, 2, 3, 4), 2);
  AddLineStyleInfo(TdxBorderLineStyle.ThinThickThinMediumGap, TArray<Integer>.Create(0, 1, 2, 4, 5, 6), 2);
  AddLineStyleInfo(TdxBorderLineStyle.ThinThickLargeGap, TArray<Integer>.Create(0, 1, 9, 11), 8);
  AddLineStyleInfo(TdxBorderLineStyle.ThickThinLargeGap, TArray<Integer>.Create(0, 2, 10, 11), 8);
  AddLineStyleInfo(TdxBorderLineStyle.ThinThickThinLargeGap, TArray<Integer>.Create(0, 1, 9, 11, 19, 20), 8);
  AddLineStyleInfo(TdxBorderLineStyle.Wave, TArray<Integer>.Create(0, 1), 1);
  AddLineStyleInfo(TdxBorderLineStyle.DoubleWave, TArray<Integer>.Create(0, 1, 1, 2), 1);
  AddLineStyleInfo(TdxBorderLineStyle.DashSmallGap, TArray<Integer>.Create(0, 1), 1);
  AddLineStyleInfo(TdxBorderLineStyle.DashDotStroked, TArray<Integer>.Create(0, 1), 1);
  AddLineStyleInfo(TdxBorderLineStyle.ThreeDEmboss, TArray<Integer>.Create(0, 1, 1, 5, 6), 4);
  AddLineStyleInfo(TdxBorderLineStyle.ThreeDEngrave, TArray<Integer>.Create(0, 1, 1, 5, 6), 4);
  AddLineStyleInfo(TdxBorderLineStyle.Outset, TArray<Integer>.Create(0, 1), 1);
  AddLineStyleInfo(TdxBorderLineStyle.Inset, TArray<Integer>.Create(0, 1), 1);
end;

function TdxTableBorderCalculator.IsVisuallyAdjacentBorder(ABorder1, ABorder2: TdxBorderInfo;
  ASameDirection: Boolean): Boolean;
var
  ABorderInfo1, ABorderInfo2: TdxTableBorderInfo;
begin
  if (ABorder1 = nil) or (ABorder2 = nil) then
    Exit(False);
  if ABorder1 = ABorder2 then
    Exit(True);
  if not LineStyleInfosTryGetValue(GetActualBorderLineStyle(ABorder1.Style), ABorderInfo1) then
    Exit(False);
  if not LineStyleInfosTryGetValue(GetActualBorderLineStyle(ABorder2.Style), ABorderInfo2) then
    Exit(False);
  if ABorderInfo1.GetActualWidth(ABorder1.Width) <> ABorderInfo2.GetActualWidth(ABorder2.Width) then
    Exit(False);
  if ABorderInfo1.LineCount <> ABorderInfo2.LineCount then
    Exit(False);
  if ASameDirection then
    Result := ABorder1.Style = ABorder2.Style
  else
    Result := True;
end;

function TdxTableBorderCalculator.GetActualWidth(ABorder: TdxBorderInfo): Integer;
begin
  Result := GetActualWidth(ABorder.Style, ABorder.Width);
end;

function TdxTableBorderCalculator.GetDrawingCompoundArray(ABorder: TdxBorderInfo): TArray<Single>;
begin
  Result := GetDrawingCompoundArray(ABorder.Style);
end;

function TdxTableBorderCalculator.GetDrawingCompoundArray(ABorderLineStyle: TdxBorderLineStyle): TArray<Single>;
var
  AInfo: TdxTableBorderInfo;
begin
  if not LineStyleInfosTryGetValue(GetActualBorderLineStyle(ABorderLineStyle), AInfo) then
    Result := nil
  else
    Result := AInfo.DrawingCompoundArray;
end;

function TdxTableBorderCalculator.GetLineCount(ABorder: TdxBorderInfo): Integer;
var
  AInfo: TdxTableBorderInfo;
begin
  if not LineStyleInfosTryGetValue(GetActualBorderLineStyle(ABorder.Style), AInfo) then
    Result := 0
  else
    Result := AInfo.LineCount;
end;

function TdxTableBorderCalculator.GetVerticalBorderSource(ATable: TdxTable; AFirstCellBorder,
  ASecondCellBorder: TdxBorderInfo): TdxBorderInfo;
var
  ALeftCellBorderWeight, ARightCellBorderWeight, ALeftCellStyleWeight, ARightCellStyleWeight,
    ALeftCellBrightness, ARightCellBrightness: Integer;
begin
  if AFirstCellBorder = nil then
    Exit(ASecondCellBorder);
  if ASecondCellBorder = nil then
    Exit(AFirstCellBorder);
  ALeftCellBorderWeight := CalculateWeight(AFirstCellBorder);
  ARightCellBorderWeight := CalculateWeight(ASecondCellBorder);
  if ALeftCellBorderWeight > ARightCellBorderWeight then
    Exit(AFirstCellBorder)
  else
    if ARightCellBorderWeight > ALeftCellBorderWeight then
      Exit(ASecondCellBorder);
  ALeftCellStyleWeight := Ord(AFirstCellBorder.Style);
  ARightCellStyleWeight := Ord(ASecondCellBorder.Style);
  if ALeftCellStyleWeight > ARightCellStyleWeight then
    Exit(AFirstCellBorder)
  else
    if ARightCellStyleWeight > ALeftCellStyleWeight then
      Exit(ASecondCellBorder);

  ALeftCellBrightness := dxGetRed(AFirstCellBorder.Color) + dxGetBlue(AFirstCellBorder.Color) +
    2 * dxGetGreen(AFirstCellBorder.Color);
  ARightCellBrightness := dxGetRed(ASecondCellBorder.Color) + dxGetBlue(ASecondCellBorder.Color) +
    2 * dxGetGreen(ASecondCellBorder.Color);
  if ALeftCellBrightness = ARightCellBrightness then
  begin
    ALeftCellBrightness := dxGetRed(AFirstCellBorder.Color) + 2 * dxGetGreen(AFirstCellBorder.Color);
    ARightCellBrightness := dxGetBlue(ASecondCellBorder.Color) + 2 * dxGetGreen(ASecondCellBorder.Color);
    if ALeftCellBrightness = ARightCellBrightness then
    begin
      ALeftCellBrightness := dxGetGreen(AFirstCellBorder.Color);
      ARightCellBrightness := dxGetGreen(ASecondCellBorder.Color);
    end;
  end;
  if ALeftCellBrightness < ARightCellBrightness then
    Exit(AFirstCellBorder)
  else
    if ARightCellBrightness < ALeftCellBrightness then
      Exit(ASecondCellBorder);
  Result := AFirstCellBorder;
end;

function TdxTableBorderCalculator.GetDrawingCompoundArray(ABorder: TdxBorderBase): TArray<Single>;
begin
  Result := GetDrawingCompoundArray(ABorder.Style);
end;

{ TdxTableCellVerticalAnchorCollection }

constructor TdxTableCellVerticalAnchorCollection.Create;
begin
  inherited Create;
  FAnchors := TdxTableCellVerticalAnchorList.Create;
end;

destructor TdxTableCellVerticalAnchorCollection.Destroy;
begin
  FAnchors.Free;
  inherited Destroy;
end;

function TdxTableCellVerticalAnchorCollection.Count: Integer;
begin
  Result := FAnchors.Count;
end;

function TdxTableCellVerticalAnchorCollection.First: TdxTableCellVerticalAnchor;
begin
  if Count > 0 then
    Result := Self[0]
  else
    Result := nil;
end;

function TdxTableCellVerticalAnchorCollection.Last: TdxTableCellVerticalAnchor;
begin
  if Count > 0 then
    Result := Self[Count - 1]
  else
    Result := nil;
end;

procedure TdxTableCellVerticalAnchorCollection.EnsureCapacity(AIndex: Integer);
var
  I: Integer;
begin
  for I := Count to AIndex do
    FAnchors.Add(nil);
end;

procedure TdxTableCellVerticalAnchorCollection.RemoveAnchors(AStartAnchorIndex, AAnchorCount: Integer);
var
  I: Integer;
begin
  for I := AStartAnchorIndex + AAnchorCount - 1 downto AStartAnchorIndex do
    if I < FAnchors.Count then
    begin
      FAnchors[I].Free;
      FAnchors.Delete(I);
    end;
end;

procedure TdxTableCellVerticalAnchorCollection.ShiftForward(AFrom, ADelta: Integer);
var
  I, AInitialAnchorCount: Integer;
begin
  Assert(ADelta >= 0);
  AInitialAnchorCount := FAnchors.Count;
  for I := 0 to ADelta - 1 do
    FAnchors.Add(nil);
  Assert(AFrom < AInitialAnchorCount);
  for I := AInitialAnchorCount - 1 downto AFrom do
    FAnchors[I + ADelta] := FAnchors[I];
  for I := 0 to ADelta - 1 do
    FAnchors[I + AFrom] := nil;
end;

function TdxTableCellVerticalAnchorCollection.GetAnchorIndex(ALogicalVerticalPoint: Integer): Integer;
var
  AComparable: TdxTableCellVerticalAnchorYComparable;
begin
  AComparable := TdxTableCellVerticalAnchorYComparable.Create(ALogicalVerticalPoint);
  try
    if not TdxAlgorithms1<TdxTableCellVerticalAnchor>.BinarySearch(FAnchors, AComparable, Result) then
      Dec(Result);
    if Result >= FAnchors.Count then
      Result := FAnchors.Count - 1;
  finally
    AComparable.Free;
  end;
end;

function TdxTableCellVerticalAnchorCollection.GetItem(Index: Integer): TdxTableCellVerticalAnchor;
begin
  if Index < Count then
    Result := FAnchors[Index]
  else
    Result := nil;
end;

function TdxTableCellVerticalAnchorCollection.GetItems: TdxTableCellVerticalAnchorList;
begin
  Result := FAnchors;
end;

procedure TdxTableCellVerticalAnchorCollection.SetItem(Index: Integer; const Value: TdxTableCellVerticalAnchor);
begin
  EnsureCapacity(Index);
  if FAnchors[Index] <> Value then
    FAnchors[Index].Free;
  FAnchors[Index] := Value;
end;

{ TdxTableCellViewInfoCollection }

function TdxTableCellViewInfoCollection.Last: TdxTableCellViewInfo;
begin
  if Count > 0 then
    Result := Self[Count - 1]
  else
    Result := nil;
end;

function TdxTableCellViewInfoCollection.First: TdxTableCellViewInfo;
begin
  if Count > 0 then
    Result := Self[0]
   else
     Result := nil;
end;

{ TdxCornerViewInfoBase }

constructor TdxCornerViewInfoBase.Create(ACornerType: TdxCornerViewInfoType);
begin
  inherited Create;
  CornerType := ACornerType;
end;

class function TdxCornerViewInfoBase.CreateCorner(AConverter: TdxDocumentModelUnitToLayoutUnitConverter; ABorderAtLeft,
  ABorderAtTop, ABorderAtRight, ABorderAtBottom: TdxBorderInfo; ACellSpacing: TdxModelUnit): TdxCornerViewInfoBase;
begin
  Result := CreateCorner(TdxCornerViewInfoType.Normal, AConverter, ABorderAtLeft, ABorderAtTop, ABorderAtRight,
    ABorderAtBottom, ACellSpacing);
end;

class function TdxCornerViewInfoBase.CreateCorner(ACornerType: TdxCornerViewInfoType;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter; ABorderAtLeft, ABorderAtTop, ABorderAtRight,
  ABorderAtBottom: TdxBorderInfo; ACellSpacing: TdxModelUnit): TdxCornerViewInfoBase;
var
  ALineCount: Integer;
  AIsResultHorizontal: Boolean;
  AWidth, AHeight: TdxModelUnit;
  ALayoutBorderHeight, ALayoutBorderWidth: Single;
  ATableBorderCalculator: TdxTableBorderCalculator;
  ABorderInfo, AActualBorderAtLeft, AActualBorderAtRight, AActualBorderAtTop, AActualBorderAtBottom: TdxBorderInfo;
begin
  ATableBorderCalculator := TdxTableBorderCalculator.Create;
  try
    ABorderInfo := ATableBorderCalculator.GetVerticalBorderSource(nil, ABorderAtLeft, ABorderAtTop);
    ABorderInfo := ATableBorderCalculator.GetVerticalBorderSource(nil, ABorderInfo, ABorderAtRight);
    ABorderInfo := ATableBorderCalculator.GetVerticalBorderSource(nil, ABorderInfo, ABorderAtBottom);
    if ABorderInfo = nil then
      Exit(TdxNoneLineCornerViewInfo.Create);
    AIsResultHorizontal := (ABorderInfo = ABorderAtLeft) or (ABorderInfo = ABorderAtRight);

    AActualBorderAtLeft := ABorderAtLeft;
    AActualBorderAtRight := ABorderAtRight;
    AActualBorderAtTop := ABorderAtTop;
    AActualBorderAtBottom := ABorderAtBottom;

    if not ATableBorderCalculator.IsVisuallyAdjacentBorder(ABorderInfo, ABorderAtLeft, AIsResultHorizontal) then
      AActualBorderAtLeft := nil;
    if not ATableBorderCalculator.IsVisuallyAdjacentBorder(ABorderInfo, ABorderAtRight, AIsResultHorizontal) then
      AActualBorderAtRight := nil;
    if not ATableBorderCalculator.IsVisuallyAdjacentBorder(ABorderInfo, ABorderAtTop, not AIsResultHorizontal) then
      AActualBorderAtTop := nil;
    if not ATableBorderCalculator.IsVisuallyAdjacentBorder(ABorderInfo, ABorderAtBottom, not AIsResultHorizontal) then
      AActualBorderAtBottom := nil;
    AWidth := 0;
    if (AActualBorderAtTop = nil) and (AActualBorderAtBottom = nil) then
    begin
      if ABorderAtTop <> nil then
        AWidth := Math.Max(AWidth, ATableBorderCalculator.GetActualWidth(ABorderAtTop));
      if ABorderAtBottom <> nil then
          AWidth := Math.Max(AWidth, ATableBorderCalculator.GetActualWidth(ABorderAtBottom));
      if AWidth = 0 then
      begin
        AWidth := ACellSpacing div 2;
        if ACellSpacing > 0 then
          if ABorderAtLeft = nil then
            ACornerType := TdxCornerViewInfoType.OuterHorizontalStart
          else
            ACornerType := TdxCornerViewInfoType.OuterHorizontalEnd;
      end;
    end
    else
    begin
      if AActualBorderAtTop <> nil then
        AWidth := Math.Max(AWidth, ATableBorderCalculator.GetActualWidth(AActualBorderAtTop));
      if AActualBorderAtBottom <> nil then
        AWidth := Math.Max(AWidth, ATableBorderCalculator.GetActualWidth(AActualBorderAtBottom));
    end;
    AHeight := 0;
    if (AActualBorderAtLeft = nil) and (AActualBorderAtRight = nil) then
    begin
      if ABorderAtLeft <> nil then
        AHeight := Math.Max(AHeight, ATableBorderCalculator.GetActualWidth(ABorderAtLeft));
      if ABorderAtRight <> nil then
        AHeight := Math.Max(AHeight, ATableBorderCalculator.GetActualWidth(ABorderAtRight));
      if AHeight = 0 then
      begin
        AHeight := ACellSpacing div 2;
        if ACellSpacing > 0 then
          if ABorderAtTop = nil then
            ACornerType := TdxCornerViewInfoType.OuterVerticalStart
          else
            ACornerType := TdxCornerViewInfoType.OuterVerticalEnd;
      end;
    end
    else
    begin
      if AActualBorderAtLeft <> nil then
        AHeight := Math.Max(AHeight, ATableBorderCalculator.GetActualWidth(AActualBorderAtLeft));
      if AActualBorderAtRight <> nil then
        AHeight := Math.Max(AHeight, ATableBorderCalculator.GetActualWidth(AActualBorderAtRight));
    end;
    ALayoutBorderWidth := AConverter.ToLayoutUnits(Int(AWidth));
    ALayoutBorderHeight := AConverter.ToLayoutUnits(Int(AHeight));
    ALineCount := ATableBorderCalculator.GetLineCount(ABorderInfo);
    case ALineCount of
      0: Result := TdxNoneLineCornerViewInfo.Create;
      1: Result := TdxSingleLineCornerViewInfo.Create(AActualBorderAtLeft, AActualBorderAtTop, AActualBorderAtRight, AActualBorderAtBottom, ALayoutBorderWidth, ALayoutBorderHeight, ACornerType);
      2: Result := TdxDoubleLineCornerViewInfo.Create(AActualBorderAtLeft, AActualBorderAtTop, AActualBorderAtRight, AActualBorderAtBottom, ALayoutBorderWidth, ALayoutBorderHeight, ACornerType);
      3: Result := TdxTripleLineCornerViewInfo.Create(AActualBorderAtLeft, AActualBorderAtTop, AActualBorderAtRight, AActualBorderAtBottom, ALayoutBorderWidth, ALayoutBorderHeight, ACornerType);
    else
      Result := nil;
      TdxRichEditExceptions.ThrowInternalException;
    end;
  finally
    ATableBorderCalculator.Free;
  end;
end;

function TdxCornerViewInfoBase.GetHeight: TdxLayoutUnit;
begin
  Result := Trunc(HeightF);
end;

function TdxCornerViewInfoBase.GetWidth: TdxLayoutUnit;
begin
  Result := Trunc(WidthF);
end;

{ TdxNoneLineCornerViewInfo }

constructor TdxNoneLineCornerViewInfo.Create;
begin
  inherited Create(TdxCornerViewInfoType.Normal);
end;

procedure TdxNoneLineCornerViewInfo.Export(const AExporter: IdxDocumentLayoutExporter; X, Y: Integer);
begin
end;

function TdxNoneLineCornerViewInfo.GetColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.Empty;
end;

{ TdxSingleLineCornerViewInfo }

constructor TdxSingleLineCornerViewInfo.Create(ALeftBorder, ARightBorder, ATopBorder, ABottomBorder: TdxBorderInfo;
  const AWidth, AHeight: Single; ACornerType: TdxCornerViewInfoType);
var
  ABorder: TdxBorderInfo;
begin
  inherited Create(ACornerType);
  if ALeftBorder <> nil then
    ABorder := ALeftBorder
  else
    if ARightBorder <> nil then
      ABorder := ARightBorder
    else
      if ATopBorder <> nil then
        ABorder := ATopBorder
      else
        if ABottomBorder <> nil then
          ABorder := ABottomBorder
        else
        begin
          TdxRichEditExceptions.ThrowInternalException;
          ABorder := ALeftBorder
        end;
  FColor := ABorder.Color;

  SetLength(Widths, 2);
  Widths[0] := 0;
  Widths[1] := 1;

  SetLength(Heights, 2);
  Heights[0] := 0;
  Heights[1] := 1;

  WidthF := AWidth;
  HeightF := AHeight;

  Pattern := FSingleLinePattern;
end;

destructor TdxSingleLineCornerViewInfo.Destroy;
begin
  SetLength(Widths, 0);
  SetLength(Heights, 0);
  inherited Destroy;
end;

class constructor TdxSingleLineCornerViewInfo.Initialize;
begin
  SetLength(FSingleLinePattern, 1);
  SetLength(FSingleLinePattern[0], 1);
  FSingleLinePattern[0][0] := True;
end;

procedure TdxSingleLineCornerViewInfo.Export(const AExporter: IdxDocumentLayoutExporter; X, Y: Integer);
begin
  AExporter.ExportTableBorderCorner(Self, X, Y);
end;

function TdxSingleLineCornerViewInfo.GetColor: TdxAlphaColor;
begin
  Result := FColor;
end;

{ TdxMultiLineCornerViewInfoBase }

constructor TdxMultiLineCornerViewInfoBase.Create(ABorderAtLeft, ABorderAtTop, ABorderAtRight,
  ABorderAtBottom: TdxBorderInfo; const AWidth, AHeight: Single; ACornerType: TdxCornerViewInfoType);
begin
  inherited Create(ACornerType);
  WidthF := AWidth;
  HeightF := AHeight;
  if ABorderAtLeft <> nil then
    FColor := ABorderAtLeft.Color
  else
    if ABorderAtTop <> nil then
      FColor := ABorderAtTop.Color
    else
      if ABorderAtRight <> nil then
        FColor := ABorderAtRight.Color
      else
        if ABorderAtBottom <> nil then
          FColor := ABorderAtBottom.Color
        else
          Assert(False);
  Pattern := CreatePattern(ABorderAtLeft, ABorderAtTop, ABorderAtRight, ABorderAtBottom);
end;

destructor TdxMultiLineCornerViewInfoBase.Destroy;
begin
  inherited Destroy;
end;

function TdxMultiLineCornerViewInfoBase.CreatePattern(ABorderAtLeft, ABorderAtTop, ABorderAtRight,
  ABorderAtBottom: TdxBorderInfo): TArray<TArray<Boolean>>;
var
  ATableBorderCalculator: TdxTableBorderCalculator;
  ATopOrBottomBorderInfo, ALeftOrRightBorderInfo: TdxBorderInfo;
begin
  ATopOrBottomBorderInfo := ABorderAtTop;
  if ATopOrBottomBorderInfo = nil then
    ATopOrBottomBorderInfo := ABorderAtBottom;
  ALeftOrRightBorderInfo := ABorderAtLeft;
  if ALeftOrRightBorderInfo = nil then
    ALeftOrRightBorderInfo := ABorderAtRight;

  ATableBorderCalculator := TdxTableBorderCalculator.Create;
  try
    if (ABorderAtLeft = nil) and (ABorderAtRight = nil) then
    begin
      Widths := ATableBorderCalculator.GetDrawingCompoundArray(ATopOrBottomBorderInfo);
      SetLength(Heights, 2);
      Heights[0] := 0;
      Heights[1] := 1;

      Exit(VerticalBordersMask);
    end;
    if (ABorderAtTop = nil) and (ABorderAtBottom = nil) then
    begin
      SetLength(Widths, 2);
      Widths[0] := 0;
      Widths[1] := 1;
      Heights := ATableBorderCalculator.GetDrawingCompoundArray(ALeftOrRightBorderInfo);
      Exit(HorizontalBordersMask);
    end;
    Result := CommonBorderMask;
    if ABorderAtLeft <> nil then
      Result := IntersectPattern(Result, LeftBorderMask);
    if ABorderAtRight <> nil then
      Result := IntersectPattern(Result, RightBorderMask);
    if ABorderAtTop <> nil then
      Result := IntersectPattern(Result, TopBorderMask);
    if ABorderAtBottom <> nil then
      Result := IntersectPattern(Result, BottomBorderMask);

    Widths := ATableBorderCalculator.GetDrawingCompoundArray(ATopOrBottomBorderInfo);
    Heights := ATableBorderCalculator.GetDrawingCompoundArray(ALeftOrRightBorderInfo);
  finally
    ATableBorderCalculator.Free;
  end;
end;

procedure TdxMultiLineCornerViewInfoBase.Export(const AExporter: IdxDocumentLayoutExporter; X, Y: Integer);
begin
  AExporter.ExportTableBorderCorner(Self, X, Y);
end;

function TdxMultiLineCornerViewInfoBase.GetColor: TdxAlphaColor;
begin
  Result := FColor;
end;

function TdxMultiLineCornerViewInfoBase.IntersectPattern(const APattern1,
  APattern2: TArray<TArray<Boolean>>): TArray<TArray<Boolean>>;
var
  I, J, ASize: Integer;
begin
  ASize := Length(APattern1);
  SetLength(Result, ASize);
  for I := 0 to ASize - 1 do
  begin
    SetLength(Result[I], ASize);
    for J := 0 to ASize - 1 do
      Result[I][J] := APattern1[I][J] and APattern2[I][J];
  end;
end;

{ TdxDoubleLineCornerViewInfo }

class constructor TdxDoubleLineCornerViewInfo.Initialize;
begin
  SetLength(FBottomBorderMask, 3);
  SetLength(FBottomBorderMask[0], 3);
  SetLength(FBottomBorderMask[1], 3);
  SetLength(FBottomBorderMask[2], 3);

  FBottomBorderMask[0][0] := True;
  FBottomBorderMask[0][1] := True;
  FBottomBorderMask[0][2] := True;

  FBottomBorderMask[1][0] := True;
  FBottomBorderMask[1][1] := True;
  FBottomBorderMask[1][2] := True;

  FBottomBorderMask[2][0] := True;
  FBottomBorderMask[2][1] := False;
  FBottomBorderMask[2][2] := True;

  SetLength(FCommonBorderMask, 3);
  SetLength(FCommonBorderMask[0], 3);
  SetLength(FCommonBorderMask[1], 3);
  SetLength(FCommonBorderMask[2], 3);

  FCommonBorderMask[0][0] := True;
  FCommonBorderMask[0][1] := True;
  FCommonBorderMask[0][2] := True;

  FCommonBorderMask[1][0] := True;
  FCommonBorderMask[1][1] := False;
  FCommonBorderMask[1][2] := True;

  FCommonBorderMask[2][0] := True;
  FCommonBorderMask[2][1] := True;
  FCommonBorderMask[2][2] := True;

  SetLength(FHorizontalBordersMask, 3);
  SetLength(FHorizontalBordersMask[0], 1);
  SetLength(FHorizontalBordersMask[1], 1);
  SetLength(FHorizontalBordersMask[2], 1);

  FHorizontalBordersMask[0][0] := True;
  FHorizontalBordersMask[1][0] := False;
  FHorizontalBordersMask[2][0] := True;

  SetLength(FLeftBorderMask, 3);
  SetLength(FLeftBorderMask[0], 3);
  SetLength(FLeftBorderMask[1], 3);
  SetLength(FLeftBorderMask[2], 3);

  FLeftBorderMask[0][0] := True;
  FLeftBorderMask[0][1] := True;
  FLeftBorderMask[0][2] := True;

  FLeftBorderMask[1][0] := False;
  FLeftBorderMask[1][1] := True;
  FLeftBorderMask[1][2] := True;

  FLeftBorderMask[2][0] := True;
  FLeftBorderMask[2][1] := True;
  FLeftBorderMask[2][2] := True;

  SetLength(FRightBorderMask, 3);
  SetLength(FRightBorderMask[0], 3);
  SetLength(FRightBorderMask[1], 3);
  SetLength(FRightBorderMask[2], 3);

  FRightBorderMask[0][0] := True;
  FRightBorderMask[0][1] := True;
  FRightBorderMask[0][2] := True;

  FRightBorderMask[1][0] := True;
  FRightBorderMask[1][1] := True;
  FRightBorderMask[1][2] := False;

  FRightBorderMask[2][0] := True;
  FRightBorderMask[2][1] := True;
  FRightBorderMask[2][2] := True;

  SetLength(FTopBorderMask, 3);
  SetLength(FTopBorderMask[0], 3);
  SetLength(FTopBorderMask[1], 3);
  SetLength(FTopBorderMask[2], 3);

  FTopBorderMask[0][0] := True;
  FTopBorderMask[0][1] := False;
  FTopBorderMask[0][2] := True;

  FTopBorderMask[1][0] := True;
  FTopBorderMask[1][1] := True;
  FTopBorderMask[1][2] := True;

  FTopBorderMask[2][0] := True;
  FTopBorderMask[2][1] := True;
  FTopBorderMask[2][2] := True;

  SetLength(FVerticalBordersMask, 1);
  SetLength(FVerticalBordersMask[0], 3);

  FVerticalBordersMask[0][0] := True;
  FVerticalBordersMask[0][1] := False;
  FVerticalBordersMask[0][2] := True;
end;

function TdxDoubleLineCornerViewInfo.GetBottomBorderMask: TArray<TArray<Boolean>>;
begin
  Result := FBottomBorderMask;
end;

function TdxDoubleLineCornerViewInfo.GetCommonBorderMask: TArray<TArray<Boolean>>;
begin
  Result := FCommonBorderMask;
end;

function TdxDoubleLineCornerViewInfo.GetHorizontalBordersMask: TArray<TArray<Boolean>>;
begin
  Result := FHorizontalBordersMask;
end;

function TdxDoubleLineCornerViewInfo.GetLeftBorderMask: TArray<TArray<Boolean>>;
begin
  Result := FLeftBorderMask;
end;

function TdxDoubleLineCornerViewInfo.GetRightBorderMask: TArray<TArray<Boolean>>;
begin
  Result := FRightBorderMask;
end;

function TdxDoubleLineCornerViewInfo.GetTopBorderMask: TArray<TArray<Boolean>>;
begin
  Result := FTopBorderMask;
end;

function TdxDoubleLineCornerViewInfo.GetVerticalBordersMask: TArray<TArray<Boolean>>;
begin
  Result := FVerticalBordersMask;
end;

{ TdxTripleLineCornerViewInfo }

class constructor TdxTripleLineCornerViewInfo.Initialize;
begin
  SetLength(FCommonBorderMask, 5);
  SetLength(FCommonBorderMask[0], 5);
  SetLength(FCommonBorderMask[1], 5);
  SetLength(FCommonBorderMask[2], 5);
  SetLength(FCommonBorderMask[3], 5);
  SetLength(FCommonBorderMask[4], 5);

  FCommonBorderMask[0][0] := True;
  FCommonBorderMask[0][1] := True;
  FCommonBorderMask[0][2] := True;
  FCommonBorderMask[0][3] := True;
  FCommonBorderMask[0][4] := True;

  FCommonBorderMask[1][0] := True;
  FCommonBorderMask[1][1] := False;
  FCommonBorderMask[1][2] := True;
  FCommonBorderMask[1][3] := False;
  FCommonBorderMask[1][4] := True;

  FCommonBorderMask[2][0] := True;
  FCommonBorderMask[2][1] := True;
  FCommonBorderMask[2][2] := True;
  FCommonBorderMask[2][3] := True;
  FCommonBorderMask[2][4] := True;

  FCommonBorderMask[3][0] := True;
  FCommonBorderMask[3][1] := False;
  FCommonBorderMask[3][2] := True;
  FCommonBorderMask[3][3] := False;
  FCommonBorderMask[3][4] := True;

  FCommonBorderMask[4][0] := True;
  FCommonBorderMask[4][1] := True;
  FCommonBorderMask[4][2] := True;
  FCommonBorderMask[4][3] := True;
  FCommonBorderMask[4][4] := True;

  SetLength(FLeftBorderMask, 5);
  SetLength(FLeftBorderMask[0], 5);
  SetLength(FLeftBorderMask[1], 5);
  SetLength(FLeftBorderMask[2], 5);
  SetLength(FLeftBorderMask[3], 5);
  SetLength(FLeftBorderMask[4], 5);

  FLeftBorderMask[0][0] := True;
  FLeftBorderMask[0][1] := True;
  FLeftBorderMask[0][2] := True;
  FLeftBorderMask[0][3] := True;
  FLeftBorderMask[0][4] := True;

  FLeftBorderMask[1][0] := False;
  FLeftBorderMask[1][1] := True;
  FLeftBorderMask[1][2] := True;
  FLeftBorderMask[1][3] := True;
  FLeftBorderMask[1][4] := True;

  FLeftBorderMask[2][0] := True;
  FLeftBorderMask[2][1] := True;
  FLeftBorderMask[2][2] := True;
  FLeftBorderMask[2][3] := True;
  FLeftBorderMask[2][4] := True;

  FLeftBorderMask[3][0] := False;
  FLeftBorderMask[3][1] := True;
  FLeftBorderMask[3][2] := True;
  FLeftBorderMask[3][3] := True;
  FLeftBorderMask[3][4] := True;

  FLeftBorderMask[4][0] := True;
  FLeftBorderMask[4][1] := True;
  FLeftBorderMask[4][2] := True;
  FLeftBorderMask[4][3] := True;
  FLeftBorderMask[4][4] := True;

  SetLength(FNoLeftBorderMask, 5);
  SetLength(FNoLeftBorderMask[0], 5);
  SetLength(FNoLeftBorderMask[1], 5);
  SetLength(FNoLeftBorderMask[2], 5);
  SetLength(FNoLeftBorderMask[3], 5);
  SetLength(FNoLeftBorderMask[4], 5);

  FNoLeftBorderMask[0][0] := True;
  FNoLeftBorderMask[0][1] := True;
  FNoLeftBorderMask[0][2] := True;
  FNoLeftBorderMask[0][3] := True;
  FNoLeftBorderMask[0][4] := True;

  FNoLeftBorderMask[1][0] := True;
  FNoLeftBorderMask[1][1] := True;
  FNoLeftBorderMask[1][2] := True;
  FNoLeftBorderMask[1][3] := True;
  FNoLeftBorderMask[1][4] := True;

  FNoLeftBorderMask[2][0] := True;
  FNoLeftBorderMask[2][1] := False;
  FNoLeftBorderMask[2][2] := True;
  FNoLeftBorderMask[2][3] := True;
  FNoLeftBorderMask[2][4] := True;

  FNoLeftBorderMask[3][0] := True;
  FNoLeftBorderMask[3][1] := True;
  FNoLeftBorderMask[3][2] := True;
  FNoLeftBorderMask[3][3] := True;
  FNoLeftBorderMask[3][4] := True;

  FNoLeftBorderMask[4][0] := True;
  FNoLeftBorderMask[4][1] := True;
  FNoLeftBorderMask[4][2] := True;
  FNoLeftBorderMask[4][3] := True;
  FNoLeftBorderMask[4][4] := True;

  SetLength(FRightBorderMask, 5);
  SetLength(FRightBorderMask[0], 5);
  SetLength(FRightBorderMask[1], 5);
  SetLength(FRightBorderMask[2], 5);
  SetLength(FRightBorderMask[3], 5);
  SetLength(FRightBorderMask[4], 5);

  FRightBorderMask[0][0] := True;
  FRightBorderMask[0][1] := True;
  FRightBorderMask[0][2] := True;
  FRightBorderMask[0][3] := True;
  FRightBorderMask[0][4] := True;

  FRightBorderMask[1][0] := True;
  FRightBorderMask[1][1] := True;
  FRightBorderMask[1][2] := True;
  FRightBorderMask[1][3] := True;
  FRightBorderMask[1][4] := False;

  FRightBorderMask[2][0] := True;
  FRightBorderMask[2][1] := True;
  FRightBorderMask[2][2] := True;
  FRightBorderMask[2][3] := True;
  FRightBorderMask[2][4] := True;

  FRightBorderMask[3][0] := True;
  FRightBorderMask[3][1] := True;
  FRightBorderMask[3][2] := True;
  FRightBorderMask[3][3] := True;
  FRightBorderMask[3][4] := False;

  FRightBorderMask[4][0] := True;
  FRightBorderMask[4][1] := True;
  FRightBorderMask[4][2] := True;
  FRightBorderMask[4][3] := True;
  FRightBorderMask[4][4] := True;

  SetLength(FNoRightBorderMask, 5);
  SetLength(FNoRightBorderMask[0], 5);
  SetLength(FNoRightBorderMask[1], 5);
  SetLength(FNoRightBorderMask[2], 5);
  SetLength(FNoRightBorderMask[3], 5);
  SetLength(FNoRightBorderMask[4], 5);

  FNoRightBorderMask[0][0] := True;
  FNoRightBorderMask[0][1] := True;
  FNoRightBorderMask[0][2] := True;
  FNoRightBorderMask[0][3] := True;
  FNoRightBorderMask[0][4] := True;

  FNoRightBorderMask[1][0] := True;
  FNoRightBorderMask[1][1] := True;
  FNoRightBorderMask[1][2] := True;
  FNoRightBorderMask[1][3] := True;
  FNoRightBorderMask[1][4] := True;

  FNoRightBorderMask[2][0] := True;
  FNoRightBorderMask[2][1] := True;
  FNoRightBorderMask[2][2] := True;
  FNoRightBorderMask[2][3] := False;
  FNoRightBorderMask[2][4] := True;

  FNoRightBorderMask[3][0] := True;
  FNoRightBorderMask[3][1] := True;
  FNoRightBorderMask[3][2] := True;
  FNoRightBorderMask[3][3] := True;
  FNoRightBorderMask[3][4] := True;

  FNoRightBorderMask[4][0] := True;
  FNoRightBorderMask[4][1] := True;
  FNoRightBorderMask[4][2] := True;
  FNoRightBorderMask[4][3] := True;
  FNoRightBorderMask[4][4] := True;

  SetLength(FTopBorderMask, 5);
  SetLength(FTopBorderMask[0], 5);
  SetLength(FTopBorderMask[1], 5);
  SetLength(FTopBorderMask[2], 5);
  SetLength(FTopBorderMask[3], 5);
  SetLength(FTopBorderMask[4], 5);

  FTopBorderMask[0][0] := True;
  FTopBorderMask[0][1] := False;
  FTopBorderMask[0][2] := True;
  FTopBorderMask[0][3] := False;
  FTopBorderMask[0][4] := True;

  FTopBorderMask[1][0] := True;
  FTopBorderMask[1][1] := True;
  FTopBorderMask[1][2] := True;
  FTopBorderMask[1][3] := True;
  FTopBorderMask[1][4] := True;

  FTopBorderMask[2][0] := True;
  FTopBorderMask[2][1] := True;
  FTopBorderMask[2][2] := True;
  FTopBorderMask[2][3] := True;
  FTopBorderMask[2][4] := True;

  FTopBorderMask[3][0] := True;
  FTopBorderMask[3][1] := True;
  FTopBorderMask[3][2] := True;
  FTopBorderMask[3][3] := True;
  FTopBorderMask[3][4] := True;

  FTopBorderMask[4][0] := True;
  FTopBorderMask[4][1] := True;
  FTopBorderMask[4][2] := True;
  FTopBorderMask[4][3] := True;
  FTopBorderMask[4][4] := True;

  SetLength(FNoTopBorderMask, 5);
  SetLength(FNoTopBorderMask[0], 5);
  SetLength(FNoTopBorderMask[1], 5);
  SetLength(FNoTopBorderMask[2], 5);
  SetLength(FNoTopBorderMask[3], 5);
  SetLength(FNoTopBorderMask[4], 5);

  FNoTopBorderMask[0][0] := True;
  FNoTopBorderMask[0][1] := True;
  FNoTopBorderMask[0][2] := True;
  FNoTopBorderMask[0][3] := True;
  FNoTopBorderMask[0][4] := True;

  FNoTopBorderMask[1][0] := True;
  FNoTopBorderMask[1][1] := True;
  FNoTopBorderMask[1][2] := False;
  FNoTopBorderMask[1][3] := True;
  FNoTopBorderMask[1][4] := True;

  FNoTopBorderMask[2][0] := True;
  FNoTopBorderMask[2][1] := True;
  FNoTopBorderMask[2][2] := True;
  FNoTopBorderMask[2][3] := True;
  FNoTopBorderMask[2][4] := True;

  FNoTopBorderMask[3][0] := True;
  FNoTopBorderMask[3][1] := True;
  FNoTopBorderMask[3][2] := True;
  FNoTopBorderMask[3][3] := True;
  FNoTopBorderMask[3][4] := True;

  FNoTopBorderMask[4][0] := True;
  FNoTopBorderMask[4][1] := True;
  FNoTopBorderMask[4][2] := True;
  FNoTopBorderMask[4][3] := True;
  FNoTopBorderMask[4][4] := True;

  SetLength(FBottomBorderMask, 5);
  SetLength(FBottomBorderMask[0], 5);
  SetLength(FBottomBorderMask[1], 5);
  SetLength(FBottomBorderMask[2], 5);
  SetLength(FBottomBorderMask[3], 5);
  SetLength(FBottomBorderMask[4], 5);

  FBottomBorderMask[0][0] := True;
  FBottomBorderMask[0][1] := True;
  FBottomBorderMask[0][2] := True;
  FBottomBorderMask[0][3] := True;
  FBottomBorderMask[0][4] := True;

  FBottomBorderMask[1][0] := True;
  FBottomBorderMask[1][1] := True;
  FBottomBorderMask[1][2] := True;
  FBottomBorderMask[1][3] := True;
  FBottomBorderMask[1][4] := True;

  FBottomBorderMask[2][0] := True;
  FBottomBorderMask[2][1] := True;
  FBottomBorderMask[2][2] := True;
  FBottomBorderMask[2][3] := True;
  FBottomBorderMask[2][4] := True;

  FBottomBorderMask[3][0] := True;
  FBottomBorderMask[3][1] := True;
  FBottomBorderMask[3][2] := True;
  FBottomBorderMask[3][3] := True;
  FBottomBorderMask[3][4] := True;

  FBottomBorderMask[4][0] := True;
  FBottomBorderMask[4][1] := False;
  FBottomBorderMask[4][2] := True;
  FBottomBorderMask[4][3] := False;
  FBottomBorderMask[4][4] := True;

  SetLength(FNoBottomBorderMask, 5);
  SetLength(FNoBottomBorderMask[0], 5);
  SetLength(FNoBottomBorderMask[1], 5);
  SetLength(FNoBottomBorderMask[2], 5);
  SetLength(FNoBottomBorderMask[3], 5);
  SetLength(FNoBottomBorderMask[4], 5);

  FNoBottomBorderMask[0][0] := True;
  FNoBottomBorderMask[0][1] := True;
  FNoBottomBorderMask[0][2] := True;
  FNoBottomBorderMask[0][3] := True;
  FNoBottomBorderMask[0][4] := True;

  FNoBottomBorderMask[1][0] := True;
  FNoBottomBorderMask[1][1] := True;
  FNoBottomBorderMask[1][2] := True;
  FNoBottomBorderMask[1][3] := True;
  FNoBottomBorderMask[1][4] := True;

  FNoBottomBorderMask[2][0] := True;
  FNoBottomBorderMask[2][1] := True;
  FNoBottomBorderMask[2][2] := True;
  FNoBottomBorderMask[2][3] := True;
  FNoBottomBorderMask[2][4] := True;

  FNoBottomBorderMask[3][0] := True;
  FNoBottomBorderMask[3][1] := True;
  FNoBottomBorderMask[3][2] := False;
  FNoBottomBorderMask[3][3] := True;
  FNoBottomBorderMask[3][4] := True;

  FNoBottomBorderMask[4][0] := True;
  FNoBottomBorderMask[4][1] := True;
  FNoBottomBorderMask[4][2] := True;
  FNoBottomBorderMask[4][3] := True;
  FNoBottomBorderMask[4][4] := True;

  SetLength(FVerticalBordersMask, 1);
  SetLength(FVerticalBordersMask[0], 5);

  FVerticalBordersMask[0][0] := True;
  FVerticalBordersMask[0][1] := False;
  FVerticalBordersMask[0][2] := True;
  FVerticalBordersMask[0][3] := False;
  FVerticalBordersMask[0][4] := True;

  SetLength(FHorizontalBordersMask, 5);
  SetLength(FHorizontalBordersMask[0], 1);
  SetLength(FHorizontalBordersMask[1], 1);
  SetLength(FHorizontalBordersMask[2], 1);
  SetLength(FHorizontalBordersMask[3], 1);
  SetLength(FHorizontalBordersMask[4], 1);

  FHorizontalBordersMask[0][0] := True;
  FHorizontalBordersMask[1][0] := False;
  FHorizontalBordersMask[2][0] := True;
  FHorizontalBordersMask[3][0] := False;
  FHorizontalBordersMask[4][0] := True;
end;

function TdxTripleLineCornerViewInfo.CreatePattern(ABorderAtLeft, ABorderAtTop, ABorderAtRight,
  ABorderAtBottom: TdxBorderInfo): TArray<TArray<Boolean>>;
begin
  Result := inherited CreatePattern(ABorderAtLeft, ABorderAtTop, ABorderAtRight, ABorderAtBottom);
  if (Result = HorizontalBordersMask) or (Result = VerticalBordersMask) then
    Exit;
  if ABorderAtLeft = nil then
    Result := IntersectPattern(Result, FNoLeftBorderMask);
  if ABorderAtRight = nil then
    Result := IntersectPattern(Result, FNoRightBorderMask);
  if ABorderAtTop = nil then
    Result := IntersectPattern(Result, FNoTopBorderMask);
  if ABorderAtBottom = nil then
    Result := IntersectPattern(Result, FNoBottomBorderMask);
end;

function TdxTripleLineCornerViewInfo.GetBottomBorderMask: TArray<TArray<Boolean>>;
begin
  Result := FBottomBorderMask;
end;

function TdxTripleLineCornerViewInfo.GetCommonBorderMask: TArray<TArray<Boolean>>;
begin
  Result := FCommonBorderMask;
end;

function TdxTripleLineCornerViewInfo.GetHorizontalBordersMask: TArray<TArray<Boolean>>;
begin
  Result := FHorizontalBordersMask;
end;

function TdxTripleLineCornerViewInfo.GetLeftBorderMask: TArray<TArray<Boolean>>;
begin
  Result := FLeftBorderMask;
end;

function TdxTripleLineCornerViewInfo.GetRightBorderMask: TArray<TArray<Boolean>>;
begin
  Result := FRightBorderMask;
end;

function TdxTripleLineCornerViewInfo.GetTopBorderMask: TArray<TArray<Boolean>>;
begin
  Result := FTopBorderMask;
end;

function TdxTripleLineCornerViewInfo.GetVerticalBordersMask: TArray<TArray<Boolean>>;
begin
  Result := FVerticalBordersMask;
end;

{ TdxTableRowViewInfoBase }

constructor TdxTableRowViewInfoBase.Create(ATableViewInfo: TdxTableViewInfo; ARowIndex: Integer);
begin
  inherited Create;
  FCells := TdxTableCellViewInfoCollection.Create;
  FTableViewInfo := ATableViewInfo;
  FRowIndex := ARowIndex;
  FVerticalBorders := CalculateVerticalBorders;
end;

destructor TdxTableRowViewInfoBase.Destroy;
begin
  FreeAndNil(FVerticalBorders);
  FreeAndNil(FCells);
  inherited Destroy;
end;

function TdxTableRowViewInfoBase.GetTopAnchorIndex: Integer;
begin
  Result := FRowIndex - TableViewInfo.TopRowIndex;
end;

function TdxTableRowViewInfoBase.GetBottomAnchorIndex: Integer;
begin
  Result := TopAnchorIndex + 1;
end;

function TdxTableRowViewInfoBase.GetTopAnchor: TdxTableCellVerticalAnchor;
begin
  Result := TableViewInfo.Anchors[TopAnchorIndex];
end;

function TdxTableRowViewInfoBase.GetBottomAnchor: TdxTableCellVerticalAnchor;
begin
  Result := TableViewInfo.Anchors[BottomAnchorIndex];
end;

function TdxTableRowViewInfoBase.GetRow: TdxTableRow;
begin
  Result := TableViewInfo.Table.Rows[FRowIndex];
end;

function TdxTableRowViewInfoBase.GetPrevious: TdxTableRowViewInfoBase;
var
  AViewInfoRowIndex: Integer;
begin
  AViewInfoRowIndex := FRowIndex - TableViewInfo.TopRowIndex;
  if AViewInfoRowIndex <= 0 then
    Result := nil
  else
    Result := TableViewInfo.Rows[AViewInfoRowIndex - 1];
end;

function TdxTableRowViewInfoBase.GetNext: TdxTableRowViewInfoBase;
var
  AViewInfoRowIndex: Integer;
begin
  AViewInfoRowIndex := FRowIndex - TableViewInfo.TopRowIndex;
  if AViewInfoRowIndex + 1 < TableViewInfo.Rows.Count then
    Result := TableViewInfo.Rows[AViewInfoRowIndex + 1]
  else
    Result := nil;
end;

function TdxTableRowViewInfoBase.IsLastRow: Boolean;
begin
  Result := Self = TableViewInfo.Rows.Last;
end;

function TdxTableRowViewInfoBase.IsFirstRow: Boolean;
begin
  Result := Self = TableViewInfo.Rows.First;
end;

function TdxTableRowViewInfoBase.ContainsEmptyCell: Boolean;
var
  I: Integer;
begin
  for I := 0 to FCells.Count - 1 do
    if FCells[I].EmptyCell then
      Exit(True);
  Result := False;
end;

{ TdxTableRowViewInfoNoCellSpacing }

function TdxTableRowViewInfoNoCellSpacing.CalculateVerticalBorders: TdxTableCellVerticalBorderViewInfoList;
var
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  I, ACount, ALayoutBorderIndex, AModelBorderIndex: Integer;
  ACalculator: TdxTableBorderCalculator;
  ATable: TdxTable;
  ALeftCellBorder, ARightCellBorder, AResultBorder: TdxBorderInfo;
begin
  ARow := Row;
  ACells := ARow.Cells;
  ACount := ACells.Count;
  Result := TdxTableCellVerticalBorderViewInfoList.Create;
  ACalculator := TdxTableBorderCalculator.Create;
  try
    ALayoutBorderIndex := ARow.LayoutProperties.GridBefore;
    AModelBorderIndex := ARow.GridBefore;
    ATable := ARow.Table;
    for I := 0 to ACount do
    begin
      if I > 0  then
        ALeftCellBorder := ACells[I - 1].GetActualRightCellBorder.Info
      else
        ALeftCellBorder := nil;

      if I < ACount then
        ARightCellBorder := ACells[I].GetActualLeftCellBorder.Info
      else
        ARightCellBorder := nil;

      AResultBorder := ACalculator.GetVerticalBorderSource(ATable, ALeftCellBorder, ARightCellBorder);

      Result.Add(TdxTableCellVerticalBorderViewInfo.Create(Self, AResultBorder, ALayoutBorderIndex, AModelBorderIndex, TableViewInfo.Table.DocumentModel.ToDocumentLayoutUnitConverter));
      if I < ACount then
      begin
        Inc(ALayoutBorderIndex, ACells[I].LayoutProperties.ColumnSpan);
        Inc(AModelBorderIndex, ACells[I].ColumnSpan);
      end;
    end;
  finally
    ACalculator.Free;
  end;
end;

function TdxTableRowViewInfoNoCellSpacing.GetBorderIndexByColumnIndex(ALayoutBorderIndex: Integer): Integer;
var
  AComparer: TdxVerticalBorderAndColumnIndexComparer;
begin
  AComparer := TdxVerticalBorderAndColumnIndexComparer.Create(ALayoutBorderIndex);
  try
    TdxAlgorithms1<TdxTableCellVerticalBorderViewInfo>.BinarySearch(VerticalBorders, AComparer, Result);
  finally
    AComparer.Free;
  end;
end;

procedure TdxTableRowViewInfoNoCellSpacing.ExportTo(const AExporter: IdxDocumentLayoutExporter);
var
  I, ACount: Integer;
begin
  if VerticalBorders = nil then
    exit;
  ACount := VerticalBorders.Count;
  for I := 0 to ACount - 1 do
    VerticalBorders[I].ExportTo(TableViewInfo, AExporter);
end;

function TdxTableRowViewInfoNoCellSpacing.GetBounds: TRect;
var
  ATop, ABottom, ALeft, ARight: Integer;
begin
  ATop := TopAnchor.VerticalPosition;
  ABottom := BottomAnchor.VerticalPosition;
  if IsLastRow then
    Inc(ABottom, BottomAnchor.BottomTextIndent);
  ALeft := TableViewInfo.GetVerticalBorderPosition(Row.LayoutProperties.GridBefore);
  ARight := TableViewInfo.GetVerticalBorderPosition(Row.Table.GetLastCellColumnIndexConsiderRowGrid(Row, True));
  Result.Init(ALeft, ATop, ARight, ABottom);
end;

function TdxTableRowViewInfoNoCellSpacing.GetVerticalBorderBounds(ALayoutBorderIndex: Integer): TRect;
var
  ABorderIndex: Integer;
  ABorder: TdxTableCellVerticalBorderViewInfo;
  ACalculator: TdxTableBorderCalculator;
  AActualWidth: TdxModelUnit;
  ALayoutActualWidth: TdxLayoutUnit;
begin
  ABorderIndex := GetBorderIndexByColumnIndex(ALayoutBorderIndex);
  ABorder := VerticalBorders[ABorderIndex];
  Result := ABorder.GetBounds(TableViewInfo);
  ACalculator := TdxTableBorderCalculator.Create;
  try
    AActualWidth := ACalculator.GetActualWidth(ABorder.Border);
  finally
    ACalculator.Free;
  end;
  ALayoutActualWidth := Row.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(AActualWidth);
  Result.InitSize(Result.Left - ALayoutActualWidth div 2, Result.Top, ALayoutActualWidth, Result.Height);
end;

{ TdxTableRowViewInfoWithCellSpacing }

constructor TdxTableRowViewInfoWithCellSpacing.Create(ATableViewInfo: TdxTableViewInfo; ARowIndex: Integer; ACellSpacing: TdxModelUnit);
begin
  inherited Create(ATableViewInfo, ARowIndex);
  FCellSpacing := ACellSpacing;
end;

function TdxTableRowViewInfoWithCellSpacing.GetVerticalBorderBounds(ALayoutBorderIndex: Integer): TRect;
var
  APosition, ATop, ABottom, AWidth: TdxLayoutUnit;
  ACellIndex: Integer;
  ACalculator: TdxTableBorderCalculator;
  ABorderAtLeftWidth, ABorderAtRightWidth: TdxModelUnit;
  ACells: TdxTableCellCollection;
begin
  APosition := TableViewInfo.VerticalBorderPositions.AlignedPosition[ALayoutBorderIndex] + TableViewInfo.Column.Bounds.Left;
  ATop := TopAnchor.VerticalPosition;
  ABottom := BottomAnchor.VerticalPosition;
  ACellIndex := Row.Table.GetAbsoluteCellIndexInRow(Row, ALayoutBorderIndex, true);
  ACalculator := TdxTableBorderCalculator.Create;
  try
    ACells := Row.Cells;
    if ACellIndex > 0 then
      ABorderAtLeftWidth := ACalculator.GetActualWidth(ACells[ACellIndex - 1].GetActualRightCellBorder)
    else
      ABorderAtLeftWidth := 0;
    if ACellIndex < ACells.Count then
      ABorderAtRightWidth := ACalculator.GetActualWidth(ACells[ACellIndex].GetActualLeftCellBorder)
    else
      ABorderAtRightWidth := 0;
  finally
    ACalculator.Free;
  end;
  AWidth := Converter.ToLayoutUnits(ABorderAtLeftWidth + ABorderAtRightWidth + FCellSpacing);
  Result.InitSize(APosition - AWidth div 2, ATop, AWidth, ABottom - ATop);
end;

procedure TdxTableRowViewInfoWithCellSpacing.ExportTo(const AExporter: IdxDocumentLayoutExporter);
var
  I, ACellCount: Integer;
  ACellViewInfo: TdxTableCellViewInfo;
  ALeftBorderViewInfo: TdxLeftTableCellBorderViewInfo;
  ARightBorderViewInfo: TdxRightTableCellBorderViewInfo;
  ATopBorderViewInfo: TdxTopTableCellBorderViewInfo;
  ABottomBorderViewInfo: TdxBottomTableCellBorderViewInfo;
  ATopBounds, ABottomBounds: TRect;
  ATopLeftCorner, ATopRightCorner, ABottomLeftCorner, ABottomRightCorner: TdxCornerViewInfoBase;
begin
  if VerticalBorders = nil then
    Exit;
  ACellCount := Cells.Count;

  ExportTableBorders(AExporter);

  for I := 0 to ACellCount - 1 do
  begin
    ACellViewInfo := Cells[I];
    ALeftBorderViewInfo := TdxLeftTableCellBorderViewInfo.Create(ACellViewInfo);
    ARightBorderViewInfo := TdxRightTableCellBorderViewInfo.Create(ACellViewInfo);
    ATopBorderViewInfo := TdxTopTableCellBorderViewInfo.Create(ACellViewInfo);
    ABottomBorderViewInfo := TdxBottomTableCellBorderViewInfo.Create(ACellViewInfo);
    try
      ATopBounds := ATopBorderViewInfo.GetBoundsCore(TableViewInfo);
      ATopLeftCorner := TdxCornerViewInfoBase.CreateCorner(Converter, nil, nil, ATopBorderViewInfo.Border, ALeftBorderViewInfo.Border, cellSpacing);
      AExporter.ExportTableBorderCorner(ATopLeftCorner, ATopBounds.Left, ATopBounds.Top);
      ATopRightCorner := TdxCornerViewInfoBase.CreateCorner(Converter, ATopBorderViewInfo.Border, nil, nil, ARightBorderViewInfo.Border, cellSpacing);
      AExporter.ExportTableBorderCorner(ATopRightCorner, ATopBounds.Right, ATopBounds.Top);

      ABottomBounds := ABottomBorderViewInfo.GetBoundsCore(TableViewInfo);
      ABottomLeftCorner := TdxCornerViewInfoBase.CreateCorner(Converter, nil, ALeftBorderViewInfo.Border, ABottomBorderViewInfo.Border, nil, cellSpacing);
      AExporter.ExportTableBorderCorner(ABottomLeftCorner, ABottomBounds.Left, ABottomBounds.Bottom);
      ABottomRightCorner := TdxCornerViewInfoBase.CreateCorner(Converter, ABottomBorderViewInfo.Border, ARightBorderViewInfo.Border, nil, nil, cellSpacing);
      AExporter.ExportTableBorderCorner(ABottomRightCorner, ABottomBounds.Right, ABottomBounds.Bottom);

      ALeftBorderViewInfo.TopCorner := ATopLeftCorner;
      ALeftBorderViewInfo.BottomCorner := ABottomLeftCorner;
      ARightBorderViewInfo.TopCorner := ATopRightCorner;
      ARightBorderViewInfo.BottomCorner := ABottomRightCorner;
      ATopBorderViewInfo.LeftCorner := ATopLeftCorner;
      ATopBorderViewInfo.RightCorner := ATopRightCorner;
      ABottomBorderViewInfo.LeftCorner := ABottomLeftCorner;
      ABottomBorderViewInfo.RightCorner := ABottomRightCorner;


      ALeftBorderViewInfo.ExportTo(TableViewInfo, AExporter);
      ARightBorderViewInfo.ExportTo(TableViewInfo, AExporter);
      ATopBorderViewInfo.ExportTo(TableViewInfo, AExporter);
      ABottomBorderViewInfo.ExportTo(TableViewInfo, AExporter);
    finally
      ALeftBorderViewInfo.Free;
      ARightBorderViewInfo.Free;
      ATopBorderViewInfo.Free;
      ABottomBorderViewInfo.Free;
    end;
  end;
end;

function TdxTableRowViewInfoWithCellSpacing.GetBounds: TRect;
var
  ATop, ABottom, ACellSpacing, ALeft, ARight: Integer;
  AFirstCellViewInfo, ALastCellViewInfo: TdxTableCellViewInfo;
begin
  ATop := TopAnchor.VerticalPosition;
  if not IsFirstRow and (Previous is TdxTableRowViewInfoWithCellSpacing) then
  begin
    ACellSpacing := Converter.ToLayoutUnits(TdxTableRowViewInfoWithCellSpacing(Previous).CellSpacing);
    Inc(ATop, ACellSpacing div 2);
  end;
  ABottom := BottomAnchor.VerticalPosition;
  ACellSpacing := Converter.ToLayoutUnits(CellSpacing);
  if IsLastRow then
    Inc(ABottom, ACellSpacing)
  else
    Inc(ABottom, ACellSpacing - ACellSpacing div 2);

  AFirstCellViewInfo := Cells[0];
  ALeft := AFirstCellViewInfo.Left - ACellSpacing;
  ALastCellViewInfo := Cells.Last;
  ARight := ALastCellViewInfo.Left + ALastCellViewInfo.Width + ACellSpacing;

  Result.Init(ALeft, ATop, ARight, ABottom);
end;

procedure TdxTableRowViewInfoWithCellSpacing.ExportTableBorders(const AExporter: IdxDocumentLayoutExporter);
var
  ALeftTableBorderViewInfo: TdxLeftTableBorderWithSpacingViewInfo;
  ARightTableBorderViewInfo: TdxRightTableBorderWithSpacingViewInfo;
  ATopTableBorderViewInfo: TdxTopTableBorderWithSpacingViewInfo;
  ABottomTableBorderViewInfo: TdxBottomTableBorderWithSpacingViewInfo;
  ALeftBounds, ARightBounds: TRect;
  ATopBorder, ABottomBorder: TdxBorderInfo;
  ATopLeftCorner, ATopRightCorner, ABottomLeftCorner, ABottomRightCorner: TdxCornerViewInfoBase;
begin
  ALeftTableBorderViewInfo := TdxLeftTableBorderWithSpacingViewInfo.Create(Self);
  ARightTableBorderViewInfo := TdxRightTableBorderWithSpacingViewInfo.Create(Self);
  if IsFirstRow then
    ATopTableBorderViewInfo := TdxTopTableBorderWithSpacingViewInfo.Create(Self)
  else
    ATopTableBorderViewInfo := nil;

  if IsLastRow then
    ABottomTableBorderViewInfo := TdxBottomTableBorderWithSpacingViewInfo.Create(Self)
  else
    ABottomTableBorderViewInfo := nil;

  ALeftBounds := ALeftTableBorderViewInfo.GetBounds(TableViewInfo);
  if ATopTableBorderViewInfo <> nil then
    ATopBorder := ATopTableBorderViewInfo.Border
  else
    ATopBorder := nil;

  ARightBounds := ARightTableBorderViewInfo.GetBounds(TableViewInfo);
  if ABottomTableBorderViewInfo <> nil then
    ABottomBorder := ABottomTableBorderViewInfo.Border
  else
    ABottomBorder := nil;

  ATopLeftCorner := TdxCornerViewInfoBase.CreateCorner(Converter, nil, nil, ATopBorder, ALeftTableBorderViewInfo.Border, 0);
  ATopRightCorner := TdxCornerViewInfoBase.CreateCorner(Converter, ATopBorder, nil, nil, ARightTableBorderViewInfo.Border, 0);
  ABottomLeftCorner := TdxCornerViewInfoBase.CreateCorner(Converter, nil, ALeftTableBorderViewInfo.Border, ABottomBorder, nil, 0);
  ABottomRightCorner := TdxCornerViewInfoBase.CreateCorner(Converter, ABottomBorder, ARightTableBorderViewInfo.Border, nil, nil, 0);
  try
    AExporter.ExportTableBorderCorner(ATopLeftCorner, ALeftBounds.Left, ALeftBounds.Top);
    AExporter.ExportTableBorderCorner(ATopRightCorner, ALeftBounds.Right, ALeftBounds.Top);
    AExporter.ExportTableBorderCorner(ABottomLeftCorner, ARightBounds.Left, ARightBounds.Bottom);
    AExporter.ExportTableBorderCorner(ABottomRightCorner, ARightBounds.Right, ARightBounds.Bottom);

    ALeftTableBorderViewInfo.LeftCorner := ATopLeftCorner;
    ALeftTableBorderViewInfo.RightCorner := ABottomLeftCorner;
    ARightTableBorderViewInfo.LeftCorner := ATopRightCorner;
    ARightTableBorderViewInfo.RightCorner := ABottomRightCorner;

    if ATopTableBorderViewInfo <> nil then
    begin
      ATopTableBorderViewInfo.LeftCorner := ATopLeftCorner;
      ATopTableBorderViewInfo.RightCorner := ATopRightCorner;
    end;

    if ABottomTableBorderViewInfo <> nil then
    begin
      ABottomTableBorderViewInfo.LeftCorner := ABottomLeftCorner;
      ABottomTableBorderViewInfo.RightCorner := ABottomRightCorner;
    end;

    ALeftTableBorderViewInfo.ExportTo(TableViewInfo, AExporter);
    ARightTableBorderViewInfo.ExportTo(TableViewInfo, AExporter);

    if ATopTableBorderViewInfo <> nil then
      ATopTableBorderViewInfo.ExportTo(TableViewInfo, AExporter);
    if ABottomTableBorderViewInfo <> nil then
      ABottomTableBorderViewInfo.ExportTo(TableViewInfo, AExporter);

  finally
    ATopLeftCorner.Free;
    ATopRightCorner.Free;
    ABottomLeftCorner.Free;
    ABottomRightCorner.Free;

    ALeftTableBorderViewInfo.Free;
    ARightTableBorderViewInfo.Free;
    ATopTableBorderViewInfo.Free;
    ABottomTableBorderViewInfo.Free;
  end;
end;

function TdxTableRowViewInfoWithCellSpacing.GetConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := TableViewInfo.Table.DocumentModel.ToDocumentLayoutUnitConverter;
end;

function TdxTableRowViewInfoWithCellSpacing.CalculateVerticalBorders: TdxTableCellVerticalBorderViewInfoList;
begin
  Result := TdxTableCellVerticalBorderViewInfoList.Create(False);
end;

{ TdxParagraphIndexComparer }

function TdxParagraphIndexComparer.Compare(const X, Y: TdxTableCellViewInfo): Integer;
begin
  Result := CompareInteger(X.Cell.StartParagraphIndex, Y.Cell.StartParagraphIndex);
end;

{ TdxTableViewInfo }

constructor TdxTableViewInfo.Create(ATable: TdxTable; ATopLevelColumn: TdxColumn;
  AColumn: TdxColumn; AVerticalBorderPositions: TdxVerticalBorderPositions; ATopRowIndex: Integer;
  AParentTableCellViewInfo: TdxTableCellViewInfo; AFirstContentInParentCell: Boolean);
begin
  inherited Create;
  FCells := TdxTableCellViewInfoCollection.Create;
  FRows := TdxTableRowViewInfoCollection.Create(Self);
  FAnchors := TdxTableCellVerticalAnchorCollection.Create;
  FTable := ATable;
  FTopLevelColumn := ATopLevelColumn;
  FColumn := AColumn;
  FVerticalBorderPositions := AVerticalBorderPositions;
  FTopRowIndex := ATopRowIndex;
  FParentTableCellViewInfo := AParentTableCellViewInfo;
  FFirstContentInParentCell := AFirstContentInParentCell;
end;

destructor TdxTableViewInfo.Destroy;
var
  I: Integer;
begin
  FVerticalBorderPositions.Free;
  for I := 0 to FAnchors.Count - 1 do
    FAnchors[I].Free;
  FAnchors.Free;
  FRows.Free;
  FCells.Free;
  inherited Destroy;
end;

function TdxTableViewInfo.GetActualLeftBorder: TdxBorderBase;
begin
  Result := Table.GetActualLeftBorder;
end;

function TdxTableViewInfo.GetActualRightBorder: TdxBorderBase;
begin
  Result := Table.GetActualRightBorder;
end;

function TdxTableViewInfo.GetActualTopBorder: TdxBorderBase;
begin
  Result := Table.GetActualTopBorder;
end;

function TdxTableViewInfo.GetActualBottomBorder: TdxBorderBase;
begin
  Result := Table.GetActualBottomBorder;
end;

procedure TdxTableViewInfo.SetTopRowIndex(ATopRowIndex: Integer);
begin
  FTopRowIndex := ATopRowIndex
end;

function TdxTableViewInfo.GetColumnLeft(AColumnIndex: Integer): TdxLayoutUnit;
begin
  Result := VerticalBorderPositions.AlignedPosition[AColumnIndex] + Column.Bounds.Left
end;

function TdxTableViewInfo.GetColumnRight(AColumnIndex: Integer): TdxLayoutUnit;
begin
  Result := VerticalBorderPositions.AlignedPosition[AColumnIndex + 1] + Column.Bounds.Left;
end;

function TdxTableViewInfo.GetVerticalBorderPosition(ABorderIndex: Integer): Integer;
begin
  Result := VerticalBorderPositions.AlignedPosition[ABorderIndex] + Column.Bounds.Left;
end;

procedure TdxTableViewInfo.ExportTo(const AExporter: IdxDocumentLayoutExporter);
var
  ARow: TdxTableRowViewInfoBase;
  I, ARowCount, ACount: Integer;
  AAnchor: TdxTableCellVerticalAnchor;
begin
  ARowCount := Rows.Count;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := Rows[I];
    if AExporter.IsTableRowVisible(ARow) then
      ExportCellsVerticalBorders(ARow, AExporter);
  end;

  ACount := Anchors.Count;
  for I := 0 to ACount - 1 do
  begin
    AAnchor := Anchors[I];
    if AExporter.IsAnchorVisible(AAnchor) then
      ExportAnchorBorders(AAnchor, AExporter);
  end;

  for I := 0 to ACount - 1 do
    if AExporter.IsAnchorVisible(Anchors[I]) then
      ExportAnchorCorner(AExporter, Anchors[I]);
end;

procedure TdxTableViewInfo.ExportAnchorCorner(const AExporter: IdxDocumentLayoutExporter; AAnchor: TdxTableCellVerticalAnchor);
var
  ABorders: TdxHorizontalCellBordersInfoList;
  ACornerViewInfo: TdxCornerViewInfoBase;
  I, ACornerCount, ALeft: Integer;
begin
  ABorders := AAnchor.CellBorders;
  if ABorders = nil then
    Exit;
  ACornerCount := AAnchor.Corners.Count;
  Assert(ACornerCount = AAnchor.CellBorders.Count + 1);
  for I := 0 to ACornerCount - 1 do
  begin
    ACornerViewInfo := AAnchor.Corners[I];
    if I < AAnchor.CellBorders.Count then
      ALeft := GetColumnLeft(AAnchor.CellBorders[I].StartColumnIndex)
    else
      ALeft := GetColumnRight(AAnchor.CellBorders[AAnchor.CellBorders.Count - 1].EndColumnIndex);
    ACornerViewInfo.Export(AExporter, ALeft, AAnchor.VerticalPosition);
  end;
end;

function TdxTableViewInfo.GetVerticalBorderByColumnIndex(ARowViewInfo: TdxTableRowViewInfoNoCellSpacing; ACornerColumnIndex: Integer): TdxTableCellVerticalBorderViewInfo;
var
  ARow: TdxTableRow;
  AColumnIndex, ACellIndex: Integer;
begin
  if ARowViewInfo = nil then
     Exit(nil);
  ARow := ARowViewInfo.Row;
  AColumnIndex := ARow.LayoutProperties.GridBefore;
  ACellIndex := 0;
  while (ACornerColumnIndex > AColumnIndex) and (ACellIndex < ARow.Cells.Count) do
  begin
    Inc(AColumnIndex, ARow.Cells[ACellIndex].LayoutProperties.ColumnSpan);
    Inc(ACellIndex);
  end;
  if ACornerColumnIndex = AColumnIndex then
    Result := ARowViewInfo.VerticalBorders[ACellIndex]
  else
    Result := nil;
end;

procedure TdxTableViewInfo.ExportCellsVerticalBorders(ATableRowViewInfoBase: TdxTableRowViewInfoBase; const AExporter: IdxDocumentLayoutExporter);
begin
  ATableRowViewInfoBase.ExportTo(AExporter);
end;

procedure TdxTableViewInfo.ExportBackground(const AExporter: IdxDocumentLayoutExporter);
begin
  ExportBackgroundCore(AExporter, procedure(const ARow: TdxTableRowViewInfoBase)
    begin
      AExporter.ExportTableRow(ARow);
    end);
  ExportBackgroundCore(AExporter, procedure(const ARow: TdxTableRowViewInfoBase)
    begin
      ExportRowCells(ARow, AExporter);
    end);
end;

procedure TdxTableViewInfo.ExportBackgroundCore(const AExporter: IdxDocumentLayoutExporter;
  const ARowAction: TdxAction<TdxTableRowViewInfoBase>);
var
  ARow: TdxTableRowViewInfoBase;
  I, ARowCount: Integer;
begin
  ARowCount := RowCount;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := Rows[I];
    if AExporter.IsTableRowVisible(ARow) then
      ARowAction(ARow);
  end;
end;

procedure TdxTableViewInfo.ExportRowCells(ARow: TdxTableRowViewInfoBase; const AExporter: IdxDocumentLayoutExporter);
var
  I, ACount: Integer;
  ACells: TdxTableCellViewInfoCollection;
begin
  ACells := ARow.Cells;
  ACount := ACells.Count;
  for I := 0 to ACount - 1 do
    if Anchors[ACells[I].TopAnchorIndex] = TdxTableCellVerticalAnchor(ARow.TopAnchor) then
      ACells[I].ExportTo(AExporter);
end;

function TdxTableViewInfo.GetTopAnchor: TdxTableCellVerticalAnchor;
begin
  Result := Rows.First.TopAnchor;
end;

function TdxTableViewInfo.GetBottomAnchor: TdxTableCellVerticalAnchor;
begin
  Result := Rows.Last.BottomAnchor;
end;

function TdxTableViewInfo.GetRowCount: Integer;
begin
  Result := Anchors.Count - 1;
end;

function TdxTableViewInfo.GetBottomRowIndex: Integer;
begin
  Result := TopRowIndex + RowCount - 1;
end;

function TdxTableViewInfo.GetSingleColumn: Boolean;
begin
  Result := (FPrevTableViewInfo = nil) and (FNextTableViewInfo = nil);
end;

procedure TdxTableViewInfo.ExportAnchorBorders(AAnchor: TdxTableCellVerticalAnchor; const AExporter: IdxDocumentLayoutExporter);
var
  ACellBorders: TdxHorizontalCellBordersInfoList;
  I, ACount: Integer;
  ABorderInfo: TdxHorizontalCellBordersInfo;
  AViewInfo: TdxTableBorderViewInfoBase;
begin
  ACellBorders := AAnchor.CellBorders;
  if ACellBorders = nil then
    Exit;
  ACount := ACellBorders.Count;
  Assert(ACount + 1 = AAnchor.Corners.Count);
  for I := 0 to ACount - 1 do
  begin
    ABorderInfo := ACellBorders[I];
    if ABorderInfo.Border = nil then
      Continue;
    AViewInfo := TdxTableCellHorizontalBorderViewInfo.Create(AAnchor, ABorderInfo, Table.DocumentModel.ToDocumentLayoutUnitConverter, AAnchor.Corners[I], AAnchor.Corners[I + 1]);
    try
      AViewInfo.ExportTo(Self, AExporter);
    finally
      AViewInfo.Free;
    end;
  end;
end;

procedure TdxTableViewInfo.NestedChanged;
begin
  if IsNested then
    TdxColumn.AddReference(FColumn)
  else
    TdxColumn.Release(FColumn);
end;

function TdxTableViewInfo.GetRowBounds(ARow: TdxTableRowViewInfoBase): TRect;
begin
  Result := ARow.GetBounds;
end;

function TdxTableViewInfo.GetCellBounds(ACellViewInfo: TdxTableCellViewInfo): TRect;
var
  ATopAnchorIndex, ABottomAnchorIndex, ATop, ATopBorder, ATopMarginValue: Integer;
  ATopAnchor, ABottomAnchor: TdxTableCellVerticalAnchor;
  ABorderCalculator: TdxTableBorderCalculator;
  ATopMargin: TdxMarginUnitBase;
begin
  ATopAnchorIndex := ACellViewInfo.TopAnchorIndex;
  ABottomAnchorIndex := ACellViewInfo.BottomAnchorIndex;
  ATopAnchor := Anchors[ATopAnchorIndex];
  ABottomAnchor := Anchors[ABottomAnchorIndex];
  if (ATopAnchor = nil) or (ABottomAnchor = nil) then
    Exit(cxNullRect);

  ATop := ATopAnchor.VerticalPosition + ATopAnchor.BottomTextIndent;
  ABorderCalculator := TdxTableBorderCalculator.Create;
  try
    ATopBorder := ABorderCalculator.GetActualWidth(ACellViewInfo.Cell.GetActualTopCellBorder);
  finally
    ABorderCalculator.Free;
  end;
  ATopMargin := ACellViewInfo.Cell.GetActualTopMargin;
  if ATopMargin.&Type = TdxWidthUnitType.ModelUnits then
    ATopMarginValue := ATopMargin.Value
  else
    ATopMarginValue := 0;

  Dec(ATop, ACellViewInfo.Cell.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ATopBorder + ATopMarginValue));
  Result.InitSize(ACellViewInfo.Left, ATop, ACellViewInfo.Width, ABottomAnchor.VerticalPosition - ATop);
end;

procedure TdxTableViewInfo.Complete(ATopBorderHeight: TdxLayoutUnit; ABottomBorderHeight: TdxLayoutUnit);
var
  I, ACount, ARowCount: Integer;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ATopRow, ABottomRow: TdxTableRowViewInfoNoCellSpacing;
  AComparer: TdxParagraphIndexComparer;
begin
  RemoveEmptyCells;
  AComparer := TdxParagraphIndexComparer.Create;
  try
    FCells.Sort(AComparer);
  finally
    AComparer.Free;
  end;
  ACount := Anchors.Count;
  ARowCount := Rows.Count;

  AConverter := FTable.DocumentModel.ToDocumentLayoutUnitConverter;
  for I := 0 to ACount - 1 do
  begin
    if (I > 0) and (Rows[I - 1] is TdxTableRowViewInfoNoCellSpacing) then
      ATopRow := TdxTableRowViewInfoNoCellSpacing(Rows[I - 1])
    else
      ATopRow := nil;

    if (I < ARowCount) and (Rows[I] is TdxTableRowViewInfoNoCellSpacing) then
      ABottomRow := TdxTableRowViewInfoNoCellSpacing(Rows[I])
    else
      ABottomRow := nil;

    CreateAnchorCorner(AConverter, Anchors[I], ATopRow, ABottomRow);
  end;
end;

procedure TdxTableViewInfo.RemoveEmptyCells;
var
  I, ARowCount: Integer;
  ARow: TdxTableRowViewInfoBase;
begin
  ARowCount := Rows.Count;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := Rows[I];
    if ARow.ContainsEmptyCell then
    begin
      RemoveRowsFromIndex(I);
      Break;
    end;
  end;
  if Rows.Count = 0 then
  begin
    if ParentTableCellViewInfo <> nil then
     begin
       if FirstContentInParentCell then
         ParentTableCellViewInfo.EmptyCell := True;
       ParentTableCellViewInfo.InnerTables.Remove(Self);
     end;
  end;
end;

procedure TdxTableViewInfo.RemoveRowsFromIndex(AFirstRowViewInfoIndex: Integer);
var
  I, ARowCount, ALastTableAnchorIndex, ATotalCellCount, ACellCount: Integer;
  ALastRow: TdxTableRowViewInfoBase;
  ACell: TdxTableCellViewInfo;
  ALastRowCells: TdxTableCellViewInfoCollection;
begin
  ARowCount := Rows.Count;
  for I := ARowCount - 1 downto AFirstRowViewInfoIndex do
    Rows.Delete(I);

  ALastTableAnchorIndex := AFirstRowViewInfoIndex;
  ATotalCellCount := FCells.Count;
  for I := ATotalCellCount - 1 downto 0 do
    if FCells[I].TopAnchorIndex >= ALastTableAnchorIndex then
    begin
      FColumn.TopLevelColumn.RemoveTableCellViewInfoContent(FCells[I]);
      RemoveInnerTablesFromTopLevelColumn(FCells[I]);
      FCells.Delete(I);
    end;

  if AFirstRowViewInfoIndex > 0 then
  begin
    Anchors.RemoveAnchors(ALastTableAnchorIndex + 1, Anchors.Count - ALastTableAnchorIndex - 1);

    ALastRow := Rows[AFirstRowViewInfoIndex - 1];
    ALastRowCells := ALastRow.Cells;
    ACellCount := ALastRowCells.Count;

    for I := 0 to ACellCount - 1 do
    begin
      ACell := ALastRowCells[I];
      if ACell.BottomAnchorIndex > ALastTableAnchorIndex then
        ACell.SetBottomAnchorIndexToLastAnchor;
    end;
  end
  else
    Anchors.RemoveAnchors(0, Anchors.Count);
end;

procedure TdxTableViewInfo.RemoveInnerTablesFromTopLevelColumn(ATableCellViewInfo: TdxTableCellViewInfo);
var
  ATableViewInfo: TdxTableViewInfo;
  I: Integer;
begin
  for I := 0 to ATableCellViewInfo.InnerTables.Count - 1 do
  begin
    ATableViewInfo := ATableCellViewInfo.InnerTables[I];
    RemoveTableViewInfoFromTopLevelColumn(ATableViewInfo);
  end;
end;

procedure TdxTableViewInfo.RemoveTableViewInfoFromTopLevelColumn(ATableViewInfo: TdxTableViewInfo);
var
  ATables: TdxTableViewInfoCollection;
  I, ACellCount: Integer;
begin
  ATables := FColumn.TopLevelColumn.InnerTables;
  if ATables <> nil then
    ATables.Remove(ATableViewInfo);

  ACellCount := ATableViewInfo.Cells.Count;
  for I := 0 to ACellCount - 1 do
  begin
    FColumn.TopLevelColumn.RemoveTableCellViewInfoContent(ATableViewInfo.Cells[I]);
    RemoveInnerTablesFromTopLevelColumn(ATableViewInfo.Cells[I])
  end;
end;

procedure TdxTableViewInfo.SetIsNested(const Value: Boolean);
begin
  if IsNested <> Value then
  begin
    FIsNested := Value;
    NestedChanged;
  end;
end;

procedure TdxTableViewInfo.CreateAnchorCorner(AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AAnchor: TdxTableCellVerticalAnchor; ATopRow: TdxTableRowViewInfoNoCellSpacing; ABottomRow: TdxTableRowViewInfoNoCellSpacing);
var
  ABorders: TdxHorizontalCellBordersInfoList;
  I, ABorderCount, ACornerColumnIndex: Integer;
  ALeftBorderInfo, ARightBorderInfo: TdxNullableHorizontalCellBordersInfo;
  ABorderViewInfoAtTop, ABorderViewInfoAtBottom: TdxTableCellVerticalBorderViewInfo;
  ABorderAtLeft, ABorderAtRight, ABorderAtTop, ABorderAtBottom: TdxBorderInfo;
  ACornerViewInfo: TdxCornerViewInfoBase;
begin
  ABorders := AAnchor.CellBorders;
  if ABorders = nil then
    Exit;
  ABorderCount := ABorders.Count;
  for I := -1 to ABorderCount - 1 do
  begin
    ALeftBorderInfo.Reset;
    if I >= 0 then
      ALeftBorderInfo := ABorders[I];

    ARightBorderInfo.Reset;
    if I + 1 < ABorderCount then
      ARightBorderInfo := ABorders[I + 1];

    if (not ALeftBorderInfo.IsNull) and (ALeftBorderInfo.Value.Border <> nil) then
      ABorderAtLeft := ALeftBorderInfo.Value.Border.Info
    else
      ABorderAtLeft := nil;

    if (not ARightBorderInfo.IsNull) and (ARightBorderInfo.Value.Border <> nil) then
      ABorderAtRight := ARightBorderInfo.Value.Border.Info
    else
      ABorderAtRight := nil;

    if not ALeftBorderInfo.IsNull then
      ACornerColumnIndex := ALeftBorderInfo.Value.EndColumnIndex + 1
    else
      ACornerColumnIndex := ARightBorderInfo.Value.StartColumnIndex;

    ABorderViewInfoAtTop := GetVerticalBorderByColumnIndex(ATopRow, ACornerColumnIndex);
    ABorderViewInfoAtBottom := GetVerticalBorderByColumnIndex(ABottomRow, ACornerColumnIndex);

    if ABorderViewInfoAtTop <> nil then
      ABorderAtTop := ABorderViewInfoAtTop.Border
    else
      ABorderAtTop := nil;

    if ABorderViewInfoAtBottom <> nil then
      ABorderAtBottom := ABorderViewInfoAtBottom.Border
    else
      ABorderAtBottom := nil;

    ACornerViewInfo := TdxCornerViewInfoBase.CreateCorner(AConverter, ABorderAtLeft, ABorderAtTop, ABorderAtRight, ABorderAtBottom, 0);
    AAnchor.Corners.Add(ACornerViewInfo);
    if ABorderViewInfoAtTop <> nil then
    begin
      Assert(ABorderViewInfoAtTop.CornerAtBottom = nil);
      ABorderViewInfoAtTop.CornerAtBottom := ACornerViewInfo;
    end;
    if ABorderViewInfoAtBottom <> nil then
    begin
      Assert(ABorderViewInfoAtBottom.CornerAtTop = nil);
      ABorderViewInfoAtBottom.CornerAtTop := ACornerViewInfo;
    end;
  end;
end;

procedure TdxTableViewInfo.MoveVertically(ADeltaY: TdxLayoutUnit);
var
  I, ACount: Integer;
begin
  ACount := Anchors.Count;
  for I := 0 to ACount - 1 do
    Anchors[I].MoveVertically(ADeltaY);
end;

procedure TdxTableViewInfo.MoveVerticallyRecursive(ADeltaY: TdxLayoutUnit);
var
  I, ACount: Integer;
begin
  MoveVertically(ADeltaY);
  ACount := Cells.Count;
  for I := 0 to ACount - 1 do
    Cells[I].InnerTables.MoveVertically(ADeltaY);
end;

function TdxTableViewInfo.GetAlignedPosition(AHorizontalPositionIndex: Integer): Integer;
begin
  Result := VerticalBorderPositions.AlignedPosition[AHorizontalPositionIndex] + Column.Bounds.Left;
end;

function TdxTableViewInfo.GetActualBottomPosition: TdxLayoutUnit;
var
  ALastAnchor: TdxTableCellVerticalAnchor;
begin
  ALastAnchor := Anchors.Last;
  Result := ALastAnchor.VerticalPosition;

  if NextTableViewInfo = nil then
    Inc(Result, ALastAnchor.BottomTextIndent)
end;

function TdxTableViewInfo.GetTableBottom: Integer;
var
  ALastAnchor: TdxTableCellVerticalAnchor;
begin
  ALastAnchor := Anchors.Last;
  Result := ALastAnchor.VerticalPosition + ALastAnchor.BottomTextIndent;
end;

function TdxTableViewInfo.GetTableTop: Integer;
var
  AFirstAnchor: TdxTableCellVerticalAnchor;
begin
  AFirstAnchor := Anchors.First;
  Result := AFirstAnchor.VerticalPosition
end;

{ TdxNumberingListBox }

class function TdxNumberingListBox.CreateBox: TdxBox;
begin
  Result := TdxNumberingListBox.Create;
end;

function TdxNumberingListBox.GetNumerationCharacterProperties(APieceTable: TdxCustomPieceTable): TdxMergedCharacterProperties;
var
  AParagraph: TdxParagraph;
begin
  AParagraph := TdxParagraph(APieceTable.Runs[StartPos.RunIndex].Paragraph);
  Result := AParagraph.GetNumerationCharacterProperties;
end;

procedure TdxNumberingListBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportNumberingListBox(Self);
end;

function TdxNumberingListBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := (ACalculator as IdxBoxHitTestCalculator).CreateNumberingListBoxHitTestManager(Self);
end;

function TdxNumberingListBox.GetDocumentPosition(APieceTable: TdxCustomPieceTable;
  const APos: TdxFormatterPosition): TdxDocumentModelPosition;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := inherited GetDocumentPosition(APieceTable, APos);
end;

function TdxNumberingListBox.GetFirstFormatterPosition: TdxFormatterPosition;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := inherited GetFirstFormatterPosition;
end;

function TdxNumberingListBox.GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := GetFirstPosition(APieceTable);
end;

function TdxNumberingListBox.GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo;
var
  AParagraph: TdxParagraph;
begin
  AParagraph := TdxParagraph(APieceTable.Runs[Self.StartPos.RunIndex].Paragraph);
  Result := AParagraph.GetNumerationFontInfo;
end;

function TdxNumberingListBox.GetActualForeColor(APieceTable: TdxCustomPieceTable; ATextColors: TdxTextColors;
  ABackColor: TdxAlphaColor): TdxAlphaColor;
var
  AColor: TdxAlphaColor;
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  ACharacterProperties := GetNumerationCharacterProperties(APieceTable);
  try
    AColor := ACharacterProperties.Info.ForeColor;
    Result := TdxAutoColorUtils.GetActualForeColor(ABackColor, AColor, ATextColors);
  finally
    ACharacterProperties.Free;
  end;
end;

function TdxNumberingListBox.GetFontUnderlineType(APieceTable: TdxCustomPieceTable): TdxUnderlineType;
var
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  ACharacterProperties := GetNumerationCharacterProperties(APieceTable);
  try
    Result := ACharacterProperties.Info.FontUnderlineType;
  finally
    ACharacterProperties.Free;
  end;
end;

function TdxNumberingListBox.GetFontStrikeoutType(APieceTable: TdxCustomPieceTable): TdxStrikeoutType;
var
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  ACharacterProperties := GetNumerationCharacterProperties(APieceTable);
  try
    Result := ACharacterProperties.Info.FontStrikeoutType;
  finally
    ACharacterProperties.Free;
  end;
end;

function TdxNumberingListBox.GetLastFormatterPosition: TdxFormatterPosition;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := inherited GetLastFormatterPosition;
end;

function TdxNumberingListBox.GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := inherited GetLastPosition(APieceTable);
end;

function TdxNumberingListBox.GetStrikeoutColorCore(APieceTable: TdxCustomPieceTable): TdxAlphaColor;
var
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  ACharacterProperties := GetNumerationCharacterProperties(APieceTable);
  try
    Result := ACharacterProperties.Info.StrikeoutColor;
  finally
    ACharacterProperties.Free;
  end;
end;

function TdxNumberingListBox.GetText(ATable: TdxCustomPieceTable): string;
begin
  Result := NumberingListText;
end;

function TdxNumberingListBox.GetUnderlineColorCore(APieceTable: TdxCustomPieceTable): TdxAlphaColor;
var
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  ACharacterProperties := GetNumerationCharacterProperties(APieceTable);
  try
    Result := ACharacterProperties.Info.UnderlineColor;
  finally
    ACharacterProperties.Free;
  end;
end;

function TdxNumberingListBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxNumberingListBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxNumberingListBox.IsVisible: Boolean;
begin
  Result := True;
end;

function TdxNumberingListBox.IsHyperlinkSupported: Boolean;
begin
  Result := False;
end;

{ TdxNumberingListBoxWithSeparator }

destructor TdxNumberingListBoxWithSeparator.Destroy;
begin
  SeparatorBox := nil;
  inherited Destroy;
end;

procedure TdxNumberingListBoxWithSeparator.OffsetRunIndices(ADelta: Integer);
begin
  inherited OffsetRunIndices(ADelta);
  SeparatorBox.OffsetRunIndices(ADelta);
end;

procedure TdxNumberingListBoxWithSeparator.SetSeparatorBox(const Value: TdxBox);
begin
  if FSeparatorBox <> Value then
  begin
    TdxBox.Release(FSeparatorBox);
    FSeparatorBox := Value;
    TdxBox.AddReference(FSeparatorBox);
  end;
end;

{ TdxPageBreakBox }

class function TdxPageBreakBox.CreateBox: TdxBox;
begin
  Result := TdxPageBreakBox.Create;
end;

function TdxPageBreakBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := (ACalculator as IdxBoxHitTestCalculator).CreatePageBreakBoxHitTestManager(Self);
end;

procedure TdxPageBreakBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportPageBreakBox(Self);
end;

function TdxPageBreakBox.IsLineBreak: Boolean;
begin
  Result := True;
end;

function TdxPageBreakBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxPageBreakBox.IsPageBreakBox: Boolean;
begin
  Result := True;
end;

function TdxPageBreakBox.IsSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxPageBreakBox.IsVisible: Boolean;
begin
  Result := False;
end;

{ TdxColumnBreakBox }

class function TdxColumnBreakBox.CreateBox: TdxBox;
begin
  Result := TdxColumnBreakBox.Create;
end;

function TdxColumnBreakBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := (ACalculator as IdxBoxHitTestCalculator).CreateColumnBreakBoxHitTestManager(Self);
end;

procedure TdxColumnBreakBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportColumnBreakBox(Self);
end;

function TdxColumnBreakBox.IsColumnBreakBox: Boolean;
begin
  Result := True;
end;

function TdxColumnBreakBox.IsLineBreak: Boolean;
begin
  Result := True;
end;

function TdxColumnBreakBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxColumnBreakBox.IsVisible: Boolean;
begin
  Result := False;
end;

{ TdxNumberingListBoxInfo }

function TdxNumberingListBoxInfo.GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo;
begin
  Result := Box.GetFontInfo(APieceTable);
end;

function TdxNumberingListBoxInfo.GetForceUpdateCurrentRowHeight: Boolean;
begin
  Result := True;
end;

{ TdxTableCellRow }

constructor TdxTableCellRow.Create(ACellViewInfo: TdxTableCellViewInfo);
begin
  inherited Create;
  FCellViewInfo := ACellViewInfo;
end;

function TdxTableCellRow.GetCellViewInfo: TdxTableCellViewInfo;
begin
  Result := FCellViewInfo;
end;

function TdxTableCellRow.IsTabelCellRow: Boolean;
begin
  Result := True;
end;

{ TdxRowCollection }

procedure TdxRowCollection.RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox);
var
  ABoxCalculator: TdxBoxHitTestCalculator;
begin
  ABoxCalculator := TdxBoxHitTestCalculator(ACalculator);
  ABoxCalculator.HitTestResult.Row := TdxRow(AItem);
  ABoxCalculator.HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Row);
end;

procedure TdxRowCollection.RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator);
begin
  TdxBoxHitTestCalculator(ACalculator).HitTestResult.Row := nil;
end;

{ TdxFloatingObjectAnchorBox }

function TdxFloatingObjectAnchorBox.CalcAscentAndFree(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := Bounds.Height;
end;

function TdxFloatingObjectAnchorBox.CalcBaseAscentAndFree(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := CalcAscentAndFree(APieceTable);
end;

function TdxFloatingObjectAnchorBox.CalcBaseDescent(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := CalcDescent(APieceTable);
end;

function TdxFloatingObjectAnchorBox.CalcDescent(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := 0;
end;

class function TdxFloatingObjectAnchorBox.CreateBox: TdxBox;
begin
  Result := TdxFloatingObjectAnchorBox.Create;
end;

function TdxFloatingObjectAnchorBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := nil;
end;

procedure TdxFloatingObjectAnchorBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
end;

function TdxFloatingObjectAnchorBox.IsVisible: Boolean;
begin
  Result := True;
end;

function TdxFloatingObjectAnchorBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

procedure TdxFloatingObjectAnchorBox.IncreaseHeight(ADelta: Integer);
begin
  Bounds := TRect.CreateSize(Bounds.Left, Bounds.Top, Bounds.Width, Bounds.Height + ADelta);
  FActualBounds.Height := FActualBounds.Height + ADelta;
  FActualSizeBounds.Height := FActualSizeBounds.Height + ADelta;
  FContentBounds.Height := FContentBounds.Height + ADelta;
  FShapeBounds.Height := FShapeBounds.Height + ADelta;
end;

function TdxFloatingObjectAnchorBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxFloatingObjectAnchorBox.GetActualSizeBounds: TRect;
begin
  Result := FActualSizeBounds;
end;

function TdxFloatingObjectAnchorBox.GetFloatingObjectRun(APieceTable: TdxCustomPieceTable): TdxFloatingObjectAnchorRun;
begin
  if FAnchorRun = nil then
    FAnchorRun := TdxFloatingObjectAnchorRun(GetRun(APieceTable));
  Result := FAnchorRun;
end;

procedure TdxFloatingObjectAnchorBox.SetFloatingObjectRun(AAnchorRun: TdxFloatingObjectAnchorRun);
begin
  FAnchorRun := AAnchorRun;
end;

procedure TdxFloatingObjectAnchorBox.SetActualSizeBounds(const ABounds: TRect);
begin
  FActualSizeBounds := ABounds;
end;

{ TdxExactPageAndLogPositionComparable }

function TdxExactPageAndLogPositionComparable.CompareTo(const Value: TdxBoxBase): Integer;
var
  AFirstPos: TdxDocumentModelPosition;
  AComparable: TdxExactPageAreaAndLogPositionComparable;
  AFirstAreaCompareResult, ALastAreaCompareResult: Integer;
  APage: TdxPage;
begin
  AFirstPos := Value.GetFirstPosition(PieceTable);
  if AFirstPos.LogPosition > LogPosition then
    Result := 1
  else
    if AFirstPos.LogPosition = LogPosition then
      Result := 0
    else
    begin
      AComparable := TdxExactPageAreaAndLogPositionComparable.Create(PieceTable, LogPosition);
      try
        APage := Value as TdxPage;
        AFirstAreaCompareResult := AComparable.CompareTo(APage.Areas[0]);
        if (APage.Areas.Count = 1) or (AFirstAreaCompareResult >= 0) then
          Result := AFirstAreaCompareResult
        else
        begin
          ALastAreaCompareResult := AComparable.CompareTo(APage.Areas.Last);
          if ALastAreaCompareResult >= 0 then
            Result := 0
          else
            Result := -1;
        end;
      finally
        AComparable.Free;
      end;
    end;
end;

{ TdxExactPageAreaAndLogPositionComparable }

function TdxExactPageAreaAndLogPositionComparable.CompareTo(const Value: TdxBoxBase): Integer;
var
  AFirstPos: TdxDocumentModelPosition;
  AArea: TdxPageArea;
  AComparable: TdxExactColumnAndLogPositionComparable;
  AFirstBoxCompareResult, ALastBoxCompareResult: Integer;
begin
  AArea := Value as TdxPageArea;
  AFirstPos := AArea.GetFirstPosition(PieceTable);
  if AFirstPos.LogPosition > LogPosition then
    Result := 1
  else
  begin
    if AFirstPos.LogPosition = LogPosition then
      Result := 0
    else
    begin
      AComparable := TdxExactColumnAndLogPositionComparable.Create(PieceTable, LogPosition);
      try
        AFirstBoxCompareResult := AComparable.CompareTo(AArea.Columns[0]);
        if (AArea.Columns.Count = 1) or (AFirstBoxCompareResult >= 0) then
          Result := AFirstBoxCompareResult
        else
        begin
          ALastBoxCompareResult := AComparable.CompareTo(AArea.Columns.Last);
          if ALastBoxCompareResult >= 0 then
            Result := 0
          else
            Result := -1;
        end;
      finally
        AComparable.Free;
      end;
    end;
  end;
end;

{ TdxExactColumnAndLogPositionComparable }

function TdxExactColumnAndLogPositionComparable.CompareTo(const Value: TdxBoxBase): Integer;
var
  AColumn: TdxColumn;
  ACell: TdxTableCell;
  AParagraph: TdxParagraph;
  AFirstRow, ALastRow: TdxRow;
  ATableViewInfo: TdxTableViewInfo;
  AParagraphIndex: TdxParagraphIndex;
  ATables: TdxTableViewInfoCollection;
  AFirstPosition, ALastPosition: TdxDocumentLogPosition;
  ATableCellViewInfo: TdxTableCellViewInfo;
  AFirstPos, ALastPos: TdxDocumentModelPosition;
begin
  AColumn := Value as TdxColumn;
  AFirstPos := AColumn.GetFirstPosition(PieceTable);
  if AFirstPos.LogPosition > LogPosition then
    Result := 1
  else
  begin
    if AFirstPos.LogPosition = LogPosition then
      Result := 0
    else
    begin
      ATables := AColumn.InnerTables;
      if (ATables <> nil) and (ATables.Count > 0) then
      begin
        AParagraphIndex := PieceTable.FindParagraphIndex(LogPosition);
        AParagraph := TdxParagraph(PieceTable.Paragraphs[AParagraphIndex]);
        ACell := AParagraph.GetCell;
        while ACell <> nil do
        begin
          ATableViewInfo := FindTable(ATables, ACell, ATableCellViewInfo);
          if ATableViewInfo <> nil then
          begin
            if ATableViewInfo.SingleColumn then
              Exit(0);
            if ATableCellViewInfo <> nil then
            begin
              if ATableCellViewInfo.SingleColumn then
                Exit(0);
              AFirstRow := ATableCellViewInfo.GetFirstRow(AColumn);
              if AFirstRow = nil then
                Exit(1);
              AFirstPosition := AFirstRow.GetFirstPosition(PieceTable).LogPosition;
              if AFirstPosition > LogPosition then
                Exit(1);
              ALastRow := ATableCellViewInfo.GetLastRow(AColumn);
              if ALastRow <> nil then
              begin
                ALastPosition := ALastRow.GetLastPosition(PieceTable).LogPosition;
                if ALastPosition < LogPosition then
                  Exit(-1)
                else
                  Exit(0);
              end
              else
                Exit(1);
            end
            else
              Exit(ATableViewInfo.TopRowIndex - ACell.Table.Rows.IndexOf(ACell.Row));
          end;
          ACell := ACell.Table.ParentCell;
        end;
      end;
      ALastPos := AColumn.GetLastPosition(PieceTable);
      if ALastPos.LogPosition >= LogPosition then
        Result := 0
      else
        Result := -1;
    end;
  end;
end;

function TdxExactColumnAndLogPositionComparable.FindTable(ATables: TdxTableViewInfoCollection;
  ACell: TdxTableCell; out ACellViewInfo: TdxTableCellViewInfo): TdxTableViewInfo;
var
  ATableIndex, ACellIndex: Integer;
  ATableViewInfo: TdxTableViewInfo;
begin
  Result := nil;
  for ATableIndex := 0 to ATables.Count - 1 do
  begin
    ATableViewInfo := ATables[ATableIndex];
    if ATableViewInfo.Table = ACell.Table then
      Result := ATableViewInfo;
    for ACellIndex := 0 to ATableViewInfo.Cells.Count - 1 do
      if ATableViewInfo.Cells[ACellIndex].Cell = ACell then
      begin
        ACellViewInfo := ATableViewInfo.Cells[ACellIndex];
        Exit(Result);
      end;
  end;
  ACellViewInfo := nil;
end;

{ TdxFootNotePageArea }

constructor TdxFootNotePageArea.Create(AFootNote: TdxFootNote; ASection: TdxSection; AReferenceRunIndex: TdxRunIndex);
begin
  inherited Create(AFootNote, ASection);
  FReferenceRunIndex := AReferenceRunIndex;
end;

function TdxFootNotePageArea.GetNote: TdxFootNote;
begin
  Result := TdxFootNote(PieceTable.ContentType);
end;

{ TdxFootNotePageAreaList }

function TdxFootNotePageAreaList.GetItem(Index: Integer): TdxFootNotePageArea;
begin
  Result := TdxFootNotePageArea(inherited Items[Index]);
end;

{ TdxBookmarkStartBox }

procedure TdxBookmarkStartBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportBookmarkStartBox(Self);
end;

{ TdxBookmarkEndBox }

procedure TdxBookmarkEndBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportBookmarkEndBox(Self);
end;

{ TdxLineNumberBox }

constructor TdxLineNumberBox.Create(ARun: TdxLineNumberCommonRun; ARow: TdxRow; const AText: string);
begin
  inherited Create;
  Assert(ARun <> nil);
  Assert(ARow <> nil);
  FRun := ARun;
  FRow := ARow;
  FText := AText;
  StartPos := TdxFormatterPosition.Create(0, 0, 0);
  EndPos := TdxFormatterPosition.Create(0, 0, 0);
end;

class function TdxLineNumberBox.CreateBox: TdxBox;
begin
  Result := NotImplemented;
end;

function TdxLineNumberBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := nil;
end;

procedure TdxLineNumberBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportLineNumberBox(Self);
end;

function TdxLineNumberBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxLineNumberBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxLineNumberBox.IsVisible: Boolean;
begin
  Result := True;
end;

function TdxLineNumberBox.GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase;
begin
  Result := FRun;
end;

function TdxLineNumberBox.GetText(ATable: TdxCustomPieceTable): string;
begin
  Result := FText;
end;

{ TdxDocumentLayout }

constructor TdxDocumentLayout.Create(ADocumentModel: TdxCustomDocumentModel; const AMeasurerProvider: IdxBoxMeasurerProvider);
begin
  inherited Create;
  FFirstVisiblePageIndex := -1;
  FLastVisiblePageIndex := -1;
  Assert(ADocumentModel <> nil);
  Assert(AMeasurerProvider <> nil);
  FDocumentModel := ADocumentModel;
  FMeasurerProvider := AMeasurerProvider;
  FPages := TdxPageCollection.Create;
  FCounters := TdxCounters.Create;
  FCounters.RegisterCounter(TdxFootNote.FootNoteCounterId);
  FCounters.RegisterCounter(TdxEndNote.EndNoteCounterId);
end;

destructor TdxDocumentLayout.Destroy;
begin
  FreeAndNil(FCounters);
  FreeAndNil(FPages);
  inherited Destroy;
end;

function TdxDocumentLayout.GetUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  Result := DocumentModel.LayoutUnitConverter;
end;

function TdxDocumentLayout.GetMeasurer: TdxBoxMeasurer;
begin
  Result := FMeasurerProvider.Measurer;
end;

function TdxDocumentLayout.CreateLayoutPosition(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition;
  APreferredPageIndex: Integer): TObject;
begin
  if APieceTable.ContentType.IsHeaderFooter then
    Result := TdxHeaderFooterDocumentLayoutPosition.Create(Self, TdxPieceTable(APieceTable), ALogPosition, APreferredPageIndex)
  else
    if APieceTable.ContentType.IsTextBox then
      Result := TdxTextBoxDocumentLayoutPosition.Create(Self, TdxTextBoxContentType(APieceTable.ContentType),
        ALogPosition, APreferredPageIndex)
    else
      Result := TdxDocumentLayoutPosition.Create(Self, APieceTable, ALogPosition);
end;

function TdxDocumentLayout.CreateDetailRow(ARow: TdxRow): TdxDetailRow;
begin
  DoBeforeCreateDetailRow;
  try
    Result := CreateDetailRowCore(ARow);
  finally
    DoAfterCreateDetailRow;
  end;
end;

function TdxDocumentLayout.CreateDetailRowForBox(ARow: TdxRow; ABox: TdxBox; ASuppressSuspendFormatting: Boolean): TdxDetailRow;
begin
  if not ASuppressSuspendFormatting then
    DoBeforeCreateDetailRow;
  try
    Result := CreateDetailRowForBoxCore(ARow, ABox);
  finally
    if not ASuppressSuspendFormatting then
      DoAfterCreateDetailRow;
  end;
end;

function TdxDocumentLayout.CreateDetailRowCore(ARow: TdxRow): TdxDetailRow;
var
  AChars: TdxCharacterBoxCollection;
  AExporter: TdxCharacterBoxLevelDocumentLayoutExporter;
begin
  Result := TdxDetailRow.Create;
  AChars := Result.Characters;
  AExporter := TdxCharacterBoxLevelDocumentLayoutExporter.Create(TdxDocumentModel(DocumentModel), AChars, Measurer);
  try
    AExporter.ExportRow(ARow);
  finally
    AExporter.Free;
  end;
end;

function TdxDocumentLayout.CreateDetailRowForBoxCore(ARow: TdxRow; ABox: TdxBox): TdxDetailRow;
var
  AChars: TdxCharacterBoxCollection;
  AExporter: TdxCharacterBoxLevelDocumentLayoutExporter;
begin
  Result := TdxDetailRow.Create;
  AChars := Result.Characters;
  AExporter := TdxCharacterBoxLevelDocumentLayoutExporter.Create(TdxDocumentModel(DocumentModel), AChars, Measurer);
  try
    AExporter.ExportRowBox(ARow, ABox);
  finally
    AExporter.Free;
  end;
end;

procedure TdxDocumentLayout.DoAfterCreateDetailRow;
begin
  if not FOnAfterCreateDetailRow.Empty then
    FOnAfterCreateDetailRow.Invoke(Self);
end;

procedure TdxDocumentLayout.DoBeforeCreateDetailRow;
begin
  if not FOnBeforeCreateDetailRow.Empty then
    FOnBeforeCreateDetailRow.Invoke(Self);
end;

{ TdxSectionMarkBox }

class function TdxSectionMarkBox.CreateBox: TdxBox;
begin
  Result := TdxSectionMarkBox.Create;
end;

function TdxSectionMarkBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := (ACalculator as IdxBoxHitTestCalculator).CreateSectionMarkBoxHitTestManager(Self);
end;

procedure TdxSectionMarkBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportSectionMarkBox(Self);
end;

function TdxSectionMarkBox.IsLineBreak: Boolean;
begin
  Result := True;
end;

function TdxSectionMarkBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxSectionMarkBox.IsSectionMarkBox: Boolean;
begin
  Result := True;
end;

function TdxSectionMarkBox.IsVisible: Boolean;
begin
  Result := False;
end;

{ TdxLayoutDependentTextBox }

class function TdxLayoutDependentTextBox.CreateBox: TdxBox;
begin
  Result := TdxLayoutDependentTextBox.Create;
end;

procedure TdxLayoutDependentTextBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportLayoutDependentTextBox(Self);
end;

function TdxLayoutDependentTextBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := (ACalculator as IdxBoxHitTestCalculator).CreateTextBoxHitTestManager(Self);
end;

function TdxLayoutDependentTextBox.GetText(ATable: TdxCustomPieceTable): string;
begin
  Result := CalculatedText;
end;

{ TdxColumn }

constructor TdxColumn.Create;
begin
  inherited Create;
  FRows := TdxRowCollection.Create;
  FChildren := TdxBoxList.Create;
end;

destructor TdxColumn.Destroy;
begin
  FreeAndNil(FChildren);
  FreeAndNil(FRows);
  FreeAndNil(FExtendedBoxes);
  inherited Destroy;
end;

procedure TdxColumn.AddParagraphFrame(AParagraph: TdxSimpleParagraph);
begin
  if not ContainsParagraphFrame(AParagraph) then
    ParagraphFrames.Add(TdxParagraphFrameBox.Create(AParagraph));
end;

procedure TdxColumn.ClearInvalidatedParagraphFrames(AFrom: TdxParagraphIndex);
begin
  ClearInvalidatedParagraphFramesCore(AFrom, MaxInt);
end;

procedure TdxColumn.ClearInvalidatedParagraphFramesCore(AFrom, ATo: TdxParagraphIndex);
begin
  if InnerParagraphFrames <> nil then
    InnerParagraphFrames.ClearInvalidatedParagraphFrames(AFrom, ATo);
end;

function TdxColumn.ContainsParagraphFrame(AParagraph: TdxParagraphBase): Boolean;
var
  I, ACount: Integer;
begin
  ACount := ParagraphFrames.Count;
  for I := 0 to ACount - 1 do
  begin
    if (ParagraphFrames[I].ParagraphIndex = AParagraph.Index) and (ParagraphFrames[I].PieceTable = AParagraph.PieceTable) then
      Exit(True);
  end;
  Result := False;
end;

class function TdxColumn.CreateBox: TdxBox;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxColumn.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := (ACalculator as IdxBoxHitTestCalculator).CreateColumnHitTestManager(Self);
end;

procedure TdxColumn.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportColumn(Self);
end;

function TdxColumn.GetDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Column;
end;

function TdxColumn.GetExtendedBoxes: TdxColumnExtendedBoxes;
begin
  if FExtendedBoxes = nil then
    FExtendedBoxes := TdxColumnExtendedBoxes.Create;
  Result := FExtendedBoxes;
end;

function TdxColumn.GetFirstFormatterPosition: TdxFormatterPosition;
begin
  if IsEmpty then
    Result := TdxFormatterPosition.MaxValue
  else
    Result := Rows.First.GetFirstFormatterPosition;
end;

function TdxColumn.IsVisible: Boolean;
begin
  Result := True;
end;

function TdxColumn.GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  if IsEmpty then
    Result := TdxDocumentModelPosition.MaxValue
  else
    Result := Rows.First.GetFirstPosition(APieceTable);
end;

function TdxColumn.GetInnerParagraphFrames: TdxParagraphFrameBoxCollection;
begin
  if InnerExtendedBoxes = nil then
    Result := nil
  else
    Result := InnerExtendedBoxes.InnerParagraphFrames;
end;

function TdxColumn.GetInnerTables: TdxTableViewInfoCollection;
begin
  if InnerExtendedBoxes = nil then
    Result := nil
  else
    Result := InnerExtendedBoxes.InnerTables;
end;

function TdxColumn.GetIsEmpty: Boolean;
begin
  Result := FRows.Count <= 0;
end;

function TdxColumn.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxColumn.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxColumn.GetLastFormatterPosition: TdxFormatterPosition;
begin
  if IsEmpty then
    Result := TdxFormatterPosition.MaxValue
  else
    Result := Rows.Last.GetLastFormatterPosition;
end;

function TdxColumn.GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  if IsEmpty then
    Result := TdxDocumentModelPosition.MaxValue
  else
    Result := Rows.Last.GetLastPosition(APieceTable);
end;

function TdxColumn.IsIntersectedWithPrevColumn: Boolean;
var
  I, ARowCount, ACellCount: Integer;
  AFirstRow: TdxTableCellRow;
  ATopLevelTableViewInfo: TdxTableViewInfo;
  AFirstTableRow: TdxTableRowViewInfoBase;
  ACells: TdxTableCellViewInfoCollection;
begin
  ARowCount := FRows.Count;
  if ARowCount = 0 then
  begin
    Assert(False);
    Exit(False);
  end;
  if not (FRows[0] is TdxTableCellRow) then
    Exit(False);
  AFirstRow := FRows[0] as TdxTableCellRow;
  ATopLevelTableViewInfo := AFirstRow.CellViewInfo.TableViewInfo;
  while ATopLevelTableViewInfo.ParentTableCellViewInfo <> nil do
    ATopLevelTableViewInfo := ATopLevelTableViewInfo.ParentTableCellViewInfo.TableViewInfo;
  AFirstTableRow := ATopLevelTableViewInfo.Rows.First;
  ACells := AFirstTableRow.Cells;
  ACellCount := ACells.Count;
  for I := 0 to ACellCount - 1 do
    if ACells[I].IsStartOnPreviousTableViewInfo then
      Exit(True);
  Result := False;
end;

function TdxColumn.GetText(ATable: TdxCustomPieceTable): string;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := '';
end;

function TdxColumn.GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxColumn.GetOwnRows: TdxRowCollection;
begin
  Result := Rows;
end;

function TdxColumn.GetParagraphFrames: TdxParagraphFrameBoxCollection;
begin
  Result := ExtendedBoxes.ParagraphFrames;
end;

function TdxColumn.GetRows: TdxRowCollection;
begin
  Result := FRows;
end;

function TdxColumn.GetShouldProcessParagraphFrames: Boolean;
begin
  Result := (InnerParagraphFrames <> nil) and (InnerParagraphFrames.Count > 0);
end;

function TdxColumn.GetShouldProcessTables: Boolean;
begin
  Result := (InnerTables <> nil) and (InnerTables.Count > 0);
end;

function TdxColumn.GetTables: TdxTableViewInfoCollection;
begin
  Result := ExtendedBoxes.Tables;
end;

function TdxColumn.GetTopLevelColumn: TdxColumn;
begin
  Result := Self;
end;

procedure TdxColumn.MoveVertically(ADeltaY: Integer);
begin
  inherited MoveVertically(ADeltaY);
  Rows.MoveVertically(ADeltaY);
  if InnerTables <> nil then
    InnerTables.MoveVertically(ADeltaY);
  if InnerParagraphFrames <> nil then
    InnerParagraphFrames.MoveVertically(ADeltaY);
end;

procedure TdxColumn.RemoveTableViewInfoWithContent(ATableViewInfo: TdxTableViewInfo);
begin
  ATableViewInfo.RemoveRowsFromIndex(0);
  if InnerTables <> nil then
    InnerTables.Remove(ATableViewInfo);
end;

procedure TdxColumn.RemoveTableCellViewInfoContent(ACell: TdxTableCellViewInfo);
var
  I, ARowCount: Integer;
  ARow: TdxTableCellRow;
begin
  ARowCount := FRows.Count;
  for I := ARowCount - 1 downto 0 do
  begin
    if FRows[I] is TdxTableCellRow then
    begin
      ARow := TdxTableCellRow(FRows[I]);
      if ARow.CellViewInfo = ACell then
        FRows.Delete(I);
    end;
  end;
  ClearInvalidatedParagraphFramesCore(ACell.Cell.StartParagraphIndex, ACell.Cell.EndParagraphIndex);
end;

procedure TdxColumn.RemoveEmptyTableViewInfos;
var
  ATableRemoved: Boolean;
  I, ACount: Integer;
  AViewInfo: TdxTableViewInfo;
begin
  if InnerTables = nil then
    Exit;
  repeat
    ATableRemoved := False;
    ACount := InnerTables.Count;
    for I := ACount - 1 downto 1 do
    begin
      AViewInfo := InnerTables[I];
      AViewInfo.RemoveEmptyCells;
      if AViewInfo.Cells.Count = 0 then
      begin
        InnerTables.Delete(I);
        ATableRemoved := True;
      end;
    end;
  until not ATableRemoved;
end;

function TdxColumn.GetHitTestAccuracy: TdxHitTestAccuracy;
begin
  Result := ExactColumn;
end;

{ TdxColumnCollection }

procedure TdxColumnCollection.RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator);
begin
  TdxBoxHitTestCalculator(ACalculator).HitTestResult.Column := nil;
end;

procedure TdxColumnCollection.RegisterSuccessfullItemHitTest(
  ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox);
begin
  TdxBoxHitTestCalculator(ACalculator).HitTestResult.Column := TdxColumn(AItem);
  TdxBoxHitTestCalculator(ACalculator).HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Column);
end;

{ TdxCustomMarkBox }

procedure TdxCustomMarkBox.MoveVertically(ADeltaY: Integer);
begin
  Inc(FBounds.Top, ADeltaY);
  Inc(FBounds.Bottom, ADeltaY);
end;

procedure TdxCustomMarkBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportCustomMarkBox(Self);
end;

{ TdxCustomMarkBoxCollection }

procedure TdxCustomMarkBoxCollection.MoveVertically(ADeltaY: Integer);
var
  ACount, I: Integer;
begin
  ACount := Count;
  for I := 0 to ACount - 1 do
    Items[I].MoveVertically(ADeltaY);
end;

{ TdxPageAreaCollection }

procedure TdxPageAreaCollection.RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox);
var
  ABoxCalculator: TdxBoxHitTestCalculator;
begin
  ABoxCalculator := TdxBoxHitTestCalculator(ACalculator);
  ABoxCalculator.HitTestResult.PageArea := TdxPageArea(AItem);
  ABoxCalculator.HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.PageArea);
end;

procedure TdxPageAreaCollection.RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator);
begin
  TdxBoxHitTestCalculator(ACalculator).HitTestResult.PageArea := nil;
end;

{ TIndexComparer }

function TIndexComparer.Compare(const ALeft, ARight: TdxBoxBase): Integer;
begin
  Result := CompareInteger(TdxParagraphFrameBox(ALeft).ParagraphIndex, TdxParagraphFrameBox(ARight).ParagraphIndex);
end;

{ TdxParagraphFrameBox }

constructor TdxParagraphFrameBox.Create(AParagraph: TdxSimpleParagraph);
begin
  Create(AParagraph.PieceTable, AParagraph.Index);
end;

constructor TdxParagraphFrameBox.Create(APieceTable: TdxSimplePieceTable;
  AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FParagraphIndex := AParagraphIndex;
  FLastParagraphIndex := AParagraphIndex;
  FRowCollection := TdxRowCollection.Create;
end;

destructor TdxParagraphFrameBox.Destroy;
begin
  FreeAndNil(FDocumentLayout);
  FreeAndNil(FRowCollection);
  inherited Destroy;
end;

class constructor TdxParagraphFrameBox.Initialize;
begin
  FIndexComparer := TIndexComparer.Create;
end;

class destructor TdxParagraphFrameBox.Finalize;
begin
  FreeAndNil(FIndexComparer);
end;

function TdxParagraphFrameBox.CreateBox: TdxBox;
begin
  Result := TdxParagraphFrameBox.Create(PieceTable, ParagraphIndex);
end;

procedure TdxParagraphFrameBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportParagraphFrameBox(self)
end;

function TdxParagraphFrameBox.GetActualSizeBounds: TRect;
begin
  Result := FActualSizeBounds;
end;

function TdxParagraphFrameBox.GetX: Integer;
begin
  Result := FExtendedBounds.Left;
end;

function TdxParagraphFrameBox.GetY: Integer;
begin
  Result := FExtendedBounds.Top;
end;

function TdxParagraphFrameBox.IsInCell: Boolean;
begin
  Result := GetParagraph.IsInCell;
end;

function TdxParagraphFrameBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxParagraphFrameBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxParagraphFrameBox.IsVisible: Boolean;
begin
  Result := True;
end;

procedure TdxParagraphFrameBox.SetX(Value: Integer);
begin
  FExtendedBounds.MoveToLeft(Value);
end;

procedure TdxParagraphFrameBox.SetY(Value: Integer);
begin
  FExtendedBounds.MoveToTop(Value);
end;

function TdxParagraphFrameBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  raise TdxNotImplementedException.Create;
end;

class function TdxParagraphFrameBox.CreateTransformUnsafe(AAngle: Single; const ABounds: TRect): TdxTransformMatrix;
var
  ATransform: TdxTransformMatrix;
begin
  if IsZero(Single(dxFMod(AAngle, 360))) then
    Exit(nil);
  ATransform := TdxTransformMatrix.Create;
  ATransform.Rotate(AAngle, dxPointF(ABounds.CenterPoint));
  Exit(ATransform);
end;

function TdxParagraphFrameBox.CreateBackwardTransformUnsafe: TdxTransformMatrix;
begin
  Result := CreateTransformUnsafe(-TdxDocumentModel(PieceTable.DocumentModel).GetBoxEffectiveRotationAngleInDegrees(Self), ActualSizeBounds)
end;

function TdxParagraphFrameBox.CreateTransformUnsafe: TdxTransformMatrix;
begin
  Result := CreateTransformUnsafe(TdxDocumentModel(PieceTable.DocumentModel).GetBoxEffectiveRotationAngleInDegrees(Self), ActualSizeBounds)
end;

function TdxParagraphFrameBox.GetParagraph: TdxSimpleParagraph;
begin
  Result := PieceTable.Paragraphs[ParagraphIndex];
end;

procedure TdxParagraphFrameBox.MoveVertically(ADeltaY: Integer);
begin
  if not HasFrameProperties then
  begin
    inherited MoveVertically(ADeltaY);
    Exit;
  end;

  if not ShouldMoveVertically then
    Exit;

  inherited MoveVertically(ADeltaY);
  FExtendedBounds.Y := FExtendedBounds.Y + ADeltaY;
  FContentBounds.Y := FContentBounds.Y + ADeltaY;
  FActualSizeBounds.Y := FActualSizeBounds.Y + ADeltaY;
  if (FDocumentLayout <> nil) and (FDocumentLayout.Pages.Count > 0) then
    FDocumentLayout.Pages[0].MoveVertically(ADeltaY);
end;

function TdxParagraphFrameBox.ShouldMoveVertically: Boolean;
var
  AVerticalAlignment: TdxParagraphFrameVerticalPositionAlignment;
  AFrameProperties: TdxMergedFrameProperties;
begin
  AFrameProperties := GetFrameProperties;
  try
    AVerticalAlignment := AFrameProperties.Info.VerticalPositionAlignment;
    Result := ((AFrameProperties.Info.Y <> 0) or (AVerticalAlignment = TdxParagraphFrameVerticalPositionAlignment.None) or
      (AVerticalAlignment = TdxParagraphFrameVerticalPositionAlignment.Inline)) and
      (AFrameProperties.Info.VerticalPositionType = TdxParagraphFrameVerticalPositionType.Paragraph);
  finally
    AFrameProperties.Free;
  end;
end;

function TdxParagraphFrameBox.TransformPointBackward(const APoint: TPoint): TPoint;
var
  ATransform: TdxTransformMatrix;
begin
  ATransform := CreateBackwardTransformUnsafe;
  try
    if ATransform = nil then
      Result := APoint
    else
      Result := ATransform.TransformPoint(APoint);
  finally
    ATransform.Free;
  end;
end;

procedure TdxParagraphFrameBox.SetActualSizeBounds(const ABounds: TRect);
begin
  FActualSizeBounds := ABounds;
end;

function TdxParagraphFrameBox.GetFrameProperties: TdxMergedFrameProperties;
begin
  Result := GetParagraph.GetMergedFrameProperties;
end;

function TdxParagraphFrameBox.HasFrameProperties: Boolean;
begin
  Result := GetParagraph.HasFrameProperties;
end;

{ TdxParagraphFrameBoxList }

function TdxParagraphFrameBoxList.GetItem(Index: Integer): TdxParagraphFrameBox;
begin
  Result := TdxParagraphFrameBox(inherited Items[Index]);
end;

{ TdxParagraphFrameBoxAndParagraphIndexComparable }

constructor TdxParagraphFrameBoxAndParagraphIndexComparable.Create(
  AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create;
  FParagraphIndex := AParagraphIndex;
end;

function TdxParagraphFrameBoxAndParagraphIndexComparable.CompareTo(const AOther: TdxParagraphFrameBox): Integer;
begin
  if FParagraphIndex < AOther.ParagraphIndex then
    Result := 1
  else
    if FParagraphIndex > AOther.ParagraphIndex then
      Result := -1
    else
      Result := 0;
end;

{ TdxParagraphFrameBoxCollection }

procedure TdxParagraphFrameBoxCollection.ExportTo(const AExporter: IdxDocumentLayoutExporter);
var
  I, ACount: Integer;
begin
  ACount := Count;
  for I := 0 to ACount - 1 do
    AExporter.ExportParagraphFrameBox(Items[I]);
end;

procedure TdxParagraphFrameBoxCollection.MoveVertically(ADeltaY: TdxLayoutUnit);
var
  I, ACount: Integer;
begin
  ACount := Count;
  for I := 0 to ACount - 1 do
    Items[I].MoveVertically(ADeltaY);
end;

procedure TdxParagraphFrameBoxCollection.ClearInvalidatedParagraphFrames(AFrom, ATo: TdxParagraphIndex);
var
  I, AFirstIndex: Integer;
  AComparable: TdxParagraphFrameBoxAndParagraphIndexComparable;
begin
  AComparable := TdxParagraphFrameBoxAndParagraphIndexComparable.Create(AFrom);
  try
    if not TdxAlgorithms1<TdxParagraphFrameBox>.BinarySearch(Self, AComparable, AFirstIndex) then
    begin
      if AFirstIndex >= Count then
        Exit;
      if Items[AFirstIndex].ParagraphIndex > ATo then
        Exit
    end;
  finally
    AComparable.Free;
  end;

  if ATo = MaxInt then
  begin
    DeleteRange(AFirstIndex, Count - AFirstIndex);
    Exit;
  end;

  for I := Count -1 downto AFirstIndex do
    if Items[I].ParagraphIndex <= ATo then
      Delete(I);
end;

{ TdxColumnExtendedBoxes }

destructor TdxColumnExtendedBoxes.Destroy;
begin
  FreeAndNil(FTables);
  FreeAndNil(FParagraphFrames);
  inherited Destroy;
end;

function TdxColumnExtendedBoxes.GetParagraphFrames: TdxParagraphFrameBoxCollection;
begin
  if FParagraphFrames = nil then
    FParagraphFrames := TdxParagraphFrameBoxCollection.Create;
  Result := FParagraphFrames;
end;

function TdxColumnExtendedBoxes.GetTables: TdxTableViewInfoCollection;
begin
  if FTables = nil then
    FTables := TdxTableViewInfoCollection.Create;
  Result := FTables;
end;

{ TdxCounterItem }

constructor TdxCounterItem.Create(ALogPosition: TdxDocumentLogPosition; AValue: Integer);
begin
  inherited Create;
  FLogPosition := ALogPosition;
  FValue := AValue;
end;

{ TdxCounter }

constructor TdxCounter.Create;
begin
  inherited Create;
  FItems := TdxCounterItemList.Create;
end;

destructor TdxCounter.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TdxCounter.Reset;
begin
  FItems.Clear;
  FLastValue := 1;
end;

procedure TdxCounter.ResetFrom(ALogPosition: TdxDocumentLogPosition);
var
  AIndex: Integer;
  AComparable: TdxCounterItemAndLogPositionComparable;
begin
  AComparable := TdxCounterItemAndLogPositionComparable.Create(ALogPosition);
  try
    TdxAlgorithms1<TdxCounterItem>.BinarySearch(FItems, AComparable, AIndex);
  finally
    AComparable.Free;
  end;

  if AIndex >= FItems.Count then
    Exit;

  FItems.DeleteRange(AIndex, FItems.Count - AIndex);
  if HasItems then
    FLastValue := LastItem.Value
  else
    FLastValue := 1;
end;

function TdxCounter.GetHasItems: Boolean;
begin
  Result := FItems.Count > 0;
end;

function TdxCounter.GetLastItem: TdxCounterItem;
begin
  if HasItems then
    Result := FItems[FItems.Count - 1]
  else
    Result := nil;
end;

function TdxCounter.Increment(ALogPosition: TdxDocumentLogPosition): Integer;
var
  AIndex: Integer;
  AItem: TdxCounterItem;
  AComparable: TdxCounterItemAndLogPositionComparable;
begin
  AComparable := TdxCounterItemAndLogPositionComparable.Create(ALogPosition);
  try
    if TdxAlgorithms1<TdxCounterItem>.BinarySearch(FItems, AComparable, AIndex) then
      Exit(FItems[AIndex].Value);
  finally
    AComparable.Free;
  end;
  AItem := TdxCounterItem.Create(ALogPosition, FLastValue);
  FItems.Add(AItem);
  Inc(FLastValue);
  Result := AItem.Value;
end;

{ TdxCounters }

constructor TdxCounters.Create;
begin
  inherited Create;
  FItems := TdxNamedObjectDictionary<TdxCounter>.Create(True);
end;

destructor TdxCounters.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxCounters.Clear;
begin
  FItems.Clear;
end;

function TdxCounters.GetCounter(const AId: string): TdxCounter;
begin
  Result := FItems[AId];
end;

function TdxCounters.RegisterCounter(const AId: string): TdxCounter;
begin
  Result := TdxCounter.Create;
  FItems.Add(AId, Result);
end;

procedure TdxCounters.Reset;
var
  AId: string;
begin
  for AId in FItems.Keys do
    FItems[AId].Reset;
end;

procedure TdxCounters.ResetFrom(ALogPosition: TdxDocumentLogPosition);
var
  AId: string;
begin
  for AId in FItems.Keys do
    FItems[AId].ResetFrom(ALogPosition);
end;

procedure TdxCounters.UnregisterCounter(const AId: string);
begin
  if FItems.ContainsKey(AId) then
    FItems.Remove(AId);
end;

{ TdxPageCollection }

procedure TdxPageCollection.RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox);
var
  ABoxCalculator: TdxBoxHitTestCalculator;
begin
  ABoxCalculator := TdxBoxHitTestCalculator(ACalculator);
  ABoxCalculator.HitTestResult.Page := TdxPage(AItem);
  ABoxCalculator.HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Page);
end;

procedure TdxPageCollection.RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator);
var
  ABoxCalculator: TdxBoxHitTestCalculator;
begin
  ABoxCalculator := TdxBoxHitTestCalculator(ACalculator);
  ABoxCalculator.HitTestResult.Page := nil;
end;

{ TdxPageArea }

constructor TdxPageArea.Create(APieceTable: TdxContentTypeBase; ASection: TdxSection);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  Assert(ASection <> nil);
  FPieceTable := APieceTable;
  FSection := ASection;
  FColumns := TdxColumnCollection.Create;
  FLineNumbers := TdxLineNumberBoxCollection.Create;
end;

destructor TdxPageArea.Destroy;
begin
  FreeAndNil(FLineNumbers);
  FreeAndNil(FColumns);
  inherited Destroy;
end;

class function TdxPageArea.CreateBox: TdxBox;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxPageArea.GetPieceTable: TdxCustomPieceTable;
begin
  Result := FPieceTable.PieceTable;
end;

function TdxPageArea.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := (ACalculator as IdxBoxHitTestCalculator).CreatePageAreaHitTestManager(Self);
end;

procedure TdxPageArea.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportPageArea(Self);
end;

function TdxPageArea.GetDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.PageArea;
end;

function TdxPageArea.GetFirstFormatterPosition: TdxFormatterPosition;
begin
  if IsEmpty then
    Result := TdxFormatterPosition.MaxValue
  else
    Result := Columns.First.GetFirstFormatterPosition;
end;

function TdxPageArea.GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  if IsEmpty then
    Result := TdxDocumentModelPosition.MaxValue
  else
    Result := Columns.First.GetFirstPosition(APieceTable);
end;

function TdxPageArea.GetIsEmpty: Boolean;
begin
  Result := (Columns.Count <= 0) or Columns[0].IsEmpty;
end;

function TdxPageArea.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxPageArea.IsVisible: Boolean;
begin
  Result := True;
end;

function TdxPageArea.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxPageArea.GetLastFormatterPosition: TdxFormatterPosition;
begin
  if IsEmpty then
    Result := TdxFormatterPosition.MaxValue
  else
    Result := Columns.Last.GetLastFormatterPosition;
end;

function TdxPageArea.GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
begin
  if IsEmpty then
    Result := TdxDocumentModelPosition.MaxValue
  else
    Result := Columns.Last.GetLastPosition(APieceTable);
end;

function TdxPageArea.GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxPageArea.GetText(ATable: TdxCustomPieceTable): string;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := '';
end;

procedure TdxPageArea.MoveVertically(ADeltaY: Integer);
begin
  inherited MoveVertically(ADeltaY);
  Columns.MoveVertically(ADeltaY);
  LineNumbers.MoveVertically(ADeltaY);
end;

function TdxPageArea.GetHitTestAccuracy: TdxHitTestAccuracy;
begin
  Result := ExactPageArea;
end;

{ TdxHeaderPageArea }

function TdxHeaderPageArea.GetHeader: TdxSectionHeader;
begin
  Result := TdxSectionHeader(PieceTable.ContentType);
end;

{ TdxFooterPageArea }

function TdxFooterPageArea.GetFooter: TdxSectionFooter;
begin
  Result := TdxSectionFooter(PieceTable.ContentType);
end;

{ TdxPage }

constructor TdxPage.Create(APageNumberSource: TdxPage = nil);
begin
  inherited Create;
  FPageOrdinal := -1;
  FNumPages := -1;
  FNumSkippedPages := 0;
  FPageIndex := -1;
  FAreas := TdxPageAreaCollection.Create;
  FPageNumberSource := APageNumberSource;
end;

destructor TdxPage.Destroy;
begin
  FreeAndNil(FInnerParagraphFrames);
  FreeAndNil(FInnerFloatingObjects);
  FreeAndNil(FInnerForegroundFloatingObjects);
  FreeAndNil(FInnerBackgroundFloatingObjects);
  FreeAndNil(FInnerFootNotes);
  FreeAndNil(FHeader);
  FreeAndNil(FFooter);
  FreeAndNil(FAreas);
  inherited Destroy;
end;

class function TdxPage.CreateBox: TdxBox;
begin
  raise TdxInternalException.Create;
end;

function TdxPage.IsVisible: Boolean;
begin
  Result := True;
end;

function TdxPage.GetPageNumberSource: TdxPage;
begin
  if FPageNumberSource <> nil then
    Result := FPageNumberSource
  else
    Result := Self;
end;

procedure TdxPage.SetFooter(const Value: TdxFooterPageArea);
begin
  if FFooter <> Value then
  begin
    FFooter.Free;
    FFooter := Value;
  end;
end;

procedure TdxPage.SetHeader(const Value: TdxHeaderPageArea);
begin
  if FHeader <> Value then
  begin
    FHeader.Free;
    FHeader := Value;
  end;
end;

function TdxPage.FindFloatingObject(AObjects: TdxFloatingObjectBoxList; ARun: TdxFloatingObjectAnchorRun): TdxFloatingObjectBox;
var
  I: Integer;
begin
  Result := nil;
  if AObjects = nil then
    Exit;

  for I := 0 to AObjects.Count - 1 do
    if AObjects[I].GetFloatingObjectRun = ARun then
      Exit(AObjects[I]);
end;

function TdxPage.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxPage.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxPage.IsEven: Boolean;
begin
  Result := (PageOrdinal mod 2) = 0;
end;

procedure TdxPage.ClearFloatingObjects;
begin
  FreeAndNil(FInnerFloatingObjects);
  FreeAndNil(FInnerForegroundFloatingObjects);
  FreeAndNil(FInnerBackgroundFloatingObjects);
end;

procedure TdxPage.ClearFloatingObjects(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex;
  AObjects: TdxFloatingObjectBoxList);
var
  I: Integer;
begin
  if AObjects = nil then
    Exit;

  for I := AObjects.Count - 1 downto 0 do
    if (APieceTable = nil) or (AObjects[I].PieceTable = APieceTable) and (ARunIndex <= AObjects[I].StartPos.RunIndex) then
      AObjects.Delete(I);
end;

procedure TdxPage.ClearFootNotes(ARunIndex: TdxRunIndex);
var
  I: Integer;
begin
  if InnerFootNotes = nil then
    Exit;
  for I := InnerFootNotes.Count - 1 downto 0 do
    if (ARunIndex <= InnerFootNotes[I].ReferenceRunIndex) then
      InnerFootNotes.Delete(I);
end;

procedure TdxPage.ClearComments(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex);
begin
  if InnerComments = nil then
    Exit;
  NotImplemented
end;

function TdxPage.FindFloatingObject(ARun: TdxFloatingObjectAnchorRun): TdxFloatingObjectBox;
begin
  Result := FindFloatingObject(InnerFloatingObjects, ARun);
  if Result <> nil then
    Exit;

  Result := FindFloatingObject(InnerForegroundFloatingObjects, ARun);
  if Result <> nil then
    Exit;

  Result := FindFloatingObject(InnerBackgroundFloatingObjects, ARun);
  if Result <> nil then
    Exit;
end;

procedure TdxPage.ClearInvalidatedContent(ARunIndex: TdxRunIndex; APieceTable: TdxCustomPieceTable);
begin
  ClearFloatingObjects(APieceTable, ARunIndex, InnerFloatingObjects);
  ClearFloatingObjects(APieceTable, ARunIndex, InnerForegroundFloatingObjects);
  ClearFloatingObjects(APieceTable, ARunIndex, InnerBackgroundFloatingObjects);
  ClearParagraphFrames(APieceTable, ARunIndex, InnerParagraphFrames);
  ClearFootNotes(ARunIndex);
  ClearComments(APieceTable, ARunIndex);
end;

procedure TdxPage.ClearParagraphFrames;
begin
  FreeAndNil(FInnerParagraphFrames);
end;

procedure TdxPage.ClearParagraphFrames(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex; AItems: TdxParagraphFrameBoxList);
var
  I: Integer;
begin
  if AItems = nil then
    Exit;

  for I := AItems.Count - 1 downto 0 do
    if (APieceTable = nil) or ((AItems[I].PieceTable = APieceTable) and (ARunIndex <= AItems[I].StartPos.RunIndex)) then
      AItems.Delete(I);
end;

function TdxPage.GetActiveFirstArea(APieceTable: TdxCustomPieceTable): TdxPageArea;
begin
  Result := nil;
  if (Header <> nil) and (APieceTable = Header.PieceTable) then
    Exit(Header);
  if (Footer <> nil) and (APieceTable = Footer.PieceTable) then
    Exit(Footer);
  if APieceTable.IsMain then
    Result := Areas.First;
end;

function TdxPage.GetActiveLastArea(APieceTable: TdxCustomPieceTable): TdxPageArea;
begin
  Result := nil;
  if (Header <> nil) and (APieceTable = Header.PieceTable) then
    Exit(Header);
  if (Footer <> nil) and (APieceTable = Footer.PieceTable) then
    Exit(Footer);
  if APieceTable.IsMain then
    Result := Areas.Last;
end;

function TdxPage.GetBackgroundFloatingObjects: TdxFloatingObjectBoxList;
begin
  if FInnerBackgroundFloatingObjects = nil then
    FInnerBackgroundFloatingObjects := TdxFloatingObjectBoxList.Create;
  Result := FInnerBackgroundFloatingObjects;
end;

function TdxPage.GetParagraphFramesProperty: TdxParagraphFrameBoxList;
begin
  if FInnerParagraphFrames = nil then
    FInnerParagraphFrames := TdxParagraphFrameBoxList.Create;
  Result := FInnerParagraphFrames;
end;

function TdxPage.GetFirstFormatterPosition: TdxFormatterPosition;
begin
  if IsEmpty then
    Result := TdxFormatterPosition.MaxValue
  else
    Result := Areas.First.GetFirstFormatterPosition;
end;

function TdxPage.GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
var
  AArea: TdxPageArea;
begin
  if IsEmpty then
    Result := TdxDocumentModelPosition.MaxValue
  else
  begin
    AArea := GetActiveFirstArea(APieceTable);
    if AArea = nil then
      Result := TdxDocumentModelPosition.MaxValue
    else
      Result := AArea.GetFirstPosition(APieceTable);
  end;
end;

function TdxPage.GetFootNotes: TdxFootNotePageAreaList;
begin
  if FInnerFootNotes = nil then
    FInnerFootNotes := TdxFootNotePageAreaList.Create;
  Result := FInnerFootNotes;
end;

function TdxPage.GetFloatingObjects: TdxFloatingObjectBoxList;
begin
  if FInnerFloatingObjects = nil then
    FInnerFloatingObjects := TdxFloatingObjectBoxList.Create;
  Result := FInnerFloatingObjects;
end;

function TdxPage.GetForegroundFloatingObjects: TdxFloatingObjectBoxList;
begin
  if FInnerForegroundFloatingObjects = nil then
    FInnerForegroundFloatingObjects := TdxFloatingObjectBoxList.Create;
  Result := FInnerForegroundFloatingObjects;
end;

function TdxPage.GetDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Page;
end;

function TdxPage.GetHitTestAccuracy: TdxHitTestAccuracy;
begin
  Result := ExactPage;
end;

function TdxPage.IsEmpty: Boolean;
begin
  Result := (Areas.Count <= 0) or Areas[0].IsEmpty;
end;

function TdxPage.GetLastColumn: TdxColumn;
var
  ALastArea: TdxPageArea;
begin
  ALastArea := Areas.Last;
  if ALastArea = nil then
    Result := nil
  else
    Result := ALastArea.Columns.Last;
end;

function TdxPage.GetLastFormatterPosition: TdxFormatterPosition;
begin
  if IsEmpty then
    Result := TdxFormatterPosition.MaxValue
  else
    Result := Areas.Last.GetLastFormatterPosition;
end;

function TdxPage.GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition;
var
  AArea: TdxPageArea;
begin
  if IsEmpty then
    Result := TdxDocumentModelPosition.MaxValue
  else
  begin
    AArea := GetActiveLastArea(APieceTable);
    if AArea = nil then
      Result.Invalidate
    else
      Result := AArea.GetLastPosition(APieceTable);
  end;
end;

function TdxPage.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := (ACalculator as IdxBoxHitTestCalculator).CreatePageHitTestManager(Self);
end;

procedure TdxPage.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportPage(Self);
end;

procedure TdxPage.MoveVertically(ADeltaY: Integer);
begin
  inherited MoveVertically(ADeltaY);
  Areas.MoveVertically(ADeltaY);
  if Header <> nil then
    Header.MoveVertically(ADeltaY);
  if Footer <> nil then
    Footer.MoveVertically(ADeltaY);
  FClientBounds.Offset(0, ADeltaY);
end;

function TdxPage.GetText(ATable: TdxCustomPieceTable): string;
begin
  raise TdxInternalException.Create;
end;

function TdxPage.GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase;
begin
  raise TdxInternalException.Create;
end;

function TdxPage.GetNonBackgroundFloatingObjects: TdxFloatingObjectBoxList;
begin
  Result := TdxFloatingObjectBoxList.Create;
  if InnerFloatingObjects <> nil then
    Result.AddRange(InnerFloatingObjects);

  if ForegroundFloatingObjects <> nil then
    Result.AddRange(ForegroundFloatingObjects);
end;

function TdxPage.GetSortedNonBackgroundFloatingObjects: TdxFloatingObjectBoxList;
begin
  Result := GetNonBackgroundFloatingObjects;
  Result.Sort(TdxFloatingObjectBox.ZOrderComparer);
end;

function TdxPage.GetParagraphFrames: TdxParagraphFrameBoxList;
begin
  Result := TdxParagraphFrameBoxList.Create(False);
  if InnerParagraphFrames <> nil then
    Result.AddRange(InnerParagraphFrames);
end;

function TdxPage.GetSortedParagraphFrames: TdxParagraphFrameBoxList;
begin
  Result := GetParagraphFrames;
  Result.Sort(TdxParagraphFrameBox.IndexComparer);
end;

{ TdxLineNumberBoxCollection }

procedure TdxLineNumberBoxCollection.RegisterSuccessfullItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator; AItem: TdxBox);
begin
//do nothing
end;

procedure TdxLineNumberBoxCollection.RegisterFailedItemHitTest(ACalculator: TdxBoxHitTestCustomCalculator);
begin
//do nothing
end;

{ TdxDetailRow }

constructor TdxDetailRow.Create;
begin
  inherited Create;
  FCharacters := TdxCharacterBoxCollection.Create;
end;

destructor TdxDetailRow.Destroy;
begin
  FreeAndNil(FCharacters);
  inherited Destroy;
end;

{ TZOrderComparer }

function TZOrderComparer.Compare(const ALeft, ARight: TdxBoxBase): Integer;
begin
  Result := CompareInteger(
    TdxFloatingObjectBox(ALeft).GetFloatingObjectRun.FloatingObjectProperties.ZOrder,
    TdxFloatingObjectBox(ARight).GetFloatingObjectRun.FloatingObjectProperties.ZOrder);
end;

{ TdxFloatingObjectBox }

destructor TdxFloatingObjectBox.Destroy;
begin
  DocumentLayout := nil;
  inherited Destroy;
end;

class constructor TdxFloatingObjectBox.Initialize;
begin
  FZOrderComparer := TZOrderComparer.Create;
end;

class destructor TdxFloatingObjectBox.Finalize;
begin
  FreeAndNil(FZOrderComparer);
end;

class function TdxFloatingObjectBox.CreateBox: TdxBox;
begin
  Result := TdxFloatingObjectBox.Create;
end;

function TdxFloatingObjectBox.CalcAscentAndFree(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := Bounds.Height;
end;

function TdxFloatingObjectBox.CalcBaseAscentAndFree(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := CalcAscentAndFree(PieceTable);
end;

function TdxFloatingObjectBox.CalcBaseDescent(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := CalcDescent(PieceTable);
end;

function TdxFloatingObjectBox.CalcDescent(APieceTable: TdxCustomPieceTable): Integer;
begin
  Result := 0;
end;

function TdxFloatingObjectBox.CreateBackwardTransformUnsafe: TdxTransformMatrix;
begin
  Result := CreateTransformUnsafe(-TdxDocumentModel(PieceTable.DocumentModel).GetBoxEffectiveRotationAngleInDegrees(Self), ActualSizeBounds);
end;

function TdxFloatingObjectBox.CreateHitTestManager(const ACalculator: IdxBoxHitTestCustomCalculator): TdxBoxHitTestCustomManager;
begin
  Result := nil;
end;

function TdxFloatingObjectBox.CreateTransformUnsafe: TdxTransformMatrix;
begin
  Result := CreateTransformUnsafe(TdxDocumentModel(PieceTable.DocumentModel).GetBoxEffectiveRotationAngleInDegrees(Self), ActualSizeBounds);
end;

class function TdxFloatingObjectBox.CreateTransformUnsafe(const AAngle: Single;
  const ABounds: TRect): TdxTransformMatrix;
begin
  if IsZero(Single(dxFMod(AAngle, 360))) then
    Exit(nil);
  Result := TdxTransformMatrix.Create;
  Result.Rotate(AAngle, dxPointF(TdxRectangleUtils.CenterPoint(ABounds)));
end;

procedure TdxFloatingObjectBox.ExportTo(const AExporter: IdxSimpleDocumentLayoutExporter);
begin
  (AExporter as IdxDocumentLayoutExporter).ExportFloatingObjectBox(Self);
end;

function TdxFloatingObjectBox.GetActualSizeBounds: TRect;
begin
  Result := FActualSizeBounds;
end;

function TdxFloatingObjectBox.GetFloatingObjectRun: TdxFloatingObjectAnchorRun;
var
  ACurrentRun: TdxRunBase;
begin
  ACurrentRun := GetRun(FPieceTable);
  Result := TdxFloatingObjectAnchorRun(ACurrentRun);
end;

function TdxFloatingObjectBox.GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase;
begin
  Result := inherited GetRun(PieceTable);
end;

function TdxFloatingObjectBox.GetTextBoxContentBounds: TRect;
var
  ASize: TSize;
  ALocation: TPoint;
  ALeftMargin, ATopMargin: Integer;
  ARun: TdxFloatingObjectAnchorRun;
  ATextBoxProperties: TdxTextBoxProperties;
  AContent: TdxTextBoxFloatingObjectContent;
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  ARun := GetFloatingObjectRun;
  AContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  if AContent = nil then
    Exit(ContentBounds);

  AUnitConverter := FPieceTable.DocumentModel.ToDocumentLayoutUnitConverter;
  ALocation := ContentBounds.Location;
  ATextBoxProperties := AContent.TextBoxProperties;
  ALeftMargin := AUnitConverter.ToLayoutUnits(ATextBoxProperties.LeftMargin);
  ATopMargin := AUnitConverter.ToLayoutUnits(ATextBoxProperties.TopMargin);
  ALocation.X := ALocation.X + ALeftMargin;
  ALocation.Y := ALocation.Y + ATopMargin;

  ASize := ContentBounds.Size;
  ASize.cx := ASize.cx - (ALeftMargin + AUnitConverter.ToLayoutUnits(ATextBoxProperties.RightMargin));
  ASize.cy := ASize.cy - (ATopMargin + AUnitConverter.ToLayoutUnits(ATextBoxProperties.BottomMargin));
  ASize.cx := Math.Max(ASize.cx, 4);
  ASize.cy := Math.Max(ASize.cy, 1);
  Result.InitSize(ALocation.X, ALocation.Y, ASize);
end;

function TdxFloatingObjectBox.GetX: Integer;
begin
  Result := FExtendedBounds.Left;
end;

function TdxFloatingObjectBox.GetY: Integer;
begin
  Result := FExtendedBounds.Top;
end;

procedure TdxFloatingObjectBox.IncreaseHeight(ADelta: Integer);
begin
  Bounds := TRect.Create(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom + ADelta);
  FActualSizeBounds.Height := FActualSizeBounds.Height + ADelta;
  FExtendedBounds.Height := FExtendedBounds.Height + ADelta;
  FContentBounds.Height := FContentBounds.Height + ADelta;
end;

function TdxFloatingObjectBox.IsLineBreak: Boolean;
begin
  Result := False;
end;

function TdxFloatingObjectBox.IsNotWhiteSpaceBox: Boolean;
begin
  Result := True;
end;

function TdxFloatingObjectBox.IsVisible: Boolean;
begin
  Result := True;
end;

procedure TdxFloatingObjectBox.MoveVertically(ADeltaY: Integer);
var
  AProperties: TdxFloatingObjectProperties;
  AVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  AProperties := GetFloatingObjectRun.FloatingObjectProperties;
  AVerticalPositionAlignment := AProperties.VerticalPositionAlignment;
  if AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.None then
    Exit;

  MoveVerticallyCore(ADeltaY);
end;

procedure TdxFloatingObjectBox.MoveVerticallyCore(ADeltaY: Integer);
begin
  inherited MoveVertically(ADeltaY);
  FExtendedBounds.Y := FExtendedBounds.Y + ADeltaY;
  FContentBounds.Y := FContentBounds.Y + ADeltaY;
  FActualSizeBounds.Y := FActualSizeBounds.Y + ADeltaY;
  MoveLayoutVertically(ADeltaY);
end;

procedure TdxFloatingObjectBox.MoveLayoutVertically(ADeltaY: Integer);
begin
  if (FDocumentLayout <> nil) and (FDocumentLayout.Pages.Count > 0) then
    FDocumentLayout.Pages[0].MoveVertically(ADeltaY);
end;

procedure TdxFloatingObjectBox.SetActualSizeBounds(const ABounds: TRect);
begin
  FActualSizeBounds := ABounds;
end;

procedure TdxFloatingObjectBox.SetDocumentLayout(const Value: TdxDocumentLayout);
begin
  if FDocumentLayout <> Value then
  begin
    FreeAndNil(FDocumentLayout);
    FDocumentLayout := Value;
  end;
end;

procedure TdxFloatingObjectBox.SetX(const Value: Integer);
begin
  FExtendedBounds.MoveToLeft(Value);
end;

procedure TdxFloatingObjectBox.SetY(const Value: Integer);
begin
  FExtendedBounds.MoveToTop(Value);
end;

function TdxFloatingObjectBox.TransformPointBackward(APoint: TPoint): TPoint;
var
  ATransform: TdxTransformMatrix;
begin
  ATransform := CreateBackwardTransformUnsafe;
  try
    if ATransform = nil then
      Result := APoint
    else
      Result := ATransform.TransformPoint(APoint);
  finally
    ATransform.Free;
  end;
end;

{ TdxFloatingObjectBoxList }

function TdxFloatingObjectBoxList.GetItem(Index: Integer): TdxFloatingObjectBox;
begin
  Result := TdxFloatingObjectBox(inherited Items[Index]);
end;

{ TdxTableCellVerticalAnchor }

constructor TdxTableCellVerticalAnchor.Create(AVerticalPosition, ABottomTextIndent: TdxLayoutUnit;
  ACellBorders: TdxHorizontalCellBordersInfoList);
begin
  inherited Create;
  FVerticalPosition := AVerticalPosition;
  FBottomTextIndent := ABottomTextIndent;
  FCellBorders := ACellBorders;
  FCorners := TdxObjectList<TdxCornerViewInfoBase>.Create;
end;

destructor TdxTableCellVerticalAnchor.Destroy;
begin
  FCellBorders.Free;
  FCorners.Free;
  inherited Destroy;
end;

function TdxTableCellVerticalAnchor.CloneWithNewVerticalPosition(
  ANewVerticalPostion: TdxLayoutUnit): TdxTableCellVerticalAnchor;
var
  ACellBorders: TdxHorizontalCellBordersInfoList;
begin
  ACellBorders := CellBorders.Clone;
  Result := TdxTableCellVerticalAnchor.Create(ANewVerticalPostion, BottomTextIndent, ACellBorders);
  Result.TopTextIndent := TopTextIndent;
  Assert(Corners.Count = 0);
end;

function TdxTableCellVerticalAnchor.GetBottomTextIndent: TdxLayoutUnit;
begin
  Result := FBottomTextIndent;
end;

function TdxTableCellVerticalAnchor.GetVerticalPosition: TdxLayoutUnit;
begin
  Result := FVerticalPosition;
end;

procedure TdxTableCellVerticalAnchor.MoveVertically(ADeltaY: TdxLayoutUnit);
begin
  Inc(FVerticalPosition, ADeltaY);
end;

{ TdxTableCellVerticalAnchorYComparable }

constructor TdxTableCellVerticalAnchorYComparable.Create(APos: Integer);
begin
  inherited Create;
  FPos := APos;
end;

function TdxTableCellVerticalAnchorYComparable.CompareTo(const AAnchor: TdxTableCellVerticalAnchor): Integer;
begin
  if FPos < AAnchor.VerticalPosition then
    Exit(1)
  else
    if FPos > AAnchor.VerticalPosition then
      Exit(-1)
    else
      Exit(0);
end;

{ TdxTableRowAnchorComparable }

constructor TdxTableRowAnchorComparable.Create(APos: Integer; ALastRow: TdxTableRow);
begin
  inherited Create;
  FPos := APos;
  FLastRow := ALastRow;
end;

function TdxTableRowAnchorComparable.CompareTo(const Value: TdxTableRowViewInfoBase): Integer;
var
  AActualRowBottom: Integer;
  ARow: TdxTableRowViewInfoBase absolute Value;
begin
  if FPos < ARow.TopAnchor.VerticalPosition then
    Exit(1);
  AActualRowBottom := ARow.BottomAnchor.VerticalPosition;
  if ARow.Row = FLastRow then
    Inc(AActualRowBottom, ARow.BottomAnchor.BottomTextIndent);
  if FPos > AActualRowBottom then
    Result := -1
  else
    Result := 0;
end;

{ TdxTableCellAnchorComparable }

constructor TdxTableCellAnchorComparable.Create(APos: Integer);
begin
  inherited Create;
  FPos := APos;
end;

function TdxTableCellAnchorComparable.CompareTo(const Value: TdxTableCellViewInfo): Integer;
begin
  if FPos < Value.Left then
    Result := 1
  else
    if FPos > Value.Left + Value.Width then
      Result := -1
    else
      Result := 0;
end;

end.
