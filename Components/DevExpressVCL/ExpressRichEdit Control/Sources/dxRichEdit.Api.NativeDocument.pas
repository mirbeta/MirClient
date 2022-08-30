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

unit dxRichEdit.Api.NativeDocument;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, Graphics, RegularExpressions,
  dxCore, dxCoreClasses, dxCoreGraphics, dxGDIPlusClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Api.NativeDocumentBase,
  dxGenerics,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.InnerControl,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.Paragraphs,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.PieceTable;

type
  TdxNativeSubDocument = class;
  TdxNativeDocument = class;

  { TdxParagraphRange }

  TdxParagraphRange = record
  strict private
    FStart: TdxParagraphIndex;
    FEnd: TdxParagraphIndex;
    function GetLength: Integer;
  public
    constructor Create(AStart, AEnd: TdxParagraphIndex);

    property Start: TdxParagraphIndex read FStart;
    property &End: TdxParagraphIndex read FEnd;
    property Length: Integer read GetLength;
  end;

  { TdxDocumentContentInserter }

  TdxDocumentContentInserter = class abstract
  strict private
    FAppend: Boolean;
    FResetMerging: Boolean;
  public
    constructor Create(AResetMerging: Boolean = False);
    function CanInsertContent: Boolean; virtual; abstract;
    function InsertContent(APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition): Boolean;
    procedure CopyFormatting(APieceTable: TdxPieceTable);
    function InsertContentCore(APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition): Boolean; virtual; abstract;

    property Append: Boolean read FAppend write FAppend;
  end;

  { TdxNativeDocumentPosition }

  TdxNativeDocumentPosition = class(TInterfacedObject, IdxRichEditDocumentPosition)
  strict private
    FDocument: IdxRichEditSubDocument;
    FPosition: TdxDocumentModelPosition;
    FAnchor: TdxDocumentModelPositionAnchor;
    function GetLogPosition: Integer;
  protected
    function CompareTo(const APos: IdxRichEditDocumentPosition): Integer;
    function ToInt: Integer;

    property LogPosition: Integer read GetLogPosition;
  public
    constructor Create(const ADocument: IdxRichEditSubDocument; const APos: TdxDocumentModelPosition);
    destructor Destroy; override;

    function Equals(AObj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function ToString: string; override;

    property Document: IdxRichEditSubDocument read FDocument;
    property Position: TdxDocumentModelPosition read FPosition;
  end;

  { TdxNativeDocumentRange }

  TdxNativeDocumentRange = class(TInterfacedObject, IdxRichEditDocumentRange)
  strict private
    FStart: IdxRichEditDocumentPosition;
    FEnd: IdxRichEditDocumentPosition;
    function GetEnd: IdxRichEditDocumentPosition;
    function GetStart: IdxRichEditDocumentPosition;
    function GetNormalizedStart: IdxRichEditDocumentPosition;
    function GetNormalizedEnd: IdxRichEditDocumentPosition;
    function GetLength: Integer;
  public
    constructor Create(const AStart, AEnd: IdxRichEditDocumentPosition); overload;
    constructor Create(const ADocument: IdxRichEditSubDocument;
      const AStart, AEnd: TdxDocumentModelPosition); overload;
    function Contains(const APos: IdxRichEditDocumentPosition): Boolean;

    property Start: IdxRichEditDocumentPosition read FStart;
    property &End: IdxRichEditDocumentPosition read FEnd;
    property NormalizedStart: IdxRichEditDocumentPosition read GetNormalizedStart;
    property NormalizedEnd: IdxRichEditDocumentPosition read GetNormalizedEnd;
    property Length: Integer read GetLength;
  end;

  { TdxNativeFieldCollection }

  TdxNativeFieldCollection = class(TdxIUnknownList<IdxRichEditField>,
    IdxRichEditFieldCollection,
    IdxRichEditReadOnlyFieldCollection)
  strict private
    FDocument: TdxNativeSubDocument;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IdxRichEditField;
    function GetPieceTable: TdxPieceTable;
  protected
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(ADocument: TdxNativeSubDocument);

    procedure Update;

    function Add(const Value: IdxRichEditField): Integer; overload;
    function Add(const ACodeRange: IdxRichEditDocumentRange): IdxRichEditField; overload;
    function Add(const AStart: IdxRichEditDocumentPosition; const ACode: string): IdxRichEditField; overload;
    function CreateField(const ACodeRange: IdxRichEditDocumentRange): IdxRichEditField; overload;
    function CreateField(const AStart: IdxRichEditDocumentPosition; const ACode: string): IdxRichEditField; overload;
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyFieldCollection;
  end;

  { TdxNativeSubDocument }

  TdxNativeSubDocument = class(TdxNativeSubDocumentBase, IdxRichEditSubDocument)
  strict private
    FUnitConverter: TdxUnitConverter;
    FPointsConverter: TdxUnitConverter;
    FFields: IdxRichEditFieldCollection;
    FParagraphs: IdxRichEditParagraphCollection;
    FBookmarks: IdxRichEditBookmarkCollection;
    FHyperlinks: IdxRichEditHyperlinkCollection;
    FTables: IdxRichEditTableCollection;
    FShapes: IdxRichEditShapeCollection;
    FImages: IdxRichEditDocumentImageCollection;
    function GetBookmarks: IdxRichEditBookmarkCollection;
    function GetEndPosition: IdxRichEditDocumentPosition;
    function GetFields: IdxRichEditFieldCollection;
    function GetHyperlinks: IdxRichEditHyperlinkCollection;
    function GetImages: IdxRichEditDocumentImageCollection; overload;
    function GetLength: Integer;
    function GetPageBackColor: TdxAlphaColor;
    function GetParagraphs: IdxRichEditParagraphCollection; overload;
    function GetRange: IdxRichEditDocumentRange;
    function GetShapes: IdxRichEditShapeCollection; overload;
    function GetShowPageBackground: Boolean;
    function GetTables: IdxRichEditTableCollection;
    function GetUnit: TdxRichEditDocumentUnit;
    procedure SetPageBackColor(const Value: TdxAlphaColor);
    procedure SetShowPageBackground(const Value: Boolean);
    procedure SetUnit(const Value: TdxRichEditDocumentUnit);
  protected
    function GetMainDocument: TdxNativeDocument; virtual;

    procedure Invalidate;
    procedure CreateApiObjects; override;
    procedure DoInitialize; override;
    procedure DoFinalize; override;
    procedure DestroyApiObjects; override;

    procedure OnUnitsChanged;
    procedure PopulateParagraphs;
    procedure RegisterParagraph(const AParagraph: TdxCustomParagraph);
    procedure PopulateFields;
    procedure RegisterField(const AField: TdxField);
    procedure PopulateBookmarks;


    procedure PopulateHyperlinks;
    procedure PopulateTables;
    procedure DoSubscribeInternalAPIEvents; override;
    procedure DoUnsubscribeInternalAPIEvents; override;
    procedure OnParagraphInserted(ASender: TObject; E: TdxParagraphEventArgs);
    procedure OnParagraphRemoved(ASender: TObject; E: TdxParagraphEventArgs);
    procedure OnParagraphMerged(ASender: TObject; E: TdxParagraphEventArgs);
    procedure OnParagraphRemovedCore(E: TdxParagraphEventArgs);
    procedure OnFieldInserted(ASender: TObject; E: TdxFieldEventArgs);
    procedure OnFieldRemoved(ASender: TObject; E: TdxFieldEventArgs);
    procedure OnHyperlinkInfoInserted(ASender: TObject; E: TdxHyperlinkInfoEventArgs);
    procedure OnHyperlinkInfoDeleted(ASender: TObject; E: TdxHyperlinkInfoEventArgs);
    procedure OnTablesCollectionChanged(const AItem: TObject; Action: TListNotification);
    function CreateNativeRange(const AStart, AEnd: TdxDocumentModelPosition): TdxNativeDocumentRange;
    function CreateZeroLengthRange(ALogPosition: TdxDocumentLogPosition): IdxRichEditDocumentRange;
    function CreateNativePosition(const APos: TdxDocumentModelPosition): TdxNativeDocumentPosition;
    function GetSelectionRangeCollection(const ARange: IdxRichEditDocumentRange): TdxSelectionRangeCollection;
    function CreateCopySelectionManagerForGetContent: TdxCopySelectionManager;
    procedure ThrowDocumentPositionPieceTableMismatch;
    function AppendDocumentContentCore(AInserter: TdxDocumentContentInserter): IdxRichEditDocumentRange;
  public
    constructor Create(APieceTable: TdxPieceTable; AServer: TdxInnerRichEditDocumentServer); override;

    procedure UnitsChanged; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    function InsertParagraph(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph; overload;
    function InsertParagraph(const APos: IdxRichEditDocumentPosition; const AInsertOptions: TdxRichEditInsertOptions): IdxRichEditParagraph; overload;
    function AppendParagraph: IdxRichEditParagraph;
    function InsertContentCore(const APos: IdxRichEditDocumentPosition; AInserter: TdxDocumentContentInserter): IdxRichEditDocumentRange;

    function AppendText(const AText: string): IdxRichEditDocumentRange;
    function InsertText(const APos: IdxRichEditDocumentPosition; const AText: string): IdxRichEditDocumentRange;

    function AppendSingleLineText(const AText: string): IdxRichEditDocumentRange;
    function InsertSingleLineText(const APos: IdxRichEditDocumentPosition;
      const AText: string): IdxRichEditDocumentRange;

    function AppendRtfText(const ARtfText: string; AInsertOptions: TdxRichEditInsertOptions = TdxRichEditInsertOptions.MatchDestinationFormatting): IdxRichEditDocumentRange;
    function InsertRtfText(const APos: IdxRichEditDocumentPosition; const ARtfText: string; AInsertOptions: TdxRichEditInsertOptions = TdxRichEditInsertOptions.MatchDestinationFormatting): IdxRichEditDocumentRange;


    function AppendImage(AImage: TGraphic): IdxRichEditDocumentImage;
    function InsertImage(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditDocumentImage;

    function InsertPicture(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditShape; virtual;
    function InsertTextBox(const APos: IdxRichEditDocumentPosition): IdxRichEditShape; virtual;

    function InsertTable(const APos: IdxRichEditDocumentPosition; ARowCount, AColumnCount: Integer;
      AAutoFitBehavior: TdxRichEditAutoFitBehaviorType = TdxRichEditAutoFitBehaviorType.AutoFitToContents;
      AFixedColumnWidths: Integer = MinInt): IdxRichEditTable;

    function CreateRange(AStart, ALength: Integer): IdxRichEditDocumentRange; overload;
    function CreateRange(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditDocumentRange; overload;
    function CreatePosition(AStart: Integer): IdxRichEditDocumentPosition;

    procedure Delete(const ARange: IdxRichEditDocumentRange);
    procedure SelectAll;

    function CalculateParagraphsRange(const ARange: IdxRichEditDocumentRange): TdxParagraphRange;
    function BeginUpdateParagraphs(const ARange: IdxRichEditDocumentRange): IdxRichEditParagraphProperties;
    procedure EndUpdateParagraphs(const AProperties: IdxRichEditParagraphProperties);
    function BeginUpdateCharacters(AStart, ALength: Integer): IdxRichEditCharacterProperties; overload;
    function BeginUpdateCharacters(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditCharacterProperties; overload;
    function BeginUpdateCharacters(const ARange: IdxRichEditDocumentRange): IdxRichEditCharacterProperties; overload;
    function BeginUpdateCharactersCore(AStart, AEnd: Integer): IdxRichEditCharacterProperties;
    procedure EndUpdateCharacters(const AProperties: IdxRichEditCharacterProperties);

    procedure ApplyNewRangePermissions(const APermissions: IdxRichEditRangePermissionCollection);
    procedure ApplyNewRangePermission(const ARangePermission: IdxRichEditRangePermission);
    function CreateRangePermission(const ACollection: IdxRichEditRangePermissionCollection;
      const ARangePermission: TdxRangePermission): IdxRichEditRangePermission;
    function BeginUpdateRangePermissions: IdxRichEditRangePermissionCollection;
    procedure CancelUpdateRangePermissions(const APermissions: IdxRichEditRangePermissionCollection);
    procedure EndUpdateRangePermissions(const APermissions: IdxRichEditRangePermissionCollection);


    function CreateBookmark(const AStart: IdxRichEditDocumentPosition; ALength: Integer; const AName: string): IdxRichEditBookmark; overload;
    function CreateBookmark(const ARange: IdxRichEditDocumentRange; const AName: string): IdxRichEditBookmark; overload;
    procedure SelectBookmark(const ABookmark: IdxRichEditBookmark); overload;
    procedure RemoveBookmark(const ABookmark: IdxRichEditBookmark); overload;

    function CreateHyperlink(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditHyperlink; overload;
    function CreateHyperlink(const ARange: IdxRichEditDocumentRange): IdxRichEditHyperlink; overload;
    procedure RemoveHyperlink(const AHyperlink: IdxRichEditHyperlink);

    function GetParagraph(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph;
    function GetParagraphs(const ARange: IdxRichEditDocumentRange): IdxRichEditParagraphCollection; overload;

    function AppendDocumentContent(const AFileName: string;
      AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(const AFileName: string;
      AFormat: TdxRichEditDocumentFormat; const ASourceUri: string): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(const AFileName: string;
      AFormat: TdxRichEditDocumentFormat; const ASourceUri: string;
      AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(AStream: TStream;
      AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(AStream: TStream;
      AFormat: TdxRichEditDocumentFormat; const ASourceUri: string): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(AStream: TStream;
      AFormat: TdxRichEditDocumentFormat; const ASourceUri: string;
      AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(const ARange: IdxRichEditDocumentRange): IdxRichEditDocumentRange; overload;
    function AppendDocumentContent(const ARange: IdxRichEditDocumentRange;
      AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const AFileName: string; AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const AFileName: string; AFormat: TdxRichEditDocumentFormat;
      AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const AFileName: string; AFormat: TdxRichEditDocumentFormat;
      const ASourceUri: string): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const AFileName: string; AFormat: TdxRichEditDocumentFormat;
      const ASourceUri: string; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      AStream: TStream; AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      AStream: TStream; AFormat: TdxRichEditDocumentFormat;
      AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      AStream: TStream; AFormat: TdxRichEditDocumentFormat;
      const ASourceUri: string): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      AStream: TStream; AFormat: TdxRichEditDocumentFormat;
      const ASourceUri: string; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const ARange: IdxRichEditDocumentRange): IdxRichEditDocumentRange; overload;
    function InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
      const ARange: IdxRichEditDocumentRange; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange; overload;

    function GetImages(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyDocumentImageCollection; overload;
    function GetShapes(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyShapeCollection; overload;

    function GetOpenXmlBytes(const ARange: IdxRichEditDocumentRange): TBytes;
    function GetRtfText(const ARange: IdxRichEditDocumentRange): string;
    function GetText(const ARange: IdxRichEditDocumentRange): string; overload;
    function GetText(const ARange: IdxRichEditDocumentRange; const AFragmentOptions: TdxRichEditTextFragmentOptions): string; overload;

    function Replace(const ARange: IdxRichEditDocumentRange; const AText: string): Integer;
    function FindAll(const ATextToFind: string; AOptions: TdxRichEditSearchOptions = [];
      const ARange: IdxRichEditDocumentRange = nil): TArray<IdxRichEditDocumentRange>; overload;
    function FindAll(const ARegex: TRegEx; const ARange: IdxRichEditDocumentRange = nil): TArray<IdxRichEditDocumentRange>; overload;
    function ReplaceAll(const ATextToFind, AReplaceWith: string;
      AOptions: TdxRichEditSearchOptions = []; const ARange: IdxRichEditDocumentRange = nil): Integer; overload;
    function ReplaceAll(const ARegex: TRegEx; const AReplaceWith: string;
      const ARange: IdxRichEditDocumentRange = nil): Integer; overload;
    function StartSearch(const ATextToFind: string; AOptions: TdxRichEditSearchOptions = [];
      ADirection: TdxRichEditSearchDirection = TdxRichEditSearchDirection.Forward;
      const ARange: IdxRichEditDocumentRange = nil): IdxRichEditSearchResult; overload;
    function StartSearch(const ARegex: TRegEx; const ARange: IdxRichEditDocumentRange = nil): IdxRichEditRegexSearchResult; overload;

    procedure AddParagraphsToList(const ARange: IdxRichEditDocumentRange; const AList: IdxRichEditNumberingList; ALevelIndex: Integer);
    procedure AddParagraphToList(const AParagraph: IdxRichEditParagraph;
      const AList: IdxRichEditNumberingList; ALevelIndex: Integer); overload;
    procedure AddParagraphToList(const AParagraph: IdxRichEditParagraph;
      ANumberingListIndex, ALevelIndex: Integer); overload;
    procedure RemoveNumberingFromParagraph(const AParagraph: IdxRichEditParagraph);
    procedure RemoveNumberingFromParagraphs(const ARange: IdxRichEditDocumentRange);


    function GetSubDocumentType: TdxRichEditSubDocumentType;

    property &Unit: TdxRichEditDocumentUnit read GetUnit write SetUnit;
    property Paragraphs: IdxRichEditParagraphCollection read GetParagraphs;
    property Fields: IdxRichEditFieldCollection read GetFields;
    property Bookmarks: IdxRichEditBookmarkCollection read GetBookmarks;
    property Hyperlinks: IdxRichEditHyperlinkCollection read GetHyperlinks;
    property Tables: IdxRichEditTableCollection read GetTables;
    property Shapes: IdxRichEditShapeCollection read GetShapes;
    property Range: IdxRichEditDocumentRange read GetRange;
    property Length: Integer read GetLength;
  public
    procedure CheckValid;
    procedure CheckDocumentPosition(const ADocumentPosition: IdxRichEditDocumentPosition);
    procedure CheckDocumentRange(const ADocumentRange: IdxRichEditDocumentRange);
    function CreatePositionCore(APos: TdxDocumentLogPosition): IdxRichEditDocumentPosition;
    procedure SetSelectionCore(AStart, AEnd: TdxDocumentLogPosition; AForceUpdateTableSelection: Boolean = True);

    function NormalizeLogPosition(APos: TdxDocumentLogPosition): TdxDocumentLogPosition; overload;
    function NormalizeLogPosition(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition): TdxDocumentLogPosition; overload;

    function GetWidthUnitFixedValue(AUnitValue: TdxWidthUnit): Single;
    procedure SetWidthUnitFixedValue(AUnitValue: TdxWidthUnit; AValue: Single);
    function GetWidthValue(AUnitValue: TdxWidthUnit): Single;
    procedure SetWidthValue(AUnitValue: TdxWidthUnit; AValue: Single);

    function GetTableCellStyle(ACell: TdxTableCell): IdxRichEditTableCellStyle;
    function GetInnerTableCellStyle(const AStyle: IdxRichEditTableCellStyle): TdxTableCellStyle;

    function ModelUnitsToUnits(Value: Integer): Single; overload;
    function ModelUnitsToUnitsF(Value: Single): Single; overload;
    function ModelUnitsToUnits(const Value: TdxNullableInteger): TdxNullableSingle; overload;
    function ModelUnitsToUnitsF(const Value: TdxNullableSingle): TdxNullableSingle; overload;
    function UnitsToModelUnits(Value: Single): Integer;
    function UnitsToModelUnitsF(Value: Single): Single;

    function ModelUnitsToPointsF(const Value: Single): Single;
    function PointsToModelUnitsF(Value: Single): Single;

    property EndPosition: IdxRichEditDocumentPosition read GetEndPosition;
    property MainDocument: TdxNativeDocument read GetMainDocument;
    property UnitConverter: TdxUnitConverter read FUnitConverter;
  end;

  { TdxNativeSectionCollection }

  TdxNativeSectionCollection = class(TdxIUnknownList<IdxRichEditSection>, IdxRichEditSectionCollection)
  private
    function GetCount: Integer;
  end;

  { TdxNativeSelectionCollection }

  TdxNativeSelectionCollection = class(TInterfacedObject, IdxRichEditSelectionCollection)
  strict private const
  {$REGION 'private const'}
    DocumentModelChangeActions = [TdxDocumentModelChangeAction.ResetSelectionLayout, TdxDocumentModelChangeAction.RaiseSelectionChanged, TdxDocumentModelChangeAction.Redraw];
  {$ENDREGION}
  strict private type
  {$REGION 'private type'}
    TSelectionRangeType = (
      Single,
      Start,
      Middle,
      &End);
  {$ENDREGION}
  strict private
    FDocument: TdxNativeDocument;

    function AreSelectionItemsIntersected(const AItem1, AItem2: IdxRichEditDocumentRange): Boolean;
    procedure CalcColumnsAndCellsCount(ARow: TdxTableRow; AStartIndexInRow, AEndIndexInRow: Integer;
      out AColumnCount, ACellCount: Integer);
    function CloneCollectionByValue(ACollection: TdxSelectionItemList): TdxSelectionItemList;
    procedure CorrectEndLogPosByEndOfDocument(ARanges: TList<IdxRichEditDocumentRange>);
    function FillGroup(ASortedRanges: TList<IdxRichEditDocumentRange>;
      APrevRangeIndex: Integer; APrevStartCell: TdxTableCell; ASelectedCellCount: Integer;
      ASelectedColumnCount: Integer; ACurrentGroup: TList<IdxRichEditDocumentRange>): Integer;
    procedure InternalAdd(const ARange: IdxRichEditDocumentRange; ASelElementType: TSelectionRangeType);
    function IsCellPartiallySelected(ACell: TdxTableCell; APos: TdxDocumentLogPosition): Boolean;
    function IsEndOfDocument(APos: TdxDocumentLogPosition): Boolean;
    function IsLogPositionSelectAnotherCell(ACell: TdxTableCell; APos: TdxDocumentLogPosition): Boolean;
    function IsParent(AParent, AChild: TdxTableCell): Boolean;
    function GetCell(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition): TdxTableCell;
    function GetContainerTable(ACell1, ACell2: TdxTableCell): TdxTable;
    function GetFirstNormalizedCellInRow(ARow: TdxTableRow): TdxTableCell;
    function GetGroupedSortedRanges(ARanges: TList<IdxRichEditDocumentRange>): TdxObjectList<TList<IdxRichEditDocumentRange>>;
    function GetInnerSelection: TdxSelection;
    function GetLastSelectedCell(AStartCell, AEndCell: TdxTableCell; AEndLogPosition: TdxDocumentLogPosition): TdxTableCell;
    function GetLastNormalizedCellInRow(ARow: TdxTableRow): TdxTableCell;
    function GetParentCellWithNestedLevel(ACell: TdxTableCell; ANestedLevel: Integer): TdxTableCell;
    function GetPreviousCellInRow(ACell: TdxTableCell): TdxTableCell;
    function MergeNeighbourhoodRanges(ASortedRanges: TList<IdxRichEditDocumentRange>): TList<IdxRichEditDocumentRange>;
    procedure NormalizeCells(ACell1, ACell2: TdxTableCell; out ANormalizedCell1, ANormalizedCell2: TdxTableCell);
    procedure RemoveRangesWithZeroWidth(ARanges: TList<IdxRichEditDocumentRange>);
    function SameRowCell(ACell1, ACell2: TdxTableCell): Boolean;
    function ShouldSkipValidation(AStart, AEnd, ALastSelected: TdxTableCell): Boolean;
    function SortRanges(AItems: TList<IdxRichEditDocumentRange>): TList<IdxRichEditDocumentRange>;
    function SplitRanges(ASortedRanges: TList<IdxRichEditDocumentRange>): TList<IdxRichEditDocumentRange>;
    function SplitRangesByRowWithChildTables(ASortedRanges: TList<IdxRichEditDocumentRange>): TList<IdxRichEditDocumentRange>;
    function SplitRangesByRowSameTable(ASortedRanges: TList<IdxRichEditDocumentRange>): TList<IdxRichEditDocumentRange>;
    procedure SplitRows(ARange: TdxNativeDocumentRange; ATable: TdxTable; AStartRowIndex, AEndRowIndex: Integer; ARanges: TList<IdxRichEditDocumentRange>);
    procedure UnselectCore(ACloneCollection: TdxSelectionItemList; ARange: TdxNativeDocumentRange);
    procedure ValidateSelection(const ARange: IdxRichEditDocumentRange; ASelElementType: TSelectionRangeType; AIsRemoved: Boolean = False);
    procedure ValidateSelections(AGroupedItems: TdxObjectList<TList<IdxRichEditDocumentRange>>; AIsRemoved: Boolean = False);
    procedure ValidateSelectionCells(const ARange: IdxRichEditDocumentRange; ASelElementType: TSelectionRangeType);
    procedure ValidateCellsWithinSameTable(AFirstSelectedCell, ALastSelectedCell: TdxTableCell;
      ASelStartLogPosition, ASelEndLogPosition: TdxDocumentLogPosition; ASelElementType: TSelectionRangeType);
    procedure ValidateCellsWithinDifferentTables(AFirstSelectedCell, ALastSelectedCell: TdxTableCell;
      ASelStartLogPosition, ASelEndLogPosition: TdxDocumentLogPosition);
    procedure ValidateTableCellTextBeforeTable(ASelectedCell: TdxTableCell;
      ASelEndLogPosition: TdxDocumentLogPosition; ACellForNormalize: TdxTableCell = nil);
    procedure ValidateTableCellTextAfterTable(ASelectedCell: TdxTableCell;
      ASelStartLogPosition: TdxDocumentLogPosition);

    function GetCount: Integer;
    function GetItem(Index: Integer): IdxRichEditDocumentRange;
  protected
    function CreateDocumentRange(const AStart, AEnd: TdxDocumentLogPosition): IdxRichEditDocumentRange; overload;
    function CreateDocumentRange(const AItem: TdxSelectionItem): IdxRichEditDocumentRange; overload;
    function CreateSelectionItem(AStart, AEnd: TdxDocumentLogPosition): TdxSelectionItem; overload;
    function CreateSelectionItem(const ARange: IdxRichEditDocumentRange): TdxSelectionItem; overload;

    property InnerSelection: TdxSelection read GetInnerSelection;
  public
    constructor Create(ADocument: TdxNativeDocument);

    function Add(const ARange: IdxRichEditDocumentRange): Integer; overload;
    procedure Add(const ARanges: TList<IdxRichEditDocumentRange>); overload;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Unselect(const ARange: IdxRichEditDocumentRange);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: IdxRichEditDocumentRange read GetItem; default;
  end;

  { TdxNativeDocument }

  TdxNativeDocument = class(TdxNativeSubDocument, IdxRichEditDocument)
  strict private
    FAbstractNumberingLists: IdxRichEditAbstractNumberingListCollection;
    FCharacterStyles: IdxRichEditCharacterStyleCollection;
    FDefaultCharacterProperties: IdxRichEditCharacterPropertiesBase;
    FDefaultParagraphProperties: IdxRichEditParagraphPropertiesBase;
    FDefaultTableProperties: IdxRichEditTablePropertiesBase;
    FNumberingLists: IdxRichEditNumberingListCollection;
    FParagraphStyles: IdxRichEditParagraphStyleCollection;
    FSections: IdxRichEditSectionCollection;
    FSelections: IdxRichEditSelectionCollection;
    FTableStyles: IdxRichEditTableStyleCollection;
    function GetAbstractNumberingLists: IdxRichEditAbstractNumberingListCollection;
    function GetCharacterStyles: IdxRichEditCharacterStyleCollection;
    function GetDefaultCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetDefaultParagraphProperties: IdxRichEditParagraphPropertiesBase;
    function GetDefaultTableProperties: IdxRichEditTablePropertiesBase;
    function GetNumberingLists: IdxRichEditNumberingListCollection;
    function GetParagraphStyles: IdxRichEditParagraphStyleCollection;
    function GetSections: IdxRichEditSectionCollection;
    function GetSelections: IdxRichEditSelectionCollection;
    function GetTableStyles: IdxRichEditTableStyleCollection;
    function GetVariables: IdxRichEditDocumentVariableCollection;

    function GetIsDocumentProtected: Boolean;

    function GetOpenXmlBytes: TBytes; overload;
    function GetRtfText: string; overload;
    function GetText: string; overload;
    procedure SetOpenXmlBytes(const Value: TBytes); overload;
    procedure SetRtfText(const Value: string); overload;
    procedure SetText(const Value: string); overload;

    function GetCaretPosition: IdxRichEditDocumentPosition;
    function GetIsEmpty: Boolean;
    function GetSelection: IdxRichEditDocumentRange;
    procedure SetCaretPosition(const Value: IdxRichEditDocumentPosition);
    procedure SetSelection(const Value: IdxRichEditDocumentRange);
  protected
    procedure CreateApiObjects; override;
    procedure DoInitialize; override;
    procedure DoFinalize; override;
    procedure DestroyApiObjects; override;

    procedure PopulateSections; virtual;
    procedure RegisterSection(ASection: TdxSection); virtual;
    function CreateNativeSection(ASection: TdxSection): IdxRichEditSection; virtual;
    procedure DoSubscribeInternalAPIEvents; override;
    procedure DoUnsubscribeInternalAPIEvents; override;
    procedure SubscribeInternalAPIEventsCore; virtual;
    procedure UnsubscribeInternalAPIEventsCore; virtual;
    procedure OnDocumentCleared(ASender: TObject; E: TdxEventArgs);
    procedure OnDocumentReplaced(ASender: TObject);
    procedure OnSectionInserted(ASender: TObject; E: TdxSectionEventArgs);
    procedure OnSectionRemoved(ASender: TObject; E: TdxSectionEventArgs);
    procedure LoadDocumentCore(AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ASourceUri: string); virtual;
    class procedure SaveDocument(ADocumentModel: TdxDocumentModel; const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload; static;
    function CreateNativeSubDocument(APieceTable: TdxPieceTable;
      AServer: TdxInnerRichEditDocumentServer): TdxNativeSubDocument; virtual;
    function ShouldCreateNewSubDocument: Boolean;
  public
    procedure CreateNewDocument;
    procedure LoadDocument(AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;
    procedure LoadDocument(AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ASourceUri: string); overload;
    procedure LoadDocument(const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure LoadDocument(const AFileName: string; AFormat: TdxRichEditDocumentFormat; const ASourceUri: string); overload;
    procedure SaveDocument(AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;
    procedure SaveDocumentCore(AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ATargetUri: string); overload;
    procedure SaveDocument(const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;

    function InsertSection(const APos: IdxRichEditDocumentPosition): IdxRichEditSection;
    function AppendSection: IdxRichEditSection;
    function InsertSectionCore(const APos: IdxRichEditDocumentPosition; AAppend: Boolean): IdxRichEditSection;
    function GetSection(const APos: IdxRichEditDocumentPosition): IdxRichEditSection;

    procedure ChangeActiveDocument(const ADocument: IdxRichEditSubDocument);
    procedure ChangeActivePieceTable(const AControl: IdxRichEditControl; APieceTable: TdxPieceTable);
    function GetActiveSubDocument: IdxRichEditSubDocument;

    procedure Copy(const ARange: IdxRichEditDocumentRange); overload;
    procedure Copy; overload;
    procedure Cut(const ARange: IdxRichEditDocumentRange); overload;
    procedure Cut; overload;
    procedure Paste; overload;
    procedure Paste(const AFormat: TdxRichEditDocumentFormat); overload;

    function GetSubDocument(const APos: IdxRichEditDocumentPosition): TdxNativeSubDocument;

    function CreateMailMergeOptions: IdxRichEditMailMergeOptions;
    procedure MailMerge(const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const ATargetDocument: IdxRichEditDocument); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; const ATargetDocument: IdxRichEditDocument); overload;

    procedure Protect(const APassword: string);
    procedure Unprotect;
    procedure SetEncryptionPassword(const APassword: string);
    function HasEncryptionPassword: Boolean;

    property AbstractNumberingLists: IdxRichEditAbstractNumberingListCollection read GetAbstractNumberingLists;
    property ActiveSubDocument: IdxRichEditSubDocument read GetActiveSubDocument;
    property CaretPosition: IdxRichEditDocumentPosition read GetCaretPosition write SetCaretPosition;
    property CharacterStyles: IdxRichEditCharacterStyleCollection read GetCharacterStyles;
    property IsDocumentProtected: Boolean read GetIsDocumentProtected;
    property IsEmpty: Boolean read GetIsEmpty;
    property NumberingLists: IdxRichEditNumberingListCollection read GetNumberingLists;
    property ParagraphStyles: IdxRichEditParagraphStyleCollection read GetParagraphStyles;
    property Sections: IdxRichEditSectionCollection read GetSections;
    property Selection: IdxRichEditDocumentRange read GetSelection write SetSelection;
    property Selections: IdxRichEditSelectionCollection read GetSelections;
    property TableStyles: IdxRichEditTableStyleCollection read FTableStyles;
  end;

implementation

uses
  Contnrs, Math,

  dxRichEdit.Api.Formatting,
  dxRichEdit.Api.Sections,
  dxRichEdit.Api.Paragraphs,
  dxRichEdit.Api.Fields,
  dxRichEdit.Api.Images,
  dxRichEdit.Api.Hyperlinks,
  dxRichEdit.Api.Shapes,
  dxRichEdit.Api.FindAndReplace,
  dxRichEdit.Api.Protection,
  dxRichEdit.Api.Tables,

  dxRichEdit.Options,
  dxRichEdit.ServiceManager,
  dxRichEdit.Import.Core,
  dxRichEdit.Import.DocumentImportHelper,
  dxRichEdit.Commands.CopyAndPaste,
  dxRichEdit.Commands.HeaderFooter,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.UriStreamService,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

type
  TdxCopySelectionManagerAccess = class(TdxCopySelectionManager);
  TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBaseAccess = class(TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase);

  { TdxDocumentTextContentInserter }

  TdxDocumentTextContentInserter = class(TdxDocumentContentInserter)
  strict private
    FText: string;
  public
    constructor Create(const AText: string; AResetMerging: Boolean); overload;
    constructor Create(const AText: string); overload;
    function CanInsertContent: Boolean; override;
    function InsertContentCore(APieceTable: TdxPieceTable;
      ALogPosition: TdxDocumentLogPosition): Boolean; override;
  end;

  { TdxDocumentModelContentInserter }

  TdxDocumentModelContentInserter = class abstract(TdxDocumentContentInserter)
  strict private
    FInsertOptions: TdxRichEditInsertOptions;
  protected
    function GetCopyLastParagraph: Boolean; virtual; abstract;
    function CreateDocumentModel(ATargetDocumentModel: TdxDocumentModel): TdxDocumentModel; virtual; abstract;

    property CopyLastParagraph: Boolean read GetCopyLastParagraph;
  public
    constructor Create(AInsertOptions: TdxRichEditInsertOptions); reintroduce;
    function CanInsertContent: Boolean; override;
    function InsertContentCore(APieceTable: TdxPieceTable;
      ALogPosition: TdxDocumentLogPosition): Boolean; override;

    property InsertOptions: TdxRichEditInsertOptions read FInsertOptions;
  end;

  { TdxDocumentStreamContentInserterEventRouter }

  TdxDocumentStreamContentInserterEventRouter = class
  strict private
    FDocumentModel: TdxDocumentModel;
    procedure DoBeforeImport(ASender: TObject; E: TdxBeforeImportEventArgs);
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;

    procedure SubscribeEvents(ADocumentModel: TdxDocumentModel);
    procedure UnsubscribeEvents(ADocumentModel: TdxDocumentModel);
  end;

  { TdxDocumentStreamContentInserter }

  TdxDocumentStreamContentInserter = class(TdxDocumentModelContentInserter)
  strict private
    FStream: TStream;
    FFormat: TdxRichEditDocumentFormat;
    FSourceUri: string;
  protected
    function GetCopyLastParagraph: Boolean; override;
    function CreateDocumentModel(ATargetDocumentModel: TdxDocumentModel): TdxDocumentModel; override;
  public
    constructor Create(AStream: TStream;
      AFormat: TdxRichEditDocumentFormat;
      const ASourceUri: string; AInsertOptions: TdxRichEditInsertOptions); reintroduce;
  end;

  { TdxDocumentRangeContentInserter }

  TdxDocumentRangeContentInserter = class(TdxDocumentModelContentInserter)
  strict private
    FSourceRange: IdxRichEditDocumentRange;
  protected
    function GetCopyLastParagraph: Boolean; override;
    function CreateDocumentModel(ATargetDocumentModel: TdxDocumentModel): TdxDocumentModel; override;
  public
    constructor Create(const ASourceRange: IdxRichEditDocumentRange;
      AInsertOptions: TdxRichEditInsertOptions); reintroduce;
  end;

  { TdxFormattedDocumentContentInserter }

  TdxFormattedDocumentContentInserter = class abstract(TdxDocumentContentInserter)
  strict private
    FText: string;
    FInsertOptions: TdxRichEditInsertOptions;
  protected
    function CreateCommand(APieceTable: TdxPieceTable;
      AInsertOptions: TdxRichEditInsertOptions): TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase; virtual; abstract;
  public
    constructor Create(const AText: string; AInsertOptions: TdxRichEditInsertOptions); reintroduce;
    function CanInsertContent: Boolean; override;
    function InsertContentCore(APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition): Boolean; override;
  end;

  { TdxDocumentRtfTextContentInserter }

  TdxDocumentRtfTextContentInserter = class(TdxFormattedDocumentContentInserter)
  protected
    function CreateCommand(APieceTable: TdxPieceTable;
      AInsertOptions: TdxRichEditInsertOptions): TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase; override;
  end;

  { TdxDocumentSingleLineTextContentInserter }

  TdxDocumentSingleLineTextContentInserter = class(TdxDocumentContentInserter)
  strict private
    FText: string;
  public
    constructor Create(const AText: string); reintroduce;
    function CanInsertContent: Boolean; override;
    function InsertContentCore(APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition): Boolean; override;
  end;

  { TdxRangeComparer }

  TdxRangeComparer = class(TInterfacedObject, IComparer<IdxRichEditDocumentRange>)
  public
    function Compare(const Left, Right: IdxRichEditDocumentRange): Integer;
  end;

{ TdxDocumentTextContentInserter }

constructor TdxDocumentTextContentInserter.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

constructor TdxDocumentTextContentInserter.Create(const AText: string; AResetMerging: Boolean);
begin
  inherited Create(AResetMerging);
  FText := AText;
end;

function TdxDocumentTextContentInserter.CanInsertContent: Boolean;
begin
  Result := FText <> '';
end;

function TdxDocumentTextContentInserter.InsertContentCore(APieceTable: TdxPieceTable;
  ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  APieceTable.InsertPlainText(ALogPosition, FText);
  Result := True;
end;

{ TdxDocumentModelContentInserter }

constructor TdxDocumentModelContentInserter.Create(AInsertOptions: TdxRichEditInsertOptions);
begin
  inherited Create;
  FInsertOptions := AInsertOptions;
end;

function TdxDocumentModelContentInserter.CanInsertContent: Boolean;
begin
  Result := True;
end;

function TdxDocumentModelContentInserter.InsertContentCore(APieceTable: TdxPieceTable;
  ALogPosition: TdxDocumentLogPosition): Boolean;
var
  ADocumentModel: TdxDocumentModel;
  ACommand: TdxPieceTableInsertContentConvertedToDocumentModelCommand;
begin
  ADocumentModel := CreateDocumentModel(APieceTable.DocumentModel);
  try
    if ADocumentModel = nil then
      Exit(False);

    ADocumentModel.IntermediateModel := True;
    ACommand := TdxPieceTableInsertContentConvertedToDocumentModelCommand.Create(APieceTable, ADocumentModel,
      ALogPosition, InsertOptions, False);
    try
      ACommand.CopyLastParagraph := CopyLastParagraph;
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  finally
    ADocumentModel.Free;
  end;
  Result := True;
end;

{ TdxDocumentStreamContentInserterEventRouter }

constructor TdxDocumentStreamContentInserterEventRouter.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
end;

destructor TdxDocumentStreamContentInserterEventRouter.Destroy;
begin
  inherited Destroy;
end;

procedure TdxDocumentStreamContentInserterEventRouter.SubscribeEvents(ADocumentModel: TdxDocumentModel);
begin
  ADocumentModel.BeforeImport.Add(DoBeforeImport);
end;

procedure TdxDocumentStreamContentInserterEventRouter.UnsubscribeEvents;
begin
  ADocumentModel.BeforeImport.Remove(DoBeforeImport);
end;

procedure TdxDocumentStreamContentInserterEventRouter.DoBeforeImport(ASender: TObject; E: TdxBeforeImportEventArgs);
begin
  FDocumentModel.RaiseBeforeImport(E.DocumentFormat, E.Options);
end;

{ TdxDocumentStreamContentInserter }

constructor TdxDocumentStreamContentInserter.Create(AStream: TStream;
  AFormat: TdxRichEditDocumentFormat;
  const ASourceUri: string; AInsertOptions: TdxRichEditInsertOptions);
begin
  inherited Create(AInsertOptions);
  FStream := AStream;
  FFormat := AFormat;
  FSourceUri := ASourceUri;
end;

function TdxDocumentStreamContentInserter.GetCopyLastParagraph: Boolean;
begin
  Result := True;
end;

function TdxDocumentStreamContentInserter.CreateDocumentModel(ATargetDocumentModel: TdxDocumentModel): TdxDocumentModel;
var
  AUriStreamService: IdxUriStreamService;
  AImportManagerService: IdxDocumentImportManagerService;
  AImportHelper: TdxDocumentImportHelper;
  ARouter: TdxDocumentStreamContentInserterEventRouter;
begin
  Result := ATargetDocumentModel.CreateNew;
  Result.IntermediateModel := True;


  AUriStreamService := ATargetDocumentModel.GetService<IdxUriStreamService>;
  if AUriStreamService <> nil then
    Result.ReplaceService<IdxUriStreamService>(AUriStreamService);

  ARouter := TdxDocumentStreamContentInserterEventRouter.Create(ATargetDocumentModel);
  try
    ARouter.SubscribeEvents(Result);
    try
      AImportManagerService := ATargetDocumentModel.GetService<IdxDocumentImportManagerService>;
      if AImportManagerService = nil then
        TdxRichEditExceptions.ThrowInternalException;
      AImportHelper := TdxDocumentImportHelper.Create(Result);
      try
        AImportHelper.Import(FStream, FFormat, FSourceUri, AImportManagerService);
      finally
        AImportHelper.Free;
      end;
    finally
      ARouter.UnsubscribeEvents(Result);
      (Result as IdxServiceContainer).RemoveService(IdxUriStreamService);
    end;
  finally
    ARouter.Free;
  end;
end;

{ TdxDocumentRangeContentInserter }

constructor TdxDocumentRangeContentInserter.Create(
  const ASourceRange: IdxRichEditDocumentRange;
  AInsertOptions: TdxRichEditInsertOptions);
begin
  inherited Create(AInsertOptions);
  FSourceRange := ASourceRange;
end;

function TdxDocumentRangeContentInserter.GetCopyLastParagraph: Boolean;
begin
  Result := False;
end;

function TdxDocumentRangeContentInserter.CreateDocumentModel(ATargetDocumentModel: TdxDocumentModel): TdxDocumentModel;
var
  ADocument: TdxNativeSubDocument;
  ASelectionRanges: TdxSelectionRangeCollection;
  AManager: TdxCopySelectionManager;
  AOptions: TdxFormattingCopyOptions;
begin
  ADocument := TdxNativeSubDocument(TdxNativeDocumentPosition(FSourceRange.Start).Document);

  ASelectionRanges := ADocument.GetSelectionRangeCollection(FSourceRange);
  if ASelectionRanges = nil then
    Exit(nil);

  AManager := TdxCopySelectionManager.Create(ADocument.DocumentServer as IdxInnerControl);
  try
    if InsertOptions = TdxRichEditInsertOptions.MatchDestinationFormatting then
      AOptions := TdxFormattingCopyOptions.UseDestinationStyles
    else
      AOptions := TdxFormattingCopyOptions.KeepSourceFormatting;
    Result := TdxCopySelectionManagerAccess(AManager).CreateDocumentModel(TdxParagraphNumerationCopyOptions.CopyIfWholeSelected,
      AOptions, ADocument.PieceTable, ASelectionRanges);
  finally
    AManager.Free;
  end;
end;

{ TdxFormattedDocumentContentInserter }

constructor TdxFormattedDocumentContentInserter.Create(const AText: string;
  AInsertOptions: TdxRichEditInsertOptions);
begin
  inherited Create;
  FText := AText;
  FInsertOptions := AInsertOptions;
end;

function TdxFormattedDocumentContentInserter.CanInsertContent: Boolean;
begin
  Result := FText <> '';
end;

function TdxFormattedDocumentContentInserter.InsertContentCore(APieceTable: TdxPieceTable;
  ALogPosition: TdxDocumentLogPosition): Boolean;
var
  ACommand: TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase;
  AContent: TdxClipboardStringContent;
begin
  APieceTable.DocumentModel.BeginUpdate;
  try
    ACommand := CreateCommand(APieceTable, FInsertOptions);
    try
      AContent := TdxClipboardStringContent.Create(FText);
      try
        TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBaseAccess(ACommand).PasteContent(AContent, ALogPosition, '');
      finally
        AContent.free;
      end;
    finally
      ACommand.Free;
    end;
  finally
    APieceTable.DocumentModel.EndUpdate;
  end;
  Result := True;
end;

{ TdxDocumentRtfTextContentInserter }


function TdxDocumentRtfTextContentInserter.CreateCommand(APieceTable: TdxPieceTable;
  AInsertOptions: TdxRichEditInsertOptions): TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase;
begin
  Result := TdxPieceTablePasteRtfTextCommand.Create(APieceTable, AInsertOptions);
end;

{ TdxDocumentSingleLineTextContentInserter }

constructor TdxDocumentSingleLineTextContentInserter.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

function TdxDocumentSingleLineTextContentInserter.CanInsertContent: Boolean;
begin
  Result := FText <> '';
end;

function TdxDocumentSingleLineTextContentInserter.InsertContentCore(
  APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  APieceTable.InsertText(ALogPosition, FText);
  Result := True;
end;

{ TdxRangeComparer }

function TdxRangeComparer.Compare(const Left, Right: IdxRichEditDocumentRange): Integer;
begin
  Result := TdxNativeDocumentRange(Left).NormalizedStart.CompareTo(TdxNativeDocumentRange(Right).NormalizedStart);
end;

{ TdxDocumentContentInserter }

constructor TdxDocumentContentInserter.Create(AResetMerging: Boolean = False);
begin
  inherited Create;
  FResetMerging := AResetMerging;
end;

function TdxDocumentContentInserter.InsertContent(APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition): Boolean;
var
  ALastParagraph: TdxParagraph;
begin
  if not CanInsertContent then
    Exit(False);

  APieceTable.DocumentModel.BeginUpdate;
  try
    if Append then
    begin
      APieceTable.InsertParagraph(APieceTable.DocumentEndLogPosition);
      Result := InsertContentCore(APieceTable, ALogPosition + 1);
      CopyFormatting(APieceTable);
      ALastParagraph := APieceTable.Paragraphs.Last;
      APieceTable.DeleteContent(ALastParagraph.LogPosition - 1, 1, False);
    end
    else
    begin
      if FResetMerging then
        APieceTable.DocumentModel.ResetMerging;
      Result := InsertContentCore(APieceTable, ALogPosition);
    end;
  finally
    APieceTable.DocumentModel.EndUpdate;
  end;
end;

procedure TdxDocumentContentInserter.CopyFormatting(APieceTable: TdxPieceTable);
var
  ARuns: TdxTextRunCollection;
  ALastRun, APreviosToLastRun: TdxTextRunBase;
  AParagraphs: TdxParagraphCollection;
  ALastParagraph, APreviosToLastParagraph: TdxParagraph;
begin
  ARuns := APieceTable.Runs;
  ALastRun := ARuns.Last;
  APreviosToLastRun := ARuns[ARuns.Count - 2];
  ALastRun.CharacterProperties.CopyFrom(APreviosToLastRun.CharacterProperties);
  ALastRun.CharacterStyleIndex := APreviosToLastRun.CharacterStyleIndex;

  AParagraphs := APieceTable.Paragraphs;
  ALastParagraph := AParagraphs.Last;
  APreviosToLastParagraph := AParagraphs[AParagraphs.Count - 2];
  ALastParagraph.ParagraphProperties.CopyFrom(APreviosToLastParagraph.ParagraphProperties);
  ALastParagraph.ParagraphStyleIndex := APreviosToLastParagraph.ParagraphStyleIndex;
end;

{ TdxParagraphRange }

constructor TdxParagraphRange.Create(AStart, AEnd: TdxParagraphIndex);
begin
  FStart := AStart;
  FEnd := AEnd;
end;

function TdxParagraphRange.GetLength: Integer;
begin
  Result := &End - Start + 1;
end;

{ TdxNativeDocumentPosition }

constructor TdxNativeDocumentPosition.Create(const ADocument: IdxRichEditSubDocument;
  const APos: TdxDocumentModelPosition);
begin
  inherited Create;
  FPosition := APos;
  FAnchor := TdxDocumentModelPositionAnchor.Create(@FPosition);
  FDocument := ADocument;
  TdxNativeSubDocument(FDocument).RegisterAnchor(FAnchor);
end;

destructor TdxNativeDocumentPosition.Destroy;
begin
  TdxNativeSubDocument(FDocument).UnregisterAnchor(FAnchor);
  FreeAndNil(FAnchor);
  inherited Destroy;
end;

function TdxNativeDocumentPosition.GetLogPosition: Integer;
begin
  Result := Position.LogPosition;
end;

function TdxNativeDocumentPosition.Equals(AObj: TObject): Boolean;
var
  APos: IdxRichEditDocumentPosition;
begin
  Result := (Self = AObj) or
    ((Self <> nil) and
      Supports(AObj, IdxRichEditDocumentPosition, APos) and
      (LogPosition = APos.LogPosition));
end;

function TdxNativeDocumentPosition.GetHashCode: Integer;
begin
  Result := LogPosition;
end;

function TdxNativeDocumentPosition.ToString: string;
begin
  Result := IntToStr(LogPosition);
end;

function TdxNativeDocumentPosition.CompareTo(const APos: IdxRichEditDocumentPosition): Integer;
begin
  Result := LogPosition - APos.LogPosition;
end;

function TdxNativeDocumentPosition.ToInt: Integer;
begin
  Result := LogPosition;
end;

{ TdxNativeDocumentRange }

constructor TdxNativeDocumentRange.Create(const AStart, AEnd: IdxRichEditDocumentPosition);
begin
  inherited Create;
  FStart := AStart;
  FEnd := AEnd;
end;

constructor TdxNativeDocumentRange.Create(const ADocument: IdxRichEditSubDocument;
  const AStart, AEnd: TdxDocumentModelPosition);
begin
  inherited Create;
  FStart := TdxNativeDocumentPosition.Create(ADocument, AStart);
  FEnd := TdxNativeDocumentPosition.Create(ADocument, AEnd);
end;

function TdxNativeDocumentRange.GetNormalizedStart: IdxRichEditDocumentPosition;
begin
  if FStart.LogPosition <= FEnd.LogPosition then
    Result := FStart
  else
    Result := FEnd;
end;

function TdxNativeDocumentRange.GetNormalizedEnd: IdxRichEditDocumentPosition;
begin
  if FStart.LogPosition <= FEnd.LogPosition then
    Result := FEnd
  else
    Result := FStart;
end;

function TdxNativeDocumentRange.GetLength: Integer;
begin
  Result := Abs(FEnd.LogPosition - FStart.LogPosition);
end;

function TdxNativeDocumentRange.Contains(const APos: IdxRichEditDocumentPosition): Boolean;
begin
  Result := (NormalizedStart.LogPosition <= APos.LogPosition) and
    (APos.LogPosition < NormalizedEnd.LogPosition);
end;

function TdxNativeDocumentRange.GetEnd: IdxRichEditDocumentPosition;
begin
  Result := FEnd;
end;

function TdxNativeDocumentRange.GetStart: IdxRichEditDocumentPosition;
begin
  Result := FStart;
end;

{ TdxNativeFieldCollection }

constructor TdxNativeFieldCollection.Create(ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TdxNativeFieldCollection.GetPieceTable: TdxPieceTable;
begin
  Result := FDocument.PieceTable;
end;

function TdxNativeFieldCollection.GetCount: Integer;
begin
  Result := Count;
end;

function TdxNativeFieldCollection.GetItem(AIndex: Integer): IdxRichEditField;
begin
  Result := Self[AIndex];
end;

procedure TdxNativeFieldCollection.Update;
var
  ADocumentModel: TdxDocumentModel;
  APieceTable: TdxPieceTable;
  AUpdater: TdxFieldUpdater;
begin
  FDocument.BeginUpdate;
  try
    ADocumentModel := FDocument.DocumentModel;
    ADocumentModel.BeginUpdate;
    try
      APieceTable := FDocument.PieceTable;
      AUpdater := TdxFieldUpdater.Create(APieceTable);
      try
        AUpdater.UpdateFields(TdxUpdateFieldOperationType.Normal);
      finally
        AUpdater.Free;
      end;
    finally
      ADocumentModel.EndUpdate;
    end;
  finally
    FDocument.EndUpdate;
  end;
end;

function TdxNativeFieldCollection.Add(const Value: IdxRichEditField): Integer;
begin
  Result := inherited Add(Value);
end;

function TdxNativeFieldCollection.Add(const ACodeRange: IdxRichEditDocumentRange): IdxRichEditField;
var
  AField: TdxField;
begin
  AField := FDocument.PieceTable.CreateField(ACodeRange.Start.LogPosition, ACodeRange.Length);
  Result := Self[AField.Index];
end;

function TdxNativeFieldCollection.Add(const AStart: IdxRichEditDocumentPosition; const ACode: string): IdxRichEditField;
var
  ARange: IdxRichEditDocumentRange;
begin
  FDocument.BeginUpdate;
  try
    ARange := FDocument.InsertText(AStart, ACode);
    Result := Add(ARange);
  finally
    FDocument.EndUpdate;
  end;
end;

function TdxNativeFieldCollection.CreateField(const ACodeRange: IdxRichEditDocumentRange): IdxRichEditField;
var
  AField: TdxField;
begin
  AField := FDocument.PieceTable.CreateField(ACodeRange.Start.LogPosition, ACodeRange.Length);
  Result := Self[AField.Index];
end;

function TdxNativeFieldCollection.CreateField(const AStart: IdxRichEditDocumentPosition; const ACode: string): IdxRichEditField;
var
  ARange: IdxRichEditDocumentRange;
begin
  FDocument.BeginUpdate;
  try
    ARange := FDocument.InsertText(AStart, ACode);
    Result := CreateField(ARange);
  finally
    FDocument.EndUpdate;
  end;
end;

function TdxNativeFieldCollection.Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyFieldCollection;
var
  ANativeRange: TdxNativeDocumentRange;
  AFirstRunIndex, ALastRunIndex, AFirstIndex, ALastIndex: Integer;
  ACount, I: Integer;
  AField: TdxNativeField;
  AResult: TdxNativeFieldCollection;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentRange(ARange);

  ANativeRange := TdxNativeDocumentRange(ARange);
  AFirstRunIndex := TdxNativeDocumentPosition(ANativeRange.NormalizedStart).Position.RunIndex;
  ALastRunIndex := TdxNativeDocumentPosition(ANativeRange.NormalizedEnd).Position.RunIndex;
  AResult := TdxNativeFieldCollection.Create(FDocument);
  ACount := PieceTable.Fields.Count;
  for I := 0 to ACount - 1 do
  begin
    AFirstIndex := PieceTable.Fields[I].FirstRunIndex;
    ALastIndex := PieceTable.Fields[I].LastRunIndex;
    if (AFirstIndex >= AFirstRunIndex) and (ALastIndex <= ALastRunIndex) then
    begin
      AField := TdxNativeField.Create(FDocument, PieceTable.Fields[I]);
      AResult.Add(AField);
    end;
  end;
  Result := AResult;
end;

{ TdxNativeSubDocument }

constructor TdxNativeSubDocument.Create(APieceTable: TdxPieceTable; AServer: TdxInnerRichEditDocumentServer);
begin
  inherited Create(APieceTable, AServer);
  OnUnitsChanged;
end;

procedure TdxNativeSubDocument.UnitsChanged;
begin
  OnUnitsChanged;
end;

function TdxNativeSubDocument.GetMainDocument: TdxNativeDocument;
begin
  Result := TdxNativeDocument(Server.NativeDocument);
end;

function TdxNativeSubDocument.GetTableCellStyle(ACell: TdxTableCell): IdxRichEditTableCellStyle;
begin
  Result := nil;
end;

function TdxNativeSubDocument.GetInnerTableCellStyle(const AStyle: IdxRichEditTableCellStyle): TdxTableCellStyle;
begin
  Result := nil;
end;

function TdxNativeSubDocument.GetUnit: TdxRichEditDocumentUnit;
begin
  CheckValid;
  Result := DocumentServer.MeasurementUnit;
end;

procedure TdxNativeSubDocument.SetUnit(const Value: TdxRichEditDocumentUnit);
begin
  CheckValid;
  DocumentServer.MeasurementUnit := Value;
end;

function TdxNativeSubDocument.GetFields: IdxRichEditFieldCollection;
begin
  CheckValid;
  Result := FFields;
end;

function TdxNativeSubDocument.GetImages: IdxRichEditDocumentImageCollection;
begin
  CheckValid;
  Result := FImages;
end;

function TdxNativeSubDocument.GetParagraphs: IdxRichEditParagraphCollection;
begin
  CheckValid;
  Result := FParagraphs;
end;

function TdxNativeSubDocument.GetBookmarks: IdxRichEditBookmarkCollection;
begin
  CheckValid;
  Result := FBookmarks;
end;


function TdxNativeSubDocument.GetHyperlinks: IdxRichEditHyperlinkCollection;
begin
  CheckValid;
  Result := FHyperlinks;
end;

function TdxNativeSubDocument.GetTables: IdxRichEditTableCollection;
begin
  CheckValid;
  Result := FTables;
end;

function TdxNativeSubDocument.GetShapes: IdxRichEditShapeCollection;
begin
  CheckValid;
  Result := FShapes;
end;


function TdxNativeSubDocument.GetRange: IdxRichEditDocumentRange;
var
  APieceTable: TdxPieceTable;
  AParagraphs: TdxParagraphCollection;
begin
  CheckValid;
  APieceTable := PieceTable;
  AParagraphs := APieceTable.Paragraphs;
  Result := CreateNativeRange(TdxDocumentModelPosition.FromParagraphStart(APieceTable, AParagraphs.First.Index),
    TdxDocumentModelPosition.FromParagraphEnd(APieceTable, AParagraphs.Last.Index));
end;

function TdxNativeSubDocument.GetLength: Integer;
var
  ALastParagraph: TdxParagraph;
begin
  CheckValid;
  ALastParagraph := PieceTable.Paragraphs.Last;
  Result := ALastParagraph.LogPosition + ALastParagraph.Length;
end;

function TdxNativeSubDocument.GetEndPosition: IdxRichEditDocumentPosition;
var
  APieceTable: TdxPieceTable;
  AParagraphs: TdxParagraphCollection;
begin
  APieceTable := PieceTable;
  AParagraphs := APieceTable.Paragraphs;
  Result := TdxNativeDocumentPosition.Create(Self,
    TdxDocumentModelPosition.FromParagraphEnd(APieceTable, AParagraphs.Last.Index));
end;

function TdxNativeSubDocument.GetPageBackColor: TdxAlphaColor;
begin
  CheckValid;
  Result := DocumentModel.DocumentProperties.PageBackColor;
end;

function TdxNativeSubDocument.GetShowPageBackground: Boolean;
begin
  CheckValid;
  Result := DocumentModel.DocumentProperties.DisplayBackgroundShape;
end;

procedure TdxNativeSubDocument.SetPageBackColor(const Value: TdxAlphaColor);
begin
  CheckValid;
  DocumentModel.DocumentProperties.PageBackColor := Value;
end;

procedure TdxNativeSubDocument.SetShowPageBackground(const Value: Boolean);
begin
  CheckValid;
  DocumentModel.DocumentProperties.DisplayBackgroundShape := Value;
end;

procedure TdxNativeSubDocument.CheckValid;
begin
  if Self = nil then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUseInvalidDocument));
end;

procedure TdxNativeSubDocument.Invalidate;
begin
  Finalize;
  DestroyApiObjects;
  CreateApiObjects;
  Initialize;
end;

procedure TdxNativeSubDocument.CreateApiObjects;
begin
  inherited CreateApiObjects;
  FParagraphs := TdxNativeParagraphCollection.Create(Self);
  FFields := TdxNativeFieldCollection.Create(Self);
  FHyperlinks := TdxNativeHyperlinkCollection.Create(Self);
  FTables := TdxNativeTableCollection.Create(Self);
  FBookmarks := TdxNativeBookmarkCollection.Create(Self);
  FShapes := TdxNativeShapeCollection.Create(Self);
  FImages := TdxNativeDocumentImageCollection.Create(Self);
end;

procedure TdxNativeSubDocument.DoInitialize;
begin
  PopulateParagraphs;
  PopulateFields;
  PopulateBookmarks;
  PopulateHyperlinks;
  PopulateTables;
end;

procedure TdxNativeSubDocument.DoFinalize;
begin
end;

procedure TdxNativeSubDocument.DestroyApiObjects;
begin
  inherited DestroyApiObjects;
  FFields := nil;
  FParagraphs := nil;
  FBookmarks := nil;
  FHyperlinks := nil;
  FTables := nil;
  FShapes := nil;
  FImages := nil;
end;

procedure TdxNativeSubDocument.OnUnitsChanged;
begin
  FUnitConverter := InternalAPI.UnitConverters[&Unit];
  FPointsConverter := InternalAPI.UnitConverters[TdxRichEditDocumentUnit.Point];
end;

procedure TdxNativeSubDocument.PopulateParagraphs;
begin
  TdxNativeParagraphCollection(FParagraphs).Clear;
  PieceTable.Paragraphs.ForEach(RegisterParagraph);
end;

procedure TdxNativeSubDocument.RegisterParagraph(const AParagraph: TdxCustomParagraph);
begin
  TdxNativeParagraphCollection(FParagraphs).Add(TdxNativeParagraph.Create(Self, TdxParagraph(AParagraph)));
end;

procedure TdxNativeSubDocument.PopulateFields;
begin
  TdxNativeFieldCollection(FFields).Clear;
  PieceTable.Fields.ForEach(RegisterField);
end;

procedure TdxNativeSubDocument.RegisterField(const AField: TdxField);
begin
  TdxNativeFieldCollection(FFields).Add(TdxNativeField.Create(Self, AField));
end;

procedure TdxNativeSubDocument.PopulateBookmarks;
begin
  CheckValid;
  TdxNativeBookmarkCollection(FBookmarks).PopulateBookmarks;
end;


procedure TdxNativeSubDocument.PopulateHyperlinks;
begin
  TdxNativeHyperlinkCollection(FHyperlinks).PopulateHyperlinks;
end;

procedure TdxNativeSubDocument.PopulateTables;
begin
  TdxNativeTableCollection(FTables).PopulateTables;
end;

procedure TdxNativeSubDocument.DoSubscribeInternalAPIEvents;
begin
  inherited DoSubscribeInternalAPIEvents;
  InternalAPI.ParagraphInserted.Add(OnParagraphInserted);
  InternalAPI.ParagraphRemoved.Add(OnParagraphRemoved);
  InternalAPI.ParagraphMerged.Add(OnParagraphMerged);
  InternalAPI.FieldInserted.Add(OnFieldInserted);
  InternalAPI.FieldRemoved.Add(OnFieldRemoved);
  InternalAPI.HyperlinkInfoInserted.Add(OnHyperlinkInfoInserted);
  InternalAPI.HyperlinkInfoDeleted.Add(OnHyperlinkInfoDeleted);
  PieceTable.Tables.AddCollectionChangedHandler(OnTablesCollectionChanged);
  TdxNativeBookmarkCollection(FBookmarks).SubscribeEvents;
end;

procedure TdxNativeSubDocument.DoUnsubscribeInternalAPIEvents;
begin
  InternalAPI.ParagraphInserted.Remove(OnParagraphInserted);
  InternalAPI.ParagraphRemoved.Remove(OnParagraphRemoved);
  InternalAPI.ParagraphMerged.Remove(OnParagraphMerged);
  InternalAPI.FieldInserted.Remove(OnFieldInserted);
  InternalAPI.FieldRemoved.Remove(OnFieldRemoved);
  InternalAPI.HyperlinkInfoInserted.Remove(OnHyperlinkInfoInserted);
  InternalAPI.HyperlinkInfoDeleted.Remove(OnHyperlinkInfoDeleted);
  PieceTable.Tables.RemoveCollectionChangedHandler(OnTablesCollectionChanged);
  TdxNativeBookmarkCollection(FBookmarks).UnsubscribeEvents;
  inherited DoUnsubscribeInternalAPIEvents;
end;

procedure TdxNativeSubDocument.OnParagraphInserted(ASender: TObject; E: TdxParagraphEventArgs);
var
  AParagraphIndex: Integer;
  AParagraph: IdxRichEditParagraph;
begin
  if PieceTable <> E.PieceTable then
    Exit;

  AParagraphIndex := E.ParagraphIndex;
  AParagraph := TdxNativeParagraph.Create(Self, PieceTable.Paragraphs[E.ParagraphIndex]);
  if FParagraphs.Count <= AParagraphIndex then
    TdxNativeParagraphCollection(FParagraphs).Add(AParagraph)
  else
    TdxNativeParagraphCollection(FParagraphs).Insert(AParagraphIndex, AParagraph);
end;

procedure TdxNativeSubDocument.OnParagraphRemoved(ASender: TObject; E: TdxParagraphEventArgs);
begin
  if PieceTable <> E.PieceTable then
    Exit;
  OnParagraphRemovedCore(E);
end;

procedure TdxNativeSubDocument.OnParagraphMerged(ASender: TObject; E: TdxParagraphEventArgs);
begin
  if PieceTable <> E.PieceTable then
    Exit;
  OnParagraphRemovedCore(E);
end;

procedure TdxNativeSubDocument.OnParagraphRemovedCore(E: TdxParagraphEventArgs);
var
  AParagraphIndex: Integer;
  AParagraph: TdxNativeParagraph;
begin
  AParagraphIndex := E.ParagraphIndex;
  AParagraph := TdxNativeParagraph(FParagraphs[AParagraphIndex]);
  AParagraph.IsValid := False;
  TdxNativeParagraphCollection(FParagraphs).Delete(AParagraphIndex);
end;

procedure TdxNativeSubDocument.OnFieldInserted(ASender: TObject; E: TdxFieldEventArgs);
var
  AFieldIndex: Integer;
  AField: TdxNativeField;
begin
  if PieceTable <> E.PieceTable then
    Exit;

  AFieldIndex := E.FieldIndex;

  AField := TdxNativeField.Create(Self, PieceTable.Fields[AFieldIndex]);
  TdxNativeFieldCollection(FFields).Insert(AFieldIndex, AField);
end;

procedure TdxNativeSubDocument.OnFieldRemoved(ASender: TObject; E: TdxFieldEventArgs);
var
  AFieldIndex: Integer;
  AField: TdxNativeField;
begin
  if PieceTable <> E.PieceTable then
    Exit;

  AFieldIndex := E.FieldIndex;

  AField := TdxNativeField(FFields[AFieldIndex]);

  AField.IsValid := False;
  TdxNativeFieldCollection(FFields).Delete(AFieldIndex);
end;

procedure TdxNativeSubDocument.OnHyperlinkInfoInserted(ASender: TObject; E: TdxHyperlinkInfoEventArgs);
var
  AHyperlink: IdxRichEditHyperlink;
begin
  if PieceTable <> E.PieceTable then
    Exit;

  AHyperlink := TdxNativeHyperlink.Create(Self, PieceTable, PieceTable.Fields[E.FieldIndex]);
  TdxNativeHyperlinkCollection(FHyperlinks).Add(AHyperlink);
end;

procedure TdxNativeSubDocument.OnHyperlinkInfoDeleted(ASender: TObject; E: TdxHyperlinkInfoEventArgs);
var
  AHyperlink: IdxRichEditHyperlink;
begin
  if PieceTable <> E.PieceTable then
    Exit;
  if not FHyperlinks.FindHyperlink(E.FieldIndex, AHyperlink) then
    Exit;

  TdxNativeHyperlink(AHyperlink).IsValid := False;
  TdxNativeHyperlinkCollection(FHyperlinks).Remove(AHyperlink);
end;


procedure TdxNativeSubDocument.OnTablesCollectionChanged(const AItem: TObject; Action: TListNotification);
begin
  TdxNativeTableCollection(FTables).TablesCollectionChanged(TdxTable(AItem), Action);
end;

procedure TdxNativeSubDocument.BeginUpdate;
begin
  CheckValid;
  DocumentModel.BeginUpdate;
end;

procedure TdxNativeSubDocument.EndUpdate;
begin
  CheckValid;
  DocumentModel.EndUpdate;
end;


function TdxNativeSubDocument.UnitsToModelUnits(Value: Single): Integer;
begin
  Result := Round(UnitConverter.ToUnits(Value));
end;

function TdxNativeSubDocument.UnitsToModelUnitsF(Value: Single): Single;
begin
  Result := UnitConverter.ToUnits(Value);
end;

function TdxNativeSubDocument.ModelUnitsToPointsF(const Value: Single): Single;
begin
  Result := FPointsConverter.FromUnits(Value);
end;

function TdxNativeSubDocument.PointsToModelUnitsF(Value: Single): Single;
begin
  Result := FPointsConverter.ToUnits(Value);
end;

function TdxNativeSubDocument.ModelUnitsToUnits(Value: Integer): Single;
begin
  Result := UnitConverter.FromUnits(Value);
end;

function TdxNativeSubDocument.ModelUnitsToUnitsF(Value: Single): Single;
begin
  Result := UnitConverter.FromUnits(Value);
end;

function TdxNativeSubDocument.ModelUnitsToUnits(const Value: TdxNullableInteger): TdxNullableSingle;
begin
  if Value.HasValue then
    Result := ModelUnitsToUnits(Value.Value)
  else
    Result := TdxNullableSingle.Null;
end;

function TdxNativeSubDocument.ModelUnitsToUnitsF(const Value: TdxNullableSingle): TdxNullableSingle;
begin
  if Value.HasValue then
    Result := ModelUnitsToUnitsF(Value.Value)
  else
    Result := TdxNullableSingle.Null;
end;

function TdxNativeSubDocument.NormalizeLogPosition(APos: TdxDocumentLogPosition): TdxDocumentLogPosition;
begin
  Result := NormalizeLogPosition(PieceTable, APos);
end;

function TdxNativeSubDocument.NormalizeLogPosition(APieceTable: TdxPieceTable;
  APos: TdxDocumentLogPosition): TdxDocumentLogPosition;
begin
  Result := Max(APos, 0);
  Result := Min(Result, APieceTable.DocumentEndLogPosition);
end;

function TdxNativeSubDocument.InsertParagraph(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph;
begin
  Result := InsertParagraph(APos, TdxRichEditInsertOptions.MatchDestinationFormatting);
end;

function TdxNativeSubDocument.InsertParagraph(const APos: IdxRichEditDocumentPosition;
  const AInsertOptions: TdxRichEditInsertOptions): IdxRichEditParagraph;
var
  ANativePosition: TdxNativeDocumentPosition;
  AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition;
  AParagraph: TdxParagraph;
begin
  CheckValid;
  CheckDocumentPosition(APos);
  ANativePosition := TdxNativeDocumentPosition(APos);
  AParagraphIndex := ANativePosition.Position.ParagraphIndex;
  ALogPosition := NormalizeLogPosition(ANativePosition.Position.LogPosition);
  PieceTable.InsertParagraph(ALogPosition);
  AParagraph := PieceTable.Paragraphs[AParagraphIndex + 1];
  if AParagraph.IsInList and (AInsertOptions = TdxRichEditInsertOptions.KeepSourceFormatting) then
    PieceTable.RemoveNumberingFromParagraph(AParagraph);
  Result := Paragraphs[AParagraphIndex + 1];
end;

function TdxNativeSubDocument.AppendParagraph: IdxRichEditParagraph;
begin
  Result := InsertParagraph(EndPosition);
end;

function TdxNativeSubDocument.InsertContentCore(const APos: IdxRichEditDocumentPosition;
  AInserter: TdxDocumentContentInserter): IdxRichEditDocumentRange;
var
  ANativePosition: TdxNativeDocumentPosition;
  ALogPosition, ALogPosition2: TdxDocumentLogPosition;
  ALength: Integer;
begin
  CheckValid;
  CheckDocumentPosition(APos);

  ANativePosition := TdxNativeDocumentPosition(APos);
  ALogPosition := ANativePosition.Position.LogPosition;

  ALogPosition := NormalizeLogPosition(ALogPosition);

  if not AInserter.InsertContent(PieceTable, ALogPosition) then
    Exit(CreateZeroLengthRange(ALogPosition));

  ALogPosition2 := NormalizeLogPosition(ANativePosition.Position.LogPosition);
  ALength := ALogPosition2 - ALogPosition;
  if AInserter.Append then
    Inc(ALogPosition);
  if ALength > 0 then
    Result := CreateRange(ALogPosition, ALength)
  else
    Result := CreateZeroLengthRange(ALogPosition);
end;

function TdxNativeSubDocument.InsertText(const APos: IdxRichEditDocumentPosition;
  const AText: string): IdxRichEditDocumentRange;
var
  AInserter: TdxDocumentTextContentInserter;
begin
  AInserter := TdxDocumentTextContentInserter.Create(AText);
  try
    Result := InsertContentCore(APos, AInserter);
  finally
    AInserter.Free;
  end;
end;

function TdxNativeSubDocument.AppendText(const AText: string): IdxRichEditDocumentRange;
var
  AInserter: TdxDocumentTextContentInserter;
begin
  AInserter := TdxDocumentTextContentInserter.Create(AText, True);
  try
    Result := InsertContentCore(EndPosition, AInserter);
  finally
    AInserter.Free;
  end;
end;


function TdxNativeSubDocument.AppendSingleLineText(const AText: string): IdxRichEditDocumentRange;
begin
  Result := InsertSingleLineText(EndPosition, AText);
end;

function TdxNativeSubDocument.InsertSingleLineText(const APos: IdxRichEditDocumentPosition; const AText: string): IdxRichEditDocumentRange;
var
  AInserter: TdxDocumentSingleLineTextContentInserter;
begin
  AInserter := TdxDocumentSingleLineTextContentInserter.Create(AText);
  try
    Result := InsertContentCore(APos, AInserter);
  finally
    AInserter.Free;
  end;
end;

function TdxNativeSubDocument.InsertRtfText(const APos: IdxRichEditDocumentPosition;
  const ARtfText: string;
  AInsertOptions: TdxRichEditInsertOptions = TdxRichEditInsertOptions.MatchDestinationFormatting): IdxRichEditDocumentRange;
var
  AInserter: TdxDocumentRtfTextContentInserter;
begin
  AInserter := TdxDocumentRtfTextContentInserter.Create(ARtfText, AInsertOptions);
  try
    Result := InsertContentCore(APos, AInserter);
  finally
    AInserter.Free;
  end;
end;

function TdxNativeSubDocument.AppendRtfText(const ARtfText: string;
  AInsertOptions: TdxRichEditInsertOptions = TdxRichEditInsertOptions.MatchDestinationFormatting): IdxRichEditDocumentRange;
begin
  Result := InsertRtfText(EndPosition, ARtfText, AInsertOptions);
end;


function TdxNativeSubDocument.InsertPicture(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditShape;
begin
  Result := Shapes.InsertPicture(APos, AImage);
end;

function TdxNativeSubDocument.InsertTextBox(const APos: IdxRichEditDocumentPosition): IdxRichEditShape;
begin
  Result := Shapes.InsertTextBox(APos);
end;

function TdxNativeSubDocument.InsertImage(const APos: IdxRichEditDocumentPosition;
  AImage: TGraphic): IdxRichEditDocumentImage;
begin
  CheckValid;
  CheckDocumentPosition(APos);
  Result := FImages.Insert(APos, AImage);
end;

function TdxNativeSubDocument.AppendImage(AImage: TGraphic): IdxRichEditDocumentImage;
begin
  Result := InsertImage(EndPosition, AImage);
end;


function TdxNativeSubDocument.InsertTable(const APos: IdxRichEditDocumentPosition; ARowCount, AColumnCount: Integer;
  AAutoFitBehavior: TdxRichEditAutoFitBehaviorType = TdxRichEditAutoFitBehaviorType.AutoFitToContents;
  AFixedColumnWidths: Integer = MinInt): IdxRichEditTable;
begin
  Result := Tables.Add(APos, ARowCount, AColumnCount, AAutoFitBehavior, AFixedColumnWidths);
end;

function TdxNativeSubDocument.CreateRange(AStart, ALength: Integer): IdxRichEditDocumentRange;
var
  ARangeInfo: TdxRunInfo;
begin
  CheckValid;
  ARangeInfo := PieceTable.FindRunInfo(AStart, ALength + 1);
  try
    Result := CreateNativeRange(ARangeInfo.Start, ARangeInfo.&End);
  finally
    ARangeInfo.Free;
  end;
end;

function TdxNativeSubDocument.CreateRange(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditDocumentRange;
begin
  Result := CreateRange(AStart.LogPosition, ALength);
end;

function TdxNativeSubDocument.CreateNativeRange(const AStart, AEnd: TdxDocumentModelPosition): TdxNativeDocumentRange;
begin
  Result := TdxNativeDocumentRange.Create(Self, AStart, AEnd);
end;

function TdxNativeSubDocument.CreateZeroLengthRange(ALogPosition: TdxDocumentLogPosition): IdxRichEditDocumentRange;
begin
  Result := TdxNativeDocumentRange.Create(CreatePositionCore(ALogPosition), CreatePositionCore(ALogPosition));
end;

function TdxNativeSubDocument.CreatePosition(AStart: Integer): IdxRichEditDocumentPosition;
begin
  CheckValid;
  Result := CreatePositionCore(AStart);
end;

function TdxNativeSubDocument.CreatePositionCore(APos: TdxDocumentLogPosition): IdxRichEditDocumentPosition;
var
  APieceTable: TdxPieceTable;
  ARangeInfo: TdxRunInfo;
begin
  APieceTable := PieceTable;
  ARangeInfo := TdxRunInfo.Create(APieceTable);
  try
    APieceTable.CalculateRunInfoStart(APos, ARangeInfo);
    Result := CreateNativePosition(ARangeInfo.Start);
  finally
    ARangeInfo.Free;
  end;
end;

function TdxNativeSubDocument.CreateNativePosition(const APos: TdxDocumentModelPosition): TdxNativeDocumentPosition;
begin
  Result := TdxNativeDocumentPosition.Create(Self, APos);
end;

procedure TdxNativeSubDocument.Delete(const ARange: IdxRichEditDocumentRange);
var
  ANativeRange: TdxNativeDocumentRange;
  ALength: Integer;
  ADocumentLastParagraphSelected: Boolean;
begin
  CheckValid;
  CheckDocumentRange(ARange);

  ANativeRange := TdxNativeDocumentRange(ARange);
  ALength := ANativeRange.NormalizedEnd.LogPosition - ANativeRange.NormalizedStart.LogPosition;
  ADocumentLastParagraphSelected := False;
  if ANativeRange.NormalizedEnd.LogPosition > PieceTable.DocumentEndLogPosition then
  begin
    Dec(ALength, ANativeRange.NormalizedEnd.LogPosition - PieceTable.DocumentEndLogPosition);
    ADocumentLastParagraphSelected := True;
  end;
  if ALength <= 0 then
    Exit;
  PieceTable.DeleteContent(ANativeRange.NormalizedStart.LogPosition, ALength, ADocumentLastParagraphSelected);
end;

procedure TdxNativeSubDocument.SelectAll;
var
  ASelection: TdxSelection;
begin
  CheckValid;

  ASelection := DocumentModel.Selection;
  DocumentModel.BeginUpdate;
  try
    ASelection.ClearMultiSelection;
    ASelection.Start := PieceTable.DocumentStartLogPosition;
    ASelection.&End := PieceTable.DocumentEndLogPosition + 1;
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxNativeSubDocument.CalculateParagraphsRange(const ARange: IdxRichEditDocumentRange): TdxParagraphRange;
var
  ANativeRange: TdxNativeDocumentRange;
  AFirstParagraphIndex, ALastParagraphIndex: TdxParagraphIndex;
  ALastPosition: TdxDocumentModelPosition;
begin
  CheckDocumentRange(ARange);

  ANativeRange := TdxNativeDocumentRange(ARange);

  AFirstParagraphIndex := TdxNativeDocumentPosition(ANativeRange.NormalizedStart).Position.ParagraphIndex;
  ALastPosition := TdxNativeDocumentPosition(ANativeRange.NormalizedEnd).Position;
  ALastParagraphIndex := ALastPosition.ParagraphIndex;
  if ALastPosition.LogPosition = PieceTable.Paragraphs[ALastParagraphIndex].LogPosition then
    ALastParagraphIndex := Max(AFirstParagraphIndex, ALastParagraphIndex - 1);

  Result := TdxParagraphRange.Create(AFirstParagraphIndex, ALastParagraphIndex);
end;

function TdxNativeSubDocument.BeginUpdateParagraphs(const ARange: IdxRichEditDocumentRange): IdxRichEditParagraphProperties;
var
  AParagraphsRange: TdxParagraphRange;
  AModelParagraphs: TdxParagraphCollection;
  AList: TdxList<TdxCustomParagraph>;
  AParagraph: TdxParagraphBase;
  AFrom, ALength: Integer;
begin
  CheckValid;

  AParagraphsRange := CalculateParagraphsRange(ARange);
  AModelParagraphs := PieceTable.Paragraphs;

  AList := AModelParagraphs.GetRange(AParagraphsRange.Start, AParagraphsRange.Length);
  try
    AParagraph := AList.First;
    AFrom := TdxDocumentModelPosition.FromParagraphStart(PieceTable, AParagraph.Index).LogPosition;
    AParagraph := AList.Last;
    ALength := TdxDocumentModelPosition.FromParagraphEnd(PieceTable, AParagraph.Index).LogPosition - AFrom;
    Result := TdxNativeParagraphProperties.Create(Self, AFrom, ALength);
  finally
    AList.Free;
  end;
  DocumentModel.BeginUpdate;
end;

procedure TdxNativeSubDocument.EndUpdateParagraphs(const AProperties: IdxRichEditParagraphProperties);
var
  AProps: TdxNativeParagraphProperties;
begin
  CheckValid;
  AProps := TdxNativeParagraphProperties(AProperties);
  AProps.Apply;
  DocumentModel.EndUpdate;
end;

function TdxNativeSubDocument.BeginUpdateCharacters(AStart, ALength: Integer): IdxRichEditCharacterProperties;
begin
  CheckValid;
  Result := BeginUpdateCharactersCore(AStart, AStart + ALength);
end;

function TdxNativeSubDocument.BeginUpdateCharacters(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditCharacterProperties;
begin
  CheckValid;
  Result := BeginUpdateCharactersCore(AStart.LogPosition, AStart.LogPosition + ALength);
end;

function TdxNativeSubDocument.BeginUpdateCharacters(const ARange: IdxRichEditDocumentRange): IdxRichEditCharacterProperties;
var
  ANativeRange: TdxNativeDocumentRange;
begin
  CheckValid;
  CheckDocumentRange(ARange);
  ANativeRange := TdxNativeDocumentRange(ARange);
  Result := BeginUpdateCharactersCore(ANativeRange.NormalizedStart.LogPosition, ANativeRange.NormalizedEnd.LogPosition);
end;

function TdxNativeSubDocument.BeginUpdateCharactersCore(AStart, AEnd: Integer): IdxRichEditCharacterProperties;
var
  ALength: Integer;
  AResult: TdxNativeCharacterProperties;
begin
  DocumentModel.BeginUpdate;
  AStart := NormalizeLogPosition(AStart);
  AEnd := Min(AEnd, PieceTable.DocumentEndLogPosition + 1);

  ALength := AEnd - AStart;
  AResult := TdxNativeCharacterProperties.Create(Self, AStart, ALength);
  Result := AResult;
end;

procedure TdxNativeSubDocument.EndUpdateCharacters(const AProperties: IdxRichEditCharacterProperties);
var
  AProps: TdxNativeCharacterProperties;
begin
  CheckValid;
  AProps := TdxNativeCharacterProperties(AProperties);
  AProps.Apply;
  DocumentModel.EndUpdate;
end;

function TdxNativeSubDocument.BeginUpdateRangePermissions: IdxRichEditRangePermissionCollection;
var
  AModelPermissions: TdxRangePermissionCollection;
  ACount, I: Integer;
begin
  Result := TdxNativeRangePermissionCollection.Create;
  AModelPermissions := PieceTable.RangePermissions;
  ACount := AModelPermissions.Count;
  for I := 0 to ACount - 1 do
    Result.Add(CreateRangePermission(Result, AModelPermissions[I]));
end;

function TdxNativeSubDocument.CreateRangePermission(const ACollection: IdxRichEditRangePermissionCollection;
  const ARangePermission: TdxRangePermission): IdxRichEditRangePermission;
var
  ALength: Integer;
  ARange: IdxRichEditDocumentRange;
begin
  ALength := ARangePermission.&End - ARangePermission.Start;
  if ALength > 0 then
    ARange := CreateRange(ARangePermission.Start, ALength)
  else
    ARange := CreateZeroLengthRange(ARangePermission.Start);
  Result := ACollection.CreateRangePermission(ARange);
  Result.UserName := ARangePermission.UserName;
  Result.Group := ARangePermission.Group;
end;

procedure TdxNativeSubDocument.EndUpdateRangePermissions(const APermissions: IdxRichEditRangePermissionCollection);
var
  AModelPermissions: TdxRangePermissionCollection;
  I: Integer;
begin
  DocumentModel.BeginUpdate;
  try
    AModelPermissions := PieceTable.RangePermissions;
    for I := AModelPermissions.Count - 1 downto 0 do
      PieceTable.DeleteRangePermission(AModelPermissions[I]);

    if APermissions <> nil then
      ApplyNewRangePermissions(APermissions);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeSubDocument.ApplyNewRangePermissions(const APermissions: IdxRichEditRangePermissionCollection);
var
  ACount, I: Integer;
begin
  ACount := APermissions.Count;
  for I := 0 to ACount - 1 do
    ApplyNewRangePermission(APermissions[I]);
end;

procedure TdxNativeSubDocument.ApplyNewRangePermission(const ARangePermission: IdxRichEditRangePermission);
var
  ANativePositionStart, ANativePositionEnd: IdxRichEditDocumentPosition;
  ALogPositionStart, ALogPositionEnd: TdxDocumentLogPosition;
  AInfo: TdxRangePermissionInfo;
begin
  if (ARangePermission.UserName = '') and (ARangePermission.Group = '') then
    Exit;

  ANativePositionStart := ARangePermission.Range.Start;
  ANativePositionEnd := ARangePermission.Range.&End;
  ALogPositionStart := NormalizeLogPosition(ANativePositionStart.LogPosition);
  ALogPositionEnd := NormalizeLogPosition(ANativePositionEnd.LogPosition);

  AInfo := TdxRangePermissionInfo.Create;
  AInfo.UserName := ARangePermission.UserName;
  AInfo.Group := ARangePermission.Group;
  PieceTable.ApplyDocumentPermission(ALogPositionStart, ALogPositionEnd, AInfo);
end;

procedure TdxNativeSubDocument.CancelUpdateRangePermissions(const APermissions: IdxRichEditRangePermissionCollection);
begin
end;

function TdxNativeSubDocument.CreateBookmark(const AStart: IdxRichEditDocumentPosition;
  ALength: Integer; const AName: string): IdxRichEditBookmark;
begin
  CheckValid;
  Result := FBookmarks.CreateBookmark(AStart, ALength, AName);
end;

function TdxNativeSubDocument.CreateBookmark(const ARange: IdxRichEditDocumentRange; const AName: string): IdxRichEditBookmark;
begin
  Result := CreateBookmark(ARange.Start, ARange.Length, AName);
end;


procedure TdxNativeSubDocument.SelectBookmark(const ABookmark: IdxRichEditBookmark);
begin
  CheckValid;
  FBookmarks.SelectBookmark(ABookmark);
end;

procedure TdxNativeSubDocument.SetSelectionCore(AStart, AEnd: TdxDocumentLogPosition;
  AForceUpdateTableSelection: Boolean = True);
var
  ASelection: TdxSelection;
begin
  DocumentModel.BeginUpdate;
  try
    AStart := NormalizeLogPosition(DocumentModel.Selection.PieceTable, AStart);
    AEnd := Min(AEnd, DocumentModel.Selection.PieceTable.DocumentEndLogPosition + 1);

    ASelection := DocumentModel.Selection;
    ASelection.BeginUpdate;
    try
      ASelection.ClearMultiSelection;
      ASelection.Start := AStart;
      ASelection.&End := AEnd;
      ASelection.SetStartCell(AStart);
      if (AStart <> AEnd) and AForceUpdateTableSelection then
      begin
        ASelection.UpdateTableSelectionEnd(AEnd);
        ASelection.UpdateTableSelectionStart(AStart);
      end;
    finally
      ASelection.EndUpdate;
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeSubDocument.RemoveBookmark(const ABookmark: IdxRichEditBookmark);
begin
  CheckValid;
  FBookmarks.RemoveBookmark(ABookmark);
end;

function TdxNativeSubDocument.CreateHyperlink(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditHyperlink;
begin
  CheckValid;
  Result := FHyperlinks.CreateHyperlink(AStart, ALength);
end;

function TdxNativeSubDocument.CreateHyperlink(const ARange: IdxRichEditDocumentRange): IdxRichEditHyperlink;
begin
  Result := CreateHyperlink(ARange.Start, ARange.Length);
end;

procedure TdxNativeSubDocument.RemoveHyperlink(const AHyperlink: IdxRichEditHyperlink);
begin
  CheckValid;
  FHyperlinks.RemoveHyperlink(AHyperlink);
end;

function TdxNativeSubDocument.GetParagraph(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph;
var
  AParagraphIndex: Integer;
begin
  CheckValid;
  CheckDocumentPosition(APos);

  AParagraphIndex := PieceTable.FindParagraphIndex(APos.LogPosition, False);
  if AParagraphIndex < 0 then
    Exit(nil);
  Result := Paragraphs[AParagraphIndex];
end;

function TdxNativeSubDocument.GetParagraphs(const ARange: IdxRichEditDocumentRange): IdxRichEditParagraphCollection;
var
  AParagraphsRange: TdxParagraphRange;
  AFirstIndex, ALastIndex, I: Integer;
  AResult: TdxNativeParagraphCollection;
begin
  CheckValid;

  AParagraphsRange := CalculateParagraphsRange(ARange);
  AFirstIndex := AParagraphsRange.Start;
  ALastIndex := AFirstIndex + AParagraphsRange.Length - 1;
  AResult := TdxNativeParagraphCollection.Create(Self);
  for I := AFirstIndex to ALastIndex do
    AResult.Add(FParagraphs[I]);
  Result := AResult;
end;

function TdxNativeSubDocument.GetImages(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyDocumentImageCollection;
begin
  CheckValid;
  CheckDocumentRange(ARange);
  Result := FImages.Get(ARange);
end;

function TdxNativeSubDocument.GetShapes(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyShapeCollection;
begin
  CheckValid;
  CheckDocumentRange(ARange);
  Result := Shapes.Get(ARange);
end;

function TdxNativeSubDocument.GetSelectionRangeCollection(const ARange: IdxRichEditDocumentRange): TdxSelectionRangeCollection;
var
  ALength: Integer;
  ANativeRange: TdxNativeDocumentRange;
  AStart, AEnd: TdxDocumentLogPosition;
begin
  ALength := ARange.Length;
  if ALength <= 0 then
    Exit(nil);

  ANativeRange := TdxNativeDocumentRange(ARange);
  AStart := NormalizeLogPosition(ANativeRange.NormalizedStart.LogPosition);
  AEnd := Min(ANativeRange.NormalizedEnd.LogPosition, PieceTable.DocumentEndLogPosition + 1);
  ALength := AEnd - AStart;
  if ALength <= 0 then
    Exit(nil);
  Result := TdxSelectionRangeCollection.Create(AStart, ALength);
end;

function TdxNativeSubDocument.CreateCopySelectionManagerForGetContent: TdxCopySelectionManager;
begin
  Result := TdxCopySelectionManager.Create(DocumentServer as IdxInnerControl);
  Result.FixLastParagraph := True;
  Result.AllowCopyWholeFieldResult := True;
  Result.DefaultPropertiesCopyOptions := TdxDefaultPropertiesCopyOptions.Always;
end;

function TdxNativeSubDocument.GetText(const ARange: IdxRichEditDocumentRange): string;
var
  ATextFragmentOptions: TdxTextFragmentOptions;
begin
  CheckValid;
  CheckDocumentRange(ARange);
  ATextFragmentOptions := TdxTextFragmentOptions.Create;
  Result := GetText(ARange, ATextFragmentOptions);
end;

function TdxNativeSubDocument.GetText(const ARange: IdxRichEditDocumentRange; const AFragmentOptions: TdxRichEditTextFragmentOptions): string;
var
  AManager: TdxCopySelectionManager;
  ASelection: TdxSelectionRangeCollection;
begin
  CheckValid;
  CheckDocumentRange(ARange);

  ASelection := GetSelectionRangeCollection(ARange);
  try
    if ASelection = nil then
      Exit('');
    AManager := CreateCopySelectionManagerForGetContent;
    try
      Result := AManager.GetPlainText(PieceTable, ASelection, nil, @AFragmentOptions);
    finally
      AManager.Free;
    end;
  finally
    ASelection.Free;
  end;
end;

function TdxNativeSubDocument.GetRtfText(const ARange: IdxRichEditDocumentRange): string;
var
  AOptions: TdxRtfDocumentExporterOptions;
  ASelection: TdxSelectionRangeCollection;
  AManager: TdxCopySelectionManager;
begin
  CheckValid;
  CheckDocumentRange(ARange);
  ASelection := GetSelectionRangeCollection(ARange);
  try
    if ASelection = nil then
      Exit('');
    AOptions := TdxRtfDocumentExporterOptions.Create;
    try
      AOptions.ExportFinalParagraphMark := TdxExportFinalParagraphMark.Never;
      AManager := CreateCopySelectionManagerForGetContent;
      try
        Result := AManager.GetRtfText(PieceTable, ASelection, AOptions, True, True);
      finally
        AManager.Free;
      end;
    finally
      AOptions.Free;
    end;
  finally
    ASelection.Free;
  end;
end;


function TdxNativeSubDocument.GetOpenXmlBytes(const ARange: IdxRichEditDocumentRange): TBytes;
var
  ASelection: TdxSelectionRangeCollection;
  AManager: TdxCopySelectionManager;
begin
  CheckValid;
  CheckDocumentRange(ARange);
  ASelection := GetSelectionRangeCollection(ARange);
  try
    if ASelection = nil then
      Exit(nil);
    AManager := CreateCopySelectionManagerForGetContent;
    try
      Result := AManager.GetOpenXmlBytes(PieceTable, ASelection, nil);
    finally
      AManager.Free;
    end;
  finally
    ASelection.Free;
  end;
end;

function TdxNativeSubDocument.Replace(const ARange: IdxRichEditDocumentRange; const AText: string): Integer;
var
  AStart, AEnd, AEndLogPositionBefore, AEndLogPositionAfter: TdxDocumentLogPosition;
  ALength: Integer;
begin
  CheckValid;
  CheckDocumentRange(ARange);

  AStart := ARange.Start.LogPosition;
  AEnd := ARange.&End.LogPosition;
  ALength := AEnd - AStart;
  if ARange.&End.CompareTo(Range.&End) = 0 then
    Dec(ALength);
  AEndLogPositionBefore := PieceTable.DocumentEndLogPosition;
  PieceTable.ReplaceText(AStart, ALength, AText);
  AEndLogPositionAfter := PieceTable.DocumentEndLogPosition;
  Result := AEndLogPositionAfter - AEndLogPositionBefore;
end;

function TdxNativeSubDocument.FindAll(const ATextToFind: string; AOptions: TdxRichEditSearchOptions = [];
  const ARange: IdxRichEditDocumentRange = nil): TArray<IdxRichEditDocumentRange>;
var
  AResult: TList<IdxRichEditDocumentRange>;
  ASearchResult: IdxRichEditSearchResult;
begin
  if ARange = nil then
    Exit(FindAll(ATextToFind, AOptions, Range));
  CheckValid;
  CheckDocumentRange(ARange);
  AResult := TList<IdxRichEditDocumentRange>.Create;
  try
    ASearchResult := StartSearch(ATextToFind, AOptions, TdxRichEditSearchDirection.Forward, ARange);
    while ASearchResult.FindNext do
      AResult.Add(ASearchResult.CurrentResult);
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

function TdxNativeSubDocument.FindAll(const ARegex: TRegEx;
  const ARange: IdxRichEditDocumentRange = nil): TArray<IdxRichEditDocumentRange>;
var
  AResult: TList<IdxRichEditDocumentRange>;
  ASearchResult: IdxRichEditSearchResult;
begin
  if ARange = nil then
    Exit(FindAll(ARegex, Range));
  CheckValid;
  CheckDocumentRange(ARange);
  AResult := TList<IdxRichEditDocumentRange>.Create;
  try
    ASearchResult := StartSearch(ARegex, ARange);
    while ASearchResult.FindNext do
      AResult.Add(ASearchResult.CurrentResult);
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

function TdxNativeSubDocument.ReplaceAll(const ATextToFind, AReplaceWith: string;
  AOptions: TdxRichEditSearchOptions = []; const ARange: IdxRichEditDocumentRange = nil): Integer;
var
  ASearchResult: IdxRichEditSearchResult;
begin
  if ARange = nil then
    Exit(ReplaceAll(ATextToFind, AReplaceWith, AOptions, Range));
  Result := 0;
  CheckValid;
  CheckDocumentRange(ARange);
  ASearchResult := StartSearch(ATextToFind, AOptions, TdxRichEditSearchDirection.Forward, ARange);
  DocumentModel.BeginUpdate;
  try
    while ASearchResult.FindNext do
    begin
      ASearchResult.Replace(AReplaceWith);
      Inc(Result);
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxNativeSubDocument.ReplaceAll(const ARegex: TRegEx; const AReplaceWith: string;
  const ARange: IdxRichEditDocumentRange = nil): Integer;
var
  AResult: IdxRichEditRegexSearchResult;
begin
  if ARange = nil then
    Exit(ReplaceAll(ARegex, AReplaceWith, Range));
  Result := 0;
  CheckValid;
  CheckDocumentRange(ARange);
  AResult := StartSearch(ARegex, ARange);
  DocumentModel.BeginUpdate;
  try
    while AResult.FindNext do
    begin
      AResult.Replace(AReplaceWith);
      Inc(Result);
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxNativeSubDocument.StartSearch(const ATextToFind: string; AOptions: TdxRichEditSearchOptions = [];
  ADirection: TdxRichEditSearchDirection = TdxRichEditSearchDirection.Forward;
  const ARange: IdxRichEditDocumentRange = nil): IdxRichEditSearchResult;
begin
  if ARange = nil then
    Exit(StartSearch(ATextToFind, AOptions, ADirection, Range));
  CheckValid;
  CheckDocumentRange(ARange);

  if ADirection = TdxRichEditSearchDirection.Forward then
    Result := TdxNativeSearchResultForward.Create(Self, ATextToFind, AOptions, ARange)
  else
    Result := TdxNativeSearchResultBackward.Create(Self, ATextToFind, AOptions, ARange);
end;

function TdxNativeSubDocument.StartSearch(const ARegex: TRegEx;
  const ARange: IdxRichEditDocumentRange = nil): IdxRichEditRegexSearchResult;
var
  AMaxGuaranteedSearchResultLength: Integer;
begin
  if ARange = nil then
    Exit(StartSearch(ARegex, Range));
  CheckValid;
  CheckDocumentRange(ARange);
  AMaxGuaranteedSearchResultLength := DocumentModel.SearchOptions.RegExResultMaxGuaranteedLength;
  Result := TdxNativeRegexSearchResult.Create(Self, ARegex, ARange, AMaxGuaranteedSearchResultLength)
end;

procedure TdxNativeSubDocument.CheckDocumentPosition(const ADocumentPosition: IdxRichEditDocumentPosition);
var
  APos: TdxNativeDocumentPosition;
begin
  APos := TdxNativeDocumentPosition(ADocumentPosition);
  if APos.Position.PieceTable <> PieceTable then
    ThrowDocumentPositionPieceTableMismatch;
end;

procedure TdxNativeSubDocument.CheckDocumentRange(const ADocumentRange: IdxRichEditDocumentRange);
var
  ARange: TdxNativeDocumentRange;
begin
  ARange := TdxNativeDocumentRange(ADocumentRange);
  if TdxNativeDocumentPosition(ARange.Start).Position.PieceTable <> PieceTable then
    ThrowDocumentPositionPieceTableMismatch;
end;

procedure TdxNativeSubDocument.ThrowDocumentPositionPieceTableMismatch;
begin
  TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionDocumentPositionDoesntMatchDocument));
end;

function TdxNativeSubDocument.InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
  AStream: TStream; AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange;
begin
  Result := InsertDocumentContent(APos, AStream, AFormat, TdxRichEditInsertOptions.MatchDestinationFormatting);
end;

function TdxNativeSubDocument.InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
  AStream: TStream; AFormat: TdxRichEditDocumentFormat; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange;
begin
  Result := InsertDocumentContent(APos, AStream, AFormat, '', AInsertOptions);
end;

function TdxNativeSubDocument.InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
  AStream: TStream; AFormat: TdxRichEditDocumentFormat;
  const ASourceUri: string): IdxRichEditDocumentRange;
begin
  Result := InsertDocumentContent(APos, AStream, AFormat, ASourceUri, TdxRichEditInsertOptions.MatchDestinationFormatting);
end;

function TdxNativeSubDocument.InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
  AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ASourceUri: string; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange;
var
  AInserter: TdxDocumentStreamContentInserter;
begin
  AInserter := TdxDocumentStreamContentInserter.Create(AStream, AFormat, ASourceUri, AInsertOptions);
  try
    Result := InsertContentCore(APos, AInserter);
  finally
    AInserter.Free;
  end;
end;

function TdxNativeSubDocument.InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
  const AFileName: string; AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange;
begin
  Result := InsertDocumentContent(APos, AFileName, AFormat, AFileName, TdxRichEditInsertOptions.MatchDestinationFormatting);
end;

function TdxNativeSubDocument.InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
  const AFileName: string; AFormat: TdxRichEditDocumentFormat; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange;
begin
  Result := InsertDocumentContent(APos, AFileName, AFormat, AFileName, AInsertOptions);
end;

function TdxNativeSubDocument.InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
  const AFileName: string; AFormat: TdxRichEditDocumentFormat; const ASourceUri: string): IdxRichEditDocumentRange;
begin
  Result := InsertDocumentContent(APos, AFileName, AFormat, ASourceUri, TdxRichEditInsertOptions.MatchDestinationFormatting);
end;

function TdxNativeSubDocument.InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
  const AFileName: string; AFormat: TdxRichEditDocumentFormat; const ASourceUri: string; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange;
var
  AStream: TdxMemoryStream;
  AInserter: TdxDocumentStreamContentInserter;
begin
  if AFormat = TdxRichEditDocumentFormat.Undefined then
    AFormat := DocumentModel.AutodetectDocumentFormat(AFileName);
  AStream := TdxMemoryStream.Create(AFileName);
  try
    AInserter := TdxDocumentStreamContentInserter.Create(AStream, AFormat, ASourceUri, AInsertOptions);
    try
      Result := InsertContentCore(APos, AInserter);
    finally
      AInserter.Free;
    end;
  finally
    AStream.Free;
  end;
end;

function TdxNativeSubDocument.InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
  const ARange: IdxRichEditDocumentRange): IdxRichEditDocumentRange;
begin
  Result := InsertDocumentContent(APos, ARange, TdxRichEditInsertOptions.MatchDestinationFormatting);
end;

function TdxNativeSubDocument.InsertDocumentContent(const APos: IdxRichEditDocumentPosition;
  const ARange: IdxRichEditDocumentRange; AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange;
var
  AInserter: TdxDocumentRangeContentInserter;
begin
  AInserter := TdxDocumentRangeContentInserter.Create(ARange, AInsertOptions);
  try
    Result := InsertContentCore(APos, AInserter);
  finally
    AInserter.Free;
  end;
end;

function TdxNativeSubDocument.AppendDocumentContent(AStream: TStream;
  AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange;
begin
  Result := AppendDocumentContent(AStream, AFormat, '');
end;

function TdxNativeSubDocument.AppendDocumentContent(AStream: TStream;
  AFormat: TdxRichEditDocumentFormat; const ASourceUri: string): IdxRichEditDocumentRange;
begin
  Result := AppendDocumentContent(AStream, AFormat, ASourceUri, TdxRichEditInsertOptions.MatchDestinationFormatting);
end;

function TdxNativeSubDocument.AppendDocumentContent(AStream: TStream;
  AFormat: TdxRichEditDocumentFormat; const ASourceUri: string;
  AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange;
var
  AInserter: TdxDocumentStreamContentInserter;
begin
  AInserter := TdxDocumentStreamContentInserter.Create(AStream, AFormat, ASourceUri, AInsertOptions);
  try
    Result := AppendDocumentContentCore(AInserter);
  finally
    AInserter.Free;
  end;
end;

function TdxNativeSubDocument.AppendDocumentContent(const AFileName: string;
  AFormat: TdxRichEditDocumentFormat): IdxRichEditDocumentRange;
begin
  Result := AppendDocumentContent(AFileName, AFormat, AFileName);
end;

function TdxNativeSubDocument.AppendDocumentContent(const AFileName: string;
  AFormat: TdxRichEditDocumentFormat; const ASourceUri: string): IdxRichEditDocumentRange;
begin
  Result := AppendDocumentContent(AFileName, AFormat, ASourceUri,
    TdxRichEditInsertOptions.MatchDestinationFormatting);
end;

function TdxNativeSubDocument.AppendDocumentContent(const AFileName: string;
  AFormat: TdxRichEditDocumentFormat; const ASourceUri: string;
  AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange;
var
  AStream: TdxMemoryStream;
  AInserter: TdxDocumentStreamContentInserter;
begin
  if AFormat = TdxRichEditDocumentFormat.Undefined then
    AFormat := DocumentModel.AutodetectDocumentFormat(AFileName);
  AStream := TdxMemoryStream.Create(AFileName);
  try
    AInserter := TdxDocumentStreamContentInserter.Create(AStream, AFormat, ASourceUri, AInsertOptions);
    try
      Result := AppendDocumentContentCore(AInserter);
    finally
      AInserter.Free;
    end;
  finally
    AStream.Free;
  end;
end;

function TdxNativeSubDocument.AppendDocumentContent(const ARange: IdxRichEditDocumentRange): IdxRichEditDocumentRange;
begin
  Result := AppendDocumentContent(ARange, TdxInsertOptions.MatchDestinationFormatting);
end;

function TdxNativeSubDocument.AppendDocumentContent(const ARange: IdxRichEditDocumentRange;
  AInsertOptions: TdxRichEditInsertOptions): IdxRichEditDocumentRange;
var
  AInserter: TdxDocumentRangeContentInserter;
begin
  AInserter := TdxDocumentRangeContentInserter.Create(ARange, AInsertOptions);
  try
    Result := AppendDocumentContentCore(AInserter);
  finally
    AInserter.Free;
  end;
end;

function TdxNativeSubDocument.AppendDocumentContentCore(AInserter: TdxDocumentContentInserter): IdxRichEditDocumentRange;
begin
  AInserter.Append := True;
  Result := InsertContentCore(EndPosition, AInserter);
end;

function TdxNativeSubDocument.GetWidthUnitFixedValue(AUnitValue: TdxWidthUnit): Single;
begin
  if AUnitValue.&Type = TdxWidthUnitType.ModelUnits then
    Result := ModelUnitsToUnits(AUnitValue.Value)
  else
    Result := 0;
end;

procedure TdxNativeSubDocument.SetWidthUnitFixedValue(AUnitValue: TdxWidthUnit; AValue: Single);
begin
  AUnitValue.&Type := TdxWidthUnitType.ModelUnits;
  AUnitValue.Value := UnitsToModelUnits(AValue);
end;

function TdxNativeSubDocument.GetWidthValue(AUnitValue: TdxWidthUnit): Single;
begin
  if AUnitValue.&Type = TdxWidthUnitType.ModelUnits then
    Result := ModelUnitsToUnits(AUnitValue.Value)
  else
    Result := AUnitValue.Value;
end;

procedure TdxNativeSubDocument.SetWidthValue(AUnitValue: TdxWidthUnit; AValue: Single);
begin
  if AUnitValue.&Type = TdxWidthUnitType.ModelUnits then
    AUnitValue.Value := UnitsToModelUnits(AValue)
  else
    AUnitValue.Value := Round(AValue);
end;

procedure TdxNativeSubDocument.AddParagraphsToList(const ARange: IdxRichEditDocumentRange;
  const AList: IdxRichEditNumberingList; ALevelIndex: Integer);
begin
  CheckValid;
  CheckDocumentRange(ARange);
  Paragraphs.AddParagraphsToList(ARange, AList, ALevelIndex);
end;

procedure TdxNativeSubDocument.AddParagraphToList(const AParagraph: IdxRichEditParagraph;
  const AList: IdxRichEditNumberingList; ALevelIndex: Integer);
begin
  Paragraphs.AddParagraphToList(AParagraph, AList, ALevelIndex);
end;

procedure TdxNativeSubDocument.AddParagraphToList(const AParagraph: IdxRichEditParagraph;
  ANumberingListIndex, ALevelIndex: Integer);
begin
  Paragraphs.AddParagraphToList(AParagraph, ANumberingListIndex, ALevelIndex);
end;

procedure TdxNativeSubDocument.RemoveNumberingFromParagraph(const AParagraph: IdxRichEditParagraph);
begin
  Paragraphs.RemoveNumberingFromParagraph(AParagraph);
end;

procedure TdxNativeSubDocument.RemoveNumberingFromParagraphs(const ARange: IdxRichEditDocumentRange);
begin
  CheckValid;
  CheckDocumentRange(ARange);
  Paragraphs.RemoveNumberingFromParagraphs(ARange);
end;


function TdxNativeSubDocument.GetSubDocumentType: TdxRichEditSubDocumentType;
begin
  if PieceTable.IsMain then
    Result := TdxRichEditSubDocumentType.Main
  else if PieceTable.IsHeader then
    Result := TdxRichEditSubDocumentType.Header
  else if PieceTable.IsFooter then
    Result := TdxRichEditSubDocumentType.Footer
  else if PieceTable.IsTextBox then
    Result := TdxRichEditSubDocumentType.TextBox
  else
    raise TdxNotImplementedException.Create;
end;

{ TdxNativeSectionCollection }

function TdxNativeSectionCollection.GetCount: Integer;
begin
  Result := inherited Count;
end;

{ TdxNativeSelectionCollection }

constructor TdxNativeSelectionCollection.Create(ADocument: TdxNativeDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TdxNativeSelectionCollection.Add(const ARange: IdxRichEditDocumentRange): Integer;
var
  AList: TList<IdxRichEditDocumentRange>;
begin
  AList := TList<IdxRichEditDocumentRange>.Create;
  try
    AList.Add(ARange);
    Add(AList);
    Result := Count - 1;
  finally
    AList.Free;
  end;
end;

procedure TdxNativeSelectionCollection.Add(const ARanges: TList<IdxRichEditDocumentRange>);
var
  AGroupedRanges: TdxObjectList<TList<IdxRichEditDocumentRange>>;
  I, J: Integer;
  ASelections: TList<IdxRichEditDocumentRange>;
  AItem: IdxRichEditDocumentRange;
  ASelElementType: TSelectionRangeType;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
  ALastRange: TList<IdxRichEditDocumentRange>;
begin
  if (ARanges = nil) or (ARanges.Count = 0) then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionEmptyCollection));
  AGroupedRanges := GetGroupedSortedRanges(ARanges);
  try
    ValidateSelections(AGroupedRanges);
    FDocument.DocumentModel.BeginUpdate;
    try
      for I := 0 to AGroupedRanges.Count - 1 do
      begin
        ASelections := AGroupedRanges[I];
        if ASelections.Count = 1 then
          InternalAdd(ASelections[0], TSelectionRangeType.Single)
        else
        begin
          for J := 0 to ASelections.Count - 1 do
          begin
            AItem := ASelections[J];
            if J = 0 then
              ASelElementType := TSelectionRangeType.Start
            else
              if J = ASelections.Count - 1 then
                ASelElementType := TSelectionRangeType.End
              else
                ASelElementType := TSelectionRangeType.Middle;
            InternalAdd(AItem, ASelElementType);
          end;
        end;
      end;
      AStartRunIndex := TdxNativeDocumentPosition(TdxNativeDocumentRange(AGroupedRanges[0][0]).NormalizedStart).Position.RunIndex;
      ALastRange := AGroupedRanges[AGroupedRanges.Count - 1];
      AEndRunIndex := TdxNativeDocumentPosition(TdxNativeDocumentRange(ALastRange[ALastRange.Count - 1]).NormalizedEnd).Position.RunIndex;
      FDocument.DocumentModel.ApplyChangesCore(FDocument.DocumentModel.ActivePieceTable,
        DocumentModelChangeActions, AStartRunIndex, AEndRunIndex);
    finally
      FDocument.DocumentModel.EndUpdate;
    end;
  finally
    AGroupedRanges.Free;
  end;
end;

procedure TdxNativeSelectionCollection.Clear;
var
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
begin
  if InnerSelection.Items.Count = 0 then
    TdxRichEditExceptions.ThrowInternalException;
  FDocument.DocumentModel.BeginUpdate;
  try
    AStartRunIndex := InnerSelection.Items[0].Interval.NormalizedStart.RunIndex;
    AEndRunIndex := InnerSelection.Items[InnerSelection.Items.Count - 1].Interval.NormalizedEnd.RunIndex;

    InnerSelection.Items.DeleteRange(1, Count - 1);
    InnerSelection.Items[0].&End := InnerSelection.Items[0].Start;
    FDocument.DocumentModel.ApplyChangesCore(FDocument.DocumentModel.ActivePieceTable,
      DocumentModelChangeActions, AStartRunIndex, AEndRunIndex);
  finally
    FDocument.DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeSelectionCollection.Delete(Index: Integer);
var
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
  AClone: TList<IdxRichEditDocumentRange>;
  AGroupedItems: TdxObjectList<TList<IdxRichEditDocumentRange>>;
  I: Integer;
begin
  if (Index < 0) or (Index >= InnerSelection.Items.Count) then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionOutOfRange));
  if (InnerSelection.Items.Count = 1) and (InnerSelection.Items[Index].Length = 0) then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionCannotRemoveCaret));

  if InnerSelection.Items.Count = 1 then
  begin
    FDocument.DocumentModel.BeginUpdate;
    try
      AStartRunIndex := InnerSelection.Items[0].Interval.NormalizedStart.RunIndex;
      AEndRunIndex := InnerSelection.Items[0].Interval.NormalizedEnd.RunIndex;
      InnerSelection.Items[0].&End := InnerSelection.Items[0].Start;
      FDocument.DocumentModel.ApplyChangesCore(FDocument.DocumentModel.ActivePieceTable,
        DocumentModelChangeActions, AStartRunIndex, AEndRunIndex);
    finally
      FDocument.DocumentModel.EndUpdate;
    end;
    Exit;
  end;

  AClone := TList<IdxRichEditDocumentRange>.Create;
  try
    for I := 0 to InnerSelection.Items.Count - 1 do
      AClone.Add(CreateDocumentRange(InnerSelection.Items[I]));
    AClone.Delete(Index);
    AGroupedItems := GetGroupedSortedRanges(AClone);
    try
      ValidateSelections(AGroupedItems, True);
    finally
      AGroupedItems.Free;
    end;
  finally
    AClone.Free;
  end;

  FDocument.DocumentModel.BeginUpdate;
  try
    AStartRunIndex := InnerSelection.Items[Index].Interval.NormalizedStart.RunIndex;
    AEndRunIndex := InnerSelection.Items[Index].Interval.NormalizedEnd.RunIndex;
    InnerSelection.Items.Delete(Index);
    FDocument.DocumentModel.ApplyChangesCore(FDocument.DocumentModel.ActivePieceTable,
      DocumentModelChangeActions, AStartRunIndex, AEndRunIndex);
  finally
    FDocument.DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeSelectionCollection.Unselect(const ARange: IdxRichEditDocumentRange);
var
  ACloneCollection: TdxSelectionItemList;
  ARanges: TList<IdxRichEditDocumentRange>;
  AItem: TdxSelectionItem;
  AGroupedItems: TdxObjectList<TList<IdxRichEditDocumentRange>>;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
  I: Integer;
begin
  if InnerSelection.Items.Count = 0 then
    TdxRichEditExceptions.ThrowInternalException;
  if (InnerSelection.Items.Count = 1) and (InnerSelection.Items[0].Length = 0) then
    Exit;
  if ARange.Length = 0 then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionRangeCannotBeEmpty));

  ACloneCollection := CloneCollectionByValue(InnerSelection.Items);
  try
    UnselectCore(ACloneCollection, TdxNativeDocumentRange(ARange));

    ARanges := TList<IdxRichEditDocumentRange>.Create;
    try
      for I := 0 to ACloneCollection.Count - 1 do
        ARanges.Add(CreateDocumentRange(ACloneCollection[I]));

      AGroupedItems := GetGroupedSortedRanges(ARanges);
      try
        ValidateSelections(AGroupedItems, True);

        ARanges.Clear;
        AGroupedItems.Clear;

        FDocument.DocumentModel.BeginUpdate;
        try
          if ACloneCollection.Count = 0 then
          begin
            AStartRunIndex := InnerSelection.Items[0].Interval.NormalizedStart.RunIndex;
            AEndRunIndex := InnerSelection.Items[InnerSelection.Items.Count - 1].Interval.NormalizedEnd.RunIndex;

            InnerSelection.Items.DeleteRange(1, InnerSelection.Items.Count - 1);
            InnerSelection.Items[0].&End := InnerSelection.Items[0].Start;
          end
          else
          begin
            AStartRunIndex := InnerSelection.Items[0].Interval.NormalizedStart.RunIndex;
            AEndRunIndex := InnerSelection.Items[InnerSelection.Items.Count - 1].Interval.NormalizedEnd.RunIndex;

            InnerSelection.Items.Clear;
            while ACloneCollection.Count > 0 do
            begin
              AItem := ACloneCollection[0];
              InnerSelection.AddSelection(AItem);
              ACloneCollection.Extract(AItem);
            end;
          end;
          FDocument.DocumentModel.ApplyChangesCore(FDocument.DocumentModel.ActivePieceTable,
            DocumentModelChangeActions, AStartRunIndex, AEndRunIndex);
        finally
          FDocument.DocumentModel.EndUpdate;
        end;
      finally
        AGroupedItems.Free;
      end;
    finally
      ARanges.Free;
    end;
  finally
    ACloneCollection.Free;
  end;
end;

function TdxNativeSelectionCollection.CreateDocumentRange(const AStart, AEnd: TdxDocumentLogPosition): IdxRichEditDocumentRange;
var
  ANormalStart, ALength: Integer;
begin
  ANormalStart := Min(AStart, AEnd);
  ALength := Abs(AEnd - AStart);
  Result := FDocument.ActiveSubDocument.CreateRange(ANormalStart, ALength);
end;

function TdxNativeSelectionCollection.CreateDocumentRange(const AItem: TdxSelectionItem): IdxRichEditDocumentRange;
begin
  Result := CreateDocumentRange(AItem.Start, AItem.&End);
end;

function TdxNativeSelectionCollection.CreateSelectionItem(AStart, AEnd: TdxDocumentLogPosition): TdxSelectionItem;
begin
  Result := TdxSelectionItem.Create(FDocument.DocumentModel.ActivePieceTable);
  Result.Start := AStart;
  Result.&End := AEnd;
end;

function TdxNativeSelectionCollection.CreateSelectionItem(const ARange: IdxRichEditDocumentRange): TdxSelectionItem;
begin
  Result := CreateSelectionItem(ARange.Start.LogPosition, ARange.&End.LogPosition);
end;

function TdxNativeSelectionCollection.GetCount: Integer;
begin
  Result := InnerSelection.Items.Count;
end;

function TdxNativeSelectionCollection.GetItem(Index: Integer): IdxRichEditDocumentRange;
begin
  Result := CreateDocumentRange(InnerSelection.Items[Index]);
end;

function TdxNativeSelectionCollection.AreSelectionItemsIntersected(
  const AItem1, AItem2: IdxRichEditDocumentRange): Boolean;
var
  ANativeItem1: TdxNativeDocumentRange;
  ANativeItem2: TdxNativeDocumentRange;
begin
  ANativeItem1 := TdxNativeDocumentRange(AItem1);
  ANativeItem2 := TdxNativeDocumentRange(AItem2);
  Result := False;
  if ANativeItem1.NormalizedStart = ANativeItem1.NormalizedEnd then
  begin
    if (ANativeItem2.NormalizedStart.CompareTo(ANativeItem1.NormalizedStart) < 0) and
        (ANativeItem1.NormalizedStart.CompareTo(ANativeItem2.NormalizedEnd) < 0) then
      Result := True;
    Exit;
  end;
  if (ANativeItem1.NormalizedStart.CompareTo(ANativeItem2.NormalizedStart) <= 0) and
      (ANativeItem2.NormalizedStart.CompareTo(ANativeItem1.NormalizedEnd) < 0) then
    Result := True;
  if (ANativeItem1.NormalizedStart.CompareTo(ANativeItem2.NormalizedEnd) < 0) and
      (ANativeItem2.NormalizedEnd.CompareTo(ANativeItem1.NormalizedEnd) <= 0) then
    Result := True;
  if (ANativeItem2.NormalizedStart.CompareTo(ANativeItem1.NormalizedStart) <= 0) and
      (ANativeItem1.NormalizedEnd.CompareTo(ANativeItem2.NormalizedEnd)  <= 0) then
    Result := True;
end;

procedure TdxNativeSelectionCollection.CalcColumnsAndCellsCount(ARow: TdxTableRow; AStartIndexInRow, AEndIndexInRow: Integer;
  out AColumnCount, ACellCount: Integer);
var
  I: Integer;
begin
  AColumnCount := 0;
  for I := AStartIndexInRow to AEndIndexInRow do
    Inc(AColumnCount, ARow.Cells[I].ColumnSpan);
  ACellCount := AEndIndexInRow - AStartIndexInRow;
end;

function TdxNativeSelectionCollection.CloneCollectionByValue(ACollection: TdxSelectionItemList): TdxSelectionItemList;
var
  I: Integer;
  AItem, ACloneItem: TdxSelectionItem;
begin
  Result := TdxSelectionItemList.Create;
  for I := 0 to ACollection.Count - 1 do
  begin
    AItem := ACollection[I];
    ACloneItem := TdxSelectionItem.Create(AItem.PieceTable);
    ACloneItem.Start := AItem.Start;
    ACloneItem.&End := AItem.&End;
    Result.Add(ACloneItem);
  end;
end;

procedure TdxNativeSelectionCollection.CorrectEndLogPosByEndOfDocument(ARanges: TList<IdxRichEditDocumentRange>);
var
  ADocEndLogPos, AStartLogPos, AEndLogPos: TdxDocumentLogPosition;
  I: Integer;
  ARange: IdxRichEditDocumentRange;
begin
  if (ARanges = nil) or (ARanges.Count = 0) then
    Exit;

  ADocEndLogPos := InnerSelection.PieceTable.Paragraphs.Last.EndLogPosition;
  if FDocument.DocumentModel.MainPieceTable = InnerSelection.PieceTable then
    ADocEndLogPos := FDocument.Paragraphs[FDocument.Paragraphs.Count - 1].Range.&End.LogPosition;

  for I := 0 to ARanges.Count - 1 do
  begin
    ARange := ARanges[I];
    if (ADocEndLogPos < ARange.Start.LogPosition) or (ADocEndLogPos < ARange.&End.LogPosition) then
    begin
      if ADocEndLogPos < ARange.Start.LogPosition then
        AStartLogPos := ADocEndLogPos
      else
        AStartLogPos := ARange.Start.LogPosition;
      if ADocEndLogPos < ARange.&End.LogPosition then
        AEndLogPos := ADocEndLogPos
      else
        AEndLogPos := ARange.&End.LogPosition;
      ARanges[I] := CreateDocumentRange(AStartLogPos, AEndLogPos);
    end;
  end;
end;

function TdxNativeSelectionCollection.FillGroup(ASortedRanges: TList<IdxRichEditDocumentRange>;
  APrevRangeIndex: Integer; APrevStartCell: TdxTableCell; ASelectedCellCount: Integer;
  ASelectedColumnCount: Integer; ACurrentGroup: TList<IdxRichEditDocumentRange>): Integer;
var
  APrevCellIndexInRow, ASelectedColumnCountInCurrentRow, ASelectedCellCountInCurrentRow, I, ANormStartCellIndexInRow: Integer;
  ARange: TdxNativeDocumentRange;
  AStartPosCell, AEndPosCell, ANormalizedStartCell, ANormalizedEndCell, ANormalizedPrevStartCell, ANormalizedCurrStartCell: TdxTableCell;
  ADocModelEndPos: TdxDocumentModelPosition;
  AEndPosParagraph: TdxParagraph;
begin
  APrevCellIndexInRow := APrevStartCell.IndexInRow;
  ASelectedColumnCountInCurrentRow := 0;
  ASelectedCellCountInCurrentRow := 0;
  for I := APrevRangeIndex + 1 to ASortedRanges.Count - 1 do
  begin
    ARange := TdxNativeDocumentRange(ASortedRanges[I]);
    AStartPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedStart.LogPosition);
    AEndPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedEnd.LogPosition);

    if AEndPosCell <> nil then
    begin
      if FDocument.DocumentModel.ActivePieceTable.Paragraphs[AEndPosCell.StartParagraphIndex].LogPosition = ARange.NormalizedEnd.LogPosition then
        AEndPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedEnd.LogPosition - 1);
    end
    else
      if not IsEndOfDocument(ARange.NormalizedEnd.LogPosition) then
      begin
        ADocModelEndPos := TdxPositionConverter.ToDocumentModelPosition(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedEnd.LogPosition);
        AEndPosParagraph := FDocument.DocumentModel.ActivePieceTable.Paragraphs[ADocModelEndPos.ParagraphIndex];
        if AEndPosParagraph.LogPosition = ARange.NormalizedEnd.LogPosition then
          AEndPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedEnd.LogPosition - 1);
      end;

    ANormalizedStartCell := nil;
    ANormalizedEndCell := nil;
    NormalizeCells(AStartPosCell, AEndPosCell, ANormalizedStartCell, ANormalizedEndCell);

    if not SameRowCell(ANormalizedStartCell, ANormalizedEndCell) then
      Exit(I);

    ANormalizedPrevStartCell := nil;
    ANormalizedCurrStartCell := nil;
    NormalizeCells(APrevStartCell, ANormalizedStartCell, ANormalizedPrevStartCell, ANormalizedCurrStartCell);

    if ANormalizedPrevStartCell.Table <> ANormalizedCurrStartCell.Table then
      Exit(I);
    if ANormalizedStartCell.RowIndex - APrevStartCell.RowIndex <> 1 then
      Exit(I);

    ANormStartCellIndexInRow := ANormalizedStartCell.IndexInRow;
    CalcColumnsAndCellsCount(ANormalizedStartCell.Row, ANormStartCellIndexInRow, ANormalizedEndCell.IndexInRow, ASelectedColumnCountInCurrentRow, ASelectedCellCountInCurrentRow);
    if (ASelectedCellCount <> ASelectedCellCountInCurrentRow) and (ASelectedColumnCount <> ASelectedColumnCountInCurrentRow) then
      Exit(I);
    if APrevCellIndexInRow <> ANormStartCellIndexInRow then
      Exit(I);

    ACurrentGroup.Add(ARange);
    APrevStartCell := ANormalizedStartCell;
  end;
  Result := ASortedRanges.Count;
end;

procedure TdxNativeSelectionCollection.InternalAdd(
  const ARange: IdxRichEditDocumentRange; ASelElementType: TSelectionRangeType);
var
  AItem: TdxSelectionItem;
begin
  ValidateSelection(ARange, ASelElementType);
  if (InnerSelection.Items.Count = 1) and (InnerSelection.Items[0].Length = 0) then
    InnerSelection.Items.Clear;
  AItem := CreateSelectionItem(ARange);
  try
    InnerSelection.AddSelection(AItem);
  finally
    if InnerSelection.Items.IndexOf(AItem) = -1 then
      AItem.Free;
  end;
end;

function TdxNativeSelectionCollection.IsCellPartiallySelected(ACell: TdxTableCell; APos: TdxDocumentLogPosition): Boolean;
begin
  Result := (ACell <> nil) and
    (ACell.PieceTable.Paragraphs[ACell.StartParagraphIndex].LogPosition <> APos);
end;

function TdxNativeSelectionCollection.IsEndOfDocument(APos: TdxDocumentLogPosition): Boolean;
begin
  Result := FDocument.Paragraphs[FDocument.Paragraphs.Count - 1].Range.&End.LogPosition = APos;
end;

function TdxNativeSelectionCollection.IsLogPositionSelectAnotherCell(ACell: TdxTableCell; APos: TdxDocumentLogPosition): Boolean;
var
  APrevPosCell, ATempCell: TdxTableCell;
begin
  APrevPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, APos - 1);
  if ACell = nil then
    Exit(APrevPosCell <> nil);
  if FDocument.DocumentModel.ActivePieceTable.Paragraphs[ACell.StartParagraphIndex].LogPosition = APos then
    Exit(True);
  ATempCell := nil;
  NormalizeCells(ACell, APrevPosCell, ATempCell, APrevPosCell);
  Result := ACell <> APrevPosCell;
end;

function TdxNativeSelectionCollection.IsParent(AParent, AChild: TdxTableCell): Boolean;
begin
  Result := False;
  if (AParent = AChild) or (AParent.Table = AChild.Table) or
      (AChild.Table.NestedLevel <= AParent.Table.NestedLevel) then
    Exit;

  while AChild.Table.NestedLevel > AParent.Table.NestedLevel do
  begin
    if AChild.Table.ParentCell = AParent then
      Exit(True);
    AChild := AChild.Table.ParentCell;
  end;
end;

function TdxNativeSelectionCollection.GetCell(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition): TdxTableCell;
var
  AModelPos: TdxDocumentModelPosition;
begin
  if IsEndOfDocument(APos) then
    Exit(nil);
  AModelPos := TdxPositionConverter.ToDocumentModelPosition(APieceTable, APos);
  Result := APieceTable.Paragraphs[AModelPos.ParagraphIndex].GetCell;
end;

function TdxNativeSelectionCollection.GetContainerTable(ACell1, ACell2: TdxTableCell): TdxTable;
begin
  Result := nil;
  if (ACell1 = nil) or (ACell2 = nil) then
    Exit;

  if ACell1.Table.NestedLevel <> ACell2.Table.NestedLevel then
  begin
    if ACell1.Table.NestedLevel < ACell2.Table.NestedLevel then
      ACell2 := GetParentCellWithNestedLevel(ACell2, ACell1.Table.NestedLevel)
    else
      ACell1 := GetParentCellWithNestedLevel(ACell1, ACell2.Table.NestedLevel);
  end;

  while ACell1.Table <> ACell2.Table do
  begin
    if ACell1.Table.ParentCell = nil then
      Break;
    ACell1 := ACell1.Table.ParentCell;
    ACell2 := ACell2.Table.ParentCell;
  end;

  if ACell1.Table = ACell2.Table then
    Result := ACell1.Table;
end;

function TdxNativeSelectionCollection.GetFirstNormalizedCellInRow(ARow: TdxTableRow): TdxTableCell;
begin
  if ARow = nil then
    Exit(nil);
  Result := ARow.FirstCell;
  while Result <> nil do
  begin
    if Result.VerticalMerging <> TdxMergingState.Continue then
      Exit(Result);
    Result := Result.NextCellInRow;
  end;
  Result := nil;
end;

function TdxNativeSelectionCollection.GetGroupedSortedRanges(ARanges: TList<IdxRichEditDocumentRange>): TdxObjectList<TList<IdxRichEditDocumentRange>>;
var
  ASortedRanges, ASortedMergedRanges, ASortedCorrectedRanges, ACurrentGroup: TList<IdxRichEditDocumentRange>;
  ASelectedCellsCount, ASelectedColumnsCount, I: Integer;
  ARange: TdxNativeDocumentRange;
  AStartPosCell, AEndPosCell, ANormalizedStartCell, ANormalizedEndCell: TdxTableCell;
  ADocModelEndPos: TdxDocumentModelPosition;
  AEndPosParagraph: TdxParagraph;
begin
  ASortedRanges := SortRanges(ARanges);
  try
    CorrectEndLogPosByEndOfDocument(ASortedRanges);
    ASortedMergedRanges := MergeNeighbourhoodRanges(ASortedRanges);
    try
      RemoveRangesWithZeroWidth(ASortedMergedRanges);
      ASortedCorrectedRanges := SplitRanges(ASortedMergedRanges);
      try
        Result := TdxObjectList<TList<IdxRichEditDocumentRange>>.Create;
        ASelectedCellsCount := 0;
        ASelectedColumnsCount := 0;
        I := 0;
        while I < ASortedCorrectedRanges.Count do
        begin
          ARange := TdxNativeDocumentRange(ASortedCorrectedRanges[I]);
          AStartPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedStart.LogPosition);
          AEndPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedEnd.LogPosition);

          if AEndPosCell <> nil then
          begin
            if FDocument.DocumentModel.ActivePieceTable.Paragraphs[AEndPosCell.StartParagraphIndex].LogPosition = ARange.NormalizedEnd.LogPosition then
              AEndPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedEnd.LogPosition - 1);
          end
          else
            if not IsEndOfDocument(ARange.NormalizedEnd.LogPosition) then
            begin
              ADocModelEndPos := TdxPositionConverter.ToDocumentModelPosition(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedEnd.LogPosition);
              AEndPosParagraph := FDocument.DocumentModel.ActivePieceTable.Paragraphs[ADocModelEndPos.ParagraphIndex];
              if AEndPosParagraph.LogPosition = ARange.NormalizedEnd.LogPosition then
                AEndPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedEnd.LogPosition - 1);
            end;

          ANormalizedStartCell := nil;
          ANormalizedEndCell := nil;
          NormalizeCells(AStartPosCell, AEndPosCell, ANormalizedStartCell, ANormalizedEndCell);
          ACurrentGroup := TList<IdxRichEditDocumentRange>.Create;
          ACurrentGroup.Add(ARange);
          Result.Add(ACurrentGroup);

          if not SameRowCell(ANormalizedStartCell, ANormalizedEndCell) then
          begin
            Inc(I);
            Continue;
          end;
          CalcColumnsAndCellsCount(ANormalizedStartCell.Row, ANormalizedStartCell.IndexInRow, ANormalizedEndCell.IndexInRow, ASelectedColumnsCount, ASelectedCellsCount);
          I := FillGroup(ASortedCorrectedRanges, I, ANormalizedStartCell, ASelectedCellsCount, ASelectedColumnsCount, ACurrentGroup);
        end;
      finally
        ASortedCorrectedRanges.Free;
      end;
    finally
      ASortedMergedRanges.Free;
    end;
  finally
    ASortedRanges.Free;
  end;
end;

function TdxNativeSelectionCollection.GetInnerSelection: TdxSelection;
begin
  Result := FDocument.DocumentModel.Selection;
end;

function TdxNativeSelectionCollection.GetLastSelectedCell(AStartCell, AEndCell: TdxTableCell; AEndLogPosition: TdxDocumentLogPosition): TdxTableCell;
var
  ALastSelectedCell, ATempCell: TdxTableCell;
begin
  if AEndCell = nil then
    Exit(GetCell(FDocument.DocumentModel.ActivePieceTable, AEndLogPosition - 1));

  ALastSelectedCell := nil;
  if not IsLogPositionSelectAnotherCell(AEndCell, AEndLogPosition) then
    Exit(AEndCell);

  ALastSelectedCell := GetCell(FDocument.DocumentModel.ActivePieceTable, AEndLogPosition - 1);
  if (ALastSelectedCell <> nil) and (AStartCell <> nil) then
  begin
    ATempCell := nil;
    NormalizeCells(AStartCell, ALastSelectedCell, ATempCell, ALastSelectedCell);
  end;
  Result := ALastSelectedCell;
end;

function TdxNativeSelectionCollection.GetLastNormalizedCellInRow(ARow: TdxTableRow): TdxTableCell;
begin
  if ARow = nil then
    Exit(nil);
  Result := ARow.LastCell;
  while Result <> nil do
  begin
    if Result.VerticalMerging <> TdxMergingState.Continue then
      Exit(Result);
    Result := GetPreviousCellInRow(Result);
  end;
  Result := nil;
end;

function TdxNativeSelectionCollection.GetParentCellWithNestedLevel(ACell: TdxTableCell; ANestedLevel: Integer): TdxTableCell;
begin
  Result := nil;
  if (ACell = nil) or (ANestedLevel < 0) then
    Exit;
  if ACell.Table.NestedLevel < ANestedLevel then
    Exit;
  while ACell.Table.NestedLevel > ANestedLevel do
    ACell := ACell.Table.ParentCell;
  Result := ACell;
end;

function TdxNativeSelectionCollection.GetPreviousCellInRow(ACell: TdxTableCell): TdxTableCell;
begin
  if ACell = nil then
    Exit(nil);
  Result := ACell.Previous;
  if (Result <> nil) and (Result.Row = ACell.Row) then
    Exit;
  Result := nil;
end;

function TdxNativeSelectionCollection.MergeNeighbourhoodRanges(ASortedRanges: TList<IdxRichEditDocumentRange>): TList<IdxRichEditDocumentRange>;
var
  APrevRange, ARange: IdxRichEditDocumentRange;
  I: Integer;
begin
  Result := TList<IdxRichEditDocumentRange>.Create;
  APrevRange := nil;
  for I := 0 to ASortedRanges.Count - 1 do
  begin
    if APrevRange <> nil then
    begin
      ARange := ASortedRanges[I];
      if TdxNativeDocumentRange(APrevRange).NormalizedEnd.CompareTo(TdxNativeDocumentRange(ARange).NormalizedStart) = 0 then
      begin
        APrevRange := CreateDocumentRange(TdxNativeDocumentRange(APrevRange).NormalizedStart.LogPosition,
          TdxNativeDocumentRange(ARange).NormalizedEnd.LogPosition);
        Continue;
      end
      else
        Result.Add(APrevRange);
    end;
    APrevRange := ASortedRanges[I];
  end;
  if APrevRange <> nil then
    Result.Add(APrevRange);
end;

procedure TdxNativeSelectionCollection.NormalizeCells(ACell1, ACell2: TdxTableCell; out ANormalizedCell1, ANormalizedCell2: TdxTableCell);
var
  ABaseParentTable: TdxTable;
begin
  if (ACell1 = nil) and (ACell2 = nil) then
  begin
    ANormalizedCell1 := nil;
    ANormalizedCell2 := nil;
    Exit;
  end;
  if (ACell1 <> nil) and (ACell2 = nil) then
  begin
    ANormalizedCell1 := GetParentCellWithNestedLevel(ACell1, 0);
    ANormalizedCell2 := nil;
    Exit;
  end;
  if (ACell1 = nil) and (ACell2 <> nil) then
  begin
    ANormalizedCell1 := nil;
    ANormalizedCell2 := GetParentCellWithNestedLevel(ACell2, 0);
    Exit;
  end;

  if ACell1.Table = ACell2.Table then
  begin
    ANormalizedCell1 := ACell1;
    ANormalizedCell2 := ACell2;
    Exit;
  end;

  ABaseParentTable := GetContainerTable(ACell1, ACell2);
  if ABaseParentTable <> nil then
  begin
    if IsParent(ACell1, ACell2) then
    begin
      ANormalizedCell1 := ACell1;
      ANormalizedCell2 := GetParentCellWithNestedLevel(ACell2, ACell1.Table.NestedLevel + 1);
    end
    else
      if IsParent(ACell2, ACell1) then
      begin
        ANormalizedCell2 := ACell2;
        ANormalizedCell1 := GetParentCellWithNestedLevel(ACell1, ACell2.Table.NestedLevel + 1);
      end
      else
      begin
        ANormalizedCell1 := GetParentCellWithNestedLevel(ACell1, ABaseParentTable.NestedLevel);
        ANormalizedCell2 := GetParentCellWithNestedLevel(ACell2, ABaseParentTable.NestedLevel);
        if ANormalizedCell1 = ANormalizedCell2 then
        begin
          ANormalizedCell1 := GetParentCellWithNestedLevel(ACell1, ABaseParentTable.NestedLevel + 1);
          ANormalizedCell2 := GetParentCellWithNestedLevel(ACell2, ABaseParentTable.NestedLevel + 1);
        end;
      end;
    Exit;
  end;

  ANormalizedCell1 := GetParentCellWithNestedLevel(ACell1, 0);
  ANormalizedCell2 := GetParentCellWithNestedLevel(ACell2, 0);
end;

procedure TdxNativeSelectionCollection.RemoveRangesWithZeroWidth(ARanges: TList<IdxRichEditDocumentRange>);
var
  I: Integer;
begin
  if (ARanges = nil) or (ARanges.Count = 0) then
    Exit;
  for I := ARanges.Count - 1 downto 0 do
  begin
    if ARanges[I].Length = 0 then
      ARanges.Delete(I);
  end;
end;

function TdxNativeSelectionCollection.SameRowCell(ACell1, ACell2: TdxTableCell): Boolean;
begin
  Result := (ACell1 <> nil) and (ACell2 <> nil) and (ACell1.Row = ACell2.Row);
end;

function TdxNativeSelectionCollection.ShouldSkipValidation(AStart, AEnd, ALastSelected: TdxTableCell): Boolean;
begin
  Result := True;
  if (AStart = nil) and (AEnd = nil) and (ALastSelected = nil) then
    Exit;
  if (AStart = ALastSelected) and (AEnd <> nil) and (AEnd.Table.ParentCell = AStart) then
    Exit;
  Result := False;
end;

function TdxNativeSelectionCollection.SortRanges(AItems: TList<IdxRichEditDocumentRange>): TList<IdxRichEditDocumentRange>;
var
  AComparer: IComparer<IdxRichEditDocumentRange>;
begin
  Result := TList<IdxRichEditDocumentRange>.Create(AItems);
  AComparer := TdxRangeComparer.Create;
  Result.Sort(AComparer);
end;

function TdxNativeSelectionCollection.SplitRanges(ASortedRanges: TList<IdxRichEditDocumentRange>): TList<IdxRichEditDocumentRange>;
var
  AResult, APartiallySelectedCells, ASplittedRangesByRow: TList<IdxRichEditDocumentRange>;
  I: Integer;
  ARange: TdxNativeDocumentRange;
  AFirstRangePart, ASecondRangePart: IdxRichEditDocumentRange;
  AStartCell, AEndCell, ANormalStartCell, ANormalEndCell, ALastSelectedCell: TdxTableCell;
  AEndLogPos, AEndCellStartLogPos: TdxDocumentLogPosition;
begin
  AResult := TList<IdxRichEditDocumentRange>.Create;
  try
    APartiallySelectedCells := TList<IdxRichEditDocumentRange>.Create;
    try
      ASplittedRangesByRow := SplitRangesByRowWithChildTables(ASortedRanges);
      try
        repeat
          APartiallySelectedCells.Clear;
          for I := 0 to ASplittedRangesByRow.Count - 1 do
          begin
            ARange := TdxNativeDocumentRange(ASplittedRangesByRow[I]);
            AStartCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedStart.LogPosition);
            AEndCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedEnd.LogPosition);

            ANormalStartCell := nil;
            ANormalEndCell := nil;
            NormalizeCells(AStartCell, AEndCell, ANormalStartCell, ANormalEndCell);

            if (ANormalStartCell = nil) or (ANormalEndCell = nil) or (ANormalStartCell = ANormalEndCell) then
            begin
              AResult.Add(ARange);
              Continue;
            end;

            AEndLogPos := ARange.NormalizedEnd.LogPosition;

            ALastSelectedCell := GetLastSelectedCell(ANormalStartCell, ANormalEndCell, ARange.NormalizedEnd.LogPosition);
            if ANormalEndCell <> ALastSelectedCell then
            begin
              AResult.Add(ARange);
              Continue;
            end;

            AEndCellStartLogPos := FDocument.DocumentModel.ActivePieceTable.Paragraphs[ANormalEndCell.StartParagraphIndex].LogPosition;
            AFirstRangePart := CreateDocumentRange(ARange.NormalizedStart.LogPosition, AEndCellStartLogPos);
            ASecondRangePart := CreateDocumentRange(AEndCellStartLogPos, AEndLogPos);
            AResult.Add(AFirstRangePart);
            APartiallySelectedCells.Add(ASecondRangePart);
          end;
          ASplittedRangesByRow.Free;
          ASplittedRangesByRow := SplitRangesByRowWithChildTables(APartiallySelectedCells);
        until not (APartiallySelectedCells.Count <> 0);
      finally
        ASplittedRangesByRow.Free;
      end;

      Result := SortRanges(AResult);
    finally
      APartiallySelectedCells.Free;
    end;
  finally
    AResult.Free;
  end;
end;

function TdxNativeSelectionCollection.SplitRangesByRowWithChildTables(ASortedRanges: TList<IdxRichEditDocumentRange>): TList<IdxRichEditDocumentRange>;
var
  APrevRangeCount: Integer;
  AResult: TList<IdxRichEditDocumentRange>;
begin
  Result := TList<IdxRichEditDocumentRange>.Create(ASortedRanges);
  repeat
    APrevRangeCount := Result.Count;
    AResult := SplitRangesByRowSameTable(Result);
    Result.Free;
    Result := AResult;
  until not (APrevRangeCount <> Result.Count);
end;

function TdxNativeSelectionCollection.SplitRangesByRowSameTable(ASortedRanges: TList<IdxRichEditDocumentRange>): TList<IdxRichEditDocumentRange>;
var
  I, AStartCellRowIndex, AEndCellRowIndex: Integer;
  ARange: TdxNativeDocumentRange;
  AStartCell, AEndCell, ANormalStartCell, ANormalEndCell, ALastSelectedCell, ALastRangeFirstCell: TdxTableCell;
  AFirstTableRows, ATableRows: TdxTableRowCollection;
  AEndLogPos: TdxDocumentLogPosition;
  ANewRange, ALastRange: IdxRichEditDocumentRange;
begin
  Result := TList<IdxRichEditDocumentRange>.Create;
  for I := 0 to ASortedRanges.Count - 1 do
  begin
    ARange := TdxNativeDocumentRange(ASortedRanges[I]);
    AStartCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedStart.LogPosition);
    AEndCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ARange.NormalizedEnd.LogPosition);

    if AStartCell = AEndCell then
    begin
      Result.Add(ARange);
      Continue;
    end;

    ANormalStartCell := nil;
    ANormalEndCell := nil;
    NormalizeCells(AStartCell, AEndCell, ANormalStartCell, ANormalEndCell);

    ALastSelectedCell := GetLastSelectedCell(ANormalStartCell, ANormalEndCell, ARange.NormalizedEnd.LogPosition);

    if ANormalStartCell = ALastSelectedCell then
    begin
      Result.Add(ARange);
      Continue;
    end;

    if (ANormalStartCell <> nil) and (ALastSelectedCell <> nil) then
    begin
      if ANormalStartCell.Table = ALastSelectedCell.Table then
      begin
        AStartCellRowIndex := ANormalStartCell.RowIndex;
        AEndCellRowIndex := ALastSelectedCell.RowIndex;
        if AStartCellRowIndex <> AEndCellRowIndex then
        begin
          SplitRows(ARange, ANormalStartCell.Table, AStartCellRowIndex + 1, AEndCellRowIndex, Result);
          Continue;
        end;
      end
      else
        if ANormalStartCell.Table.ParentCell = ALastSelectedCell.Table.ParentCell then
        begin
          AStartCellRowIndex := ANormalStartCell.RowIndex;
          AFirstTableRows := ANormalStartCell.Table.Rows;
          SplitRows(ARange, ANormalStartCell.Table, AStartCellRowIndex + 1, AFirstTableRows.Count - 1, Result);

          repeat
            ALastRange := TdxNativeDocumentRange(Result[Result.Count - 1]);
            Result.Delete(Result.Count - 1);
            ALastRangeFirstCell := GetCell(FDocument.DocumentModel.ActivePieceTable, TdxNativeDocumentRange(ALastRange).NormalizedStart.LogPosition);
            if (ALastRangeFirstCell = nil) or (ALastRangeFirstCell.Table.NestedLevel < ANormalStartCell.Table.NestedLevel) then
            begin
              AEndLogPos := FDocument.DocumentModel.ActivePieceTable.Paragraphs[ALastSelectedCell.Table.FirstRow.FirstCell.StartParagraphIndex].LogPosition;
              ANewRange := CreateDocumentRange(TdxNativeDocumentRange(ALastRange).NormalizedStart.LogPosition, AEndLogPos);
              Result.Add(ANewRange);
              ALastRange := CreateDocumentRange(AEndLogPos, TdxNativeDocumentRange(ALastRange).NormalizedEnd.LogPosition);
              ALastRangeFirstCell := ALastSelectedCell;
            end
            else
              ALastRangeFirstCell := GetParentCellWithNestedLevel(ALastRangeFirstCell, ANormalStartCell.Table.NestedLevel);
            if ALastRangeFirstCell.Table = ALastSelectedCell.Table then
              AEndCellRowIndex := ALastSelectedCell.RowIndex
            else
              AEndCellRowIndex := ALastRangeFirstCell.Table.Rows.Count - 1;
            SplitRows(TdxNativeDocumentRange(ALastRange), ALastRangeFirstCell.Table, 1, AEndCellRowIndex, Result);
          until not (ALastRangeFirstCell.Table <> ALastSelectedCell.Table);
          Continue;
        end
        else
          if ANormalStartCell = ALastSelectedCell.Table.ParentCell then
          begin
            SplitRows(ARange, ALastSelectedCell.Table, 0, ALastSelectedCell.RowIndex, Result);
            Continue;
          end
          else
            if ANormalStartCell.Table.ParentCell = ALastSelectedCell then
            begin
              AStartCellRowIndex := ANormalStartCell.RowIndex;
              ATableRows := ANormalStartCell.Table.Rows;
              SplitRows(ARange, ANormalStartCell.Table, AStartCellRowIndex + 1, ATableRows.Count - 1, Result);
              Continue;
            end;
    end
    else
      if (ANormalStartCell <> nil) and (ALastSelectedCell = nil) then
      begin
        AStartCellRowIndex := ANormalStartCell.RowIndex;
        ATableRows := ANormalStartCell.Table.Rows;
        SplitRows(ARange, ANormalStartCell.Table, AStartCellRowIndex + 1, ATableRows.Count - 1, Result);
        Continue;
      end
      else
        if (ANormalStartCell = nil) and (ALastSelectedCell <> nil) then
        begin
          SplitRows(ARange, ALastSelectedCell.Table, 0, ALastSelectedCell.RowIndex, Result);
          Continue;
        end;

    Result.Add(ARange);
  end;
end;

procedure TdxNativeSelectionCollection.SplitRows(ARange: TdxNativeDocumentRange; ATable: TdxTable; AStartRowIndex, AEndRowIndex: Integer; ARanges: TList<IdxRichEditDocumentRange>);
var
  ARows: TdxTableRowCollection;
  AStartLogPos, AEndLogPos, AAfterTableLogPos: TdxDocumentLogPosition;
  I: Integer;
  ARow: TdxTableRow;
  ANewRange, ALastRange: IdxRichEditDocumentRange;
begin
  ARows := ATable.Rows;
  AStartLogPos := ARange.NormalizedStart.LogPosition;
  for I := AStartRowIndex to AEndRowIndex do
  begin
    ARow := ARows[I];
    AEndLogPos := FDocument.DocumentModel.ActivePieceTable.Paragraphs[ARow.FirstCell.StartParagraphIndex].LogPosition;
    ANewRange := CreateDocumentRange(AStartLogPos, AEndLogPos);
    ARanges.Add(ANewRange);
    AStartLogPos := AEndLogPos;
  end;
  if AStartLogPos <> ARange.NormalizedEnd.LogPosition then
  begin
    AAfterTableLogPos := FDocument.DocumentModel.ActivePieceTable.Paragraphs[ARows.Last.LastCell.EndParagraphIndex].EndLogPosition + 1;
    if AAfterTableLogPos < ARange.NormalizedEnd.LogPosition then
    begin
      ANewRange := CreateDocumentRange(AStartLogPos, AAfterTableLogPos);
      ARanges.Add(ANewRange);
      AStartLogPos := AAfterTableLogPos;
    end;
    ALastRange := CreateDocumentRange(AStartLogPos, ARange.NormalizedEnd.LogPosition);
    ARanges.Add(ALastRange);
  end;
end;

procedure TdxNativeSelectionCollection.UnselectCore(ACloneCollection: TdxSelectionItemList; ARange: TdxNativeDocumentRange);
var
  I: Integer;
  ASel, ANewSelItem: TdxSelectionItem;
  AOldSelEnd: Integer;
begin
  for I := ACloneCollection.Count - 1 downto 0 do
  begin
    ASel := ACloneCollection[I];
    if ASel.Length = 0 then
      Continue;
    if (ASel.NormalizedStart >= ARange.NormalizedStart.LogPosition) and
      (ASel.NormalizedEnd <= ARange.NormalizedEnd.LogPosition) then
    begin
      ACloneCollection.Remove(ASel);
      Continue;
    end
    else
      if (ASel.NormalizedStart < ARange.NormalizedStart.LogPosition) and
        (ASel.NormalizedEnd > ARange.NormalizedEnd.LogPosition) then
      begin
        if ASel.Start < ASel.&End then
        begin
          AOldSelEnd := ASel.&End;
          ASel.&End := ARange.NormalizedStart.LogPosition;
          ANewSelItem := CreateSelectionItem(ARange.NormalizedEnd.LogPosition, AOldSelEnd);
          ACloneCollection.Insert(I + 1, ANewSelItem);
        end
        else
          if ASel.Start > ASel.&End then
          begin
            AOldSelEnd := ASel.&End;
            ASel.&End := ARange.NormalizedEnd.LogPosition;
            ANewSelItem := CreateSelectionItem(ARange.NormalizedStart.LogPosition, AOldSelEnd);
            ACloneCollection.Insert(I + 1, ANewSelItem);
          end;
      end
      else
        if (ASel.NormalizedStart < ARange.NormalizedEnd.LogPosition) and
          (ASel.NormalizedStart >= ARange.NormalizedStart.LogPosition) then
        begin
          if ASel.Start < ASel.&End then
            ASel.Start := ARange.NormalizedEnd.LogPosition
          else
            if ASel.Start > ASel.&End then
              ASel.&End := ARange.NormalizedEnd.LogPosition;
        end
        else
          if (ASel.NormalizedEnd > ARange.NormalizedStart.LogPosition) and
            (ASel.NormalizedEnd <= ARange.NormalizedEnd.LogPosition) then
          begin
            if ASel.Start < ASel.&End then
              ASel.&End := ARange.NormalizedStart.LogPosition
            else
              if ASel.Start > ASel.&End then
                ASel.Start := ARange.NormalizedStart.LogPosition;
          end;
  end;
end;

procedure TdxNativeSelectionCollection.ValidateSelection(const ARange: IdxRichEditDocumentRange;
  ASelElementType: TSelectionRangeType; AIsRemoved: Boolean = False);
var
  ASelectionItem: TdxSelectionItem;
  ASelRange: IdxRichEditDocumentRange;
  AStart, AEnd: TdxDocumentModelPosition;
  I: Integer;
begin
  if (ARange.Start = ARange.&End) and (InnerSelection.Items.Count <> 0) then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionSelectionShouldContainAtLeastOneCharacter));

  if not AIsRemoved then
  begin
    for I := 0 to InnerSelection.Items.Count - 1 do
    begin
      ASelectionItem := InnerSelection.Items[I];
      if ASelectionItem.Length = 0 then
        Continue;
      AStart := TdxDocumentModelPosition.Create(ASelectionItem.PieceTable);
      AStart.LogPosition := ASelectionItem.NormalizedStart;
      AEnd := TdxDocumentModelPosition.Create(ASelectionItem.PieceTable);
      AEnd.LogPosition := ASelectionItem.NormalizedEnd;
      ASelRange := TdxNativeDocumentRange.Create(FDocument, AStart, AEnd);
      if AreSelectionItemsIntersected(ASelRange, ARange) then
        TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionCurrentSelectionAndSpecifiedSelectionIntersect));
    end;
  end;

  ValidateSelectionCells(ARange, ASelElementType);
end;

procedure TdxNativeSelectionCollection.ValidateSelections(AGroupedItems: TdxObjectList<TList<IdxRichEditDocumentRange>>; AIsRemoved: Boolean = False);
var
  I, J, K, L: Integer;
  ASelections, ASelections2: TList<IdxRichEditDocumentRange>;
  AItem, AItem2: IdxRichEditDocumentRange;
  ASelElementType: TSelectionRangeType;
begin
  for I := 0 to AGroupedItems.Count - 1 do
  begin
    ASelections := AGroupedItems[I];
    for J := 0 to ASelections.Count - 1 do
    begin
      AItem := ASelections[J];

      for K := J + 1 to ASelections.Count - 1 do
      begin
        AItem2 := ASelections[K];
        if AreSelectionItemsIntersected(TdxNativeDocumentRange(AItem), TdxNativeDocumentRange(AItem2)) then
          TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionSpecifiedSelectionsIntersect));
      end;

      for K := I + 1 to AGroupedItems.Count - 1 do
      begin
        ASelections2 := AGroupedItems[K];
        for L := 0 to ASelections2.Count - 1 do
        begin
          AItem2 := ASelections2[L];
          if AreSelectionItemsIntersected(TdxNativeDocumentRange(AItem), TdxNativeDocumentRange(AItem2)) then
            TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionSpecifiedSelectionsIntersect));
        end;
      end;

      ASelElementType := TSelectionRangeType.Single;
      if ASelections.Count <> 1 then
        if J = 0 then
          ASelElementType := TSelectionRangeType.Start
        else
          if J = ASelections.Count - 1 then
            ASelElementType := TSelectionRangeType.&End
          else
            ASelElementType := TSelectionRangeType.Middle;
      ValidateSelection(AItem, ASelElementType, AIsRemoved);
    end;
  end;
end;

procedure TdxNativeSelectionCollection.ValidateSelectionCells(const ARange: IdxRichEditDocumentRange; ASelElementType: TSelectionRangeType);
var
  ANativeRange: TdxNativeDocumentRange;
  AStartLogPos, AEndLogPos: TdxDocumentLogPosition;
  ANormalizedStartCell, ANormalizedEndCell, ALastSelectedCell: TdxTableCell;
  AStartPosCell, AEndPosCell: TdxTableCell;
begin
  ANativeRange := TdxNativeDocumentRange(ARange);
  AStartLogPos := ANativeRange.NormalizedStart.LogPosition;
  AEndLogPos := ANativeRange.NormalizedEnd.LogPosition;

  AStartPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, AStartLogPos);
  AEndPosCell := GetCell(FDocument.DocumentModel.ActivePieceTable, AEndLogPos);

  if AStartPosCell = AEndPosCell then
    Exit;

  ANormalizedStartCell := nil;
  ANormalizedEndCell := nil;
  NormalizeCells(AStartPosCell, AEndPosCell, ANormalizedStartCell, ANormalizedEndCell);

  ALastSelectedCell := GetLastSelectedCell(ANormalizedStartCell, ANormalizedEndCell, AEndLogPos);

  if ShouldSkipValidation(ANormalizedStartCell, ANormalizedEndCell, ALastSelectedCell) then
    Exit;

  if ANormalizedEndCell <> ALastSelectedCell then
    ANormalizedEndCell := nil;

  if (ANormalizedStartCell <> nil) and (ALastSelectedCell <> nil) then
  begin
    if ANormalizedStartCell.Table = ALastSelectedCell.Table then
      ValidateCellsWithinSameTable(ANormalizedStartCell, ANormalizedEndCell, AStartLogPos, AEndLogPos, ASelElementType)
    else
      if ANormalizedStartCell.Table.ParentCell = ALastSelectedCell.Table.ParentCell then
        ValidateCellsWithinDifferentTables(ANormalizedStartCell, ANormalizedEndCell, AStartLogPos, AEndLogPos)
      else
        if ANormalizedStartCell = ALastSelectedCell.Table.ParentCell then
          ValidateTableCellTextBeforeTable(ANormalizedEndCell, AEndLogPos)
        else
          if ANormalizedStartCell.Table.ParentCell = ALastSelectedCell then
            ValidateTableCellTextAfterTable(ANormalizedStartCell, AStartLogPos);
  end
  else
    if (ANormalizedStartCell <> nil) and (ALastSelectedCell = nil) then
      ValidateTableCellTextAfterTable(ANormalizedStartCell, AStartLogPos)
    else
      if (ANormalizedStartCell = nil) and (ALastSelectedCell <> nil) then
        ValidateTableCellTextBeforeTable(ANormalizedEndCell, AEndLogPos);
end;

procedure TdxNativeSelectionCollection.ValidateCellsWithinSameTable(AFirstSelectedCell, ALastSelectedCell: TdxTableCell;
  ASelStartLogPosition, ASelEndLogPosition: TdxDocumentLogPosition; ASelElementType: TSelectionRangeType);
var
  AIsStartCellPartiallySelected, AIsEndCellPartiallySelected: Boolean;
  ALastTableCell: TdxTableCell;
  AStartCellRowIndex, AEndCellRowIndex: Integer;
begin
  AIsStartCellPartiallySelected := IsCellPartiallySelected(AFirstSelectedCell, ASelStartLogPosition);
  ALastTableCell := nil;
  if ALastSelectedCell = nil then
    ALastTableCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ASelEndLogPosition - 1);
  if ALastSelectedCell = nil then
    AIsEndCellPartiallySelected := False
  else
    AIsEndCellPartiallySelected := IsCellPartiallySelected(ALastSelectedCell, ASelEndLogPosition);
  AStartCellRowIndex := AFirstSelectedCell.RowIndex;
  if ALastSelectedCell = nil then
    AEndCellRowIndex := ALastTableCell.RowIndex
  else
    AEndCellRowIndex := ALastSelectedCell.RowIndex;
  if (ALastSelectedCell <> nil) and ALastSelectedCell.IsFirstCellInRow and not AIsEndCellPartiallySelected then
    Dec(AEndCellRowIndex);
  if AStartCellRowIndex <> AEndCellRowIndex then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionSelectionShouldIncludeNotMoreThanOneRow));
  if (AFirstSelectedCell.VerticalMerging = TdxMergingState.Continue) and
      (ASelElementType in [TSelectionRangeType.Start, TSelectionRangeType.Single]) then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionFirstCellContinuesVerticalMerge));
  if AIsStartCellPartiallySelected or AIsEndCellPartiallySelected then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionPartiallySelectedCells));
  if ALastSelectedCell <> nil then
  begin
    if (ALastSelectedCell.Previous.VerticalMerging = TdxMergingState.Continue) and
        (ASelElementType in [TSelectionRangeType.&End, TSelectionRangeType.Single]) then
      TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionLastCellContinuesVerticalMerge));
  end
  else
    if ALastSelectedCell = nil then
    begin
      if (ALastTableCell.VerticalMerging = TdxMergingState.Continue) and (ASelElementType in [TSelectionRangeType.&End, TSelectionRangeType.Single]) then
        TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionLastCellContinuesVerticalMerge));
    end;
end;

procedure TdxNativeSelectionCollection.ValidateCellsWithinDifferentTables(AFirstSelectedCell, ALastSelectedCell: TdxTableCell;
  ASelStartLogPosition, ASelEndLogPosition: TdxDocumentLogPosition);
var
  ACellForNormalize: TdxTableCell;
begin
  ValidateTableCellTextAfterTable(AFirstSelectedCell, ASelStartLogPosition);
  if ALastSelectedCell = nil then
    ACellForNormalize := AFirstSelectedCell
  else
    ACellForNormalize := nil;
  ValidateTableCellTextBeforeTable(ALastSelectedCell, ASelEndLogPosition, ACellForNormalize);
end;

procedure TdxNativeSelectionCollection.ValidateTableCellTextBeforeTable(ASelectedCell: TdxTableCell;
  ASelEndLogPosition: TdxDocumentLogPosition; ACellForNormalize: TdxTableCell = nil);
var
  ALastTableCell, ANormCell, ALastNormCellInRow: TdxTableCell;
  AIsSelectedCellPartiallySelected: Boolean;
begin
  ALastTableCell := nil;
  if ASelectedCell = nil then
  begin
    ALastTableCell := GetCell(FDocument.DocumentModel.ActivePieceTable, ASelEndLogPosition - 1);
    if ACellForNormalize <> nil then
    begin
      ANormCell := nil;
      NormalizeCells(ACellForNormalize, ALastTableCell, ANormCell, ALastTableCell);
    end;
  end;
  if ASelectedCell = nil then
    AIsSelectedCellPartiallySelected := False
  else
    AIsSelectedCellPartiallySelected := IsCellPartiallySelected(ASelectedCell, ASelEndLogPosition);
  if AIsSelectedCellPartiallySelected then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionPartiallySelectedCells));
  if ASelectedCell <> nil then
    ALastNormCellInRow := GetLastNormalizedCellInRow(ASelectedCell.Previous.Row)
  else
    ALastNormCellInRow := GetLastNormalizedCellInRow(ALastTableCell.Row);
  if ASelectedCell <> nil then
  begin
    if ASelectedCell.Previous.VerticalMerging = TdxMergingState.Continue then
      TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionLastCellContinuesVerticalMerge));
    if ALastNormCellInRow <> ASelectedCell.Previous then
      TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionSelectionExtendsOutsideTable));
  end
  else
  begin
    if ALastTableCell.VerticalMerging = TdxMergingState.Continue then
      TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionLastCellContinuesVerticalMerge));
    if ALastNormCellInRow <> ALastTableCell then
      TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionSelectionExtendsOutsideTable));
  end;
end;

procedure TdxNativeSelectionCollection.ValidateTableCellTextAfterTable(ASelectedCell: TdxTableCell;
  ASelStartLogPosition: TdxDocumentLogPosition);
var
  AFirstNormalCellInRow: TdxTableCell;
  AIsCellPartiallySel: Boolean;
begin
  AFirstNormalCellInRow := GetFirstNormalizedCellInRow(ASelectedCell.Row);
  AIsCellPartiallySel := IsCellPartiallySelected(ASelectedCell, ASelStartLogPosition);
  if AFirstNormalCellInRow <> ASelectedCell then
  begin
    if ASelectedCell.VerticalMerging = TdxMergingState.Continue then
      TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionFirstCellContinuesVerticalMerge));
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionSelectionExtendsOutsideTable));
  end;
  if AIsCellPartiallySel then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionPartiallySelectedCells));
end;

{ TdxNativeDocument }

function TdxNativeDocument.GetIsDocumentProtected: Boolean;
begin
  Result := DocumentModel.ProtectionProperties.EnforceProtection;
end;

function TdxNativeDocument.GetDefaultParagraphProperties: IdxRichEditParagraphPropertiesBase;
begin
  CheckValid;
  Result := FDefaultParagraphProperties;
end;

function TdxNativeDocument.GetDefaultCharacterProperties: IdxRichEditCharacterPropertiesBase;
begin
  CheckValid;
  Result := FDefaultCharacterProperties;
end;

function TdxNativeDocument.GetDefaultTableProperties: IdxRichEditTablePropertiesBase;
begin
  CheckValid;
  Result := FDefaultTableProperties;
end;

function TdxNativeDocument.GetRtfText: string;
begin
  Result := InternalAPI.RtfText;
end;

procedure TdxNativeDocument.SetRtfText(const Value: string);
begin
  UnsubscribeInternalAPIEventsCore;
  try
    InternalAPI.RtfText := Value;
  finally
    SubscribeInternalAPIEventsCore;
  end;
end;


function TdxNativeDocument.GetText: string;
begin
  Result := InternalAPI.Text;
end;

procedure TdxNativeDocument.SetText(const Value: string);
begin
  UnsubscribeInternalAPIEventsCore;
  try
    InternalAPI.Text := Value;
  finally
    SubscribeInternalAPIEventsCore;
  end;
end;

function TdxNativeDocument.GetOpenXmlBytes: TBytes;
begin
  Result := InternalAPI.OpenXmlBytes;
end;

procedure TdxNativeDocument.SetOpenXmlBytes(const Value: TBytes);
begin
  UnsubscribeInternalAPIEventsCore;
  try
    InternalAPI.OpenXmlBytes := Value;
  finally
    SubscribeInternalAPIEventsCore;
  end;
end;


function TdxNativeDocument.GetSelections: IdxRichEditSelectionCollection;
begin
  if FSelections = nil then
    FSelections := TdxNativeSelectionCollection.Create(Self);
  Result := FSelections;
end;

function TdxNativeDocument.GetTableStyles: IdxRichEditTableStyleCollection;
begin
  Result := FTableStyles;
end;

function TdxNativeDocument.GetCaretPosition: IdxRichEditDocumentPosition;
var
  ARangeInfo: TdxRunInfo;
begin
  CheckValid;
  ARangeInfo := DocumentModel.Selection.Interval;
  Result := TdxNativeDocumentPosition.Create(ActiveSubDocument, ARangeInfo.&End);
end;

procedure TdxNativeDocument.SetCaretPosition(const Value: IdxRichEditDocumentPosition);
var
  ALogPosition: TdxDocumentLogPosition;
begin
  CheckValid;
  Assert(Value <> nil, 'CaretPosition');
  TdxNativeSubDocument(ActiveSubDocument).CheckDocumentPosition(Value);

  ALogPosition := Min(Value.LogPosition, TdxNativeSubDocument(ActiveSubDocument).PieceTable.DocumentEndLogPosition);
  SetSelectionCore(ALogPosition, ALogPosition);
end;

function TdxNativeDocument.GetIsEmpty: Boolean;
begin
  Result := DocumentModel.IsEmpty;
end;

procedure TdxNativeDocument.CreateApiObjects;
begin
  inherited CreateApiObjects;
  FSections := TdxNativeSectionCollection.Create;
  FCharacterStyles := TdxNativeCharacterStyleCollection.Create(Self);
  FParagraphStyles := TdxNativeParagraphStyleCollection.Create(Self);
  FTableStyles := TdxNativeTableStyleCollection.Create(Self);
  FDefaultParagraphProperties := TdxNativeDefaultParagraphProperties.Create(Self, DocumentModel.DefaultParagraphProperties);
  FDefaultCharacterProperties := TdxNativeDefaultCharacterProperties.Create(DocumentModel.DefaultCharacterProperties);
  FDefaultTableProperties := TdxNativeDefaultTableProperties.Create(Self, DocumentModel.DefaultTableProperties);
  FAbstractNumberingLists := TdxNativeAbstractNumberingListCollection.Create(Self);
  FNumberingLists := TdxNativeNumberingListCollection.Create(Self);
end;

procedure TdxNativeDocument.DoInitialize;
begin
  inherited DoInitialize;
  PopulateSections;
end;

procedure TdxNativeDocument.DoFinalize;
begin
  inherited DoFinalize;
  TdxNativeSectionCollection(FSections).Clear;
  TdxNativeCharacterStyleCollection(FCharacterStyles).Invalidate;
  TdxNativeParagraphStyleCollection(FParagraphStyles).Invalidate;
  TdxNativeTableStyleCollection(FTableStyles).Invalidate;
  TdxNativeAbstractNumberingListCollection(FAbstractNumberingLists).Invalidate;
  TdxNativeNumberingListCollection(FNumberingLists).Invalidate;
end;

procedure TdxNativeDocument.DestroyApiObjects;
begin
  FSections := nil;
  FAbstractNumberingLists := nil;
  FCharacterStyles := nil;
  FDefaultCharacterProperties := nil;
  FDefaultParagraphProperties := nil;
  FDefaultTableProperties := nil;
  FNumberingLists := nil;
  FParagraphStyles := nil;
  FSelections := nil;
  FTableStyles := nil;
  inherited DestroyApiObjects;
end;

procedure TdxNativeDocument.PopulateSections;
var
  I: Integer;
begin
  TdxNativeSectionCollection(FSections).Clear;
  for I := 0 to DocumentModel.Sections.Count - 1 do
    RegisterSection(DocumentModel.Sections[I]);
end;

procedure TdxNativeDocument.RegisterSection(ASection: TdxSection);
begin
  TdxNativeSectionCollection(FSections).Add(CreateNativeSection(ASection));
end;

function TdxNativeDocument.CreateNativeSection(ASection: TdxSection): IdxRichEditSection;
begin
  Result := TdxNativeSection.Create(Self, ASection);
end;

procedure TdxNativeDocument.DoSubscribeInternalAPIEvents;
begin
  inherited DoSubscribeInternalAPIEvents;
  SubscribeInternalAPIEventsCore;
  InternalAPI.DocumentReplaced.Add(OnDocumentReplaced);
end;

procedure TdxNativeDocument.DoUnsubscribeInternalAPIEvents;
begin
  inherited DoUnsubscribeInternalAPIEvents;
  UnsubscribeInternalAPIEventsCore;
  InternalAPI.DocumentReplaced.Remove(OnDocumentReplaced);
end;

procedure TdxNativeDocument.UnsubscribeInternalAPIEventsCore;
begin
  InternalAPI.SectionInserted.Remove(OnSectionInserted);
  InternalAPI.SectionRemoved.Remove(OnSectionRemoved);
  DocumentModel.InnerDocumentCleared.Remove(OnDocumentCleared);
end;

procedure TdxNativeDocument.SubscribeInternalAPIEventsCore;
begin
  InternalAPI.SectionInserted.Add(OnSectionInserted);
  InternalAPI.SectionRemoved.Add(OnSectionRemoved);
  DocumentModel.InnerDocumentCleared.Add(OnDocumentCleared);
end;

procedure TdxNativeDocument.OnDocumentCleared(ASender: TObject; E: TdxEventArgs);
begin
  Invalidate;
end;

procedure TdxNativeDocument.OnDocumentReplaced(ASender: TObject);
begin
  Invalidate;
end;

procedure TdxNativeDocument.OnSectionInserted(ASender: TObject;
  E: TdxSectionEventArgs);
var
  ASectionIndex: Integer;
  ASection: IdxRichEditSection;
begin
  ASectionIndex := E.SectionIndex;
  ASection := CreateNativeSection(DocumentModel.Sections[E.SectionIndex]);
  TdxNativeSectionCollection(FSections).Insert(ASectionIndex, ASection);
end;

procedure TdxNativeDocument.OnSectionRemoved(ASender: TObject;
  E: TdxSectionEventArgs);
var
  ASectionIndex: Integer;
  ASection: IdxRichEditSection;
begin
  ASectionIndex := E.SectionIndex;
  ASection := FSections[ASectionIndex];
  TdxNativeSection(ASection).IsValid := False;
  TdxNativeSectionCollection(FSections).Delete(ASectionIndex);
end;

procedure TdxNativeDocument.CreateNewDocument;
begin
  DocumentModel.InternalAPI.CreateNewDocument;
end;

procedure TdxNativeDocument.LoadDocument(AStream: TStream; AFormat: TdxRichEditDocumentFormat);
begin
  LoadDocument(AStream, AFormat, '');
end;

procedure TdxNativeDocument.LoadDocument(AStream: TStream; AFormat: TdxRichEditDocumentFormat;
  const ASourceUri: string);
begin
  LoadDocumentCore(AStream, AFormat, ASourceUri);
end;

procedure TdxNativeDocument.LoadDocument(const AFileName: string; AFormat: TdxRichEditDocumentFormat);
begin
  LoadDocument(AFileName, AFormat, AFileName);
end;

procedure TdxNativeDocument.LoadDocument(const AFileName: string;
  AFormat: TdxRichEditDocumentFormat; const ASourceUri: string);
var
  AStream: TdxMemoryStream;
begin
  if AFormat = TdxRichEditDocumentFormat.Undefined then
    AFormat := DocumentModel.AutodetectDocumentFormat(AFileName);
  AStream := TdxMemoryStream.Create(AFileName);
  try
    LoadDocumentCore(AStream, AFormat, ASourceUri);
  finally
    AStream.Free;
  end;
end;

procedure TdxNativeDocument.LoadDocumentCore(AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ASourceUri: string);
var
  AImportManagerService: IdxDocumentImportManagerService;
  AImportHelper: TdxDocumentImportHelper;
begin
  AImportManagerService := DocumentModel.GetService<IdxDocumentImportManagerService>;
  if AImportManagerService = nil then
    TdxRichEditExceptions.ThrowInvalidOperationException('Could not find service: IdxDocumentExportManagerService');

  AImportHelper := TdxDocumentImportHelper.Create(DocumentModel);
  try
    AImportHelper.Import(AStream, AFormat, ASourceUri, AImportManagerService);
  finally
    AImportHelper.Free;
  end;
end;

procedure TdxNativeDocument.SaveDocument(AStream: TStream; AFormat: TdxRichEditDocumentFormat);
begin
  SaveDocumentCore(AStream, AFormat, '');
end;

procedure TdxNativeDocument.SaveDocumentCore(AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ATargetUri: string);
begin
  TdxInnerRichEditDocumentServer.SaveDocumentCore(DocumentModel, AStream, AFormat, ATargetUri);
end;

procedure TdxNativeDocument.SaveDocument(const AFileName: string; AFormat: TdxRichEditDocumentFormat);
begin
  SaveDocument(DocumentModel, AFileName, AFormat);
end;

class procedure TdxNativeDocument.SaveDocument(ADocumentModel: TdxDocumentModel; const AFileName: string; AFormat: TdxRichEditDocumentFormat);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate or fmOpenWrite or fmShareDenyWrite);
  try
    TdxInnerRichEditDocumentServer.SaveDocumentCore(ADocumentModel, AStream, AFormat, AFileName);
  finally
    AStream.Free;
  end;
end;

function TdxNativeDocument.InsertSection(const APos: IdxRichEditDocumentPosition): IdxRichEditSection;
begin
  Result := InsertSectionCore(APos, False);
end;

function TdxNativeDocument.AppendSection: IdxRichEditSection;
begin
  Result := InsertSectionCore(EndPosition, True);
end;

function TdxNativeDocument.InsertSectionCore(const APos: IdxRichEditDocumentPosition; AAppend: Boolean): IdxRichEditSection;
var
  ANativePosition: TdxNativeDocumentPosition;
  ALogPosition: TdxDocumentLogPosition;
  ASectionIndex: Integer;
begin
  CheckDocumentPosition(APos);

  ANativePosition := TdxNativeDocumentPosition(APos);
  ALogPosition := NormalizeLogPosition(ANativePosition.Position.LogPosition);
  ASectionIndex := DocumentModel.FindSectionIndex(ALogPosition);
  DocumentModel.InsertSection(ALogPosition);
  if AAppend and (Sections.Count <> 0) then
    ASectionIndex := Sections.Count - 1;
  Result := Sections[ASectionIndex];
end;

procedure TdxNativeDocument.ChangeActiveDocument(const ADocument: IdxRichEditSubDocument);
var
  AControl: IdxRichEditControl;
  APieceTable: TdxPieceTable;
begin
  AControl := DocumentServer.Owner as IdxRichEditControl;
  if AControl = nil then
    Exit;

  APieceTable := TdxNativeSubDocument(ADocument).PieceTable;
  if DocumentModel.ActivePieceTable = APieceTable then
    Exit;

  ChangeActivePieceTable(AControl, APieceTable);
end;

procedure TdxNativeDocument.ChangeActivePieceTable(const AControl: IdxRichEditControl;
  APieceTable: TdxPieceTable);
var
  ACommand: TdxChangeActivePieceTableCommand;
  AHeaderCommand: TdxMakeNearestHeaderActiveCommand;
  AFooterCommand: TdxMakeNearestFooterActiveCommand;
begin
  if (APieceTable.IsMain) or (APieceTable.IsTextBox) or (APieceTable.IsComment) then
  begin
    ACommand := TdxChangeActivePieceTableCommand.Create(AControl, APieceTable, nil, -1);
    try
      ACommand.ForceExecute(ACommand.CreateDefaultCommandUIState);
    finally
      ACommand.Free;
    end;
  end
  else
  begin
    if APieceTable.IsHeader then
    begin
      AHeaderCommand := TdxMakeNearestHeaderActiveCommand.Create(AControl, TdxSectionHeader(APieceTable.ContentType));
      try
        AHeaderCommand.ForceExecute(AHeaderCommand.CreateDefaultCommandUIState);
      finally
        AHeaderCommand.Free;
      end;
    end
    else
      if APieceTable.IsFooter then
      begin
        AFooterCommand := TdxMakeNearestFooterActiveCommand.Create(AControl, TdxSectionFooter(APieceTable.ContentType));
        try
          AFooterCommand.ForceExecute(AFooterCommand.CreateDefaultCommandUIState);
        finally
          AFooterCommand.Free;
        end;
      end;
  end;
end;

function TdxNativeDocument.GetActiveSubDocument: IdxRichEditSubDocument;
begin
  if DocumentModel.ActivePieceTable.IsMain then
    Exit(Self);

  if ShouldCreateNewSubDocument then
    Result := CreateNativeSubDocument(TdxPieceTable(DocumentModel.ActivePieceTable.ContentType.PieceTable), DocumentServer)
  else
    Result := Self;
end;

function TdxNativeDocument.CreateNativeSubDocument(APieceTable: TdxPieceTable;
  AServer: TdxInnerRichEditDocumentServer): TdxNativeSubDocument;
begin
  Result := TdxNativeSubDocument.Create(APieceTable, AServer);
end;

function TdxNativeDocument.ShouldCreateNewSubDocument: Boolean;
begin
  Result := (DocumentModel.ActivePieceTable.ContentType is TdxSectionHeaderFooterBase) or
    (DocumentModel.ActivePieceTable.ContentType is TdxTextBoxContentType) or
    (DocumentModel.ActivePieceTable.ContentType is TdxFootNote) or
    (DocumentModel.ActivePieceTable.ContentType is TdxEndNote)
;
end;

function TdxNativeDocument.GetSelection: IdxRichEditDocumentRange;
var
  ARangeInfo: TdxRunInfo;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  CheckValid;
  ARangeInfo := DocumentModel.Selection.Interval;
  AStart := ARangeInfo.NormalizedStart^;
  AEnd := ARangeInfo.NormalizedEnd^;
  Result := TdxNativeDocumentRange.Create(ActiveSubDocument, AStart, AEnd);
end;

procedure TdxNativeDocument.SetSelection(const Value: IdxRichEditDocumentRange);
begin
  CheckValid;
  TdxNativeSubDocument(ActiveSubDocument).CheckDocumentRange(Value);
  SetSelectionCore(Value.Start.LogPosition, Value.&End.LogPosition);
end;

procedure TdxNativeDocument.Copy(const ARange: IdxRichEditDocumentRange);
var
  ASelection: TdxSelectionRangeCollection;
  AManager: TdxCopySelectionManager;
begin
  CheckDocumentRange(ARange);
  ASelection := GetSelectionRangeCollection(ARange);
  try
    if ASelection = nil then
      Exit;
    AManager := TdxCopySelectionManager.Create(DocumentServer as IdxInnerControl);
    try
      AManager.CopyDocumentRange(PieceTable, ASelection);
    finally
      AManager.Free;
    end;
  finally
    ASelection.Free;
  end;
end;

procedure TdxNativeDocument.Copy;
var
  AControl: IdxRichEditControl;
  ACommand: TdxCopySelectionCommand;
begin
  AControl := DocumentServer.Owner as IdxRichEditControl;
  if AControl = nil then
    Exit;
  ACommand := TdxCopySelectionCommand.Create(AControl);
  try
    ACommand.ForceExecute(ACommand.CreateDefaultCommandUIState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxNativeDocument.Cut(const ARange: IdxRichEditDocumentRange);
begin
  DocumentModel.BeginUpdate;
  try
    Copy(ARange);
    Delete(ARange);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeDocument.Cut;
var
  AControl: IdxRichEditControl;
  ACommand: TdxCutSelectionCommand;
begin
  AControl := DocumentServer.Owner as IdxRichEditControl;
  if AControl = nil then
    Exit;
  ACommand := TdxCutSelectionCommand.Create(AControl);
  try
    ACommand.ForceExecute(ACommand.CreateDefaultCommandUIState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxNativeDocument.Paste;
var
  AControl: IdxRichEditControl;
  ACommand: TdxPasteSelectionCommand;
begin
  AControl := DocumentServer.Owner as IdxRichEditControl;
  if AControl = nil then
    Exit;
  ACommand := TdxPasteSelectionCommand.Create(AControl);
  try
    ACommand.ForceExecute(ACommand.CreateDefaultCommandUIState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxNativeDocument.Paste(const AFormat: TdxRichEditDocumentFormat);
var
  AControl: IdxRichEditControl;
  ACommand: TdxPasteSelectionCommand;
begin
  AControl := DocumentServer.Owner as IdxRichEditControl;
  if AControl = nil then
    Exit;
  ACommand := TdxPasteSelectionCommand.Create(AControl);
  try
    ACommand.Format := AFormat;
    ACommand.ForceExecute(ACommand.CreateDefaultCommandUIState);
  finally
    ACommand.Free;
  end;
end;

function TdxNativeDocument.GetSection(const APos: IdxRichEditDocumentPosition): IdxRichEditSection;
var
  ASubDocument: TdxNativeSubDocument;
  AHeaderFooter: TdxSectionHeaderFooterBase;
  AIndex: Integer;
begin
  ASubDocument := GetSubDocument(APos);

  AHeaderFooter := Safe<TdxSectionHeaderFooterBase>.Cast(ASubDocument.PieceTable.ContentType);
  if AHeaderFooter <> nil then
    AIndex := AHeaderFooter.GetSectionIndex
  else
  begin
    CheckDocumentPosition(APos);
    AIndex := DocumentModel.FindSectionIndex(APos.LogPosition, False);
  end;
  if AIndex < 0 then
    Exit(nil);
  Result := Sections[AIndex];
end;

function TdxNativeDocument.GetSubDocument(const APos: IdxRichEditDocumentPosition): TdxNativeSubDocument;
var
  ANativePosition: TdxNativeDocumentPosition;
begin
  ANativePosition := TdxNativeDocumentPosition(APos);
  if ANativePosition <> nil then
    Result := TdxNativeSubDocument(ANativePosition.Document)
  else
    Result := TdxNativeSubDocument(ActiveSubDocument);
end;

function TdxNativeDocument.CreateMailMergeOptions: IdxRichEditMailMergeOptions;
begin
  Result := DocumentServer.CreateMailMergeOptions;
end;

procedure TdxNativeDocument.MailMerge(const AFileName: string; AFormat: TdxRichEditDocumentFormat);
begin
  DocumentServer.MailMerge(AFileName, AFormat);
end;

procedure TdxNativeDocument.MailMerge(AStream: TStream; AFormat: TdxRichEditDocumentFormat);
begin
  DocumentServer.MailMerge(AStream, AFormat);
end;

procedure TdxNativeDocument.MailMerge(const ATargetDocument: IdxRichEditDocument);
begin
  DocumentServer.MailMerge(ATargetDocument);
end;

procedure TdxNativeDocument.MailMerge(const AOptions: IdxRichEditMailMergeOptions; const AFileName: string; AFormat: TdxRichEditDocumentFormat);
begin
  DocumentServer.MailMerge(AOptions, AFileName, AFormat);
end;

procedure TdxNativeDocument.MailMerge(const AOptions: IdxRichEditMailMergeOptions; AStream: TStream; AFormat: TdxRichEditDocumentFormat);
begin
  DocumentServer.MailMerge(AOptions, AStream, AFormat);
end;

function TdxNativeDocument.GetParagraphStyles: IdxRichEditParagraphStyleCollection;
begin
  CheckValid;
  Result := FParagraphStyles;
end;

function TdxNativeDocument.GetAbstractNumberingLists: IdxRichEditAbstractNumberingListCollection;
begin
  CheckValid;
  Result := FAbstractNumberingLists;
end;

function TdxNativeDocument.GetNumberingLists: IdxRichEditNumberingListCollection;
begin
  CheckValid;
  Result := FNumberingLists;
end;

function TdxNativeDocument.GetCharacterStyles: IdxRichEditCharacterStyleCollection;
begin
  CheckValid;
  Result := FCharacterStyles;
end;

function TdxNativeDocument.GetSections: IdxRichEditSectionCollection;
begin
  CheckValid;
  Result := FSections;
end;

function TdxNativeDocument.GetVariables: IdxRichEditDocumentVariableCollection;
begin
  CheckValid;
  Result := DocumentModel.Variables;
end;

procedure TdxNativeDocument.MailMerge(const AOptions: IdxRichEditMailMergeOptions; const ATargetDocument: IdxRichEditDocument);
begin
  DocumentServer.MailMerge(AOptions, ATargetDocument);
end;

procedure TdxNativeDocument.Protect(const APassword: string);
begin
  DocumentModel.EnforceDocumentProtection(APassword);
end;

procedure TdxNativeDocument.Unprotect;
begin
  DocumentModel.ForceRemoveDocumentProtection;
end;

procedure TdxNativeDocument.SetEncryptionPassword(const APassword: string);
begin
  DocumentModel.EncryptionProperties.Password := APassword;
end;

function TdxNativeDocument.HasEncryptionPassword: Boolean;
begin
  Result := DocumentModel.EncryptionProperties.IsEncrypted;
end;


end.
