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

unit dxRichEdit.Api.Layout.Painters;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Types, Generics.Defaults, Generics.Collections, Controls, Contnrs,
  Graphics, Classes, SyncObjs,
  cxGeometry, cxGraphics, dxCore, dxCoreClasses, cxControls, dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses,

  dxGenerics,
  dxRichEdit.Types,
  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.Core;

type
  TdxRichEditLayoutRow = class;
  TdxRichEditLayoutTableCell = class;
  TdxRichEditLayoutTableRow = class;
  TdxRichEditLayoutTable = class;
  TdxRichEditLayoutFloatingObject = class;
  TdxRichEditLayoutColumn = class;
  TdxRichEditLayoutComment = class;
  TdxRichEditLayoutPageArea = class;
  TdxRichEditLayoutPage = class;
  TdxRichEditLineNumberBox = class;
  TdxRichEditUnderlineBox = class;
  TdxRichEditStrikeoutBox = class;
  TdxRichEditRangePermissionBox = class;
  TdxRichEditCommentBox = class;
  TdxRichEditBookmarkBox = class;
  TdxRichEditHighlightAreaBox = class;
  TdxRichEditFieldHighlightAreaBox = class;
  TdxRichEditRangePermissionHighlightAreaBox = class;
  TdxRichEditCommentHighlightAreaBox = class;
  TdxRichEditHiddenTextUnderlineBox = class;
  TdxRichEditLayoutVisitor = class;
  TdxRichEditPagePainter = class;
  TdxRichEditPagePainterVisitor = class;
  TdxRichEditLayoutHeader = class;
  TdxRichEditLayoutFooter = class;
  TdxRichEditFixedRange = class;
  TdxRichEditDocumentLayout = class;
  TdxRichEditBox = class;
  TdxRichEditRowExtendedBoxes = class;
  TdxRichEditLayoutTableCollection = class;
  TdxRichEditFloatingObjectAnchorBox = class;

  TdxModelDocumentLayout = TdxDocumentLayout;
  TdxModelBox = TdxBox;
  TdxModelRowExtendedBoxes = TdxRowExtendedBoxes;
  TdxModelInlinePictureBox = TdxInlinePictureBox;
  TdxModelFloatingObjectAnchorBox = TdxFloatingObjectAnchorBox;
  TdxModelLineNumberBox = TdxLineNumberBox;
  TdxModelLineNumberBoxCollection = TdxLineNumberBoxCollection;
  TdxModelUnderlineBox = TdxUnderlineBox;
  TdxModelHiddenTextUnderlineBox = TdxHiddenTextUnderlineBox;

  TdxRichEditNativeSection = class(TObject);
  TdxRichEditLayoutDocumentPrinter = class(TObject);

  TdxRichEditLayoutType = (
    Page,
    PageArea,
    Column,
    Comment,
    Header,
    Footer,
    //FootNote,
    //EndNote,
    TextBox,
    FloatingPicture,
    Table,
    TableRow,
    TableCell,
    Row,
    PlainTextBox,
    SpecialTextBox,
    InlinePictureBox,
    FloatingObjectAnchorBox,
    SpaceBox,
    ParagraphMarkBox,
    SectionBreakBox,
    LineBreakBox,
    PageBreakBox,
    ColumnBreakBox,
    HyphenBox,
    TabSpaceBox,
    NumberingListMarkBox,
    NumberingListWithSeparatorBox,
    UnderlineBox,
    StrikeoutBox,
    ErrorBox,
    HighlightAreaBox,
    FieldHighlightAreaBox,
    RangePermissionHighlightAreaBox,
    BookmarkStartBox,
    BookmarkEndBox,
    RangePermissionStartBox,
    RangePermissionEndBox,
    CommentHighlightAreaBox,
    CommentStartBox,
    CommentEndBox,
    HiddenTextUnderlineBox,
    LineNumberBox,
    PageNumberBox,
    //SeparatorBox,
    //DataContainerRunBox,
    CharacterBox,
    Frame,
    ParagraphFrameBox
  );

  { TdxRichEditLayoutBorder }

  TdxRichEditLayoutBorder = record
  private
    FColor: TdxAlphaColor;
    FStyle: TdxRichEditTableBorderLineStyle;
    FThickness: Integer;
  public
    constructor Create(AColor: TdxAlphaColor; AStyle: TdxRichEditTableBorderLineStyle; AThickness: Integer);

    property Thickness: Integer read FThickness;
    property Color: TdxAlphaColor read FColor;
    property Style: TdxRichEditTableBorderLineStyle read FStyle;
  end;

  { TdxRichEditBorders }

  TdxRichEditBorders = record
  private
    FLeft: TdxRichEditLayoutBorder;
    FRight: TdxRichEditLayoutBorder;
    FTop: TdxRichEditLayoutBorder;
    FBottom: TdxRichEditLayoutBorder;
  public
    constructor Create(const ALeft, ARight, ATop, ABottom: TdxRichEditLayoutBorder);

    property Left: TdxRichEditLayoutBorder read FLeft;
    property Right: TdxRichEditLayoutBorder read FRight;
    property Top: TdxRichEditLayoutBorder read FTop;
    property Bottom: TdxRichEditLayoutBorder read FBottom;
  end;

  { IdxRichEditLayoutElement }

  IdxRichEditLayoutElement = interface
  ['{6FF57EA1-F574-4063-B424-5685BD01B245}']
    function GetType: TdxRichEditLayoutType;
    function GetBounds: TRect;
    function GetParent: IdxRichEditLayoutElement;
    function GetAsObject: TObject;

    procedure Accept(AVisitor: TdxRichEditLayoutVisitor);
    function GetRelativeBounds(const AElement: IdxRichEditLayoutElement): TRect;
    function GetParentByType(AClass: TClass): TObject;
    function GetParentByIntf(const IID: TGUID): IInterface;

    property AsObject: TObject read GetAsObject;
    property &Type: TdxRichEditLayoutType read GetType;
    property Bounds: TRect read GetBounds;
    property Parent: IdxRichEditLayoutElement read GetParent;
  end;

  { TdxRichEditLayoutElementCollection }

  TdxRichEditLayoutElementCollection = class abstract(TObjectList);

  { TdxRichEditObjectLayoutElementCollection<T> }

  TdxRichEditObjectLayoutElementCollection<T: class> = class(TdxRichEditLayoutElementCollection)
  strict private
    function GetItem(Index: Integer): T;
  public
    function First: T;
    function Last: T;
    property Items[Index: Integer]: T read GetItem; default;
  end;

  { IdxRichEditRangedLayoutElement }

  IdxRichEditRangedLayoutElement = interface
  ['{DFA44B89-B2BB-46B2-B8BA-2205118437AC}']
    function GetRange: TdxRichEditFixedRange;
    function GetAsObject: TObject;
    function GetType: TdxRichEditLayoutType;
    function GetBounds: TRect;
    function GetParent: IdxRichEditLayoutElement;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor);
    function GetRelativeBounds(const AElement: IdxRichEditLayoutElement): TRect;
    function GetParentByType(AClass: TClass): TObject;
    function GetParentByIntf(const IID: TGUID): IUnknown;

    property Range: TdxRichEditFixedRange read GetRange;
    property AsObject: TObject read GetAsObject;
    property &Type: TdxRichEditLayoutType read GetType;
    property Bounds: TRect read GetBounds;
    property Parent: IdxRichEditLayoutElement read GetParent;
  end;

  { IdxRichEditDocumentLayoutProvider }

  IdxRichEditDocumentLayoutProvider = interface
  ['{CBAA6E00-17DE-44E0-91C2-2B718CC3BA8E}']
    function GetDocument: IdxRichEditDocument;
    function GetLayoutCalculationMode: TdxCalculationModeType;
    function GetDocumentLayout: TdxDocumentLayout;
    function GetDocumentLayoutAsync: TdxDocumentLayout;
    procedure AddDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure AddPageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure AddDocumentFormatted(const AHandler: TdxEvent);
    procedure RemoveDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure RemovePageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure RemoveDocumentFormatted(const AHandler: TdxEvent);
    procedure PerformPageSecondaryFormatting(APage: TdxPage);

    property Document: IdxRichEditDocument read GetDocument;
    property LayoutCalculationMode: TdxCalculationModeType read GetLayoutCalculationMode;
  end;

  { IdxRichEditPageCanvas }

  IdxRichEditPageCanvas = interface
  ['{A5D54F2B-715B-468D-AF18-03219B0C8A74}']
  end;

  { IdxPieceTableContainer }

  IdxPieceTableContainer = interface
  ['{9CE4BD44-2B82-4D29-A951-82CECFC068A8}']
    function GetPieceTable: TdxPieceTable;

    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxRichEditBeforePagePaintEventArgs }

  TdxRichEditBeforePagePaintEventArgs = class(TdxEventArgs)
  strict private
    FPainter: TdxRichEditPagePainter;
    FPageIndex: Integer;
    function GetCanvas: IdxRichEditPageCanvas;
    function GetPage: TdxRichEditLayoutPage;
  public
    constructor Create(APainter: TdxRichEditPagePainter; APageIndex: Integer);

    property Painter: TdxRichEditPagePainter read FPainter write FPainter;
    property Canvas: IdxRichEditPageCanvas read GetCanvas;
    property Page: TdxRichEditLayoutPage read GetPage;
  end;

  { TdxRichEditLayoutElementBase }

  TdxRichEditLayoutElementBase = class abstract(TcxIUnknownObject, IdxRichEditLayoutElement)
  strict private
    FParent: IdxRichEditLayoutElement;
    FBounds: TRect;
    function InternalGetBounds: TRect;
  protected
    function GetAsObject: TObject;
    function GetType: TdxRichEditLayoutType; virtual; abstract;
    function GetBounds: TRect; virtual; abstract;
    function GetParent: IdxRichEditLayoutElement;
    function GetParentByType(AClass: TClass): TObject; overload;
    function GetParentByIntf(const IID: TGUID): IUnknown;
  public
    constructor Create(const AParent: IdxRichEditLayoutElement);
    procedure SetBounds(const ABounds: TRect);
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); virtual; abstract;
    function GetParentByType<T: class>: T; overload;
    function GetParentByTypeCore(AClass: TClass; const AParent: IdxRichEditLayoutElement): TObject;
    function GetParentByIntfCore(const IID: TGUID; const AParent: IdxRichEditLayoutElement): IUnknown;
    function GetRelativeBounds(const AElement: IdxRichEditLayoutElement): TRect;

    property &Type: TdxRichEditLayoutType read GetType;
    property Bounds: TRect read InternalGetBounds;
    property Parent: IdxRichEditLayoutElement read FParent;
  end;

  { TdxRichEditParagraphFrameBox }

  TdxRichEditParagraphFrameBox = class(TdxRichEditLayoutElementBase)
  strict private
    FBorders: TdxRichEditBorders;
    FBackColor: TdxAlphaColor;
    FModelBox: TdxParagraphFrameBox;
    FDrawingParent: IdxRichEditLayoutElement;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;

    property DrawingParent: IdxRichEditLayoutElement read FDrawingParent;
    property ModelBox: TdxParagraphFrameBox read FModelBox;
  public
    constructor Create(AModelBox: TdxParagraphFrameBox; const ADrawingParent, AParent: IdxRichEditLayoutElement);
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property BackColor: TdxAlphaColor read FBackColor;
    property Borders: TdxRichEditBorders read FBorders;
  end;

  { TdxRichEditParagraphFrameBoxCollection }

  TdxRichEditParagraphFrameBoxCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditParagraphFrameBox>);

  { TdxRichEditRangedLayoutElementBase }

  TdxRichEditRangedLayoutElementBase = class abstract(TdxRichEditLayoutElementBase,
    IdxRichEditLayoutElement,
    IdxRichEditRangedLayoutElement)
  strict private
    FRange: TdxRichEditFixedRange;
    function GetRange: TdxRichEditFixedRange;
  protected
    function CreateRange: TdxRichEditFixedRange; virtual; abstract;
    function IdxRichEditRangedLayoutElement.GetParentByType = InnerGetParentByType;
  public
    destructor Destroy; override;

    property Range: TdxRichEditFixedRange read GetRange;
  end;

  { TdxRichEditBox }

  TdxRichEditBox = class abstract(TdxRichEditRangedLayoutElementBase)
  strict private
    FBox: TdxModelBox;
    FType: TdxRichEditLayoutType;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
    function CreateRange: TdxRichEditFixedRange; override;

    property ModelBox: TdxModelBox read FBox;
  public
    constructor Create(ABox: TdxModelBox; AType: TdxRichEditLayoutType; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
  end;

  TdxRichEditBoxCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditBox>);


  { TdxRichEditFixedRange }

  TdxRichEditFixedRange = class
  strict private
    FStart: Integer;
    FEnd: Integer;
    FLength: Integer;
  public
    constructor Create(AStart: Integer; ALength: Integer);
    function Contains(ARange: TdxRichEditFixedRange): Boolean; overload;
    function Contains(APosition: Integer): Boolean; overload;
    function Intersect(ARange: TdxRichEditFixedRange): Boolean;
    function Equals(AObj: TObject): Boolean; override;
    function ToString: string; override;

    property Start: Integer read FStart;
    property Length: Integer read FLength;
    property &End: Integer read FEnd;
  end;

  { TdxRichEditPlainTextBox }

  TdxRichEditPlainTextBox = class(TdxRichEditBox)
  strict private
    FText: string;
    function GetText: string;
  public
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
    procedure SetText(const AText: string);

    property Text: string read GetText;
  end;

  { TdxRichEditInlinePictureBox }

  TdxRichEditInlinePictureBox = class(TdxRichEditBox)
  strict private
    FImage: TdxOfficeImage;
    function GetImage: TdxOfficeImage;
  public
    constructor Create(ABox: TdxModelInlinePictureBox; const AParent: IdxRichEditLayoutElement);
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property Image: TdxOfficeImage read GetImage;
  end;

  { TdxRichEditNumberingListMarkBox }

  TdxRichEditNumberingListMarkBox = class(TdxRichEditLayoutElementBase)
  strict private
    FBox: TdxNumberingListBox;
    FListIndex: Integer;
    FListLevel: Integer;
    function GetListIndex: Integer;
    function GetListLevel: Integer;
    function GetText: string;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
    function GetParagraph: TdxParagraph;

    property ModelBox: TdxNumberingListBox read FBox;
  public
    constructor Create(ABox: TdxNumberingListBox; const AParent: IdxRichEditLayoutElement);
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property ListIndex: Integer read GetListIndex;
    property ListLevel: Integer read GetListLevel;
    property Text: string read GetText;
  end;

  { TdxRichEditNumberingListWithSeparatorBox }

  TdxRichEditNumberingListWithSeparatorBox = class(TdxRichEditNumberingListMarkBox)
  strict private
    FSeparator: TdxRichEditPlainTextBox;
  protected
    function GetType: TdxRichEditLayoutType; override;
  public
    constructor Create(ABox: TdxNumberingListBoxWithSeparator; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property Separator: TdxRichEditPlainTextBox read FSeparator;
  end;

  { TdxRichEditLayoutRow }

  TdxRichEditLayoutRow = class(TdxRichEditRangedLayoutElementBase)
  strict private
    FRow: TdxRow;
    FBoxes: TdxRichEditBoxCollection;
    FExtendedBoxes: TdxRichEditRowExtendedBoxes;
    FNumberingListBox: TdxRichEditNumberingListMarkBox;
    function GetBoxes: TdxRichEditBoxCollection;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
  public
    constructor Create(ARow: TdxRow; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
    procedure InitializeNumberingListBox;
    function CreateRange: TdxRichEditFixedRange; override;

    property Boxes: TdxRichEditBoxCollection read GetBoxes;
    property NumberingListBox: TdxRichEditNumberingListMarkBox read FNumberingListBox;
    property ExtendedBoxes: TdxRichEditRowExtendedBoxes read FExtendedBoxes;
    property ModelRow: TdxRow read FRow;
  end;

  TdxRichEditLayoutRowCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditLayoutRow>);

  { TdxRichEditLayoutTableCell }

  TdxRichEditLayoutTableCell = class(TdxRichEditRangedLayoutElementBase)
  strict private
    FTableCell: TdxTableCellViewInfo;
    FRows: TdxRichEditLayoutRowCollection;
    FNestedTables: TdxRichEditLayoutTableCollection;
    FBorders: TdxRichEditBorders;
    function GetRows: TdxRichEditLayoutRowCollection;
    function GetNestedTables: TdxRichEditLayoutTableCollection;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
  public
    constructor Create(ATableCell: TdxTableCellViewInfo; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
    function CreateRange: TdxRichEditFixedRange; override;

    property Rows: TdxRichEditLayoutRowCollection read GetRows;
    property NestedTables: TdxRichEditLayoutTableCollection read GetNestedTables;
    property Borders: TdxRichEditBorders read FBorders;
    property ModelTableCell: TdxTableCellViewInfo read FTableCell;
  end;

  TdxRichEditLayoutTableCellCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditLayoutTableCell>);

  { TdxRichEditLayoutTableRow }

  TdxRichEditLayoutTableRow = class(TdxRichEditRangedLayoutElementBase)
  strict private
    FModelTableRow: TdxTableRowViewInfoBase;
    FTableCells: TdxRichEditLayoutTableCellCollection;
    function GetTableCells: TdxRichEditLayoutTableCellCollection;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;

    property ModelTableRow: TdxTableRowViewInfoBase read FModelTableRow;
  public
    constructor Create(ATableRow: TdxTableRowViewInfoBase; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
    function CreateRange: TdxRichEditFixedRange; override;

    property TableCells: TdxRichEditLayoutTableCellCollection read GetTableCells;
  end;

  TdxRichEditLayoutTableRowCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditLayoutTableRow>);

  { TdxRichEditLayoutTable }

  TdxRichEditLayoutTable = class(TdxRichEditRangedLayoutElementBase)
  strict private
    FTable: TdxTableViewInfo;
    FTableRows: TdxRichEditLayoutTableRowCollection;
    FBorders: TdxRichEditBorders;
    function GetTableRows: TdxRichEditLayoutTableRowCollection;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
  public
    constructor Create(ATable: TdxTableViewInfo; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
    function CreateRange: TdxRichEditFixedRange; override;

    property TableRows: TdxRichEditLayoutTableRowCollection read GetTableRows;
    property Borders: TdxRichEditBorders read FBorders;
    property ModelTable: TdxTableViewInfo read FTable;
  end;

  TdxRichEditLayoutTableCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditLayoutTable>);

  { TdxRichEditLineNumberBox }

  TdxRichEditLineNumberBox = class(TdxRichEditLayoutElementBase)
  strict private
    FModelBox: TdxModelLineNumberBox;
    FRow: TdxRichEditLayoutRow;
    function GetRow: TdxRichEditLayoutRow;
    function GetText: string;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;

    property ModelBox: TdxModelLineNumberBox read FModelBox;
  public
    constructor Create(AModelBox: TdxModelLineNumberBox; const AParent: IdxRichEditLayoutElement);
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property Row: TdxRichEditLayoutRow read GetRow;
    property Text: string read GetText;
  end;

  TdxRichEditLineNumberBoxCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditLineNumberBox>);

  IdxRichEditParagraphFrameContainer = interface
  ['{9611C37D-2424-479F-B0B1-3479E9B05A11}']
    function GetParagraphFrames: TdxRichEditParagraphFrameBoxCollection;

    property ParagraphFrames: TdxRichEditParagraphFrameBoxCollection read GetParagraphFrames;
  end;

  { TdxRichEditLayoutColumn }

  TdxRichEditLayoutColumn = class(TdxRichEditRangedLayoutElementBase, IdxRichEditParagraphFrameContainer)
  strict private
    FColumn: TdxColumn;
    FTables: TdxRichEditLayoutTableCollection;
    FRows: TdxRichEditLayoutRowCollection;
    FLineNumbers: TdxRichEditLineNumberBoxCollection;
    FParagraphFrames: TdxRichEditParagraphFrameBoxCollection;
    function GetModelRows: TdxRowCollection;
    function GetModelTables: TdxTableViewInfoCollection;
    function GetTables: TdxRichEditLayoutTableCollection;
    function GetRows: TdxRichEditLayoutRowCollection;
    function GetLineNumbers: TdxRichEditLineNumberBoxCollection;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
    function GetParagraphFrames: TdxRichEditParagraphFrameBoxCollection;
  public
    constructor Create(AColumn: TdxColumn; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
    function CreateRange: TdxRichEditFixedRange; override;

    property ModelRows: TdxRowCollection read GetModelRows;
    property ModelTables: TdxTableViewInfoCollection read GetModelTables;
    property Tables: TdxRichEditLayoutTableCollection read GetTables;
    property Rows: TdxRichEditLayoutRowCollection read GetRows;
    property LineNumbers: TdxRichEditLineNumberBoxCollection read GetLineNumbers;
    property ModelColumn: TdxColumn read FColumn;
    property ParagraphFrames: TdxRichEditParagraphFrameBoxCollection read GetParagraphFrames;
  end;

  TdxRichEditLayoutColumnCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditLayoutColumn>);

  { TdxRichEditLayoutFloatingObject }

  TdxRichEditLayoutFloatingObject = class abstract(TdxRichEditLayoutElementBase)
  strict private
    UnitsPerDegree: Integer;
    FFloatingObject: TdxFloatingObjectBox;
    FAnchorRun: TdxFloatingObjectAnchorRun;
    FAnchorBox: TdxRichEditFloatingObjectAnchorBox;
    function GetAnchorBox: TdxRichEditFloatingObjectAnchorBox;
    function GetAnchorRun: TdxFloatingObjectAnchorRun;
    function GetContentBounds: TRect;
    function GetRotationAngle: Single;
  protected
    function GetBounds: TRect; override;
  public
    constructor Create(AFloatingObject: TdxFloatingObjectBox; const AParent: IdxRichEditLayoutElement);
    function GetDocumentShape: TdxShape;
    function GetCoordinates: TArray<TPoint>;
    function GetContentCoordinates: TArray<TPoint>;
    function GetCoordinatesCore(const ABounds: TRect): TArray<TPoint>;

    property AnchorBox: TdxRichEditFloatingObjectAnchorBox read GetAnchorBox;
    property ModelFloatingObject: TdxFloatingObjectBox read FFloatingObject;
    property AnchorRun: TdxFloatingObjectAnchorRun read GetAnchorRun;
    property ContentBounds: TRect read GetContentBounds;
    property RotationAngle: Single read GetRotationAngle;
  end;

  TdxRichEditLayoutFloatingObjectCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditLayoutFloatingObject>);

  { TdxRichEditFloatingObjectAnchorBox }

  TdxRichEditFloatingObjectAnchorBox = class(TdxRichEditBox)
  strict private
    FFloatingObjectBox: TdxRichEditLayoutFloatingObject;
    FBounds: TRect;
    function InternalGetFloatingObjectBox: TdxRichEditLayoutFloatingObject;
  protected
    function GetBounds: TRect; override;
    function GetFloatingObjectBox: TdxRichEditLayoutFloatingObject;
    function GetFloatingObjectBoxCore(ACollection: TdxRichEditLayoutFloatingObjectCollection; AAnchorRun: TdxFloatingObjectAnchorRun): TdxRichEditLayoutFloatingObject;
  public
    constructor Create(AAnchorBox: TdxModelFloatingObjectAnchorBox; const ABounds: TRect; const AParent: IdxRichEditLayoutElement);
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property FloatingObjectBox: TdxRichEditLayoutFloatingObject read InternalGetFloatingObjectBox;
  end;

  { TdxRichEditLayoutTextBox }

  TdxRichEditLayoutTextBox = class(TdxRichEditLayoutFloatingObject,
    IdxRichEditLayoutElement,
    IdxRichEditRangedLayoutElement,
    IdxPieceTableContainer,
    IdxRichEditParagraphFrameContainer)
  strict private
    FDocument: IdxRichEditSubDocument;
    FTables: TdxRichEditLayoutTableCollection;
    FRows: TdxRichEditLayoutRowCollection;
    FRange: TdxRichEditFixedRange;
    FParagraphFrames: TdxRichEditParagraphFrameBoxCollection;
    function GetTables: TdxRichEditLayoutTableCollection;
    function GetRows: TdxRichEditLayoutRowCollection;
    function GetDocument: IdxRichEditSubDocument;
    function GetRange: TdxRichEditFixedRange;
    function GetPieceTable: TdxPieceTable;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function IdxRichEditRangedLayoutElement.GetParentByType = InnerGetParentByType;
    function GetParagraphFrames: TdxRichEditParagraphFrameBoxCollection;
  public
    constructor Create(AFloatingObject: TdxFloatingObjectBox; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property Tables: TdxRichEditLayoutTableCollection read GetTables;
    property Rows: TdxRichEditLayoutRowCollection read GetRows;
    property Document: IdxRichEditSubDocument read GetDocument;
    property Range: TdxRichEditFixedRange read GetRange;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property ParagraphFrames: TdxRichEditParagraphFrameBoxCollection read GetParagraphFrames;
  end;

  { TdxRichEditLayoutFloatingPicture }

  TdxRichEditLayoutFloatingPicture = class(TdxRichEditLayoutFloatingObject)
  strict private
    FImage: TdxOfficeImage;
    function GetImage: TdxOfficeImage;
  protected
    function GetType: TdxRichEditLayoutType; override;
  public
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property Image: TdxOfficeImage read GetImage;
  end;

  { TdxRichEditLayoutPageAreaBase }

  TdxRichEditLayoutPageAreaBase = class abstract(TdxRichEditRangedLayoutElementBase, IdxPieceTableContainer)
  strict private
    FColumns: TdxRichEditLayoutColumnCollection;
    function GetColumns: TdxRichEditLayoutColumnCollection;
  protected
    function GetModelColumns: TdxColumnCollection; virtual; abstract;
    function GetPieceTable: TdxPieceTable; virtual; abstract;
    function CreateRange: TdxRichEditFixedRange; override;

    property PieceTable: TdxPieceTable read GetPieceTable;
    property ModelColumns: TdxColumnCollection read GetModelColumns;
  public
    constructor Create(const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;

    property Columns: TdxRichEditLayoutColumnCollection read GetColumns;
  end;

  { TdxRichEditLayoutComment }

  TdxRichEditLayoutComment = class(TdxRichEditLayoutPageAreaBase)
  strict private
    FComment: TdxCommentViewInfo;
    function GetContentBounds: TRect;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetModelColumns: TdxColumnCollection; override;
    function GetBounds: TRect; override;
    function GetPieceTable: TdxPieceTable; override;

    property ModelComment: TdxCommentViewInfo read FComment;
  public
    constructor Create(AComment: TdxCommentViewInfo; const AParent: IdxRichEditLayoutElement);
    function GetDocumentComment: TdxComment;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
    function BeginUpdate: IdxRichEditSubDocument;
    procedure EndUpdate(ADocument: IdxRichEditSubDocument);
    function GetComment: TdxComment;

    property ContentBounds: TRect read GetContentBounds;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  TdxRichEditLayoutCommentCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditLayoutComment>);

  { TdxRichEditUnderlineBox }

  TdxRichEditUnderlineBox = class(TdxRichEditRangedLayoutElementBase)
  strict private
    FModelBox: TdxModelUnderlineBox;
    function GetThickness: Integer;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
    function CreateRange: TdxRichEditFixedRange; override;

    property ModelBox: TdxModelUnderlineBox read FModelBox;
  public
    constructor Create(AModelBox: TdxModelUnderlineBox; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property Thickness: Integer read GetThickness;
  end;

  TdxRichEditUnderlineBoxCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditUnderlineBox>);

  { TdxRichEditStrikeoutBox }

  TdxRichEditStrikeoutBox = class(TdxRichEditUnderlineBox)
  protected
    function GetType: TdxRichEditLayoutType; override;
  public
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  TdxRichEditStrikeoutBoxCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditStrikeoutBox>);

  { TdxRichEditErrorBox }

  TdxRichEditErrorBox = class(TdxRichEditUnderlineBox)
  protected
    function GetType: TdxRichEditLayoutType; override;
  public
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  TdxRichEditErrorBoxCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditErrorBox>);

  { TdxRichEditHighlightAreaBox }

  TdxRichEditHighlightAreaBox = class(TdxRichEditRangedLayoutElementBase)
  strict private
    FModelBox: TdxHighlightArea;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
    function CreateRange: TdxRichEditFixedRange; override;

    property ModelBox: TdxHighlightArea read FModelBox;
  public
    constructor Create(const AModelBox: TdxHighlightArea; const AParent: IdxRichEditLayoutElement);
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  TdxRichEditHighlightAreaCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditHighlightAreaBox>);

  { TdxRichEditFieldHighlightAreaBox }

  TdxRichEditFieldHighlightAreaBox = class(TdxRichEditHighlightAreaBox)
  protected
    function GetType: TdxRichEditLayoutType; override;
  public
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  TdxRichEditFieldHighlightAreaCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditFieldHighlightAreaBox>);

  { TdxRichEditRangePermissionHighlightAreaBox }

  TdxRichEditRangePermissionHighlightAreaBox = class(TdxRichEditHighlightAreaBox)
  protected
    function GetType: TdxRichEditLayoutType; override;
  public
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  TdxRichEditRangePermissionHighlightAreaCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditRangePermissionHighlightAreaBox>);

  { TdxRichEditCommentHighlightAreaBox }

  TdxRichEditCommentHighlightAreaBox = class(TdxRichEditHighlightAreaBox)
  protected
    function GetType: TdxRichEditLayoutType; override;
  public
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  TdxRichEditCommentHighlightAreaCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditCommentHighlightAreaBox>);

  { TdxRichEditBookmarkBox }

  TdxRichEditBookmarkBox = class(TdxRichEditLayoutElementBase)
  strict private
    FModelBox: TdxVisitableDocumentIntervalBox;
    FType: TdxRichEditLayoutType;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;

    property ModelBox: TdxVisitableDocumentIntervalBox read FModelBox;
  public
    constructor Create(AModelBox: TdxVisitableDocumentIntervalBox; AType: TdxRichEditLayoutType; const AParent: IdxRichEditLayoutElement);
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  TdxRichEditBookmarkBoxCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditBookmarkBox>);

  { TdxRichEditRangePermissionBox }

  TdxRichEditRangePermissionBox = class(TdxRichEditBookmarkBox)
  public
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  TdxRichEditRangePermissionBoxCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditRangePermissionBox>);

  { TdxRichEditCommentBox }

  TdxRichEditCommentBox = class(TdxRichEditBookmarkBox)
  public
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  TdxRichEditCommentBoxCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditCommentBox>);

  { TdxRichEditHiddenTextUnderlineBox }

  TdxRichEditHiddenTextUnderlineBox = class(TdxRichEditRangedLayoutElementBase)
  strict private
    FModelBox: TdxModelHiddenTextUnderlineBox;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
    function CreateRange: TdxRichEditFixedRange; override;

    property ModelBox: TdxModelHiddenTextUnderlineBox read FModelBox;
  public
    constructor Create(AModelBox: TdxModelHiddenTextUnderlineBox; const AParent: IdxRichEditLayoutElement);
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  TdxRichEditHiddenTextUnderlineBoxCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditHiddenTextUnderlineBox>);

  { TdxRichEditLayoutFrame }

  TdxRichEditLayoutFrame = class(TdxRichEditRangedLayoutElementBase,
    IdxRichEditParagraphFrameContainer,
    IdxPieceTableContainer)
  strict private
    FTables: TdxRichEditLayoutTableCollection;
    FRows: TdxRichEditLayoutRowCollection;
    FParagraphFrames: TdxRichEditParagraphFrameBoxCollection;
    FModelBox: TdxParagraphFrameBox;
    function GetTables: TdxRichEditLayoutTableCollection;
    function GetRows: TdxRichEditLayoutRowCollection;
    function GetPieceTable: TdxPieceTable;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
    function GetParagraphFrames: TdxRichEditParagraphFrameBoxCollection;

    property ModelBox: TdxParagraphFrameBox read FModelBox;
  public
    constructor Create(AModelBox: TdxParagraphFrameBox; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
    function CreateRange: TdxRichEditFixedRange; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property Tables: TdxRichEditLayoutTableCollection read GetTables;
    property Rows: TdxRichEditLayoutRowCollection read GetRows;
    property ParagraphFrames: TdxRichEditParagraphFrameBoxCollection read GetParagraphFrames;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxRichEditRowExtendedBoxes }

  TdxRichEditRowExtendedBoxes = class
  strict private
    FBoxes: TdxModelRowExtendedBoxes;
    FParent: TdxRichEditLayoutRow;
    FUnderlines: TdxRichEditUnderlineBoxCollection;
    FStrikeouts: TdxRichEditStrikeoutBoxCollection;
    FErrors: TdxRichEditErrorBoxCollection;
    FHighlightAreas: TdxRichEditHighlightAreaCollection;
    FFieldHighlightAreas: TdxRichEditFieldHighlightAreaCollection;
    FRangePermissionHighlightAreas: TdxRichEditRangePermissionHighlightAreaCollection;
    FCommentHighlightAreas: TdxRichEditCommentHighlightAreaCollection;
    FBookmarkBoxes: TdxRichEditBookmarkBoxCollection;
    FRangePermissionBoxes: TdxRichEditRangePermissionBoxCollection;
    FCommentBoxes: TdxRichEditCommentBoxCollection;
    FHiddenTextUnderlineBoxes: TdxRichEditHiddenTextUnderlineBoxCollection;
    function GetUnderlines: TdxRichEditUnderlineBoxCollection;
    function GetErrors: TdxRichEditErrorBoxCollection;
    function GetStrikeouts: TdxRichEditStrikeoutBoxCollection;
    function GetHighlightAreas: TdxRichEditHighlightAreaCollection;
    function GetFieldHighlightAreas: TdxRichEditFieldHighlightAreaCollection;
    function GetRangePermissionHighlightAreas: TdxRichEditRangePermissionHighlightAreaCollection;
    function GetCommentHighlightAreas: TdxRichEditCommentHighlightAreaCollection;
    function GetBookmarkBoxes: TdxRichEditBookmarkBoxCollection;
    function GetRangePermissionBoxes: TdxRichEditRangePermissionBoxCollection;
    function GetCommentBoxes: TdxRichEditCommentBoxCollection;
    function GetHiddenTextUnderlineBoxes: TdxRichEditHiddenTextUnderlineBoxCollection;
  public
    constructor Create(ABoxes: TdxModelRowExtendedBoxes; AParent: TdxRichEditLayoutRow);
    destructor Destroy; override;

    property Underlines: TdxRichEditUnderlineBoxCollection read GetUnderlines;
    property Strikeouts: TdxRichEditStrikeoutBoxCollection read GetStrikeouts;
    property Errors: TdxRichEditErrorBoxCollection read GetErrors;
    property HighlightAreas: TdxRichEditHighlightAreaCollection read GetHighlightAreas;
    property FieldHighlightAreas: TdxRichEditFieldHighlightAreaCollection read GetFieldHighlightAreas;
    property RangePermissionHighlightAreas: TdxRichEditRangePermissionHighlightAreaCollection read GetRangePermissionHighlightAreas;
    property CommentHighlightAreas: TdxRichEditCommentHighlightAreaCollection read GetCommentHighlightAreas;
    property BookmarkBoxes: TdxRichEditBookmarkBoxCollection read GetBookmarkBoxes;
    property RangePermissionBoxes: TdxRichEditRangePermissionBoxCollection read GetRangePermissionBoxes;
    property CommentBoxes: TdxRichEditCommentBoxCollection read GetCommentBoxes;
    property HiddenTextUnderlineBoxes: TdxRichEditHiddenTextUnderlineBoxCollection read GetHiddenTextUnderlineBoxes;
  end;

  { TdxRichEditLayoutRowExporter }

  TdxRichEditLayoutRowExporter = class(TcxIUnknownObject,
    IdxSimpleDocumentLayoutExporter,
    IdxDocumentLayoutExporter)
  strict private
    FRow: TdxRichEditLayoutRow;
    FBoxes: TdxRichEditBoxCollection;
  public
    constructor Create(ARow: TdxRichEditLayoutRow);
    procedure AddAnchorBox(ABox: TdxModelBox; ACollection: TdxSimpleParagraphBoxCollection);
    function GetBoxIndex(ABox: TdxModelBox; ACollection: TdxSimpleParagraphBoxCollection): Integer;
    procedure ExportRow(ARow: TdxRow);
    procedure ExportSimpleRow(ARow: TdxSimpleRow);
    procedure ExportTextBox(ABox: TdxTextBox);
    procedure ExportSpecialTextBox(ABox: TdxSpecialTextBox);
    procedure ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox);
    procedure ExportHyphenBox(ABox: TdxHyphenBox);
    procedure ExportInlinePictureBox(ABox: TdxModelInlinePictureBox);
    procedure ExportSpaceBox(ABox: TdxModelBox);
    procedure ExportTabSpaceBox(ABox: TdxTabSpaceBox);
    procedure ExportNumberingListBox(ABox: TdxNumberingListBox);
    procedure ExportLineBreakBox(ABox: TdxLineBreakBox);
    procedure ExportPageBreakBox(ABox: TdxPageBreakBox);
    procedure ExportColumnBreakBox(ABox: TdxColumnBreakBox);
    procedure ExportParagraphMarkBox(ABox: TdxParagraphMarkBox);
    procedure ExportSectionMarkBox(ABox: TdxSectionMarkBox);
    procedure ExportSeparatorBox(ABox: TdxSeparatorBox);
    procedure ExportLineNumberBox(ABox: TdxModelLineNumberBox);
    procedure ExportPage(APage: TdxPage);
    procedure ExportParagraphFramePage(APage: TdxPage; const APageClipBounds: TdxRectF; AExportContent: Boolean);
    procedure ExportPageArea(APageArea: TdxPageArea);
    procedure ExportColumn(AColumn: TdxColumn);
    procedure ExportFloatingObjectBox(ABox: TdxFloatingObjectBox);
    procedure ExportParagraphFrameBox(ABox: TdxParagraphFrameBox);
    procedure ExportUnderlineBox(ARow: TdxSimpleRow; AUnderlineBox: TdxModelUnderlineBox);
    procedure ExportStrikeoutBox(ARow: TdxSimpleRow; AStrikeoutBox: TdxModelUnderlineBox);
    procedure ExportErrorBox(AErrorBox: TdxErrorBox);
    procedure ExportBookmarkStartBox(ABox: TdxVisitableDocumentIntervalBox);
    procedure ExportBookmarkEndBox(ABox: TdxVisitableDocumentIntervalBox);
    procedure ExportCommentStartBox(ABox: TdxVisitableDocumentIntervalBox);
    procedure ExportCommentEndBox(ABox: TdxVisitableDocumentIntervalBox);
    procedure ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect);
    procedure ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X: Integer; Y: Integer);
    procedure ExportTableCell(ACell: TdxTableCellViewInfo);
    procedure ExportTableRow(ARow: TdxTableRowViewInfoBase);
    procedure ExportCustomMarkBox(ABox: TdxCustomMarkBox);
    function IsAnchorVisible(const AAnchor: TdxTableCellVerticalAnchor): Boolean;
    function IsTableRowVisible(ARow: TdxTableRowViewInfoBase): Boolean;

    property Boxes: TdxRichEditBoxCollection read FBoxes;
  end;

  { TdxRichEditLayoutPageArea }

  TdxRichEditLayoutPageArea = class(TdxRichEditLayoutPageAreaBase)
  strict private
    FModelPageArea: TdxPageArea;
    FDocument: IdxRichEditDocument;
    function GetDocument: IdxRichEditDocument;
    function GetModelLineNumberBoxes: TdxModelLineNumberBoxCollection;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetModelColumns: TdxColumnCollection; override;
    function GetBounds: TRect; override;
    function GetPieceTable: TdxPieceTable; override;
  public
    constructor Create(APageArea: TdxPageArea; const AParent: IdxRichEditLayoutElement);
    destructor Destroy; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property Document: IdxRichEditDocument read GetDocument;
    property ModelPageArea: TdxPageArea read FModelPageArea;
    property ModelLineNumberBoxes: TdxModelLineNumberBoxCollection read GetModelLineNumberBoxes;
  end;

  TdxRichEditLayoutPageAreaCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditLayoutPageArea>);

  TdxRichEditLayoutFrameCollection = TdxRichEditObjectLayoutElementCollection<TdxRichEditLayoutFrame>;

  { TdxRichEditLayoutPage }

  TdxRichEditLayoutPage = class(TdxRichEditLayoutElementBase)
  strict private
    FPage: TdxPage;
    FDocument: IdxRichEditDocument;
    FPageAreas: TdxRichEditLayoutPageAreaCollection;
    FHeader: TdxRichEditLayoutHeader;
    FFooter: TdxRichEditLayoutFooter;
    FComments: TdxRichEditLayoutCommentCollection;
    FBackgroundFloatingObjects: TdxRichEditLayoutFloatingObjectCollection;
    FFloatingObjects: TdxRichEditLayoutFloatingObjectCollection;
    FForegroundFloatingObjects: TdxRichEditLayoutFloatingObjectCollection;
    FMainContentRange: TdxRichEditFixedRange;
    FFrames: TdxRichEditLayoutFrameCollection;
    function GetPageAreas: TdxRichEditLayoutPageAreaCollection;
    function GetHeader: TdxRichEditLayoutHeader;
    function GetFooter: TdxRichEditLayoutFooter;
    function GetComments: TdxRichEditLayoutCommentCollection;
    function GetBackgroundFloatingObjects: TdxRichEditLayoutFloatingObjectCollection;
    function GetFloatingObjects: TdxRichEditLayoutFloatingObjectCollection;
    function GetForegroundFloatingObjects: TdxRichEditLayoutFloatingObjectCollection;
    function GetIndex: Integer;
    function GetMainContentRange: TdxRichEditFixedRange;
    function CreateParagraphFrames(AModelParagraphFrames: TdxParagraphFrameBoxList): TdxRichEditLayoutFrameCollection;
    function GetFrames: TdxRichEditLayoutFrameCollection;
  protected
    function GetType: TdxRichEditLayoutType; override;
    function GetBounds: TRect; override;
    function CreateFloatingObjects(AModelFloatingObjects: TdxFloatingObjectBoxList): TdxRichEditLayoutFloatingObjectCollection;

    property ModelPage: TdxPage read FPage;
  public
    constructor Create(APage: TdxPage; const ADocument: IdxRichEditDocument);
    destructor Destroy; override;
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;

    property PageAreas: TdxRichEditLayoutPageAreaCollection read GetPageAreas;
    property Header: TdxRichEditLayoutHeader read GetHeader;
    property Footer: TdxRichEditLayoutFooter read GetFooter;
    property Comments: TdxRichEditLayoutCommentCollection read GetComments;
    property BackgroundFloatingObjects: TdxRichEditLayoutFloatingObjectCollection read GetBackgroundFloatingObjects;
    property FloatingObjects: TdxRichEditLayoutFloatingObjectCollection read GetFloatingObjects;
    property ForegroundFloatingObjects: TdxRichEditLayoutFloatingObjectCollection read GetForegroundFloatingObjects;
    property Index: Integer read GetIndex;
    property Document: IdxRichEditDocument read FDocument;
    property MainContentRange: TdxRichEditFixedRange read GetMainContentRange;
    property Frames: TdxRichEditLayoutFrameCollection read GetFrames;
  end;

  TdxRichEditLayoutPageCollection = class(TdxRichEditObjectLayoutElementCollection<TdxRichEditLayoutPage>);

  { TdxRichEditLayoutHeaderFooterBase }

  TdxRichEditLayoutHeaderFooterBase = class abstract(TdxRichEditLayoutPageAreaBase)
  strict private
    FModelHeaderFooter: TdxHeaderFooterPageAreaBase;
    FSection: TdxRichEditNativeSection;
    function DoGetSection: TdxRichEditNativeSection;
  protected
    function GetModelColumns: TdxColumnCollection; override;
    function GetBounds: TRect; override;
    function GetPieceTable: TdxPieceTable; override;
    function GetSection: TdxRichEditNativeSection;

    property ModelHeaderFooter: TdxHeaderFooterPageAreaBase read FModelHeaderFooter;
  public
    constructor Create(AModelHeaderFooter: TdxHeaderFooterPageAreaBase; const AParent: IdxRichEditLayoutElement);
  end;

  { TdxLayoutHeader }

  TdxRichEditLayoutHeader = class(TdxRichEditLayoutHeaderFooterBase)
  protected
    function GetType: TdxRichEditLayoutType; override;
  public
    constructor Create(AHeader: TdxHeaderPageArea; const AParent: IdxRichEditLayoutElement);
    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  { TdxRichEditLayoutFooter }

  TdxRichEditLayoutFooter = class(TdxRichEditLayoutHeaderFooterBase)
  protected
    function GetType: TdxRichEditLayoutType; override;
  public
    constructor Create(AFooter: TdxFooterPageArea; const AParent: IdxRichEditLayoutElement);

    procedure Accept(AVisitor: TdxRichEditLayoutVisitor); override;
  end;

  { TdxRichEditPagePainter }

  TdxRichEditPagePainter = class
  strict private
    FPage: TdxRichEditLayoutPage;
    FExporter: TdxDocumentLayoutExporter;
    FModelPage: TdxPage;
    FCanvas: IdxRichEditPageCanvas;
    FPainterVisitor: TdxRichEditPagePainterVisitor;
  public
    constructor Create;
    destructor Destroy; override;
    function ShouldOptimizeBox(ABox: IdxRichEditLayoutElement): Boolean; virtual;
    function GetDrawingBounds(const ABounds: TRect): TRect;
    procedure Draw; virtual;
    procedure DrawPage(APage: TdxRichEditLayoutPage); virtual;
    procedure DrawPageArea(APageArea: TdxRichEditLayoutPageArea); virtual;
    procedure DrawColumn(AColumn: TdxRichEditLayoutColumn); virtual;
    procedure DrawComments(APage: TdxRichEditLayoutPage);
    procedure DrawComment(AComment: TdxRichEditLayoutComment); virtual;
    procedure DrawHeader(AHeader: TdxRichEditLayoutHeader); virtual;
    procedure DrawFooter(AFooter: TdxRichEditLayoutFooter); virtual;
    procedure DrawFrame(AFrame: TdxRichEditLayoutFrame); virtual;
    procedure DrawParagraphFrame(AParagraphFrame: TdxRichEditParagraphFrameBox); virtual;
    procedure DrawTextBox(ATextBox: TdxRichEditLayoutTextBox); virtual;
    procedure DrawFloatingPicture(AFloatingPicture: TdxRichEditLayoutFloatingPicture); virtual;
    procedure DrawTable(ATable: TdxRichEditLayoutTable); virtual;
    procedure DrawTableRow(ATableRow: TdxRichEditLayoutTableRow); virtual;
    procedure DrawTableCell(ATableCell: TdxRichEditLayoutTableCell); virtual;
    function ShouldExportRowBackground(ARowExtendedBoxes: TdxRichEditRowExtendedBoxes): Boolean;
    procedure DrawRowBackground(ARowExtendedBoxes: TdxRichEditRowExtendedBoxes);
    function ShouldDrawRow(ARow: TdxRichEditLayoutRow): Boolean;
    procedure DrawRow(ARow: TdxRichEditLayoutRow); virtual;
    procedure DrawPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox); virtual;
    procedure DrawSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox); virtual;
    procedure DrawInlinePictureBox(AInlinePictureBox: TdxRichEditInlinePictureBox); virtual;
    procedure DrawFloatingObjectAnchorBox(AFloatingObjectAnchorBox: TdxRichEditFloatingObjectAnchorBox); virtual;
    procedure DrawSpaceBox(ASpaceBox: TdxRichEditPlainTextBox); virtual;
    procedure DrawParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox); virtual;
    procedure DrawSectionBreakBox(ASectionBreakBox: TdxRichEditPlainTextBox); virtual;
    procedure DrawLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox); virtual;
    procedure DrawPageBreakBox(APageBreakBox: TdxRichEditPlainTextBox); virtual;
    procedure DrawColumnBreakBox(AColumnBreakBox: TdxRichEditPlainTextBox); virtual;
    procedure DrawHyphenBox(AHyphen: TdxRichEditPlainTextBox); virtual;
    procedure DrawTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox); virtual;
    procedure DrawPageNumberBox(APageNumberBox: TdxRichEditPlainTextBox); virtual;
    procedure DrawNumberingListMarkBox(ANumberingListMarkBox: TdxRichEditNumberingListMarkBox); virtual;
    procedure DrawNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox: TdxRichEditNumberingListWithSeparatorBox); virtual;
    procedure DrawUnderlineBoxes(AUnderlineBoxes: TdxRichEditUnderlineBoxCollection);
    procedure DrawStrikeoutBoxes(AStrikeoutBoxes: TdxRichEditStrikeoutBoxCollection);
    procedure DrawBookmarkBoxes(ABookmarkBoxes: TdxRichEditBookmarkBoxCollection);
    procedure DrawUnderlineBox(AUnderlineBox: TdxRichEditUnderlineBox); virtual;
    procedure DrawStrikeoutBox(AStrikeoutBox: TdxRichEditStrikeoutBox); virtual;
    procedure DrawErrorBox(AErrorBox: TdxRichEditErrorBox); virtual;
    procedure DrawHighlightAreaBox(AHighlightAreaBox: TdxRichEditHighlightAreaBox); virtual;
    procedure DrawBookmarkStartBox(ABookmarkStartBox: TdxRichEditBookmarkBox); virtual;
    procedure DrawBookmarkEndBox(ABookmarkEndBox: TdxRichEditBookmarkBox); virtual;
    procedure DrawHiddenTextUnderlineBox(AHiddenTextUnderlineBox: TdxRichEditHiddenTextUnderlineBox); virtual;
    procedure DrawLineNumberBox(ALineNumberBox: TdxRichEditLineNumberBox); virtual;
    procedure DrawFieldHighlightAreaBox(AFieldHighlightAreaBox: TdxRichEditFieldHighlightAreaBox); virtual;
    procedure DrawRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox: TdxRichEditRangePermissionHighlightAreaBox); virtual;
    procedure DrawRangePermissionStartBox(ARangePermissionStartBox: TdxRichEditRangePermissionBox); virtual;
    procedure DrawRangePermissionEndBox(ARangePermissionEndBox: TdxRichEditRangePermissionBox); virtual;
    procedure DrawCommentHighlightAreaBox(ACommentHighlightAreaBox: TdxRichEditCommentHighlightAreaBox); virtual;
    procedure DrawCommentStartBox(ACommentStartBox: TdxRichEditCommentBox); virtual;
    procedure DrawCommentEndBox(ACommentEndBox: TdxRichEditCommentBox); virtual;

    property Page: TdxRichEditLayoutPage read FPage write FPage;
    property Exporter: TdxDocumentLayoutExporter read FExporter write FExporter;
    property ModelPage: TdxPage read FModelPage write FModelPage;
    property Canvas: IdxRichEditPageCanvas read FCanvas write FCanvas;
  end;

  { TdxRichEditLayoutVisitor }

  TdxRichEditLayoutVisitor = class abstract
  protected
    procedure VisitPage(APage: TdxRichEditLayoutPage); virtual;
    procedure VisitPageArea(APageArea: TdxRichEditLayoutPageArea); virtual;
    procedure VisitColumn(AColumn: TdxRichEditLayoutColumn); virtual;
    procedure VisitComments(APage: TdxRichEditLayoutPage); virtual;
    procedure VisitComment(AComment: TdxRichEditLayoutComment); virtual;
    procedure VisitHeader(AHeader: TdxRichEditLayoutHeader); virtual;
    procedure VisitFooter(AFooter: TdxRichEditLayoutFooter); virtual;
    procedure VisitTextBox(ATextBox: TdxRichEditLayoutTextBox); virtual;
    procedure VisitFloatingPicture(AFloatingPicture: TdxRichEditLayoutFloatingPicture); virtual;
    procedure VisitTable(ATable: TdxRichEditLayoutTable); virtual;
    procedure VisitTableRow(ATableRow: TdxRichEditLayoutTableRow); virtual;
    procedure VisitTableCell(ATableCell: TdxRichEditLayoutTableCell); virtual;
    procedure VisitRowBackground(ARowExtendedBoxes: TdxRichEditRowExtendedBoxes); virtual;
    procedure VisitRow(ARow: TdxRichEditLayoutRow); virtual;
    procedure VisitUnderlineBoxes(AUnderlineBoxes: TdxRichEditUnderlineBoxCollection); virtual;
    procedure VisitStrikeoutBoxes(AStrikeoutBoxes: TdxRichEditStrikeoutBoxCollection); virtual;
    procedure VisitErrorBoxes(AErrorBoxes: TdxRichEditErrorBoxCollection); virtual;
    procedure VisitBookmarkBoxes(ABookmarkBoxes: TdxRichEditBookmarkBoxCollection); virtual;
    procedure VisitPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox); virtual;
    procedure VisitSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox); virtual;
    procedure VisitInlinePictureBox(AInlinePictureBox: TdxRichEditInlinePictureBox); virtual;
    procedure VisitFloatingObjectAnchorBox(AFloatingObjectAnchorBox: TdxRichEditFloatingObjectAnchorBox); virtual;
    procedure VisitSpaceBox(ASpaceBox: TdxRichEditPlainTextBox); virtual;
    procedure VisitParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox); virtual;
    procedure VisitSectionBreakBox(ASectionBreakBox: TdxRichEditPlainTextBox); virtual;
    procedure VisitLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox); virtual;
    procedure VisitPageBreakBox(APageBreakBox: TdxRichEditPlainTextBox); virtual;
    procedure VisitColumnBreakBox(AColumnBreakBox: TdxRichEditPlainTextBox); virtual;
    procedure VisitHyphenBox(AHyphen: TdxRichEditPlainTextBox); virtual;
    procedure VisitTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox); virtual;
    procedure VisitPageNumberBox(APageNumberBox: TdxRichEditPlainTextBox); virtual;
    procedure VisitNumberingListMarkBox(ANumberingListMarkBox: TdxRichEditNumberingListMarkBox); virtual;
    procedure VisitNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox: TdxRichEditNumberingListWithSeparatorBox); virtual;
    procedure VisitUnderlineBox(AUnderlineBox: TdxRichEditUnderlineBox); virtual;
    procedure VisitStrikeoutBox(AStrikeoutBox: TdxRichEditStrikeoutBox); virtual;
    procedure VisitErrorBox(AErrorBox: TdxRichEditErrorBox); virtual;
    procedure VisitHighlightAreaBox(AHighlightAreaBox: TdxRichEditHighlightAreaBox); virtual;
    procedure VisitBookmarkStartBox(ABookmarkStartBox: TdxRichEditBookmarkBox); virtual;
    procedure VisitBookmarkEndBox(ABookmarkEndBox: TdxRichEditBookmarkBox); virtual;
    procedure VisitHiddenTextUnderlineBox(AHiddenTextUnderlineBox: TdxRichEditHiddenTextUnderlineBox); virtual;
    procedure VisitLineNumberBox(ALineNumberBox: TdxRichEditLineNumberBox); virtual;
    procedure VisitFieldHighlightAreaBox(AFieldHighlightAreaBox: TdxRichEditFieldHighlightAreaBox); virtual;
    procedure VisitRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox: TdxRichEditRangePermissionHighlightAreaBox); virtual;
    procedure VisitRangePermissionStartBox(ARangePermissionStartBox: TdxRichEditRangePermissionBox); virtual;
    procedure VisitRangePermissionEndBox(ARangePermissionEndBox: TdxRichEditRangePermissionBox); virtual;
    procedure VisitCommentHighlightAreaBox(ACommentHighlightAreaBox: TdxRichEditCommentHighlightAreaBox); virtual;
    procedure VisitCommentStartBox(ACommentStartBox: TdxRichEditCommentBox); virtual;
    procedure VisitCommentEndBox(ACommentEndBox: TdxRichEditCommentBox); virtual;
    procedure VisitFrame(AFrame: TdxRichEditLayoutFrame); virtual;
    procedure VisitParagraphFrameBox(AParagraphFrameBox: TdxRichEditParagraphFrameBox); virtual;
    procedure VisitLayoutElementsCollection(ACollection: TdxRichEditLayoutElementCollection);
    procedure VisitLayoutRowCollection(ACollection: TdxRichEditLayoutRowCollection);
    procedure VisitParagraphFrameBoxes(const AParagraphFrameContainer: IdxRichEditParagraphFrameContainer; const ARowParent: IdxRichEditLayoutElement); virtual;
    procedure VisitArea(AArea: TdxRichEditLayoutPageAreaBase);
    procedure VisitFloatingObjects(ACollection: TdxRichEditLayoutFloatingObjectCollection; APieceTable: TdxPieceTable);
    procedure VisitFrames(ACollection: TdxRichEditLayoutFrameCollection; APieceTable: TdxPieceTable);
  public
    procedure Visit(const AElement: IdxRichEditLayoutElement); virtual;
  end;

  { TdxRichEditCommentVisitor }

  TdxRichEditCommentVisitor = class(TdxRichEditLayoutVisitor)
  strict private
    FComment: TdxRichEditLayoutComment;
    procedure OffsetBounds(ABox: TdxRichEditLayoutElementBase; AXOffset: Integer; AYOffset: Integer); overload;
    procedure OffsetBounds(ABox: TdxRichEditLayoutElementBase); overload;
  protected
    procedure VisitColumn(AColumn: TdxRichEditLayoutColumn); override;
    procedure VisitBookmarkEndBox(ABookmarkEndBox: TdxRichEditBookmarkBox); override;
    procedure VisitBookmarkStartBox(ABookmarkStartBox: TdxRichEditBookmarkBox); override;
    procedure VisitRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox: TdxRichEditRangePermissionHighlightAreaBox); override;
    procedure VisitRangePermissionStartBox(ARangePermissionStartBox: TdxRichEditRangePermissionBox); override;
    procedure VisitRangePermissionEndBox(ARangePermissionEndBox: TdxRichEditRangePermissionBox); override;
    procedure VisitFieldHighlightAreaBox(AFieldHighlightAreaBox: TdxRichEditFieldHighlightAreaBox); override;
    procedure VisitHiddenTextUnderlineBox(AHiddenTextUnderlineBox: TdxRichEditHiddenTextUnderlineBox); override;
    procedure VisitHighlightAreaBox(AHighlightAreaBox: TdxRichEditHighlightAreaBox); override;
    procedure VisitHyphenBox(AHyphen: TdxRichEditPlainTextBox); override;
    procedure VisitInlinePictureBox(AInlinePictureBox: TdxRichEditInlinePictureBox); override;
    procedure VisitLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitNumberingListMarkBox(ANumberingListMarkBox: TdxRichEditNumberingListMarkBox); override;
    procedure VisitNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox: TdxRichEditNumberingListWithSeparatorBox); override;
    procedure VisitParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox); override;
    procedure VisitPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox); override;
    procedure VisitRow(ARow: TdxRichEditLayoutRow); override;
    procedure VisitSpaceBox(ASpaceBox: TdxRichEditPlainTextBox); override;
    procedure VisitSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox); override;
    procedure VisitStrikeoutBox(AStrikeoutBox: TdxRichEditStrikeoutBox); override;
    procedure VisitTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox); override;
    procedure VisitUnderlineBox(AUnderlineBox: TdxRichEditUnderlineBox); override;
    procedure VisitErrorBox(AErrorBox: TdxRichEditErrorBox); override;
  public
    constructor Create(AComment: TdxRichEditLayoutComment);
  end;

  { TdxRichEditPagePainterVisitor }

  TdxRichEditPagePainterVisitor = class(TdxRichEditLayoutVisitor)
  strict private
    FPainter: TdxRichEditPagePainter;
  protected
    procedure VisitPage(APage: TdxRichEditLayoutPage); override;
    procedure VisitRowBackground(ARowExtendedBoxes: TdxRichEditRowExtendedBoxes); override;
    procedure VisitRow(ARow: TdxRichEditLayoutRow); override;
    procedure VisitUnderlineBoxes(AUnderlineBoxes: TdxRichEditUnderlineBoxCollection); override;
    procedure VisitStrikeoutBoxes(AStrikeoutBoxes: TdxRichEditStrikeoutBoxCollection); override;
    procedure VisitBookmarkBoxes(ABookmarkBoxes: TdxRichEditBookmarkBoxCollection); override;
    procedure VisitTable(ATable: TdxRichEditLayoutTable); override;
    procedure VisitTableRow(ATableRow: TdxRichEditLayoutTableRow); override;
    procedure VisitTableCell(ATableCell: TdxRichEditLayoutTableCell); override;
    procedure VisitPageArea(APageArea: TdxRichEditLayoutPageArea); override;
    procedure VisitColumn(AColumn: TdxRichEditLayoutColumn); override;
    procedure VisitComments(APage: TdxRichEditLayoutPage); override;
    procedure VisitComment(AComment: TdxRichEditLayoutComment); override;
    procedure VisitHeader(AHeader: TdxRichEditLayoutHeader); override;
    procedure VisitFooter(AFooter: TdxRichEditLayoutFooter); override;
    procedure VisitTextBox(ATextBox: TdxRichEditLayoutTextBox); override;
    procedure VisitFloatingPicture(AFloatingPicture: TdxRichEditLayoutFloatingPicture); override;
    procedure VisitPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox); override;
    procedure VisitSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox); override;
    procedure VisitInlinePictureBox(AInlinePictureBox: TdxRichEditInlinePictureBox); override;
    procedure VisitFloatingObjectAnchorBox(AFloatingObjectAnchorBox: TdxRichEditFloatingObjectAnchorBox); override;
    procedure VisitSpaceBox(ASpaceBox: TdxRichEditPlainTextBox); override;
    procedure VisitParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox); override;
    procedure VisitSectionBreakBox(ASectionBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitPageBreakBox(APageBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitColumnBreakBox(AColumnBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitHyphenBox(AHyphenBox: TdxRichEditPlainTextBox); override;
    procedure VisitTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox); override;
    procedure VisitPageNumberBox(APageNumberBox: TdxRichEditPlainTextBox); override;
    procedure VisitNumberingListMarkBox(ANumberingListMarkBox: TdxRichEditNumberingListMarkBox); override;
    procedure VisitNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox: TdxRichEditNumberingListWithSeparatorBox); override;
    procedure VisitUnderlineBox(AUnderlineBox: TdxRichEditUnderlineBox); override;
    procedure VisitStrikeoutBox(AStrikeoutBox: TdxRichEditStrikeoutBox); override;
    procedure VisitErrorBox(AErrorBox: TdxRichEditErrorBox); override;
    procedure VisitHighlightAreaBox(AHighlightAreaBox: TdxRichEditHighlightAreaBox); override;
    procedure VisitBookmarkStartBox(ABookmarkStartBox: TdxRichEditBookmarkBox); override;
    procedure VisitBookmarkEndBox(ABookmarkEndBox: TdxRichEditBookmarkBox); override;
    procedure VisitHiddenTextUnderlineBox(AHiddenTextUnderlineBox: TdxRichEditHiddenTextUnderlineBox); override;
    procedure VisitLineNumberBox(ALineNumberBox: TdxRichEditLineNumberBox); override;
    procedure VisitFieldHighlightAreaBox(AFieldHighlightAreaBox: TdxRichEditFieldHighlightAreaBox); override;
    procedure VisitRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox: TdxRichEditRangePermissionHighlightAreaBox); override;
    procedure VisitRangePermissionStartBox(ARangePermissionStartBox: TdxRichEditRangePermissionBox); override;
    procedure VisitRangePermissionEndBox(ARangePermissionEndBox: TdxRichEditRangePermissionBox); override;
    procedure VisitCommentHighlightAreaBox(ACommentHighlightAreaBox: TdxRichEditCommentHighlightAreaBox); override;
    procedure VisitCommentStartBox(ACommentStartBox: TdxRichEditCommentBox); override;
    procedure VisitCommentEndBox(ACommentEndBox: TdxRichEditCommentBox); override;
    procedure VisitFrame(AFrame: TdxRichEditLayoutFrame); override;
    procedure VisitParagraphFrameBox(AParagraphFrameBox: TdxRichEditParagraphFrameBox); override;
    procedure DefaultVisitPage(APage: TdxRichEditLayoutPage);
    procedure DefaultVisitPageArea(APageArea: TdxRichEditLayoutPageArea);
    procedure DefaultVisitColumn(AColumn: TdxRichEditLayoutColumn);
    procedure DefaultVisitComments(APage: TdxRichEditLayoutPage);
    procedure DefaultVisitComment(AComment: TdxRichEditLayoutComment);
    procedure DefaultVisitHeader(AHeader: TdxRichEditLayoutHeader);
    procedure DefaultVisitFooter(AFooter: TdxRichEditLayoutFooter);
    procedure DefaultVisitTextBox(ATextBox: TdxRichEditLayoutTextBox);
    procedure DefaultVisitTable(ATable: TdxRichEditLayoutTable);
    procedure DefaultVisitTableRow(ATableRow: TdxRichEditLayoutTableRow);
    procedure DefaultVisitTableCell(ATableCell: TdxRichEditLayoutTableCell);
    procedure DefaultVisitRowBackground(ARowExtendedBoxes: TdxRichEditRowExtendedBoxes);
    procedure DefaultVisitRow(ARow: TdxRichEditLayoutRow);
    procedure DefaultVisitUnderlineBoxes(AUnderlineBoxes: TdxRichEditUnderlineBoxCollection);
    procedure DefaultVisitStrikeoutBoxes(AStrikeoutBoxes: TdxRichEditStrikeoutBoxCollection);
    procedure DefaultVisitBookmarkBoxes(ABookmarkBoxes: TdxRichEditBookmarkBoxCollection);
  public
    constructor Create(APainter: TdxRichEditPagePainter);
  end;

  { TdxRichEditLayoutElementSearcher }

  TdxRichEditLayoutElementSearcher = class(TdxRichEditLayoutVisitor)
  strict private
    FPosition: Integer;
    FPieceTable: TdxPieceTable;
    FFoundElement: IdxRichEditRangedLayoutElement;
    FComparison: TdxFunc<IdxRichEditRangedLayoutElement, Boolean>;
  protected
    procedure VisitColumn(AColumn: TdxRichEditLayoutColumn); override;
    procedure VisitColumnBreakBox(AColumnBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitComment(AComment: TdxRichEditLayoutComment); override;
    procedure VisitCommentHighlightAreaBox(ACommentHighlightAreaBox: TdxRichEditCommentHighlightAreaBox); override;
    procedure VisitFieldHighlightAreaBox(AFieldHighlightAreaBox: TdxRichEditFieldHighlightAreaBox); override;
    procedure VisitFloatingObjectAnchorBox(AFloatingObjectAnchorBox: TdxRichEditFloatingObjectAnchorBox); override;
    procedure VisitFooter(AFooter: TdxRichEditLayoutFooter); override;
    procedure VisitHeader(AHeader: TdxRichEditLayoutHeader); override;
    procedure VisitHiddenTextUnderlineBox(AHiddenTextUnderlineBox: TdxRichEditHiddenTextUnderlineBox); override;
    procedure VisitHighlightAreaBox(AHighlightAreaBox: TdxRichEditHighlightAreaBox); override;
    procedure VisitHyphenBox(AHyphen: TdxRichEditPlainTextBox); override;
    procedure VisitInlinePictureBox(AInlinePictureBox: TdxRichEditInlinePictureBox); override;
    procedure VisitLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitPageArea(APageArea: TdxRichEditLayoutPageArea); override;
    procedure VisitPageBreakBox(APageBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitPageNumberBox(APageNumberBox: TdxRichEditPlainTextBox); override;
    procedure VisitParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox); override;
    procedure VisitPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox); override;
    procedure VisitRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox: TdxRichEditRangePermissionHighlightAreaBox); override;
    procedure VisitRow(ARow: TdxRichEditLayoutRow); override;
    procedure VisitSectionBreakBox(ASectionBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitSpaceBox(ASpaceBox: TdxRichEditPlainTextBox); override;
    procedure VisitSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox); override;
    procedure VisitStrikeoutBox(AStrikeoutBox: TdxRichEditStrikeoutBox); override;
    procedure VisitTable(ATable: TdxRichEditLayoutTable); override;
    procedure VisitTableCell(ATableCell: TdxRichEditLayoutTableCell); override;
    procedure VisitTableRow(ATableRow: TdxRichEditLayoutTableRow); override;
    procedure VisitTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox); override;
    procedure VisitTextBox(ATextBox: TdxRichEditLayoutTextBox); override;
    procedure VisitUnderlineBox(AUnderlineBox: TdxRichEditUnderlineBox); override;
    procedure FindElement(AElement: IdxRichEditRangedLayoutElement);
    function IsInSearchingDocument(const AContainer: IdxPieceTableContainer): Boolean;
  public
    constructor Create(APosition: Integer; APieceTable: TdxPieceTable;
      const AComparison: TdxFunc<IdxRichEditRangedLayoutElement, Boolean>);

    property Position: Integer read FPosition;
    property PieceTable: TdxPieceTable read FPieceTable;
    property Comparison: TdxFunc<IdxRichEditRangedLayoutElement, Boolean> read FComparison;
    property FoundElement: IdxRichEditRangedLayoutElement read FFoundElement;
  end;

  { TdxRichEditDocumentLayout }

  TdxRichEditDocumentLayout = class
  strict private
    FProvider: IdxRichEditDocumentLayoutProvider;
    FPages: TdxRichEditLayoutPageCollection;
    FIsDocumentFormattingCompleted: Boolean;
    FDocumentFormatted: TdxEventHandler;
    FPageFormatted: TdxPageFormattedEventHandler;
    FDocumentFormattedResetEvent: TSimpleEvent;
    FDocumentLayoutInvalidated: TdxDocumentLayoutInvalidatedEventHandler;
    function GetDocument: IdxRichEditDocument;
    function GetDocumentFormattedResetEvent: TSimpleEvent;
  protected
    procedure SubscribeToEvents;
    procedure UnsubscribeFromEvents;
    procedure InitializePages(ADocumentLayout: TdxModelDocumentLayout; APageIndex: Integer);

    property Pages: TdxRichEditLayoutPageCollection read FPages;
    property DocumentFormattedResetEvent: TSimpleEvent read GetDocumentFormattedResetEvent;
  public
    constructor Create(const AProvider: IdxRichEditDocumentLayoutProvider);
    destructor Destroy; override;
    procedure RaiseDocumentLayoutInvalidated(AArgs: TdxDocumentLayoutInvalidatedEventArgs);
    procedure RaiseDocumentFormatted;
    procedure RaisePageFormatted(AArgs: TdxPageFormattedEventArgs);
    procedure OnDocumentLayoutInvalidated(ASender: TObject; E: TdxDocumentLayoutInvalidatedEventArgs);
    procedure OnDocumentFormatted(ASender: TObject; E: TdxEventArgs);
    procedure OnPageFormatted(ASender: TObject; E: TdxPageFormattedEventArgs);
    function GetPage(APageIndex: Integer): TdxRichEditLayoutPage;
    function GetPageCore(APageIndex: Integer; ADocumentLayout: TdxModelDocumentLayout): TdxRichEditLayoutPage;
    procedure ThrowNullCallbackException; virtual;
    procedure ThrowInvalidAsyncOperationException; virtual;
    function TryGetPageAsyncSynchronously(APageIndex: Integer): TdxRichEditLayoutPage;
    function GetPageIndex(AElement: IdxRichEditLayoutElement): Integer;
    function GetFormattedPageCount: Integer;
    function GetPageCount: Integer;
    class function GetText(AElement: IdxRichEditLayoutElement): string; static;
    function IsRangeSupported(AType: TdxRichEditLayoutType): Boolean;
    class function GetElementFromPage(APosition: Integer; AClass: TClass;
      APieceTable: TdxPieceTable; APage: TdxRichEditLayoutPage): TObject; static;

    property DocumentLayoutInvalidated: TdxDocumentLayoutInvalidatedEventHandler read FDocumentLayoutInvalidated;
    property DocumentFormatted: TdxEventHandler read FDocumentFormatted;
    property PageFormatted: TdxPageFormattedEventHandler read FPageFormatted;
    property Provider: IdxRichEditDocumentLayoutProvider read FProvider;
    property IsDocumentFormattingCompleted: Boolean read FIsDocumentFormattingCompleted write FIsDocumentFormattingCompleted;
    property Document: IdxRichEditDocument read GetDocument;
  end;

  { TdxRichEditDocumentLayoutHelper }

  TdxRichEditDocumentLayoutHelper = class
  protected type
    TdxParagraphFrameDrawingParentSearcher = class(TdxRichEditLayoutVisitor)
    strict private
      FParagraphFrame: TdxParagraphFrameBox;
      FResult: IdxRichEditLayoutElement;
    protected
      procedure VisitRow(ARow: TdxRichEditLayoutRow); override;
      procedure VisitTextBox(ATextBox: TdxRichEditLayoutTextBox); override;
      procedure VisitColumn(AColumn: TdxRichEditLayoutColumn); override;
      procedure VisitTable(ATable: TdxRichEditLayoutTable); override;
      procedure VisitTableRow(ATableRow: TdxRichEditLayoutTableRow); override;
      procedure VisitTableCell(ATableCell: TdxRichEditLayoutTableCell); override;
      procedure VisitParagraphFrameBoxes(const AParagraphFrameContainer: IdxRichEditParagraphFrameContainer;
        const ARowParent: IdxRichEditLayoutElement); override;
    public
      constructor Create(AParagraphFrame: TdxParagraphFrameBox);

      property Result: IdxRichEditLayoutElement read FResult;
    end;
  protected
    class function CalculateLayoutBorderThickness(ABorderWidth: Integer; AConverter: TdxDocumentModelUnitToLayoutUnitConverter): Integer; static;
  public
    class function CreateLayoutRowCollection(AModelRows: TdxRowCollection; const AParent: IdxRichEditLayoutElement;
      AIsTableCellRow: Boolean): TdxRichEditLayoutRowCollection; static;
    class procedure InitializeRowCollection(AModelRows: TdxRowCollection; ALayoutRows: TdxRichEditLayoutRowCollection;
      const AParent: IdxRichEditLayoutElement); static;
    class procedure InitializeTableCellRowCollection(AModelRows: TdxRowCollection;
      ALayoutRows: TdxRichEditLayoutRowCollection; const AParent: IdxRichEditLayoutElement); static;
    class function CreateLayoutTableCollection(AModelTables: TdxTableViewInfoCollection;
      const AParent: IdxRichEditLayoutElement): TdxRichEditLayoutTableCollection; static;
    class function CreateBorders(const AElement: IdxRichEditLayoutElement; ALeftBorder,
      ARightBorder, ATopBorder, ABottomBorder: TdxBorderInfo): TdxRichEditBorders; static;
    class function GetRangeForTablesAndRowsContainer(ARows: TdxRichEditLayoutRowCollection;
      ATables: TdxRichEditLayoutTableCollection): TdxRichEditFixedRange; static;
    class function GetRangeFromRowByCoordinates(ARow: TdxRichEditLayoutRow; AX1: Integer; AX2: Integer): TdxRichEditFixedRange; static;
    class function CreateParagraphFrameBoxCollection(AModelColumn: TdxColumn; const AElement: IdxRichEditLayoutElement): TdxRichEditParagraphFrameBoxCollection; static;
  end;

  { TdxRichEditTextCollectorVisitor }

  TdxRichEditTextCollectorVisitor = class(TdxRichEditLayoutVisitor)
  strict private
    FText: string;
  protected
    procedure VisitColumnBreakBox(AColumnBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitHyphenBox(AHyphen: TdxRichEditPlainTextBox); override;
    procedure VisitLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitLineNumberBox(ALineNumberBox: TdxRichEditLineNumberBox); override;
    procedure VisitNumberingListMarkBox(ANumberingListMarkBox: TdxRichEditNumberingListMarkBox); override;
    procedure VisitNumberingListWithSeparatorBox(
      ANumberingListWithSeparatorBox: TdxRichEditNumberingListWithSeparatorBox); override;
    procedure VisitPageBreakBox(APageBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox); override;
    procedure VisitPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox); override;
    procedure VisitSectionBreakBox(ASectionBreakBox: TdxRichEditPlainTextBox); override;
    procedure VisitSpaceBox(ASpaceBox: TdxRichEditPlainTextBox); override;
    procedure VisitSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox); override;
    procedure VisitTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox); override;
    procedure VisitPageNumberBox(APageNumberBox: TdxRichEditPlainTextBox); override;
  public
    property Text: string read FText write FText;
  end;

implementation

uses
  Math,
  dxTypeHelpers,
  dxRichEdit.Api.NativeDocumentBase,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.Utils.Graphics;

type
  TdxDocumentLayoutExporterAccess = class(TdxDocumentLayoutExporter);

  TdxRichEditPagePainterHelper = class helper for TdxRichEditPagePainter
  private
    function GetExporter: TdxDocumentLayoutExporterAccess;
  public
    property Exporter: TdxDocumentLayoutExporterAccess read GetExporter;
  end;

function TdxRichEditPagePainterHelper.GetExporter: TdxDocumentLayoutExporterAccess;
begin
  Result := TdxDocumentLayoutExporterAccess(inherited Exporter);
end;

{ TdxRichEditLayoutBorder }

constructor TdxRichEditLayoutBorder.Create(AColor: TdxAlphaColor; AStyle: TdxRichEditTableBorderLineStyle;
  AThickness: Integer);
begin
  FColor := AColor;
  FStyle := AStyle;
  FThickness := AThickness;
end;

{ TdxRichEditBorders }

constructor TdxRichEditBorders.Create(const ALeft, ARight, ATop, ABottom: TdxRichEditLayoutBorder);
begin
  FLeft := ALeft;
  FRight := ARight;
  FTop := ATop;
  FBottom := ABottom;
end;

{ TdxRichEditBeforePagePaintEventArgs }

constructor TdxRichEditBeforePagePaintEventArgs.Create(APainter: TdxRichEditPagePainter; APageIndex: Integer);
begin
  inherited Create;
  FPainter := APainter;
  FPageIndex := APageIndex;
end;

function TdxRichEditBeforePagePaintEventArgs.GetCanvas: IdxRichEditPageCanvas;
begin
  Result := FPainter.Canvas;
end;

function TdxRichEditBeforePagePaintEventArgs.GetPage: TdxRichEditLayoutPage;
begin
  Result := Painter.Page;
end;

{ TdxRichEditLayoutElementBase }

constructor TdxRichEditLayoutElementBase.Create(const AParent: IdxRichEditLayoutElement);
begin
  inherited Create;
  FParent := AParent;
end;

function TdxRichEditLayoutElementBase.InternalGetBounds: TRect;
begin
  if FBounds.IsZero then
    FBounds := GetBounds;
  Result := FBounds;
end;

procedure TdxRichEditLayoutElementBase.SetBounds(const ABounds: TRect);
begin
  FBounds := ABounds;
end;

function TdxRichEditLayoutElementBase.GetParentByType(AClass: TClass): TObject;
begin
  Result := GetParentByTypeCore(AClass, FParent);
end;

function TdxRichEditLayoutElementBase.GetParentByIntf(const IID: TGUID): IUnknown;
begin
  Result := GetParentByIntfCore(IID, FParent);
end;

function TdxRichEditLayoutElementBase.GetParentByType<T>: T;
var
  AClass: TClass;
begin
  AClass := TClass(T);
  Result := T(GetParentByType(AClass));
end;

function TdxRichEditLayoutElementBase.GetParentByTypeCore(AClass: TClass;
  const AParent: IdxRichEditLayoutElement): TObject;
begin
  if AParent = nil then
    Exit(nil);
  if AParent.AsObject.ClassType = AClass then
    Exit(AParent.AsObject);
  Result := GetParentByTypeCore(AClass, AParent.Parent);
end;

function TdxRichEditLayoutElementBase.GetParentByIntfCore(const IID: TGUID; const AParent: IdxRichEditLayoutElement): IUnknown;
begin
  if AParent = nil then
    Exit(nil);
  if Supports(AParent, IID, Result) then
    Exit;
  Result := GetParentByIntfCore(IID, AParent.Parent);
end;

function TdxRichEditLayoutElementBase.GetRelativeBounds(const AElement: IdxRichEditLayoutElement): TRect;
begin
  Result.Empty;
  Result.Height := Bounds.Height;
  Result.Width := Bounds.Width;
  Result.X := Bounds.X - AElement.Bounds.X;
  Result.Y := Bounds.Y - AElement.Bounds.Y;
end;

function TdxRichEditLayoutElementBase.GetAsObject: TObject;
begin
  Result := Self;
end;

function TdxRichEditLayoutElementBase.GetParent: IdxRichEditLayoutElement;
begin
  Result := FParent;
end;

{ TdxRichEditParagraphFrameBox }

constructor TdxRichEditParagraphFrameBox.Create(AModelBox: TdxParagraphFrameBox;
  const ADrawingParent, AParent: IdxRichEditLayoutElement);
var
  AParagraph: TdxSimpleParagraph;
begin
  inherited Create(AParent);
  FModelBox := AModelBox;
  FDrawingParent := ADrawingParent;

  AParagraph := ModelBox.GetParagraph;
  FBackColor := AParagraph.BackColor;
  FBorders := TdxRichEditDocumentLayoutHelper.CreateBorders(Self,
    AParagraph.LeftBorder, AParagraph.RightBorder, AParagraph.TopBorder, AParagraph.BottomBorder);
end;

function TdxRichEditParagraphFrameBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.ParagraphFrameBox;
end;

function TdxRichEditParagraphFrameBox.GetBounds: TRect;
begin
  Result := ModelBox.Bounds;
end;

procedure TdxRichEditParagraphFrameBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitParagraphFrameBox(Self);
end;

{ TdxRichEditRangedLayoutElementBase }

destructor TdxRichEditRangedLayoutElementBase.Destroy;
begin
  FreeAndNil(FRange);
  inherited Destroy;
end;

function TdxRichEditRangedLayoutElementBase.GetRange: TdxRichEditFixedRange;
begin
  if FRange = nil then
    FRange := CreateRange;
  Result := FRange;
end;

{ TdxRichEditBox }

constructor TdxRichEditBox.Create(ABox: TdxModelBox; AType: TdxRichEditLayoutType; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FBox := ABox;
  TdxModelBox.AddReference(FBox);
  FType := AType;
end;

destructor TdxRichEditBox.Destroy;
begin
  TdxModelBox.Release(FBox);
  inherited Destroy;
end;

function TdxRichEditBox.GetType: TdxRichEditLayoutType;
begin
  Result := FType;
end;

function TdxRichEditBox.GetBounds: TRect;
begin
  Result := ModelBox.Bounds;
end;

function TdxRichEditBox.CreateRange: TdxRichEditFixedRange;
var
  AContainer: IdxPieceTableContainer;
  AStartPosition, AEndPosition: TdxDocumentModelPosition;
  AStart, AEnd: Integer;
begin
  AContainer := GetParentByIntf(IdxPieceTableContainer) as IdxPieceTableContainer;
  AStartPosition := TdxDocumentModelPosition.FromRunStart(AContainer.PieceTable, FBox.StartPos.RunIndex);
  AEndPosition := TdxDocumentModelPosition.FromRunStart(AContainer.PieceTable, FBox.EndPos.RunIndex);
  AStart := AStartPosition.LogPosition + FBox.StartPos.Offset;
  AEnd := AEndPosition.LogPosition + FBox.EndPos.Offset;
  Result := TdxRichEditFixedRange.Create(AStart, AEnd - AStart + 1);
end;

{ TdxRichEditLayoutPage }

constructor TdxRichEditLayoutPage.Create(APage: TdxPage; const ADocument: IdxRichEditDocument);
begin
  inherited Create(nil);
  FPage := APage;
  FDocument := ADocument;
end;

destructor TdxRichEditLayoutPage.Destroy;
begin
  FreeAndNil(FPageAreas);
  FreeAndNil(FComments);
  FreeAndNil(FBackgroundFloatingObjects);
  FreeAndNil(FFloatingObjects);
  FreeAndNil(FForegroundFloatingObjects);
  FreeAndNil(FMainContentRange);

  FreeAndNil(FHeader);
  FreeAndNil(FFooter);
  FreeAndNil(FFrames);
  inherited Destroy;
end;


function TdxRichEditLayoutPage.GetPageAreas: TdxRichEditLayoutPageAreaCollection;
var
  I: Integer;
begin
  if FPageAreas = nil then
  begin
    FPageAreas := TdxRichEditLayoutPageAreaCollection.Create;
    for I := 0 to FPage.Areas.Count - 1 do
      FPageAreas.Add(TdxRichEditLayoutPageArea.Create(FPage.Areas[I], Self));
  end;
  Result := FPageAreas;
end;

function TdxRichEditLayoutPage.GetHeader: TdxRichEditLayoutHeader;
begin
   if (FPage.Header <> nil) and (FHeader = nil) then
     FHeader := TdxRichEditLayoutHeader.Create(FPage.Header, Self);
   Result := FHeader;
end;

function TdxRichEditLayoutPage.GetFooter: TdxRichEditLayoutFooter;
begin
   if (FPage.Footer <> nil) and (FFooter = nil) then
     FFooter := TdxRichEditLayoutFooter.Create(FPage.Footer, Self);
   Result := FFooter;
end;

function TdxRichEditLayoutPage.GetComments: TdxRichEditLayoutCommentCollection;
begin
  if FComments = nil then
  begin
    FComments := TdxRichEditLayoutCommentCollection.Create;
  end;
  Result := FComments;
end;

function TdxRichEditLayoutPage.GetBackgroundFloatingObjects: TdxRichEditLayoutFloatingObjectCollection;
begin
  if FBackgroundFloatingObjects = nil then
    FBackgroundFloatingObjects := CreateFloatingObjects(FPage.BackgroundFloatingObjects);
  Result := FBackgroundFloatingObjects;
end;

function TdxRichEditLayoutPage.GetFloatingObjects: TdxRichEditLayoutFloatingObjectCollection;
begin
  if FFloatingObjects = nil then
    FFloatingObjects := CreateFloatingObjects(FPage.FloatingObjects);
  Result := FFloatingObjects;
end;

function TdxRichEditLayoutPage.GetForegroundFloatingObjects: TdxRichEditLayoutFloatingObjectCollection;
begin
  if FForegroundFloatingObjects = nil then
    FForegroundFloatingObjects := CreateFloatingObjects(FPage.ForegroundFloatingObjects);
  Result := FForegroundFloatingObjects;
end;

function TdxRichEditLayoutPage.GetType: TdxRichEditLayoutType;
begin
   Result := TdxRichEditLayoutType.Page;
end;

function TdxRichEditLayoutPage.GetIndex: Integer;
begin
  Result := ModelPage.PageIndex;
end;

function TdxRichEditLayoutPage.GetMainContentRange: TdxRichEditFixedRange;
var
  AStart, AEnd: Integer;
begin
  if FMainContentRange = nil then
  begin
    AStart := PageAreas.First.Range.Start;
    AEnd := PageAreas.Last.Range.Start + PageAreas.Last.Range.Length;
    FMainContentRange := TdxRichEditFixedRange.Create(AStart, AEnd - AStart);
  end;
  Result := FMainContentRange;
end;

function TdxRichEditLayoutPage.CreateParagraphFrames(AModelParagraphFrames: TdxParagraphFrameBoxList): TdxRichEditLayoutFrameCollection;
var
  I: Integer;
begin
  Result := TdxRichEditLayoutFrameCollection.Create;
  if (AModelParagraphFrames = nil) or (AModelParagraphFrames.Count = 0) then
    Exit;
  Result.Capacity := AModelParagraphFrames.Count;
  for I := 0 to AModelParagraphFrames.Count - 1 do
    Result.Add(TdxRichEditLayoutFrame.Create(AModelParagraphFrames[I], Self));
end;

function TdxRichEditLayoutPage.GetFrames: TdxRichEditLayoutFrameCollection;
begin
  if FFrames = nil then
    FFrames := CreateParagraphFrames(FPage.InnerParagraphFrames);
  Result := FFrames;
end;

function TdxRichEditLayoutPage.GetBounds: TRect;
begin
  Result := ModelPage.Bounds;
end;

function TdxRichEditLayoutPage.CreateFloatingObjects(AModelFloatingObjects: TdxFloatingObjectBoxList): TdxRichEditLayoutFloatingObjectCollection;
var
  I: Integer;
  ACurrentFloatingObject: TdxFloatingObjectBox;
begin
  Result := TdxRichEditLayoutFloatingObjectCollection.Create;
  for I := 0 to AModelFloatingObjects.Count - 1 do
  begin
    ACurrentFloatingObject := AModelFloatingObjects[I];
    if ACurrentFloatingObject.DocumentLayout <> nil then
      Result.Add(TdxRichEditLayoutTextBox.Create(ACurrentFloatingObject, Self))
    else
      Result.Add(TdxRichEditLayoutFloatingPicture.Create(ACurrentFloatingObject, Self));
  end;
end;

procedure TdxRichEditLayoutPage.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitPage(Self);
end;

{ TdxRichEditLayoutColumn }

constructor TdxRichEditLayoutColumn.Create(AColumn: TdxColumn; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FColumn := AColumn;
end;

destructor TdxRichEditLayoutColumn.Destroy;
begin
  FreeAndNil(FParagraphFrames);
  FreeAndNil(FLineNumbers);
  FreeAndNil(FRows);
  FreeAndNil(FTables);
  inherited Destroy;
end;

function TdxRichEditLayoutColumn.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.Column;
end;

function TdxRichEditLayoutColumn.GetModelRows: TdxRowCollection;
begin
  Result := FColumn.Rows;
end;

function TdxRichEditLayoutColumn.GetModelTables: TdxTableViewInfoCollection;
begin
  Result := FColumn.Tables;
end;

function TdxRichEditLayoutColumn.GetTables: TdxRichEditLayoutTableCollection;
begin
  if FTables = nil then
    FTables := TdxRichEditDocumentLayoutHelper.CreateLayoutTableCollection(ModelTables, Self);
  Result := FTables;
end;

function TdxRichEditLayoutColumn.GetRows: TdxRichEditLayoutRowCollection;
begin
  if FRows = nil then
    FRows := TdxRichEditDocumentLayoutHelper.CreateLayoutRowCollection(ModelRows, Self, False);
  Result := FRows;
end;

function TdxRichEditLayoutColumn.GetLineNumbers: TdxRichEditLineNumberBoxCollection;
var
  I: Integer;
  ABox: TdxModelLineNumberBox;
  APageArea: TdxRichEditLayoutPageArea;
  ABoxes: TdxModelLineNumberBoxCollection;
  J: Integer;
begin
  if FLineNumbers = nil then
  begin
    FLineNumbers := TdxRichEditLineNumberBoxCollection.Create;
    APageArea := Safe<TdxRichEditLayoutPageArea>.Cast(TObject(Parent));
    if APageArea <> nil then
    begin
      for I := 0 to ModelRows.Count - 1 do
      begin
        ABox := nil;
        ABoxes := APageArea.ModelLineNumberBoxes;
        for J := 0 to ABoxes.Count - 1 do
          if ModelRows[I] = ABoxes[J].Row then
          begin
            ABox := ABoxes[J];
            Break;
          end;
        if ABox <> nil then
          FLineNumbers.Add(TdxRichEditLineNumberBox.Create(ABox, Self));
      end;
    end;
  end;
  Result := FLineNumbers;
end;

function TdxRichEditLayoutColumn.GetBounds: TRect;
begin
  Result := ModelColumn.Bounds;
end;

function TdxRichEditLayoutColumn.GetParagraphFrames: TdxRichEditParagraphFrameBoxCollection;
begin
  if FParagraphFrames = nil then
    FParagraphFrames := TdxRichEditDocumentLayoutHelper.CreateParagraphFrameBoxCollection(ModelColumn, Self);
  Result := FParagraphFrames;
end;

procedure TdxRichEditLayoutColumn.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitColumn(Self);
end;

function TdxRichEditLayoutColumn.CreateRange: TdxRichEditFixedRange;
begin
  Result := TdxRichEditDocumentLayoutHelper.GetRangeForTablesAndRowsContainer(Rows, Tables);
end;

{ TdxRichEditFixedRange }

constructor TdxRichEditFixedRange.Create(AStart: Integer; ALength: Integer);
begin
  inherited Create;
  Assert(AStart >= 0);
  Assert(ALength >= 0);
  FStart := AStart;
  FLength := ALength;
  FEnd := AStart + ALength - 1;
end;

function TdxRichEditFixedRange.Contains(ARange: TdxRichEditFixedRange): Boolean;
begin
  Result := (Start <= ARange.Start) and (&End >= ARange.&End);
end;

function TdxRichEditFixedRange.Contains(APosition: Integer): Boolean;
begin
  Result := (Start <= APosition) and (&End >= APosition);
end;

function TdxRichEditFixedRange.Intersect(ARange: TdxRichEditFixedRange): Boolean;
begin
  Result := (ARange.Start <= Start) and (ARange.&End >= Start) or
    (ARange.Start <= &End) and (ARange.&End >= &End) or Contains(ARange);
end;

function TdxRichEditFixedRange.Equals(AObj: TObject): Boolean;
var
  ARange: TdxRichEditFixedRange;
begin
  ARange := Safe<TdxRichEditFixedRange>.Cast(AObj);
  if ARange = nil then
    Exit(False);
  Result := (Start = ARange.Start) and (Length = ARange.Length);
end;

function TdxRichEditFixedRange.ToString: string;
begin
  Result := Format('%d, %d', [Start, Length]);
end;

{ TdxRichEditPlainTextBox }

function TdxRichEditPlainTextBox.GetText: string;
var
  AParent: IdxPieceTableContainer;
begin
  if FText = '' then
  begin
    AParent := GetParentByIntf(IdxPieceTableContainer) as IdxPieceTableContainer;
    FText := ModelBox.GetText(AParent.PieceTable);
  end;
  Result := FText;
end;

procedure TdxRichEditPlainTextBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  case &Type of
    TdxRichEditLayoutType.PlainTextBox:
      AVisitor.VisitPlainTextBox(Self);
    TdxRichEditLayoutType.SpecialTextBox:
      AVisitor.VisitSpecialTextBox(Self);
    TdxRichEditLayoutType.SpaceBox:
      AVisitor.VisitSpaceBox(Self);
    TdxRichEditLayoutType.ParagraphMarkBox:
      AVisitor.VisitParagraphMarkBox(Self);
    TdxRichEditLayoutType.SectionBreakBox:
      AVisitor.VisitSectionBreakBox(Self);
    TdxRichEditLayoutType.LineBreakBox:
      AVisitor.VisitLineBreakBox(Self);
    TdxRichEditLayoutType.PageBreakBox:
      AVisitor.VisitPageBreakBox(Self);
    TdxRichEditLayoutType.ColumnBreakBox:
      AVisitor.VisitColumnBreakBox(Self);
    TdxRichEditLayoutType.HyphenBox:
      AVisitor.VisitHyphenBox(Self);
    TdxRichEditLayoutType.TabSpaceBox:
      AVisitor.VisitTabSpaceBox(Self);
    TdxRichEditLayoutType.PageNumberBox:
      AVisitor.VisitPageNumberBox(Self);
  end;
end;

procedure TdxRichEditPlainTextBox.SetText(const AText: string);
begin
  FText := AText;
end;

{ TdxRichEditInlinePictureBox }

constructor TdxRichEditInlinePictureBox.Create(ABox: TdxModelInlinePictureBox; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(ABox, TdxRichEditLayoutType.InlinePictureBox, AParent);
end;

function TdxRichEditInlinePictureBox.GetImage: TdxOfficeImage;
var
  AParent: IdxPieceTableContainer;
begin
  if FImage = nil then
  begin
    AParent := GetParentByIntf(IdxPieceTableContainer) as IdxPieceTableContainer;
    FImage := (TdxModelInlinePictureBox(ModelBox)).GetImage(AParent.PieceTable, True);
  end;
  Result := FImage;
end;

procedure TdxRichEditInlinePictureBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitInlinePictureBox(Self);
end;

{ TdxRichEditFloatingObjectAnchorBox }

constructor TdxRichEditFloatingObjectAnchorBox.Create(AAnchorBox: TdxModelFloatingObjectAnchorBox; const ABounds: TRect;
  const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AAnchorBox, TdxRichEditLayoutType.FloatingObjectAnchorBox, AParent);
  FBounds := ABounds;
end;

function TdxRichEditFloatingObjectAnchorBox.InternalGetFloatingObjectBox: TdxRichEditLayoutFloatingObject;
begin
  if FFloatingObjectBox = nil then
    FFloatingObjectBox := GetFloatingObjectBox;
  Result := FFloatingObjectBox;
end;

function TdxRichEditFloatingObjectAnchorBox.GetBounds: TRect;
begin
  Result := FBounds;
end;

function TdxRichEditFloatingObjectAnchorBox.GetFloatingObjectBox: TdxRichEditLayoutFloatingObject;
var
  APage: TdxRichEditLayoutPage;
  APieceTableContainer: IdxPieceTableContainer;
  AAnchorRun: TdxFloatingObjectAnchorRun;
begin
  APage := GetParentByType<TdxRichEditLayoutPage>;
  APieceTableContainer := GetParentByIntf(IdxPieceTableContainer) as IdxPieceTableContainer;
  AAnchorRun := TdxFloatingObjectAnchorRun(ModelBox.GetRun(APieceTableContainer.PieceTable));

  if AAnchorRun.FloatingObjectProperties.TextWrapType <> TdxFloatingObjectTextWrapType.None then
    Exit(GetFloatingObjectBoxCore(APage.FloatingObjects, AAnchorRun));
  if AAnchorRun.FloatingObjectProperties.IsBehindDoc then
    Exit(GetFloatingObjectBoxCore(APage.BackgroundFloatingObjects, AAnchorRun))
  else
    Exit(GetFloatingObjectBoxCore(APage.ForegroundFloatingObjects, AAnchorRun));
end;

function TdxRichEditFloatingObjectAnchorBox.GetFloatingObjectBoxCore(
  ACollection: TdxRichEditLayoutFloatingObjectCollection; AAnchorRun: TdxFloatingObjectAnchorRun): TdxRichEditLayoutFloatingObject;
var
  I: Integer;
begin
  for I := 0 to ACollection.Count - 1 do
    if ACollection[I].AnchorRun = AAnchorRun then
      Exit(ACollection[I]);
  Result := nil;
end;

procedure TdxRichEditFloatingObjectAnchorBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitFloatingObjectAnchorBox(Self);
end;

{ TdxRichEditNumberingListMarkBox }

constructor TdxRichEditNumberingListMarkBox.Create(ABox: TdxNumberingListBox; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FListIndex := -1;
  FListLevel := -1;
  FBox := ABox;
end;

function TdxRichEditNumberingListMarkBox.GetListIndex: Integer;
var
  AParagraph: TdxParagraph;
begin
  if FListIndex < 0 then
  begin
    AParagraph := GetParagraph;
    FListIndex := AParagraph.NumberingListIndex;
  end;
  Result := FListIndex;
end;

function TdxRichEditNumberingListMarkBox.GetListLevel: Integer;
var
  AParagraph: TdxParagraph;
begin
  if FListLevel < 0 then
  begin
    AParagraph := GetParagraph;
    FListLevel := AParagraph.GetListLevelIndex;
  end;
  Result := FListLevel;
end;

function TdxRichEditNumberingListMarkBox.GetText: string;
begin
  Result := FBox.NumberingListText;
end;

function TdxRichEditNumberingListMarkBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.NumberingListMarkBox;
end;

function TdxRichEditNumberingListMarkBox.GetBounds: TRect;
begin
  Result := ModelBox.Bounds;
end;

procedure TdxRichEditNumberingListMarkBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitNumberingListMarkBox(Self);
end;

function TdxRichEditNumberingListMarkBox.GetParagraph: TdxParagraph;
var
  ARun: TdxRunBase;
  AParent: IdxPieceTableContainer;
begin
  AParent := GetParentByIntf(IdxPieceTableContainer) as IdxPieceTableContainer;
  ARun := FBox.GetRun(AParent.PieceTable);
  Result := TdxParagraph(ARun.Paragraph);
end;

{ TdxRichEditNumberingListWithSeparatorBox }

constructor TdxRichEditNumberingListWithSeparatorBox.Create(ABox: TdxNumberingListBoxWithSeparator; const AParent: IdxRichEditLayoutElement);
var
  AType: TdxRichEditLayoutType;
  ASeparatorBounds: TRect;
  AParagraph: TdxParagraph;
begin
  inherited Create(ABox, AParent);
  if ABox.SeparatorBox is TdxTabSpaceBox then
    AType := TdxRichEditLayoutType.TabSpaceBox
  else
    AType := TdxRichEditLayoutType.SpaceBox;
  FSeparator := TdxRichEditPlainTextBox.Create(ABox.SeparatorBox, AType, Self);
  ASeparatorBounds := TRect.CreateSize(FSeparator.Bounds.X, ABox.Bounds.Y, FSeparator.Bounds.Width, ABox.Bounds.Height);
  FSeparator.SetBounds(ASeparatorBounds);
  AParagraph := GetParagraph;
  FSeparator.SetText(AParagraph.GetListLevelSeparator);
end;

destructor TdxRichEditNumberingListWithSeparatorBox.Destroy;
begin
  FreeAndNil(FSeparator);
  inherited Destroy;
end;

function TdxRichEditNumberingListWithSeparatorBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.NumberingListWithSeparatorBox;
end;

procedure TdxRichEditNumberingListWithSeparatorBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitNumberingListWithSeparatorBox(Self);
end;

{ TdxRichEditRowExtendedBoxes }

constructor TdxRichEditRowExtendedBoxes.Create(ABoxes: TdxModelRowExtendedBoxes; AParent: TdxRichEditLayoutRow);
begin
  inherited Create;
  FBoxes := ABoxes;
  FParent := AParent;
end;

destructor TdxRichEditRowExtendedBoxes.Destroy;
begin
  FreeAndNil(FUnderlines);
  FreeAndNil(FStrikeouts);
  FreeAndNil(FErrors);
  FreeAndNil(FHighlightAreas);
  FreeAndNil(FFieldHighlightAreas);
  FreeAndNil(FRangePermissionHighlightAreas);
  FreeAndNil(FCommentHighlightAreas);
  FreeAndNil(FBookmarkBoxes);
  FreeAndNil(FRangePermissionBoxes);
  FreeAndNil(FCommentBoxes);
  FreeAndNil(FHiddenTextUnderlineBoxes);
  inherited Destroy;
end;

function TdxRichEditRowExtendedBoxes.GetUnderlines: TdxRichEditUnderlineBoxCollection;
var
  I: Integer;
begin
  if FUnderlines = nil then
  begin
    FUnderlines := TdxRichEditUnderlineBoxCollection.Create;
    for I := 0 to FBoxes.Underlines.Count - 1 do
      FUnderlines.Add(TdxRichEditUnderlineBox.Create(FBoxes.Underlines[I], FParent));
  end;
  Result := FUnderlines;
end;

function TdxRichEditRowExtendedBoxes.GetErrors: TdxRichEditErrorBoxCollection;
var
  I: Integer;
begin
  if FErrors = nil then
  begin
    FErrors := TdxRichEditErrorBoxCollection.Create;
    for I := 0 to FBoxes.Errors.Count - 1 do
      FErrors.Add(TdxRichEditErrorBox.Create(FBoxes.Errors[I], FParent));
  end;
  Result := FErrors;
end;

function TdxRichEditRowExtendedBoxes.GetStrikeouts: TdxRichEditStrikeoutBoxCollection;
var
  I: Integer;
begin
  if FStrikeouts = nil then
  begin
    FStrikeouts := TdxRichEditStrikeoutBoxCollection.Create;
    for I := 0 to FBoxes.Strikeouts.Count - 1 do
      FStrikeouts.Add(TdxRichEditStrikeoutBox.Create(FBoxes.Strikeouts[i], FParent));
  end;
  Result := FStrikeouts;
end;

function TdxRichEditRowExtendedBoxes.GetHighlightAreas: TdxRichEditHighlightAreaCollection;
var
  I: Integer;
begin
  if FHighlightAreas = nil then
  begin
    FHighlightAreas := TdxRichEditHighlightAreaCollection.Create;
    for I := 0 to FBoxes.HighlightAreas.Count - 1 do
      FHighlightAreas.Add(TdxRichEditHighlightAreaBox.Create(FBoxes.HighlightAreas[I], FParent));
  end;
  Result := FHighlightAreas;
end;

function TdxRichEditRowExtendedBoxes.GetFieldHighlightAreas: TdxRichEditFieldHighlightAreaCollection;
var
  I: Integer;
begin
  if FFieldHighlightAreas = nil then
  begin
    FFieldHighlightAreas := TdxRichEditFieldHighlightAreaCollection.Create;
    for I := 0 to FBoxes.FieldHighlightAreas.Count - 1 do
      FFieldHighlightAreas.Add(TdxRichEditFieldHighlightAreaBox.Create(FBoxes.FieldHighlightAreas[I], FParent));
  end;
  Result := FFieldHighlightAreas;
end;

function TdxRichEditRowExtendedBoxes.GetRangePermissionHighlightAreas: TdxRichEditRangePermissionHighlightAreaCollection;
var
  I: Integer;
begin
 if FRangePermissionHighlightAreas = nil then
 begin
  FRangePermissionHighlightAreas := TdxRichEditRangePermissionHighlightAreaCollection.Create;
  for I := 0 to FBoxes.RangePermissionHighlightAreas.Count - 1 do
    FRangePermissionHighlightAreas.Add(
      TdxRichEditRangePermissionHighlightAreaBox.Create(FBoxes.RangePermissionHighlightAreas[I], FParent));
  end;
  Result := FRangePermissionHighlightAreas;
end;

function TdxRichEditRowExtendedBoxes.GetCommentHighlightAreas: TdxRichEditCommentHighlightAreaCollection;
var
  I: Integer;
begin
  if FCommentHighlightAreas = nil then
  begin
    FCommentHighlightAreas := TdxRichEditCommentHighlightAreaCollection.Create;
    for I := 0 to FBoxes.CommentHighlightAreas.Count - 1 do
      FCommentHighlightAreas.Add(TdxRichEditCommentHighlightAreaBox.Create(FBoxes.CommentHighlightAreas[I], FParent));
   end;
   Result := FCommentHighlightAreas;
end;

function TdxRichEditRowExtendedBoxes.GetBookmarkBoxes: TdxRichEditBookmarkBoxCollection;
var
  I: Integer;
begin
  if FBookmarkBoxes = nil then
  begin
    FBookmarkBoxes := TdxRichEditBookmarkBoxCollection.Create;
    for I := 0 to FBoxes.BookmarkBoxes.Count - 1 do
    begin
      if FBoxes.BookmarkBoxes[I] is TdxBookmarkStartBox then
        FBookmarkBoxes.Add(TdxRichEditBookmarkBox.Create(FBoxes.BookmarkBoxes[I],
          TdxRichEditLayoutType.BookmarkStartBox, FParent))
      else
        FBookmarkBoxes.Add(TdxRichEditBookmarkBox.Create(FBoxes.BookmarkBoxes[I],
          TdxRichEditLayoutType.BookmarkEndBox, FParent));
    end;
  end;
  Result := FBookmarkBoxes;
end;

function TdxRichEditRowExtendedBoxes.GetRangePermissionBoxes: TdxRichEditRangePermissionBoxCollection;
var
  I: Integer;
begin
  if FRangePermissionBoxes = nil then
  begin
    FRangePermissionBoxes := TdxRichEditRangePermissionBoxCollection.Create;
    for I := 0 to FBoxes.RangePermissionBoxes.Count - 1 do
    begin
      if FBoxes.RangePermissionBoxes[I] is TdxBookmarkStartBox then
        FRangePermissionBoxes.Add(TdxRichEditRangePermissionBox.Create(FBoxes.RangePermissionBoxes[I],
          TdxRichEditLayoutType.RangePermissionStartBox, FParent))
      else
        FRangePermissionBoxes.Add(TdxRichEditRangePermissionBox.Create(FBoxes.RangePermissionBoxes[I],
          TdxRichEditLayoutType.RangePermissionEndBox, FParent));
    end;
  end;
  Result := FRangePermissionBoxes;
end;

function TdxRichEditRowExtendedBoxes.GetCommentBoxes: TdxRichEditCommentBoxCollection;
begin
  if FCommentBoxes = nil then
  begin
    FCommentBoxes := TdxRichEditCommentBoxCollection.Create;
  end;
  Result := FCommentBoxes;
end;

function TdxRichEditRowExtendedBoxes.GetHiddenTextUnderlineBoxes: TdxRichEditHiddenTextUnderlineBoxCollection;
var
  I: Integer;
begin
  if FHiddenTextUnderlineBoxes = nil then
  begin
    FHiddenTextUnderlineBoxes := TdxRichEditHiddenTextUnderlineBoxCollection.Create;
    for I := 0 to FBoxes.HiddenTextBoxes.Count - 1 do
      FHiddenTextUnderlineBoxes.Add(TdxRichEditHiddenTextUnderlineBox.Create(FBoxes.HiddenTextBoxes[I], FParent));
  end;
  Result := FHiddenTextUnderlineBoxes;
end;

{ TdxRichEditLayoutRow }

constructor TdxRichEditLayoutRow.Create(ARow: TdxRow; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FRow := ARow;
  TdxRow.AddReference(FRow);
  InitializeNumberingListBox;
  FExtendedBoxes := TdxRichEditRowExtendedBoxes.Create(ARow.ExtendedBoxes, Self);
end;

destructor TdxRichEditLayoutRow.Destroy;
begin
  FreeAndNil(FBoxes);
  FreeAndNil(FExtendedBoxes);
  TdxRow.Release(FRow);
  FreeAndNil(FNumberingListBox);
  inherited Destroy;
end;

function TdxRichEditLayoutRow.GetBoxes: TdxRichEditBoxCollection;
var
  AExporter: TdxRichEditLayoutRowExporter;
begin
  if FBoxes = nil then
  begin
    AExporter := TdxRichEditLayoutRowExporter.Create(Self);
    try
      AExporter.ExportRow(FRow);
      FBoxes := AExporter.Boxes;
    finally
      AExporter.Free;
    end;
  end;
  Result := FBoxes
end;

function TdxRichEditLayoutRow.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.Row;
end;

function TdxRichEditLayoutRow.GetBounds: TRect;
begin
  Result := ModelRow.Bounds;
end;

procedure TdxRichEditLayoutRow.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitRow(Self);
end;

procedure TdxRichEditLayoutRow.InitializeNumberingListBox;
var
  ABoxWithSeparator: TdxNumberingListBoxWithSeparator;
begin
  if FRow.NumberingListBox <> nil then
  begin
    ABoxWithSeparator := Safe<TdxNumberingListBoxWithSeparator>.Cast(FRow.NumberingListBox);
    if ABoxWithSeparator <> nil then
      FNumberingListBox := TdxRichEditNumberingListWithSeparatorBox.Create(ABoxWithSeparator, Self)
    else
      FNumberingListBox := TdxRichEditNumberingListMarkBox.Create(FRow.NumberingListBox, Self);
  end;
end;

function TdxRichEditLayoutRow.CreateRange: TdxRichEditFixedRange;
var
  AFirstBoxRangeStart, ALastBoxRangeEnd: Integer;
  ALastBoxRange: TdxRichEditFixedRange;
begin
  AFirstBoxRangeStart := Boxes[0].Range.Start;
  ALastBoxRange := Boxes[Boxes.Count - 1].Range;
  ALastBoxRangeEnd := ALastBoxRange.Start + ALastBoxRange.Length;
  Result := TdxRichEditFixedRange.Create(AFirstBoxRangeStart, ALastBoxRangeEnd - AFirstBoxRangeStart);
end;

{ TdxRichEditLayoutTableCell }

constructor TdxRichEditLayoutTableCell.Create(ATableCell: TdxTableCellViewInfo; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FTableCell := ATableCell;
  FBorders := TdxRichEditDocumentLayoutHelper.CreateBorders(Self, ATableCell.LeftBorder.Info, ATableCell.RightBorder.Info,
    ATableCell.TopBorder.Info, ATableCell.BottomBorder.Info);
end;

destructor TdxRichEditLayoutTableCell.Destroy;
begin
  FreeAndNil(FRows);
  FreeAndNil(FNestedTables);
  inherited Destroy;
end;

function TdxRichEditLayoutTableCell.GetRows: TdxRichEditLayoutRowCollection;
var
  ARowCollection: TdxRowCollection;
begin
  if FRows = nil then
  begin
    ARowCollection := FTableCell.GetRows(FTableCell.TableViewInfo.Column);
    try
      FRows := TdxRichEditDocumentLayoutHelper.CreateLayoutRowCollection(ARowCollection, Self, True);
    finally
      ARowCollection.Free;
    end;
  end;
  Result := FRows;
end;

function TdxRichEditLayoutTableCell.GetNestedTables: TdxRichEditLayoutTableCollection;
var
  I: Integer;
begin
  if FNestedTables = nil then
  begin
    FNestedTables := TdxRichEditLayoutTableCollection.Create;
    for I := 0 to FTableCell.InnerTables.Count - 1 do
      FNestedTables.Add(TdxRichEditLayoutTable.Create(FTableCell.InnerTables[i], Self));
  end;
  Result := FNestedTables;
end;

function TdxRichEditLayoutTableCell.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.TableCell;
end;

function TdxRichEditLayoutTableCell.GetBounds: TRect;
begin
  Result := ModelTableCell.GetBounds;
end;

procedure TdxRichEditLayoutTableCell.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitTableCell(Self);
end;

function TdxRichEditLayoutTableCell.CreateRange: TdxRichEditFixedRange;
var
  AContainer: IdxPieceTableContainer;
  APieceTable: TdxPieceTable;
  AFistParagraph, ALastParagraph: TdxParagraph;
  AStartPosition, AEndPosition: TdxDocumentModelPosition;
  AStart, AEnd: Integer;
begin
  AContainer := GetParentByIntf(IdxPieceTableContainer) as IdxPieceTableContainer;
  APieceTable := AContainer.PieceTable;

  AFistParagraph := APieceTable.Paragraphs[FTableCell.Cell.StartParagraphIndex];
  ALastParagraph := APieceTable.Paragraphs[FTableCell.Cell.EndParagraphIndex];

  AStartPosition := TdxDocumentModelPosition.FromRunStart(AContainer.PieceTable, AFistParagraph.FirstRunIndex);
  AEndPosition := TdxDocumentModelPosition.FromRunStart(AContainer.PieceTable, ALastParagraph.LastRunIndex);

  AStart := AStartPosition.LogPosition;
  AEnd := AEndPosition.LogPosition;

  Result := TdxRichEditFixedRange.Create(AStart, AEnd - AStart + 1);
end;

{ TdxRichEditLayoutTableRow }

constructor TdxRichEditLayoutTableRow.Create(ATableRow: TdxTableRowViewInfoBase; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FModelTableRow := ATableRow;
end;

destructor TdxRichEditLayoutTableRow.Destroy;
begin
  FreeAndNil(FTableCells);
  inherited Destroy;
end;

function TdxRichEditLayoutTableRow.GetTableCells: TdxRichEditLayoutTableCellCollection;
var
  I: Integer;
  ACell: TdxTableCellViewInfo;
begin
  if FTableCells = nil then
  begin
    FTableCells := TdxRichEditLayoutTableCellCollection.Create;
    for I := 0 to FModelTableRow.Cells.Count - 1 do
    begin
      ACell := FModelTableRow.Cells[I];
      if FModelTableRow = ACell.GetTableRow then
        FTableCells.Add(TdxRichEditLayoutTableCell.Create(ACell, Self));
    end;
  end;
  Result := FTableCells;
end;

function TdxRichEditLayoutTableRow.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.TableRow;
end;

function TdxRichEditLayoutTableRow.GetBounds: TRect;
begin
  Result := ModelTableRow.GetBounds;
end;

procedure TdxRichEditLayoutTableRow.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitTableRow(Self);
end;

function TdxRichEditLayoutTableRow.CreateRange: TdxRichEditFixedRange;
var
  AFirstTableCellRangeStart, ALastTableCellRangeEnd: Integer;
  ALastTableCellRange: TdxRichEditFixedRange;
begin
  AFirstTableCellRangeStart := TableCells[0].Range.Start;
  ALastTableCellRange := TableCells.Last.Range;
  ALastTableCellRangeEnd := ALastTableCellRange.Start + ALastTableCellRange.Length;
  Result := TdxRichEditFixedRange.Create(AFirstTableCellRangeStart, ALastTableCellRangeEnd - AFirstTableCellRangeStart);
end;

{ TdxRichEditLayoutTable }

constructor TdxRichEditLayoutTable.Create(ATable: TdxTableViewInfo; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FTable := ATable;
  FBorders := TdxRichEditDocumentLayoutHelper.CreateBorders(Self,
    ATable.GetActualLeftBorder.Info, ATable.GetActualRightBorder.Info, ATable.GetActualTopBorder.Info, ATable.GetActualBottomBorder.Info);
end;

destructor TdxRichEditLayoutTable.Destroy;
begin
  FreeAndNil(FTableRows);
  inherited Destroy;
end;

function TdxRichEditLayoutTable.GetTableRows: TdxRichEditLayoutTableRowCollection;
var
  I: Integer;
begin
  if FTableRows = nil then
  begin
    FTableRows := TdxRichEditLayoutTableRowCollection.Create;
    for I := 0 to FTable.RowCount - 1 do
      FTableRows.Add(TdxRichEditLayoutTableRow.Create(FTable.Rows[I], Self));
  end;
  Result := FTableRows;
end;

function TdxRichEditLayoutTable.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.Table;
end;

function TdxRichEditLayoutTable.GetBounds: TRect;
var
  AFirstCellBounds, ALastCellBounds: TRect;
  AWidth, AHeight: Integer;
begin
  AFirstCellBounds := FTable.Cells.First.GetBounds;
  ALastCellBounds := FTable.Cells.Last.GetBounds;
  AWidth := ALastCellBounds.X + ALastCellBounds.Width - AFirstCellBounds.X;
  AHeight := ALastCellBounds.Y + ALastCellBounds.Height - AFirstCellBounds.Y;
  Result := TRect.CreateSize(AFirstCellBounds.X, AFirstCellBounds.Y, AWidth, AHeight);
end;

procedure TdxRichEditLayoutTable.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitTable(Self);
end;

function TdxRichEditLayoutTable.CreateRange: TdxRichEditFixedRange;
var
  AFirstTableRowRangeStart, ALastTableRowRangeEnd: Integer;
  ALastTableRowRange: TdxRichEditFixedRange;
begin
  AFirstTableRowRangeStart := TableRows[0].Range.Start;
  ALastTableRowRange := TableRows.Last.Range;
  ALastTableRowRangeEnd := ALastTableRowRange.Start + ALastTableRowRange.Length;
  Result := TdxRichEditFixedRange.Create(AFirstTableRowRangeStart, ALastTableRowRangeEnd - AFirstTableRowRangeStart);
end;

{ TdxRichEditLayoutFloatingObject }

constructor TdxRichEditLayoutFloatingObject.Create(AFloatingObject: TdxFloatingObjectBox; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  UnitsPerDegree := 60000;
  FFloatingObject := AFloatingObject;
end;

function TdxRichEditLayoutFloatingObject.GetAnchorBox: TdxRichEditFloatingObjectAnchorBox;
var
  AModelPosition: TdxDocumentModelPosition;
  APosition: Integer;
  APage: TdxRichEditLayoutPage;
begin
  if FAnchorBox = nil then
  begin
    AModelPosition := TdxDocumentModelPosition.FromRunStart(ModelFloatingObject.PieceTable, AnchorRun.GetRunIndex);
    APosition := AModelPosition.LogPosition;
    APage := GetParentByType<TdxRichEditLayoutPage>;
    FAnchorBox := TdxRichEditDocumentLayout.GetElementFromPage(APosition, TdxRichEditFloatingObjectAnchorBox,
      TdxNativeSubDocumentBase(APage.Document).PieceTable, APage) as TdxRichEditFloatingObjectAnchorBox;
  end;
  Result := FAnchorBox;
end;

function TdxRichEditLayoutFloatingObject.GetAnchorRun: TdxFloatingObjectAnchorRun;
begin
  if FAnchorRun = nil then
    FAnchorRun := ModelFloatingObject.GetFloatingObjectRun;
  Result := FAnchorRun;
end;

function TdxRichEditLayoutFloatingObject.GetContentBounds: TRect;
begin
  Result := ModelFloatingObject.ContentBounds;
end;

function TdxRichEditLayoutFloatingObject.GetRotationAngle: Single;
begin
  Result := AnchorRun.Shape.Rotation / UnitsPerDegree;
end;

function TdxRichEditLayoutFloatingObject.GetBounds: TRect;
begin
  Result := ModelFloatingObject.ActualSizeBounds;
end;

function TdxRichEditLayoutFloatingObject.GetDocumentShape: TdxShape;
begin
  Result := NotImplemented;
end;

function TdxRichEditLayoutFloatingObject.GetCoordinates: TArray<TPoint>;
begin
  Result := GetCoordinatesCore(Bounds);
end;

function TdxRichEditLayoutFloatingObject.GetContentCoordinates: TArray<TPoint>;
begin
  Result := GetCoordinatesCore(ContentBounds);
end;

function TdxRichEditLayoutFloatingObject.GetCoordinatesCore(const ABounds: TRect): TArray<TPoint>;
var
  AMatrix: TdxTransformMatrix;
begin
  AMatrix := TdxTransformMatrix.Create;
  try
    AMatrix.Rotate(ModelFloatingObject.PieceTable.DocumentModel.UnitConverter.ModelUnitsToDegreeF(AnchorRun.Shape.Rotation));
    Result := TArray<TPoint>.Create(
      AMatrix.TransformPoint(ABounds.Location),
      AMatrix.TransformPoint(TPoint.Create(ABounds.Right, ABounds.Top)),
      AMatrix.TransformPoint(TPoint.Create(ABounds.Right, ABounds.Bottom)),
      AMatrix.TransformPoint(TPoint.Create(ABounds.Left, ABounds.Bottom))
    );
  finally
    AMatrix.Free;
  end;
end;

{ TdxRichEditLayoutTextBox }

constructor TdxRichEditLayoutTextBox.Create(AFloatingObject: TdxFloatingObjectBox; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AFloatingObject, AParent);
end;

destructor TdxRichEditLayoutTextBox.Destroy;
begin
  FreeAndNil(FParagraphFrames);
  FreeAndNil(FRows);
  FreeAndNil(FTables);
  inherited Destroy;
end;

function TdxRichEditLayoutTextBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.TextBox;
end;

function TdxRichEditLayoutTextBox.GetParagraphFrames: TdxRichEditParagraphFrameBoxCollection;
begin
  if FParagraphFrames = nil then
  begin
    FParagraphFrames := TdxRichEditDocumentLayoutHelper.CreateParagraphFrameBoxCollection(
      ModelFloatingObject.DocumentLayout.Pages[0].Areas[0].Columns[0], Self);
  end;
  Result := FParagraphFrames;
end;


function TdxRichEditLayoutTextBox.GetTables: TdxRichEditLayoutTableCollection;
var
  AModelCollection: TdxTableViewInfoCollection;
begin
  if FTables = nil then
  begin
    AModelCollection := ModelFloatingObject.DocumentLayout.Pages[0].Areas[0].Columns[0].Tables;
    FTables := TdxRichEditDocumentLayoutHelper.CreateLayoutTableCollection(AModelCollection, Self);
  end;
  Result := FTables;
end;

function TdxRichEditLayoutTextBox.GetRows: TdxRichEditLayoutRowCollection;
var
  AModelCollection: TdxRowCollection;
begin
  if FRows = nil then
  begin
    AModelCollection := ModelFloatingObject.DocumentLayout.Pages[0].Areas[0].Columns[0].Rows;
    FRows := TdxRichEditDocumentLayoutHelper.CreateLayoutRowCollection(AModelCollection, Self, False);
  end;
  Result := FRows;
end;

function TdxRichEditLayoutTextBox.GetDocument: IdxRichEditSubDocument;
var
  APage: TdxRichEditLayoutPage;
  AStartPos: TdxDocumentModelPosition;
  ATextBoxRange: IdxRichEditDocumentRange;
begin
  if FDocument = nil then
  begin
    APage := GetParentByType<TdxRichEditLayoutPage>;
    AStartPos := ModelFloatingObject.GetFirstPosition(ModelFloatingObject.PieceTable);
    ATextBoxRange := APage.Document.CreateRange(AStartPos.LogPosition, 1);
    NotImplemented;
  end;
  Result := FDocument;
end;

function TdxRichEditLayoutTextBox.GetRange: TdxRichEditFixedRange;
begin
  if FRange = nil then
    FRange := TdxRichEditDocumentLayoutHelper.GetRangeForTablesAndRowsContainer(Rows, Tables);
  Result := FRange;
end;

function TdxRichEditLayoutTextBox.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(TdxTextBoxFloatingObjectContent(AnchorRun.Content).TextBox.PieceTable);
end;

procedure TdxRichEditLayoutTextBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitTextBox(Self);
end;

{ TdxRichEditLayoutFloatingPicture }

function TdxRichEditLayoutFloatingPicture.GetImage: TdxOfficeImage;
begin
  if FImage = nil then
    FImage := AnchorRun.PictureContent.Image.Image;
  Result := FImage;
end;

function TdxRichEditLayoutFloatingPicture.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.FloatingPicture;
end;

procedure TdxRichEditLayoutFloatingPicture.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitFloatingPicture(Self);
end;

{ TdxRichEditCommentVisitor }

constructor TdxRichEditCommentVisitor.Create(AComment: TdxRichEditLayoutComment);
begin
  FComment := AComment;
end;

procedure TdxRichEditCommentVisitor.OffsetBounds(ABox: TdxRichEditLayoutElementBase; AXOffset: Integer; AYOffset: Integer);
var
  ABoxBounds, ABounds: TRect;
  ALocation: TPoint;
begin
  ABoxBounds := ABox.Bounds;
  ALocation := TPoint.Create(ABoxBounds.X + AXOffset, ABoxBounds.Y + AYOffset);
  ABounds := TRect.CreateSize(ALocation, ABoxBounds.Size);
  ABox.SetBounds(ABounds);
end;

procedure TdxRichEditCommentVisitor.OffsetBounds(ABox: TdxRichEditLayoutElementBase);
begin
  OffsetBounds(ABox, FComment.ContentBounds.X, FComment.ContentBounds.Y);
end;

procedure TdxRichEditCommentVisitor.VisitColumn(AColumn: TdxRichEditLayoutColumn);
begin
  OffsetBounds(AColumn);
  inherited VisitColumn(AColumn);
end;

procedure TdxRichEditCommentVisitor.VisitBookmarkEndBox(ABookmarkEndBox: TdxRichEditBookmarkBox);
begin
  OffsetBounds(ABookmarkEndBox, FComment.ContentBounds.X, 0);
  inherited VisitBookmarkEndBox(ABookmarkEndBox);
end;

procedure TdxRichEditCommentVisitor.VisitBookmarkStartBox(ABookmarkStartBox: TdxRichEditBookmarkBox);
begin
  OffsetBounds(ABookmarkStartBox, FComment.ContentBounds.X, 0);
  inherited VisitBookmarkStartBox(ABookmarkStartBox);
end;

procedure TdxRichEditCommentVisitor.VisitRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox: TdxRichEditRangePermissionHighlightAreaBox);
begin
  OffsetBounds(ARangePermissionHighlightAreaBox);
  inherited VisitRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox);
end;

procedure TdxRichEditCommentVisitor.VisitRangePermissionStartBox(ARangePermissionStartBox: TdxRichEditRangePermissionBox);
begin
  OffsetBounds(ARangePermissionStartBox, FComment.ContentBounds.X, 0);
  inherited VisitRangePermissionStartBox(ARangePermissionStartBox);
end;

procedure TdxRichEditCommentVisitor.VisitRangePermissionEndBox(ARangePermissionEndBox: TdxRichEditRangePermissionBox);
begin
  OffsetBounds(ARangePermissionEndBox, FComment.ContentBounds.X, 0);
  inherited VisitRangePermissionEndBox(ARangePermissionEndBox);
end;

procedure TdxRichEditCommentVisitor.VisitFieldHighlightAreaBox(AFieldHighlightAreaBox: TdxRichEditFieldHighlightAreaBox);
begin
  OffsetBounds(AFieldHighlightAreaBox);
  inherited VisitFieldHighlightAreaBox(AFieldHighlightAreaBox);
end;

procedure TdxRichEditCommentVisitor.VisitHiddenTextUnderlineBox(AHiddenTextUnderlineBox: TdxRichEditHiddenTextUnderlineBox);
begin
  OffsetBounds(AHiddenTextUnderlineBox);
  inherited VisitHiddenTextUnderlineBox(AHiddenTextUnderlineBox);
end;

procedure TdxRichEditCommentVisitor.VisitHighlightAreaBox(AHighlightAreaBox: TdxRichEditHighlightAreaBox);
begin
  OffsetBounds(AHighlightAreaBox);
  inherited VisitHighlightAreaBox(AHighlightAreaBox);
end;

procedure TdxRichEditCommentVisitor.VisitHyphenBox(AHyphen: TdxRichEditPlainTextBox);
begin
  OffsetBounds(AHyphen);
  inherited VisitHyphenBox(AHyphen);
end;

procedure TdxRichEditCommentVisitor.VisitInlinePictureBox(AInlinePictureBox: TdxRichEditInlinePictureBox);
begin
  OffsetBounds(AInlinePictureBox);
  inherited VisitInlinePictureBox(AInlinePictureBox);
end;

procedure TdxRichEditCommentVisitor.VisitLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox);
begin
  OffsetBounds(ALineBreakBox);
  inherited VisitLineBreakBox(ALineBreakBox);
end;

procedure TdxRichEditCommentVisitor.VisitNumberingListMarkBox(ANumberingListMarkBox: TdxRichEditNumberingListMarkBox);
begin
  OffsetBounds(ANumberingListMarkBox);
  inherited VisitNumberingListMarkBox(ANumberingListMarkBox);
end;

procedure TdxRichEditCommentVisitor.VisitNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox: TdxRichEditNumberingListWithSeparatorBox);
begin
  OffsetBounds(ANumberingListWithSeparatorBox);
  inherited VisitNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox);
end;

procedure TdxRichEditCommentVisitor.VisitParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox);
begin
  OffsetBounds(AParagraphMarkBox);
  inherited VisitParagraphMarkBox(AParagraphMarkBox);
end;

procedure TdxRichEditCommentVisitor.VisitPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox);
begin
  OffsetBounds(APlainTextBox);
  inherited VisitPlainTextBox(APlainTextBox);
end;

procedure TdxRichEditCommentVisitor.VisitRow(ARow: TdxRichEditLayoutRow);
begin
  OffsetBounds(ARow);
  inherited VisitRow(ARow);
end;

procedure TdxRichEditCommentVisitor.VisitSpaceBox(ASpaceBox: TdxRichEditPlainTextBox);
begin
  OffsetBounds(ASpaceBox);
  inherited VisitSpaceBox(ASpaceBox);
end;

procedure TdxRichEditCommentVisitor.VisitSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox);
begin
  OffsetBounds(ASpecialTextBox);
  inherited VisitSpecialTextBox(ASpecialTextBox);
end;

procedure TdxRichEditCommentVisitor.VisitStrikeoutBox(AStrikeoutBox: TdxRichEditStrikeoutBox);
begin
  OffsetBounds(AStrikeoutBox);
  inherited VisitStrikeoutBox(AStrikeoutBox);
end;

procedure TdxRichEditCommentVisitor.VisitTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox);
begin
  OffsetBounds(ATabSpaceBox);
  inherited VisitTabSpaceBox(ATabSpaceBox);
end;

procedure TdxRichEditCommentVisitor.VisitUnderlineBox(AUnderlineBox: TdxRichEditUnderlineBox);
begin
  OffsetBounds(AUnderlineBox);
  inherited VisitUnderlineBox(AUnderlineBox);
end;

procedure TdxRichEditCommentVisitor.VisitErrorBox(AErrorBox: TdxRichEditErrorBox);
begin
  OffsetBounds(AErrorBox);
  inherited VisitErrorBox(AErrorBox);
end;

{ TdxRichEditLayoutComment }

constructor TdxRichEditLayoutComment.Create(AComment: TdxCommentViewInfo; const AParent: IdxRichEditLayoutElement);
var
  AVisitor: TdxRichEditCommentVisitor;
begin
  inherited Create(AParent);
  FComment := AComment;
  AVisitor := TdxRichEditCommentVisitor.Create(Self);
  try
    AVisitor.Visit(Self);
  finally
    AVisitor.Free;
  end;
end;


function TdxRichEditLayoutComment.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.Comment;
end;

function TdxRichEditLayoutComment.GetContentBounds: TRect;
begin
  Result := cxNullRect;
  NotImplemented;
end;

function TdxRichEditLayoutComment.GetModelColumns: TdxColumnCollection;
begin
  Result := NotImplemented;
end;

function TdxRichEditLayoutComment.GetBounds: TRect;
begin
  Result := cxNullRect;
  NotImplemented;
end;

function TdxRichEditLayoutComment.GetDocumentComment: TdxComment;
begin
  Result := NotImplemented
end;

function TdxRichEditLayoutComment.GetPieceTable: TdxPieceTable;
begin
  Result := NotImplemented;
end;

procedure TdxRichEditLayoutComment.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitComment(Self);
end;

function TdxRichEditLayoutComment.BeginUpdate: IdxRichEditSubDocument;
begin
  NotImplemented;
end;

procedure TdxRichEditLayoutComment.EndUpdate(ADocument: IdxRichEditSubDocument);
begin
  NotImplemented;
end;

function TdxRichEditLayoutComment.GetComment: TdxComment;
begin
  Result := NotImplemented;
end;

{ TdxRichEditLineNumberBox }

constructor TdxRichEditLineNumberBox.Create(AModelBox: TdxModelLineNumberBox; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FModelBox := AModelBox;
end;

function TdxRichEditLineNumberBox.GetRow: TdxRichEditLayoutRow;
var
  AColumn: TdxRichEditLayoutColumn;
  I: Integer;
begin
  if FRow = nil then
  begin
    AColumn := TdxRichEditLayoutColumn(Parent);
    for I := 0 to AColumn.Rows.Count - 1 do
      if AColumn.Rows[I].ModelRow = FModelBox.Row then
      begin
        FRow := AColumn.Rows[I];
        Break;
      end;
  end;
  Result := FRow;
end;

function TdxRichEditLineNumberBox.GetText: string;
begin
  Result := FModelBox.GetText(nil);
end;

function TdxRichEditLineNumberBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.LineNumberBox;
end;

function TdxRichEditLineNumberBox.GetBounds: TRect;
begin
  Result := ModelBox.Bounds;
end;

procedure TdxRichEditLineNumberBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitLineNumberBox(Self);
end;

{ TdxRichEditUnderlineBox }

constructor TdxRichEditUnderlineBox.Create(AModelBox: TdxModelUnderlineBox; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FModelBox := AModelBox;
  TdxModelUnderlineBox.AddReference(FModelBox);
end;

destructor TdxRichEditUnderlineBox.Destroy;
begin
  TdxModelUnderlineBox.Release(FModelBox);
  inherited Destroy;
end;

function TdxRichEditUnderlineBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.UnderlineBox;
end;

function TdxRichEditUnderlineBox.GetThickness: Integer;
begin
  Result := ModelBox.UnderlineThickness;
end;

function TdxRichEditUnderlineBox.GetBounds: TRect;
begin
  Result := ModelBox.ClipBounds;
end;

function TdxRichEditUnderlineBox.CreateRange: TdxRichEditFixedRange;
var
  ARow: TdxRichEditLayoutRow;
  AStartBox, AEndBox: TdxRichEditBox;
begin
  ARow := TdxRichEditLayoutRow(Parent.AsObject);
  AStartBox := ARow.Boxes[ModelBox.StartAnchorIndex];
  AEndBox := ARow.Boxes[ModelBox.EndAnchorIndex - 1];
  Result := TdxRichEditFixedRange.Create(AStartBox.Range.Start,
    AEndBox.Range.Start + AEndBox.Range.Length - AStartBox.Range.Start);
end;

procedure TdxRichEditUnderlineBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitUnderlineBox(Self);
end;

{ TdxRichEditStrikeoutBox }

function TdxRichEditStrikeoutBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.StrikeoutBox;
end;

procedure TdxRichEditStrikeoutBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitStrikeoutBox(Self);
end;

{ TdxRichEditErrorBox }

function TdxRichEditErrorBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.ErrorBox;
end;

procedure TdxRichEditErrorBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitErrorBox(Self);
end;

{ TdxRichEditHighlightAreaBox }

constructor TdxRichEditHighlightAreaBox.Create(const AModelBox: TdxHighlightArea; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FModelBox := AModelBox;
end;

function TdxRichEditHighlightAreaBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.HighlightAreaBox;
end;

function TdxRichEditHighlightAreaBox.GetBounds: TRect;
begin
  Result := ModelBox.Bounds;
end;

function TdxRichEditHighlightAreaBox.CreateRange: TdxRichEditFixedRange;
begin
  Result := TdxRichEditDocumentLayoutHelper.GetRangeFromRowByCoordinates(TdxRichEditLayoutRow(Parent), Bounds.X,
    Bounds.X + Bounds.Width);
end;

procedure TdxRichEditHighlightAreaBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitHighlightAreaBox(Self);
end;

{ TdxRichEditFieldHighlightAreaBox }

function TdxRichEditFieldHighlightAreaBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.FieldHighlightAreaBox;
end;

procedure TdxRichEditFieldHighlightAreaBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitFieldHighlightAreaBox(Self);
end;

{ TdxRichEditRangePermissionHighlightAreaBox }

function TdxRichEditRangePermissionHighlightAreaBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.RangePermissionHighlightAreaBox;
end;

procedure TdxRichEditRangePermissionHighlightAreaBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitRangePermissionHighlightAreaBox(Self);
end;

{ TdxRichEditCommentHighlightAreaBox }

function TdxRichEditCommentHighlightAreaBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.CommentHighlightAreaBox;
end;

procedure TdxRichEditCommentHighlightAreaBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitCommentHighlightAreaBox(Self);
end;

{ TdxRichEditBookmarkBox }

constructor TdxRichEditBookmarkBox.Create(AModelBox: TdxVisitableDocumentIntervalBox; AType: TdxRichEditLayoutType;
  const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FModelBox := AModelBox;
  FType := AType;
end;

function TdxRichEditBookmarkBox.GetType: TdxRichEditLayoutType;
begin
  Result := FType;
end;

function TdxRichEditBookmarkBox.GetBounds: TRect;
begin
  Result := TRect.CreateSize(TPoint.Create(ModelBox.HorizontalPosition, Parent.Bounds.Top), TSize.Empty);
end;

procedure TdxRichEditBookmarkBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  if &Type = TdxRichEditLayoutType.BookmarkStartBox then
    AVisitor.VisitBookmarkStartBox(Self)
  else
    AVisitor.VisitBookmarkEndBox(Self);
end;

{ TdxRichEditRangePermissionBox }

procedure TdxRichEditRangePermissionBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  if &Type = TdxRichEditLayoutType.RangePermissionStartBox then
    AVisitor.VisitRangePermissionStartBox(Self)
  else
    AVisitor.VisitRangePermissionEndBox(Self);
end;

{ TdxRichEditCommentBox }

procedure TdxRichEditCommentBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  if &Type = TdxRichEditLayoutType.CommentStartBox then
    AVisitor.VisitCommentStartBox(Self)
  else
    AVisitor.VisitCommentEndBox(Self);
end;

{ TdxRichEditHiddenTextUnderlineBox }

constructor TdxRichEditHiddenTextUnderlineBox.Create(AModelBox: TdxModelHiddenTextUnderlineBox;
  const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FModelBox := AModelBox;
end;

function TdxRichEditHiddenTextUnderlineBox.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.HiddenTextUnderlineBox;
end;

function TdxRichEditHiddenTextUnderlineBox.GetBounds: TRect;
var
  ARow: TdxRichEditLayoutRow;
  Y: Integer;
begin
  ARow := TdxRichEditLayoutRow(Parent);
  Y := ARow.Bounds.Top + ARow.ModelRow.BaseLineOffset + ModelBox.BottomOffset;
  Result.Init(ModelBox.Start, Y, ModelBox.&End, Y);
end;

function TdxRichEditHiddenTextUnderlineBox.CreateRange: TdxRichEditFixedRange;
begin
  Result := TdxRichEditDocumentLayoutHelper.GetRangeFromRowByCoordinates(TdxRichEditLayoutRow(Parent), Bounds.X,
    Bounds.X + Bounds.Width);
end;

procedure TdxRichEditHiddenTextUnderlineBox.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitHiddenTextUnderlineBox(Self);
end;

{ TdxRichEditLayoutFrame }

constructor TdxRichEditLayoutFrame.Create(AModelBox: TdxParagraphFrameBox; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FModelBox := AModelBox;
end;

destructor TdxRichEditLayoutFrame.Destroy;
begin
  FreeAndNil(FParagraphFrames);
  FreeAndNil(FTables);
  FreeAndNil(FRows);
  inherited Destroy;
end;

function TdxRichEditLayoutFrame.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.Frame;
end;

function TdxRichEditLayoutFrame.GetTables: TdxRichEditLayoutTableCollection;
var
  AModelCollection: TdxTableViewInfoCollection;
begin
  if FTables = nil then
  begin
    AModelCollection := ModelBox.DocumentLayout.Pages[0].Areas[0].Columns[0].Tables;
    FTables := TdxRichEditDocumentLayoutHelper.CreateLayoutTableCollection(AModelCollection, Self);
  end;
  Result := FTables;
end;

function TdxRichEditLayoutFrame.GetRows: TdxRichEditLayoutRowCollection;
var
  AModelCollection: TdxRowCollection;
begin
  if FRows = nil then
  begin
    AModelCollection := ModelBox.DocumentLayout.Pages[0].Areas[0].Columns[0].Rows;
    FRows := TdxRichEditDocumentLayoutHelper.CreateLayoutRowCollection(AModelCollection, Self, False);
  end;
  Result := FRows;
end;

function TdxRichEditLayoutFrame.GetParagraphFrames: TdxRichEditParagraphFrameBoxCollection;
begin
  if FParagraphFrames = nil then
  begin
    FParagraphFrames := TdxRichEditDocumentLayoutHelper.CreateParagraphFrameBoxCollection(
      ModelBox.DocumentLayout.Pages[0].Areas[0].Columns[0], Self);
  end;
  Result := FParagraphFrames;
end;

function TdxRichEditLayoutFrame.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(ModelBox.PieceTable);
end;

function TdxRichEditLayoutFrame.CreateRange: TdxRichEditFixedRange;
begin
  Result := TdxRichEditDocumentLayoutHelper.GetRangeForTablesAndRowsContainer(Rows, Tables);
end;

function TdxRichEditLayoutFrame.GetBounds: TRect;
begin
  Result := ModelBox.Bounds;
end;

procedure TdxRichEditLayoutFrame.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitFrame(Self);
end;

{ TdxRichEditLayoutRowExporter }

constructor TdxRichEditLayoutRowExporter.Create(ARow: TdxRichEditLayoutRow);
begin
  inherited Create;
  FRow := ARow;
  FBoxes := TdxRichEditBoxCollection.Create;
end;

procedure TdxRichEditLayoutRowExporter.ExportRow(ARow: TdxRow);
var
  I: Integer;
begin
  for I := 0 to ARow.Boxes.Count - 1 do
  begin
    AddAnchorBox(ARow.Boxes[I], ARow.Paragraph.BoxCollection);
    ARow.Boxes[I].ExportTo(Self);
  end;
end;

procedure TdxRichEditLayoutRowExporter.ExportSimpleRow(ARow: TdxSimpleRow);
begin
NotImplemented;
end;

procedure TdxRichEditLayoutRowExporter.AddAnchorBox(ABox: TdxModelBox; ACollection: TdxSimpleParagraphBoxCollection);
var
  ABoxIndex, I: Integer;
  AInverseList: TList<TdxRichEditFloatingObjectAnchorBox>;
  AModelBox: TdxModelFloatingObjectAnchorBox;
begin
  if not ACollection.ContainsFloatingObjectAnchorBox then
    Exit;
  ABoxIndex := GetBoxIndex(ABox, ACollection);
  if ABoxIndex <= 0 then
    Exit;
  AInverseList := TList<TdxRichEditFloatingObjectAnchorBox>.Create;
  try
    for I := ABoxIndex - 1 downto 0 do
    begin
      AModelBox := Safe<TdxModelFloatingObjectAnchorBox>.Cast(ACollection[I]);
      if AModelBox = nil then
        Break;
      AInverseList.Add(TdxRichEditFloatingObjectAnchorBox.Create(AModelBox, TRect.CreateSize(ACollection[ABoxIndex].Bounds.Location, AModelBox.Bounds.Size), FRow));
    end;
    if AInverseList.Count = 0 then
      Exit;
    for I := AInverseList.Count - 1 downto 0 do
      Boxes.Add(AInverseList[I]);
  finally
    AInverseList.Free;
  end;
end;

function TdxRichEditLayoutRowExporter.GetBoxIndex(ABox: TdxModelBox; ACollection: TdxSimpleParagraphBoxCollection): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ACollection.Count - 1 do
    if (ABox = ACollection[I]) or
      (ABox.StartPos = ACollection[I].StartPos) and (ABox.EndPos = ACollection[I].EndPos) then
      Exit(I);
end;

procedure TdxRichEditLayoutRowExporter.ExportTextBox(ABox: TdxTextBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.PlainTextBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportSpecialTextBox(ABox: TdxSpecialTextBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.SpecialTextBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.PageNumberBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportHyphenBox(ABox: TdxHyphenBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.HyphenBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportInlinePictureBox(ABox: TdxModelInlinePictureBox);
begin
  Boxes.Add(TdxRichEditInlinePictureBox.Create(ABox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportSpaceBox(ABox: TdxModelBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.SpaceBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportTabSpaceBox(ABox: TdxTabSpaceBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.TabSpaceBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportNumberingListBox(ABox: TdxNumberingListBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.NumberingListMarkBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportLineBreakBox(ABox: TdxLineBreakBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.LineBreakBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportPageBreakBox(ABox: TdxPageBreakBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.PageBreakBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportColumnBreakBox(ABox: TdxColumnBreakBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.ColumnBreakBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportParagraphMarkBox(ABox: TdxParagraphMarkBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.ParagraphMarkBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportSectionMarkBox(ABox: TdxSectionMarkBox);
begin
  Boxes.Add(TdxRichEditPlainTextBox.Create(ABox, TdxRichEditLayoutType.SectionBreakBox, FRow));
end;

procedure TdxRichEditLayoutRowExporter.ExportSeparatorBox(ABox: TdxSeparatorBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportLineNumberBox(ABox: TdxModelLineNumberBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportPage(APage: TdxPage);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportParagraphFramePage(APage: TdxPage; const APageClipBounds: TdxRectF; AExportContent: Boolean);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportPageArea(APageArea: TdxPageArea);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportColumn(AColumn: TdxColumn);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportFloatingObjectBox(ABox: TdxFloatingObjectBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportParagraphFrameBox(ABox: TdxParagraphFrameBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportUnderlineBox(ARow: TdxSimpleRow; AUnderlineBox: TdxModelUnderlineBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportStrikeoutBox(ARow: TdxSimpleRow; AStrikeoutBox: TdxModelUnderlineBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportErrorBox(AErrorBox: TdxErrorBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportBookmarkStartBox(ABox: TdxVisitableDocumentIntervalBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportBookmarkEndBox(ABox: TdxVisitableDocumentIntervalBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportCommentStartBox(ABox: TdxVisitableDocumentIntervalBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportCommentEndBox(ABox: TdxVisitableDocumentIntervalBox);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X: Integer; Y: Integer);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportTableCell(ACell: TdxTableCellViewInfo);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportTableRow(ARow: TdxTableRowViewInfoBase);
begin
end;

procedure TdxRichEditLayoutRowExporter.ExportCustomMarkBox(ABox: TdxCustomMarkBox);
begin
end;

function TdxRichEditLayoutRowExporter.IsAnchorVisible(const AAnchor: TdxTableCellVerticalAnchor): Boolean;
begin
  raise TdxNotImplementedException.Create;
end;

function TdxRichEditLayoutRowExporter.IsTableRowVisible(ARow: TdxTableRowViewInfoBase): Boolean;
begin
  raise TdxNotImplementedException.Create;
end;


{ TdxRichEditLayoutPageAreaBase }

constructor TdxRichEditLayoutPageAreaBase.Create(const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
end;

destructor TdxRichEditLayoutPageAreaBase.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

function TdxRichEditLayoutPageAreaBase.GetColumns: TdxRichEditLayoutColumnCollection;
var
  I: Integer;
begin
  if FColumns = nil then
  begin
    FColumns := TdxRichEditLayoutColumnCollection.Create;
    for I := 0 to ModelColumns.Count - 1 do
      FColumns.Add(TdxRichEditLayoutColumn.Create(ModelColumns[I], Self));
  end;
  Result := FColumns;
end;

function TdxRichEditLayoutPageAreaBase.CreateRange: TdxRichEditFixedRange;
var
  AStart, AEnd: Integer;
  ALastColumnRange: TdxRichEditFixedRange;
begin
  AStart := Columns.First.Range.Start;
  ALastColumnRange := Columns.Last.Range;
  AEnd := ALastColumnRange.Start + ALastColumnRange.Length;
  Result := TdxRichEditFixedRange.Create(AStart, AEnd - AStart);
end;

{ TdxRichEditLayoutPageArea }

constructor TdxRichEditLayoutPageArea.Create(APageArea: TdxPageArea; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FModelPageArea := APageArea;
  TdxPageArea.AddReference(FModelPageArea);
end;

destructor TdxRichEditLayoutPageArea.Destroy;
begin
  TdxPageArea.Release(FModelPageArea);
  inherited Destroy;
end;

function TdxRichEditLayoutPageArea.GetDocument: IdxRichEditDocument;
var
  AParent: TdxRichEditLayoutPage;
begin
  if FDocument = nil then
  begin
    AParent := GetParentByType<TdxRichEditLayoutPage>;
    FDocument := AParent.Document;
  end;
  Result := FDocument;
end;

function TdxRichEditLayoutPageArea.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.PageArea;
end;

function TdxRichEditLayoutPageArea.GetModelLineNumberBoxes: TdxModelLineNumberBoxCollection;
begin
  Result := ModelPageArea.LineNumbers;
end;

function TdxRichEditLayoutPageArea.GetModelColumns: TdxColumnCollection;
begin
  Result := ModelPageArea.Columns;
end;

function TdxRichEditLayoutPageArea.GetBounds: TRect;
begin
  Result := ModelPageArea.Bounds;
end;

procedure TdxRichEditLayoutPageArea.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitPageArea(Self);
end;

function TdxRichEditLayoutPageArea.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(ModelPageArea.PieceTable);
end;

{ TdxRichEditLayoutHeaderFooterBase }

constructor TdxRichEditLayoutHeaderFooterBase.Create(AModelHeaderFooter: TdxHeaderFooterPageAreaBase;
  const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AParent);
  FModelHeaderFooter := AModelHeaderFooter;
end;

function TdxRichEditLayoutHeaderFooterBase.GetModelColumns: TdxColumnCollection;
begin
  Result := FModelHeaderFooter.Columns;
end;

function TdxRichEditLayoutHeaderFooterBase.GetSection: TdxRichEditNativeSection;
begin
  if FSection = nil then
    FSection := DoGetSection;
  Result := FSection;
end;

function TdxRichEditLayoutHeaderFooterBase.GetBounds: TRect;
begin
  Result := ModelHeaderFooter.Bounds;
end;

function TdxRichEditLayoutHeaderFooterBase.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(FModelHeaderFooter.PieceTable);
end;

function TdxRichEditLayoutHeaderFooterBase.DoGetSection: TdxRichEditNativeSection;
begin
  Result := nil;
end;

{ TdxLayoutHeader }

constructor TdxRichEditLayoutHeader.Create(AHeader: TdxHeaderPageArea; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AHeader, AParent);
end;

function TdxRichEditLayoutHeader.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.Header;
end;

procedure TdxRichEditLayoutHeader.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitHeader(Self);
end;


{ TdxRichEditLayoutFooter }

constructor TdxRichEditLayoutFooter.Create(AFooter: TdxFooterPageArea; const AParent: IdxRichEditLayoutElement);
begin
  inherited Create(AFooter, AParent)
end;

function TdxRichEditLayoutFooter.GetType: TdxRichEditLayoutType;
begin
  Result := TdxRichEditLayoutType.Footer;
end;

procedure TdxRichEditLayoutFooter.Accept(AVisitor: TdxRichEditLayoutVisitor);
begin
  AVisitor.VisitFooter(Self);
end;


{ TdxRichEditObjectLayoutElementCollection<T> }

function TdxRichEditObjectLayoutElementCollection<T>.First: T;
begin
  Result := T(inherited First);
end;

function TdxRichEditObjectLayoutElementCollection<T>.GetItem(Index: Integer): T;
begin
  Result := T(inherited Items[Index]);
end;

function TdxRichEditObjectLayoutElementCollection<T>.Last: T;
begin
  Result := T(inherited Last);
end;

{ TdxRichEditPagePainter }

constructor TdxRichEditPagePainter.Create;
begin
  inherited Create;
  FPainterVisitor := TdxRichEditPagePainterVisitor.Create(Self);
end;

destructor TdxRichEditPagePainter.Destroy;
begin
  FreeAndNil(FPainterVisitor);
  inherited Destroy;
end;

function TdxRichEditPagePainter.ShouldOptimizeBox(ABox: IdxRichEditLayoutElement): Boolean;
begin
  Result := ABox.Bounds.Height < Exporter.MinReadableTextHeight;
end;

function TdxRichEditPagePainter.GetDrawingBounds(const ABounds: TRect): TRect;
begin
  Result := ABounds;
  Result.Offset(Page.Bounds.Location);
end;

procedure TdxRichEditPagePainter.Draw;
begin
  FPainterVisitor.Visit(Page);
end;

procedure TdxRichEditPagePainter.DrawPage(APage: TdxRichEditLayoutPage);
begin
  Exporter.ExportPage(APage.ModelPage,
    procedure
    begin
      FPainterVisitor.DefaultVisitPage(APage);
    end);
end;

procedure TdxRichEditPagePainter.DrawPageArea(APageArea: TdxRichEditLayoutPageArea);
begin
  FPainterVisitor.DefaultVisitPageArea(APageArea);
  Exporter.AfterExportPageArea;
end;

procedure TdxRichEditPagePainter.DrawColumn(AColumn: TdxRichEditLayoutColumn);
var
  AHeaderFooter: TdxRichEditLayoutHeaderFooterBase;
  AOldClipBounds, AClipBounds: TdxRectF;
begin
  AHeaderFooter := Safe<TdxRichEditLayoutHeaderFooterBase>.Cast(AColumn.Parent.AsObject);

  AOldClipBounds.Empty;
  if AHeaderFooter <> nil then
  begin
    AClipBounds := Exporter.CalculateHeaderFooterClipBounds(AHeaderFooter.ModelHeaderFooter.ContentBounds.ToRectF);
    AOldClipBounds := Exporter.BeginHeaderFooterExport(AClipBounds);
  end;

  Exporter.PushBackgroundLayer;
  Exporter.ColumnClipBounds := Exporter.GetClipBounds;
  FPainterVisitor.DefaultVisitColumn(AColumn);
  Exporter.SetClipBounds(Exporter.ColumnClipBounds);
  Exporter.PopBackgroundLayer;

  if AHeaderFooter <> nil then
    Exporter.EndHeaderFooterExport(AOldClipBounds);
end;

procedure TdxRichEditPagePainter.DrawComments(APage: TdxRichEditLayoutPage);
begin
  if Exporter.ShouldExportComments(APage.ModelPage) then
  begin
    Exporter.BeforeExportComments(APage.ModelPage);
    FPainterVisitor.DefaultVisitComments(APage);
  end;
end;

procedure TdxRichEditPagePainter.DrawComment(AComment: TdxRichEditLayoutComment);
var
  AExporter: TdxBoundedDocumentLayoutExporter;
  APageContentClipping, AOldVisibleBounds: TRect;
  AOldClipRect, AClipRect: TdxRectF;
  AOldPieceTable: TdxPieceTable;
begin
  AExporter := TdxBoundedDocumentLayoutExporter(Exporter);
  APageContentClipping := AExporter.Bounds;
  AExporter.Bounds := AComment.ContentBounds;
  AOldVisibleBounds := AExporter.VisibleBounds;
  if AExporter <> nil then
    AExporter.VisibleBounds.Empty;

  AOldClipRect.Empty;
  AClipRect.Empty;
  AOldPieceTable := nil;
  if not Exporter.BeginExportCompositeObject(GetDrawingBounds(AComment.ContentBounds), AComment.PieceTable,
    AOldClipRect, AClipRect, AOldPieceTable) then
    Exit;

  Exporter.PushBackgroundLayer;
  Exporter.BeforeExportComment(AComment.ModelComment);
  if Exporter.ShouldExportComment(AComment.ModelComment) then
    FPainterVisitor.DefaultVisitComment(AComment);
  Exporter.AfterExportComment(AComment.ModelComment, ModelPage.CommentBounds);
  Exporter.PopBackgroundLayer;

  Exporter.EndExportCompositeObject(AOldClipRect, AOldPieceTable);
  AExporter.Bounds := APageContentClipping;
  AExporter.VisibleBounds := AOldVisibleBounds;
end;

procedure TdxRichEditPagePainter.DrawHeader(AHeader: TdxRichEditLayoutHeader);
var
  AClipBounds: TdxRectF;
begin
  AClipBounds := Exporter.CalculateHeaderFooterClipBounds(AHeader.ModelHeaderFooter.ContentBounds.ToRectF);
  if AClipBounds.IsZero then
    Exit;

  Exporter.PushBackgroundLayer;
  Exporter.ApplyActivePieceTable(AHeader.ModelHeaderFooter);
  FPainterVisitor.DefaultVisitHeader(AHeader);
  Exporter.RectorePieceTable;
  Exporter.PopBackgroundLayer;
end;

procedure TdxRichEditPagePainter.DrawFooter(AFooter: TdxRichEditLayoutFooter);
var
  AClipBounds: TdxRectF;
begin
  AClipBounds := Exporter.CalculateHeaderFooterClipBounds(AFooter.ModelHeaderFooter.ContentBounds.ToRectF);
  if AClipBounds.IsZero then
    Exit;

  Exporter.PushBackgroundLayer;
  Exporter.ApplyActivePieceTable(AFooter.ModelHeaderFooter);
  FPainterVisitor.DefaultVisitFooter(AFooter);
  Exporter.RectorePieceTable;
  Exporter.PopBackgroundLayer;
end;

procedure TdxRichEditPagePainter.DrawFrame(AFrame: TdxRichEditLayoutFrame);
begin
  Exporter.ExportParagraphFrameBox(AFrame.ModelBox);
end;

procedure TdxRichEditPagePainter.DrawParagraphFrame(AParagraphFrame: TdxRichEditParagraphFrameBox);
begin
  Exporter.ExportParagraphFrame(AParagraphFrame.ModelBox);
end;

procedure TdxRichEditPagePainter.DrawTextBox(ATextBox: TdxRichEditLayoutTextBox);
var
  AOldClipRect, AClipRect: TdxRectF;
  AOldPieceTable: TdxPieceTable;
  ATransformApplied: Boolean;
  ABounds: TRect;
  AModelFloatingObject: TdxFloatingObjectBox;
  AContent: TdxTextBoxFloatingObjectContent;
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  AOldClipRect.Empty;
  AClipRect.Empty;
  AOldPieceTable := nil;
  AModelFloatingObject := ATextBox.ModelFloatingObject;
  ATransformApplied := Exporter.BeforeExportRotatedContent(AModelFloatingObject);
  try
    Exporter.ExportFloatingObjectShape(AModelFloatingObject, ATextBox.AnchorRun.Shape);
    ABounds := GetDrawingBounds(ATextBox.ContentBounds);

    AContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ATextBox.AnchorRun.Content);
    if AContent <> nil then
    begin
      AUnitConverter := AModelFloatingObject.PieceTable.DocumentModel.ToDocumentLayoutUnitConverter;
      Dec(ABounds.Bottom, AUnitConverter.ToLayoutUnits(AContent.TextBoxProperties.BottomMargin));
    end;

    if not Exporter.BeginExportCompositeObject(ABounds, (IdxPieceTableContainer(ATextBox)).PieceTable, AOldClipRect, AClipRect, AOldPieceTable) then
      Exit;
    Exporter.PushBackgroundLayer;

    Exporter.SetBackColor(ATextBox.AnchorRun.Shape.FillColor, ABounds);
    FPainterVisitor.DefaultVisitTextBox(ATextBox);
    Exporter.PopBackgroundLayer;
    Exporter.EndExportCompositeObject(AOldClipRect, AOldPieceTable);
  finally
    Exporter.AfterExportRotatedContent(ATransformApplied);
  end;
end;

procedure TdxRichEditPagePainter.DrawFloatingPicture(AFloatingPicture: TdxRichEditLayoutFloatingPicture);
begin
  Exporter.ExportFloatingObjectBox(AFloatingPicture.ModelFloatingObject);
end;

procedure TdxRichEditPagePainter.DrawTable(ATable: TdxRichEditLayoutTable);
var
  AClip: TdxRectF;
begin
  ATable.ModelTable.ExportBackground(Exporter);
  FPainterVisitor.DefaultVisitTable(ATable);
  AClip := Exporter.ApplyClipBounds(Exporter.GetClipBounds);
  ATable.ModelTable.ExportTo(Exporter);
  Exporter.RestoreClipBounds(AClip);
end;

procedure TdxRichEditPagePainter.DrawTableRow(ATableRow: TdxRichEditLayoutTableRow);
begin
  FPainterVisitor.DefaultVisitTableRow(ATableRow);
end;

procedure TdxRichEditPagePainter.DrawTableCell(ATableCell: TdxRichEditLayoutTableCell);
 var
   AOldClipBounds: TdxRectF;
begin
   AOldClipBounds := Exporter.BeginExportTableCell(ATableCell.ModelTableCell);
   FPainterVisitor.DefaultVisitTableCell(ATableCell);
   Exporter.EndExportTableCell(AOldClipBounds);
end;

function TdxRichEditPagePainter.ShouldExportRowBackground(ARowExtendedBoxes: TdxRichEditRowExtendedBoxes): Boolean;
begin
  if (ARowExtendedBoxes.HighlightAreas <> nil) and (ARowExtendedBoxes.HighlightAreas.Count > 0) then
    Exit(True);
  if (ARowExtendedBoxes.FieldHighlightAreas <> nil) and (ARowExtendedBoxes.FieldHighlightAreas.Count > 0) then
    Exit(True);
  if (ARowExtendedBoxes.RangePermissionHighlightAreas <> nil) and (ARowExtendedBoxes.RangePermissionHighlightAreas.Count > 0) then
    Exit(True);
  if (ARowExtendedBoxes.CommentHighlightAreas <> nil) and (ARowExtendedBoxes.CommentHighlightAreas.Count > 0) then
    Exit(True);
  Result := False;
end;

procedure TdxRichEditPagePainter.DrawRowBackground(ARowExtendedBoxes: TdxRichEditRowExtendedBoxes);
begin
  if ShouldExportRowBackground(ARowExtendedBoxes) then
    FPainterVisitor.DefaultVisitRowBackground(ARowExtendedBoxes);
end;

function TdxRichEditPagePainter.ShouldDrawRow(ARow: TdxRichEditLayoutRow): Boolean;
begin
  Result := Exporter.ShouldExportRow(ARow.ModelRow);
end;

procedure TdxRichEditPagePainter.DrawRow(ARow: TdxRichEditLayoutRow);
begin
  if not ShouldDrawRow(ARow) then
    Exit;

  Exporter.SetCurrentRow(ARow.ModelRow);

  FPainterVisitor.DefaultVisitRow(ARow);
  Exporter.SetCurrentRow(nil);
end;

procedure TdxRichEditPagePainter.DrawPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox);
begin
  Exporter.ExportTextBox(TdxTextBox(APlainTextBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox);
begin
  Exporter.ExportTextBox(TdxTextBox(ASpecialTextBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawInlinePictureBox(AInlinePictureBox: TdxRichEditInlinePictureBox);
begin
  Exporter.ExportInlinePictureBox(TdxInlinePictureBox(AInlinePictureBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawFloatingObjectAnchorBox(AFloatingObjectAnchorBox: TdxRichEditFloatingObjectAnchorBox);
begin
end;

procedure TdxRichEditPagePainter.DrawSpaceBox(ASpaceBox: TdxRichEditPlainTextBox);
begin
  Exporter.ExportSpaceBox(ASpaceBox.ModelBox);
end;

procedure TdxRichEditPagePainter.DrawParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox);
begin
  Exporter.ExportParagraphMarkBox(TdxParagraphMarkBox(AParagraphMarkBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawSectionBreakBox(ASectionBreakBox: TdxRichEditPlainTextBox);
begin
  Exporter.ExportSectionMarkBox(TdxSectionMarkBox(ASectionBreakBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox);
begin
  Exporter.ExportLineBreakBox(TdxLineBreakBox(ALineBreakBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawPageBreakBox(APageBreakBox: TdxRichEditPlainTextBox);
begin
  Exporter.ExportPageBreakBox(TdxPageBreakBox(APageBreakBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawColumnBreakBox(AColumnBreakBox: TdxRichEditPlainTextBox);
begin
  Exporter.ExportColumnBreakBox(TdxColumnBreakBox(AColumnBreakBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawHyphenBox(AHyphen: TdxRichEditPlainTextBox);
begin
  Exporter.ExportHyphenBox(TdxHyphenBox(AHyphen.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox);
begin
  Exporter.ExportTabSpaceBox(TdxTabSpaceBox(ATabSpaceBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawPageNumberBox(APageNumberBox: TdxRichEditPlainTextBox);
begin
  Exporter.ExportLayoutDependentTextBox(TdxLayoutDependentTextBox(APageNumberBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawNumberingListMarkBox(ANumberingListMarkBox: TdxRichEditNumberingListMarkBox);
begin
  Exporter.ExportNumberingListBox(TdxNumberingListBox(ANumberingListMarkBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox: TdxRichEditNumberingListWithSeparatorBox);
begin
  Exporter.ExportNumberingListBox(TdxNumberingListBox(ANumberingListWithSeparatorBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawUnderlineBoxes(AUnderlineBoxes: TdxRichEditUnderlineBoxCollection);
var
  ACurrentRow: TdxRichEditLayoutRow;
begin
  if AUnderlineBoxes.Count = 0 then
    Exit;
  ACurrentRow := AUnderlineBoxes[0].GetParentByType<TdxRichEditLayoutRow>;
  if not ShouldOptimizeBox(ACurrentRow) then
    FPainterVisitor.DefaultVisitUnderlineBoxes(AUnderlineBoxes);
end;

procedure TdxRichEditPagePainter.DrawStrikeoutBoxes(AStrikeoutBoxes: TdxRichEditStrikeoutBoxCollection);
var
  ACurrentRow: TdxRichEditLayoutRow;
begin
  if AStrikeoutBoxes.Count = 0 then
    Exit;
  ACurrentRow := AStrikeoutBoxes[0].GetParentByType<TdxRichEditLayoutRow>;
  if not ShouldOptimizeBox(ACurrentRow) then
    FPainterVisitor.DefaultVisitStrikeoutBoxes(AStrikeoutBoxes);
end;

procedure TdxRichEditPagePainter.DrawBookmarkBoxes(ABookmarkBoxes: TdxRichEditBookmarkBoxCollection);
begin
  if ABookmarkBoxes.Count > 0 then
    FPainterVisitor.DefaultVisitBookmarkBoxes(ABookmarkBoxes);
end;

procedure TdxRichEditPagePainter.DrawUnderlineBox(AUnderlineBox: TdxRichEditUnderlineBox);
var
  ACurrentRow: TdxRichEditLayoutRow;
begin
  ACurrentRow := AUnderlineBox.GetParentByType<TdxRichEditLayoutRow>;
  Exporter.ExportUnderlineBox(ACurrentRow.ModelRow, TdxUnderlineBox(AUnderlineBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawStrikeoutBox(AStrikeoutBox: TdxRichEditStrikeoutBox);
var
  ACurrentRow: TdxRichEditLayoutRow;
begin
  ACurrentRow := AStrikeoutBox.GetParentByType<TdxRichEditLayoutRow>;
  Exporter.ExportStrikeoutBox(ACurrentRow.ModelRow, TdxUnderlineBox(AStrikeoutBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawErrorBox(AErrorBox: TdxRichEditErrorBox);
begin
  Exporter.ExportErrorBox(TdxErrorBox(AErrorBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawHighlightAreaBox(AHighlightAreaBox: TdxRichEditHighlightAreaBox);
begin
  Exporter.ExportHighlightArea(AHighlightAreaBox.ModelBox);
end;

procedure TdxRichEditPagePainter.DrawBookmarkStartBox(ABookmarkStartBox: TdxRichEditBookmarkBox);
begin
  Exporter.ExportBookmarkStartBox(TdxVisitableDocumentIntervalBox(ABookmarkStartBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawBookmarkEndBox(ABookmarkEndBox: TdxRichEditBookmarkBox);
begin
  Exporter.ExportBookmarkEndBox(TdxVisitableDocumentIntervalBox(ABookmarkEndBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawHiddenTextUnderlineBox(AHiddenTextUnderlineBox: TdxRichEditHiddenTextUnderlineBox);
begin
end;

procedure TdxRichEditPagePainter.DrawLineNumberBox(ALineNumberBox: TdxRichEditLineNumberBox);
begin
  Exporter.ExportLineNumberBox(TdxLineNumberBox(ALineNumberBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawFieldHighlightAreaBox(AFieldHighlightAreaBox: TdxRichEditFieldHighlightAreaBox);
begin
  Exporter.ExportHighlightArea(AFieldHighlightAreaBox.ModelBox);
end;

procedure TdxRichEditPagePainter.DrawRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox: TdxRichEditRangePermissionHighlightAreaBox);
begin
  Exporter.ExportHighlightArea(ARangePermissionHighlightAreaBox.ModelBox);
end;

procedure TdxRichEditPagePainter.DrawRangePermissionStartBox(ARangePermissionStartBox: TdxRichEditRangePermissionBox);
begin
end;

procedure TdxRichEditPagePainter.DrawRangePermissionEndBox(ARangePermissionEndBox: TdxRichEditRangePermissionBox);
begin
end;

procedure TdxRichEditPagePainter.DrawCommentHighlightAreaBox(ACommentHighlightAreaBox: TdxRichEditCommentHighlightAreaBox);
begin
  Exporter.ExportHighlightArea(ACommentHighlightAreaBox.ModelBox);
end;

procedure TdxRichEditPagePainter.DrawCommentStartBox(ACommentStartBox: TdxRichEditCommentBox);
begin
  Exporter.ExportCommentStartBox(TdxVisitableDocumentIntervalBox(ACommentStartBox.ModelBox));
end;

procedure TdxRichEditPagePainter.DrawCommentEndBox(ACommentEndBox: TdxRichEditCommentBox);
begin
  Exporter.ExportCommentEndBox(TdxVisitableDocumentIntervalBox(ACommentEndBox.ModelBox));
end;

{ TdxRichEditLayoutVisitor }

procedure TdxRichEditLayoutVisitor.Visit(const AElement: IdxRichEditLayoutElement);
begin
  AElement.Accept(Self);
end;

procedure TdxRichEditLayoutVisitor.VisitPage(APage: TdxRichEditLayoutPage);
var
  I: Integer;
begin
  if APage.Header <> nil then
    VisitHeader(APage.Header);
  if APage.Footer <> nil then
    VisitFooter(APage.Footer);
  for I := 0 to APage.PageAreas.Count - 1 do
    VisitPageArea(APage.PageAreas[I]);
end;

procedure TdxRichEditLayoutVisitor.VisitPageArea(APageArea: TdxRichEditLayoutPageArea);
begin
  VisitArea(APageArea);
end;

procedure TdxRichEditLayoutVisitor.VisitColumn(AColumn: TdxRichEditLayoutColumn);
begin
  VisitLayoutRowCollection(AColumn.Rows);
  VisitLayoutElementsCollection(AColumn.Tables);
  VisitLayoutElementsCollection(AColumn.LineNumbers);
end;

procedure TdxRichEditLayoutVisitor.VisitComments(APage: TdxRichEditLayoutPage);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitComment(AComment: TdxRichEditLayoutComment);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitHeader(AHeader: TdxRichEditLayoutHeader);
begin
  VisitArea(AHeader);
end;

procedure TdxRichEditLayoutVisitor.VisitFooter(AFooter: TdxRichEditLayoutFooter);
begin
  VisitArea(AFooter);
end;

procedure TdxRichEditLayoutVisitor.VisitTextBox(ATextBox: TdxRichEditLayoutTextBox);
begin
  VisitLayoutRowCollection(ATextBox.Rows);
  VisitLayoutElementsCollection(ATextBox.Tables);
end;

procedure TdxRichEditLayoutVisitor.VisitFloatingPicture(AFloatingPicture: TdxRichEditLayoutFloatingPicture);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitTable(ATable: TdxRichEditLayoutTable);
begin
  VisitLayoutElementsCollection(ATable.TableRows);
end;

procedure TdxRichEditLayoutVisitor.VisitTableRow(ATableRow: TdxRichEditLayoutTableRow);
begin
  VisitLayoutElementsCollection(ATableRow.TableCells);
end;

procedure TdxRichEditLayoutVisitor.VisitTableCell(ATableCell: TdxRichEditLayoutTableCell);
begin
  VisitLayoutRowCollection(ATableCell.Rows);
  VisitLayoutElementsCollection(ATableCell.NestedTables);
end;

procedure TdxRichEditLayoutVisitor.VisitRowBackground(ARowExtendedBoxes: TdxRichEditRowExtendedBoxes);
begin
  VisitLayoutElementsCollection(ARowExtendedBoxes.FieldHighlightAreas);
  VisitLayoutElementsCollection(ARowExtendedBoxes.RangePermissionHighlightAreas);
  VisitLayoutElementsCollection(ARowExtendedBoxes.CommentHighlightAreas);
  VisitLayoutElementsCollection(ARowExtendedBoxes.HighlightAreas);
end;

procedure TdxRichEditLayoutVisitor.VisitRow(ARow: TdxRichEditLayoutRow);
var
  ARowExtendedBoxes: TdxRichEditRowExtendedBoxes;
begin
  ARowExtendedBoxes := ARow.ExtendedBoxes;

  VisitRowBackground(ARowExtendedBoxes);

  if ARow.NumberingListBox <> nil then
    ARow.NumberingListBox.Accept(Self);

  VisitLayoutElementsCollection(ARow.Boxes);
  VisitUnderlineBoxes(ARowExtendedBoxes.Underlines);
  VisitStrikeoutBoxes(ARowExtendedBoxes.Strikeouts);
  VisitErrorBoxes(ARowExtendedBoxes.Errors);

  VisitBookmarkBoxes(ARowExtendedBoxes.BookmarkBoxes);
  VisitLayoutElementsCollection(ARowExtendedBoxes.RangePermissionBoxes);
  VisitLayoutElementsCollection(ARowExtendedBoxes.CommentBoxes);
  VisitLayoutElementsCollection(ARowExtendedBoxes.HiddenTextUnderlineBoxes);
end;

procedure TdxRichEditLayoutVisitor.VisitUnderlineBoxes(AUnderlineBoxes: TdxRichEditUnderlineBoxCollection);
begin
  VisitLayoutElementsCollection(AUnderlineBoxes);
end;

procedure TdxRichEditLayoutVisitor.VisitStrikeoutBoxes(AStrikeoutBoxes: TdxRichEditStrikeoutBoxCollection);
begin
  VisitLayoutElementsCollection(AStrikeoutBoxes);
end;

procedure TdxRichEditLayoutVisitor.VisitErrorBoxes(AErrorBoxes: TdxRichEditErrorBoxCollection);
begin
  VisitLayoutElementsCollection(AErrorBoxes);
end;

procedure TdxRichEditLayoutVisitor.VisitBookmarkBoxes(ABookmarkBoxes: TdxRichEditBookmarkBoxCollection);
begin
  VisitLayoutElementsCollection(ABookmarkBoxes);
end;

procedure TdxRichEditLayoutVisitor.VisitPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitInlinePictureBox(AInlinePictureBox: TdxRichEditInlinePictureBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitFloatingObjectAnchorBox(AFloatingObjectAnchorBox: TdxRichEditFloatingObjectAnchorBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitSpaceBox(ASpaceBox: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitSectionBreakBox(ASectionBreakBox: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitPageBreakBox(APageBreakBox: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitColumnBreakBox(AColumnBreakBox: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitHyphenBox(AHyphen: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitPageNumberBox(APageNumberBox: TdxRichEditPlainTextBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitNumberingListMarkBox(ANumberingListMarkBox: TdxRichEditNumberingListMarkBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox: TdxRichEditNumberingListWithSeparatorBox);
begin
  ANumberingListWithSeparatorBox.Separator.Accept(Self);
end;

procedure TdxRichEditLayoutVisitor.VisitUnderlineBox(AUnderlineBox: TdxRichEditUnderlineBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitStrikeoutBox(AStrikeoutBox: TdxRichEditStrikeoutBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitErrorBox(AErrorBox: TdxRichEditErrorBox);
begin
end;


procedure TdxRichEditLayoutVisitor.VisitHighlightAreaBox(AHighlightAreaBox: TdxRichEditHighlightAreaBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitBookmarkStartBox(ABookmarkStartBox: TdxRichEditBookmarkBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitBookmarkEndBox(ABookmarkEndBox: TdxRichEditBookmarkBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitHiddenTextUnderlineBox(AHiddenTextUnderlineBox: TdxRichEditHiddenTextUnderlineBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitLineNumberBox(ALineNumberBox: TdxRichEditLineNumberBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitFieldHighlightAreaBox(AFieldHighlightAreaBox: TdxRichEditFieldHighlightAreaBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox: TdxRichEditRangePermissionHighlightAreaBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitRangePermissionStartBox(ARangePermissionStartBox: TdxRichEditRangePermissionBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitRangePermissionEndBox(ARangePermissionEndBox: TdxRichEditRangePermissionBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitCommentHighlightAreaBox(ACommentHighlightAreaBox: TdxRichEditCommentHighlightAreaBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitCommentStartBox(ACommentStartBox: TdxRichEditCommentBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitCommentEndBox(ACommentEndBox: TdxRichEditCommentBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitFrame(AFrame: TdxRichEditLayoutFrame);
begin
  VisitLayoutRowCollection(AFrame.Rows);
  VisitLayoutElementsCollection(AFrame.Tables);
end;

procedure TdxRichEditLayoutVisitor.VisitParagraphFrameBox(AParagraphFrameBox: TdxRichEditParagraphFrameBox);
begin
end;

procedure TdxRichEditLayoutVisitor.VisitLayoutElementsCollection(ACollection: TdxRichEditLayoutElementCollection);
var
  ACount, I: Integer;
  AIntf: IdxRichEditLayoutElement;
begin
  ACount := ACollection.Count;
  for I := 0 to ACount - 1 do
  begin
    if Supports(ACollection[I], IdxRichEditLayoutElement, AIntf) then
      AIntf.Accept(Self);
  end;
end;

procedure TdxRichEditLayoutVisitor.VisitLayoutRowCollection(ACollection: TdxRichEditLayoutRowCollection);
var
  ARow: TdxRichEditLayoutRow;
  AIntf: IdxRichEditParagraphFrameContainer;
begin
  if ACollection.Count = 0 then
    Exit;
  ARow := ACollection.First;
  AIntf := ARow.GetParentByIntf(IdxRichEditParagraphFrameContainer) as IdxRichEditParagraphFrameContainer;
  VisitParagraphFrameBoxes(AIntf, ARow.Parent);
  VisitLayoutElementsCollection(ACollection);
end;

procedure TdxRichEditLayoutVisitor.VisitParagraphFrameBoxes(const AParagraphFrameContainer: IdxRichEditParagraphFrameContainer;
  const ARowParent: IdxRichEditLayoutElement);
var
  ACollection: TdxRichEditParagraphFrameBoxCollection;
  ACount, I: Integer;
begin
  ACollection := AParagraphFrameContainer.ParagraphFrames;
  ACount := ACollection.Count;
  for I := 0 to ACount - 1 do
  begin
    if ACollection[I].DrawingParent = ARowParent then
      ACollection[I].Accept(Self);
  end;
end;

procedure TdxRichEditLayoutVisitor.VisitArea(AArea: TdxRichEditLayoutPageAreaBase);
var
  APieceTable: TdxPieceTable;
  APage: TdxRichEditLayoutPage;
begin
  APieceTable := IdxPieceTableContainer(AArea).PieceTable;
  APage := AArea.GetParentByType<TdxRichEditLayoutPage>;

  VisitFloatingObjects(APage.BackgroundFloatingObjects, APieceTable);

  VisitLayoutElementsCollection(AArea.Columns);
  VisitFrames(APage.Frames, APieceTable);

  VisitFloatingObjects(APage.FloatingObjects, APieceTable);
  VisitFloatingObjects(APage.ForegroundFloatingObjects, APieceTable);
end;

procedure TdxRichEditLayoutVisitor.VisitFloatingObjects(ACollection: TdxRichEditLayoutFloatingObjectCollection; APieceTable: TdxPieceTable);
var
  ACount, I: Integer;
  AFloatingObject: TdxRichEditLayoutFloatingObject;
begin
  ACount := ACollection.Count;
  for I := 0 to ACount - 1 do
  begin
    AFloatingObject := ACollection[I];
    if AFloatingObject.ModelFloatingObject.PieceTable = APieceTable then
      AFloatingObject.Accept(Self);
  end;
end;

procedure TdxRichEditLayoutVisitor.VisitFrames(ACollection: TdxRichEditLayoutFrameCollection;
  APieceTable: TdxPieceTable);
var
  ACount, I: Integer;
  AParagraphFrame: TdxRichEditLayoutFrame;
begin
  ACount := ACollection.Count;
  for I := 0 to ACount - 1 do
  begin
    AParagraphFrame := ACollection[I];
    if AParagraphFrame.ModelBox.PieceTable = APieceTable then
      AParagraphFrame.Accept(Self);
  end;
end;

{ TdxRichEditPagePainterVisitor }

constructor TdxRichEditPagePainterVisitor.Create(APainter: TdxRichEditPagePainter);
begin
  inherited Create;
  FPainter := APainter;
end;

procedure TdxRichEditPagePainterVisitor.VisitPage(APage: TdxRichEditLayoutPage);
begin
  FPainter.DrawPage(APage);
end;

procedure TdxRichEditPagePainterVisitor.VisitRowBackground(ARowExtendedBoxes: TdxRichEditRowExtendedBoxes);
begin
  FPainter.DrawRowBackground(ARowExtendedBoxes);
end;

procedure TdxRichEditPagePainterVisitor.VisitRow(ARow: TdxRichEditLayoutRow);
begin
  FPainter.DrawRow(ARow);
end;

procedure TdxRichEditPagePainterVisitor.VisitUnderlineBoxes(AUnderlineBoxes: TdxRichEditUnderlineBoxCollection);
begin
  FPainter.DrawUnderlineBoxes(AUnderlineBoxes);
end;

procedure TdxRichEditPagePainterVisitor.VisitStrikeoutBoxes(AStrikeoutBoxes: TdxRichEditStrikeoutBoxCollection);
begin
  FPainter.DrawStrikeoutBoxes(AStrikeoutBoxes);
end;

procedure TdxRichEditPagePainterVisitor.VisitBookmarkBoxes(ABookmarkBoxes: TdxRichEditBookmarkBoxCollection);
begin
  FPainter.DrawBookmarkBoxes(ABookmarkBoxes);
end;

procedure TdxRichEditPagePainterVisitor.VisitTable(ATable: TdxRichEditLayoutTable);
begin
  FPainter.DrawTable(ATable);
end;

procedure TdxRichEditPagePainterVisitor.VisitTableRow(ATableRow: TdxRichEditLayoutTableRow);
begin
  FPainter.DrawTableRow(ATableRow);
end;

procedure TdxRichEditPagePainterVisitor.VisitTableCell(ATableCell: TdxRichEditLayoutTableCell);
begin
  FPainter.DrawTableCell(ATableCell);
end;

procedure TdxRichEditPagePainterVisitor.VisitPageArea(APageArea: TdxRichEditLayoutPageArea);
begin
  FPainter.DrawPageArea(APageArea);
end;

procedure TdxRichEditPagePainterVisitor.VisitColumn(AColumn: TdxRichEditLayoutColumn);
begin
  FPainter.DrawColumn(AColumn);
end;

procedure TdxRichEditPagePainterVisitor.VisitComments(APage: TdxRichEditLayoutPage);
begin
  FPainter.DrawComments(APage);
end;

procedure TdxRichEditPagePainterVisitor.VisitComment(AComment: TdxRichEditLayoutComment);
begin
  FPainter.DrawComment(AComment);
end;

procedure TdxRichEditPagePainterVisitor.VisitHeader(AHeader: TdxRichEditLayoutHeader);
begin
  FPainter.DrawHeader(AHeader);
end;

procedure TdxRichEditPagePainterVisitor.VisitFooter(AFooter: TdxRichEditLayoutFooter);
begin
  FPainter.DrawFooter(AFooter);
end;

procedure TdxRichEditPagePainterVisitor.VisitTextBox(ATextBox: TdxRichEditLayoutTextBox);
begin
  FPainter.DrawTextBox(ATextBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitFloatingPicture(AFloatingPicture: TdxRichEditLayoutFloatingPicture);
begin
  FPainter.DrawFloatingPicture(AFloatingPicture);
end;

procedure TdxRichEditPagePainterVisitor.VisitPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawPlainTextBox(APlainTextBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawSpecialTextBox(ASpecialTextBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitInlinePictureBox(AInlinePictureBox: TdxRichEditInlinePictureBox);
begin
  FPainter.DrawInlinePictureBox(AInlinePictureBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitFloatingObjectAnchorBox(AFloatingObjectAnchorBox: TdxRichEditFloatingObjectAnchorBox);
begin
  FPainter.DrawFloatingObjectAnchorBox(AFloatingObjectAnchorBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitSpaceBox(ASpaceBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawSpaceBox(ASpaceBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawParagraphMarkBox(AParagraphMarkBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitSectionBreakBox(ASectionBreakBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawSectionBreakBox(ASectionBreakBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawLineBreakBox(ALineBreakBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitPageBreakBox(APageBreakBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawPageBreakBox(APageBreakBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitColumnBreakBox(AColumnBreakBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawColumnBreakBox(AColumnBreakBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitHyphenBox(AHyphenBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawHyphenBox(AHyphenBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawTabSpaceBox(ATabSpaceBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitPageNumberBox(APageNumberBox: TdxRichEditPlainTextBox);
begin
  FPainter.DrawPageNumberBox(APageNumberBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitNumberingListMarkBox(ANumberingListMarkBox: TdxRichEditNumberingListMarkBox);
begin
  FPainter.DrawNumberingListMarkBox(ANumberingListMarkBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox: TdxRichEditNumberingListWithSeparatorBox);
begin
  FPainter.DrawNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitUnderlineBox(AUnderlineBox: TdxRichEditUnderlineBox);
begin
  FPainter.DrawUnderlineBox(AUnderlineBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitStrikeoutBox(AStrikeoutBox: TdxRichEditStrikeoutBox);
begin
  FPainter.DrawStrikeoutBox(AStrikeoutBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitErrorBox(AErrorBox: TdxRichEditErrorBox);
begin
  FPainter.DrawErrorBox(AErrorBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitHighlightAreaBox(AHighlightAreaBox: TdxRichEditHighlightAreaBox);
begin
  FPainter.DrawHighlightAreaBox(AHighlightAreaBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitBookmarkStartBox(ABookmarkStartBox: TdxRichEditBookmarkBox);
begin
  FPainter.DrawBookmarkStartBox(ABookmarkStartBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitBookmarkEndBox(ABookmarkEndBox: TdxRichEditBookmarkBox);
begin
  FPainter.DrawBookmarkEndBox(ABookmarkEndBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitHiddenTextUnderlineBox(AHiddenTextUnderlineBox: TdxRichEditHiddenTextUnderlineBox);
begin
  FPainter.DrawHiddenTextUnderlineBox(AHiddenTextUnderlineBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitLineNumberBox(ALineNumberBox: TdxRichEditLineNumberBox);
begin
  FPainter.DrawLineNumberBox(ALineNumberBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitFieldHighlightAreaBox(AFieldHighlightAreaBox: TdxRichEditFieldHighlightAreaBox);
begin
  FPainter.DrawFieldHighlightAreaBox(AFieldHighlightAreaBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox: TdxRichEditRangePermissionHighlightAreaBox);
begin
  FPainter.DrawRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitRangePermissionStartBox(ARangePermissionStartBox: TdxRichEditRangePermissionBox);
begin
  FPainter.DrawRangePermissionStartBox(ARangePermissionStartBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitRangePermissionEndBox(ARangePermissionEndBox: TdxRichEditRangePermissionBox);
begin
  FPainter.DrawRangePermissionEndBox(ARangePermissionEndBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitCommentHighlightAreaBox(ACommentHighlightAreaBox: TdxRichEditCommentHighlightAreaBox);
begin
  FPainter.DrawCommentHighlightAreaBox(ACommentHighlightAreaBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitCommentStartBox(ACommentStartBox: TdxRichEditCommentBox);
begin
  FPainter.DrawCommentStartBox(ACommentStartBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitCommentEndBox(ACommentEndBox: TdxRichEditCommentBox);
begin
  FPainter.DrawCommentEndBox(ACommentEndBox);
end;

procedure TdxRichEditPagePainterVisitor.VisitFrame(AFrame: TdxRichEditLayoutFrame);
begin
  FPainter.DrawFrame(AFrame);
end;

procedure TdxRichEditPagePainterVisitor.VisitParagraphFrameBox(AParagraphFrameBox: TdxRichEditParagraphFrameBox);
begin
  FPainter.DrawParagraphFrame(AParagraphFrameBox);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitPage(APage: TdxRichEditLayoutPage);
begin
  inherited VisitPage(APage);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitPageArea(APageArea: TdxRichEditLayoutPageArea);
begin
  inherited VisitPageArea(APageArea);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitColumn(AColumn: TdxRichEditLayoutColumn);
begin
  inherited VisitColumn(AColumn);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitComments(APage: TdxRichEditLayoutPage);
begin
  inherited VisitComments(APage);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitComment(AComment: TdxRichEditLayoutComment);
begin
  inherited VisitComment(AComment);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitHeader(AHeader: TdxRichEditLayoutHeader);
begin
  inherited VisitHeader(AHeader);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitFooter(AFooter: TdxRichEditLayoutFooter);
begin
  inherited VisitFooter(AFooter);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitTextBox(ATextBox: TdxRichEditLayoutTextBox);
begin
  inherited VisitTextBox(ATextBox);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitTable(ATable: TdxRichEditLayoutTable);
begin
  inherited VisitTable(ATable);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitTableRow(ATableRow: TdxRichEditLayoutTableRow);
begin
  inherited VisitTableRow(ATableRow);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitTableCell(ATableCell: TdxRichEditLayoutTableCell);
begin
  inherited VisitTableCell(ATableCell);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitRowBackground(ARowExtendedBoxes: TdxRichEditRowExtendedBoxes);
begin
  inherited VisitRowBackground(ARowExtendedBoxes);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitRow(ARow: TdxRichEditLayoutRow);
begin
  inherited VisitRow(ARow);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitUnderlineBoxes(AUnderlineBoxes: TdxRichEditUnderlineBoxCollection);
begin
  inherited VisitUnderlineBoxes(AUnderlineBoxes);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitStrikeoutBoxes(AStrikeoutBoxes: TdxRichEditStrikeoutBoxCollection);
begin
  inherited VisitStrikeoutBoxes(AStrikeoutBoxes);
end;

procedure TdxRichEditPagePainterVisitor.DefaultVisitBookmarkBoxes(ABookmarkBoxes: TdxRichEditBookmarkBoxCollection);
begin
  inherited VisitBookmarkBoxes(ABookmarkBoxes);
end;

{ TdxRichEditLayoutElementSearcher }

constructor TdxRichEditLayoutElementSearcher.Create(APosition: Integer; APieceTable: TdxPieceTable;
  const AComparison: TdxFunc<IdxRichEditRangedLayoutElement, Boolean>);
begin
  inherited Create;
  FPosition := APosition;
  FPieceTable := APieceTable;
  FComparison := AComparison;
end;

procedure TdxRichEditLayoutElementSearcher.VisitColumn(AColumn: TdxRichEditLayoutColumn);
begin
  if Comparison(AColumn) then
    FindElement(AColumn)
  else
    inherited VisitColumn(AColumn);
end;

procedure TdxRichEditLayoutElementSearcher.VisitColumnBreakBox(AColumnBreakBox: TdxRichEditPlainTextBox);
begin
  if Comparison(AColumnBreakBox) then
    FindElement(AColumnBreakBox)
  else
    inherited VisitColumnBreakBox(AColumnBreakBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitComment(AComment: TdxRichEditLayoutComment);
begin
  if not IsInSearchingDocument(IdxPieceTableContainer(AComment)) then
    Exit;
  if Comparison(AComment) then
    FindElement(AComment)
  else
    inherited VisitComment(AComment);
end;

procedure TdxRichEditLayoutElementSearcher.VisitCommentHighlightAreaBox(ACommentHighlightAreaBox: TdxRichEditCommentHighlightAreaBox);
begin
  if Comparison(ACommentHighlightAreaBox) then
    FindElement(ACommentHighlightAreaBox)
  else
    inherited VisitCommentHighlightAreaBox(ACommentHighlightAreaBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitFieldHighlightAreaBox(AFieldHighlightAreaBox: TdxRichEditFieldHighlightAreaBox);
begin
  if Comparison(AFieldHighlightAreaBox) then
    FindElement(AFieldHighlightAreaBox)
  else
    inherited VisitFieldHighlightAreaBox(AFieldHighlightAreaBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitFloatingObjectAnchorBox(AFloatingObjectAnchorBox: TdxRichEditFloatingObjectAnchorBox);
begin
  if Comparison(AFloatingObjectAnchorBox) then
    FindElement(AFloatingObjectAnchorBox)
  else
    inherited VisitFloatingObjectAnchorBox(AFloatingObjectAnchorBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitFooter(AFooter: TdxRichEditLayoutFooter);
begin
  if Comparison(AFooter) and IsInSearchingDocument(IdxPieceTableContainer(AFooter)) then
    FindElement(AFooter)
  else
    inherited VisitFooter(AFooter);
end;

procedure TdxRichEditLayoutElementSearcher.VisitHeader(AHeader: TdxRichEditLayoutHeader);
begin
  if Comparison(AHeader) and IsInSearchingDocument(IdxPieceTableContainer(AHeader)) then
    FindElement(AHeader)
  else
    inherited VisitHeader(AHeader);
end;

procedure TdxRichEditLayoutElementSearcher.VisitHiddenTextUnderlineBox(AHiddenTextUnderlineBox: TdxRichEditHiddenTextUnderlineBox);
begin
  if Comparison(AHiddenTextUnderlineBox) then
    FindElement(AHiddenTextUnderlineBox)
  else
    inherited VisitHiddenTextUnderlineBox(AHiddenTextUnderlineBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitHighlightAreaBox(AHighlightAreaBox: TdxRichEditHighlightAreaBox);
begin
  if Comparison(AHighlightAreaBox) then
    FindElement(AHighlightAreaBox)
  else
    inherited VisitHighlightAreaBox(AHighlightAreaBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitHyphenBox(AHyphen: TdxRichEditPlainTextBox);
begin
  if Comparison(AHyphen) then
    FindElement(AHyphen)
  else
    inherited VisitHyphenBox(AHyphen);
end;

procedure TdxRichEditLayoutElementSearcher.VisitInlinePictureBox(AInlinePictureBox: TdxRichEditInlinePictureBox);
begin
  if Comparison(AInlinePictureBox) then
    FindElement(AInlinePictureBox)
  else
    inherited VisitInlinePictureBox(AInlinePictureBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox);
begin
  if Comparison(ALineBreakBox) then
    FindElement(ALineBreakBox)
  else
    inherited VisitLineBreakBox(ALineBreakBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitPageArea(APageArea: TdxRichEditLayoutPageArea);
begin
  if Comparison(APageArea) and IsInSearchingDocument(IdxPieceTableContainer(APageArea)) then
    FindElement(APageArea)
  else
    inherited VisitPageArea(APageArea);
end;

procedure TdxRichEditLayoutElementSearcher.VisitPageBreakBox(APageBreakBox: TdxRichEditPlainTextBox);
begin
  if Comparison(APageBreakBox) then
    FindElement(APageBreakBox)
  else
    inherited VisitPageBreakBox(APageBreakBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitPageNumberBox(APageNumberBox: TdxRichEditPlainTextBox);
begin
  if Comparison(APageNumberBox) then
    FindElement(APageNumberBox)
  else
    inherited VisitPageNumberBox(APageNumberBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox);
begin
  if Comparison(AParagraphMarkBox) then
    FindElement(AParagraphMarkBox)
  else
    inherited VisitParagraphMarkBox(AParagraphMarkBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox);
begin
  if Comparison(APlainTextBox) then
    FindElement(APlainTextBox)
  else
    inherited VisitPlainTextBox(APlainTextBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox: TdxRichEditRangePermissionHighlightAreaBox);
begin
  if Comparison(ARangePermissionHighlightAreaBox) then
    FindElement(ARangePermissionHighlightAreaBox)
  else
    inherited VisitRangePermissionHighlightAreaBox(ARangePermissionHighlightAreaBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitRow(ARow: TdxRichEditLayoutRow);
begin
  if Comparison(ARow) then
    FindElement(ARow)
  else
    inherited VisitRow(ARow);
end;

procedure TdxRichEditLayoutElementSearcher.VisitSectionBreakBox(ASectionBreakBox: TdxRichEditPlainTextBox);
begin
  if Comparison(ASectionBreakBox) then
    FindElement(ASectionBreakBox)
  else
    inherited VisitSectionBreakBox(ASectionBreakBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitSpaceBox(ASpaceBox: TdxRichEditPlainTextBox);
begin
  if Comparison(ASpaceBox) then
    FindElement(ASpaceBox)
  else
    inherited VisitSpaceBox(ASpaceBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox);
begin
  if Comparison(ASpecialTextBox) then
    FindElement(ASpecialTextBox)
  else
    inherited VisitSpecialTextBox(ASpecialTextBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitStrikeoutBox(AStrikeoutBox: TdxRichEditStrikeoutBox);
begin
  if Comparison(AStrikeoutBox) then
    FindElement(AStrikeoutBox)
  else
    inherited VisitStrikeoutBox(AStrikeoutBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitTable(ATable: TdxRichEditLayoutTable);
begin
  if Comparison(ATable) then
    FindElement(ATable)
  else
    inherited VisitTable(ATable);
end;

procedure TdxRichEditLayoutElementSearcher.VisitTableCell(ATableCell: TdxRichEditLayoutTableCell);
begin
  if Comparison(ATableCell) then
  begin
    FindElement(ATableCell);
    if ATableCell.NestedTables.Count = 0 then
      Exit;
  end;
  inherited VisitTableCell(ATableCell);
end;

procedure TdxRichEditLayoutElementSearcher.VisitTableRow(ATableRow: TdxRichEditLayoutTableRow);
begin
  if Comparison(ATableRow) then
    FindElement(ATableRow)
  else
    inherited VisitTableRow(ATableRow);
end;

procedure TdxRichEditLayoutElementSearcher.VisitTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox);
begin
  if Comparison(ATabSpaceBox) then
    FindElement(ATabSpaceBox)
  else
    inherited VisitTabSpaceBox(ATabSpaceBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitTextBox(ATextBox: TdxRichEditLayoutTextBox);
begin
  if not IsInSearchingDocument(IdxPieceTableContainer(ATextBox)) then
    Exit;
  if Comparison(ATextBox) then
    FindElement(ATextBox)
  else
    inherited VisitTextBox(ATextBox);
end;

procedure TdxRichEditLayoutElementSearcher.VisitUnderlineBox(AUnderlineBox: TdxRichEditUnderlineBox);
begin
  if Comparison(AUnderlineBox) then
    FindElement(AUnderlineBox)
  else
    inherited VisitUnderlineBox(AUnderlineBox);
end;

procedure TdxRichEditLayoutElementSearcher.FindElement(AElement: IdxRichEditRangedLayoutElement);
var
  APieceTableContainer: IdxPieceTableContainer;
begin
  if AElement.Range.Contains(Position) then
  begin
    if not Supports(AElement, IdxPieceTableContainer, APieceTableContainer) then
      APieceTableContainer := AElement.GetParentByIntf(IdxPieceTableContainer) as IdxPieceTableContainer;
    if IsInSearchingDocument(APieceTableContainer) then
      FFoundElement := AElement;
  end;
end;

function TdxRichEditLayoutElementSearcher.IsInSearchingDocument(const AContainer: IdxPieceTableContainer): Boolean;
begin
  Result := PieceTable = AContainer.PieceTable;
end;

{ TdxRichEditDocumentLayout }

constructor TdxRichEditDocumentLayout.Create(const AProvider: IdxRichEditDocumentLayoutProvider);
begin
  inherited Create;
  FProvider := AProvider;
  FPages := TdxRichEditLayoutPageCollection.Create;
  SubscribeToEvents;
end;

destructor TdxRichEditDocumentLayout.Destroy;
begin
  UnsubscribeFromEvents;
  FreeAndNil(FDocumentFormattedResetEvent);
  FreeAndNil(FPages);
  inherited Destroy;
end;

procedure TdxRichEditDocumentLayout.RaiseDocumentLayoutInvalidated(AArgs: TdxDocumentLayoutInvalidatedEventArgs);
begin
  IsDocumentFormattingCompleted := False;
  if not FDocumentLayoutInvalidated.Empty then
    FDocumentLayoutInvalidated.Invoke(Self, AArgs);
end;

procedure TdxRichEditDocumentLayout.RaiseDocumentFormatted;
begin
  IsDocumentFormattingCompleted := True;
  if not FDocumentFormatted.Empty then
    FDocumentFormatted.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxRichEditDocumentLayout.RaisePageFormatted(AArgs: TdxPageFormattedEventArgs);
begin
  if not FPageFormatted.Empty then
    FPageFormatted.Invoke(Self, AArgs);
end;

procedure TdxRichEditDocumentLayout.OnDocumentLayoutInvalidated(ASender: TObject; E: TdxDocumentLayoutInvalidatedEventArgs);
var
  I: Integer;
begin
  DocumentFormattedResetEvent.ResetEvent;
  for I := Pages.Count - 1 downto E.PageIndex do
    Pages.Delete(I);
  RaiseDocumentLayoutInvalidated(E);
end;

procedure TdxRichEditDocumentLayout.OnDocumentFormatted(ASender: TObject; E: TdxEventArgs);
begin
  DocumentFormattedResetEvent.SetEvent;
  RaiseDocumentFormatted;
end;

procedure TdxRichEditDocumentLayout.OnPageFormatted(ASender: TObject; E: TdxPageFormattedEventArgs);
begin
  RaisePageFormatted(E);
end;

function TdxRichEditDocumentLayout.GetDocument: IdxRichEditDocument;
begin
  Result := Provider.Document;
end;

function TdxRichEditDocumentLayout.GetDocumentFormattedResetEvent: TSimpleEvent;
begin
  if FDocumentFormattedResetEvent = nil then
    FDocumentFormattedResetEvent := TSimpleEvent.Create(nil, True, False, '');
  Result := FDocumentFormattedResetEvent;
end;

procedure TdxRichEditDocumentLayout.SubscribeToEvents;
begin
  Provider.AddDocumentLayoutInvalidated(OnDocumentLayoutInvalidated);
  Provider.AddDocumentFormatted(OnDocumentFormatted);
  Provider.AddPageFormatted(OnPageFormatted);
end;

procedure TdxRichEditDocumentLayout.UnsubscribeFromEvents;
begin
  Provider.RemoveDocumentLayoutInvalidated(OnDocumentLayoutInvalidated);
  Provider.RemoveDocumentFormatted(OnDocumentFormatted);
  Provider.RemovePageFormatted(OnPageFormatted);
end;

procedure TdxRichEditDocumentLayout.InitializePages(ADocumentLayout: TdxModelDocumentLayout; APageIndex: Integer);
var
  I: Integer;
begin
  for I := Pages.Count to APageIndex do
    Pages.Add(TdxRichEditLayoutPage.Create(ADocumentLayout.Pages[I], Document));
end;

function TdxRichEditDocumentLayout.GetPage(APageIndex: Integer): TdxRichEditLayoutPage;
var
  ADocumentLayout: TdxModelDocumentLayout;
begin
  if Provider.LayoutCalculationMode = TdxCalculationModeType.Automatic then
  begin
  end;
  ADocumentLayout := Provider.GetDocumentLayout;
  if APageIndex >= ADocumentLayout.Pages.Count then
    Exit(nil);
  Result := GetPageCore(APageIndex, ADocumentLayout);
end;

function TdxRichEditDocumentLayout.GetPageCore(APageIndex: Integer; ADocumentLayout: TdxModelDocumentLayout): TdxRichEditLayoutPage;
begin
  if APageIndex < Pages.Count then
    Exit(Pages[APageIndex]);
  InitializePages(ADocumentLayout, APageIndex);
  Result := Pages[APageIndex];
end;

procedure TdxRichEditDocumentLayout.ThrowNullCallbackException;
begin
  TdxRichEditExceptions.ThrowArgumentException('callback', nil);
end;

procedure TdxRichEditDocumentLayout.ThrowInvalidAsyncOperationException;
begin
  TdxRichEditExceptions.ThrowInvalidOperationException(
    'Cannot obtain the page because the layout is not calculated. Set LayoutCalculationMode to Automatic to perform' +
    ' calculation.');
end;

function TdxRichEditDocumentLayout.TryGetPageAsyncSynchronously(APageIndex: Integer): TdxRichEditLayoutPage;
var
  AModelDocumentLayout: TdxModelDocumentLayout;
begin
  if APageIndex < Pages.Count then
    Exit(Pages[APageIndex]);
  AModelDocumentLayout := Provider.GetDocumentLayoutAsync;
  if (AModelDocumentLayout = nil) or (APageIndex >= AModelDocumentLayout.Pages.Count) or
    (not AModelDocumentLayout.Pages[APageIndex].PrimaryFormattingComplete) then
    Exit(nil);
  if not AModelDocumentLayout.Pages[APageIndex].SecondaryFormattingComplete then
    Provider.PerformPageSecondaryFormatting(AModelDocumentLayout.Pages[APageIndex]);
  Result := GetPageCore(APageIndex, AModelDocumentLayout);
end;

function TdxRichEditDocumentLayout.GetPageIndex(AElement: IdxRichEditLayoutElement): Integer;
var
  APage: TdxRichEditLayoutPage;
begin
  APage := Safe<TdxRichEditLayoutPage>.Cast(AElement.AsObject);
  if APage = nil then
    APage := TdxRichEditLayoutPage(AElement.GetParentByType(TdxRichEditLayoutPage));
  Result := APage.Index;
end;

function TdxRichEditDocumentLayout.GetFormattedPageCount: Integer;
var
  ADocumentLayout: TdxModelDocumentLayout;
begin
  ADocumentLayout := Provider.GetDocumentLayoutAsync;
  if ADocumentLayout <> nil then
    Result := ADocumentLayout.Pages.Count
  else
    Result := 0;
end;

function TdxRichEditDocumentLayout.GetPageCount: Integer;
var
  ADocumentLayout: TdxModelDocumentLayout;
begin
  if Provider.LayoutCalculationMode = TdxCalculationModeType.Manual then
  begin
    ADocumentLayout := Provider.GetDocumentLayout;
    if ADocumentLayout <> nil then
      Exit(ADocumentLayout.Pages.Count)
    else
      Exit(0);
  end;
  if IsDocumentFormattingCompleted then
    Exit(GetFormattedPageCount);
  DocumentFormattedResetEvent.WaitFor(INFINITE);
  Result := GetFormattedPageCount;
end;

class function TdxRichEditDocumentLayout.GetText(AElement: IdxRichEditLayoutElement): string;
var
  AVisitor: TdxRichEditTextCollectorVisitor;
begin
  AVisitor := TdxRichEditTextCollectorVisitor.Create;
  try
    AVisitor.Visit(AElement);
    Result := AVisitor.Text;
  finally
    AVisitor.Free;
  end;
end;



function TdxRichEditDocumentLayout.IsRangeSupported(AType: TdxRichEditLayoutType): Boolean;
begin
  case AType of
    TdxRichEditLayoutType.Page,
    TdxRichEditLayoutType.FloatingPicture,
    TdxRichEditLayoutType.NumberingListMarkBox,
    TdxRichEditLayoutType.NumberingListWithSeparatorBox,
    TdxRichEditLayoutType.BookmarkStartBox,
    TdxRichEditLayoutType.BookmarkEndBox,
    TdxRichEditLayoutType.RangePermissionStartBox,
    TdxRichEditLayoutType.RangePermissionEndBox,
    TdxRichEditLayoutType.CommentStartBox,
    TdxRichEditLayoutType.CommentEndBox,
    TdxRichEditLayoutType.LineNumberBox,
    TdxRichEditLayoutType.ParagraphFrameBox:
      Result := False;
    else
      Result := True;
  end;
end;


class function TdxRichEditDocumentLayout.GetElementFromPage(APosition: Integer; AClass: TClass;
  APieceTable: TdxPieceTable; APage: TdxRichEditLayoutPage): TObject;
var
  AComparison: TdxFunc<IdxRichEditRangedLayoutElement, Boolean>;
  ASearcher: TdxRichEditLayoutElementSearcher;
begin
  AComparison := function (const AElement: IdxRichEditRangedLayoutElement): Boolean
    var
      AObject: TObject;
    begin
      AObject := AElement.AsObject;
      Result := AObject.InheritsFrom(AClass);
    end;
  ASearcher := TdxRichEditLayoutElementSearcher.Create(APosition, APieceTable, AComparison);
  try
    ASearcher.Visit(APage);
    Result := ASearcher.FoundElement.AsObject;
  finally
    ASearcher.Free;
  end;
end;

{ TdxRichEditDocumentLayoutHelper.TdxParagraphFrameDrawingParentSearcher }

constructor TdxRichEditDocumentLayoutHelper.TdxParagraphFrameDrawingParentSearcher.Create(AParagraphFrame: TdxParagraphFrameBox);
begin
  FParagraphFrame := AParagraphFrame;
end;

procedure TdxRichEditDocumentLayoutHelper.TdxParagraphFrameDrawingParentSearcher.VisitColumn(AColumn: TdxRichEditLayoutColumn);
begin
  if FResult = nil then
    inherited VisitColumn(AColumn);
end;

procedure TdxRichEditDocumentLayoutHelper.TdxParagraphFrameDrawingParentSearcher.VisitParagraphFrameBoxes(
  const AParagraphFrameContainer: IdxRichEditParagraphFrameContainer; const ARowParent: IdxRichEditLayoutElement);
begin
end;

procedure TdxRichEditDocumentLayoutHelper.TdxParagraphFrameDrawingParentSearcher.VisitRow(ARow: TdxRichEditLayoutRow);
begin
  if ARow.ModelRow = FParagraphFrame.FirstRow then
    FResult := ARow.Parent;
end;

procedure TdxRichEditDocumentLayoutHelper.TdxParagraphFrameDrawingParentSearcher.VisitTable(ATable: TdxRichEditLayoutTable);
begin
  if FResult = nil then
    inherited VisitTable(ATable);
end;

procedure TdxRichEditDocumentLayoutHelper.TdxParagraphFrameDrawingParentSearcher.VisitTableCell(ATableCell: TdxRichEditLayoutTableCell);
begin
  if FResult = nil then
    inherited VisitTableCell(ATableCell);
end;

procedure TdxRichEditDocumentLayoutHelper.TdxParagraphFrameDrawingParentSearcher.VisitTableRow(ATableRow: TdxRichEditLayoutTableRow);
begin
  if FResult = nil then
    inherited VisitTableRow(ATableRow);
end;

procedure TdxRichEditDocumentLayoutHelper.TdxParagraphFrameDrawingParentSearcher.VisitTextBox(ATextBox: TdxRichEditLayoutTextBox);
begin
  if FResult = nil then
    inherited VisitTextBox(ATextBox);
end;

{ TdxRichEditDocumentLayoutHelper }

class function TdxRichEditDocumentLayoutHelper.CreateLayoutRowCollection(AModelRows: TdxRowCollection;
  const AParent: IdxRichEditLayoutElement; AIsTableCellRow: Boolean): TdxRichEditLayoutRowCollection;
begin
  Result := TdxRichEditLayoutRowCollection.Create;
  if AIsTableCellRow then
    InitializeTableCellRowCollection(AModelRows, Result, AParent)
  else
    InitializeRowCollection(AModelRows, Result, AParent);
end;

class procedure TdxRichEditDocumentLayoutHelper.InitializeRowCollection(AModelRows: TdxRowCollection;
  ALayoutRows: TdxRichEditLayoutRowCollection; const AParent: IdxRichEditLayoutElement);
var
  I: Integer;
begin
  for I := 0 to AModelRows.Count - 1 do
    if not AModelRows[I].IsTabelCellRow then
      ALayoutRows.Add(TdxRichEditLayoutRow.Create(AModelRows[I], AParent));
end;

class procedure TdxRichEditDocumentLayoutHelper.InitializeTableCellRowCollection(AModelRows: TdxRowCollection;
  ALayoutRows: TdxRichEditLayoutRowCollection; const AParent: IdxRichEditLayoutElement);
var
  ATable: TdxRichEditLayoutTable;
  I: Integer;
  ARow: TdxTableCellRow;
begin
  ATable := AParent.GetParentByType(TdxRichEditLayoutTable) as TdxRichEditLayoutTable;
  for I := 0 to AModelRows.Count - 1 do
  begin
    ARow := TdxTableCellRow(AModelRows[I]);
    if ARow.IsTabelCellRow and (ATable.ModelTable = ARow.CellViewInfo.TableViewInfo) then
      ALayoutRows.Add(TdxRichEditLayoutRow.Create(ARow, AParent));
  end;
end;

class function TdxRichEditDocumentLayoutHelper.CreateLayoutTableCollection(
  AModelTables: TdxTableViewInfoCollection; const AParent: IdxRichEditLayoutElement): TdxRichEditLayoutTableCollection;
var
  ALayoutTables: TdxRichEditLayoutTableCollection;
  I: Integer;
begin
  ALayoutTables := TdxRichEditLayoutTableCollection.Create;
  for I := 0 to AModelTables.Count - 1 do
  begin
    if AModelTables[I].ParentTableCellViewInfo = nil then
      ALayoutTables.Add(TdxRichEditLayoutTable.Create(AModelTables[I], AParent));
  end;
  Result := ALayoutTables;
end;

class function TdxRichEditDocumentLayoutHelper.CreateBorders(const AElement: IdxRichEditLayoutElement;
  ALeftBorder, ARightBorder, ATopBorder, ABottomBorder: TdxBorderInfo): TdxRichEditBorders;
var
  AContainer: IdxPieceTableContainer;
  ALeftBorderThickness, ARightBorderThickness, ATopBorderThickness, ABottomBorderThickness: Integer;
  ALeft, ARight, ATop, ABottom: TdxRichEditLayoutBorder;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  AContainer := AElement.GetParentByIntf(IdxPieceTableContainer) as IdxPieceTableContainer;

  AConverter := AContainer.PieceTable.DocumentModel.ToDocumentLayoutUnitConverter;
  ALeftBorderThickness := CalculateLayoutBorderThickness(ALeftBorder.Width, AConverter);
  ARightBorderThickness := CalculateLayoutBorderThickness(ARightBorder.Width, AConverter);
  ATopBorderThickness := CalculateLayoutBorderThickness(ATopBorder.Width, AConverter);
  ABottomBorderThickness := CalculateLayoutBorderThickness(ABottomBorder.Width, AConverter);

  ALeft := TdxRichEditLayoutBorder.Create(ALeftBorder.Color,
    TdxRichEditTableBorderLineStyle(ALeftBorder.Style), ALeftBorderThickness);
  ARight := TdxRichEditLayoutBorder.Create(ARightBorder.Color,
    TdxRichEditTableBorderLineStyle(ARightBorder.Style), ARightBorderThickness);
  ATop := TdxRichEditLayoutBorder.Create(ATopBorder.Color,
    TdxRichEditTableBorderLineStyle(ATopBorder.Style), ATopBorderThickness);
  ABottom := TdxRichEditLayoutBorder.Create(ABottomBorder.Color,
    TdxRichEditTableBorderLineStyle(ABottomBorder.Style), ABottomBorderThickness);

  Result := TdxRichEditBorders.Create(ALeft, ARight, ATop, ABottom);
end;

class function TdxRichEditDocumentLayoutHelper.CalculateLayoutBorderThickness(ABorderWidth: Integer;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter): Integer;
begin
  if ABorderWidth = 0 then
    Result := 0
  else
    Result := Max(1, AConverter.ToLayoutUnits(ABorderWidth));
end;

class function TdxRichEditDocumentLayoutHelper.GetRangeForTablesAndRowsContainer(
  ARows: TdxRichEditLayoutRowCollection; ATables: TdxRichEditLayoutTableCollection): TdxRichEditFixedRange;
var
  AFistRowRangeStart, ALastRowRangeEnd, AStart, AEnd: Integer;
  ALastRowRange: TdxRichEditFixedRange;
begin
  if ARows.Count <= 0 then
    Exit(TdxRichEditFixedRange.Create(ATables[0].Range.Start, ATables[0].Range.Length));
  AFistRowRangeStart := ARows[0].Range.Start;
  ALastRowRange := ARows.Last.Range;
  ALastRowRangeEnd := ALastRowRange.Start + ALastRowRange.Length;
  if ATables.Count > 0 then
    AStart := Min(AFistRowRangeStart, ATables[0].Range.Start)
  else
    AStart := AFistRowRangeStart;
  if ATables.Count > 0 then
    AEnd := Max(ALastRowRangeEnd, ATables.Last.Range.Start + ATables.Last.Range.Length)
  else
    AEnd := ALastRowRangeEnd;
  Result := TdxRichEditFixedRange.Create(AStart, AEnd - AStart);
end;

class function TdxRichEditDocumentLayoutHelper.GetRangeFromRowByCoordinates(ARow: TdxRichEditLayoutRow;
  AX1, AX2: Integer): TdxRichEditFixedRange;
var
  AStartBox, AEndBox, ABox: TdxRichEditBox;
  I: Integer;
begin
  AStartBox := nil;
  AEndBox := nil;
  for I := 0 to ARow.Boxes.Count - 1 do
  begin
    if (AStartBox <> nil) and (AEndBox <> nil) then
      Break;

    ABox := ARow.Boxes[I];
    if ABox.&Type = TdxRichEditLayoutType.FloatingObjectAnchorBox then
      Continue;
    if ABox.Bounds.X = AX1 then
      AStartBox := ABox;
    if ABox.Bounds.X + ABox.Bounds.Width = AX2 then
      AEndBox := ABox;
  end;

  Result := TdxRichEditFixedRange.Create(AStartBox.Range.Start,
    AEndBox.Range.Start + AEndBox.Range.Length - AStartBox.Range.Start);
end;

class function TdxRichEditDocumentLayoutHelper.CreateParagraphFrameBoxCollection(AModelColumn: TdxColumn;
  const AElement: IdxRichEditLayoutElement): TdxRichEditParagraphFrameBoxCollection;
var
  I: Integer;
  AParagraphFrame: TdxParagraphFrameBox;
  ASearcher: TdxParagraphFrameDrawingParentSearcher;
begin
  Result := TdxRichEditParagraphFrameBoxCollection.Create;
  if AModelColumn.InnerParagraphFrames = nil then
    Exit;
  for I := 0 to AModelColumn.InnerParagraphFrames.Count - 1 do
  begin
    AParagraphFrame := AModelColumn.InnerParagraphFrames[I];
    if not AParagraphFrame.GetParagraph.IsInCell then
      Result.Add(TdxRichEditParagraphFrameBox.Create(AParagraphFrame, AElement, AElement))
    else
    begin
      ASearcher := TdxParagraphFrameDrawingParentSearcher.Create(AParagraphFrame);
      try
        ASearcher.Visit(AElement);
        if ASearcher.Result <> nil then
          Result.Add(TdxRichEditParagraphFrameBox.Create(AParagraphFrame, ASearcher.Result, AElement));
      finally
        ASearcher.Free;
      end;
    end;
  end;
end;

{ TdxRichEditTextCollectorVisitor }

procedure TdxRichEditTextCollectorVisitor.VisitColumnBreakBox(AColumnBreakBox: TdxRichEditPlainTextBox);
begin
  Text := Text + AColumnBreakBox.Text;
  inherited VisitColumnBreakBox(AColumnBreakBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitHyphenBox(AHyphen: TdxRichEditPlainTextBox);
begin
  Text := Text + AHyphen.Text;
  inherited VisitHyphenBox(AHyphen);
end;

procedure TdxRichEditTextCollectorVisitor.VisitLineBreakBox(ALineBreakBox: TdxRichEditPlainTextBox);
begin
  Text := Text + ALineBreakBox.Text;
  inherited VisitLineBreakBox(ALineBreakBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitLineNumberBox(ALineNumberBox: TdxRichEditLineNumberBox);
begin
  Text := Text + ALineNumberBox.Text;
  inherited VisitLineNumberBox(ALineNumberBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitNumberingListMarkBox(ANumberingListMarkBox: TdxRichEditNumberingListMarkBox);
begin
  Text := Text + ANumberingListMarkBox.Text;
  inherited VisitNumberingListMarkBox(ANumberingListMarkBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitNumberingListWithSeparatorBox(
  ANumberingListWithSeparatorBox: TdxRichEditNumberingListWithSeparatorBox);
begin
  Text := Text + ANumberingListWithSeparatorBox.Text;
  inherited VisitNumberingListWithSeparatorBox(ANumberingListWithSeparatorBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitPageBreakBox(APageBreakBox: TdxRichEditPlainTextBox);
begin
  Text := Text + APageBreakBox.Text;
  inherited VisitPageBreakBox(APageBreakBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitParagraphMarkBox(AParagraphMarkBox: TdxRichEditPlainTextBox);
begin
  Text := Text + AParagraphMarkBox.Text;
  inherited VisitParagraphMarkBox(AParagraphMarkBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitPlainTextBox(APlainTextBox: TdxRichEditPlainTextBox);
begin
  Text := Text + APlainTextBox.Text;
  inherited VisitPlainTextBox(APlainTextBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitSectionBreakBox(ASectionBreakBox: TdxRichEditPlainTextBox);
begin
  Text := Text + ASectionBreakBox.Text;
  inherited VisitSectionBreakBox(ASectionBreakBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitSpaceBox(ASpaceBox: TdxRichEditPlainTextBox);
begin
  Text := Text + ASpaceBox.Text;
  inherited VisitSpaceBox(ASpaceBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitSpecialTextBox(ASpecialTextBox: TdxRichEditPlainTextBox);
begin
  Text := Text + ASpecialTextBox.Text;
  inherited VisitSpecialTextBox(ASpecialTextBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitTabSpaceBox(ATabSpaceBox: TdxRichEditPlainTextBox);
begin
  Text := Text + ATabSpaceBox.Text;
  inherited VisitTabSpaceBox(ATabSpaceBox);
end;

procedure TdxRichEditTextCollectorVisitor.VisitPageNumberBox(APageNumberBox: TdxRichEditPlainTextBox);
begin
  Text := Text + APageNumberBox.Text;
  inherited VisitPageNumberBox(APageNumberBox);
end;

end.
