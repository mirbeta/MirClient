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
unit dxRichEdit.Import.Doc.DCO;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocCharacterFormattingInfo,
  dxRichEdit.Import.Doc.DocPictureBulletInformation,
  dxRichEdit.Import.Doc.BorderDescriptor,
  dxRichEdit.Import.Doc.DocFieldsImportHelper,
  dxRichEdit.Import.Doc.DocumentProperties;

type
  TdxTableDefinitionOperand = class;
  TdxDocCommandFactoryBase = class;
  TLP = class;

  TdxTableCellWidthOperandList = class;
  TdxColumnWidthOperandList = class;
  TdxInsertOperandList = class;
  TdxCellSpacingOperandList = class;
  TdxTableBordersOverrideOperandList = class;
  TdxCellRangeVerticalAlignmentOperandList = class;
  TdxCellHideMarkOperandList = class;

  TdxFatl = class
  public const
    UseBorders           = SmallInt($0001);
    UseShading           = SmallInt($0002);
    UseFont              = SmallInt($0004);
    UseColor             = SmallInt($0008);
    UseBestFit           = SmallInt($0010);
    ApplyToHeaderRows    = SmallInt($0020);
    ApplyToLastRow       = SmallInt($0040);
    ApplyToHeaderColumns = SmallInt($0080);
    ApplyToLastColumn    = SmallInt($0100);
  end;

  TdxDocTableCellBorder = (
    Top,
    Left,
    Bottom,
    Right,
    TopLeftToBottomRight,
    TopRightToBottomLeft
  );
  TdxDocTableCellBorders = set of TdxDocTableCellBorder;

  TdxChangeActionType = (
    None,
    Character,
    Paragraph,
    Section,
    TableCell,
    TableRow,
    Table,
    Frame
  );
  TdxChangeActionTypes = set of TdxChangeActionType;

  { TdxSectionInfo }

  TdxSectionInfo = class
  strict private
    FColumnsInfo: TdxColumnsInfo;
    FMarginsInfo: TdxMarginsInfo;
    FPageInfo: TdxPageInfo;
    FGeneralSectionInfo: TdxGeneralSectionInfo;
    FPageNumberingInfo: TdxPageNumberingInfo;
    FLineNumberingInfo: TdxLineNumberingInfo;
    FFootNote: TdxSectionFootNote;
    FEndNote: TdxSectionFootNote;
  public
    destructor Destroy; override;
    class function CreateDefault(ADocumentModel: TdxDocumentModel): TdxSectionInfo; static;

    property SectionMargins: TdxMarginsInfo read FMarginsInfo;
    property SectionColumns: TdxColumnsInfo read FColumnsInfo;
    property SectionPage: TdxPageInfo read FPageInfo;
    property SectionGeneralSettings: TdxGeneralSectionInfo read FGeneralSectionInfo;
    property SectionPageNumbering: TdxPageNumberingInfo read FPageNumberingInfo;
    property SectionLineNumbering: TdxLineNumberingInfo read FLineNumberingInfo;
    property FootNote: TdxSectionFootNote read FFootNote;
    property EndNote: TdxSectionFootNote read FEndNote;
  end;

  { TdxParagraphInfo }

  TdxParagraphInfo = class
  strict private
    FFormattingInfo: TdxParagraphFormattingInfo;
    FFormattingOptions: TdxParagraphFormattingOptions;
    FTabInfo: TdxTabFormattingInfo;
    FParagraphStyleIndex: Integer;
    FListLevel: Byte;
    FListInfoIndex: Integer;
    FInTable: Boolean;
    FInnerTableCell: Boolean;
    FNestedTableTrailer: Boolean;
    FTableTrailer: Boolean;
    FTableDepth: Integer;
    procedure SetFormattingInfo(const AValue: TdxParagraphFormattingInfo);
    function GetTableDepth: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateDefault(ADocumentModel: TdxDocumentModel): TdxParagraphInfo; static;

    property FormattingInfo: TdxParagraphFormattingInfo read FFormattingInfo write SetFormattingInfo;
    property FormattingOptions: TdxParagraphFormattingOptions read FFormattingOptions write FFormattingOptions;
    property Tabs: TdxTabFormattingInfo read FTabInfo;
    property ParagraphStyleIndex: Integer read FParagraphStyleIndex write FParagraphStyleIndex;
    property ListLevel: Byte read FListLevel write FListLevel;
    property ListInfoIndex: Integer read FListInfoIndex write FListInfoIndex;
    property InnerTableCell: Boolean read FInnerTableCell write FInnerTableCell;
    property NestedTableTrailer: Boolean read FNestedTableTrailer write FNestedTableTrailer;
    property InTable: Boolean read FInTable write FInTable;
    property TableTrailer: Boolean read FTableTrailer write FTableTrailer;
    property TableDepth: Integer read GetTableDepth write FTableDepth;
  end;

  { TdxFrameInfo }

  TdxFrameInfo = class
  strict private
    FFormattingInfo: TdxParagraphFrameFormattingInfo;
    FFormattingOptions: TdxParagraphFrameFormattingOptions;
    procedure SetFormattingInfo(const AValue: TdxParagraphFrameFormattingInfo);
  public
    destructor Destroy; override;
    class function CreateDefault(ADocumentModel: TdxDocumentModel): TdxFrameInfo; static;

    property FormattingInfo: TdxParagraphFrameFormattingInfo read FFormattingInfo write SetFormattingInfo;
    property FormattingOptions: TdxParagraphFrameFormattingOptions read FFormattingOptions write FFormattingOptions;
  end;

  { TdxCharacterInfo }

  TdxCharacterInfo = class
  strict private
    FFormattingInfo: TdxDocCharacterFormattingInfo;
    FFormattingOptions: TdxCharacterFormattingOptions;
    FPictureBulletInformation: TdxDocPictureBulletInformation;
    FSpecial: Boolean;
    FBinaryData: Boolean;
    FEmbeddedObject: Boolean;
    FOle2Object: Boolean;
    FSymbol: Char;
    FSpecialCharactersFontFamilyNameIndex: SmallInt;
    procedure SetFormattingInfo(const AValue: TdxDocCharacterFormattingInfo);
  public
    destructor Destroy; override;
    class function CreateDefault(ADocumentModel: TdxDocumentModel): TdxCharacterInfo; static;

    property FormattingInfo: TdxDocCharacterFormattingInfo read FFormattingInfo write SetFormattingInfo;
    property FormattingOptions: TdxCharacterFormattingOptions read FFormattingOptions write FFormattingOptions;
    property SpecialCharactersFontFamilyNameIndex: SmallInt read FSpecialCharactersFontFamilyNameIndex write FSpecialCharactersFontFamilyNameIndex;
    property Special: Boolean read FSpecial write FSpecial;
    property BinaryData: Boolean read FBinaryData write FBinaryData;
    property EmbeddedObject: Boolean read FEmbeddedObject write FEmbeddedObject;
    property Ole2Object: Boolean read FOle2Object write FOle2Object;
    property Symbol: Char read FSymbol write FSymbol;
    property PictureBulletInformation: TdxDocPictureBulletInformation read FPictureBulletInformation;
  end;

  { TdxDocTableInfo }

  TdxDocTableInfo = class
  strict private
    FTableStyleIndex: Integer;
    FTableDefinition: TdxTableDefinitionOperand;
    FTableProperties: TdxTableProperties;
    procedure SetTableDefinition(const AValue: TdxTableDefinitionOperand);
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateDefault(ADocumentModel: TdxDocumentModel): TdxDocTableInfo; static;

    property TableStyleIndex: Integer read FTableStyleIndex write FTableStyleIndex;
    property TableProperties: TdxTableProperties read FTableProperties;
    property TableDefinition: TdxTableDefinitionOperand read FTableDefinition write SetTableDefinition;
  end;

  { TdxVerticalMergeInfo }

  TdxVerticalMergeInfo = class
  strict private
    FCellIndex: Byte;
    FMergingState: TdxMergingState;
  public
    constructor Create(ACellIndex: Byte; AMergingState: TdxMergingState);

    property CellIndex: Byte read FCellIndex write FCellIndex;
    property MergingState: TdxMergingState read FMergingState write FMergingState;
  end;

  { TdxVerticalMergeInfoList }

  TdxVerticalMergeInfoList = class(TdxObjectList<TdxVerticalMergeInfo>);

  { TdxHorizontalMergeInfo }

  TdxHorizontalMergeInfo = class
  strict private
    FFirstCellIndex: Byte;
    FLastCellIndex: Byte;
    FSplit: Boolean;
  public
    constructor Create(AFirstCellIndex: Byte; ALastCellIndex: Byte; ASplit: Boolean);

    property FirstCellIndex: Byte read FFirstCellIndex;
    property LastCellIndex: Byte read FLastCellIndex;
  end;

  { TdxTableCellInfo }

  TdxTableCellInfo = class
  public const
    MaxCellCount = 64;
  strict private
    FTableCellProperties: TdxTableCellProperties;
    FCellColors: TdxAlphaColorList;
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateDefault(ADocumentModel: TdxDocumentModel): TdxTableCellInfo; static;

    property TableCellProperties: TdxTableCellProperties read FTableCellProperties;
    property CellColors: TdxAlphaColorList read FCellColors;
  end;

  { TdxTableRowInfo }

  TdxTableRowInfo = class
  strict private
    FWidthBeforeSetted: Boolean;
    FRowLeft: Integer;
    FRowLeftSetted: Boolean;
    FRowLeftOffset: Integer;
    FRowLeftOffsetSetted: Boolean;
    FHorizontalMerging: TdxObjectList<TdxHorizontalMergeInfo>;
    FVerticalMerging: TdxVerticalMergeInfoList;
    FPreferredCellWidths: TdxTableCellWidthOperandList;

    FColumnWidthActions: TdxColumnWidthOperandList;
    FInsertActions: TdxInsertOperandList;
    FCellMarginActions: TdxCellSpacingOperandList;
    FOverrideCellBordersActions: TdxTableBordersOverrideOperandList;
    FCellRangeVerticalAlignmentActions: TdxCellRangeVerticalAlignmentOperandList;
    FCellHideMarkActions: TdxCellHideMarkOperandList;

    FTopBorders: TdxDocTableBorderColorReferenceList;
    FLeftBorders: TdxDocTableBorderColorReferenceList;
    FRightBorders: TdxDocTableBorderColorReferenceList;
    FBottomBorders: TdxDocTableBorderColorReferenceList;
    FCellShading1: TdxAlphaColorList;
    FCellShading2: TdxAlphaColorList;
    FCellShading3: TdxAlphaColorList;
    FDefaultCellsShading: TdxAlphaColorList;
    FTableRowProperties: TdxTableRowProperties;
    FTableAutoformatLookSpecifier: TLP;
    procedure SetTableAutoformatLookSpecifier(const Value: TLP);
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateDefault(ADocumentModel: TdxDocumentModel): TdxTableRowInfo; static;

    property HorizontalMerging: TdxObjectList<TdxHorizontalMergeInfo> read FHorizontalMerging;
    property VerticalMerging: TdxVerticalMergeInfoList read FVerticalMerging;
    property PreferredCellWidths: TdxTableCellWidthOperandList read FPreferredCellWidths;
    property ColumnWidthActions: TdxColumnWidthOperandList read FColumnWidthActions;
    property InsertActions: TdxInsertOperandList read FInsertActions;
    property CellMarginsActions: TdxCellSpacingOperandList read FCellMarginActions;
    property OverrideCellBordersActions: TdxTableBordersOverrideOperandList read FOverrideCellBordersActions;
    property CellRangeVerticalAlignmentActions: TdxCellRangeVerticalAlignmentOperandList read FCellRangeVerticalAlignmentActions;
    property CellHideMarkActions: TdxCellHideMarkOperandList read FCellHideMarkActions;
    property TableRowProperties: TdxTableRowProperties read FTableRowProperties;
    property TopBorders: TdxDocTableBorderColorReferenceList read FTopBorders;
    property LeftBorders: TdxDocTableBorderColorReferenceList read FLeftBorders;
    property RightBorders: TdxDocTableBorderColorReferenceList read FRightBorders;
    property BottomBorders: TdxDocTableBorderColorReferenceList read FBottomBorders;
    property CellShading1: TdxAlphaColorList read FCellShading1;
    property CellShading2: TdxAlphaColorList read FCellShading2;
    property CellShading3: TdxAlphaColorList read FCellShading3;
    property DefaultCellsShading: TdxAlphaColorList read FDefaultCellsShading;
    property WidthBeforeSetted: Boolean read FWidthBeforeSetted write FWidthBeforeSetted;
    property RowLeft: Integer read FRowLeft write FRowLeft;
    property RowLeftSetted: Boolean read FRowLeftSetted write FRowLeftSetted;
    property RowLeftOffset: Integer read FRowLeftOffset write FRowLeftOffset;
    property RowLeftOffsetSetted: Boolean read FRowLeftOffsetSetted write FRowLeftOffsetSetted;
    property TableAutoformatLookSpecifier: TLP read FTableAutoformatLookSpecifier write SetTableAutoformatLookSpecifier;
  end;

  { TdxDocPropertyContainer }

  TdxDocPropertyContainer = class
  strict private
    FFactory: TdxDocCommandFactoryBase;
    FUnitConverter: TdxDocumentModelUnitConverter;
    FIsDeleted: Boolean;
    FFontFamilyNameIndex: SmallInt;
    FDataStreamOffset: Integer;
    FCharacterInfo: TdxCharacterInfo;
    FParagraphInfo: TdxParagraphInfo;
    FFrameInfo: TdxFrameInfo;
    FSectionInfo: TdxSectionInfo;
    FTableInfo: TdxDocTableInfo;
    FTableRowInfo: TdxTableRowInfo;
    FTableCellInfo: TdxTableCellInfo;
    FFieldProperties: Byte;
  public
    constructor Create(AFactory: TdxDocCommandFactoryBase; AUnitConverter: TdxDocumentModelUnitConverter);
    destructor Destroy; override;
    procedure Update(AChangeType: TdxChangeActionTypes);

    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter write FUnitConverter;
    property IsDeleted: Boolean read FIsDeleted write FIsDeleted;
    property FontFamilyNameIndex: SmallInt read FFontFamilyNameIndex write FFontFamilyNameIndex;
    property DataStreamOffset: Integer read FDataStreamOffset write FDataStreamOffset;
    property CharacterInfo: TdxCharacterInfo read FCharacterInfo write FCharacterInfo;
    property ParagraphInfo: TdxParagraphInfo read FParagraphInfo write FParagraphInfo;
    property FrameInfo: TdxFrameInfo read FFrameInfo write FFrameInfo;
    property SectionInfo: TdxSectionInfo read FSectionInfo write FSectionInfo;
    property TableInfo: TdxDocTableInfo read FTableInfo write FTableInfo;
    property TableRowInfo: TdxTableRowInfo read FTableRowInfo write FTableRowInfo;
    property TableCellInfo: TdxTableCellInfo read FTableCellInfo write FTableCellInfo;
    property FieldProperties: Byte read FFieldProperties write FFieldProperties;

    property Factory: TdxDocCommandFactoryBase read FFactory write FFactory;
  end;

  { TdxDocCommand }

  IdxDocCommand = interface
    function GetChangeAction: TdxChangeActionType;
    procedure Read(const ASprm: TBytes);
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
    procedure Write(AWriter: TBinaryWriter);

    property ChangeAction: TdxChangeActionType read GetChangeAction;
  end;

  TdxDocCommand = class(TInterfacedObject, IdxDocCommand)
  protected
    function GetChangeAction: TdxChangeActionType; virtual; abstract;
  public
    constructor Create; overload; virtual;
    procedure Read(const ASprm: TBytes); virtual; abstract;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); virtual; abstract;
    procedure Write(AWriter: TBinaryWriter);  virtual; abstract;

    property ChangeAction: TdxChangeActionType read GetChangeAction;
  end;

  TdxDocCommandClass = class of TdxDocCommand;

  { TdxWidthUnitOperand }

  TdxWidthUnitOperand = class
  strict private
    FType: TdxWidthUnitType;
    FValue: SmallInt;
  protected
    procedure Read(const AData: TBytes; AStartIndex: Integer);
  public
    class function FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxWidthUnitOperand; static;
    class function NonNegativeFromByteArray(const AData: TBytes; AStartIndex: Integer): TdxWidthUnitOperand; static;
    procedure ConvertFromWidthUnitInfo(AInfo: TdxWidthUnitInfo; AUnitConverter: TdxDocumentModelUnitConverter);
    function GetBytes: TBytes;
    function Clone: TdxWidthUnitOperand;

    property &Type: TdxWidthUnitType read FType write FType;
    property Value: SmallInt read FValue write FValue;
  end;

  { TdxTableCellWidthOperand }

  TdxTableCellWidthOperand = class
  strict private
    FStartIndex: Byte;
    FEndIndex: Byte;
    FWidthUnit: TdxWidthUnitOperand;
  private
    procedure SetWidthUnit(const Value: TdxWidthUnitOperand);
  protected
    procedure Read(const AData: TBytes);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromByteArray(const AData: TBytes): TdxTableCellWidthOperand; static;
    function GetBytes: TBytes;
    function Clone: TdxTableCellWidthOperand;

    property StartIndex: Byte read FStartIndex write FStartIndex;
    property EndIndex: Byte read FEndIndex write FEndIndex;
    property WidthUnit: TdxWidthUnitOperand read FWidthUnit write SetWidthUnit;
  end;
  TdxTableCellWidthOperandList = class(TdxObjectList<TdxTableCellWidthOperand>);

  { TdxColumnWidthOperand }

  TdxColumnWidthOperand = class
  strict private
    FStartIndex: Byte;
    FEndIndex: Byte;
    FWidthInTwips: SmallInt;
  protected
    procedure Read(const AData: TBytes);
  public
    class function FromByteArray(const AData: TBytes): TdxColumnWidthOperand; static;
    function GetBytes: TBytes;
    function Clone: TdxColumnWidthOperand;

    property StartIndex: Byte read FStartIndex write FStartIndex;
    property EndIndex: Byte read FEndIndex write FEndIndex;
    property WidthInTwips: SmallInt read FWidthInTwips write FWidthInTwips;
  end;
  TdxColumnWidthOperandList = class(TdxObjectList<TdxColumnWidthOperand>);

  { TdxInsertOperand }

  TdxInsertOperand = class
  strict private
    FStartIndex: Byte;
    FCount: Byte;
    FWidthInTwips: SmallInt;
  protected
    procedure Read(const AData: TBytes);
  public
    class function FromByteArray(const AData: TBytes): TdxInsertOperand; static;
    function GetBytes: TBytes;
    function Clone: TdxInsertOperand;

    property StartIndex: Byte read FStartIndex write FStartIndex;
    property Count: Byte read FCount write FCount;
    property WidthInTwips: SmallInt read FWidthInTwips write FWidthInTwips;
  end;
  TdxInsertOperandList = class(TdxObjectList<TdxInsertOperand>);

  { TdxTabDescriptor }

  TdxTabDescriptor = class
  strict private class var
    FTabAlignments: TDictionary<Byte, TdxTabAlignmentType>;
    FTabLeaders: TDictionary<Byte, TdxTabLeaderType>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FTabAlignment: TdxTabAlignmentType;
    FTabLeader: TdxTabLeaderType;
  public
    constructor Create(AAlignment: TdxTabAlignmentType; ALeader: TdxTabLeaderType); overload;
    constructor Create(ATbd: Byte); overload;
    procedure Write(AWriter: TBinaryWriter);
    function CalcTabAlignmentCode: Byte;
    function CalcTabLeaderCode: Byte;

    property Alignment: TdxTabAlignmentType read FTabAlignment;
    property Leader: TdxTabLeaderType read FTabLeader;
  end;

  { TdxTabsOperandBase }

  TdxTabsOperandBase = class abstract
  public const
    PositionSize = Integer(2);
  strict private
    FDeletedTabsCount: Byte;
    FAddedTabsCount: Byte;
    FDeletedTabsPositions: TdxIntegerList;
    FAddedTabsPositions: TdxIntegerList;
    FAddedTabs: TList<TdxTabDescriptor>;
  protected
    procedure Read(const ATabs: TBytes);
    function ReadDeletedTabs(const ATabs: TBytes; ACurrentPosition: Integer): Integer; virtual;
    procedure ReadAddedTabs(const ATabs: TBytes; ACurrentPosition: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write(AWriter: TBinaryWriter);
    procedure ConvertFromFormattingTabInfo(AInfo: TdxTabFormattingInfo; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure AddTabs(AFormattingInfo: TdxTabFormattingInfo; AUnitConverter: TdxDocumentModelUnitConverter);

    property DeletedTabsCount: Byte read FDeletedTabsCount;
    property AddedTabsCount: Byte read FAddedTabsCount;
    property DeletedTabsPositions: TdxIntegerList read FDeletedTabsPositions;
    property AddedTabsPositions: TdxIntegerList read FAddedTabsPositions;
    property AddedTabs: TList<TdxTabDescriptor> read FAddedTabs;
  end;

  { TdxTabsOperand }

  TdxTabsOperand = class(TdxTabsOperandBase)
  public
    class function FromByteArray(const ATabs: TBytes): TdxTabsOperand; static;
  end;

  { TdxTabsOperandClose }

  TdxTabsOperandClose = class(TdxTabsOperandBase)
  strict private
    FTolerances: TdxIntegerList;
  protected
    function ReadDeletedTabs(const ATabs: TBytes; ACurrentPosition: Integer): Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromByteArray(const ATabs: TBytes): TdxTabsOperandClose; static;

    property Tolerances: TdxIntegerList read FTolerances;
  end;

  { TdxTableCellDescriptor }

  TdxTableCellDescriptor = class
  public const
    Size = Integer(20);
  strict private
    FFitText: Boolean;
    FHideCellMark: Boolean;
    FHorizontalMerging: TdxMergingState;
    FNoWrap: Boolean;
    FPreferredWidth: SmallInt;
    FTextDirection: TdxTextDirection;
    FType: TdxWidthUnitType;
    FVerticalAlignment: TdxVerticalAlignment;
    FVerticalMerging: TdxMergingState;
    FBottomBorder: TdxBorderDescriptor97;
    FLeftBorder: TdxBorderDescriptor97;
    FRightBorder: TdxBorderDescriptor97;
    FTopBorder: TdxBorderDescriptor97;
  protected
    procedure Read(const AData: TBytes; AStartIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxTableCellDescriptor; static;
    function Clone: TdxTableCellDescriptor;

    property FitText: Boolean read FFitText;
    property HideCellMark: Boolean read FHideCellMark;
    property HorizontalMerging: TdxMergingState read FHorizontalMerging;
    property NoWrap: Boolean read FNoWrap;
    property PreferredWidth: SmallInt read FPreferredWidth;
    property TextDirection: TdxTextDirection read FTextDirection;
    property &Type: TdxWidthUnitType read FType;
    property VerticalAlignment: TdxVerticalAlignment read FVerticalAlignment;
    property VerticalMerging: TdxMergingState read FVerticalMerging;
    property BottomBorder: TdxBorderDescriptor97 read FBottomBorder;
    property LeftBorder: TdxBorderDescriptor97 read FLeftBorder;
    property RightBorder: TdxBorderDescriptor97 read FRightBorder;
    property TopBorder: TdxBorderDescriptor97 read FTopBorder;
  end;
  TdxTableCellDescriptorList = class(TdxObjectList<TdxTableCellDescriptor>);

  { TdxTableBordersOperand80 }

  TdxTableBordersOperand80 = class
  strict private
    FTopBorder: TdxBorderDescriptor97;
    FLeftBorder: TdxBorderDescriptor97;
    FBottomBorder: TdxBorderDescriptor97;
    FRightBorder: TdxBorderDescriptor97;
    FInsideHorizontalBorder: TdxBorderDescriptor97;
    FInsideVerticalBorder: TdxBorderDescriptor97;
  protected
    procedure Read(const AData: TBytes);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromByteArray(const AData: TBytes): TdxTableBordersOperand80; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure ApplyProperties(ADestination: TdxTableBorders; AUnitConverter: TdxDocumentModelUnitConverter);

    property TopBorder: TdxBorderDescriptor97 read FTopBorder;
    property LeftBorder: TdxBorderDescriptor97 read FLeftBorder;
    property BottomBorder: TdxBorderDescriptor97 read FBottomBorder;
    property RightBorder: TdxBorderDescriptor97 read FRightBorder;
    property InsideHorizontalBorder: TdxBorderDescriptor97 read FInsideHorizontalBorder;
    property InsideVerticalBorder: TdxBorderDescriptor97 read FInsideVerticalBorder;
  end;

  { TdxTableBordersOperand }

  TdxTableBordersOperand = class
  strict private
    FTopBorder: TdxBorderDescriptor;
    FLeftBorder: TdxBorderDescriptor;
    FBottomBorder: TdxBorderDescriptor;
    FRightBorder: TdxBorderDescriptor;
    FInsideHorizontalBorder: TdxBorderDescriptor;
    FInsideVerticalBorder: TdxBorderDescriptor;
  protected
    procedure Read(const AData: TBytes);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromByteArray(const AData: TBytes): TdxTableBordersOperand; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure ApplyProperties(ADestination: TdxTableBorders; AUnitConverter: TdxDocumentModelUnitConverter);

    property TopBorder: TdxBorderDescriptor read FTopBorder;
    property LeftBorder: TdxBorderDescriptor read FLeftBorder;
    property BottomBorder: TdxBorderDescriptor read FBottomBorder;
    property RightBorder: TdxBorderDescriptor read FRightBorder;
    property InsideHorizontalBorder: TdxBorderDescriptor read FInsideHorizontalBorder;
    property InsideVerticalBorder: TdxBorderDescriptor read FInsideVerticalBorder;
  end;

  { TdxTableBordersOverrideOperand }

  TdxTableBordersOverrideOperand = class
  public const
    Size = Byte($0b);
  strict private
    FStartIndex: Byte;
    FEndIndex: Byte;
    FCellBorders: TdxDocTableCellBorders;
    FBorder: TdxBorderDescriptor;
  protected
    procedure Read(const AData: TBytes);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromByteArray(const AData: TBytes): TdxTableBordersOverrideOperand; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure ApplyProperties(ADestination: TdxTableCellBorders; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure ApplyPropertiesCore(ABorder: TdxBorderBase; AUnitConverter: TdxDocumentModelUnitConverter);
    function Clone: TdxTableBordersOverrideOperand;

    property StartIndex: Byte read FStartIndex write FStartIndex;
    property EndIndex: Byte read FEndIndex write FEndIndex;
    property CellBorders: TdxDocTableCellBorders read FCellBorders write FCellBorders;
    property Border: TdxBorderDescriptor read FBorder;
  end;
  TdxTableBordersOverrideOperandList = class(TdxObjectList<TdxTableBordersOverrideOperand>);

  { TdxCellSpacingOperand }

  TdxCellSpacingOperand = class
  public const
    Size = Byte($06);
  strict private
    FStartIndex: Byte;
    FEndIndex: Byte;
    FCellBorders: TdxDocTableCellBorders;
    FWidthUnit: TdxWidthUnitOperand;
  protected
    procedure Read(const AData: TBytes);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromByteArray(const AData: TBytes): TdxCellSpacingOperand; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure ApplyProperties(ADestination: TdxCellMargins; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure ApplyPropertiesCore(AMargin: TdxMarginUnitBase);
    function Clone: TdxCellSpacingOperand;

    property StartIndex: Byte read FStartIndex write FStartIndex;
    property EndIndex: Byte read FEndIndex write FEndIndex;
    property CellBorders: TdxDocTableCellBorders read FCellBorders write FCellBorders;
    property WidthUnit: TdxWidthUnitOperand read FWidthUnit;
  end;
  TdxCellSpacingOperandList = class(TdxObjectList<TdxCellSpacingOperand>);

  { TdxCellRangeVerticalAlignmentOperand }

  TdxCellRangeVerticalAlignmentOperand = class
  public const
    Size = Byte($03);
  strict private
    FStartIndex: Byte;
    FEndIndex: Byte;
    FVerticalAlignment: TdxVerticalAlignment;
  protected
    procedure Read(const AData: TBytes);
  public
    class function FromByteArray(const AData: TBytes): TdxCellRangeVerticalAlignmentOperand; static;
    procedure Write(AWriter: TBinaryWriter);
    function Clone: TdxCellRangeVerticalAlignmentOperand;

    property StartIndex: Byte read FStartIndex write FStartIndex;
    property EndIndex: Byte read FEndIndex write FEndIndex;
    property VerticalAlignment: TdxVerticalAlignment read FVerticalAlignment write FVerticalAlignment;
  end;
  TdxCellRangeVerticalAlignmentOperandList = class(TdxObjectList<TdxCellRangeVerticalAlignmentOperand>);

  { TdxDocCommandParagraphBorder97 }

  TdxDocCommandParagraphBorder97 = class abstract(TdxDocCommand)
  strict private
    FCurrentBorder: TdxBorderDescriptor97;
  protected
    procedure ApplyBorderProperties(ABorder: TdxBorderInfo; AUnitConverter: TdxDocumentModelUnitConverter);
    function GetChangeAction: TdxChangeActionType; override;
  public
    destructor Destroy; override;
    procedure Read(const AData: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;

    property CurrentBorder: TdxBorderDescriptor97 read FCurrentBorder;
    property ChangeAction: TdxChangeActionType read GetChangeAction;
  end;

  { TdxDocCommandParagraphTopBorder }

  TdxDocCommandParagraphTopBorder = class(TdxDocCommandParagraphBorder97)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandParagraphLeftBorder }

  TdxDocCommandParagraphLeftBorder = class(TdxDocCommandParagraphBorder97)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandParagraphBottomBorder }

  TdxDocCommandParagraphBottomBorder = class(TdxDocCommandParagraphBorder97)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandParagraphRightBorder }

  TdxDocCommandParagraphRightBorder = class(TdxDocCommandParagraphBorder97)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandParagraphBorder }

  TdxDocCommandParagraphBorder = class abstract(TdxDocCommand)
  public const
    ParagraphBorderOperandSize = Byte($08);
  strict private
    FCurrentBorder: TdxBorderDescriptor;
  protected
    procedure ApplyBorderProperties(ABorder: TdxBorderInfo; AUnitConverter: TdxDocumentModelUnitConverter);
    function GetChangeAction: TdxChangeActionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const AData: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;

    property CurrentBorder: TdxBorderDescriptor read FCurrentBorder;
    property ChangeAction: TdxChangeActionType read GetChangeAction;
  end;

  { TdxDocCommandParagraphTopBorderNew }

  TdxDocCommandParagraphTopBorderNew = class(TdxDocCommandParagraphBorder)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandParagraphLeftBorderNew }

  TdxDocCommandParagraphLeftBorderNew = class(TdxDocCommandParagraphBorder)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandParagraphBottomBorderNew }

  TdxDocCommandParagraphBottomBorderNew = class(TdxDocCommandParagraphBorder)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandParagraphRightBorderNew }

  TdxDocCommandParagraphRightBorderNew = class(TdxDocCommandParagraphBorder)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxTableDefinitionOperand }

  TdxTableDefinitionOperand = class
  strict private
    FColumnsCount: Byte;
    FPositions: TList<SmallInt>;
    FCells: TdxTableCellDescriptorList;
  protected
    procedure Read(const AData: TBytes);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromByteArray(const AData: TBytes): TdxTableDefinitionOperand; static;
    function Clone: TdxTableDefinitionOperand;

    property ColumnsCount: Byte read FColumnsCount write FColumnsCount;
    property Positions: TList<SmallInt> read FPositions;
    property Cells: TdxTableCellDescriptorList read FCells;
  end;

  { TLP }

  TLP = class
  public const
    Size = Byte($03);
  strict private class var
    FKnownStyles: TDictionary<SmallInt, string>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FItl: SmallInt;
    FGrfatl: SmallInt;
    class function GetKnownStyles: TDictionary<SmallInt, string>; static;
  protected
    procedure Read(const AData: TBytes);
  public
    constructor Create;
    function Clone: TLP;
    class function FromByteArray(const AData: TBytes): TLP; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure ApplyProperties(AProperties: TdxTableProperties);
    function GetTableStyle(ADocumentModel: TdxDocumentModel): TdxTableStyle;
    procedure ApplyPropertiesCore(AStyle: TdxTableStyle; AProperties: TdxTableProperties);

    property TableStyleOptions: SmallInt read FGrfatl write FGrfatl;
  end;

  { TdxCellHideMarkOperand }

  TdxCellHideMarkOperand = class
  public const
    Size = Byte($03);
  strict private
    FStartIndex: Byte;
    FEndIndex: Byte;
    FHideCellMark: Boolean;
  protected
    procedure Read(const AData: TBytes);
  public
    class function FromByteArray(const AData: TBytes): TdxCellHideMarkOperand; static;
    procedure Write(AWriter: TBinaryWriter);
    function Clone: TdxCellHideMarkOperand;

    property StartIndex: Byte read FStartIndex write FStartIndex;
    property EndIndex: Byte read FEndIndex write FEndIndex;
    property HideCellMark: Boolean read FHideCellMark write FHideCellMark;
  end;
  TdxCellHideMarkOperandList = class(TdxObjectList<TdxCellHideMarkOperand>);

  { TdxDocCommandFactoryBase }

  TdxDocCommandFactoryBase = class
  strict private
    FDocumentProperties: TdxDocDocumentProperties;
    FModel: TdxDocumentModel;
    FVersion: Integer;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
  protected
    property Model: TdxDocumentModel read FModel;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
  public
    constructor Create(AModel: TdxDocumentModel);
    class function GetOpcodeByType(ACommandType: TClass): SmallInt; virtual;
    function CreateCommand(AOpcode: SmallInt): IdxDocCommand; virtual; abstract;
    function CreatePropertyContainer(AChangeType: TdxChangeActionTypes): TdxDocPropertyContainer;
    procedure UpdatePropertyContainer(AContainer: TdxDocPropertyContainer; AChangeType: TdxChangeActionTypes); virtual; abstract;

    property DocumentProperties: TdxDocDocumentProperties read FDocumentProperties write FDocumentProperties;
    property Version: Integer read FVersion write FVersion;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.DocumentModel.Cache;

type

  { TdxKnownStyleNames }

  TdxKnownStyleNames = class
  public const
    AnnotationText = 'annotation text';
    AnnotationReference = 'annotation reference';
    AnnotationSubject = 'annotation subject';
    ArticleSection = 'Article / Section';
    BalloonText = 'Balloon Text';
    BlockText = 'Block Text';
    BodyText = 'Body Text';
    BodyText2 = 'Body Text 2';
    BodyText3 = 'Body Text 3';
    BodyTextFirstIndent = 'Body Text First Indent';
    BodyTextFirstIndent2 = 'Body Text First Indent 2';
    BodyTextIndent = 'Body Text Indent';
    BodyTextIndent2 = 'Body Text Indent 2';
    BodyTextIndent3 = 'Body Text Indent 3';
    Caption = 'Caption';
    Closing = 'Closing';
    CommentReference = 'Comment Reference';
    CommentSubject = 'Comment Subject';
    CommentText = 'Comment Text';
    Date = 'Date';
    DocumentMap = 'Document Map';
    EmailSignature = 'E-mail Signature';
    EndnoteReference = 'Endnote Reference';
    EndnoteText = 'Endnote Text';
    EnvelopeAddress = 'Envelope Address';
    EnvelopeReturn = 'Envelope Return';
    Footer = 'Footer';
    FootnoteText = 'Footnote Text';
    FootnoteReference = 'Footnote Reference';
    Header = 'Header';
    Heading1 = 'Heading 1';
    Heading2 = 'Heading 2';
    Heading3 = 'Heading 3';
    Heading4 = 'Heading 4';
    Heading5 = 'Heading 5';
    Heading6 = 'Heading 6';
    Heading7 = 'Heading 7';
    Heading8 = 'Heading 8';
    Heading9 = 'Heading 9';
    HTMLAcronym = 'HTML Acronym';
    HTMLAddress = 'HTML Address';
    HTMLBottomOfForm = 'HTML Bottom of Form';
    HTMLCite = 'HTML Cite';
    HTMLCode = 'HTML Code';
    HTMLDefinition = 'HTML Definition';
    HTMLKeyboard = 'HTML Keyboard';
    HTMLPreformatted = 'HTML Preformatted';
    HTMLSample = 'HTML Sample';
    HTMLTopOfForm = 'HTML Top of Form';
    HTMLTypewriter = 'HTML Typewriter';
    HTMLVariable = 'HTML Variable';
    HyperlinkFollowed = 'HyperlinkFollowed';
    HyperlinkStrongEmphasis = 'HyperlinkStrongEmphasis';
    Emphasis = 'Emphasis';
    FollowedHyperlink = 'FollowedHyperlink';
    Index1 = 'Index 1';
    Index2 = 'Index 2';
    Index3 = 'Index 3';
    Index4 = 'Index 4';
    Index5 = 'Index 5';
    Index6 = 'Index 6';
    Index7 = 'Index 7';
    Index8 = 'Index 8';
    Index9 = 'Index 9';
    IndexHeading = 'Index Heading';
    List = 'List';
    List2 = 'List 2';
    List3 = 'List 3';
    List4 = 'List 4';
    List5 = 'List 5';
    ListBullet = 'List Bullet';
    ListBullet2 = 'List Bullet 2';
    ListBullet3 = 'List Bullet 3';
    ListBullet4 = 'List Bullet 4';
    ListBullet5 = 'List Bullet 5';
    ListContinue = 'List Continue';
    ListContinue2 = 'List Continue 2';
    ListContinue3 = 'List Continue 3';
    ListContinue4 = 'List Continue 4';
    ListContinue5 = 'List Continue 5';
    ListNumber = 'List Number';
    ListNumber2 = 'List Number 2';
    ListNumber3 = 'List Number 3';
    ListNumber4 = 'List Number 4';
    ListNumber5 = 'List Number 5';
    MacroText = 'Macro Text';
    MacroToAHeading = 'macro to a heading';
    MessageHeader = 'Message Header';
    NoList = 'No List';
    NormalIndent = 'Normal Indent';
    NormalWeb = 'Normal (Web)';
    NoteHeading = 'Note Heading';
    NormalTable = 'Normal Table';
    OutlineList1 = 'Outline List 1';
    OutlineList2 = 'Outline List 2';
    OutlineList3 = 'Outline List 3';
    PageNumber = 'Page Number';
    PlainText = 'Plain Text';
    Salutation = 'Salutation';
    Signature = 'Signature';
    Strong = 'Strong';
    Subtitle = 'Subtitle';
    TableOfAuthorities = 'Table of Authorities';
    TableOfFigures = 'Table of Figures';
    Table3DEffects1 = 'Table 3D effects 1';
    Table3DEffects2 = 'Table 3D effects 2';
    Table3DEffects3 = 'Table 3D effects 3';
    TableClassic1 = 'Table Classic 1';
    TableClassic2 = 'Table Classic 2';
    TableClassic3 = 'Table Classic 3';
    TableClassic4 = 'Table Classic 4';
    TableColorful1 = 'Table Colorful 1';
    TableColorful2 = 'Table Colorful 2';
    TableColorful3 = 'Table Colorful 3';
    TableColumns1 = 'Table Columns 1';
    TableColumns2 = 'Table Columns 2';
    TableColumns3 = 'Table Columns 3';
    TableColumns4 = 'Table Columns 4';
    TableColumns5 = 'Table Columns 5';
    TableContemporary = 'Table Contemporary';
    TableElegant = 'Table Elegant';
    TableGrid = 'Table Grid';
    TableGrid1 = 'Table Grid 1';
    TableGrid2 = 'Table Grid 2';
    TableGrid3 = 'Table Grid 3';
    TableGrid4 = 'Table Grid 4';
    TableGrid5 = 'Table Grid 5';
    TableGrid6 = 'Table Grid 6';
    TableGrid7 = 'Table Grid 7';
    TableGrid8 = 'Table Grid 8';
    TableList1 = 'Table List 1';
    TableList2 = 'Table List 2';
    TableList3 = 'Table List 3';
    TableList4 = 'Table List 4';
    TableList5 = 'Table List 5';
    TableList6 = 'Table List 6';
    TableList7 = 'Table List 7';
    TableList8 = 'Table List 8';
    TableNormal = 'Table Normal';
    TableProfessional = 'Table Professional';
    TableSimple1 = 'Table Simple 1';
    TableSimple2 = 'Table Simple 2';
    TableSimple3 = 'Table Simple 3';
    TableSubtle1 = 'Table Subtle 1';
    TableSubtle2 = 'Table Subtle 2';
    TableTheme = 'Table Theme';
    TableWeb1 = 'Table Web 1';
    TableWeb2 = 'Table Web 2';
    TableWeb3 = 'Table Web 3';
    Title = 'Title';
    TOAHeading = 'TOA Heading';
    TOC1 = 'TOC 1';
    TOC2 = 'TOC 2';
    TOC3 = 'TOC 3';
    TOC4 = 'TOC 4';
    TOC5 = 'TOC 5';
    TOC6 = 'TOC 6';
    TOC7 = 'TOC 7';
    TOC8 = 'TOC 8';
    TOC9 = 'TOC 9';
  end;

{ TdxDocCommand }

constructor TdxDocCommand.Create;
begin
end;

{ TdxSectionInfo }

destructor TdxSectionInfo.Destroy;
begin
  FColumnsInfo.Free;
  FMarginsInfo.Free;
  FPageInfo.Free;
  FGeneralSectionInfo.Free;
  FPageNumberingInfo.Free;
  FLineNumberingInfo.Free;
  FFootNote.Free;
  FEndNote.Free;
  inherited Destroy;
end;

class function TdxSectionInfo.CreateDefault(ADocumentModel: TdxDocumentModel): TdxSectionInfo;
var
  ACache: TdxDocumentCache;
begin
  ACache := ADocumentModel.Cache;
  Result := TdxSectionInfo.Create;
  Result.FMarginsInfo := ACache.MarginsInfoCache.DefaultItem.Clone;
  Result.FColumnsInfo := ACache.ColumnsInfoCache.DefaultItem.Clone;
  Result.FPageInfo := ACache.PageInfoCache.DefaultItem.Clone;
  Result.FGeneralSectionInfo := ACache.GeneralSectionInfoCache.DefaultItem.Clone;
  Result.FGeneralSectionInfo.VerticalTextAlignment := TdxVerticalAlignment.Top;
  Result.FPageNumberingInfo := ACache.PageNumberingInfoCache.DefaultItem.Clone;
  Result.FLineNumberingInfo := ACache.LineNumberingInfoCache.DefaultItem.Clone;
  Result.FFootNote := TdxSectionFootNote.Create(ADocumentModel);
  Result.FFootNote.CopyFrom(ACache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultFootNoteItemIndex]);
  Result.FEndNote := TdxSectionFootNote.Create(ADocumentModel);
  Result.FEndNote.CopyFrom(ACache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultEndNoteItemIndex]);
end;

{ TdxParagraphInfo }

constructor TdxParagraphInfo.Create;
begin
  FListInfoIndex := -1;
end;

destructor TdxParagraphInfo.Destroy;
begin
  FFormattingInfo.Free;
  FTabInfo.Free;
  inherited Destroy;
end;

class function TdxParagraphInfo.CreateDefault(ADocumentModel: TdxDocumentModel): TdxParagraphInfo;
begin
  Result := TdxParagraphInfo.Create;
  Result.FFormattingInfo := ADocumentModel.Cache.ParagraphFormattingInfoCache.DefaultItem.Clone;
  Result.FFormattingOptions := TdxParagraphFormattingOptions.EmptyParagraphFormattingOption;
  Result.FTabInfo := ADocumentModel.Cache.TabFormattingInfoCache.DefaultItem.Clone;
  Result.FParagraphStyleIndex := SmallInt(ADocumentModel.ParagraphStyles.DefaultItemIndex);
end;

procedure TdxParagraphInfo.SetFormattingInfo(const AValue: TdxParagraphFormattingInfo);
begin
  Assert(FFormattingInfo <> nil);
  FFormattingInfo.CopyFrom(AValue);
end;

function TdxParagraphInfo.GetTableDepth: Integer;
begin
  if FInTable then
    Result := Max(1, FTableDepth)
  else
    Result := FTableDepth;
end;

{ TdxFrameInfo }

destructor TdxFrameInfo.Destroy;
begin
  FFormattingInfo.Free;
  inherited Destroy;
end;

class function TdxFrameInfo.CreateDefault(ADocumentModel: TdxDocumentModel): TdxFrameInfo;
begin
  Result := TdxFrameInfo.Create;
  Result.FFormattingInfo := ADocumentModel.Cache.ParagraphFrameFormattingInfoCache.DefaultItem.Clone;
  Result.FFormattingOptions := TdxParagraphFrameFormattingOptions.EmptyParagraphFrameFormattingOption;
end;

procedure TdxFrameInfo.SetFormattingInfo(const AValue: TdxParagraphFrameFormattingInfo);
begin
  Assert(FFormattingInfo <> nil);
  FFormattingInfo.CopyFrom(AValue);
end;

{ TdxCharacterInfo }

destructor TdxCharacterInfo.Destroy;
begin
  FFormattingInfo.Free;
  FPictureBulletInformation.Free;
  inherited Destroy;
end;

class function TdxCharacterInfo.CreateDefault(ADocumentModel: TdxDocumentModel): TdxCharacterInfo;
begin
  Result := TdxCharacterInfo.Create;
  Result.FFormattingInfo := TdxDocCharacterFormattingInfo.CreateDefault;
  Result.FFormattingOptions := ADocumentModel.Cache.CharacterFormattingCache[TdxCharacterFormattingInfoCache.DefaultItemIndex].Options.Clone;
  Result.FPictureBulletInformation := TdxDocPictureBulletInformation.Create;
end;

procedure TdxCharacterInfo.SetFormattingInfo(const AValue: TdxDocCharacterFormattingInfo);
begin
  Assert(FFormattingInfo <> nil);
  FFormattingInfo := AValue;
end;

{ TdxDocTableInfo }

constructor TdxDocTableInfo.Create;
begin
  FTableStyleIndex := -1;
end;

destructor TdxDocTableInfo.Destroy;
begin
  FTableDefinition.Free;
  FTableProperties.Free;
  inherited Destroy;
end;

class function TdxDocTableInfo.CreateDefault(ADocumentModel: TdxDocumentModel): TdxDocTableInfo;
begin
  Result := TdxDocTableInfo.Create;
  Result.FTableProperties := TdxTableProperties.Create(ADocumentModel.MainPieceTable);
  Result.FTableProperties.SetIndexInitial(TdxTablePropertiesOptionsCache.EmptyTableFormattingOptionsItem);
  Result.FTableProperties.TableLayout := TdxTableLayoutType.Fixed;
  Result.FTableDefinition := TdxTableDefinitionOperand.Create;
end;

procedure TdxDocTableInfo.SetTableDefinition(const AValue: TdxTableDefinitionOperand);
begin
  FTableDefinition.Free;
  FTableDefinition := AValue.Clone;
end;

{ TdxVerticalMergeInfo }

constructor TdxVerticalMergeInfo.Create(ACellIndex: Byte; AMergingState: TdxMergingState);
begin
  FCellIndex := ACellIndex;
  FMergingState := AMergingState;
end;

{ TdxHorizontalMergeInfo }

constructor TdxHorizontalMergeInfo.Create(AFirstCellIndex: Byte; ALastCellIndex: Byte; ASplit: Boolean);
begin
  FFirstCellIndex := AFirstCellIndex;
  FLastCellIndex := ALastCellIndex;
  FSplit := ASplit;
end;

{ TdxTableRowInfo }

constructor TdxTableRowInfo.Create;
begin
  FHorizontalMerging := TdxObjectList<TdxHorizontalMergeInfo>.Create;
  FVerticalMerging := TdxVerticalMergeInfoList.Create;

  FPreferredCellWidths := TdxTableCellWidthOperandList.Create;
  FColumnWidthActions := TdxColumnWidthOperandList.Create;

  FInsertActions := TdxInsertOperandList.Create;
  FCellMarginActions := TdxCellSpacingOperandList.Create;
  FOverrideCellBordersActions := TdxTableBordersOverrideOperandList.Create;
  FCellRangeVerticalAlignmentActions := TdxCellRangeVerticalAlignmentOperandList.Create;
  FCellHideMarkActions := TdxCellHideMarkOperandList.Create;

  FTopBorders := TdxDocTableBorderColorReferenceList.Create;
  FLeftBorders := TdxDocTableBorderColorReferenceList.Create;
  FRightBorders := TdxDocTableBorderColorReferenceList.Create;
  FBottomBorders := TdxDocTableBorderColorReferenceList.Create;

  FCellShading1 := TdxAlphaColorList.Create;
  FCellShading2 := TdxAlphaColorList.Create;
  FCellShading3 := TdxAlphaColorList.Create;
  FDefaultCellsShading := TdxAlphaColorList.Create;
end;

destructor TdxTableRowInfo.Destroy;
begin
  FHorizontalMerging.Free;
  FVerticalMerging.Free;
  FPreferredCellWidths.Free;
  FColumnWidthActions.Free;
  FInsertActions.Free;
  FCellMarginActions.Free;
  FOverrideCellBordersActions.Free;
  FCellRangeVerticalAlignmentActions.Free;
  FCellHideMarkActions.Free;
  FTopBorders.Free;
  FLeftBorders.Free;
  FRightBorders.Free;
  FBottomBorders.Free;

  FCellShading1.Free;
  FCellShading2.Free;
  FCellShading3.Free;
  FDefaultCellsShading.Free;
  FTableRowProperties.Free;
  FreeAndNil(FTableAutoformatLookSpecifier);
  inherited Destroy;
end;

procedure TdxTableRowInfo.SetTableAutoformatLookSpecifier(const Value: TLP);
begin
  if FTableAutoformatLookSpecifier = Value then
    Exit;
  FTableAutoformatLookSpecifier.Free;
  FTableAutoformatLookSpecifier := Value;
end;

class function TdxTableRowInfo.CreateDefault(ADocumentModel: TdxDocumentModel): TdxTableRowInfo;
begin
  Result := TdxTableRowInfo.Create;
  Result.FTableRowProperties.Free;
  Result.FTableRowProperties := TdxTableRowProperties.Create(ADocumentModel.MainPieceTable);
  Result.FTableRowProperties.SetIndexInitial(TdxTableRowPropertiesOptionsCache.EmptyRowPropertiesOptionsItem);
end;

{ TdxTableCellInfo }

constructor TdxTableCellInfo.Create;
begin
  FCellColors := TdxAlphaColorList.Create;
  FCellColors.Capacity := MaxCellCount;
end;

destructor TdxTableCellInfo.Destroy;
begin
  FCellColors.Free;
  FTableCellProperties.Free;
  inherited Destroy;
end;

class function TdxTableCellInfo.CreateDefault(ADocumentModel: TdxDocumentModel): TdxTableCellInfo;
begin
  Result := TdxTableCellInfo.Create;
  Result.FTableCellProperties := TdxTableCellProperties.Create(ADocumentModel.MainPieceTable, ADocumentModel);
  Result.FTableCellProperties.SetIndexInitial(TdxTableCellPropertiesOptionsCache.EmptyCellPropertiesOptionsItem);
end;

{ TdxTabDescriptor }

constructor TdxTabDescriptor.Create(ATbd: Byte);
var
  ATabAlignment: TdxTabAlignmentType;
  ATabLeader: TdxTabLeaderType;
  ATabAlignmentCode, ATabLeaderCode: Byte;
begin
  ATabAlignmentCode := Byte(ATbd and $07);
  ATabLeaderCode := Byte((ATbd and $38) shr 3);
  if FTabAlignments.TryGetValue(ATabAlignmentCode, ATabAlignment) then
    FTabAlignment := ATabAlignment;
  if FTabLeaders.TryGetValue(ATabLeaderCode, ATabLeader) then
    FTabLeader := ATabLeader;
end;

constructor TdxTabDescriptor.Create(AAlignment: TdxTabAlignmentType; ALeader: TdxTabLeaderType);
begin
  FTabAlignment := AAlignment;
  FTabLeader := ALeader;
end;

class constructor TdxTabDescriptor.Initialize;
begin
  FTabAlignments := TDictionary<Byte, TdxTabAlignmentType>.Create(4);
  FTabAlignments.Add($00, TdxTabAlignmentType.Left);
  FTabAlignments.Add($01, TdxTabAlignmentType.Center);
  FTabAlignments.Add($02, TdxTabAlignmentType.Right);
  FTabAlignments.Add($03, TdxTabAlignmentType.Decimal);

  FTabLeaders := TDictionary<Byte, TdxTabLeaderType>.Create(6);
  FTabLeaders.Add($00, TdxTabLeaderType.None);
  FTabLeaders.Add($01, TdxTabLeaderType.Dots);
  FTabLeaders.Add($02, TdxTabLeaderType.Hyphens);
  FTabLeaders.Add($03, TdxTabLeaderType.Underline);
  FTabLeaders.Add($04, TdxTabLeaderType.ThickLine);
  FTabLeaders.Add($05, TdxTabLeaderType.MiddleDots);
end;

class destructor TdxTabDescriptor.Finalize;
begin
  FTabAlignments.Free;;
  FTabLeaders.Free;
end;

procedure TdxTabDescriptor.Write(AWriter: TBinaryWriter);
var
  ATbd: Byte;
begin
  Assert(AWriter <> nil, 'writer');
  ATbd := Byte((CalcTabAlignmentCode) or (CalcTabLeaderCode shl 3));
  AWriter.Write(ATbd);
end;

function TdxTabDescriptor.CalcTabAlignmentCode: Byte;
begin
  case Alignment of
    TdxTabAlignmentType.Left:    Result := $00;
    TdxTabAlignmentType.Center:  Result := $01;
    TdxTabAlignmentType.Right:   Result := $02;
    TdxTabAlignmentType.Decimal: Result := $03;
  else
    Result := $00;
  end;
end;

function TdxTabDescriptor.CalcTabLeaderCode: Byte;
begin
  case Leader of
    TdxTabLeaderType.EqualSign,
    TdxTabLeaderType.None:       Result := $00;
    TdxTabLeaderType.Dots:       Result := $01;
    TdxTabLeaderType.Hyphens:    Result := $02;
    TdxTabLeaderType.Underline:  Result := $03;
    TdxTabLeaderType.ThickLine:  Result := $04;
    TdxTabLeaderType.MiddleDots: Result := $05;
  else
      Result := $00;
  end;
end;

{ TdxTabsOperandBase }

constructor TdxTabsOperandBase.Create;
begin
  FAddedTabsPositions := TdxIntegerList.Create;
  FDeletedTabsPositions := TdxIntegerList.Create;
  FAddedTabs := TObjectList<TdxTabDescriptor>.Create;
end;

destructor TdxTabsOperandBase.Destroy;
begin
  FAddedTabsPositions.Free;
  FDeletedTabsPositions.Free;
  FAddedTabs.Free;
  inherited Destroy;
end;

procedure TdxTabsOperandBase.Read(const ATabs: TBytes);
var
  ACurrentPosition: Integer;
begin
  FDeletedTabsCount := ATabs[0];
  ACurrentPosition := 1;
  Inc(ACurrentPosition, ReadDeletedTabs(ATabs, ACurrentPosition));
  FAddedTabsCount := ATabs[ACurrentPosition];
  Inc(ACurrentPosition);
  ReadAddedTabs(ATabs, ACurrentPosition);
end;

function TdxTabsOperandBase.ReadDeletedTabs(const ATabs: TBytes; ACurrentPosition: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to DeletedTabsCount - 1 do
    FDeletedTabsPositions.Add(PSmallInt(@ATabs[I * PositionSize + ACurrentPosition])^);
  Result := DeletedTabsCount * PositionSize;
end;

procedure TdxTabsOperandBase.ReadAddedTabs(const ATabs: TBytes; ACurrentPosition: Integer);
var
  I: Integer;
begin
  for I := 0 to AddedTabsCount - 1 do
    FAddedTabsPositions.Add(PSmallInt(@ATabs[I * PositionSize + ACurrentPosition])^);
  Inc(ACurrentPosition, AddedTabsCount * PositionSize);
  for I := 0 to AddedTabsCount - 1 do
    FAddedTabs.Add(TdxTabDescriptor.Create(ATabs[I + ACurrentPosition]));
end;

procedure TdxTabsOperandBase.Write(AWriter: TBinaryWriter);
var
  ATotalSize: Byte;
  I: Integer;
begin
  Assert(AWriter <> nil, 'writer');
  ATotalSize := Byte(1 + DeletedTabsCount * PositionSize + 1 + AddedTabsCount * (1 + PositionSize));
  AWriter.Write(ATotalSize);
  AWriter.Write(DeletedTabsCount);
  for I := 0 to DeletedTabsCount - 1 do
    AWriter.Write(SmallInt(Min(TdxDocConstants.MaxXASValue, DeletedTabsPositions[I])));
  AWriter.Write(AddedTabsCount);
  for I := 0 to AddedTabsCount - 1 do
    AWriter.Write(SmallInt(Min(TdxDocConstants.MaxXASValue, AddedTabsPositions[I])));
  for I := 0 to AddedTabsCount - 1 do
    AddedTabs[I].Write(AWriter);
end;

procedure TdxTabsOperandBase.ConvertFromFormattingTabInfo(AInfo: TdxTabFormattingInfo; AUnitConverter: TdxDocumentModelUnitConverter);
var
  I: Integer;
  ATbd: TdxTabDescriptor;
begin
  for I := 0 to AInfo.Count - 1 do
    if AInfo[I].Deleted then
      FDeletedTabsPositions.Add(AUnitConverter.ModelUnitsToTwips(AInfo[I].Position))
    else
    begin
      FAddedTabsPositions.Add(AUnitConverter.ModelUnitsToTwips(AInfo[I].Position));
      ATbd := TdxTabDescriptor.Create(AInfo[I].Alignment, AInfo[I].Leader);
      FAddedTabs.Add(ATbd);
    end;
  FDeletedTabsCount := Byte(FDeletedTabsPositions.Count);
  FAddedTabsCount := Byte(FAddedTabs.Count);
end;

procedure TdxTabsOperandBase.AddTabs(AFormattingInfo: TdxTabFormattingInfo; AUnitConverter: TdxDocumentModelUnitConverter);
var
  ACount, I: Integer;
  AInfo: TdxTabInfo;
begin
  ACount := DeletedTabsCount;
  for I := 0 to ACount - 1 do
  begin
    AInfo := TdxTabInfo.Create(AUnitConverter.TwipsToModelUnits(DeletedTabsPositions[I]), TdxTabAlignmentType.Left, TdxTabLeaderType.None, True, False);
    AFormattingInfo.Add(AInfo);
  end;
  ACount := AddedTabsCount;
  for I := 0 to ACount - 1 do
  begin
    AInfo := TdxTabInfo.Create(AUnitConverter.TwipsToModelUnits(AddedTabsPositions[I]), AddedTabs[I].Alignment, AddedTabs[I].Leader, False, False);
    AFormattingInfo.Add(AInfo);
  end;
end;

{ TdxTabsOperand }

class function TdxTabsOperand.FromByteArray(const ATabs: TBytes): TdxTabsOperand;
begin
  Result := TdxTabsOperand.Create;
  Result.Read(ATabs);
end;

{ TdxTabsOperandClose }

constructor TdxTabsOperandClose.Create;
begin
  inherited Create;
  FTolerances := TdxIntegerList.Create;
end;

destructor TdxTabsOperandClose.Destroy;
begin
  FTolerances.Free;
  inherited Destroy;
end;

class function TdxTabsOperandClose.FromByteArray(const ATabs: TBytes): TdxTabsOperandClose;
begin
  Result := TdxTabsOperandClose.Create;
  Result.Read(ATabs);
end;

function TdxTabsOperandClose.ReadDeletedTabs(const ATabs: TBytes; ACurrentPosition: Integer): Integer;
var
  I: Integer;
begin
  Result := inherited ReadDeletedTabs(ATabs, ACurrentPosition);
  for I := 0 to DeletedTabsCount - 1 do
    FTolerances.Add(PSmallInt(@ATabs[I * PositionSize + ACurrentPosition])^);
  Inc(Result, DeletedTabsCount * PositionSize);
end;

{ TdxTableCellDescriptor }

constructor TdxTableCellDescriptor.Create;
begin
  FBottomBorder := TdxBorderDescriptor97.Create;
  FLeftBorder := TdxBorderDescriptor97.Create;
  FRightBorder := TdxBorderDescriptor97.Create;
  FTopBorder := TdxBorderDescriptor97.Create;
end;

destructor TdxTableCellDescriptor.Destroy;
begin
  FTopBorder.Free;
  FLeftBorder.Free;
  FBottomBorder.Free;
  FRightBorder.Free;
  inherited Destroy;
end;

class function TdxTableCellDescriptor.FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxTableCellDescriptor;
begin
  Result := TdxTableCellDescriptor.Create;
  Result.Read(AData, AStartIndex);
end;

procedure TdxTableCellDescriptor.Read(const AData: TBytes; AStartIndex: Integer);
var
  ABitwiseField: SmallInt;
begin
  Assert(AData <> nil, 'data');
  ABitwiseField := PSmallInt(@AData[AStartIndex])^;
  FHorizontalMerging := TdxMergingStateCalculator.CalcHorizontalMergingState(Byte(ABitwiseField and $3));
  FTextDirection := TdxTextDirectionCalculator.CalcTextDirection(Byte((ABitwiseField and $1c) shr 2));
  FVerticalMerging := TdxMergingStateCalculator.CalcVerticalMergingState(Byte((ABitwiseField and $60) shr 5));
  FVerticalAlignment := TdxAlignmentCalculator.CalcVerticalAlignment(Byte((ABitwiseField and $0180) shr 7));
  FType := TdxWidthUnitCalculator.CalcWidthUnitType(Byte((ABitwiseField and $0e00) shr 9));
  FFitText := (ABitwiseField and $1000) = $1000;
  FNoWrap := (ABitwiseField and $2000) = $2000;
  FHideCellMark := (ABitwiseField and $4000) = $4000;
  FPreferredWidth := PSmallInt(@AData[AStartIndex + 2])^;
  FTopBorder.Free;
  FTopBorder := TdxBorderDescriptor97.FromByteArray(AData, AStartIndex + 4);
  FLeftBorder.Free;
  FLeftBorder := TdxBorderDescriptor97.FromByteArray(AData, AStartIndex + 8);
  FBottomBorder.Free;
  FBottomBorder := TdxBorderDescriptor97.FromByteArray(AData, AStartIndex + 12);
  FRightBorder.Free;
  FRightBorder := TdxBorderDescriptor97.FromByteArray(AData, AStartIndex + 16);
end;

function TdxTableCellDescriptor.Clone: TdxTableCellDescriptor;
begin
  Result := TdxTableCellDescriptor.Create;
  Result.FFitText := FFitText;
  Result.FHideCellMark := FHideCellMark;
  Result.FHorizontalMerging := FHorizontalMerging;
  Result.FNoWrap := FNoWrap;
  Result.FPreferredWidth := FPreferredWidth;
  Result.FTextDirection := FTextDirection;
  Result.FType := FType;
  Result.FVerticalAlignment := FVerticalAlignment;
  Result.FVerticalMerging := FVerticalMerging;
  Result.FBottomBorder.Free;
  Result.FBottomBorder := FBottomBorder.Clone;
  Result.FLeftBorder.Free;
  Result.FLeftBorder := FLeftBorder.Clone;
  Result.FRightBorder.Free;
  Result.FRightBorder := FRightBorder.Clone;
  Result.FTopBorder.Free;
  Result.FTopBorder := FTopBorder.Clone;
end;

{ TdxColumnWidthOperand }

class function TdxColumnWidthOperand.FromByteArray(const AData: TBytes): TdxColumnWidthOperand;
begin
  Result := TdxColumnWidthOperand.Create;
  Result.Read(AData);
end;

procedure TdxColumnWidthOperand.Read(const AData: TBytes);
begin
  FStartIndex := AData[0];
  FEndIndex := Byte(AData[1] - 1);
  FWidthInTwips := PSmallInt(@AData[2])^;
end;

function TdxColumnWidthOperand.GetBytes: TBytes;
begin
  SetLength(Result, 4);
  Result[0] := FStartIndex;
  Result[1] := Byte(FEndIndex + 1);
  Move(FWidthInTwips, Result[2], 2);
end;

function TdxColumnWidthOperand.Clone: TdxColumnWidthOperand;
begin
  Result := TdxColumnWidthOperand.Create;
  Result.FEndIndex := FEndIndex;
  Result.FStartIndex := FStartIndex;
  Result.FWidthInTwips := FWidthInTwips;
end;

{ TdxWidthUnitOperand }

class function TdxWidthUnitOperand.FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxWidthUnitOperand;
begin
  Result := TdxWidthUnitOperand.Create;
  Result.Read(AData, AStartIndex);
end;

class function TdxWidthUnitOperand.NonNegativeFromByteArray(const AData: TBytes; AStartIndex: Integer): TdxWidthUnitOperand;
begin
  Result := TdxWidthUnitOperand.Create;
  Result.Read(AData, AStartIndex);
  if Result.FValue < 0 then
    Result.FValue := 0;
end;

procedure TdxWidthUnitOperand.Read(const AData: TBytes; AStartIndex: Integer);
begin
  FType := TdxWidthUnitCalculator.CalcWidthUnitType(AData[AStartIndex]);
  FValue := PSmallInt(@AData[AStartIndex + 1])^;
end;

procedure TdxWidthUnitOperand.ConvertFromWidthUnitInfo(AInfo: TdxWidthUnitInfo; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  FType := AInfo.&Type;
  case FType of
    TdxWidthUnitType.Nil:
      FValue := 0;
    TdxWidthUnitType.ModelUnits:
      FValue := SmallInt(Min(AUnitConverter.ModelUnitsToTwips(AInfo.Value), TdxDocConstants.MaxXASValue));
    else
      FValue := SmallInt(AInfo.Value);
  end;
end;

function TdxWidthUnitOperand.GetBytes: TBytes;
begin
  SetLength(Result, 3);
  Result[0] := TdxWidthUnitCalculator.CalcWidthUnitTypeCode(FType);

  if FType <> TdxWidthUnitType.Auto then
    Move(FValue, Result[1], 2);
end;

function TdxWidthUnitOperand.Clone: TdxWidthUnitOperand;
begin
  Result := TdxWidthUnitOperand.Create;
  Result.FType := FType;
  Result.FValue := FValue;
end;

{ TdxTableCellWidthOperand }

constructor TdxTableCellWidthOperand.Create;
begin
  FWidthUnit := TdxWidthUnitOperand.Create;
end;

destructor TdxTableCellWidthOperand.Destroy;
begin
  FWidthUnit.Free;
  inherited Destroy;
end;

class function TdxTableCellWidthOperand.FromByteArray(const AData: TBytes): TdxTableCellWidthOperand;
begin
  Result := TdxTableCellWidthOperand.Create;
  Result.Read(AData);
end;

procedure TdxTableCellWidthOperand.Read(const AData: TBytes);
begin
  FStartIndex := AData[0];
  FEndIndex := Byte((AData[1] - 1));
  WidthUnit := TdxWidthUnitOperand.NonNegativeFromByteArray(AData, 2);
end;

procedure TdxTableCellWidthOperand.SetWidthUnit(
  const Value: TdxWidthUnitOperand);
begin
  FWidthUnit.Free;
  FWidthUnit := Value;
end;

function TdxTableCellWidthOperand.GetBytes: TBytes;
begin
  SetLength(Result, 5);
  Result[0] := FStartIndex;
  Result[1] := Byte((FEndIndex + 1));
  Move(FWidthUnit.GetBytes[0], Result[2], 3);
end;

function TdxTableCellWidthOperand.Clone: TdxTableCellWidthOperand;
begin
  Result := TdxTableCellWidthOperand.Create;
  Result.FEndIndex := FEndIndex;
  Result.FStartIndex := FStartIndex;
  Result.WidthUnit := FWidthUnit.Clone;
end;

{ TdxInsertOperand }

class function TdxInsertOperand.FromByteArray(const AData: TBytes): TdxInsertOperand;
begin
  Result := TdxInsertOperand.Create;
  Result.Read(AData);
end;

procedure TdxInsertOperand.Read(const AData: TBytes);
begin
  FStartIndex := AData[0];
  FCount := AData[1];
  FWidthInTwips := PSmallInt(@AData[2])^;
end;

function TdxInsertOperand.GetBytes: TBytes;
begin
  SetLength(Result, 4);
  Result[0] := FStartIndex;
  Result[1] := FCount;
  Move(FWidthInTwips, Result[2], 2);
end;

function TdxInsertOperand.Clone: TdxInsertOperand;
begin
  Result := TdxInsertOperand.Create;
  Result.FCount := FCount;
  Result.FStartIndex := FStartIndex;
  Result.FWidthInTwips := FWidthInTwips;
end;

{ TdxTableBordersOperand80 }

constructor TdxTableBordersOperand80.Create;
begin
  FBottomBorder := TdxBorderDescriptor97.Create;
  FInsideHorizontalBorder := TdxBorderDescriptor97.Create;
  FInsideVerticalBorder := TdxBorderDescriptor97.Create;
  FLeftBorder := TdxBorderDescriptor97.Create;
  FRightBorder := TdxBorderDescriptor97.Create;
  FTopBorder := TdxBorderDescriptor97.Create;
end;

destructor TdxTableBordersOperand80.Destroy;
begin
  FBottomBorder.Free;
  FInsideHorizontalBorder.Free;
  FInsideVerticalBorder.Free;
  FLeftBorder.Free;
  FRightBorder.Free;
  FTopBorder.Free;
  inherited Destroy;
end;

class function TdxTableBordersOperand80.FromByteArray(const AData: TBytes): TdxTableBordersOperand80;
begin
  Result := TdxTableBordersOperand80.Create;
  Result.Read(AData);
end;

procedure TdxTableBordersOperand80.Read(const AData: TBytes);
var
  ACurrentStartIndex: Integer;
begin
  Assert(AData <> nil, 'data');
  ACurrentStartIndex := 0;

  FTopBorder.Free;
  FTopBorder := TdxBorderDescriptor97.FromByteArray(AData, ACurrentStartIndex);
  Inc(ACurrentStartIndex, TdxBorderDescriptor97.BorderDescriptorSize);

  FLeftBorder.Free;
  FLeftBorder := TdxBorderDescriptor97.FromByteArray(AData, ACurrentStartIndex);
  Inc(ACurrentStartIndex, TdxBorderDescriptor97.BorderDescriptorSize);

  FBottomBorder.Free;
  FBottomBorder := TdxBorderDescriptor97.FromByteArray(AData, ACurrentStartIndex);
  Inc(ACurrentStartIndex, TdxBorderDescriptor97.BorderDescriptorSize);

  FRightBorder.Free;
  FRightBorder := TdxBorderDescriptor97.FromByteArray(AData, ACurrentStartIndex);
  Inc(ACurrentStartIndex, TdxBorderDescriptor97.BorderDescriptorSize);

  FInsideHorizontalBorder.Free;
  FInsideHorizontalBorder := TdxBorderDescriptor97.FromByteArray(AData, ACurrentStartIndex);
  Inc(ACurrentStartIndex, TdxBorderDescriptor97.BorderDescriptorSize);

  FInsideVerticalBorder.Free;
  FInsideVerticalBorder := TdxBorderDescriptor97.FromByteArray(AData, ACurrentStartIndex);
end;

procedure TdxTableBordersOperand80.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  FTopBorder.Write(AWriter);
  FLeftBorder.Write(AWriter);
  FBottomBorder.Write(AWriter);
  FRightBorder.Write(AWriter);
  FInsideHorizontalBorder.Write(AWriter);
  FInsideVerticalBorder.Write(AWriter);
end;

procedure TdxTableBordersOperand80.ApplyProperties(ADestination: TdxTableBorders; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  BottomBorder.ApplyProperties(ADestination.BottomBorder, AUnitConverter);
  InsideHorizontalBorder.ApplyProperties(ADestination.InsideHorizontalBorder, AUnitConverter);
  InsideVerticalBorder.ApplyProperties(ADestination.InsideVerticalBorder, AUnitConverter);
  LeftBorder.ApplyProperties(ADestination.LeftBorder, AUnitConverter);
  RightBorder.ApplyProperties(ADestination.RightBorder, AUnitConverter);
  TopBorder.ApplyProperties(ADestination.TopBorder, AUnitConverter);
end;

{ TdxTableBordersOperand }

constructor TdxTableBordersOperand.Create;
begin
  FBottomBorder := TdxBorderDescriptor.Create;
  FInsideHorizontalBorder := TdxBorderDescriptor.Create;
  FInsideVerticalBorder := TdxBorderDescriptor.Create;
  FLeftBorder := TdxBorderDescriptor.Create;
  FRightBorder := TdxBorderDescriptor.Create;
  FTopBorder := TdxBorderDescriptor.Create;
end;

destructor TdxTableBordersOperand.Destroy;
begin
  FBottomBorder.Free;
  FInsideHorizontalBorder.Free;
  FInsideVerticalBorder.Free;
  FLeftBorder.Free;
  FRightBorder.Free;
  FTopBorder.Free;
  inherited Destroy;
end;

class function TdxTableBordersOperand.FromByteArray(const AData: TBytes): TdxTableBordersOperand;
begin
  Result := TdxTableBordersOperand.Create;
  Result.Read(AData);
end;

procedure TdxTableBordersOperand.Read(const AData: TBytes);
var
  ACurrentStartIndex: Integer;
begin
  Assert(AData <> nil, 'data');
  ACurrentStartIndex := 0;

  FTopBorder.Free;
  FTopBorder := TdxBorderDescriptor.FromByteArray(AData, ACurrentStartIndex);
  Inc(ACurrentStartIndex, TdxBorderDescriptor.BorderDescriptorSize);

  FLeftBorder.Free;
  FLeftBorder := TdxBorderDescriptor.FromByteArray(AData, ACurrentStartIndex);
  Inc(ACurrentStartIndex, TdxBorderDescriptor.BorderDescriptorSize);

  FBottomBorder.Free;
  FBottomBorder := TdxBorderDescriptor.FromByteArray(AData, ACurrentStartIndex);
  Inc(ACurrentStartIndex, TdxBorderDescriptor.BorderDescriptorSize);

  FRightBorder.Free;
  FRightBorder := TdxBorderDescriptor.FromByteArray(AData, ACurrentStartIndex);
  Inc(ACurrentStartIndex, TdxBorderDescriptor.BorderDescriptorSize);

  FInsideHorizontalBorder.Free;
  FInsideHorizontalBorder := TdxBorderDescriptor.FromByteArray(AData, ACurrentStartIndex);
  Inc(ACurrentStartIndex, TdxBorderDescriptor.BorderDescriptorSize);

  FInsideVerticalBorder.Free;
  FInsideVerticalBorder := TdxBorderDescriptor.FromByteArray(AData, ACurrentStartIndex);
end;

procedure TdxTableBordersOperand.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  FTopBorder.Write(AWriter);
  FLeftBorder.Write(AWriter);
  FBottomBorder.Write(AWriter);
  FRightBorder.Write(AWriter);
  FInsideHorizontalBorder.Write(AWriter);
  FInsideVerticalBorder.Write(AWriter);
end;

procedure TdxTableBordersOperand.ApplyProperties(ADestination: TdxTableBorders; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  BottomBorder.ApplyProperties(ADestination.BottomBorder, AUnitConverter);
  InsideHorizontalBorder.ApplyProperties(ADestination.InsideHorizontalBorder, AUnitConverter);
  InsideVerticalBorder.ApplyProperties(ADestination.InsideVerticalBorder, AUnitConverter);
  LeftBorder.ApplyProperties(ADestination.LeftBorder, AUnitConverter);
  RightBorder.ApplyProperties(ADestination.RightBorder, AUnitConverter);
  TopBorder.ApplyProperties(ADestination.TopBorder, AUnitConverter);
end;

{ TdxTableBordersOverrideOperand }

constructor TdxTableBordersOverrideOperand.Create;
begin
  FBorder := TdxBorderDescriptor.Create;
end;

destructor TdxTableBordersOverrideOperand.Destroy;
begin
  FBorder.Free;
  inherited Destroy;
end;

class function TdxTableBordersOverrideOperand.FromByteArray(const AData: TBytes): TdxTableBordersOverrideOperand;
begin
  Result := TdxTableBordersOverrideOperand.Create;
  Result.Read(AData);
end;

procedure TdxTableBordersOverrideOperand.Read(const AData: TBytes);
begin
  Assert(AData <> nil, 'data');
  FStartIndex := AData[0];
  FEndIndex := Byte(AData[1] - 1);
  FCellBorders := TdxDocTableCellBorders(AData[2]);

  FBorder.Free;
  FBorder := TdxBorderDescriptor.FromByteArray(AData, 3);
end;

procedure TdxTableBordersOverrideOperand.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(FStartIndex);
  AWriter.Write(Byte(FEndIndex + 1));
  AWriter.Write(Byte(FCellBorders));
  FBorder.Write(AWriter);
end;

procedure TdxTableBordersOverrideOperand.ApplyProperties(ADestination: TdxTableCellBorders; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  if TdxDocTableCellBorder.Bottom in CellBorders then
    ApplyPropertiesCore(ADestination.BottomBorder, AUnitConverter);
  if TdxDocTableCellBorder.Left in CellBorders then
    ApplyPropertiesCore(ADestination.LeftBorder, AUnitConverter);
  if TdxDocTableCellBorder.Right in CellBorders then
    ApplyPropertiesCore(ADestination.RightBorder, AUnitConverter);
  if TdxDocTableCellBorder.Top in CellBorders then
    ApplyPropertiesCore(ADestination.TopBorder, AUnitConverter);
end;

procedure TdxTableBordersOverrideOperand.ApplyPropertiesCore(ABorder: TdxBorderBase; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  ABorder.BeginUpdate;
  try
    Border.ApplyProperties(ABorder, AUnitConverter);
  finally
    ABorder.EndUpdate;
  end;
end;

function TdxTableBordersOverrideOperand.Clone: TdxTableBordersOverrideOperand;
begin
  Result := TdxTableBordersOverrideOperand.Create;
  Result.FStartIndex := FStartIndex;
  Result.FEndIndex := FEndIndex;
  Result.FCellBorders := FCellBorders;
  Result.FBorder := FBorder.Clone;
end;

{ TdxCellSpacingOperand }

constructor TdxCellSpacingOperand.Create;
begin
  FWidthUnit := TdxWidthUnitOperand.Create;
end;

destructor TdxCellSpacingOperand.Destroy;
begin
  FWidthUnit.Free;
  inherited Destroy;
end;

class function TdxCellSpacingOperand.FromByteArray(const AData: TBytes): TdxCellSpacingOperand;
begin
  Result := TdxCellSpacingOperand.Create;
  Result.Read(AData);
end;

procedure TdxCellSpacingOperand.Read(const AData: TBytes);
begin
  Assert(AData <> nil, 'data');
  FStartIndex := AData[0];
  FEndIndex := Byte(AData[1] - 1);
  FCellBorders := TdxDocTableCellBorders(AData[2]);

  FWidthUnit.Free;
  FWidthUnit := TdxWidthUnitOperand.NonNegativeFromByteArray(AData, 3);
end;

procedure TdxCellSpacingOperand.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(FStartIndex);
  AWriter.Write(Byte(FEndIndex + 1));
  AWriter.Write(Byte(FCellBorders));
  AWriter.Write(FWidthUnit.GetBytes);
end;

procedure TdxCellSpacingOperand.ApplyProperties(ADestination: TdxCellMargins; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  if WidthUnit.&Type = TdxWidthUnitType.ModelUnits then
    WidthUnit.Value := AUnitConverter.TwipsToModelUnits(WidthUnit.Value);
  if TdxDocTableCellBorder.Bottom in CellBorders then
    ApplyPropertiesCore(ADestination.Bottom);
  if TdxDocTableCellBorder.Left in CellBorders then
    ApplyPropertiesCore(ADestination.Left);
  if TdxDocTableCellBorder.Right in CellBorders then
    ApplyPropertiesCore(ADestination.Right);
  if TdxDocTableCellBorder.Top in CellBorders then
    ApplyPropertiesCore(ADestination.Top);
end;

procedure TdxCellSpacingOperand.ApplyPropertiesCore(AMargin: TdxMarginUnitBase);
begin
  AMargin.BeginUpdate;
  try
    AMargin.&Type := WidthUnit.&Type;
    AMargin.Value := WidthUnit.Value;
  finally
    AMargin.EndUpdate;
  end;
end;

function TdxCellSpacingOperand.Clone: TdxCellSpacingOperand;
begin
  Result := TdxCellSpacingOperand.Create;
  Result.FCellBorders := FCellBorders;
  Result.FEndIndex := FEndIndex;
  Result.FStartIndex := FStartIndex;
  Result.FWidthUnit := FWidthUnit.Clone;
end;

{ TdxCellRangeVerticalAlignmentOperand }

class function TdxCellRangeVerticalAlignmentOperand.FromByteArray(const AData: TBytes): TdxCellRangeVerticalAlignmentOperand;
begin
  Result := TdxCellRangeVerticalAlignmentOperand.Create;
  Result.Read(AData);
end;

procedure TdxCellRangeVerticalAlignmentOperand.Read(const AData: TBytes);
begin
  FStartIndex := AData[0];
  FEndIndex := Byte(AData[1] - 1);
  FVerticalAlignment := TdxAlignmentCalculator.CalcVerticalAlignment(AData[2]);
end;

procedure TdxCellRangeVerticalAlignmentOperand.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(FStartIndex);
  AWriter.Write(Byte(FEndIndex + 1));
  AWriter.Write(TdxAlignmentCalculator.CalcVerticalAlignmentTypeCode(FVerticalAlignment));
end;

function TdxCellRangeVerticalAlignmentOperand.Clone: TdxCellRangeVerticalAlignmentOperand;
begin
  Result := TdxCellRangeVerticalAlignmentOperand.Create;
  Result.FStartIndex := FStartIndex;
  Result.FEndIndex := FEndIndex;
  Result.FVerticalAlignment := FVerticalAlignment;
end;

{ TdxDocCommandParagraphBorder97 }

destructor TdxDocCommandParagraphBorder97.Destroy;
begin
  FCurrentBorder.Free;
  inherited Destroy;
end;

function TdxDocCommandParagraphBorder97.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandParagraphBorder97.Read(const AData: TBytes);
begin
  FCurrentBorder := TdxBorderDescriptor97.FromByteArray(AData, 0);
end;

procedure TdxDocCommandParagraphBorder97.Write(AWriter: TBinaryWriter);
begin
  FCurrentBorder.Write(AWriter);
end;

procedure TdxDocCommandParagraphBorder97.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
end;

procedure TdxDocCommandParagraphBorder97.ApplyBorderProperties(ABorder: TdxBorderInfo; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  ABorder.Width := AUnitConverter.TwipsToModelUnits(CurrentBorder.Width * 5 div 2);
  ABorder.Style := TdxDocBorderCalculator.MapToBorderLineStyle(CurrentBorder.Style);
  ABorder.Color := CurrentBorder.BorderColor;
  ABorder.Offset := CurrentBorder.Distance;
end;

{ TdxDocCommandParagraphTopBorder }

procedure TdxDocCommandParagraphTopBorder.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AParagraphInfo: TdxParagraphInfo;
  ABorder: TdxBorderInfo;
begin
  AParagraphInfo := APropertyContainer.ParagraphInfo;
  ABorder := TdxBorderInfo.Create;
  try
    ApplyBorderProperties(ABorder, APropertyContainer.UnitConverter);
    AParagraphInfo.FormattingInfo.TopBorder := ABorder;
  finally
    ABorder.Free;
  end;
  AParagraphInfo.FormattingOptions.UseTopBorder := True;
end;

{ TdxDocCommandParagraphLeftBorder }

procedure TdxDocCommandParagraphLeftBorder.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AParagraphInfo: TdxParagraphInfo;
  ABorder: TdxBorderInfo;
begin
  AParagraphInfo := APropertyContainer.ParagraphInfo;
  ABorder := TdxBorderInfo.Create;
  try
    ApplyBorderProperties(ABorder, APropertyContainer.UnitConverter);
    AParagraphInfo.FormattingInfo.LeftBorder := ABorder;
  finally
    ABorder.Free;
  end;
  AParagraphInfo.FormattingOptions.UseLeftBorder := True;
end;

{ TdxDocCommandParagraphBottomBorder }

procedure TdxDocCommandParagraphBottomBorder.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AParagraphInfo: TdxParagraphInfo;
  ABorder: TdxBorderInfo;
begin
  AParagraphInfo := APropertyContainer.ParagraphInfo;
  ABorder := TdxBorderInfo.Create;
  try
    ApplyBorderProperties(ABorder, APropertyContainer.UnitConverter);
    AParagraphInfo.FormattingInfo.BottomBorder := ABorder;
  finally
    ABorder.Free;
  end;
  AParagraphInfo.FormattingOptions.UseBottomBorder := True;
end;

{ TdxDocCommandParagraphRightBorder }

procedure TdxDocCommandParagraphRightBorder.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AParagraphInfo: TdxParagraphInfo;
  ABorder: TdxBorderInfo;
begin
  AParagraphInfo := APropertyContainer.ParagraphInfo;
  ABorder := TdxBorderInfo.Create;
  try
    ApplyBorderProperties(ABorder, APropertyContainer.UnitConverter);
    AParagraphInfo.FormattingInfo.RightBorder := ABorder;
  finally
    ABorder.Free;
  end;
  AParagraphInfo.FormattingOptions.UseRightBorder := True;
end;

{ TdxDocCommandParagraphBorder }

constructor TdxDocCommandParagraphBorder.Create;
begin
  inherited Create;
  FCurrentBorder := TdxBorderDescriptor.Create;
end;

destructor TdxDocCommandParagraphBorder.Destroy;
begin
  FCurrentBorder.Free;
  inherited Destroy;
end;

function TdxDocCommandParagraphBorder.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandParagraphBorder.Read(const AData: TBytes);
begin
  FCurrentBorder.Free;
  FCurrentBorder := TdxBorderDescriptor.FromByteArray(AData, 0);
end;

procedure TdxDocCommandParagraphBorder.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandFactoryBase.GetOpcodeByType(ClassType));
  AWriter.Write(ParagraphBorderOperandSize);
  FCurrentBorder.Write(AWriter);
end;

procedure TdxDocCommandParagraphBorder.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
end;

procedure TdxDocCommandParagraphBorder.ApplyBorderProperties(ABorder: TdxBorderInfo; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  ABorder.Width := AUnitConverter.TwipsToModelUnits(CurrentBorder.Width * 5 div 2);
  ABorder.Style := TdxDocBorderCalculator.MapToBorderLineStyle(CurrentBorder.Style);
  ABorder.Color := CurrentBorder.Color;
  ABorder.Offset := CurrentBorder.Offset;
end;

{ TdxDocCommandParagraphTopBorderNew }

procedure TdxDocCommandParagraphTopBorderNew.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AParagraphInfo: TdxParagraphInfo;
  ABorder: TdxBorderInfo;
begin
  AParagraphInfo := APropertyContainer.ParagraphInfo;
  ABorder := TdxBorderInfo.Create;
  try
    ApplyBorderProperties(ABorder, APropertyContainer.UnitConverter);
    AParagraphInfo.FormattingInfo.TopBorder := ABorder;
  finally
    ABorder.Free;
  end;
  AParagraphInfo.FormattingOptions.UseTopBorder := True;
end;

{ TdxDocCommandParagraphLeftBorderNew }

procedure TdxDocCommandParagraphLeftBorderNew.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AParagraphInfo: TdxParagraphInfo;
  ABorder: TdxBorderInfo;
begin
  AParagraphInfo := APropertyContainer.ParagraphInfo;
  ABorder := TdxBorderInfo.Create;
  try
    ApplyBorderProperties(ABorder, APropertyContainer.UnitConverter);
    AParagraphInfo.FormattingInfo.LeftBorder := ABorder;
  finally
    ABorder.Free;
  end;
  AParagraphInfo.FormattingOptions.UseLeftBorder := True;
end;

{ TdxDocCommandParagraphBottomBorderNew }

procedure TdxDocCommandParagraphBottomBorderNew.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AParagraphInfo: TdxParagraphInfo;
  ABorder: TdxBorderInfo;
begin
  AParagraphInfo := APropertyContainer.ParagraphInfo;
  ABorder := TdxBorderInfo.Create;
  try
    ApplyBorderProperties(ABorder, APropertyContainer.UnitConverter);
    AParagraphInfo.FormattingInfo.BottomBorder := ABorder;
  finally
    ABorder.Free;
  end;
  AParagraphInfo.FormattingOptions.UseBottomBorder := True;
end;

{ TdxDocCommandParagraphRightBorderNew }

procedure TdxDocCommandParagraphRightBorderNew.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AParagraphInfo: TdxParagraphInfo;
  ABorder: TdxBorderInfo;
begin
  AParagraphInfo := APropertyContainer.ParagraphInfo;
  ABorder := TdxBorderInfo.Create;
  try
    ApplyBorderProperties(ABorder, APropertyContainer.UnitConverter);
    AParagraphInfo.FormattingInfo.RightBorder := ABorder;
  finally
    ABorder.Free;
  end;
  AParagraphInfo.FormattingOptions.UseRightBorder := True;
end;

{ TdxTableDefinitionOperand }

constructor TdxTableDefinitionOperand.Create;
begin
  FPositions := TList<SmallInt>.Create;
  FCells := TdxTableCellDescriptorList.Create;
end;

destructor TdxTableDefinitionOperand.Destroy;
begin
  FPositions.Free;
  FCells.Free;
  inherited Destroy;
end;

class function TdxTableDefinitionOperand.FromByteArray(const AData: TBytes): TdxTableDefinitionOperand;
begin
  Result := TdxTableDefinitionOperand.Create;
  Result.Read(AData);
end;

procedure TdxTableDefinitionOperand.Read(const AData: TBytes);
var
  ACount, I, ACurrentPosition: Integer;
begin
  Assert(AData <> nil, 'data');
  FColumnsCount := AData[0];
  ACount := FColumnsCount + 1;
  for I := 0 to ACount - 1 do
    FPositions.Add(PSmallInt(@AData[I * 2 + 1])^);

  ACurrentPosition := 1 + (FColumnsCount + 1) * 2;
  ACount := (Length(AData) - ACurrentPosition) div TdxTableCellDescriptor.Size;
  for I := 0 to ACount - 1 do
    FCells.Add(TdxTableCellDescriptor.FromByteArray(AData, ACurrentPosition + I * TdxTableCellDescriptor.Size));
end;

function TdxTableDefinitionOperand.Clone: TdxTableDefinitionOperand;
var
  ACount, I: Integer;
begin
  Result := TdxTableDefinitionOperand.Create;
  Result.FColumnsCount := FColumnsCount;
  ACount := FPositions.Count;
  for I := 0 to ACount - 1 do
    Result.FPositions.Add(FPositions[I]);

  ACount := FCells.Count;
  for I := 0 to ACount - 1 do
    Result.FCells.Add(FCells[I].Clone);
end;

{ TLP }

class constructor TLP.Initialize;
begin
  FKnownStyles := GetKnownStyles;
end;

class destructor TLP.Finalize;
begin
  FKnownStyles.Free;
end;

constructor TLP.Create;
begin
  FItl := -1;
end;

function TLP.Clone: TLP;
begin
  if Self = nil then
    Exit(nil);
  Result := TLP.Create;
  Result.FItl := FItl;
  Result.FGrfatl := FGrfatl;
end;

class function TLP.GetKnownStyles: TDictionary<SmallInt, string>;
begin
  Result := TDictionary<SmallInt, string>.Create;
  Result.Add(0, TdxKnownStyleNames.NormalTable);
  Result.Add(1, TdxKnownStyleNames.TableSimple1);
  Result.Add(2, TdxKnownStyleNames.TableSimple2);
  Result.Add(3, TdxKnownStyleNames.TableSimple3);
  Result.Add(4, TdxKnownStyleNames.TableClassic1);
  Result.Add(5, TdxKnownStyleNames.TableClassic2);
  Result.Add(6, TdxKnownStyleNames.TableClassic3);
  Result.Add(7, TdxKnownStyleNames.TableClassic4);
  Result.Add(8, TdxKnownStyleNames.TableColorful1);
  Result.Add(9, TdxKnownStyleNames.TableColorful2);
  Result.Add(10, TdxKnownStyleNames.TableColorful3);
  Result.Add(11, TdxKnownStyleNames.TableColumns1);
  Result.Add(12, TdxKnownStyleNames.TableColumns2);
  Result.Add(13, TdxKnownStyleNames.TableColumns3);
  Result.Add(14, TdxKnownStyleNames.TableColumns4);
  Result.Add(15, TdxKnownStyleNames.TableColumns5);
  Result.Add(16, TdxKnownStyleNames.TableGrid1);
  Result.Add(17, TdxKnownStyleNames.TableGrid2);
  Result.Add(18, TdxKnownStyleNames.TableGrid3);
  Result.Add(19, TdxKnownStyleNames.TableGrid4);
  Result.Add(20, TdxKnownStyleNames.TableGrid5);
  Result.Add(21, TdxKnownStyleNames.TableGrid6);
  Result.Add(22, TdxKnownStyleNames.TableGrid7);
  Result.Add(23, TdxKnownStyleNames.TableGrid8);
  Result.Add(24, TdxKnownStyleNames.TableList1);
  Result.Add(25, TdxKnownStyleNames.TableList2);
  Result.Add(26, TdxKnownStyleNames.TableList3);
  Result.Add(27, TdxKnownStyleNames.TableList4);
  Result.Add(28, TdxKnownStyleNames.TableList5);
  Result.Add(29, TdxKnownStyleNames.TableList6);
  Result.Add(30, TdxKnownStyleNames.TableList7);
  Result.Add(31, TdxKnownStyleNames.TableList8);
  Result.Add(32, TdxKnownStyleNames.Table3DEffects1);
  Result.Add(33, TdxKnownStyleNames.Table3DEffects2);
  Result.Add(34, TdxKnownStyleNames.Table3DEffects3);
  Result.Add(35, TdxKnownStyleNames.TableContemporary);
  Result.Add(36, TdxKnownStyleNames.TableElegant);
  Result.Add(37, TdxKnownStyleNames.TableProfessional);
  Result.Add(38, TdxKnownStyleNames.TableSubtle1);
  Result.Add(39, TdxKnownStyleNames.TableSubtle2);
end;

class function TLP.FromByteArray(const AData: TBytes): TLP;
begin
  Result := TLP.Create;
  Result.Read(AData);
end;

procedure TLP.Read(const AData: TBytes);
begin
  Assert(AData <> nil, 'data');
  FItl    := PSmallInt(@AData[0])^;
  FGrfatl := PSmallInt(@AData[2])^;
end;

procedure TLP.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(FItl);
  AWriter.Write(FGrfatl);
end;

procedure TLP.ApplyProperties(AProperties: TdxTableProperties);
var
  ATableStyle: TdxTableStyle;
begin
  ATableStyle := GetTableStyle(TdxDocumentModel(AProperties.DocumentModel));
  if ATableStyle = nil then
    Exit;
  ApplyPropertiesCore(ATableStyle, AProperties);
end;

function TLP.GetTableStyle(ADocumentModel: TdxDocumentModel): TdxTableStyle;
var
  AStyleName: string;
begin
  if not FKnownStyles.TryGetValue(FItl, AStyleName) then
    Exit(nil);
  Result := TdxTableStyle(ADocumentModel.TableStyles.GetStyleByName(AStyleName));
end;

procedure TLP.ApplyPropertiesCore(AStyle: TdxTableStyle; AProperties: TdxTableProperties);
begin
  AProperties.BeginInit;
  if (FGrfatl and TdxFatl.UseBorders) = TdxFatl.UseBorders then
    AProperties.Borders.CopyFrom(AStyle.TableProperties.Borders);
  AProperties.EndInit;
end;

{ TdxCellHideMarkOperand }

class function TdxCellHideMarkOperand.FromByteArray(const AData: TBytes): TdxCellHideMarkOperand;
begin
  Result := TdxCellHideMarkOperand.Create;
  Result.Read(AData);
end;

procedure TdxCellHideMarkOperand.Read(const AData: TBytes);
begin
  FStartIndex := AData[0];
  FEndIndex := Byte(AData[1] - 1);
  FHideCellMark := AData[2] = 1;
end;

procedure TdxCellHideMarkOperand.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(FStartIndex);
  AWriter.Write(Byte(FEndIndex + 1));
  AWriter.Write(FHideCellMark);
end;

function TdxCellHideMarkOperand.Clone: TdxCellHideMarkOperand;
begin
  Result := TdxCellHideMarkOperand.Create;
  Result.FStartIndex := FStartIndex;
  Result.FEndIndex := FEndIndex;
  Result.FHideCellMark := FHideCellMark;
end;

{ TdxDocCommandFactoryBase }

constructor TdxDocCommandFactoryBase.Create(AModel: TdxDocumentModel);
begin
  Assert(AModel <> nil, 'model');
  FModel := AModel;
end;

function TdxDocCommandFactoryBase.CreatePropertyContainer(AChangeType: TdxChangeActionTypes): TdxDocPropertyContainer;
begin
  Result := TdxDocPropertyContainer.Create(Self, FModel.UnitConverter);
  UpdatePropertyContainer(Result, AChangeType);
end;

class function TdxDocCommandFactoryBase.GetOpcodeByType(ACommandType: TClass): SmallInt;
begin
  Result := -1;
end;

function TdxDocCommandFactoryBase.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := FModel.UnitConverter;
end;

{ TdxDocPropertyContainer }

constructor TdxDocPropertyContainer.Create(AFactory: TdxDocCommandFactoryBase; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  Assert(AFactory <> nil, 'factory');
  Factory := AFactory;
  UnitConverter := AUnitConverter;
end;

destructor TdxDocPropertyContainer.Destroy;
begin
  FCharacterInfo.Free;
  FParagraphInfo.Free;
  FFrameInfo.Free;
  FSectionInfo.Free;
  FTableInfo.Free;
  FTableRowInfo.Free;
  FTableCellInfo.Free;
  inherited Destroy;
end;

procedure TdxDocPropertyContainer.Update(AChangeType: TdxChangeActionTypes);
begin
  Factory.UpdatePropertyContainer(Self, AChangeType);
end;

end.
