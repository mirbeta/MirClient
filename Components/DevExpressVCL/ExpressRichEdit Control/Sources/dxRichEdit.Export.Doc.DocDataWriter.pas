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
unit dxRichEdit.Export.Doc.DocDataWriter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Platform.Font,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentFormats.DocumentFormatUtils,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.Import.Doc.OfficeArtContent,
  dxRichEdit.Import.Doc.FileInformationBlock,
  dxRichEdit.Export.Doc.FKPWriter,
  dxRichEdit.Import.Doc.DocBookmark,
  dxRichEdit.Import.Doc.DocCharacterFormattingInfo,
  dxRichEdit.Import.Doc.DocStyles,
  dxRichEdit.Import.Doc.DocStyleSheet,
  dxRichEdit.Import.Doc.ListFormatInformation,
  dxRichEdit.Import.Doc.ListFormatOverride,
  dxRichEdit.Import.Doc.DocContentIterator,
  dxRichEdit.Import.Doc.BreakDescriptor,
  dxRichEdit.Import.Doc.FileShapeAddress,
  dxRichEdit.Import.Doc.DocumentFileRecords,
  dxRichEdit.Import.Doc.DocHeadersFooters,
  dxRichEdit.Export.Doc.SectionPropertiesWriter,
  dxRichEdit.Import.Doc.DocRangeEditPermission;

type

  { TdxDocStyleIndexes }

  TdxDocStyleIndexes = class
  public const
    DefaultParagraphStyleIndex = Integer($0000);
    DefaultCharacterStyleIndex = Integer($000a);
    DefaultTableStyleIndex     = Integer($000b);
    DefaultListStyleIndex      = Integer($000c);
  end;

  { TdxTextBoxReusableInfo }

  TdxTextBoxReusableInfo = record
  private
    FNextItem: Integer;
    FReusableChainLength: Integer;
  public
    constructor Create(ANextItem: Integer; AReusableChainLength: Integer);
    procedure Write(AWriter: TBinaryWriter);
    class function NonReusable: TdxTextBoxReusableInfo; static; inline;
    class function LastInfoInChain: TdxTextBoxReusableInfo; static; inline;

    property NextItem: Integer read FNextItem write FNextItem;
    property ReusableChainLength: Integer read FReusableChainLength write FReusableChainLength;
  end;

  { TdxFileTextBoxIdentifier }

  TdxFileTextBoxIdentifier = class
  public const
    ItxbxsDest = Integer(-1);
    TxidUndo   = Integer(0);
  strict private
    FReusableInfo: TdxTextBoxReusableInfo;
    FIsReusable: Boolean;
    FShapeIdentifier: Integer;
  public
    constructor Create(AShapeIdentifier: Integer; AIsLastInChain: Boolean);
    procedure Write(AWriter: TBinaryWriter);

    property ReusableInfo: TdxTextBoxReusableInfo read FReusableInfo;
    property IsReusable: Boolean read FIsReusable write FIsReusable;
    property ShapeIdentifier: Integer read FShapeIdentifier write FShapeIdentifier;
  end;

  { TdxTextBoxesExporterState }

  TdxTextBoxesExporterState = class
  strict private
    FTextBoxes: TDictionary<Integer, TdxTextBoxContentType>;
    FPositions: TdxIntegerList;
    FTextBoxesIdentifiers: TdxObjectList<TdxFileTextBoxIdentifier>;
    FBreakDescriptors: TdxBreakDescriptorTable;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterTextBoxFloatingObject(AContent: TdxTextBoxFloatingObjectContent; AShapeIdentifier: Integer): Integer;
    procedure AddTextBoxTableEntry(ACharacterPosition: Integer; AShapeIdentifier: Integer; AIsLastInChain: Boolean);
    procedure AddTextBoxPosition(APosition: Integer);
    procedure AddTextBoxReusableInfo(AShapeIdentifier: Integer; AIsLastInChain: Boolean);
    procedure AddBreakDescriptor(APosition: Integer);
    procedure FinishState(ALastCharacterPosition: Integer);
    procedure FinishDocument(ALastCharacterPosition: Integer);

    property TextBoxes: TDictionary<Integer, TdxTextBoxContentType> read FTextBoxes;
    property Positions: TdxIntegerList read FPositions;
    property TextBoxesIdentifiers: TdxObjectList<TdxFileTextBoxIdentifier> read FTextBoxesIdentifiers;
    property BreakDescriptors: TdxBreakDescriptorTable read FBreakDescriptors;
  end;

  { TdxTextBoxesExporter }

  TdxTextBoxesExporter = class
  strict private
    FState: TdxDocContentState;
    FMainDocument: TdxTextBoxesExporterState;
    FHeaderDocument: TdxTextBoxesExporterState;
    FActiveState: TdxTextBoxesExporterState;
    procedure SetState(const AValue: TdxDocContentState);
  protected
    property MainDocument: TdxTextBoxesExporterState read FMainDocument;
    property HeaderDocument: TdxTextBoxesExporterState read FHeaderDocument;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterTextBoxFloatingObject(AContent: TdxTextBoxFloatingObjectContent; AShapeIdentifier: Integer): Integer;
    procedure AddTextBoxTableEntry(ACharacterPosition: Integer; AShapeIdentifier: Integer; AIsLastInChain: Boolean);
    function GetCurrentTextBoxes: TDictionary<Integer, TdxTextBoxContentType>;
    procedure ExportTextBoxesTables(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    function ShouldInsertEmptyParagraph: Boolean;
    procedure ExportMainDocumentTextBoxesTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure ExportHeaderTextBoxesTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure FinishCurrentState(ACharacterPosition: Integer);
    procedure FinishDocument(ALastCharacterPosition: Integer);

    property State: TdxDocContentState read FState write SetState;
    property ActiveExporterState: TdxTextBoxesExporterState read FActiveState;
  end;

  { TdxFileShapeAddressesExporter }

  TdxFileShapeAddressesExporter = class
  strict private
    FUnitConverter: TdxDocumentModelUnitConverter;
    FState: TdxDocContentState;
    FMainDocumentAddresses: TdxFileShapeAddressTable;
    FHeaderAddresses: TdxFileShapeAddressTable;
  protected
    procedure ExportFloatingObjectProperties(AFloatingObjectProperties: TdxFloatingObjectProperties; ACharacterPosition: Integer; AShapeIdentifier: Integer); virtual;
    procedure ExportFileShapeAddresses(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter); virtual;
    procedure SetFileShapeAddressProperties(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
    procedure SetLocked(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
    procedure SetHorizontalPositionType(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
    procedure SetVerticalPositionType(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
    procedure SetTextWrapSide(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
    procedure SetTextWrapType(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
    procedure SetIsBehindText(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
    procedure SetLeft(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
    procedure SetRight(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
    procedure SetTop(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
    procedure SetBottom(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);

    property State: TdxDocContentState read FState write FState;
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
    property MainDocumentAddresses: TdxFileShapeAddressTable read FMainDocumentAddresses;
    property HeaderAddresses: TdxFileShapeAddressTable read FHeaderAddresses;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitConverter);
    destructor Destroy; override;
    function CreateShapeIdentifier(ARun: TdxFloatingObjectAnchorRun; ACharacterPosition: Integer): Integer;
    procedure Finish(ALastCharacterPosition: Integer);
    function CalcShapeIdentifier: Integer;
  end;

  { TdxDocFloatingObjectsExporter }

  TdxDocFloatingObjectsExporter = class
  strict private
    FState: TdxDocContentState;
    FArtContent: TdxOfficeArtContent;
    FShapeAddressesExporter: TdxFileShapeAddressesExporter;
    FTextBoxesExporter: TdxTextBoxesExporter;
  protected
    procedure SetState(AState: TdxDocContentState);
    procedure ExportOfficeArtContent(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter); virtual;
    procedure PrepareOfficeArtContentForExport;

    property State: TdxDocContentState read FState write SetState;
    property ShapeAddressesExporter: TdxFileShapeAddressesExporter read FShapeAddressesExporter;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitConverter; APageBackColor: TdxAlphaColor);
    destructor Destroy; override;
    procedure RegisterFloatingObject(ARun: TdxFloatingObjectAnchorRun; ACharacterPosition: Integer);
    procedure RegisterFloatingObjectCore(ARun: TdxFloatingObjectAnchorRun; ACharacterPosition: Integer; AShapeIdentifier: Integer);
    procedure RegisterPictureFloatingObject(AImage: TdxOfficeImageReference; AShapeIdentifier: Integer; ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure RegisterTextBoxFloatingObject(AContent: TdxTextBoxFloatingObjectContent; AShapeIdentifier: Integer; ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure ExportFloatingObjectsInfo(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);
    procedure Finish(ALastCharacterPosition: Integer);

    property OfficeArtContent: TdxOfficeArtContent read FArtContent;
    property TextBoxesExporter: TdxTextBoxesExporter read FTextBoxesExporter;
  end;

  { TdxDocNotesExporter }

  TdxDocNotesExporter = class
  strict private
    FFootNoteReferences: TdxIntegerList;
    FFootNotePositions: TdxIntegerList;
    FEndNoteReferences: TdxIntegerList;
    FEndNotePositions: TdxIntegerList;
    FFootNoteFlags: TBits;
    FEndNoteFlags: TBits;
  protected
    property FootNoteReferences: TdxIntegerList read FFootNoteReferences;
    property EndNoteReferences: TdxIntegerList read FEndNoteReferences;
    property FootNoteFlags: TBits read FFootNoteFlags;
    property EndNoteFlags: TBits read FEndNoteFlags;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FinishFootNotePositions(ALastFootNoteEnd: Integer; ALastPosition: Integer);
    procedure FinishEndNotePositions(ALastEndNoteEnd: Integer);
    procedure AddFootNoteReferenceEntry(ACharacterPosition: Integer; AIsAutoNumbered: Boolean);
    procedure AddEndNoteReferenceEntry(ACharacterPosition: Integer; AIsAutoNumbered: Boolean);
    procedure FinishFootNoteReferences(ALastCharacterPosition: Integer);
    procedure FinishEndNoteReferences(ALastCharacterPosition: Integer);
    procedure ExportFootNoteTables(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure ExportEndNoteTables(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure ExportNoteReferences(AWriter: TBinaryWriter; AReferences: TdxIntegerList; AFlags: TBits);
    procedure ExportNotePositions(AWriter: TBinaryWriter; APositions: TdxIntegerList);

    property FootNotePositions: TdxIntegerList read FFootNotePositions;
    property EndNotePositions: TdxIntegerList read FEndNotePositions;
  end;

  { TdxDocTablesExporter }

  TdxDocTablesExporter = class
  strict private
    FCurrentTableDepth: Integer;
    FGrids: TObjectStack<TdxTableGrid>;
    function GetInTable: Boolean;
  protected
    property Grids: TObjectStack<TdxTableGrid> read FGrids;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AdvanceNext(AInfo: TdxTableInfo);
    procedure FinishTable;
    function GetTableUnitMark: string;
    function GetTableCellPropertyModifiers(AParagraph: TdxParagraph): TBytes;
    function GetTableRowPropertyModifiers(ARow: TdxTableRow; ATableStyleIndex: Integer): TBytes;
    function GetParentSectionWidthInModelUnits(ATable: TdxTable): Integer;

    property InTable: Boolean read GetInTable;
    property TableDepth: Integer read FCurrentTableDepth;
  end;

  { TdxDocFieldsExporter }

  TdxDocFieldsExporter = class
  strict private
    FMainDocumentFieldTable: TdxDocFieldTable;
    FFootNotesFieldTable: TdxDocFieldTable;
    FHeadersFootersFieldTable: TdxDocFieldTable;
    FCommentsFieldTable: TdxDocFieldTable;
    FEndNotesFieldTable: TdxDocFieldTable;
    FTextBoxesFieldTable: TdxDocFieldTable;
    FHeaderTextBoxesFieldTable: TdxDocFieldTable;
    FState: TdxDocContentState;
  protected
    property State: TdxDocContentState read FState write FState;
  public
    destructor Destroy; override;
    function GetFieldTable: TdxDocFieldTable;
    procedure Finish(ALastCharacterPosition: Integer);
    procedure ExportFieldTables(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure ExportMainDocumentFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure ExportHeadersFootersFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure ExportFootNotesFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure ExportCommentsFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure ExportEndNotesFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure ExportTextBoxesFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    procedure ExportHeaderTextBoxesFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
  end;

  { TdxFontsExportHelper }

  TdxFontsExportHelper = class
  strict private
    FDefaultFontIndex: Integer;
    FFontNamesCollection: TdxStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFontIndexByName(const AFontName: string): Integer;
    function GetFontNameByIndex(AIndex: Integer): string;

    property FontNamesCollection: TdxStringList read FFontNamesCollection;
    property DefaultFontIndex: Integer read FDefaultFontIndex;
  end;

  { TdxDocStylesExporter }

  TdxDocStylesExporter = class
  public const
    DefaultParagraphStyleIdentifier = Integer($0000);
    DefaultCharacterStyleIdentifier = Integer($0041);
    DefaultTableStyleIdentifier     = Integer($0069);
    DefaultListStyleIdentifier      = Integer($006b);
    DefaultParagraphStyleName       = 'Normal';
    DefaultCharacterStyleName       = 'Default Paragraph Font';
    DefaultListStyleName            = 'No List';
    DefaultTableStyleName           = 'Table Normal';
  strict private
    class var
      FHeadingStyles: TDictionary<Word, string>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FFontsExportHelper: TdxFontsExportHelper;
    FDocumentModel: TdxDocumentModel;
    FStyleNamesCollection: TDictionary<string, Integer>;
    FStyleSheet: TdxDocStyleSheet;
    function GetFontsCount: Integer;
    function GetParagraphStyleInfoForExport(ASourceStyle: TdxParagraphStyle): TdxDocParagraphStyleInfo;
  protected
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property StyleSheet: TdxDocStyleSheet read FStyleSheet;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;
    function GetParagraphStyleIndex(ADocumentModelParagraphIndex: Integer): Integer;
    function GetCharacterStyleIndex(ADocumentModelCharacterStyleIndex: Integer): Integer;
    function GetNumberingStyleIndex(ADocumentModelNumberingStyleIndex: Integer): Integer;
    function GetStyleIndexByName(const AStyleName: string): Integer;
    function GetFontIndexByName(const AFontName: string): Integer;
    function GetFontNameByIndex(AFontNameIndex: Integer): string;
    function GetCharacterGroupPropertyModifiers(AListLevel: TdxListLevel; ACharacterStyleIndex: Integer): TBytes; overload;
    function GetCharacterGroupPropertyModifiers(ARun: TdxTextRunBase; ASpecialSymbol: Boolean): TBytes; overload;
    function GetInlinePicturePropertyModifiers(ACharacterProperties: TdxCharacterProperties;
      ACharacterStyleIndex: Integer; ADataStreamOffset: Integer): TBytes;
    function GetParagraphGroupPropertyModifiers(AParagraph: TdxParagraph): TBytes; overload;
    function GetParagraphGroupPropertyModifiers(AListLevel: TdxListLevel): TBytes; overload;
    function GetTableParagraphPropertyModifiers(AParagraph: TdxParagraph; ATableDepth: Integer): TBytes;
    procedure CreateStyleSheet;
    procedure WriteStyleSheet(AWriter: TBinaryWriter);
    procedure SetStyleSheetInformation(AStyleSheet: TdxDocStyleSheet);
    procedure AddDefaultStyles(AStyleSheet: TdxDocStyleSheet);
    procedure AddDefaultParagraphStyle(AStyleSheet: TdxDocStyleSheet);
    procedure AddHeadingStyles(AStyleSheet: TdxDocStyleSheet);
    procedure AddDefaultCharacterStyle(AStyleSheet: TdxDocStyleSheet);
    procedure AddDefaultTableStyle(AStyleSheet: TdxDocStyleSheet);
    procedure AddDefaultListStyle(AStyleSheet: TdxDocStyleSheet);
    procedure AddParagraphStyles(AStyleSheet: TdxDocStyleSheet);
    procedure SetNextParagraphStyles;
    procedure AddCharacterStyles(AStyleSheet: TdxDocStyleSheet);
    procedure AddListStyles(AStyleSheet: TdxDocStyleSheet);
    procedure AddTableStyles(AStyleSheet: TdxDocStyleSheet);

    property FontsExportHelper: TdxFontsExportHelper read FFontsExportHelper;
    property StyleNamesCollection: TDictionary<string, Integer> read FStyleNamesCollection;
    property FontsCount: Integer read GetFontsCount;
  end;

  { TdxDocListsExporter }

  TdxDocListsExporter = class
  public const
    EmptyStyleIdentifier   = Integer($0fff);
    EmptyCharacterPosition = Integer($ffffffff);
  strict private
    FDocumentModel: TdxDocumentModel;
    FOptions: TdxDocDocumentExporterOptions;
    FStylesExportHelper: TdxDocStylesExporter;
    FListInfo: TdxDocListFormatInformation;
    FListOverrideInfo: TdxDocListOverrideFormatInformation;
    FListStyles: TList<TdxListStylesRecordItem>;
  protected
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property Options: TdxDocDocumentExporterOptions read FOptions;
    property StylesExportHelper: TdxDocStylesExporter read FStylesExportHelper;
    property ListInfo: TdxDocListFormatInformation read FListInfo;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; AOptions: TdxDocDocumentExporterOptions; AStylesExportHelper: TdxDocStylesExporter);
    destructor Destroy; override;
    procedure WriteListFormatInformation(AWriter: TBinaryWriter);
    procedure WriteListOverrideFormatInformation(AWriter: TBinaryWriter);
    procedure CreateLists(AStylesExporter: TdxDocStylesExporter);
    function CreateListInfo(AStylesExporter: TdxDocStylesExporter): TdxDocListFormatInformation;
    function CreateDocListData(AList: TdxAbstractNumberingList): TdxDocListData;
    function CreateListFormatting(AList: TdxAbstractNumberingList): TdxDocListFormatting;
    function CreateDocListLevel(AListLevel: TdxListLevel; AListLevelIndex: Integer): TdxDocListLevel;
    function CreateListLevelProperties(AListLevelProperties: TdxListLevelProperties; AListLevelIndex: Integer): TdxDocListLevelProperties;
    function CreateListOverrideInfo: TdxDocListOverrideFormatInformation;
    function CreateDocListOverride(AList: TdxNumberingList; AOverrideLevelCount: Integer): TdxDocListOverrideFormat;
    function CreateDocListOverrideData(AList: TdxNumberingList): TdxDocListOverrideLevelInformation;
    function CreateOverrideLevel(const AListLevel: IdxOverrideListLevel; ALevelIndex: Integer): TdxDocListOverrideLevelFormat;

    property ListStyles: TList<TdxListStylesRecordItem> read FListStyles;
    property ListOverrideInfo: TdxDocListOverrideFormatInformation read FListOverrideInfo;
  end;

  { TdxDocDataWriter }

  TdxDocDataWriter = class
  strict private
    FFkpMemoryStream: TdxMemoryStream;
    FSepxMemoryStream: TdxMemoryStream;
    FFkpWriter: TdxFKPWriter;
    FSepxWriter: TdxSectionPropertiesWriter;
    FStylesExporter: TdxDocStylesExporter;
    FFieldsExporter: TdxDocFieldsExporter;
    FListsExporter: TdxDocListsExporter;
    FTablesExporter: TdxDocTablesExporter;
    FNotesExporter: TdxDocNotesExporter;
    FFloatingObjectsExporter: TdxDocFloatingObjectsExporter;
    FBookmarkIterator: TdxDocBookmarkIterator;
    FPermissionIterator: TdxDocRangeEditPermissionIterator;
    function GetFieldTable: TdxDocFieldTable;
  protected

    property FormattedDiskPageWriter: TdxFKPWriter read FFkpWriter;
    property SectionPropertiesWriter: TdxSectionPropertiesWriter read FSepxWriter;
  public
    constructor Create(ADataStreamWriter: TBinaryWriter; AOptions: TdxDocDocumentExporterOptions; ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;
    procedure SetState(AState: TdxDocContentState); virtual;
    procedure WriteSection(ACharacterPosition: Integer; ASection: TdxSection);
    procedure WriteSectionPositions(AWriter: TBinaryWriter);
    procedure UpdateSectionsOffsets(AOffset: Integer);
    procedure WriteCharactersBinTable(AWriter: TBinaryWriter);
    procedure WriteParagraphsBinTable(AWriter: TBinaryWriter);
    procedure WriteParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer); overload;
    procedure WriteParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer; AParagraph: TdxParagraph); overload;
    procedure WriteInTableParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer; AParagraph: TdxParagraph);
    procedure WriteParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer; ARow: TdxTableRow); overload;
    procedure WriteTextRun(ACharacterPosition: Integer); overload;
    procedure WriteTextRun(ACharacterPosition: Integer; ATextRun: TdxTextRunBase; ASpecialSymbol: Boolean); overload;
    procedure WriteInlinePictureRun(ACharacterPosition, ACharacterStyleIndex, ADataStreamOffset: Integer; ACharacterProperties: TdxCharacterProperties);
    procedure Finish(ALastCharacterPosition: Integer);
    procedure ExportFieldTables(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
    function GetFormattedDiskPages: TBytes;
    function GetSectionProperties: TBytes;

    property StylesExporter: TdxDocStylesExporter read FStylesExporter;
    property ListsExporter: TdxDocListsExporter read FListsExporter;
    property TablesExporter: TdxDocTablesExporter read FTablesExporter;
    property FieldsExporter: TdxDocFieldsExporter read FFieldsExporter;
    property BookmarkIterator: TdxDocBookmarkIterator read FBookmarkIterator;
    property PermissionIterator: TdxDocRangeEditPermissionIterator read FPermissionIterator;
    property FieldTable: TdxDocFieldTable read GetFieldTable;
    property NotesExporter: TdxDocNotesExporter read FNotesExporter;
    property FloatingObjectsExporter: TdxDocFloatingObjectsExporter read FFloatingObjectsExporter;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.Export.Doc.DocTableActions,
  dxRichEdit.Export.Doc.DocCharacterPropertiesActions,
  dxRichEdit.Export.Doc.DocParagraphPropertiesActions,
  dxRichEdit.Doc.Utils;

{ TdxTextBoxReusableInfo }

class function TdxTextBoxReusableInfo.LastInfoInChain: TdxTextBoxReusableInfo;
begin
  Result := TdxTextBoxReusableInfo.Create(-1, 0);
end;

class function TdxTextBoxReusableInfo.NonReusable: TdxTextBoxReusableInfo;
begin
  Result := TdxTextBoxReusableInfo.Create(1, 0);
end;

constructor TdxTextBoxReusableInfo.Create(ANextItem: Integer; AReusableChainLength: Integer);
begin
  FNextItem := ANextItem;
  FReusableChainLength := AReusableChainLength;
end;

procedure TdxTextBoxReusableInfo.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(NextItem);
  AWriter.Write(ReusableChainLength);
end;

{ TdxFileTextBoxIdentifier }

constructor TdxFileTextBoxIdentifier.Create(AShapeIdentifier: Integer; AIsLastInChain: Boolean);
begin
  FIsReusable := False;
  FShapeIdentifier := AShapeIdentifier;
  if AIsLastInChain then
    FReusableInfo := TdxTextBoxReusableInfo.LastInfoInChain
  else
    FReusableInfo := TdxTextBoxReusableInfo.NonReusable;
end;

procedure TdxFileTextBoxIdentifier.Write(AWriter: TBinaryWriter);
begin
  ReusableInfo.Write(AWriter);
  AWriter.Write(Word(Ord(FIsReusable)));
  AWriter.Write(ItxbxsDest);
  AWriter.Write(FShapeIdentifier);
  AWriter.Write(TxidUndo);
end;

{ TdxTextBoxesExporterState }

constructor TdxTextBoxesExporterState.Create;
begin
  FTextBoxes := TDictionary<Integer, TdxTextBoxContentType>.Create;
  FPositions := TdxIntegerList.Create;
  FTextBoxesIdentifiers := TdxObjectList<TdxFileTextBoxIdentifier>.Create;
  FBreakDescriptors := TdxBreakDescriptorTable.Create;
end;

destructor TdxTextBoxesExporterState.Destroy;
begin
  FTextBoxes.Free;
  FPositions.Free;
  FTextBoxesIdentifiers.Free;
  FBreakDescriptors.Free;
  inherited Destroy;
end;

function TdxTextBoxesExporterState.RegisterTextBoxFloatingObject(AContent: TdxTextBoxFloatingObjectContent; AShapeIdentifier: Integer): Integer;
begin
  if not TextBoxes.ContainsKey(AShapeIdentifier) then
  begin
    TextBoxes.Add(AShapeIdentifier, AContent.TextBox);
    Exit(TextBoxes.Count);
  end;
  TdxRichEditExceptions.ThrowInternalException;
  Result := -1;
end;

procedure TdxTextBoxesExporterState.AddTextBoxTableEntry(ACharacterPosition: Integer; AShapeIdentifier: Integer; AIsLastInChain: Boolean);
begin
  AddTextBoxPosition(ACharacterPosition);
  AddTextBoxReusableInfo(AShapeIdentifier, AIsLastInChain);
  AddBreakDescriptor(ACharacterPosition);
end;

procedure TdxTextBoxesExporterState.AddTextBoxPosition(APosition: Integer);
begin
  Positions.Add(APosition);
end;

procedure TdxTextBoxesExporterState.AddTextBoxReusableInfo(AShapeIdentifier: Integer; AIsLastInChain: Boolean);
begin
  TextBoxesIdentifiers.Add(TdxFileTextBoxIdentifier.Create(AShapeIdentifier, AIsLastInChain));
end;

procedure TdxTextBoxesExporterState.AddBreakDescriptor(APosition: Integer);
begin
  BreakDescriptors.AddEntry(APosition);
end;

procedure TdxTextBoxesExporterState.FinishState(ALastCharacterPosition: Integer);
begin
  AddTextBoxPosition(ALastCharacterPosition);
  AddTextBoxReusableInfo(0, True);
  BreakDescriptors.Finish(ALastCharacterPosition);
end;

procedure TdxTextBoxesExporterState.FinishDocument(ALastCharacterPosition: Integer);
begin
  if Positions.Count > 0 then
  begin
    Positions.Add(ALastCharacterPosition);
    BreakDescriptors.CharacterPositions.Add(ALastCharacterPosition);
  end;
end;

{ TdxTextBoxesExporter }

constructor TdxTextBoxesExporter.Create;
begin
  FMainDocument := TdxTextBoxesExporterState.Create;
  FHeaderDocument := TdxTextBoxesExporterState.Create;
end;

destructor TdxTextBoxesExporter.Destroy;
begin
  FMainDocument.Free;
  FHeaderDocument.Free;
  inherited Destroy;
end;

procedure TdxTextBoxesExporter.SetState(const AValue: TdxDocContentState);
begin
  if AValue = TdxDocContentState.TextBoxes then
    FActiveState := FMainDocument
  else
    if AValue = TdxDocContentState.HeaderTextBoxes then
      FActiveState := FHeaderDocument
    else
      FActiveState := nil;
  FState := AValue;
end;

function TdxTextBoxesExporter.RegisterTextBoxFloatingObject(AContent: TdxTextBoxFloatingObjectContent; AShapeIdentifier: Integer): Integer;
var
  AExporterState: TdxTextBoxesExporterState;
begin
  if State = TdxDocContentState.MainDocument then
    AExporterState := MainDocument
  else
    if State = TdxDocContentState.HeadersFootersStory then
      AExporterState := HeaderDocument
    else
    begin
      TdxRichEditExceptions.ThrowInternalException;
      Exit(-1);
    end;
  Result := AExporterState.RegisterTextBoxFloatingObject(AContent, AShapeIdentifier);
end;

procedure TdxTextBoxesExporter.AddTextBoxTableEntry(ACharacterPosition: Integer; AShapeIdentifier: Integer; AIsLastInChain: Boolean);
begin
  ActiveExporterState.AddTextBoxTableEntry(ACharacterPosition, AShapeIdentifier, AIsLastInChain);
end;

function TdxTextBoxesExporter.GetCurrentTextBoxes: TDictionary<Integer, TdxTextBoxContentType>;
begin
  if ActiveExporterState <> nil then
    Result := ActiveExporterState.TextBoxes
  else
    Result := nil;
end;

procedure TdxTextBoxesExporter.ExportTextBoxesTables(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  ExportMainDocumentTextBoxesTable(AFib, AWriter);
  ExportHeaderTextBoxesTable(AFib, AWriter);
end;

function TdxTextBoxesExporter.ShouldInsertEmptyParagraph: Boolean;
begin
  Result := (ActiveExporterState <> nil) and (ActiveExporterState.TextBoxes.Count > 0);
end;

procedure TdxTextBoxesExporter.ExportMainDocumentTextBoxesTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  AFib.MainDocumentTextBoxesTextOffset := Integer(AWriter.BaseStream.Position);
  ACount := MainDocument.Positions.Count;
  for I := 0 to ACount - 1 do
    AWriter.Write(MainDocument.Positions[I]);
  ACount := MainDocument.TextBoxesIdentifiers.Count;
  for I := 0 to ACount - 1 do
    MainDocument.TextBoxesIdentifiers[I].Write(AWriter);
  AFib.MainDocumentTextBoxesTextSize := Integer((AWriter.BaseStream.Position - AFib.MainDocumentTextBoxesTextOffset));
  AFib.MainTextBoxBreakTableOffset := Integer(AWriter.BaseStream.Position);
  MainDocument.BreakDescriptors.Write(AWriter);
  AFib.MainTextBoxBreakTableSize := Integer((AWriter.BaseStream.Position - AFib.MainTextBoxBreakTableOffset));
end;

procedure TdxTextBoxesExporter.ExportHeaderTextBoxesTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  AFib.HeaderTextBoxesTextOffset := Integer(AWriter.BaseStream.Position);
  ACount := HeaderDocument.Positions.Count;
  for I := 0 to ACount - 1 do
    AWriter.Write(HeaderDocument.Positions[I]);
  ACount := HeaderDocument.TextBoxesIdentifiers.Count;
  for I := 0 to ACount - 1 do
    HeaderDocument.TextBoxesIdentifiers[I].Write(AWriter);
  AFib.HeaderTextBoxesTextSize := Integer((AWriter.BaseStream.Position - AFib.HeaderTextBoxesTextOffset));
  AFib.HeadersFootersTextBoxBreakTableOffset := Integer(AWriter.BaseStream.Position);
  HeaderDocument.BreakDescriptors.Write(AWriter);
  AFib.HeadersFootersTextBoxBreakTableSize := Integer((AWriter.BaseStream.Position - AFib.HeadersFootersTextBoxBreakTableOffset));
end;

procedure TdxTextBoxesExporter.FinishCurrentState(ACharacterPosition: Integer);
begin
  ActiveExporterState.FinishState(ACharacterPosition);
end;

procedure TdxTextBoxesExporter.FinishDocument(ALastCharacterPosition: Integer);
begin
  MainDocument.FinishDocument(ALastCharacterPosition);
  HeaderDocument.FinishDocument(ALastCharacterPosition);
end;

{ TdxFileShapeAddressesExporter }

constructor TdxFileShapeAddressesExporter.Create(AUnitConverter: TdxDocumentModelUnitConverter);
begin
  FUnitConverter := AUnitConverter;
  FMainDocumentAddresses := TdxFileShapeAddressTable.Create;
  FHeaderAddresses := TdxFileShapeAddressTable.Create;
end;

destructor TdxFileShapeAddressesExporter.Destroy;
begin
  FMainDocumentAddresses.Free;
  FHeaderAddresses.Free;
  inherited Destroy;
end;

function TdxFileShapeAddressesExporter.CreateShapeIdentifier(ARun: TdxFloatingObjectAnchorRun; ACharacterPosition: Integer): Integer;
begin
  Result := CalcShapeIdentifier;
  ExportFloatingObjectProperties(ARun.FloatingObjectProperties, ACharacterPosition, Result);
end;

procedure TdxFileShapeAddressesExporter.ExportFloatingObjectProperties(AFloatingObjectProperties: TdxFloatingObjectProperties;
  ACharacterPosition: Integer; AShapeIdentifier: Integer);
var
  AAddress: TdxFileShapeAddress;
begin
  AAddress := TdxFileShapeAddress.Create;
  AAddress.ShapeIdentifier := AShapeIdentifier;
  SetFileShapeAddressProperties(AFloatingObjectProperties, AAddress);
  if State = TdxDocContentState.MainDocument then
    MainDocumentAddresses.AddEntry(ACharacterPosition, AAddress);
  if State = TdxDocContentState.HeadersFootersStory then
    HeaderAddresses.AddEntry(ACharacterPosition, AAddress);
end;

procedure TdxFileShapeAddressesExporter.ExportFileShapeAddresses(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  if MainDocumentAddresses.AddressesCount > 0 then
  begin
    AFib.MainDocumentFileShapeTableOffset := Integer(AWriter.BaseStream.Position);
    MainDocumentAddresses.Write(AWriter);
    AFib.MainDocumentFileShapeTableSize := Integer((AWriter.BaseStream.Position - AFib.MainDocumentFileShapeTableOffset));
  end;
  if HeaderAddresses.AddressesCount > 0 then
  begin
    AFib.HeadersFootersFileShapeTableOffset := Integer(AWriter.BaseStream.Position);
    HeaderAddresses.Write(AWriter);
    AFib.HeadersFootersFileShapeTableSize := Integer((AWriter.BaseStream.Position - AFib.HeadersFootersFileShapeTableOffset));
  end;
end;

procedure TdxFileShapeAddressesExporter.SetFileShapeAddressProperties(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  SetLocked(AFloatingObjectProperties, AAddress);
  SetHorizontalPositionType(AFloatingObjectProperties, AAddress);
  SetVerticalPositionType(AFloatingObjectProperties, AAddress);
  SetTextWrapSide(AFloatingObjectProperties, AAddress);
  SetTextWrapType(AFloatingObjectProperties, AAddress);
  SetIsBehindText(AFloatingObjectProperties, AAddress);
  SetLeft(AFloatingObjectProperties, AAddress);
  SetTop(AFloatingObjectProperties, AAddress);
  SetRight(AFloatingObjectProperties, AAddress);
  SetBottom(AFloatingObjectProperties, AAddress);
end;

procedure TdxFileShapeAddressesExporter.SetLocked(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  if AFloatingObjectProperties.UseLocked then
    AAddress.Locked := AFloatingObjectProperties.Locked;
end;

procedure TdxFileShapeAddressesExporter.SetHorizontalPositionType(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  if AFloatingObjectProperties.UseHorizontalPositionType then
    AAddress.HorisontalPositionType := AFloatingObjectProperties.HorizontalPositionType;
end;

procedure TdxFileShapeAddressesExporter.SetVerticalPositionType(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  if AFloatingObjectProperties.UseVerticalPositionType then
    AAddress.VericalPositionType := AFloatingObjectProperties.VerticalPositionType;
end;

procedure TdxFileShapeAddressesExporter.SetTextWrapSide(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  if AFloatingObjectProperties.UseTextWrapSide then
    AAddress.TextWrapSide := AFloatingObjectProperties.TextWrapSide;
end;

procedure TdxFileShapeAddressesExporter.SetTextWrapType(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  if AFloatingObjectProperties.TextWrapType = TdxFloatingObjectTextWrapType.None then
    AAddress.TextWrapType := TdxDocFloatingObjectTextWrapTypeCalculator.WrapTypeBehindText
  else
    AAddress.TextWrapType := AFloatingObjectProperties.TextWrapType;
end;

procedure TdxFileShapeAddressesExporter.SetIsBehindText(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  AAddress.IsBehindDoc := AFloatingObjectProperties.IsBehindDoc;
end;

procedure TdxFileShapeAddressesExporter.SetLeft(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  AAddress.Left := UnitConverter.ModelUnitsToTwips(AFloatingObjectProperties.Offset.X);
end;

procedure TdxFileShapeAddressesExporter.SetRight(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  AAddress.Right := UnitConverter.ModelUnitsToTwips(AFloatingObjectProperties.Offset.X + AFloatingObjectProperties.ActualSize.cx);
end;

procedure TdxFileShapeAddressesExporter.SetTop(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  AAddress.Top := UnitConverter.ModelUnitsToTwips(AFloatingObjectProperties.Offset.Y);
end;

procedure TdxFileShapeAddressesExporter.SetBottom(AFloatingObjectProperties: TdxFloatingObjectProperties; AAddress: TdxFileShapeAddress);
begin
  AAddress.Bottom := UnitConverter.ModelUnitsToTwips(AFloatingObjectProperties.Offset.Y + AFloatingObjectProperties.ActualSize.cy);
end;

procedure TdxFileShapeAddressesExporter.Finish(ALastCharacterPosition: Integer);
begin
  MainDocumentAddresses.Finish(ALastCharacterPosition);
  HeaderAddresses.Finish(ALastCharacterPosition);
end;

function TdxFileShapeAddressesExporter.CalcShapeIdentifier: Integer;
begin
  if State = TdxDocContentState.MainDocument then
    Result := TdxOfficeArtConstants.DefaultMainDocumentShapeIdentifier + MainDocumentAddresses.AddressesCount + 1
  else
    Result := TdxOfficeArtConstants.DefaultHeaderShapeIdentifier + HeaderAddresses.AddressesCount + 1;
end;

{ TdxDocFloatingObjectsExporter }

constructor TdxDocFloatingObjectsExporter.Create(AUnitConverter: TdxDocumentModelUnitConverter; APageBackColor: TdxAlphaColor);
begin
  FShapeAddressesExporter := TdxFileShapeAddressesExporter.Create(AUnitConverter);
  FTextBoxesExporter := TdxTextBoxesExporter.Create;
  FArtContent := TdxOfficeArtContent.Create;
  FArtContent.SetPageBackColor(APageBackColor);
end;

destructor TdxDocFloatingObjectsExporter.Destroy;
begin
  FShapeAddressesExporter.Free;
  FTextBoxesExporter.Free;
  FArtContent.Free;
  inherited Destroy;
end;

procedure TdxDocFloatingObjectsExporter.SetState(AState: TdxDocContentState);
begin
  FState := AState;
  ShapeAddressesExporter.State := AState;
  TextBoxesExporter.State := AState;
end;

procedure TdxDocFloatingObjectsExporter.RegisterFloatingObject(ARun: TdxFloatingObjectAnchorRun; ACharacterPosition: Integer);
var
  AShapeIdentifier: Integer;
begin
  AShapeIdentifier := ShapeAddressesExporter.CreateShapeIdentifier(ARun, ACharacterPosition);
  RegisterFloatingObjectCore(ARun, ACharacterPosition, AShapeIdentifier);
end;

procedure TdxDocFloatingObjectsExporter.RegisterFloatingObjectCore(ARun: TdxFloatingObjectAnchorRun; ACharacterPosition: Integer; AShapeIdentifier: Integer);
var
  APictureContent: TdxPictureFloatingObjectContent;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
begin
  APictureContent := Safe<TdxPictureFloatingObjectContent>.Cast(ARun.Content);
  if APictureContent <> nil then
    RegisterPictureFloatingObject(APictureContent.Image, AShapeIdentifier, ARun, ShapeAddressesExporter.UnitConverter);
  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  if ATextBoxContent <> nil then
    RegisterTextBoxFloatingObject(ATextBoxContent, AShapeIdentifier, ARun, ShapeAddressesExporter.UnitConverter);
end;

procedure TdxDocFloatingObjectsExporter.RegisterPictureFloatingObject(AImage: TdxOfficeImageReference;
  AShapeIdentifier: Integer; ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  OfficeArtContent.InsertPictureFloatingObject(AImage, State, AShapeIdentifier, ARun, AUnitConverter);
end;

procedure TdxDocFloatingObjectsExporter.RegisterTextBoxFloatingObject(AContent: TdxTextBoxFloatingObjectContent;
  AShapeIdentifier: Integer; ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
var
  ATextBoxId: Integer;
begin
  ATextBoxId := TextBoxesExporter.RegisterTextBoxFloatingObject(AContent, AShapeIdentifier);
  OfficeArtContent.InsertTextBoxFloatingObject(State, AShapeIdentifier, ATextBoxId, ARun, AUnitConverter);
end;

procedure TdxDocFloatingObjectsExporter.ExportFloatingObjectsInfo(AFib: TdxFileInformationBlock;
  AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);
begin
  ShapeAddressesExporter.ExportFileShapeAddresses(AFib, AWriter);
  TextBoxesExporter.ExportTextBoxesTables(AFib, AWriter);
  ExportOfficeArtContent(AFib, AWriter, AEmbeddedWriter);
end;

procedure TdxDocFloatingObjectsExporter.ExportOfficeArtContent(AFib: TdxFileInformationBlock;
  AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);
begin
  AFib.DrawingObjectTableOffset := Integer(AWriter.BaseStream.Position);
  PrepareOfficeArtContentForExport;
  OfficeArtContent.Write(AWriter, AEmbeddedWriter);
  AFib.DrawingObjectTableSize := Integer((AWriter.BaseStream.Position - AFib.DrawingObjectTableOffset));
end;

procedure TdxDocFloatingObjectsExporter.PrepareOfficeArtContentForExport;
begin
  OfficeArtContent.DrawingContainer.FileDrawingBlock.HeaderFloatingObjectsCount := ShapeAddressesExporter.HeaderAddresses.AddressesCount;
  OfficeArtContent.DrawingContainer.FileDrawingBlock.MainDocumentFloatingObjectsCount := ShapeAddressesExporter.MainDocumentAddresses.AddressesCount;
  OfficeArtContent.MainDocumentDrawing.DrawingObjectsContainer.SetBackgroundShapeProperties;
end;

procedure TdxDocFloatingObjectsExporter.Finish(ALastCharacterPosition: Integer);
begin
  ShapeAddressesExporter.Finish(ALastCharacterPosition);
  TextBoxesExporter.FinishDocument(ALastCharacterPosition);
end;

{ TdxDocNotesExporter }

constructor TdxDocNotesExporter.Create;
begin
  FFootNoteReferences := TdxIntegerList.Create;
  FFootNotePositions := TdxIntegerList.Create;
  FEndNoteReferences := TdxIntegerList.Create;
  FEndNotePositions := TdxIntegerList.Create;
  FFootNoteFlags := TBits.Create;
  FEndNoteFlags := TBits.Create;
end;

destructor TdxDocNotesExporter.Destroy;
begin
  FFootNoteReferences.Free;
  FFootNotePositions.Free;
  FEndNoteReferences.Free;
  FEndNotePositions.Free;
  FFootNoteFlags.Free;
  FEndNoteFlags.Free;
  inherited Destroy;
end;

procedure TdxDocNotesExporter.FinishFootNotePositions(ALastFootNoteEnd: Integer; ALastPosition: Integer);
begin
  FootNotePositions.Add(ALastFootNoteEnd);
  FootNotePositions.Add(ALastPosition);
end;

procedure TdxDocNotesExporter.FinishEndNotePositions(ALastEndNoteEnd: Integer);
begin
  EndNotePositions.Add(ALastEndNoteEnd - 1);
  EndNotePositions.Add(ALastEndNoteEnd);
end;

procedure TdxDocNotesExporter.AddFootNoteReferenceEntry(ACharacterPosition: Integer; AIsAutoNumbered: Boolean);
begin
  FootNoteReferences.Add(ACharacterPosition);
  FFootNoteFlags[FFootNoteFlags.Size] := AIsAutoNumbered;
end;

procedure TdxDocNotesExporter.AddEndNoteReferenceEntry(ACharacterPosition: Integer; AIsAutoNumbered: Boolean);
begin
  EndNoteReferences.Add(ACharacterPosition);
  FEndNoteFlags[FEndNoteFlags.Size] := AIsAutoNumbered;
end;

procedure TdxDocNotesExporter.FinishFootNoteReferences(ALastCharacterPosition: Integer);
begin
  FootNoteReferences.Add(ALastCharacterPosition);
end;

procedure TdxDocNotesExporter.FinishEndNoteReferences(ALastCharacterPosition: Integer);
begin
  EndNoteReferences.Add(ALastCharacterPosition);
end;

procedure TdxDocNotesExporter.ExportFootNoteTables(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  AFib.FootNotesReferenceOffset := Integer(AWriter.BaseStream.Position);
  ExportNoteReferences(AWriter, FootNoteReferences, FootNoteFlags);
  AFib.FootNotesReferenceSize := Integer((AWriter.BaseStream.Position - AFib.FootNotesReferenceOffset));
  AFib.FootNotesTextOffset := Integer(AWriter.BaseStream.Position);
  ExportNotePositions(AWriter, FootNotePositions);
  AFib.FootNotesTextSize := Integer((AWriter.BaseStream.Position - AFib.FootNotesTextOffset));
end;

procedure TdxDocNotesExporter.ExportEndNoteTables(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  AFib.EndNotesReferenceOffset := Integer(AWriter.BaseStream.Position);
  ExportNoteReferences(AWriter, EndNoteReferences, EndNoteFlags);
  AFib.EndNotesReferenceSize := Integer((AWriter.BaseStream.Position - AFib.EndNotesReferenceOffset));
  AFib.EndnotesTextOffset := Integer(AWriter.BaseStream.Position);
  ExportNotePositions(AWriter, EndNotePositions);
  AFib.EndnotesTextSize := Integer((AWriter.BaseStream.Position - AFib.EndnotesTextOffset));
end;

procedure TdxDocNotesExporter.ExportNoteReferences(AWriter: TBinaryWriter; AReferences: TdxIntegerList; AFlags: TBits);
var
  ACount, I: Integer;
begin
  ACount := AReferences.Count;
  if ACount <= 1 then
    Exit;
  for I := 0 to ACount - 1 do
    AWriter.Write(AReferences[I]);
  ACount := AFlags.Size;
  for I := 0 to ACount - 1 do
  begin
    if AFlags[I] then
      AWriter.Write(SmallInt((I + 1)))
    else
      AWriter.Write(Integer(0));
  end;
end;

procedure TdxDocNotesExporter.ExportNotePositions(AWriter: TBinaryWriter; APositions: TdxIntegerList);
var
  ACount, I: Integer;
begin
  ACount := APositions.Count;
  for I := 0 to ACount - 1 do
    AWriter.Write(APositions[I]);
end;

{ TdxDocTablesExporter }

constructor TdxDocTablesExporter.Create;
begin
  FGrids := TObjectStack<TdxTableGrid>.Create;
end;

destructor TdxDocTablesExporter.Destroy;
begin
  FGrids.Free;
  inherited Destroy;
end;

function TdxDocTablesExporter.GetInTable: Boolean;
begin
  Result := FCurrentTableDepth > 0;
end;

procedure TdxDocTablesExporter.AdvanceNext(AInfo: TdxTableInfo);
var
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ATableWidthsCalculator: TdxDocTableWidthsCalculator;
  AGridCalculator: TdxTableGridCalculator;
  AParentSectionWidth: Integer;
begin
  Inc(FCurrentTableDepth);
  AConverter := AInfo.Table.DocumentModel.UnitConverter.CreateConverterToLayoutUnits(TdxDocumentLayoutUnit.Twip);
  try
    ATableWidthsCalculator := TdxDocTableWidthsCalculator.Create(AConverter);
    try
      AGridCalculator := TdxTableGridCalculator.Create(ATableWidthsCalculator, MaxInt);
      try
        AParentSectionWidth := GetParentSectionWidthInModelUnits(AInfo.Table);
        Grids.Push(AGridCalculator.CalculateTableGrid(AInfo.Table, AParentSectionWidth));
      finally
        AGridCalculator.Free;
      end;
    finally
      ATableWidthsCalculator.Free;
    end;
  finally
    AConverter.Free;
  end;
end;

procedure TdxDocTablesExporter.FinishTable;
begin
  Assert(FCurrentTableDepth > 0);
  Dec(FCurrentTableDepth);
  Grids.Pop;
end;

function TdxDocTablesExporter.GetTableUnitMark: string;
begin
  if (TableDepth = 1) then
    Result := TdxTextCodes.TableUnitMark
  else
    Result := TdxTextCodes.ParagraphMark;
end;

function TdxDocTablesExporter.GetTableCellPropertyModifiers(AParagraph: TdxParagraph): TBytes;
var
  AOutput: TdxMemoryStream;
  AParagraphActions: TdxDocParagraphPropertiesActions;
  ACell: TdxTableCell;
  ACellActions: TdxDocTableCellActions;
begin
  AOutput := TdxMemoryStream.Create;
  try
    AParagraphActions := TdxDocParagraphPropertiesActions.Create(AOutput, AParagraph);
    try
      AParagraphActions.CreateParagarphPropertyModifiers;
      ACell := AParagraph.GetCell;
      ACellActions := TdxDocTableCellActions.Create(AOutput, ACell);
      try
        ACellActions.CreateTableCellPropertyModifiers(TableDepth, (ACell <> nil) and (ACell.EndParagraphIndex = AParagraph.Index));
        Result := AOutput.ToArray;
      finally
        ACellActions.Free;
      end;
    finally
      AParagraphActions.Free;
    end;
  finally
    AOutput.Free;
  end;
end;

function TdxDocTablesExporter.GetTableRowPropertyModifiers(ARow: TdxTableRow; ATableStyleIndex: Integer): TBytes;
var
  AOutput: TdxMemoryStream;
  ARowActions: TdxDocTableRowActions;
  ATableActions: TdxDocTableActions;
begin
  AOutput := TdxMemoryStream.Create;
  try
    ARowActions := TdxDocTableRowActions.Create(AOutput, ARow, Grids.Peek);
    try
      ARowActions.CreateTableRowPropertyModifiers(TableDepth, ATableStyleIndex);
      ATableActions := TdxDocTableActions.Create(AOutput, ARow);
      try
        ATableActions.CreateTablePropertyModifiers;
        Result := AOutput.ToArray;
      finally
        ATableActions.Free;
      end;
    finally
      ARowActions.Free;
    end;
  finally
    AOutput.Free;
  end;
end;

function TdxDocTablesExporter.GetParentSectionWidthInModelUnits(ATable: TdxTable): Integer;
var
  APos: TdxDocumentModelPosition;
  ASectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  ASectionMargins: TdxSectionMargins;
  AMargins: Integer;
begin
  APos := TdxDocumentModelPosition.FromParagraphStart(ATable.PieceTable, ATable.FirstRow.FirstCell.StartParagraphIndex);
  ASectionIndex := ATable.DocumentModel.FindSectionIndex(APos.LogPosition);
  ASection := TdxSection(ATable.DocumentModel.Sections[ASectionIndex]);
  ASectionMargins := ASection.Margins;
  AMargins := ASectionMargins.Left + ASectionMargins.Right;
  Result := ASection.Page.Width - AMargins;
end;

{ TdxDocFieldsExporter }

destructor TdxDocFieldsExporter.Destroy;
begin
  FMainDocumentFieldTable.Free;
  FFootNotesFieldTable.Free;
  FHeadersFootersFieldTable.Free;
  FCommentsFieldTable.Free;
  FEndNotesFieldTable.Free;
  FTextBoxesFieldTable.Free;
  FHeaderTextBoxesFieldTable.Free;
  inherited Destroy;
end;

function TdxDocFieldsExporter.GetFieldTable: TdxDocFieldTable;
begin
  case State of
    TdxDocContentState.MainDocument:
      begin
        if FMainDocumentFieldTable = nil then
          FMainDocumentFieldTable := TdxDocFieldTable.Create;
        Exit(FMainDocumentFieldTable);
      end;
    TdxDocContentState.Footnotes:
      begin
        if FFootNotesFieldTable = nil then
          FFootNotesFieldTable := TdxDocFieldTable.Create;
        Exit(FFootNotesFieldTable);
      end;
    TdxDocContentState.HeadersFootersStory:
      begin
        if FHeadersFootersFieldTable = nil then
          FHeadersFootersFieldTable := TdxDocFieldTable.Create;
        Exit(FHeadersFootersFieldTable);
      end;
    TdxDocContentState.Comments:
      begin
        if FCommentsFieldTable = nil then
          FCommentsFieldTable := TdxDocFieldTable.Create;
        Exit(FCommentsFieldTable);
      end;
    TdxDocContentState.Endnotes:
      begin
        if FEndNotesFieldTable = nil then
          FEndNotesFieldTable := TdxDocFieldTable.Create;
        Exit(FEndNotesFieldTable);
      end;
    TdxDocContentState.TextBoxes:
      begin
        if FTextBoxesFieldTable = nil then
          FTextBoxesFieldTable := TdxDocFieldTable.Create;
        Exit(FTextBoxesFieldTable);
      end;
    TdxDocContentState.HeaderTextBoxes:
      begin
        if FHeaderTextBoxesFieldTable = nil then
          FHeaderTextBoxesFieldTable := TdxDocFieldTable.Create;
        Exit(FHeaderTextBoxesFieldTable);
      end
    else
      Result := nil;
  end;
end;

procedure TdxDocFieldsExporter.Finish(ALastCharacterPosition: Integer);
begin
  if FMainDocumentFieldTable <> nil then
    FMainDocumentFieldTable.Finish(ALastCharacterPosition);
  if FHeadersFootersFieldTable <> nil then
    FHeadersFootersFieldTable.Finish(ALastCharacterPosition);
  if FTextBoxesFieldTable <> nil then
    FTextBoxesFieldTable.Finish(ALastCharacterPosition);
  if FHeaderTextBoxesFieldTable <> nil then
    FHeaderTextBoxesFieldTable.Finish(ALastCharacterPosition);
  if FFootNotesFieldTable <> nil then
    FFootNotesFieldTable.Finish(ALastCharacterPosition);
  if FEndNotesFieldTable <> nil then
    FEndNotesFieldTable.Finish(ALastCharacterPosition);
  if FCommentsFieldTable <> nil then
    FCommentsFieldTable.Finish(ALastCharacterPosition);
end;

procedure TdxDocFieldsExporter.ExportFieldTables(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  ExportMainDocumentFieldTable(AFib, AWriter);
  ExportHeadersFootersFieldTable(AFib, AWriter);
  ExportFootNotesFieldTable(AFib, AWriter);
  ExportCommentsFieldTable(AFib, AWriter);
  ExportEndNotesFieldTable(AFib, AWriter);
  ExportTextBoxesFieldTable(AFib, AWriter);
  ExportHeaderTextBoxesFieldTable(AFib, AWriter);
end;

procedure TdxDocFieldsExporter.ExportMainDocumentFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  if FMainDocumentFieldTable <> nil then
  begin
    AFib.MainDocumentFieldTableOffset := Integer(AWriter.BaseStream.Position);
    FMainDocumentFieldTable.Write(AWriter);
    AFib.MainDocumentFieldTableSize := Integer(AWriter.BaseStream.Position - AFib.MainDocumentFieldTableOffset);
  end;
end;

procedure TdxDocFieldsExporter.ExportHeadersFootersFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  if FHeadersFootersFieldTable <> nil then
  begin
    AFib.HeadersFootersFieldTableOffset := Integer(AWriter.BaseStream.Position);
    FHeadersFootersFieldTable.Write(AWriter);
    AFib.HeadersFootersFieldTableSize := Integer(AWriter.BaseStream.Position - AFib.HeadersFootersFieldTableOffset);
  end;
end;

procedure TdxDocFieldsExporter.ExportFootNotesFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  if FFootNotesFieldTable <> nil then
  begin
    AFib.FootNotesFieldTableOffset := Integer(AWriter.BaseStream.Position);
    FFootNotesFieldTable.Write(AWriter);
    AFib.FootNotesFieldTableSize := Integer(AWriter.BaseStream.Position - AFib.FootNotesFieldTableOffset);
  end;
end;

procedure TdxDocFieldsExporter.ExportCommentsFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  if FCommentsFieldTable <> nil then
  begin
    AFib.CommentsFieldTableOffset := Integer(AWriter.BaseStream.Position);
    FCommentsFieldTable.Write(AWriter);
    AFib.CommentsFieldTableSize := Integer(AWriter.BaseStream.Position - AFib.CommentsFieldTableOffset);
  end;
end;

procedure TdxDocFieldsExporter.ExportEndNotesFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  if FEndNotesFieldTable <> nil then
  begin
    AFib.EndNotesFieldTableOffset := Integer(AWriter.BaseStream.Position);
    FEndNotesFieldTable.Write(AWriter);
    AFib.EndNotesFieldTableSize := Integer((AWriter.BaseStream.Position - AFib.EndNotesFieldTableOffset));
  end;
end;

procedure TdxDocFieldsExporter.ExportTextBoxesFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  if FTextBoxesFieldTable <> nil then
  begin
    AFib.MainDocumentTextBoxesFieldTableOffset := Integer(AWriter.BaseStream.Position);
    FTextBoxesFieldTable.Write(AWriter);
    AFib.MainDocumentTextBoxesFieldTableSize := Integer(AWriter.BaseStream.Position - AFib.MainDocumentTextBoxesFieldTableOffset);
  end;
end;

procedure TdxDocFieldsExporter.ExportHeaderTextBoxesFieldTable(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  if FHeaderTextBoxesFieldTable <> nil then
  begin
    AFib.HeaderTextBoxesFieldTableOffset := Integer((AWriter.BaseStream.Position));
    FHeaderTextBoxesFieldTable.Write(AWriter);
    AFib.HeaderTextBoxesFieldTableSize := Integer(AWriter.BaseStream.Position - AFib.HeaderTextBoxesFieldTableOffset);
  end;
end;

{ TdxFontsExportHelper }

constructor TdxFontsExportHelper.Create;
begin
  FFontNamesCollection := TdxStringList.Create;
  FDefaultFontIndex := GetFontIndexByName(TdxDocCharacterFormattingInfo.DefaultFontName);
end;

destructor TdxFontsExportHelper.Destroy;
begin
  FFontNamesCollection.Free;
  inherited Destroy;
end;

function TdxFontsExportHelper.GetFontIndexByName(const AFontName: string): Integer;
var
  ACount, I: Integer;
begin
  ACount := FFontNamesCollection.Count;
  for I := 0 to ACount - 1 do
  begin
    if SameText(AFontName, FFontNamesCollection[I]) then
      Exit(I);
  end;
  FFontNamesCollection.Add(AFontName);
  Result := ACount;
end;

function TdxFontsExportHelper.GetFontNameByIndex(AIndex: Integer): string;
begin
  if AIndex >= FFontNamesCollection.Count then
    Result := ''
  else
    Result := FFontNamesCollection[AIndex];
end;

{ TdxDocStylesExporter }

constructor TdxDocStylesExporter.Create(ADocumentModel: TdxDocumentModel);
begin
  FDocumentModel := ADocumentModel;
  FStyleNamesCollection := TDictionary<string, Integer>.Create;
  FFontsExportHelper := TdxFontsExportHelper.Create;
end;

destructor TdxDocStylesExporter.Destroy;
begin
  FStyleNamesCollection.Free;;
  FFontsExportHelper.Free;
  FStyleSheet.Free;
  inherited Destroy;
end;

class constructor TdxDocStylesExporter.Initialize;
begin
  FHeadingStyles := TDictionary<Word, string>.Create(9);
  FHeadingStyles.Add($0001, 'Heading 1');
  FHeadingStyles.Add($0002, 'Heading 2');
  FHeadingStyles.Add($0003, 'Heading 3');
  FHeadingStyles.Add($0004, 'Heading 4');
  FHeadingStyles.Add($0005, 'Heading 5');
  FHeadingStyles.Add($0006, 'Heading 6');
  FHeadingStyles.Add($0007, 'Heading 7');
  FHeadingStyles.Add($0008, 'Heading 8');
  FHeadingStyles.Add($0009, 'Heading 9');
end;

class destructor TdxDocStylesExporter.Finalize;
begin
  FHeadingStyles.Free;
end;

function TdxDocStylesExporter.GetFontsCount: Integer;
begin
  Result := FontsExportHelper.FontNamesCollection.Count;
end;

function TdxDocStylesExporter.GetParagraphStyleInfoForExport(ASourceStyle: TdxParagraphStyle): TdxDocParagraphStyleInfo;
var
  ASourceStyleFontNameIndex, AFontNameIndex: Integer;
  ADocumentModel: TdxDocumentModel;
  ACharacterProperties: TdxMergedCharacterProperties;
  AParagraphProperties: TdxMergedParagraphProperties;
begin
  if ASourceStyle.Parent <> nil then
  begin
    ASourceStyleFontNameIndex := FontsExportHelper.GetFontIndexByName(ASourceStyle.CharacterProperties.FontName);
    Exit(TdxDocParagraphStyleInfo.Create(ASourceStyle, ASourceStyleFontNameIndex));
  end;

  ADocumentModel := TdxDocumentModel(ASourceStyle.DocumentModel);

  ACharacterProperties := ASourceStyle.GetMergedCharacterProperties;
  try
    ACharacterProperties.Merge(ADocumentModel.DefaultCharacterProperties);

    AParagraphProperties := ASourceStyle.GetMergedParagraphProperties;
    try
      AParagraphProperties.Merge(ADocumentModel.DefaultParagraphProperties);
      AFontNameIndex := FontsExportHelper.GetFontIndexByName(ACharacterProperties.Info.FontName);
      Result := TdxDocParagraphStyleInfo.Create(
        ACharacterProperties.Info,
        ACharacterProperties.Options,
        AParagraphProperties.Info,
        AParagraphProperties.Options,
        ASourceStyle.Tabs.Info,
        ASourceStyle.GetNumberingListIndex,
        ASourceStyle.GetListLevelIndex,
        AFontNameIndex,
        ADocumentModel);
    finally
      AParagraphProperties.Free;
    end;
  finally
    ACharacterProperties.Free;
  end;
end;

function TdxDocStylesExporter.GetParagraphStyleIndex(ADocumentModelParagraphIndex: Integer): Integer;
begin
  if not StyleNamesCollection.TryGetValue(DocumentModel.ParagraphStyles[ADocumentModelParagraphIndex].StyleName, Result) then
    Result := 0;
end;

function TdxDocStylesExporter.GetCharacterStyleIndex(ADocumentModelCharacterStyleIndex: Integer): Integer;
begin
  if not StyleNamesCollection.TryGetValue(DocumentModel.CharacterStyles[ADocumentModelCharacterStyleIndex].StyleName, Result) then
    Result := TdxDocStyleIndexes.DefaultCharacterStyleIndex;
end;

function TdxDocStylesExporter.GetNumberingStyleIndex(ADocumentModelNumberingStyleIndex: Integer): Integer;
begin
  if not StyleNamesCollection.TryGetValue(DocumentModel.NumberingListStyles[ADocumentModelNumberingStyleIndex].StyleName, Result) then
    Result := -1;
end;

function TdxDocStylesExporter.GetStyleIndexByName(const AStyleName: string): Integer;
begin
  if not StyleNamesCollection.TryGetValue(AStyleName, Result) then
    Result := -1;
end;

function TdxDocStylesExporter.GetFontIndexByName(const AFontName: string): Integer;
begin
  Result := FontsExportHelper.GetFontIndexByName(AFontName);
end;

function TdxDocStylesExporter.GetFontNameByIndex(AFontNameIndex: Integer): string;
begin
  Result := FontsExportHelper.GetFontNameByIndex(AFontNameIndex);
end;

function TdxDocStylesExporter.GetCharacterGroupPropertyModifiers(AListLevel: TdxListLevel; ACharacterStyleIndex: Integer): TBytes;
var
  AFontNameIndex: Integer;
  AOutput: TdxMemoryStream;
  AActions: TdxDocCharacterPropertiesActions;
begin
  AFontNameIndex := GetFontIndexByName(AListLevel.CharacterProperties.FontName);
  AOutput := TdxMemoryStream.Create;
  try
    AActions := TdxDocListCharacterPropertiesActions.Create(AOutput, AListLevel, AFontNameIndex);
    try
      AActions.CreateCharacterPropertiesModifiers(ACharacterStyleIndex, TdxDocStyleIndexes.DefaultParagraphStyleIndex, False);
      Result := AOutput.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AOutput.Free;
  end;
end;

function TdxDocStylesExporter.GetCharacterGroupPropertyModifiers(ARun: TdxTextRunBase; ASpecialSymbol: Boolean): TBytes;
var
  ACharacterStyleIndex, AFontNameIndex, AParagraphStyleIndex: Integer;
  AOutput: TdxMemoryStream;
  ACharacterProperties: TdxCharacterProperties;
  AActions: TdxDocCharacterPropertiesActions;
begin
  if ARun <> nil then
    ACharacterStyleIndex := GetCharacterStyleIndex(ARun.CharacterStyleIndex)
  else
    ACharacterStyleIndex := TdxDocStyleIndexes.DefaultCharacterStyleIndex;
  if ARun <> nil then
    AFontNameIndex := GetFontIndexByName(ARun.CharacterProperties.FontName)
  else
    AFontNameIndex := GetFontIndexByName(TdxDocCharacterFormattingInfo.DefaultFontName);
  AOutput := TdxMemoryStream.Create;
  try
    if ARun <> nil then
      ACharacterProperties := ARun.CharacterProperties
    else
      ACharacterProperties := nil;
    if ARun <> nil then
      AParagraphStyleIndex := GetParagraphStyleIndex(ARun.Paragraph.ParagraphStyleIndex)
    else
      AParagraphStyleIndex := TdxDocStyleIndexes.DefaultParagraphStyleIndex;
    AActions := TdxDocCharacterPropertiesActions.Create(AOutput, ACharacterProperties, AFontNameIndex);
    try
      AActions.CreateCharacterPropertiesModifiers(ACharacterStyleIndex, AParagraphStyleIndex, ASpecialSymbol);
      Result := AOutput.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AOutput.Free;
  end;
end;

function TdxDocStylesExporter.GetInlinePicturePropertyModifiers(ACharacterProperties: TdxCharacterProperties;
  ACharacterStyleIndex: Integer; ADataStreamOffset: Integer): TBytes;
var
  AFontNameIndex: Integer;
  AOutput: TdxMemoryStream;
  AActions: TdxDocCharacterPropertiesActions;
begin
  AFontNameIndex := GetFontIndexByName(ACharacterProperties.FontName);
  AOutput := TdxMemoryStream.Create;
  try
    AActions := TdxDocCharacterPropertiesActions.Create(AOutput, ACharacterProperties, AFontNameIndex);
    try
      AActions.CreateInlinePicturePropertiesModifiers(ACharacterStyleIndex, ADataStreamOffset);
      Result := AOutput.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AOutput.Free;
  end;
end;

function TdxDocStylesExporter.GetParagraphGroupPropertyModifiers(AParagraph: TdxParagraph): TBytes;
var
  AOutput: TdxMemoryStream;
  AActions: TdxDocParagraphPropertiesActions;
begin
  AOutput := TdxMemoryStream.Create;
  try
    AActions := TdxDocParagraphPropertiesActions.Create(AOutput, AParagraph);
    try
      AActions.CreateParagarphPropertyModifiers;
      Result := AOutput.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AOutput.Free;
  end;
end;

function TdxDocStylesExporter.GetParagraphGroupPropertyModifiers(AListLevel: TdxListLevel): TBytes;
var
  AOutput: TdxMemoryStream;
  AActions: TdxDocParagraphPropertiesActions;
begin
  AOutput := TdxMemoryStream.Create;
  try
    AActions := TdxDocParagraphPropertiesActions.Create(AOutput, AListLevel);
    try
      AActions.CreateParagarphPropertyModifiers;
      Result := AOutput.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AOutput.Free;
  end;
end;

function TdxDocStylesExporter.GetTableParagraphPropertyModifiers(AParagraph: TdxParagraph; ATableDepth: Integer): TBytes;
var
  AOutput: TdxMemoryStream;
  AActions: TdxDocParagraphPropertiesActions;
begin
  AOutput := TdxMemoryStream.Create;
  try
    AActions := TdxDocParagraphPropertiesActions.Create(AOutput, AParagraph);
    try
      AActions.CreateTableParagraphPropertyModifiers(ATableDepth);
      Result := AOutput.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AOutput.Free;
  end;
end;

procedure TdxDocStylesExporter.CreateStyleSheet;
begin
  FStyleSheet := TdxDocStyleSheet.CreateDefault;
  SetStyleSheetInformation(FStyleSheet);
  AddDefaultStyles(FStyleSheet);
  AddParagraphStyles(FStyleSheet);
  AddCharacterStyles(FStyleSheet);
  AddListStyles(FStyleSheet);
  AddTableStyles(FStyleSheet);
  FStyleSheet.StylesInformation.StylesCount := SmallInt(FStyleSheet.Styles.Count);
end;

procedure TdxDocStylesExporter.WriteStyleSheet(AWriter: TBinaryWriter);
begin
  FStyleSheet.Write(AWriter);
end;

procedure TdxDocStylesExporter.SetStyleSheetInformation(AStyleSheet: TdxDocStyleSheet);
begin
  if FDocumentModel.DefaultCharacterProperties.UseFontName then
    AStyleSheet.StylesInformation.DefaultASCIIFont := SmallInt(FontsExportHelper.GetFontIndexByName(DocumentModel.DefaultCharacterProperties.FontName));
end;

procedure TdxDocStylesExporter.AddDefaultStyles(AStyleSheet: TdxDocStyleSheet);
begin
  AddDefaultParagraphStyle(AStyleSheet);
  AddHeadingStyles(AStyleSheet);
  AddDefaultCharacterStyle(AStyleSheet);
  AddDefaultTableStyle(AStyleSheet);
  AddDefaultListStyle(AStyleSheet);
  AStyleSheet.Styles.Add(nil);
  AStyleSheet.Styles.Add(nil);
  AStyleSheet.StylesInformation.FixedIndexStylesCount := $000f;
end;

procedure TdxDocStylesExporter.AddDefaultParagraphStyle(AStyleSheet: TdxDocStyleSheet);
var
  ADefaultParagraphStyle: TdxParagraphStyle;
  AParagraphStyleInfo: TdxDocParagraphStyleInfo;
  ADefaultParagraphStyleDescription: TdxParagraphStyleDescription;
begin
  ADefaultParagraphStyle := TdxParagraphStyle(DocumentModel.ParagraphStyles.GetStyleByName(DefaultParagraphStyleName));
  if ADefaultParagraphStyle = nil then
    ADefaultParagraphStyle := DocumentModel.ParagraphStyles.DefaultItem;
  AParagraphStyleInfo := GetParagraphStyleInfoForExport(ADefaultParagraphStyle);
  try
    ADefaultParagraphStyleDescription := TdxParagraphStyleDescription.Create(AParagraphStyleInfo);
    ADefaultParagraphStyleDescription.StyleIdentifier := DefaultParagraphStyleIdentifier;
    ADefaultParagraphStyleDescription.StyleName := ADefaultParagraphStyle.StyleName;
    AStyleSheet.Styles.Add(ADefaultParagraphStyleDescription);
    StyleNamesCollection.Add(ADefaultParagraphStyle.StyleName, 0);
  finally
    AParagraphStyleInfo.Free;
  end;
end;

procedure TdxDocStylesExporter.AddHeadingStyles(AStyleSheet: TdxDocStyleSheet);
var
  ADefaultHeadingStyleIndex: Word;
  AHeadingStyleName: string;
  AHeading: TdxParagraphStyle;
  AParagraphStyleInfo: TdxDocParagraphStyleInfo;
  AHeadingStyleDescription: TdxParagraphStyleDescription;
begin
  for ADefaultHeadingStyleIndex := 1 to 10 - 1 do
  begin
    AHeadingStyleName := FHeadingStyles[ADefaultHeadingStyleIndex];
    AHeading := TdxParagraphStyle(DocumentModel.ParagraphStyles.GetStyleByName(AHeadingStyleName));
    if AHeading <> nil then
    begin
      AParagraphStyleInfo := GetParagraphStyleInfoForExport(AHeading);
      try
        AHeadingStyleDescription := TdxParagraphStyleDescription.Create(AParagraphStyleInfo);
        AHeadingStyleDescription.StyleIdentifier := $0001;
        AHeadingStyleDescription.StyleName := AHeadingStyleName;
        AHeadingStyleDescription.BaseStyleIndex := DefaultParagraphStyleIdentifier;
        AHeadingStyleDescription.NextStyleIndex := DefaultParagraphStyleIdentifier;
        AStyleSheet.Styles.Add(AHeadingStyleDescription);
        StyleNamesCollection.Add(AHeadingStyleName, ADefaultHeadingStyleIndex);
      finally
        AParagraphStyleInfo.Free;
      end;
    end
    else
      AStyleSheet.Styles.Add(nil);
  end;
end;

procedure TdxDocStylesExporter.AddDefaultCharacterStyle(AStyleSheet: TdxDocStyleSheet);
var
  ADefaultCharacterStyle: TdxCharacterStyle;
  AFontNameIndex: Integer;
  ADefaultCharacterStyleDescription: TdxCharacterStyleDescription;
begin
  ADefaultCharacterStyle := TdxCharacterStyle(DocumentModel.CharacterStyles.GetStyleByName(DefaultCharacterStyleName));
  AFontNameIndex := FontsExportHelper.GetFontIndexByName(ADefaultCharacterStyle.CharacterProperties.FontName);
  ADefaultCharacterStyleDescription := TdxCharacterStyleDescription.Create(ADefaultCharacterStyle, AStyleSheet.Styles.Count, AFontNameIndex);
  ADefaultCharacterStyleDescription.StyleIdentifier := DefaultCharacterStyleIdentifier;
  ADefaultCharacterStyleDescription.StyleName := DefaultCharacterStyleName;
  AStyleSheet.Styles.Add(ADefaultCharacterStyleDescription);
  StyleNamesCollection.Add(DefaultCharacterStyleName, TdxDocStyleIndexes.DefaultCharacterStyleIndex);
end;

procedure TdxDocStylesExporter.AddDefaultTableStyle(AStyleSheet: TdxDocStyleSheet);
var
  ADefaultTableStyle: TdxTableStyle;
  AFontNameIndex: Integer;
  ADefaultTableStyleDescription: TdxTableStyleDescription;
begin
  ADefaultTableStyle := TdxTableStyle(DocumentModel.TableStyles.GetStyleByName(DefaultTableStyleName));
  if ADefaultTableStyle <> nil then
  begin
    AFontNameIndex := FontsExportHelper.GetFontIndexByName(ADefaultTableStyle.CharacterProperties.FontName);
    ADefaultTableStyleDescription := TdxTableStyleDescription.Create(ADefaultTableStyle, AFontNameIndex);
    ADefaultTableStyleDescription.StyleIdentifier := DefaultTableStyleIdentifier;
    ADefaultTableStyleDescription.StyleName := DefaultTableStyleName;
    ADefaultTableStyleDescription.StyleIndex := TdxDocStyleIndexes.DefaultTableStyleIndex;
    StyleNamesCollection.Add(DefaultTableStyleName, TdxDocStyleIndexes.DefaultTableStyleIndex);
    AStyleSheet.Styles.Add(ADefaultTableStyleDescription);
  end
  else
    AStyleSheet.Styles.Add(nil);
end;

procedure TdxDocStylesExporter.AddDefaultListStyle(AStyleSheet: TdxDocStyleSheet);
var
  ADefaultListStyleDescription: TdxListStyleDescription;
begin
  ADefaultListStyleDescription := TdxListStyleDescription.Create;
  ADefaultListStyleDescription.StyleIdentifier := DefaultListStyleIdentifier;
  ADefaultListStyleDescription.StyleName := DefaultListStyleName;
  ADefaultListStyleDescription.StyleIndex := TdxDocStyleIndexes.DefaultListStyleIndex;
  StyleNamesCollection.Add(DefaultListStyleName, TdxDocStyleIndexes.DefaultListStyleIndex);
  AStyleSheet.Styles.Add(ADefaultListStyleDescription);
end;

procedure TdxDocStylesExporter.AddParagraphStyles(AStyleSheet: TdxDocStyleSheet);
var
  ACount, AParagraphStyleIndex: Integer;
  ACurrentStyle: TdxParagraphStyle;
  AStyleDescription: TdxParagraphStyleDescription;
  AParagraphStyleInfo: TdxDocParagraphStyleInfo;
begin
  ACount := DocumentModel.ParagraphStyles.Count;
  for AParagraphStyleIndex := 0 to ACount - 1 do
  begin
    ACurrentStyle := DocumentModel.ParagraphStyles[AParagraphStyleIndex];
    if GetStyleIndexByName(ACurrentStyle.StyleName) < 0 then
    begin
      AParagraphStyleInfo := GetParagraphStyleInfoForExport(ACurrentStyle);
      try
        AStyleDescription := TdxParagraphStyleDescription.Create(AParagraphStyleInfo);
        AStyleDescription.StyleName := ACurrentStyle.StyleName;
        AStyleDescription.Hidden := ACurrentStyle.Hidden;
        AStyleDescription.QFormat := ACurrentStyle.Primary;
        AStyleDescription.StyleIndex := AStyleSheet.Styles.Count;
        if ACurrentStyle.LinkedStyle <> nil then
          AStyleDescription.LinkedStyleIndex := SmallInt(GetStyleIndexByName(ACurrentStyle.LinkedStyle.StyleName));
        if ACurrentStyle.Parent <> nil then
          AStyleDescription.BaseStyleIndex := SmallInt(GetStyleIndexByName(ACurrentStyle.Parent.StyleName));
        StyleNamesCollection.Add(AStyleDescription.StyleName, AStyleDescription.StyleIndex);
        StyleSheet.Styles.Add(AStyleDescription);
      finally
        AParagraphStyleInfo.Free;
      end;
    end;
  end;
  SetNextParagraphStyles;
end;

procedure TdxDocStylesExporter.SetNextParagraphStyles;
var
  ACount, I: Integer;
  ADescription: TdxStyleDescriptionBase;
  AStyle: TdxParagraphStyle;
begin
  ACount := StyleSheet.Styles.Count;
  for I := 0 to ACount - 1 do
  begin
    ADescription := StyleSheet.Styles[I];
    if ADescription is TdxParagraphStyleDescription then
    begin
      AStyle := TdxParagraphStyle(DocumentModel.ParagraphStyles.GetStyleByName(ADescription.StyleName));
      if AStyle.NextParagraphStyle <> nil then
        ADescription.NextStyleIndex := SmallInt(GetStyleIndexByName(AStyle.NextParagraphStyle.StyleName));
    end;
  end;
end;

procedure TdxDocStylesExporter.AddCharacterStyles(AStyleSheet: TdxDocStyleSheet);
var
  ACount, ACharacterStyleIndex, AFontNameIndex: Integer;
  ACurrentStyle: TdxCharacterStyle;
  AStyleDescription: TdxCharacterStyleDescription;
begin
  ACount := DocumentModel.CharacterStyles.Count;
  for ACharacterStyleIndex := 0 to ACount - 1 do
  begin
    ACurrentStyle := DocumentModel.CharacterStyles[ACharacterStyleIndex];
    if GetStyleIndexByName(ACurrentStyle.StyleName) < 0 then
    begin
      AFontNameIndex := GetFontIndexByName(ACurrentStyle.CharacterProperties.FontName);
      AStyleDescription := TdxCharacterStyleDescription.Create(ACurrentStyle, AStyleSheet.Styles.Count, AFontNameIndex);
      AStyleDescription.StyleName := ACurrentStyle.StyleName;
      AStyleDescription.Hidden := ACurrentStyle.Hidden;
      AStyleDescription.QFormat := ACurrentStyle.Primary;
      AStyleDescription.StyleIndex := AStyleSheet.Styles.Count;
      if ACurrentStyle.Parent <> nil then
        AStyleDescription.BaseStyleIndex := SmallInt(GetStyleIndexByName(ACurrentStyle.Parent.StyleName));
      if ACurrentStyle.LinkedStyle <> nil then
        AStyleDescription.LinkedStyleIndex := SmallInt(GetStyleIndexByName(ACurrentStyle.LinkedStyle.StyleName));
      AStyleDescription.NextStyleIndex := SmallInt(AStyleDescription.StyleIndex);
      StyleNamesCollection.Add(AStyleDescription.StyleName, AStyleDescription.StyleIndex);
      AStyleSheet.Styles.Add(AStyleDescription);
    end;
  end;
end;

procedure TdxDocStylesExporter.AddListStyles(AStyleSheet: TdxDocStyleSheet);
var
  ACount, I: Integer;
  ACurrentStyle: TdxNumberingListStyle;
  AStyleDescription: TdxListStyleDescription;
begin
  ACount := DocumentModel.NumberingListStyles.Count;
  for I := 0 to ACount - 1 do
  begin
    ACurrentStyle := DocumentModel.NumberingListStyles[I];
    if ACurrentStyle.NumberingListIndex < NumberingListIndexMinValue then
      Continue;
    if GetStyleIndexByName(ACurrentStyle.StyleName) < 0 then
    begin
      AStyleDescription := TdxListStyleDescription.Create(ACurrentStyle);
      AStyleDescription.StyleName := ACurrentStyle.StyleName;
      AStyleDescription.Hidden := ACurrentStyle.Hidden;
      AStyleDescription.QFormat := ACurrentStyle.Primary;
      AStyleDescription.StyleIndex := AStyleSheet.Styles.Count;
      if ACurrentStyle.Parent <> nil then
        AStyleDescription.BaseStyleIndex := SmallInt(GetStyleIndexByName(ACurrentStyle.Parent.StyleName));
      AStyleDescription.NextStyleIndex := SmallInt(AStyleDescription.StyleIndex);
      StyleNamesCollection.Add(AStyleDescription.StyleName, AStyleDescription.StyleIndex);
      AStyleSheet.Styles.Add(AStyleDescription);
    end;
  end;
end;

procedure TdxDocStylesExporter.AddTableStyles(AStyleSheet: TdxDocStyleSheet);
var
  ACount, ATableStyleIndex, AFontNameIndex: Integer;
  ACurrentStyle: TdxTableStyle;
  AStyleDescription: TdxTableStyleDescription;
begin
  ACount := DocumentModel.TableStyles.Count;
  for ATableStyleIndex := 0 to ACount - 1 do
  begin
    ACurrentStyle := DocumentModel.TableStyles[ATableStyleIndex];
    if GetStyleIndexByName(ACurrentStyle.StyleName) < 0 then
    begin
      AFontNameIndex := GetFontIndexByName(ACurrentStyle.CharacterProperties.FontName);
      AStyleDescription := TdxTableStyleDescription.Create(ACurrentStyle, AFontNameIndex);
      AStyleDescription.StyleName := ACurrentStyle.StyleName;
      AStyleDescription.Hidden := ACurrentStyle.Hidden;
      AStyleDescription.QFormat := ACurrentStyle.Primary;
      AStyleDescription.StyleIndex := AStyleSheet.Styles.Count;
      AStyleDescription.NextStyleIndex := SmallInt(AStyleDescription.StyleIndex);
      if ACurrentStyle.Parent <> nil then
        AStyleDescription.BaseStyleIndex := SmallInt(GetStyleIndexByName(ACurrentStyle.Parent.StyleName));
      StyleNamesCollection.Add(AStyleDescription.StyleName, AStyleDescription.StyleIndex);
      AStyleSheet.Styles.Add(AStyleDescription);
    end;
  end;
end;

{ TdxDocListsExporter }

constructor TdxDocListsExporter.Create(ADocumentModel: TdxDocumentModel; AOptions: TdxDocDocumentExporterOptions; AStylesExportHelper: TdxDocStylesExporter);
begin
  Assert(ADocumentModel <> nil, 'documentModel');
  Assert(AOptions <> nil, 'options');
  FDocumentModel := ADocumentModel;
  FOptions := AOptions;
  FStylesExportHelper := AStylesExportHelper;
end;

destructor TdxDocListsExporter.Destroy;
begin
  FListInfo.Free;
  FListOverrideInfo.Free;
  FListStyles.Free;
  inherited Destroy;
end;

procedure TdxDocListsExporter.WriteListFormatInformation(AWriter: TBinaryWriter);
begin
  FListInfo.Write(AWriter);
end;

procedure TdxDocListsExporter.WriteListOverrideFormatInformation(AWriter: TBinaryWriter);
begin
  FListOverrideInfo.Write(AWriter, Options.Compatibility.AllowNonLinkedListDefinitions);
end;

procedure TdxDocListsExporter.CreateLists(AStylesExporter: TdxDocStylesExporter);
begin
  FListInfo := CreateListInfo(AStylesExporter);
  FListOverrideInfo := CreateListOverrideInfo;
end;

type
  TListComparer = class(TComparer<TdxAbstractNumberingListIndex>)
  strict private
    FDocumentModel: TdxDocumentModel;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    function Compare(const Left, Right: TdxAbstractNumberingListIndex): Integer; override;
  end;

{ TListComparer }

constructor TListComparer.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
end;

function TListComparer.Compare(const Left, Right: TdxAbstractNumberingListIndex): Integer;
begin
  Result := FDocumentModel.AbstractNumberingLists[Left].Id - FDocumentModel.AbstractNumberingLists[Right].Id;
end;

function TdxDocListsExporter.CreateListInfo(AStylesExporter: TdxDocStylesExporter): TdxDocListFormatInformation;
var
  ACount, I, ADocStyleIndex: Integer;
  AIndexesSortedById: TArray<TdxAbstractNumberingListIndex>;
  AComparer: IComparer<TdxAbstractNumberingListIndex>;
  AList: TdxAbstractNumberingList;
begin
  Result := TdxDocListFormatInformation.Create;

  FListStyles.Free;
  FListStyles := TList<TdxListStylesRecordItem>.Create;

  ACount := DocumentModel.AbstractNumberingLists.Count;
  SetLength(AIndexesSortedById, ACount);
  for I := 0 to ACount - 1 do
    AIndexesSortedById[I] := I;

  AComparer := TListComparer.Create(DocumentModel);
  TArray.Sort<TdxAbstractNumberingListIndex>(AIndexesSortedById, AComparer);

  for I := 0 to ACount - 1 do
  begin
    AList := DocumentModel.AbstractNumberingLists[AIndexesSortedById[I]];
    Result.ListData.Add(CreateDocListData(AList));
    if AList.StyleLinkIndex >= 0 then
    begin
      ADocStyleIndex := AStylesExporter.GetNumberingStyleIndex(AList.StyleLinkIndex);
      if ADocStyleIndex >= 0 then
        FListStyles.Add(TdxListStylesRecordItem.Create(I, ADocStyleIndex, True));
    end
    else
      if AList.NumberingStyleReferenceIndex >= 0 then
      begin
        ADocStyleIndex := AStylesExporter.GetNumberingStyleIndex(AList.NumberingStyleReferenceIndex);
        if ADocStyleIndex >= 0 then
          FListStyles.Add(TdxListStylesRecordItem.Create(I, ADocStyleIndex, False));
      end;
  end;
end;

function TdxDocListsExporter.CreateDocListData(AList: TdxAbstractNumberingList): TdxDocListData;
var
  I: Integer;
begin
  Result := TdxDocListData.Create;
  Result.ListFormatting := CreateListFormatting(AList);
  for I := 0 to TdxDocListFormatting.MaxLevelCount - 1 do
    Result.LevelsFormatting.Add(CreateDocListLevel(TdxListLevel(AList.Levels[I]), I));
end;

function TdxDocListsExporter.CreateListFormatting(AList: TdxAbstractNumberingList): TdxDocListFormatting;
var
  I: Integer;
begin
  Result := TdxDocListFormatting.Create;
  Result.ListIdentifier := AList.Id;
  if (AList.StyleLinkIndex >= 0) or (AList.NumberingStyleReferenceIndex >= 0) then
    Result.TemplateCode := AList.TemplateCode
  else
    Result.TemplateCode := AList.GetHashCode;
  Result.SimpleList := False;
  for I := 0 to TdxDocListFormatting.MaxLevelCount - 1 do
    if AList.Levels[I].ParagraphStyle = nil then
      Result.LevelStyleIdentifiers[I] := EmptyStyleIdentifier
    else
      Result.LevelStyleIdentifiers[I] := SmallInt(StylesExportHelper.GetParagraphStyleIndex(TdxListLevel(AList.Levels[I]).ParagraphStyleIndex));
end;

function TdxDocListsExporter.CreateDocListLevel(AListLevel: TdxListLevel; AListLevelIndex: Integer): TdxDocListLevel;
begin
  Result := TdxDocListLevel.Create;
  Result.ListLevelProperties := CreateListLevelProperties(AListLevel.ListLevelProperties, AListLevelIndex);
  Result.CharacterUPX := StylesExportHelper.GetCharacterGroupPropertyModifiers(AListLevel, TdxDocStyleIndexes.DefaultCharacterStyleIndex);
  Result.ListLevelProperties.CharacterUPXSize := Byte(Length(Result.CharacterUPX));
  Result.ParagraphUPX := StylesExportHelper.GetParagraphGroupPropertyModifiers(AListLevel);
  Result.ListLevelProperties.ParagraphUPXSize := Byte(Length(Result.ParagraphUPX));
  Result.SetDisplayFormatString(AListLevel.ListLevelProperties.DisplayFormatString);
end;

function TdxDocListsExporter.CreateListLevelProperties(AListLevelProperties: TdxListLevelProperties; AListLevelIndex: Integer): TdxDocListLevelProperties;
begin
  Result := TdxDocListLevelProperties.Create;
  Result.Alignment := AListLevelProperties.Alignment;
  Result.ConvertPreviousLevelNumberingToDecimal := AListLevelProperties.ConvertPreviousLevelNumberingToDecimal;
  Result.Legacy := AListLevelProperties.Legacy;
  Result.LegacyIndent := AListLevelProperties.LegacyIndent;
  Result.LegacySpace := AListLevelProperties.LegacySpace;
  Result.NumberingFormat := AListLevelProperties.Format;
  if AListLevelProperties.SuppressRestart then
  begin
    Result.RestartLevelLimit := 0;
    Result.RestartAfterLevelLimit := True;
  end
  else
    if AListLevelProperties.RelativeRestartLevel <> 0 then
    begin
      Result.RestartLevelLimit := Byte(Max(0, AListLevelIndex - AListLevelProperties.RelativeRestartLevel));
      Result.RestartAfterLevelLimit := True;
    end;
  Result.Separator := AListLevelProperties.Separator;
  Result.Start := AListLevelProperties.Start;
end;

function TdxDocListsExporter.CreateListOverrideInfo: TdxDocListOverrideFormatInformation;
var
  ACount, I: Integer;
  AList: TdxNumberingList;
  ADocListOverrideData: TdxDocListOverrideLevelInformation;
begin
  Result := TdxDocListOverrideFormatInformation.Create;
  ACount := DocumentModel.NumberingLists.Count;
  for I := 0 to ACount - 1 do
  begin
    AList := DocumentModel.NumberingLists[I];
    ADocListOverrideData := CreateDocListOverrideData(AList);
    Result.FormatOverrideData.Add(ADocListOverrideData);
    Result.FormatOverride.Add(CreateDocListOverride(AList, ADocListOverrideData.LevelFormatOverrideData.Count));
  end;
end;

function TdxDocListsExporter.CreateDocListOverride(AList: TdxNumberingList; AOverrideLevelCount: Integer): TdxDocListOverrideFormat;
begin
  Result := TdxDocListOverrideFormat.Create;
  Result.ListIdentifier := AList.AbstractNumberingList.Id;
  Result.LevelsCount := Byte(AOverrideLevelCount);
end;

function TdxDocListsExporter.CreateDocListOverrideData(AList: TdxNumberingList): TdxDocListOverrideLevelInformation;
var
  ACount, I: Integer;
  AOverriddenLevel: TdxDocListOverrideLevelFormat;
begin
  Result := TdxDocListOverrideLevelInformation.Create;
  Result.CharacterPosition := EmptyCharacterPosition;
  ACount := AList.Levels.Count;
  for I := 0 to ACount - 1 do
  begin
    AOverriddenLevel := CreateOverrideLevel(AList.Levels[I] as IdxOverrideListLevel, I);
    if AOverriddenLevel <> nil then
      Result.LevelFormatOverrideData.Add(AOverriddenLevel);
  end;
end;

function TdxDocListsExporter.CreateOverrideLevel(const AListLevel: IdxOverrideListLevel; ALevelIndex: Integer): TdxDocListOverrideLevelFormat;
var
  ALevelFormat: TdxDocListOverrideLevelFormat;
  AOverrideListLevel: TdxOverrideListLevel;
begin
  ALevelFormat := TdxDocListOverrideLevelFormat.Create;
  ALevelFormat.StartAt := AListLevel.NewStart;
  ALevelFormat.OverriddenLevel := ALevelIndex;
  ALevelFormat.OverrideStart := AListLevel.OverrideStart;

  AOverrideListLevel := TdxOverrideListLevel(AListLevel);
  if AOverrideListLevel <> nil then
  begin
    ALevelFormat.OverrideFormatting := True;
    ALevelFormat.OverrideLevelFormatting := CreateDocListLevel(AOverrideListLevel, ALevelIndex);
  end;
  if ALevelFormat.OverrideStart or ALevelFormat.OverrideFormatting then
    Result := ALevelFormat
  else
  begin
    ALevelFormat.Free;
    Result := nil;
  end;
end;

{ TdxDocDataWriter }

constructor TdxDocDataWriter.Create(ADataStreamWriter: TBinaryWriter; AOptions: TdxDocDocumentExporterOptions; ADocumentModel: TdxDocumentModel);
begin
  Assert(ADataStreamWriter <> nil, 'dataStreamWriter');
  FFkpMemoryStream := TdxMemoryStream.Create;
  FSepxMemoryStream := TdxMemoryStream.Create;
  FFkpWriter := TdxFKPWriter.Create(TdxFileInformationBlock.FIBSize, FFkpMemoryStream, ADataStreamWriter);
  FSepxWriter := TdxSectionPropertiesWriter.Create(FSepxMemoryStream);
  FStylesExporter := TdxDocStylesExporter.Create(ADocumentModel);
  FListsExporter := TdxDocListsExporter.Create(ADocumentModel, AOptions, FStylesExporter);
  FTablesExporter := TdxDocTablesExporter.Create;
  FFieldsExporter := TdxDocFieldsExporter.Create;
  FNotesExporter := TdxDocNotesExporter.Create;

  FFloatingObjectsExporter := TdxDocFloatingObjectsExporter.Create(ADocumentModel.UnitConverter, ADocumentModel.DocumentProperties.PageBackColor);
  FBookmarkIterator := TdxDocBookmarkIterator.Create;
  FPermissionIterator := TdxDocRangeEditPermissionIterator.Create;
end;

destructor TdxDocDataWriter.Destroy;
begin
  FStylesExporter.Free;
  FListsExporter.Free;
  FTablesExporter.Free;
  FFieldsExporter.Free;
  FNotesExporter.Free;
  FFloatingObjectsExporter.Free;
  FBookmarkIterator.Free;
  FPermissionIterator.Free;
  FFkpWriter.Free;
  FSepxWriter.Free;
  FFkpMemoryStream.Free;
  FSepxMemoryStream.Free;
  inherited Destroy;
end;

function TdxDocDataWriter.GetFieldTable: TdxDocFieldTable;
begin
  Result := FFieldsExporter.GetFieldTable;
end;

procedure TdxDocDataWriter.WriteSection(ACharacterPosition: Integer; ASection: TdxSection);
begin
  SectionPropertiesWriter.WriteSection(ACharacterPosition, ASection);
end;

procedure TdxDocDataWriter.WriteSectionPositions(AWriter: TBinaryWriter);
begin
  SectionPropertiesWriter.SectionHelper.Write(AWriter);
end;

procedure TdxDocDataWriter.UpdateSectionsOffsets(AOffset: Integer);
begin
  SectionPropertiesWriter.SectionHelper.UpdateOffsets(AOffset);
end;

procedure TdxDocDataWriter.WriteCharactersBinTable(AWriter: TBinaryWriter);
begin
  FormattedDiskPageWriter.CharactersBinTable.Write(AWriter);
end;

procedure TdxDocDataWriter.WriteParagraphsBinTable(AWriter: TBinaryWriter);
begin
  FormattedDiskPageWriter.ParagraphsBinTable.Write(AWriter);
end;

procedure TdxDocDataWriter.WriteParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer);
begin
  FormattedDiskPageWriter.WriteParagraph(ACharacterPosition, AParagraphStyleIndex);
end;

procedure TdxDocDataWriter.WriteParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer; AParagraph: TdxParagraph);
var
  AGrpprl: TBytes;
begin
  if TablesExporter.InTable then
    AGrpprl := StylesExporter.GetTableParagraphPropertyModifiers(AParagraph, TablesExporter.TableDepth)
  else
    AGrpprl := StylesExporter.GetParagraphGroupPropertyModifiers(AParagraph);
  FormattedDiskPageWriter.WriteParagraph(ACharacterPosition, AParagraphStyleIndex, AGrpprl);
end;

procedure TdxDocDataWriter.WriteInTableParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer; AParagraph: TdxParagraph);
var
  AGrpprl: TBytes;
begin
  AGrpprl := TablesExporter.GetTableCellPropertyModifiers(AParagraph);
  FormattedDiskPageWriter.WriteParagraph(ACharacterPosition, AParagraphStyleIndex, AGrpprl);
end;

procedure TdxDocDataWriter.WriteParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer; ARow: TdxTableRow);
var
  ATableStyleIndex: Integer;
  AGrpprl: TBytes;
begin
  ATableStyleIndex := StylesExporter.GetStyleIndexByName(ARow.Table.TableStyle.StyleName);
  AGrpprl := TablesExporter.GetTableRowPropertyModifiers(ARow, ATableStyleIndex);
  FormattedDiskPageWriter.WriteTableProperties(ACharacterPosition, AParagraphStyleIndex, AGrpprl);
end;

procedure TdxDocDataWriter.WriteTextRun(ACharacterPosition: Integer);
begin
  FormattedDiskPageWriter.WriteTextRun(ACharacterPosition);
end;

procedure TdxDocDataWriter.WriteTextRun(ACharacterPosition: Integer; ATextRun: TdxTextRunBase; ASpecialSymbol: Boolean);
var
  AGrpprl: TBytes;
begin
  AGrpprl := StylesExporter.GetCharacterGroupPropertyModifiers(ATextRun, ASpecialSymbol);
  FormattedDiskPageWriter.WriteTextRun(ACharacterPosition, AGrpprl);
end;

procedure TdxDocDataWriter.WriteInlinePictureRun(ACharacterPosition, ACharacterStyleIndex, ADataStreamOffset: Integer;
  ACharacterProperties: TdxCharacterProperties);
var
  AGrpprl: TBytes;
begin
  AGrpprl := StylesExporter.GetInlinePicturePropertyModifiers(ACharacterProperties, ACharacterStyleIndex, ADataStreamOffset);
  FormattedDiskPageWriter.WriteTextRun(ACharacterPosition, AGrpprl);
end;

procedure TdxDocDataWriter.Finish(ALastCharacterPosition: Integer);
begin
  FormattedDiskPageWriter.Finish(ALastCharacterPosition);
  SectionPropertiesWriter.Finish(ALastCharacterPosition);
  FieldsExporter.Finish(ALastCharacterPosition);
  NotesExporter.FinishFootNoteReferences(ALastCharacterPosition);
  NotesExporter.FinishEndNoteReferences(ALastCharacterPosition);
  FloatingObjectsExporter.Finish(ALastCharacterPosition);
  BookmarkIterator.Finish(ALastCharacterPosition);
  PermissionIterator.Finish(ALastCharacterPosition);
end;

procedure TdxDocDataWriter.ExportFieldTables(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  FFieldsExporter.ExportFieldTables(AFib, AWriter);
end;

function TdxDocDataWriter.GetFormattedDiskPages: TBytes;
begin
  Result := FFkpMemoryStream.ToArray;
end;

function TdxDocDataWriter.GetSectionProperties: TBytes;
begin
  Result := FSepxMemoryStream.ToArray;
end;

procedure TdxDocDataWriter.SetState(AState: TdxDocContentState);
begin
  FieldsExporter.State := AState;
  FloatingObjectsExporter.State := AState;
end;

end.
