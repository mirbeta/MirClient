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
unit dxRichEdit.Import.Doc.DocImporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreGraphics, dxGenerics,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.Import.Doc.DocObjectCollection,
  dxRichEdit.Import.Doc.DocContentBuilder,
  dxRichEdit.Import.Doc.DCO,
  dxRichEdit.Import.Doc.DocStylesImportHelper,
  dxRichEdit.Import.Doc.DocListsImportHelper,
  dxRichEdit.Import.Doc.DocTablesImportHelper,
  dxRichEdit.Import.Core,
  dxRichEdit.Import,
  dxRichEdit.Doc.Utils;

type
  TdxDocImporter = class;

  { TdxDocImportPieceTableInfo }

  TdxDocImportPieceTableInfo = class
  strict private
    FPieceTable: TdxPieceTable;
    FPieceTableInfo: TdxImportPieceTableInfo;
    FFieldsImporter: TdxDocFieldsImporter;
    FTablesImporter: TdxDocTablesImporter;
  public
    constructor Create(AImporter: TdxDocImporter; APieceTable: TdxPieceTable);
    destructor Destroy; override;

    property PieceTable: TdxPieceTable read FPieceTable;
    property PieceTableInfo: TdxImportPieceTableInfo read FPieceTableInfo;
    property FieldsImporter: TdxDocFieldsImporter read FFieldsImporter;
    property TablesImporter: TdxDocTablesImporter read FTablesImporter;
  end;

  { TdxDocTableCellHorizontalMerging }

  TdxDocTableCellHorizontalMerging = class
  strict private
    FFirstCellIndex: Integer;
    FCount: Integer;
  public
    constructor Create(AFirstCellIndex: Integer; ACount: Integer);

    property FirstCellIndex: Integer read FFirstCellIndex;
    property Count: Integer read FCount;
  end;

  { TdxDocTableCellHorizontalMergingList }

  TdxDocTableCellHorizontalMergingList = class(TdxObjectList<TdxDocTableCellHorizontalMerging>);

  { TdxDocImporter }

  TdxInsertDocObjectDelegate = reference to procedure(const ADocObject: IdxDocObject;
    APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);

  TdxDocImporter = class(TdxDocumentModelImporter)
  strict private
    FImportInfoStack: TObjectStack<TdxDocImportPieceTableInfo>;
    FContentBuilder: TdxDocContentBuilder;
    FCurrentSectionIndex: TdxSectionIndex;
    FStylesImportHelper: TdxDocStylesImportHelper;
    FListsImportHelper: TdxDocListsImportHelper;
    FDelayedFormatting: TDictionary<TdxTextRunBase, TdxCharacterInfo>;
    FDocObjectDispatcher: TDictionary<TdxDocObjectType, TdxInsertDocObjectDelegate>;
    FCellsHorizontalMerging: TObjectDictionary<TdxTableRow, TdxDocTableCellHorizontalMergingList>;
    FIsContainsParagraphFrame: Boolean;
    function GetDocumentModel: TdxDocumentModel;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
    function GetOptions: TdxDocDocumentImporterOptions;
    function GetActiveImportInfo: TdxDocImportPieceTableInfo;
  protected
    procedure ImportDocument; virtual;
    procedure ImportContent; virtual;
    procedure Initialize;
    procedure CreateMainText;
    procedure SetDocumentSettings;
    procedure SetDocumentProperties;
    procedure SetRemovePersonalInformation;
    procedure SetCustomProperties;
    procedure SetPageBackground;
    procedure SetDocumentProtection;
    procedure SetHeadersFooters;
    procedure SetHeadersFootersCore(ASectionIndex: Integer);
    procedure SetEvenPageHeader(ASectionIndex: Integer);
    procedure SetEvenPageFooter(ASectionIndex: Integer);
    procedure SetOddPageHeader(ASectionIndex: Integer);
    procedure SetOddPageFooter(ASectionIndex: Integer);
    procedure SetFirstPageHeader(ASectionIndex: Integer);
    procedure SetFirstPageFooter(ASectionIndex: Integer);
    procedure SetDocumentVariables;
    procedure ProcessDocObject(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure Finish(ATargetPieceTable: TdxPieceTable);
    procedure FixAbsentLastParagraph(ATargetPieceTable: TdxPieceTable);
    procedure InsertText(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertExpectedObjectAsText(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertTextCore(const APlainText: string; AInfo: TdxCharacterInfo; APieceTable: TdxPieceTable; APos: TdxInputPosition);
    procedure InsertPictureFloatingObject(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertPictureFloatingObjectCore(AFloatingObject: TdxDocPictureFloatingObject; AInputPosition: TdxInputPosition);
    procedure InsertTextBoxFloatingObject(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertTextBoxFloatingObjectCore(ATextBox: TdxDocTextBoxFloatingObject; AInputPosition: TdxInputPosition);
    procedure InsertImage(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertImageCore(AImage: TdxDocImage; APieceTable: TdxPieceTable; APosition: TdxInputPosition);
    procedure InsertFieldBegin(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertFieldSeparator(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertFieldEnd(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertHyperlinkFieldData(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertFootNoteReference(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertFootNoteContent(ANote: TdxFootNote; ACharacterPosition: Integer);
    procedure InsertEndnoteReference(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertEndNoteContent(ANote: TdxEndNote; ACharacterPosition: Integer);
    procedure InsertEmbeddedContent(APieceTable: TdxPieceTable; AContent: TdxDocObjectCollection);
    procedure InsertNoteNumber(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure TryInsertFootNoteNumber(APosition: TdxInputPosition; APieceTable: TdxPieceTable);
    procedure TryInsertEndNoteNumber(APosition: TdxInputPosition; APieceTable: TdxPieceTable);
    procedure InsertAnnotationReference(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertCommentContent(ACommentContent: TdxCommentContentType; ACharacterPosition: Integer);
    procedure InsertTableCell(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure InsertTableRow(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure ProcessParagraphListInfoIndex(APieceTable: TdxPieceTable; AIndex: TdxParagraphIndex; AInfo: TdxParagraphInfo);
    procedure SpecifyCharacterFormattingForInputPosition(AInfo: TdxCharacterInfo; APosition: TdxInputPosition);
    procedure AddDelayedTextFormatting(AInfo: TdxCharacterInfo; APieceTable: TdxPieceTable);
    procedure AddDelayedImageFormatting(AInfo: TdxCharacterInfo; APieceTable: TdxPieceTable);
    procedure AddDelayedFormattingCore(AInfo: TdxCharacterInfo; APieceTable: TdxPieceTable; ARun: TdxTextRunBase);
    procedure ProcessDelayedFormatting;
    procedure InsertParagraph(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure SetDefaultDocumentProperties; virtual;
    procedure SetCharacterProperties(AInputPosition: TdxInputPosition; APropertyContainer: TdxDocPropertyContainer);
    procedure SetParagraphProperties(APieceTable: TdxPieceTable; AParagraphIndex: TdxParagraphIndex; APropertyContainer: TdxDocPropertyContainer; ASectionMark: Boolean);
    procedure InsertSection(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
    procedure ApplySectionProperties(APropertyContainer: TdxDocPropertyContainer);
    procedure UpdateActiveImportInfo(APieceTable: TdxPieceTable);

    procedure ImportCore(AStream: TStream); override;

    property ImportInfoStack: TObjectStack<TdxDocImportPieceTableInfo> read FImportInfoStack;
    property ActiveImportInfo: TdxDocImportPieceTableInfo read GetActiveImportInfo;
    property StylesImportHelper: TdxDocStylesImportHelper read FStylesImportHelper;
    property ListsImportHelper: TdxDocListsImportHelper read FListsImportHelper;
    property DocObjectDispatcher: TDictionary<TdxDocObjectType, TdxInsertDocObjectDelegate> read FDocObjectDispatcher;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions); override;
    destructor Destroy; override;
    procedure CreateDocObjectDispatcher;
    procedure ApplyHorizontalMerging;
    procedure MergeCellsHorizontally(ARow: TdxTableRow; AMerging: TdxDocTableCellHorizontalMerging);
    class procedure ThrowInvalidDocFile; static;
    class procedure ThrowInvalidFile; override;
    procedure AddCellsHorizontalMerging(ARow: TdxTableRow; AFirstCellIndex: Integer; ASpan: Integer);

    property CellsHorizontalMerging: TObjectDictionary<TdxTableRow, TdxDocTableCellHorizontalMergingList> read FCellsHorizontalMerging;
    property ContentBuilder: TdxDocContentBuilder read FContentBuilder;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
    property IsContainsParagraphFrame: Boolean read FIsContainsParagraphFrame write FIsContainsParagraphFrame;
    property Options: TdxDocDocumentImporterOptions read GetOptions;
  end;

implementation

uses
  Contnrs,
  dxStringHelper,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.Import.Doc.DocCharacterFormattingInfo,
  dxRichEdit.Import.Doc.DocContentIterator,
  dxRichEdit.Options.Simple,
  dxRichEdit.Import.Doc.DocumentProperties,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.NativeApi;

type
  { TdxDocTableCellHorizontalMergingComparer }

  TdxDocTableCellHorizontalMergingComparer = class(TInterfacedObject, IComparer<TdxDocTableCellHorizontalMerging>)
  protected
    function Compare(const Left, Right: TdxDocTableCellHorizontalMerging): Integer;
  end;

{ TdxDocTableCellHorizontalMergingComparer }

function TdxDocTableCellHorizontalMergingComparer.Compare(const Left, Right: TdxDocTableCellHorizontalMerging): Integer;
begin
  Result := Right.FirstCellIndex - Left.FirstCellIndex
end;

{ TdxDocImportPieceTableInfo }

constructor TdxDocImportPieceTableInfo.Create(AImporter: TdxDocImporter; APieceTable: TdxPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FPieceTableInfo := TdxImportPieceTableInfo.Create(APieceTable);
  FTablesImporter := TdxDocTablesImporter.Create(AImporter, APieceTable);
  FFieldsImporter := TdxDocFieldsImporter.Create(APieceTable, FPieceTableInfo.FieldInfoStack);
end;

destructor TdxDocImportPieceTableInfo.Destroy;
begin
  FreeAndNil(FPieceTableInfo);
  FreeAndNil(FTablesImporter);
  FreeAndNil(FFieldsImporter);
  inherited Destroy;
end;

{ TdxDocTableCellHorizontalMerging }

constructor TdxDocTableCellHorizontalMerging.Create(AFirstCellIndex: Integer; ACount: Integer);
begin
  inherited Create;
  FFirstCellIndex := AFirstCellIndex;
  FCount := ACount;
end;

{ TdxDocImporter }

constructor TdxDocImporter.Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FImportInfoStack := TObjectStack<TdxDocImportPieceTableInfo>.Create;
  CreateDocObjectDispatcher;
end;

destructor TdxDocImporter.Destroy;
begin
  FreeAndNil(FImportInfoStack);
  FreeAndNil(FDocObjectDispatcher);
  inherited Destroy;
end;

function TdxDocImporter.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxDocImporter.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := DocumentModel.UnitConverter;
end;

function TdxDocImporter.GetOptions: TdxDocDocumentImporterOptions;
begin
  Result := TdxDocDocumentImporterOptions(inherited Options);
end;

function TdxDocImporter.GetActiveImportInfo: TdxDocImportPieceTableInfo;
begin
  Result := ImportInfoStack.Peek;
end;

procedure TdxDocImporter.CreateDocObjectDispatcher;
begin
  FDocObjectDispatcher := TDictionary<TdxDocObjectType, TdxInsertDocObjectDelegate>.Create;
  FDocObjectDispatcher.Add(TdxDocObjectType.InlineImage, InsertImage);
  FDocObjectDispatcher.Add(TdxDocObjectType.PictureFloatingObject, InsertPictureFloatingObject);
  FDocObjectDispatcher.Add(TdxDocObjectType.TextBoxFloatingObject, InsertTextBoxFloatingObject);
  FDocObjectDispatcher.Add(TdxDocObjectType.TextRun, InsertText);
  FDocObjectDispatcher.Add(TdxDocObjectType.FieldBegin, InsertFieldBegin);
  FDocObjectDispatcher.Add(TdxDocObjectType.FieldSeparator, InsertFieldSeparator);
  FDocObjectDispatcher.Add(TdxDocObjectType.FieldEnd, InsertFieldEnd);
  FDocObjectDispatcher.Add(TdxDocObjectType.HyperlinkFieldData, InsertHyperlinkFieldData);
  FDocObjectDispatcher.Add(TdxDocObjectType.AutoNumberedFootnoteReference, InsertFootNoteReference);
  FDocObjectDispatcher.Add(TdxDocObjectType.AnnotationReference, InsertAnnotationReference);
  FDocObjectDispatcher.Add(TdxDocObjectType.EndnoteReference, InsertEndnoteReference);
  FDocObjectDispatcher.Add(TdxDocObjectType.NoteNumber, InsertNoteNumber);
  FDocObjectDispatcher.Add(TdxDocObjectType.TableCell, InsertTableCell);
  FDocObjectDispatcher.Add(TdxDocObjectType.TableRow, InsertTableRow);
  FDocObjectDispatcher.Add(TdxDocObjectType.Paragraph, InsertParagraph);
  FDocObjectDispatcher.Add(TdxDocObjectType.Section, InsertSection);
  FDocObjectDispatcher.Add(TdxDocObjectType.ExpectedFieldBegin, InsertExpectedObjectAsText);
  FDocObjectDispatcher.Add(TdxDocObjectType.ExpectedFieldSeparator, InsertExpectedObjectAsText);
  FDocObjectDispatcher.Add(TdxDocObjectType.ExpectedFieldEnd, InsertExpectedObjectAsText);
end;

procedure TdxDocImporter.ImportCore(AStream: TStream);
var
  AOptions: TdxFieldUpdateOnLoadOptions;
begin
  DocumentModel.BeginSetContent;
  try
    SetDefaultDocumentProperties;
    FCellsHorizontalMerging := TObjectDictionary<TdxTableRow, TdxDocTableCellHorizontalMergingList>.Create([doOwnsValues]);
    FContentBuilder := TdxDocContentBuilder.Create(DocumentModel, TdxDocDocumentImporterOptions(Options));
    try
      try
        ContentBuilder.BuildDocumentContent(AStream, DocumentModel);
        ImportDocument;
      except
        DocumentModel.ClearDocument;
        raise ;
      end;
    finally
      FreeAndNil(FCellsHorizontalMerging);
      FreeAndNil(FContentBuilder);
    end;
  finally
    AOptions := Options.UpdateField.GetNativeOptions;
    try
      DocumentModel.EndSetContent(TdxDocumentModelChangeType.LoadNewDocument, True, False, AOptions);
    finally
      AOptions.Free;
    end;
  end;
end;

procedure TdxDocImporter.ApplyHorizontalMerging;
var
  ATables: TdxList<TdxTable>;
  AMerging: TdxDocTableCellHorizontalMerging;
  AKeyValuePair: TPair<TdxTableRow, TdxDocTableCellHorizontalMergingList>;
  ACellsMerging: TdxDocTableCellHorizontalMergingList;
  AComparer: IComparer<TdxDocTableCellHorizontalMerging>;
  I: Integer;
begin
  ATables := TdxList<TdxTable>.Create;
  try
    AComparer := TdxDocTableCellHorizontalMergingComparer.Create;
    for AKeyValuePair in FCellsHorizontalMerging do
    begin
      ACellsMerging := AKeyValuePair.Value;
      ACellsMerging.Sort(AComparer);
      for I := 0 to ACellsMerging.Count - 1 do
      begin
        AMerging := ACellsMerging[I];
        MergeCellsHorizontally(AKeyValuePair.Key, AMerging);
      end;
      ATables.Add(AKeyValuePair.Key.Table);
    end;
    for I := 0 to ATables.Count - 1 do
      ATables[I].NormalizeCellColumnSpans;
  finally
    ATables.Free;
  end;
end;

procedure TdxDocImporter.MergeCellsHorizontally(ARow: TdxTableRow; AMerging: TdxDocTableCellHorizontalMerging);
var
  APieceTable: TdxPieceTable;
  ASpan, AFirstCellIndex, ASummaryWidth, I, ACellIndex: Integer;
  ACell, AFirstCell: TdxTableCell;
begin
  APieceTable := TdxPieceTable(ARow.PieceTable);
  ASpan := AMerging.Count;
  AFirstCellIndex := AMerging.FirstCellIndex;
  ASummaryWidth := 0;
  for I := ASpan - 2 downto 0 do
  begin
    ACellIndex := I + AFirstCellIndex + 1;
    ACell := ARow.Cells[ACellIndex];
    if ACell.PreferredWidth.&Type = TdxWidthUnitType.ModelUnits then
      Inc(ASummaryWidth, ACell.PreferredWidth.Value);
    APieceTable.TableCellsManager.RemoveTableCell(ACell);
    APieceTable.DeleteContent(APieceTable.Paragraphs[ACell.StartParagraphIndex].LogPosition, 1, False);
    ARow.Cells.DeleteInternal(ACell);
  end;
  AFirstCell := ARow.Cells[AFirstCellIndex];
  AFirstCell.ColumnSpan := AFirstCell.ColumnSpan + ASpan - 1;
  if AFirstCell.Properties.PreferredWidth.&Type = TdxWidthUnitType.ModelUnits then
    AFirstCell.Properties.PreferredWidth.Value := AFirstCell.Properties.PreferredWidth.Value + ASummaryWidth;
end;

procedure TdxDocImporter.ImportDocument;
begin
  FDelayedFormatting := TDictionary<TdxTextRunBase, TdxCharacterInfo>.Create;
  FStylesImportHelper := TdxDocStylesImportHelper.Create(ContentBuilder, DocumentModel);
  FListsImportHelper := TdxDocListsImportHelper.Create(ContentBuilder, DocumentModel);
  try
    ImportContent;
    DocumentModel.NormalizeZOrder;
  finally
    FreeAndNil(FDelayedFormatting);
    FreeAndNil(FStylesImportHelper);
    FreeAndNil(FListsImportHelper);
  end;
end;

procedure TdxDocImporter.ImportContent;
begin
  Initialize;
  CreateMainText;
  SetHeadersFooters;
  SetDocumentSettings;
  SetDocumentProperties;
  SetCustomProperties;
  SetDocumentVariables;
end;

procedure TdxDocImporter.Initialize;
begin
  ListsImportHelper.InitializeAbstractLists;
  ListsImportHelper.InitializeLists;
  StylesImportHelper.SetDocunentDefaults;
  StylesImportHelper.InitializeStyles;
  ListsImportHelper.LinkNumberingListStyles(StylesImportHelper);
end;

procedure TdxDocImporter.CreateMainText;
var
  ADocObjects: TdxDocObjectCollection;
begin
  ADocObjects := ContentBuilder.Iterator.MainTextDocObjects;
  FCurrentSectionIndex := 0;
  InsertEmbeddedContent(DocumentModel.MainPieceTable, ADocObjects);
  ApplySectionProperties(ADocObjects[ADocObjects.Count - 1].PropertyContainer);
end;

procedure TdxDocImporter.SetDocumentSettings;
begin
  SetRemovePersonalInformation;
  SetPageBackground;
  SetDocumentProtection;
end;

procedure TdxDocImporter.SetDocumentProperties;
begin
end;

procedure TdxDocImporter.SetRemovePersonalInformation;
begin
end;

procedure TdxDocImporter.SetCustomProperties;
begin
end;

procedure TdxDocImporter.SetPageBackground;
begin
  DocumentModel.DocumentProperties.DisplayBackgroundShape := ContentBuilder.DocumentProperties.DisplayBackgroundShape;
end;

procedure TdxDocImporter.SetDocumentProtection;
var
  AProperties: TdxDocumentProtectionProperties;
  ADocumentProperties: TdxDocDocumentProperties;
begin
  AProperties := DocumentModel.ProtectionProperties;
  AProperties.BeginInit;
  ADocumentProperties := ContentBuilder.DocumentProperties;
  AProperties.EnforceProtection := ADocumentProperties.EnforceProtection;
  AProperties.ProtectionType := ADocumentProperties.ProtectionType;
  AProperties.Word2003PasswordHash := ADocumentProperties.PasswordHash;
  AProperties.EndInit;
end;

procedure TdxDocImporter.SetHeadersFooters;
var
  I: Integer;
begin
  DocumentModel.DocumentProperties.DifferentOddAndEvenPages := ContentBuilder.DocumentProperties.DifferentOddAndEvenPages;
  for I := 0 to DocumentModel.Sections.Count - 1 do
    SetHeadersFootersCore(I);
end;

procedure TdxDocImporter.SetHeadersFootersCore(ASectionIndex: Integer);
begin
  SetEvenPageHeader(ASectionIndex);
  SetOddPageHeader(ASectionIndex);
  SetEvenPageFooter(ASectionIndex);
  SetOddPageFooter(ASectionIndex);
  SetFirstPageHeader(ASectionIndex);
  SetFirstPageFooter(ASectionIndex);
end;

procedure TdxDocImporter.SetEvenPageHeader(ASectionIndex: Integer);
var
  ADocObjects: TdxDocObjectCollection;
  ACurrentSection: TdxSection;
  AEvenPageHeader: TdxSectionHeader;
  AEvenPageHeaderPieceTable: TdxPieceTable;
begin
  ADocObjects := ContentBuilder.Iterator.HeadersFooters.GetEvenPageHeaderObjects(ASectionIndex);
  ACurrentSection := DocumentModel.Sections[ASectionIndex];
  if (ADocObjects = nil) or (ADocObjects.Count = 0) then
  begin
    ACurrentSection.Headers.LinkToPrevious(TdxHeaderFooterType.Even);
    Exit;
  end;
  ACurrentSection.Headers.Add(TdxHeaderFooterType.Even);
  AEvenPageHeader := ACurrentSection.InnerEvenPageHeader;
  AEvenPageHeaderPieceTable := TdxPieceTable(AEvenPageHeader.PieceTable);
  InsertEmbeddedContent(AEvenPageHeaderPieceTable, ADocObjects);
end;

procedure TdxDocImporter.SetEvenPageFooter(ASectionIndex: Integer);
var
  ADocObjects: TdxDocObjectCollection;
  ACurrentSection: TdxSection;
  AEvenPageFooter: TdxSectionFooter;
  AEvenPageFooterPieceTable: TdxPieceTable;
begin
  ADocObjects := ContentBuilder.Iterator.HeadersFooters.GetEvenPageFooterObjects(ASectionIndex);
  ACurrentSection := DocumentModel.Sections[ASectionIndex];
  if (ADocObjects = nil) or (ADocObjects.Count = 0) then
  begin
    ACurrentSection.Footers.LinkToPrevious(TdxHeaderFooterType.Even);
    Exit;
  end;
  ACurrentSection.Footers.Add(TdxHeaderFooterType.Even);
  AEvenPageFooter := ACurrentSection.InnerEvenPageFooter;
  AEvenPageFooterPieceTable := TdxPieceTable(AEvenPageFooter.PieceTable);
  InsertEmbeddedContent(AEvenPageFooterPieceTable, ADocObjects);
end;

procedure TdxDocImporter.SetOddPageHeader(ASectionIndex: Integer);
var
  ADocObjects: TdxDocObjectCollection;
  ACurrentSection: TdxSection;
  AOddPageHeader: TdxSectionHeader;
  AOddPageHeaderPieceTable: TdxPieceTable;
begin
  ADocObjects := ContentBuilder.Iterator.HeadersFooters.GetOddPageHeaderObjects(ASectionIndex);
  ACurrentSection := DocumentModel.Sections[ASectionIndex];
  if (ADocObjects = nil) or (ADocObjects.Count = 0) then
  begin
    ACurrentSection.Headers.LinkToPrevious(TdxHeaderFooterType.Odd);
    Exit;
  end;
  ACurrentSection.Headers.Add(TdxHeaderFooterType.Odd);
  AOddPageHeader := ACurrentSection.InnerOddPageHeader;
  AOddPageHeaderPieceTable := TdxPieceTable(AOddPageHeader.PieceTable);
  InsertEmbeddedContent(AOddPageHeaderPieceTable, ADocObjects);
end;

procedure TdxDocImporter.SetOddPageFooter(ASectionIndex: Integer);
var
  ADocObjects: TdxDocObjectCollection;
  ACurrentSection: TdxSection;
  AOddPageFooter: TdxSectionFooter;
  AOddPageFooterPieceTable: TdxPieceTable;
begin
  ADocObjects := ContentBuilder.Iterator.HeadersFooters.GetOddPageFooterObjects(ASectionIndex);
  ACurrentSection := DocumentModel.Sections[ASectionIndex];
  if (ADocObjects = nil) or (ADocObjects.Count = 0) then
  begin
    ACurrentSection.Footers.LinkToPrevious(TdxHeaderFooterType.Odd);
    Exit;
  end;
  ACurrentSection.Footers.Add(TdxHeaderFooterType.Odd);
  AOddPageFooter := ACurrentSection.InnerOddPageFooter;
  AOddPageFooterPieceTable := TdxPieceTable(AOddPageFooter.PieceTable);
  InsertEmbeddedContent(AOddPageFooterPieceTable, ADocObjects);
end;

procedure TdxDocImporter.SetFirstPageHeader(ASectionIndex: Integer);
var
  ADocObjects: TdxDocObjectCollection;
  ACurrentSection: TdxSection;
  AFirstPageHeader: TdxSectionHeader;
  AFirstPageHeaderPieceTable: TdxPieceTable;
begin
  ADocObjects := ContentBuilder.Iterator.HeadersFooters.GetFirstPageHeaderObjects(ASectionIndex);
  ACurrentSection := DocumentModel.Sections[ASectionIndex];
  if (ADocObjects = nil) or (ADocObjects.Count = 0) then
  begin
    ACurrentSection.Headers.LinkToPrevious(TdxHeaderFooterType.First);
    Exit;
  end;
  ACurrentSection.Headers.Add(TdxHeaderFooterType.First);
  AFirstPageHeader := ACurrentSection.InnerFirstPageHeader;
  AFirstPageHeaderPieceTable := TdxPieceTable(AFirstPageHeader.PieceTable);
  InsertEmbeddedContent(AFirstPageHeaderPieceTable, ADocObjects);
end;

procedure TdxDocImporter.SetFirstPageFooter(ASectionIndex: Integer);
var
  ADocObjects: TdxDocObjectCollection;
  ACurrentSection: TdxSection;
  AFirstPageFooter: TdxSectionFooter;
  AFirstPageFooterPieceTable: TdxPieceTable;
begin
  ADocObjects := ContentBuilder.Iterator.HeadersFooters.GetFirstPageFooterObjects(ASectionIndex);
  ACurrentSection := DocumentModel.Sections[ASectionIndex];
  if (ADocObjects = nil) or (ADocObjects.Count = 0) then
  begin
    ACurrentSection.Footers.LinkToPrevious(TdxHeaderFooterType.First);
    Exit;
  end;
  ACurrentSection.Footers.Add(TdxHeaderFooterType.First);
  AFirstPageFooter := ACurrentSection.InnerFirstPageFooter;
  AFirstPageFooterPieceTable := TdxPieceTable(AFirstPageFooter.PieceTable);
  InsertEmbeddedContent(AFirstPageFooterPieceTable, ADocObjects);
end;

procedure TdxDocImporter.SetDocumentVariables;
begin
  ContentBuilder.DocumentVariables.SetVariables(DocumentModel.Variables, DocumentModel.DocumentProperties);
end;

procedure TdxDocImporter.ProcessDocObject(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
var
  AAction: TdxInsertDocObjectDelegate;
begin
  if DocObjectDispatcher.TryGetValue(ADocObject.DocObjectType, AAction) then
  begin
    ContentBuilder.Iterator.AdvancePosition(AInputPosition.LogPosition, ADocObject.Position, ADocObject.Length);
    ContentBuilder.FontManager.SetFontName(ADocObject.PropertyContainer);
    AAction(ADocObject, APieceTable, AInputPosition);
  end;
end;

procedure TdxDocImporter.Finish(ATargetPieceTable: TdxPieceTable);
var
  AHideLastParagraph: Boolean;
begin
  AHideLastParagraph := ActiveImportInfo.TablesImporter.Finish;
  if ActiveImportInfo.FieldsImporter.ImportFieldStack.Count <> 0 then
    ThrowInvalidDocFile;

  ContentBuilder.Iterator.InsertBookmarks(ATargetPieceTable);
  ATargetPieceTable.FixLastParagraph;
  ATargetPieceTable.FixTables;
  if not ATargetPieceTable.IsMain and AHideLastParagraph then
    FixAbsentLastParagraph(ATargetPieceTable);
  if (IsContainsParagraphFrame) or (FStylesImportHelper.IsContainsParagraphFrame) then
    ATargetPieceTable.FixParagraphFramesInTables;
  ImportInfoStack.Pop;
end;

procedure TdxDocImporter.FixAbsentLastParagraph(ATargetPieceTable: TdxPieceTable);
var
  ALastRun: TdxTextRunBase;
begin
  ALastRun := ATargetPieceTable.Runs[ATargetPieceTable.Runs.Count - 1];
  (ALastRun as IdxBatchUpdateable).BeginUpdate;
  try
    ALastRun.DoubleFontSize := 2;
    ALastRun.Hidden := True;
  finally
    (ALastRun as IdxBatchUpdateable).EndUpdate;
  end;
end;

procedure TdxDocImporter.InsertText(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
begin
  InsertTextCore((TdxDocTextRun(ADocObject)).Text, ADocObject.PropertyContainer.CharacterInfo, APieceTable, AInputPosition);
end;

procedure TdxDocImporter.InsertExpectedObjectAsText(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
begin
  InsertTextCore((TdxExpectedDocObject(ADocObject)).Text, ADocObject.PropertyContainer.CharacterInfo, APieceTable, AInputPosition);
end;

procedure TdxDocImporter.InsertTextCore(const APlainText: string;
  AInfo: TdxCharacterInfo; APieceTable: TdxPieceTable; APos: TdxInputPosition);
var
  S: string;
begin
  SpecifyCharacterFormattingForInputPosition(AInfo, APos);
  S := TdxStringHelper.ReplaceParagraphMarksWithLineBreaks(APlainText);
  if S <> '' then
    APieceTable.InsertTextCore(APos, S);
  AddDelayedTextFormatting(AInfo, APieceTable);
end;

procedure TdxDocImporter.InsertPictureFloatingObject(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
begin
  InsertPictureFloatingObjectCore(TdxDocPictureFloatingObject(ADocObject), AInputPosition);
end;

procedure TdxDocImporter.InsertPictureFloatingObjectCore(AFloatingObject: TdxDocPictureFloatingObject; AInputPosition: TdxInputPosition);
var
  ARun: TdxFloatingObjectAnchorRun;
  AImage: TdxOfficeImageReference;
begin
  SpecifyCharacterFormattingForInputPosition(AFloatingObject.PropertyContainer.CharacterInfo, AInputPosition);
  ARun := ActiveImportInfo.PieceTable.InsertFloatingObjectAnchorCore(AInputPosition);
  ARun.FloatingObjectProperties.CopyFrom(AFloatingObject.Formatting);
  AFloatingObject.ApplyShapeProperties(ARun.Shape);
  AImage := AFloatingObject.Image;
  ARun.SetContent(TdxPictureFloatingObjectContent.Create(ARun, AImage));
end;

procedure TdxDocImporter.InsertTextBoxFloatingObject(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
begin
  InsertTextBoxFloatingObjectCore(TdxDocTextBoxFloatingObject(ADocObject), AInputPosition);
end;

procedure TdxDocImporter.InsertTextBoxFloatingObjectCore(ATextBox: TdxDocTextBoxFloatingObject; AInputPosition: TdxInputPosition);
var
  APieceTable: TdxPieceTable;
  ARun: TdxFloatingObjectAnchorRun;
  ATextBoxContentType: TdxTextBoxContentType;
  AContent: TdxTextBoxFloatingObjectContent;
  AIterator: TdxDocFloatingObjectsIterator;
  AId: Integer;
  ATextBoxContent: TdxDocObjectCollection;
begin
  SpecifyCharacterFormattingForInputPosition(ATextBox.PropertyContainer.CharacterInfo, AInputPosition);
  APieceTable := ActiveImportInfo.PieceTable;
  ARun := APieceTable.InsertFloatingObjectAnchorCore(AInputPosition);
  ARun.FloatingObjectProperties.CopyFrom(ATextBox.Formatting);
  ATextBox.ApplyShapeProperties(ARun.Shape);
  AddDelayedTextFormatting(ATextBox.PropertyContainer.CharacterInfo, APieceTable);
  ATextBoxContentType := TdxTextBoxContentType.Create(DocumentModel);
  AContent := TdxTextBoxFloatingObjectContent.Create(ARun, ATextBoxContentType);
  ARun.SetContent(AContent);
  ATextBox.ApplyTextBoxProperties(AContent.TextBoxProperties);
  AIterator := ContentBuilder.Iterator.FloatingObjectsIterator;
  AId := ATextBox.ShapeId;
  if APieceTable = DocumentModel.MainPieceTable then
    ATextBoxContent := AIterator.GetMainTextBoxObjects(AId)
  else
    ATextBoxContent := AIterator.GetHeaderTextBoxObjects(AId);
  InsertEmbeddedContent(TdxPieceTable(ATextBoxContentType.PieceTable), ATextBoxContent);
end;

procedure TdxDocImporter.InsertImage(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
var
  AImage: TdxDocImage;
begin
  if (ADocObject <> nil) and (ADocObject.GetObject is TdxDocImage) then
  begin
    AImage := TdxDocImage(ADocObject.GetObject);
    InsertImageCore(AImage, APieceTable, AInputPosition);
  end;
end;

procedure TdxDocImporter.InsertImageCore(AImage: TdxDocImage; APieceTable: TdxPieceTable; APosition: TdxInputPosition);
var
  AReference: TdxOfficeImageReference;
begin
  if not DocumentModel.DocumentCapabilities.InlinePicturesAllowed then
    Exit;
  SpecifyCharacterFormattingForInputPosition(AImage.PropertyContainer.CharacterInfo, APosition);
  AReference := AImage.Image;
  ContentBuilder.RemoveFromGC(AImage.Image);
  APieceTable.AppendImage(APosition, AReference, AImage.ScaleX, AImage.ScaleY);
  AddDelayedImageFormatting(AImage.PropertyContainer.CharacterInfo, APieceTable);
end;

procedure TdxDocImporter.InsertFieldBegin(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
begin
  ActiveImportInfo.FieldsImporter.ProcessFieldBegin(AInputPosition);
end;

procedure TdxDocImporter.InsertFieldSeparator(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
begin
  ActiveImportInfo.FieldsImporter.ProcessFieldSeparator(AInputPosition);
end;

procedure TdxDocImporter.InsertFieldEnd(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
begin
  ActiveImportInfo.FieldsImporter.ProcessFieldEnd(AInputPosition, ADocObject.PropertyContainer);
end;

procedure TdxDocImporter.InsertHyperlinkFieldData(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
var
  AHyperlinkData: TdxDocHyperlinkFieldData;
begin
  AHyperlinkData := TdxDocHyperlinkFieldData(ADocObject);
  ActiveImportInfo.FieldsImporter.ProcessHyperlinkData(AHyperlinkData);
end;

procedure TdxDocImporter.InsertFootNoteReference(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
var
  AFootNote: TdxFootNote;
  APieceTableFootNote: TdxPieceTable;
  AIndex: Integer;
  AInfo: TdxCharacterInfo;
  ARun: TdxFootNoteRun;
begin
  AFootNote := TdxFootNote.Create(DocumentModel);
  APieceTableFootNote := TdxPieceTable(AFootNote.PieceTable);
  AIndex := DocumentModel.FootNotes.Count;
  DocumentModel.UnsafeEditor.InsertFirstParagraph(APieceTableFootNote);
  DocumentModel.FootNotes.Add(AFootNote);
  AInfo := ADocObject.PropertyContainer.CharacterInfo;
  SpecifyCharacterFormattingForInputPosition(AInfo, AInputPosition);
  ARun := TdxFootNoteRun(APieceTable.InsertFootNoteRun(AInputPosition, AIndex));
  AddDelayedTextFormatting(AInfo, APieceTable);
  AFootNote.ReferenceRun := ARun;
  InsertFootNoteContent(AFootNote, ADocObject.Position);
end;

procedure TdxDocImporter.InsertFootNoteContent(ANote: TdxFootNote; ACharacterPosition: Integer);
var
  ANoteContent: TdxDocObjectCollection;
begin
  ANoteContent := ContentBuilder.Iterator.NotesIterator.GetFootNoteObjects(ACharacterPosition);
  InsertEmbeddedContent(TdxPieceTable(ANote.PieceTable), ANoteContent);
end;

procedure TdxDocImporter.InsertEndnoteReference(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
var
  AEndNote: TdxEndNote;
  AEndNotePieceTable: TdxPieceTable;
  AIndex: Integer;
  AInfo: TdxCharacterInfo;
  ARun: TdxEndNoteRun;
begin
  AEndNote := TdxEndNote.Create(DocumentModel);
  AEndNotePieceTable := TdxPieceTable(AEndNote.PieceTable);
  AIndex := DocumentModel.EndNotes.Count;
  DocumentModel.UnsafeEditor.InsertFirstParagraph(AEndNotePieceTable);
  DocumentModel.EndNotes.Add(AEndNote);
  AInfo := ADocObject.PropertyContainer.CharacterInfo;
  SpecifyCharacterFormattingForInputPosition(AInfo, AInputPosition);
  ARun := TdxEndNoteRun(APieceTable.InsertEndNoteRun(AInputPosition, AIndex));
  AddDelayedTextFormatting(AInfo, APieceTable);
  AEndNote.ReferenceRun := ARun;
  InsertEndNoteContent(AEndNote, ADocObject.Position);
end;

procedure TdxDocImporter.InsertEndNoteContent(ANote: TdxEndNote; ACharacterPosition: Integer);
var
  ANoteContent: TdxDocObjectCollection;
begin
  ANoteContent := ContentBuilder.Iterator.NotesIterator.GetEndNoteObjects(ACharacterPosition);
  InsertEmbeddedContent(TdxPieceTable(ANote.PieceTable), ANoteContent);
end;

procedure TdxDocImporter.InsertEmbeddedContent(APieceTable: TdxPieceTable; AContent: TdxDocObjectCollection);
var
  APosition: TdxInputPosition;
  ADocObject: IdxDocObject;
begin
  if AContent.Count = 0 then
    Exit;
  ContentBuilder.BeginEmbeddedContent(AContent);
  UpdateActiveImportInfo(APieceTable);
  APosition := TdxInputPosition.Create(APieceTable);
  try
    for ADocObject in AContent do
      ProcessDocObject(ADocObject, APieceTable, APosition);
    Finish(APieceTable);
    ContentBuilder.EndEmbeddedContent;
  finally
    APosition.Free;
  end;
end;

procedure TdxDocImporter.InsertNoteNumber(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
var
  AInfo: TdxCharacterInfo;
begin
  AInfo := ADocObject.PropertyContainer.CharacterInfo;
  SpecifyCharacterFormattingForInputPosition(AInfo, AInputPosition);
  if APieceTable.ContentType is TdxFootNote then
    TryInsertFootNoteNumber(AInputPosition, APieceTable)
  else
    if APieceTable.ContentType is TdxEndNote then
      TryInsertEndNoteNumber(AInputPosition, APieceTable);
  AddDelayedTextFormatting(AInfo, APieceTable);
end;

procedure TdxDocImporter.TryInsertFootNoteNumber(APosition: TdxInputPosition; APieceTable: TdxPieceTable);
var
  AIndex: Integer;
begin
  AIndex := DocumentModel.FootNotes.Count - 1;
  if AIndex >= 0 then
    APieceTable.InsertFootNoteRun(APosition, AIndex);
end;

procedure TdxDocImporter.TryInsertEndNoteNumber(APosition: TdxInputPosition; APieceTable: TdxPieceTable);
var
  AIndex: Integer;
begin
  AIndex := DocumentModel.EndNotes.Count - 1;
  if AIndex >= 0 then
    APieceTable.InsertEndNoteRun(APosition, AIndex);
end;

procedure TdxDocImporter.InsertAnnotationReference(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
begin
end;

procedure TdxDocImporter.InsertCommentContent(ACommentContent: TdxCommentContentType; ACharacterPosition: Integer);
begin
end;

procedure TdxDocImporter.InsertTableCell(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
begin
  InsertParagraph(ADocObject, APieceTable, AInputPosition);
  ActiveImportInfo.TablesImporter.CellEndReached(ADocObject.PropertyContainer);
end;

procedure TdxDocImporter.InsertTableRow(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
var
  APropertyContainer: TdxDocPropertyContainer;
  ATableStyleIndex, AStyleIndex: Integer;
begin
  APropertyContainer := ADocObject.PropertyContainer;
  APropertyContainer.Update([TdxChangeActionType.Table]);
  ATableStyleIndex := APropertyContainer.TableInfo.TableStyleIndex;
  if ATableStyleIndex >= 0 then
    AStyleIndex := StylesImportHelper.GetStyleIndex(APropertyContainer.TableInfo.TableStyleIndex, TdxStyleType.TableStyle)
  else
    AStyleIndex := -1;
  APropertyContainer.TableInfo.TableStyleIndex := AStyleIndex;
  ActiveImportInfo.TablesImporter.RowEndReached(APropertyContainer);
end;

procedure TdxDocImporter.ProcessParagraphListInfoIndex(APieceTable: TdxPieceTable; AIndex: TdxParagraphIndex; AInfo: TdxParagraphInfo);
var
  AParagraph: TdxParagraph;
  AListInfoIndex, AListLevel: Integer;
  AActualListIndex: TdxNumberingListIndex;
begin
  AParagraph := APieceTable.Paragraphs[AIndex];
  AListInfoIndex := AInfo.ListInfoIndex;
  AListLevel := AInfo.ListLevel;

  if AListLevel = $0c then
    Exit;
  if AListInfoIndex = 0 then
  begin
    AActualListIndex := AParagraph.ParagraphStyle.GetNumberingListIndex;
    if AActualListIndex < NumberingListIndexMinValue then
      Exit;
    if AListLevel >= DocumentModel.NumberingLists[AActualListIndex].Levels.Count then
      AListLevel := 0;
    APieceTable.AddNumberingListToParagraph(AParagraph, NumberingListIndexNoNumberingList, AListLevel);
  end;
  if (AListInfoIndex > 0) and (AListInfoIndex - 1 < DocumentModel.NumberingLists.Count) then
  begin
    APieceTable.AddNumberingListToParagraph(AParagraph, AListInfoIndex - 1, AListLevel);
  end;
end;

procedure TdxDocImporter.SpecifyCharacterFormattingForInputPosition(AInfo: TdxCharacterInfo; APosition: TdxInputPosition);
var
  AIsInverted: Boolean;
  AFormattingInfo: TdxCharacterFormattingInfo;
begin
  if AInfo = nil then
  begin
    APosition.CharacterFormatting.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
    APosition.CharacterStyleIndex := DocumentModel.CharacterStyles.DefaultItemIndex;
    Exit;
  end;
  APosition.CharacterStyleIndex := StylesImportHelper.GetStyleIndex(AInfo.FormattingInfo.StyleIndex, TdxStyleType.CharacterStyle);
  AIsInverted := AInfo.FormattingInfo.ContainsInvertedProperties;

  if not AIsInverted then
  begin
    AFormattingInfo := StylesImportHelper.GetCharacterFormattingInfo(AInfo.FormattingInfo);
    try
      APosition.CharacterFormatting.CopyFrom(AFormattingInfo, AInfo.FormattingOptions);
    finally
      AFormattingInfo.Free;
    end;
  end
  else
    DocumentModel.ResetMerging;
end;

procedure TdxDocImporter.AddDelayedTextFormatting(AInfo: TdxCharacterInfo; APieceTable: TdxPieceTable);
begin
  AddDelayedFormattingCore(AInfo, APieceTable, APieceTable.LastInsertedRunInfo.Run);
end;

procedure TdxDocImporter.AddDelayedImageFormatting(AInfo: TdxCharacterInfo; APieceTable: TdxPieceTable);
begin
  AddDelayedFormattingCore(AInfo, APieceTable, APieceTable.LastInsertedInlinePictureRunInfo.Run);
end;

procedure TdxDocImporter.AddDelayedFormattingCore(AInfo: TdxCharacterInfo; APieceTable: TdxPieceTable; ARun: TdxTextRunBase);
begin
  if (ARun = nil) or (AInfo = nil) or (not AInfo.FormattingInfo.ContainsInvertedProperties) then
    Exit;
  FDelayedFormatting.Add(ARun, AInfo);
  DocumentModel.ResetMerging;
end;

procedure TdxDocImporter.ProcessDelayedFormatting;
var
  ARun: TdxTextRunBase;
  AProperties: TdxMergedCharacterProperties;
  ACharacterInfo: TdxCharacterInfo;
  AInfo: TdxCharacterFormattingInfo;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  for ARun in FDelayedFormatting.Keys do
  begin
    AProperties := ARun.GetParentMergedCharacterProperties;
    try
      ACharacterInfo := FDelayedFormatting[ARun];
      AInfo := TdxDocCharacterFormattingHelper.GetMergedCharacterFormattingInfo(ACharacterInfo.FormattingInfo, AProperties.Info, DocumentModel);
      try
        AMergedCharacterProperties := TdxMergedCharacterProperties.Create(AInfo, ACharacterInfo.FormattingOptions);
        try
          ARun.CharacterProperties.CopyFrom(AMergedCharacterProperties);
        finally
          AMergedCharacterProperties.Free;
        end;
      finally
        AInfo.Free;
      end;
    finally
      AProperties.Free;
    end;
  end;
  FDelayedFormatting.Clear;
end;

procedure TdxDocImporter.InsertParagraph(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
var
  AParagraphIndex: TdxParagraphIndex;
  APropertyContainer: TdxDocPropertyContainer;
begin
  AParagraphIndex := AInputPosition.ParagraphIndex;
  APropertyContainer := ADocObject.PropertyContainer;
  SetCharacterProperties(AInputPosition, APropertyContainer);
  APieceTable.InsertParagraphCore(AInputPosition);
  SetParagraphProperties(APieceTable, AParagraphIndex, APropertyContainer, False);
end;

procedure TdxDocImporter.SetDefaultDocumentProperties;
begin
  DocumentModel.DefaultCharacterProperties.DoubleFontSize := 20;
  DocumentModel.DefaultCharacterProperties.FontName := TdxDocCharacterFormattingInfo.DefaultFontName;

  DocumentModel.DefaultParagraphProperties.LineSpacing := 0.0;
  DocumentModel.DefaultParagraphProperties.LineSpacingType := TdxRichEditParagraphLineSpacing.Single;
  DocumentModel.DefaultParagraphProperties.SpacingBefore := 0;
  DocumentModel.DefaultParagraphProperties.SpacingAfter := 0;
end;

procedure TdxDocImporter.SetCharacterProperties(AInputPosition: TdxInputPosition; APropertyContainer: TdxDocPropertyContainer);
var
  AInfo: TdxDocCharacterFormattingInfo;
  AFormattingOptions: TdxCharacterFormattingOptions;
  AFormattingInfo: TdxCharacterFormattingInfo;
begin
  if APropertyContainer.CharacterInfo = nil then
  begin
    AInputPosition.CharacterFormatting.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
    Exit;
  end;
  AInfo := APropertyContainer.CharacterInfo.FormattingInfo;
  AFormattingOptions := APropertyContainer.CharacterInfo.FormattingOptions;
  AFormattingInfo := StylesImportHelper.GetCharacterFormattingInfo(AInfo);
  try
    AInputPosition.CharacterFormatting.CopyFrom(AFormattingInfo, AFormattingOptions);
  finally
    AFormattingInfo.Free;
  end;
  AInputPosition.CharacterStyleIndex := StylesImportHelper.GetStyleIndex(AInfo.StyleIndex, TdxStyleType.CharacterStyle);
end;

procedure TdxDocImporter.SetParagraphProperties(APieceTable: TdxPieceTable; AParagraphIndex: TdxParagraphIndex; APropertyContainer: TdxDocPropertyContainer; ASectionMark: Boolean);
var
  AParagraph: TdxParagraph;
  AParagraphInfo: TdxParagraphInfo;
  AFrameInfo: TdxFrameInfo;
  AMergedParagraphProperties: TdxMergedParagraphProperties;
  AMergedFrameProperties: TdxMergedFrameProperties;
begin
  AParagraph := APieceTable.Paragraphs[AParagraphIndex];
  AParagraphInfo := APropertyContainer.ParagraphInfo;
  if AParagraphInfo <> nil then
  begin
    AMergedParagraphProperties := TdxMergedParagraphProperties.Create(AParagraphInfo.FormattingInfo, AParagraphInfo.FormattingOptions);
    try
      AParagraph.ParagraphProperties.CopyFrom(AMergedParagraphProperties);
    finally
      AMergedParagraphProperties.Free;
    end;
    AParagraph.SetOwnTabs(AParagraphInfo.Tabs);
    AParagraph.ParagraphStyleIndex := StylesImportHelper.GetStyleIndex(AParagraphInfo.ParagraphStyleIndex, TdxStyleType.ParagraphStyle);
    ProcessParagraphListInfoIndex(APieceTable, AParagraphIndex, AParagraphInfo);
    AFrameInfo := APropertyContainer.FrameInfo;
    if (AFrameInfo <> nil) and DocumentModel.DocumentCapabilities.ParagraphFramesAllowed then
    begin
      if not IsContainsParagraphFrame then
        IsContainsParagraphFrame := True;
      AParagraph.CreateFrameProperties;
      AMergedFrameProperties := TdxMergedFrameProperties.Create(AFrameInfo.FormattingInfo, AFrameInfo.FormattingOptions);
      try
        AParagraph.FrameProperties.CopyFrom(AMergedFrameProperties);
      finally
        AMergedFrameProperties.Free;
      end;
    end;
  end;
  if not ASectionMark then
    ActiveImportInfo.TablesImporter.ParagraphAdded(AParagraph, APropertyContainer);
  if FDelayedFormatting.Count > 0 then
    ProcessDelayedFormatting;
end;

procedure TdxDocImporter.InsertSection(const ADocObject: IdxDocObject; APieceTable: TdxPieceTable; AInputPosition: TdxInputPosition);
var
  AParagraphIndex: TdxParagraphIndex;
  APropContainer: TdxDocPropertyContainer;
  AInfo: TdxDocCharacterFormattingInfo;
  AFormattingInfo: TdxCharacterFormattingInfo;
  AFormattingOptions: TdxCharacterFormattingOptions;
begin
  AParagraphIndex := AInputPosition.ParagraphIndex;
  APropContainer := ADocObject.PropertyContainer;
  if APropContainer.CharacterInfo <> nil then
  begin
    AInfo := APropContainer.CharacterInfo.FormattingInfo;
    AFormattingInfo := StylesImportHelper.GetCharacterFormattingInfo(AInfo);
    try
      AFormattingOptions := APropContainer.CharacterInfo.FormattingOptions;
      AInputPosition.CharacterFormatting.CopyFrom(AFormattingInfo, AFormattingOptions);
    finally
      AFormattingInfo.Free;
    end;
  end;
  ActiveImportInfo.TablesImporter.BeforeSectionAdded;
  DocumentModel.MainPieceTable.InsertSectionParagraphCore(AInputPosition);
  DocumentModel.SafeEditor.PerformInsertSectionCore(AParagraphIndex);
  if APropContainer.ParagraphInfo <> nil then
    SetParagraphProperties(APieceTable, AParagraphIndex, APropContainer, True);
  ApplySectionProperties(APropContainer);
  Inc(FCurrentSectionIndex);
end;

procedure TdxDocImporter.ApplySectionProperties(APropertyContainer: TdxDocPropertyContainer);
var
  ACurrentSection: TdxSection;
begin
  if (APropertyContainer.SectionInfo = nil) or (FCurrentSectionIndex >= DocumentModel.Sections.Count) then
    Exit;
  ACurrentSection := DocumentModel.Sections[FCurrentSectionIndex];
  ACurrentSection.Columns.CopyFrom(APropertyContainer.SectionInfo.SectionColumns);
  ACurrentSection.Margins.CopyFrom(APropertyContainer.SectionInfo.SectionMargins);
  if ContentBuilder.DocumentProperties.GutterPosition = TdxGutterPosition.Top then
    ACurrentSection.Margins.GutterAlignment := TdxSectionGutterAlignment.Top;
  APropertyContainer.SectionInfo.SectionPage.ValidatePaperKind(DocumentModel.UnitConverter);
  ACurrentSection.Page.CopyFrom(APropertyContainer.SectionInfo.SectionPage);
  ACurrentSection.GeneralSettings.CopyFrom(APropertyContainer.SectionInfo.SectionGeneralSettings);
  ACurrentSection.PageNumbering.CopyFrom(APropertyContainer.SectionInfo.SectionPageNumbering);
  ACurrentSection.LineNumbering.CopyFrom(APropertyContainer.SectionInfo.SectionLineNumbering);
  ACurrentSection.FootNote.CopyFrom(APropertyContainer.SectionInfo.FootNote);
  ACurrentSection.EndNote.CopyFrom(APropertyContainer.SectionInfo.EndNote);
end;

procedure TdxDocImporter.UpdateActiveImportInfo(APieceTable: TdxPieceTable);
begin
  ImportInfoStack.Push(TdxDocImportPieceTableInfo.Create(Self, APieceTable));
end;

class procedure TdxDocImporter.ThrowInvalidDocFile;
begin
  raise EdxRichEditArgumentException.Create('Invalid Doc file');
end;

class procedure TdxDocImporter.ThrowInvalidFile;
begin
  raise EdxRichEditArgumentException.Create('Invalid Doc file');
end;

procedure TdxDocImporter.AddCellsHorizontalMerging(ARow: TdxTableRow; AFirstCellIndex: Integer; ASpan: Integer);
var
  AList: TdxDocTableCellHorizontalMergingList;
begin
  if not FCellsHorizontalMerging.TryGetValue(ARow, AList) then
  begin
    AList := TdxDocTableCellHorizontalMergingList.Create;
    FCellsHorizontalMerging.Add(ARow, AList);
  end;
  AList.Add(TdxDocTableCellHorizontalMerging.Create(AFirstCellIndex, ASpan));
end;

end.
