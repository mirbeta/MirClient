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
unit dxRichEdit.Export.Doc.DocExporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxOLEDocument,
  dxRichEdit.Utils.Types,
  dxRichEdit.Platform.Font,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.DocumentModel.Exporter,
  dxRichEdit.Import.Doc.FileInformationBlock,
  dxRichEdit.Export.Core,
  dxRichEdit.Import.Doc.OfficeArtContent,
  dxRichEdit.Import.Doc.DocFieldsImportHelper,
  dxRichEdit.Import.Doc.DocCharacterFormattingInfo,
  dxRichEdit.Import.Doc.DocHeadersFooters,
  dxRichEdit.Export.Doc.DocDataWriter,
  dxRichEdit.DocumentFormats.DocumentFormatUtils,
  dxRichEdit.Import.Doc.DocumentProperties;

type
  { TdxExportFieldsIterator }

  TdxExportFieldsIterator = class
  strict private
    FCurrentFields: TArray<TdxField>;
    FCurrentFieldIndex: Integer;
    FFields: TStack<TdxField>;
    function GetCurrentField: TdxField;
  protected
    property CurrentFields: TArray<TdxField> read FCurrentFields write FCurrentFields;
    property CurrentFieldIndex: Integer read FCurrentFieldIndex write FCurrentFieldIndex;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateFieldsInfo(APieceTable: TdxPieceTable);
    function CreateFieldBeginDescriptor(APieceTable: TdxPieceTable): TdxDocFieldBeginDescriptor;
    function CreateFieldEndDescriptor(AFieldLevel: Integer): TdxDocFieldEndDescriptor;

    property CurrentField: TdxField read GetCurrentField;
  end;

  { TdxExportImagesIterator }

  TdxExportImagesIterator = class
  strict private
    FCurrentPictureIndex: Integer;
    FState: TdxDocContentState;
    function GetHasPictures: Boolean;
  protected
    property CurrentPictureIndex: Integer read FCurrentPictureIndex write FCurrentPictureIndex;
  public
    constructor Create;
    procedure WritePictureDescriptor(AWriter: TBinaryWriter; ARun: TdxInlinePictureRun);
    procedure SetState(AState: TdxDocContentState);
    function CalculateCurrentShapeId: Integer;

    property HasPictures: Boolean read GetHasPictures;
  end;

  { TdxDocExporter }

  TdxDocExporter = class(TdxDocumentModelExporter{, IdxDocExporter})
  public const
    TextStartOffset      = Integer($0800);
    EmptyShapeIdentifier = Integer(0);
  protected type

    TTextStreamBorders = class
    strict private
      FMainDocumentLength: Integer;
      FFootNotesTextLength: Integer;
      FHeadersFootersTextLength: Integer;
      FCommentsTextLength: Integer;
      FEndNotesTextLength: Integer;
      FTextBoxesTextLength: Integer;
      FHeaderTextBoxesTextLength: Integer;
    public
      function ShouldWriteEmptyParagraphAtEnd: Boolean;
      property MainDocumentLength: Integer read FMainDocumentLength write FMainDocumentLength;
      property FootNotesTextLength: Integer read FFootNotesTextLength write FFootNotesTextLength;
      property HeadersFootersTextLength: Integer read FHeadersFootersTextLength write FHeadersFootersTextLength;
      property CommentsTextLength: Integer read FCommentsTextLength write FCommentsTextLength;
      property EndNotesTextLength: Integer read FEndNotesTextLength write FEndNotesTextLength;
      property TextBoxesTextLength: Integer read FTextBoxesTextLength write FTextBoxesTextLength;
      property HeaderTextBoxesTextLength: Integer read FHeaderTextBoxesTextLength write FHeaderTextBoxesTextLength;
    end;

  strict private
    FMainStream: TdxMemoryStream;
    FTableStream: TdxMemoryStream;
    FDataStream: TdxMemoryStream;
    FOutputStream: TStream;
    FMainStreamWriter: TBinaryWriter;
    FTableStreamWriter: TBinaryWriter;
    FDataStreamWriter: TBinaryWriter;
    FDocDataWriter: TdxDocDataWriter;
    FImagesIterator: TdxExportImagesIterator;
    FFieldsIterator: TdxExportFieldsIterator;
    FActivePieceTable: TdxPieceTable;
    FHeadersFootersPositions: TdxDocHeadersFootersPositions;
    FCurrentCharacterPosition: Integer;
    FCurrentDocumentPartStart: Integer;
    FTextStreamBorders: TTextStreamBorders;
    function GetOptions: TdxDocDocumentExporterOptions;
  protected
    function ShouldExportHiddenText: Boolean; override;
    procedure CreateDocumentContent;
    procedure CreateDocumentContentCore;
    procedure FinishDocumentContentCreation;
    procedure CreateDocumentInformation;
    procedure ExportMainText;
    procedure ExportCommentsContent;
    procedure ExportCommentsContentCore;
    procedure ExportFootNotes;
    procedure ExportFootNotesCore;
    procedure ExportHeadersFooters;
    function ShouldExportHeadersFootersInfo: Boolean;
    procedure ExportEndNotes;
    procedure ExportEndNotesCore;
    procedure ExportTextBoxes;
    procedure ExportTextBoxesCore(AState: TdxDocContentState);
    procedure SetTextBoxesLength(AState: TdxDocContentState);
    procedure ExportTextBoxesContent(ATextBoxes: TDictionary<Integer, TdxTextBoxContentType>);
    procedure ExportFootnoteSeparators;
    procedure AddDefaultSeparators;
    procedure ExportDefaultCommentSeparators;
    procedure ExportDefaultFootnoteSeparator;
    procedure ExportDefaultFootnoteContinuation;
    procedure ExportItemWithGuardParagraphMark(const AItem: string);
    procedure ExportSectionHeadersFootersFiltered(ASection: TdxSection);
    procedure ExportSectionHeadersFootersCore(ASection: TdxSection); override;
    procedure DoExportEvenPageHeader(ASection: TdxSection);
    procedure DoExportOddPageHeader(ASection: TdxSection);
    procedure DoExportEvenPageFooter(ASection: TdxSection);
    procedure DoExportOddPageFooter(ASection: TdxSection);
    procedure DoExportFirstPageHeader(ASection: TdxSection);
    procedure DoExportFirstPageFooter(ASection: TdxSection);
    procedure AlignMainStreamOffset;
    function CreateFileInformationBlock: TdxFileInformationBlock;
    procedure ExportStyles(AFib: TdxFileInformationBlock);
    procedure ExportFootNoteTables(AFib: TdxFileInformationBlock);
    procedure ExportCommentsTables(AFib: TdxFileInformationBlock);
    procedure ExportEndNoteTables(AFib: TdxFileInformationBlock);
    procedure ExportSectionProperties(AFib: TdxFileInformationBlock);
    procedure ExportDocumentProperties(AFib: TdxFileInformationBlock);
    procedure SetNotesProperties(ADop: TdxDocDocumentProperties; ASection: TdxSection);
    procedure SetDocumentProtection(ADop: TdxDocDocumentProperties);
    procedure ExportDocumentVariables(AFib: TdxFileInformationBlock);
    procedure ExportFonts(AFib: TdxFileInformationBlock);
    procedure ExportComplexFileInformation(AFib: TdxFileInformationBlock);
    procedure ExportFormattingBinTables(AFib: TdxFileInformationBlock);
    procedure ExportHeadersFootersPositionsTable(AFib: TdxFileInformationBlock);
    procedure ExportListFormatInformation(AFib: TdxFileInformationBlock);
    procedure ExportDocumentFormatRecords(AFib: TdxFileInformationBlock);
    procedure ExportListOverrideFormatInformation(AFib: TdxFileInformationBlock);
    procedure ExportFieldTables(AFib: TdxFileInformationBlock);
    procedure ExportBookmarks(AFib: TdxFileInformationBlock);
    procedure ExportRangeEditPermissions(AFib: TdxFileInformationBlock);
    procedure ExportFloatingObjectsInfo(AFib: TdxFileInformationBlock);
    procedure ExportTextSettings(AFib: TdxFileInformationBlock);
    procedure ExportRmdThreading(AFib: TdxFileInformationBlock);
    procedure SetTextStreamBorders(AFib: TdxFileInformationBlock);
    procedure ExportSection(const ASection: TdxSection); override;
    procedure ExportSectionRun(ARun: TdxSectionRun); override;
    procedure ExportParagraphs(AFrom: TdxParagraphIndex; ATo: TdxParagraphIndex); override;
    function ExportFirstCommentParagraph(AParagraph: TdxParagraph): TdxParagraphIndex;
    function ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex; override;
    procedure BeginParagraphExport(AParagraph: TdxParagraph);
    procedure ExportEmptyParagraph; virtual;
    procedure ExportEmptyParagraphWithLastParagraphProperties;
    procedure ExportGuardParagraphMark;
    procedure ExportEmptyParagraphCore;
    procedure ExportEmptyParagraphRun;
    procedure ExportParagraphRun(ARun: TdxParagraphRun); override;
    procedure ExportTextRun(ARun: TdxTextRun); override;
    procedure ExportInlinePictureRun(ARun: TdxInlinePictureRun); override;
    procedure ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun); override;
    procedure ExportBookmarkStart(ABookmark: TdxBookmark); override;
    procedure ExportBookmarkEnd(ABookmark: TdxBookmark); override;
    procedure ExportRangePermissionStart(ARangePermission: TdxRangePermission); override;
    procedure ExportRangePermissionEnd(ARangePermission: TdxRangePermission); override;
    procedure ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun); override;
    procedure ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun); override;
    procedure ExportFieldResultEndRun(ARun: TdxFieldResultEndRun); override;
    procedure ExportFootNoteRun(ARun: TdxFootNoteRun); override;
    procedure ExportEndNoteRun(ARun: TdxEndNoteRun); override;
    procedure ExportRow(ARow: TdxTableRow; ATableInfo: TdxTableInfo); override;
    function ExportTable(ATableInfo: TdxTableInfo): TdxParagraphIndex; override;
    procedure ExportRowMark(ARow: TdxTableRow);
    procedure ExportTextRunBaseCore(const AText: string); overload;
    procedure ExportTextRunBaseCore(ARun: TdxTextRunBase; const AText: string; ASpecial: Boolean = False); overload;
    procedure UpdateState(AState: TdxDocContentState);
    procedure SetActivePieceTable(APieceTable: TdxPieceTable);
    function GetRelativeCharacterPosition: Integer;

    property MainStreamWriter: TBinaryWriter read FMainStreamWriter;
    property TableStreamWriter: TBinaryWriter read FTableStreamWriter;
    property DataStreamWriter: TBinaryWriter read FDataStreamWriter;
    property ImagesIterator: TdxExportImagesIterator read FImagesIterator;
    property FieldsIterator: TdxExportFieldsIterator read FFieldsIterator;
    property ActivePieceTable: TdxPieceTable read FActivePieceTable write SetActivePieceTable;
    property TextBorders: TTextStreamBorders read FTextStreamBorders;
    property HeadersFootersPositions: TdxDocHeadersFootersPositions read FHeadersFootersPositions;
  public
    destructor Destroy; override;
    procedure Export(AOutputStream: TStream); overload; override;
    procedure Export; overload; override;
    procedure Initialize;
    procedure Write;
    procedure AddStreamDirectoryEntry(ARoot: TdxOLEDocumentDirectoryEntry; const AName: string; ABinaryWriter: TBinaryWriter);

    property CurrentCharacterPosition: Integer read FCurrentCharacterPosition write FCurrentCharacterPosition;
    property CurrentDocumentPartStart: Integer read FCurrentDocumentPartStart write FCurrentDocumentPartStart;
    property DocDataWriter: TdxDocDataWriter read FDocDataWriter;
    property Options: TdxDocDocumentExporterOptions read GetOptions;
  end;

implementation

uses
  Math, Contnrs, dxTypeHelpers,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Token,
  dxRichEdit.Types,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.FieldCalculatorService,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.Import.Doc.DocObjectCollection,
  dxRichEdit.Export.Doc.DocTableActions,
  dxRichEdit.Export.Doc.DocCharacterPropertiesActions,
  dxRichEdit.Export.Doc.DocParagraphPropertiesActions,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocContentBuilder,
  dxRichEdit.Import.Doc.FontFamilyName,
  dxRichEdit.Import.Doc.DocPieceTable,
  dxRichEdit.Import.Doc.ComplexFileInformation,
  dxRichEdit.Import.Doc.DocStringTable,
  dxRichEdit.Import.Doc.DocumentFileRecords,
  dxRichEdit.Import.Doc.DocumentVariables,
  dxRichEdit.Import.Doc.ListFormatOverride,
  dxRichEdit.Import.Doc.DocStyles, dxEncoding,
  dxRichEdit.DocumentModel.DocumentProperties;

{ TdxExportFieldsIterator }

constructor TdxExportFieldsIterator.Create;
begin
  FFields := TStack<TdxField>.Create;
end;

destructor TdxExportFieldsIterator.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TdxExportFieldsIterator.GetCurrentField: TdxField;
begin
  Result := FCurrentFields[FCurrentFieldIndex];
end;

procedure TdxExportFieldsIterator.UpdateFieldsInfo(APieceTable: TdxPieceTable);
var
  AComparer: IComparer<TdxField>;
begin
  CurrentFieldIndex := 0;
  SetLength(FCurrentFields, APieceTable.Fields.Count);
  APieceTable.Fields.CopyTo(FCurrentFields, 0);
  AComparer := TdxFieldStartComparer.Create;
  TArray.Sort<TdxField>(FCurrentFields, AComparer);
end;

function TdxExportFieldsIterator.CreateFieldBeginDescriptor(APieceTable: TdxPieceTable): TdxDocFieldBeginDescriptor;
var
  AField: TdxField;
  AIterator: TdxDocumentFieldIterator;
  AModel: TdxDocumentModel;
  AScanner: TdxFieldScanner;
  AToken: IdxToken;
begin
  if CurrentFieldIndex >= Length(CurrentFields) then
    Exit(TdxDocFieldBeginDescriptor.Empty);
  AField := CurrentFields[CurrentFieldIndex];
  AIterator := TdxDocumentFieldIterator.Create(APieceTable, AField);
  try
    AModel := APieceTable.DocumentModel;
    AScanner := TdxFieldScanner.Create(AIterator, AModel.MaxFieldSwitchLength, AModel.EnableFieldNames, APieceTable.SupportFieldCommonStringFormat);
    try
      AToken := AScanner.Scan;
    finally
      AScanner.Free;
    end;
  finally
    AIterator.Free;
  end;
  Inc(FCurrentFieldIndex);
  FFields.Push(AField);
  Result := TdxDocFieldBeginDescriptor.Create(AToken);
end;

function TdxExportFieldsIterator.CreateFieldEndDescriptor(AFieldLevel: Integer): TdxDocFieldEndDescriptor;
var
  AField: TdxField;
begin
  AField := FFields.Pop;
  Result := TdxDocFieldEndDescriptor.Create;
  if AFieldLevel > 0 then
    Result.Properties := Result.Properties or TdxFieldProperties.Nested;
  if AField.Locked then
    Result.Properties := Result.Properties or TdxFieldProperties.Locked;
end;

{ TdxExportImagesIterator }

constructor TdxExportImagesIterator.Create;
begin
  FCurrentPictureIndex := 1;
end;

function TdxExportImagesIterator.GetHasPictures: Boolean;
begin
  Result := FCurrentPictureIndex > 1;
end;

procedure TdxExportImagesIterator.WritePictureDescriptor(AWriter: TBinaryWriter; ARun: TdxInlinePictureRun);
var
  APictureDescriptor: TdxPictureDescriptor;
begin
  APictureDescriptor := TdxPictureDescriptor.Create(ARun, CalculateCurrentShapeId, CurrentPictureIndex);
  try
    APictureDescriptor.Write(AWriter);
  finally
    APictureDescriptor.Free;
  end;
  Inc(FCurrentPictureIndex);
end;

procedure TdxExportImagesIterator.SetState(AState: TdxDocContentState);
begin
  if FState = AState then
    Exit;
  FState := AState;
  CurrentPictureIndex := 1;
end;

function TdxExportImagesIterator.CalculateCurrentShapeId: Integer;
begin
  if FState = TdxDocContentState.MainDocument then
    Exit(TdxOfficeArtConstants.DefaultMainDocumentShapeIdentifier + CurrentPictureIndex);
  if FState = TdxDocContentState.HeadersFootersStory then
    Exit(TdxOfficeArtConstants.DefaultHeaderShapeIdentifier + CurrentPictureIndex);

  TdxRichEditExceptions.ThrowInternalException;
  Result := 0;
end;

{ TdxDocExporter.TTextStreamBorders }

function TdxDocExporter.TTextStreamBorders.ShouldWriteEmptyParagraphAtEnd: Boolean;
begin
  Result :=
    (FootNotesTextLength <> 0) or
    (HeadersFootersTextLength <> 0) or
    (CommentsTextLength <> 0) or
    (EndNotesTextLength <> 0) or
    (TextBoxesTextLength <> 0) or
    (HeaderTextBoxesTextLength <> 0);
end;

{ TdxDocExporter }

destructor TdxDocExporter.Destroy;
begin
  FMainStreamWriter.Free;
  FTableStreamWriter.Free;
  FDataStreamWriter.Free;
  FImagesIterator.Free;
  FFieldsIterator.Free;
  FTextStreamBorders.Free;
  FMainStream.Free;
  FTableStream.Free;
  FDataStream.Free;
  FHeadersFootersPositions.Free;

  FDocDataWriter.Free;
  inherited Destroy;
end;

function TdxDocExporter.ShouldExportHiddenText: Boolean;
begin
  Result := True;
end;

procedure TdxDocExporter.Export(AOutputStream: TStream);
begin
  FOutputStream := AOutputStream;
  Export;
end;

procedure TdxDocExporter.Export;
begin
  if FOutputStream = nil then
    TdxRichEditExceptions.ThrowInternalException;
  Initialize;
  CreateDocumentContent;
  Write;
end;

procedure TdxDocExporter.Initialize;
begin
  FMainStream := TdxMemoryStream.Create;
//  FMainStream.Size := TdxFileInformationBlock.FIBSize;
//  FillChar(FMainStream.Memory^, TdxFileInformationBlock.FIBSize, 0);
  FMainStream.Seek(TdxFileInformationBlock.FIBSize, TSeekOrigin.soBeginning);
  FTableStream := TdxMemoryStream.Create;
  FDataStream := TdxMemoryStream.Create;


  FMainStreamWriter := TBinaryWriter.Create(FMainStream);
  FTableStreamWriter := TBinaryWriter.Create(FTableStream);
  FDataStreamWriter := TBinaryWriter.Create(FDataStream);


  FDocDataWriter := TdxDocDataWriter.Create(DataStreamWriter, Options, DocumentModel);
  FImagesIterator := TdxExportImagesIterator.Create;
  FFieldsIterator := TdxExportFieldsIterator.Create;
  FTextStreamBorders := TTextStreamBorders.Create;
end;

procedure TdxDocExporter.Write;
var
  AOleDocument: TdxOLEDocument;
  ARoot: TdxOLEDocumentDirectoryEntry;
begin
  AOleDocument := TdxOLEDocument.Create(FOutputStream, TdxOLEDocumentMode.dmWriting);
  ARoot := AOleDocument.CreateDirEntry(TdxOLEDocument.RootDirName, ET_ROOT);
  try
    AddStreamDirectoryEntry(ARoot, TdxDocumentStreams.Stream1TableName, TableStreamWriter);
    AddStreamDirectoryEntry(ARoot, TdxDocumentStreams.MainStreamName, MainStreamWriter);


    if DataStreamWriter.BaseStream.Size > 0 then
      AddStreamDirectoryEntry(ARoot, TdxDocumentStreams.DataStreamName, DataStreamWriter);
    AOleDocument.Commit;
  finally
    AOleDocument.Free;
  end;
end;

procedure TdxDocExporter.AddStreamDirectoryEntry(ARoot: TdxOLEDocumentDirectoryEntry; const AName: string; ABinaryWriter: TBinaryWriter);
begin
  ARoot.Document.CreateStream(ARoot, AName).CopyFrom(ABinaryWriter.BaseStream, 0);
end;

procedure TdxDocExporter.CreateDocumentContent;
begin
  DocDataWriter.StylesExporter.CreateStyleSheet;
  DocDataWriter.ListsExporter.CreateLists(DocDataWriter.StylesExporter);
  CreateDocumentContentCore;
  FinishDocumentContentCreation;
end;

procedure TdxDocExporter.CreateDocumentContentCore;
begin
  ExportMainText;
  ExportFootNotes;
  ExportHeadersFooters;
  ExportCommentsContent;
  ExportEndNotes;
  ExportTextBoxes;
end;

procedure TdxDocExporter.FinishDocumentContentCreation;
var
  AFib: TdxFileInformationBlock;
begin
  if TextBorders.ShouldWriteEmptyParagraphAtEnd then
    ExportEmptyParagraphWithLastParagraphProperties;
  DocDataWriter.Finish(CurrentCharacterPosition);
  AlignMainStreamOffset;
  MainStreamWriter.Write(DocDataWriter.GetFormattedDiskPages);
  AlignMainStreamOffset;
  DocDataWriter.UpdateSectionsOffsets(MainStreamWriter.BaseStream.Position);
  MainStreamWriter.Write(DocDataWriter.GetSectionProperties);
  AFib := CreateFileInformationBlock;
  try
    AFib.Write(MainStreamWriter);
  finally
    AFib.Free;
  end;
  CreateDocumentInformation;
end;

procedure TdxDocExporter.CreateDocumentInformation;
begin
end;

procedure TdxDocExporter.ExportMainText;
begin
  UpdateState(TdxDocContentState.MainDocument);
  ActivePieceTable := DocumentModel.MainPieceTable;
  inherited Export;
  TextBorders.MainDocumentLength := CurrentCharacterPosition;
end;

procedure TdxDocExporter.ExportCommentsContent;
begin
end;

procedure TdxDocExporter.ExportCommentsContentCore;
begin
end;

procedure TdxDocExporter.ExportFootNotes;
begin
  if (not DocumentModel.DocumentCapabilities.FootNotesAllowed) or (DocumentModel.FootNotes.Count = 0) then
    Exit;
  UpdateState(TdxDocContentState.Footnotes);
  ExportEmptyParagraphCore;
  ExportFootNotesCore;
  ExportEmptyParagraph;
  TextBorders.FootNotesTextLength := GetRelativeCharacterPosition;
  DocDataWriter.NotesExporter.FinishFootNotePositions(TextBorders.FootNotesTextLength - 1, CurrentCharacterPosition);
end;

procedure TdxDocExporter.ExportFootNotesCore;
var
  ACount, I: Integer;
  AFootNote: TdxFootNote;
begin
  ACount := DocumentModel.FootNotes.Count;
  for I := 0 to ACount - 1 do
  begin
    AFootNote := DocumentModel.FootNotes[I];
    if not AFootNote.IsReferenced then
      Continue;

    DocDataWriter.NotesExporter.FootNotePositions.Add(GetRelativeCharacterPosition);
    ActivePieceTable := TdxPieceTable(AFootNote.PieceTable);
    PerformExportPieceTable(ActivePieceTable, ExportPieceTable);
  end;
end;

procedure TdxDocExporter.ExportHeadersFooters;
var
  I: Integer;
begin
  if not ShouldExportHeadersFootersInfo then
    Exit;
  FHeadersFootersPositions := TdxDocHeadersFootersPositions.Create;
  UpdateState(TdxDocContentState.HeadersFootersStory);
  ExportFootnoteSeparators;
  for I := 0 to DocumentModel.Sections.Count - 1 do
    ExportSectionHeadersFootersFiltered(DocumentModel.Sections[I]);
  ExportGuardParagraphMark;
  TextBorders.HeadersFootersTextLength := GetRelativeCharacterPosition;
  HeadersFootersPositions.CharacterPositions.Add(TextBorders.HeadersFootersTextLength + 2);
end;

function TdxDocExporter.ShouldExportHeadersFootersInfo: Boolean;
begin
  if (DocumentModel.Headers.Count > 0) or (DocumentModel.Footers.Count > 0) then
    Exit(True);
  if (DocumentModel.FootNotes.Count > 0) or (DocumentModel.EndNotes.Count > 0) then
    Exit(True);
  Result := False;
end;

procedure TdxDocExporter.ExportEndNotes;
begin
  if (not DocumentModel.DocumentCapabilities.EndNotesAllowed) or (DocumentModel.EndNotes.Count = 0) then
    Exit;
  UpdateState(TdxDocContentState.Endnotes);
  ExportEmptyParagraphCore;
  ExportEndNotesCore;
  ExportEmptyParagraph;
  TextBorders.EndNotesTextLength := GetRelativeCharacterPosition;
  DocDataWriter.NotesExporter.FinishEndNotePositions(TextBorders.EndNotesTextLength);
end;

procedure TdxDocExporter.ExportEndNotesCore;
var
  ACount, I: Integer;
  AEndNote: TdxEndNote;
begin
  ACount := DocumentModel.EndNotes.Count;
  for I := 0 to ACount - 1 do
  begin
    AEndNote := DocumentModel.EndNotes[I];
    if not AEndNote.IsReferenced then
      Continue;

    DocDataWriter.NotesExporter.EndNotePositions.Add(GetRelativeCharacterPosition);
    ActivePieceTable := TdxPieceTable(AEndNote.PieceTable);
    PerformExportPieceTable(ActivePieceTable, ExportPieceTable);
  end;
end;

procedure TdxDocExporter.ExportTextBoxes;
begin
  ExportTextBoxesCore(TdxDocContentState.TextBoxes);
  ExportTextBoxesCore(TdxDocContentState.HeaderTextBoxes);
end;

procedure TdxDocExporter.ExportTextBoxesCore(AState: TdxDocContentState);
var
  AExporter: TdxTextBoxesExporter;
  ATextBoxes: TDictionary<Integer, TdxTextBoxContentType>;
begin
  UpdateState(AState);
  AExporter := DocDataWriter.FloatingObjectsExporter.TextBoxesExporter;
  ATextBoxes := AExporter.GetCurrentTextBoxes;
  if ATextBoxes <> nil then
  begin
    ExportTextBoxesContent(ATextBoxes);
    if ATextBoxes.Count > 0 then
      AExporter.FinishCurrentState(GetRelativeCharacterPosition);
  end;
  if AExporter.ShouldInsertEmptyParagraph then
    ExportEmptyParagraph;
  SetTextBoxesLength(AState);
end;

procedure TdxDocExporter.SetTextBoxesLength(AState: TdxDocContentState);
begin
  if AState = TdxDocContentState.TextBoxes then
    TextBorders.TextBoxesTextLength := GetRelativeCharacterPosition;
  if AState = TdxDocContentState.HeaderTextBoxes then
    TextBorders.HeaderTextBoxesTextLength := GetRelativeCharacterPosition;
end;

procedure TdxDocExporter.ExportTextBoxesContent(ATextBoxes: TDictionary<Integer, TdxTextBoxContentType>);
var
//  APair: TPair<Integer, TdxTextBoxContentType>;
  ATextBoxExporter: TdxTextBoxesExporter;
  AKeys: TArray<Integer>;
  AKey: Integer;
begin
  ATextBoxExporter := DocDataWriter.FloatingObjectsExporter.TextBoxesExporter;
  AKeys := ATextBoxes.Keys.ToArray;
  TArray.Sort<Integer>(AKeys, TComparer<Integer>.Default);
  for AKey in AKeys do
  begin
    ATextBoxExporter.AddTextBoxTableEntry(GetRelativeCharacterPosition, AKey, False);
    ActivePieceTable := TdxPieceTable(ATextBoxes[AKey].PieceTable);
    PerformExportPieceTable(ActivePieceTable, ExportPieceTable);
    ExportEmptyParagraph;
  end;
end;

procedure TdxDocExporter.ExportFootnoteSeparators;
begin
  HeadersFootersPositions.CharacterPositions.Add(GetRelativeCharacterPosition);
  AddDefaultSeparators;
  AddDefaultSeparators;
end;

procedure TdxDocExporter.AddDefaultSeparators;
begin
  ExportDefaultFootnoteSeparator;
  HeadersFootersPositions.CharacterPositions.Add(GetRelativeCharacterPosition);
  ExportDefaultFootnoteContinuation;
  HeadersFootersPositions.CharacterPositions.Add(GetRelativeCharacterPosition);
  HeadersFootersPositions.CharacterPositions.Add(GetRelativeCharacterPosition);
end;

procedure TdxDocExporter.ExportDefaultCommentSeparators;
var
  ACommentSeparator: string;
begin
  ACommentSeparator := TdxTextCodes.AnnotationReference;
  ExportTextRunBaseCore(nil, ACommentSeparator, True);
end;

procedure TdxDocExporter.ExportDefaultFootnoteSeparator;
begin
  ExportItemWithGuardParagraphMark(TdxTextCodes.FootNoteSeparatorCharacter);
end;

procedure TdxDocExporter.ExportDefaultFootnoteContinuation;
begin
  ExportItemWithGuardParagraphMark(TdxTextCodes.FootNoteContinuationCharacter);
end;

procedure TdxDocExporter.ExportItemWithGuardParagraphMark(const AItem: string);
begin
  ExportEmptyParagraphCore;
  ExportTextRunBaseCore(nil, AItem, True);
  ExportEmptyParagraphRun;
  ExportGuardParagraphMark;
end;

procedure TdxDocExporter.ExportSectionHeadersFootersFiltered(ASection: TdxSection);
begin
  if ShouldExportSection(ASection) then
    ExportSectionHeadersFooters(ASection);
end;

procedure TdxDocExporter.ExportSectionHeadersFootersCore(ASection: TdxSection);
begin
  if not DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
    Exit;
  DoExportEvenPageHeader(ASection);
  DoExportOddPageHeader(ASection);
  DoExportEvenPageFooter(ASection);
  DoExportOddPageFooter(ASection);
  DoExportFirstPageHeader(ASection);
  DoExportFirstPageFooter(ASection);
end;

procedure TdxDocExporter.DoExportEvenPageHeader(ASection: TdxSection);
begin
  if ASection.InnerEvenPageHeader <> nil then
  begin
    ActivePieceTable := TdxPieceTable(ASection.InnerEvenPageHeader.PieceTable);
    ExportEvenPageHeader(ASection.InnerEvenPageHeader, ASection.Headers.IsLinkedToPrevious(TdxHeaderFooterType.Even));
    ExportEmptyParagraph;
  end;
  HeadersFootersPositions.CharacterPositions.Add(GetRelativeCharacterPosition);
end;

procedure TdxDocExporter.DoExportOddPageHeader(ASection: TdxSection);
begin
  if ASection.InnerOddPageHeader <> nil then
  begin
    ActivePieceTable := TdxPieceTable(ASection.InnerOddPageHeader.PieceTable);
    ExportOddPageHeader(ASection.InnerOddPageHeader, ASection.Headers.IsLinkedToPrevious(TdxHeaderFooterType.Odd));
    ExportEmptyParagraph;
  end;
  HeadersFootersPositions.CharacterPositions.Add(GetRelativeCharacterPosition);
end;

procedure TdxDocExporter.DoExportEvenPageFooter(ASection: TdxSection);
begin
  if ASection.InnerEvenPageFooter <> nil then
  begin
    ActivePieceTable := TdxPieceTable(ASection.InnerEvenPageFooter.PieceTable);
    ExportEvenPageFooter(ASection.InnerEvenPageFooter, ASection.Headers.IsLinkedToPrevious(TdxHeaderFooterType.Even));
    ExportEmptyParagraph;
  end;
  HeadersFootersPositions.CharacterPositions.Add(GetRelativeCharacterPosition);
end;

procedure TdxDocExporter.DoExportOddPageFooter(ASection: TdxSection);
begin
  if ASection.InnerOddPageFooter <> nil then
  begin
    ActivePieceTable := TdxPieceTable(ASection.InnerOddPageFooter.PieceTable);
    ExportOddPageFooter(ASection.InnerOddPageFooter, ASection.Headers.IsLinkedToPrevious(TdxHeaderFooterType.Odd));
    ExportEmptyParagraph;
  end;
  HeadersFootersPositions.CharacterPositions.Add(GetRelativeCharacterPosition);
end;

procedure TdxDocExporter.DoExportFirstPageHeader(ASection: TdxSection);
begin
  if ASection.InnerFirstPageHeader <> nil then
  begin
    ActivePieceTable := TdxPieceTable(ASection.InnerFirstPageHeader.PieceTable);
    ExportFirstPageHeader(ASection.InnerFirstPageHeader, ASection.Headers.IsLinkedToPrevious(TdxHeaderFooterType.First));
    ExportEmptyParagraph;
  end;
  HeadersFootersPositions.CharacterPositions.Add(GetRelativeCharacterPosition);
end;

procedure TdxDocExporter.DoExportFirstPageFooter(ASection: TdxSection);
begin
  if ASection.InnerFirstPageFooter <> nil then
  begin
    ActivePieceTable := TdxPieceTable(ASection.InnerFirstPageFooter.PieceTable);
    ExportFirstPageFooter(ASection.InnerFirstPageFooter, ASection.Headers.IsLinkedToPrevious(TdxHeaderFooterType.First));
    ExportEmptyParagraph;
  end;
  HeadersFootersPositions.CharacterPositions.Add(GetRelativeCharacterPosition);
end;

procedure TdxDocExporter.AlignMainStreamOffset;
var
  AOffset: Int64;
begin
  if MainStreamWriter.BaseStream.Position mod TdxDocContentBuilder.SectorSize <> 0 then
  begin
    AOffset := (MainStreamWriter.BaseStream.Position div TdxDocContentBuilder.SectorSize + 1) * TdxDocContentBuilder.SectorSize;
    MainStreamWriter.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  end;
end;

function TdxDocExporter.CreateFileInformationBlock: TdxFileInformationBlock;
begin
  Result := TdxFileInformationBlock.Create;

  Result.Flags := [TdxFileInformationFlag.TableStreamType, TdxFileInformationFlag.ExtendedCharset, TdxFileInformationFlag.QuickSaves];
  if ImagesIterator.HasPictures then
    Result.Flags := Result.Flags + [TdxFileInformationFlag.HasPictures];

  ExportStyles(Result);
  ExportFootNoteTables(Result);
  ExportCommentsTables(Result);
  ExportSectionProperties(Result);
  ExportEndNoteTables(Result);
  ExportDocumentProperties(Result);
  ExportDocumentVariables(Result);
  ExportFonts(Result);
  ExportComplexFileInformation(Result);
  ExportFormattingBinTables(Result);
  ExportHeadersFootersPositionsTable(Result);
  ExportListFormatInformation(Result);
  ExportListOverrideFormatInformation(Result);
  ExportDocumentFormatRecords(Result);
  ExportFieldTables(Result);
  ExportBookmarks(Result);
  ExportRangeEditPermissions(Result);
  ExportFloatingObjectsInfo(Result);
  ExportTextSettings(Result);
  ExportRmdThreading(Result);
  SetTextStreamBorders(Result);
end;

procedure TdxDocExporter.ExportStyles(AFib: TdxFileInformationBlock);
begin
  AFib.StyleSheetOffset := TableStreamWriter.BaseStream.Position;
  DocDataWriter.StylesExporter.WriteStyleSheet(TableStreamWriter);
  AFib.StyleSheetSize := TableStreamWriter.BaseStream.Position - AFib.StyleSheetOffset;
end;

procedure TdxDocExporter.ExportFootNoteTables(AFib: TdxFileInformationBlock);
begin
  DocDataWriter.NotesExporter.ExportFootNoteTables(AFib, TableStreamWriter);
end;

procedure TdxDocExporter.ExportCommentsTables(AFib: TdxFileInformationBlock);
begin
end;

procedure TdxDocExporter.ExportEndNoteTables(AFib: TdxFileInformationBlock);
begin
  DocDataWriter.NotesExporter.ExportEndNoteTables(AFib, TableStreamWriter);
end;

procedure TdxDocExporter.ExportSectionProperties(AFib: TdxFileInformationBlock);
begin
  AFib.SectionTableOffset := TableStreamWriter.BaseStream.Position;
  DocDataWriter.WriteSectionPositions(TableStreamWriter);
  AFib.SectionTableSize := TableStreamWriter.BaseStream.Position - AFib.SectionTableOffset;
  AFib.ParagraphHeightsOffset := TableStreamWriter.BaseStream.Position;
  AFib.ParagraphHeightsSize := AFib.ParagraphHeightsOffset - TableStreamWriter.BaseStream.Position;
end;

procedure TdxDocExporter.ExportDocumentProperties(AFib: TdxFileInformationBlock);
var
  ADop: TdxDocDocumentProperties;
  ASection: TdxSection;
  ADocumentProperties: TdxDocumentProperties;
begin
  ADop := TdxDocDocumentProperties.CreateDefault;
  try
    ASection := DocumentModel.Sections[0];
    if ASection.Margins.GutterAlignment = TdxSectionGutterAlignment.Top then
      ADop.GutterPosition := TdxGutterPosition.Top
    else
      ADop.GutterPosition := TdxGutterPosition.Side;
    ADocumentProperties := DocumentModel.DocumentProperties;
    ADop.DifferentOddAndEvenPages := ADocumentProperties.DifferentOddAndEvenPages;
    ADop.DisplayBackgroundShape := ADocumentProperties.DisplayBackgroundShape;
    ADop.DefaultTabWidth := ADocumentProperties.DefaultTabWidth;
    SetNotesProperties(ADop, ASection);
    SetDocumentProtection(ADop);
    AFib.DocumentPropertiesOffset := TableStreamWriter.BaseStream.Position;
    ADop.Write(TableStreamWriter);
    AFib.DocumentPropertiesSize := TableStreamWriter.BaseStream.Position - AFib.DocumentPropertiesOffset;
  finally
    ADop.Free;
  end;
end;





procedure TdxDocExporter.SetNotesProperties(ADop: TdxDocDocumentProperties; ASection: TdxSection);
begin
  ADop.FootNoteInitialNumber := ASection.FootNote.StartingNumber;
  ADop.FootNoteNumberingRestartType := ASection.FootNote.NumberingRestartType;
  ADop.FootNotePosition := ASection.FootNote.Position;
  ADop.EndnoteInitialNumber := ASection.EndNote.StartingNumber;
  ADop.EndNoteNumberingRestartType := ASection.EndNote.NumberingRestartType;
  ADop.EndNotePosition := ASection.EndNote.Position;
end;

procedure TdxDocExporter.SetDocumentProtection(ADop: TdxDocDocumentProperties);
var
  AProtectionProperties: TdxDocumentProtectionProperties;
  APasswordHash: TBytes;
begin
  AProtectionProperties := DocumentModel.ProtectionProperties;
  ADop.EnforceProtection := AProtectionProperties.EnforceProtection;
  ADop.ProtectionType := AProtectionProperties.ProtectionType;
  if AProtectionProperties.Word2003PasswordHash <> nil then
  begin
    APasswordHash := AProtectionProperties.Word2003PasswordHash;
    TArray.Reverse<Byte>(APasswordHash);
    ADop.PasswordHash := APasswordHash;
  end;
end;

procedure TdxDocExporter.ExportDocumentVariables(AFib: TdxFileInformationBlock);
var
  ADocVars: TdxDocumentVariables;
begin
  AFib.DocumentVariablesOffset := TableStreamWriter.BaseStream.Position;
  ADocVars := TdxDocumentVariables.FromVariablesCollection(DocumentModel.Variables, DocumentModel.DocumentProperties);
  try
    ADocVars.Write(TableStreamWriter);
  finally
    ADocVars.Free;
  end;
  AFib.DocumentVariablesSize := TableStreamWriter.BaseStream.Position - AFib.DocumentVariablesOffset;
end;

procedure TdxDocExporter.ExportFonts(AFib: TdxFileInformationBlock);
var
  AFontsCount: SmallInt;
  AFontNameIndex: Integer;
  AFont: TdxDocFontFamilyName;
  AFontName: string;
begin
  AFib.FontTableOffset := TableStreamWriter.BaseStream.Position;
  AFontsCount := SmallInt(DocDataWriter.StylesExporter.FontsCount);
  TableStreamWriter.Write(AFontsCount);
  TableStreamWriter.Write(SmallInt(0));
  for AFontNameIndex := 0 to AFontsCount - 1 do
  begin
    AFont := TdxDocFontFamilyName.Create;
    try
      AFontName := DocDataWriter.StylesExporter.GetFontNameByIndex(AFontNameIndex);
      AFont.Charset := DocumentModel.FontCache.GetCharsetByFontName(AFontName);
      AFont.FontName := AFontName;
      AFont.Write(TableStreamWriter);
    finally
      AFont.Free;
    end;
  end;
  AFib.FontTableSize := TableStreamWriter.BaseStream.Position - AFib.FontTableOffset;
end;

procedure TdxDocExporter.ExportComplexFileInformation(AFib: TdxFileInformationBlock);
var
  APieceTable: TdxDocPieceTable;
  AClxInfo: TdxComplexFileInformation;
begin
  APieceTable := TdxDocPieceTable.CreateDefault(TextStartOffset, CurrentCharacterPosition);
  try
    AClxInfo := TdxComplexFileInformation.Create;
    try
      AClxInfo.PieceTable := APieceTable.ToByteArray;
      AFib.ComplexFileInformationOffset := TableStreamWriter.BaseStream.Position;
      AClxInfo.Write(TableStreamWriter);
      AFib.ComplexFileInformationSize := TableStreamWriter.BaseStream.Position - AFib.ComplexFileInformationOffset;
    finally
      AClxInfo.Free;
    end;
  finally
    APieceTable.Free;
  end;
end;

procedure TdxDocExporter.ExportFormattingBinTables(AFib: TdxFileInformationBlock);
begin
  AFib.CharacterTableOffset := TableStreamWriter.BaseStream.Position;
  DocDataWriter.WriteCharactersBinTable(TableStreamWriter);
  AFib.CharacterTableSize := TableStreamWriter.BaseStream.Position - AFib.CharacterTableOffset;
  AFib.ParagraphTableOffset := TableStreamWriter.BaseStream.Position;
  DocDataWriter.WriteParagraphsBinTable(TableStreamWriter);
  AFib.ParagraphTableSize := TableStreamWriter.BaseStream.Position - AFib.ParagraphTableOffset;
end;

procedure TdxDocExporter.ExportHeadersFootersPositionsTable(AFib: TdxFileInformationBlock);
begin
  if HeadersFootersPositions = nil then
    Exit;
  AFib.HeadersFootersPositionsOffset := TableStreamWriter.BaseStream.Position;
  HeadersFootersPositions.Write(TableStreamWriter);
  AFib.HeadersFootersPositionsSize := TableStreamWriter.BaseStream.Position - AFib.HeadersFootersPositionsOffset;
end;

procedure TdxDocExporter.ExportListFormatInformation(AFib: TdxFileInformationBlock);
begin
  AFib.ListFormatInformationOffset := TableStreamWriter.BaseStream.Position;
  DocDataWriter.ListsExporter.WriteListFormatInformation(TableStreamWriter);
  AFib.ListFormatInformationSize := TableStreamWriter.BaseStream.Position - AFib.ListFormatInformationOffset;
end;

procedure TdxDocExporter.ExportDocumentFormatRecords(AFib: TdxFileInformationBlock);
var
  ADocFileRecords: TdxDocumentFileRecords;
begin
  if DocDataWriter.ListsExporter.ListStyles.Count = 0 then
    Exit;
  AFib.DocumentFileRecordsOffset := TableStreamWriter.BaseStream.Position;
  ADocFileRecords := TdxDocumentFileRecords.Create;
  try
    ADocFileRecords.ListStyles.AddRange(DocDataWriter.ListsExporter.ListStyles);
    ADocFileRecords.Write(TableStreamWriter);
  finally
    ADocFileRecords.Free;
  end;
  AFib.DocumentFileRecordsSize := TableStreamWriter.BaseStream.Position - AFib.DocumentFileRecordsOffset;
end;

procedure TdxDocExporter.ExportListOverrideFormatInformation(AFib: TdxFileInformationBlock);
begin
  AFib.ListFormatOverrideInformationOffset := TableStreamWriter.BaseStream.Position;
  DocDataWriter.ListsExporter.WriteListOverrideFormatInformation(TableStreamWriter);
  AFib.ListFormatOverrideInformationSize := TableStreamWriter.BaseStream.Position - AFib.ListFormatOverrideInformationOffset;
end;

procedure TdxDocExporter.ExportFieldTables(AFib: TdxFileInformationBlock);
begin
  DocDataWriter.ExportFieldTables(AFib, TableStreamWriter);
end;

procedure TdxDocExporter.ExportBookmarks(AFib: TdxFileInformationBlock);
begin
  DocDataWriter.BookmarkIterator.Write(AFib, TableStreamWriter);
end;

procedure TdxDocExporter.ExportRangeEditPermissions(AFib: TdxFileInformationBlock);
begin
  DocDataWriter.PermissionIterator.Write(AFib, TableStreamWriter);
end;

procedure TdxDocExporter.ExportFloatingObjectsInfo(AFib: TdxFileInformationBlock);
begin
  DocDataWriter.FloatingObjectsExporter.ExportFloatingObjectsInfo(AFib, TableStreamWriter, MainStreamWriter);
end;

procedure TdxDocExporter.ExportTextSettings(AFib: TdxFileInformationBlock);
begin
  AFib.FirstCharacterFileOffset := TdxFileInformationBlock.FIBSize;
  AFib.LastCharacterFileOffset := TdxFileInformationBlock.FIBSize + CurrentCharacterPosition * 2;
  AFib.LastByteFileOffset := MainStreamWriter.BaseStream.Size;
end;

procedure TdxDocExporter.ExportRmdThreading(AFib: TdxFileInformationBlock);
var
  ARmdThreading: TdxRmdThreading;
begin
  AFib.RmdThreadingOffset := TableStreamWriter.BaseStream.Position;
  ARmdThreading := TdxRmdThreading.Create;
  try
    ARmdThreading.Write(TableStreamWriter);
  finally
    ARmdThreading.Free;
  end;
  AFib.RmdThreadingSize := TableStreamWriter.BaseStream.Position - AFib.RmdThreadingOffset;
end;

procedure TdxDocExporter.SetTextStreamBorders(AFib: TdxFileInformationBlock);
begin
  AFib.MainDocumentLength := TextBorders.MainDocumentLength;
  AFib.FootNotesLength := TextBorders.FootNotesTextLength;
  AFib.HeadersFootersLength := TextBorders.HeadersFootersTextLength;
  AFib.CommentsLength := TextBorders.CommentsTextLength;
  AFib.EndNotesLength := TextBorders.EndNotesTextLength;
  AFib.MainDocumentTextBoxesLength := TextBorders.TextBoxesTextLength;
  AFib.HeaderTextBoxesLength := TextBorders.HeaderTextBoxesTextLength;
end;

procedure TdxDocExporter.ExportSection(const ASection: TdxSection);
begin
  DocDataWriter.WriteSection(CurrentCharacterPosition, ASection);
  ExportParagraphs(ASection.FirstParagraphIndex, ASection.LastParagraphIndex);
end;

procedure TdxDocExporter.ExportSectionRun(ARun: TdxSectionRun);
begin
  ExportTextRunBaseCore(ARun, TdxTextCodes.SectionMark);
end;

procedure TdxDocExporter.ExportParagraphs(AFrom: TdxParagraphIndex; ATo: TdxParagraphIndex);
begin
  if ActivePieceTable.IsComment then
  begin
    AFrom := ExportFirstCommentParagraph(PieceTable.Paragraphs[AFrom]);
    Inc(AFrom);
  end;
  inherited ExportParagraphs(AFrom, ATo);
end;

function TdxDocExporter.ExportFirstCommentParagraph(AParagraph: TdxParagraph): TdxParagraphIndex;
begin
  BeginParagraphExport(AParagraph);
  ExportDefaultCommentSeparators;
  Result := inherited ExportParagraph(AParagraph);
end;

function TdxDocExporter.ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex;
begin
  BeginParagraphExport(AParagraph);
  Result := inherited ExportParagraph(AParagraph);
end;

procedure TdxDocExporter.BeginParagraphExport(AParagraph: TdxParagraph);
var
  AParagraphStyleIndex: Integer;
  AListIndex: TdxNumberingListIndex;
  ALevelInfo: TdxDocListOverrideLevelInformation;
begin
  AParagraphStyleIndex := DocDataWriter.StylesExporter.GetParagraphStyleIndex(AParagraph.ParagraphStyleIndex);
  if AParagraph.GetCell <> nil then
    DocDataWriter.WriteInTableParagraph(CurrentCharacterPosition, AParagraphStyleIndex, AParagraph)
  else
    DocDataWriter.WriteParagraph(CurrentCharacterPosition, AParagraphStyleIndex, AParagraph);
  AListIndex := AParagraph.NumberingListIndex;
  if (AListIndex = NumberingListIndexNoNumberingList) or (AListIndex = NumberingListIndexListIndexNotSetted) then
    Exit;
  ALevelInfo := DocDataWriter.ListsExporter.ListOverrideInfo.FormatOverrideData[AListIndex];
  if ALevelInfo.CharacterPosition <> TdxDocListsExporter.EmptyCharacterPosition then
    Exit;
  ALevelInfo.CharacterPosition := CurrentCharacterPosition;
end;

procedure TdxDocExporter.ExportEmptyParagraph;
begin
  ExportEmptyParagraphCore;
  ExportEmptyParagraphRun;
end;

procedure TdxDocExporter.ExportEmptyParagraphWithLastParagraphProperties;
var
  AParagraph: TdxParagraph;
  AParagraphStyleIndex: Integer;
begin
  AParagraph := DocumentModel.MainPieceTable.Paragraphs.Last;
  AParagraphStyleIndex := DocDataWriter.StylesExporter.GetParagraphStyleIndex(AParagraph.ParagraphStyleIndex);
  DocDataWriter.WriteParagraph(CurrentCharacterPosition, AParagraphStyleIndex, AParagraph);
  ExportTextRunBaseCore(DocumentModel.MainPieceTable.Runs.Last, TdxTextCodes.ParagraphMark);
end;

procedure TdxDocExporter.ExportGuardParagraphMark;
begin
  ExportEmptyParagraphCore;
  MainStreamWriter.Write(TdxEncoding.Unicode.GetBytes(TdxTextCodes.ParagraphMark));
  Inc(FCurrentCharacterPosition);
end;

procedure TdxDocExporter.ExportEmptyParagraphCore;
var
  AParagraphStyleIndex: Integer;
begin
  AParagraphStyleIndex := DocDataWriter.StylesExporter.GetParagraphStyleIndex(DocumentModel.ParagraphStyles.DefaultItemIndex);
  DocDataWriter.WriteParagraph(CurrentCharacterPosition, AParagraphStyleIndex);
end;

procedure TdxDocExporter.ExportEmptyParagraphRun;
begin
  ExportTextRunBaseCore(TdxTextCodes.ParagraphMark);
end;

procedure TdxDocExporter.ExportParagraphRun(ARun: TdxParagraphRun);
var
  AParagraph: TdxParagraph;
  ACell: TdxTableCell;
  AIsLastParagraphInTableCell: Boolean;
  AParagraphTerminator: string;
begin
  AParagraph := TdxParagraph(ARun.Paragraph);
  ACell := AParagraph.GetCell;
  AIsLastParagraphInTableCell := (ACell <> nil) and (ACell.EndParagraphIndex = AParagraph.Index);
  if (AIsLastParagraphInTableCell) then
    AParagraphTerminator := DocDataWriter.TablesExporter.GetTableUnitMark
  else
    AParagraphTerminator := TdxTextCodes.ParagraphMark;
  ExportTextRunBaseCore(ARun, AParagraphTerminator);
end;

procedure TdxDocExporter.ExportTextRun(ARun: TdxTextRun);
var
  AText: string;
begin
  AText := ARun.GetPlainText(ActivePieceTable.TextBuffer);
  ExportTextRunBaseCore(ARun, AText);
end;

procedure TdxDocExporter.ExportInlinePictureRun(ARun: TdxInlinePictureRun);
var
  AInlinePicture: string;
  ACharacterStyleIndex: Integer;
begin
  AInlinePicture := TdxTextCodes.InlinePicture;
  MainStreamWriter.Write(TdxEncoding.Unicode.GetBytes(AInlinePicture));
  ACharacterStyleIndex := DocDataWriter.StylesExporter.GetCharacterStyleIndex(ARun.CharacterStyleIndex);
  DocDataWriter.WriteInlinePictureRun(CurrentCharacterPosition, ACharacterStyleIndex, DataStreamWriter.BaseStream.Position, ARun.CharacterProperties);
  ImagesIterator.WritePictureDescriptor(DataStreamWriter, ARun);
  Inc(FCurrentCharacterPosition);
end;

procedure TdxDocExporter.ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun);
begin
  DocDataWriter.FloatingObjectsExporter.RegisterFloatingObject(ARun, GetRelativeCharacterPosition);
  ExportTextRunBaseCore(ARun, TdxTextCodes.FloatingObjectAnchor, True);
end;

procedure TdxDocExporter.ExportBookmarkStart(ABookmark: TdxBookmark);
begin
  inherited ExportBookmarkStart(ABookmark);
  DocDataWriter.BookmarkIterator.AddBookmarkStart(ABookmark.Name, CurrentCharacterPosition);
end;

procedure TdxDocExporter.ExportBookmarkEnd(ABookmark: TdxBookmark);
begin
  inherited ExportBookmarkEnd(ABookmark);
  DocDataWriter.BookmarkIterator.AddBookmarkEnd(ABookmark.Name, CurrentCharacterPosition);
end;

procedure TdxDocExporter.ExportRangePermissionStart(ARangePermission: TdxRangePermission);
begin
  inherited ExportRangePermissionStart(ARangePermission);
  DocDataWriter.PermissionIterator.AddPermissionStart(ARangePermission, CurrentCharacterPosition);
end;

procedure TdxDocExporter.ExportRangePermissionEnd(ARangePermission: TdxRangePermission);
begin
  inherited ExportRangePermissionEnd(ARangePermission);
  DocDataWriter.PermissionIterator.AddPermissionEnd(ARangePermission, CurrentCharacterPosition);
end;


procedure TdxDocExporter.ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun);
var
  AFieldBeginDescriptor: TdxDocFieldBeginDescriptor;
begin
  inherited ExportFieldCodeStartRun(ARun);
  AFieldBeginDescriptor := FieldsIterator.CreateFieldBeginDescriptor(ActivePieceTable);
  DocDataWriter.FieldTable.AddEntry(GetRelativeCharacterPosition, AFieldBeginDescriptor);
  ExportTextRunBaseCore(ARun, TdxTextCodes.FieldBegin, True);
end;

procedure TdxDocExporter.ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun);
var
  AFieldSeparatorDescriptor: TdxDocFieldSeparatorDescriptor;
begin
  inherited ExportFieldCodeEndRun(ARun);
  AFieldSeparatorDescriptor := TdxDocFieldSeparatorDescriptor.Create;
  DocDataWriter.FieldTable.AddEntry(GetRelativeCharacterPosition, AFieldSeparatorDescriptor);
  ExportTextRunBaseCore(ARun, TdxTextCodes.FieldSeparator, True);
end;

procedure TdxDocExporter.ExportFieldResultEndRun(ARun: TdxFieldResultEndRun);
var
  AFieldEndDescriptor: IdxDocFieldDescriptor;
begin
  inherited ExportFieldResultEndRun(ARun);
  AFieldEndDescriptor := FieldsIterator.CreateFieldEndDescriptor(FieldLevel);
  DocDataWriter.FieldTable.AddEntry(GetRelativeCharacterPosition, AFieldEndDescriptor);
  ExportTextRunBaseCore(ARun, TdxTextCodes.FieldEnd, True);
end;

procedure TdxDocExporter.ExportFootNoteRun(ARun: TdxFootNoteRun);
begin
  if not DocumentModel.DocumentCapabilities.FootNotesAllowed then
    Exit;
  if ActivePieceTable.IsMain then
  begin
    inherited ExportFootNoteRun(ARun);
    DocDataWriter.NotesExporter.AddFootNoteReferenceEntry(GetRelativeCharacterPosition, True);
  end;
  ExportTextRunBaseCore(ARun, TdxTextCodes.AutoNumberedFootNoteReference, True);
end;

procedure TdxDocExporter.ExportEndNoteRun(ARun: TdxEndNoteRun);
begin
  if not DocumentModel.DocumentCapabilities.EndNotesAllowed then
    Exit;
  if ActivePieceTable.IsMain then
  begin
    inherited ExportEndNoteRun(ARun);
    DocDataWriter.NotesExporter.AddEndNoteReferenceEntry(GetRelativeCharacterPosition, True);
  end;
  ExportTextRunBaseCore(ARun, TdxTextCodes.AutoNumberedFootNoteReference, True);
end;

procedure TdxDocExporter.ExportRow(ARow: TdxTableRow; ATableInfo: TdxTableInfo);
begin
  inherited ExportRow(ARow, ATableInfo);
  ExportRowMark(ARow);
end;

function TdxDocExporter.ExportTable(ATableInfo: TdxTableInfo): TdxParagraphIndex;
var
  AIndex: TdxParagraphIndex;
begin
  DocDataWriter.TablesExporter.AdvanceNext(ATableInfo);
  AIndex := inherited ExportTable(ATableInfo);
  DocDataWriter.TablesExporter.FinishTable;
  Result := AIndex;
end;

procedure TdxDocExporter.ExportRowMark(ARow: TdxTableRow);
var
  ARowMark: string;
  AParagraphStyleIndex: Integer;
begin
  ARowMark := DocDataWriter.TablesExporter.GetTableUnitMark;
  AParagraphStyleIndex := DocDataWriter.StylesExporter.GetParagraphStyleIndex(DocumentModel.ParagraphStyles.DefaultItemIndex);
  DocDataWriter.WriteParagraph(CurrentCharacterPosition, AParagraphStyleIndex, ARow);
  ExportTextRunBaseCore(ARowMark);
end;

procedure TdxDocExporter.ExportTextRunBaseCore(const AText: string);
begin
  ExportTextRunBaseCore(nil, AText, False);
end;

procedure TdxDocExporter.ExportTextRunBaseCore(ARun: TdxTextRunBase; const AText: string; ASpecial: Boolean = False);
begin
  MainStreamWriter.Write(TdxEncoding.Unicode.GetBytes(AText));
  DocDataWriter.WriteTextRun(CurrentCharacterPosition, ARun, ASpecial);
  CurrentCharacterPosition := CurrentCharacterPosition + Length(AText);
end;

procedure TdxDocExporter.UpdateState(AState: TdxDocContentState);
begin
  CurrentDocumentPartStart := CurrentCharacterPosition;

  DocDataWriter.SetState(AState);
end;

procedure TdxDocExporter.SetActivePieceTable(APieceTable: TdxPieceTable);
begin
  FActivePieceTable := APieceTable;
  FieldsIterator.UpdateFieldsInfo(APieceTable);
end;

function TdxDocExporter.GetRelativeCharacterPosition: Integer;
begin
  Result := CurrentCharacterPosition - CurrentDocumentPartStart;
end;

function TdxDocExporter.GetOptions: TdxDocDocumentExporterOptions;
begin
  Result := TdxDocDocumentExporterOptions(inherited Options);
end;

end.
