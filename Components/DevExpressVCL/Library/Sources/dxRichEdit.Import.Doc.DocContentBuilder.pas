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
unit dxRichEdit.Import.Doc.DocContentBuilder;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics, dxOLEDocument,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Platform.Font,

  dxRichEdit.Options,
  dxRichEdit.Import.Doc.FormattedDiskPage,
  dxRichEdit.Import.Doc.FormattedDiskPageHelper,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.SectionPropertiesHelper,
  dxRichEdit.Import.Doc.DocPieceTable,
  dxRichEdit.Import.Doc.FontFamilyName,
  dxRichEdit.Import.Doc.DocCharacterFormattingInfo,
  dxRichEdit.Import.Doc.DCO,
  dxRichEdit.Import.Doc.DocCommand,
  dxRichEdit.Import.Doc.DocStyleSheet,
  dxRichEdit.Import.Doc.ListFormatInformation,
  dxRichEdit.Import.Doc.ListFormatOverride,
  dxRichEdit.Import.Doc.DocumentFileRecords,
  dxRichEdit.Import.Doc.FileInformationBlock,
  dxRichEdit.Import.Doc.DocContentIterator,
  dxRichEdit.Import.Doc.DocumentVariables,
  dxRichEdit.Import.Doc.DocObjectCollection,
  dxRichEdit.Import.Doc.DocumentProperties;

type

  { TdxDocumentStreams }

  TdxDocumentStreams = class
  public const
    MainStreamName                 = 'WordDocument';
    Stream0TableName               = '0Table';
    Stream1TableName               = '1Table';
    DataStreamName                 = 'Data';
    SummaryInformationName         = #5'SummaryInformation';
    DocumentSummaryInformationName = #5'DocumentSummaryInformation';
  end;

  TdxProcessSymbolDelegate = procedure(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer) of object;

  { TdxDocFontManager }

  TdxDocFontManager = class
  strict private
    FPreviousFont: string;
    FShouldResetFont: Boolean;
    FDocumentFonts: TdxObjectList<TdxDocFontFamilyName>;
  protected
    function GetFonts(AReader: TBinaryReader; AFontTableOffset: Integer): TdxObjectList<TdxDocFontFamilyName>;

    property DocumentFonts: TdxObjectList<TdxDocFontFamilyName> read FDocumentFonts;
  public
    constructor Create(AReader: TBinaryReader; AOffset: Integer);
    destructor Destroy; override;
    procedure SetFontName(APropertyContainer: TdxDocPropertyContainer);
    function TryResetFontName(APropertyContainer: TdxDocPropertyContainer): Boolean;
    function GetFontName(AIndex: SmallInt): string;
  end;

  { TdxUnsupportedObject }

  TdxUnsupportedObject = class(TInterfacedObject, IdxDocObject)
  strict private
    FPropertyContainer: TdxDocPropertyContainer;
    FPosition: Integer;
    FLength: Integer;
    function GetDocObjectType: TdxDocObjectType;
    function GetObject: TObject;
  private
    function GetLength: Integer;
  protected
    function GetPosition: Integer;
    function GetPropertyContainer: TdxDocPropertyContainer;
  public
    constructor Create(APosition: Integer; APropertyContainer: TdxDocPropertyContainer);

    property DocObjectType: TdxDocObjectType read GetDocObjectType;
    property PropertyContainer: TdxDocPropertyContainer read GetPropertyContainer;
    property Position: Integer read GetPosition write FPosition;
    property Length: Integer read GetLength write FLength;
  end;

  { TdxDocContentBuilder }

  TdxDocContentBuilder = class(TcxIUnknownObject,
    IdxDocOfficeImageCreator)
  public const
    LastSupportedVersion = Integer(105);
    SectorSize           = Integer(512);
    NoBreakHyphen        = #30;
    Hyphen               = '-';
  strict private
    FDocumentModel: TdxDocumentModel;
    FObjectsToDelete: TdxFastObjectList;
    FOLEDocument: TdxOLEDocument;
    FTextRunBorders: TdxObjectList<TdxTextRunBorder>;
    FMainStreamReader: TdxVirtualStreamBinaryReader;
    FDataStreamReader: TBinaryReader;
    FTableStreamReader: TBinaryReader;
    FPieceTable: TdxDocPieceTable;
    FFkpHelper: TdxFormattedDiskPageHelper;
    FSectionPropertiesHelper: TdxSectionPropertiesHelper;
    FFactory: TdxDocCommandFactory;
    FStyleSheet: TdxDocStyleSheet;
    FDocProperties: TdxDocDocumentProperties;
    FDocVariables: TdxDocumentVariables;
    FListFormatInfo: TdxDocListFormatInformation;
    FListFormatOverrideInfo: TdxDocListOverrideFormatInformation;
    FDocFileRecords: TdxDocumentFileRecords;
    FIterator: TdxDocContentIterator;
    FSpecialSymbolProcessors: TDictionary<Char, TdxProcessSymbolDelegate>;
    FExpectedSpecialSymbolsProcessors: TDictionary<Char, TdxProcessSymbolDelegate>;
    FOptions: TdxDocDocumentImporterOptions;
    FFontManager: TdxDocFontManager;
    FFib: TdxFileInformationBlock;
  protected
    //IdxDocOfficeImageCreator
    function CreateImage(const AStream: TStream): TdxOfficeImageReference; overload;
    function CreateImage(AImage: TdxOfficeImage): TdxOfficeImageReference; overload;
    function CreateMetafile(AStream: TMemoryStream; AMapMode: TdxMapMode; APictureWidth, APictureHeight: Integer): TdxOfficeImageReference;

    property PieceTable: TdxDocPieceTable read FPieceTable;
    property FKPHelper: TdxFormattedDiskPageHelper read FFkpHelper;
    property SectionPropertiesHelper: TdxSectionPropertiesHelper read FSectionPropertiesHelper;
    property MainStreamReader: TdxVirtualStreamBinaryReader read FMainStreamReader;
  public
    constructor Create(AModel: TdxDocumentModel; AOptions: TdxDocDocumentImporterOptions);
    destructor Destroy; override;
    procedure AddToGC(AObject: TObject);
    procedure RemoveFromGC(AObject: TObject);
    procedure BuildDocumentContent(AStream: TStream; AModel: TdxDocumentModel; ALeaveOpen: Boolean = True);
    procedure Initialize(AModel: TdxDocumentModel);
    procedure CheckFileInfo(AFib: TdxFileInformationBlock);
    procedure CreateSpecialSymbolsProcessors;
    procedure CreateExpectedSpecialSymbolsProcessors;
    procedure GetTableStreamInformation(ATableStreamReader: TBinaryReader; AFib: TdxFileInformationBlock);
    procedure CalculateTextRunBorders;
    procedure CheckPieceDescriptorBorders(I: Integer);
    function GetPieceTable(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxDocPieceTable;
    function GetFormattedDiskPageHelper(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxFormattedDiskPageHelper;
    procedure GetListInformation(AReader: TBinaryReader; AFib: TdxFileInformationBlock);
    procedure ReadTextStream;
    procedure ProcessPieceDescriptor(APcdIndex: Integer);
    procedure ProcessTextRunBorder(APcdIndex: Integer; ACurrentBorder: TdxTextRunBorder; ANextBorder: TdxTextRunBorder);
    procedure ProcessObjectInfo(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure ProcessTextCharacters(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure ProcessNonTextCharacters(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    function GetBorderDocObjects(AReason: TdxTextRunStartReasons; AOffset: Integer; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocObjectCollection;
    function GetDocObjectByParagraphMark(AOffset: Integer; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): IdxDocObject;
    function GetDocObjectInfo(APcdIndex: Integer; ACurrentBorder: TdxTextRunBorder; ANextBorder: TdxTextRunBorder): TdxDocObjectInfo;
    function GetPictures(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocImageCollection;
    function GetFloatingObject(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocFloatingObjectBase;
    function GetFieldData(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocObjectBase;
    function GetTableUnit(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocObjectBase;
    function GetSection(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocSection;
    procedure SkipUnsupportedObject(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertFloatingObject(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertAutoNumberedFootnoteReference(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertAnnotationReference(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertInlinePicture(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertInlinePictureCore(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertFieldData(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertFieldBegin(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertFieldSeparator(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertFieldEnd(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertExpectedFieldBegin(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertExpectedFieldSeparator(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertExpectedFieldEnd(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    procedure InsertSpecialSymbol(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
    function GetInnerBorders(APcdIndex: Integer): TdxList<TdxTextRunBorder>;
    function GetString(AOffset: Integer; ALength: Integer; AEncoding: TEncoding): string;
    procedure CorrectParagraphBorderType(AParagraphBorder: TdxTextRunBorder; APcdIndex: Integer);
    procedure BeginEmbeddedContent(AContent: TdxDocObjectCollection);
    procedure EndEmbeddedContent;

    property DataReader: TBinaryReader read FDataStreamReader;
    property Factory: TdxDocCommandFactory read FFactory;
    property Iterator: TdxDocContentIterator read FIterator;
    property ListInfo: TdxDocListFormatInformation read FListFormatInfo;
    property ListOverrideInfo: TdxDocListOverrideFormatInformation read FListFormatOverrideInfo;
    property StyleSheet: TdxDocStyleSheet read FStyleSheet;
    property FontManager: TdxDocFontManager read FFontManager write FFontManager;
    property DocumentProperties: TdxDocDocumentProperties read FDocProperties;
    property DocumentVariables: TdxDocumentVariables read FDocVariables;
    property DocFileRecords: TdxDocumentFileRecords read FDocFileRecords;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Import.Doc.ComplexFileInformation,
  dxRichEdit.Import.Doc.BinTable,
  dxRichEdit.Import.Doc.BlipContainer,
  dxRichEdit.Import.Doc.DocFieldsImportHelper,
  dxStringHelper,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.Import.Doc.DocImporter, dxEncoding;

{ TdxDocFontManager }

constructor TdxDocFontManager.Create(AReader: TBinaryReader; AOffset: Integer);
begin
  FPreviousFont := TdxDocCharacterFormattingInfo.DefaultFontName;
  FShouldResetFont := False;
  Assert(AReader <> nil, 'reader');
  FDocumentFonts := GetFonts(AReader, AOffset);
end;

destructor TdxDocFontManager.Destroy;
begin
  FDocumentFonts.Free;
  inherited Destroy;
end;

function TdxDocFontManager.GetFonts(AReader: TBinaryReader; AFontTableOffset: Integer): TdxObjectList<TdxDocFontFamilyName>;
var
  I: Integer;
  AFontsCount, AExtraDataSize: SmallInt;
  ADocFontFamilyName: TdxDocFontFamilyName;
begin
  AReader.BaseStream.Seek(AFontTableOffset, TSeekOrigin.soBeginning);
  AFontsCount := AReader.ReadSmallInt;
  AExtraDataSize := AReader.ReadSmallInt;
  Result := TdxObjectList<TdxDocFontFamilyName>.Create;
  Result.Capacity := AFontsCount;
  for I := 0 to AFontsCount - 1 do
  begin
    ADocFontFamilyName := TdxDocFontFamilyName.FromStream(AReader);
    AReader.BaseStream.Seek(AExtraDataSize, TSeekOrigin.soCurrent);
    Result.Add(ADocFontFamilyName);
  end;
end;

procedure TdxDocFontManager.SetFontName(APropertyContainer: TdxDocPropertyContainer);
var
  ACharacterInfo: TdxCharacterInfo;
  AIndex: SmallInt;
begin
  if TryResetFontName(APropertyContainer) then
    Exit;
  ACharacterInfo := APropertyContainer.CharacterInfo;
  if ACharacterInfo.Special then
    AIndex := ACharacterInfo.SpecialCharactersFontFamilyNameIndex
  else
    AIndex := APropertyContainer.FontFamilyNameIndex;
  ACharacterInfo.FormattingInfo.FontName := GetFontName(AIndex);
  if ACharacterInfo.Special then
    FShouldResetFont := True
  else
    FPreviousFont := ACharacterInfo.FormattingInfo.FontName;
end;

function TdxDocFontManager.TryResetFontName(APropertyContainer: TdxDocPropertyContainer): Boolean;
var
  ACharacterInfo: TdxCharacterInfo;
begin
  ACharacterInfo := APropertyContainer.CharacterInfo;
  if (ACharacterInfo <> nil) and ACharacterInfo.FormattingOptions.UseFontName then
    Exit(False);
  if FShouldResetFont then
  begin
    APropertyContainer.Update([TdxChangeActionType.Character]);
    APropertyContainer.CharacterInfo.FormattingInfo.FontName := FPreviousFont;
    FShouldResetFont := False;
  end;
  Result := True;
end;

function TdxDocFontManager.GetFontName(AIndex: SmallInt): string;
begin
  if (AIndex >= 0) and (AIndex < DocumentFonts.Count) then
    Exit(DocumentFonts[AIndex].FontName);
  Result := TdxDocCharacterFormattingInfo.DefaultFontName;
end;

{ TdxUnsupportedObject }

constructor TdxUnsupportedObject.Create(APosition: Integer; APropertyContainer: TdxDocPropertyContainer);
begin
  FPropertyContainer := APropertyContainer;
  Position := APosition;
  Length := 1;
end;

function TdxUnsupportedObject.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.UnsupportedObject;
end;

function TdxUnsupportedObject.GetLength: Integer;
begin
  Result := FLength;
end;

function TdxUnsupportedObject.GetObject: TObject;
begin
  Result := Self;
end;

function TdxUnsupportedObject.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TdxUnsupportedObject.GetPropertyContainer: TdxDocPropertyContainer;
begin
  Result := FPropertyContainer;
end;

{ TdxDocContentBuilder }

constructor TdxDocContentBuilder.Create(AModel: TdxDocumentModel; AOptions: TdxDocDocumentImporterOptions);
begin
  inherited Create;
  FDocumentModel := AModel;
  FObjectsToDelete := TdxFastObjectList.Create(True, 2048);
  FFactory := TdxDocCommandFactory.Create(AModel);
  FOptions := AOptions;
end;

destructor TdxDocContentBuilder.Destroy;
begin
  FOLEDocument.Free;
  FTextRunBorders.Free;
  FMainStreamReader.Free;
  FDataStreamReader.Free;
  FTableStreamReader.Free;
  FPieceTable.Free;
  FSectionPropertiesHelper.Free;
  FFactory.Free;
  FStyleSheet.Free;
  FDocProperties.Free;
  FDocVariables.Free;
  FListFormatInfo.Free;
  FListFormatOverrideInfo.Free;
  FDocFileRecords.Free;
  FIterator.Free;
  FSpecialSymbolProcessors.Free;
  FExpectedSpecialSymbolsProcessors.Free;
  FFontManager.Free;
  FFkpHelper.Free;
  FFib.Free;
  FObjectsToDelete.Free;
  inherited Destroy;
end;

procedure TdxDocContentBuilder.BuildDocumentContent(AStream: TStream; AModel: TdxDocumentModel; ALeaveOpen: Boolean = True);
var
  ANamedStream: TStream;
begin
  FOLEDocument := TdxOLEDocument.Create(AStream, TdxOLEDocumentMode.dmReading);
  FTextRunBorders := TdxObjectList<TdxTextRunBorder>.Create;
  ANamedStream := FOLEDocument.StreamByName(TdxDocumentStreams.MainStreamName);
  FMainStreamReader := TdxVirtualStreamBinaryReader.Create(ANamedStream);
  ANamedStream := FOLEDocument.StreamByName(TdxDocumentStreams.DataStreamName);
  if ANamedStream <> nil then
    FDataStreamReader := TdxVirtualStreamBinaryReader.Create(ANamedStream);

  if FMainStreamReader = nil then
    TdxDocImporter.ThrowInvalidDocFile;

  Initialize(AModel);
  ReadTextStream;
end;

procedure TdxDocContentBuilder.Initialize(AModel: TdxDocumentModel);
var
  ATableStreamName: string;
  AStream: TStream;
begin
  FFib := TdxFileInformationBlock.Create;
  FFib.Read(MainStreamReader);
  FFactory.Version := FFib.Version;
  CheckFileInfo(FFib);

  if TdxFileInformationFlag.TableStreamType in FFib.Flags then
    ATableStreamName := TdxDocumentStreams.Stream1TableName
  else
    ATableStreamName := TdxDocumentStreams.Stream0TableName;

  AStream := FOLEDocument.StreamByName(ATableStreamName);
  if AStream = nil then
    TdxDocImporter.ThrowInvalidDocFile;
  FTableStreamReader := TBinaryReader.Create(AStream);

  CreateSpecialSymbolsProcessors;
  CreateExpectedSpecialSymbolsProcessors;
  GetTableStreamInformation(FTableStreamReader, FFib);

  CalculateTextRunBorders;
  FIterator := TdxDocContentIterator.Create(Self, FFib, MainStreamReader, FTableStreamReader, AModel);
//  ASummaryInformationStreamReader := FileReader.GetCachedPackageFileReader(TdxDocumentStreams.SummaryInformationName);
//  if ASummaryInformationStreamReader <> nil then
//    FSummaryInfo := TdxSummaryInfo.FromStream(ASummaryInformationStreamReader);
//  ADocumentSummaryInformationStreamReader := FileReader.GetCachedPackageFileReader(TdxDocumentStreams.DocumentSummaryInformationName);
//  if ADocumentSummaryInformationStreamReader <> nil then
//    FDocumentSummaryInfo := TdxDocumentSummaryInfo.FromStream(ADocumentSummaryInformationStreamReader);
end;

procedure TdxDocContentBuilder.CheckFileInfo(AFib: TdxFileInformationBlock);
begin
  if AFib.Version <= LastSupportedVersion then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUnsupportedDocVersion));
  if TdxFileInformationFlag.Encryped in AFib.Flags then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionEncryptedFile));
end;

procedure TdxDocContentBuilder.CreateSpecialSymbolsProcessors;
begin
  FSpecialSymbolProcessors := TDictionary<Char, TdxProcessSymbolDelegate>.Create(7);
  FSpecialSymbolProcessors.Add(TdxTextCodes.InlinePicture, InsertInlinePicture);
  FSpecialSymbolProcessors.Add(TdxTextCodes.AutoNumberedFootNoteReference, InsertAutoNumberedFootnoteReference);
  FSpecialSymbolProcessors.Add(TdxTextCodes.AnnotationReference, InsertAnnotationReference);
  FSpecialSymbolProcessors.Add(TdxTextCodes.FloatingObjectAnchor, InsertFloatingObject);
  FSpecialSymbolProcessors.Add(TdxTextCodes.FieldBegin, InsertFieldBegin);
  FSpecialSymbolProcessors.Add(TdxTextCodes.FieldSeparator, InsertFieldSeparator);
  FSpecialSymbolProcessors.Add(TdxTextCodes.FieldEnd, InsertFieldEnd);
  FSpecialSymbolProcessors.Add(TdxTextCodes.SpecialSymbol, InsertSpecialSymbol);
end;

procedure TdxDocContentBuilder.CreateExpectedSpecialSymbolsProcessors;
begin
  FExpectedSpecialSymbolsProcessors := TDictionary<Char, TdxProcessSymbolDelegate>.Create;
  FExpectedSpecialSymbolsProcessors.Add(TdxTextCodes.AutoNumberedFootNoteReference, InsertAutoNumberedFootnoteReference);
  FExpectedSpecialSymbolsProcessors.Add(TdxTextCodes.FieldBegin, InsertExpectedFieldBegin);
  FExpectedSpecialSymbolsProcessors.Add(TdxTextCodes.FieldSeparator, InsertExpectedFieldSeparator);
  FExpectedSpecialSymbolsProcessors.Add(TdxTextCodes.FieldEnd, InsertExpectedFieldEnd);
end;

procedure TdxDocContentBuilder.GetTableStreamInformation(ATableStreamReader: TBinaryReader; AFib: TdxFileInformationBlock);
begin
  FPieceTable := GetPieceTable(ATableStreamReader, AFib);
  FFkpHelper := GetFormattedDiskPageHelper(ATableStreamReader, AFib);
  FStyleSheet := TdxDocStyleSheet.FromStream(ATableStreamReader, AFib.StyleSheetOffset, AFib.StyleSheetSize);
  if AFib.FontTableSize <> 0 then
    FontManager := TdxDocFontManager.Create(ATableStreamReader, AFib.FontTableOffset);
  FSectionPropertiesHelper := SectionPropertiesHelper.FromStream(MainStreamReader, ATableStreamReader, AFib.SectionTableOffset, AFib.SectionTableSize);
  GetListInformation(ATableStreamReader, AFib);
  FDocProperties := TdxDocDocumentProperties.FromStream(ATableStreamReader, AFib.DocumentPropertiesOffset);
  FFactory.DocumentProperties := FDocProperties;
  FDocVariables := TdxDocumentVariables.FromStream(ATableStreamReader, AFib.DocumentVariablesOffset, AFib.DocumentVariablesSize);
  FDocFileRecords := TdxDocumentFileRecords.FromStream(ATableStreamReader, AFib.DocumentFileRecordsOffset, AFib.DocumentFileRecordsSize);
end;

procedure TdxDocContentBuilder.CalculateTextRunBorders;
var
  ACount, I: Integer;
begin
  FTextRunBorders.Free;
  FTextRunBorders := FKPHelper.GetTextRunBorders;
  ACount := PieceTable.PcdCount;
  for I := 0 to ACount - 1 do
    CheckPieceDescriptorBorders(I);
end;

procedure TdxDocContentBuilder.CheckPieceDescriptorBorders(I: Integer);
var
  AOffset, ALength, AIndex: Integer;
  ATextRunBorder: TdxTextRunBorder;
  AComparable: IdxComparable<TdxTextRunBorder>;
begin
  AOffset := PieceTable.GetOffset(I);
  ALength := PieceTable.GetLength(I);
  AComparable := TdxTextRunBorderComparable.Create(AOffset);
  if not TdxAlgorithms1<TdxTextRunBorder>.BinarySearch(FTextRunBorders, AComparable, AIndex) then
  begin
    ATextRunBorder := TdxTextRunBorder.Create(AOffset, [TdxTextRunStartReason.TextRunMark]);
    FTextRunBorders.Insert(AIndex, ATextRunBorder);
  end;
  AComparable := TdxTextRunBorderComparable.Create(AOffset + ALength);
  if not TdxAlgorithms1<TdxTextRunBorder>.BinarySearch(FTextRunBorders, AComparable, AIndex) then
  begin
    ATextRunBorder := TdxTextRunBorder.Create(AOffset + ALength, [TdxTextRunStartReason.TextRunMark]);
    FTextRunBorders.Insert(AIndex, ATextRunBorder);
  end;
end;

function TdxDocContentBuilder.GetPieceTable(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxDocPieceTable;
var
  AClxInfo: TdxComplexFileInformation;
  ALastCharacterPosition: Integer;
begin
  AClxInfo := TdxComplexFileInformation.FromStream(AReader, AFib.ComplexFileInformationOffset, AFib.ComplexFileInformationSize);
  try
  if AClxInfo.PieceTable <> nil then
    Exit(TdxDocPieceTable.FromByteArray(AClxInfo.PieceTable));
  finally
    AClxInfo.Free;
  end;
  ALastCharacterPosition := AFib.MainDocumentLength + AFib.FootNotesLength + AFib.HeadersFootersLength;
  Result := TdxDocPieceTable.CreateDefault(AFib.FirstCharacterFileOffset, ALastCharacterPosition);
end;

function TdxDocContentBuilder.GetFormattedDiskPageHelper(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxFormattedDiskPageHelper;
var
  AMainStreamLength: Int64;
  AParagraphsBinTable, ACharactersBinTable: TdxBinTable;
begin
  AMainStreamLength := MainStreamReader.BaseStream.Size;
  AParagraphsBinTable := TdxBinTable.FromStream(AReader, AFib.ParagraphTableOffset, AFib.ParagraphTableSize, AMainStreamLength);
  ACharactersBinTable := TdxBinTable.FromStream(AReader, AFib.CharacterTableOffset, AFib.CharacterTableSize, AMainStreamLength);
  Result := TdxFormattedDiskPageHelper.Create(MainStreamReader, DataReader, AParagraphsBinTable, ACharactersBinTable);
end;

procedure TdxDocContentBuilder.GetListInformation(AReader: TBinaryReader; AFib: TdxFileInformationBlock);
begin
  FListFormatInfo := TdxDocListFormatInformation.FromStream(AReader, AFib.ListFormatInformationOffset, AFib.ListFormatInformationSize);
  FListFormatOverrideInfo := TdxDocListOverrideFormatInformation.FromStream(AReader, AFib.ListFormatOverrideInformationOffset, AFib.ListFormatOverrideInformationSize);
end;

procedure TdxDocContentBuilder.ReadTextStream;
var
  ACount, I: Integer;
  ALastPropertiesContainer: TdxDocPropertyContainer;
begin
  ACount := PieceTable.PcdCount;
  for I := 0 to ACount - 1 do
    ProcessPieceDescriptor(I);

  ALastPropertiesContainer := Iterator.MainTextDocObjects[Iterator.MainTextDocObjects.Count - 1].PropertyContainer;
  SectionPropertiesHelper.UpdateCurrentSectionProperties(MainStreamReader, DataReader, ALastPropertiesContainer);
end;

procedure TdxDocContentBuilder.ProcessPieceDescriptor(APcdIndex: Integer);
var
  AInnerBorders: TdxList<TdxTextRunBorder>;
  ACount, I: Integer;
begin
  AInnerBorders := GetInnerBorders(APcdIndex);
  try
    ACount := AInnerBorders.Count - 1;
    for I := 0 to ACount - 1 do
      if Iterator.ShouldProcessTextRun then
        ProcessTextRunBorder(APcdIndex, AInnerBorders[I], AInnerBorders[I + 1]);
  finally
    AInnerBorders.Free;
  end;
end;

procedure TdxDocContentBuilder.ProcessTextRunBorder(APcdIndex: Integer; ACurrentBorder: TdxTextRunBorder; ANextBorder: TdxTextRunBorder);
var
  APropertyContainer: TdxDocPropertyContainer;
  AObjectInfo: TdxDocObjectInfo;
  ABorders: TdxDocObjectCollection;
begin
  APropertyContainer := FKPHelper.UpdateCharacterProperties(ACurrentBorder.Offset, Factory);
  AObjectInfo := GetDocObjectInfo(APcdIndex, ACurrentBorder, ANextBorder);
  if not (APropertyContainer.IsDeleted and FOptions.IgnoreDeletedText) then
  begin
    ProcessObjectInfo(AObjectInfo, APropertyContainer);
    ABorders := GetBorderDocObjects(ANextBorder.Reason, ACurrentBorder.Offset, AObjectInfo, APropertyContainer);
    try
      Iterator.Destination.AddRange(ABorders);
    finally
      ABorders.Free;
    end;
  end;
  Iterator.UpdateState;
end;

procedure TdxDocContentBuilder.ProcessObjectInfo(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
var
  ADestination: TdxDocObjectCollection;
begin
  if AObjectInfo.Text = '' then
    Exit;
  ADestination := Iterator.Destination;
  if (APropertyContainer.CharacterInfo <> nil) and APropertyContainer.CharacterInfo.Special then
    ProcessNonTextCharacters(ADestination, AObjectInfo, APropertyContainer)
  else
    ProcessTextCharacters(ADestination, AObjectInfo, APropertyContainer);
end;

procedure TdxDocContentBuilder.ProcessTextCharacters(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
var
  ACh: Char;
  AExpectedSymbolDelegate: TdxProcessSymbolDelegate;
begin
  ACh := AObjectInfo.Text[1];
  if FExpectedSpecialSymbolsProcessors.TryGetValue(ACh, AExpectedSymbolDelegate) then
    AExpectedSymbolDelegate(ADestination, AObjectInfo, APropertyContainer)
  else
  begin
    ADestination.Add(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.TextRun, AObjectInfo, APropertyContainer));
    AObjectInfo.Position := AObjectInfo.Position + Length(AObjectInfo.Text);
  end;
end;

procedure TdxDocContentBuilder.ProcessNonTextCharacters(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
var
  ACount, I: Integer;
  ACh: Char;
  ASpecialCharacterProcessor: TdxProcessSymbolDelegate;
begin
  ACount := Length(AObjectInfo.Text);
  for I := 1 to ACount do
  begin
    ACh := AObjectInfo.Text[I];
    if not FSpecialSymbolProcessors.TryGetValue(ACh, ASpecialCharacterProcessor) then
      ASpecialCharacterProcessor := SkipUnsupportedObject;
    ASpecialCharacterProcessor(ADestination, AObjectInfo, APropertyContainer);
    AObjectInfo.Position := AObjectInfo.Position + 1;
  end;
end;

function TdxDocContentBuilder.GetBorderDocObjects(AReason: TdxTextRunStartReasons; AOffset: Integer; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocObjectCollection;
begin
  Result := TdxDocObjectCollection.Create;
  if TdxTextRunStartReason.ParagraphMark in AReason then
    Result.Add(GetDocObjectByParagraphMark(AOffset, AObjectInfo, APropertyContainer) as IdxDocObject);
  if TdxTextRunStartReason.SectionMark in AReason then
  begin
    FKPHelper.UpdateParagraphProperties(AOffset, APropertyContainer);
    Result.Add(GetSection(AObjectInfo, APropertyContainer) as IdxDocObject);
  end;
  if TdxTextRunStartReason.TableUnitMark in AReason then
  begin
    FKPHelper.UpdateParagraphProperties(AOffset, APropertyContainer);
    Result.Add(GetTableUnit(AObjectInfo, APropertyContainer) as IdxDocObject);
  end;
end;

function TdxDocContentBuilder.GetDocObjectByParagraphMark(AOffset: Integer; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): IdxDocObject;
begin
  FKPHelper.UpdateParagraphProperties(AOffset, APropertyContainer);
  if (APropertyContainer.ParagraphInfo <> nil) and APropertyContainer.ParagraphInfo.NestedTableTrailer then
    Exit(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.TableRow, AObjectInfo, APropertyContainer));
  if (APropertyContainer.ParagraphInfo <> nil) and APropertyContainer.ParagraphInfo.InnerTableCell then
    Exit(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.TableCell, AObjectInfo, APropertyContainer));
  Result := TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.Paragraph, AObjectInfo, APropertyContainer);
end;

function TdxDocContentBuilder.GetDocObjectInfo(APcdIndex: Integer; ACurrentBorder: TdxTextRunBorder; ANextBorder: TdxTextRunBorder): TdxDocObjectInfo;
var
  AText: string;
  APosition: Integer;
  AParagraphOrTableUnitMark: TdxTextRunStartReasons;
begin
  AText := GetString(ACurrentBorder.Offset, ANextBorder.Offset - ACurrentBorder.Offset, PieceTable.GetEncoding(APcdIndex));
  APosition := Iterator.AdvanceNext(Length(AText));
  AParagraphOrTableUnitMark := [TdxTextRunStartReason.ParagraphMark, TdxTextRunStartReason.TableUnitMark];
  if (ANextBorder.Reason * AParagraphOrTableUnitMark) = [] then
    Exit(TdxDocObjectInfo.Create(APosition, AText));
  CorrectParagraphBorderType(ANextBorder, APcdIndex);
  if not (TdxTextRunStartReason.ColumnBreak in ANextBorder.Reason) then
    AText := TdxStringHelper.Remove(AText, Length(AText) - 1);
  Result := TdxDocObjectInfo.Create(APosition, AText);
end;

function TdxDocContentBuilder.GetPictures(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocImageCollection;
var
  APictureDescriptor: TdxPictureDescriptor;
  ABlip: TdxBlipBase;
begin
  APictureDescriptor := TdxPictureDescriptor.FromStream(DataReader, APropertyContainer.DataStreamOffset, Self);
  try
    if Iterator.ShapeField then
    begin
      ABlip := Iterator.FloatingObjectsIterator.GetBlip(AObjectInfo.Position - 1);
      if ABlip <> nil then
      begin
        APictureDescriptor.Images.Add(ABlip.Image.Clone);
        APictureDescriptor.MetafileHeaders.Add(ABlip.MetafileHeader.Clone);
      end;
    end;
    Result := TdxDocImageCollection.Create(AObjectInfo, APropertyContainer, APictureDescriptor);
  finally
    APictureDescriptor.Free;
  end;
end;

function TdxDocContentBuilder.GetFloatingObject(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocFloatingObjectBase;
begin
  Result := Iterator.GetFloatingObject(AObjectInfo, APropertyContainer);
  if Iterator.ShapeField and (Result is TdxDocPictureFloatingObject) then
    FreeAndNil(Result);
end;

function TdxDocContentBuilder.GetFieldData(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocObjectBase;
var
  AFieldType: TdxFieldType;
  AHyperlinkInfo: TdxDocHyperlinkInfo;
begin
  AFieldType := Iterator.GetCurrentFieldType;
  if AFieldType = TdxFieldType.Hyperlink then
  begin
    AHyperlinkInfo := TdxDocHyperlinkInfo.Create(DataReader, APropertyContainer.DataStreamOffset);
    Result := TdxDocHyperlinkFieldData.Create(AObjectInfo, APropertyContainer, AHyperlinkInfo);
  end
  else
    Result := nil;
end;

function TdxDocContentBuilder.GetTableUnit(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocObjectBase;
begin
  if (APropertyContainer.ParagraphInfo <> nil) and APropertyContainer.ParagraphInfo.TableTrailer then
    Exit(TdxDocTableRow.Create(AObjectInfo, APropertyContainer))
  else
    Exit(TdxDocTableCell.Create(AObjectInfo, APropertyContainer));
end;

function TdxDocContentBuilder.GetSection(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocSection;
begin
  SectionPropertiesHelper.UpdateCurrentSectionProperties(MainStreamReader, DataReader, APropertyContainer);
  Result := TdxDocSection.Create(AObjectInfo, APropertyContainer);
end;

procedure TdxDocContentBuilder.SkipUnsupportedObject(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
end;

procedure TdxDocContentBuilder.InsertFloatingObject(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
var
  AFloatingObject: TdxDocFloatingObjectBase;
begin
  AFloatingObject := GetFloatingObject(AObjectInfo, APropertyContainer);
  if AFloatingObject <> nil then
    ADestination.Add(AFloatingObject)
  else
    ADestination.Add(TdxUnsupportedObject.Create(AObjectInfo.Position, APropertyContainer));
end;

procedure TdxDocContentBuilder.InsertAutoNumberedFootnoteReference(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  ADestination.Add(Iterator.GetNoteObject(AObjectInfo, APropertyContainer));
end;

procedure TdxDocContentBuilder.InsertAnnotationReference(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  ADestination.Add(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.AnnotationReference, AObjectInfo, APropertyContainer));
end;

procedure TdxDocContentBuilder.InsertInlinePicture(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  if (APropertyContainer.CharacterInfo <> nil) and APropertyContainer.CharacterInfo.BinaryData then
  begin
    InsertFieldData(ADestination, AObjectInfo, APropertyContainer);
    Exit;
  end;

  if (APropertyContainer.CharacterInfo <> nil) and APropertyContainer.CharacterInfo.Ole2Object then
    Exit;
  InsertInlinePictureCore(ADestination, AObjectInfo, APropertyContainer);
end;

procedure TdxDocContentBuilder.InsertInlinePictureCore(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
var
  APictures: TdxDocImageCollection;
  I, ACount: Integer;
begin
  APictures := GetPictures(AObjectInfo, APropertyContainer);
  try
    ACount := APictures.Count;
    for I := 0 to ACount - 1 do
      ADestination.Add(APictures[I] as IdxDocObject);
  finally
    APictures.Free;
  end;
end;

procedure TdxDocContentBuilder.InsertFieldData(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
var
  AFieldData: TdxDocObjectBase;
begin
  AFieldData := GetFieldData(AObjectInfo, APropertyContainer);
  if AFieldData <> nil then
    ADestination.Add(AFieldData);
end;

procedure TdxDocContentBuilder.InsertFieldBegin(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  if Iterator.AdvanceField(APropertyContainer) then
    ADestination.Add(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.FieldBegin, AObjectInfo, APropertyContainer));
end;

procedure TdxDocContentBuilder.InsertFieldSeparator(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  if Iterator.AdvanceField(APropertyContainer) then
  begin
    ADestination.Add(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.FieldSeparator, AObjectInfo, APropertyContainer));
    Iterator.State.FieldIterator.SetShapeField(ADestination);
  end;
end;

procedure TdxDocContentBuilder.InsertFieldEnd(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  if Iterator.AdvanceField(APropertyContainer) then
  begin
    ADestination.Add(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.FieldEnd, AObjectInfo, APropertyContainer));
    Iterator.CheckFieldCompatibility(ADestination);
    Iterator.State.FieldIterator.ResetShapeField;
  end;
end;

procedure TdxDocContentBuilder.InsertExpectedFieldBegin(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  ADestination.Add(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.ExpectedFieldBegin, AObjectInfo, APropertyContainer));
end;

procedure TdxDocContentBuilder.InsertExpectedFieldSeparator(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  ADestination.Add(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.ExpectedFieldSeparator, AObjectInfo, APropertyContainer));
end;

procedure TdxDocContentBuilder.InsertExpectedFieldEnd(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  ADestination.Add(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.ExpectedFieldEnd, AObjectInfo, APropertyContainer));
end;

procedure TdxDocContentBuilder.InsertSpecialSymbol(ADestination: TdxDocObjectCollection; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
var
  ASpecialCharacter: string;
  ASymbolInfo: TdxDocObjectInfo;
begin
  if APropertyContainer.CharacterInfo = nil then
    Exit;
  ASpecialCharacter := APropertyContainer.CharacterInfo.Symbol;
  ASymbolInfo := TdxDocObjectInfo.Create(AObjectInfo.Position, ASpecialCharacter);
  ADestination.Add(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.TextRun, ASymbolInfo, APropertyContainer));
end;

function TdxDocContentBuilder.GetInnerBorders(APcdIndex: Integer): TdxList<TdxTextRunBorder>;
var
  ABorders: TdxList<TdxTextRunBorder>;
  AOffset, ALength, ALowIndex, AHighIndex, I: Integer;
  AComparable: IdxComparable<TdxTextRunBorder>;
begin
  ABorders := FTextRunBorders;
  Result := TdxList<TdxTextRunBorder>.Create;
  AOffset := PieceTable.GetOffset(APcdIndex);
  ALength := PieceTable.GetLength(APcdIndex);
  AComparable := TdxTextRunBorderComparable.Create(AOffset);
  TdxAlgorithms1<TdxTextRunBorder>.BinarySearch(ABorders, AComparable, ALowIndex);
  AComparable := TdxTextRunBorderComparable.Create(AOffset + ALength);
  TdxAlgorithms1<TdxTextRunBorder>.BinarySearch(ABorders, AComparable, AHighIndex);
  for I := ALowIndex to AHighIndex do
    Result.Add(ABorders[I]);
end;

function TdxDocContentBuilder.GetString(AOffset: Integer; ALength: Integer; AEncoding: TEncoding): string;
var
  ABuffer: TBytes;
begin
  MainStreamReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ABuffer := MainStreamReader.ReadBytes(ALength);
  Result := AEncoding.GetString(ABuffer, 0, Length(ABuffer));
  TdxStringHelper.Replace(Result, NoBreakHyphen, Hyphen);
end;

procedure TdxDocContentBuilder.CorrectParagraphBorderType(AParagraphBorder: TdxTextRunBorder; APcdIndex: Integer);
var
  AOffset: Integer;
  AMarkType: Char;
begin
  if (PieceTable.GetEncoding(APcdIndex) = TdxEncoding.Unicode) then
    AOffset := 2
  else
    AOffset := 1;
  MainStreamReader.BaseStream.Seek(AParagraphBorder.Offset - AOffset, TSeekOrigin.soBeginning);
  AMarkType := Chr(MainStreamReader.ReadByte);
  if (AMarkType = TdxTextCodes.SectionMark) and (AParagraphBorder.Offset <> FTextRunBorders[FTextRunBorders.Count - 1].Offset) then
  begin
    AParagraphBorder.Reason := AParagraphBorder.Reason + [TdxTextRunStartReason.SectionMark];
    AParagraphBorder.Reason := AParagraphBorder.Reason - [TdxTextRunStartReason.ParagraphMark];
  end;
  if AMarkType = TdxTextCodes.ColumnBreak then
  begin
    AParagraphBorder.Reason := AParagraphBorder.Reason + [TdxTextRunStartReason.ColumnBreak];
    AParagraphBorder.Reason := AParagraphBorder.Reason - [TdxTextRunStartReason.ParagraphMark];
  end;
  if AMarkType = TdxTextCodes.TableUnitMark then
  begin
    AParagraphBorder.Reason := AParagraphBorder.Reason + [TdxTextRunStartReason.TableUnitMark];
    AParagraphBorder.Reason := AParagraphBorder.Reason - [TdxTextRunStartReason.ParagraphMark];
  end;
end;

procedure TdxDocContentBuilder.AddToGC(AObject: TObject);
begin
  if FObjectsToDelete.IndexOf(AObject, ldFromEnd) < 0 then
    FObjectsToDelete.Add(AObject)
  else
    raise Exception.Create('Error Message');
end;

procedure TdxDocContentBuilder.RemoveFromGC(AObject: TObject);
begin
  FObjectsToDelete.Extract(AObject);
end;

procedure TdxDocContentBuilder.BeginEmbeddedContent(AContent: TdxDocObjectCollection);
begin
  Iterator.BeginEmbeddedContent(AContent);
end;

procedure TdxDocContentBuilder.EndEmbeddedContent;
begin
  Iterator.EndEmbeddedContent;
end;

function TdxDocContentBuilder.CreateImage(const AStream: TStream): TdxOfficeImageReference;
var
  AImage: TdxOfficeImage;
begin
  AImage := TdxOfficeImage.CreateImage(AStream);
  Result := CreateImage(AImage);
end;

function TdxDocContentBuilder.CreateImage(AImage: TdxOfficeImage): TdxOfficeImageReference;
begin
  Result := FDocumentModel.CreateImage(AImage);
  if Result.Image <> AImage then
    AImage.Free;
end;

function TdxDocContentBuilder.CreateMetafile(AStream: TMemoryStream; AMapMode: TdxMapMode; APictureWidth, APictureHeight: Integer): TdxOfficeImageReference;
var
  AImage: TdxOfficeImage;
begin
  AImage := TdxMetafileHelper.CreateMetafile(AStream, AMapMode, APictureWidth, APictureHeight);
  Result := CreateImage(AImage);
end;

end.
