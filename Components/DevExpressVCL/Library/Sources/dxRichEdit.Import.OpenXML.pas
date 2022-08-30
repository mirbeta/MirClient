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

unit dxRichEdit.Import.OpenXML;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.NativeApi,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.Import.Core,
  dxRichEdit.Import.PackageUtils,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationSection,
  dxRichEdit.Import.OpenXML.DestinationStyles,
  dxRichEdit.Import.OpenXML.DestinationParagraph,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Import.Formats;

type
  TdxOpenXmlImporter = class;

  { TdxOpenXmlRelation }

  TdxOpenXmlRelation = class
  strict private
    FId: string;
    FTarget: string;
    FType: string;
    FTargetMode: string;
  public
    constructor Create(const AId, ATarget, AType: string; const ATargetMode: string = ''); overload;
    constructor Create; overload;

    property Id: string read FId write FId;
    property Target: string read FTarget write FTarget;
    property &Type: string read FType write FType;
    property TargetMode: string read FTargetMode write FTargetMode;
  end;

  { TdxOpenXmlRelationCollection }

  TdxOpenXmlRelationCollection = class(TdxObjectList<TdxOpenXmlRelation>)
  public
    function LookupRelationById(const AId: string): TdxOpenXmlRelation;
    function LookupRelationByType(const AType: string): TdxOpenXmlRelation;
    function GenerateId: string;
    function LookupRelationByTargetAndType(const ATarget, AType: string): TdxOpenXmlRelation;
  end;

  { TdxRelationshipsDestination }

  TdxRelationshipsDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
  strict private
    FRelations: TdxOpenXmlRelationCollection;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxRelationshipsDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnRelation(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxWordProcessingMLBaseImporter; ARelations: TdxOpenXmlRelationCollection);
  end;

  { TdxRelationDestination }

  TdxRelationDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
  strict private
    FRelations: TdxOpenXmlRelationCollection;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ARelations: TdxOpenXmlRelationCollection);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxAltChunkInfo }

  TdxAltChunkInfo = class
  strict private
    FRelationId: string;
    FPieceTable: TdxPieceTable;
    FLogPosition: TdxDocumentLogPosition;
  public
    constructor Create(const ARelationId: string; ALogPosition: TdxDocumentLogPosition; APieceTable: TdxPieceTable);

    property RelationId: string read FRelationId;
    property LogPosition: TdxDocumentLogPosition read FLogPosition;
    property PieceTable: TdxPieceTable read FPieceTable;
  end;

  { TdxAltChunkPropertiesDestination }

  TdxAltChunkPropertiesDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
  strict private
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxAltChunkDestination }

  TdxAltChunkDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
  strict private
    FRelId: string;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxAltChunkDestination; static;
    function CreateAltChunkPropertiesDestination: TdxAltChunkPropertiesDestination; virtual;
    class function OnAltChunkProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxSeparatorDestination }

  TdxSeparatorDestination = class(TdxElementDestination)
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxOpenXmlImporter }

  TdxOpenXmlImporter = class(TdxWordProcessingMLBaseImporter{, IdxDisposable, IdxOpenXmlImporter})
  strict private
    FPackageFiles: TdxPackageFileCollection;
    FDocumentRelationsStack: TdxObjectStack<TdxOpenXmlRelationCollection>;
    FDocumentRootFolder: string;
    FPackageFileStreamsStack: TdxObjectStack<TdxPackageFileStreams>;
    FPackageImagesStack: TdxObjectStack<TdxNamedObjectDictionary<TdxOfficeImageReference>>;
    FAltChunkInfos: TdxObjectList<TdxAltChunkInfo>;
    FFootNotes: TdxNamedOrdinalDictionary<TdxFootNote>;
    FEndNotes: TdxNamedOrdinalDictionary<TdxEndNote>;
    FContentStreams: TdxFastObjectList;
    function GetOptions: TdxOpenXmlDocumentImporterOptions;
    function GetDocumentRelations: TdxOpenXmlRelationCollection;
    function GetPackageFileStreams: TdxPackageFileStreams;
    function GetPackageImages: TdxNamedObjectDictionary<TdxOfficeImageReference>;
  protected
    FUniqueUriBasedImages: TdxNamedObjectDictionary<TdxUriOfficeImage>;
    function GetWordProcessingNamespaceConst: string;  override;
    function GetOfficeNamespace: string; override;
    procedure DisposePackageFiles; virtual;
    procedure OpenPackage(AStream: TStream); virtual;
    function GetPackageFiles(AStream: TStream): TdxPackageFileCollection; virtual;
    function GetPackageFileStream(const AFileName: string): TStream; virtual;
    function GetPackageFile(const AFileName: string): TdxPackageFile; virtual;
    function GetPackageFileXmlReader(const AFileName: string): TdxXmlReader; virtual;
    function ImportRelationsCore(AReader: TdxXmlReader): TdxOpenXmlRelationCollection; virtual;
    function LookupRelationTargetByType(ARelations: TdxOpenXmlRelationCollection; const AType: string; const ARootFolder: string; const ADefaultFileName: string): string; virtual;
    function CreateUriBasedRichEditImageCore(const AUri: string; APixelTargetWidth: Integer; APixelTargetHeight: Integer): TdxUriOfficeImage;
    function CreatePackageFileStreamByRelationId(const ARelationId: string; const ARootFolder: string): TStream; virtual;
    function CalculateRelationTargetCore(ARelation: TdxOpenXmlRelation; const ARootFolder: string; const ADefaultFileName: string): string; virtual;
    procedure ImportSettings; virtual;
    procedure ImportSettingsCore(AReader: TdxXmlReader); virtual;
    procedure ImportWebSettings; virtual;
    procedure ImportWebSettingsCore(AReader: TdxXmlReader); virtual;
    procedure ImportStyles; virtual;
    procedure ImportStylesCore(AReader: TdxXmlReader); virtual;
    procedure ImportNumbering; virtual;
    procedure ImportNumberingCore(AReader: TdxXmlReader); virtual;
    procedure ImportFootNotes; virtual;
    procedure ImportEndNotes; virtual;
    procedure ImportFootNotesCore(AReader: TdxXmlReader); virtual;
    procedure ImportEndNotesCore(AReader: TdxXmlReader); virtual;
    procedure ImportComments; virtual;
    procedure BeforeImportComment;
    procedure AfterImportComment;
    procedure ImportCommentsCore(AReader: TdxXmlReader); virtual;
    procedure BeforeImportMainDocument; override;
    function CreateMainDocumentDestination: TdxDestination; override;
    procedure AfterImportMainDocument; override;
    procedure InsertAltChunks; virtual;
    procedure InsertAltChunk(AChunkInfo: TdxAltChunkInfo); virtual;
    procedure AddAltChunkInfo(AAltChunkInfo: TdxAltChunkInfo); virtual;
    procedure ImportCore(AStream: TStream); override;
    procedure ImportFromStream(AStream: TStream); virtual;

    property ContentStreams: TdxFastObjectList read FContentStreams;
    property PackageFileStreams: TdxPackageFileStreams read GetPackageFileStreams;
    property PackageImages: TdxNamedObjectDictionary<TdxOfficeImageReference> read GetPackageImages;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions); override;
    destructor Destroy; override;
    procedure CheckIsEncryptedFile(AStream: TStream);
    procedure CheckVersion; virtual;
    function ValidateStream(AStream: TStream): Boolean;
    class procedure ThrowInvalidFile; override;
    function ConvertToBool(const AValue: string): Boolean; override;

    function GetWordProcessingMLValue(AValue: TdxWordProcessingMLValue): string; override;
    function CreateOpenXmlSectionTextDirectionDestination: TdxSectionTextDirectionDestination; override;
    function CreateWordMLSectionTextDirectionDestination: TdxSectionTextDirectionDestination; override;
    function CreateStyleParagraphPropertiesDestination(AStyleDestination: TdxStyleDestinationBase; AParagraphFormatting:
      TdxParagraphFormattingBase; ATabs: TdxTabFormattingInfo): TdxParagraphPropertiesBaseDestination; override;

    function RegisterFootNote(ANote: TdxFootNote; const AId: string): Integer; override;
    function RegisterEndNote(ANote: TdxEndNote; const AId: string): Integer; override;
    function LookupImageByRelationId(const ARelationId: string; const ARootFolder: string): TdxOfficeImageReference; virtual;
    function LookupExternalImageByRelationId(const ARelationId: string; const ARootFolder: string): TdxUriOfficeImage; virtual;
    function LookupRelationTargetById(ARelations: TdxOpenXmlRelationCollection; const AId: string; const ARootFolder: string; const ADefaultFileName: string): string; virtual;
    function LookupMetafileByRelationId(const ARelationId: string; const ARootFolder: string; APictureWidth: Integer; APictureHeight: Integer): TdxOfficeImageReference;
    function LookupPackageFileStreamByRelationId(const ARelationId: string; const ARootFolder: string; ACacheStream: Boolean): TStream; virtual;
    function ImportRelations(const AFileName: string): TdxOpenXmlRelationCollection; virtual;

    property Options: TdxOpenXmlDocumentImporterOptions read GetOptions;
    property PackageFiles: TdxPackageFileCollection read FPackageFiles;
    property DocumentRelationsStack: TdxObjectStack<TdxOpenXmlRelationCollection> read FDocumentRelationsStack;
    property DocumentRelations: TdxOpenXmlRelationCollection read GetDocumentRelations;
    property DocumentRootFolder: string read FDocumentRootFolder write FDocumentRootFolder;
    property FootNotes: TdxNamedOrdinalDictionary<TdxFootNote> read FFootNotes;
    property EndNotes: TdxNamedOrdinalDictionary<TdxEndNote> read FEndNotes;
    property PackageFileStreamsStack: TdxObjectStack<TdxPackageFileStreams> read FPackageFileStreamsStack;
    property PackageImagesStack: TdxObjectStack<TdxNamedObjectDictionary<TdxOfficeImageReference>> read FPackageImagesStack;
  end;

  { TdxImportOpenXmlFormat }

  TdxImportOpenXmlFormat = class(TdxImportFileFormat)
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; override;
    function GetImporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxImporterOptions): TdxDocumentModelImporter; override;
    function GetDocumentImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>; override;
  end;

implementation

uses
  IOUtils, Contnrs,
  dxZIPUtils, dxOLEDocument, dxOLECryptoContainer,
  dxRichEdit.Strs,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Export.OpenXML,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.OpenXML.DestinationDocument,
  dxRichEdit.Import.OpenXML.DestinationNumbering,
  dxRichEdit.Import.OpenXML.DestinationSettings,
  dxRichEdit.Import.OpenXML.DestinationWebSettings,
  dxRichEdit.Import.OpenXML.DestinationFootNote,
  dxRichEdit.Import.OpenXML.DestinationEndNote,
  dxRichEdit.Import.OpenXML.DestinationComment,
  dxRichEdit.Import.OpenXML.DocumentImporter;

{ TdxOpenXmlRelation }

constructor TdxOpenXmlRelation.Create(const AId, ATarget, AType: string; const ATargetMode: string = '');
begin
  FId := AId;
  FTarget := ATarget;
  FType := AType;
  FTargetMode := ATargetMode;
end;

constructor TdxOpenXmlRelation.Create;
begin
  Create('', '', '');
end;

{ TdxOpenXmlRelationCollection }

function TdxOpenXmlRelationCollection.LookupRelationById(const AId: string): TdxOpenXmlRelation;
var
  I: Integer;
begin
  Result := nil;
  if AId = '' then
    Exit;

  for I := 0 to Count - 1 do
    if Items[I].Id = AId then
      Exit(Items[I]);
end;

function TdxOpenXmlRelationCollection.LookupRelationByType(const AType: string): TdxOpenXmlRelation;
var
  I: Integer;
begin
  Result := nil;
  if AType = '' then
    Exit;

  for I := 0 to Count - 1 do
    if Items[I].&Type = AType then
      Exit(Items[I]);
end;

function TdxOpenXmlRelationCollection.GenerateId: string;
begin
  Result := Format('rId%d', [Count + 1]);
end;

function TdxOpenXmlRelationCollection.LookupRelationByTargetAndType(const ATarget, AType: string): TdxOpenXmlRelation;
var
  I: Integer;
begin
  Result := nil;
  if (ATarget = '') or (AType = '') then
    Exit;

  for I := 0 to Count - 1 do
    if (Items[I].Target = ATarget) and (Items[I].&Type = AType) then
      Exit(Items[I]);
end;

{ TdxRelationshipsDestination }

constructor TdxRelationshipsDestination.Create(AImporter: TdxWordProcessingMLBaseImporter; ARelations: TdxOpenXmlRelationCollection);
begin
  inherited Create(AImporter);
  Assert(ARelations <> nil);
  FRelations := ARelations;
end;

class constructor TdxRelationshipsDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxRelationshipsDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxRelationshipsDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('Relationship', OnRelation);
end;

class function TdxRelationshipsDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxRelationshipsDestination;
begin
  Result := TdxRelationshipsDestination(AImporter.PeekDestination);
end;

function TdxRelationshipsDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxRelationshipsDestination.OnRelation(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRelationDestination.Create(AImporter, GetThis(AImporter).FRelations);
end;

{ TdxRelationDestination }

constructor TdxRelationDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ARelations: TdxOpenXmlRelationCollection);
begin
  inherited Create(AImporter);
  Assert(ARelations <> nil);
  FRelations := ARelations;
end;

class constructor TdxRelationDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxRelationDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxRelationDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
end;

function TdxRelationDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxRelationDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ARelation: TdxOpenXmlRelation;
  AId, AType, ATarget: string;
begin
  AId := AReader.GetAttribute('Id');
  if AId = '' then
    Exit;
  AType := AReader.GetAttribute('Type');
  if AType = '' then
    Exit;
  ATarget := AReader.GetAttribute('Target');
  if ATarget = '' then
    Exit;

  ARelation := TdxOpenXmlRelation.Create;
  ARelation.Id := AId;
  ARelation.&Type := AType;
  ARelation.Target := ATarget;
  ARelation.TargetMode := AReader.GetAttribute('TargetMode');

  FRelations.Add(ARelation);
end;

{ TdxAltChunkInfo }

constructor TdxAltChunkInfo.Create(const ARelationId: string; ALogPosition: TdxDocumentLogPosition; APieceTable: TdxPieceTable);
begin
  Assert(APieceTable <> nil);
  FRelationId := ARelationId;
  FLogPosition := ALogPosition;
  FPieceTable := APieceTable;
end;

{ TdxAltChunkPropertiesDestination }

class constructor TdxAltChunkPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxAltChunkPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxAltChunkPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
end;

function TdxAltChunkPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxAltChunkPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
end;

procedure TdxAltChunkPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
end;

{ TdxAltChunkDestination }

class constructor TdxAltChunkDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class function TdxAltChunkDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('altChunkPr', OnAltChunkProperties);
end;

class destructor TdxAltChunkDestination.Finalize;
begin
  FHandlerTable.Free;
end;

function TdxAltChunkDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxAltChunkDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxAltChunkDestination;
begin
  Result := TdxAltChunkDestination(AImporter.PeekDestination);
end;

function TdxAltChunkDestination.CreateAltChunkPropertiesDestination: TdxAltChunkPropertiesDestination;
begin
  Result := TdxAltChunkPropertiesDestination.Create(Importer);
end;

class function TdxAltChunkDestination.OnAltChunkProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := GetThis(AImporter).CreateAltChunkPropertiesDestination;
end;

procedure TdxAltChunkDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AId: string;
begin
  AId := AReader.GetAttribute('id', TdxOpenXmlExporter.RelsNamespace);
  if AId <> '' then
    FRelId := AId;
end;

procedure TdxAltChunkDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AOpenXmlImporter: TdxOpenXmlImporter;
begin
  if FRelId = '' then
    Exit;
  AOpenXmlImporter := TdxOpenXmlImporter(Importer);
  AOpenXmlImporter.AddAltChunkInfo(TdxAltChunkInfo.Create(FRelId, AOpenXmlImporter.Position.LogPosition,
    TdxPieceTable(AOpenXmlImporter.Position.PieceTable)));
end;

{ TdxSeparatorDestination }

function TdxSeparatorDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := ElementHandlerTable.Empty;
end;

procedure TdxSeparatorDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Importer.PieceTable.InsertSeparatorTextRunCore(Importer.Position);
end;


{ TdxOpenXmlImporter }

constructor TdxOpenXmlImporter.Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FUniqueUriBasedImages := TdxNamedObjectDictionary<TdxUriOfficeImage>.Create(False);
  FContentStreams := TdxFastObjectList.Create;
end;

destructor TdxOpenXmlImporter.Destroy;
begin
  DisposePackageFiles;
  FUniqueUriBasedImages.Free;

  FreeAndNil(FDocumentRelationsStack);
  FreeAndNil(FPackageFileStreamsStack);
  FreeAndNil(FPackageImagesStack);
  FreeAndNil(FAltChunkInfos);
  FreeAndNil(FFootNotes);
  FreeAndNil(FEndNotes);

  FreeAndNil(FContentStreams);

  inherited Destroy;
end;

function TdxOpenXmlImporter.GetOptions: TdxOpenXmlDocumentImporterOptions;
begin
  Result := TdxOpenXmlDocumentImporterOptions(InnerOptions);
end;

function TdxOpenXmlImporter.GetDocumentRelations: TdxOpenXmlRelationCollection;
begin
  Result := FDocumentRelationsStack.Peek;
end;

function TdxOpenXmlImporter.GetPackageFileStreams: TdxPackageFileStreams;
begin
  Result := FPackageFileStreamsStack.Peek;
end;

function TdxOpenXmlImporter.GetPackageImages: TdxNamedObjectDictionary<TdxOfficeImageReference>;
begin
  Result := FPackageImagesStack.Peek;
end;

function TdxOpenXmlImporter.GetWordProcessingNamespaceConst: string;
begin
  Result := TdxOpenXmlExporter.WordProcessingNamespace;
end;

function TdxOpenXmlImporter.GetOfficeNamespace: string;
begin
  Result := TdxOpenXmlExporter.OfficeNamespaceConst;
end;

procedure TdxOpenXmlImporter.ImportCore(AStream: TStream);
var
  APassword: string;
begin
  APassword := DocumentModel.EncryptionProperties.Password;
  if (AStream <> nil) and TdxOLECryptoContainer.Decrypt(
    AStream,
    AStream,
    APassword,
    DocumentModel.QueryEncryptionPassword,
    DocumentModel.EncryptionProperties.TryCount) then
  begin
    try
      ImportFromStream(AStream);
      DocumentModel.EncryptionProperties.Password := APassword;
    finally
      AStream.Free;
    end;
  end
  else
    ImportFromStream(AStream);
end;

procedure TdxOpenXmlImporter.ImportFromStream(AStream: TStream);
var
  ARootRelations: TdxOpenXmlRelationCollection;
  AFileName: string;
  AReader: TdxXmlReader;
begin
  OpenPackage(AStream);

  ARootRelations := ImportRelations('_rels/.rels');
  try
    AFileName := LookupRelationTargetByType(ARootRelations, TdxOpenXmlExporter.OfficeDocumentType, '', 'document.xml');
  finally
    ARootRelations.Free;
  end;
  AReader := GetPackageFileXmlReader(AFileName);
  if AReader = nil then
  begin
    CheckIsEncryptedFile(AStream);
    TdxRichEditExceptions.ThrowArgumentException('stream', AStream);
  end
  else
  try
    if not ReadToRootElement(AReader, 'document') then
    begin
      TdxRichEditExceptions.ThrowArgumentException('stream', AStream);
      Exit;
    end;

    FDocumentRootFolder := TPath.GetDirectoryName(AFileName);
    FDocumentRelationsStack := TdxObjectStack<TdxOpenXmlRelationCollection>.Create(True);
    FPackageFileStreamsStack := TdxObjectStack<TdxPackageFileStreams>.Create(True);
    FPackageImagesStack := TdxObjectStack<TdxNamedObjectDictionary<TdxOfficeImageReference>>.Create(True);

    FDocumentRelationsStack.Push(ImportRelations(FDocumentRootFolder + '/_rels/' + TPath.GetFileName(AFileName) + '.rels'));
    FPackageFileStreamsStack.Push(TdxPackageFileStreams.Create);
    FPackageImagesStack.Push(TdxNamedObjectDictionary<TdxOfficeImageReference>.Create(True));

    ImportMainDocument(AReader, AStream);
  finally
    AReader.Free;
  end;
end;

procedure TdxOpenXmlImporter.CheckIsEncryptedFile(AStream: TStream);
var
  ADocument: TdxOLEDocument;
  AEncryptionInfo: TStream;
begin
  if not ValidateStream(AStream) then
    Exit;

  ADocument := TdxOLEDocument.Create(AStream, dmReading);
  try
    AEncryptionInfo := ADocument.StreamByName('EncryptionInfo');
    if AEncryptionInfo <> nil then
      TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditMsg_EncryptedFile));
  finally
    ADocument.Free;
  end;
end;

function TdxOpenXmlImporter.ValidateStream(AStream: TStream): Boolean;
const
  MAGIC_NUMBER: UInt64 = 16220472316735377360;
var
  AMagicNumberBuffer: UInt64;
begin
  if AStream = nil then
    Exit(False);
  AStream.Position := 0;
  AStream.Read(AMagicNumberBuffer, 8);
  Result := MAGIC_NUMBER = AMagicNumberBuffer;
  AStream.Position := 0;
end;

procedure TdxOpenXmlImporter.DisposePackageFiles;
begin
  FreeAndNil(FPackageFiles);
end;

procedure TdxOpenXmlImporter.OpenPackage(AStream: TStream);
begin
  FPackageFiles := GetPackageFiles(AStream);
end;

function TdxOpenXmlImporter.GetPackageFiles(AStream: TStream): TdxPackageFileCollection;
var
  ADataStream: TMemoryStream;
  AItem: TdxZIPFileEntry;
  AReader: TdxZIPStreamReader;
  AName: string;
  I: Integer;
begin
  DisposePackageFiles;
  Result := TdxPackageFileCollection.Create;
  AReader := TdxZIPStreamReader.Create(AStream);
  try
    try
      for I := 0 to AReader.Files.Count - 1 do
      begin
        AItem := AReader.Files[I];

        ADataStream := TMemoryStream.Create;
        try
          AReader.Extract(AItem, ADataStream);
          ADataStream.Position := 0;
        except
          FreeAndNil(ADataStream);
          raise;
        end;

        AName := StringReplace(string(AItem.Name), '\', '/', [rfReplaceAll]);
        Result.Add(TdxPackageFile.Create(AName, ADataStream, ADataStream.Size));
      end;
    except
      DisposePackageFiles;
      raise;
    end;
  finally
    AReader.Free;
  end;
end;

function TdxOpenXmlImporter.GetPackageFileStream(const AFileName: string): TStream;
var
  AFile: TdxPackageFile;
begin
  AFile := GetPackageFile(AFileName);
  if AFile = nil then
    Result := nil
  else
    Result := AFile.Stream;
end;

function TdxOpenXmlImporter.GetPackageFile(const AFileName: string): TdxPackageFile;
var
  ACount, I: Integer;
  AFile: TdxPackageFile;
  AName: string;
begin
  if PackageFiles = nil then
    Exit(nil);

  AName := StringReplace(AFileName, '\', '/', [rfReplaceAll]);
  if (Length(AName) > 0) and (AName[1] = '/') then
    Delete(AName, 1, 1);
  ACount := PackageFiles.Count;
  for I := 0 to ACount - 1 do
  begin
    AFile := PackageFiles[I];
    if SameText(AFile.FileName, AName) then
      Exit(AFile);
  end;
  Result := nil;
end;

function TdxOpenXmlImporter.GetPackageFileXmlReader(const AFileName: string): TdxXmlReader;
var
  AStream: TStream;
begin
  AStream := GetPackageFileStream(AFileName);
  if AStream = nil then
    Exit(nil);

  Result := CreateXmlReader(AStream);
end;

function TdxOpenXmlImporter.ImportRelations(const AFileName: string): TdxOpenXmlRelationCollection;
var
  AReader: TdxXmlReader;
begin
  AReader := GetPackageFileXmlReader(AFileName);
  if AReader <> nil then
  try
    Result := ImportRelationsCore(AReader)
  finally
    AReader.Free;
  end
  else
    Result := TdxOpenXmlRelationCollection.Create;
end;

function TdxOpenXmlImporter.ImportRelationsCore(AReader: TdxXmlReader): TdxOpenXmlRelationCollection;
begin
  Result := TdxOpenXmlRelationCollection.Create;
  if not ReadToRootElement(AReader, 'Relationships', TdxOpenXmlExporter.PackageRelsNamespace) then
    Exit(Result);
  ImportContent(AReader, TdxRelationshipsDestination.Create(Self, Result));
end;

function TdxOpenXmlImporter.LookupRelationTargetByType(ARelations: TdxOpenXmlRelationCollection;
  const AType: string; const ARootFolder: string; const ADefaultFileName: string): string;
var
  ARelation: TdxOpenXmlRelation;
begin
  ARelation := ARelations.LookupRelationByType(AType);
  Result := CalculateRelationTargetCore(ARelation, ARootFolder, ADefaultFileName);
end;

function TdxOpenXmlImporter.LookupRelationTargetById(ARelations: TdxOpenXmlRelationCollection;
  const AId: string; const ARootFolder: string; const ADefaultFileName: string): string;
var
  ARelation: TdxOpenXmlRelation;
begin
  ARelation := ARelations.LookupRelationById(AId);
  Result := CalculateRelationTargetCore(ARelation, ARootFolder, ADefaultFileName);
end;

function TdxOpenXmlImporter.LookupImageByRelationId(const ARelationId: string; const ARootFolder: string): TdxOfficeImageReference;
var
  AStream: TStream;
begin
  if PackageImages.TryGetValue(ARelationId, Result) then
    Exit(Result);

  Result := nil;
  AStream := LookupPackageFileStreamByRelationId(ARelationId, ARootFolder, False);
  if AStream <> nil then
  try
    Result := DocumentModel.CreateImage(AStream);
    if Result <> nil then
      PackageImages.Add(ARelationId, Result);
  except
    FreeAndNil(Result);
  end;
end;

function TdxOpenXmlImporter.LookupMetafileByRelationId(const ARelationId: string; const ARootFolder: string; APictureWidth: Integer; APictureHeight: Integer): TdxOfficeImageReference;
var
  AStream: TStream;
  AInfo: TdxRtfImageInfo;
  APictureStream: TBytesStream;
begin
  if PackageImages.TryGetValue(ARelationId, Result) then
    Exit(Result);

  AStream := LookupPackageFileStreamByRelationId(ARelationId, ARootFolder, False);
  if AStream = nil then
    Exit(nil);

  AInfo := TdxRtfImageInfo.Create(DocumentModel);
  try
    try
      APictureStream := TBytesStream.Create;
      try
        APictureStream.CopyFrom(AStream, -1);
        AInfo.LoadMetafileFromStream(APictureStream, TdxMapMode.Anisotropic, APictureWidth, APictureHeight);
        if AInfo.RtfImage <> nil then
        begin
          Result := DocumentModel.CreateImage(AInfo.RtfImage.Image);
          if AInfo.RtfImage.Image <> Result.Image then
            AInfo.RtfImage.Image.Free;
          PackageImages.Add(ARelationId, Result);
          Exit;
        end;
      finally
        APictureStream.Free;
      end;
    except
    end;
  finally
    AInfo.Free;
  end;
  Result := nil;
end;

function TdxOpenXmlImporter.LookupExternalImageByRelationId(const ARelationId: string; const ARootFolder: string): TdxUriOfficeImage;
var
  ARelation: TdxOpenXmlRelation;
  AResult: TdxUriOfficeImage;
begin
  ARelation := DocumentRelations.LookupRelationById(ARelationId);
  if SameText(ARelation.TargetMode, 'external') then
    Exit(nil);

  if FUniqueUriBasedImages.TryGetValue(ARelation.Target, AResult) then
    Result := TdxUriOfficeImage(AResult.Clone)
  else
    Result := CreateUriBasedRichEditImageCore(ARelation.Target, 0, 0);
end;

function TdxOpenXmlImporter.CreateUriBasedRichEditImageCore(const AUri: string;
  APixelTargetWidth: Integer; APixelTargetHeight: Integer): TdxUriOfficeImage;
var
  AUriList: TArray<string>;
begin
  AUriList := TArray<string>.Create(AUri);
  Result := TdxUriOfficeImage.Create(DocumentModel.ImageCache, DocumentModel, AUriList,
    APixelTargetWidth, APixelTargetHeight, False);
  FUniqueUriBasedImages.Add(AUri, Result);
end;

function TdxOpenXmlImporter.LookupPackageFileStreamByRelationId(const ARelationId: string;
  const ARootFolder: string; ACacheStream: Boolean): TStream;
begin
  if PackageFileStreams.TryGetValue(ARelationId, Result) then
  begin
    Result.Position := 0;
    Exit;
  end;

  Result := CreatePackageFileStreamByRelationId(ARelationId, ARootFolder);
  if Result = nil then
    Exit;

  Result.Position := 0;
  PackageFileStreams.Add(ARelationId, Result);
end;

function TdxOpenXmlImporter.CreatePackageFileStreamByRelationId(const ARelationId: string;
  const ARootFolder: string): TStream;
var
  AFileName: string;
  AFile: TdxPackageFile;
begin
  AFileName := LookupRelationTargetById(DocumentRelations, ARelationId, ARootFolder, '');
  AFile := GetPackageFile(AFileName);
  if AFile = nil then
    Exit(nil);

  Result := AFile.GetMemoryStream(ContentStreams);
end;

function TdxOpenXmlImporter.CalculateRelationTargetCore(ARelation: TdxOpenXmlRelation; const ARootFolder: string;
  const ADefaultFileName: string): string;
begin
  if ARelation = nil then
  begin
    if ARootFolder = '' then
      Exit(ADefaultFileName)
    else
      Exit(ARootFolder + '/' + ADefaultFileName);
  end;

  if (Length(ARelation.Target) > 0) and (ARelation.Target[1] = '/') then
    Result := ARelation.Target
  else
  begin
    if ARootFolder = '' then
      Result := ARelation.Target
    else
      Result := ARootFolder + '/' + ARelation.Target;
  end;
end;

procedure TdxOpenXmlImporter.ImportSettings;
var
  AFileName: string;
  AReader: TdxXmlReader;
begin
  AFileName := LookupRelationTargetByType(DocumentRelations, TdxOpenXmlExporter.OfficeDocumentSettings,
    DocumentRootFolder, 'settings.xml');
  AReader := GetPackageFileXmlReader(AFileName);
  if AReader <> nil then
  try
    ImportSettingsCore(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxOpenXmlImporter.ImportSettingsCore(AReader: TdxXmlReader);
begin
  if not ReadToRootElement(AReader, 'settings') then
    Exit;
  ImportContent(AReader, TdxDocumentSettingsDestination.Create(Self));
end;

procedure TdxOpenXmlImporter.ImportWebSettings;
var
  AFileName: string;
  AReader: TdxXmlReader;
begin
  AFileName := LookupRelationTargetByType(DocumentRelations, TdxOpenXmlExporter.OfficeWebSettingsType,
    DocumentRootFolder, 'webSettings.xml');
  AReader := GetPackageFileXmlReader(AFileName);
  if AReader <> nil then
  try
    ImportWebSettingsCore(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxOpenXmlImporter.ImportWebSettingsCore(AReader: TdxXmlReader);
begin
  if not ReadToRootElement(AReader, 'webSettings') then
    Exit;
  ImportContent(AReader, TdxDocumentWebSettingsDestination.Create(Self));
end;

procedure TdxOpenXmlImporter.ImportStyles;
var
  AFileName: string;
  AReader: TdxXmlReader;
begin
  AFileName := LookupRelationTargetByType(DocumentRelations, TdxOpenXmlExporter.OfficeStylesType,
    DocumentRootFolder, 'styles.xml');
  AReader := GetPackageFileXmlReader(AFileName);
  if AReader <> nil then
  try
    ImportStylesCore(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxOpenXmlImporter.ImportStylesCore(AReader: TdxXmlReader);
begin
  if not ReadToRootElement(AReader, 'styles') then
    Exit;
  ImportContent(AReader, TdxStylesDestination.Create(Self));

  CreateStylesHierarchy;
  LinkStyles;
end;

procedure TdxOpenXmlImporter.ImportNumbering;
var
  AFileName: string;
  AReader: TdxXmlReader;
begin
  AFileName := LookupRelationTargetByType(DocumentRelations, TdxOpenXmlExporter.OfficeNumberingType,
    DocumentRootFolder, 'numbering.xml');
  AReader := GetPackageFileXmlReader(AFileName);
  if AReader <> nil then
  try
    ImportNumberingCore(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxOpenXmlImporter.ImportNumberingCore(AReader: TdxXmlReader);
begin
  if not ReadToRootElement(AReader, 'numbering') then
    Exit;
  BeginCollectGarbage;
  try
    ImportContent(AReader, TdxNumberingsDestination.Create(Self));
    CreateNumberingLists;
  finally
    EndCollectGarbage;
  end;
end;

procedure TdxOpenXmlImporter.ImportFootNotes;
var
  AFileName: string;
  AReader: TdxXmlReader;
begin
  if not DocumentModel.DocumentCapabilities.FootNotesAllowed then
    Exit;

  AFileName := LookupRelationTargetByType(DocumentRelations, TdxOpenXmlExporter.OfficeFootNoteType, DocumentRootFolder,
    'footnotes.xml');
  AReader := GetPackageFileXmlReader(AFileName);
  if AReader <> nil then
  try
    ImportFootNotesCore(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxOpenXmlImporter.ImportEndNotes;
var
  AFileName: string;
  AReader: TdxXmlReader;
begin
  if not DocumentModel.DocumentCapabilities.EndNotesAllowed then
    Exit;

  AFileName := LookupRelationTargetByType(DocumentRelations, TdxOpenXmlExporter.OfficeEndNoteType, DocumentRootFolder,
    'endnotes.xml');
  AReader := GetPackageFileXmlReader(AFileName);
  if AReader <> nil then
  try
    ImportEndNotesCore(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxOpenXmlImporter.ImportFootNotesCore(AReader: TdxXmlReader);
begin
  if not ReadToRootElement(AReader, 'footnotes') then
    Exit;
  ImportContent(AReader, TdxFootNotesDestination.Create(Self));
end;

procedure TdxOpenXmlImporter.ImportEndNotesCore(AReader: TdxXmlReader);
begin
  if not ReadToRootElement(AReader, 'endnotes') then
    Exit;
  ImportContent(AReader, TdxEndNotesDestination.Create(Self));
end;

procedure TdxOpenXmlImporter.ImportComments;
var
  AFileName: string;
  AReader: TdxXmlReader;
begin
  AFileName := LookupRelationTargetByType(DocumentRelations, TdxOpenXmlExporter.OfficeCommentType, DocumentRootFolder,
    'comments.xml');
  AReader := GetPackageFileXmlReader(AFileName);
  if AReader <> nil then
  try
    BeforeImportComment;
    ImportCommentsCore(AReader);
    AfterImportComment;
  finally
    AReader.Free;
  end;
end;

procedure TdxOpenXmlImporter.BeforeImportComment;
begin
  DocumentRelationsStack.Push(ImportRelations(DocumentRootFolder + '/_rels/' + 'comments.xml' + '.rels'));
  PackageFileStreamsStack.Push(TdxPackageFileStreams.Create);
  PackageImagesStack.Push(TdxNamedObjectDictionary<TdxOfficeImageReference>.Create(True));
end;

procedure TdxOpenXmlImporter.AfterImportComment;
begin
  PackageFileStreamsStack.Pop;
  DocumentRelationsStack.Pop;
  PackageImagesStack.Pop;
end;

procedure TdxOpenXmlImporter.ImportCommentsCore(AReader: TdxXmlReader);
begin
  if not ReadToRootElement(AReader, 'comments') then
    Exit;
  ImportContent(AReader, TdxCommentsDestination.Create(Self));
end;

procedure TdxOpenXmlImporter.BeforeImportMainDocument;
begin
  FAltChunkInfos := TdxObjectList<TdxAltChunkInfo>.Create;
  FFootNotes := TdxNamedOrdinalDictionary<TdxFootNote>.Create;
  FEndNotes := TdxNamedOrdinalDictionary<TdxEndNote>.Create;
  ImportSettings;
  ImportWebSettings;
  ImportStyles;
  ImportNumbering;
  ImportFootNotes;
  ImportEndNotes;
  ImportComments;
  LinkParagraphStylesWithNumberingLists;
  LinkNumberingListStyles;
  inherited BeforeImportMainDocument;
end;

function TdxOpenXmlImporter.CreateMainDocumentDestination: TdxDestination;
begin
  Result := TdxDocumentDestination.Create(Self);
end;

procedure TdxOpenXmlImporter.AfterImportMainDocument;
begin
  inherited AfterImportMainDocument;
  InsertAltChunks;
  DocumentModel.NormalizeZOrder;
end;

procedure TdxOpenXmlImporter.InsertAltChunks;
var
  I, ACount: Integer;
begin
  ACount := FAltChunkInfos.Count;
  for I := ACount - 1 downto 0 do
    InsertAltChunk(FAltChunkInfos[I]);
end;

procedure TdxOpenXmlImporter.InsertAltChunk(AChunkInfo: TdxAltChunkInfo);
var
  ARelId, ARelationTarget: string;
  AStream: TStream;
  ATargetPieceTable: TdxPieceTable;
  AAltChunkDocumentModel: TdxDocumentModel;
  ADocumentFormat: TdxRichEditDocumentFormat;
  AInvalidFormat: Boolean;
  AInitialPosition: Int64;
begin
  ARelId := AChunkInfo.RelationId;
  ARelationTarget := LookupRelationTargetById(DocumentRelations, ARelId, DocumentRootFolder, '');
  if ARelationTarget = '' then
    Exit;

  AStream := LookupPackageFileStreamByRelationId(ARelId, DocumentRootFolder, True);
  if AStream = nil then
    Exit;

  ATargetPieceTable := AChunkInfo.PieceTable;

  AAltChunkDocumentModel := nil;
  ADocumentFormat := DocumentModel.AutodetectDocumentFormat(ARelationTarget, False);
  if ADocumentFormat = TdxRichEditDocumentFormat.Undefined then
  begin
    AAltChunkDocumentModel := ATargetPieceTable.DocumentModel.CreateNew;
    AInvalidFormat := False;
    NotImplemented;
    AInitialPosition := AStream.Position;
    try
      AAltChunkDocumentModel.LoadDocument(AStream, ADocumentFormat, ARelationTarget);
      AAltChunkDocumentModel.MainPieceTable.InsertParagraph(AAltChunkDocumentModel.MainPieceTable.DocumentEndLogPosition);
    except
      AInvalidFormat := True;
    end;
    if AInvalidFormat then
    begin
      FreeAndNil(AAltChunkDocumentModel);
      ADocumentFormat := TdxRichEditDocumentFormat.PlainText;
      AStream.Position := AInitialPosition;
    end;
  end;
  if AAltChunkDocumentModel = nil then
  begin
    AAltChunkDocumentModel := ATargetPieceTable.DocumentModel.CreateNew;
    AAltChunkDocumentModel.LoadDocument(AStream, ADocumentFormat, ARelationTarget);
    AAltChunkDocumentModel.MainPieceTable.InsertParagraph(AAltChunkDocumentModel.MainPieceTable.DocumentEndLogPosition);
  end;

  if AChunkInfo.LogPosition = ATargetPieceTable.DocumentEndLogPosition + 1 then
    ATargetPieceTable.InsertParagraph(AChunkInfo.LogPosition - 1);
  if AChunkInfo.LogPosition > ATargetPieceTable.DocumentEndLogPosition + 1 then
    Exit;

  ATargetPieceTable.InsertDocumentModelContent(AAltChunkDocumentModel, AChunkInfo.LogPosition, False, False, True);
end;

class procedure TdxOpenXmlImporter.ThrowInvalidFile;
begin
  raise Exception.Create('Invalid OpenXml file');
end;

function TdxOpenXmlImporter.GetWordProcessingMLValue(AValue: TdxWordProcessingMLValue): string;
begin
  Result := AValue.OpenXmlValue;
end;

function TdxOpenXmlImporter.CreateOpenXmlSectionTextDirectionDestination: TdxSectionTextDirectionDestination;
begin
  Result := TdxSectionTextDirectionDestination.Create(Self);
end;

function TdxOpenXmlImporter.CreateWordMLSectionTextDirectionDestination: TdxSectionTextDirectionDestination;
begin
  Result := nil;
end;

function TdxOpenXmlImporter.CreateStyleParagraphPropertiesDestination(AStyleDestination: TdxStyleDestinationBase;
  AParagraphFormatting: TdxParagraphFormattingBase; ATabs: TdxTabFormattingInfo): TdxParagraphPropertiesBaseDestination;
begin
  Result := TdxStyleParagraphPropertiesDestination.Create(Self, AStyleDestination, AParagraphFormatting, ATabs);
end;

function TdxOpenXmlImporter.ConvertToBool(const AValue: string): Boolean;
begin
  if (AValue = '1') or (AValue = 'on') or (AValue = 'true') then
    Exit(True);
  if (AValue = '0') or (AValue = 'off') or (AValue = 'false') then
    Exit(False);

  ThrowInvalidFile;
  Result := False;
end;

procedure TdxOpenXmlImporter.AddAltChunkInfo(AAltChunkInfo: TdxAltChunkInfo);
begin
  FAltChunkInfos.Add(AAltChunkInfo);
end;

function TdxOpenXmlImporter.RegisterFootNote(ANote: TdxFootNote; const AId: string): Integer;
var
  AIndex: Integer;
begin
  if AId <> '' then
  begin
    FFootNotes.AddOrSetValue(AId, ANote);
    AIndex := DocumentModel.FootNotes.Count;
    DocumentModel.FootNotes.Add(ANote);
    Result := AIndex;
  end
  else
    Result := -1;
end;

function TdxOpenXmlImporter.RegisterEndNote(ANote: TdxEndNote; const AId: string): Integer;
var
  AIndex: Integer;
begin
  if AId <> '' then
  begin
    FEndNotes.AddOrSetValue(AId, ANote);
    AIndex := DocumentModel.EndNotes.Count;
    DocumentModel.EndNotes.Add(ANote);
    Result := AIndex;
  end
  else
    Result := -1;
end;

procedure TdxOpenXmlImporter.CheckVersion;
begin
end;

{ TdxImportOpenXmlFormat }

class function TdxImportOpenXmlFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.OpenXml;
end;

function TdxImportOpenXmlFormat.GetDocumentImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>;
begin
  Result := TdxOpenXmlDocumentImporter.Create;
end;

function TdxImportOpenXmlFormat.GetImporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxImporterOptions): TdxDocumentModelImporter;
begin
  Result := TdxOpenXmlImporter.Create(ADocumentModel, AOptions as TdxOpenXmlDocumentImporterOptions);
end;

end.
