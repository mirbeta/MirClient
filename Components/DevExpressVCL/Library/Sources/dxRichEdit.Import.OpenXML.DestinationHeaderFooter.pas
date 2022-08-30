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

unit dxRichEdit.Import.OpenXML.DestinationHeaderFooter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.Types,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.OpenXML.DestinationBody,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.Import.OpenXML;

type

  { TdxHeaderFooterDestinationBase }

  TdxHeaderFooterDestinationBase<T: TdxSectionHeaderFooterBase> = class abstract(TdxBodyDestinationBase)
  strict private
    FNewHeaderFooter: T;
  protected
    function CalculateTargetHeaderFooter(AReader: TdxXmlReader): T; virtual; abstract;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxHeaderFooterDestination }

  TdxHeaderFooterDestination<T: TdxSectionHeaderFooterBase> = class abstract(TdxHeaderFooterDestinationBase<T>)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
  strict private
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxHeaderFooterReferenceDestinationBase }

  TdxHeaderFooterReferenceDestinationBase<T: TdxSectionHeaderFooterBase> = class abstract(TdxLeafElementDestination)
  protected
    type
      TdxHeaderFooterTypeReference = (
        Unknown,
        Default,
        First,
        Even
      );
  protected
    function GetHeaderFooterTypeReference(const AType: string): TdxHeaderFooterTypeReference;

    function GetRootPieceTableTag: string; virtual; abstract;
    function GetImporter: TdxOpenXmlImporter; reintroduce;
    function CreatePieceTable(const AType: string): T; virtual; abstract;
    function CreatePieceTableDestination(APieceTable: T): TdxHeaderFooterDestination<T>; virtual; abstract;

    property Importer: TdxOpenXmlImporter read GetImporter;
    property RootPieceTableTag: string read GetRootPieceTableTag;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxHeaderReferenceDestination }

  TdxHeaderReferenceDestination = class(TdxHeaderFooterReferenceDestinationBase<TdxSectionHeader>)
  protected
    function GetRootPieceTableTag: string; override;
    function CreatePieceTableDestination(APieceTable: TdxSectionHeader): TdxHeaderFooterDestination<TdxSectionHeader>; override;
    function CreatePieceTable(const AType: string): TdxSectionHeader; override;
  end;

  { TdxFooterReferenceDestination }

  TdxFooterReferenceDestination = class(TdxHeaderFooterReferenceDestinationBase<TdxSectionFooter>)
  protected
    function GetRootPieceTableTag: string; override;
    function CreatePieceTableDestination(APieceTable: TdxSectionFooter): TdxHeaderFooterDestination<TdxSectionFooter>; override;
    function CreatePieceTable(const AType: string): TdxSectionFooter; override;
  end;

  { TdxHeaderDestination }

  TdxHeaderDestination = class(TdxHeaderFooterDestination<TdxSectionHeader>)
  strict private
    FHeader: TdxSectionHeader;
  protected
    function CalculateTargetHeaderFooter(AReader: TdxXmlReader): TdxSectionHeader; override;
  public
    constructor Create(AImporter: TdxWordProcessingMLBaseImporter; AHeader: TdxSectionHeader);
  end;

  { TdxFooterDestination }

  TdxFooterDestination = class(TdxHeaderFooterDestination<TdxSectionFooter>)
  strict private
    FFooter: TdxSectionFooter;
  protected
    function CalculateTargetHeaderFooter(AReader: TdxXmlReader): TdxSectionFooter; override;
  public
    constructor Create(AImporter: TdxWordProcessingMLBaseImporter; AFooter: TdxSectionFooter);
  end;

implementation

uses
  IOUtils, Contnrs,
  dxRichEdit.Import.OpenXML.DestinationTable,
  dxRichEdit.Export.OpenXML,
  dxRichEdit.Import.PackageUtils,
  dxRichEdit.Utils.OfficeImage;

{ TdxHeaderFooterDestinationBase }

procedure TdxHeaderFooterDestinationBase<T>.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FNewHeaderFooter := CalculateTargetHeaderFooter(AReader);
  Importer.PushCurrentPieceTable(TdxPieceTable(FNewHeaderFooter.PieceTable));
end;

procedure TdxHeaderFooterDestinationBase<T>.ProcessElementClose(AReader: TdxXmlReader);
begin
  PieceTable.FixLastParagraph;
  Importer.InsertBookmarks;
  Importer.InsertRangePermissions;
  PieceTable.FixTables;
  Importer.PopCurrentPieceTable;
end;

{ TdxHeaderFooterDestination }

class constructor TdxHeaderFooterDestination<T>.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class function TdxHeaderFooterDestination<T>.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('p', OnParagraph);
  Result.Add('tbl', OnTable);
  Result.Add('bookmarkStart', OnBookmarkStart);
  Result.Add('bookmarkEnd', OnBookmarkEnd);
  Result.Add('permStart', OnRangePermissionStart);
  Result.Add('permEnd', OnRangePermissionEnd);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('customXml', OnCustomXml);
end;

class destructor TdxHeaderFooterDestination<T>.Finalize;
begin
  FreeAndNil(FHandlerTable);
end;

function TdxHeaderFooterDestination<T>.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxHeaderFooterDestination<T>.OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  if AImporter.DocumentModel.DocumentCapabilities.TablesAllowed then
    Result := TdxTableDestination.Create(AImporter)
  else
    Result := TdxTableDisabledDestination.Create(AImporter);
end;

class function TdxHeaderFooterDestination<T>.OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateParagraphDestination;
end;

{ TdxHeaderFooterReferenceDestinationBase }

function TdxHeaderFooterReferenceDestinationBase<T>.GetHeaderFooterTypeReference(const AType: string): TdxHeaderFooterTypeReference;
begin
  if (AType = '') or (AType = 'default') then
    Result := TdxHeaderFooterTypeReference.Default
  else
    if AType = 'first' then
      Result := TdxHeaderFooterTypeReference.First
    else
      if AType = 'even' then
        Result := TdxHeaderFooterTypeReference.Even
      else
        Result := TdxHeaderFooterTypeReference.Unknown;
end;

function TdxHeaderFooterReferenceDestinationBase<T>.GetImporter: TdxOpenXmlImporter;
begin
  Result := TdxOpenXmlImporter(inherited Importer);
end;

procedure TdxHeaderFooterReferenceDestinationBase<T>.ProcessElementOpen(AReader: TdxXmlReader);
var
  AImporter: TdxOpenXmlImporter;
  AId, AHeaderFooterFileName: string;
  APieceTableStream: TStream;
  AHeaderReader: TdxXmlReader;
  ANewPieceTable: T;
  ADestination: TdxHeaderFooterDestination<T>;
begin
  AImporter := TdxOpenXmlImporter(Importer);
  AId := AReader.GetAttribute('id', TdxOpenXmlExporter.RelsNamespace);
  if AId = '' then
    Importer.ThrowInvalidFile;

  APieceTableStream := AImporter.LookupPackageFileStreamByRelationId(AId, AImporter.DocumentRootFolder, True);
  if APieceTableStream = nil then
    Importer.ThrowInvalidFile;

  AHeaderFooterFileName := Importer.LookupRelationTargetById(Importer.DocumentRelations, AId, AImporter.DocumentRootFolder, '');
  Importer.DocumentRelationsStack.Push(Importer.ImportRelations(Importer.DocumentRootFolder + '/_rels/' +
    TPath.GetFileName(AHeaderFooterFileName) + '.rels'));
  Importer.PackageFileStreamsStack.Push(TdxPackageFileStreams.Create);
  Importer.PackageImagesStack.Push(TdxNamedObjectDictionary<TdxOfficeImageReference>.Create(True));

  AHeaderReader := AImporter.CreateXmlReader(APieceTableStream);
  try
    if not AImporter.ReadToRootElement(AHeaderReader, RootPieceTableTag) then
      Importer.ThrowInvalidFile;

    ANewPieceTable := CreatePieceTable(AReader.GetAttribute('type', Importer.WordProcessingNamespaceConst));
    ADestination := CreatePieceTableDestination(ANewPieceTable);

    ADestination.ProcessElementOpen(AReader);
    AImporter.ImportContent(AHeaderReader, ADestination);
  finally
    AHeaderReader.Free;
  end;

  Importer.PackageFileStreamsStack.Pop;
  Importer.DocumentRelationsStack.Pop;
  Importer.PackageImagesStack.Pop;
end;

{ TdxHeaderReferenceDestination }

function TdxHeaderReferenceDestination.GetRootPieceTableTag: string;
begin
  Result := 'hdr';
end;

function TdxHeaderReferenceDestination.CreatePieceTableDestination(APieceTable: TdxSectionHeader): TdxHeaderFooterDestination<TdxSectionHeader>;
begin
  Result := TdxHeaderDestination.Create(Importer, APieceTable);
end;

function TdxHeaderReferenceDestination.CreatePieceTable(const AType: string): TdxSectionHeader;
var
  ASection: TdxSection;
  ATypeValue: TdxHeaderFooterTypeReference;
begin
  ATypeValue := GetHeaderFooterTypeReference(AType);

  ASection := Importer.CurrentSection;
  case ATypeValue of
    TdxHeaderFooterTypeReference.First:
      begin
        ASection.Headers.Add(TdxHeaderFooterType.First);
        Result := ASection.InnerFirstPageHeader;
      end;
    TdxHeaderFooterTypeReference.Even:
      begin
        ASection.Headers.Add(TdxHeaderFooterType.Even);
        Result := ASection.InnerEvenPageHeader;
      end;
    TdxHeaderFooterTypeReference.Default:
      begin
        ASection.Headers.Add(TdxHeaderFooterType.Odd);
        Result := ASection.InnerOddPageHeader;
      end
    else
      begin
        Importer.ThrowInvalidFile;
        Result := nil;
      end;
  end;
end;

{ TdxFooterReferenceDestination }

function TdxFooterReferenceDestination.GetRootPieceTableTag: string;
begin
  Result := 'ftr';
end;

function TdxFooterReferenceDestination.CreatePieceTableDestination(APieceTable: TdxSectionFooter): TdxHeaderFooterDestination<TdxSectionFooter>;
begin
  Result := TdxFooterDestination.Create(Importer, APieceTable);
end;

function TdxFooterReferenceDestination.CreatePieceTable(const AType: string): TdxSectionFooter;
var
  ASection: TdxSection;
  ATypeValue: TdxHeaderFooterTypeReference;
begin
  ATypeValue := GetHeaderFooterTypeReference(AType);

  ASection := Importer.CurrentSection;
  case ATypeValue of
    TdxHeaderFooterTypeReference.First:
      begin
        ASection.Footers.Add(TdxHeaderFooterType.First);
        Result := ASection.InnerFirstPageFooter;
      end;
    TdxHeaderFooterTypeReference.Even:
      begin
        ASection.Footers.Add(TdxHeaderFooterType.Even);
        Result := ASection.InnerEvenPageFooter;
      end;
    TdxHeaderFooterTypeReference.Default:
      begin
        ASection.Footers.Add(TdxHeaderFooterType.Odd);
        Result := ASection.InnerOddPageFooter;
      end
    else
      begin
        Importer.ThrowInvalidFile;
        Result := nil;
      end;
  end;
end;

{ TdxHeaderDestination }

constructor TdxHeaderDestination.Create(AImporter: TdxWordProcessingMLBaseImporter; AHeader: TdxSectionHeader);
begin
  inherited Create(AImporter);
  Assert(AHeader <> nil, 'header');
  FHeader := AHeader;
end;

function TdxHeaderDestination.CalculateTargetHeaderFooter(AReader: TdxXmlReader): TdxSectionHeader;
begin
  Result := FHeader;
end;

{ TdxFooterDestination }

constructor TdxFooterDestination.Create(AImporter: TdxWordProcessingMLBaseImporter; AFooter: TdxSectionFooter);
begin
  inherited Create(AImporter);
  Assert(AFooter <> nil, 'footer');
  FFooter := AFooter;
end;

function TdxFooterDestination.CalculateTargetHeaderFooter(AReader: TdxXmlReader): TdxSectionFooter;
begin
  Result := FFooter;
end;

end.
