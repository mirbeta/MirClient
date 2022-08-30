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

unit dxRichEdit.Import.OpenXML.DestinationFields;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.DocumentFormats.DocumentFormatUtils,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Hyperlink;

type

  { TdxFieldDestinationBase }

  TdxFieldDestinationBase = class abstract(TdxElementDestination)
  strict private
    FImportFieldHelper: TdxImportFieldHelper;
  protected
    procedure ProcessFieldBegin(ADisableUpdate: Boolean); virtual;
    procedure ProcessFieldSeparator; virtual;
    procedure ProcessFieldEnd; virtual;

    property ImportFieldHelper: TdxImportFieldHelper read FImportFieldHelper;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    destructor Destroy; override;
  end;

  { TdxFieldSimpleDestination }

  TdxFieldSimpleDestination = class(TdxFieldDestinationBase)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
  strict private
    FFieldCode: string;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnRun(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFieldSimple(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnHyperlink(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxFieldCharDestination }

  TdxFieldCharDestination = class(TdxFieldDestinationBase)
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
  end;

  { TdxHyperlinkAttributeHandlerTable }

  TdxHyperlinkAttributeHandler = reference to procedure(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
    AInfo: TdxHyperlinkInfo; const AValue: string);

  TdxHyperlinkAttributeHandlerTable = class(TdxNamedDelegateDictionary<TdxHyperlinkAttributeHandler>);

  { TdxHyperlinkDestination }

  TdxHyperlinkDestination = class(TdxFieldDestinationBase)
  strict private
    class var
      FAttributeHandlerTable: TdxHyperlinkAttributeHandlerTable;
      FHandlerTable: TdxElementHandlerTable;
  strict private
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    class function CreateAttributeHandlerTable: TdxHyperlinkAttributeHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    function GetAttributeHandlerTable: TdxHyperlinkAttributeHandlerTable; virtual;
    function CreateHyperlinkInfo(AReader: TdxXmlReader): TdxHyperlinkInfo; virtual;
    class function OnBookmarkStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBookmarkEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRangePermissionStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRangePermissionEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class procedure OnIdAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string); static;
    class procedure OnAnchorAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string); static;
    class procedure OnTargetFrameAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string); static;
    class procedure OnTooltipAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string); static;
    class procedure OnHistoryAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string); static;
    class procedure OnDocLocationAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string); static;

    property AttributeHandlerTable: TdxHyperlinkAttributeHandlerTable read GetAttributeHandlerTable;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnRun(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFieldSimple(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnHyperlink(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

implementation

uses
  Contnrs, StrUtils,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.Import.OpenXML,
  dxStringHelper;

{ TdxFieldDestinationBase }

constructor TdxFieldDestinationBase.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  FImportFieldHelper := TdxImportFieldHelper.Create(Importer.PieceTable);
end;

destructor TdxFieldDestinationBase.Destroy;
begin
  FreeAndNil(FImportFieldHelper);
  inherited Destroy;
end;

procedure TdxFieldDestinationBase.ProcessFieldBegin(ADisableUpdate: Boolean);
var
  AFieldInfo: TdxImportFieldInfo;
begin
  AFieldInfo := TdxImportFieldInfo.Create(Importer.PieceTable);

  FImportFieldHelper.ProcessFieldBegin(AFieldInfo, Importer.Position);
  Importer.FieldInfoStack.Push(AFieldInfo);
end;

procedure TdxFieldDestinationBase.ProcessFieldSeparator;
var
  AFieldInfo: TdxImportFieldInfo;
begin
  AFieldInfo := Importer.FieldInfoStack.Peek;
  FImportFieldHelper.ProcessFieldSeparator(AFieldInfo, Importer.Position);
end;

procedure TdxFieldDestinationBase.ProcessFieldEnd;
var
  AFieldInfo: TdxImportFieldInfo;
  AField: TdxField;
begin
  AFieldInfo := Importer.FieldInfoStack.Peek;

  AField := FImportFieldHelper.ProcessFieldEnd(AFieldInfo, Importer.Position);

  Importer.FieldInfoStack.Pop;
  if Importer.FieldInfoStack.Count > 0 then
    AField.Parent := Importer.FieldInfoStack.Peek.Field;
end;

{ TdxFieldSimpleDestination }

class constructor TdxFieldSimpleDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class function TdxFieldSimpleDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('r', OnRun);
  Result.Add('fldSimple', OnFieldSimple);
  Result.Add('hyperlink', OnHyperlink);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('customXml', OnCustomXml);
end;

class destructor TdxFieldSimpleDestination.Finalize;
begin
  FreeAndNil(FHandlerTable);
end;

function TdxFieldSimpleDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxFieldSimpleDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FFieldCode := AReader.GetAttribute('instr', Importer.WordProcessingNamespaceConst);
  if FFieldCode = '' then
    Exit;

  ProcessFieldBegin(False);
  Importer.PieceTable.InsertTextCore(Importer.Position, FFieldCode);
  ProcessFieldSeparator;
end;

procedure TdxFieldSimpleDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  if FFieldCode <> '' then
    ProcessFieldEnd;
end;

class function TdxFieldSimpleDestination.OnRun(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateRunDestination;
end;

class function TdxFieldSimpleDestination.OnFieldSimple(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFieldSimpleDestination.Create(TdxWordProcessingMLBaseImporter(AImporter));
end;

class function TdxFieldSimpleDestination.OnHyperlink(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxHyperlinkDestination.Create(TdxWordProcessingMLBaseImporter(AImporter));
end;

class function TdxFieldSimpleDestination.OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStructuredDocumentDestination.Create(AImporter);
end;

class function TdxFieldSimpleDestination.OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCustomXmlDestination.Create(AImporter);
end;

{ TdxFieldCharDestination }

class constructor TdxFieldCharDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class function TdxFieldCharDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
end;

class destructor TdxFieldCharDestination.Finalize;
begin
  FreeAndNil(FHandlerTable);
end;

function TdxFieldCharDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxFieldCharDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AType, AAttr: string;
  ADisableUpdate: Boolean;
begin
  AType := AReader.GetAttribute('fldCharType', Importer.WordProcessingNamespaceConst);
  AAttr := AReader.GetAttribute('disableUpdate', Importer.WordProcessingNamespaceConst);
  if AAttr <> '' then
    ADisableUpdate := Importer.ConvertToBool(AAttr)
  else
    ADisableUpdate := False;

  if AType = 'begin' then
    ProcessFieldBegin(ADisableUpdate)
  else
    if AType = 'separate' then
      ProcessFieldSeparator
    else
      if AType = 'end' then
        ProcessFieldEnd;
end;

{ TdxHyperlinkDestination }

class constructor TdxHyperlinkDestination.Initialize;
begin
  FAttributeHandlerTable := CreateAttributeHandlerTable;
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxHyperlinkDestination.Finalize;
begin
  FreeAndNil(FAttributeHandlerTable);
  FreeAndNil(FHandlerTable);
end;

class function TdxHyperlinkDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('r', OnRun);
  Result.Add('fldSimple', OnFieldSimple);
  Result.Add('hyperlink', OnHyperlink);
  Result.Add('bookmarkStart', OnBookmarkStart);
  Result.Add('bookmarkEnd', OnBookmarkEnd);
  Result.Add('permStart', OnRangePermissionStart);
  Result.Add('permEnd', OnRangePermissionEnd);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('customXml', OnCustomXml);
end;

class function TdxHyperlinkDestination.CreateAttributeHandlerTable: TdxHyperlinkAttributeHandlerTable;
begin
  Result := TdxHyperlinkAttributeHandlerTable.Create;
  Result.Add('id', OnIdAttribute);
  Result.Add('anchor', OnAnchorAttribute);
  Result.Add('tgtFrame', OnTargetFrameAttribute);
  Result.Add('tooltip', OnTooltipAttribute);
  Result.Add('history', OnHistoryAttribute);
  Result.Add('docLocation', OnDocLocationAttribute);
end;

function TdxHyperlinkDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxHyperlinkDestination.GetAttributeHandlerTable: TdxHyperlinkAttributeHandlerTable;
begin
  Result := FAttributeHandlerTable;
end;

procedure TdxHyperlinkDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AHyperlinkInfo: TdxHyperlinkInfo;
begin
  if not TdxDocumentFormatsHelper.ShouldInsertHyperlink(DocumentModel) then
    Exit;
  ProcessFieldBegin(False);
  AHyperlinkInfo := CreateHyperlinkInfo(AReader);
  try
    ImportFieldHelper.InsertHyperlinkInstruction(AHyperlinkInfo, Importer.Position);
  finally
    AHyperlinkInfo.Free;
  end;
  ProcessFieldSeparator;
end;

function TdxHyperlinkDestination.CreateHyperlinkInfo(AReader: TdxXmlReader): TdxHyperlinkInfo;
var
  AHyperlinkInfo: TdxHyperlinkInfo;
  AHandler: TdxHyperlinkAttributeHandler;
begin
  AHyperlinkInfo := TdxHyperlinkInfo.Create;
  while AReader.MoveToNextAttribute do
  begin
    if AttributeHandlerTable.TryGetValue(AReader.LocalName, AHandler) and AReader.HasValue then
      AHandler(Importer, AHyperlinkInfo, AReader.Value)
    else
      if TdxStringHelper.IndexOf(AReader.Name, 'xmlns') < 0 then
        Importer.ThrowInvalidFile;
  end;
  Result := AHyperlinkInfo;
end;

procedure TdxHyperlinkDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  if not TdxDocumentFormatsHelper.ShouldInsertHyperlink(DocumentModel) then
    Exit;
  ProcessFieldEnd;
end;

class function TdxHyperlinkDestination.OnRun(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateRunDestination;
end;

class function TdxHyperlinkDestination.OnFieldSimple(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFieldSimpleDestination.Create(TdxWordProcessingMLBaseImporter(AImporter));
end;

class function TdxHyperlinkDestination.OnHyperlink(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxHyperlinkDestination.Create(TdxWordProcessingMLBaseImporter(AImporter));
end;

class function TdxHyperlinkDestination.OnBookmarkStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateBookmarkStartElementDestination(AReader);
end;

class function TdxHyperlinkDestination.OnBookmarkEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateBookmarkEndElementDestination(AReader);
end;

class function TdxHyperlinkDestination.OnRangePermissionStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRangePermissionStartElementDestination.Create(AImporter);
end;

class function TdxHyperlinkDestination.OnRangePermissionEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRangePermissionEndElementDestination.Create(AImporter);
end;

class function TdxHyperlinkDestination.OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStructuredDocumentDestination.Create(AImporter);
end;

class function TdxHyperlinkDestination.OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCustomXmlDestination.Create(AImporter);
end;

class procedure TdxHyperlinkDestination.OnIdAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string);
var
  ARelation: TdxOpenXmlRelation;
begin
  ARelation := (TdxOpenXmlImporter(AImporter)).DocumentRelations.LookupRelationById(AValue);
  AInfo.NavigateUri := ARelation.Target;
end;

class procedure TdxHyperlinkDestination.OnAnchorAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string);
begin
  AInfo.Anchor := AValue;
end;

class procedure TdxHyperlinkDestination.OnTargetFrameAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string);
begin
  AInfo.Target := AValue;
end;

class procedure TdxHyperlinkDestination.OnTooltipAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string);
begin
  AInfo.ToolTip := AValue;
end;

class procedure TdxHyperlinkDestination.OnHistoryAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string);
begin
  AInfo.Visited := not AImporter.ConvertToBool(AValue);
end;

class procedure TdxHyperlinkDestination.OnDocLocationAttribute(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInfo: TdxHyperlinkInfo; const AValue: string);
begin
end;

end.
