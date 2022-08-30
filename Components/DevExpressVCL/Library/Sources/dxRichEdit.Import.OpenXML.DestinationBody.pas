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

unit dxRichEdit.Import.OpenXML.DestinationBody;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxGenerics,

  dxXMLReader,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationSection;

type

  { TdxBodyDestinationBase }

  TdxBodyDestinationBase = class abstract(TdxElementDestination)
  protected
    class function OnBookmarkStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBookmarkEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRangePermissionStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRangePermissionEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCommentStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCommentEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxBodyDestination }

  TdxBodyDestination = class(TdxBodyDestinationBase)
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
    class function OnAltChunk(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSection(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  TdxLastSectionDestination = class(TdxSectionDestination);

implementation

uses
  dxRichEdit.Utils.Types,
  dxRichEdit.Import.OpenXML,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.OpenXML.DestinationComment,
  dxRichEdit.Import.OpenXML.DestinationTable;

{ TdxBodyDestinationBase }

class function TdxBodyDestinationBase.OnBookmarkStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateBookmarkStartElementDestination(AReader);
end;

class function TdxBodyDestinationBase.OnBookmarkEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateBookmarkEndElementDestination(AReader);
end;

class function TdxBodyDestinationBase.OnRangePermissionStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRangePermissionStartElementDestination.Create(AImporter);
end;

class function TdxBodyDestinationBase.OnRangePermissionEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRangePermissionEndElementDestination.Create(AImporter);
end;

class function TdxBodyDestinationBase.OnCommentStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCommentStartElementDestination.Create(AImporter);
end;

class function TdxBodyDestinationBase.OnCommentEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCommentEndElementDestination.Create(AImporter);
end;

class function TdxBodyDestinationBase.OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStructuredDocumentDestination.Create(AImporter);
end;

class function TdxBodyDestinationBase.OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCustomXmlDestination.Create(AImporter);
end;

{ TdxBodyDestination }

class constructor TdxBodyDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxBodyDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxBodyDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('p', OnParagraph);
  Result.Add('tbl', OnTable);
  Result.Add('sectPr', OnSection);
  Result.Add('bookmarkStart', OnBookmarkStart);
  Result.Add('bookmarkEnd', OnBookmarkEnd);
  Result.Add('permStart', OnRangePermissionStart);
  Result.Add('permEnd', OnRangePermissionEnd);
  Result.Add('commentRangeStart', OnCommentStart);
  Result.Add('commentRangeEnd', OnCommentEnd);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('altChunk', OnAltChunk);
  Result.Add('customXml', OnCustomXml);
end;

function TdxBodyDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxBodyDestination.OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  if AImporter.DocumentModel.DocumentCapabilities.TablesAllowed then
    Result := TdxTableDestination.Create(AImporter)
  else
    Result := TdxTableDisabledDestination.Create(AImporter);
end;

class function TdxBodyDestination.OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateParagraphDestination;
end;

class function TdxBodyDestination.OnAltChunk(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxAltChunkDestination.Create(AImporter);
end;

class function TdxBodyDestination.OnSection(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxLastSectionDestination.Create(AImporter);
end;

end.


