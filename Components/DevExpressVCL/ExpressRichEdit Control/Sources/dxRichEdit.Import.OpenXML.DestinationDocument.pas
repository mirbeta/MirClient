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

unit dxRichEdit.Import.OpenXML.DestinationDocument;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes,
  dxXMLReader,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.OpenXML;

type

  { TdxDocumentDestination }

  TdxDocumentDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnBody(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBackground(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnVersion(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxDocumentBackgroundDestination }

  TdxDocumentBackgroundDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDocumentVersionDestination }

  TdxDocumentVersionDestination = class(TdxLeafElementDestination)
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  end;

implementation

uses
  Contnrs, dxCoreGraphics, dxGenerics,
  dxRichEdit.Import.OpenXML.DestinationBody;

{ TdxDocumentDestination }

class constructor TdxDocumentDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDocumentDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDocumentDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('body', OnBody);
  Result.Add('background', OnBackground);
  Result.Add('version', OnVersion);
end;

function TdxDocumentDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDocumentDestination.OnBody(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  (AImporter as TdxOpenXmlImporter).CheckVersion;
  Result := TdxBodyDestination.Create(AImporter);
end;

class function TdxDocumentDestination.OnBackground(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentBackgroundDestination.Create(AImporter);
end;

class function TdxDocumentDestination.OnVersion(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateVersionDestination(AReader);
end;

{ TdxDocumentBackgroundDestination }

procedure TdxDocumentBackgroundDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AColor: TdxAlphaColor;
begin
  AColor := TdxWordProcessingMLBaseImporter(Importer).GetWpSTColorValue(AReader, 'color');
  if AColor <> TdxAlphaColors.Empty then
    Importer.DocumentModel.DocumentProperties.PageBackColor := AColor;
end;

{ TdxDocumentVersionDestination }

function TdxDocumentVersionDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Empty;
end;

end.
