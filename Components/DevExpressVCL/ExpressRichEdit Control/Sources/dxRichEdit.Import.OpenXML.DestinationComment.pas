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

unit dxRichEdit.Import.OpenXML.DestinationComment;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes,
  dxCore, dxCoreClasses, dxGenerics,
  dxXMLReader,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationBody,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.OpenXML;

type
  TdxCommentElementDestination = TdxEmptyDestination;
  TdxCommentStartElementDestination = TdxEmptyDestination;
  TdxCommentEndElementDestination = TdxEmptyDestination;
  TdxCommentReferenceElementDestination = TdxEmptyDestination;


  { TdxCommentsDestination }

  TdxCommentsDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    class function OnComment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  end;

  { TdxCommentDestination }

  TdxCommentDestination = class(TdxBodyDestinationBase)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    class function OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAltChunk(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

implementation

uses
  Contnrs,
  dxRichEdit.Utils.Types, dxRichEdit.Import.OpenXML.DestinationTable;


{ TdxCommentsDestination }

class constructor TdxCommentsDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxCommentsDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxCommentsDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('comment', OnComment);
end;

class function TdxCommentsDestination.OnComment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxEmptyDestination.Create(AImporter);
end;

function TdxCommentsDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

{ TdxCommentDestination }

class constructor TdxCommentDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxCommentDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxCommentDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('p', OnParagraph);
  Result.Add('tbl', OnTable);
  Result.Add('bookmarkStart', OnBookmarkStart);
  Result.Add('bookmarkEnd', OnBookmarkEnd);
  Result.Add('permStart', OnRangePermissionStart);
  Result.Add('permEnd', OnRangePermissionEnd);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('altChunk', OnAltChunk);
  Result.Add('customXml', OnCustomXml);
end;

class function TdxCommentDestination.OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  if AImporter.DocumentModel.DocumentCapabilities.TablesAllowed then
    Result := TdxTableDestination.Create(AImporter)
  else
    Result := TdxTableDisabledDestination.Create(AImporter);
end;

class function TdxCommentDestination.OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateParagraphDestination;
end;

class function TdxCommentDestination.OnAltChunk(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxAltChunkDestination.Create(AImporter);
end;

function TdxCommentDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxCommentDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  NotImplemented;
end;

procedure TdxCommentDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  NotImplemented;
end;

end.
