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

unit dxRichEdit.Import.OpenXML.DestinationBookmark;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  SysUtils, dxGenerics,
  dxXMLReader,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.OpenXML;

type

  { TdxBookmarkElementDestination }

  TdxBookmarkElementDestination = class abstract(TdxLeafElementDestination)
  protected
    procedure AssignBookmarkPosition(ABookmark: TdxImportBookmarkInfo); virtual; abstract;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxBookmarkStartElementDestination }

  TdxBookmarkStartElementDestination = class(TdxBookmarkElementDestination)
  protected
    procedure AssignBookmarkPosition(ABookmark: TdxImportBookmarkInfo); override;
  end;

  { TdxBookmarkEndElementDestination }

  TdxBookmarkEndElementDestination = class(TdxBookmarkElementDestination)
  protected
    procedure AssignBookmarkPosition(ABookmark: TdxImportBookmarkInfo); override;
  end;

implementation

{ TdxBookmarkElementDestination }

procedure TdxBookmarkElementDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AId, AName: string;
  ABookmark: TdxImportBookmarkInfo;
begin
  AId := AReader.GetAttribute('id', Importer.WordProcessingNamespaceConst);
  if AId <> '' then
    AId := Trim(AId);
  if AId = '' then
    Exit;

  if not Importer.Bookmarks.TryGetValue(AId, ABookmark) then
  begin
    ABookmark := TdxImportBookmarkInfo.Create;
    Importer.Bookmarks.Add(AId, ABookmark);
  end;
  AName := AReader.GetAttribute('name', Importer.WordProcessingNamespaceConst);
  if AName <> '' then
    ABookmark.Name := Trim(AName);
  AssignBookmarkPosition(ABookmark);
end;

{ TdxBookmarkStartElementDestination }

procedure TdxBookmarkStartElementDestination.AssignBookmarkPosition(ABookmark: TdxImportBookmarkInfo);
begin
  ABookmark.Start := Importer.Position.LogPosition;
end;

{ TdxBookmarkEndElementDestination }

procedure TdxBookmarkEndElementDestination.AssignBookmarkPosition(ABookmark: TdxImportBookmarkInfo);
begin
  ABookmark.&End := Importer.Position.LogPosition;
end;

end.
