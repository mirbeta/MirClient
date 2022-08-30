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

unit dxRichEdit.Import.Html.DocumentImporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes,
  dxCoreClasses,

  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Utils.FileDialogFilter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Import.Core,
  dxRichEdit.ImportExportHelper;

type
  TdxHtmlDocumentImporter = class(TInterfacedObject, IdxImporter<TdxRichEditDocumentFormat, Boolean>)
  public
    //IdxImporter
    function Filter: TdxFileDialogFilter;
    function Format: TdxRichEditDocumentFormat;
    function LoadDocument(const ADocumentModel: TdxCustomDocumentModel;
      AStream: TStream; const AOptions: IdxImporterOptions): Boolean;
    function SetupLoading: TObject;
  end;

implementation

uses
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Options;

{ TdxHtmlDocumentImporter }

function TdxHtmlDocumentImporter.Filter: TdxFileDialogFilter;
begin
  Result := TdxFileDialogFilter.HtmlFiles;
end;

function TdxHtmlDocumentImporter.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Html;
end;

function TdxHtmlDocumentImporter.LoadDocument(const ADocumentModel: TdxCustomDocumentModel;
  AStream: TStream; const AOptions: IdxImporterOptions): Boolean;
var
  AModel: TdxDocumentModel absolute ADocumentModel;
begin
  AModel.InternalAPI.LoadDocumentHtmlContent(AStream, TdxHtmlDocumentImporterOptions(AOptions));
  Result := True;
end;

function TdxHtmlDocumentImporter.SetupLoading: TObject;
begin
  Result := TdxHtmlDocumentImporterOptions.Create;
end;

end.
