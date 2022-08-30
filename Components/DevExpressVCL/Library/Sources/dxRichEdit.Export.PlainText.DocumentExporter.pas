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

unit dxRichEdit.Export.PlainText.DocumentExporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes,
  dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Utils.FileDialogFilter,
  dxRichEdit.Export.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.ImportExportHelper;

type
  { TdxPlainTextDocumentExporter }

  TdxPlainTextDocumentExporter = class(TInterfacedObject, IdxExporter)
  public
    //IdxExporter
    function Filter: TdxFileDialogFilter; virtual;
    function Format: TdxRichEditDocumentFormat; virtual;
    function FormatEquals(const AValue: TdxRichEditDocumentFormat): Boolean;
    function SupportsEncryption: Boolean;
    function SetupSaving: TObject; virtual;
    function SaveDocument(const ADocumentModel: TdxCustomDocumentModel;
      AStream: TStream; const AOptions: IdxExporterOptions): Boolean; virtual;
  end;

implementation

uses
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Options,
  dxRichEdit.Import.PlainText.DocumentImporter;

{ TdxPlainTextDocumentExporter }

function TdxPlainTextDocumentExporter.FormatEquals(
  const AValue: TdxRichEditDocumentFormat): Boolean;
begin
  Result := Format = AValue;
end;

function TdxPlainTextDocumentExporter.Filter: TdxFileDialogFilter;
begin
  Result := TdxPlainTextDocumentImporter.DefaultFilter;
end;

function TdxPlainTextDocumentExporter.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.PlainText;
end;

function TdxPlainTextDocumentExporter.SetupSaving: TObject;
begin
  Result := TdxPlainTextDocumentExporterOptions.Create;
end;

function TdxPlainTextDocumentExporter.SupportsEncryption: Boolean;
begin
  Result := False;
end;

function TdxPlainTextDocumentExporter.SaveDocument(const ADocumentModel: TdxCustomDocumentModel;
  AStream: TStream; const AOptions: IdxExporterOptions): Boolean;
var
  AModel: TdxDocumentModel;
begin
  AModel := TdxDocumentModel(ADocumentModel);
  AModel.InternalAPI.SaveDocumentPlainTextContent(AStream, TdxPlainTextDocumentExporterOptions(AOptions));
  Result := True;
end;

end.
