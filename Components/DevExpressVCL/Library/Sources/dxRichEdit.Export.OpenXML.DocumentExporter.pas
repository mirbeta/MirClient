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

unit dxRichEdit.Export.OpenXML.DocumentExporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes,
  dxRichEdit.NativeApi,
  dxRichEdit.Export.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Utils.FileDialogFilter;

type
  { TdxOpenXmlDocumentExporter }

  TdxOpenXmlDocumentExporter = class(TInterfacedObject, IdxExporter)
  public
    //IdxExporter
    function Filter: TdxFileDialogFilter;
    function Format: TdxRichEditDocumentFormat;
    function FormatEquals(const AValue: TdxRichEditDocumentFormat): Boolean;
    function SupportsEncryption: Boolean;
    function SetupSaving: TObject;
    function SaveDocument(const ADocumentModel: TdxCustomDocumentModel;
      AStream: TStream; const AOptions: IdxExporterOptions): Boolean;
  end;

implementation

uses
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.PieceTable;

{ TdxOpenXmlDocumentExporter }

function TdxOpenXmlDocumentExporter.Filter: TdxFileDialogFilter;
begin
  Result := TdxFileDialogFilter.OpenXMLFiles;
end;

function TdxOpenXmlDocumentExporter.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.OpenXml;
end;

function TdxOpenXmlDocumentExporter.FormatEquals(const AValue: TdxRichEditDocumentFormat): Boolean;
begin
  Result := Format = AValue;
end;

function TdxOpenXmlDocumentExporter.SetupSaving: TObject;
begin
  Result := TdxOpenXmlDocumentExporterOptions.Create;
end;

function TdxOpenXmlDocumentExporter.SupportsEncryption: Boolean;
begin
  Result := True;
end;

function TdxOpenXmlDocumentExporter.SaveDocument(const ADocumentModel: TdxCustomDocumentModel;
  AStream: TStream; const AOptions: IdxExporterOptions): Boolean;
var
  AModel: TdxDocumentModel absolute ADocumentModel;
begin
  AModel.InternalAPI.SaveDocumentOpenXmlContent(AStream, TdxOpenXmlDocumentExporterOptions(AOptions));
  Result := True;
end;

end.
