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
unit dxRichEdit.Import.PlainText;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.NativeApi,
  dxRichEdit.Import.Core,
  dxRichEdit.Import.Formats,
  dxRichEdit.DocumentModel.Core;

type
  { TdxImportPlainTextFormat }

  TdxImportPlainTextFormat = class(TdxImportFileFormat)
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; override;
    function GetImporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxImporterOptions): TdxDocumentModelImporter; override;
    function GetDocumentImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>; override;
  end;

implementation

uses
  dxRichEdit.Import.PlainText.DocumentImporter;

{ TdxImportPlainTextFormat }

class function TdxImportPlainTextFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.PlainText;
end;

function TdxImportPlainTextFormat.GetDocumentImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>;
begin
  Result := TdxPlainTextDocumentImporter.Create;
end;

function TdxImportPlainTextFormat.GetImporter(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxImporterOptions): TdxDocumentModelImporter;
begin
  Result := nil;
end;

end.
