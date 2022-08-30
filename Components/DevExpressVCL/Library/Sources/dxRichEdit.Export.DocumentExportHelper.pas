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

unit dxRichEdit.Export.DocumentExportHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxEncoding,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Export.Core;

type
  { TdxDocumentExportHelper }

  TdxDocumentExportHelper = class(TdxExportHelper)
  private
    function GetDocumentModel: TdxDocumentModel;
  protected
    function GetPredefinedOptions(AFormat: TdxRichEditDocumentFormat): IdxExporterOptions; override;
    procedure PreprocessContentBeforeExport(AFormat: TdxRichEditDocumentFormat); override;
    function GetFileNameForSaving: string; override;
    function GetCurrentDocumentFormat: TdxRichEditDocumentFormat; override;
    procedure ApplyEncoding(const AOptions: IdxExporterOptions; AEncoding: TEncoding); override;
  public
    procedure ThrowUnsupportedFormatException; override;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Options,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

{ TdxDocumentExportHelper }

function TdxDocumentExportHelper.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxDocumentExportHelper.GetPredefinedOptions(AFormat: TdxRichEditDocumentFormat): IdxExporterOptions;
begin
  Result := DocumentModel.DocumentExportOptions.GetOptions(AFormat);
end;

procedure TdxDocumentExportHelper.PreprocessContentBeforeExport(AFormat: TdxRichEditDocumentFormat);
begin
  DocumentModel.PreprocessContentBeforeExport(AFormat);
end;

function TdxDocumentExportHelper.GetFileNameForSaving: string;
var
  ADocumentSaveOptions: TdxDocumentSaveOptions;
begin
  ADocumentSaveOptions := DocumentModel.DocumentSaveOptions;
  if not ADocumentSaveOptions.CanSaveToCurrentFileName or (ADocumentSaveOptions.CurrentFileName = '') then
    Result := ADocumentSaveOptions.DefaultFileName
  else
    Result := ADocumentSaveOptions.CurrentFileName;
end;

function TdxDocumentExportHelper.GetCurrentDocumentFormat: TdxRichEditDocumentFormat;
var
  ADocumentSaveOptions: TdxDocumentSaveOptions;
begin
  ADocumentSaveOptions := DocumentModel.DocumentSaveOptions;
  Result := ADocumentSaveOptions.CurrentFormat;
  if Result = TdxRichEditDocumentFormat.Undefined then
    Result := ADocumentSaveOptions.DefaultFormat;
end;

procedure TdxDocumentExportHelper.ThrowUnsupportedFormatException;
begin
  TdxRichEditExceptions.ThrowUnsupportedFormatException;
end;

procedure TdxDocumentExportHelper.ApplyEncoding(const AOptions: IdxExporterOptions; AEncoding: TEncoding);
begin
  if AOptions is TdxDocumentExporterOptions then
    TdxDocumentExporterOptions(AOptions).ActualEncoding := AEncoding;
end;

end.
