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

unit dxRichEdit.Import.DocumentImportHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxGenerics,
  dxEncoding,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.Core;

type
  { TdxDocumentImportHelper }

  TdxDocumentImportHelper = class(TdxImportHelper<TdxRichEditDocumentFormat, Boolean>)
  strict private
    function GetDocumentModel: TdxDocumentModel;
  protected
    procedure ApplyEncoding(const AOptions: IdxImporterOptions; AEncoding: TEncoding); override;
    function GetUndefinedFormat: TdxRichEditDocumentFormat; override;
    function GetFallbackFormat: TdxRichEditDocumentFormat; override;
    function GetPredefinedOptions(AFormat: TdxRichEditDocumentFormat): TObject; override;
    function EqualsFormat(const Value1, Value2: TdxRichEditDocumentFormat): Boolean; override;
  public
    procedure ThrowUnsupportedFormatException; override;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  end;

  { TdxImageImportHelper }

  TdxImageImportHelper = class(TdxImportHelper<TdxOfficeImageFormat, TdxOfficeImageReference>)
  protected
    procedure ApplyEncoding(const AOptions: IdxImporterOptions; AEncoding: TEncoding); override;
    function GetUndefinedFormat: TdxOfficeImageFormat; override;
    function GetFallbackFormat: TdxOfficeImageFormat; override;
    function GetPredefinedOptions(AFormat: TdxOfficeImageFormat): TObject; override;
    function EqualsFormat(const Value1, Value2: TdxOfficeImageFormat): Boolean; override;
  end;

  { TdxRichEditImageImportHelper }

  TdxRichEditImageImportHelper = class(TdxImageImportHelper)
  public
    procedure ThrowUnsupportedFormatException; override;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Options,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

{ TdxDocumentImportHelper }

function TdxDocumentImportHelper.EqualsFormat(const Value1,
  Value2: TdxRichEditDocumentFormat): Boolean;
begin
  Result := Value1 = Value2;
end;

function TdxDocumentImportHelper.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxDocumentImportHelper.GetUndefinedFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Undefined;
end;

function TdxDocumentImportHelper.GetFallbackFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.PlainText;
end;

function TdxDocumentImportHelper.GetPredefinedOptions(AFormat: TdxRichEditDocumentFormat): TObject;
begin
  Result := DocumentModel.DocumentImportOptions.GetOptions(AFormat);
end;

procedure TdxDocumentImportHelper.ThrowUnsupportedFormatException;
begin
  TdxRichEditExceptions.ThrowUnsupportedFormatException;
end;

procedure TdxDocumentImportHelper.ApplyEncoding(const AOptions: IdxImporterOptions; AEncoding: TEncoding);
begin
  if AOptions is TdxDocumentImporterOptions then
    TdxDocumentImporterOptions(AOptions).ActualEncoding := AEncoding;
end;

{ TdxImageImportHelper }

procedure TdxImageImportHelper.ApplyEncoding(const AOptions: IdxImporterOptions; AEncoding: TEncoding);
begin
// do nothing
end;

function TdxImageImportHelper.EqualsFormat(const Value1,
  Value2: TdxOfficeImageFormat): Boolean;
begin
  Result := Value1 = Value2;
end;

function TdxImageImportHelper.GetFallbackFormat: TdxOfficeImageFormat;
begin
  Result := UndefinedFormat;
end;

function TdxImageImportHelper.GetPredefinedOptions(
  AFormat: TdxOfficeImageFormat): TObject;
begin
  Result := nil;
end;

function TdxImageImportHelper.GetUndefinedFormat: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.None;
end;

{ TdxRichEditImageImportHelper }

procedure TdxRichEditImageImportHelper.ThrowUnsupportedFormatException;
begin
  TdxRichEditExceptions.ThrowUnsupportedFormatException;
end;

end.
