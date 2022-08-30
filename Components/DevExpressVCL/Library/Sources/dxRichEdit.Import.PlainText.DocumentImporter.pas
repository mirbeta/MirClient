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

unit dxRichEdit.Import.PlainText.DocumentImporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes,
  dxCoreClasses,

  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Utils.FileDialogFilter,
  dxRichEdit.Import.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.ImportExportHelper;

type
  { TdxPlainTextDocumentImporter }

  TdxPlainTextDocumentImporter = class(TInterfacedObject, IdxImporter<TdxRichEditDocumentFormat, Boolean>)
  strict private
    class var
      FFilter: TdxFileDialogFilter;
  strict private
    class constructor Initialize;
    class destructor Finalize;
  protected
    function Format: TdxRichEditDocumentFormat; virtual;
    function Filter: TdxFileDialogFilter; virtual;
    function FormatEquals(const AValue: TdxRichEditDocumentFormat): Boolean;
    function SetupLoading: TObject; virtual;
    function LoadDocument(const ADocumentModel: TdxCustomDocumentModel;
      AStream: TStream; const AOptions: IdxImporterOptions): Boolean; virtual;
  public
    class property DefaultFilter: TdxFileDialogFilter read FFilter;
  end;

implementation

uses
  SysUtils, dxCore,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Strs;

{ TdxPlainTextDocumentImporter }

class constructor TdxPlainTextDocumentImporter.Initialize;
begin
  FFilter := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_TextFiles), 'txt');
end;

class destructor TdxPlainTextDocumentImporter.Finalize;
begin
  FreeAndNil(FFilter);
end;

function TdxPlainTextDocumentImporter.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.PlainText;
end;

function TdxPlainTextDocumentImporter.Filter: TdxFileDialogFilter;
begin
  Result := FFilter;
end;

function TdxPlainTextDocumentImporter.FormatEquals(const AValue: TdxRichEditDocumentFormat): Boolean;
begin
  Result := Format = AValue;
end;

function TdxPlainTextDocumentImporter.SetupLoading: TObject;
begin
  Result := TdxPlainTextDocumentImporterOptions.Create;
end;

function TdxPlainTextDocumentImporter.LoadDocument(const ADocumentModel: TdxCustomDocumentModel;
  AStream: TStream; const AOptions: IdxImporterOptions): Boolean;
var
  AModel: TdxDocumentModel absolute ADocumentModel;
begin
  AModel.InternalAPI.LoadDocumentPlainTextContent(AStream, TdxPlainTextDocumentImporterOptions(AOptions));
  Result := True;
end;

end.
