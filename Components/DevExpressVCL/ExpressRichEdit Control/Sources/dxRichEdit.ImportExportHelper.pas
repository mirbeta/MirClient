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

unit dxRichEdit.ImportExportHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes,

  dxRichEdit.NativeApi,
  dxRichEdit.Utils.FileDialogFilter,
  dxRichEdit.DocumentModel.Core;

type

  IdxDocumentSaveOptions = interface
  ['{53F63F26-447E-4841-A8E5-545B47766477}']
    function GetDefaultFileName: string;
    procedure SetDefaultFileName(const AFileName: string);
    function GetCurrentFileName: string;
    procedure SetCurrentFileName(const AFileName: string);
    function GetDefaultFormat: TdxRichEditDocumentFormat;
    procedure SetDefaultFormat(const AFormat: TdxRichEditDocumentFormat);
    function GetCurrentFormat: TdxRichEditDocumentFormat;
    procedure SetCurrentFormat(const AFormat: TdxRichEditDocumentFormat);

    property DefaultFileName: string read GetDefaultFileName write SetDefaultFileName;
    property CurrentFileName: string read GetCurrentFileName write SetCurrentFileName;
    property DefaultFormat: TdxRichEditDocumentFormat read GetDefaultFormat write SetDefaultFormat;
    property CurrentFormat: TdxRichEditDocumentFormat read GetCurrentFormat write SetCurrentFormat;
  end;

  { TdxImportExportHelper }

  TdxImportExportHelper = class abstract
  private
    FDocumentModel: TdxCustomDocumentModel;
  protected
    function CreateFilterString(AFilters: TdxFileDialogFilterCollection): string;
  public
    constructor Create(const ADocumentModel: TdxCustomDocumentModel);
    procedure ThrowUnsupportedFormatException; virtual; abstract;

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
  end;

implementation

{ TdxImportExportHelper }

constructor TdxImportExportHelper.Create(const ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
end;

function TdxImportExportHelper.CreateFilterString(AFilters: TdxFileDialogFilterCollection): string;
begin
  Result := AFilters.CreateFilterString;
end;

end.
